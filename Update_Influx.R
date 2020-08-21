##########################################C
# Initialisation
##########################################C
rm(list =  ls())
# Setting Directory and sourcing files
Possible.Dir <- c("S:\\Box Sync\\AirSensEUR\\Fieldtests\\Shiny",
                  "C:\\Bureau\\DIffusion\\Box Sync\\AirSensEUR\\Fieldtests\\Shiny",
                  "/media/sf_Box_Sync/AirSensEUR/Fieldtests/Shiny",
                  "/home/shinyadmin/App",
                  "E:/Bureau/Diffusion/Box Sync/AirSensEUR/Fieldtests/Shiny")
for (Dir in Possible.Dir) if (dir.exists(Dir)) {setwd(Dir); break()}
rm(Possible.Dir)
# Load packages
source("global.R")
# Installing package librarian to install and load packages from CRAN, github and bio conductor in 1 line
# https://towardsdatascience.com/an-efficient-way-to-install-and-load-r-packages-bc53247f058d
if (!"remotes"   %in% utils::installed.packages()) install.packages("remotes")
if (!"librarian" %in% utils::installed.packages()) remotes::install_github("DesiQuintans/librarian")
if (exists("list.Packages") && list.Packages != "") librarian::shelf(list.Packages)
if (exists("list.Packages")) rm(list.Packages)
if (exists("list.packages.github") && list.packages.github != "") librarian::shelf(list.packages.github)
if (exists("list.packages.github")) rm(list.packages.github)

futile.logger::flog.info("[Global] List of installed packages")
print(search(), quote = FALSE)
cat("\n")

##########################################C
# Selection of ASE boxes ####
##########################################C
#list.dirs(file.path(getwd(), "ASE_Boxes"),full.names = FALSE, recursive = FALSE)
List.ASE <- c("40458D","4047D0","406414","40641B","40642E","4065D0","4065E0","4065E3","4065ED","40816F")

##########################################
# Downloading new data if available ####
##########################################C
for (i in List.ASE) {
    influx.downloadAndPredict(boxName = i, boxConfig = influx.getConfig(boxName = i), Add.Ref = TRUE)
    if (.Platform$OS.type == "windows") {
        gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)   
    } else gc(verbose = getOption("verbose"), reset = FALSE)} 

##########################################C
# Upload Calibrated data to Influx    ####
##########################################C
for (i in List.ASE) General <- influx.downloadAndPredict(boxName = i, boxConfig = influx.getConfig(boxName = i), Add.Ref = TRUE)

##########################################C
# Upload Calibrated data to Influx    ####
##########################################C
boxConfig <- influx.getConfig(boxName = List.ASE[1])
if (boxConfig$Server$PROXY) {
    if (is.null(boxConfig$Server$LOGIN)) {
        set_config(use_proxy(url=boxConfig$Server$URL, port=boxConfig$Server$PORT)) 
    } else set_config( use_proxy(url=boxConfig$Server$URL, port=boxConfig$Server$PORT, username = LOGIN, password = PASSWORD))
} else reset_config()
Influx.con <- httr::GET(paste0("http://",boxConfig$Server$Host,":",boxConfig$Server$Port,"/ping"),
                        config = authenticate(user = boxConfig$Server$User, password = boxConfig$Server$Pass, type = "basic"))
if (Influx.con$status_code != 204) {
    futile.logger::flog.error("[Down_Influx] ERROR Influx server is down. Stopping the script.", "/n")
    return(futile.logger::flog.error("[Down_Influx] Influx server is down. Stopping the script."))
} else futile.logger::flog.info("[Down_Influx] Influx server is up; connected to server")

#------------------------------------------------------------------------------CR
# table dataset exists?
#------------------------------------------------------------------------------CR
Dataset <- paste0(boxConfig$Server$Dataset, "_Calibrated")

# total number of rows
Influx.Q <- httr::GET(URLencode(paste0("http://", boxConfig$Server$Host,":", boxConfig$Server$HostPort,"/query?db=", boxConfig$Server$Db)),
                            config = authenticate(user = boxConfig$Server$User, password = boxConfig$Server$Pass, type = "basic"),
                            query = list(q = paste0("SELECT count(boardTimeStamp) FROM \"", Dataset,"\"")))


if (dbExistsTable(conn = SQLite.con, name = Dataset) || dbExistsTable(conn = SQLite.con, name = paste0(Dataset,"_Cast"))) { 
    # the table Dataset exists in airsenseur.db
    futile.logger::flog.info(paste0("[Down_Influx] the table ", Dataset, " already exists in airsenseur.db."))
    if (dbExistsTable(conn = SQLite.con, name = paste0(Dataset,"_Cast"))) {
        Dataset.N               <- DBI::dbGetQuery(SQLite.con, paste0("SELECT max(rowid) FROM \"", paste0(Dataset,"_Cast"), "\""))[1,1]
        SQL.time.Last           <- DBI::dbGetQuery(SQLite.con, paste0("SELECT * FROM \"", paste0(Dataset,"_Cast"),"\"  order by rowid desc limit 1;"))$time
        SQL.gpsTimestamp.Last   <- DBI::dbGetQuery(SQLite.con, paste0("SELECT * FROM \"", paste0(Dataset,"_Cast"),"\"  order by rowid desc limit 1;"))$gpsTimestamp
        # deleteing the table DataSet, not used. Now DataSet_cast is used
        if (dbExistsTable(conn = SQLite.con, name = Dataset)) {
            DBI::dbRemoveTable(conn = SQLite.con, name = Dataset)
            DBI::dbExecute(conn = SQLite.con, "VACUUM;")
        } 
    } else {
        DT.SQL.query <- data.table::data.table(DBI::dbGetQuery(SQLite.con, paste0("SELECT * FROM \"", Dataset,"\"  order by rowid")))
        # remove duplicated if any
        Duplicated.rows <- which(duplicated(DT.SQL.query))
        if (length(Duplicated.rows) > 0) DT.SQL.query <- DT.SQL.query[-Duplicated.rows]
        DT.SQL.query <- dcast(DT.SQL.query, time + altitude + boardTimeStamp + gpsTimestamp + latitude + longitude ~ name, value.var = "sampleEvaluatedVal", fill = NA)
        #DBI::dbRemoveTable( conn = SQLite.con, name = paste0(Dataset,"_Cast"))
        RSQLite::dbWriteTable(conn = SQLite.con, name = paste0(Dataset,"_Cast"), value = DT.SQL.query)
        # Counting the number of records in AirSensEUR$Dataset - This will work provided that all rowid exist.
        Dataset.N               <- DBI::dbGetQuery(SQLite.con, paste0("SELECT max(rowid) FROM \"", Dataset, "\""))[1,1]
        SQL.time.Last           <- DBI::dbGetQuery(SQLite.con, paste0("SELECT * FROM \"", Dataset,"\"  order by rowid desc limit 1;"))$time
        SQL.gpsTimestamp.Last   <- DBI::dbGetQuery(SQLite.con, paste0("SELECT * FROM \"", Dataset,"\"  order by rowid desc limit 1;"))$gpsTimestamp
        # deleteing the table DataSet, not used. Now DataSet_cast is used
        DBI::dbRemoveTable(conn = SQLite.con, name = Dataset)
        DBI::dbExecute(conn = SQLite.con, "VACUUM;")
    }
    # Error Message and stop the script if there more data in the airsenseur.db than in InfluxDB
    if (difftime(ymd_hms(Influx.Last$time), ymd_hms(SQL.time.Last), units = "mins") < Mean) {
        futile.logger::flog.info("[Down_Influx] Downloading is up to date. No need for data download.")
    } else futile.logger::flog.info(paste0("[Down_Influx] records between ",format(ymd_hms(SQL.time.Last),"%Y-%m-%d %H:%M")," and ",format(ymd_hms(Influx.Last$time),"%Y-%m-%d %H:%M"), 
                                           " are added into the table ", paste0(Dataset,"_cast"), " in airsenseur.db."))
    Dataset.index   <- FALSE # if airsenseur.db exists then the indexes were already created, then no need to create the indexes
} else {# the table Dataset does not exist in airsenseur.db
    # There are no records in AirSensEUR$Dataset
    futile.logger::flog.info(paste0("[Down_Influx] the table ", Dataset, " does not exist in airsenseur.db. It is going to be created."))
    Dataset.N       <- 0
    # Get SQL.time.Last as First GPS time (and the timestamp together) of InfluxDB
    SQL.time.Last <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)),
                               config = authenticate(user = User, password = Pass, type = "basic"),
                               query = list(q = paste0("SELECT  FIRST(sampleEvaluatedVal) FROM \"", Dataset, "\"")))
    if (SQL.time.Last$status_code != 200) futile.logger::flog.error("[Down_Influx] query first sampleEvaluatedValin airsenseur.db. Influx server may be down./n")
    SQL.time.Last <- Json_To_df(SQL.time.Last)$time
    Dataset.index   <- TRUE # if true indexes will be created
} 

# total number of rows
Influx.Total.N <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)),
                            config = authenticate(user = User, password = Pass, type = "basic"),
                            query = list(q = paste0("SELECT count(boardTimeStamp) FROM \"", Dataset,"\"")))
Influx.Total.N <- Json_To_df(Influx.Total.N, Numeric = "boardTimeStamp")$count
# Last GPS time (and the timestamp together)
Influx.Last <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)),
                         config = authenticate(user = User, password = Pass, type = "basic"),
                         query = list(q = paste0("SELECT  LAST(boardTimeStamp) FROM \"", Dataset,"\"")))
Influx.Last <- Json_To_df(Influx.Last, Numeric = "last")

