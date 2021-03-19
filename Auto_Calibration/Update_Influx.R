# Initialisation ####
# Cleaning environment ----
rm(list = ls())
# Setting Directory and sourcing files ----
Possible.Dir <- c("S:\\Box Sync\\AirSensEUR\\Fieldtests\\Shiny",
                  "C:\\Bureau\\DIffusion\\Box Sync\\AirSensEUR\\Fieldtests\\Shiny",
                  "H:\\Bureau\\Diffusion\\Box Sync\\AirSensEUR\\Fieldtests\\Shiny",
                  "/media/sf_Box_Sync/AirSensEUR/Fieldtests/Shiny",
                  "/home/shinyadmin/App")
for (WD in Possible.Dir) if (dir.exists(WD)) {setwd(WD); break()}
rm(Possible.Dir)
Project     <- "Federico"
DIR_General <- "General_data"
DIR_Config  <- "Configuration"
DIR_Results <- "Results"

# Load packages ----
# Installing package librarian to install and load packages from CRAN, github and bio conductor in 1 line
# https://towardsdatascience.com/an-efficient-way-to-install-and-load-r-packages-bc53247f058d
if (!"remotes"   %in% utils::installed.packages()) install.packages("remotes")
if (!"librarian" %in% utils::installed.packages()) remotes::install_github("DesiQuintans/librarian")
librarian::shelf(parallel)
librarian::shelf(foreach)
librarian::shelf(profvis) # cheking memory use

# Select AIrSensEUR boxes ####

# Load packages for windows
if (.Platform$OS.type == "unix") {
    
    source("global_Upload_InfluxDB.R")
    if (exists("list.Packages") && list.Packages != "") {
        librarian::shelf(list.Packages)
        rm(list.Packages, envir = .GlobalEnv)}
    # level of flog message, only error message
    flog.threshold(INFO)
    # be sure that PROXY is set to FALSE in all ASE.cfg config files
    source(file.path(WD, "SET_PROXY_FALSE.R"))}

##################### Parallel computing                 #####################
# detect number of cores to use parallel backend
cores <- min(c(detectCores()- 16, length(List.ASE), 6), na.rm = T)
#setup parallel backend to use many processors
if (file.exists("Log_Parallel.txt")) unlink("Log_Parallel.txt")
if (.Platform$OS.type == "windows"){
    cl <- parallel::makePSOCKcluster(cores[1], outfile = "Log_Parallel.txt", methods = F, useXDR = F)   
} else cl <- parallel::makeForkCluster(cores[1], outfile = "Log_Parallel.txt", methods = F, useXDR = F)
doParallel::registerDoParallel(cl)

Essai <- foreach(i = 1:length(List.ASE), .verbose = T, .combine = c) %dopar% {
    
    # Load packages for windows
    if (.Platform$OS.type == "windows") {
        
        source("global_Upload_InfluxDB.R")
        if (exists("list.Packages") && list.Packages != "") {
            librarian::shelf(list.Packages)
            rm(list.Packages, envir = .GlobalEnv)}
        # level of flog message, only error message
        flog.threshold(INFO)}
    boxConfig = influx.getConfig(boxName = List.ASE[i], Project = Project)
    ASE.Download <- influx.downloadAndPredict(boxName = List.ASE[i], Project = Project, boxConfig = boxConfig, Add.Ref = ifelse(i==1,TRUE,FALSE))
    
    # uploading calibrated data to InfluxDB
    Upload2Influx(WD, Project, List.ASE = List.ASE[i], User = "jrcuser", Pass = "", DIR_Config = DIR_Config, DIR_General = DIR_General, DIR_Results = DIR_Results,
                  General.DT = ASE.Download$data, SetTime = ASE.Download$timeConfig, Filter.Ref = ifelse(i==1,TRUE,FALSE),
                  Ref_Analysers = data.table(variable    = c("Ref.CO_ppm", "Ref.NO", "Ref.NO2", "Ref.NOx","Ref.O3", "Ref.SO2","Ref.CO2" , "Ref.PM10", "Ref.PM2.5", "ref.PM1","Ref.Temp"),
                                             name.sensor = c("APMA-370"  , "42i"   , "42i"    , "42i"    ,"49i"   , "43i"    , "Picarro", "FIDAS"   , "FIDAS"    , "FIDAS", "")))
    
        # Return NULL to be sure that combine = c use NULL
    return(NULL)}

#stop cluster
parallel::stopCluster(cl)
