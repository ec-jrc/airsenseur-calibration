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
Project     <- "ASE_Boxes"
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
librarian::shelf(future.apply)

# Select AIrSensEUR boxes ####
List.ASE <- c(#Ispra ====
              #"40458D","4047D0","406414","40641B","40642E",
              #"4065D0","4065E0","4065E3","4065ED","40816F",
              # Antwerp ====
              "402723", "402B00", "4043A7", "4043AE", "4043B1",
              "4047CD", "4047D7", "4047DD", "4047E0", "4047E7",
              "40499C", "40499F", "4049A6", "40623F", "406246",
              "406249", "40641B", "406424", "40642B", "4065D0",
              "4065D3", "4065DA", "4065DD", "4065E0", "4065E3",
              "4065EA", "4067B0", "4067B3", "4067BA", "4067BD",
              "408165", "408168", "408175", "408178",
              # Oslo ====
              "40458D", "40642E", "4065ED", "40816F", "40817F",
              "425FB3", "425FB4", "426178", "426179", #"42816E", problem Arietta not working
              "647D5A", "648B91", "649312", "649526", "64A291",
              "64A292", "64B082", "64CB6D", "64CB70", "64CB78",
              "64E9C5", "64FD0A", "64FD11", "65063E", "6517DD",
              "651EF5", "651EFC", "652A32", "652D3A", "652FA4",
              "652FAF", "653257", "65325E", "65326C"
              # Zagreb ====
              ,"4047D0", "406414", "40641E", "427906", "427907",
              "428164", "42816D", "648157", "648169", "64876B",
              "64876C", "649738", "64C225", "64C52B", "64E03B",
              "652A38", "652FA1"
)

if (.Platform$OS.type == "unix") {
    
    # Load packages for Unix
    source("global_Upload_InfluxDB.R")
    if (exists("list.Packages") && list.Packages != "") {
        librarian::shelf(list.Packages)
        rm(list.Packages, envir = .GlobalEnv)}
    # be sure that PROXY is set to FALSE in all ASE.cfg config files
    source(file.path(WD, "SET_PROXY_FALSE.R"))
} else if (.Platform$OS.type == "windows") {
    
    # Load packages for windows, useful for future_apply
    source("global_Upload_InfluxDB.R")
    if (exists("list.Packages") && list.Packages != "") {
        librarian::shelf(list.Packages)
        rm(list.Packages, envir = .GlobalEnv)}}

# level of flog message, only ERROR or WARN or INFO message
flog.threshold(INFO)

##################### Configuration #####################
Parallel       <- c("Future_apply", "ForEach", " Sequential")[2]
# "ForEach"         : it is the fastest parallel computing and creates a log file. However, it seems to have problem to release memory under Unix
# "Future_apply"    : it is is slower than the "ForEach" method and does not allow saving a log file, it only print a few messages when computing ends. However, this method is more robust.
# "Sequential"      : no parallel computing, only sequential. Extremely slow but save a log file which is sorted by ASE boxes
# "ForEach": number of ASE boxes treated per cluster of cores if Parallel == "ForEach"
List.ASE.width <- 7

# detect number of cores to use parallel backend
Max.cores      <- 12
Free.cores     <- 8
cores          <- min(c(detectCores()- Free.cores, length(List.ASE), Max.cores), na.rm = T)

# "ForEach": deleting log file if it exists
if (file.exists("Log_Parallel.txt")) unlink("Log_Parallel.txt")

# set uploading of reference data
Add.Ref <- TRUE

# Set user and passwor for writing to the Influx server
User = "########"
Pass = "########"

##################### Computing  #####################
if (Parallel == "ForEach") {
    
    # List of ASE to include in all steps per cluster of cores
    Steps <- lapply(0:(length(List.ASE) %/% List.ASE.width), function(Step) (Step * List.ASE.width+1):min(length(List.ASE),(Step * List.ASE.width + List.ASE.width)))
    
    for (Step in seq_along(Steps)) {
        
        #setup parallel backend to use many processors
        if (.Platform$OS.type == "windows"){
            cl <- parallel::makePSOCKcluster(cores[1], outfile = "Log_Parallel.txt", methods = F, useXDR = F)   
        } else cl <- parallel::makeForkCluster(cores[1], outfile = "Log_Parallel.txt", methods = F, useXDR = F)
        doParallel::registerDoParallel(cl)
        
        Essai <- foreach(i = seq_along(List.ASE), .verbose = T, .combine = c) %dopar% {
            
            # Load packages for windows
            if (.Platform$OS.type == "windows") {
                
                source("global_Upload_InfluxDB.R")
                if (exists("list.Packages") && list.Packages != "") {
                    librarian::shelf(list.Packages)
                    rm(list.Packages, envir = .GlobalEnv)}
                # level of flog message, only error message
                flog.threshold(INFO)}
            boxConfig    <- influx.getConfig(boxName = List.ASE[i])
            ASE.Download <- influx.downloadAndPredict(boxName = List.ASE[i], boxConfig = boxConfig, Add.Ref = Add.Ref)
            
            # uploading calibrated data to InfluxDB
            Upload2Influx(WD, Project, List.ASE = List.ASE[i], User = User, Pass = Pass, DIR_Config = DIR_Config, DIR_General = DIR_General, DIR_Results = DIR_Results,
                          General.DT = ASE.Download$data, SetTime = ASE.Download$timeConfig)
            
            Return NULL to be sure that combine = c use NULL
            return(NULL)
        }
        #stop cluster
        parallel::stopCluster(cl)
    }
} else if (Parallel == "Sequential") {
    Essai <- foreach(i = Steps[[Step]], .verbose = T, .combine = c) %do% {
        #Essai <- foreach(i = 1:length(List.ASE), .verbose = T, .combine = c) %dopar% {
        
        # Load packages for windows
        if (.Platform$OS.type == "windows") {
            
            source("global_Upload_InfluxDB.R")
            if (exists("list.Packages") && list.Packages != "") {
                librarian::shelf(list.Packages)
                rm(list.Packages, envir = .GlobalEnv)}
            # level of flog message, only error message
            flog.threshold(INFO)}
        boxConfig    <- influx.getConfig(boxName = List.ASE[i])
        ASE.Download <- influx.downloadAndPredict(boxName = List.ASE[i], boxConfig = boxConfig, Add.Ref = Add.Ref)
        
        # uploading calibrated data to InfluxDB
        Upload2Influx(WD, Project, List.ASE = List.ASE[i], User = User, Pass = Pass, DIR_Config = DIR_Config, DIR_General = DIR_General, DIR_Results = DIR_Results,
                      General.DT = ASE.Download$data, SetTime = ASE.Download$timeConfig)
        #})
        
        # Return NULL to be sure that combine = c use NULL
        return(NULL)
    }
    
    # Another way like Future_apply but sequential
    # Updated.Data <- apply(Param.combined, MARGIN =  1, function(Simul){
    #     # Setting the file name
    #     # Setting the file name
    #     boxConfig    <- influx.getConfig(boxName = List.ASE[i])
    #     ASE.Download <- influx.downloadAndPredict(boxName = List.ASE[i], boxConfig = boxConfig, Add.Ref = TRUE)
    #     # cleaning garbage
    #     # if (.Platform$OS.type == "windows") {
    #     #     gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)   
    #     # } else gc(verbose = getOption("verbose"), reset = FALSE)
    #     
    #     #profvis({
    #     # uploading calibrated data to InfluxDB
    #     Upload2Influx(WD, Project, List.ASE = List.ASE[i], User = User, Pass = Pass, DIR_Config = DIR_Config, DIR_General = DIR_General, DIR_Results = DIR_Results,
    #                   General.DT = ASE.Download$data, SetTime = ASE.Download$timeConfig)
    #     return(NULL)})
} else if (Parallel == "Future_apply") {
    # Set cluster of cores
    plan(multisession, workers = cores)
    
    Updated.Data <- future_lapply(seq_along(List.ASE), function(i){
        
        # Setting the file name
        boxConfig    <- influx.getConfig(boxName = List.ASE[i])
        ASE.Download <- influx.downloadAndPredict(boxName = List.ASE[i], boxConfig = boxConfig, Add.Ref = Add.Ref)
        
        # uploading calibrated data to InfluxDB
        Upload2Influx(WD, Project, List.ASE = List.ASE[i], User = User, Pass = Pass, DIR_Config = DIR_Config, DIR_General = DIR_General, DIR_Results = DIR_Results,
                      General.DT = ASE.Download$data, SetTime = ASE.Download$timeConfig)
        return(NULL)
    }, future.seed = NULL, future.conditions = "message")
    
    # Close the cluster
    future:::ClusterRegistry("stop")}

