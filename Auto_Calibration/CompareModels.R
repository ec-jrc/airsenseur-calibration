##########################################C
# Initialisation ####
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
Project     <- "Federico"
DIR_General <- "General_data"
DIR_Config  <- "Configuration"
DIR_Models  <- "Models"
# Load packages
# Installing package librarian to install and load packages from CRAN, github and bio conductor in 1 line
# https://towardsdatascience.com/an-efficient-way-to-install-and-load-r-packages-bc53247f058d
if (!"remotes"   %in% utils::installed.packages()) install.packages("remotes")
if (!"librarian" %in% utils::installed.packages()) remotes::install_github("DesiQuintans/librarian")
source("global.R")
if (exists("list.Packages") && list.Packages != "") {
    librarian::shelf(list.Packages)
    rm(list.Packages, envir = .GlobalEnv)} 
if (exists("list.packages.github") && list.packages.github != "") {
    librarian::shelf(list.packages.github)
    rm(list.packages.github, envir = .GlobalEnv)} 
librarian::shelf(gplots) # heatmap2
library(plyr)
library(BSDA)
futile.logger::flog.info("[Global] List of installed packages")
print(search(), quote = FALSE)
cat("\n")
futile.logger::flog.threshold(INFO)


#################################C
# Model of Calibration per sensor ####
##########################################C
Cal.Param   <- data.table::data.table(
    Sensor               = c("CO_A4_P1","D300","NO_B4_P1", "NO2_B43F_P1", "OX_A431_P1", "5310CAT", "5325CAT", "5301CAT", "5310CST", "5325CST", "5301CST", "OPCN3PM10", "OPCN3PM25", "OPCN3PM1"),
    Mod_Type             = c("Linear.Robust", "Linear.Robust","exp_kT", "Linear.Robust", "Linear.Robust", "Kohler", "Kohler", "Kohler", "Kohler", "Kohler", "Kohler", "Kohler", "Kohler", "Kohler"),
    Add.Covariates       = c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    Interval             = c(3L, 3L, 5L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L),
    Relationships        = list(NULL, NULL, "Out.Temperature", NULL, "NO2_B43F_P1_Volt", "Out.Relative_humidity", "Out.Relative_humidity", "Out.Relative_humidity", "Out.Relative_humidity", "Out.Relative_humidity", "Out.Relative_humidity", "Out.Relative_humidity", "Out.Relative_humidity", "Out.Relative_humidity"), 
    Degrees              = rep(1, times = 14),
    Discarded.covariates = list(c("OPCN3PM10_volt", "5310CAT_volt", "NO2_B43F_P1_volt"),
                                c("5310CAT_volt","5310CST_volt","5325CAT_volt","5325CST_volt","5301CAT_volt","5301CST_volt","OPCN3PM10_volt", "OPCN3PM25_volt", "OPCN3PM1_volt", "NO_B4_P1_volt", "NO2_B43F_P1_volt","OX_A431_P1_volt"),
                                c("5310CAT_volt","OPCN3PM10_volt", "Atmospheric_pressure"),
                                c("5310CAT_volt","5310CST_volt","5325CAT_volt","5325CST_volt","5301CAT_volt","5301CST_volt","OPCN3PM10_volt", "OPCN3PM25_volt", "OPCN3PM1_volt",
                                  "Out.Atmospheric_pressure","Out.Relative_humidity_int", "Out.Relative_humidity", "D300_volt", "NO_B4_P1_volt"),
                                c("5310CAT_volt", "5301CAT_volt","5325CAT_volt","5301CST_volt", "5325CST_volt", "5310CST_volt", "OPCN3PM10_volt", "OPCN3PM25_volt", "OPCN3PM1_volt") ,
                                "","","","","","","","",""),
    Treshold.VIF         = rep(10, times = 14),
    VIF                  = rep(TRUE, times = 14),
    Conf.level           = rep(0.05, times = 14),
    Thresh.R2            = rep(0.00, times = 14),
    DRIFT                = rep(FALSE, times = 14),
    volt                 = rep(TRUE, times = 14),
    modelled             = rep(FALSE, times = 14),
    Register             = rep(TRUE, times = 14),
    Exclude.Cal          = rep(TRUE, times = 14))
# adding degrees depending on the length of Relationships
set(Cal.Param, j = "Degrees", value = apply(Cal.Param, MARGIN = 1, function(i) rep(1, times = length(i$Relationships))))
# Relationships for NO2 could be: Relationships: c("Absolute_humidity", "Out.Temperature_int", "OX_A431_P1_volt")

##########################################C
# Parameters: selection of ASE boxes, sensors, place and date interval for calibration and prediction (this one is not so relevant)####
##########################################C
# List.ASE <- c("424A56","42A57","42D501","425D0A","425D0B","4278FD")
# For the calibration of the first box only
List.ASE <- c("424A56")
# Sensor to calibrate
Cal.Sensors <- c("CO_A4_P1", "NO_B4_P1", "NO2_B43F_P1", "OX_A431_P1")

Place                <- "Ispra"
Cal_Interval         <- c(as.Date("2020-11-22"), as.Date("2020-11-29")) # Be careful repeated in the calibration loop as NO2 needs another date interval
Predict_Interval     <- c(as.Date("2020-10-08"), as.Date("2021-01-13"))

##########################################C
# Calibrating ####
##########################################C

if (length(List.ASE) > 0) for (ASE in List.ASE) {
    if (length(Cal.Sensors) > 0) for (i.Sensor in which(Cal.Param$Sensor %in% Cal.Sensors)) {
        
        Sensor_name          <- Cal.Param$Sensor[i.Sensor]
        Sens.Model           <- Cal.Param$Mod_Type[i.Sensor]
        Add.Covariates       <- Cal.Param$Add.Covariates[i.Sensor]
        Relationships        <- Cal.Param$Relationships[i.Sensor][[1]]
        Degrees              <- Cal.Param$Degrees[i.Sensor][[1]]
        namesCovariates      <- if (length(Relationships) > 0 && Relationships != "") paste0(paste(Relationships,Degrees, sep = "-"),collapse = "&") else ""
        Auto.cal.name        <- paste0(c("Auto.Cal",Sensor_name, Place, format(Cal_Interval[1],"%Y%m%d"), format(Cal_Interval[2],"%Y%m%d"), Sens.Model,Add.Covariates,namesCovariates),collapse = "__")
        
        # changing calibration date for NO2_B43F
        if (Sensor_name== "NO2_B43F_P1") Cal_Interval <- c(as.Date("2020-11-15"), as.Date("2020-11-29")) else Cal_Interval <- c(as.Date("2020-11-22"), as.Date("2020-11-29")) 
        
        Returned  <- AutoCal.Boxes.Sensor(Dir = Dir, Project = Project, DIR_Models = DIR_Models, 
                                          List.ASE             = ASE, 
                                          name.sensor          = Sensor_name,
                                          Interval             = Cal.Param$Interval[i.Sensor], 
                                          DateIN = Cal_Interval[1], DateEND = Cal_Interval[2], Meas.DateIN = Predict_Interval[1], Meas.DateEND = Predict_Interval[2],
                                          Mod_type             = Sens.Model,
                                          Relationships        = Relationships,
                                          Degrees              = Degrees, 
                                          Add.Covariates       = Add.Covariates, 
                                          Discarded.covariates = Cal.Param$Discarded.covariates[i.Sensor][[1]],
                                          Treshold.VIF         = Cal.Param$Treshold.VIF[i.Sensor],
                                          VIF                  = Cal.Param$VIF[i.Sensor],
                                          Threshold.R2         = Cal.Param$Thresh.R2[i.Sensor],
                                          Conf.level           = Cal.Param$Conf.level[i.Sensor], 
                                          DRIFT                = Cal.Param$DRIFT[i.Sensor],
                                          volt                 = Cal.Param$volt[i.Sensor],
                                          modelled             = Cal.Param$modelled[i.Sensor], 
                                          Register             = Cal.Param$Register[i.Sensor],
                                          Exclude.Cal          = Cal.Param$Exclude.Cal[i.Sensor],
                                          Rdata.name.file      = paste0(Auto.cal.name,".rdata"))}
}

# Comparing all calibration models
# List.models.Final <- sapply(List.ASE, function(i){
#     ASEDir   <- file.path(Dir, Project, i)
#     Model    <- load_obj(file.path(ASEDir,"Models", paste0(Auto.cal.name,".rdata")))
#     return(file.path(ASEDir,"Models",Model$Final_median_Model$List.NewModels))
# })
# Compared <- Compare_Models(Exclude.Cal= F, List.models =  List.models.Final, Verbose = T, Meas.DateIN = Predict_Interval[1], Meas.DateEND = Predict_Interval[2])
