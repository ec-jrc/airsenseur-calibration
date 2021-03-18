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

##########################################C
# Selection of ASE boxes Ispra ####
##########################################C
#list.dirs(file.path(getwd(), Project),full.names = FALSE, recursive = FALSE)
List.ASE <- c("424A56","42A57","42D501","425D0A","425D0B","4278FD")
# For the calibration of the first box
List.ASE <- c("424A56")

##########################################C
# Calibrating in Ispra ####
##########################################C
Place                <- "Ispra"
Cal_Interval         <- c(as.Date("2020-11-22"), as.Date("2020-11-29"))
Predict_Interval     <- c(as.Date("2020-10-08"), as.Date("2021-01-13"))

# CO_A4_P1 ----
Sensor_name          <- "CO_A4_P1"
Auto.cal.name        <- paste0(c("Auto.Cal",Sensor_name, Place, format(Cal_Interval[1],"%Y%m%d"), format(Cal_Interval[2],"%Y%m%d"), Sens.Model,Add.Covariates,namesCovariates),collapse = "__")

Sens.Model           <- "Linear.Robust"
Add.Covariates       <- FALSE
Relationships        <- NULL
Degrees              <- NULL
Discarded.covariates <- c("OPCN3PM10_volt", "5310CAT_volt", "NO2_B43F_P1_volt") 
namesCovariates      <- if (length(Relationships) > 0 && Relationships != "") paste0(paste(Relationships,Degrees, sep = "-"),collapse = "&") else ""
Ispra.CO  <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, Dir = Dir, Project = Project, DIR_Models = "Models", name.sensor = Sensor_name, 
                                  Interval = 3L, DateIN = Cal_Interval[1], DateEND = Cal_Interval[2], Meas.DateIN = Predict_Interval[1], Meas.DateEND = Predict_Interval[2], 
                                  Mod_type = Sens.Model, Relationships = Relationships, Add.Covariates = Add.Covariates, 
                                  DRIFT = FALSE, volt = TRUE, modelled = FALSE, Discarded.covariates = Discarded.covariates, 
                                  Register = TRUE, Rdata.name.file = paste0(Auto.cal.name,".rdata"), Exclude.Cal = FALSE)


# Comparing all calibration models
List.models.Final <- sapply(List.ASE, function(i){
    ASEDir   <- file.path(Dir, Project, i)
    Model    <- load_obj(file.path(ASEDir,"Models", paste0(Auto.cal.name,".rdata")))
    return(file.path(ASEDir,"Models",Model$Final_median_Model$List.NewModels))
})
Compared.CO.Ispra <- Compare_Models(Exclude.Cal= F, List.models =  List.models.Final, Verbose = T, Meas.DateIN = Predict_Interval[1], Meas.DateEND = Predict_Interval[2])

# # D300 ----
# Sensor_name          <- "D300"
# Sens.Model           <- "Linear.Robust"
# Add.Covariates       <- TRUE
# Relationships        <- NULL
# Degrees              <- "1"
# Discarded.covariates <- c("5310CAT_volt","OPCN3PM10_volt", "NO_B4_P1_volt", "NO2_B43F_P1_volt","OX_A431_P1_volt")
# namesCovariates      <- if (length(Relationships) > 0 && Relationships != "") paste0(paste(Relationships,Degrees, sep = "-"),collapse = "&") else ""
# Auto.cal.name        <- paste0(c("Auto.Cal",Sensor_name, Place, format(Cal_Interval[1],"%Y%m%d"), format(Cal_Interval[2],"%Y%m%d"), Sens.Model,Add.Covariates,namesCovariates),collapse = "__")
# Ispra.CO2 <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, name.sensor = Sensor_name,
#                                   Interval = 3L, DateIN = Cal_Interval[1], DateEND = Cal_Interval[2],
#                                   Treshold.VIF = 10, Conf.level = 0.05, Mod_type = Sens.Model, Relationships = Relationships,
#                                   Add.Covariates = Add.Covariates, DRIFT = FALSE, volt = TRUE, modelled = FALSE, Register = TRUE, 
#                                   Discarded.covariates =Discarded.covariates, Rdata.name.file = paste0(Auto.cal.name,".rdata"), Exclude.Cal = FALSE)
# 
# # Comparing all calibration models
# List.models.Final <- sapply(List.ASE, function(i){
#     ASEDir   <- file.path(Dir, Project, i)
#     Model    <- load_obj(file.path(ASEDir,"Models", paste0(Auto.cal.name,".rdata")))
#     return(file.path(ASEDir,"Models",Model$Final_median_Model$List.NewModels))
# })
# Compared.CO2.Ispra <- Compare_Models(Exclude.Cal= F, List.models =  List.models.Final, Verbose = T,Meas.DateIN = Predict_Interval[1], Meas.DateEND = Predict_Interval[2])

# NO_A4_P1 ====
Sensor_name          <- "NO_B4_P1"

# "T_power" with temperature ----
Relationships        <- "Out.Temperature"
Ispra.NO  <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, Dir = Dir, Project = Project, DIR_Models = "Models", name.sensor = "NO_B4_P1",
                                  Interval = 5L,  DateIN = Cal_Interval[1], DateEND = Cal_Interval[2], Meas.DateIN = Predict_Interval[1], Meas.DateEND = Predict_Interval[2],
                                  Treshold.VIF = 10, Conf.level = 0.05, Mod_type = "T_power", Relationships = Relationships,
                                  Add.Covariates = FALSE, DRIFT = TRUE, volt = TRUE, modelled = FALSE, 
                                  Discarded.covariates = c("5310CAT_volt","OPCN3PM10_volt", "Atmospheric_pressure"), Register = TRUE)
# "exp_kT" with temperature ----
Sens.Model           <- "exp_kT"
Add.Covariates       <- FALSE
Relationships        <- "Out.Temperature"
Degrees              <- "1"
Discarded.covariates <- c("5310CAT_volt","OPCN3PM10_volt", "Atmospheric_pressure")
namesCovariates      <- if (length(Relationships) > 0 && Relationships != "") paste0(paste(Relationships,Degrees, sep = "-"),collapse = "&") else ""
Auto.cal.name        <- paste0(c("Auto.Cal",Sensor_name, Place, format(Cal_Interval[1],"%Y%m%d"), format(Cal_Interval[2],"%Y%m%d"), Sens.Model,Add.Covariates,namesCovariates),collapse = "__")
Ispra.NO.T.Amb  <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, name.sensor = Sensor_name,
                                        Dir = Dir, Project = Project, DIR_Models = "Models", 
                                        Interval = 5L, DateIN = Cal_Interval[1], DateEND = Cal_Interval[2], Meas.DateIN = Predict_Interval[1], Meas.DateEND = Predict_Interval[2],
                                        Treshold.VIF = 10, Conf.level = 0.05, Mod_type = Sens.Model, Relationships = Relationships, Add.Covariates = Add.Covariates, Discarded.covariates =Discarded.covariates,
                                        DRIFT = TRUE, volt = TRUE, modelled = FALSE, 
                                        Rdata.name.file = paste0(Auto.cal.name,".rdata"), Register = TRUE, Weighted = F, Exclude.Cal = FALSE)

List.models.Final <- sapply(List.ASE, function(i){
    ASEDir   <- file.path(Dir, Project, i)
    Model    <- load_obj(file.path(ASEDir,"Models", paste0(Auto.cal.name,".rdata")))
    return(file.path(ASEDir,"Models",Model$Final_median_Model$List.NewModels))
})
Compared.NO.Ispra <- Compare_Models(Exclude.Cal= F, List.models =  List.models.Final, Verbose = T,Meas.DateIN = Predict_Interval[1], Meas.DateEND = Predict_Interval[2])

# NO2_B43F_P1 ----
# Relative_humidity_int should not have an effect since water can only enter the sensor from the membrane
Sensor_name          <- "NO2_B43F_P1"
Cal_Interval         <- c(as.Date("2020-11-15"), as.Date("2020-11-29"))
Sens.Model           <- "Linear.Robust"
Add.Covariates       <- TRUE
Relationships        <- NULL #c("Absolute_humidity", "Out.Temperature_int", "OX_A431_P1_volt")
Degrees              <- rep("1", times = length(Relationships))
Discarded.covariates <- c("5310CAT_volt","5310CST_volt","5325CAT_volt","5325CST_volt","5301CAT_volt","5301CST_volt","OPCN3PM10_volt", "OPCN3PM25_volt", "OPCN3PM1_volt",
                          "Out.Atmospheric_pressure","Out.Relative_humidity_int", "Out.Relative_humidity", "D300_volt", "NO_B4_P1_volt") 
namesCovariates      <- if (length(Relationships) > 0 && Relationships != "") paste0(paste(Relationships,Degrees, sep = "-"),collapse = "&") else ""
Auto.cal.name        <- paste0(c("Auto.Cal",Sensor_name, Place, format(Cal_Interval[1],"%Y%m%d"), format(Cal_Interval[2],"%Y%m%d"), Sens.Model,Add.Covariates,namesCovariates),collapse = "__")
Ispra.NO2 <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, name.sensor = Sensor_name,
                                  Dir = Dir, Project = Project, DIR_Models = "Models", 
                                  Interval = 3L, DateIN = Cal_Interval[1], DateEND = Cal_Interval[2], Meas.DateIN = Predict_Interval[1], Meas.DateEND = Predict_Interval[2],
                                  Treshold.VIF = 10, Conf.level = 0.05, Mod_type = Sens.Model, Relationships = Relationships, Add.Covariates = Add.Covariates, Discarded.covariates =Discarded.covariates, 
                                  DRIFT = FALSE, volt = TRUE, modelled = FALSE, 
                                  Rdata.name.file = paste0(Auto.cal.name,".rdata"), Register = TRUE, Exclude.Cal = FALSE)
# Comparing all calibration models
List.models.Final <- sapply(List.ASE, function(i){
    ASEDir   <- file.path(Dir, Project, i)
    Model    <- load_obj(file.path(ASEDir,"Models", paste0(Auto.cal.name,".rdata")))
    return(file.path(ASEDir,"Models",Model$Final_median_Model$List.NewModels))
})
Compared.NO2.Ispra <- Compare_Models(Exclude.Cal= F, List.models =  List.models.Final, Verbose = T,Meas.DateIN = Predict_Interval[1], Meas.DateEND = Predict_Interval[2])

# OX_A431_P1 ----
# Thresh.R2 = -0.03: mainly for box 406414, because NO2 sensor and reference O3 are more correlated than NO2 sensor with the residuals
# increase interval to 7 days in order to diminish the standard error of estimate of coefficients of models
Sensor_name          <- "OX_A431_P1"
Cal_Interval         <- c(as.Date("2020-11-22"), as.Date("2020-11-29"))
Predict_Interval     <- c(as.Date("2020-10-08"), as.Date("2021-01-13"))
Sens.Model           <- "Linear.Robust"
Add.Covariates       <- TRUE
Relationships        <- c("NO2_B43F_P1_volt")
Degrees              <- rep("1", times = length(Relationships))
Discarded.covariates <- c("5310CAT_volt","OPCN3PM10_volt", "Out.Atmospheric_pressure","Out.Relative_humidity_int", "NO_B4_P1_volt") #we discarded NO since its coefficient is 0.00 with SIGN test
namesCovariates      <- if (length(Relationships) > 0 && Relationships != "") paste0(paste(Relationships,Degrees, sep = "-"),collapse = "&") else ""
Auto.cal.name        <- paste0(c("Auto.Cal",Sensor_name, Place, format(Cal_Interval[1],"%Y%m%d"), format(Cal_Interval[2],"%Y%m%d"), Sens.Model,Add.Covariates,namesCovariates),collapse = "__")
Ispra.O3  <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, Dir = Dir, Project = Project, DIR_Models = "Models", name.sensor = Sensor_name,
                                  Interval = 5L, DateIN = Cal_Interval[1], DateEND = Cal_Interval[2], Meas.DateIN = Predict_Interval[1], Meas.DateEND = Predict_Interval[2],
                                  Treshold.VIF = 10, Conf.level = 0.075, Mod_type = Sens.Model, Relationships = Relationships,
                                  Add.Covariates = Add.Covariates, DRIFT = FALSE, volt = TRUE, modelled = FALSE, 
                                  Discarded.covariates =Discarded.covariates, Rdata.name.file = paste0(Auto.cal.name,".rdata"), 
                                  Thresh.R2 = -0.03, Register = TRUE, Exclude.Cal = FALSE) # Conf.level 0.075 is particularly for box 40458D

# Comparing all calibration models
List.models.Final <- sapply(List.ASE, function(i){
    ASEDir   <- file.path(Dir, Project, i)
    Model    <- load_obj(file.path(ASEDir,"Models", paste0(Auto.cal.name,".rdata")))
    return(file.path(ASEDir,"Models",Model$Final_median_Model$List.NewModels))
})
Compared.O3.Ispra <- Compare_Models(Exclude.Cal= F, List.models =  List.models.Final, Verbose = T,Meas.DateIN = Predict_Interval[1], Meas.DateEND = Predict_Interval[2])
# 5310CAT with Multilinear----
Sensor_name          <- "5310CAT"
Cal_Interval         <- c(as.Date("2020-11-22"), as.Date("2020-11-29"))
Predict_Interval     <- c(as.Date("2020-10-08"), as.Date("2021-01-13"))
Sens.Model           <- "Linear.Robust"
Add.Covariates       <- TRUE
Relationships        <- NULL
Degrees              <- NULL
Discarded.covariates <- c("NO2_B43F_P1_volt","OX_A431_P1_volt", "NO_B4_P1_volt", "D300_volt")
namesCovariates      <- if (length(Relationships) > 0 && Relationships != "") paste0(paste(Relationships,Degrees, sep = "-"),collapse = "&") else ""
Auto.cal.name        <- paste0(c("Auto.Cal",Sensor_name, Place, format(Cal_Interval[1],"%Y%m%d"), format(Cal_Interval[2],"%Y%m%d"), Sens.Model,Add.Covariates,namesCovariates),collapse = "__")
Ispra.PMS10 <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, Dir = Dir, Project = Project, DIR_Models = "Models", name.sensor = Sensor_name,
                                    Interval = 3L, DateIN = Cal_Interval[1], DateEND = Cal_Interval[2], Meas.DateIN = Predict_Interval[1], Meas.DateEND = Predict_Interval[2], 
                                    Treshold.VIF = 10, Conf.level = 0.075, Mod_type = Sens.Model, Relationships = Relationships,
                                    Add.Covariates = Add.Covariates, DRIFT = FALSE, volt = TRUE, modelled = FALSE, 
                                    Discarded.covariates =Discarded.covariates, Rdata.name.file = paste0(Auto.cal.name,".rdata"), Register = TRUE, Exclude.Cal = FALSE)
# Comparing all calibration models
# List.models.Final <- sapply(List.ASE, function(i){
#     ASEDir   <- file.path(Dir, Project, i)
#     Model    <- load_obj(file.path(ASEDir,"Models", paste0(Auto.cal.name,".rdata")))
#     return(file.path(ASEDir,"Models",Model$Final_median_Model$List.NewModels))
# })
# Compared.PMS10.Ispra.ML <- Compare_Models(Exclude.Cal= F, List.models =  List.models.Final, Verbose = T,Meas.DateIN = Predict_Interval[1], Meas.DateEND = Predict_Interval[2])
# 5310CAT with Kohler correction----
Sensor_name          <- "5310CAT"
Cal_Interval         <- c(as.Date("2020-11-22"), as.Date("2020-11-29"))
Predict_Interval     <- c(as.Date("2020-10-08"), as.Date("2021-01-13"))
Sens.Model           <- "Kohler"
Add.Covariates       <- FALSE
Relationships        <- "Out.Relative_humidity"
Degrees              <- "1"
Discarded.covariates <- c("NO2_B43F_P1_volt","OX_A431_P1_volt", "NO_B4_P1_volt", "D300_volt")
namesCovariates      <- if (length(Relationships) > 0 && Relationships != "") paste0(paste(Relationships,Degrees, sep = "-"),collapse = "&") else ""
Auto.cal.name        <- paste0(c("Auto.Cal",Sensor_name, Place, format(Cal_Interval[1],"%Y%m%d"), format(Cal_Interval[2],"%Y%m%d"), Sens.Model,Add.Covariates,namesCovariates),collapse = "__")
Ispra.PMS10 <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, Dir = Dir, Project = Project, DIR_Models = "Models", name.sensor = Sensor_name,
                                  Interval = 3L, DateIN = Cal_Interval[1], DateEND = Cal_Interval[2], Meas.DateIN = Predict_Interval[1], Meas.DateEND = Predict_Interval[2],
                                  Treshold.VIF = 10, Conf.level = 0.05, Mod_type = Sens.Model, Relationships = Relationships,
                                  Add.Covariates = Add.Covariates, DRIFT = FALSE, volt = TRUE, modelled = FALSE, 
                                  Discarded.covariates =Discarded.covariates, Rdata.name.file = paste0(Auto.cal.name,".rdata"), Register = TRUE, Exclude.Cal = FALSE)

#Ispra.PMS10  <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, name.sensor = "5310CAT",
#                                     Interval = 3L, DateIN = as.Date("2020-01-17"), DateEND = as.Date("2020-01-31"), 
#                                     Treshold.VIF = 10, Conf.level = 0.05, 
#                                     Mod_type = "Kohler", Add.Covariates = FALSE, DRIFT = FALSE, volt = TRUE, modelled = FALSE, Relationships = "Relative_humidity",
#                                     Discarded.covariates = c("NO2_B43F_P1_volt","OX_A431_P1_volt", "NO_B4_P1_volt", "D300_volt"), Register = TRUE)

#Ispra.PMS10  <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, name.sensor = "5310CAT",
                                     #Interval = 3L, DateIN = as.Date("2020-01-17"), DateEND = as.Date("2020-01-31"), 
                                     #Treshold.VIF = 10, Conf.level = 0.05, # 0.25 otherwise probability for a1 does not pass
                                    #Mod_type = "Linear.Robust", Add.Covariates = TRUE, DRIFT = FALSE, volt = TRUE, modelled = FALSE,  Relationships = "Temperature_int",
                                     #Discarded.covariates = c("NO2_B43F_P1_volt","OX_A431_P1_volt", "NO_B4_P1_volt","D300_volt"))
# Comparing all calibration models
List.models.Final <- sapply(List.ASE, function(i){
    ASEDir   <- file.path(Dir, Project, i)
    Model    <- load_obj(file.path(ASEDir,"Models", paste0(Auto.cal.name,".rdata")))
    return(file.path(ASEDir,"Models",Model$Final_median_Model$List.NewModels))
})
Compared.PMS10.Ispra.Kohler <- Compare_Models(Exclude.Cal= F, List.models =  List.models.Final, Verbose = T,Meas.DateIN = Predict_Interval[1], Meas.DateEND = Predict_Interval[2])

# 5310CST with Multilinear----
Sensor_name          <- "5310CST"
Cal_Interval         <- c(as.Date("2020-11-22"), as.Date("2020-11-29"))
Predict_Interval     <- c(as.Date("2020-10-08"), as.Date("2021-01-13"))
Sens.Model           <- "Linear.Robust"
Add.Covariates       <- TRUE
Relationships        <- NULL
Degrees              <- NULL
Discarded.covariates <- c("NO2_B43F_P1_volt","OX_A431_P1_volt", "NO_B4_P1_volt", "D300_volt")
namesCovariates      <- if (length(Relationships) > 0 && Relationships != "") paste0(paste(Relationships,Degrees, sep = "-"),collapse = "&") else ""
Auto.cal.name        <- paste0(c("Auto.Cal",Sensor_name, Place, format(Cal_Interval[1],"%Y%m%d"), format(Cal_Interval[2],"%Y%m%d"), Sens.Model,Add.Covariates,namesCovariates),collapse = "__")
Ispra.PMS10 <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, Dir = Dir, Project = Project, DIR_Models = "Models", name.sensor = Sensor_name,
                                    Interval = 3L, DateIN = Cal_Interval[1], DateEND = Cal_Interval[2], 
                                    Treshold.VIF = 10, Conf.level = 0.05, Mod_type = Sens.Model, Relationships = Relationships,
                                    Add.Covariates = Add.Covariates, DRIFT = FALSE, volt = TRUE, modelled = FALSE, 
                                    Discarded.covariates =Discarded.covariates, Rdata.name.file = paste0(Auto.cal.name,".rdata"), Register = TRUE, Exclude.Cal = FALSE)
# Comparing all calibration models
List.models.Final <- sapply(List.ASE, function(i){
    ASEDir   <- file.path(Dir, Project, i)
    Model    <- load_obj(file.path(ASEDir,"Models", paste0(Auto.cal.name,".rdata")))
    return(file.path(ASEDir,"Models",Model$Final_median_Model$List.NewModels))
})
Compared.CST10.Ispra.ML <- Compare_Models(Exclude.Cal= F, List.models =  List.models.Final, Verbose = T,Meas.DateIN = Predict_Interval[1], Meas.DateEND = Predict_Interval[2])
# 5310CST with Kohler correction----
Sensor_name          <- "5310CST"
Cal_Interval         <- c(as.Date("2020-11-22"), as.Date("2020-11-29"))
Predict_Interval     <- c(as.Date("2020-10-08"), as.Date("2021-01-13"))
Sens.Model           <- "Kohler"
Add.Covariates       <- FALSE
Relationships        <- "Out.Relative_humidity"
Degrees              <- "1"
Discarded.covariates <- c("NO2_B43F_P1_volt","OX_A431_P1_volt", "NO_B4_P1_volt", "D300_volt")
namesCovariates      <- if (length(Relationships) > 0 && Relationships != "") paste0(paste(Relationships,Degrees, sep = "-"),collapse = "&") else ""
Auto.cal.name        <- paste0(c("Auto.Cal",Sensor_name, Place, format(Cal_Interval[1],"%Y%m%d"), format(Cal_Interval[2],"%Y%m%d"), Sens.Model,Add.Covariates,namesCovariates),collapse = "__")
Ispra.PMS10 <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, Dir = Dir, Project = Project, DIR_Models = "Models", name.sensor = Sensor_name,
                                    Interval = 3L, DateIN = Cal_Interval[1], DateEND = Cal_Interval[2], 
                                    Treshold.VIF = 10, Conf.level = 0.05, Mod_type = Sens.Model, Relationships = Relationships,
                                    Add.Covariates = Add.Covariates, DRIFT = FALSE, volt = TRUE, modelled = FALSE, 
                                    Discarded.covariates =Discarded.covariates, Rdata.name.file = paste0(Auto.cal.name,".rdata"), Register = TRUE, Exclude.Cal = FALSE)

# Comparing all calibration models
List.models.Final <- sapply(List.ASE, function(i){
    ASEDir   <- file.path(Dir, Project, i)
    Model    <- load_obj(file.path(ASEDir,"Models", paste0(Auto.cal.name,".rdata")))
    return(file.path(ASEDir,"Models",Model$Final_median_Model$List.NewModels))
})
Compared.CST10.Ispra.Kohler <- Compare_Models(Exclude.Cal= F, List.models =  List.models.Final, Verbose = T,Meas.DateIN = Predict_Interval[1], Meas.DateEND = Predict_Interval[2])

# The Kohler model is worse
# OPCN3PM10 ----
# Multi Linear
Sensor_name          <- "OPCN3PM10"
Cal_Interval         <- c(as.Date("2020-11-22"), as.Date("2020-11-29"))
Predict_Interval     <- c(as.Date("2020-10-08"), as.Date("2021-01-13"))
Sens.Model           <- "Linear.Robust"
Add.Covariates       <- FALSE
Relationships        <- NULL
Degrees              <- "1"
Discarded.covariates <- c("NO2_B43F_P1_volt","OX_A431_P1_volt", "NO_B4_P1_volt", "D300_volt")
namesCovariates      <- if (length(Relationships) > 0 && Relationships != "") paste0(paste(Relationships,Degrees, sep = "-"),collapse = "&") else ""
Auto.cal.name        <- paste0(c("Auto.Cal",Sensor_name, Place, format(Cal_Interval[1],"%Y%m%d"), format(Cal_Interval[2],"%Y%m%d"), Sens.Model,Add.Covariates,namesCovariates),collapse = "__")
Ispra.OPC10 <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, Dir = Dir, Project = Project, DIR_Models = "Models", name.sensor = Sensor_name,
                                  Interval = 3L, DateIN = Cal_Interval[1], DateEND = Cal_Interval[2], 
                                  Treshold.VIF = 10, Conf.level = 0.05, Mod_type = Sens.Model, Relationships = Relationships,
                                  Add.Covariates = Add.Covariates, DRIFT = FALSE, volt = TRUE, modelled = FALSE, 
                                  Discarded.covariates =Discarded.covariates, Rdata.name.file = paste0(Auto.cal.name,".rdata"), Register = TRUE, Exclude.Cal = FALSE)
#Ispra.OPC10  <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, name.sensor = "OPCN3PM10",
                                     #Interval = 3L, DateIN = as.Date("2020-01-17"), DateEND = as.Date("2020-01-31"), 
                                     #Treshold.VIF = 10, Conf.level = 0.05, # 0.25 otherwise probability for a1 does not pass
                                     #Mod_type = "Kohler", Add.Covariates = FALSE, DRIFT = FALSE, volt = TRUE, modelled = FALSE, Relationships = "Relative_humidity", 
                                     #Discarded.covariates = c("NO2_B43F_P1_volt","OX_A431_P1_volt", "NO_B4_P1_volt", "D300_volt"))# Updating configuration with new Calibration Model
#Ispra.OPC10  <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, name.sensor = "OPCN3PM10",
#                                     Interval = 3L, DateIN = as.Date("2020-01-17"), DateEND = as.Date("2020-01-31"), 
#                                     Treshold.VIF = 10, Conf.level = 0.05, # 0.25 otherwise probability for a1 does not pass
#                                     Mod_type = "Linear.Robust", Add.Covariates = TRUE, DRIFT = FALSE, volt = TRUE, modelled = FALSE, 
#                                     Discarded.covariates = c("NO2_B43F_P1_volt","OX_A431_P1_volt", "NO_B4_P1_volt", "D300_volt"))# Updating configuration with new Calibration Model
# Comparing all calibration models
List.models.Final <- sapply(List.ASE, function(i){
    ASEDir   <- file.path(Dir, Project, i)
    Model    <- load_obj(file.path(ASEDir,"Models", paste0(Auto.cal.name,".rdata")))
    return(file.path(ASEDir,"Models",Model$Final_median_Model$List.NewModels))
})
Compared.OPC10.Ispra <- Compare_Models(Exclude.Cal= F, List.models =  List.models.Final, Verbose = T,Meas.DateIN = Predict_Interval[1], Meas.DateEND = Predict_Interval[2])

# Kohler
Sensor_name          <- "OPCN3PM10"
Cal_Interval         <- c(as.Date("2020-11-22"), as.Date("2020-11-29"))
Predict_Interval     <- c(as.Date("2020-10-08"), as.Date("2021-01-13"))
Sens.Model           <- "Kohler"
Add.Covariates       <- FALSE
Relationships        <- "Out.Relative_humidity"
Degrees              <- "1"
Discarded.covariates <- c("NO2_B43F_P1_volt","OX_A431_P1_volt", "NO_B4_P1_volt", "D300_volt")
namesCovariates      <- if (length(Relationships) > 0 && Relationships != "") paste0(paste(Relationships,Degrees, sep = "-"),collapse = "&") else ""
Auto.cal.name        <- paste0(c("Auto.Cal",Sensor_name, Place, format(Cal_Interval[1],"%Y%m%d"), format(Cal_Interval[2],"%Y%m%d"), Sens.Model,Add.Covariates,namesCovariates),collapse = "__")
Ispra.OPC10 <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, Dir = Dir, Project = Project, DIR_Models = "Models", name.sensor = Sensor_name,
                                    Interval = 3L, DateIN = Cal_Interval[1], DateEND = Cal_Interval[2], 
                                    Treshold.VIF = 10, Conf.level = 0.05, Mod_type = Sens.Model, Relationships = Relationships,
                                    Add.Covariates = Add.Covariates, DRIFT = FALSE, volt = TRUE, modelled = FALSE, 
                                    Discarded.covariates =Discarded.covariates, Rdata.name.file = paste0(Auto.cal.name,".rdata"), Register = TRUE, Exclude.Cal = FALSE)
# Comparing all calibration models
List.models.Final <- sapply(List.ASE, function(i){
    ASEDir   <- file.path(Dir, Project, i)
    Model    <- load_obj(file.path(ASEDir,"Models", paste0(Auto.cal.name,".rdata")))
    return(file.path(ASEDir,"Models",Model$Final_median_Model$List.NewModels))
})
Compared.OPC10.Ispra.Kohler <- Compare_Models(Exclude.Cal= F, List.models =  List.models.Final, Verbose = T,Meas.DateIN = Predict_Interval[1], Meas.DateEND = Predict_Interval[2])

# BMP280 ----
Sensor_name          <- "BMP280"
Cal_Interval         <- c(as.Date("2020-11-22"), as.Date("2020-11-29"))
Predict_Interval     <- c(as.Date("2020-10-08"), as.Date("2021-01-13"))
Sens.Model           <- "Linear.Robust"
Add.Covariates       <- TRUE
Relationships        <- NULL
Degrees              <- "1"
Discarded.covariates <- c("NO2_B43F_P1_volt","OX_A431_P1_volt","5310CAT_volt","OPCN3PM10_volt","NO_B4_P1_volt","D300_volt","CO_A4_P1_volt")
namesCovariates      <- if (length(Relationships) > 0 && Relationships != "") paste0(paste(Relationships,Degrees, sep = "-"),collapse = "&") else ""
Auto.cal.name        <- paste0(c("Auto.Cal",Sensor_name, Place, format(Cal_Interval[1],"%Y%m%d"), format(Cal_Interval[2],"%Y%m%d"), Sens.Model,Add.Covariates,namesCovariates),collapse = "__")
Ispra.AtmPres <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, Dir = Dir, Project = Project, DIR_Models = "Models", name.sensor = Sensor_name,
                                    Interval = 3L, DateIN = Cal_Interval[1], DateEND = Cal_Interval[2], 
                                    Treshold.VIF = 10, Conf.level = 0.05, Mod_type = Sens.Model, Relationships = Relationships,
                                    Add.Covariates = Add.Covariates, DRIFT = FALSE, volt = TRUE, modelled = FALSE, 
                                    Discarded.covariates =Discarded.covariates, Rdata.name.file = paste0(Auto.cal.name,".rdata"), Register = TRUE, Exclude.Cal = FALSE)
#Ispra.AtmPres  <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, name.sensor = "BMP280",
#                                     Interval = 3L, DateIN = as.Date("2020-01-17"), DateEND = as.Date("2020-01-31"), 
#                                     Treshold.VIF = 10, Conf.level = 0.05, # 0.25 otherwise probability for a1 does not pass
#                                     Discarded.covariates = c("NO2_B43F_P1_volt","OX_A431_P1_volt","OPCN3PM10","5310CAT","NO_B4_P1","D300","CO_A4_P1"))# Updating configuration with new Calibration Model
# Comparing all calibration models
List.models.Final <- sapply(List.ASE, function(i){
    ASEDir   <- file.path(Dir, Project, i)
    Model    <- load_obj(file.path(ASEDir,"Models", paste0(Auto.cal.name,".rdata")))
    return(file.path(ASEDir,"Models",Model$Final_median_Model$List.NewModels))
})
Compared.AtmPres.Ispra <- Compare_Models(Exclude.Cal= F, List.models =  List.models.Final, Verbose = T,Meas.DateIN = Predict_Interval[1], Meas.DateEND = Predict_Interval[2])
# RH_SHT31HE ----
Sensor_name          <- "SHT31HE"
Cal_Interval         <- c(as.Date("2020-11-22"), as.Date("2020-11-29"))
Predict_Interval     <- c(as.Date("2020-10-08"), as.Date("2021-01-13"))
Sens.Model           <- "Linear.Robust"
Add.Covariates       <- TRUE
Relationships        <- NULL
Degrees              <- "1"
Discarded.covariates <- c("NO2_B43F_P1_volt","OX_A431_P1_volt","5310CAT_volt","OPCN3PM10_volt","NO_B4_P1_volt","D300_volt","CO_A4_P1_volt")
namesCovariates      <- if (length(Relationships) > 0 && Relationships != "") paste0(paste(Relationships,Degrees, sep = "-"),collapse = "&") else ""
Auto.cal.name        <- paste0(c("Auto.Cal",Sensor_name, Place, format(Cal_Interval[1],"%Y%m%d"), format(Cal_Interval[2],"%Y%m%d"), Sens.Model,Add.Covariates,namesCovariates),collapse = "__")
Ispra.RH <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, Dir = Dir, Project = Project, DIR_Models = "Models", name.sensor = Sensor_name,
                                      Interval = 3L, DateIN = Cal_Interval[1], DateEND = Cal_Interval[2], 
                                      Treshold.VIF = 10, Conf.level = 0.05, Mod_type = Sens.Model, Relationships = Relationships,
                                      Add.Covariates = Add.Covariates, DRIFT = FALSE, volt = TRUE, modelled = FALSE, 
                                      Discarded.covariates =Discarded.covariates, Rdata.name.file = paste0(Auto.cal.name,".rdata"), Register = TRUE, Exclude.Cal = FALSE)
#Ispra.RH  <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, name.sensor = "SHT31HE",
#                                       Interval = 3L, DateIN = as.Date("2020-01-17"), DateEND = as.Date("2020-01-31"), 
#                                       Treshold.VIF = 10, Conf.level = 0.05, # 0.25 otherwise probability for a1 does not pass
#                                       Mod_type = "Linear.Robust", Add.Covariates = TRUE, DRIFT = FALSE, volt = TRUE, modelled = FALSE, 
#                                       Discarded.covariates = c("NO2_B43F_P1_volt","OX_A431_P1_volt","OPCN3PM10","5310CAT","NO_B4_P1","D300","CO_A4_P1"))# Updating configuration with new Calibration Model
# Comparing all calibration models
List.models.Final <- sapply(List.ASE, function(i){
    ASEDir   <- file.path(Dir, Project, i)
    Model    <- load_obj(file.path(ASEDir,"Models", paste0(Auto.cal.name,".rdata")))
    return(file.path(ASEDir,"Models",Model$Final_median_Model$List.NewModels))
})
Compared.RH.Ispra <-Compare_Models(Exclude.Cal= F, List.models =  List.models.Final, Verbose = T,Meas.DateIN = Predict_Interval[1], Meas.DateEND = Predict_Interval[2])
# Temperature_SHT31TE ----
Sensor_name          <- "SHT31TE"
Cal_Interval         <- c(as.Date("2020-11-22"), as.Date("2020-11-29"))
Predict_Interval     <- c(as.Date("2020-10-08"), as.Date("2021-01-13"))
Sens.Model           <- "Linear.Robust"
Add.Covariates       <- TRUE
Relationships        <- NULL
Degrees              <- "1"
Discarded.covariates <- c("NO2_B43F_P1_volt","OX_A431_P1_volt","5310CAT_volt","OPCN3PM10_volt","NO_B4_P1_volt","D300_volt","CO_A4_P1_volt")
namesCovariates      <- if (length(Relationships) > 0 && Relationships != "") paste0(paste(Relationships,Degrees, sep = "-"),collapse = "&") else ""
Auto.cal.name        <- paste0(c("Auto.Cal",Sensor_name, Place, format(Cal_Interval[1],"%Y%m%d"), format(Cal_Interval[2],"%Y%m%d"), Sens.Model,Add.Covariates,namesCovariates),collapse = "__")
Ispra.Temp <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, Dir = Dir, Project = Project, DIR_Models = "Models", name.sensor = Sensor_name,
                                 Interval = 3L, DateIN = Cal_Interval[1], DateEND = Cal_Interval[2], 
                                 Treshold.VIF = 10, Conf.level = 0.05, Mod_type = Sens.Model, Relationships = Relationships,
                                 Add.Covariates = Add.Covariates, DRIFT = FALSE, volt = TRUE, modelled = FALSE, 
                                 Discarded.covariates =Discarded.covariates, Rdata.name.file = paste0(Auto.cal.name,".rdata"), Register = TRUE)
#Ispra.Temp  <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, name.sensor = "SHT31TE",
#                                  Interval = 3L, DateIN = as.Date("2020-01-17"), DateEND = as.Date("2020-01-31"), 
#                                  Treshold.VIF = 10, Conf.level = 0.05, # 0.25 otherwise probability for a1 does not pass
#                                  Mod_type = "Linear.Robust", Add.Covariates = TRUE, DRIFT = FALSE, volt = TRUE, modelled = FALSE, 
#                                  Discarded.covariates = c("NO2_B43F_P1_volt","OX_A431_P1_volt","OPCN3PM10","5310CAT","NO_B4_P1","D300","CO_A4_P1"))# Updating configuration with new Calibration Model

# Comparing all calibration models
List.models.Final <- sapply(List.ASE, function(i){
    ASEDir   <- file.path(Dir, Project, i)
    Model    <- load_obj(file.path(ASEDir,"Models", paste0(Auto.cal.name,".rdata")))
    return(file.path(ASEDir,"Models",Model$Final_median_Model$List.NewModels))
})
Compared.Temp.Ispra <- Compare_Models(Exclude.Cal= F, List.models =  List.models.Final, Verbose = T,Meas.DateIN = Predict_Interval[1], Meas.DateEND = Predict_Interval[2])
