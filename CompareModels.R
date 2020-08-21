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
librarian::shelf(gplots) # heatmap2

futile.logger::flog.info("[Global] List of installed packages")
print(search(), quote = FALSE)
cat("\n")

##########################################
# Selection of ASE boxes ####
##########################################C
#list.dirs(file.path(getwd(), "ASE_Boxes"),full.names = FALSE, recursive = FALSE)
List.ASE <- c("40458D","4047D0","406414","40641B","40642E","4065D0","4065E0","4065E3","4065ED","40816F")
List.ASE <- c("40641B")

##########################################
# checking if new data are available ####
##########################################C
for (i in List.ASE) influx.downloadAndPredict(boxName = i, boxConfig = influx.getConfig(boxName = i), Add.Ref = TRUE)

##########################################
# Calibrating
##########################################C
# CO_A4_P1 ----
Ispra.CO  <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, name.sensor = "CO_A4_P1", 
                                  Interval = 1, DateIN = as.Date("2020-01-19"), DateEND = as.Date("2020-01-31"), 
                                  Add.Covariates = TRUE, DRIFT = TRUE, volt = TRUE, modelled = FALSE, Discarded.covariates = "NO2_B43F_P1_volt")
# Comparing all calibration models
List.models.Final <- sapply(List.ASE, function(i){
    ASEDir   <- file.path(Dir, "ASE_Boxes", i)
    Model    <- load_obj(file.path(ASEDir,"Models", paste0("Auto.Cal","CO_A4_P1",".rdata")))
    return(file.path(ASEDir,"Models",Model$Final_median_Model$List.NewModels))
})
Compared.CO.Ispra <- Compare_Models(List.models =  List.models.Final, Verbose = T)
# D300 ----
Ispra.CO2 <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, name.sensor = "D300",
                                   Interval = 1, DateIN = as.Date("2020-01-17"), DateEND = as.Date("2020-01-29"), 
                                   DRIFT = TRUE, volt = TRUE, modelled = TRUE, Register = TRUE)
# Comparing all calibration models
List.models.Final <- sapply(List.ASE, function(i){
    ASEDir   <- file.path(Dir, "ASE_Boxes", i)
    Model    <- load_obj(file.path(ASEDir,"Models", paste0("Auto.Cal","D300",".rdata")))
    return(file.path(ASEDir,"Models",Model$Final_median_Model$List.NewModels))
})
Compare_Models(List.models =  List.models.Final, Verbose = T)
# NO_A4_P1 ----
# exp of Temperature_int
Ispra.NO  <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, name.sensor = "NO_B4_P1",
                                  Interval = 1L, DateIN = as.Date("2020-01-17"), DateEND = as.Date("2020-01-31"), 
                                  Relationships = c("Temperature_int"), degrees = "ExpGrowth", 
                                  Treshold.VIF = 10, Conf.level = 0.10, 
                                  Add.Covariates = TRUE, DRIFT = TRUE, volt = TRUE, modelled = FALSE, Discarded.covariates = "5310CAT_volt", Register = FALSE)
# Multilinear
Ispra.NO  <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, name.sensor = "NO_B4_P1",
                                  Interval = 1, DateIN = as.Date("2020-01-17"), DateEND = as.Date("2020-01-31"), 
                                  Treshold.VIF = 10, Conf.level = 0.30, 
                                  Add.Covariates = TRUE, DRIFT = TRUE, volt = TRUE, modelled = FALSE, Discarded.covariates = "5310CAT_volt", Register = FALSE)
# "exp_kT_NoC"  with temperature
Ispra.NO  <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, name.sensor = "NO_B4_P1",
                                  Interval = 1L, DateIN = as.Date("2020-01-19"), DateEND = as.Date("2020-01-31"), 
                                  Treshold.VIF = 10, Conf.level = 0.10, Mod_type = "exp_kT_NoC", Relationships = "Temperature",
                                  Add.Covariates = FALSE, DRIFT = TRUE, volt = TRUE, modelled = FALSE, Discarded.covariates = "5310CAT_volt")
# "exp_kT_NoC" with temperature_int
Ispra.NO  <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, name.sensor = "NO_B4_P1",
                                  Interval = 1, DateIN = as.Date("2020-01-19"), DateEND = as.Date("2020-01-31"), 
                                  Treshold.VIF = 10, Conf.level = 0.10, Mod_type = "exp_kT_NoC", Relationships = "Temperature_int",
                                  Add.Covariates = FALSE, DRIFT = TRUE, volt = TRUE, modelled = FALSE, Discarded.covariates = "5310CAT_volt")
# Comparing all calibration models
List.models.Final <- sapply(List.ASE, function(i){
    ASEDir   <- file.path(Dir, "ASE_Boxes", i)
    Model    <- load_obj(file.path(ASEDir,"Models", paste0("Auto.Cal","NO_B4_P1",".rdata")))
    return(file.path(ASEDir,"Models",Model$Final_median_Model$List.NewModels))
})
Compared.NO.Ispra <- Compare_Models(List.models =  List.models.Final, Verbose = T,Meas.DateIN = as.Date("2020-01-19"), Meas.DateEND = as.Date("2020-01-31"))
# "T_power" with temperature
Ispra.NO  <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, name.sensor = "NO_B4_P1",
                                  Interval = 1, DateIN = as.Date("2020-01-19"), DateEND = as.Date("2020-01-31"), 
                                  Treshold.VIF = 10, Conf.level = 0.10, Mod_type = "T_power", Relationships = "Temperature",
                                  Add.Covariates = FALSE, DRIFT = TRUE, volt = TRUE, modelled = FALSE, Discarded.covariates = "5310CAT_volt")
# Comparing all calibration models
List.models.Final <- sapply(List.ASE, function(i){
    ASEDir   <- file.path(Dir, "ASE_Boxes", i)
    Model    <- load_obj(file.path(ASEDir,"Models", paste0("Auto.Cal","NO_B4_P1",".rdata")))
    return(file.path(ASEDir,"Models",Model$Final_median_Model$List.NewModels))
})
Compared.NO.Ispra <- Compare_Models(List.models =  List.models.Final, Verbose = T)

# NO2_B43F_P1 ----
Ispra.NO2 <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, name.sensor = "NO2_B43F_P1",
                                  Interval = 1, DateIN = as.Date("2020-01-17"), DateEND = as.Date("2020-01-31"), 
                                  Mod_type = "Linear.Robust", Add.Covariates = TRUE, DRIFT = TRUE, volt = TRUE, modelled = FALSE, Discarded.covariates = "5310CAT_volt")
# Comparing all calibration models
List.models.Final <- sapply(List.ASE, function(i){
    ASEDir   <- file.path(Dir, "ASE_Boxes", i)
    Model    <- load_obj(file.path(ASEDir,"Models", paste0("Auto.Cal","NO2_B43F_P1",".rdata")))
    return(file.path(ASEDir,"Models",Model$Final_median_Model$List.NewModels))
})
Compare_Models(List.models =  List.models.Final, Verbose = T)
# OX_A431_P1 ----
Ispra.O3  <- AutoCal.Boxes.Sensor(List.ASE = List.ASE, name.sensor = "OX_A431_P1",
                                  Interval = 1, DateIN = as.Date("2020-01-17"), DateEND = as.Date("2020-01-31"), 
                                  Mod_type = "Linear.Robust", Add.Covariates = TRUE, DRIFT = TRUE, volt = TRUE, modelled = FALSE, 
                                  Discarded.covariates = "5310CAT_volt")
# Updating configuration with new Calibration Model
for (i in List.models.Final) Register.Model(i)


##########################################
# Box per box 40641B ####
##########################################C
# Init
ASEDir   <- file.path(Dir, "ASE_Boxes", "40641B")
#------------------------------------------C
# CO  "CO_A4_P1"between 19 and 30 January ----
#------------------------------------------C
List.models <- List_models(ASEDir, "CO_A4_P1")
ASE.ID      <- Identify_ASE(Model = file.path(ASEDir,"Models", List.models[1]))
assign(paste0("Fit.CO_", "40641B"), 
       Auto.Cal(ASEDir, ASE.ID = ASE.ID, name.sensor = "CO_A4_P1", Interval = 1, DateIN = as.Date("2020-01-19"), DateEND = as.Date("2020-01-31"), date = TRUE, volt = FALSE, modelled = FALSE))
#------------------------------------------C
# CO2  "D300"between 19 and 30 January ----
#------------------------------------------C
List.models    <- List_models(ASEDir, "D300")
ASE.ID         <- Identify_ASE(Model = file.path(ASEDir,"Models", List.models[1]))
Fit.CO2  <- Auto.Cal(ASEDir, ASE.ID = ASE.ID, name.sensor = "D300", Interval = 1, DateIN = NULL, DateEND = as.Date("2020-01-29"), date = TRUE, volt = FALSE)
#------------------------------------------C
# NO "NO_B4_P1" between 17 and 30 January----
#------------------------------------------C
ASE.ID      <- Identify_ASE(Model = file.path(ASEDir,"Models", List_models(ASEDir = ASEDir, name.sensor = "NO_B4_P1")[1]))
List.models <- List_models(ASEDir, "NO_B4_P1")
Fit.NO      <- Auto.Cal(ASEDir, name.sensor = "NO_B4_P1", Interval = 1, DateIN = as.Date("2020-01-21"), DateEND = NULL, date = FALSE, volt = TRUE, modelled = FALSE, 
                        Mod_type = "Linear.Robust", Relationships = c("Temperature_int"), degrees = "ExpGrowth", Add.Covariates = TRUE)
Fit.NO      <- Auto.Cal(ASEDir, name.sensor = "NO_B4_P1", Interval = 1, DateIN = as.Date("2020-01-21"), DateEND = NULL, date = FALSE, volt = TRUE, modelled = FALSE, 
                        Mod_type = "Linear.Robust", Add.Covariates = TRUE)
#------------------------------------------C
# NO2 "NO2_B43F_P1" between 17 and 30 January----
#------------------------------------------C
List.models    <- List_models(ASEDir, "NO2_B43F_P1")
ASE.ID <- Identify_ASE(Model = file.path(ASEDir,"Models", List_models(ASEDir = ASEDir, name.sensor = "NO2_B43F_P1")[1]))
Fit.NO2 <- Auto.Cal(ASEDir, name.sensor = "NO2_B43F_P1", Interval = 1, DateIN = NULL, DateEND = NULL, date = FALSE, volt = TRUE, modelled = FALSE)
Fit.NO2 <- Auto.Cal(ASEDir, name.sensor = "NO2_B43F_P1", Interval = 1, DateIN = NULL, DateEND = NULL, date = FALSE, volt = TRUE, modelled = FALSE,
                    Mod_type = "Linear.Robust", Relationships = c("Temperature_int", "Td_deficit"), Add.Covariates = TRUE)
#------------------------------------------C
# O3 "OX_A431_P1" between 17 and 30 January----
#------------------------------------------C
ASE.ID      <- Identify_ASE(Model = file.path(ASEDir,"Models", List_models(ASEDir = ASEDir, name.sensor = "OX_A431_P1")[1]))
List.models <- List_models(ASEDir, "OX_A431_P1")
Fit.O3      <- Auto.Cal(ASEDir, name.sensor = "OX_A431_P1", Interval = 1, DateIN = NULL, DateEND = NULL, date = FALSE, volt = TRUE, modelled = FALSE)
#------------------------------------------C
# PM10 "5310CAT" between 17 and 30 January----
#------------------------------------------C
ASE.ID      <- Identify_ASE(Model = file.path(ASEDir,"Models", List_models(ASEDir = ASEDir, name.sensor = "5310CAT")[1]))
List.models <- List_models(ASEDir, "5310CAT")
Fit.PM10PMS <- Auto.Cal(ASEDir, name.sensor = "5310CAT", Interval = 1, DateIN = as.Date("2020-01-21"), DateEND = NULL, date = FALSE, volt = TRUE, modelled = FALSE)
##########################################
# Box per box 40458D ####
##########################################C
# Init
ASEDir   <- file.path(Dir, "ASE_Boxes", List.ASE[1])
#------------------------------------------C
# CO  "CO_A4_P1"between 19 and 30 January ----
#------------------------------------------C
List.models <- List_models(ASEDir, "CO_A4_P1")
ASE.ID      <- Identify_ASE(Model = file.path(ASEDir,"Models", List.models[1]))
Fit.CO      <- Auto.Cal(ASEDir, name.sensor = "CO_A4_P1", Interval = 1, DateIN = NULL, DateEND = NULL, date = TRUE, volt = FALSE)
#------------------------------------------C
# CO2  "D300"between 19 and 30 January ----
#------------------------------------------C
List.models    <- List_models(ASEDir, "D300")
ASE.ID         <- Identify_ASE(Model = file.path(ASEDir,"Models", List.models[1]))
Fit.CO2  <- Auto.Cal(ASEDir, name.sensor = "D300", Interval = 1, DateIN = NULL, DateEND = as.Date("2020-01-29"), date = TRUE, volt = FALSE)
#------------------------------------------C
# NO "NO_B4_P1" between 17 and 30 January----
#------------------------------------------C
ASE.ID      <- Identify_ASE(Model = file.path(ASEDir,"Models", List_models(ASEDir = ASEDir, name.sensor = "NO_B4_P1")[1]))
List.models <- List_models(ASEDir, "NO_B4_P1")
Fit.NO      <- Auto.Cal(ASEDir, name.sensor = "NO_B4_P1", Interval = 1, DateIN = as.Date("2020-01-21"), DateEND = NULL, date = FALSE, volt = TRUE, modelled = FALSE, 
                        Mod_type = "Linear.Robust", Relationships = c("Temperature_int"), degrees = "ExpGrowth", Add.Covariates = TRUE)
Fit.NO      <- Auto.Cal(ASEDir, name.sensor = "NO_B4_P1", Interval = 1, DateIN = as.Date("2020-01-21"), DateEND = NULL, date = FALSE, volt = TRUE, modelled = FALSE, 
                        Mod_type = "Linear.Robust", Add.Covariates = TRUE)
#------------------------------------------C
# NO2 "NO2_B43F_P1" between 17 and 30 January----
#------------------------------------------C
List.models    <- List_models(ASEDir, "NO2_B43F_P1")
ASE.ID <- Identify_ASE(Model = file.path(ASEDir,"Models", List_models(ASEDir = ASEDir, name.sensor = "NO2_B43F_P1")[1]))
Fit.NO2 <- Auto.Cal(ASEDir, name.sensor = "NO2_B43F_P1", Interval = 1, DateIN = NULL, DateEND = NULL, date = FALSE, volt = TRUE, modelled = FALSE)
Fit.NO2 <- Auto.Cal(ASEDir, name.sensor = "NO2_B43F_P1", Interval = 1, DateIN = NULL, DateEND = NULL, date = FALSE, volt = TRUE, modelled = FALSE,
                    Mod_type = "Linear.Robust", Relationships = c("Temperature_int", "Td_deficit"), Add.Covariates = TRUE)
#------------------------------------------C
# O3 "OX_A431_P1" between 17 and 30 January----
#------------------------------------------C
ASE.ID      <- Identify_ASE(Model = file.path(ASEDir,"Models", List_models(ASEDir = ASEDir, name.sensor = "OX_A431_P1")[1]))
List.models <- List_models(ASEDir, "OX_A431_P1")
Fit.O3      <- Auto.Cal(ASEDir, name.sensor = "OX_A431_P1", Interval = 1, DateIN = NULL, DateEND = NULL, date = FALSE, volt = TRUE, modelled = FALSE)
#------------------------------------------C
# PM10 "5310CAT" between 17 and 30 January----
#------------------------------------------C
ASE.ID      <- Identify_ASE(Model = file.path(ASEDir,"Models", List_models(ASEDir = ASEDir, name.sensor = "5310CAT")[1]))
List.models <- List_models(ASEDir, "5310CAT")
Fit.PM10PMS <- Auto.Cal(ASEDir, name.sensor = "5310CAT", Interval = 1, DateIN = as.Date("2020-01-21"), DateEND = NULL, date = FALSE, volt = TRUE, modelled = FALSE)

##########################################
# Box per box 40642E ####
##########################################C
# Init
ASEDir   <- file.path(Dir, "ASE_Boxes", List.ASE[5])
#------------------------------------------C
# CO  "CO_A4_P1"between 19 and 30 January ----
#------------------------------------------C
List.models    <- List_models(ASEDir, "CO_A4_P1")
ASE.ID         <- Identify_ASE(Model = file.path(ASEDir,"Models", List.models[1]))
Fit.CO  <- Auto.Cal(ASEDir = ASEDir, ASE.ID = ASE.ID, name.sensor = "CO_A4_P1", Interval = 1, DateIN = as.Date("2020-01-19"), DateEND = as.Date("2020-01-31"), date = TRUE, volt = FALSE)
#------------------------------------------C
# CO2  "D300"between 19 and 30 January ----
#------------------------------------------C
List.models    <- List_models(ASEDir, "D300")
ASE.ID         <- Identify_ASE(Model = file.path(ASEDir,"Models", List.models[1]))
Fit.CO2  <- Auto.Cal(ASEDir, name.sensor = "D300", Interval = 1, DateIN = NULL, DateEND = as.Date("2020-01-29"), date = TRUE, volt = FALSE)
#------------------------------------------C
# NO "NO_B4_P1" between 17 and 30 January----
#------------------------------------------C
List.models <- List_models(ASEDir, "NO_B4_P1")
ASE.ID      <- Identify_ASE_Dir(ASEDir = ASEDir, name.sensor = "NO_B4_P1")
Fit.NO      <- Auto.Cal(ASEDir, ASE.ID = ASE.ID, name.sensor = "NO_B4_P1", Interval = 1, DateIN = as.Date("2020-01-21"), DateEND = as.Date("2020-01-31"), 
                        date = FALSE, volt = TRUE, modelled = FALSE, Mod_type = "Linear.Robust", Relationships = c("Temperature_int"), degrees = "ExpGrowth", Add.Covariates = TRUE)
Fit.NO      <- Auto.Cal(ASEDir, ASE.ID = ASE.ID, name.sensor = "NO_B4_P1", Interval = 1, DateIN = as.Date("2020-01-21"), DateEND = as.Date("2020-01-31"), 
                        date = FALSE, volt = TRUE, modelled = FALSE, Mod_type = "Linear.Robust", Add.Covariates = TRUE)
#------------------------------------------C
# NO2 "NO2_B43F_P1" between 17 and 30 January----
#------------------------------------------C
List.models    <- List_models(ASEDir, "NO2_B43F_P1")
ASE.ID <- Identify_ASE(Model = file.path(ASEDir,"Models", List_models(ASEDir = ASEDir, name.sensor = "NO2_B43F_P1")[1]))
Fit.NO2 <- Auto.Cal(ASEDir, name.sensor = "NO2_B43F_P1", Interval = 1, DateIN = NULL, DateEND = NULL, date = FALSE, volt = TRUE, modelled = FALSE)
Fit.NO2 <- Auto.Cal(ASEDir, name.sensor = "NO2_B43F_P1", Interval = 1, DateIN = NULL, DateEND = NULL, date = FALSE, volt = TRUE, modelled = FALSE,
                    Mod_type = "Linear.Robust", Relationships = c("Temperature_int", "Td_deficit"), Add.Covariates = TRUE)
#------------------------------------------C
# O3 "OX_A431_P1" between 17 and 30 January----
#------------------------------------------C
ASE.ID      <- Identify_ASE(Model = file.path(ASEDir,"Models", List_models(ASEDir = ASEDir, name.sensor = "OX_A431_P1")[1]))
List.models <- List_models(ASEDir, "OX_A431_P1")
Fit.O3      <- Auto.Cal(ASEDir, name.sensor = "OX_A431_P1", Interval = 1, DateIN = NULL, DateEND = NULL, date = FALSE, volt = TRUE, modelled = FALSE)
#------------------------------------------C
# PM10 "5310CAT" between 17 and 30 January----
#------------------------------------------C
ASE.ID      <- Identify_ASE(Model = file.path(ASEDir,"Models", List_models(ASEDir = ASEDir, name.sensor = "5310CAT")[1]))
List.models <- List_models(ASEDir, "5310CAT")
Fit.PM10PMS <- Auto.Cal(ASEDir, name.sensor = "5310CAT", Interval = 1, DateIN = as.Date("2020-01-21"), DateEND = NULL, date = FALSE, volt = TRUE, modelled = FALSE)


##########################################C
# Box per box 4065D0 ####
##########################################C
# Init
ASEDir       <- file.path(Dir, "ASE_Boxes", List.ASE[6])
#------------------------------------------C
# CO ----
#------------------------------------------C
ASE.ID <- Identify_ASE(Model = file.path(ASEDir,"Models", List_models(ASEDir = ASEDir, name.sensor = "CO_A4_P1")[1]))
# Fitting all Linear.Robust models over the whole period
List.CO.Robust <- Roll_Fit_New_Model(ASEDir = ASEDir, Interval = 5, 
                                      name.sensors = ASE.ID$name.sensor, Mod_type = "Linear.Robust",
                                      DateIN = as.Date("2020-01-17"), DateEND = as.Date("2020-01-30"))
ASE.ID       <- Identify_ASE(Model = file.path(ASEDir,"Models", List.CO.Robust[1]))
# Comparing all models of selected boxes in a vector of filepaths (ASEDir), saving and returning the comparisons
All.Compare.CO.Robust <- List_All_Compare(ASEDir, name.sensors = ASE.ID$name.sensor, All.Models = basename(List.CO.Robust))
# Confidence interval of coefficents of Models for Calibration models
Table.Coeffs.CO.Robust <- Confidence_Coeffs(All.Compare = All.Compare.CO.Robust, Mod_type = ASE.ID$Mod_type)
Median.Model.CO.Robust <- Median_Model(ASEDir, name.sensors = NULL, Table.Coeffs = Table.Coeffs.CO.Robust, 
                                            Mod_type = ASE.ID$Mod_type, All.Compare = All.Compare.CO.Robust)
Co_variates.CO.Robust <- List_Covariates(Median.Model.CO.Robust)
#    row.names CO_A4_P1_volt Carbon_monoxide_modelled Out.Ref.CO_ppm Residuals
# 1:      date   0.006248746              0.006248746   0.0007157414 0.1155245
Model.Median.Model.CO.Robust <- load_obj(Median.Model.CO.Robust)
# Deleting 
unlink(List.CO.Robust)
To.delete <- paste0(c("List.", "All.Compare.", "Table.Coeffs.", "Median.Model.", "Co_variates."), "CO.Robust")
rm(list = To.delete[To.delete %in% ls()])
# There is a little drift between 17-19 Jan, but no need to further model
#------------------------------------------C
# NO ----
#------------------------------------------C
ASE.ID <- Identify_ASE(Model = file.path(ASEDir,"Models", List_models(ASEDir = ASEDir, name.sensor = "NO_B4_P1")[1]))
# Fitting all Linear.Robust models over the whole period
List.NO.Robust <- Roll_Fit_New_Model(ASEDir = ASEDir, Interval = 5, 
                                      name.sensors = ASE.ID$name.sensor, Mod_type = "Linear.Robust",
                                      DateIN = as.Date("2020-01-17"), DateEND = as.Date("2020-01-30"))
ASE.ID <- Identify_ASE(List.NO.Robust[1], name.sensor = ASE.ID$name.sensor)
# Comparing all models of selected boxes in a vector of filepaths (ASEDir), saving and returning the comparisons
All.Compare.NO.Robust <- List_All_Compare(ASEDir, name.sensors = ASE.ID$name.sensor, All.Models = basename(List.NO.Robust))
# Confidence interval of coefficents of Models for Calibration models
Table.Coeffs.NO.Robust <- Confidence_Coeffs(All.Compare = All.Compare.NO.Robust, Mod_type = ASE.ID$Mod_type)
Median.Model.NO.Robust <- Median_Model(ASEDir, name.sensors = NULL, Table.Coeffs = Table.Coeffs.NO.Robust, 
                                            Mod_type = ASE.ID$Mod_type, All.Compare = All.Compare.NO.Robust) 
Co_variates.NO.Robust <- List_Covariates(Median.Model.NO.Robust)
#                     row.names NO_B4_P1_volt Nitric_oxide_modelled Out.Ref.NO Residuals
# 1:                 Td_deficit    0.02196064            0.02196064  0.1401485 0.4444233
# 2:               SHT31HE_volt    0.02032912            0.02032912  0.1242010 0.4029506
# 3: Relative_humidity_modelled    0.02032912            0.02032912  0.1242010 0.4029506
# 4:          Relative_humidity    0.02032912            0.02032912  0.1267995 0.4029506
Model.Median.Model.NO.Robust <- load_obj(Median.Model.NO.Robust)
# Adding Td_deficit
List.NO.Td_deficit <- Roll_Fit_New_Model(ASEDir = ASEDir, Interval = 5, name.sensors = ASE.ID$name.sensor, 
                                               Mod_type = "MultiLinear", namesCovariates = Co_variates.NO.Robust[[1]][1]$row.names,
                                               DateIN = as.Date("2020-01-17"), DateEND = as.Date("2020-01-30"))
unlink(List.NO.Robust)
To.delete <- paste0(c("List.", "All.Compare.", "Table.Coeffs.", "Median.Model.", "Co_variates."), "NO.Robust")
rm(list = To.delete[To.delete %in% ls()])
ASE.ID <- Identify_ASE(List.NO.Td_deficit[1], name.sensor = ASE.ID$name.sensor)
# Comparing all models of selected boxes in a vector of filepaths (ASEDir), saving and returning the comparisons
All.Compare.NO.Td_deficit <- List_All_Compare(ASEDir, name.sensors = ASE.ID$name.sensor, All.Models = basename(List.NO.Td_deficit))
# Confidence interval of coefficents of +Models for Calibration models
Table.Coeffs.NO.Td_deficit <- Confidence_Coeffs(All.Compare = All.Compare.NO.Td_deficit, Mod_type = ASE.ID$Mod_type)
Median.Model.NO.Td_deficit <- Median_Model(ASEDir, name.sensors = NULL, Table.Coeffs = Table.Coeffs.NO.Td_deficit, 
                                                     Mod_type = ASE.ID$Mod_type, All.Compare = All.Compare.NO.Td_deficit) 
Co_variates.NO.Td_deficit <- List_Covariates(Median.Model.NO.Td_deficit)
Co_variates.NO.Td_deficit$covariates.Matrix
#                        row.names NO_B4_P1_volt Nitric_oxide_modelled Out.Ref.NO Residuals
# 1:             Absolute_humidity   0.005319002           0.001247662 0.07824967 0.4418454
# 2: Atmospheric_pressure_modelled   0.017622933           0.002286608 0.04152033 0.3005416
# 3:                   BMP280_volt   0.017622933           0.002286608 0.04152033 0.3005416
# 4:          Atmospheric_pressure   0.017622933           0.002286608 0.04181308 0.3005416
# Adding Absolute_humidity
List.NO.Td_deficit.Absolute_humidity <- Roll_Fit_New_Model(ASEDir = ASEDir, Interval = 5, name.sensors = ASE.ID$name.sensor, 
                                         Mod_type = "MultiLinear", namesCovariates = c(".Td_deficit", Co_variates.NO.Td_deficit[[1]][1]$row.names),
                                         DateIN = as.Date("2020-01-17"), DateEND = as.Date("2020-01-30"))
unlink(List.NO.Td_deficit)
To.delete <- paste0(c("List.", "All.Compare.", "Table.Coeffs.", "Median.Model.", "Co_variates."), "NO.Td_deficit")
rm(list = To.delete[To.delete %in% ls()])

#------------------------------------------C
# NO2 ----
#------------------------------------------C
ASE.ID <- Identify_ASE(Model = file.path(ASEDir,"Models", List_models(ASEDir = ASEDir, name.sensor = "NO2_B43F_P1")[1]))
# Fitting all Linear.Robust models over the whole period
List.NO2.Robust <- Roll_Fit_New_Model(ASEDir = ASEDir, Interval = 5, 
                                      name.sensors = ASE.ID$name.sensor, Mod_type = "Linear.Robust",
                                      DateIN = as.Date("2020-01-17"), DateEND = as.Date("2020-01-30"))
ASE.ID <- Identify_ASE(List.NO2.Robust[1], name.sensor = ASE.ID$name.sensor)
# Comparing all models of selected boxes in a vector of filepaths (ASEDir), saving and returning the comparisons
All.Compare.NO2.Robust <- List_All_Compare(ASEDir, name.sensors = ASE.ID$name.sensor, All.Models = basename(List.NO2.Robust))
# Confidence interval of coefficents of Models for Calibration models
Table.Coeffs.NO2.Robust <- Confidence_Coeffs(All.Compare = All.Compare.NO2.Robust, Mod_type = ASE.ID$Mod_type)
Median.Model.NO2.Robust <- Median_Model(ASEDir, name.sensors = NULL, Table.Coeffs = Table.Coeffs.NO2.Robust, 
                                            Mod_type = ASE.ID$Mod_type, All.Compare = All.Compare.NO2.Robust) 
Co_variates.NO2.Robust <- List_Covariates(Median.Model.NO2.Robust)
#               row.names NO2_B43F_P1_volt Nitrogen_dioxide_modelled Out.Ref.NO2 Residuals
# 1:      Temperature_int        0.6315641                 0.6315641  0.01553369 0.8702646
# 2: Temperature_modelled        0.5917969                 0.5917969  0.02361999 0.8530614
# 3:         SHT31TE_volt        0.5917969                 0.5917969  0.02361999 0.8530614
# 4:          Temperature        0.5917969                 0.5917969  0.02477947 0.8530614
unlink(List.NO2.Robust)
for (i in paste0(c("List.", "All.Compare.", "Table.Coeffs.", "Median.Model.", "Co_variates."), "NO2.Robust")) {
    browser()
    if (i %in% ls()) rm(get(i))
} 
# Starting with Temperature_int
List.NO2.Temperature_int <- Roll_Fit_New_Model(ASEDir = ASEDir, Interval = 5, name.sensors = ASE.ID$name.sensor, 
                                               Mod_type = "MultiLinear", namesCovariates = Co_variates.NO2.Robust[1,1],
                                               DateIN = as.Date("2020-01-17"), DateEND = as.Date("2020-01-30"))
ASE.ID <- Identify_ASE(List.NO2.Temperature_int[1], name.sensor = ASE.ID$name.sensor)
# Comparing all models of selected boxes in a vector of filepaths (ASEDir), saving and returning the comparisons
All.Compare.NO2.Temperature_int <- List_All_Compare(ASEDir, name.sensors = ASE.ID$name.sensor, All.Models = basename(List.NO2.Temperature_int))
# Confidence interval of coefficents of Models for Calibration models
Table.Coeffs.NO2.Temperature_int <- Confidence_Coeffs(All.Compare = All.Compare.NO2.Temperature_int, Mod_type = ASE.ID$Mod_type)
Median.Model.NO2.Temperature_int <- Median_Model(ASEDir, name.sensors = NULL, Table.Coeffs = Table.Coeffs.NO2.Temperature_int, 
                                                     Mod_type = ASE.ID$Mod_type, All.Compare = All.Compare.NO2.Temperature_int) 
Co_variates.NO2.Temperature_int <- List_Covariates(Median.Model.NO2.Temperature_int)
Co_variates.NO2.Temperature_int
#                        row.names NO2_B43F_P1_volt Nitrogen_dioxide_modelled  Out.Ref.NO2 Residuals
# 1:             Absolute_humidity        0.3757319               0.090064865 0.0009774605 0.2430168
# 2:          Atmospheric_pressure        0.1262541               0.007255146 0.0223110314 0.1504390
# 3:                   BMP280_volt        0.1262541               0.007255146 0.0223178619 0.1504390
# 4: Atmospheric_pressure_modelled        0.1262541               0.007255146 0.0223178619 0.1504390unlink(List.NO2.Temperature_int)
for (i in paste0(c("List.", "All.Compare.", "Table.Coeffs.", "Median.Model.", "Co_variates."), "NO2.Temperature_int")) {
    browser()
    if (i %in% ls()) rm(get(i))
} 
#------------------------------------------C
# O3 ----
#------------------------------------------C
ASE.ID       <- Identify_ASE(Model = file.path(ASEDir,"Models", List_models(ASEDir = ASEDir, name.sensor = "OX_A431_P1")[1]))
# Fitting all Linear.Robust models over the whole period
List.O3.Robust <- Roll_Fit_New_Model(ASEDir = ASEDir, Interval = 5, 
                                     name.sensors = ASE.ID$name.sensor, Mod_type = "Linear.Robust",
                                     DateIN = as.Date("2020-01-17"), DateEND = as.Date("2020-01-30"))
ASE.ID <- Identify_ASE(List.O3.Robust[1], name.sensor = ASE.ID$name.sensor)
# Comparing all models of selected boxes in a vector of filepaths (ASEDir), saving and returning the comparisons
All.Compare.O3.Robust <- List_All_Compare(ASEDir, name.sensors = ASE.ID$name.sensor, All.Models = basename(List.O3.Robust))
# Confidence interval of coefficents of Models for Calibration models
Table.Coeffs.O3.Robust <- Confidence_Coeffs(All.Compare = All.Compare.O3.Robust, Mod_type = ASE.ID$Mod_type)
Median.Model.O3.Robust <- Median_Model(ASEDir, name.sensors = NULL, Table.Coeffs = Table.Coeffs.O3.Robust, 
                                           Mod_type = ASE.ID$Mod_type, All.Compare = All.Compare.O3.Robust) 
Co_variates.O3.Robust <- List_Covariates(Median.Model.O3.Robust)
#                    row.names OX_A431_P1_volt Ozone_modelled  Out.Ref.O3  Residuals
# 1:          NO2_B43F_P1_volt       0.8381145     -0.8381145 -0.25515890 -0.7911780
# 2: Nitrogen_dioxide_modelled      -0.2173824      0.2173824 -0.63226850  0.8024862
# 3:         Absolute_humidity      -0.3687890      0.3687890  0.12195905  0.3400212
# 4:                      date      -0.1582767      0.1582767  0.06655571  0.1054137
unlink(List.O3.Robust)
for (i in paste0(c("List.", "All.Compare.", "Table.Coeffs.", "Median.Model.", "Co_variates."), "O3.Robust")) {
    browser()
    if (i %in% ls()) rm(get(i))
} 

# Starting with "NO2_B43F_P1_volt"
List.O3.NO2_B43F_P1 <- Roll_Fit_New_Model(ASEDir = ASEDir, Interval = 5, 
                                          name.sensors = ASE.ID$name.sensor, Mod_type = "MultiLinear", namesCovariates = "NO2_B43F_P1_volt",
                                          DateIN = as.Date("2020-01-17"), DateEND = as.Date("2020-01-30"))
ASE.ID <- Identify_ASE(List.O3.NO2_B43F_P1[1], name.sensor = ASE.ID$name.sensor)
# Comparing all models of selected boxes in a vector of filepaths (ASEDir), saving and returning the comparisons
All.Compare.O3.NO2_B43F_P1 <- List_All_Compare(ASEDir, name.sensors = ASE.ID$name.sensor, All.Models = basename(List.O3.NO2_B43F_P1))
# Confidence interval of coefficents of Models for Calibration models
Table.Coeffs.O3.NO2_B43F_P1 <- Confidence_Coeffs(All.Compare = All.Compare.O3.NO2_B43F_P1, Mod_type = ASE.ID$Mod_type)
Median.Model.O3.NO2_B43F_P1 <- Median_Model(ASEDir, name.sensors = NULL, Table.Coeffs = Table.Coeffs.O3.NO2_B43F_P1, 
                                                Mod_type = ASE.ID$Mod_type, All.Compare = All.Compare.O3.NO2_B43F_P1) 
Co_variates.O3.NO2_B43F_P1 <- List_Covariates(Median.Model.O3.NO2_B43F_P1)
#                        row.names OX_A431o_P1_volt Ozone_modelled Out.Ref.O3  Residuals
# 1:             Absolute_humidity      -0.3687890     -0.1756894  0.1219590 -0.3339259
# 2:                   BMP280_volt       0.2141397      0.1011679 -0.1304622  0.2439738
# 3: Atmospheric_pressure_modelled       0.2141397      0.1011679 -0.1304622  0.2439738
# 4:          Atmospheric_pressure       0.2141397      0.1011679 -0.1324950  0.2439738
unlink(List.O3.NO2_B43F_P1)
##########################################
# Box per box 4065E3 (VITO) ####
##########################################C
# Init
ASEDir   <- file.path(Dir, "ASE_Boxes", "4065E3")
#------------------------------------------C
# CO  "CO_A4_P1"between 19 and 30 January ----
#------------------------------------------C
List.models    <- List_models(ASEDir, "CO_A4_P1")
ASE.ID         <- Identify_ASE(Model = file.path(ASEDir,"Models", List.models[1]))
Fit.CO  <- Auto.Cal(ASEDir, ASE.ID = ASE.ID, name.sensor = "CO_A4_P1", Interval = 1, DateIN = as.Date("2020-01-19"), DateEND = as.Date("2020-01-30"), date = TRUE, volt = TRUE, )
#------------------------------------------C
# CO2  "D300"between 19 and 30 January ----
#------------------------------------------C
List.models    <- List_models(ASEDir, "D300")
ASE.ID         <- Identify_ASE(Model = file.path(ASEDir,"Models", List.models[1]))
Fit.CO2  <- Auto.Cal(ASEDir, name.sensor = "D300", Interval = 1, DateIN = NULL, DateEND = as.Date("2020-01-29"), date = TRUE, volt = FALSE)
#------------------------------------------C
# NO "NO_B4_P1" between 17 and 30 January----
#------------------------------------------C
ASE.ID      <- Identify_ASE(Model = file.path(ASEDir,"Models", List_models(ASEDir = ASEDir, name.sensor = "NO_B4_P1")[1]))
List.models <- List_models(ASEDir, "NO_B4_P1")
Fit.NO      <- Auto.Cal(ASEDir, name.sensor = "NO_B4_P1", Interval = 1, DateIN = as.Date("2020-01-21"), DateEND = NULL, date = FALSE, volt = TRUE, modelled = FALSE, 
                        Mod_type = "Linear.Robust", Relationships = c("Temperature_int"), degrees = "ExpGrowth", Add.Covariates = TRUE)
Fit.NO      <- Auto.Cal(ASEDir, name.sensor = "NO_B4_P1", Interval = 1, DateIN = as.Date("2020-01-21"), DateEND = NULL, date = FALSE, volt = TRUE, modelled = FALSE, 
                        Mod_type = "Linear.Robust", Add.Covariates = TRUE)
#------------------------------------------C
# NO2 "NO2_B43F_P1" between 17 and 30 January----
#------------------------------------------C
List.models    <- List_models(ASEDir, "NO2_B43F_P1")
ASE.ID <- Identify_ASE(Model = file.path(ASEDir,"Models", List_models(ASEDir = ASEDir, name.sensor = "NO2_B43F_P1")[1]))
Fit.NO2 <- Auto.Cal(ASEDir, name.sensor = "NO2_B43F_P1", Interval = 1, DateIN = NULL, DateEND = NULL, date = FALSE, volt = TRUE, modelled = FALSE)
Fit.NO2 <- Auto.Cal(ASEDir, name.sensor = "NO2_B43F_P1", Interval = 1, DateIN = NULL, DateEND = NULL, date = FALSE, volt = TRUE, modelled = FALSE,
                    Mod_type = "Linear.Robust", Relationships = c("Temperature_int", "Td_deficit"), Add.Covariates = TRUE)
#------------------------------------------C
# O3 "OX_A431_P1" between 17 and 30 January----
#------------------------------------------C
ASE.ID      <- Identify_ASE(Model = file.path(ASEDir,"Models", List_models(ASEDir = ASEDir, name.sensor = "OX_A431_P1")[1]))
List.models <- List_models(ASEDir, "OX_A431_P1")
Fit.O3      <- Auto.Cal(ASEDir, name.sensor = "OX_A431_P1", Interval = 1, DateIN = NULL, DateEND = NULL, date = FALSE, volt = TRUE, modelled = FALSE)
#------------------------------------------C
# PM10 "5310CAT" between 17 and 30 January----
#------------------------------------------C
ASE.ID      <- Identify_ASE(Model = file.path(ASEDir,"Models", List_models(ASEDir = ASEDir, name.sensor = "5310CAT")[1]))
List.models <- List_models(ASEDir, "5310CAT")
Fit.PM10PMS <- Auto.Cal(ASEDir, name.sensor = "5310CAT", Interval = 1, DateIN = as.Date("2020-01-21"), DateEND = NULL, date = FALSE, volt = TRUE, modelled = FALSE)

####################################################################################
# Box per box 40641B ####
##########################################C
# Init
List.ASE <- c("40458D","4047D0","406414","40641B","40642E","4065D0","4065E0","4065E3","4065ED","40816F")
ASEDir       <- file.path(Dir, "ASE_Boxes", List.ASE[4])
#------------------------------------------C
#    CO ----
#------------------------------------------C
ASE.ID <- Identify_ASE(Model = file.path(ASEDir,"Models", List_models(ASEDir = ASEDir, name.sensor = "CO_A4_P1")[1]))
# Fitting all Linear.Robust models over the whole period
List.CO.Robust <- Roll_Fit_New_Model(ASEDir = ASEDir, Interval = 5, 
                                     name.sensors = ASE.ID$name.sensor, Mod_type = "Linear.Robust",
                                     DateIN = as.Date("2020-01-19"), DateEND = as.Date("2020-01-30"))
ASE.ID       <- Identify_ASE(Model = file.path(ASEDir,"Models", List.CO.Robust[1]))
# Comparing all models of selected boxes in a vector of filepaths (ASEDir), saving and returning the comparisons
All.Compare.CO.Robust <- List_All_Compare(ASEDir, name.sensors = ASE.ID$name.sensor, All.Models = basename(List.CO.Robust))
# Confidence interval of coefficents of Models for Calibration models
Table.Coeffs.CO.Robust <- Confidence_Coeffs(All.Compare = All.Compare.CO.Robust, Mod_type = ASE.ID$Mod_type)
Median.Model.CO.Robust <- Median_Model(ASEDir, name.sensors = NULL, Table.Coeffs = Table.Coeffs.CO.Robust, 
                                           Mod_type = ASE.ID$Mod_type, All.Compare = All.Compare.CO.Robust)
List_All_Compare(ASEDir = ASE.ID$ASEDir,name.sensors = ASE.ID$name.sensor, All.Models = basename(Median.Model.CO.Robust))
# Model : 40641B__CO_A4_P1__nA__Linear.Robust__20200119__20200130____Median.rdata
# ASE.name name.sensor Unit      Mod_type Cal.DateIN Cal.DateEND Variables R2raw     R2Cal      Intcal SlopeCal    RMSECal    AICCal
# 1:   40641B    CO_A4_P1   nA Linear.Robust 2020-01-19  2020-01-30              NA 0.9739747 0.003309941 1.004612 0.04942854 -52031.52
# Calibration Prediction.IN Prediction.END    R2Pred     IntPred SlopePred   RMSEPred
# 1: Linear: y= 3.310e-03+ 1.005e+00 x, R2=0.9740, RMSE=4.943e-02,AIC= -52031.5    2020-01-19     2020-01-31 0.9739747 0.003309941  1.004612 0.04942854
# AICPred                                                                 Prediction (Intercept)        x
# 1: -52031.52 Linear: y= 3.310e-03+ 1.005e+00 x, R2=0.9740, RMSE=4.943e-02,AIC= -52031.5   -32.24706 211.3525
Co_variates.CO.Robust <- List_Covariates(Median.Model.CO.Robust)
# $covariates.Matrix
#        row.names CO_A4_P1_volt Carbon_monoxide_modelled Out.Ref.CO_ppm  Residuals
# 1: NO_B4_P1_volt     0.0264123                0.0264123     0.05071218 0.05880844
Model.Median.Model.CO.Robust <- load_obj(Median.Model.CO.Robust)
# Deleting 
unlink(List.CO.Robust)
To.delete <- paste0(c("List.", "All.Compare.", "Table.Coeffs.", "Median.Model.", "Co_variates."), "CO.Robust")
rm(list = To.delete[To.delete %in% ls()])
# There is a little drift between 17-19 Jan, but no need to further model
#------------------------------------------C
# NO ----
#------------------------------------------C
ASE.ID <- Identify_ASE(Model = file.path(ASEDir,"Models", List_models(ASEDir = ASEDir, name.sensor = "NO_B4_P1")[1]))
# Fitting all Linear.Robust models over the whole period
List.NO.Robust <- Roll_Fit_New_Model(ASEDir = ASEDir, Interval = 5, 
                                     name.sensors = ASE.ID$name.sensor, Mod_type = "Linear.Robust",
                                     DateIN = as.Date("2020-01-17"), DateEND = as.Date("2020-01-30"))
ASE.ID <- Identify_ASE(List.NO.Robust[1], name.sensor = ASE.ID$name.sensor)
# Comparing all models of selected boxes in a vector of filepaths (ASEDir), saving and returning the comparisons
All.Compare.NO.Robust <- List_All_Compare(ASEDir, name.sensors = ASE.ID$name.sensor, All.Models = basename(List.NO.Robust))
# Confidence interval of coefficents of Models for Calibration models
Table.Coeffs.NO.Robust <- Confidence_Coeffs(All.Compare = All.Compare.NO.Robust, Mod_type = ASE.ID$Mod_type)
Median.Model.NO.Robust <- Median_Model(ASEDir, name.sensors = NULL, Table.Coeffs = Table.Coeffs.NO.Robust, 
                                           Mod_type = ASE.ID$Mod_type, All.Compare = All.Compare.NO.Robust) 
List_All_Compare(ASEDir = ASE.ID$ASEDir,name.sensors = ASE.ID$name.sensor, All.Models = basename(Median.Model.NO.Robust))
# Model : 40641B__NO_B4_P1__nA__Linear.Robust__20200117__20200130____Median.rdata
# ASE.name name.sensor Unit      Mod_type Cal.DateIN Cal.DateEND Variables R2raw     R2Cal   Intcal SlopeCal  RMSECal   AICCal
# 1:   40641B    NO_B4_P1   nA Linear.Robust 2020-01-17  2020-01-30              NA 0.5363939 1.404412 1.024434 14.62619 135033.7
# Calibration Prediction.IN Prediction.END    R2Pred  IntPred SlopePred RMSEPred  AICPred
# 1: Linear: y= 1.404e+00+ 1.024e+00 x, R2=0.5364, RMSE=1.463e+01,AIC= 135033.7    2020-01-18     2020-01-30 0.5363939 1.404412  1.024434 14.62619 135033.7
# Prediction (Intercept)        x
# 1: Linear: y= 1.404e+00+ 1.024e+00 x, R2=0.5364, RMSE=1.463e+01,AIC= 135033.7       16.56 0.365183
Co_variates.NO.Robust <- List_Covariates(Median.Model.NO.Robust)
# row.names NO_B4_P1_volt Nitric_oxide_modelled Out.Ref.NO Residuals
# 1:     Temperature   0.010550934           0.010550934  0.2633535 0.5257625
# 2:    SHT31TE_volt   0.010550934           0.010550934  0.2657780 0.5257625
# 3: Temperature_int   0.007911102           0.007911102  0.2601239 0.4901781
# 4:      Td_deficit   0.013998532           0.013998532  0.1732107 0.3992328
Model.Median.Model.NO.Robust <- load_obj(Median.Model.NO.Robust)
# Adding Temperature
List.NO.Temperature <- Roll_Fit_New_Model(ASEDir = ASEDir, Interval = 5, name.sensors = ASE.ID$name.sensor, 
                                         Mod_type = "exp_kT", namesCovariates = Co_variates.NO.Robust[[1]][1]$row.names, degrees = "",
                                         DateIN = as.Date("2020-01-17"), DateEND = as.Date("2020-01-30"))
unlink(List.NO.Robust)
To.delete <- paste0(c("List.", "All.Compare.", "Table.Coeffs.", "Median.Model.", "Co_variates."), "NO.Robust")
rm(list = To.delete[To.delete %in% ls()])
ASE.ID <- Identify_ASE(List.NO.Temperature[1], name.sensor = ASE.ID$name.sensor)
# Comparing all models of selected boxes in a vector of filepaths (ASEDir), saving and returning the comparisons
All.Compare.NO.Temperature <- List_All_Compare(ASEDir, name.sensors = ASE.ID$name.sensor, All.Models = basename(List.NO.Temperature))
# Confidence interval of coefficents of +Models for Calibration models
Table.Coeffs.NO.Temperature <- Confidence_Coeffs(All.Compare = All.Compare.NO.Temperature, Mod_type = ASE.ID$Mod_type)
Median.Model.NO.Temperature <- Median_Model(ASEDir, name.sensors = NULL, Table.Coeffs = Table.Coeffs.NO.Temperature, 
                                               Mod_type = ASE.ID$Mod_type, All.Compare = All.Compare.NO.Temperature) 
Co_variates.NO.Temperature <- List_Covariates(Median.Model.NO.Temperature)
Co_variates.NO.Temperature$covariates.Matrix
#                        row.names NO_B4_P1_volt Nitric_oxide_modelled Out.Ref.NO Residuals
# 1:             Absolute_humidity   0.005319002           0.001247662 0.07824967 0.4418454
# 2: Atmospheric_pressure_modelled   0.017622933           0.002286608 0.04152033 0.3005416
# 3:                   BMP280_volt   0.017622933           0.002286608 0.04152033 0.3005416
# 4:          Atmospheric_pressure   0.017622933           0.002286608 0.04181308 0.3005416
# Adding Absolute_humidity
List.NO.Temperature.Absolute_humidity <- Roll_Fit_New_Model(ASEDir = ASEDir, Interval = 5, name.sensors = ASE.ID$name.sensor, 
                                                           Mod_type = "MultiLinear", namesCovariates = c(".Td_deficit", Co_variates.NO.Temperature[[1]][1]$row.names),
                                                           DateIN = as.Date("2020-01-17"), DateEND = as.Date("2020-01-30"))
unlink(List.NO.Temperature)
To.delete <- paste0(c("List.", "All.Compare.", "Table.Coeffs.", "Median.Model.", "Co_variates."), "NO.Temperature")
rm(list = To.delete[To.delete %in% ls()])

#------------------------------------------C
# NO2 ----
#------------------------------------------C
ASE.ID <- Identify_ASE(Model = file.path(ASEDir,"Models", List_models(ASEDir = ASEDir, name.sensor = "NO2_B43F_P1")[1]))
# Fitting all Linear.Robust models over the whole period
List.NO2.Robust <- Roll_Fit_New_Model(ASEDir = ASEDir, Interval = 5, 
                                      name.sensors = ASE.ID$name.sensor, Mod_type = "Linear.Robust",
                                      DateIN = as.Date("2020-01-17"), DateEND = as.Date("2020-01-30"))
ASE.ID <- Identify_ASE(List.NO2.Robust[1], name.sensor = ASE.ID$name.sensor)
# Comparing all models of selected boxes in a vector of filepaths (ASEDir), saving and returning the comparisons
All.Compare.NO2.Robust <- List_All_Compare(ASEDir, name.sensors = ASE.ID$name.sensor, All.Models = basename(List.NO2.Robust))
# Confidence interval of coefficents of Models for Calibration models
Table.Coeffs.NO2.Robust <- Confidence_Coeffs(All.Compare = All.Compare.NO2.Robust, Mod_type = ASE.ID$Mod_type)
Median.Model.NO2.Robust <- Median_Model(ASEDir, name.sensors = NULL, Table.Coeffs = Table.Coeffs.NO2.Robust, 
                                            Mod_type = ASE.ID$Mod_type, All.Compare = All.Compare.NO2.Robust) 
Co_variates.NO2.Robust <- List_Covariates(Median.Model.NO2.Robust)
#               row.names NO2_B43F_P1_volt Nitrogen_dioxide_modelled Out.Ref.NO2 Residuals
# 1:      Temperature_int        0.6315641                 0.6315641  0.01553369 0.8702646
# 2: Temperature_modelled        0.5917969                 0.5917969  0.02361999 0.8530614
# 3:         SHT31TE_volt        0.5917969                 0.5917969  0.02361999 0.8530614
# 4:          Temperature        0.5917969                 0.5917969  0.02477947 0.8530614
unlink(List.NO2.Robust)o
for (i in paste0(c("List.", "All.Compare.", "Table.Coeffs.", "Median.Model.", "Co_variates."), "NO2.Robust")) {
    browser()
    if (i %in% ls()) rm(get(i))
} 
# Starting with Temperature_int
List.NO2.Temperature_int <- Roll_Fit_New_Model(ASEDir = ASEDir, Interval = 5, name.sensors = ASE.ID$name.sensor, 
                                               Mod_type = "MultiLinear", namesCovariates = Co_variates.NO2.Robust[1,1],
                                               DateIN = as.Date("2020-01-17"), DateEND = as.Date("2020-01-30"))
ASE.ID <- Identify_ASE(List.NO2.Temperature_int[1], name.sensor = ASE.ID$name.sensor)
# Comparing all models of selected boxes in a vector of filepaths (ASEDir), saving and returning the comparisons
All.Compare.NO2.Temperature_int <- List_All_Compare(ASEDir, name.sensors = ASE.ID$name.sensor, All.Models = basename(List.NO2.Temperature_int))
# Confidence interval of coefficents of Models for Calibration models
Table.Coeffs.NO2.Temperature_int <- Confidence_Coeffs(All.Compare = All.Compare.NO2.Temperature_int, Mod_type = ASE.ID$Mod_type)
Median.Model.NO2.Temperature_int <- Median_Model(ASEDir, name.sensors = NULL, Table.Coeffs = Table.Coeffs.NO2.Temperature_int, 
                                                     Mod_type = ASE.ID$Mod_type, All.Compare = All.Compare.NO2.Temperature_int) 
Co_variates.NO2.Temperature_int <- List_Covariates(Median.Model.NO2.Temperature_int)
Co_variates.NO2.Temperature_int
#                        row.names NO2_B43F_P1_volt Nitrogen_dioxide_modelled  Out.Ref.NO2 Residuals
# 1:             Absolute_humidity        0.3757319               0.090064865 0.0009774605 0.2430168
# 2:          Atmospheric_pressure        0.1262541               0.007255146 0.0223110314 0.1504390
# 3:                   BMP280_volt        0.1262541               0.007255146 0.0223178619 0.1504390
# 4: Atmospheric_pressure_modelled        0.1262541               0.007255146 0.0223178619 0.1504390unlink(List.NO2.Temperature_int)
for (i in paste0(c("List.", "All.Compare.", "Table.Coeffs.", "Median.Model.", "Co_variates."), "NO2.Temperature_int")) {
    browser()
    if (i %in% ls()) rm(get(i))
} 
#------------------------------------------C
# O3 ----
#------------------------------------------C
ASE.ID       <- Identify_ASE(Model = file.path(ASEDir,"Models", List_models(ASEDir = ASEDir, name.sensor = "OX_A431_P1")[1]))
# Fitting all Linear.Robust models over the whole period
List.O3.Robust <- Roll_Fit_New_Model(ASEDir = ASEDir, Interval = 5, 
                                     name.sensors = ASE.ID$name.sensor, Mod_type = "Linear.Robust",
                                     DateIN = as.Date("2020-01-17"), DateEND = as.Date("2020-01-30"))
ASE.ID <- Identify_ASE(List.O3.Robust[1], name.sensor = ASE.ID$name.sensor)
# Comparing all models of selected boxes in a vector of filepaths (ASEDir), saving and returning the comparisons
All.Compare.O3.Robust <- List_All_Compare(ASEDir, name.sensors = ASE.ID$name.sensor, All.Models = basename(List.O3.Robust))
# Confidence interval of coefficents of Models for Calibration models
Table.Coeffs.O3.Robust <- Confidence_Coeffs(All.Compare = All.Compare.O3.Robust, Mod_type = ASE.ID$Mod_type)
Median.Model.O3.Robust <- Median_Model(ASEDir, name.sensors = NULL, Table.Coeffs = Table.Coeffs.O3.Robust, 
                                           Mod_type = ASE.ID$Mod_type, All.Compare = All.Compare.O3.Robust) 
Co_variates.O3.Robust <- List_Covariates(Median.Model.O3.Robust)
#                    row.names OX_A431_P1_volt Ozone_modelled  Out.Ref.O3  Residuals
# 1:          NO2_B43F_P1_volt       0.8381145     -0.8381145 -0.25515890 -0.7911780
# 2: Nitrogen_dioxide_modelled      -0.2173824      0.2173824 -0.63226850  0.8024862
# 3:         Absolute_humidity      -0.3687890      0.3687890  0.12195905  0.3400212
# 4:                      date      -0.1582767      0.1582767  0.06655571  0.1054137
unlink(List.O3.Robust)
for (i in paste0(c("List.", "All.Compare.", "Table.Coeffs.", "Median.Model.", "Co_variates."), "O3.Robust")) {
    browser()
    if (i %in% ls()) rm(get(i))
} 

# Starting with "NO2_B43F_P1_volt"
List.O3.NO2_B43F_P1 <- Roll_Fit_New_Model(ASEDir = ASEDir, Interval = 5, 
                                          name.sensors = ASE.ID$name.sensor, Mod_type = "MultiLinear", namesCovariates = "NO2_B43F_P1_volt",
                                          DateIN = as.Date("2020-01-17"), DateEND = as.Date("2020-01-30"))
ASE.ID <- Identify_ASE(List.O3.NO2_B43F_P1[1], name.sensor = ASE.ID$name.sensor)
# Comparing all models of selected boxes in a vector of filepaths (ASEDir), saving and returning the comparisons
All.Compare.O3.NO2_B43F_P1 <- List_All_Compare(ASEDir, name.sensors = ASE.ID$name.sensor, All.Models = basename(List.O3.NO2_B43F_P1))
# Confidence interval of coefficents of Models for Calibration models
Table.Coeffs.O3.NO2_B43F_P1 <- Confidence_Coeffs(All.Compare = All.Compare.O3.NO2_B43F_P1, Mod_type = ASE.ID$Mod_type)
Median.Model.O3.NO2_B43F_P1 <- Median_Model(ASEDir, name.sensors = NULL, Table.Coeffs = Table.Coeffs.O3.NO2_B43F_P1, 
                                                Mod_type = ASE.ID$Mod_type, All.Compare = All.Compare.O3.NO2_B43F_P1) 
Co_variates.O3.NO2_B43F_P1 <- List_Covariates(Median.Model.O3.NO2_B43F_P1)
#                        row.names OX_A431_P1_volt Ozone_modelled Out.Ref.O3  Residuals
# 1:             Absolute_humidity      -0.3687890     -0.1756894  0.1219590 -0.3339259
# 2:                   BMP280_volt       0.2141397      0.1011679 -0.1304622  0.2439738
# 3: Atmospheric_pressure_modelled       0.2141397      0.1011679 -0.1304622  0.2439738
# 4:          Atmospheric_pressure       0.2141397      0.1011679 -0.1324950  0.2439738
unlink(List.O3.NO2_B43F_P1)

