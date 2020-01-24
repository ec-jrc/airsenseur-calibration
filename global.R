#================================================================CR
# Licence: ====
# Copyright 2018 EUROPEAN UNION
# Licensed under the EUPL, Version 1.2 or subsequent versions of the EUPL (the "License");
# You may not use this work except in compliance with the License.
# You may obtain a copy of the License at: http://ec.europa.eu/idabc/eupl
# Unless required by applicable law or agreed to in writing, the software distributed
# under the License is distributed on an "AS IS" basis, WITHOUT WARRANTIES OR CONDITIONS
# OF ANY KIND, either express or implied. See the License for the specific language
# governing permissions and limitations under the License.
# Date: 05/11/2017
#
# Authors
# - Michel Gerboles        , michel.gerboles@ec.europa.eu  - European Commission - Joint Research Centre
# # - Laurent Spinelle       , laurent.spinelle@ec.europa.eu - European Commission - Joint Research Centre
# - Maria Gabriella Villani, mariagabriella.villani@enea.it - ENEA
# - Marco Signorini        , marco.signorini@liberaintentio.com - Liberatintentio srl
# - Alex Kotsev            , alexander.kotsev@ec.europa.eu - European Commission - Joint Research Centre
# - Federico Karagulian    , federico.karagulian@ec.europa.eu - European Commission - Joint Research Centre
#================================================================CR
#----------------------------------------------------------------CR
#   1.b Checking Functions4AES.R and SensorToolBox availability
#       Checking availability of Config files of AirSensEUrs
#----------------------------------------------------------------CR
# Checking if Functions4ASE.R is available
cat("-----------------------------------------------------------------------------------\n")
cat("[Global] INFO, checking presence of necessary files (151016 Sensor_Toolbox.R, Functions4ASE.R).\n")
Functions4ASE  <- file.path(getwd(), "Functions4ASE.R")
if (!file.exists(c(Functions4ASE))) {
    cat(paste0("[Global] ERROR, file ", Functions4ASE, " not found, stopping the process\n"))
    stop(cat(paste0("[Global] ERROR, file ", Functions4ASE), " not found, stopping the process\n"))
} else cat(paste0("[Global] INFO, file ", Functions4ASE, " found and ready to be sourced\n"))
cat("-----------------------------------------------------------------------------------\n")
# Checking if "151016 Sensor_Toolbox.R" is available
cat("\n")
cat("-----------------------------------------------------------------------------------\n")
DisqueSensToolBox  <- file.path(getwd(),"151016 Sensor_Toolbox.R")
cat("[Global] INFO, checking presence of necessary file 151016 Sensor_Toolbox.R.\n")
if (!file.exists(c(DisqueSensToolBox))) {
    cat(paste0("[Global] ERROR, file ", DisqueSensToolBox, " not found, stopping the process\n"))
    stop(cat(paste0("[Global] ERROR, file ", DisqueSensToolBox), " not found, stopping the process\n"))
} else cat(paste0("[Global] INFO, file ", DisqueSensToolBox , " found and ready to be sourced\n"))
cat("-----------------------------------------------------------------------------------\n")
#----------------------------------------------------------------CR
# 1.c Sourcing SensorToolBox and Functions4AES.R----
#----------------------------------------------------------------CR
cat(paste0("[Global] INFO, sourcing 151016 Sensor_Toolbox.R and Funtions4ASE.R"), sep = "\n")
# Loading SensorToolBox
source(DisqueSensToolBox)
remove(DisqueSensToolBox)
# Source Functions4ASE.R after SensorToolBox in order to update the last version of functions in Functions4ASE.R
source(Functions4ASE)
remove(Functions4ASE)
cat("-----------------------------------------------------------------------------------\n")
cat("\n")
#----------------------------------------------------------CR
#  1.d Install packages (CRAN + Github) ----
#----------------------------------------------------------CR
cat("-----------------------------------------------------------------------------------\n")
cat("[Global] INFO, Check or install packages needed to run the script\n")
# Packages to be loaded
# transpose dataFrame, and rbindlist (faster than rbindfill)            --> data.table
# Clean and consistent tools to split-apply-combine pattern in R        --> plyr # use the function plyr::rbind.fill to add dataframes together
# Packages of tidyVerse: tibbel, %>%                                    --> tidyVerse
# ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
# stringr: function str_detect, like grepl but for several pattern
# rsqlite query name of tables                                          --> dbplyr
# function to tidy Models 												--> broom
# To read sensor data, needed for senorweb4R, install before openair    --> stringi
# Easier management of time interval                                    --> lubridate
# When removing ouliers, using rollapply()                              --> zoo
# Date format for Influxdbr and other times series                      --> xts
# Packages needed for github sensorweb4R if you have a proxy            --> futile.logger, futile.options, lambda.r, geosphere
# Package needed for devtools::install_github("52North/sensorweb4R")    --> curl
# to retrieve EMEP data using function: getURL                          --> Rcurl
# To configure the proxy when using github to install sensoreb4r        --> httr
# To install libraries for reading sensor urls:sensorweb4r              --> devtools, sp, curl, 
# To solve linear robust linear regression (median)                     --> quantreg
# Function: to solve nls with Levenberg Marquardt method                --> minpack.lm  # We use function nlsLM
# To solve system of linear equation                                    --> limSolve
# For general additive models, function gam()                           --> mgcv
# crating polynomial for solving the cubic equation                     --> polynom
# To read the airsenseur.db SQLite database                             --> RSQLite, sqldf, RODBC, jsonlite
# To get the time zone using the ggogle API in Down_Influx              --> RJSONIO,  XML
# load packages for alphanumeric operations (shield config file)        --> BMS
# package for saving loading list (index for warming , outliers...)     --> rlist
# file extension file_ext                                               --> tools
# Projection of coordinates for leaflet                                 --> OSMscale
# inserting rows with NAs in a dataframe                                --> berryFunctions
# library for fastin moving average calculations                        --> RcppRoll
# parallel computing                                                    --> foreach
# parallel computing Linux or windows                                   --> doParallel
# Rolling mad and median                                                --> caTools, stats
# computation of dew points (humidity.to.dewpoint)                      --> weathermetrics
#
list.Packages <- c("data.table"      , "plyr"            , "tidyverse"       ,"dbplyr"           , "broom"           , "stringi"         ,
                   "lubridate"       , "zoo"             , "xts"             ,
                   "futile.options"  , "lambda.r"        , "futile.logger"   , "geosphere"       ,
                   "curl"            , "RCurl"           , "httr"            , "devtools"        , "processx"        , "sp"              ,
                   "quantreg"        , "minpack.lm"      , "limSolve"        , "mgcv"            , "polynom"         ,
                   "RSQLite"         , "sqldf"           , "RODBC"           , "jsonlite"        ,
                   "RJSONIO"         , "XML"             ,
                   "BMS"             , "rlist"           , "tools"           , "stringr"         ,
                   "OSMscale"        , "berryFunctions"  ,
                   "RcppRoll"        , "foreach"         , "doParallel"      ,
                   "caTools"         , "weathermetrics"  , "colorspace"      , "backports"       )
Load.Packages(list.Packages)
# if error on plyr then type install.packages("plyr") at the console
# Install PhatomJS Should be done only ONCE - add a tst for this, see https://groups.google.com/forum/#!topic/phantomjs/3IUqGG31imI
# https://www.rdocumentation.org/packages/webshot/versions/0.5.1/topics/install_phantomjs
# GitHub, this can crash the code if you have a PROXY, the lines can be commented
list.packages.github <- c("52North/sensorweb4R", "rundel/timezone")
for (i in list.packages.github) {
    # removing author name and version number
    lib.i <- tail(unlist(strsplit(i, split = "/")), n = 1)
    lib.i <- head(unlist(strsplit(lib.i, split = "@")), n = 1)
    if (!(lib.i %in% rownames(installed.packages()))) {
        cat(sprintf("[Global] INFO, installing package %s", lib.i), sep = "\n")
        devtools::install_github(i)
        cat(sprintf("Package %s installed", lib.i), sep = "\n")
    } else cat(paste0("[Global] INFO, package ", i, " already installed"), sep = "\n")
    do.call("library", as.list(lib.i))
    cat(sprintf("[Global] INFO, Package %s loaded",i), sep = "\n")
}
rm(i, lib.i)
