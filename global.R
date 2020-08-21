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
if (!"remotes"   %in% utils::installed.packages()) install.packages("remotes")
if (!"librarian" %in% utils::installed.packages()) remotes::install_github("DesiQuintans/librarian")
librarian::shelf(futile.logger)
futile.logger::flog.info("[Global] checking presence of necessary files Functions4ASE.R.")
Functions4ASE  <- file.path(getwd(), "Functions4ASE.R")
if (!file.exists(c(Functions4ASE))) {
    stop(futile.logger::flog.error(paste0("[Global] ERROR, file ", Functions4ASE), " not found, stopping the process."))
} else futile.logger::flog.info(paste0("[Global] file ", Functions4ASE, " found and ready to be sourced."))
# Checking if "151016 Sensor_Toolbox.R" is available
DisqueSensToolBox  <- file.path(getwd(),"151016 Sensor_Toolbox.R")
futile.logger::flog.info("[Global] checking presence of necessary file 151016 Sensor_Toolbox.R.")
if (!file.exists(c(DisqueSensToolBox))) {
    stop(futile.logger::flog.error(paste0("[Global] ERROR, file ", DisqueSensToolBox), " not found, stopping the process."))
} else futile.logger::flog.info(paste0("[Global] file ", DisqueSensToolBox , " found and ready to be sourced."))
#----------------------------------------------------------------CR
# 1.c Sourcing SensorToolBox and Functions4AES.R----
#----------------------------------------------------------------CR
futile.logger::flog.info(paste0("[Global] sourcing 151016 Sensor_Toolbox.R and Funtions4ASE.R"), sep = ".")
# Loading SensorToolBox
source(DisqueSensToolBox)
remove(DisqueSensToolBox)
# Source Functions4ASE.R after SensorToolBox in order to update the last version of functions in Functions4ASE.R
source(Functions4ASE)
remove(Functions4ASE)
#----------------------------------------------------------CR
#  1.d Install packages (CRAN + Github) ----
#----------------------------------------------------------CR
futile.logger::flog.info("[Global] List of packages needed to run the scripts.")
# Packages to be loaded
# transpose dataFrame, and rbindlist (faster than rbindfill)            --> data.table
# Clean and consistent tools to split-apply-combine pattern in R        --> plyr # use the function plyr::rbind.fill to add dataframes together
# Packages of tidyVerse: tibbel, %>%                                    --> tidyVerse
# ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
# stringr: function str_detect, like grepl but for several pattern
# rsqlite query name of tables                                          --> dbplyr
# function to tidy Models 												                      --> broom, broomExtra
# To read sensor data, needed for senorweb4R, install before openair    --> stringi
# Easier management of time interval                                    --> lubridate
# When removing ouliers, using rollapply(), na.locf                     --> zoo
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
# correlation matrix                                                    --> Hmisc
# Variance Inflation Factors vif()                                      --> HH
# To read the airsenseur.db SQLite database                             --> RSQLite, sqldf, RODBC, jsonlite
# To get the time zone using the ggogle API in Down_Influx and for later
# github package threadr                                                --> RJSONIO,  XML
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
# extension()                                                           --> raster
# package needed to install Github rundel/timezone                      --> proj4
list.Packages <- c("data.table"      , "plyr"            , "tidyverse"       ,"dbplyr"           , "broomExtra"           , "stringi"         ,
                   "lubridate"       , "zoo"             , "xts"             ,
                   "futile.options"  , "lambda.r"        , "futile.logger"   , "geosphere"       ,
                   "curl"            , "RCurl"           , "httr"            , "devtools"        , "processx"        , "sp"              ,
                   "quantreg"        , "minpack.lm"      , "limSolve"        , "mgcv"            , "polynom"         , "Hmisc"           , "HH"               ,
                   "RSQLite"         , "sqldf"           , "jsonlite"        , #"RODBC"           ,
                   "RJSONIO"         , "XML"             ,
                   "BMS"             , "rlist"           , "tools"           , "stringr"         ,
                   "OSMscale"        , "berryFunctions"  ,
                   "RcppRoll"        , "foreach"         , "doParallel"      ,
                   "caTools"         , "weathermetrics"  , "colorspace"      , "backports"       , "raster"          ,
                   "proj4")
# if error on plyr then type install.packages("plyr") at the console
list.packages.github <- c("52North/sensorweb4R", "rundel/timezone")
cat("-----------------------------------------------------------------------------------\n")
cat("\n")
