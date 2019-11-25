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
# - Laurent Spinelle       , laurent.spinelle@ec.europa.eu - European Commission - Joint Research Centre
# - Maria Gabriella Villani, mariagabriella.villani@enea.it - ENEA
# - Marco Signorini        , marco.signorini@liberaintentio.com - Liberatintentio srl
# - Alex Kotsev            , alexander.kotsev@ec.europa.eu - European Commission - Joint Research Centre
# - Federico Karagulian    , federico.karagulian@ec.europa.eu - European Commission - Joint Research Centre
#================================================================CR


#----------------------------------------------------------------CR
#   1.b Checking Functions4AES.R and SensorToolBox availability.
#       Checking availability of Config files of AirSensEUrs
#----------------------------------------------------------------CR

# Checking if Functions4ASE.R is available
cat("-----------------------------------------------------------------------------------\n")
cat("[Global] INFO, checking presence of necessary files (ASEconfig_xx.R and Functions4ASE.R). Then setting the Working Directory.\n")
Functions4ASE  <- file.path(getwd(), "Functions4ASE.R")
if (!file.exists(c(Functions4ASE))) {

    cat(paste0("[Global] ERROR, file ", Functions4ASE, " not found, stopping the process\n"))
    stop(cat(paste0("[Global] ERROR, file ", Functions4ASE), " not found, stopping the process\n"))

} else cat(paste0("[Global] INFO, file ", Functions4ASE , " found and ready to be sourced\n"))
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

# Checking if any config file is available
cat("\n")
cat("-----------------------------------------------------------------------------------\n")
if (all(!grepl(pattern = glob2rx("ASEconfig*.R"), list.files(path = getwd(), pattern = ".R")))) {

    cat(paste0("[Global] ERROR, no AirSensEUR config file found (ASEconfig_*.R), stopping the process\n"))
    stop(cat(paste0("[Global] ERROR, no AirSensEUR config file found (ASEconfig_*.R), stopping the process\n")))
} else cat(paste0("[Global] AirSensEUR config file found: ", list.files(path = getwd(), pattern = glob2rx("ASEconfig*.R")), "\n"))
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
# Packages to be loaded
# Grahical User Interface                                               --> shiny
# function close.window                                                 --> shinyjs
# change shiny theme                                                    --> shinythemes
# small shiny button                                                    --> shinyBS
# Add CSS Loading Animations to 'shiny' Outputs                         --> shinycssloaders
# modal message box                                                     --> shinyalert
# Modal message box confirm Sweetalert                                  --> shinyWidgets
# cross-platform dialog box to select file for uploading ref data       --> rChoiceDialogs uses rJava which does not install under linux rstudio-server.
#                                                                           Anyhow this not important because it is not possible to upload local file to the shiny server,
#                                                                           only server side files. Finally the functionality

# Clean and consistent tools to split-apply-combine pattern in R        --> plyr # use the function plyr::rbind.fill to add number of dataframes together
# Packages of tidyVerse: tibbel, %>%                                    --> tidyVerse
# ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
# stringr: function str_detect, like grepl but for several pattern
# rsqlite query name of tables                                          --> dbplyr
# function to tidy Models 												--> broom
# To locate current file and dir (jchoose.files)                        --> R.utils
# To read sensor data, needed for senorweb4R, install before openair    --> stringi
# Date format for Influxdbr                                             --> xts         # 17-07-15: influxdrb is not used anymore maybe we can get rid of xts?
# When removing ouliers, using rollapply()                              --> zoo
# Easier management of time interval                                    --> lubridate
# To plot time series                                                   --> openair
# Package needed for devtools::install_github("52North/sensorweb4R")    --> curl
# Two packages needed for github sensorweb4R if you have a proxy        --> futile.logger, futile.options, lambda.r, geosphere
# To configure the proxy when using github to install sensoreb4r        --> httr
# To install libraries for reading sensor urls:sensorweb4r              --> devtools, sp, curl
# To solve linear robust linear regression (median)                     --> quantreg
# Function: to solve nls with Levenberg Marquardt method                --> minpack.lm  # We use function nlsLM
# To solve system of linear equation                                    --> limSolve
# to retrieve EMEP data using function: getURL                          --> Rcurl
# To read the airsenseur.db SQLite database                             --> RSQLite, sqldf, RODBC
# TO assemble data frame                                                --> reshape2 , function colsplit(), function cast in SQLite2df to pass from sequential to tabulated dataframe
# To get the time zone using the ggogle API in Down_Influx              --> RJSONIO,  XML
# To work with the SQLite, airsenseur.db                                --> sqldf,
# corelation matrix                                                     --> corrplot
# For general additive models, function gam()                           --> mgcv
# downloading data from influxdb server                                 --> influxdbr #17-07-15 : not used anymore because of mistakes, now using hhtr + JSONLIte
# downloading data from influxdb server, used instaed of influxdbr      --> httr, jsonlite
# transpose dataFrame, and rbindlist (faster than rbindfill)            --> data.table
# Correlation matrix                                                    --> corrplot
# crating polynomial for solving the cubic equation                     --> polynom
# load packages for alphanumeric operations (shield config file)        --> BMS
# package for saving loading list (index for warming , outliers...)     --> rlist
# Plot data table in shiny web interface                                --> DT
# Edit dataTable                                                        --> rhandsontable
# legend with colorbar.plot                                             --> fields
# Better arrows for Target Diagram                                      --> shape
# file extension file_ext                                               --> tools
# Automatic reporting                                                   --> rmarkdown, knitr, rmarkdown, xtable
# interactive time series plotting                                      --> dygraphs
# For mapping                                                           --> leaflet
# visualtisation of reactive HTML object used with dygraphs				--> htmltools, threadr
# Saving the time series created with dygraphs                          --> htmlwidgets, webshot
# Projection of coordinates for leaflet                                 --> OSMscale
# inesrting rows with NAs in a dataframe                                --> berryFunctions
# matrix plot with ploty and ggplot                                     --> GGally
# Interactive plot in shiny                                             --> GGally
# path of AirSensEUR spatial analysis                                   --> maptools
# path of AirSensEUR spatial analysis                                   --> raster
# path of AirSensEUR spatial analysis                                   --> rgeos
# library for fastin moving average calculations                        --> RcppRoll
# parallel computing                                                    --> foreach
# parallel computing Linux or windows                                   --> doParallel
# Rolling mad and median                                                --> caTools, stats
# computation of dew points (humidity.to.dewpoint)                      --> weathermetrics
#
list.Packages <- c("shiny"           , "shinyjs"         , "shinythemes"     , "shinyBS"         , "shinycssloaders" , "shinyWidgets"    ,
                   "DT"              , "rhandsontable"   , "stringi"         , "plyr"            , "tidyverse"       , "broom"           , "dbplyr"          ,
                   "openair"         , "lubridate"       , "zoo"             , "xts"             , "futile.options"  ,
                   "lambda.r"        , "futile.logger"   , "geosphere"       ,
                   "curl"            , "RCurl"           , "httr"            , "processx"        , "sp"              ,
                   "colorspace"      , "backports"       , "devtools"        ,
                   "limSolve"        , "minpack.lm"      ,
                   "quantreg"        , "reshape"         , "RJSONIO"         , "XML"             , "jsonlite"        , "RODBC"           , "RSQLite"         , "sqldf"           ,
                   "mgcv"            , "corrplot"        , "polynom"         , "corrplot"        ,
                   "data.table"      , "BMS"             , "rlist"           ,
                   "fields"          , "shape"           , "tools"           , "R.utils"         ,
                   "stringr"         , "rmarkdown"       , "xtable"          , "knitr"           ,
                   "leaflet"         , "dygraphs"        , "htmltools"       , "htmlwidgets"     , "webshot"         , "OSMscale"        , "berryFunctions"  ,
                   "GGally"          , "plotly"          , "maptools"        , "raster"          , "rgeos"           , "RcppRoll"        , "foreach"         , "doParallel"      ,
                   "caTools"         , "weathermetrics")

Load.Packages(list.Packages)
# if error on plyr then type install.packages("plyr") at the console

# Install PhatomJS Should be done only ONCE - add a tst for this, see https://groups.google.com/forum/#!topic/phantomjs/3IUqGG31imI
# https://www.rdocumentation.org/packages/webshot/versions/0.5.1/topics/install_phantomjs
#webshot::install_phantomjs(version = "2.1.1", baseURL = "https://github.com/wch/webshot/releases/download/v0.3.1/")
# for linux see https://github.com/rstudio/shinyapps-package-dependencies/pull/180

# GitHub, this can crash the code if you have a PROXY, the lines can be commented
list.packages.github <- c("52North/sensorweb4R", "skgrange/threadr", "daattali/shinyalert")
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

cat("[Global] INFO, List of installed packages\n")
print(search(), quote = FALSE)
cat("\n")
