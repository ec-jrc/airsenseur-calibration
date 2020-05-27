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
# - Federico Karagulian    , federico.karagulian@ec.europa.eu - European Commission - Joint Research Centre
# - Laurent Spinelle       , laurent.spinelle@ec.europa.eu - European Commission - Joint Research Centre
# - Maria Gabriella Villani, mariagabriella.villani@enea.it - ENEA
# - Marco Signorini        , marco.signorini@liberaintentio.com - Liberatintentio srl
# - Alex Kotsev            , alexander.kotsev@ec.europa.eu - European Commission - Joint Research Centre
#================================================================CR
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
# Modal message box                                                     --> shinyalert
# Modal message box confirm Sweetalert                                  --> shinyWidgets
# DashBoard                                                             --> shinydashboard
# cross-platform dialog box to select file for uploading ref data       --> rChoiceDialogs uses rJava which does not install under linux rstudio-server,
#                                                                           Anyhow this not important because it is not possible to upload local file to the shiny server,
#                                                                           only server side files. Finally the functionality.
# To locate current file and dir (jchoose.files)                        --> R.utils
# To plot time series                                                   --> openair
# corelation matrix                                                     --> corrplot
# Plot data table in shiny web interface                                --> DT
# Edit dataTable                                                        --> rhandsontable
# legend with colorbar.plot                                             --> fields
# Better arrows for Target Diagram                                      --> shape
# interactive time series plotting                                      --> dygraphs
# For mapping                                                           --> leaflet
# visualtisation of reactive HTML object used with dygraphs				--> htmltools, threadr
# Saving the time series created with dygraphs                          --> htmlwidgets, webshot
# Interactive plot in shiny, matrix plot with ploty and ggplot          --> GGally, plotly
# path of AirSensEUR spatial analysis                                   --> maptools
# path of AirSensEUR spatial analysis                                   --> raster
# path of AirSensEUR spatial analysis                                   --> rgeos
# Automatic reporting                                                   --> rmarkdown, xtable, knitr
# Rmarkdown report                                                      --> formattable, flextable, pandoc, captioner, kableExtra
# unzip                                                                 --> utils
#
# Manca rgdal?
#
list.Packages <- c("shiny"           , "shinyjs"         , "shinythemes"     , "shinyBS"         , "shinycssloaders" , "shinyWidgets"    ,
                   "shinydashboard"  ,
                   "rChoiceDialogs"  , "R.utils"         , "utils"           ,
                   "openair"         , "corrplot"        , "DT"              , 
                   "rhandsontable"   , "fields"          , "shape"           , 
                   "dygraphs"        , "leaflet"         , "htmltools"       , 
                   "htmlwidgets"     , "webshot"         , 
                   "GGally"          , "plotly"          , "maptools"        , "raster"          , "rgeos"           ,
                   "rmarkdown"       , "xtable"          , "knitr"           ,
                   "formattable"     , "flextable"       , "pander"          , "captioner"       , "kableExtra")
Load_Packages(list.Packages)
# if error on plyr then type install.packages("plyr") at the console
# Install PhatomJS Should be done only ONCE - add a test for this, see https://groups.google.com/forum/#!topic/phantomjs/3IUqGG31imI
# https://www.rdocumentation.org/packages/webshot/versions/0.5.1/topics/install_phantomjs
#webshot::install_phantomjs(version = "2.1.1", baseURL = "https://github.com/wch/webshot/releases/download/v0.3.1/")
# for linux see https://github.com/rstudio/shinyapps-package-dependencies/pull/180
# GitHub, this can crash the code if you have a PROXY, the lines can be commented
list.packages.github <- c("skgrange/threadr", "daattali/shinyalert")
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
cat("[Global] INFO, List of installed packages\n")
print(search(), quote = FALSE)
cat("\n")
