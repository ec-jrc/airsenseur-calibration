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
#================================================================CR
# Content ====
#================================================================CR
#  0 - Clear memory and restart R-session
#  1 - Get configuration parameters in ASEconfig_MG.R - Create file system structure check for General.Rdata availbility, create log file
#  3 - Shiny App
#================================================================CR
# 0 START ====
#================================================================CR
cat("-----------------------------------------------------------------------------------\n")
# Clear memory and restart R-session
remove(list = ls()) # [-which(ls() %in% c("DisqueFieldtest"))])
# detectiong the OS
isOS <- .Platform$OS.type
cat(paste0("[shiny] INFO, the OS platform is : ", isOS), sep = "\n")
# Checking if RStudio is used
isRStudio  <- Sys.getenv("RSTUDIO") == "1"
if (isRStudio)  cat("[shiny, isTcltk] INFO, ASE_Script is run under Rstudio\n") else cat("[shiny, isTcltk] INFO, ASE_Script is not run under Rstudio\n")
# checking if internet is available to access CRAN
havingIP <- function() {
    binary <- "ipconfig"
    if (.Platform$OS.type != "windows") {
        # test for ifconfig
        if (!system("which ifconfig > /dev/null", intern = FALSE)) {
            binary = "ifconfig"
        } else if (!system("which ip > /dev/null", intern = FALSE)) {
            binary = "ip addr"
        } else {
            stop("Could not identify binary for IP identification. Tried: ifconfig, ipconfig, ip")
        }
    }
    ipmessage <- system(binary, intern= TRUE)
    # validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]) {3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
    validIP <- "(?<=[^0-9.]|^)[1-9][0-9]{0,2}([.]([0-9]{0,3})){3}(?=[^0-9.]|$)"
    return(any(unlist(gregexpr( validIP, ipmessage, perl = TRUE) ) != -1))
}
isInternet <- havingIP() # isInternet <- havingIP() # no use to test. If there is no Internet then it is not necessary to run the script
if (isInternet) cat("[shiny] INFO, internet is available\n") else stop("[shiny] INFO, internet is not available\n")
#================================================================CR
#  1 - Get configuration parameters in ASEconfig_MG.R - Create file system structure check for General.Rdata availbility, create log file
#================================================================CR
#================================================================CR
#  1 Config ====
#   1.a Setting the Working Directory
#   1.b Checking Functions4AES.R and SensorToolBox availability.
#   1.c Sourcing Functions4AES.R and SensorToolBox, geting path of ASEconfig_xx.R
#   1.d Loading packages (global.R)
#   1.e Create the directory three for the AirSensEUR device, change working directory to point to the directory of the AirSensEUR device,
#       sending console to a file in the directory three (script log)
#   1.f Init Shiny ----
#================================================================CR
#----------------------------------------------------------------CR
# 1.a Setting the Working Directory, ----
#  The files  ASEconfig_xx.R and Functions4ASE.R shall be in this Directory
#----------------------------------------------------------------CR
cat("-----------------------------------------------------------------------------------\n")
cat("[Shiny] INFO, setting working directory\n")
# Searching in the directory from where app.R is run
# https://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
# Using automatic identification of the current directory with kimisc::thisfile(),
# if it does not work we could use choose.dir() or tcltk/JAVA, java if the OS does not have tcltk capabilities.
# Once the DisqueFieldtest is set, we stop looking for it
Script_Dir <- function(isRStudio, isInternet = TRUE) {
    # isRStudio :  Logical, TRUE if Rstudio is used otherwise FALSE
    # isInternet:  Logical, TRUE  if internet is available to access CRAN, ddefault is TRUE
    # Return the path of current script
    # trying function getScriptPath of package envDocument
    if (!require(envDocument)) {
        if (isInternet) {
            install.packages("envDocument")
        } else stop(cat("[shiny, Script_Dir()] ERROR, internet missing to install the package envDocument The script is stopped."))
    }
    require(envDocument)
    env_doc <- env_doc(output = c("return", "print", "table"), system = TRUE,
                       version = TRUE, packages = TRUE, script = FALSE, git = FALSE,
                       domino = c("auto", "on", "off"))
    print(env_doc)
    DisqueFieldtest <- as.character(env_doc[env_doc$Name == "Directory", "Value"])
    if (is.null(DisqueFieldtest)) DisqueFieldtest <- getSrcDirectory(function(x) {x})
    if (is.null(DisqueFieldtest)) DisqueFieldtest <- sys.calls()[[1]] [[2]]
    if (is.null(DisqueFieldtest) & isRStudio) {
        IsRstudioapi <- require(rstudioapi)
        if (!IsRstudioapi) { # kimisc needs to be installed ot use kimisc::thisfile(), checking if internet is available
            if (isInternet) {
                install.packages("rstudioapi");
                require(rstudioapi);
            } else stop(cat("[shiny, Script_Dir()] ERROR, internet missing to install the package rstudioapi The script is stopped."))
        }
        # Searching in the directory where the script is run
        if (dirname(rstudioapi::getActiveDocumentContext()$path) != "") {
            cat(paste0("[app.R] INFO, using rstudioapi::getActiveDocumentContext()$path, app.R is run from ", rstudioapi::getActiveDocumentContext()$path, "\n"))
            DisqueFieldtest <- dirname(rstudioapi::getActiveDocumentContext()$path)
        } else {
            cat(paste0("[app.R] ERROR, rstudioapi::getActiveDocumentContext()$path unable to detect the directory of app.R. Returning NULL.\n"))
            DisqueFieldtest <- NULL
        }
    }
    if (is.null(DisqueFieldtest)) {
        IsKmisc <- require(kimisc);
        IsKnitr <- require(knitr)
        if (!IsKmisc) { # kimisc needs to be installed ot use kimisc::thisfile(), checking if internet is available
            if (isInternet) {
                install.packages("kimisc");
                require(kimisc);
            } else stop(cat("[shiny, Script_Dir()] ERROR, internet missing to install the package kimisc. The script is stopped."))
        }
        if (!IsKnitr) {# knitr needs to be installed, checking if internet is available
            if (isInternet) {
                install.packages("knitr");
                require(knitr);
            } else stop(cat("[shiny, Script_Dir()] ERROR, internet missing to install the package knitr. The script is stopped."))
        }
        # Searching in the directory where the script is run
        if (!is.null(kimisc::thisfile())) {
            cat(paste0("[app.R] INFO, app.R is run from ", dirname(kimisc::thisfile()), "\n"))
            DisqueFieldtest <- dirname(kimisc::thisfile())
        } else {
            cat(paste0("[app.R] ERROR, kimisc::thisfile() unable to detect the directory from where is run app.R. Returning NULL.\n"))
            DisqueFieldtest <- NULL
        }
    }
    # This work only if we never change of working directory.
    # In fact we really change of working directory once an AirSensEUR is selected
    if (is.null(DisqueFieldtest)) DisqueFieldtest <- setwd(".")
    return(DisqueFieldtest)
}
# dectecting the directory of app.R
#if (!exists("DisqueFieldtest"))
DisqueFieldtest <- Script_Dir(isRStudio = isRStudio, isInternet = isInternet)
DirShiny        <- DisqueFieldtest
# if Script_Dir does not work, e. g. when deploying the app then force to "/home/shinyadmin/R"
if (is.null(DisqueFieldtest)) DisqueFieldtest <- "/home/shinyadmin/App"
cat(paste0("[shiny, isTcltk] INFO, directory from where the script is run: ", DisqueFieldtest), sep = "\n")
# Set working directory to directory where is App.R
cat(paste0("[Shiny] INFO, setting working directory to ", DisqueFieldtest), sep = "\n")
setwd(DisqueFieldtest)
cat("-----------------------------------------------------------------------------------\n")
cat("\n")
#----------------------------------------------------------------CR
#   1.d Sourcing Functions4AES.R and SensorToolBox, Loading packages (global.R and global4Shiny.R)
#----------------------------------------------------------------CR
source("global.R")
source("global4Shiny.R")
#----------------------------------------------------------------CR
#  1.e Getting the file path for ASEconfig_xx.R ----
#----------------------------------------------------------------CR
# Name of the configuration file, the extension shall be .R
#----------------------------------------------------------------CR
# 1.f Init Shiny ----
#----------------------------------------------------------------CR
#choices.ASEconfig <- list.files(path = getwd(), pattern = glob2rx("ASEconfig*.R"))
choices.ASEconfig <- list.dirs(path = file.path(getwd(),"ASE_Boxes"), recursive = FALSE)
#Dir.Logs          <- grep(pattern = glob2rx("*scriptsLog*"), x = list.dirs(DirShiny), value = TRUE)
Dir.Logs          <- grep(pattern = glob2rx("*scriptsLog*"), x = list.dirs(choices.ASEconfig), value = TRUE)
choices.Logs      <- list.files(Dir.Logs, full.names = TRUE)
Selected.Logs     <- choices.Logs[which.max(file.info(choices.Logs)$mtime)]
jscode            <- "shinyjs.closeWindow = function() { window.close(); }"
TimeZone          <- c("UTC", "Etc/GMT-1", "Europe/Amsterdam" , "Europe/Berlin", "Europe/Paris", "Europe/Rome") # Fill up with other Time Zone
Influx.TimeZone   <- c("UTC", "Etc/GMT-1", "Local time", "Europe/Amsterdam" , "Europe/Berlin", "Europe/Paris", "Europe/Rome") # Fill up with other Time Zone
TableTZ           <- as.data.frame(cbind(seq_along(TimeZone), TimeZone), stringsAsFactors = FALSE)
Influx.TableTZ    <- as.data.frame(cbind( seq_along(Influx.TimeZone), Influx.TimeZone), stringsAsFactors = FALSE)
choices.shield    <- list.files(path = file.path(getwd(), "Shield_Files"), pattern = "*.asc")
choices.Ref.unit  <- c("ppb","ppm", "ug/m3","mg/m3","counts")
Models            <- c("Linear", "Linear.Robust","MultiLinear", "exp_kT", "exp_kK", "T_power", "K_power", "RH_Hysteresis","gam", "Quadratic", "Cubic", "Michelis", "Sigmoid")
# ui =============================================================
ui <- navbarPage(title = "ASE_App v0.17", id = "ASE", theme = shinytheme("cerulean"), selected = "SelectASE",
                 # shinyjs must be initialized with a call to useShinyjs() in the app's ui.
                 useShinyjs(),
                 extendShinyjs(text = jscode, functions = c("closeWindow")),
                 # Set up shinyalert
                 useShinyalert(),
                 # Include the line below in ui.R so you can send messages
                 # https://stackoverflow.com/questions/32226331/r-shiny-pop-up-window-with-options
                 tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});')),
                           tags$style(".shiny-plot-output{width: 72vw !important; height: 90vh !important; text-align: right;}")),
                 tabPanel("SelectASE" , value = "SelectASE", icon = icon("mouse-pointer"),
                          sidebarLayout(
                              sidebarPanel(
                                  br(),
                                  selectInput( inputId = "Config_Files", label = "List of configured AirSensEURs"                      ,
                                               choices = choices.ASEconfig, selected = choices.ASEconfig[1]),
                                  textInput(   inputId = "Selected", label = "Selected AirSensEUR", value = ""),
                                  actionButton(inputId = "Select", label = "Select AirSensEUR", icon = icon("check-square")),
                                  # actionButton("Apply", label = "Apply"),
                                  hr(),
                                  #textInput(inputId = "NewFile", label = "New config file (ASEconfig*, * = SOS id)", value = "ASEconfig"),
                                  textInput(inputId = "NewFile", label = "New AirSensEUR (SOS id)"),
                                  actionButton(inputId = "Create.New", label = "Create new AirSensEUR", icon = icon("plus-circle")),
                                  hr(),
                                  actionButton(inputId = "Quit"   , label = "Quit", icon = icon("power-off"))
                                  , width = 3),
                              mainPanel(
                                  tabPanel("Selected_ASE",
                                           tabsetPanel(id = "tabMainPanelSelectASE",
                                                       # do not add a Spinner on the next TableOutput as it will not update
                                                       tabPanel(title = "Push data", icon = icon("download"), tableOutput("Pushdata.cfg")),
                                                       tabPanel("Filtering", icon = icon("filter"),
                                                                h4("Filtering Sensors data")  , tableOutput("FilteringSensor"),
                                                                h4("Filtering Reference data"), tableOutput("FilteringRef")),
                                                       tabPanel(title = "Calibration", tableOutput("Calib.cfg"), icon = icon("tachometer")),
                                                       tabPanel(title = "SetTime"    , tableOutput("SetTime.cfg"), icon = icon("time", lib = "glyphicon")),
                                                       tabPanel(title = "Sensors"    , tableOutput("Sensors.cfg"), icon = icon("tachometer")),
                                                       tabPanel(title = "Boards"     , tableOutput("Boards.cfg") , icon = icon("tachometer"))
                                           )
                                  )
                                  , width = 9)
                          )
                 ),
                 tabPanel("GetData", value = "GetData", icon = icon("database"),
                          sidebarLayout(
                              sidebarPanel(
                                  tabsetPanel(id = "ForServers",
                                              tabPanel(title = "Time-shield",
                                                       value = "tPTimeshield",
                                                       uiOutput("uiUserMins"),
                                                       uiOutput("uiUserMinsAvg"),
                                                       uiOutput("uiDelay"),
                                                       uiOutput("Dates"),
                                                       uiOutput("Variables"),
                                                       uiOutput("uiasc.File")),
                                              tabPanel(title = "Proxy",
                                                       value = "tPProxy",
                                                       uiOutput("uiPROXY"),
                                                       div(style = "display: inline-block;vertical-align:top; width: 74%;",uiOutput("uiURL")),
                                                       div(style = "display: inline-block;vertical-align:top; width: 25%;",uiOutput("uiPORT")),
                                                       div(style = "display: inline-block;vertical-align:top; width: 48%;",uiOutput("uiLOGIN")),
                                                       div(style = "display: inline-block;vertical-align:top; width: 48%;",uiOutput("uiPASSWORD"))),
                                              tabPanel(title = "Sensor Data",
                                                       value = "tPSensordown",
                                                       tabsetPanel(id = "SensorDown",
                                                                   tabPanel(title = "SOS",
                                                                            value = "tPSOS",
                                                                            uiOutput("uiDown.SOS"),
                                                                            uiOutput("uiAirsensWeb"),
                                                                            uiOutput("ASE.name"),
                                                                            uiOutput("uiSOS.tzone"),
                                                                            uiOutput("uiDown_SOS")),
                                                                   tabPanel(title = "Influx",
                                                                            value = "tPInflux",
                                                                            div(style = "display: inline-block;vertical-align:top; width: 48%;",uiOutput("uiDown.Influx")),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 48%;",uiOutput("uiDown_Influx")),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 74%;",uiOutput("uiHost")),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 1%;",HTML("<br>")),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 23%;",uiOutput("uiPort")),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 48%;",uiOutput("uiUser")),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 48%;",uiOutput("uiPass")),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 48%;",uiOutput("uiDb")),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 48%;",uiOutput("ASE.Box")),
                                                                            uiOutput("uiInflux.TZ")))),
                                              tabPanel(title = "Reference Data" , value = "tPRef",
                                                       div(style = "display: inline-block;vertical-align:top; width: 48%;",uiOutput("uiDown.Ref")),
                                                       div(style = "display: inline-block;vertical-align:top; width: 48%;",uiOutput("uiDown_Ref")),
                                                       uiOutput("uiSelected"),
                                                       br(),
                                                       tabsetPanel(id = "DownloadMode", selected = "ftp",
                                                                   # https://shiny.rstudio.com/reference/shiny/latest/fileInput.html
                                                                   # https://stackoverflow.com/questions/21813773/r-shiny-uploading-a-file-on-button-click
                                                                   tabPanel("csv",
                                                                            br(),
                                                                            p("reference data can only be in 1 csv or Rdata file with headers: date(Y-m-d H:M:S), CO_ppm or CO/co,NO,NO2,O3,NOx,SO2, PM2.5, PM10"),
                                                                            textInput(inputId = "file1", label   = "Choose CSV or Rdata file:"),
                                                                            actionButton(inputId = "browse" , label   = "Browse"),
                                                                            checkboxInput(inputId = "header", label   = "Header", value   = TRUE),
                                                                            radioButtons(inputId = 'sep',
                                                                                         label = 'Separator',
                                                                                         choices = c(Comma = ",", Semicolon = ";", Tab = '\t'),
                                                                                         selected = ",",
                                                                                         inline = TRUE),
                                                                            radioButtons(inputId = 'quote',
                                                                                         label = 'Quote',
                                                                                         choices = c(None = '', 'Double Quote' = '"','Single Quote' = "'"),
                                                                                         selected = '"',
                                                                                         inline = TRUE),
                                                                            radioButtons(inputId = 'Ref.Type',
                                                                                         label   = 'Type of references: one Reference pollutant or binned PM distribution (bins in micrometers)',
                                                                                         choices = c('Ref', 'Bin.DMPS', 'Bin.APS', 'GRIMM'),
                                                                                         selected = 'Ref',
                                                                                         inline   = TRUE)),
                                                                   tabPanel("ftp",
                                                                            uiOutput("uiurlref"),
                                                                            p("Data can be in 1 or more url linking to 1 csv files with headers:
                                                                              DateTime(Y-m-d H:M:S), CO_ppm,NO,NO2,O3,NOx,SO2, on a ftp site")),
                                                                   tabPanel("SOS",
                                                                            uiOutput("uiRefSOSname"),
                                                                            uiOutput("uiRef.SOS.name"),
                                                                            uiOutput("uiRefDate"),
                                                                            uiOutput("uiRefPollutants")),
                                                                   tabPanel("a_i_p",
                                                                            uiOutput("uiRef__a_i_p__name"),
                                                                            uiOutput("uiRef.a_i_p.name"),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 48%;", uiOutput("ui__a_i_p__User")),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 48%;", uiOutput("ui__a_i_p__Pass")),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 48%;", uiOutput("uiRef__a_i_p__Organisation")),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 48%;", uiOutput("uiRef__a_i_p__Station")),
                                                                            uiOutput("uiRef__a_i_p__Date"),
                                                                            uiOutput("uiRef__a_i_p__Pollutants"))),
                                                       tags$hr(),
                                                       uiOutput("uiReference.name"),
                                                       div(style = "display: inline-block;vertical-align:top; width: 48%;", uiOutput("uicoord.ref.Long")),
                                                       div(style = "display: inline-block;vertical-align:top; width: 48%;", uiOutput("uicoord.ref.Lat")),
                                                       uiOutput("uialt.ref"),
                                                       uiOutput("uiref.tzone")
                                              )
                                  )
                                  , width = 3),
                              mainPanel(
                                  tabsetPanel(id = "InfoPrint",
                                              #tabPanel("Log", verbatimTextOutput("console")),
                                              tabPanel(title = "GetData Panel",
                                                       h3("Time-shield"),
                                                       h4("Time"),
                                                       textOutput("UserMins"),
                                                       textOutput("UserMinsAvg"),
                                                       textOutput("Delay"),
                                                       hr(), h4("Shield Data"),
                                                       textOutput("asc.File"),
                                                       tableOutput("Shield"),
                                                       hr(), h3("Updated Proxy values"),
                                                       textOutput("PROXY"),
                                                       textOutput("URL"),
                                                       textOutput("PORT"),
                                                       textOutput("LOGIN"),
                                                       hr(), h3("Sensor data download"),
                                                       h4("Influx"),
                                                       textOutput("Down.Influx"),
                                                       textOutput("Host"),
                                                       textOutput("Port"),
                                                       textOutput("User"),
                                                       textOutput("Db"),
                                                       textOutput("Dataset"),
                                                       textOutput("Influx.TZ"),
                                                       hr(), h4("SOS"),
                                                       textOutput("Down.SOS"),
                                                       textOutput("AirsensWeb"),
                                                       textOutput("AirSensEur.name"),
                                                       textOutput("SOS.TZ"),
                                                       hr(), h3("Reference data download"),
                                                       textOutput("Down.Ref"),
                                                       textOutput("Reference.name"),
                                                       textOutput("coord.ref"),
                                                       textOutput("alt.ref"),
                                                       textOutput("ref.tzone"),
                                                       textOutput("FTPMode"),
                                                       h4("csv"),
                                                       textOutput("csvref"),
                                                       h4("ftp"),
                                                       textOutput("urlref"),
                                                       h4("SOS"),
                                                       textOutput("GDPRefSOSname"),
                                                       textOutput("GDPRef.SOS.name"),
                                                       textOutput("GDPRefPollutants"),
                                                       textOutput("GDPRefDateDownload"),
                                                       h4("a_i_p"),
                                                       textOutput("GDPRef__a_i_p__name"),
                                                       textOutput("GDPRefUser__a_i_p__"),
                                                       textOutput("GDPRef__a_i_p__Organisation"),
                                                       textOutput("GDPRef__a_i_p__Station"),
                                                       textOutput("GDPRef__a_i_p__Pollutants"),
                                                       textOutput("GDPRef__a_i_p__DateDownload")
                                              ),
                                              tabPanel(title = "Influx Sensor Data",  verbatimTextOutput('Influx.Content')),
                                              tabPanel(title = "SOS Sensor Data",     verbatimTextOutput('SOS.Content')),
                                              tabPanel(title = "Reference Data",      verbatimTextOutput('Ref.content')),
                                              tabPanel(title = "General Sensor Data", verbatimTextOutput('General.Content'))
                                  )
                                  , width = 9)
                          )
                 )
                 ,
                 tabPanel("Data Treatment"  , value = "Data Treatment", icon = icon("calculator"),
                          sidebarLayout(
                              sidebarPanel(
                                  # change nav-tabs font size
                                  # #https://stackoverflow.com/questions/19813429/r-shiny-tabset-title-modify-font-size#19814885
                                  tags$head(tags$style(type = 'text/css', ".nav-tabs {font-size: 13px} "),
                                            tags$style(HTML(".selectize-input, .selectize-dropdown {font-size: 75%;}"))
                                  ),
                                  actionButton(inputId = "Merge"    , label = "Merge Influx <-SOS <-Ref", icon = icon("compress")),
                                  actionButton(inputId = "Save"     , label = "Save"                    , icon = icon("save")) ,
                                  actionButton(inputId = "UpdateLog", label = "UpdateLog"               , icon = icon("list-ol")) ,
                                  hr(),
                                  div(style = "display: inline-block;vertical-align:top; width: 20%;",
                                      checkboxInput(inputId = "SavePlot", label = "Save Plot", value = FALSE)
                                      #, bsButton(inputId = "SaveGeneral", label = "Save Gen.",size = "extra-small")
                                  ),
                                  div(style = "display: inline-block;vertical-align:top; width: 79%;",
                                      uiOutput("uiNameSensors")),
                                  tabsetPanel(id = "Calib_data",
                                              selected = "uiFiltering",
                                              tabPanel("Filtering"       , icon = icon("filter")                 , uiOutput("uiFiltering")),
                                              tabPanel("Calib"           , icon = icon("tachometer")             , uiOutput("uiCalib") ),
                                              tabPanel("SetTime"         , icon = icon("time", lib = "glyphicon"), uiOutput("uiSetTime"))
                                  )
                                  , width = 3),
                              mainPanel(
                                  tabsetPanel(id = "tabMainPanel",
                                              tabPanel("Config", icon = icon("eye"),
                                                       tabsetPanel(id = "Configs",
                                                                   tabPanel("Downloaded"   , icon = icon("download"), withSpinner(tableOutput("Downloaded" ), type = 6)),
                                                                   tabPanel("FilteringMain", icon = icon("filter"),
                                                                            h4("Filtering Sensor data")   , tableOutput("Outliers_Sensor"),
                                                                            h4("Filtering Reference data"), tableOutput("Outliers_Ref")),
                                                                   tabPanel("CalibMain"    , tableOutput("Calib_data"), icon = icon("tachometer")),
                                                                   tabPanel("SetTimeMain"  , tableOutput("CalTime"), icon = icon("time", lib = "glyphicon") ),
                                                                   tabPanel(title = "Sensors", tableOutput("Select.Sensors.cfg"), icon = icon("tachometer")),
                                                                   tabPanel(title = "Boards" , tableOutput("Select.Boards.cfg") , icon = icon("tachometer"))
                                                       )
                                              ),
                                              tabPanel("PlotFiltering", icon = icon("filter"),
                                                       tabsetPanel(id = "tabPlots",
                                                                   tabPanel("Warming"      ,  icon = icon("toggle-off")  , withSpinner(dygraphOutput(outputId = "Warming"   , height = 750), type = 8) , width = "96%"),
                                                                   tabPanel("Temp.&Humid." ,  icon = icon("tint")        , withSpinner(dygraphOutput(outputId = "Temp.Humid", height = 750), type = 8) , width = "96%"),
                                                                   tabPanel("Invalid", icon = icon("cut"),
                                                                            tabsetPanel(id = "TabInvalid",
                                                                                        tabPanel("Table", icon = icon("bars"),
                                                                                                 br(),
                                                                                                 helpText("Time periods of Invalid data for the selected sensor and reference data. ",
                                                                                                          "Right-click on the table to delete/insert rows. ",
                                                                                                          "Double-click on a cell to edit. The button \"Save\" only saves the file of invalid
                                                                                                          date time periods while discarding of invalids is carried out by setting the CheckBoxes
                                                                                                          \"Apply validity periods\" to TRUE."),
                                                                                                 br(),
                                                                                                 actionButton(inputId = "Save.row.Valid", label = "Save",icon = icon("save")),
                                                                                                 actionButton(inputId = "At.Ref"        , label = "At Reference station",icon = icon("save")),
                                                                                                 br(),
                                                                                                 rHandsontableOutput("hot")),
                                                                                        tabPanel("Plot", icon = icon("signal"),  withSpinner(dygraphOutput(outputId = "Invalid.Sens", height = 750), type = 8) , width = "96%" ))),
                                                                   tabPanel("Neg.values",  icon = icon("minus-circle"), withSpinner(dygraphOutput(outputId = "Neg.values", height = 750), type = 8) , width = "96%" ),
                                                                   tabPanel("Outliers"  ,  icon = icon("log-out", lib = "glyphicon"),
                                                                            tabsetPanel(id = "TabOutliers",
                                                                                        tabPanel("Sens.Outliers", icon = icon("thermometer"), withSpinner(dygraphOutput(outputId = "Sens.Outliers", height = 750), type = 8) , width = "96%"),
                                                                                        tabPanel("Ref.Outliers" , icon = icon("thermometer"), withSpinner(dygraphOutput(outputId = "Ref.Outliers" , height = 750), type = 8) , width = "96%" )
                                                                            )
                                                                   ),
                                                                   tabPanel("StatFiltering",  icon = icon("eye"), tableOutput("StatFiltered"))
                                                       )
                                              ),
                                              tabPanel("Covariates", icon = icon("sort-by-alphabet", lib = "glyphicon"),
                                                       tabsetPanel(id = "TabCovariates",
                                                                   tabPanel("TimeSeries" , icon = icon("stats", lib = "glyphicon"), withSpinner(htmlOutput("ts_Cov_dygraphs"), type = 8), width = "100%" ),
                                                                   tabPanel("Matrix"     , icon = icon("th"   , lib = "glyphicon"), withSpinner(plotOutput("ValidCovarMatrix"), type = 8) )
                                                       )
                                              ),
                                              tabPanel("Calibration", icon = icon("tachometer"),
                                                       tabsetPanel(id = "TabCalibration",
                                                                   tabPanel("Map"             , icon = icon("map-marker", lib = "glyphicon")   , leafletOutput("mymapCal", height = 800)),
                                                                   tabPanel("Scatterplot"     , icon = icon("line-chart", lib = "font-awesome"), plotOutput("Calibration") ),
                                                                   tabPanel("SummaryCal"      , icon = icon("list-alt"  , lib = "glyphicon")   ,
                                                                            verbatimTextOutput("SummaryCal.Cal"),
                                                                            tableOutput("SummaryCal.Coefficients"),
                                                                            tableOutput("SummaryCal.Tidy"),
                                                                            tableOutput("SummaryCal.Glance.Title"),
                                                                            tableOutput("SummaryCal.Glance")),
                                                                   tabPanel("Calibrated"      , icon = icon("line-chart", lib = "font-awesome"), plotOutput("Calibrated") ),
                                                                   tabPanel("TimeSeries"      , icon = icon("stats"     , lib = "glyphicon")   , withSpinner(dygraphOutput("ts_Cal_dygraphs", height = 750), type = 8) , width = "96%"),
                                                                   tabPanel("Residual Matrix" , icon = icon("th"        , lib = "glyphicon")   , withSpinner(plotOutput("ResCalMatrix"), type = 8) ),
                                                                   tabPanel("MultiLinear"     , icon = icon("list-alt"  , lib = "glyphicon")   ,
                                                                            fluidRow(
                                                                                column(width = 5, offset = 0,
                                                                                       br(),
                                                                                       helpText("Coefficients and type of relationship between covariates and sensor data: ",
                                                                                                "Double-click on a cell with values to edit. The button \"Save\"  saves the file of multivariate
                                                                                                fitting. It shall be saved in order to be used for fitting calibration model.",
                                                                                                "The file of MultiLinear fitting can be edited and taken into consideration if \"Model for calibration\" in the SideBar Layout is set to MuliLinear."),
                                                                                       br(),
                                                                                       actionButton(inputId = "Save.row.Multi", label = "Save"  ,icon = icon("save")),
                                                                                       actionButton(inputId = "Del.row.Multi" , label = "Delete",icon = icon("Del")),
                                                                                       #actionButton(inputId = "New.row.Multi" , label = "Update file contents"   ,icon("list-alt"  , lib = "glyphicon")),
                                                                                       br(),
                                                                                       rHandsontableOutput("Multi")
                                                                                ),
                                                                                column(width = 5, offset = 0,
                                                                                       br(),
                                                                                       helpText("When fitting Multilinear calibration, the degrees of the covariates ",
                                                                                                "are either set to 1 if there is no Multivariate file ",
                                                                                                "in the window below or they are set to the degrees found in the",
                                                                                                " Multivariate file if it exists.",
                                                                                                "In order to change the degrees of the covariates , select a sensor,",
                                                                                                "edit the coefficient in the table at left and click the button \"Save\".",
                                                                                                "In the SideBarLayout, tab \"Calib\", set the radio button",
                                                                                                "\"Method of Prediction\" to: \"Calibration with current data\".",
                                                                                                "The fitting starts automatically.",
                                                                                                "The field \"degree\" can be set to 0 (constant mean), 1 (Linear)",
                                                                                                "2 (parabolic), 3 (cubic) or ExpGrowth (exponential growth",
                                                                                                " C.exp(kx).",
                                                                                                "Use the button \"Delete\" to return to MultiLinear with degrees set to 1."),
                                                                                       br(),
                                                                                       verbatimTextOutput("ListValid")
                                                                                )
                                                                            )
                                                                   )
                                                       )
                                              ),
                                              tabPanel("Prediction" , icon = icon("line-chart"),
                                                       tabsetPanel(id = "TabPrediction",
                                                                   tabPanel("Map"             , icon = icon("map-marker", lib = "glyphicon")   , leafletOutput("mymapExtrap",height = 800)),
                                                                   tabPanel("Scatterplot"     , icon = icon("line-chart", lib = "font-awesome"), plotOutput("Prediction") ),
                                                                   tabPanel("SummaryExtra."   , icon = icon("list-alt"  , lib = "glyphicon")   , verbatimTextOutput("SummaryExtra")),
                                                                   tabPanel("TimeSeries"      , icon = icon("stats"     , lib = "glyphicon")   , withSpinner(dygraphOutput("ts_Extra_dygraphs", height = 750), type = 8) , width = "96%"),
                                                                   tabPanel("Residual Matrix" , icon = icon("th"        , lib = "glyphicon")   , withSpinner(plotOutput("ResExtraMatrix"), type = 8) ),
                                                                   tabPanel("Uncertainty"     , icon = icon("stats"     , lib = "glyphicon")   ,
                                                                            tabsetPanel(id = "TabUncertainty",
                                                                                        tabPanel("Orth. Regression", icon = icon("stats", lib = "glyphicon"), withSpinner(tableOutput("U_Table"   ), type = 8)),
                                                                                        tabPanel("Plot Regression", icon = icon("stats", lib = "glyphicon"),  withSpinner(plotOutput("Scatter",   ), type = 8)),
                                                                                        tabPanel("Sqr. Residuals", icon = icon("stats", lib = "glyphicon"),   withSpinner(plotOutput("SqrRes",    ), type = 8)),
                                                                                        tabPanel("U", icon = icon("stats", lib = "glyphicon"),                withSpinner(plotOutput("Uncertainty"), type = 8))
                                                                            )
                                                                   ),
                                                                   tabPanel("U Target Diagram", icon = icon("screenshot"   , lib = "glyphicon"), withSpinner(htmlOutput("Target"), type = 8) ),
                                                                   tabPanel("Drift"        , icon = icon("external-link", lib = "font-awesome"),
                                                                            tabsetPanel(id = "Drift",
                                                                                        tabPanel("Absolute Drift vs time" , icon = icon("line-chart", lib = "font-awesome"), plotOutput("Drift") ),
                                                                                        tabPanel("Relative Drift vs time" , icon = icon("line-chart", lib = "font-awesome"), plotOutput("Rel.Drift") ),
                                                                                        tabPanel("Absolute Drift vs dose" , icon = icon("line-chart", lib = "font-awesome"), plotOutput("Dose.Drift") ),
                                                                                        tabPanel("Relative Drift vs dose" , icon = icon("line-chart", lib = "font-awesome"), plotOutput("Rel.Dose.Drift") )
                                                                            )
                                                                   )
                                                       )
                                              ),
                                              tabPanel("Report MarkDown", icon = icon("bars")   , h3("work in progress..."),
                                                       withSpinner(htmlOutput("renderedReport"), type = 8), downloadButton(outputId = "report", label = "Generate report")),
                                              tabPanel("DataTable"      , icon = icon("bars")   , withSpinner(DT::dataTableOutput(outputId = "DataTable"), type = 8) ),
                                              #tabPanel("Retrieved"     , icon = icon("signal") , withSpinner(plotOutput(outputId = "Retrieved"), type = 8) ),
                                              tabPanel("RawData"        , icon = icon("signal") , withSpinner(htmlOutput("ts_RawData_dygraphs"), type = 8) , width = "96%"),
                                              tabPanel("Log"            , icon = icon("list-ol"), verbatimTextOutput("console"))
                                  )
                                  , width = 9)
                          )
                 ),
                 tabPanel("Memory" , value = "MemoryUsage", icon = icon("mouse-pointer"),
                          # https://stackoverflow.com/questions/33502903/how-to-make-shiny-give-back-memory-after-a-session-ends
                          sidebarLayout(
                              sidebarPanel(
                                  br(), width = 3
                              ),
                              mainPanel(
                                  tabPanel("Memory",
                                           tableOutput('foo')
                                  )
                                  , width = 9
                              )
                          )
                 ),
                 tabPanel("About",  value = "About", icon = icon("info-circle"),
                          sidebarPanel(titlePanel("Version history")
                                       , width = 3
                          ),
                          mainPanel(
                              verbatimTextOutput("VersionInfo"),
                              width = 9
                          )
                 )
                 ,
                 tabPanel("Help", value = "Help", icon = icon("question"),
                          sidebarPanel(titlePanel("User Manual")
                                       , width = 3
                          ),
                          mainPanel(
                              #tags$iframe(style = "height:900px; width:100%; scrolling=yes", src = "ShinyASE.pdf"),
                              tags$iframe(class = "shiny-plot-output",
                                          src = "https://docs.google.com/document/d/e/2PACX-1vSH7N4piil32823BM5jJxNElQkwkm17RXczmgR6qyMXNOJyoY3BpxJoqL444o9s54VoNpxDZp74dwQB/pub?embedded=true")
                              , width = 9
                          )
                 ),
                 tabPanel("Console Logs", value = "ConsoleLogs", icon = icon("info-circle"),
                          sidebarPanel(titlePanel("Select Logs"),
                                       selectInput( inputId  = "ConsoleLogsFile",
                                                    label    = "List of console logs files",
                                                    choices  = choices.Logs,
                                                    selected = Selected.Logs
                                       ),
                                       width = 3
                          ),
                          mainPanel(
                              verbatimTextOutput('LogstextWithHTML'), # ui output as a list of HTML p() tags
                              width = 9
                          )
                 )
)
# row <- function(...) {
#     tags$div(class= "row", ...)
# }
#
# col <- function(width, ...) {
#     tags$div(class=paste0("span", width), ...)
# }
#=============================================================C
# server
server <- function(input, output, session) {
    # checking if R runs in 64 bits
    if (!grepl(pattern = "64", x = Sys.info()[["machine"]])) {
        my_message <- paste0("[shiny] Warning, R set to 32-bit, \n please switch to 64-bit to ease computation.\n")
        cat(my_message)
        shinyalert(
            title = "Warning R set to 32-bit",
            text = my_message,
            closeOnEsc = TRUE,
            closeOnClickOutside = TRUE,
            html = FALSE,
            type = "warning",
            showConfirmButton = TRUE,
            showCancelButton = FALSE,
            confirmButtonText = "OK",
            confirmButtonCol = "#AEDEF4",
            timer = 0,
            imageUrl = "",
            animation = FALSE
        )
    }
    # initial config
    shinyjs::disable("Selected")
    # The "NewFile" field is mandatory to create a new config file for new AirSensER box and thus the "Create.New" button should not be enabled if there is no new name
    # https://cran.r-project.org/web/packages/shinyjs/vignettes/shinyjs-example.html
    observe({
        if (is.null(input$NewFile) || input$NewFile == "" || input$NewFile == "ASEconfig") {
            shinyjs::disable("Create.New")
        } else {
            shinyjs::enable("Create.New")
        }
    })
    # Initial values and reactives ----
    # Reactive values used to check if there are initial data in General.file() and DF general data
    DF.NULL <<- reactiveValues(Init = FALSE)
    DF      <<- reactiveValues()
    Influx  <<- reactiveValues()
    Sos     <<- reactiveValues()
    Ref     <<- reactiveValues()
    # AirSensEUR name: The one selected in the list of configured AirSensEURs
    ASE_name           <- reactive({
        cc <- basename(input$Selected)
        #for (i in c("\\.[[:alnum:]]+$","ASEconfig")) cc <- sub(pattern = i,replacement = '', basename(as.character(cc)))
        return(cc)
    })
    # AirSensEUR name: The one selected in the combo box of existing AirSensEURs
    ASE_name.List      <- reactive({
        cc <- basename(input$Config_Files)
        #for (i in c("\\.[[:alnum:]]+$","ASEconfig")) cc <- sub(pattern = i,replacement = '', basename(as.character(cc)))
        return(cc)
    })
    # Reactive DisqueFieldtestDir() ----
    # DisqueFieldtestDir     : The one of the Selected AirSensEUR
    # DisqueFieldtestDir      <- reactive({return(file.path(DirShiny, "ASE_Boxes", ASE_name())) })
    # DisqueFieldtestDir.List: The one given by the Selected config file in the list of Configured AirSensEUR
    DisqueFieldtestDir.List <- reactive({return(file.path(DirShiny, "ASE_Boxes", ASE_name.List())) })
    General.file            <- reactive({file.path(input$Selected, "General_data" , "General.csv")})
    InfluxData.file         <- reactive({file.path(input$Selected, "General_data" , "InfluxData.csv")})
    SOSData.file            <- reactive({file.path(input$Selected, "General_data" , "SOSData.csv")})
    RefData.file            <- reactive({file.path(input$Selected, "General_data" , "RefData.csv")})
    ind.warm.file           <- reactive({file.path(input$Selected, "General_data" , "ind_warm.RDS")})
    ind.TRh.file            <- reactive({file.path(input$Selected, "General_data" , "ind_TRh.RDS"  )})
    ind.Invalid.file        <- reactive({file.path(input$Selected, "General_data" , "ind_Invalid.RDS")})
    ind.sens.out.file       <- reactive({file.path(input$Selected, "General_data" , "ind_sens_out.RDS")})
    ind.ref.out.file        <- reactive({file.path(input$Selected, "General_data" , "ind_ref_out.RDS")})
    # Reactive cfg_file() ----
    # cfg_file     : The cfg file  of the Selected AirSensEUR when clicking button "Selected AirSensEUR"
    Configuration.TRUE <- reactive({
        # make it reactive to function CONFIG that will change cfg_file, SETTIME_file ...
        if (exists("Config") && !is.null(Config$All)) Config$All
        ifelse(length(list.files(pattern = ".cfg", file.path(input$Config_Files, "Configuration"))) > 0, TRUE, FALSE)})
    cfg_file           <- reactive({if (Configuration.TRUE()) {
        file.path(input$Selected     ,"Configuration",paste0(ASE_name()     ,".cfg"))
    } else file.path(input$Selected     ,"General_data",paste0(ASE_name()     ,".cfg"))})
    SETTIME_file       <- reactive({if (Configuration.TRUE()) {
        file.path(input$Selected     ,"Configuration",paste0(ASE_name()     ,"_SETTIME.cfg"))
    } else file.path(input$Selected     ,"General_data",paste0(ASE_name()     ,"_SETTIME.cfg"))})
    Servers_file       <- reactive({if (Configuration.TRUE()) {
        file.path(input$Selected     ,"Configuration",paste0(ASE_name()     ,"_Servers.cfg"))
    } else file.path(input$Selected     ,"General_data",paste0(ASE_name()     ,"_Servers.cfg"))})
    # cfg_file.List: The cfg file  Selected config file in the combo box of Configured/existing AirSensEUR
    cfg_file.List      <- reactive({if (Configuration.TRUE()) {
        file.path(DisqueFieldtestDir.List(),"Configuration",paste0(ASE_name.List(),".cfg"))
    }  else file.path(DisqueFieldtestDir.List(),"General_data",paste0(ASE_name.List(),".cfg"))})
    SETTIME_file.List  <- reactive({if (Configuration.TRUE()) {
        file.path(DisqueFieldtestDir.List(),"Configuration",paste0(ASE_name.List(),"_SETTIME.cfg"))
    } else file.path(DisqueFieldtestDir.List(),"General_data",paste0(ASE_name.List(),"_SETTIME.cfg"))})
    Servers_file.List  <- reactive({if (Configuration.TRUE()) {
        file.path(DisqueFieldtestDir.List(),"Configuration",paste0(ASE_name.List(),"_Servers.cfg"))
    } else file.path(DisqueFieldtestDir.List(),"General_data",paste0(ASE_name.List(),"_Servers.cfg"))})
    # Reactive i.sensors Once AirSensEUR is Selected
    i.sensors          <- reactive({
        # Returning the indexes of valid sensors in ASE_name.cfg taking into account NAs
        # depends on input$asc.File, to make i.sensors() reactive on change of shield config file
        # I do not think it is necesary, the number of sensors should remain the same, but maybe a new sensor for a new compound could be installed
        input$asc.File
        # avoid na and names of sensor in the checmical shield
        #return(which(!is.na(Config$all[["sens2ref"]]$name.sensor) & Config$all[["sens2ref"]]$name.sensor %in% Config$all$sens2ref.shield$name.sensor))
        return(which(!is.na(Set$Time$name.gas) & Set$Time$name.gas %in% Config$all$sens2ref$name.gas))
    })
    list.gas.sensor       <- reactive({
        Config$all[["sens2ref"]]$gas.sensor[
            !is.na(Config$all[["sens2ref"]]$gas.sensor) &
                Config$all[["sens2ref"]]$gas.sensor  != "" &
                Config$all[["sens2ref"]]$name.sensor %in% Config$all$sens2ref.shield$name.sensor]})
    list.name.sensor      <- reactive({
        Config$all[["sens2ref"]]$name.sensor[
            !is.na(Config$all[["sens2ref"]]$name.sensor) &
                Config$all[["sens2ref"]]$name.sensor != "" &
                Config$all[["sens2ref"]]$name.sensor %in% Config$all$sens2ref.shield$name.sensor]})
    list.name.gas      <- reactive({
        Config$all[["sens2ref"]]$name.gas[
            !is.na(Config$all[["sens2ref"]]$name.sensor) &
                Config$all[["sens2ref"]]$name.sensor != "" &
                Config$all[["sens2ref"]]$name.sensor %in% Config$all$sens2ref.shield$name.sensor]})
    list.gas.reference2use <- reactive({
        # return a vector with the names of gas reference using the file ASE.cfg
        Config$all[["sens2ref"]]$gas.reference2use[!is.na(Config$all[["sens2ref"]]$gas.reference2use) &
                                                       Config$all[["sens2ref"]]$gas.reference2use != "" &
                                                       Config$all[["sens2ref"]]$gas.sensor %in% list.gas.sensor()]})
    list.gas.reference <- reactive({
        # return a vector with the names of gas reference using the file ASE.cfg
        Config$all[["sens2ref"]]$gas.reference[!is.na(Config$all[["sens2ref"]]$gas.reference) &
                                                   Config$all[["sens2ref"]]$gas.reference != "" &
                                                   Config$all[["sens2ref"]]$gas.sensor %in% list.gas.sensor()]})
    # Reactive DownloadSensor() ----
    # Getting info on last downloaded data, make i reactive to change in INFLUX(), SOSDATA(), REFDATA(), DF$General
    DownloadSensor          <- eventReactive(
        {
            input$Dataset
            input$UserMins
            Influx$DATA
            Sos$DATA
            Ref$DATA
            DF$General
            input$Selected
        },{
            Check_Download(Influx.name = input$Dataset,
                           WDinput     = file.path(input$Selected, "General_data"),
                           UserMins    = if (!is.null(input$UserMins)) as.numeric(input$UserMins) else Config$all$Server$UserMins,
                           General.df  = if (!is.null(DF$General))  DF$General else NA,
                           RefData     = if (!is.null(Ref$DATA))    Ref$DATA else NA,
                           InfluxData  = if (!is.null(Influx$DATA)) Influx$DATA else NA,
                           SOSData     = if (!is.null(Sos$DATA))    Sos$DATA else NA)
        }
    )
    # Navbar menu "About"
    output$VersionInfo <- renderPrint(AboutVersions(DisqueFieldtest = DirShiny,
                                                    FirstLineText   = "# Version History ====",
                                                    LastLineText    = "# Content ===="), width = getOption("width"))
    # Navbar menu "Console Logs"
    output$LogstextWithHTML <- renderPrint({
        ## Create connection
        con <- file(description = input$ConsoleLogsFile,
                    open = "r")
        ## Reading App.R
        Com <- readLines(con, n = -1)
        # Close connection
        close(con)
        return(Com[(length(Com) - 1000):length(Com)])
    })
    # NavBar"SelectASE", Button Create.New,  ----
    # After a click on button "Create New AirSensEUR"
    observeEvent(input$Create.New, {
        # https://deanattali.com/blog/advanced-shiny-tips/#plot-spinner
        # Stop the App when the browser tab is closed
        session$onSessionEnded(stopApp)
        # Create a Progress object
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        progress$set(message = "Creating New Config File of AirSensEUR Box", value = 0.5)
        #if (!file.exists(file.path(DirShiny, paste0(input$NewFile,".R"))) ) {
        if (!dir.exists(file.path(DirShiny, "ASE_Boxes", input$NewFile))) {
            #----------------------------------------------------------CR
            #  1.c Create file system structure. check for General.Rdata availability, sourcing ASEConfig_xx.R ####
            #----------------------------------------------------------CR
            # Check directories existence or create , create log file
            # Setting the current directory to the root of the file system with the name of The AirSensEUR
            old_ASE_name       <- basename(input$Config_Files)
            if (input$NewFile != "") {
                DisqueFieldtestDir <- file.path(DisqueFieldtest, "ASE_Boxes", input$NewFile)
                # Creating the the working directory of the AirSensEUR box
                cat("-----------------------------------------------------------------------------------\n")
                cat(paste0("[shiny, Create.New] INFO, Creating the directory: ",DisqueFieldtestDir, ". Setting it as working directory."), sep = "\n")
                if (!dir.exists(DisqueFieldtestDir)) dir.create(DisqueFieldtestDir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
                # Creating File structure
                cat("-----------------------------------------------------------------------------------\n")
                cat(paste0("[shiny, Create.New] INFO creating the file system for data treatment at ", DisqueFieldtestDir), sep = "\n")
                List.Dirs <- c("Calibration","Configuration","Drift","Estimated_coef","General_data","Models","Modelled_gas","Outliers","scriptsLog",
                               "SensorData","Retrieved_plots","Statistics","Verification_plots", "MarkDown")
                for (i in List.Dirs) {
                    if (!dir.exists(file.path(DisqueFieldtestDir, i))) {
                        dir.create(file.path(DisqueFieldtestDir, i), showWarning = TRUE, recursive = TRUE, mode = "0777")
                        cat(paste0("[shiny, Create.New] INFO Dir. created: ", file.path(DisqueFieldtestDir,i)), sep = "\n\r")
                    } else cat(paste0("[shiny, Create.New] INFO Dir. already exists: ", file.path(DisqueFieldtestDir,i)), sep = "\n")
                }
                # Populating the configuration intormation with cfg and effect files
                New_General_dir <- file.path(DisqueFieldtestDir, "Configuration")
                cfg_Files       <- list.files(path = file.path(input$Config_Files), pattern = ".cfg", recursive = TRUE)
                cfg_Files       <- cfg_Files[-grep(pattern= paste(c("Boards.cfg", "Sensors.cfg"), collapse = "|"), cfg_Files)]
                for (i in cfg_Files) {
                    cat(paste0("[shiny, Create.New] INFO, copying ", basename(i), " at ", New_General_dir), sep = "\n")
                    file.copy(from = file.path(input$Config_Files,i), to = file.path(DisqueFieldtestDir, "Configuration",gsub(pattern = old_ASE_name, replacement = input$NewFile, basename(i))),
                              overwrite = TRUE, copy.mode = TRUE, copy.date = FALSE)}
                # Updating list of ASE boxes and select the newly created one
                Newchoices      <- list.dirs(path = file.path(DirShiny, "ASE_Boxes"), recursive = FALSE)
                updateSelectInput( session = session, inputId = "Config_Files", choices = Newchoices,
                                   selected = DisqueFieldtestDir)
            } else shinyalert(
                title = "ERROR ASE box name",
                text = "ERROR the same AirSensEUR box name cannot be empty",
                closeOnEsc = TRUE,
                closeOnClickOutside = TRUE,
                html = FALSE,
                type = "error",
                showConfirmButton = TRUE,
                showCancelButton = FALSE,
                confirmButtonText = "OK",
                confirmButtonCol = "#AEDEF4",
                timer = 4000,
                imageUrl = "",
                animation = FALSE)
        } else shinyalert(
            title = "ERROR ASE box already exist",
            text = "ERROR it is not possible to use twice the same AirSensEUR box name. Change name or delete directory in ../Shiny/ASE_Boxes if needed",
            closeOnEsc = TRUE,
            closeOnClickOutside = TRUE,
            html = FALSE,
            type = "error",
            showConfirmButton = TRUE,
            showCancelButton = FALSE,
            confirmButtonText = "OK",
            confirmButtonCol = "#AEDEF4",
            timer = 4000,
            imageUrl = "",
            animation = FALSE)
        progress$set(message = "[shiny, Create.New] INFO, Creating New Config File of AirSensEUR Box", value = 1)
    })
    # NavBar"SelectASE", Button "Select AirSensEUR" ----
    # NewFile used to be disabled. On 200114 it was enabled again, 
    observeEvent(input$Select, {
        # update disabled inoutText "Selected"
        updateTextInput(session, inputId = "Selected", value = input$Config_Files)
        #shinyjs::disable("NewFile")
    })
    observeEvent(input$Selected, {
        # not run without clicking once on button "Select ASE"
        if (input$Select > 0) {
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            progress$set(message = "[Shiny] INFO, Selecting AirSensEUR Box", value = 0.0)
            # disable the possibility to uncheck headers of csv file for reference data, local files
            shinyjs::disable("header")
            #----------------------------------------------------------CR
            #  0) Reactive values DF and others ----
            #----------------------------------------------------------CR
            # initial Config
            progress$set(message = "[Shiny] INFO, Loading config file", value = 0.1)
            Config <<- reactiveValues(all = CONFIG(DisqueFieldtestDir = input$Selected, DisqueFieldtest = DirShiny, Dir.Config = ifelse(Configuration.TRUE(),"Configuration", "General_data")))
            # Returning a list with 4 elements see below
            # Config$all[["Server"]]   : server parameters
            # Config$all[["sens2ref"]] : cfg parameters
            # Config$all[["CovPlot"]]  : covariates for plotting
            # Config$all[["CovMod"]]   : covariates for calibrating
            # initial data in General.file()
            progress$set(message = "[Shiny] INFO, Loading Sensor, reference and General data", value = 0.2)
            if (file.exists(InfluxData.file())) {
                if (extension(InfluxData.file()) == ".csv") {
                    Influx$DATA <- fread(file = InfluxData.file(), na.strings = c("","NA", "<NA>"))
                    if (!"" %in% Config$all$Server$Influx.TZ) {
                        data.table::set(Influx$DATA, j = "date", value =  ymd_hms(Influx$DATA[["date"]], tz = Config$all$Server$Influx.TZ))
                    } else data.table::set(Influx$DATA, j = "date", value =  ymd_hms(Influx$DATA[["date"]], tz = "UTC"))
                } else if (extension(InfluxData.file()) == ".Rdata") {
                    Influx$DATA <- load_obj(InfluxData.file())
                    if (!"data.table" %in% class(Influx$DATA)) Influx$DATA <- data.table(Influx$DATA, key = "date")
                }
                if ("V1" %in% names(Influx$DATA)) Influx$DATA[, V1 := NULL]
            }
            if (file.exists(SOSData.file())) {
                if (extension(SOSData.file()) == ".csv") {
                    Sos$DATA <- fread(file = SOSData.file(), na.strings = c("","NA", "<NA>"))
                    if (!"" %in% Config$all$Server$SOS.TZ) {
                        data.table::set(Sos$DATA, j = "date", value =  ymd_hms(Sos$DATA[["date"]], tz = Config$all$Server$SOS.TZ))
                    } else data.table::set(Sos$DATA, j = "date", value =  ymd_hms(Sos$DATA[["date"]], tz = "UTC"))
                } else if (extension(SOSData.file()) == ".Rdata") {
                    Sos$DATA <- load_obj(SOSData.file())
                    if (!"data.table" %in% class(Sos$DATA)) Sos$DATA <- data.table(Sos$DATA, key = "date")
                }
                if ("V1" %in% names(Sos$DATA)) Sos$DATA[, V1 := NULL]
            }
            if (file.exists(RefData.file())) {
                if (extension(RefData.file()) == ".csv") {
                    Ref$DATA <- fread(file = RefData.file(), na.strings = c("","NA", "<NA>"))
                    if (!"" %in% Config$all$Server$ref.tzone) {
                        data.table::set(Ref$DATA, j = "date", value =  ymd_hms(Ref$DATA[["date"]], tz = Config$all$Server$ref.tzone))
                    } else data.table::set(Ref$DATA, j = "date", value =  ymd_hms(Ref$DATA[["date"]], tz = "UTC"))
                } else if (extension(SOSData.file()) == ".Rdata") {
                    Ref$DATA <- load_obj(RefData.file())
                    if (!"data.table" %in% class(Ref$DATA)) Ref$DATA <- data.table(Ref$DATA, key = "date")
                }
                if ("V1" %in% names(Ref$DATA)) Ref$DATA[, V1 := NULL]
                # Message in case coordinates are not included
                if (!all(c("Ref.Long",  "Ref.Lat") %in% names(Ref$DATA))) shinyalert(
                    title = "ERROR Coordinates of reference station missing",
                    text = "You should delete RefData.csv, General.csv and all .RDS files\n
                            in directory General_data and reload reference data",
                    closeOnEsc = TRUE,
                    closeOnClickOutside = TRUE,
                    html = FALSE,
                    type = "error",
                    showConfirmButton = TRUE,
                    showCancelButton = FALSE,
                    confirmButtonText = "OK",
                    confirmButtonCol = "#AEDEF4",
                    timer = 3000,
                    imageUrl = "",
                    animation = FALSE
                )
            }
            if (file.exists(General.file())) {
                if (extension(General.file()) == ".csv") {
                    DF$General <- data.table::fread(General.file()) #, na.strings = getOption("","NA")
                    # Convert date to POSIXct
                    if (!all("" %in% c(Config$all$Server$Influx.TZ, Config$all$Server$SOS.TZ))) {
                        if (!"" %in% c(Config$all$Server$Influx.TZ)) {
                            data.table::set(DF$General, j = "date"         , value =  ymd_hms(DF$General[["date"]]         , tz = Config$all$Server$Influx.TZ))
                            data.table::set(DF$General, j = "date_PreDelay", value =  ymd_hms(DF$General[["date_PreDelay"]], tz = Config$all$Server$Influx.TZ))
                        } else {
                            data.table::set(DF$General, j = "date"         , value =  ymd_hms(DF$General[["date"]]         , tz = Config$all$Server$SOS.TZ))
                            data.table::set(DF$General, j = "date_PreDelay", value =  ymd_hms(DF$General[["date_PreDelay"]], tz = Config$all$Server$SOS.TZ))
                        }
                    } else {
                        data.table::set(DF$General, j = "date"         , value =  ymd_hms(DF$General[["date"]]         , tz = "UTC"))
                        data.table::set(DF$General, j = "date_PreDelay", value =  ymd_hms(DF$General[["date_PreDelay"]], tz = "UTC"))
                    }
                } else if (extension(General.file()) == ".Rdata") {
                    DF$General <- load_obj(General.file())
                    if (!"data.table" %in% class(DF$General)) DF$General <- data.table(DF$General, key = "date")
                }
                if ("V1" %in% names(DF$General)) DF$General[, V1 := NULL]
                # Checking that dew point deficit and absolute humidity are included for old download of AirSensEUR boxes
                # adding absolute humidity is relative humidity and temperature exist
                if (!all(c("Absolute_humidity", "Td_deficit") %in% names(DF$General))) {
                    if (all(c("Temperature", "Relative_humidity") %in% names(DF$General))) {
                        DF$General$Absolute_humidity <- NA_real_
                        DF$General$Td_deficit        <- NA_real_
                        both.Temp.Hum <- complete.cases(DF$General[, c("Temperature", "Relative_humidity")])
                        DF$General[both.Temp.Hum, "Absolute_humidity"] <- threadr::absolute_humidity(DF$General[["Temperature"]][both.Temp.Hum], DF$General[["Relative_humidity"]][both.Temp.Hum])
                        Td <- weathermetrics::humidity.to.dewpoint(rh = DF$General[["Relative_humidity"]][both.Temp.Hum], t = DF$General[["Temperature"]][both.Temp.Hum], temperature.metric = "celsius")
                        DF$General[both.Temp.Hum, Td_deficit := DF$General[both.Temp.Hum, "Temperature"] - Td]
                    }
                }
                if (!all(c("Ref.Absolute_humidity", "Ref.Td_deficit") %in% names(DF$General))) {
                    if (all(c("Ref.Temp", "Ref.RH") %in% names(DF$General))) {
                        DF$General$Ref.Absolute_humidity <- NA_real_
                        DF$General$Ref.Td_deficit        <- NA_real_
                        Ref.both.Temp.Hum <- complete.cases(DF$General[, c("Ref.Temp", "Ref.RH")])
                        DF$General[Ref.both.Temp.Hum, "Ref.Absolute_humidity"] <- threadr::absolute_humidity(DF$General[["Ref.Temp"]][Ref.both.Temp.Hum], DF$General[["Ref.RH"]])
                        Td <- weathermetrics::humidity.to.dewpoint(rh = DF$General[Ref.both.Temp.Hum, "Ref.RH"], t = DF$General[["Ref.Temp"]][Ref.both.Temp.Hum], temperature.metric = "celsius")
                        DF$General[Ref.both.Temp.Hum, Ref.Td_deficit := DF$General[["Ref.Temp"]][Ref.both.Temp.Hum] - Td]
                    }
                }
                #}
                progress$set(message = "[shiny] INFO, Loading General data", value = 0.3)
                # se c'e' General.Rdata ma alcuni sensori o referenze di Ref$DATA e Influx$DATA non sono in DF$General le si combina
                if (!all(c(names(Ref$DATA)[grep(pattern = paste(c("Bin.", "boardTimeStamp", "gpsTimestamp"), collapse = "|"),
                                                x = names(Ref$DATA), invert = T)], names(Influx$DATA)
                ) %in% names(DF$General)) ) {
                    DF$General   <<- NULL
                    DF.NULL$Init <<- TRUE
                }
            } else {
                DF$General   <<- NULL
                DF.NULL$Init <<- TRUE
            }
            # Initial DownloadSensor
            progress$set(message = "[Shiny] INFO, Initial DownloadSensor", value = 0.4)
            Download <<- reactiveValues(Sensor = DownloadSensor())
            # Initial SetTime
            progress$set(message = "[Shiny] INFO, Loading SetTime file", value = 0.35)
            Set <<- reactiveValues(Time = SETTIME(DisqueFieldtestDir = input$Selected, 
                                                  DisqueFieldtest    = DirShiny, 
                                                  General.t.Valid    = DF$General,
                                                  Influx.TZ          = Config$all[["Server"]]$Influx.TZ,
                                                  SOS.TZ             = Config$all[["Server"]]$SOS.TZ,
                                                  Ref.TZ             = Config$all[["Server"]]$ref.tzone,
                                                  DownloadSensor     = Download$Sensor,
                                                  Config             = Config$all,
                                                  sens2ref.shield    = Config$all$sens2ref.shield,
                                                  Dir.Config         = "Configuration"))
            # Indexes of data discarded during warming time of sensors
            progress$set(message = "[Shiny] INFO, Loading indexes of data discarded during warming time of sensors", value = 0.5)
            Warm <<- reactiveValues(Forced = FALSE)
            if (file.exists(ind.warm.file())) init.ind.warm <- list.load(ind.warm.file()) else {
                init.ind.warm <- NULL
                Warm$Forced   <<- TRUE
            }
            ind.warm <<- reactiveValues(out = init.ind.warm)
            # Indexes of data discarded outside temperature and humidity tolerance
            progress$set(message = "[Shiny] INFO, Loading indexes of data discarded outside temperature and humidity tolerance", value = 0.5)
            TRh <<- reactiveValues(Forced = FALSE)
            if (file.exists(ind.TRh.file())) init.ind.TRh <- list.load(ind.TRh.file()) else {
                init.ind.TRh <- NULL
                TRh$Forced   <<- TRUE
            }
            ind.TRh <<- reactiveValues(out = init.ind.TRh)
            # Flagging the sensor data for Invalid sensor data
            progress$set(message = "[Shiny] INFO, Loading indexes of Invalid sensor data", value = 0.6)
            Inv <<- reactiveValues(Forced = FALSE)
            if (file.exists(ind.Invalid.file())) init.ind.Invalid <- list.load(ind.Invalid.file()) else {
                init.ind.Invalid <- NULL
                Inv$Forced <<- TRUE
            }
            ind.Invalid <<- reactiveValues(out = init.ind.Invalid)
            # Indexes of outliers for sensor data
            progress$set(message = "[Shiny] INFO, Loading indexes of outliers for sensor data", value = 0.7)
            Outliers.Sens <<- reactiveValues(Forced = FALSE)
            if (file.exists(ind.sens.out.file())) Init.ind.sens <- list.load(ind.sens.out.file()) else {
                Init.ind.sens <- NULL
                Outliers.Sens$Forced <<- TRUE
            }
            ind.sens <<- reactiveValues(out = Init.ind.sens)
            # Indexes of outliers for sensor data
            progress$set(message = "[Shiny] INFO, Loading indexes of outliers for reference data", value = 0.8)
            Outliers.Ref <<- reactiveValues(Forced = FALSE)
            Neg          <<- reactiveValues(Forced = FALSE)
            if (file.exists(ind.ref.out.file()) && file.size(ind.ref.out.file()) > 0) init.ind.ref <- list.load(ind.ref.out.file()) else {
                init.ind.ref <- NULL
                Outliers.Ref$Forced <<- TRUE
            }
            ind.ref <<- reactiveValues(out = init.ind.ref)
            # Initialising for conversion and calibration
            Conv <<- reactiveValues(Forced = FALSE)
            Cal  <<- reactiveValues(Forced = FALSE)
            # Models for Calibration once AirSensEUR is Selected
            Covariates        <- reactive({c(paste0("Out.",list.gas.reference2use()), INFLUX()[[2]], "Absolute_humidity", "Td_deficit","date",
                                             paste0(list.gas.sensor(),"_modelled"),
                                             paste0(list.name.sensor(),"_volt"),
                                             paste0("Out.",list.gas.sensor()), "longitude", "latitude")})
            Covariates.Model  <- reactive({c(paste0("Out.",list.gas.reference2use()), INFLUX()[[2]], "Absolute_humidity", "Td_deficit","date", paste0(list.gas.sensor(),"_modelled"), paste0(list.name.sensor(),"_volt"), paste0("Out.",list.gas.sensor())) })
            #----------------------------------------------------------CR
            #  1.c Create file system structure. check for General.Rdata availability, sourcing ASEConfig_xx.R ####
            #----------------------------------------------------------CR
            # Store the current directory
            initial.dir <- DirShiny
            # Check directory existence or create
            cat("-----------------------------------------------------------------------------------\n")
            cat(paste0("[shiny] INFO creating the file system for data treatment at ", input$Selected)," if it does not exist\n")
            progress$set(message = paste0("[shiny] INFO creating the file system for data treatment at ", input$Selected," if it does not exist\n"), value = 0.5)
            if (!(initial.dir == input$Selected)) {if (!dir.exists(input$Selected)) dir.create(input$Selected)}
            List.Dirs <- c("Calibration", "Configuration","Drift","Estimated_coef","General_data","Models","Modelled_gas","Outliers","scriptsLog","SensorData",
                           "Retrieved_plots","Statistics","Verification_plots")
            for (i in List.Dirs) {
                if (!dir.exists(file.path(input$Selected, i))) {
                    dir.create(file.path(input$Selected, i),
                               showWarning = TRUE, recursive = TRUE)
                    cat(paste0("[shiny, Selected] INFO Dir. created: ", file.path(input$Selected,i)), sep = "\n\r")
                } else cat(paste0("[shiny, Selected] INFO Dir. already exists: ", file.path(input$Selected,i)), sep = "\n")
            }
            remove(List.Dirs,i)
            # setting the current directory to the root of the file system with the name of the AirSensEUR
            cat("-----------------------------------------------------------------------------------\n")
            cat(paste0("[shiny, Selected] INFO Change the working directory to: ",input$Selected), sep = "\n")
            setwd(input$Selected)
            #----------------------------------------------------------CR
            # 1.c sending console to a file in the directory three (script log) and to variable Console for shiny TextOutput ####
            #----------------------------------------------------------CR
            while (sink.number() > 0) {print(paste0("Number of sink channels opened: ", sink.number(), ". Closing opened channels"))
                sink(file = NULL)
            }
            sink(file.path(input$Selected, "scriptsLog",paste0("console_", Sys.Date(),".log")),
                 type = c("output", "message"),
                 split = TRUE, append = TRUE ) # with split = TRUE we get the file on the screen and in log file
            cat("-----------------------------------------------------------------------------------\n")
            cat(paste0("[shiny, Selected] INFO Starting log file ", file.path(input$Selected, "scriptsLog",paste0("console_", Sys.Date(),".log"))), sep = "\n")
            progress$set(message = paste0("[shiny] INFO creating the file system for data treatment at ", input$Selected," if it does not exist\n"), value = 0.5)
            cat("-----------------------------------------------------------------------------------\n")
            # Now ASEConfig can be sourced with function CONFIG and SETTIME to update the config files
            # Changed this source does not do anything anymore
            #source(file.path(initial.dir, input$Selected))
            #  NavBar"GetData", sideBar tabPanel "time-shield" ----
            output$uiUserMins       <- renderUI({
                selectInput(inputId = "UserMins",
                            label = "Averaging time in min for downloading"            ,
                            choices = c("1","2","3","4","5","6","10","12","15","20","30","40","45","60", "480", "1440"),
                            selected = Config$all[["Server"]]$UserMins)})
            output$uiUserMinsAvg   <- renderUI({
                selectInput(inputId = "UserMinsAvg",
                            label = "Averaging time in min for predicted data"            ,
                            choices = c("1", "10","15","20","30","60", "480", "1440"),
                            selected = Config$all[["Server"]]$UserMinsAvg)})
            output$uiDelay          <- renderUI({
                if (!is.null(input$UserMins)) {
                    med.Index    <- median(seq(seq(-1440,1440, by = as.integer(input$UserMins))))
                    length.Index <- length(seq(seq(-1440,1440, by = as.integer(input$UserMins))))
                    selectInput(inputId = "Delay"   ,
                                label = "Delay in min, add minutes to sensor time (automatic Save)",
                                choices = seq(-1440, 1440, by = as.integer(input$UserMins))[max(med.Index - 500, 1):min(length.Index,med.Index + 500)], # also add a few minutes if not present
                                selected = Config$all[["Server"]]$Delay)}})
            output$uiasc.File       <- renderUI({
                selectInput("asc.File"   ,
                            label = "Sensor shield config file *.asc",
                            choices = choices.shield,
                            selected = choices.shield[choices.shield %in% Config$all[["Server"]]$asc.File])})
            output$uiNameSensors <- renderUI({
                radioButtons(inputId = "Sensors",
                             label   = "Select Sensor",
                             choices = list.name.sensor(), inline = TRUE)})
            #  NavBar"GetData", sideBar tabPanel "Proxy" ----
            output$uiPROXY    <- renderUI({
                checkboxInput("PROXY",
                              label = "Enable PROXY",
                              value = as.logical(Config$all[["Server"]]$PROXY))})
            output$uiURL      <- renderUI({
                textInput("URL",
                          label = "URL of your proxy",
                          value = Config$all[["Server"]]$URL)})
            output$uiPORT     <- renderUI({
                textInput("PORT",
                          label = "PORT",
                          value = Config$all[["Server"]]$PORT)})
            output$uiLOGIN    <- renderUI({
                textInput("LOGIN",
                          label = "LOGIN for the proxy",
                          value = Config$all[["Server"]]$LOGIN)})
            output$uiPASSWORD <- renderUI({
                passwordInput("PASSWORD",
                              label = "PASSWORD of the proxy",
                              value = Config$all[["Server"]]$PASSWORD)})
            #  NavBar"GetData", sideBar tabPanel "Influx" ----
            output$uiDown.Influx <- renderUI({
                checkboxInput("Down.Influx",
                              label = "Enable InFluxDB",
                              value = as.logical(Config$all[["Server"]]$Down.Influx))})
            output$uiHost <- renderUI({
                textInput("Host", label = "URL of Influx Rest API",
                          value = Config$all[["Server"]]$Host)})
            output$uiPort <- renderUI({
                selectInput("Port",
                            label = "Port",
                            choices = c("3000","8086"),
                            selected = Config$all[["Server"]]$Port)})
            output$uiUser <- renderUI({
                textInput("User",
                          label = "Login for the Host",
                          value = Config$all[["Server"]]$User)})
            output$uiPass <- renderUI({
                passwordInput("Pass",
                              label = "Password ",
                              value = Config$all[["Server"]]$Pass
                )
            })
            output$uiDb   <- renderUI({
                textInput("Db",
                          label = "SQLite database",
                          value = Config$all[["Server"]]$Db)})
            ASE.names.Influx     <- reactive({
                # set the list of available datasets at the on-line db
                # depends: input$PROXY to set or reset proxy with
                #               input$LOGIN, input$URL, input$PORT, input$LOGIN and input$PASSWORD
                # Set PROXY
                # Create a Progress object
                progress <- shiny::Progress$new()
                # Make sure it closes when we exit this reactive, even if there's an error
                on.exit(progress$close())
                progress$set(message = "Connecting to Influx server", value = 0.5)
                if (!is.null(input$PROXY)) {
                    if (length(input$Down.Influx) != 0) {
                        if (input$Down.Influx && !is.null(input$Down.Influx)) {
                            # detect names only if Down.Influx is checked
                            if (input$Down.Influx) {
                                if (input$PROXY) {
                                    if (input$LOGIN == "") {
                                        set_config(use_proxy(url  = input$URL, port = as.numeric(input$PORT)))
                                    } else {
                                        set_config(use_proxy(url      = input$URL,
                                                             port     = as.numeric(input$PORT),
                                                             username = input$LOGIN,
                                                             password = input$PASSWORD))}
                                } else reset_config()
                                # Check connection to InfluxDB server
                                Influx.con <- httr::GET(paste0("http://",input$Host,":",input$Port,"/ping"),
                                                        config = authenticate(user = input$User,
                                                                              password = input$Pass,
                                                                              type = "basic"))
                                if (Influx.con$status_code != 204) { # code 204 : no content
                                    my_message <- paste0("[shiny, ASE.names.Influx()] ERROR connecting to the InfluxDB server. Error : \"",
                                                         http_status(Influx.con)$message,
                                                         "\". Change the server URL and/or Port, login and/or password.\n")
                                    cat(my_message)
                                    shinyalert(
                                        title = "ERROR connecting to the InfluxDB server",
                                        text = my_message,
                                        closeOnEsc = TRUE,
                                        closeOnClickOutside = TRUE,
                                        html = FALSE,
                                        type = "error",
                                        showConfirmButton = TRUE,
                                        showCancelButton = FALSE,
                                        confirmButtonText = "OK",
                                        confirmButtonCol = "#AEDEF4",
                                        timer = 0,
                                        imageUrl = "",
                                        animation = FALSE
                                    )
                                } else {
                                    my_message <- paste0("[shiny, ASE.names.Influx()] INFO, Influx server is up; connected to server. Message : \"", http_status(Influx.con)$message, "\".\n")
                                    cat(my_message)
                                    shinyalert(
                                        title = "Influx server is up",
                                        text = my_message,
                                        closeOnEsc = TRUE,
                                        closeOnClickOutside = TRUE,
                                        html = FALSE,
                                        type = "success",
                                        showConfirmButton = TRUE,
                                        showCancelButton = FALSE,
                                        confirmButtonText = "OK",
                                        confirmButtonCol = "#AEDEF4",
                                        timer = 0,
                                        imageUrl = "",
                                        animation = FALSE
                                    )
                                    series <- httr::GET(URLencode(paste0("http://",input$Host,":",input$Port,"/query?db=", input$Db)),
                                                        config = authenticate(user = input$User,
                                                                              password = input$Pass),
                                                        query = list(q = "SHOW SERIES")) # this is too slow: q = "select * from /.*/ limit 1"
                                    if (series$status_code != 200) {
                                        my_message <- paste0("[shiny, ASE.names.Influx()] ERROR reading the names of AirSensEUR boxes availabe at the InfluxDB server. Error :",series$status_code, " \".\n")
                                        cat(my_message)
                                        shinyalert(
                                            title = "ERROR reading the names of AirSensEUR boxes",
                                            text = my_message,
                                            closeOnEsc = TRUE,
                                            closeOnClickOutside = TRUE,
                                            html = FALSE,
                                            type = "error",
                                            showConfirmButton = TRUE,
                                            showCancelButton = FALSE,
                                            confirmButtonText = "OK",
                                            confirmButtonCol = "#AEDEF4",
                                            timer = 0,
                                            imageUrl = "",
                                            animation = FALSE
                                        )
                                    } else {
                                        series <- jsonlite::fromJSON(content(series, "text",
                                                                             encoding = "ISO-8859-1"),
                                                                     simplifyVector = TRUE,
                                                                     flatten = TRUE)
                                        series <- series$results$series[[1]]$values[[1]]
                                        series <- unique(sapply(strsplit(x = series, split = ","),function(x) x[1]))
                                        series <- series[!grepl(pattern = paste(c("_Boards", "_Sensors"), collapse = "|"), x = series)]
                                        print(series)
                                        return(series)
                                    }
                                }
                            } else return(Config$all[["Server"]]$Dataset)
                        } else return(Config$all[["Server"]]$Dataset)
                    } else return(Config$all[["Server"]]$Dataset)
                } else return(Config$all[["Server"]]$Dataset)
                progress$set(message = "Connecting to Influx server", value = 1)
            })
            output$ASE.Box       <- renderUI({
                selectInput("Dataset",
                            label = "Available Datasets",
                            choices = ASE.names.Influx(),
                            selected = Config$all[["Server"]]$Dataset)})
            observeEvent(input$Down.Influx, {
                if (input$Down.Influx ) {
                    updateSelectInput(session = session,
                                      inputId  = "Dataset",
                                      choices  = ASE.names.Influx(),
                                      selected = Config$all[["Server"]]$Dataset)
                } else {
                    updateSelectInput(session = session,
                                      inputId  = "Dataset",
                                      choices  = ASE.names.Influx())
                }})
            output$uiInflux.TZ          <- renderUI({
                selectInput("Influx.TZ",
                            label = "Time Zone, Influx data are in UTC:",
                            choices = Influx.TimeZone,
                            selected = TimeZone[match(x = Config$all[["Server"]]$Influx.TZ,
                                                      table = Influx.TableTZ$TimeZone)])})
            output$uiDown_Influx <- renderUI({
                actionButton(inputId = "Down_Influx" ,
                             label = "Download Influx data")})
            #  NavBar"GetData", sideBar tabPanel "SOS" ----
            output$uiDown.SOS   <- renderUI({
                checkboxInput(inputId = "Down.SOS",
                              label = "Enable SOS",
                              value = as.logical(Config$all[["Server"]]$Down.SOS))})
            output$uiAirsensWeb <- renderUI({
                textInput("AirsensWeb",
                          label = "URL of SOS Rest API",
                          value = Config$all[["Server"]]$AirsensWeb)})
            output$uiPort       <- renderUI({
                selectInput("Port", label = "Port",
                            choices = c("3000","8086"),
                            selected = Config$all[["Server"]]$Port)})
            ASE.names.SOS       <- reactive({
                # Create a Progress object
                progress <- shiny::Progress$new()
                # Make sure it closes when we exit this reactive, even if there's an error
                on.exit(progress$close())
                progress$set(message = "Connecting to SOS server", value = 0.5)
                if (!is.null(input$PROXY)) {
                    if (length(input$Down.SOS) != 0) {
                        # detect names only if Down.SOS is checked
                        if (input$Down.SOS && !is.null(input$Down.SOS)) {
                            # Set PROXY
                            if (input$PROXY) {
                                if (is.null(input$LOGIN)) {
                                    set_config(use_proxy(url = input$URL,
                                                         port = as.numeric(input$PORT)))
                                } else {
                                    set_config( use_proxy(url = input$URL,
                                                          port =as.numeric(input$PORT),
                                                          username = input$LOGIN,
                                                          password = input$PASSWORD))
                                }
                            } else reset_config()
                            list.packages.github <- c("52North/sensorweb4R")
                            for (i in list.packages.github) {
                                # removing author name anad version number
                                lib.i <- tail(unlist(strsplit(i, split = "/")), n = 1)
                                lib.i <- head(unlist(strsplit(lib.i, split = "@")), n = 1)
                                if (!(lib.i %in% rownames(installed.packages()))) {
                                    devtools::install_github(i)
                                    cat(sprintf("Package ", lib.i, " installed"), sep = "\n")
                                } else cat(paste0("[shiny, ASE.names.SOS()] INFO, Package ", i, " already installed"), sep = "\n")
                                do.call("library", as.list(lib.i))
                                cat(sprintf("[shiny, ASE.names.SOS()] INFO, Package %s loaded",i), sep = "\n")
                            }
                            # connect
                            apiEndpoint <- Endpoint(input$AirsensWeb)
                            # number of category at the apiEndpoint
                            cat(paste0("[shiny, ASE.names.SOS()] INFO, in total ", length(timeseries(apiEndpoint)), " Sensors at the SOS Rest API."), sep = "\n")
                            # Selecting service "AirSensEUR" with name
                            srv <- services(apiEndpoint)
                            # get all phenomena
                            phe <- phenomena(apiEndpoint)
                            print(label(phenomena(apiEndpoint)), quote = FALSE)
                            print(label(stations(srv)), quote = FALSE)
                            return(label(stations(srv)))
                        } else return(Config$all[["Server"]]$AirSensEur.name)
                    }
                }
                progress$set(message = "Connecting to SOS server", value = 1.0)
            })
            output$ASE.name     <- renderUI({
                selectInput("AirSensEur.name",
                            label = "SOS ID of the AirSensEUR box",
                            choices = ASE.names.SOS(),
                            selected = Config$all[["Server"]]$AirSensEur.name)})
            output$uiSOS.tzone <- renderUI({
                selectInput("SOS.TZ",
                            label = "Time Zone",
                            choices = TimeZone,
                            selected = TimeZone[match(x = Config$all[["Server"]]$SOS.TZ,
                                                      table = TableTZ$TimeZone)])})
            output$uiDown_SOS <- renderUI({
                actionButton("Down_SOS"    ,
                             label = "Download SOS data")})
            #  NavBar"GetData", sideBar tabPanel "Reference" ----
            output$uiDown.Ref       <- renderUI({
                checkboxInput("Down.Ref",
                              label = "Enable download Reference data",
                              value = as.logical(Config$all[["Server"]]$Down.Ref))})
            output$uiSelected       <- renderUI({
                radioButtons(inputId = "FTPMode",
                             label = "Selected download",
                             choices = c("csv", "ftp", "SOS", "a_i_p"),
                             selected = Config$all[["Server"]]$FTPMode,
                             inline = TRUE)})
            output$uiurlref         <- renderUI({
                textAreaInput(inputId = "urlref",
                              label   = "URL of the ftp server with full name",
                              value   = Config$all[["Server"]]$urlref,
                              rows    = 4,
                              resize = "vertical")})
            output$uiRefSOSname <- renderUI({
                textInput("RefSOSname",
                          label = "URL of SOS Rest API",
                          value = Config$all[["Server"]]$RefSOSname)})
            Ref.SOS.name        <- reactive({
                # return a dataframe of avaialbe stations at the rest API, 1st column station name, 2nd column pollutant
                # if there is not rest API or Down_ref FALSE, return character vector Config$all[["Server"]]$Ref.SOS.name
                # Create a Progress object
                progress <- shiny::Progress$new()
                # Make sure it closes when we exit this reactive, even if there's an error
                on.exit(progress$close())
                progress$set(message = "Connecting to SOS server of Reference station", value = 0.5)
                if (!is.null(input$PROXY)) {
                    if (length(input$Down.Ref) != 0) {
                        # detect names only if Down.SOS is checked
                        if (input$Down.Ref && !is.null(input$Down.Ref) & input$FTPMode == "SOS") {
                            # Detect SOS rest API only if input$RefSOSname is not empty
                            if (!is.null(input$RefSOSname)) {
                                if (!(input$RefSOSname == "")) {
                                    # Set PROXY
                                    if (input$PROXY) {
                                        if (is.null(input$LOGIN)) {
                                            set_config(use_proxy(url  = input$URL,
                                                                 port = as.numeric(input$PORT)))
                                        } else {
                                            set_config( use_proxy(url      = input$URL,
                                                                  port     = as.numeric(input$PORT),
                                                                  username = input$LOGIN,
                                                                  password = input$PASSWORD))
                                        }
                                    } else reset_config()
                                    # Installing packages
                                    list.packages.github <- c("52North/sensorweb4R")
                                    for (i in list.packages.github) {
                                        # removing author name anad version number
                                        lib.i <- tail(unlist(strsplit(i, split = "/")), n = 1)
                                        lib.i <- head(unlist(strsplit(lib.i, split = "@")), n = 1)
                                        if (!(lib.i %in% rownames(installed.packages()))) {
                                            devtools::install_github(i)
                                            cat(sprintf("Package ", lib.i, " installed"), sep = "\n")
                                        } else cat(paste0("[shiny, Ref.SOS.name()] INFO, Package ", i, " already installed"), sep = "\n")
                                        do.call("library", as.list(lib.i))
                                        cat(sprintf("[shiny, Ref.SOS.name()] INFO, Package %s loaded",i), sep = "\n")
                                    }
                                    # connect
                                    apiEndpoint <- Endpoint(input$RefSOSname)
                                    # number of category at the apiEndpoint
                                    my_message <- paste0("[shiny, Ref.SOS.name()] INFO, in total ", length(timeseries(apiEndpoint)), " Sensors at the SOS Rest API", "\n")
                                    cat(paste0("[shiny, Ref.SOS.name()] INFO, in total ", length(timeseries(apiEndpoint)), " Sensors at the SOS Rest API"), sep = "\n")
                                    shinyalert(
                                        title = "Connected to SOS server",
                                        text = my_message,
                                        closeOnEsc = TRUE,
                                        closeOnClickOutside = TRUE,
                                        html = FALSE,
                                        type = "success",
                                        showConfirmButton = TRUE,
                                        showCancelButton = FALSE,
                                        confirmButtonText = "OK",
                                        confirmButtonCol = "#AEDEF4",
                                        timer = 0,
                                        imageUrl = "",
                                        animation = FALSE
                                    )
                                    # Selecting service "AirSensEUR" with name
                                    srv <- services(apiEndpoint)
                                    # get all phenomena
                                    phe <- phenomena(apiEndpoint)
                                    cat(paste0(label(phenomena(apiEndpoint)), "\n"))
                                    cat(paste0(label(stations(srv)), "\n"))
                                    #returing station list
                                    return(
                                        unique(
                                            as.data.frame(
                                                matrix(
                                                    unlist(
                                                        strsplit(x = label(stations(srv)),
                                                                 split = ":")
                                                    ),
                                                    nrow = length(label(stations(srv))),
                                                    byrow = T
                                                ),
                                                stringsAsFactors = FALSE
                                            )
                                        )
                                    )
                                } else return(Config$all[["Server"]]$Ref.SOS.name)
                            } else return(Config$all[["Server"]]$Ref.SOS.name)
                        } else return(Config$all[["Server"]]$Ref.SOS.name)
                    }  else return(Config$all[["Server"]]$Ref.SOS.name)
                }
                progress$set(message = "Connecting to SOS server of Reference station", value = 1.0)
            })
            output$uiRef.SOS.name <- renderUI({
                selectInput(inputId  = "Ref.SOS.name",
                            label    = "SOS ID of the Reference station",
                            choices  = Config$all[["Server"]]$Ref.SOS.name,
                            selected = Config$all[["Server"]]$Ref.SOS.name)})
            output$uiRefPollutants <- renderUI({
                selectInput("RefPollutants",
                            label    = "List of pollutants at the Reference station",
                            choices  = gsub(pattern = "!", replacement = " ", x = Config$all[["Server"]]$RefPollutants),
                            selected = gsub(pattern = "!", replacement = " ", x = Config$all[["Server"]]$RefPollutants),
                            multiple = TRUE)})
            output$uiRefDate <- renderUI({
                dateRangeInput(inputId = "RefDateDownload",
                               label   = "Range of dates for downloading data of station:",
                               format  = "yyyy-mm-dd",
                               start   = Config$all[["Server"]]$RefDateEnd,
                               end     = as.Date(Sys.Date()),
                               weekstart = 1,
                               min = as.Date("2015-01-01"),
                               max = Sys.Date())})
            output$uiRef__a_i_p__name <- renderUI({
                textInput(inputId  = "Ref__a_i_p__name",
                          label = "URL of a_i_p Rest API ",
                          value = Config$all[["Server"]]$Ref__a_i_p__name)})
            output$ui__a_i_p__User <- renderUI({
                textInput(inputId  = "User__a_i_p__",
                          label = "Login",
                          value = Config$all[["Server"]]$User__a_i_p__)})
            output$ui__a_i_p__Pass <- renderUI({
                passwordInput(inputId  = "Pass__a_i_p__",
                              label = "Password ",
                              value = Config$all[["Server"]]$Pass__a_i_p__)})
            output$uiRef__a_i_p__Organisation <- renderUI({
                textInput(inputId  = "Ref__a_i_p__Organisation",
                          label    = "ID of the Organisation",
                          value = Config$all[["Server"]]$Ref__a_i_p__Organisation)})
            output$uiRef__a_i_p__Station <- renderUI({
                textInput(inputId  = "Ref__a_i_p__Station",
                          label    = "ID of the station",
                          value = Config$all[["Server"]]$Ref__a_i_p__Station)
            })
            output$uiRef__a_i_p__Pollutants <- renderUI({
                selectInput("Ref__a_i_p__Pollutants",
                            label    = "Select pollutants to download",
                            choices  = Config$all[["Server"]]$Ref__a_i_p__Pollutants,
                            selected = Config$all[["Server"]]$Ref__a_i_p__Pollutants,
                            multiple = TRUE)
            })
            Ref__a_i_p__Pollutants        <- reactive({
                # return a vecor of avaialbe parameters at the rest API, for the organisation and station
                # if there is not rest API or Down_ref FALSE, return character vector Config$all[["Server"]]$Ref__a_i_p__Pollutants
                # Create a Progress object
                progress <- shiny::Progress$new()
                # Make sure it closes when we exit this reactive, even if there's an error
                on.exit(progress$close())
                progress$set(message = "Connecting to a_i_p server", value = 0.5)
                # Waiting for PROXY
                if (!is.null(input$PROXY)) {
                    # detect names only if Down.Ref is checked
                    if (length(input$Down.Ref) != 0 && input$Down.Ref && !is.null(input$Down.Ref)) {
                        # detect names only if a_i_p is selected
                        if (input$FTPMode == "a_i_p") {
                            # Detect rest API only if Ref__a_i_p__name,  User__a_i_p__, Pass__a_i_p__,
                            # Ref__a_i_p__Organisation, Ref__a_i_p__Station, Ref__a_i_p__Date is not empty
                            if (!any(c(input$Ref__a_i_p__name, input$User__a_i_p__, input$Pass__a_i_p__,
                                       input$Ref__a_i_p__Organisation, input$Ref__a_i_p__Station, input$Ref__a_i_p__Date[1]) %in% "")) {
                                # Set PROXY
                                if (input$PROXY) {
                                    if (is.null(input$LOGIN)) {
                                        set_config(use_proxy(url  = input$URL,
                                                             port = as.numeric(input$PORT)))
                                    } else {
                                        set_config( use_proxy(url      = input$URL,
                                                              port     = as.numeric(input$PORT),
                                                              username = input$LOGIN,
                                                              password = input$PASSWORD))
                                    }
                                } else reset_config()
                                # detect available pollutants
                                param <- a_i_p_param(URL = input$Ref__a_i_p__name,
                                                     username = input$User__a_i_p__,
                                                     password = input$Pass__a_i_p__,
                                                     organisation = input$Ref__a_i_p__Organisation,
                                                     station = input$Ref__a_i_p__Station,
                                                     start = input$Ref__a_i_p__Date[1],
                                                     end = input$Ref__a_i_p__Date[2])
                                # number of pollutatns at the apiEndpoint
                                if (length(param) > 0) {
                                    my_message <- paste0("[shiny, Ref.a_i_p.name()] INFO, in total ", length(param), " pollutants at the a_i_p Rest API", "\n")
                                    cat(my_message)
                                    shinyalert(
                                        title = "Connected to a_i_p server",
                                        text = my_message,
                                        closeOnEsc = TRUE,
                                        closeOnClickOutside = TRUE,
                                        html = FALSE,
                                        type = "success",
                                        showConfirmButton = TRUE,
                                        showCancelButton = FALSE,
                                        confirmButtonText = "OK",
                                        confirmButtonCol = "#AEDEF4",
                                        timer = 0,
                                        imageUrl = "",
                                        animation = FALSE)
                                    # Updating list of pollutants
                                    updateSelectInput(session = session,
                                                      inputId = "Ref__a_i_p__Pollutants",
                                                      choices  = param,
                                                      selected = param)
                                    #returing pollutant list
                                    return(param)
                                } else {
                                    my_message <- paste0("[shiny, Ref.a_i_p.name()] ERROR, no pollutants found on start date with current parameters\n")
                                    cat(my_message)
                                    shinyalert(
                                        title = "ERROR Connection to a_i_p server",
                                        text = my_message,
                                        closeOnEsc = TRUE,
                                        closeOnClickOutside = TRUE,
                                        html = FALSE,
                                        type = "error",
                                        showConfirmButton = TRUE,
                                        showCancelButton = FALSE,
                                        confirmButtonText = "OK",
                                        confirmButtonCol = "#AEDEF4",
                                        timer = 0,
                                        imageUrl = "",
                                        animation = FALSE)
                                    #returing pollutant list
                                    return(Config$all[["Server"]]$Ref__a_i_p__Pollutants)
                                }
                            } else return(Config$all[["Server"]]$Ref__a_i_p__Pollutants)
                        } else return(Config$all[["Server"]]$Ref__a_i_p__Pollutants)
                    }  else return(Config$all[["Server"]]$Ref__a_i_p__Pollutants)
                }
                progress$set(message = "Connecting to a_i_p server", value = 1.0)
            })
            observe(updateSelectInput(session = session, inputId = "uiRef__a_i_p__Pollutants",
                                      choices = Ref__a_i_p__Pollutants(),
                                      selected = Ref__a_i_p__Pollutants()))
            output$uiRef__a_i_p__Date <- renderUI({
                dateRangeInput(inputId = "Ref__a_i_p__Date",
                               label   = "Range of dates for downloading data of station:",
                               format  = "yyyy-mm-dd",
                               start   = Config$all[["Server"]]$Ref.a_i_p.DateEND,
                               end     = as.Date(Sys.Date()),
                               weekstart = 1,
                               min = as.Date("2015-01-01"),
                               max = Sys.Date())})
            output$uicoord.ref.Long      <- renderUI({
                textInput(inputId = "coord.ref.Long",
                          label   = paste0("Longitude in decimal degrees or d\'m\'s.s\'E"),
                          value   = unlist(strsplit(Config$all[["Server"]]$coord.ref, split = ","))[1])})
            output$uicoord.ref.Lat      <- renderUI({
                textInput(inputId = "coord.ref.Lat",
                          label   = paste0("Latitude in decimal degrees or d\'m\'s.s\'N"),
                          value   = unlist(strsplit(Config$all[["Server"]]$coord.ref, split = ","))[2])})
            observeEvent(
                input$Down.Ref, {
                    if (input$Down.Ref & class(Ref.SOS.name()) == "data.frame") {
                        updateSelectInput(session = session,
                                          inputId  = "Ref.SOS.name",
                                          choices  = unique(Ref.SOS.name()[,1]),
                                          selected = Config$all[["Server"]]$Ref.SOS.name
                        )
                    } else {
                        updateSelectInput(session = session,
                                          inputId  = "Ref.SOS.name",
                                          choices  = Ref.SOS.name())
                    }
                    if (!is.null(input$RefPollutants)) {
                        if (input$Down.Ref & class(Ref.SOS.name()) == "data.frame") {
                            selectInput("RefPollutants",
                                        label    = "List of pollutants at the Reference station",
                                        choices  = Ref.SOS.name()[Ref.SOS.name()[,1] == input$Ref.SOS.name,2],
                                        selected = Ref.SOS.name()[Ref.SOS.name()[,1] == input$Ref.SOS.name,2],
                                        multiple = TRUE)
                        } else {
                            selectInput("RefPollutants",
                                        label    = "List of pollutants at the Reference station",
                                        choices  = gsub(pattern = "!", replacement = " ", x = Config$all[["Server"]]$RefPollutants),
                                        selected = gsub(pattern = "!", replacement = " ", x = Config$all[["Server"]]$RefPollutants),
                                        multiple = TRUE)}
                    }
                }
            )
            observeEvent(
                input$Ref.SOS.name, {
                    if (!is.null(input$PROXY)) {
                        if (input$Down.Ref && input$Ref.SOS.name != "") {
                            # Updating list of pollutants every time another station is selected
                            updateSelectInput(session = session,
                                              inputId = "RefPollutants",
                                              choices  = Ref.SOS.name()[Ref.SOS.name()[,1] == input$Ref.SOS.name,2],
                                              selected = Ref.SOS.name()[Ref.SOS.name()[,1] == input$Ref.SOS.name,2])

                            # updating coordinates of SOS Ref station
                            # apiEndpoint
                            apiEndpoint <- sensorweb4R::Endpoint(input$RefSOSname)
                            # Selecting service
                            srv <- sensorweb4R::services(apiEndpoint)
                            # Selecting Station
                            sta  <- sensorweb4R::stations(srv)[grep(pattern = input$Ref.SOS.name, x = label(sensorweb4R::stations(srv)))]
                            geom <- sp::geometry(sta[1])
                            updateTextInput(session = session,
                                            inputId = "coord.ref.Long",
                                            value = paste0(geom@coords[1,], collapse = " "))
                            updateTextInput(session = session,
                                            inputId = "coord.ref.Lat",
                                            value = paste0(geom@coords[1,], collapse = " "))
                        } else {
                            # if no station selected use the ASE_server.cfg file
                            updateSelectInput(session  = session,
                                              inputId  = "RefPollutants",
                                              choices  = gsub(pattern = "!", replacement = " ", x = Config$all[["Server"]]$RefPollutants),
                                              selected = gsub(pattern = "!", replacement = " ", x = Config$all[["Server"]]$RefPollutants)
                            )
                            updateTextInput(session     = session,
                                            inputId     = "coord.ref.Long",
                                            value       = unlist(strsplit(Config$all[["Server"]]$coord.ref, split = ","))[1])
                            updateTextInput(session     = session,
                                            inputId     = "coord.ref.Lat",
                                            value       = unlist(strsplit(Config$all[["Server"]]$coord.ref, split = ","))[2])}
                    }
                }
            )
            # Navbar Menu "GetData", SideBar Panel, TabPanel "Reference"-"csv"
            observe({
                if (input$browse == 0) return()
                if (isOS == "windows") {
                    updateTextInput(session = session,
                                    inputId = "file1",
                                    value = choose.files(default = paste0(file.path(getwd(), "General_data"), "/*.csv"),
                                                         caption = "Select csv, dat or Rdata file",
                                                         filters = rbind(c("csv files (*.csv)","*.csv"),
                                                                         c("Rdata files (*.Rdata)","*.Rdata"),
                                                                         c("Text files (*.txt)","*.txt")),
                                                         multi = FALSE,
                                                         index = 1
                                    )
                    )
                } else {
                    if (isOS == "unix") {
                        # Checking if rstudio-server is used, avoid loading files on the server
                        if (!is.null(Sys.getenv()[["RSTUDIO_PANDOC"]])) {
                            if (!grepl(pattern = "server", x = Sys.getenv()[["RSTUDIO_PANDOC"]])) {
                                if (require(rChoiceDialogs)) {
                                    updateTextInput(session = session,
                                                    inputId = "file1",
                                                    value = rchoose.files(default = paste0(getwd(),"/*.csv"),
                                                                          caption = "Select csv, dat or txt file",
                                                                          filters = rbind(c("csv files (*.csv)","*.csv"),
                                                                                          c("dat files (*.dat)","*.txt"),
                                                                                          c("Text files (*.txt)","*.txt"),
                                                                                          c("All files (*.*)", "*.*")),
                                                                          multi = FALSE,
                                                                          index = 1
                                                    )
                                    )
                                } else {
                                    updateTextInput(session = session,
                                                    inputId = "file1",
                                                    value = file.choose()#,
                                                    #multi = FALSE,
                                                    #index = 1
                                    )
                                }
                            } else {
                                my_message <- paste0("[Shiny] ERROR uploading files to the shiny server is not allowed. Please run the shiny code on your local PC to upload reference data with a csv file.\n")
                                shinyalert(
                                    title = "ERROR uploading files to the shiny server not permitted",
                                    text = my_message,
                                    closeOnEsc = TRUE,
                                    closeOnClickOutside = TRUE,
                                    html = FALSE,
                                    type = "error",
                                    showConfirmButton = TRUE,
                                    showCancelButton = FALSE,
                                    confirmButtonText = "OK",
                                    confirmButtonCol = "#AEDEF4",
                                    timer = 0,
                                    imageUrl = "",
                                    animation = FALSE
                                )
                            }
                        }
                    }
                }
            })
            output$uiReference.name <- renderUI({
                textInput("Reference.name",
                          label = "Identifier of the reference station",
                          value = Config$all[["Server"]]$Reference.name)})
            output$uialt.ref        <- renderUI({
                textInput("alt.ref",
                          label = "Altitude of the reference station",
                          value = Config$all[["Server"]]$alt.ref)})
            output$uiref.tzone      <- renderUI({
                selectInput("ref.tzone",
                            label = "Time Zone",
                            choices = TimeZone,
                            selected = TimeZone[match(x = Config$all[["Server"]]$ref.tzone,
                                                      table = TableTZ$TimeZone)])})
            output$uiDown_Ref       <- renderUI({
                actionButton(inputId = "Down_Ref"   ,
                             label   = "Download Reference data")})
            # The "Down.Influx" control is mandatory for downloading influx data and thus the "Down_Influx" button should not be enabled if it is FALSE
            # https://cran.r-project.org/web/packages/shinyjs/vignettes/shinyjs-example.html
            observe({
                if (!is.null(input$Down.Influx)) {
                    if (input$Down.Influx  && !is.null(input$Dataset) && input$Dataset != "") {
                        shinyjs::enable("Down_Influx")
                    } else shinyjs::disable("Down_Influx")
                } else shinyjs::disable("Down_Influx")
            })
            # The "Down.SOS" control is mandatory for downloading influx data and thus the "Down_SOS" button should not be enabled if it is FALSE
            # https://cran.r-project.org/web/packages/shinyjs/vignettes/shinyjs-example.html
            observe({
                if (!is.null(input$Down.SOS)) {
                    if (input$Down.SOS) {
                        shinyjs::enable("Down_SOS")
                    } else {
                        shinyjs::disable("Down_SOS")
                    }
                }
            })
            # The "Down.Ref" control is mandatory for downloading data reference data and that config parameters are correct
            # https://cran.r-project.org/web/packages/shinyjs/vignettes/shinyjs-example.html
            observe({
                if (!is.null(input$Down.Ref) && input$Down.Ref) {
                    if (input$FTPMode == "ftp") {
                        if (!is.null(input$urlref) && input$urlref != "") shinyjs::enable("Down_Ref") else shinyjs::disable("Down_Ref")
                    } else {
                        if (input$FTPMode == "csv") {
                            if (!any(is.null(c(input$file1, input$sep, input$quote))) && input$file1 != "") shinyjs::enable("Down_Ref") else shinyjs::disable("Down_Ref")
                        } else {
                            if (input$FTPMode == "SOS") {
                                if (!is.null(input$RefPollutants)) shinyjs::enable("Down_Ref") else shinyjs::disable("Down_Ref")
                            } else if (input$FTPMode == "a_i_p") {
                                if (!is.null(input$Ref__a_i_p__Pollutants) && input$Ref__a_i_p__Pollutants != "") shinyjs::enable("Down_Ref") else shinyjs::disable("Down_Ref")
                            }
                        }
                    }
                } else shinyjs::disable("Down_Ref")})
            #  NavBar"GetData", mainTabPanel "GetData Panel" reporting of data ----
            output$PROXY            <- renderText({ paste("Enable Proxy                      : "  , input$PROXY) })
            output$URL              <- renderText({ paste("URL of your proxy                 : "  , input$URL) })
            output$PORT             <- renderText({ paste("PORT proxy                        : "  , input$PORT) })
            output$LOGIN            <- renderText({ paste("Login for the proxy               : "  , input$LOGIN) })
            output$Down.Influx      <- renderText({ paste("Enable Influx download            : "  , input$Down.Influx) })
            output$Host             <- renderText({ paste("URL of the Influx server          : "  , input$Host) })
            output$Port             <- renderText({ paste("Port URL                          : "  , input$Port) })
            output$User             <- renderText({ paste("Login for the Server              : "  , input$User) })
            output$Db               <- renderText({ paste("SQLite database at the Influx server (*.db): "    , input$Db) })
            output$Dataset          <- renderText({ paste("Table in the SQLite database      : "  , input$Dataset) })
            output$Influx.TZ        <- renderText({ paste("Influx data time zone             : "  , input$Influx.TZ) })
            output$Down.SOS         <- renderText({ paste("Enable SOS download               : "  , input$Down.SOS) })
            output$AirsensWeb       <- renderText({ paste("URL of the SOS server             : "  , input$AirsensWeb) })
            output$AirSensEur.name  <- renderText({ paste("AirsensEur ID at the SOS server   : "  , input$AirSensEur.name) })
            output$SOS.TZ           <- renderText({ paste("SOS data time zone                : "  , input$SOS.TZ) })
            output$Down.Ref         <- renderText({ paste("Enable Reference data download    : "  , input$Down.Ref) })
            output$FTPMode          <- renderText({ paste("Selected download of Ref data     : "  , input$FTPMode) })
            output$csvref           <- renderText({ paste("csv file the reference data       : "  , input$file1) })
            output$urlref           <- renderText({ paste("ftp of the reference server       : "  , input$urlref) })
            output$GDPRefSOSname       <- renderText({ paste("Reference station SOS Rest API URL: "  , input$RefSOSname) })
            output$GDPRef.SOS.name     <- renderText({ paste("SOS ID of the Reference station   : "  , input$Ref.SOS.name) })
            output$GDPRefPollutants    <- renderText({ paste("Pollutants at the ref. SOS station: "  , input$RefPollutants) })
            output$GDPRefDateDownload  <- renderText({ paste("Dates for Downloading Ref SOS data: "  , input$RefDateDownload) })
            output$GDPRef__a_i_p__name         <- renderText({ paste("URL of a_i_p Rest API             : "  , input$Ref__a_i_p__name) })
            output$GDPRefUser__a_i_p__         <- renderText({ paste("Login for the a_i_p Rest API      : "  , input$User__a_i_p__) })
            output$GDPRef__a_i_p__Organisation <- renderText({ paste("Organisation managing the station : "  , input$Ref__a_i_p__Organisation) })
            output$GDPRef__a_i_p__Station      <- renderText({ paste("ID of the reference station       : "  , input$Ref__a_i_p__Station) })
            output$GDPRef__a_i_p__Pollutants   <- renderText({ paste("Pollutants at the station         : "  , input$Ref__a_i_p__Pollutants) })
            output$GDPRef__a_i_p__DateDownload <- renderText({ paste("Dates for Downloading a_i_p data  : "  , paste0(input$Ref__a_i_p__DateDownload, sep = " ")) })
            output$Reference.name   <- renderText({ paste("Name of the refence station       : "  , input$Reference.name) })
            output$coord.ref        <- renderText({ paste("Long. and lat. of the ref. station: "  , paste0(input$coord.ref.Long,", ",input$coord.ref.Lat)) })
            output$alt.ref          <- renderText({ paste("Altitude of the reference station : "  , input$alt.ref) })
            output$ref.tzone        <- renderText({ paste("Time Zone of the ref. station     : "  , input$ref.tzone) })
            output$UserMins         <- renderText({ paste("Averaging time in min for download: "  , input$UserMins) })
            output$UserMinsAvg      <- renderText({ paste("Averaging time in min for Prediction: ", input$UserMinsAvg) })
            output$Delay            <- renderText({ paste("Delay in min sensor vs reference  : "  , input$Delay) })
            output$asc.File         <- renderText({ paste("Sensor shield config file *.asc   : "  , input$asc.File) })
            output$Shield           <- renderTable(Shield())
            # mainPanel - NavBarMenu "Get Data"
            output$Influx.Content    <- renderPrint({
                # Checking if the Rdata exist
                if (!is.null(Influx$DATA)) {
                    options(datatable.print.topn = 5)
                    return(Influx$DATA)
                } else return("[shiny, Influx.Content] INFO, file InfluxData.Rdata does not exist")})
            output$SOS.Content    <- renderPrint({
                # Checking if the Rdata exist
                if (!is.null(Sos$DATA)) {
                    options(datatable.print.topn = 5)
                    return(Sos$DATA)
                } else return("[shiny, SOS.Content] INFO, file SOSData.Rdata does not exist")})
            output$Ref.content <- renderPrint({
                # Checking if the Rdata exist
                if (!is.null(Ref$DATA)) {
                    options(datatable.print.topn = 5)
                    return(Ref$DATA)
                } else return("[shiny, Ref.content] INFO, file RefData.Rdata does not exist")})
            output$General.Content <- renderPrint({
                # Checking if the Rdata exist
                if (!is.null(DF$General)) {
                    #https://cran.r-project.org/web/packages/data.table/vignettes/datatable-faq.html#only-the-first-10-rows-are-printed-how-do-i-print-more
                    options(datatable.print.topn = 2)
                    return(DF$General)
                } else return("[Shiny,General.Content] INFO, file General.Rdata does not exist")})
            # NavBar"DataTreatment", sidebar "Filtering" ----
            output$uiFiltering      <- renderUI({
                tabsetPanel(id = "Treatments",
                            tabPanel(title = "Sensors", icon = icon("thermometer"),
                                     do.call(tabsetPanel,
                                             c(id = 'Filtering.Sensors',
                                               lapply(list.name.sensor(),
                                                      function(i) {
                                                          tabPanel(
                                                              title = i,
                                                              div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                                                  checkboxInput(inputId = paste0("Apply.Warm", which(list.name.sensor() == i)) ,
                                                                                label = "Apply filter for warming of sensors"    ,
                                                                                value = FALSE)
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                                                  numericInput(inputId  = paste0("Warming", which(list.name.sensor() == i))    ,
                                                                               label = "Number of hours of warming"             ,
                                                                               min = 0 ,
                                                                               max = 1440, step = 1 ,
                                                                               value = Config$all[["sens2ref"]]$hoursWarming[which(Config$all[["sens2ref"]]$name.sensor == i)])
                                                              ),
                                                              hr(),
                                                              checkboxInput(inputId  = paste0("Apply.TRh", which(list.name.sensor() == i)),
                                                                            label = "Apply filter on Temp and Rel. Humidity" ,
                                                                            value = FALSE
                                                              ),
                                                              sliderInput(inputId   = paste0("Temperature", which(list.name.sensor() == i)) ,
                                                                          label = "Range of accepted temperature (Celsius degrees):" ,
                                                                          min = -20,
                                                                          max = 60 ,
                                                                          step = 0.5,
                                                                          value = c(Config$all[["sens2ref"]]$temp.thres.min[which(Config$all[["sens2ref"]]$name.sensor == i)],
                                                                                    Config$all[["sens2ref"]]$temp.thres.max[which(Config$all[["sens2ref"]]$name.sensor == i)])
                                                              ),
                                                              sliderInput(inputId   = paste0("Humidity", which(list.name.sensor() == i))   ,
                                                                          label = "Range of accepted relative humidity (%):",
                                                                          min = 0 ,
                                                                          max = 100 ,
                                                                          step = 0.5 ,
                                                                          value = c(Config$all[["sens2ref"]]$rh.thres.min[which(Config$all[["sens2ref"]]$name.sensor == i)] ,
                                                                                    Config$all[["sens2ref"]]$rh.thres.max[which(Config$all[["sens2ref"]]$name.sensor == i)])
                                                              ),
                                                              hr(),
                                                              div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                                                  checkboxInput(inputId = paste0("Sens.Inval.Out", which(list.name.sensor() == i)),
                                                                                label = "Discard Invalid data" ,
                                                                                value = Set$Time$Sens.Inval.Out[which(Set$Time$name.sensor == i)])
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                                                  checkboxInput(inputId = paste0("Apply.Invalid", which(list.name.sensor() == i)),
                                                                                label = "Apply validity periods",
                                                                                value = FALSE)
                                                                  # bsButton(
                                                                  #     inputId  = paste0("Apply.Invalid", which(list.name.sensor() == i)),
                                                                  #     label    = "Apply validity periods",
                                                                  #     style    = "default",
                                                                  #     size     = "default",
                                                                  #     type     = "toggle",
                                                                  #     block    = FALSE,
                                                                  #     disabled = FALSE,
                                                                  #     value    = FALSE)
                                                              ),
                                                              hr(),
                                                              div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                                                  checkboxInput(inputId = paste0("Sens.rm.Out", which(list.name.sensor() == i)),
                                                                                label = "Enable Outlier discarding"              ,
                                                                                value = Config$all[["sens2ref"]]$Sens.rm.Out[which(Config$all[["sens2ref"]]$name.sensor == i)]
                                                                  )
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                                                  checkboxInput(inputId = paste0("Apply.S.Out", which(list.name.sensor() == i)),
                                                                                label = "Apply parameters of filter"             ,
                                                                                value = FALSE)
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                                                  numericInput(inputId  = paste0("Sens.window", which(list.name.sensor() == i)),
                                                                               label = "Nb of data in rolling window"                      ,
                                                                               value = Config$all[["sens2ref"]]$Sens.window[which(Config$all[["sens2ref"]]$name.sensor == i)])),
                                                              div(style = "display: inline-block;vertical-align:top; width: 1%;",HTML("<br>")),
                                                              div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                                                  numericInput(inputId  = paste0("Sens.threshold", which(list.name.sensor() == i)),
                                                                               label = "Threshold for MAD"                               ,
                                                                               value = Config$all[["sens2ref"]]$Sens.threshold[which(Config$all[["sens2ref"]]$name.sensor == i)])
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                                                  numericInput(inputId  = paste0("Sens.ThresholdMin", which(list.name.sensor() == i)),
                                                                               label = "Mininimum Value median-mad"      ,
                                                                               value = Config$all[["sens2ref"]]$Sens.ThresholdMin[which(Config$all[["sens2ref"]]$name.sensor == i)])
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 1%;",HTML("<br>")),
                                                              div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                                                  numericInput(inputId  = paste0("Sens.iterations", which(list.name.sensor() == i)),
                                                                               label = "Nb. of iterations of the Median Average Deviation",
                                                                               value = Config$all[["sens2ref"]]$Sens.iterations[which(Config$all[["sens2ref"]]$name.sensor == i)])
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                                                  numericInput(inputId  = paste0("Sens.Ymin", which(list.name.sensor() == i)),
                                                                               label = "Minimum values in digital data series"           ,
                                                                               value = Config$all[["sens2ref"]]$Sens.Ymin[which(Config$all[["sens2ref"]]$name.sensor == i)])
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 1%;",HTML("<br>")),
                                                              div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                                                  numericInput(inputId  = paste0("Sens.Ymax", which(list.name.sensor() == i)),
                                                                               label = "Maximum values in digital data series"           ,
                                                                               value = Config$all[["sens2ref"]]$Sens.Ymax[which(Config$all[["sens2ref"]]$name.sensor == i)])
                                                              )
                                                          )
                                                      }
                                               )
                                             )
                                     )
                            ),
                            tabPanel(title =  "Reference", icon = icon("calculator"),
                                     do.call(tabsetPanel,
                                             c(id ='Filtering.References',
                                               lapply(list.gas.reference(),
                                                      function(i) {
                                                          tabPanel(
                                                              title = i,
                                                              div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                                                  bsButton(inputId = paste0("left2Out.Ref.Date", which(list.gas.reference() == i)),
                                                                           label = "<<",
                                                                           style = "default",
                                                                           size = "extra-small",
                                                                           type = "action",
                                                                           block = FALSE,
                                                                           disabled = FALSE,
                                                                           value = FALSE)
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                                                  bsButton(inputId = paste0("left1Out.Ref.Date", which(list.gas.reference() == i)),
                                                                           label = "<",
                                                                           style = "default",
                                                                           size = "extra-small",
                                                                           type = "action",
                                                                           block = FALSE,
                                                                           disabled = FALSE,
                                                                           value = FALSE)
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 77%;",
                                                                  dateRangeInput(inputId = paste0("Out.Ref.Date", which(list.gas.reference() == i)),
                                                                                 label   = "Range of dates for plotting outliers and negative values for this pollutant:",
                                                                                 format  = "yyyy-mm-dd",
                                                                                 start   = Set$Time[["Out.Ref.IN"]][which(Set$Time$name.sensor == Config$all[["sens2ref"]]$name.sensor[Config$all[["sens2ref"]]$gas.reference == i])],
                                                                                 end     = as.Date(Set$Time[["Out.Ref.END"]][which(Set$Time$name.sensor == Config$all[["sens2ref"]]$name.sensor[Config$all[["sens2ref"]]$gas.reference == i])]),
                                                                                 weekstart = 1,
                                                                                 min = as.Date(c("2015-01-01")),
                                                                                 max = Sys.Date())
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                                                  bsButton(inputId = paste0("right1Out.Ref.Date", which(list.gas.reference() == i)),
                                                                           label = ">",
                                                                           style = "default",
                                                                           size = "extra-small",
                                                                           type = "action",
                                                                           block = FALSE,
                                                                           disabled = FALSE,
                                                                           value = FALSE)
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                                                  bsButton(inputId = paste0("right2Out.Ref.Date", which(list.gas.reference() == i)),
                                                                           label = ">>",
                                                                           style = "default",
                                                                           size = "extra-small",
                                                                           type = "action",
                                                                           block = FALSE,
                                                                           disabled = FALSE,
                                                                           value = FALSE)
                                                              ),
                                                              checkboxInput(inputId = paste0("rm.neg", which(list.gas.reference() == i)),
                                                                            label = "Remove negative reference values"          ,
                                                                            value = Config$all[["sens2ref"]]$remove.neg[which(Config$all[["sens2ref"]]$gas.reference == i)]
                                                              ),
                                                              selectInput( inputId  = paste0("Ref.unit", which(list.gas.reference() == i)),
                                                                           label = "Unit of reference data",
                                                                           choices = choices.Ref.unit,
                                                                           selected = Config$all[["sens2ref"]]$ref.unitgas[which(Config$all[["sens2ref"]]$gas.reference == i)]
                                                              ),
                                                              hr(),
                                                              div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                                                  checkboxInput(inputId = paste0("Ref.rm.Out", which(list.gas.reference() == i)),
                                                                                label = "Enable Outlier discarding"                       ,
                                                                                value = Config$all[["sens2ref"]]$Ref.rm.Out[which(Config$all[["sens2ref"]]$gas.reference == i)])
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                                                  checkboxInput(inputId = paste0("Apply.R.Out", which(list.gas.reference() == i)),
                                                                                label = "Apply parameters of filter"                      ,
                                                                                value = FALSE)
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                                                  numericInput(inputId  = paste0("Ref.window", which(list.gas.reference() == i)),
                                                                               label = "Nb of data in rolling window"                      ,
                                                                               value = Config$all[["sens2ref"]]$Ref.window[which(Config$all[["sens2ref"]]$gas.reference == i)])
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 1%;",HTML("<br>")),
                                                              div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                                                  numericInput(inputId  = paste0("Ref.threshold", which(list.gas.reference() == i)),
                                                                               label = "Threshold for MAD"                               ,
                                                                               value = Config$all[["sens2ref"]]$Ref.threshold[which(Config$all[["sens2ref"]]$gas.reference == i)])
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                                                  numericInput(inputId  = paste0("Ref.ThresholdMin", which(list.gas.reference() == i)),
                                                                               label = "Mininimum Value median-mad",
                                                                               value = Config$all[["sens2ref"]]$Ref.ThresholdMin[i])
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 1%;",HTML("<br>")),
                                                              div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                                                  numericInput(inputId  = paste0("Ref.iterations", which(list.gas.reference() == i)),
                                                                               label = "Nb. of iterations of the Median Average Deviation",
                                                                               value = Config$all[["sens2ref"]]$Ref.iterations[which(Config$all[["sens2ref"]]$gas.reference == i)])
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                                                  numericInput(inputId  = paste0("Ref.Ymin", which(list.gas.reference() == i)),
                                                                               label = "Minimum values in digital data series"           ,
                                                                               value = Config$all[["sens2ref"]]$Ref.Ymin[which(Config$all[["sens2ref"]]$gas.reference == i)])
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 1%;",HTML("<br>")),
                                                              div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                                                  numericInput(inputId  = paste0("Ref.Ymax", which(list.gas.reference() == i)),
                                                                               label = "Maximum values in digital data series"           ,
                                                                               value = Config$all[["sens2ref"]]$Ref.Ymax[which(Config$all[["sens2ref"]]$gas.reference == i)])
                                                              )
                                                          )
                                                      }
                                               )
                                             )
                                     )
                            )
                )
            })
            # NavBar"DataTreatment", sideBar button "SelectSensors" ----
            output$uiSelectSensors <- renderUI({radioButtons(inputId = "SelSensors", label = "Sensor:", selected = list.name.sensor()[1],
                                                             c(list.name.sensor()[1],list.name.sensor()[2], list.name.sensor()[3], list.name.sensor()[4])
                                                             , inline = TRUE)})
            # NavBar"DataTreatment", sidebar "Calib." ----
            output$uiCalib         <- renderUI({
                do.call(tabsetPanel,
                        c(id = 'Calib.Sensors',
                          lapply(list.name.sensor(),
                                 function(i) {
                                     tabPanel(
                                         title = i,
                                         div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                             selectInput(  inputId  = paste0("Sens.raw.unit", which(list.name.sensor() == i)),
                                                           label    = "Raw unit of sensor data"                     ,
                                                           choices  = c("V","nA", "ppb", "ug.m-3", "ppm", "mg.m-3", "counts.mL-1", "counts.L-1"),
                                                           selected = Config$all[["sens2ref"]]$Sens.raw.unit[which(Config$all[["sens2ref"]]$name.sensor == i)])),
                                         div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                             selectInput(  inputId  = paste0("Sens.unit", which(list.name.sensor() == i)),
                                                           label = "Unit for calibrated sensor"              ,
                                                           choices = c("ppb","ug/m3","ppm","mg/m3", "counts.mL-1", "counts.L-1"),
                                                           selected = Config$all[["sens2ref"]]$Sens.unit[which(Config$all[["sens2ref"]]$name.sensor == i)])),
                                         checkboxInput(inputId = paste0("Apply.conv", which(list.name.sensor() == i)),
                                                       label   = "Force Conversion to V/nA ..."                ,
                                                       value   = FALSE),
                                         selectInput(  inputId  = paste0("Sens", which(list.name.sensor() == i)),
                                                       label    = "List of covariates to plot"         ,
                                                       choices  = Covariates(),
                                                       selected = as.vector(Config$all[["CovPlot"]][[i]]$Effects),
                                                       multiple = TRUE),
                                         div(style = "display: inline-block;vertical-align:top; width: 64%;",
                                             radioButtons( inputId  = paste0("Cal.Line", which(list.name.sensor() == i)),
                                                           label    = "Calibration or Prediction"                     ,
                                                           choices  = list("Calibration with current data","Prediction with previous calibration","Calibration with slope and intercept"),
                                                           selected = Config$all[["sens2ref"]]$Cal.Line[which(Config$all[["sens2ref"]]$name.sensor == i)])),
                                         div(style = "display: inline-block;vertical-align:top; width: 33%;",
                                             checkboxInput(inputId = paste0("Sync.Cal", which(list.name.sensor() == i)),
                                                           label   = "Sync. Cal."         ,
                                                           value   = Config$all[["sens2ref"]]$Sync.Cal[which(Config$all[["sens2ref"]]$name.sensor == i)]),
                                             checkboxInput(inputId = paste0("Sync.Pred", which(list.name.sensor() == i)),
                                                           label   = "Sync. Pred."         ,
                                                           value   = Config$all[["sens2ref"]]$Sync.Pred[which(Config$all[["sens2ref"]]$name.sensor == i)])),
                                         div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                             selectInput(  inputId  = paste0("Calibration", which(list.name.sensor() == i)),
                                                           label    = "Model for calibration"            ,
                                                           choices  = na.omit(Models) ,
                                                           selected = Config$all[["sens2ref"]]$mod.eta.model.type[which(Config$all[["sens2ref"]]$name.sensor == i)])),
                                         div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                             selectInput(  inputId  = paste0("CovMod", which(list.name.sensor() == i)),
                                                           label    = "Covariates for calibration"         ,
                                                           choices  = Covariates.Model()[-which(Covariates.Model()   == paste0(i,"_volt") |
                                                                                                    Covariates.Model() == paste0(list.gas.sensor()[i],"_modelled") )],
                                                           selected = as.vector(Config$all[["CovMod"]][[i]]$Effects),
                                                           multiple = TRUE)),
                                         div(style = "display: inline-block;vertical-align:top; width: 94%;",
                                             selectInput(  inputId  = paste0("Cal", which(list.name.sensor() == i)),
                                                           label    = "Select a previous calibration "              ,
                                                           choices  = substr(list.files(path = file.path(input$Selected,"Models"),
                                                                                        pattern = glob2rx(paste0(Config$all[["Server"]]$AirSensEur.name,"__",i,"__*"))),
                                                                             start = nchar(paste0(Config$all[["Server"]]$AirSensEur.name,"__",i,"__")) + 1,
                                                                             stop  = nchar(list.files(path = file.path(input$Selected,"Models"),
                                                                                                      pattern = glob2rx(paste0(Config$all[["Server"]]$AirSensEur.name,"__",i,"__*"))))),
                                                           selected = Config$all[["sens2ref"]]$Cal.func[which(Config$all[["sens2ref"]]$name.sensor == i)])),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId  =  paste0("DelModel", which(list.name.sensor() == i)),
                                                      label    = "Del",
                                                      style    = "default",
                                                      size     = "extra-small",
                                                      type     = "action",
                                                      block    = FALSE,
                                                      disabled = FALSE,
                                                      value    = FALSE)),
                                         div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                             numericInput(inputId = paste0("Slope", which(list.name.sensor() == i)),
                                                          label   = "Slope for linear calibration"                ,
                                                          value  = Config$all[["sens2ref"]]$Slope[which(Config$all[["sens2ref"]]$name.sensor == i)], step = 0.000001)),
                                         div(style = "display: inline-block;vertical-align:top; width: 1%;",HTML("<br>")),
                                         div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                             numericInput(inputId = paste0("Intercept", which(list.name.sensor() == i)),
                                                          label = "Intercept for calibration"            ,
                                                          value = Config$all[["sens2ref"]]$Intercept[which(Config$all[["sens2ref"]]$name.sensor == i)], step = 0.0001)),
                                         div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                             checkboxInput(inputId = paste0("Neg.mod", which(list.name.sensor() == i)),
                                                           label   = "Discard negative predicted data?",
                                                           value   = Config$all[["sens2ref"]]$Neg.mod[which(Config$all[["sens2ref"]]$name.sensor == i)])),
                                         div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                             checkboxInput(inputId = paste0("Apply.cal", which(list.name.sensor() == i)),
                                                           label   = "Force prediction with cal. model"                          ,
                                                           value   = FALSE)),
                                         br(),
                                         div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                             selectInput(  inputId  = paste0("ubsRM", which(list.name.sensor() == i)),
                                                           label = "u(bsRM), between reference data uncertainty"              ,
                                                           choices = c(seq(from = 0, to = 0.2, by = 0.01), seq(from = 0.2, to = 10, by = 0.1)),
                                                           selected = Config$all[["sens2ref"]]$ubsRM[which(Config$all[["sens2ref"]]$name.sensor == i)])),
                                         div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                             selectInput(  inputId  = paste0("ubss", which(list.name.sensor() == i)),
                                                           label = "u(bs), between sensor uncertainty "              ,
                                                           choices = c(seq(from = 0, to = 0.2, by = 0.01), seq(from = 0.2, to = 10, by = 0.1)),
                                                           selected = Config$all[["sens2ref"]]$ubss[which(Config$all[["sens2ref"]]$name.sensor == i)])),
                                         div(style = "display: inline-block;vertical-align:top; width: 100%;",
                                             selectInput(  inputId  = paste0("Comparison", which(list.name.sensor() == i)),
                                                           label = "Model chosen for comparison with reference data"  ,
                                                           choices = "Linear",
                                                           selected = Config$all[["sens2ref"]]$eta.model.type[which(Config$all[["sens2ref"]]$name.sensor == i)]))
                                     )
                                 }
                          )
                        )
                )
            })
            # NavBar"DataTreatment", sidebar "SetTime" ----
            output$uiSetTime       <- renderUI({
                do.call(tabsetPanel,
                        c(id = 'SetTime.Sensors',
                          lapply(list.name.sensor(),
                                 function(i) {
                                     tabPanel(
                                         title = i,
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("left2Out.Valid", which(list.name.sensor() == i)), label = "<<", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                             bsButton(inputId = paste0("left1Out.Valid", which(list.name.sensor() == i)), label = "<", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 77%;",
                                             dateRangeInput(inputId = paste0("Valid", which(list.name.sensor() == i)),
                                                            label   = "Range of valid dates (invalid are hidden):",
                                                            format  = "yyyy-mm-dd",
                                                            start   = Set$Time[["Valid.IN"]][which(Set$Time$name.sensor == i)],
                                                            end     = as.Date(Set$Time[["Valid.END"]][which(Set$Time$name.sensor == i)]),
                                                            weekstart = 1,
                                                            min = Set$Time[["Valid.IN"]][which(Set$Time$name.sensor == i)],
                                                            max = Set$Time[["Valid.END"]][which(Set$Time$name.sensor == i)]
                                             )
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                             bsButton(inputId = paste0("right1Out.Valid", which(list.name.sensor() == i)), label = ">", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("right2Out.Valid", which(list.name.sensor() == i)), label = ">>", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         br(),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("left2Out.Sens.Date", which(list.name.sensor() == i)), label = "<<", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                             bsButton(inputId = paste0("left1Out.Sens.Date", which(list.name.sensor() == i)), label = "<", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 77%;",
                                             dateRangeInput(inputId = paste0("Out.Sens.Date", which(list.name.sensor() == i)),
                                                            label   = "Range of dates for plotting outliers:",                             #### Need the message for the i = 1, "Range of dates for plotting RawData, DataTable, Retrieved, Warming, Temp&Humid, Invalids and outliers:"
                                                            format  = "yyyy-mm-dd",
                                                            start   = Set$Time$Out.Sens.IN[which(Set$Time$name.sensor == i)],
                                                            end     = as.Date(Set$Time$Out.Sens.END[which(Set$Time$name.sensor == i)]),
                                                            weekstart = 1,
                                                            min = Set$Time[["Valid.IN"]][which(Set$Time$name.sensor == i)],
                                                            max = Set$Time[["Valid.END"]][which(Set$Time$name.sensor == i)]
                                             )
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                             bsButton(inputId = paste0("right1Out.Sens.Date", which(list.name.sensor() == i)), label = ">", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("right2Out.Sens.Date", which(list.name.sensor() == i)), label = ">>", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("left2Date", which(list.name.sensor() == i)), label = "<<", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                             bsButton(inputId = paste0("left1Date", which(list.name.sensor() == i)), label = "<", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 71%;",
                                             dateRangeInput(inputId = paste0("Date", which(list.name.sensor() == i)),
                                                            label   = "Range of dates for plotting covariates:",
                                                            format  = "yyyy-mm-dd",
                                                            start   = Set$Time$Cov.Date.IN[which(Set$Time$name.sensor == i)],
                                                            end     = as.Date(Set$Time$Cov.Date.END[which(Set$Time$name.sensor == i)]),
                                                            weekstart = 1,
                                                            min     = Set$Time$Valid.IN[which(Set$Time$name.sensor == i)],
                                                            max     = Set$Time$Valid.END[which(Set$Time$name.sensor == i)]
                                             )
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 6%;",
                                             bsButton(inputId = paste0("DateCOVCal", which(list.name.sensor() == i)), label = "Cal", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                             bsButton(inputId = paste0("right1Date", which(list.name.sensor() == i)), label = ">", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("right2Date", which(list.name.sensor() == i)), label = ">>", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         hr(),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("left2DateCal", which(list.name.sensor() == i)), label = "<<", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                             bsButton(inputId = paste0("left1DateCal", which(list.name.sensor() == i)), label = "<", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 57%;",
                                             dateRangeInput(inputId = paste0("DateCal", which(list.name.sensor() == i)),
                                                            label   = "Range of dates for calibration:",
                                                            format  = "yyyy-mm-dd",
                                                            start   = Set$Time$DateCal.IN[which(Set$Time$name.sensor == i)],
                                                            end     = as.Date(Set$Time$DateCal.END[which(Set$Time$name.sensor == i)]),
                                                            weekstart = 1,
                                                            min = Set$Time$Valid.IN[which(Set$Time$name.sensor == i)],
                                                            max = Set$Time$Valid.END[which(Set$Time$name.sensor == i)]
                                             )
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 6%;",
                                             bsButton(inputId = paste0("DateCALCal", which(list.name.sensor() == i)), label = "Cal", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 6%;",
                                             bsButton(inputId = paste0("DateCALCovCal", which(list.name.sensor() == i)), label = "Cov", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 6%;",
                                             bsButton(inputId = paste0("DateCALExtCal", which(list.name.sensor() == i)), label = "Ext", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                             bsButton(inputId = paste0("right1DateCal", which(list.name.sensor() == i)), label = ">", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("right2DateCal", which(list.name.sensor() == i)), label = ">>", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("left2DatePlotCal", which(list.name.sensor() == i)), label = "<<", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                             bsButton(inputId = paste0("left1DatePlotCal", which(list.name.sensor() == i)), label = "<", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 57%;",
                                             dateRangeInput(inputId = paste0("DatePlotCal", which(list.name.sensor() == i)),
                                                            label   = "Range of dates for plotting calibration:",
                                                            format  = "yyyy-mm-dd",
                                                            start   = Set$Time$DatePlotCal.IN[which(Set$Time$name.sensor == i)],
                                                            end     = as.Date(Set$Time$DatePlotCal.END[which(Set$Time$name.sensor == i)]),
                                                            weekstart = 1,
                                                            min = Set$Time$Valid.IN[which(Set$Time$name.sensor == i)],
                                                            max = Set$Time$Valid.END[which(Set$Time$name.sensor == i)]
                                             )
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 24%;",
                                             bsButton(inputId = paste0("right1DatePlotCal", which(list.name.sensor() == i)), label = ">", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("right2DatePlotCal", which(list.name.sensor() == i)), label = ">>", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         hr(),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("left2DateMeas", which(list.name.sensor() == i)), label = "<<", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                             bsButton(inputId = paste0("left1DateMeas", which(list.name.sensor() == i)), label = "<", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 77%;",
                                             dateRangeInput(inputId = paste0("DateMeas", which(list.name.sensor() == i)),
                                                            label   = "Range of dates for Prediction:",
                                                            format  = "yyyy-mm-dd",
                                                            start   = Set$Time$DateMeas.IN[which(Set$Time$name.sensor == i)],
                                                            end     = as.Date(Set$Time$DateMeas.END[which(Set$Time$name.sensor == i)]),
                                                            weekstart = 1,
                                                            min = Set$Time$Valid.IN[which(Set$Time$name.sensor == i)],
                                                            max = Set$Time$Valid.END[which(Set$Time$name.sensor == i)]
                                             )
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                             bsButton(inputId = paste0("right1DateMeas", which(list.name.sensor() == i)), label = ">", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("right2DateMeas", which(list.name.sensor() == i)), label = ">>", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("left2DatePlotMeas", which(list.name.sensor() == i)), label = "<<", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                             bsButton(inputId = paste0("left1DatePlotMeas", which(list.name.sensor() == i)), label = "<", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 77%;",
                                             dateRangeInput(inputId = paste0("DatePlotMeas", which(list.name.sensor() == i)),
                                                            label   = "Range of dates for plotting predicted data:",
                                                            format  = "yyyy-mm-dd",
                                                            start   = Set$Time$DatePlotMeas.IN[which(Set$Time$name.sensor == i)],
                                                            end     = as.Date(Set$Time$DatePlotMeas.END[which(Set$Time$name.sensor == i)]),
                                                            weekstart = 1,
                                                            min = Set$Time$Valid.IN[which(Set$Time$name.sensor == i)],
                                                            max = Set$Time$Valid.END[which(Set$Time$name.sensor == i)]
                                             )
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                             bsButton(inputId = paste0("right1DatePlotMeas", which(list.name.sensor() == i)), label = ">", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("right2DatePlotMeas", which(list.name.sensor() == i)), label = ">>", style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         )
                                     )
                                 }
                          )
                        )
                )
            })
            # Be sure to open "Filtering" once
            updateTabsetPanel(session, inputId = "Calib_data", selected = "Filtering")
            # Goto GetData in NavBar
            updateNavbarPage(session, inputId = "ASE", selected = "GetData")
            # Opening at least once the TabPanel of sideBarLayout Getdata to update all parameters for download
            updateTabsetPanel(session, inputId = "ForServers", selected = "tPRef")
            progress$set(message = "Selecting AirSensEUR Box", value = 1)}
    }, ignoreNULL = TRUE, ignoreInit = FALSE)
    # Reactive Shield(), NavBar"SelectASE" ----
    Shield                   <- eventReactive(input$asc.File,{
        if (!is.null(input$asc.File) & length(input$asc.File) != 0) {
            ASEPanel04Read(ASEPanel04File = file.path(DirShiny, "Shield_Files", input$asc.File))
        } else return("[Shiny, Shield()] ERROR, Config file of chemical shield not existing.\n")
    })
    # NavBar"SelectASE", mainTabPanel "Push data" ----
    output$Pushdata.cfg      <- renderTable(Pushdata_cfg())
    Pushdata_cfg             <- reactive({
        # to show when selecting AirSensEUr in1st screen
        # Make it reactive to Save
        input$Save
        # Create a Progress object
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        progress$set(message = "Reading Server File", value = 0.5)
        if (file.exists(cfg_file.List())) {
            Servers_file <- fread(file = Servers_file.List(), header = TRUE)
            # remove all password not to be displayed
            Servers_file <- Servers_file[-which(Servers_file$PROXY %in% c("PASSWORD", "Pass", "Pass__a_i_p__"))]
            Servers_file[na.omit(match(c("UserMins","UserMinsAvg","Delay", "PROXY","URL","PORT","LOGIN",
                                         "Down.Influx","Host","Port","User","Db","Dataset","Influx.TZ",
                                         "Down.SOS","AirsensWeb","AirSensEur.name","SOS.TZ",
                                         "Down.Ref","FTPMode","urlref","Reference.name",
                                         "RefSOSname","Ref.SOS.name","RefPollutants","RefDateDownload",
                                         "Ref__a_i_p__name", "User__a_i_p__",
                                         "Ref__a_i_p__Organisation", "Ref__a_i_p__Station", "Ref__a_i_p__Pollutants", "Ref__a_i_p__Date",
                                         "coord.ref","alt.ref","ref.tzone",
                                         "asc.File") , Servers_file[[1]]))]
        }
        progress$set(message = "Reading Server File", value = 1)
        return(Servers_file)
    })
    # NavBar"SelectASE", mainTabPanel "Filtering" "Calibration" ----
    output$Calib.cfg         <- renderTable(Read_cfg()[[1]],
                                            rownames = TRUE,
                                            digits = -4)
    output$FilteringSensor   <- renderTable(Read_cfg()[[2]],
                                            rownames = TRUE,
                                            digits = 1)
    output$FilteringRef      <- renderTable(Read_cfg()[[3]],
                                            rownames = TRUE,
                                            digits = 1)
    # Reactive cfgValues
    Read_cfg          <- reactive({
        # Make it reactive to Save
        input$Save
        # Create a Progress object
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        progress$set(message = "Reading Config File", value = 0.5)
        if (file.exists(cfg_file.List())) {
            cfg_file <- data.table::fread(file = cfg_file.List(), na.strings = c("NA","NaN"), header = TRUE)
            G <- list(cfg_file[which(name.gas %in% c("name.sensor", "Sens.raw.unit", "Sens.unit", "Cal.Line", "Sync.Cal", "Sync.Pred", "Cal.func",
                                                     "mod.eta.model.type", "Slope", "Intercept", "Neg.mod", "eta.model.type"))],
                      cfg_file[which(name.gas  %in% c("name.sensor","hoursWarming", "temp.thres.min", "temp.thres.max", "rh.thres.min", "rh.thres.max",
                                                      "Sens.Inval.Out","Sens.rm.Out", "Sens.window", "Sens.threshold", "Sens.Ymin", "Sens.Ymax",
                                                      "Sens.ThresholdMin", "Sens.iterations"))],
                      cfg_file[which(name.gas  %in% c("remove.neg", "ref.unitgas", "ubsRM", "ubss","Ref.rm.Out", "Ref.window", "Ref.threshold",
                                                      "Ref.Ymin", "Ref.Ymax", "Ref.ThresholdMin", "Ref.iterations", "gas.reference", "gas.reference2use"))])
        }
        progress$set(message = "Reading Config File", value = 1)
        return(G)
    })
    # NavBar"SelectASE", mainTabPanel "SetTime" ----
    output$SetTime.cfg       <- renderTable(SetTime_cfg(), rownames = TRUE)
    SetTime_cfg              <- reactive({
        # Create a Progress object
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        progress$set(message = "Reading Date/Time File", value = 0.5)
        if (file.exists(SETTIME_file.List())) {
            SETTIME_file        <- data.table::fread(file = SETTIME_file.List(), na.strings = c("NA","NaN"), header = TRUE)
        } else SETTIME_file <- NULL
        progress$set(message = "Reading Date/Time File", value = 1)
        return(SETTIME_file)
    })
    output$Sensors.cfg        <- renderTable(Sensors.cfg(), rownames = TRUE)
    output$Select.Sensors.cfg <- renderTable(Select.Sensors.cfg(), rownames = TRUE)
    Sensors.cfg               <- reactive({
        cfg.path <- ifelse(Configuration.TRUE(),file.path(input$Config_Files,"Configuration"),file.path(input$Config_Files,"General_data")) 
        if (file.exists(file.path(cfg.path,"Sensors.cfg"))) {
            Sensors.cfg       <- data.table::fread(file = file.path(cfg.path,"Sensors.cfg"), na.strings = c("NA","NaN"), header = TRUE)
        } else {
            Sensors.cfg <- "No data available about AirSensEUR Sensors at the Influx server. The AirSensEUR version is either too old or no data have been downloaded yet."  
        } 
        return(Sensors.cfg)
    })
    Select.Sensors.cfg        <- reactive({
        cfg.path <- ifelse(Configuration.TRUE(),file.path(input$Selected,"Configuration"),file.path(input$Selected,"General_data")) 
        if (file.exists(file.path(cfg.path,"Sensors.cfg"))) {
            Sensors.cfg       <- data.table::fread(file = file.path(cfg.path,"Sensors.cfg"), na.strings = c("NA","NaN"), header = TRUE)
        } else {
            Sensors.cfg <- "No data available about AirSensEUR Sensors at the Influx server. The AirSensEUR version is either too old or no data have been downloaded yet."
        } 
        return(Sensors.cfg)
    })
    output$Boards.cfg        <- renderTable(Boards.cfg(), rownames = TRUE)
    output$Select.Boards.cfg <- renderTable(Select.Boards.cfg(), rownames = TRUE)
    Boards.cfg               <- reactive({
        cfg.path <- ifelse(Configuration.TRUE(),file.path(input$Config_Files,"Configuration"),file.path(input$Config_Files,"General_data")) 
        if (file.exists(file.path(cfg.path,"Boards.cfg"))) {
            Boards.cfg       <- data.table::fread(file = file.path(cfg.path,"Boards.cfg"), na.strings = c("NA","NaN"), header = TRUE)
        } else Boards.cfg <- "No data available about AirSensEUR Boards at the Influx server. The AirSensEUR version is likely too old or no data have been downloaded yet."
        return(Boards.cfg)
    })
    Select.Boards.cfg        <- reactive({
        cfg.path <- ifelse(Configuration.TRUE(),file.path(input$Selected,"Configuration"),file.path(input$Selected,"General_data")) 
        if (file.exists(file.path(cfg.path,"Boards.cfg"))) {
            Boards.cfg       <- data.table::fread(file = file.path(cfg.path,"Boards.cfg"), na.strings = c("NA","NaN"), header = TRUE)
        } else Boards.cfg <- "No data available about AirSensEUR Boards at the Influx server. The AirSensEUR version is likely too old or no data have been downloaded yet."
        return(Boards.cfg)
    })
    #  Observer asc.File of NavBar "Getdata", sideBarLayout: TimeShield - Proxy - InfluxDB - SOS -Refer.
    #  TimeShield
    observeEvent(input$asc.File,
        {
            # update the name of asc.File in ASE_name.cfg in order to update the name of sensors
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            progress$set(message = "Reading schield Config File", value = 0.5)
            # Reading the _Servers.cfg file or using the one existing in Config$all
            if (!"Server" %in% names(Config$all)) {
                if (file.exists(Servers_file())) {
                    Config$all[["Server"]] <<- data.table::transpose(fread(file = Servers_file(), header = FALSE), fill = NA, make.names = 1)
                    cat(paste0("[shiny, asc.File] Info, the config file ", Servers_file(), " for the configuration of servers  is loaded"), sep = "\n")
                } else stop(cat(paste0("[shiny, asc.File] The file of server configuration for AirSensEUR: ", Servers_file,
                                       " does not exist. Please select another AirSensEUR of manually create the file"), sep = "\n"))
            }
            # Update asc.File
            if (input$asc.File != Config$all[["Server"]]$asc.File) {
                Delete.General <- function(value) {
                    if (value == TRUE) {
                        set(Config$all[["Server"]], j = "asc.File", value = input$asc.File)
                        # save modified file
                        fwrite(data.table::transpose(Config$all[["Server"]], fill = NA, make.names = 1, keep.names = "PROXY"), file = Servers_file(), row.names = FALSE)
                        cat(paste0("[shiny, asc.File] INFO, ", paste0(ASE_name(),"_Servers.cfg")," config file  saved in directory General_data.\n"))
                        # now delete the existing General.Rdata and redo all data treatment
                        if (file.exists(General.file())) unlink(General.file(), recursive = FALSE, force = TRUE)
                        RDS.files <- list.files(path = file.path(DisqueFieldtestDir.List(), "General_data"), pattern = "\\.RDS$")
                        RDS.files <- RDS.files[RDS.files %in% c("ind_Invalid.RDS","ind_ref_out.RDS","ind_sens_out.RDS", "ind_TRh.RDS","ind_warm.RDS" )]
                        if (!identical(RDS.files, character(0))) file.remove(file.path(input$Selected, "General_data", RDS.files))
                        # Remove calibration and modelled gas
                        # Quit to restart the UI and redo the whole data treatment
                        shinyjs::click(id = "Quit")
                    }
                }
                shinyalert(title = "Warning, deleting data",
                           text  = "Do you really want to delete General data all data and data treatment?\n
                                    The App will exit. Please restart.\n ",
                           confirmButtonText   = "Yes",
                           showCancelButton    = TRUE,
                           cancelButtonText    = "No",
                           callbackR           = Delete.General,
                           closeOnEsc          = FALSE,
                           closeOnClickOutside = FALSE,
                           html                = FALSE,
                           type                = "warning",
                           timer               = 0,
                           imageUrl            = "",
                           animation           = FALSE)
                # update the choice of sensor names for the radiotbutton selecting sensors
                # updateRadioButtons(session, "Sensors",
                #                    label   = "Sensors",
                #                    choices = list.name.sensor(),
                #                    inline  = TRUE)
            } else {
                message <- paste0("[shiny, asc.File] INFO, \"Sensor shield config file *.asc\" is already in the config file.
                           No further data treament is needed.\n")
                shinyalert(
                    title = "INFO",
                    text = message,
                    closeOnEsc = TRUE,
                    closeOnClickOutside = TRUE,
                    html = FALSE,
                    type = "info",
                    showConfirmButton = FALSE,
                    showCancelButton = FALSE,
                    timer = 2000,
                    imageUrl = "",
                    animation = FALSE)
            }
            progress$set(message = "Reading schield Config File", value = 1)
        }, ignoreNULL = TRUE, ignoreInit = TRUE)
    # Reactive InfluxDB ----
    INFLUX <- eventReactive({
        # making the function reactive to Shield() and to action buttons "Download Influx data", "merge" and "Selected" of a new AirSensEUR
        input$asc.file
        input$Down_Influx
        input$Merge
        input$Selected
    },{
        # return(INFLUX)
        # var.names.meteo     <- INFLUX[[2]]
        # var.name.GasSensors <- INFLUX[[3]]
        # var.names.sens      <- INFLUX[[4]]
        # InfluxData          <- INFLUX[[1]]
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        progress$set(message = "[shiny, INFLUX()] INFO, Loading INFLUX dataframe", value = 0.5)
        if ((!is.null(input$Down_Influx) && input$Down_Influx > 0) ||
            (!is.null(input$Merge)       && input$Merge > 0)) { # avoid computing INFLUX before input$Merge is clicked
            if (!is.null(Influx$DATA)) InfluxData <- Influx$DATA[] else InfluxData <- NA_real_
            A <- INFLUXDB(
                WDoutput        = file.path(input$Selected,"General_data"),
                DownloadSensor  = Download$Sensor,
                UserMins        = as.numeric(input$UserMins),
                PROXY           = input$PROXY,
                URL             = input$URL,
                PORT            = input$PORT,
                LOGIN           = input$LOGIN,
                PASSWORD        = input$PASSWORD,
                Down.Influx     = input$Down.Influx,
                Host            = input$Host,
                Port            = input$Port,
                User            = input$User,
                Pass            = input$Pass,
                Db              = input$Db,
                Dataset         = input$Dataset,
                Influx.TZ       = input$Influx.TZ,
                name.SQLite     = file.path(input$Selected,"General_data","airsenseur.db"),
                name.SQLite.old = file.path(input$Selected,"General_data","airsenseur_old.db"),
                sens2ref        = Config$all[["sens2ref"]],
                asc.File        = Shield(),
                InfluxData      = InfluxData)
        }  else {
            if (!is.null(Influx$DATA)) InfluxData <- Influx$DATA[] else InfluxData <- NA_real_
            A <- INFLUXDB(
                WDoutput        = file.path(input$Selected,"General_data"),
                DownloadSensor  = Download$Sensor,
                UserMins        = as.numeric(input$UserMins),
                PROXY           = input$PROXY,
                URL             = input$URL,
                PORT            = input$PORT,
                LOGIN           = input$LOGIN,
                PASSWORD        = input$PASSWORD,
                Down.Influx     = input$Down.Influx,
                Host            = input$Host,
                Port            = input$Port,
                User            = input$User,
                Pass            = input$Pass,
                Db              = input$Db,
                Dataset         = input$Dataset,
                name.SQLite     = file.path(input$Selected,"General_data","airsenseur.db"),
                name.SQLite.old = file.path(input$Selected,"General_data","airsenseur_old.db"),
                sens2ref        = Config$all[["sens2ref"]],
                InfluxData      = InfluxData)}
        
        progress$set(message = "[shiny, INFLUX()] INFO, Checking/Downloading Connection to INFLUX", value = 1)
        # updating Input$DATA
        if (!is.null(A[[1]])) {
            Influx$DATA <<- A[[1]]
            Download$Sensor <<- DownloadSensor()
        } 
        return(A)
    })
    observeEvent(input$Down_Influx, {
        # dependent on function INFLUX()
        str(INFLUX())
        updateTabsetPanel(session, "ForServers", selected = "tPRef")
    })
    # Reactive SOSData ----
    SOSDATA <- eventReactive({
        # making the function reactive to action button "Download SOS data", merging of sensor and reference data and selecting new ASE box
        input$Down_SOS
        input$Merge
        input$Selected
    },{
        # Create a Progress object
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        progress$set(message = "[shiny, SOSDATA()] INFO, setting SOS dataframe", value = 0.5)
        if ((!is.null(input$Down_SOS) && input$Down_SOS > 0) ||
            (!is.null(input$Merge)    && input$Merge > 0)) {
            # avoid computing SOSDATA before input$Merge is clicked
            B <- SOS(WDoutput            = file.path(input$Selected, "General_data"),
                     DownloadSensor      = Download$Sensor,
                     Down.SOS            = input$Down.SOS,
                     AirSensEur.name     = input$AirSensEur.name,
                     UserMins            = as.numeric(input$UserMins),
                     AirsensWeb          = input$AirsensWeb,
                     Duration            = 1,
                     sens2ref            = Config$all[["sens2ref"]])
        } else {
            B <- SOS(WDoutput            = file.path(input$Selected, "General_data"),
                     DownloadSensor      = data.frame(Retrieve.data.SOS = FALSE,
                                                      SOS.file          = SOSData.file(),
                                                      stringsAsFactors  = FALSE),
                     Down.SOS            = input$Down.SOS,
                     AirSensEur.name     = input$AirSensEur.name,
                     UserMins            = as.numeric(input$UserMins),
                     AirsensWeb          = input$AirsensWeb,
                     Duration            = 1,
                     sens2ref            = Config$all[["sens2ref"]])
        }
        progress$set(message = "[shiny, SOSDATA()] INFO, Checking/Downloading Connection to SOS", value = 1)
        # updating Sos$DATA
        if (!is.null(B[[1]])) {
            Sos$DATA <<- B[[1]]
            Download$Sensor <<- DownloadSensor()
        } 
        return(B)
        # SOSData             <- SOSDATA()[[1]]
        # var.names.meteo     <- SOSDATA()[[2]]
        # var.name.GasSensors <- SOSDATA()[[3]]
        # var.names.sens      <- SOSDATA()[[4]]
    })
    observeEvent(input$Down_SOS, {
        str(SOSDATA())
        updateTabsetPanel(session, "ForServers", selected = "tpRef")
    })
    # Reactive REFDATA ----
    REFDATA <- eventReactive({
        # making the function reactive to action button "Download Reference data" and "Merge" and select AirSensEUR
        input$Down_Ref
        input$Merge
        input$Selected
    },{
        # RefData   in REFDATA()[[3]]
        # variables in REFDATA()[[2]]
        if ((!is.null(input$Down_Ref) && input$Down_Ref > 0) ||
            (!is.null(input$Merge)    && input$Merge > 0)) { # avoid computing REFDATA before input$Merge is clicked
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when exiting this reactive function, even if there's an error
            on.exit(progress$close())
            progress$set(message = "[shiny, REFDATA()] INFO, Updating or Loading Reference data", value = 0.5)
            # Initial coordinates of reference station, project if coordinates are in DMS , update text inputs
            coord.ref <- paste0(input$coord.ref.Long,",",input$coord.ref.Lat)
            if (input$coord.ref.Long != "" || input$coord.ref.Lat != "") coord.ref <- get_Coord.Ref(coord.ref,
                                                                                                    ShinyUpdate = TRUE,
                                                                                                    session = sesion,
                                                                                                    ID.Long = "coord.ref.Long",
                                                                                                    ID.Lat  = "coord.ref.Lat")
            r <- reactiveValues(coord.ref = coord.ref)
            if (r$coord.ref == "," ||
                unlist(strsplit(r$coord.ref, split = ","))[1] == "" ||
                unlist(strsplit(r$coord.ref, split = ","))[2] == "") {
                showModal(
                    modalDialog(
                        title  = "Incorrect coordinates of the reference station, last chance",
                        footer = tagList(
                            modalButton("Cancel"),
                            actionButton("ok", "OK")
                        ),
                        size   = "l",
                        textInput(inputId     = "New_coordinates",
                                  label       = "New coordinates: ",
                                  width	      = "100%",
                                  placeholder = "enter long. and lat. in decimal degrees or d\'m\'s.s\'E,d\'m\'s.s\'N \n")))
                observeEvent(input$ok, {
                    if (!is.null(input$New_coordinates) && nzchar(input$New_coordinates) &&
                        exists(input$New_coordinates) && is.data.frame(get(input$New_coordinates))) {
                        r$coord.ref <- get(input$New_coordinates)
                        coord.ref <- get_Coord.Ref(coord.ref, ShinyUpdate = TRUE, session = sesion, ID.Long = "coord.ref.Long", ID.Lat = "coord.ref.Lat")
                    }
                })
            }
            # Checking if there are several ftp url
            if (any(grepl(pattern = ",", x = input$urlref))) urlref = unlist(strsplit(gsub(pattern = " ","",x = input$urlref), split = ",")  ) else urlref = gsub(pattern = " ","",x = input$urlref)
            # Setting REFDATA
            if (!is.null(Ref$DATA)) RefData <- Ref$DATA[] else RefData <- NA_real_
            C <- REF(DownloadSensor     = Download$Sensor,
                     AirSensEur.name    = input$AirSensEur.name,
                     DisqueFieldtestDir = (input$Selected),
                     UserMins           = as.numeric(input$UserMins),
                     Down.Ref           = input$Down.Ref,
                     ref.tzone          = input$ref.tzone,
                     InfluxData         = Influx$DATA,
                     SOSData            = Sos$DATA,
                     Reference.name     = input$Reference.name,
                     urlref             = urlref,
                     sens2ref           = Config$all[["sens2ref"]],
                     FTPMode            = input$FTPMode,
                     Ref.SOS.name       = input$Ref.SOS.name,
                     RefSOSname         = input$RefSOSname,
                     RefSOSDateIN       = as.Date(input$RefDateDownload[1], format = "%Y-%m-%d"),
                     RefSOSDateEND      = as.Date(input$RefDateDownload[2], format = "%Y-%m-%d") + 1,
                     Ref__a_i_p__name         = input$Ref__a_i_p__name,
                     User__a_i_p__            = input$User__a_i_p__,
                     Pass__a_i_p__            = input$Pass__a_i_p__,
                     Ref__a_i_p__Organisation = input$Ref__a_i_p__Organisation,
                     Ref__a_i_p__Station      = input$Ref__a_i_p__Station,
                     Ref__a_i_p__Pollutants   = input$Ref__a_i_p__Pollutants,
                     Ref__a_i_p__DateIN       = as.Date(input$Ref__a_i_p__Date[1], format = "%Y-%m-%d"),
                     Ref__a_i_p__DateEND      = as.Date(input$Ref__a_i_p__Date[2], format = "%Y-%m-%d"),
                     csvFile            = input$file1,
                     csvFile.sep        = input$sep,
                     csvFile.quote      = input$quote,
                     coord.ref          = coord.ref,
                     Ref.Type           = input$Ref.Type,
                     RefData            = RefData)
        } else {
            # loading existing RefData
            if (!is.null(Ref$DATA)) RefData <- Ref$DATA[] else RefData <- NA_real_
            C <- REF(DownloadSensor     = Download$Sensor,
                     AirSensEur.name    = input$AirSensEur.name,
                     DisqueFieldtestDir = input$Selected,
                     UserMins           = NULL,
                     Down.Ref           = FALSE,
                     ref.tzone          = NULL,
                     InfluxData         = Influx$DATA,
                     SOSData            = Sos$DATA,
                     Reference.name     = NULL,
                     urlref             = NULL,
                     sens2ref           = NULL,
                     FTPMode            = NULL,
                     RefData            = RefData)
        }
        # updating Ref$DATA
        if (!is.null(C[[1]])) {
            Ref$DATA <<- C[[1]]
            Download$Sensor <<- DownloadSensor()
        } 
        return(C)
    })
    observeEvent(input$Down_Ref, {
        str(REFDATA()) # should it be Ref$DATA?
        # Goto GetData in NavBar
        if (input$ASE != "DataTreatment") updateNavbarPage(session, "ASE", selected = "DataTreatment")
    })
    # NavBar"Data Treatment", SideBar Button "Merge" ----
    # The "Merge" button is not enable if sideBar tabPanel "Calib" and "SetTime" are not opened
    # https://cran.r-project.org/web/packages/shinyjs/vignettes/shinyjs-example.html
    observe({
        if (!is.null(input[[paste0("Sens", 1)]]) && !is.null(input[[paste0("Valid", 1)]]) ) {
            shinyjs::enable("Merge")
            if (input$Merge == 0) shinyjs::click(id = "Merge")
        } else {
            shinyjs::disable("Merge")
        }})
    # The "Save" button is not enable if sideBar tabPanel "Calib" and "SetTime" are not opened  and the button "Merge" is not clicked to create the Genernal dataFrame
    # https://cran.r-project.org/web/packages/shinyjs/vignettes/shinyjs-example.html
    observe({
        if ( !is.null(input$Sens1) && !is.null(input$Valid1) & input$Merge > 0 ) {
            shinyjs::enable("Save")
        } else {
            shinyjs::disable("Save")
        }
    })
    # Checking that all Tabpanel of "getdata" are opened once
    observe({
        if (input$ASE ==  "GetData" ) {
            if (is.null(input$PROXY))                              updateTabsetPanel(session, inputId = "ForServers"  , selected = "tPProxy")
            if (is.null(input$SOS.TZ) || is.null(input$Influx.TZ)) updateTabsetPanel(session, inputId = "ForServers"  , selected = "tPSensordown")
            if (is.null(input$UserMins))                           updateTabsetPanel(session, inputId = "ForServers"  , selected = "tPTimeshield")
            if (is.null(input$Down.Ref))                           updateTabsetPanel(session, inputId = "ForServers"  , selected = "tPRef")
        }
    })
    observe({
        if (input$ForServers ==  "tPRef" ) {
            if (is.null(input$file1))            updateTabsetPanel(session, inputId = "DownloadMode", selected = "csv")
            if (is.null(input$RefSOSname))       updateTabsetPanel(session, inputId = "DownloadMode", selected = "SOS")
            if (is.null(input$Ref__a_i_p__name)) updateTabsetPanel(session, inputId = "DownloadMode", selected = "a_i_p")
        }
    })
    observe({
        if (input$ForServers ==  "tPSensordown" ) {
            if (is.null(input$Host))       updateTabsetPanel(session, inputId = "SensorDown"  , selected = "tPInflux")
            if (is.null(input$Down.SOS))   updateTabsetPanel(session, inputId = "SensorDown"  , selected = "tPSOS")
        }
    })
    # Checking that all Tabpanel of "Data Treatment" are opened once
    observe({
        if (input$ASE ==  "Data Treatment") {
            if (is.null(input[[paste0("Sens", 1)]])) updateTabsetPanel(session, inputId = "Calib_data", selected = "Calib") else {
                if (is.null(input[[paste0("left2Out.Sens.Date", 1)]])) updateTabsetPanel(session, inputId = "Calib_data", selected = "SetTime")}}})
    observeEvent(input$Merge,{
        # Allow to change Model only in the Calibration TabPanel - ScattrPlot or Calibrated
        observe({
            if (input$tabMainPanel == "Calibration" && (input$TabCalibration %in% c("Scatterplot", "Calibrated", "TimeSeries", "Residual Matrix", "SummaryCal"))) {
                #shinyjs::enable(paste0("Cal", CalSet()$k))
                shinyjs::enable(paste0("Cal.Line", CalSet()$k))
            } else {
                #shinyjs::disable(paste0("Cal", CalSet()$k))
                shinyjs::disable(paste0("Cal.Line", CalSet()$k))
            }
        })
        # Updating date of al dateRange when date of input$Valid is changed and moving buttons ----
        observeEvent({
            # inputs to react on
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Valid", i)]]))
        },{
            # index k of selected sensor in list.namesensors(())
            Sens.Index    <- match(x = input$SetTime.Sensors, table = list.name.sensor())
            if (!any(is.na(input[[paste0("Valid", Sens.Index)]]))) {

                if (input[[paste0("Valid", Sens.Index)]][1] %within% interval(min.General.date(), max.General.date())) {
                    MINI <- as.Date(input[[paste0("Valid", Sens.Index)]][1])
                    Update.MINI <- FALSE
                } else {
                    MINI <- as.Date(min.General.date())
                    Update.MINI <- TRUE
                }
                if (input[[paste0("Valid", Sens.Index)]][2] %within% interval(min.General.date(), max.General.date())) {
                    MAXI <- as.Date(input[[paste0("Valid", Sens.Index)]][2])
                    Update.MAXI <- FALSE
                } else {
                    MAXI <- as.Date(max.General.date())
                    Update.MAXI <- TRUE
                }
            } else {
                Is.na.Valid <- is.na(input[[paste0("Valid", Sens.Index)]])
                if (which(Is.na.Valid) == 1) {
                    MINI <- as.Date(min.General.date())
                    Update.MINI <- TRUE}
                if (which(Is.na.Valid) == 2) {
                    MAXI <- as.Date(max.General.date())
                    Update.MAXI <- TRUE}
            }
            if (Update.MINI) updateDateRangeInput(session = session,
                                                  inputId = paste0("Valid",Sens.Index),
                                                  start   = MINI,
                                                  min     = as.Date(min.General.date()))
            if (Update.MAXI) updateDateRangeInput(session = session,
                                                  inputId = paste0("Valid",Sens.Index),
                                                  end     = MAXI,
                                                  max     = as.Date(max.General.date()))
            # updating the dateRange according to input$valid, using only the date
            List.dateRange <- c(paste0("Out.Ref.Date",Sens.Index),
                                paste0("Out.Sens.Date",Sens.Index),
                                paste0("Date",Sens.Index),
                                paste0("DateCal",Sens.Index),
                                paste0("DatePlotCal",Sens.Index),
                                paste0("DateMeas",Sens.Index),
                                paste0("DatePlotMeas",Sens.Index))
            Interval.MINI.MAXI <- interval(MINI,MAXI)
            for (i in List.dateRange) {
                # input[[i]][1] outside date range and input[[i]][2] within date range
                if (!any(is.na(input[[i]]))) {
                    if (!input[[i]][1] %within% Interval.MINI.MAXI && input[[i]][2] %within% Interval.MINI.MAXI) {
                        updateDateRangeInput(session,
                                             inputId = i,
                                             min     = MINI,
                                             max     = MAXI,
                                             start   = MINI)
                    } else {
                        # input[[i]][1] and input[[i]][2] outside date range
                        if (!input[[i]][1] %within% Interval.MINI.MAXI && !input[[i]][2] %within% Interval.MINI.MAXI) {
                            updateDateRangeInput(session,
                                                 inputId = i,
                                                 min     = MINI,
                                                 max     = MAXI,
                                                 start   = MINI,
                                                 end     = MAXI)
                        } else  {
                            # input[[i]][1] within date range and input[[i]][2] outside date range
                            if (input[[i]][1] %within% Interval.MINI.MAXI && !input[[i]][2] %within% Interval.MINI.MAXI) {
                                updateDateRangeInput(session,
                                                     inputId = i,
                                                     min     = MINI,
                                                     max     = MAXI,
                                                     end     = MAXI)}
                        }
                    }
                } else {
                    Is.na.Valid <- is.na(input[[i]])
                    if (which(Is.na.Valid) == 1) {
                        updateDateRangeInput(session,
                                             inputId = i,
                                             min     = MINI,
                                             start   = MINI)}
                    if (which(Is.na.Valid) == 2) {
                        updateDateRangeInput(session,
                                             inputId = i,
                                             max     = MAXI,
                                             end     = MAXI)}
                }
            }
        }, ignoreInit = TRUE)
        # Out.Ref.Date1: Update Valid1 date range with left and right buttons ----
        # What if the VALID1 of this pollutant does not exist? put min()
        observeEvent({
            unlist(sapply(seq_along(list.gas.reference()), function(i) input[[paste0("left2Out.Ref.Date", i)]]))
        },{
            # index 1,2,3,4, 5, 6 of the reference gas selected in uiFiltering corresponding of rows of ASE_name.cfg
            Ref.Index <-  match(x = input$Filtering.References, table = list.gas.reference())
            # Corresponding index for Valid
            # Corresponding index for input$Valid
            Sens.Index <-  match(x = list.gas.sensor()[Ref.Index], table = list.gas.sensor())
            if (!is.na(Sens.Index)){
                # update correct dateRange
                if (!is.na(input[[paste0("Valid",Sens.Index)]][1])) updateDateRangeInput(session,
                                                                                         inputId = paste0("Out.Ref.Date",Ref.Index),
                                                                                         label   = NULL,
                                                                                         start   = as.Date(input[[paste0("Valid",Sens.Index)]][1]),
                                                                                         end     = NULL,
                                                                                         min     = as.Date(input[[paste0("Valid",Sens.Index)]][1]),
                                                                                         max     = NULL)
            }
        },
        ignoreInit = TRUE)
        observeEvent({
            unlist(sapply(seq_along(list.gas.reference()), function(i) input[[paste0("left1Out.Ref.Date", i)]]))
        }, {
            # index 1,2,3,4, 5, 6 of the reference gas selected in uiFiltering corresponding of rows of ASE_name.cfg
            Ref.Index  <-  match(x = input$Filtering.References, table = list.gas.reference())
            # Corresponding index for input$Valid
            Sens.Index <-  match(x = list.gas.sensor()[Ref.Index], table = list.gas.sensor())
            if (!is.na(Sens.Index)){
                range.date = input[[paste0("Out.Ref.Date",Ref.Index)]][2] - input[[paste0("Out.Ref.Date",Ref.Index)]][1]
                if (input[[paste0("Out.Ref.Date",Ref.Index)]][1] - range.date < as.Date(input[[paste0("Valid",Sens.Index)]][1])) {
                    Start = input[[paste0("Valid",Sens.Index)]][1]
                } else Start = input[[paste0("Out.Ref.Date",Ref.Index)]][1] - range.date
                End =  Start + range.date
                updateDateRangeInput(session,
                                     inputId = paste0("Out.Ref.Date",Ref.Index),
                                     label   = NULL,
                                     start   = Start,
                                     end     = End,
                                     min     = NULL,
                                     max     = NULL)
            }
        },
        ignoreInit = TRUE
        )
        observeEvent({
            unlist(sapply(seq_along(list.gas.reference()), function(i) input[[paste0("right1Out.Ref.Date", i)]]))
        }, {
            # index 1,2,3,4, 5, 6 of the reference gas selected in uiFiltering corresponding of rows of ASE_name.cfg
            Ref.Index <-  match(x = input$Filtering.References, table = list.gas.reference())
            # Corresponding index for input$Valid
            Sens.Index <-  match(x = list.gas.sensor()[Ref.Index], table = list.gas.sensor())
            if (!is.na(Sens.Index)) {
                range.date = input[[paste0("Out.Ref.Date",Ref.Index)]][2] - input[[paste0("Out.Ref.Date",Ref.Index)]][1]
                if (input[[paste0("Out.Ref.Date",Ref.Index)]][2] + range.date > as.Date(input[[paste0("Valid",Sens.Index)]][2])) {
                    End = input[[paste0("Valid",Sens.Index)]][2]
                } else End = input[[paste0("Out.Ref.Date",Ref.Index)]][2] + range.date
                Start = End - range.date
                updateDateRangeInput(session,
                                     inputId =  paste0("Out.Ref.Date",Ref.Index),
                                     label   = NULL,
                                     start   = Start,
                                     end     = End,
                                     min     = NULL,
                                     max     = NULL)}
        },
        ignoreInit = TRUE
        )
        observeEvent({
            unlist(sapply(seq_along(list.gas.reference()), function(i) input[[paste0("right2Out.Ref.Date", i)]]))
        }, {
            # index 1,2,3,4, 5, 6 of the reference gas selected in uiFiltering corresponding of rows of ASE_name.cfg
            Ref.Index <-  match(x = input$Filtering.References, table = list.gas.reference())
            # Corresponding index for input$Valid
            Sens.Index <-  match(x = list.gas.sensor()[Ref.Index], table = list.gas.sensor())
            if (!is.na(Sens.Index)) {
                # update correct dateRange
                if (!is.na(input[[paste0("Valid",Sens.Index)]][2])) updateDateRangeInput(session,
                                                                                         inputId = paste0("Out.Ref.Date",Ref.Index),
                                                                                         label   = NULL,
                                                                                         start   = NULL,
                                                                                         end     = input[[paste0("Valid",Sens.Index)]][2],
                                                                                         min     = NULL,
                                                                                         max     = input[[paste0("Valid",Sens.Index)]][2])}
        },ignoreInit = TRUE)
        # Valid1: Update Valid1 date range with left and right buttons ----
        observeEvent({
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("left2Out.Valid", i)]]))
        }, {
            # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
            Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensor())
            updateDateRangeInput(session,
                                 inputId = paste0("Valid", Sens.Index),
                                 start   = as.Date(min.General.date()),
                                 min     = as.Date(min.General.date()))
            },
        ignoreInit = TRUE
        )
        observeEvent({
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("right2Out.Valid", i)]]))
        }, {
            # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
            Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensor())
            updateDateRangeInput(session,
                                 inputId = paste0("Valid",Sens.Index),
                                 end     = as.Date(max.General.date()),
                                 max     = as.Date(max.General.date())
            )
        },
        ignoreInit = TRUE
        )
        # Out.Sens.Date1: Range of dates for plotting RawData, DataTable, Retrieved, Warming, Temp&Humid, Invalids and outliers: ----
        observeEvent({
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("left2Out.Sens.Date", i)]]))
        }, {
            # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
            Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensor())
            updateDateRangeInput(session,
                                 inputId = paste0("Out.Sens.Date", Sens.Index),
                                 start   = as.Date(input[[paste0("Valid",Sens.Index)]][1]),
                                 min     = as.Date(input[[paste0("Valid",Sens.Index)]][1]))
        },
        ignoreInit = TRUE
        )
        observeEvent({
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("left1Out.Sens.Date", i)]]))
        }, {
            # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
            Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensor())
            range.date = input[[paste0("Out.Sens.Date",Sens.Index)]][2] - input[[paste0("Out.Sens.Date",Sens.Index)]][1]
            if (input[[paste0("Out.Sens.Date",Sens.Index)]][1] - range.date < as.Date(input[[paste0("Valid",Sens.Index)]][1])) {
                Start = as.Date(input[[paste0("Valid",Sens.Index)]][1])
            } else Start = input[[paste0("Out.Sens.Date",Sens.Index)]][1] - range.date
            End =  Start + range.date
            updateDateRangeInput(session,
                                 inputId = paste0("Out.Sens.Date", Sens.Index),
                                 start   = Start,
                                 end     = End)
        },
        ignoreInit = TRUE
        )
        observeEvent({
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("right1Out.Sens.Date", i)]]))
        }, {
            # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
            Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensor())
            range.date = input[[paste0("Out.Sens.Date",Sens.Index)]][2] - input[[paste0("Out.Sens.Date",Sens.Index)]][1]
            if (input[[paste0("Out.Sens.Date",Sens.Index)]][2] + range.date > as.Date(input$Valid1[2])) {
                End = as.Date(input[[paste0("Valid",Sens.Index)]][2])
            } else End = input[[paste0("Out.Sens.Date",Sens.Index)]][2] + range.date
            Start = End - range.date
            updateDateRangeInput(session,
                                 inputId = paste0("Out.Sens.Date", Sens.Index),
                                 start   = Start,
                                 end     = End)
        },
        ignoreInit = TRUE
        )
        observeEvent({
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("right2Out.Sens.Date", i)]]))
        }, {
            # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
            Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensor())
            updateDateRangeInput(session,
                                 inputId = paste0("Out.Sens.Date", Sens.Index),
                                 end     = as.Date(input[[paste0("Valid",Sens.Index)]][2]),
                                 max     = as.Date(input[[paste0("Valid",Sens.Index)]][2]))
        },
        ignoreInit = TRUE
        )
        # Date1: Range of dates for plotting covariates in UTC:, buttons left and write and checking rage of date
        observeEvent(
            {unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("left2Date", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensor())
                updateDateRangeInput(session,
                                     inputId = paste0("Date", Sens.Index),
                                     start   = as.Date(input[[paste0("Valid",Sens.Index)]][1]),
                                     min     = as.Date(input[[paste0("Valid",Sens.Index)]][1]))
            },
            ignoreInit = TRUE
        )
        observeEvent(
            {unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("left1Date", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensor())
                range.date = input[[paste0("Date",Sens.Index)]][2] - input[[paste0("Date",Sens.Index)]][1]
                if (input[[paste0("Date",Sens.Index)]][1] - range.date < as.Date(input[[paste0("Valid",Sens.Index)]][1])) {
                    Start = as.Date(input[[paste0("Valid",Sens.Index)]][1])
                } else Start = input[[paste0("Date",Sens.Index)]][1] - range.date
                End =  Start + range.date
                updateDateRangeInput(session,
                                     inputId = paste0("Date", Sens.Index),
                                     start   = Start,
                                     end     = End)
            },
            ignoreInit = TRUE
        )
        observeEvent(
            {unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("right1Date", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensor())
                range.date = input[[paste0("Date",Sens.Index)]][2] - input[[paste0("Date",Sens.Index)]][1]
                if (input[[paste0("Date",Sens.Index)]][2] + range.date > as.Date(input$Valid1[2])) {
                    End = as.Date(input[[paste0("Valid",Sens.Index)]][2])
                } else End = input[[paste0("Date",Sens.Index)]][2] + range.date
                Start = End - range.date
                updateDateRangeInput(session,
                                     inputId = paste0("Date", Sens.Index),
                                     start   = Start,
                                     end     = End)
            },
            ignoreInit = TRUE
        )
        observeEvent(
            {unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("right2Date", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensor())
                updateDateRangeInput(session,
                                     inputId = paste0("Date", Sens.Index),
                                     end     = as.Date(input[[paste0("Valid",Sens.Index)]][2]),
                                     max     = as.Date(input[[paste0("Valid",Sens.Index)]][2]))
            },
            ignoreInit = TRUE
        )
        # DateCal1: Range of dates for calibration in UTC:, buttons left and write and checking rage of date
        observeEvent(
            {unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("left2DateCal", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensor())
                updateDateRangeInput(session,
                                     inputId = paste0("DateCal", Sens.Index),
                                     start   = as.Date(input[[paste0("Valid",Sens.Index)]][1]),
                                     min     = as.Date(input[[paste0("Valid",Sens.Index)]][1]))
            },
            ignoreInit = TRUE
        )
        observeEvent(
            {unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Left1DateCal", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensor())
                range.date = input[[paste0("DateCal",Sens.Index)]][2] - input[[paste0("DateCal",Sens.Index)]][1]
                #Class.date   <- class(input[[paste0("DateCal",Sens.Index)]]) == "date"
                #Correct.date <- input[[paste0("DateCal",Sens.Index)]][1] - range.date < as.Date(input[[paste0("Valid",Sens.Index)]][1])
                if (input[[paste0("DateCal",Sens.Index)]][1] - range.date < as.Date(input[[paste0("Valid",Sens.Index)]][1])) {
                    Start = as.Date(input[[paste0("Valid",Sens.Index)]][1])
                } else Start = input[[paste0("DateCal",Sens.Index)]][1] - range.date
                End =  Start + range.date
                updateDateRangeInput(session,
                                     inputId = paste0("DateCal", Sens.Index),
                                     label   = NULL,
                                     start   = Start,
                                     end     = End,
                                     min     = NULL,
                                     max     = NULL)
            },
            ignoreInit = TRUE
        )
        observeEvent(
            {unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("right1DateCal", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensor())
                range.date = input[[paste0("DateCal",Sens.Index)]][2] - input[[paste0("DateCal",Sens.Index)]][1]
                if (input[[paste0("DateCal",Sens.Index)]][2] + range.date > as.Date(input$Valid1[2])) {
                    End = as.Date(input[[paste0("Valid",Sens.Index)]][2])
                } else End = input[[paste0("DateCal",Sens.Index)]][2] + range.date
                Start = End - range.date
                updateDateRangeInput(session,
                                     inputId = paste0("DateCal", Sens.Index),
                                     label   = NULL,
                                     start   = Start,
                                     end     = End,
                                     min     = NULL,
                                     max     = NULL)
            },
            ignoreInit = TRUE
        )
        observeEvent(
            {unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("right2DateCal", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensor())
                updateDateRangeInput(session,
                                     inputId = paste0("DateCal", Sens.Index),
                                     label   = NULL,
                                     start   = NULL,
                                     end     = as.Date(input[[paste0("Valid",Sens.Index)]][2]),
                                     min     = NULL,
                                     max     = as.Date(input[[paste0("Valid",Sens.Index)]][2]))
            },
            ignoreInit = TRUE
        )
        # DatePlotCal1: Range of dates for plotting calibration in UTC:, buttons left and write and checking rage of date
        observeEvent(
            {unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("left2DatePlotCal", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensor())
                updateDateRangeInput(session,
                                     inputId = paste0("DatePlotCal", Sens.Index),
                                     label   = NULL,
                                     start   = as.Date(input[[paste0("Valid",Sens.Index)]][1]),
                                     end     = NULL,
                                     min     = as.Date(input[[paste0("Valid",Sens.Index)]][1]),
                                     max     = NULL)
            },
            ignoreInit = TRUE
        )
        observeEvent(
            {unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("left1DatePlotCal", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensor())
                range.date = input[[paste0("DatePlotCal",Sens.Index)]][2] - input[[paste0("DatePlotCal",Sens.Index)]][1]
                if (input[[paste0("DatePlotCal",Sens.Index)]][1] - range.date < as.Date(input[[paste0("Valid",Sens.Index)]][1])) {
                    Start = as.Date(input[[paste0("Valid",Sens.Index)]][1])
                } else Start = input[[paste0("DatePlotCal",Sens.Index)]][1] - range.date
                End =  Start + range.date
                updateDateRangeInput(session,
                                     inputId = paste0("DatePlotCal", Sens.Index),
                                     label   = NULL,
                                     start   = Start,
                                     end     = End,
                                     min     = NULL,
                                     max     = NULL)
            },
            ignoreInit = TRUE
        )
        observeEvent(
            {unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("right1DatePlotCal", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensor())
                range.date = input[[paste0("DatePlotCal",Sens.Index)]][2] - input[[paste0("DatePlotCal",Sens.Index)]][1]
                if (input[[paste0("DatePlotCal",Sens.Index)]][2] + range.date > as.Date(input$Valid1[2])) {
                    End = as.Date(input[[paste0("Valid",Sens.Index)]][2])
                } else End = input[[paste0("DatePlotCal",Sens.Index)]][2] + range.date
                Start = End - range.date
                updateDateRangeInput(session,
                                     inputId = paste0("DatePlotCal", Sens.Index),
                                     label   = NULL,
                                     start   = Start,
                                     end     = End,
                                     min     = NULL,
                                     max     = NULL)
            },
            ignoreInit = TRUE
        )
        observeEvent(
            {unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("right2DatePlotCal", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensor())
                updateDateRangeInput(session,
                                     inputId = paste0("DatePlotCal", Sens.Index),
                                     label   = NULL,
                                     start   = NULL,
                                     end     = as.Date(input[[paste0("Valid",Sens.Index)]][2]),
                                     min     = NULL,
                                     max     = as.Date(input[[paste0("Valid",Sens.Index)]][2]))
            },
            ignoreInit = TRUE
        )
        # DateMeas1: Range of dates for Prediction in UTC:, buttons left and write and checking rage of date
        observeEvent(
            {unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("left2DateMeas", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensor())
                updateDateRangeInput(session,
                                     inputId = paste0("DateMeas", Sens.Index),
                                     label   = NULL,
                                     start   = as.Date(input[[paste0("Valid",Sens.Index)]][1]),
                                     end     = NULL,
                                     min     = as.Date(input[[paste0("Valid",Sens.Index)]][1]),
                                     max     = NULL)
            },
            ignoreInit = TRUE
        )
        observeEvent(
            {unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("left1DateMeas", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensor())
                range.date = input[[paste0("DateMeas",Sens.Index)]][2] - input[[paste0("DateMeas",Sens.Index)]][1]
                if (input[[paste0("DateMeas",Sens.Index)]][1] - range.date < as.Date(input[[paste0("Valid",Sens.Index)]][1])) {
                    Start = as.Date(input[[paste0("Valid",Sens.Index)]][1])
                } else Start = input[[paste0("DateMeas",Sens.Index)]][1] - range.date
                End =  Start + range.date
                updateDateRangeInput(session,
                                     inputId = paste0("DateMeas", Sens.Index),
                                     label   = NULL,
                                     start   = Start,
                                     end     = End,
                                     min     = NULL,
                                     max     = NULL)
            },
            ignoreInit = TRUE
        )
        observeEvent(
            {unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("right1DateMeas", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensor())
                range.date = input[[paste0("DateMeas",Sens.Index)]][2] - input[[paste0("DateMeas",Sens.Index)]][1]
                if (input[[paste0("DateMeas",Sens.Index)]][2] + range.date > as.Date(input$Valid1[2])) {
                    End = as.Date(input[[paste0("Valid",Sens.Index)]][2])
                } else End = input[[paste0("DateMeas",Sens.Index)]][2] + range.date
                Start = End - range.date
                updateDateRangeInput(session,
                                     inputId = paste0("DateMeas", Sens.Index),
                                     label   = NULL,
                                     start   = Start,
                                     end     = End,
                                     min     = NULL,
                                     max     = NULL)
            },
            ignoreInit = TRUE
        )
        observeEvent(
            {unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("right2DateMeas", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensor())
                updateDateRangeInput(session,
                                     inputId = paste0("DateMeas", Sens.Index),
                                     label   = NULL,
                                     start   = NULL,
                                     end     = as.Date(input[[paste0("Valid",Sens.Index)]][2]),
                                     min     = NULL,
                                     max     = as.Date(input[[paste0("Valid",Sens.Index)]][2]))
            },
            ignoreInit = TRUE
        )
        # DatePlotMeas1: Range of dates for plotting Prediction in UTC:, buttons left and write and checking rage of date
        observeEvent(
            {unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("left2DatePlotMeas", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensor())
                updateDateRangeInput(session,
                                     inputId = paste0("DatePlotMeas", Sens.Index),
                                     label   = NULL,
                                     start   = as.Date(input[[paste0("Valid",Sens.Index)]][1]),
                                     end     = NULL,
                                     min     = as.Date(input[[paste0("Valid",Sens.Index)]][1]),
                                     max     = NULL)
            },
            ignoreInit = TRUE
        )
        observeEvent(
            {unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("left1DatePlotMeas", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensor())
                range.date = input[[paste0("DatePlotMeas",Sens.Index)]][2] - input[[paste0("DatePlotMeas",Sens.Index)]][1]
                if (input[[paste0("DatePlotMeas",Sens.Index)]][1] - range.date < as.Date(input[[paste0("Valid",Sens.Index)]][1])) {
                    Start = as.Date(input[[paste0("Valid",Sens.Index)]][1])
                } else Start = input[[paste0("DatePlotMeas",Sens.Index)]][1] - range.date
                End =  Start + range.date
                updateDateRangeInput(session,
                                     inputId = paste0("DatePlotMeas", Sens.Index),
                                     label   = NULL,
                                     start   = Start,
                                     end     = End,
                                     min     = NULL,
                                     max     = NULL)
            },
            ignoreInit = TRUE
        )
        observeEvent(
            {unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("right1DatePlotMeas", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensor())
                range.date = input[[paste0("DatePlotMeas",Sens.Index)]][2] - input[[paste0("DatePlotMeas",Sens.Index)]][1]
                if (input[[paste0("DatePlotMeas",Sens.Index)]][2] + range.date > as.Date(input$Valid1[2])) {
                    End = as.Date(input[[paste0("Valid",Sens.Index)]][2])
                } else End = input[[paste0("DatePlotMeas",Sens.Index)]][2] + range.date
                Start = End - range.date
                updateDateRangeInput(session,
                                     inputId = paste0("DatePlotMeas", Sens.Index),
                                     label   = NULL,
                                     start   = Start,
                                     end     = End,
                                     min     = NULL,
                                     max     = NULL)
            },
            ignoreInit = TRUE
        )
        observeEvent(
            {unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("right2DatePlotMeas", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensor())
                updateDateRangeInput(session,
                                     inputId = paste0("DatePlotMeas", Sens.Index),
                                     label   = NULL,
                                     start   = NULL,
                                     end     = as.Date(input[[paste0("Valid",Sens.Index)]][2]),
                                     min     = NULL,
                                     max     = as.Date(input[[paste0("Valid",Sens.Index)]][2]))
            },
            ignoreInit = TRUE
        )
        # NavBar"DataTreatment", SideBar"Calib", button Delete calibration model ----
        # We need the variables of the curent environment, so it is important that function Delete.Model remains in the App.R
        observeEvent(input$Delete.Model, {
            if (input$Delete.Model) {
                # Detect Selected Sensors
                # k is the index (1,2,3,4, of the selected  sensors in uiCalib corresponding of position in list.name.sensor()
                k    <- match(x = input$Calib.Sensors, table = list.name.sensor())
                # Detect Selected Model
                Cal  <- paste0(CalSet()$AirSensEur.name,"__",input$Calib.Sensors,"__",input[[paste0("Cal",k)]])
                cat(paste0("[shiny, Delete.Model] INFO, deleting calibrattion model, value shinyalert : ", Cal))
                if (!is.null(Cal)) {
                    #deleting
                    do.call(file.remove, list(list.files(path       = input$Selected,
                                                         pattern    = glob2rx(paste0("*", Cal,"*")),
                                                         full.names = TRUE,
                                                         recursive = TRUE,
                                                         include.dirs = TRUE)))
                    # Update list of Models
                    choices <- substr(list.files(path = file.path(input$Selected,"Models"),
                                                 pattern = glob2rx(paste0(CalSet()$AirSensEur.name,"*",input$Calib.Sensors,"*"))),
                                      start = nchar(paste0(CalSet()$AirSensEur.name,"__",input$Calib.Sensors,"__")) + 1,
                                      stop  = nchar(list.files(path = file.path(input$Selected,"Models"),
                                                               pattern = glob2rx(paste0(CalSet()$AirSensEur.name,"*",input$Calib.Sensors,"*"))))
                    )
                    # Update Selected Model
                    updateSelectInput(session  = session,
                                      inputId  = paste0("Cal",k),
                                      label    = NULL,
                                      choices  = choices,
                                      selected = choices[1]
                    )
                } else {
                    # Message no model to select
                    shinyalert(
                        title = "Warning",
                        text = "Select a model to delete!",
                        closeOnEsc = TRUE,
                        closeOnClickOutside = FALSE,
                        html = FALSE,
                        type = "warning",
                        showConfirmButton = FALSE,
                        showCancelButton = FALSE,
                        timer = 0,
                        imageUrl = "",
                        animation = FALSE)
                }
            }
        })
        observeEvent({
            sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DelModel",i)]])
        },{
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            progress$set(message = "[shiny, DelModel] INFO, Deleting Model and all related pictures. CLICK OK ONLY ONCE!", value = 0.5)
            if (!is.null(input$Calib.Sensors)) {
                #if (!is.null(list.name.sensor())) {
                # k is the index (1,2,3,4, of the selected  sensors in uiCalib corresponding of position in list.name.sensor()
                k    <- match(x = input$Calib.Sensors, table = list.name.sensor())
                if (!is.null(input[[paste0("Cal",k)]])) {
                    # Detect Selected Model
                    #Cal  <- input[[paste0("Cal",k)]]
                    # Show a modal message when the button "DelModel" is pressed
                    if (CalSet()$Cal != "") {
                        confirmSweetAlert(
                            session     = session,
                            inputId     = "Delete.Model",
                            type        = "warning",
                            title       = "Confirm",
                            text        = paste0("Are you sure to delete the calibration model ",input[[paste0("Cal",k)]]," and relative plots?"),
                            btn_labels  = c("Cancel", "Delete files"),
                            danger_mode = TRUE,
                            html        = TRUE
                        )
                    } else {
                        shinyalert(
                            title = "Warning Model",
                            text = "No model is selected.",
                            closeOnEsc = TRUE,
                            closeOnClickOutside = TRUE,
                            html = FALSE,
                            type = "warning",
                            showConfirmButton = TRUE,
                            showCancelButton = FALSE,
                            confirmButtonText = "OK",
                            confirmButtonCol = "#AEDEF4",
                            timer = 0,
                            imageUrl = "",
                            animation = FALSE
                        )                }
                }
            }
            progress$set(message = "[shiny, DelModel] INFO, Deleting Model and all related pictures. CLICK OK ONLY ONCE!", value = 1)
        },
        ignoreInit = TRUE, # Whether the action should be triggered (or value calculated, in the case of eventReactive) when the input is NULL.
        ignoreNULL = TRUE  # If TRUE, then, when this observeEvent is first created/initialized, ignore the handlerExpr (the second argument), whether it is otherwise supposed to run or not. The default is FALSE.
        )
        # REPORT SERVER ----
        output$renderedReport <- renderUI({
            includeMarkdown(knitr::knit(file.path(DirShiny, 'report.Rmd')))
        })
        Render_report <- eventReactive(CalSet()$cal, {
            knitr::knit(file.path(DirShiny, 'report.Rmd'))
        })
        # download report
        output$report <- downloadHandler(
            filename = file.path(CalSet()$WDModelled_gas, paste0(AirSensEur.name(),"__",CalSet()$name.sensor,"__",CalSet()$Cal,"__.html")),
            content  = function(file) {
                #if (file.exists(filename)) file.remove(filename)
                renderedFile <- render(
                    input = file.path(DirShiny, "report.Rmd"),
                    output_file = paste0(input$Selected, "/", "report.html"))
                markdown::markdownToHTML(paste0(input$Selected,"/","report.md"),
                                         paste0(input$Selected,"/","report.html"), options = c("use_xhml"))})
        # Reactive AirSensEur.name() name of ASE box ----
        AirSensEur.name <- reactive({
            old_ASE_name       <- basename(input$Selected)
            for (i in c("\\.[[:alnum:]_]+$" ,"ASEconfig")) old_ASE_name <- sub(pattern = i,replacement = '', basename(as.character(old_ASE_name)))
            return(old_ASE_name)
            #if (input$AirSensEur.name == "") return(ASE_name()) else return(input$AirSensEur.name)
        })
        # NavBar"Console Logs", Plotting Console output ----
        output$console <- renderPrint({logText()})
        logText        <- reactive({
            input$UpdateLog # It updates each time we click of the button UpdateLog
            return(ReadLastLines(file.path(input$Selected, "scriptsLog",paste0("console_", Sys.Date(),".log")),1000)) # only 1000 lines can be viewed
        })
        # Merging All data
        # Reactive Change.Delay ----
        # Detecting if input$Delay was changed to trigger a new DF$General data.table
        Change.Delay             <- reactive(
            # input$Delay
            # Config$all[["Server"]]$Delay,
            {
                if (as.integer(input$Delay) != Config$all[["Server"]]$Delay) {
                    shinyjs::click(id = "Save")
                    return(TRUE)
                } else return(FALSE)
            })
        # Reactive Change.UserMins ----
        # Detecting if input$UserMins was changed to trigger a new DF$General dataFrame
        Change.UserMins             <- reactive({
            if (as.integer(input$UserMins) != Config$all[["Server"]]$UserMins) return(TRUE) else return(FALSE)
        })
        # MemoryUsage
        env <- environment()  # can use globalenv(), parent.frame(), etc
        Memory.use <- reactive({
            DF$General
            return(data.frame(
                object = ls(env),
                size   = unlist(lapply(ls(env), function(x) {object.size(get(x, envir = env, inherits = FALSE))}
                ))
            )
            )
        })
        output$foo <- renderTable({
            Memory.use()
        })
        observeEvent({
            input$Down.Influx
            input$Down.Ref
            DF.NULL$Init
            input$Merge
            Change.Delay()
            list.gas.sensor()
        },{
            # depends :
            #           input$Selected
            #           input$UserMins
            #           input$Delay
            #           Ref$DATA
            #           Influx$DATA
            #           Sos$DATA
            #           DownloadSensor()
            # Checking that parameters for sensor download are complete or that they are new data
            if (DF.NULL$Init || Change.Delay() ||
                isTRUE(Download$Sensor$DateEND.General.prev < Download$Sensor$DateEND.Ref.prev) ||
                isTRUE(Download$Sensor$DateEND.General.prev < Download$Sensor$DateEND.Influx.prev) ||
                isTRUE(Download$Sensor$DateEND.General.prev < Download$Sensor$DateEND.SOS.prev)) {
                
                # Create a Progress object
                progress <- shiny::Progress$new()
                progress$set(message = "[shiny, General()] INFO, Merging Influx, SOS and Reference data", value = 0.5)
                # Make sure it closes when we exit this reactive, even if there's an error
                on.exit(progress$close())
                
                if (input$Down.Influx && any(is.null(c(input$Host, input$User, input$Pass, input$Db, input$Dataset))) ) {
                    shinyalert(
                        title = "Error Influx download",
                        text = "Download of sensor data requested but parameters are missing, check Down.Influx, Host, User, Pass, Db, Dataset",
                        closeOnEsc = TRUE,
                        closeOnClickOutside = TRUE,
                        html = FALSE,
                        type = "error",
                        showConfirmButton = TRUE,
                        showCancelButton = FALSE,
                        confirmButtonText = "OK",
                        confirmButtonCol = "#AEDEF4",
                        timer = 0,
                        imageUrl = "",
                        animation = FALSE
                    )
                } else {
                    # Checking that if SOS download is requested an SOS rest API is supplied
                    if (input$Down.SOS && input$AirsensWeb == "") {
                        shinyalert(
                            title = "Error sensor data download",
                            text  = "SOS sensor data download is requested. However, you have to fill the rest API and AirSensEUR name.",
                            closeOnEsc = TRUE,
                            closeOnClickOutside = TRUE,
                            html = FALSE,
                            type = "error",
                            showConfirmButton = TRUE,
                            showCancelButton = FALSE,
                            confirmButtonText = "OK",
                            confirmButtonCol = "#AEDEF4",
                            timer = 0,
                            imageUrl = "",
                            animation = FALSE
                        )
                    } else {
                        # Checking download of reference data
                        if (input$Down.Ref && (
                            (input$FTPMode == "ftp" && input$urlref == "") ||
                            (input$FTPMode == "SOS" && any(is.null(c(input$Ref.SOS.name, input$RefSOSname))))
                            )) {
                            shinyalert(
                                title = "Error SOS reference data download",
                                text  = "Download of sensor data is requested but parameters are missing: \"URL of the ftp server with full name\" for ftp or
                                \"Reference station SOS Rest API URL\" and \"SOS ID of the Reference station\" for SOS or the parameters of the aip server.",
                                closeOnEsc = TRUE,
                                closeOnClickOutside = TRUE,
                                html = FALSE,
                                type = "error",
                                showConfirmButton = TRUE,
                                showCancelButton = FALSE,
                                confirmButtonText = "OK",
                                confirmButtonCol = "#AEDEF4",
                                timer = 0,
                                imageUrl = "",
                                animation = FALSE
                            )
                        } else {
                            # checking that both SOS and Influx are not requested for downloading sensor data
                            if (input$Down.SOS && input$Down.Influx) {
                                shinyalert(
                                    title = "Error sensor data download",
                                    text  = "It is not possible to allow both for SOS and Influx sensor data download in the same session, please check only one tyope of download.",
                                    closeOnEsc = TRUE,
                                    closeOnClickOutside = TRUE,
                                    html = FALSE,
                                    type = "error",
                                    showConfirmButton = TRUE,
                                    showCancelButton = FALSE,
                                    confirmButtonText = "OK",
                                    confirmButtonCol = "#AEDEF4",
                                    timer = 0,
                                    imageUrl = "",
                                    animation = FALSE
                                )
                            } else {
                                # getting what it would be to put sensor and reference data together to later compare with what is in DF$General
                                D <- GENERAL(WDoutput            = file.path(input$Selected, "General_data"),
                                             UserMins            = as.numeric(input$UserMins),
                                             Delay               = as.numeric(input$Delay),
                                             RefData             = Ref$DATA,
                                             InfluxData          = Influx$DATA,
                                             SOSData             = Sos$DATA,
                                             var.name.GasSensors = list.gas.sensor()  ,
                                             DownloadSensor      = Download$Sensor,
                                             Change.Delay        = Change.Delay(),
                                             Change.UserMins     = Change.UserMins()
                                )
                                # saving New General data if needed
                                save.General.df <- FALSE
                                if (is.null(DF$General) || nrow(DF$General) == 0 ||
                                    (!is.null(DF$General) && !isTRUE(all.equal(D[,.SD, .SDcols = intersect(names(D),names(DF$General))],
                                                                               DF$General[,.SD, .SDcols = intersect(names(D),names(DF$General))],
                                                                               , check.attributes = FALSE)))) {
                                        save.General.df <- TRUE
                                        DF$General <- D
                                        # Checking if the SETTIME are consistent with the avalable date in General()
                                        MINI = min(D$date, na.rm = T)
                                        MAXI = max(D$date, na.rm = T)
                                        List.dateRange <- c(paste0("Valid"        , c(seq_along(list.name.sensor()))),
                                                            paste0("Out.Sens.Date", c(seq_along(list.name.sensor()))),
                                                            paste0("Date"         , c(seq_along(list.name.sensor()))),
                                                            paste0("DateCal"      , c(seq_along(list.name.sensor()))),
                                                            paste0("DatePlotCal"  , c(seq_along(list.name.sensor()))),
                                                            paste0("DateMeas"     , c(seq_along(list.name.sensor()))),
                                                            paste0("DatePlotMeas" , c(seq_along(list.name.sensor()))),
                                                            paste0("Out.Ref.Date" , c(seq_along(list.gas.reference2use())))
                                        )
                                        # Adapting the date ranges
                                        for (i in List.dateRange) {
                                            if (i %in% c("Valid" ,"Out.Sens.Date", "Date", "DateCal", "DatePlotCal",
                                                         "DateMeas", "DatePlotMeas", "Out.Ref.Date")) next else {
                                                             if (is.na(input[[i]][1])) {
                                                                 updateDateRangeInput(session,
                                                                                      inputId = i,
                                                                                      start   = MINI,
                                                                                      end     = NULL,
                                                                                      min     = MINI,
                                                                                      max     = NULL)
                                                             } else if (is.na(input[[i]][2])) {
                                                                 updateDateRangeInput(session,
                                                                                      inputId = i,
                                                                                      start   = NULL,
                                                                                      end     = MAXI,
                                                                                      min     = NULL,
                                                                                      max     = MAXI)
                                                             } else if ((input[[i]][1] <  MINI | input[[i]][1] >  MAXI) &&
                                                                        (input[[i]][2] >= MINI &  input[[i]][2] <= MAXI) ) {
                                                                 # input[[i]][1] outside date range and input[[i]][2] within date range
                                                                 updateDateRangeInput(session,
                                                                                      inputId = i,
                                                                                      start   = MINI,
                                                                                      end     = input[[i]][2],
                                                                                      min     = MINI,
                                                                                      max     = input$Valid1[2])
                                                             } else {
                                                                 # input[[i]][1] and input[[i]][2] outside date range
                                                                 if ((input[[i]][1] < MINI | input[[i]][1] > MAXI) &&
                                                                     (input[[i]][2] < MINI | input[[i]][2] > MAXI) ) {
                                                                     updateDateRangeInput(session,
                                                                                          inputId = i,
                                                                                          min     = MINI,
                                                                                          max     = MAXI,
                                                                                          start   = MINI,
                                                                                          end     = MAXI)
                                                                 } else  {
                                                                     # input[[i]][1] within date range and input[[i]][2] outside date range
                                                                     if ((input[[i]][1] >=  MINI & input[[i]][1] <=  MAXI) &&
                                                                         (input[[i]][2] <   MINI | input[[i]][2] >   MAXI) ) {
                                                                         updateDateRangeInput(session,
                                                                                              inputId = i,
                                                                                              min     = input[[i]][1],
                                                                                              max     = MAXI,
                                                                                              start   = input[[i]][1],
                                                                                              end     =  MAXI)
                                                                     }
                                                                 }
                                                             }
                                                         }
                                        }
                                }
                                if (save.General.df) {
                                    # Saving downloaded data in General_data Files
                                    cat("-----------------------------------------------------------------------------------\n")
                                    # replacing nan by NA before saving
                                    #General.df[] <- lapply(General.df, nan.to.na)
                                    Cols.for.Avg <- names(DF$General)[-which(names(DF$General) == "date")]
                                    data.table::set(DF$General, j = Cols.for.Avg, value = lapply(DF$General[,..Cols.for.Avg], nan.to.na))
                                    # saving New General data
                                    progress$set(message = "[shiny, General()] INFO, Saving General dataset", value = 0.6)
                                    if (extension(General.file()) == ".csv") {
                                        fwrite(DF$General, file = General.file(), na = "NA")
                                    } else if (extension(General.file()) == ".Rdata") save(DF$General, file = General.file())
                                    # if general is saved, it is necessary to run the detection of warming, T/RH out of tolerance, Negative Ref., Invalids and outlier detection, sensor data conversion and calibration.
                                    # It is sufficient to set to TRUE to run ind.warm then in.TRH,
                                    progress$set(message = "[shiny, General()] INFO, Enabling detection of warming of sensors", value = 0.9)
                                    for (i in seq_along(list.name.sensor())) {
                                        if (!input[[paste0("Apply.Warm",i)]]) updateCheckboxInput(session, inputId = paste0("Apply.Warm",i), value = TRUE)
                                    }
                                    # We also need to save all config file since the ##################################################################################################C
                                    # Updating Download$Sensor
                                    Download$Sensor <<- DownloadSensor()
                                }
                                progress$set(message = "[shiny, General()] INFO, Merging Influx, SOS and Reference data", value = 1)
                            }
                        }
                    }
                }
            }
        }, priority = 3000)
        # Reactive min.General.date() and max.General.date----
        min.General.date <- eventReactive({
            input$Selected
            input$Merge
        },{if (!is.null(DF$General)) return(min(DF$General$date, na.rm = TRUE)) else return(NULL)
        })
        max.General.date <- eventReactive({
            input$Selected
            input$Merge
        },{
            if (!is.null(DF$General)) return(max(DF$General$date, na.rm = TRUE)) else return(NULL)
        })
        # Updating input$Valid max values of the rangeInput if max.General.date increase when new data are added
        observeEvent(max.General.date(), {
            # updating all InputDateranges
            List.dateRange <- c("Valid", "Out.Ref.Date", "Out.Sens.Date", "Date", "DateCal", "DatePlotCal", "DateMeas", "DatePlotMeas")
            for (i in seq_along(list.name.sensor())) {
                for (j in List.dateRange) {
                    updateDateRangeInput(session,
                                         inputId = paste0(j,i),
                                         label   = NULL,
                                         start   = NULL,
                                         end     = NULL,
                                         min     = NULL,
                                         max     = as.Date(max.General.date()))
                }
            }
        })
        # NavBar "SelectASE", mainTabPanel "Downloaded" ----
        # INformation on existin downloaded data in airsenseur.db, InfluxData, SOSData, RefData and General
        output$Downloaded      <- renderTable(Downloaded())
        # DownloadedSensor: info about status of merging sensor and reference data
        Downloaded <- reactive({
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            progress$set(message = "[shiny, Downloaded()] INFO, Detecting downloaded data in airsenseur.db, Influx, SOS and Reference and General files", value = 0.2)
            if (is.null(Download$Sensor[["DateIN.db.prev"]]))  DateIN.db.prev  <- "NULL" else DateIN.db.prev  <- format(ymd_hms(Download$Sensor[["DateIN.db.prev"]]) , "%Y-%m-%d %H:%M:%S")
            if (is.null(Download$Sensor[["DateEND.db.prev"]])) DateEND.db.prev <- "NULL" else DateEND.db.prev <- format(ymd_hms(Download$Sensor[["DateEND.db.prev"]]), "%Y-%m-%d %H:%M:%S")
            progress$set(message = "[shiny, Downloaded()] INFO, Detecting downloaded data in airsenseur.db, Influx, SOS and Reference and General files", value = 0.5)
            Downloaded <- data.frame(
                DataSets = c("airsenseur.db","InfluxData", "SOSData", "ReferenceData","General"),
                Exists   = c(Download$Sensor[["ExistFil.data.db"]],
                             Download$Sensor[["ExistFil.data.Influx"]],
                             Download$Sensor[["ExistFil.data.SOS"]],
                             Download$Sensor[["ExistFil.data.Ref"]],
                             Download$Sensor[["ExistFil.data.General"]]),
                NeedRetrieve = c(Download$Sensor[["Retrieve.data.db"]],
                                 Download$Sensor[["Retrieve.data.Influx"]],
                                 Download$Sensor[["Retrieve.data.SOS"]],
                                 Download$Sensor[["Retrieve.data.Ref"]],
                                 Download$Sensor[["Retrieve.data.General"]]),
                INdate   = c(DateIN.db.prev,
                             format(Download$Sensor[["DateIN.Influx.prev"]]  , "%Y-%m-%d %H:%M"),
                             format(Download$Sensor[["DateIN.SOS.prev"]]     , "%Y-%m-%d %H:%M"),
                             format(Download$Sensor[["DateIN.Ref.prev"]]     , "%Y-%m-%d %H:%M"),
                             format(Download$Sensor[["DateIN.General.prev"]] , "%Y-%m-%d %H:%M")),
                ENDdate  = c(DateEND.db.prev,
                             format(Download$Sensor[["DateEND.Influx.prev"]] , "%Y-%m-%d %H:%M"),
                             format(Download$Sensor[["DateEND.SOS.prev"]]    , "%Y-%m-%d %H:%M"),
                             format(Download$Sensor[["DateEND.Ref.prev"]]    , "%Y-%m-%d %H:%M"),
                             format(Download$Sensor[["DateEND.General.prev"]], "%Y-%m-%d %H:%M")),
                row.names = NULL,
                check.rows = FALSE,
                check.names = FALSE,
                stringsAsFactors = FALSE)
            progress$set(message = "[shiny, Downloaded()] INFO, Detecting downloaded data in airsenseur.db, Influx, SOS and Reference and General files", value = 1)
            on.exit(progress$close())
            return(Downloaded)
        })
        # NavBar"Data Treatment", mainTabPanel "FilteringMain" - "Config"  ----
        # Reactive Outliers_Sensor()
        Outliers_Sensor        <- reactive({
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "[Shiny, Outliers_Sensor()] INFO, Printing sensor outlier config file", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            # Compose data frame
            Sensors.Outliers <- data.frame(
                name.gas           = list.name.gas(),
                name.sensor        = list.name.sensor(),
                hoursWarming       = as.integer(sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("Warming",i)]])),
                temp.thres.min     = as.numeric(sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("Temperature",i)]][1])),
                temp.thres.max     = as.numeric(sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("Temperature",i)]][2])),
                rh.thres.min       = as.integer(sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("Humidity",i)]][1])),
                rh.thres.max       = as.integer(sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("Humidity",i)]][2])),
                Sens.Inval.Out     = as.logical(sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("Sens.Inval.Out",i)]])),
                Sens.rm.Out        = as.logical(sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("Sens.rm.Out",i)]])),
                Sens.window        = as.integer(sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("Sens.window",i)]])),
                Sens.threshold     = as.integer(sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("Sens.threshold",i)]])),
                Sens.Ymin          = as.integer(sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("Sens.Ymin",i)]])),
                Sens.Ymax          = as.integer(sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("Sens.Ymax",i)]])),
                Sens.ThresholdMin  = as.integer(sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("Sens.ThresholdMin",i)]])),
                Sens.iterations    = as.integer(sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("Sens.iterations",i)]])),
                stringsAsFactors   = FALSE)
            progress$set(message = "[Shiny, Outliers_Sensor()] INFO, Printing sensor outlier config file ", value = 1)
            return(Sensors.Outliers)
        })
        output$Outliers_Sensor <- renderTable({
            t(Outliers_Sensor())
        },
        digits = 1,
        include.rownames = TRUE,
        include.colnames = FALSE
        )
        # Reactive Outliers_Ref
        Outliers_Ref           <- reactive({
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "[Shiny, Outliers_Ref] INFO, Printing reference outlier config file", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            # Compose data frame
            Reference.Outliers <- data.frame(
                name.gas           = list.name.gas(),
                name.sensor        = list.name.sensor(),
                remove.neg         = as.logical(  sapply(seq_along(list.gas.reference2use()), function(i) input[[paste0("rm.neg",i)]])),
                ref.unitgas        = as.character(sapply(seq_along(list.gas.reference2use()), function(i) input[[paste0("Ref.unit",i)]])),
                Ref.rm.Out         = as.logical(  sapply(seq_along(list.gas.reference2use()), function(i) input[[paste0("Ref.rm.Out",i)]])),
                Ref.window         = as.integer(  sapply(seq_along(list.gas.reference2use()), function(i) input[[paste0("Ref.window",i)]])),
                Ref.threshold      = as.numeric(  sapply(seq_along(list.gas.reference2use()), function(i) input[[paste0("Ref.threshold",i)]])),
                Ref.Ymin           = as.numeric(  sapply(seq_along(list.gas.reference2use()), function(i) input[[paste0("Ref.Ymin",i)]])),
                Ref.Ymax           = as.numeric(  sapply(seq_along(list.gas.reference2use()), function(i) input[[paste0("Ref.Ymax",i)]])),
                Ref.ThresholdMin   = as.numeric(  sapply(seq_along(list.gas.reference2use()), function(i) input[[paste0("Ref.ThresholdMin",i)]])),
                Ref.iterations     = as.integer(  sapply(seq_along(list.gas.reference2use()), function(i) input[[paste0("Ref.iterations",i)]])),
                stringsAsFactors = FALSE)
            progress$set(message = "[Shiny, Outliers_Ref] INFO, Printing reference outlier config file", value = 1)
            return(Reference.Outliers)
        })
        output$Outliers_Ref    <- renderTable({
            t(Outliers_Ref())
        },
        digits = 1,
        include.rownames = TRUE,
        include.colnames = FALSE
        )
        # NavBar"Data Treatment", mainTabPanel "CalibMain" - "Config", ----
        output$Calib_data           <- renderTable({
            t(Calib_data())
        },
        digits = -4,
        include.rownames = TRUE,
        include.colnames = FALSE
        )
        # Reactive Calib_data
        Calib_data               <- reactive({
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "[Shiny, Calib_data()] INFO, Reading sensor calibration config file", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            Sensors.Outliers       <- data.frame(
                name.gas           = list.name.gas(),
                name.sensor        = list.name.sensor(),
                gas.reference      = list.gas.reference(),
                gas.reference2use  = list.gas.reference2use(),
                gas.sensor         = list.gas.sensor(),
                Sens.raw.unit      = as.character(sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("Sens.raw.unit",i)]])),
                Sens.unit          = as.character(sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("Sens.unit",i)]])),
                Cal.Line           = as.character(sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("Cal.Line",i)]])),
                Sync.Cal           = as.logical(sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("Sync.Cal",i)]])),
                Sync.Pred          = as.logical(sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("Sync.Pred",i)]])),
                Cal.func           = as.character(sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("Cal",i)]])),
                mod.eta.model.type = as.character(sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("Calibration",i)]])),
                Neg.mod            = as.logical(sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("Neg.mod",i)]])),
                Slope              = as.numeric(sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("Slope",i)]])),
                Intercept          = as.numeric(sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("Intercept",i)]])),
                ubsRM              = as.numeric(sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("ubsRM",i)]])),
                ubss               = as.numeric(sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("ubss",i)]])),
                eta.model.type     = as.character(sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("Comparison",i)]])),
                stringsAsFactors = FALSE)
            # To avoid ERROR/to update Calib_data make sure that the Tabset "Calib" is opened
            updateTabsetPanel(session, inputId = "Calib_data", selected = "Calib")
            progress$set(message = "Updatin sensor calibration using reactive values of the SideLayout", value = 1)
            return(Sensors.Outliers)
        })
        # NavBar"Data Treatment", mainTabPanel "SetTimeMain" - "Config" ----
        output$CalTime           <- renderTable({
            if (!is.null(CalTime())) t(CalTime())
        },
        include.rownames = TRUE,
        include.colnames = FALSE
        )
        # Reactive CalTime
        CalTime                  <- reactive({
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "Reading sensor calibration date time of the SideBarLayout", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            Sensors.Outliers <- data.frame(
                name.gas           = list.name.gas(),
                name.sensor        = list.name.sensor(),
                Out.Ref.IN         = sapply(seq_along(list.gas.reference2use()), function(i) format(input[[paste0("Out.Ref.Date",i)]][1], format = "%Y-%m-%d %H:%M:%S")),
                Out.Ref.END        = sapply(seq_along(list.gas.reference2use()), function(i) format(input[[paste0("Out.Ref.Date",i)]][2], format = "%Y-%m-%d %H:%M:%S")),
                # in uiFiltering date plot for sensors and Referencces
                Out.Sens.IN         = sapply(seq_along(list.gas.sensor()), function(i) format(input[[paste0("Out.Sens.Date",i)]][1], format = "%Y-%m-%d %H:%M:%S")),
                Out.Sens.END        = sapply(seq_along(list.gas.sensor()), function(i) format(input[[paste0("Out.Sens.Date",i)]][2], format = "%Y-%m-%d %H:%M:%S")),
                # in uiSetTime Valid, Cal, Prediction and Plotting dates
                Sens.Inval.Out     = sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("Sens.Inval.Out",i)]]),
                Apply.Invalid      = sapply(seq_along(list.gas.sensor()), function(i) input[[paste0("Apply.Invalid",i )]]),
                # Valid date
                Valid.IN             = sapply(seq_along(list.gas.sensor()), function(i) format(input[[paste0("Valid",i)]][1], format = "%Y-%m-%d %H:%M:%S")),
                Valid.END            = sapply(seq_along(list.gas.sensor()), function(i) format(input[[paste0("Valid",i)]][2], format = "%Y-%m-%d %H:%M:%S")),
                # Date for plotting covariates
                Cov.Date.IN          = sapply(seq_along(list.gas.sensor()), function(i) format(input[[paste0("Date",i)]][1], format = "%Y-%m-%d %H:%M:%S")),
                Cov.Date.END         = sapply(seq_along(list.gas.sensor()), function(i) format(input[[paste0("Date",i)]][2], format = "%Y-%m-%d %H:%M:%S")),
                # Calibration dates
                DateCal.IN          = sapply(seq_along(list.gas.sensor()), function(i) format(input[[paste0("DateCal",i)]][1], format = "%Y-%m-%d %H:%M:%S")),
                DateCal.END         = sapply(seq_along(list.gas.sensor()), function(i) format(input[[paste0("DateCal",i)]][2], format = "%Y-%m-%d %H:%M:%S")),
                # Plotting Calibration dates
                DatePlotCal.IN      = sapply(seq_along(list.gas.sensor()), function(i) format(input[[paste0("DatePlotCal",i)]][1], format = "%Y-%m-%d %H:%M:%S")),
                DatePlotCal.END     = sapply(seq_along(list.gas.sensor()), function(i) format(input[[paste0("DatePlotCal",i)]][2], format = "%Y-%m-%d %H:%M:%S")),
                # Extratpolation date
                DateMeas.IN         = sapply(seq_along(list.gas.sensor()), function(i) format(input[[paste0("DateMeas",i)]][1], format = "%Y-%m-%d %H:%M:%S")),
                DateMeas.END        = sapply(seq_along(list.gas.sensor()), function(i) format(input[[paste0("DateMeas",i)]][2], format = "%Y-%m-%d %H:%M:%S")),
                # Prediction date for plotting
                DatePlotMeas.IN     = sapply(seq_along(list.gas.sensor()), function(i) format(input[[paste0("DatePlotMeas",i)]][1], format = "%Y-%m-%d %H:%M:%S")),
                DatePlotMeas.END    = sapply(seq_along(list.gas.sensor()), function(i) format(input[[paste0("DatePlotMeas",i)]][2], format = "%Y-%m-%d %H:%M:%S")),
                stringsAsFactors = FALSE)
            progress$set(message = "Updating sensor calibration using reactive values of the SideLayout", value = 1)
            return((Sensors.Outliers))
        })
        # NavBar"Data Treatment", mainTabPanel "RawData", ----
        output$ts_RawData_dygraphs <- renderUI({
            #----------------------------------------------------------CR
            # plotting time series of Raw Data (mainlt Influx Data)
            #----------------------------------------------------------CR
            # depends:
            #   DF$General
            #   input$Valid1
            # isolates:
            #   input$Sensors,
            # Plotting raw downloaded data in DF$General
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat("[shiny, Plot.RawData()] INFO, plotting raw digital data series\n")
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "Plotting all raw data of AirSensEUR Box", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            # Plot file name
            General.df <- DF$General[date >= input$Valid1[1] & date <= input$Valid1[2]]
            # storing graphical parameters
            op <- par(no.readonly = TRUE)
            par(mfrow = c(1, 1))
            par(mar   = c(0,0,0,0))
            # Restoring graphical parameters on exit of function
            on.exit(par(op))
            if (nrow(General.df) > 1) {
                # Selecting
                General.df <- DF$General[, .SD, .SDcols = names(General.df)[-c(which(names(General.df) %in% c("gpsTimestamp", "date_PreDelay", "altitude")),
                                            grep(pattern = paste0(c("Out.", "_volt", "_DV", "_modelled"), collapse = "|"), x = names(General.df)) )]]
                time_series_RawData <- data_frame_to_timeseries(General.df, tz = threadr::time_zone(General.df[["date"]][1]))
                # colour_vector <- threadr::ggplot2_colours(45)
                Names.to.plot <- names(General.df)[-which(names(General.df) == "date")]
                colour_vector <- c("red", "blue", "black", "green", "cornflowerblue", "chocolate4", "darkblue",
                                   "darkgoldenrod3", "darkorange", "darkolivegreen4", "goldenrod4", "darkred",
                                   "darkmagenta", "darkgreen", "darkcyan", "red", "blue", "black", "green",
                                   "cornflowerblue", "chocolate4", "darkblue", "darkgoldenrod3", "darkorange", "darkolivegreen4",
                                   "goldenrod4", "darkred")
                colour_vector <- colour_vector[1:length(Names.to.plot)]
                # make interactive time-series plot
                plot_RawData_list <- list() #initialize list
                if (length(Names.to.plot) <= 4) Height <- as.character(1/length(Names.to.plot) * 950) else Height <- as.character(100)
                for (i in seq_along(Names.to.plot)) {
                    ts_RawData <- time_series_RawData[[i]]
                    plot_RawData <- dygraph(ts_RawData, group = "Influx", height = Height, width = "100%") %>%
                        dySeries(label = Names.to.plot[i], color = colour_vector[i]) %>%
                        dyAxis("y", label = Names.to.plot[i]) %>%
                        dyOptions(labelsUTC = T) # plot in UTC
                    #dyOptions(useDataTimezone = TRUE) # do not use the local time zone
                    plot_RawData_list[[i]] <- plot_RawData  #add each element to list
                }
            } else cat("[shiny, Plot.RawData()] WARMING, no raw digital data to plot from df General.\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            if (input$SavePlot) {
                WDoutput <- file.path(input$Selected, "Verification_plots")
                filename_html <- filename_png <- file.path(WDoutput,paste0(AirSensEur.name(),"_Full_time_series_",
                                                                           format(min(General.df[["date"]], na.rm = TRUE),"%Y%m%d"),"_",
                                                                           format(max(General.df[["date"]], na.rm = TRUE),"%Y%m%d"),"temp.html"))
                filename_png <- file.path(WDoutput,paste0(AirSensEur.name(),"_Full_time_series_",
                                                          format(min(General.df[["date"]], na.rm = TRUE),"%Y%m%d"),"_",
                                                          format(max(General.df[["date"]], na.rm = TRUE),"%Y%m%d"),".png"))
                save_html(plot_Cov_list, filename_html)
                webshot(filename_html, file     = filename_png, cliprect = "viewport")
                # Update button save plot
                updateCheckboxInput(session, inputId = "SavePlot", value = FALSE)
            }
            progress$set(message = "[shiny, Plot.RawData()] INFO, all raw data of AirSensEUR Box", value = 1)
            on.exit(progress$close())
            # render the dygraphs objects using htmltools
            plot_RawData_list <- htmltools::tagList(plot_RawData_list)
            plot_RawData_list
        })
        # NavBar"Data Treatment", mainTabPanel "Retrieved",  ----
        output$Retrieved     <- renderPlot(Plot.Retrieved(), width = 'auto', height = 'auto')
        # NavBar"Data Treatment", Reactive Plot.Retrieved
        Plot.Retrieved       <- reactive({
            #   Plot Retrieved data, the plot of the retrieved data are added in directory Retrieved_data
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "[shiny, Plot.Retrieved()] INFO, Plotting the last retrieved data", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            op <- par(no.readonly = TRUE)
            par(mfrow = c(1, 1))
            # Restoring graphical parameters on exit of function
            on.exit(par(op))
            General.df <- DF$General
            if (!is.null(Download$Sensor$DateIN.General.prev)) {
                General.plot <- General.df[date > Download$Sensor$DateIN.General.prev, ]
            } else General.plot <- General.df
            General.plot <-  selectByDate(General.plot,start = input[[paste0("Out.Sens.Date", CalSet()$k)]][1], end = input[[paste0("Out.Sens.Date", CalSet()$k)]][2])
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            if (all(is.na(General.plot[,INFLUX()[[4]]]))  | nrow(General.plot) < 10 ) { # INFLUX()[[4]] : var.names.sens
                cat("[shiny, Plot.Retrieved()] ERROR, not enough newly downloaded sensor data, not plotting new data.\n")
                plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                text(1,1,"[shiny, Plot.Retrieved()] ERROR, not enough newly downloaded sensor data, not plotting new data.")
            } else {
                # checking if there are any pollutants only with NA that would create an error with timePlot
                if (!any(apply(General.plot[,INFLUX()[[4]]], 2, function(x) return(all(is.na(x))))) ) {
                    Names.to.plot <- names(General.df)[
                        -c(which(names(General.df) %in% c("date", "gpsTimestamp", "date_PreDelay", "altitude")),
                           grep(pattern = paste0(c("Out.", "_volt", "_DV", "_modelled"),
                                                 collapse = "|"),
                                x = names(General.df))
                        )]
                    timePlot(General.plot, pollutant = Names.to.plot, date.pad = TRUE, auto.text = FALSE, y.relation = "free",
                             key.columns = round(length(which(names(General.df) != "date"))/3), Key = TRUE, strip = FALSE, ylab = "", # Key is the column of legend and strip is the list of ylabels
                             main = paste0("Last retrieved raw sensor digital values and AQMS station values for ", AirSensEur.name() ))
                    # Saving plot if requested
                    if (input$SavePlot) {
                        WDoutput <- file.path(input$Selected, "Retrieved_plots")
                        dev.copy(png,
                                 filename = file.path(WDoutput,
                                                      paste0(AirSensEur.name(),"_Retrieved_",
                                                             format(min(General.plot$date, na.rm = TRUE),"%Y%m%d"),"_",
                                                             format(max(General.plot$date, na.rm = TRUE),"%Y%m%d"),".png")),
                                 res = 300
                        )
                        dev.off()
                        cat(paste0("[shiny, Plot.Retrieved()] INFO, ", AirSensEur.name(),"_Retrieved_",
                                   format(min(General.plot$date, na.rm = TRUE),"%Y%m%d"),"_",
                                   format(max(General.plot$date, na.rm = TRUE),"%Y%m%d"),".png saved in ", WDoutput, "\n" ))
                        updateCheckboxInput(session,
                                            inputId = "SavePlot",
                                            label = NULL,
                                            value = FALSE)
                    }
                } else cat("[shiny, Plot.Retrieved()] WARNING, sensors:"
                           ,names(which(apply(General.plot[,INFLUX()[[4]]], 2, function(x) return(all(is.na(x))))))
                           ," do not have new data and prevent from plotting the new data of other sensors")
            }
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            # Cleaning memory space
            remove(General.plot)
            progress$set(message = "[shiny, Plot.Retrieved()] INFO, Plotting the last retrieved data", value = 1)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
        })
        # Reactive ind.warm ----
        observeEvent({
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Apply.Warm", i)]]))
        },{
            # Reactive function to trigger for Warming time, temperature/humidity tolerance, negative reference values and invalid, DF$General
            # Warm$Forced is TRUE if Apply.Warm    is TRUE
            #
            # depends:
            #       list.gas.sensor()
            #       DF$General
            #       input$Apply.Warm
            if (any(unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Apply.Warm", i)]])))
            ) Warm$Forced <- TRUE else Warm$Forced <- FALSE
        },
        ignoreInit = TRUE,
        priority = 200
        )
        observeEvent(Warm$Forced, {
            # Flagging the sensor data for warming time
            # This dataTreatment can only works if boardTimeStamp exists, meaning only in InfluxData. It will not work with SOSData
            # depends:
            #   DF$General                 : reactive function passing a dataframe merging data from Influx, SOS and reference
            #   list.gas.sensor()         : character vector giving the gas compounds of sensors ("Carbon_monoxide", "Nitric_oxide","Nitrogen_dioxide" and "Ozone")
            #   input$Warming              : integer, number of hours of warming per sensors
            #   input$UserMins             : integer number of minutes to average raw data
            # output: a list of 4 character vectors, corresponding to sensors with row index of DF$General corresponding to warming time of sensor data,
            #       the names of the 4 elements are the ones of list.gas.sensor()   in the same order
            if (Warm$Forced) {
                # Create a Progress object
                progress <- shiny::Progress$new()
                # Make sure it closes when we exit this reactive, even if there's an error
                on.exit(progress$close())
                progress$set(message = "[shiny, ind.warm()] INFO,Setting the index of dates for sensor warming time", value = 0.33)
                cat("[shiny, ind.warm()]  INFO, Setting index of sensor data during warming period\n")
                # setting index for warming
                if (!is.null(DF$General[,"boardTimeStamp"]) ) { # use to be class(DF$General) == "data.frame"
                    # http://r.789695.n4.nabble.com/Replace-zeroes-in-vector-with-nearest-non-zero-value-td893922.html
                    # https://stackoverflow.com/questions/26414579/r-replacing-zeros-in-dataframe-with-last-non-zero-value
                    # replace everythin boardTimeStamp which does not change with NA so na.locf will works
                    # Index of boardtimeStamp similar for consecutive boardtimeStamp
                    Consecutive <- which(diff(DF$General$boardTimeStamp, lag = 1) == 0)
                    # Values of indexes whith previous values that are non consecutive (Re-start)
                    Re_start <- Consecutive[diff(Consecutive, lag = 1) > 1]
                    # Setting NA boardTimeStamp to the last non-NA boardTimeStamp
                    data.table::set(DF$General,  j = "boardTimeStamp", value = na.locf(DF$General[["boardTimeStamp"]], na.rm = FALSE, fromLast = FALSE))
                    # detecting when boardTimeStamp decreases suddenly (re-boot)
                    Re_boot <- which(diff(DF$General$boardTimeStamp, lag = 1) < 0)
                    # Combining Re_start and reboot
                    ind <- unique(c(Re_start, Re_boot))
                } else {
                    # This is for SOS
                    ind <- apply(DF$General[, list.gas.sensor(), with = FALSE  ], 1, function(i) !all(is.na(i)))
                    ind <- which(ind[2:length(ind)] & !ind[1:(length(ind) - 1 )])
                }
                ind = ind + 1
                # Adding the first switch-on
                ind <- c(1,ind)
                progress$set(message = "[shiny, ind.warm()] INFO, Setting the index of dates for sensor warming time", value = 0.66)
                for (n in seq_along(list.gas.sensor())) {
                    indfull <- integer(length(ind)*input[[paste0("Warming",n)]] * 60 / as.integer(input$UserMins))
                    # developing IndFull
                    for (i in seq_along(ind)) {
                        indfull[((i - 1) * input[[paste0("Warming",n)]]*60/as.integer(input$UserMins) + 1):((i)*input[[paste0("Warming",n)]]*60 / as.integer(input$UserMins))] <- ind[i]:(ind[i] + input[[paste0("Warming",n)]] * 60 / as.integer(input$UserMins) - 1)
                    }
                    # removing  indexes outside the number of rows of DF$General
                    indfull <- indfull[indfull <= length(DF$General[[list.gas.sensor()[n]]])]
                    if (exists("return.ind.warm")) return.ind.warm[[n]] <- indfull else return.ind.warm <- list(indfull)
                }
                names(return.ind.warm) <- list.gas.sensor()
                ind.warm$out <<- return.ind.warm
                # Setting TRh$Forced to TRUE to be sure that it is done before ind.Sens
                TRh$Forced <<- TRUE
                progress$set(message = "[shiny, ind.warm()] INFO, Setting the index of dates for sensor warming time", value = 1)
            }
        },
        priority = 195)
        # NavBar"Data Treatment", MainTabPanel "Warming" - "PlotFiltering"  ----
        output$Warming <- renderDygraph(Plot.Warming()) # if using base plots add , width = 'auto', height = 'auto'
        # NavBar"Data Treatment", Reactive Plot.Warming
        Plot.Warming   <- reactive({
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "[shiny, Plot.Warming()] INFO, Plotting raw data during warming in red dots", value = 0.5)
            # Preparing graphical parameters for the number of plots equals to the number of sensors
            op <- par(no.readonly = TRUE)
            par(mfrow = c(ceiling(length(list.name.sensor())/2), 2))
            # Restoring graphical parameters on exit of function
            on.exit(par(op))
            # Selecting date to be plotted
            General.df <- DF$General[date >= input[[paste0("Out.Sens.Date", CalSet()$k)]][1] & date <= input[[paste0("Out.Sens.Date", CalSet()$k)]][2] + 1]
            #General.DT <- data.table(DF$General, key = "date")[date >= input[[paste0("Out.Sens.Date", CalSet()$k)]][1] & date <= input[[paste0("Out.Sens.Date", CalSet()$k)]][2] + 1]
            cat("-----------------------------------------------------------------------------------\n")
            # checking the initial number of valid sensor values
            unlist(lapply(list.gas.sensor()[!is.na(list.name.sensor())], function(i) {
                cat(paste0("[shiny, Plot.Warming()] INFO, sensor ",
                           list.name.sensor()[match(x = i, table = list.gas.sensor())],
                           " starting with ", length(which(!is.na(General.df[[i]]))), " valid measurements between ",
                           format(input[[paste0("Out.Sens.Date", CalSet()$k)]][1],"%Y-%m-%d  %H:%M"), " and ",format(input[[paste0("Out.Sens.Date", CalSet()$k)]][2],"%Y-%m-%d  %H:%M"),".\n"))
            }))
            cat("-----------------------------------------------------------------------------------\n")
            # date in ind.warm$out : all dates even the invalid ones and the one valid that are not selected
            Index.warm     <- lapply(ind.warm$out, function(x) {DF$General$date[x]} )
            if (length(Index.warm) != 0) {
                # plotting discared data only one sensor at a time
                if (!is.null(ind.warm$out[[CalSet()$gas.sensor]])) {
                    # index of valid sensor corresponding to CalSet()$k
                    plot_warm <- GraphOut(date    = General.df[["date"]],
                                          y       = General.df[[CalSet()$gas.sensor]],
                                          Col     = "green",
                                          Ylab    = "Raw Sensor values",
                                          indfull = which(General.df[["date"]] %in% unlist(Index.warm[CalSet()$gas.sensor])),
                                          Title   = (paste0("Data invalidated during warming of sensor ", CalSet()$name.sensor,
                                                            " for ", isolate(input[[paste0("Warming",CalSet()$k)]]), " hours after each switch-on.")),
                                          Dygraphs = TRUE)
                } else {
                    cat(paste0("[shiny, Plot.Warming()] INFO, There is no warming data to discard for ", CalSet()$name.sensor, "\n"))
                    plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                    text(1,1,paste0("[shiny, Plot.Warming()] INFO, There is no warming data to discard for ", CalSet()$name.sensor))
                }
                cat(paste0("[shiny, Plot.Warming()] INFO, sensor ", CalSet()$name.sensor,
                           ", ",length(which(!is.na(General.df[, .SD, .SDcols = paste0("Out.Warm.",CalSet()$gas.sensor)]))),
                           " valid data after removing ",length(which(General.df[["date"]] %in% unlist(Index.warm[CalSet()$gas.sensor]))),
                           " values during ", isolate(input[[paste0("Warming", CalSet()$k)]])," hours of warming at each switch-on\n"))
                # Saving plot if requested
                if (input$SavePlot) {
                    dev.copy(png,filename = file.path(CalSet()$WDOutliers, paste0(AirSensEur.name(), "_", CalSet()$name.sensor, "_Warming.png")), res = 300 )
                    dev.off()
                    cat(paste0("[shiny] INFO, ", AirSensEur.name(), "_", CalSet()$name.sensor, "_Warming.png saved in ", CalSet()$WDOutliers, "\n" ))
                    # reset of CheckBox
                    updateCheckboxInput(session,  inputId = "SavePlot", value = FALSE)
                }
            } else {
                cat(paste0("[shiny, Plot.Warming()] INFO, There is no warming data to discard \n"))
                plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                text(1,1,paste0("[shiny, Plot.Warming()] INFO, There is no warming data to discard"))
            }
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            progress$set(message = "[shiny, Plot.Warming()] INFO, Plotting raw data during warming in red dots", value = 1)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            # Opening the Filtering TabSet for GUI consistency
            isolate({
                if (input$Treatments != "Sensors") updateTabsetPanel(session, inputId = "Treatments", selected = "Sensors")
            })
            if (length(Index.warm) != 0 && !is.null(ind.warm$out[[CalSet()$gas.sensor]]) ) return(plot_warm)
        })
        # Reactive ind.TRh ----
        # Flagging the sensor data for Temperature and Humidity
        observeEvent({
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Apply.TRh", i)]]))
        },{
            # Reactive function to trigger for temperature/humidity tolerance
            # TRh$Forced is TRUE if Apply.TRh     is TRUE
            #
            # depends:
            #       list.gas.sensor()
            #       DF$General
            #       input$Apply.TRh
            if (any(unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Apply.TRh", i)]])))
            ) TRh$Forced <- TRUE else TRh$Forced <- FALSE},
        ignoreInit = TRUE,
        priority = 190
        )
        observeEvent(TRh$Forced, {
            # Input:
            #   General.df           : dataframe General created by reactive function DF$General merfing influx, SOS and reference data.
            #   INFLUX()[[2]]        : charater vector with the name of meteorological parameters ("Temperature","Relative_humidity" and "Atmospheric_pressure")
            #   list.gas.sensor()   : charater vector with the name of sensor gas compounds ("Carbon_monoxide","Nitric_oxide","Nitrogen_dioxide" and "Ozone")
            # Output:                : list of NAs for discarded temperature and humidity with as many elements as in list.gas.sensor()
            #                          consisting of vector of integers of the index of rows of DF$General dataframe
            if (TRh$Forced) {
                # Create a Progress object
                progress <- shiny::Progress$new()
                progress$set(message = "[shiny, ind.TRh()] INFO, Setting the index temperature and Humidity out of tolerance", value = 0.5)
                cat("[shiny, ind.TRh()] INFO, Setting index of sensor data outside temperature and RH validity ranges\n")
                # Always starting detection of outleirs for T and RH from the dataframe set in DF$General
                index.temp <- which(colnames(DF$General) %in% INFLUX()[[2]][1])   # Temperature
                index.rh   <- which(colnames(DF$General) %in% INFLUX()[[2]][2])   # Humidity
                return.ind.TRh    <- list()
                return.ind.T.min  <- list()
                return.ind.T.max  <- list()
                return.ind.Rh.min <- list()
                return.ind.Rh.max <- list()
                #for (l in INFLUX()[[3]]) {
                for (l in list.gas.sensor()) {
                    # Global index of temperature/humidity exceeding thresholds
                    ind <- (DF$General[, index.temp, with = FALSE]   < input[[paste0("Temperature",match(x = l, table = list.gas.sensor()))]][1]  |
                                DF$General[, index.temp, with = FALSE]   > input[[paste0("Temperature",match(x = l, table = list.gas.sensor()))]][2]) |
                        (DF$General[, index.rh, with = FALSE]     < input[[paste0("Humidity"   ,match(x = l, table = list.gas.sensor()))]][1]  |
                             DF$General[, index.rh, with = FALSE]     > input[[paste0("Humidity"   ,match(x = l, table = list.gas.sensor()))]][2])
                    # Global index of temperature/humidity exceeding thresholds
                    T.min  <- DF$General[, index.temp, with = FALSE] < input[[paste0("Temperature",match(x = l, table = list.gas.sensor()))]][1]
                    T.max  <- DF$General[, index.temp, with = FALSE] > input[[paste0("Temperature",match(x = l, table = list.gas.sensor()))]][2]
                    Rh.min <- DF$General[, index.rh, with = FALSE]   < input[[paste0("Humidity"   ,match(x = l, table = list.gas.sensor()))]][1]
                    Rh.max <- DF$General[, index.rh, with = FALSE]   > input[[paste0("Humidity"   ,match(x = l, table = list.gas.sensor()))]][2]
                    # if (exists("return.ind.TRh")) {
                    return.ind.TRh[[Config$all[["sens2ref"]][which(gas.sensor == l), name.sensor]]] <- which(ind)
                    return.ind.T.min[[ paste0(Config$all[["sens2ref"]][which(gas.sensor == l), name.sensor],"__Temp. < ",input[[paste0("Temperature",match(x = l, table = list.gas.sensor()))]][1])]] <- which(T.min)
                    return.ind.T.max[[ paste0(Config$all[["sens2ref"]][which(gas.sensor == l), name.sensor],"__Temp. > ",input[[paste0("Temperature",match(x = l, table = list.gas.sensor()))]][2])]] <- which(T.max)
                    return.ind.Rh.min[[paste0(Config$all[["sens2ref"]][which(gas.sensor == l), name.sensor],"__RH < "   ,input[[paste0("Humidity"   ,match(x = l, table = list.gas.sensor()))]][1])]] <- which(Rh.min)
                    return.ind.Rh.max[[paste0(Config$all[["sens2ref"]][which(gas.sensor == l), name.sensor],"__RH > "   ,input[[paste0("Humidity"   ,match(x = l, table = list.gas.sensor()))]][2])]] <- which(Rh.max)
                }
                ind.TRh$out <<- list(ind.TRh = return.ind.TRh, T.min = return.ind.T.min, T.max = return.ind.T.max, Rh.min = return.ind.Rh.min, Rh.max = return.ind.Rh.max)
                # Setting Invalid$Forced to True to be sure that it is carried out before ind.sens
                Inv$Forced <- TRUE
                progress$set(message = "[shiny, ind.TRh()] INFO, Setting the index temperature and Humidity out of tolerance", value = 1)
                progress$close()
            }
        },
        priority = 185
        )
        # NavBar"Data Treatment", MainTabPanel "Temp&Humid" - "PlotFiltering"  ----
        output$Temp.Humid    <- renderDygraph(Plot.Temp.Humid()) # if using base plots add , width = 'auto', height = 'auto'
        # Reactive Plot.Temp.Humid
        Plot.Temp.Humid      <- reactive({
            # Plot NA for the sensor data out of temperature and RH validity ranges for each raw sensors
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "[shiny, Plot.Temp.Humid()] INFO, Plotting data out of temperature/humidity tolerance", value = 0.20)
            # Selecting date to be plotted
            General.df <- DF$General[date >= input[[paste0("Out.Sens.Date", CalSet()$k)]][1] & date <= input[[paste0("Out.Sens.Date", CalSet()$k)]][2] + 1]
            # Creating a list of dates with values out of tolerance of T and RH : T.min, T.max, Rh.min and Rh.maxin in ind.TRh$out
            Index.TRh <- lapply(ind.TRh$out$ind.TRh, function(x) {DF$General$date[x]} )
            T.min     <- lapply(ind.TRh$out$T.min  , function(x) {DF$General$date[x]} )
            T.max     <- lapply(ind.TRh$out$T.max  , function(x) {DF$General$date[x]} )
            Rh.min    <- lapply(ind.TRh$out$Rh.min , function(x) {DF$General$date[x]} )
            Rh.max    <- lapply(ind.TRh$out$Rh.max , function(x) {DF$General$date[x]} )
            cat("-----------------------------------------------------------------------------------\n")
            if (length(Index.TRh) != 0) {
                if (!is.null(Index.TRh[CalSet()$name.sensor])) {
                    ind = list(
                        T.min[[ names(T.min)[ grep(CalSet()$name.sensor, x = names(T.min))] ]],
                        T.max[[ names(T.max)[ grep(CalSet()$name.sensor, x = names(T.max))] ]],
                        Rh.min[[names(Rh.min)[grep(CalSet()$name.sensor, x = names(Rh.min))] ]],
                        Rh.max[[names(Rh.max)[grep(CalSet()$name.sensor, x = names(Rh.max))] ]]
                    )
                    names(ind) <-  c(unlist(strsplit(names(T.min)[  match(CalSet()$gas.sensor, list.gas.sensor())], split = "__"))[2],
                                     unlist(strsplit(names(T.max)[  match(CalSet()$gas.sensor, list.gas.sensor())], split = "__"))[2],
                                     unlist(strsplit(names(Rh.min)[ match(CalSet()$gas.sensor, list.gas.sensor())], split = "__"))[2],
                                     unlist(strsplit(names(Rh.max)[ match(CalSet()$gas.sensor, list.gas.sensor())], split = "__"))[2])
                    Plot.Temp.Humid <- GraphOut(date = General.df[["date"]],
                                                y       = General.df[[CalSet()$gas.sensor]],
                                                Col     = "green",
                                                Ylab    = "Raw Sensor values",
                                                indfull = ind,
                                                Title   = paste0("Data invalidated for temperature or humidity outside thresholds for sensor ", CalSet()$name.sensor),
                                                Dygraphs = TRUE
                    )
                    cat(paste0("[shiny, Plot.Temp.Humid()] INFO, for sensor ", CalSet()$name.sensor,": ",
                               length(which(!is.na(General.df[[paste0("Out.Warm.TRh.",CalSet()$gas.sensor)]]))),
                               " valid data after removing sensor warm up data and ",
                               length(which(General.df[["date"]] %in% unlist(Index.TRh[CalSet()$name.sensor]))),
                               " values outside temperature/humidity intervals\n"))
                } else{
                    cat(paste0("[shiny, Plot.Temp.Humid()] INFO, sensor ", CalSet()$name.sensor,
                               " was not used outside temperature or humidity validity ranges.\n"))
                    plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                    text(1,1,paste0("[shiny, Plot.Temp.Humid()] INFO, INFO, sensor ", CalSet()$name.sensor,
                                    " was not used outside temperature or humidity validity ranges."))
                }
                # Saving plot if requested
                if (input$SavePlot) {
                    dev.copy(png,filename = file.path(CalSet()$WDOutliers, paste0(AirSensEur.name(),"_TRh.png")),
                             #units = "cm",
                             #width = 35.55,
                             #height = 20,
                             res = 300
                    )
                    dev.off()
                    cat(paste0("[shiny] INFO, ", AirSensEur.name(),"_TRh.png saved in ", CalSet()$WDOutliers, "\n" ))
                    updateCheckboxInput(session, inputId = "SavePlot", value = FALSE)
                }
            } else {
                cat(paste0("[shiny, Plot.Temp.Humid()] INFO, no data outside temperature or humidity validity ranges."), sep = "\n")
                plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                text(1,1,paste0("[shiny, Plot.Temp.Humid()] INFO, no data outside temperature or humidity validity ranges."))
            }
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            progress$set(message = "[shiny, Plot.Temp.Humid()] INFO, Plotting data out of temperature/humidity tolerance", value = 1)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            # Opening the Filtering TabSet for GUI consistency
            isolate({
                if (input$Treatments != "Sensors")   updateTabsetPanel(session, inputId = "Treatments", selected = "Sensors")
            })
            return(Plot.Temp.Humid)
        })
        # NavBar"Data Treatment, MmainTabPanel "Neg.values" - "PlotFiltering" ----
        output$Neg.values    <- renderDygraph(Plot.Neg.values()) # with base plots use , width = 'auto', height = 'auto'
        # reactive Plot.Neg.values
        Plot.Neg.values      <- reactive({
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "[Shiny, Plot.Neg.values()] INFO, Plotting discarded negative reference data", value = 0)
            cat("-----------------------------------------------------------------------------------\n")
            # Inital count of reference
            cat(paste0("[Shiny]Plot.Neg.values, INFO, number of valid measurements for ", CalSet()$name.reference,
                       " before removing negative values: ", length(which(!is.na(DF$General[,CalSet()$name.reference])))), sep = "\n")
            # number index of reference in the list of references
            j <- match(x = CalSet()$name.reference , table = list.gas.reference2use())
            # Discarding negative values
            # negative values for selected pollutants
            ind.neg <- which(DF$General[,CalSet()$name.reference] < 0)
            # Plotting
            if (length(ind.neg) > 0) {
                progress$set(message = "[Shiny, Plot.Neg.values()] INFO, Plotting discarded negative reference data", value = 0.5)
                if (input[[paste0("rm.neg",j)]]) {
                    General.df <- DF$General[date >= input[[paste0("Out.Ref.Date",j)]][1] & date <= input[[paste0("Out.Ref.Date",j)]][2] + 1,
                                             .SD, .SDcols = c("date", CalSet()$name.reference )]
                    indfull <- which(General.df[["date"]] %in% DF$General$date[ind.neg])
                    if (length(indfull)) {
                        Plot.Neg.values <- GraphOut(date     = General.df[["date"]],
                                                    y        = General.df[[CalSet()$name.reference]],
                                                    Col      = "green",
                                                    Ylab     = "Reference values",
                                                    indfull  = indfull,
                                                    Title    = (paste0("Negative reference data invalidated for ", CalSet()$name.reference )),
                                                    Dygraphs = TRUE
                        )
                        cat(paste0("[Shiny]Plot.Neg.values, INFO, reference pollutant ", CalSet()$name.reference , " number of negative data data ",
                                   length(ind.neg), "\n"))
                    } else {
                        cat(paste0("[Shiny]Plot.Neg.values, INFO, no negative reference values in the selected period\n"))
                        plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                        text(1,1,paste0("[Shiny]Plot.Neg.values, INFO, no negative reference values  in the selected period\n"))
                        shinyalert(
                            title = "Info: no negative reference data",
                            text = "[Shiny]Plot.Neg.values, INFO, no negative reference values in the selected period",
                            closeOnEsc = TRUE,
                            closeOnClickOutside = TRUE,
                            html = FALSE,
                            type = "info",
                            showConfirmButton = TRUE,
                            showCancelButton  = FALSE,
                            confirmButtonText = "OK",
                            confirmButtonCol  = "#AEDEF4",
                            timer             = 2000,
                            imageUrl          = "",
                            animation         = FALSE
                        )
                    }
                } else {
                    cat(paste0("[Shiny]Plot.Neg.values, INFO, Removing negative values not requested for ", CalSet()$name.reference , "\n"))
                    plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                    text(1,1,paste0("[Shiny]Plot.Neg.values, INFO, Removing negative values not requested for ", CalSet()$name.reference , "\n"))
                    shinyalert(
                        title = "Info no negative data",
                        text = paste0("[Shiny]Plot.Neg.values, INFO, Removing negative values not requested for ", CalSet()$name.reference),
                        closeOnEsc = TRUE,
                        closeOnClickOutside = TRUE,
                        html = FALSE,
                        type = "info",
                        showConfirmButton = TRUE,
                        showCancelButton  = TRUE,
                        confirmButtonText = "OK",
                        confirmButtonCol  = "#AEDEF4",
                        timer             = 2000,
                        imageUrl          = "",
                        animation         = FALSE
                    )
                }
                # Saving plot if requested
                if (input$SavePlot) {
                    dev.copy(png, filename = file.path(CalSet()$WDOutliers, paste0(AirSensEur.name(),"_RefNeg.png")) ,
                             units = "cm",
                             #res = 300,
                             #width = 35.55,
                             #height = 20,
                             res = 300
                    )
                    dev.off()
                    cat(paste0("[shiny] INFO, ", AirSensEur.name(),"_RefNeg.png saved in ", CalSet()$WDOutliers, "\n" ))
                    updateCheckboxInput(session,
                                        inputId = "SavePlot",
                                        label = NULL,
                                        value = FALSE)
                }
            } else {
                cat(paste0("[Shiny]Plot.Neg.values, INFO, no negative reference values\n"))
                plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                text(1,1,paste0("[Shiny]Plot.Neg.values, INFO, no negative reference values\n"))
                shinyalert(
                    title = "INFO no negative data",
                    text = "[Shiny]Plot.Neg.values, INFO, no negative reference values",
                    closeOnEsc = TRUE,
                    closeOnClickOutside = TRUE,
                    html = FALSE,
                    type = "info",
                    showConfirmButton = TRUE,
                    showCancelButton  = FALSE,
                    confirmButtonText = "OK",
                    confirmButtonCol  = "#AEDEF4",
                    timer             = 3000,
                    imageUrl          = "",
                    animation         = FALSE
                )
            }
            # New count of reference values
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            progress$set(message = "[Shiny, Plot.Neg.values()] INFO, Plotting discarded negative reference data", value = 1)
            # # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            # Opening the Filtering TabSet for GUI consistency
            isolate({
                if (input$Treatments != "Reference") updateTabsetPanel(session, inputId = "Treatments", selected = "Reference")
            })
            if (input[[paste0("rm.neg",j)]] && length(ind.neg) > 0 &&
                !is.na(any(match(DF$General$date[ind.neg],General.df[["date"]])))) return(Plot.Neg.values)
        })
        # NavBar"Data Treatment", mainTabPanel "Invalid"-"PlotFiltering"  ,  ----
        output$Invalid.Sens    <- renderDygraph(Plot.Invalid.Sens()) # for base plot add , width = 'auto', height = 750
        # Reactive Invalid.DF
        Invalid.DF <- reactive({
            # depends on:
            #input$Selected
            #input$Sensors
            #ASE_name()
            #min.General.date()
            input$At.Ref
            nameFile <- file.path(input$Selected,"Configuration",paste0(AirSensEur.name(),"_Valid_",input$Sensors,".cfg"))
            if (file.exists(nameFile)) {
                DF <- read.table(file             = nameFile,
                                 header           = TRUE,
                                 row.names        = NULL,
                                 comment.char     = "#",
                                 stringsAsFactors = FALSE
                )
                DF <- dplyr::arrange(DF,In)
            } else {
                DF <- data.frame(In               = strftime(min.General.date()),
                                 End              = strftime(min.General.date()),
                                 Comments         = "Inital setting",
                                 stringsAsFactors = FALSE
                )
            }
            return(DF)
        })
        output$hot <- rhandsontable::renderRHandsontable({
            # converts Invalid.DF() to rhandsontable object
            if (!is.null(Invalid.DF())) rhandsontable::rhandsontable(Invalid.DF(), stretchH = "all")
        })
        ## Save
        observeEvent(input$Save.row.Valid, {
            # rows must be in increasing order before saving otherwise the following error stops the script:
            # Warning: Error in seq.default: 'by' must be of length 1
            finalDF <- hot_to_r(input$hot)
            finalDF <- dplyr::arrange(finalDF,In)
            nameFile <- file.path(input$Selected,"Configuration",paste0(AirSensEur.name(),"_Valid_",input$Sensors,".cfg"))
            write.table(finalDF, file = nameFile, row.names = FALSE)
        })
        observeEvent(input$At.Ref, {
            if (!is.null(DF$General) && nrow(DF$General) > 0 &&
                all(c("Ref.Lat", "Ref.Long") %in% names(DF$General))) {
                # Detecting ASE at/not at reference station
                # https://stackoverflow.com/questions/55752064/r-finding-closest-coordinates-between-two-large-data-sets
                At.Station   <- which(distHaversine(DF$General[, .(longitude, latitude)], DF$General[, .(Ref.Long,Ref.Lat)]) < 100)
                Away.Station <- which(!DF$General$date %in% DF$General$date[At.Station])
                # https://stackoverflow.com/questions/16118050/how-to-check-if-a-vector-contains-n-consecutive-numbers
                rle.At.Station   <- rle(diff(At.Station))
                rle.Away.Station <- rle(diff(Away.Station))
                Consecutive.Away.Station <- which(rle.Away.Station$values == 1)
                Cumsum.Away.Station      <- cumsum(rle.Away.Station$lengths) #
                IN.Away.Station  <- DF$General$date[c(1,Away.Station[Cumsum.Away.Station[Consecutive.Away.Station-1]+1])]
                END.Away.Station <- DF$General$date[Away.Station[Cumsum.Away.Station[Consecutive.Away.Station]]]
                Add.Away.Station <- data.frame(In               = strftime(IN.Away.Station),
                                               End              = strftime(END.Away.Station),
                                               Comments         = rep("AirSensEUR away from reference station", times = length(IN.Away.Station)),
                                               stringsAsFactors = FALSE)
                # checking that Invalid.DF() has column "Comments"
                if (!"Comments" %in% names(Invalid.DF())) {
                    Comments <- rep("Inital setting", times = nrow(Invalid.DF()))
                    Invalid.DF <- cbind(Invalid.DF(), Comments)
                } else Invalid.DF <- Invalid.DF()
                DF <- unique(rbindlist(list(Invalid.DF, Add.Away.Station)))
                DF <- setkey(DF,In)
                nameFile <- file.path(input$Selected,"Configuration",paste0(AirSensEur.name(),"_Valid_",input$Sensors,".cfg"))
                write.table(DF, file = nameFile, row.names = FALSE)
            } else {
                my_message <- "[shiny, At.Ref] ERROR, there are no reference data or no coordiantes of reference station\n"
                cat(my_message)
                shinyalert(
                    title = "ERROR no General data",
                    text = my_message,
                    closeOnEsc = TRUE,
                    closeOnClickOutside = TRUE,
                    html = FALSE,
                    type = "error",
                    showConfirmButton = TRUE,
                    showCancelButton  = FALSE,
                    confirmButtonText = "OK",
                    confirmButtonCol  = "#AEDEF4",
                    timer             = 3000,
                    imageUrl          = "",
                    animation         = FALSE)
            }
        },
        priority = 200)
        ## Delete
        observeEvent(input$Del.row.Valid, {
            # rows must be in increasing order before saving otherwise the following error stops the script:
            # Warning: Error in seq.default: 'by' must be of length 1
            nameFile <- file.path(input$Selected,"Configuration",paste0(AirSensEur.name(),"_Valid_",input$Sensors,".cfg"))
            if (file.exists(nameFile)) file.remove(nameFile)
            # Need to reset Invalid.DF  ################################################################################################
        })
        # Reactive ind.Invalid ----
        # Flagging the sensor data for Invalid ensor data
        observeEvent({
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Apply.Invalid", i)]]))
        },{
            # Reactive function to trigger for invalid, DF$General
            # Inv$Forced is TRUE if Apply.Invalid is TRUE
            # depends:
            #       list.gas.sensor()
            #       input$Apply.Invalid
            if (any(unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Apply.Invalid", i)]])))
            ) Inv$Forced <- TRUE else Inv$Forced <- FALSE
        },
        ignoreInit = TRUE,
        priority = 170
        )
        observeEvent(Inv$Forced, {
            # inputs:
            #   input$Apply.Invalid1,2,3,4  : crosscheckboxes requiring to check DF$General for warming  time
            #   DF$General                  : reactive function passing a dataframe merging data from Influx, SOS and reference
            #   list.gas.sensor()          : character vector giving the gas compounds of sensors ("Carbon_monoxide", "Nitric_oxide","Nitrogen_dioxide" and "Ozone")
            #   input$UserMins              : integer number of minutes to average raw data
            #
            # output: a list of 4 character vectors, corresponding to sensors with row index of DF$General corresponding to Invalidf sensor data,
            #       the names of the 4 elements are the ones of list.gas.sensor()   in the same order
            # Only if "apply filter" for Invalid data is selected
            if (Inv$Forced) {
                # Create a Progress object
                progress <- shiny::Progress$new()
                # Make sure it closes when we exit this reactive, even if there's an error
                on.exit(progress$close())
                progress$set(message = "Setting the index of dates for Invalid sensor data", value = 0.33)
                cat("[shiny, ind.Invalid()] INFO, Setting index of invalid sensor data\n")
                if (!is.null(DF$General)) { # DF$General
                    # reading the files with period of valid data
                    for (i in seq_along(list.name.sensor())) {
                        nameFile <- file.path(input$Selected,"Configuration",paste0(AirSensEur.name(),"_Valid_",list.name.sensor()[i],".cfg"))
                        if (file.exists(nameFile)) {
                            cat(paste0("[shiny, ind.Invalid()] INFO, the file with valid periods of sensor data ", nameFile, " exists "), sep = "\n")
                            assign(paste0("Valid_",list.name.sensor()[i]), read.table(file = nameFile, header = TRUE, row.names = NULL, comment.char = "#", stringsAsFactors = FALSE))
                            cat(paste0("[shiny, ind.Invalid()] INFO, ", get(paste0("Valid_",list.name.sensor()[i]))), sep = "\n")
                        } else {
                            # There are no Valid files. Creates files with IN = END = min(General$date)
                            cat(paste0("[shiny, ind.Invalid()] INFO, the files with valid periods of sensor data ", nameFile, " do not exist. Set validity to the whole available time interval"), sep = "\n")
                            assign(paste0("Valid_",list.name.sensor()[i]), rbind(c(strftime(min(DF$General$date, na.rm = TRUE)), strftime(min(DF$General$date, na.rm = TRUE)))))
                            write.table(x         = data.frame(In = gsub(" UTC", "",strftime(min.General.date())),
                                                               End = gsub(" UTC", "",strftime(min.General.date())),
                                                               stringsAsFactors = FALSE),
                                        file      = nameFile,
                                        row.names = FALSE
                            )
                        }
                    }
                    # Creating one list with invalid periods for all sensors
                    Valid <- list()
                    for (i in paste0("Valid_",list.name.sensor())) Valid[[i]] <- get(i)
                    # Function to convert charater strings to POSIX
                    NewValid <- function(x) {
                        # making each element a dataframe of POSIXct
                        x <- data.frame( x, stringsAsFactors = FALSE)
                        colnames(x) <- c("In", "End")
                        x$In  <- parse_date_time(x$In , tz = threadr::time_zone(DF$General$date[1]), orders = "YmdHMS")
                        x$End <- parse_date_time(x$End, tz = threadr::time_zone(DF$General$date[1]), orders = "YmdHMS")
                        return(x)
                    }
                    Valid.date <- lapply(Valid, NewValid)
                    cat("[shiny, ind.Invalid()] INFO, SET VALID TIME parameters\n")
                    # Set inital date for data retrieving (i.e. maximum length time period for data retrieval).
                    # These dates may change according to the data availability
                    # UserDateIN.0, SOS.TZ is set in ASEConfig_MG.R
                    # Set correct time zone
                    # list of invalid date to be used for sensor Evaluation with reference values, a kind of life cycle of each sensor - time zone shall be the same as SOS.TZ (UTC?)
                    # seting invalid to NA and create a list for plotting invalids
                    ind.Inval <- list()
                    for (i in gsub(pattern = "Valid_", replacement = "", names(Valid.date))) {
                        for (j in 1:nrow(Valid.date[[paste0("Valid_",i)]])) {
                            Valid.interval.j <- which(DF$General$date %within% interval(Valid.date[[paste0("Valid_",i)]]$In[j], Valid.date[[paste0("Valid_",i)]]$End[j]))
                            if (length(Valid.interval.j) > 0) {
                                if (!(i %in% names(ind.Inval))) {
                                    ind.Inval[[i]] <- DF$General$date[Valid.interval.j]
                                } else {
                                    ind.Inval[[i]] <- c(ind.Inval[[i]], DF$General$date[Valid.interval.j])
                                }
                            }
                        }
                    }
                }
                cat("-----------------------------------------------------------------------------------\n")
                ind.Invalid$out <<- list(Valid.date,ind.Inval)
                # make sure that Outliers.Sens$Forced is run after Invalid, to discard outliers again and to apply invalid and outliers to DF$General
                if (!Outliers.Sens$Forced) Outliers.Sens$Forced <<- TRUE
                progress$set(message = "Setting the index of dates for Invalid sensor data", value = 1)
            }
        },
        ignoreInit = TRUE,
        priority = 165
        )
        # Reactive Plot.Invalid.Sens ----
        Plot.Invalid.Sens      <- reactive({
            # Plotting Invalid data for sensor data before Outlier detection
            # depends: list.gas.sensor(), input$Sensors, list.name.sensor(), ,
            #          DF$General : dataFrame with invalidated data,
            #          ind.Invalid$out[[2]] : list with invalid date
            #          input[[paste0("Out.Sens.Date",k)]],input$Selected,
            #
            # isolate: input$Sensors
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "[Shiny, Plot.Inv()] INFO, Plotting discarded invalid sensor data", value = 0.2)
            # Selecting date to be plotted
            General.df <- DF$General[date >= input[[paste0("Out.Sens.Date", CalSet()$k)]][1] & date <= input[[paste0("Out.Sens.Date", CalSet()$k)]][2] + 1]
            cat("-----------------------------------------------------------------------------------\n")
            cat("[Shiny, Plot.Inv()] INFO, plotting invalid sensor data\n")
            # Checking if the list of invalid date is empty
            if (length(ind.Invalid$out[[2]]) != 0) {
                # plotting the data discarded for the selected sensor
                # checking that discarding of invalid data is requested
                if (input[[paste0("Sens.Inval.Out", CalSet()$k)]]) {
                    if (!is.null(ind.Invalid$out[[2]][[CalSet()$name.sensor]])) {
                        # Selecting the species associated with sensor in rows of ASE_name.cfg
                        Plot_invalid <- GraphOut(date    = General.df[["date"]],
                                                 y       = General.df[[CalSet()$gas.sensor]],
                                                 Col     = "green",
                                                 Ylab    = "Raw Sensor values",
                                                 indfull = which(General.df[["date"]] %in% ind.Invalid$out[[2]][[CalSet()$name.sensor]]),
                                                 Title   = paste0("Data invalidated, dates outside valid period, for sensor ", CalSet()$name.sensor),
                                                 Dygraphs = TRUE)
                    } else {
                        cat(paste0("[Shiny, Plot.Inv()] INFO, There is no Invalid data to discard for sensor ", CalSet()$name.sensor, "\n"))
                        plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                        text(1,1,paste0("[Shiny, Plot.Inv()] INFO, There is no invalid data to discard for sensor ", CalSet()$name.sensor))
                    }
                } else {
                    cat(paste0("[Shiny, Plot.Inv()] INFO, Discarding of invalid data not requested for sensor ", CalSet()$name.sensor,"\n"))
                    plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                    text(1,1,paste0("[Shiny, Plot.Inv()] INFO, Discarding of invalid data not requested for sensor ", CalSet()$name.sensor))
                }
                cat(paste0("[Shiny, Plot.Inv()] INFO, sensor ", CalSet()$name.sensor,
                           ", ",length(which(!is.na(DF$General[,paste0("Out.",CalSet()$gas.sensor)]))),
                           " valid data within validity period removing ",length(which(General.df[["date"]] %in% ind.Invalid$out[[2]][[CalSet()$name.sensor]])),
                           " values outside validity period.\n"))
                # Saving plot if requested
                if (input$SavePlot) {
                    dev.copy(png,
                             filename = file.path(CalSet()$WDOutliers, paste0(AirSensEur.name(),"_Invalid.png")),
                             #units = "cm",
                             #width = 35.55,
                             #height = 20,
                             res = 300
                    )
                    dev.off()
                    cat(paste0("[shiny] INFO, ", AirSensEur.name(),"_Invalid.png saved in ", CalSet()$WDOutliers, "\n" ))
                    updateCheckboxInput(session, inputId = "SavePlot", value = FALSE)
                }
            } else {
                my_message <- "[Shiny, Plot.Inv()] INFO, There is no invalid data to discard \n"
                shinyalert(
                    title = "INFO no sensor invalid data",
                    text = "[Shiny]Plot.Neg.values, INFO, no negative reference values",
                    closeOnEsc = TRUE,
                    closeOnClickOutside = TRUE,
                    html = FALSE,
                    type = "info",
                    showConfirmButton = TRUE,
                    showCancelButton  = FALSE,
                    confirmButtonText = "OK",
                    confirmButtonCol  = "#AEDEF4",
                    timer             = 3000,
                    imageUrl          = "",
                    animation         = FALSE
                )
            }
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            # Opening the Filtering TabSet for GUI consistency
            isolate({
                if (input$Treatments != "Sensors")   updateTabsetPanel(session, inputId = "Treatments", selected = "Sensors")
            })
            progress$set(message = "[Shiny, Plot.Inv()] INFO, Plotting discarded invalid sensor data", value = 1)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            if (length(ind.Invalid$out[[2]]) != 0 && input[[paste0("Sens.Inval.Out", CalSet()$k)]] &&
                !is.null(ind.Invalid$out[[2]][[CalSet()$name.sensor]])) return(Plot_invalid)
        })
        # NavBar"Data Treatment", mainTabPanel "Outliers" - "PlotFiltering", ----
        # Flagging outliers
        # Reactive ind.sens.out ----
        observeEvent({
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Apply.S.Out", i)]]))
            Inv$Forced
        },{
            # Reactive function to trigger detection of Outliers in sensor data
            # Outliers.Sens$Forced is TRUE if Warm$Forced | TRh$Forced | Inv$Forced is TRUE
            #                         if any Apply.S.Out is TRUE or
            #                         any names "Out.", "Out.Warm.", "Out.TRh.", "Out.Invalid.", "Out.Warm.TRh.", "Out.Warm.TRh.Inv." missing in
            # depends:
            #       list.gas.sensor()
            #       Warm$Forced | TRh$Forced | Inv$Forced
            #       input$Apply.S.Out
            if ( Warm$Forced || TRh$Forced || Inv$Forced ||
                 any(unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Apply.S.Out", i)]]))) ||
                 !any(grepl(pattern = paste0(c("Out.", "Out.Warm.", "Out.TRh.", "Out.Invalid.", "Out.Warm.TRh.", "Out.Warm.TRh.Inv."), collapse = "|"), x = names(DF$General)))
            ) Outliers.Sens$Forced <<- TRUE else Outliers.Sens$Forced <<- FALSE
        },
        priority = 150)
        observeEvent({Outliers.Sens$Forced},{
            # Return a list:
            #   - dataframe : name.sensor.iteration: date + for each sensor and each iteration : lowvalues, highvalues, outlierMin and Outliers max
            #                                         with values TRUE if sensor data are outliers and FALSE if not, + zmin and zmax the interval of tolerance for not being an outlier
            #   - dataframe : name.sensor.iteration: date + for each sensor and each iteration: TRUE if outliers and FALSE if not
            # Setting the index of outliers for sensors before calibration
            # Only if "apply filter" for outliers of sensors is selected
            if (Outliers.Sens$Forced) {
                progress <- shiny::Progress$new()
                progress$set(message = "[shiny, Outliers.Sens$Forced] INFO, purging for Warming, temperature/humidity tolerance and invalid", value = 0.10)
                on.exit(progress$close())
                cat("-----------------------------------------------------------------------------------\n")
                cat("[shiny, Outliers.Sens$Forced] INFO, purging for Warming, temperature/humidity tolerance and invalid\n")
                Tot.Iter <- sum(sapply(seq_along(list.gas.sensor()), function(x) as.numeric(input[[paste0("Sens.iterations",x)]])))
                rate     <- 1/(Tot.Iter + 2)
                ValueRate <- rate
                progress$set(message = "[shiny, Outliers.Sens$Forced] INFO, Setting index of outliers in sensor data", value = ValueRate)
                cat("[shiny, Outliers.Sens$Forced] INFO, detecting row indexes of outliers in sensor data\n")
                if (!is.null(DF$General)) setalloccol(DF$General)
                for (i in list.gas.sensor()) {
                    # Checking if sensor data exists in DF$General
                    if (i %in% names(DF$General)) {
                        # Initialisation of columns of DF$General
                        Sensor.i <- na.omit(Config$all[["sens2ref"]][[which(Config$all[["sens2ref"]][,"gas.sensor"] == i),"name.sensor"]])
                        # resetting to initial values
                        progress$set(message = "[shiny, Outliers.Sens$Forced] INFO, Initialising filtered data columns", value = 0.165)
                        cat("[shiny, Outliers.Sens$Forced] INFO, Initialising filtered data columns for ", i, "\n")
                        Vector.columns <- paste0(c("Out.", "Out.Warm.", "Out.TRh.", "Out.Invalid.", "Out.Warm.TRh.", "Out.Warm.TRh.Inv."),i)
                        set(DF$General, j = Vector.columns, value = rep(list(DF$General[[i]]), times = length(Vector.columns)))
                        progress$set(message = "[shiny, Outliers.Sens$Forced] INFO, Discarding sensor data for Warming", value = 0.33)
                        cat("[shiny, Outliers.Sens$Forced] INFO, Discarding sensor data for Warming for ", i, "\n")
                        if (!is.null(ind.warm$out[i][[1]])) {
                            Vector.columns <- paste0(c("Out.", "Out.Warm.", "Out.Warm.TRh.", "Out.Warm.TRh.Inv."),i)
                            i.Rows <- as.integer(ind.warm$out[[i]])
                            set(DF$General, i = i.Rows, j = Vector.columns, value = rep(list(rep(NA, times = length(i.Rows))), times = length(Vector.columns)))
                        }
                        progress$set(message = "[shiny, Outliers.Sens$Forced] INFO, Discarding data outside temperature and RH validity ranges", value = 0.66)
                        cat("[shiny, Outliers.Sens$Forced] INFO, Excluding sensor data outside temperature and RH validity ranges for ", i, "\n")
                        if (!is.null(ind.TRh$out$ind.TR[[Sensor.i]]) && length(ind.TRh$out$ind.TR[[Sensor.i]]) > 0) {
                            Vector.columns <- paste0(c("Out.", "Out.TRh.", "Out.Warm.TRh.", "Out.Warm.TRh.Inv."),i)
                            set(DF$General,i = ind.TRh$out$ind.TRh[Sensor.i][[1]], j = Vector.columns,
                                value = rep(list(rep(NA, times = length(ind.TRh$out$ind.TRh[Sensor.i][[1]]))), times = length(Vector.columns)))
                        }
                        progress$set(message = "[shiny, Outliers.Sens$Forced] INFO, Excluding invalid sensor data", value = 0.85)
                        cat("[shiny, Outliers.Sens$Forced] INFO, Excluding invalid sensor data for ", i, "\n")
                        if (!is.null(ind.Invalid$out[[2]][[Sensor.i]]) && length(ind.Invalid$out[[2]][[Sensor.i]]) > 0) {
                            Vector.columns <- paste0(c("Out.", "Out.Invalid." , "Out.Warm.TRh.Inv."),i)
                            set(DF$General,i = which(DF$General$date %in% ind.Invalid$out[[2]][[Sensor.i]]), j = Vector.columns,
                                value = rep(list(rep(NA, times = length(which(DF$General$date %in% ind.Invalid$out[[2]][[Sensor.i]])))), times = length(Vector.columns)))
                        }
                        progress$set(message = "[shiny, Outliers.Sens$Forced] INFO, initialising outlier sensor data", value = 1.0)
                        cat("[shiny, Outliers.Sens$Forced] INFO, initialising outlier sensor data for ", i, "\n")
                        # index (1, 2,3, 4  or 1,2,3, 6 ... comng from  selection of control uiFiltering, Calib and SetTime)
                        k <- match(x = i, table = list.gas.sensor())
                        set(DF$General, j = paste0("Out.",i,".",1:input[[paste0("Sens.iterations", k)]]),
                            value = rep(list(DF$General[[paste0("Out.",i)]]), times = input[[paste0("Sens.iterations", k)]]))
                        # deleting bigger iterations
                        j  <- input[[paste0("Sens.iterations", k)]]
                        repeat (
                            if (any(grepl(pattern = paste0("Out.",i,".", j + 1)      , x = names(DF$General)))) {
                                set(DF$General, j = paste0("Out.",i,".",j + 1), value = NULL)
                                j <- j + 1
                            } else break # leaving the Repeat if there are no higher iterations
                        )
                        if (input[[paste0("Sens.rm.Out",k)]]) {
                            for (j in 1:input[[paste0("Sens.iterations",k)]]) { # number of iterations
                                ValueRate <- ValueRate + rate
                                progress$set(message = "[shiny, Outliers.Sens$Forced] INFO, Setting index of outliers in sensor data", value = ValueRate)
                                if (all(is.na(DF$General[[i]]))) {
                                } else {
                                    # Setting the columns of sensor data previous to detect outliers
                                    Y <- DF$General[[paste0("Out.Warm.TRh.Inv.",i)]]
                                    # setting Y for the outliers of previous iterations to NA. If null then stop outlier detection
                                    if (j > 1) {
                                        if (length(which(return.ind.sens.out[[paste0(i,".",(j - 1))]]$Outliers)) != 0) {
                                            if (class(Y)[1] == "tbl_df") {
                                                Y[as.numeric(paste(unlist(sapply(return.ind.sens.out[c(paste0(i,".",1:(j - 1)))],function(x) which(x$Outliers))))),] <- NA
                                            } else Y[as.numeric(paste(unlist(sapply(return.ind.sens.out[c(paste0(i,".",1:(j - 1)))],function(x) which(x$Outliers)))))] <- NA
                                        } else break
                                    }
                                    cat(paste0("[shiny, Outliers.Sens$Forced] INFO, sensor: ",i,", iteration: ",j,"\n"))
                                    Outli <- My.rm.Outliers(ymin         = input[[paste0("Sens.Ymin",k)]],
                                                            ymax         = input[[paste0("Sens.Ymax",k)]],
                                                            ThresholdMin = input[[paste0("Sens.ThresholdMin",k)]],
                                                            date         = DF$General[["date"]],
                                                            y            = Y,
                                                            window       = input[[paste0("Sens.window"   ,k)]],
                                                            threshold    = input[[paste0("Sens.threshold",k)]],
                                                            plotting     = FALSE
                                    )
                                    nameInd      <- paste0(i,".",j)
                                    OutlinameInd <- paste0(i,".",j,".Outli")
                                    assign(nameInd , data.frame(date = Outli$date, Outliers = apply(Outli[,c("Low_values","High_values","OutliersMin","OutliersMax")], 1, any), stringsAsFactors = FALSE))
                                    if (exists("return.ind.sens.out")) return.ind.sens.out[[nameInd]] <- get(nameInd) else {
                                        return.ind.sens.out <- list(get(nameInd)); names(return.ind.sens.out) <- nameInd
                                    }
                                    return.ind.sens.out[[OutlinameInd]] <- Outli
                                }
                                # Discarding outliers if requested for the compound
                                progress$set(message = "[shiny, Outliers.Sens$Forced] INFO, Setting outlier values for sensors to NA", value = ValueRate)
                                # Discading outliers
                                if (any(names(ind.sens$out) %in% paste0(i,".",j), na.rm = TRUE)) {
                                    set(DF$General,i = which(return.ind.sens.out[[paste0(i,".",j)]]$Outliers), j = paste0("Out."      ,i),
                                        value = list(rep(NA, times = length(which(return.ind.sens.out[[paste0(i,".",j)]]$Outliers)))))
                                    set(DF$General,i = which(return.ind.sens.out[[paste0(i,".",j)]]$Outliers), j = paste0("Out.",i,".",j),
                                        value = list(rep(NA, times = length(which(return.ind.sens.out[[paste0(i,".",j)]]$Outliers)))))
                                }
                            }
                        }
                    }
                }
                if (exists("return.ind.sens.out"))  ind.sens$out <<- return.ind.sens.out
                # reseting return.ind.sens.out
                if (exists("return.ind.sens.out")) rm(return.ind.sens.out)
                progress$set(message = "[shiny, Outliers.Sens$Forced] INFO, Setting index of outliers in sensor data", value = 1)
            }
        },
        priority = 145
        )
        # Reactive ind.ref.out ----
        observeEvent({
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("rm.neg", i)]]))
        },{
            # Reactive function to trigger detection of Outliers in referencer data with or without negative reference data discarded
            # Neg$Forced is TRUE if Apply$rm.neg changes
            # depends:
            #       Apply$rm
            Neg$Forced <<- TRUE
        },
        priority = 151)
        observeEvent({
            unlist(sapply(seq_along(list.gas.reference()), function(i) input[[paste0("Apply.R.Out", i)]]))
            Neg$Forced
        },{
            # Reactive function to trigger detection of Outliers in Reference data
            # Outliers.Ref$Forced is TRUE if Neg$Forced is TRUE
            #                             if Apply.R.Out is TRUE
            # depends:
            #       list.gas.reference2use()
            #       Neg$Forced
            #       list.gas.reference()
            #       input$Apply.R.Out
            if ( Neg$Forced ||
                 any(unlist(sapply(seq_along(list.gas.reference2use()), function(i) input[[paste0("Apply.R.Out", i)]]))) ||
                 !any(grepl(pattern = paste0(c("Out.Neg."), collapse = "|"), x = names(DF$General)))
            ) Outliers.Ref$Forced <<- TRUE else Outliers.Ref$Forced <<- FALSE
        },
        ignoreInit = TRUE,
        priority = 140)
        observeEvent(Outliers.Ref$Forced,{
            # Return a list:
            #   - dataframe : name.sensor.iteration: date + for each sensor and each iteration : lowvalues, highvalues, outlierMin and Outliers max
            #                                         with values TRUE if outliers and FALSE if not, + zmin and zmax the interval of tolerance for not being an outlier
            #   - dataframe : name.sensor.iteration: date + for each sensor and each iteration: TRUE if outliers and FALSE if not
            # depends:
            # isolates:
            # Only if "apply filter" for warming or temperature/humidity is selected
            if (Outliers.Ref$Forced) {
                # Create a Progress object
                progress <- shiny::Progress$new()
                Tot.Iter <- sum(sapply(seq_along(list.gas.reference2use()), function(x) as.numeric(input[[paste0("Ref.iterations",x)]])))
                rate     <- 1/(Tot.Iter + 2)
                ValueRate <- rate
                progress$set(message = "[shiny, ind.ref.out()] INFO, Setting index of outliers in reference data", value = ValueRate)
                on.exit(progress$close())
                # list of index of negative values
                ################################ ADD a Test to check that all reference parameters exists ######################
                #ind.neg <- apply(X = DF$General[,list.gas.reference2use(), with = FALSE], MARGIN = 2, function(x) {as.vector(which(x < 0))})
                ind.neg <- apply(X = DF$General[, .SD, .SDcols = list.gas.reference2use()[list.gas.reference2use() %in% names(DF$General)]], MARGIN = 2, function(x) {which(x < 0)})
                for (i in list.gas.reference2use()  ) {
                    # resetting to initial values
                    progress$set(message = "[shiny, ind.ref()] INFO, Initialising filtered reference data columns", value = 0.33)
                    cat("[shiny, ind.ref()] INFO, Initialising filtered reference data columns for ", i, "\n")
                    Vector.columns <- paste0(c("Out.", "Out.Neg."),i)
                    DF$General[,(Vector.columns) := rep(list(DF$General[[i]]), times = length(Vector.columns))]# discarding negative values if needed
                    # number index of reference pollutant in the list of references
                    k <- match(x = i, table = list.gas.reference2use())
                    if (input[[paste0("rm.neg",k)]]) {
                        if (exists("ind.neg") && length(ind.neg) > 0 && length(ind.neg[[i]]) > 0) {
                            progress$set(message = "[shiny, ind.ref()] INFO, Discarding sensor data for reference negative values", value = 0.66)
                            cat("[shiny, ind.ref()] INFO, Discarding sensor data for reference negative values\n")
                            set(DF$General,i = ind.neg[[i]], j = Vector.columns, value = rep(list(rep(NA, times = length(ind.neg[[i]]))), times = length(Vector.columns)))
                        }
                    }
                    progress$set(message = "[shiny, ind.ref()] INFO, initialising outlier sensor data", value = 1.0)
                    cat("[shiny, ind.ref()] INFO, initialising outlier sensor data for ", i, "\n")
                    DF$General[,(paste0("Out.",i,".",1:input[[paste0("Ref.iterations", k)]])) := rep(list(DF$General[[paste0("Out.Neg.",i)]]), times = input[[paste0("Ref.iterations", k)]])]
                    # discarding negative values if needed
                    
                    # deleting bigger iterations
                    j  <- input[[paste0("Ref.iterations", k)]]
                    repeat (
                        if (any(grepl(pattern = paste0("Out.",i,".", j + 1)      , x = names(DF$General)))) {
                            set(DF$General, j = paste0("Out.",i,".",j + 1), value = NULL)
                            j <- j + 1
                        } else break # leaving the Repeat if there are no higher iterations
                    )
                    # Index of outliers for reference data
                    cat("[shiny, ind.ref.out()] INFO, detecting row indexes of outliers in reference data for ", i, "\n")
                    if (input[[paste0("Ref.rm.Out", k)]]) {
                        for (j in 1:input[[paste0("Ref.iterations",k)]]) { # numver of iterations
                            ValueRate <- ValueRate + rate
                            progress$set(message = "[shiny, ind.ref.out()] INFO, Setting index of outliers in reference data", value = ValueRate)
                            if (i %in% names(DF$General)) {
                                if (all(is.na(DF$General[[i]]))) {
                                } else {
                                    Y <- DF$General[[paste0("Out.Neg.",i)]]
                                    # setting the outliers of previous iterations to NA. If null then stop outlier detection
                                    if (j > 1) {
                                        if (length(which(return.ind.ref.out[[paste0(i,".",(j - 1))]]$Outliers)) != 0) {
                                            Y[as.numeric(paste(unlist(sapply(return.ind.ref.out[c(paste0(i,".",1:(j - 1)))], function(x) which(x$Outliers)))))] <- NA
                                        }  else break
                                    }
                                    cat(paste0("[shiny, ind.ref.out()] sensor: ",i,", iteration: ",j,"\n"))
                                    Outli <- My.rm.Outliers(ymin         = input[[paste0("Ref.Ymin",k)]],
                                                            ymax         = input[[paste0("Ref.Ymax",k)]],
                                                            ThresholdMin = input[[paste0("Ref.ThresholdMin",k)]],
                                                            date         = DF$General[["date"]],
                                                            y            = Y,
                                                            window       = input[[paste0("Ref.window"   ,k)]],
                                                            threshold    = input[[paste0("Ref.threshold",k)]],
                                                            plotting     = FALSE
                                    )
                                    nameInd      <- paste0(i,".",j)
                                    OutlinameInd <- paste0(i,".",j,".Outli")
                                    assign(nameInd , data.frame(date = Outli$date,
                                                                Outliers = unlist(apply(Outli[,c("Low_values","High_values","OutliersMin","OutliersMax")],
                                                                                        MARGIN = 1,
                                                                                        function(x) any(x))),
                                                                stringsAsFactors = FALSE))
                                    if (exists("return.ind.ref.out")) return.ind.ref.out[[nameInd]] <- get(nameInd) else {
                                        return.ind.ref.out <- list(get(nameInd))
                                        names(return.ind.ref.out) <- nameInd
                                    }
                                    return.ind.ref.out[[OutlinameInd]] <- Outli
                                    # Discarding outliers
                                    if (any(names(return.ind.ref.out) %in% paste0(i,".",j), na.rm = TRUE)) {
                                        set(DF$General,i = which(return.ind.ref.out[[paste0(i,".",j)]]$Outliers), j = paste0("Out."      ,i),
                                            value = list(rep(NA, times = length(which(return.ind.ref.out[[paste0(i,".",j)]]$Outliers)))))
                                        set(DF$General,i = which(return.ind.ref.out[[paste0(i,".",j)]]$Outliers), j = paste0("Out.",i,".",j),
                                            value = list(rep(NA, times = length(which(return.ind.ref.out[[paste0(i,".",j)]]$Outliers)))))
                                    }
                                }
                            } else cat("[Shiny, ind.ref.out()] ERROR, Warning no reference values impossible to discard outliers\n")
                            # Discarding outliers if requested for the compound
                            progress$set(message = "[shiny, Outliers.Sens$Forced] INFO, Setting outlier values for reference data to NA", value = ValueRate)
                        }
                    }
                }
                if (exists("return.ind.ref.out")) ind.ref$out <<- return.ind.ref.out
                # reseting return.ind.ref.out
                if (exists("return.ind.ref.out")) rm(return.ind.ref.out)
                progress$set(message = "[shiny, ind.ref.out()] INFO, Setting index of outliers in reference data", value = 1)
            }
        },
        priority = 135)
        # NavBar"Data Treatment", mainTabPanel "Sens.Outliers" - "PlotFiltering" ----
        output$Sens.Outliers <- renderDygraph(Plot.Sens.Outliers()) # if base plot is used add , width = 'auto', height = 'auto'
        # Reactive FUN Plot.Sens.Outliers
        Plot.Sens.Outliers   <- reactive({
            # Plotting outliers for sensor data before calibration
            # depends: list.gas.sensor(), input$Sensors, list.name.sensor(), , DF$General, input[[paste0("Out.Sens.Date",k)]],input$Selected,
            #
            # isolate: number of iterations, "Treatments", "Filtering.Sensors"
            progress <- shiny::Progress$new()
            progress$set(message = "[Shiny, Plot.Sens.Outliers()], INFO, plotting outliers of sensor data", value = 0.10)
            cat("-----------------------------------------------------------------------------------\n")
            cat("[Shiny, Plot.Sens.Outliers()], INFO, plotting outliers of sensor data\n")
            # Selecting dates in DF$General ; created for the data series and selected for date for plotting
            General.df <- DF$General[date >= input[[paste0("Out.Sens.Date",CalSet()$k)]][1] & date <= input[[paste0("Out.Sens.Date",CalSet()$k)]][2] + 1]
            op <- par(no.readonly = TRUE)
            # number of plot according to the number of iterations
            if (isolate(input[[paste0("Sens.iterations",CalSet()$k)]]) == 1) par(mfrow = c(1,1)) else {par(mfrow = c(ceiling(isolate(input[[paste0("Sens.iterations",CalSet()$k)]])/2), 2))}
            # Restoring graphical parameters on exit of function
            on.exit(par(op))
            if (input[[paste0("Sens.rm.Out",CalSet()$k)]]) {
                for (j in 1:isolate(input[[paste0("Sens.iterations",CalSet()$k)]])) { # number of iterations
                    if (all(is.na(General.df[[CalSet()$gas.sensor]]))) {
                        cat(paste0("[Shiny, Plot.Sens.Outliers()], ERROR, All data sensor for ", input$Sensors, " are NAs, cannot filter outliers"), sep = "\n")
                        plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                        text(1,1,paste0("[Shiny, Plot.Sens.Outliers()], ERROR, All outlier data sensor for ", input$Sensors, " are NAs, cannot filter outliers"))
                    } else {
                        # Checking if we have the data frame of outliers for in General.df, maybe there is no because there was no outliers. In this case next j.
                        if (!any(grepl(pattern = paste0("Out.",CalSet()$gas.sensor,".",j), x = objects(General.df)))) {
                            cat(paste0("[Shiny, Plot.Sens.Outliers()], INFO, There is no outliers to detect for ", input$Sensors, " iteration ", j, "\n"))
                            plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                            text(1,1,paste0("[Shiny, Plot.Sens.Outliers()], INFO, There is no outliers to detect for ", input$Sensors, " iteration ", j, "\n"))
                            next
                        } else {
                            cat(paste0("[Shiny, Plot.Sens.Outliers()], Plotting the outliers for ",input$Sensors, " iteration ", j, "\n"))
                            if (j == 1) {
                                Y = General.df[[paste0("Out.Warm.TRh.Inv.",CalSet()$gas.sensor)]]
                            } else{
                                Y = General.df[[paste0("Out.",CalSet()$gas.sensor,".",j - 1)]] # we need to plot the data of j-1 iterations and add the points of outliers
                            }
                            # Date
                            Date <- General.df[["date"]]
                            # Selecting ind.sens$out for sensor input$Sensors; created for the selected dates for plotting
                            ind.sens.out   <- ind.sens$out[[paste0(CalSet()$gas.sensor,".",j,".Outli")]] %>%
                                filter(date >= input[[paste0("Out.Sens.Date",CalSet()$k)]][1] & date <= input[[paste0("Out.Sens.Date",CalSet()$k)]][2] + 1)
                            ind.sens.out.n <- ind.sens$out[[paste0(CalSet()$gas.sensor,".",j)]] %>%
                                filter(date >= input[[paste0("Out.Sens.Date",CalSet()$k)]][1] & date <= input[[paste0("Out.Sens.Date",CalSet()$k)]][2] + 1)
                            plot_outli <- My.rm.Outliers(ymin         = input[[paste0("Sens.Ymin",CalSet()$k)]],
                                                         ymax         = input[[paste0("Sens.Ymax",CalSet()$k)]],
                                                         ThresholdMin = input[[paste0("Sens.ThresholdMin",CalSet()$k)]],
                                                         date         = Date,
                                                         y            = Y,
                                                         window       = input[[paste0("Sens.window",CalSet()$k)]],
                                                         threshold    = input[[paste0("Sens.threshold",CalSet()$k)]],
                                                         ind          = ind.sens.out,
                                                         plotting     = TRUE,
                                                         set.Outliers = FALSE,
                                                         Title        = paste0("Outliers for ", CalSet()$gas.sensor, " , iteration ",j),
                                                         Dygraphs = TRUE)
                            cat(paste0("[Shiny, Plot.Sens.Outliers()], INFO, sensor for ", input$Sensors,
                                       ", number of valid measurements after removing outliers ",
                                       length(which(!is.na(General.df[[paste0("Out.",CalSet()$gas.sensor)]]))),". Number of outliers: ",
                                       length(which(ind.sens.out.n$Outliers)) , "\n")) ################################################ Add plotting dates   ###################################################C
                            # Opening the Filtering TabSet for GUI consistency
                            isolate({
                                updateTabsetPanel(session, inputId = "Treatments" , selected = "Sensors")
                            })
                        }
                    }
                }
                # Saving plot if requested
                if (input$SavePlot) {
                    dev.copy(png,
                             filename = file.path(CalSet()$WDOutliers, paste0(AirSensEur.name(),"_Outliers_",input$Sensors,"_",j,".png")),
                             #          units = "cm",
                             #          width = 35.55,
                             #          height = 20
                             res = 300
                    )
                    dev.off()
                    cat(paste0("[shiny] INFO, ", AirSensEur.name(), "_Outliers_", input$Sensors, "_", j, ".png saved in ", CalSet()$WDOutliers, "\n" ))
                    updateCheckboxInput(session, inputId = "SavePlot", value = FALSE)
                }
            } else {
                cat(paste0("[Shiny, Plot.Sens.Outliers()], INFO, discarding of outliers not requested for ", list.name.sensor()[CalSet()$k], " in the Graphical User Interface "), sep = "\n")
                plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                text(1,1,paste0("[Shiny, Plot.Sens.Outliers()], INFO, discarding of outliers not requested for ", list.name.sensor()[CalSet()$k], " in the Graphical User Interface "))
            }
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            on.exit(progress$close())
            return(plot_outli)
        })
        # NavBar"Data Treatment", mainTabPanel "Ref.Outliers" - "PlotFiltering", ----
        output$Ref.Outliers  <- renderDygraph(Plot.Ref.Outliers()) # if base plot is used add  , width = 'auto', height = 'auto'
        # Reactive FUN Plot.Ref.Outliers
        Plot.Ref.Outliers    <- reactive({
            # depends: Config(), input$Filtering.References,input[[paste0("Ref.iterations",k)]], input[[paste0("Ref.rm.Out",k)]], input[[paste0("Out.Ref.Date",k)]]
            # isolate: number of iterations
            # index 1,2,3,4, 5, 6 of the reference gas selected in uiFiltering corresponding of rows of ASE_name.cfg
            k <-  match(x = input$Filtering.References, table = list.gas.reference())
            # Selecting gas.reference2use corresponding to input$Filtering.References selected
            i <-  list.gas.reference2use()[k]
            # checking that the data of Reference pollutant exists
            if (i %in% list.gas.reference2use()) {
                # Executing DF$General and ind.ref$out; created for the data series and selected for date for plotting
                General.df <- DF$General[date >= input[[paste0("Out.Ref.Date",k)]][1] & date <= input[[paste0("Out.Ref.Date",k)]][2] + 1]
                # Flagging outliers for reference data and all sensors
                op <- par(no.readonly = TRUE)
                # number of plot according to the number of iterations
                if (isolate(input[[paste0("Ref.iterations",k)]]) == 1) {
                    par(mfrow = c(1, 1))
                } else {
                    par(mfrow = c(ceiling(isolate(input[[paste0("Ref.iterations",k)]]) / 2), 2))
                }
                # Restoring graphical parameters on exit of function
                on.exit(par(op))
                cat("\n")
                cat("-----------------------------------------------------------------------------------\n")
                cat("[Shiny, Plot.Ref.Outliers()] INFO, plotting outliers of reference data\n")
                if (isolate(input[[paste0("Ref.rm.Out",k)]]) ) {
                    for (j in 1:isolate(input[[paste0("Ref.iterations",k)]])) { # numver of iterations
                        if (all(is.na( General.df[[i]]))) {
                            cat(paste0("[Shiny, Plot.Ref.Outliers()] ERROR All new reference data for ", input$Filtering.References, " are empty; cannot filter outliers"), sep = "\n")
                            plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                            text(1,1,paste0("[Shiny, Plot.Ref.Outliers()] ERROR All new reference data for ", input$Filtering.References, " are empty; cannot filter outliers"))
                        } else {
                            # Checking if we have the data frame of outliers for in General.df, maybe there is no because there was no outliers. In this case next j.
                            if (!any(grepl(pattern = paste0("Out.",i,".",j), x = objects(General.df)))) {
                                cat(paste0("[Shiny, Plot.Ref.Outliers()], INFO, There is no outliers to detect for ", input$Filtering.References, " iteration ", j, "\n"))
                                plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                                text(1,1,paste0("[Shiny, Plot.Ref.Outliers()] INFO, There is no outliers to detect for ", input$Filtering.References, " iteration ", j, "\n"))
                                next
                            } else {
                                cat(paste0("[Shiny, Plot.Ref.Outliers()] Plotting the outliers for ", input$Filtering.References, " iteration ", j), sep = "\n")
                                if (j == 1) {
                                    Y = General.df[[c(paste0("Out.Neg.",i))]]
                                } else{
                                    Y = General.df[[c(paste0("Out.",i,".",j - 1))]] # we need to plot the data of j-1 iterations and add the points of outliers
                                }
                                Date <- General.df[["date"]]
                                # Selecting ind.ref$out; created for the data series and selected for date for plotting
                                #ind.ref.out   <- selectByDate(ind.ref$out[[paste0(i,".",j,".Outli")]], start = input[[paste0("Out.Ref.Date",k)]][1], end = input[[paste0("Out.Ref.Date",k)]][2])
                                ind.ref.out   <- subset(ind.ref$out[[paste0(i,".",j,".Outli")]],
                                                        ind.ref$out[[paste0(i,".",j,".Outli")]]$date >= input[[paste0("Out.Ref.Date",k)]][1] &
                                                            ind.ref$out[[paste0(i,".",j,".Outli")]]$date <= input[[paste0("Out.Ref.Date",k)]][2] + 1
                                )
                                #ind.ref.out.n <- selectByDate(ind.ref$out[[paste0(i,".",j         )]], start = input[[paste0("Out.Ref.Date",k)]][1], end = input[[paste0("Out.Ref.Date",k)]][2])
                                ind.ref.out.n <- subset(ind.ref$out[[paste0(i,".",j)]],
                                                        ind.ref$out[[paste0(i,".",j)]]$date >= input[[paste0("Out.Ref.Date",k)]][1] &
                                                            ind.ref$out[[paste0(i,".",j)]]$date <= input[[paste0("Out.Ref.Date",k)]][2] + 1
                                )
                                plot_outli <- My.rm.Outliers(ymin         = input[[paste0("Ref.Ymin",k)]] ,
                                                             ymax         = input[[paste0("Ref.Ymax",k)]] ,
                                                             ThresholdMin = input[[paste0("Ref.ThresholdMin",k)]] ,
                                                             date         = Date,
                                                             y            = Y,
                                                             Ylab         = paste0("Reference data in ", CalSet()$unit.ref),
                                                             window       = input[[paste0("Ref.window",k)]] ,
                                                             threshold    = input[[paste0("Ref.threshold",k)]],
                                                             plotting     = TRUE,
                                                             set.Outliers = FALSE,
                                                             ind          = ind.ref.out,
                                                             Title        = paste0("Outliers of ",i, " iteration ", j),
                                                             Dygraphs     = TRUE
                                )
                                # Saving plot if requested
                                if (input$SavePlot) {
                                    dev.copy(png,filename = file.path(CalSet()$WDOutliers, paste0(AirSensEur.name(),"_Outliers_",i,".png")),
                                             #units = "cm", width = 35.55, height = 20,
                                             res = 300
                                    )
                                    dev.off()
                                    cat(paste0("[shiny] INFO, ",AirSensEur.name(),"_Outliers_",i,".png saved in ", CalSet()$WDOutliers, "\n" ))
                                    updateCheckboxInput(session, inputId = "SavePlot", value = FALSE)
                                }
                                cat(paste0("[Shiny, Plot.Ref.Outliers()] INFO, reference values for ", input$Filtering.References, " number of valid measurements after removing outliers ",
                                           length(which(!is.na(General.df[[paste0("Out.",i)]]))),". Number of outliers: ", length(which(ind.ref.out.n$Outliers)), "\n"))
                            }
                        }
                    }
                } else {
                    cat(paste0("[Shiny, Plot.Ref.Outliers()] INFO, discarding of outliers not requested for ", i, " in the Graphical User Interface "), sep = "\n")
                    plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                    text(1,1,paste0("[Shiny, Plot.Ref.Outliers()] INFO, discarding of outliers not requested for ", i, " in the Graphical User Interface "))
                    # Put back the outliers eventual removed? ################################################################################################################################?
                }
                cat("-----------------------------------------------------------------------------------\n")
                cat("\n")
                # Opening the Filtering TabSet for GUI consistency
                isolate({
                    updateTabsetPanel(session, inputId = "Treatments", selected = "Reference")
                })
            } else {
                cat(paste0("[Shiny]Plot.Ref.Outliers WARNING, No data existing for Reference pollutant ", i, " . Cannot discard outliers.\n"))
                plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                text(1,1,paste0("[Shiny]Plot.Ref.Outliers WARNING, No data existing for Reference pollutant ", i, " . Cannot discard outliers.\n"))
            }
            if (exists("plot_outli")) return(plot_outli)
        })
        # NavBar"Data Treatment", mainTabPanel "StatFiltered" - "PlotFiltering", ----
        output$StatFiltered        <- renderTable({
            t(StatFiltered())
        },
        digits = 0,
        include.rownames = TRUE,
        include.colnames = FALSE
        )
        # Reactive StatFiltered
        StatFiltered               <- reactive({
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            progress$set(message = "Reading Statistics of Filtered data", value = 0.5)
            # indexes of reference negative values
            ind.neg <- apply(X = DF$General[, .SD, .SDcols = list.gas.reference2use()], MARGIN = 2, function(x) {which(x < 0)})
            StatFiltered   <- data.frame(
                #name.gas           = list.gas.sensor(),
                name.sensor         = list.name.sensor(),
                Initial.Total       = sapply(seq_along(list.gas.sensor())      , function(i) length(na.omit(DF$General[[list.gas.sensor()[i]]])) ),
                Warming             = sapply(seq_along(list.gas.sensor())      , function(i) length(ind.warm$out[[i]]) ),
                T.min               = sapply(seq_along(list.gas.sensor())      , function(i) length(unlist(ind.TRh$out$T.min[paste0(list.name.sensor()[i], "__Temp. < ", input[[paste0("Temperature",i)]][1])])) ),
                T.max               = sapply(seq_along(list.gas.sensor())      , function(i) length(unlist(ind.TRh$out$T.max[paste0(list.name.sensor()[i], "__Temp. > ", input[[paste0("Temperature",i)]][2])])) ),
                RH.min              = sapply(seq_along(list.gas.sensor())      , function(i) length(unlist(ind.TRh$out$Rh.min[paste0(list.name.sensor()[i], "__RH < ", input[[paste0("Humidity",i)]][1])])) ),
                RH.max              = sapply(seq_along(list.gas.sensor())      , function(i) length(unlist(ind.TRh$out$Rh.max[paste0(list.name.sensor()[i], "__RH < ", input[[paste0("Humidity",i)]][2])])) ),
                Invalid             = sapply(seq_along(list.gas.sensor())      , function(i) if (Outliers_Sensor()$Sens.Inval.Out[i]) length(unlist(lapply(ind.Invalid$out[[2]][[list.name.sensor()[i]]], is.numeric))) else NA),
                Sens.Outlier_Max    = sapply(seq_along(list.gas.sensor())      , function(i) if (Outliers_Sensor()$Sens.rm.Out[i])    length(unlist(which(ind.sens$out[paste0(list.gas.sensor()[i],".1.Outli")][[1]][,"OutliersMax"]))) else NA),
                Sens.Outlier_Min    = sapply(seq_along(list.gas.sensor())      , function(i) if (Outliers_Sensor()$Sens.rm.Out[i])    length(unlist(which(ind.sens$out[paste0(list.gas.sensor()[i],".1.Outli")][[1]][,"OutliersMin"]))) else NA),
                Sens.High_values    = sapply(seq_along(list.gas.sensor())      , function(i) if (Outliers_Sensor()$Sens.rm.Out[i])    length(unlist(which(ind.sens$out[paste0(list.gas.sensor()[i],".1.Outli")][[1]][,"High_values"]))) else NA),
                Sens.Low_values     = sapply(seq_along(list.gas.sensor())      , function(i) if (Outliers_Sensor()$Sens.rm.Out[i])    length(unlist(which(ind.sens$out[paste0(list.gas.sensor()[i],".1.Outli")][[1]][,"Low_values"]) )) else NA),
                Sens.Filtered.Total = sapply(seq_along(list.gas.sensor())      , function(i) length(na.omit(DF$General[[paste0("Out.",list.gas.sensor()[i])]]))),
                gas.reference2use   = list.gas.reference2use(),
                Negative.Reference  = sapply(seq_along(list.gas.reference2use()), function(i) if (Outliers_Ref()$remove.neg[i]) length(ind.neg[[i]]) else NA),
                Refe.Outlier_Max    = sapply(seq_along(list.gas.reference2use()), function(i) if (Outliers_Ref()$Ref.rm.Out[i]) length(unlist(which(ind.ref$out[paste0(list.gas.reference2use()[i],".1.Outli")][[1]][,"OutliersMax"]))) else NA),
                Refe.Outlier_Min    = sapply(seq_along(list.gas.reference2use()), function(i) if (Outliers_Ref()$Ref.rm.Out[i]) length(unlist(which(ind.ref$out[paste0(list.gas.reference2use()[i],".1.Outli")][[1]][,"OutliersMin"]))) else NA),
                Refe.High_values    = sapply(seq_along(list.gas.reference2use()), function(i) if (Outliers_Ref()$Ref.rm.Out[i]) length(unlist(which(ind.ref$out[paste0(list.gas.reference2use()[i],".1.Outli")][[1]][,"High_values"]))) else NA),
                Refe.Low_values     = sapply(seq_along(list.gas.reference2use()), function(i) if (Outliers_Ref()$Ref.rm.Out[i]) length(unlist(which(ind.ref$out[paste0(list.gas.reference2use()[i],".1.Outli")][[1]][,"Low_values"]) )) else NA))
            return(StatFiltered)
        })
        # NavBar"Data Treatment", mainTabPanel "Covariates" -"Plots", ----
        # Observer raw.unit ----
        observeEvent({
            # Making reactive changes in the conversion function according to the sensor raw units,
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Sens.raw.unit", i)]]))
        },{
            k <- match(x = input$Calib.Sensors, table = list.name.sensor())
            updateCheckboxInput(session, inputId = paste0("Apply.conv", k) , value = TRUE)
        },
        ignoreInit = TRUE
        )
        # Reactive Force.Conv ----
        observeEvent({
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Apply.conv", i)]]))
            Outliers.Sens$Forced
        },{
            # Reactive function to trigger General.conv()
            # Force.Conv is TRUE if there is no _volt in General.conv() or
            #                    if any Button Force.Conv is checked or if any CheckBoxes Neg.mod1 is checked or
            #                    if Warm.TRh.Neg.Inv$Forced | Outliers.Ref$Forced are TRUE
            #
            # depends:
            #       list.gas.sensor()
            #       DF$General
            #       input$Force.Conv
            if (Outliers.Sens$Forced ||
                !any(grepl(pattern = paste0(list.name.sensor()[1],"_volt"), x = colnames(DF$General)))          ||
                any(unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Apply.conv", i)]])))
            ) Conv$Forced <<- TRUE else Conv$Forced <<- FALSE
        },
        priority = 140
        )
        # NavBar"Data Treatment", mainTabPanel "Time series" - "Covariates", ----
        Plot.ts_Cov_dygraphs <- eventReactive({
            input$Sensors
            input$Selected
            input$Merge
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Date",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Sens",i)]]))
        },{
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "[Shiny, Plot.ValidCovar()] INFO, Plotting times series of covariates", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat("[shiny, ts_Cov_dygraphs()] INFO, plotting time series of validated data with covariates\n")
            # Checking that there are data to plot
            
            if (!all(input[[paste0("Sens",CalSet()$k)]] %in% names(DF$General)) || all(is.na(DF$General[, .SD, .SDcols = input[[paste0("Sens",CalSet()$k)]]]))) {
                Cal$Forced <- TRUE
                my_message <- paste0("[Shiny, ts_Cov_dygraphs()] ERROR, All or some sensor time series are empty, not plotting any times series, miising:\n",
                                     input[[paste0("Sens",CalSet()$k)]][which(!input[[paste0("Sens",CalSet()$k)]] %in% names(DF$General))])    
                cat(my_message)
                shinyalert(
                    title = "ERROR no data to plot",
                    text = my_message,
                    closeOnEsc = TRUE,
                    closeOnClickOutside = TRUE,
                    html = FALSE,
                    type = "error",
                    showConfirmButton = TRUE,
                    showCancelButton  = FALSE,
                    confirmButtonText = "OK",
                    confirmButtonCol  = "#AEDEF4",
                    timer             = 3000,
                    imageUrl          = "",
                    animation         = FALSE)
            } else {
                # Sensor relationships with other variables
                cat(paste0("[shiny, ts_Cov_dygraphs()] INFO, Plot sensor data in volt/nA with covariates for Sensor ",
                           input$Sensors, " in order to check relationships with other variables\n"))
                Relationships         <- na.omit(colnames(DF$General)[which(colnames(DF$General) %in% input[[paste0("Sens",CalSet()$k)]]) ])
                # removing variable date for time series plotting
                if (any(grepl(pattern = "date", x = Relationships))) Relationships <- Relationships[-which(Relationships == "date")]
                # Plotting timeseries, changing names of variables
                Name.pol = Relationships %>%
                    gsub(pattern = "Out." , replacement = "", x = .) %>%
                    gsub(pattern = "_volt", replacement = paste0(".",CalSet()$Sens.raw.unit), x = .)
                # Selecting data
                General.df <- DF$General[date >= input[[paste0("Date",CalSet()$k)]][1] & date <= input[[paste0("Date",CalSet()$k)]][2] + 1,
                                         .SD,.SDcols = c("date",Relationships)]
                time_series_sensor_Cov <- data_frame_to_timeseries(General.df, tz = threadr::time_zone(General.df[["date"]][1]))
                # colour_vector <- threadr::ggplot2_colours(45)
                colour_vector <- c("red", "blue", "black", "green", "cornflowerblue", "chocolate4", "darkblue",
                                   "darkgoldenrod3", "darkorange", "darkolivegreen4", "goldenrod4", "darkred",
                                   "darkmagenta", "darkgreen", "darkcyan", "red", "blue", "black", "green",
                                   "cornflowerblue", "chocolate4", "darkblue", "darkgoldenrod3", "darkorange", "darkolivegreen4",
                                   "goldenrod4", "darkred")
                colour_vector <- colour_vector[1:length(Relationships)]
                # Make interactive time-series plot
                # Define heith of the Combined plots
                Height <- as.character(round(1/length(time_series_sensor_Cov) * 800))
                #initialize list
                plot_Cov_list <- list()
                for (i in seq_along(Relationships)) {
                    ts_Cov <- time_series_sensor_Cov[[i]]
                    plot_Cov <- dygraph(ts_Cov, group = "covariates", height = Height, width = "100%") %>% #
                        dySeries(label = Name.pol[i], color = colour_vector[i]) %>%
                        dyAxis("y", label = Name.pol[i]) %>%
                        dyRangeSelector(height = 10) %>%
                        dyOptions(labelsUTC = T) # plot in UTC
                    plot_Cov_list[[i]] <- plot_Cov  #add each element to list
                }
                # save html to png
                # check if PhantomJS is installed in C:\Users\karaf\AppData\Roaming\PhantomJS, else "install_phantomjs()"
                if (input$SavePlot) {
                    WDoutput <- file.path(input$Selected, "Verification_plots")
                    filename_html <- file.path(WDoutput,paste0(CalSet()$name.sensor,"_ts_",
                                                               format(input[[paste0("Date",CalSet()$k)]][1],"%Y%m%d"),"_",
                                                               format(input[[paste0("Date",CalSet()$k)]][2],"%Y%m%d"),"temp.html"))
                    filename_png <- file.path(WDoutput,paste0(CalSet()$name.sensor,"_ts_",
                                                              format(input[[paste0("Date",CalSet()$k)]][1],"%Y%m%d"),"_",
                                                              format(input[[paste0("Date",CalSet()$k)]][2],"%Y%m%d"),".png"))
                    save_html(plot_Cov_list, filename_html)
                    webshot(filename_html, file     = filename_png, cliprect = "viewport")
                    # Update button save plot
                    updateCheckboxInput(session, inputId = "SavePlot", value = FALSE)
                }
                
                cat("-----------------------------------------------------------------------------------\n")
                cat("\n")
                progress$set(message = "[Shiny, Plot.ValidCovar()] INFO, Plotting times series of covariates", value = 0.5)
                
                # render the dygraphs objects using htmltools
                plot_Cov_list <- htmltools::tagList(plot_Cov_list)
                return(plot_Cov_list)
            }
        })
        output$ts_Cov_dygraphs <- renderUI(Plot.ts_Cov_dygraphs())
        # NavBar "Data Treatment", mainTabPanel "Matrix" - Covariates",  ----
        output$ValidCovarMatrix <- renderPlot(Plot.ValidCovarMatrix(), width = 'auto', height = 'auto')
        # Reactive FUN Plot.ValidCovarMatrix
        Plot.ValidCovarMatrix   <- eventReactive({
            input$Sensors
            input$Selected
            input$Merge
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Date",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Sens",i)]]))
        },{
            # Plotting correllation matrix using pairs()
            op <- par(no.readonly = TRUE)
            # Restoring graphical parameters on exit of function
            on.exit(par(op))
            par(mfrow = c(1,1))
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "[Shiny, Plot.ValidCovarMatrix ()] INFO, Plotting matrix of covariates", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat("[Shiny, Plot.ValidCovarMatrix ()] INFO, plotting correlation matrix of validated data with covariates\n")
            
            if (all(is.na(DF$General[,input[[paste0("Sens",CalSet()$k)]], with = FALSE]))) {
                Cal$Forced <- TRUE
                my_message <- "[[Shiny, Plot.ValidCovarMatrix ()] ERROR, All sensor time series are empty, not plotting any correlation matrix\n"
                cat(my_message)
                shinyalert(
                    title = "ERROR no data to plot",
                    text = my_message,
                    closeOnEsc = TRUE,
                    closeOnClickOutside = TRUE,
                    html = FALSE,
                    type = "error",
                    showConfirmButton = TRUE,
                    showCancelButton  = FALSE,
                    confirmButtonText = "OK",
                    confirmButtonCol  = "#AEDEF4",
                    timer             = 3000,
                    imageUrl          = "",
                    animation         = FALSE)
            } else {
                # Sensor relationships with other variables
                cat(paste0("[Shiny, Plot.ValidCovarMatrix ()] INFO, Plot sensor data in volt with covariates for Sensor ", input$Sensors, 
                           " in order to check relationships with other variables\n"))
                # unique to avoid repeating sensors_modelled
                Relationships         <- unique(na.omit(colnames(DF$General)[colnames(DF$General) %in% input[[paste0("Sens",CalSet()$k)]] ]) )
                AddOut                <- which(Relationships %in% list.gas.sensor())
                Relationships[AddOut] <- paste0(Relationships[AddOut])
                # changing names of variables
                Pattern  <- rbind(c("Out.", ""),c("Ref.", paste0("Reference in ",CalSet()$unit.ref,", ")),c("_volt", paste0(" Sensor in ",CalSet()$Sens.raw.unit)))
                if (nrow(Pattern) > 0) Labels <- gsub(pattern = Pattern[1,1], replacement = Pattern[1,2], x = Relationships)
                if (nrow(Pattern) > 1) for (i in 2:nrow(Pattern)) Labels <- gsub(pattern = Pattern[i,1], replacement = Pattern[i,2], x = Labels)
                
                pairs(x = DF$General[date >= CalSet()$Cov.DateIN & date <= CalSet()$Cov.DateEND + 1, ..Relationships],
                      lower.panel = panel.smooth,
                      upper.panel = panel.cor,
                      diag.panel  = panel.hist,
                      labels = Labels,
                      main = paste0("Correlation matrix of sensor data (R2 in bold) versus covariates for sensor ", input$Sensors,
                                    " between ", CalSet()$Cov.DateIN, " and ", CalSet()$Cov.DateEND),
                      cex.labels = 1.8
                ) # cex.cor = 1.3
                
                # Saving plot if requested
                if (input$SavePlot) {
                    WDoutput <- file.path(input$Selected, "Verification_plots")
                    dev.copy(png,
                             filename = file.path(WDoutput,
                                                  paste0(CalSet()$name.sensor,"_pairs_",
                                                         format(min(DF$General$date, na.rm = TRUE),"%Y%m%d"),"_",
                                                         format(max(DF$General$date, na.rm = TRUE),"%Y%m%d"),".png")),
                             #units = "cm", width = 35.55, height = 20,
                             res = 300
                    )
                    dev.off()
                    cat(paste0("[shiny] INFO, ", CalSet()$name.sensor,"_pairs_",
                               format(min(DF$General$date, na.rm = TRUE),"%Y%m%d"),"_",
                               format(max(DF$General$date, na.rm = TRUE),"%Y%m%d"),".png saved in ", WDoutput, "\n" ))
                    updateCheckboxInput(session, inputId = "SavePlot", value = FALSE)
                }
            }
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            progress$set(message = "[Shiny, Plot.ValidCovarMatrix ()] INFO, Plotting matrix of covariates", value = 1)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
        })
        # Observer General.conv(), input$Cal and input$Neg.mod ----
        observeEvent({
            # Making reactive changes in the calibration if Negative shall be discarded
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Neg.mod",i)]]))
        },{
            k <- match(x = input$Calib.Sensors, table = list.name.sensor())
            updateCheckboxInput(session, inputId = paste0("Apply.cal", k) , value = TRUE)
        },
        ignoreInit = TRUE
        )
        # update Raw unit, model for calibration and range for calibration when selecting a Calibration model
        # if not commented TimesSeries and Residual Matrix do not update when changing model
        observeEvent({
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Cal", i)]]))
        },{
            # checking that input$Calx is not NULL
            if (!is.null(input[[paste0("Cal",CalSet()$k)]])) {
                # checking that input$Calx is not empty
                if (input[[paste0("Cal",CalSet()$k)]] != "") {
                    Splitted.Cal <- unlist(strsplit(x = CalSet()$Cal, split = "__"))
                    Unit  <- Splitted.Cal[3]
                    Model <- Splitted.Cal[4]
                    Start <- strptime(Splitted.Cal[5], format = "%Y%m%d", tz = threadr::time_zone(DF$General$date[1]))
                    End   <- strptime(Splitted.Cal[6], format = "%Y%m%d", tz = threadr::time_zone(DF$General$date[1]))
                    # Changing unit
                    if (is.null(input[[paste0("Sens.raw.unit",CalSet()$k)]]) || is.na(input[[paste0("Sens.raw.unit",CalSet()$k)]]) )  {
                        updateSelectInput(session = session,
                                          inputId  = paste0("Sens.raw.unit",CalSet()$k),
                                          selected = Unit)
                    } else if (input[[paste0("Sens.raw.unit",CalSet()$k)]] != Unit) {
                        updateSelectInput(session = session,
                                          inputId  = paste0("Sens.raw.unit",CalSet()$k),
                                          selected = Unit)
                    }
                    # determinig model type
                    if (Model != input[[paste0("Calibration",CalSet()$k)]]) {
                        updateSelectInput(session = session,
                                          inputId  = paste0("Calibration",CalSet()$k),
                                          selected = Model
                        )
                    }
                    # determining date interval
                    # Updating DateCal
                    Date.names <- c("DateCal","DatePlotCal")
                    for (l in Date.names) {
                        if (any(sapply(1:2, function(m) is.null(input[[paste0(l,CalSet()$k)]][m]) || is.na(input[[paste0(l,CalSet()$k)]][m]) || is.nan(input[[paste0(l,CalSet()$k)]][m])))) {
                            updateDateRangeInput(session,
                                                 inputId = paste0(l,CalSet()$k),
                                                 start   = Start,
                                                 end     = End)
                        }  else if (Start != input[[paste0(l,CalSet()$k)]][1] || End != input[[paste0(l,CalSet()$k)]][2]) {
                            updateDateRangeInput(session,
                                                 inputId = paste0(l,CalSet()$k),
                                                 start   = Start,
                                                 end     = End)
                        }
                    }
                    # determining Covariates
                    if (Model == "MultiLinear") {
                        # Co-Variates selected in UI
                        Covariates.CovMod    <- str_replace(Splitted.Cal[7], pattern = ".rdata", replacement = "")
                        Covariates.CovMod    <- unlist(strsplit(x = Covariates.CovMod , split = "&"))
                        if (any(grepl(pattern = "-", x = Covariates.CovMod[1]))) {
                            Covariates.CovMod <- unlist(strsplit(x = Covariates.CovMod , split = "-"))
                            Covariates.CovMod <- Covariates.CovMod[ seq(from = 1, to = length(Covariates.CovMod), by = 2) ]
                        }
                        if (!all(Covariates.CovMod %in% input[[paste0("CovMod",CalSet()$k)]]) | !all(input[[paste0("CovMod",CalSet()$k)]] %in%  Covariates.CovMod )) {
                            updateSelectInput(session = session,
                                              inputId  = paste0("CovMod",CalSet()$k),
                                              selected = Covariates.CovMod
                            )
                        }
                    }
                    # Triggering a calibration
                    if (!input[[paste0("Apply.cal", CalSet()$k)]]) updateCheckboxInput(session, inputId = paste0("Apply.cal", CalSet()$k), value = TRUE)
                }
            }
        },
        ignoreInit = TRUE,
        priority = 3000
        )
        # Reactive General.cal ----
        observeEvent({
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Apply.cal", i)]]))
            Outliers.Sens$Forced
            Outliers.Ref$Forced
            Conv$Forced
        },{
            # Reactive function to trigger DF$General
            # Cal$Forced is TRUE if any Button Apply.cal is checked or
            #                    if there is no gas_modelled in DF$General or
            #                    if any Outliers.Sens$Forced | Outliers.Ref$Forced | Conv$Forced are TRUE
            # depends:
            #       list.gas.sensor()
            #       DF$General
            #       input$Force.ConvN
            #       input$Apply.calN
            if (Outliers.Sens$Forced || Outliers.Ref$Forced || Conv$Forced ||
                !any(grepl(pattern = paste0(list.gas.sensor(),"_modelled",collapse = "|"), x = colnames(DF$General)))    |
                any(unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Apply.cal", i)]])))
            ) Cal$Forced <<- TRUE else Cal$Forced <<- FALSE
        }, priority = 130)
        # The whole calibration
        observeEvent({
            # Outliers.Ref$Forced, not needed since if Outliers.Ref$Forced is TRUE then Cal$Forced is set to TRUE
            Conv$Forced
            Cal$Forced
        },{
            # Output:
            #    DF$General is dataFrame stating from DF$General with values set to NA for
            #               sensor warming time
            #               temperature and humidity outside interval of tolerance
            #               sensor data invaidated between periods of dates
            #               outliers of sensor data
            #               outliers of reference data
            #    DF$General includes converted and calibrated sensor data (Prediction by application of calibration models)
            # depends:
            #       Warm.TRh.Neg.Inv
            #       Outliers.Ref$Forced
            #       Conv$Forced
            #       Cal$Forced
            # initial values Loading General.Rdata or General
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat(paste0("[shiny, General.cal()] INFO, Outliers.Sens$Forced is : ", Outliers.Ref$Forced,"\n"))
            cat(paste0("[shiny, General.cal()] INFO, Outliers.Ref$Forced is : " , Outliers.Ref$Forced,"\n"))
            cat(paste0("[shiny, General.cal()] INFO, Conv$Forced is : "         , Conv$Forced,"\n"))
            cat(paste0("[shiny, General.cal()] INFO, Cal$Forced is : "          , Cal$Forced,"\n"))
            cat("-----------------------------------------------------------------------------------\n")
            if (Conv$Forced || Cal$Forced) { # "Outliers.Ref$Forced |" not needed since if Outliers.Ref$Forced is TRUE then Cal$Forced is set to TRUE
                # input:
                #   DF$General         :     dataframe created after discarding invalid data,
                #   list.gas.sensor() :     charater vector with the names of compounds
                # Output:
                #     Change DF$General starting from DF$General. Provided that outlier discarding is enabled, for each sensor compound name i in list.gas.sensor()
                #     (e. g. "Carbon_monoxide","Nitric_oxide","Nitrogen_dioxide" and "Ozone"), add to DF$General columns names "Out.",i and  "Out.",i,".",j where j is
                #     the number of iterations of the outlier discarding. "Out.",i has the whole outliers of all iterationset to NA while "Out.",i,".",j has
                #     the whole outliers up to iteration j.
                #     Provided that outlier discarding is eneabled, for each reference compound name i in list.gas.reference2use() (e. g. "Ref.NO","Ref.NO2","Ref.NOx","Ref.SO2",
                #     "Ref.O3","Ref.CO_ppm"), add to DF$General columns names "Out.",i and  "Out.",i,".",j where j is the number of iterations of the outlier discarding.
                #     "Out.",i has the whole outliers of all iterationset to NA while "Out.",i,".",j has the whole outliers up to iteration j.
                # depends:
                #     Outliers.Sens, Outliers.Ref
                #     ind.sens$out
                # Converting to nA or V
                if (Conv$Forced) {
                    # Create a Progress object
                    progress <- shiny::Progress$new()
                    # Make sure it closes when we exit this reactive, even if there's an error
                    progress$set(message = "[shiny, General.conv()] INFO, Converting sensor digital data to analogue V or nA", value = 0.5)
                    # digits2volt conversion for whole data in nA or V
                    cat("\n")
                    cat("-----------------------------------------------------------------------------------\n")
                    cat("[shiny, General.conv()] INFO, digital to volt conversion for all sensors on the shields\n")
                    # Conversion to volts/A
                    Sensors_Cal <- merge(x = Calib_data()[c("name.gas","gas.sensor","name.sensor","Sens.raw.unit")], # if we use CaliB_data the file is saved every time we update Calid_data
                                         y = Shield()[,c("name.gas","name.sensor","gas.sensor","RefAD","Ref","board.zero.set","GAIN","Rload")],
                                         by = c("name.gas", "gas.sensor", "name.sensor"),
                                         all = TRUE)
                    # order Sensors_Cal as Calib_data()Goo
                    #Sensors_Cal <- Sensors_Cal[na.omit(match(list.gas.sensor(),Sensors_Cal$gas.sensor)),]
                    # Values converted in volt or nA of sensors in Shield() only if sensor data exist
                    data.table::set(DF$General,  j = paste0(Shield()$name.sensor,"_volt"),
                                    value = ASEDigi2Volt(Sensors_Cal = Sensors_Cal[Sensors_Cal$name.gas %in% Shield()$name.gas,],
                                                         Digital = DF$General[,paste0("Out.",Shield()$gas.sensor), with = FALSE]))
                    # Values converted in volt or nA - Board zero in Volt? change to V or nA
                    # x <- t(matrix(data = rep(x     = Config$all[["sens2ref"]]$Ref - Config$all[["sens2ref"]]$RefAD,
                    #                          times = nrow(DF$General)), ncol = nrow(DF$General) ))
                    # # https://stackoverflow.com/questions/6819804/how-to-convert-a-matrix-to-a-list-of-column-vectors-in-r
                    # data.table::set(DF$General,  j = paste0(Shield()$name.sensor,"_DV"),
                    #                 value = lapply(1:ncol(x), function(i) x[,i]))
                    # rm(x)
                    data.table::set(DF$General,  j = paste0(Shield()$name.sensor,"_DV"),
                                    value = lapply(Shield()$name.sensor, function(i) rep(Shield()$Ref[Shield()$name.sensor == i] - Shield()$RefAD[Shield()$name.sensor == i],
                                                                                         times = nrow(DF$General))))
                    # No conversion for the sensors which are not in the Shield only if sensor data exist
                    No.Shield.gas.Sensors <- setdiff(list.gas.sensor(), Shield()$gas.sensor)
                    No.Shield.gas.Sensors <- No.Shield.gas.Sensors[which(c(paste0("Out.",No.Shield.gas.Sensors) %in% names(DF$General) ))]
                    if (length(No.Shield.gas.Sensors) > 0) {
                        No.Shield.name.Sensors <- setdiff(list.name.sensor(), Shield()$name.sensor)
                        x <- DF$General[,paste0("Out.",No.Shield.gas.Sensors), with = FALSE]
                        data.table::set(DF$General,  j =  paste0(No.Shield.name.Sensors,"_volt"),
                                        value = lapply(seq_len(ncol(x)), function(i) x[,i]))
                        rm(x)
                    }
                    progress$set(message = "[shiny, General.conv()] INFO, Converting sensor digital data to V or nA", value = 1)
                    progress$close()
                }
                # Starting calibration
                if (Cal$Forced) {
                    # Create a Progress object
                    progress <- shiny::Progress$new()
                    # Make sure it closes when we exit this reactive, even if there's an error
                    on.exit(progress$close())
                    Tot.Iter <- length(list.gas.sensor())
                    rate <- 1 / (Tot.Iter + 2)
                    ValueRate <- rate
                    progress$set(message = paste0("[shiny, General.cal()] INFO, Calibrating raw sensor values to ",CalSet()$unit.sensor), value = ValueRate)
                    # Application of Calibration function to Complete data set
                    if (!is.null(DF$General)) {
                        # initial Calibration with values in input[[paste0("Cal",j)]])) provided that "Method of Prediction" is "Prediction with previous calibration"
                        #for (k in seq_along(list.name.sensor())) {
                        # calibrating only one sensor at a time
                        k <- CalSet()$k
                        ValueRate <- ValueRate + rate
                        progress$set(message = paste0("[shiny, General.cal()] INFO, Calibrating raw sensor values to ", CalSet()$unit.sensor), value = ValueRate)
                        if (CalSet()$Cal_Line == "Prediction with previous calibration") { #if a for loop is used, replace CalSet()$Cal_Line with input[[paste0("Cal.Line",k)]]
                            if (nchar(CalSet()$Cal) != 0) { #if a for loop is used replace, CalSet()$Cal with input[[paste0("Cal",k)]]
                                # reading file
                                name.Model.i <- file.path(CalSet()$WDoutputMod, CalSet()$Cal)       # if you use a for loop, replace CalSet()$Cal with input[[paste0("Cal",k)]]))
                                if (file.exists(name.Model.i)) { # & input[[paste0("Cal",k)]] != ""
                                    cat(paste0("[shiny, General.cal()] INFO, Calibrating raw values of sensor ", list.name.sensor()[k],
                                               " using model ", CalSet()$Cal, " to unit ", CalSet()$unit.sensor,"\n"))
                                    # Loading Model.i either as Rdata list or as a RDS file
                                    if (extension(name.Model.i) == ".rdata") Model.i <- load_obj(name.Model.i) else if (extension(name.Model.i) == ".rds") {
                                        if (file.exists(name.Model.i)) {
                                            # Read model object as a RDS object
                                            Model.i <- readRDS(file = name.Model.i)
                                            #Convert to a broom oject to tidy model
                                            Model.i <- list(Tidy = tidy(Model.i), Augment = augment(Model.i), Glance = glance(Model.i), Call = Model.i$call, Coef = coef(Model.i))
                                            # save as a Rdata list
                                            list.save(x    = Model.i,
                                                      file = sub(pattern = ".rds", replacement = ".rdata", x = name.Model.i))
                                            file.remove(name.Model.i)
                                            # Updating the selected model
                                            Newchoices <- substr(list.files(path    = CalSet()$WDoutputMod,
                                                                            pattern = glob2rx(paste0(CalSet()$AirSensEur.name,"__",CalSet()$name.sensor,"__*"))),
                                                                 start = nchar(paste0(CalSet()$AirSensEur.name,"__",CalSet()$name.sensor,"__")) + 1,
                                                                 stop  = nchar(list.files(path    = CalSet()$WDoutputMod,
                                                                                          pattern = glob2rx(paste0(CalSet()$AirSensEur.name,"*",CalSet()$name.sensor,"*")))))
                                            # Current model
                                            NewModel  <-  name.Model.i %>%
                                                basename(.) %>%
                                                sub(pattern = ".rds", replacement = ".rdata", x = .) %>%
                                                sub(pattern = paste0(CalSet()$AirSensEur.name,"__",CalSet()$name.sensor,"__"), replacement = "", x = .)
                                            updateSelectInput(session, inputId = paste0("Cal", CalSet()$k), label = "Select a previous calibration ",
                                                              choices = Newchoices, selected = NewModel[1])
                                        }
                                    }
                                    # sensor gas in volt or nA or Count
                                    nameGasVolt <- CalSet()$nameGasVolt      # if you use a for loop, replace CalSet()$nameGasVolt with paste0(list.name.sensor()[k],"_volt")
                                    # modelled sensor gas
                                    nameGasMod  <- CalSet()$nameGasMod  # if you use a for loop, replace CalSet()$nameGasMod with paste0(list.gas.sensor()[k] ,"_modelled")
                                    # Detecting the model type of the selected calibration model
                                    Mod_type <- Models[grep(pattern = paste0("_",strsplit(CalSet()$Cal, split = "__")[[1]][4],"_"), x = paste0("_",Models,"_"))]
                                    # Preparing the matrix of covariates
                                    # Removing na for nameGasMod for nameGasVolt missing
                                    is.not.NA.y <- which(!is.na(DF$General[[nameGasVolt]]))
                                    is.NA.y     <- which( is.na(DF$General[[nameGasVolt]]))
                                    if (Mod_type == "MultiLinear") {
                                        CovMod  <- unlist(strsplit(x = unlist(strsplit(x = sub(pattern = paste(c(".rds",".rdata"), collapse = "|"),
                                                                                               replacement = "",
                                                                                               x = CalSet()$Cal),         # if you use a for loop, replace CalSet()$Cal with input[[paste0("Cal",k)]]))
                                                                                       split = "__"))[7],
                                                                   split = "&",
                                                                   fixed = T))
                                        # Checking if there are "-" in the CovMod, deleting degrees of polynomial
                                        if (any(grepl(pattern = "-", x = CovMod[1]))) {
                                            Model.CovMod  <- unlist(strsplit(x = CovMod , split = "-"))
                                            CovMod  <- Model.CovMod[ seq(from = 1, to = length(Model.CovMod), by = 2) ]
                                            Degrees <- Model.CovMod[ seq(from = 2, to = length(Model.CovMod), by = 2) ]
                                        }
                                        # checking that all CovMod are included in DF$General
                                        if (!all(CovMod %in% names(DF$General))) {
                                            my_message <- paste0("[shiny, Cal$Forced] ERROR, not all Covariates are available, something missing in ", CovMod,
                                                                 ". The app  may crash.\n")
                                            cat(my_message)
                                            shinyalert(
                                                title = "ERROR missing covariates",
                                                text = my_message,
                                                closeOnEsc = TRUE,
                                                closeOnClickOutside = TRUE,
                                                html = FALSE,
                                                type = "error",
                                                showConfirmButton = TRUE,
                                                showCancelButton  = FALSE,
                                                confirmButtonText = "OK",
                                                confirmButtonCol  = "#AEDEF4",
                                                timer             = 0,
                                                imageUrl          = "",
                                                animation         = FALSE)
                                        } else {
                                            # take only the one that is nor NA of y = DF$General[!is.na(DF$General[, nameGasVolt]), nameGasVolt]
                                            is.not.NA.y <- which(complete.cases(DF$General[, .SD, .SDcols = c(nameGasVolt,CovMod)]))
                                            is.NA.y     <- setdiff(1:nrow(DF$General), is.not.NA.y)
                                            Matrice <- data.frame(DF$General[is.not.NA.y, CovMod, with = FALSE],
                                                                  row.names = row.names(DF$General[is.not.NA.y,]),
                                                                  stringsAsFactors = FALSE)
                                            names(Matrice) <- CovMod
                                        }
                                    } else if (Mod_type %in% c("exp_kT","exp_kK","T_power", "K_power")) {
                                        # take only the one that is nor NA of y = DF$General[!is.na(DF$General[, nameGasVolt]), nameGasVolt]
                                        is.not.NA.y <- which(complete.cases(DF$General[, .SD, .SDcols = c(nameGasVolt, "Temperature")]))
                                        is.NA.y     <- setdiff(1:nrow(DF$General), is.not.NA.y)
                                        Matrice <- data.frame(DF$General[is.not.NA.y, Temperature],
                                                              row.names = row.names(DF$General[is.not.NA.y,]),
                                                              stringsAsFactors = FALSE)
                                        names(Matrice) <- "Temperature"
                                    } else {
                                        Matrice <- NULL
                                    }
                                    # Using the reverse calibration function (measuring function) to extrapolate calibration
                                    if (Mod_type != "MultiLinear" || (Mod_type == "MultiLinear" && all(CovMod %in% names(DF$General)))) {
                                        data.table::set(DF$General, i = is.not.NA.y, j = nameGasMod,
                                                        value = list(Meas_Function(y          = DF$General[[nameGasVolt]][is.not.NA.y],
                                                                                   Mod_type   = Mod_type ,
                                                                                   covariates = CovMod,
                                                                                   Degrees    = Degrees,
                                                                                   Model      = Model.i,
                                                                                   Matrice    = Matrice)))
                                        # Removing na for nameGasMod either nameGasVolt missing or CovMod missing
                                        data.table::set(DF$General, i = is.NA.y, j = nameGasMod, value = list(rep(NA, times = length(is.NA.y))))
                                        # setting negative values to NA
                                        if (input[[paste0("Neg.mod",k)]]) {
                                            data.table::set(DF$General, i = which(DF$General[, nameGasMod, with = FALSE] < 0), j = nameGasMod,
                                                            value = list(rep(NA, times = length(which(DF$General[, nameGasMod, with = FALSE] < 0)))))
                                        }
                                    }
                                } else {
                                    my_message <- paste0(paste0("[shiny, Cal$Forced] INFO, there is no calibration function for sensors: ", list.name.sensor()[k], "\n"))
                                    cat(my_message)
                                    shinyalert(
                                        title = "INFO missing calibration",
                                        text = my_message,
                                        closeOnEsc = TRUE,
                                        closeOnClickOutside = TRUE,
                                        html = FALSE,
                                        type = "info",
                                        showConfirmButton = TRUE,
                                        showCancelButton  = FALSE,
                                        confirmButtonText = "OK",
                                        confirmButtonCol  = "#AEDEF4",
                                        timer             = 3000,
                                        imageUrl          = "",
                                        animation         = FALSE)
                                    NewModel <- substr(list.files(path = file.path(input$Selected,"Models"),
                                                                  pattern = glob2rx(paste0(ASE_name(),"__",CalSet()$name.sensor,"__*"))),
                                                       start = nchar(paste0(ASE_name(),"__",CalSet()$name.sensor,"__")) + 1,
                                                       stop  = nchar(list.files(path = file.path(input$Selected,"Models"),
                                                                                pattern = glob2rx(paste0(ASE_name(),"__",CalSet()$name.sensor,"__*")))))
                                    if (length(NewModel) > 0) {
                                        updateSelectInput(session, inputId = paste0("Cal", CalSet()$k), label = "Select a previous calibration",
                                                          choices = NewModel, selected = NewModel[1])
                                    }
                                }
                            }
                        }
                        #}
                        progress$set(message = paste0("[shiny, General.cal()] INFO, Calibrating raw sensor values to ", CalSet()$unit.sensor), value = 1)
                        #progress$close()
                    } else cat("[shiny, General.cal()] INFO, ADD A SHINY ALERT THAT THERE IS NO General.df or no sensor data converted to voltage or current.\n")
                }
                # Resetting CheckBoxes and Forces...
                if (Neg$Forced)              Neg$Forced              <<- FALSE
                if (Outliers.Sens$Forced)    Outliers.Sens$Forced    <<- FALSE
                if (Outliers.Ref$Forced)     Outliers.Ref$Forced     <<- FALSE
                if (Conv$Forced)             Conv$Forced             <<- FALSE
                if (Cal$Forced)              Cal$Forced              <<- FALSE
                if (Warm$Forced)             Warm$Forced             <<- FALSE
                if (TRh$Forced)              TRh$Forced              <<- FALSE
                if (Inv$Forced)              Inv$Forced              <<- FALSE
                for (i in seq_along(list.name.sensor())) {
                    if (input[[paste0("Apply.Warm"   ,i)]]) updateCheckboxInput(session, inputId = paste0("Apply.Warm"   ,i), value = FALSE)
                    if (input[[paste0("Apply.TRh"    ,i)]]) updateCheckboxInput(session, inputId = paste0("Apply.TRh"    ,i), value = FALSE)
                    if (input[[paste0("Apply.Invalid",i)]]) updateCheckboxInput(session, inputId = paste0("Apply.Invalid",i), value = FALSE)
                    if (input[[paste0("Apply.S.Out"  ,i)]]) updateCheckboxInput(session, inputId = paste0("Apply.S.Out"  ,i), value = FALSE)
                    if (input[[paste0("Apply.conv"   ,i)]]) updateCheckboxInput(session, inputId = paste0("Apply.conv"   ,i), value = FALSE)
                    if (input[[paste0("Apply.cal"    ,i)]]) updateCheckboxInput(session, inputId = paste0("Apply.cal"    ,i), value = FALSE)
                }
                for (i in seq_along(list.gas.reference())) {
                    if (input[[paste0("Apply.R.Out"  ,i)]]) updateCheckboxInput(session, inputId = paste0("Apply.R.Out", i), value = FALSE)
                }
                cat("-----------------------------------------------------------------------------------\n")
            }
        },
        priority = 120)
        # update Tabs for selected sensor ----
        observeEvent(input$Sensors, {
            if (input$Filtering.Sensors != input$Sensors) updateTabsetPanel(session, inputId = "Filtering.Sensors", selected = input$Sensors)
            if (input$Calib.Sensors     != input$Sensors) updateTabsetPanel(session, inputId = "Calib.Sensors"    , selected = input$Sensors)
            if (input$SetTime.Sensors   != input$Sensors) updateTabsetPanel(session, inputId = "SetTime.Sensors"  , selected = input$Sensors)
        } )
        # Reactive FUN CalSet ----
        # Get all parameters for Selected Sensor
        CalSet     <- eventReactive({
            # Selected Sensor
            input$Sensors
            list.gas.sensor()
            sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Ref.unit" , i)]])
            sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Sens.unit" , i)]])
            sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Sens.raw.unit" , i)]])
            input$Reference.name
            input$coord.ref.Long
            input$coord.ref.Lat
            AirSensEur.name()
            input$Selected
            sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Calibration" , i)]])
            sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Comparison" , i)]])
            sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Neg.mod" , i)]])
            sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Cal.Line" , i)]])
            sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Cal",i)]])
            sapply(seq_along(list.name.sensor()), function(i) input[[paste0("CovMod", i)]])
            sapply(seq_along(list.name.sensor()), function(i) input[[paste0("ubsRM" , i)]])
            sapply(seq_along(list.name.sensor()), function(i) input[[paste0("ubss" , i)]])
            sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Neg.mod" , i)]])
            ASE_name()
            sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Sync.Cal" , i)]])
            sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Sync.Pred" , i)]])
            sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DateMeas", i)]])
            sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DatePlotMeas", i)]])
            sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DateCal", i)]])
            sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DatePlotCal", i)]])
        },{
            # Get parameters for Selected Sensor
            # selecting the sensor name
            name.sensor <- input$Sensors
            # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
            k    <- match(x = name.sensor, table = list.name.sensor())
            # Selecting compounds associated with selected sensor
            gas.sensor    <-  list.gas.sensor()[k]
            # Determining the type of model use for calibration form the name of file input[[paste0("Cal",k)]]
            if (!is.null(input[[paste0("Cal",k)]]) && !is.na(input[[paste0("Cal",k)]]) && !is.nan(input[[paste0("Cal",k)]])) {
                if (input[[paste0("Cal",k)]] == "") NewCalSet <- "" else {
                    #type of model in the file name
                    NewCalSet <- strsplit(input[[paste0("Cal",k)]], split = "__")[[1]][2]
                    # check if it is a recognized Model type
                    if (!NewCalSet %in% Models) NewCalSet <- ""
                }
            }
            # initial value of NewcalSet if null after previous loop
            if (!exists("NewCalSet") || is.null(NewCalSet)) NewCalSet <- Models[1]
            # Limit Value, DQOs, UAT, LAT
            if (gas.sensor == "Carbon_monoxide") {
                if (input[[paste0("Ref.unit" , k)]] == "ppm") {
                    LV = 10/1.34
                    IT = NA
                    AT = NA
                } else if (input[[paste0("Ref.unit" , k)]] == "ppb") {
                    LV = 10000/1.34
                    IT = NA
                    AT = NA
                } else if (input[[paste0("Ref.unit" , k)]] == "mg/m3") {
                    LV = 10; IT = NA; AT = NA
                } else if (input[[paste0("Ref.unit" , k)]] == "ug/m3") {
                    LV = 10000; IT = NA; AT = NA
                }
                DQO.1 = 0.25 * LV
                DQO.2 = 0.50 * LV
                DQO.3 = 0.75 * LV
                LAT   = 0.50 * LV
                UAT   = 0.70 * LV
            } else if (gas.sensor == "Nitrogen_dioxide") {
                if (input[[paste0("Ref.unit" , k)]] == "ppb") {
                    LV = 200/1.91
                    IT = NA
                    AT = 400/1.91
                } else if (input[[paste0("Ref.unit" , k)]] == "ug/m3") {LV = 200; IT = NA; AT = 400}
                DQO.1 = 0.25 * LV
                DQO.2 = 0.50 * LV
                DQO.3 = 0.75 * LV
                LAT   = 0.50 * LV
                UAT   = 0.70 * LV
            } else if (gas.sensor == "Nitric_oxide") {
                if (input[[paste0("Ref.unit" , k)]] == "ppb") {
                    LV = 200/1.91
                    IT = NA
                    AT = 400/1.91
                } else if (input[[paste0("Ref.unit" , k)]] == "ug/m3") {LV = 200; IT = NA; AT = 400}
                DQO.1 = 0.25 * LV
                DQO.2 = 0.50 * LV
                DQO.3 = 0.75 * LV
                LAT   = 0.50 * LV
                UAT   = 0.70 * LV
            } else if (gas.sensor == "Ozone") {
                if (input[[paste0("Ref.unit" , k)]] == "ppb") {
                    LV = 120/2.05
                    IT = 180/2.05
                    AT = 240/2.05
                } else if (input[[paste0("Ref.unit" , k)]] == "ug/m3") {
                    LV = 120
                    IT = 180
                    AT = 240
                }
                DQO.1 = 0.30 * LV
                DQO.2 = 0.50 * LV
                DQO.3 = 0.75 * LV
                LAT   = NA
                UAT   = NA
            } else if (gas.sensor == "Sulphur_dioxide") {
                # Using LV for 1 year time average
                if (input[[paste0("Ref.unit" , k)]] == "ppb") {
                    LV = 120/2.05
                    IT = NA
                    AT = 500/2.05
                } else if (input[[paste0("Ref.unit" , k)]] == "ug/m3") {LV = 350; IT = NA; AT = 500}
                DQO.1 = 0.25 * LV
                DQO.2 = 0.50 * LV
                DQO.3 = 0.75 * LV
                LAT   = 0.40 * LV
                UAT   = 0.60 * LV
            } else if (gas.sensor == "Benzene") {
                # Using LV for 1 year time average
                if (input[[paste0("Ref.unit" , k)]] == "ppb") {
                    LV = 5/2.05
                    IT = NA
                    AT = NA
                } else if (input[[paste0("Ref.unit" , k)]] == "ug/m3") {LV = 5; IT = NA; AT = NA}
                DQO.1 = 0.30 * LV
                DQO.2 = 0.50 * LV
                DQO.3 = 1.00 * LV
                LAT   = 0.40 * LV
                UAT   = 0.70 * LV
            } else if (gas.sensor == "Particulate_Matter_10") {
                # Using LV for 24 hours time average
                LV    = 50
                IT    = NA
                AT    = NA
                DQO.1 = 0.50 * LV
                DQO.2 = 0.50 * LV
                DQO.3 = 1.00 * LV
                LAT   = 0.50 * LV
                UAT   = 1.00 * LV
            }  else if (gas.sensor == "Particulate_Matter_25") {
                # Using LV for 24 hours time average
                LV    = 25
                IT    = NA
                AT    = NA
                DQO.1 = 0.50 * LV
                DQO.2 = 0.50 * LV
                DQO.3 = 1.00 * LV
                LAT   = 0.50 * LV
                UAT   = 1.00 * LV
            }
            CalSet <- data.frame(
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                k                  = k,
                # Selecting compounds associated with selected sensor
                gas.sensor         = gas.sensor,
                name.sensor        = name.sensor,
                # selecting the sensor
                name.gas           = list.name.gas()[match(x = name.sensor, table = list.name.sensor())],
                name.reference     = list.gas.reference2use()[k],
                # Setting parameters
                nameGasRef         = paste0("Out.",list.gas.reference2use()[k]),  # reference gas
                nameGasVolt        = paste0(name.sensor,"_volt"),                                     # sensor gas in volt
                nameGasMod         = paste0(gas.sensor,"_modelled"),                                  # modelled sensor gas
                unit.ref           = input[[paste0("Ref.unit" , k)]],
                unit.sensor        = input[[paste0("Sens.unit", k)]],
                Sens.raw.unit      = input[[paste0("Sens.raw.unit", k)]],
                Reference.name     = input$Reference.name,
                coord.ref          = paste0(input$coord.ref.Long,",", input$coord.ref.Lat),           # Hand written coordinates of the reference station
                AirSensEur.name    = AirSensEur.name(),
                WDoutputMod        = file.path(input$Selected,"Models"),
                WDoutput           = file.path(input$Selected,"Calibration"),
                WDoutputStats      = file.path(input$Selected,"Statistics"),
                WDModelled_gas     = file.path(input$Selected,"Modelled_gas"),
                WDOutliers         = file.path(input$Selected,"Outliers"),
                mod.eta.model.type = input[[paste0("Calibration",k)]],                                # Model for calibration"
                NewCalSet          = NewCalSet,                                                       # Model of the  CalSet()$Cal
                eta.model.type     = input[[paste0("Comparison",k)]],
                remove.neg         = input[[paste0("Neg.mod",k)]],
                Cal_Line           = input[[paste0("Cal.Line",k)]],                                   # "Equation of calibation/Prediction"
                Cal                = paste0(AirSensEur.name(),"__",name.sensor,"__",input[[paste0("Cal",k)]]),# Selected calibration model  for the current sensor
                Multi.File         = file.path(input$Selected,"General_data", paste0(ASE_name(),"_Multi_",input$Sensors,".cfg")), # Config file for calibration with Multivariables
                CovMod             = paste0(input[[paste0("CovMod",k)]], collapse = "&"),             # Selected List of covariates to calibrate
                LV                 = LV ,                                                             # limit for the gas.sensor
                LAT                = LAT ,                                                            # limit for the gas.sensor
                UAT                = UAT ,                                                            # limit for the gas.sensor
                AT                 = AT ,                                                             # limit for the gas.sensor
                DQO.1              = DQO.1,                                                           # Data quality Objective for the gas.sensor
                DQO.2              = DQO.2,                                                           # Data quality Objective for the gas.sensor
                DQO.3              = DQO.3,                                                           # Data quality Objective for the gas.sensor
                ubsRM              = as.numeric(input[[paste0("ubsRM",k)]]) ,                         # Between sampler uncertainty of the reference data
                ubss               = as.numeric(input[[paste0("ubss",k)]]) ,                         # Between sampler uncertainty of the sensor data
                Neg.mod            = as.logical(input[[paste0("Neg.mod",k)]]),                        # Remove negative predicted data
                Sync.Cal           = as.logical(input[[paste0("Sync.Cal",k)]]),                       # Synchronise sensor and reference before calibration
                Sync.Pred          = as.logical(input[[paste0("Sync.Pred",k)]]),                      # Synchronise sensor and reference when comparing prediction
                Cov.DateIN         = input[[paste0("Date",k)]][1],
                Cov.DateEND        = input[[paste0("Date",k)]][2],
                Cal.DateIN         = max(c(input[[paste0("DateCal",k)]][1],input[[paste0("DatePlotCal",k)]][1]), na.rm = TRUE),
                Cal.DateEND        = min(c(input[[paste0("DateCal",k)]][2],input[[paste0("DatePlotCal",k)]][2]), na.rm = TRUE),
                Meas.DateIN        = max(c(input[[paste0("DateMeas",k)]][1],input[[paste0("DatePlotMeas",k)]][1]), na.rm = TRUE),
                Meas.DateEND       = min(c(input[[paste0("DateMeas",k)]][2],input[[paste0("DatePlotMeas",k)]][2]), na.rm = TRUE),
                stringsAsFactors = FALSE)
            return(CalSet)
        })
        # NavBar"Data Treatment", mainTabPanel "Map",  ----
        pointsCal <- reactive( {
            # Selecting dates and coordinates
            Available.Coord <- grep(pattern = paste0(c("latitude","longitude", "Ref.Long", "Ref.Lat"), collapse = "|" ), x = names(DF$General), value = TRUE)
            PointsCal <- DF$General[which(date >= CalSet()$Cal.DateIN & date <= CalSet()$Cal.DateEND + 1), .SD, .SDcols = c("date", Available.Coord)]
            # Determining coordinates ofreference station
            if ("Ref.Long" %in% names(PointsCal) && "Ref.Lat" %in% names(PointsCal) && any(!is.na(PointsCal$Ref.Long)) && any(!is.na(PointsCal$Ref.Lat))) {
                Ref.coord_LON_LAT <- unique(PointsCal[!is.na(Ref.Long) & !is.nan(Ref.Long), .SD, .SDcols = c("Ref.Long","Ref.Lat")])
                Ref.coord_LON <- Ref.coord_LON_LAT[["Ref.Long"]]
                Ref.coord_LAT <- Ref.coord_LON_LAT[["Ref.Lat"]]
            } else if (!is.null(CalSet()$coord.ref) && grepl(pattern = paste0(c(" ", ","), collapse = "|"), x = CalSet()$coord.ref)) {
                # checking if the separator is ,
                if (any(grepl(pattern =  ",", x = CalSet()$coord.ref))) {
                    # Checking is the coordinates are in spherical or decimal format, projection to OpenStreet map
                    if (any(grep(pattern = paste0(c("N","S", "E", "W", "d"), collapse = "|" ), x = CalSet()$coord.ref))) {
                        # extract spherical coordinates
                        Ref.coord_LAT  <- unlist(strsplit(x = CalSet()$coord.ref, split = ","))[2]
                        Ref.coord_LON  <- unlist(strsplit(x = CalSet()$coord.ref, split = ","))[1]
                        # transform spherical coordinates to decimal degrees for later projection
                        Ref.coord_d    <- OSMscale::degree(Ref.coord_LAT, Ref.coord_LON, digits = 5)
                        # Project the spherical coordinates in Mercator web WS84 of OPenStreet view
                        #Ref.coord_p    <- OSMscale::projectPoints(Ref.coord_d[1], Ref.coord_d[2], to=OSMscale::pll())
                        Ref.coord_LAT  <- Ref.coord_d[1,1]
                        Ref.coord_LON  <- Ref.coord_d[1,2]
                    } else {
                        my_message <- paste0("[shiny, pointsCal()] ERROR, the coordinates of the reference station are not separated with a comma.\n")
                        cat(my_message)
                        shinyalert(
                            title = "ERROR missing data",
                            text = my_message,
                            closeOnEsc = TRUE,
                            closeOnClickOutside = TRUE,
                            html = FALSE,
                            type = "error",
                            showConfirmButton = TRUE,
                            showCancelButton  = FALSE,
                            confirmButtonText = "OK",
                            confirmButtonCol  = "#AEDEF4",
                            timer             = 0,
                            imageUrl          = "",
                            animation         = FALSE)}
                } else {
                    Ref.coord_LON <- as.numeric(unlist(strsplit(x = CalSet()$coord.ref, split = " "))[1])
                    Ref.coord_LAT <- as.numeric(unlist(strsplit(x = CalSet()$coord.ref, split = " "))[2])}
            } else {
                my_message <- paste0("[shiny, pointsCal()] ERROR, the coordinates of the reference station are incorrect\n")
                cat(my_message)
                shinyalert(
                    title = "ERROR Incorrect coordinates",
                    text = my_message,
                    closeOnEsc = TRUE,
                    closeOnClickOutside = TRUE,
                    html = FALSE,
                    type = "error",
                    showConfirmButton = TRUE,
                    showCancelButton  = FALSE,
                    confirmButtonText = "OK",
                    confirmButtonCol  = "#AEDEF4",
                    timer             = 0,
                    imageUrl          = "",
                    animation         = FALSE
                )
                Ref.coord_LON <- NULL
                Ref.coord_LAT <- NULL
            }
            # mean coordinates of the AirSensEUR
            PointsCal <- DF$General[which(date >= CalSet()$Cal.DateIN & date <= CalSet()$Cal.DateEND + 1 &
                                              !is.na(longitude) & !is.nan(longitude) & !is.na(latitude) & !is.nan(latitude)), .SD, .SDcols = c("date", Available.Coord)]
            if (PointsCal[,.N] > 0) {
                Names.coord.Station <- c("latitude", "longitude")
                PointsCal[, latitude  := round(PointsCal[["latitude"]] , digits = 4)]
                PointsCal[, longitude := round(PointsCal[["longitude"]], digits = 4)]
                MEAS_LON_LAT <- unique(PointsCal[, date := mean(date), by = list(latitude, latitude)])[, .SD, .SDcols = c("date", "latitude", "longitude") ]
                # # Position coordinates of the AirSensEUR
                # # make a spatial dataframe with traffic data
                sp_traffic_real <- SpatialPointsDataFrame(MEAS_LON_LAT[,c("longitude","latitude")], MEAS_LON_LAT[,],     # lat, lon
                                                          proj4string = CRS("+init=epsg:4326"))
                # make intersection between open street and the buffer
                buffer_sp_traffic_real <- rgeos::gBuffer(sp_traffic_real, width = 0.0002)  #0.00005, units in grade
                class(buffer_sp_traffic_real)
                (p.df <- data.frame( ID = 1:length(buffer_sp_traffic_real)))
                # # trasform SpatialPolygons to SpatialPolygonsDataFrame
                buffer_sp_traffic_real <- SpatialPolygonsDataFrame(buffer_sp_traffic_real, p.df, match.ID = F)
                buffer_sp_traffic_real <- ggplot2::fortify(buffer_sp_traffic_real)
                MEAS_LON_LAT <- buffer_sp_traffic_real %>%
                    group_by(group) %>%
                    summarise(longitude = mean(long),
                              latitude  = mean(lat))
                popup_REF <- paste0("<strong><i>", "reference stn. @ " ,
                                    round(Ref.coord_LON, digits = 4), ", " ,
                                    round(Ref.coord_LAT, digits = 4), "</i></strong>")
                popup_CAL <- paste0("<strong><i>", "AirSensEUR box. @ ",
                                    round(MEAS_LON_LAT$longitude, digits  = 4), ", " ,
                                    round(MEAS_LON_LAT$latitude , digits  = 4), ", " ,
                                    #format(MEAS_LON_LAT$date, "%Y-%m-%d %H:%M"),
                                    "</i></strong>")
                return(list(Ref.coord_LON = Ref.coord_LON,
                            Ref.coord_LAT = Ref.coord_LAT,
                            Cal_LON = MEAS_LON_LAT$longitude,
                            Cal_LAT = MEAS_LON_LAT$latitude,
                            popup_REF = popup_REF,
                            popup_CAL = popup_CAL))
            } else {
                my_message <- paste0("[shiny, pointsCal()] ERROR, there are no coordinates of the AirSensEUR box\n")
                cat(my_message)
                shinyalert(
                    title = "INFO no AirSensEUR coordinates available",
                    text = my_message,
                    closeOnEsc = TRUE,
                    closeOnClickOutside = TRUE,
                    html = FALSE,
                    type = "info",
                    showConfirmButton = TRUE,
                    showCancelButton  = FALSE,
                    confirmButtonText = "OK",
                    confirmButtonCol  = "#AEDEF4",
                    timer             = 0,
                    imageUrl          = "",
                    animation         = FALSE
                )
                return(list(Ref.coord_LON = Ref.coord_LON,
                            Ref.coord_LAT = Ref.coord_LAT,
                            Cal_LON = NULL,
                            Cal_LAT = NULL,
                            popup_REF = NULL,
                            popup_CAL = NULL))
            }
        })
        output$mymapCal <- renderLeaflet({
            # Map of location of AirSensEUR and reference station during calibration
            if ((!is.null(pointsCal()$Cal_LON) && !is.null(pointsCal()$Cal_LAT)) || (!is.null(pointsCal()$Ref.coord_LON) && !is.null(pointsCal()$Ref.coord_LAT))) {
                title_CAL <- paste0('<h0><strong>', "Position of ", CalSet()$AirSensEur.name, " during calibration",
                                    "</i></strong><br> The blue pointer is the location of the reference station, <br> the grey circle is the location of the AirSensEur during calibration")
                m <- leaflet() %>%
                    addTiles(group = "OSM (default)") %>%
                    addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
                    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
                    addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
                    addLayersControl(
                        baseGroups = c("Road map", "Satellite", "Toner Lite"),
                        options = layersControlOptions(collapsed = TRUE))
                    #setView(lng = mean(pointsCal()$Cal_LON), lat = mean(pointsCal()$Cal_LAT), zoom = 10)
                if (!is.null(pointsCal()$Cal_LON) && !is.null(pointsCal()$Cal_LAT)) {
                    m <- m %>%
                        fitBounds(lng1 = min(pointsCal()$Cal_LON, na.rm = T),
                                  lat1 = min(pointsCal()$Cal_LAT, na.rm = T),
                                  lng2 = max(pointsCal()$Cal_LON, na.rm = T),
                                  lat2 = max(pointsCal()$Cal_LAT, na.rm = T),
                                  options = list(maxZoom = 16)) %>%
                        addCircleMarkers(lng = pointsCal()$Cal_LON, lat = pointsCal()$Cal_LAT,
                                         popup = pointsCal()$popup_CAL, opacity = 1, color = "black", fillOpacity = 0.7, radius = 5, weight = 1)}
                if (!is.null(pointsCal()$Ref.coord_LON) && !is.null(pointsCal()$Ref.coord_LAT)) {
                    m <- m %>%
                        addMarkers(lng = pointsCal()$Ref.coord_LON, lat = pointsCal()$Ref.coord_LAT,
                                   popup = pointsCal()$popup_REF) %>%
                        addPopups(pointsCal()$Ref.coord_LON,
                                  pointsCal()$Ref.coord_LAT +
                                      max(c(0.001,0.1*sqrt(diff(range(pointsCal()$Cal_LON))^2+diff(range(pointsCal()$Cal_LAT))^2)), na.rm = T),
                                  title_CAL,
                                  options = popupOptions(closeOnClick  = FALSE))}
                m
            }
        })
        # NavBar"Data Treatment", mainTabPanel "Calibration",  ----
        output$Calibration   <- renderPlot(Plot.Calibration()      , width = 'auto', height = 'auto')
        # NavBar"Data Treatment", mainTabPanel "Calibration"-"MultiLinear" ----
        observeEvent({
            sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Calibration",i)]])
            sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Cal.Line",i)]])
        },{
            # SideBarLayout Calib, hiding CovMod and MultiLinear MainTabPanel ----
            
            # checking which claibration type is requested
            if (input[[paste0("Cal.Line",CalSet()$k)]] == "Calibration with current data") {
                
            } else if (input[[paste0("Cal.Line",CalSet()$k)]] == "Prediction with previous calibration") {
                shinyjs::hide(id = paste0("Slope",CalSet()$k))
                shinyjs::hide(id = paste0("Intercept",CalSet()$k))
                shinyjs::show(id = paste0("Calibration",CalSet()$k) )
                shinyjs::show(id = paste0("Cal",CalSet()$k) )
                # Hiding or showing TabPanel MultiLinear and Select Input CovMod
                if (!is.null(input[[paste0("Calibration",CalSet()$k)]])) {
                    if (input[[paste0("Calibration",CalSet()$k)]] == "MultiLinear") {
                        shinyjs::show(id = paste0("CovMod",CalSet()$k) )
                        # Showing the rhansome table to edit the MultiLinear file
                        shinyjs::show(id = "Multi" )
                        shinyjs::show(id = "Save.row.Multi")
                        shinyjs::show(id = "Del.row.Multi")
                        #showTab(inputId = "TabCalibration", target = "MultiLinear")
                    } else {
                        shinyjs::hide(id = paste0("CovMod",CalSet()$k) )
                        shinyjs::hide(id = "Multi" )
                        shinyjs::hide(id = "Save.row.Multi")
                        shinyjs::hide(id = "Del.row.Multi")
                        #hideTab(inputId = "TabCalibration", target = "MultiLinear")
                    }
                }
            } else if (input[[paste0("Cal.Line",CalSet()$k)]] == "Calibration with slope and intercept") {
                shinyjs::show(id = paste0("Slope",CalSet()$k))
                shinyjs::show(id = paste0("Intercept",CalSet()$k))
                shinyjs::hide(id = paste0("Calibration",CalSet()$k) )
                shinyjs::hide(id = paste0("Cal",CalSet()$k) )
                shinyjs::hide(id = paste0("CovMod",CalSet()$k) )
                shinyalert(
                    title = "Info not implemented yet!",
                    text = "Calibration with slope and internet not yet implemented",
                    closeOnEsc = TRUE,
                    closeOnClickOutside = TRUE,
                    html = FALSE,
                    type = "error",
                    showConfirmButton = TRUE,
                    showCancelButton  = FALSE,
                    confirmButtonText = "OK",
                    confirmButtonCol  = "#AEDEF4",
                    timer             = 2000,
                    imageUrl          = "",
                    animation         = FALSE)
            }
        }, priority = 800)
        # Reactive Multi.DF
        Multi.DF <- eventReactive(CalSet(),{
            # Return DataFrame for editing model wjrn calibration with MultiLinear is selected
            # depends on:
            #  input$Selected, input$Sensors, CalSet(), ASE_name()
            if (CalSet()$mod.eta.model.type == "MultiLinear") {
                nameFile <- file.path(input$Selected,"General_data",paste0(ASE_name(),"_Multi_",input$Sensors,".cfg"))
                # UI covariates
                names.Covariates <- unlist(strsplit(CalSet()$CovMod, split = "&"))
                # Is ther a multivariate file?
                if (file.exists(nameFile)) {
                    Multi.DF <- read.table(file             = nameFile,
                                           header           = TRUE,
                                           row.names        = NULL,
                                           comment.char     = "#"
                                           # , stringsAsFactors = FALSE
                    )
                    #DF <- dplyr::arrange(DF,Variables)
                    # checking that the UI covariates and the covariates in the file are consistent
                    if (length(names.Covariates) != length(Multi.DF$Covariates[-which(Multi.DF$Covariates == "Intercept")]))  {
                        # not the same number of Covariates in Multi.file and selected -> new Multi.df
                        Multi.DF <- data.frame(Covariates = c(names.Covariates, "Intercept"),
                                               Enabled    = rep(TRUE      , length(c(names.Covariates, "Intercept"))),
                                               degree     = factor(c(rep("1", length(names.Covariates)), NA), levels = c("1","1.75","2","3","0","ExpGrowth"), ordered = TRUE) ,
                                               Forced     = rep(FALSE     , length(c(names.Covariates, "Intercept"))),
                                               a0_an      = rep("1"       , length(c(names.Covariates, "Intercept"))))
                        # , stringsAsFactors = FALSE)
                    } else if (!all(names.Covariates %in% Multi.DF$Covariates[-which(Multi.DF$Covariates == "Intercept")] &
                                    Multi.DF$Covariates[-which(Multi.DF$Covariates == "Intercept")] %in% names.Covariates)) {
                        # not the same Covariates in Multi.file and selected -> new Multi.df
                        Multi.DF <- data.frame(Covariates = c(names.Covariates, "Intercept"),
                                               Enabled    = rep(TRUE      , length(c(names.Covariates, "Intercept"))),
                                               degree     = factor(c(rep("1", length(names.Covariates)), NA), levels = c("1","1.75","2","3","0","ExpGrowth"), ordered = TRUE) ,
                                               Forced     = rep(FALSE     , length(c(names.Covariates, "Intercept"))),
                                               a0_an      = rep("1"       , length(c(names.Covariates, "Intercept"))))
                        # , stringsAsFactors = FALSE
                    }
                } else {
                    Multi.DF <- data.frame(Covariates = c(names.Covariates, "Intercept"),
                                           Enabled    = rep(TRUE      , length(c(names.Covariates, "Intercept"))),
                                           degree     = factor(c(rep("1", length(names.Covariates)), NA), levels = c("1","1.75","2","3","0","ExpGrowth"), ordered = TRUE) ,
                                           Forced     = rep(FALSE     , length(c(names.Covariates, "Intercept"))),
                                           a0_an      = rep("1"       , length(c(names.Covariates, "Intercept"))))
                    # , stringsAsFactors = FALSE
                }
                return(Multi.DF)
            } else return(NULL)
        })
        output$Multi <- rhandsontable::renderRHandsontable({
            # converts Multi.DF() to rhandsontable object
            if (!is.null(Multi.DF())) rhandsontable::rhandsontable(Multi.DF()) %>% hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
        })
        Multi.Models <- reactive({
            # Make it reactive to input$New.row.Multi, input$Save.row.Multi, input$Del.row.Multi
            #input$New.row.Multi
            input$Save.row.Multi
            input$Del.row.Multi
            # Existing MultiLinear files
            List.Multi.Files <- list.files(path    = file.path(input$Selected,"General_data"), pattern = glob2rx(paste0("*Multi*", input$Sensors, "*")))
            # Creating the text files to render
            Multi.Lignes <- paste0("Existing MultiLinear File in ASE/General_Data:\n")
            Multi.Lignes <- paste(Multi.Lignes,"\n")
            if (length(List.Multi.Files) > 0) {
                for (i in List.Multi.Files) {
                    # Adding the name of the Multivariate file to Multi.Lignes
                    Multi.Lignes <- paste(Multi.Lignes, i, sep = "\n")
                    cat(paste0("Existing MultiLinear Files in ASE/General_Data:", i))
                    # reading Multivarites files line by line
                    con = file(description = file.path(input$Selected,"General_data", i), "r")
                    repeat {
                        Multi.1Ligne = readLines(con, n = 1)
                        # exiting if list is empty or appending the line to Multi.Lignes
                        if (length(Multi.1Ligne) == 0) break else {
                            Multi.Lignes <- paste(Multi.Lignes, Multi.1Ligne, sep = "\n")
                            cat(paste0(Multi.1Ligne, "\n"))
                        }
                    }
                    close(con)
                    Multi.Lignes <- paste0(Multi.Lignes,"\n")
                }
            } else Multi.Lignes <- paste(Multi.Lignes, "No Multivariate file for this sensor.", sep = "\n")
            return(Multi.Lignes)
        })
        output$ListValid <- renderText({
            # list of _Multi_ files of calibration for all sensors
            Multi.Models()
        })
        # Del
        observeEvent(input$Del.row.Multi, {
            # delete current Multi file
            nameFile <- file.path(input$Selected,"General_data",paste0(ASE_name(),"_Multi_",input$Sensors,".cfg"))
            file.remove(nameFile)
        })
        ## Save
        observeEvent(input$Save.row.Multi, {
            # rows must be in increasing order before saving otherwise the following error stops the script:
            # Warning: Error in seq.default: 'by' must be of length 1
            finalDF <- hot_to_r(input$Multi)
            nameFile <- file.path(input$Selected,"General_data",paste0(ASE_name(),"_Multi_",input$Sensors,".cfg"))
            write.table(finalDF, file = nameFile, row.names = FALSE)
            #click(id = "New.row.Multi")
        })
        # ObserveEvent Buttons to set the ranges of dates for calibration ----
        # Setting the date of DateCal1 according to the name of the calibration model when selecting another model
        observeEvent({
            sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DateCALCal",i)]])
        }, {
            Splitted.Cal <- unlist(strsplit(x = paste0(CalSet()$AirSensEur.name,"__",CalSet()$name.sensor,"__",input[[paste0("Cal",CalSet()$k)]]), split = "__"))
            # JRC_02__COMF200__nA__Linear.Robust__20170109__20170113__.rds
            Start <- strptime(Splitted.Cal[5], format = "%Y%m%d", tz = threadr::time_zone(DF$General$date[1]))
            End   <- strptime(Splitted.Cal[6], format = "%Y%m%d", tz = threadr::time_zone(DF$General$date[1]))
            updateDateRangeInput(session,
                                 inputId = paste0("DateCal",CalSet()$k),
                                 start   = Start,
                                 end     = End)
        },
        ignoreInit = TRUE
        )
        # Setting the dates DateCal1 to the one of input$Date1 (Covariates)
        observeEvent({sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DateCALCovCal",i)]])
        }, {
            #if (input$DateCALCovCal1>0) {
            Start <- input[[paste0("Date",CalSet()$k)]][1]
            End   <- input[[paste0("Date",CalSet()$k)]][2]
            updateDateRangeInput(session,
                                 inputId = paste0("DateCal",CalSet()$k),
                                 start   = Start,
                                 end     = End)
            updateDateRangeInput(session,
                                 inputId = paste0("DatePlotCal",CalSet()$k),
                                 start   = Start,
                                 end     = End)
        },
        ignoreInit = TRUE
        )
        # Setting the dates input$Date1 (Covariates)  to the one of DateCal1
        observeEvent({sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DateCOVCal",i)]])
        }, {
            #if (input$DateCALCovCal1>0) {
            Start <- input[[paste0("DateCal",CalSet()$k)]][1]
            End   <- input[[paste0("DateCal",CalSet()$k)]][2]
            updateDateRangeInput(session,
                                 inputId = paste0("Date",CalSet()$k),
                                 start   = Start,
                                 end     = End)
        },
        ignoreInit = TRUE
        )
        # Setting the dates DateCal1 to the one of input$DatePlotMeas1
        observeEvent({sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DateCALExtCal",i)]])
        }, {
            #if (input$DateCALExtCal1>0) {
            Start <- input[[paste0("DatePlotMeas",CalSet()$k)]][1]
            End   <- input[[paste0("DatePlotMeas",CalSet()$k)]][2]
            updateDateRangeInput(session,
                                 inputId = paste0("DateCal",CalSet()$k),
                                 start   = Start,
                                 end     = End)
        },
        ignoreInit = TRUE
        )
        #observeEvent to calibrate
        observeEvent({
            CalSet()$Cal_Line # trigger on "Method of Prediction" for new calibration model
        },{
            if (CalSet()$Cal_Line == "Calibration with current data") {
                # put here the calibration function
                # Create a Progress object
                progress <- shiny::Progress$new()
                progress$set(message = "[shiny] INFO, New Calibration Function", value = 0.5)
                # Make sure it closes when we exit this reactive, even if there's an error
                on.exit(progress$close())
                cat("\n")
                cat("-----------------------------------------------------------------------------------\n")
                cat(paste0("[shiny] INFO, New Calibration Function for ", input$Sensors, sep = "\n"))
                # 1 - if "Calibration with current data" is selected
                #     1.1 Check that the model does not already exist for the same sensor, model type, unit, dateIN/date/END and covariates
                #     1.2 Check that the model does not already exist for Covariates. If model does not alrady exist then calibrate
                # 1
                if (CalSet()$Cal_Line == "Calibration with current data") {
                    # 1.1 Check that the model does not already exist for the same sensor, model type, unit, dateIN/date/END
                    # List of files for current AirSensEUR name, sensor name, unit, model type, start and end dates
                    ModelFiles <-  list.files(path = CalSet()$WDoutputMod,
                                              pattern = glob2rx(
                                                  paste0(CalSet()$AirSensEur.name,"__",
                                                         CalSet()$name.sensor,"__",
                                                         CalSet()$Sens.raw.unit,"_",
                                                         CalSet()$mod.eta.model.type,"__",
                                                         format(CalSet()$Cal.DateIN ,"%Y%m%d"),"__",format(CalSet()$Cal.DateEND,"%Y%m%d"),"__*", "*")))
                    # Flag to request new model
                    NewModelFlag <- TRUE
                    # Model exist with current AirSensEUR name, sensor name, unit, model type, start and end dates?
                    if (!identical(ModelFiles,character(0))) {
                        # Checking if within existing ModelFiles there is the same set of covariates only for MultiLinear Models
                        if (CalSet()$mod.eta.model.type == "MultiLinear") {
                            # The model use covariates, it is fine to ask for a model with same AirSensEUR name, sensor name, unit, model type, start and end dates, if other covariates are requested
                            # listing the Covariates
                            for (i in ModelFiles) {
                                # Covariates in the selected model - MISTAKE ON Covariates.Model
                                Splitted.Model      <- unlist(strsplit(x = CalSet()$Cal, split = "__"))
                                Covariates.Model    <- str_replace(Splitted.Model[7], pattern = ".rdata", replacement = "")
                                Covariates.Model    <- unlist(strsplit(x = Covariates.Model , split = "&"))
                                # Co-Variates selected in UI
                                Covariates.CovMod <- unlist(strsplit(x = CalSet()$CovMod, split = "&"))
                                if (file.exists(CalSet()$Multi.File)) {
                                    # read Multi.File
                                    Multi.File.df <-  read.table(file             = CalSet()$Multi.File,
                                                                 header           = TRUE,
                                                                 row.names        = NULL,
                                                                 comment.char     = "#",
                                                                 stringsAsFactors = FALSE
                                    )
                                    # degree of polynomial of all Co_Variates
                                    Degrees <-  Multi.File.df[Multi.File.df$Covariates %in% Covariates.CovMod, "degree"]
                                    Covariates.CovMod <- paste(unlist(strsplit(split = "&", CalSet()$CovMod)), Degrees, sep = "-")
                                } else if (identical(Covariates.Model, character(0))) {
                                    # Set to "" to be able to test all(Covariates.Model %in% Covariates.CovMod) & all(Covariates.CovMod %in%  Covariates.Model)
                                    Covariates.Model = ""
                                    Degrees <-  base::rep(1, times = length(Covariates.CovMod))
                                } else if (any(grepl(pattern = "-", x = Covariates.Model))) {
                                    # When more than one covariates is selected for fitting
                                    Degrees <-  base::rep(1, times = length(Covariates.CovMod))
                                    Covariates.CovMod <- paste(unlist(strsplit(split = "&", CalSet()$CovMod)), Degrees, sep = "-")
                                }
                                # Checking if model i is the model asked to fit and already exists
                                if (all(Covariates.Model %in% Covariates.CovMod) &
                                    all(Covariates.CovMod %in%  Covariates.Model)
                                ) {
                                    # Set that the model with the list of coavariates already exists
                                    NewModelFlag <- FALSE
                                    break
                                }
                            }
                        } else NewModelFlag <- FALSE # Model already exists (we are sure it exists)
                    }
                    if (NewModelFlag) {
                        cat(paste0("[shiny, Plot.Calibration()], INFO, new calibration with ", isolate(CalSet()$mod.eta.model.type), " calibration method between ",
                                   format(CalSet()$Cal.DateIN ,"%Y%m%d")," and ",format(CalSet()$Cal.DateEND,"%Y%m%d"),"\n")) # ADD Calibration DATES
                        # Setting parameters
                        model.log          = TRUE
                        timeseries.display = FALSE
                        process.step       = "Calibration"
                        # Checking if there are data to calibrate
                        General.df <- DF$General[date >= CalSet()$Cal.DateIN & date <= CalSet()$Cal.DateEND + 1]
                        if (all(is.na(General.df[[CalSet()$nameGasRef]])) || all(is.na(General.df[[CalSet()$nameGasVolt]]))) {
                            # Message missing data
                            my_message <- paste0("[shiny, Plot.Calibration()] ERROR, No data for calibration for sensor ", CalSet()$name.sensor,
                                                 " in  the \"Range of date for calibration:\" under \"SetTime\". Change date or use \"New calibration with current data\".\n")
                            cat(my_message)
                            shinyalert(
                                title = "ERROR missing data",
                                text = my_message,
                                closeOnEsc = TRUE,
                                closeOnClickOutside = TRUE,
                                html = FALSE,
                                type = "error",
                                showConfirmButton = TRUE,
                                showCancelButton  = FALSE,
                                confirmButtonText = "OK",
                                confirmButtonCol  = "#AEDEF4",
                                timer             = 0,
                                imageUrl          = "",
                                animation         = FALSE)
                            plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                            text(1,1,my_message)
                        } else {
                            Covariates <- unlist(strsplit(split = "&",CalSet()$CovMod))
                            is.Cov.Ok  <- TRUE
                            if (file.exists(CalSet()$Multi.File)) {
                                # read Multi.File
                                Multi.File.df <-  read.table(file             = CalSet()$Multi.File,
                                                             header           = TRUE,
                                                             row.names        = NULL,
                                                             comment.char     = "#",
                                                             stringsAsFactors = FALSE
                                )
                                if (!identical(Covariates , Multi.File.df$Covariates[-which(Multi.File.df$Covariates == "Intercept")])) {
                                    my_message <- paste0("ERROR, multilinear calibration requested \n
                                                     with covariates list different or not ordered as the list \n
                                                     in MultiLinear file list.\n
                                                     Change the \"Covariates for calibration\" or change-delete the MultiLinear file.\n
                                                     The app  may crash.\n")
                                    cat(my_message)
                                    shinyalert(
                                        title = "Wrong Co-variates",
                                        text = my_message,
                                        closeOnEsc = TRUE,
                                        closeOnClickOutside = TRUE,
                                        html = FALSE,
                                        type = "error",
                                        showConfirmButton = TRUE,
                                        showCancelButton  = FALSE,
                                        confirmButtonText = "OK",
                                        confirmButtonCol  = "#AEDEF4",
                                        timer             = 0,
                                        imageUrl          = "",
                                        animation         = FALSE)
                                    is.Cov.Ok <- FALSE
                                }
                            }
                            # Calibration model
                            if (is.Cov.Ok #&& !file.exists(CalSet()$Multi.File) || (file.exists(CalSet()$Multi.File) && identical(Covariates, Multi.File.df$Covariates[-which(Multi.File.df$Covariates == "Intercept")])
                            ) {
                                Validation.tool(General            = General.df,
                                                DateIN             = CalSet()$Cal.DateIN ,
                                                DateEND            = CalSet()$Cal.DateEND,
                                                name.gas           = CalSet()$name.gas,
                                                model.log          = model.log ,
                                                nameGasRef         = CalSet()$nameGasRef,   # reference gas
                                                nameGasVolt        = CalSet()$nameGasVolt,  # sensor gas in volt (or nA)
                                                nameGasMod         = CalSet()$nameGasMod,   # modelled sensor gas
                                                unit.ref           = CalSet()$unit.ref,
                                                unit.sensor        = CalSet()$unit.sensor,
                                                Sens.raw.unit      = CalSet()$Sens.raw.unit,
                                                Reference.name     = CalSet()$Reference.name,
                                                AirSensEur.name    = CalSet()$AirSensEur.name,
                                                name.sensor        = CalSet()$name.sensor,
                                                timeseries.display = timeseries.display ,
                                                WDoutputMod        = CalSet()$WDoutputMod,
                                                WDoutput           = CalSet()$WDoutput,
                                                WDoutputStats      = CalSet()$WDoutputStats,
                                                process.step       = process.step,
                                                mod.eta.model.type = isolate(CalSet()$mod.eta.model.type),
                                                Multi.File         = CalSet()$Multi.File,
                                                eta.model.type     = CalSet()$eta.model.type,
                                                remove.neg         = CalSet()$remove.neg,
                                                Covariates         = unlist(strsplit(split = "&",CalSet()$CovMod)),
                                                PlotCal            = FALSE,
                                                Auto.Lag           = CalSet()$Sync.Cal)
                                # updating the calibration function in GUI and claibration to use "Prediction with previous calibration"
                                updateRadioButtons(session, inputId = paste0("Cal.Line", CalSet()$k), label = "Method of Prediction",
                                                   choices = list("Calibration with current data","Prediction with previous calibration","Calibration with slope and intercept"),
                                                   selected = "Prediction with previous calibration")
                                # Removing the name of AirSensEUR from the model because there may be a confusion between Influx name and SOS name
                                Newchoices <- substr(list.files(path    = CalSet()$WDoutputMod,
                                                                pattern = glob2rx(paste0(CalSet()$AirSensEur.name,"__",CalSet()$name.sensor,"__*"))),
                                                     start = nchar(paste0(CalSet()$AirSensEur.name,"__",CalSet()$name.sensor,"__")) + 1,
                                                     stop  = nchar(list.files(path    = CalSet()$WDoutputMod,
                                                                              pattern = glob2rx(paste0(CalSet()$AirSensEur.name,"__",CalSet()$name.sensor,"__*")))))
                                # Current model
                                # Use separator "__" because the character "_" maybe be used in the name of ASE
                                if (CalSet()$mod.eta.model.type == "MultiLinear") {
                                    if (file.exists(CalSet()$Multi.File)) {
                                        # read Multi.File
                                        Multi.File.df <-  read.table(file             = CalSet()$Multi.File,
                                                                     header           = TRUE,
                                                                     row.names        = NULL,
                                                                     comment.char     = "#",
                                                                     stringsAsFactors = FALSE)
                                        # their degree of polynomial
                                        Degrees <-  Multi.File.df[Multi.File.df$Covariates %in% unlist(strsplit(split = "&",CalSet()$CovMod)), "degree"]
                                    } else Degrees <-  base::rep(1, times = length(unlist(strsplit(split = "&",CalSet()$CovMod))) )
                                    namesCovariates <- paste0(paste(unlist(strsplit(split = "&", CalSet()$CovMod)), Degrees, sep = "-"), collapse = "&")
                                } else if (CalSet()$mod.eta.model.type %in% c("exp_kT", "exp_kK", "T_power", "K_power")) namesCovariates = "Temperature" else namesCovariates = ""
                                NewModel  <-  paste0(paste(CalSet()$Sens.raw.unit, CalSet()$mod.eta.model.type, format(CalSet()$Cal.DateIN,"%Y%m%d"),format(CalSet()$Cal.DateEND,"%Y%m%d"),namesCovariates,sep = "__"),".rdata")
                                updateSelectInput(session, inputId = paste0("Cal", CalSet()$k), label = "Select a previous calibration ",
                                                  choices = Newchoices, selected = NewModel[1])
                            }
                        }
                    } else {
                        # 2.2
                        # Message Model already esists
                        my_message <- paste0("[shiny, Plot.Calibration()] ERROR, a calibration model with the same \"Raw unit of sensor data\" or
                                         \" Model for calibration\" or date range of calibration in \"SetTime\" already exists. Click on \"Previous Calibration \"
                                         and select it in \"Select previous calibration\".\n")
                        cat(my_message)
                        shinyalert(
                            title = "ERROR Model Already exists",
                            text = my_message,
                            closeOnEsc = TRUE,
                            closeOnClickOutside = TRUE,
                            html = FALSE,
                            type = "error",
                            showConfirmButton = TRUE,
                            showCancelButton  = FALSE,
                            confirmButtonText = "OK",
                            confirmButtonCol  = "#AEDEF4",
                            timer             = 0,
                            imageUrl          = "",
                            animation         = FALSE)
                        
                        plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                        text(1,1,my_message)
                    }
                }
            }
        }, priority = 2500, ignoreInit = TRUE
        )
        # Reactive FUN Plot.Calibration
        Plot.Calibration     <- eventReactive({
            input$Reference.name
            input$Sensors
            input$Selected
            input$Merge
            # if CalSet() is used it becomes reactive every something changes in CalSet()
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DateCal",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DateCalMeas",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Cal",i)]]))
            DF.aggregated.Avg()
            Cal$Forced
        },{
            # Setting calibration models and plotting calibration
            # depends:
            #  CalSet()$Cal_Line
            #  input[[paste0("DateCal",CalSet()$k)]],input[[paste0("Date",CalSet()$k)]],
            #           CalSet()$Cal_Line, CalSet()$Cal)
            #
            # Isolates: CalSet()$mod.eta.model.type, input[[paste0("Calibration",CalSet()$k)]] when calibrating
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "[shiny, Plot.Calibration()] INFO, Plotting scatter plot of calibration", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat(paste0("[shiny, Plot.Calibration()] INFO, Plotting scatter plot of calibration for ", input$Sensors, sep = "\n"))
            op <- par(no.readonly = TRUE)
            # Restoring graphical parameters on exit of function
            par(mfrow = c(1,1))
            on.exit(par(op))
            # Method:
            # 2 - if a "Prediction with previous calibration" is selected
            #     2.1 Check that CalSet()$Cal != "" is not empty, if empty message select model or New Calibration
            #     2.2 Check that the selected model is corect for model type, unit, dates and Covariates, if no message Select correct model of New Calibration
            #     2.3.1 Check covariates (no order)
            #     2.3.2 Check the start/end dates
            #     2.4 Plot the existing calibration model
            # 3 - if "Calibration with slope and intercept" is selected
            #     3.1 Check that the unit, slope and intercept are corrects
            #     3.1 Create Linear model based of the slope and intercept given
            if (CalSet()$Cal_Line == "Prediction with previous calibration") {
                # 2
                # plotting "Prediction with previous calibration" if CalSet()$Cal exists, the same CalSet()$Sens.raw.unit, same CalSet()$mod.eta.model.type,
                # We keep floating DateIN and DateEnd to be able to play with them in SetTime
                # 2.1 Null calibration model
                if (CalSet()$Cal != "") {
                    # 2.2 Correct units and model type
                    if (grepl(pattern = glob2rx(paste0("*_",CalSet()$Sens.raw.unit,"*_",CalSet()$mod.eta.model.type,"_*")), x = CalSet()$Cal)) {
                        # 2.3.2 Checking Correct covariates for model with covariates
                        # Covariates in the selected model
                        Splitted.Cal      <- unlist(strsplit(x = CalSet()$Cal, split = "__"))
                        Covariates.Cal    <- str_replace(Splitted.Cal[7], pattern = ".rdata", replacement = "")
                        Covariates.Cal    <- unlist(strsplit(x = Covariates.Cal , split = "&"))
                        # Co-Variates selected in UI
                        Covariates.CovMod <- unlist(strsplit(x = CalSet()$CovMod, split = "&"))
                        if (file.exists(CalSet()$Multi.File)) {
                            # read Multi.File
                            Multi.File.df <-  read.table(file             = CalSet()$Multi.File,
                                                         header           = TRUE,
                                                         row.names        = NULL,
                                                         comment.char     = "#",
                                                         stringsAsFactors = FALSE
                            )
                            # degree of polynomial of all Co_Variates
                            Degrees <-  Multi.File.df[Multi.File.df$Covariates %in% Covariates.CovMod, "degree"]
                            Covariates.CovMod <- paste(unlist(strsplit(split = "&", CalSet()$CovMod)), Degrees, sep = "-")
                        } else if (grepl(pattern = "-", x = Covariates.Cal[1])) {
                            Degrees <-  base::rep(1, times = length(Covariates.CovMod))
                            Covariates.CovMod <- paste(unlist(strsplit(split = "&", CalSet()$CovMod)), Degrees, sep = "-")
                        }
                        if (!(CalSet()$mod.eta.model.type == "MultiLinear") ||
                            (CalSet()$mod.eta.model.type == "MultiLinear" & all(Covariates.Cal %in% Covariates.CovMod) & all(Covariates.CovMod %in%  Covariates.Cal))) {
                            # Either the model has no covariates or the covariates of the model are correlctly selected in CovMod
                            # 2.3.1 correct dates
                            if (grepl(pattern = glob2rx(paste0("*_",
                                                               format(input[[paste0("DateCal",CalSet()$k)]][1],"%Y%m%d"),"*_*",
                                                               format(input[[paste0("DateCal",CalSet()$k)]][2],"%Y%m%d"), "*")),
                                      x = CalSet()$Cal)
                            ) {
                                # 2.4
                                cat(paste0("[shiny, Plot.Calibration()], INFO, using previous calibration ",CalSet()$Cal, " with ",
                                           CalSet()$NewCalSet, " calibration method\n"))
                                # loading the calibration file
                                name.Model.i <- file.path(CalSet()$WDoutputMod, CalSet()$Cal)       # if you use a for loop, replace CalSet()$Cal with input[[paste0("Cal",k)]]))
                                if (extension(name.Model.i) == ".rdata") Model.i <- load_obj(name.Model.i) else if (extension(name.Model.i) == ".rds") {
                                    # Read model object as a RDS object
                                    if (file.exists(name.Model.i)) {
                                        Model.i <- readRDS(file = name.Model.i)
                                        #Convert to a broom oject to tidy model
                                        Model.i <- list(Tidy = tidy(Model.i), Augment = augment(Model.i), Glance = glance(Model.i), Call = Model.i$call, Coef = coef(Model.i))
                                        # save as a Rdata list
                                        list.save(x    = Model.i,
                                                  file = sub(pattern = ".rds", replacement = ".rdata", x = name.Model.i))
                                        # delete the rds file
                                        file.remove(name.Model.i)
                                        # Updating the selected model
                                        Newchoices <- substr(list.files(path    = CalSet()$WDoutputMod,
                                                                        pattern = glob2rx(paste0(CalSet()$AirSensEur.name,"*",CalSet()$name.sensor,"*"))),
                                                             start = nchar(paste0(CalSet()$AirSensEur.name,"__",CalSet()$name.sensor,"__")) + 1,
                                                             stop  = nchar(list.files(path    = CalSet()$WDoutputMod,
                                                                                      pattern = glob2rx(paste0(CalSet()$AirSensEur.name,"*",CalSet()$name.sensor,"*")))))
                                        # Current model
                                        NewModel  <-   name.Model.i %>%
                                            basename(.) %>%
                                            sub(pattern = ".rds", replacement = ".rdata", x = .) %>%
                                            sub(pattern = paste0(CalSet()$AirSensEur.name,"__",CalSet()$name.sensor,"__"), replacement = "", x = .)
                                        updateSelectInput(session, inputId = paste0("Cal", CalSet()$k), label = "Select a previous calibration ",
                                                          choices = Newchoices, selected = NewModel[1])
                                    }
                                }
                                # Plotting
                                x <- Model.i$Augment$x
                                y <- Model.i$Augment$y
                                # Changing axis labels
                                Pattern  <- rbind(c("Out.", ""),c("Ref.", "Reference "),c("ppm",""),c("ppb",""),c("_",""))
                                if (nrow(Pattern) > 0) A.Labels <- gsub(pattern = Pattern[1,1], replacement = Pattern[1,2], x = CalSet()$nameGasRef)
                                if (nrow(Pattern) > 1) for (i in 2:nrow(Pattern)) A.Labels <- gsub(pattern = Pattern[i,1], replacement = Pattern[i,2], x = A.Labels)
                                if (any(CalSet()$mod.eta.model.type %in% "gam")) {
                                    A.Labels.X <- paste0( CalSet()$AirSensEur.name, ", raw data of ", CalSet()$name.sensor," in ",CalSet()$Sens.raw.unit)
                                    A.Labels.Y <- paste0(A.Labels ," in ",CalSet()$unit.ref, " at ",CalSet()$Reference.name)
                                } else {
                                    A.Labels.X <- paste0(A.Labels ," in ",CalSet()$unit.ref, " at ",CalSet()$Reference.name)
                                    A.Labels.Y <- paste0( CalSet()$AirSensEur.name, ", raw data of ", CalSet()$name.sensor," in ",CalSet()$Sens.raw.unit)
                                }
                                EtalLim <- Etalonnage( x = x,
                                                       s_x = NULL,
                                                       y = y,
                                                       s_y = NULL,
                                                       AxisLabelX = A.Labels.X,
                                                       AxisLabelY = A.Labels.Y,
                                                       Title = paste0("Raw data for", CalSet()$Cal),
                                                       Marker = 1,
                                                       Couleur = "blue",
                                                       ligne = 'p',
                                                       XY_same = FALSE,
                                                       lim = NULL,
                                                       steps = c(10,10),
                                                       digitround = c(2,3),
                                                       marges = c(4,4,3,0.5)
                                )
                                if (!(CalSet()$NewCalSet %in% c("ExpGrowth",
                                                                "exp_kT",
                                                                "exp_kK",
                                                                "T_power",
                                                                "K_power",
                                                                "MultiLinear"))) Cal_Line(x             = x,
                                                                                          s_x           = NULL,
                                                                                          y             = y,
                                                                                          s_y           = NULL,
                                                                                          Mod_type      = isolate(CalSet()$NewCalSet),
                                                                                          Matrice       = NULL,
                                                                                          line_position = 0,
                                                                                          Couleur       = "red",
                                                                                          Sensor_name   = NULL,
                                                                                          f_coef1       = "%.3e",
                                                                                          f_coef2       = "%.3e",
                                                                                          f_R2          = "%.4f",
                                                                                          lim           = EtalLim,
                                                                                          marges        = NULL,
                                                                                          Covariates    = NULL,
                                                                                          Weighted      = TRUE,
                                                                                          Lag_interval  = (max(x, na.rm = T) - min(x, na.rm = T)) / 15,
                                                                                          Auto.Lag      = CalSet()$Sync.Pred)
                            } else {
                                # 2.3.2 start/end dates
                                cat("[shiny, Plot.Calibration()] ERROR, \"Range of date for calibration:\" or \"Range for plotting calibration\" not consistent with \"Selected previous calibration\". Change \"Range of date for calibration:\" or select  another previous calibration.\n")
                                plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                                text(1,1,"[shiny, Plot.Calibration()] ERROR, \"Range of date for calibration:\" or \"Range for plotting calibration\" not consistent with \"Selected previous calibration\". Change \"Range of date for calibration:\" or select  another previous calibration.\n")
                            }
                        } else {
                            # 2.3.1 Incorrect covariate lists
                            my_message <- paste0("[shiny, Plot.Calibration()] ERROR, \"List of covariates to calibrate\" or the \"degrees of covariates\" not consistent with \"Selected previous calibration\".
                                                 Change \"List of covariates to calibrate\" or select  another previous calibration.\n")
                            cat(my_message)
                            # shinyalert(
                            #     title = "ERROR list of covariates is inconsistent with Model name",
                            #     text = my_message,
                            #     closeOnEsc = TRUE,
                            #     closeOnClickOutside = TRUE,
                            #     html = FALSE,
                            #     type = "error",
                            #     showConfirmButton = TRUE,
                            #     showCancelButton  = FALSE,
                            #     confirmButtonText = "OK",
                            #     confirmButtonCol  = "#AEDEF4",
                            #     timer             = 0,
                            #     imageUrl          = "",
                            #     animation         = FALSE
                            # )
                            plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                            text(1,1,my_message)
                        }
                    } else {
                        # 2.2 Units and model type
                        cat("[shiny, Plot.Calibration()] ERROR, \"Raw unit of sensor data\" or \" Model for calibration\" not consistent with \"Selected previous calibration\". Click on \"New calibration with current data\" or select another previous calibration.\n")
                        plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                        text(1,1,"[shiny, Plot.Calibration()] ERROR, \"Raw unit of sensor data\" or \" Model for calibration\" not consistent with \"Selected previous calibration\". Click on \"New calibration with current data\" or select another previous calibration.\n")
                    }
                } else {
                    # 2.1 NULL calibration model
                    cat("[shiny, Plot.Calibration()] ERROR, \" Model for calibration\" is empty, select a model o choose \"New calibration with current data\".\n")
                    plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                    text(1,1,"[shiny, Plot.Calibration()] ERROR, \" Model for calibration\" is empty, select a model o choose \"New calibration with current data\".\n")
                }
            } else if (CalSet()$Cal_Line == "Calibration with slope and intercept") {
                #### TO BE CODED
            }
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            progress$set(message = "[shiny, Plot.Calibration()] INFO, Plotting scatter plot of calibration", value = 1)
            on.exit(progress$close())
        })
        # NavBar"Data Treatment", mainTabPanel SummaryCal - Calibration  ----
        output$SummaryCal.Cal  <- renderPrint(SummaryCal.Cal())
        SummaryCal.Cal <- reactive({if (!is.null(Table.SummaryCal()$Equation)) unlist(Table.SummaryCal()$Equation) else unlist(Table.SummaryCal()$Call)})
        output$SummaryCal.Coefficients  <- renderText({"\n Coefficients"})
        output$SummaryCal.Tidy <- renderTable({Table.SummaryCal()$Tidy}, digits = 10)
        output$SummaryCal.Glance.Title  <- renderText({"\n Glance"})
        output$SummaryCal.Glance <- renderTable({Table.SummaryCal()$Glance}, digits = 10)
        # Reactive FUN Table.SummaryCal
        Table.SummaryCal     <- reactive({
            # Setting calibration models and plotting calibration
            # depends: input[[paste0("DateCal",CalSet()$k)]],input[[paste0("Date",CalSet()$k)]],
            #           CalSet()$Cal_Line, CalSet()$Cal)
            #
            # Isolates: CalSet()$mod.eta.model.type, input[[paste0("Calibration",CalSet()$k)]] when calibrating
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            progress$set(message = "Summary table of calibration model", value = 0.5)
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat(paste0("[Shiny]Table.SummaryCal, INFO, calibration of sensor ", input$Sensors, sep = "\n"))
            # checking that CalSet()$Cal is not empty
            if (CalSet()$Cal != "") {
                # checking that the model file exists
                if (file.exists(file.path(CalSet()$WDoutputMod, CalSet()$Cal))) {
                    cat(paste0("[Shiny]Table.SummaryCal, INFO, calibration model ", CalSet()$Cal, " exists\n"))
                    # loading the calibration files
                    name.Model.i <- file.path(CalSet()$WDoutputMod, CalSet()$Cal)
                    # Loading Model.i either as Rdata list or as a RDS file
                    if (extension(name.Model.i) == ".rdata") Model.i <- load_obj(name.Model.i) else if (extension(name.Model.i) == ".rds") {
                        # Read model object as a RDS object
                        Model.i <- readRDS(file = name.Model.i)
                        #Convert to a broom oject to tidy model
                        Model.i <- list(Tidy = tidy(Model.i), Augment = augment(Model.i), Glance = glance(Model.i), Call = Model.i$call, Coef = coef(Model.i))
                        # save as a Rdata list
                        list.save(x    = Model.i,
                                  file = sub(pattern = ".rds", replacement = ".rdata", x = name.Model.i))
                    }
                    options(digits = 10)
                    if (CalSet()$mod.eta.model.type == "Linear.robust") return.SummaryCal <- summary.rq(Model.i) else return.SummaryCal <- summary(Model.i)
                } else {
                    cat(paste0("[Shiny]Table.SummaryCal, ERROR, calibration model ", CalSet()$Cal, " does not exist\n"))
                    return.SummaryCal <- paste0("[Shiny]Table.SummaryCal, ERROR, calibration model ", CalSet()$Cal, " does not exist\n")
                }
            } else  {
                cat(paste0("[Shiny]Table.SummaryCal, ERROR, calibration model is empty\n"))
                return.SummaryCal <- paste0("[Shiny]Table.SummaryCal, ERROR, calibration model is empty\n")
            }
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            Tidy.Model        <- list(Call = Model.i$Call, Tidy = Model.i$Tidy, Glance = Model.i$Glance )
            if (!is.null(Model.i$Equation)) Tidy.Model[["Equation"]] <- Model.i$Equation
            progress$set(message = "Summary table of calibration model", value = 1)
            on.exit(progress$close())
            if (exists("Tidy.Model")) return(Tidy.Model) else return()
        })
        # NavBar"Data Treatment", mainTabPanel Calibrated - Calibration ----
        output$Calibrated   <- renderPlot(Plot.Calibrated(), width = 'auto', height = 'auto')
        # Reactive FUN Plot.Calibrated
        Plot.Calibrated     <- eventReactive({
            input$Sensors
            input$Selected
            input$Merge
            input$Reference.name
            # if CalSet() is used it becomes reactive every something changes in CalSet()
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DateCal",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DateCalMeas",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Cal",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Calibration",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Cal_Line",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Ref.unit",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Sens.unit",i)]]))
            input$Reference.name
            DF.aggregated.Avg()
            Cal$Forced
        },{
            # Plotting calibrated data

            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "[shiny, Plot.Calibrated()] INFO,  plotting calibration of sensor", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat(paste0("[shiny, Plot.Calibrated()] INFO, plotting calibration of sensor ", input$Sensors, sep = "\n"))
            op <- par(no.readonly = TRUE)
            
            # Restoring graphical parameters on exit of function
            par(mfrow = c(1,1))
            on.exit(par(op))
            
            if (CalSet()$Cal_Line == "Prediction with previous calibration") {
                # 2
                # plotting "Prediction with previous calibration" if CalSet()$Cal exists, the same CalSet()$Sens.raw.unit, same CalSet()$mod.eta.model.type,
                # We keep floating DateIN and DateEnd to be able to play with them in SetTime
                # 2.1 Null calibration model
                if (CalSet()$Cal != "") {
                    # 2.2 Correct units and model type
                    if (grepl(pattern = glob2rx(paste0("*_",CalSet()$Sens.raw.unit,"*_",CalSet()$mod.eta.model.type,"_*")), x = CalSet()$Cal)) {
                        # 2.3.2 Checking Correct covariates for model with covariates
                        # Covariates in the selected model
                        Splitted.Cal      <- unlist(strsplit(x = CalSet()$Cal, split = "__"))
                        Covariates.Cal    <- str_replace(Splitted.Cal[7], pattern = ".rdata", replacement = "")
                        Covariates.Cal    <- unlist(strsplit(x = Covariates.Cal , split = "&"))
                        # Co-Variates selected in UI
                        Covariates.CovMod <- unlist(strsplit(x = CalSet()$CovMod, split = "&"))
                        if (file.exists(CalSet()$Multi.File)) {
                            # read Multi.File
                            Multi.File.df <-  read.table(file             = CalSet()$Multi.File,
                                                         header           = TRUE,
                                                         row.names        = NULL,
                                                         comment.char     = "#",
                                                         stringsAsFactors = FALSE
                            )
                            # degree of polynomial of all Co_Variates
                            Degrees <-  Multi.File.df[Multi.File.df$Covariates %in% Covariates.CovMod, "degree"]
                            Covariates.CovMod <- paste(unlist(strsplit(split = "&", CalSet()$CovMod)), Degrees, sep = "-")
                        } else if (grepl(pattern = "-", x = Covariates.Cal[1])) {
                            Degrees <-  base::rep(1, times = length(Covariates.CovMod))
                            Covariates.CovMod <- paste(unlist(strsplit(split = "&", CalSet()$CovMod)), Degrees, sep = "-")
                        }
                        if (!(CalSet()$mod.eta.model.type == "MultiLinear") ||
                            (CalSet()$mod.eta.model.type == "MultiLinear" & all(Covariates.Cal %in% Covariates.CovMod) & all(Covariates.CovMod %in%  Covariates.Cal))) {
                            # Either the model has no covariates or the covariates of the model are correlctly selected in CovMod
                            # 2.3.1 correct dates
                            if (grepl(pattern = glob2rx(paste0("*_",
                                                               format(input[[paste0("DateCal",CalSet()$k)]][1],"%Y%m%d"),"*_*",
                                                               format(input[[paste0("DateCal",CalSet()$k)]][2],"%Y%m%d"), "*")),
                                      x = CalSet()$Cal)
                            ) {
                                # 2.4
                                cat(paste0("[shiny, Plot.Calibrated()] INFO, using previous calibration ",CalSet()$Cal, " with ",
                                           CalSet()$NewCalSet, " calibration method\n"))
                                # Loading previous model an updating General.cal
                                x <- DF$General[[CalSet()$nameGasRef]][DF$General$date >= CalSet()$Cal.DateIN & DF$General$date <= CalSet()$Cal.DateEND + 1]
                                y <- DF$General[[CalSet()$nameGasMod]][DF$General$date >= CalSet()$Cal.DateIN & DF$General$date <= CalSet()$Cal.DateEND + 1]
                                if (is.null(y)) Cal$Forced <<- TRUE else {
                                    # Changing axis labels
                                    Pattern  <- rbind(c("Out.", ""),c("Ref.", "Reference "),c("ppm",""),c("ppb",""),c("_",""))
                                    if (nrow(Pattern) > 0) A.Labels <- gsub(pattern = Pattern[1,1], replacement = Pattern[1,2], x = CalSet()$nameGasRef)
                                    if (nrow(Pattern) > 1) for (i in 2:nrow(Pattern)) A.Labels <- gsub(pattern = Pattern[i,1], replacement = Pattern[i,2], x = A.Labels)
                                    A.Labels.X <- paste0(A.Labels ," in ",CalSet()$unit.ref, " at ",CalSet()$Reference.name)
                                    A.Labels.Y <- paste0( CalSet()$AirSensEur.name, ", Calibrated data of ", CalSet()$name.sensor," in ",CalSet()$unit.sensor)
                                    EtalLim <- Etalonnage( x = x, s_x = NULL, y = y, s_y = NULL,
                                                           AxisLabelX = A.Labels.X, AxisLabelY = A.Labels.Y,
                                                           Title = CalSet()$Cal,
                                                           Marker = 1,
                                                           Couleur = "blue",
                                                           ligne = 'p',
                                                           XY_same = TRUE,
                                                           lim = NULL,
                                                           steps = c(10,10),
                                                           digitround = c(1,1),
                                                           marges = c(4,4,3,0.5))
                                    Comparison <- Cal_Line(x = x, s_x = NULL,
                                                           y = y, s_y = NULL,
                                                           Mod_type      = CalSet()$eta.model.type,
                                                           Matrice       = NULL,
                                                           line_position = 0,
                                                           Couleur       = "red",
                                                           Sensor_name   = NULL,
                                                           f_coef1       = "%.3e",
                                                           f_coef2       = "%.3e",
                                                           f_R2          = "%.4f",
                                                           lim           = EtalLim,
                                                           marges        = NULL,
                                                           Covariates    = NULL,
                                                           Weighted      = FALSE,
                                                           Lag_interval  = (max(x, na.rm = T) - min(x, na.rm = T)) / 15,
                                                           Auto.Lag      = CalSet()$Sync.Cal)
                                    # Saving plot if requested
                                    if (input$SavePlot) {
                                        dev.copy(png,
                                                 filename = file.path(CalSet()$WDoutput,
                                                                      paste0(CalSet()$Cal,"_Calibrated_",
                                                                             format(CalSet()$Cal.DateIN, "%Y%m%d"),"_",
                                                                             format(CalSet()$Cal.DateEND,"%Y%m%d"),".png")),
                                                 units = "cm",
                                                 width = 20,
                                                 height = 20,
                                                 res = 300
                                        )
                                        dev.off()
                                        cat(paste0("[shiny] INFO, ", CalSet()$Cal,"_Calibrated_",
                                                   format(CalSet()$Cal.DateIN, "%Y%m%d"),"_",
                                                   format(CalSet()$Cal.DateEND,"%Y%m%d"),".png saved in ", CalSet()$WDoutput, "\n" ))
                                        updateCheckboxInput(session, inputId = "SavePlot", value = FALSE)
                                    }
                                }
                            } else {
                                # 2.3.2 start/end dates
                                cat("[shiny, Plot.Calibrated()] ERROR, \"Range of date for calibration:\" or \"Range for plotting calibration\" not consistent with \"Selected previous calibration\". Change \"Range of date for calibration:\" or select  another previous calibration.\n")
                                plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                                text(1,1,"[shiny, Plot.Calibrated()] ERROR, \"Range of date for calibration:\" or \"Range for plotting calibration\" not consistent with \"Selected previous calibration\". Change \"Range of date for calibration:\" or select  another previous calibration.\n")
                            }
                        } else {
                            # 2.3.1 Incorrect covariate lists
                            my_message <- paste0("[shiny, Plot.Calibrated()] ERROR, \"List of covariates to calibrate\" not consistent with \"Selected previous calibration\".
                                                 Change \"List of covariates to calibrate\" or select  another previous calibration.\n")
                            cat(my_message)
                            # shinyalert(
                            #     title = "ERROR list of covariates is inconsistent with Model name",
                            #     text = my_message,
                            #     closeOnEsc = TRUE,
                            #     closeOnClickOutside = TRUE,
                            #     html = FALSE,
                            #     type = "error",
                            #     showConfirmButton = TRUE,
                            #     showCancelButton  = FALSE,
                            #     confirmButtonText = "OK",
                            #     confirmButtonCol  = "#AEDEF4",
                            #     timer             = 0,
                            #     imageUrl          = "",
                            #     animation         = FALSE
                            # )
                            plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                            text(1,1,my_message)
                        }
                    } else {
                        # 2.2 Units and model type
                        cat("[shiny, Plot.Calibrated()] ERROR, \"Raw unit of sensor data\" or \" Model for calibration\" not consistent with \"Selected previous calibration\". Click on \"New calibration with current data\" or select another previous calibration.\n")
                        plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                        text(1,1,"[shiny, Plot.Calibrated()] ERROR, \"Raw unit of sensor data\" or \" Model for calibration\" not consistent with \"Selected previous calibration\". Click on \"New calibration with current data\" or select another previous calibration.\n")
                    }
                } else {
                    # 2.1 NULL calibration model
                    cat("[shiny, Plot.Calibrated()] ERROR, \" Model for calibration\" is empty, select a model o choose \"New calibration with current data\".\n")
                    plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                    text(1,1,"[shiny, Plot.Calibrated()] ERROR, \" Model for calibration\" is empty, select a model o choose \"New calibration with current data\".\n")
                }
            } else if (CalSet()$Cal_Line == "Calibration with slope and intercept") {
                #### TO BE CODED
            }
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            progress$set(message = "[shiny, Plot.Calibrated()] INFO,  plotting calibration of sensor", value = 1)
            on.exit(progress$close())
            if (exists("Comparison")) return(Comparison)
        })
        # NavBar"Data Treatment", mainTabPanel TimeSeries - Calibration ----
        plot.ts_Cal_dygraphs <- eventReactive({
            input$Reference.name
            input$Sensors
            input$Selected
            input$Merge
            # if CalSet() is used it becomes reactive every something changes in CalSet()
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DateCal",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DatePlotCal",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Sens",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Cal",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Sens.unit",i)]]))
        },{
            # plotting correlation in time series of validated data with Covariates
            
            progress <- shiny::Progress$new()
            progress$set(message = "[shiny, ts_cal_dygraphs()] INFO, plotting time series of Calibrated data", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat("[shiny, ts_cal_dygraphs()] INFO, plotting time series of Calibrated data\n")
            
            General.df <- DF$General[date >= CalSet()$Cal.DateIN & date <= CalSet()$Cal.DateEND + 1, .SD, .SDcols = c("date", CalSet()$nameGasRef, CalSet()$nameGasMod)]
            if (all(is.na(General.df[, .SD, .SDcols = c(CalSet()$nameGasRef,CalSet()$nameGasMod)]))) {
                cat("[shiny, ts_cal_dygraphs()] ERROR, All data in calibration time series are empty, not plotting any times series\n")
            } else {
                # Sensor relationships with other variables
                cat(paste0("[shiny, ts_cal_dygraphs()] INFO, Plot calibrate sensor data and reference data for Sensor ", input$Sensors, " in order to check relationships with other variables\n"))
                # changing names of variables
                Pattern  <- rbind(c("Out.", ""),
                                  c("ppm",""), c("ppb",""),
                                  c("Ref.", paste0("Reference, ", CalSet()$unit.ref, ", ")),
                                  c("_volt", paste0(", ",CalSet()$Sens.raw.unit)),
                                  c(CalSet()$gas.sensor, paste0(CalSet()$name.sensor)),
                                  c("_modelled",paste0(", ",CalSet()$unit.sensor)),
                                  c("_"," "))
                Name.pol <- c(CalSet()$nameGasRef, CalSet()$nameGasMod)
                for (i in 1:nrow(Pattern)) {
                    Name.pol[1] <- gsub(pattern = Pattern[i,1], replacement = Pattern[i,2], x = Name.pol[1])
                    Name.pol[2] <- gsub(pattern = Pattern[i,1], replacement = Pattern[i,2], x = Name.pol[2])
                }
                # make interactive time-series plot
                time_series_sensor <- data_frame_to_timeseries(General.df, tz = threadr::time_zone(General.df[["date"]][1]))
                ts_cal <- cbind(time_series_sensor[[CalSet()$nameGasRef]],
                                time_series_sensor[[CalSet()$nameGasMod]])
                names(ts_cal) <- c(CalSet()$nameGasRef, CalSet()$nameGasMod)
                colour_vector <- threadr::ggplot2_colours(45)
                plot_cal <- dygraph(ts_cal,
                                    main = paste0(CalSet()$Cal," at ",CalSet()$Reference.name) ) %>%
                    dySeries(CalSet()$nameGasRef,label = Name.pol[1], color = "red") %>%
                    dySeries(CalSet()$nameGasMod,label = Name.pol[2], color = "blue") %>%
                    dyAxis("y", label = Name.pol[2]) %>%
                    dyLegend(show = "always", hideOnMouseOut = FALSE, width = 500) %>%
                    dyRangeSelector(height = 40) %>%
                    dyOptions(labelsUTC = T) # plot in UTC
                #dyOptions(useDataTimezone = TRUE) # do not use the local time zone
                # END reactive time series
                # Saving plot if requested
                # check if PhantomJS is installed in C:\Users\karaf\AppData\Roaming\PhantomJS, else "install_phantomjs()"
                if (input$SavePlot) {
                    WDoutput <- file.path(input$Selected, "Verification_plots")
                    filename_html <- file.path(CalSet()$WDoutput, paste0(CalSet()$Cal,"_Calibration_ts_",
                                                                         format(CalSet()$Cal.DateIN, "%Y%m%d"),"_",
                                                                         format(CalSet()$Cal.DateEND,"%Y%m%d"),"temp.html"))
                    filename_png <- file.path(CalSet()$WDoutput, paste0(CalSet()$Cal,"_Calibration_ts_",
                                                                        format(CalSet()$Cal.DateIN, "%Y%m%d"),"_",
                                                                        format(CalSet()$Cal.DateEND,"%Y%m%d"),".png"))
                    save_html(plot_cal, filename_html)
                    webshot(filename_html, file     = filename_png, cliprect = "viewport")
                    # Update button save plot
                    updateCheckboxInput(session, inputId = "SavePlot", value = FALSE)
                }
            }
            
            cat("-----------------------------------------------------------------------------------\n")
            # Return
            plot_cal
        })
        output$ts_Cal_dygraphs <- renderDygraph(plot.ts_Cal_dygraphs())
        # NavBar"Data Treatment", mainTabPanel ResidualMatrix - Calibration  ----
        output$ResCalMatrix  <- renderPlot(Plot.ResCalMatrix()   , width = 'auto', height = 'auto')
        # Reactive FUN Plot.ResCalMatrix
        Plot.ResCalMatrix    <- eventReactive({
            input$Sensors
            input$Selected
            input$Merge
            # if CalSet() is used it becomes reactive every something changes in CalSet()
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DateCal",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DatePlotCal",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Sens",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Cal",i)]]))
        },{
            # Plotting correalation matrix using pairs()
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "[Shiny]Plot.ResCalMatrix, INFO, Plotting matrix plots of calibration with residuals", value = 0.5)
            
            # Sensor relationships with other variables
            Relationships <- unique(c(na.omit(names(DF$General)[names(DF$General) %in% input[[paste0("Sens",CalSet()$k)]] ] ), CalSet()$nameGasRef, CalSet()$nameGasMod))
            
            # filter date, select Relationships, add residuals
            General.df <- DF$General[date >= CalSet()$Cal.DateIN & date <= CalSet()$Cal.DateEND + 1, .SD, .SDcols = c("date", Relationships)]
            data.table::set(General.df, j = "Residuals", value = General.df[[CalSet()$nameGasMod]] - General.df[[CalSet()$nameGasRef]])
            Relationships <- c(Relationships, "Residuals")
            
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat("[Shiny]Plot.ResCalMatrix, INFO, plotting correlation matrix of calibration residuals data with Covariates\n")
            
            if (all(is.na(General.df[, .SD, .SDcols = Relationships]))) {
                my_message <- "[[Shiny, Plot.ResCalMatrix ()] ERROR, All sensor time series are empty, not plotting any correlation matrix\n"
                cat(my_message)
                shinyalert(
                    title = "ERROR no data to plot",
                    text = my_message,
                    closeOnEsc = TRUE,
                    closeOnClickOutside = TRUE,
                    html = FALSE,
                    type = "error",
                    showConfirmButton = TRUE,
                    showCancelButton  = FALSE,
                    confirmButtonText = "OK",
                    confirmButtonCol  = "#AEDEF4",
                    timer             = 3000,
                    imageUrl          = "",
                    animation         = FALSE)
            } else {
                # changing names of variables
                Pattern  <- rbind(c("Out.", ""),
                                  c("ppm",""), c("ppb",""),
                                  c("Ref.", paste0("Reference, ", CalSet()$unit.ref, " ")),
                                  c("_volt", paste0(", ",CalSet()$Sens.raw.unit)),
                                  c(CalSet()$gas.sensor, paste0(CalSet()$name.sensor)),
                                  c("_modelled",paste0(", ",CalSet()$unit.sensor)),
                                  c("_"," "))
                if (nrow(Pattern) > 0) Labels <- gsub(pattern = Pattern[1,1], replacement = Pattern[1,2], x = Relationships)
                if (nrow(Pattern) > 1) for (i in 2:nrow(Pattern)) Labels <- gsub(pattern = Pattern[i,1], replacement = Pattern[i,2], x = Labels)
                op <- par(no.readonly = TRUE)
                # Restoring graphical parameters on exit of function
                on.exit(par(op))
                par(mfrow = c(1,1))
                # in panel smooth() change pch and cex, in panel.cor() change digits and cex.cor, in pairs() change cex.labels to fit the plots
                pairs(General.df[, .SD, .SDcols = c(Relationships)],
                      lower.panel = panel.smooth,
                      upper.panel = panel.cor,
                      diag.panel  = panel.hist,
                      labels      = Labels,
                      main        = paste0("Correlation matrix (R2 in bold) with ", CalSet()$Cal),
                      cex.labels  = 2) # cex.cor = 1.3
                # Saving plot if requested
                if (input$SavePlot) {
                    dev.copy(png, filename = file.path(input$Selected,"Calibration",
                                                       paste0(CalSet()$Cal,"_Res_pairs_",
                                                              format(min(General.df[["date"]], na.rm = TRUE),"%Y%m%d"),"_",
                                                              format(max(General.df[["date"]], na.rm = TRUE),"%Y%m%d"),".png")),
                             type  = "cairo",
                             units = "cm",
                             width = 35.55,
                             height = 20,
                             res = 300
                    )
                    dev.off()
                    cat(paste0("[shiny] INFO, ", CalSet()$Cal,"_Res_pairs_",
                               format(min(General.df[["date"]], na.rm = TRUE),"%Y%m%d"),"_",
                               format(max(General.df[["date"]], na.rm = TRUE),"%Y%m%d"),".png saved in ", file.path(input$Selected,"Calibration"), "\n" ))
                    updateCheckboxInput(session, inputId = "SavePlot", value = FALSE)
                }
            }
            cat("-----------------------------------------------------------------------------------\n")
            progress$set(message = "[Shiny]Plot.ResCalMatrix, INFO, Plotting matrix plots of calibration with residuals", value = 1)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
        })
        # NavBar"Data Treatment", mainTabPanel "DataTable",
        React.General.cal <- reactive({
            if (!is.null(DF$General)) DF$General[date >= input$Out.Sens.Date1[1] & date <= input$Out.Sens.Date1[2] + 1]
            #%>% filter(date >= input$Out.Sens.Date1[1] & date <= input$Out.Sens.Date1[2] + 1)
        })
        action <- dataTableAjax(session, React.General.cal())
        widget <- datatable(React.General.cal(),
                            class = 'display cell-border compact',
                            filter = 'top',
                            #server = TRUE,
                            options = list(ajax = list(url = action))
        )
        output$DataTable <- DT::renderDataTable({
            DT::datatable( React.General.cal(),
                           options = list(
                               "filter"     = "top",
                               "autoWidth"  = TRUE,
                               "pageLength" = 19,                     # number of rows to plot by default
                               "lengthMenu" = sort(unique(c(15, 19, 24, round(60/as.numeric(input$UserMins)),
                                                            round(1440/as.numeric(input$UserMins)),
                                                            round(7*1440/as.numeric(input$UserMins)),
                                                            round(14*1440/as.numeric(input$UserMins)))))
                           )
                           #, editable = TRUE                        # possibility to edit the values
            ) %>% DT::formatRound( c("altitude",
                                     "Temperature",
                                     "Relative_humidity",
                                     names(React.General.cal())[grep(pattern = "Ref", x = names(React.General.cal()))]
                                     #list.gas.reference2use(),
                                     #paste0("Out.",list.gas.reference2use())
            ), digits = 1) %>%
                DT::formatRound( c("gpsTimestamp",
                                   "boardTimeStamp",
                                   "Atmospheric_pressure",
                                   list.gas.sensor(),
                                   paste0("Out.",list.gas.sensor()),
                                   #paste0("Out.",list.gas.sensor(),".1"),
                                   names(React.General.cal())[grep(pattern = "Out.Warm."    , x = names(React.General.cal()))],
                                   names(React.General.cal())[grep(pattern = "Out.TRh."     , x = names(React.General.cal()))],
                                   names(React.General.cal())[grep(pattern = "Out.Invalid." , x = names(React.General.cal()))],
                                   names(React.General.cal())[grep(pattern = "Out.Warm.TRh.", x = names(React.General.cal()))]
                ), digits = 0) %>%
                DT::formatRound( c(names(React.General.cal())[grep(pattern = "_volt"     , x = names(React.General.cal()))],
                                   names(React.General.cal())[grep(pattern = "_DV"       , x = names(React.General.cal()))],
                                   names(React.General.cal())[grep(pattern = "_modelled" , x = names(React.General.cal()))]
                ), digits = 3) %>%
                DT::formatRound( c("latitude", "longitude"), digits = 5) #%>%
            # formatDate(names(React.General.cal())[grep(pattern = "date" , x = names(React.General.cal()))],
            #            method= "toLocaleDateString",
            #            params = list('en-US', list(month  = 'numeric', day    = 'numeric', hour = 'numeric', minute = 'numeric', hour12 = FALSE)))
        })
        # # NavBar"Data Treatment", mainTabPanel Prediction ----
        # if (input$UserMinsAvg == input$UserMins) {
        #     if (!is.null(DF$General)) {
        #         Init.DF.avg <- DF$General
        #     }  else {
        #         # to avoid error due to averaging DF$General which does not exist
        #         updateSelectInput(session = session, inputId = "UserMinsAvg", selected = input$UserMins)
        #         Init.DF.avg <- data.table()
        #     }
        # } else {
        #     progress <- shiny::Progress$new()
        #     on.exit(progress$close())
        #     progress$set(message = "[shiny] INFO, averaging Summary table of calibration model", value = 0.2)
        #     if (!is.null(DF$General)) {
        #         # Selecting columns for averaging
        #         Cols.for.Avg <- c(intersect(c("date","gpsTimestamp","altitude","latitude","longitude",
        #                                       "Relative_humidity","Atmospheric_pressure","Temperature", "boardTimeStamp",
        #                                       "Ref.Long","Ref.Lat","Absolute_humidity", "Td_deficit",
        #                                       paste0("Out.",list.gas.sensor()), paste0("Out.",list.gas.sensor())), names(DF$General)),
        #                           grep(pattern = paste(c("_volt", "_modelled"), collapse = "|"), x = names(DF$General), value = TRUE))
        #         # removing nan if any
        #         if (any(sapply(DF$General[,..Cols.for.Avg], function(i) any(is.nan(i))))) set(DF$General, j = Cols.for.Avg, value = lapply(DF$General[,..Cols.for.Avg], nan.to.na))
        #         Init.DF.avg   <- DF_avg(DF$General[, ..Cols.for.Avg], width = as.numeric(input$UserMinsAvg))
        #     }  else {
        #         # to avoid error due to averaging DF$General which does not exist
        #         updateSelectInput(session = session, inputId = "UserMinsAvg", selected = input$UserMins)
        #         Init.DF.avg <- data.table()
        #     }
        #     progress$set(message = "[shiny] INFO, averaging Summary table of calibration model", value = 1.0)
        # }
        #if (exists("Init.DF.avg") && !is.null(Init.DF.avg))
        # DF.aggregated <<- reactiveValues(Avg = Init.DF.avg)
        # if (exists("Init.DF.avg")) rm(Init.DF.avg)
        DF.aggregated.Avg <- eventReactive({
            input$Selected
            input$merge
            input$UserMinsAvg
            input$UserMins
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Apply.cal", i)]]))
            Cal$Forced
        },{
            if (!is.null(DF$General)) {
                if (input$UserMinsAvg == input$UserMins) {
                    DF.aggregated <- DF$General
                } else {
                    progress <- shiny::Progress$new()
                    on.exit(progress$close())
                    progress$set(message = "[shiny] INFO, averaging Summary table of calibration model", value = 0.2)
                    
                    # only keeping the complete cases by pairs of reference and sensor data
                    # set(DF$General, j = names(DF$General)[-which(names(DF$General) == "date")], value = lapply(DF$General[,..Cols.for.Avg], nan.to.na))
                    # Selecting columns for averaging
                    Cols.for.Avg <- c(intersect(c("date","gpsTimestamp","altitude","latitude","longitude",
                                                  "Relative_humidity","Atmospheric_pressure","Temperature", "boardTimeStamp",
                                                  "Ref.Long","Ref.Lat","Absolute_humidity", "Td_deficit"), names(DF$General)),
                                      paste0("Out.",list.gas.sensor()), paste0("Out.", list.gas.reference2use()),
                                      grep(pattern = paste(c("_volt", "_modelled"), collapse = "|"), x = names(DF$General), value = TRUE))
                    
                    # removing nan if any
                    #if (any(sapply(DF$General[,..Cols.for.Avg], function(i) any(is.nan(i))))) set(DF$General, j = Cols.for.Avg, value = lapply(DF$General[,..Cols.for.Avg], nan.to.na))
                    
                    # Averaging
                    DF.aggregated <- DF_avg(DF$General[,..Cols.for.Avg], width = as.numeric(input$UserMinsAvg))
                    progress$set(message = "[shiny] INFO, averaging Summary table of calibration model", value = 1.0)
                    
                    return(DF.aggregated)}
            } else return()
        })
        
        # observeEvent({
        #     input$UserMinsAvg
        #     DF$General
        # },{
        #     if (input$UserMinsAvg == input$UserMins) {
        #         DF.aggregated$Avg <<- DF$General
        #     } else {
        #         progress <- shiny::Progress$new()
        #         on.exit(progress$close())
        #         progress$set(message = "[shiny] INFO, averaging Summary table of calibration model", value = 0.2)
        #         
        #         # only keeping the complete cases by pairs of reference and sensor data
        #         #set(DF$General, j = names(DF$General)[-which(names(DF$General) == "date")], value = lapply(DF$General[,..Cols.for.Avg], nan.to.na))
        #         # Selecting columns for averaging
        #         Cols.for.Avg <- c(intersect(c("date","gpsTimestamp","altitude","latitude","longitude",
        #                                       "Relative_humidity","Atmospheric_pressure","Temperature", "boardTimeStamp",
        #                                       "Ref.Long","Ref.Lat","Absolute_humidity", "Td_deficit"), names(DF$General)),
        #                           paste0("Out.",list.gas.sensor()), paste0("Out.", list.gas.reference2use()),
        #                           grep(pattern = paste(c("_volt", "_modelled"), collapse = "|"), x = names(DF$General), value = TRUE))
        # 
        #         # removing nan if any
        #         #if (any(sapply(DF$General[,..Cols.for.Avg], function(i) any(is.nan(i))))) set(DF$General, j = Cols.for.Avg, value = lapply(DF$General[,..Cols.for.Avg], nan.to.na))
        # 
        #         # Averaging
        #         DF.aggregated$Avg <<- DF_avg(DF$General[,..Cols.for.Avg], width = as.numeric(input$UserMinsAvg))
        #         progress$set(message = "[shiny] INFO, averaging Summary table of calibration model", value = 1.0)
        #     }
        # }, priority = 50)
        
        # NavBar"Data Treatment", mainTabPanel Prediction - Map ----
        pointsExtrap <- reactive( {
            # Selecting dates and coordinates
            Available.Coord <- grep(pattern = paste0(c("latitude","longitude", "Ref.Long", "Ref.Lat"), collapse = "|" ), x = names(DF$General), value = TRUE)
            PointsExtrap    <- unique(DF$General[date >= CalSet()$Meas.DateIN & date <= CalSet()$Meas.DateEND + 1, .SD, .SDcols = c("date",Available.Coord)])
            # checking if the coordinates of the reference station are included into the data frame DF$General
            if (any("Ref.Long" %in% names(PointsExtrap)) && any("Ref.Lat" %in% names(PointsExtrap)) && any(!is.na(PointsExtrap$Ref.Long)) && any(!is.na(PointsExtrap$Ref.Lat))) {
                Ref.coord_LON_LAT <- unique(PointsExtrap[!is.na(Ref.Long) & !is.nan(Ref.Long), .SD, .SDcols = c("Ref.Long","Ref.Lat")] )
                Ref.coord_LON <- Ref.coord_LON_LAT[["Ref.Long"]]
                Ref.coord_LAT <- Ref.coord_LON_LAT[["Ref.Lat"]]
            } else if (!is.null(CalSet()$coord.ref) && grepl(pattern = paste0(c(" ", ","), collapse = "|"), x = CalSet()$coord.ref)) {
                # using the coordinates in the UI because the coordinates are not inluded in DF$General
                # checking if the lat long separator is ,
                if (grepl(pattern = paste0(c(","), collapse = "|"), x = CalSet()$coord.ref)) {
                    # Checking is the coordinates are in spherical or decimal format, projection to OpenStreet map
                    if (any(grep(pattern = paste0(c("N","S", "E", "W", "d"), collapse = "|" ), x = CalSet()$coord.ref))) {
                        # extract spherical coordinates
                        Ref.coord_LAT  <- unlist(strsplit(x = CalSet()$coord.ref, split = ","))[2]
                        Ref.coord_LON  <- unlist(strsplit(x = CalSet()$coord.ref, split = ","))[1]
                        # transform spherical coordinates to decimal degrees for later projection
                        Ref.coord_d    <- OSMscale::degree(Ref.coord_LAT, Ref.coord_LON, digits = 5)
                        # Project the spherical coordinates in Mercator web WS84 of OPenStreet view
                        #Ref.coord_p    <- OSMscale::projectPoints(Ref.coord_d[1], Ref.coord_d[2], to=OSMscale::pll())
                        Ref.coord_LAT  <- Ref.coord_d[1,1]
                        Ref.coord_LON  <- Ref.coord_d[1,2]
                    } else {
                        # assume numeric coordinates
                        Ref.coord_LON <- as.numeric(unlist(strsplit(x = CalSet()$coord.ref, split = ","))[1])
                        Ref.coord_LAT <- as.numeric(unlist(strsplit(x = CalSet()$coord.ref, split = ","))[2])
                    }
                } else {
                    my_message <- paste0("[shiny, PointsExtrap()] ERROR, the coordinates of the reference station are not separated with a comma.\n")
                    cat(my_message)
                    shinyalert(
                        title = "ERROR seperator of coordinates",
                        text = my_message,
                        closeOnEsc = TRUE,
                        closeOnClickOutside = TRUE,
                        html = FALSE,
                        type = "error",
                        showConfirmButton = TRUE,
                        showCancelButton  = FALSE,
                        confirmButtonText = "OK",
                        confirmButtonCol  = "#AEDEF4",
                        timer             = 0,
                        imageUrl          = "",
                        animation         = FALSE)
                }
            } else {
                my_message <- paste0("[shiny, PointsExtrap()] ERROR, the coordinates of the reference station are not entered into the UI\n")
                cat(my_message)
                shinyalert(
                    title = "ERROR missing coordinates",
                    text = my_message,
                    closeOnEsc = TRUE,
                    closeOnClickOutside = TRUE,
                    html = FALSE,
                    type = "error",
                    showConfirmButton = TRUE,
                    showCancelButton  = FALSE,
                    confirmButtonText = "OK",
                    confirmButtonCol  = "#AEDEF4",
                    timer             = 0,
                    imageUrl          = "",
                    animation         = FALSE
                )
                Ref.coord_LON <- NULL
                Ref.coord_LAT <- NULL
            }
            # mean coordinates of the AirSensEUR
            PointsExtrap <- unique(PointsExtrap[which(!is.na(longitude) & !is.nan(longitude) & !is.na(latitude) & !is.nan(latitude)), .SD, .SDcols = c("date",Available.Coord)])
            if (PointsExtrap[,.N] > 0) {
                Names.coord.Station <- c("latitude", "longitude")
                PointsExtrap[, latitude  := round(PointsExtrap[["latitude"]] , digits = 4)]
                PointsExtrap[, longitude := round(PointsExtrap[["longitude"]], digits = 4)]
                MEAS_LON_LAT <- unique(PointsExtrap[, date := mean(date), by = list(latitude, latitude)])[, .SD, .SDcols = c("date", "latitude", "longitude") ]
                # # Position coordinates of the AirSensEUR
                # # make a spatial dataframe with traffic data
                sp_traffic_real <- SpatialPointsDataFrame(MEAS_LON_LAT[,c("longitude","latitude")], MEAS_LON_LAT[,],     # lat, lon
                                                          proj4string = CRS("+init=epsg:4326"))
                # make intersection between open street and the buffer
                buffer_sp_traffic_real <- rgeos::gBuffer(sp_traffic_real, width = 0.0002)  #0.00005, units in grade
                class(buffer_sp_traffic_real)
                (p.df <- data.frame( ID = 1:length(buffer_sp_traffic_real)))
                # # trasform SpatialPolygons to SpatialPolygonsDataFrame
                buffer_sp_traffic_real <- SpatialPolygonsDataFrame(buffer_sp_traffic_real, p.df, match.ID = F)
                buffer_sp_traffic_real <- ggplot2::fortify(buffer_sp_traffic_real)
                MEAS_LON_LAT <- buffer_sp_traffic_real %>%
                    group_by(group) %>%
                    summarise(longitude = mean(long),
                              latitude  = mean(lat))
                popup_REF  <- paste0("<strong><i>", "reference stn. @ ",
                                     round(Ref.coord_LON, digits = 4), ", " ,
                                     round(Ref.coord_LAT, digits = 4), "</i></strong>")
                popup_MEAS <- paste0("<strong><i>", "AirSensEUR box. @ ",
                                     round(MEAS_LON_LAT$longitude, digits = 4), ", " ,
                                     round(MEAS_LON_LAT$latitude, digits  = 4), "</i></strong>")
                return(list(Ref.coord_LON = Ref.coord_LON,
                            Ref.coord_LAT = Ref.coord_LAT,
                            MEAS_LON      = MEAS_LON_LAT$longitude,
                            MEAS_LAT      = MEAS_LON_LAT$latitude,
                            popup_REF     = popup_REF,
                            popup_MEAS    = popup_MEAS))
            } else {
                my_message <- paste0("[shiny, PointsExtrap()] ERROR, there are no coordinates of the AirSensEUR box\n")
                cat(my_message)
                shinyalert(
                    title = "INFO no AirSensEUR coordinates available",
                    text = my_message,
                    closeOnEsc = TRUE,
                    closeOnClickOutside = TRUE,
                    html = FALSE,
                    type = "info",
                    showConfirmButton = TRUE,
                    showCancelButton  = FALSE,
                    confirmButtonText = "OK",
                    confirmButtonCol  = "#AEDEF4",
                    timer             = 0,
                    imageUrl          = "",
                    animation         = FALSE
                )
                return(list(Ref.coord_LON = Ref.coord_LON,
                            Ref.coord_LAT = Ref.coord_LAT,
                            MEAS_LON = NULL,
                            MEAS_LAT = NULL,
                            popup_REF  = NULL,
                            popup_MEAS = NULL))}
        })
        output$mymapExtrap <- renderLeaflet({
            if ((!is.null(pointsExtrap()$MEAS_LON) && !is.null(pointsExtrap()$MEAS_LAT)) || (!is.null(pointsExtrap()$Ref.coord_LON) && !is.null(pointsExtrap()$Ref.coord_LAT))) {
                title_CAL <- paste0('<h0><strong>', "Position of ", CalSet()$AirSensEur.name, " during calibration",
                                    "</i></strong><br> The blue pointer is the location of the reference station, <br> the grey circle is the location of the AirSensEur during calibration")
                m <- leaflet() %>%
                    addTiles(group = "OSM (default)") %>%
                    addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
                    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
                    addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
                    addLayersControl(
                        baseGroups = c("Road map", "Satellite", "Toner Lite"),
                        options = layersControlOptions(collapsed = TRUE))
                #setView(lng = mean(pointsExtrap()$MEAS_LON), lat = mean(pointsExtrap()$MEAS_LAT), zoom = 10)
                if (!is.null(pointsExtrap()$MEAS_LON) && !is.null(pointsExtrap()$MEAS_LAT)) {
                    m <- m %>%
                        fitBounds(lng1 = min(pointsExtrap()$MEAS_LON, na.rm = T),
                                  lat1 = min(pointsExtrap()$MEAS_LAT, na.rm = T),
                                  lng2 = max(pointsExtrap()$MEAS_LON, na.rm = T),
                                  lat2 = max(pointsExtrap()$MEAS_LAT, na.rm = T),
                                  options = list(maxZoom = 16)) %>%
                        addCircleMarkers(lng = pointsExtrap()$MEAS_LON, lat = pointsExtrap()$MEAS_LAT,
                                         popup = pointsExtrap()$popup_CAL, opacity = 1, color = "black", fillOpacity = 0.7, radius = 5, weight = 1)}
                if (!is.null(pointsExtrap()$Ref.coord_LON) && !is.null(pointsExtrap()$Ref.coord_LAT)) {
                    m <- m %>%
                        addMarkers(lng = pointsExtrap()$Ref.coord_LON, lat = pointsExtrap()$Ref.coord_LAT,
                                   popup = pointsExtrap()$popup_REF) %>%
                        addPopups(pointsExtrap()$Ref.coord_LON,
                                  pointsExtrap()$Ref.coord_LAT +
                                      max(c(0.001,0.1*sqrt(diff(range(pointsExtrap()$MEAS_LON))^2+diff(range(pointsExtrap()$MEAS_LAT))^2)), na.rm = T),
                                  title_CAL,
                                  options = popupOptions(closeOnClick  = FALSE))}
                m
            }
        })
        output$Prediction   <- renderPlot(Plot.Prediction()  , width = 'auto', height = 'auto')
        # Reactive FUN Plot.Prediction
        Plot.Prediction     <- eventReactive({
            input$Reference.name
            input$Sensors
            input$Selected
            input$Merge
            # if CalSet() is used it becomes reactive every something changes in CalSet()
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DateMeas",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DatePlotMeas",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Cal",i)]]))
            DF.aggregated.Avg()
            Cal$Forced
        },{
            # return the linear model of calibration
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "[shiny, Plot.Prediction()] INFO, Plotting scatter plot of predicted sensor data", value = 0.5)
            
            # Set Measuring functions, plots GRAPHS AND STATISTICS ON MODELLED quantities
            op <- par(no.readonly = TRUE)
            # Restoring graphical parameters on exit of function
            on.exit(par(op))
            
            # Selecting date range
            General.df <- DF.aggregated.Avg()[date >= CalSet()$Meas.DateIN & date <= CalSet()$Meas.DateEND + 1]
            
            # Setting Covariates_i for multi variable model
            if (CalSet()$mod.eta.model.type == "MultiLinear") {
                Covariates      <- input[[paste0("CovMod",CalSet()$k)]]
            } else Covariates      <- NULL
            
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat(paste0("[shiny, Plot.Prediction()] INFO, Plotting Predicted data for ", isolate(CalSet()$name.sensor)), sep = "\n")
            # next in case no data to be calibrated
            if (is.null(General.df[[c(CalSet()$nameGasMod)]]) || all(is.na(General.df[[CalSet()$nameGasVolt]]))) {
                Cal$Forced <<- TRUE
                my_message <- paste0("[shiny, Plot.Prediction()] ERROR, No predicted data for sensor ",CalSet()$name.sensor, "\n")
                cat(my_message)
                shinyalert(
                    title = "ERROR Predication data",
                    text = my_message,
                    closeOnEsc = TRUE,
                    closeOnClickOutside = TRUE,
                    html = FALSE,
                    type = "error",
                    showConfirmButton = TRUE,
                    showCancelButton  = FALSE,
                    confirmButtonText = "OK",
                    confirmButtonCol  = "#AEDEF4",
                    timer             = 0,
                    imageUrl          = "",
                    animation         = FALSE)
            } else {
                cat(paste0("[shiny, Plot.Prediction()] INFO, using calibration ",input[[paste0("Cal",CalSet()$k)]], " with ", input[[paste0("Calibration",CalSet()$k)]], " calibration method\n"))
                #loading the image of calibration
                if (CalSet()$remove.neg) negatif <- "_remove.neg_" else negatif <- "_"
                # Loading previous model
                #Model.i         <- readRDS(file = file.path(CalSet()$WDoutputMod, CalSet()$Cal))
                Pattern  <- rbind(c("Out.", ""),c("Ref.", "Reference "),c("ppm",""),c("ppb",""),c("_modelled",""), c("_",""))
                if (nrow(Pattern) > 0) {
                    Name.pol <- c(gsub(pattern = Pattern[1,1], replacement = Pattern[1,2], x = CalSet()$nameGasRef), gsub(pattern = Pattern[1,1], replacement = Pattern[1,2], x = CalSet()$nameGasMod))
                }
                if (nrow(Pattern) > 1) {
                    for (i in 2:nrow(Pattern)) {
                        Name.pol[1] <- gsub(pattern = Pattern[i,1], replacement = Pattern[i,2], x = Name.pol[1])
                        Name.pol[2] <- gsub(pattern = Pattern[i,1], replacement = Pattern[i,2], x = Name.pol[2])
                    }
                }
                Name.pol[1] <- paste0(Name.pol[1] ," in ",CalSet()$unit.ref, " at ",CalSet()$Reference.name)
                Name.pol[2] <- paste0(Name.pol[2] ,paste0(", calibrated data of ", CalSet()$name.sensor," sensor in ",CalSet()$unit.sensor))
                x <- General.df[[c(CalSet()$nameGasRef)]]
                y <- General.df[[c(CalSet()$nameGasMod)]]
                if (is.null(y)) Cal$Forced <<- TRUE else {
                    EtalLim <- Etalonnage( x = x,
                                           s_x = NULL,
                                           y = y,
                                           s_y = NULL,
                                           AxisLabelX = Name.pol[1],
                                           AxisLabelY = Name.pol[2],
                                           Title = paste0("Prediction with ", CalSet()$Cal," from ",
                                                          format(CalSet()$Meas.DateIN,"%y-%m-%d")," to ",format(CalSet()$Meas.DateEND,"%y-%m-%d")), 
                                           Marker = 1,
                                           Couleur = "blue",
                                           ligne = 'p',
                                           XY_same = TRUE,
                                           lim = NULL,
                                           steps = c(10,10),
                                           digitround = c(1,1),
                                           marges = c(4,4,3,0.5))
                    Comparison <- Cal_Line(x             = x,
                                           s_x           = NULL,
                                           y             = y,
                                           s_y           = NULL,
                                           Mod_type      = CalSet()$eta.model.type,
                                           Matrice       = General.df,
                                           line_position = 0,
                                           Couleur       = "red",
                                           Sensor_name   = "",
                                           f_coef1       = "%.3e",
                                           f_coef2       = "%.3e",
                                           f_R2          = "%.4f",
                                           lim           = EtalLim,
                                           marges        = NULL,
                                           Covariates    = NULL,
                                           Weighted      = FALSE,
                                           Lag_interval  = (max(x, na.rm = T) - min(x, na.rm = T)) / 15,
                                           Auto.Lag      = CalSet()$Sync.Pred)
                    # Saving plot if requested
                    if (input$SavePlot) {
                        # adding minutes if DF$General is aggregated
                        if (input$UserMinsAvg == input$UserMins) {
                            File.name <- file.path(CalSet()$WDModelled_gas, paste0(CalSet()$Cal,"_Calibrated_", format(CalSet()$Meas.DateIN, "%Y%m%d"),"_", format(CalSet()$Meas.DateEND,"%Y%m%d"),".png"))
                        } else File.name <- file.path(CalSet()$WDModelled_gas, paste0(CalSet()$Cal,"_Calibrated_", format(CalSet()$Meas.DateIN, "%Y%m%d"),"_", format(CalSet()$Meas.DateEND,"%Y%m%d"),"_", input$UserMinsAvg,"mins.png"))
                        dev.copy(png,
                                 filename = File.name,
                                 units = "cm",
                                 width = 20,
                                 height = 20,
                                 res = 300
                        )
                        dev.off()
                        cat(paste0("[shiny] INFO, ", CalSet()$Cal,"_Calibrated_",
                                   format(CalSet()$Meas.DateIN, "%Y%m%d"),"_",
                                   format(CalSet()$Meas.DateEND,"%Y%m%d"),".png saved in ", CalSet()$WDModelled_gas, "\n" ))
                        updateCheckboxInput(session, inputId = "SavePlot", value = FALSE)
                        for (i in c("formula","terms","model")) {
                            if (any(i %in% names(Comparison))) {
                                rm(list = ls(envir = attr(Comparison[[i]], ".Environment")), envir = attr(Comparison[[i]], ".Environment"))
                                break
                            }
                        }
                        saveRDS(object = Comparison,
                                file   = file.path(CalSet()$WDModelled_gas, paste0(CalSet()$Cal, "__",
                                                                                   CalSet()$eta.model.type, "__",
                                                                                   format(CalSet()$Meas.DateIN, "%Y%m%d"),"__",
                                                                                   format(CalSet()$Meas.DateEND,"%Y%m%d"),"__",
                                                                                   ".rds"))
                        )
                    }
                }
            }
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            progress$set(message = "[shiny, Plot.Prediction()] INFO, Plotting scatter plot of predicted sensor data", value = 1)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
            if (exists("Comparison")) return(Comparison)
        })
        # NavBar"Data Treatment", mainTabPanel SummaryExtra - Prediction ----
        output$SummaryExtra   <- renderPrint(Table.SummaryExtra())
        # Reactive FUN Table.SummaryExtra
        Table.SummaryExtra     <- reactive({
            # Print results of comparison model calibrated data vs reference data
            # depends: input[[paste0("DateCal",CalSet()$k)]],input[[paste0("Date",CalSet()$k)]],
            #           CalSet()$Cal_Line, CalSet()$Cal)
            #
            # Isolates: CalSet()$mod.eta.model.type, input[[paste0("Calibration",CalSet()$k)]] when calibrating
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            progress$set(message = "[shiny, Table.SummaryExtra()] INFO, Summary table of comparison model", value = 0.5)
            on.exit(progress$close())
            # Date range: intersection between the range for Prediction and the range for plotting
            # DateIN  <- max(c(input[[paste0("DateMeas",CalSet()$k)]][1],input[[paste0("DatePlotMeas",CalSet()$k)]][1]), na.rm = TRUE)
            # DateEND <- min(c(input[[paste0("DateMeas",CalSet()$k)]][2],input[[paste0("DatePlotMeas",CalSet()$k)]][2]), na.rm = TRUE)
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat(paste0("[shiny, Table.SummaryExtra()] INFO, printing comparison model of predicted data vs reference data of sensor ", input$Sensors, sep = "\n"))
            # # Comparison model saved in plot.Prediction
            # if (CalSet()$Neg.mod) Negative <- "Neg_mode_TRUE" else Negative <- "Neg_mode_FALSE"
            # Comparison = file.path(paste0(CalSet()$Cal, "__",
            #                               CalSet()$eta.model.type,"__",
            #                               Negative,"__",
            #                               format(DateIN,"%Y%m%d"),"__",format(DateEND,"%Y%m%d"),
            #                               ".rds"))
            # checking that CalSet()$Cal is not empty
            options(digits = 10)
            if (CalSet()$Cal != "") return.SummaryExtra <- summary(Plot.Prediction()) else  {
                cat(paste0("[shiny, Table.SummaryExtra()] ERROR, calibration model is empty\n"))
                return.SummaryExtra <- paste0("[Shiny]Table.SummaryExtra, ERROR, calibration model is empty\n")
            }
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            progress$set(message = "[shiny, Table.SummaryExtra()] INFO, Summary table of comparison model", value = 1)
            on.exit(progress$close())
            print(return.SummaryExtra)
        })
        # NavBar"Data Treatment", mainTabPanel TimeSeries - Prediction ----
        plot.ts_Extra_dygraphs <- eventReactive({
            input$Sensors
            input$Selected
            input$Merge
            # if CalSet() is used it becomes reactive every something changes in CalSet()
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DateMeas",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DatePlotMeas",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Cal",i)]]))
            DF.aggregated.Avg()
            Cal$Forced
        },{
            # plotting correlation in time series of validated data with Covariates
            
            progress <- shiny::Progress$new()
            progress$set(message = "[shiny, ts_extra_dygraphs()] INFO, plotting time series of Predicted data", value = 0.5)
            # # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat("[shiny, ts_extra_dygraphs()] INFO, plotting time series of Predicted data\n")
            General.df <- DF.aggregated.Avg()[date >= CalSet()$Meas.DateIN & date <= CalSet()$Meas.DateEND + 1,
                                              .SD, .SDcols = c("date", CalSet()$nameGasRef, CalSet()$nameGasMod)]
            if (all(is.na(General.df[, .SD, .SDcols = c(CalSet()$nameGasRef,CalSet()$nameGasMod)]))) {
                cat("[shiny, ts_extra_dygraphs()] ERROR, All data in calibration time series are empty, not plotting any times series\n")
            } else {
                # Renaming variables
                # changing names of variables
                Pattern  <- rbind(c("Out.", ""),
                                  c("ppm",""), c("ppb",""),
                                  c("Ref.", paste0("Reference, ", CalSet()$unit.ref, ", ")),
                                  c("_volt", paste0(", ",CalSet()$Sens.raw.unit)),
                                  c(CalSet()$gas.sensor, paste0(CalSet()$name.sensor)),
                                  c("_modelled",paste0(", ",CalSet()$unit.sensor)),
                                  c("_"," "))
                Name.pol <- c( CalSet()$nameGasRef, CalSet()$nameGasMod)
                for (i in 1:nrow(Pattern)) {
                    Name.pol[1] <- sub(pattern = Pattern[i,1], replacement = Pattern[i,2], x = Name.pol[1])
                    Name.pol[2] <- sub(pattern = Pattern[i,1], replacement = Pattern[i,2], x = Name.pol[2])
                }
                # make interactive time-series plot
                TZ <- threadr::time_zone(General.df[["date"]][1])
                ts_extra <- cbind(xts(General.df[[CalSet()$nameGasRef]], order.by = General.df[["date"]], tzone = TZ),
                                  xts(General.df[[CalSet()$nameGasMod]], order.by = General.df[["date"]], tzone = TZ))
                names(ts_extra) <- c(CalSet()$nameGasRef, CalSet()$nameGasMod)
                colour_vector <- threadr::ggplot2_colours(45)
                plot_extra <- dygraph(ts_extra,
                                      main = paste0("Prediction with ", CalSet()$Cal," from ",
                                                    format(CalSet()$Meas.DateIN,"%y-%m-%d")," to ",format(CalSet()$Meas.DateEND,"%y-%m-%d"))) %>%
                    dySeries(CalSet()$nameGasRef,label = Name.pol[1], color = "red") %>%
                    dySeries(CalSet()$nameGasMod,label = Name.pol[2], color = "blue") %>%
                    dyAxis("y", label = Name.pol[2]) %>%
                    dyLegend(show = "always", hideOnMouseOut = FALSE, width = 500) %>%
                    dyRangeSelector(height = 40) %>%
                    dyOptions(labelsUTC = T) # plot in UTC
                #     dyOptions(useDataTimezone = TRUE) %>% # do not use the local time zone
                # Saving plot if requested
                if (input$SavePlot) {
                    filename_html <- file.path(CalSet()$WDModelled_gas, paste0(CalSet()$Cal,"_Calibration_ts_",
                                                                               format(CalSet()$Meas.DateIN, "%Y%m%d"),"_",
                                                                               format(CalSet()$Meas.DateEND,"%Y%m%d"),"temp.html"))
                    filename_png <- file.path(CalSet()$WDModelled_gas, paste0(CalSet()$Cal,"_Calibration_ts_",
                                                                              format(CalSet()$Meas.DateIN, "%Y%m%d"),"_",
                                                                              format(CalSet()$Meas.DateEND,"%Y%m%d"),".png"))
                    save_html(plot_extra, filename_html)
                    webshot(filename_html, file     = filename_png, cliprect = "viewport")
                    # Update button save plot
                    updateCheckboxInput(session, inputId = "SavePlot", value = FALSE)
                }
            }
            
            # Return
            cat("-----------------------------------------------------------------------------------\n")
            plot_extra
        })
        output$ts_Extra_dygraphs <- renderDygraph(plot.ts_Extra_dygraphs())
        
        # NavBar"Data Treatment", mainTabPanel residualMatrix - Prediction ----
        output$ResExtraMatrix  <- renderPlot(Plot.ResPredictedMatrix()   , width = 'auto', height = 'auto')
        # Reactive FUN Plot.ResExtraMatrix
        Plot.ResPredictedMatrix    <- eventReactive({
            input$Sensors
            input$Selected
            input$Merge
            # if CalSet() is used it becomes reactive every something changes in CalSet()
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DateMeas",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DatePlotMeas",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Cal",i)]]))
            DF.aggregated.Avg()
            Cal$Forced
        },{
            # Plotting correllation matrix using pairs()
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "[shiny, Plot.ResExtraMatrix()] INFO, Plotting matrix plots of Prediction with residuals", value = 0.5)
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat("[shiny, Plot.ResExtraMatrix()] INFO, plotting correlation matrix of calibration residuals data with Covariates\n")
            
            # Select aggregated dataFrame or non aggregated if needed
            General.df <- DF.aggregated.Avg()[date >= CalSet()$Meas.DateIN & date <= CalSet()$Meas.DateEND + 1]
            
            # Sensor relationships with other variables
            Relationships <- unique(c(na.omit(names(General.df)[names(General.df) %in% input[[paste0("Sens",CalSet()$k)]] ] ), CalSet()$nameGasRef, CalSet()$nameGasMod))
            # filter date, select Relationships, add residuals
            General.df <- General.df[date >= CalSet()$Meas.DateIN & date <= CalSet()$Meas.DateEND + 1, .SD, .SDcols = c("date", Relationships)]
            General.df[, Residuals :=  General.df[[CalSet()$nameGasMod]] - General.df[[CalSet()$nameGasRef]]]
            Relationships <- c(Relationships, "Residuals")
            if (all(is.na(General.df[, .SD, .SDcols = Relationships]))) {
                my_message <- "[[Shiny, Plot.ResExtraMatrix ()] ERROR, All sensor time series are empty, not plotting any correlation matrix\n"
                cat(my_message)
                shinyalert(
                    title = "ERROR no data to plot",
                    text = my_message,
                    closeOnEsc = TRUE,
                    closeOnClickOutside = TRUE,
                    html = FALSE,
                    type = "error",
                    showConfirmButton = TRUE,
                    showCancelButton  = FALSE,
                    confirmButtonText = "OK",
                    confirmButtonCol  = "#AEDEF4",
                    timer             = 3000,
                    imageUrl          = "",
                    animation         = FALSE)
            } else {
                cat(paste0("[shiny, Plot.ResExtraMatrix()] INFO, Plot sensor data in volt with Covariates for Sensor ", input$Sensors, " in order to check relationships with other variables\n"))
                # changing names of variables
                Pattern  <- rbind(c("Out.", ""),
                                  c("ppm",""), c("ppb",""),
                                  c("Ref.", paste0("Reference, ", CalSet()$unit.ref, " ")),
                                  c("_volt", paste0(", ",CalSet()$Sens.raw.unit)),
                                  c(CalSet()$gas.sensor, paste0(CalSet()$name.sensor)),
                                  c("_modelled",paste0(", ",CalSet()$unit.sensor)),
                                  c("_"," "))
                if (nrow(Pattern) > 0) Labels <- gsub(pattern = Pattern[1,1], replacement = Pattern[1,2], x = Relationships)
                if (nrow(Pattern) > 1) for (i in 2:nrow(Pattern)) Labels <- gsub(pattern = Pattern[i,1], replacement = Pattern[i,2], x = Labels)
                op <- par(no.readonly = TRUE)
                # Restoring graphical parameters on exit of function
                on.exit(par(op))
                par(mfrow = c(1,1))
                # in panel smooth() change pch and cex, in panel.cor() change digits and cex.cor, in pairs() change cex.labels to fit the plots
                pairs(General.df[, .SD, .SDcols = c(Relationships)],
                      lower.panel = panel.smooth,
                      upper.panel = panel.cor,
                      diag.panel  = panel.hist,
                      labels = Labels,
                      main = paste0("Correlation matrix (R2 in bold) with ", CalSet()$Cal," from ",
                                    format(CalSet()$Meas.DateIN,"%y-%m-%d")," to ",format(CalSet()$Meas.DateEND,"%y-%m-%d"),
                                    " between ", CalSet()$Meas.DateIN, " and ", CalSet()$Meas.DateEND),
                      cex.labels = 2) # cex.cor = 1.3
                # Saving plot if requested
                if (input$SavePlot) {
                    # adding minutes if DF$General is aggregated
                    if (input$UserMinsAvg == input$UserMins) {
                        File.name <- file.path(CalSet()$WDModelled_gas, paste0(CalSet()$Cal,"_Res_pairs_", format(CalSet()$Meas.DateIN, "%Y%m%d"),"_", format(CalSet()$Meas.DateEND,"%Y%m%d"),".png"))
                    } else File.name <- file.path(CalSet()$WDModelled_gas, paste0(CalSet()$Cal,"_Res_pairs_", format(CalSet()$Meas.DateIN, "%Y%m%d"),"_", format(CalSet()$Meas.DateEND,"%Y%m%d"),"_", input$UserMinsAvg,"mins.png"))
                    dev.copy(png, filename = File.name,
                             type   = "cairo",
                             units  = "cm",
                             width  = 35.55,
                             height = 20,
                             res    = 300)
                    dev.off()
                    cat(paste0("[shiny] INFO, ", CalSet()$Cal,"_Res_pairs_",
                               format(min(General.df[["date"]], na.rm = TRUE),"%Y%m%d"),"_",
                               format(max(General.df[["date"]], na.rm = TRUE),"%Y%m%d"),".png saved in ", CalSet()$WDModelled_gas, "\n" ))
                    updateCheckboxInput(session, inputId = "SavePlot", value = FALSE)
                }
                #}
            }
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            progress$set(message = "[shiny, Plot.ResExtraMatrix()] INFO, Plotting matrix plots of Prediction with residuals", value = 1)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
        })
        # NavBar"Data Treatment", mainTabPanel Uncertainty - Prediction  ----
        output$U_Table      <- renderTable(Table.U_Table(), rownames = TRUE, width = 'auto')
        Table.U_Table <- eventReactive({
            input$Sensors
            input$Selected
            input$Merge
            # if CalSet() is used it becomes reactive every something changes in CalSet()
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Ref.unit",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Sens.unit",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Cal",i)]]))
            U.orth.List()
            Cal$Forced
        },{
            data.frame(
                values = sapply(U.orth.List()[which(names(U.orth.List()) %in% names(U.orth.List())[-which(names(U.orth.List()) %in% c("Mat", "RS.Fitted"))])],function(x) x),
                # units of parameters= # c("mo","sdo", "mm","sdm", "b1", "ub1", "b0", "ub0", "RSS","rmse", "mbe", "Correlation", "nb", "CRMSE", "NMSD", "Mat", "RS.Fitted")
                units  = c(paste0(CalSet()$unit.sensor),
                           paste0(CalSet()$unit.sensor),
                           paste0(CalSet()$unit.sensor),
                           paste0(CalSet()$unit.sensor),
                           paste0(""),
                           paste0(""),
                           paste0(CalSet()$unit.sensor),
                           paste0(CalSet()$unit.sensor),
                           paste0(CalSet()$unit.sensor,"^2"),
                           paste0(CalSet()$unit.sensor,"^2"),
                           paste0(CalSet()$unit.sensor),
                           paste0(""),
                           paste0(""),
                           paste0(CalSet()$unit.sensor,"^2"),
                           paste0("")),
                Mames = c("Mean of reference values", "Standard deviation of reference values","Mean of sensor values", "Standard deviation of sensor values",
                          "Slope of orthogonal regression line", "Standard uncertainty of the slope", "Intercept of orthogonal regression line", "Standard uncertainty of the intercept",
                          "Sum of square residuals of the regression line", "Root mean square of Error sqrt(sigma(reference-sensor)2/nb)", "Mean bias error (mm-mo)/mo",
                          "Coefficent of correlation of the reference and sensor values", "Number of valid pairs reference and sensor values",
                          "Centered Root Mean Square Error sqrt(sigma((sensor-mm)-(reference-mo))2/nb)", "Normalized Mean Standard Deviation (s(sensor) - s(reference))/s(reference)"),
                stringsAsFactors = TRUE)
        })
        Plot.Scatter <- eventReactive({
            input$Sensors
            input$Selected
            input$Merge
            # if CalSet() is used it becomes reactive every something changes in CalSet()
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DateMeas",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DatePlotMeas",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Ref.unit",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Sens.unit",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Cal",i)]]))
            U.orth.List()
            Cal$Forced
        },{
            Xlabel    = paste0(CalSet()$gas.sensor, " in ", CalSet()$unit.ref)
            Ylabel    = paste0("Sensor measurement in ", CalSet()$unit.sensor)
            Title     = paste0("Measurement uncertainty of sensor ",input$Sensors," between ",
                               format(CalSet()$Meas.DateIN, "%y%m%d")," and ", format(CalSet()$Meas.DateEND,"%y%m%d"))
            DQO.1     = CalSet()$DQO.1
            LV        = CalSet()$LV
            Units     = CalSet()$unit.sensor
            Dir       = CalSet()$WDModelled_gas
            nameModel = CalSet()$Cal
            Sensor_name    = CalSet()$name.sensor
            f_coef1        = "%.3f"
            f_coef2        = "%.3f"
            f_R2           = "%.4f"
            #-----------------[1]----------------------
            # Plots scatterplot of orthogonal regression
            #------------------------------------------
            Gamme <- Etalonnage(x          = U.orth.List()$Mat[["xis"]],
                                s_x        = NULL,
                                y          = U.orth.List()$Mat[["yis"]],
                                s_y        = NULL,
                                AxisLabelX = Xlabel,
                                AxisLabelY = Ylabel,
                                Title      = paste0("Orth. regression, ", CalSet()$Cal," , ", format(CalSet()$Meas.DateIN,"%y-%m-%d")," - ",format(CalSet()$Meas.DateEND,"%y-%m-%d")) ,
                                Marker     = 19,
                                Couleur    = 'blue',
                                lim        = NULL,
                                XY_same    = TRUE,
                                digitround = NULL,
                                marges     = NULL,
                                PlotAxis   = "s"
            )
            # Overlay new plot
            par(new = TRUE)
            # Set same margin for the line and Etalonnage
            par(mar=c(Gamme[,"mar12"],Gamme[,"mar34"]))
            # Plotting orthogonal regression line
            plot(U.orth.List()$Mat[["xis"]], U.orth.List()$b1 * U.orth.List()$Mat[["xis"]] + U.orth.List()$b0,
                 type = "l",
                 col  = "red",
                 xlim = Gamme[,1],
                 ylim = Gamme[,2],
                 axes = FALSE ,
                 xlab = "",
                 ylab = ""
            )
            if (is.null(f_coef1)) f_coef1 <- "%.2f"
            if (is.null(f_coef2)) f_coef2 <- "%.2f"
            if (is.null(f_R2))    f_R2    <- "%.4f"
            mtext(sprintf(paste0(Sensor_name, ", y= ",f_coef1,"+ ",f_coef2," x",", R2=",f_R2,", RMSE=",f_coef1), # ", s(Res)=",f_coef1,
                          U.orth.List()$b0,
                          U.orth.List()$b1,
                          U.orth.List()$Correlation^2,
                          #sd(b1 * Mat$xis + b0 - Mat$yis),
                          U.orth.List()$rmse),
                  line = 0,
                  adj  = 1,
                  padj = 0,
                  col  = "red",
                  cex  = 0.875)
            # Saving the plot of orthogonal regression
            dev.copy(png,filename = file.path(Dir,paste0(nameModel, "_Scatter.png")), units = "cm", res = 300, width = 22, height = 22);
            dev.off()
        })
        output$Scatter      <- renderPlot( Plot.Scatter(), width = 750, height = 750)
        plot.SqrRes    <- eventReactive({
            input$Sensors
            input$Selected
            input$Merge
            # if CalSet() is used it becomes reactive every something changes in CalSet()
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DateMeas",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DatePlotMeas",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Sens",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Ref.unit",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Sens.unit",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Cal",i)]]))
            U.orth.List()
            Cal$Forced
        },{
            Xlabel    = paste0(CalSet()$gas.sensor, " in ", CalSet()$unit.ref)
            Ylabel    = paste0("Sensor measurement in ", CalSet()$unit.sensor)
            Title     = paste0("Measurement uncertainty of sensor ",input$Sensors," between ",
                               format(CalSet()$Meas.DateIN, "%y%m%d")," and ", format(CalSet()$Meas.DateEND,"%y%m%d"))
            DQO.1     = CalSet()$DQO.1
            LV        = CalSet()$LV
            Units     = CalSet()$unit.sensor
            Dir       = CalSet()$WDModelled_gas
            nameModel = CalSet()$Cal
            Sensor_name    = CalSet()$name.sensor
            f_coef1        = "%.3f"
            f_coef2        = "%.3f"
            f_R2           = "%.4f"
            #-----------------[2]----------------------
            # Plots square of absolute residuals versus xis
            #------------------------------------------
            if (!is.null(Units)) Ylab = paste0("Square of Residuals in (", Units,")^2") else Ylab = "Square of Residuals"
            gamme <- Etalonnage(x          = U.orth.List()$Mat[["xis"]],
                                s_x        = NULL,
                                y          = U.orth.List()$Mat[["RS"]],
                                s_y        = NULL,
                                AxisLabelX = Xlabel,
                                AxisLabelY = Ylab,
                                Title      = paste0("Square of residuals ", CalSet()$Cal," , ", format(CalSet()$Meas.DateIN,"%y-%m-%d")," - ",format(CalSet()$Meas.DateEND,"%y-%m-%d")),
                                Marker     = 19,
                                Couleur    = 'blue',
                                lim        = NULL,
                                XY_same    = FALSE,
                                digitround = NULL,
                                marges     = NULL,
                                PlotAxis   = "s"
            )
            # if  the square of residuals are fitted
            if (U.orth.List()$RS.Fitted) {
                # plotting the line of the regression of the square of residuals
                # Overlay new plot
                par(new = TRUE)
                # Set same margin for the line and Etalonnage
                par(mar = c(gamme[,"mar12"],gamme[,"mar34"]))
                # Plotting orthogonal regression line
                order.xis <- order(U.orth.List()$Mat$xis)
                plot(x    = U.orth.List()$Mat[["xis"]][order.xis],
                     y    = U.orth.List()$Mat[["RS"]][order.xis],
                     type = "l",
                     col  = "red",
                     xlim = gamme[,"Xlim"],
                     ylim = gamme[,"Ylim"],
                     axes = FALSE ,
                     xlab = "",
                     ylab = "")
                mtext("The correlation between reference values and square residuals is significant. Fitting a Generalized additive model (k = 5)",
                      line = 1, adj  = 1, padj = 0, col  = "red", cex  = 0.875)
            } else {
                mtext("RSS set to constant, likely because the correlation between x and the squares of residuals is not significant",
                      line = 1, adj  = 1, padj = 0, col  = "red", cex  = 0.875)
            }
            # saving plots of squares of residuals with fitted model
            dev.copy(png,filename = file.path(Dir,paste0(nameModel, "_SqrRes.png")), units = "cm", res = 300, width = 22, height = 22);
            dev.off()
        })
        output$SqrRes  <- renderPlot(plot.SqrRes(), width = 750, height = 750)
        Plot.Uncertainty    <- eventReactive({
            input$Sensors
            input$Selected
            input$Merge
            # if CalSet() is used it becomes reactive every something changes in CalSet()
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DateMeas",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DatePlotMeas",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Ref.unit",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Sens.unit",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("ubsRM",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("ubss",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Cal",i)]]))
            U.orth.List()
            Cal$Forced
        },{
            # Saving plots of uncertainty of sensor data
            Xlabel    = paste0(CalSet()$gas.sensor, " in ", CalSet()$unit.ref)
            Ylabel    = paste0("Sensor measurement in ", CalSet()$unit.sensor)
            Title     = paste0("Uncertainty with ", CalSet()$Cal," , ", format(CalSet()$Meas.DateIN,"%y-%m-%d")," - ",format(CalSet()$Meas.DateEND,"%y-%m-%d"))
            DQO.1     = CalSet()$DQO.1
            DQO.2     = CalSet()$DQO.2                                                           # Data quality Objective for the gas.sensor
            DQO.3     = CalSet()$DQO.3                                                           # Data quality Objective for the gas.sensor
            LV        = CalSet()$LV
            Units     = CalSet()$unit.sensor
            Dir       = CalSet()$WDModelled_gas
            nameModel = CalSet()$Cal
            #-----------------[3]----------------------
            # Plots Expanded Uncertainty
            #------------------------------------------
            if (!is.null(CalSet()$unit.sensor)) Ylab = paste0("Expanded uncertainty in ", CalSet()$unit.sensor) else Ylab <- "Expanded uncertainty"
            order.xis <- order(U.orth.List()$Mat$xis)
            plot(U.orth.List()$Mat[["xis"]][order.xis],
                 U.orth.List()$Mat[["U"]][order.xis],
                 xlab  = Xlabel,
                 ylab  = Ylab,
                 main  = Title ,
                 col   = 'blue',
                 type  = "l",
                 ylim  = c(0, max(U.orth.List()$Mat$U, na.rm = T))
            )
            if (!is.na(LV)) {
                abline(v = LV)
                text(x = LV,
                     y = 0 + 0.05 * (max(U.orth.List()$Mat[order.xis, "U"], na.rm = T) - 0),
                     labels = "LV")
            }
            if (!is.na(DQO.1)) {
                abline(h = DQO.1) # in ppb
                text(x = min(U.orth.List()$Mat[order.xis, "xis"], na.rm = T) + 0.05 * (max(U.orth.List()$Mat[order.xis, "xis"], na.rm = T) - min(U.orth.List()$Mat[order.xis, "xis"], na.rm = T)),
                     y = DQO.1,
                     labels = "DQO.1")
            }
            if (!is.na(DQO.2)) {
                abline(h = DQO.2) # in ppb
                text(x = min(U.orth.List()$Mat[order.xis, "xis"], na.rm = T) + 0.05 * (max(U.orth.List()$Mat[order.xis, "xis"], na.rm = T) - min(U.orth.List()$Mat[order.xis, "xis"], na.rm = T)),
                     y = DQO.2,
                     labels = "DQO.2")
            }
            if (!is.na(DQO.3)) {
                abline(h = DQO.3) # in ppb
                text(x = min(U.orth.List()$Mat[order.xis, "xis"], na.rm = T) + 0.05 * (max(U.orth.List()$Mat[order.xis, "xis"], na.rm = T) - min(U.orth.List()$Mat[order.xis, "xis"], na.rm = T)),
                     y = DQO.3,
                     labels = "DQO.3")
            }
            grid(nx = NULL, ny = NULL, lty = 2, col = "grey")
            dev.copy(png,filename = file.path(Dir,paste0(nameModel,"_U.png")), units = "cm", res = 300, width = 22, height = 22);
            dev.off()
        })
        output$Uncertainty  <- renderPlot( Plot.Uncertainty(), width = 750, height = 750)
        # Reactive FUN U.orth.List
        U.orth.List <- eventReactive({
            input$Sensors
            input$Selected
            input$Merge
            # if CalSet() is used it becomes reactive every something changes in CalSet()
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DateMeas",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DatePlotMeas",i)]]))
            #unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Sens",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Ref.unit",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Sens.unit",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Cal",i)]]))
            DF.aggregated.Avg()
            Cal$Forced
        },{
            #----------------------------------------------------------CR
            # Returning parameters of the measurement uncertainty of the selected sensor using the method of the GDE
            #----------------------------------------------------------CR
            # depends:
            #   DF$General
            #   input$UserMinsAvg == input$UserMins
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            progress$set(message = "Calculating  measurement uncertainty of the selected sensor using the method of the GDE", value = 0.5)
            on.exit(progress$close())
            General.df <- DF.aggregated.Avg()[date >= CalSet()$Meas.DateIN & date <= CalSet()$Meas.DateEND + 1]
            cat("-----------------------------------------------------------------------------------\n")
            if (all(is.na(General.df[, .SD, .SDcols = c(CalSet()$nameGasRef,CalSet()$nameGasMod)]))) {
                cat("[Shiny]U.orth.List, ERROR, All data in calibrated time series are empty, not plotting any times series\n")
            } else {
                # Sensor relationships with other variables
                cat(paste0("[Shiny]U.orth.List, INFO, Calculating measurement uncertainty of calibrated data for Sensor ", input$Sensors, "\n"))
                U.orth.List <- U.orth.DF(
                    Mat          = cbind(1:nrow(General.df),
                                         General.df[, .SD, .SDcols = c("date", CalSet()$nameGasRef, CalSet()$nameGasMod)],
                                         rep(CalSet()$ubsRM,times = nrow(General.df)),
                                         rep(CalSet()$ubss,times = nrow(General.df))),
                    ubsRM          = as.numeric(CalSet()$ubsRM),
                    variable.ubsRM = FALSE,
                    ubss           = as.numeric(CalSet()$ubss),
                    variable.ubss  = FALSE,
                    Fitted.RS      = FALSE
                )
            }
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            progress$set(message = "Calculating the measurement uncertainty of the selected sensor using the method of the GDE", value = 1)
            # Make sure it closes when we exit this reactive, even if there's an error
            # on.exit(progress$close())
            return(U.orth.List)
        })
        # NavBar"Data Treatment", mainTabPanel Target - Prediction ----
        Plot.Target.File <- function(Type = "pdf") {
            # return:       u.Target.File the name of the plot with path
            return(file.path(CalSet()$WDModelled_gas, paste0(CalSet()$Cal,"_UTD_", format(CalSet()$Meas.DateIN,"%Y%m%d"),"_", format(CalSet()$Meas.DateEND,"%Y%m%d"),".",Type)))}
        plot.Target    <- eventReactive({
            input$Sensors
            input$Selected
            input$Merge
            # if CalSet() is used it becomes reactive every something changes in CalSet()
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DateMeas",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DatePlotMeas",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Ref.unit",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Sens.unit",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("ubsRM",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("ubss",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Cal",i)]]))
            U.orth.List()
            Cal$Forced
        },{
            #----------------------------------------------------------CR
            # Plotting Target diagram
            #----------------------------------------------------------CR
            # depends:
            #   CalSet(), U.orth.List()
            # isolates:
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat("[Shiny]plot.Target, INFO, plotting Target diagram\n")
            # Saving plot if requested
            u.Target.File <- Plot.Target.File()
            #if (!file.exists(u.Target.File) || file.size(u.Target.File) == 0 || input$SavePlot)
            pdf(file = file.path(u.Target.File), width = 8, height = 7, paper = "special", pagecentre = FALSE)
            # Plotting Modified Target Diagram
            Target.Diagram(Sensor_name = CalSet()$name.sensor,
                           Mat         = U.orth.List()[["Mat"]],
                           ubsRM       = as.numeric(CalSet()$ubsRM),
                           ubss        = as.numeric(CalSet()$ubss),
                           Unit.Ref    = CalSet()$unit.ref,
                           b0          = U.orth.List()[["b0"]],
                           b1          = U.orth.List()[["b1"]],
                           xAxisLabel  = NULL,
                           yAxisLabel  = NULL,
                           DQO.1       = CalSet()$DQO.1 / CalSet()$LV,
                           DQO.2       = CalSet()$DQO.2 / CalSet()$LV,
                           DQO.3       = CalSet()$DQO.3 / CalSet()$LV,
                           LAT         = CalSet()$LAT,
                           UAT         = CalSet()$UAT,
                           LV          = CalSet()$LV,
                           AT          = CalSet()$AT,
                           sdm_sdo     = U.orth.List()[["sdm"]] > U.orth.List()[["sdo"]],
                           Model.used  = CalSet()$Cal,
                           BeginEnd    = c(format(CalSet()$Meas.DateIN, "%Y%m%d"), format(CalSet()$Meas.DateEND, "%Y%m%d")))
            dev.off()
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
        })
        output$Target  <- renderText({
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "Plotting Target Diagram", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            plot.Target()
            # Return the filename with path
            addResourcePath("TargetDiag", normalizePath(dirname(Plot.Target.File()), winslash = "/"))
            width  = 900
            height = 900
            SRC    = paste0("TargetDiag/", basename(Plot.Target.File()))
            progress$set(message = "Plotting Target Diagram", value = 1)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            return(paste0('<iframe style = "height:',height,'px; width:100%" src="',SRC,'"></iframe>'))
        })
        # NavBar"Data Treatment", mainTabPanel Drift - Prediction ----
        Drift.df <- eventReactive({
            input$Sensors
            input$Selected
            input$Merge
            # if CalSet() is used it becomes reactive every something changes in CalSet()
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DateMeas",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DatePlotMeas",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Ref.unit",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Sens.unit",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Cal",i)]]))
            DF$General
            Cal$Forced
        },{
            #----------------------------------------------------------CR
            # creatin a reactive dataFrame with drift, duration and dose data
            #----------------------------------------------------------CR
            # depends:
            #   input$DateMeas
            #   input$DatePlotMeas
            #   CalSet()$k
            #   DF$General
            #   CalSet()$nameGasRef
            #   CalSet()$nameGasMod
            # isolates:
            General.df <- DF$General[date >= CalSet()$Meas.DateIN & date <= CalSet()$Meas.DateEND + 1, .SD, .SDcols = c("date", CalSet()$nameGasRef, CalSet()$nameGasMod)]
            # Removing rows with NaN
            General.df <- General.df[complete.cases(General.df),]
            General.df <- DF_avg(General.df, width = 60 * 24)
            # Calculating the long-term relative drift only for complete cases
            General.df[, rel.drift := (General.df[[CalSet()$nameGasMod]] - General.df[[CalSet()$nameGasRef]])*100/
                           General.df[[CalSet()$nameGasRef]]]
            General.df[, drift := General.df[[CalSet()$nameGasMod]] - General.df[[CalSet()$nameGasRef]]]
            # Adding duration
            General.df[, duration := as.numeric(difftime(General.df[["date"]],  General.df[1,date], units = "days"))]
            # Adding dose
            General.df[2:nrow(General.df), days := as.numeric(difftime(General.df[2:nrow(General.df), date], General.df[1:(nrow(General.df) - 1), date], units = "days"))]
            General.df[1, days := 0]
            General.df[, day.dose := General.df[["days"]] * General.df[[CalSet()$nameGasRef]]]
            General.df[, add.dose := cumsum(General.df[["day.dose"]])]
            return(General.df)
        })
        output$Drift  <- renderPlot(plot.drift()   , width = 'auto', height = 'auto')
        plot.drift <- eventReactive({
            input$Sensors
            input$Selected
            input$Merge
            # if CalSet() is used it becomes reactive every something changes in CalSet()
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DateMeas",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DatePlotMeas",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Ref.unit",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Sens.unit",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Cal",i)]]))
            DF$General
            Cal$Forced
        },{
            #----------------------------------------------------------CR
            # plotting drift in time series of calibrated sensor data
            #----------------------------------------------------------CR
            # depends:
            #   input$Sensors,
            #   list.name.sensor()
            #
            #   DF$General
            #   input[[paste0("Sens",k)]]
            #   input[[paste0("Date",k)]]
            # isolates:
            #dev.off()
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "[shiny, plot.drift()] INFO, Plotting long term drift of calibratedd sensor data", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            op <- par(no.readonly = TRUE)
            par(mfrow = c(1,1))
            # Restoring graphical parameters on exit of function
            on.exit(par(op))
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat("[shiny, plot.drift()] INFO, plotting long-term drift vs time of calibrated sensors\n")
            #
            if (all(is.na(Drift.df()))) {
                cat("[shiny, plot.drift()] ERROR, No data in calibrated time series, not plotting any drift times series\n")
            } else {
                plot(x = Drift.df()$duration, y = Drift.df()$drift,
                     #ylim = Ylim,
                     xlim = c(min(pretty(Drift.df()$duration, n = 10)), max(pretty(Drift.df()$duration, n = 10))),
                     xlab = "Number of days from 1st data transfer or selected date for plotting Prediction",
                     xaxt = "n",
                     xaxs = "i", # grid is putting nx grid lines in the user space, but plot is adding 4% extra space on each side. You can take control of this. Adding xaxs= "i", yaxs= "i"
                     ylab = CalSet()$unit.ref,
                     main = paste0("Daily residuals in ",CalSet()$unit.sensor," for ",input$Sensors," Sensors - Ref.)"),
                     col  = "blue",
                     type = "l",
                     lty  = 1,
                     lwd  = 1
                )
                points(x = Drift.df()$duration, y = Drift.df()$drift, col = "blue", xaxt = "n", yaxt = "n")
                # x axis labels
                axis(side = 1, at = pretty(Drift.df()$duration, n = 10), labels = pretty(Drift.df()$duration, n = 10))
                # grid for y axis
                grid(nx = NULL, ny = NULL, lty = 6, col = "grey")
                # grid for x axis, grid does align correctly with Posix and date
                #for (i in pretty(Drift.df()$duration, n = 10)[2:length(pretty(Drift.df()$duration, n = 10))]) abline(v= i, lty = 6, col = "grey")
                abline(h = 0)
                # plotting a trend line if there is correlation
                if (cor.test(Drift.df()$duration,Drift.df()$drift)$p.value < 0.05) {
                    # add trend line and equation
                    Cal_Line(x             = Drift.df()$duration,
                             s_x           = NULL,
                             y             = Drift.df()$drift,
                             s_y           = NULL,
                             Mod_type      = "Linear",
                             Matrice       = NULL,
                             line_position = -1,
                             Couleur       = "red",
                             Sensor_name   = input$Sensors,
                             f_coef1       = "%.1f",
                             f_coef2       = "%.2e",
                             f_R2          = "%.4f",
                             lim           = NULL,
                             marges        = c(5,4,4,1),
                             Covariates    = NULL
                    )
                }
                # Saving plot if requested
                if (input$SavePlot) {
                    # Directory for saving plots
                    WDoutput <- file.path(input$Selected, "Drift")
                    dev.copy(png,
                             filename = file.path(WDoutput, paste0(CalSet()$Cal,"_Drift_ts_",
                                                                   format(CalSet()$Meas.DateIN, "%Y%m%d"),"_",
                                                                   format(CalSet()$Meas.DateEND,"%Y%m%d"),".png")),
                             units    = "cm",
                             width    = 38,
                             height   = 27,
                             res = 200 )
                    dev.off()
                    cat(paste0("[shiny] INFO, ", CalSet()$Cal,"_Drift_ts_",
                               format(CalSet()$Meas.DateIN, "%Y%m%d"),"_",
                               format(CalSet()$Meas.DateEND,"%Y%m%d"),".png saved in ", WDoutput, "\n" ))
                    updateCheckboxInput(session, inputId = "SavePlot", value = FALSE)
                }
            }
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            progress$set(message = "[shiny, plot.drift()] INFO, Plotting time series of predicted sensor datPlotting long term drift of calibratedd sensor data", value = 1)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
        })
        # NavBar"Data Treatment", mainTabPanel Relative drift - Prediction ----
        output$Rel.Drift  <- renderPlot(plot.rel.drift()   , width = 'auto', height = 'auto')
        plot.rel.drift <- eventReactive({
            input$Sensors
            input$Selected
            input$Merge
            # if CalSet() is used it becomes reactive every something changes in CalSet()
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DateMeas",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DatePlotMeas",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Ref.unit",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Sens.unit",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Cal",i)]]))
            DF$General
            Cal$Forced
        },{
            #----------------------------------------------------------CR
            # plotting relative drift in time series of calibrated sensor data
            #----------------------------------------------------------CR
            # depends:
            #   input$Sensors,
            #
            #   DF$General
            # isolates:
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "Plotting long term relative drift of calibratedd sensor data", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            op <- par(no.readonly = TRUE)
            par(mfrow = c(1,1))
            # Restoring graphical parameters on exit of function
            on.exit(par(op))
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat("[Shiny]plot.relative drift, INFO, plotting long-term relative drift vs time of calibrated sensors\n")
            #
            if (all(is.na(Drift.df()))) {
                cat("[Shiny]plot.relative drift, ERROR, No data in calibrated time series, not plotting any relative drift times series\n")
            } else {
                plot(x = Drift.df()$duration, y = Drift.df()$rel.drift,
                     #ylim = Ylim,
                     xlim = c(min(pretty(Drift.df()$duration, n = 10)),max(pretty(Drift.df()$duration,n = 10))),
                     xlab = "Number of days from 1st data transfer or selected date for plotting Prediction",
                     xaxt = "n",
                     xaxs = "i", # grid is putting nx grid lines in the user space, but plot is adding 4% extra space on each side. You can take control of this. Adding xaxs= "i", yaxs= "i"
                     ylab = "%",
                     main = paste0("Daily relative residuals in % for ",input$Sensors," (Sensors - Ref.)/Ref. x 100"),
                     col  = "blue",
                     type = "l",
                     lty  = 1,
                     lwd  = 1
                )
                points(x = Drift.df()$duration, y = Drift.df()$rel.drift, col = "blue", xaxt = "n", yaxt = "n")
                # x axis labels
                axis(side = 1, at = pretty(Drift.df()$duration, n = 10), labels = pretty(Drift.df()$duration, n = 10))
                # grid for y axis
                grid (nx = NULL, ny = NULL, lty = 6, col = "grey")
                # grid for x axis, grid does align correctly with Posix and date
                #for (i in pretty(Drift.df()$duration, n = 10)[2:length(pretty(Drift.df()$duration, n = 10))]) abline(v= i, lty = 6, col = "grey")
                abline(h = 0)
                # plotting a trend line if there is correlation
                if (cor.test(Drift.df()$duration,Drift.df()$rel.drift)$p.value < 0.05) {
                    # add trend line and equation
                    Cal_Line(x = Drift.df()$duration,
                             s_x = NULL,
                             y = Drift.df()$rel.drift,
                             s_y = NULL,
                             Mod_type = "Linear",
                             Matrice  = NULL,
                             line_position = -1,
                             Couleur       = "red",
                             Sensor_name   = input$Sensors,
                             f_coef1 = "%.1f",
                             f_coef2 = "%.2e",
                             f_R2    = "%.4f",
                             lim     = NULL,
                             marges  = c(5,4,4,1),
                             Covariates = NULL
                    )
                }
                # Saving plot if requested
                if (input$SavePlot) {
                    # Directory for saving plots
                    WDoutput <- file.path(input$Selected, "Drift")
                    dev.copy(png,
                             filename = file.path(WDoutput, paste0(CalSet()$Cal,"_Rel.Drift_ts_",
                                                                   format(CalSet()$Meas.DateIN, "%Y%m%d"),"_",
                                                                   format(CalSet()$Meas.DateEND,"%Y%m%d"),".png")),
                             res = 300)
                    dev.off()
                    cat(paste0("[shiny] INFO, ", CalSet()$Cal,"_Rel.Drift_ts_",
                               format(CalSet()$Meas.DateIN, "%Y%m%d"),"_",
                               format(CalSet()$Meas.DateEND,"%Y%m%d"),".png saved in ", WDoutput, "\n" ))
                    updateCheckboxInput(session, inputId = "SavePlot", value = FALSE)
                }
            }
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            progress$set(message = "Plotting long term relative drift of calibratedd sensor data", value = 1)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
        })
        # NavBar"Data Treatment", mainTabPanel Absolute Drift vs dose - Prediction ----
        output$Dose.Drift  <- renderPlot(plot.Dose.Drift()   , width = 'auto', height = 'auto')
        plot.Dose.Drift <- eventReactive({
            input$Sensors
            input$Selected
            input$Merge
            # if CalSet() is used it becomes reactive every something changes in CalSet()
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DateMeas",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DatePlotMeas",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Ref.unit",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Sens.unit",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Cal",i)]]))
            DF$General
            Cal$Forced
        },{
            #----------------------------------------------------------CR
            # Plotting Absolute Drift vs dose of calibrated sensor data
            #----------------------------------------------------------CR
            # depends:
            #   input$DateMeas
            #   input$DatePlotMeas
            #   CalSet()$k
            #   DF$General
            #   CalSet()$nameGasRef
            #   CalSet()$nameGasMod
            #   input$Selected
            #   Drift.df() ( Drift.df()$add.dose, y = Drift.df()$drift )
            #   input$Sensors
            #   CalSet()$Cal
            # isolates:
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "Plotting Absolute Drift vs dose of calibrated sensor data", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            op <- par(no.readonly = TRUE)
            par(mfrow = c(1,1))
            # Restoring graphical parameters on exit of function
            on.exit(par(op))
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat("[Shiny]plot.relative drift, INFO, plotting long-term drift vs dose of calibrated sensors\n")
            #
            if (all(is.na(Drift.df()))) {
                cat("[Shiny]plot.relative drift, ERROR, No data in calibrated time series, not plotting any relative drift times series\n")
            } else {
                plot(x = Drift.df()$add.dose, y = Drift.df()$drift,
                     xlim = c(min(pretty(Drift.df()$add.dose, n = 10)),max(pretty(Drift.df()$add.dose,n = 10))),
                     xlab = paste0("Dose in ", CalSet()$unit.ref, ".days from 1st data transfer or selected date for plotting Prediction"),
                     xaxt = "n",
                     xaxs = "i", # grid is putting nx grid lines in the user space, but plot is adding 4% extra space on each side. You can take control of this. Adding xaxs= "i", yaxs= "i"
                     ylab = CalSet()$unit.ref,
                     main = paste0("Residuals vs dose in ", CalSet()$unit.ref," for ",input$Sensors," (Sensors - Ref.)"),
                     col  = "blue",
                     type = "l",
                     lty  =  1,
                     lwd  = 1
                )
                points(x = Drift.df()$add.dose, y = Drift.df()$drift, col = "blue", xaxt = "n", yaxt = "n")
                # x axis labels
                axis(side = 1, at = pretty(Drift.df()$add.dose, n = 10), labels = pretty(Drift.df()$add.dose, n = 10))
                # grid for y axis
                grid(nx = NULL, ny = NULL, lty = 6, col = "grey")
                abline(h = 0)
                # plotting a trend line if there is correlation
                if (cor.test(Drift.df()$add.dose,Drift.df()$drift)$p.value < 0.05) {
                    # add trend line and equation
                    Cal_Line(x = Drift.df()$add.dose, s_x = NULL,
                             y = Drift.df()$drift   , s_y = NULL,
                             Mod_type = "Linear",
                             Matrice  = NULL,
                             line_position = -1,
                             Couleur       = "red",
                             Sensor_name   = input$Sensors,
                             f_coef1 = "%.1f",
                             f_coef2 = "%.2e",
                             f_R2    = "%.4f",
                             lim     = NULL,
                             marges  = c(5,4,4,1),
                             Covariates = NULL
                    )
                }
                # Saving plot if requested
                if (input$SavePlot) {
                    # Directory for saving plots
                    WDoutput <- file.path(input$Selected, "Drift")
                    dev.copy(png,
                             filename = file.path(WDoutput,
                                                  paste0(CalSet()$Cal,"_Dose_",
                                                         format(CalSet()$Meas.DateIN, "%Y%m%d"),"_",
                                                         format(CalSet()$Meas.DateEND,"%Y%m%d"),".png")),
                             res = 300)
                    dev.off()
                    cat(paste0("[shiny] INFO, ", CalSet()$Cal,"_Dose_",
                               format(CalSet()$Meas.DateIN, "%Y%m%d"),"_",
                               format(CalSet()$Meas.DateEND,"%Y%m%d"),".png saved in ", WDoutput, "\n" ))
                    updateCheckboxInput(session, inputId = "SavePlot", value = FALSE)
                }
            }
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            progress$set(message = "Plotting Absolute Drift vs dose of calibrated sensor data", value = 1)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
        })
        # NavBar"Data Treatment", mainTabPanel Relative Drift vs dose - Prediction ----
        output$Rel.Dose.Drift  <- renderPlot(plot.Rel.Dose.Drift()   , width = 'auto', height = 'auto')
        plot.Rel.Dose.Drift <- eventReactive({
            input$Sensors
            input$Selected
            input$Merge
            # if CalSet() is used it becomes reactive every something changes in CalSet()
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DateMeas",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("DatePlotMeas",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Ref.unit",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Sens.unit",i)]]))
            unlist(sapply(seq_along(list.name.sensor()), function(i) input[[paste0("Cal",i)]]))
            DF$General
            Cal$Forced
        },{
            #----------------------------------------------------------CR
            # Plotting Relative Drift vs dose of calibrated sensor data
            #----------------------------------------------------------CR
            # depends:
            #   input$DateMeas
            #   input$DatePlotMeas
            #   CalSet()$k
            #   DF$General
            #   CalSet()$nameGasRef
            #   CalSet()$nameGasMod
            #   input$Selected
            #   Drift.df() ( Drift.df()$add.dose, y = Drift.df()$drift )
            #   input$Sensors
            #   CalSet()$Cal
            # isolates:
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "Plotting Relative Drift vs dose of calibrated sensor data", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            op <- par(no.readonly = TRUE)
            par(mfrow = c(1,1))
            # Restoring graphical parameters on exit of function
            on.exit(par(op))
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat("[Shiny]plot.relative drift, INFO, plotting long-term relative drift vs dose of calibrated sensors\n")
            #
            if (all(is.na(Drift.df()))) {
                cat("[Shiny]plot.relative drift, ERROR, No data in calibrated time series, not plotting any relative drift times series\n")
            } else {
                plot(x = Drift.df()$add.dose, y = Drift.df()$rel.drift,
                     xlim = c(min(pretty(Drift.df()$add.dose, n = 10)),max(pretty(Drift.df()$add.dose,n = 10))),
                     xlab = paste0("Dose in ", CalSet()$unit.ref, ".days from 1st data transfer or selected date for plotting Prediction"),
                     xaxt = "n",
                     xaxs = "i", # grid is putting nx grid lines in the user space, but plot is adding 4% extra space on each side. You can take control of this. Adding xaxs= "i", yaxs= "i"
                     ylab = "%",
                     main = paste0("Relative residuals vs dose in % for ",input$Sensors," (Sensors - Ref.)/Ref. x 100"),
                     col  = "blue",
                     type = "l",
                     lty  = 1,
                     lwd  = 1
                )
                points(x = Drift.df()$add.dose, y = Drift.df()$rel.drift, col = "blue", xaxt = "n", yaxt = "n")
                # x axis labels
                axis(side = 1, at = pretty(Drift.df()$add.dose, n = 10), labels = pretty(Drift.df()$add.dose, n = 10))
                # grid for y axis
                grid(nx = NULL, ny = NULL, lty = 6, col = "grey")
                # grid for x axis, grid does align correctly with Posix and date
                #for (i in pretty(Drift.df()$add.dose, n = 10)[2:length(pretty(Drift.df()$add.dose, n = 10))]) abline(v= i, lty = 6, col = "grey")
                abline(h = 0)
                # plotting a trend line if there is correlation
                if (cor.test(Drift.df()$add.dose,Drift.df()$rel.drift)$p.value < 0.05) {
                    # add trend line and equation
                    Cal_Line(x = Drift.df()$add.dose, s_x = NULL,
                             y = Drift.df()$rel.drift, s_y = NULL,
                             Mod_type = "Linear",
                             Matrice  = NULL,
                             line_position = -1,
                             Couleur       = "red",
                             Sensor_name   = input$Sensors,
                             f_coef1 = "%.1f",
                             f_coef2 = "%.2e",
                             f_R2    = "%.4f",
                             lim     = NULL,
                             marges  = c(5,4,4,1),
                             Covariates = NULL
                    )
                }
                # Saving plot if requested
                if (input$SavePlot) {
                    # Directory for saving plots
                    WDoutput <- file.path(input$Selected, "Drift")
                    dev.copy(png,
                             filename = file.path(WDoutput,
                                                  paste0(CalSet()$Cal,"_Rel.Dose_",
                                                         format(CalSet()$Meas.DateIN, "%Y%m%d"),"_",
                                                         format(CalSet()$Meas.DateEND,"%Y%m%d"),".png")),
                             res = 300
                    )
                    dev.off()
                    cat(paste0("[shiny] INFO, ", CalSet()$Cal,"_Rel.Dose_",
                               format(CalSet()$Meas.DateIN, "%Y%m%d"),"_",
                               format(CalSet()$Meas.DateEND,"%Y%m%d"),".png saved in ", WDoutput, "\n" ))
                    updateCheckboxInput(session, inputId = "SavePlot", value = FALSE)
                }
            }
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            progress$set(message = "Plotting Relative Drift vs dose of calibrated sensor data", value = 1)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
        })
        # NavBar"SelectASE", SideBar Button input$Save ,  ----
        observeEvent(input$Save, {
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            progress$set(message = "[shiny, Save()] INFO,Saving all data and Config files", value = 0.2)
            on.exit(progress$close())
            #-----------------------------------------------------------------------------------CR
            # 1 - Saving all config files
            #-----------------------------------------------------------------------------------CR
            # Saving file *.cfg
            # Updating with new names of chemical sensors
            sens2ref_new <- data.table::transpose(fread(file = cfg_file(), header = FALSE, na.strings=c("","NA", "<NA>")), fill = NA, make.names = 1)
            # Saving file *.cfg (df sens2ref)
            Outliers_Ref.added   <- c(which(colnames(Outliers_Ref()) == "name.gas"),which(!(names(Outliers_Ref()) %in% intersect(names(Outliers_Sensor()), names(Outliers_Ref())))))
            sens2ref             <- merge(x = Outliers_Sensor(), y = Outliers_Ref()[,Outliers_Ref.added], by = "name.gas", all.x = TRUE, sort = FALSE)
            # add a column "gas.sensor"
            sens2ref             <- merge(sens2ref, sens2ref_new[, which(names(sens2ref_new) %in% c("gas.sensor", "name.gas")), with = FALSE], by = "name.gas")
            Shield.added         <- c(which(colnames(Shield()) == "name.sensor"),which(!(names(Shield()) %in% intersect(names(sens2ref), names(Shield())))))
            sens2ref             <- merge(x = sens2ref, y = Shield()[,Shield.added], by = "name.sensor", all.x = TRUE, sort = FALSE)
            Calib_data.added     <- c(which(names(Calib_data()) == "name.gas"),which(!(names(Calib_data()) %in% intersect(names(sens2ref), names(Calib_data())))))
            sens2ref             <- merge(x = sens2ref, y = Calib_data()[,Calib_data.added], by = "name.gas", all.x = TRUE, sort = FALSE)
            sens2ref             <- sens2ref[,unique(names(sens2ref))]
            # saving
            fwrite(setDT(as.data.frame(t(sens2ref)), keep.rownames = "name.gas")[], file = cfg_file(), row.names = FALSE, col.names = FALSE)
            cat(paste0("[shiny, Save()] INFO,", ASE_name(),".cfg config file saved in directory General_data.\n"))
            # Saving ASE_name_Servers.cfg file
            progress$set(message = "[shiny, Save()] INFO,Saving ASE_name_Servers.cfg", value = 0.2)
            # Saving file *_Servers.R (df cfg)
            # in case Down.SOS is FALSE then input$airSensEUR is ""
            # replacing NULL variables with "" to be saved as string
            ColumnsName <- c("RefSOSname","Ref.SOS.name")
            for (j in ColumnsName) {
                if (is.null(input[[j]])) assign(j, "") else assign(j,input[[j]])
            }
            if (is.null(input$RefPollutants)) RefPollutants <- "" else RefPollutants <- paste0(input$RefPollutants, collapse = "!")
            if (is.null(input$RefDateDownload)) {
                RefDateStart <- ""
                RefDateEnd   <- ""
            } else {
                RefDateStart <- input$RefDateDownload[1]
                RefDateEnd   <- input$RefDateDownload[2]
            }
            ColumnsName <- c("Ref__a_i_p__name","Ref__a_i_p__name")
            for (j in ColumnsName) {
                if (is.null(input[[j]])) assign(j, "") else assign(j,input[[j]])
            }
            if (is.null(input$Ref__a_i_p__Pollutants)) Ref__a_i_p__Pollutants <- "" else Ref__a_i_p__Pollutants <- paste0(input$Ref__a_i_p__Pollutants, collapse = "!")
            if (is.null(input$Ref__a_i_p__Date)) {
                Ref.a_i_p.DateIN  <- ""
                Ref.a_i_p.DateEND <- ""
            } else {
                Ref.a_i_p.DateIN  <- input$Ref__a_i_p__Date[1]
                Ref.a_i_p.DateEND <- input$Ref__a_i_p__Date[2]
            }
            cfg <- data.frame(PROXY    = as.logical(input$PROXY),
                              URL      = input$URL,
                              PORT     = input$PORT,
                              LOGIN    = input$LOGIN,
                              PASSWORD = input$PASSWORD,
                              Down.Influx = as.logical(input$Down.Influx),
                              Host      = input$Host,
                              Port      = input$Port,
                              User      = input$User,
                              Pass      = input$Pass,
                              Db        = input$Db,
                              Dataset   = input$Dataset,
                              Influx.TZ = input$Influx.TZ,
                              Down.SOS        = as.logical(input$Down.SOS),
                              AirsensWeb      = input$AirsensWeb,
                              AirSensEur.name = CalSet()$AirSensEur.name,
                              SOS.TZ          = input$SOS.TZ,
                              Down.Ref       = as.logical(input$Down.Ref),
                              FTPMode        = input$FTPMode,
                              urlref         = input$urlref,
                              Reference.name = input$Reference.name,
                              RefSOSname     = RefSOSname,
                              Ref.SOS.name   = Ref.SOS.name,
                              RefPollutants  = RefPollutants,
                              RefDateStart   = RefDateStart,
                              RefDateEnd     = RefDateEnd,
                              Ref__a_i_p__name         = input$Ref__a_i_p__name,
                              User__a_i_p__            = input$User__a_i_p__,
                              Pass__a_i_p__            = input$Pass__a_i_p__,
                              Ref__a_i_p__Organisation = input$Ref__a_i_p__Organisation,
                              Ref__a_i_p__Station      = input$Ref__a_i_p__Station,
                              Ref__a_i_p__Pollutants   = Ref__a_i_p__Pollutants,
                              Ref.a_i_p.DateIN         = Ref.a_i_p.DateIN,
                              Ref.a_i_p.DateEND        = Ref.a_i_p.DateEND,
                              coord.ref      = paste0(input$coord.ref.Long,",",input$coord.ref.Lat),
                              alt.ref        = input$alt.ref,
                              ref.tzone      = input$ref.tzone,
                              asc.File       = input$asc.File,
                              UserMins       = as.numeric(input$UserMins),
                              UserMinsAvg    = as.numeric(input$UserMinsAvg),
                              Delay          = as.numeric(input$Delay),
                              stringsAsFactors = FALSE)
            # saving
            fwrite(setDT(as.data.frame(t(cfg)), keep.rownames = "PROXY")[],
                   file = Servers_file(), row.names = FALSE, col.names = FALSE)
            cat(paste0("[shiny, Save()] INFO,", ASE_name(),"_Servers.cfg config file saved in directory Configuration.\n"))
            # Saving file *_SETTIME
            progress$set(message = "[shiny, Save()] INFO,Saving Calibrated/predicted data in General.df", value = 0.2)
            sens2ref <- CalTime()
            fwrite(setDT(as.data.frame(t(sens2ref)), keep.rownames = "name.gas")[], file = SETTIME_file(), row.names = FALSE, col.names = FALSE)
            cat(paste0("[Shiny, Save()] INFO, ", paste0(ASE_name(),"_SETTIME.cfg")," config file saved in directory Configuration\n"))
            # saving Covariates and CovMod config file
            # Make sure it closes when we exit this reactive, even if there's an error
            progress$set(message = "[shiny, Save()] INFO, Saving Covariates and CovMod Config Files", value = 0.2)
            for (i in seq_along(list.name.sensor())) {
                write.csv(data.frame(Effects = input[[paste0("Sens",i)]],
                                     stringsAsFactors = FALSE),
                          file = file.path(input$Selected,"Configuration",paste0(ASE_name(),"_Covariates_",list.name.sensor()[i],".cfg")),
                          row.names = FALSE)
                write.csv(data.frame(Effects = input[[paste0("CovMod",i)]],
                                     stringsAsFactors = FALSE),
                          file = file.path(input$Selected,"Configuration",paste0(ASE_name(),"_CovMod_",list.name.sensor()[i],".cfg")),
                          row.names = FALSE)
            }
            cat(paste0("[shiny, Save()] INFO,", paste0(ASE_name(),"_CovMod*.cfg")," config file  saved in directory Configuration\n"))
            #-----------------------------------------------------------------------------------CR
            # 2 - Saving data in General.file()
            #-----------------------------------------------------------------------------------CR
            # Saving Calibrated/predicted data in General_data Files - Saved is put on quit otherwise it is too long
            progress$set(message = "[shiny, Save()] Saving Filtered, Calibrated and predicted data", value = 0.2)
            cat("-----------------------------------------------------------------------------------\n")
            cat("[shiny, Save()] Saving Filtered, Calibrated and predicted data in ", General.file(),"\n")
            #General.df <- DF$General
            #save(General.df, file = General.file())
            fwrite(DF$General, General.file(), na = "")
            progress$set(message = "[shiny, Save()] Saving Filtered, Calibrated and predicted data", value = 1.0)
            #-----------------------------------------------------------------------------------CR
            # 3 - Saving list of Indexes for warming, Temperature/Humidity, Invalid sensor data, Negative reference values, Outliers
            #-----------------------------------------------------------------------------------CR
            # Saving Index for warming of sensors
            cat("-----------------------------------------------------------------------------------\n")
            cat("[shiny, Save()] INFO, Saving Index of warming of sensors\n")
            progress$set(message = "[shiny, Save()] INFO, Saving Index of warming of sensors", value = 0.2)
            list.save(x = ind.warm$out, file = ind.warm.file())
            # Saving the list of Index of temperature and humidity out of interval of tolerance
            cat("[shiny, Save()] INFO, Saving Index of temperature and humidity out of interval of tolerance\n")
            progress$set(message = "[shiny, Save()] INFO, Saving Index of temperature and humidity out of interval of tolerance", value = 0.4)
            list.save(x = ind.TRh$out, file = ind.TRh.file())
            # Saving the list of Index of invalid sensor data
            cat("[shiny, Save()] INFO, Saving Index of invalid sensor data\n")
            progress$set(message = "[shiny, Save()] INFO, Saving Index of invalid sensor data", value = 0.6)
            list.save(x = ind.Invalid$out, file = ind.Invalid.file())
            # Saving the list of Index of sensor data outliers
            cat("[shiny, Save()] INFO, Saving Index of sensor data Outliers\n")
            progress$set(message = "[shiny, Save()] INFO, Saving Index of sensor data Outliers", value = 0.6)
            list.save(x = ind.sens$out, file = ind.sens.out.file())
            # Saving the list of Index of reference data outliers
            cat("[shiny, Save()] INFO, Saving Index of reference data Outliers\n")
            progress$set(message = "[shiny, Save()] INFO, Saving Index of reference data Outliers", value = 0.6)
            list.save(x = ind.ref$out, file = ind.ref.out.file())
            cat("-----------------------------------------------------------------------------------\n")
            progress$set(message = "[shiny, Save()] INFO, Saving all data and Config files", value = 1)
            on.exit(progress$close())
        })
        })
    # Button "Quit", NavBar "SelectASE" ----
    observeEvent(input$Quit, {
        sink()
        sessionInfo()
        js$closeWindow()
        # free memory
        rm(list = ls(all.names = TRUE))
        if (.Platform$OS.type == "windows") gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE) else gc(verbose = getOption("verbose"), reset = FALSE)
        stopApp(input$Config_Files)
    })
}
# Run the application ====
shinyApp(ui = ui, server = server)
