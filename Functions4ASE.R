# This script contains R-functions written for ASE$_Load_Data_Sensorweb -- to be included into Draft Sensor_Toolbox
#=====================================================================================CR
# Licence:
# Copyright 2017 EUROPEAN UNION
# Licensed under the EUPL, Version 1.2 or subsequent versions of the EUPL (the "License"); 
# You may not use this work except in compliance with the License. 
# You may obtain a copy of the License at: http://ec.europa.eu/idabc/eupl
# Unless required by applicable law or agreed to in writing, the software distributed 
# under the License is distributed on an "AS IS" basis, WITHOUT WARRANTIES OR CONDITIONS 
# OF ANY KIND, either express or implied. See the License for the specific language 
# governing permissions and limitations under the License.
# Date: 02/11/2016
# 
# Authors
# - Michel Gerboles        , michel.gerboles@ec.europa.eu  - European Commission - Joint Research Centre
# - Laurent Spinelle       , laurent.spinelle@ec.europa.eu - European Commission - Joint Research Centre
# - Maria Gabriella Villani, mariagabriella.villani@enea.it - ENEA
# - Marco Signorini        , marco.signorini@liberaintentio.com - Liberatintentio srl
# - Alex Kotsev            , alexander.kotsev@ec.europa.eu - European Commission - Joint Research Centre
#=====================================================================================CR
#
# Functions:
# 
# 1603?? MGV: fromGeneral2DataIN        general data prepared to be fed into the Solve linear equations models. With na removed (NOT INCLUDED)
# 141114 MGV: Cal_Line_mgv              calibration and plot of calibration line, modified version of Cal_Line with option Mod_type =='Linear.Robust', 
#                                       to model the median of y as a function of x, rather than modelling the mean of y as a function of x, in the case of least squares regression.
#                                       This Funtion is no more used and has bee deleted on 20190212
# 160418 MGV: Validation.tool           for validations (of what? )
# 160503 MGV: citytechNO2O3ModelDelta   model to solve gas sensors components  (only NO2 and O3) (NOT INCLUDED)
# 160505 MGV: SensorModel               model to solve gas sensors components  (for NO2, CO, O3 and NO?) (NOT INCLUDED)
# 161129 MG : SensorModel               Modification of the equation to make it working for any corrections function and set of sensors (NOT INCLUDED)

# 161029 MG : Check_Download            Check if General.Rdata file exists and if it is necessary to retrieve.data (true or false)
# 161030 MG : Down_SOS                  Download Sensor data from SOS?
# 161030 MG : Down_Ref                  Download Reference for data retrieving (NOT INCLUDED)
# 161031 MG : My.rm.Outliers            Removing outliers using the Median Average Deviation method
# 161107 MG : ASEDigi2Volt              converting sensor digital values to voltages
# 161107 MG : ASEVolt2Conc              converting sensor voltages to concetrations
# 161107 MG : Convert.ASE               converting sensor digital values to concetrations
# 161112 MG : Invalid.out               Removing Invalid data when ASE is indoor or not at a monitoring station (NOT INCLUDED)
# 161120 MG : Down_Influx               Downloading AirSensEUR data using the Influx protocol, create or update the airsenseur.db SQLite database, get timezone
# 161120 CR : get_google_tz             Getting time zone from Google API (NOT INCLUDED)
# 161123 MG : Sqlite2df                 converting a local airsenseur.db into a General dataframe
# 161125 MG : Make.Old                  Creating a copy of the file as old file 
# 170420 MG : Load.Packages             install and/or load packages
# 170609 MG : Pinging WEB site          Pinging site to check if internet is available
# 170609 MG : havingIP                  Checking availability of internet 
# 170609 MG : JAVAPanel04               Looking for the sensor config file of the AirSensEURPanel any files *.asc in directory Config_files
# 171207 MG : CONFIG                    Configuration of ASE BOX: 1. Configuring Proxy server, 2. Sensor configuration for download for Influx and SOS. InfluxDB has more info and is preferred over SOS
#                                       Reference data, configuration for download, ftp, 4. Create sensor configuration file and matching between reference and sensor names 
#                                       5. SET Average time for sensor data, 9. SET temperature and relative humidity thresholds for sensors validity
# 171207 MG : SETTIME                   Configuration of ASE BOX: 11. Valid Periods, 12. SET TIME PARAMETERS -> see in ASE_OPER_SCRIPT.R                               (NOT USED)
# 180131 MG : Json_To_df, Down_influx_old and Down_Influx: Returns a df a query data coverting from JSON with conversion of data columns from string to numeric
# 190309 MG : Function GENERAL add absolute humidity in the returned dataFrame

#=====================================================================================CR

#=====================================================================================CR
# Version History
#=====================================================================================CR
# 2017-01-26 First Commit
# 2017-02-03 SQLite2df, line 223: storing Page when reading airsenseur.db to be used for Aggregating in tabulated form
#                       line 299: adding the date when a sensor name changes
#                       line 302 - 364: paging the "Putting data in tabulated dataframe" to be progress of casting and timeAverage
#            Down_Influx, line 691 - 697: correction for determining the time zone.
#            Validation_Tool, line 2093 - : remove time from the names of files when saving a plot
# 2017-05-29 Add first stepts of ASEConfig-XX.R
#			 1. create file system structure, create log file
#			 2. Loading toolBox
#			 4. Install packages needed in the script (CRAN + Github)
# 2017-06-07  Added DateEND = NULL in Down_SOS in order to use the function on shorter time interval
# 2017-07+16  Changed Down_influx using packages httr and JSONlite instead of influxdr. Changed SQLite2df using Lubridate instead of strptime to convert chr(time) to POSIX
# 2018-01-17  Function ASEPanel04Read: removed some curly bracket missing, do not return TIA and REF, change label for Ref_source, mode and Fet_short. VREF replaced by RefAFE
#             problem of Ref changed in string solved
# 2018-01-31  Function Down_Influx, the download of Influx data is now much faster because the Influx query asks for averaged data according to input$UserMins. The data do not need be averaged in R anymore
# 2018-02-12  Function GraphOut: change of input "indfull": integer: the index of vector date, to be plotted as invalid points in red or a list of Posix with 4 elements
#             (Tmin, Tmax, RH.min and Rh.max). Using the names of list "infull" in legend.
# 2018-02-17  Function Check_Download, check of last date of file General.Rdata is added
#             FUNCTION GENERAL, if there is no new data in InfluxData and SOSData, the last General data frame is loaded when clicking button "Merge Influx, SOS - Ref.
# 2018-02-23  Function ASEDigi2Volt corrected mistake with the matrix calculation when more than 2 sensor were converted into nA or V
# 2018-02-27  Functions min.DateRange and max.DateRange, the extension of daterange was tested for + 25 % and set to + 75 %, correting by testing and setting at + 75 %
# 2018-03-18  Function Check_Download added field "ExistFil" to know if the files SOS, INFLUX, General, Reference and airseneur exist
#             and the fields dateIn givinf the start date for each file. Check_Download checks now wahat is in airsenseur.db
# 2018-03-27  Bug in function SETTIME, the variable DisqueFieldTest was used while not declared. Correction: variable DisqueFieldtest replaced with dirname(DisqueFiledtestDir)
# 180606 MG   Bug in SQLite2df, function selectBydate replace with subset
# 180606 MG   Bug in SETTIME, automatic correction of names of df sens2ref for smooth update between version shiny0.6 to shiny0.7, adding Cov date
# 180705 MG   Bug corrected in Validation.Tool: the size of calibration model file has been considerably decreased as the R environment was save in the file (100 MB --> < 100 kB) in function validation tool
# 181102 MG   Bug correction in Down_Influx(), setting Influx.TZ = Influx.TZ,to pass the time zone selected into 
# 181113 MG   Bug Correction in SETTIME, the time zone of all dates were set to the time zone of RefData. This is changed to setting to the time zone of DownloadSensor$DateIN.General.prev if it exists then to 
#             DateIN.Influx.prev if it exists then to DateIN.SOS.prev  if it exists otherwise it is set to "UTC"
# 181012 MG   my.rm.outliers: the computing time has been divided by two, comput ing the min and max of interval of tolerance within one rollapply
# 190110 MG   Etalonnage: an error has been colved when all s_y are NA
#           
# TO BE DONE:
#             Funtion Down-Influx repeats the download of last 4 weeks of date each time it is called (for now, unique() is used)
#=====================================================================================CR
### Function Load.Packages
#=====================================================================================CR
Load.Packages <- function(list.Packages, verbose = FALSE) {
    # This function install packages if needed and load them
    # list.Packages                 vector of names of the packages to load
    # verbose                       logical default FALSE, return info message about error and installed packages
    # dependence                    havingIP()
    
    cat("-----------------------------------------------------------------------------------\n")
    
    if (verbose) cat("[Load.Packages] INFO CHECK Installed packages and Toolbox to run the script\n")
    #
    # checking if internet is available and CRAN can be accessed
    # isInternet <- havingIP()
    # if (verbose) if (isInternet) cat("[Load.Packages] Info: internet is available\n") else cat("[Load.Packages] Info: internet is not available\n")
    
    for (i in list.Packages) {
        
        if (i %in% rownames(installed.packages()) == FALSE) {
            if (verbose) cat(sprintf("[Load.Packages] INFO Installing %s", i), sep = "\n")
            install.packages(i)
        } else {
            if (verbose) cat(sprintf("[Load.Packages] INFO Package %s already installed",i), sep = "\n")
        }
        
        do.call("library", as.list(i))
        if (verbose) cat(sprintf("[Load.Packages] INFO Package %s loaded",i), sep = "\n")
        
    }
    
    # List of loaded packages
    if (verbose) cat("[Load.Packages] INFO List of installed packages\n")
    if (verbose) print(search(), quote = FALSE)
    
    cat("-----------------------------------------------------------------------------------\n")
}

#=====================================================================================CR
# 161125 MG : Make.Old                  Creating a copy of the file as old file 
#=====================================================================================CR
Make.Old <- function(File, File.old = NULL) {
    # Make.Old updates or make a copy of File into File.old  . File is updated only if the date of File is newer than the one of File.old 
    # File File.old is NULL, .old is added to File ans is used as the name of File.old
    
    # File                              : character, name of the file to be copied 
    # File.old                          : character, default is NULL  
    # 
    # Return                            : Send a message il File.old was Updated or created or not modified 
    
    cat("-----------------------------------------------------------------------------------\n")
    
    # creating File.old if it does not exist
    if (is.null(File.old)) {
        
        File.old <- paste0(File, ".old")
        # cat(paste0("[Make.Old] INFO, the current ", File, " is being copied into ", File.old), sep = "\n")
        file.copy(from = File, to = File.old, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
        return(cat(paste0("[Make.Old] INFO, ", File.old, " was created."), sep = "\n"))
        
    } else {
        
        if (file.exists(File.old)) {
            
            #checking for newer dates
            if (file.info(File)$mtime > file.info(File.old)$mtime) {
                
                # New version of File, save File.Old 
                # cat(paste0("[Make.Old] INFO, the current ", Rdata.file, " is being copied into ", File.old), sep = "\n")
                file.copy(from = Rdata.file, to = Rdata.Old.file, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
                return(cat(paste0("[Make.Old] INFO, ", File.old, " was updated."), sep = "\n"))
                
                
            } else return(cat(paste0("[Make.Old] INFO, the current ", File.old, " is up to date. No need to make a a copy.", File.old), sep = "\n"))
            
        } else {
            
            # cat(paste0("[Make.Old] INFO, the current ", File, " is being copied into ", File.old), sep = "\n")
            file.copy(from = File, to = File.old, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
            return(cat(paste0("[Make.Old] INFO, ", File.old, " did not exist. It was created."), sep = "\n"))
            
        }
        
    }
    
    # if (file.exists(File.old)) return(cat(paste0("[Make.Old] INFO, ", File.old, " was created."), sep = "\n")) else return(cat(paste0("[Make.Old] INFO ", File.old, " could not be created."), sep = "\n"))
    cat("-----------------------------------------------------------------------------------\n")
} 

#=====================================================================================CR
# 161107 MG : functions for converting digital values 
#=====================================================================================CR
ASEDigi2Volt <- function(Sensors_Cal, Digital, ADC = 16, Volt.Convert = TRUE) {
    # Sensors_Cal : dataframe with the following column vectors: 
    #               name.sensor   : a vector of string with the names of sensors mounted on the AirSensEUR
    #               Ref           : a float vector, one value per sensor in Sensors_Cal$Sensors, giving the voltage in the middle of the analogue to digital converter (ADC) on the shield
    #               RefAD         : a float vector, one value per sensor in Sensors_Cal$Sensors, corresponding to the range of the ADC conversion (ref ? RefAD)
    #               Intercept     : a float vector, one value per sensor in Sensors_Cal$Sensors, intercept of conversion equation Conc[] = Intercept + Slope x Voltage
    #               Slope         : a float vector, one value per sensor in Sensors_Cal$Sensors, intercept of conversion equation Conc[] = Intercept + Slope x Voltage
    #               Sens.raw.unit : vector of strings, one value per sensor in Sensors_Cal$Sensors, with the Unit names after conversion of Digital values into concetration values
    # ADC         : number of bits of the Aanalogue to digital conversion
    # Digital     : a dataframe of numerics with the Digital values to be converted into Voltages or Currents
    # Volt.Convert: logical, default is TRUE. If TRUE, the data in Digital dataFrame need conversion from Digital to volts. If FALSE data are already in volts and conversion is not necessary.
    # return      : the function return a dataframe with the voltages or currents for all sensors
    
    # reorder Sensors_Cal as Digital for gas sensors - create empty data.table of results
    # The name of the sensors are given by the names of the column in Digital
    Sensors_Cal  <- Sensors_Cal[as.vector(sapply(gsub(pattern = "Out.", replacement = "", names(Digital)), function(i) grep(i, Sensors_Cal$gas.sensor))),]
    
    # Check which ones to converts in V and in nA
    # no need we convert all in volts, maybe this helps to get a dataframe only of numeric
    indexA <- which(Sensors_Cal$Sens.raw.unit == "nA")
    
    # converts in Volts
    if (Volt.Convert) {
        MyVectorMul  <- 2*Sensors_Cal$RefAD/(2^ADC)
        MyVectorAdd  <- Sensors_Cal$Ref - Sensors_Cal$RefAD
        Digital <- Digital[, Map(`+`, 1, .SD)][, Map("*", .SD, MyVectorMul)][, Map(`+`, MyVectorAdd, .SD)]}
    
    # converts in nA
    if (length(indexA) != 0) {
        MyVectornA  <- 10^9/(Sensors_Cal$GAIN[indexA] * Sensors_Cal$Rload[indexA])
        Digital <- Digital[, Map(`-`, .SD, Sensors_Cal$board.zero.set)][, Map("*", .SD, MyVectornA)]} 
    
    colnames(Digital) <- Sensors_Cal$name.sensor
    return(Digital)
}
ASEVolt2Conc <- function(Sensors_Cal, Voltage) {
    # Sensors_Cal: dataframe with the following column vectors: 
    #              Sensors   : a vector of string with the names of sensors mounted on the AirSensEUR
    #              Ref       : a float vector, one value per sensor in Sensors_Cal$Sensors, giving the voltage in the middle of the analogue to digital converter (ADC) on the shield
    #              RefAD     : a float vector, one value per sensor in Sensors_Cal$Sensors, corresponding to range of the ADC conversion (ref ? RefAD)
    #              Intercept : a float vector, one value per sensor in Sensors_Cal$Sensors, intercept of conversion equation Conc[] = Intercept + Slope x Voltage
    #              Slope     : a float vector, one value per sensor in Sensors_Cal$Sensors, intercept of conversion equation Conc[] = Intercept + Slope x Voltage
    #              Unit      : vector of strings, one value per sensor in Sensors_Cal$Sensors, with the Unit names after conversion of Digital values into concetration values
    # Voltage    : a dataframe with 4 columns representing the voltage values of the 4 sensors of an AirSensEUR, each columns are floats vector, to be corrected with Sensors_Cal$Sensors
    #             gives the Voltage values to be converted into concentration values
    # Currently a simple linear calibration is applied
    return(  t( (t(Voltage) -  Sensors_Cal$Intercept) / Sensors_Cal$Slope ))
}
Convert.ASE  <- function(Sensors_Cal, Digital) {
    # Sensors_Cal: dataframe with the following column vectors: 
    #              Sensors  : a vector of string with the names of sensors mounted on the AirSensEUR
    #              Ref      : a float vector, one value per sensor in Sensors_Cal$Sensors, with the voltage in the middle of the analogue to digital converter (ADC) on the shield
    #              RefAD    : a float vector, one value per sensor in Sensors_Cal$Sensors, corresponding to range of the ADC conversion (ref ? RefAD)
    #              Intercept: a float vector, one value per sensor in Sensors_Cal$Sensors, intercept of conversion equation Conc[] = Intercept + Slope x Voltage
    #              Slope    : a float vector, one value per sensor in Sensors_Cal$Sensors, intercept of conversion equation Conc[] = Intercept + Slope x Voltage
    #              Unit     : vector of strings, one value per sensor in Sensors_Cal$Sensors, with the Unit names after conversion of Digital values into concetration values
    # 
    # Digital    : vector of digital values to be converted to Volt and Concentration
    # return a dataframe with voltage and concentration values
    #
    Voltage <- ASEDigi2Volt(Sensors_Cal = Sensors_Cal, Digital = Digital)
    Conc    <- ASEVolt2Conc(Sensors_Cal = Sensors_Cal, Voltage = Voltage)
    
    return (data.frame(Voltage_v = Voltage
                       , Conc = Conc
                       , check.names = FALSE, stringsAsFactors = FALSE)  )
}

#=====================================================================================CR
# 161031 MG : Removing outliers using the Median Average Deviation method
#=====================================================================================CR
# REMOVING Ouliers - An upper threshold ("utmax" and "utmin) calculation based on the MAD:
# Can be replaced by mad(x, center = median(x), constant = threshold, na.rm = TRUE, high = TRUE) and 2nd , low = TRUE instead of , high = TRUE
#utmax <- function(x, threshold) {m = median(x, na.rm = TRUE); return(median(x, na.rm = TRUE) + threshold * median(abs(x - m), na.rm = TRUE))}
#utmin <- function(x, threshold) {m = median(x, na.rm = TRUE); return(median(x, na.rm = TRUE) - threshold * median(abs(x - m), na.rm = TRUE))}
utmax    <- function(x, threshold) {median(x, na.rm = TRUE) + mad(x, constant = threshold, na.rm = TRUE)}
utmin    <- function(x, threshold) {median(x, na.rm = TRUE) - mad(x, constant = threshold, na.rm = TRUE)}
utmaxmin <- function(x, threshold, Median = NULL) {
    Median <- median(x, na.rm = TRUE)
    MAD    <- mad(x, constant = threshold, na.rm = TRUE)
    return(c(Median + MAD, Median - MAD))
}
My.rm.Outliers <- function(date, y, ymin = NULL, ymax = NULL, ThresholdMin = NULL, window, threshold, plotting = TRUE, set.Outliers = TRUE, ind = NULL,
                           nTicksX = 10, nTicksY = 10, LasY = 3, Title = NULL, Ylab = "Raw Sensor values", Dygraphs = FALSE) {
    # date                  = x axis, date values Posixct
    # y                     = time series on which ouliers are detected
    # ymin, ymax            = minimum values for y, for example to remove negative values
    # ThresholdMin          = minimum values for zmin, the minimum values that evidence outliers when exceeded
    # window                = width of the window used to compute median and average
    # threshold             = coefficient that muliplied by the difference between median and average that is exceeded result in outlier
    # plotting              = logical, default TRUE, if TRUE the plot of outliers is performed
    # set.Outliers          = logical, default TRUE, if TRUE the the procedure to detect outliers is carried out
    # Ind                   = when not determining outliers (set.Outliers = F), this substitue the datframe  df
    # nTickX,Y                  : integer, number of spaces between ticks on x and y axis
    # LasY                      : integer, orientation of numbers on y axis> 0: always parallel to the axis [default], 1: always horizontal,
    #                           : 2: always perpendicular to the axis and 3: always vertical.
    # Title                     : Charater vector, Title to be plotted
    # Ylab                      : The label of the y axis, a character vector, the default is "Raw Sensor values"
    # Dygraphs              = logical, defaults is FALSE to plot a base plot. If TRUE use Dygraphs Dygraphs are plotted in UTC.
    
    # Return                = dataframe with Logical for low values, logical if value exceed lower MAD, logical if value exceed High MAD,
    #                         low MAD and high MAD
    
    # convert tible to data.frame then vector, this is necessary for rollapply
    if (class(y)[1] == "tbl_df")  y <- as.data.frame(y)[,1] 
    
    if (set.Outliers) {
        
        # Removing values lower than ymin
        if (!is.null(ymin)) Low_values  <- (y < ymin)
        if (!is.null(ymax)) High_values <- (y > ymax)
        
        # DT <- data.table::data.table(date = date, y = y, key = "date")
        # DT[,Median := roll_median(y, n = window, fill = NA, align = "center", na.rm = F)]
        # DT[,zmax.zmin := rollapply(DT[,2, with = F], width = window, FUN = utmaxmin, threshold = threshold, Median = Median, align = "center", partial = TRUE)]
        
        # max and max limits using package zoo: too slow
        # zmax.zmin <- zoo::rollapply(zoo(y), width = window, 
        #                             FUN = utmaxmin, threshold = threshold,
        #                             align = "center", partial = TRUE, fill = NA)
        Median <- caTools::runquantile(y,19,0.5, type =2)
        MAD    <- caTools::runmad(y, window, center = runquantile(y,19,0.5,type = 2), constant = threshold, endrule= "mad", align = "center") 
        zmax.zmin <- data.frame(zmax = Median + MAD, zmin = Median - MAD)
        
        #zmax <- c(rep(zmax[1], window-1), zmax) # Use z[1] throughout the initial period. # Not needed if align center and partial = TRUE
        OutliersMax <- y > zmax.zmin[,1]
        
        # Changing the low values of the minimum of the interval of tolerance by ThresholdMin
        Index.Lower <- which(zmax.zmin[,2] < ThresholdMin)
        if (!is.null(ThresholdMin) && !is.na(ThresholdMin)) {zmax.zmin[Index.Lower,2] <- rep(ThresholdMin, length.out = length(Index.Lower))}
        
        #zmin <- c(rep(zmin[1], window-1), zmin) # Use z[1] throughout the initial period. # Not needed if align center and partial = TRUE
        OutliersMin <- y < zmax.zmin[,2]
        
        # data frame to return
        df <- data.frame(date = date, 
                         Low_values  = Low_values, 
                         High_values = High_values, 
                         OutliersMin = OutliersMin, 
                         OutliersMax = OutliersMax, 
                         zmin        = zmax.zmin[,2], 
                         zmax        = zmax.zmin[,1]) 
    } 
    
    # Plotting the data, show the ut() cutoffs, and mark the outliers:
    if (plotting) {
        
        if (Dygraphs) {
            
            # convert tible to data.frame then vector, this is necessary for rollapply
            if (class(date)[1] == "tbl_df")  date <- as.data.frame(date)[,1] 
            
            Commom.dates <- which(ind$date %in% date)
            # creating dataframe with xts data series
            data <- data.frame(date        = date,
                               Sensor      = y,
                               OutliersMax = ind[Commom.dates,"OutliersMax"],
                               OutliersMin = ind[Commom.dates,"OutliersMin"],
                               Low_values  = ind[Commom.dates,"Low_values"],
                               High_values = ind[Commom.dates,"High_values"],
                               zmin        = ind[Commom.dates,"zmin"],
                               zmax        = ind[Commom.dates,"zmax"])
            
            # Set invalid data to Sensor values
            for (i in c("OutliersMax", "OutliersMin", "Low_values", "High_values")) {
                
                is.outliers <- which(data[,i])
                if (length(is.outliers) > 0) {
                    data[is.outliers, i]      <- data[is.outliers, "Sensor"]
                    #data[is.outliers, "Sensor"] <- NA
                } else data[, i] <- NA    
                if (length(is.outliers) > 0 && length(is.outliers) < nrow(data)) data[-is.outliers, i] <- NA
            }
            
            # Create dygraphs time_series
            #data <- data_frame_to_timeseries(data, tz = threadr::time_zone(General.df$date))
            TZ = threadr::time_zone(data$date[1])
            data <- cbind(xts(data$Sensor,        order.by = data$date, tzone = TZ), 
                          xts(data$OutliersMax,   order.by = data$date, tzone = TZ), 
                          xts(data$OutliersMin,   order.by = data$date, tzone = TZ), 
                          xts(data$Low_values,    order.by = data$date, tzone = TZ), 
                          xts(data$High_values,   order.by = data$date, tzone = TZ),
                          xts(data$zmin,          order.by = data$date, tzone = TZ), 
                          xts(data$zmax,          order.by = data$date, tzone = TZ))
            names(data) <- c("Sensor", "OutliersMax", "OutliersMin", "Low_values", "High_values", "zmin", "zmax")
            names.data  <- c("Sensor", "> max(CI)", "< min(CI)", "< Ymin", "> Ymax", "min(CI)", "max(CI)")
            
            # dygraphs
            Colors <- c("Red","orange","Blue","violet")
            plot_outli <- dygraph(data = data, ylab = Ylab, main = Title) %>%
                dySeries("Sensor"     , label = "Sensor value", color = "green",   drawPoints = TRUE, pointSize = 2) %>%
                dySeries("OutliersMax", label = names.data[2] , color = Colors[1], drawPoints = TRUE, pointSize = 3) %>%
                dySeries("OutliersMin", label = names.data[3] , color = Colors[2], drawPoints = TRUE, pointSize = 3) %>%
                dySeries("Low_values" , label = names.data[4] , color = Colors[3], drawPoints = TRUE, pointSize = 3) %>%
                dySeries("High_values", label = names.data[5] , color = Colors[4], drawPoints = TRUE, pointSize = 3) %>%
                dyOptions(drawPoints = TRUE, pointSize = 3) %>% 
                dySeries("zmax",        label = names.data[7] , color = "grey",    drawPoints = FALSE) %>%
                dySeries("zmin",        label = names.data[6] , color = "grey",    drawPoints = FALSE) %>%
                dyLegend(show = "always", hideOnMouseOut = FALSE, width = 800) %>% 
                dyRangeSelector() %>% 
                dyOptions(labelsUTC = T) # plot in UTC
            
        } else {
            
            # saving the original graphical parameters
            op <- par(no.readonly = TRUE)
            # Restoring graphical parameters on exit of function, even if an error occurs
            on.exit(par(op)) # it reset the par(mfrow) allways plotting on the upper left plot
            par(mar = c(2,2.7,1.7,0.2))
            par(mgp = c(1.5,0.3,0))
            
            if (!set.Outliers) if (!is.null(ind)) df <- ind else cat("[My.rm.Outliers] ERROR: index are not asked to be determined and index is set to null or not given")
            
            # Limit of the timeserie plot
            Xlim <- c(min(date, na.rm = TRUE), max(date, na.rm = TRUE))
            Ylim <- c(min(y, na.rm = TRUE), max(y, na.rm = TRUE))
            
            # Ticks of the time series
            dates  <- pretty(date, n = nTicksX)
            yticks <- pretty(y   , n = nTicksY)
            
            plot(x = date , y = y, 
                 type = "p", 
                 lwd = 2, col = "green4", 
                 xlim = Xlim, ylim = Ylim, 
                 ylab = Ylab, cex.lab = 1.2, xlab = "", 
                 cex = 0.2, xaxt = "n", yaxt = "n")
            lines(date, y, col = "green4") # "#E00000"
            if (abs(difftime(time1 = dates[length(dates)], 
                             time2  = dates[1], 
                             units  = "days")) <=  nTicksX ) { 
                
                axis.POSIXct(1, at = dates, las = 1, format = "%d-%b %H:%M")
            } else axis.POSIXct(1, at = dates, las = 1, format = "%d-%b")
            #Ylim <- format(seq(min(y, na.rm = TRUE), max(y, na.rm = TRUE), by = (max(y, na.rm = TRUE)-min(y, na.rm = TRUE))/10), scientific =FALSE, digits = 0)
            axis(2, at = yticks, srt = 45, las = LasY)
            abline(v = dates, h = yticks, col = "lightgray", lty = "dotted")
            lines(df[df$date %in% intersect(date, df$date), "date"], df[which(df$date %in% intersect(date, df$date)),"zmax"], col = "Gray")
            points(date[which(df$OutliersMax)] , y[which(df$OutliersMax)], pch = 19, col = "black")
            lines(df[df$date %in% intersect(date, df$date), "date"], df[which(df$date %in% intersect(date, df$date)),"zmin"], col = "Gray")
            points(date[which(df$OutliersMin)] , y[which(df$OutliersMin)], pch = 19, col = "orange")
            points(date[which(df$Low_values)]  , y[which(df$Low_values)] , pch = 19, col = "Blue")
            points(date[which(df$High_values)] , y[which(df$High_values)], pch = 19, col = "Violet")
            if (!is.null(Title)) title(Title, line = 0.5)
            
            # add the legend, 1st the lines then the symbols
            legend(x = "topleft", # places a legend at the appropriate place 
                   c("Raw values","Confi. interval CI"), # puts text in the legend
                   lty = c(1,1), # gives the legend appropriate symbols (lines)
                   lwd = c(2.5,2.5),
                   col = c("green4", "grey")
            ) 
            legend(x = "topright", # places a legend at the appropriate place 
                   c(paste0("> CI (n= "  ,length(which(df$OutliersMax)),")"), 
                     paste0("< CI (n= "  ,length(which(df$OutliersMin)),")"),
                     paste0("< Ymin (n= ",length(which(df$Low_values )),")"),
                     paste0("> Ymax (n= ",length(which(df$High_values)),")")
                   ), # puts text in the legend
                   col = c("black","orange","Blue","violet"),
                   pch = c(19,19,19,19)) 
        }
        
    } 
    
    if (set.Outliers) return(df) else if (Dygraphs) return(plot_outli)
}
RollCall <- function(date, Sensor, reference, window = 7, threshold, plotting = TRUE, set.Cal = TRUE, 
                     nTicksX = 10, nTicksY = 10, LasY = 3, Title = NULL, Ylab = "Raw Sensor values", Dygraphs = FALSE) {
    # date                  = x axis, date values Posixct
    # y                     = time series on which ouliers are detected
    # ymin, ymax            = minimum values for y, for example to remove negative values
    # ThresholdMin          = minimum values for zmin, the minimum values that evidence outliers when exceeded
    # window                = width of the window used to compute median and average
    # threshold             = coefficient that muliplied by the difference between median and average that is exceeded result in outlier
    # plotting              = logical, default TRUE, if TRUE the plot of outliers is performed
    # set.Outliers          = logical, default TRUE, if TRUE the the procedure to detect outliers is carried out
    # Ind                   = when not determining outliers (set.Outliers = F), this substitue the datframe  df
    # nTickX,Y                  : integer, number of spaces between ticks on x and y axis
    # LasY                      : integer, orientation of numbers on y axis> 0: always parallel to the axis [default], 1: always horizontal,
    #                           : 2: always perpendicular to the axis and 3: always vertical.
    # Title                     : Charater vector, Title to be plotted
    # Ylab                      : The label of the y axis, a character vector, the default is "Raw Sensor values"
    # Dygraphs              = logical, defaults is FALSE to plot a base plot. If TRUE use Dygraphs Dygraphs are plotted in UTC.
    
    # Return                = dataframe with Logical for low values, logical if value exceed lower MAD, logical if value exceed High MAD,
    #                         low MAD and high MAD
    
    # convert tible to data.frame then vector, this is necessary for rollapply
    if (class(y)[1] == "tbl_df")  y <- as.data.frame(y)[,1] 
    
    if (set.Outliers) {
        
        # Removing values lower than ymin
        if (!is.null(ymin)) Low_values  <- (y < ymin)
        if (!is.null(ymax)) High_values <- (y > ymax)
        
        # DT <- data.table::data.table(date = date, y = y, key = "date")
        # DT[,Median := roll_median(y, n = window, fill = NA, align = "center", na.rm = F)]
        # DT[,zmax.zmin := rollapply(DT[,2, with = F], width = window, FUN = utmaxmin, threshold = threshold, Median = Median, align = "center", partial = TRUE)]
        
        # max and max limits using package zoo: too slow
        # zmax.zmin <- zoo::rollapply(zoo(y), width = window, 
        #                             FUN = utmaxmin, threshold = threshold,
        #                             align = "center", partial = TRUE, fill = NA)
        Median <- caTools::runquantile(y,19,0.5, type =2)
        MAD    <- caTools::runmad(y, window, center = runquantile(y,19,0.5,type = 2), constant = threshold, endrule= "mad", align = "center") 
        zmax.zmin <- data.frame(zmax = Median + MAD, zmin = Median - MAD)
        
        #zmax <- c(rep(zmax[1], window-1), zmax) # Use z[1] throughout the initial period. # Not needed if align center and partial = TRUE
        OutliersMax <- y > zmax.zmin[,1]
        
        # Changing the low values of the minimum of the interval of tolerance by ThresholdMin
        Index.Lower <- which(zmax.zmin[,2] < ThresholdMin)
        if (!is.null(ThresholdMin) && !is.na(ThresholdMin)) {zmax.zmin[Index.Lower,2] <- rep(ThresholdMin, length.out = length(Index.Lower))}
        
        #zmin <- c(rep(zmin[1], window-1), zmin) # Use z[1] throughout the initial period. # Not needed if align center and partial = TRUE
        OutliersMin <- y < zmax.zmin[,2]
        
        # data frame to return
        df <- data.frame(date = date, 
                         Low_values  = Low_values, 
                         High_values = High_values, 
                         OutliersMin = OutliersMin, 
                         OutliersMax = OutliersMax, 
                         zmin        = zmax.zmin[,2], 
                         zmax        = zmax.zmin[,1]) 
    } 
    
    # Plotting the data, show the ut() cutoffs, and mark the outliers:
    if (plotting) {
        
        if (Dygraphs) {
            
            # convert tible to data.frame then vector, this is necessary for rollapply
            if (class(date)[1] == "tbl_df")  date <- as.data.frame(date)[,1] 
            
            Commom.dates <- which(ind$date %in% date)
            # creating dataframe with xts data series
            data <- data.frame(date        = date,
                               Sensor      = y,
                               OutliersMax = ind[Commom.dates,"OutliersMax"],
                               OutliersMin = ind[Commom.dates,"OutliersMin"],
                               Low_values  = ind[Commom.dates,"Low_values"],
                               High_values = ind[Commom.dates,"High_values"],
                               zmin        = ind[Commom.dates,"zmin"],
                               zmax        = ind[Commom.dates,"zmax"])
            
            # Set invalid data to Sensor values
            for (i in c("OutliersMax", "OutliersMin", "Low_values", "High_values")) {
                
                is.outliers <- which(data[,i])
                if (length(is.outliers) > 0) {
                    data[is.outliers, i]      <- data[is.outliers, "Sensor"]
                    #data[is.outliers, "Sensor"] <- NA
                } else data[, i] <- NA    
                if (length(is.outliers) > 0 && length(is.outliers) < nrow(data)) data[-is.outliers, i] <- NA
            }
            
            # Create dygraphs time_series
            #data <- data_frame_to_timeseries(data, tz = threadr::time_zone(General.df$date))
            TZ = threadr::time_zone(data$date[1])
            data <- cbind(xts(data$Sensor,        order.by = data$date, tzone = TZ), 
                          xts(data$OutliersMax,   order.by = data$date, tzone = TZ), 
                          xts(data$OutliersMin,   order.by = data$date, tzone = TZ), 
                          xts(data$Low_values,    order.by = data$date, tzone = TZ), 
                          xts(data$High_values,   order.by = data$date, tzone = TZ),
                          xts(data$zmin,          order.by = data$date, tzone = TZ), 
                          xts(data$zmax,          order.by = data$date, tzone = TZ))
            names(data) <- c("Sensor", "OutliersMax", "OutliersMin", "Low_values", "High_values", "zmin", "zmax")
            names.data  <- c("Sensor", "> max(CI)", "< min(CI)", "< Ymin", "> Ymax", "min(CI)", "max(CI)")
            
            # dygraphs
            Colors <- c("Red","orange","Blue","violet")
            plot_outli <- dygraph(data = data, ylab = Ylab, main = Title) %>%
                dySeries("Sensor"     , label = "Sensor value", color = "green",   drawPoints = TRUE, pointSize = 2) %>%
                dySeries("OutliersMax", label = names.data[2] , color = Colors[1], drawPoints = TRUE, pointSize = 3) %>%
                dySeries("OutliersMin", label = names.data[3] , color = Colors[2], drawPoints = TRUE, pointSize = 3) %>%
                dySeries("Low_values" , label = names.data[4] , color = Colors[3], drawPoints = TRUE, pointSize = 3) %>%
                dySeries("High_values", label = names.data[5] , color = Colors[4], drawPoints = TRUE, pointSize = 3) %>%
                dyOptions(drawPoints = TRUE, pointSize = 3) %>% 
                dySeries("zmax",        label = names.data[7] , color = "grey",    drawPoints = FALSE) %>%
                dySeries("zmin",        label = names.data[6] , color = "grey",    drawPoints = FALSE) %>%
                dyLegend(show = "always", hideOnMouseOut = FALSE, width = 800) %>% 
                dyRangeSelector() %>% 
                dyOptions(labelsUTC = T) # plot in UTC
            
        } else {
            
            # saving the original graphical parameters
            op <- par(no.readonly = TRUE)
            # Restoring graphical parameters on exit of function, even if an error occurs
            on.exit(par(op)) # it reset the par(mfrow) allways plotting on the upper left plot
            par(mar = c(2,2.7,1.7,0.2))
            par(mgp = c(1.5,0.3,0))
            
            if (!set.Outliers) if (!is.null(ind)) df <- ind else cat("[My.rm.Outliers] ERROR: index are not asked to be determined and index is set to null or not given")
            
            # Limit of the timeserie plot
            Xlim <- c(min(date, na.rm = TRUE), max(date, na.rm = TRUE))
            Ylim <- c(min(y, na.rm = TRUE), max(y, na.rm = TRUE))
            
            # Ticks of the time series
            dates  <- pretty(date, n = nTicksX)
            yticks <- pretty(y   , n = nTicksY)
            
            plot(x = date , y = y, 
                 type = "p", 
                 lwd = 2, col = "green4", 
                 xlim = Xlim, ylim = Ylim, 
                 ylab = Ylab, cex.lab = 1.2, xlab = "", 
                 cex = 0.2, xaxt = "n", yaxt = "n")
            lines(date, y, col = "green4") # "#E00000"
            if (abs(difftime(time1 = dates[length(dates)], 
                             time2  = dates[1], 
                             units  = "days")) <=  nTicksX ) { 
                
                axis.POSIXct(1, at = dates, las = 1, format = "%d-%b %H:%M")
            } else axis.POSIXct(1, at = dates, las = 1, format = "%d-%b")
            #Ylim <- format(seq(min(y, na.rm = TRUE), max(y, na.rm = TRUE), by = (max(y, na.rm = TRUE)-min(y, na.rm = TRUE))/10), scientific =FALSE, digits = 0)
            axis(2, at = yticks, srt = 45, las = LasY)
            abline(v = dates, h = yticks, col = "lightgray", lty = "dotted")
            lines(df[df$date %in% intersect(date, df$date), "date"], df[which(df$date %in% intersect(date, df$date)),"zmax"], col = "Gray")
            points(date[which(df$OutliersMax)] , y[which(df$OutliersMax)], pch = 19, col = "black")
            lines(df[df$date %in% intersect(date, df$date), "date"], df[which(df$date %in% intersect(date, df$date)),"zmin"], col = "Gray")
            points(date[which(df$OutliersMin)] , y[which(df$OutliersMin)], pch = 19, col = "orange")
            points(date[which(df$Low_values)]  , y[which(df$Low_values)] , pch = 19, col = "Blue")
            points(date[which(df$High_values)] , y[which(df$High_values)], pch = 19, col = "Violet")
            if (!is.null(Title)) title(Title, line = 0.5)
            
            # add the legend, 1st the lines then the symbols
            legend(x = "topleft", # places a legend at the appropriate place 
                   c("Raw values","Confi. interval CI"), # puts text in the legend
                   lty = c(1,1), # gives the legend appropriate symbols (lines)
                   lwd = c(2.5,2.5),
                   col = c("green4", "grey")
            ) 
            legend(x = "topright", # places a legend at the appropriate place 
                   c(paste0("> CI (n= "  ,length(which(df$OutliersMax)),")"), 
                     paste0("< CI (n= "  ,length(which(df$OutliersMin)),")"),
                     paste0("< Ymin (n= ",length(which(df$Low_values )),")"),
                     paste0("> Ymax (n= ",length(which(df$High_values)),")")
                   ), # puts text in the legend
                   col = c("black","orange","Blue","violet"),
                   pch = c(19,19,19,19)) 
        }
        
    } 
    
    if (set.Outliers) return(df) else if (Dygraphs) return(plot_outli)
}

#=====================================================================================CR
# 170609 MG : Plotting points and a subset of points
#=====================================================================================CR
GraphOut <- function(date , y, Col = "#E00000", Ylab = "Raw Sensor values", indfull, 
                     nTicksX = 10, nTicksY = 10, LasY = 3, Title = NULL, Dygraphs = FALSE)  {
    # This function plot the data, show the ut() cutoffs, and mark the outliers. 
    # date                      : the time series date, a vector of POSIXCt
    # y                         : the y values to be plotted, a numeric vector
    # Col                       : The color of the time series, the default color is "#E00000"
    # Ylab                      : The label of the y axis, a character vector, the default is "Raw Sensor values"
    # indfull                   : integer: the index of vector date, to be plotted as invalid points in red or a list of Posix with 4 elements 
    #                             (Tmin, Tmax, RH.min and Rh.max). Using the names of list "infull" in legend
    # nTickX,Y                  : integer, number of spaces between ticks on x and y axis, default 10
    # LasY                      : integer, orientation of numbers on y axis> 0: always parallel to the axis [default], 1: always horizontal,
    #                           : 2: always perpendicular to the axis and 3: always vertical.
    # Title                     : Charater vector, Title to be plotted
    # Dygraphs                  : logical, defaults is FALSE to plot a base plot. If TRUE use Dygraphs Dygraphs are plotted in UTC.
    
    # convert tible to data.frame then vector, this is necessary for rollapply     
    if (class(y)[1] == "tbl_df")  y <- as.data.frame(y)[,1] 
    
    if (Dygraphs) {
        if (class(indfull) == "integer") {
            
            # creating dataframe with xts data series
            data <- data.frame(date   = date,
                               Sensor = y,
                               Invalid   = NA)
            # Setting values of invalid and sensor
            if (length(indfull) > 0) {
                
                data$Invalid[indfull] <- data$Sensor[indfull]
                data$Sensor[indfull]  <- NA
            } 
            
            # data <- data_frame_to_timeseries(data, tz = threadr::time_zone(General.df$date))
            TZ <- threadr::time_zone(date[1])
            data <- cbind(xts(data$Sensor , order.by = data$date, tzone = TZ),
                          xts(data$Invalid, order.by = data$date, tzone = TZ))
            names(data) <- c("Sensor", "Invalid")
            
            # dygraphs
            plot_Warm <- dygraph(data = data, ylab = Ylab, main = Title) %>%
                dySeries("Sensor"  , label = "Sensor value"          , color = Col) %>%
                dySeries("Invalid" , label = "Invalid value"  , color = "red") %>%
                dyOptions(drawPoints = TRUE, pointSize = 2) %>% 
                dyLegend(show = "always", hideOnMouseOut = FALSE, width = 350) %>% 
                dyRangeSelector() %>% 
                dyOptions(labelsUTC = T) # plot in UTC
            
        } else if (class(indfull) == "list") {
            
            # creating dataframe with xts data series
            data <- data.frame(date   = date,
                               Sensor = y,
                               TempMini = NA,
                               TempMaxi = NA,
                               RHMini   = NA,
                               RHMaxi   = NA )
            
            
            # Set invalid data to Sensor values
            if (length(indfull[[1]]) > 0) data$TempMini[which(data$date %in% indfull[[1]])]   <- data$Sensor[which(data$date %in% indfull[[1]])]
            if (length(indfull[[2]]) > 0) data$TempMaxi[which(data$date %in% indfull[[2]])]   <- data$Sensor[which(data$date %in% indfull[[2]])]
            if (length(indfull[[3]]) > 0) data$RHMini[  which(data$date %in% indfull[[3]])]   <- data$Sensor[which(data$date %in% indfull[[3]])]
            if (length(indfull[[4]]) > 0) data$RHMini[  which(data$date %in% indfull[[4]])]   <- data$Sensor[which(data$date %in% indfull[[4]])]
            if (length(c(indfull[[1]], indfull[[2]], indfull[[3]], indfull[[4]])) > 0) data$Sensor[which(data$date %in% c(indfull[[1]], indfull[[2]], indfull[[3]], indfull[[4]]))] <- NA
            
            # Create dygraphs time_series
            #data <- data_frame_to_timeseries(data, tz = threadr::time_zone(General.df$date))
            TZ <- threadr::time_zone(data$date[1])
            data <- cbind(xts(data$Sensor,   order.by = data$date, tzone = TZ), 
                          xts(data$TempMini, order.by = data$date, tzone = TZ), 
                          xts(data$TempMaxi, order.by = data$date, tzone = TZ), 
                          xts(data$RHMini,   order.by = data$date, tzone = TZ), 
                          xts(data$RHMaxi,   order.by = data$date, tzone = TZ))
            names(data) <- c("Sensor", "TempMini", "TempMaxi", "RHMini", "RHMaxi")
            
            # dygraphs
            Colors <- c("blue", "red", "orange","darkgrey")
            plot_Warm <- dygraph(data = data, ylab = Ylab, main = Title) %>%
                dySeries("Sensor"   , label = "Sensor value"    , color = Col) %>%
                dySeries("TempMini" , label = names(indfull)[1] , color = Colors[1]) %>%
                dySeries("TempMaxi" , label = names(indfull)[2] , color = Colors[2]) %>%
                dySeries("RHMini"   , label = names(indfull)[3] , color = Colors[3]) %>%
                dySeries("RHMaxi"   , label = names(indfull)[4] , color = Colors[4]) %>%
                dyLegend(show = "always", hideOnMouseOut = FALSE, width = 350) %>% 
                dyOptions(drawPoints = TRUE, pointSize = 2) %>% 
                dyRangeSelector()
        }
    } else {
        
        # saving the original par values in case they would be modified in this function
        op <- par(no.readonly = TRUE)
        # Passing and resuming the par values
        on.exit(par(op)) # it reste par(mfrow) and plotting always on the 1st plot upper left
        par(mar = c(2,2.7,1.7,0.2))
        par(mgp = c(1.5,0.3,0))
        
        # Limit of the timeserie plot
        Xlim <- c(min(date, na.rm = TRUE), max(date, na.rm = TRUE))
        Ylim <- c(min(y, na.rm = TRUE)   , max(y, na.rm = TRUE))
        
        # Ticks of the time series
        dates  <- pretty(date, n = nTicksX)
        yticks <- pretty(y   , n = nTicksY)
        
        if (class(indfull) == "integer") {
            
            plot(date , y, type = "p", lwd = 2, col = Col, xlim = Xlim, ylim = Ylim, ylab = Ylab, cex.lab = 1.2, 
                 xlab = "", cex = 0.2, xaxt = "n", yaxt = "n")
            points(date[indfull] , y[indfull], pch = 19, col = "red")
            
            # add the legend, 1st the lines then the symbols
            legend(x = "topright", # places a legend at the appropriate place 
                   c(paste0("Invalid (n= "  ,length(indfull),")")
                   ), # puts text in the legend
                   col = "red",
                   pch = c(19)
            ) 
        } else {
            
            if (class(indfull) == "list") { #??? when influll is a list with T.min. T.max, RH.min and RH.max
                
                plot(x = date , y = y, type = "p", 
                     lwd = 2, col = Col, 
                     xlim = Xlim, ylim = Ylim, 
                     ylab = Ylab, cex.lab = 1.2, 
                     xlab = "", 
                     cex = 0.2, xaxt = "n", yaxt = "n")
                
                Col <- c("blue", "red", "orange","darkgrey")
                for (i in 1:length(indfull)) {
                    if (length(indfull[[i]][which(indfull[[i]] %in% date)]) > 0)  { # we could add  & all(class(indfull[[i]]) %in% c("POSIXct","POSIXt")) )
                        which.date <- indfull[[i]][which(indfull[[i]] %in% date)]
                        index.in <- match(which.date, date)
                        points(which.date , y[index.in], pch = 19, col = Col[i])
                    }
                }
                
                legend(x = "topright", # places a legend at the appropriate place 
                       c(paste0(names(indfull)[1], " (n= " ,length(indfull[[names(indfull)[1]]][which(indfull[[names(indfull)[1]]] %in% date)]),")"), 
                         paste0(names(indfull)[2], " (n= " ,length(indfull[[names(indfull)[2]]][which(indfull[[names(indfull)[2]]] %in% date)]),")"), 
                         paste0(names(indfull)[3], " (n= " ,length(indfull[[names(indfull)[3]]][which(indfull[[names(indfull)[3]]] %in% date)]),")"), 
                         paste0(names(indfull)[4], " (n= " ,length(indfull[[names(indfull)[4]]][which(indfull[[names(indfull)[4]]] %in% date)]),")") 
                       ), # puts text in the legend
                       col = col,
                       pch = c(19,19,19,19))
            } 
        }
        
        if (!is.null(Title)) title(Title, line = 0.5)
        
        if (abs(difftime(time1 = dates[length(dates)], 
                         time2 = dates[1], 
                         units = "days")) <=  nTicksX ) { 
            
            axis.POSIXct(1, at = dates, las = 1, format = "%d-%b %H:%M")
        } else axis.POSIXct(1, at = dates, las = 1, format = "%d-%b")
        
        axis(2, at = yticks, srt = 45, las= LasY)
        abline(v = dates, h= yticks, col = "lightgray", lty = "dotted")
        
        # add the legend, 1st the lines then the symbols
        legend(x = "topleft", # places a legend at the appropriate place 
               c("Raw values"), # puts text in the legend
               # lty = c(1), # gives the legend appropriate symbols (lines)
               # lwd=c(2.5),
               col = Col,
               pt.cex = 0.5,
               pch = c(19)
        ) 
    } 
    
    if (Dygraphs) return(plot_Warm)
} 

#=====================================================================================CR
# 161030 MG :  Download Sensor data from SOS and later from InfluxDB?
#=====================================================================================CR
Down_SOS <- function(AirsensEur.name, UserMins, DownloadSensor = NULL, AirsensWeb, Duration = NULL, DateEND = NULL, ref.tzone = "UTC") {
    
    # AirsensEur.name       = Name of for AirSensEUR for SOS download
    # UserMins              = periodicity of data requested for the returned dataframe
    # DownloadSensor        = a list with possible values
    #                         General.Rdata, the name of the data frame with downloaded data in directory WDinput
    #                         WDinput, the directory where the Rdata are saved
    #                         Retrieve.data.SOS, true if data need be retrieved
    #                         DateEND.SOS.prev, date to start download of SOS sensor data if InfluxData.Rdata already exist (may not exist)
    #                         The time zone is the one of SOS (GMT). 
    #                         Default value for DownloadSensor is NULL, nothing passed. In this case the Down_SOS
    #                         will create new Rdata/csv determining DateIN and DateEND using SOS information
    # AirSensWeb            = URI of the SOS server
    # Duration              = integer, the number of days to download per page (as Limit in SQL), default is NULL, data are downloaded in slices of 7 days
    # DateEND               = To be set if the whole function is rune over an internal from DownloadSensor$DateEND.SOS.prev until DateEND. DownloadSensor$DateEND.SOS.prev 
    #                         can be set manually before running Down_SOS.
    # Ref.tzone             = Time zone of the reference data, default is "UTC"                        
    # return                = dataframe InfluxData with the data to be added + 2 files are savedd SOSData.Rdata and SOSData.csv
    # dependences           = havingIP(), ping()
    
    #------------------------------------------------------------------------------CR
    # Sensor Data retrieving at apiEndpoint
    #------------------------------------------------------------------------------CR
    cat("\n")
    cat("-----------------------------------------------------------------------------------\n")
    cat(paste0("[Down_SOS] INFO, ", AirsensEur.name," sensor data retrieving"), sep = "\n")
    
    # Checking internet connection availability
    if (havingIP()) {
        URL <- unlist(strsplit(unlist(strsplit(gsub('http://', '', AirsensWeb), split = '/'))[1], split = ':'))[1]
        if (PingThisSite(URL)) {
            cat(paste0("[Down_SOS] INFO; ping to ", AirsensWeb, " Ok"), sep = "\n")
        } else{
            # return(cat(paste0("[Down_SOS] ERROR: you have an internet connection but cannot ping to ",AirsensWeb,". SOS download cannot be carried out."), sep = "\n"))
        } 
    } else {
        return(cat(paste0("[Down_SOS] ERROR: no internet connection. SOS download cannot be carried out."), sep = "\n"))
    }
    
    # connect
    apiEndpoint <- sensorweb4R::Endpoint(AirsensWeb)
    # number of category at the apiEndpoint
    cat(paste0("[Down_SOS] INFO, in total ", length(timeseries(apiEndpoint)), " Sensors at the SOS client."), sep = "\n")
    
    # Selecting service "AirSensEUR" with name 
    srv <- sensorweb4R::services(apiEndpoint)
    
    # get all phenomena
    phe <- sensorweb4R::phenomena(apiEndpoint)
    print(label(sensorweb4R::phenomena(apiEndpoint)), quote = FALSE)
    #cat("[Down_SOS] INFO, phenomena: ", paste0(label(phe), collapse = ","), sep = "\n")
    
    # get the station number corresponding to AirsensEur.name in label(stations(srv))
    if (AirsensEur.name %in% label(sensorweb4R::stations(srv))) {
        sta <- sensorweb4R::stations(srv)[match(x=AirsensEur.name, table=label(sensorweb4R::stations(srv)))] 
    } else {
        stop(cat(paste0("[Down_SOS] ERROR, ", AirsensEur.name, " is not found at the apiEndpoint. Correct the name of AirSensEUR or 
                        set Down.SOS to FALSE in the ASEconfig_xx.R file"), sep = "\n"))
    }
    
    # Select the timeseries of the station AirsensEur.name
    ts <- sensorweb4R::timeseries(sta)
    # The following is only for the JRC Ispra, mistakes with names
    if (AirsensEur.name == "JRC_C5_01") {
        ts <- ts[-grep(pattern = "http://www.airsenseur.org/ch1/o3_3e1f/11915854-335/1", x = label(ts))]
    }
    if (AirsensEur.name == "JRC_C5_05") {
        ts <- ts[-grep(pattern = "1_old", x = label(ts))]
    }
    
    cat(label(ts),sep = "\n")
    # fetch all the meta data of ts
    ts <- sensorweb4R::fetch(ts)
    # Position
    geom <- sp::geometry(sta)
    cat(paste0("[Down_SOS] INFO: Position of station ", AirsensEur.name, ":", head(geom@coords)),sep = "\n")
    
    # Phenomenon at the station
    Sensors <- data.frame(label(phenomenon(ts)), stringsAsFactors = FALSE)
    
    ### Trying to determine the name of variable using label(phenomenon(ts)), we will check if "Temperature", "Relative humidity" and "Atmospheric pressure" are in the label(phenomenon(ts))
    label.variable = any(grepl(pattern = "Temperature", x = Sensors[,1]), na.rm = FALSE) & 
        any(grepl(pattern = "Relative humidity", x = Sensors[,1]), na.rm = FALSE) & 
        any(grepl(pattern = "Atmospheric pressure", x = Sensors[,1]), na.rm = FALSE)
    if (label.variable) {
        
        # Removing Institute from the Category and replace blank spaces with _
        Sensors <- data.frame(apply(Sensors, 1, function(x) {x <- substr(x, start = 1, stop=unlist(gregexpr(pattern = " (", text = x, fixed= "TRUE")) - 1); return(x)}))
        Sensors <- data.frame(Pollutants = apply(Sensors, 1, function(x) {x <- sub(pattern = " ", replacement = "_",x); return(x)}), stringsAsFactors = FALSE)
        
    } else {
        
        # Defining names and variables for meteo and gas sensors - Used the same names of variables as in SOS for compatibility reasons
        # # meteo
        if (any(grepl(pattern = "ch6", x = Sensors[,1]))) Sensors$Pollutants[which(grepl(pattern = "ch6", x = Sensors[,1]))] <- "Relative_humidity" else cat(paste0("[ASEConfig] warning, ch6 (humidity) missing in the SOS service"), sep = "\n")
        if (any(grepl(pattern = "ch5", x = Sensors[,1]))) Sensors$Pollutants[which(grepl(pattern = "ch5", x = Sensors[,1]))] <- "Temperature" else cat(paste0("[ASEConfig] warning, ch5 (temperature) missing in the SOS service"), sep = "\n")
        if (any(grepl(pattern = "ch4", x = Sensors[,1]))) Sensors$Pollutants[which(grepl(pattern = "ch4", x = Sensors[,1]))] <- "Atmospheric_pressure" else cat(paste0("[ASEConfig] warning, ch4 (pressure) missing in the SOS service"), sep = "\n")
        # sensors
        #------------------------------------------------------------------------------CR
        # Adding sensor model type if more than 1 model of sensors then the model type are separated with "!" - The last sensor mdel type is used
        #------------------------------------------------------------------------------CR
        Sensor.names        <- list(Nitrogen_dioxide    = c("no2_b43f","NO2-B43F", "NO2_C_20", "NO2/C-20", "NO23E50"),
                                    Carbon_monoxide     = c("co_a4","CO-B4", "CO_MF_200", "CO_MF_20", "CO/MF-200", "CO3E300","co_mf_200"),
                                    Ozone               = c("o3_a431","O3_M-5","O3/M-5", "O3-B4", "O33EF1","o3_m_5","ox_a431"),
                                    Nitric_oxide        = c("no_b4","no-b4_op1","NO-B4", "NO_C_25", "NO/C-25","NO3E100")) # Add new sensor model to be recognized if needed
        # Setting gas names in Sensors$Pollutants
        for (i in 1:length(Sensor.names)) {
            for (j in 1:length(Sensor.names[[i]])) {
                if (any(grepl(pattern = Sensor.names[[i]][j], x = Sensors[,1]))) {
                    Sensors$Pollutants[which(grepl(pattern = Sensor.names[[i]][j], x = Sensors[,1]))] <- names(Sensor.names)[i]
                    break
                } 
            }
        }
        remove(Sensor.names)
        cat("[Down_SOS] INFO, sensors found in the SOS APi:\n")
        print(Sensors)
        
        
    }
    ##Sensors$ts <- as.numeric(ts)
    
    #------------------------------------------------------------------------------CR
    # Downloading sensor data
    #------------------------------------------------------------------------------CR
    # Saving files : names
    SOS.Rdata.file  = file.path(DownloadSensor$WDinput, "SOSData.Rdata")
    SOS.csv.file    = file.path(DownloadSensor$WDinput, "SOSData.csv"  )
    # Determining DateIn and DateEND for data download with a lubridate::interval
    if (is.null(DateEND)) DateEND <- max(time(lastValue(ts)))
    cat(paste0("[Down_SOS] info last date in AirSensEUR data to be downloaded is: ", DateEND), sep = "\n")
    # set DateIN for data retrieving, either from origin or last date in previous DataFrame
    if (is.null(DownloadSensor)) {
        if ("SOSData.Rdata" %in% list.files(DownloadSensor$WDinput)) { #  Rdata file exists : take the last date in dataframe General
            
            load(SOS.Rdata.file); DateIN <- max(SOSData$date, na.rm = TRUE)
            
        } else {
            
            # Rdata file dos not exist: take the value for SOS with minimum vaue at 2015-12-01
            DateIN  <- max(as.POSIXct(strptime("2015-12-01 00:00:00", format= "%Y-%m-%d %H:%M:%S", tz = ref.tzone))
                           , max(time(firstValue(ts))) ) # Tz is set to "UTC" to avoid conflict with Refdata which is in UTC although SOS used GMT
            
        }
    } else {
        
        # DownloadSensor exists: check if we have a "DateEND.SOS.prev"
        if (any(grepl(pattern = "DateEND.SOS.prev", x = objects(DownloadSensor)))) { # # DateEND.SOS.prev does not exist: take the value for SOS with minimum vaue at 2015-12-01
            
            if (!is.null(DownloadSensor$DateEND.SOS.prev)) {
                DateIN  <- DownloadSensor$DateEND.SOS.prev
            } else DateIN  <- max(as.POSIXct(strptime("2015-12-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = ref.tzone)), 
                                  max(time(firstValue(ts)), na.rm = TRUE) ) 
        } else DateIN  <- max(as.POSIXct(strptime("2015-12-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = ref.tzone)), max(time(firstValue(ts)), na.rm = TRUE) ) 
        
    }
    # Setting end date to curent date
    if (is.null(Duration)) Duration <- 7 # length of interval to download in days
    
    
    DateIN.partial  <- DateIN
    DateEND.partial <- DateIN + 3600 * 24 * Duration
    
    while (DateIN.partial < DateEND ) {
        
        date.partial <- lubridate::interval(DateIN.partial, DateEND.partial)
        # Downloading
        cat(paste0("[Down_SOS] INFO, downloading from ", DateIN.partial, " to ", DateEND.partial), sep = "\n")
        Buffer <- lapply(ts, function(x) {Buffer <- sensorweb4R::getData(x, timespan = date.partial);return(Buffer)})
        
        if (exists("Frame")) rm(Frame)
        for (i in 1:length(Sensors$Pollutant)) {
            #Buffer <- getData(ts[i], timespan=date.partial) # I suspect that if the timespan has no data, getData returns a error
            Buffer.df <- data.frame(Buffer[[i]][[1]])
            colnames(Buffer.df) <- c("date", Sensors$Pollutant[i])
            if (exists("Frame")) Frame <- merge(Frame,Buffer.df, by = "date", all = TRUE) else Frame <- Buffer.df
        }
        
        # Writing file if Frame is not empty of full of NA
        # Removing the NA from Frame that create error with timeAverage if there is only one line with NA
        if (nrow(Frame) == 1) Frame <- na.omit(Frame)
        if (nrow(Frame) > 1) {
            Frame <- timeAverage(Frame, 
                                 avg.time = paste0(toString(UserMins)," ","min"), 
                                 statistic = "mean", 
                                 start.date = round(DateIN.partial, units = "hours"), fill = TRUE)
            
            if (!"SOSData.Rdata" %in% list.files(DownloadSensor$WDinput)) {
                
                SOSData <- data.frame(Frame)
                
            } else {
                
                load(SOS.Rdata.file)
                SOSData <- rbind.fill(SOSData, data.frame(Frame)) # if merge, add , by = "date", all = TRUE
            }
            
            # convert SOSData$date to UTC to be consistent with reference data
            if (any(base::format(SOSData$date, format = "%Z") != "UTC")) attr(SOSData$date, "tzone") <- ref.tzone
            
            save(SOSData, file = SOS.Rdata.file)
            readr::write_csv(SOSData, path = SOS.csv.file, na = "NA", append = FALSE)
        }
        
        # Setting time interval to one duration more
        DateEND.partial <- DateEND.partial + 3600 * 24 * Duration
        DateIN.partial  <- DateIN.partial  + 3600 * 24 * Duration
        
    }
    
    cat("-----------------------------------------------------------------------------------\n")
    cat("\n")
    if (file.exists(SOS.Rdata.file)) load(SOS.Rdata.file)
    if ("SOSData" %in% ls()) return(SOSData) else return(cat("[Down_SOS] error no data to download"))
}

#=====================================================================================CR
# 161029 MG: Check General.R data file and the retrieve.data true or false
#=====================================================================================CR
Check_Download <- function(Influx.name = NULL, WDinput, UserMins, General.df = NA, RefData = NA, InfluxData = NA, SOSData = NA) {
    # Influx.name              = Name of for AirSensEUR in airsenseur.db, default Value NULL
    # WDinput                  = Sub directory of DisqueFieldtest where are the Refdata and InfluxData Rdata files
    # UserMins                 = periodicity of data requested after final data treatment
    # General.df               = Data.table or dataframe, default value is NA. General dataset
    # RefData                  = Data.table or dataframe, default value is NA. RefData dataset
    # InfluxData               = Data.table or dataframe, default value is NA. InfluxData dataset
    # SOSData                  = Data.table or dataframe, default value is NA. SOSData dataset
    # Return                   = a list with 
    #                              Ref.Rdata.file and Influx.Rdata.file, the name of the files with dataframe of reference and sensor downloaded data
    #                              WDinput, the directory where the Rdata are saved
    #                              Retrieve.data.Ref, true if reference data need be retrieved
    #                              Retrieve.data.Influx, true if sensor data need be retrieved (Influxdb)
    #                              Retrieve.data.SOS, true if sensor data need be retrieved (SOS)
    #                              Retrieve.data.General, true if sysdate is posterior to DateEND.General.prev
    #                              DateEND.Ref.prev, date to start download of reference data (last existing date), Null if "RefData.Rdata" does not exist
    #                              DateEND.Influx.prev, date to start download of sensor data (last existing date), Null if ""InfluxData.Rdata" does not exist
    #                              DateEND.SOS.prev, date to start download of sensor data (last existing date), Null if ""SOSData.Rdata" does not exist
    #                              DateEND.General.prev, last date  in General.Rdata, Null if ""General.Rdata" does not exist
    
    #------------------------------------------------------------------------------CR
    # Installing necessary packages
    #------------------------------------------------------------------------------CR
    # Both RSQLite and sqldf (and others too) are loaded by library(sqldf), so it is enough to instal sqldf
    list.Packages <- c("dplyr")
    Load.Packages(list.Packages)
    
    # Set the Rdata file of input data
    airsenseur.db.file  = file.path(WDinput, "airsenseur.db")
    Ref.file            = file.path(WDinput, "RefData.csv")
    Influx.file         = file.path(WDinput, "InfluxData.csv")
    SOS.file            = file.path(WDinput, "SOSData.csv")
    General.file        = file.path(WDinput, "General.csv")
    cat("-----------------------------------------------------------------------------------\n")
    cat(paste0("[Check_Download] INFO, Checking \n",
               airsenseur.db.file, "\n",
               General.file, "\n",
               Ref.file, "\n",
               SOS.file, " \n",
               Influx.file,"\n in ", WDinput, "\n"))
    
    # Checking if the directory exist
    if (!dir.exists(WDinput)) {
        
        cat(paste0("[Check_Download] INFO, Directory", WDinput, "does not exist. It is going to be created. All sensor and reference data are going to be downloaded."), sep = "\n")
        dir.create(WDinput, showWarnings = TRUE, recursive = FALSE, mode = "0777")
        
        # in this case downloading of all sensor and reference data is necessary
        Retrieve.data.Ref     = TRUE
        ExistFil.data.Ref     = FALSE
        DateIN.Ref.prev       = NULL
        DateEND.Ref.prev      = NULL
        
        Retrieve.data.Influx  = TRUE
        ExistFil.data.Influx  = FALSE
        DateIN.Influx.prev    = NULL
        DateEND.Influx.prev   = NULL
        
        Retrieve.data.SOS     = TRUE
        ExistFil.data.SOS     = FALSE
        DateIN.SOS.prev       = NULL
        DateEND.SOS.prev      = NULL
        
        Retrieve.data.General = TRUE
        ExistFil.data.General = FALSE
        DateIN.General.prev   = NULL
        DateEND.General.prev  = NULL
        
        Retrieve.data.db      = TRUE
        ExistFil.data.db      = FALSE
        DateIN.db.prev        = NULL
        DateEND.db.prev       = NULL
        
    } else {
        # The WDinput directory exists
        
        # checking if InfluxData exists
        if (!is.null(General.df) && !is.na(General.df)) {
            
            # General.file exists if data exist
            ExistFil.data.General = TRUE
            
            # General.df exists and is not NULL
            DateIN.General.prev  <- min(General.df$date, na.rm = TRUE)
            DateEND.General.prev <- max(General.df$date, na.rm = TRUE)
            
            # Checking if Download of General.df is necessary
            if (difftime(Sys.time(), max(General.df$date, na.rm = TRUE), units = "mins") > UserMins) {    ### MG , I doubt about the tz here, I think all is changed to UTM, as it is a difference maybe it does not matter
                
                Retrieve.data.General  = TRUE
                cat(paste0("[Check_Download] INFO, sensor data are going to be retrieved. Start date for data download: ", DateEND.General.prev), sep = "\n")
                
            } else {
                
                Retrieve.data.General  = FALSE
                cat(paste0("[Check_Download] INFO, no sensor data are going to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."), sep = "\n")
            }
        } else {
            
            # General.df is NULL
            if (!file.exists(General.file)) {
                
                # General.file does not exist
                ExistFil.data.General = FALSE
                
                Retrieve.data.General  = TRUE
                DateIN.General.prev    = NULL
                DateEND.General.prev   = NULL
                cat(paste0("[Check_Download] INFO, ", General.file, " does not exist. It is going to be created, sensor data will be retrieved."), sep = "\n")
                
            } else {
                
                # General.file exists
                ExistFil.data.General = TRUE
                
                cat(paste0("[Check_Download] INFO, ", General.file, " exists."), sep = "\n")
                if (extension(General.file) == ".csv") {
                    General.df <- fread(file = General.file, na.strings = c("","NA", "<NA>"), select = "date")
                } else if (extension(General.file) == ".Rdata") load(General.file)
                
                if (is.null(General.df)) {
                    
                    DateIN.General.prev    = NULL
                    DateEND.General.prev   = NULL
                    Retrieve.data.General  = TRUE
                    cat(paste0("[Check_Download] INFO, ", General.file, " is NULL (no values). It is going to be created, data, if any, will be retrieved."), sep = "\n")
                    
                } else {
                    
                    # General.df exists and is not NULL
                    DateIN.General.prev  <- min(General.df$date, na.rm = TRUE)
                    DateEND.General.prev <- max(General.df$date, na.rm = TRUE)
                    
                    
                    # Checking if Download of General.df is necessary
                    if (difftime(Sys.time(), max(General.df$date, na.rm = TRUE), units = "mins") > UserMins) {    ### MG , I doubt about the tz here, I think all is changed to UTM, as it is a difference maybe it does not matter
                        
                        Retrieve.data.General  = TRUE
                        cat(paste0("[Check_Download] INFO, sensor data are going to be retrieved. Start date for data download: ", DateEND.General.prev), sep = "\n")
                        
                    } else {
                        
                        Retrieve.data.General  = FALSE
                        cat(paste0("[Check_Download] INFO, no sensor data are going to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."), sep = "\n")
                    }
                }
            }
        } 
        
        # checking if RefData exists
        if (!is.null(RefData) && !is.na(RefData)) {
            
            # RefData is not NULL
            # Thus Ref.file exists as well
            ExistFil.data.Ref = TRUE
            
            # Not considering end rows with only NA values for sensors
            ind <- apply(RefData[, which(names(RefData) != "date"), with = FALSE], 1, function(x) !all(is.na(x)))
            DateIN.Ref.prev  <- min(RefData$date[ind], na.rm = TRUE)
            DateEND.Ref.prev <- max(RefData$date[ind], na.rm = TRUE)
            Var.Ref.prev     <- names(RefData)
            
            # Checking if Download of RefData is necessary
            if (difftime(Sys.time(), DateEND.Ref.prev, units = "mins") > UserMins) {    ### MG , I doubt about the tz here, I think all is changed to UTM, as it is a difference maybe it does not matter
                
                Retrieve.data.Ref = TRUE
                cat(paste0("[Check_Download] INFO, reference data are going to be retrieved. Start new reference data at : ", DateEND.Ref.prev), sep = "\n")
                
            } else {
                
                Retrieve.data.Ref = FALSE
                cat(paste0("[Check_Download] INFO, reference data are going to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."), sep = "\n")
                
            }
        } else {
            
            # RefData is NULL, Checking if RefData.file exists
            if (!file.exists(Ref.file)) { # Ref.file does not exist
                
                # RefData does not exist
                Retrieve.data.Ref = TRUE
                ExistFil.data.Ref = FALSE
                DateIN.Ref.prev   = NULL
                DateEND.Ref.prev  = NULL
                Var.Ref.prev      = NULL
                cat(paste0("[Check_Download] INFO, ", Ref.file, " does not exist. It is going to be created, data will be retrieved."), sep = "\n")
                
            } else { 
                
                # Ref.file exists
                ExistFil.data.Ref = TRUE
                
                cat(paste0("[Check_Download] INFO, ", Ref.file, " exists."), sep = "\n")
                if (extension(Ref.file) == ".csv") {
                    RefData <- fread(file = Ref.file, na.strings = c("","NA", "<NA>")) # , select = "date"
                } else if (extension(Ref.file) == ".Rdata") load(Ref.file)
                
                if (!is.null(RefData)) {
                    
                    # RefData exists and is not NULL
                    
                    # # Not considering end rows with only NA values for sensors
                    ind <- apply(RefData[, which(names(RefData) != "date"), with = FALSE], 1, function(x) !all(is.na(x)))
                    DateIN.Ref.prev  <- min(RefData$date[ind], na.rm = TRUE)
                    DateEND.Ref.prev <- max(RefData$date[ind], na.rm = TRUE)
                    Var.Ref.prev     <- names(RefData)
                    
                    # Checking if Download of RefData is necessary
                    if (difftime(Sys.time(), DateEND.Ref.prev, units = "mins") > UserMins) {    ### MG , I doubt about the tz here, I think all is changed to UTM, as it is a difference maybe it does not matter
                        
                        Retrieve.data.Ref = TRUE
                        cat(paste0("[Check_Download] INFO, reference data are going to be retrieved. Start new reference data at : ", DateEND.Ref.prev), sep = "\n")
                        
                    } else {
                        
                        Retrieve.data.Ref = FALSE
                        cat(paste0("[Check_Download] INFO, reference data are going to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."), sep = "\n")
                        
                    }
                } else {
                    
                    # RefData exists but it is NULL
                    
                    Retrieve.data.Ref <- TRUE
                    DateIN.Ref.prev   <- NULL
                    DateEND.Ref.prev  <- NULL
                    Var.Ref.prev      <- NULL
                    cat(paste0("[Check_Download] INFO, ", Ref.file, " is NULL (no values). It is going to be created, data will be retrieved."), sep = "\n")
                }
            } 
        }
        
        # checking if InfluxData exists
        if (!is.null(InfluxData) && !is.na(InfluxData)) {
            
            # Influx.file exists if data exist
            ExistFil.data.Influx  = TRUE
            
            # InfluxData exists and is not NULL
            DateIN.Influx.prev  <- min(InfluxData$date, na.rm = TRUE)
            DateEND.Influx.prev <- max(InfluxData$date, na.rm = TRUE)
            
            # Checking if Download of InfluxData is necessary
            if (difftime(Sys.time(), max(InfluxData$date, na.rm = TRUE), units = "mins") > UserMins) {    ### MG , I doubt about the tz here, I think all is changed to UTM, as it is a difference maybe it does not matter
                
                Retrieve.data.Influx  = TRUE
                cat(paste0("[Check_Download] INFO, sensor data are going to be retrieved. Start date for data download: ", DateEND.Influx.prev), sep = "\n")
                
            } else {
                
                Retrieve.data.Influx  = FALSE
                cat(paste0("[Check_Download] INFO, no sensor data are going to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."), sep = "\n")
                
            }
        } else {
            if (!file.exists(Influx.file)) {
                
                # InfluxData does not exist
                ExistFil.data.Influx  = FALSE
                
                Retrieve.data.Influx  = TRUE
                DateIN.Influx.prev    = NULL
                DateEND.Influx.prev   = NULL
                cat(paste0("[Check_Download] INFO, ", Influx.file, " does not exist. It is going to be created, sensor data will be retrieved."), sep = "\n")
                
            } else {
                
                # Influx.file exists
                ExistFil.data.Influx  = TRUE
                
                cat(paste0("[Check_Download] INFO, ", Influx.file, " exists."), sep = "\n")
                if (extension(Influx.file) == ".csv") {
                    InfluxData <- fread(file = Influx.file, na.strings = c("","NA", "<NA>"), select = "date")
                } else if (extension(Influx.file) == ".Rdata") load(Influx.file)
                
                if (!is.null(InfluxData)) {
                    
                    # InfluxData exists and is not NULL
                    DateIN.Influx.prev  <- min(InfluxData$date, na.rm = TRUE)
                    DateEND.Influx.prev <- max(InfluxData$date, na.rm = TRUE)
                    
                    # Checking if Download of InfluxData is necessary
                    if (difftime(Sys.time(), max(InfluxData$date, na.rm = TRUE), units = "mins") > UserMins) {    ### MG , I doubt about the tz here, I think all is changed to UTM, as it is a difference maybe it does not matter
                        
                        Retrieve.data.Influx  = TRUE
                        cat(paste0("[Check_Download] INFO, sensor data are going to be retrieved. Start date for data download: ", DateEND.Influx.prev), sep = "\n")
                        
                    } else {
                        
                        Retrieve.data.Influx  = FALSE
                        cat(paste0("[Check_Download] INFO, no sensor data are going to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."), sep = "\n")
                        
                    }
                } else {
                    
                    # InfluxData exists but it is NULL
                    
                    Retrieve.data.Ref   = TRUE
                    DateIN.Influx.prev  = NULL
                    DateEND.Influx.prev = NULL
                    cat(paste0("[Check_Download] INFO, ", Influx.file, " is NULL (no values). It is going to be created, data will be retrieved."), sep = "\n")
                }
            }
        }
        
        
        # checking if SOSData exists
        if (!is.null(SOSData) && !is.na(SOSData)) {
            
            # SOS.file exists if data exist
            ExistFil.data.SOS     = TRUE
            
            # SOSDataSOSData exists and is not NULL
            DateIN.SOS.prev      <- min(SOSData$date, na.rm = TRUE)
            DateEND.SOS.prev     <- max(SOSData$date, na.rm = TRUE)
            
            # Checking if Download of SOSData is necessary
            if (difftime(Sys.time(), max(SOSData$date, na.rm = TRUE), units = "mins") > UserMins) {    ### MG , I doubt about the tz here, I think all is changed to UTM, as it is a difference maybe it does not matter
                
                Retrieve.data.SOS   = TRUE
                cat(paste0("[Check_Download] INFO, SOS sensor data are going to be retrieved. Start date for data download: ", DateEND.Influx.prev), sep = "\n")
                
            } else {
                
                Retrieve.data.SOS   = FALSE
                cat(paste0("[Check_Download] INFO, no SOS sensor data are going to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."), sep = "\n")
                
            }
        } else {
            if (!file.exists(SOS.file)) {
                
                # SOS.file does not exist
                ExistFil.data.SOS     = FALSE
                
                Retrieve.data.SOS   = TRUE
                DateIN.SOS.prev     = NULL
                DateEND.SOS.prev    = NULL
                cat(paste0("[Check_Download] INFO, ", SOS.file, " does not exist. It should be created, SOS sensor data should be retrieved."), sep = "\n")
                
            } else {
                
                # SOS.file exists
                ExistFil.data.SOS     = TRUE
                
                cat(paste0("[Check_Download] INFO, ", SOS.file, " exists."), sep = "\n")
                if (extension(SOS.file) == ".csv") {
                    SOSData <- fread(file = SOS.file, na.strings = c("","NA", "<NA>"), select = "date")
                } else if (extension(SOS.file) == ".Rdata") load(SOS.file)
                load()
                
                if (!is.null(SOSData)) {
                    
                    # SOSDataSOSData exists and is not NULL
                    DateIN.SOS.prev      <- min(SOSData$date, na.rm = TRUE)
                    DateEND.SOS.prev     <- max(SOSData$date, na.rm = TRUE)
                    
                    # Checking if Download of SOSData is necessary
                    if (difftime(Sys.time(), max(SOSData$date, na.rm = TRUE), units = "mins") > UserMins) {    ### MG , I doubt about the tz here, I think all is changed to UTM, as it is a difference maybe it does not matter
                        
                        Retrieve.data.SOS   = TRUE
                        cat(paste0("[Check_Download] INFO, SOS sensor data are going to be retrieved. Start date for data download: ", DateEND.Influx.prev), sep = "\n")
                        
                    } else {
                        
                        Retrieve.data.SOS   = FALSE
                        cat(paste0("[Check_Download] INFO, no SOS sensor data are going to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."), sep = "\n")
                        
                    }
                } else {
                    
                    # SOSData exists but it is NULL
                    
                    Retrieve.data.SOS   = TRUE
                    DateIN.SOS.prev     = NULL
                    DateEND.SOS.prev    = NULL
                    cat(paste0("[Check_Download] INFO, ", SOS.file, " is NULL (no values). It is going to be created, data will be retrieved."), sep = "\n")
                }
            }
        }
        
        if (!file.exists(airsenseur.db.file)) {
            
            # airsenseur.db.file does not exist
            ExistFil.data.db      = FALSE
            
            Retrieve.data.db      = TRUE
            DateIN.db.prev        = NULL
            DateEND.db.prev       = NULL
            cat(paste0("[Check_Download] INFO, ", airsenseur.db.file, " does not exist. It should be created, SOS sensor data should be retrieved."), sep = "\n")
            
        } else {
            
            # airsenseur.db.file exists
            ExistFil.data.db      = TRUE
            
            cat(paste0("[Check_Download] INFO, ", airsenseur.db.file, " exists."), sep = "\n")
            # Checking table Dataset in airsenseur.db
            SQLite.con <- dbConnect(SQLite(), dbname = airsenseur.db.file)
            
            # Checking if the SQLite.con database and the table Dataset exists? 
            test_db <- src_sqlite(airsenseur.db.file)
            list    <- src_tbls(test_db)
            if (length(list) > 0) {
                
                cat(paste0("[Check_Download] INFO, The database ", airsenseur.db.file, " includes the table ", list[1]," with the columns: ", 
                           paste0(dbListFields(SQLite.con, list[1]), collapse = ", ") ), sep = "\n")
                
                # DataSet in airsenseur.db exists and is not NULL
                DateIN.db.prev      <- dbGetQuery(SQLite.con, paste0("SELECT min(time) FROM ", list[1]))[1,1]
                DateEND.db.prev     <- dbGetQuery(SQLite.con, paste0("SELECT max(time) FROM ", list[1]))[1,1]
                dbDisconnect(conn = SQLite.con)    
                
                # Checking if Download of InfluxData is necessary
                if (difftime(Sys.time(), DateEND.db.prev , units = "mins") > UserMins) {    ### MG , I doubt about the tz here, I think all is changed to UTM, as it is a difference maybe it does not matter
                    
                    Retrieve.data.db   = TRUE
                    cat(paste0("[Check_Download] INFO, SOS sensor data are going to be retrieved. Start date for data download: ", DateEND.Influx.prev), sep = "\n")
                    
                } else {
                    
                    Retrieve.data.db   = FALSE
                    cat(paste0("[Check_Download] INFO, no SOS sensor data are going to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."), sep = "\n")
                    
                }
            } else {
                
                # DataSet in airsenseur.db does not exist
                
                Retrieve.data.db      = TRUE
                DateIN.db.prev        = NULL
                DateEND.db.prev       = NULL
                cat(paste0("[Check_Download] INFO, ", airsenseur.db.file, " is NULL (no values). It is going to be created, data will be retrieved."), sep = "\n")
            }
        }
    }
    
    # Showing DownloadSens
    print(
        list(Ref.Rdata.file     = Ref.file, 
             Influx.Rdata.file  = Influx.file, 
             SOS.file     = SOS.file,
             General.Rdata.file = General.file,
             airsenseur.db.file = airsenseur.db.file,
             WDinput            = WDinput,
             ExistFil.data.db     = ExistFil.data.db     , Retrieve.data.db     = Retrieve.data.db     , DateIN.db.prev       = DateIN.db.prev,      DateEND.db.prev       = DateEND.db.prev,
             ExistFil.data.Ref    = ExistFil.data.Ref    , Retrieve.data.Ref    = Retrieve.data.Ref    , DateIN.Ref.prev      = DateIN.Ref.prev,     DateEND.Ref.prev      = DateEND.Ref.prev, Var.Ref.prev = Var.Ref.prev,
             ExistFil.data.Influx = ExistFil.data.Influx , Retrieve.data.Influx = Retrieve.data.Influx , DateIN.Influx.prev   = DateIN.Influx.prev,  DateEND.Influx.prev   = DateEND.Influx.prev,
             ExistFil.data.SOS    = ExistFil.data.SOS    , Retrieve.data.SOS    = Retrieve.data.SOS    , DateIN.SOS.prev      = DateIN.SOS.prev,     DateEND.SOS.prev      = DateEND.SOS.prev,
             ExistFil.data.General= ExistFil.data.General, Retrieve.data.General= Retrieve.data.General, DateIN.General.prev  = DateIN.General.prev, DateEND.General.prev  = DateEND.General.prev))
    
    cat("-----------------------------------------------------------------------------------\n")
    
    return(list(Ref.Rdata.file       = Ref.file, 
                Influx.Rdata.file    = Influx.file, 
                SOS.file       = SOS.file,
                General.Rdata.file   = General.file,
                airsenseur.db.file   = airsenseur.db.file,
                WDinput              = WDinput,
                ExistFil.data.db     = ExistFil.data.db     , Retrieve.data.db     = Retrieve.data.db     , DateIN.db.prev       = DateIN.db.prev,      DateEND.db.prev       = DateEND.db.prev,
                ExistFil.data.Ref    = ExistFil.data.Ref    , Retrieve.data.Ref    = Retrieve.data.Ref    , DateIN.Ref.prev      = DateIN.Ref.prev,     DateEND.Ref.prev      = DateEND.Ref.prev, Var.Ref.prev = Var.Ref.prev,
                ExistFil.data.Influx = ExistFil.data.Influx , Retrieve.data.Influx = Retrieve.data.Influx , DateIN.Influx.prev   = DateIN.Influx.prev,  DateEND.Influx.prev   = DateEND.Influx.prev,
                ExistFil.data.SOS    = ExistFil.data.SOS    , Retrieve.data.SOS    = Retrieve.data.SOS    , DateIN.SOS.prev      = DateIN.SOS.prev,     DateEND.SOS.prev      = DateEND.SOS.prev,
                ExistFil.data.General= ExistFil.data.General, Retrieve.data.General= Retrieve.data.General, DateIN.General.prev  = DateIN.General.prev, DateEND.General.prev  = DateEND.General.prev))
}

#=====================================================================================CR
# 161120 MG : Downloading AirSensEUR.db data using the Influx protocol, create or update the airsenseur.db SQLite database, get timezone
#=====================================================================================CR
Json_To_df <- function(JSON, Numeric = NULL, verbose = FALSE, Discard = NULL) {
    # JSON      : class "response" as returned by function httr::GET for INFLUX
    # Numeric   : charater vector with the colname of df to be converted into numeric
    # Discard   : column to drop from the returned dataframe
    # 
    # Returns a df a query data coverting from JSON of INFLUX with conversion of data columns from string to numeric
    
    if (JSON$status_code != 200) {
        Error.Message <- gsub(pattern = "%20", replacement = " ", JSON$url)
        Error.Message <- gsub(pattern = "%3D", replacement = "= ", Error.Message)
        Error.Message <- gsub(pattern = "%3B", replacement = ";", Error.Message)
        Error.Message <- gsub(pattern = "%22", replacement = "\"", Error.Message)
        Error.Message <- gsub(pattern = "%2C", replacement = "'", Error.Message)
        print(JSON, quote = FALSE)
        if (verbose) cat("Response to ",Error.Message,"\n")
        return(cat(paste0("[Json_To_df] ERROR, query returning error status code ",JSON$status_code, ". The query is wrong or the Influx server may be down.\n")))
    } else {
        JSON <- jsonlite::fromJSON(content(JSON, "text", encoding = "ISO-8859-1"), simplifyVector = TRUE, flatten = T)
        
        # Checking that JSON is not empty, e. g. there was no data for the Key or tag selected
        if (any(names(JSON) %in% "results")) {
            if (any(names(JSON$results) %in% "series")) {
                # delete "mean_" in case of query mean
                JSON.Colnames <- gsub(pattern = "mean_", replacement= "",JSON$results$series[[1]]$columns[[1]])
                if (verbose) cat(paste0("columns in JSON object: ",paste0(JSON.Colnames, collapse = ", "),"\n"))
                JSON <- setNames(data.frame(JSON$results$series[[1]]$values, 
                                            row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = TRUE, stringsAsFactors = FALSE), JSON.Colnames)
                if (any(Numeric %in% JSON.Colnames)) JSON[,Numeric[Numeric %in% JSON.Colnames]] <- sapply(JSON[,Numeric[Numeric %in% JSON.Colnames]], as.numeric)
                if (verbose) str(JSON)
            } else {
                # if (JSON$results[1,] == "data frame with 0 columns and 1 row")
                JSON <- "data frame with 0 columns and 1 row"
            }
        } else {
            JSON <- "No JSON data returned"
        }
        
        if (!is.null(Discard)) if (any(colnames(JSON) %in% Discard)) JSON <- JSON[,-which(colnames(JSON) %in% Discard)]
        return(JSON)
    }
}

Down_Influx <- function(PROXY = FALSE, URL = NULL, PORT = NULL, LOGIN = NULL, PASSWORD = NULL,
                        Host, Port = 8086, User, Pass, name.SQLite,name.SQLite.old,  Db, Dataset, Influx.TZ = NULL, 
                        Page = 200, Mean = 1, use_google = TRUE) {
    # Down_Influx downloads AirSensEUR.db data from an Influx server using JSON (package Jsonlite)
    
    # INPUT:
    # PROXY                           : Logical, default value FALSE, PROXY info necessary
    # URL                             : Character, default value NULL, url of your proxy, jrc = "10.168.209.72";
    # PORT                            : numeric, default value NULL, open Port for the proxy, jrc = 8012; 
    # LOGIN                           : character, default value = NULL, login for the proxy server, JRC = NULL;
    # PASSWORD                        : character, default value = NULL, password for the proxy server, jrc = NULL;
    
    # Parameters for the Influx download
    # Host                            : character, mandatory, url of the Influx server, jrc = 'influxdb1.liberaintentio.com', without "http://"
    # Port                            : numeric, default value = 8086, port used for the Influx transfer, the port ust be an open in your browser,
    # User                            : character, your login at the Influx server, jrc = "jrcuser",
    # Pass                            : character, your password at the Influx server, jrc = "idbairsenseur",
    # name.SQLite                     : character, path + name of the airsenseur.Db file, it shall be in the General.data directory
    # name.SQLite.old                 : character, backup of name.SQLite
    # Db                              : character, name of the database at the Influx server, jrc = "jrcispra",
    # Dataset                         : character, name of the table(Dataset) in the database Db that you want to download, e. g. "AirSensEUR05"
    # Influx.TZ                       : character, default value NULL. if NULL or "local time" the function will try to determine the time zone otherwise Influx.TZ will be used
    # use_google                      : logical: default = TRUE, if TRUE the google API is used for detecting time zone from coordinates (require port 443)
    # Page                            : numeric, default value 200 (LImit Influx_query), if Null the size of the page of data to download from the influx server is : LIMIT 10000, as requested in the Influx
    # Mean                            : numeric, default value 1 (Group by time(1m)), time average for the download of Influx data, 
    #                                   if Null the size of the page of data to download from the influx server is : LIMIT 10000, as requested in the Influx
    # 
    # Return                          : the time zone Influx.TZ, create the local database airsenseur.db in name.SQLite and return the time zone determined by find_tz(LastLong, Lastlat, use_google = TRUE)
    # Dependences                     : Ping(), Load.Packages()
    
    cat("\n")
    cat("-----------------------------------------------------------------------------------\n")
    #------------------------------------------------------------------------------CR
    # Checking internet connection availability
    #------------------------------------------------------------------------------CR
    if (havingIP()) {
        if (PingThisSite(gsub('http://', '', Host))) {
            cat(paste0("[Down_Influx] INFO; ping to ", Host, " Ok"), sep = "\n")
        } else{
            # return(cat(paste0("[Down_Influx] ERROR: you have internet connection but cant ping to ",Host,". InfluxDB download cannot be carried out."), sep = "\n"))
        } 
    } else {
        return(cat(paste0("[Down_Influx] ERROR: no internet connection. SOS download cannot be carried out."), sep = "\n"))
    }
    
    #------------------------------------------------------------------------------CR
    # Installing necessary packages
    #------------------------------------------------------------------------------CR
    # Both RSQLite and sqldf (and others too) are loaded by library(sqldf), so it is enough to instal sqldf
    list.Packages <- c("curl", "devtools", "sqldf", "zoo", "xts", "XML", "httr", "RJSONIO", "jsonlite", "lubridate")
    Load.Packages(list.Packages)
    
    #------------------------------------------------------------------------------CR
    # Downloading timezone from Github
    #------------------------------------------------------------------------------CR
    if (PROXY) {
        if (is.null(LOGIN)) set_config(use_proxy(url=URL, port=PORT)) else set_config( use_proxy(url=URL, port=PORT, username    = LOGIN, password = PASSWORD))
    } else reset_config()
    list.packages.github <- c("52North/sensorweb4R","rundel/timezone")
    for (i in list.packages.github) {
        
        # removing author name anad version number
        lib.i <- tail(unlist(strsplit(i, split = "/")), n = 1)
        lib.i <- head(unlist(strsplit(lib.i, split = "@")), n = 1)
        
        if (!(lib.i %in% rownames(installed.packages()))) {
            devtools::install_github(i)
            cat(sprintf("Package ", lib.i, " installed"), sep = "\n")
        } else cat(paste0("[Down_Influx] INFO, Package ", i, " already installed"), sep = "\n")
        
        do.call("library", as.list(lib.i))
        cat(sprintf("[Down_Influx] INFO, Package %s loaded",i), sep = "\n")
    }
    
    #------------------------------------------------------------------------------CR
    # create influx connection object and getting number of records
    #------------------------------------------------------------------------------CR
    Influx.con <- httr::GET(paste0("http://",Host,":",Port,"/ping"), 
                            config = authenticate(user = User, password = Pass, type = "basic"))
    if (Influx.con$status_code != 204) {
        cat("[Down_Influx] ERROR Influx server is down. Stopping the script.", "/n") 
        return(cat("[Down_Influx] ERROR Influx server is down. Stopping the script.\n"))
    } else cat("[Down_Influx] Influx server is up; connected to server\n")
    
    # total number of rows
    Influx.Total.N <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)), 
                                config = authenticate(user = User, password = Pass, type = "basic"),
                                query = list(q = paste0("SELECT count(latitude) FROM ", Dataset)))
    Influx.Total.N <- Json_To_df(Influx.Total.N, Numeric = "count")$count
    # Last GPS time (and the timestamp together)
    Influx.Last <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)), 
                             config = authenticate(user = User, password = Pass, type = "basic"),
                             query = list(q = paste0("SELECT  LAST(gpsTimestamp) FROM ", Dataset)))
    Influx.Last <- Json_To_df(Influx.Last, Numeric = "last")
    # Downloading Influxdb data in airsenseur.db:
    # if airsenseur.db does not exist       -->     create database : now we are in the 2nd case, airsenseur.db exists!
    # if airsenseur.db exists:   
    # if the table Dataset does not exist   -->     create the table and download all influx data
    # if the table Dataset  exists          -->     download only new data to the table dataset
    
    #------------------------------------------------------------------------------CR
    # AirsensEur.db exists? Make a copy if dates are different from Old db. Connect to the db
    #------------------------------------------------------------------------------CR
    if (file.exists(name.SQLite)) { # airsenseur.db exists
        
        cat(paste0("[Down_Influx] INFO, ", name.SQLite, " already exists."), sep = "\n")
        
        # making a copy of airsenseur.db as airsenseur_old.db if airsenseur_old.db is older than airsenseur.db
        if (file.exists(name.SQLite.old)) {
            
            if (file.info(name.SQLite)$size != file.info(name.SQLite.old)$size) {
                
                cat(paste0("[Down_Influx] INFO, the current airsenseur.db is being copied into airsenseur_old.db, this may take some time."), sep = "\n")
                file.copy(from = name.SQLite, to = name.SQLite.old, overwrite = TRUE, copy.mode = TRUE)
                
            } else cat(paste0("[Down_Influx] INFO, the current airsenseur_old.db has the same date (saved) as airsenseur.db, no need to backup "), sep = "\n")
            
        } else {
            
            cat(paste0("[Down_Influx] INFO, there is no backup of airsenseur.db. Copying to airsenseur_old.db"), sep = "\n")
            file.copy(from = name.SQLite, 
                      to = name.SQLite.old, 
                      overwrite = TRUE, 
                      copy.mode = TRUE) # copy.date = TRUE
        }
    } else {
        
        # airsenseur.db does not exist
        cat(paste0("[Down_Influx] INFO, ", name.SQLite, " does not exist and it is going to be created."), sep = "\n")
        
    }
    SQLite.con <- dbConnect(SQLite(), dbname = name.SQLite)
    #------------------------------------------------------------------------------CR
    # table dataset exists?
    #------------------------------------------------------------------------------CR
    if (dbExistsTable(conn = SQLite.con, name = Dataset)) { # the table Dataset exists in airsenseur.db
        
        cat(paste0("[Down_Influx] INFO, the table ", Dataset, " already exists in airsenseur.db"), sep = "\n")
        
        # Counting the number of records in AirSensEUR$Dataset - This will work provided that all rowid exist.
        #Dataset.N   <- dbGetQuery(SQLite.con, paste0("SELECT COUNT(gpsTimestamp) FROM ", Dataset))[1,1]
        Dataset.N               <- dbGetQuery(SQLite.con, paste0("SELECT max(rowid) FROM ", Dataset))[1,1]
        SQL.time.Last           <- dbGetQuery(SQLite.con, paste0("SELECT * FROM ", Dataset,"  order by rowid desc limit 1;"))$time
        SQL.gpsTimestamp.Last   <- dbGetQuery(SQLite.con, paste0("SELECT * FROM ", Dataset,"  order by rowid desc limit 1;"))$gpsTimestamp
        
        # Error Message and stop the script if there more data in the airsenseur.db than in InfluxDB
        if (difftime(Influx.Last$time, SQL.time.Last, units = "mins") < Mean) {
            cat("[Down_Influx] Warning,  Downloading is up to date. No need for data download.\n")
        } else cat(paste0("[Down_Influx] INFO, records between ",format(ymd_hms(SQL.time.Last),"%Y-%m-%d %H:%M")," and ",format(ymd_hms(Influx.Last$time),"%Y-%m-%d %H:%M"), " are going to be added into the table ", Dataset, " in the local airsenseur.db.\n"))
        Dataset.index   <- FALSE # if airsenseur.db exists then the indexes were already created, then no need to create the indexes
        
    } else {# the table Dataset does not exist in airsenseur.db
        
        # There are no records in AirSensEUR$Dataset
        cat(paste0("[Down_Influx] INFO, the table ", Dataset, " does not exist in airsenseur.db. It is going to be created"), sep = "\n")
        Dataset.N       <- 0
        
        # Get SQL.time.Last as First GPS time (and the timestamp together) of InfluxDB
        SQL.time.Last <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)), 
                                   config = authenticate(user = User, password = Pass, type = "basic"),
                                   query = list(q = paste0("SELECT  FIRST(sampleEvaluatedVal) FROM ", Dataset)))
        if (SQL.time.Last$status_code != 200) cat("[Down_Influx] ERROR query last GPSTime in airsenseur.db, Influx server may be down. Stopping the script.", "/n")
        SQL.time.Last <- Json_To_df(SQL.time.Last)$time
        
        Dataset.index   <- TRUE # if true indexes will be created
        
    } # the table Dataset exists in airsenseur.db
    
    #------------------------------------------------------------------------------CR
    # Downloading InfluxDB data and add them to the airsenseur.db
    #------------------------------------------------------------------------------CR
    # List of sensors to download
    # Sensors names, it seems that I cannot query directly name or channel (strings). Adding SELECT of a float field it works. Selecting the first 50 ones. Use SHOW TAG VALUES INSTEAD
    Influx.Sensor <-  httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)), 
                                config = authenticate(user = User, password = Pass, type = "basic"),
                                query = list(q = paste0("SHOW TAG VALUES FROM ", Dataset," WITH KEY IN ( \"name\") ; ")))
    Influx.Sensor <- Json_To_df(Influx.Sensor); 
    colnames(Influx.Sensor)[colnames(Influx.Sensor) == "value"] <- "name"; 
    
    # Adding channel for each Sensor names
    for (i in Influx.Sensor$name) {
        Influx.Channel.number <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)), 
                                           config = authenticate(user = User, password = Pass, type = "basic"),
                                           query = list(q = paste0("SELECT altitude, channel, \"name\" FROM ", Dataset," WHERE \"name\" = '",i,"' LIMIT 1;"))) 
        Influx.Channel.number <- Json_To_df(Influx.Channel.number)
        Influx.Sensor[which(Influx.Sensor$name == i),"channel"] <- Influx.Channel.number$channel
        Influx.Sensor[which(Influx.Sensor$name == i),"time"]   <- Influx.Channel.number$time
    }
    Influx.Sensor <- Influx.Sensor[order(Influx.Sensor$time),]
    print(Influx.Sensor, quote = FALSE)
    
    
    # Downloading always in increasing date, max download data in InfuxDB: chunks of 10000
    NbofDays.For10000data <- 10000/(24*60/Mean)
    # Number of seconds corresponding to NbofDays.For10000data
    Step <- NbofDays.For10000data * 24 * 60 * 60
    while (difftime(Influx.Last$time, SQL.time.Last, units = "mins") > Mean) {
        
        # Trying to query 5 times all numeric field keys
        for (j in 1:length(Influx.Sensor$name)) { # Downloadding sensor by sensor
            
            cat(paste0("[Down_Influx] INFO, downloading averages every ",Mean," min Influx data between ", format(ymd_hms(SQL.time.Last),"%Y-%m-%d %H:%M"),
                       " and ",format(ymd_hms(SQL.time.Last)+Step,"%Y-%m-%d %H:%M")," for sensor ",Influx.Sensor[j,"name"],"\n"))
            
            # Downloading from Inlfux server using httr
            Trial <- 1
            repeat{
                Mean.Query <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)), 
                                        config = authenticate(user = User, password = Pass, type = "basic"),
                                        query = list(q = paste0("SELECT mean(*) FROM ", Dataset," WHERE \"name\" = '",Influx.Sensor[j,"name"],
                                                                "'  AND time >= '",format(ymd_hms(SQL.time.Last),"%Y-%m-%d %H:%M:%S"),"' AND time < '",
                                                                format(ymd_hms(SQL.time.Last)+Step,"%Y-%m-%d %H:%M:%S"),"' GROUP BY time(",Mean,"m);"
                                                                # LIMIT " , format(round(1440/Mean), scientific = FALSE), " OFFSET ", format(0, scientific = FALSE)
                                                                # It is necessary to add :%S in the time tags
                                        ))) 
                if (Mean.Query$status_code == 200 || Trial > 5) break
                Trial <- Trial + 1
            }
            
            # Checking good query status code
            if (Mean.Query$status_code != 200) cat(paste0("[Down_Influx] ERROR, does not succed to query the influxDB with status_code <> 200. After 5 trials the script is stopped.\n")) else {
                
                # extracting lists from json
                if (length(colnames(Json_To_df(Mean.Query))) > 1 ) {
                    #Adding <- Json_To_df(Mean.Query, Numeric = c("altitude", "boardTimeStamp", "calibrated", "gpsTimestamp", "latitude", "longitude", "sampleEvaluatedVal", "sampleRawVal"))
                    # calibrated is not necessary, we will create a new table in influx in cloud
                    Adding <- Json_To_df(Mean.Query, 
                                         Numeric = c("altitude", "boardTimeStamp", "gpsTimestamp", "latitude", "longitude", "sampleEvaluatedVal", "sampleRawVal"),
                                         Discard = c("Calibrated","calibrated"))
                    
                    # Adding Channel and name
                    Adding$channel <- as.integer(Influx.Sensor[j,"channel"]); Adding$name <- Influx.Sensor[j,"name"]
                    
                } else next
                
            }
            
            # updating Adding
            if (exists("All.Sensors.Adding")) All.Sensors.Adding <- rbind.fill(All.Sensors.Adding,Adding) else All.Sensors.Adding <- Adding
            
        }
        
        # adding data to airSensEUR.db
        if (exists("All.Sensors.Adding")) {
            
            # discarding rows with all Na Values
            NA.values <- which(
                rowSums(
                    is.na(All.Sensors.Adding[,-which(names(All.Sensors.Adding) %in% c("time","channel","name"))])) ==
                    ncol(All.Sensors.Adding[,-which(names(All.Sensors.Adding) %in% c("time","channel","name"))]
                    )
            )
            if (length(NA.values) > 0) All.Sensors.Adding <- All.Sensors.Adding[-NA.values,]
            
            # add overwrite = TRUE to allow to recalculate the last 10 min values when some values are added
            RSQLite::dbWriteTable(conn = SQLite.con, name = Dataset, value = All.Sensors.Adding, append = TRUE) 
            cat(paste0("[Down_Influx] INFO, ", format(nrow(All.Sensors.Adding), scientific = FALSE), " records downloaded between ",
                       format(ymd_hms(SQL.time.Last),"%Y-%m-%d %H:%M")," and ",format(ymd_hms(SQL.time.Last)+Step,"%Y-%m-%d %H:%M")
                       ," added to table ", Dataset, " of airsenseur.db"), sep = "\n")
            remove(All.Sensors.Adding, Adding)
        } else cat(paste0("[Down_Influx] INFO, No influx data between ", format(ymd_hms(SQL.time.Last),"%Y-%m-%d %H:%M")," and ",format(ymd_hms(SQL.time.Last) + Step,"%Y-%m-%d %H:%M"),".\n"))
        
        # updating SQL.time.Last for while loop
        SQL.time.Last  <- ymd_hms(SQL.time.Last) + Step
        
    } 
    
    cat(paste0("[Down_Influx] INFO, the downloading of sensor data from the Influx server is finished.\n"))
    # I need to add index ?!?
    
    # Counting the number of records in AirSensEUR$Dataset
    Dataset.N   <- dbGetQuery(SQLite.con, paste0("SELECT max(rowid) FROM ", Dataset))[1,1]
    
    # getting the time zone, port 443 of the Browser shall be opened
    if (is.null(Influx.TZ) || Influx.TZ == "Local time") { 
        
        cat(paste0("[Down_influx] INFO, determining the time zone with the last valid latitude and longitude of ", Dataset, " in airsenseur.db"), sep = "\n")
        
        Offset <- Dataset.N
        repeat {
            
            Coord.lat.long   <- dbGetQuery(SQLite.con, paste0("SELECT time, longitude, latitude FROM ", Dataset, " WHERE rowid > ", Offset - 500, " AND rowid <= ", Offset, " ;"))
            
            if (all(is.na.data.frame(Coord.lat.long[,c("longitude","latitude")])) || 
                all(Coord.lat.long[!is.na.data.frame(Coord.lat.long[,c("longitude")]),c("longitude","latitude")] == 0)) {
                
                if (Offset > 500) {
                    Offset <- Offset - 500   
                } else {
                    cat(paste0("[Down_influx] INFO, impossible to determine the time zone of the sensor data. TZ is kept as ", Influx.TZ), sep = "\n")
                    break  
                }   
            } else {
                
                Lastlat     <- tail(na.omit(Coord.lat.long$latitude[Coord.lat.long$latitude != 0]), n = 1)
                LastLong    <- tail(na.omit(Coord.lat.long$longitude[Coord.lat.long$longitude != 0]), n = 1) 
                Influx.TZ <- find_tz(LastLong, Lastlat, use_google = use_google)
                cat(paste0("[Down_influx] INFO, the time zone of the sensor data is ", Influx.TZ), sep = "\n")
                
                break
            }
            
        }
        
    } 
    
    # # getting the last date, latitude and longitude in name.SQLite.
    # Last date to add only new data, latitude and longitude to get the time zone 
    # checking for non zero values and no NA()
    LastDate <- dbGetQuery(SQLite.con, paste0("SELECT time FROM ", Dataset," ORDER BY rowid DESC LIMIT 1;"))$time
    # InfluxDB gives everything in UTC not in local time zone - Well by observation in grafana it seems that the dates are in Local Time
    if (is.null(Influx.TZ) || Influx.TZ == "Local time") {
        LastDate <- ymd_hms(LastDate, tz = "UTC")
    } else {
        LastDate <- ymd_hms(LastDate, tz = Influx.TZ)
    } 
    
    # Creating index to speed up select in function SQLite2df, it seems that this is not used anymore so it is commented
    # if (Dataset.index) {
    #     dbGetQuery(SQLite.con, paste0("CREATE INDEX IDtime ON "   , Dataset, " (time);"))
    #     dbGetQuery(SQLite.con, paste0("CREATE INDEX IDchanne ON " , Dataset, " (channel);"))
    #     dbGetQuery(SQLite.con, paste0("CREATE INDEX IDname ON "   , Dataset, " (name);"))
    # } 
    
    # Disconnect SQLite.con
    dbDisconnect(conn = SQLite.con)
    cat(paste0("[Down_Influx] INFO, the airsenseur.db goes until ", format(LastDate, "%Y-%m-%d %H:%M"), ", with ", Dataset.N, " records for the table ", Dataset), sep = "\n")
    cat("-----------------------------------------------------------------------------------\n")
    cat("\n")
    return(Influx.TZ)
}

#=====================================================================================CR
# 161123 MG : Sqlite2df                 converting a local airsenseur.db into a General dataframe
#=====================================================================================CR
Sqlite2df <- function(name.SQLite, Dataset, Influx.TZ, UserMins = NULL, DownloadSensor = NULL, Page = NULL, Complete = FALSE, asc.File=NULL, InfluxData = NULL) {
    # Sqlite2df transforms an airsenseur.db table into a General data frame. airsenseur.db shall be created previously with Down_Influx 
    # Return            : A Values_db (existing data added if any) dataframe with date (as.POSIXct) to be used by openair, coordinates
    #                     , 7 sensor values as downloaded from Influx. Data are averaged with UserMins averaging time if Averaging is TRUE
    
    # Inputs:
    # name.SQLite       : character, path of the airsenseur.Db file, it shall be in the General.data directory
    # Dataset           : character, name of the table (Dataset) in the database Db that you want to download, e. g. "AirSensEUR05"
    # Influx.TZ         : character, the time zone for variable time in Dataset of the InfluxDB
    # UserMins          : numeric, default is NULL, if UserMins is not NULL aveaging of sensor dated with UserMins averaging time is performed, the periodicity of data requested for the returned dataframe,
    # DownloadSensor    : a list with 
    #                     character, Influx.Rdata.file, the path.file/name of an existing InfluxData.Rdata file
    #                     chrater WDinput, the directory where to save Rdata and csv files
    #                     logical Retrieve.data.Influx, wether it is necessary to retrive sensor data (not used)
    #                     character DateEND.Influx.prev, last date in Influx.Rdata.file
    #                     The time zone is the one of InfluxDB and SOS (GMT). 
    #                     Default value for DownloadSensor$Influx.Rdata.file is NULL, nothing passed. In this case, SQLite2df
    #                     creates new Rdata/csv
    # Page              : numeric, default value NULL, if Null the size of the page of data to download from the influx server is LIMIT to 200000
    # complete          : Logical, default is FALSE, If TRUE the Sqlite2df function will return a dataFrame concatenating the existing data in name.Sqlite with the new ones in Values_db
    # asc.File          : dataframe, default is NULL, used for giveing the correct name of the sensor
    # InfluxData        : data.table or dataframe, default is null. DataSet of sensor values
    
    # Dependence        : Load.Packages
    ### Still need adding when the AirSensEUR is switched on and off, when the name of sensors are changed and when it is at the Reference Stations
    
    cat("\n")
    cat("-----------------------------------------------------------------------------------\n")
    #------------------------------------------------------------------------------CR
    # Installing necessary packages
    #------------------------------------------------------------------------------CR
    # Both RSQLite and sqldf (and others too) are loaded by library(sqldf), so it is enough to instal sqldf
    list.Packages <- c("sqldf", "openair", "reshape","tidyverse", "data.table")
    Load.Packages(list.Packages)
    
    #------------------------------------------------------------------------------CR
    # AirsensEur.db exists? creating the db or just the connect to the db
    #------------------------------------------------------------------------------CR
    if (file.exists(name.SQLite)) # airsenseur.db exists
        cat(paste0("[Sqlite2df] INFO, ", name.SQLite, " exists."), sep = "\n") else  # airsenseur.db does not exist
            stop(cat(paste0("[Sqlite2df] INFO, ", name.SQLite, " does not exist. The script is stopped."), sep = "\n"))
    
    #------------------------------------------------------------------------------CR
    # Checking table Dataset in airsenseur.db
    #------------------------------------------------------------------------------CR
    SQLite.con <- dbConnect(SQLite(), dbname = name.SQLite)
    
    # Checking if the SQLite.con database and the table Dataset exists? 
    if (dbExistsTable(SQLite.con, Dataset)) {
        
        cat(paste0("[Sqlite2df] INFO, The database ", name.SQLite, " includes the table ", Dataset," with the columns: ", 
                   paste0(dbListFields(SQLite.con, Dataset), collapse = ", ") ), sep = "\n")
        
    } else stop(cat(paste0("[Sqlite2df] ERROR There is no table called ", Dataset, " in ", name.SQLite, ". The scipt is stoped."), sep = "\n"))
    
    #------------------------------------------------------------------------------CR
    # Reading local airsenseur.db in slice of Page records - from last data of InfluxData in DownloadSensor to only add the necessary data
    #------------------------------------------------------------------------------CR
    cat(paste0("[SQLite2df] INFO, reading table ", Dataset), sep = "\n")
    # Initial values
    Download.N  <- 0
    SQL.Total.N <- dbGetQuery(SQLite.con, paste0("SELECT max(rowid) FROM ", Dataset))[1,1]
    if (!is.null(DownloadSensor$DateEND.Influx.prev)) { # the table Dataset exists in airsenseur.db
        
        cat(paste0("[SQLite2df] INFO, InfluxData already exists."), sep = "\n")
        
        # Counting the row number where to add records in AirSensEUR$Dataset
        cat(paste0("[SQLite2df] INFO, looking for the first data in airsenseur.db to add to InfluxData.Rdata. This can be very long with large datasets ... TZ must be set"), sep = "\n")
        if (!exists("Influx.TZ")) stop(cat("[SQLite2df] ERROR: you must set the parameter TZ in the ASEConfig_xx.R file. The script is stopped."))
        
        # changing FirstDate timezone from the value in DownloadSensor to the local timezone called Influx.TZ
        FirstDate <- as.POSIXct(DownloadSensor$DateEND.Influx.prev, tz = "UTC", usetz = TRUE) # attr(FirstDate, "tzone") <- "UTC"
        FirstDate <- format(FirstDate, tz = Influx.TZ, usetz = TRUE)
        Dataset.N <- dbGetQuery(SQLite.con, paste0("SELECT min(rowid) FROM ", Dataset, " WHERE datetime(time) >= '", # >= instead of > to recalculate the last average
                                                   FirstDate,"';"))[1,1]
        #Dataset.N   <- dbGetQuery(SQLite.con, paste0("SELECT rowid FROM ", Dataset, " WHERE datetime(time) > '2016-11-14 17:18:00';"))
        if (is.na(Dataset.N)) return(cat("[SQLite2df] ERROR, there are no new data in airSenseur.db to add to InfluxData.Rdata and InfluxData.csv. The script is stopped"))
        
    } else {# the table Dataset exists in airsenseur.db
        
        # There are no records in AirSensEUR$Dataset
        cat(paste0("[SQLite2df] INFO, files InfluxData.Rdata and InfluxData.csv do not exist. Reading all data of airsenseur.db."), sep = "\n")
        Dataset.N  <- 0
        
    } 
    #dbGetQuery(SQLite.con, paste0("CREATE INDEX IDtime ON ",Dataset, "(time);"))
    
    # getting the default Page of data to download
    if (is.null(Page)) Page <- 200000
    
    while (Download.N < (SQL.Total.N - Dataset.N)) {
        
        # For the last Page to read used modulo instead of Page in order not to download twice the same records
        if ((Download.N + Page) > (SQL.Total.N - Dataset.N)) {Old_Page <- Page ; Page <- (SQL.Total.N - Dataset.N) %% Page } 
        
        # Downloading a slice Page
        Adding  <- dbGetQuery(SQLite.con, paste0("SELECT time, boardTimeStamp, gpsTimestamp, 
                                                 channel, name, 
                                                 cast(altitude AS DECIMAL) AS altitude, 
                                                 cast(latitude AS DECIMAL) AS latitude, 
                                                 cast(longitude AS DECIMAL) AS longitude, 
                                                 sampleEvaluatedVal
                                                 FROM ", Dataset, " LIMIT ", format(Page, scientific = FALSE), 
                                                 " OFFSET ", format(Dataset.N + Download.N, scientific = FALSE)
        ))
        
        # adding data to Values_db
        cat(paste0("[SQLite2df] INFO, ", format(Download.N + nrow(Adding), scientific = FALSE), "/",(SQL.Total.N - Dataset.N)," records were read "), sep = "\n")
        if (exists("Values_db")) Values_db <- rbindlist(list(Values_db,Adding)) else Values_db <- Adding
        # updating counter
        Download.N  <- Download.N + Page
        
    }
    # resuming Page for tabulating values
    if (exists("Old_Page")) Page <- Old_Page
    # dbReadTable(SQLite.con, Dataset) # dbReadTable loses the decimal of coordinates
    cat(paste0("[SQLite2df] INFO, Disconnecting ", name.SQLite), sep = "\n")
    cat(paste0("[SQLite2df] INFO, The table ", Dataset, " has ", nrow(Values_db), " new records and ", length(unique(Values_db$time)), " new unique TimeStamps"), sep = "\n")
    # Closing connection
    dbDisconnect(SQLite.con)
    rm(Adding, Download.N, SQL.Total.N)
    
    #------------------------------------------------------------------------------CR
    # Defining names and variables for meteo
    #------------------------------------------------------------------------------CR
    Meteo.names.change  <- data.frame(Influx.names  = c(          "SHT31HE",             "Humid",     "SHT31TE", "Tempe"      ,"Temp"        ,                "Press", "BMP280"              , "SHT31TI", "SHT31HI", "L2942VOL"), 
                                      General.names = c("Relative_humidity", "Relative_humidity", "Temperature", "Temperature", "Temperature", "Atmospheric_pressure", "Atmospheric_pressure", "IntTemp", "IntHum" , "Voltage"), stringsAsFactors = FALSE)
    #------------------------------------------------------------------------------CR
    # Adding final name (Temperature, Relative ...) 
    #------------------------------------------------------------------------------CR
    Channel.names <- unique(Values_db[,c("channel","name")])
    for (i in Meteo.names.change$Influx.names) {
        
        if (i %in% Channel.names$name) {
            
            # setting column variables in Channel.names with names of Meteo.names.change
            Channel.names[Channel.names$name == i,"Variables"] <- Meteo.names.change[Meteo.names.change$Influx.names == i,"General.names"]  
            
            # setting correct colnames in values_db for Meteo.names.change if chang are requested
            Sensor.rows <- which(Values_db$name == i)
            Values_db[Sensor.rows,"Pollutants"]   <- Meteo.names.change[Meteo.names.change$Influx.names == i,"General.names"] 
        
        }
    } 
    
    cat(paste0("[SQLite2df] INFO, looking for the names of all sensors and channel numbers from a list of sensor names, using best guess"), sep = "\n")
    if (is.null(asc.File)) {
        
        # In case of several sensor name on the same channel number
        for (i in Channel.names$channel) {
            
            # Taking the last name if sensors have been replaced
            if (anyDuplicated.array(Channel.names[Channel.names$channel == i - 1,c("channel","name")])) {
                Channel.names$name[i + 1] <- paste0(unique(Values_db[Values_db$channel == i, "name"]),collapse = "!")
                if (grepl(pattern = "!", x = Channel.names$Sensor.names[i + 1])) {
                    
                    Channel.names$Sensor.names[i+1] <- tail(unlist(strsplit(Channel.names$name[i+1], split = "!")), n = 1)
                    cat(paste0("[SLite2df] WARNING the name of the sensor of channel ", i, " has been changed. The script is assuming that the sensor model type was not changed during use and it is  ", Channel.names$Sensor.names[i+1], ", the last one."), sep = "\n")
                    
                }
            }
        }
    } else { 
        
        # Set Channel.names$name, Values_db$name and Values_db$Pollutants based on the shield config file of the chemical sensor board. Sensor shall be in channel 1, 2, 3 and 4!!!
        for (i in which(!is.na(asc.File$name.sensor))) { 
            
            # Setting name of sensor from file base on channel number - 1
            cat(paste0("[SLite2df] INFO setting name.sensor and gas.sensor using the shield config file for sensor ", i), sep = "\n")
            Sensor.rows                             <- which(Values_db$channel==(i-1))
            if (length(Sensor.rows) > 0) {
                
                Values_db[Sensor.rows,"name"]       <- asc.File$name.sensor[i]
                Values_db[Sensor.rows,"Pollutants"] <- asc.File$gas.sensor[i]
            }
            
            # updating the model of sensor in df Channel.names corresponding to sensor in asc.File based on channel number
            Channel.names[Channel.names$channel==i-1 ,"name"] <- asc.File$name.sensor[i]
        }
    }
    
    # Defining names and variables for gas sensors - Used the same names of variables as in SOS for compatibility reasons
    Sensor.names        <- list(Nitrogen_dioxide      = c("NO2_B43F_P1","NO2_A43F_P1","no2_b43f", "NO2-B43F", "NO2B43F", "NO2_M20", "NO2_C1", "NO2_C25", "NO2/C-20", "NO2_3E50", "NO23E50", "NO2", "S1"),
                                Carbon_monoxide       = c("CO_A4_P1"  , "CO_B4_P1","CO-B4", "CO-A4", "CO_MF200","CO/MF-200", "CO/MF-20", "CO-MF200", "CO_C200", "CO_CF200", "CO_3E300","CO3E300", "CO","COMF200","CO-A4 O","COA4", "S2"),
                                Ozone                 = c("OX_A431_P1", "OX_B431_P1","O3/M-5", "O3-B4", "AX-A431", "OX-A431", "OX_A431", "O3-A431", "O3_M5", "O3_C5", "O3_C100", "O3-M5", "o3_m_5", "O3_3E1F", "O33EF1", "O3", "O3E100", "S3"),
                                Nitric_oxide          = c("NO_B4_P1"  , "NO_A4_P1","NO-B4", "NOB4_P1","NOB4", "NO_M25", "NO_C1", "NO_C25","NO/C-25", "NO3E100", "NO_3E100", "NO", "No Sensor", "S4"),
                                Sulfur_dioxide        = c("SO2_B4_P1" , "SO2_A4_P1", "SO2_M20", "SO2_M20", "SO2_MF20", "SO2_C1", "SO2_C20", "SO2_CF20"),
                                Ammonia               = c("NH3_MR100" , "NH3_CR50"),
                                Particulate_Matter_1  = c("OPCN2PM1"  , "OPCN3PM1"), 
                                Particulate_Matter_25 = c("OPCN2PM25" , "OPCN3PM25"),
                                Particulate_Matter_10 = c("OPCN2PM10" , "OPCN3PM10"),
                                Bin0                  = c("OPCN2Bin0" , "OPCN3Bin0"),
                                Bin1                  = c("OPCN2Bin1" , "OPCN3Bin1"),
                                Bin2                  = c("OPCN2Bin2" , "OPCN3Bin2"),
                                Bin3                  = c("OPCN2Bin3" , "OPCN3Bin3"),
                                Bin4                  = c("OPCN2Bin4" , "OPCN3Bin4"),
                                Bin5                  = c("OPCN2Bin5" , "OPCN3Bin5"),
                                Bin6                  = c("OPCN2Bin6" , "OPCN3Bin6"),
                                Bin7                  = c("OPCN2Bin7" , "OPCN3Bin7"),
                                Bin8                  = c("OPCN2Bin8" , "OPCN3Bin8"),
                                Bin9                  = c("OPCN2Bin9" , "OPCN3Bin9"),
                                Bin10                 = c("OPCN2Bin10", "OPCN3Bin10"),
                                Bin11                 = c("OPCN2Bin11", "OPCN3Bin11"),
                                Bin12                 = c("OPCN2Bin12", "OPCN3Bin12"),
                                Bin13                 = c("OPCN2Bin13", "OPCN3Bin13"),
                                Bin14                 = c("OPCN2Bin14", "OPCN3Bin14"),
                                Bin15                 = c("OPCN2Bin15", "OPCN3Bin15"),
                                Bin16                 = "OPCN3Bin16",
                                Bin17                 = "OPCN3Bin17",
                                Bin18                 = "OPCN3Bin18",
                                Bin19                 = "OPCN3Bin19",
                                Bin20                 = "OPCN3Bin20",
                                Bin21                 = "OPCN3Bin21",
                                Bin22                 = "OPCN3Bin22",
                                Bin23                 = "OPCN3Bin23",
                                OPCHum                = "OPCN3Hum",
                                OPCLsr                = "OPCN3Lsr",
                                OPCTsam               = "OPCN3TSam",
                                OPCVol                = c("OPCN2Vol" , "OPCN3Vol"),
                                OPCTemp               = c("OPCN2Temp", "OPCN3Temp"),
                                OPCFlow               = "OPCN3FRt",
                                MOx                   = "MOX",
                                Carbon_dioxide        = "D300",
                                Bin1_PMS              = "53PT003",
                                Bin2_PMS              = "53PT005",
                                Bin3_PMS              = "53PT010",
                                Bin4_PMS              = "53PT025",
                                Bin5_PMS              = "53PT050",
                                Bin6_PMS              = "53PT100",
                                PM1_PMSraw            = "5301CST",
                                PM1_PMSCal            = "5301CAT",
                                PM25_PMSraw           = "5325CST",
                                PM25_PMSCal           = "5325CAT",
                                PM10_PMSraw           = "5310CST",
                                PM10_PMSCal           = "5310CAT",
                                Radon                 = "RD200M") # Add new sensor model to be recognized if needed
    
    #------------------------------------------------------------------------------CR
    # Adding sensor model type (Nitic_Oxide...) if more than 1 model of sensors then the model type are separated with "!" - 
    # The last sensor mdel type is used
    #------------------------------------------------------------------------------CR
    for (i in 1:length(Sensor.names)) {
        
        for (j in 1:length(Sensor.names[[i]])) {
            
            if (Sensor.names[[i]][j] %in% Channel.names$name) {
                
                Channel.names[which(Channel.names$name == Sensor.names[[i]][j]), "Variables"] <- names(Sensor.names)[i]
                break
            } 
        }
    }
    
    cat("[SQLite2df] INFO, sensors found in the airsenseur.db\n")
    print(cbind(Channel.names, lubridate::ymd_hms(Values_db$time[as.numeric(row.names(Channel.names))]) ))
    
    # Setting Values_db$Pollutants that gives the correct Polluants names even if the sensors are changed of position during use, not for chemical sensors, already done
    for (i in unique(Channel.names$name)) {
        
        # setting correct colnames in values_db for Meteo.names.change if chang are requested
        cat(paste0("[SLite2df] INFO setting Values_db$Pollutants to ", unique(Channel.names[Channel.names$name == i,"Variables"])," using the shield config file for sensor ", i), sep = "\n")
        Sensor.rows <- which(Values_db$name == i)
        if (length(Sensor.rows) > 0) {
            
            Values_db[Sensor.rows,"Pollutants"]   <- unique(Channel.names[Channel.names$name == i,"Variables"])
            #Values_db[Sensor.rows,"name"]         <- unique(Channel.names[Channel.names$name == i,"Variables"])
        }
    } 
    
    # Checking if some sensors were not recognized before aggregating, these data are discarded
    if (any(is.na(Values_db$Pollutants))) {
        
        cat("[SQLite2df] ERROR, At least one sensor name was not recognized. Check variable Sensor.names in function SQLite2df.\n")
        is.NA <- which(is.na(Values_db$Pollutants))
        Name.is.NA <- unique(Values_db[is.NA,"name"])
        
        for (i in Name.is.NA) {
            Channel.is.NA   <- unique(Values_db[Values_db$name == i, "channel"])
            min.time.is.NA <- min(lubridate::ymd_hms(Values_db[Values_db$channel == Channel.is.NA,"time"]), na.rm = T)
            max.time.is.NA <- max(lubridate::ymd_hms(Values_db[Values_db$channel == Channel.is.NA,"time"]), na.rm = T)
            cat(paste0("[SQLite2df] ERROR, sensor name ",i," channel number ", Channel.is.NA, " is not recognized between ", min.time.is.NA, " and ", max.time.is.NA, " these data are deleted\n"))
        }
        Values_db <- Values_db[-is.NA,]
    } 
    
    #------------------------------------------------------------------------------CR
    # Putting data in tabulated dataframe
    #------------------------------------------------------------------------------CR
    cat("[SQLite2df] INFO, Putting data in tabulated form and aggregatting on the time column values, this can be long with large datasets\n")
    # removing sampleEvaluatedVal, name and channel. Aggregating in tabulated form. Discarding 0s in coordinates and altitude to avoid error when averaging
    Values_db[which(Values_db$altitude  == 0), "altitude"]  <- rep(NA, length(which(Values_db$altitude  == 0)))
    Values_db[which(Values_db$longitude == 0), "longitude"] <- rep(NA, length(which(Values_db$longitude == 0)))
    Values_db[which(Values_db$latitude  == 0), "latitude"]  <- rep(NA, length(which(Values_db$latitude  == 0)))
    data.table::set(Values_db, j = "time", value =  ymd_hms(Values_db[["time"]], tz = Influx.TZ))
    if (!haskey(Values_db)) setkey(Values_db, "time")
    
    # Aggregating in tabulated form.
    i <- 0
    if (nrow(Values_db) < Page) Page <- nrow(Values_db)
    while (i < nrow(Values_db)) {
        # Checking for a correct Page value for paging
        if ((i + Page) > nrow(Values_db)) Page <- nrow(Values_db) - i
        cat(paste0("[SQLite2df] INFO, aggregating airsenseur.db in tabulated rows ", format(i + Page, scientific = FALSE),"/", nrow(Values_db)), sep = "\n" )
        # casting data according to channel names
        # Buffer <- cast(Values_db[(i + 1):(i + Page),], time + boardTimeStamp + gpsTimestamp + altitude + latitude + longitude  ~ Pollutants, 
        #                value = "sampleEvaluatedVal", fun.aggregate = 'mean' , fill = NA, na.rm = TRUE)
        # Buffer <- spread(data = Values_db[(i + 1):(i + Page), c("time", "boardTimeStamp","gpsTimestamp", "altitude", "latitude", "longitude", "Pollutants", "sampleEvaluatedVal")], 
        #                  key = Pollutants, value = sampleEvaluatedVal) %>% arrange(time)
        Buffer <- dcast.data.table(Values_db[(i + 1):(i + Page)], time+boardTimeStamp+gpsTimestamp+altitude+latitude+longitude ~ Pollutants, value.var = "sampleEvaluatedVal", fun.aggregate = mean, na.rm = TRUE)
        # aggregating in Values_db_cast
        if (exists("Values_db_cast")) {
            Values_db_cast <- rbindlist(list(Values_db_cast, Buffer), use.names = TRUE, fill = TRUE)  
        } else Values_db_cast <- Buffer
        i <- i + Page
    }
    Values_db <- Values_db_cast
    remove(Values_db_cast, Buffer, Sensor.names, Channel.names, Meteo.names.change)
    
    # Transforming column time in POSIX with the correct time zone (UTC), changing name to date
    if (is.null(Influx.TZ)) {
        cat("[SQLite2df] INFO, Converting datetime from character to POSIX format, ERROR time zone is not set for InfluxDB. Using UTC.\n")
        Values_db$time <- lubridate::ymd_hms(Values_db$time, tz = "UTC")
        
    } else{
        cat(paste0("[SQLite2df] INFO, Converting Values_db$time from character to POSIX format, timezone is ", Influx.TZ), sep = "\n")
        #Values_db$time <- as.POSIXct(strptime(Values_db$time, format = "%Y-%m-%d %H:%M:%S", tz = Influx.TZ)) 
        Values_db$time <- lubridate::ymd_hms(Values_db$time, tz = Influx.TZ)
    }
    
    # Change to date to use OpenAir
    names(Values_db)[colnames(Values_db) == "time"] <- "date"
    
    # Averaging with UserMIns averaging time, creating Values_db_Mins, this allow to reduce the number of rows with empty sensor values
    if (exists("Values_db") && !is.null(UserMins)) {
        
        cat(paste0("[SQLite2df] INFO, averaging each ", UserMins, " mins. This can be long with large datasets."), sep = "\n")
        Values_db_Mins <- DF_avg(Values_db, width = UserMins)
        
    } else stop(cat(paste0("[SQLite2df] ERROR, UserMins is not set in ASEConfig_xx.R. Please set it, default 10 mins, the script is stopped."), sep = "\n"))
    
    # returning data if any
    # Trying to load the existing data or Influx.Rdata.file
    if (is.null(InfluxData) || is.na(InfluxData)) {
        
        if (file.exists(DownloadSensor$Influx.Rdata.file)) {
            if (extension(DownloadSensor$Influx.Rdata.file) == ".csv") {
                
                InfluxData <- fread(file = DownloadSensor$Influx.Rdata.file, na.strings = c("","NA", "<NA>"))
                if (!is.null(Influx.TZ) && Influx.TZ != "") {
                    data.table::set(InfluxData, j = "date", value =  ymd_hms(InfluxData[["date"]], tz = Influx.TZ))
                } else data.table::set(InfluxData, j = "date", value =  ymd_hms(InfluxData[["date"]], tz = "UTC"))
                
            } else if (extension(DownloadSensor$Influx.Rdata.file) == ".Rdata") {
                
                load(DownloadSensor$Influx.Rdata.file)
                InfluxData <- data.table(InfluxData)
                
            } 
        } else cat(paste0("[INFLUXDB] INFO: there is no previously saved Influx.Rdata.file. Missing InfluxData and Down.Influx request of sensor data download ."), sep = "\n")
    }
    if (!Complete) {
        if (exists("Values_db_Mins") && !is.na(Values_db_Mins) && nrow(Values_db_Mins) > 0) {
            
            cat("[SQLite2df] INFO, returning newly downlaoded sensor data.\n")
            print(str(Values_db_Mins))
            return(Values_db_Mins)
            
        } else {
            cat("[SQLite2df] WARNING, no new downloaded INflux data nor previous ones.\n")
            # adding the existing data before averaging, all rows except the last one that shall be recalculated with new values
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            return()
        }
    } else {
        if (exists("Values_db_Mins") && !is.na(Values_db_Mins) && nrow(Values_db_Mins) > 0) {
            
            if (exists("InfluxData") && !is.na(InfluxData) && nrow(InfluxData) > 0) {
                
                cat("[SQLite2df] INFO, returning previously and newly downloaded sensor data.\n")
                if (InfluxData[nrow(InfluxData),"date"] == Values_db[1,"date"])
                    Values_db_Mins <- rbindlist(list(InfluxData, Values_db_Mins), fill = TRUE)
                    print(str(Values_db_Mins))
                    return(Values_db_Mins)
                    
            } else {
                
                cat("[SQLite2df] INFO, returning newly downloaded sensor data.\n")
                print(str(Values_db_Mins))
                return(Values_db_Mins)
                
            }
        }
    }
}

#=====================================================================================CR
# MG : Download Reference Data retrieving
#=====================================================================================CR
Down_Ref <- function(Reference.name, urlref, UserMins, DownloadSensor, AirsensWeb, naStrings = NULL, WDoutput = NULL, ref.tzone = "UTC", 
                     FTPMode = "ftp", 
                     
                     Ref.SOS.name = NULL, RefSOSname = NULL, RefSOSDateIN = NULL, RefSOSDateEND = NULL,
                     
                     Ref__a_i_p__name = NULL, User__a_i_p__ = NULL, Pass__a_i_p__ = NULL, Ref__a_i_p__Organisation = NULL, 
                     Ref__a_i_p__Station = NULL, Ref__a_i_p__Pollutants = NULL, Ref__a_i_p__DateIN = NULL, Ref__a_i_p__DateEND = NULL,
                     
                     csvFile = NULL, csvFile.sep = NULL, csvFile.quote = NULL, Old.Ref.Data = NULL, coord.ref = NULL, Ref.Type = "Ref") {
    
    # Reference.name        = Name of for Reference station
    # urlref                = Vector of URIs linking to csv files with the reference data. Frst row: header with variable names as in ASEConfig.R
    #                         and one column of date (DateTime), errors and NAs as -999.99, only one column may include one of the string c("date","time","Date", "Time", "DATE", "TIME")
    #                         ftp uri for reference data retrievals. The link shall point to a vector of character with csv files, with headers with the names of variable
    #                         One header shall includes the string DateTime of dates
    # UserMins              = periodicity of data requested after final data treatment
    # nastring              = a string character giving wrong values
    # DownloadSensor        = a list with 
    #                         WDinput, the directory where the Rdata are saved
    #                         Retrieve.data.Ref, true if data need be retrieved
    #                         DateEND.Ref.prev, date to start new download of data if "RefData.Rdata" already exist (the file may not inexist)
    # DownloadSensor        = a list with possible values
    #                         General.Rdata, the name of the data frame with downloaded data in directory WDinput
    #                         WDinput, the directory where the Rdata are saved
    #                         Retrieve.data.SOS, true if data need be retrieved
    #                         DateEND.SOS.prev, date to start download of SOS sensor data if InfluxData.Rdata already exist (may not exist)
    #                         The time zone is the one of SOS (GMT). 
    #                         Default value for DownloadSensor is NULL, nothing passed. In this case the Down_SOS
    #                         will create new Rdata/csv determining DateIN and DateEND using SOS information
    # WDoutput              = where to save the open Air plot of Reference data, if NULL there are saved in the current directory (getwd())
    # ref.tzone             = string, refernce time name of the reference data. Default = "UTC"
    # FTPMode               = string, default = "ftp", type of download of reference data: "ftp" using a csv file on a ftp server, "csv" the same with a local file and SOS: SOS download
    
    # Ref.SOS.name          = SOS ID of the Reference station
    # RefSOSname            = Reference station SOS Rest API URL
    # RefSOSDateIN          = Starting  date for downloading Reference data using SOS
    # RefSOSDateEND         = Ending date for downloading Reference data using SOS
    
    # Ref__a_i_p__name         = input$Ref__a_i_p__name, 
    # User__a_i_p__            = input$User__a_i_p__, 
    # Pass__a_i_p__            = input$Pass__a_i_p__, 
    # Ref__a_i_p__Organisation = input$Ref__a_i_p__Organisation, 
    # Ref__a_i_p__Station      = input$Ref__a_i_p__Station, 
    # Ref__a_i_p__Pollutants   = input$Ref__a_i_p__Pollutants, 
    # Ref__a_i_p__DateIN       = as.Date(input$Ref__a_i_p__Date[1], format = "%Y-%m-%d"),
    # Ref__a_i_p__DateEND      = as.Date(input$Ref__a_i_p__Date[2], format = "%Y-%m-%d"),
    
    # csvFile               = if FTPMode = "csv", file path to the csv file to load
    # csvFile.sep           = if FTPMode = "csv", separator between columns in the csvFile
    # csvFile.quote         = if FTPMode = "csv", separator of values in all columns
    # Old.Ref.Data          = dataframe with previously loaded reference data to be merged with currently loading dataframe reference data, default is null, NULL
    # coord.ref             = string with coordinates of reference data longitude and latitude separated by a blank
    # Ref.type              = label to be written in front of pollutatns names, defaut is Ref, other possibility Bine for PM distribution
    
    # return                = dataframe Ref with the data
    
    cat("\n")
    cat("-----------------------------------------------------------------------------------\n")
    cat(paste0("[Down_Ref] INFO, Reference Data retrieving for ", Reference.name), sep = "\n")
    
    #------------------------------------------------------------------------------CR
    # Set time interval of interest
    #------------------------------------------------------------------------------CR
    minSec <- UserMins*60.
    
    #------------------------------------------------------------------------------CR
    # Name of Reference pollutants using label(phenomenon(ts))
    #------------------------------------------------------------------------------CR
    # Add new sensor model to be recognized if needed
    Reference.names        <- list(date        = c("date" ,"Date", "DATE", "DateTime"),
                                   time        = c("time" , "Time"),
                                   Ref.NO2     = c("NO2"  , "Nitrogen dioxide (air)", "Ref.NO2", "nitrogen dioxide"),
                                   Ref.SO2     = c("SO2"  , "Sulfur dioxide (air)", "Ref.SO2", "sulfur dioxide"),
                                   Ref.O3      = c("O3"   , "Ozone (air)", "Ref.O3", "ozone"),
                                   Ref.NO      = c("NO"   , "Nitrogen monoxide (air)", "Ref.NO", "nitrogen monoxide")  ,
                                   Ref.NOx     = c("NOx"   , "nitrogen oxides")  ,
                                   Ref.PM10    = c("PM10" , "Particulate matter < 10 µµm (aerosol)", "Ref.PM10"),
                                   Ref.PM2.5   = c("PM2.5", "Particulate matter < 2.5 µµm (aerosol)", "Ref.PM2.5"),
                                   Ref.CO_ppm  = c("CO"   , "Carbon monoxide (air)", "co", "Ref.CO_ppm", "CO_ppm", "carbon monoxide"),
                                   Ref.Temp    = c("Temperature", "Sample_air temperature"),
                                   Ref.RH      = "Relative_humidity",
                                   Ref.Press   = "Atmospheric_pressure") 
    
    # Downloading according to FTPMode
    if (FTPMode == "ftp" | FTPMode == "csv") {
        
        # set DateIN for data retrieving, either from initial date or last date in previous DataFrame
        if (any(grepl(pattern = "DateEND.Ref.prev", x = objects(DownloadSensor)))) {
            # download of ref data exists
            if (is.null(DownloadSensor$DateEND.Ref.prev)) {
                # mindateRef: the starting date of InfluxData or SOSData
                if (any(grepl(pattern = "mindateRef", x = objects(DownloadSensor)))) {
                    DateIN  <- DownloadSensor$mindateRef
                } else {
                    DateIN  <- as.POSIXct("2015-12-01 00:00", tz = ref.tzone)
                }
            } else  DateIN  <- DownloadSensor$DateEND.Ref.prev + minSec
        } else DateIN  <- as.POSIXct("2015-12-01 00:00", tz = ref.tzone)
        
        # Setting end date to curent date (the time zone of the refrence shall be in UTC, normally it is allways like that)
        if (exists("ref.tzone")) {
            DateEND <- as.POSIXct(Sys.time(), tz = ref.tzone)
        } else {
            stop(cat("[Down_Ref] ERROR, ref.tzone is not set in ASEConfig_xx.R. The script is stopped, please set ref.tzone ."))
        }
        
        #Set time interval, with function interval of package lubridate
        date <- lubridate::interval(DateIN, DateEND, tzone = ref.tzone) # tzone=user.tzone
        # the function  interval returns a variable of class lubridate 
        cat(paste0("[Down_Ref] INFO, Time zone for reference data: ", date@tzone), sep = "\n")
        
        # creating returning data frame Ref
        Ref       <- data.frame(date = seq(date@start, length = date@.Data/minSec, by = paste0(toString(UserMins)," ","min")),                     
                                row.names = NULL, check.rows = FALSE,
                                check.names = TRUE,
                                stringsAsFactors = FALSE)
        if (nrow(Ref) == 0) stop(" Either the start or end downloading date or UserMins parameter is wrong. The script is stopped ...")
        
        if (FTPMode == "ftp") {
            
            for (i in seq(urlref)) {
                
                # File to search
                File.csv  <- basename(urlref[i])
                url       <- dirname(urlref[i])
                
                # adding final "/" if missing to use getURL
                if (substr(url, nchar(url), nchar(url)) != "/") url <- paste0(url,"/")
                filenames <- getURL(url = url, 
                                    ftp.use.epsv = FALSE, 
                                    dirlistonly = TRUE
                )
                
                # Deal with newlines as \n or \r\n. (BDR)
                # Or alternatively, instruct libcurl to change \n's to \r\n's for us with crlf = TRUE
                # filenames = getURL(url, ftp.use.epsv = FALSE, ftplistonly = TRUE, crlf = TRUE)
                filenames = unlist(strsplit(filenames, "\r*\n")[[1]])
                
                # checking if File.csv is present
                if (any(grepl(pattern = File.csv, x = filenames))) {
                    
                    filename = urlref[i]
                    con = getCurlHandle( ftp.use.epsv = FALSE)
                    
                    # there is a slight possibility that some of the files that are
                    # returned in the directory listing and in filenames will disappear
                    # when we go back to get them. So we use a try() in the call getURL.
                    #contents = sapply(filenames, function(x) try(getURL(x, curl = con)))
                    #names(contents) = filenames[1:length(contents)]
                    
                    cat(paste0("[Down_Ref] INFO, trying to download data from ", urlref[i]), sep = "\n")
                    #xx = getURL(urlref[i], nobody=1L)
                    
                    #if (!is.na(xx)) {
                    
                    #    xxx<- strsplit(xx, "\r\n")
                    # as.numeric(xx["Content-Length:"]) 
                    # xtmp <- unlist(strsplit(unlist(xxx), split = " "))
                    # if (!is.na(xtmp[2:2])) {
                    
                    Reference.i <- read.csv(urlref[i], 
                                            header           = TRUE,
                                            na.strings       = naStrings,
                                            check.names      = FALSE,
                                            stringsAsFactors = FALSE) 
                    
                    # Selecting data within date interval
                    if (nrow(Reference.i) == 0) {
                        
                        my_message <- paste0("[Down_Ref] ERROR no data found in the file of reference data for ", Reference.name, " .\n")
                        shinyalert(
                            title = "ERROR no data in the csv file",
                            text = "my_message",
                            closeOnEsc = TRUE,
                            closeOnClickOutside = TRUE,
                            html = FALSE,
                            type = "error",
                            showConfirmButton = TRUE,
                            showCancelButton  = FALSE,
                            confirmButtonText = "OK",
                            confirmButtonCol  = "#AEDEF4",
                            timer = 0,
                            imageUrl = "",
                            animation = FALSE
                        )
                        cat(my_message)
                    } else {
                        # use openair function to aggregate to the selected time average, in openair time must be replaced in date
                        
                        # Adding coordinates of the reference stations
                        if (!is.null(coord.ref)) {
                            
                            # taking coordinates from coord.ref
                            long <- unlist(strsplit(x = coord.ref, split = ","))[1]
                            lat  <- unlist(strsplit(x = coord.ref, split = ","))[2]
                            
                            if (any(grep(pattern = paste0(c("N","S", "E", "W", "d"), collapse = "|" ), x = coord.ref))) {
                                
                                # transform spherical coordinates to decimal degrees for later projection
                                Ref.coord_d    <- OSMscale::degree(lat, long, digits = 5)
                                Reference.i$Ref.Lat   <- Ref.coord_d[1,1]
                                Reference.i$Ref.Long  <- Ref.coord_d[1,2]
                                
                            } else {
                                
                                Reference.i$Ref.Long <- as.numeric(long)
                                Reference.i$Ref.Lat  <- as.numeric(lat)
                            }
                        }
                        
                        # checking if we have a date column in the referenceValues()
                        if (any(grepl(pattern = paste0(c("date","time","Date", "Time", "DATE", "TIME", "DateTime"), collapse = "|"), 
                                      x       = colnames(Reference.i)))) {
                            
                            # checking if there is more than 1 field with "date","time","Date", "Time", "DATE", "TIME"
                            if (length(which(grepl(pattern = paste0(c("date","time","Date", "Time", "DATE", "TIME"), collapse = "|"), 
                                                   x       = colnames(Reference.i)))) == 1) {
                                
                                names(Reference.i)[grep(pattern = paste0(c("date","time","Date", "Time", "DATE", "TIME", "DateTime"), collapse = "|"),
                                                        x       = colnames(Reference.i)
                                )] <- "date" 
                                Reference.i$date <- as.POSIXct(strptime(Reference.i$date,  "%Y-%m-%d %H:%M:%S", tz = ref.tzone))
                                Reference.i <- data.frame(timeAverage(Reference.i, avg.time = paste0(toString(UserMins)," ","min"), statistic = "mean", 
                                                                      start.date = round(min(Reference.i$date), units = "hours"), 
                                                                      fill = TRUE))
                                
                                # matching dates, MG changed using merge
                                for (i in colnames(Reference.i)[-which(colnames(Reference.i) == "date")]) { 
                                    
                                    # Ref$i <- NA 
                                    if (any(Ref$date %in% Reference.i$date)) {
                                        
                                        Ref[which(Ref$date %in% Reference.i$date),paste0("Ref.",i)] <- Reference.i[which(Reference.i$date %in% Ref$date),i]    
                                    } 
                                }
                            } else {
                                
                                my_message <- "[Down_Ref] ERROR, There is no or more than one column called  with names date, time, Date , Time, DATE, TIME or DateTime. The script is stopped"
                                shinyalert(
                                    title = "ERROR with date column",
                                    text = "my_message",
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
                                cat(my_message)
                            } 
                        } else {
                            
                            my_message <- "[Down_Ref] ERROR, There is no column called date, time, Date , Time, DATE, TIME or DateTime or separator and quote are not set correctly. The script is stopped"
                            shinyalert(
                                title = "ERROR with date column",
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
                            cat(my_message)
                        }
                    }
                } else {
                    
                    my_message <- paste0("[Down_Ref] ERROR the csv file ",File.csv," does not exist at ", url," .\n")
                    shinyalert(
                        title = "ERROR with the csv file",
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
                    cat(my_message)
                }
            }
        } else {
            
            if (FTPMode == "csv") {
                
                # No need to check if the file exist since it was selected with choose.file
                cat(paste0("[Down_Ref] INFO, loading local file for reference data, file ", csvFile, "\n"))
                
                if (file.exists(csvFile)) { #REMOVE THIS TEST, The FILE exists since it is selected
                    
                    if (grepl(".csv", csvFile, fixed = T)) {
                        
                        # if you load a .csv file:
                        
                        Reference.i <- read.csv(file        = csvFile,
                                                header      = TRUE, 
                                                na.strings  = naStrings,
                                                sep         = csvFile.sep,
                                                quote       = csvFile.quote,
                                                check.names = FALSE,
                                                stringsAsFactors = FALSE)
                    } else if (grepl(".Rdata", csvFile, fixed = T)) {
                        
                        # if you load a .Rdata file:
                        # loaded Rdata with unknown name dataframe
                        # see https://stackoverflow.com/questions/2520780/determining-name-of-object-loaded-in-r
                        temp.space <- new.env()
                        Reference.i <- load(file = csvFile, envir = temp.space)
                        Reference.i <- get(Reference.i, envir = temp.space)
                        rm(temp.space)
                        
                        # removing un-necessary columns of Reference.i
                        # possible names 
                        all.names <- character(0)
                        for (i in seq_along(Reference.names)) all.names <- c(all.names, unlist(Reference.names[[i]]))
                        Reference.i <- Reference.i[,which(names(Reference.i) %in% all.names)]
                        
                    } else {
                        
                        my_message <- paste0("[Down_Ref()] ERROR, unrecognized file type for \n
                                               reference data .\n")
                        cat(my_message)
                        shinyalert(
                            title = "ERROR unrecognised file type",
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
                    
                    # if you load a GRIMM .txt file
                    if (grepl(".txt", csvFile, fixed = T) & Ref.Type == "GRIMM") {
                        
                        # read Bin names
                        bins_diameters_GRIMM <- read.table(csvFile,
                                                           header = F, skip = 1, sep=",", nrows = 1)[-1]
                        names(bins_diameters_GRIMM) <- paste0("Bin", seq(1:length(names(bins_diameters_GRIMM))))
                        # remove special characters such as ">" and "µm"
                        bins_diameters_GRIMM <- apply(bins_diameters_GRIMM,  MARGIN = 2, function(col) gsub(paste(c("µm", ">"), collapse = "|"), "",(col)) )
                        diameters_GRIMM <- as.numeric(bins_diameters_GRIMM)
                        MAX_Diam_GRIMM <- max(diameters_GRIMM)
                        bins_diameters_GRIMM <- as.data.frame(t(bins_diameters_GRIMM))
                        
                        # read GRIMM data
                        # check the structure of the GRIMM file first...
                        Reference.i <- read.table(csvFile,
                                                  header = F, sep="," , fill = T)
                        
                        # find lines where it is reported the word "File" and the symbol ">"
                        # add all neccessary criteria to skip lines
                        logical <- apply(Reference.i, MARGIN = 2, function(col) grepl(paste(c("File", ">"), collapse = "|"), col))
                        logical <- as.data.frame(logical)
                        line.to.remove <- apply(logical, MARGIN = 2, function(col) which(col == TRUE) )
                        line.to.remove <-  unique(unlist(line.to.remove))
                        
                        # read GRIMM data again but skip selected lines
                        Reference.i <- read.table(csvFile,
                                                  header = F, sep="," , fill = T)[-c(line.to.remove), ]
                        
                        # remove columns with 0 values
                        Reference.i <- Reference.i[, colSums(Reference.i != 0) > 0]
                        # add names of header
                        names(Reference.i) <- c("date", names(bins_diameters_GRIMM))
                        
                        ## !!! need to do some data cleaning
                        n.bin.GRIMM <- names(bins_diameters_GRIMM)
                        nbin.GRIMM.next <- paste0("Bin", as.numeric(sub(pattern = "Bin",replacement = "",n.bin.GRIMM)) +1 )
                        
                        
                        names(Reference.i)[which(names(Reference.i) != "date")] <- n.bin.GRIMM
                        # Adjust date format --> replace "." with ":"
                        Reference.i$date <- gsub(".", ":", Reference.i$date, fixed = TRUE)
                        Reference.i$date  <- as.POSIXct(Reference.i$date,format="%d/%m/%Y %H:%M:%S", tz="UTC")
                        
                        # make all fileds as numeric
                        Reference.i[, names(Reference.i)[which(names(Reference.i) != "date")]] <- sapply(names(Reference.i)[which(names(Reference.i) != "date")], function(x) { as.numeric(Reference.i[,x])})
                        
                        # calculate the effective number of counts within two consecutive Bins of the GRIMM
                        Reference.i[,paste0("Bin", 1:(length(n.bin.GRIMM)-1))] <- sapply(1:(length(n.bin.GRIMM)-1),   function(i) Reference.i[ ,paste0("Bin",i)] - Reference.i[, paste0("Bin",i+1)])
                        
                    }
                    
                    # Selecting data within date interval
                    if (nrow(Reference.i) == 0) {
                        
                        my_message <- paste0("[Down_Ref] ERROR no data found for reference, lack of new data for ", Reference.name, "\n")
                        cat(my_message)
                        shinyalert(
                            title = "ERROR no reference data",
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
                        
                        # Discarding columns without name
                        if (any(colnames(Reference.i) == "")) {
                            Reference.i <- Reference.i[,-which(names(Reference.i) == "")]
                        }
                        
                        # Adding coordinates of the reference stations
                        if (!is.null(coord.ref)) {
                            
                            # taking coordinates from coord.ref
                            long <- unlist(strsplit(x = coord.ref, split = ","))[1]
                            lat  <- unlist(strsplit(x = coord.ref, split = ","))[2]
                            
                            if (any(grep(pattern = paste0(c("N","S", "E", "W", "d"), collapse = "|" ), x = coord.ref))) {
                                
                                # transform spherical coordinates to decimal degrees for later projection
                                Ref.coord_d    <- OSMscale::degree(lat, long, digits = 5)
                                Reference.i$Ref.Lat   <- Ref.coord_d[1,1]
                                Reference.i$Ref.Long  <- Ref.coord_d[1,2]
                                
                            } else {
                                
                                Reference.i$Ref.Long <- as.numeric(long)
                                Reference.i$Ref.Lat  <- as.numeric(lat)
                            }
                        }
                        
                        # checking if we have a date column in the referenceValues()
                        if (any(grepl(pattern = paste0(c("date","time","Date", "Time", "DATE", "TIME", "DateTime"), collapse = "|"), 
                                      x       = colnames(Reference.i)
                        ))) {
                            
                            # checking if there is more than 1 field with "date","time","Date", "Time", "DATE", "TIME"
                            if (length(which(grepl(pattern = paste0(c("date","time","Date", "Time", "DATE", "TIME", "DateTime"), collapse = "|"), 
                                                   x        = colnames(Reference.i)))
                            ) == 1 ) {
                                
                                # setting name of timedate column as "date" for openair
                                names(Reference.i)[grep(pattern = paste0(c("date","time","Date", "Time", "DATE", "TIME", "DateTime"), collapse = "|"), 
                                                        x       = colnames(Reference.i))] <- "date" 
                                
                                # convert date to POSIX with time zone set in shiny
                                if (!("POSIXct" %in% class(Reference.i$date))) Reference.i$date <- as.POSIXct(Reference.i$date,  tz = ref.tzone, 
                                                                                                              tryFormats = c("%Y-%m-%d %H:%M:%OS",
                                                                                                                             "%Y/%m/%d %H:%M:%OS",
                                                                                                                             "%Y-%m-%d %H:%M:%S",
                                                                                                                             "%Y-%m-%d %H:%M",
                                                                                                                             "%m/%d/%Y %H:%M",
                                                                                                                             "%Y-%m-%d",
                                                                                                                             "%m/%d/%Y")) # "%m/%d/%Y %H:%M", strptime removed with the format as this may cause a crash, but slower operation
                                
                                # Convert all other columns to numeric if they are not excepts coordinates
                                for (j in names(Reference.i)[-which(names(Reference.i) %in% c("date", "Ref.Long", "Ref.Lat"))]) if (class(Reference.i[,j]) != "numeric") Reference.i[,j] <- as.numeric(Reference.i[,j])
                                
                                # aggregate Reference.i with mean over UserMins
                                # use openair function to aggregate to the selected time average, in openair time must be replaced in date
                                if (threadr::detect_date_interval(Reference.i$date, skip = 3, n = 50) / 60 != UserMins) {
                                    
                                    Reference.i <- data.frame(openair::timeAverage(Reference.i, 
                                                                                   avg.time = paste0(toString(UserMins)," ","min"), 
                                                                                   interval = paste0(toString(UserMins)," ","min"), 
                                                                                   statistic = "mean", 
                                                                                   start.date = round(min(Reference.i$date), units = "hours"), 
                                                                                   fill = TRUE),
                                                              check.names = FALSE)
                                    
                                }  else if (is.POSIXct(DownloadSensor$DateIN.Ref.prev) && is.POSIXct(DownloadSensor$DateEND.Ref.prev) && 
                                            !all( as.logical(Reference.i$date[as.Date(Reference.i$date) == as.Date(Reference.i$date)[1]] %in% 
                                                             seq(from = DownloadSensor$DateIN.Ref.prev, 
                                                                 to   = max(Reference.i$date, na.rm = TRUE), 
                                                                 by = paste0(toString(UserMins)," ","min"))) ) ) {
                                    
                                    # Checking if dates fall on full hours
                                    Reference.i <- data.frame(openair::timeAverage(Reference.i, 
                                                                                   avg.time = paste0(toString(UserMins)," ","min"), 
                                                                                   interval = paste0(toString(UserMins)," ","min"), 
                                                                                   statistic = "mean", 
                                                                                   start.date = round(min(Reference.i$date), units = "hours"), 
                                                                                   fill = TRUE),
                                                              check.names = FALSE)
                                }  else if (!all( as.logical(Reference.i$date[as.Date(Reference.i$date) == as.Date(Reference.i$date)[1]] %in% 
                                                             seq(from = as.POSIXct(as.Date(Reference.i$date)[1]), 
                                                                 to   = as.POSIXct(as.Date(Reference.i$date)[length(as.Date(Reference.i$date))]), 
                                                                 by = paste0(toString(UserMins)," ","min"))) ) ) {
                                    Reference.i <- data.frame(openair::timeAverage(Reference.i, 
                                                                                   avg.time = paste0(toString(UserMins)," ","min"), 
                                                                                   interval = paste0(toString(UserMins)," ","min"), 
                                                                                   statistic = "mean", 
                                                                                   start.date = round(min(Reference.i$date), units = "hours"), 
                                                                                   fill = TRUE),
                                                              check.names = FALSE)
                                }
                                
                                # Setting Reference names (change names of pollutants adding Ref.)
                                if (Ref.Type == "Ref") {
                                    
                                    for (i in seq(Reference.names)) {
                                        
                                        for (j in seq(Reference.names[[i]])) {
                                            
                                            if (any(names(Reference.i) == Reference.names[[i]][j])) {
                                                names(Reference.i)[names(Reference.i) == Reference.names[[i]][j]] <- names(Reference.names)[i]
                                                break
                                            } 
                                        }
                                    }
                                } else if (Ref.Type %in% c("Bin.DMPS", "Bin.APS", "GRIMM")) {
                                    
                                    # Adding lable to pollutants which are not in Reference.names
                                    names.not.Ref <- names(Reference.i)[grep(pattern = paste(c("date","Ref.", "Bin.DMPS.", "Bin.APS.", "GRIMM."), collapse = "|"), x = names(Reference.i), invert = TRUE)]
                                    names(Reference.i)[which(names(Reference.i) %in% names.not.Ref)] <- sapply(seq_along(names.not.Ref), function(k) paste0(Ref.Type, ".", names.not.Ref[k]))
                                }
                                
                                # matching dates,
                                # set DateIN for data retrieving, either from initial date or last date in previous DataFrame
                                if ( !any(names(Reference.i)[-which(names(Reference.i) %in% c("date", "Ref.Long", "Ref.Lat"))] %in% DownloadSensor$Var.Ref.prev[-which(names(Reference.i) %in% c("date", "Ref.Long", "Ref.Lat"))])
                                     || max(Reference.i$date, na.rm = T) > DownloadSensor$DateEND.Ref.prev)  {
                                    
                                    # download of ref data exists
                                    
                                    DateIN  <- min(Reference.i$date, na.rm = T) 
                                    
                                    #Set time interval, with function interval of package lubridate
                                    date <- lubridate::interval(DateIN, DateEND, tzone = ref.tzone) # tzone=user.tzone
                                    
                                    # creating returning data frame Ref
                                    Ref       <- data.frame(date = seq(date@start, length = date@.Data/minSec, by = paste0(toString(UserMins)," ","min")),                     
                                                            row.names = NULL, check.rows = FALSE,
                                                            check.names = TRUE,
                                                            stringsAsFactors = FALSE)
                                    if (nrow(Ref) == 0) stop(" Either the start or end downloading date or UserMins parameter is wrong. The script is stopped ...")
                                    
                                    for (i in colnames(Reference.i)[-which(colnames(Reference.i) == "date")]) { 
                                        
                                        # Ref$i <- NA 
                                        if (any(Ref$date %in% Reference.i$date)) {
                                            
                                            Ref[which(Ref$date %in% Reference.i$date),i] <- Reference.i[which(Reference.i$date %in% Ref$date),i]    
                                        } 
                                    }
                                }
                            } else {
                                
                                my_message <- "[Down_Ref] ERROR, There is no or more than one column called  with names date, time, Date , Time, DATE, TIME or DateTime. The script is stopped"
                                shinyalert(
                                    title = "ERROR with date column",
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
                                cat(my_message)
                                
                            } 
                        } else {
                            
                            my_message <- "[Down_Ref] ERROR, There is no column called date, time, Date , Time, DATE, TIME or DateTime. The script is stopped"
                            shinyalert(
                                title = "ERROR with date column",
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
                            cat(my_message)
                        } 
                    }
                } else {
                    
                    my_message <- "[Down_Ref] ERROR, please select file of reference data"
                    shinyalert(
                        title = "ERROR no file selected",
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
                    cat(my_message)
                }
            }
        }
    } else {
        
        if (FTPMode == "SOS") {
            
            # UserMins              = periodicity of data requested for the returned dataframe
            # Duration              = integer, the number of days to download per page (as Limit in SQL), default is NULL, data are downloaded in slices of 7 days
            # DateEND               = To be set if the whole function is run over an internal from DownloadSensor$DateEND.SOS.prev until DateEND. DownloadSensor$DateEND.SOS.prev 
            #                         can be set manually before running Down_SOS.
            # Ref.tzone             = Time zone of the reference data, default is "UTC"                        
            # return                = dataframe InfluxData with the data to be added + 2 files are saved SOSData.Rdata and SOSData.csv
            # dependences           = havingIP(), ping()
            
            # Sensor Data retrieving at apiEndpoint
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat(paste0("[Down_Ref] INFO, ", Ref.SOS.name," reference data retrieving"), sep = "\n")
            
            # Checking internet connection availability
            if (havingIP()) {
                URL <- unlist(strsplit(unlist(strsplit(gsub('http://', '', RefSOSname), split = '/'))[1], split = ':'))[1]
                if (PingThisSite(URL)) {
                    cat(paste0("[Down_Ref] INFO; ping to ", RefSOSname, " Ok"), sep = "\n")
                } else{
                    cat(paste0("[Down_Ref] ERROR: you have an internet connection but cannot ping to ",RefSOSname,". I maybe for security reasons or download cannot be carried out.\n"))
                } 
            } else {
                return(cat(paste0("[Down_Ref] ERROR: no internet connection. Download cannot be carried out."), sep = "\n"))
            }
            
            # connect
            Endpoint <- sensorweb4R::Endpoint(RefSOSname)
            # number of category at the Endpoint
            cat(paste0("[Down_Ref] INFO, in total ", length(timeseries(Endpoint)), " data series at the SOS client for the selected station."), sep = "\n")
            
            # Selecting service "AirSensEUR" with name 
            srv <- sensorweb4R::services(Endpoint)
            cat(paste0("[Down_Ref] INFO, SOS services: ", label(srv), "\n"))
            
            # get all phenomena
            phe <- sensorweb4R::phenomena(Endpoint)
            cat(paste0("[Down_Ref] INFO, SOS phenomena avaialble :",label(sensorweb4R::phenomena(Endpoint)), "\n"))
            
            # get the station number corresponding to Ref.SOS.name in label(stations(srv))
            if (
                any(
                    grepl(pattern = Ref.SOS.name, 
                          x       = label(sensorweb4R::stations(srv))
                    )
                )
            ) {
                sta <- sensorweb4R::stations(srv)[grep(pattern = Ref.SOS.name, 
                                                       x       = label(sensorweb4R::stations(srv)
                                                       )
                )
                ]
                cat(paste0(label(sta),"\n"))
            } else {
                cat(cat(paste0("[Down_Ref] ERROR, ", Ref.SOS.name, " is not found at the apiEndpoint. Correct the name of AirSensEUR or 
                                    set Down.SOS to FALSE in the ASEconfig_xx.R file"), sep = "\n"))
            }
            
            #------------------------------------------------------------------------------CR
            # Downloading sensor data
            #------------------------------------------------------------------------------CR
            # Determining DateIn and DateEND for data download with a lubridate::interval
            DateEND <- RefSOSDateEND
            cat(paste0("[Down_Ref] INFO, last date in Reference data to be downloaded is: ", DateEND, "\n"))
            
            # set DateIN for data retrieving, either from origin or last date in previous DataFrame
            # DownloadSensor exists: check if we have a "DateEND.SOS.prev"
            if (any(grepl(pattern = "DateEND.Ref.prev", x = objects(DownloadSensor)))) { 
                
                # DateEND.Ref.prev exists: Check if NULL
                if (!is.null(DownloadSensor$DateEND.Ref.prev)) {
                    
                    DateIN  <- max(DownloadSensor$DateEND.Ref.prev,
                                   RefSOSDateIN,
                                   na.rm = TRUE
                    )
                    
                } else {
                    
                    # DateEND.Ref.prev is NULL
                    DateIN  <- RefSOSDateIN 
                } 
            } else {
                
                # DateEND.Ref.prev does not exist
                DateIN  <- RefSOSDateIN 
            } 
            cat(paste0("[Down_Ref] INFO, First date in Reference data to be downloaded is: ", DateIN, "\n"))
            
            # Setting end date to curent date
            Duration <- 7 # length of interval to download in days
            
            # Setting original start and end dates
            DateIN.partial  <- DateIN
            # class dates: 1 corresponds to 1 day 
            # class posixct: 24 * 60 * 60 crresponds to one day in second
            if (class(DateIN)[1] == "Date") {
                
                DateEND.partial <- DateIN + 1 * Duration 
                Last.Day        <- DateEND + 1
            } else {
                
                if (class(DateIN)[1] == "POSIXct") {
                    
                    DateEND.partial <- DateIN + 24 * 60 * 60 * Duration 
                    Last.Day        <- lubridate::ceiling_date(as.POSIXct.Date(DateEND), unit = "day")
                } else {
                    
                    DateEND.partial <- DateIN + 24 * 60 * 60 * Duration 
                    Last.Day        <- lubridate::ceiling_date(as.POSIXct.Date(DateEND), unit = "day")
                }
            } 
            
            
            # SOS downloading, added + 1 to be able to downalod current day
            while (DateIN.partial < Last.Day) { 
                
                # interval of time for the get data of SOS
                date.partial <- lubridate::interval(DateIN.partial, DateEND.partial)
                
                for (i in seq(sta)) {
                    
                    # Select the timeseries of the station Ref.SOS.name
                    ts <- sensorweb4R::timeseries(Endpoint, station = sta[i])
                    cat(paste0("Timeseries at the station: ", sensorweb4R::label(ts),"\n"))
                    
                    # fetch all the meta data of ts
                    ts <- sensorweb4R::fetch(ts)
                    
                    # Downloading
                    cat(paste0("[Down_Ref] INFO, downloading from ", DateIN.partial, " to ", DateEND.partial), sep = "\n")
                    Buffer    <- lapply(ts, function(x) {Buffer <- sensorweb4R::getData(x, timespan=date.partial);return(Buffer)})
                    Buffer.df <- data.frame(Buffer[[1]][[1]])
                    
                    # Setting Reference names 
                    for (i in 1:length(Reference.names)) {
                        
                        for (j in 1:length(Reference.names[[i]])) {
                            
                            if (any(label(phenomenon(ts)) == Reference.names[[i]][j])) {
                                colnames(Buffer.df) <- c("date", names(Reference.names)[i])
                                break
                            } 
                        }
                        if (any(label(phenomenon(ts)) == Reference.names[[i]][j])) break
                    }
                    if (exists("Frame")) Frame <- merge(Frame,Buffer.df, by = "date", all = TRUE) else Frame <- Buffer.df
                    
                    # removing to avoid adding the same data for other pollutants
                    if (exists("Buffer"))    rm(Buffer)
                    if (exists("Buffer.df")) rm(Buffer.df)
                }
                
                # Appending downloaded data, no need to discar lines of NA at the end of Ref, since SOS does not return empty lines
                if (exists("Ref")) Ref <- rbind.fill(Ref, Frame) else Ref <- Frame
                if (exists("Frame")) rm(Frame)
                
                # Setting dates for next interval to one duration more
                if (class(DateIN.partial)[1] == "Date") {
                    
                    DateIN.partial  <- DateIN.partial  + 1 * Duration
                    DateEND.partial <- DateEND.partial + 1 * Duration 
                } else {
                    
                    if (class(DateIN.partial)[1] == "POSIXct") {
                        
                        DateIN.partial  <- DateIN.partial  + 24 * 60 * 60 * Duration
                        DateEND.partial <- DateEND.partial + 24 * 60 * 60 * Duration
                    } else {
                        
                        DateIN.partial  <- DateIN.partial  + 24 * 60 * 60 * Duration
                        DateEND.partial <- DateEND.partial + 24 * 60 * 60 * Duration  
                    } 
                }
            }
            # interval of time for the get data of SOS
            date.partial <- lubridate::interval(DateIN.partial, DateEND.partial)
            cat(paste0("[Down_Ref] INFO, Time zone for reference data: ", date.partial@tzone), sep = "\n")
            
        } else if (FTPMode == "a_i_p") {
            
            # Sensor Data retrieving at apiEndpoint
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat(paste0("[Down_Ref] INFO, ", Ref__a_i_p__name," reference data retrieving"), sep = "\n")
            
            # Checking internet connection availability
            if (havingIP()) {
                cat(paste0("[Down_Ref] INFO: There is an internet connection.\n"))
            } else {
                return(cat(paste0("[Down_Ref] ERROR: no internet connection. Download cannot be carried out."), sep = "\n"))
            }
            cat(paste0("[Down_Ref] INFO, downloading from ", Ref__a_i_p__DateIN, " to ", Ref__a_i_p__DateEND), sep = "\n")
            Reference.i <- a_i_p_data(URL          = Ref__a_i_p__name, 
                                      username     = User__a_i_p__, 
                                      password     = Pass__a_i_p__, 
                                      organisation = Ref__a_i_p__Organisation, 
                                      station      = Ref__a_i_p__Station, 
                                      start        = Ref__a_i_p__DateIN, 
                                      end          = Ref__a_i_p__DateEND, 
                                      param        = Ref__a_i_p__Pollutants,
                                      Time_zone    = ref.tzone)
            # Selecting data within date interval
            if (nrow(Reference.i) == 0) {
                
                my_message <- paste0("[Down_Ref] ERROR no data found for reference, lack of new data for ", Reference.name, "\n")
                cat(my_message)
                shinyalert(
                    title = "ERROR no reference data",
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
                
                # Adding coordinates of the reference stations
                if (!is.null(coord.ref)) {
                    # taking coordinates from coord.ref
                    long <- unlist(strsplit(x = coord.ref, split = ","))[1]
                    lat  <- unlist(strsplit(x = coord.ref, split = ","))[2]
                    
                    if (any(grep(pattern = paste0(c("N","S", "E", "W", "d"), collapse = "|" ), x = coord.ref))) {
                        
                        # transform spherical coordinates to decimal degrees for later projection
                        Ref.coord_d    <- OSMscale::degree(lat, long, digits = 5)
                        Reference.i$Ref.Lat   <- Ref.coord_d[1,1]
                        Reference.i$Ref.Long  <- Ref.coord_d[1,2]
                        
                    } else {
                        
                        Reference.i$Ref.Long <- as.numeric(long)
                        Reference.i$Ref.Lat  <- as.numeric(lat)
                    }
                }
                
                # aggregate Reference.i with mean over UserMins
                # use openair function to aggregate to the selected time average, in openair time must be replaced in date
                if (threadr::detect_date_interval(Reference.i$date, skip = 3, n = 50) / 60 != UserMins) {
                    
                    Reference.i <- DF_avg(Reference.i, width = UserMins)
                    
                }  else if (is.POSIXct(DownloadSensor$DateIN.Ref.prev) && is.POSIXct(DownloadSensor$DateEND.Ref.prev) && 
                            !all( as.logical(Reference.i$date[as.Date(Reference.i$date) == as.Date(Reference.i$date)[1]] %in% 
                                             seq(from = DownloadSensor$DateIN.Ref.prev, 
                                                 to   = max(Reference.i$date, na.rm = T), 
                                                 by = paste0(toString(UserMins)," ","min"))) ) ) {
                    
                    Reference.i <- DF_avg(Reference.i, width = UserMins)
                    
                }  else if (!all( as.logical(Reference.i$date[as.Date(Reference.i$date) == as.Date(Reference.i$date)[1]] %in% 
                                             seq(from = as.POSIXct(as.Date(Reference.i$date)[1]), 
                                                 to   = as.POSIXct(as.Date(Reference.i$date)[length(as.Date(Reference.i$date))]), 
                                                 by = paste0(toString(UserMins)," ","min"))) ) ) {
                    Reference.i <- DF_avg(Reference.i, width = UserMins)
                }
                
                # Setting Reference names (change names of pollutants adding Ref.)
                if (Ref.Type == "Ref") {
                    
                    for (i in seq(Reference.names)) {
                        
                        for (j in seq(Reference.names[[i]])) {
                            
                            if (any(names(Reference.i) == Reference.names[[i]][j])) {
                                names(Reference.i)[names(Reference.i) == Reference.names[[i]][j]] <- names(Reference.names)[i]
                                break
                            } 
                        }
                    }
                } else if (Ref.Type %in% c("Bin.DMPS", "Bin.APS", "GRIMM")) {
                    
                    # Adding lable to pollutants which are not in Reference.names
                    names.not.Ref <- names(Reference.i)[grep(pattern = paste(c("date","Ref.", "Bin.DMPS.", "Bin.APS.", "GRIMM."), collapse = "|"), x = names(Reference.i), invert = TRUE)]
                    names(Reference.i)[which(names(Reference.i) %in% names.not.Ref)] <- sapply(seq_along(names.not.Ref), function(k) paste0(Ref.Type, ".", names.not.Ref[k]))
                }
                
                # matching dates,
                Ref <- Reference.i
            }
        }
    } 
    
    # removing NAs to Ref to avoid to add empty lines that will not be updated later
    if (exists("Ref")) {
        Full.Nas <- which(
            apply(as.matrix(Ref[,-which(names(Ref) %in% c("date","Ref.Long","Ref.Lat"))], 
                            ncol = length(names(Ref[,-which(names(Ref) %in% c("date","Ref.Long","Ref.Lat"))]))), 
                  MARGIN = 1, 
                  function(x) all(is.na(x) || is.nan(x))
            )
        )
        
        if (!is.null(Full.Nas) & length(Full.Nas) > 0 ) Ref <- Ref[-Full.Nas,]
        
        print(str(Ref), Quote = FALSE)
        
    }
    
    if (exists("Ref")) {
        
        if (length(names(Ref)) > 0 ) {
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            return(Ref)        
        } else {
            
            # removing Ref when there are no sensor data
            remove(Ref)
            cat("[Down_ref] INFO, there is no new data for the reference station",sep = "\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            return()  
        } 
    } else {
        
        # removing Ref when there are no sensor data
        remove(Ref)
        cat("[Down_ref] ERROR, there is no new data for the reference station",sep = "\n")
        cat("-----------------------------------------------------------------------------------\n")
        cat("\n")
        return()
    }  
}

#=====================================================================================CR
# 170609 MG : Pinging WEB site
#=====================================================================================CR
PingThisSite <- function(test.site) {
    # this function returns TRUE if it is possible to ping a test.site
    # test.site                     the URL whose existence we are to test
    
    if (!require(RCurl)) {
        # RCurl needs to be installed, checking if internet is available
        install.packages("RCurl")
    }
    require(RCurl)
    url.exists(test.site)
}

#=====================================================================================CR
# 170609 MG : Pinging WEB site
#=====================================================================================CR
ping <- function(x, stderr = FALSE, stdout = FALSE, ...) {
    pingvec <- system2("ping", x,
                       stderr = FALSE,
                       stdout = FALSE,...)
    if (pingvec == 0) TRUE else FALSE
}

#=====================================================================================CR
# 170609 MG : Pinging WEB site
#=====================================================================================CR
havingIP <- function() {
    
    if (.Platform$OS.type == "windows") {
        ipmessage <- system("ipconfig", intern = TRUE)
    } else {
        ipmessage <- system("/sbin/ifconfig", intern = TRUE)
    }
    
    # validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]) {3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
    validIP <- "(?<=[^0-9.]|^)[1-9][0-9]{0,2}([.]([0-9]{0,3})){3}(?=[^0-9.]|$)"
    
    return(any(unlist(gregexpr( validIP, ipmessage, perl = TRUE) ) != -1))
}

#=====================================================================================CR
# 160418 MGV: Validation.tool       function for validations
#=====================================================================================CR
Tidy_Model.i <- function(Model.i)
{
    # https://stackoverflow.com/questions/24559099/vectorize-environment-access-in-r
    ENV <- new.env()
    ENV[["Model"]] <- list(Tidy = tidy(Model.i), Augment = augment(Model.i), Glance = glance(Model.i), Call = Model.i$call, Coef = coef(Model.i))
    return(ENV[["Model"]])
}

Validation.tool <- function(  General, DateIN, DateEND, DateINCal = NULL, DateENDCal = NULL, name.gas, model.log, nameGasRef, nameGasVolt, nameGasMod, 
                              unit.ref, unit.sensor, Sens.raw.unit = NULL, Reference.name, AirsensEur.name, name.sensor, 
                              timeseries.display, DateINPlot = DateIN, DateENDPlot = DateEND, 
                              WDoutputMod, WDoutput, WDoutputStats, 
                              process.step, mod.eta.model.type, Multi.File = NULL, 
                              eta.model.type, remove.neg = TRUE, Covariates = NULL, PlotCal = TRUE, Auto.Lag = FALSE) {
    #INput: 
    #  General              : dataframe- containing all data within selected dates
    #  DateIN/END           : as.POSIXct- datetime in and datetime out to start validation 
    #  DateINCal/DateENDCal : Dates of previous calibration with which nameGasMod was calibrated
    #  name.gas             : char() - gas component
    #  model.log            : logic  - If true calibration nA/V to ppm/ppb is perfomed
    #  nameGasRef           : char   - column of gas reference data
    #  nameGasVolt          : char   - column of gas gas sensor data in volt or nA
    #  nameGasMod           : char   - column of gas gas sensor data in same unit as reference
    #  unit.ref/sensor      : char   - units (ppb or ppm)
    #  Sens.raw.unit        : raw unit of sensors: V or nA
    #  Reference.name       : char   - name of reference data
    #  AirsensEur.name      : char   - name of airsensor data
    #  timeseries.display   : logic  - True -> displays timeseries  
    #  DateINPlot/END       : as.POSIXct- datetime in and datetime out to plot time series 
    #  name.sensor          : char   - name of specific gas sensor
    #  WDoutputMod          : char   - directory to save computed models (not used if (!model.log))
    #  WDoutput             : char   - directory to save plots from etalonnage
    #  WDoutputStats        : char   - directory to save statistics for modelled data
    #  process.step         : char   - variable to refer to the process status (e.g. calibration, modelling)
    #  mod.eta.model.type   : char   - name for model in Etalonnage and Cal_line
    #  Multi.File           : char, defualt is NULL, path.file of the config file used for calibration with multivariates
    #  eta.model.type       : char   - name for evaluation in Etalonnage and Cal_line
    #  Covariates           : List of covariates to calibrate, vector of characters, default is NULL
    #  remove.neg           : logical, defaut TRUE, if TRUE discard negative from calibrated data after calibration
    #  PlotCal              : logical, defaut TRUE, if TRUE plot the calibrated data (scatterplot and timeseries) after calibration
    #  Auto.Lag             : logical, default is FaLSE If Auto.Lag is TRUE, y is changed using the lag at which cross correlation between x and y is maximum using ccf( )
    
    # OUTPUT:
    # General               : with modelled values
    # models 
    # plots 
    # statistics
    
    # determining number pf plots
    op <- par(no.readonly = TRUE)
    if (model.log) {
        if (timeseries.display) {par(mfrow = c(1,2))} else {par(mfrow = c(1,1))}
    } else {
        if (timeseries.display) {par(mfrow = c(1,2))} else {par(mfrow = c(1,1))}
    }
    # Restoring graphical parameters on exit of function, even if an error occurs
    on.exit(par(op))
    
    if (model.log) {
        # timeplots uncalibrated values
        if (timeseries.display) {
            Relationships         <- na.omit(colnames(General)[colnames(General) %in% Covariates_i])
            if (!is.null(Relationships)) 
                timePlot(mydata = General[ date >= DateINPlot & date <= DateENDPlot,], pollutant = Relationships, date.pad = TRUE, auto.text = FALSE, y.relation = "free",
                         main = paste0(AirsensEur.name, ": Effects on ", name.sensor," from ",format(DateIN,"%d-%b-%y")," to ",format(DateEND,"%d-%b-%y"),
                                       " at ",Reference.name)) 
            # save plots in files
            dev.copy(png, filename = file.path(WDoutput,paste0(AirsensEur.name,"_",name.sensor,"_Effects", 
                                                               "_Timeserie_",format(DateINPlot,"%Y-%m-%d"),"_",format(DateENDPlot,"%Y-%m-%d"),".png")), 
                     units = "cm", 
                     res = 300, 
                     width = 35.5, 
                     height = 25)
            dev.off() 
        }
        
        # Setting axis and labels
        if (mod.eta.model.type == "gam") {# General additive model
            y = General[[nameGasRef]][General[["date"]] >= DateIN & General[["date"]] <= DateEND]
            x = General[[nameGasVolt]][General[["date"]] >= DateIN & General[["date"]] <= DateEND]
            AxisLabelY = paste0(nameGasRef," ", unit.ref, " ",Reference.name)
            AxisLabelX = paste0(nameGasVolt," in ", Sens.raw.unit," ", AirsensEur.name)
        } else {
            x = General[[nameGasRef]][General[["date"]] >= DateIN & General[["date"]] <= DateEND]
            y = General[[nameGasVolt]][General[["date"]] >= DateIN & General[["date"]] <= DateEND]
            AxisLabelX = paste0(nameGasRef," ", unit.ref, " ",Reference.name)
            AxisLabelY = paste0(nameGasVolt," in ", Sens.raw.unit," ", AirsensEur.name)
        } 
        
        EtalLim <- Etalonnage( x = x, s_x = NULL, y = y , s_y = NULL
                               , AxisLabelX = AxisLabelX
                               , AxisLabelY = AxisLabelY 
                               , Title = paste0(AirsensEur.name, ": ",process.step," ", name.sensor," from ",format(DateIN,"%d-%b-%y")," to ",format(DateEND,"%d-%b-%y"), " at ",Reference.name)                        
                               , Marker = 1, Couleur = "blue", ligne = 'p', XY_same = FALSE, lim = NULL, steps = c(10,10)
                               , digitround = c(3,3), marges = c(4,4,3,0.5))
        
        if (mod.eta.model.type == "MultiLinear") {
            Matrice         <- General[date >= DateINPlot & date <= DateENDPlot, .SD, .SDcols = Covariates]
            names(Matrice)  <- Covariates
            
            if (!is.null(Multi.File)) {
                
                if (file.exists(Multi.File)) {
                    
                    # read Multi.File
                    Multi.File.df <-  read.table(file             = Multi.File, 
                                                 header           = TRUE, 
                                                 row.names        = NULL, 
                                                 comment.char     = "#"
                                                 ,stringsAsFactors = FALSE
                    )
                    
                    # add covariate degrees of polynomial
                    Degrees <-  Multi.File.df[Multi.File.df$Covariates %in% Covariates, "degree"]
                    
                } else {
                    
                    # degree of polynomial set to 1 
                    Degrees <-  base::rep(1, times = length(Covariates) )
                } 
            } else {
                
                # degree of polynomial set to 1 
                Degrees <-  base::rep(1, times = length(Covariates) )
            } 
            
            namesCovariates <- paste0(paste(Covariates,Degrees, sep = "-"),collapse = "&")
            
        } else if (any(mod.eta.model.type %in% c("exp_kT", "exp_kK", "T_power", "K_power"))) {
            namesCovariates <- "Temperature"
            Matrice         <- General[date >= DateINPlot & date <= DateENDPlot, .SD, .SDcols = Covariates]
            names(Matrice)  <- namesCovariates
        } else {
            namesCovariates <- ""
            Matrice         <- NULL
        } 
        
        Model.i <- Cal_Line(x = x, s_x = NULL, 
                            y = y, s_y = NULL, 
                            Mod_type      = mod.eta.model.type,
                            Multi.File    = Multi.File,
                            Covariates    = Covariates,
                            Matrice       = Matrice, 
                            line_position = 0, 
                            Couleur       = "red", 
                            Sensor_name   = name.sensor, 
                            f_coef1       = "%.3e", f_coef2 = "%.3e", f_R2 = "%.4f", 
                            lim           = EtalLim, 
                            marges        = NULL,
                            Weighted      = FALSE,
                            Lag_interval  = sqrt((max(x, na.rm = T) - min(x, na.rm = T))) / (length(Covariates) + 1),
                            Auto.Lag      = Auto.Lag)
        
        # saving the model
        nameModel  <- paste(AirsensEur.name,name.sensor,Sens.raw.unit,mod.eta.model.type,format(DateIN,"%Y%m%d"),format(DateEND,"%Y%m%d"),namesCovariates, sep = "__")
        Model.i <- Tidy_Model.i(Model.i)
        # https://stackoverflow.com/questions/42230920/saverds-inflating-size-of-object
        list.save(unserialize(serialize(Model.i,NULL),NULL), file = file.path(WDoutputMod, paste0(nameModel,".rdata")))
        
        # save scatterplots in files
        NameFile <- file.path(WDoutput, paste0(nameModel,"__",process.step,".png"))
        dev.copy(png, 
                 filename = NameFile, 
                 units = "cm", 
                 res = 300, 
                 width = 20, 
                 height = 20
        )
        dev.off()  
    }
    
    if (PlotCal) {
        
        # Fill in General with modelled data
        General[, ..nameGasMod ] <- NULL # General[ (date >= DateIN & date <= DateEND ), ..nameGasMod ] <-NA
        
        if (mod.eta.model.type == "Linear"| mod.eta.model.type == "Linear.Robust" ) {
            General[date >= DateIN  & date <= DateEND & !is.na(General[, nameGasVolt]), nameGasMod] <-
                (General[ date >= DateIN  & date <= DateEND & !is.na(General[, nameGasVolt]), nameGasVolt]- coef(Model.i)[1]) /  coef(Model.i)[2]
        }
        if (mod.eta.model.type == "gam") {
            General[ General$date >= DateIN  & General$date <= DateEND  & !is.na(General[, nameGasVolt]), nameGasMod] <-
                predict(Model.i, newdata = data.frame(x = General[ General$date >= DateIN  & General$date <= DateEND  & !is.na(General[, nameGasVolt]), nameGasVolt])
                        , type = "response")
        }
        
        # Remove negative values when using the linear median regression (linear.robust). This is good to do
        if (remove.neg) {
            index.which <- which(General[(General$date >= DateIN & General$date <= DateEND ),nameGasMod] < 0., arr.ind = TRUE)
            if (length(index.which)>0) {
                General[(General$date >= DateIN & General$date <= DateEND ),nameGasMod][index.which] <- NA
                cat(paste0("Length of values < zero: ", length(index.which), " out of ",length(General[(General$date >= DateIN & General$date <= DateEND ),nameGasMod])), sep = "\n")
                cat(paste0("Loss of ",format(length(index.which)/length(General[(General$date >= DateIN & General$date <= DateEND ),nameGasMod])*100), digit = 0, " [%] of data"), sep = "\n")  
            } else {}
        } else {}
        # plotting the modelled/calibrated values
        EtalLim <- Etalonnage( x = General[,nameGasRef][ (General$date >= DateIN & General$date <= DateEND)]
                               , s_x = NULL
                               , y = General[,nameGasMod][ (General$date >= DateIN & General$date <= DateEND)]
                               , s_y = NULL
                               , AxisLabelX = paste0(nameGasRef," ",unit.ref," ",Reference.name)
                               , AxisLabelY = paste0(nameGasMod," ", unit.sensor, " ", AirsensEur.name) 
                               , Title = paste0(AirsensEur.name, ": Calibrated ", name.sensor," from ",format(DateIN,"%d-%b-%y")," to ",format(DateEND,"%d-%b-%y"), 
                                                " at ",Reference.name) 
                               , Marker = 1, Couleur = "blue", ligne = 'p', XY_same = TRUE, lim = NULL, steps = c(10,10)
                               , digitround = c(2,3), marges = c(4,4,3,0.5))
        Cal_Line(x = General[,nameGasRef][ (General$date >= DateIN & General$date <= DateEND)]
                 , s_x = NULL
                 ,y = General[,nameGasMod][ (General$date >= DateIN & General$date <= DateEND)]
                 , s_y = NULL
                 , Mod_type = eta.model.type, Matrice = General, line_position = 0, Couleur = "red", Sensor_name = "", 
                 f_coef1 = "%.3e", f_coef2 = "%.3e", f_R2 = "%.4f", lim = EtalLim, marges = NULL, Covariates = NULL)         
        
        
        # save scatterplots in files
        if (is.null(DateINCal) & is.null(DateENDCal)) { chaine <- paste0(mod.eta.model.type, "_", format(DateIN,"%Y%m%d"),"_",format(DateEND,"%Y%m%d"))
        }  else { chaine <- paste0(mod.eta.model.type, "_", format(DateINCal,"%Y%m%d"),"_",format(DateENDCal,"%Y%m%d"))    }
        if (remove.neg) negatif <- "_remove.neg_" else negatif <- "_"
        dev.copy(png,filename = file.path(WDoutput, paste0(AirsensEur.name,"_",name.sensor,"_","Calibrated", "_",eta.model.type,negatif,format(DateIN,"%Y%m%d"),
                                                           "_",format(DateEND,"%Y%m%d"),"_", chaine,".png"))
                 , units = "cm", res = 300, width = 25, height = 25)
        
        dev.off()
        
        # timeplots calibrated values
        if (timeseries.display) {
            if (model.log) {
                timePlot(General[ General$date >= DateINPlot & General$date <= DateENDPlot,], pollutant = c(nameGasRef,nameGasMod), group=TRUE, date.pad=TRUE
                         , main = paste0(AirsensEur.name, ": Calibrated ", name.sensor," from ",format(DateIN,"%d-%b-%y")," to "
                                         , format(DateEND,"%d-%b-%y"), " at ",Reference.name))
            } else {
                timePlot(General[ General$date >= DateINPlot & General$date <= DateENDPlot,], pollutant = c(nameGasRef,nameGasMod), group=TRUE, date.pad=TRUE
                         , main = paste0(AirsensEur.name, ": Calibrated ", name.sensor," from ",format(DateIN,"%d-%b-%y")," to "
                                         , format(DateEND,"%d-%b-%y"), " at ",Reference.name)
                         , ref.x = list(v = c(DateINCal, DateENDCal), lty = c(1, 1), col = c("black", "black"), lwd = c(2,2)))
                
            }
            
            # save plots in files
            NameFile <- file.path(WDoutput,paste0(AirsensEur.name,"_",name.sensor,"_","Modelled",negatif,
                                                  "_TS_",format(DateINPlot,"%Y%m%d"),"_",format(DateENDPlot,"%Y%m%d"),"_",chaine,".png"))
            dev.copy(png, filename = NameFile, units = "cm", res = 300, width = 35.5, height = 20);
            dev.off() 
        }
        
        # statistics    
        gas.statistics <-modStats(General[ (General$date >= DateIN & General$date <= DateEND),]
                                  , mod = nameGasMod
                                  , obs = nameGasRef
                                  , statistic = c("n", "FAC2", "MB", "MGE", "NMB", "NMGE", "RMSE", "r", "COE", "IOA")
                                  , type = "default" 
                                  , rank.name = NULL)
        print(paste0("Statistics results",AirsensEur.name, name.sensor,process.step,eta.model.type, sep= "_"))
        print(summary(gas.statistics))
        saveRDS(gas.statistics , 
                file = file.path(object = WDoutputStats, 
                                 file   = paste0(AirsensEur.name, "_", name.sensor, "_",process.step, "_",Sens.raw.unit, "_","Stats",eta.model.type, "_",
                                                 format(DateIN,"%Y%m%d"),"_",format(DateEND,"%Y%m%d"),".rds") )  )
        
        # Warnings 
        stats.thres <- 0.5 # thresholds to nitify for new calibration to be performed
        
        if (gas.statistics$r^2 <stats.thres & gas.statistics$IOA < stats.thres) {
            cat(paste0("***** Warning message*****"," R2 and IOA are < stats.thres for ", name.sensor," in ", process.step))
        }
    }
    
    return(General)
}

#=====================================================================================CR
# 170609 MG : Selecting a fine name in window
#=====================================================================================CR
SearchFile <- function(dirCurrent = getwd(), Caption = "", Filters = matrix(c("ASEConfig", ".R"),1, 2, byrow = TRUE)) {
    # This function selects and return a file name in a window. It is compatible with Windows and Linux
    # The operating system is automatically detected.
    # 
    # dirCurrent        : directory in which the window will list the available file
    # Filters           : a matrix of filename filters 
    # return            : a file.path to the selected file
    # dependences       : Load.Packages
    
    # detectiong the OS
    isOS <- .Platform$OS.type 
    cat(paste0("[SearchFile] INFO: the OS is : ", isOS), sep = "\n") 
    
    if (isOS == "windows") {
        FilePath <- choose.files(default = dirCurrent, caption = Caption,
                                 multi = FALSE, filters = Filters,
                                 index = nrow(Filters)) 
    } else { 
        # Checking tcltk capability
        isTcltk <- capabilities("tcltk")
        if (isTcltk) {
            cat("[SearchFile] INFO: the OS is able to run tcltk\n")
        } else {
            cat("[SearchFile] INFO: the OS is not able to run tcltk\n") 
        }  
        
        # using tcltk not depending on the platform Windows or Linux
        if (isTcltk) {
            Load.Packages("tcltk")
            FilePath <- tk_choose.files(default = file.path(dirCurrent,"ASEconfig*.R"), caption = Caption,
                                        multi = FALSE, filters = Filters,
                                        index = nrow(Filters)) 
        } else {
            # try selecting with JAVA jchoose.files()
            Load.Packages("rJava", "rChoiceDialogs")
            FilePath <- jchoose.files(default = file.path(dirCurrent,"ASEconfig*.R"), caption = Caption,
                                      multi = FALSE, filters = Filters,
                                      index = nrow(Filters), modal = canUseJavaModal())
        }
    }
    return(FilePath)
}

#=====================================================================================CR
# 170609 MG : dirCurrent, returning the Current directory of a script
#=====================================================================================CR
CurrentDir <- function() {
    # This function returns the directory from where the current R script is run.
    # It uses two methods according if Rstudio is used or not.
    
    # Checking if RStudio is used
    isRStudio  <- Sys.getenv("RSTUDIO") == "1"
    if (isRStudio) cat("[CurrentDir] INFO: ASE_Script is run under Rstudio\n") 
    
    # Getting the current scritp
    if (isRStudio) {
        # checking if rstudioapi is loaded
        Load.Packages("rstudioapi")
        # get the direcctory of the ASE_Script using RStudio
        dirCurrent <- dirname(rstudioapi::getActiveDocumentContext()$path)
    }  else {
        # get the direcctory of the ASE_Script without using RStudio
        dirCurrent <- getSrcDirectory(function(x) {x})
    }
    cat(paste0("[CurrentDir] INFO: the curent directory of execution of the script is ", dirCurrent), sep = "\n")
    return(dirCurrent)
}

#=====================================================================================CR
# 170609 MG : Looking for the sensor config file
#=====================================================================================CR
ASEPanel04File <- function(dirCurrent = getwd(), JAVAPanel = c("*.asc")) {
    # This function returns the config file of the AirSensEURPanel version 0.4. It looks for the directory () of JAVAPanel
    # first in the current directory(dirCurrent) then in the directory from which the scritp is run 
    # and finally up to the directory of the root disk.
    # It is then asked in a select window to select the config file (*.ASC) use in AirSensEURPanel for the current AirSensEUR device
    # if the directory of JAVAPanel was not found, it is asked to the user to manually select the directory and config file in a window.
    # 
    # dirCurrent        : path from where to look for JAVAPanel
    # JAVAPanel         : Name of the JAVA AirSensEURPanel, the deaulft is AirSensEURPanel.jar 
    # return            : a file.path where is c("AirSensEURPanel.jar") SensorConfig with all sensor parameters of a AirSensEURPanel config file
    # dependences       : Load.Packages
    
    # init
    FoundASEPanel04Dir <- FALSE
    initDir <- getwd()
    
    # Searching in the current directory or the Directory passed to ASEPanel04File and subdirectories
    ListDirs     <- list.files(path = dirCurrent, recursive = TRUE, full.names = TRUE)
    if (!identical(ListDirs, character(0))) {
        if (any(file.exists(file.path(ListDirs, JAVAPanel)))) {    
            FoundASEPanel04Dir <- TRUE
            ASEPanel04Dir <- ListDirs[which(file.exists(file.path(ListDirs, JAVAPanel)))][1]
            cat(paste0("[ASEPanel04File] INFO; the AirSensEURPanel.jar is found in ",ASEPanel04Dir), sep = "\n")
        } else {
            # Searching in the directory where the script is run
            Load.Packages(c("kimisc", "knitr")) # for thisfile()
            
            if (!is.null(thisfile())) {
                setwd(dirname(thisfile()))
                dirCurrent   <- dirname(thisfile())
                ListJAVA     <- list.files(path = getwd(), pattern = JAVAPanel, recursive = TRUE, full.names = TRUE, 
                                           all.files = FALSE, ignore.case = FALSE)
                dirCurrentParent <- ""
                while (identical(ListJAVA, character(0)) & getwd() != dirCurrentParent) {
                    # going to one higher level of the Directory three
                    ListJAVA     <- list.files(path = getwd(), pattern = JAVAPanel, recursive = TRUE, full.names = TRUE, 
                                               all.files = FALSE, ignore.case = FALSE)
                    cat(paste0("[ASE_script] INFO: searching ", JAVAPanel, " in ", getwd() ), sep = "\n")
                    dirCurrentParent <- getwd()
                    setwd("..")
                }
                
                if (!identical(ListJAVA, character(0))) {
                    # JAVAPAnel is found
                    FoundASEPanel04Dir <- TRUE
                    ASEPanel04Dir <- dirname(ListJAVA[1])
                    cat(paste0("[ASEPanel04File] INFO: ", JAVAPanel, " is found in ",ASEPanel04Dir), sep = "\n")
                }
            }
        }
    }
    
    if (FoundASEPanel04Dir) {
        ASEPanel04File <- tk_choose.files(default = ASEPanel04Dir, caption = paste0("Select the Sensor Configuration File used with ", JAVAPanel), 
                                          filters = matrix(c("", ".ASC"),1, 2, byrow = TRUE))
    } else {
        ASEPanel04File <- jchoose.files(default = file.path(dirCurrent,"*.asc"), caption = paste0("Select the Sensor Configuration File used with ", JAVAPanel),
                                        multi = FALSE, filters = matrix(c("", ".ASC"),1, 2, byrow = TRUE),
                                        index = nrow(Filters), modal = canUseJavaModal())
        # tk_choose.files(default = dirCurrent, caption = paste0("Select the Sensor Configuration File used with ", JAVAPanel), 
        #                            filters = matrix(c("", ".ASC"),1, 2, byrow = TRUE))
    }
    
    #returining to the initial Directory
    setwd(initDir)
    
    return(ASEPanel04File)
}

#=====================================================================================CR
# 170609 MG : Reading the sensor config file
#=====================================================================================CR
ASEPanel04Read <- function(ASEPanel04File = NULL , dirASEPanel = c("AirSensEURPanelR04"), dirCurrent = getwd()) {
    # This function read the config parameters of the AirSensEURPanel version 0.4 - All values are in Hexadecimal and need be converted
    # ASEPanel04File    : the filepath to be read, character vector
    # return            : a dataFrame SensorConfig with all sensor parameters of a AirSensEURPanel config file
    # dependences       : Load.Packages
    # 
    # Returns a data frame with sensor config parameters
    
    ASEFile <- read.table(file = ASEPanel04File, header = FALSE, sep = ":", stringsAsFactors = FALSE)
    
    # dataFrame of sensor config
    # typical order of sensors, that may be changed after reading the the shield config file
    name.gas              <- c("NO2", "CO" , "O3",  "NO" ) 
    sens2ref                <- data.frame(name.gas          = name.gas,
                                          Ref               = c(-999,-999,-999,-999),
                                          RefAD             = c(-999,-999,-999,-999),
                                          RefAFE            = c(-999,-999,-999,-999),
                                          check.names       = FALSE, 
                                          stringsAsFactors  = FALSE)
    
    # load packages for alphanumeric operations
    Load.Packages("BMS")
    require(BMS)
    
    for (i in 1:4) {
        # Name of Sensor 
        Command <- ASEFile[which(ASEFile[,2] == paste0("Write Preset Name for channel ",i-1))[1], 1]
        # Discarding curly bracket at the begining and end of the string under linux
        Command <- gsub("[{}]", "", Command)
        # discarding W at the begining and end
        Command <- substring(Command,2, nchar(Command))
        hCommand <- sapply(seq(1, nchar(Command), by=2), function(x) substr(Command, x, x+1))
        # discarding "00" trayling elements
        if (hCommand[1]                == "00") hCommand <- hCommand[-1]
        if (hCommand[length(hCommand)] == "00") hCommand <- hCommand[-length(hCommand)]
        # https://stackoverflow.com/questions/29251934/how-to-convert-a-hex-string-to-text-in-r
        sens2ref$name.sensor[i] <- gsub('[^[:print:]]+', '', rawToChar(as.raw(strtoi(hCommand, 16L)))) 
        
        # LMP9100 Register 
        Command <- ASEFile[which(ASEFile[,2] == paste0("LMP9100 Register Setup for channel ",i-1))[1], 1]
        # Discarding curly bracket at the begining and end of the string under linux
        Command <- gsub("[{}]", "", Command)
        # discarding R at the begining 
        Command <- substring(Command,2, nchar(Command))
        hCommand <- sapply(seq(1, nchar(Command), by=2), function(x) substr(Command, x, x+1))
        #sens2ref$TIA[i]    <- hCommand[2] not useful and confusing
        # The bits seem to be in opposite order as written in the datasheet
        if (all(hex2bin(hCommand[2])[7:8] == c(0,0)))     sens2ref$Rload[i]     <- 10
        if (all(hex2bin(hCommand[2])[7:8] == c(0,1)))     sens2ref$Rload[i]     <- 33
        if (all(hex2bin(hCommand[2])[7:8] == c(1,0)))     sens2ref$Rload[i]     <- 50
        if (all(hex2bin(hCommand[2])[7:8] == c(1,1)))     sens2ref$Rload[i]     <- 100
        if (all(hex2bin(hCommand[2])[4:6] == c(0,0,0)))   sens2ref$TIA_Gain[i]  <- 1000000
        if (all(hex2bin(hCommand[2])[4:6] == c(0,0,1)))   sens2ref$TIA_Gain[i]  <- 2750
        if (all(hex2bin(hCommand[2])[4:6] == c(0,1,0)))   sens2ref$TIA_Gain[i]  <- 3500
        if (all(hex2bin(hCommand[2])[4:6] == c(0,1,1)))   sens2ref$TIA_Gain[i]  <- 7000
        if (all(hex2bin(hCommand[2])[4:6] == c(1,0,0)))   sens2ref$TIA_Gain[i]  <- 14000
        if (all(hex2bin(hCommand[2])[4:6] == c(1,0,1)))   sens2ref$TIA_Gain[i]  <- 35000
        if (all(hex2bin(hCommand[2])[4:6] == c(1,1,0)))   sens2ref$TIA_Gain[i]  <- 120000
        if (all(hex2bin(hCommand[2])[4:6] == c(1,1,1)))   sens2ref$TIA_Gain[i]  <- 350000
        sens2ref$GAIN[i] <- 1 + (sens2ref$TIA_Gain[i]/sens2ref$Rload[i])
        
        #sens2ref$REF[i] <- hCommand[3] not useful and confusing
        # The bits seem to be in opposite order as written in the datasheet
        if (all(hex2bin(hCommand[3])[1]   == c(0)))       sens2ref$Ref_Source[i]  <- "Internal"
        if (all(hex2bin(hCommand[3])[1]   == c(1)))       sens2ref$Ref_Source[i]  <- "External"
        if (all(hex2bin(hCommand[3])[2:3] == c(0,0)))     sens2ref$Int_Z[i]       <- 0.20
        if (all(hex2bin(hCommand[3])[2:3] == c(0,1)))     sens2ref$Int_Z[i]       <- 0.50
        if (all(hex2bin(hCommand[3])[2:3] == c(1,0)))     sens2ref$Int_Z[i]       <- 0.67
        if (all(hex2bin(hCommand[3])[2:3] == c(1,1)))     sens2ref$Int_Z[i]       <- 11
        if (all(hex2bin(hCommand[3])[4]   == c(0)))       sens2ref$Bias_Sign[i]   <- -1
        if (all(hex2bin(hCommand[3])[4]   == c(1)))       sens2ref$Bias_Sign[i]   <- 1
        if (all(hex2bin(hCommand[3])[5:8] == c(0,0,0,0))) sens2ref$Bias[i]   <- 0
        if (all(hex2bin(hCommand[3])[5:8] == c(0,0,0,1))) sens2ref$Bias[i]   <- 0.01
        if (all(hex2bin(hCommand[3])[5:8] == c(0,0,1,0))) sens2ref$Bias[i]   <- 0.02
        if (all(hex2bin(hCommand[3])[5:8] == c(0,0,1,1))) sens2ref$Bias[i]   <- 0.04
        if (all(hex2bin(hCommand[3])[5:8] == c(0,1,0,0))) sens2ref$Bias[i]   <- 0.06
        if (all(hex2bin(hCommand[3])[5:8] == c(0,1,0,1))) sens2ref$Bias[i]   <- 0.08
        if (all(hex2bin(hCommand[3])[5:8] == c(0,1,1,0))) sens2ref$Bias[i]   <- 0.10
        if (all(hex2bin(hCommand[3])[5:8] == c(0,1,1,1))) sens2ref$Bias[i]   <- 0.12
        if (all(hex2bin(hCommand[3])[5:8] == c(1,0,0,0))) sens2ref$Bias[i]   <- 0.14
        if (all(hex2bin(hCommand[3])[5:8] == c(1,0,0,1))) sens2ref$Bias[i]   <- 0.16
        if (all(hex2bin(hCommand[3])[5:8] == c(1,0,1,0))) sens2ref$Bias[i]   <- 0.18
        if (all(hex2bin(hCommand[3])[5:8] == c(1,0,1,1))) sens2ref$Bias[i]   <- 0.20
        if (all(hex2bin(hCommand[3])[5:8] == c(1,1,0,0))) sens2ref$Bias[i]   <- 0.22
        if (all(hex2bin(hCommand[3])[5:8] == c(1,1,0,1))) sens2ref$Bias[i]   <- 0.24
        if (all(hex2bin(hCommand[3])[5:8] == c(1,1,1,0))) sens2ref$Bias[i]   <- 0
        if (all(hex2bin(hCommand[3])[5:8] == c(1,1,1,1))) sens2ref$Bias[i]   <- 0
        
        if (substring(hCommand[4],1,1) ==0) sens2ref$Fet_Short[i] <- "Disabled"
        if (substring(hCommand[4],1,1) ==1) sens2ref$Fet_Short[i] <- "Enabled"
        sens2ref$Mode[i]      <- substring(hCommand[4],2,2)
        
        # DAC5694R Register 
        for (j in 1:3) {
            Command <- ASEFile[which(ASEFile[,2] == paste0("DAC5694R Register Setup for channel ",i-1," subchannel ",j-1))[1], 1]
            # Discarding curly bracket at the begining and end of the string under linux
            Command <- gsub("[{}]", "", Command)
            # discarding W atthe begining
            Command <- substring(Command,2, nchar(Command))
            # Discarding curly bracket at the begining and end of the string under linux
            Command <- gsub("[{}]", "", Command)
            hCommand <- sapply(seq(1, nchar(Command), by=2), function(x) substr(Command, x, x+1))
            if (hCommand[5] == "00") Vtotal = 2.5 else Vtotal = 5 
            if (j==1) sens2ref$Ref[i]     <- as.numeric(strtoi(paste0(hCommand[3],hCommand[4]), base = 16L)/4095 * Vtotal)
            if (j==2) sens2ref$RefAD[i]   <- as.numeric(strtoi(paste0(hCommand[3],hCommand[4]), base = 16L)/4095 * Vtotal)
            if (j==3) sens2ref$RefAFE[i]  <- as.numeric(strtoi(paste0(hCommand[3],hCommand[4]), base = 16L)/4095 * Vtotal)
        }
        # Calculate the zero signal and Bias voltage
        sens2ref$board.zero.set[i] <- sens2ref$Int_Z[i] * sens2ref$RefAFE[i]
        sens2ref$BIAIS[i]          <- sens2ref$Bias_Sign[i] * sens2ref$Bias[i] * sens2ref$RefAFE[i]
    }
    
    # Find correct name of compounds according to the name of sensor
    name.gas <- list(NO2 = c("NO2", "no2_b43f","NO2-B43F", "NO2B43F", "NO2/C-20"                                  , "NO23E50", "NO2_3E50"         , "S1"),
                     CO  = c("CO" , "CO-B4", "CO-A4","COA4"         , "CO/MF-200", "CO/MF-20", "COMF200"          , "CO3E300", "CO_3E300"         , "S2"),
                     O3  = c("O3" , "O3-A431","OX_A431", "O3-B4", "OX-A431", "AX-A431", "O3/M-5", "o3_m_5","O3_M5", "O33EF1" , "O3E100" ,"O3_3E1F", "S3"),
                     NO  = c("NO" , "NOB4_P1","NOB4","NO-B4"        , "NO/C-25"                                   , "NO3E100", "NO_3E100"         , "S4")
    ) # Add new sensor model to be recognized if needed
    # Finding the sensor model 
    for (i in 1:length(sens2ref$name.gas)) for (j in 1:length(name.gas)) {
        if (any(sens2ref$name.sensor[i] %in% name.gas[[j]])) {sens2ref$name.gas[i] <- names(name.gas)[j]; break()}
    } 
    # Adding gas.sensor for use with SOS protocol
    gas.sensor.df <- data.frame(Nitrogen_dioxide  = c("NO2"),
                                Carbon_monoxide   = c("CO"),
                                Ozone             = c("O3"),
                                Nitric_oxide      = c("NO"), # this name is wrong but Alex used it in SOS
                                stringsAsFactors  = FALSE
    ) # Add new compound to be recognized if needed
    for (i in 1:length(sens2ref$name.gas)) sens2ref[i,"gas.sensor"] <- names(gas.sensor.df)[which(gas.sensor.df[1,]==sens2ref$name.gas[i])]
    
    # reordering as c("NO2","CO" , "O3",  "NO")
    # sens2ref <- cbind(sens2ref[,which(names(sens2ref) == "NO2")],
    #                   sens2ref[,which(names(sens2ref) == "CO")],
    #                   sens2ref[,which(names(sens2ref) == "O3")],
    #                   sens2ref[,which(names(sens2ref) == "NO")])
    
    #print(sens2ref, quote = FALSE)
    return(sens2ref)
}

#=====================================================================================CR
# 170619 MG : Showing the sensor configuration in a window
#=====================================================================================CR
ShowConf <- function(mat1) {
    # This fucntion is to create one window to show a matrix of data sensor configuration
    
    Load.Packages("tcltk2")
    
    # A simple matrix in R
    
    # Data must be transferred one item at a time to the tclArray object
    # Also note that Tcl indexes start from 0, while they start from 1 in R
    # and that without the strsplit() hack, strings with spaces are displayed
    # as {string wuth spaces} in Tk Table
    tclTable <- tclArray()
    for (i in 1:nrow(mat1))
        for (j in 1:ncol(mat1))
            tclTable[[i-1, j-1]] <- strsplit(mat1[i, j], " ", fixed = TRUE)[[1]]
    
    # Create a window to display this table
    win1 <- tktoplevel()
    win1$env$table1 <- tk2table(win1, variable = tclTable, rows = nrow(mat1), cols = ncol(mat1), 
                                titlerows = 1, selectmode = "extended", colwidth = 18, background = "white")
    tkpack(win1$env$table1, fill = "both", expand = TRUE) 
    
    return(0)
}

#=====================================================================================CR
# 170721 MG : Downloading INFLUXDB data 
#=====================================================================================CR
INFLUXDB <- function(WDoutput, DownloadSensor, UserMins,
                     PROXY,URL, PORT, LOGIN, PASSWORD,
                     Down.Influx, Host, Port, User ,Pass, name.SQLite, name.SQLite.old, Db, Dataset, Influx.TZ = NULL,
                     sens2ref, asc.File=NULL, InfluxData = NULL) {
    
    # Parameters PROXY:  PROXY, URL, PORT, LOGIN, PASSWORD
    # Parameters Influx: Down.Influx, Host, Port, User, Pass, name.SQLite, name.SQLite.old, Db, Dataset, Influx.TZ
    # Sqlite database  : name.SQLite,name.SQLite.old
    # sens2ref          : data.table or dataframe: Configuration of sensors, output of function CONFIG
    # asc.File          : dataframe, default is NULL, used for giving the correct name of the sensor
    # Influx.file       : charater vector, default is null, File path of Influx data file. Can be .csv or .Rdata
    # InfluxData        : data.table or dataframe, default is null. DataSet of sensor values
    
    cat("-----------------------------------------------------------------------------------\n")
    cat("[INFLUXDB] INFO: Downloading InfluxDB data\n")
    # Saving Influx Sensor data
    if (is.null(DownloadSensor$Influx.Rdata.file)) {
        DownloadSensor$Influx.Rdata.file <- file.path(WDoutput, "InfluxData.csv")
        # Influx.Rdata.file = file.path(WDoutput, "InfluxData.Rdata")
        # Influx.csv.file   = file.path(WDoutput, "InfluxData.csv"  )
    }
    
    if (DownloadSensor$Retrieve.data.Influx) {
        
        if (Down.Influx) {
            
            # downloading data from InfluxDB and updating airsenseur.db
            Influx.TZ <- Down_Influx(PROXY = PROXY, URL = URL  , PORT = as.numeric(PORT), LOGIN = LOGIN, PASSWORD = PASSWORD,
                                     Host = Host  , User = User, Port = as.numeric(Port), Pass = Pass, name.SQLite = name.SQLite, name.SQLite.old = name.SQLite.old,
                                     Db = Db      , Dataset = Dataset, Influx.TZ = Influx.TZ, use_google = FALSE, Page = 10000, Mean = as.numeric(UserMins)) 
            # if there are problems accessing port 443 for the google api to determine time zone add , use_google = FALSE
            # Sqlite2df returns only the new data from the AirSensEUR.db, if the whole set is needed add: Complete = TRUE in function Down_Influx
            InfluxDataNew <- Sqlite2df(name.SQLite = name.SQLite, Dataset = Dataset, Influx.TZ = Influx.TZ, UserMins = UserMins, DownloadSensor = DownloadSensor, asc.File = asc.File, InfluxData = InfluxData)
        
        } else cat(paste0("[INFLUXDB] INFO: Data download not requested."), sep = "\n")
        
        
    } else cat(paste0("[INFLUXDB] INFO: Data downalod is already up to date."), sep = "\n")
    
    # Trying to use the existing data or Influx.Rdata.file
    if (is.null(InfluxData) || is.na(InfluxData)) {
        
        if (file.exists(DownloadSensor$Influx.Rdata.file)) {
            if (extension(DownloadSensor$Influx.Rdata.file) == ".csv") {
                
                InfluxData <- fread(file = DownloadSensor$Influx.Rdata.file, na.strings = c("","NA", "<NA>"))
                if (!is.null(Influx.TZ) && Influx.TZ != "") {
                    data.table::set(InfluxData, j = "date", value =  ymd_hms(InfluxData[["date"]], tz = Influx.TZ))
                } else data.table::set(InfluxData, j = "date", value =  ymd_hms(InfluxData[["date"]], tz = "UTC"))
                
            } else if (extension(DownloadSensor$Influx.Rdata.file) == ".Rdata") {
                
                load(DownloadSensor$Influx.Rdata.file)
                InfluxData <- data.table(InfluxData)
                
            } 
        } else {
            
            cat(paste0("[INFLUXDB] INFO: there is no previously saved Influx.Rdata.file. Missing InfluxData and Down.Influx request of sensor data download ."), sep = "\n")
        }
    } 
    
    # merging InfluxData and InfluxDataNew if needed
    if (exists("InfluxDataNew") && !is.null(InfluxDataNew) && !is.na(InfluxDataNew)) {
        
        if (exists("InfluxData") && !is.null(InfluxData) && !is.na(InfluxData)) {
            
            InfluxData <- rbind.fill(InfluxData,InfluxDataNew)
            rm(InfluxDataNew) 
            
        } else {
            
            InfluxData <- InfluxDataNew
            rm(InfluxDataNew)
            
        }
        
        if (extension(DownloadSensor$Influx.Rdata.file) == ".csv") {
            fwrite(InfluxData, file = DownloadSensor$Influx.Rdata.file, na = "NA")
        } else if (extension(DownloadSensor$Influx.Rdata.file) == ".Rdata") save(InfluxData, file = DownloadSensor$Influx.Rdata.file)
        
        cat(paste0("[INFLUXDB] INFO: Influx Sensor data saved in ", DownloadSensor$Influx.Rdata.file), sep = "\n")
    } 
    
    # returning data
    if (!is.null(InfluxData) && !is.na(InfluxData)) {
        
        var.names.meteo <- c("Temperature","Relative_humidity",  "Atmospheric_pressure")
        # setting the name of sensors
        var.names.sens  <- colnames(InfluxData)[-grep(pattern = paste0(c("date","_raw","gpsTimestamp","boardTimeStamp",  "channel", "latitude", "longitude", "altitude"),collapse = "|"), x = colnames(InfluxData))]
        if (length(var.names.sens) == 0) {
            stop(paste0("[INFLUXDB] ERROR: no sensor variable downloaded for ", Dataset," InFluxDB. Please check in the INfluxDB client -> STOP"))
        } else cat(paste0("[INFLUXDB] INFO: Sensor variables existing in airsenseur.db: ", paste0(var.names.sens, collapse = ", "), ", with date timestamp and coordinates."), sep = "\n")
        # Setting the Sensor names
        var.name.GasSensors <- var.names.sens[-(which(var.names.sens %in% var.names.meteo))]
        cat("[INFLUXDB] INFO INFLUXDB returning list with InfluxData, var.names.meteo, var.name.GasSensors and var.names.sens\n")
        
        cat("-----------------------------------------------------------------------------------\n")
        return(list(InfluxData, var.names.meteo, var.name.GasSensors, var.names.sens)) 
        
    } else {
        
        cat("-----------------------------------------------------------------------------------\n")
        return(cat("[INFLUXDB] ERROR no Influx data available\n"))
    }
}

#=====================================================================================CR
# 170721 MG : Downloading SOS data
#=====================================================================================CR
SOS      <- function(WDoutput, DownloadSensor, Down.SOS, AirsensEur.name, UserMins, AirsensWeb, Duration = 1, sens2ref) {
    # DownloadSensor   : output of function check_download
    # Parameters SOS: Down.SOS,AirsensEur.name,UserMins,AirsensWeb,Duration
    # Sqlite database  : name.SQLite,name.SQLite.old
    # Configuration sensors: sens2ref
    
    # SOSData
    cat("-----------------------------------------------------------------------------------\n")
    cat("[SOS] INFO: Downloading SOS data\n")
    if (DownloadSensor$Retrieve.data.SOS) {
        if (!is.null(Down.SOS)) {
            if (Down.SOS) {
                
                #------------------------------------------------------------------------------CR
                # Checking if Rdata file exists and setting the DownloadSensor$Retrieve.data.Ref and Retrieve.data.Influx true or false
                # Checking if there are previously downloaded data in General.Rdata and settings DownloadSensor 
                #------------------------------------------------------------------------------CR
                # Check that your Proxy is correctly set before running the next command
                SOSData <- Down_SOS(AirsensEur.name = AirsensEur.name, UserMins = UserMins, 
                                    DownloadSensor = DownloadSensor, AirsensWeb = AirsensWeb, Duration = Duration) # add DownloadSensor = DownloadSensor, to force the DateIN for download, 
                # in case of error during download set Duration = 1 and then set vack Duration = 7, launch a Check_Download in between
                # Down_SOS returns the whole dataFrame (old and new data) and save SOSData.Rdata and SOSdata.csv in general_data
                
                # setting the name of sensors
                var.names.meteo <- c("Temperature","Relative_humidity",  "Atmospheric_pressure")
                if (exists("SOSData")) {
                    # List of Pollutant/sensor installed in the AirSensEUR
                    var.names.sens <- colnames(SOSData)[-which(colnames(SOSData) == "date")]
                     
                    if (length(var.names.sens) == 0) {
                        stop(paste0("[SOS] ERROR: no sensor variable downloaded for ",AirsensEur.name," at the apiEndPoint. Please check in SOS client -> STOP"))
                    } else cat(paste0("[SOS] INFO: Sensor variables existing in the dataframe downloaded at the apiEndPoint: ", paste0(var.names.sens, collapse = ", "), ", plus date added"), sep = "\n")
                    
                    # Setting the Sensor names
                    var.name.GasSensors      <- var.names.sens[-(which(var.names.sens %in% var.names.meteo))]
                } else {
                    # if we do not have new data for sensors we use the names of sensors in sens2ref
                    var.name.GasSensors <- na.omit(sens2ref$gas.sensor)
                    var.names.sens      <- c(var.name.GasSensors, var.names.meteo)
                }
                
                # Saving Sensor data - It is alredy saved, it is not needed to save again, just use Make.old
                # No need to save SOS.file and SOS.csv.file, as it is already done in Down_SOS
                cat(paste0("[SOS] INFO: SOS Sensor data are in ", DownloadSensor$SOS.file), sep = "\n")

            } else {
                
                # no request Down.SOS. Trying to use the existing SOS.file
                if (file.exists(file.path(DownloadSensor$SOS.file))) {
                    
                    cat(paste0("[SOS] INFO: Down.SOS set to FALSE in ASEConfig_xx.R  (no request of sensor data download from SOS). Using previously saved file ."), sep = "\n")
                    if (extension(DownloadSensor$SOS.file) == ".csv") {
                        RefData <- fread(file = DownloadSensor$SOS.file, na.strings = c("","NA", "<NA>"))
                    } else if (extension(DownloadSensor$SOS.file) == ".Rdata") load(DownloadSensor$SOS.file)
                    var.names.meteo     <- c("Temperature","Relative_humidity",  "Atmospheric_pressure")
                    var.name.GasSensors <- na.omit(sens2ref$gas.sensor)
                    var.names.sens      <- c(var.name.GasSensors, var.names.meteo)
                    
                } else {
                    cat(paste0("[SOS] INFO: Down.SOS set to FALSE in ASEConfig_xx.R  (no request of sensor data download from SOS). There is no previously saved file.",
                               "Missing SOSData and Down.SOS request of sensor data download set to FALSE in ASEConfig_xx.R .\n"))
                }
            }    
        }
    } else {
        
        if (file.exists(file.path(DownloadSensor$SOS.file))) {
            
            if (extension(DownloadSensor$SOS.file) == ".csv") {
                RefData <- fread(file = DownloadSensor$SOS.file, na.strings = c("","NA", "<NA>"))
            } else if (extension(DownloadSensor$SOS.file) == ".Rdata") load(DownloadSensor$SOS.file)
            var.names.meteo     <- c("Temperature","Relative_humidity",  "Atmospheric_pressure")
            var.name.GasSensors <- na.omit(sens2ref$gas.sensor)
            var.names.sens      <- c(var.name.GasSensors, var.names.meteo)
        } else {
            cat(paste0("[SOS] INFO: sensor data download from SOS already updated (DownloadSensor$Retrieve.data.SOS set to FALSE)"), sep = "\n")
        }
    } 
    
    cat("-----------------------------------------------------------------------------------\n")
    if (exists("SOSData")) {
        cat("[SOS] INFO SOS returning list with SOSData, var.names.meteo, var.name.GasSensors and var.names.sens\n")
        return(list(SOSData, var.names.meteo, var.name.GasSensors, var.names.sens)) 
    } else return(cat("[SOS] INFO no SOS data available\n"))
    
}

#=====================================================================================CR
# 170721 MG : Downloading REFERENCE data
#=====================================================================================CR
REF      <- function(DownloadSensor, AirsensEur.name, DisqueFieldtestDir, UserMins, 
                     Down.Ref, FTPMode, ref.tzone, InfluxData, SOSData, Reference.name, urlref, sens2ref, 
                     Ref.SOS.name = NULL, RefSOSname = NULL, RefSOSDateIN = NULL, RefSOSDateEND = NULL,
                     Ref__a_i_p__name = NULL, User__a_i_p__ = NULL, Pass__a_i_p__ = NULL, Ref__a_i_p__Organisation = NULL, 
                     Ref__a_i_p__Station = NULL, Ref__a_i_p__Pollutants = NULL, Ref__a_i_p__DateIN = NULL, Ref__a_i_p__DateEND = NULL,
                     csvFile = NULL, csvFile.sep = NULL, csvFile.quote = NULL, coord.ref = NULL,
                     Ref.Type = "Ref", RefData = NULL) {
    # DownloadSensor        = Output of function DownloadSensor()
    # Down.ref              = logical, if true reference data are downloaded
    # FTPMode               = string, default = "ftp", type of download of reference data: "ftp" using a csv file on a ftp server, "csv" the same with a local file and SOS: SOS download
    # ref.tzone             = string, refernce time name of the reference data. Default = "UTC"
    
    # Ref.SOS.name          = SOS ID of the Reference station
    # RefSOSname            = Reference station SOS Rest API URL
    # RefSOSDateIN          = Starting  date for downloading Reference data using SOS
    # RefSOSDateEND         = Ending date for downloading Reference data using SOS
    
    # Ref__a_i_p__name         = input$Ref__a_i_p__name, 
    # User__a_i_p__            = input$User__a_i_p__, 
    # Pass__a_i_p__            = input$Pass__a_i_p__, 
    # Ref__a_i_p__Organisation = input$Ref__a_i_p__Organisation, 
    # Ref__a_i_p__Station      = input$Ref__a_i_p__Station, 
    # Ref__a_i_p__Pollutants   = input$Ref__a_i_p__Pollutants, 
    # Ref__a_i_p__DateIN       = as.Date(input$Ref__a_i_p__Date[1], format = "%Y-%m-%d"),
    # Ref__a_i_p__DateEND      = as.Date(input$Ref__a_i_p__Date[2], format = "%Y-%m-%d"),
    
    # csvFile               = if FTPMode = "csv", file path to the csv file to load
    # csvFile.sep           = if FTPMode = "csv", separator between columns in the csvFile
    # csvFile.quote         = if FTPMode = "csv", separator of values in all columns
    # coord.ref             = string with coordinates of reference data longitude and latitude separated by a blank
    # Ref.type              = label to be written in front of pollutatns names, defaut is Ref, other possibility Bin.APS and Bin.DMPS for PM distribution
    # RefData               = data.table or dataframe, default is null, reference dataset

    
    #------------------------------------------------------------------------------CR
    # Downloading Reference data, Only new values, save RefData.RData and refData.csv with all reference values
    #------------------------------------------------------------------------------CR
    # Getting what is the first date in InfluxData and or SOSData and setting in DownloadSensor
    
    cat("-----------------------------------------------------------------------------------\n")
    cat("[REF] INFO: Reading or downloading Reference data, save RefData.RData and refData.csv with all reference values in directory General_Data\n")
    if (exists("InfluxData") && !is.null(InfluxData)) {
        minDateInflux <- min(InfluxData$date, na.rm = TRUE)
        if (exists("SOSData") && !is.null(SOSData)) {
            minDateSOS    <- min(SOSData$date, na.rm = TRUE)
            DownloadSensor$mindateRef <- min(c(minDateInflux,minDateSOS), na.rm = TRUE)
        } else {
            DownloadSensor$mindateRef <- min(minDateInflux, na.rm = TRUE)
        }
    } else {
        if (exists("SOSData") && !is.null(SOSData)) {
            minDateSOS    <- min(SOSData$date, na.rm = TRUE)
            DownloadSensor$mindateRef <- min(c(minDateSOS), na.rm = TRUE)
        }
    } # The if statement should be added to DownloadSensor
    
    if (DownloadSensor$Retrieve.data.Ref) {
        
        if (Down.Ref) {
            
            cat("-----------------------------------------------------------------------------------\n")
            cat(paste0("[REF] INFO, Starting downloading data for ", Reference.name, sep = "\n"))
            RefDataNew  <- Down_Ref(Reference.name = Reference.name, UserMins = UserMins, DownloadSensor = DownloadSensor, urlref = urlref, ref.tzone = ref.tzone, 
                                    naString = c("-999.99", "-999.98999", NaN, NA), WDoutput = file.path(DisqueFieldtestDir, "General_data"), 
                                    FTPMode = FTPMode, 
                                    Ref.SOS.name = Ref.SOS.name, RefSOSname = RefSOSname, RefSOSDateIN = RefSOSDateIN, RefSOSDateEND = RefSOSDateEND,
                                    
                                    Ref__a_i_p__name = Ref__a_i_p__name, User__a_i_p__ = User__a_i_p__, Pass__a_i_p__ = Pass__a_i_p__, 
                                    Ref__a_i_p__Organisation = Ref__a_i_p__Organisation, Ref__a_i_p__Station = Ref__a_i_p__Station, 
                                    Ref__a_i_p__Pollutants = Ref__a_i_p__Pollutants, Ref__a_i_p__DateIN = Ref__a_i_p__DateIN, Ref__a_i_p__DateEND = Ref__a_i_p__DateEND,
                                    
                                    csvFile = csvFile, csvFile.sep = csvFile.sep, csvFile.quote = csvFile.quote, coord.ref = trimws(x = coord.ref), Ref.Type = Ref.Type) # this return only new Data
            
        } else cat(paste0("[REF] INFO: Data download not requested."), sep = "\n")
        
    } else cat(paste0("[REF] INFO: Data download already up to date."), sep = "\n")
    
    # Trying to use the existing data or Influx.Rdata.file
    if (is.null(RefData) || is.na(RefData)) {
        
        # loading the possible existing data in Refdata
        if (file.exists(DownloadSensor$Ref.Rdata.file)) {
            
            if (extension(DownloadSensor$Ref.Rdata.file) == ".csv") {
                
                RefData <- fread(file = DownloadSensor$Ref.Rdata.file, na.strings = c("","NA", "<NA>"))
                # removing rownames if any
                if ("V1" %in% names(RefData)) RefData[, V1 := NULL]
                if (!is.null(Ref.TZ) && Ref.TZ != "") {
                    data.table::set(InfluxData, j = "date", value =  ymd_hms(InfluxData[["date"]], tz = Ref.TZ))
                } else data.table::set(InfluxData, j = "date", value =  ymd_hms(InfluxData[["date"]], tz = "UTC"))
                
            } else if (extension(DownloadSensor$Ref.Rdata.file) == ".Rdata") {
              
                load(DownloadSensor$Ref.Rdata.file)  
                if (!"data.table" %in% class(RefData)) RefData <- data.table(RefData)
                
            } 
            
            #if (is.null(key(RefData))) setkey(RefData, date) # very slow, avoid
            
        } else cat(paste0("[REF] INFO: there is no previously saved Ref data."), sep = "\n")
    } 
    
    # merging RefData and RefDataNew if needed
    if (exists("RefDataNew") && !is.null(RefDataNew) && !is.na(RefDataNew)) {
        
        if (exists("RefData") && !is.null(RefData) && !is.na(RefData)) {
            
            # https://stackoverflow.com/questions/34834257/r-programming-merge-function-returns-column-names-with-x-and-y
            RefData <- merge(x = RefData, y = RefDataNew, by = intersect(names(RefData),names(RefDataNew)), all = TRUE)  
            rm(RefDataNew) 
            
        } else {
            
            RefData <- RefDataNew
            rm(RefDataNew)
            
        }
        
        if (extension(DownloadSensor$Ref.Rdata.file) == ".csv") {
            fwrite(RefData, file = DownloadSensor$Ref.Rdata.file, na = "NA")
        } else if (extension(DownloadSensor$Ref.Rdata.file) == ".Rdata") save(RefData, file = DownloadSensor$Ref.Rdata.file)
        
        cat(paste0("[REF] INFO: reference data saved in ", DownloadSensor$Ref.Rdata.file), sep = "\n")
        
    } else cat(paste0("[REF] WARNING, There is no new reference data for ",Reference.name,"\n"))
    
    # Preparing for returning data, setting var.names.ref
    if (exists("RefData") && !is.null(RefData) && !is.na(RefData)) {
        
        if (!identical(colnames(RefData)[-which(colnames(RefData) %in% c("date", "Ref.Long", "Ref.Lat"))],character(0))) {
            
            # List of Pollutants monitored at the Referencce stations
            var.names.ref <- colnames(RefData)[-which(colnames(RefData) %in% c("date", "Ref.Long", "Ref.Lat"))]
            
            if (length(var.names.ref) == 0) {
                
                cat(paste0("[REF] ERROR no reference data exisiting for ", AirsensEur.name," .Please check data at ", Reference.name))
                
            } else cat(paste0("[REF] INFO, Variables found in the reference dataset: ", paste0(var.names.ref, collapse = ", "),"\n"))
            
        } else {
            
            # if we do not have new data for sensors we use the names of sensors in sens2ref
            var.names.ref <- na.omit(sens2ref$gas.reference2use)
        }
    }  
    
    if (exists("RefData") & exists("var.names.ref") & exists("DownloadSensor")) {
        
        cat("[REF] INFO, returning list with RefData, var.names.ref and DownloadSensor\n")
        cat("-----------------------------------------------------------------------------------\n")
        return(list(RefData, var.names.ref, DownloadSensor)) 
        
    }  else {
        
        cat("[REF] ERROR no Rerence data available\n")
        cat("-----------------------------------------------------------------------------------\n")
        return(cat("[REF] ERROR no Rerence data available\n"))
    }
}

#=====================================================================================CR
# 170721 MG : Merging InfluxData or SOSData and RefData 
#=====================================================================================CR
# https://stackoverflow.com/questions/5577221/how-can-i-load-an-object-into-a-variable-name-that-i-specify-from-an-r-data-file
load_obj <- function(file_path)
{
    # returning a loaded R object
    env <- new.env()
    nm <- load(file = file_path, envir = env, verbose = FALSE)[1]
    env[[nm]]
}
GENERAL  <- function(WDoutput, UserMins, RefData, InfluxData, SOSData, Delay, var.name.GasSensors, DownloadSensor, Change.Delay = FALSE, Change.UserMins = FALSE) {
    # input:
    #       WDoutput,
    #       UserMins,
    #       RefData,er
    #       InfluxData,
    #       SOSData, 
    #       Delay, 
    #       var.name.GasSensors
    #       DownloadSensor     : list output of Check_Downalod, use to know if DateEND.General.prev is before last date in INfluxData or SOSData
    #       Change.Delay       : logical, default False, TRUE if Delay has been changed and General shall be created new
    #       Change.UserMins    : logical, default False, TRUE if UserMins has been changed and General shall be created new
    #       
    # Return the data.table General, adding new data to the existing one and averaging every UserMins minutes
    
    #------------------------------------------------------------------------------CR
    # Merging InfluxData or SOSData and RefData 
    #------------------------------------------------------------------------------CR
    # - using preferably the latest date of the previous general dataset from influxdb, otherwise SOS data
    
    cat("-----------------------------------------------------------------------------------\n")
    cat("[GENERAL] INFO, Checking if there are more data in InfluxData or SOSData than in General.Rdata\n")
    # Checking if there are sensor data to be added to General 
    # if General.Rdata does not exist it must be created in all cases. The same if the Delay has changed 
    is.sensorData <- FALSE
    if (is.null(DownloadSensor$DateEND.General.prev) || Change.Delay || Change.UserMins) {
        
        is.sensorData <- TRUE  
        
    } else {
        
        # Checking last date in InfluxData 
        if (exists("InfluxData") && !is.null(InfluxData)) {
            
            is.sensorData <- !all(is.na(InfluxData[which(InfluxData$date > DownloadSensor$DateEND.General.prev),-which(colnames(InfluxData) == "date")])) 
        } 
        
        # Checking last date in SOSData
        if (exists("SOSData") && !is.null(SOSData)) {
            
            # Max.Sensor.date <- max(SOSData[all(is.na(SOSData[SOSData$date > DownloadSensor$DateEND.General.prev,-which(colnames(SOSData) == "date")])),"date"], na.rm = T)
            if (exists("is.sensorData")) {
                
                is.sensorData <- is.sensorData || !all(is.na(SOSData[which(SOSData$date > DownloadSensor$DateEND.General.prev),-which(colnames(SOSData) == "date")]))
            } else is.sensorData <- !all(is.na(SOSData[which(SOSData$date > DownloadSensor$DateEND.General.prev),-which(colnames(SOSData) == "date")]))
        }   
        
        # Checking last date in RefData
        if (exists("RefData") && !is.null(RefData)) {
            
            # Max.Sensor.date <- max(RefData[all(is.na(RefData[RefData$date > DownloadSensor$DateEND.General.prev,-which(colnames(RefData) == "date")])),"date"], na.rm = T)
            # Date to be added from refData
            row.New.Ref.date <- which(RefData$date > DownloadSensor$DateEND.General.prev)
            if (length(row.New.Ref.date) > 0 && any(RefData[row.New.Ref.date, "date"] %in% InfluxData$date)) {
                
                if (exists("is.sensorData")) {
                    
                    is.sensorData <- is.sensorData || !all(is.na(RefData[row.New.Ref.date,-which(colnames(RefData) == "date")]))
                } else is.sensorData <- !all(is.na(RefData[row.New.Ref.date,-which(colnames(RefData) == "date")]))
            }
        } 
    }
    
    if (is.sensorData) {
        
        cat("[GENERAL] INFO, Merging InfluxData or SOSData with RefData \n")
        if (exists("RefData") && !is.null(RefData)) { 
            
            if (exists("InfluxData") && !is.null(InfluxData)) {
                
                # Fine adjusting of InfluxData$date due to delays between sensor and reference data - The Delay should be the same for all sensors, this makes sense
                #  Adding field "date_PreDelay" in Influx and SOS data that will be saved into General but not in Influx and SOS.Rdata
                #if (Delay != 0) {
                #    if (!any(names(InfluxData) == "date_PreDelay")) {
                InfluxData$date_PreDelay <- InfluxData$date
                InfluxData$date          <- InfluxData$date + Delay * 60
                #    } else{ # This is impossible as date_PreDelay is never saved in Influx.Rdata
                #        InfluxData$date          <- InfluxData$date_PreDelay + Delay * 60
                #    }
                #} # if Delay == 0 set General$date to General$date_PreDelay if it exits 
                
                # Trying to rbind.fill InfluxData and SOSdata
                # we prefer InfluxData data over SOSData if they exist for the boardTImeStamp and gpsTimeStamp
                if (exists("SOSData") && !(is.null(SOSData))) { # RefData, InfluxData and SOSData exists
                    
                    # Fine adjusting of InfluxData$date due to delays between sensor and reference data - The Delay should be the same for all sensors, this makes sense
                    # Adding field "date_PreDelay" in Influx and SOS data that will be saved into General but not in Influx and SOS.Rdata
                    #if (Delay != 0) {
                    #    if (!any(names(SOSData) == "date_PreDelay")) {
                    SOSData$date_PreDelay <- SOSData$date
                    SOSData$date          <- SOSData$date + Delay * 60
                    #    } else{ # This is impossible as date_PreDelay is never saved in SOS.Rdata
                    #        SOSData$date          <- SOSData$date_PreDelay + Delay * 60
                    #    }
                    #} # if Delay == 0 set General$date to General$date_PreDelay if it exits
                    
                    # if the union of SOSData and InfluxData gives additional data
                    index.Date.SOSData <- which(SOSData$date %in% InfluxData$date)
                    General            <- merge(x = rbind.fill(SOSData[!(row.names(SOSData) %in% index.Date.SOSData),], InfluxData), y = RefData, by = "date", all.x = TRUE )
                    # Discarding Reference dates when sensor dates do no exist
                    General            <- General[General$date >= min(min(InfluxData$date),min(SOSData$date)) & General$date <= max(max(InfluxData$date),max(SOSData$date)),]
                    
                } else {
                    # RefData exists, InfluxData present but no SOSData
                    
                    # In case of names with "_raw", the digital values in raw form are not saved in Genera.data, they are only kept in the airsenseur.db if Down_Influx is used
                    if (any(grepl(pattern = "_raw", x = colnames(InfluxData)))) {
                        
                        General <- merge(data.table(InfluxData[-grep(pattern = "_raw", x = colnames(InfluxData)),], key = "date"), 
                                         data.table(RefData, key = "date"), by = "date", all.x = TRUE)
                        
                    } else General <- merge(data.table(InfluxData, key = "date"), data.table(RefData, key = "date"), by = "date", all.x = TRUE)
                    
                    # Keeping only data with values in InfluxDB
                    General <- General[General$date >= min(InfluxData$date, na.rm = TRUE) & General$date <= max(InfluxData$date, na.rm = TRUE),]
                }
                
                cat("[GENERAL] INFO General data frame, there are new sensor data and new reference data \n")
                
                
            } else { 
                
                # RefData exists but no Influx Data
                
                if (exists("SOSData") & !(is.null(SOSData))) { # RefData and SOSData exist but no Influx Data
                    
                    # Fine adjusting of InfluxData$date due to delays between sensor and reference data - The Delay should be the same for all sensors, this makes sense
                    # Adding field "date_PreDelay" in Influx and SOS data that will be saved into General but not in Influx and SOS.Rdata
                    #if (Delay != 0) {
                    #    if (!any(names(SOSData) == "date_PreDelay")) {
                    SOSData$date_PreDelay <- SOSData$date
                    SOSData$date          <- SOSData$date + Delay * 60
                    
                    # if InfluxData does not exist, we keep all data of SOSData
                    General <- merge(x = SOSData, y = RefData, by = "date", all = TRUE ) 
                    
                    # Discarding Reference dates in GEeneral, keeping only dates within SOSData$date
                    General <- General[General$date >= min(SOSData$date) & General$date <= max(SOSData$date),]
                    # For ASEConfig02, if SOSData and InfluxData give  consecutive data
                    if (ASEConfig == "ASEconfig_02.R") {
                        General <- merge(x = rbind.fill(SOSData, InfluxData)[rbind.fill(InfluxData,SOSData)$date >= as.POSIXct("2016-09-09",tz= "UTC"),], 
                                         y = RefData[RefData$date >= as.POSIXct("2016-09-09",tz= "UTC"),], by = "date", all.x = TRUE ) 
                    } 
                } else {
                    # RefData exists but no InfluxData and no SOSData
                    General <- RefData
                    cat("[GENERAL] ERROR: General data frame, there are no new sensor data and/or new reference data.\n")
                }
            }
            
        } else { # RefData does not exist
            
            if (exists("InfluxData") && !is.null(InfluxData)) { # RefData does not exist but InfluxData exists
                
                # Fine adjusting of InfluxData$date due to delays between sensor and reference data - The Delay should be the same for all sensors, this makes sense
                #  Adding field "date_PreDelay" in Influx and SOS data that will be saved into General but not in Influx and SOS.Rdata
                InfluxData$date_PreDelay <- InfluxData$date
                InfluxData$date          <- InfluxData$date + Delay * 60
                
                General <- InfluxData
                
                cat("[GENERAL] INFO, General data frame, there are new sensor data while there are no new reference data \n")
                
            } else { 
                
                # RefData and InfluxData do not exist
                if (exists("SOSData") && !is.null(SOSData)) { # RefData and InfluxData do not exist but SOSData exists
                    
                    # Fine adjusting of InfluxData$date due to delays between sensor and reference data - The Delay should be the same for all sensors, this makes sense
                    # Adding field "date_PreDelay" in Influx and SOS data that will be saved into General but not in Influx and SOS.Rdata
                    SOSData$date_PreDelay <- SOSData$date
                    SOSData$date          <- SOSData$date + Delay * 60
                    
                    General     <- SOSData
                    
                } else { # RefData, InfluxData and SOSData do not exist
                    
                    General <- NA
                    cat("[GENERAL] ERROR: General data frame, there are no new sensor data nor reference data \n")
                }
            }
        }
        
        General <- DF_avg(General, width = UserMins)
        # Discaring any _raw column
        if (any(grepl(pattern = "_raw", x = colnames(InfluxData)))) {
            General <- General[-grep(pattern = "_raw", x = colnames(General)),]  
        } 
        
        # discarding rows with all NAs and NaNs for All gas sensors
        if (exists("General") && !is.null(General) & !all(is.na(General))) {
            
            cat("[GENERAL] INFO, Discarding rows with NA and NaN for all gas sensors\n")
            # replacing NaN with NA
            cat(paste0("[GENERAL] INFO, replacing sensors values which are not numbers (NaN) with NA for all parameters."), sep = "\n")
            General[which(names(General) != "date")] <- purrr::map_dfc(General[which(names(General)!= "date")], function (i) nan.to.na(i) )
            
            # We should not remove the empty rows otherwise rollapply will not work for outleirs discarding
            # ind <- which(apply(General[, var.name.GasSensors], 1, function(x) all(is.na(x))))
            # if (length(ind)!=0) {
            #     General <- General[-ind,] 
            #     cat(paste0("[GENERAL] INFO, ", length(ind), " rows have been discarded, with only NAs for all gas sensors\n"))
            # } 
            # remove(ind)
        }
        
        # Averaging using UserMins
        # The timeAverage is not needed maybe since from now the average is carried out by the InfluxQL. Comment the lines for later use, maybe
        # Well, it is still needed if UserMins or Delay is changed
        
        
    } else {
        # Selecting General.Rdata if it exists
        if (file.exists(DownloadSensor$General.Rdata.file)) {
            General <- load_obj(DownloadSensor$General.Rdata.file)
            General <- DF_avg(General, width = UserMins)
            
        } else {
            return(cat("[GENERAL] ERROR no General data available\n"))   
            cat("-----------------------------------------------------------------------------------\n")
        } 
    }
    
    if (exists("General") && !is.null(General)) {
        cat("[GENERAL] INFO returning General data.table\n")
        
        # adding absolute humidity is relative humidity and temperature exist
        if (all(c("Temperature", "Relative_humidity") %in% names(General))) {
            # https://r.789695.n4.nabble.com/Best-way-to-preallocate-numeric-NA-array-td861943.html
            General$Absolute_humidity <- NA_real_
            General$Td_deficit        <- NA_real_
            both.Temp.Hum <- which(complete.cases(General[, c("Temperature", "Relative_humidity")]))
            General[both.Temp.Hum, Absolute_humidity := threadr::absolute_humidity(General[both.Temp.Hum, Temperature], General[both.Temp.Hum, Relative_humidity])]  
            Td <- weathermetrics::humidity.to.dewpoint(rh = General[both.Temp.Hum, Relative_humidity], t = General[both.Temp.Hum, Temperature], temperature.metric = "celsius") 
            General[both.Temp.Hum, Td_deficit := General[both.Temp.Hum, Temperature] - Td]
        }
        
        if (all(c("Ref.Temp", "Ref.RH") %in% names(General))) {
            General$Ref.Absolute_humidity <- NA_real_
            General$Ref.Td_deficit        <- NA_real_
            Ref.both.Temp.Hum <- which(complete.cases(General[, c("Ref.Temp", "Ref.RH")]))
            General[Ref.both.Temp.Hum, Ref.Absolute_humidity := threadr::absolute_humidity(General[Ref.both.Temp.Hum, Ref.Temp], General[Ref.both.Temp.Hum, Ref.RH])]
            Td <- weathermetrics::humidity.to.dewpoint(rh = General[Ref.both.Temp.Hum, Ref.RH], t = General[Ref.both.Temp.Hum, Ref.Temp], temperature.metric = "celsius") 
            General[Ref.both.Temp.Hum, Ref.Td_deficit := General[Ref.both.Temp.Hum, Ref.Temp] - Td]
        }
        
        # returning General
        cat("-----------------------------------------------------------------------------------\n")
        return(General) 
    } else{
        cat("-----------------------------------------------------------------------------------\n")
        return(cat("[GENERAL] ERROR no General data available\n"))   
    }
    
}

#=====================================================================================CR
# Function View Scatter Plot of calibration function (Vs 170420)
#=====================================================================================CR
Etalonnage <- function(x, s_x, y, s_y, AxisLabelX, AxisLabelY, Title, Marker , Couleur, 
                       ligne= NULL, XY_same, lim = NULL, steps = c(10,10), digitround = NULL, marges = NULL, PlotAxis = NULL) {
    # This function plot a typical XY calibration graph, estimate Xlim and ylim, add x and y labels and gridlines
    # Title  : Charater string, title of the plot
    # Marker : type marker, typical 19
    # Couleur: color of the marker, ex 'blue'
    #### NEW ###ligne: used in plot as type = 'ligne' to have dots or lines or ...
    # XY_same: if TRUE: X and Y have the same extent, IF False: Xlim ranges between min(x) and max(x) and ylim ranges between min(y) and max(y)
    # lim : if NULL Xlim and Ylim are calculated otherwise lim is used. Lim must be a matrix of xlim and ylim in column vector
    # steps: number of steps on the x an y axis, if NULL, default =10 as c(stepX, stepY)
    # digitround: number of digit for x and y axis labels (c(digitX,digitY)), should be c(0,0) for one digit
    # marges: margin of graph, default c(4,4,3,0.5)) if NULL
    # PlotAxis: = "n" to disable the plot of the axis. If empty the axis will be plot
    
    # return(cbind(Xlim,Ylim, par("usr")[1:2], par("usr")[3:4])) # par("usr") gives the true chosen xlim and ylim to which 4% of range is added in both sides. Used for arrows
    
    # checking that not all data are NA
    if (!all(is.na(x)) & !all(is.na(y))) {
        
        # for consistency with previous version of function Etalonnage if ligne is not given
        if (is.null(ligne)) ligne = "p"
        
        # saving the original par values in case they would be modified in this function
        op <- par(no.readonly = TRUE)
        # Passing and resuming the par values
        on.exit(par(op))
        
        # settings the margins
        if (is.null(marges)) {par(mar = c(4,4,3,0.5))} else {par(mar = marges)}  
        
        # Creating the DataXY data frame
        if (is.null(s_y)  || any(s_y == 0) || all(is.na(s_y))) {
            DataXY <- data.frame(cbind(x, y),stringsAsFactors = FALSE)
            colnames(DataXY) <- c("x", "y")
            DataXY <- subset(DataXY, !is.na(DataXY$x) & !is.na(DataXY$y))
        } else {
            DataXY <- data.frame(cbind(x, y, s_y),stringsAsFactors = FALSE)
            colnames(DataXY) <- c("x","y","s_y")
            DataXY <- subset(DataXY, !is.na(DataXY$x) & !is.na(DataXY$y) & !is.na(DataXY$s_y))
        }  
        
        # Automatic estimation of digitround
        if (is.null(digitround)) {
            Int <- c("x","y")
            Range <- sapply(DataXY[,Int], range, na.rm = TRUE, finite = TRUE)[2,] - sapply(DataXY[,Int], range, na.rm = TRUE, finite = TRUE)[1,]
            cat("Range of values on x and y axis\n")
            print(Range, quote = FALSE)
            digitround <- round(log10(1/Range)) + 2 # +1 gives too many digits? no it is fine
        }
        
        # Calculating the limits of the graph
        if (is.null(lim)) {
            if (isTRUE(XY_same)) {
                if (is.null(s_y) || any(s_y == 0) || all(is.na(s_y))) {
                    Xlim <- c(round(min(DataXY[is.finite(DataXY$x),"x"], DataXY[is.finite(DataXY$y),"y"]),digits = digitround[1]), 
                              round(max(DataXY[is.finite(DataXY$x),"x"], DataXY[is.finite(DataXY$y),"y"]),digits = digitround[1]))
                    Ylim <- Xlim
                } else {
                    Xlim <- c(round(min(DataXY[is.finite(DataXY$x),"x"], DataXY[is.finite(DataXY$y),"y"] - DataXY$s_y),digits = digitround[1]),
                              round(max(DataXY[is.finite(DataXY$x),"x"], DataXY[is.finite(DataXY$y),"y"] + DataXY$s_y),digits = digitround[1]))
                    Ylim <- Xlim
                }
            } else {
                if (is.null(s_y)|| any(s_y == 0) || all(is.na(s_y))) {
                    Ylim <- c(round(min(DataXY$y, na.rm = TRUE),digits = digitround[2]),
                              round(max(DataXY[is.finite(DataXY$y),"y"], na.rm=TRUE),digits = digitround[2]))
                } else {
                    #if ((max(DataXY$y + DataXY$s_y)-min(DataXY$y - DataXY$s_y)) < 1) {
                    #  Ylim <- c((min(DataXY$y - DataXY$s_y)),(max(DataXY$y + DataXY$s_y)))
                    #} else {
                    Ylim <- c(round(min(DataXY$y - DataXY$s_y),digits = digitround[2]),
                              round(max(DataXY$y + DataXY$s_y),digits = digitround[2]))
                    #}
                }
                if (class(DataXY$x) == "POSIXct") {
                    Xlim <- c(min(DataXY$x),max(DataXY$x))
                } else {
                    Xlim <- c(round(min(DataXY$x),digits = digitround[1]),
                              round(max(DataXY$x),digits = digitround[1]))
                }
            }
        } else {
            Xlim <- c(round(min(lim[,1]),digits = digitround[1]),round(max(lim[,1]),digits = digitround[1]))
            Ylim <- c(round(min(lim[,2]),digits = digitround[2]),round(max(lim[,2]),digits = digitround[2]))
        }
        # specifying ticks and grid
        if (is.null(steps)) {
            stepsX <- 10
            stepsY <- 10
        } else {
            stepsX <- steps[1] # was steps[,1] but imply to enter steps values as a matrix
            stepsY <- steps[2] # was steps[,2] but imply to enter steps values as a matrix
        }
        
        # plotting the scatterplot
        plot( DataXY$x, DataXY$y
              ,xlab= AxisLabelX
              ,ylab= AxisLabelY
              ,xlim = Xlim
              ,ylim = Ylim
              ,col = Couleur
              ,type = ligne
              ,pch = Marker
              ,xaxt = "n"
              ,yaxt = "n"
              
        )
        axis(side = 1, at = pretty(x,stepsX))
        axis(side = 2, at = pretty(y,stepsY))
        if (!is.null(s_y) && all(s_y != 0)) {
            # hack: we draw arrows but with flat "arrowheads"
            arrows(DataXY$x, DataXY$y - DataXY$s_y , DataXY$x, DataXY$y + DataXY$s_y, length=0.05, angle=90, code=3)
        }
        
        abline(h=pretty(y,stepsY), v =pretty(x,stepsX), lty = 2, col = "grey")
        title (main = Title, outer = TRUE, line = -1)
        
        # Saving par variables before resuming 
        Xusr = par("usr")[1:2]
        Yusr = par("usr")[3:4]
        mar12 = par("mar")[1:2]
        mar34 = par("mar")[3:4]
        
        # Passing and resuming the par values
        on.exit(par(op))
        
        # par("usr") gives the true chosen xlim and ylim to which 4% of range is added in both sides. Used for arrows
        return(cbind(Xlim,Ylim, Xusr, Yusr, mar12, mar34)) 
    } else {
        # all data re NA
        plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
        text(1,1,paste0("[Etalonnage], ERROR, all x or all y data are NA"))
        
    }
}

#=====================================================================================CR
# Config of ASE Boxes ex.  ASEconfig.R
#=====================================================================================CR
# USER CONFIG PARAGRAPH
#  1. Configuring Proxy server
#  2. Sensor configuration for download for Influx and SOS. InfluxDB has more info and is preferred over SOS
#  3. Reference data, configuration for download, ftp                               
#  4. Create sensor configuration file and matching between reference and sensor names 
#  5. SET Average time for sensor data
#  9. SET temperature and relative humidity thresholds for sensors validity
# 11. Valid Periods                                                                 (NOT USED)
# 12. SET TIME PARAMETERS -> see in ASE_OPER_SCRIPT.R                               (NOT USED)
#=====================================================================================CR
CONFIG <- function(DisqueFieldtest , ASEconfig, sens2ref.shield = NULL) {
    # Return a list with the config of servers, sensors and effects
    # DisqueFieldtest   : directory where is the file ASEconfig*.R file
    # ASEConfig         : AirSensEUR name e.g LANUV_01 in ASEConfigLANUV_01.R or the AirSensEUR config file as in ASEConfigLANUV_01.R
    # sens2ref.shield   : dataframe, default is NULL, dataframe returned by function ASEPanel04Read giving the configuration of the chemical shield
    
    cat("-----------------------------------------------------------------------------------\n")
    ASE_name           <- basename(ASEconfig); for (i in c("\\.[[:alnum:]]+$","ASEconfig")) ASE_name <- sub(pattern=i,replacement = '', basename(as.character(ASE_name)))
    DisqueFieldtestDir <- file.path(DisqueFieldtest, ASE_name)
    
    #=====================================================================================CR
    #  ASE_name,"_Servers.cfg"
    #=====================================================================================CR
    # Read config file (TRUE)
    File_Server_cfg <- list.files(path = file.path(DisqueFieldtestDir, "General_data"), pattern = paste0(ASE_name,"_Servers.cfg"))
    if (!identical(File_Server_cfg,character(0))) {
        
        # reading the Server configuration files
        File_Server_cfg <- file.path(DisqueFieldtestDir, "General_data", paste0(ASE_name,"_Servers.cfg"))
        if (file.exists(File_Server_cfg)) {
            
            cfg <- transpose(fread(file = File_Server_cfg, header = FALSE, na.strings=c("","NA", "<NA>")), fill = NA, make.names = 1)
            cat(paste0("[CONFIG] Info, the config file ", File_Server_cfg, " for the configuration of servers exists"), sep = "\n")
            
            # Creating UserMinsAvg if it does not exist
            if (!"UserMinsAvg" %in% names(cfg)) cfg$UserMinsAvg <- cfg$UserMins
            
            # Converting to correct format
            Vector.type <- c("PROXY", "Down.Influx", "Down.SOS", "Down.Ref")
            Vector.type <- Vector.type[Vector.type %in% names(cfg)]
            for (j in Vector.type) set(cfg, j = j, value = as.logical(cfg[[j]]))
            
            Vector.type <- c("PORT", "Port", "UserMins", "UserMinsAvg", "Delay")
            Vector.type <- Vector.type[Vector.type %in% names(cfg)]
            for (j in Vector.type) set(cfg, j = j, value = as.numeric(cfg[[j]]))
        } else { # if File_Server_cfg does not exist, Message of error
            
            my_message <- paste0("[CONFIG] ERROR, no server config file for the AirSensEUR box. \n", 
                                 "The App is going to crash. This AirSensEUR cannot be selected.\n")
            cat(my_message)
            shinyalert(
                title = "ERROR Config file",
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
        }
    } else { # if File_Server_cfg does not exist, Message of error
        
        my_message <- paste0("[CONFIG] ERROR, no server config file for the AirSensEUR box. \n", 
                             "The App is going to crash. This AirSensEUR cannot be selected.\n")
        cat(my_message)
        shinyalert(
            title = "ERROR Config file",
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
    }
    
    #=====================================================================================CR
    #  ASE_name,".cfg"
    #=====================================================================================CR
    # This is to insert both sensors and reference configuration into a dataframe and file
    File_cfg <- list.files(path = file.path(DisqueFieldtestDir,"General_data"), pattern = paste0(ASE_name,".cfg"))
    if (!identical(File_cfg,character(0))) {
        
        # reading the configuration files sens2ref
        File_cfg <- file.path(DisqueFieldtestDir,"General_data", paste0(ASE_name,".cfg"))
        if (file.exists(File_cfg)) {
            
            cat(paste0("[CONFIG] Info, the config file ", File_cfg, " for the configuration of AirSensEUR exists"), sep = "\n")
            sens2ref <- data.table::transpose(fread(file = File_cfg, header = FALSE, na.strings = c("","NA", "<NA>"), fill = TRUE), fill = NA, make.names = 1)
            sens2ref.order <- sens2ref$name.gas
            if (is.null(key(sens2ref))) sens2ref <- data.table::setkey(x = sens2ref, key = "name.gas")
            
            # Changing label of Cal.Line
            sens2ref$Cal.Line <- sapply(seq_along(sens2ref$Cal.Line), function(l){
                if (!is.na(sens2ref$Cal.Line[l])) {
                    if (sens2ref$Cal.Line[l] == "Previous calibration") {
                        return("Prediction with previous calibration")
                    } else if (sens2ref$Cal.Line[l] == "New calibration with current data") {
                        return("Calibration with current data")
                    } else if (sens2ref$Cal.Line[l] == "Calibration with slope and intercept") {
                        return("Calibration with slope and intercept")
                    } else return("Prediction with previous calibration")
                } else return(NA)
            })
            
            # Adding Sync.Cal and Sync.Pred if missing
            if (!"Sync.Cal"  %in% names(sens2ref)) sens2ref$Sync.Cal  <- FALSE 
            if (!"Sync.Pred" %in% names(sens2ref)) sens2ref$Sync.Pred <- FALSE
            
            # Adding ubss and ubsRM if missing, the between sampler uncertainties for sensor and reference method
            if (!"ubsRM" %in% names(sens2ref)) {
                if (!"uxi" %in% names(sens2ref)) {
                    sens2ref$ubsRM  <- rep(0, times = nrow(sens2ref))
                } else sens2ref$ubsRM  <- sens2ref$uxi
            } 
            if (!"ubss" %in% names(sens2ref)) {
                if (!"uxi" %in% names(sens2ref)) {
                    sens2ref$ubss  <- rep(0, times = nrow(sens2ref))
                } else {
                    sens2ref$ubss  <- sens2ref$uxi
                    sens2ref[, uxi := NULL]  
                } 
            }
            
            #change the type of column in df
            Vector.type <- c("Ref.rm.Out","Sens.Inval.Out","Apply.Invalid", "remove.neg","Sens.rm.Out","Neg.mod", "Sync.Cal" , "Sync.Pred")
            Vector.type <- Vector.type[Vector.type %in% names(sens2ref)]
            for (j in Vector.type) set(sens2ref, j = j, value = as.logical(gsub(" ","",sens2ref[[j]])))
            
            Vector.type <- c("Ref.window","Ref.threshold","Ref.Ymin","Ref.Ymax","Ref.ThresholdMin","Ref.iterations","Gain","Intercept","Slope",
                             "Sens.window","Sens.threshold","Sens.Ymin","Sens.Ymax","Sens.ThresholdMin","Sens.iterations", "ubsRM", "ubss",
                             "Rload","TIA_Gain","GAIN","Int_Z","Bias_Sign","Bias", "Ref","RefAD","RefAFE","board.zero.set","BIAIS",
                             "temp.thres.min","temp.thres.max","rh.thres.min","rh.thres.max","hoursWarming")
            Vector.type <- Vector.type[Vector.type %in% names(sens2ref)]
            for (j in Vector.type) set(sens2ref, j = j, value = as.numeric(gsub(" ","",sens2ref[[j]])))
            
        } else { # if ASE_name,".cfg", Message of error
            
            my_message <- paste0("[CONFIG] ERROR, no config file for the AirSensEUR box. \n", 
                                 "The App is going to crash. This AirSensEUR cannot be selected.\n")
            cat(my_message)
            shinyalert(
                title = "ERROR Config file",
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
        }
        
        # updating names of sensors with the sensor schield config file
        if (is.null(sens2ref.shield)) {
            
            # Reading chemical shield config file and merging with sens2ref if the file exists
            Shield.file <- file.path(DisqueFieldtest,"Shield_Files",cfg$asc.File)
            if (file.exists(Shield.file)) {
                sens2ref.shield <-ASEPanel04Read(ASEPanel04File = Shield.file)
            } else {
                
                # if ASE_name,".cfg", Message of error
                my_message <- paste0("[CONFIG] ERROR, no chemical shield config file for the AirSensEUR box. \n", 
                                     "The App is going to crash. This AirSensEUR cannot be selected.\n")
                shinyalert(
                    title = "ERROR Config file",
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
                return(my_message)
            }
            
            if (!"data.table" %in% class(sens2ref.shield)) sens2ref.shield <- data.table::data.table(sens2ref.shield, key = "name.gas")
            
            # Set same order as sens2ref for rows and columns
            sens2ref.shield      <- sens2ref.shield[match(na.omit(sens2ref.shield[["gas.sensor"]]), na.omit(sens2ref.shield[["gas.sensor"]]))]
            sens2ref.cols2change <- intersect(names(sens2ref), names(sens2ref.shield))
            setcolorder (sens2ref.shield, sens2ref.cols2change)
            
            # checking shield columns shall be changed
            if (!isTRUE(all.equal(sens2ref.shield, 
                                  sens2ref[match(na.omit(sens2ref.shield[["gas.sensor"]]), sens2ref$gas.sensor), ..sens2ref.cols2change], 
                                  check.attributes = TRUE, tolerance = 1E-5))) {
                
                #change the type of column in sens2ref.shield
                Vector.type <- c("Ref.rm.Out","Sens.Inval.Out","Apply.Invalid", "remove.neg","Sens.rm.Out","Neg.mod", "Sync.Cal" , "Sync.Pred")
                Vector.type <- Vector.type[Vector.type %in% names(sens2ref.shield)]
                for (j in Vector.type) set(sens2ref.shield, j = j, value = as.logical(gsub(" ","",sens2ref.shield[[j]])))
                
                Vector.type <- c("Ref.window","Ref.threshold","Ref.Ymin","Ref.Ymax","Ref.ThresholdMin","Ref.iterations","Gain","Intercept","Slope",
                                 "Sens.window","Sens.threshold","Sens.Ymin","Sens.Ymax","Sens.ThresholdMin","Sens.iterations", "ubsRM", "ubss",
                                 "Rload","TIA_Gain","GAIN","Int_Z","Bias_Sign","Bias", "Ref","RefAD","RefAFE","board.zero.set","BIAIS",
                                 "temp.thres.min","temp.thres.max","rh.thres.min","rh.thres.max","hoursWarming")
                Vector.type <- Vector.type[Vector.type %in% names(sens2ref.shield)]
                for (j in Vector.type) set(sens2ref.shield, j = j, value = as.numeric(gsub(" ","",sens2ref.shield[[j]])))
                
                set(sens2ref, i = match(na.omit(sens2ref.shield[["gas.sensor"]]), sens2ref$gas.sensor), j = sens2ref.cols2change, value = sens2ref.shield)
                
                # Saving new version
                fwrite(setDT(as.data.frame(t(sens2ref)), keep.rownames = "name.gas")[], file = file.path(DisqueFieldtestDir,"General_data",paste0(ASE_name,".cfg")), row.names = FALSE,col.names = FALSE)
            }
        }
    } else { # if File_cfg does not exist, , Message of error
        
        my_message <- paste0("[CONFIG] ERROR, no server config file for the AirSensEUR box. \n", 
                             "The App is going to crash. This AirSensEUR cannot be selected.\n")
        cat(my_message)
        shinyalert(
            title = "ERROR Config file",
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
        return(my_message)
    }
    
    # reading the files with Covariates to plot and covariates to calibrate
    for (i in 1:length(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)])) {
        
        nameFile <- file.path(DisqueFieldtestDir,"General_data",paste0(ASE_name,"_Covariates_",sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],".cfg"))
        nameGas  <- sens2ref[which(sens2ref$name.sensor == sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i]),"name.gas"]
        nameSens <- sens2ref[which(sens2ref$name.sensor == sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i]),"name.sensor"]
        
        if (file.exists(nameFile)) {
            
            cat(paste0("[CONFIG] INFO, the file with covariates to plot ", nameFile, " exists "), sep = "\n")
            assign(paste0(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i]), 
                   read.csv(file = nameFile,
                            header = TRUE, 
                            comment.char = "", 
                            stringsAsFactors = FALSE)
            )
        } else{
            
            cat(paste0("[CONFIG] ERROR, the file with covariates to plot ", nameFile, " does not exist. File is iniatized with the R script info."), sep = "\n")
            # DEFINE The lists of variables to plot in retrieving data () to understand the inerferences - use report of lab. tests
            if (nameGas == "CO") {
                
                assign(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i], 
                       data.frame(Effects = c(paste0(nameSens,"_volt"), "Out.Ref.CO_ppm" , "Relative_humidity", "Temperature"))) #####################################################
            } else {
                
                if (nameGas == "O3" || nameGas == "NO2") {
                    
                    assign(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i], 
                           data.frame(Effects = c(paste0(nameSens,"_volt"), "Out.Ref.NO2"    , "Out.Ref.O3"       , "Relative_humidity", "Temperature")))
                } else assign(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i], 
                              data.frame(Effects = c(paste0(nameSens,"_volt"), "Out.Ref.NO"     , "Relative_humidity", "Temperature")))
            } 
            
            # Saving the effect files
            SENS <- get(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i])
            write.csv(SENS, file = nameFile, row.names = FALSE)
            rm(SENS)
        } 
        
        # Covariates to calibrate
        nameFile <- file.path(DisqueFieldtestDir,"General_data",paste0(ASE_name,"_CovMod_",sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],".cfg"))
        if (file.exists(nameFile) && nrow(read_csv(nameFile, col_types = cols(Effects = col_character()))) > 0) {
            cat(paste0("[CONFIG] INFO, the file with covariates to calibrate ", nameFile, " exists "), sep = "\n")
            assign(paste0(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],"CovMod"), 
                   read.csv(file = nameFile, 
                            header = TRUE, 
                            comment.char = "#", 
                            stringsAsFactors = FALSE)
            )
            
        } else{
            
            cat(paste0("[CONFIG] ERROR, the file with covariates to calibrate ", nameFile, " does not exist. File is iniatized with the R script info."), sep = "\n")
            # DEFINE The lists of variables to plot in retrieving data () to understand the inerferences - use report of lab. tests
            assign(paste0(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],"CovMod"), 
                   data.frame(Effects = c(paste0(nameSens,"_volt"), "Temperature")))
            
            # Saving the effect files
            SENS <- get(paste0(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],"CovMod"))
            write.csv(SENS, file = nameFile, row.names = FALSE)
            rm(SENS)
        } 
        
    }
    
    Covariates <- lapply(which(!is.na(sens2ref$name.sensor)), function(i) get(sens2ref$name.sensor[i]) )
    names(Covariates) <- paste0(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)])
    
    CovMod <- lapply(which(!is.na(sens2ref$name.sensor)), function(i) get(paste0(sens2ref$name.sensor[i],"CovMod")))
    names(CovMod) <- paste0(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)])
    
    cat("-----------------------------------------------------------------------------------\n")
    return.CONFIG <- list(cfg,sens2ref,Covariates,CovMod, sens2ref.shield)
    names(return.CONFIG) <- c("Server","sens2ref","CovPlot","CovMod", "sens2ref.shield")
    
    return(return.CONFIG)
}

#=====================================================================================CR
# Valid Periods                                                                 
#=====================================================================================CR
SETTIME <- function(DisqueFieldtestDir, General.t.Valid = NULL, Influx.TZ = "UTC" , SOS.TZ = "UTC", Ref.TZ = "UTC", DownloadSensor, Config = NULL, 
                    sens2ref.shield = NULL) {
    # Return             : list with sens2ref (only time parameters)
    
    # DisqueFieldtestDir : file.path where the config files of the AIrSensEUR are located. The directory ""Shield_Files" shall be located at the pareent directory
    # General.t.Valid    : dataframe with date , sensor and reference data, default is NULL, it is only use if the the File_SETTIME_cfg file does not exist
    # DownloadSensor     : output of function DownloadSensor()
    # Influx.TZ          : String, time zone of INFLUXDB data, default is UTC
    # SOS.TZ             : String, time zone of SOS data, default is UTC
    # Ref.TZ             : String, time zone of Reference data, default is UTC
    # Config             : List, default is null, list returned by function CONFIG()
    
    cat("-----------------------------------------------------------------------------------\n")
    ASE_name           <- basename(DisqueFieldtestDir)
    
    # Setting the General time zone to the one of DownloadSensor$DateIN.General.prev 
    # or DateIN.Influx.prev or DateIN.SOS.prev otherwise it is set to "UTC"
    if (exists("DownloadSensor")) {
        if (!is.null(DownloadSensor$DateIN.General.prev) && !is.na(DownloadSensor$DateIN.General.prev)) {
            General.TZ <- base::format(DownloadSensor$DateIN.General.prev, format= "%Z")
        } else {
            if (!is.null(DownloadSensor$DateIN.Influx.prev) && !is.na(DownloadSensor$DateIN.Influx.prev)) {
                General.TZ <- base::format(DownloadSensor$DateIN.Influx.prev, format= "%Z")
            } else {
                if (!is.null(DownloadSensor$DateIN.SOS.prev) && !is.na(DownloadSensor$DateIN.SOS.prev)) {
                    General.TZ <- base::format(DownloadSensor$DateIN.SOS.prev, format= "%Z")
                } else General.TZ <- "UTC"  
            }
        }
    } else  General.TZ <- "UTC"
    
    # Read SetTime file 
    File_SETTIME_cfg   <- list.files(path = file.path(DisqueFieldtestDir, "General_data"), pattern = paste0(ASE_name,"_SETTIME.cfg")  )
    if (!identical(File_SETTIME_cfg,character(0))) {
        
        # reading the configuration files sens2ref
        File_SETTIME_cfg <- file.path(DisqueFieldtestDir, "General_data", paste0(ASE_name,"_SETTIME",".cfg"))
        if (file.exists(File_SETTIME_cfg)) {
            cat(paste0("[SETTIME] Info, the config file ", File_SETTIME_cfg, " for the configuration of AirSensEUR exists"), sep = "\n")
            sens2ref <- data.table::fread(file = File_SETTIME_cfg, header = TRUE, na.strings=c("","NA", "<NA>"))
            # sens2ref.order <- sens2ref$name.gas
            # if (is.null(key(sens2ref))) sens2ref <- data.table::setkey(x = sens2ref, key = "name.gas")
            
        } else { # sens2Ref missing, error message
            
            my_message <- paste0("[SETTIME] ERROR, no SetTime server config file for the AirSensEUR box. \n", 
                                 "The App is going to crash. This AirSensEUR cannot be selected.\n")
            cat(my_message)
            shinyalert(
                title = "ERROR Config file",
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
                animation = FALSE)}
    } else {
        
        # sens2Ref missing, error message
        my_message <- paste0("[SETTIME] ERROR, no SetTime server config file for the AirSensEUR box. \n", 
                             "The App is going to crash. This AirSensEUR cannot be selected.\n")
        cat(my_message)
        shinyalert(
            title = "ERROR Config file",
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
            animation = FALSE)}
    
    # Setting the chemical shield config file
    if (is.null(sens2ref.shield)) {
        
        # Changing the name.sensor when asc.File changes
        # First  read the -server.cfg file to get the file name of the shield config file
        if (is.null(Config) || !"sens2ref" %in% names(Config) || is.null(Config[["sens2ref"]])) {
            
            File_Server_cfg    <- file.path(DisqueFieldtestDir, "General_data", paste0(ASE_name,"_Servers.cfg"))
            if (file.exists(File_Server_cfg)) {
                
                cfg <- transpose(fread(file = File_Server_cfg, header = FALSE, na.strings=c("","NA", "<NA>")), fill = NA, make.names = 1)
                
                # Changes names for change Shiny App version 0.9 to 0.10
                Change.names <- rbind(c("TZ"           , "Influx.TZ"),     # Time zone of Influx data
                                      c("sens.tzone"   , "SOS.TZ"))         # Time zone of SOS data
                for (k in 1:nrow(Change.names)) if (Change.names[k,1] %in% colnames(cfg)) colnames(cfg)[colnames(cfg) == Change.names[k,1]] <- Change.names[k,2]
                
            } else cat(paste0("[SETTIME] The file of server configuration for AirSensEUR: ", File_Server_cfg, " does not exist.\n"))
        } else cfg <- Config[["Server"]]
        
        
        # Second read the shield config file to get the sensor names
        if (file.exists(file.path(dirname(DisqueFieldtestDir),"Shield_Files",cfg$asc.File))) {
            
            sens2ref.shield <- ASEPanel04Read(ASEPanel04File = file.path(dirname(DisqueFieldtestDir),"Shield_Files",cfg$asc.File))
            
        }  else cat("[SETTIME] ERROR shield file (asc.File) not found\n")} 
    
    # update the name of sensors in the SETTIME.cfg
    Save.sens2ref <- FALSE
    if (!all(sens2ref.shield$name.sensor %in% na.omit(unlist(sens2ref[which(name.gas == "name.sensor"), .SD])[-1]))) {
        name.sensor2Change <- which(!sens2ref.shield$name.sensor %in% na.omit(unlist(sens2ref[which(name.gas == "name.sensor"), .SD])[-1]))
        for (i in name.sensor2Change) {
            set(sens2ref, i = which(sens2ref$name.gas  == "name.sensor"), 
                j = which(names(sens2ref) %in% sens2ref.shield$name.gas[i]), value = sens2ref.shield$name.sensor[i])
        } 
        
        # set to save file
        Save.sens2ref}
    
    # transpose the data.table
    sens2ref <- cbind(names(sens2ref)[-1],transpose(sens2ref, fill = NA, make.names =  "name.gas"))
    setnames(sens2ref, c("name.gas",names(sens2ref)[-1]) )
    
    # Changes names for change Shiny App version 0.6 to 0.7
    Change.names <- rbind(c("RefDateINPlot"   ,"Out.Ref.IN"),       # for plotting outlier of reference data
                          c("RefDateENDPlot"  ,"Out.Ref.END"),      # for plotting outlier of reference data
                          c("DateINPlot"      ,"Out.Sens.IN"),      # for plotting outlier of sensor data
                          c("DateENPlot"      ,"Out.Sens.END"),     # for plotting outlier of reference data
                          c("DateIN"          ,"Valid.IN"),         # to change the date range of all controls in  Set time 
                          c("DateEND"         ,"Valid.END"),        # to change the date range of all controls in  Set time 
                          c("DateINCal"       ,"DateCal.IN"),       # date range for calibration
                          c("DateENDCal"      ,"DateCal.END"),      # date range for calibration
                          c("DateINmeasPlot"  ,"DatePlotCal.IN"),   # date range to plot calibration
                          c("DateENDmeasPlot" ,"DatePlotCal.END"),  # date range to plot calibration
                          c("DateINmeas"      ,"DateMeas.IN"),      # date range for extrapolation
                          c("Datemeas.IN"     ,"DateMeas.IN"),      # date range for extrapolation
                          c("DateENDmeas"     ,"DateMeas.END"),     # date range for extrapolation
                          c("Datemeas.END"    ,"DateMeas.END"),     # date range for extrapolation
                          c("DateINExtraPlot" ,"DatePlotMeas.IN"),  # date range to plot extrapolation
                          c("DatePlotmeas.IN" ,"DatePlotMeas.IN"),  # date range to plot extrapolation
                          c("DateENDExtraPlot","DatePlotMeas.END"), # date range to plot extrapolation
                          c("DatePlotmeas.END","DatePlotMeas.END")  # date range to plot extrapolation
    )
    if (any(Change.names[,1] %in% names(sens2ref))) {
        Names2Change <- which(Change.names[,1] %in% names(sens2ref))
        for (k in Names2Change) colnames(sens2ref)[colnames(sens2ref) == Change.names[k,1]] <- Change.names[k,2]
        
        # set to save file
        Save.sens2ref}
    # adding "Cov.Date.IN" "Cov.Date.END"if missing
    if (!"Cov.Date.IN"  %in% names(sens2ref)) {
        sens2ref <-  cbind(sens2ref, sens2ref[,Valid.IN])
        setnames(sens2ref, length(names(sens2ref)), "Cov.Date.IN")
        
        # set to save file
        Save.sens2ref}
    
    if (!"Cov.Date.END" %in% names(sens2ref)) {
        sens2ref <-  cbind(sens2ref, sens2ref[,Valid.END])
        setnames(sens2ref, length(names(sens2ref)), "Cov.Date.END")
        
        # set to save file
        Save.sens2ref} 
    
    # coerce Sens.Inval.Out  and "Apply.Invalid" to logical
    Vector.type <- c("Sens.Inval.Out", "Apply.Invalid")
    Vector.type <- Vector.type[Vector.type %in% names(sens2ref)]
    for (j in Vector.type) set(sens2ref, j = j, value = as.logical(gsub(" ","",sens2ref[[j]])))
    
    # coerce chr of dates to POSIXct with Time Zone of General
    Vector.type <- c("Out.Ref.IN"     , "Out.Ref.END",
                     "Out.Sens.IN"    , "Out.Sens.END",
                     "Valid.IN"       , "Valid.END",
                     "Cov.Date.IN"    , "Cov.Date.END", 
                     "DateCal.IN"     , "DateCal.END",
                     "DatePlotCal.IN" , "DatePlotCal.END",
                     "DateMeas.IN"    , "DateMeas.END", 
                     "DatePlotMeas.IN", "DatePlotMeas.END" )
    Vector.type <- Vector.type[Vector.type %in% names(sens2ref)]
    for (k in Vector.type) set(sens2ref, j = k, value = parse_date_time(sens2ref[[k]], tz = General.TZ, orders = c("YmdHMS","YmdHM")))
    
    # Checking validity of dates and correcting if needed
    if ((is.POSIXct(DownloadSensor$DateIN.General.prev) || is.POSIXct(DownloadSensor$DateIN.Influx.prev) || is.POSIXct(DownloadSensor$DateIN.SOS.prev) || is.POSIXct(DownloadSensor$DateIN.Ref.prev))
        && (is.POSIXct(DownloadSensor$DateEND.General.prev) || is.POSIXct(DownloadSensor$DateEND.Influx.prev) || is.POSIXct(DownloadSensor$DateEND.SOS.prev) || is.POSIXct(DownloadSensor$DateEND.Ref.prev))) {
        
        if (is.POSIXct(DownloadSensor$DateIN.General.prev)) DateIN <- DownloadSensor$DateIN.General.prev else {
            if (is.POSIXct(DownloadSensor$DateIN.Influx.prev)) DateIN <- DownloadSensor$DateIN.Influx.prev else {
                if (is.POSIXct(DownloadSensor$DateIN.SOS.prev)) DateIN <- DownloadSensor$DateIN.SOS.prev else {
                    if (is.POSIXct(DownloadSensor$DateIN.Ref.prev)) DateIN <- DownloadSensor$DateIN.Ref.prev
                }
            }
        }
        if (is.POSIXct(DownloadSensor$DateEND.General.prev)) DateEND <- DownloadSensor$DateEND.General.prev else {
            if (is.POSIXct(DownloadSensor$DateEND.Influx.prev)) DateEND <- DownloadSensor$DateEND.Influx.prev else {
                if (is.POSIXct(DownloadSensor$DateEND.SOS.prev)) DateEND <- DownloadSensor$DateEND.SOS.prev else {
                    if (is.POSIXct(DownloadSensor$DateEND.Ref.prev)) DateEND <- DownloadSensor$DateEND.Ref.prev
                }
            }
        }
        
        if (any(is.na(sens2ref$Valid.IN))) {
            sens2ref$Valid.IN[which(is.na(sens2ref$Valid.IN))]      <- DateIN
            
            # set to save file
            Save.sens2ref <- TRUE
        }
        if (any(sens2ref$Valid.IN < DateIN)) {
            sens2ref$Valid.IN[which(sens2ref$Valid.IN < DateIN)]    <- DateIN
            
            # set to save file
            Save.sens2ref <- TRUE
        }   
        if (any(is.na(sens2ref$Valid.END))) {
            sens2ref$Valid.END[which(is.na(sens2ref$Valid.END))]    <- DateEND
            
            # set to save file
            Save.sens2ref <- TRUE
        }                                
        if (any(sens2ref$Valid.END < DateEND)) {
            sens2ref$Valid.END[which(sens2ref$Valid.END < DateEND)] <- DateEND
            
            # set to save file
            Save.sens2ref <- TRUE
        } 
        
        Check_Dates.IN <- c("Out.Ref.IN", "Out.Sens.IN", "Cov.Date.IN", "DateCal.IN", "DatePlotCal.IN", "DateMeas.IN", "DatePlotMeas.IN")
        for (i in Check_Dates.IN) {
            if (any(is.na(sens2ref[[i]]))) {
                sens2ref[[i]][which(is.na(sens2ref[[i]]))]               <- sens2ref$Valid.IN[which(is.na(sens2ref[[i]]))]
                
                # set to save file
                Save.sens2ref <- TRUE
            }                 
            if (any(sens2ref[[i]] < sens2ref$Valid.IN)){
                sens2ref[[i]][which(sens2ref[[i]] < sens2ref$Valid.IN)]  <- sens2ref$Valid.IN[which(sens2ref[[i]] < sens2ref$Valid.IN)]
                
                # set to save file
                Save.sens2ref <- TRUE
            }    
        }
        Check_Dates.END <- c("Out.Ref.END", "Out.Sens.END", "Cov.Date.END", "DateCal.END", "DatePlotCal.END", "DateMeas.END", "DatePlotMeas.END")
        for (i in Check_Dates.END) {
            if (any(is.na(sens2ref[[i]]))) {
                sens2ref[[i]][which(is.na(sens2ref[[i]]))]               <- sens2ref$Valid.END[which(is.na(sens2ref[[i]]))]
                
                # set to save file
                Save.sens2ref <- TRUE
            }                 
            if (any(sens2ref[[i]] > sens2ref$Valid.END)) {
                sens2ref[[i]][which(sens2ref[[i]] < sens2ref$Valid.END)] <- sens2ref$Valid.END[which(sens2ref[[i]] < sens2ref$Valid.END)]
                
                # set to save file
                Save.sens2ref <- TRUE
            }   
        }
    }
    if (Save.sens2ref) {
        # Saving config file
        fwrite(setDT(as.data.frame(t(sens2ref)), keep.rownames = "name.gas")[], file = File_SETTIME_cfg, row.names = FALSE,col.names = FALSE)
        cat("[SETTIME] INFO Saving the ASE_name_SETTIME.cfg with correct name of sensors.\n")}
    
    cat("-----------------------------------------------------------------------------------\n")
    return(sens2ref)
}

#=====================================================================================CR
# Function to read the last lines of the log file (Vs 1701210)
#=====================================================================================CR
ReadLastLines <- function(filepath, n=NULL) {
    # https://stackoverflow.com/questions/5596107/reading-the-last-n-lines-from-a-huge-text-file
    # filepath; log file to read
    # n : number of lines to read
    # skip : number of lines to skip
    # 
    con <- file(filepath)
    open(con)
    out <- scan(con, n, what = "char(0)",sep = "\n", quiet = TRUE)
    
    while (TRUE) {
        tmp <- scan(con,1,what = "char(0)",sep = "\n",quiet = TRUE)
        if (length(tmp) == 0) {close(con) ; break }
        out <- c(out[-1],tmp)
    }
    out
}

#=====================================================================================CR
# Function to plot correlation matrix (Vs 1701210)
#=====================================================================================CR
panel.smooth <- function(x, y, col = "blue", bg = NA, pch = 1, cex = 0.6, col.smooth = "red", span = 2/3, iter = 3, ...) { 
    #For panel.smooth() function defined cex=, col = and pch = arguments.
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) 
        lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
              col = col.smooth, ...)
}
panel.cor <- function(x, y, digits=3, prefix= "", cex.cor = 2) {
    
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r1  <- cor(x, y, use = "pairwise.complete.obs")
    r   <- abs(cor(x, y,use = "pairwise.complete.obs"))
    txt <- format(c(r1^2, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (missing(cex.cor)) { cex <- 0.8/strwidth(txt) } else {
        cex = cex.cor}
    text(0.5, 0.75, txt, cex = cex * r)
    lm.r <- lm(y ~ x)
    txt <- paste0("y = ",
                  format(coef(lm.r)[2], 
                         digits = 3, 
                         scientific = T
                  ),
                  " x + ",
                  format(coef(lm.r)[1], 
                         digits = 2, 
                         scientific = T
                  )
    )
    text(0.5, 0.25, txt, cex = 0.8/strwidth(txt))
}
panel.hist <- function(x, ...) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "red", ...)
}

#=====================================================================================CR
# Function format, min, max, steps for Shiny sliderInput
#=====================================================================================CR
dateFormat <- function(dateRange = NULL, Value1 = NULL, Value2 = NULL, nb.Day.Limit = 25) {
    # dateRange     : vector of Posix, default NULL containg all dates
    # Value1      : Posix, default null, minimum date to use for an input slider
    # Value2      : Posix, default null, maximum date to use for an input slider
    # nb.Day.Limit  : humeric, default 25, limit of length of date range in days to switch between '%Y-%m-%d %H:%M' and '%Y-%m-%d' format
    # return        : string format of date to use with or without hours and mins : '%Y-%m-%d %H:%M' or '%Y-%m-%d'
    
    # Checking that datRange or Value1, Value2 exist. if not return '%Y-%m-%d'
    if (is.null(dateRange) & is.null(Value1) & is.null(Value2))  return('%Y-%m-%d') else {
        # Using Value1, Value2 of the min and max of dateRange
        if (is.null(Value1)) minDate <- min(dateRange, na.rm = TRUE) else minDate <- Value1 
        if (is.null(Value2)) maxDate <- max(dateRange, na.rm = TRUE) else maxDate <- Value2 
        if (difftime(maxDate, minDate, units = "days") >= nb.Day.Limit) {
            return('%Y-%m-%d')
        } else {
            return('%Y-%m-%d %H:%M')
        }
    }
}
dateStep <- function(dateRange = NULL, Min = NULL, Max = NULL, Value2 = NULL, Value1 = NULL) {
    # dateRange     : vector of Posix conating all data
    # Value1      : Posix, default null, minimum date to use for an input slider
    # Value2      : Posix, default null, maximum date to use for an input slider
    # return        : string format of date to use with or without hour : '%Y-%m-%d %H:%M' or '%Y-%m-%d' if dateRange is more than 25 days days
    
    # Checking that datRange or Value1, Value2 exist. if not return '%Y-%m-%d'
    if (is.null(dateRange) & is.null(Value1) & is.null(Value2))  return('%Y-%m-%d %H:%M') else {
        # Using Value1, Value2 of the min and max of dateRange
        if (is.null(Value1)) minDate <- min(dateRange, na.rm = TRUE) else minDate <- Value1 
        if (is.null(Value2)) maxDate <- max(dateRange, na.rm = TRUE) else maxDate <- Value2 
        if (difftime(maxDate, minDate, units = "days") >= 25) {
            return('%Y-%m-%d')
        } else {
            return('%Y-%m-%d %H:%M')
        }
    }
}
max.dateRange <- function(dateRange = NULL, Value1 = NULL, Value2 = NULL, ValidMin = NULL, ValidMax = NULL) {
    # Function to update the max of sliderInput
    # Input
    # dateRange   : vector of Posix containg all available dates
    # Value1      : Posix, default null, minimum date to use for an input slider
    # Value2      : Posix, default null, maximum date to use for an input slider
    # ValidMin, ValidMax : Posix, default NUll, that are used to limit dateRange
    # 
    # return      : Posix, the max of a shiny sliderInput.
    #               1 - If dateRange and Value2 are both NULL, this function cannot return a max value for sliderInout. Stopping the function.
    #               2 - If dateRange is NULL, but Value2 is not NULL while Value1 is null then Value2 is returned (maybe this is a mistake?)
    #               3 - If dateRange is NULL, but Value1 and Value2 are not NULL then Value2 + 0.66 x (Value2 -valie1) is returned
    #               4 - If dateRange is not null while Value2 is null, the max of dateRange is returned.
    #               5 - If dateRange and Value2 are not null while Value1 is null, the max date returned is at least Value1 - 66 % x (max(dateTange) - Value2)) without exceeding limit max(dateRange)
    #               6 - If dateRange, Value1 and Value2 are not null,              the max date returned is at least Value1 - 66 % x (Value2   - Value2))       without exceeding limit max(dateRange)
    
    # Checking that datRange or Min, Max exist. if not return '%Y-%m-%d'
    if (is.null(dateRange)) {
        if (is.null(Value2)) {
            # 1
            return(cat("[max.dateRange] ERROR, dateRange and Value2 are both NULL, this function cannot return a max value for sliderInout. Stopping the function." )) 
        } else if (is.null(Value1)) {
            # 2
            return(Value2)   
        } else {
            # 3
            dateSpan = difftime(Value2, Value1)
            return(Value2 + 0.66 * dateSpan)
        }
    } else {
        
        # limiting dateRange with ValidMIn and ValidMax
        if (!(is.null(ValidMin) & is.null(ValidMax))) dateRange <- subset(dateRange, dateRange >= ValidMin &  dateRange <= ValidMax)
        
        if (is.null(Value2)) {
            # 4
            return(max(dateRange, na.rm = TRUE))
        } else {
            if (is.null(Value1)) {
                # 5 
                dateSpan = difftime(Value2, min(dateRange, na.rm = TRUE))
                if (Value2 + 0.66 * dateSpan > max(dateRange, na.rm = TRUE)) return(max(dateRange, na.rm = TRUE)) else return(Value2 + 0.66 * dateSpan)
            } else {
                # 6 
                dateSpan = difftime(Value2, Value1)
                if (Value2 + 0.66 * dateSpan > max(dateRange, na.rm = TRUE)) return(max(dateRange, na.rm = TRUE)) else return(Value2 + 0.66 * dateSpan)
            }
        } 
    }
}

min.dateRange <- function(dateRange = NULL, Value1 = NULL, Value2 = NULL, ValidMin = NULL, ValidMax = NULL) {
    # Function to update the min of sliderInput
    # Input
    # dateRange   : vector of Posix containg all available dates
    # Value1      : Posix, default null, minimum date to use for an input slider
    # Value2      : Posix, default null, maximum date to use for an input slider, if 
    # ValidMin, ValidMax : Posix, default NUll, that are used to limit dateRange
    # 
    # return      : Posix, the min of a shiny sliderInput.
    #               1 - If dateRange and Value1 are both NULL, this function cannot return a min value for sliderInout. Stopping the function.
    #               2 - If dateRange is NULL, but Value1 is not NULL while Value2 is null then Value1 is returned (maybe this is a mistake?).
    #               3 - If dateRange is NULL, but Value1 and Value2 are not NULL then Value1 - 0.66 x(Value2 - Value1) is returned.
    #               4 - If dateRange is not null while Value1 is null, the min of dateRange is returned.
    #               5 - If dateRange and Value1 are not null while Value2 is null, the min date returned is at least Value1 - 66 % x (max(dateTange) - minRange)) without exceeding limit min(dateRange)
    #               6 - If dateRange, Value1 and Value2 are not null,              the min date returned is at least Value1 - 66 % x (max(maxRange   - minRange)) without exceeding limit min(dateRange)
    
    # Checking if datRange, Min and Max exist. if not return '%Y-%m-%d'
    if (is.null(dateRange)) {
        if (is.null(Value1)) {
            # 1
            return(cat("[max.dateRange] ERROR, dateRange and Value1 are both NULL, this function cannot return a min value for sliderInout. Stopping the function.\n" )) 
        } else if (is.null(Value2)) {
            # 2
            return(Value1)   
        } else {
            # 3
            dateSpan = difftime(Value1, Value2)
            return(Value1 + 0.66 * dateSpan)
        }
    } else {
        
        # limiting dateRange with ValidMIn and ValidMax
        if (!(is.null(ValidMin) & is.null(ValidMax))) dateRange <- subset(dateRange, dateRange >= ValidMin &  dateRange <= ValidMax)
        
        if (is.null(Value1)) { 
            # 4
            return(min(dateRange, na.rm = TRUE))
        } else {
            if (is.null(Value2)) {
                # 5
                dateSpan = difftime(Value1, max(dateRange, na.rm = TRUE))
                if (Value1 + 0.66 * dateSpan < min(dateRange, na.rm = TRUE)) return(min(dateRange, na.rm = TRUE)) else return(Value1 + 0.66 * dateSpan)
            } else {
                dateSpan = difftime(Value1, Value2)
                if (Value1 + 0.66 * dateSpan < min(dateRange, na.rm = TRUE)) return(min(dateRange, na.rm = TRUE)) else return(Value1 + 0.66 * dateSpan)
            }
        } 
    }
}

AboutVersions <- function(DisqueFieldtest, FirstLineText, LastLineText) {
    # DisqueFieldtest : character vector, filepath of the text file to use
    # FirstLineText   : Character vector, first line to select containing FirstLineText
    # LastLineText    : Character vector, last line to select containing LastLineText
    # Return a chacracter vector with all lines between FirstLineText and LastLineText
    
    ## Create connection
    con <- file(description=file.path(DisqueFieldtest,"Versions.R"), 
                open= "r")
    
    ## Reading App.R
    Com <- readLines(con, n=-1)
    
    # Close connection
    close(con)
    
    # Select Lines to render
    return(
        cat(
            gsub(pattern     = "# ", 
                 replacement = "",
                 Com[(which(Com == FirstLineText)+2):(which(Com == LastLineText)-3)]
            ), 
            sep = "\n"
        )
    )    
}

editTable <- function(DF, outdir=getwd(), outfilename= "table") {
    ui <- shinyUI(fluidPage(
        
        titlePanel("Edit and save a table"),
        sidebarLayout(
            sidebarPanel(
                helpText("Shiny app based on an example given in the rhandsontable package.", 
                         "Right-click on the table to delete/insert rows.", 
                         "Double-click on a cell to edit"),
                
                wellPanel(
                    h3("Table options"),
                    radioButtons("useType", "Use Data Types", c("TRUE", "FALSE"))
                ),
                br(), 
                
                wellPanel(
                    h3("Save table"), 
                    div(class='row', 
                        div(class= "col-sm-6", 
                            actionButton("save", "Save")),
                        div(class= "col-sm-6",
                            radioButtons("fileType", "File type", c("ASCII", "RDS")))
                    )
                )
                
            ),
            
            mainPanel(
                wellPanel(
                    uiOutput("message", inline=TRUE)
                ),
                
                actionButton("cancel", "Cancel last action"),
                br(), br(), 
                
                rHandsontableOutput("hot"),
                br(),
                
                wellPanel(
                    h3("Add a column"),
                    div(class='row', 
                        div(class= "col-sm-5", 
                            uiOutput("ui_newcolname"),
                            actionButton("addcolumn", "Add")),
                        div(class= "col-sm-4", 
                            radioButtons("newcolumntype", "Type", c("integer", "double", "character"))),
                        div(class= "col-sm-3")
                    )
                )
                
            )
        )
    ))
    
    server <- shinyServer(function(input, output) {
        
        values <- reactiveValues()
        
        ## Handsontable
        observe({
            if (!is.null(input$hot)) {
                values[["previous"]] <- isolate(values[["DF"]])
                DF = hot_to_r(input$hot)
            } else {
                if (is.null(values[["DF"]]))
                    DF <- DF
                else
                    DF <- values[["DF"]]
            }
            values[["DF"]] <- DF
        })
        
        output$hot <- renderRHandsontable({
            DF <- values[["DF"]]
            if (!is.null(DF))
                rhandsontable(DF, useTypes = as.logical(input$useType), stretchH = "all")
        })
        
        ## Save 
        observeEvent(input$save, {
            fileType <- isolate(input$fileType)
            finalDF <- isolate(values[["DF"]])
            if (fileType == "ASCII") {
                dput(finalDF, file=file.path(outdir, sprintf("%s.txt", outfilename)))
            }
            else{
                saveRDS(finalDF, file=file.path(outdir, sprintf("%s.rds", outfilename)))
            }
        }
        )
        
        ## Cancel last action    
        observeEvent(input$cancel, {
            if (!is.null(isolate(values[["previous"]]))) values[["DF"]] <- isolate(values[["previous"]])
        })
        
        ## Add column
        output$ui_newcolname <- renderUI({
            textInput("newcolumnname", "Name", sprintf("newcol%s", 1+ncol(values[["DF"]])))
        })
        observeEvent(input$addcolumn, {
            DF <- isolate(values[["DF"]])
            values[["previous"]] <- DF
            newcolumn <- eval(parse(text=sprintf('%s(nrow(DF))', isolate(input$newcolumntype))))
            values[["DF"]] <- setNames(cbind(DF, newcolumn, stringsAsFactors=FALSE), c(names(DF), isolate(input$newcolumnname)))
        })
        
        ## Message
        output$message <- renderUI({
            if (input$save==0) {
                helpText(sprintf("This table will be saved in folder \"%s\" once you press the Save button.", outdir))
            }else{
                outfile <- ifelse(isolate(input$fileType) == "ASCII", "table.txt", "table.rds")
                fun <- ifelse(isolate(input$fileType) == "ASCII", "dget", "readRDS")
                list(helpText(sprintf("File saved: \"%s\".", file.path(outdir, outfile))),
                     helpText(sprintf("Type %s(\"%s\") to get it.", fun, outfile)))
            }
        })
        
    })
    
    ## run app 
    runApp(list(ui=ui, server=server))
    return(invisible())
}

#================================================================CR
### Target.Param: Function to plot a Modified U Target Diagram (Vs 180428)
#================================================================CR
Target.Diagram <- function(Sensor_name, Mat, ubsRM = NULL, ubss = NULL, b0 = NULL, b1 = NULL,  Unit.Ref = NULL,
                           xAxisLabel = NULL, yAxisLabel = NULL, Xlim = NA, Ylim = NA,
                           DQO.1 = NA, DQO.2 = NA, DQO.3 = NA, LAT = NA, UAT = NA, LV = NA, AT = NA, CL = NA,
                           Disk = NA, WD = NA, Dir = NA, sdm_sdo = FALSE, SavePlot = TRUE, 
                           Model.used = NULL, BeginEnd = NULL, Time.average = NULL) {
    
    # Sensor_name               : name of the sensor to be written in front of the calibration equation. If NULL, do not print sensor name.
    # Mat                       : DataFrame of data including Case number, Date, xis, y, optional ubsRM if ubsRM is not constant for all reference values,
    #                             "Rel.bias", "Rel.RSS". "Rel.bias", "Rel.RSS", "xis" msut be included into dataFrame Mat.
    # ubsRM                     : numeric (default = NULL ), random standard uncertainty of the results of the reference method, xis, given as a constant value for all xis reference values
    # ubss                      : numeric (default = NULL ), Between standard uncertainty of the sensor, xis, given as a constant value for all xis reference values
    # b0 , b1                   : numeric, intercept and slope of the orthgonal regression, default: NULL. If not NULL b0/xi and b1 - 1 are plotted
    # Unit.Ref                  : Character, default is NULL. Unit of refrence values, can "ppm", "ppb", "ug/m3", "mg/m3" ...
    # Xlabel, Ylabel            : label On the x And y axis
    # Xlim, Ylim                : limits of x and y axis, default values is NA, vectors of two values min and max values
    # Title                     : title to appear On the top of the scatter plot of x And y values
    # DQO.1, DQO.2, 
    # DQO.3                     : numeric, data qualtiy objective for Indicative measurements, Modelling and objective estimation. The DQOs are  expressed in percentage, 
    #                             defaul NA, if NA no DQO target circle is plotted
    # LAT, UAT, LV, 
    # AT, CL                    : numeric, lower and upper assessment threshold, limit value, Alert threshold and Critical level of the European Air Quality Directive for Mat$xis, 
    #                             same unit as Mat$xis, default value = NA, used for color scale and target circles
    # Units                     : character vector, units for the expanded uncertainty, xis, yis
    # Disk, WD, Dir             : where you put the graphic files (Disk, working directory, directory), It is sufficient if only Dir is supplied
    # lim                       : passing variable for the limits of the Etalonnage function (cbind(c(minX,maxX),c(minY,maxY)) or NULL)
    # variable.ubsRM              : logical, if FALSE (default = FALSE ), ubsRM is used as constant random standard uncertainties for all xis reference values. 
    #                             If TRUE ubsRM given in Mat and is used for each reference values
    # f_coef1, f_coef2, f_R2    : number of digit for intercept, slope and R2 using sprintf syntax. 
    #                             f_coef1 is used both for intercept and s(Res), f_coef2 is used for all parameters of the model apart from the intercept
    # nameModel                 : name of model to be used to save uncertainty plots, character, default NULL
    # SavePlot                  : logical, default is TRUE if TRUE uncertainty plts are saved
    # Disk, WD, Dir             : where you put the graphic files (Disk, working directory, directory), It is sufficient if only Dir is supplied
    # sdm_sdo                   : logical, TRUE id the standard devaition of sensor measurements or model is lower than the one of eference measurements (observation)
    # SavePlot                  : logical, default is TRUE if TRUE uncertainty plts are saved
    # Model.used                : character, defualt is null. name of calibration model used
    # BeginEnd                  : character vector representing the starting and ending date, default is null
    # Time.average              : numeric, default is null. Tme average in minutes
    
    # return                    : nothing, target diagram is savved as pdf file
    
    #---------------------------CR
    # WHEN TO USE THIS SCRIPT 
    #---------------------------CR
    # Use this script when the following is true:
    # * Inside your dataframe, you have three sets of numeric data stored as three separate columns ("yis", "Rel.bias", "Rel.RSS"), and the individual data points + slope and 
    # * intercept of an orhtognal regression fitted to Mat$yis inside the two are related to each other (or 'paired'). This is applicable, for instance, whenever a single
    # * event/phenomenon/subject simultaneously yields two data points.
    # * You wish to plot these numbers onto a 'scatterplot', where the horizontal axis corresponds to one of your columns of numbers and the vertical axis corresponds to the other. 
    # * Each individual pair of numbers, then, is represented as a point in this two-dimensional space.
    # * You have one other additional column with labels in it (e.g. telling you which experimental condition each pair of data points came from), and you want to encode these labels
    # * into the plot as the different colors of the points in the scatterplot.
    
    #checking that Mat is dataFrame
    if (!"data.frame" %in% class(Mat) || !"data.table" %in% class(Mat)) return(cat("Mat is not a data.table or dataFrame. Returning NAs.\n")) else {
        
        # convert to data.table if needed
        if (!"data.table" %in% class(Mat)) Mat <- data.table(Mat, key = "date")
        
        #checking that the mat dataFrame is not empty, considering only the complete cases
        Mat <- Mat[complete.cases(Mat),]
        if (nrow(Mat) <= 0) {
            
            return("The Mat dataFrame is empty. Returning NAs.")
            
        } else {
            
            # checking if Mat includes "Rel.bias", "Rel.RSS", "xis"
            if (!all(c("Rel.bias", "Rel.RSS", "xis") %in% colnames(Mat)) || any(is.null(c("b0","b1")))) {
                
                return("The Mat dataFrame does not contain Rel.bias, Rel.RSS or xis ; or b0 and/or b1 are null. Returning NAs.")
                
            } else {
                
                # plotting the Target Diagram
                # Create X-Y scatterplot: Coded by color by Aaron Albin
                
                #=====[Ordering dataset]=====
                # Ordering Mat according to xis to be able to create a color pallete e
                Mat <- Mat[order(Mat$xis),]
                #=============CR
                
                #=====[Set X and Y variables]=====
                # Rel.RS will be on the x axis
                # Rel.Bias will be on y axis
                
                # Values are multiplied by 100 to plot in percentage
                XData <- Mat$Rel.RSS  * 100
                YData <- Mat$Rel.bias * 100
                # For the decomposition of relative bias
                # Contribution of b0 to Rel.Bias
                Mat$b0 <- 2 * b0 / Mat$xis * 100
                # Contribution of b1 to Rel.Bias
                Mat$b1 <- 2 * rep((b1-1) * 100, times = nrow(Mat))
                # Coordinates in triange height, ...
                # http://debart.pagesperso-orange.fr/geoplan/tr_rectangle_classique.html#relation_metrique
                # Theoreme des cathetes
                Mat$x1 <- - abs(Mat$b0 * Mat$b1 / YData)
                # Premier theroreme d'Euclide
                Mat$y1 <-   Mat$b0^2 / YData
                #=============CR
                
                
                #=====[Set Xlim and Ylim]=====
                # Generally, it is a good idea to let R determine the limits (i.e. range) of the x (horizontal) axis and y (vertical) axis for you. 
                # Leaving the values below as NULL will have this effect.
                # However, if you would like to adjust them, you can do that here.
                # Just say Xlim or Ylim equal to 'c( ___, ___ )', where the two '___'s indicate the lower and upper bound (respectively) of the range for the relevant axis.
                # If you want the axis to be backwards/flip-flopped, then but the upper bound first, followed by the lower bound, e.g. rather than c(5,100) say c(100,5).
                
                # Selecting visible data
                Max.percent <- 150 # DQO.3
                if (all(is.na(Xlim))) {
                    
                    if (any(XData <= Max.percent)) {
                        
                        # There are XData < Max.percent
                        Index.Good.X <- which(abs(XData) <= Max.percent)
                        
                    } else {
                        
                        # All XData > Max.percent, creating a WRONG INDEX
                        Index.Good.X <- NULL
                    } 
                }
                if (all(is.na(Ylim))) {
                    if (any(YData < Max.percent)) {
                        
                        # There are YData < Max.percent
                        Index.Good.Y <- which(abs(YData) <= Max.percent)
                        
                    } else {
                        
                        # All YData > Max.percent, creating a WRONG INDEX nothing to plot
                        Index.Good.Y <- NULL
                    } 
                }
                # index of intersection of Index.Good.X and Index.Good.Y
                if (!is.null(Index.Good.X)) {
                    
                    if (!is.null(Index.Good.Y)) {
                        
                        Index.Good <- intersect(x = Index.Good.X, y = Index.Good.Y) 
                        
                    } else Index.Good <- NULL
                } else Index.Good <- NULL
                
                if (all(is.na(Xlim))) {
                    
                    if (!is.null(Index.Good)) {
                        
                        Xlim <- c(max(-Max.percent,min(Mat$b1[Index.Good],Mat$b0[Index.Good],XData[Index.Good])), min(Max.percent, max(DQO.3, max(XData[Index.Good]))))    
                        
                    } else Xlim <- c(0, min(Max.percent, DQO.3))
                } 
                if (all(is.na(Ylim))) {
                    
                    if (!is.null(Index.Good)) {
                        
                        Ylim <- c(max(-Max.percent,min(0, min(YData[Index.Good]))), min(Max.percent, max(DQO.3, max(YData[Index.Good])))) 
                        
                    } else Ylim <- c(0, min(Max.percent, DQO.3)) 
                } 
                #=============CR
                
                #=====[3]=====
                # Pick out which of your columns have your label data that you would like to encode as coloring of the points on the scatterplot.
                # Type the names of that column between the double-quotes below (leaving the other code untouched).
                # Levels include Which Targets are not NULL
                Levels       <- c(LAT, UAT, LV, AT ,CL)[which(!is.na(c(UAT,LAT,LV, AT,CL)))]
                factor.Color <- pretty(Mat[["xis"]], n = (10 - length(levels)))
                factor.Color <- unique(sort(c(factor.Color, Levels)))
                LabelColumn_PointColor = factor.Color
                #=============CR
                
                #=====[LegendLabels]=====
                # Run these two lines to store the unique namess from the label column and also show them to you (so you can double-check that they are as you expect them to be).
                LegendLabels = levels(LabelColumn_PointColor)
                # Run this line of code to see what labels are inside your label column selected earlier.
                levels(LabelColumn_PointColor)
                #=============CR
                
                #=====[MainTitle]=====
                # Set the name for the main title (centered along the top of the plot) here.
                if (!is.null(Model.used)) {
                    MainTitle = gsub("__", "_", sub('\\..*$', '', basename(Model.used)))
                } else {
                    if (!is.null(Sensor_name)) MainTitle = paste0(Sensor_name, " - Target Diagram - Relative expanded uncertainty") else MainTitle = "Target Diagram - Relative expanded uncertainty"
                }
                if (!is.null(BeginEnd)) {
                    MainTitle <- paste0(MainTitle, " between ", BeginEnd[1], " - ", BeginEnd[2])
                }
                #=============CR
                
                #=====[axis labels]=====
                # In the quotes after 'xAxisLabel', type the words(s) you want to see displayed beneath the x (horizontal) axis.
                # In the quotes after 'yAxisLabel', type the word(s) you want to see displayed to the left of the y (vertical) axis.
                # If you don't want to include an axis label at all for either of these, just leave the double-quotes empty, i.e. "", for that one.
                if (is.null(xAxisLabel)) xAxisLabel = bquote(paste("Random effect: 2 ", sqrt("u"[bs_s]^2*" + RMSE"^2*" - u"[bs_RM]^2), "/x"[i]*" in %"))
                if (is.null(yAxisLabel)) yAxisLabel = bquote("Bias: 2 (b"[0]*"/x"[i]*" + (b"[1]*" - 1)) in %")
                #=============CR
                
                #=====[LogAxis]=====
                # In some applications, the interesting patterns in your numbers are happening at small numbers (e.g. under 10), but you have a few outliers (e.g. over 100) that are forcing R to zoom out far enough to encompass all the data, so far that you can't see the interesting patterns in your data in the small-number range.
                # To overcome this problem, you can have R plot one or both of the axes 'logarithmically'.
                # What this means is that, visually, smaller numbers will be 'given more space', whereas the higher numbers will be 'scrunched together' more.
                # Below, indicate which axes
                # PlotWhichAxesLogarithmically = "" means you don't want either axis to be plotted logarithmically
                # PlotWhichAxesLogarithmically = "x" means you want the horizontal axis to be plotted logarithmically
                # PlotWhichAxesLogarithmically = "y" means you want the vertical axis to be plotted logarithmically
                # PlotWhichAxesLogarithmically = "xy" means you want *both* axes to be plotted logarithmically
                # Note: If you choose to plot an axis logarithmically, any zeroes and any negative numbers will be omitted from the plot. (R will give you a warning telling you how many total points this was.) This is necessary because of how the underlying logarithmic transformation itself works.
                PlotWhichAxesLogarithmically = ""
                #=============CR
                
                #=====[PointSymbol]=====
                # Choose here the symbol that you want to use for all the points that get plotted in the scatter plot. Each symbol is represented by a special 'code number':
                #  1 = unfilled circle
                #  0 = unfilled square
                #  2 = unfilled triangle
                #  5 = unfilled diamond
                # 16 =   filled circle
                # 15 =   filled square
                # 17 =   filled triangle
                # 18 =   filled diamond
                #  3 = a plus sign (i.e. '+')
                #  4 = an 'x' symbol
                #  8 = a star composed of '+' and 'x' superimposed
                # You should *not* surround any numbers like these with quotes.
                # Indicate which one you like by typing the code number below after the equals sign (without quotes).
                PointSymbol = 16
                #=============CR
                
                #=====[colorRampPalette]=====
                # Here, you can choose the colors that you would like R to use when drawing all of your points on the scatterplot.
                # Be sure to surround the name of the colors with double-quotes, e.g. "black".
                # Type 'colors()' (without the single-quotes) at the R console and hit ENTER to see the list of all the color names R accepts.
                # Hint: Colors like red, green, blue, cyan, magenta, yellow, and many others can be easily darkened by putting a number after them.
                #       Taking green for instance, "green" is the same thing as "green1".
                #       To darken the color, you can make it "green2", "green3" or "green4".
                #       This stops at 4, however - there is no "green5" or anything beyond that.
                #       Grey colors are different. There is "grey0" (black) to "grey100" (white) and everything in-between.
                #          This gives you lots of flexibility in specifying greyscale colors. (Note: The spelling 'gray' also works.)
                # Below, inside the 'combine' function 'c()', list which color you would like associated with each character by giving the color name for each in quotes.
                # The ordering will match the order of labels you just saw in the last step - in other words, the first thing there will be the first color here, the second will be the second, and so on.
                # It's perfectly OK to have more colors specified here than you will actually use. Think of this list here as the full default hierarchy that is referred to in making each plot.
                #         [1]     [2]     [3]       [4]       [5]        [6]          [7]       [8]       [9]        [10]        [11]   [12]      [13]
                coul <- c( "black", "red", "green3", "brown4", "magenta", "magenta4" , "purple", "purple4", "orange", "orange4", "gray", "gray33", "gray66", topo.colors(12),rainbow(12))
                coul <- rainbow(length(factor.Color))
                PointColors<-coul[1:length(factor.Color)]
                # https://stackoverflow.com/questions/13353213/gradient-of-n-colors-ranging-from-color-1-and-color-2
                # functions that interpolate a set of given colors to create new color palettes
                colfunc <- colorRampPalette(PointColors)
                Mat[, col := colfunc(nrow(Mat))]
                #=============CR
                
                #=====[PointSizeModifier]=====
                # If you would like your points to be bigger or smaller, the following lets you adjust their size.
                # '1' represents 100% of the default size, so if you want to make the points larger you could type, for example, 1.5 (i.e. 150%). Similarly, if you want to make them smaller you could type 0.5 (i.e. 50%).
                PointSizeModifier = 2
                #=============CR
                
                #=====[GridlineType]=====
                # SPECIFY PARAMETERS FOR GRID #
                # This section lets you add a 'grid' to the plot, running across the main plotting region to help you keep track of where things align with the axes.
                # R will draw lines wherever the 'tick marks' are on both axes. If you wish to adjust these, change 'Xlim' and 'Ylim' above.
                # Choose the type of line you would like to use for your gridlines. You have the following six choices:
                #  "solid"    = -----------------------------------------------             CR
                #  "dashed"   = ----    ----    ----    ----    ----    ----                CR
                #  "dotted"   = -   -   -   -   -   -   -   -   -   -   -   -               CR
                #  "dotdash"  = -   ----   -   ----   -   ----   -   ----   -               CR
                #  "longdash" = -------   -------   -------   -------   -------             CR
                #  "twodash"  = --  ------  --  ------  --  ------  --  ------              CR
                # If you do not want gridlines on your plot, choose "blank" instead.
                GridlineType = "dashed"
                #=============CR
                
                #=====[GridlineColor]=====
                # Choose the color for your grid lines. See step 12 for help.
                GridlineColor = "lightgrey"
                #=============CR
                
                #=====[GridlineWidthModifier]=====
                # If you would like your gridlines to be thicker, the following lets you increase its width.
                # '1' represents 100% of the default thickness, so for example, you could type 2 to make it 200% that thickness.
                GridlineWidthModifier = 1
                #=============CR
                
                # If you would like the different components of your legend (color+label pairs) stacked vertically, leave this FALSE.
                # If you want them arranged horizontally, however, set this to TRUE (without quotes).
                #=====[HorizontalLegend]=====
                HorizontalLegend = FALSE
                #=============CR
                
                #=====[Target Diagram]=====
                op <- par(no.readonly = TRUE)
                # Restoring graphical parameters on exit of function
                par(mar = c(2.8, 3, 2.5, 3.25), # mar=c(lines below, lines at left, lines at top, lines at right)
                    mgp = c(1.8, 0.4, 0),      # mgp=c(label, tick mar label, tick mark)
                    cex.axis = 0.8
                ) 
                on.exit(par(op))
                
                plot.default(x           = if (!is.null(Index.Good)) XData[Index.Good] else 1, 
                             y           = if (!is.null(Index.Good)) YData[Index.Good] else 1, 
                             xlim        = Xlim, 
                             ylim        = Ylim, 
                             xlab        = xAxisLabel, 
                             ylab        = yAxisLabel, 
                             pch         = PointSymbol, 
                             col         = Mat$col, 
                             cex         = PointSizeModifier, 
                             type        = "p", 
                             ann         = TRUE, 
                             axes        = TRUE, 
                             frame.plot  = TRUE, 
                             panel.first = grid(nx = NULL, 
                                                ny = NULL, 
                                                lty = GridlineType, 
                                                col = GridlineColor, 
                                                lwd = GridlineWidthModifier), 
                             panel.last  = NULL, 
                             asp         = 1
                )
                title(main = MainTitle, line = 1.5, font.main = 1, cex.main = 0.7)
                
                # get the limits of x and y axis
                usr <- par('usr') 
                
                # Text highest standard deviation
                if (sdm_sdo) {
                    label.sigma <- " < "
                } else {
                    label.sigma <- " >  "
                } 
                if (Xlim[2] > Xlim[1]) label.Bias <-  "Bias > 0" else label.Bias <-  "Bias < 0"
                # Text on the top see https://stackoverflow.com/questions/4973898/combining-paste-and-expression-functions-in-plot-labels
                text(x      = usr[2]/2,
                     y      = usr[4],
                     pos    = 1, # below 
                     labels = bquote(sigma[Sensor] ~ .(label.sigma) ~ sigma[Reference] ~ " and " ~ .(label.Bias)), 
                     cex    = 0.7
                )
                
                if (Unit.Ref == "ppm" || Unit.Ref == "mg/m3") {
                    mtext(text = bquote(paste("y = b"[0]*" + b"[1]*" x, with b"[0]*" = ", .(format(b0, digits = 1)), " and b"[1]*" = ", .(format(b1, digits = 2)),
                                              ", u"[bs_RM]*" = ", .(format(ubsRM, digits = 3)), " and u"[bs_s]*" = ", .(format(ubss, digits = 3)))),
                          side = 3, 
                          line = 0.1, 
                          cex  = 0.8
                    )
                } else {
                    if (is.null(Time.average)) {
                        mtext(text = bquote(paste("y = b"[0]*" + b"[1]*" x, with b"[0]*" = ", .(format(b0, digits = 1)), " and b"[1]*" = ", .(format(b1, digits = 2)),
                                                  ", u"[bs_RM]*" = ", .(format(ubsRM, digits = 3)), " and u"[bs_s]*" = ", .(format(ubss, digits = 3)))),
                              side = 3, 
                              line = 0.1, 
                              cex  = 0.8
                        )    
                    } else {
                        mtext(text = bquote(paste("y = b"[0]*" + b"[1]*" x, with b"[0]*" = ", .(format(b0, digits = 1)), " and b"[1]*" = ", .(format(b1, digits = 2)),
                                                  ", u"[bs_RM]*" = ", .(format(ubsRM, digits = 3)), " and u"[bs_s]*" = ", .(format(ubss, digits = 3)), 
                                                  ", ", .(format(Time.average, digits = 0)), " min average")),
                              side = 3, 
                              line = 0.1, 
                              cex  = 0.8
                        )
                    }
                    
                }
                
                # Contribution of b1 : (b1 - 1)
                points(x = if (!is.null(Index.Good)) Mat$b1[Index.Good] else 1, 
                       y = if (!is.null(Index.Good)) YData[Index.Good] else 1, 
                       col = Mat$col, 
                       pch  = "-",
                       cex  = 0.3
                )
                # Contribution of b0 : b0/x
                points(x = if (!is.null(Index.Good)) Mat$b0[Index.Good] else 1, 
                       y = if (!is.null(Index.Good)) YData[Index.Good] else 1, 
                       col = Mat$col, 
                       pch  = "-",
                       cex  = 0.3
                )
                # draw axis
                abline(h = 0)
                abline(v = 0)
                #=============CR
                
                #=====[colorscale]=====
                if (!is.null(Index.Good)) {
                    
                    # Add colorscale
                    LegendTitle = Unit.Ref
                    image.plot(bigplot      = usr,
                               legend.only  = TRUE, 
                               zlim         = range(Mat[Index.Good, xis]),
                               col          = Mat[Index.Good, col],
                               legend.args  = list(text = LegendTitle, side = 3, cex = 1, line = 0, srt = 0, adj = 0),
                               legend.width = 0.5,
                               legend.mar   = 3.6,
                               verbose      = FALSE
                               #smallplot    = c(usr[2]-10,0, 10,Ylim[2])
                    )
                }
                #=============CR
                
                #=====[target circles]=====
                for (i in c(DQO.1*100, DQO.2*100, DQO.3*100)) {
                    if (!is.na(i)) {
                        cercle <- rbind(cbind(seq(-i, i, by =  i/1000),  sqrt(i^2 - seq(-i,i, by = i/1000)^2)),
                                        cbind(seq( i,-i, by = -i/1000), -sqrt(i^2 - seq(-i,i, by = i/1000)^2)))
                        lines(cercle,type = "l")
                        text(x = 0, y = i + 1, paste0(i," %"), pos = 4) # pos of the label at rigth of the coordinate
                        
                    }
                }
                # adding circles for Ur > DQO.3
                if (sqrt(usr[2]^2 + usr[4]^2) > DQO.3) {
                    
                    # determining DQO steps
                    DQO.step <- min(c(DQO.2*100 - DQO.1*100, DQO.3*100 - DQO.2*100))
                    DQO.max  <- max(c(DQO.1*100, DQO.2*100, DQO.3*100))
                    while (sqrt(usr[2]^2 + usr[4]^2) > DQO.max + DQO.step) {
                        DQO.max <- DQO.max + DQO.step
                        cercle <- rbind(cbind(seq(-DQO.max, DQO.max, by =  DQO.max/1000),  sqrt(DQO.max^2 - seq(-DQO.max,DQO.max, by = DQO.max/1000)^2)),
                                        cbind(seq( DQO.max,-DQO.max, by = -DQO.max/1000), -sqrt(DQO.max^2 - seq(-DQO.max,DQO.max, by = DQO.max/1000)^2)))
                        lines(cercle,type = "l", lty = GridlineType, col = GridlineColor)
                        text(x = 2, 
                             y = DQO.max + 1, 
                             paste0(DQO.max," %"), 
                             pos = 4, 
                             cex = 0.7) # pos to the rigth of the coordinate
                    }
                    
                }
                #=============CR
                
                #=====[Arrows]=====
                # Plotting segments for relative measurement uncertainty
                
                # checking that data is visible
                if (!is.null(Index.Good)) {
                    
                    # Select last quartile of these 90 % the biggest Ur within Index.Good
                    Index.Big.Ur <- which(Mat[Index.Good,"Ur"] < 0.90 * max(Mat[Index.Good,"Ur"]) & Mat[Index.Good,"Ur"] > 0.75 * max(Mat[Index.Good,"Ur"]))
                    
                    # Absolute difference between Angle of Ur and diagonal 45 and -45 degress
                    Mat[, Angle45  := abs(atan(YData/XData) * 180 / pi  -   45 )]
                    Mat[, Angle_45 := abs(atan(YData/XData) * 180 / pi  - (-45))]
                    
                    # Selecting best point for plotting arrows
                    if (min( Mat[Index.Good, Angle45][Index.Big.Ur], na.rm = T) < min(Mat[Index.Good, Angle_45][Index.Big.Ur], na.rm = T)) {
                        Index.med.UR <- which(Mat[["Angle45"]]   == min(Mat[["Angle45"]][Index.Good][Index.Big.Ur] ))[1] 
                    } else Index.med.UR <- which(Mat[["Angle_45"]]  == min(Mat[["Angle_45"]][Index.Good][Index.Big.Ur]))[1]
                    
                    # plotting Arrows and text Rel.Bias, rel.Rc, Ur 
                    Arrows(x0 = XData[Index.med.UR], y0 = 0, 
                           x1 = XData[Index.med.UR], y1 = YData[Index.med.UR], 
                           col = "black", 
                           lty = 1, 
                           lwd = 1,
                           arr.type = "curved",
                           arr.length = 0.2,
                           code = 3,
                           arr.adj = 1
                    )
                    text(x = XData[Index.med.UR], 
                         y = YData[Index.med.UR]/2, 
                         labels = c("Bias"),
                         pos = 4,
                         srt = 90,
                         cex = 0.8
                    )
                    Arrows(x0 = 0                  , y0 = YData[Index.med.UR], 
                           x1 = XData[Index.med.UR], y1 = YData[Index.med.UR],
                           col = "black", 
                           lty = 1, 
                           lwd = 1,
                           arr.type = "curved",
                           arr.length = 0.2,
                           code = 3,
                           arr.adj = 1
                    )
                    text(x = XData[Index.med.UR]/2, 
                         y = YData[Index.med.UR], 
                         labels = c("Random effect"),
                         pos = 3,
                         adj = 0.5,
                         cex = 0.8
                    )
                    Arrows(x0 = 0                  , y0 = 0, 
                           x1 = XData[Index.med.UR], y1 = YData[Index.med.UR],
                           col = "black", 
                           lwd = 2,
                           arr.type = "curved",
                           code = 3,
                           arr.adj = 1
                    )
                    text(x = XData[Index.med.UR]/2, 
                         y = YData[Index.med.UR]/2, 
                         labels = c("Relative expanded  uncertainty in %"),
                         srt = atan(YData[Index.med.UR]/XData[Index.med.UR]) * 180 /pi,
                         pos = 3
                    )
                    
                    # Text and line b1 - 1 
                    text(x = Mat$b1[1]/2, 
                         y = Ylim[1], 
                         labels = bquote("2(b"[1]*"-1)"), 
                         cex = 0.8,
                         pos = 3) # pos label above the coordinate
                    text(x = Mat$b1[1]/2, 
                         y = Ylim[1], 
                         labels = paste0(format(2*(b1 - 1)*100, digits = 1)," %"), 
                         cex = 0.8,
                         pos = 1) # pos label above the coordinate
                    Arrows(x0 = 0         , y0 = Ylim[1], 
                           x1 =  Mat$b1[1], y1 = Ylim[1], 
                           col = "black", 
                           lty = 1, 
                           lwd = 1,
                           arr.type = "curved",
                           arr.length = 0.2,
                           code = 3,
                           arr.adj = 1
                    )
                    
                    # Text and point b0 
                    if (b0 > 0) {
                        
                        if (any(Mat$b0 <= Xlim[2])) {
                            
                            # There are Mat$b0 <= Xlim[2]
                            Index.Good.b0 <- which(Mat[Index.Good, "b0"] <= Xlim[2])
                            x.b0          <- max(Mat[Index.Good, b0][Index.Good.b0])
                            y.b0          <- Mat[Mat$b0 == x.b0, Rel.bias][1] * 100 
                            text(x = x.b0, 
                                 y = y.b0, 
                                 labels = bquote("2 b"[0]*"/x"[i]*" in %"),
                                 srt = atan(y.b0 /(x.b0 + Mat$b1[1])) * 180 / pi,
                                 pos = 2
                            )
                        } 
                    } else {
                        
                        if (any(Mat$b0 >= Xlim[1])) {
                            
                            # There are Mat$b0 >= Xlim[1]
                            Index.Good.b0 <- which(Mat[Index.Good, "b0"] >= Xlim[1])
                            x.b0          <- min(Mat[Index.Good.b0, "b0"])
                            y.b0          <- Mat[["Rel.bias"]][Mat$b0 == x.b0][1] * 100 
                            text(x = x.b0, 
                                 y = y.b0, 
                                 labels = bquote("2 b"[0]*"/x"[i]*" in %"),
                                 srt = atan(y.b0 / (x.b0 + Mat$b1[1])) * 180 / pi,
                                 pos = 4
                            )
                        }
                    }
                    points(x = Mat$b0[Index.med.UR], y = YData[Index.med.UR], type = "p", col = "black")
                    segments(x0 = Mat$b0[Index.med.UR], y0 = 0, 
                             x1 = Mat$b0[Index.med.UR], y1 = YData[Index.med.UR], 
                             col = "black", lty = 2, lwd = 1)
                    text(x = Mat$b0[Index.med.UR], 
                         y = 0, 
                         labels = paste0("2 b0/x: ", format(Mat$b0[Index.med.UR],digits =0), " %"),
                         pos = 1,
                         cex = 0.8
                    )
                    #=============CR
                    
                    #=====[Limit Values]=====
                    # Limit values
                    # Checking if LV is not NA
                    if (!is.na(LV)) {
                        
                        if (LV < max(Mat[Index.Good,"xis"], na.rm = T)) {
                            
                            # checking is LV is witin plotted x values not necessary, plot LV is will apper only if within xlim an ylim
                            if (any(Mat[Index.Good,"xis"] == LV)) {
                                
                                Index.LV <- which(Mat[Index.Good,"xis"] == LV)[1] 
                                
                            } else {
                                
                                Index.LV <- which(abs(Mat[Index.Good,"xis"] - LV) == min(abs(Mat[Index.Good,"xis"] - LV), na.rm = T))[1]   
                            } 
                            
                            # PLotting a point for LV
                            points(x = XData[Index.Good][Index.LV], 
                                   y = YData[Index.Good][Index.LV],
                                   type = "p",
                                   col  = "black",
                                   pch  = "+"
                            )
                            
                            # plotting label LV
                            text(x = XData[Index.Good][Index.LV], 
                                 y = YData[Index.Good][Index.LV],
                                 labels = "LV",
                                 pos = 1
                            )
                        } 
                    }        
                    
                    # Low Assessment Threshold
                    # Checking if LAT is not NA
                    if (!is.na(LAT)) {
                        
                        if (LAT < max(Mat[Index.Good,"xis"], na.rm = T)) {
                            
                            # checking is LAT is witin plotted x values not necessary, plot LAT is will apper only if within xlim an ylim
                            if (any(Mat[Index.Good,"xis"] == LAT)) {
                                
                                Index.LAT <- which(Mat[Index.Good,"xis"] == LAT)[1] 
                                
                            } else {
                                
                                Index.LAT <- which(abs(Mat[Index.Good,"xis"] - LAT) == min(abs(Mat[Index.Good,"xis"] - LAT), na.rm = T))[1]   
                            } 
                            
                            # PLotting a point for LAT
                            points(x = XData[Index.Good][Index.LAT], 
                                   y = YData[Index.Good][Index.LAT],
                                   type = "p",
                                   col  = "black",
                                   pch  = "+"
                            )
                            
                            # plotting label LAT
                            text(x = XData[Index.Good][Index.LAT], 
                                 y = YData[Index.Good][Index.LAT],
                                 labels = "LAT",
                                 pos = 1
                            )
                        } 
                    }
                    # Upper Assessment Threshold
                    # Checking if UAT is not NA
                    if (!is.na(UAT)) {
                        
                        if (UAT < max(Mat[Index.Good,"xis"], na.rm = T)) {
                            
                            # checking is UAT is witin plotted x values not necessary, plot UAT is will apper only if within xlim an ylim
                            if (any(Mat[Index.Good,"xis"] == UAT)) {
                                
                                Index.UAT <- which(Mat[Index.Good,"xis"] == UAT)[1] 
                                
                            } else {
                                
                                Index.UAT <- which(abs(Mat[Index.Good,"xis"] - UAT) == min(abs(Mat[Index.Good,"xis"] - UAT), na.rm = T))[1]   
                            } 
                            
                            # PLotting a point for UAT
                            points(x = XData[Index.Good][Index.UAT], 
                                   y = YData[Index.Good][Index.UAT],
                                   type = "p",
                                   col  = "black",
                                   pch  = "+"
                            )
                            
                            # plotting label UAT
                            text(x = XData[Index.Good][Index.UAT], 
                                 y = YData[Index.Good][Index.UAT],
                                 labels = "UAT",
                                 pos = 1
                            )
                        } 
                    }        
                    # Alert  Threshold
                    # Checking if AT is not NA
                    if (!is.na(AT)) {
                        
                        if (AT < max(Mat[Index.Good,"xis"], na.rm = T)) {
                            
                            # checking is AT is witin plotted x values not necessary, plot AT is will apper only if within xlim an ylim
                            if (any(Mat[Index.Good,"xis"] == AT)) {
                                
                                Index.AT <- which(Mat[Index.Good,"xis"] == AT)[1] 
                                
                            } else {
                                
                                Index.AT <- which(abs(Mat[Index.Good,"xis"] - AT) == min(abs(Mat[Index.Good,"xis"] - AT), na.rm = T))[1]   
                            } 
                            
                            # PLotting a point for AT
                            points(x = XData[Index.Good][Index.AT], 
                                   y = YData[Index.Good][Index.AT],
                                   type = "p",
                                   col  = "black",
                                   pch  = "+"
                            )
                            
                            # plotting label AT
                            text(x = XData[Index.Good][Index.AT], 
                                 y = YData[Index.Good][Index.AT],
                                 labels = "AT",
                                 pos = 1
                            )
                        } 
                    }        
                    # Critical Level
                    # Checking if CL is not NA
                    if (!is.na(CL)) {
                        
                        if (CL < max(Mat[Index.Good,"xis"], na.rm = T)) {
                            
                            # checking is CL is witin plotted x values not necessary, plot CL is will apper only if within xlim an ylim
                            if (any(Mat[Index.Good,"xis"] == CL)) {
                                
                                Index.CL <- which(Mat[Index.Good,"xis"] == CL)[1] 
                                
                            } else {
                                
                                Index.CL <- which(abs(Mat[Index.Good,"xis"] - CL) == min(abs(Mat[Index.Good,"xis"] - CL), na.rm = T))[1]   
                            } 
                            
                            # PLotting a point for CL
                            points(x = XData[Index.Good][Index.CL], 
                                   y = YData[Index.Good][Index.CL],
                                   type = "p",
                                   col  = "black",
                                   pch  = "+"
                            )
                            
                            # plotting label CL
                            text(x = XData[Index.Good][Index.CL], 
                                 y = YData[Index.Good][Index.CL],
                                 labels = "CL",
                                 pos = 1
                            )
                        } 
                    }
                }
                
            }
        }
    }
}

#================================================================CR
### function to plot and compare x reference values against y the sensor values (y 0 measurement function) 
#================================================================CR
lm.Model.Compare <- function(General.df, DateIN, DateEND, x, y, Title = NULL) {
    # This function plot the x and y data of model and draw a linear line
    # input  : General.df : dataFrame with y data and back predicted data
    #          DateIn     : character strings, begining and ending dates of selected data, e.g DateIn  <- "2018-10-09" DateEND <- "2019-01-10"
    #          x, y       : character strings, names of columns x and y in General.df dataFrame, they will be usd for x and y axis labels
    #          Title      : optional chacter string of the scatterplot
    # Output : The linear comparison model
    
    # Subset General.df to selected date
    General    <- subset(General.df[, c("date", x, y)], date >= as.POSIXct(DateIn) & date <= as.POSIXct(DateEND))
    
    # loading packages
    library(broom)
    
    # Linear regression of the x and predicted data of the model
    Comparison <- lm(General[,y] ~ General[,x], data = General, model = TRUE, x = TRUE, y = TRUE)
    
    # tidy model output, coefficients
    Aug.Comparison <- data.frame(augment(Comparison))
    print(tidy(Comparison))
    
    # Select x, y and predict
    X         <- grep(pattern = ".x."     ,  x = names(Aug.Comparison))
    Y         <- grep(pattern = ".y."     ,  x = names(Aug.Comparison))
    Predicted <- grep(pattern = ".fitted" ,  x = names(Aug.Comparison))
    
    # Scatter plot y = f(x) and predicted line
    Xlim <- c(min(c(Aug.Comparison[,X], Aug.Comparison[,Y]), na.rm = T), max(c(Aug.Comparison[,X], Aug.Comparison[,Y]), na.rm = T))
    plot(Aug.Comparison[,X], Aug.Comparison[,Y], xlim = Xlim, ylim = Xlim, xlab = x, ylab = y)
    grid(NULL,NULL)
    lines(Aug.Comparison[,X], Aug.Comparison[,Predicted], col = "blue")
    if (!is.null(Title)) title(main = Title, outer = TRUE, line = -1)
    
    # display equations and R^2
    mtext(sprintf(paste0("Linear: y= %.2f + %.3f x, R2= %.4f, RMSE= %.2f, AIC= %.1f"), 
                  coef(Comparison)[1], 
                  coef(Comparison)[2], 
                  summary(Comparison)$r.squared,
                  sqrt(sum(resid(Comparison)^2)/(length(resid(Comparison)) - 2)),
                  AIC(Comparison)), 
          line = 1, adj = 1, padj = 0,col = "blue", cex = 0.875)
    
    return(Comparison)
    
}

#================================================================CR
### function to query data using a_i_p server
#================================================================CR
a_i_p_param <- function(URL, username, password, organisation, station, start, end = NULL, 
                        allparams = "true", avgtime=600) {
    # Mail "Erich Kitzmüller" <erich.kitzmueller@a-i-p.com>
    # please append the parameter "&allparams=true" to your URL. Without that, the request only returns the values of monitoring-site-parameters marked as default (see image).
    # Hint: For each unique combination of "Monitoring Site" and "Global Parameter", there can be only one monitoring-site-parameter marked as default. 
    
    ### Load packages
    list.Packages <- c( "RCurl"    , "curl"         , "jsonlite"     , "httr") 
    if (!all(list.Packages %in% installed.packages())) Load.Packages(list.Packages)
    
    # start/end date
    if (is.null(end)) end <- format(start + 2, "%Y-%m-%d-%H-%M-%S") else end <- format(end, "%Y-%m-%d-%H-%M-%S")
    start <- format(start, "%Y-%m-%d-%H-%M-%S")
    
    #----------------------------------------------------------------CR
    # 1) Contacting Server
    #----------------------------------------------------------------CR
    cat("Sending JSON request: \n")
    JSON.request <- paste0(URL, "username=", username,"&",
                           "password=", password,"&",
                           "organisation=", organisation,"&",
                           "station=", station,"&",
                           "allparams=",allparams,"&",
                           #"AvgTime=10", "&",
                           "start=", start,"&",
                           "end=", end, "&",
                           "aggregation=2h+Mean+Value")
    cat(paste0(JSON.request,"\n"))
    JSON <- httr::GET(URLencode(JSON.request))
    
    if (JSON$status_code == 200) {
        
        #================================================================CR
        # 2) Extract the data from the JSON file ====
        #================================================================CR
        # extract the data node
        Reference <- content(JSON, type = "application/json", as = 'parsed')
        Reference <- Reference$Stations[[1]]$Devices
        
        #================================================================CR
        # 3) Returning parameters
        #================================================================CR
        return(sapply(seq_along(Reference), function(i) Reference[[i]]$Components[[1]]$Component  ))
        
    } else if (JSON$status_code == 204) {
        
        my_message <- paste0("[a_i_p_param] INFO the server was contacted with succes but there no data to return. Change dates.\n")
        shinyalert(
            title = "INFO Connected to a_i_p server",
            text = "my_message",
            closeOnEsc = TRUE,
            closeOnClickOutside = TRUE,
            html = FALSE,
            type = "success",
            showConfirmButton = TRUE,
            showCancelButton  = FALSE,
            confirmButtonText = "OK",
            confirmButtonCol  = "#AEDEF4",
            timer = 0,
            imageUrl = "",
            animation = FALSE)
        return()   
        
    } else {
        
        my_message <- paste0("[a_i_p_param] ERROR the parameter to contact the a_i_p server are wrong, please check\n")
        shinyalert(
            title = "ERROR no connection to a_i_p server",
            text = "my_message",
            closeOnEsc = TRUE,
            closeOnClickOutside = TRUE,
            html = FALSE,
            type = "error",
            showConfirmButton = TRUE,
            showCancelButton  = FALSE,
            confirmButtonText = "OK",
            confirmButtonCol  = "#AEDEF4",
            timer = 0,
            imageUrl = "",
            animation = FALSE)
        return()   
    }
}


# a_i_p_data  <- function(URL, username, password, organisation, station, start, end, param = NULL, Time_zone = "UTC") {
#     # URL          character string indicating the a-i-p URL for data transmission
#     # username     character string indicating the login at the a-i-p URL 
#     # password     character string indicating the password at the a-i-p URL 
#     # organisation character string indicating the organisation quering the a-i-p URL
#     # station      character string indicating the station being interogatedat the a-i-p URL
#     # start        POSIXCt indicating the starting date for data downlaod, format: "2019-03-01-00-00-00"
#     # end          POSIXCt indicating the ending   date for data downlaod, format: "2019-03-05-00-00-00"
#     # param        vector of charater string listing the parameters measured at the station to be considered
#     #              default is NULL. If NULL all parameters are downloaded
#     # Time_zone    Character vector, default is "UTC", a character string that specifies which time zone to parse 
#     #              the date with. The string must be a time zone that is recognized by the user's OS.     
#     
#     # End date
#     f_start <- format(lubridate::ymd(start), "%Y-%m-%d-%H-%M-%S")
#     f_end   <- format(lubridate::ymd(end),   "%Y-%m-%d-%H-%M-%S")
#     
#     #================================================================CR
#     # 1) Grab the data
#     #================================================================CR
#     # CUrl needed see http://appliedmathbytes.com/2015/08/using-json-in-r/
#     # Otherwise we will get error Error in file(con, “r”) : cannot open the connection
#     Reference.JSON.httr <- httr::GET(URLencode(paste0(URL,"username=", username,"&",
#                                                       "password=", password,"&",
#                                                       "organisation=", organisation,"&",
#                                                       "station=", station,"&",
#                                                       "start=", f_start,"&",
#                                                       "end=", f_end)))
#     
#     #================================================================CR
#     # 2) Extract the data from the JSON file ====
#     #================================================================CR
#     # extract the data node
#     Reference <- content(Reference.JSON.httr, type = "application/json", as = 'parsed')
#     Reference <- Reference$Stations[[1]]$Devices
#     
#     # determining Component
#     Components <- sapply(seq_along(Reference), function(i) Reference[[i]]$Components[[1]]$Component)
#     # determining Units
#     Units      <- sapply(seq_along(Reference), function(i) Reference[[i]]$Components[[1]]$Unit)
#     # counts of data per parameter
#     Counts     <- sapply(seq_along(Reference), function(i) length(Reference[[i]]$Components[[1]]$MeasuredValues))
#     
#     # Reference data
#     MeasuredValues <- lapply(seq_along(Reference), function(i) {
#         
#         if (!is.null(param)) {
#             
#             if (Components[i] %in% param) {
#                 cat(paste0("Component: ",Components[i]," is being downloaded\n"))
#                 
#                 # Downloading data, taking only valid measurements, dropping Valid, Convert Time to Posix format   
#                 Param.i <- data.table::rbindlist(lapply(Reference[[i]]$Components[[1]]$MeasuredValues, as.data.frame.list), fill = T) %>% 
#                     dplyr::filter(Valid == TRUE) %>% 
#                     dplyr::select(Time,Value) %>% 
#                     dplyr::mutate(Time = ymd_hms(Time, tz = Time_zone))
#                 
#                 colnames(Param.i) <- c("date", Components[i])
#                 
#                 return(Param.i)
#             }
#         } else {
#             
#             cat(paste0("Component: ",Components[i]," is being downloaded\n"))
#             
#             # Downloading data, taking only valid measurements, dropping Valid, Convert Time to Posix format   
#             Param.i <- data.table::rbindlist(lapply(Reference[[i]]$Components[[1]]$MeasuredValues, as.data.frame.list), fill = T) %>% 
#                 dplyr::filter(Valid == TRUE) %>% 
#                 dplyr::select(Time,Value) %>% 
#                 dplyr::mutate(Time = ymd_hms(Time, tz = Time_zone))
#             
#             colnames(Param.i) <- c("date", Components[i])
#             
#             # convert to xts
#             #Param.i <- xts::xts(x = Param.i[,2], order.by = Param.i$date) #, tzone = threadr::time_zone(Param.i$date)
#             
#             return(Param.i)
#         }
#     })
#     # Creating RefData
#     for (i in seq_along(MeasuredValues)) {
#         if (!is.null(MeasuredValues[[i]])) {
#             
#             if (exists("RefData")) RefData <- merge(x = RefData, y = MeasuredValues[[i]], by = "date") else RefData <- MeasuredValues[[i]]
#         }
#     }
#     
#     return(RefData)
# }

a_i_p_data <- function(URL, username, password, organisation, station, 
                       start, end = format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), 
                       param = NULL, Time_zone = "UTC", allparams = "true", avgtime = 1, Valid = TRUE, unflagged = TRUE, flushtime=120) {
    # URL           character string indicating the a-i-p URL for data transmission
    # username      character string <- "datareq"
    # password      character string <- "aip2jrc"
    # organisation  character string <- "ABCIS"
    # station       character string indicating <- "JRC+Ispra"
    # start         character string indicating the starting date for data downlaod, format: "2019-03-01-00-00-00"
    # end           character string indicating the endinding date for data downlaod, format: "2019-03-01-00-00-00"
    # avgtime       Integer, 1, 10, 15 or 60, default is 1, the averaging time of Reference data in minutes
    # Valid         see below mail exchange
    # unflagged     see below mail exchange, shall be TRUE or FALSE, default TRUE, only return unflagged data
    # flushtime     see below mail exchange, shall be an integer, 0 means no flush, other wise time in seconds, e. g. 60, 60 , 120 (2 mintes addvised)
    # 
    # return a list with 2 dataframes: data that gives all data with  1st column a POSIXct "date" only Valid and unflagged values
    #                                  meta taht gives a dataframe of metadata: a_i_p components, reference name and units
    
    # Mail "Erich Kitzmüller" <erich.kitzmueller@a-i-p.com>
    # please append the parameter "&allparams=true" to your URL. Without that, the request only returns the values of monitoring-site-parameters marked as default (see image).
    # Hint: For each unique combination of "Monitoring Site" and "Global Parameter", there can be only one monitoring-site-parameter marked as default. 
    
    # you have to supply the name of an aggregation (configured in UBIS4) to use for averaging, e.g.
    # https://ubis-air.a-i-p.com/UidepService/values/simple?username=datareq&password=aip2jrc&organisation=ABCIS&station=Sensors&allparams=true&start=2019-08-29-00-00-00&end=2019-09-11-17-00-00&aggregation=1h+Mean+Value
    # The parameter "avgtime" does not automatically choose an aggregation; instead, you can only use it to chose a category of measured values ( 1min or 10min ) where to take the data from. (Those values are calculated by the station (IOX), UBIS4 stores values of both categories in the database).
    
    # The parameter name is "avgtime" (not "AvgTime", which is simply ignored) and the unit is seconds. Therefore,
    # https://ubis-air.a-i-p.com/UidepService/values/simple?username=datareq&password=aip2jrc&organisation=ABCIS&station=Sensors&avgtime=60&allparams=true&start=2019-08-29-00-00-00&end=2019-09-11-17-00-00
    # returns the 1-Minute values, and
    # https://ubis-air.a-i-p.com/UidepService/values/simple?username=datareq&password=aip2jrc&organisation=ABCIS&station=Sensors&avgtime=600&allparams=true&start=2019-08-29-00-00-00&end=2019-09-11-17-00-00
    # returns the 10-Minute values (which are also default).
    # You can use both the "avgtime" and the "aggregation" parameter in one query; for example, you can request "1h Mean Values" calculated from the 1-Minute values; in a perfect world, that would return the same like "1h Mean values" calculated from 10-Minute values, but in reality, small rounding differences might occur.
    
    
    # Hi Michel,
    # I have implemented a new option in the UidepService for you: unflagged=true means that all flagged values are considered invalid (even though the flags do not normally render them invalid in UBIS)
    # Please compare the output of
    # https://ubis-air.a-i-p.com/UidepService/values/simple?username=datareq&password=aip2jrc&organisation=ABCIS&station=JRC+Ispra&component=nitrogen+dioxide&start=2019-09-18-15-00-00&end=2019-09-18-17-00-00&valid=true&avgtime=60&aggregation=15m+Mean+Value&unflagged=true
    # with the output of
    # https://ubis-air.a-i-p.com/UidepService/values/simple?username=datareq&password=aip2jrc&organisation=ABCIS&station=JRC+Ispra&component=nitrogen+dioxide&start=2019-09-18-15-00-00&end=2019-09-18-17-00-00&valid=true&avgtime=60&aggregation=15m+Mean+Value&unflagged=false
    # to see what I mean. This works with one-minute-Values just as well.
    # Best regards,
    # Erich
    
    # Hi Michel,
    # About your first problem: Those values, which are obviously too high, belong to the flush phase of the function check. The analyzer already inhales normal air, but there is still some span gas in the measurement chamber.
    # Since the device is no longer fed with span gas, there is no operating status set on those values. In the output of the IOX, there are internal status bits set that indicate this state, but unfortunately those are currently ignored by UBIS, and I cannot change that easily.
    # As a workaround, I have added yet another parameter to the UIDEP webservice: "flushtime=120" causes all values within 120 seconds after a marked value to be considered invalid, too. (120 is the recommended value, but you can of course choose any other)
    # https://ubis-air.a-i-p.com/UidepService/values/simple?username=datareq&password=aip2jrc&organisation=ABCIS&station=JRC+Ispra&component=sulfur+dioxide&start=2019-09-19-16-00-00&end=2019-09-19-18-00-00&valid=true&avgtime=60&unflagged=true&flushtime=120
    # Without unflagged=true, flushtime has no effect at all.
    # About your second problem: Since the CAPS device does the logging all by itself, we just transfer and import the recorded values "as-is" without even trying to fit them into a one-minute time grid. 
    
    ### Load packages
    list.Packages <- c("RCurl", "curl", "jsonlite", "httr", "tidyverse", "purrr", "dplyr") 
    if (!all(list.Packages %in% installed.packages())) Load.Packages(list.Packages)
    
    # start/end date
    end   <- format(end,   "%Y-%m-%d-%H-%M-%S")
    start <- format(start, "%Y-%m-%d-%H-%M-%S")
    
    
    #----------------------------------------------------------------CR
    # 1) Grab the data
    #----------------------------------------------------------------CR
    if (avgtime %in% c(1,10,15, 60)) {
        if (avgtime ==  1) {
            aggregation <- NA; AvgTime <- 60
        } else {
            if (avgtime == 10) {
                aggregation <- NA; AvgTime <- 600
            } else {
                if (avgtime == 15) {
                    aggregation <- "15m+Mean+Value";AvgTime <- 60
                } else if (avgtime == 60) aggregation <- "1h+Mean+Value" ;AvgTime <- 600
            }
        }
    } else {return(cat("avgtime shall be 1, 10, 15 or 60"))}
    cat("Sending JSON request: \n")
    JSON.request <- paste0(URL, "username=", username,
                           "&password=", password,
                           "&organisation=", organisation,
                           "&station=", station,
                           "&allparams=",allparams,
                           "&start=", start,
                           "&end=", end,
                           "&valid=true",
                           "&avgtime=", AvgTime,
                           ifelse(!is.na(aggregation),paste0("&aggregation=", aggregation),""),
                           "&unflagged=", ifelse(unflagged, "true", "false"),
                           ifelse(flushtime>0,paste0("&flushtime=",flushtime),""))
    cat(paste0(JSON.request,"\n"))
    Ref.JSON <- httr::GET(URLencode(JSON.request))
    if (Ref.JSON$status_code != 200) {
        cat("Invalid JSO request\n")
        cat(paste0("HTTP status code :", Ref.JSON$status_code, "\n"))
        return(cat(Ref.JSON$request$url))
    } else cat("Successful JSON request with HTTP status = 200 \n")
    
    #----------------------------------------------------------------CR
    # 2) Extract the data from the JSON file ====
    #----------------------------------------------------------------CR
    # extract the data node
    Ref <- httr::content(Ref.JSON, type = "application/json", as = 'parsed')
    if (exists("Ref.JSON"))      remove(Ref.JSON)
    Ref <- Ref$Stations[[1]]$Devices
    # determining Units
    Units      <- sapply(seq_along(Ref), function(i) Ref[[i]]$Components[[1]]$Unit)
    # determining Component
    Components <- sapply(seq_along(Ref), function(i) Ref[[i]]$Components[[1]]$Component)
    # MeasuredValues, list of dataframes for each component
    MeasuredValues <- purrr::map(Ref, function(Refi) {
        
        # Making one data frame per list   
        # Param.i <- data.table::rbindlist(lapply(Refi$Components[[1]]$MeasuredValues, as.data.frame.list, stringsAsFactors = F), 
        #                                  fill = T, use.names = T) 
        Param.i <- data.table::rbindlist(Refi$Components[[1]]$MeasuredValues)
        
        if ("Value" %in% names(Param.i) && Refi$Components[[1]]$Component %in% param) {
            
            cat(paste0("Component ",stringr::str_pad(which(Components %in% Refi$Components[[1]]$Component), 2, side = "left", pad = " "),"/",length(seq_along(Ref)),": ",
                       stringr::str_pad(Refi$Components[[1]]$Component, max(nchar(Components)), side = "right", pad = " ")," was correctly downloaded\n"))
            
            Param.i <- Param.i %>%
                dplyr::select(Time,Value) 
            # %>% dplyr::filter(Valid == TRUE) 
            colnames(Param.i) <- c("date", Refi$Components[[1]]$Component)
            
            return(Param.i)
        } else {
            cat(paste0("Component ",stringr::str_pad(which(Components %in% Refi$Components[[1]]$Component), 2, side = "left", pad = " "),"/",length(seq_along(Ref)), ": ",
                       str_pad(Refi$Components[[1]]$Component, max(nchar(Components)), side ="right", pad=" "), " no data to be downloaded\n"))
        } 
    })
    # Creating ReferenceData
    RefData <- MeasuredValues[!sapply(MeasuredValues, is.null)] %>% 
        purrr::reduce(full_join, by = "date") %>% 
        dplyr::mutate(date = ymd_hms(date, tz = Time_zone))
    
    # if (exists("MeasuredValues")) remove(MeasuredValues)
    # if (exists("Ref"))            remove(Ref)
    # #----------------------------------------------------------------CR
    # # 3) convert Reference names
    # #----------------------------------------------------------------CR
    # # remove the Units of the element if there are no data
    # Units <- Units[Components %in% names(RefData)]
    # Columns   <- names(RefData)[-which(names(RefData) == "date")]
    # # Add Ref names to be recognized if needed, "carbon monoxide parallel" is from the Los gatos anayser than filer values > 2 pmm: not ot be used.
    # Ref.names <- data.frame(Ref.names = c(             "NO2",              "CO",    "O3",                "NO",             "NOx",            "SO2",                   "Temp", "manifold flow", "temperature cpu",             "CO Los Gatos", "nitrogen dioxide caps", "pressure caps", "temperature caps"),
    #                         Columns   = c("nitrogen dioxide", "carbon monoxide", "ozone", "nitrogen monoxide", "nitrogen oxides", "sulfur dioxide", "Sample_air temperature", "manifold flow", "temperature cpu", "carbon monoxide parallel", "nitrogen dioxide caps", "pressure caps", "temperature caps")) 
    # Refs.meta <- data.frame(Columns = Columns,
    #                         Units   = Units) %>% 
    #     left_join(Ref.names, by = "Columns")
    
    # returning
    return(RefData)
}
