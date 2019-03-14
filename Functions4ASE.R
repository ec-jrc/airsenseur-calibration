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
### Function Load.Packages (170420)
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
    isInternet <- havingIP()
    if (verbose) if (isInternet) cat("[Load.Packages] Info: internet is available\n") else cat("[Load.Packages] Info: internet is not available\n")
    
    for (i in list.Packages) {
        
        if (i %in% rownames(installed.packages()) == FALSE) {
            if (verbose) cat(sprintf("[Load.Packages] INFO Installing %s", i), sep = "\n")
            if (isInternet) install.packages(i) else stop(cat(paste0("[Load.Packages] ERROR: missing internet to install package ", i), sep = "\n"))
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
ASEDigi2Volt <- function(Sensors_Cal, Digital, ADC = 16) {
    # Sensors_Cal: dataframe with the following column vectors: 
    #              name.sensor   : a vector of string with the names of sensors mounted on the AirSensEUR
    #              Ref           : a float vector, one value per sensor in Sensors_Cal$Sensors, giving the voltage in the middle of the analogue to digital converter (ADC) on the shield
    #              RefAD         : a float vector, one value per sensor in Sensors_Cal$Sensors, corresponding to the range of the ADC conversion (ref ? RefAD)
    #              Intercept     : a float vector, one value per sensor in Sensors_Cal$Sensors, intercept of conversion equation Conc[] = Intercept + Slope x Voltage
    #              Slope         : a float vector, one value per sensor in Sensors_Cal$Sensors, intercept of conversion equation Conc[] = Intercept + Slope x Voltage
    #              Sens.raw.unit : vector of strings, one value per sensor in Sensors_Cal$Sensors, with the Unit names after conversion of Digital values into concetration values
    # ADC        : number of bits of the Aanalogue to digital conversion
    # Digital    : a dataframe of numerics with the Digital values to be converted into Voltages or Currents
    # return     : the function return a dataframe with the voltages or currents for all sensors
    
    # Check which ones to converts in V and in nA
    # indexV <- which(Sensors_Cal$Sens.raw.unit == "V") # no need we convert all in volts, maybe this helps to get a dataframe onnly of numeric
    indexA <- which(Sensors_Cal$Sens.raw.unit == "nA")
    Converted <- data.frame(matrix(numeric(), ncol = length(Sensors_Cal$name.sensor), nrow = nrow(Digital)))
    colnames(Converted) <- Sensors_Cal$name.sensor
    
    # converts in Volts all variables
    # if (length(indexV)!=0) { 
    #     MyMatrix    <- Digital[,indexV]+1
    #     MyVectorMul <- 2*Sensors_Cal$RefAD[indexV]/(2^ADC)
    #     MyVectorAdd <- Sensors_Cal$Ref[indexV] - Sensors_Cal$RefAD[indexV] 
    #     Converted[,Sensors_Cal$name.sensor[indexV]] <- t(t( t(t(MyMatrix) * MyVectorMul) )+ MyVectorAdd ) 
    # } 
    MyMatrix    <- Digital[,] + 1
    MyVectorMul <- 2*Sensors_Cal$RefAD/(2^ADC)
    MyVectorAdd <- Sensors_Cal$Ref - Sensors_Cal$RefAD
    Converted[,Sensors_Cal$name.sensor] <- t(t( t(t(MyMatrix) * MyVectorMul) ) + MyVectorAdd )
    
    # converts in nA
    if (length(indexA) != 0) {
        MyMatrix    <- Digital[,indexA] + 1
        MyVectorMul <- 2*Sensors_Cal$RefAD[indexA]/(2^ADC)
        MyVectorAdd <- Sensors_Cal$Ref[indexA] - Sensors_Cal$RefAD[indexA] - Sensors_Cal$board.zero.set[indexA]
        MyVectornA  <- 10^9/(Sensors_Cal$GAIN[indexA] * Sensors_Cal$Rload[indexA])
        # converting to a matrix
        matrix.Converted <- t(t(  t(apply(Converted[,Sensors_Cal$name.sensor[indexA]], 1 , function(x) x - Sensors_Cal$board.zero.set[indexA])) ) * MyVectornA) 
        # if is not subtracted, replace
        # t(apply(Converted[,Sensors_Cal$name.sensor[indexA]], 1 , function(x) x - Sensors_Cal$board.zero.set[indexA]))
        # with Converted[,Sensors_Cal$name.sensor[indexA]]
        for (i in indexA) Converted[,Sensors_Cal$name.sensor[i]]   <- matrix.Converted[,match(i,indexA)] 
    } 
    return(Converted)
    # The name of the sensors are given by the names of the column in Digital
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
utmaxmin <- function(x, threshold) {
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
    
    # Return                = dataframe with Logical for low values, logical if value exceed lower MAD, logical if value exceed High MAD,
    #                         low MAD and high MAD
    
    #browser()
    # # Extracting the outliers with 2 rollapply (zmax and zmin)
    # if (set.Outliers) {
    #     # Removing values lower than ymin
    #     if (!is.null(ymin)) {Low_values  <- (y < ymin)}
    #     if (!is.null(ymax)) {High_values <- (y > ymax)}
    #     
    #     # max limits
    #     browser()
    #     zmax <- zoo::rollapply(zoo(y), width = window, FUN = utmax, threshold = threshold, align = "center", partial = TRUE)
    #     #zmax <- c(rep(zmax[1], window-1), zmax) # Use z[1] throughout the initial period. # Not needed if align center and partial = TRUE
    #     OutliersMax <- y > zmax
    #     #
    #     # min limits
    #     zmin <- zoo::rollapply(zoo(y), width = window, FUN = utmin, threshold = threshold, align= "center", partial = TRUE)
    #     
    #     # Changing negative values of the minimum of interval of tolerance by ThresholdMin
    #     if (!is.null(ThresholdMin)) {zmin[which(zmin<ThresholdMin)] <- ThresholdMin}
    #     
    #     #zmin <- c(rep(zmin[1], window-1), zmin) # Use z[1] throughout the initial period. # Not needed if align center and partial = TRUE
    #     OutliersMin <- y < zmin
    #     
    #     # data frame to return
    #     df <- data.frame(date = date, Low_values = Low_values, High_values = High_values, OutliersMin = OutliersMin, OutliersMax = OutliersMax, zmin = zmin, zmax = zmax) 
    # } 
    # Extracting the outliers with rollapply (zmax and zmin together)
    if (set.Outliers) {
        
        #cat("[My.rm.Outliers] INFO: computing indexes of outliers")
        
        # Removing values lower than ymin
        if (!is.null(ymin)) Low_values  <- (y < ymin)
        if (!is.null(ymax)) High_values <- (y > ymax)
        
        # max and max limits
        #browser()
        zmax.zmin <- zoo::rollapply(zoo(y), width = window, FUN = utmaxmin, threshold = threshold, align = "center", partial = TRUE)
        
        #zmax <- c(rep(zmax[1], window-1), zmax) # Use z[1] throughout the initial period. # Not needed if align center and partial = TRUE
        OutliersMax <- y > zmax.zmin[,1]
        
        # Changing the low values of the minimum of the interval of tolerance by ThresholdMin
        Index.Lower <- which(zmax.zmin[,2] < ThresholdMin)
        if (!is.null(ThresholdMin) && !is.na(ThresholdMin)) {zmax.zmin[Index.Lower,2] <- rep(ThresholdMin, length.out = length(Index.Lower))}
        
        #browser()
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
        
        #browser()
        if (Dygraphs) {
            
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
                dyRangeSelector()
            
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
            
            #browser()
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
            #browser()
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
    # This function plot the data, show the ut() cutoffs, and mark the outliers:
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
                dyRangeSelector()
            #   dyOptions(useDataTimezone = TRUE) # do not use the local time zone
            
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
    
    #browser()
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
    if (AirsensEur.name== "JRC_C5_01") {
        ts <- ts[-grep(pattern = "http://www.airsenseur.org/ch1/o3_3e1f/11915854-335/1", x = label(ts))]
    }
    if (AirsensEur.name== "JRC_C5_05") {
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
    label.variable = any(grepl(pattern = "Temperature", x=Sensors[,1]), na.rm = FALSE) & 
        any(grepl(pattern = "Relative humidity", x=Sensors[,1]), na.rm = FALSE) & 
        any(grepl(pattern = "Atmospheric pressure", x=Sensors[,1]), na.rm = FALSE)
    if (label.variable) {
        
        # Removing Institute from the Category and replace blank spaces with _
        Sensors <- data.frame(apply(Sensors, 1, function(x) {x <- substr(x, start=1, stop=unlist(gregexpr(pattern = " (", text = x, fixed= "TRUE"))-1); return(x)}))
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
            for (j in 1: length(Sensor.names[[i]])) {
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
            
        } else { # Rdata file dos not exist: take the value for SOS with minimum vaue at 2015-12-01
            
            DateIN  <- max(as.POSIXct(strptime("2015-12-01 00:00:00", format= "%Y-%m-%d %H:%M:%S", tz = ref.tzone))
                           , max(time(firstValue(ts))) ) # Tz is set to "UTC" to avoid conflict with Refdata which is in UTC although SOS used GMT
            
        }
    } else { # DownloadSensor exists: check if we have a "DateEND.SOS.prev"
        
        if (any(grepl(pattern = "DateEND.SOS.prev", x = objects(DownloadSensor)))) { # # DateEND.SOS.prev does not exist: take the value for SOS with minimum vaue at 2015-12-01
            
            if (!is.null(DownloadSensor$DateEND.SOS.prev)) {
                DateIN  <- DownloadSensor$DateEND.SOS.prev
            } else DateIN  <- max(as.POSIXct(strptime("2015-12-01 00:00:00", format= "%Y-%m-%d %H:%M:%S", tz = ref.tzone)), 
                                  max(time(firstValue(ts)), na.rm = TRUE) ) 
        } else DateIN  <- max(as.POSIXct(strptime("2015-12-01 00:00:00", format= "%Y-%m-%d %H:%M:%S", tz = ref.tzone)), max(time(firstValue(ts)), na.rm = TRUE) ) 
        
    }
    # Setting end date to curent date
    if (is.null(Duration)) Duration <- 7 # length of interval to download in days
    
    
    DateIN.partial  <- DateIN
    DateEND.partial <- DateIN + 3600 * 24 * Duration
    
    while(DateIN.partial < DateEND ) {
        
        date.partial <- lubridate::interval(DateIN.partial, DateEND.partial)
        # Downloading
        cat(paste0("[Down_SOS] INFO, downloading from ", DateIN.partial, " to ", DateEND.partial), sep = "\n")
        Buffer <- lapply(ts, function(x) {Buffer <-sensorweb4R::getData(x, timespan=date.partial);return(Buffer)})
        #Buffer <- mapply(function(x) {Buffer <-getData(x, timespan=date.partial); colnames(Buffer) <- c("date", Sensors$Pollutant[i]);return(Buffer)}, ts, Sensors$Pollutant)
        
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
            Frame <- timeAverage(Frame, avg.time = paste0(toString(UserMins)," ","min"), 
                                 statistic = "mean", start.date = round(DateIN.partial, units = "hours"), fill = TRUE)
            
            if (!"SOSData.Rdata" %in% list.files(DownloadSensor$WDinput)) {
                
                SOSData <- data.frame(Frame)
                
            } else {
                
                load(SOS.Rdata.file)
                SOSData <- rbind.fill(SOSData, data.frame(Frame)) # if merge, add , by = "date", all = TRUE
            }
            
            # convert SOSData$date to UTC to be consistent with reference data
            if (any(base::format(SOSData$date, format= "%Z") != "UTC")) attr(SOSData$date, "tzone") <- ref.tzone
            
            save(SOSData, file = SOS.Rdata.file) 
            write.csv(SOSData, file = SOS.csv.file  )
            
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
Check_Download <- function(Influx.name = NULL, WDinput, UserMins) {
    # Influx.name              = Name of for AirSensEUR in airsenseur.db, default Value NULL
    # WDinput                  = Sub directory of DisqueFieldtest where are the Refdata and InfluxData Rdata files
    # UserMins                 = periodicity of data requested after final data treatment
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
    
    #browser()
    # Set the Rdata file of input data
    airsenseur.db.file  = file.path(WDinput, "airsenseur.db")
    Ref.Rdata.file      = file.path(WDinput, "RefData.Rdata")
    Influx.Rdata.file   = file.path(WDinput, "InfluxData.Rdata")
    SOS.Rdata.file      = file.path(WDinput, "SOSData.Rdata")
    General.Rdata.file  = file.path(WDinput, "General.Rdata")
    cat("-----------------------------------------------------------------------------------\n")
    cat(paste0("[Check_Download] INFO, Checking ",
               airsenseur.db.file, "\n",
               General.Rdata.file, "\n",
               Ref.Rdata.file, "\n",
               SOS.Rdata.file, " \n",
               Influx.Rdata.file,"\n in ", WDinput, "\n"))
    
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
        
        # The directory exists, checking if RefData exists
        if (!file.exists(Ref.Rdata.file)) { # Ref.Rdata.file does not exist
            
            # RefData does not exist
            Retrieve.data.Ref = TRUE
            ExistFil.data.Ref = FALSE
            DateIN.Ref.prev   = NULL
            DateEND.Ref.prev  = NULL
            Var.Ref.prev      = NULL
            cat(paste0("[Check_Download] INFO, ", Ref.Rdata.file, " does not exist. It is going to be created, data will be retrieved."), sep = "\n")
            
        } else { 
            
            # Ref.Rdata.file exists
            ExistFil.data.Ref = TRUE
            
            cat(paste0("[Check_Download] INFO, ", Ref.Rdata.file, " exists."), sep = "\n")
            load(Ref.Rdata.file)
            
            if (!is.null(RefData)) {
                
                # RefData exists and is not NULL
                
                # Not considering end rows with only NA values for sensors
                ind <- apply(RefData[names(RefData)!= "date"], 1, function(x) !all(is.na(x)))
                DateIN.Ref.prev  <- min(RefData[ind,"date"], na.rm = TRUE)
                DateEND.Ref.prev <- max(RefData[ind,"date"], na.rm = TRUE)
                Var.Ref.prev     <- names(RefData)
                
                # Checking if Download of RefData is necessary
                # browser()
                if (difftime(Sys.time(), DateEND.Ref.prev, units = "mins") > UserMins) {    ### MG , I doubt about the tz here, I think all is changed to UTM, as it is a difference maybe it does not matter
                    
                    Retrieve.data.Ref = TRUE
                    # re-assign initial date for data retrieval   
                    cat(paste0("[Check_Download] INFO, reference data are going to be retrieved. Start new reference data at : ", DateEND.Ref.prev), sep = "\n")
                    
                } else {
                    
                    Retrieve.data.Ref = FALSE
                    #DateEND.Ref.prev   = NULL
                    cat(paste0("[Check_Download] INFO, reference data are going to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."), sep = "\n")
                    
                }
            } else {
                
                # RefData exists but it is NULL
                
                Retrieve.data.Ref <- TRUE
                DateIN.Ref.prev   <- NULL
                DateEND.Ref.prev  <- NULL
                Var.Ref.prev      <- NULL
                
                cat(paste0("[Check_Download] INFO, ", Ref.Rdata.file, " is NULL (no values). It is going to be created, data will be retrieved."), sep = "\n")
            }
        } 
        
        if (!file.exists(Influx.Rdata.file)) {
            
            # InfluxData does not exist
            ExistFil.data.Influx  = FALSE
            
            Retrieve.data.Influx  = TRUE
            DateIN.Influx.prev    = NULL
            DateEND.Influx.prev   = NULL
            cat(paste0("[Check_Download] INFO, ", Influx.Rdata.file, " does not exist. It is going to be created, sensor data will be retrieved."), sep = "\n")
            
        } else {
            
            # Influx.Rdata.file exists
            ExistFil.data.Influx  = TRUE
            
            cat(paste0("[Check_Download] INFO, ", Influx.Rdata.file, " exists."), sep = "\n")
            load(Influx.Rdata.file)
            
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
                cat(paste0("[Check_Download] INFO, ", Influx.Rdata.file, " is NULL (no values). It is going to be created, data will be retrieved."), sep = "\n")
            }
        }
        
        if (!file.exists(General.Rdata.file)) {
            
            # General.Rdata.file does not exist
            ExistFil.data.General = FALSE
            
            Retrieve.data.General  = TRUE
            DateIN.General.prev    = NULL
            DateEND.General.prev   = NULL
            cat(paste0("[Check_Download] INFO, ", General.Rdata.file, " does not exist. It is going to be created, sensor data will be retrieved."), sep = "\n")
            
        } else {
            
            # General.Rdata.file exists
            ExistFil.data.General = TRUE
            
            cat(paste0("[Check_Download] INFO, ", General.Rdata.file, " exists."), sep = "\n")
            #browser()
            load(General.Rdata.file)
            
            if (!is.null(General.df)) {
                
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
                
                # General.df exists but it is NULL
                
                Retrieve.data.General  = TRUE
                DateIN.General.prev    = NULL
                DateEND.General.prev   = NULL
                cat(paste0("[Check_Download] INFO, ", General.Rdata.file, " is NULL (no values). It is going to be created, data will be retrieved."), sep = "\n")
            }
        }
        
        if (!file.exists(SOS.Rdata.file)) {
            
            # SOS.Rdata.file does not exist
            ExistFil.data.SOS     = FALSE
            
            Retrieve.data.SOS   = TRUE
            DateIN.SOS.prev     = NULL
            DateEND.SOS.prev    = NULL
            cat(paste0("[Check_Download] INFO, ", SOS.Rdata.file, " does not exist. It should be created, SOS sensor data should be retrieved."), sep = "\n")
            
        } else {
            
            # General.Rdata.file exists
            ExistFil.data.SOS     = TRUE
            
            cat(paste0("[Check_Download] INFO, ", SOS.Rdata.file, " exists."), sep = "\n")
            load(SOS.Rdata.file)
            
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
                cat(paste0("[Check_Download] INFO, ", SOS.Rdata.file, " is NULL (no values). It is going to be created, data will be retrieved."), sep = "\n")
            }
        }
        
        if (!file.exists(airsenseur.db.file)) {
            
            # airsenseur.db.file does not exist
            ExistFil.data.db      = FALSE
            
            Retrieve.data.db      = TRUE
            DateIN.db.prev        = NULL
            DateEND.db.prev       = NULL
            cat(paste0("[Check_Download] INFO, ", SOS.Rdata.file, " does not exist. It should be created, SOS sensor data should be retrieved."), sep = "\n")
            
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
        list(Ref.Rdata.file     = Ref.Rdata.file, 
             Influx.Rdata.file  = Influx.Rdata.file, 
             SOS.Rdata.file     = SOS.Rdata.file,
             General.Rdata.file = General.Rdata.file,
             airsenseur.db.file = airsenseur.db.file,
             WDinput            = WDinput,
             ExistFil.data.db     = ExistFil.data.db     , Retrieve.data.db     = Retrieve.data.db     , DateIN.db.prev       = DateIN.db.prev,      DateEND.db.prev       = DateEND.db.prev,
             ExistFil.data.Ref    = ExistFil.data.Ref    , Retrieve.data.Ref    = Retrieve.data.Ref    , DateIN.Ref.prev      = DateIN.Ref.prev,     DateEND.Ref.prev      = DateEND.Ref.prev, Var.Ref.prev = Var.Ref.prev,
             ExistFil.data.Influx = ExistFil.data.Influx , Retrieve.data.Influx = Retrieve.data.Influx , DateIN.Influx.prev   = DateIN.Influx.prev,  DateEND.Influx.prev   = DateEND.Influx.prev,
             ExistFil.data.SOS    = ExistFil.data.SOS    , Retrieve.data.SOS    = Retrieve.data.SOS    , DateIN.SOS.prev      = DateIN.SOS.prev,     DateEND.SOS.prev      = DateEND.SOS.prev,
             ExistFil.data.General= ExistFil.data.General, Retrieve.data.General= Retrieve.data.General, DateIN.General.prev  = DateIN.General.prev, DateEND.General.prev  = DateEND.General.prev))
    
    cat("-----------------------------------------------------------------------------------\n")
    
    return(list(Ref.Rdata.file       = Ref.Rdata.file, 
                Influx.Rdata.file    = Influx.Rdata.file, 
                SOS.Rdata.file       = SOS.Rdata.file,
                General.Rdata.file   = General.Rdata.file,
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
Down_Influx_Old <- function(PROXY = FALSE, URL = NULL, PORT = NULL, LOGIN = NULL, PASSWORD = NULL,
                            Host, Port = 8086, User, Pass, name.SQLite,name.SQLite.old,  Db, Dataset, Influx.TZ = NULL, Page = NULL, Mean = NULL, use_google = TRUE) {
    # Down_Influx downloads AirSensEUR.db data from an Influx server using JSON (package Jsonlite)
    # from dleutnant/influxdbr@0.11.1. 
    # The influxdbr package can be installed beforehand or it is installed managing a possible PROXY used to download from from github
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
    # Influx.TZ                       : character, default value NULL. if NULL the function will try to determine the time zone otherwise Influx.TZ will be used
    # use_google                      : logical: default = TRUE, if TRUE the google API is used for detecting time zone from coordinates (require port 443)
    # Page                            : numeric, default value NULL, if Null the size of the page of data to download from the influx server is : LIMIT 10000, as requested in the Influx
    # Mean                            : numeric, default value NULL, time average for the download of Influx data, 
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
    list.Packages <- c("curl", "devtools", "sqldf", "zoo", "xts", "XML", "httr", "RJSONIO", "jsonlite")
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
        stop(cat("[Down_Influx] ERROR Influx server is down. Stopping the script.", "/n"))  
    } else cat("[Down_Influx] Influx server is up; connected to server\n")
    # Influx.con <- influx_connection(host = Host, port = Port, user = User, pass =  Pass, verbose = TRUE)
    # if (is.null(influx_ping(con = Influx.con))) {
    #     stop(cat("[Down_Influx] ERROR Influx server is down, stopping the script\n"))
    # } else {
    #     cat("[Down_Influx] Influx server is up; connected to server\n")
    # }
    ## # Total number of rows to download from Influx server
    
    Total.N <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)), 
                         config = authenticate(user = User, password = Pass, type = "basic"),
                         query = list(q = paste0("SELECT count(latitude) FROM ", Dataset)))
    if (Total.N$status_code != 200) stop(cat("[Down_Influx] ERROR Cannot count the number of record in airsenseur.db, Influx server may be down . Stopping the script.", "/n"))
    Total.N <- jsonlite::fromJSON(content(Total.N, "text", encoding = "ISO-8859-1"), simplifyVector = TRUE, flatten = T)
    Total.N <- as.numeric(Total.N[[1]]$series[[1]]$values[[1]][2])
    Influx.Last <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", db)), 
                             config = authenticate(user = User, password = Pass, type = "basic"),
                             query = list(q = paste0("SELECT  LAST(gpsTimestamp) FROM ", Dataset)))
    if (Last.Time$status_code != 200) stop(cat("[Down_Influx] ERROR query last GPSTime in airsenseur.db, Influx server may be down. Stopping the script.", "/n"))
    Last.Time <- jsonlite::fromJSON(content(Last.Time, "text", encoding = "ISO-8859-1"), simplifyVector = TRUE, flatten = T)
    Last.Time <- as.numeric(Last.Time[[1]]$series[[1]]$values[[1]][2])
    # Total.N <- influx_query(con              = Influx.con, 
    #                         db               = Dataset, 
    #                         query            = paste0("SELECT COUNT(channel) FROM ", Dataset), #, " Where time > '2016-10-01' ;"  - SELECT LAST(rowid) FROM ", Dataset) does not work, it gives NULL
    #                         timestamp_format = "s",   # format in seconds
    #                         return_xts       = FALSE  # If set to TRUE, xts objects are returned, FALSE gives data.frames. xts is difficult to read, better in datafram
    #                         , chunked = 10000) # verbose          = TRUE
    # Total.N <- unlist(Total.N[[1]][[1]][Dataset])[2]
    # if (is.null(Total.N)) stop(cat(paste0("[Down_Influx] ERROR cannot select data in the table ", Dataset,", stopping the script"), sep = "\n"))
    
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
                file.copy(from = name.SQLite, to = name.SQLite.old, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
                
            } else cat(paste0("[Down_Influx] INFO, the current airsenseur_old.db has the same date (saved) as airsenseur.db, no need to backup "), sep = "\n")
            
        } else {
            
            cat(paste0("[Down_Influx] INFO, there is no backup of airsenseur.db. Copying to airsenseur_old.db"), sep = "\n")
            file.copy(from = name.SQLite, to = name.SQLite.old, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
            
        }
        
    } else  { # airsenseur.db does not exist
        
        cat(paste0("[Down_Influx] INFO, ", name.SQLite, " does not exist and it is going to be created."), sep = "\n")
        
    }
    SQLite.con <- dbConnect(SQLite(), dbname = name.SQLite)
    
    #------------------------------------------------------------------------------CR
    # table dataset exists?
    #------------------------------------------------------------------------------CR
    #browser()
    if (dbExistsTable(conn = SQLite.con, name = Dataset)) { # the table Dataset exists in airsenseur.db
        
        cat(paste0("[Down_Influx] INFO, the table ", Dataset, " already exists in airsenseur.db"), sep = "\n")
        
        # Counting the number of records in AirSensEUR$Dataset
        #Dataset.N   <- dbGetQuery(SQLite.con, paste0("SELECT COUNT(gpsTimestamp) FROM ", Dataset))[1,1]
        Dataset.N   <- dbGetQuery(SQLite.con, paste0("SELECT max(rowid) FROM ", Dataset))[1,1]
        
        # Error Message and stop the script if there more data in the airsenseur.db than in InfluxDB
        if (Dataset.N > Total.N) {
            stop(cat("[Down_Influx] ERROR there are more records in your local airsenseur.db than in InfluxDB. Cannot dowload data. The script is stopped."))   
            
        } else cat(paste0("[Down_Influx] INFO, ", Total.N - Dataset.N, " records are going to be added into the table ", Dataset, " in the local airsenseur.db"), sep = "\n") 
        Dataset.index   <- FALSE # if airsenseur.db exists then the indexes were already created, then no need to create the indexes
        
    } else {# the table Dataset exists in airsenseur.db
        
        # There are no records in AirSensEUR$Dataset
        cat(paste0("[Down_Influx] INFO, the table ", Dataset, " does not exist in airsenseur.db. It is going to be created"), sep = "\n")
        Dataset.N       <- 0
        Dataset.index   <- TRUE # if true indexes will be created
        
    } # the table Dataset exists in airsenseur.db
    
    #------------------------------------------------------------------------------CR
    # Downloading InfluxDB data and add them to the airsenseur.db
    #------------------------------------------------------------------------------CR
    # create a counter of Downloaded values, assume there is no GPS Coordinate
    Download.N <- 0
    exist.GPS <- FALSE
    
    # getting the default Page of data to download
    if (is.null(Page)) Page <- 10000
    # getting the time average of data to download
    if (is.null(Mean)) Mean <- 60
    # Downloading
    
    
    while (Download.N < (Total.N - Dataset.N)) {
        
        # For the last Page to download used modulo instead of Page in order to to download twice the same records
        if ((Download.N + Page) > (Total.N - Dataset.N)) {Page <- (Total.N - Dataset.N) %% Page } 
        
        # Downloading (be careful with LIMIT and oFFSET to be sure not to have real in scientific notation)
        # In InfluxDB 0.8 there was a problem with the int64 type of data in the airsenseur.db that used for gpsTimeStamp and BoardTimeStmap
        # Starting time in ms from 1/1/1970 were given value > 1.47 10^12, the biggest int in R which uses 32 bit maxi for int.
        # Conversely the current implementations of R can only use 32-bit integers for integer vectors, so that the range of representable integers
        # is restricted to about +/-2*10^9, doubles could  hold much larger integers exactly.
        # Consequence the collectedts (in ms from 1/1/1970) field of the table measures that is about 1.47 10^12 is truncated and it is wrong.
        # SOLUTION: use the dbGetQuery function to convert to real with function CAST(timeStamp AS REAL)
        # With InfluxDB 1.0, it seem that all times are in seconds by specifying timestamp = "s", when runing an influx_query, making gpsTimeStamp < 2 e-9
        # The Influxdbr is not used anymore, it does not work since 17-07-15. We use now httr and JSON,
        # result <- influx_query(con              = Influx.con, 
        #                        db               = Db, 
        #                        query            = paste0("SELECT altitude, boardTimeStamp, channel, gpsTimestamp, latitude, longitude, \"name\", sampleEvaluatedVal, sampleRawVal FROM ", Dataset,
        #                                                  " LIMIT ", format(Page, scientific = FALSE), " OFFSET ", format(Dataset.N + Download.N, scientific = FALSE)), 
        #                        # be sure to use \"name\" to avoid confusion with the command name
        #                        # Where time > '2016-10-01' ;"  - SELECT LAST(rowid) FROM ", Dataset) does not work, it gives NULL
        #                        timestamp_format = "s",   # format in seconds
        #                        return_xts       = FALSE # If set to TRUE, xts objects are returned, FALSE gives data.frames. xts is difficult to read, better in datafram
        # ) # verbose          = FALSE
        # # Adding the downloaded sendor data to Adding dataFrame 
        # Adding <- as.data.frame(result[[1]][[1]][Dataset])
        # 
        # # Testing Adding to download again if nrow(Adding) < Page
        # Trial <- 1
        # while ( nrow(Adding) < Page & Trial < 5) {
        #     if (Trial <= 5) {
        #         if (nrow(Adding) == Page) {
        #             
        #             cat(paste0("[Down_Influx] INFO, trial n.", Trial,". N. of record downloaded: ", nrow(Adding), " equals to query request between lines ", Download.N, " and ", Download.N + Page), sep = "\n")
        #             
        #         } else {
        #             
        #             cat(paste0("[Down_Influx] ERROR trial n.", Trial,". N. of record downloaded too low: ", nrow(Adding), " between lines ", Download.N, " and ", Download.N + Page, ". There will be 5 trials to download"), sep = "\n")
        #             
        #         }
        #         
        #     } else stop(cat(paste0("[Down_Influx] ERROR trial n.", Trial,". N. of record downloaded too low: ", nrow(Adding), " between lines ", Download.N, " and ", Download.N + Page, ". After 5 trials the script is stopped."), sep = "\n"))
        #     
        #     # downloading again
        #     result <- influx_query(con              = Influx.con, 
        #                            db               = Db, 
        #                            query            = paste0("select time, altitude, boardTimeStamp, channel, gpsTimestamp, latitude, longitude, \"name\", sampleEvaluatedVal, sampleRawVal from ", Dataset, " LIMIT ", format(Page, scientific = FALSE), " OFFSET ", format(Download.N, scientific = FALSE) ), #, " Where time > '2016-10-01' ;"
        #                            timestamp_format = "s",   # format in seconds
        #                            return_xts       = FALSE, # If set to TRUE, xts objects are returned, FALSE gives data.frames. xts is difficult to read, better in datafram
        #                            verbose          = FALSE)
        #     
        #     # Adding the downloaded sendor data to Adding dataFrame 
        #     Adding <- as.data.frame(result[[1]][[1]][Dataset])
        #     Trial <- Trial + 1
        #     
        # }
        # # removing the name of the dataset from the colnames
        # colnames(Adding) <- sub(pattern = paste0(Dataset,"."), replacement = "", x = colnames(Adding), ignore.case = TRUE)
        # 
        # # Converting time to character to avoid to loose information with large POSIXct number
        # Adding$time <- as.character(Adding$time)
        
        cat(paste0("[Down_Influx] INFO, starting downloading sensor data from the Influx server ..."), sep = "\n")
        
        results <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)), 
                             config = authenticate(user = User, password = Pass, type = "basic"),
                             query = list(q = paste0("SELECT altitude, boardTimeStamp, channel, gpsTimestamp, latitude, longitude, \"name\", sampleEvaluatedVal, sampleRawVal FROM ", Dataset,
                                                     " LIMIT ", format(Page, scientific = FALSE), " OFFSET ", format(Dataset.N + Download.N, scientific = FALSE))
                             ))
        results <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)), 
                             config = authenticate(user = User, password = Pass, type = "basic"),
                             query = list(q = paste0("SELECT mean(*) FROM ", Dataset, " WHERE time >= '2018-01-01T23:48:00Z' AND time <= '2018-02-05T00:54:00Z' GROUP BY time(1m,1440m),\name\ ")
                             ))
        
        Trial <- 1
        if (results$status_code != 200) {
            
            while (results$status_code != 200 & Trial < 5) {
                
                if (Trial <= 5) {
                    
                    if (nrow(Adding) == Page) {
                        
                        cat(paste0("[Down_Influx] INFO, trial n.", Trial,". N. of record downloaded: ", nrow(Adding), " equals to query request between lines ", Download.N, " and ", Download.N + Page), sep = "\n")
                        
                    } else {
                        
                        cat(paste0("[Down_Influx] ERROR trial n.", Trial,". N. of record downloaded too low: ", nrow(Adding), " between lines ", Download.N, " and ", Download.N + Page, ". There will be 5 trials to download"), sep = "\n")
                        
                    }
                    
                } else stop(cat(paste0("[Down_Influx] ERROR trial n.", Trial,". N. of record downloaded too low: ", nrow(Adding), " between lines ", Download.N, " and ", Download.N + Page, ". After 5 trials the script is stopped."), sep = "\n"))
                
                
                # downloading again
                results <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)), 
                                     config = authenticate(user = User, password = Pass, type = "basic"),
                                     query = list(q = paste0("SELECT altitude, boardTimeStamp, channel, gpsTimestamp, latitude, longitude, \"name\", sampleEvaluatedVal, sampleRawVal FROM ", Dataset,
                                                             " LIMIT ", format(Page, scientific = FALSE), " OFFSET ", format(Dataset.N + Download.N, scientific = FALSE))
                                     ))
                
                # Adding the downloaded sendor data to Adding dataFrame 
                Adding <- as.data.frame(result[[1]][[1]][Dataset])
                Trial <- Trial + 1
                
            }
        } 
        # extracting lists from json
        results <- jsonlite::fromJSON(content(results, as= "text", encoding = "ISO-8859-1"),  flatten=TRUE)
        Adding  <- data.frame(results[[1]]$series[[1]]$values[[1]], row.names = NULL, check.rows = FALSE,
                              check.names = FALSE, fix.empty.names = TRUE,
                              stringsAsFactors = FALSE)
        names(Adding) <- gsub(pattern = "mean_",replacement = "", x = results[[1]]$series[[1]]$columns[[1]])
        #Adding$time <- ymd_hms(Adding$time)
        Adding[,-which(names(Adding) %in% c("time","name"))] <- sapply(Adding[-which(names(Adding) %in% c("time","name"))], as.numeric)
        #Adding[,c("channel","sampleRawVal")] <- as.integer(Adding[,c("channel","sampleRawVal")]) # Error (list) object cannot be coerced to type 'integer'
        #print(result)
        if (any(!isZero(Adding$gpsTimestamp))) exist.GPS <- TRUE
        
        # adding data to airSensEUR.db
        #browser()
        RSQLite::dbWriteTable(conn = SQLite.con, name = Dataset, value = Adding, append = TRUE)
        cat(paste0("[Down_Influx] INFO, ", format(Download.N + nrow(Adding), scientific = FALSE), "/",(Total.N - Dataset.N)," records downloaded and added to table ", Dataset, " of airsenseur.db"), sep = "\n")
        
        # updating counter
        Download.N  <- Download.N + nrow(Adding)
        
    }
    cat(paste0("[Down_Influx] INFO, the downloading of sensor data from the Influx server is finished."), sep = "\n")
    #  show_field_keys(con = Influx.con, db = Db, measurement = NULL)
    # We need to add index
    
    # Counting the number of records in AirSensEUR$Dataset
    Dataset.N   <- dbGetQuery(SQLite.con, paste0("SELECT max(rowid) FROM ", Dataset))[1,1]
    if (Dataset.N == Total.N) {
        
        cat(paste0("[Down_Influx] INFO, there is the same number of records in the local airsenseur.db and InfluxDB for ", Dataset), sep = "\n")
        
    } else {
        
        stop(cat(paste0("[Down_Influx] ERROR the number of records in the local airsenseur.db and InfluxDB is not the same for ", Dataset, ". The script is stopped"), sep = "\n"))
        
    }
    
    # getting the time zone, port 443 of the Browser shall be opened
    if (is.null(Influx.TZ) ) { ##remove & exist.GPS
        cat(paste0("[Down_influx] INFO, determining the time zone with the last valid latitude and longitude of ", Dataset, " in airsenseur.db"), sep = "\n")
        #Coord.lat.long <- rep(0,length.out = 2)
        Offset <- Total.N
        Coord.lat.long   <- dbGetQuery(SQLite.con, paste0("SELECT time, longitude, latitude  FROM ", Dataset, " WHERE rowid > ", Offset - 10, " AND rowid <= ", Offset, " ;"))
        # Starting from the end selecting the first non NA coordinates
        while(all(Coord.lat.long[,c("longitude","latitude")] == 0) & Offset > 10) {
            Coord.lat.long   <- dbGetQuery(SQLite.con, paste0("SELECT time, longitude, latitude  FROM ", Dataset, " WHERE rowid > ", Offset - 10, " AND rowid <= ", Offset, " ;"))
            Offset <- Offset - 10
        }
        
        if (any(Coord.lat.long != 0)) {
            Lastlat     <- tail(na.omit(Coord.lat.long$latitude[Coord.lat.long$latitude != 0]), n = 1)
            LastLong    <- tail(na.omit(Coord.lat.long$longitude[Coord.lat.long$longitude != 0]), n = 1) 
            Influx.TZ <- find_tz(LastLong, Lastlat, use_google = use_google)
            cat(paste0("[Down_influx] INFO, the time zone of the sensor data is ", Influx.TZ), sep = "\n")
        }
        
    } 
    # # getting the last date, latitude and longitude in name.SQLite.
    # Last date to add only new data, latitude and longitude to get the time zone 
    # checking for non zero values and no NA()
    LastDate <- dbGetQuery(SQLite.con, paste0("SELECT time FROM ", Dataset," ORDER BY rowid DESC LIMIT 1;"))
    # InfluxDB gives everything in UTC not in local time zone - Well by observation in grafana it seems that the dates are in Local Time
    if (is.null(Influx.TZ)) {
        LastDate <- as.POSIXct(strptime(LastDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) 
    } else {
        LastDate <- as.POSIXct(strptime(LastDate, format = "%Y-%m-%d %H:%M:%S", tz = Influx.TZ))
    } 
    # Disconnect Influx, STRANGE there is no command to disconnect !
    # dbDisconnect(conn = Influx.con)    
    
    # Creating index to speed up select in function SQLite2df
    if (Dataset.index) {
        dbGetQuery(SQLite.con, paste0("CREATE INDEX IDtime ON "   , Dataset, " (time);"))
        dbGetQuery(SQLite.con, paste0("CREATE INDEX IDchanne ON " , Dataset, " (channel);"))
        dbGetQuery(SQLite.con, paste0("CREATE INDEX IDname ON "   , Dataset, " (name);"))
    } 
    # Disconnect SQLite.con
    dbDisconnect(conn = SQLite.con)
    #browser()
    cat(paste0("[Down_Influx] INFO, the airsenseur.db goes until ", LastDate, ", with ", Total.N, " records for the table ", Dataset), sep = "\n")
    cat("-----------------------------------------------------------------------------------\n")
    cat("\n")
    return(Influx.TZ)
    
}
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
                JSON.Colnames <- gsub(pattern= "mean_", replacement= "",JSON$results$series[[1]]$columns[[1]])
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
        #browser()
        if (!is.null(Discard)) if (any(colnames(JSON) %in% Discard)) JSON <- JSON[,-which(colnames(JSON) %in% Discard)]
        #print(JSON, quote = FALSE)
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
    list.Packages <- c("curl", "devtools", "sqldf", "zoo", "xts", "XML", "httr", "RJSONIO", "jsonlite")
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
                #browser()
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
        
    } else  { # airsenseur.db does not exist
        
        cat(paste0("[Down_Influx] INFO, ", name.SQLite, " does not exist and it is going to be created."), sep = "\n")
        
    }
    SQLite.con <- dbConnect(SQLite(), dbname = name.SQLite)
    #------------------------------------------------------------------------------CR
    # table dataset exists?
    #------------------------------------------------------------------------------CR
    #browser()
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
        #browser()
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
    #browser()
    # List of sensors to download
    # Sensors names, it seems that I cannot query directly name or channel (strings). Adding SELECT of a float field it works. Selecting the first 50 ones. Use SHOW TAG VALUES INSTEAD
    Influx.Sensor <-  httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)), 
                                config = authenticate(user = User, password = Pass, type = "basic"),
                                query = list(q = paste0("SHOW TAG VALUES FROM ", Dataset," WITH KEY IN ( \"name\") ; ")))
    Influx.Sensor <- Json_To_df(Influx.Sensor); 
    #browser()
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
    
    
    # Downloading always in increasing date
    #while (Download.N < (Influx.Total.N - Dataset.N)) {
    Step <- 30 * 24 * 60 * 60 # first digit  (1) is number of days
    while (difftime(Influx.Last$time, SQL.time.Last, units = "mins") > Mean) {
        
        # Calculating page for "Limit Page" corresponding to Step (1 day)
        # Influx.Count.Data.Step <-  httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)), 
        #                                   config = authenticate(user = User, password = Pass, type = "basic"),
        #                                   query = list(q = paste0("SELECT COUNT(altitude) FROM ", Dataset," WHERE time >= '",format(ymd_hms(SQL.time.Last),"%Y-%m-%d %H:%M:%S"),
        #                                                           "' AND time <= '",format(ymd_hms(SQL.time.Last) + Step,"%Y-%m-%d %H:%M:%S"),"';")))#, , " GROUP BY \"name\" "" LIMIT 100000"
        # Page  <- Json_To_df(Influx.Count.Data.Step, Numeric = c("count"))$count
        
        # Determine Field Keys on which to average on time
        # Field.Keys <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)), 
        #                         config = authenticate(user = User, password = Pass, type = "basic"),
        #                         query = list(q = paste0("SHOW FIELD KEYS FROM ", Dataset)))
        # Field.Keys <- Json_To_df(Field.Keys)$fieldKey
        
        # query without mean
        # results <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)), 
        #                       config = authenticate(user = User, password = Pass, type = "basic"),
        #                       query = list(q = paste0("SELECT altitude, boardTimeStamp, channel, gpsTimestamp, latitude, longitude, \"name\", sampleEvaluatedVal, sampleRawVal FROM ", Dataset,
        #                                               " LIMIT ", format(Page, scientific = FALSE), " OFFSET ", format(Dataset.N + Download.N, scientific = FALSE))
        #                       ))
        
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
                    
                    # Dropping sampleRawVal
                    #Adding <- Adding[, - which(colnames(Adding) == "sampleRawVal")]
                    #Adding$time <- ymd_hms(Adding$time)
                    
                    # # Converting time to character to avoid to loose information with large POSIXct number
                    # Adding$time <- as.character(Adding$time)
                    
                } else next
                
            }
            
            # updating Adding
            if (exists("All.Sensors.Adding")) All.Sensors.Adding <- rbind.fill(All.Sensors.Adding,Adding) else All.Sensors.Adding <- Adding
            
        }
        
        #browser()
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
        } else cat(paste0("[Down_Influx] INFO, No influx data between ", format(ymd_hms(SQL.time.Last),"%Y-%m-%d %H:%M")," and ",format(ymd_hms(SQL.time.Last)+Step,"%Y-%m-%d %H:%M"),".\n"))
        
        # updating SQL.time.Last for while loop
        #browser()
        SQL.time.Last  <- ymd_hms(SQL.time.Last)+Step
        
    } 
    
    cat(paste0("[Down_Influx] INFO, the downloading of sensor data from the Influx server is finished.\n"))
    # I need to add index ?!?
    
    # Counting the number of records in AirSensEUR$Dataset
    Dataset.N   <- dbGetQuery(SQLite.con, paste0("SELECT max(rowid) FROM ", Dataset))[1,1]
    
    # getting the time zone, port 443 of the Browser shall be opened
    #browser()
    if (is.null(Influx.TZ) || Influx.TZ == "Local time") { 
        
        cat(paste0("[Down_influx] INFO, determining the time zone with the last valid latitude and longitude of ", Dataset, " in airsenseur.db"), sep = "\n")
        
        Offset <- Dataset.N
        repeat{
            
            Coord.lat.long   <- dbGetQuery(SQLite.con, paste0("SELECT time, longitude, latitude FROM ", Dataset, " WHERE rowid > ", Offset - 500, " AND rowid <= ", Offset, " ;"))
            # print(Coord.lat.long, quote = F)
            # browser()
            
            if (all(is.na.data.frame(Coord.lat.long[,c("longitude","latitude")])) || 
               all(Coord.lat.long[!is.na.data.frame(Coord.lat.long[,c("longitude")]),c("longitude","latitude")] ==0)) {
                
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
    #browser()
    LastDate <- dbGetQuery(SQLite.con, paste0("SELECT time FROM ", Dataset," ORDER BY rowid DESC LIMIT 1;"))$time
    # InfluxDB gives everything in UTC not in local time zone - Well by observation in grafana it seems that the dates are in Local Time
    if (is.null(Influx.TZ) || Influx.TZ == "Local time") {
        LastDate <- ymd_hms(LastDate, tz = "UTC")
    } else {
        LastDate <- ymd_hms(LastDate, tz = Influx.TZ)
    } 
    
    # Creating index to speed up select in function SQLite2df
    if (Dataset.index) {
        dbGetQuery(SQLite.con, paste0("CREATE INDEX IDtime ON "   , Dataset, " (time);"))
        dbGetQuery(SQLite.con, paste0("CREATE INDEX IDchanne ON " , Dataset, " (channel);"))
        dbGetQuery(SQLite.con, paste0("CREATE INDEX IDname ON "   , Dataset, " (name);"))
    } 
    # Disconnect SQLite.con
    dbDisconnect(conn = SQLite.con)
    cat(paste0("[Down_Influx] INFO, the airsenseur.db goes until ", format(LastDate, "%Y-%m-%d %H:%M"), ", with ", Dataset.N, " records for the table ", Dataset), sep = "\n")
    cat("-----------------------------------------------------------------------------------\n")
    cat("\n")
    #browser()
    return(Influx.TZ)
}

#=====================================================================================CR
# 161123 MG : Sqlite2df                 converting a local airsenseur.db into a General dataframe
#=====================================================================================CR
Sqlite2df <- function(name.SQLite, Dataset, Influx.TZ, UserMins = NULL, DownloadSensor = NULL, Page = NULL, Complete = FALSE, asc.File=NULL) {
    # Sqlite2df transforms an airsenseur.db table into a General data frame. airsenseur.db shall be created previously with Down_Influx 
    
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
    #                   The time zone is the one of InfluxDB and SOS (GMT). 
    #                   Default value for DownloadSensor$Influx.Rdata.file is NULL, nothing passed. In this case, SQLite2df
    #                   creates new Rdata/csv
    # Page              : numeric, default value NULL, if Null the size of the page of data to download from the influx server is LIMIT to 200000
    # complete          : Logical, default is FALSE, If TRUE the Sqlite2df function will return a dataFrame concatenating the existing data in name.Sqlite with the new ones in Values_db
    # asc.File          : dataframe, default is NULL, used for giveing the correct name of the sensor
    # 
    # Return            : A Values_db (existing data added if any) dataframe with date (as.POSIXct) to be used by openair, coordinates
    #                     , 7 sensor values as downloaded from Influx. Data are averaged with UserMins averaging time if Averaging is TRUE
    # Dependence        : Load.Packages
    ### Still need adding when the AirSensEUR is switched on and off, when the name of sensors are changed and when it is at the Reference Stations
    
    #------------------------------------------------------------------------------CR
    # Set time interval - Not used anymore this is commented
    #------------------------------------------------------------------------------CR
    #minSec <- UserMins*60.
    
    # set range time for data retrieving, either from origin or last date in previous DataFrame
    #if (any(grepl(pattern = "DateIN.0.General.prev", x = objects(DownloadSensor)))) {
    #    DateIN  <- DownloadSensor$DateIN.0.General.prev + minSec
    #} else DateIN  <- as.POSIXct("2015-12-01 00:00.00 UTC")
    # Setting end date to curent date
    #DateEND <- as.POSIXct(Sys.Date())
    #
    # Set time interval, with function interval of package lubridate
    # InterVal <- lubridate::interval(DateIN, DateEND, tzone= "UTC")
    # Creating the General Data.Frame
    # General <- data.frame(date = seq(InterVal@start, length = InterVal@.Data/minSec, by = paste0(toString(UserMins)," ","min")),                     
    #                      row.names = NULL, check.rows = FALSE,
    #                      check.names = TRUE,
    #                     stringsAsFactors = FALSE)
    
    #browser()
    cat("\n")
    cat("-----------------------------------------------------------------------------------\n")
    #------------------------------------------------------------------------------CR
    # Installing necessary packages
    #------------------------------------------------------------------------------CR
    # Both RSQLite and sqldf (and others too) are loaded by library(sqldf), so it is enough to instal sqldf
    list.Packages <- c("sqldf", "openair", "reshape")
    Load.Packages(list.Packages)
    
    #browser()
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
    # Reading local airsenseur.db in slice of Page records - checking if InfluxData.Rdata and InfluxData.csv already exist to only add the necessary data
    #------------------------------------------------------------------------------CR
    cat(paste0("[SQLite2df] INFO, reading table ", Dataset), sep = "\n")
    # Initial values
    Download.N  <- 0
    SQL.Total.N     <- dbGetQuery(SQLite.con, paste0("SELECT max(rowid) FROM ", Dataset))[1,1]
    if (!is.null(DownloadSensor$DateEND.Influx.prev)) { # the table Dataset exists in airsenseur.db
        
        cat(paste0("[SQLite2df] INFO, the files InfluxData.Rdata and InfluxData.csv already exist."), sep = "\n")
        
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
        #browser()
        
        # adding data to Values_db
        cat(paste0("[SQLite2df] INFO, ", format(Download.N + nrow(Adding), scientific = FALSE), "/",(SQL.Total.N - Dataset.N)," records were read "), sep = "\n")
        if (exists("Values_db")) Values_db <- rbind.fill(Values_db, Adding) else Values_db <- Adding
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
    Meteo.names.change  <- data.frame(Influx.names  = c("Humid", "Tempe","Temp","Press"), 
                                      General.names = c("Relative_humidity", "Temperature", "Temperature", "Atmospheric_pressure"), stringsAsFactors = FALSE)
    #------------------------------------------------------------------------------CR
    # Adding final name (Temperature, Relative ...) 
    #------------------------------------------------------------------------------CR
    Channel.names <- unique(Values_db[,c("channel","name")])
    for (i in Meteo.names.change$Influx.names) {
        
        if (i %in% Channel.names$name) {
            
            # setting column variables in Channel.names with names of Meteo.names.change
            Channel.names[Channel.names$name == i,"Variables"] <- Meteo.names.change[Meteo.names.change$Influx.names == i,"General.names"]  
            
            # setting correct colnames in values_db for Meteo.names.change if chang are requested
            Sensor.rows <- which(Values_db$name==i)
            Values_db[Sensor.rows,"Pollutants"]   <- Meteo.names.change[Meteo.names.change$Influx.names == i,"General.names"] 
            #Values_db[Sensor.rows,"name"]         <- Meteo.names.change[Meteo.names.change$Influx.names == i,"General.names"] 
        }
    } 
    
    cat(paste0("[SQLite2df] INFO, looking for the names of all sensors and channel numbers from a list of sensor names, using best guess"), sep = "\n")
    if (is.null(asc.File)) {
        
        # In case of several sensor name on the same channel number
        for (i in Channel.names$channel) {
            
            # Taking the last name if sensors have been replaced
            if (anyDuplicated.array(Channel.names[Channel.names$channel == i-1,c("channel","name")])) {
                Channel.names$name[i+1] <- paste0(unique(Values_db[Values_db$channel == i, "name"]),collapse = "!")
                if (grepl(pattern = "!", x = Channel.names$Sensor.names[i+1])) {
                    
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
    Sensor.names        <- list(Nitrogen_dioxide      = c("no2_b43f", "NO2-B43F", "NO2B43F", "NO2/C-20", "NO23E50", "NO2_3E50", "NO2", "S1"),
                                Carbon_monoxide       = c("CO-B4", "CO-A4", "CO/MF-200", "CO/MF-20", "CO-MF200", "CO3E300", "CO_3E300", "CO","COMF200","CO-A4 O","COA4", "S2"),
                                Ozone                 = c("O3/M-5", "O3-B4", "AX-A431", "OX-A431", "OX_A431", "O3-A431", "O33EF1", "O3_3E1F", "O3", "O3E100", "o3_m_5", "O3_M5", "O3-M5", "S3"),
                                Nitric_oxide          = c("NO-B4", "NOB4_P1","NOB4", "NO/C-25", "NO3E100", "NO_3E100", "NO", "No Sensor", "S4"),
                                Particulate_Matter_1  = c("OPCN2PM1", "OPCN3PM1"), 
                                Particulate_Matter_25 = c("OPCN2PM25", "OPCN3PM25"),
                                Particulate_Matter_10 = c("OPCN2PM10", "OPCN3PM10"),
                                Bin0                  = c("OPCN2Bin0", "OPCN3Bin0"),
                                Bin1                  = c("OPCN2Bin1", "OPCN3Bin1"),
                                Bin2                  = c("OPCN2Bin2", "OPCN3Bin2"),
                                Bin3                  = c("OPCN2Bin3", "OPCN3Bin3"),
                                Bin4                  = c("OPCN2Bin4", "OPCN3Bin4"),
                                Bin5                  = c("OPCN2Bin5", "OPCN3Bin5"),
                                Bin6                  = c("OPCN2Bin6", "OPCN3Bin6"),
                                Bin7                  = c("OPCN2Bin7", "OPCN3Bin7"),
                                Bin8                  = c("OPCN2Bin8", "OPCN3Bin8"),
                                Bin9                  = c("OPCN2Bin9", "OPCN3Bin9"),
                                Bin10                 = c("OPCN2Bin10", "OPCN3Bin10"),
                                Bin11                 = c("OPCN2Bin11", "OPCN3Bin11"),
                                Bin12                 = c("OPCN2Bin12", "OPCN3Bin12"),
                                Bin13                 = c("OPCN2Bin13", "OPCN3Bin13"),
                                Bin14                 = c("OPCN2Bin14", "OPCN3Bin14"),
                                Bin15                 = c("OPCN2Bin15", "OPCN3Bin15"),
                                Bin16                 = c("OPCN2Bin15", "OPCN3Bin16"),
                                Bin17                 = c("OPCN2Bin15", "OPCN3Bin17"),
                                Bin18                 = c("OPCN2Bin15", "OPCN3Bin18"),
                                Bin19                 = c("OPCN2Bin15", "OPCN3Bin19"),
                                Bin20                 = c("OPCN2Bin15", "OPCN3Bin20"),
                                Bin21                 = c("OPCN2Bin15", "OPCN3Bin21"),
                                Bin22                 = c("OPCN2Bin15", "OPCN3Bin22"),
                                Bin23                 = c("OPCN2Bin15", "OPCN3Bin23"),
                                OPCHum                = c("OPCN3Hum"),
                                OPCLsr                = c("OPCN3Lsr"),
                                OPCTsam               = c("OPCN3TSam"),
                                OPCVol                = c("OPCN2Vol", "OPCN3Vol"),
                                OPCTemp               = c("OPCN2Temp", "OPCN3Temp"),
                                MOx                   = c("MOX"),
                                Carbon_dioxide        = c("D300")
                                
                                
    ) # Add new sensor model to be recognized if needed
    
    #------------------------------------------------------------------------------CR
    # Adding sensor model type (Nitic_Oxide...) if more than 1 model of sensors then the model type are separated with "!" - 
    # The last sensor mdel type is used
    #------------------------------------------------------------------------------CR
    for (i in 1:length(Sensor.names)) {
        
        for (j in 1: length(Sensor.names[[i]])) {
            
            if (Sensor.names[[i]][j] %in% Channel.names$name) {
                
                Channel.names[which(Channel.names$name == Sensor.names[[i]][j]), "Variables"] <- names(Sensor.names)[i]
                break
            } 
        }
    }
    
    #browser()
    cat("[SQLite2df] INFO, sensors found in the airsenseur.db\n")
    print(cbind(Channel.names, lubridate::ymd_hms(Values_db$time[as.numeric(row.names(Channel.names))]) ))
    
    # Setting Values_db$Pollutants that gives the correct Polluants names even if the sensors are changed of position during use, not for chemical sensors, already done
    for (i in unique(Channel.names$name)) {
        
        # setting correct colnames in values_db for Meteo.names.change if chang are requested
        cat(paste0("[SLite2df] INFO setting Values_db$Pollutants to ", unique(Channel.names[Channel.names$name == i,"Variables"])," using the shield config file for sensor ", i), sep = "\n")
        Sensor.rows <- which(Values_db$name==i)
        if (length(Sensor.rows) > 0) {
            
            Values_db[Sensor.rows,"Pollutants"]   <- unique(Channel.names[Channel.names$name == i,"Variables"])
            #Values_db[Sensor.rows,"name"]         <- unique(Channel.names[Channel.names$name == i,"Variables"])
        }
    } 
    
    #browser()
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
    Values_db[which(Values_db$altitude  == 0), "altitude"]  <- rep(NA, length(which(Values_db$altitude  ==0)))
    Values_db[which(Values_db$longitude == 0), "longitude"] <- rep(NA, length(which(Values_db$longitude ==0)))
    Values_db[which(Values_db$latitude  == 0), "latitude"]  <- rep(NA, length(which(Values_db$latitude  ==0)))
    
    # Aggregating in tabulated form.
    i <- 0
    if (nrow(Values_db) < Page) Page <- nrow(Values_db)
    while(i < nrow(Values_db)) {
        # Checking for a correct Page value for paging
        if ((i + Page)> nrow(Values_db)) Page <- nrow(Values_db) - i
        
        cat(paste0("[SQLite2df] INFO, aggregating airsenseur.db in tabulated rows ", format(i+Page,scientific = FALSE),"/", nrow(Values_db)), sep = "\n" )
        # casting data according to channel names
        Buffer <- cast(Values_db[(i+1):(i+Page),], time + boardTimeStamp + gpsTimestamp + altitude + latitude + longitude  ~ Pollutants, 
                       value = "sampleEvaluatedVal", fun.aggregate = 'mean' , fill = NA, na.rm = TRUE)
        
        # aggregating in Values_db_cast
        # if (exists("Values_db_cast")) Values_db_cast <- rbind.fill(Values_db_cast, Buffer) else Values_db_cast <- Buffer
        if (exists("Values_db_cast")) Values_db_cast <- rbindlist(list(Values_db_cast, Buffer), use.names = TRUE, fill = TRUE) else Values_db_cast <- Buffer
        i <- i + Page
    }
    #browser()
    Values_db <- data.frame(Values_db_cast)
    
    #for (i in Channel.names$channel) colnames(Values_db)[which(colnames(Values_db) ==i)] <- Channel.names$Variables[which(Channel.names$channel == i)]
    rm(Values_db_cast, Buffer)
    remove(Sensor.names, Channel.names)
    remove(Meteo.names.change)
    
    # Transforming column time in POSIX with the correct time zone (UTC), changing name to date
    # I thought that all values from Influx were in tz UTC but it rather seems that they are in local time
    #browser()
    if (is.null(Influx.TZ)) {
        cat("[SQLite2df] INFO, Converting Values_db$time from character to POSIX format, ERROR time zone is not set for InfluxDB.\n")
        cat("[SQLite2df] ERROR: you must set the parameter TZ in the ASEConfig_xx.R file. The script should be  stopped.")
    } else{
        cat(paste0("[SQLite2df] INFO, Converting Values_db$time from character to POSIX format, timezone is ", Influx.TZ), sep = "\n")
        #Values_db$time <- as.POSIXct(strptime(Values_db$time, format = "%Y-%m-%d %H:%M:%S", tz = Influx.TZ)) 
        Values_db$time <- lubridate::ymd_hms(Values_db$time, tz = Influx.TZ)
    }
    # convert Values_DB$time to UTC to be consistent with reference data
    #if (any(base::format(Values_db$time, format= "%Z") != "UTC")) attr(Values_db$time, "tzone") <- "UTC"
    
    # Change to date to use OpenAir
    names(Values_db)[colnames(Values_db) == "time"] <- "date"
    
    #browser()
    # Averaging with UserMIns averaging time, creating Values_db_Mins
    if (!is.null(UserMins)) {
        
        cat(paste0("[SQLite2df] INFO, averaging each ", UserMins, " mins. This can be long with large datasets."), sep = "\n")
        
        # we will have to wait UserMins mins to have new values
        i <- lubridate::floor_date(Values_db$date[1],  unit = paste0(toString(UserMins)," ","min"))
        while(difftime(max(Values_db$date), i, units = "mins") > UserMins) {
            
            cat(paste0("[SQLite2df] INFO, timeAverage of Values_db on ", i), sep = "\n" )
            SelectedRows    <- which(Values_db$date >= i & Values_db$date < i + 86400) # taking interval of one day in seconds
            if (length(SelectedRows) > 0) { # avoid the error of timeAverage when there are 0 rows
                
                # timeAverage with UserMins
                # Using SelectByDate creates troubles with the class of i (Posix or string of charater), it creates a tibble
                # subset looks without problem but on line it is suggested not to use it
                # not using boardTimeStamp in the time average, in case of 2 shields or more the boardTimeStamp values may be completely independent and different
                # Adding boardTimesTamp afterwards
                #browser()
                Real.Sensors        <- colnames(Values_db)[!colnames(Values_db) %in% c("date","boardTimeStamp", "gpsTimestamp", "altitude", "latitude", "longitude")]
                SelectedColumns     <- colnames(Values_db)[which(colnames(Values_db)!= "boardTimeStamp")]
                SelectedRowsSensors <- as.numeric(row.names(Values_db[SelectedRows,])[as.vector(rowSums(!is.na(Values_db[SelectedRows, Real.Sensors])) >0)])
                if (length(SelectedRowsSensors) > 0 ) {
                    
                    Buffer <- timeAverage( # selectByDate(Values_db, 
                        #              start = lubridate::as_date(i), 
                        #              end   = lubridate::as_date(i), 
                        #              hour = 0:23
                        # ),
                        mydata     = Values_db[SelectedRowsSensors, SelectedColumns], # SelectedColumns, the -2 should be boardTimeStamp
                        avg.time   = paste0(toString(UserMins)," ","min"), 
                        statistic  = "mean", 
                        start.date = lubridate::floor_date(i,  unit = paste0(toString(UserMins)," ","min")), 
                        fill = FALSE
                    )
                    
                    # Adding the boardTimesStamp only of the chemical shield to avoid confusion with the boardTimeStamp of the other shield
                    # Selecting only the pollutants of asc.File to avoid to average with timeBoardStamps of other shield (OPC)
                    #if (as.POSIXct("2018-05-26 07:30:00",format= "%Y-%m-%d %H:%M:%S")<i) browser()
                    SelectedRowsStamp <- as.numeric(row.names(Values_db[SelectedRowsSensors,])[as.vector(rowSums(!is.na(Values_db[SelectedRowsSensors, asc.File$gas.sensor])) >0)])
                    if (length(SelectedRowsStamp) == 0) {
                        
                        SelectedRowsStamp <- as.numeric(row.names(Values_db[SelectedRowsSensors,])[as.vector(rowSums(is.na(Values_db[SelectedRowsSensors, asc.File$gas.sensor])) >0)])
                    } 
                    
                    boardTimeStamp    <- timeAverage( mydata     = Values_db[SelectedRowsStamp,c("date",'boardTimeStamp')], 
                                                      avg.time   = paste0(toString(UserMins)," ","min"), 
                                                      statistic  = "mean", 
                                                      start.date = lubridate::floor_date(i,  unit = paste0(toString(UserMins)," ","min")), 
                                                      fill = FALSE)
                    
                    # Merging Buffer and boardTimeStamp
                    Buffer <- merge(x = Buffer, y = boardTimeStamp, by = "date", all.x = TRUE )
                    rm(boardTimeStamp)
                    
                    # aggregating in Values_db_cast
                    if (exists("Values_db_Mins")) Values_db_Mins <- rbind.fill(Values_db_Mins, Buffer) else Values_db_Mins <- Buffer
                    rm(Buffer)
                }
            }
            i <- i + 86400
            
        }
        # we could use package data.table
    } else stop(cat(paste0("[SQLite2df] ERROR, UserMins is not set in ASEConfig_xx.R. Please set it, default 10 mins, the script is stopped."), sep = "\n"))
    
    #browser()
    # returning data if any
    if (exists("Values_db_Mins")) {
        if (nrow(Values_db_Mins)>0) {
            
            # adding the existing data before averaging, all rows except the last one that shall be recalculated with new values
            if (file.exists(DownloadSensor$Influx.Rdata.file) & Complete) { 
                
                load(DownloadSensor$Influx.Rdata.file)
                # avoiding the last common date beetween InfluxData and Values_db
                if (InfluxData[nrow(InfluxData),"date"] == Values_db[1,"date"])
                    Values_db_Mins <- rbind.fill(InfluxData[1:(nrow(InfluxData)-1),], Values_db_Mins) else Values_db <- rbind.fill(InfluxData, Values_db_Mins)
            }
            
            # We coud use data.table if we need speeding up
            #TimeStampDT         <- data.table(TimeStamp)
            #TimeStamp           <- as.data.frame(TimeStampDT)
            #TimeStampDT <- TimeStampDT[,.(time, gpsTimestamp = mean(gpsTimestamp, na.rm = TRUE), boardTimeStamp = mean(boardTimeStamp, na.rm = TRUE), 
            #                              latitude = mean(latitude, na.rm = TRUE),longitude = mean(longitude, na.rm = TRUE),altitude = mean(altitude, na.rm = TRUE)), by = .(time)]
            
            #=====================================================================================CR
            #=== 3. Ploting - Enter your time slot (From and To) - preferably only the new data
            #=====================================================================================CR
            # Select the time period you want to plot: only the sensor date added to InfluxData
            # in the future try to use timeSpan from lubridate TimeSpan <- as.POSIXct("2015-12-21 01:00:00") %--% as.POSIXct("2016-01-20 01:00:00")
            From <- min(Values_db_Mins$date, na.rm = TRUE)
            To   <- max(Values_db_Mins$date, na.rm = TRUE)
            
            cat("[SQLite2df] INFO, preparing time series to plot")
            # timePlot(selectByDate(Values_db_Mins, start = From, end = To), 
            #          pollutant = Channel.names$Variables, y.relation = "free", date.pad = TRUE, auto.text = FALSE)
            
            cat("[SQLite2df] INFO, returning dataframe with sensor data in column with 1st column named 'date'\n")
            print(str(Values_db_Mins))
            return(Values_db_Mins)
            
        } else {
            #S There is no Values_db_Mins
            cat("[SQLite2df] WARNING, no new INfluxDB data downloaded to be tabulated and averaged. Adding existing InFuxData.Rdata is existing.\n")
            
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            # adding the existing data before averaging, all rows except the last one that shall be recalculated with new values
            if (file.exists(DownloadSensor$Influx.Rdata.file) & Complete) { 
                load(DownloadSensor$Influx.Rdata.file)
                return(InfluxData)
            }
        }
    } else {
        #S There is no Values_db_Mins
        cat("[SQLite2df] WARNING, no new INfluxDB data downloaded to be tabulated and averaged. Adding existing InFuxData.Rdata is existing.\n")
        # adding the existing data before averaging, all rows except the last one that shall be recalculated with new values
        cat("-----------------------------------------------------------------------------------\n")
        cat("\n")
        if (file.exists(DownloadSensor$Influx.Rdata.file) & Complete) { 
            load(DownloadSensor$Influx.Rdata.file)
            return(InfluxData)
        }
    }
    #browser()
}

#=====================================================================================CR
# 161030 MG : Download Reference Data retrieving
#=====================================================================================CR
Down_Ref <- function(Reference.name, urlref, UserMins, DownloadSensor, AirsensWeb, naStrings = NULL, WDoutput = NULL, ref.tzone = "UTC", 
                     FTPMode = "ftp", Ref.SOS.name = NULL, RefSOSname = NULL, RefSOSDateIN = NULL, RefSOSDateEND = NULL,
                     csvFile = NULL, csvFile.sep = NULL, csvFile.quote = NULL, Old.Ref.Data = NULL, Coord.Ref = NULL, Ref.Type = "Ref") {
    
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
    # csvFile               = if FTPMode = "csv", file path to the csv file to load
    # csvFile.sep           = if FTPMode = "csv", separator between columns in the csvFile
    # csvFile.quote         = if FTPMode = "csv", separator of values in all columns
    # Old.Ref.Data          = dataframe with previously loaded reference data to be merged with currently loading dataframe reference data, default is null, NULL
    # Coord.Ref             = string with coordinates of reference data longitude and latitude separated by a blank
    # Ref.type              = label to be written in front of pollutatns names, defaut is Ref, other possibility Bine for PM distribution
    
    # return                = dataframe Ref with the data
    
    cat("\n")
    cat("-----------------------------------------------------------------------------------\n")
    cat(paste0("[Down_Ref] INFO, Reference Data retrieving for ", Reference.name), sep = "\n")
    
    #------------------------------------------------------------------------------CR
    # Checking internet connection availability
    #------------------------------------------------------------------------------CR
    if (havingIP()) {
        
        # cat(paste0("[Down_Ref] INFO; checking ping to ", urlref[1],"\n"))
        # if (PingThisSite(gsub('http://', '', urlref[1]))) {
        #     
        #     cat(paste0("[Down_Ref] INFO; ping to ", urlref[1], " Ok\n"))
        # } else{
        #     
        #     # return(cat(paste0("[Down_Influx] ERROR: you have internet connection but cant ping to ",Host,". InfluxDB download cannot be carried out."), sep = "\n"))
        #     cat(paste0("[Down_Ref] ERROR: the ftp site does not answer to a ping. Maybe for security reasons, the ping is unabled. It is likely that Reference data cannot be downloaded.\n"))
        # } 
    } else {
        return(cat(paste0("[Down_Ref] ERROR: there is no internet connection. The Reference data cannot be downloaded.\n")))
    }
    
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
                                   Ref.NO2     = c("NO2"  , "Nitrogen dioxide (air)", "Ref.NO2"),
                                   Ref.SO2     = c("SO2"  , "Sulfur dioxide (air)", "Ref.SO2"),
                                   Ref.O3      = c("O3"   , "Ozone (air)", "Ref.O3"),
                                   Ref.NO      = c("NO"   , "Nitrogen monoxide (air)", "Ref.NO")  ,
                                   Ref.PM10    = c("PM10" , "Particulate matter < 10 Âµm (aerosol)", "Ref.PM10"),
                                   Ref.PM2.5   = c("PM2.5", "Particulate matter < 2.5 Âµm (aerosol)", "Ref.PM2.5"),
                                   Ref.CO_ppm  = c("CO"   , "Carbon monoxide (air)", "co", "Ref.CO_ppm", "CO_ppm")) 
    
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
                        if (!is.null(Coord.Ref)) {
                            
                            # taking coordinates from Coord.Ref
                            long <- as.numeric(unlist(strsplit(x = Coord.Ref, split = " "))[1])
                            lat  <- as.numeric(unlist(strsplit(x = Coord.Ref, split = " "))[2])
                            Reference.i$Ref.Long <- long
                            Reference.i$Ref.Lat  <- lat
                            
                        }
                        
                        #browser()
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
                    # browser()
                    # if you load a .csv file:
                    if (grepl(".csv", csvFile, fixed =T)) {
                    
                    Reference.i <- read.csv(file        = csvFile,
                                            header      = TRUE, 
                                            na.strings  = naStrings,
                                            sep         = csvFile.sep,
                                            quote       = csvFile.quote,
                                            check.names = FALSE,
                                            stringsAsFactors = FALSE)
                    }
                    
                    # if you load a .Rdata file:
                    if (grepl(".Rdata", csvFile, fixed=T)) {
                        
                        # loaded Rdata with unknown name dataframe
                        load(file = csvFile)
                        
                        # name of loaded dataframe
                        Reference.i <- load(csvFile)
                        Reference.i <- get(Reference.i)
                        
                        # removing un-necessary columns of Reference.i
                        # possible names 
                        all.names <- character(0)
                        for (i in seq_along(Reference.names)) all.names <- c(all.names, unlist(Reference.names[[i]]))
                        Reference.i <- Reference.i[,which(names(gas.RefData) %in% all.names)]
                        
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
                        if (!is.null(Coord.Ref)) {
                            
                            # taking coordinates from Coord.Ref
                            long <- as.numeric(unlist(strsplit(x = Coord.Ref, split = " "))[1])
                            lat  <- as.numeric(unlist(strsplit(x = Coord.Ref, split = " "))[2])
                            Reference.i$Ref.Long <- long
                            Reference.i$Ref.Lat  <- lat
                            
                        }
                        
                        #browser()
                        # checking if we have a date column in the referenceValues()
                        if (any(grepl(pattern = paste0(c("date","time","Date", "Time", "DATE", "TIME", "DateTime"),
                                                       collapse = "|"), 
                                      x       = colnames(Reference.i)
                                ))) {
                            
                            # checking if there is more than 1 field with "date","time","Date", "Time", "DATE", "TIME"
                            if (length(which(grepl(pattern = paste0(c("date","time","Date", "Time", "DATE", "TIME", "DateTime"), collapse = "|"), 
                                                  x        = colnames(Reference.i))
                                             )
                                       ) == 1 ) {
                                
                                # setting name of timedate column as "date" for openair
                                names(Reference.i)[grep(pattern = paste0(c("date","time","Date", "Time", "DATE", "TIME", "DateTime"), collapse = "|"), 
                                                        x       = colnames(Reference.i))] <- "date" 
                                
                                # convert date to POSIX with time zone set in shiny
                                if(!("POSIXct" %in% class(Reference.i$date))) Reference.i$date <- as.POSIXct(Reference.i$date,  tz = ref.tzone, tryFormats = c("%Y-%m-%d %H:%M:%OS",
                                                                                                                 "%Y/%m/%d %H:%M:%OS",
                                                                                                                 "%Y-%m-%d %H:%M:%S",
                                                                                                                 "%Y/%m/%d %H:%M",
                                                                                                                 "%Y-%m-%d",
                                                                                                                 "%Y/%m/%d",
                                                                                                                 "%m/%d/%Y %H:%M")) # "%m/%d/%Y %H:%M", strptime removed with the format as this may cause a crash, but slower operation
                                
                                # Convert all other columns to numeric if they are not excepts coordinates
                                for (j in names(Reference.i)[-which(names(Reference.i) %in% c("date", "Ref.Long", "Ref.Lat"))]) if (class(Reference.i[,j]) != "numeric") Reference.i[,j] <- as.numeric(Reference.i[,j])
                                
                                # aggregate Reference.i with mean over UserMins
                                # use openair function to aggregate to the selected time average, in openair time must be replaced in date
                                # browser()
                                Reference.i <- data.frame(openair::timeAverage(Reference.i, 
                                                                      avg.time = paste0(toString(UserMins)," ","min"), 
                                                                      interval = paste0(toString(UserMins)," ","min"), 
                                                                      statistic = "mean", 
                                                                      start.date = round(min(Reference.i$date), units = "hours"), 
                                                                      fill = TRUE),
                                                          check.names = FALSE
                                )
                                #browser()
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
                                } else if (Ref.Type %in% c("Bin.DMPS", "Bin.APS")) {
                                    
                                    # Adding lable to pollutants which are not in Reference.names
                                    names.not.Ref <- names(Reference.i)[grep(pattern = paste(c("date","Ref.", "Bin.DMPS.", "Bin.APS."), collapse = "|"), x = names(Reference.i), invert = TRUE)]
                                    names(Reference.i)[which(names(Reference.i) %in% names.not.Ref)] <- sapply(seq_along(names.not.Ref), function(k) paste0(Ref.Type, ".", names.not.Ref[k]))
                                }
                          
                                # matching dates, MG changed using merge
                                # set DateIN for data retrieving, either from initial date or last date in previous DataFrame
                                if ( !any(names(Reference.i)[-which(names(Reference.i) %in% c("date", "Ref.Long", "Ref.Lat"))] %in% DownloadSensor$Var.Ref.prev[-which(names(Reference.i) %in% c("date", "Ref.Long", "Ref.Lat"))]))  {
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
            
            #browser()
            #------------------------------------------------------------------------------CR
            # Sensor Data retrieving at apiEndpoint
            #------------------------------------------------------------------------------CR
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
            #browser()
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
            
        }
    } 
    
    # removing NAs to Ref to avoid to add empty lines that will not be updated later
    #browser()
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
            cat("[Down_ref] INFO, there is no new data for the reference station",sep= "\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            return()  
        } 
    } else {
        
        # removing Ref when there are no sensor data
        remove(Ref)
        cat("[Down_ref] ERROR, there is no new data for the reference station",sep= "\n")
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
    
    #browser()
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
Validation.tool <- function(  General, DateIN, DateEND, DateINCal = NULL, DateENDCal = NULL, name.gas, model.log, nameGasRef, nameGasVolt, nameGasMod, 
                              unit.ref, unit.sensor, Sens.raw.unit = NULL, Reference.name, AirsensEur.name, name.sensor, 
                              timeseries.display, DateINPlot = DateIN, DateENDPlot = DateEND, 
                              WDoutputMod, WDoutput, WDoutputStats, 
                              process.step, mod.eta.model.type, Multi.File = NULL, 
                              eta.model.type, remove.neg = TRUE, Covariates = NULL, PlotCal = TRUE) {
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
    
    #browser()
    if (model.log) {
        # timeplots uncalibrated values
        if (timeseries.display) {
            Relationships         <- na.omit(colnames(General)[colnames(General) %in% Covariates_i])
            if (!is.null(Relationships)) 
                timePlot(mydata = General[ General$date >= DateINPlot & General$date <= DateENDPlot,], pollutant = Relationships, date.pad = TRUE, auto.text = FALSE, y.relation = "free",
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
            y = General[,nameGasRef ][General$date >= DateIN & General$date <= DateEND]
            x = General[,nameGasVolt][General$date >= DateIN & General$date <= DateEND]
            AxisLabelY = paste0(nameGasRef," ", unit.ref, " ",Reference.name)
            AxisLabelX = paste0(nameGasVolt," in ", Sens.raw.unit," ", AirsensEur.name)
        } else {
            x = General[,nameGasRef ][General$date >= DateIN & General$date <= DateEND]
            y = General[,nameGasVolt][General$date >= DateIN & General$date <= DateEND]
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
            Matrice         <- data.frame(General[General$date >= DateINPlot & General$date <= DateENDPlot, Covariates])
            names(Matrice)  <- Covariates
            
            if (!is.null(Multi.File)) {
                
                if (file.exists(Multi.File)) {
                    
                    # read Multi.File
                    Multi.File.df <-  read.table(file             = Multi.File, 
                                                 header           = TRUE, 
                                                 row.names        = NULL, 
                                                 comment.char     = "#"
                                                 # ,stringsAsFactors = FALSE
                    )
                    
                    # add covariate degrees of polynomial
                    Degrees <-  Multi.File.df[Multi.File.df$Covariates == Covariates, "degree"]
                } else {
                    
                    # degree of polynomial set to 1 
                    Degrees <-  base::rep(1, times = length(Covariates) )
                } 
            } else {
                
                # degree of polynomial set to 1 
                Degrees <-  base::rep(1, times = length(Covariates) )
            } 
            
            #browser()
            namesCovariates <- paste0(paste(Covariates,Degrees, sep = "-"),collapse = "&")
            
        } else if (any(mod.eta.model.type %in% c("exp_kT", "exp_kK", "T_power", "K_power"))) {
            namesCovariates <- "Temperature"
            Matrice         <- data.frame(General[General$date >= DateINPlot & General$date <= DateENDPlot, namesCovariates])
            names(Matrice)  <- namesCovariates
        } else {
            namesCovariates <- ""
            Matrice         <- NULL
        } 
        
        #browser()
        Model.i <- Cal_Line(x = x, s_x = NULL, 
                            y = y, s_y = NULL, 
                            Mod_type      = mod.eta.model.type,
                            Multi.File    = Multi.File,
                            Covariates    = Covariates,
                            Matrice       = Matrice, 
                            line_position = 0, 
                            Couleur       = "red", 
                            Sensor_name   = name.sensor, 
                            f_coef1 = "%.3e", f_coef2 = "%.3e", f_R2 = "%.4f", 
                            lim = EtalLim, 
                            marges = NULL,
                            Weighted = FALSE,
                            Lag_interval = sqrt((max(x, na.rm = T) - min(x, na.rm = T))) / (length(Covariates) + 1))
        
        # saving the model
        nameModel  <- paste(AirsensEur.name,name.sensor,Sens.raw.unit,mod.eta.model.type,format(DateIN,"%Y%m%d"),format(DateEND,"%Y%m%d"),namesCovariates, sep = "__")
        assign(nameModel, Model.i)
        # https://stackoverflow.com/questions/42230920/saverds-inflating-size-of-object
        # It turns out that the lm.object$terms component includes an environment component that references to the objects present in the global environment 
        # when the model was built. Under certain circumstances, when you saveRDS R will try and draw in the environmental objects into the save object.
        #rm(list=ls(envir = attr(Model.i$formula, ".Environment")), envir = attr(Model.i$formula, ".Environment")) 
        #rm(list=ls(envir = attr(Model.i$terms  , ".Environment")), envir = attr(Model.i$terms  , ".Environment")) 
        #rm(list=ls(envir = attr(Model.i$model  , ".Environment")), envir = attr(Model.i$model  , ".Environment")) 
        # for (i in names(Model.i)) {
        #     cat(paste0("[ValidationTool] INFO, the size of the calibration model (",i,") is: ", object.size(Model.i[[i]]),"\n")) 
        #     saveRDS(object = Model.i[[i]], 
        #             file   = file.path(WDoutputMod, paste0(nameModel,".rds"))
        #     )
        # } 
        # for (i in c("formula","terms","model")) { # "m", "ConvInfo", "control"
        #     if (any(i %in% names(Model.i))) {
        #         
        #         # keeping list model for multivaratie models
        #         if (!(i == "model" & mod.eta.model.type %in% c("MultiLinear","exp_kT", "exp_kK", "T_power", "K_power"))) {
        #             rm(list = ls(envir = attr(Model.i[[i]], ".Environment")), 
        #                envir = attr(Model.i[[i]], ".Environment"))
        #             #Model.i <- Model.i[-which(names(Model.i) == i)]
        #         } 
        #     }
        # }
        #browser()
        Model.i <- list(Tidy = tidy(Model.i), Augment = augment(Model.i), Glance = glance(Model.i), Call = Model.i$call, Coef = coef(Model.i))
        list.save(Model.i, file = file.path(WDoutputMod, paste0(nameModel,".rdata")))
        # saveRDS(object = Model.i, 
        #         file   = file.path(WDoutputMod, paste0(nameModel,".rds"))
        # )
        
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
        General[,nameGasMod ] <- NULL # General[ (General$date >= DateIN & General$date <= DateEND ),nameGasMod ] <-NA
        
        if (mod.eta.model.type == "Linear"| mod.eta.model.type == "Linear.Robust" ) {
            General[ General$date >= DateIN  & General$date <= DateEND & !is.na(General[, nameGasVolt]), nameGasMod] <-
                (General[ General$date >= DateIN  & General$date <= DateEND & !is.na(General[, nameGasVolt]), nameGasVolt]- coef(Model.i)[1]) /  coef(Model.i)[2]
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
                while(identical(ListJAVA, character(0)) & getwd() != dirCurrentParent) {
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
    
    #browser()
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
    
    print(sens2ref, quote = FALSE)
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
INFLUXDB <- function(WDoutput,DownloadSensor,UserMins,
                     PROXY,URL, PORT, LOGIN, PASSWORD,
                     Down.Influx, Host, Port, User ,Pass, name.SQLite, name.SQLite.old, Db, Dataset, Influx.TZ = NULL,
                     sens2ref, asc.File=NULL) {
    
    # Parameters PROXY:  PROXY, URL, PORT, LOGIN, PASSWORD
    # Parameters Influx: Down.Influx, Host, Port, User, Pass, name.SQLite, name.SQLite.old, Db, Dataset, Influx.TZ
    # Sqlite database  : name.SQLite,name.SQLite.old
    # Configuration sensors: sens2ref
    # InfluxDB data
    # asc.File          : dataframe, default is NULL, used for giving the correct name of the sensor
    
    #browser()
    cat("-----------------------------------------------------------------------------------\n")
    cat("[INFLUXDB] INFO: Downloading InfluxDB data\n")
    # Saving Influx Sensor data
    Influx.Rdata.file  = file.path(WDoutput, "InfluxData.Rdata")
    Influx.csv.file    = file.path(WDoutput, "InfluxData.csv"  )
    if (DownloadSensor$Retrieve.data.Influx) {
        
        if (Down.Influx) {
            
            # downloading data from InfluxDB and updating airsenseur.db
            Influx.TZ <- Down_Influx(PROXY = PROXY, URL = URL  , PORT = as.numeric(PORT), LOGIN = LOGIN, PASSWORD = PASSWORD,
                                     Host = Host  , User = User, Port = as.numeric(Port), Pass = Pass, name.SQLite = name.SQLite, name.SQLite.old = name.SQLite.old,
                                     Db = Db      , Dataset = Dataset, Influx.TZ = Influx.TZ, use_google = FALSE, Page = 10000, Mean = as.numeric(UserMins)) 
            # if there are problems accessing port 443 for the google api to determine time zone add , use_google = FALSE
            # Sqlite2df returns only the new data from the AirSensEUR.db, if the whole set is needed add: Complete = TRUE in function Down_Influx
            #browser()
            InfluxData <- Sqlite2df(name.SQLite = name.SQLite, Dataset = Dataset, Influx.TZ = Influx.TZ, UserMins = UserMins, DownloadSensor = DownloadSensor, asc.File = asc.File)
            
            var.names.meteo <-c("Temperature","Relative_humidity",  "Atmospheric_pressure")
            if (!is.null(InfluxData) && is.data.frame(InfluxData)) {
                # setting the name of sensors
                if (exists("InfluxData")) {
                    # List of Pollutant/sensor installed in the AirSensEUR
                    var.names.sens <- colnames(InfluxData)[-grep(pattern = paste0(c("date","_raw","gpsTimestamp","boardTimeStamp",  "channel", "latitude", "longitude", "altitude"),collapse = "|"), x = colnames(InfluxData))]
                    # 
                    if (length(var.names.sens) == 0) {
                        stop(paste0("[INFLUXDB] ERROR: no sensor variable downloaded for ", Dataset," InFluxDB. Please check in the INfluxDB client -> STOP"))
                    } else cat(paste0("[INFLUXDB] INFO: Sensor variables existing in airsenseur.db: ", paste0(var.names.sens, collapse = ", "), ", with date timestamp and coordinates."), sep = "\n")
                    #
                    # Setting the Sensor names
                    var.name.GasSensors      <- var.names.sens[-(which(var.names.sens %in% var.names.meteo))]
                } else { # if we do not have new data for sensors we use the names of sensors in sens2ref
                    var.name.GasSensors <- na.omit(sens2ref$gas.sensor)
                    var.names.sens      <- c(var.name.GasSensors, var.names.meteo)
                }
                InfluxDataNew <- InfluxData
            } else {
                remove(InfluxData) # removing  influxData if empty  
            } 
            
            if (file.exists(DownloadSensor$Influx.Rdata.file)) {
                # loading the existing data in InfluxData
                load(DownloadSensor$Influx.Rdata.file) 
                var.names.meteo <-c("Temperature","Relative_humidity",  "Atmospheric_pressure")
                var.name.GasSensors <- na.omit(sens2ref$gas.sensor)
                var.names.sens      <- c(var.name.GasSensors, var.names.meteo)
                # merging old and new data
                if (exists("InfluxDataNew")) {
                    InfluxData <- rbind.fill(InfluxData,InfluxDataNew)
                    rm(InfluxDataNew)    
                } 
            } 
            write.csv(x = InfluxData, file = Influx.csv.file)
            save(InfluxData, file = Influx.Rdata.file)
            cat(paste0("[INFLUXDB] INFO: Influx Sensor data saved in ", Influx.Rdata.file, " & ", Influx.csv.file,". Updating copies in .old files."), sep = "\n")
            Make.Old(File = Influx.Rdata.file)
            Make.Old(File = Influx.csv.file)
            
        } else { # Trying to use the existing Influx.Rdata.file
            
            if (file.exists(file.path(Influx.Rdata.file))) {
                cat(paste0("[INFLUXDB] INFO: Down.Influx set to FALSE in ASEConfig.R  (no request of sensor data download from InfluxDB). Using previously saved Influx.Rdata.file ."), sep = "\n")
                load(file.path(Influx.Rdata.file))
                var.names.meteo <-c("Temperature","Relative_humidity",  "Atmospheric_pressure")
                var.name.GasSensors <- na.omit(sens2ref$gas.sensor)
                var.names.sens      <- c(var.name.GasSensors, var.names.meteo)
            } else {
                cat(paste0("[INFLUXDB] INFO: there is no previously saved Influx.Rdata.file. Missing InfluxData and Down.Influx request of sensor data download set to FALSE in ASEConfig.R ."), sep = "\n")
            }
        } 
    } else {
        if (file.exists(file.path(Influx.Rdata.file))) {
            load(file.path(Influx.Rdata.file))
            var.names.meteo <-c("Temperature","Relative_humidity",  "Atmospheric_pressure")
            var.name.GasSensors <- na.omit(sens2ref$gas.sensor)
            var.names.sens      <- c(var.name.GasSensors, var.names.meteo)
        } else {
            cat(paste0("[INFLUXDB] INFO: sensor data download from InfluxDB already updated, DownloadSensor$Retrieve.data.Influx set to FALSE"), sep = "\n")
        }
    }
    
    if (exists("InfluxData")) {
        
        cat("[INFLUXDB] INFO INFLUXDB returning list with InfluxData, var.names.meteo, var.name.GasSensors and var.names.sens\n")
        return(list(InfluxData, var.names.meteo, var.name.GasSensors, var.names.sens)) 
        
    } else return(cat("[INFLUXDB] ERROR no Influx data available\n"))
    
    cat("-----------------------------------------------------------------------------------\n")
    cat("[INFLUXDB] INFO: Downloading InfluxDB data\n")
    
}

#=====================================================================================CR
# 170721 MG : Downloading SOS data
#=====================================================================================CR
SOS      <- function(WDoutput, DownloadSensor, Down.SOS, AirsensEur.name, UserMins, AirsensWeb, Duration = 1, sens2ref) {
    # Parameters SOS: Down.SOS,AirsensEur.name,UserMins,AirsensWeb,Duration
    # Sqlite database  : name.SQLite,name.SQLite.old
    # Configuration sensors: sens2ref
    
    # SOSData
    cat("-----------------------------------------------------------------------------------\n")
    cat("[SOS] INFO: Downloading SOS data\n")
    SOS.Rdata.file  <- file.path(WDoutput, "SOSData.Rdata")
    SOS.csv.file    <- file.path(WDoutput, "SOSData.csv"  )
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
                    # 
                    if (length(var.names.sens) == 0) {
                        stop(paste0("[SOS] ERROR: no sensor variable downloaded for ",AirsensEur.name," at the apiEndPoint. Please check in SOS client -> STOP"))
                    } else cat(paste0("[SOS] INFO: Sensor variables existing in the dataframe downloaded at the apiEndPoint: ", paste0(var.names.sens, collapse = ", "), ", plus date added"), sep = "\n")
                    #
                    # Setting the Sensor names
                    var.name.GasSensors      <- var.names.sens[-(which(var.names.sens %in% var.names.meteo))]
                } else {
                    # if we do not have new data for sensors we use the names of sensors in sens2ref
                    var.name.GasSensors <- na.omit(sens2ref$gas.sensor)
                    var.names.sens      <- c(var.name.GasSensors, var.names.meteo)
                }
                
                # Saving Sensor data - It is alredy saved, it is not needed to save again, just use Make.old
                # No need to save SOS.Rdata.file and SOS.csv.file, as it is already done in Down_SOS
                #save(SOSData, file = SOS.Rdata.file)
                #write.csv(SOSData, file = SOS.csv.file)
                cat(paste0("[SOS] INFO: Influx Sensor data saved in ", SOS.Rdata.file, " & ", SOS.csv.file,". Updating copies in .old files."), sep = "\n")
                Make.Old(File = SOS.Rdata.file)
                Make.Old(File = SOS.csv.file)
                
            } else {
                
                # no request Down.SOS. Trying to use the existing Influx.Rdata.file
                
                if (file.exists(file.path(SOS.Rdata.file))) {
                    cat(paste0("[SOS] INFO: Down.SOS set to FALSE in ASEConfig_xx.R  (no request of sensor data download from SOS). Using previously saved  SOS.Rdata.file ."), sep = "\n")
                    load(file.path(SOS.Rdata.file))
                    var.names.meteo     <- c("Temperature","Relative_humidity",  "Atmospheric_pressure")
                    var.name.GasSensors <- na.omit(sens2ref$gas.sensor)
                    var.names.sens      <- c(var.name.GasSensors, var.names.meteo)
                } else {
                    cat(paste0("[SOS] INFO: Down.SOS set to FALSE in ASEConfig_xx.R  (no request of sensor data download from SOS). There is no previously saved SOS.Rdata.file.",
                               "Missing SOSData and Down.SOS request of sensor data download set to FALSE in ASEConfig_xx.R .\n"))
                }
            }    
        }
        
    } else {
        
        if (file.exists(file.path(SOS.Rdata.file))) {
            load(file.path(SOS.Rdata.file))
            var.names.meteo     <- c("Temperature","Relative_humidity",  "Atmospheric_pressure")
            var.name.GasSensors <- na.omit(sens2ref$gas.sensor)
            var.names.sens      <- c(var.name.GasSensors, var.names.meteo)
        } else {
            cat(paste0("[SOS] INFO: sensor data download from SOS already updated (DownloadSensor$Retrieve.data.SOS set to FALSE)"), sep = "\n")
        }
    } 
    
    if (exists("SOSData")) {
        cat("[SOS] INFO SOS returning list with SOSData, var.names.meteo, var.name.GasSensors and var.names.sens\n")
        return(list(SOSData, var.names.meteo, var.name.GasSensors, var.names.sens)) 
    } else return(cat("[SOS] INFO no SOS data available\n"))
    
}

#=====================================================================================CR
# 170721 MG : Downloading REFERENCE data
#=====================================================================================CR
REF      <- function(DownloadSensor, AirsensEur.name, DisqueFieldtestDir, UserMins, 
                     Down.Ref, ref.tzone, InfluxData, SOSData, Reference.name, urlref,sens2ref, 
                     FTPMode = "ftp", Ref.SOS.name = NULL, RefSOSname = NULL, RefSOSDateIN = NULL, RefSOSDateEND = NULL,
                     csvFile = NULL, csvFile.sep = NULL, csvFile.quote = NULL, Coord.Ref = NULL,
                     Ref.Type = "Ref") {
    # DownloadSensor        = Output of function DownloadSensor
    # Down.ref              = logical, if true reference data are downloaded
    # ref.tzone             = string, refernce time name of the reference data. Default = "UTC"
    # FTPMode               = string, default = "ftp", type of download of reference data: "ftp" using a csv file on a ftp server, "csv" the same with a local file and SOS: SOS download
    # Ref.SOS.name          = SOS ID of the Reference station
    # RefSOSname            = Reference station SOS Rest API URL
    # RefSOSDateIN          = Starting  date for downloading Reference data using SOS
    # RefSOSDateEND         = Ending date for downloading Reference data using SOS
    # csvFile               = if FTPMode = "csv", file path to the csv file to load
    # csvFile.sep           = if FTPMode = "csv", separator between columns in the csvFile
    # csvFile.quote         = if FTPMode = "csv", separator of values in all columns
    # Coord.Ref             = string with coordinates of reference data longitude and latitude separated by a blank
    # Ref.type              = label to be written in front of pollutatns names, defaut is Ref, other possibility Bine for PM distribution
    
    #------------------------------------------------------------------------------CR
    # Downloading Reference data, Only new values, save RefData.RData and refData.csv with all reference values
    #------------------------------------------------------------------------------CR
    # Getting what is the first date in InfluxData and or SOSData and setting in DownloadSensor
    # browser()
    cat("-----------------------------------------------------------------------------------\n")
    cat("[REF] INFO: Downloading Reference data, save RefData.RData and refData.csv with all reference values in directory General_Data\n")
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
    
    if (DownloadSensor$Retrieve.data.Ref & Down.Ref) {
        
        cat("-----------------------------------------------------------------------------------\n")
        cat(paste0("[REF] INFO, Starting downloading data for ", Reference.name, sep = "\n"))
        RefData  <- Down_Ref(Reference.name = Reference.name, UserMins = UserMins, DownloadSensor = DownloadSensor, urlref = urlref, ref.tzone = ref.tzone
                             , naString = c("-999.99", "-999.98999", NaN, NA), WDoutput = file.path(DisqueFieldtestDir, "General_data")
                             , FTPMode = FTPMode, Ref.SOS.name = Ref.SOS.name, RefSOSname = RefSOSname, RefSOSDateIN = RefSOSDateIN, RefSOSDateEND = RefSOSDateEND
                             , csvFile = csvFile, csvFile.sep = csvFile.sep, csvFile.quote = csvFile.quote, Coord.Ref = trimws(x = Coord.Ref), Ref.Type = Ref.Type) # this return only new Data
            
        # setting the name of sensors
        if (exists("RefData") && !is.null(RefData)) {
            
            # Checking that there are pollutants and not only dates
            if (!identical(colnames(RefData)[-which(colnames(RefData) %in% c("date", "Ref.Long", "Ref.Lat"))], character(0))) {
                
                # List of Pollutants monitored at the Referencce stations
                var.names.ref <- colnames(RefData)[-which(colnames(RefData) %in% c("date", "Ref.Long", "Ref.Lat"))]
                
                # Checking that excepted dates, there are not only coordinates
                if (length(var.names.ref) == 0) {
                    
                    cat(paste0("[REF] ERROR no reference data downloaded for ",AirsensEur.name," .Please check reference data of ", Reference.name))
                    
                } else cat(paste0("[REF] INFO, Variables found in the reference dataset: ", paste0(var.names.ref, collapse = ", ")), sep = "\n")
            } else {
                # if we do not have new data for sensors we use the names of sensors in sens2ref
                var.names.ref <- na.omit(sens2ref$gas.reference2use)
            }
            
            #Preparing for appending the newly downloaded data
            RefDataNew <- RefData
            remove(RefData)
            
            # removing NAs to RefData to avoid to add empty lines that will not be updated later
            # Full.Nas <- which(apply(RefDataNew[,var.names.ref], 1, function(x) all(is.na(x))))
            # if (exists("RefDataNew")) {
            #     Full.Nas <- which(
            #         apply(as.matrix(RefDataNew[,-which(names(RefDataNew) %in% c("date", "Ref.Long", "Ref.Lat"))],
            #                         ncol = length(names(RefDataNew[,-which(names(RefDataNew)%in% c("date", "Ref.Long", "Ref.Lat"))]))),
            #               MARGIN = 1,
            #               function(x) all(is.na(x))
            #         )
            #     )
            # }
            
            # if (length(Full.Nas) > 0) RefDataNew <- unique(RefDataNew[-All.Nas,])
        } 
    }
    
    # List of Pollutants
    # loading the possible existing data in Refdata
    if (file.exists(DownloadSensor$Ref.Rdata.file)) {
        
        load(DownloadSensor$Ref.Rdata.file)
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
    # merging old and new data, or taking only the new data
    # browser()
     if (exists("RefDataNew") && exists("RefData")) {
        # browser()
        # RefData <- merge(x=RefData, y = RefDataNew, by = c("date", "Ref.Long", "Ref.Lat"), all = TRUE)  
          RefData <- merge(x=RefData, y = RefDataNew, by = c("date"), all = TRUE)  
     # if (!any(RefData$date %in% RefDataNew$date)) {
     #     RefData <- merge(x=RefData, y = RefDataNew, by = c("date", "Ref.Long", "Ref.Lat"), all = TRUE)
     # }
        #   
        #     RefData <- rbindlist(list(RefData,RefDataNew), use.names = TRUE, fill = TRUE) 
        #     
        # } else {
        #     RefData <- merge(x=RefData, y = RefDataNew, by = c("date", "Ref.Long", "Ref.Lat"), all = TRUE)
        # 
        # }
                
        # List of Pollutants monitored at the Referencce stations
        var.names.ref <- colnames(RefData)[-which(colnames(RefData) %in% c("date", "Ref.Long", "Ref.Lat"))]
        
    } else if (exists("RefDataNew")) {
        
        RefData <- RefDataNew  
    } 
    
    # Saving reference data if new data exists
    if ((exists("RefDataNew") && is.data.frame(RefDataNew)) | !file.exists(DownloadSensor$Ref.Rdata.file)) {
        
        WDoutput        = file.path(DisqueFieldtestDir, "General_data")
        Ref.Rdata.file  = DownloadSensor$Ref.Rdata.file                 # file.path(WDoutput, "RefData.Rdata")
        Ref.csv.file    = file.path(WDoutput, "RefData.csv"  )
        
        write.csv(x = RefData, file = Ref.csv.file)
        save(RefData, file = Ref.Rdata.file)
        #Make.Old(File = Ref.Rdata.file)
        #Make.Old(File = Ref.csv.file)
        cat(paste0("[REF] INFO, Reference data for ",Reference.name," saved in ", Ref.Rdata.file, " & ", Ref.csv.file,".Rdata. backup copies in .old files."), sep = "\n")
    } else cat(paste0("[REF] WARNING, There is no new reference data for ",Reference.name,"\n"))
    
    #rm(RefDataNew)
    if (exists("RefData") & exists("var.names.ref") & exists("DownloadSensor")) {
        
        cat("[REF] INFO, returning list with RefData, var.names.ref and DownloadSensor\n")
        return(list(RefData, var.names.ref, DownloadSensor)) 
    } else return(cat("[REF] ERROR no Reference data available\n"))
    
}

#=====================================================================================CR
# 170721 MG : Merging InfluxData or SOSData and RefData 
#=====================================================================================CR
GENERAL  <- function(WDoutput, UserMins, RefData, InfluxData, SOSData, Delay, var.name.GasSensors, DownloadSensor, Change.Delay = FALSE, Change.UserMins = FALSE) {
    # input:
    #       WDoutput,
    #       UserMins,
    #       RefData,
    #       InfluxData,
    #       SOSData, 
    #       Delay, 
    #       var.name.GasSensors
    #       DownloadSensor     : list output of Check_Downalod, use to know if DateEND.General.prev is before last date in INfluxData or SOSData
    #       Change.Delay       : logical, default False, TRUE if Delay has been changed and General shall be created new
    #       Change.UserMins    : logical, default False, TRUE if UserMins has been changed and General shall be created new
    #       
    # Return the dataFrame General, adding new data to the existing one and averaging every UserMins minutes
    
    #------------------------------------------------------------------------------CR
    # Merging InfluxData or SOSData and RefData 
    #------------------------------------------------------------------------------CR
    # - using preferably the latest date of the previous general dataset from influxdb, otherwise SOS data
    
    cat("-----------------------------------------------------------------------------------\n")
    cat("[GENERAL] INFO, Checking if there are more data in InfluxData or SOSData than in General.Rdata\n")
    #browser()
    # Checking if there are sensor data to be added to General 
    # if General.Rdata does not exist it must be created in all cases. The same if the Delay has changed 
    if (is.null(DownloadSensor$DateEND.General.prev) || Change.Delay || Change.UserMins) {
        
        is.sensorData <- TRUE  
        
    } else {
        
        # Checking last date in InfluxData 
        if (exists("InfluxData")) {
            
            if (!is.null(InfluxData)) {
                
                is.sensorData <- !all(is.na(InfluxData[which(InfluxData$date > DownloadSensor$DateEND.General.prev),-which(colnames(InfluxData) == "date")])) 
            }  
        } 
        
        # Checking last date in SOSData
        if (exists("SOSData")) {
            
            if (!is.null(SOSData)) {
                
                # Max.Sensor.date <- max(SOSData[all(is.na(SOSData[SOSData$date > DownloadSensor$DateEND.General.prev,-which(colnames(SOSData) == "date")])),"date"], na.rm = T)
                if (exists("is.sensorData")) {
                    
                    is.sensorData <- is.sensorData || !all(is.na(SOSData[which(SOSData$date > DownloadSensor$DateEND.General.prev),-which(colnames(SOSData) == "date")]))
                } else is.sensorData <- !all(is.na(SOSData[which(SOSData$date > DownloadSensor$DateEND.General.prev),-which(colnames(SOSData) == "date")]))
            }   
        } 
        
        # Checking last date in RefData
        if (exists("RefData")) {
            
            if (!is.null(RefData)) {
                
                # Max.Sensor.date <- max(RefData[all(is.na(RefData[RefData$date > DownloadSensor$DateEND.General.prev,-which(colnames(RefData) == "date")])),"date"], na.rm = T)
                if (exists("is.sensorData")) {
                    
                    is.sensorData <- is.sensorData || !all(is.na(RefData[which(RefData$date > DownloadSensor$DateEND.General.prev),-which(colnames(RefData) == "date")]))
                } else is.sensorData <- !all(is.na(RefData[which(RefData$date > DownloadSensor$DateEND.General.prev),-which(colnames(RefData) == "date")]))
            }   
        } 
    }
    
    #browser()
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
                if (exists("SOSData") & !(is.null(SOSData))) { # RefData, InfluxData and SOSData exists
                    
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
                    
                } else {  # RefData exists InfluxData present but no SOSData
                    
                    # In case of names with "_raw", the digital values in raw form are not saved in Genera.data, they are only kept in the airsenseur.db if Down_Influx is used
                    if (any(grepl(pattern = "_raw", x = colnames(InfluxData)))) {
                        
                        General <- merge(x = InfluxData[-grep(pattern = "_raw", x = colnames(InfluxData)),], Y = RefData, by = "date", all = TRUE )
                        
                    } else General <- merge(x = InfluxData, y = RefData, by = "date", all = TRUE )
                    
                    # Keeping only data with values in InfluxDB
                    General <- General[General$date >= min(InfluxData$date, na.rm = TRUE) & General$date <= max(InfluxData$date, na.rm = TRUE),]
                }
                
                cat("[GENERAL] INFO General data frame, there are new sensor data and new reference data \n")
                
                
            } else { # RefData exists but no Influx Data
                
                if (exists("SOSData")) { # RefData and SOSData exist but no Influx Data
                    
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
                    
                    # if InfluxData does not exist, we keep all data of SOSData
                    General <- merge(x = SOSData, y = RefData, by = "date", all = TRUE ) 
                    # Discarding Reference dates in GEeneral, keeping only dates within SOSData$date
                    General <- General[General$date >= min(SOSData$date) & General$date <= max(SOSData$date),]
                    # For ASEConfig02, if SOSData and InfluxData give  consecutive data
                    if (ASEConfig == "ASEconfig_02.R") {
                        General <- merge(x = rbind.fill(SOSData, InfluxData)[rbind.fill(InfluxData,SOSData)$date>= as.POSIXct("2016-09-09",tz= "UTC"),], 
                                         y = RefData[RefData$date>= as.POSIXct("2016-09-09",tz= "UTC"),], by = "date", all.x = TRUE ) 
                    } 
                } else { # RefData exists but no InfluxData and no SOSData
                    General <- RefData
                    cat("[GENERAL] ERROR: General data frame, there are no new sensor data and/or new reference data.\n")
                }
            }
            
        } else { # RefData does not exist
            
            if (exists("InfluxData") && !is.null(InfluxData)) { # RefData does not exist but InfluxData exists
                
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
                
                # General <- data.frame(timeAverage(InfluxData, 
                #                                   avg.time = paste0(toString(UserMins)," ","min"), 
                #                                   statistic = "mean", 
                #                                   start.date = round(min(InfluxData$date), units = "hours")
                # ))
                # if (any(grepl(pattern = "_raw", x = colnames(InfluxData)))) {
                #     General <- General[-grep(pattern = "_raw", x = colnames(General)),]  
                # } 
                General <- InfluxData
                
                cat("[GENERAL] INFO, General data frame, there are new sensor data while there are no new reference data \n")
                
            } else { # RefData and InfluxData do not exist
                
                if (exists("SOSData") && !is.null(SOSData)) { # RefData and InfluxData do not exist but SOSData exists
                    
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
                    
                    # General     <- data.frame(timeAverage(SOSData, 
                    #                                       avg.time = paste0(toString(UserMins)," ","min"), 
                    #                                       statistic = "mean", 
                    #                                       start.date = round(min(SOSData$date), units = "hours")
                    # )
                    General     <- SOSData
                    
                } else { # RefData, InfluxData and SOSData do not exist
                    
                    General <- NA
                    cat("[GENERAL] ERROR: General data frame, there are no new sensor data nor reference data \n")
                }
            }
        }
        
        # Select only the dataframe (not tbl_df" and "tbl") averaging if needed 
        if (exists("General") && !is.null(General) & !all(is.na(General))) {
            
            # discarding rows with all NAs and NaNs for All gas sensors
            cat("[GENERAL] INFO, Discarding rows with NA and NaN for all gas sensors\n")
            # replacing NaN with NA
            cat(paste0("[GENERAL] INFO, replacing sensors values which are not numbers (NaN) with NA for all parameters."), sep = "\n")
            for (i in names(General)[which(names(General)!= "date")]) if (any(is.nan(General[,i]))) General[is.nan(General[,i]),i] <- NA
            
            #browser()
            ind <- which(apply(General[,grep(paste(var.name.GasSensors,collapse="|"), x = names(General))], 1, function(x) all(is.na(x))))
            if (length(ind)!=0) {
                General <- General[-ind,] 
                cat(paste0("[GENERAL] INFO, ", length(ind), " rows have been discarded, with only NAs for all gas sensors\n"))
            } 
            remove(ind)
        }
        
        # Averaging using UserMins
        # The timeAverage is not needed maybe since from now the average is carried out by the InfluxQL. Comment the lines for later use, maybe
        # Yes, it is still needed if UserMins is changed
        General <- data.frame(timeAverage(General, 
                                          avg.time   = paste0(toString(UserMins)," ","min"), 
                                          statistic  = "mean", 
                                          start.date = round(min(General$date, na.rm = TRUE), units = "hours"), 
                                          end.date   = round(max(General$date, na.rm = TRUE), units = "hours")))
        
        # Discaring any _raw column
        if (any(grepl(pattern = "_raw", x = colnames(InfluxData)))) {
            General <- General[-grep(pattern = "_raw", x = colnames(General)),]  
        } 
    } else { # Selecting General.Rdata if it exists
        
        if (file.exists(file.path(WDoutput, "General.Rdata"))) {
            load(file.path(WDoutput, "General.Rdata"))
            General <- General.df
        } else {
            return(cat("[GENERAL] ERROR no General data available\n"))   
            cat("-----------------------------------------------------------------------------------\n")
        } 
    }
    
    if (exists("General") && !is.null(General)) {
        cat("[GENERAL] INFO returning General dataframe\n")
        cat("-----------------------------------------------------------------------------------\n")
        
        # adding absolute humidity is relative humidity and temperature exist
        if (all(c("Temperature", "Relative_humidity") %in% names(General))) {
            
            General$Absolute_humidity <- NA
            both.Temp.Hum <- complete.cases(General[, c("Temperature", "Relative_humidity")])
            #browser()
            General[both.Temp.Hum, "Absolute_humidity"] <- threadr::absolute_humidity(General[both.Temp.Hum, "Temperature"], General[both.Temp.Hum, "Relative_humidity"]) 
        }
        
        # returning General
        return(General) 
    } else{
        return(cat("[GENERAL] ERROR no General data available\n"))   
        cat("-----------------------------------------------------------------------------------\n")
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
        
        #browser() 
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
            digitround <- round(log10(1/Range))+2 # +1 gives too many digits? no it is fine
        }
        
        # Calculating the limits of the graph
        if (is.null(lim)) {
            if (isTRUE(XY_same)) {
                if (is.null(s_y) || any(s_y == 0) || all(is.na(s_y))) {
                    Xlim <- c(round(min(DataXY[is.finite(DataXY$x),"x"], DataXY[is.finite(DataXY$y),"y"]),digits=digitround[1]), 
                              round(max(DataXY[is.finite(DataXY$x),"x"], DataXY[is.finite(DataXY$y),"y"]),digits=digitround[1]))
                    Ylim <- Xlim
                } else {
                    Xlim <- c(round(min(DataXY[is.finite(DataXY$x),"x"], DataXY[is.finite(DataXY$y),"y"] - DataXY$s_y),digits=digitround[1]),
                              round(max(DataXY[is.finite(DataXY$x),"x"], DataXY[is.finite(DataXY$y),"y"] + DataXY$s_y),digits=digitround[1]))
                    Ylim <- Xlim
                }
            } else {
                if (is.null(s_y)|| any(s_y == 0) || all(is.na(s_y))) {
                    Ylim <- c(round(min(DataXY$y, na.rm = TRUE),digits=digitround[2]),
                              round(max(DataXY[is.finite(DataXY$y),"y"], na.rm=TRUE),digits=digitround[2]))
                } else {
                    #if ((max(DataXY$y + DataXY$s_y)-min(DataXY$y - DataXY$s_y)) < 1) {
                    #  Ylim <- c((min(DataXY$y - DataXY$s_y)),(max(DataXY$y + DataXY$s_y)))
                    #} else {
                    Ylim <- c(round(min(DataXY$y - DataXY$s_y),digits=digitround[2]),
                              round(max(DataXY$y + DataXY$s_y),digits=digitround[2]))
                    #}
                }
                if (class(DataXY$x) == "POSIXct") {
                    Xlim <- c(min(DataXY$x),max(DataXY$x))
                } else {
                    Xlim <- c(round(min(DataXY$x),digits=digitround[1]),
                              round(max(DataXY$x),digits=digitround[1]))
                }
            }
        } else {
            Xlim <- c(round(min(lim[,1]),digits=digitround[1]),round(max(lim[,1]),digits=digitround[1]))
            Ylim <- c(round(min(lim[,2]),digits=digitround[2]),round(max(lim[,2]),digits=digitround[2]))
        }
        # specifying ticks and grid
        if (is.null(steps)) {
            stepsX <- 10
            stepsY <- 10
        } else {
            stepsX <- steps[1] # was steps[,1] but imply to enter steps values as a matrix
            stepsY <- steps[2] # was steps[,2] but imply to enter steps values as a matrix
        }
        #browser()
        # plotting the scatterplot
        plot( DataXY$x, DataXY$y
              ,xlab= AxisLabelX
              ,ylab= AxisLabelY
              ,xlim = Xlim
              ,ylim = Ylim
              ,col = Couleur
              ,type = ligne
              ,pch = Marker
              # ,xaxp = c(min(Xlim), max(Xlim), stepsX)
              # ,yaxp = c(min(Ylim), max(Ylim), stepsY)
              # ,xaxt = PlotAxis
              # ,yaxt = PlotAxis
              ,xaxt = "n"
              ,yaxt = "n"
              
        )
        axis(side = 1, at = pretty(x,stepsX))
        axis(side = 2, at = pretty(y,stepsY))
        if (!is.null(s_y) && all(!is.na(s_y))) {
            if (all(s_y != 0)) {
                # hack: we draw arrows but with flat "arrowheads"
                arrows(DataXY$x, DataXY$y - DataXY$s_y , DataXY$x, DataXY$y + DataXY$s_y, length = 0.05, angle = 90, code = 3)
            }
        } 
        
        # par(xaxp = c(min(Xlim), max(Xlim), stepsX), 
        #     yaxp = c(min(Ylim), max(Ylim), stepsY)
        #     )
        #grid (NULL,NULL, lty = 2, col = "grey")
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
CONFIG <- function(DisqueFieldtest , ASEconfig) {
    # DisqueFieldtest   : directory where is the file ASEconfig*.R file
    # ASEConfig         : AirSensEUR name e.g LANUV_01 in ASEConfigLANUV_01.R or the AirSensEUR config file as in ASEConfigLANUV_01.R
    # Return a list with the config of servers, sensors and effects
    
    cat("-----------------------------------------------------------------------------------\n")
    File_Sensor.config <- TRUE
    ASE_name           <- basename(ASEconfig); for (i in c("\\.[[:alnum:]]+$","ASEconfig")) ASE_name <- sub(pattern=i,replacement = '', basename(as.character(ASE_name)))
    DisqueFieldtestDir <- file.path(DisqueFieldtest, ASE_name)
    
    #=====================================================================================CR
    #  ASE_name,"_Servers.cfg"
    #=====================================================================================CR
    # Read config file (TRUE) or manually create it (FALSE)?
    File_Server_cfg <- list.files(path = file.path(DisqueFieldtestDir, "General_data"), pattern = paste0(ASE_name,"_Servers.cfg"))
    if (!identical(File_Server_cfg,character(0)) & File_Sensor.config) {
        
        # reading the Server configuration files
        File_Server_cfg <- file.path(DisqueFieldtestDir, "General_data", paste0(ASE_name,"_Servers.cfg"))
        if (file.exists(File_Server_cfg)) {
            
            cfg <- transpose(
                read.table(file = File_Server_cfg, 
                           header = FALSE, 
                           row.names = NULL, 
                           stringsAsFactors = FALSE 
                )
            )
            row.names(cfg) <- NULL; names(cfg) <- cfg[1,]; cfg <- cfg[-1,]; cfg <-as.data.frame(cfg, stringsAsFactors = FALSE); row.names(cfg) <- NULL
            # Creating UserMinsAvg if it does not exist
            if (!any("UserMinsAvg" %in% names(cfg))) cfg$UserMinsAvg <- cfg$UserMins
            cat(paste0("[CONFIG] Info, the config file ", File_Server_cfg, " for the configuration of servers  exists"), sep = "\n")
            Vector.type <- c("PROXY",
                             "Down.Influx",
                             "Down.SOS",
                             "Down.Ref")
            for (i in Vector.type) if (i %in% colnames(cfg)) cfg[,i] <- as.logical(cfg[,i])
            Vector.type <- c("PORT",
                             "Port",
                             "UserMins",
                             "UserMinsAvg",
                             "Delay")
            for (i in Vector.type) {if (i %in% colnames(cfg)) cfg[,i] <- as.numeric(cfg[,i])}
        } else stop(cat(paste0("[CONFIG] The file of server configuration for AirSensEUR: ", File_Server_cfg, " does not exist. Please change File_Sensor.config <- FALSE "), sep = "\n"))
        
    } else { # if File_Server_cfg does not exist, default values are given
        
        #=====================================================================================CR
        #  1. Configuring Proxy server
        #=====================================================================================CR
        # At JRC we have now a proxy that block downloading from github, using the following code, we can download from Github
        PROXY = TRUE; URL      = "10.168.209.72"; PORT     = 8012; LOGIN    = NULL; PASSWORD = NULL
        # no login and no password on our proxy 
        if (PROXY) {
            # checking that we have the httr package to use function Set_Config()
            Load.Packages("httr")
            cat("[CONFIG] INFO Package httr loaded\n")
            # implement PROXY 
            set_config(use_proxy(url=URL, port=PORT, username = LOGIN, password = PASSWORD)) 
        } else reset_config()
        
        #=====================================================================================CR
        #  2. Sensor configuration for download for Influx and SOS. InfluxDB has more info and is preferred over SOS
        #=====================================================================================CR
        # Influx
        Down.Influx         <- TRUE                             # is download from Influx possible ?
        Host                <- "influxdb1.liberaintentio.com"   # web site to retrieve Sensor data from InfluxDB
        Port                <- "8086"                           # Port of communication on http 3000 (might be blocket for IT security) or 8086 generally opened
        User                <- "shinyreader"                    # Login to InfluxDb
        Pass                <- "sqwEgzli"                       # password associated with the Login
        Db                  <- "asmeeting17"                    # Name of the SQLite database
        Dataset             <- "AirSensEUR01"                   # Name of the table in the airsenseur.db. There is a mess with the name of SOS: "AirSensEUR01" corresponds to "C05_EMEP_01" ? why not JRC_C05_01
        Influx.TZ            <- 'UTC'                           # Time zone of Influx data, default is UTC
        ### Influx.TZ used in Functions4ASE.R::Sqlite2df. I am wondering if Influx.TZ should be a shiny input??? I guess not since Influx convert to UTC. By observation the influx time seems to be in CEST
        ### The Influx site states that all Influx data are in UTC (https://docs.influxdata.com/influxdb/v1.0/guides/querying_data/#timestamp-format)
        ### I should be checked comparing the airsenseur.db on an ASE and on Influxdb --> no InfluxDB data appear to be in local time!
        ### Additionally we can determine the time zone using the coordinats of GPS if available with the function get_google_tz()
        
        # SOS
        Down.SOS            <- TRUE             # is download from SOS possible ?
        AirsensWeb          <- "http://sossvr1.liberaintentio.com:8080/AirSenSOS/api/v1/"   ### # web site to retrieve Sensor data from SOS
        AirsensEur.name     <- ASE_name # This shall be the last identifier in each line of timeseries(apiEndpoint), see in Down_SOS - using ASEconfig
        SOS.TZ              <- "UTC"            # Define time zone for SOS data
        
        #=====================================================================================CR
        #  3. Reference data, configuration for download, ftp 
        #=====================================================================================CR
        # SET EMEP TIMESERIES parameters
        # UTC time
        Down.Ref            <- TRUE                  # is download from reference data server requested ?
        # The file shall be csv files, with headers, either on a ftp sites or local PC files - describe the column in section 7 in the section Ref
        urlref <- c( "ftp://cidportal.jrc.ec.europa.eu/jrc-opendata/ABCIS/InorganicGases/Ver2017-01-01/InorganicGases_2017.csv")
        Reference.name   <- "Reference_EMEP_station" # Identifier reference station 
        # Coordinates of the EMEP sation, coord.sens <- "45 49' N, 8 38' E", altitude of the sensor alt.sens <- "209 m"
        coord.ref <- "45 49' N, 8 38' E"             # Latitude and longitude reference position - They should be used to find when AirSensEUR is at the station, work for Riccardo
        alt.ref   <- "209 m"                         # Altitude refrence station
        ref.tzone <- "UTC"                           # Define time zone
        
        # Sensor shield configuration file
        asc.File <- "170604 ASE_R24 NO2B43F_COA4_OXA431_NOB4_Training.asc"
        
        #=====================================================================================CR
        # 8. SET Average time for reference and sensor data
        #=====================================================================================CR
        # set number of minutes (<60). The unit shall be in minutes
        UserMins    <- 10
        UserMinsAvg <- 10
        
        #=====================================================================================CR
        # 11 - Delays between AirSensEUR and Reference data
        #=====================================================================================CR
        # delays in minutes (positive and negative, the values are added) in SensData, default values should be 0
        Delay <- -10
        
        # Saving config file
        cfg <- data.frame(PROXY = PROXY, 
                          URL      = URL, 
                          PORT     = PORT, 
                          LOGIN    = LOGIN, 
                          PASSWORD = PASSWORD,
                          
                          Down.Influx = Down.Influx, 
                          Host        = Host, 
                          Port        = Port, 
                          User        = User, 
                          Pass        = Pass, 
                          Db          = Db, 
                          Dataset     = Dataset, 
                          Influx.TZ   = Influx.TZ,
                          
                          Down.SOS        = Down.SOS, 
                          AirsensWeb      = AirsensWeb, 
                          AirsensEur.name = AirsensEur.name, 
                          SOS.TZ          = SOS.TZ,
                          
                          Down.Ref       = Down.Ref, 
                          urlref         = urlref, 
                          Reference.name = Reference.name, 
                          coord.ref      = coord.ref, 
                          alt.ref        = alt.ref, 
                          ref.tzone      = ref.tzone,
                          
                          asc.File    = asc.File, 
                          UserMins    = UserMins,
                          UserMinsAvg = UserMinsAvg,
                          Delay    = Delay,
                          
                          stringsAsFactors = FALSE)
        write.csv(t(cfg), file = file.path(DisqueFieldtestDir,"General_data",paste0(ASE_name,"_Servers.cfg")))
    }
    
    #browser()
    #=====================================================================================CR
    #  4. Create sensor configuration file and matching between reference and sensor names: ASE_name,".cfg"
    #=====================================================================================CR
    # This is to insert both sensors and reference configuration into a dataframe and file
    File_cfg <- list.files(path = file.path(DisqueFieldtestDir,"General_data"), pattern = paste0(ASE_name,".cfg"))
    if (!identical(File_cfg,character(0)) & File_Sensor.config) {
        # reading the configuration files sens2ref
        File_cfg <- file.path(DisqueFieldtestDir,"General_data", paste0(ASE_name,".cfg"))
        if (file.exists(File_cfg)) {
            cat(paste0("[CONFIG] Info, the config file ", File_cfg, " for the configuration of AirSensEUR exists"), sep = "\n")
            #browser()
            sens2ref <- t(read.table(file = File_cfg, header = TRUE, stringsAsFactors = FALSE))
            row.names(sens2ref) <- NULL
            sens2ref <- as.data.frame(sens2ref, stringsAsFactors = FALSE)
            #change the type of column in df
            Vector.type <- c("Ref.rm.Out","Sens.Inval.Out","Apply.Invalid", "remove.neg","Sens.rm.Out","Neg.mod")
            for (i in Vector.type) if (i %in% colnames(sens2ref)) sens2ref[,i] <- as.logical(gsub(" ","",sens2ref[,i]))
            Vector.type <- c("uxi","Ref.window","Ref.threshold","Ref.Ymin","Ref.Ymax","Ref.ThresholdMin","Ref.iterations","Gain","Intercept","Slope",
                             "Sens.window","Sens.threshold","Sens.Ymin","Sens.Ymax","Sens.ThresholdMin","Sens.iterations",
                             "Rload","TIA_Gain","GAIN","Int_Z","Bias_Sign","Bias","Fet_Short","Ref","RefAD","RefAFE","board.zero.set","BIAIS",
                             "temp.thres.min","temp.thres.max","rh.thres.min","rh.thres.max","hoursWarming")
            # suppress warning Warning: NAs introduced by coercion or use function taRifx::destring
            for (i in Vector.type) {
                
                if (i %in% colnames(sens2ref)) sens2ref[,i] <- suppressWarnings(as.numeric(gsub(" ","",sens2ref[,i])))
            }
        } else cat(paste0("[CONFIG] The config file of sensors configuration for AirSensEUR: ", File_cfg, " does not exist. Please change File_Sensor.config <- FALSE "), sep = "\n")
        
        # updating names of sensors with the sensor schield config file
        # Reading sensor config file and merging with sens2ref if the file exists
        if (file.exists(file.path(DisqueFieldtest,"Shield_Files",cfg$asc.File))) {
            sens2ref.Covariates <-ASEPanel04Read(ASEPanel04File = file.path(DisqueFieldtest,"Shield_Files",cfg$asc.File))
           # browser()
            # sens2ref <- merge(x = sens2ref[,-which(names(sens2ref) %in% names(sens2ref.Covariates)[-which(names(sens2ref.Covariates) == "name.gas")])], 
            #                   y = sens2ref.Covariates, by = 'name.gas', all.x = TRUE, fill = NA)
            
            # Updating with new names of chemical sensors
            sens2ref[match(sens2ref.Covariates$name.gas, na.omit(sens2ref$name.gas)), "name.sensor"] <- sens2ref.Covariates$name.sensor
            
            # gas sensor names whcih are in sens2ref and not in sens2ref.Covariates, e. g. PM10
            Other.name.gas     <- sens2ref$name.gas[which(!(sens2ref$name.gas %in% sens2ref.Covariates$name.gas) & !is.na(sens2ref$gas.sensor))]
            Other.gas.sensors  <- sens2ref$gas.sensor[which(sens2ref$name.gas %in% Other.name.gas & !is.na(sens2ref$gas.sensor))]
            Other.name.sensors <- sens2ref$name.sensor[which(sens2ref$name.gas %in% Other.name.gas & !is.na(sens2ref$name.sensor))]
            
            # Merging sens2ref with new parameters of checmical shield
            sens2ref <- merge(x = sens2ref[,-which(names(sens2ref) %in% names(sens2ref.Covariates)[-which(names(sens2ref.Covariates) %in% c("name.sensor", "name.gas"))])], 
                  y = sens2ref.Covariates[, -which(names(sens2ref.Covariates) == "name.sensor")], by = 'name.gas', all.x = TRUE, fill = NA)
        
            sens2ref[which(sens2ref$name.gas %in% Other.name.gas), "gas.sensor"]  <- Other.gas.sensors
            sens2ref[which(sens2ref$name.gas %in% Other.name.gas), "name.sensor"] <- Other.name.sensors
            
        }
    } else { # if File_cfg does not exist, default values are given
        
        #1 gas name to refer to IMPORTANT THis names should come from the Download of Reference data in the Shiny App before
        name.gas                <- c("CO"               , "NO"              , "NO2"             , "O3"      , "NOx"     , "SO2"    )
        sens2ref                <- data.frame(name.gas          = name.gas, 
                                              check.names       = FALSE, 
                                              stringsAsFactors  = FALSE)
        rm(name.gas)
        
        #3 gas sensor: IMPORTANT - put name.gas and gas.sensor under the same column
        # Order of reference parameters not of the sensors on AirSensEUR
        sens2ref$gas.sensor     <- c("Carbon_monoxide"  , "Nitric_oxide"    , "Nitrogen_dioxide", "Ozone"   , NA        , NA       )
        # Insert the name given in SOS
        sens2ref$name.sensor    <- c("co_a4"            , "no_b4"           , "NO2_b43f"        , "o3_ax431", NA       , NA       )
        
        #2 gas reference
        sens2ref$gas.reference   <- c("CO_ppm"        , "NO"          , "NO2"             , "O3"        , "NOx"    , "SO2"    )
        sens2ref$gas.reference2use  <- paste0("Ref.", sens2ref$gas.reference)
        sens2ref$ref.unitgas     <- c("ppm"           , "ppb"         , "ppb"             , "ppb"       , "ppb"    , "ppb"    )
        
        # Filtering reference values? TRUE: outliers are discarded, False outliers are not examined
        sens2ref$Ref.rm.Out      <- c(FALSE           , TRUE          ,TRUE               , TRUE        ,TRUE      , TRUE     )
        # Outliers filtering for Reference:
        ## Rolling window: if we want to have 24 hours before sampling : 24 * 6  = 144, 18 --> 3 hours
        sens2ref$Ref.window      <- c(18              , 18            ,18                 , 18         , 18        , 18       )
        # MG coefficent that multiplied by difference between mean and median evidences outliers if exceeded
        sens2ref$Ref.threshold   <- c(20              , 30            ,25                 , 5          , 25        , 25       )
        # Minimum values in data series
        sens2ref$Ref.Ymin        <- c(0               , 0             , 0                 , 0          , 0         , 0        )
        # Minimum values in data series, in unit of reference data series
        sens2ref$Ref.Ymax        <- c(30              ,2000           ,500                , 200        , 3000      , 30       )
        # Minimum z values for testing
        sens2ref$Ref.ThresholdMin<- c(0               ,0              ,0                  , 0          , 0         , 0        )
        sens2ref$Ref.iterations  <- c(1               ,1              ,1                  , 1          , 1         , 1        )
        
        #5 AIRSENSNEUR BOARD: Characteristic volt values, this is set in JAVAControl Panel
        # to identify the correct column always refers to sens2ref$gas.sensor 
        sens2ref$Sens.raw.unit   <- c("V"             , "V"           , "V"               , "V"        , NA        , NA       )
        
        sens2ref$Ref             <- c(1.5             , 1.7           , 1.7               , 1.7        , NA        , NA       )
        sens2ref$RefAD           <- c(0.5             , 1             , 0.5               , 0.5        , NA        , NA       )
        sens2ref$board.zero.set  <- c(1.1             , 0.8316        , 2.15              , 2.15       , NA        , NA       )
        sens2ref$Rload           <- c(50              , 50            , 50                , 50         , NA        , NA       )
        sens2ref$Gain            <- c(350E3           , 350E3         , 350E3             , 350E3      , NA        , NA       )
        sens2ref$Intercept       <- c(1.08358         , 0.84113       , 2.149             , 2.148      , NA        , NA       )
        sens2ref$Slope           <- c(0.0811733/1000  , 0.0001372     , -0.0000877        , -0.0003670 , NA        , NA       ) 
        sens2ref$Sens.unit       <- c("ppb"           , "ppb"         , "ppb"             , "ppb"      , NA        , NA       )
        # the slopes correspond to the sensor sensitivity in V/ppb. The Sensitivity of CO that is normally given in V/ppm
        # was transformed in V/ppb for homogeneity with the other sensors: same unit
        
        #4 Outliers filtering for sensors:
        # Filtering invlid Sensors data? TRUE: Invalids are discarded, False outliers are not examined
        sens2ref$Sens.Invalid.Out     <- c(FALSE            , TRUE          ,TRUE               , TRUE      , NA        , NA       )
        # Filtering Sensors? TRUE: outliers are discarded, False outliers are not examined
        sens2ref$Sens.rm.Out     <- c(FALSE            , TRUE          ,TRUE               , TRUE      , NA        , NA       )
        ## Rolling window: if we want to have 24 hours before sampling : 24 * 6  = 144, 18 --> 3 hours
        sens2ref$Sens.window     <- c(18               , 18            ,18                 ,18        , NA        , NA       ) # use these defaults values first
        
        # MG coefficent that multiplied by difference between mean and median evidences outliers if exceeded
        sens2ref$Sens.threshold  <- c(20               , 20            ,20                 ,20         , NA        , NA       ) # use these defaults values first
        
        # Minimum values in data series, digital values
        sens2ref$Sens.Ymin       <- c(0                ,0              ,55000              ,55000      , NA        , NA       )
        
        # Minimum values in data series, digital values
        sens2ref$Sens.Ymax       <- c(20000            ,20000          ,2^16-1+1           ,2^16-1+1     , NA        , NA       )
        # Minimum z values for testing
        sens2ref$Sens.ThresholdMin<- c(0               ,0              ,40000              ,35000      , NA        , NA       )
        ### Number of iteration of precise outlier discarding 
        sens2ref$Sens.iterations  <- c(3                ,3              ,3                  , 3         , NA         , NA        )
        # Selecting valid period of Sensors? (valid because ASE is at the station?)
        if (!any(names(sens2ref) %in% "Sens.Inval.Out")) sens2ref$Sens.Inval.Out  <- c(TRUE             , TRUE          ,TRUE               , TRUE      , NA        , NA       )
        if (!any(names(sens2ref) %in% "Apply.Invalid")) sens2ref$Sens.Inval.Out  <- c(TRUE             , TRUE          ,TRUE               , TRUE      , NA        , NA       )
        
        # Reading sensor config file and merging with sens2ref
        if (file.exists(file.path(DisqueFieldtest,"Shield_Files",asc.File))) {
            sens2ref.Covariates <-ASEPanel04Read(ASEPanel04File = file.path(DisqueFieldtest,"Shield_Files",asc.File))
            sens2ref <- merge(x = sens2ref[,-which(names(sens2ref) %in% names(sens2ref.Covariates)[-which(names(sens2ref.Covariates) == "name.gas")])], 
                              y = sens2ref.Covariates, by = 'name.gas', all.x = TRUE, fill = NA)
        } else stop(cat("[CONFIG] ERROR, asc.File not found\n"))
        
        
        #=====================================================================================CR
        #  9. Calibration with Etalonnage
        #=====================================================================================CR
        # This is to model data with etalonage, MGV suggested Linear.Robust that avoid the effect of very high or very low values together with remove.neg =TRUE
        # Options: "Linear", "Linear.Robust" and Michel's + gam
        sens2ref$mod.eta.model.type <- c("Linear.Robust", "Linear.Robust"  , "Linear.Robust", "Linear.Robust", NA        , NA       )
        # this is to validate modelled data vs referenced measurements
        sens2ref$eta.model.type     <- c("Linear"       , "Linear"         , "Linear"       , "Linear"       , NA        , NA       )
        sens2ref$remove.neg         <- c(TRUE           , TRUE             , TRUE           , TRUE           , TRUE      , TRUE     )
        sens2ref$Neg.mod            <- c(TRUE           , TRUE             , TRUE           , TRUE           , TRUE      , TRUE     )
        
        #=====================================================================================CR
        # 10. SET temperature and relative humidity thresholds for sensors validity
        #=====================================================================================CR
        sens2ref$temp.thres.min     <- c(-20            , -20               , -20           , -20       , NA        , NA       )
        sens2ref$temp.thres.max     <- c(40             , 40                , 40            , 40        , NA        , NA       )
        sens2ref$rh.thres.min       <- c(15             , 15                , 15            , 15        , NA        , NA       )
        sens2ref$rh.thres.max       <- c(97             , 97                , 97            , 97        , NA        , NA       )
        
        #=====================================================================================CR
        # 11 - Warming hours of SensEUR
        #=====================================================================================CR
        # number of hours of warming after each swith on of the sensors
        sens2ref$hoursWarming <- c(  7,  7,  7,  7, NA, NA)
        
        #=====================================================================================CR
        # 13. SET  Models for NO2 and O3 system resolution
        #=====================================================================================CR
        # Apply Etalonnage and Cal_line for all gases and one by one?
        sens2ref$Cal.Line   <- c("New calibration with current data","New calibration with current data","New calibration with current data","New calibration with current data",NA,NA)
        sens2ref$Cal.func   <- c(NA,NA,NA,NA,NA,NA)
        # function model for NO2 and O3 to find coefficient for linear solving
        CO.NO.NO2.O3.model  <- FALSE     # do you want the NO2.O3 model to be applied?
        LM.model.lab.coeff  <- FALSE    # If TRUE Coefficients for LM model are provided from lab-tests
        # if FALSE -> fitting
        # if LM.model.lab.coeff = TRUE, the following needs to be specified
        file.coeff.dir  <-  paste0(DisqueFieldtestDir,"lab_test_coeff/")
    
    }
    # Saving config file in all cases if asc.File is changed
    write.table(t(sens2ref), file = file.path(DisqueFieldtestDir,"General_data",paste0(ASE_name,".cfg")))
    
    # reading the files with Covariates to plot and covariates to calibrate
    #browser()
    for (i in 1:length(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)])) {
        # browser()
        nameFile <- file.path(DisqueFieldtestDir,"General_data",paste0(ASE_name,"_Covariates_",sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],".cfg"))
        nameGas  <- sens2ref.Covariates[sens2ref$name.sensor[!is.na(sens2ref$name.sensor)] == sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],"name.gas"]
        # browser()
        if (file.exists(nameFile)) {
            
            cat(paste0("[CONFIG] INFO, the file with covariates to plot ", nameFile, " exists "), sep = "\n")
            assign(paste0(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i]), 
                   read.csv(file = nameFile,
                            header = TRUE, 
                            comment.char = "#", 
                            stringsAsFactors = FALSE)
                   )
        } else{
            
            cat(paste0("[CONFIG] ERROR, the file with covariates to plot ", nameFile, " does not exist. File is iniatized with the R script info."), sep = "\n")
            # DEFINE The lists of variables to plot in retrieving data () to understand the inerferences - use report of lab. tests
            if (nameGas == "CO") {
                
                assign(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i], 
                       data.frame(Effects = c(paste0(nameGas,"_volt"), "Out.Ref.CO_ppm" , "Relative_humidity", "Temperature")))
            } else {
                
                if (nameGas == "O3" || nameGas == "NO2") {
                    
                    assign(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i], 
                           data.frame(Effects = c(paste0(nameGas,"_volt"), "Out.Ref.NO2"    , "Out.Ref.O3"       , "Relative_humidity", "Temperature")))
                } else assign(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i], 
                              data.frame(Effects = c(paste0(nameGas,"_volt"), "Out.Ref.NO"     , "Relative_humidity", "Temperature")))
            } 
            
            # Saving the effect files
            SENS <- get(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i])
            write.csv(SENS, file = nameFile, row.names = FALSE)
            rm(SENS)
        } 
        
        # Covariates to calibrate
        nameFile <- file.path(DisqueFieldtestDir,"General_data",paste0(ASE_name,"_CovMod_",sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],".cfg"))
        if (file.exists(nameFile)) {
            
            cat(paste0("[CONFIG] INFO, the file with covariates to calibrate ", nameFile, " exists "), sep = "\n")
            assign(paste0(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],"CovMod"), read.csv(file = nameFile, 
                                                                                 header = TRUE, 
                                                                                 comment.char = "#", 
                                                                                 stringsAsFactors = FALSE
            )
            )
            
        } else{
            
            cat(paste0("[CONFIG] ERROR, the file with covariates to calibrate ", nameFile, " does not exist. File is iniatized with the R script info."), sep = "\n")
            # DEFINE The lists of variables to plot in retrieving data () to understand the inerferences - use report of lab. tests
            assign(paste0(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],"CovMod"), 
                   data.frame(Effects = c(paste0(nameGas,"_volt"), "Temperature")))
            
            # Saving the effect files
            SENS <- get(paste0(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],"CovMod"))
            write.csv(SENS, file = nameFile, row.names = FALSE)
            rm(SENS)
        } 
        
    }
    
    #browser()
    Covariates <- lapply(which(!is.na(sens2ref$name.sensor)), function(i) get(sens2ref$name.sensor[i]) )
    # list(get(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][1]), 
    #                    get(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][2]), 
    #                    get(sens2ref.Covariates$name.sensor[3]), 
    #                    get(sens2ref.Covariates$name.sensor[4])
    # ) 
    
    
    
    names(Covariates) <- paste0(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)])
    
    CovMod <- lapply(which(!is.na(sens2ref$name.sensor)), function(i) get(paste0(sens2ref$name.sensor[i],"CovMod")))
    # list(get(paste0(sens2ref.Covariates$name.sensor[1],"CovMod")), 
    #                get(paste0(sens2ref.Covariates$name.sensor[2],"CovMod")), 
    #                get(paste0(sens2ref.Covariates$name.sensor[3],"CovMod")), 
    #                get(paste0(sens2ref.Covariates$name.sensor[4],"CovMod"))
    # ) 
    names(CovMod) <- paste0(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)])
    
    cat("-----------------------------------------------------------------------------------\n")
    return.CONFIG <- list(cfg,sens2ref,Covariates,CovMod)
    names(return.CONFIG) <- c("cfg","sens2ref","Covariates","CovMod")
    #browser()
    return(list(cfg,sens2ref,Covariates,CovMod))
}

#=====================================================================================CR
# SETTIME of ASE Boxes ex.  ASEconfig.R 
#=====================================================================================CR
# USER CONFIG PARAGRAPH
# 11. Valid Periods                                                                 (NOT USED)
#=====================================================================================CR
SETTIME <- function(DisqueFieldtestDir, General.t.Valid = NULL, Influx.TZ = "UTC" , SOS.TZ = "UTC", Ref.TZ = "UTC", DownloadSensor) {
    # DisqueFieldtestDir : file.path where the config files of the AIrSensEUR are located. The directory ""Shield_Files" shall be located at the pareent directory
    # General.t.Valid    : dataframe with date , sensor and reference data, default is NULL, it is only use if the the File_SETTIME_cfg file does not exist
    # DownloadSensor     : output of function DownloadSensor()
    # Influx.TZ          : String, time zone of INFLUXDB data, default is UTC
    # SOS.TZ             : String, time zone of SOS data, default is UTC
    # Ref.TZ             : String, time zone of Reference data, default is UTC
    # Return             : list with sens2ref (only time parameters)
    
    cat("-----------------------------------------------------------------------------------\n")
    File_Sensor.config <- TRUE
    ASE_name           <- basename(DisqueFieldtestDir)
    File_Server_cfg    <- file.path(DisqueFieldtestDir, "General_data", paste0(ASE_name,"_Servers.cfg"))
    
    # Setting the General time zone to the one of DownloadSensor$DateIN.General.prev or DateIN.Influx.prev or DateIN.SOS.prev otherwise it is set to "UTC"
    if (exists("DownloadSensor")) {
        if (!is.null(DownloadSensor$DateIN.General.prev)) {
            General.TZ <- base::format(DownloadSensor$DateIN.General.prev, format= "%Z")
        } else {
            if (!is.null(DownloadSensor$DateIN.Influx.prev)) {
                General.TZ <- base::format(DownloadSensor$DateIN.Influx.prev, format= "%Z")
            } else {
                if (!is.null(DownloadSensor$DateIN.SOS.prev)) {
                    General.TZ <- base::format(DownloadSensor$DateIN.SOS.prev, format= "%Z")
                } else General.TZ <- "UTC"  
            }
        }
    } else  General.TZ <- "UTC"
    
    # Read config file (TRUE) or manually create it (FALSE)?
    File_SETTIME_cfg   <- list.files(path = file.path(DisqueFieldtestDir, "General_data"), pattern = paste0(ASE_name,"_SETTIME.cfg")  )
    if (!identical(File_SETTIME_cfg,character(0)) & File_Sensor.config) {
        
        # reading the configuration files sens2ref
        File_SETTIME_cfg <- file.path(DisqueFieldtestDir, "General_data", paste0(ASE_name,"_SETTIME",".cfg"))
        if (file.exists(File_SETTIME_cfg)) {
            
            cat(paste0("[SETTIME] Info, the config file ", File_SETTIME_cfg, " for the configuration of AirSensEUR exists"), sep = "\n")
            #browser()
            sens2ref <- as.data.frame(read.table(file = file.path(File_SETTIME_cfg), header = TRUE, stringsAsFactors = FALSE), stringsAsFactors = FALSE)
            
            # Changing the name.sensor when asc.File changes
            # First  read the -server.cfg file to get teh file name of the shiel config file
            if (file.exists(File_Server_cfg)) {
                
                cfg <- transpose(read.table(file = File_Server_cfg, 
                                            header = FALSE, 
                                            row.names = NULL, 
                                            stringsAsFactors = FALSE )
                )
                row.names(cfg) <- NULL
                names(cfg) <- cfg[1,]
                cfg <- cfg[-1,]
                cfg<-as.data.frame(cfg, 
                                   stringsAsFactors = FALSE
                )
                row.names(cfg) <- NULL
                
                # Changes names for change version 0.9 to 0.10
                Change.names <- rbind(c("TZ"           , "Influx.TZ"),     # Time zone of Influx data
                                      c("sens.tzone"   , "SOS.TZ")         # Time zone of SOS data
                )
                list.names.cfg <- colnames(cfg) 
                for (k in 1:nrow(Change.names)) if (Change.names[k,1] %in% list.names.cfg) colnames(cfg)[colnames(cfg) == Change.names[k,1]] <- Change.names[k,2]
                
                
            } else cat(paste0("[SETTIME] The file of server configuration for AirSensEUR: ", File_Server_cfg, " does not exist. Please change File_Sensor.config <- FALSE \n"))
            
            # Second read the shield config file to get the sensor names
            if (file.exists(file.path(dirname(DisqueFieldtestDir),"Shield_Files",cfg$asc.File))) {
                
                sens2ref.Covariates <- ASEPanel04Read(ASEPanel04File = file.path(dirname(DisqueFieldtestDir),"Shield_Files",cfg$asc.File))
                
            }  else cat("[SETTIME] ERROR shield file (asc.File) not found\n")
            
            # update the name of sensors in the SETTIME.cfg
            if (!all(sens2ref.Covariates$name.sensor %in% sens2ref["name.sensor",])) {
                for (i in 1:length(sens2ref.Covariates$name.sensor)) sens2ref["name.sensor", which(sens2ref["name.gas",] == sens2ref.Covariates$name.gas[i])] <- sens2ref.Covariates$name.sensor[i]
                # Saving config file
                write.table(sens2ref, file = file.path(DisqueFieldtestDir,"General_data",paste0(ASE_name,"_SETTIME.cfg")), col.names = TRUE, row.names = TRUE)
                cat("[SETTIME] INFO Saving the ASE_name_SETTIME.cfg with correct name of sensors.\n")
            }
            
            sens2ref <- t(sens2ref) 
            
            # after t() a dataframe is coerced into matrix, set to dataframe again
            row.names(sens2ref) <- NULL
            sens2ref<-as.data.frame(sens2ref, stringsAsFactors = FALSE)
            
            # Changes names for change version 0.6 to 0.7
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
            list.names.sens2ref <- colnames(sens2ref) 
            for (k in 1:nrow(Change.names)) if (Change.names[k,1] %in% list.names.sens2ref) colnames(sens2ref)[colnames(sens2ref) == Change.names[k,1]] <- Change.names[k,2]
            # adding "Cov.Date.IN" "Cov.Date.END"if missing
            if (!("Cov.Date.IN"  %in% list.names.sens2ref)) sens2ref$Cov.Date.IN  <- sens2ref$Valid.IN
            if (!("Cov.Date.END" %in% list.names.sens2ref)) sens2ref$Cov.Date.END <- sens2ref$Valid.END
            
            # coerce Sens.Inval.Out  and "Apply.Invalid" to logical
            Vector.type <- c("Sens.Inval.Out", "Apply.Invalid")
            # coerce to POSIX "Out.Ref.IN","Out.Ref.END","Out.Sens.IN","Out.Sens.END","Valid.IN","Valid.END","Cov.Date.IN", "Cov.Date.END", "DateCal.IN","DateCal.END","DatePlotCal.IN","DatePlotCal.END","DateMeas.IN","DateMeas.END","DatePlotMeas.IN", "DatePlotMeas.END" 
            for (i in Vector.type) if (i %in% colnames(sens2ref)) sens2ref[,i] <- as.logical(sens2ref[,i])
            
            # coerce chr of dates to POSIXct with Time Zone of General
            Vector.type <- c("Out.Ref.IN",
                             "Out.Ref.END",
                             "Out.Sens.IN",
                             "Out.Sens.END",
                             "Valid.IN",
                             "Valid.END",
                             "Cov.Date.IN", 
                             "Cov.Date.END", 
                             "DateCal.IN",
                             "DateCal.END",
                             "DatePlotCal.IN",
                             "DatePlotCal.END",
                             "DateMeas.IN",
                             "DateMeas.END",
                             "DatePlotMeas.IN", 
                             "DatePlotMeas.END" )
            for (i in Vector.type) if (i %in% colnames(sens2ref)) sens2ref[,i] <- parse_date_time(sens2ref[,i], tz = General.TZ, orders = "ymdHM") #which(!is.na(sens2ref$name.sensor))?
        } else cat(paste0("[SETTIME] The config file of sensors configuration for AirSensEUR: ", File_cfg, " does not exist. Please change File_Sensor.config <- FALSE "), sep = "\n")
        
    } else { # trying to create sens2Ref
        
        #1 gas name to refer to IMPORTANT THis names should come from the Download of Reference data in the Shiny App before
        sens2ref                <- data.frame(name.gas          = c("CO"               , "NO"              , "NO2"             , "O3"      , "NOx"    , "SO2"    ), 
                                              gas.sensor        = c("Carbon_monoxide"  , "Nitric_oxide"    , "Nitrogen_dioxide", "Ozone"   , NA       , NA       ),
                                              name.sensor       = c("COA4"             , "NOB4"            , "NO2Bb43F"        , "O3A431"  , NA       , NA       ),
                                              check.names       = FALSE, 
                                              stringsAsFactors  = FALSE)
        
        #=====================================================================================CR
        # 12. SET TIME PARAMETERS
        #=====================================================================================CR
        if ( !identical(File_SETTIME_cfg,character(0)) & any(!(c("Valid.IN","Valid.END") %in% names(sens2ref))) | identical(File_SETTIME_cfg,character(0))) {
            
            # UserDateIN, UserDateINCal and UserDateINmeas can be a date to be chosen, "System" or NULL (""); in these cases the code will set UserDateIN.0 and UserDateEND.0. 
            # If not, user shall enter the values UserDateIN.0 and UserDateEND.0,  UserDateINCal and UserDateENDCal, UserDateINmeas and UserDateENDmeas  as charater in the format %Y-%m-%d %H:%M
            # e. g. for 
            # UserDateIN      <- "2015-12-24 00:00"       # defined value, the interval is determined by the user
            # UserDateIN      <- ""                       # NULL value   , the interval is determined automatically
            # UserDateIN      <- "System"                 # system value , the interval is determined automatically 
            
            # Define how interval for calibration extrapolation is determined, time in TimeZone
            sens2ref$UserDateIN         <- c("2017-05-26 12:00", "2017-05-26 12:00", "2017-05-26 12:00", "2017-05-26 12:00", NA        , NA       )
            sens2ref$UserDateINCal      <- c("2017-06-04 00:00", "2017-06-04 00:00", "2017-06-04 00:00", "2017-06-04 00:00", NA        , NA       )
            sens2ref$UserDateENDCal     <- c("2017-06-11 10:00", "2017-06-10 12:00", "2017-06-10 12:00", "2017-06-10 12:00", NA        , NA       )
            sens2ref$UserDateINmeas     <- c("2017-06-04 00:00", "2017-06-04 00:00", "2017-06-04 00:00", "2017-06-04 00:00", NA        , NA       )
            sens2ref$UserDateENDmeas    <- c(rep(as.character(max(General.t.Valid$date, na.rm = TRUE)), times = 4),rep(NA,2))
            
            # Initial values
            sens2ref$Valid.IN        <- as.POSIXct(sens2ref$UserDateIN     , format = "%Y-%m-%d %H:%M", tz = General.TZ)
            sens2ref$Valid.END       <- as.POSIXct(sens2ref$UserDateENDmeas, format = "%Y-%m-%d %H:%M", tz = General.TZ)
            sens2ref$DateCal.IN      <- as.POSIXct(sens2ref$UserDateINCal  , format = "%Y-%m-%d %H:%M", tz = General.TZ)
            sens2ref$DateCal.END     <- as.POSIXct(sens2ref$UserDateENDCal , format = "%Y-%m-%d %H:%M", tz = General.TZ)
            sens2ref$DatePlotCal.IN  <- as.POSIXct(sens2ref$UserDateINCal  , format = "%Y-%m-%d %H:%M", tz = General.TZ)
            sens2ref$DatePlotCal.END <- as.POSIXct(sens2ref$UserDateENDCal , format = "%Y-%m-%d %H:%M", tz = General.TZ)
            sens2ref$DateMeas.IN     <- as.POSIXct(sens2ref$UserDateINmeas , format = "%Y-%m-%d %H:%M", tz = General.TZ)
            sens2ref$DateMeas.END    <- as.POSIXct(sens2ref$UserDateENDmeas, format = "%Y-%m-%d %H:%M", tz = General.TZ)
            
            for (i in which(!is.na(sens2ref$gas.sensor))) {
                if (sens2ref$UserDateINCal[i] == "System") {
                    # For OPTION == "System"
                    # Number of days to take for calibration in case the user defines: "System" as date time option
                    # Make a warning Note: if Valid.END.0-DateIN.0 < system.cal, "System" becomes "", the NULL option
                    sens2ref$system.cal[i]         <- 7  # this is also a Shiny input
                }
                
                #------------------------------------------------------------------------------CR
                # Set inital and final dates for data time series UserDateIN.0
                #------------------------------------------------------------------------------CR
                # Be careful with the user time zone. I think both Inlfuxdb and SOS are in UTC. 
                if ( sens2ref$UserDateIN[i] != "System" &  sens2ref$UserDateIN[i] != "") {
                    sens2ref$Valid.IN[i]        <- as.POSIXct(strptime(sens2ref$UserDateIN[i], format = "%Y-%m-%d %H:%M", tz = TimeZone))
                    sens2ref$Valid.END[i]       <- max(General.t.Valid$date, na.rm = TRUE) 
                } else { 
                    sens2ref$Valid.IN[i]        <- min(General.t.Valid$date, na.rm = TRUE)
                    sens2ref$Valid.END[i]       <- max(General.t.Valid$date, na.rm = TRUE) # MG I do not understand why we have tz = "CET" ! I think it should be tz = TimeZone
                }
                
                #------------------------------------------------------------------------------CR
                # Set initial and final dates to start calibration functions, values for UserDateINCal/UserDateENDCal
                #------------------------------------------------------------------------------CR
                if (sens2ref$UserDateINCal[i] != "System" & sens2ref$UserDateINCal[i] != "") {
                    sens2ref$DateCal.IN[i]       <- as.POSIXct(sens2ref$UserDateINCal[i] , format = "%Y-%m-%d %H:%M", tz =TimeZone )
                    sens2ref$DateCal.END[i]      <- as.POSIXct(sens2ref$UserDateENDCal[i], format = "%Y-%m-%d %H:%M", tz =TimeZone )
                } else {
                    #------------------------------------------------------------------------------CR
                    # Automatic calibration period
                    #------------------------------------------------------------------------------CR
                    cat("[SETTIME] INFO, SET TIME parameters for Calibration and measuring functions when users do not define dates")
                    # number of days between Valid.END.0 and DateIN.0
                    diff.days <-difftime(sens2ref$Valid.END[i], sens2ref$Valid.IN[i], units = "days") 
                    diff.days <-as.double(diff.days)
                    if (diff.days < system.cal) {
                        cat(sprintf(paste0("NOTE: Dates Mode changed from system  to NULL -- retrieval time less that %s days"), system.cal))
                        # NULL datetime option
                        sens2ref$UserDateINCal[i]   <- ""
                        # UserDateENDCal  <- "" # This is not useful for now as we do not make the difference between UserDateINCal and UserDateINCal
                    }
                    # Convert when User does not defines dates for calibration empty string or "System"
                    if (sens2ref$UserDateINCal[i] == "" ) { # I removed & UserDateENDCal == ""that we so not check for now
                        cat("[SETTIME] INFO, SET TIME parameters for Calibration functions for NULL dates")
                        sens2ref$DateCal.IN[i]    <- sens2ref$Valid.IN[i]
                        sens2ref$DateCal.END[i]   <- sens2ref$Valid.END[i]
                    } else if (sens2ref$UserDateINCal[i] == "System") { 
                        cat("[SETTIME] INFO, SET TIME parameters for Calibration functions system dates")
                        sens2ref$DateCal.IN[i]  <- sens2ref$Valid.IN[i]
                        sens2ref$DateCal.END[i] <- sens2ref$DateCal.IN[i] + days(system.cal)
                    } 
                }
                
                #------------------------------------------------------------------------------CR
                # Set initial and final dates to start measuring functions values for UserDateINmeas (after calibration)
                #------------------------------------------------------------------------------CR
                if (sens2ref$UserDateINmeas[i] != "System" & sens2ref$UserDateINmeas[i] != "") {
                    #UserDateENDmeas    <- as.character(now(tzone = TimeZone)- 60) # MGV set it to "2016-01-01 00:00", I hope now() will more convenient. 
                    # If we use now() rather than now()-60sec, UserDateENDmeas is later than UserDateEND.0, which returns an error wehn checking date consistency in main code
                    sens2ref$DateMeas.IN[i]  <- as.POSIXct(sens2ref$UserDateINmeas[i] , format = "%Y-%m-%d %H:%M", tz =TimeZone )
                    sens2ref$DateMeas.END[i] <- as.POSIXct(sens2ref$UserDateENDmeas[i], format = "%Y-%m-%d %H:%M", tz =TimeZone )
                } else {
                    #------------------------------------------------------------------------------CR
                    # Set dates to start measuring functions when User does not defines dates for calibration empty string or "System"
                    #------------------------------------------------------------------------------CR
                    if (sens2ref$UserDateINmeas[i] == "" ) { 
                        cat("[SETTIME] INFO, SET TIME parameters for Measuring functions for NULL dates")
                        sens2ref$DateMeas.IN[i]  <- sens2ref$DateIN[i]
                        sens2ref$DateMeas.END[i] <- sens2ref$Valid.END[i]
                    } else if (sens2ref$UserDateINmeas[i] == "System") { 
                        cat("[SETTIME] INFO, SET TIME parameters for Measuring functions system dates")
                        sens2ref$DateMeas.IN[i]  <- sens2ref$DateCal.END[i]
                        sens2ref$DateMeas.END[i] <- sens2ref$Valid.END[i]
                    } 
                }
                
                #------------------------------------------------------------------------------CR
                # Set time zone to UTC  and Check dates consistance
                #------------------------------------------------------------------------------CR
                attr(sens2ref[,c("Valid.IN")]     , "tzone") <- "UTC"
                attr(sens2ref[,c("Valid.END")]    , "tzone") <- "UTC"
                attr(sens2ref[,c("DateCal.IN")]  , "tzone") <- "UTC"
                attr(sens2ref[,c("DateCal.END")] , "tzone") <- "UTC"
                attr(sens2ref[,c("DateMeas.IN")] , "tzone") <- "UTC"
                attr(sens2ref[,c("DateMeas.END")], "tzone") <- "UTC"
                
                if (sens2ref$UserDateIN[i]      != "" & sens2ref$UserDateIN[i]      != "System"  &
                   sens2ref$UserDateINCal[i]   != "" & sens2ref$UserDateINCal[i]   != "System"  &
                   sens2ref$UserDateINmeas[i]  != "" & sens2ref$UserDateINmeas[i]  != "System") {
                    cat("[SETTIME] Info, Checking dates consistancy for User defined dates\n")
                    if (sens2ref$Valid.IN[i]   >= sens2ref$Valid.END[i])       stop("[SETTIME] ERROR, Valid.IN.0 >= Today ")
                    if (sens2ref$DateCal.IN[i]  >= sens2ref$DateCal.END[i])  stop("[SETTIME] ERROR DateCal.IN >= DateCal.END ")
                    if (sens2ref$DateMeas.IN[i] >= sens2ref$DateMeas.END[i]) stop("[SETTIME] ERROR DateMeas.IN >= DateMeas.END ")
                    if (sens2ref$DateCal.IN[i]  < sens2ref$Valid.IN[i] | sens2ref$DateCal.END[i] > sens2ref$Valid.END[i] ) {
                        cat(paste0(" Valid.IN.0: ",sens2ref$Valid.IN[i],"  Valid.END.0: ",sens2ref$Valid.END[i], "   DateCal.IN: ",sens2ref$DateCal.IN[i],"  DateCal.END: ",sens2ref$DateCal.END[i]), sep = "\n")
                        stop("[SETTIME] ERROR Calibration dates does NOT stand within retrieval range ")
                    }
                    if (sens2ref$DateMeas.IN[i] < sens2ref$Valid.IN[i] | sens2ref$DateMeas.END[i] > sens2ref$Valid.END[i] ) {
                        cat(paste0("  Valid.IN.0: ",sens2ref$Valid.IN[i]," Valid.END.0: ",sens2ref$Valid.END[i],"  DateMeas.IN: ",sens2ref$DateMeas.IN[i],"  DateMeas.END: ",sens2ref$DateMeas.END[i] ), sep = "\n")
                        stop("[SETTIME] ERROR Dates for Modelling NOT within retrieval range ")
                    }
                    cat("[SETTIME] INFO, periods of available data, calibration period and extrapollation method are consistent\n")
                } 
                
                #------------------------------------------------------------------------------CR
                # reporting of the periods of available data, calibration period and extrapollation method
                #------------------------------------------------------------------------------CR
                cat(paste0("Sensor: ", sens2ref$name.sensor[i]), sep = "\n")
                cat(paste0("[SETTIME] INFO: available data from Valid.IN     = " , format(sens2ref$Valid.IN[i]    , "%Y-%m-%d %H:%M:%S"), " to Valid.END     = " , format(sens2ref$Valid.END[i]    , "%Y-%m-%d %H:%M:%S")), sep = "\n")
                cat(paste0("[SETTIME] INFO: calibration    from DateCal.IN  = "  , format(sens2ref$DateCal.IN[i] , "%Y-%m-%d %H:%M:%S"), " to DateCal.END  = " , format(sens2ref$DateCal.END[i] , "%Y-%m-%d %H:%M:%S")), sep = "\n")
                cat(paste0("[SETTIME] INFO: extrapolation  from DateMeas.IN = "  , format(sens2ref$DateMeas.IN[i], "%Y-%m-%d %H:%M:%S"), " to DateMeas.END = " , format(sens2ref$DateMeas.END[i], "%Y-%m-%d %H:%M:%S")), sep = "\n")
                
                if (any(grepl(pattern = "General.prev", x = objects(DownloadSensor)))) {
                    remove(DownloadSensor)
                }
            } 
            
            # Saving config file
            write.table(t(sens2ref), file = file.path(DisqueFieldtestDir,"General_data",paste0(ASE_name,"_SETTIME.cfg")), col.names = TRUE, row.names = TRUE)
        }
        
        # Selecting valid period of Sensors? (valid because ASE is at the station?)
        if (!any(names(sens2ref) %in% "Sens.Inval.Out")) sens2ref$Sens.Inval.Out  <- c(TRUE             , TRUE          ,TRUE               , TRUE      , NA        , NA       )
        
    }
    
    
    #=====================================================================================CR
    # 14 - Valid periods
    #=====================================================================================CR
    # if (exists("General.t.Valid")) { # General.t.Valid
    #     # reading the files with period of valid data
    #     for (i in 1:4) {
    #         nameFile <- file.path(DisqueFieldtestDir,"General_data",paste0(ASE_name,"_Valid","_",sens2ref.Covariates$name.sensor[i],".cfg"))
    #         if (file.exists(nameFile)) {
    #             cat(paste0("[SETTIME] INFO, the file with valid periods of sensor data ", nameFile, " exists "), sep = "\n")
    #             assign(paste0("Valid_",sens2ref.Covariates$name.sensor[i]), read.table(file = nameFile, header = TRUE, row.names = NULL, comment.char = "#", stringsAsFactors = FALSE))
    #             cat(paste0("[SETTIME] INFO, ", get(paste0("Valid_",sens2ref.Covariates$name.sensor[i]))), sep = "\n")
    #         } else {
    #             cat(paste0("[SETTIME] INFO, the files with valid periods of sensor data ", nameFile, " do not exist. Set validity to the whole available time interval"), sep = "\n")
    #             assign(paste0("Valid_",sens2ref.Covariates$name.sensor[i]), rbind(c(strftime(min(General.t.Valid$date, na.rm = TRUE)), strftime(max(General.t.Valid$date, na.rm = TRUE)))))
    #             write.table(data.frame(In = gsub(" UTC", "",strftime(min(General.t.Valid$date, na.rm = TRUE))), 
    #                                    End = gsub(" UTC", "",strftime(max(General.t.Valid$date, na.rm = TRUE))), stringsAsFactors = FALSE), 
    #                         file = nameFile, row.names = FALSE)
    #         }
    #     }
    #     
    #     #if (all(exists(paste0("Valid_",sens2ref.Covariates$name.sensor))))
    #     Valid <- list()
    #     for (i in paste0("Valid_",sens2ref.Covariates$name.sensor)) Valid[[i]] <- get(i)
    #     
    #     NewValid<-function(x) {
    #         # making each element a dataframe of POSIXct
    #         x <-data.frame( x, stringsAsFactors = FALSE)
    #         colnames(x)<- c("In", "End")
    #         x$In  <- parse_date_time(x$In , tz = Ref.TZ, orders = "YmdHMS") #ymd_hm(x$In , tz = Ref.TZ) # as.POSIXct(strptime(x$In , "%Y-%m-%d %H:%M"), tz = Ref.TZ)
    #         x$End <- parse_date_time(x$End, tz = Ref.TZ, orders = "YmdHMS") #ymd_hm(x$End, tz = Ref.TZ) # as.POSIXct(strptime(x$End, "%Y-%m-%d %H:%M"), tz = Ref.TZ)
    #         return(x)
    #     }
    #     Valid.date <- lapply(Valid, NewValid)
    #     
    #     # rewriting files, not necessary nothing has been changed
    #     # for (i in 1:4) {
    #     #     nameFile <- file.path(DisqueFieldtestDir,"General_data",paste0(ASE_name,"_Valid",i,".cfg"))
    #     #     write.table(data.frame(In = as.character(format(Valid.date[[1]][1], "%Y-%m-%d %H:%M:%S")), 
    #     #                            End = as.character(format(Valid.date[[1]][2], "%Y-%m-%d %H:%M:%S")), 
    #     #                            stringsAsFactors = FALSE), 
    #     #                 file = nameFile, row.names = FALSE)
    #     # }
    #     
    #     cat("[SETTIME] INFO, SET VALID TIME parameters\n")
    #     # Set inital date for data retrieving (i.e. maximum length time period for data retrieval). 
    #     # These dates may change according to the data availability
    #     # UserDateIN.0, SOS.TZ is set in ASEConfig_MG.R
    #     # Set correct time zone
    #     
    #     #------------------------------------------------------------------------------CR
    #     # Valid Period to be used for sensor Evaluation with reference values, a life cycle of each sensor is needed - time zone shall be the same as SOS.TZ (UTC?)
    #     #------------------------------------------------------------------------------CR
    #     # Create the df of Valid date
    #     for (i in 1:4) if (exists("Start")) Start <- min(Start, min(Valid.date[[i]]$In )) else Start <- min(Valid.date[[i]]$In )
    #     for (i in 1:4) if (exists("End"))   End   <- max(End  , max(Valid.date[[i]]$End)) else End   <- max(Valid.date[[i]]$End)
    #     General.t.Valid <- selectByDate(General.t.Valid, start = Start, end = End)
    #     
    #     #browser()
    #     # list of valid date per sensor
    #     # seting invalid to NA and create a list for plotting invalids
    #     ind.Invalid <- list()        
    #     for (i in gsub(pattern = "Valid_", replacement = "", names(Valid.date))) { 
    #         for (j in 1:nrow(Valid.date[[paste0("Valid_",i)]])) {
    #             # dates before In date of first row
    #             if (j == 1) { # data before the first rows of valid dates
    #                 
    #                 if (!is.null(General.t.Valid$date < Valid.date[[paste0("Valid_",i)]]$In[j])) {
    #                     General.t.Valid[which(General.t.Valid$date < Valid.date[[paste0("Valid_",i)]]$In[j]),
    #                                     grep(pattern = sens2ref.Covariates[sens2ref.Covariates$name.sensor == i,"gas.sensor"], names(General.t.Valid), value = TRUE)]  <- NA
    #                     ind.Invalid[[i]] <- General.t.Valid[which(General.t.Valid$date < Valid.date[[paste0("Valid_",i)]]$In[j]), "date"]
    #                 } 
    #                 
    #             }  else if (j > 1 & j <= nrow(Valid.date[[paste0("Valid_",i)]])) { # data between 2 rows of valid dates
    #                 
    #                 if (!is.null(which(General.t.Valid$date < Valid.date[[paste0("Valid_",i)]]$In[j] & General.t.Valid$date > Valid.date[[paste0("Valid_",i)]]$End[j-1]))) {
    #                   
    #                     General.t.Valid[which(General.t.Valid$date < Valid.date[[paste0("Valid_",i)]]$In[j] & General.t.Valid$date > Valid.date[[paste0("Valid_",i)]]$End[j-1]),
    #                                     grep(pattern = sens2ref.Covariates[sens2ref.Covariates$name.sensor == i,"gas.sensor"], names(General.t.Valid), value = TRUE)] <- NA 
    #                     if (!(i %in% names(ind.Invalid))) {
    #                         ind.Invalid[[i]] <- General.t.Valid[which(General.t.Valid$date < Valid.date[[paste0("Valid_",i)]]$In[j] & 
    #                                                                       General.t.Valid$date > Valid.date[[paste0("Valid_",i)]]$End[j-1]),"date"]  
    #                     } else {
    #                         ind.Invalid[[i]] <- c(ind.Invalid[[i]], General.t.Valid[which(General.t.Valid$date < Valid.date[[paste0("Valid_",i)]]$In[j] & 
    #                                                                                       General.t.Valid$date > Valid.date[[paste0("Valid_",i)]]$End[j-1]),"date"])
    #                     }
    #                 } 
    #                 
    #             } else if (j == nrow(Valid.date[[paste0("Valid_",i)]])) { # dates after the End f the last row
    #               
    #                 if (!is.null(which(General.t.Valid$date > Valid.date[[paste0("Valid_",i)]]$End[j]))) {
    #                   
    #                     General.t.Valid[which(General.t.Valid$date > Valid.date[[paste0("Valid_",i)]]$End[j]),
    #                                     grep(pattern = sens2ref.Covariates[sens2ref.Covariates$name.sensor == i,"gas.sensor"], names(General.t.Valid), value = TRUE)] <- NA
    #                     if (!(i %in% names(ind.Invalid))) {
    #                         ind.Invalid[[i]] <- General.t.Valid[which(General.t.Valid$date > Valid.date[[paste0("Valid_",i)]]$End[j]),"date"]  
    #                     } else {
    #                         ind.Invalid[[i]] <- c(ind.Invalid[[i]], General.t.Valid[which(General.t.Valid$date > Valid.date[[paste0("Valid_",i)]]$End[j]),"date"])
    #                     }
    #                     
    #                 } 
    #             } 
    #             
    #         }
    #     }
    # }
    
    cat("-----------------------------------------------------------------------------------\n")
    #return(list(sens2ref, Valid.date, General.t.Valid,ind.Invalid))
    #browser()
    return(list(sens2ref))
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
    out <- scan(con,n,what= "char(0)",sep= "\n",quiet=TRUE)
    
    while(TRUE) {
        tmp <- scan(con,1,what= "char(0)",sep= "\n",quiet=TRUE)
        if (length(tmp) ==0) {close(con) ; break }
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
    # browser()
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
    #browser()
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
   
    #browser() 
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
Target.Diagram <- function(Sensor_name, Mat, uxi = NULL, b0 = NULL, b1 = NULL,  Unit.Ref = NULL,
                           xAxisLabel = NULL, yAxisLabel = NULL, Xlim = NA, Ylim = NA,
                           DQO.I = NA, DQO.M = NA, DQO.O = NA, LAT = NA, UAT = NA, LV = NA, AT = NA, CL = NA,
                           Disk = NA, WD = NA, Dir = NA, sdm_sdo = FALSE, SavePlot = TRUE) {
    
    # Sensor_name               : name of the sensor to be written in front of the calibration equation. If NULL, do not print sensor name.
    # Mat                       : DataFrame of data including Case number, Date, xis, y, optional uxi if uxi is not constant for all reference values,
    #                             "Rel.bias", "Rel.RSS". "Rel.bias", "Rel.RSS", "xis" msut be included into dataFrame Mat.
    # uxi                       : numeric (default = NULL ), random standard uncertainty of the results of the reference method, xis, given as a constant value for all xis reference values
    # b0 , b1                   : numeric, intercept and slope of the orthgonal regression, default: NULL. If not NULL b0/xi and b1 - 1 are plotted
    # Unit.Ref                  : Character, default is NULL. Unit of refrence values, can "ppm", "ppb", "ug/m3", "mg/m3" ...
    # Xlabel, Ylabel            : label On the x And y axis
    # Xlim, Ylim                : limits of x and y axis, default values is NA, vectors of two values min and max values
    # Title                     : title to appear On the top of the scatter plot of x And y values
    # DQO.I, DQO.M, 
    # DQO.O                     : numeric, data qualtiy objective for Indicative measurements, Modelling and objective estimation. The DQOs are  expressed in percentage, 
    #                             defaul NA, if NA no DQO target circle is plotted
    # LAT, UAT, LV, 
    # AT, CL                    : numeric, lower and upper assessment threshold, limit value, Alert threshold and Critical level of the European Air Quality Directive for Mat$xis, 
    #                             same unit as Mat$xis, default value = NA, used for color scale and target circles
    # Units                     : character vector, units for the expanded uncertainty, xis, yis
    # Disk, WD, Dir             : where you put the graphic files (Disk, working directory, directory), It is sufficient if only Dir is supplied
    # lim                       : passing variable for the limits of the Etalonnage function (cbind(c(minX,maxX),c(minY,maxY)) or NULL)
    # variable.uxi              : logical, if FALSE (default = FALSE ), uxi is used as constant random standard uncertainties for all xis reference values. 
    #                             If TRUE uxi given in Mat and is used for each reference values
    # f_coef1, f_coef2, f_R2    : number of digit for intercept, slope and R2 using sprintf syntax. 
    #                             f_coef1 is used both for intercept and s(Res), f_coef2 is used for all parameters of the model apart from the intercept
    # nameModel                 : name of model to be used to save uncertainty plots, character, default NULL
    # SavePlot                  : logical, default is TRUE if TRUE uncertainty plts are saved
    # Disk, WD, Dir             : where you put the graphic files (Disk, working directory, directory), It is sufficient if only Dir is supplied
    # sdm_sdo                   : logical, TRUE id the standard devaition of sensor measurements or model is lower than the one of eference measurements (observation)
    # SavePlot                  : logical, default is TRUE if TRUE uncertainty plts are saved
    
    # return                    : nothing, target diagram is savved as png file
    
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
    
    #browser()
    #checking that Mat is dataFrame
    if (class(Mat) != "data.frame") {
        
        return("Mat is not of dataFrame class. Returning NAs.")
        
    } else {
        
        #checking that the mat dataFrame is not empty, considering only the complete cases
        Mat <- Mat[complete.cases(Mat),]
        if (nrow(Mat) < 0) {
            
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
                Max.percent <- 150
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
                        
                    } else {
                        
                        Index.Good <- NULL
                    }
                    
                } else {
                    
                    Index.Good <- NULL
                    
                }
                
                if (all(is.na(Xlim))) {
                    
                    if (!is.null(Index.Good)) {
                        
                        Xlim <- c(max(-Max.percent,min(Mat$b1[Index.Good],Mat$b0[Index.Good],XData[Index.Good])), min(Max.percent, max(DQO.O, max(XData[Index.Good]))))    
                        
                    } else {
                        
                        Xlim <- c(0, min(Max.percent, DQO.O))
                    }
                } 
                if (all(is.na(Ylim))) {
                    
                    if (!is.null(Index.Good)) {
                        
                        Ylim <- c(max(-Max.percent,min(0, min(YData[Index.Good]))), min(Max.percent, max(DQO.O, max(YData[Index.Good])))) 
                        
                    } else {
                        
                        Ylim <- c(0, min(Max.percent, DQO.O)) 
                    }
                    
                } 
                #=============CR
                
                #=====[3]=====
                # Pick out which of your columns have your label data that you would like to encode as coloring of the points on the scatterplot.
                # Type the names of that column between the double-quotes below (leaving the other code untouched).
                # Levels include Which Targets are not NULL
                Levels       <- c(LAT, UAT, LV, AT ,CL)[which(!is.na(c(UAT,LAT,LV, AT,CL)))]
                factor.Color <- pretty(Mat[,"xis"], n = (10 - length(levels)))
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
                if (!is.null(Sensor_name)) MainTitle = paste0(Sensor_name, " - Target Diagram - Relative expanded uncertainty") else MainTitle = "Target Diagram - Relative expanded uncertainty"
                #=============CR
                
                #=====[axis labels]=====
                # In the quotes after 'xAxisLabel', type the words(s) you want to see displayed beneath the x (horizontal) axis.
                # In the quotes after 'yAxisLabel', type the word(s) you want to see displayed to the left of the y (vertical) axis.
                # If you don't want to include an axis label at all for either of these, just leave the double-quotes empty, i.e. "", for that one.
                if (is.null(xAxisLabel)) xAxisLabel = c("Random effect: 2((RS/(n-2) - u(xi)^1/2)/xi in %")
                if (is.null(yAxisLabel)) yAxisLabel = c("Bias: 2(b0/xi + (b1 - 1)) in %")
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
                Mat$col <- colfunc(nrow(Mat))
                #=============CR
                
                #=====[PointSizeModifier]=====
                # If you would like your points to be bigger or smaller, the following lets you adjust their size.
                # '1' represents 100% of the default size, so if you want to make the points larger you could type, for example, 1.5 (i.e. 150%). Similarly, if you want to make them smaller you could type 0.5 (i.e. 50%).
                PointSizeModifier = 1
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
                par(mar=c(2.8,2.8,2.5,3.25), # mar=c(lines below, lines at left, lines at top, lines at right)
                    mgp=c(1.8, 0.4, 0),      # mgp=c(label, tick mar label, tick mark)
                    cex.axis = 0.8
                ) 
                on.exit(par(op))
                # browser()
                plot.default(x           = XData[if (!is.null(Index.Good)) Index.Good else 1], 
                             y           = YData[if (!is.null(Index.Good)) Index.Good else 1], 
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
                title(main = MainTitle, line = 1.5)
                
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
                    mtext(text = sprintf("y = b0 + b1 x, with b0 = %.3f and b1 = %.2f, u(xi) = %.3f", b0, b1, uxi),
                          side = 3, 
                          line = 0.1, 
                          cex  = 0.8
                    )
                } else {
                    mtext(text = sprintf("y = b0 + b1 x, with b0 = %.1f and b1 = %.2f, u(xi) = %.1f", b0, b1, uxi),
                          side = 3, 
                          line = 0.1, 
                          cex  = 0.8
                    )
                }
                
                # Contribution of b1 : (b1 - 1)
                points(x = Mat$b1[if (!is.null(Index.Good)) Index.Good else 1], 
                       y = YData[if (!is.null(Index.Good)) Index.Good else 1], 
                       col = Mat$col, 
                       pch  = "-",
                       cex  = 0.4
                )
                # Contribution of b0 : b0/x
                points(x = Mat$b0[if (!is.null(Index.Good)) Index.Good else 1], 
                       y = YData[if (!is.null(Index.Good)) Index.Good else 1], 
                       col = Mat$col, 
                       pch  = "-",
                       cex  = 0.4
                )
                # draw axis
                abline(h=0)
                abline(v =0)
                #=============CR
                
                #=====[colorscale]=====
                if (!is.null(Index.Good)) {
                    
                    # Add colorscale
                    LegendTitle = "x values"
                    image.plot(bigplot      = usr,
                               legend.only  = TRUE, 
                               zlim         = range(Mat[Index.Good,"xis"]),
                               col          = Mat[Index.Good,"col"],
                               legend.args  = list(text = LegendTitle, side = 3, cex = 1, line = 0, srt = 0, adj = 0),
                               legend.width = 0.5,
                               legend.mar   = 3.6,
                               verbose      = FALSE
                               #smallplot    = c(usr[2]-10,0, 10,Ylim[2])
                    )
                }
                #=============CR
                
                #=====[target circles]=====
                for (i in c(DQO.I*100, DQO.M*100, DQO.O*100)) {
                    if (!is.na(i)) {
                        cercle <- rbind(cbind(seq(-i, i, by =  i/1000),  sqrt(i^2-seq(-i,i, by = i/1000)^2)),
                                        cbind(seq( i,-i, by = -i/1000), -sqrt(i^2-seq(-i,i, by = i/1000)^2)))
                        lines(cercle,type = "l")
                        text(x = 0, y = i + 1, paste0(i," %"), pos = 4) # pos of the label at rigth of the coordinate
                        
                    }
                }
                # adding circles for Ur > DQO.O
                if (sqrt(usr[2]^2 + usr[4]^2) > DQO.O) {
                    
                    # determining DQO steps
                    DQO.step <- min(c(DQO.M*100 - DQO.I*100, DQO.O*100 - DQO.M*100))
                    DQO.max  <- max(c(DQO.I*100, DQO.M*100, DQO.O*100))
                    while(sqrt(usr[2]^2 + usr[4]^2) > DQO.max + DQO.step) {
                        DQO.max <- DQO.max + DQO.step
                        cercle <- rbind(cbind(seq(-DQO.max, DQO.max, by =  DQO.max/1000),  sqrt(DQO.max^2-seq(-DQO.max,DQO.max, by = DQO.max/1000)^2)),
                                        cbind(seq( DQO.max,-DQO.max, by = -DQO.max/1000), -sqrt(DQO.max^2-seq(-DQO.max,DQO.max, by = DQO.max/1000)^2)))
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
                #browser()
                
                # checking that data is visible
                if (!is.null(Index.Good)) {
                    
                    # Select last quartile of these 90 % the biggest Ur within Index.Good
                    Index.Big.Ur <- which(Mat[Index.Good,"Ur"] < 0.90 * max(Mat[Index.Good,"Ur"]) & Mat[Index.Good,"Ur"] > 0.75 * max(Mat[Index.Good,"Ur"]))
                    
                    # Absolute difference between Angle of Ur and diagonal 45 and -45 degress
                    Mat$Angle45  <- abs(atan(YData/XData) * 180 /pi  -   45 )
                    Mat$Angle_45 <- abs(atan(YData/XData) * 180 /pi  - (-45))
                    
                    # Selecting best point for plotting arrows
                    if (min( Mat[Index.Good,"Angle45"][Index.Big.Ur], na.rm = T) < min(Mat[Index.Good,"Angle_45"][Index.Big.Ur], na.rm = T)) {
                        Index.med.UR <- which(Mat[,"Angle45"]   == min(Mat[Index.Good,"Angle45"][Index.Big.Ur] ))[1] 
                    } else Index.med.UR <- which(Mat[,"Angle_45"]  == min(Mat[Index.Good,"Angle_45"][Index.Big.Ur]))[1]
                    
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
                    Arrows(x0 = 0              , y0 = YData[Index.med.UR], 
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
                    Arrows(x0 = 0              , y0 = 0, 
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
                         labels = paste0("2(b1-1)"), 
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
                    #browser()
                    if (b0 > 0) {
                        
                        if (any(Mat$b0 <= Xlim[2])) {
                            
                            # There are Mat$b0 <= Xlim[2]
                            Index.Good.b0 <- which(Mat[Index.Good, "b0"] <= Xlim[2])
                            x.b0          <- max(Mat[Index.Good, "b0"][Index.Good.b0])
                            y.b0          <- Mat[Mat$b0 == x.b0, "Rel.bias"][1] * 100 
                            text(x = x.b0, 
                                 y = y.b0, 
                                 labels = c("2 b0/x in %"),
                                 srt = atan(y.b0 /(x.b0 + Mat$b1[1])) * 180 /pi,
                                 pos = 2
                            )
                        } 
                    } else {
                        
                        if (any(Mat$b0 >= Xlim[1])) {
                            
                            # There are Mat$b0 >= Xlim[1]
                            Index.Good.b0 <- which(Mat[Index.Good, "b0"] >= Xlim[1])
                            x.b0          <- min(Mat[Index.Good.b0, "b0"])
                            y.b0          <- Mat[Mat$b0 == x.b0, "Rel.bias"][1] * 100 
                            text(x = x.b0, 
                                 y = y.b0, 
                                 labels = c("2 b0/x in %"),
                                 srt = atan(y.b0 /(x.b0 + Mat$b1[1])) * 180 /pi,
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
                }
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
                #=============CR
                
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


# Distribution_Ref <- function(RefData, Date_Begin = NULL, Date_End = NULL, Sensor_dates = NULL, Min_APS = NULL, Max_APS = NULL) {
#     # RefData:      a dataframe that includes all the reference data (dates, coordinates, gas, PM mass concentration, bins ...) 
#     #               The binned counts and diameters shall not be log transformed. Units of diameters: working with micrometers 
#     #               and other units were not tested
#     
#     # Date_Begin,   The start and ending selected dates in POSIXCt format. Default values are NULL, 
#     # Date_End      It is possible to have only one date limit, Date_Begin or Date_End. Date_Begin or Date_End are not discarded.
#     # Sensor_dates  vector of POSIXCt: the dates for which the PM sensor provides binned counts. 
#     #               In case the sensor PM data series is not complete, dates with missing PM sensor data will be discared from RefData
#      
#     # Min_APS,      Numeric, Minimum and maximum diameters of reference counts to be selected 
#     # Max_APS       for reference counts. Default values are NULL. It is possible to have only one diameter limit,  Min_APS or Max_APS.
#     #               Min_APS and Max_APS are discarded.
#     
#     # Return        a list with datafame RefData (only selected dates and names of bins corresponding to selected diameters),
#     #               datafame counts_Ref with column diameters and mean counts over the selected dates
#     #               vector with selected APS diameters
#     #               vector with DMPS diameter
#     #                       
#     # select only columns containing .Bin.DMPS & .Bin.APS
#     
#     # Selecting dates
#     if (!all(is.null(c(Date_Begin, Date_End)))) {
#         
#         if (!(is.null(Date_Begin))) RefData <- RefData %>% filter(date >= Date_Begin)
#         if (!(is.null(Date_End)))   RefData <- RefData %>% filter(date <= Date_End)
#     }
#     if (!is.null(Sensor_dates))  RefData <- RefData %>% filter(date <= Sensor_dates)
#     
#     # browser()
#     # Vector of diameters measured by the APS instrument
#     Diam_APS <- names(RefData)[grep(pattern = "Bin.APS.", x = names(RefData) )] %>%
#         gsub(pattern = "Bin.APS.", replacement = "", x = .) %>%
#         as.numeric
#     
#     ### APS diameters to be dropped, i. e. <= 0.89 | >= 11.97
#     if (!all(is.null(c(Min_APS, Max_APS)))) {
#         
#         Diam_APS_to_remove <- numeric()
#         if (!(is.null(Min_APS))) Diam_APS_to_remove <- Diam_APS[Diam_APS <= Min_APS]
#         if (!(is.null(Max_APS))) Diam_APS_to_remove <- c(Diam_APS_to_remove, Diam_APS[Diam_APS >= Max_APS])
#         # Discarding diameters if any diameters < Min_APS or > Max_APS
#         if (length(Diam_APS_to_remove) > 0) {
#             
#             # Discarding APS diameters from Diam_APS and RefData
#             Diam_APS           <- Diam_APS[-which(Diam_APS %in% Diam_APS_to_remove)] 
#             Diam_APS_to_remove <- paste0("Bin.APS.", Diam_APS_to_remove)
#             RefData            <- RefData[ , !names(RefData) %in% Diam_APS_to_remove]
#         }
#     }
#     
#     # Vector of diameters measured by the DMPS instrument
#     Diam_DMPS <- names(RefData)[grep(pattern = "Bin.DMPS.", x = names(RefData) )] %>%
#         gsub(pattern = "Bin.DMPS.", replacement = "", x = .) %>%
#         as.numeric
#     
#     ### Remove "Bin.DMPS." and "Bin.APS." from column names of RefData
#     names(RefData) = gsub(pattern = paste0(c("Bin.DMPS.", "Bin.APS."),collapse = "|"), 
#                                     replacement = "", x = names(RefData))
#     
#     # change with colmeans
#     diameter_names <- as.character(c(Diam_DMPS,Diam_APS))
#     Ref_Bins       <- RefData[, which(names(RefData) %in% diameter_names)]
#     
#     counts_Ref <- colMeans(Ref_Bins, na.rm = T)
#     #counts_Ref <- t(counts_Ref)
#     counts_Ref <- as.data.frame(counts_Ref)
#     counts_Ref$diameters <- rownames(counts_Ref)
#     rownames(counts_Ref) <- NULL
#     names(counts_Ref) <- c("counts", "diameters")
#     
#     
#     # transpose data by bin
#     Ref_Bins <- gather(Ref_Bins, "diameters", "counts")
# 
#     # Mean of all counts in each bin, filtered for counts with zero values
#     counts_Ref <- Ref_Bins %>%
#         dplyr::group_by(diameters) %>%
#         dplyr::summarise(counts = mean(counts, na.rm = T)) %>%
#         dplyr::mutate(diameters = as.numeric(diameters)) %>%
#         dplyr::arrange(diameters) %>%
#         dplyr::filter(counts > 0)
#     
#     # Convert to data frame
#     counts_Ref <- as.data.frame(counts_Ref) 
#     
#     return(list(RefData_filtered = RefData, counts_Ref = counts_Ref, Diam_DMPS = Diam_DMPS, Diam_APS = Diam_APS))
#     
# }


Plot_Dist_Ref_log <- function(counts_Ref, Model.i = NULL, Count) {

    # browser()    
    # Plotting distribution in log and modelled distribution if Model.i is not NULL
   
    # changes names of count_ref in case it uses x and y as for fitting model for density determination
    if (any(c("x", "y") %in% names(counts_Ref))) names(counts_Ref) <- c("diameters", "counts")
    
    if  (is.null(Model.i)) {
        
     plot <-  ggplot() + 
        theme_bw() +
        geom_point(data = counts_Ref, aes(log10(diameters), log10(counts)), stat = "identity", fill = "gray") +
      #   geom_line(data = augmented, aes(exp(x), exp(.fitted),  col = "Modelled"), size = 2) +
        theme(axis.title.x = element_text(colour  = "black", size = 15),
              axis.text.x  = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15, colour = "black")) +
        theme(axis.title.y = element_text(colour = "black", size = 15),
              axis.text.y  = element_text(angle = 0, vjust = 0.5, size = 15, colour = "black")) +
        xlab(expression(paste("log10 of diameter (?m)"))) + 
        ylab(expression(paste("log10 of counts / log of diameters"))) 
     
  } else {
      
      plot <-  ggplot() + 
          theme_bw() +
          geom_point(data = counts_Ref, aes(log10(diameters), log10(counts), col = "Ref"), stat = "identity", fill = "gray") +
          geom_line(data = Model.i$Augment, aes((x), (.fitted),  col = "Modelled"), size = 1) +
          scale_color_manual(values = c("Modelled" = "blue", "Ref" = "black")) +
          theme(axis.title.x = element_text(colour = "black", size = 15),
                axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size = 15, colour = "black")) +
          theme(axis.title.y = element_text(colour = "black", size = 15),
                axis.text.y  = element_text(angle=0, vjust=0.5, size = 15, colour = "black")) +
          xlab(expression(paste("diameter (?m)"))) + 
          ylab(expression(paste("tot # of counts"))) 
  }
      
    return(plot)
} 

Plot_Dist_Vol_log <- function(Volume_Ref, Dist_OPC_Sensor, Model.i = NULL, Count, DateBegin = Start_Time, DateEnd = Stop_Time) {
    
    # browser()    
    # Plotting distribution in log and modelled distribution if Model.i is not NULL
    
    # changes names of count_ref in case it uses x and y as for fitting model for density determination
    if (any(c("x", "y") %in% names(Volume_Ref))) names(Volume_Ref) <- c("diameters", "volume")
    
    if  (is.null(Model.i)) {
        
        
        plot <-  ggplot() + 
            theme_bw() +
            geom_point(data = Volume_Ref, aes((diameters), (volume), col = "ref"), stat = "identity", fill = "gray") +
            # add points for the Volume of the OPC sensor
            geom_point(data = Dist_OPC_Sensor, aes((diameters), (volume),col = "OPC"), stat = "identity", fill = "gray") +
            #   geom_line(data = augmented, aes(exp(x), exp(.fitted),  col = "Modelled"), size = 2) +
            scale_color_manual(values = c("ref" = "red", "OPC" = "black")) +
            scale_x_continuous(trans='log10') +
            scale_y_continuous(trans='log10') +
            theme(axis.title.x = element_text(colour  = "black", size = 15),
                  axis.text.x  = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15, colour = "black")) +
            theme(axis.title.y = element_text(colour = "black", size = 15),
                  axis.text.y  = element_text(angle = 0, vjust = 0.5, size = 15, colour = "black")) +
            xlab(expression(paste("diameter (?m)"))) + 
            ylab(expression(paste("Volume  (" , cm^-3, ")"))) +
            ggtitle((paste("from ", format(DateBegin, "%y-%m-%d %H:%M"), " to ", format(DateEnd, "%y-%m-%d %H:%M")))) 
        
        png(paste0(output_folder,"Volume_OPC/", i, ".jpg"),
            width = 1200, height = 1050, units = "px", pointsize = 30,
            bg = "white", res = 150)
        print(plot)
        dev.off()
        
        
        
        
    } else {
        
        # plot <-  ggplot() +
        #   theme_bw() +
        #   geom_point(data = counts_Ref, aes(log10(diameters), log10(counts), col = "Ref"), stat = "identity", fill = "gray") +
        #   geom_line(data = Model.i$Augment, aes((x), (.fitted),  col = "Modelled"), size = 1) +
        #   scale_color_manual(values = c("Modelled" = "blue", "Ref" = "black")) +
        #   theme(axis.title.x = element_text(colour = "black", size = 15),
        #         axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size = 15, colour = "black")) +
        #   theme(axis.title.y = element_text(colour = "black", size = 15),
        #         axis.text.y  = element_text(angle=0, vjust=0.5, size = 15, colour = "black")) +
        #   xlab(expression(paste("diameter (µm)"))) +
        #   ylab(expression(paste("tot # of counts")))
    }
    
    return(plot)
} 


Plot_Dist_Ref <- function(counts_Ref, Model.i = NULL) {
    
    # Plotting distribution not in log of counts versus diameters and modelled distribution if Model.i is not NULL
    
    # browser()
    # changes names of count_ref in case it uses x and y as for fitting model for density determination
    if (any(c("x", "y") %in% names(counts_Ref))) names(counts_Ref) <- c("diameters", "counts")
    
    if  (is.null(Model.i)) {
        
        plot <-  ggplot() + 
            theme_bw() +
            geom_point(data = counts_Ref, aes((diameters), (counts)), stat = "identity", fill = "gray") +
            #   geom_line(data = augmented, aes(exp(x), exp(.fitted),  col = "Modelled"), size = 2) +
            theme(axis.title.x = element_text(colour  = "black", size = 15),
                  axis.text.x  = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15, colour = "black")) +
            theme(axis.title.y = element_text(colour = "black", size = 15),
                  axis.text.y  = element_text(angle = 0, vjust = 0.5, size = 15, colour = "black")) +
            xlab(expression(paste("diameter (µm)"))) + 
            ylab(expression(paste("counts per ml"))) 
        
    } else {
        
        plot <-  ggplot() + 
            theme_bw() +
            geom_point(data = counts_Ref, aes(log10(diameters), log10(counts), col = "Ref"), stat = "identity", fill = "gray") +
            geom_line(data = Model.i$Augment, aes((x), (.fitted),  col = "Modelled"), size = 1) +
            scale_color_manual(values = c("Modelled" = "blue", "Ref" = "black")) +
            theme(axis.title.x = element_text(colour = "black", size = 15),
                  axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size = 15, colour = "black")) +
            theme(axis.title.y = element_text(colour = "black", size = 15),
                  axis.text.y  = element_text(angle=0, vjust=0.5, size = 15, colour = "black")) +
            xlab(expression(paste("diameter (µm)"))) + 
            ylab(expression(paste("tot # of counts"))) 
    }
    
    return(plot)
} 

Density_Ref_log <- function(counts_Ref, Density, Mod_type, Diam_APS = NULL) {
    
    # counts_Ref         Dataframe with two columns, diameters in micrometers and mean counts per ml that are not log transform (real values).
    # Density            Numeric, inital mean density of particulate matter, i. e. : 1.5
    # Mod_type           Character, type of distribution that is fitted to PM distribution, can be 'Normal_density' or 'GAM_GAUSS', "Normal."
    #                    Look in Sensor_Toolbox for details
    # Diam_APS           numeric vector with the diameters of counts measured by the APS, used for correction with sqrt of density of particles           
    
    # Return            a model which fit the chosen distribution for the DMPS and APS for log10(counts_Ref$diameters),
    #                   and log10(counts_Ref$counts). With 'Normal_density' fit, the model determines the best density 
    #                   that allows continuity of DMPS and APS counts in decimal logarithm versus decimal logarithm of diameters
    
    # browser()
    # conversion to log to fit a log/log normal distribution
    
    if (counts_Ref == counts_Ref) {
    DataXY <- data.frame(x = log10(counts_Ref$diameters),
                         y = log10(counts_Ref$counts))
    }
    # Estimating mu by the mode where the count are maximum
    x.max <- which.max(counts_Ref$counts)
    MU    <- DataXY$x[x.max]
    
    # Estimating SIGMA, the standard deviation of the normal distribution using the lognormal distribution
    # initial values
    MeanLog <- counts_Ref$diameters[x.max]
    SdLog   <- (max(counts_Ref$diameters) - MeanLog)/2
    K = counts_Ref$counts[x.max]
    # Model.Log.Normal <- nlsLM(y ~ K * dlnorm(x, meanlog, sdLog), 
    #                           data    = DataXY, 
    #                           start   = list(meanlog = MeanLog, sdLog = SdLog, K = K),
    #                           model   = TRUE, trace = T)
    # SIGMA <- sqrt( 2 * (log(coef(Model.Log.Normal)[1]) - MU))
    
    
    # Estimating SIGMA, the standard deviation of the normal distribution as half height width (~ 50%)
    x.min          <- which.min(counts_Ref$counts)
    sigma_height   <- mean(c(DataXY$y[x.max] , DataXY$y[x.min])) 
    min_dist_sigma <- which.min(abs(sigma_height - DataXY$y))
    SIGMA          <- DataXY$x[min_dist_sigma] - MU
    
    # initial values
    C <- min(DataXY$y, na.rm = T)
    K = abs(DataXY$y[x.max] - DataXY$y[x.min])
    
    #browser()
    if (Mod_type == 'Normal_density') {
        Model <- nlsLM(y ~ K * f_normal_density(x, mu, sigma, Density) + C, 
                       data    = DataXY, 
                       start   = list(mu = MU, sigma = SIGMA, K = K , Density = Density, C = C), 
                       # lower = c(ifelse(MU < 0, 1.1 * MU, 0.9 * MU), 0.2 * SIGMA, 0.1  * K, 0.5, 0.1 * C), 
                       # upper = c(ifelse(MU < 0, 0.9 * MU, 1.1 * MU), 10  * SIGMA, 100  * K, 4.0, 5   * C),
                       control = nls.lm.control(ftol = (.Machine$double.eps),
                                                ptol = sqrt(.Machine$double.eps), gtol = 0, diag = list(), epsfcn = 0,
                                                factor = 100, maxfev = integer(), maxiter = 200, nprint = 0),
                       model   = TRUE, trace = T)
    } else if (Mod_type == 'Normal') {
        Model <- nlsLM(y ~ K * dnorm(x, mu, sigma) + C, 
                       data    = DataXY, 
                       start   = list(mu = MU, sigma = SIGMA, K = K, C = C), 
                       # lower = c(ifelse(MU < 0, 1.1 * MU, 0.9 * MU), 0.2 * SIGMA, 0.1  * K, 0.5, 0.1 * C), 
                       # upper = c(ifelse(MU < 0, 0.9 * MU, 1.1 * MU), 10  * SIGMA, 100  * K, 4.0, 5   * C),
                       # control = nls.lm.control(ftol = (.Machine$double.eps),
                       #                          ptol = sqrt(.Machine$double.eps), gtol = 0, diag = list(), epsfcn = 0,
                       #                          factor = 100, maxfev = integer(), maxiter = 200, nprint = 0),
                       model   = TRUE, trace = T)
    } else if (Mod_type == 'GAM_GAUSS') {
        Model <- gam(y~s(x), family = gaussian(link = identity), data = DataXY)
    }
    
    print(summary(Model))
    
    # fitted data
    Model.i <- list(Model = Model, Tidy = tidy(Model), Augment = augment(Model), Glance = glance(Model), Call = Model$call, Coef = coef(Model))
    #list.save(Model.i, file = file.path(WDoutputMod, paste0(nameModel,".rdata")))
    
    # Adding the intercept for normal density
    #if (Mod_type == 'Normal_density') Model.i$Augment$.fitted <- Model.i$Augment$.fitted + Model.i$Coef[5]
    # Correction of diameters of APS using the fitted density
    if (Mod_type == 'Normal_density') Model.i$Augment$x <- log10(Diam_APS_Corr(Diam_APS, 10^Model.i$Augment$x, Model.i ))
    
    # Plot of distribution of reference counts versus diameter in log/log with fitted density
    # Plot <- Plot_Dist_Ref_log(DataXY, Model.i = Model.i)
    # Plot
    
    return(Model.i)
}

Density_Ref <- function(counts_Ref, Density, Mod_type = "f_log10normal_density") {
    
    # counts_Ref    Dataframe with two columns, diameters in micrometers and mean counts per ml that are not log transform (real values).
    # Density            Numeric, inital mean density of particulate matter, i. e. : 1.5
    # Mod_type           Character, type of distribution that is fitted to PM distribution, can be 'f_log10normal_density'.
    #                    Look in Sensor_Toolbox for details
    
    # Return            a model which fit the chosen distribution for the DMPS and APS for log10(counts_Ref$diameters),
    #                   and log10(counts_Ref$counts). With 'Normal_density' fit, the model determines the best density 
    #                   that allows continuity of DMPS and APS counts in decimal logarithm versus decimal logarithm of diameters
    
    # conversion to log to fit a log/log normal distribution
    DataXY <- data.frame(x = (counts_Ref$diameters),
                         y = (counts_Ref$counts))
    
    # diameter @ max of counts
    x.max <- which.max(counts_Ref[,2])
    x.min <- which.min(counts_Ref[,2])
    MU <- DataXY$x[x.max]
    
    # half height width (~ 50%)
    sigma_height   <- 0.5*(DataXY$y[x.max] + DataXY$y[x.min] ) 
    min_dist_sigma <- which.min(abs(sigma_height - DataXY$y))
    SIGMA <- DataXY$x[min_dist_sigma] - MU
    
    # initial values
    K = abs(DataXY$y[x.max]) - abs(DataXY$y[x.min])
    
    #browser()
    if (Mod_type == 'f_log10normal_density') {
        Model <- nlsLM(y ~ K * f_log10normal_density(x, mu, sigma, Density), 
                       data    = DataXY, 
                       start   = list(mu = MU, sigma = SIGMA, K = K , Density = Density), 
                       lower = c(ifelse(MU<0,1.1 * MU, 0.9 * MU), 0.8 * SIGMA, 0.1  * K), 
                       upper = c(ifelse(MU<0,0.9 * MU, 1.1 * MU), 10  * SIGMA, 100  * K),
                       control = nls.lm.control(ftol = (.Machine$double.eps),
                                                ptol = sqrt(.Machine$double.eps), gtol = 0, diag = list(), epsfcn = 0,
                                                factor = 100, maxfev = integer(), maxiter = 200, nprint = 0),
                       model   = TRUE, trace = T)
    }
    if (Mod_type == 'GAM_GAUSS') {
        Model <- gam(y~s(x), family = gaussian(link = identity), data = DataXY)
    }
    
    print(summary(Model))
    
    # fitted data
    Model.i <- list(Tidy = tidy(Model), Augment = augment(Model), Glance = glance(Model), Call = Model$call, Coef = coef(Model))
    #list.save(Model.i, file = file.path(WDoutputMod, paste0(nameModel,".rdata")))
    
    # Adding the intercept for normal density
    if (Mod_type == 'Normal_density') Model.i$Augment$.fitted <- Model.i$Augment$.fitted + Model.i$Coef[5]
    
    # Plot of distribution of reference counts versus diameter in log/log with fitted density
    Plot_Dist_Ref(DataXY, Model.i = Model.i)
    
    return(Model.i)
}

# correction APS diamters with density
Diam_APS_Corr <- function(Diam_APS, Diam_Dist_Ref, density) {
    # Diam_Dist_Ref numeric vector, diameters in micrometers measured by APD and DMPS
    # Diam_APS:     Optical diameter of the APS instrument to be corrected with the square root of the particle density
    # Model.i:      Model representing the log normal fitted distribution
    
    # return        a numerical vector with all diameters including the the APS corrected diameter in micrometers
    
    #browser()
    Diam_APS_to_correct <- which(round(Diam_Dist_Ref, digit = 5) %in% round(Diam_APS, digit = 5))
    # Diam_Dist_Ref[Diam_APS_to_correct] <- Diam_Dist_Ref[Diam_APS_to_correct]/sqrt(Model.i$Coef[4])
    # use density obtained from the f_error
    Diam_Dist_Ref[Diam_APS_to_correct] <- Diam_Dist_Ref[Diam_APS_to_correct]/sqrt(density$minimum)
    return(Diam_Dist_Ref)
}

# OPC Sensors (OPCN3) ################################################
f_dcounts_ddp <- function(nbin, bins_OPC, n.bins_OPC, bins_diameters, Counts_units ) {
    # nbin:             name of current bin as "Bin0", "Bin1", ...
    # bins_OPC:         dataframe with counts (real values) for all bins,. Column names "Bin0", "Bin1", ...
    # n.bins_OPC        dataframe of one line of charaters with all bin names as "Bin0", "Bin1", ...
    # bins_diameters    dataframe of one line of numerical giving the starting diameters of all bins, + ending diameter of last bin
    # Counts_units:     Character, default is "dN/dlogDp" corresponding to the unit of the reference counts
    # return:           Numerical vector, with values of dN/dlogDp
    #browser()
    nbin.next <- paste0("Bin", as.numeric(sub(pattern = "Bin",replacement = "",nbin)) +1 )
    if (Counts_units == "dN/dlogDp") {
        return(as.vector(bins_OPC[,nbin] / log10( t(bins_diameters[nbin.next]) / t(bins_diameters[nbin])))) 
        # return(as.vector(bins_OPC[,nbin])) 
    }  else stop(cat("ERROR unknown units of reference counts to correct sensor counts\n"))
    
} 


Distribution_OPC_Sensor <- function(General.df, DateBegin= NULL, DateEnd=NULL, bins_diameters, Sensor_dates = NULL, 
                                    Dist_Ref.full = NULL, Counts_units ="dN/dlogDp", Vol_units = "ml") {
    # Counts_units:     Character, default is counts per ml
    # General.df        numerical counts for OPC data for each Bin (Bin0, Bin1.....Bin23)
    # output is a list of bins_OPC: character vector of diameter names (Bins)
    #                     counts_OPC: dataframe of dCount/dlogDp
    #                     Volume_OPC = Volume_OPC, Met_OPC = Met_OPC, PM_data
    # Selecting dates
    if (!all(is.null(c(DateBegin, DateEnd)))) {

        if (!(is.null(DateBegin))) General.df <- General.df %>% filter(date >= DateBegin)
        if (!(is.null(DateEnd)))   General.df <- General.df %>% filter(date <= DateEnd)
    }
    if (!is.null(Sensor_dates))  General.df <- General.df %>% filter(date <= Sensor_dates)
    
    # make all fields as numeric for OPCN3
    # browser()
    General.df[,grep("Bin",names(General.df))] <- sapply(names(General.df[,grep("Bin",names(General.df))]), function(x) { as.numeric(General.df[,x])})
    
    # assign diameters to OPCN3 data
    bins_OPC <- General.df[ , grepl("Bin" ,names(General.df))]
    
    # include met data: Temprature and Humidity from ambient air (Tem, Hum) and OPC sensor (OPCHum, OPCTemp)
    # include sampling volume and rate (OPCVol, OPCTsam)
    Met_OPC <-  General.df %>%
        dplyr::select(Temperature,
                      Relative_humidity,
                      OPCTemp,
                      OPCHum,
                      OPCVol,
                      OPCTsam)
    
    
    # inlculde PM data from sensor (Particulate_Matter_10) and from reference (Ref.PM10)
    PM_data <- General.df %>%
        dplyr::select(Particulate_Matter_1,
                      Particulate_Matter_10,
                      Particulate_Matter_25,
                      Ref.PM10)


    # remove all NA and NaN value
    bins_OPC <- na.omit(bins_OPC)
    
    # calculate dN/d(bin) in log10 scale for 2 each consecutive bins  "dN/dlogDp" corresponding to the unit of the reference counts
    # index and names of ONLY OPC columns in bin_OPC
    i.bins_OPC <- setdiff(names(bins_OPC) %>% grep(pattern = "Bin", x = .),names(bins_OPC) %>% grep(pattern = paste(c("DMPS", "APS"), collapse = "|"), x = .))
    n.bins_OPC <- names(bins_OPC[i.bins_OPC])
    
    # initialize empty matrix
    dcounts_dLogDp <- matrix(rep(0, nrow(bins_OPC) *  length(i.bins_OPC)), nrow = nrow(bins_OPC), ncol = length(i.bins_OPC))
    colnames(dcounts_dLogDp) <- n.bins_OPC
    
   
    dcounts_dLogDp <- sapply(n.bins_OPC, f_dcounts_ddp, bins_OPC = bins_OPC, n.bins_OPC = n.bins_OPC, bins_diameters = bins_diameters,
                          Counts_units = Counts_units)

    #####################################################
    # calculate Volume for each Bin of the OPC-----------
    #####################################################
    
    # calculate volume for all Bins of the OPC sensor using the fucntion f_Volume_OPC()
    # we need to use counts per seconds, therefore we will divide the row counts by the sampling time
    # sampling time is obtained dividing the total sampling time over 1 minute 
    
    # define sampling Period (in seconds)
    # sampling_period <- 4.08 # OPCTsam/60 (60 --> interval == 1 minute)
    sampling_period <- General.df$OPCTsam/60  # OPCTsam/60 (60 --> interval == 1 minute)
    dv_OPC_dLogDp <-  apply(bins_OPC[,n.bins_OPC]/sampling_period, MARGIN = 1,
                            function(n.row) return(n.row * bins_weight[,n.bins_OPC] * bins_volume[, n.bins_OPC] ))
    
    dv_OPC_dLogDp <-  dplyr::bind_rows(dv_OPC_dLogDp)
 
    # transpose data by bin 
    bins_diameters <- gather(bins_diameters, "bins", "diameters")
    # bins_diameters <- bins_diameters %>%
    #     filter(diameters < max(Dist_Ref.full$Diam_APS, na.rm = T))
    
    
    dcounts_dLogDp <- as.data.frame(dcounts_dLogDp)
    dcounts_dLogDp <- dcounts_dLogDp %>%
        gather("bins", "counts")

    # merge with diameters
    dcounts_dLogDp <- dcounts_dLogDp %>%
        left_join(bins_diameters, by = c("bins"))

    # remove all NA and NaN value
    dcounts_dLogDp <- na.omit(dcounts_dLogDp)

    # only select counts > 0
    counts_OPC <- dcounts_dLogDp %>%
        dplyr::select(diameters,
                      counts) %>%
        dplyr::filter(counts > 0)

    
    # calculate mean of counts of OPC by time range
    counts_OPC <- counts_OPC %>%
        dplyr::group_by(diameters) %>%
        dplyr::summarise(counts = mean(counts, na.rm = T))
    
    
    
    dv_OPC_dLogDp <- as.data.frame(dv_OPC_dLogDp)
    dv_OPC_dLogDp <- dv_OPC_dLogDp %>%
        gather("bins", "volume")
    
    # merge with diameters
    dv_OPC_dLogDp <- dv_OPC_dLogDp %>%
        left_join(bins_diameters, by = c("bins"))
    
    # remove all NA and NaN value
    dv_OPC_dLogDp <- na.omit(dv_OPC_dLogDp)
    
    # only select counts > 0
    Volume_OPC <- dv_OPC_dLogDp %>%
        dplyr::select(diameters,
                      volume) %>%
        dplyr::filter(volume > 0)
    
    
    # calculate mean of volume of OPC by time range
    Volume_OPC <- Volume_OPC %>%
        dplyr::group_by(diameters) %>%
        dplyr::summarise(volume = mean(volume, na.rm = T))
    
    
    # add Meteorolgical data
    Met_OPC <- Met_OPC %>%
        dplyr::summarise(Temp = mean(Temperature, na.rm = T),
                         Hum = mean(Relative_humidity, na.rm = T),
                         OPCTemp = mean(OPCTemp, na.rm = T),
                         OPCHum = mean(OPCHum, na.rm = T),
                         OPCVol = mean(OPCVol, na.rm = T),
                         OPCTsam = mean(OPCTsam, na.rm = T))
    
    
    PM_data <- PM_data %>%
        dplyr::summarise(OPC_PM1 = mean(Particulate_Matter_1, na.rm = T),
                         OPC_PM25 = mean(Particulate_Matter_25, na.rm = T),
                         OPC_PM10 = mean(Particulate_Matter_10, na.rm = T),
                         Ref_PM10 = mean(Ref.PM10, na.rm = T))
    
    
    return(list(bins_OPC = bins_OPC, counts_OPC = counts_OPC, Volume_OPC = Volume_OPC, Met_OPC = Met_OPC, PM_data = PM_data))
    
}


# prediction of OPC values based of fitting model from Reference data
# return a dataframe with corrected diamters and counts in log10 units
Density_OPC_predict <- function(counts_OPC, Mod_type, density, Model.i.Gam = NULL) {
    
    # counts_OPC contains diamters and counts in micrometers
    # diameter correction with the density
    counts_OPC$diameters <- counts_OPC$diameters/sqrt(density)
    
    # browser()
    DataXY <- data.frame(x = log10(counts_OPC$diameters),
                         y = log10(counts_OPC$counts))
    
    predict_OPC <- predict(Model.i.Gam$Model, DataXY)
    predict_OPC <- data.frame(DataXY$x, predict_OPC)
    names(predict_OPC) <- c("diameters", "predict_counts")
    
    predict_OPC$diameters <- 10^predict_OPC$diameters
    predict_OPC$predict_counts <- 10^predict_OPC$predict_counts
    
    return(predict_OPC = predict_OPC)
}


# plot predicted value of the OPC sensor
Plot_Distribution_OPC_Sensor <- function(counts_OPCN, counts_Ref, predict_OPC, DateBegin = Start_Time, DateEnd = Stop_Time) {
    
    
    plot <-  ggplot() + 
        theme_bw() +
        geom_point(data = counts_OPCN, aes(log10(diameters), log10(counts), col = "OPC"), stat="identity") +
        geom_point(data = counts_Ref, aes(log10(diameters), log10(counts), col = "ref"), stat = "identity", fill = "gray") +
        geom_point(data = predict_OPC, aes(log10(diameters), log10(predict_counts), col = "predict"), stat="identity") +
        scale_color_manual(values = c("OPC" = "black", "ref" = "red", predict = "blue")) +
        theme(axis.title.x = element_text(colour="black", size=15),
              axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=15, colour = "black")) +
        theme(axis.title.y = element_text(colour="black", size=15),
              axis.text.y  = element_text(angle=0, vjust=0.5, size=15, colour = "black")) +
        xlab(expression(paste("log10 of diameter (?m)"))) + 
        ylab(expression(paste("log10 of counts / log of diameters"))) +
        ggtitle((paste("from ", format(DateBegin, "%y-%m-%d %H:%M"), " to ", format(DateEnd, "%y-%m-%d %H:%M")))) 
    
    plot
}



Distribution_Ref_TS <- function(RefData, DateBegin = NULL, DateEnd = NULL, Sensor_dates = NULL, Min_APS = NULL, Max_APS = NULL) {
    # RefData:      a dataframe that includes all the reference data (dates, coordinates, gas, PM mass concentration, bins ...) 
    #               The binned counts and diameters shall not be log transformed. Units of diameters: working with micrometers 
    #               and other units were not tested
    
    # DateBegin,   The start and ending selected dates in POSIXCt format. Default values are NULL, 
    # DateBegin      It is possible to have only one date limit, DateBegin or DateBegin. DateBegin or DateBegin are not discarded.
    # Sensor_dates  vector of POSIXCt: the dates for which the PM sensor provides binned counts. 
    #               In case the sensor PM data series is not complete, dates with missing PM sensor data will be discared from RefData
    
    # Min_APS,      Numeric, Minimum and maximum diameters of reference counts to be selected 
    # Max_APS       for reference counts. Default values are NULL. It is possible to have only one diameter limit,  Min_APS or Max_APS.
    #               Min_APS and Max_APS are discarded.
    
    # Return        a list with datafame RefData (only selected dates and names of bins corresponding to selected diameters),
    #               datafame counts_Ref with column diameters and mean counts over the selected dates
    #               vector with selected APS diameters
    #               vector with DMPS diameter
    #                       
    # select only columns containing .Bin.DMPS & .Bin.APS
    
    
    # browser()
    # Selecting dates
    if (!all(is.null(c(DateBegin, DateEnd)))) {
        
        if (!(is.null(DateBegin))) RefData <- RefData %>% filter(date >= DateBegin)
        if (!(is.null(DateEnd)))   RefData <- RefData %>% filter(date <= DateEnd)
    }
    if (!is.null(Sensor_dates))  RefData <- RefData %>% filter(date <= Sensor_dates)
    
    #browser()
    # Vector of diameters measured by the APS instrument
    Diam_APS <- names(RefData)[grep(pattern = "Bin.APS.", x = names(RefData) )] %>%
        gsub(pattern = "Bin.APS.", replacement = "", x = .) %>%
        as.numeric
    
    ### APS diameters to be dropped, i. e. <= 0.89 | >= 11.97
    if (!all(is.null(c(Min_APS, Max_APS)))) {
        
        Diam_APS_to_remove <- numeric()
        if (!(is.null(Min_APS))) Diam_APS_to_remove <- Diam_APS[Diam_APS <= Min_APS]
        if (!(is.null(Max_APS))) Diam_APS_to_remove <- c(Diam_APS_to_remove, Diam_APS[Diam_APS >= Max_APS])
        # Discarding diameters if any diameters < Min_APS or > Max_APS
        if (length(Diam_APS_to_remove) > 0) {
            
            # Discarding APS diameters from Diam_APS and RefData
            Diam_APS           <- Diam_APS[-which(Diam_APS %in% Diam_APS_to_remove)] 
            Diam_APS_to_remove <- paste0("Bin.APS.", Diam_APS_to_remove)
            RefData            <- RefData[ , !names(RefData) %in% Diam_APS_to_remove]
        }
    }
    
    

    # browser()
    # Vector of diameters measured by the DMPS instrument
    Diam_DMPS <- names(RefData)[grep(pattern = "Bin.DMPS.", x = names(RefData) )] %>%
        gsub(pattern = "Bin.DMPS.", replacement = "", x = .) %>%
        as.numeric
    
    ### Remove "Bin.DMPS." and "Bin.APS." from column names of RefData
    names(RefData) = gsub(pattern = paste0(c("Bin.DMPS.", "Bin.APS."),collapse = "|"), 
                          replacement = "", x = names(RefData))
    

    # change with colmeans
    diameter_names <- as.character(c(Diam_DMPS,Diam_APS))
    Ref_Bins       <- RefData[, which(names(RefData) %in% diameter_names)]
    

    counts_Ref <- colMeans(Ref_Bins, na.rm = T)
    counts_Ref <- as.data.frame((counts_Ref))
    counts_Ref$diameters <- as.numeric(rownames(counts_Ref))
    rownames(counts_Ref) <- NULL
    names(counts_Ref) <- c("counts", "diameters")
    counts_Ref <- counts_Ref %>%
        dplyr::filter(counts > 0)
    
    if (!nrow(counts_Ref) == 0 ) {
        
        return(list(RefData_filtered = RefData, counts_Ref = counts_Ref, Diam_DMPS = Diam_DMPS, Diam_APS = Diam_APS))
    } else  print ("Reference data is empty!", quote = FALSE) 
    
}

# calculate VOLUME for each Bin for APS
f_Volume_APS <- function(Dist_Ref.full, names.Diam_APS, density = density, units = "ml") {    #density = 1.207
   # browser()
   # This function return a numerical vector with the volume of samples particles in each Bins (volume is units of ml)
   # Dist_Ref.full:     list with (1) data frame, name "RefData_filtered", of counts of APS and DMPS 
   #                              (2) a numerical vector of real Diam_DMPS (not log transformed) in µm
   #                              (3) a numerical vector of real Diam_APS in  µm, after diameter correction of the APS data
   # names.Diam_APS:    character vector,  representing the numerical diameters used in the column names Dist_Ref.full$RefData_filtered
   # density:           numerical, density value of the particles, to be used for correcting the APS optical diameter 
   # unit:              character, deault value is "ml", not used so far
    
    # use numerical values for the names.Diam_APS
    n.Diam_APS <- as.numeric(names.Diam_APS)
    
    # Checking that there is still one diameter more 
    if (n.Diam_APS <= Dist_Ref.full$Diam_APS[length(Dist_Ref.full$Diam_APS)]) {
        
        # next consecutive APS diameter
        Diam_APS.next  <- Dist_Ref.full$Diam_APS[which(Dist_Ref.full$Diam_APS == n.Diam_APS) + 1] 
        # calculate volume for each diameters of the APS (including density corrections)
        return( as.vector( log10(Diam_APS.next / n.Diam_APS) * ((n.Diam_APS / sqrt(density))^3) *pi/6 * Dist_Ref.full$RefData_filtered[ , names.Diam_APS] ))
        # return( as.vector(  Dist_Ref.full$RefData_filtered[ , names.Diam_APS] * ((n.Diam_APS / sqrt(density))^3) *pi/6 ))
        
    } else return(cat("ERROR, last diameter of APS selected"))
    
}

# calculate VOLUME for each Bin for APS (1ml == 1cm3)
f_Volume_DMPS <- function(Dist_Ref.full, names.Diam_DMPS, units = "ml") {
    # browser()
    # This function return a numerical vector with the volume of samples particles in each Bins (volume is units of ml)
    # Dist_Ref.full:     list with (1) data frame, name "RefData_filtered", of counts of APS and DMPS 
    #                              (2) a numerical vector of real Diam_DMPS (not log transformed) in µm
    #                              (3) a numerical vector of real Diam_APS in  µm, after diameter correction of the APS data
    # names.Diam_APS:    character vector,  representing the numerical diameters used in the column names Dist_Ref.full$RefData_filtered
    # density:           numerical, density value of the particles, to be used for correcting the APS optical diameter 
    # unit:              character, deault value is "ml", not used so far
    
    # use numerical values for the names.Diam_APS
    n.Diam_DMPS <- as.numeric(names.Diam_DMPS)
    
    # Checking that there is still one diameter more 
    if (n.Diam_DMPS <= Dist_Ref.full$Diam_DMPS[length(Dist_Ref.full$Diam_DMPS)]) {
        
        # next consecutive APS diameter
        Diam_DMPS.next  <- Dist_Ref.full$Diam_DMPS[which(Dist_Ref.full$Diam_DMPS == n.Diam_DMPS) + 1] 
        return( as.vector( log10(Diam_DMPS.next / n.Diam_DMPS) * ((n.Diam_DMPS)^3) *pi/6 * Dist_Ref.full$RefData_filtered[ , names.Diam_DMPS] ))
        # return( as.vector(  Dist_Ref.full$RefData_filtered[ , names.Diam_DMPS] * ((n.Diam_DMPS)^3) *pi/6 ))
        
    } else return(cat("ERROR, last diameter of DMPS selected"))
    
}


