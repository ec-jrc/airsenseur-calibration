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
# - Federico Karagulian    , federico.karagulian@ec.europa.eu - European Commission - Joint Research Centre
# - Laurent Spinelle       , laurent.spinelle@ec.europa.eu - European Commission - Joint Research Centre
#   Eike Hinderk JC<rrens   , e.h.juerrens@52north.org
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
#                                       This Funtion is no more used and has been deleted on 20190212
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
#             DateIN.Influx.prev if it exists then to DateIN.SOS.prev if it exists otherwise it is set to "UTC"
# 181012 MG   my.rm.outliers: the computing time has been divided by two, computing the min and max of interval of tolerance within one rollapply
# 190110 MG   Etalonnage: an error has been colved when all s_y are NA
#
# TO BE DONE:
#             Funtion Down-Influx repeats the download of last 4 weeks of date each time it is called (for now, unique() is used)
#=====================================================================================CR
# 161125 MG : Make.Old Creating a copy of the file as old file ####
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
# 161107 MG : functions for converting digital values ####
#=====================================================================================CR
ASEDigi2Volt <- function(Sensors_Cal, Digital, ADC = 16, Volt.Convert = TRUE, Pattern = "Out.", Name.Digital = "gas.sensor") {
    # return      : the function return a data.frame/data.table with the voltages or currents for all sensors. It will convert _StDev if any in Digital
    # Sensors_Cal : dataframe with the following column vectors:
    #               name.gas      : a vector of strings with the molecule symbols of sensors mounted on the AirSensEUR (CO...)
    #               gas.sensor    : a vector of strings with the molecule name that are measured by the AirSensEUR (Carbon_monoxide...)
    #               name.sensor   : a vector of strings with the names of sensors mounted on the AirSensEUR (COA4...)
    #               Ref           : a float vector, one value per sensor in Sensors_Cal$Sensors, giving the voltage in the middle of the analogue to digital converter (ADC) on the shield
    #               RefAD         : a float vector, one value per sensor in Sensors_Cal$Sensors, corresponding to the range of the ADC conversion (ref ? RefAD)
    #               Intercept     : a float vector, one value per sensor in Sensors_Cal$Sensors, intercept of conversion equation Conc[] = Intercept + Slope x Voltage
    #               Slope         : a float vector, one value per sensor in Sensors_Cal$Sensors, intercept of conversion equation Conc[] = Intercept + Slope x Voltage
    #               Sens.raw.unit : vector of strings, one value per sensor with the Unit names after conversion of Digital values into raw data
    # ADC         : number of bits of the Analogue to digital conversion
    # Digital     : a dataframe of numerics with the Digital values to be converted into Voltages or Currents (Out.Nitrogen_dioxide, ...
    # Volt.Convert: logical, default is TRUE. If TRUE, the data in Digital dataFrame need conversion from Digital to V/nA. If FALSE data units do not need conversion.
    # Pattern     : string, default "Out.". Pattern to be deleted from names(Digital)
    # Name.Digital: string, default is  "gas.sensor",. Possible values: "gas.sensor" or "name.sensor". 
    #               It indicates if the columns names of digital are base on sensor names (name.sensor) or molecules (gas.sensor) found in Sensors_Cal.
    # reorder Sensors_Cal as Digital for gas sensors - create empty data.table of results
    # The name of the sensors are given by the names of the column in Digital
    
    # Deleting Pattern from gas.name or sensor.name if needed. Discard non chemical sensor from Sensors_Cal
    if (Name.Digital == "gas.sensor") {
        Sensors_Cal  <- Sensors_Cal[as.vector(sapply(gsub(pattern = Pattern, replacement = "", unique(gsub("_StDev","",names(Digital)))),
                                                     function(i) grep(i, Sensors_Cal$gas.sensor))),]
    } else if (Name.Digital == "name.sensor") Sensors_Cal <- Sensors_Cal[as.vector(sapply(gsub(pattern = Pattern, replacement = "", unique(gsub("_StDev","",names(Digital)))),
                                                                                          function(i) grep(i, Sensors_Cal$name.sensor))),]
    # Check which ones to converts in V and in nA
    # no need we convert all in volts, maybe this helps to get a dataframe only of numeric
    indexA <- which(Sensors_Cal$Sens.raw.unit == "nA")
    name.Sensors <- names(Digital)[grep("_StDev", names(Digital), invert = T)]
    name.StDev   <- names(Digital)[grep("_StDev", names(Digital))]
    # converts in Volts
    if (Volt.Convert) {
        MyVectorMul  <- 2*Sensors_Cal$RefAD/(2^ADC)
        MyVectorAdd  <- Sensors_Cal$Ref - Sensors_Cal$RefAD
        data.table::set(Digital, j = name.Sensors, value = Digital[, .SD, .SDcols = name.Sensors][, Map(`+`, 1, .SD)][, Map("*", .SD, MyVectorMul)][, Map(`+`, MyVectorAdd, .SD)])
        if (length(name.StDev) > 0) data.table::set(Digital, j = name.StDev, value = Digital[, .SD, .SDcols = name.StDev][, Map("*", .SD, MyVectorMul)])}
    # converts in nA
    if (length(indexA) != 0) {
        MyVectornA  <- 10^9/(Sensors_Cal$GAIN[indexA])
        data.table::set(Digital, j = name.Sensors, value = Digital[, .SD, .SDcols = name.Sensors][, Map(`-`, .SD, Sensors_Cal$board.zero.set)][, Map("*", .SD, MyVectornA)])
        if (length(name.StDev) > 0) data.table::set(Digital, j = name.StDev, value = Digital[, .SD, .SDcols = name.StDev][, Map("*", .SD, MyVectornA)])}
    if (length(name.StDev) == 0) {
        colnames(Digital) <- Sensors_Cal$name.sensor
    } else colnames(Digital) <- c(Sensors_Cal$name.sensor, paste0(Sensors_Cal$name.sensor,"_StDev"))
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
# 161031 MG : My.rm.Outliers Removing outliers using the Median Average Deviation method ####
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
#' Identification of values exceeding ymin and ymax values and outliers using an hampfel filter.
#'#' @description For the Hampfel filter, first the median and Median Average Deviation multiplied by threshold (MAD), are computed within every rolling window, 
#' in rolling (moving) windows switching by one y at the time. The window has width "window" (number of consecutive y data in each widow). 
#' Then the interval of confidence is set for each y value corresponding to zmax = Median + MAD and zmin = Median - MAD at each y value.
#' It is also checked that zmin is not lower than ThresholdMin, otherwise ThresholdMin replaces zmin where zmin is lower than ThresholdMin (e.g to filter negative data if ThresholdMin is equal to 0)
#' In addition, the time resolution (periodicity) of y is checked, preventing applying any Hampfel filtering for data with 60 minutes or more periodicity, although the ymin and ymax filtering is still applied.
#' The Hampfel filer needs the median and MAD computation on moving windows, y data are cutted in slices of finite consecutive data before applying the filtering
#' @param date (mandatory) vector of POSICxt for the x axis, date are supposed to be consecutive (no date missing).
#' @param y (mandatory) numeric vector on y axis on which ouliers can be detected. Y may also be a tbl.
#' @param ymin numeric, default is NULL, minimum value for y, for example to remove negative values
#' @param ymax numeric, default is NULL, maximum value for y, for example to remove peak values
#' @param ThresholdMin numeric, default is NULL. Minimum values for zmin, the minimum values that evidence outliers when exceeded
#' @param window (mandatory) numeric, window width, number of rows used to compute median and MAD  in rolling windows
#' @param threshold (mandatory) numeric, coefficient muliplied by the difference between median and average. If data exceed this threshold x diff, data is an outlier
#' @param plotting logical, default TRUE. If TRUE the plot of outliers is returned.
#' @param set.Outliers logical, default TRUE. If TRUE the the procedure to detect outliers is carried out
#' @param Ind id detecting outliers is not requested, in case of only plotting (when set.Outliers is FALSE), inf is dataframe or data.table with columns date, OutliersMax, OutliersMin,
#'  Low_values and high vales, for example determined inat a previous step using function My.rm.Outliers
#' @param LasY integer, defualt value is 3. Orientation of numbers on y axis, 0: always parallel to the axis, 1: always horizontal, 2: always perpendicular to the axis and 3: always vertical.
#' @param nTickX,nTickY integer, number of tick marks on x and y axis
#' @param Title character vector, Title to be plotted
#' @param Ylab  character vector, defaut is "Raw Sensor values" y axis label to be plotted
#' @param Dygraphs logical, defaults is FALSE to plot a base plot. If TRUE, a Dygraphs (in UTC.) is plotted and returned
#' @return if set.Outliers is TRUE the function returns a dataframe with Logical for:
#'  - Low_values for y lower than low values,
#'  - High_values for y exceeding lower MAD,
#'  - OutliersMin for y lower than zmin,
#'  - OutliersMax for y exceeding zmax,
#'  - zmin,
#'  - zmax,
#'  - Median
#'  - MAD,
#'  - y
#'  - width (TUE if Periodicity >= 60, FALSE otherwise.
#' In addition, if plotting is TRUE a DYgraph is returned if Dygraphs is TRUE and a base plot is plotted if Dygraphs is FALSE. 
My.rm.Outliers <- function(date, y, ymin = NULL, ymax = NULL, ThresholdMin = NULL, window, threshold, plotting = TRUE, set.Outliers = TRUE, ind = NULL,
                           nTicksX = 10, nTicksY = 10, LasY = 3, Title = NULL, Ylab = "Raw Sensor values", Dygraphs = FALSE) {
    
    # convert tible to data.frame then vector, this is necessary for rollapply
    if (class(y)[1] == "tbl_df")  y <- as.data.frame(y)[,1]
    
    if (set.Outliers) {
        
        # Detecting values lower than ymin and higher than ymax, preparing df
        if (!is.null(ymin)) Low_values  <- ifelse(y <= ymin, TRUE, FALSE) else Low_values  = NA
        if (!is.null(ymax)) High_values <- ifelse(y  > ymax, TRUE, FALSE) else High_values = NA
        df <- data.table::data.table(date = date,
                                     Low_values  = Low_values,
                                     High_values = High_values)
        
        # Cutting in slices of consecutive y data excluding NAs for finite data
        DT <- data.table::data.table(date = date, y = y)
        DT.finite <- DT[is.finite(DT[["y"]])]
        Stops <- data.table::data.table(Start = c(1, which(difftime(DT.finite$date, data.table::shift(DT.finite$date), units = "min") > 60)),
                                        End = c(which(diff(DT.finite$date) > 60), nrow(DT.finite)))
        
        for(Period in seq_along(Stops$Start)){
            
            # Date start and end of each period with their indexes in date vector (idem y)
            Date.Start <- DT.finite$date[Stops$Start[Period]]
            Date.End   <- DT.finite$date[Stops$End[Period]]
            # Corresponding index of date and y
            i.Date.Start  <- which(date == Date.Start)
            i.Date.End    <- which(date == Date.End)
            
            # Computing for the period
            df.Period <- data.table::data.table(date = date[i.Date.Start:i.Date.End], y = y[i.Date.Start:i.Date.End])
            data.table::set(df.Period, j = "Median",
                            value = caTools::runquantile(y[i.Date.Start:i.Date.End],window,0.5, type =2, endrule = "quantile", align = "center"))
            # threshold * MazamaRollUtils::roll_MAD seems not able to manage NA even if  na.rm = T is added
            #MAD    <- threshold * MazamaRollUtils::roll_MAD(y, width = window, align = "center", na.rm = T)
            data.table::set(df.Period, j = "MAD",
                            value = caTools::runmad(x = y[i.Date.Start:i.Date.End], k = window,
                                                     center = runmed(y[i.Date.Start:i.Date.End], k = window),
                                                     constant = threshold, endrule = "mad", align = "center"))
            
            ### Hampfel filter, high value and Low value###
            data.table::set(df.Period, j = "zmax", value = df.Period$Median + df.Period$MAD)
            
            ### Hampfel filter, low value ###
            data.table::set(df.Period, j = "zmin", value = df.Period$Median - df.Period$MAD)
            # Changing the low values of the minimum of the interval of tolerance by ThresholdMin
            if (shiny::isTruthy(ThresholdMin) || shiny::isTruthy(ymin)) {
                if (!shiny::isTruthy(ThresholdMin) && shiny::isTruthy(ymin)) ThresholdMin <- ymin
                Index.Lower <- which(df.Period$zmin < ThresholdMin)
                if(length(Index.Lower) > 0){
                    data.table::set(df.Period, i = Index.Lower, j = "zmin", value = rep(ThresholdMin, length.out = length(Index.Lower)))}
            }

            # data frame to return with all metrics
            data.table::set(df.Period, j = "OutliersMin", value = ifelse(df.Period$y < df.Period$zmin, TRUE, FALSE))
            data.table::set(df.Period, j = "OutliersMax", value = ifelse(df.Period$y > df.Period$zmax, TRUE, FALSE))
            
            # Appending df.Period to df.Periods
            if(!exists("df.Periods")){
                df.Periods <- df.Period
            } else df.Periods <- data.table::rbindlist(list(df.Periods, df.Period), use.names = TRUE, fill = TRUE)
            rm(df.Period)
        }
        # merging df.Periods with df
        df <- merge(df, df.Periods, by = "date", all = TRUE)
        
        # Preventing from detecting outliers in case of hourly data
        # checking for periodicity >= 1, adding column HOurs to df
        Periodicity <- DF_sd(DF=df, Col.for.sd = "y", width = 60L)
        # Creating Flag Hours, TRUE when Periodicity is >= 60
        data.table::set(df, j = "Hours", value = rep(FALSE, nrow(df)))
        if("i.dates" %in% names(Periodicity) && length(Periodicity[["i.dates"]]) > 0){
            data.table::set(df, i = Periodicity[["i.dates"]], j = "Hours" , value = rep(TRUE, length(Periodicity[["i.dates"]])))}
        
        i.Outliers <- which((df$Hours & (df$OutliersMin | df$OutliersMax)))
        if(length(i.Outliers) > 0){
            data.table::set(df, i = i.Outliers, j = "OutliersMin", value = rep(FALSE, length(i.Outliers)))
            data.table::set(df, i = i.Outliers, j = "OutliersMax", value = rep(FALSE, length(i.Outliers)))}
    }
    # Plotting the data, show the ut() cutoffs, and mark the outliers:
    if (plotting) {
        if (Dygraphs) {
            # convert tible to data.frame then vector, this is necessary for rollapply
            if (class(date)[1] == "tbl_df")  date <- as.data.frame(date)[,1]
            #Commom.dates <- which(ind$date %in% date)
            # creating dataframe with xts data series
            data <- merge(data.frame(date = date, Sensor = y), ind, by = "date", all = FALSE)
            
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
            plot_outli <- dygraph(data = data, ylab = Ylab, main = Title, height = "auto", width = "100%") %>%
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
                #dyOptions(labelsUTC = T) %>% 
                dyOptions(useDataTimezone = TRUE)
        } else {
            # saving the original graphical parameters
            op <- par(no.readonly = TRUE)
            # Restoring graphical parameters on exit of function, even if an error occurs
            on.exit(par(op)) # it reset the par(mfrow) allways plotting on the upper left plot
            par(mar = c(2,2.7,1.7,0.2))
            par(mgp = c(1.5,0.3,0))
            if (!set.Outliers) if (!is.null(ind)) df <- ind else futile.logger::flog.error("[My.rm.Outliers] determination of index is not requeted and indexes are set to null or not given")
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
                graphics::axis.POSIXct(1, at = dates, las = 1, format = "%d-%b %H:%M")
            } else graphics::axis.POSIXct(1, at = dates, las = 1, format = "%d-%b")
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
    if (set.Outliers) return(as.data.frame(df)) else if (Dygraphs) return(plot_outli)
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
    # Title                     : character vector, Title to be plotted
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
        if (shiny::isTruthy(ThresholdMin)) {zmax.zmin[Index.Lower,2] <- rep(ThresholdMin, length.out = length(Index.Lower))}
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
            #Commom.dates <- which(ind$date %in% date)
            # creating dataframe with xts data series
            data <-  merge(data.frame(date = date, Sensor = y), ind, by = "date", all = FALSE)
            # data.frame(date        = date,
            #            Sensor      = y,
            #            OutliersMax = ind[Commom.dates,"OutliersMax"],
            #            OutliersMin = ind[Commom.dates,"OutliersMin"],
            #            Low_values  = ind[Commom.dates,"Low_values"],
            #            High_values = ind[Commom.dates,"High_values"],
            #            zmin        = ind[Commom.dates,"zmin"],
            #            zmax        = ind[Commom.dates,"zmax"])
            
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
            plot_outli <- dygraph(data = data, ylab = Ylab, main = Title, height = "auto", width="100%") %>%
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
                #dyOptions(labelsUTC = T) %>% 
                dyOptions(useDataTimezone = TRUE)
        } else {
            # saving the original graphical parameters
            op <- par(no.readonly = TRUE)
            # Restoring graphical parameters on exit of function, even if an error occurs
            on.exit(par(op)) # it reset the par(mfrow) allways plotting on the upper left plot
            par(mar = c(2,2.7,1.7,0.2))
            par(mgp = c(1.5,0.3,0))
            if (!set.Outliers) if (!is.null(ind)) df <- ind else futile.logger::flog.error("[My.rm.Outliers] determination of index is not requeted and indexes are set to null or not given")
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
                graphics::axis.POSIXct(1, at = dates, las = 1, format = "%d-%b %H:%M")
            } else graphics::axis.POSIXct(1, at = dates, las = 1, format = "%d-%b")
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
# 170609 MG : GraphOut Plotting points and a subset of points ####
#=====================================================================================CR
#' This function plot the data, show the ut() cutoffs, and mark the outliers.
#' @param date: the time series date, a vector of POSIXCt
#' @param y: the y values to be plotted, a numeric vector
#' @param Col: The color of the time series, the default color is "#E00000"
#' @param Ylab: The label of the y axis, a character vector, the default is "Raw Sensor values"
#' @param indfull: integer: the index of vector date, to be plotted as invalid points in red or a list of POSIXct with 4 elements: Tmin, Tmax, RH.min and Rh.max. Using the names of list "infull" in legend
#' @param nTickX,Y: integer, plotting with the number of spaces between ticks on x and y axis, default 10
#' @param LasY: integer, orientation of numbers on y axis> 0: always parallel to the axis [default], 1: always horizontal, 2: always perpendicular to the axis and 3: always vertical.
#' @param Title: Character vector, Title to be plotted
#' @param Dygraphs: logical, defaults is FALSE to plot a base plot. If TRUE use Dygraphs Dygraphs are plotted in UTC.
GraphOut <- function(date , y, Col = "#E00000", Ylab = "Raw Sensor values", indfull,
                     nTicksX = 10, nTicksY = 10, LasY = 3, Title = NULL, Dygraphs = FALSE)  {
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
            plot_Warm <- dygraph(data = data, ylab = Ylab, main = Title, height = "auto", width="100%") %>%
                dySeries("Sensor"  , label = "Sensor value"          , color = Col) %>%
                dySeries("Invalid" , label = "Invalid value"  , color = "red") %>%
                dyOptions(drawPoints = TRUE, pointSize = 2) %>%
                dyLegend(show = "always", hideOnMouseOut = FALSE, width = 350) %>%
                dyRangeSelector() %>%
                #dyOptions(labelsUTC = T) %>% 
                dyOptions(useDataTimezone = TRUE)
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
            if (length(indfull[[4]]) > 0) data$RHMaxi[  which(data$date %in% indfull[[4]])]   <- data$Sensor[which(data$date %in% indfull[[4]])]
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
            plot_Warm <- dygraph(data = data, ylab = Ylab, main = Title, height = "auto", width="100%") %>%
                dySeries("Sensor"   , label = "Sensor value"    , color = Col) %>%
                dySeries("TempMini" , label = names(indfull)[1] , color = Colors[1]) %>%
                dySeries("TempMaxi" , label = names(indfull)[2] , color = Colors[2]) %>%
                dySeries("RHMini"   , label = names(indfull)[3] , color = Colors[3]) %>%
                dySeries("RHMaxi"   , label = names(indfull)[4] , color = Colors[4]) %>%
                dyLegend(show = "always", hideOnMouseOut = FALSE, width = 350) %>%
                dyOptions(drawPoints = TRUE, pointSize = 2) %>%
                #dyOptions(labelsUTC = T) %>% 
                dyOptions(useDataTimezone = TRUE) %>% 
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
            graphics::axis.POSIXct(1, at = dates, las = 1, format = "%d-%b %H:%M")
        } else graphics::axis.POSIXct(1, at = dates, las = 1, format = "%d-%b")
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
# 161030 MG : Down_SOS Download Sensor data from SOS and later from InfluxDB? ####
#=====================================================================================CR
Down_SOS <- function(AirSensEur.name, UserMins, DownloadSensor = NULL, AirsensWeb, Duration = NULL, DateEND = NULL, ref.tzone = "UTC") {
    # AirSensEur.name       = Name of for AirSensEUR for SOS download
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
    
    #------------------------------------------------------------------------------CR
    # Sensor Data retrieving at apiEndpoint
    #------------------------------------------------------------------------------CR
    cat("\n")
    cat("-----------------------------------------------------------------------------------\n")
    futile.logger::flog.info(paste0("[Down_SOS] ", AirSensEur.name," sensor data retrieving"), sep = "")
    # Checking internet connection availability
    if (curl::has_internet()) {
        URL <- unlist(strsplit(unlist(strsplit(gsub('http://', '', AirsensWeb), split = '/'))[1], split = ':'))[1]
        if (PingThisSite(URL)) {
            futile.logger::flog.info(paste0("[Down_SOS] ping to ", AirsensWeb, " Ok"), sep = "")
        } else return(cat(paste0("[Down_SOS] ERROR: you have an internet connection but cannot ping to ",AirsensWeb,". SOS download cannot be carried out."), sep = "\n"))
    } else {
        return(futile.logger::flog.error(paste0("[Down_SOS] no internet connection. SOS download cannot be carried out.")))
    }
    # connect
    apiEndpoint <- sensorweb4R::Endpoint(AirsensWeb)
    # number of category at the apiEndpoint
    futile.logger::flog.info(paste0("[Down_SOS] in total ", length(sensorweb4R::timeseries(apiEndpoint)), " Sensors at the SOS client."), sep = "")
    # Selecting service "AirSensEUR" with name
    srv <- sensorweb4R::services(apiEndpoint)
    # get all phenomena
    phe <- sensorweb4R::phenomena(apiEndpoint)
    print(sensorweb4R::label(sensorweb4R::phenomena(apiEndpoint)), quote = FALSE)
    # get the station number corresponding to AirSensEur.name in sensorweb4R::label(stations(srv))
    if (AirSensEur.name %in% sensorweb4R::label(sensorweb4R::stations(srv))) {
        sta <- sensorweb4R::stations(srv)[match(x=AirSensEur.name, table=sensorweb4R::label(sensorweb4R::stations(srv)))]
    } else {
        stop(futile.logger::flog.error(paste0("[Down_SOS] ", AirSensEur.name, " is not found at the apiEndpoint. Correct the name of AirSensEUR or
                        set Down.SOS to FALSE in the ASEconfig_xx.R file"), sep = ""))
    }
    # Select the timeseries of the station AirSensEur.name
    ts <- sensorweb4R::timeseries(sta)
    # The following is only for the JRC Ispra, mistakes with names
    if (AirSensEur.name == "JRC_C5_01") {
        ts <- ts[-grep(pattern = "http://www.airsenseur.org/ch1/o3_3e1f/11915854-335/1", x = sensorweb4R::label(ts))]
    }
    if (AirSensEur.name == "JRC_C5_05") {
        ts <- ts[-grep(pattern = "1_old", x = sensorweb4R::label(ts))]
    }
    cat(sensorweb4R::label(ts),sep = "\n")
    # fetch all the meta data of ts
    ts <- sensorweb4R::fetch(ts)
    # Position
    geom <- sp::geometry(sta)
    futile.logger::flog.info(paste0("[Down_SOS] Position of station ", AirSensEur.name, ":", head(geom@coords)),sep = "")
    # Phenomenon at the station
    Sensors <- data.frame(sensorweb4R::label(phenomenon(ts)), stringsAsFactors = FALSE)
    ### Trying to determine the name of variable using sensorweb4R::label(phenomenon(ts)), we will check if "Temperature", "Relative humidity" and "Atmospheric pressure" are in the label(phenomenon(ts))
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
    # Determining DateIN and DateEND for data download with a lubridate::interval
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
# 161029 MG: Check_Download Check General data file and the retrieve.data true or false ####
#=====================================================================================CR
#' Check status of data for AirSensEUR box
#' @param Influx.name Character, Name of for AirSensEUR in airsenseur.db, default Value NULL
#' @param WDinput MANDATORY, file.path, sub directory of where are the General, Refdata and InfluxData data files
#' @param UserMins numeric, periodicity of data requested after final data treatment
#' @param General.df Data.table or dataframe, default value is NA. General dataset
#' @param RefData Data.table or dataframe, default value is NA. RefData dataset
#' @param InfluxData Data.table or dataframe, default value is NA. InfluxData dataset
#' @param SOSData Data.table or dataframe, default value is NA. SOSData dataset
#' @returns a list with
#                              Ref.Rdata.file and Influx.Rdata.file, the name of the files with dataframe of reference and sensor downloaded data
#                              WDinput, the directory where the Rdata are saved
#                              Retrieve.data.Ref, true if reference data need be retrieved
#                              Retrieve.data.Influx, true if sensor data need be retrieved (Influxdb)
#                              Retrieve.data.SOS, true if sensor data need be retrieved (SOS)
#                              Retrieve.data.General, true if sysdate is posterior to DateEND.General.prev
#                              DateEND.Ref.prev, date to start download of reference data (last existing date), Null if "RefData.Rdata" does not exist
#                              DateEND.Influx.prev, date to start download of sensor data (last existing date), Null if ""InfluxData.Rdata" does not exist
#                              DateEND.SOS.prev, date to start download of sensor data (last existing date), Null if ""SOSData.Rdata" does not exist
#                              DateEND.General.prev, last date in General.Rdata, Null if ""General.Rdata" does not exist
Check_Download <- function(Influx.name = NULL, WDinput, UserMins, General.df = NA, RefData = NA, InfluxData = NA, SOSData = NA, Verbose = FALSE) {
    #profvis::profvis({
    # Set the Rdata file of input data
    airsenseur.db.file  = file.path(WDinput, "airsenseur.db")
    Ref.file            = file.path(WDinput, "RefData.csv")
    Influx.file         = file.path(WDinput, "InfluxData.csv")
    SOS.file            = file.path(WDinput, "SOSData.csv")
    General.file        = file.path(WDinput, "General.csv")
    if (is.null(Influx.name)) {
        Start.Dir <- WDinput
        repeat {
            Curent.Dir  <- basename(Start.Dir)
            Curent.Path <- dirname(Start.Dir)
            Influx.name <- basename(Curent.Path)
            if (Curent.Dir == "General_data") {
                rm(Start.Dir, Current.Dir, Curent.Path)
                break
            } else Start.Dir <- dirname(Start.Dir)}} 
    cat("\n-----------------------------------------------------------------------------------\n")
    futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " Checking:\n",
                                    airsenseur.db.file, "\n",
                                    General.file, "\n",
                                    Ref.file, "\n",
                                    SOS.file, " \n",
                                    Influx.file,"\n"))
    # Checking if the directory exist
    if (!dir.exists(WDinput)) {
        futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " Directory", WDinput, "does not exist. It should be created and sensor and reference data shall be downloaded."), sep = "\n")
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
        if (shiny::isTruthy(General.df)) {
            # General.file exists if data exist
            ExistFil.data.General = TRUE
            # General.df exists and is not NULL
            DateIN.General.prev  <- min(General.df$date, na.rm = TRUE)
            DateEND.General.prev <- max(General.df$date, na.rm = TRUE)
            # Checking if Download of General.df is necessary
            if (difftime(Sys.time(), max(General.df$date, na.rm = TRUE), units = "mins") > UserMins) {    ### MG , I doubt about the tz here, I think all is changed to UTC, as it is a difference maybe it does not matter
                Retrieve.data.General  = TRUE
                futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " sensor data should be retrieved. Start date for data download: ", DateEND.General.prev))
            } else {
                Retrieve.data.General  = FALSE
                futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " no sensor data should be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."))
            }
        } else {
            # General.df is NULL
            if (!file.exists(General.file)) {
                # General.file does not exist
                ExistFil.data.General = FALSE
                Retrieve.data.General  = TRUE
                DateIN.General.prev    = NULL
                DateEND.General.prev   = NULL
                futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " ", General.file, " should be created and sensor data shall be downloaded."))
            } else {
                # General.file exists
                ExistFil.data.General = TRUE
                futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " ", General.file, " exists."))
                if (file_ext(General.file) == "csv") {
                    General.df <- data.table::fread(file = General.file, na.strings = c("","NA", "<NA>"), select = "date")
                } else if (file_ext(General.file) == "Rdata") load(General.file)
                if (is.null(General.df)) {
                    DateIN.General.prev    = NULL
                    DateEND.General.prev   = NULL
                    Retrieve.data.General  = TRUE
                    futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " ", General.file, " is NULL (no values). It is going to be created, data, if any, will be retrieved."))
                } else {
                    # General.df exists and is not NULL
                    DateIN.General.prev  <- min(General.df$date, na.rm = TRUE)
                    DateEND.General.prev <- max(General.df$date, na.rm = TRUE)
                    # Checking if Download of General.df is necessary
                    if (difftime(Sys.time(), max(General.df$date, na.rm = TRUE), units = "mins") > UserMins) {    ### MG , I doubt about the tz here, I think all is changed to UTM, as it is a difference maybe it does not matter
                        Retrieve.data.General  = TRUE
                        futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " sensor data should be retrieved. Start date for data download: ", DateEND.General.prev))
                    } else {
                        Retrieve.data.General  = FALSE
                        futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " no sensor data do not have to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."))
                    }
                }
            }
        }
        # checking if RefData exists
        if (shiny::isTruthy(RefData)) {
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
                futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " reference data should be retrieved. Start new reference data at : ", DateEND.Ref.prev, "."))
            } else {
                Retrieve.data.Ref = FALSE
                futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " reference data do not have to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."))
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
                futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " ", Ref.file, " base::Sys.time(), data will be retrieved."))
            } else {
                # Ref.file exists
                ExistFil.data.Ref = TRUE
                futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " ", Ref.file, " exists."))
                if (file_ext(Ref.file) == "csv") {
                    RefData <- data.table::fread(file = Ref.file, na.strings = c("","NA", "<NA>"))
                    if (class(RefData$date) == "integer")
                        if (class(RefData$date) == "integer") data.table::set(RefData, j = "date", value = as.POSIXct(RefData$date, origin = "1970-01-01", TZ = "UTC"))
                } else if (file_ext(Ref.file) == "Rdata") load(Ref.file)
                if (shiny::isTruthy(RefData)) {
                    # RefData exists and is not NULL
                    # # Not considering end rows with only NA values for sensors
                    ind <- apply(RefData[, which(names(RefData) != "date"), with = FALSE], 1, function(x) !all(is.na(x)))
                    DateIN.Ref.prev  <- min(RefData$date[ind], na.rm = TRUE)
                    DateEND.Ref.prev <- max(RefData$date[ind], na.rm = TRUE)
                    Var.Ref.prev     <- names(RefData)
                    # Checking if Download of RefData is necessary
                    if (difftime(Sys.time(), DateEND.Ref.prev, units = "mins") > UserMins) {    ### MG , I doubt about the tz here, I think all is changed to UTC, as it is a difference maybe it does not matter
                        Retrieve.data.Ref = TRUE
                        futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " reference data are should retrieved. Start new reference data at : ", DateEND.Ref.prev, "."))
                    } else {
                        Retrieve.data.Ref = FALSE
                        futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " reference data do not have to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."))}
                } else {
                    # RefData exists but it is NULL
                    Retrieve.data.Ref <- TRUE
                    DateIN.Ref.prev   <- NULL
                    DateEND.Ref.prev  <- NULL
                    Var.Ref.prev      <- NULL
                    futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " ", Ref.file, " is NULL (no values). It is going to be created, data will be retrieved."))}}}
        # checking if InfluxData exists
        if (shiny::isTruthy(InfluxData)) {
            # Influx.file exists if data exist
            ExistFil.data.Influx  = TRUE
            # InfluxData exists and is not NULL
            DateIN.Influx.prev  <- min(InfluxData$date, na.rm = TRUE)
            DateEND.Influx.prev <- max(InfluxData$date, na.rm = TRUE)
            # Checking if Download of InfluxData is necessary
            if (difftime(Sys.time(), DateEND.Influx.prev, units = "mins") > UserMins) {    
                Retrieve.data.Influx  = TRUE
                futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " sensor data should be retrieved. Start date for data download: ", DateEND.Influx.prev, "."))
            } else {
                Retrieve.data.Influx  = FALSE
                futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " no sensor data do not have to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."))}
        } else {
            if (!file.exists(Influx.file)) {
                # InfluxData does not exist
                ExistFil.data.Influx  = FALSE
                Retrieve.data.Influx  = TRUE
                DateIN.Influx.prev    = NULL
                DateEND.Influx.prev   = NULL
                futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " ", Influx.file, " should be created and sensor data shall be downloaded."))
            } else {
                # Influx.file exists
                ExistFil.data.Influx  = TRUE
                futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " ", Influx.file, " exists."))
                if (file_ext(Influx.file) == "csv") {
                    InfluxData <- data.table::fread(file = Influx.file, na.strings = c("","NA", "<NA>"), select = "date")
                } else if (file_ext(Influx.file) == "Rdata") load(Influx.file)
                if (shiny::isTruthy(InfluxData)) {
                    # InfluxData exists and is not NULL
                    DateIN.Influx.prev  <- min(InfluxData$date, na.rm = TRUE)
                    DateEND.Influx.prev <- max(InfluxData$date, na.rm = TRUE)
                    # Checking if Download of InfluxData is necessary
                    if (difftime(Sys.time(), DateEND.Influx.prev, units = "mins") > UserMins) {    
                        Retrieve.data.Influx  = TRUE
                        futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " sensor data should be retrieved. Start date for data download: ", DateEND.Influx.prev, "."))
                    } else {
                        Retrieve.data.Influx  = FALSE
                        futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " no sensor data do not have to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."))}
                } else {
                    # InfluxData exists but it is NULL
                    Retrieve.data.Ref   = TRUE
                    DateIN.Influx.prev  = NULL
                    DateEND.Influx.prev = NULL
                    futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " ", Influx.file, " is NULL (no values). It is going to be created, data will be retrieved."))}}}
        # checking if SOSData exists
        if (shiny::isTruthy(SOSData)) {
            # SOS.file exists if data exist
            ExistFil.data.SOS     = TRUE
            # SOSDataSOSData exists and is not NULL
            DateIN.SOS.prev      <- min(SOSData$date, na.rm = TRUE)
            DateEND.SOS.prev     <- max(SOSData$date, na.rm = TRUE)
            # Checking if Download of SOSData is necessary
            if (difftime(Sys.time(), max(SOSData$date, na.rm = TRUE), units = "mins") > UserMins) {    ### MG , I doubt about the tz here, I think all is changed to UTM, as it is a difference maybe it does not matter
                Retrieve.data.SOS   = TRUE
                futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " SOS sensor data should be retrieved. Start date for data download: ", DateEND.Influx.prev, "."))
            } else {
                Retrieve.data.SOS   = FALSE
                futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " no SOS sensor data do not have to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."))}
        } else {
            if (!file.exists(SOS.file)) {
                # SOS.file does not exist
                ExistFil.data.SOS     = FALSE
                Retrieve.data.SOS   = TRUE
                DateIN.SOS.prev     = NULL
                DateEND.SOS.prev    = NULL
                futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " ", SOS.file, " does not exist. It should be created, SOS sensor data should be retrieved."))
            } else {
                # SOS.file exists
                ExistFil.data.SOS     = TRUE
                futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " ", SOS.file, " exists."))
                if (file_ext(SOS.file) == "csv") {
                    SOSData <- data.table::fread(file = SOS.file, na.strings = c("","NA", "<NA>"), select = "date")
                } else if (file_ext(SOS.file) == "Rdata") load(SOS.file)
                load()
                if (shiny::isTruthy(SOSData)) {
                    # SOSDataSOSData exists and is not NULL
                    DateIN.SOS.prev      <- min(SOSData$date, na.rm = TRUE)
                    DateEND.SOS.prev     <- max(SOSData$date, na.rm = TRUE)
                    # Checking if Download of SOSData is necessary
                    if (difftime(Sys.time(), max(SOSData$date, na.rm = TRUE), units = "mins") > UserMins) {    ### MG , I doubt about the tz here, I think all is changed to UTM, as it is a difference maybe it does not matter
                        Retrieve.data.SOS   = TRUE
                        futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " SOS sensor data should be retrieved. Start date for data download: ", DateEND.Influx.prev, "."))
                    } else {
                        Retrieve.data.SOS   = FALSE
                        futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " no SOS sensor data do not have to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."))}
                } else {
                    # SOSData exists but it is NULL
                    Retrieve.data.SOS   = TRUE
                    DateIN.SOS.prev     = NULL
                    DateEND.SOS.prev    = NULL
                    futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " ", SOS.file, " is NULL (no values). It is going to be created, data will be retrieved."))}}}
        
        if (!file.exists(airsenseur.db.file)) {
            # airsenseur.db.file does not exist
            ExistFil.data.db      = FALSE
            Retrieve.data.db      = TRUE
            DateIN.db.prev        = NULL
            DateEND.db.prev       = NULL
            futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " ", airsenseur.db.file, " does not exist. It should be created, SOS sensor data should be retrieved."))
        } else {
            # airsenseur.db.file exists
            ExistFil.data.db      = TRUE
            futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " ", airsenseur.db.file, " exists."))
            # Checking table Dataset in airsenseur.db
            SQLite.con <- DBI::dbConnect(RSQLite::SQLite(), dbname = airsenseur.db.file)
            # Checking if the SQLite.con database and the table Dataset exists?
            list.Tables       <- DBI::dbListTables(SQLite.con)
            if (length(list.Tables) > 0 && any(grepl("_Cast", list.Tables))) {
                Table <- list.Tables[grep("_Cast", list.Tables)]   
            } else {
                if (any(grepl(Influx.name, list.Tables))) {
                    Table <- list.Tables[grepl(Influx.name, list.Tables)]
                } else {
                    futile.logger::flog.error(paste0("Tables ", Influx.name, " ",Influx.name, "not found in airsenseur.db."))}}
            if (exists("Table") && length(Table) > 0) {
                if (length(Table) == 1) {
                    futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " ", airsenseur.db.file, " includes the table ", Table," with the columns: ",
                                                    paste0(DBI::dbListFields(SQLite.con, Table), collapse = ", ") ))
                } else {
                    futile.logger::flog.warn(paste0("[Check_Download] ", Influx.name, " ", airsenseur.db.file, " has been mixed up with several datasets. It includes the table ", paste(Table, collapse = ", ")))
                    if (any(grepl(pattern = Influx.name, Table))) {
                        Table <- Table[grep(pattern = Influx.name, Table)]
                        futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " only Table ",Table, " is considered"))
                    } else {
                        stop("[Check_Download] ", airsenseur.db.file, " includes the several datasets. None of them being", Influx.name,".\n The script is stopped. Delete airsenseur.db and restart influx download")}}
                # DataSet in airsenseur.db exists and is not NULL. Looking for starting ting and ending time
                # The following code is slow
                # DateIN.db.prev      <- DBI::dbGetQuery(SQLite.con, paste0("SELECT time FROM \"", Table, "\"", " ORDER BY time ASC LIMIT 1;"))[1,1] # slower than line above
                # DateEND.db.prev     <- DBI::dbGetQuery(SQLite.con, paste0("SELECT time FROM \"", Table, "\"", " ORDER BY time DESC LIMIT 1;"))[1,1] # slower than line above
                # DateIN.db.prev      <- Set_date2POSIXct(DBI::dbGetQuery(SQLite.con, paste0("SELECT min(time) FROM \"", Table, "\""))[1,1])
                # DateEND.db.prev     <- Set_date2POSIXct(DBI::dbGetQuery(SQLite.con, paste0("SELECT max(time) FROM \"", Table, "\""))[1,1])
                # The following code is very fast and use that time is ordered as rowid #### Please check with Marco ####
                DateIN.db.prev      <- Set_date2POSIXct(DBI::dbGetQuery(SQLite.con, paste0("SELECT time FROM \"", Table, "\" ORDER BY rowid asc LIMIT 1;"))[1,1])
                DateEND.db.prev     <- Set_date2POSIXct(DBI::dbGetQuery(SQLite.con, paste0("SELECT time FROM \"", Table, "\" ORDER BY rowid desc LIMIT 1;"))[1,1])
                DBI::dbDisconnect(conn = SQLite.con)
                # Checking if Download of InfluxData is necessary
                if (difftime(Sys.time(), DateEND.db.prev , units = "mins") > UserMins) {    ### MG , I doubt about the tz here, I think all is changed to UTM, as it is a difference maybe it does not matter
                    Retrieve.data.db   = TRUE
                    futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " sensor data should be retrieved. Start date for data download: ", DateEND.Influx.prev, ""))
                } else {
                    Retrieve.data.db   = FALSE
                    futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " no sensor data do not have to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."))
                }
            } else {
                # DataSet in airsenseur.db does not exist
                Retrieve.data.db      = TRUE
                DateIN.db.prev        = NULL
                DateEND.db.prev       = NULL
                futile.logger::flog.info(paste0("[Check_Download] ", Influx.name, " ", airsenseur.db.file, " is NULL (no values). It is going to be created, data will be retrieved."))}}}
    
    # Showing DownloadSens
    Returned_list <- list(Ref.Rdata.file     = Ref.file,
                          Influx.Rdata.file  = Influx.file,
                          SOS.file           = SOS.file,
                          General.Rdata.file = General.file,
                          airsenseur.db.file = airsenseur.db.file,
                          WDinput            = WDinput,
                          ExistFil.data.db     = ExistFil.data.db     , Retrieve.data.db     = Retrieve.data.db     , DateIN.db.prev       = DateIN.db.prev,      DateEND.db.prev       = DateEND.db.prev,
                          ExistFil.data.Ref    = ExistFil.data.Ref    , Retrieve.data.Ref    = Retrieve.data.Ref    , DateIN.Ref.prev      = DateIN.Ref.prev,     DateEND.Ref.prev      = DateEND.Ref.prev, Var.Ref.prev = Var.Ref.prev,
                          ExistFil.data.Influx = ExistFil.data.Influx , Retrieve.data.Influx = Retrieve.data.Influx , DateIN.Influx.prev   = DateIN.Influx.prev,  DateEND.Influx.prev   = DateEND.Influx.prev,
                          ExistFil.data.SOS    = ExistFil.data.SOS    , Retrieve.data.SOS    = Retrieve.data.SOS    , DateIN.SOS.prev      = DateIN.SOS.prev,     DateEND.SOS.prev      = DateEND.SOS.prev,
                          ExistFil.data.General= ExistFil.data.General, Retrieve.data.General= Retrieve.data.General, DateIN.General.prev  = DateIN.General.prev, DateEND.General.prev  = DateEND.General.prev)
    if (Verbose) print(Returned_list)
    cat("-----------------------------------------------------------------------------------\n")
    #})
    return(Returned_list)
}
#=====================================================================================CR
# 161120 MG : Json_To_df Downloading AirSensEUR.db data using the Influx protocol, create or update the airsenseur.db SQLite database, get timezone ####
#=====================================================================================CR
#' convert Json to data.frame
#' @param JSON      : class "response" as returned by function httr::GET for INFLUX
#' @param Numeric   : character vector with the colnames df to be converted into numeric
#' @param Discard   : character vector, default is NULL. column names to drop from the returned dataframe
#' @parma verbose   : logical default is FALSE. If TRUE message are displayed
Json_To_df <- function(JSON, Numeric = NULL, verbose = FALSE, Discard = NULL) {
    # 
    #
    # Returns a df a query data coverting from JSON of INFLUX with conversion of data columns from string to numeric
    if (JSON$status_code != 200) {
        Error.Message <- gsub(pattern = "%20", replacement = " ", JSON$url)
        Error.Message <- gsub(pattern = "%3D", replacement = "= ", Error.Message)
        Error.Message <- gsub(pattern = "%3B", replacement = ";", Error.Message)
        Error.Message <- gsub(pattern = "%22", replacement = "\"", Error.Message)
        Error.Message <- gsub(pattern = "%2C", replacement = "'", Error.Message)
        print(JSON, quote = FALSE)
        if (verbose) futile.logger::flog.info("[Json_To_df] Response to ",Error.Message,"")
        return(futile.logger::flog.error(paste0("[Json_To_df] ERROR, query returning error status code ",JSON$status_code, ". The query is wrong or the Influx server may be down.")))
    } else {
        JSON <- jsonlite::fromJSON(content(JSON, "text", encoding = "ISO-8859-1"), simplifyVector = TRUE, flatten = T)
        # Checking that JSON is not empty, e. g. there was no data for the Key or tag selected
        if (any(names(JSON) %in% "results")) {
            if (any(names(JSON$results) %in% "series")) {
                # delete "mean_" in case of query mean
                JSON.Colnames <- gsub(pattern = "mean_", replacement= "",JSON$results$series[[1]]$columns[[1]])
                if (verbose) futile.logger::flog.info(paste0("[Json_To_df] columns in JSON object: ",paste0(JSON.Colnames, collapse = ", "),""))
                JSON <- data.table::setnames(data.frame(JSON$results$series[[1]]$values,
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
        return(JSON)}}
#=====================================================================================CR
#  Down_Influx Downloading AirSensEUR.db data using the Influx protocol, create or update the airsenseur.db SQLite database, get timezone ####
#=====================================================================================CR
#' Down_Influx downloads AirSensEUR.db data from an Influx server using JSON (package Jsonlite) ---
#'
#' Parameters for Internet connection:
#' @param PROXY          : Logical, default value FALSE. If TRUE PROXY is configured using the following 4 parameters:
#' @param PORT           : numeric, default value NULL, open Port for the proxy, jrc = 8012;
#' @param LOGIN          : character, default value = NULL, login for the proxy server, JRC = NULL;
#' @param PASSWORD       : character, default value = NULL, password for the proxy server, jrc = NULL;
#' Parameters for the Influx download:
#' @param Host           : character, mandatory, url of the Influx server, jrc = 'influxdb1.liberaintentio.com', without "http://";
#' @param Port           : numeric, default value = 8086, port used for the Influx transfer, the port must be an open in your browser;
#' @param User           : character, your login at the Influx server, jrc = "jrcuser";
#' @param Pass           : character, your password at the Influx server, jrc = "???";
#' @param name.SQLite    : character, path + name of the airsenseur.Db file, it shall be in the General.data directory
#' @param name.SQLite.old: character, backup of name.SQLite. No more used, set to NULL.
#' @param Db             : character, name of the database at the Influx server, e.g. jrc = "jrcispra",
#' @param Dataset        : character, name of the table(Dataset) in the database Db that you want to download, e. g. "ASE_45058D"
#' @param Influx.TZ      : character or list with elements Influx.TZ, FirstDate and LastDate. Default value NULL. If NULL or "local time", the function Down_Influx will try to determine the time zone otherwise Influx.TZ ("UTC") will be used
#' @param Sensor.Date    : character vector with 2 strings, default is NULL, Range of date for Influx download, 1st string is the starting date (YYY-MM-DD) for Influx download, 2nd string is the ending date.
#' @param Page           : numeric, default value 200 (LImit Influx_query). If Null the size of the page of data to download from the influx server is : LIMIT 10000, as requested in the Influx
#' @param Mean           : numeric, default value 1 (Group by time(1m)), time average for the download of Influx data,
#' @param use_google     : logical: default = TRUE, if TRUE the google API is used for detecting time zone from coordinates (require port 443), deprecated using package lutz instaed of rundel/timezone

#' @return create the local database airsenseur.db in name.SQLite and return the time zone determined by lutz (LastLong, Lastlat) and LastDate in airsenseur.db
#' @examples Down_Influx(PROXY = PROXY, URL = URL  , PORT = as.numeric(PORT), LOGIN = LOGIN, PASSWORD = PASSWORD, Host = Host  , User = User, Port = as.numeric(Port), Pass = Pass, name.SQLite = name.SQLite, name.SQLite.old = name.SQLite.old, Db = Db, Dataset = Dataset, Influx.TZ = Influx.TZ, use_google = FALSE, Page = 10000, Mean = as.numeric(UserMins)), 
Down_Influx <- function(PROXY = FALSE, URL = NULL, PORT = NULL, LOGIN = NULL, PASSWORD = NULL, Host, Port = 8086, User, Pass, 
                        name.SQLite, name.SQLite.old = NULL,  Db, Dataset, Influx.TZ = NULL, Sensor.Date = NULL, Page = 200, Mean = 1, use_google = TRUE, Verbose = F) {
    cat("\n")
    cat("-----------------------------------------------------------------------------------\n")
    
    # Checking internet connection availability
    if (!curl::has_internet()) return(futile.logger::flog.error(paste0("[Down_Influx] ", Dataset, " no internet connection. InfluxDB download cannot be carried out.")))
    
    # create influx connection object and getting number of records
    if (PROXY) {
        if (is.null(LOGIN)) httr::set_config(use_proxy(url=URL, port=PORT)) else httr::set_config( use_proxy(url=URL, port=PORT, username = LOGIN, password = PASSWORD))
    } else httr::reset_config()
    # checking if Host can be pinged
    if (PingThisSite(gsub('http://', '', Host))) {
        futile.logger::flog.info(paste0("[Down_Influx] ", Dataset, " ping to ", Host, " Ok"))
    } else return(futile.logger::flog.error(paste0("[Down_Influx] ", Dataset, " you have internet connection but can't ping to ",Host,". InfluxDB download cannot be carried out.")))
    
    Influx.con <- httr::GET(paste0("http://",Host,":",Port,"/ping"), config = authenticate(user = User, password = Pass, type = "basic"))
    if (Influx.con$status_code != 204) {
        futile.logger::flog.error(paste0("[Down_Influx] ", Dataset, " Influx server is down. Stopping the script."))
        return(futile.logger::flog.error(paste0("[Down_Influx] ", Dataset, " Influx server is down. Stopping the script.")))
    } else futile.logger::flog.info(paste0("[Down_Influx] ", Dataset, " Influx server is up; connected to server"))
    Influx.Last <- httr::GET(utils::URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)),
                             config = authenticate(user = User, password = Pass, type = "basic"),
                             query = list(q = paste0("SELECT boardTimeStamp,time FROM \"", Dataset,"\" ORDER BY time DESC LIMIT 1;")))
    Influx.Last <- lubridate::ymd_hms(Json_To_df(Influx.Last, Numeric = "last")$time, tz = ifelse(class(Influx.TZ)=="list",Influx.TZ$Influx.TZ,Influx.TZ))
    
    # Downloading Influxdb data in airsenseur.db:
    # if airsenseur.db does not exist       -->     create database
    # if airsenseur.db exists:
    # if the table Dataset does not exist   -->     create the table and download all influx data
    # if the table Dataset  exists          -->     download only new data to the table dataset
    #------------------------------------------------------------------------------CR
    # AirsensEur.db exists? Make a copy if dates are different from Old db. Connect to the db
    #------------------------------------------------------------------------------CR
    if (file.exists(name.SQLite)) { # airsenseur.db exists
        futile.logger::flog.info(paste0("[Down_Influx] ", Dataset, " ", name.SQLite, " already exists."))
    } else {
        # airsenseur.db does not exist
        futile.logger::flog.warn(paste0("[Down_Influx] ", Dataset, " ", name.SQLite, " does not exist and it is going to be created."))}
    SQLite.con <- DBI::dbConnect(RSQLite::SQLite(), dbname = name.SQLite)
    #------------------------------------------------------------------------------CR
    # table dataset exists?
    #------------------------------------------------------------------------------CR
    if (DBI::dbExistsTable(conn = SQLite.con, name = paste0(Dataset,"_Cast" ))|| DBI::dbExistsTable(conn = SQLite.con, name = Dataset)) { 
        # the table Dataset exists in airsenseur.db
        futile.logger::flog.info(paste0("[Down_Influx] ", Dataset, " the table ", Dataset, "_Cast already exists in airsenseur.db."))
        if (DBI::dbExistsTable(conn = SQLite.con, name = paste0(Dataset,"_Cast"))) {
            Dataset.N               <- DBI::dbGetQuery(SQLite.con, paste0("SELECT rowid FROM \"", paste0(Dataset,"_Cast"), "\" ORDER BY rowid DESC LIMIT 1;"))[1,1]
            SQL.time.Last           <- DBI::dbGetQuery(SQLite.con, paste0("SELECT * FROM \"", paste0(Dataset,"_Cast"),"\" ORDER BY rowid DESC LIMIT 1;"))$time
            # deleting the sequential table DataSet, no more used. Now DataSet_cast is used
            if (DBI::dbExistsTable(conn = SQLite.con, name = Dataset)) {
                DBI::dbRemoveTable(conn = SQLite.con, name = Dataset)
                DBI::dbExecute(conn = SQLite.con, "VACUUM;")}
        } else {
            DT.SQL.query <- data.table::data.table(DBI::dbGetQuery(SQLite.con, paste0("SELECT * FROM \"", Dataset,"\"  order by rowid")))
            # remove duplicated if any
            Duplicated.rows <- which(duplicated(DT.SQL.query))
            if (length(Duplicated.rows) > 0) DT.SQL.query <- DT.SQL.query[-Duplicated.rows]
            DT.SQL.query <- dcast(DT.SQL.query, time + altitude + boardTimeStamp + gpsTimestamp + latitude + longitude ~ name, value.var = "sampleEvaluatedVal", fill = NA)
            RSQLite::dbWriteTable(conn = SQLite.con, name = paste0(Dataset,"_Cast"), value = DT.SQL.query, overwrite = T)
            # Counting the number of records in AirSensEUR$Dataset - This will work provided that all rowid exist.
            Dataset.N               <- DBI::dbGetQuery(SQLite.con, paste0("SELECT max(rowid) FROM \"", Dataset, "\""))[1,1]
            SQL.time.Last           <- DBI::dbGetQuery(SQLite.con, paste0("SELECT * FROM \"", Dataset,"\" ORDER BY rowid DESC LIMIT 1;"))$time
            # deleting the table DataSet, not used. From now DataSet_cast will be used
            DBI::dbRemoveTable(conn = SQLite.con, name = Dataset)
            DBI::dbExecute(conn = SQLite.con, "VACUUM;")}
        Dataset.index   <- FALSE # if airsenseur.db exists then the indexes were already created, then no need to create the indexes
    } else {# the table Dataset does not exist in airsenseur.db
        # There are no records in AirSensEUR$Dataset
        futile.logger::flog.warn(paste0("[Down_Influx] ", Dataset, " the table ", Dataset, "_Cast does not exist in airsenseur.db. It is going to be created."))
        Dataset.N       <- 0
        # Get SQL.time.Last as First GPS time (and the timestamp together) of InfluxDB
        SQL.time.Last <- httr::GET(utils::URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)),
                                   config = authenticate(user = User, password = Pass, type = "basic"),
                                   query = list(q = paste0("SELECT  FIRST(sampleEvaluatedVal) FROM \"", Dataset, "\"")))
        if (SQL.time.Last$status_code != 200) futile.logger::flog.error(paste0("[Down_Influx] ", Dataset, " query first sampleEvaluatedVal in airsenseur.db. Influx server may be down."))
        SQL.time.Last <- Json_To_df(SQL.time.Last)$time
        # if true indexes will be created
        Dataset.index   <- TRUE
    } 
    
    #------------------------------------------------------------------------------CR
    # Downloading InfluxDB data and add them to airsenseur.db
    #------------------------------------------------------------------------------CR
    # List of sensors to download
    Influx.Sensor <-  httr::GET(utils::URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)),
                                config = authenticate(user = User, password = Pass, type = "basic"),
                                query = list(q = paste0("SHOW TAG VALUES FROM \"", Dataset,"\" WITH KEY IN (\"name\") ; ")))
    Influx.Sensor <- Json_To_df(Influx.Sensor)
    colnames(Influx.Sensor)[colnames(Influx.Sensor) == "value"] <- "name";
    # Adding channel for each Sensor names
    # deleting rows with name in c("L2942CUR","L2942VOL","L4156STA") which have periodicity of 10 minutes making the calculation of boardtime not continuous every 11 minutes
    if (any(grepl(pattern = paste(c("L2942CUR","L2942VOL","L4156STA"), collapse = "|"),Influx.Sensor$name))) Influx.Sensor <- Influx.Sensor[-which(Influx.Sensor$name %in% c("L2942CUR","L2942VOL","L4156STA")),]
    if (DBI::dbExistsTable(conn = SQLite.con, name = "Channel.names")) Influx.Channel.number <- DBI::dbGetQuery(SQLite.con, paste0("SELECT * FROM \"Channel.names\""))
    if (exists("Influx.Channel.number") && all(Influx.Sensor$name %in% Influx.Channel.number$name)) {
        Influx.Sensor <- Influx.Channel.number
    } else {
        for (i in Influx.Sensor$name) {
            # altitude needed because channel and name are Key Field (TAGs)
            Influx.Channel.number <- httr::GET(utils::URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)),
                                               config = authenticate(user = User, password = Pass, type = "basic"),
                                               query = list(q = paste0("SELECT altitude, channel, \"name\" FROM \"", 
                                                                       Dataset,"\" WHERE \"name\" = '",i,"' LIMIT 1;")))
            Influx.Channel.number <- Json_To_df(Influx.Channel.number)
            Influx.Sensor[which(Influx.Sensor$name == i),"channel"] <- Influx.Channel.number$channel
            Influx.Sensor[which(Influx.Sensor$name == i),"time"]    <- Influx.Channel.number$time}
        Influx.Sensor <- Influx.Sensor[order(Influx.Sensor$time),]
        # Adding to database
        RSQLite::dbWriteTable(conn = SQLite.con, name = "Channel.names", value = Influx.Sensor, overwrite = TRUE)}
    if (Verbose) print(Influx.Sensor, quote = FALSE)
    
    # Using desired first and last dates
    if(!is.null(Sensor.Date) && Sensor.Date[1] > SQL.time.Last) SQL.time.Last <- Sensor.Date[1]
    if(!is.null(Sensor.Date) && Sensor.Date[2] < Influx.Last) Influx.Last <- Sensor.Date[2]
    # Convert SQL.time.Last to POSIXct
    SQL.time.Last <- Set_date2POSIXct(SQL.time.Last, tz = "UTC")
    
    # Error Message and stop the script if there more data in the airsenseur.db than in InfluxDB
    if (difftime(Influx.Last, SQL.time.Last, units = "mins") < Mean) {
        futile.logger::flog.info(paste0("[Down_Influx] ", Dataset, " Downloading is up to date. No need for data download."))
    } else futile.logger::flog.info(paste0("[Down_Influx] ", Dataset, " records between ",format(SQL.time.Last,"%Y-%m-%d %H:%M")," and ",format(Influx.Last,"%Y-%m-%d %H:%M"), 
                                           " are going to be added into the table ", paste0(Dataset,"_cast"), " of airsenseur.db."))
    # Downloading always in increasing date, max download data in InfuxDB: chunks of 10000
    NbofDays.For10000data <- 10000/(24*60/Mean)/length(Influx.Sensor$name)
    # Number of seconds corresponding to NbofDays.For10000data
    Step <- NbofDays.For10000data * 24 * 60 * 60
    while (difftime(Influx.Last, SQL.time.Last, units = "mins") > Mean) {
        # Downloading from Influx server using httr, query different for Mean = 1 min <> 1 min (average needed, takes more time)
        if ( Mean == 1) {
            # Do not use LIMIT 10000 it is slow
            Mean.Query <- httr::GET(utils::URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)),
                                    config = authenticate(user = User, password = Pass, type = "basic"),
                                    query = list(q = paste0("SELECT * FROM \"", Dataset,"\" WHERE ",
                                                            paste(c(paste0(paste0("\"name\" = '",Influx.Sensor$name,"'"), collapse = " OR ")), collapse = ""),
                                                            " AND time >  '",format(SQL.time.Last,"%Y-%m-%d %H:%M:%OS3"),"'",
                                                            " AND time <= '",format(SQL.time.Last+Step,"%Y-%m-%d %H:%M:%OS3"),"'")))
        } else {
            Mean.Query <- httr::GET(utils::URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)),
                                    config = authenticate(user = User, password = Pass, type = "basic"),
                                    query = list(q = paste0("SELECT mean(*) FROM \"", Dataset, "\" WHERE ",
                                                            paste(c(paste0(paste0("\"name\" = '",Influx.Sensor$name,"'"), collapse = " OR ")), collapse = ""),
                                                            " AND time > '",format(SQL.time.Last,"%Y-%m-%d %H:%M:%OS3"),
                                                            "' AND time <= '", format(SQL.time.Last + Step,"%Y-%m-%d %H:%M:%OS3"),"' GROUP BY time(",Mean,"m);")))
            # LIMIT " , format(round(1440/Mean), scientific = FALSE), " OFFSET ", format(0, scientific = FALSE)
        }
        # Checking good query status code
        if (Mean.Query$status_code != 200) {
            futile.logger::flog.error(paste0("[Down_Influx] ", Dataset, " does not succeed to query the influxDB with status_code <> 200."))
        } else {
            # extracting lists from json
            if (length(colnames(Json_To_df(Mean.Query))) > 1 ) {
                # calibrated is not necessary, we will create a new table in influx in cloud
                All.Sensors.Adding <- data.table::data.table(Json_To_df(Mean.Query,
                                                                        Numeric = c("altitude", "boardTimeStamp", "gpsTimestamp", "latitude", "longitude", "sampleEvaluatedVal", "sampleRawVal"),
                                                                        Discard = c("Calibrated","calibrated")))
                # Patch in case some sensors had channel number between 0 and 3 without being chemical sensors are code in complement to 2 and therefore wrongly converted
                Corr.Val <- which(All.Sensors.Adding$sampleEvaluatedVal > 2^16/2 & All.Sensors.Adding$sampleRawVal < 2^16/2 & All.Sensors.Adding$name %in% c("5325CST", "5301CST", "D300", "RD200M"))
                if (length(Corr.Val) > 0) data.table::set(All.Sensors.Adding, i = Corr.Val, j = "sampleEvaluatedVal", value = All.Sensors.Adding$sampleRawVal[Corr.Val])
                rm(Corr.Val)}}
        
        # adding data to airSensEUR.db
        if (exists("All.Sensors.Adding") && format(lubridate::ymd_hms(All.Sensors.Adding[.N,time]), "%Y-%m-%d %H:%m:%OS4") !=  format(SQL.time.Last, "%Y-%m-%d %H:%m:%OS4")) {
            # discarding rows with all NA Values
            NA.values <- which(
                rowSums(
                    is.na(All.Sensors.Adding[,-which(names(All.Sensors.Adding) %in% c("time","channel","name")), with = FALSE])) ==
                    ncol(All.Sensors.Adding[,-which(names(All.Sensors.Adding) %in% c("time","channel","name")), with = FALSE]))
            if (length(NA.values) > 0) All.Sensors.Adding <- All.Sensors.Adding[-NA.values,]
            # Appending to Dataset" 
            futile.logger::flog.info(paste0("[Down_Influx] ", Dataset, " ", format(nrow(All.Sensors.Adding), scientific = FALSE), " records added between ",
                                            format(lubridate::ymd_hms(All.Sensors.Adding[1,time]),"%Y-%m-%d %H:%M")," and ",format(lubridate::ymd_hms(All.Sensors.Adding[.N,time]),"%Y-%m-%d %H:%M")
                                            ," added to table ", paste0(Dataset,"_Cast")))
            # Appending to (Dataset,"_Cast"
            cast.All.Sensors.Adding <- data.table::dcast(All.Sensors.Adding, time + altitude + boardTimeStamp + gpsTimestamp + latitude + longitude ~ name, fill = NA, value.var = "sampleEvaluatedVal")
            # Avoid duplicate rows with same datetime, time becomes a POSIXct
            cast.All.Sensors.Adding <- Unique.DT.time(cast.All.Sensors.Adding)
            # format time in tet before dbWriteTable otherwise it saves it as numeric POSIXct
            if (DBI::dbExistsTable(SQLite.con, paste0(Dataset,"_Cast"))) {
                Names.db <- DBI::dbListFields(SQLite.con, paste0(Dataset,"_Cast"))
                Names    <- names(cast.All.Sensors.Adding)
                if (!all(Names %in% Names.db)) {
                    # adding new columns
                    New.Columns <- Names[!Names %in% Names.db]
                    for (i in New.Columns) dbExecute(SQLite.con, paste0("ALTER TABLE \"", paste0(Dataset,"_Cast"),"\" ADD COLUMN \"",i,"\" REAL;"))}}
            # updating SQL.time.Last for while loop before converting time to character
            SQL.time.Last  <- cast.All.Sensors.Adding[.N]$time
            # format time in text with millisecs before dbWriteTable() otherwise it saves time as numeric POSIXct
            data.table::set(cast.All.Sensors.Adding, j = "time",         value = format(cast.All.Sensors.Adding$time,"%Y-%m-%dT%H:%M:%OS3Z"))
            #data.table::set(cast.All.Sensors.Adding, j = "gpsTimestamp", value = format(cast.All.Sensors.Adding$gpsTimestamp,"%Y-%m-%dT%H:%M:%OS3Z"))
            # Updating SQLite table
            RSQLite::dbWriteTable(conn = SQLite.con, name = paste0(Dataset,"_Cast"), value = cast.All.Sensors.Adding, append = TRUE)
            if (nrow(All.Sensors.Adding) < 10000) Step <- Step * 2
            remove(All.Sensors.Adding, cast.All.Sensors.Adding)
        } else {
            futile.logger::flog.warn(paste0("[Down_Influx] ", Dataset, " No influx data between ", format(SQL.time.Last,"%Y-%m-%d %H:%M")," and ",format(SQL.time.Last + Step,"%Y-%m-%d %H:%M"),"."))
            # updating SQL.time.Last for while loop with next row
            Next.date.Query <- httr::GET(utils::URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)),
                                         config = authenticate(user = User, password = Pass, type = "basic"),
                                         query = list(q = paste0("SELECT * FROM \"", Dataset,
                                                                 "\" WHERE ", paste(c(paste0(paste0("\"name\" = '",Influx.Sensor$name,"'"), collapse = " OR ")), collapse = ""),
                                                                 "AND time > '",format(SQL.time.Last + Step,"%Y-%m-%d %H:%M:%OS3"), "' ORDER BY time ASC LIMIT 1 ")))
            # Checking good query status code
            if (Next.date.Query$status_code != 200) {
                futile.logger::flog.warn(paste0("[Down_Influx] ", Dataset, " does not succed to query the influxDB with status_code <> 200. Likely there is no more data for ",Influx.Sensor[j,"name"],""))
                # updating SQL.time.Last for while loop
                SQL.time.Last  <- Influx.Last
            } else {
                # extracting lists from json
                Next.date <- Json_To_df(Next.date.Query,
                                        Numeric = c("altitude", "boardTimeStamp", "gpsTimestamp", "latitude", "longitude", "sampleEvaluatedVal", "sampleRawVal", "channel"),
                                        Discard = c("Calibrated","calibrated"))
                # updating SQL.time.Last for while loop
                SQL.time.Last  <- Set_date2POSIXct(Next.date$time, tz = ifelse(class(Influx.TZ)=="list",Influx.TZ$Influx.TZ,Influx.TZ)) - Mean * 60}}} # in case the ASE boxes give data with periodicity lower than Mean
    futile.logger::flog.info(paste0("[Down_Influx] ", Dataset, " the downloading of sensor data from the Influx server is finished."))
    # Counting the number of records in AirSensEUR$Dataset
    Dataset.N   <- DBI::dbGetQuery(SQLite.con, paste0("SELECT max(rowid) FROM \"", paste0(Dataset,"_Cast"), "\""))[1,1]
    
    # getting the time zone, port 443 of the Browser shall be opened
    if (is.null(ifelse(class(Influx.TZ)=="list",Influx.TZ$Influx.TZ,Influx.TZ)) || ifelse(class(Influx.TZ)=="list",Influx.TZ$Influx.TZ,Influx.TZ) == "Local time") {
        futile.logger::flog.info(paste0("[Down_influx] ", Dataset, " determining the time zone with the last valid latitude and longitude of ", Dataset, " in airsenseur.db."))
        Offset <- Dataset.N
        repeat {
            Coord.lat.long   <- DBI::dbGetQuery(SQLite.con, paste0("SELECT time, longitude, latitude FROM \"", Dataset, "\" WHERE rowid > ", Offset - 500, " AND rowid <= ", Offset, " ;"))
            if (all(is.na.data.frame(Coord.lat.long[,c("longitude","latitude")])) ||
                all(Coord.lat.long[!is.na.data.frame(Coord.lat.long[,c("longitude")]),c("longitude","latitude")] == 0)) {
                if (Offset > 500) {
                    Offset <- Offset - 500
                } else {
                    futile.logger::flog.warn(paste0("[Down_influx] ", Dataset, " impossible to determine the time zone of the sensor data. TZ is kept as ", ifelse(class(Influx.TZ)=="list",Influx.TZ$Influx.TZ,Influx.TZ)))
                    break}
            } else {
                Lastlat   <- tail(na.omit(Coord.lat.long$latitude[Coord.lat.long$latitude != 0]), n = 1)
                LastLong  <- tail(na.omit(Coord.lat.long$longitude[Coord.lat.long$longitude != 0]), n = 1)
                require(lutz)
                Influx.TZ <- lutz::tz_lookup_coords(LastLong, Lastlat)
                futile.logger::flog.info(paste0("[Down_influx] ", Dataset, " the time zone of the sensor data is ", Influx.TZ))
                break}}}
    # # getting the last date, latitude and longitude in name.SQLite.
    # Last date to add only new data, latitude and longitude to get the time zone
    # checking for non zero values and no NA()
    LastDate  <- Set_date2POSIXct(DBI::dbGetQuery(SQLite.con, paste0("SELECT time FROM \"", paste0(Dataset,"_Cast"),"\" ORDER BY rowid DESC LIMIT 1;"))$time, tz = ifelse(class(Influx.TZ)=="list",Influx.TZ$Influx.TZ,Influx.TZ))
    FirstDate <- Set_date2POSIXct(DBI::dbGetQuery(SQLite.con, paste0("SELECT time FROM \"", paste0(Dataset,"_Cast"),"\" ORDER BY rowid ASC LIMIT 1;"))$time, tz = ifelse(class(Influx.TZ)=="list",Influx.TZ$Influx.TZ,Influx.TZ))
    # # InfluxDB gives everything in UTC not in local time zone - Well by observation in grafana it seems that the dates are in Local Time
    
    # looking for table _Board and _Sensors, saving in directory Confiiguration
    series <- httr::GET(utils::URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)),
                        config = authenticate(user = User, password = Pass),
                        query = list(q = "SHOW SERIES"))
    series <- jsonlite::fromJSON(content(series, "text", encoding = "ISO-8859-1"), simplifyVector = TRUE, flatten = TRUE)
    series <- series$results$series[[1]]$values[[1]]
    series <- unique(sapply(strsplit(x = series, split = ","),function(x) x[1]))
    series <- series[grepl(pattern = paste(c("_Boards", "_Sensors"), collapse = "|"), x = series)]
    if (any(grepl(pattern = Dataset, series))) {
        Boards <- httr::GET(utils::URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)),
                            config = authenticate(user = User, password = Pass, type = "basic"),
                            query = list(q = paste0("SELECT * FROM \"", Dataset,"_Boards\"")))
        Boards <- jsonlite::fromJSON(content(Boards, "text", encoding = "ISO-8859-1"), simplifyVector = TRUE, flatten = TRUE)
        names.Boards <- Boards$results$series[[1]]$columns[[1]]
        Boards <- data.table::data.table(Boards$results$series[[1]]$values[[1]])
        data.table::setnames(Boards,names.Boards)
        data.table::fwrite(Boards, file.path(dirname(dirname(SQLite.con@dbname)), "Configuration","Boards.cfg"))
        Sensors <- httr::GET(utils::URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)),
                             config = authenticate(user = User, password = Pass, type = "basic"),
                             query = list(q = paste0("SELECT * FROM \"", Dataset,"_Sensors\"")))
        Sensors <- jsonlite::fromJSON(content(Sensors, "text", encoding = "ISO-8859-1"), simplifyVector = TRUE, flatten = TRUE)
        names.Sensors <- Sensors$results$series[[1]]$columns[[1]]
        Sensors <- data.table::data.table(Sensors$results$series[[1]]$values[[1]])
        data.table::setnames(Sensors,names.Sensors)
        data.table::fwrite(Sensors, file.path(dirname(dirname(SQLite.con@dbname)), "Configuration","Sensors.cfg"))}
    # Disconnect SQLite.con
    DBI::dbDisconnect(conn = SQLite.con)
    futile.logger::flog.info(paste0("[Down_Influx] ", Dataset, " dates in airsenseur.db go until ", format(LastDate, "%Y-%m-%d %H:%M"), ", with ", Dataset.N, " records for the table ", paste0(Dataset,"_cast")))
    cat("-----------------------------------------------------------------------------------\n")
    cat("\n")
    return(list(Influx.TZ = ifelse(class(Influx.TZ)=="list",Influx.TZ$Influx.TZ,Influx.TZ), LastDate = LastDate, FirstDate = FirstDate))
}
#=====================================================================================CR
# 161123 MG : Sqlite2df converting a local airsenseur.db into a General dataframe ####
#=====================================================================================CR
#' Return a list with names to be used in shiny and a vector of possible names in Influx
#' Add new sensor model to be recognized if needed
Influx2Shiny.names <- function(){
    # Defining names and variables for gas sensors - Used the same names of variables as in SOS for compatibility reasons
    return(list(
        ##### Host ####
        Batt_Cumulated_Charge = "L2942CUR",
        Batt_Voltage          = "L2942VOL",
        Batt_Charge_Status    = "L4156STA",
        ##### GasShield ####
        Nitrogen_dioxide      = c("NO2_B43F_P1", "NO2_A43F_P1","no2_b43f", "NO2-B43F", "NO2B43F", "NO2_B43","NO2_M20", "NO2_C1", "NO2_C25", "NO2/C-20", "NO2_3E50", "NO23E50", "NO2", "S1"),
        Carbon_monoxide       = c("CO_A4_P1"   , "CO_B4_P1","CO-B4", "CO-A4",  "COA4", "COMF200", "CO_MF200","CO/MF-200", "CO/MF-20", "CO-MF200", "CO_C200", "CO_CF200", "CO_3E300","CO3E300", "CO","CO-A4 O", "S2"),
        Ozone                 = c("OX_A431_P1" , "OX_B431_P1","O3/M-5", "O3-B4", "AX-A431", "OX-A431", "OX_A431", "O3-A431", "O3_M5", "O3_C5", "O3_C100", "O3-M5", "o3_m_5", "O3_3E1F", "O33EF1", "O3", "O3E100", "S3"),
        Nitric_oxide          = c("NO_B4_P1"   , "NO_A4_P1"  , "NO-B4", "NOB4_P1","NOB4", "NO_M25", "NO_C1", "NO_C25","NO/C-25", "NO3E100", "NO_3E100", "NO", "No Sensor", "S4"),
        Sulfur_dioxide        = c("SO2_B4_P1"  , "SO2_A4_P1" , "SO2_M20", "SO2_MF20", "SO2_C1", "SO2_C20", "SO2_CF20"),
        Ammonia               = c("NH3_MR100"  , "NH3_CR50") ,
        Relative_humidity     = c("SHT31HE", "Humid"),
        Temperature           = c("SHT31TE", "Tempe", "Temp"),
        Atmospheric_pressure  = c("Press", "BMP280"),
        Temperature_int       = "SHT31TI",
        Relative_humidity_int = "SHT31HI",
        #### EXpShield1 ####
        # CO2, D300
        Carbon_dioxide        = "D300",
        # Radon RD200M
        Radon                 = "RD200M",
        # PMS5003
        Bin1_PMS              = "53PT003",
        Bin2_PMS              = "53PT005",
        Bin3_PMS              = "53PT010",
        Bin4_PMS              = "53PT025",
        Bin5_PMS              = "53PT050",
        Bin6_PMS              = c("53PT100", "53P"),
        PM1_PMSraw            = "5301CST",
        PM1_PMSCal            = "5301CAT",
        PM25_PMSraw           = "5325CST",
        PM25_PMSCal           = "5325CAT",
        PM10_PMSraw           = "5310CST",
        PM10_PMSCal           = "5310CAT",
        # OPcN3
        Particulate_Matter_1  = c("OPCN2PM1"   , "OPCN3PM1") ,
        Particulate_Matter_25 = c("OPCN2PM25"  , "OPCN3PM25"),
        Particulate_Matter_10 = c("OPCN2PM10"  , "OPCN3PM10"),
        Bin0                  = c("OPCN2Bin0"  , "OPCN3Bin0"),
        Bin1                  = c("OPCN2Bin1"  , "OPCN3Bin1"),
        Bin2                  = c("OPCN2Bin2"  , "OPCN3Bin2"),
        Bin3                  = c("OPCN2Bin3"  , "OPCN3Bin3"),
        Bin4                  = c("OPCN2Bin4"  , "OPCN3Bin4"),
        Bin5                  = c("OPCN2Bin5"  , "OPCN3Bin5"),
        Bin6                  = c("OPCN2Bin6"  , "OPCN3Bin6"),
        Bin7                  = c("OPCN2Bin7"  , "OPCN3Bin7"),
        Bin8                  = c("OPCN2Bin8"  , "OPCN3Bin8"),
        Bin9                  = c("OPCN2Bin9"  , "OPCN3Bin9"),
        Bin10                 = c("OPCN2Bin10" , "OPCN3Bin10"),
        Bin11                 = c("OPCN2Bin11" , "OPCN3Bin11"),
        Bin12                 = c("OPCN2Bin12" , "OPCN3Bin12"),
        Bin13                 = c("OPCN2Bin13" , "OPCN3Bin13"),
        Bin14                 = c("OPCN2Bin14" , "OPCN3Bin14"),
        Bin15                 = c("OPCN2Bin15" , "OPCN3Bin15"),
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
        # Sensirion SP-30
        S30PM01                  = "S30PM01",
        S30PM25                  = "S30PM25",
        S30PM10                  = "S30PM10",
        S30PM40                  = "S30PM40",
        S30P005                  = "S30P005",
        S30P010                  = "S30P010",
        S30P025                  = "S30P025",
        S30P040                  = "S30P040",
        S30P100                  = "S30P100",
        S30TSZE                  = "S30TSZE",
        #NextPM
        NPMPC01                  = "NPMPC01",
        NPMPC25                  = "NPMPC25",
        NPMPC10                  = "NPMPC10",
        NPMPM01                  = "NPMPM01",
        NPMPM25                  = "NPMPM25",
        NPMPM10                  = "NPMPM10",
        NPMTemp                  = "NPMTemp",
        NPMHum                   = "NPMHum",
        NPMSta                   = "NPMSta",
        #### ExpShield2 ####
        # K96_Carbon_dioxide    = "SPLCPC",
        # K96_Methane           = "LPLCPC",
        K96_SPLCPC            = "SPLCPC",
        K96_LPLCPC            = "LPLCPC",
        #K96_Water_vapour      = "MPLCPC",
        K96_MPLCPC            = "MPLCPC",
        K96_UFSPIR            = "UFSPIR",
        K96_UFLPIR            = "UFLPIR",
        K96_UFMPIR            = "UFMPIR",
        K96_Temperature       = "TRH0",
        K96_Relative_humidity = "RH0",
        K96_Atmospheric_pressure = "PSEN0",
        K96_NTC0                 = "TNTC0",
        K96_NTC1                 = "TNTC1",
        K96_Chamber_Temperature  = "TICHMBR",
        K96_Error                = "ERRST"
    ))
}
#' @details  Sqlite2df transforms an airsenseur.db table into a General dataframe/datatable. airsenseur.db shall be created previously with Down_Influx
#' @return A Values_db (existing data added if any) dataframe with date (as.POSIXct) to be used by openair, coordinates, 7 sensor values as downloaded from Influx. Data are averaged with UserMins averaging time if Averaging is TRUE
#' @param name.SQLite       : character, path of the airsenseur.Db file, it shall be in the General.data directory
#' @param Dataset           : character, name of the table (Dataset) in the database Db that you want to download, e. g. "AirSensEUR05"
#' @param Influx.TZ         : character, the time zone for variable time in Dataset of the InfluxDB
#' @param UserMins          : numeric, default is NULL, if UserMins is not NULL aveaging of sensor dated with UserMins averaging time is performed, the periodicity of data requested for the returned dataframe,
#' @param DownloadSensor    : a list with: character, Influx.Rdata.file, the path.file/name of an existing InfluxData.Rdata file; character WDinput, the directory where to save Rdata and csv files; logical Retrieve.data.Influx, wether it is necessary to retrive sensor data (not used); character DateEND.Influx.prev, last date in Influx.Rdata.file. The time zone is the one of InfluxDB and SOS (GMT). The default value for DownloadSensor$Influx.Rdata.file is NULL, nothing passed. In this case, SQLite2df creates new Rdata/csv
#' @param Page              : numeric, default value NULL, if Null the size of the page of data to download from the influx server is LIMIT to 200000
#' @param complete          : Logical, default is FALSE, If TRUE the Sqlite2df function will return a dataFrame concatenating the existing data in name.Sqlite with the new ones in Values_db
#' @param asc.File          : dataframe, default is NULL, used for giveing the correct name of the sensor
#' @param InfluxData        : data.table or dataframe, default is null. DataSet of sensor values
#' @param Parallel          : logical default is FALSE. If TRUE airsenseur.db is read using parallel computing with 4 cores.
### Still need adding when the AirSensEUR is switched on and off, when the name of sensors are changed and when it is at the Reference Stations
Sqlite2df <- function(name.SQLite, Dataset, Influx.TZ, UserMins = NULL, DownloadSensor = NULL, Page = NULL, Complete = FALSE, asc.File=NULL, 
                      InfluxData = NULL, Parallel = F, Verbose = F) {
    cat("\n")
    cat("-----------------------------------------------------------------------------------\n")
    #------------------------------------------------------------------------------CR
    # Installing necessary packages
    #------------------------------------------------------------------------------CR
    # Both RSQLite and sqldf (and others too) are loaded by library(sqldf), so it is enough to instal sqldf
    list.Packages <- c("openair", "reshape")
    if (!all(list.Packages %in% installed.packages())) librarian::shelf(list.Packages, cran_repo = "https://cran.r-project.org");rm(list.Packages)
    #------------------------------------------------------------------------------CR
    # AirsensEur.db exists? creating the db or just the connect to the db
    #------------------------------------------------------------------------------CR
    if (file.exists(name.SQLite)) # airsenseur.db exists
        futile.logger::flog.info(paste0("[Sqlite2df] ", Dataset, " in ", name.SQLite, " exists.")) else  # airsenseur.db does not exist
            stop(futile.logger::flog.error(paste0("[Sqlite2df] ", Dataset, " ", name.SQLite, " does not exist. The script is stopped.")))
    #------------------------------------------------------------------------------CR
    # Checking table Dataset in airsenseur.db
    #------------------------------------------------------------------------------CR
    SQLite.con <- DBI::dbConnect(RSQLite::SQLite(), dbname = name.SQLite)
    # Checking if the SQLite.con database and the table Dataset exists?
    if (DBI::dbExistsTable(SQLite.con, paste0(Dataset,"_Cast"))) {
        futile.logger::flog.info(paste0("[Sqlite2df] ", Dataset, " ", name.SQLite, " includes the table ", paste0(Dataset,"_Cast")," with columns: ",
                                        paste0(DBI::dbListFields(SQLite.con, paste0(Dataset,"_Cast")), collapse = ", ")))
    } else stop(futile.logger::flog.error(paste0("[Sqlite2df] ", Dataset, " There is no table called ", paste0(Dataset,"_Cast"), " in ", name.SQLite, ". The scipt is stoped.")))
    #------------------------------------------------------------------------------CR
    # Reading local airsenseur.db in slice of Page records - from last data of InfluxData in DownloadSensor to only add the necessary data
    #------------------------------------------------------------------------------CR
    futile.logger::flog.info(paste0("[SQLite2df] ", Dataset, " reading table ", paste0(Dataset,"_Cast")))
    # Initial values
    Download.N  <- 0
    SQL.Total.N <- DBI::dbGetQuery(SQLite.con, paste0("SELECT max(rowid) FROM \"", paste0(Dataset,"_Cast"), "\""))[1,1]
    if (!is.null(DownloadSensor$DateEND.Influx.prev) && shiny::isTruthy(DownloadSensor$DateEND.Influx.prev)) { 
        # the table paste0(Dataset,"_Cast") exists in airsenseur.db
        futile.logger::flog.info(paste0("[SQLite2df] ", Dataset, " InfluxData already exists."))
        
        # Counting the row number where to add records in paste0(Dataset,"_Cast")
        futile.logger::flog.info(paste0("[SQLite2df] ", Dataset, " looking for the first date in InfluxData to append to airsenseur.db. This can be very long with large datasets ... TZ must be set"))
        # Getting the class of time, it is easier if it is numeric
        if(is.numeric(DBI::dbGetQuery(SQLite.con, paste0("SELECT time FROM \"", paste0(Dataset,"_Cast"),"\" LIMIT 1;"))$time)){
            Dataset.N <- DBI::dbGetQuery(SQLite.con, paste0("SELECT min(rowid) FROM \"", paste0(Dataset,"_Cast"), "\" WHERE time >= '", # >= instead of > to recalculate the last average
                                                            as.numeric(DownloadSensor$DateEND.Influx.prev),"';"))[1,1]
        } else {
            # it is easier charater of POSIXct
            if (!exists("Influx.TZ")) {
                futile.logger::flog.warn(paste0("[SQLite2df] ", Dataset, " the time zone TZ is not defined. It is set to \"UTC\""))
                Influx.TZ <- "UTC"}
            
            # changing FirstDate timezone from the value in DownloadSensor to the local timezone called Influx.TZ
            FirstDate <- Set_date2POSIXct(DownloadSensor$DateEND.Influx.prev, tz = "UTC") #as.POSIXct(DownloadSensor$DateEND.Influx.prev, tz = "UTC", usetz = TRUE) # attr(FirstDate, "tzone") <- "UTC"
            # >= instead of > to recalculate the last average
            Dataset.N <- DBI::dbGetQuery(SQLite.con, paste0("SELECT min(rowid)  FROM \"", paste0(Dataset,"_Cast"), "\" WHERE datetime(time) >= '", format(FirstDate, "%Y-%m-%d %H:%M:%S"), "';"))[1,1]}
        if (is.na(Dataset.N)) return(futile.logger::flog.warn(paste0("[SQLite2df] ", Dataset, " there are no new data in airSenseur.db to add to InfluxData.Rdata and InfluxData.csv. The script is stopped")))
    } else {# the table Dataset exists in airsenseur.db
        # There are no records in AirSensEUR$Dataset
        futile.logger::flog.info(paste0("[SQLite2df] ", Dataset, " file InfluxData.csv does not exist. Reading all data of airsenseur.db."))
        Dataset.N  <- 0
        #Parallel <- FALSE
    }
    
    # # getting the default Page of data to download
    if (is.null(Page)) Page <- 500000
    futile.logger::flog.info(paste0("[SQLite2df] ", Dataset, " reading ",(SQL.Total.N - Dataset.N)," records"))
    # too slow: Values_db <- data.table::data.table(RSQLite::dbReadTable(SQLite.con, paste0(Dataset,"_Cast"),check.names = FALSE))
    if (Parallel) {
        #https://gist.github.com/ibombonato/9a2934c05bb8da23fc7e20aaf2613872
        #https://stackoverflow.com/questions/3902796/foreach-dopar-rpostgresql
        #Creating the cluster
        n.cores <- min(detectCores()-1,4)
        cl      <- makePSOCKcluster(n.cores)
        
        #https://stackoverflow.com/questions/22739876/how-to-export-objects-to-parallel-clusters-within-a-function-in-r
        clusterExport(cl=cl, list("name.SQLite","Dataset.N","n.cores"), envir=environment())
        # Defining packages and variables for cluster
        clusterEvalQ(cl=cl, {
            library(DBI)
            library(RSQLite)
            SQLite.con <- DBI::dbConnect(DBI::dbDriver("SQLite"), dbname = name.SQLite)
            #RowIds     <- split(1:Dataset.N, ceiling(seq_along(1:Dataset.N)/ceiling(Dataset.N/n.cores)))
            NULL
        })
        
        #Register the Cluster
        registerDoParallel(cl)
        
        # Selecting only data to add
        if (Dataset.N == 0) {
            rowID2use <- 1:SQL.Total.N 
        } else {
            rowID2use <- Dataset.N:SQL.Total.N
        }
        
        #Start the Parallel Loop
        Values_db <- data.table::data.table(data.table::rbindlist(
            foreach(i= 1:n.cores, .inorder = FALSE, .noexport="SQLite.con", .packages=c("DBI","RSQLite")) %dopar% { #.combine = "rbindlist"
                sql_text <- paste0("SELECT rowid,* FROM '", paste0(Dataset,"_Cast"), 
                                   "' WHERE rowid >= ", round(quantile(rowID2use, (i-1)/n.cores)),
                                   " LIMIT ", (round(quantile(rowID2use, (i)/n.cores)) - round(quantile(rowID2use, (i-1)/n.cores))-1),";")
                return(DBI::dbGetQuery(SQLite.con, sql_text))
            }, fill = TRUE),check.names = FALSE)
        
        #Closing connection in all clusters
        clusterEvalQ(cl=cl, {
            DBI::dbDisconnect(SQLite.con)
        })
        
        # Stopping cluster
        stopCluster(cl)
        stopImplicitCluster()
    } else Values_db <- data.table::data.table(DBI::dbGetQuery(SQLite.con, statement=paste("SELECT * FROM '", paste0(Dataset,"_Cast"), "'", sep="")),check.names = FALSE)
    
    # Using gpstimestamp if available, only if gpsTimestamp > time + 120 sec. It seems that gpstimestamp freeze during the process of data acquisition some times resulting with 2 data being in the same minute and some data missing on some minutes
    #Make sure time is POSIXct for time and gpsTimestamp to compare them
    data.table::set(Values_db, j = "time", value = Set_date2POSIXct(Values_db$time, tz = "UTC"))
    data.table::set(Values_db, j = "gpsTimestamp", value = Set_date2POSIXct(Values_db$gpsTimestamp, tz = "UTC"))
    if("gpsTimestamp" %in% names(Values_db)){
        # GPSTimeStamp to POSIX
        Row.GPS2Use <- which(is.finite(Values_db$gpsTimestamp))
        if(length(Row.GPS2Use) > 0){
            
            # Where gpsTimeStamp > time, use gpsTimeStamp
            # The gpsTimestamp fluctuates around time of about 70 sec (threshold 100 sec) and it seems that "time" is increasing more accurately (constant periodicity). So use an offset
            Row2swith2GPS <- which(Values_db[Row.GPS2Use]$gpsTimestamp > Values_db[Row.GPS2Use]$time + 100)
            # We may have several periods where time freezed with different values for gpsTimestamp - time. So the mean of gpsTimesptamp - time may be inaccurate
            # Number of freezes: we can check when the difference gpsTimestamp - time is > 100 on Row.GPS2Use[Row2swith2GPS], with mean a new median of gpsTimeSpamp - time per freeze
            if (length(Row2swith2GPS) > 0) {
                Timestamp.pop <- which(abs(diff(Values_db$gpsTimestamp[Row.GPS2Use[Row2swith2GPS]]-Values_db$time[Row.GPS2Use[Row2swith2GPS]])) > 120)
                if(length(Timestamp.pop) == 0){
                    
                    # only one freeze of unix epoch, the one at the begining of Row.GPS2Use[Row2swith2GPS]. Take the median of the 5 1st time differences
                    Min.Lag <- median((Values_db$gpsTimestamp[Row.GPS2Use[Row2swith2GPS]]-Values_db$time[Row.GPS2Use[Row2swith2GPS]])[1:5])
                    # add the difference gpsTimeSpamp - time to time
                    data.table::set(Values_db, i = Row.GPS2Use[Row2swith2GPS], j = "time", value = Values_db$time[Row.GPS2Use[Row2swith2GPS]] + Min.Lag)
                    
                } else {
                    
                    # There are several freezes on unix time
                    for(pop in Timestamp.pop){
                        if(pop == Timestamp.pop[1]) pop.In <- 1 else pop.in <- Timestamp.pop[pop-1]
                        Freeze.period <- Row.GPS2Use[Row2swith2GPS][pop.In:pop]
                        # only one freeze of unix epoch, the one at the begining of Row.GPS2Use[Row2swith2GPS]. Take the median of the 5 1st time differences per freeze period
                        Min.Lag <- median((Values_db$gpsTimestamp[Freeze.period] - Values_db$time[Freeze.period])[1:5])
                        # add the difference gpsTimeSpamp - time to time
                        data.table::set(Values_db, i = Freeze.period, j = "time", value = Values_db$time[Freeze.period] + Min.Lag)}}}}}
    
    # set class of time to POSIXct and set index, Use GpsTimesstamp if it exist
    # Transforming column time in POSIXCt with the correct time zone (UTC)
    if (is.null(Influx.TZ)) {
        futile.logger::flog.info(paste0("[SQLite2df] ", Dataset, " Converting datetime from character to POSIX format, ERROR time zone is not set for InfluxDB. Using UTC."))
        if (!lubridate::is.POSIXct(Values_db$time)) Values_db$time <- Set_date2POSIXct(Values_db$time, tz = "UTC")
    } else{
        futile.logger::flog.info(paste0("[SQLite2df] ", Dataset, " Converting Values_db$time from character to POSIX format, timezone is ", Influx.TZ))
        if (!lubridate::is.POSIXct(Values_db$time)) Values_db$time <- Set_date2POSIXct(Values_db$time, tz = Influx.TZ)}
    
    # To avoid duplicate row with same time
    Values_db <- Unique.DT.time(Values_db)
    
    if (!haskey(Values_db)) setkey(Values_db, "time")
    # resuming Page for tabulating values
    if (exists("Old_Page")) Page <- Old_Page
    futile.logger::flog.info(paste0("[SQLite2df] ", Dataset, " Disconnecting ", name.SQLite))
    futile.logger::flog.info(paste0("[SQLite2df] ", Dataset, " The table ", paste0(Dataset,"_Cast"), " has ", nrow(Values_db), " new records and ", length(unique(Values_db$time)), " new unique TimeStamps"))
    # reading Channel.names
    # for recognizing names of sensors
    Channel.names <- unique(data.table::data.table(RSQLite::dbReadTable(SQLite.con, name = "Channel.names", check.names = FALSE))[, c("channel","name")])
    if (exists("Adding"))      rm(Adding)
    if (exists("Download.N"))  rm(Download.N)
    if (exists("SQL.Total.N")) rm( SQL.Total.N)
    # Closing connection
    DBI::dbDisconnect(SQLite.con)
    #------------------------------------------------------------------------------CR
    # Defining names and variables for meteo
    #------------------------------------------------------------------------------CR
    Meteo.names.change  <- data.frame(Influx.names  = c(          "SHT31HE",             "Humid",     "SHT31TE",       "Tempe",        "Temp",                "Press",               "BMP280"),
                                      General.names = c("Relative_humidity", "Relative_humidity", "Temperature", "Temperature", "Temperature", "Atmospheric_pressure", "Atmospheric_pressure"),
                                      stringsAsFactors = FALSE)
    ASE_Status          <- data.frame(Influx.names  = c(        "SHT31TI",               "SHT31HI",              "L2942CUR",     "L2942VOL",           "L4156STA"),
                                      General.names = c("Temperature_int", "Relative_humidity_int", "Batt_Cumulated_Charge", "Batt_Voltage", "Batt_Charge_Status"),
                                      stringsAsFactors = FALSE)
    #------------------------------------------------------------------------------CR
    # Adding final name (Temperature, Relative ...)
    #------------------------------------------------------------------------------CR
    futile.logger::flog.info(paste0("[SQLite2df] ", Dataset, " looking for the names of meteorological sensors and ASE Status using the sensor names"))
    for (i in Meteo.names.change$Influx.names[Meteo.names.change$Influx.names %in% Channel.names$name]) {
        i.rows <- which(Channel.names$name == i)
        # setting column variables in Channel.names with names of Meteo.names.change
        if (!"Variables" %in% names(Channel.names)) {
            Channel.names[i = i.rows, Variables := Meteo.names.change[Meteo.names.change$Influx.names == i,"General.names"]]
        } else data.table::set(Channel.names, i = i.rows, j = "Variables", value = Meteo.names.change[Meteo.names.change$Influx.names == i,"General.names"])}
    for (i in ASE_Status$Influx.names[ASE_Status$Influx.names %in% Channel.names$name]) {
        i.rows <- which(Channel.names$name == i)
        # setting column variables in Channel.names with names of Meteo.names.change
        if (!"Variables" %in% names(Channel.names)) {
            Channel.names[i = i.rows, Variables := ASE_Status[ASE_Status$Influx.names == i,"General.names"]]
        } else data.table::set(Channel.names, i = i.rows, j = "Variables", value = ASE_Status[ASE_Status$Influx.names == i,"General.names"])}
    # Defining names and variables for gas sensors - Used the same names of variables as in SOS for compatibility reasons
    Sensor.names <- Influx2Shiny.names()
    #------------------------------------------------------------------------------CR
    # Renaming sensor model type (Nitic_Oxide...) if more than 1 model of sensors on one channel number
    # then columns are combined and the last sensor model type is used, keeping the names of the 1st one
    #------------------------------------------------------------------------------CR
    for (i in 1:length(Sensor.names)) {
        Index.sensors <- which(Channel.names$name %in% Sensor.names[[i]] & Channel.names$name %in% names(Values_db))
        if(length(Index.sensors)>1) browser()
        # Dealing with change of sensor name on the same channel, keeping last sensor name
        if (length(Index.sensors) > 1) Dupli.Sensors <- Channel.names$name[Index.sensors] else Dupli.Sensors <- NA
        if (shiny::isTruthy(Dupli.Sensors)) {
            for (Dupli in 2:length(Dupli.Sensors)) {
                if (all(Dupli.Sensors[c(Dupli - 1, Dupli)] %in% names(Values_db))) {
                    Rows.1st <- which(!is.na(Values_db[[Dupli.Sensors[Dupli - 1]]]))
                    Rows.2nd <- which(is.na(Values_db[[Dupli.Sensors[Dupli]]]))
                    Missing.2nd <- intersect(Rows.1st, Rows.2nd)
                    if (length(Rows.1st) > 0  && length(Missing.2nd) > 0) data.table::set(Values_db, i = Missing.2nd, j = Dupli.Sensors[Dupli],
                                                                                          value = Values_db[[Dupli.Sensors[Dupli - 1]]][Missing.2nd])
                    # deleting 1st sensor in Values_db
                    Sensors2Del <- Dupli.Sensors[Dupli - 1]
                    Values_db[, (Sensors2Del) := NULL]}}
            # giving official name of the remaining sensor found in the shield configuration
            if (Dupli.Sensors[Dupli] %in% names(Values_db)) data.table::setnames(Values_db, old = Dupli.Sensors[Dupli], new = names(Sensor.names)[i])
        } else if (length(Index.sensors) > 0) data.table::setnames(Values_db, old = Channel.names$name[Index.sensors], new = names(Sensor.names)[i])}
    
    # Dropping rowid if present in airsenseur.db
    if ("rowid" %in% names(Values_db)) Values_db[, rowid := NULL]
    #------------------------------------------------------------------------------CR
    # Putting data in tabulated dataframe
    #------------------------------------------------------------------------------CR
    # Aggregating in tabulated form. Discarding 0s in coordinates and altitude to avoid error when averaging
    for(GPS.Var in c("altitude","longitude","latitude")){
        Zero.GPS <- which(Values_db[[GPS.Var]] == 0)
        if(length(Zero.GPS) > 0){
            data.table::set(Values_db, i = Zero.GPS, j = GPS.Var , value = rep(NA, length(Zero.GPS)))}
        rm(Zero.GPS)}
    rm(GPS.Var)
    
    # Change "time" to "date" to use OpenAir
    data.table::setnames(Values_db, "time", "date")
    
    # dropping unused columns
    Columns2Drop <- grep(paste0(c("date","altitude","boardTimeStamp","gpsTimestamp","latitude","longitude",names(Sensor.names)), collapse = "|"), names(Values_db),invert = T, value = T)
    if(length(Columns2Drop) > 0) Values_db[, (Columns2Drop):= NULL]
    remove(Sensor.names, Channel.names, Meteo.names.change, Columns2Drop)
    
    # Averaging with UserMins averaging time, creating Values_db_Mins, this allow to reduce the number of rows with empty sensor values
    if (exists("Values_db") && !is.null(UserMins)) {
        futile.logger::flog.info(paste0("[SQLite2df] ", Dataset, " averaging each ", UserMins, " mins. This can be long with large datasets."))
        Values_db_Mins <- DF_avg(Values_db, width = UserMins)
    } else stop(futile.logger::flog.error(paste0("[SQLite2df] ", Dataset, " UserMins is not set in ASEConfig_xx.R. Please set it, default 1 mins. The script is stopped.")))
    # returning data if any
    # Trying to load the existing data or Influx.Rdata.file
    if (!shiny::isTruthy(InfluxData)) {
        if (file.exists(DownloadSensor$Influx.Rdata.file)) {
            if (file_ext(DownloadSensor$Influx.Rdata.file) == "csv") {
                InfluxData <- data.table::fread(file = DownloadSensor$Influx.Rdata.file, na.strings = c("","NA", "<NA>"))
                if (!is.null(Influx.TZ) && Influx.TZ != "") {
                    data.table::set(InfluxData, j = "date", value =  lubridate::ymd_hms(InfluxData[["date"]], tz = Influx.TZ))
                } else data.table::set(InfluxData, j = "date", value =  lubridate::ymd_hms(InfluxData[["date"]], tz = "UTC"))
            } else if (file_ext(DownloadSensor$Influx.Rdata.file) == "Rdata") {
                load(DownloadSensor$Influx.Rdata.file)
                InfluxData <- data.table::data.table(InfluxData)
            }
        } else futile.logger::flog.info(paste0("[SQLite2df] ", Dataset, " there is no previously saved Influx.Rdata.file. Missing InfluxData and Down.Influx request of sensor data download ."))
    }
    
    if (!Complete) {
        if (exists("Values_db_Mins") && shiny::isTruthy(Values_db_Mins) && nrow(Values_db_Mins) > 0) {
            futile.logger::flog.info(paste0("[SQLite2df] ", Dataset, " returning newly downloaded sensor data."))
            if (Verbose) print(str(Values_db_Mins))
            return(Values_db_Mins)
        } else {
            cat("[SQLite2df] WARNING, no new downloaded INflux data nor previous ones.\n")
            # adding the existing data before averaging, all rows except the last one that shall be recalculated with new values
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            return()}
    } else {
        if (exists("Values_db_Mins") && shiny::isTruthy(Values_db_Mins) && nrow(Values_db_Mins) > 0) {
            if (exists("InfluxData") && shiny::isTruthy(InfluxData) && nrow(InfluxData) > 0) {
                futile.logger::flog.info(paste0("[SQLite2df] ", Dataset, " returning previously and newly downloaded sensor data."))
                if (InfluxData[nrow(InfluxData),"date"] == Values_db[1,"date"])
                    Values_db_Mins <- data.table::rbindlist(list(InfluxData, Values_db_Mins), fill = TRUE)
                if (Verbose) print(str(Values_db_Mins))
                return(Values_db_Mins)
            } else {
                futile.logger::flog.info(paste0("[SQLite2df] ", Dataset, " returning only newly downloaded sensor data."))
                if (Verbose) print(str(Values_db_Mins))
                return(Values_db_Mins)}}}
}
#=====================================================================================CR
# MG : Down_Ref: Download Reference Data ####
#=====================================================================================CR
#' Download Reference Data
#' @description reference data can be downloaded from SOS or a_i_p servers or by local loading of rdata or csv files of by uploading csv files from a ftp server
#' @param PROXY          : Logical, default value FALSE. If TRUE PROXY is configured using the following 4 parameters:
#' @param PORT           : numeric, default value NULL, open Port for the proxy, jrc = 8012;
#' @param LOGIN          : character, default value = NULL, login for the proxy server, JRC = NULL;
#' @param PASSWORD       : character, default value = NULL, password for the proxy server, jrc = NULL;
#' @param Reference.name character vector, Name of the Reference station
#' @param FTPMode character vector with possible value csv, ftp, SOS, a_i_p, default is "ftp". Type of download of reference data: 
#' "ftp" using a csv file on a ftp server, "csv" the same with a local file, SOS: from a SOS download and a_i_p form an a_i_p server.
#' @param Ref.Type character vector with possible values: "Ref, "Bin.DMPS", "Bin.APS", "GRIMM". 
#' Label to be written in front of pollutants names, default is Ref, other possibility is about bins for PM distribution
#' @param UserMins numeric, requested periodicity of returned data after final data
#' @param nastring character vector, a strings to be identified as NA
#' @param WDoutput character vector, file paht where to save reference data as RefData.csv. If NULL RefData.csv is saved in the current directory (getwd())
#' @param ref.tzone character vector, giving the time zone of reference data. Default is "UTC"
#' @param DownloadSensor a list output of function check_download()
#' @param AirsensWeb no more used
#' @param Old.Ref.Data data.tavle or datafFrame, previous reference data to be merged with currently loading reference data, default is NULL. If NULL no previous data is included in returned data.table
#' @param coord.ref vector of 2 strings. with coordinates of reference data longitude and latitude separated by a blank.
#' @details FTPMode: "ftp" mode
#' @param urlref vector of character vectors, used if FTPMode = "ftp". Vector of URIs linking to csv files with the reference data. Header with variable names as in ASEConfig.R
#' and one column of date (DateTime), errors and NAs as -999.99, only one column may include one of the string c("date","time","Date", "Time", "DATE", "TIME")
#' ftp uri for reference data retrievals. The link shall point to a vector of character with csv files, with headers with the names of variable.
#' One header shall includes the string DateTime of dates
#' @details FTPMode: "csv" mode
#' @param csvFile character vector, file path to the csv file to load
#' @param csvFile.sep character vector, separator between columns in the csvFile
#' @param csvFile.quote character vector, separator of values in all columns, default "\""
#' @param csvFile.DateIN Date or POSIXct, as.Date(input$Ref__a_i_p__Date[1], format = "%Y-%m-%d"), only data starting from this dateTime will be considered for insertion in RefData.csv
#' @param csvFile.DateEND Date or POSIXct, as.Date(input$Ref__a_i_p__Date[2], format = "%Y-%m-%d"), only data until this dateTime will be considered for insertion in RefData.csv
#' @details FTPMode: "SOS" mode
#' @param RefSOSname character vector, Reference station SOS Rest API URL
#' @param Ref.SOS.name character vector, SOS ID of the Reference station
#' @param RefSOSPollutants, Character vector, list of pollutants to download. Default is NULL. In this case pollutants are downloaded.
#' @param RefSOSDateIN Starting  date for downloading Reference data using SOS
#' @param RefSOSDateEND Ending date for downloading Reference data using SOS
#' @details FTPMode: "a_i_p" mode
#' @param Ref__a_i_p__name character vector, input$Ref__a_i_p__name
#' @param User__a_i_p__ character vector, input$User__a_i_p__
#' @param Pass__a_i_p__ character vecor, input$Pass__a_i_p__
#' @param Ref__a_i_p__Organisation character vector input$Ref__a_i_p__Organisation
#' @param Ref__a_i_p__Station character vector input$Ref__a_i_p__Station
#' @param Ref__a_i_p__Pollutants   character vector, input$Ref__a_i_p__Pollutants,
#' @param Ref__a_i_p__DateIN Date, as.Date(input$Ref__a_i_p__Date[1], format = "%Y-%m-%d")
#' @param Ref__a_i_p__DateEND Date, as.Date(input$Ref__a_i_p__Date[2], format = "%Y-%m-%d")
#' @param avgtime       Integer, possible values are 1, 10, 15 or 60, default is NULL, the averaging time of data in minute at the aip server
#' @return  data.table Ref of the reference data with correct header and perodicity (UserMins)
#' @example  
Down_Ref <- function(PROXY = FALSE, URL = NULL, PORT = NULL, LOGIN = NULL, PASSWORD = NULL, 
                     Reference.name, FTPMode = "ftp", Ref.Type = "Ref", UserMins, naStrings = NULL, WDoutput = NULL, ref.tzone = "UTC", 
                     DownloadSensor, AirsensWeb, Old.Ref.Data = NULL, coord.ref = NULL, shiny = TRUE,
                     urlref,
                     csvFile = NULL, csvFile.sep = NULL, csvFile.quote = "\"", csvFile.DateIN = NULL, csvFile.DateEND = NULL,
                     RefSOSname = NULL, Ref.SOS.name = NULL, RefSOSPollutants = NULL, RefSOSDateIN = NULL, RefSOSDateEND = NULL,
                     Ref__a_i_p__name = NULL, User__a_i_p__ = NULL, Pass__a_i_p__ = NULL, Ref__a_i_p__Organisation = NULL,
                     Ref__a_i_p__Station = NULL, Ref__a_i_p__Pollutants = NULL, Ref__a_i_p__DateIN = NULL, Ref__a_i_p__DateEND = NULL, avgtime = 1) {
    
    cat("\n")
    cat("-----------------------------------------------------------------------------------\n")
    futile.logger::flog.info(paste0("[Down_Ref] INFO, Reference Data retrieving for ", Reference.name))
    # Set time interval of interest in seconds
    minSec <- UserMins*60.
    # Name of Reference pollutants using label(phenomenon(ts))
    # Add new reference to be recognized if needed
    Reference.names        <- list(date         = c("date","time","Date", "Time", "DATE", "TIME", "DateTime", "datetime"),
                                   time         = c("time" , "Time"),
                                   Ref.CO_ppm   = c("CO"   , "co"  , "Carbon monoxide (air)", "Ref.CO_ppm", "CO_ppm", "carbon monoxide", "Carbon Monoxide"),
                                   Ref.NO2      = c("NO2"  , "no2" , "Nitrogen dioxide (air)", "Ref.NO2", "nitrogen dioxide", "nitrogen dioxide caps","nitrogen dioxide BLC", "Nitrogen dioxide"),
                                   Ref.O3       = c("O3"   , "o3"  ,"Ozone (air)", "Ref.O3", "ozone", "Ozone"),
                                   Ref.NO       = c("NO"   , "no"  ,"Nitrogen monoxide (air)", "Ref.NO", "nitrogen monoxide", "nitrogen monoxide BLC", "Nitrogen monoxide")  ,
                                   Ref.NOx      = c("NOx"  , "nox" , "Ref.NOx", "nitrogen oxides", "nitrogen oxides BLC")  ,
                                   Ref.SO2      = c("SO2"  , "so2" ,"Sulfur dioxide (air)", "Ref.SO2", "sulfur dioxide", "Sulphur dioxide"),
                                   Ref.PM1      = c("PM1"  , "pm1" ,"Particulate matter < 1 \U00B5m (aerosol)", "Particulate Matter < 1 \U00B5m", "Ref.PM1"),
                                   Ref.PM2.5    = c("PM2.5", "pm25","Particulate matter < 2.5 \U00B5m (aerosol)", "Particulate Matter < 2.5 \U00B5m", "Ref.PM2.5"),
                                   Ref.PM4      = c("PM4"  , "pm4", "Ref.PM4"),
                                   Ref.PM10     = c("PM10" , "pm10", "Particulate matter < 10 \U00B5m (aerosol)", "Particulate Matter < 10 \U00B5m", "Ref.PM10"),
                                   Ref.PM10.TEOM   = c("Alternative_TEOM_PM10", "TEOM","TEOM_PM10", "pm10.TEOM"),
                                   Ref.PM2.5.TEOM  = c("Alternative_TEOM_PM25", "pm25.TEOM"),
                                   Ref.PM10.Beta   = c("pm10.Beta"),
                                   Ref.PM2.5.Beta  = c("pm25.Beta"),
                                   Ref.PM1.Fidas   = c("pm1.Fidas", "PM1 measured"),
                                   Ref.PM2.5.Fidas = c("pm25.Fidas", "PM2.5 measured"),
                                   Ref.PM4.Fidas   = c("pm4.Fidas"),
                                   Ref.PM10.Fidas  = c("pm10.Fidas", "PM10 measured"),
                                   Ref.PMtot.Fidas = c("PMtot", "pmtot.Fidas"),
                                   Ref.PMCoarse.Fidas = c("pmcoarse.Fidas"),
                                   Ref.Cn.Fidas    = c("Cn.Fidas", "pm count"), # total counts of particulate
                                   Ref.PM10.LVS  = "pm10.LVS",
                                   Ref.PM2.5.LVS = "pm25.LVS",
                                   Ref.Temp     = c("Temperature", "AirTemp", "T1", "ta", "temperature"), # T1 for VITO, it is in B0C, ta for NILU, it is in B0C, temperatuer aip JRC-Ispra-3 (FIDAS), discard "Sample_air temperature" which is T of the sampling line
                                   Ref.RH       = c("Relative_humidity", "RH", "relative humidity", "rh", "rel. humidity"), # rh for NILU in % #rel humidity from FIDAS
                                   Ref.Press    = c("Atmospheric_pressure", "AirPress", "PP", "p", "pressure"), # PP for VITO, it is hPa, "p" for NILU
                                   Ref.SolarRad = "Solar Radiation",
                                   Ref.WV       = c("WSAve", "VM", "ws"), # VM for VITO it is in m/s, ws for NILU
                                   Ref.WD       = c("WDAve", "DD", "wd"), # DD for VITO , it is in degrees, wd for NILU
                                   Ref.Rain     = c("RainAcc", "RR"), # RR for VITO, it is in mm
                                   Ref.CO2      = c("CO2", "co2", "CO2_sync"),
                                   Ref.CO2.Dry  = "CO2_dry_sync",
                                   Ref.CH4      = "CH4_sync",
                                   Ref.CH4.Dry  = "CH4_dry_sync",
                                   Ref.H2O      = "H2O_sync")
    # set DateIN for data retrieving, either from mindateRef (min Influx and SOS date) or last date in previous DataFrame
    # download if Influx data exists
    if (!is.null(DownloadSensor$mindateRef)) {
        DateIN  <- DownloadSensor$mindateRef
        DateEND <- DownloadSensor$maxdateRef
    } else if (!is.null(DownloadSensor$DateIN.Ref.prev)) {
        DateIN  <- DownloadSensor$DateIN.Ref.prev
        DateEND <- DownloadSensor$DateIN.Ref.prev
    } else {
        DateIN  <- as.POSIXct("2015-12-01 00:00", tz = ref.tzone) 
        # Setting end date to curent date (the time zone of the reference shall be in UTC, normally it is allways like that)
        if (exists("ref.tzone")) {
            DateEND <- as.POSIXct(Sys.time(), tz = ref.tzone)
        } else DateEND <- as.POSIXct(Sys.time(), tz = "UTC")
    } 
    #Set time interval, with function interval of package lubridate
    date <- lubridate::interval(DateIN, DateEND, tzone = ref.tzone) # tzone=user.tzone
    # the function  interval returns a variable of class lubridate
    futile.logger::flog.info(paste0("[Down_Ref] Time zone for reference data: ", date@tzone))
    # creating returning data frame Ref
    Ref       <- data.table::as.data.table(data.frame(date = seq(date@start, length = date@.Data/minSec, by = paste0(toString(UserMins)," ","min")),
                                                      row.names = NULL, check.rows = FALSE,
                                                      check.names = TRUE,
                                                      stringsAsFactors = FALSE))
    # Limiting Ref to the interval of download of data
    if (FTPMode %in% c("ftp", "csv", "SOS", "a_i_p")) {
        if (shiny::isTruthy(csvFile.DateIN)  && (lubridate::is.Date(csvFile.DateIN)  || lubridate::is.POSIXct(csvFile.DateIN)))  Ref <- Ref[date > csvFile.DateIN]
        if (shiny::isTruthy(csvFile.DateEND) && (lubridate::is.Date(csvFile.DateEND) || lubridate::is.POSIXct(csvFile.DateEND))) Ref <- Ref[date <= as.Date(csvFile.DateEND)+1]}
    
    if (nrow(Ref) == 0) {
        stop(" Either the start or end downloading date or UserMins parameter is wrong. The script is stopped ...")
        my_message <- paste0("[Down_Ref] ERROR no data found in the file of reference data for ", Reference.name, " .\n")
        cat(my_message)
        if (shiny) shinyalert(
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
        Ref <- NULL
    } else {
        # Setting PROXY (not used for csv!)
        if (PROXY) {
            if (is.null(LOGIN)) httr::set_config(use_proxy(url=URL, port=PORT)) else httr::set_config( use_proxy(url=URL, port=PORT, username = LOGIN, password = PASSWORD))
        } else httr::reset_config()
        # Downloading according to FTPMode
        if (FTPMode == "ftp" || FTPMode == "csv") {
            if (FTPMode == "ftp") {
                for (i in seq(urlref)) {
                    # File to search
                    File.csv  <- basename(urlref[i])
                    url       <- dirname(urlref[i])
                    # adding final "/" if missing to use getURL
                    if (substr(url, nchar(url), nchar(url)) != "/") url <- paste0(url,"/")
                    filenames <- getURL(url = url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
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
                        futile.logger::flog.info(paste0("[Down_Ref] trying to download data from ", urlref[i]))
                        #xx = getURL(urlref[i], nobody=1L)
                        #if (!is.na(xx)) {
                        #    xxx<- strsplit(xx, "\r\n")
                        # as.numeric(xx["Content-Length:"])
                        # xtmp <- unlist(strsplit(unlist(xxx), split = " "))
                        # if (!is.na(xtmp[2:2])) {
                        Reference.i <- data.table::fread(file = urlref[i])
                        # Selecting data within date interval
                        if (nrow(Reference.i) == 0) {
                            my_message <- paste0("[Down_Ref] ERROR no data found in the file of reference data for ", Reference.name, " .\n")
                            cat(my_message)
                            if (shiny) shinyalert(
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
                                animation = FALSE)
                        } else {
                            # use openair function to aggregate to the selected time average, in openair time must be replaced in date
                            # Adding coordinates of the reference stations
                            if (shiny::isTruthy(coord.ref)) {
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
                                    Reference.i$Ref.Lat  <- as.numeric(lat)}}
                            # checking if we have a date column in the referenceValues()
                            if (any(grepl(pattern = paste0(c("date","time","Date", "Time", "DATE", "TIME", "DateTime", "dateTime"), collapse = "|"),
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
                                            Ref[which(Ref$date %in% Reference.i$date),paste0("Ref.",i)] <- Reference.i[which(Reference.i$date %in% Ref$date),i]}}
                                } else {
                                    my_message <- "[Down_Ref] ERROR, There is no or more than one column called  with names date, time, Date , Time, DATE, TIME or DateTime. The script is stopped"
                                    cat(my_message)
                                    if (shiny) shinyalert(
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
                                        animation = FALSE)}
                            } else {
                                my_message <- "[Down_Ref] ERROR, There is no column called date, time, Date , Time, DATE, TIME or DateTime or separator and quote are not set correctly. The script is stopped"
                                cat(my_message)
                                if (shiny) shinyalert(
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
                                    animation = FALSE)}
                        }
                    } else {
                        my_message <- paste0("[Down_Ref] ERROR the csv file ",File.csv," does not exist at ", url," .\n")
                        cat(my_message)
                        if (shiny) shinyalert(
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
                            animation = FALSE)}
                }
            } else {
                if (FTPMode == "csv") {
                    cat(paste0("[Down_Ref] INFO, loading local file for reference data, file ", csvFile, "\n"))
                    stopifnot(file.exists(csvFile))
                    if (grepl(".csv", csvFile, fixed = T)) {
                        # if you load a .csv file:
                        Reference.i <- data.table::fread(file = csvFile, na.strings = naStrings, sep = csvFile.sep, quote = csvFile.quote)
                    } else if (grepl(".Rdata", csvFile, fixed = T)) {
                        # if you load a .Rdata file:
                        # loaded Rdata with unknown name dataframe
                        Reference.i <- load_obj(csvFile)
                        if (!data.table::is.data.table(Reference.i)) Reference.i <- data.table::data.table(Reference.i)
                        # removing un-necessary columns of Reference.i
                        # possible names
                        all.names <- character(0)
                        for (i in seq_along(Reference.names)) all.names <- c(all.names, unlist(Reference.names[[i]]))
                        Reference.i <- Reference.i[,which(names(Reference.i) %in% all.names)]
                    } else {
                        my_message <- paste0("[Down_Ref] ERROR, unrecognized file type for \n reference data .\n")
                        cat(my_message)
                        if (shiny) shinyalert(
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
                            animation         = FALSE)}
                    # if you load a GRIMM .txt file
                    if (grepl(".txt", csvFile, fixed = T) & Ref.Type == "GRIMM") {
                        # read Bin names
                        bins_diameters_GRIMM <- read.table(csvFile,
                                                           header = F, skip = 1, sep=",", nrows = 1)[-1]
                        names(bins_diameters_GRIMM) <- paste0("Bin", seq(1:length(names(bins_diameters_GRIMM))))
                        # remove special characters such as ">" and "\U00B5m"
                        bins_diameters_GRIMM <- apply(bins_diameters_GRIMM,  MARGIN = 2, function(col) gsub(paste(c("\U00B5m", ">"), collapse = "|"), "",(col)) )
                        diameters_GRIMM <- as.numeric(bins_diameters_GRIMM)
                        MAX_Diam_GRIMM  <- max(diameters_GRIMM)
                        bins_diameters_GRIMM <- as.data.frame(t(bins_diameters_GRIMM))
                        # read GRIMM data
                        # check the structure of the GRIMM file first...
                        Reference.i <- read.table(csvFile, header = F, sep="," , fill = T)
                        # find lines where it is reported the word "File" and the symbol ">"
                        # add all neccessary criteria to skip lines
                        logical <- apply(Reference.i, MARGIN = 2, function(col) grepl(paste(c("File", ">"), collapse = "|"), col))
                        logical <- as.data.frame(logical)
                        line.to.remove <- apply(logical, MARGIN = 2, function(col) which(col == TRUE) )
                        line.to.remove <-  unique(unlist(line.to.remove))
                        # read GRIMM data again but skip selected lines
                        Reference.i <- read.table(csvFile, header = F, sep="," , fill = T)[-c(line.to.remove), ]
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
                        Reference.i[,paste0("Bin", 1:(length(n.bin.GRIMM)-1))] <- sapply(1:(length(n.bin.GRIMM)-1),   function(i) Reference.i[ ,paste0("Bin",i)] - Reference.i[, paste0("Bin",i+1)])}
                }
            }
        } else {
            if (FTPMode == "SOS") {
                
                # Sensor Data retrieving at apiEndpoint
                # Duration              = integer, the number of days to download per page (as Limit in SQL), default is NULL, data are downloaded in slices of 7 days
                # DateEND               = To be set if the whole function is run over an internal from DownloadSensor$DateEND.SOS.prev until DateEND. DownloadSensor$DateEND.SOS.prev
                #                         can be set manually before running Down_SOS.
                cat("\n")
                cat("-----------------------------------------------------------------------------------\n")
                futile.logger::flog.info(paste0("[Down_Ref] ", Ref.SOS.name," reference data retrieving"))
                # Checking internet connection availability
                if (curl::has_internet()) {
                    if (PingThisSite(RefSOSname)) {
                        futile.logger::flog.info(paste0("[Down_Ref] ping to ", RefSOSname, " Ok"))
                    } else cat(paste0("[Down_Ref] ERROR: you have an internet connection but cannot ping to ",RefSOSname,
                                      ". It may be because of security reasons or download cannot be carried out.\n"))
                } else return(cat(paste0("[Down_Ref] ERROR: no internet connection. Download cannot be carried out."), sep = "\n"))
                # connect
                Endpoint <- sensorweb4R::Endpoint(RefSOSname)
                # Selecting service "AirSensEUR" with name
                srv <- sensorweb4R::services(Endpoint)
                cat(paste0("[Down_Ref] INFO, SOS services: ", sensorweb4R::label(srv), "\n"))
                # get the station number corresponding to Ref.SOS.name in sensorweb4R::label(stations(srv))
                if (Ref.SOS.name %in% sensorweb4R::label(sensorweb4R::stations(srv))) {
                    sta <- sensorweb4R::stations(srv)[grep(pattern = Ref.SOS.name, x = sensorweb4R::label(sensorweb4R::stations(srv)))]
                    cat(paste0("Station ",sensorweb4R::label(sta)," found.\n"))
                } else {
                    return(paste0("[Down_Ref] ERROR, ", Ref.SOS.name, 
                                  " is not found at the apiEndpoint. Correct the name of AirSensEUR or set Down.SOS to FALSE in the ASEconfig_xx.R file\n"))}
                # Downloading sensor data
                # Determining DateIN and DateEND for data download with a lubridate::interval
                DateEND <- csvFile.DateEND
                cat(paste0("[Down_Ref] INFO, last date in Reference data to be downloaded is: ", DateEND, "\n"))
                # set DateIN for data retrieving, either from origin or last date in previous DataFrame
                # DownloadSensor exists: check if we have a "DateEND.SOS.prev"
                if ("DateEND.Ref.prev" %in% objects(DownloadSensor)) {
                    # DateEND.Ref.prev exists: Check if NULL
                    if (shiny::isTruthy(DownloadSensor$DateEND.Ref.prev)) {
                        DateIN  <- max(DownloadSensor$DateEND.Ref.prev, csvFile.DateIN, na.rm = TRUE)
                    } else DateIN  <- csvFile.DateIN
                } else {
                    # DateEND.Ref.prev does not exist
                    DateIN  <- csvFile.DateIN}
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
                        Last.Day        <- lubridate::ceiling_date(as.POSIXct.Date(DateEND), unit = "day")}}
                # SOS downloading, added + 1 to be able to download current day
                # Select the timeseries of the station Ref.SOS.name
                ts <- sensorweb4R::timeseries(Endpoint, station = sta)
                cat(paste0("Timeseries at the station: ", sensorweb4R::label(ts),"\n"))
                # Keeping only selected pollutants
                if (shiny::isTruthy(RefSOSPollutants)) {
                    Keep.ts <- sapply(sensorweb4R::label(ts), function(i) {
                        for (j in RefSOSPollutants) if (grepl(j, i)) return(TRUE) else if (j == tail(RefSOSPollutants, n = 1)) return(FALSE)})
                    ts <- ts[Keep.ts]} 
                # fetch all the meta data of ts
                ts <- sensorweb4R::fetch(ts)
                while (DateIN.partial < Last.Day) {
                    # interval of time for the get data of SOS
                    date.partial <- lubridate::interval(DateIN.partial, DateEND.partial)
                    #for (i in seq(sta)) {
                    # Downloading
                    cat(paste0("[Down_Ref] INFO, SOS downloading from ", DateIN.partial, " to ", DateEND.partial), sep = "\n")
                    Buffer.list    <- lapply(ts, function(ts.ID) {
                        Buffer         <- sensorweb4R::getData(ts.ID, timespan=date.partial)
                        Pollutant.name <- sensorweb4R::label(phenomenon(ts.ID))
                        Buffer.DT      <- data.table::data.table(date=Buffer[[1]]@time)
                        Buffer.DT[, (Pollutant.name) := Buffer[[1]]@value]
                        return(Buffer.DT)})
                    #https://stackoverflow.com/questions/13273833/merging-multiple-data-tables
                    Buffer.DT <- Reduce(function(...) merge(..., all = TRUE), Buffer.list)
                    #}
                    # Appending downloaded data, no need to discard lines of NA at the end of Ref, since SOS does not return empty lines
                    if (exists("Reference.i")) Reference.i <- data.table::rbindlist(list(Reference.i, Buffer.DT), use.names = TRUE, fill = TRUE) else Reference.i <- Buffer.DT
                    # removing to avoid adding the same data for other date.partial
                    if (exists("Buffer.list")) rm(Buffer.list)
                    if (exists("Buffer.DT"))   rm(Buffer.DT)
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
                            DateEND.partial <- DateEND.partial + 24 * 60 * 60 * Duration}}}
                # interval of time for the get next data of SOS
                date.partial <- lubridate::interval(DateIN.partial, DateEND.partial)
                cat(paste0("[Down_Ref] INFO, Time zone for reference data: ", date.partial@tzone), sep = "\n")
            } else if (FTPMode == "a_i_p") {
                # Sensor Data retrieving at apiEndpoint
                cat("\n")
                cat("-----------------------------------------------------------------------------------\n")
                futile.logger::flog.info(paste0("[Down_Ref] INFO, ", Ref__a_i_p__name," reference data retrieving"))
                futile.logger::flog.info(paste0("[Down_Ref] INFO, downloading from ", csvFile.DateIN, " to ", csvFile.DateEND))
                Reference.i <- a_i_p_data(URL          = Ref__a_i_p__name,
                                          username     = User__a_i_p__,
                                          password     = Pass__a_i_p__,
                                          organisation = Ref__a_i_p__Organisation,
                                          station      = Ref__a_i_p__Station,
                                          start        = csvFile.DateIN,
                                          end          = csvFile.DateEND + 1,
                                          avgtime      = avgtime,
                                          param        = Ref__a_i_p__Pollutants,
                                          Time_zone    = ref.tzone)}
        }
        # Checking if data are available
        if (Reference.i[,.N] == 0) {
            my_message <- paste0("[Down_Ref] ERROR no data found for reference, lack of new data for ", Reference.name, "\n")
            cat(my_message)
            if (shiny) shinyalert(
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
                animation = FALSE)
        } else {
            # Discarding columns without name
            if (any(colnames(Reference.i) == "")) Reference.i <- Reference.i[,-which(names(Reference.i) == "")]
            # Adding coordinates of the reference stations
            if (shiny::isTruthy(coord.ref)) {
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
                    Reference.i$Ref.Lat  <- as.numeric(lat)}}
            # checking if we have a date column in the referenceValues()
            if (any(c("date","time","Date", "Time", "DATE", "TIME", "DateTime", "dateTime") %in% names(Reference.i))) {
                # checking if there is more than 1 field with "date","time","Date", "Time", "DATE", "TIME"
                if (length(which(c("date","time","Date", "Time", "DATE", "TIME", "DateTime", "dateTime") %in% names(Reference.i))) == 1 ) {
                    # setting name of timedate column as "date" for openair
                    data.table::setnames(Reference.i,grep(pattern = paste(c("date","time","Date", "Time", "DATE", "TIME", "DateTime", "dateTime"), collapse = "|"),
                                                          x = names(Reference.i), value = T),"date")
                    # convert date to POSIX with time zone set in shiny
                    if (!lubridate::is.POSIXct(Reference.i$date)) {
                        if (grepl("Z", Reference.i$date[1])) {
                            data.table::set(Reference.i, j = "date", value = lubridate::ymd_hms(Reference.i$date, tz = ref.tzone))
                        } else Reference.i$date <- as.POSIXct(Reference.i$date,  tz = ref.tzone,
                                                              tryFormats = c("%Y-%m-%d %H:%M:%OS",
                                                                             "%Y/%m/%d %H:%M:%OS",
                                                                             "%Y-%m-%d %H:%M:%S",
                                                                             "%Y-%m-%d %H.%M.%S",
                                                                             "%Y-%m-%d %H:%M",
                                                                             "%m/%d/%Y %H:%M",
                                                                             "%d/%m/%Y %H:%M",
                                                                             "%Y-%m-%d",
                                                                             "%m/%d/%Y")) # "%m/%d/%Y %H:%M", strptime removed with the format as this may cause a crash, but slower operation
                    }
                    
                    # discarding date outside csvFile.DateIN and csvFile.DateEND for the new reference data
                    if (FTPMode %in% c("ftp", "csv", "SOS", "a_i_p")) {
                        if (shiny::isTruthy(csvFile.DateIN)  && (lubridate::is.Date(csvFile.DateIN)  || lubridate::is.POSIXct(csvFile.DateIN)))  Reference.i <- Reference.i[date > csvFile.DateIN]
                        if (shiny::isTruthy(csvFile.DateEND) && (lubridate::is.Date(csvFile.DateEND) || lubridate::is.POSIXct(csvFile.DateEND))) Reference.i <- Reference.i[date <= as.Date(csvFile.DateEND)+1]}
                    
                    # check that there are new data left after selecting new dates
                    if (nrow(Reference.i) < 2) {
                        futile.logger::flog.warn("[Down_Ref] There are not new Reference data")
                        return()}
                    
                    # Convert all other columns to numeric if they are not excepts coordinates and keep known columns
                    for (j in names(Reference.i)[-which(names(Reference.i) %in% c("date", "Ref.Long", "Ref.Lat"))]) {
                        if (class(Reference.i[[j]]) != "numeric") Reference.i[[j]] <- as.numeric(Reference.i[[j]])   
                        # Replace nan and inf with NA
                        Nan.Inf2Na <- which(!is.finite(Reference.i[[j]]))
                        if (length(Nan.Inf2Na) > 0)  data.table::set(Reference.i, i = Nan.Inf2Na, j = j, value = rep(NA, length(Nan.Inf2Na)))
                        if (!j %in% unlist(Reference.names, use.names = F)) Reference.i[, (j):=NULL] else {
                            if (is.null(Ref.Type) || Ref.Type == "Ref") {
                                name.detected <- names(Reference.names)[which(sapply(Reference.names, function(i) any(i %in% j)))]
                                data.table::setnames(Reference.i, j, name.detected)}}} 
                    # aggregate Reference.i with mean over UserMins
                    Periodicity <- threadr::detect_date_interval(Reference.i$date, skip = 3, n = 50) / 60
                    if (Periodicity != UserMins) {
                        Reference.i <- DF_avg(Reference.i, width = UserMins)
                    } else if (lubridate::is.POSIXct(DownloadSensor$DateIN.Ref.prev) && lubridate::is.POSIXct(DownloadSensor$DateEND.Ref.prev) &&
                               !all( as.logical(Reference.i$date[as.Date(Reference.i$date) == as.Date(Reference.i$date)[1]] %in%
                                                seq(from = DownloadSensor$DateIN.Ref.prev,
                                                    to   = max(Reference.i$date, na.rm = TRUE),
                                                    by = paste0(toString(UserMins)," ","min"))) ) ) {
                        # Checking if dates fall on full hours
                        Reference.i <- DF_avg(Reference.i, width = UserMins)
                    }  else if (!all( as.logical(Reference.i$date[as.Date(Reference.i$date) == as.Date(Reference.i$date)[1]] %in%
                                                 seq(from = as.POSIXct(as.Date(Reference.i$date)[1]),
                                                     to   = as.POSIXct(as.Date(Reference.i$date)[length(as.Date(Reference.i$date))]),
                                                     by = paste0(toString(UserMins)," ","min"))) ) ) {
                        Reference.i <- DF_avg(Reference.i, width = UserMins)}
                    # Setting Reference names (change names of pollutants adding Ref.)
                    if (Ref.Type %in% c("Bin.DMPS", "Bin.APS", "FIDAS", "FIDAS","GRIMM")) {
                        # Adding label to pollutants which are not in Reference.names
                        names.not.Ref <- names(Reference.i)[grep(pattern = paste(c("date","Ref.", "Bin.DMPS.", "Bin.APS.", "GRIMM.", "FIDAS."), collapse = "|"), x = names(Reference.i), invert = TRUE)]
                        names(Reference.i)[which(names(Reference.i) %in% names.not.Ref)] <- sapply(seq_along(names.not.Ref), function(k) paste0(Ref.Type, ".", names.not.Ref[k]))}
                    # matching dates,
                    Common.dates <- which(Reference.i$date %within% date)
                    if (length(Common.dates) == 0) {
                        #Set time interval, with function interval of package lubridate
                        date <- lubridate::interval(min(Reference.i$date, na.rm = T), max(Reference.i$date, na.rm = T))
                        # the function  interval returns a variable of class lubridate
                        futile.logger::flog.info(paste0("[Down_Ref] INFO, Time zone for reference data: ", date@tzone, ""))
                        # creating returning data frame Ref
                        Ref <- data.table::rbindlist(list(Ref,data.table::data.table(date = seq(date@start, length = date@.Data/minSec, by = paste0(toString(UserMins)," ","min")))))
                        Common.dates <- which(Reference.i$date %within% date)}
                    
                    if (length(Common.dates) > 0) {
                        Ref <- data.table::merge.data.table(Ref, Reference.i[Common.dates], by = "date", all.x = T)
                        # Rows with date immediately after missing. Values of missing dates will be replaced with previous available values. Starts from the 2nd row of Common.dates since the 1st row is NA
                        Row2replace <- which(difftime(Reference.i[Common.dates]$date[2:nrow(Reference.i[Common.dates])], 
                                                      Reference.i[Common.dates]$date[1:(nrow(Reference.i[Common.dates])-1)], units = "mins") > UserMins)
                        if (Periodicity == 1440 && all(grepl(paste(c("date", "Long", "Lat", "PM"), collapse = "|"), names(Ref)))) {
                            # For PM daily filter change
                            # replacing NA with last non Na values if UserMins is lower than the periodicty of the downloaded measurements
                            if (length(Row2replace) > 0) {
                                for (i in Row2replace) {
                                    NA.Interval <- which(Ref$date %within% lubridate::interval(Reference.i[Common.dates, date][i] + UserMins*60, 
                                                                                               Reference.i[Common.dates, date][i] + (Periodicity - UserMins)*60))
                                    if (length(NA.Interval) > 0) {
                                        cat(paste0("Filling Na rows with first values at date ",Reference.i[Common.dates, date][i], "\n"))
                                        Values <- Reference.i[Common.dates, .SD, .SDcols = names(Ref)[-which(names(Ref) == "date")]][i]
                                        Values <- lapply(Values, function(i) rep(i, length(NA.Interval)))
                                        data.table::set(Ref, i = NA.Interval, j = names(Ref)[-which(names(Ref) == "date")], value = Values)
                                        rm(Values)}}}
                        } else {
                            # replacing NA with next non Na values if UserMins is lower or higher than the periodicity of the downloaded measurements
                            if (length(Row2replace) > 0) {
                                #Per.Mins <- Periodicity > UserMins
                                for (i in Row2replace) { # i is the missing dateTime
                                    #   if (Per.Mins) {
                                    NA.Interval  <- which(Ref$date > Reference.i[Common.dates, date][i] & Ref$date <= Reference.i[Common.dates, date][i+1] - Periodicity*60)
                                    Val.Interval <- which(Ref$date > Reference.i[Common.dates, date][i+1] - Periodicity*60 & Ref$date < Reference.i[Common.dates, date][i+1])
                                    if (length(NA.Interval) > 0) {
                                        futile.logger::flog.info(paste0("Filling rows at date before ",Reference.i[-1][Common.dates, date][i], " with NA."))
                                        Values <- Reference.i[-1][Common.dates, .SD, .SDcols = names(Ref)[-which(names(Ref) == "date")]][i]
                                        Values <- lapply(Values, function(i) rep(NA, length(NA.Interval)))
                                        data.table::set(Ref, i = NA.Interval, j = names(Ref)[-which(names(Ref) == "date")], value = Values)
                                        rm(Values, NA.Interval)}
                                    if (length(Val.Interval) > 0) {
                                        futile.logger::flog.info(paste0("Filling Na rows with last values at date ",Reference.i[-1][Common.dates, date][i]))
                                        Values <- Reference.i[-1][Common.dates, .SD, .SDcols = names(Ref)[-which(names(Ref) == "date")]][i]
                                        Values <- lapply(Values, function(i) rep(i, length(Val.Interval)))
                                        data.table::set(Ref, i = Val.Interval, j = names(Ref)[-which(names(Ref) == "date")], value = Values)
                                        rm(Values, Val.Interval)}}}}
                    } else futile.logger::flog.error("You are trying to add reference data from a file without date")
                } else {
                    my_message <- "[Down_Ref] ERROR, There is no or more than one column called  with names date, time, Date , Time, DATE, TIME or DateTime. The script is stopped"
                    cat(my_message)
                    if (shiny) shinyalert(
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
                        animation = FALSE)}
            } else {
                my_message <- "[Down_Ref] ERROR, There is no column called date, time, Date , Time, DATE, TIME or DateTime. The script is stopped"
                cat(my_message)
                if (shiny) shinyalert(
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
                    animation = FALSE)}
        }
    }
    # Do not remove rows filled with NA/NaN to Ref to avoid to add empty lines that will not be updated later with a new Download
    if (exists("Ref") && data.table::is.data.table(Ref) && nrow(Ref) > 0) {
        print(str(Ref), Quote = FALSE)}
    if (exists("Ref")) {
        if (length(names(Ref)) > 0 ) {
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            if(!data.table::is.data.table(Ref)) Ref <- data.table::data.table(Ref, key = "date")
            return(Ref)
        } else {
            # removing Ref when there are no sensor data
            remove(Ref)
            futile.logger::flog.info("[Down_Ref] there is no new data for the reference station",sep = "\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            return()}
    } else {
        # removing Ref when there are no sensor data
        remove(Ref)
        futile.logger::flog.warn("[Down_Ref] there is no new data for the reference station",sep = "")
        cat("-----------------------------------------------------------------------------------\n")
        cat("\n")
        return()}
}
#=====================================================================================CR
# 170609 MG : Pinging WEB site ####
#=====================================================================================CR
#' @description This function returns TRUE if it is possible to ping a test.site and FALSE otherwise
#' @param test.site the URL whose existence we are to test
PingThisSite <- function(test.site) {
    if (!require(httr)) {
        # httr needs to be installed, checking if internet is available
        install.packages("httr")}
    require(httr)
    # https://stackoverflow.com/questions/31420210/r-check-existence-of-url-problems-with-httrget-and-url-exists?noredirect=1&lq=1
    !httr::http_error(test.site)
}
#=====================================================================================CR
# 170609 MG : Pinging WEB site ####
#=====================================================================================CR
ping <- function(x, stderr = FALSE, stdout = FALSE, ...) {
    pingvec <- system2("ping", x,
                       stderr = FALSE,
                       stdout = FALSE,...)
    if (pingvec == 0) TRUE else FALSE
}
#=====================================================================================CR
# 170609 MG havingIP Pinging WEB site *** No more used since 2020-02-01 replaced with curl::has_internet() ####
#=====================================================================================CR
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
#=====================================================================================CR
# 160418 MGV: Validation.tool function for validations ####
#=====================================================================================CR
Tidy_Model.i <- function(Model.i, WDoutputMod, nameModel, Mod_type = NULL, Include.Model = FALSE, SAVE = TRUE) {
    # https://stackoverflow.com/questions/24559099/vectorize-environment-access-in-r
    # https://stackoverflow.com/questions/42230920/saverds-inflating-size-of-object
    # Model.i$Equation includes an environment that might be big. use format to all keep the character equation
    ENV <- new.env()
    if (shiny::isTruthy(Mod_type) && (Mod_type == "Linear.Robust" || Mod_type == "Linear.Robust.rqs")) {
        # In order to get repeatible se.error for rq and rqs use se.type = nid. Boot gives different se.error at each boot
        # as from comments, boot would be best but not repeatible. Boot is bery slow, makes a lot of computation
        # iid and nid assume specific distributions: asymptotic and linear
        # ker does not assume a specific distribution, but fits it. It is fast and repatible. We select it
        # see https://stats.stackexchange.com/questions/46434/quantile-regression-which-standard-errors
        ENV[["Model"]] <- list(Tidy = broom::tidy(Model.i,
                                                  se.type = "ker"),
                               Glance = broom::glance(Model.i))
    } else if (Mod_type == "Ridge") {
        ENV[["Model"]] <- list(Tidy = tibble::tibble(term      = names(coef(Model.i)),
                                                     estimate  = coef(Model.i),
                                                     std.error = summary(Model.i)$summaries[["summary  1"]]$coefficients[,3]),
                               Glance = tibble::as_tibble(summary(Model.i)$summaries[["summary  1"]]$stats)) # instead of broom::tidy(Model.i), 
        # Change name of R2 and adjR2 in Glance
        names(ENV[["Model"]][["Glance"]])[names(ENV[["Model"]][["Glance"]]) == "R2"] <- "r.squared"
        names(ENV[["Model"]][["Glance"]])[names(ENV[["Model"]][["Glance"]]) == "adj-R2"] <- "adj.r.squared"
    } else if (Mod_type == "Yatkin") {
        # Inc_RH
        ENV[["Model"]][[names(Model.i)[1]]] <- list(Tidy = broom::tidy(Model.i[[names(Model.i)[1]]]), Glance = broom::glance(Model.i[[names(Model.i)[1]]]))
        ENV[["Model"]][[names(Model.i)[1]]][["Coef"]]     <- coef(Model.i[[names(Model.i)[1]]])
        ENV[["Model"]][[names(Model.i)[1]]][["Equation"]] <- format(Model.i[[names(Model.i)[3]]])
        ENV[["Model"]][[names(Model.i)[1]]][["Call"]]     <- Model.i[[names(Model.i)[1]]]$call
        #Dec_RH
        ENV[["Model"]][[names(Model.i)[2]]] <- list(Tidy = broom::tidy(Model.i[[names(Model.i)[2]]]), Glance = broom::glance(Model.i[[names(Model.i)[2]]]))
        ENV[["Model"]][[names(Model.i)[2]]][["Coef"]]     <- coef(Model.i[[names(Model.i)[2]]])
        ENV[["Model"]][[names(Model.i)[2]]][["Equation"]] <- format(Model.i[[names(Model.i)[4]]])
        ENV[["Model"]][[names(Model.i)[2]]][["Call"]]     <- Model.i[[names(Model.i)[2]]]$call
    } else {
        ENV[["Model"]] <- list(Tidy = broom::tidy(Model.i),
                               Glance = broom::glance(Model.i))}
    
    # Adding constant elements to the broom list
    if (Mod_type != "Yatkin") {
        ENV[["Model"]][["Coef"]]     <- coef(Model.i)
        ENV[["Model"]][["Equation"]] <- format(Model.i$Equation)
        ENV[["Model"]][["Call"]]     <- Model.i$call}
    
    # Augment and Glance
    if (shiny::isTruthy(Mod_type) && Mod_type == "Linear.Robust"){
        
        ENV[["Model"]][["Augment"]]    <- augment(Model.i, data = data.frame(x = Model.i$x[,2], y = Model.i$y))
        ENV[["Model"]][["Glance"]]$r.squared <- R1_rq(ENV[["Model"]][["Augment"]]$x,ENV[["Model"]][["Augment"]]$y, probs = ENV[["Model"]][["Glance"]]$tau)
        
    } else if (shiny::isTruthy(Mod_type) && Mod_type == "Linear.Robust.rqs"){
        
        ENV[["Model"]][["Augment"]] <- data.table::data.table(x = Model.i$x, y = unname(Model.i$y))
        ENV[["Model"]][["Augment"]][, .tau := cut(y, quantile(y, probs = seq(0.1,0.9,0.1)))]
        levels(ENV[["Model"]][["Augment"]]$.tau) <- 1:ncol(Model.i$coefficients)
        # adding levels for y outside values of calibration
        Which.Low  <- which(ENV[["Model"]]$Augment$y < quantile(ENV[["Model"]]$Augment$y, probs = 0.1))
        Which.High <- which(ENV[["Model"]]$Augment$y > quantile(ENV[["Model"]]$Augment$y, probs = 0.9))
        if (length(Which.Low)  > 0) data.table::set(ENV[["Model"]]$Augment, i = Which.Low,  j = ".tau", value = 1)
        if (length(Which.High) > 0) data.table::set(ENV[["Model"]]$Augment, i = Which.High, j = ".tau", value = ncol(ENV[["Model"]]$Coef))
        ENV[["Model"]][["Augment"]][, slope     := Model.i$coefficients[2,.tau]]
        ENV[["Model"]][["Augment"]][, intercept := Model.i$coefficients[1,.tau]]
        ENV[["Model"]][["Augment"]][, .fitted    := x.x * slope + intercept]
        ENV[["Model"]][["Augment"]][, .resid     := y - .fitted]
        ENV[["Model"]][["Augment"]][, .predicted := (y - intercept) / slope]
        data.table::setnames(ENV[["Model"]][["Augment"]], "x.x", "x")
        #setorderv(ENV[["Model"]][["Augment"]], c("y", "x", ".tau", ".resid", ".fitted"))
        ENV[["Model"]][["Augment"]] <- tibble(ENV[["Model"]][["Augment"]][,-1])
        
    } else if (Mod_type == "Ridge") {
        ENV[["Model"]][["Augment"]] <- Model.i$Data
    } else if (Mod_type == "Yatkin") {
        # Inc_RH
        ENV[["Model"]][[names(Model.i)[1]]][["Augment"]] <- data.table::data.table(broom.mixed::augment(Model.i[[names(Model.i)[1]]]))
        #Dec_RH
        ENV[["Model"]][[names(Model.i)[2]]][["Augment"]] <- data.table::data.table(broom.mixed::augment(Model.i[[names(Model.i)[2]]]))
        
    } else ENV[["Model"]][["Augment"]] <- data.table::data.table(broom.mixed::augment(Model.i))
    if (Include.Model) ENV[["Model"]][["InitModel"]] <- Model.i
    if (SAVE) list.save(ENV[["Model"]], file = file.path(WDoutputMod, paste0(nameModel,".rdata")))
    return(ENV[["Model"]])}

#' Validation.tool function for validations ####
#' @param General dataframe/datatable containing all data within selected dates
#' @param DateIN/END           : as.POSIXct- datetime in and datetime out to start validation (adding 1 day to DateEND).
#' @param DateINCal/DateENDCal : Dates of previous calibration with which nameGasMod was calibrated. Default are DateIN/END.
#' @param DateINPlot/END       : as.POSIXct- datetime in and datetime out to plot time series, default are DateIN/END.
#' @param name.gas             : char() - gas component
#' @param model.log            : logic  - If true plotting times series of raw data with timePlot()
#' @param nameGasRef           : char   - column of gas reference data
#' @param nameGasVolt          : char   - column of gas gas sensor data in volt or nA
#' @param nameGasMod           : char   - column of gas gas sensor data in same unit as reference
#' @param unit.ref/sensor      : char   - units (ppb or ppm)
#' @param Sens.raw.unit        : raw unit of sensors: V or nA
#' @param Reference.name       : char   - name of reference data
#' @param AirSensEur.name      : char   - ID of AirSensEUR box
#' @param timeseries.display   : logic  - True -> displays timeseries
#' @param name.sensor          : char   - name of specific gas sensor
#' @param WDoutputMod          : char   - directory to save computed models (not used if (!model.log))
#' @param WDoutput             : char   - directory to save plots from etalonnage
#' @param WDoutputStats        : char   - directory to save statistics for modelled data
#' @param process.step         : char   - variable to refer to the process status (e.g. calibration, modelling)
#' @param mod.eta.model.type   : char   - name for model in Etalonnage and Cal_line
#' @param Multi.File           : char, default is NULL, path.file of the config file used for calibration with multivariates
#' @param eta.model.type       : char   - name for evaluation in Etalonnage and Cal_line
#' @param Covariates           : List of covariates to calibrate, vector of characters, default is NULL
#' @param remove.neg           : logical, default TRUE, if TRUE discard negative from calibrated data after calibration
#' @param Plot_Line            : logical, default is TRUE. If TRUE the calibration line is added using par(new=TRUE) to an existaing scatterplot
#' @param PlotCal              : logical, default TRUE, if TRUE plot the calibrated data (scatterplot and timeseries) after calibration
#' @param Auto.Lag             : logical, default is FaLSE If Auto.Lag is TRUE, y is changed using the lag at which cross correlation between x and y is maximum using ccf( )
#' @param DateCal default value NULL. If not, numeric vector giving the indexes of dates in ASE.ID$General.DT used for calibration
#' @return General dataframe/datatable with the whole data.table General modelled values
Validation.tool <- function(General, DateIN, DateEND, DateINCal = DateIN, DateENDCal = DateEND, name.gas, model.log, nameGasRef, nameGasVolt, nameGasMod,
                            unit.ref, unit.sensor, Sens.raw.unit = NULL, Reference.name, AirSensEur.name, name.sensor,
                            timeseries.display, DateINPlot = DateIN, DateENDPlot = DateEND,
                            WDoutputMod, WDoutput, WDoutputStats, process.step, mod.eta.model.type, Probs = NULL, Multi.File = NULL,
                            eta.model.type, remove.neg = TRUE, Covariates = NULL, Plot_Line = TRUE, PlotCal = TRUE, Auto.Lag = FALSE, Verbose = FALSE, Include.Model = FALSE, SAVE = TRUE, Weighted = FALSE, 
                            DateCal = NULL) {
    # determining number pf plots
    op <- par(no.readonly = TRUE)
    if (model.log) {
        if (timeseries.display) {par(mfrow = c(1,2))} else {par(mfrow = c(1,1))}
    } else {
        if (timeseries.display) {par(mfrow = c(1,2))} else {par(mfrow = c(1,1))}
        on.exit(par(op))
    }
    # Restoring graphical parameters on exit of function, even if an error occurs
    if (model.log) {
        # timeplots uncalibrated values
        if (timeseries.display) {
            Relationships         <- na.omit(colnames(General)[colnames(General) %in% Covariates])
            if (shiny::isTruthy(Relationships) && !identical(character(0), Relationships)) {
                timePlot(mydata = General[ date > DateINPlot & date <= DateENDPlot + 1,], pollutant = Relationships, date.pad = TRUE, auto.text = FALSE, y.relation = "free",
                         main = paste0(AirSensEur.name, ": Effects on ", name.sensor," from ",format(DateIN,"%d-%b-%y")," to ",format(DateEND,"%d-%b-%y"),
                                       " at ",Reference.name))
                # save plots in files
                dev.copy(png, filename = file.path(WDoutput,paste0(AirSensEur.name,"_",name.sensor,"_Effects",
                                                                   "_Timeserie_",format(DateINPlot,"%Y-%m-%d"),"_",format(DateENDPlot,"%Y-%m-%d"),".png")),
                         units = "cm",
                         res = 300,
                         width = 35.5,
                         height = 25)
                dev.off()}
        }
        # checking completeness of x, y and Covariates, taking care of POSIXct if any
        if(is.null(DateCal)) General <- General[date > DateINCal & date <= DateENDCal + 1,] else General <- General[DateCal]
        if (data.table::is.data.table(General)) {
            if (!shiny::isTruthy(Covariates) || length(Covariates) == 0) {
                General <- General[is.finite(rowSums(General[,.SD,.SDcols = c(nameGasRef,nameGasVolt)])),.SD,
                                   .SDcols = c(nameGasRef,nameGasVolt, "date")]
            } else {
                General <- General[is.finite(rowSums(General[,.SD,.SDcols = c(nameGasRef,nameGasVolt,Covariates[!sapply(General[,..Covariates], inherits, "POSIXct")])])),
                                   .SD, .SDcols = unique(c(nameGasRef,nameGasVolt,Covariates, "date"))]}
        } else if (is.data.frame(General)) {
            if (!shiny::isTruthy(Covariates) || length(Covariates) == 0) {
                General <- General[is.finite(rowSums(General[,c(nameGasRef,nameGasVolt)]))]
            } else {
                General <- General[is.finite(rowSums(General[,c(nameGasRef,nameGasVolt,Covariates[!sapply(General[,..Covariates], inherits, "POSIXct")])]))]}}
        # Setting axis and labels
        if (mod.eta.model.type == "gam") {# General additive model
            y = General[[nameGasRef]]
            x = General[[nameGasVolt]]
            AxisLabelY = paste0(nameGasRef," ", unit.ref, " ",Reference.name)
            AxisLabelX = paste0(nameGasVolt," in ", Sens.raw.unit," ", AirSensEur.name)
        } else {
            x = General[[nameGasRef]]
            y = General[[nameGasVolt]]
            AxisLabelX = paste0(nameGasRef," ", unit.ref, " ",Reference.name)
            AxisLabelY = paste0(nameGasVolt," in ", Sens.raw.unit," ", AirSensEur.name)
        }
        if (Plot_Line) EtalLim <- Etalonnage( x = x, s_x = NULL, y = y , s_y = NULL
                                              , AxisLabelX = AxisLabelX
                                              , AxisLabelY = AxisLabelY
                                              , Title = paste0(AirSensEur.name, ": ",process.step," ", name.sensor," from ",format(DateIN,"%d-%b-%y")," to ",format(DateEND,"%d-%b-%y"), " at ",Reference.name)
                                              , Marker = 1, Couleur = "blue", ligne = 'p', XY_same = FALSE, lim = NULL, steps = c(10,10)
                                              , digitround = c(3,3), marges = c(4,4,3,0.5))
        if (mod.eta.model.type == "MultiLinear") {
            Matrice         <- General[, .SD, .SDcols = Covariates]
            names(Matrice)  <- Covariates
            if (shiny::isTruthy(Multi.File)) {
                if (file.exists(Multi.File)) {
                    # read Multi.File
                    Multi.File.df <-  data.table::data.table(file = Multi.File, comment.char     = "#")
                    
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
        } else if (any(mod.eta.model.type %in% c("NO2_Lab", "NO2_Lab_decay_inc"))) {
            if (!shiny::isTruthy(Covariates) || any(Covariates == "")) Covariates <- c("Out.Relative_humidity", "Out.Temperature")
            Degrees          <- rep(1, times = length(Covariates))
            namesCovariates  <- paste0(paste(Covariates,Degrees, sep = "-"),collapse = "&")
            Matrice          <- General[, .SD, .SDcols = c(Covariates, "date")]
            #names(Matrice)  <- namesCovariates
        } else if (any(mod.eta.model.type %in% c("Peaks_baseline", "Yatkin","exp_kT_NoC", "exp_kT", "exp_kTn", "exp_kK", "T_power", "K_power"))) {
            if (!shiny::isTruthy(Covariates) || any(Covariates == "")) Covariates <- c("Out.Temperature", "Out.Relative_humidity", "Absolute_humidity")
            Degrees          <- rep(1, times = length(Covariates))
            namesCovariates  <- paste0(paste(Covariates,Degrees, sep = "-"),collapse = "&")
            Matrice          <- General[, .SD, .SDcols = c(Covariates, "date")]
            #names(Matrice)  <- namesCovariates
        }  else if (any(mod.eta.model.type %in% c("PLS", "Ridge"))) {
            if (!shiny::isTruthy(Covariates) || any(Covariates == "")) Covariates <- c("Out.Temperature", "Out.Relative_humidity")
            Degrees          <- rep(1, times = length(Covariates))
            namesCovariates  <- paste0(paste(Covariates,Degrees, sep = "-"),collapse = "&")
            Matrice          <- General[, .SD, .SDcols = c(Covariates, "date")]
            #names(Matrice)  <- namesCovariates
        } else if (any(mod.eta.model.type %in% c("BeerLambert"))) {
            Covariates      <- c("Out.Temperature", "Out.Atmospheric_pressure")
            Degrees         <-  c(1,-1)
            namesCovariates <- paste0(Covariates,collapse = "&")
            Matrice         <- General[, .SD, .SDcols = Covariates]
        } else if (any(mod.eta.model.type %in% c("Kohler", "Kohler_modified", "Kohler_lit", "Kohler_only"))) {
            if (is.null(Covariates)) Covariates <- c("Out.Relative_humidity")
            Degrees         <- 1
            namesCovariates <- paste0(paste(Covariates,Degrees, sep = "-"),collapse = "&")
            Matrice         <- General[, .SD, .SDcols = Covariates]
            #names(Matrice)  <- namesCovariates
        } else {
            namesCovariates <- ""
            Matrice         <- NULL}
        Model.i <- Cal_Line(x = x, s_x = NULL,
                            y = y, s_y = NULL,
                            Mod_type      = mod.eta.model.type,
                            Probs         = Probs,
                            Multi.File    = Multi.File,
                            Covariates    = Covariates,
                            Matrice       = Matrice,
                            Sensor_name   = name.sensor,
                            lim           = EtalLim,
                            Auto.Lag      = Auto.Lag,
                            Plot_Line     = Plot_Line,
                            Verbose       = Verbose,
                            Weighted      = Weighted,
                            Date          = General$date)
        # saving the model
        nameModel  <- paste0(paste0(c(AirSensEur.name,name.sensor,Sens.raw.unit,mod.eta.model.type,format(DateIN,"%Y%m%d"),format(DateEND,"%Y%m%d"),namesCovariates, Weighted), "__"), collapse = "")
        Model.i    <- Tidy_Model.i(Model.i, WDoutputMod, nameModel, Mod_type = mod.eta.model.type, Include.Model = Include.Model, SAVE = SAVE)
        if (mod.eta.model.type == "Yatkin") Model.i$nameModel <- paste0(nameModel, '.rdata')
        # save scatterplots in files
        if (Plot_Line) {
            NameFile <- file.path(WDoutput, paste0(nameModel,"__",process.step,".png"))
            dev.copy(png, filename = NameFile, units = "cm", res = 300, width = 20, height = 20)
            dev.off()}}
    if (PlotCal) {
        # Fill in General with modelled data, first reintialise nameGasMod
        if (nameGasMod %in% names(General)) General[, (nameGasMod) := NULL ] 
        Rows <- which(General$date > DateIN  & General$date <= DateEND + 1 & !is.na(General[, nameGasVolt, with = F]))
        if (mod.eta.model.type == "Linear" || mod.eta.model.type == "Linear.Robust") {
            data.table::set(General, i = Rows, j = nameGasMod, value = (General[Rows,nameGasVolt,with = F] - Model.i$Coef[1]) / Model.i$Coef[2])
        } else if (mod.eta.model.type == "gam") {
            data.table::set(General, i = Rows, j = nameGasMod, 
                            value = predict(Model.i, newdata = data.frame(x = General[Rows, nameGasVolt, with = FALSE]), type = "response"))}
        # Remove negative values when using the linear median regression (linear.robust). This is good to do
        if (remove.neg) {
            index.which <- which(General[(General$date > DateIN & General$date <= DateEND + 1),nameGasMod, which = F] < 0., arr.ind = TRUE)
            if (length(index.which)>0) {
                data.table::set(General, i = index.which, j = nameGasMod, value = rep(NA_real_, times = length(index.which)))
                cat(paste0("Length of values < zero: ", length(index.which), " out of ",length(General[(General$date > DateIN & General$date <= DateEND +1),nameGasMod])), sep = "\n")
                cat(paste0("Loss of ",format(length(index.which)/length(General[(General$date > DateIN & General$date <= DateEND + 1),nameGasMod])*100), digit = 0, " [%] of data"), sep = "\n")
            } else {}
        } else {}
        # plotting the modelled/calibrated values
        EtalLim <- Etalonnage( x = unlist(General[Rows,..nameGasRef]), s_x = NULL, 
                               y = unlist(General[Rows,..nameGasMod]), s_y = NULL, 
                               AxisLabelX = paste0(nameGasRef," ",unit.ref," ",Reference.name), 
                               AxisLabelY = paste0(nameGasMod," ", unit.sensor, " ", AirSensEur.name), 
                               Title = paste0(AirSensEur.name, ": Calibrated ", name.sensor," from ",format(DateIN,"%d-%b-%y")," to ",format(DateEND,"%d-%b-%y"),
                                              " at ",Reference.name), 
                               Marker = 1, Couleur = "blue", ligne = 'p', XY_same = TRUE, lim = NULL, steps = c(10,10), 
                               digitround = c(2,3), marges = c(4,4,3,0.5))
        Model.i <- Cal_Line(x = unlist(General[Rows,..nameGasRef]), s_x = NULL,
                            y = unlist(General[Rows,..nameGasMod]), s_y = NULL, 
                            Mod_type = eta.model.type, Matrice = General, Sensor_name = "", lim = EtalLim)
        # save scatterplots in files
        if (is.null(DateINCal) & is.null(DateENDCal)) { 
            chaine <- paste0(mod.eta.model.type, "_", format(DateIN,"%Y%m%d"),"_",format(DateEND,"%Y%m%d"))
        }  else { 
            chaine <- paste0(mod.eta.model.type, "_", format(DateINCal,"%Y%m%d"),"_",format(DateENDCal,"%Y%m%d"))}
        if (remove.neg) negatif <- "_remove.neg_" else negatif <- "_"
        dev.copy(png,filename = file.path(WDoutput, paste0(AirSensEur.name,"_",name.sensor,"_","Calibrated", "_",eta.model.type,negatif,format(DateIN,"%Y%m%d"),
                                                           "_",format(DateEND,"%Y%m%d"),"_", chaine,".png"))
                 , units = "cm", res = 300, width = 25, height = 25)
        dev.off()
        # timeplots calibrated values
        if (timeseries.display) {
            if (model.log) {
                timePlot(General[ General$date > DateINPlot & General$date <= DateENDPlot +1,], pollutant = c(nameGasRef,nameGasMod), group=TRUE, date.pad=TRUE
                         , main = paste0(AirSensEur.name, ": Calibrated ", name.sensor," from ",format(DateIN,"%d-%b-%y")," to "
                                         , format(DateEND,"%d-%b-%y"), " at ",Reference.name))
            } else {
                timePlot(General[ General$date > DateINPlot & General$date <= DateENDPlot +1,], pollutant = c(nameGasRef,nameGasMod), group=TRUE, date.pad=TRUE
                         , main = paste0(AirSensEur.name, ": Calibrated ", name.sensor," from ",format(DateIN,"%d-%b-%y")," to "
                                         , format(DateEND,"%d-%b-%y"), " at ",Reference.name)
                         , ref.x = list(v = c(DateINCal, DateENDCal), lty = c(1, 1), col = c("black", "black"), lwd = c(2,2)))
            }
            # save plots in files
            NameFile <- file.path(WDoutput,paste0(AirSensEur.name,"_",name.sensor,"_","Modelled",negatif,
                                                  "_TS_",format(DateINPlot,"%Y%m%d"),"_",format(DateENDPlot,"%Y%m%d"),"_",chaine,".png"))
            dev.copy(png, filename = NameFile, units = "cm", res = 300, width = 35.5, height = 20);
            dev.off()
        }
    }
    # statistics
    if (Verbose) {
        gas.statistics <-modStats(General[ (General$date > DateIN & General$date <= DateEND + 1),]
                                  , mod = nameGasMod
                                  , obs = nameGasRef
                                  , statistic = c("n", "FAC2", "MB", "MGE", "NMB", "NMGE", "RMSE", "r", "COE", "IOA")
                                  , type = "default"
                                  , rank.name = NULL)
        cat(paste0("Statistics results for ASE Box ",AirSensEur.name, " sensor ",name.sensor, " in ",process.step, " with ",eta.model.type, " comparison with reference data.\n"))
        print(summary(gas.statistics))
        saveRDS(gas.statistics ,
                file = file.path(WDoutputStats, paste0(AirSensEur.name, "_", name.sensor, "_",process.step, "_",Sens.raw.unit, "_","Stats",eta.model.type, "_",
                                                       format(DateIN,"%Y%m%d"),"_",format(DateEND,"%Y%m%d"),".rds") )  )
        # Warnings
        stats.thres <- 0.5 # thresholds to nitify for new calibration to be performed
        if (gas.statistics$r^2 <stats.thres & gas.statistics$IOA < stats.thres) {
            cat(paste0("***** Warning message*****"," R2 and IOA are < stats.thres for ", name.sensor," in ", process.step, "\n"))
        }
    }
    return(Model.i)
}
#=====================================================================================CR
# 170609 MG SearchFile Selecting a fine name in window ####
#=====================================================================================CR
SearchFile <- function(dirCurrent = getwd(), Caption = "", Filters = matrix(c("ASEConfig", ".R"),1, 2, byrow = TRUE)) {
    # This function selects and return a file name in a window. It is compatible with Windows and Linux
    # The operating system is automatically detected.
    #
    # dirCurrent        : directory in which the window will list the available file
    # Filters           : a matrix of filename filters
    # return            : a file.path to the selected file
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
            if (!all("tcltk" %in% installed.packages())) librarian::shelf("tcltk", cran_repo = "https://cran.r-project.org")
            FilePath <- tk_choose.files(default = file.path(dirCurrent,"ASEconfig*.R"), caption = Caption,
                                        multi = FALSE, filters = Filters,
                                        index = nrow(Filters))
        } else {
            # try selecting with JAVA jchoose.files()
            list.Packages <- c("rJava", "rChoiceDialogs")
            if (!all(list.Packages %in% installed.packages())) librarian::shelf(list.Packages, cran_repo = "https://cran.r-project.org");rm(list.Packages)
            FilePath <- jchoose.files(default = file.path(dirCurrent,"ASEconfig*.R"), caption = Caption,
                                      multi = FALSE, filters = Filters,
                                      index = nrow(Filters), modal = canUseJavaModal())
        }
    }
    return(FilePath)
}
#=====================================================================================CR
# 170609 MG ASEPanel04File Looking for the sensor config file ####
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
            list.Packages <- c("kimisc", "knitr") # for thisfile()
            if (!all(list.Packages %in% installed.packages())) librarian::shelf(list.Packages, cran_repo = "https://cran.r-project.org");rm(list.Packages)
            if (shiny::isTruthy(thisfile())) {
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
# 170609 MG ASEPanel04Read Reading the sensor config file ####
#=====================================================================================CR
#' This function read the config parameters of the AirSensEURPanel version 0.4 - All values are in Hexadecimal and need be converted
#' @param ASEPanel04File character vector, the file path to be read, character vector
#' @return a dataFrame SensorConfig with all sensor parameters of a AirSensEURPanel config file
ASEPanel04Read <- function(ASEPanel04File = NULL , dirASEPanel = c("AirSensEURPanelR04"), dirCurrent = getwd()) {
    
    ASEFile <- read.table(file = ASEPanel04File, header = FALSE, sep = ":", stringsAsFactors = FALSE)
    # dataFrame of sensor config
    # typical order of sensors, that may be changed after reading the the shield config file
    name.gas <- c("NO2", "CO" , "O3",  "NO" )
    sens2ref <- data.frame(name.gas          = name.gas,
                           Ref               = c(-999,-999,-999,-999),
                           RefAD             = c(-999,-999,-999,-999),
                           RefAFE            = c(-999,-999,-999,-999),
                           check.names       = FALSE,
                           stringsAsFactors  = FALSE)
    # load packages for alphanumeric operations
    if (!"BMS" %in% installed.packages()) librarian::shelf("BMS", cran_repo = "https://cran.r-project.org")
    require(BMS)
    for (i in 1:4) {
        # Name of Sensor
        Command <- ASEFile[which(ASEFile[,2] == paste0("Write Preset Name for channel ",i-1))[1], 1]
        # Discarding curly bracket at the beginning and end of the string under linux
        Command <- gsub("[{}]", "", Command)
        # discarding W at the beginning and end
        Command <- substring(Command,2, nchar(Command))
        hCommand <- sapply(seq(1, nchar(Command), by=2), function(x) substr(Command, x, x+1))
        # discarding "00" trayling elements
        if (hCommand[1]                == "00") hCommand <- hCommand[-1]
        if (hCommand[length(hCommand)] == "00") hCommand <- hCommand[-length(hCommand)]
        # https://stackoverflow.com/questions/29251934/how-to-convert-a-hex-string-to-text-in-r
        sens2ref$name.sensor[i] <- gsub('[^[:print:]]+', '', rawToChar(as.raw(strtoi(hCommand, 16L))))
        # LMP9100 Register
        Command <- ASEFile[which(ASEFile[,2] == paste0("LMP9100 Register Setup for channel ",i-1))[1], 1]
        # Discarding curly bracket at the beginning and end of the string under linux
        Command <- gsub("[{}]", "", Command)
        # discarding R at the beginning
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
        #sens2ref$GAIN[i]  <- 1 + (sens2ref$TIA_Gain[i]/sens2ref$Rload[i])
        if (sens2ref$TIA_Gain[i]  != 1000000) {
            sens2ref$GAIN[i] <- 1/(1/1000000 + 1/sens2ref$TIA_Gain[i])
        } else sens2ref$GAIN[i]  <- sens2ref$TIA_Gain[i]
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
            # Discarding curly bracket at the beginning and end of the string under linux
            Command <- gsub("[{}]", "", Command)
            # discarding W atthe beginning
            Command <- substring(Command,2, nchar(Command))
            # Discarding curly bracket at the beginning and end of the string under linux
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
# 170619 MG ShowConf Showing the sensor configuration in a window ####
#=====================================================================================CR
ShowConf <- function(mat1) {
    # This fucntion is to create one window to show a matrix of data sensor configuration
    if ("tcltk2" %in% installed.packages()) librarian::shelf(list.Packages, cran_repo = "https://cran.r-project.org");rm("tcltk2")
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
# 170721 MG INFLUXDB Downloading INFLUXDB data ####
#=====================================================================================CR
#' @param WDoutput          : char   - directory to save InfluxData.csv
#' @param DownloadSensor    : output of function Check_Download
#' @param Parameters PROXY  : PROXY, URL, PORT, LOGIN, PASSWORD see Down_INflux()
#' @param Parameters Influx : Down.Influx, Host, Port, User, Pass, name.SQLite, name.SQLite.old, Db, Dataset, Influx.TZ
#' @param Sqlite database   : name.SQLite,name.SQLite.old
#' @param Sensor.Date       : chararter vector with 2 strings, default is NULL, Range of date for Influx download, 1st string is the starting date (YYY-MM-DD) for Influx download, 2nd string is the ending date.
#' @param sens2ref          : data.table or dataframe: Configuration of sensors, output of function CONFIG. Only used in case of K96 sensor with CO2 on LPLCPC sensor (to divide CO2 / 10).
#' @param asc.File          : dataframe, default is NULL, used for giving the correct name of the sensor
#' @param Influx.file       : character vector, default is null, File path of Influx data file. Can be .csv or .Rdata
#' @param InfluxData        : data.table or dataframe, default is null. DataSet of sensor values
#' @param Parallel          ; logical, default is FALSE. If TRUE airsenseur.db is read in parallel computing with 4 cores
#' @param Monitor           ; logical, default is TRUE. If TRUE data is monitored for missing data using function Data_reception()
#' @details 
#' At the end of the process the following file are saved: InfluxData.cfg (data), and configuration file var.names.meteo.cfg, var.names.Pollusens.cfg and var.names.sens.cfg
#' the function returns a  list with elements: InfluxData a datable with influx data, and configuration lists: var.names.meteo, var.names.Pollusens and var.names.sens.
#' Additionally if the argument Monitor is TRUE a plot showing missing and available data is saved in WDoutput. If data are missing a warning mail is sent
#' 
INFLUXDB <- function(WDoutput, DownloadSensor, UserMins,
                     PROXY, URL, PORT, LOGIN, PASSWORD,
                     Down.Influx = FALSE, Host, Port, User , Pass, name.SQLite, name.SQLite.old, Db, Dataset, Influx.TZ = NULL, Sensor.Date = NULL,
                     sens2ref = NULL, asc.File=NULL, InfluxData = NULL, Parallel = F, Monitor = T) {
    cat("-----------------------------------------------------------------------------------\n")
    futile.logger::flog.info(paste0("[INFLUXDB] ",Dataset," Downloading InfluxDB data"))
    # Saving Influx Sensor data
    if (is.null(DownloadSensor$Influx.Rdata.file)) DownloadSensor$Influx.Rdata.file <- file.path(WDoutput, "InfluxData.csv")
    # Influx.Rdata.file = file.path(WDoutput, "InfluxData.Rdata")
    # Influx.csv.file   = file.path(WDoutput, "InfluxData.csv"  )
    
    if (DownloadSensor$Retrieve.data.Influx) {
        # Initial date for creating or updating INfluxData
        if (!shiny::isTruthy(DownloadSensor$DateEND.Influx.prev)){
            # If airsenseur.db does not exist, first time download
            if (shiny::isTruthy(DownloadSensor$DateIN.db.prev)){
                Init.DB <- Set_date2POSIXct(DownloadSensor$DateIN.db.prev, tz = Influx.TZ)
            } else {
                futile.logger::flog.info(paste0("[INFLUXDB] non begining date in airsenseur.db assuming start on 2018-12-01 15:00:00"))
                Init.DB <- Set_date2POSIXct("2018-12-01 15:00:00", tz = Influx.TZ)   
            }
        } else Init.DB <- Set_date2POSIXct(DownloadSensor$DateEND.Influx.prev, tz = Influx.TZ)
        
        # Downloading INluxData if requested
        if (Down.Influx) {
            # downloading data from InfluxDB and updating airsenseur.db, the function does not return data that are saved into airsenseur.db
            Influx.TZ <- Down_Influx(PROXY = PROXY, URL = URL  , PORT = as.numeric(PORT), LOGIN = LOGIN, PASSWORD = PASSWORD,
                                     Host = Host  , User = User, Port = as.numeric(Port), Pass = Pass, name.SQLite = name.SQLite, name.SQLite.old = name.SQLite.old,
                                     Db = Db      , Dataset = Dataset, Influx.TZ = Influx.TZ, Sensor.Date = Sensor.Date,use_google = FALSE, Page = 10000, Mean = as.numeric(UserMins))
            
            # Creating or updating INfluxData
            # if there are problems accessing port 443 for the google api to determine time zone add , use_google = FALSE
            # Sqlite2df returns only the new data from the AirSensEUR.db, if the whole set is needed add: Complete = TRUE in function Down_Influx
            if (file.exists(DownloadSensor$airsenseur.db.file) && 
                (base::difftime(Influx.TZ[["LastDate"]], Init.DB, units = "min") > UserMins || !file.exists(DownloadSensor$Influx.Rdata.file))) {
                InfluxDataNew <- Sqlite2df(name.SQLite = name.SQLite, Dataset = Dataset, Influx.TZ = Influx.TZ[["Influx.TZ"]], 
                                           UserMins = UserMins, DownloadSensor = DownloadSensor, asc.File = asc.File, InfluxData = InfluxData, Parallel = Parallel)
            }    
        } else futile.logger::flog.info(paste0("[INFLUXDB] ",Dataset, " Data download not requested."))
    } else futile.logger::flog.info(paste0("[INFLUXDB] ",Dataset, " Data download is already up to date."))
    # Trying to use the existing data or Influx.Rdata.file
    if (is.null(InfluxData) || !isTruthy(InfluxData)) {
        if (file.exists(DownloadSensor$Influx.Rdata.file)) {
            if (file_ext(DownloadSensor$Influx.Rdata.file) == "csv") {
                InfluxData <- data.table::fread(file = DownloadSensor$Influx.Rdata.file, na.strings = c("","NA", "<NA>"))
                if (shiny::isTruthy(Influx.TZ) && Influx.TZ != "") {
                    data.table::set(InfluxData, j = "date", value =  lubridate::ymd_hms(InfluxData[["date"]], tz = Influx.TZ[["Influx.TZ"]]))
                } else data.table::set(InfluxData, j = "date", value =  lubridate::ymd_hms(InfluxData[["date"]], tz = "UTC"))
            } else if (file_ext(DownloadSensor$Influx.Rdata.file) == "Rdata") {
                load(DownloadSensor$Influx.Rdata.file)
                InfluxData <- data.table::data.table(InfluxData)}
        } else {
            futile.logger::flog.info(paste0("[INFLUXDB] ",Dataset," there is no previously saved Influx data. 
                                            Missing InfluxData and Down.Influx request of sensor data download ."))}}
    
    # merging InfluxData and InfluxDataNew if needed. InfluxDataNew can be a charter type. "There are no new data"
    if (exists("InfluxDataNew") && shiny::isTruthy(InfluxDataNew) && data.table::is.data.table(InfluxDataNew)) {
        if (exists("InfluxData") && shiny::isTruthy(InfluxData)) {
            InfluxData <- data.table::rbindlist(list(InfluxData, InfluxDataNew), use.names = TRUE, fill = TRUE) # rbind.fill(InfluxData,InfluxDataNew)
            rm(InfluxDataNew)
        } else {
            InfluxData <- InfluxDataNew
            rm(InfluxDataNew)}
        
        # In case of K96 sensor with CO2 channel on LPLCPC, the CO2 values shall be divided by 10 to get CO2 values in ppm
        if(any(grepl("LPLCPC", sens2ref$name.sensor)) && sens2ref$name.gas[grep("LPLCPC", sens2ref$name.sensor)] == "CO2"){
            data2Correct <- which(InfluxData[["K96_LPLCPC"]] > 3000)
            if(length(data2Correct) > 0) data.table::set(InfluxData, i = data2Correct, j = "K96_LPLCPC", value = InfluxData[data2Correct][["K96_LPLCPC"]]/10)}
        
        if (file_ext(DownloadSensor$Influx.Rdata.file) == "csv") {
            duplicated.Data <- which(duplicated(InfluxData$date, fromLast = T))
            if (length(duplicated.Data) > 0) InfluxData <- InfluxData[-duplicated.Data]
            data.table::fwrite(InfluxData     , file = DownloadSensor$Influx.Rdata.file, na = "NA")
        } else if (file_ext(DownloadSensor$Influx.Rdata.file) == "Rdata") save(InfluxData, file = DownloadSensor$Influx.Rdata.file)
        futile.logger::flog.info(paste0("[INFLUXDB] ", Dataset," Influx Sensor data saved in ", DownloadSensor$Influx.Rdata.file))}
    
    # returning data
    if (shiny::isTruthy(InfluxData)) {
        var.names.meteo <- c("Temperature", "Temperature_int","Relative_humidity", "Relative_humidity_int","Atmospheric_pressure")
        var.names.meteo <- var.names.meteo[var.names.meteo %in% names(InfluxData)]
        # setting the name of sensors
        var.names.sens  <- colnames(InfluxData)[-grep(pattern = paste0(c("date","_raw","gpsTimestamp","boardTimeStamp",  "channel", 
                                                                         "latitude", "longitude", "altitude"),collapse = "|"), x = colnames(InfluxData))]
        if (length(var.names.sens) == 0) {
            stop(futile.logger::flog.error(paste0("[INFLUXDB] ",Dataset," ERROR: no sensor variable downloaded for ", Dataset," InFluxDB. Please check in the INfluxDB client -> STOP")))
        } else futile.logger::flog.info((paste0("[INFLUXDB] ",Dataset," Sensor variables existing in airsenseur.db: ", paste0(var.names.sens, collapse = ", "), ", with date timestamp and coordinates.")))
        # Setting the Sensor names
        var.names.Pollusens <- var.names.sens[-(which(var.names.sens %in% var.names.meteo))]
        data.table::fwrite(data.table::data.table(columns = var.names.meteo)    , file = file.path(dirname(DownloadSensor$WDinput), "Configuration","var.names.meteo.cfg"), na = "NA")
        data.table::fwrite(data.table::data.table(columns = var.names.Pollusens), file = file.path(dirname(DownloadSensor$WDinput), "Configuration","var.names.Pollusens.cfg"), na = "NA")
        data.table::fwrite(data.table::data.table(columns = var.names.sens)     , file = file.path(dirname(DownloadSensor$WDinput), "Configuration","var.names.sens.cfg"), na = "NA")
        futile.logger::flog.info(paste0("[INFLUXDB] ",Dataset, " INFLUXDB returning list with InfluxData, var.names.meteo, var.names.Pollusens and var.names.sens"))
        
        # Updating in order not to have to use Check_Download which is rather long
        DownloadSensor$ExistFil.data.db = TRUE
        # if data have been downloaded from influx update date of database, otherwiser Influx.TZ[["FirstDate"]] and Influx.TZ[["LastDate"]] do not exist and Influx.TZ is "UTC"
        if(Down.Influx){
            DownloadSensor$Retrieve.data.db = base::difftime(Sys.time(), Set_date2POSIXct(Influx.TZ[["LastDate"]], tz = Influx.TZ[["Influx.TZ"]]), units = "min") > UserMins
            DownloadSensor$DateIN.db.prev   = Set_date2POSIXct(Influx.TZ[["FirstDate"]], tz = Influx.TZ[["Influx.TZ"]])
            DownloadSensor$DateEND.db.prev  = Set_date2POSIXct(Influx.TZ[["LastDate"]], tz = Influx.TZ[["Influx.TZ"]])}
        
        DownloadSensor$ExistFil.data.Influx = TRUE
        DownloadSensor$Retrieve.data.Influx = base::difftime(Sys.time(), Set_date2POSIXct(max(InfluxData$date, na.rm = T)), units = "min")  > UserMins
        DownloadSensor$DateIN.Influx.prev   = min(InfluxData$date, na.rm = T)
        DownloadSensor$DateEND.Influx.prev  = max(InfluxData$date, na.rm = T)
        cat("-----------------------------------------------------------------------------------\n")
        
        # Checking missing data
        if(Monitor){
            # Plotting and checking missing data
            Check.Data <- Data_reception(DT.Data = InfluxData, Last.Days = 2L, Max.hours.missing = 5L, Discarded.Variables = c("altitude", "latitude", "longitude"),
                                         In.Dir = WDoutput, Save = TRUE, Web.Service = Dataset)
            
            # Sending a mail if Missing data and during working time, using default emal and server
            if(as.numeric(format(Sys.time(), "%H")) %in% 9L:18L) Send_Mail(Check.Data)}
        
        return(list(InfluxData = InfluxData, 
                    var.names.meteo = var.names.meteo, var.names.Pollusens = var.names.Pollusens, var.names.sens = var.names.sens,
                    DownloadSensor = DownloadSensor))
    } else {
        cat("-----------------------------------------------------------------------------------\n")
        return(futile.logger::flog.error(paste0("[INFLUXDB] ",Dataset," no Influx data available")))
    }
}
#=====================================================================================CR
# 170721 MG SOS Downloading SOS data ####
#=====================================================================================CR
SOS      <- function(WDoutput, DownloadSensor, Down.SOS, AirSensEur.name, UserMins, AirsensWeb, Duration = 1, sens2ref) {
    # DownloadSensor   : output of function check_download
    # Parameters SOS: Down.SOS,AirSensEur.name,UserMins,AirsensWeb,Duration
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
                SOSData <- Down_SOS(AirSensEur.name = AirSensEur.name, UserMins = UserMins,
                                    DownloadSensor = DownloadSensor, AirsensWeb = AirsensWeb, Duration = Duration) # add DownloadSensor = DownloadSensor, to force the DateIN for download,
                # in case of error during download set Duration = 1 and then set vack Duration = 7, launch a Check_Download in between
                # Down_SOS returns the whole dataFrame (old and new data) and save SOSData.Rdata and SOSdata.csv in general_data
                # setting the name of sensors
                var.names.meteo <- c("Temperature","Relative_humidity",  "Atmospheric_pressure")
                if (exists("SOSData")) {
                    # List of Pollutant/sensor installed in the AirSensEUR
                    var.names.sens <- colnames(SOSData)[-which(colnames(SOSData) == "date")]
                    if (length(var.names.sens) == 0) {
                        stop(paste0("[SOS] ERROR: no sensor variable downloaded for ",AirSensEur.name," at the apiEndPoint. Please check in SOS client -> STOP"))
                    } else cat(paste0("[SOS] INFO: Sensor variables existing in the dataframe downloaded at the apiEndPoint: ", paste0(var.names.sens, collapse = ", "), ", plus date added"), sep = "\n")
                    # Setting the Sensor names
                    var.names.Pollusens      <- var.names.sens[-(which(var.names.sens %in% var.names.meteo))]
                } else {
                    # if we do not have new data for sensors we use the names of sensors in sens2ref
                    var.names.Pollusens <- na.omit(sens2ref$gas.sensor)
                    var.names.sens      <- c(var.names.Pollusens, var.names.meteo)
                }
                # Saving Sensor data - It is alredy saved, it is not needed to save again, just use Make.old
                # No need to save SOS.file and SOS.csv.file, as it is already done in Down_SOS
                cat(paste0("[SOS] INFO: SOS Sensor data are in ", DownloadSensor$SOS.file), sep = "\n")
            } else {
                # no request Down.SOS. Trying to use the existing SOS.file
                if (file.exists(file.path(DownloadSensor$SOS.file))) {
                    cat(paste0("[SOS] INFO: Down.SOS set to FALSE in ASEConfig_xx.R  (no request of sensor data download from SOS). Using previously saved file ."), sep = "\n")
                    if (file_ext(DownloadSensor$SOS.file) == "csv") {
                        RefData <- data.table::fread(file = DownloadSensor$SOS.file, na.strings = c("","NA", "<NA>"))
                    } else if (file_ext(DownloadSensor$SOS.file) == "Rdata") load(DownloadSensor$SOS.file)
                    var.names.meteo     <- c("Temperature","Relative_humidity",  "Atmospheric_pressure")
                    var.names.Pollusens <- na.omit(sens2ref$gas.sensor)
                    var.names.sens      <- c(var.names.Pollusens, var.names.meteo)
                } else {
                    cat(paste0("[SOS] INFO: Down.SOS set to FALSE in ASEConfig_xx.R  (no request of sensor data download from SOS). There is no previously saved file.",
                               "Missing SOSData and Down.SOS request of sensor data download set to FALSE in ASEConfig_xx.R .\n"))
                }
            }
        }
    } else {
        if (file.exists(file.path(DownloadSensor$SOS.file))) {
            if (file_ext(DownloadSensor$SOS.file) == "csv") {
                RefData <- data.table::fread(file = DownloadSensor$SOS.file, na.strings = c("","NA", "<NA>"))
            } else if (file_ext(DownloadSensor$SOS.file) == "Rdata") load(DownloadSensor$SOS.file)
            var.names.meteo     <- c("Temperature","Relative_humidity",  "Atmospheric_pressure")
            var.names.Pollusens <- na.omit(sens2ref$gas.sensor)
            var.names.sens      <- c(var.names.Pollusens, var.names.meteo)
        } else {
            cat(paste0("[SOS] INFO: sensor data download from SOS already updated (DownloadSensor$Retrieve.data.SOS set to FALSE)"), sep = "\n")
        }
    }
    cat("-----------------------------------------------------------------------------------\n")
    if (exists("SOSData")) {
        cat("[SOS] INFO SOS returning list with SOSData, var.names.meteo, var.names.Pollusens and var.names.sens\n")
        return(list(SOSData, var.names.meteo, var.names.Pollusens, var.names.sens))
    } else return(cat("[SOS] INFO no SOS data available\n"))
}
#=====================================================================================CR
# 170721 function REF Downloading REFERENCE data ####
#=====================================================================================CR
#' Downloading Reference data, Only new values, save RefData.RData and refData.csv with all reference values
#' This function will return and save a data.table with reference data with dates overlappingin the dates included in  InfluxData or SOSData.
#' The downloading of reference data depends on FTPMode. It can be from a local csv file, a csv file on a FTP site,
#' using the SOS protocol or at an aip server.
#' The Ref
#' @param PROXY Logical, default value FALSE. If TRUE PROXY is configured using the following 4 parameters:
#' @param PORT numeric, default value NULL, open Port for the proxy, jrc = 8012;
#' @param LOGIN character, default value = NULL, login for the proxy server, JRC = NULL;
#' @param PASSWORD character, default value = NULL, password for the proxy server, jrc = NULL;
#' @param DownloadSensor Output of function DownloadSensor()
#' @param AirSensEur.name char   - ID of AirSensEUR box
#' @param DisqueFieldtestDir file.path of the data of AirSensEUR box with sub-directories General_data, configuration, Models ..., e.g. "S:/Box Sync/AirSensEUR/Fieldtests/Shiny/ASE_Boxes/4047D0"
#' @param UserMins numeric. Periodicty of data to be downloaded
#' @param Down.ref logical, if true reference data are downloaded
#' @param FTPMode string, default = "ftp", type of download of reference data: "ftp" using a csv file on a ftp server, "csv" the same with a local file and SOS: SOS download
#' @param ref.tzone string, reference time name of the reference data. Default = "UTC"
#' @param InfluxData data.table of sensor data previously downloaded from InfluxDB server
#' @param SOSData data.table of sensor data previously downloaded from SOS server
#' @param Reference.name  character vector, Name of the Reference station
#' @param urlref vector of character vectors, used if FTPMode = "ftp". Vector of URIs linking to csv files with the reference data. Header with variable names as in ASEConfig.R
#' @param sens2ref data.table or dataframe: Configuration of sensors, output of function CONFIG.
#' @param Ref.SOS.name SOS ID of the Reference station
#' @param RefSOSname Reference station SOS Rest API URL
#' @param RefSOSPollutants Character vector, list of pollutants to download. Default is NULL. In this case pollutants are downloaded.
#' @param RefSOSDateIN Starting  date for downloading Reference data using SOS
#' @param RefSOSDateEND Ending date for downloading Reference data using SOS
#' @param Ref__a_i_p__name input$Ref__a_i_p__name,
#' @param User__a_i_p__ input$User__a_i_p__,
#' @param Pass__a_i_p__ input$Pass__a_i_p__,
#' @param Ref__a_i_p__Organisation input$Ref__a_i_p__Organisation,
#' @param Ref__a_i_p__Station input$Ref__a_i_p__Station,
#' @param Ref__a_i_p__Pollutants input$Ref__a_i_p__Pollutants,
#' @param Ref__a_i_p__DateIN as.Date(input$Ref__a_i_p__Date[1], format = "%Y-%m-%d"),
#' @param Ref__a_i_p__DateEND as.Date(input$Ref__a_i_p__Date[2], format = "%Y-%m-%d"),
#' @param avgtime Integer, possible values are 1, 10, 15 or 60, default is NULL, the averaging time of data in minute at the aip server
#' @param csvFile if FTPMode = "csv", file path to the csv file to load
#' @param csvFile.sep if FTPMode = "csv", separator between columns in the csvFile
#' @param csvFile.quote if FTPMode = "csv", separator of values in all columns, default is "\""
#' @param csvFile.DateIN Date or POSIXct, as.Date(input$Ref__a_i_p__Date[1], format = "%Y-%m-%d"), only data starting from this dateTime will be considered for insertion in RefData.csv
#' @param csvFile.DateEND Date or POSIXct, as.Date(input$Ref__a_i_p__Date[2], format = "%Y-%m-%d"), only data until this dateTime will be considered for insertion in RefData.csv
#' @param coord.ref string with coordinates of reference data longitude and latitude separated by a blank
#' @param Ref.Type label to be written in front of pollutatns names, default is Ref, other possibility Bin.APS, Bin.DMPS, FIDAS and GRIMM for PM distribution
#' @param RefData data.table or dataframe, default is null, reference dataset
#' @param shiny logical, default value is TRUE. If TRUE the function uses in a Shiny reactive context and shinyalert message can be returned.
REF <- function(PROXY = FALSE, URL = NULL, PORT = NULL, LOGIN = NULL, PASSWORD = NULL,
                DownloadSensor, AirSensEur.name, DisqueFieldtestDir, UserMins,
                Down.Ref, FTPMode, ref.tzone, InfluxData, SOSData, Reference.name, urlref, sens2ref,
                RefSOSname = NULL, Ref.SOS.name = NULL, RefSOSPollutants = NULL, RefSOSDateIN = NULL, RefSOSDateEND = NULL,
                Ref__a_i_p__name = NULL, User__a_i_p__ = NULL, Pass__a_i_p__ = NULL, Ref__a_i_p__Organisation = NULL,
                Ref__a_i_p__Station = NULL, Ref__a_i_p__Pollutants = NULL, Ref__a_i_p__DateIN = NULL, Ref__a_i_p__DateEND = NULL, avgtime = 1,
                csvFile = NULL, csvFile.sep = NULL, csvFile.quote = "\"", csvFile.DateIN = NULL, csvFile.DateEND = NULL,
                coord.ref = NULL, Ref.Type = "Ref", RefData = NULL, shiny = TRUE) {
    # Getting what is the first date in InfluxData and or SOSData and setting in DownloadSensor
    cat("-----------------------------------------------------------------------------------\n")
    futile.logger::flog.info("[REF] INFO: Reading or downloading Reference data, save refData.csv with all reference values in directory General_Data")
    if (shiny::isTruthy(InfluxData)) {
        minDateInflux <- min(InfluxData$date, na.rm = TRUE)
        maxDateInflux <- max(InfluxData$date, na.rm = TRUE)
        if (shiny::isTruthy(SOSData)) {
            minDateSOS    <- min(SOSData$date, na.rm = TRUE)
            maxDateSOS    <- max(SOSData$date, na.rm = TRUE)
            DownloadSensor$mindateRef <- min(c(minDateInflux,minDateSOS), na.rm = TRUE)
            DownloadSensor$maxdateRef <- max(c(maxDateInflux,maxDateSOS), na.rm = TRUE)
        } else {
            DownloadSensor$mindateRef <- minDateInflux
            DownloadSensor$maxdateRef <- maxDateInflux}
    } else {
        if (shiny::isTruthy(SOSData)) {
            DownloadSensor$mindateRef <- min(SOSData$date, na.rm = TRUE)
            DownloadSensor$maxdateRef <- max(SOSData$date, na.rm = TRUE)
        } else {
            DownloadSensor$mindateRef <- NULL
            DownloadSensor$maxdateRef <- NULL}}# The if statement should be added to DownloadSensor
    
    # Downloading reference data
    if (DownloadSensor$Retrieve.data.Ref) {
        if (Down.Ref) {
            cat("-----------------------------------------------------------------------------------\n")
            futile.logger::flog.info(paste0("[REF] INFO, Starting downloading data for ", Reference.name))
            RefDataNew  <- Down_Ref(PROXY = PROXY, URL = URL, PORT = PORT, LOGIN = LOGIN, PASSWORD = PASSWORD,
                                    Reference.name = Reference.name, UserMins = UserMins, DownloadSensor = DownloadSensor, urlref = urlref, ref.tzone = ref.tzone,
                                    naString = c("-999.99", "-999.98999", NaN, NA), WDoutput = file.path(DisqueFieldtestDir, "General_data"),
                                    FTPMode = FTPMode,
                                    RefSOSname = RefSOSname, Ref.SOS.name = Ref.SOS.name, RefSOSPollutants = RefSOSPollutants, RefSOSDateIN = csvFile.DateIN, RefSOSDateEND = csvFile.DateEND,
                                    Ref__a_i_p__name = Ref__a_i_p__name, User__a_i_p__ = User__a_i_p__, Pass__a_i_p__ = Pass__a_i_p__,
                                    Ref__a_i_p__Organisation = Ref__a_i_p__Organisation, Ref__a_i_p__Station = Ref__a_i_p__Station,
                                    Ref__a_i_p__Pollutants = Ref__a_i_p__Pollutants, Ref__a_i_p__DateIN = csvFile.DateIN, Ref__a_i_p__DateEND = csvFile.DateEND, avgtime = avgtime,
                                    csvFile = csvFile, csvFile.sep = csvFile.sep, csvFile.quote = csvFile.quote, csvFile.DateIN = csvFile.DateIN, csvFile.DateEND = csvFile.DateEND,
                                    coord.ref = base::trimws(x = coord.ref), Ref.Type = Ref.Type, shiny = shiny) # this return only new Data
        } else cat(paste0("[REF] INFO: Data download not requested."), sep = "\n")
    } else cat(paste0("[REF] INFO: Data download already up to date."), sep = "\n")
    
    # Trying to use the existing data or Influx.Rdata.file even though it was not passed to the function
    if (is.null(RefData) || !shiny::isTruthy(RefData)) {
        # loading the possible existing data in Refdata.csv
        if (file.exists(DownloadSensor$Ref.Rdata.file)) {
            if (file_ext(DownloadSensor$Ref.Rdata.file) == "csv") {
                RefData <- data.table::fread(file = DownloadSensor$Ref.Rdata.file, na.strings = c("","NA", "<NA>"))
                # removing rownames if any
                if ("V1" %in% names(RefData)) RefData[, V1 := NULL]
                # Making sure RefDataNew$date is a POSIXct
                if(!lubridate::is.POSIXct(RefDataNew$date)){
                    if (!is.null(Ref.TZ) && shiny::isTruthy(Ref.TZ)) {
                        data.table::set(InfluxData, j = "date", value =  lubridate::ymd_hms(InfluxData[["date"]], tz = Ref.TZ))
                    } else data.table::set(InfluxData, j = "date", value =  lubridate::ymd_hms(InfluxData[["date"]], tz = "UTC"))}
            } else if (file_ext(DownloadSensor$Ref.Rdata.file) == "Rdata") {
                RefData <- load_obj(DownloadSensor$Ref.Rdata.file)
                if (!data.table::is.data.table(RefData)) RefData <- data.table::data.table(RefData)}
            if (!haskey(RefData)) setkey(RefData, "date") # very slow, avoid
        } else futile.logger::flog.info(paste0("[REF] INFO: there is no previously saved Ref data."))}
    # merging RefData and RefDataNew if needed
    if (exists("RefDataNew") && shiny::isTruthy(RefDataNew)) {
        # # Checking if key is defined
        # data.table::haskey(RefDataNew)
        # Discarding dupicates, keeping last dates
        duplicated.Data <- which(duplicated(RefDataNew$date, fromLast = T))
        if (length(duplicated.Data) > 0) RefDataNew <- RefDataNew[-duplicated.Data]
        # Keep only the dates that are in Influx data
        Date.Ref.Influx <- which(RefDataNew$date %in% InfluxData$date)
        if (length(Date.Ref.Influx) > 0) {
            RefDataNew <- RefDataNew[Date.Ref.Influx]
            # Append new dates and update values for Common.pollutants
            if (exists("RefData") && shiny::isTruthy(RefData)) {
                # For Common dates, replace or the values of pollutants in RefDataNew in RefData except "dates" and "coordinates"
                RefDataNew.IN.RefData <- which(RefDataNew$date %in% RefData$date)
                RefData.IN.RefDataNew <- which(RefData$date %in% RefDataNew$date)
                if (length(RefDataNew.IN.RefData) > 0) {
                    # Pollutants to add
                    # Common.pollutants <- intersect(names(RefData), names(RefDataNew[RefDataNew.IN.RefData]))
                    # Common.pollutants <- Common.pollutants[-which(Common.pollutants %in% c("date","Ref.Long","Ref.Lat"))]
                    Pollutants.Add <- names(RefDataNew[RefDataNew.IN.RefData])
                    Pollutants.Add <- Pollutants.Add[-which(Pollutants.Add %in% c("date","Ref.Long","Ref.Lat"))]
                    if (length(Pollutants.Add) > 0) {
                        data.table::set(RefData, i = RefData.IN.RefDataNew, j = Pollutants.Add, 
                                        value = RefDataNew[RefDataNew.IN.RefData, Pollutants.Add, with = FALSE])}
                    # Setting coordinates for Common dates
                    Coord.RefData        <-all(c("Ref.Long","Ref.Lat") %in% names(RefData[RefData.IN.RefDataNew]))
                    Coord.RefDataNew     <-all(c("Ref.Long","Ref.Lat") %in% names(RefDataNew[RefDataNew.IN.RefData]))
                    # if Coord.RefDataNew exists for the complete case of Ref.Long and ref.lag set Ref.Long and Ref.Lat of refData to the values of RefDataNew otherwise keep coordinates in RefData. If both do not exist set to NA
                    if ((Coord.RefDataNew & Coord.RefData) || (Coord.RefDataNew & !Coord.RefData) && any(is.finite(rowSums(RefDataNew[RefDataNew.IN.RefData,c("Ref.Long","Ref.Lat")])))) {
                        RefData.IN.RefDataNew <- which(RefData$date %in% RefDataNew[is.finite(rowSums(RefDataNew[, c("Ref.Long", "Ref.Lat")])), date])
                        RefDataNew.IN.RefData <- which(RefDataNew$date %in% RefData$date & RefDataNew$date %in% RefDataNew[is.finite(rowSums(RefDataNew[, c("Ref.Long", "Ref.Lat")]))]$date)
                        data.table::set(RefData, i = RefData.IN.RefDataNew, j = "Ref.Long", value = RefDataNew[RefDataNew.IN.RefData, Ref.Long])
                        data.table::set(RefData, i = RefData.IN.RefDataNew, j = "Ref.Lat",  value = RefDataNew[RefDataNew.IN.RefData, Ref.Lat])
                    } else if (!Coord.RefDataNew & Coord.RefData) {
                        # nothing to do let coordinates of RefData
                    } else if (!Coord.RefDataNew & !Coord.RefData) {
                        data.table::set(RefData, i = RefData.IN.RefDataNew, j = "Ref.Long", value = rep(NA, nrow(RefData)))
                        data.table::set(RefData, i = RefData.IN.RefDataNew, j = "Ref.Lat",  value = rep(NA, nrow(RefData)))}}
                # # Add new columns for existing dates
                # New.pollutants <-  names(RefDataNew[RefDataNew.IN.RefData])[!names(RefDataNew[RefDataNew.IN.RefData]) %in% names(RefData)]
                # if (length(New.pollutants) > 0) {
                #     # https://stackoverflow.com/questions/34834257/r-programming-merge-function-returns-column-names-with-x-and-y
                #     # replacing coordinates with new values and adding new pollutants
                #     Coord.RefData        <-all(c("Ref.Long","Ref.Lat") %in% names(RefData))
                #     Coord.RefDataNew     <-all(c("Ref.Long","Ref.Lat") %in% names(RefDataNew))
                #     if(Coord.RefDataNew & Coord.RefData){
                #         names.RefData <- names(RefData)[-grep(paste(c("Ref.Long","Ref.Lat"), collapse = "|"), names(RefData))]
                #     } else names.RefData <- names(RefData)
                #     Coord.New.pollutants <-all(c("Ref.Long","Ref.Lat") %in% New.pollutants)
                #     if(Coord.New.pollutants) {
                #         names.RefDataNew <- c("date", New.pollutants)
                #     } else names.RefDataNew <- c("date","Ref.Long","Ref.Lat", New.pollutants)
                #     RefData <- merge(x = RefData[, ..names.RefData], 
                #                      y = RefDataNew[RefDataNew.IN.RefData, ..names.RefDataNew], 
                #                      by = "date", all = TRUE)}
                # for New dates, append dates coordinates and new pollutants
                # rbindlist returns a data.table
                New.Dates <- which(!RefDataNew$date %in% RefData$date)
                if (length(New.Dates) > 0) {
                    RefData <- data.table::rbindlist(list(RefData, RefDataNew[New.Dates]), use.names = TRUE, fill = TRUE)}
                rm(RefDataNew)
            } else {
                RefData <- RefDataNew
                if (!data.table::is.data.table(RefData)) RefData <- data.table::data.table(RefData)
                rm(RefDataNew)}
            if (!haskey(RefData)) setkey(RefData, Key = "date")
            if (tools::file_ext(DownloadSensor$Ref.Rdata.file) == "csv") {
                duplicated.Data <- which(duplicated(RefData$date, fromLast = T))
                if (length(duplicated.Data) > 0) RefData <- RefData[-duplicated.Data]
                # making sure RefData$date is a POSIXct
                data.table::set(RefData, j = "date", value = Set_date2POSIXct(RefData$date, tz = ref.tzone))
                # format of date when saving: if dateTimeAS is default (nothing) then date is 2020-09-11T09:13:00Z (preferred)) saving is fast,
                # if dateTimeAs = "ISO" the numeric POSIX is saved (e.g. 1694736000)  saving is fast and if dateTimeAs = "write.csv" then character date is saved e.g. "2023-09-15 00:00:00" ma 10 volte piu lento
                data.table::fwrite(RefData, file = DownloadSensor$Ref.Rdata.file, na = "NA", dateTimeAs = "write.csv") #, dateTimeAs = "ISO") #, dateTimeAs = "write.csv"
            } else if (tools::file_ext(DownloadSensor$Ref.Rdata.file) == "Rdata") save(RefData, file = DownloadSensor$Ref.Rdata.file)
            futile.logger::flog.info(paste0("[REF] reference data saved in ", DownloadSensor$Ref.Rdata.file))
        }
    } else futile.logger::flog.warn(paste0("[REF] WARNING, There is no new reference data for ",Reference.name))
    
    # Preparing for returning data, setting var.names.ref
    if (exists("RefData") && shiny::isTruthy(RefData)) {
        if (!identical(colnames(RefData)[-which(colnames(RefData) %in% c("date", "Ref.Long", "Ref.Lat"))],character(0))) {
            # List of Pollutants monitored at the Referencce stations
            var.names.ref <- colnames(RefData)[-which(colnames(RefData) %in% c("date", "Ref.Long", "Ref.Lat"))]
            if (length(var.names.ref) == 0) {
                futile.logger::flog.warn(paste0("[REF] no reference data exisiting for ", AirSensEur.name," .Please check data at ", Reference.name))
            } else futile.logger::flog.info(paste0("[REF] INFO, Variables found in the reference dataset: ", paste0(var.names.ref, collapse = ", ")))
        } else {
            # if we do not have new data for sensors we use the names of sensors in sens2ref
            var.names.ref <- na.omit(sens2ref$gas.reference2use)}}
    
    # Returning data, updating DownloadSensor
    if (exists("RefData") & exists("var.names.ref") & exists("DownloadSensor")) {
        futile.logger::flog.info("[REF] returning list with RefData, var.names.ref and DownloadSensor")
        cat("-----------------------------------------------------------------------------------\n")
        # Discarding any NA if RefData$date as NA prevent from mergeing with InfluxData
        if(any(is.na(RefData$date))){
            rows2Delete <- which(is.na(RefData$date))
            RefData <- RefData[-rows2Delete]}
        
        # Updating in order not to have to use Check_Download which is rather long
        DownloadSensor$ExistFil.data.Ref = TRUE
        DownloadSensor$Retrieve.data.Ref = base::difftime(Sys.time(), Set_date2POSIXct(max(RefData$date, na.rm = T)), units = "min") > UserMins * 60
        DownloadSensor$DateIN.Ref.prev   = min(RefData$date, na.rm = T)
        DownloadSensor$DateEND.Ref.prev  = max(RefData$date, na.rm = T)
        DownloadSensor$Var.Ref.prev      = var.names.ref
        return(list(RefData = RefData, var.names.ref = var.names.ref, DownloadSensor = DownloadSensor))
    }  else {
        futile.logger::flog.warn("[REF] no Rerence data available")
        cat("-----------------------------------------------------------------------------------\n")
        return(futile.logger::flog.warn("[REF] ERROR no Rerence data available"))}
}
#=====================================================================================CR
# 170721 MG GENERAL Merging InfluxData or SOSData and RefData ####
#=====================================================================================CR
# https://stackoverflow.com/questions/5577221/how-can-i-load-an-object-into-a-variable-name-that-i-specify-from-an-r-data-file
load_obj <- function(file_path) {
    # returning a loaded R object
    env <- new.env()
    nm <- load(file = file_path, envir = env, verbose = FALSE)[1]
    env[[nm]]}
#' Merging SOSData or InfluxData and RefData into Genera.DT
#'
#' @param WDoutput character vector. Optional. File.path for saving General.csv. If null the directory is fond in DownloadSensor
#' @param UserMins numeric. Periodicty of data to be downloaded
#' @param RefData data.table of reference data
#' @param InfluxData data.table of sensor data previously downloaded from InfluxDB server
#' @param SOSData data.table of sensor data previously downloaded from SOS server
#' @param Delay numeric
#' @param var.names.Pollusens list of gas sensors. No more used
#' @param DownloadSensor list output of Check_Download, use to know if DateEND.General.prev is before last date in INfluxData or SOSData. Can be used to know ASEDir
#' @param Change.Delay logical, default FALSE. If TRUE Delay has been changed and General shall be created new
#' @param Change.UserMins logical, default FALSE. If TRUE UserMins has been changed and General shall be created new
#' @param shiny logical, default FALSE. No more used
#' @return  the data.table General, adding new data to the existing one and averaging every UserMins minutes
#' @examples
GENERAL <- function(WDoutput = NULL, UserMins, RefData, InfluxData, SOSData, Delay, var.names.Pollusens = NULL, 
                    DownloadSensor, Change.Delay = FALSE, Change.UserMins = FALSE, shiny = FALSE) {
    #------------------------------------------------------------------------------CR
    # Merging InfluxData or SOSData and RefData
    #------------------------------------------------------------------------------CR
    # using preferably the latest date of the previous general dataset from influxdb, otherwise SOS data
    cat("-----------------------------------------------------------------------------------\n")
    futile.logger::flog.info("[GENERAL] Checking if there are more data in InfluxData or SOSData than in General.Rdata")
    # Checking if there are sensor data to be added to General
    # if General does not exist it must be created in all cases. The same if the Delay has changed
    is.sensorData <- FALSE
    if (is.null(DownloadSensor$DateEND.General.prev) || Change.Delay || Change.UserMins) {
        is.sensorData <- TRUE
    } else {
        # Checking last date in InfluxData
        if (exists("InfluxData") && !is.null(InfluxData)) {
            is.sensorData <- !all(is.na(InfluxData[which(InfluxData$date > DownloadSensor$DateEND.General.prev),-which(colnames(InfluxData) == "date")]))}
        # Checking last date in SOSData
        if (exists("SOSData") && !is.null(SOSData)) {
            # Max.Sensor.date <- max(SOSData[all(is.na(SOSData[SOSData$date > DownloadSensor$DateEND.General.prev,-which(colnames(SOSData) == "date")])),"date"], na.rm = T)
            if (exists("is.sensorData")) {
                is.sensorData <- is.sensorData || !all(is.na(SOSData[which(SOSData$date > DownloadSensor$DateEND.General.prev),-which(colnames(SOSData) == "date")]))
            } else is.sensorData <- !all(is.na(SOSData[which(SOSData$date > DownloadSensor$DateEND.General.prev),-which(colnames(SOSData) == "date")]))}
        # Checking last date in RefData
        if (exists("RefData") && shiny::isTruthy(RefData)) {
            # Max.Sensor.date <- max(RefData[all(is.na(RefData[RefData$date > DownloadSensor$DateEND.General.prev,-which(colnames(RefData) == "date")])),"date"], na.rm = T)
            # Date to be added from refData
            row.New.Ref.date <- which(RefData$date > DownloadSensor$DateEND.General.prev)
            if (length(row.New.Ref.date) > 0 && any(RefData[row.New.Ref.date, "date"] %in% InfluxData$date)) {
                if (exists("is.sensorData")) {
                    is.sensorData <- is.sensorData || !all(is.na(RefData[row.New.Ref.date,-which(colnames(RefData) == "date")]))
                } else is.sensorData <- !all(is.na(RefData[row.New.Ref.date,-which(colnames(RefData) == "date")]))}}}
    
    if (is.sensorData) {
        futile.logger::flog.info("[GENERAL] Merging InfluxData or SOSData with RefData ")
        if (exists("RefData") && shiny::isTruthy(RefData)) {
            if (exists("InfluxData") && !is.null(InfluxData)) {
                # Fine adjusting of InfluxData$date due to delays between sensor and reference data - The Delay should be the same for all sensors, this makes sense
                #  Adding field "date_PreDelay" in Influx and SOS data that will be saved into General but not in Influx and SOS.Rdata
                InfluxData$date_PreDelay <- InfluxData$date
                InfluxData$date          <- InfluxData$date + Delay * 60
                # Trying to rbind.fill InfluxData and SOSdata
                # we prefer InfluxData data over SOSData if they exist for the boardTImeStamp and gpsTimeStamp
                if (exists("SOSData") && shiny::isTruthy(SOSData)) { # RefData, InfluxData and SOSData exists
                    # Fine adjusting of InfluxData$date due to delays between sensor and reference data - The Delay should be the same for all sensors, this makes sense
                    # Adding field "date_PreDelay" in Influx and SOS data that will be saved into General but not in Influx and SOS.Rdata
                    SOSData$date_PreDelay <- SOSData$date
                    SOSData$date          <- SOSData$date + Delay * 60
                    # if the union of SOSData and InfluxData gives additional data
                    index.Date.SOSData <- which(SOSData$date %in% InfluxData$date)
                    General            <- merge(x = rbind.fill(SOSData[!(row.names(SOSData) %in% index.Date.SOSData),], InfluxData), y = RefData, by = "date", all.x = TRUE )
                    # Discarding Reference dates when sensor dates do no exist
                    General            <- General[General$date >= min(min(InfluxData$date),min(SOSData$date)) & General$date <= max(max(InfluxData$date),max(SOSData$date)),]
                } else {
                    # RefData exists, InfluxData present but no SOSData
                    # In case of names with "_raw", the digital values in raw form are not saved in Genera.data, they are only kept in the airsenseur.db if Down_Influx is used
                    if (any(grepl(pattern = "_raw", x = colnames(InfluxData)))) {
                        sensor    <- data.table::data.table(InfluxData[-grep(pattern = "_raw", x = colnames(InfluxData)),], key = "date")
                    } else {
                        sensor    <- data.table::data.table(InfluxData, key = "date")}
                    if (any(duplicated(sensor$date)))    sensor <- sensor[-which(duplicated(sensor$date, fromLast = T))]
                    reference <- data.table::data.table(RefData, key = "date")
                    if (any(duplicated(reference$date))) reference <- reference[-which(duplicated(reference$date, fromLast = T))]
                    General   <- merge(sensor, reference, by = "date", all.x = TRUE)
                    rm(sensor, reference)
                    # Keeping only data with values in InfluxDB
                    General <- General[General$date >= min(InfluxData$date, na.rm = TRUE) & General$date <= max(InfluxData$date, na.rm = TRUE),]}
                futile.logger::flog.info("[GENERAL] General data table was created with sensor data and reference data ")
            } else {
                # RefData exists but no Influx Data
                if (exists("SOSData") && shiny::isTruthy(SOSData)) { # RefData and SOSData exist but no Influx Data
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
                        General <- merge(x = rbind.fill(SOSData, InfluxData)[rbind.fill(InfluxData,SOSData)$date >= as.POSIXct("2016-09-09",tz = "UTC"),],
                                         y = RefData[RefData$date >= as.POSIXct("2016-09-09",tz = "UTC"),], by = "date", all.x = TRUE )}
                } else {
                    # RefData exists but no InfluxData and no SOSData
                    General <- RefData
                    futile.logger::flog.error("[GENERAL] General data table was created without sensor data and only with reference data. The App is likely going to crash.")}}
        } else {
            # RefData does not exist
            if (exists("InfluxData") && shiny::isTruthy(InfluxData)) { # RefData does not exist but InfluxData exists
                # Fine adjusting of InfluxData$date due to delays between sensor and reference data - The Delay should be the same for all sensors, this makes sense
                #  Adding field "date_PreDelay" in Influx and SOS data that will be saved into General but not in Influx and SOS.Rdata
                InfluxData$date_PreDelay <- InfluxData$date
                InfluxData$date          <- InfluxData$date + Delay * 60
                General <- InfluxData
                futile.logger::flog.info("[GENERAL] General datatable was created with sensor data and without reference data. It is impossible to calibrate the sensors.")
            } else {
                # RefData and InfluxData do not exist
                if (exists("SOSData") && shiny::isTruthy(SOSData)) { # RefData and InfluxData do not exist but SOSData exists
                    # Fine adjusting of InfluxData$date due to delays between sensor and reference data - The Delay should be the same for all sensors, this makes sense
                    # Adding field "date_PreDelay" in Influx and SOS data that will be saved into General but not in Influx and SOS.Rdata
                    SOSData$date_PreDelay <- SOSData$date
                    SOSData$date          <- SOSData$date + Delay * 60
                    General     <- SOSData
                } else { # RefData, InfluxData and SOSData do not exist
                    General <- NA
                    futile.logger::flog.error("[GENERAL] General cannot be created, there are no sensor and reference data. General is NA for now. The App is likely going to crash.")}}}
        # discarding rows with all NAs and NaNs for All gas sensors
        if (shiny::isTruthy(General)) {
            futile.logger::flog.info("[GENERAL] Discarding rows with NA and NaN for all gas sensors")
            # replacing NaN with NA
            futile.logger::flog.info("[GENERAL] replacing sensors values which are not numbers (NaN) with NA for all sensors and reference data.")
            General[which(names(General) != "date")] <- purrr::map_dfc(General[which(names(General) != "date")], function(i) nan.to.na(i) )}
    } else {
        # Selecting General.Rdata if it exists
        if (file.exists(DownloadSensor$General.Rdata.file)) {
            General <- load_obj(DownloadSensor$General.Rdata.file)
        } else {
            cat("-----------------------------------------------------------------------------------\n")
            return(futile.logger::flog.error("[GENERAL] no General data already saved and no data added."))}}
    # Averaging using UserMins
    General <- DF_avg(General, width = UserMins)
    if (shiny::isTruthy(General)) {
        
        # Identify which also filter ind.warm then ind.TRH, Ind.Inv, sensor outliers, Sens.Conv + reference outliers and Apply.Cal
        if (!is.null(WDoutput)) ASEDir <- dirname(WDoutput) else {
            stopifnot(exists("DownloadSensor"))
            ASEDir <- dirname(DownloadSensor$WDinput)}
        General <- Identify_ASE_Dir(ASEDir = ASEDir, General.DT = General)$General.DT
        
        # Adding Absolute Humidity and T_Deficit
        if (!"Absolute_humidity" %in% names(General) && all(c("Out.Temperature", "Out.Relative_humidity") %in% names(General))){
            General <- Add_AbsHum_Tdef(General = General, name.Temp = "Out.Temperature", name.RH    = "Out.Relative_humidity",
                                       name.AbsHR = "Absolute_humidity", name.Tdef = "Td_deficit")}
        if (!"Absolute_humidity_int" %in% names(General) && all(c("Out.Temperature_int", "Out.Relative_humidity_int") %in% names(General))){
            General <- Add_AbsHum_Tdef(General = General, name.Temp = "Out.Temperature_int", name.RH = "Out.Relative_humidity_int",
                                       name.AbsHR = "Absolute_humidity_int", name.Tdef = "Td_deficit_int")}
        if (!"Absolute_humidity_modelled" %in%names(General) && all(c("Temperature_modelled", "Temperature_modelled") %in% names(General))){
            General <- Add_AbsHum_Tdef(General = General, name.Temp = "Temperature_modelled", name.RH = "Relative_humidity_modelled",
                                       name.AbsHR = "Absolute_humidity_modelled", name.Tdef = "Td_deficit_modelled")}
        if (!"Ref.Absolute_humidity" %in% names(General) && all(c("Out.Ref.Temp", "Out.Ref.RH") %in% names(General))){
            General <- Add_AbsHum_Tdef(General = General, name.Temp = "Out.Ref.Temp", name.RH = "Out.Ref.RH",
                                       name.AbsHR = "Ref.Absolute_humidity", name.Tdef = "Ref.Td_deficit")}
        
        # returning General
        cat("-----------------------------------------------------------------------------------\n")
        futile.logger::flog.info("[GENERAL] Returning General data.table")
        return(General)
    } else{
        cat("-----------------------------------------------------------------------------------\n")
        return(futile.logger::flog.error("[GENERAL] no data available"))
    }
}
#=====================================================================================CR
# Etalonnage: ScatterPlot of calibration function (Vs 170420) ####
#=====================================================================================CR
#' 
#' @description This function plot a typical XY calibration graph, estimate Xlim and ylim, add x and y labels and gridlines
#' @param x, numerical vector, x-axis values
#' @param s_x, numerical vector, standard deviation of x values. Same lenght as x
#' @param y, numerical vector, y-axis values
#' @param s_y, numerical vector, standard deviation of y values. Same lenght as x is mandatory.
#' @param AxisLabelX character vector legend of x-axis
#' @param AxisLabelY character vector legend of y-axis
#' @param Title  character vector, default is NULL. If not NULL, add main tile to the plot
#' @param Marker numeric value, default is 19. Type marker
#' @param Couleur: character vector, default is "blue". Color of the marker
#' @param ligne character, default is "p". Used in plot as type = 'ligne' to have dots or lines or both (l", "p", "b" ...
#' @param XY_same logical, default is TRUE. If TRUE: X and Y have the same extent, IF False: Xlim ranges between min(x) and max(x) and ylim ranges between min(y) and max(y)
#' @param lim numerical vector, default is NULL. if NULL Xlim and Ylim are calculated otherwise lim is used. lim must be a matrix of xlim and ylim in column vector
#' @param steps numerical vector, default is c(10,10) as c(stepX, stepY). Number of numerical label on x an y axis, if NULL, default = 10 is used for stepX and stepY.
#' @param digitround numerical value, default is NULL. Number of digit for x and y axis labels (c(digitX,digitY)), should be c(0,0) for one digit
#' @param marges margin of graph, default is c(4,4,3,0.5))
#' @param PlotAxis character, default is null. IF "n" to disable the plot of the axis. If NULL the axis will be plot
#' @param Verbose logical, default TRUE. If TRUE print some messages are sent to the console
#' @param Ggplot logical, default FALSE. If TRUE a ggplot is created otherwise a base plot is created
#' @param Sites optional, character vector used for colors of dots, default is NULL. If not null, the Ggplot will use the vector for coloring dots. Same length as x and y
#' @return (cbind(Xlim,Ylim, par("usr")[1:2], par("usr")[3:4])) # par("usr") gives the true chosen xlim and ylim to which 4% of range is added in both sides. Used for arrows
Etalonnage <- function(x, s_x = NULL, y, s_y = NULL, AxisLabelX, AxisLabelY, Title = NULL, Marker =19, Couleur = "blue", ligne = "p", XY_same = TRUE,
                       lim = NULL, steps = c(10,10), digitround = NULL, marges = c(4,4,3,0.5), PlotAxis = NULL, Verbose = TRUE, Ggplot = FALSE, Sites = NULL) {
    # checking that not all data are NA
    if (!all(is.na(x)) & !all(is.na(y))) {
        # saving the original par values in case they would be modified in this function
        op <- par(no.readonly = TRUE)
        # Passing and resuming the par values
        on.exit(par(op))
        
        # settings the margins
        par(mar = marges)
        
        # Creating the DataXY data frame
        if (is.null(s_y)  || any(s_y == 0) || all(is.na(s_y))) {
            DataXY <- data.frame(cbind(x, y),stringsAsFactors = FALSE)
            colnames(DataXY) <- c("x", "y")
            if(!is.null(Sites)){
                if(length(Sites) == length(x)){
                    DataXY$Colour <- Sites
                } else futile.logger::flog.warn("Cannot used the vector Sites to colour the dots because its length is different from vector x")}
            DataXY <- subset(DataXY, !is.na(DataXY$x) & !is.na(DataXY$y))
        } else {
            DataXY <- data.frame(cbind(x, y, s_y),stringsAsFactors = FALSE)
            colnames(DataXY) <- c("x","y","s_y")
            if(!is.null(sites)){
                if(length(Sites) == length(x)){
                    DataXY$Colour <- Sites
                } else futile.logger::flog.warn("Cannot used the vector Sites to colour the dots because its length is different from vector x")}
            DataXY <- subset(DataXY, !is.na(DataXY$x) & !is.na(DataXY$y) & !is.na(DataXY$s_y))
        }
        # Automatic estimation of digitround
        if (is.null(digitround)) {
            Int <- c("x","y")
            Range <- sapply(DataXY[,Int], range, na.rm = TRUE, finite = TRUE)[2,] - sapply(DataXY[,Int], range, na.rm = TRUE, finite = TRUE)[1,]
            if (Verbose) futile.logger::flog.info("Range of values on x and y axis")
            if (Verbose) print(Range, quote = FALSE)
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
        if(Ggplot){
            
            # plotting the scatterplot with ggplot
            if(is.null(Sites)){
                Ggplot <- ggplot2::ggplot(data = DataXY, ggplot2::aes(x = x, y = y)) +
                    ggplot2::geom_point(color='blue')+
                    scale_color_manual(values = Couleur)
            } else {
                Ggplot <- ggplot2::ggplot(data = DataXY, ggplot2::aes(x = x, y = y, color = Colour)) +
                    ggplot2::geom_point()
                # +
                #     scale_color_manual(values = Couleur)
                }
            Ggplot <- Ggplot +
                # Force axis labels
                scale_x_continuous(breaks = pretty(x,stepsX)) +
                scale_y_continuous(breaks = pretty(y,stepsY)) +
                # delete minor axis grid lines
                theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) +
                labs(x= AxisLabelX, y = AxisLabelY) +
                # https://stackoverflow.com/questions/3606697/how-to-set-limits-for-axes-in-ggplot2-r-plots
                coord_cartesian(xlim = Xlim, ylim = Ylim) +
                #scale_color_manual(values = Couleur) +
                theme_bw() +
                theme(plot.margin = margin(#t = 0,  # Top margin
                    r = marges[4],                  # Right margin
                    b = 0,                          # Bottom margin
                    l = 0,                          # Left margin
                    unit = "line"))
            
            # adding error bars to ggplot()
            if (!is.null(s_y) && all(s_y != 0)) {
                # hack: we draw arrows but with flat "arrowheads"
                Ggplot <- Ggplot +
                    geom_errorbar(ggplot2::aes(ymin = y - s_y,ymax = y + s_y), width = 1, size = 0.5, color = Couleur, position=position_dodge(.9))}
            
            # adding title to ggplot()
            if (!is.null(Title)){
                Ggplot <- Ggplot +
                    ggtitle(Title) +
                    theme(plot.title = element_text(face = "bold", hjust=0.5))}
        } else {
            plot( DataXY$x, DataXY$y
                  ,xlab= AxisLabelX
                  ,ylab= AxisLabelY
                  ,xlim = Xlim
                  ,ylim = Ylim
                  ,col = Couleur
                  ,type = ligne
                  ,pch = Marker
                  ,xaxt = "n"
                  ,yaxt = "n", cex.lab=1.5)
            axis(side = 1, at = pretty(x,stepsX), cex.axis=1.5)
            axis(side = 2, at = pretty(y,stepsY), cex.axis=1.5)
            abline(h=pretty(y,stepsY), v =pretty(x,stepsX), lty = 2, col = "grey")
            
            # adding error bars to plot
            if (!is.null(s_y) && all(s_y != 0)) {
                # hack: we draw arrows but with flat "arrowheads"
                arrows(DataXY$x, DataXY$y - DataXY$s_y , DataXY$x, DataXY$y + DataXY$s_y, length=0.05, angle=90, code=3)}
            
            # adding title to plot()
            if (!is.null(Title))title (main = Title, outer = TRUE, line = -1, cex.main=2)
        }
        
        # Saving par variables before resuming for adding Cal_Line in Base plot
        Xusr = par("usr")[1:2]
        Yusr = par("usr")[3:4]
        mar12 = par("mar")[1:2]
        mar34 = par("mar")[3:4]
        # Passing and resuming the par values
        on.exit(par(op))
        # par("usr") gives the true chosen xlim and ylim to which 4% of range is added in both sides. Used for arrows
        return(list(Limits = cbind(Xlim,Ylim, Xusr, Yusr, mar12, mar34), Ggplot = Ggplot))
    } else {
        # all data re NA
        plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
        text(1,1,paste0("[Etalonnage], ERROR, all x or all y data are NA"))
    }
}
#=====================================================================================CR
# CONFIG Read Config files of ASE Boxes ####
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
#' @param DisqueFieldtestDir file.path of the data of AirSensEUR box with sub-directories General_data, configuration, Models ..., e.g. "S:/Box Sync/AirSensEUR/Fieldtests/Shiny/ASE_Boxes/4047D0"
#' @param DisqueFieldtest directory where is the file ASEconfig*.R file, which is not used anymore. It is now used because it it the directory of the Shiny App with directory Shield_files
#' @param sens2ref.shield    : dataframe, default is NULL, dataframe returned by function ASEPanel04Read giving the configuration of the chemical shield
#' @param shiny logical, default value is TRUE. If TRUE the function uses in a Shiny reactive context and shinyalert message can be returned.
#' @param Dir.Config character string, default value is "Configuration". Sub directory of DisqueFieldtestDir that includes the config fles (*.cfg)
#' @param Names.Influx, Names.ref: character vector, names of columns in data.tables Influx$DATA and Ref$DATA, default is null
#' @return a list with the config of servers, sensors and effects ...
CONFIG <- function(DisqueFieldtestDir, DisqueFieldtest , sens2ref.shield = NULL, shiny = TRUE, Dir.Config = "Configuration", 
                   Names.Influx = NULL, Names.ref = NULL, Verbose = TRUE) {
    
    if (Verbose) cat("-----------------------------------------------------------------------------------\n")
    #=====================================================================================CR
    #  sensors.cfg and board.cfg----
    #=====================================================================================CR
    sensor_cfg <- file.path(DisqueFieldtestDir, Dir.Config, paste0("Sensors.cfg"))
    ASE.name <- basename(DisqueFieldtestDir)
    if (file.exists(sensor_cfg)) {
        # reading the Server configuration files
        Sensors.cfg <- data.table::fread(file = sensor_cfg, header = TRUE, na.strings=c("","NA", "<NA>"))
        if (Verbose) futile.logger::flog.info(paste0("[CONFIG] ", ASE.name," ", sensor_cfg, " exists."))
        # Converting to correct format
        data.table::set(Sensors.cfg, j = "time"   , value = lubridate::ymd_hms(Sensors.cfg$time, tz = "UTC"))
        data.table::set(Sensors.cfg, j = "enabled", value = as.logical(Sensors.cfg$enabled))}
    board_cfg <- file.path(DisqueFieldtestDir, Dir.Config, paste0("Boards.cfg"))
    if (file.exists(board_cfg)) {
        # reading the Server configuration files
        board.cfg <- data.table::fread(file = board_cfg, header = TRUE, na.strings=c("","NA", "<NA>"))
        if (Verbose) futile.logger::flog.info(paste0("[CONFIG] ", ASE.name," ", board_cfg, " exists."))
        # Converting to correct format
        data.table::set(board.cfg, j = "time"   , value = lubridate::ymd_hms(board.cfg$time, tz = "UTC"))}
    #=====================================================================================CR
    #  ASE.name_Servers.cfg" ----
    #=====================================================================================CR
    # Read config file (TRUE)
    File_Server_cfg <- file.path(DisqueFieldtestDir, Dir.Config, paste0(ASE.name,"_Servers.cfg"))
    if (file.exists(File_Server_cfg)) {
        # reading the Server configuration files
        cfg <- data.table::transpose(data.table::fread(file = File_Server_cfg, header = FALSE, na.strings=c("","NA", "<NA>")), fill = NA, make.names = 1)
        if (Verbose) futile.logger::flog.info(paste0("[CONFIG] ", ASE.name," ", File_Server_cfg, " for the configuration of servers exists"))
        # Changing names
        if ("AirsensEur.name" %in% names(cfg)) names(cfg)[which(names(cfg) == "AirsensEur.name")] <- "AirSensEur.name"
        # Creating csvFile if it does not esist (before we used urlRef)
        if(cfg$FTPMode == "csv"){
            if(!"csvFile" %in% names(cfg)){
                if(shiny::isTruthy(cfg$urlref) && file.exists(cfg$urlref)){
                    cfg$csvFile <- cfg$urlref
                    futile.logger::flog.info(paste0("[CONFIG] using urlref for csvFile, ", cfg$csvFile))
                } else if (Verbose) futile.logger::flog.warn("[CONFIG] csv file selected for reference data but file.path is missing")}
            if(!"csvFile.sep" %in% names(cfg)) cfg$csvFile.sep <- ","
            if(!"csvFile.quote" %in% names(cfg)) cfg$csvFile.quote <- ""}
        
        # Creating UserMinsAvg if it does not exist
        if (!"UserMinsAvg" %in% names(cfg)) cfg$UserMinsAvg <- cfg$UserMins
        # Converting to correct format
        Vector.type <- c("PROXY", "Down.Influx", "Down.SOS", "Down.Ref")
        Vector.type <- Vector.type[Vector.type %in% names(cfg)]
        for (j in Vector.type) data.table::set(cfg, j = j, value = as.logical(cfg[[j]]))
        Vector.type <- c("PORT", "Port", "UserMins", "UserMinsAvg", "Delay")
        Vector.type <- Vector.type[Vector.type %in% names(cfg)]
        for (j in Vector.type) data.table::set(cfg, j = j, value = as.numeric(cfg[[j]]))
        if(!"Sensor.DateIN" %in% names(cfg)) cfg[, Sensor.DateIN := "2015-01-01"]
    } else { # if File_Server_cfg does not exist, Message of error
        my_message <- paste0("[CONFIG] ERROR, no server config file for the AirSensEUR box. \n",
                             "The App is going to crash. This AirSensEUR cannot be selected.\n")
        cat(my_message)
        if (shiny) shinyalert(
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
    #=====================================================================================CR
    #  ASE.name.cfg" ----
    #=====================================================================================CR
    # This is to insert both sensors and reference configuration into a dataframe and file
    File_cfg <- list.files(path = file.path(DisqueFieldtestDir,Dir.Config), pattern = paste0(ASE.name,".cfg"))
    if (length(File_cfg) > 0) {
        # reading the configuration files sens2ref
        File_cfg <- file.path(DisqueFieldtestDir,Dir.Config, paste0(ASE.name,".cfg"))
        if (file.exists(File_cfg)) {
            if (Verbose) futile.logger::flog.info(paste0("[CONFIG] ", ASE.name," ", File_cfg, " for the configuration of AirSensEUR exists"))
            sens2ref <- data.table::transpose(data.table::fread(file = File_cfg, header = FALSE, na.strings = c("","NA", "<NA>"), fill = TRUE), fill = NA, make.names = 1)
            if ("V1" %in% names(sens2ref)) sens2ref[, V1 := NULL]
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
            # Adding Sync.Cal, Sync.Pred, Regression, Add.ubss, variable.ubsRM, variable.ubss and Fitted.RS if missing
            if (!"Sync.Cal"  %in% names(sens2ref)) sens2ref$Sync.Cal  <- FALSE
            if (!"Sync.Pred" %in% names(sens2ref)) sens2ref$Sync.Pred <- FALSE
            if (!"Regression" %in% names(sens2ref)) sens2ref[, Regression := "OLS"]
            if (!"Add.ubss" %in% names(sens2ref)) sens2ref[, Add.ubss := FALSE]
            if (!"variable.ubsRM" %in% names(sens2ref)) sens2ref[, variable.ubsRM := FALSE]
            if (!"perc.ubsRM" %in% names(sens2ref)) data.table::set(sens2ref, j = "perc.ubsRM", value = rep(0.03, nrow(sens2ref)))
            if (!"variable.ubss" %in% names(sens2ref)) sens2ref[, variable.ubss := FALSE]
            if (!"perc.ubss" %in% names(sens2ref)) data.table::set(sens2ref, j = "perc.ubss", value = rep(0.03, nrow(sens2ref)))
            if (!"Fitted.RS" %in% names(sens2ref)) sens2ref[, Fitted.RS := FALSE]
            if (!"UserMinsAvg" %in% names(sens2ref)) data.table::set(sens2ref, j = "UserMinsAvg", value = rep(cfg$UserMinsAvg, nrow(sens2ref)))
            if (!"hour_start" %in% names(sens2ref)) data.table::set(sens2ref, j = "hour_start", value = rep("00:00", nrow(sens2ref)))
            if (!"Agg.min" %in% names(sens2ref)) data.table::set(sens2ref, j = "Agg.min", value = rep(cfg$UserMins, nrow(sens2ref)))
            
            # Adding ubss and ubsRM if missing, the between sampler uncertainties for sensor and reference method
            if (!"ubsRM" %in% names(sens2ref)) {
                if (!"uxi" %in% names(sens2ref)) {
                    sens2ref$ubsRM  <- rep(0, times = nrow(sens2ref))
                } else sens2ref$ubsRM  <- sens2ref$uxi}
            if (!"ubss" %in% names(sens2ref)) {
                if (!"uxi" %in% names(sens2ref)) {
                    sens2ref$ubss  <- rep(0, times = nrow(sens2ref))
                } else {
                    sens2ref$ubss  <- sens2ref$uxi
                    sens2ref[, uxi := NULL]}}
            
            #change the type of column in df
            Vector.type <- c("Ref.rm.Out","Sens.Inval.Out","Apply.Invalid", "remove.neg","Sens.rm.Out","Neg.mod", "Sync.Cal" , "Sync.Pred", "Add.ubss", "variable.ubsRM", "variable.ubss", "Fitted.RS")
            Vector.type <- Vector.type[Vector.type %in% names(sens2ref)]
            for (j in Vector.type) data.table::set(sens2ref, j = j, value = as.logical(gsub(" ","",sens2ref[[j]])))
            Vector.type <- c("Ref.window","Ref.threshold","Ref.Ymin","Ref.Ymax","Ref.ThresholdMin","Ref.iterations","Gain","Intercept","Slope",
                             "Sens.window","Sens.threshold","Sens.Ymin","Sens.Ymax","Sens.ThresholdMin","Sens.iterations", "ubsRM", "perc.ubsRM","ubss", "perc.ubss",
                             "Rload","TIA_Gain","GAIN","Int_Z","Bias_Sign","Bias", "Ref","RefAD","RefAFE","board.zero.set","BIAIS",
                             "temp.thres.min","temp.thres.max","rh.thres.min","rh.thres.max","hoursWarming", "UserMinsAvg", "Agg.min")
            Vector.type <- Vector.type[Vector.type %in% names(sens2ref)]
            for (j in Vector.type) data.table::set(sens2ref, j = j, value = as.numeric(gsub(" ","",sens2ref[[j]])))
        } else { 
            # if no ASE.name,".cfg", Message of error
            my_message <- paste0("[CONFIG] ERROR, no sensor config file for the AirSensEUR box. \n",
                                 "The App is going to crash. This AirSensEUR cannot be selected.\n")
            cat(my_message)
            if (shiny) shinyalert(
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
        # Checking that all sensors are in sensors.cfg
        if (exists("Sensors.cfg") && !all(sens2ref$name.sensor %in% unique(Sensors.cfg$name))) {
            row2Delete <- which(sens2ref$name.sensor %in% unique(Sensors.cfg$name))
            if (length(row2Delete) > 0) sens2ref <- sens2ref[-row2Delete]
            stopifnot(nrow(sens2ref) > 0)
        }
        # updating names of sensors with the sensor Shield config file
        if (is.null(sens2ref.shield)) {
            # Reading chemical shield config file and merging with sens2ref if the file exists
            Shield.file <- file.path(DisqueFieldtest,"Shield_Files",cfg$asc.File)
            if (file.exists(Shield.file)) {
                sens2ref.shield <- ASEPanel04Read(ASEPanel04File = Shield.file)
            } else {
                # if ASE.name,".cfg", Message of error
                my_message <- paste0("[CONFIG] ERROR, no chemical shield config file for the AirSensEUR box. \n",
                                     "The App is going to crash. This AirSensEUR cannot be selected.\n")
                if (shiny) shinyalert(
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
                    timer = 4000,
                    imageUrl = "",
                    animation = FALSE)
                return(my_message)
            }
            if (!data.table::is.data.table(sens2ref.shield)) sens2ref.shield <- data.table::data.table(sens2ref.shield, key = "name.gas")
            # Set same order as sens2ref for rows and columns
            sens2ref.shield      <- sens2ref.shield[match(na.omit(sens2ref.shield[["gas.sensor"]]), na.omit(sens2ref.shield[["gas.sensor"]])),]
            
            sens2ref.cols2change <- intersect(names(sens2ref), names(sens2ref.shield))
            setcolorder(sens2ref.shield, sens2ref.cols2change)
            # checking if shield columns shall be changed for chemical sensors
            if (length(intersect(sens2ref.shield$name.sensor, sens2ref$name.sensor)) > 0 &&
                !isTRUE(all.equal(sens2ref.shield,
                                  sens2ref[match(na.omit(sens2ref.shield[["gas.sensor"]]), sens2ref$gas.sensor), sens2ref.cols2change, with = F],
                                  check.attributes = TRUE, tolerance = 1E-5))) {
                #change the type of column in sens2ref.shield
                Vector.type <- c("Ref.rm.Out","Sens.Inval.Out","Apply.Invalid", "remove.neg","Sens.rm.Out","Neg.mod", "Sync.Cal" , "Sync.Pred", "Add.ubss", "variable.ubsRM", "variable.ubss", "Fitted.RS")
                Vector.type <- Vector.type[Vector.type %in% names(sens2ref.shield)]
                for (j in Vector.type) data.table::set(sens2ref.shield, j = j, value = as.logical(gsub(" ","",sens2ref.shield[[j]])))
                Vector.type <- c("Ref.window","Ref.threshold","Ref.Ymin","Ref.Ymax","Ref.ThresholdMin","Ref.iterations","Gain","Intercept","Slope",
                                 "Sens.window","Sens.threshold","Sens.Ymin","Sens.Ymax","Sens.ThresholdMin","Sens.iterations", "ubsRM", "ubss",
                                 "Rload","TIA_Gain","GAIN","Int_Z","Bias_Sign","Bias", "Ref","RefAD","RefAFE","board.zero.set","BIAIS",
                                 "temp.thres.min","temp.thres.max","rh.thres.min","rh.thres.max","hoursWarming")
                Vector.type <- Vector.type[Vector.type %in% names(sens2ref.shield)]
                for (j in Vector.type) data.table::set(sens2ref.shield, j = j, value = as.numeric(gsub(" ","",sens2ref.shield[[j]])))
                data.table::set(sens2ref, i = match(na.omit(sens2ref.shield[["gas.sensor"]]), sens2ref$gas.sensor), j = sens2ref.cols2change, value = sens2ref.shield)
                # Saving new version
                data.table::fwrite(data.table::setDT(as.data.frame(t(sens2ref)), keep.rownames = "name.gas")[], file = file.path(DisqueFieldtestDir,Dir.Config,paste0(ASE.name,".cfg")), row.names = FALSE,col.names = FALSE)
            }
        }
    } else { # if File_cfg does not exist, , Message of error
        my_message <- paste0("[CONFIG] ERROR, no sensor config file for the AirSensEUR box. \n",
                             "The App is going to crash. This AirSensEUR cannot be selected.\n")
        cat(my_message)
        if (shiny) shinyalert(
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
    # re-ordering columns
    setcolorder(sens2ref, Columns <- c("name.gas", "name.sensor", "gas.sensor", "Sens.raw.unit", "Sens.unit", "gas.reference", "gas.reference2use", "ref.unitgas", 
                                       "Cal.Line", "Cal.func", "mod.eta.model.type", "Neg.mod", "Slope", "Intercept", "ubsRM",  "variable.ubsRM","ubss", "Add.ubss", "variable.ubss", "Regression", "Fitted.RS","Sync.Cal", "Sync.Pred", "eta.model.type", 
                                       "hoursWarming", "temp.thres.min", "temp.thres.max", "rh.thres.min", "rh.thres.max", "Sens.Inval.Out", "Sens.rm.Out", 
                                       "Sens.window", "Sens.threshold", "Sens.Ymin", "Sens.Ymax", "Sens.ThresholdMin", "Sens.iterations", "remove.neg", 
                                       "Ref.rm.Out", "Ref.window", "Ref.threshold", "Ref.Ymin", "Ref.Ymax", "Ref.ThresholdMin", "Ref.iterations",
                                       "Ref", "RefAD", "RefAFE", "Rload", "TIA_Gain", "GAIN", "Ref_Source", "Int_Z", "Bias_Sign", "Bias", "Fet_Short", "Mode", "board.zero.set", "BIAIS"))
    # Adding sensors if not included in sens2ref
    Columns.Text <- c("name.gas", "name.sensor", "gas.sensor"    , "Sens.raw.unit", "Sens.unit", 
                      "gas.reference", "gas.reference2use", "ref.unitgas", 
                      "Cal.Line", "Cal.func", "mod.eta.model.type", "eta.model.type",
                      "Ref_Source", "Fet_Short", "Regression")
    Columns.Num  <- c( "Slope", "Intercept", "ubsRM", "ubss", 
                       "hoursWarming", "temp.thres.min", "temp.thres.max", "rh.thres.min", "rh.thres.max",
                       "Sens.window", "Sens.threshold", "Sens.Ymin", "Sens.Ymax", "Sens.ThresholdMin", "Sens.iterations",
                       "Ref.window", "Ref.threshold", "Ref.Ymin", "Ref.Ymax", "Ref.ThresholdMin", "Ref.iterations",
                       "Ref", "RefAD", "RefAFE", "Rload", "TIA_Gain", "GAIN", "Int_Z", "Bias_Sign", "Bias", "Mode", "board.zero.set", "BIAIS")
    Columns.Bool  <- c( "Neg.mod", "Sync.Cal", "Sync.Pred", "Sens.Inval.Out", "Sens.rm.Out", "remove.neg", "Ref.rm.Out", "Add.ubss", "variable.ubsRM", "variable.ubss", "Fitted.RS")
    #### Adding missing sensors
    # Adding LPLCPC K96_methane
    # if (!is.null(Names.Influx) && !is.null(Names.ref) && 
    #     !"CH4" %in% sens2ref$name.gas && "K96_methane" %in% Names.Influx && "Ref.CH4" %in% Names.ref) {
    #     # adding the first row at the bottom of the table
    #     sens2ref <- rbind(sens2ref, sens2ref[1])
    #     # Correcting the last row
    #     Values.Text <- c("CH4", "LPLCPC", "K96_methane", "ppm", "ppm",    "CH4", "Ref.CH4", "ppm",    "Prediction with previous calibration","", "Linear.Robust", "Linear",    NA , NA, "OLS")
    #     Values.num  <- c(1, 0, 3, 15    ,4, -20, 40, 0, 100,    181, 20, 200, 2000, 200, 1,    181, 75, 200, 2000, 200, 1,     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    #     Values.Bool <- rep(FALSE, length(Columns.Bool))
    #     data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Text, value = lapply(Values.Text, function(i) i))
    #     data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Num,  value = lapply(Values.num, function(i) i))
    #     data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Bool, value = lapply(Values.Bool, function(i) i))
    #     # Saving new version
    #     data.table::fwrite(data.table::setDT(as.data.frame(t(sens2ref)), keep.rownames = "name.gas")[], file = file.path(DisqueFieldtestDir,Dir.Config,paste0(ASE.name,".cfg")), row.names = FALSE,col.names = FALSE)}
    # Adding D300 ELT
    if (!is.null(Names.Influx) && !is.null(Names.ref) && 
        !"CO2" %in% sens2ref$name.gas && "Carbon_dioxide" %in% Names.Influx && "Ref.CO2" %in% Names.ref) {
        # adding the first row at the bottom of the table
        sens2ref <- rbind(sens2ref, sens2ref[1])
        # Correcting the last row
        Values.Text <- c("CO2", "D300", "Carbon_dioxide", "ppm", "ppm",    "CO2", "Ref.CO2", "ppm",    "Prediction with previous calibration","", "Linear.Robust", "Linear",    NA , NA, "OLS")
        Values.num  <- c(1, 0, 3, 15    ,4, -20, 40, 0, 100,    181, 20, 200, 2000, 200, 1,    181, 75, 200, 2000, 200, 1,     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
        Values.Bool <- rep(FALSE, length(Columns.Bool))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Text, value = lapply(Values.Text, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Num,  value = lapply(Values.num, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Bool, value = lapply(Values.Bool, function(i) i))
        # Saving new version
        data.table::fwrite(data.table::setDT(as.data.frame(t(sens2ref)), keep.rownames = "name.gas")[], file = file.path(DisqueFieldtestDir,Dir.Config,paste0(ASE.name,".cfg")), row.names = FALSE,col.names = FALSE)}
    # Adding PM1, PM2.5  and PM10-OPC-N3 if not included
    if (!is.null(Names.Influx) && !is.null(Names.ref) && 
        !"OPCN3PM10" %in% sens2ref$name.sensor && "Particulate_Matter_10" %in% Names.Influx && "Ref.PM10" %in% Names.ref) {
        # adding the first row at the bottom of the table
        sens2ref <- rbind(sens2ref, sens2ref[1])
        # Correcting the last row
        Values.Text <- c("PM10", "OPCN3PM10", "Particulate_Matter_10", "ug/m3", "ug/m3",    "PM10", "Ref.PM10", "ug/m3",    "Prediction with previous calibration","", "Linear.Robust", "Linear",    NA , NA, "OLS")
        Values.num  <- c(1, 0, 2.5, 5    ,4, -20, 40, 0, 75,    181, 20, 0, 2000, 0, 1,    181, 75, 0, 500, 0, 1,     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
        Values.Bool <- rep(FALSE, length(Columns.Bool))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Text, value = lapply(Values.Text, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Num,  value = lapply(Values.num, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Bool, value = lapply(Values.Bool, function(i) i))
        # Saving new version
        data.table::fwrite(data.table::setDT(as.data.frame(t(sens2ref)), keep.rownames = "name.gas")[], file = file.path(DisqueFieldtestDir,Dir.Config,paste0(ASE.name,".cfg")), row.names = FALSE,col.names = FALSE)}
    if (!is.null(Names.Influx) && !is.null(Names.ref) && 
        !"OPCN3PM25" %in% sens2ref$name.sensor && "Particulate_Matter_25" %in% Names.Influx && "Ref.PM2.5" %in% Names.ref) {
        # adding the first row at the bottom of the table
        sens2ref <- rbind(sens2ref, sens2ref[1])
        # Correcting the last row
        Values.Text <- c("PM2.5", "OPCN3PM25", "Particulate_Matter_25", "ug/m3", "ug/m3",    "PM2.5", "Ref.PM2.5", "ug/m3",    "Prediction with previous calibration","", "Linear.Robust", "Linear",    NA , NA, "OLS")
        Values.num  <- c(1, 0, 2.5, 5    ,4, -20, 40, 0, 75,    181, 20, 0, 2000, 0, 1,    181, 75, 0, 500, 0, 1,     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
        Values.Bool <- rep(FALSE, length(Columns.Bool))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Text, value = lapply(Values.Text, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Num,  value = lapply(Values.num, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Bool, value = lapply(Values.Bool, function(i) i))
        # Saving new version
        data.table::fwrite(data.table::setDT(as.data.frame(t(sens2ref)), keep.rownames = "name.gas")[], file = file.path(DisqueFieldtestDir,Dir.Config,paste0(ASE.name,".cfg")), row.names = FALSE,col.names = FALSE)}
    if (!is.null(Names.Influx) && !is.null(Names.ref) && 
        !"OPCN3PM1" %in% sens2ref$name.sensor && "Particulate_Matter_1" %in% Names.Influx && "Ref.PM1" %in% Names.ref) {
        # adding the first row at the bottom of the table
        sens2ref <- rbind(sens2ref, sens2ref[1])
        # Correcting the last row
        Values.Text <- c("PM1", "OPCN3PM1", "Particulate_Matter_1", "ug/m3", "ug/m3",    "PM1", "Ref.PM1", "ug/m3",    "Prediction with previous calibration","", "Linear.Robust", "Linear",    NA , NA, "OLS")
        Values.num  <- c(1, 0, 2.5, 5    ,4, -20, 40, 0, 75,    181, 20, 0, 2000, 0, 1,    181, 75, 0, 500, 0, 1,     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
        Values.Bool <- rep(FALSE, length(Columns.Bool))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Text, value = lapply(Values.Text, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Num,  value = lapply(Values.num, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Bool, value = lapply(Values.Bool, function(i) i))
        # Saving new version
        data.table::fwrite(data.table::setDT(as.data.frame(t(sens2ref)), keep.rownames = "name.gas")[], file = file.path(DisqueFieldtestDir,Dir.Config,paste0(ASE.name,".cfg")), row.names = FALSE,col.names = FALSE)}
    # Adding PM1, PM2.5 and PM10-PMS5003 if not included
    if (!is.null(Names.Influx) && !is.null(Names.ref) &&
        !"5310CAT" %in% sens2ref$name.sensor && "PM10_PMSCal" %in% Names.Influx && "Ref.PM10" %in% Names.ref) {
        # adding the first row at the bottom of the table
        sens2ref <- rbind(sens2ref, sens2ref[1])
        # Correcting the last row
        Values.Text <- c("PM10", "5310CAT", "PM10_PMSCal", "ug/m3", "ug/m3",    "PM10", "Ref.PM10", "ug/m3",    "Prediction with previous calibration","", "Linear.Robust", "Linear",    NA , NA, "OLS")
        Values.num  <- c(1, 0, 2.5, 5    ,4, -20, 40, 0, 80,    181, 20, 0, 500, 0, 1,    181, 75, 0, 500, 0, 1,     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
        Values.Bool <- rep(FALSE, length(Columns.Bool))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Text, value = lapply(Values.Text, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Num,  value = lapply(Values.num, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Bool, value = lapply(Values.Bool, function(i) i))
        # Saving new version
        data.table::fwrite(data.table::setDT(as.data.frame(t(sens2ref)), keep.rownames = "name.gas")[], file = file.path(DisqueFieldtestDir,Dir.Config,paste0(ASE.name,".cfg")), row.names = FALSE,col.names = FALSE)}
    if (!is.null(Names.Influx) && !is.null(Names.ref) &&
        !"5325CAT" %in% sens2ref$name.sensor && "PM25_PMSCal" %in% Names.Influx && "Ref.PM25" %in% Names.ref) {
        # adding the first row at the bottom of the table
        sens2ref <- rbind(sens2ref, sens2ref[1])
        # Correcting the last row
        Values.Text <- c("PM2.5", "5325CAT", "PM25_PMSCal", "ug/m3", "ug/m3",    "PM2.5", "Ref.PM2.5", "ug/m3",    "Prediction with previous calibration","", "Linear.Robust", "Linear",    NA , NA, "OLS")
        Values.num  <- c(1, 0, 2.5, 5    ,4, -20, 40, 0, 80,    181, 20, 0, 500, 0, 1,    181, 75, 0, 500, 0, 1,     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
        Values.Bool <- rep(FALSE, length(Columns.Bool))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Text, value = lapply(Values.Text, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Num,  value = lapply(Values.num, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Bool, value = lapply(Values.Bool, function(i) i))
        # Saving new version
        data.table::fwrite(data.table::setDT(as.data.frame(t(sens2ref)), keep.rownames = "name.gas")[], file = file.path(DisqueFieldtestDir,Dir.Config,paste0(ASE.name,".cfg")), row.names = FALSE,col.names = FALSE)}
    if (!is.null(Names.Influx) && !is.null(Names.ref) &&
        !"5301CAT" %in% sens2ref$name.sensor && "PM1_PMSCal" %in% Names.Influx && "Ref.PM1" %in% Names.ref) {
        # adding the first row at the bottom of the table
        sens2ref <- rbind(sens2ref, sens2ref[1])
        # Correcting the last row
        Values.Text <- c("PM1", "5301CAT", "PM1_PMSCal", "ug/m3", "ug/m3",    "PM1", "Ref.PM1", "ug/m3",    "Prediction with previous calibration","", "Linear.Robust", "Linear",    NA , NA, "OLS")
        Values.num  <- c(1, 0, 2.5, 5    ,4, -20, 40, 0, 80,    181, 20, 0, 500, 0, 1,    181, 75, 0, 500, 0, 1,     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
        Values.Bool <- rep(FALSE, length(Columns.Bool))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Text, value = lapply(Values.Text, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Num,  value = lapply(Values.num, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Bool, value = lapply(Values.Bool, function(i) i))
        # Saving new version
        data.table::fwrite(data.table::setDT(as.data.frame(t(sens2ref)), keep.rownames = "name.gas")[], file = file.path(DisqueFieldtestDir,Dir.Config,paste0(ASE.name,".cfg")), row.names = FALSE,col.names = FALSE)}
    if (!is.null(Names.Influx) && !is.null(Names.ref) &&
        !"5310CST" %in% sens2ref$name.sensor && "PM10_PMSraw" %in% Names.Influx && "Ref.PM10" %in% Names.ref) {
        # adding the first row at the bottom of the table
        sens2ref <- rbind(sens2ref, sens2ref[1])
        # Correcting the last row
        Values.Text <- c("PM10", "5310CST", "PM10_PMSraw", "ug/m3", "ug/m3",    "PM10", "Ref.PM10", "ug/m3",    "Prediction with previous calibration","", "Linear.Robust", "Linear",    NA , NA, "OLS")
        Values.num  <- c(1, 0, 2.5, 5    ,4, -20, 40, 0, 80,    181, 20, 0, 500, 0, 1,    181, 75, 0, 500, 0, 1,     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
        Values.Bool <- rep(FALSE, length(Columns.Bool))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Text, value = lapply(Values.Text, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Num,  value = lapply(Values.num, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Bool, value = lapply(Values.Bool, function(i) i))
        # Saving new version
        data.table::fwrite(data.table::setDT(as.data.frame(t(sens2ref)), keep.rownames = "name.gas")[], file = file.path(DisqueFieldtestDir,Dir.Config,paste0(ASE.name,".cfg")), row.names = FALSE,col.names = FALSE)}
    if (!is.null(Names.Influx) && !is.null(Names.ref) &&
        !"5325CST" %in% sens2ref$name.sensor && "PM25_PMSraw" %in% Names.Influx && "Ref.PM25" %in% Names.ref) {
        # adding the first row at the bottom of the table
        sens2ref <- rbind(sens2ref, sens2ref[1])
        # Correcting the last row
        Values.Text <- c("PM2.5", "5325CST", "PM25_PMSraw", "ug/m3", "ug/m3",    "PM2.5", "Ref.PM2.5", "ug/m3",    "Prediction with previous calibration","", "Linear.Robust", "Linear",    NA , NA, "OLS")
        Values.num  <- c(1, 0, 2.5, 5    ,4, -20, 40, 0, 80,    181, 20, 0, 500, 0, 1,    181, 75, 0, 500, 0, 1,     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
        Values.Bool <- rep(FALSE, length(Columns.Bool))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Text, value = lapply(Values.Text, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Num,  value = lapply(Values.num, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Bool, value = lapply(Values.Bool, function(i) i))
        # Saving new version
        data.table::fwrite(data.table::setDT(as.data.frame(t(sens2ref)), keep.rownames = "name.gas")[], file = file.path(DisqueFieldtestDir,Dir.Config,paste0(ASE.name,".cfg")), row.names = FALSE,col.names = FALSE)}
    if (!is.null(Names.Influx) && !is.null(Names.ref) &&
        !"5301CST" %in% sens2ref$name.sensor && "PM1_PMSraw" %in% Names.Influx && "Ref.PM1" %in% Names.ref) {
        # adding the first row at the bottom of the table
        sens2ref <- rbind(sens2ref, sens2ref[1])
        # Correcting the last row
        Values.Text <- c("PM1", "5301CST", "PM1_PMSraw", "ug/m3", "ug/m3",    "PM1", "Ref.PM1", "ug/m3",    "Prediction with previous calibration","", "Linear.Robust", "Linear",    NA , NA, "OLS")
        Values.num  <- c(1, 0, 2.5, 5    ,4, -20, 40, 0, 80,    181, 20, 0, 500, 0, 1,    181, 75, 0, 500, 0, 1,     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
        Values.Bool <- rep(FALSE, length(Columns.Bool))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Text, value = lapply(Values.Text, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Num,  value = lapply(Values.num, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Bool, value = lapply(Values.Bool, function(i) i))
        # Saving new version
        data.table::fwrite(data.table::setDT(as.data.frame(t(sens2ref)), keep.rownames = "name.gas")[], file = file.path(DisqueFieldtestDir,Dir.Config,paste0(ASE.name,".cfg")), row.names = FALSE,col.names = FALSE)}
    # Adding SHT31TI if not included
    if (!is.null(Names.Influx) && !is.null(Names.ref) &&
        !"SHT31TI" %in% sens2ref$name.sensor && "Temperature_int" %in% Names.Influx && "Ref.Temp" %in% Names.ref) {
        # adding the first row at the bottom of the table
        sens2ref <- rbind(sens2ref, sens2ref[1])
        # Correcting the last row
        Values.Text <- c("Temp_int", "SHT31TI", "Temperature_int", "Celsius", "Celsius",    "Temp", "Ref.Temp", "Celsius",    "Prediction with previous calibration","", "Linear.Robust", "Linear",    NA , NA, "OLS")
        Values.num  <- c(1, 0, 2.5, 5    ,4, -20, 40, 0, 80,    181, 20, 0, 500, 0, 1,    181, 75, 0, 500, 0, 1,     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
        Values.Bool <- rep(FALSE, length(Columns.Bool))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Text, value = lapply(Values.Text, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Num,  value = lapply(Values.num, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Bool, value = lapply(Values.Bool, function(i) i))
        # Saving new version
        data.table::fwrite(data.table::setDT(as.data.frame(t(sens2ref)), keep.rownames = "name.gas")[], file = file.path(DisqueFieldtestDir,Dir.Config,paste0(ASE.name,".cfg")), row.names = FALSE,col.names = FALSE)}
    # Adding SHT31HI if not included
    if (!is.null(Names.Influx) && !is.null(Names.ref) &&
        !"SHT31HI" %in% sens2ref$name.sensor && "Relative_humidity_int" %in% Names.Influx && "Ref.RH" %in% Names.ref) {
        # adding the first row at the bottom of the table
        sens2ref <- rbind(sens2ref, sens2ref[1])
        # Correcting the last row
        Values.Text <- c("RH_int", "SHT31HI", "Relative_humidity_int", "percent", "percent",    "RH", "Ref.RH", "percent",    "Prediction with previous calibration","", "Linear.Robust", "Linear",    NA , NA, "OLS")
        Values.num  <- c(1, 0, 2.5, 5    ,4, -20, 40, 0, 80,    181, 20, 0, 500, 0, 1,    181, 75, 0, 500, 0, 1,     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
        Values.Bool <- rep(FALSE, length(Columns.Bool))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Text, value = lapply(Values.Text, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Num,  value = lapply(Values.num, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Bool, value = lapply(Values.Bool, function(i) i))
        # Saving new version
        data.table::fwrite(data.table::setDT(as.data.frame(t(sens2ref)), keep.rownames = "name.gas")[], file = file.path(DisqueFieldtestDir,Dir.Config,paste0(ASE.name,".cfg")), row.names = FALSE,col.names = FALSE)}
    
    # reading the files with Covariates to plot and covariates to calibrate
    for (i in 1:length(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)])) {
        
        nameSens <- sens2ref$name.sensor[which(!is.na(sens2ref$name.sensor))][i]
        nameGas  <- sens2ref$name.gas[which(sens2ref$name.sensor == nameSens)]
        nameFile <- file.path(DisqueFieldtestDir,Dir.Config,paste0(ASE.name,"_Covariates_",nameSens,".cfg"))
        # Covariates to plot
        if (file.exists(nameFile)) {
            if (Verbose) futile.logger::flog.info(paste0("[CONFIG] ", ASE.name," ", nameFile, " with covariates to plot exists."))
            assign(nameSens,
                   read.csv(file = nameFile,
                            header = TRUE,
                            comment.char = "",
                            stringsAsFactors = FALSE))
        } else {
            if (Verbose) futile.logger::flog.error(paste0("[CONFIG] ", ASE.name," ", nameFile, 
                                                          " covariates to plot does not exist. File is iniatized with the R script info."))
            # DEFINE The lists of variables to plot in retrieving data () to understand the inerferences - use report of lab. tests
            if (nameGas == "CH4") {
                assign(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],
                       data.frame(Effects = c(paste0(nameSens,"_volt"), "Out.Ref.CH4_sync" , "Out.Relative_humidity", "Out.Temperature")))
            } else if (nameGas == "CO") {
                assign(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],
                       data.frame(Effects = c(paste0(nameSens,"_volt"), "Out.Ref.CO_ppm" , "Out.Relative_humidity", "Out.Temperature"))) #####################################################
            } else if (nameGas == "O3" || nameGas == "NO2") {
                assign(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],
                       data.frame(Effects = c(paste0(nameSens,"_volt"), "Out.Ref.NO2", "Out.Ref.O3", "Out.Relative_humidity", "Out.Temperature")))
            } else if (nameGas == "NO") {
                assign(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],
                       data.frame(Effects = c(paste0(nameSens,"_volt"), "Out.Ref.NO", "Out.Relative_humidity", "Out.Temperature")))
            } else if (nameGas == "CO2") {
                assign(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],
                       data.frame(Effects = c(paste0(nameSens,"_volt"), "Out.Ref.CO2", "Out.Relative_humidity", "Out.Temperature")))
            } else if (nameGas == "PM10") {
                assign(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],
                       data.frame(Effects = c(paste0(nameSens,"_volt"), "Out.Ref.PM10", "Out.Relative_humidity", "Out.Temperature")))
            } else if (nameGas == "PM2.5") {
                assign(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],
                       data.frame(Effects = c(paste0(nameSens,"_volt"), "Out.Ref.PM2.5", "Out.Relative_humidity", "Out.Temperature")))
            } else if (nameGas == "PM1") {
                assign(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],
                       data.frame(Effects = c(paste0(nameSens,"_volt"), "Out.Ref.PM1", "Out.Relative_humidity", "Out.Temperature")))
            } else if (nameGas == "Temp") {
                assign(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],
                       data.frame(Effects = c(paste0(nameSens,"_volt"), "Out.Ref.Temp")))
            } else if (nameGas == "RH") {
                assign(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],
                       data.frame(Effects = c(paste0(nameSens,"_volt"), "Out.Ref.RH")))
            } else if (nameGas == "Patm") {
                assign(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],
                       data.frame(Effects = c(paste0(nameSens,"_volt"), "Out.Ref.Press")))
            } else assign(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],
                          data.frame(Effects = c(paste0(nameSens,"_volt"), "Out.Ref.Temp")))
            # Saving the effect files
            SENS <- get(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i])
            write.csv(SENS, file = nameFile, row.names = FALSE)
            rm(SENS)
        }
        # Covariates to calibrate
        nameFile <- file.path(DisqueFieldtestDir,Dir.Config,paste0(ASE.name,"_CovMod_",nameSens,".cfg"))
        if (file.exists(nameFile) && nrow(data.table::fread(nameFile)) > 0) {
            if (Verbose) futile.logger::flog.info(paste0("[CONFIG] ", ASE.name," ", nameFile, " with covariates to calibrate exists."))
            assign(paste0(nameSens,"CovMod"),
                   data.table::fread(file = nameFile, header = TRUE, stringsAsFactors = FALSE) # for read_csv add ", comment.char = "#""
            )
        } else {
            if (Verbose) futile.logger::flog.error(paste0("[CONFIG] ", ASE.name," ", nameFile, 
                                                          " covariates to calibrate does not exist. File is inialized with Temperature."))
            # DEFINE The lists of variables to plot in retrieving data () to understand the inerferences - use report of lab. tests
            assign(paste0(nameSens,"CovMod"),
                   data.frame(Effects = "Out.Temperature"))
            # Saving the effect files
            SENS <- get(paste0(nameSens,"CovMod"))
            write.csv(SENS, file = nameFile, row.names = FALSE)
            rm(SENS)
        }
    }
    Covariates <- lapply(which(!is.na(sens2ref$name.sensor)), function(i) get(sens2ref$name.sensor[i]) )
    names(Covariates) <- paste0(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)])
    CovMod <- lapply(which(!is.na(sens2ref$name.sensor)), function(i) get(paste0(sens2ref$name.sensor[i],"CovMod")))
    names(CovMod) <- paste0(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)])
    # Moving Config files to directory Configuration if needed
    if (Dir.Config == "General_data") {
        # Populating the configuration intormation with cfg and effect files, create if it does not exist
        New_General_dir <- file.path(DisqueFieldtestDir, Dir.Config)
        if (!dir.exists(New_General_dir)) dir.create(New_General_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
        cfg_Files       <- list.files(path = file.path(DisqueFieldtestDir, "General_data"), pattern = ".cfg", recursive = TRUE, full.names = TRUE)
        for (i in cfg_Files) {
            if (Verbose) futile.logger::flog.info(paste0("[CONFIG()] ", ASE.name," copying ", basename(i), " at ", New_General_dir))
            file.copy(from = i, to = file.path(New_General_dir, basename(i)), overwrite = TRUE, copy.mode = TRUE, copy.date = FALSE)
            file.remove(i)}
        
    }
    if (Verbose) cat("-----------------------------------------------------------------------------------\n")
    return.CONFIG <- list(cfg,sens2ref,Covariates,CovMod, sens2ref.shield)
    names(return.CONFIG) <- c("Server","sens2ref","CovPlot","CovMod", "sens2ref.shield")
    if (exists("Sensors.cfg")) return.CONFIG$Sensors.cfg <- Sensors.cfg
    if (exists("board.cfg"))   return.CONFIG$board.cfg   <- board.cfg
    return(return.CONFIG)}
#=====================================================================================CR
# SETTIME Reaf file with configuration dates for ASE Time Periods ####
#=====================================================================================CR
#' @param Config A List, default is null, returned by function CONFIG(). If NULL, Config is loaded using function Identify_ASE(). If General.DT is given, Config shall be given as well.

SETTIME <- function(DisqueFieldtestDir, DisqueFieldtest, General.t.Valid = NULL, Influx.TZ = "UTC" , SOS.TZ = "UTC", Ref.TZ = "UTC", DownloadSensor, Config = NULL,
                    sens2ref.shield = NULL, shiny = TRUE, Dir.Config = "Configuration") {
    # Return             : list with sens2ref (only time parameters)
    # DisqueFieldtestDir : file.path of the data of AirSensEUR box with sub-directories General_data, configuration, Models ..., e.g. "S:/Box Sync/AirSensEUR/Fieldtests/Shiny/ASE_Boxes/4047D0"
    # DisqueFieldtest    : directory where is the file ASEconfig*.R file, which is not used anymore. It is now used because it it the directory of the Shiny App with directory Shield_files
    # General.t.Valid    : dataframe with date , sensor and reference data, default is NULL, it is only use if the the File_SETTIME_cfg file does not exist
    # DownloadSensor     : output of function DownloadSensor()
    # Influx.TZ          : String, time zone of INFLUXDB data, default is UTC
    # SOS.TZ             : String, time zone of SOS data, default is UTC
    # Ref.TZ             : String, time zone of Reference data, default is UTC
    # Config             : List, default is null, list returned by function CONFIG()
    # shiny              : logical, default value is TRUE. If TRUE the function ised in a Shiny reactive context and shinyalert message can be returned.
    # Dir.Config         : character string, default value is "Configuration". Sub dirctory of DisqueFieldtestDir that includes the config fles (*.cfg)
    cat("-----------------------------------------------------------------------------------\n")
    ASE.name           <- basename(DisqueFieldtestDir)
    # Setting the General time zone to the one of DownloadSensor$DateIN.General.prev
    # or DateIN.Influx.prev or DateIN.SOS.prev otherwise it is set to "UTC"
    if (exists("DownloadSensor")) {
        if (shiny::isTruthy(DownloadSensor$DateIN.General.prev)) {
            General.TZ <- lubridate::tz(DownloadSensor$DateIN.General.prev)
        } else {
            if (shiny::isTruthy(DownloadSensor$DateIN.Influx.prev)) {
                General.TZ <- lubridate::tz(DownloadSensor$DateIN.Influx.prev)
            } else {
                if (shiny::isTruthy(DownloadSensor$DateIN.SOS.prev)) {
                    General.TZ <- lubridate::tz(DownloadSensor$DateIN.SOS.prev)
                } else General.TZ <- "UTC"}}
    } else  General.TZ <- "UTC"
    # Read SetTime file
    Save.sens2ref <- FALSE
    File_SETTIME_cfg   <- list.files(path = file.path(DisqueFieldtestDir, Dir.Config), pattern = paste0(ASE.name,"_SETTIME.cfg")  )
    if (!identical(File_SETTIME_cfg,character(0))) {
        # reading the configuration files sens2ref
        File_SETTIME_cfg <- file.path(DisqueFieldtestDir, Dir.Config, paste0(ASE.name,"_SETTIME",".cfg"))
        if (file.exists(File_SETTIME_cfg)) {
            futile.logger::flog.info(paste0("[SETTIME] ", ASE.name," ", File_SETTIME_cfg, " for the configuration of AirSensEUR exists"))
            sens2ref <- data.table::fread(file = File_SETTIME_cfg, header = TRUE, na.strings = c("","NA", "<NA>"))
            if ("CH4" %in% Config$sens2ref$name.gas && !"CH4" %in% names(sens2ref)) {
                # adding the last column the table as CH4
                sens2ref <- cbind(sens2ref, sens2ref[,length(names(sens2ref)), with = FALSE])
                data.table::setnames(sens2ref, length(names(sens2ref)), "CH4")
                data.table::set(sens2ref, i = which(sens2ref$name.gas == "name.sensor"), j = length(names(sens2ref)), value = "LPLCPC")
                Save.sens2ref <- TRUE}
            if ("OPCN3PM10" %in% Config$sens2ref$name.sensor && !"OPCN3PM10" %in% sens2ref[which(name.gas == "name.sensor")]) {
                # adding the last column the table as PM10
                sens2ref <- cbind(sens2ref, sens2ref[,length(names(sens2ref)), with = FALSE])
                data.table::setnames(sens2ref, length(names(sens2ref)), "PM10")
                data.table::set(sens2ref, i = which(sens2ref$name.gas == "name.sensor"), j = length(names(sens2ref)), value = "OPCN3PM10")
                # Correcting the last Column values
                Save.sens2ref <- TRUE}
            if ("OPCN3PM25" %in% Config$sens2ref$name.sensor && !"OPCN3PM25" %in% sens2ref[which(name.gas == "name.sensor")]) {
                # adding the last column the table as PM10
                sens2ref <- cbind(sens2ref, sens2ref[,length(names(sens2ref)), with = FALSE])
                data.table::setnames(sens2ref, length(names(sens2ref)), "PM2.5")
                data.table::set(sens2ref, i = which(sens2ref$name.gas == "name.sensor"), j = length(names(sens2ref)), value = "OPCN3PM25")
                # Correcting the last Column values
                Save.sens2ref <- TRUE}
            if ("OPCN3PM1" %in% Config$sens2ref$name.sensor && !"OPCN3PM1" %in% sens2ref[which(name.gas == "name.sensor")]) {
                # adding the last column the table as PM10
                sens2ref <- cbind(sens2ref, sens2ref[,length(names(sens2ref)), with = FALSE])
                data.table::setnames(sens2ref, length(names(sens2ref)), "PM1")
                data.table::set(sens2ref, i = which(sens2ref$name.gas == "name.sensor"), j = length(names(sens2ref)), value = "OPCN3PM1")
                # Correcting the last Column values
                Save.sens2ref <- TRUE}
            if ("5310CAT" %in% Config$sens2ref$name.sensor && !"5310CAT" %in% sens2ref[which(name.gas == "name.sensor")]) {
                # adding the last column the table as PM10
                sens2ref <- cbind(sens2ref, sens2ref[,length(names(sens2ref)), with = FALSE])
                data.table::setnames(sens2ref, length(names(sens2ref)), "PM10")
                data.table::set(sens2ref, i = which(sens2ref$name.gas == "name.sensor"), j = length(names(sens2ref)), value = "5310CAT")
                # Correcting the last Column values
                Save.sens2ref <- TRUE}
            if ("5310CST" %in% Config$sens2ref$name.sensor && !"5310CST" %in% sens2ref[which(name.gas == "name.sensor")]) {
                # adding the last column the table as PM10
                sens2ref <- cbind(sens2ref, sens2ref[,length(names(sens2ref)), with = FALSE])
                data.table::setnames(sens2ref, length(names(sens2ref)), "PM10")
                data.table::set(sens2ref, i = which(sens2ref$name.gas == "name.sensor"), j = length(names(sens2ref)), value = "5310CST")
                # Correcting the last Column values
                Save.sens2ref <- TRUE}
            if ("5325CAT" %in% Config$sens2ref$name.sensor && !"5325CAT" %in% sens2ref[which(name.gas == "name.sensor")]) {
                # adding the last column the table as PM10
                sens2ref <- cbind(sens2ref, sens2ref[,length(names(sens2ref)), with = FALSE])
                data.table::setnames(sens2ref, length(names(sens2ref)), "PM2.5")
                data.table::set(sens2ref, i = which(sens2ref$name.gas == "name.sensor"), j = length(names(sens2ref)), value = "5325CAT")
                # Correcting the last Column values
                Save.sens2ref <- TRUE}
            if ("5325CST" %in% Config$sens2ref$name.sensor && !"5325CST" %in% sens2ref[which(name.gas == "name.sensor")]) {
                # adding the last column the table as PM25
                sens2ref <- cbind(sens2ref, sens2ref[,length(names(sens2ref)), with = FALSE])
                data.table::setnames(sens2ref, length(names(sens2ref)), "PM2.5")
                data.table::set(sens2ref, i = which(sens2ref$name.gas == "name.sensor"), j = length(names(sens2ref)), value = "5325CST")
                # Correcting the last Column values
                Save.sens2ref <- TRUE}
            if ("5301CAT" %in% Config$sens2ref$name.sensor && !"5301CAT" %in% sens2ref[which(name.gas == "name.sensor")]) {
                # adding the last column the table as PM25
                sens2ref <- cbind(sens2ref, sens2ref[,length(names(sens2ref)), with = FALSE])
                data.table::setnames(sens2ref, length(names(sens2ref)), "PM1")
                data.table::set(sens2ref, i = which(sens2ref$name.gas == "name.sensor"), j = length(names(sens2ref)), value = "5301CAT")
                # Correcting the last Column values
                Save.sens2ref <- TRUE}
            if ("5301CST" %in% Config$sens2ref$name.sensor && !"5301CST" %in% sens2ref[which(name.gas == "name.sensor")]) {
                # adding the last column the table as PM1
                sens2ref <- cbind(sens2ref, sens2ref[,length(names(sens2ref)), with = FALSE])
                data.table::setnames(sens2ref, length(names(sens2ref)), "PM1")
                data.table::set(sens2ref, i = which(sens2ref$name.gas == "name.sensor"), j = length(names(sens2ref)), value = "5301CST")
                # Correcting the last Column values
                Save.sens2ref <- TRUE}
            if ("SHT31HI" %in% Config$sens2ref$name.sensor && !"SHT31HI" %in% sens2ref[which(name.gas == "name.sensor")]) {
                # adding the last column the table as RH
                sens2ref <- cbind(sens2ref, sens2ref[,length(names(sens2ref)), with = FALSE])
                data.table::setnames(sens2ref, length(names(sens2ref)), "RH")
                data.table::set(sens2ref, i = which(sens2ref$name.gas == "name.sensor"), j = length(names(sens2ref)), value = "SHT31HI")
                # Correcting the last Column values
                Save.sens2ref <- TRUE}
            if ("SHT31TI" %in% Config$sens2ref$name.sensor && !"SHT31TI" %in% sens2ref[which(name.gas == "name.sensor")]) {
                # adding the last column the table as Temp
                sens2ref <- cbind(sens2ref, sens2ref[,length(names(sens2ref)), with = FALSE])
                data.table::setnames(sens2ref, length(names(sens2ref)), "Temp")
                data.table::set(sens2ref, i = which(sens2ref$name.gas == "name.sensor"), j = length(names(sens2ref)), value = "SHT31TI")
                # Correcting the last Column values
                Save.sens2ref <- TRUE}
        } else { 
            # sens2Ref missing, error message
            my_message <- paste0("[SETTIME] ERROR, no SetTime server config file for the AirSensEUR box. \n",
                                 "The App is going to crash. This AirSensEUR cannot be selected.\n")
            cat(my_message)
            if (shiny) shinyalert(
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
        if (shiny) shinyalert(
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
            File_Server_cfg    <- file.path(DisqueFieldtestDir, Dir.Config, paste0(ASE.name,"_Servers.cfg"))
            if (file.exists(File_Server_cfg)) {
                cfg <- data.table::transpose(data.table::fread(file = File_Server_cfg, header = FALSE, na.strings=c("","NA", "<NA>")), fill = NA, make.names = 1)
                # Changes names for change Shiny App version 0.9 to 0.10
                Change.names <- rbind(c("TZ"           , "Influx.TZ"),     # Time zone of Influx data
                                      c("sens.tzone"   , "SOS.TZ"))         # Time zone of SOS data
                for (k in 1:nrow(Change.names)) if (Change.names[k,1] %in% colnames(cfg)) colnames(cfg)[colnames(cfg) == Change.names[k,1]] <- Change.names[k,2]
            } else futile.logger::flog.error(paste0("[SETTIME] ", ASE.name," ", File_Server_cfg, " for server configuration for AirSensEUR:does not exist."))
        } else cfg <- Config[["Server"]]
        # Second read the shield config file to get the sensor names
        if (file.exists(file.path(DisqueFieldtest,"Shield_Files",cfg$asc.File))) {
            sens2ref.shield <- ASEPanel04Read(ASEPanel04File = file.path(DisqueFieldtest,"Shield_Files",cfg$asc.File))
        }  else futile.logger::flog.error("[SETTIME] ", ASE.name," shield file (asc.File) not found")}
    # update the name of sensors in the SETTIME.cfg
    if (!all(sens2ref.shield$name.sensor %in% na.omit(unlist(sens2ref[which(name.gas == "name.sensor"), .SD])[-1]))) {
        name.sensor2Change <- which(!sens2ref.shield$name.sensor %in% na.omit(unlist(sens2ref[which(name.gas == "name.sensor"), .SD])[-1]))
        for (i in name.sensor2Change) {
            if(sens2ref.shield$name.gas[i] %in% names(sens2ref)[-1]){
                data.table::set(sens2ref, i = which(sens2ref$name.gas  == "name.sensor"),
                                j = which(names(sens2ref) %in% sens2ref.shield$name.gas[i]), value = sens2ref.shield$name.sensor[i])}}
        # set to save file
        Save.sens2ref <- TRUE}
    # transpose the data.table
    sens2ref <- cbind(names(sens2ref)[-1],data.table::transpose(sens2ref, fill = NA, make.names =  "name.gas"))
    data.table::setnames(sens2ref, c("name.gas",names(sens2ref)[-1]) )
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
                          c("DatePlotmeas.END","DatePlotMeas.END"), # date range to plot extrapolation
                          c("Out.Ref.IN"   ,"Ref.IN"),       # Change names in ASE_App
                          c("Out.Ref.END"  ,"Ref.END"),      # Change names in ASE_App
                          c("Out.Sens.IN"  ,"Sens.IN"),      # Change names in ASE_App
                          c("Out.Sens.END" ,"Sens.END"),     # Change names in ASE_App
                          c("Cov.Date.IN"  ,"Cov.IN"),      # Change names in ASE_App
                          c("Cov.Date.END" ,"Cov.END"),     # Change names in ASE_App
                          c("DateCal.IN"   ,"Cal.IN"),       # Change names in ASE_App
                          c("DateCal.END"  ,"Cal.END"),      # Change names in ASE_App
                          c("DateMeas.IN"  ,"Meas.IN"),      # Change names in ASE_App
                          c("DateMeas.END" ,"Meas.END")
    )
    if (any(Change.names[,1] %in% names(sens2ref))) {
        Names2Change <- which(Change.names[,1] %in% names(sens2ref))
        for (k in Names2Change) colnames(sens2ref)[colnames(sens2ref) == Change.names[k,1]] <- Change.names[k,2]
        # set to save file
        Save.sens2ref <- TRUE}
    # adding "Meas.IN" "Meas.END"if missing
    if (!"Meas.IN"  %in% names(sens2ref)) {
        sens2ref <-  cbind(sens2ref, sens2ref[,Valid.IN])
        data.table::setnames(sens2ref, length(names(sens2ref)), "Meas.IN")
        # set to save file
        Save.sens2ref <- TRUE}
    if (!"Meas.END" %in% names(sens2ref)) {
        sens2ref <-  cbind(sens2ref, sens2ref[,Valid.END])
        data.table::setnames(sens2ref, length(names(sens2ref)), "Meas.END")
        # set to save file
        Save.sens2ref <- TRUE}
    # coerce Sens.Inval.Out  and "Apply.Invalid" to logical
    Vector.type <- c("Sens.Inval.Out", "Apply.Invalid")
    Vector.type <- Vector.type[Vector.type %in% names(sens2ref)]
    for (j in Vector.type) data.table::set(sens2ref, j = j, value = as.logical(gsub(" ","",sens2ref[[j]])))
    # coerce chr of dates to POSIXct with Time Zone of General
    Vector.type <- c("Ref.IN"     , "Ref.END",
                     "Sens.IN"    , "Sens.END",
                     "Valid.IN"   , "Valid.END",
                     "Cov.IN"     , "Cov.END",
                     "Cal.IN"     , "Cal.END",
                     "Meas.IN"    , "Meas.END")
    Vector.type <- Vector.type[Vector.type %in% names(sens2ref)]
    for (k in Vector.type) {
        data.table::set(sens2ref, j = k, value = as.Date(sens2ref[[k]]))}
    # Checking validity of dates and correcting if needed
    if ((lubridate::is.POSIXct(DownloadSensor$DateIN.General.prev) || lubridate::is.POSIXct(DownloadSensor$DateIN.Influx.prev) ||
         lubridate::is.POSIXct(DownloadSensor$DateIN.SOS.prev) || lubridate::is.POSIXct(DownloadSensor$DateIN.Ref.prev))
        && (lubridate::is.POSIXct(DownloadSensor$DateEND.General.prev) || lubridate::is.POSIXct(DownloadSensor$DateEND.Influx.prev) ||
            lubridate::is.POSIXct(DownloadSensor$DateEND.SOS.prev) || lubridate::is.POSIXct(DownloadSensor$DateEND.Ref.prev))) {
        if (lubridate::is.POSIXct(DownloadSensor$DateIN.General.prev)) DateIN <- DownloadSensor$DateIN.General.prev else {
            if (lubridate::is.POSIXct(DownloadSensor$DateIN.Influx.prev)) DateIN <- DownloadSensor$DateIN.Influx.prev else {
                if (lubridate::is.POSIXct(DownloadSensor$DateIN.SOS.prev)) DateIN <- DownloadSensor$DateIN.SOS.prev else {
                    if (lubridate::is.POSIXct(DownloadSensor$DateIN.Ref.prev)) DateIN <- DownloadSensor$DateIN.Ref.prev}}}
        if (lubridate::is.POSIXct(DownloadSensor$DateEND.General.prev)) DateEND <- DownloadSensor$DateEND.General.prev else {
            if (lubridate::is.POSIXct(DownloadSensor$DateEND.Influx.prev)) DateEND <- DownloadSensor$DateEND.Influx.prev else {
                if (lubridate::is.POSIXct(DownloadSensor$DateEND.SOS.prev)) DateEND <- DownloadSensor$DateEND.SOS.prev else {
                    if (lubridate::is.POSIXct(DownloadSensor$DateEND.Ref.prev)) DateEND <- DownloadSensor$DateEND.Ref.prev}}}
        if (any(is.na(sens2ref$Valid.IN))) {
            sens2ref$Valid.IN[which(is.na(sens2ref$Valid.IN))]      <- DateIN
            # set to save file
            Save.sens2ref <- TRUE}
        if (any(sens2ref$Valid.IN < DateIN)) {
            sens2ref$Valid.IN[which(sens2ref$Valid.IN < DateIN)]    <- DateIN
            # set to save file
            Save.sens2ref <- TRUE}
        if (any(is.na(sens2ref$Valid.END))) {
            sens2ref$Valid.END[which(is.na(sens2ref$Valid.END))]    <- DateEND
            # set to save file
            Save.sens2ref <- TRUE}
        if (any(sens2ref$Valid.END < DateEND)) {
            sens2ref$Valid.END[which(sens2ref$Valid.END < DateEND)] <- DateEND
            # set to save file
            Save.sens2ref <- TRUE}
        Check_Dates.IN <- c("Ref.IN", "Sens.IN", "Cov.IN", "Cal.IN", "DatePlotCal.IN", "Meas.IN", "DatePlotMeas.IN")
        for (i in Check_Dates.IN) {
            if (any(is.na(sens2ref[[i]]))) {
                sens2ref[[i]][which(is.na(sens2ref[[i]]))]          <- sens2ref$Valid.IN[which(is.na(sens2ref[[i]]))]
                # set to save file
                Save.sens2ref <- TRUE}
            Out.range <- which(sens2ref[[i]] < sens2ref$Valid.IN | sens2ref[[i]] > sens2ref$Valid.END)
            if (length(Out.range) > 0) {
                sens2ref[[i]][Out.range]  <- sens2ref$Valid.IN[Out.range]
                # set to save file
                Save.sens2ref <- TRUE}}
        Check_Dates.END <- c("Ref.END", "Sens.END", "Meas.END", "Cal.END", "DatePlotCal.END", "Meas.END", "DatePlotMeas.END")
        for (i in Check_Dates.END) {
            if (any(is.na(sens2ref[[i]]))) {
                sens2ref[[i]][which(is.na(sens2ref[[i]]))]          <- sens2ref$Valid.END[which(is.na(sens2ref[[i]]))]
                # set to save file
                Save.sens2ref <- TRUE}
            Out.range <- which(sens2ref[[i]] < sens2ref$Valid.IN | sens2ref[[i]] > sens2ref$Valid.END)
            if (length(Out.range) > 0) {
                sens2ref[[i]][Out.range] <- sens2ref$Valid.END[Out.range]
                # set to save file
                Save.sens2ref <- TRUE}}
    }
    if (Save.sens2ref) {
        # Saving config file
        data.table::fwrite(data.table::setDT(as.data.frame(t(sens2ref)), keep.rownames = "name.gas")[], file = File_SETTIME_cfg, row.names = FALSE,col.names = FALSE)
        futile.logger::flog.info(paste0("[SETTIME] ", ASE.name," saving the ASE_name_SETTIME.cfg with correct name of sensors."))}
    cat("-----------------------------------------------------------------------------------\n")
    return(sens2ref)
}
#=====================================================================================CR
# ReadLastLines Function to read the last lines of the log file (Vs 1701210) ####
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
# panel.smooth, panel.cor, panel.hist Functions to plot correlation matrix (Vs 1701210) ####
#=====================================================================================CR
panel.orth <- function(x, y, col = "blue", bg = NA, pch = 1, cex = 0.6, col.smooth = "red", span = 2/3, iter = 3, ...) {
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) {
        m2  <- MethComp::Deming(x = x, y = y, vr = 1)
        abline(a=m2[1], b = m2[2], col = col.smooth, ...)
    }}
panel.Linear <- function(x, y, col = "blue", bg = NA, pch = 1, cex = 0.6, col.smooth = "red", span = 2/3, iter = 3, ...) {
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) {
        LM <- lm(y ~ x)
        abline(a=coef(LM)[1], b = coef(LM)[2], col = col.smooth, ...)
    }}
panel.smooth <- function(x, y, col = "blue", bg = NA, pch = 1, cex = 0.6, col.smooth = "red", span = 2/3, iter = 3, ...) {
    #For panel.smooth() function defined cex=, col = and pch = arguments.
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
        lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
              col = col.smooth, ...)}
panel.cor <- function(x, y, digits=3, prefix = "", cex.cor = 2) {
    usr <- par("usr"); on.exit(par(usr=usr))
    par(usr = c(0, 1, 0, 1))
    DataXY <- data.table::data.table(x = x, y = y)
    DataXY <- DataXY[is.finite(rowSums(DataXY))]
    lm.r <- lm(y ~ x, data =  DataXY)
    txt <- format(c(summary(lm.r)$r.squared, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (missing(cex.cor)) cex <- 0.8/strwidth(txt) else cex = cex.cor
    text(0.5, 0.75, txt, cex = cex * 0.5 * sqrt(summary(lm.r)$r.squared) + cex * 0.5)
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
    usr <- par("usr"); on.exit(par(usr=usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "red", ...)
}
#=====================================================================================CR
# dateFormat, dateStep, max.dateRange and min.dateRange Functions format, min, max, steps for Shiny sliderInput ####
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
    # Value1      : Posix, default null, minimum date to use fo an input slider
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
#=====================================================================================CR
# AboutVersions Functions Return a chacracter vector with all lines between FirstLineText and LastLineText for Shiny  ####
#=====================================================================================CR
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
            values[["DF"]] <- data.table::setnames(cbind(DF, newcolumn, stringsAsFactors=FALSE), c(names(DF), isolate(input$newcolumnname)))
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
### Target.Diagram: Function to plot a Modified U Target Diagram (Vs 180428) ####
#================================================================CR
#'
#'
#' @description  Get Critical Value, Information Threshold, Alert Threshold, Limit Value, Upper Assessment Threshold, Lower Assessment Threshold and Data Quality Objectives from name of the molecule, the molecule or the sensor name according to what is set into the European Air Quality Directive (2008). ---
#' @param name.gas,name.sensor,gas.sensor character strings, default is NULL. One of them shall not be NULL, Molecule or pollutant symbol (CH4, CO, CO2, NO, NO2, O3, PM1, PM1, PM10, PM2.5),
#'  brand name of sensor (CO_A4_P1, D300, NO_B4_P1, NO2_B43F_P1, OX_A431_P1, 5301CAT, 5301CST, OPCN3PM1, 5310CAT, 5310CST, OPCN3PM10, 5325CAT, 5325CST, OPCN3PM25) and
#'   molecule name("Carbon_monoxide", "Nitrogen_dioxide", "Nitric_oxide", "Ozone", "Sulphur_dioxide", "Benzene", "Particulate_Matter_10", "PM10_PMSCal", "Particulate_Matter_25", "PM25_PMSCal", "Carbon_dioxide")
#' @param Averaging.Period  character, Averaging periods as defined in the European Air Quality Directive. 
#' The Averaging Period changes the LV of pollutants. Values can be: "1hour", "8hour" and "1year". Default is "1hour"
#' @param unit.ref  character, units in which parameters are returned. It can be: "ug/m3", "mg/m3", "ppb" or "ppm". Default is "ug/m3".
#' @param Candidate  character, Data Quality Objectives as stated in the European Air Quality. It can be "Sensor" or "Model". Default is "Sensor"
#' According to Candidate the percentage of DQO changes. For "Sensor", the DQO of the CENT TC264/WG42 are used. For "Model" the DQO of the Air Quality Directive are used.
#' @return a list with , CL, IT, AT, LV, LAT, UAT, DQO.1, DQO.2, DQO.3 with NA for undefined parameters. They are returned in unit given by unit.ref.
#' @examples
#'

name.gases_name.sensors <- function(){
    return(
        list(NO2         = c("NO2_B43F_P1", "NO2_A43F_P1","no2_b43f", "NO2-B43F", "NO2B43F", "NO2_B43",  "NO2_M20",    "NO2_C1",  "NO2_C25", "NO2/C-20", "NO2_3E50",  "NO23E50",      "NO2",      "S1"),
             CO          = c("CO_A4_P1"   ,    "CO_B4_P1",   "CO-B4",    "CO-A4",    "COA4", "COMF200", "CO_MF200", "CO/MF-200", "CO/MF-20", "CO-MF200",  "CO_C200", "CO_CF200", "CO_3E300", "CO3E300",     "CO","CO-A4 O", "S2"),
             O3          = c("OX_A431_P1" ,  "OX_B431_P1",  "O3/M-5",    "O3-B4", "AX-A431", "OX-A431",  "OX_A431",   "O3-A431",    "O3_M5",    "O3_C5",  "O3_C100", "O3-M5",      "o3_m_5", "O3_3E1F", "O33EF1",     "O3", "O3E100", "S3"),
             NO          = c("NO_B4_P1"   ,    "NO_A4_P1",   "NO-B4",  "NOB4_P1",    "NOB4",  "NO_M25",    "NO_C1",    "NO_C25",  "NO/C-25",  "NO3E100", "NO_3E100",    "NO",   "No Sensor",      "S4"),
             SO2         = c("SO2_B4_P1"  ,  "SO2_A4_P1" , "SO2_M20", "SO2_MF20",  "SO2_C1", "SO2_C20", "SO2_CF20"),
             NH3         = c("NH3_MR100"  , "NH3_CR50") ,
             MOx         = "MOX",
             CO2         = "D300",
             PM4         = c("S30PM40"),
             PM1         = c("5301CAT", "5301CST", "OPCN3PM1","NPMPM01", "S30PM01"),
             PM2.5       = c("5325CAT", "5325CST", "OPCN3PM25", "NPMPM25", "S30PM25"),
             PM10        = c("5310CAT", "5310CST", "OPCN3PM10","NPMPM10", "S30PM10"),
             Radon       = "RD200M",
             RH          = c("SHT31HE", "Humid", "RH0", "SHT31HI"),
             Temp        = c("SHT31TE", "Tempe", "Temp", "TRH0", "TNTC0", "TNTC1", "TICHMBR", "SHT31TI"),
             Press       = c("Press", "BMP280", "PSEN0", "Patm"),
             SPLCPC      = c("SPLCPC", "UFSPIR"),
             LPLCPC      = c("LPLCPC", "UFLPIR"),
             H2O         = c("MPLCPC", "UFMPIR")
        ))
} # Add new sensor model to be recognized if needed
name.gases_gas.sensors <- function(){
    return(list(NO2 = c("Nitrogen_dioxide"),
                CO = c("Carbon_monoxide"),
                O3 = c("Ozone"),
                NO = c("Nitric_oxide"),
                CO2 = c("Carbon_dioxide"),
                PM1 = c("PM1_PMSCal", "PM1_PMSraw", "Particulate_Matter_1", "S30PM01","NPMPM01"),
                PM10 = c("PM10_PMSCal", "PM10_PMSraw", "Particulate_Matter_10","S30PM10", "NPMPM25"),
                PM2.5 = c("PM25_PMSCal", "PM25_PMSraw", "Particulate_Matter_25", "S30PM25", "NPMPM10"),
                MPLCPC = c("K96_MPLCPC"),
                #MPLCPC = c("K96_Water_vapour"),
                LPLCPC = c("K96_LPLCPC"),
                SPLCPC = c("K96_SPLCPC"),
                SHT31HE = c("Relative_humidity"),
                SHT31HI = c("Relative_humidity_int"),
                SHT31TE = c("Temperature"),
                SHT31TI = c("Temperature_int"),
                BMP280 = c(" Atmospheric_pressure")))
} # Add new sensor model to be recognized if needed

########################################
# Function name.Sensors_gas.sensors ####
########################################
#' @return a data.table with 1st column "name.sensor" ("NO_B4_P1") and 2nd column gas.sensors ("Carbon_monoxide", "Ozone", ...)
name.Sensors_gas.sensors <- function(){
    Mat <- data.table::data.table(rbind(c("NO2_B43F_P1", "Nitrogen_dioxide"),
                                        c("CO_A4_P1"   , "Carbon_monoxide"),
                                        c("OX_A431_P1" , "Ozone"),
                                        c("NO_B4_P1"   , "Nitric_oxide"),
                                        c("D300"       , "Carbon_dioxide"),
                                        c("5301CAT"    , "PM1_PMSCal"),
                                        c("5301CST"    , "PM1_PMSraw"),
                                        c("OPCN3PM1"   , "Particulate_Matter_1"),
                                        c("S30PM01"    , "S30PM01"),
                                        c("NPMPM01"    , "NPMPM01"),
                                        c("5310CAT"    , "PM10_PMSCal"),
                                        c("5310CST"    , "PM10_PMSraw"),
                                        c("OPCN3PM10"  , "Particulate_Matter_10"),
                                        c("S30PM10"    , "S30PM10"),
                                        c("NPMPM10"    , "NPMPM10"),
                                        c("5325CAT"    , "PM25_PMSCal"),
                                        c("5325CST"    , "PM25_PMSraw"),
                                        c("OPCN3PM25"  , "Particulate_Matter_25"),
                                        c("S30PM25"    , "S30PM25"),
                                        c("NPMPM25"    , "NPMPM25"),
                                        c("MPLCPC"     , "K96_MPLCPC"),
                                        #c("MPLCPC"     , "K96_Water_vapour"),
                                        c("LPLCPC"     , "K96_LPLCPC"),
                                        c("SPLCPC"     , "K96_SPLCPC"),
                                        c("SHT31HE"    , "Relative_humidity"),
                                        c("SHT31HI"    , "Relative_humidity_int"),
                                        c("SHT31TE"    , "Temperature"),
                                        c("SHT31TI"    , "Temperature_int"),
                                        c("BMP280"     , " Atmospheric_pressure")))
    DT <- data.table::data.table(Mat)
    data.table::setnames(DT, c("name.sensors", "gas.sensors"))
    return(DT)}

########################################
# Function gas.sensors_name.gases_name.gas ####
########################################
#' @return a data.table with 1st column gas.sensors ("Carbon_monoxide", "Ozone", ...), 2nd column "name.sensor" ("NO_B4_P1") and 3rd column name.gas ("No2", "O3", "PM1", "bin1" ...)
gas.sensors_name.gases_name.gas <- function(){
    # in the following list each element name is "gas.sensor", the 1st name of the vector of each element is "name.gas" and the rest of the vector is "name.sensor"
    gas.sensors_name.sensors <- list(Nitrogen_dioxide      = c("NO2"  , "NO2_B43F_P1", "NO2_A43F_P1","no2_b43f", "NO2-B43F", "NO2B43F", "NO2_B43","NO2_M20", "NO2_C1", "NO2_C25", "NO2/C-20", "NO2_3E50", "NO23E50", "NO2", "S1"),
                                     Carbon_monoxide       = c("CO"   , "CO_A4_P1"   , "CO_B4_P1","CO-B4", "CO-A4",  "COA4", "COMF200", "CO_MF200","CO/MF-200", "CO/MF-20", "CO-MF200", "CO_C200", "CO_CF200", "CO_3E300","CO3E300", "CO","CO-A4 O", "S2"),
                                     Ozone                 = c("O3"   , "OX_A431_P1" , "OX_B431_P1","O3/M-5", "O3-B4", "AX-A431", "OX-A431", "OX_A431", "O3-A431", "O3_M5", "O3_C5", "O3_C100", "O3-M5", "o3_m_5", "O3_3E1F", "O33EF1", "O3", "O3E100", "S3"),
                                     Nitric_oxide          = c("NO"   , "NO_B4_P1"   , "NO_A4_P1"  , "NO-B4", "NOB4_P1","NOB4", "NO_M25", "NO_C1", "NO_C25","NO/C-25", "NO3E100", "NO_3E100", "NO", "No Sensor", "S4"),
                                     Sulfur_dioxide        = c("SO2"  , "SO2_B4_P1"  , "SO2_A4_P1" , "SO2_M20", "SO2_MF20", "SO2_C1", "SO2_C20", "SO2_CF20"),
                                     Ammonia               = c("NH3"  , "NH3_MR100"  , "NH3_CR50") ,
                                     Particulate_Matter_1  = c("PM1"  , "OPCN2PM1"   , "OPCN3PM1") ,
                                     Particulate_Matter_25 = c("PM2.5", "OPCN2PM25"  , "OPCN3PM25"),
                                     Particulate_Matter_10 = c("PM10", "OPCN2PM10"  , "OPCN3PM10"),
                                     Bin0                  = c("Bin0" , "OPCN2Bin0"  , "OPCN3Bin0"),
                                     Bin1                  = c("Bin1" , "OPCN2Bin1"  , "OPCN3Bin1"),
                                     Bin2                  = c("Bin2" , "OPCN2Bin2"  , "OPCN3Bin2"),
                                     Bin3                  = c("Bin3" , "OPCN2Bin3"  , "OPCN3Bin3"),
                                     Bin4                  = c("Bin4" , "OPCN2Bin4"  , "OPCN3Bin4"),
                                     Bin5                  = c("Bin5" , "OPCN2Bin5"  , "OPCN3Bin5"),
                                     Bin6                  = c("Bin6" , "OPCN2Bin6"  , "OPCN3Bin6"),
                                     Bin7                  = c("Bin7" , "OPCN2Bin7"  , "OPCN3Bin7"),
                                     Bin8                  = c("Bin8" , "OPCN2Bin8"  , "OPCN3Bin8"),
                                     Bin9                  = c("Bin9" , "OPCN2Bin9"  , "OPCN3Bin9"),
                                     Bin10                 = c("Bin10", "OPCN2Bin10" , "OPCN3Bin10"),
                                     Bin11                 = c("Bin11", "OPCN2Bin11" , "OPCN3Bin11"),
                                     Bin12                 = c("Bin12", "OPCN2Bin12" , "OPCN3Bin12"),
                                     Bin13                 = c("Bin13", "OPCN2Bin13" , "OPCN3Bin13"),
                                     Bin14                 = c("Bin14", "OPCN2Bin14" , "OPCN3Bin14"),
                                     Bin15                 = c("Bin15", "OPCN2Bin15" , "OPCN3Bin15"),
                                     Bin16                 = c("Bin16", "OPCN3Bin16"),
                                     Bin17                 = c("Bin17", "OPCN3Bin17"),
                                     Bin18                 = c("Bin18", "OPCN3Bin18"),
                                     Bin19                 = c("Bin19", "OPCN3Bin19"),
                                     Bin20                 = c("Bin20", "OPCN3Bin20"),
                                     Bin21                 = c("Bin21", "OPCN3Bin21"),
                                     Bin22                 = c("Bin22", "OPCN3Bin22"),
                                     Bin23                 = c("Bin23", "OPCN3Bin23"),
                                     OPCHum                = c("OPCHum", "OPCHum", "OPCN3Hum"),
                                     OPCLsr                = c("OPCLsr", "OPCLsr", "OPCN3Lsr"),
                                     OPCTsam               = c("OPCTsam", "OPCTsam", "OPCN3TSam"),
                                     OPCVol                = c("OPCVol", "OPCVol", "OPCN2Vol" , "OPCN3Vol"),
                                     OPCTemp               = c("OPCTemp", "OPCN2Temp", "OPCN3Temp"),
                                     OPCFlow               = c("OPCFlow", "OPCFlow", "OPCN3FRt"),
                                     MOx                   = c("MOx", "MOX"),
                                     Carbon_dioxide        = c("CO2", "D300"),
                                     Bin1_PMS              = c("Bin0", "53PT003"),
                                     Bin2_PMS              = c("Bin1", "53PT005"),
                                     Bin3_PMS              = c("Bin2", "53PT010"),
                                     Bin4_PMS              = c("Bin3", "53PT025"),
                                     Bin5_PMS              = c("Bin4", "53PT050"),
                                     Bin6_PMS              = c("Bin5", "53PT100", "53P"),
                                     PM1_PMSraw            = c("PM1", "5301CST"),
                                     PM1_PMSCal            = c("PM1", "5301CAT"),
                                     PM25_PMSraw           = c("PM2.5", "5325CST"),
                                     PM25_PMSCal           = c("PM2.5", "5325CAT"),
                                     PM10_PMSraw           = c("PM10", "5310CST"),
                                     PM10_PMSCal           = c("PM10", "5310CAT"),
                                     Radon                 = c("RD200M", "RD200M"),
                                     Relative_humidity     = c("RH",  "SHT31HE", "Humid"),                                                            
                                     Temperature           = c("Temp", "SHT31TE", "Tempe", "Temp"),
                                     Atmospheric_pressure  = c("Press", "Press", "BMP280"),
                                     Temperature_int       = c("Temp_int","SHT31TI"),
                                     Relative_humidity_int = c("RH_int", "SHT31HI"),
                                     Batt_Cumulated_Charge = c("L2942CUR", "L2942CUR"),
                                     Batt_Voltage          = c("L2942VOL", "L2942VOL"),
                                     Batt_Charge_Status    = c("L4156STA", "L4156STA"),
                                     # K96_Carbon_dioxide    = "SPLCPC",
                                     # K96_Methane           = "LPLCPC",
                                     K96_SPLCPC            = c("SPLCPC", "SPLCPC"),
                                     K96_LPLCPC            = c("LPLCPC", "LPLCPC"),
                                     #K96_Water_vapour      = c("MPLCPC", "MPLCPC"),
                                     K96_MPLCPC            = c("MPLCPC", "MPLCPC"),
                                     K96_UFSPIR            = c("UFSPIR", "UFSPIR"),
                                     K96_UFLPIR            = c("UFLPIR", "UFLPIR"),
                                     K96_UFMPIR            = c("UFMPIR", "UFMPIR"),
                                     K96_Temperature       = c("TRH0", "TRH0"),
                                     K96_Relative_humidity = c("RH0", "RH0"),
                                     K96_Atmospheric_pressure = c("PSEN0", "PSEN0"),
                                     K96_NTC0                 = c("TNTC0", "PSEN0"),
                                     K96_NTC1                 = c("TNTC1", "TNTC1"),
                                     K96_Chamber_Temperature  = c("TICHMBR", "TICHMBR"),
                                     K96_Error                = c("ERRST", "ERRST"))
    return(data.table::rbindlist(lapply(names(gas.sensors_name.sensors), function(gas.sensor){
        name.sensor <- gas.sensors_name.sensors[[gas.sensor]]
        name.gas    <- name.sensor[1]
        name.sensor <- name.sensor[-1]
        data.table::data.table(gas.sensor = rep(gas.sensor, length(name.gas)), name.sensor = name.sensor, name.gas = rep(name.gas, length(name.gas)))})
    ))}

#=====================================================================================CR
# function get.DQO setting the requirequirements for sensor classification
#=====================================================================================CR
#' @description
#' The function get.DQO() returns a list of several elements: LV, UAT, LAT, AT, IT drawn from the European Air Quality Directive 2008/50/EC or 2024/2881/EC
#' and requirements for data capture, linear regression coefficients, RH factor, Coarse factor and Data Quality Objectives
#' drawn from EN TS 17660:2021 and CEN TS 17660-2:2024. 
#' TS 17660 changes a few average time of LV for a few pollutants, e.g. for CO and O3 the average time of the LV is set as 8 hours while TS 17660 set it to one hour and
#' for benzene TS 17660 change the averaging time from 1 year to one hour. In addition TS 17660 defines a LV and a DQO for NO drawn form NO2.
#' TS17660 was set when he European Air Quality Directive 2008/50/EC was into force. With the update of the European Air Quality Directive 2024/2881/EC, LVs, DQOs ... 
#' should change accordingly although this update remains a bit obscure.
#' @details
#' The identification of the selected pollutant is carried out using arguments in order of consideration: name.sensor, gas.sensor and name.gas.
#' At least one parameter among gas.sensor, name.sensor or name.gas shall be given in the argument list of the function get.DQO() 
#' in order to clearly identity the pollutant for which to return get.DQOs list of elements.
#' If passed in the list of argument of the function, the argument name.sensor shall be included into the column "name.sensors" of the returned data.table of function name.Sensors_gas.sensors().
#' If the name.sensor argument is given and is included in the accepted list of sensors given in function name.gases_name.sensors()
#' the list of elements is based on name.sensor to determine the pollutant DQO and requirements see "return".
#' The DQOs are expressed as real values, not in percentage. 
#' @param gas.sensor Not mandatory, character, default value = NULL. If TRUE PROXY is configured using the following 4 parameters:
#' @param name.sensor Not mandatory, character, default value NULL: name.sensor name shall be included into the returned vector of function name.Sensors_gas.sensors()
#' @param name.gas character, default value = NULL, login for the proxy server, JRC = NULL;
#' @param Averaging.Period character, default value = 1hour, Possible values "1hour", "8hour", "24hour","1year"
#' @param unit.ref character, default value = ppb, unit of reference values; value can be c("\u00b5g/m\u00b3","ug/m3", "mg/m3", "mg/m\u00b3", "ppb", "ppm", "percent", "Celsius", "hPa")
#' @param Ref.Pollutant character, default value Null, name of pollutant in ssymbole like name.gas; use in case name.sensor is SPLCPC or LPLCPC (for CO2 sensor)
#' @param Directive character string, default value is "EN TS 17660", possible values "EN TS 17660", "2008/50/EC" and "2024/2881/EC". It indicates if the returned values are drawn from Directive "2008/50/EC" or "2024/2881/EC"
#' @return  a list object with following elements:
#'      unit.ref units of DQOs and IT, AT , LV, UAT, LAT. Shall be in c("\u00b5g/m\u00b3","ug/m3", "mg/m3", "mg/m\u00b3", "ppb", "ppm", "percent", "Celsius", "hPa")
#'      Averaging.Period = Averaging.Period, values can be "1hour", "8hour", "24hour","1year" and should be the averaging time of the LV
#'      IT information thresholds, only for ozone, NA for other pollutants, in unit.ref.
#'      AT alert thresholds, only for sulphur dioxide and nitrogen dioxide, NA for other pollutants, in unit.ref.
#'      LV limit value or target value for ozone, defined in EN TS 17660:2021 or CEN TS 17660-2:2024, In unit.ref.
#'      UAT Upper assessment thresholds defined in the European Air Quality Directive 2008/50/EC, generally 50 % of LV. In unit.ref.
#'      LAT Lower assessment thresholds defined in the European Air Quality Directive 2008/50/EC, generally 50 % of LV. In unit.ref.
#'      DQO.0 Data Quality objective of fixed measurements defined in the European Air Quality Directive 2008/50/EC. In unit.ref.
#'      DQO.1 Data Quality objective of Class 1 sensor systems defined in EN TS 17660:2021 or CEN TS 17660-2:2024 (Indicative measurements). In unit.ref.
#'      DQO.2 Data Quality objective of Class 2 sensor systems defined in EN TS 17660:2021 or CEN TS 17660-2:2024 (Modelling and objective estimation). In unit.ref.
#'      DQO.3 Data Quality objective of Class 3 sensor systems defined in EN TS 17660:2021 or CEN TS 17660-2:2024 (not assocciated with mandatory Data Quality Objective). In unit.ref.
#'      Class.1, 2 and 3.t90 response time in min for Class 1, 2 and 3 sensor systems defined in EN TS 17660:2021 or CEN TS 17660-2:2024,
#'      Class.1, 2 and 3.Capture Minimum data capture in % for Class 1 sensor systems defined in EN TS 17660:2021 or CEN TS 17660-2:2024,
#'      Class.1, 2 and 3.ubss Maximum Between sensor system uncertainty in unit.ref for Class 1, 2 and 3 sensor systems defined in EN TS 17660:2021 or CEN TS 17660-2:2024,
#'      Class.1, 2 and 3.slp Interval of tolerance for the Slope of regression line for Class 1, 2 and 3 sensor systems defined in EN TS 17660:2021 or CEN TS 17660-2:2024,
#'      Class.1, 2 and 3.Int maximum absolute value in unit.ref of the intercept of regression line for Class 1, 2 and 3 sensor systems defined in EN TS 17660:2021 or CEN TS 17660-2:2024,
#' The DQOs are expressed as real values, not in percentage. 
#' @param LAT,UAT,LV,AT,CL numeric, lower and upper assessment threshold, limit value, Alert threshold and Critical level of the European Air Quality Directive for Mat$xis, same unit as Mat$xis, default value = NA, used for color scale and target circles
get.DQO <- function(gas.sensor = NULL, name.sensor = NULL, name.gas = NULL, Averaging.Period = "1hour", unit.ref = "ppb", Candidate = "Sensor",
                    Ref.Pollutant = NULL, Directive = "EN TS 17660") {
    # list of gas sensors
    # DT.gas <- data.table::data.table(name.gas    = c(       "H2O",        "CH4",             "CO",        "CO2",            "CO2",           "NO",              "NO2",         "O3",        "PM1",        "PM1",                  "PM1",            "PM10",        "PM10",                  "PM10",       "PM2.5",       "PM2.5",                 "PM2.5",                "RH",                "RH_int",        "Temp",        "Temp_int",                "Press"),
    #                                  name.sensor = c(    "MPLCPC",     "LPLCPC",       "CO_A4_P1",     "SPLCPC",           "D300",     "NO_B4_P1",      "NO2_B43F_P1", "OX_A431_P1",    "5301CAT",    "5301CST",             "OPCN3PM1",     "5310CAT",     "5310CST",             "OPCN3PM10",     "5325CAT",     "5325CST",             "OPCN3PM25",           "SHT31HE",               "SHT31HI",     "SHT31TE",         "SHT31TI",               "BMP280"),
    #                                  gas.sensor  = c("K96_MPLCPC", "K96_LPLCPC","Carbon_monoxide", "K96_SPLCPC", "Carbon_dioxide", "Nitric_oxide", "Nitrogen_dioxide",      "Ozone", "PM1_PMSCal", "PM1_PMSraw", "Particulate_Matter_1", "PM10_PMSCal", "PM10_PMSraw", "Particulate_Matter_10", "PM25_PMSCal", "PM25_PMSraw", "Particulate_Matter_25", "Relative_humidity", "Relative_humidity_int", "Temperature", "Temperature_int", "Atmospheric_pressure"))
    
    # Checking consistency of arguments
    if (!Directive %in% c("EN TS 17660", "2008/50/EC", "2024/2881/EC")){
        return(futile.logger::flog.error(paste0("[get.DQO] unknown legislative documnent ", Directive)))}
    if (!Averaging.Period %in% c("1hour", "8hour", "24hour","1year")) return(futile.logger::flog.error(paste0("[get.DQO] unknown Averaging.Period ", Averaging.Period)))
    if (!unit.ref %in% c("\u00b5g/m\u00b3","ug/m3", "mg/m3", "mg/m\u00b3", "ppb", "ppm", "percent", "Celsius", "hPa")){
        return(futile.logger::flog.error(paste0("[get.DQO] unknown unit.ref ", unit.ref)))}
    if (!Candidate %in% c("Sensor", "Model")) return(futile.logger::flog.error(paste0("[get.DQO] unknown Candidate type: ", Candidate)))
    
    # checking if the pollutant can be identified from name.gas, name.sensor or gas.sensor
    if (is.null(name.gas) && is.null(name.sensor) && is.null(gas.sensor) && is.null(Ref.Pollutant)){
        stop(futile.logger::flog.error("[get.DQO] either name.gas, name.sensor, Ref.Pollutant or gas.sensor shall be given"))
    } else if (!is.null(name.sensor)){
        stopifnot(any(grepl(name.sensor, name.gases_name.sensors())), name.sensor %in% name.Sensors_gas.sensors()$name.sensors)
        #gas.sensor <- name.Sensors_gas.sensors()[name.sensors == name.sensor]$gas.sensors
        if (!name.sensor %in% c("SPLCPC", "LPLCPC", "MPLCPC")){
            #name.gas   <-  names(name.gases_name.sensors())[grep(name.sensor, name.gases_name.sensors())]
            name.gas   <-  unlist(sapply(names(name.gases_name.sensors()), function(i, name.sensor){
                if (name.sensor %in% name.gases_name.sensors()[[i]]) return(i)
            }, name.sensor))
        } else {
            stopifnot(!is.null(Ref.Pollutant))
            name.gas   <- Ref.Pollutant}
    } else if (!is.null(gas.sensor)){
        stopifnot(gas.sensor %in% gas.sensors_name.gases_name.gas()[["gas.sensor"]])
        Good.gas.sensors <- which(gas.sensors_name.gases_name.gas()$gas.sensor == gas.sensor)
        name.sensor <- gas.sensors_name.gases_name.gas()[Good.gas.sensors]$name.sensor[1]
        if (!name.sensor %in% c("SPLCPC", "LPLCPC", "MPLCPC", "UFSPIR", "UFLPIR", "UFMPIR")){
            name.gas    <- gas.sensors_name.gases_name.gas()[Good.gas.sensors]$name.gas[1]
        } else {
            stopifnot(!is.null(Ref.Pollutant))
            name.gas   <- Ref.Pollutant}
    } else if (!is.null(name.gas)){
        stopifnot(name.gas %in% gas.sensors_name.gases_name.gas()[["name.gas"]] || name.gas %in% c("CH4","H2O", "CO2"))
        # stop(futile.logger::flog.error("[get.DQO] unknown name.gas"))
    }
    
    # Checking if gas sensors or name.gas is given
    if (!name.gas %in% c("H2O","CH4","CO", "CO2", "NO2", "NO", "O3", "SO2", "Benzene", "PM10", "PM1", "PM2.5", "RH", "RH_int", "Temp", "Temp_int", "Press", "Patm")) {
        return(futile.logger::flog.error(paste0("[get.DQO] unknown compound, ",name.gas," no DQO")))}
    
    # Initial values for LV, DQOs, UAT, LAT, TS17660 parameters
    Criterias <- c(
        # Nearly common parameters
        "DQO.0", "DQO.1", "DQO.2", "DQO.3",
        "Perc.DQO.0", "Perc.DQO.1", "Perc.DQO.2", "Perc.DQO.3",
        "Perc.UAT", "Perc.LAT",
        "IT", "AT", "LV", "UAT", "LAT",
        
        # Only defined in EN TS 17660
        "Class.1.t90",       "Class.2.t90",       "Class.3.t90",
        "Class.1.lof",       "Class.2.lof",       "Class.3.lof",
        "Class.1.r",         "Class.2.r",         "Class.3.r",
        "Class.1.ubss",      "Class.2.ubss",      "Class.3.ubss",
        "Class.1.Capture",   "Class.2.Capture",   "Class.3.Capture",
        "Class.1.RHfactor",  "Class.2.RHfactor",  "Class.3.RHfactor",
        "Class.1.slpCoarse", "Class.2.slpCoarse", "Class.3.slpCoarse",
        "Class.1.R2Coarse",  "Class.2.R2Coarse",  "Class.3.R2Coarse",
        "Class.1.slp",       "Class.2.slp",       "Class.3.slp",
        "Class.1.Int",       "Class.2.Int",       "Class.3.Int",
        
        # Only defined in 2024/2881/EC
        "ass.Threshold.health", "ass.Threshold.veg")
    for(Criteria in Criterias) assign(Criteria, NA)
    
    # Common parameters
    Avg.Period <- 60
    Class.1.Capture <- 90; Class.2.Capture <- 50
    
    # Parameters depending of pollutant and Directive where needed
    if (name.gas == "H2O") {
        # There is no real DQO, the following values are given to be consistent with the computation of the Code
        LV <- 10*1e6; Perc.LAT <- 0.50; Perc.UAT <- 0.70 # LV about 10 g/m³? expressed in µg/m³
        Perc.DQO.0  <- 0.05; Perc.DQO.1 <- 0.25; Perc.DQO.2 <- 0.75; Perc.DQO.3 <- 2.00
        
        # Conversion coefficient
        if (!unit.ref %in% c("\u00b5g/m\u00b3","ug/m3")){
            # Conversion coefficient
            if (unit.ref %in% c("mg/m3", "mg/m\u00b3")){
                Conv.Unit  <- 1000
            } else if (unit.ref == "ppb"){
                Conv.Unit <- 0.75
            } else if (unit.ref == "ppm"){
                Conv.Unit <- 0.75 * 1000
            } else stop(paste0("[get.DQO] ERROR Wrong unit for ",gas.sensor, ": ", unit.ref, "\n"))}
        
    } else if (name.gas == "CH4") {
        # There is no real DQO, thhe folllowing value are given to be consistent with the computation of the Code
        LV <- 2000 * 0.66; Perc.LAT <- 0.50; Perc.UAT <- 0.70 # LV 2000 ppm expressed in µg/m³
        Perc.DQO.0  <- 0.05; Perc.DQO.1 <- 0.25; Perc.DQO.2 <- 0.75; Perc.DQO.3 <- 2.00
        
        # Conversion coefficient
        if (!unit.ref %in% c("\u00b5g/m\u00b3","ug/m3")){
            # Conversion coefficient
            if (unit.ref %in% c("mg/m3", "mg/m\u00b3")){
                Conv.Unit  <- 1000
            } else if (unit.ref == "ppb"){
                Conv.Unit <- 0.66
            } else if (unit.ref == "ppm"){
                Conv.Unit <- 0.66 * 1000
            } else stop(paste0("[get.DQO] ERROR Wrong unit for ",gas.sensor, ": ", unit.ref, "\n"))}
        
    } else if (name.gas == "CO2") {
        
        if(Directive == "EN TS 17660"){
            # Parameters that are not dependent on Directive but on TS17660-1, defined in µg/m³, taken from PM2.5
            # Convert to µg/m³ at 20°C using https://www.gastec.co.jp/en/technology/knowledge/concentration/
            Class.1.t90  <- 6;             Class.2.t90     <- 15;            Class.3.t90  <- 15;
            Class.1.lof  <- 65887;         Class.2.lof     <- 98831;         Class.3.lof  <- 98831;
            Class.1.r    <- 25623;         Class.2.r       <- 36604;         Class.3.r    <- 73208;
            Class.1.ubss <- 18302;         Class.2.ubss    <- 36604;         Class.3.ubss <- 54906
            Class.1.slp  <- c(0.78, 1.29); Class.2.slp     <- c(0.60, 1.67); Class.3.slp  <- c(0.43, 2.33)
            Perc.DQO.3 <- 2 #set only in "EN TS 17660"
            # Convert from ppm to µg/m³ https://www.gastec.co.jp/en/technology/knowledge/concentration/ at 20°C for 10, 20 and 30 ppm of CO2
            LV    <- 914074
            Perc.DQO.1 <- 0.30; Perc.DQO.2 <- 0.50; Perc.DQO.3 <- 1.00
        } else if(Directive == "2008/50/EC"){
            stop(paste0(" ERROR Unknown LV and DQO for averaging time ", Averaging.Period))
        } else if(Directive == "2024/2881/EC"){
            stop(paste0(" ERROR Unknown LV and DQO for averaging time ", Averaging.Period))}
        
        # Conversion coefficient
        if (!unit.ref %in% c("\u00b5g/m\u00b3","ug/m3")){
            # Conversion coefficient
            if (unit.ref %in% c("mg/m3", "mg/m\u00b3")){
                Conv.Unit  <- 1000
            } else if (unit.ref == "ppb"){
                Conv.Unit <- 1.828
            } else if (unit.ref == "ppm"){
                Conv.Unit <- 1.828 * 1000
            } else stop(paste0("[get.DQO] ERROR Wrong unit for ",gas.sensor, ": ", unit.ref, "\n"))}
        
    } else if (name.gas %in% c("RH", "RH_int")) {
        # Using LV for 24 hours time average
        LV    <- 50
        Perc.DQO.0  <- 0.05; Perc.DQO.1 <- 0.30; Perc.DQO.2 <- 0.50; Perc.DQO.3 <- 1.00
    } else if (name.gas %in% c("Temp","Temp_int")) {
        # Using LV for 24 hours time average
        LV    <- 15
        Perc.DQO.0  <- 0.05; Perc.DQO.1 <- 0.30; Perc.DQO.2 <- 0.50; Perc.DQO.3 <- 1.00
    } else if (name.gas == "Patm") {
        # Using LV for 24 hours time average
        LV    <- 1013
        Perc.DQO.0  <- 0.05; Perc.DQO.1 <- 0.30; Perc.DQO.2 <- 0.50; Perc.DQO.3 <- 1.00
    } else if (name.gas %in% c("SO2", "NO2", "NO","CO", "O3", "Benzene", "PM1", "PM2.5", "PM10")){
        
        if(Directive == "EN TS 17660"){
            Class.1.slp <- c(0.78, 1.29); Class.2.slp <- c(0.60, 1.67); Class.3.slp <- c(0.43, 2.33)
            Perc.DQO.3 <- 2 #set in "EN TS 17660" not in the European Directives
            if (name.gas %in% c("SO2", "NO2", "NO","CO")){
                Perc.DQO.1 <- 0.25; Perc.DQO.2 <- 0.75
            } else if (name.gas == "O3"){
                Perc.DQO.1 <- 0.30; Perc.DQO.2 <- 0.75
            } else if (name.gas == "Benzene"){
                Perc.DQO.1 <- 0.30; Perc.DQO.2 <- 1.00
                Perc.UAT <- 0.70; Perc.LAT <- 0.40
            } else if (name.gas %in% c("PM1", "PM2.5", "PM10")){
                Perc.DQO.1 <- 0.50; Perc.DQO.2 <- 1.00
                Perc.UAT <- 0.70; Perc.LAT <- 0.50}
        } else if(Directive == "2008/50/EC"){
            if (name.gas %in% c("SO2", "NO2", "CO", "O3")){
                Perc.DQO.0  <- 0.15; Perc.DQO.1 <- 0.25; Perc.DQO.2 <- 0.75
            } else if (name.gas == "Benzene"){
                Perc.DQO.0  <- 0.25; Perc.DQO.1 <- 0.50; Perc.DQO.2 <- 1.00
                Perc.UAT    <- 0.70; Perc.LAT <- 0.50
            } else if (name.gas %in% c("PM1", "PM2.5", "PM10")){
                Perc.DQO.0  <- 0.25; Perc.DQO.1 <- 0.50; Perc.DQO.2 <- 1.00
                Perc.UAT    <- 0.70; Perc.LAT <- 0.50
            } else if (name.gas %in% c("O3")){
                Perc.DQO.0  <- 0.15; Perc.DQO.1 <- 0.30; Perc.DQO.2 <- 0.75
                Perc.UAT <- 0.70; Perc.LAT <- 0.50
            } else stop(paste0("Unknown pollutant ", name.gas, " in European Directive 2008/50/EC")) # For NO
        } else if(Directive == "2024/2881/EC"){
            Perc.DQO.0  <- 0.15; Perc.DQO.1 <- 0.25; Perc.DQO.2 <- min(0.85, Perc.DQO.1 * 3.2)
            Perc.DQO.2 <-  min(0.85, Perc.DQO.1 * 4.9)
            UAT   <-  ass.Threshold.health
            Perc.LAT   <-  0.50}
        
        if (name.gas == "SO2"){
            
            # Parameters that are not dependent on Directive but on TS17660-1, defined in µg/m³
            Class.1.ubss <- 10.6; Class.2.ubss <- 21 ; Class.3.ubss <- 43
            Class.1.Int  <- 1.4 ; Class.2.Int  <- 2.5; Class.3.Int  <- 4.5
            
            # Parameters which normally depend on Directive units in µg/m³
            if(Directive == "EN TS 17660"){
                if(Averaging.Period == "1hour"){
                    Avg.Period <-  60
                    LV <- 350; AT <- 500
                } else if(Averaging.Period =="24hour"){
                    Avg.Period <-  24 * 60
                    LV <- 125
                    Perc.UAT <- 0.60; Perc.LAT <- 0.40
                } else stop(paste0(" ERROR Unknown LV and DQO for averaging time ", Averaging.Period))
            } else if(Directive == "2008/50/EC"){
                if(Averaging.Period == "1hour"){
                    LV <-  350;  AT <-  500
                } else if(Averaging.Period =="24hour"){
                    LV <-  125; Avg.Period <-  1440
                    Perc.UAT <- 0.60; Perc.LAT <- 0.40
                } else stop(paste0(" ERROR Unknown LV and DQO for averaging time ", Averaging.Period))
            } else if(Directive == "2024/2881/EC"){
                if(Averaging.Period == "1hour"){
                    LV <-  350
                } else if(Averaging.Period =="24hour"){
                    LV <-  50; Avg.Period <-  1440
                } else if(Averaging.Period =="1year"){
                    LV <-  20; Avg.Period <-  60 * 24 * 365
                } else stop(paste0(" ERROR Unknown LV and DQO for averaging time ", Averaging.Period))
                Perc.DQO.2 <- min(0.85, Perc.DQO.1 * 3.2)
                ass.Threshold.health <- 4000
                UAT <- ass.Threshold.health}
            
            # Conversion coefficient
            if (!unit.ref %in% c("\u00b5g/m\u00b3","ug/m3")){
                # Conversion coefficient
                if (unit.ref %in% c("mg/m3", "mg/m\u00b3")){
                    Conv.Unit  <- 1000
                } else if (unit.ref == "ppb"){
                    Conv.Unit <- 2.666
                } else if (unit.ref == "ppm"){
                    Conv.Unit <- 2.666 * 1000
                } else stop(paste0("[get.DQO] ERROR Wrong unit for ",gas.sensor, ": ", unit.ref, "\n"))
            }
        } else if (name.gas == "NO2"){
            
            # Parameters that are not dependent on Directive but on TS17660-1, defined in µg/m³
            Class.1.ubss <- 7.6; Class.2.ubss <- 15; Class.3.ubss <- 31
            Class.1.Int  <- 9.8; Class.2.Int  <- 19; Class.3.Int  <- 33
            
            # Parameters which normally depend on Directive units in µg/m³
            if(Directive == "EN TS 17660"){
                if(Averaging.Period == "1hour"){
                    Avg.Period <-  60
                    LV <- 200; AT <- 400
                    Perc.UAT <- 0.70; Perc.LAT <- 0.50
                } else if(Averaging.Period =="24hour"){
                    # this LV is defined for 1 year in EN TS 17760. However as the test is never carried for a whole year it is attributed to 24 hours here
                    Avg.Period <-  24 * 60
                    LV <- 40
                    Perc.UAT <- 0.80; Perc.LAT <- 0.65
                } else stop(paste0(" ERROR Unknown LV and DQO for averaging time ", Averaging.Period))
            } else if(Directive == "2008/50/EC"){
                if(Averaging.Period == "1hour"){
                    Avg.Period <-  60
                    LV <- 200; AT <- 400
                    # Hourly limit value for the protection of human health (NO2)
                    Perc.UAT <- 0.70; Perc.LAT <- 0.50
                } else if(Averaging.Period =="24hour"){
                    LV <- 40
                    Avg.Period <-  24 * 60
                    # Annual limit value for the protection of human health (NO2)
                    Perc.UAT <- NA; Perc.LAT <- NA
                } else stop(paste0(" ERROR Unknown LV and DQO for averaging time ", Averaging.Period))
            } else if(Directive == "2024/2881/EC"){
                if(Averaging.Period == "1hour"){
                    LV <-  200
                } else if(Averaging.Period =="24hour"){
                    LV <-  50; Avg.Period <-  1440
                } else if(Averaging.Period =="1year"){
                    LV <-  20; Avg.Period <-  60 * 24 * 365
                } else stop(paste0(" ERROR Unknown LV and DQO for averaging time ", Averaging.Period))
                ass.Threshold.health <- 4000
                UAT   <- ass.Threshold.health}
            
            # Conversion coefficient
            if (!unit.ref %in% c("\u00b5g/m\u00b3","ug/m3")){
                # Conversion coefficient
                if (unit.ref %in% c("mg/m3", "mg/m\u00b3")){
                    Conv.Unit  <- 1000
                } else if (unit.ref == "ppb"){
                    Conv.Unit <- 1.91
                } else if (unit.ref == "ppm"){
                    Conv.Unit <- 1.91 * 1000
                } else stop(paste0("[get.DQO] ERROR Wrong unit for ",gas.sensor, ": ", unit.ref, "\n"))
            }
        } else if (name.gas == "NO") {
            
            # Parameters that are not dependent on Directive but on TS17660-1, defined in µg/m³
            Class.1.ubss <- 5.0; Class.2.ubss <- 10; Class.3.ubss <- 20
            Class.1.Int <- 6.4; Class.2.Int <- 12; Class.3.Int <- 22
            
            if(Directive == "EN TS 17660"){
                # As written in EN TS 17760 the DQO of NO2 are used
                if(Averaging.Period == "1hour"){
                    Avg.Period <-  60
                    LV <- 200; AT <- 400
                    Perc.UAT <- 0.70; Perc.LAT <- 0.50
                } else if(Averaging.Period =="24hour"){
                    # this LV is defined for 1 year in EN TS 17760. However as the test is never carried for a whole year it is attributed to 24 hours here
                    Avg.Period <-  24 * 60
                    LV <- 40
                    Perc.UAT <- 0.80; Perc.LAT <- 0.65
                } else stop(paste0(" ERROR Unknown LV and DQO for averaging time ", Averaging.Period))
            } else if(Directive == "2008/50/EC" || Directive == "2024/2881/EC"){
                stop(paste0("[get.DQO] ERROR Directive 2008/50/EC and 2024/2881/EC do not set LV and DQO for N0, change the argument Directive of funtion get.DQO()")) 
            }
            
            # Conversion coefficient
            if (!unit.ref %in% c("\u00b5g/m\u00b3","ug/m3")){
                # Conversion coefficient
                if (unit.ref %in% c("mg/m3", "mg/m\u00b3")){
                    Conv.Unit  <- 1000
                } else if (unit.ref == "ppb"){
                    Conv.Unit <- 1.25
                } else if (unit.ref == "ppm"){
                    Conv.Unit <- 1.25 * 1000
                } else stop(paste0("[get.DQO] ERROR Wrong unit for ",gas.sensor, ": ", unit.ref, "\n"))
            }
        } else if (name.gas == "CO"){
            
            # Parameters that are not dependent on Directive but on TS17660-1, defined in µg/m³
            Class.1.ubss <-  58; Class.2.ubss <- 116; Class.3.ubss <- 232
            Class.1.Int  <- 180; Class.2.Int  <- 250; Class.3.Int  <- 400
            
            # Parameters which normally depend on Directive units in µg/m³
            if(Directive == "EN TS 17660"){
                if(Averaging.Period %in% c("1hour", "8hour")){
                    Avg.Period <-  60
                    LV <- 10000
                    Perc.UAT <- 0.70; Perc.LAT <- 0.40
                } else stop(paste0(" ERROR Unknown LV and DQO for averaging time ", Averaging.Period))
            } else if(Directive == "2008/50/EC"){
                Perc.DQO.0  <- 0.15; Perc.DQO.1 <- 0.25; Perc.DQO.2 <- 0.75
                if(Averaging.Period == "8hour"){
                    LV <- 10000; Avg.Period <-  60 * 8
                } else stop(paste0(" ERROR Unknown LV and DQO for averaging time ", Averaging.Period))
            } else if(Directive == "2024/2881/EC"){
                if(Averaging.Period == "8hour"){
                    LV <-  10000
                } else if(Averaging.Period =="24hour"){
                    LV <-  4000; Avg.Period <-  1440
                } else stop(paste0(" ERROR Unknown LV and DQO for averaging time ", Averaging.Period))
                Perc.DQO.0  <- 0.10; Perc.DQO.1 <- 0.20; Perc.DQO.2 <- min(0.85, Perc.DQO.1 * 4.9)
                ass.Threshold.health <- 4000
                UAT   <- ass.Threshold.health}
            
            # Conversion coefficient
            if (!unit.ref %in% c("\u00b5g/m\u00b3","ug/m3")){
                # Conversion coefficient
                if (unit.ref %in% c("mg/m3", "mg/m\u00b3")){
                    Conv.Unit  <- 1000
                } else if (unit.ref == "ppb"){
                    Conv.Unit <- 1.34
                } else if (unit.ref == "ppm"){
                    Conv.Unit <- 1.34 * 1000
                } else stop(paste0("[get.DQO] ERROR Wrong unit for ",gas.sensor, ": ", unit.ref, "\n"))
            }
        } else if (name.gas == "O3") {
            
            # Parameters that are not dependent on Directive but on TS17660-1, defined in µg/m³
            Class.1.ubss <- 8.0; Class.2.ubss <- 12; Class.3.ubss <- 24
            Class.1.Int <- 9.8; Class.2.Int <- 19; Class.3.Int <- 33
            
            # Parameters which normally depend on Directive units in µg/m³
            if(Directive == "EN TS 17660"){
                if(Averaging.Period %in% c("1hour", "8hour")){
                    Avg.Period <-  60
                    LV <- 120; AT <- 240
                    Perc.UAT <- 0.70; Perc.LAT <- 0.50
                } else stop(paste0(" ERROR Unknown LV and DQO for averaging time ", Averaging.Period))
            } else if(Directive == "2008/50/EC"){
                Perc.DQO.1 <- 0.30
                if(Averaging.Period == "8hour"){
                    LV <- 120; Avg.Period <-  60 * 8
                    AT <- 240; IT <- 180; 
                } else stop(paste0(" ERROR Unknown LV and DQO for averaging time ", Averaging.Period))
            } else if(Directive == "2024/2881/EC"){
                AT <- 240; IT <- 180; 
                if(Averaging.Period == "8hour"){
                    LV <-  10000
                } else if(Averaging.Period =="24hour"){
                    LV <-  4000; Avg.Period <-  1440
                } else stop(paste0(" ERROR Unknown LV and DQO for averaging time ", Averaging.Period))
                Perc.DQO.0  <- 0.10; Perc.DQO.1 <- 0.20; Perc.DQO.2 <- min(0.85, Perc.DQO.1 * 4.9)
                ass.Threshold.health <- 100
                UAT   <- ass.Threshold.health}
            
            # Conversion coefficient
            if (!unit.ref %in% c("\u00b5g/m\u00b3","ug/m3")){
                # Conversion coefficient
                if (unit.ref %in% c("mg/m3", "mg/m\u00b3")){
                    Conv.Unit  <- 1000
                } else if (unit.ref == "ppb"){
                    Conv.Unit <- 2.05
                } else if (unit.ref == "ppm"){
                    Conv.Unit <- 2.05 * 1000
                } else stop(paste0("[get.DQO] ERROR Wrong unit for ",gas.sensor, ": ", unit.ref, "\n"))
            }
            
        } else if (name.gas == "Benzene") {
            
            # Parameters which normally depend on Directive units in µg/m³
            LV <-  5; 
            if(Directive == "EN TS 17660"){
                
                # Parameters that are not dependent on Directive but on TS17660-1, defined in µg/m³
                Class.1.ubss <- 1.3;  Class.2.ubss <- 2.6;  Class.3.ubss <- 5.2
                Class.1.Int  <- 0.28; Class.2.Int  <- 0.58; Class.3.Int  <- 0.60
                
                if(Averaging.Period %in% c("1year")){ #"1hour", 
                    Avg.Period <-  60 * 24 * 365
                } else stop(paste0(" ERROR Unknown LV and DQO for averaging time ", Averaging.Period))
            } else if(Directive == "2008/50/EC"){
                if(Averaging.Period == "1year"){
                    Avg.Period <-  60 * 24 * 365
                    Perc.UAT <- 0.70; Perc.LAT <- 0.40
                } else stop(paste0(" ERROR Unknown LV and DQO for averaging time ", Averaging.Period))
            } else if(Directive == "2024/2881/EC"){
                Perc.DQO.1 <- 0.35;  Perc.DQO.2 <- min(0.85, Perc.DQO.1 * 1.7)
                if(Averaging.Period == "1year"){
                    Avg.Period <-  60 * 24 * 365
                } else stop(paste0(" ERROR Unknown LV and DQO for averaging time ", Averaging.Period))
                ass.Threshold.health <- 1.7
                UAT   <- ass.Threshold.health}
            
            # Using LV for 1 year time average
            if (unit.ref == "ppb") {
                LV <- 5/3.73
                Class.1.ubss <- 1.3/3.73; Class.2.ubss <- 2.6/3.73; Class.3.ubss <- 5.2/3.73
                Class.1.Int <- 0.28/3.73; Class.2.Int <- 0.58/3.73; Class.3.Int <- 0.60/3.73
            } else if (unit.ref == "ug/m3") {
                LV <- 5
            } else stop(paste0("Wrong unit for ",gas.sensor, "\n"))
            
        } else if (name.gas == "PM10") {
            
            # Parameters that are not dependent on Directive but on TS17660-1, defined in µg/m³
            Class.1.ubss <- 3; Class.2.ubss <- 5; Class.3.ubss <- 7
            Class.1.Int  <- 3; Class.2.Int  <- 5; Class.3.Int  <- 7
            
            if(Directive == "EN TS 17660"){
                LV <- 50
                # Only one LV in EN TS 17660 with daily average
                Avg.Period <-  60 * 24
                Averaging.Period <- "24hour"
                Class.1.RHfactor <- 1.5;  Class.2.RHfactor <- 2.5
                Class.1.slpCoarse <- c(0.5,2)
                Class.1.R2Coarse <- 0.5
            } else if(Directive == "2008/50/EC"){
                Perc.UAT <- 0.70; Perc.LAT <- 0.50
                Perc.DQO.0  <- 0.25; Perc.DQO.1 <- 0.50; Perc.DQO.2 <- 1.00
                if(Averaging.Period == "24hour"){
                    LV <- 50; Avg.Period <-  60 * 24
                } else if(Averaging.Period == "1year"){
                    LV <- 40; Avg.Period <-  60 * 24 * 365
                } else stop(paste0(" ERROR Undefined LV and DQO for averaging time ", Averaging.Period))
            } else if(Directive == "2024/2881/EC"){
                Perc.DQO.0 <- 0.25; Perc.DQO.1 <- 0.35;  Perc.DQO.2 <- min(0.85, Perc.DQO.1 * 1.7)
                if(Averaging.Period == "24hour"){
                    LV <- 45; Avg.Period <-  60 * 24
                } else if(Averaging.Period == "1year"){
                    if(format(Sys.time(), "%Y") > 2026) LV <- 25 else LV <- 10
                    Avg.Period <-  60 * 24 * 365
                } else stop(paste0(" ERROR Undefined LV and DQO for averaging time ", Averaging.Period))
                ass.Threshold.health <- 1.7
                UAT   <- ass.Threshold.health}
            
        }  else if (name.gas == "PM2.5") {
            
            # Parameters that are not dependent on Directive but on TS17660-1, defined in µg/m³
            Class.1.ubss <- 3; Class.2.ubss <- 5; Class.3.ubss <- 7
            Class.1.Int  <- 3; Class.2.Int  <- 5; Class.3.Int  <- 7
            
            if(Directive == "EN TS 17660"){
                LV <- 30
                # Only one LV in EN TS 17660 with daily average
                Avg.Period <-  60 * 24
                Averaging.Period <- "24hour"
                Class.1.RHfactor <- 1.5;  Class.2.RHfactor <- 2.5
            } else if(Directive == "2008/50/EC"){
                Perc.UAT <- 0.70; Perc.LAT <- 0.50
                Perc.DQO.0  <- 0.25; Perc.DQO.1 <- 0.50; Perc.DQO.2 <- 1.00
                if(Averaging.Period == "24hour"){
                    LV <- 50; Avg.Period <-  60 * 24
                } else if(Averaging.Period == "1year"){
                    LV <- 40; Avg.Period <-  60 * 24 * 365
                } else stop(paste0(" ERROR Undefined LV and DQO for averaging time ", Averaging.Period))
            } else if(Directive == "2024/2881/EC"){
                Perc.DQO.0 <- 0.25; Perc.DQO.1 <- 0.35;  Perc.DQO.2 <- min(0.85, Perc.DQO.1 * 1.7)
                if(Averaging.Period == "24hour"){
                    LV <- 25; Avg.Period <-  60 * 24 # from 2027
                } else if(Averaging.Period == "1year"){
                    if(format(Sys.time(), "%Y") > 2026) LV <- 25 else LV <- 10 
                    Avg.Period <-  60 * 24 * 365
                } else stop(paste0(" ERROR Undefined LV and DQO for averaging time ", Averaging.Period))
                ass.Threshold.health <- 1.7
                UAT   <- ass.Threshold.health}
            
        }  else if (name.gas == "PM1") {
            if(Directive == "EN TS 17660"){
                # Fake DQO only for plotting DQO for PM1 using method of EN TS 17660
                LV    <- 20
                
                # Parameters that are not dependent on Directive but on TS17660-1, defined in µg/m³, taken from PM2.5
                Class.1.ubss <- 3; Class.2.ubss <- 5; Class.3.ubss <- 7
                Class.1.Int  <- 3; Class.2.Int  <- 5; Class.3.Int  <- 7
                LV    <- 20
                # Only one LV in EN TS 17660 with daily average by analogy of PM2.5 and PM10
                Avg.Period <-  60 * 24
                Averaging.Period <- "24hour"
                Class.1.RHfactor <- 1.5;  Class.2.RHfactor <- 2.5
            } else if(Directive == "2008/50/EC"){
                stop(paste0(" ERROR Undefined LV and DQO for ", name.gas, " in European Directive ", Directive))
            } else if(Directive == "2024/2881/EC") stop(paste0(" ERROR Undefined LV and DQO for ", name.gas, " in European Directive ", Directive))
        }
    }
    
    # Computing DQO.i, UAT and LAT from Per.DQO.i
    DQO.0 <- Perc.DQO.0  * LV; DQO.1 <- Perc.DQO.1 * LV; DQO.2 <- Perc.DQO.2 * LV; DQO.3 <- Perc.DQO.3 * LV
    UAT <- Perc.UAT * LV; LAT   <- Perc.LAT * LV
    
    # Converting according to Conv.Unit
    if (!unit.ref %in% c("\u00b5g/m\u00b3","ug/m3", "percent", "Celsius", "hPa")){
        for(Criteria in Criterias){
            if(!is.na(Criteria) && !Criteria %in% c(
                # Criteria not affected by Units
                "Perc.DQO.0", "Perc.DQO.1", "Perc.DQO.2", "Perc.DQO.3",
                "Perc.UAT", "Perc.LAT",
                
                # Only defined in EN TS 17660
                "Class.1.t90",       "Class.2.t90",       "Class.3.t90",
                "Class.1.Capture",   "Class.2.Capture",   "Class.3.Capture",
                "Class.1.RHfactor",  "Class.2.RHfactor",  "Class.3.RHfactor",
                "Class.1.slpCoarse", "Class.2.slpCoarse", "Class.3.slpCoarse",
                "Class.1.R2Coarse",  "Class.2.R2Coarse",  "Class.3.R2Coarse",
                "Class.1.slp",       "Class.2.slp",       "Class.3.slp",
                
                # Only defined in 2024/2881/EC
                "ass.Threshold.health", "ass.Threshold.veg")) assign(Criteria, get(Criteria)/Conv.Unit)}}
    
    return(list(unit.ref = unit.ref, Averaging.Period = Averaging.Period, Avg.Period = Avg.Period,
                IT = IT, AT = AT, LV = LV, UAT = UAT, LAT = LAT, Directive = Directive,
                DQO.0 = DQO.0, DQO.1 = DQO.1, DQO.2 = DQO.2, DQO.3 = DQO.3,
                Class.1.t90       = Class.1.t90,       Class.2.t90       = Class.2.t90,       Class.3.t90       = Class.3.t90,
                Class.1.lof       = Class.1.lof,       Class.2.lof       = Class.2.lof,       Class.3.lof       = Class.3.lof,
                Class.1.r         = Class.1.r,         Class.2.r         = Class.2.r,         Class.3.r         = Class.3.r,
                Class.1.Capture   = Class.1.Capture,   Class.2.Capture   = Class.2.Capture,   Class.3.Capture   = Class.3.Capture,
                Class.1.ubss      = Class.1.ubss,      Class.2.ubss      = Class.2.ubss,      Class.3.ubss      = Class.3.ubss,
                Class.1.RHfactor  = Class.1.RHfactor,  Class.2.RHfactor  = Class.2.RHfactor,  Class.3.RHfactor  = Class.3.RHfactor,
                Class.1.slpCoarse = Class.1.slpCoarse, Class.2.slpCoarse = Class.2.slpCoarse, Class.3.slpCoarse = Class.3.slpCoarse,
                Class.1.R2Coarse  = Class.1.R2Coarse,  Class.2.R2Coarse  = Class.2.R2Coarse,  Class.3.R2Coarse  = Class.3.R2Coarse,
                Class.1.slp       = Class.1.slp,       Class.2.slp       = Class.2.slp,       Class.3.slp       = Class.3.slp,
                Class.1.Int       = Class.1.Int,       Class.2.Int       = Class.2.Int,       Class.3.Int       = Class.3.Int))
}

##############################################
#' Plot a Modified Target Diagram ----
##############################################
#' @description 
#' Based on script by Aaron Albin
#' Use this script when the following is true:
#' - Inside Mat there are at least four numeric columns (xis, yis, Rel.bias, Rel.RSS).
#' - Slope and intercept of an OLS, Deming or orthogonal regression fitted to Mat$yis vs Mat$yis shall be given (b0 and b1). 
#' - xis, yis, Rel.bias, Rel.RSS, Slope and intercept are normally computed suing function U_orth_DF().
#' - You wish to plot Rel.bias, Rel.RSS onto a 'scatterplot', where the horizontal axis corresponds to Rel.RSS and the vertical axis corresponds to Rel.bias.
#' Each individual pair of numbers Rel.bias and Rel.RSS, then, is represented as a coloured point in this two-dimensional space where the color scale represents xis.
#' 
#' The relative expanded measurement uncertainty (Ur) is given by the distance between the origin (0, 0) and any point on the bold coloured line read on concentric circles.
#' The color scale indicates the reference data (xis).
#' The extent of random error (Rel.RSS) is read on the x-axis, the extent of relative bias (Rel.bias) is read on the y-axis.
#' Within Rel.bias, the contribution of the slope (b1, thin vertical coloured line) and intercept (b0, thin oblique coloured line) are read on the x-axis.

#' The primary use of the Modified Target Diagram is to check whether the Data Quality Objective (DQO) of indicative methods set in the European Directive (EC/50/2008) are satisfied.
#' DQOs are satisfied when the Limit Value (LV) blue star found on the coloured bold line falls within the 1st target circle representing the DQO.
#' The Modified Target Diagram gives additional information such as: 
#'  . The bold coloured line being above the x-axis (1st or 4th quadrant) indicates an overestimation of the sensor compared to reference data. Conversely, the bold coloured line being below the x-axis (2nd or 3rd quadrant) indicates an underestimation of the sensor compared to reference data.
#'  . Additionally, the bold coloured line being at the right of y-axis (1st or 2nd quadrant) indicates a higher sensitivity of the sensor compared to reference data. Conversely, the bold coloured line being at left of the y-axis (3rd or 4th quadrant) indicates a lower sensitivity of the sensor compared to reference data.
#'  . Variation of contributors to Rel.bias: high contribution from b0 would indicate an offset between the sensor and reference data, possibly correctable by offset subtraction, while high contribution from b1 may indicate an erroneous slope of calibration, possibly correctable by re-calibration or readjustment. Significant contribution from b0 and/or b1 are evidenced when corresponding thin coloured line(s) are far from y-axis.
#'  . The comparison of Rel.RSS and Rel.bias: overwhelming value of Rel.RSS compared to Rel.bias shows that Ur is dominated by the random errors likely resulting from several parameters including the electronic noise of sensor or by mis-calibration as missing covariates ...
#'  . Improvement by adjustment of b0 and/or b1 in order to set the Rel.bias to zero: the adjustment of b0 and b1 could allow to set the Rel.bias to zero for the entire range reference data with a rotation of the bold coloured line.
#'  However, this adjustment of b0 and b1 is only significant if Ur is not dominated by rel.RSS. In this case, setting the Rel.bias to zero by adjustment of b0 and b1 would not allow any significant decrease of Ur.
#' 
#' There are cases when the effects of b0 and b1 never cancel each other and thus Rel.bias never crosses the x-axis. Such cases might happen due to either:
#'  . the contributions of b0 and b1 are located on the same side of the y-axis with the combinations of b0 negative and b1 < 1, or, b0 positive and b1 > 1;
#'  . the contributions of b0 and b1 are located on different sides of the y-axis but one of these contributions is overwhelming with the b0 effect being higher than the b1 effect.
#'  
#' @param Sensor_name Optional, character string, default is NULL. Name of the sensor to be written in front of the calibration equation. If NULL (default), do not print sensor name.
#' @param Mat  data.table or dataframe of data including Case number, Date, xis, yis, [ubss and ubsRM if not constant], Rel.bias, Rel.RSS. Rel.bias, Rel.RSS and xis shall be included into dataFrame Mat.
#' It is easier to get this data.table from function U_orth_DF()
#' @param variable.ubsRM logical, default is FALSE. If FALSE ubsRM is a constant random standard uncertainties and if TRUE ubsRM is different for each reference data. ubsRM is printed on the plot only if variable.ubsRM is FALSE.
#' @param ubsRM numeric, default is NULL. Random between standard uncertainty of the reference data, xis, given as a constant value for all xis reference values. Only required if variable.ubsRM is FALSE.
#' @param with.ubss: logical, default is TRUE. If FALSE, Rel.RSS is computed without adding ubss. In this case Rel.RSS = 2 * (sqrt((Mat$RS - ubsRM^2) / Mat$xis). 
#'                   This useful if Mat is generated adding ubss while user wants to plot the Modified Target Diagram without ubss.
#'                   If TRUE Rel.RSS should be 2 * (sqrt(ubss^2 + Mat$RS - ubsRM^2)/ Mat$xis) but it is only possible to compute Rel.RSS if Mat includes a column ubss. This is generally not the case and Mat shalle be regernrated using function U_orth_DF()..
#' @param variable.ubss logical, default is FALSE. If FALSE ubss is a constant random standard uncertainties and if TRUE ubss is different for each reference data. ubss is printed on the plot only if variable.ubss is FALSE.
#' @param ubss numeric, default is NULL, Between standard uncertainty of the sensor data, yis, given as a constant value for all yis sensor values
#' @param b0, numeric, intercept of the orthogonal regression, default: NULL. If not NULL b0/xi is plotted
#' @param b1 numeric, slope of the orthogonal regression, default is NULL. If not NULL b1 - 1 is plotted
#' @param Unit.Ref Character, default is NULL. Unit of reference values, can "ppm", "ppb", "ug/m3", "mg/m3" ...
#' @param Unit.sensor character vector, unit for the expanded uncertainty of yis and yis. Default is Unit.Ref.
#' @param Xlabel,Ylabel label of the x and axis
#' @param Xlim,Ylim limits of x and y axis, default values is NA, vectors of two values min and max values. Xlim and Ylim is superseeded by Max.percent. Avoid using them.
#' @param Max.percent Optional, numeric in percent, default is NULL. Maximum extent of the x and y axis of the Target Diagram. This is respected provided that there exists Ur smaller than Max.percent. If NULL, DQO.3 is Max.percent.
#' @param MainTitle character, title to appear On the top of the Modified Target Diagram
#' @param DQO.1,DQO.2,DQO.3 numeric, data quality objectives for Indicative measurements, Modelling and objective estimation. Default is NA, if NA no DQO target circle is plotted. The DQOs are expressed as real values, not in percentage. Use function get.DQO. The xaxis is limited to 3 times DQO if Max.percent is NULL.
#' @param LAT,UAT,LV,AT,CL numeric, lower and upper assessment threshold, limit value, Alert threshold and Critical level of the European Air Quality Directive for Mat$xis, same unit as Mat$xis, default value = NA, used for color scale and target circles
#' @param f_coef1,f_coef2,f_R2 numeric, number of digit for intercept, slope and R2 using sprintf syntax. f_coef1 is used both for intercept and s(Res), f_coef2 is used for all parameters of the model apart from the intercept
#' @param nameModel character, name of model to be used to save uncertainty plots, character, default NULL
#' @param sdm_sdo logical, default is NULL. It shall be TRUE if the standard deviation of yis is lower than the one of xi and conversely.
#' If Null, sdm_sdo is determined using sd(Mat[Index.Good, yis]) and sd(Mat[Index.Good, yis])
#' @param Model.used character string, default is NULL. Name of calibration model used to compute yis. Only used if MainTitle is null.
#' @param BeginEnd character vector representing the starting and ending date, default is null. Only used if MainTitle is null.
#' @param Time.average: numeric, default is null. Tme average in minutes. . Only used if MainTitle is null.
#' @param Ref_Analysers name of reference analyser, default is NULL. If not null the name is added in Target_diagram
#' @param Show.Diag.Ur logical, default is TRUE. If TRUE a diagonal is printed between the origin and farest point, indicating relative expanded measurement uncertainty. A that the point the contribution of Bias and Relative Error is also plotted.
#' @return Plot a Target diagram unless error unless an error message is returned.
#' @examples TBD
Target.Diagram <- function(Sensor_name, Mat, variable.ubsRM = FALSE, ubsRM = NULL, with.ubss = TRUE, variable.ubss = FALSE, ubss = NULL, b0 = NULL, b1 = NULL,
                           Unit.Ref = NULL, Unit.sensor = Unit.Ref, xAxisLabel = NULL, yAxisLabel = NULL, Xlim = NA, Ylim = NA, MainTitle = NULL,
                           DQO.1 = NA, DQO.2 = NA, DQO.3 = NA, CL = NA, IT = NA, AT = NA, LV = NA, LAT = NA, UAT = NA,
                           sdm_sdo = NULL, Model.used = NULL, BeginEnd = NULL, Time.average = NULL, Max.percent = NULL, Ref_Analysers = NULL,
                           Regression = "OLS", Fitted.RS  = FALSE, Show.Diag.Ur = TRUE) {
    
    #=====[Consistency checks]=====
    #checking that Mat is dataFrame
    stopifnot(is.data.frame(Mat) || data.table::is.data.table(Mat))
    # convert to data.table if needed
    if (!data.table::is.data.table(Mat)) Mat <- data.table::data.table(Mat)
    #checking that the mat dataFrame is not empty, considering only the complete cases with Xis and yis > 0
    Mat <- Mat[is.finite(rowSums(Mat[, c("xis", "yis")]))]
    stopifnot(nrow(Mat) > 0)
    # Ordering Mat according to xis to be able to create a color pallete e
    stopifnot("xis" %in% names(Mat))
    setkey(Mat, key = "xis")
    # checking if Mat includes "Rel.bias", "Rel.RSS", "xis"
    stopifnot("Rel.bias" %in% names(Mat))
    stopifnot("Rel.RSS" %in% names(Mat))
    stopifnot(!is.null("b0"))
    stopifnot(!is.null("b1"))
    
    # plotting the Target Diagram
    # Create X-Y scatterplot: Coded by color by Aaron Albin
    #=====[Set XData, YData, b0 and b1 variables]=====
    # Rel.RSS is on the x axis
    # Rel.Bias is on the y axis
    # Recalculating uncertainty according to with.ubss. checking if Rel.RSS is negative and set to 0
    if (!with.ubss) {
        Sign.RS <- Mat$RS - Mat$ubsRM^2
        Negative.RS <- which(Sign.RS < 0)
        if (length(Negative.RS) > 0) {
            data.table::set(Mat, i = Negative.RS, j = "Rel.RSS", value = rep(0, times = length(Negative.RS)))
            Positive.RS <- setdiff(seq(Sign.RS), Negative.RS)
            if (length(Positive.RS) > 0) data.table::set(Mat, i = Positive.RS, j = "Rel.RSS", value = 2 * sqrt(Mat$RS[Positive.RS] - Mat$ubsRM[Positive.RS]^2) / Mat$xis[Positive.RS])
        } else data.table::set(Mat, j = "Rel.RSS", value = 2 * sqrt(Mat$RS - Mat$ubsRM^2) / Mat$xis)
    } else {
        stopifnot("ubss" %in% names(Mat))
        Sign.RS <- (Mat$RS - Mat$ubsRM^2 + Mat$ubss^2)
        Negative.RS <- which(Sign.RS < 0)
        if (length(Negative.RS) > 0) {
            data.table::set(Mat, i = Negative.RS, j = "Rel.RSS", value = rep(0, times = length(Negative.RS)))
            Positive.RS <- setdiff(seq(Sign.RS), Negative.RS)
            if (length(Positive.RS) > 0) data.table::set(Mat, i = Positive.RS, j = "Rel.RSS", value = 2 * sqrt(Mat$RS[Positive.RS] - Mat$ubsRM[Positive.RS]^2 + Mat$ubss^2) / Mat$xis[Positive.RS])
        } else data.table::set(Mat, j = "Rel.RSS", value = 2 * sqrt(Mat$RS - Mat$ubsRM^2 + Mat$ubss^2) / Mat$xis)
    }
    Mat[, U  := sqrt(Rel.RSS^2 + Rel.bias^2) * xis]
    # Values are multiplied by 100 to plot in percentage
    Mat[, Ur := U/ xis * 100]
    Mat[, XData :=  Rel.RSS * 100]
    Mat[, YData :=  Rel.bias * 100]
    # re-computing Ur in case RSS was changed
    data.table::set(Mat, j = "Ur", value = sqrt(Mat$XData^2 + Mat$YData^2))
    # For the decomposition of Rel.Bias
    # Contribution of b0 to Rel.Bias
    Mat[, b0 := 2 * b0 / xis * 100]
    # Contribution of b1 to Rel.Bias
    Mat[, b1 := 2 * rep((b1-1) * 100, times = nrow(Mat))]
    #=====[Set Index.Good, Xlim and Ylim]=====
    # To adjust xlim and ylim, set the arguments Xlim and Ylim when calling Target.Diagram()
    # Set Xlim or Ylim equal to 'c( ___, ___ )', where the two '___'s indicate the lower and upper bound (respectively) of the range for the relevant axis.
    # If you want the axis to be backwards/flip-flopped, add the upper bound first, followed by the lower bound, e.g. rather than c(5,100) say c(100,5).
    # When leaving the values Xlim and Ylim as NULL, R determines the limits (i.e. range) of the x (horizontal) axis and y (vertical) axis for you.
    if (is.null(Max.percent)) Max.percent <- DQO.3 * 100
    if (all(is.na(Xlim))) {
        if (any(abs(Mat$XData) <= Max.percent)) {
            # There are XData < Max.percent
            Index.Good.X <- which(abs(Mat$XData) <= Max.percent)
        } else {
            # All XData > Max.percent, creating a WRONG INDEX
            Index.Good.X <- NULL}}
    if (all(is.na(Ylim))) {
        if (any(abs(Mat$YData) < Max.percent)) {
            # There are YData < Max.percent
            Index.Good.Y <- which(abs(Mat$YData) <= Max.percent)
        } else {
            # All YData > Max.percent, creating a WRONG INDEX nothing to plot
            Index.Good.Y <- NULL}}
    # index of intersection of Index.Good.X and Index.Good.Y
    if (!is.null(Index.Good.X)) {
        if (!is.null(Index.Good.Y)) {
            Index.Good <- intersect(x = Index.Good.X, y = Index.Good.Y)
        } else Index.Good <- NULL
    } else Index.Good <- NULL
    if (all(is.na(Xlim))) {
        if (!is.null(Index.Good) && length(Index.Good) > 0) {
            Xlim <- c(max(-Max.percent,min(Mat$b1[Index.Good],Mat$b0[Index.Good],Mat$XData[Index.Good])), min(Max.percent, max(Mat$XData[Index.Good])))
        } else Xlim <- c(0, min(Max.percent, DQO.3 * 100))}
    if (all(is.na(Ylim))) {
        if (!is.null(Index.Good) && length(Index.Good) > 0) {
            Ylim <- c(max(-Max.percent,min(0, min(Mat$YData[Index.Good]))), min(Max.percent, max(DQO.3, max(Mat$YData[Index.Good]))))
        } else Ylim <- c(0, min(Max.percent, DQO.3 * 100))}
    if (is.null(Index.Good))     return(futile.logger::flog.warn(paste0("[Target.Diagram] ", Sensor_name, " : no data with Ur < ", Max.percent, "%")))
    if (!length(Index.Good) > 0) return(futile.logger::flog.warn(paste0("[Target.Diagram] ", Sensor_name, " : no data with bias or RSS < ", Max.percent, "%")))
    # Flipping Xlim if standard deviation of yis is lower than the one of yis using sdm_sdo
    if (is.null(sdm_sdo)) sdm_sdo <- sd(Mat[Index.Good, yis]) <= sd(Mat[Index.Good, xis])
    if (sdm_sdo) Xlim <- rev(Xlim)
    #=====[MainTitle]=====
    # Set the name for the main title (centered along the top of the plot) here.
    if (is.null(MainTitle)) {
        if (!is.null(Model.used)) {
            MainTitle = gsub("__", "_", sub('.rdata', '', basename(Model.used)))
        } else {
            if (!is.null(Sensor_name)) MainTitle = paste0(Sensor_name, " - Target Diagram - Relative expanded uncertainty") else MainTitle = "Target Diagram - Relative expanded uncertainty"}}
    #=====[axis labels]=====
    # In the argument 'xAxisLabel', type the words(s) you want to see displayed beneath the x (horizontal) axis.
    # In the argument 'yAxisLabel', type the word(s) you want to see displayed to the left of the y (vertical) axis.
    # If you don't want to include an axis label at all for either of these, just leave the double-quotes empty, i.e. "", for that one.
    if (is.null(xAxisLabel)) {
        if (with.ubss) {
            xAxisLabel = bquote(paste("Relative random effect in bold: RR = 2 ", sqrt("u"[bs_s]^2*" + RMSE"^2*" - u"[bs_RM]^2), "/X"[i]*"; oblique line: 2b"[0]*"/X"[i]*"; vertical line: 2(b"[1]*"- 1) in %"))
        } else {
            xAxisLabel = bquote(paste("Relative random effect in bold: RR = 2 ", sqrt("RMSE"^2*" - u"[bs_RM]^2), "/X"[i]*"; oblique line: 2b"[0]*"/X"[i]*"; vertical line: 2(b"[1]*"- 1) in %"))}}
    if (is.null(yAxisLabel)) yAxisLabel = bquote("Relative bias: RB = 2 (b"[0]*"/X"[i]*" + (b"[1]*" - 1)) in %")
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
    #=====[LegendLabels]=====
    # The labels plotted are the ones in factor.Color that does not include the legislative levels CL, IT, AT, LV, LAT, UAT
    # Levels include Which Targets are not NULL
    Levels       <- c(CL, IT, AT, LV, LAT, UAT)[which(!is.na(c(CL, IT, AT, LV, LAT, UAT)))]
    Levels       <- Levels[Levels >= min(Mat[Index.Good,xis]) & Levels <= max(Mat[Index.Good,xis])]
    factor.Color <- pretty(Mat[Index.Good,xis], n = (10 - length(levels)))
    factor.Color <- round(seq(from = min(Mat[Index.Good,xis]), to = max(Mat[Index.Good,xis]), length.out = 10), digits = 1)
    LegendLabels <- round(unique(sort(c(factor.Color, Levels))), digits = 0)
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
    Mat[Index.Good, col := plotrix::color.scale(Mat[Index.Good, xis],c(0,1,1),c(1,1,0),0)]
    #=====[PointSizeModifier]=====
    # If you would like your points to be bigger or smaller, the following lets you adjust their size.
    # '1' represents 100% of the default size, so if you want to make the points larger you could type, for example, 1.5 (i.e. 150%). Similarly, if you want to make them smaller you could type 0.5 (i.e. 50%).
    PointSizeModifier = 1.2
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
    #=====[GridlineColor]=====
    # Choose the color for your grid lines. See step 12 for help.
    GridlineColor = "lightgrey"
    #=====[GridlineWidthModifier]=====
    # If you would like your gridlines to be thicker, the following lets you increase its width.
    # '1' represents 100% of the default thickness, so for example, you could type 2 to make it 200% that thickness.
    GridlineWidthModifier = 1
    #=====[Target Diagram]=====
    op <- par(no.readonly = TRUE)
    # Restoring graphical parameters on exit of function
    par(mar = c(2.6, 2.6, 2.5, 3.5),  # mar=c(margin below, lines at left, lines at top, lines at right)
        mgp = c(1.35, 0.0, 0),         # mgp=c(label, tick mar label, tick mark)
        cex.axis = 0.8)
    
    on.exit(par(op))
    plot.default(x           = Mat$XData[Index.Good], y    = Mat$YData[Index.Good],
                 xlim        = Xlim                 , ylim = Ylim,
                 xlab        = xAxisLabel           , ylab = yAxisLabel,
                 cex.lab     = 0.8,
                 pch         = PointSymbol,
                 col         = Mat$col[Index.Good],
                 cex         = PointSizeModifier,
                 type        = "p",
                 ann         = TRUE,
                 axes        = TRUE,
                 frame.plot  = TRUE,
                 panel.first = grid(nx  = NULL,
                                    ny  = NULL,
                                    lty = GridlineType,
                                    col = GridlineColor,
                                    lwd = GridlineWidthModifier),
                 panel.last  = NULL,
                 asp         = 1, tck = 0)
    title(main = MainTitle, line = 1.5, font.main = 1, cex.main = 0.9)
    # get the limits of x and y axis
    usr <- par('usr')
    #=====[highest standard deviation]====
    if (sdm_sdo) {
        label.sigma <- " < "
    } else label.sigma <- " > "
    if (abs(Ylim[2]) > abs(Ylim[1])) label.Bias <-  "Bias > 0" else label.Bias <-  "Bias < 0"
    #=====[Text highest standard deviation and bias]=====
    # see https://stackoverflow.com/questions/4973898/combining-paste-and-expression-functions-in-plot-labels
    text(x      = if (sdm_sdo) usr[1]/2 else usr[2]/2, 
         y      = if (abs(Ylim[2]) > abs(Ylim[1])) usr[4] else usr[3],
         pos    = if (abs(Ylim[2]) > abs(Ylim[1])) 1 else 3,
         labels = bquote(sigma[Sensor] ~ .(label.sigma) ~ sigma[Reference] ~ " and " ~ .(label.Bias)), cex = 0.8)
    #=====[2nd line Main Title]=====
    if (!is.null(BeginEnd)) {
        Text.Dates <- paste0(BeginEnd[1], " to ", BeginEnd[2])
    } else Text.Dates <- paste0(format(min(Mat$Date, na.rm = T),"%Y%m%d"), " to ", format(max(Mat$Date, na.rm = T),"%Y%m%d"))
    Ref_Analysers <- ifelse(!is.null(Ref_Analysers), paste0(" (",Ref_Analysers,") "),"")
    # Formatting Decimal places in R: https://stackoverflow.com/questions/3443687/formatting-decimal-places-in-r
    if (variable.ubsRM) {
        Text <- substitute(paste(Text.Dates,", Y"["i"]," = b"[0]," + b"[1]," X"["i"],", with b"[0]," = ", b0.Digits, " and b"[1]," = ", b1.Digits, " (", Type.Regression,")",
                                 Ref_Analysers), 
                           list(Text.Dates = Text.Dates, b0.Digits = format(round(b0,2), digits = 2), b1.Digits = format(round(b1, digits = 2), nsmall = 2), Type.Regression = Regression, Ref_Analysers = Ref_Analysers))
    } else {
        Text <- substitute(paste(Text.Dates,", Y"["i"]," = b"[0]," + b"[1]," X"["i"],", with b"[0]," = ", b0.Digits, " and b"[1]," = ", b1.Digits, " (", Type.Regression,")",
                                 ", u"[bs_RM]," = ", ubs_RM.Digits, " ", Unit.Ref, Ref_Analysers), 
                           list(Text.Dates = Text.Dates, b0.Digits = format(round(b0,2), digits = 2), b1.Digits = format(round(b1, digits = 2), nsmall = 2), Type.Regression = Regression,ubs_RM.Digits = format(round(ubsRM, 2), digits = 2), 
                                Unit.Ref = Unit.Ref, Ref_Analysers = Ref_Analysers))
    }
    if (with.ubss && variable.ubss) Text <- substitute(paste(Text," and u"[bs_s]," = ", ubss.Digits, " ", Unit.sensor), list(Text = Text,ubss.Digits = format(round(ubss,2), digits = 2), Unit.sensor = Unit.sensor))
    if (Unit.Ref == "ppm" || Unit.Ref == "mg/m3") {
        mtext(text = Text, side = 3, line = 0.1, cex  = 0.65)
    } else {
        if (is.null(Time.average)) {
            mtext(text = Text, side = 3, line = 0.1, cex  = 0.65)
        } else mtext(text = paste0(Text, format(Time.average, digits = 0), " min average"), side = 3, line = 0.1, cex  = 1.0)}
    #=====[Contribution of b1 : (b1 - 1)]=====
    points(x = Mat$b1[Index.Good],
           y = Mat$YData[Index.Good],
           col = Mat$col[Index.Good], pch  = "-", cex  = 0.5)
    #=====[Contribution of b0 : b0/x]=====
    points(x = Mat$b0[Index.Good],
           y = Mat$YData[Index.Good],
           col = Mat$col[Index.Good], pch  = "-", cex  = 0.5)
    #=====[draw axis]=====
    abline(h = 0)
    abline(v = 0)
    #=====[colorscale]=====
    # Add colorscale
    testcol <- plotrix::color.gradient(c(0,1,1),c(1,1,0),0,nslices=100)
    plotrix::color.legend(usr[2] + 0.005 *(usr[2]-usr[1]), usr[3], usr[2] + 0.03 *(usr[2]-usr[1]),usr[4] - 0.05 * (usr[4]-usr[3]),
                          factor.Color,testcol, cex=0.6, align="rb",gradient="y")
    LegendTitle <- substitute(paste("X"[i], ", ", Unit,sep=""),list(Unit=Unit.Ref))
    #=====[target circles]=====
    DQO.step <- min(c(DQO.1, DQO.2 - DQO.1, DQO.3 - DQO.2)) * 100
    Quadrants <- c(sum(usr[c(2,4)]^2),sum(usr[c(1,4)]^2),sum(usr[c(1,3)]^2),sum(usr[2:3]^2))
    #Which.Quadrants <- which.max(Quadrants)
    #usr.Quadrant    <- switch (Which.Quadrants, c(2,4), c(1,4), c(1,3), c(2,3))
    Dist.Quadrants  <- sqrt(max(Quadrants))
    if (DQO.1 * 100 <= Dist.Quadrants && diff(Xlim) != 0 && diff(Ylim) != 0) {
        if (Xlim[2] > Xlim[1]) x = seq(Xlim[1], Xlim[2]) else x = seq(Xlim[2], Xlim[1])
        if (Ylim[2] > Ylim[1]) y = seq(Ylim[1], Ylim[2]) else y = seq(Ylim[2], Ylim[1])
        UR <- function(x,y) {z <- sqrt(x^2+y^2); return(z)}
        z <- outer(x, y, UR)
        Steps <- seq(from = DQO.1 * 100, to = Dist.Quadrants, by = DQO.step)
        Steps <- Steps[-which(as.character(Steps) %in% as.character(c(DQO.1 * 100 , DQO.2  * 100, DQO.3  * 100)))]
        
        # Plotting intermediary levels
        if (length(Steps) > 0) {
            contour(x = x, y = y, z = z, col = "grey", add = TRUE, method = "edge", levels = Steps,
                    vfont = NULL, axes = FALSE, frame.plot = axes, lty = 'dotdash', lwd = 1, labcex = 0.7)
        }
        
        # Plotting DQOs
        contour(x = x, y = y, z = z, col = "black", add = TRUE, method = "edge", levels = c(DQO.1*100, DQO.2*100, DQO.3*100),
                vfont = NULL, axes = axes, frame.plot = axes, lty = 'solid', lwd  = 2, labcex = 0.8)
    }
    #=====[Arrows]=====
    # Plotting segments for relative measurement uncertainty
    # checking that data is visible
    # Select last quartile of these 90 % the biggest Ur within Index.Good
    Index.Big.Ur <- which(Mat[Index.Good,Ur] < quantile(Mat[Index.Good,Ur], probs = 0.9) & Mat[Index.Good,Ur] > quantile(Mat[Index.Good,Ur], probs = 0.75))
    # Absolute difference between Angle of Ur and diagonal 45 and -45 degress
    Mat[, Angle45  := abs(atan(YData/XData) * 180 / pi  -   45 )]
    Mat[, Angle_45 := abs(atan(YData/XData) * 180 / pi  - (-45))]
    # Selecting best point for plotting arrows
    if (min( Mat[Index.Good, Angle45][Index.Big.Ur], na.rm = T) < min(Mat[Index.Good, Angle_45][Index.Big.Ur], na.rm = T)) {
        Index.med.UR <- which(Mat[["Angle45"]] == min(Mat[["Angle45"]][Index.Good][Index.Big.Ur] ))[1]
    } else Index.med.UR <- which(Mat[["Angle_45"]] == min(Mat[["Angle_45"]][Index.Good][Index.Big.Ur]))[1]
    # Index.med.UR based on biggest visible uncertainty
    Index.med.UR <- which(Mat$Ur == Mat$Ur[Index.Good][which.max(Mat[Index.Good][["Ur"]])])[1]
    
    # plotting Arrows and text Rel.Bias, rel.Rc, Ur
    if(Show.Diag.Ur){
        shape::Arrows(x0 = Mat$XData[Index.med.UR], y0 = 0,
                      x1 = Mat$XData[Index.med.UR], y1 = Mat$YData[Index.med.UR],
                      col = "black",
                      lty = 1, lwd = 1,
                      arr.type = "curved", arr.length = 0.1, code = 3, arr.adj = 1)
        text(x = Mat$XData[Index.med.UR], y = Mat$YData[Index.med.UR]/2,
             labels = c("Bias in %"), pos = 4, srt = 90, cex = 0.8)
        shape::Arrows(x0 = 0                  , y0 = Mat$YData[Index.med.UR],
                      x1 = Mat$XData[Index.med.UR], y1 = Mat$YData[Index.med.UR],
                      col = "black", lty = 1, lwd = 1, arr.type = "curved", arr.length = 0.1,
                      code = 3, arr.adj = 1)
        text(x = Mat$XData[Index.med.UR]/2, y = Mat$YData[Index.med.UR],
             labels = c("Random effect"), pos = 3, adj = 0.5, cex = 0.8)
        shape::Arrows(x0 = 0                  , y0 = 0,
                      x1 = Mat$XData[Index.med.UR], y1 = Mat$YData[Index.med.UR],
                      col = "black", lwd = 2, arr.type = "curved", arr.length = 0.15, code = 3, arr.adj = 1)
        Srt <- atan(Mat$YData[Index.med.UR]/Mat$XData[Index.med.UR]) * 180 /pi
        if (sdm_sdo) {
            if (Srt >= 0) Srt <- - Srt else Srt <- - Srt}
        text(x = Mat$XData[Index.med.UR]/2, y = Mat$YData[Index.med.UR]/2,
             labels = c("Relative expanded uncertainty in %"),
             srt = Srt, pos = 3)}
    #=====[Text and line b1 - 1]=====
    text(x = Mat$b1[1]/2,
         y = ifelse(label.Bias ==  "Bias > 0",max(Mat$YData[Index.Good]),min(Mat$YData[Index.Good])),
         labels = bquote("2(b"[1]*"-1)"),
         cex = 0.8, pos = 3) # pos label above the coordinate
    text(x = Mat$b1[1]/2,
         y = ifelse(label.Bias ==  "Bias > 0",max(Mat$YData[Index.Good]),min(Mat$YData[Index.Good])),
         labels = paste0(format(2*(b1 - 1)*100, digits = 1),"%"),
         cex = 0.8, pos = 1) # pos label above the coordinate
    shape::Arrows(x0 = 0         , y0 = ifelse(label.Bias ==  "Bias > 0", max(Mat$YData[Index.Good]), min(Mat$YData[Index.Good])),
                  x1 =  Mat$b1[1], y1 = ifelse(label.Bias ==  "Bias > 0",max(Mat$YData[Index.Good]),min(Mat$YData[Index.Good])),
                  col = "black", lty = 1, lwd = 1, arr.type = "curved", arr.length = 0.1, code = 3, arr.adj = 1)
    #=====[Text and point b0]=====
    Label = bquote("2 b"[0]*"/X"[i]*" in %")
    if (b0 > 0) {
        if (any(Mat$b0 <= ifelse(sdm_sdo,Xlim[1],Xlim[2]))) {
            # There are Mat$b0 <= Xlim[2]
            Index.Good.b0 <- which(Mat[Index.Good, b0] <= ifelse(sdm_sdo,Xlim[1],Xlim[2]))
            x.b0 <- median(Mat[Index.Good, b0][Index.Good.b0], na.rm = FALSE)
            Lower.x.b0 <- which(Mat$b0 <= x.b0)
            if (length(Lower.x.b0) > 0) {
                y.b0 <- Mat[Lower.x.b0][which.max(Mat$b0[Lower.x.b0]), Rel.bias] * 100
                #y.b0 <- Mat[Mat$b0 == x.b0, Rel.bias][1] * 100
                Srt = atan(y.b0 /(x.b0 + Mat$b1[1])) * 180 / pi
                if (sdm_sdo) if (Srt >= 0) Srt <- - Srt else Srt <- - Srt
                text(x = x.b0, y = y.b0, labels = Label, srt = Srt, pos = 3)}}
    } else {
        if (any(Mat$b0 >= ifelse(sdm_sdo,Xlim[2],Xlim[1]))) {
            # There are Mat$b0 >= Xlim[1]
            Index.Good.b0 <- which(Mat[Index.Good, b0] >= ifelse(sdm_sdo,Xlim[2],Xlim[1]))
            x.b0 <- median(Mat[Index.Good, b0][Index.Good.b0], na.rm = FALSE)
            Lower.x.b0 <- which(Mat$b0 <= x.b0)
            if (length(Lower.x.b0) > 0) {
                y.b0 <- Mat[Lower.x.b0][which.max(Mat$b0[Lower.x.b0]), Rel.bias] * 100
                Srt = atan(y.b0 /(x.b0 + Mat$b1[1])) * 180 / pi
                if (sdm_sdo) if (Srt >= 0) Srt <- - Srt else Srt <- - Srt
                text(x = x.b0, y = y.b0, labels = Label, srt = Srt, pos = 4)}}}
    points(x = Mat$b0[Index.med.UR], y = Mat$YData[Index.med.UR], type = "p", col = "black")
    segments(x0 = Mat$b0[Index.med.UR], y0 = 0,
             x1 = Mat$b0[Index.med.UR], y1 = Mat$YData[Index.med.UR],
             col = "black", lty = 2, lwd = 1)
    Label <- substitute(paste("2 b"[0], "/X"[i], ":\n", b0.Digits, "%"), list(b0.Digits = round(Mat$b0[Index.med.UR],digits =0L)))
    text(x = Mat$b0[Index.med.UR], y = 0, labels = Label, pos = 1, cex = 0.7)
    #=====[Limit Values identifier]=====
    Limits2plots        <- c(CL, IT, AT, LV, UAT, LAT)
    names(Limits2plots) <- c("CL", "IT", "AT", "LV", "UAT", "LAT")
    for (Limit in names(Limits2plots)) {
        # Checking if Limit is not NA
        if (!is.na(Limits2plots[Limit])) {
            if (Limits2plots[Limit] < max(Mat[Index.Good,xis], na.rm = T)) {
                # checking is Limit is witin plotted x values not necessary, plot Limit is will apper only if within xlim an ylim
                if (any(Mat[Index.Good,xis] == Limits2plots[Limit])) {
                    Index.Limit <- which(Mat[Index.Good,xis] == Limits2plots[Limit])[1]
                } else Index.Limit <- which(abs(Mat[Index.Good,xis] - Limits2plots[Limit]) == min(abs(Mat[Index.Good,xis] - Limits2plots[Limit]), na.rm = T))[1]
                # PLotting a point for Limit
                points(x = Mat$XData[Index.Good][Index.Limit],
                       y = Mat$YData[Index.Good][Index.Limit],
                       type = "p",
                       col  = "blue",
                       cex  = 1.5,
                       pch  = 8,
                       lwd  = 2) #"+")
                # plotting label Limit
                text(x = Mat$XData[Index.Good][Index.Limit],
                     y = Mat$YData[Index.Good][Index.Limit],
                     labels = Limit,
                     pos = 1)}
            if (!exists("Name.Limits2plots")) {
                Name.Limits2plots <- paste0(Limit, " = ", round(Limits2plots[Limit], digits = 1)) 
            } else Name.Limits2plots <- paste0(Name.Limits2plots, ", ",paste0(Limit, " = ", round(Limits2plots[Limit], digits = 1)))}}
    Blank.space <- 0.01 * (usr[3] - usr[4])
    if (exists("Name.Limits2plots") && length(Name.Limits2plots) > 0 ) text(x      = if (sdm_sdo) (usr[1] + usr[2])/2 else (usr[2] + usr[1])/2, 
                                                                            y      = if (abs(Ylim[2]) > abs(Ylim[1])) usr[3]-Blank.space else usr[4]+Blank.space,
                                                                            adj    = c(0.5, ifelse(abs(Ylim[2]) > abs(Ylim[1]), 0, 1)),
                                                                            #pos    = ifelse(abs(Ylim[2]) > abs(Ylim[1]), 3, 1),
                                                                            labels = paste0(Name.Limits2plots," ", Unit.sensor), cex = 0.8)
    # setting all margin accessible with mtext
    par(xpd = NA)
    text(x = usr[2] + 0.005 * (usr[2]-usr[1]), y = usr[4] - 0.005 * (usr[4]-usr[3]), labels  = LegendTitle, adj = c(0,1), cex = 0.8)
}
#================================================================CR
### function to plot and compare x reference values against y the sensor values (y 0 measurement function) ####
#================================================================CR
lm.Model.Compare <- function(General.df, DateIN, DateEND, x, y, Title = NULL) {
    # This function plot the x and y data of model and draw a linear line
    # input  : General.df : dataFrame with y data and back predicted data
    #          DateIN     : character strings, beginning and ending dates of selected data, e.g DateIN  <- "2018-10-09" DateEND <- "2019-01-10"
    #          x, y       : character strings, names of columns x and y in General.df dataFrame, they will be usd for x and y axis labels
    #          Title      : optional chacter string of the scatterplot
    # Output : The linear comparison model
    # Subset General.df to selected date
    General    <- subset(General.df[, c("date", x, y)], date > as.POSIXct(DateIN) & date <= as.POSIXct(DateEND))
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
### function to query data using a_i_p server ####
#================================================================CR
#' @description Searching for all parameters stored at an aip server. For info, mail "Erich Kitzmueller" <erich.kitzmueller@a-i-p.com>
#' @param URL           character string indicating the a-i-p URL for data transmission
#' @param username      character string, login at the aip data, default value is "datareq"
#' @param password      character string, password associated with username, default value is "aip2jrc"
#' @param organisation  character string, name of monitoring site of the aip server, default is "ABCIS", what is called "Monitoring Site" by aip?
#' @param station       character string, name of the "Global Parameter" of the aip server, default "JRC+Ispra", what is called and "Global Parameter"by aip. Can be a vector of more than one station.
#' @param start         Date, indicating the starting date for searching for stored parameters, default as.Date(Sys.time()) - 2 days ago.If the class of start is not a date, start is set to 2 days before current date.
#' @param end           Date, indicating the ending date for searching for stored parameters, default is as.Date(Sys.time()). If the class of start is not a date, start is set to current date.
#' @param allparams     character possible values are "true" and "false". Default is "true". Append the parameter "&allparams=true" to your URL. Without that, the request only returns the values of monitoring-site-parameters marked as default in UBIS4 software.
#' @param avgtime       Integer, possible values are 1, 10, 15 or 60, default is 1, the averaging time of data in minute at the aip server
#' @param shiny         Logical, default value uis TRUE. Used to return eror message in shiny apps.
#' @return A vector with all parameters that are available at the aip server for the organisation and station 
#' @usage For each unique combination of "Monitoring Site" and "Global Parameter", there can be only one monitoring-site-parameter marked as default.
#' @examples 
a_i_p_param <- function(URL, username = "datareq", password = "aip2jrc", organisation = "ABCIS", station = "JRC+Ispra", 
                        start = as.Date(Sys.time())-2, end = as.Date(Sys.time()), allparams = "true", avgtime  = 1, shiny = TRUE, Verbose = TRUE) {
    
    ### Load packages
    list.Packages <- c("RCurl", "curl", "jsonlite", "httr", "tidyverse", "purrr", "dplyr")
    if (!all(list.Packages %in% installed.packages())) librarian::shelf(list.Packages, cran_repo = "https://cran.r-project.org");rm(list.Packages)
    
    # Checking internet
    if (curl::has_internet()) {
        if(Verbose) futile.logger::flog.info(paste0("[a_i_p_param] There is an internet connection."))
    } else {
        my_message <- paste0("[a_i_p_param()] ERROR There is no internet connection\n",
                             "It is impossible to download data.\n")
        if(Verbose) cat(my_message)
        if (shiny) {
            shinyalert(
                title = "ERROR no internet Connection",
                text = my_message,
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
                animation = FALSE)
        }
        return(cat(my_message))
    }
    # start/end date
    if(lubridate::is.Date(start)) start <- format(start, "%Y-%m-%d-%H-%M-%S") else start <- format(as.Date(Sys.time())-2, "%Y-%m-%d-%H-%M-%S")
    if(lubridate::is.Date(end)) end <- format(end, "%Y-%m-%d-%H-%M-%S") else {
        if(lubridate::is.Date(start)) end <- format(start + 2, "%Y-%m-%d-%H-%M-%S") else end <- format(as.Date(Sys.time()), "%Y-%m-%d-%H-%M-%S")}
    #----------------------------------------------------------------CR
    # 1) Contacting Server
    #----------------------------------------------------------------CR
    if(Verbose) futile.logger::flog.info("[a_i_p_param] Sending JSON request: ")
    for (Udap in station){
        JSON.request <- paste0(URL, "username=", username,"&",
                               "password=", password,"&",
                               "organisation=", organisation,"&",
                               "station=", Udap,"&",
                               #gsub(" ", "", paste(paste("&station=",station, collapse = ""))),"&",
                               "allparams=",allparams,"&",
                               #"AvgTime=10", "&",
                               "start=", start,"&",
                               "end=", end, "&",
                               "aggregation=2h+Mean+Value")
        if(Verbose) cat(paste0(JSON.request,"\n"))
        JSON <- httr::GET(utils::URLencode(JSON.request))
        if (JSON$status_code == 200) {
            # Extract data from the JSON file
            # extract data node
            Reference <- httr::content(JSON, type = "application/json", as = 'parsed')
            Reference <- Reference$Stations[[1]]$Devices
            # Returning parameters
            Udap.Components <- sapply(seq_along(Reference), function(i) Reference[[i]]$Components[[1]]$Component)
            if (shiny::isTruthy(Udap.Components)){
                if (exists("Components")) Components <- c(Components, Udap.Components) else Components <- Udap.Components}
        } else if (JSON$status_code == 204) {
            my_message <- paste0("[a_i_p_param] INFO the server was contacted with succes but there no data to return. Change dates.\n")
            if(Verbose) cat(my_message)
            if (shiny) shinyalert(
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
        } else {
            my_message <- paste0("[a_i_p_param] ERROR the parameter to contact the a_i_p server are wrong, please check\n")
            if(Verbose) cat(my_message)
            if (shiny) shinyalert(
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
                animation = FALSE)}}
    if (exists("Components")) return(Components) else return()
}

#' @description Downloading data from aip server.  For info, mail "Erich Kitzmueller" <erich.kitzmueller@a-i-p.com>
#' @param URL           character string indicating the a-i-p URL for data transmission
#' @param username      character string, login at the aip data, default value is "datareq"
#' @param password      character string, password associated with username, default value is "aip2jrc"
#' @param organisation  character string, name of monitoring site of the aip server, default is "ABCIS", what is called "Monitoring Site" by aip?
#' @param station       character string, name of the "Global Parameter" of the aip server, default "JRC+Ispra", what is called and "Global Parameter"by aip. Can be a vector of more than one station.
#' @param start         POSIXct or character string indicating the starting date for searching for stored parameters, format: "YYYY-mm-dd-HH-MM-SS", default is POSIXct 2 days ago
#' @param end           POSIXct or character string indicating the ending date for searching for stored parameters, format: "YYYY-mm-dd-HH-MM-SS", default is POSIXct current date and time
#' @param avgtime       Integer, possible values are 1, 10, 15 or 60, default is 1, the averaging time of data in minute at the aip server. 
#' When using a JSON request on a browser, the parameter "avgtime" does not automatically choose an aggregation; instead, you can only use it to chose a category of measured values (two data table exist one for 1min data and
#' one for 10min data) where to take the data from. Those values are calculated by the station (IOX), UBIS4 stores values of both categories in the database).
#' The parameter name is "avgtime" (not "AvgTime", which is simply ignored) and the unit is seconds. Therefore, you have to supply the name of an aggregation (configured in UBIS4) to use for averaging, e.g.
#' https://ubis-air.a-i-p.com/UidepService/values/simple?username=datareq&password=aip2jrc&organisation=ABCIS&station=Sensors&allparams=true&start=2019-08-29-00-00-00&end=2019-09-11-17-00-00&aggregation=1h+Mean+Value
#' https://ubis-air.a-i-p.com/UidepService/values/simple?username=datareq&password=aip2jrc&organisation=ABCIS&station=Sensors&avgtime=60&allparams=true&start=2019-08-29-00-00-00&end=2019-09-11-17-00-00
#' returns the 1-Minute values, and
#' https://ubis-air.a-i-p.com/UidepService/values/simple?username=datareq&password=aip2jrc&organisation=ABCIS&station=Sensors&avgtime=600&allparams=true&start=2019-08-29-00-00-00&end=2019-09-11-17-00-00
#' returns the 10-Minute values, which are also default# You can use both the "avgtime" and the "aggregation" parameter in one query; for example, you can request "1h Mean Values" calculated from
#' the 1-Minute values; in a perfect world, that would return the same like "1h Mean values" calculated from 10-Minute values, but in reality, small rounding differences might occur.
#' @param Valid         logical, default is TRUE. see below mail exchange. Info is lost. Keep value TRUE
#' @param unflagged     logical, shall be TRUE or FALSE, default TRUE, only return unflagged data. This is a new option in the UidepService: unflagged=true means 
#' that all flagged values are considered invalid (even though the flags do not normally render them invalid in UBIS). You can compare the output of:
# https://ubis-air.a-i-p.com/UidepService/values/simple?username=datareq&password=aip2jrc&organisation=ABCIS&station=JRC+Ispra&component=nitrogen+dioxide&start=2019-09-18-15-00-00&end=2019-09-18-17-00-00&valid=true&avgtime=60&aggregation=15m+Mean+Value&unflagged=true
# with the output of
# https://ubis-air.a-i-p.com/UidepService/values/simple?username=datareq&password=aip2jrc&organisation=ABCIS&station=JRC+Ispra&component=nitrogen+dioxide&start=2019-09-18-15-00-00&end=2019-09-18-17-00-00&valid=true&avgtime=60&aggregation=15m+Mean+Value&unflagged=false
# to see what it means. This works with one-minute-Values just as well.
#' @param flushtime integer, 0 means no flush, other wise time in seconds, e. g. 60, 60 , 120 (2 minutes advised). Wrong values, which are obviously too high, belong to the flush phase of the function check. The analyzer already inhales normal air, but there is still some span gas in the measurement chamber.
#' Since the device is no longer fed with span gas, there is no operating status set on those values. In the output of the IOX, there are internal status bits set that indicate this state, but unfortunately those are currently ignored by UBIS, and I cannot change that easily.
#' As a workaround, I have added yet another parameter to the UIDEP webservice: "flushtime=120" causes all values within 120 seconds after a marked value to be considered invalid, too. (120 is the recommended value, but you can of course choose any other)
#' https://ubis-air.a-i-p.com/UidepService/values/simple?username=datareq&password=aip2jrc&organisation=ABCIS&station=JRC+Ispra&component=sulfur+dioxide&start=2019-09-19-16-00-00&end=2019-09-19-18-00-00&valid=true&avgtime=60&unflagged=true&flushtime=120
#' Without unflagged=true, flushtime has no effect at all.
#' Becareful for OPAS this parameter has no effect
#' @return A list with 2 dataframes: data that gives all data with  1st column a POSIXct "date" only Valid and unflagged values
#                                    meta that gives a dataframe of metadata: a_i_p components, reference name and units
#' @usage For each unique combination of "Monitoring Site" and "Global Parameter", there can be only one monitoring-site-parameter marked as default. 
#' Since the CAPS device does the logging all by itself, we just transfer and import the recorded values "as-is" without even trying to fit them into a one-minute time grid.
#' @examples 

a_i_p_data <- function(URL, username = "datareq", password = "aip2jrc", organisation = "ABCIS", station = "JRC+Ispra",
                       start = format(Sys.time()-2*1440*60, "%Y-%m-%d-%H-%M-%S"), end = format(Sys.time(), "%Y-%m-%d-%H-%M-%S"),
                       Time_zone = "UTC", param = NULL, allparams = "true", avgtime = 1, Valid = TRUE, unflagged = TRUE, flushtime=120, Verbose = TRUE) {
    
    # Checking param/allparams and date
    stopifnot(!is.null(param) || allparams == "true")
    stopifnot(lubridate::is.POSIXct(start) || lubridate::is.Date(start))
    
    # start/end date
    start <- format(start, "%Y-%m-%d-%H-%M-%S")
    if (is.null(end)) end <- format(start + 2, "%Y-%m-%d-%H-%M-%S") else {
        stopifnot(lubridate::is.POSIXct(end) || lubridate::is.Date(end))
        end <- format(end, "%Y-%m-%d-%H-%M-%S")}
    
    # 1) Grab the data
    if (avgtime %in% c(1,10,15,60)) {
        if (avgtime ==  1) {
            if(grepl("jrc-api.ecometer.it", URL)){
                aggregation <- NA
                AvgTime <- 1
            } else if(grepl("ubis-air.a-i-p.com", URL)){
                aggregation <- NA
                AvgTime <- 60
            } else stop("[a_i_p_data] unknown URL")
        } else if (avgtime == 10) {
            if(grepl("jrc-api.ecometer.it", URL)){
                aggregation <- NA
                AvgTime <- 10
            } else if(grepl("ubis-air.a-i-p.com", URL)){
                aggregation <- NA
                AvgTime <- 600
            } else stop("[a_i_p_data] unknown URL")
        } else if (avgtime == 15) {
            if(grepl("jrc-api.ecometer.it", URL)){
                aggregation <- NA
                AvgTime <- 1
            } else if(grepl("ubis-air.a-i-p.com", URL)){
                aggregation <- "15m+Mean+Value"
                AvgTime <- 60
            } else stop("[a_i_p_data] unknown URL")
        } else if (avgtime == 60) {
            if(grepl("jrc-api.ecometer.it", URL)){
                aggregation <- NA
                AvgTime <- 60
            } else if(grepl("ubis-air.a-i-p.com", URL)){
                aggregation <- "1h+Mean+Value"
                AvgTime <- 600
            } else stop("[a_i_p_data] unknown URL")
        }
    } else {return(futile.logger::flog.info("[a_i_p_data] avgtime shall be 1, 10, 15 or 60"))}
    
    if(Verbose) futile.logger::flog.info("Sending JSON request: \n")
    if(allparams == "false"){
        JSON.request <- paste0(URL, "username=", username,
                               "&password=", password,
                               "&organisation=", organisation,
                               #"&station=", station,
                               gsub(" ", "", paste(paste("&station=",station, collapse = ""))),
                               paste(paste0("&component=",param), collapse = ""),
                               #"&allparams=",allparams,
                               "&start=", start,
                               "&end=", end,
                               "&valid=true",
                               "&avgtime=", AvgTime,
                               ifelse(!is.na(aggregation),paste0("&aggregation=", aggregation),""),
                               "&unflagged=", ifelse(unflagged, "true", "false"),
                               ifelse(flushtime>0,paste0("&flushtime=",flushtime),""))
    } else JSON.request <- paste0(URL, "username=", username,
                                  "&password=", password,
                                  "&organisation=", organisation,
                                  #"&station=", station,
                                  gsub(" ", "", paste(paste("&station=",station, collapse = ""))),
                                  "&allparams=",allparams,
                                  "&start=", start,
                                  "&end=", end,
                                  "&valid=true",
                                  "&avgtime=", AvgTime,
                                  ifelse(!is.na(aggregation),paste0("&aggregation=", aggregation),""),
                                  "&unflagged=", ifelse(unflagged, "true", "false"),
                                  ifelse(flushtime>0,paste0("&flushtime=",flushtime),""))
    if(Verbose) futile.logger::flog.info(paste0(JSON.request,"\n"))
    Ref.JSON <- httr::GET(utils::URLencode(JSON.request))
    if (Ref.JSON$status_code != 200) {
        if(Verbose) {
            futile.logger::flog.warn("Invalid JSON request")
            futile.logger::flog.warn(paste0("HTTP status code :", Ref.JSON$status_code))}
        return(futile.logger::flog.info(Ref.JSON$request$url))
    } else if(Verbose) futile.logger::flog.info("[a_i_p_data] successful JSON request with HTTP status = 200, Ok")
    #----------------------------------------------------------------CR
    # 2) Extract the data from the JSON file ====
    #----------------------------------------------------------------CR
    # extract the data node
    Ref.Parsed <- httr::content(Ref.JSON, type = "application/json", as = 'parsed')
    if (exists("Ref.JSON")) remove(Ref.JSON)
    # Initialising Ref, all devices in Ref[[1]]
    Ref = list()
    for (Udap in seq(Ref.Parsed$Stations)) {
        if (Udap == 1){
            Ref[[1]] <- Ref.Parsed$Stations[[1]]$Devices  
        } else Ref[[1]] <- c(Ref[[1]],Ref.Parsed$Stations[[Udap]]$Devices)}
    if (exists("Ref.Parsed")) remove(Ref.Parsed)
    # determining Units
    Units      <- unlist(sapply(seq_along(Ref[[1]]), function(Device) Ref[[1]][[Device]]$Components[[1]]$Unit))
    # determining Component
    Components <- unlist(sapply(seq_along(Ref[[1]]), function(Device) Ref[[1]][[Device]]$Components[[1]]$Component))
    
    # Measured Values, list of dataframes for each component
    MeasuredValues <- purrr::map(Ref[[1]], function(Refi) {
        # Making one data frame per list
        if (exists("Param.i")) {
            Param.i <- data.table::rbindlist(Param.i, Refi$Components[[1]]$MeasuredValues)
        } else Param.i <- data.table::rbindlist(Refi$Components[[1]]$MeasuredValues)
        if ("!" %in% param) param <- strsplit(param, "!")[[1]] 
        if ("Value" %in% names(Param.i) && Refi$Components[[1]]$Component %in% param) {
            futile.logger::flog.info(paste0("Component ",stringr::str_pad(which(Components %in% Refi$Components[[1]]$Component), 2, side = "left", pad = " "),"/",length(seq_along(Ref)),": ",
                                            stringr::str_pad(Refi$Components[[1]]$Component, max(nchar(Components)), side = "right", pad = " ")," was correctly downloaded\n"))
            # OPAS added a field Valid in JSON 2025-02-03
            if("Valid" %in% names(Param.i)){
                Param.i <- Param.i[which(Valid), .SD, .SDcols = c("Time", "Value")]
            }
            colnames(Param.i) <- c("date", Refi$Components[[1]]$Component)
            return(Param.i)
        } else {
            futile.logger::flog.info(paste0("Component ",stringr::str_pad(which(Components %in% Refi$Components[[1]]$Component), 2, side = "left", pad = " "),"/",length(seq_along(Refi)), ": ",
                                            stringr::str_pad(Refi$Components[[1]]$Component, max(nchar(Components)), side ="right", pad=" "), " no data to be downloaded or parameter not requested\n"))
        }
    })
    
    # Creating Reference Data
    RefData <- data.table::rbindlist(MeasuredValues[sapply(MeasuredValues, data.table::is.data.table)], use.names = TRUE, fill = TRUE)
    data.table::set(RefData, j = "date", value =  lubridate::ymd_hms(RefData[["date"]], tz = Time_zone))
    RefData <- DF_avg(RefData, width = avgtime)
    # returning
    return(RefData)
}
#================================================================CR
### functions to query find PM density from DMPS and APS ####
#================================================================CR
Distribution_Ref_TS <- function(RefData, DateBegin = NULL, DateEnd = NULL, Sensor_dates = NULL, Min_APS = NULL, Max_APS = NULL,
                                diameters_bins = NULL, units = NULL) {
    # RefData:      a data.table or dataframe that includes all the reference data (dates, coordinates, gas, PM mass concentration, bins 
    #               ("Bin.APS."+diameter in \U00B5m and "Bin.DMPS."+diameter in \U00B5m)...) 
    #               The binned counts and diameters shall not be log transformed. Units of diameters: working with micrometers 
    #               and other units were not tested
    
    # DateBegin,   The start and ending selected dates in POSIXCt format. Default values are NULL, 
    # DateBegin      It is possible to have only one date limit, DateBegin or DateBegin. DateBegin or DateBegin are not discarded.
    # Sensor_dates  vector of POSIXCt: the dates for which the PM sensor provides binned counts. 
    #               In case the sensor PM data series is not complete, dates with missing PM sensor data will be discared from RefData
    
    # Min_APS,      Numeric, Minimum and maximum diameters of reference counts to be selected 
    # Max_APS       for reference counts. Default values are NULL. It is possible to have only one diameter limit,  Min_APS or Max_APS.
    #               Min_APS and Max_APS are discarded.
    
    # Return        a list with data.table RefData (only selected dates and names of bins corresponding to selected diameters),
    #               datafame counts_Ref with column diameters and mean counts over the selected dates
    #               vector with selected APS diameters
    #               vector with DMPS diameter
    #                       
    # select only columns containing .Bin.DMPS & .Bin.APS or Bin.GRIMM
    
    # Selecting dates
    if (!data.table::is.data.table(RefData)) RefData <- data.table::data.table(RefData)
    if ("date" %in% names(RefData) && !all(is.null(c(DateBegin, DateEnd)))) {
        if (!(is.null(DateBegin))) RefData <- RefData[Startdate > DateBegin]
        if (!(is.null(DateEnd)))   RefData <- RefData[date <= DateEnd]
    } else stop("RefData is missing a column \"date\"")
    if (!is.null(Sensor_dates))  RefData <- RefData[date %in% Sensor_dates]
    if (any(is.na(RefData[, .SD, .SDcols = -which(names(RefData) %in% c("date","Startdate"))]))) {
        futile.logger::flog.info(paste0("RefData has missing data at the selected date between ", DateBegin, " and ", DateEnd,"\n"))
        return(list(RefData_filtered = NULL, counts_Ref = NULL, Diam_DMPS = NULL, Diam_APS = NULL))
    } else {
        # Dropping diameters for APS and DMPS
        if (!all(is.null(c(Min_APS, Max_APS)))) {
            # Vector of diameters measured by the APS instrument
            Diam_APS <- names(RefData)[grep(pattern = "Bin.APS.", x = names(RefData) )] %>%
                gsub(pattern = "Bin.APS.", replacement = "", x = .) %>%
                as.numeric
            Diam_APS.init <- Diam_APS
            ### APS diameters to be dropped, i. e. <= 0.89 | >= 11.97
            if (!(is.null(Min_APS)) && is.numeric(Min_APS)) Diam_APS2remove <- Diam_APS[Diam_APS <= Min_APS]
            if (!(is.null(Max_APS)) && is.numeric(Max_APS)) Diam_APS2remove <- c(Diam_APS2remove, Diam_APS[Diam_APS >= Max_APS])
            # Discarding diameters if any diameters < Min_APS or > Max_APS
            if (length(Diam_APS2remove) > 0) Diam_APS <- Diam_APS[-which(Diam_APS %in% Diam_APS2remove)] else stop("Min_APS, Max_APS not defined")
            
            # Vector of diameters measured by the DMPS instrument
            Diam_DMPS <- names(RefData)[grep(pattern = "Bin.DMPS.", x = names(RefData) )] %>%
                gsub(pattern = "Bin.DMPS.", replacement = "", x = .) %>%
                as.numeric
            Diam_DMPS.init <- Diam_DMPS
            ### DMPS diameters to be dropped, keeping the diameters with APS to kepp + 1 diamaters lower and one upper
            Diam_DMPS2remove <- Diam_DMPS[which(Diam_DMPS <= Min_APS)[-length(which(Diam_DMPS <= Min_APS))]]
            Diam_DMPS2remove <- c(Diam_DMPS2remove, Diam_DMPS[which(Diam_DMPS >= Max_APS)][-1])
            # Discarding diameters if any diameters < Min_APS or > Max_APS
            if (length(Diam_DMPS2remove) > 0) Diam_DMPS <- Diam_DMPS[-which(Diam_DMPS %in% Diam_DMPS2remove)]
            
            # check if diameters of APS and DMPS are not Null (this is the case when we use reference data from GRIMM)
            if (length(Diam_APS != 0) && length(Diam_DMPS) != 0) {
                ### Remove "Bin.DMPS." and "Bin.APS." from column names of RefData
                names(RefData) = gsub(pattern = paste0(c("Bin.DMPS.", "Bin.APS."),collapse = "|"), 
                                      replacement = "", x = names(RefData))
                
                # change counts with means
                counts_Ref     <- data.table::transpose(RefData[,.SD,.SDcols = grep(pattern = paste(c("date", "density"),collapse = "|"), names(RefData), invert = T)][,lapply(.SD, mean)])
                counts_Ref[,diameters := as.numeric(names(RefData)[grep(pattern = paste(c("date", "density"),collapse = "|"), names(RefData), invert = T)])]
                counts_Ref[,Instrument := ifelse(diameters %in% Diam_APS.init,"APS","DMPS")]
                names(counts_Ref) <- c("counts", "diameters", "Instrument")
                counts_Ref <- counts_Ref[counts > 0]
                # typical case when we have GRIMM data (then diameters from APS and DMPS = 0)
            } else if (length(Diam_APS == 0) | length(Diam_DMPS) == 0)  {
                counts_Ref <- colMeans(RefData[which(names(RefData) != "date")] , na.rm = T)
                counts_Ref <- as.data.frame((counts_Ref))
                counts_Ref[diameters := as.numeric(diameters_bins)]
                rownames(counts_Ref) <- NULL
                names(counts_Ref) <- c("counts", "diameters")
                counts_Ref <- counts_Ref %>%
                    dplyr::filter(counts > 0)
                if (units == "count.mL-1") {
                    counts_Ref <- counts_Ref %>%
                        mutate(counts = counts/1000)}
            }
            # returning
            if (!nrow(counts_Ref) == 0 ) {
                return(list(RefData_filtered = RefData, counts_Ref = counts_Ref, Diam_DMPS = Diam_DMPS, Diam_APS = Diam_APS))
            } else {
                futile.logger::flog.info("Reference data is empty!\n")
                return(NA)
            }  
        } else {
            stop("Min and Max diameters for fitting APS not defined")
        }
    } 
}
Plot_Dist_Ref <- function(counts_Ref, Model.i = NULL) {
    # counts_Ref      : data.table or dataframe with diameters and counts
    # Plotting distribution not in log of counts versus diameters and modelled distribution if Model.i is not NULL
    
    # changes names of count_ref in case it uses x and y as for fitting model for density determination
    if (any(c("x", "y") %in% names(counts_Ref))) names(counts_Ref) <- c("diameters", "counts")
    
    if  (is.null(Model.i)) {
        
        plot <-  ggplot2::ggplot() + 
            theme_bw() +
            ggplot2::geom_point(data = counts_Ref, ggplot2::aes((diameters), (counts), colour=Instrument), stat = "identity", fill = "gray") +
            #   geom_line(data = augmented, ggplot2::aes(exp(x), exp(.fitted),  col = "Modelled"), size = 2) +
            theme(axis.title.x = element_text(colour  = "black", size = 15),
                  axis.text.x  = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15, colour = "black")) +
            theme(axis.title.y = element_text(colour = "black", size = 15),
                  axis.text.y  = element_text(angle = 0, vjust = 0.5, size = 15, colour = "black")) +
            xlab(expression(paste("diameter (\U00B5m)"))) + 
            ylab(expression(paste("counts per ml"))) 
        
    } else {
        # if there is model filtted in log, all the plot shall be in log
        x <- names(Model.i$Augment)[2]
        y <- names(Model.i$Augment)[1]
        plot <-  ggplot2::ggplot() + 
            theme_bw() +
            ggplot2::geom_point(data = counts_Ref, ggplot2::aes(x = !!ensym(x), y = !!ensym(y), col = "Ref"), stat = "identity", fill = "gray") +
            geom_line(data = Model.i$Augment, ggplot2::aes(x = !!ensym(x), (.fitted),  col = "Modelled"), size = 1) +
            scale_color_manual(values = c("Modelled" = "blue", "Ref" = "black")) +
            theme(axis.title.x = element_text(colour = "black", size = 15),
                  axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size = 15, colour = "black")) +
            theme(axis.title.y = element_text(colour = "black", size = 15),
                  axis.text.y  = element_text(angle=0, vjust=0.5, size = 15, colour = "black"))
        #xlab(expression(paste("log of diameter (\U00B5m)"))) + 
        #ylab(expression(paste("log of tot # of counts"))) 
    }
    
    return(plot)
} 
Plot_Dist_Ref_log <- function(counts_Ref, Model.i = NULL, Count) {
    
    # Plotting distribution in log and modelled distribution if Model.i is not NULL
    
    # changes names of count_ref in case it uses x and y as for fitting model for density determination
    if (any(c("x", "y") %in% names(counts_Ref))) names(counts_Ref) <- c("diameters", "counts")
    
    if  (is.null(Model.i)) {
        
        plot <-  ggplot2::ggplot() + 
            theme_bw() +
            ggplot2::geom_point(data = counts_Ref, ggplot2::aes(log10(diameters), colour = Instrument, log10(counts)), stat = "identity", fill = "gray") +
            #   geom_line(data = augmented, ggplot2::aes(exp(x), exp(.fitted),  col = "Modelled"), size = 2) +
            theme(axis.title.x = element_text(colour  = "black", size = 15),
                  axis.text.x  = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15, colour = "black")) +
            theme(axis.title.y = element_text(colour = "black", size = 15),
                  axis.text.y  = element_text(angle = 0, vjust = 0.5, size = 15, colour = "black")) +
            xlab(expression(paste("log10 of diameter (\U00B5m)"))) + 
            ylab(expression(paste("log10 of counts / log of diameters"))) 
        
    } else {
        
        plot <-  ggplot2::ggplot() + 
            theme_bw() +
            ggplot2::geom_point(data = counts_Ref, ggplot2::aes(log10(diameters), colour = Instrument, log10(counts), col = "Ref"), stat = "identity", fill = "gray") +
            geom_line(data = Model.i$Augment, ggplot2::aes((x), (.fitted),  col = "Modelled"), size = 1) +
            scale_color_manual(values = c("Modelled" = "blue", "Ref" = "black")) +
            theme(axis.title.x = element_text(colour = "black", size = 15),
                  axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size = 15, colour = "black")) +
            theme(axis.title.y = element_text(colour = "black", size = 15),
                  axis.text.y  = element_text(angle=0, vjust=0.5, size = 15, colour = "black")) +
            xlab(expression(paste("diameter (\U00B5m)"))) + 
            ylab(expression(paste("tot # of counts"))) 
    }
    
    return(plot)
} 
f_Error <- function(density, DataXY.APS, Model, Log10 = TRUE){
    # density       : to be fitted
    # DataXY.APS    : data.table with column diameters
    # error function for determining density with optmise function
    #cat(paste0("density: ", density, "\n"))
    if (Log10) {
        DataXY.APS[, diam.eq := log10(10^DataXY.APS[,names(Model$model)[2], with = FALSE]/sqrt(density))] 
    } else DataXY.APS[, diam.eq := DataXY.APS[,names(Model$model)[2], with = FALSE]/sqrt(density)]
    DataXY.APS[, Predicted_APS.counts := Model$coefficients[1] + Model$coefficients[2] * diam.eq]
    #cat(paste0("RSS: ", sum(DataXY.APS[[y]] - DataXY.APS$Predicted_APS.counts)^2, "\n"))
    return(sum(DataXY.APS[[names(Model$model)[1]]] - DataXY.APS$Predicted_APS.counts)^2)
}
density.DMPS.APS <- function(RefData, DateBegin = RefData$Startdate, DateEnd = RefData$date, Min_APS = 0.62, Max_APS =  0.800, verbose = TRUE, Log10 = TRUE) {
    # Determine the density of PM in order to fit the log- normal distribution of dN/dlogD of DMPS and APS
    
    # RefData:      one row of a data.table or dataframe that includes all the reference data of DMPS and APS dN/dlog+ date and Startdate 
    #               ("Bin.APS."+diameter in \U00B5m and "Bin.DMPS."+diameter in \U00B5m)...) 
    #               The binned counts and diameters shall not be log transformed. Units of diameters: working with micrometers 
    #               and other units were not tested
    
    # DateBegin,    The start and ending selected dates in POSIXCt format. Default values are NULL, 
    # DateBegin     It is possible to have only one date limit, DateBegin or DateBegin. DateBegin or DateBegin are not discarded.
    # Sensor_dates  vector of POSIXCt: the dates for which the PM sensor provides binned counts. 
    #               In case the sensor PM data series is not complete, dates with missing PM sensor data will be discared from RefData
    
    # Min_APS,      Numeric, Minimum and maximum diameters of reference counts to be selected 
    # Max_APS       for reference counts. Default values are NULL. It is possible to have only one diameter limit,  Min_APS or Max_APS.
    #               Min_APS and Max_APS are discarded.
    
    # Return        a vector of density, with NA when data are missing
    
    # gather data in list and dataframe
    # Compute mean of reference count
    Cols.DMPS.APS <- c("date", "Startdate", names(RefData)[grep(pattern = paste(c("Bin.DMPS", "Bin.APS"), collapse = "|"), names(RefData))])
    Dist_Ref.d_TS <- Distribution_Ref_TS(RefData = RefData[,..Cols.DMPS.APS], DateBegin = RefData$Startdate, DateEnd = RefData$date, Min_APS = 0.62, Max_APS =  0.800) #
    
    # # Plot of distribution of reference counts versus diameter in log/log
    # # lognormal distribution (plot)
    # (Plot_Dist_Ref_log(Dist_Ref.d_TS$counts_Ref))
    # # normal distribution (plot)
    # (Plot_Dist_Ref(Dist_Ref.d_TS$counts_Ref))
    
    # Determining of density with Dist_Ref.d_TS$Diam_DMPS, and Dist_Ref.d_TS$Diam_APS
    if (!is.null(Dist_Ref.d_TS$RefData_filtered)) {
        # Fitting DMPS
        DataXY.DMPS <- Dist_Ref.d_TS$counts_Ref[diameters %in% Dist_Ref.d_TS$Diam_DMPS]
        # Correcting APS diameters
        DataXY.APS <- Dist_Ref.d_TS$counts_Ref[diameters %in% Dist_Ref.d_TS$Diam_APS]
        # Fitting on the log of counts of DMPS
        if (Log10) {
            DataXY.DMPS[, log.diam   := log10(diameters)]
            DataXY.DMPS[, log.counts := log10(counts)]
            Model <- lm(log.counts ~ log.diam, data = DataXY.DMPS, model = TRUE, x = TRUE, y = TRUE) 
            DataXY.APS[, log.counts := log10(counts)]
            DataXY.APS[, log.diam   := log10(diameters)]
        } else Model   <- lm(counts     ~ diameters, data = DataXY.DMPS, model = TRUE, x = TRUE, y = TRUE)
        density_fun <- optimise(f_Error, c(0.3, 4), DataXY.APS = DataXY.APS, Model = Model, maximum = FALSE, tol = 0.001, Log10 = Log10) # limit ranges for the density
        # Plotting with correction for density
        if (verbose) {
            Model.i <- list(Tidy = tidy(Model), 
                            Augment = augment(Model), 
                            Glance = broom::glance(Model), 
                            Call = Model$call, Coef = coef(Model),
                            Model = Model)
            
            # fitted DMPS reference data
            Plot.DMPS       <- Plot_Dist_Ref(DataXY.DMPS, Model.i = Model.i)
            # add raw APS reference data
            if (Log10) {
                # add raw APS reference data
                Plot.DMPS.APS   <- Plot.DMPS + ggplot2::geom_point(data = DataXY.APS, ggplot2::aes(log.diam, log.counts), stat = "identity", col = "red")
                # raw data + minimized data (APS)
                Plot.DMPS.APS.d <- Plot.DMPS.APS + ggplot2::geom_point(data = DataXY.APS, ggplot2::aes(diam.eq, log.counts), stat = "identity", shape = 17, col = "red", size = 3)
            } else {
                # add raw APS reference data
                x <- names(DataXY.APS)[2]
                y <- names(DataXY.APS)[1]
                Plot.DMPS.APS   <- Plot.DMPS + ggplot2::geom_point(data = DataXY.APS, ggplot2::aes(x = !!ensym(x), y = !!ensym(y)), stat = "identity", col = "red")
                # raw data + minimized data (APS)
                Plot.DMPS.APS.d <- Plot.DMPS.APS + ggplot2::geom_point(data = DataXY.APS, 
                                                                       ggplot2::aes(diam.eq, y = !!ensym(y)), stat = "identity", shape = 17, col = "red", size = 3)
            }
            
            # plot 
            (Plot.DMPS.APS.d)
        }
        cat(paste0(DateEnd,", density: ",density_fun$minimum,"\n"))
        return(density_fun$minimum)
    }  else {
        cat(paste0(DateEnd,"Reference data is empty!"))
        return(NA)
    } 
}
PM_Volumes.Conc <- function(RefData) {
    # RefData.hours     : one row of data.table with columns date, bin.DMPS, bin.DMPS and density
    # Diameters DMPS
    
    # gather data in list and dataframe
    # Compute mean of reference count
    Cols.DMPS.APS <- c("date", "Startdate", names(RefData)[grep(pattern = paste(c("Bin.DMPS", "Bin.APS"), collapse = "|"), names(RefData))])
    Dist_Ref.d_TS <- Distribution_Ref_TS(RefData = RefData[,..Cols.DMPS.APS], DateBegin = RefData$Startdate, DateEnd = RefData$date, Min_APS = 0.62, Max_APS =  0.800)$counts_Ref
    # Calculating the volume per bins for DMPS
    Dist_Ref.d_TS[Instrument == "DMPS", Vol.Bins := counts * diameters^3 * pi / 6 * log10(diameters/data.table::shift(diameters, fill = NA))]
    tot.vol.DMPS <- Dist_Ref.d_TS[Instrument == "DMPS",sum(Vol.Bins, na.rm = T)]
    # Calculating the volume per bins for APS
    Dist_Ref.d_TS[Instrument == "APS", diam.eq  := diameters/sqrt(RefData$density.log)]
    Dist_Ref.d_TS[Instrument == "APS", Vol.Bins := counts * diam.eq^3 * pi / 6 * log10(diameters/data.table::shift(diameters, fill = NA))]
    # dropping the column with diam.eq < 0.800
    Dist_Ref.d_TS <- Dist_Ref.d_TS[Instrument == "DMPS" | (Instrument == "APS" & diam.eq > 0.800)]
    # Adding the Diameter of DMPS to diam.eq in order to compute volume PM2.5 and PM10
    data.table::set(Dist_Ref.d_TS, i = which(Dist_Ref.d_TS$Instrument == "DMPS"), j = "diam.eq", value = Dist_Ref.d_TS[Dist_Ref.d_TS$Instrument == "DMPS",diameters])
    # Computing volumes
    APS.Vol.PM2.5 <- Dist_Ref.d_TS[Instrument == "APS" & diam.eq > 0.8 & diam.eq <= 2.5, sum(Vol.Bins, na.rm = T)]
    Tot.Vol.PM2.5 <- tot.vol.DMPS + APS.Vol.PM2.5
    PM2.5 <- Tot.Vol.PM2.5 * RefData$density.log
    APS.Vol.PM10 <- Dist_Ref.d_TS[Instrument == "APS" & diam.eq > 0.8 & diam.eq <= 10,sum(Vol.Bins, na.rm = T)]
    Tot.Vol.PM10  <- tot.vol.DMPS + APS.Vol.PM10
    PM10 <- Tot.Vol.PM10 * RefData$density.log
    return(list(Dist_Ref = Dist_Ref.d_TS, tot.vol.DMPS = tot.vol.DMPS, 
                APS.Vol.PM2.5 = APS.Vol.PM2.5, Tot.Vol.PM2.5 = Tot.Vol.PM2.5, PM2.5 = PM2.5,
                APS.Vol.PM10 = APS.Vol.PM10, Tot.Vol.PM10 = Tot.Vol.PM10, PM10 = PM10))
}

#================================================================CR
### functions to update config of sensors from rhansomeTable ####
#================================================================CR
Update.Hansome.Config <- function(Table_Config, DT_Filtering, DT_Calib.cfg, DT_CalTime){
    # return Table_Config
    #
    # Table_Config      : data.table in whcih to add the whole configuration of sensors
    # DT_Filtering      : RhandSomeTable of tab FilteringMain
    # DT_Calib.cfg      : RhandSomeTable of tab CalibMain
    # DT_CalTime        : RhandSomeTable of tab SetTimeMain
    
    if (!is.null(DT_Filtering)) {
        Table_Config <- merge(hot_to_r(DT_Filtering), 
                              Table_Config[, c("name.gas", "name.sensor", setdiff(names(Table_Config), names(hot_to_r(DT_Filtering)))), with = FALSE],
                              by = c("name.gas", "name.sensor"), all = TRUE)} 
    if (!is.null(DT_Calib.cfg)) {
        Table_Config <- merge(hot_to_r(DT_Calib.cfg), 
                              Table_Config[, c("name.gas", "name.sensor", setdiff(names(Table_Config), names(hot_to_r(DT_Calib.cfg)))), with = FALSE],
                              by = c("name.gas", "name.sensor"), all = TRUE)} 
    if (!is.null(DT_CalTime)) {
        Table_Config <- merge(hot_to_r(DT_CalTime), 
                              Table_Config[, c("name.sensor", setdiff(names(Table_Config), names(hot_to_r(DT_CalTime)))), with = FALSE],
                              by = c("name.sensor"), all = TRUE)} 
    setcolorder(Table_Config, Columns <- c("name.gas", "name.sensor", "gas.sensor", "Sens.raw.unit", "Sens.unit", "gas.reference", "gas.reference2use", "ref.unitgas", 
                                           "Cal.Line", "Cal.func", "mod.eta.model.type", "Neg.mod", "Slope", "Intercept", "ubsRM", "ubss", "Sync.Cal", "Sync.Pred", "eta.model.type", 
                                           "hoursWarming", "temp.thres.min", "temp.thres.max", "rh.thres.min", "rh.thres.max", "Sens.Inval.Out", "Sens.rm.Out", 
                                           "Sens.window", "Sens.threshold", "Sens.Ymin", "Sens.Ymax", "Sens.ThresholdMin", "Sens.iterations", "remove.neg", 
                                           "Ref.rm.Out", "Ref.window", "Ref.threshold", "Ref.Ymin", "Ref.Ymax", "Ref.ThresholdMin", "Ref.iterations"))
    return(Table_Config)
}

#================================================================CR
# Functions for the calibration of sensors ####
#================================================================CR
# list of Calibration models for one or all sensors of one ASE box ====
#' list of Calibration models for one or all sensors of one ASE box
#'
#' @param ASEDir A character vector with the file path of the ASE Box for which to look for models. It should be something like file.path("shiny", Project, ASE.name).
#' @param name.sensor Character vector, character vector with the name(s) of sensors to look for models. Default is "ALL" that returns the models for all sensors of the ASE box.
#' @param DIR_Config Name of the subdirectory of ASEDir where is the file ASE.name.cfg.
#' @param DIR_Models Name of the subdirectory of ASEDir where are the calibration models. Default is "Models".
#' @param Median logical, default FALSE. IF False all model names including "Median" are discarded.
#' @return a vector of model files in ASEdir without filepath.
#' @examples
#' List_models(ASEDir = ASEDir)
List_models <- function(ASEDir, name.sensor = "ALL", DIR_Config = "Configuration", DIR_Models = "Models", Median = FALSE, Verbose = TRUE, List.models = NULL) {
    if (!is.null(List.models)) rm(List.models)
    if (name.sensor == "ALL") {
        # name of ASE box
        ASE.name    <- basename(ASEDir)
        ASE.cfg     <- data.table::fread(file.path(ASEDir, DIR_Config, paste0(ASE.name,".cfg")), showProgress = F)
        name.sensor <- unlist(ASE.cfg[ASE.cfg$name.gas == "name.sensor"])[-1]
        List.models <- sapply(name.sensor, function(i) {
            List.models <- list.files(file.path(ASEDir, DIR_Models),pattern = glob2rx(paste0(basename(ASEDir),"__",i,"__*")))
        })
    } else {
        if (length(list.files(path = file.path(ASEDir,DIR_Models), pattern = glob2rx(paste0(basename(ASEDir),"__",name.sensor,"__*")))) > 0) {
            List.models <-  list.files(path = file.path(ASEDir,DIR_Models), pattern = glob2rx(paste0(basename(ASEDir),"__",name.sensor,"__*")))
        } else {
            List.models <- NULL
            if (Verbose) futile.logger::flog.error(paste0("[List_models] No model found for sensor ", name.sensor))
            List.sensors <- paste(unique(sapply(list.files(file.path(ASEDir, DIR_Models), pattern = glob2rx(paste0(basename(ASEDir),"__*"))), function(i) strsplit(i, split = "__")[[1]][2])), collapse = ", ")
            if (Verbose) futile.logger::flog.info(paste0("[List_models] Available calibration for sensors ", List.sensors))
        }
    } 
    if (Median) List.models <-  List.models[grep(pattern = "Median", List.models, invert = TRUE)]
    if (exists("List.models") && length(List.models) > 0) return(List.models) else return()
}
#' Identify list of characteristics for one sensor of one ASE box determined using a ASEDir
#' Description: Influx data shall be available to identify an AirSensEUR box
#' @param ASEDir mandatory, a character vector with the file path of the ASE box to be submitted to the function Identify.ASE
#' @param name.sensor optional, character vector, default is NULL. if NULL the first sensor in ASE.cfg will be used.
#' @param General.DT Optional, a data.table with all ASE box data. Default is NULL. If NULL the General.csv file is loaded
#' @param ASE.cfg Optional, a data.table with all ASE box configuration. Default is NULL. If NULL the ASE.cfg file is loaded
#' @param SetTime Optional, a data.table with all ASE box SetTime configuration. Default is NULL. If NULL the ASE_SETTIME.cfg file is loaded
#' @param Config Optional, a List, default is NULL, returned by function CONFIG(). If NULL, Config is loaded using function CONFIG(). If General.DT is given, Config shall be given as well.
#' @param DIR_Config Optional, default is "Configuration", file path of the sub-directory of ASEDir where is the file ASE.name.cfg.
#' @param DIR_Models Optional, default is "Models", file path of the sub-directory of ASEDir where are the calibration models.
#' @param DIR_General Optional, default is "General_data", file path of the sub-directory of ASEDir where is the file General.csv.
#' @param Filter.Sens Optional, logical default is TRUE. If FALSE filtering of sensor data is not performed.
#' @param Filter.Ref Optional, logical default is TRUE. If FALSE  filtering of reference data is not performed.
#' @param Select.Columns Optional, logical default is FALSE. If TRUE the columns listed in Dropped.Columns are dropped when reading General.csv.
#' @param Dropped.Columns Optional, character vector default is FALSE. List of pattern of Dropped Columns are dropped when reading General.csv.
#' @param ASE.name Optional default is basename(ASEDir). Name of AirSensEUR box coming from the SD of the host.
Identify_ASE_Dir <- function(ASEDir, name.sensor = NULL, General.DT = NULL, ASE.cfg = NULL, SetTime = NULL, Config = NULL, Shield = NULL,
                             DIR_Config = "Configuration", DIR_Models = "Models", DIR_General = "General_data", Filter.Sens = TRUE, Filter.Ref = TRUE,
                             Select.Columns = FALSE, Dropped.Columns = c(".Warm.", ".TRh", ".Invalid","Bin"), ASE.name = basename(ASEDir)) {
    # Extracting data 
    if (is.null(Config)) Config <- CONFIG(DisqueFieldtestDir = ASEDir, DisqueFieldtest = dirname(dirname(ASEDir)), Dir.Config = DIR_Config, Verbose = F, shiny = F)
    if (is.null(Shield)){
        if(is.null(Config$sens2ref.shield)) Shield <- ASEPanel04Read(ASEPanel04File = file.path(dirname(dirname(ASEDir)), "Shield_Files", Config$Server$asc.File)) else Shield <- Config$sens2ref.shield} 
    if (is.null(ASE.cfg)){
        if(is.null(Config$sens2ref.shield)) ASE.cfg <- data.table::fread(file.path(ASEDir, DIR_Config, paste0(ASE.name,".cfg")), showProgress = F) else {
            ASE.cfg <- data.table::transpose(Config$sens2ref, make.names = 1, keep.names = "name.gas")}} 
    if (is.null(SetTime)){
        SetTime <- data.table::fread(file.path(ASEDir, DIR_Config, paste0(ASE.name,"_SETTIME.cfg")), showProgress = F)
        # Correcting old naming of DateCal.IN in Cal.IN and Out.Ref.IN in Ref.IN
        if(any(grepl("Out\\.", SetTime$name.gas)) || any(grepl("Date", SetTime$name.gas)) || any(grepl("Date\\.", SetTime$name.gas))){
            if (any(grepl("Date.", SetTime$name.gas))) data.table::set(SetTime, j = "name.gas", value = gsub("Date\\.","", SetTime$name.gas))
            if (any(grepl("Date", SetTime$name.gas)))  data.table::set(SetTime, j = "name.gas", value = gsub("Date","", SetTime$name.gas))
            if (any(grepl("Out.", SetTime$name.gas)))  data.table::set(SetTime, j = "name.gas", value = gsub("Out\\.","", SetTime$name.gas))
            data.table::fwrite(SetTime, file = file.path(ASEDir, DIR_Config, paste0(ASE.name,"_SETTIME.cfg")))}}
    if (is.null(name.sensor)) name.sensor <- unlist(ASE.cfg[ASE.cfg$name.gas == "name.sensor"])[2]
    k <- which(unlist(ASE.cfg[ASE.cfg$name.gas == "name.sensor"]) == name.sensor)
    if(!is.finite(k)) stop(paste0("[Identify_ASE_Dir] unknow sensor ", name.sensor, "in AirSensEUR ", ASE.name))
    Cal.func <- unlist(ASE.cfg[ASE.cfg$name.gas == "Cal.func"  , k, with = FALSE])
    if (exists("Cal.func") && shiny::isTruthy(Cal.func)) {
        # name of ASE box
        Stripped.Model <- unlist(strsplit(Cal.func, split = "__"))
        # Extract Sens.raw.unit
        Sens.raw.unit  <- Stripped.Model[1]
        # Extract Mod_type
        Mod_type <- Stripped.Model[2]
        # Extract Cal.DateIN and Cal.DateEND
        Cal.DateIN     <- as.Date(Stripped.Model[3], format = " %Y%m%d", optional = T)
        Cal.DateEND    <- as.Date(Stripped.Model[4], format = " %Y%m%d", optional = T)
        # Extract Variables
        Variables   <- ifelse(Stripped.Model[5] != ".rdata", sub(pattern = ".rdata", replacement = "",Stripped.Model[5]), NA)
    } else {
        Cal.DateIN     <- max(c(as.Date(SetTime[name.gas == "Cal.IN"  ][[k]], optional = T), as.Date(SetTime[name.gas == "DatePlotCal.IN"  ][[k]], optional = T)), na.rm = TRUE)
        Cal.DateEND    <- min(c(as.Date(SetTime[name.gas == "Cal.END" ][[k]], optional = T), as.Date(SetTime[name.gas == "DatePlotCal.END" ][[k]], optional = T)), na.rm = TRUE)
        Sens.raw.unit  <- unlist(ASE.cfg[ASE.cfg$name.gas == "Sens.raw.unit", k, with = FALSE])
        Mod_type       <- unlist(ASE.cfg[ASE.cfg$name.gas == "Mod_type"     , k, with = FALSE])
        Variables      <- NA}
    name.gas       <- names(which(unlist(ASE.cfg[ASE.cfg$name.gas == "name.sensor"]) == name.sensor))
    gas.sensor     <- unlist(ASE.cfg[ASE.cfg$name.gas == "gas.sensor", k, with = FALSE])
    gas.reference2use <- unlist(ASE.cfg[ASE.cfg$name.gas == "gas.reference2use", k, with = FALSE])
    Meas.DateIN    <- max(c(as.Date(SetTime[name.gas == "Meas.IN" ][[k]], optional = T), as.Date(SetTime[name.gas == "DatePlotMeas.IN" ][[k]], optional = T)), na.rm = TRUE) 
    Meas.DateEND   <- min(c(as.Date(SetTime[name.gas == "Meas.END"][[k]], optional = T), as.Date(SetTime[name.gas == "DatePlotMeas.END"][[k]], optional = T)), na.rm = TRUE)
    nameGasVolt    <- paste0(name.sensor,"_volt")      # sensor gas in volt or nA or Count
    nameGasMod     <- paste0(gas.sensor,"_modelled")   # modelled sensor gas
    nameGasRef     <- paste0("Out.",gas.reference2use) # Gas reference
    unit.ref       <- unlist(ASE.cfg[ASE.cfg$name.gas == "ref.unitgas"  , k, with = FALSE])
    unit.sensor    <- unlist(ASE.cfg[ASE.cfg$name.gas == "Sens.unit"    , k, with = FALSE])
    remove.neg     <- as.logical(unlist(ASE.cfg[ASE.cfg$name.gas == "remove.neg", k, with = FALSE]))
    ubsRM          <- unlist(ASE.cfg[ASE.cfg$name.gas == "ubsRM"     , k, with = FALSE])
    ubss           <- unlist(ASE.cfg[ASE.cfg$name.gas == "ubss"      , k, with = FALSE])
    Sync.Cal       <- as.logical(unlist(ASE.cfg[ASE.cfg$name.gas == "Sync.Cal"  , k, with = FALSE]))
    Sync.Pred      <- as.logical(unlist(ASE.cfg[ASE.cfg$name.gas == "Sync.Pred" , k, with = FALSE]))
    var.names.meteo <- data.table::fread(file.path(ASEDir, DIR_Config, "var.names.meteo.cfg"), showProgress =F)$columns
    if ("Temperature" %in% var.names.meteo)           var.names.meteo[var.names.meteo == "Temperature"]           <- "Out.Temperature"
    if ("Temperature_int" %in% var.names.meteo)       var.names.meteo[var.names.meteo == "Temperature_int"]       <- "Out.Temperature_int"
    if ("Relative_humidity" %in% var.names.meteo)     var.names.meteo[var.names.meteo == "Relative_humidity"]     <- "Out.Relative_humidity"
    if ("Relative_humidity_int" %in% var.names.meteo) var.names.meteo[var.names.meteo == "Relative_humidity_int"] <- "Out.Relative_humidity_int"
    if ("Atmospheric_pressure" %in% var.names.meteo)  var.names.meteo[var.names.meteo == "Atmospheric_pressure"]  <- "Out.Atmospheric_pressure"
    eta.model.type <- unlist(ASE.cfg[ASE.cfg$name.gas == "eta.model.type", k, with = FALSE])
    # lists for all sensors
    list.gas.sensor<- unlist(ASE.cfg[ASE.cfg$name.gas == "gas.sensor"])[-1]
    list.sensors   <- unlist(ASE.cfg[ASE.cfg$name.gas == "name.sensor"])[-1]
    list.reference <- unlist(ASE.cfg[ASE.cfg$name.gas == "gas.reference2use"])[-1]
    # General.DT
    if (is.null(General.DT)) {
        General.file <- file.path(ASEDir, DIR_General, "General.csv")
        if (file.exists(General.file)) {
            if (tools::file_ext(General.file) == "csv") {
                if (Select.Columns) {
                    General.DT <- data.table::fread(General.file, nrows = 0)
                    Drop <- grep(paste(Dropped.Columns, collapse = "|"), General.DT)
                    General.DT <- data.table::fread(General.file, drop = Drop) 
                } else General.DT <- data.table::fread(General.file) #, na.strings = getOption("","NA")
                # Convert date to POSIXct
                if (!lubridate::is.POSIXct(General.DT$date)){
                    if (shiny::isTruthy(Config$Server$Influx.TZ) || shiny::isTruthy(Config$Server$SOS.TZ)) {
                        if (shiny::isTruthy(Config$Server$Influx.TZ)) {
                            data.table::set(General.DT, j = "date", value =  Set_date2POSIXct(General.DT[["date"]], tz = Config$Server$Influx.TZ))
                            if("date_PreDelay" %in% names(General.DT)) data.table::set(General.DT, j = "date_PreDelay", value =  Set_date2POSIXct(General.DT[["date_PreDelay"]], tz = Config$Server$Influx.TZ))
                        } else {
                            data.table::set(General.DT, j = "date", value =  Set_date2POSIXct(General.DT[["date"]], tz = Config$Server$SOS.TZ))
                            if("date_PreDelay" %in% names(General.DT)) data.table::set(General.DT, j = "date_PreDelay", value =  Set_date2POSIXct(General.DT[["date_PreDelay"]], tz = Config$Server$SOS.TZ))}
                    } else {
                        data.table::set(General.DT, j = "date"         , value =  Set_date2POSIXct(General.DT[["date"]]         , tz = "UTC"))
                        if("date_PreDelay" %in% names(General.DT)) data.table::set(General.DT, j = "date_PreDelay", value =  Set_date2POSIXct(General.DT[["date_PreDelay"]], tz = "UTC"))}}
            } else if (tools::file_ext(General.file) == "Rdata") {
                General.DT <- load_obj(General.file)
                if (!data.table::is.data.table(General.DT)) General.DT <- data.table::data.table(General.DT, key = "date")}
            if ("V1" %in% names(General.DT)) General.DT[, V1 := NULL]
        } else {
            InfluxData.file <- file.path(ASEDir, DIR_General, "InfluxData.csv")
            SOSData.file    <- file.path(ASEDir, DIR_General, "SOSData.csv")
            RefData.file    <- file.path(ASEDir, DIR_General, "RefData.csv")
            if (file.exists(InfluxData.file)) {
                if (tools::file_ext(InfluxData.file) == "csv") {
                    InfluxData <- data.table::fread(file = InfluxData.file, na.strings = c("","NA", "<NA>"))
                    if (shiny::isTruthy(Config$Server$Influx.TZ)) {
                        data.table::set(InfluxData, j = "date", value =  Set_date2POSIXct(InfluxData[["date"]], tz = Config$Server$Influx.TZ))
                    } else data.table::set(InfluxData, j = "date", value =  Set_date2POSIXct(InfluxData[["date"]], tz = "UTC"))
                } else if (tools::file_ext(InfluxData.file) == "Rdata") {
                    InfluxData <- load_obj(InfluxData.file)
                    if (!data.table::is.data.table(InfluxData)) InfluxData <- data.table::data.table(InfluxData, key = "date")}
                if ("V1" %in% names(InfluxData)) InfluxData[, V1 := NULL]
            } else return(futile.logger::flog.error(paste0("[Identify_ASE_Dir] no sensor data for ASE ", ASE.name)))
            if (file.exists(SOSData.file)) {
                if (tools::file_ext(SOSData.file) == "csv") {
                    SOSData <- data.table::fread(file = SOSData.file, na.strings = c("","NA", "<NA>"))
                    if (shiny::isTruthy(Config$Server$SOS.TZ)) {
                        data.table::set(SOSData, j = "date", value =  Set_date2POSIXct(SOSData[["date"]], tz = Config$Server$SOS.TZ))
                    } else data.table::set(SOSData, j = "date", value =  Set_date2POSIXct(SOSData[["date"]], tz = "UTC"))
                } else if (tools::file_ext(SOSData.file) == "Rdata") {
                    SOSData <- load_obj(SOSData.file)
                    if (!data.table::is.data.table(SOSData)) SOSData <- data.table::data.table(SOSData, key = "date")}
                if ("V1" %in% names(SOSData)) SOSData[, V1 := NULL]
            } else SOSData <- NULL
            if (file.exists(RefData.file)) {
                if (tools::file_ext(RefData.file) == "csv") {
                    RefData <- data.table::fread(file = RefData.file, na.strings = c("","NA", "<NA>"))
                    if (!"" %in% Config$Server$ref.tzone) {
                        data.table::set(RefData, j = "date", value =  Set_date2POSIXct(RefData[["date"]], tz = Config$Server$ref.tzone))
                    } else data.table::set(RefData, j = "date", value =  Set_date2POSIXct(RefData[["date"]], tz = "UTC"))
                } else if (tools::file_ext(RefData.file) == "Rdata") {
                    RefData <- load_obj(RefData.file)
                    if (!data.table::is.data.table(RefData)) RefData <- data.table::data.table(RefData, key = "date")}
                if ("V1" %in% names(RefData)) RefData[, V1 := NULL]
                # Message in case coordinates are not included
                if (!all(c("Ref.Long",  "Ref.Lat") %in% names(RefData))) futile.logger:: flog.warn("[Identify_ASE_Dir] Coordinates of reference station missing")
            } else RefData <- NULL
            
            General.DT <- GENERAL(UserMins = Config$Server$UserMins, Delay =  Config$Server$Delay,
                                  RefData = RefData, InfluxData = InfluxData, SOSData = SOSData,
                                  DownloadSensor = Check_Download(Influx.name = ASE.name,
                                                                  WDinput = file.path(ASEDir, DIR_General), 
                                                                  UserMins = Config$Server$UserMins, 
                                                                  RefData = RefData, 
                                                                  InfluxData = InfluxData, 
                                                                  SOSData = SOSData, Verbose = FALSE))}}
    # Complete General.DT with columns for filtered sensors if needed
    General.DT <- Complete_General(f.list.name.sensor = list.sensors, f.list.gas.sensor = list.gas.sensor, f.General.DT = General.DT, f.Config = Config, 
                                   f.ASEDir = ASEDir, f.Shield = Shield, f.list.reference = list.reference, f.ASE.cfg = ASE.cfg, Filter.Sens = Filter.Sens, Filter.Ref = Filter.Ref)
    return(list(General.DT = General.DT, ASE.cfg = ASE.cfg, SetTime = SetTime, k = k, Config = Config,
                Cal.DateIN = Cal.DateIN, Cal.DateEND = Cal.DateEND,
                ASE.name = ASE.name, ASEDir = ASEDir, name.sensor = name.sensor, name.gas = name.gas, gas.sensor = gas.sensor,
                Mod_type = Mod_type, Variables = Variables,
                Meas.DateIN = Meas.DateIN, Meas.DateEND = Meas.DateEND, 
                nameGasVolt = nameGasVolt, nameGasMod = nameGasMod, nameGasRef = nameGasRef,
                ASESens.raw.unit = Sens.raw.unit, unit.ref = unit.ref, unit.sensor = unit.sensor,
                remove.neg = remove.neg, ubsRM = ubsRM, ubss = ubss, Sync.Cal = Sync.Cal, Sync.Pred = Sync.Pred, 
                var.names.meteo = var.names.meteo, eta.model.type = eta.model.type, 
                list.gas.sensor = list.gas.sensor, list.sensors = list.sensors, list.reference = list.reference))
}
#' list of characteristics for one sensor of one ASE box determined using a caibration model name
#'
#' @param Model full filepath with file name of the calibration model.
#' @param name.sensor Character vector, default is NULL. if NULL the sensor name is extracted from Model.
#' @param General.DT A data.table with all ASE box data. Default is null. If NULL the General.csv file is loaded
#' @param ASE.cfg A data.table with all ASE box configuration. Default is null. If NULL the ASE.cfg file is loaded
#' @param SetTime A data.table with all ASE box SetTime configuration. Default is null. If NULL the ASE_SETTIME.cfg file is loaded
#' @param DIR_Config subdirectory of ASEDir where is the file ASE.name.cfg.
#' @param DIR_Models subdirectory of ASEDir where are the calibration models.
#' @param DIR_General subdirectory of ASEDir where is the file General.csv.
#' @param Filter.Sens logical default is TRUE. If FALSE  filtering of sensor data is not performed.
#' @param Filter.Ref logical default is TRUE. If FALSE  filtering of reference data is not performed.
#' @param Select.Columns logical default is FALSE. If TRUE the columns listed in Dropped.Columns are dropped when reading General.csv.
#' @param Dropped.Columns character vector default is FALSE. List of pattern of Dropped Columns are dropped when reading General.csv.
#' @param Simulation logical default is FALSE. If TRUE the configuration files of the ASE Boxes is not in DIr_Models but in "Models".
#' @return a list of characteristics of a sensor in ASE box with data.table General.DT, config data.table ASE.cfg, k the index of sensor in the kist of sensors,
# the dates of calibration Cal.DateIN  and Cal.DateEND = Cal.DateEND, full date interval of measurements Meas.DateIN and Meas.DateEND, column names of 
# sensor raw data (nameGasVolt), sensor predicted data (nameGasMod), corresponding reference data (nameGasRef), sensor raw unit (Sens.raw.unit)
# vector of list of sensor (list.sensors), vector of list of meteorological parameters (var.names.meteoDates).
#' @examples
#' Identify.ASE(Model = Model)
Identify_ASE <- function(Model, name.sensor = NULL, General.DT = NULL, ASE.cfg = NULL, SetTime = NULL, Config = NULL, Shield = NULL,
                         DIR_Config = "Configuration", DIR_Models = "Models", DIR_General = "General_data", Filter.Sens = TRUE, Filter.Ref = TRUE, 
                         Select.Columns = FALSE, Dropped.Columns = c(".Warm.", ".TRh", ".Invalid","Bin"), Simulation = F, ASE.name = NULL) {
    # name of ASE box
    Stripped.Model <- unlist(strsplit(Model, split = "__"))
    # Extract ASE.name
    if (is.null(ASE.name)) ASE.name <- basename(Stripped.Model[1])
    # Extract ASEDir
    if (Simulation ){
        ASEDir <- file.path(gsub(paste0(DIR_Models, "/"), "",Stripped.Model[1]))
    } else ASEDir <- substr(Stripped.Model[1], start = 1, stop = stringr::str_locate(pattern = ASE.name,Stripped.Model[1])[1,"end"])
    # Extract name.sensor
    if (is.null(name.sensor)) name.sensor <- Stripped.Model[2]
    # Extract Sens.raw.unit
    Sens.raw.unit  <- Stripped.Model[3]
    # Extract Mod_type
    Mod_type <- Stripped.Model[4]
    # Extract Cal.DateIN and Cal.DateEND
    Cal.DateIN     <- as.Date(Stripped.Model[5], format = " %Y%m%d", optional = T)
    Cal.DateEND    <- as.Date(Stripped.Model[6], format = " %Y%m%d", optional = T)
    # Extract Variables
    Variables   <- ifelse(Stripped.Model[7] != ".rdata", sub(pattern = ".rdata", replacement = "",Stripped.Model[7]), NA)
    # Extracting data 
    if (is.null(Config)) Config <- CONFIG(DisqueFieldtestDir = ASEDir, DisqueFieldtest = dirname(dirname(ASEDir)), Dir.Config = DIR_Config, Verbose = F, shiny = F)
    if (is.null(Shield)) Shield <- ASEPanel04Read(ASEPanel04File = file.path(dirname(dirname(ASEDir)), "Shield_Files", Config$Server$asc.File))
    if (is.null(ASE.cfg)) ASE.cfg <- data.table::fread(file.path(ASEDir, DIR_Config, paste0(ASE.name,".cfg")), showProgress =F)
    if (is.null(SetTime)){
        SetTime <- data.table::fread(file.path(ASEDir, DIR_Config, paste0(ASE.name,"_SETTIME.cfg")), showProgress = F)
        # Correcting old naming of DateCal.IN in Cal.IN and Out.Ref.IN in Ref.IN
        if(any(grepl("Out\\.", SetTime$name.gas)) || any(grepl("Date", SetTime$name.gas)) || any(grepl("Date\\.", SetTime$name.gas))){
            if (any(grepl("Date.", SetTime$name.gas))) data.table::set(SetTime, j = "name.gas", value = gsub("Date\\.","", SetTime$name.gas))
            if (any(grepl("Date", SetTime$name.gas)))  data.table::set(SetTime, j = "name.gas", value = gsub("Date","", SetTime$name.gas))
            if (any(grepl("Out.", SetTime$name.gas)))  data.table::set(SetTime, j = "name.gas", value = gsub("Out\\.","", SetTime$name.gas))
            data.table::fwrite(SetTime, file = file.path(ASEDir, DxxxIR_Config, paste0(ASE.name,"_SETTIME.cfg")))}}
    k               <- as.integer(which(unlist(ASE.cfg[ASE.cfg$name.gas == "name.sensor"]) == name.sensor))
    name.gas        <- names(which(unlist(ASE.cfg[ASE.cfg$name.gas == "name.sensor"]) == name.sensor))
    gas.sensor      <- unlist(ASE.cfg[ASE.cfg$name.gas == "gas.sensor", k, with = FALSE])
    gas.reference2use <- unlist(ASE.cfg[ASE.cfg$name.gas == "gas.reference2use", k, with = FALSE])
    Meas.DateIN     <- max(c(as.Date(SetTime[name.gas == "Meas.IN" ][[k]], optional = T), as.Date(SetTime[name.gas == "DatePlotMeas.IN" ][[k]], optional = T)), na.rm = TRUE) 
    Meas.DateEND    <- min(c(as.Date(SetTime[name.gas == "Meas.END"][[k]], optional = T), as.Date(SetTime[name.gas == "DatePlotMeas.END"][[k]], optional = T)), na.rm = TRUE)
    nameGasVolt     <- paste0(name.sensor,"_volt")      # sensor gas in volt or nA or Count
    nameGasMod      <- paste0(gas.sensor,"_modelled")   # modelled sensor gas
    nameGasRef      <- paste0("Out.",gas.reference2use) # Gas reference
    unit.ref        <- unlist(ASE.cfg[ASE.cfg$name.gas == "ref.unitgas"  , k, with = FALSE])
    unit.sensor     <- unlist(ASE.cfg[ASE.cfg$name.gas == "Sens.unit"    , k, with = FALSE])
    remove.neg      <- as.logical(unlist(ASE.cfg[ASE.cfg$name.gas == "remove.neg", k, with = FALSE]))
    ubsRM           <- unlist(ASE.cfg[ASE.cfg$name.gas == "ubsRM"     , k, with = FALSE])
    ubss            <- unlist(ASE.cfg[ASE.cfg$name.gas == "ubss"      , k, with = FALSE])
    Sync.Cal        <- as.logical(unlist(ASE.cfg[ASE.cfg$name.gas == "Sync.Cal"  , k, with = FALSE]))
    Sync.Pred       <- as.logical(unlist(ASE.cfg[ASE.cfg$name.gas == "Sync.Pred" , k, with = FALSE]))
    var.names.meteo <- data.table::fread(file.path(ASEDir, DIR_Config, "var.names.meteo.cfg"), showProgress = F)$columns
    if ("Temperature" %in% var.names.meteo)           var.names.meteo[var.names.meteo == "Temperature"]           <- "Out.Temperature"
    if ("Temperature_int" %in% var.names.meteo)       var.names.meteo[var.names.meteo == "Temperature_int"]       <- "Out.Temperature_int"
    if ("Relative_humidity" %in% var.names.meteo)     var.names.meteo[var.names.meteo == "Relative_humidity"]     <- "Out.Relative_humidity"
    if ("Relative_humidity_int" %in% var.names.meteo) var.names.meteo[var.names.meteo == "Relative_humidity_int"] <- "Out.Relative_humidity_int"
    if ("Atmospheric_pressure" %in% var.names.meteo)  var.names.meteo[var.names.meteo == "Atmospheric_pressure"]  <- "Out.Atmospheric_pressure"
    eta.model.type  <- unlist(ASE.cfg[ASE.cfg$name.gas == "eta.model.type", k, with = FALSE])
    # General.DT
    if (is.null(General.DT)) {
        General.file <- file.path(ASEDir, DIR_General, "General.csv")
        if (file.exists(General.file)) {
            if (tools::file_ext(General.file) == "csv") {
                if (Select.Columns) {
                    General.DT <- data.table::fread(General.file, nrows = 0)
                    Drop <- grep(paste(Dropped.Columns, collapse = "|"), General.DT)
                    General.DT <- data.table::fread(General.file, drop = Drop) 
                } else General.DT <- data.table::fread(General.file) #, na.strings = getOption("","NA")
                # Convert date to POSIXct
                if (shiny::isTruthy(Config$Server$Influx.TZ) || shiny::isTruthy(Config$Server$SOS.TZ)) {
                    if (shiny::isTruthy(Config$Server$Influx.TZ)) {
                        data.table::set(General.DT, j = "date"         , value =  Set_date2POSIXct(General.DT[["date"]]         , tz = Config$Server$Influx.TZ))
                        if("date_PreDelay" %in% names(General.DT)) data.table::set(General.DT, j = "date_PreDelay", value =  Set_date2POSIXct(General.DT[["date_PreDelay"]], tz = Config$Server$Influx.TZ))
                    } else {
                        data.table::set(General.DT, j = "date"         , value =  Set_date2POSIXct(General.DT[["date"]]         , tz = Config$Server$SOS.TZ))
                        if("date_PreDelay" %in% names(General.DT)) data.table::set(General.DT, j = "date_PreDelay", value =  Set_date2POSIXct(General.DT[["date_PreDelay"]], tz = Config$Server$SOS.TZ))}
                } else {
                    data.table::set(General.DT, j = "date"         , value =  Set_date2POSIXct(General.DT[["date"]]         , tz = "UTC"))
                    if("date_PreDelay" %in% names(General.DT)) data.table::set(General.DT, j = "date_PreDelay", value =  Set_date2POSIXct(General.DT[["date_PreDelay"]], tz = "UTC"))}
            } else if (tools::file_ext(General.file) == "Rdata") {
                General.DT <- load_obj(General.file)
                if (!data.table::is.data.table(General.DT)) General.DT <- data.table::data.table(General.DT, key = "date")}
            if ("V1" %in% names(General.DT)) General.DT[, V1 := NULL]
        } else {
            InfluxData.file <- file.path(ASEDir, DIR_General, "InfluxData.csv")
            SOSData.file    <- file.path(ASEDir, DIR_General, "SOSData.csv")
            RefData.file    <- file.path(ASEDir, DIR_General, "RefData.csv")
            if (file.exists(InfluxData.file)) {
                if (tools::file_ext(InfluxData.file) == "csv") {
                    InfluxData <- data.table::fread(file = InfluxData.file, na.strings = c("","NA", "<NA>"))
                    if (shiny::isTruthy(Config$Server$Influx.TZ)) {
                        data.table::set(InfluxData, j = "date", value =  Set_date2POSIXct(InfluxData[["date"]], tz = Config$Server$Influx.TZ))
                    } else data.table::set(InfluxData, j = "date", value =  Set_date2POSIXct(InfluxData[["date"]], tz = "UTC"))
                } else if (tools::file_ext(InfluxData.file) == "Rdata") {
                    InfluxData <- load_obj(InfluxData.file)
                    if (!data.table::is.data.table(InfluxData)) InfluxData <- data.table::data.table(InfluxData, key = "date")}
                if ("V1" %in% names(InfluxData)) InfluxData[, V1 := NULL]
            } else return(futile.logger::flog.error(paste0("[Identify_ASE_Dir] no sensor data for ASE ", ASE.name)))
            if (file.exists(SOSData.file)) {
                if (tools::file_ext(SOSData.file) == "csv") {
                    SOSData <- data.table::fread(file = SOSData.file, na.strings = c("","NA", "<NA>"))
                    if (shiny::isTruthy(Config$Server$SOS.TZ)) {
                        data.table::set(SOSData, j = "date", value =  Set_date2POSIXct(SOSData[["date"]], tz = Config$Server$SOS.TZ))
                    } else data.table::set(SOSData, j = "date", value =  Set_date2POSIXct(SOSData[["date"]], tz = "UTC"))
                } else if (tools::file_ext(SOSData.file) == "Rdata") {
                    SOSData <- load_obj(SOSData.file)
                    if (!data.table::is.data.table(SOSData)) SOSData <- data.table::data.table(SOSData, key = "date")}
                if ("V1" %in% names(SOSData)) SOSData[, V1 := NULL]
            } else SOSData <- NULL
            if (file.exists(RefData.file)) {
                if (tools::file_ext(RefData.file) == "csv") {
                    RefData <- data.table::fread(file = RefData.file, na.strings = c("","NA", "<NA>"))
                    if (!"" %in% Config$Server$ref.tzone) {
                        data.table::set(RefData, j = "date", value =  Set_date2POSIXct(RefData[["date"]], tz = Config$Server$ref.tzone))
                    } else data.table::set(RefData, j = "date", value =  Set_date2POSIXct(RefData[["date"]], tz = "UTC"))
                } else if (tools::file_ext(RefData.file) == "Rdata") {
                    RefData <- load_obj(RefData.file)
                    if (!data.table::is.data.table(RefData)) RefData <- data.table::data.table(RefData, key = "date")}
                if ("V1" %in% names(RefData)) RefData[, V1 := NULL]
                # Message in case coordinates are not included
                if (!all(c("Ref.Long",  "Ref.Lat") %in% names(RefData))) futile.logger:: flog.warn("[Identify_ASE_Dir] Coordinates of reference station missing")
            } else RefData <- NULL
            
            General.DT <- GENERAL(UserMins = Config$Server$UserMins, Delay =  Config$Server$Delay,
                                  RefData = RefData, InfluxData = InfluxData, SOSData = SOSData,
                                  DownloadSensor = Check_Download(Influx.name = ASE.name,
                                                                  WDinput = file.path(ASEDir, DIR_General), 
                                                                  UserMins = Config$Server$UserMins, 
                                                                  RefData = RefData, 
                                                                  InfluxData = InfluxData, 
                                                                  SOSData = SOSData, Verbose = FALSE))}}
    # lists for all sensors
    list.gas.sensor  <- intersect(unlist(ASE.cfg[ASE.cfg$name.gas == "gas.sensor"])[-1], names(General.DT))
    list.sensors     <- unlist(ASE.cfg[ASE.cfg$name.gas == "name.sensor"])[unlist(ASE.cfg[ASE.cfg$name.gas == "gas.sensor"]) %in% list.gas.sensor]
    list.reference   <- intersect(unlist(ASE.cfg[ASE.cfg$name.gas == "gas.reference2use"])[-1], names(General.DT))
    # Complete General DT with columns for filtered sesnors if needed
    General.DT <- Complete_General(f.list.name.sensor = list.sensors, f.list.gas.sensor = list.gas.sensor, f.General.DT = General.DT, f.Config = Config, 
                                   f.ASEDir = ASEDir, f.Shield = Shield, f.list.reference = list.reference, f.ASE.cfg = ASE.cfg, Filter.Sens = Filter.Sens, Filter.Ref = Filter.Ref)
    # returning
    return(list(General.DT = General.DT, ASE.cfg = ASE.cfg, SetTime = SetTime, k = k, Config = Config,
                Cal.DateIN = Cal.DateIN, Cal.DateEND = Cal.DateEND,
                ASE.name = ASE.name, ASEDir = ASEDir, name.sensor = name.sensor, name.gas = name.gas, gas.sensor = gas.sensor,
                Mod_type = Mod_type, Variables = Variables,
                Meas.DateIN = Meas.DateIN, Meas.DateEND = Meas.DateEND, 
                nameGasVolt = nameGasVolt, nameGasMod = nameGasMod, nameGasRef = nameGasRef,
                ASESens.raw.unit = Sens.raw.unit, unit.ref = unit.ref, unit.sensor = unit.sensor,
                remove.neg = remove.neg, ubsRM = ubsRM, ubss = ubss, Sync.Cal = Sync.Cal, Sync.Pred = Sync.Pred, 
                var.names.meteo = var.names.meteo, eta.model.type = eta.model.type, 
                list.gas.sensor = list.gas.sensor, list.sensors = list.sensors, list.reference = list.reference))
}
# Flagging the sensor data for warming time ====
#' Flagging the sensor data for warming time
#' This data treatment can only works if file Borad.csv exists or boardTimeStamp exists, meaning only in InfluxData. It will not work with SOSData
#' 
#' @param Warm.Forced a logical, default is TRUE. Indexing of data is carried out only if TRUE
#' @param General.DT a data.table with all ASE Box and reference data. Default is NULL. if NULL, Generalcsv loaded with Identify_ASE is loaded
#' @param list.gas.sensor a vector of string representing the names of molecules (Carbon_monoxide...) on which to apply the indexing of dates within warming times.
#' @param boxConfig list, output of function CONFIG() with information on AirSensEUR configuration
#' @param ind.warm.file path file for saving the return values of the function
#' @param ASEDir A character vector with the file path of the ASE Box for which to look for models. It should be something like file.path("shiny", Project, ASE.name).
#' @param DIR_General File path of the subdirectory of ASEDir where is the file General.csv.
#' @param Save a logical, default is TRUE. Saving of Indexing of ind.warm.file is carried out only if TRUE
#' @return a list of 4(?) vectors, one for each sensor, including row indexes of General.DT that are within the warming time of sensors,
#       the names of the 4 elements are the ones of list.gas.sensor   in the same order
Warm_Index <- function(Warm.Forced  = TRUE, General.DT, list.gas.sensor, boxConfig, ind.warm.file = NULL, ASEDir, DIR_General = "General_data", Save = TRUE) {
    if (is.null(ind.warm.file)) ind.warm.file <- file.path(ASEDir, DIR_General, "ind_warm.RDS")
    # setting index for warming, reset ind.warm.out
    if (Warm.Forced) {
        ind.warm.out <- NULL
        if ("board.cfg" %in% names(boxConfig)) {
            # Re_sample times in board.cfg converted to next minute
            Re_sample <- lubridate::ceiling_date(unique(boxConfig$board.cfg$time), unit = "minute")
            # look for exact date and lime taking care of lag time within 5 minutes
            for (i in seq_along(Re_sample)){
                # Checking if boardTimesStamp is present and increasing order, it means AirSensEUR did not switched off
                Restart.Date <- General.DT[date %between% c(Re_sample[i], Re_sample[i] +15 *60), date][1]
                if(is.POSIXct(Restart.Date) && shiny::isTruthy(Restart.Date)){
                    i.Restart.Date <- General.DT[date == Restart.Date, which = TRUE]
                    min.i.Restart.Date <- max(1,(i.Restart.Date-6))
                    max.i.Restart.Date <- min(nrow(General.DT),(i.Restart.Date+6))
                    tmp <- General.DT[min.i.Restart.Date:max.i.Restart.Date, .SD, .SDcols = c("date", "boardTimeStamp")]
                    if(nrow(tmp) > 0 && (all(is.na(tmp$boardTimeStamp)) || is.unsorted(tmp$boardTimeStamp))){
                        Re_sample[i] <- General.DT[date %between% c(Re_sample[i], Re_sample[i] +5 *60), date][1]
                    } else Re_sample[i] <- NA
                    rm(tmp, i.Restart.Date, min.i.Restart.Date, max.i.Restart.Date)
                }
            }
            # returning indexes of Re_sample ddates
            ind <- which(General.DT$date %in% Re_sample)
            # add first date in case it is not included
            if (!1 %in% ind) ind <- c(1, ind)
        } else {
            if (!is.null(General.DT[,"boardTimeStamp"]) ) { 
                # http://r.789695.n4.nabble.com/Replace-zeroes-in-vector-with-nearest-non-zero-value-td893922.html
                # https://stackoverflow.com/questions/26414579/r-replacing-zeros-in-dataframe-with-last-non-zero-value
                # replace every boardTimeStamp which does not change with NA so na.locf will works
                # Index of boardtimeStamp similar for consecutive boardtimeStamp
                Consecutive <- which(diff(General.DT$boardTimeStamp, lag = 1) == 0)
                # Values of indexes whith previous values that are non consecutive (Re-start)
                Re_start    <- Consecutive[diff(Consecutive, lag = 1) > 1]
                # Setting NA boardTimeStamp to the last non-NA boardTimeStamp
                data.table::set(General.DT,  j = "boardTimeStamp", value = na.locf(General.DT[["boardTimeStamp"]], na.rm = FALSE, fromLast = FALSE))
                # detecting when boardTimeStamp decreases suddenly (re-boot)
                Re_sample     <- which(diff(General.DT$boardTimeStamp, lag = 1) < 0)
                # Combining Re_start and Re_sample
                if (length(Re_start) > 0 | length(Re_sample) > 0) ind <- unique(c(Re_start, Re_sample)) else ind = numeric(0)
            } else {
                # This is for SOS
                ind <- apply(General.DT[, list.gas.sensor, with = FALSE  ], 1, function(i) !all(is.na(i)))
                ind <- which(ind[2:length(ind)] & !ind[1:(length(ind) - 1 )])}
            # Set the first discarded value as the value restarting from 0
            ind = ind + 1
            # Adding the first switch-on
            ind <- unique(c(1,ind))} 
        # Creating a list of  vector of index of value to discard in a list including all names of sensors
        ind.warm.out <- lapply(list.gas.sensor, function(n) {
            unique(unlist(lapply(ind, function(i) {
                which(General.DT$date %within% lubridate::interval(General.DT$date[i],General.DT$date[i]+boxConfig$sens2ref[gas.sensor == n][["hoursWarming"]]*3600))})))})
        names(ind.warm.out) <- list.gas.sensor
        # Saving if requested
        if (Save) {
            # Path file for later saving
            if (is.null(ind.warm.file)) ind.warm.file <- file.path(ASEDir, DIR_General, "ind_warm.RDS")
            list.save(x = ind.warm.out, file = ind.warm.file)
            futile.logger::flog.info(paste0("[Warm_Index] ", basename(ASEDir), " A new ind_warm.RDS was saved."))}
    } else if (file.exists(ind.warm.file)) ind.warm.out <- list.load(ind.warm.file) else ind.warm.out <- NULL
    return(ind.warm.out)   
}
# Flagging the sensor data for temperature and humidity outside interval of tolerance ====
#' Flagging the sensor data for temperature and humidity outside interval of tolerance
#' @param TRh.Forced a logical, default is TRUE. Indexing of data is carried out only if TRUE
#' @param General.DT a data.table with all ASE Box and reference data. Default is NULL. if NULL, Generalcsv loaded with Identify_ASE is loaded
#' @param list.gas.sensor a vector of string representing the names of molecules (Carbon_monoxide...) on which to apply the indexing of dates within warming times.
#' @param boxConfig list, output of function CONFIG() with information on AirSensEUR configuration
#' @param ind.TRh.file path file for saving the return values of the function
#' @param ASEDir A character vector with the file path of the ASE Box for which to look for models. It should be something like file.path("shiny", Project, ASE.name).
#' @param DIR_General File path of the subdirectory of ASEDir where is the file General.csv.
#' @param Save a logical, default is TRUE. Saving of Indexing of ind.warm.file is carried out only if TRUE
#' @return : list with 5 elements: ind.TRh, T.min, T.max, Rh.min, Rh.max of NAs for discarded all conditions, temperature min and max, humidity min and max with as many elements as in list.gas.sensor
#                          consisting of vector of integers of the index of rows of General.DT dataframe
TRh_Index <- function(TRh.Forced  = TRUE, General.DT, list.gas.sensor, list.name.sensor = NULL, boxConfig, ind.TRh.file = NULL, ASEDir, DIR_General = "General_data", Save = TRUE) {
    if (is.null(ind.TRh.file)) ind.TRh.file <- file.path(ASEDir, DIR_General, "ind_TRh.RDS"  )
    if (TRh.Forced) {
        # Always starting detection of outliers for T and RH from the dataframe set in General.DT
        index.temp <- which(colnames(General.DT) %in% "Temperature")
        index.rh   <- which(colnames(General.DT) %in% "Relative_humidity")
        return.ind.TRh    <- list()
        return.ind.T.min  <- list()
        return.ind.T.max  <- list()
        return.ind.Rh.min <- list()
        return.ind.Rh.max <- list()
        if (!any(duplicated(list.gas.sensor))){
            for (l in list.gas.sensor) {
                # Indexes of temperature and humidity exceeding min/max thresholds
                T.min  <- General.DT[, index.temp, with = FALSE] < boxConfig$sens2ref$temp.thres.min[match(x = l, table = list.gas.sensor)] #| is.na(General.DT[, index.temp, with = FALSE])
                T.max  <- General.DT[, index.temp, with = FALSE] > boxConfig$sens2ref$temp.thres.max[match(x = l, table = list.gas.sensor)] #| is.na(General.DT[, index.temp, with = FALSE])
                Rh.min <- General.DT[, index.rh, with = FALSE]   < boxConfig$sens2ref$rh.thres.min[match(x = l, table = list.gas.sensor)]   #| is.na(General.DT[, index.rh,   with = FALSE])
                Rh.max <- General.DT[, index.rh, with = FALSE]   > boxConfig$sens2ref$rh.thres.max[match(x = l, table = list.gas.sensor)]   #| is.na(General.DT[, index.rh,   with = FALSE])
                # Global index of temperature/humidity exceeding thresholds
                return.ind.TRh[[boxConfig[["sens2ref"]][which(gas.sensor == l), name.sensor]]] <- which(T.min | T.max | Rh.min | Rh.max)
                return.ind.T.min[[ paste0(boxConfig[["sens2ref"]][which(gas.sensor == l), name.sensor],"__Temp. < ",boxConfig$sens2ref$temp.thres.min[match(x = l, table = list.gas.sensor)])]] <- which(T.min)
                return.ind.T.max[[ paste0(boxConfig[["sens2ref"]][which(gas.sensor == l), name.sensor],"__Temp. > ",boxConfig$sens2ref$temp.thres.max[match(x = l, table = list.gas.sensor)])]] <- which(T.max)
                return.ind.Rh.min[[paste0(boxConfig[["sens2ref"]][which(gas.sensor == l), name.sensor],"__RH < "   ,boxConfig$sens2ref$rh.thres.min[match(x = l, table = list.gas.sensor)])]] <- which(Rh.min)
                return.ind.Rh.max[[paste0(boxConfig[["sens2ref"]][which(gas.sensor == l), name.sensor],"__RH > "   ,boxConfig$sens2ref$rh.thres.max[match(x = l, table = list.gas.sensor)])]] <- which(Rh.max)}
        } else {
            for (l in seq_along(list.gas.sensor)) {
                # Indexes of temperature and humidity exceeding min/max thresholds
                T.min  <- General.DT[, index.temp, with = FALSE] < boxConfig$sens2ref$temp.thres.min[l] #| is.na(General.DT[, index.temp, with = FALSE])
                T.max  <- General.DT[, index.temp, with = FALSE] > boxConfig$sens2ref$temp.thres.max[l] #| is.na(General.DT[, index.temp, with = FALSE])
                Rh.min <- General.DT[, index.rh, with = FALSE]   < boxConfig$sens2ref$rh.thres.min[l]   #| is.na(General.DT[, index.rh,   with = FALSE])
                Rh.max <- General.DT[, index.rh, with = FALSE]   > boxConfig$sens2ref$rh.thres.max[l]   #| is.na(General.DT[, index.rh,   with = FALSE])
                # Global index of temperature/humidity exceeding thresholds
                return.ind.TRh[[boxConfig[["sens2ref"]][l, name.sensor]]] <- which(T.min | T.max | Rh.min | Rh.max)
                return.ind.T.min[[ paste0(boxConfig[["sens2ref"]][l, name.sensor],"__Temp. < ",boxConfig$sens2ref$temp.thres.min[l])]] <- which(T.min)
                return.ind.T.max[[ paste0(boxConfig[["sens2ref"]][l, name.sensor],"__Temp. > ",boxConfig$sens2ref$temp.thres.max[l])]] <- which(T.max)
                return.ind.Rh.min[[paste0(boxConfig[["sens2ref"]][l, name.sensor],"__RH < "   ,boxConfig$sens2ref$rh.thres.min[l])]] <- which(Rh.min)
                return.ind.Rh.max[[paste0(boxConfig[["sens2ref"]][l, name.sensor],"__RH > "   ,boxConfig$sens2ref$rh.thres.max[l])]] <- which(Rh.max)}}
        ind.TRh.out <- list(ind.TRh = return.ind.TRh, T.min = return.ind.T.min, T.max = return.ind.T.max, Rh.min = return.ind.Rh.min, Rh.max = return.ind.Rh.max)
        if (Save) {
            list.save(x = ind.TRh.out, file = ind.TRh.file)
            futile.logger::flog.info(paste0("[TRh_Index] ", basename(ASEDir), " A new ind_TRh.RDS was saved. Inv.Forced is set to TRUE"))}
    } else if (file.exists(ind.TRh.file)) ind.TRh.out <- list.load(ind.TRh.file) else ind.TRh.out <- NULL
    return(ind.TRh.out)
}
# Flagging the sensor data for invalid data using file ASE_Valid_sensor.cfg ====
#' Flagging the sensor data for temperature and humidity outside interval of tolerance
#' @param Inv.Forced a logical, default is TRUE. Indexing of data is carried out only if TRUE
#' @param General.DT a data.table with all ASE Box and reference data. Default is NULL. if NULL, Generalcsv loaded with Identify_ASE is loaded
#' @param list.gas.sensor a vector of string representing the names of molecules (Carbon_monoxide...) on which to apply the indexing of dates within warming times.
#' @param boxConfig list, output of function CONFIG() with information on AirSensEUR configuration
#' @param ind.Invalid.file path file for saving the return values of the function
#' @param ASEDir A character vector with the file path of the ASE Box for which to look for models. It should be something like file.path("shiny", Project, ASE.name).
#' @param DIR_Config File path of the subdirectory of ASEDir where is the file ASE.name.cfg.
#' @param DIR_General File path of the subdirectory of ASEDir where is the file General.csv.
#' @param Save a logical, default is TRUE. Saving of Indexing of ind.warm.file is carried out only if TRUE
#' @return : list with 1 elements: dates of invalid dates
Inv_Index <- function(Inv.Forced  = TRUE, General.DT, list.name.sensor, boxConfig, ind.Invalid.file = NULL, ASEDir, DIR_Config  = "Configuration", 
                      DIR_General = "General_data", Save = TRUE) {
    ind.Invalid.file <- file.path(ASEDir, DIR_General, "ind_Invalid.RDS")
    if (Inv.Forced) {
        # min.General.date and max.General.date----
        if (!is.null(General.DT)) min.General.date <- min(General.DT$date, na.rm = TRUE) else min.General.date <- NULL
        if (!is.null(General.DT)) max.General.date <- max(General.DT$date, na.rm = TRUE) else max.General.date <- NULL
        if (!is.null(General.DT)) {
            # reading the files with period of valid data
            for (i in seq_along(list.name.sensor)) {
                nameFile <- file.path(ASEDir, DIR_Config, paste0(basename(ASEDir),"_Valid_",list.name.sensor[i],".cfg"))
                if (file.exists(nameFile)) {
                    assign(paste0("Valid_",list.name.sensor[i]), data.table::fread(file = nameFile))
                } else {
                    # There are no Valid files. Creates files with IN = END = min(General$date) + Comments
                    Invalid.DT <- data.table::data.table(In       = gsub(" UTC", "",strftime(min.General.date)),
                                                         End      = gsub(" UTC", "",strftime(min.General.date)),
                                                         Comments = paste0("INFO: 1st start of ", basename(ASEDir),", sensor ", list.name.sensor[i], " with S/N: ", boxConfig$Sensors.cfg))
                    assign(paste0("Valid_",list.name.sensor[i]), Invalid.DT)
                    data.table::fwrite(x = data.table::data.table(In       = gsub(" UTC", "",strftime(min.General.date)),
                                                                  End      = gsub(" UTC", "",strftime(min.General.date)),
                                                                  Comments = "beginning"),
                                       file      = nameFile)}
            }
            # Creating one list with invalid periods for all sensors
            Valid <- list()
            for (i in paste0("Valid_",list.name.sensor)) Valid[[i]] <- get(i)
            # Function to convert character strings to POSIX in UTC tz
            NewValid <- function(x) {
                # making x a dataframe of POSIXct if needed
                if(data.table::is.data.table(x)){
                    data.table::setnames(x,names(x)[1:2], c("In", "End"))
                    if(!lubridate::is.POSIXct(x$In)) x$In  <- parse_date_time(x$In , tz = threadr::time_zone(General.DT$date[1]), orders = "YmdHMS")
                    if(!lubridate::is.POSIXct(x$In)) x$End <- parse_date_time(x$End, tz = threadr::time_zone(General.DT$date[1]), orders = "YmdHMS")
                    x <- data.frame( x, stringsAsFactors = FALSE)
                } else if(is.data.frame(x)){
                    colnames(x)[1:2] <- c("In", "End")
                    if(!lubridate::is.POSIXct(x$In)) x$In  <- parse_date_time(x$In , tz = threadr::time_zone(General.DT$date[1]), orders = "YmdHMS")
                    if(!lubridate::is.POSIXct(x$In)) x$End <- parse_date_time(x$End, tz = threadr::time_zone(General.DT$date[1]), orders = "YmdHMS")
                } else stop("[NewValid] unknown format for invalid data.frame/data.table")
                return(x)
            }
            Valid.date <- lapply(Valid, NewValid)
            # Set inital date for data retrieving (i.e. maximum length time period for data retrieval).
            # These dates may change according to the data availability
            # UserDateIN.0, SOS.TZ is set in ASEConfig_MG.R
            # Set correct time zone
            # list of invalid date to be used for sensor Evaluation with reference values, a kind of life cycle of each sensor - time zone shall be the same as SOS.TZ (UTC?)
            # seting invalid to NA and create a list for plotting invalids
            ind.Inval <- list()
            for (i in gsub(pattern = "Valid_", replacement = "", names(Valid.date))) {
                for (j in 1:nrow(Valid.date[[paste0("Valid_",i)]])) {
                    Valid.interval.j <- which(General.DT$date %within% lubridate::interval(Valid.date[[paste0("Valid_",i)]]$In[j], Valid.date[[paste0("Valid_",i)]]$End[j]))
                    if (length(Valid.interval.j) > 0) {
                        if (!(i %in% names(ind.Inval))) {
                            ind.Inval[[i]] <- General.DT$date[Valid.interval.j]
                        } else {
                            ind.Inval[[i]] <- c(ind.Inval[[i]], General.DT$date[Valid.interval.j])
                        }
                    }
                }
            }
        }
        ind.Invalid.out <- list(Valid.date,ind.Inval)
        if (Save) list.save(x = ind.Invalid.out, file = ind.Invalid.file)
        futile.logger::flog.info(paste0("[Inv_Index] ", basename(ASEDir)," A new ind_Invalid.RDS was saved. Outliers.Sens is set to TRUE"))
    } else if (file.exists(ind.Invalid.file)) ind.Invalid.out <- list.load(ind.Invalid.file) else ind.Invalid.out <- NULL
    return(ind.Invalid.out)
}
# Discard outliers of sensors ====
#' Discard outliers of sensors
#' @param Outliers.Sens a logical, default is TRUE. Indexing of data is carried out only if TRUE
#' @param General.DT a data.table with all ASE Box and reference data. Default is NULL. if NULL, Generalcsv loaded with Identify_ASE is loaded
#' @param list.gas.sensor a vector of string representing the names of molecules (Carbon_monoxide...) on which to apply the indexing of dates within warming times.
#' @param list.name.sensor a vector of string representing the names of sensors on which to apply the indexing of dates within warming times.
#' @param boxConfig list, output of function CONFIG() with information on AirSensEUR configuration
#' @param ind.Invalid.file path file for saving the return values of the function
#' @param ASEDir A character vector with the file path of the ASE Box for which to look for models. It should be something like file.path("shiny", Project, ASE.name).
#' @param DIR_General character vector, default is "General_data". It sets a sub-directory that shall exist at ASEDir where is the file General.csv, .. are stored
#' @param DIR_Config Character vector giving the subdirectory of ASEDir where is the file ASE.name.cfg,.... are stored.
#' @param ind.warm.out output of functions Warm_Index()
#' @param ind.TRh.out output of functions TRh_Index()
#' @param ind.Invalid.out output of functions Inv_Index() 
#' @return : list with 2 elements: ind.sens.out with 2 ists for each sensor (indexes and filtered data) and General.DT, the filetered General data tables
Outliers_Sens <- function(Outliers.Sens  = TRUE, General.DT, list.gas.sensor, list.name.sensor, boxConfig, ASEDir, DIR_Config  = "Configuration", DIR_General = "General_data",
                          ind.warm.out, ind.TRh.out, ind.Invalid.out) {
    ind.sens.out.file       <- file.path(ASEDir, DIR_General, "ind_sens_out.RDS")
    ASE.name <- basename(ASEDir)
    if (Outliers.Sens) {
        if (!is.null(General.DT)) setalloccol(General.DT)
        for (i in list.gas.sensor) {
            futile.logger::flog.info(paste0("[Outliers_Sens] filtering outliers of sensor ",i))
            # Checking if sensor data exists in General.DT
            if (i %in% names(General.DT)) {
                # Initialisation of columns of General.DT
                Sensor.i <- na.omit(boxConfig[["sens2ref"]][[which(boxConfig[["sens2ref"]][,"gas.sensor"] == i),"name.sensor"]])
                # setting to initial values
                #Vector.columns <- paste0(c("Out.", "Out.Warm.", "Out.TRh.", "Out.Invalid.", "Out.Warm.TRh.", "Out.Warm.TRh.Inv."),i)
                Vector.columns <- paste0(c("Out."),i)
                data.table::set(General.DT, j = Vector.columns, value = rep(list(General.DT[[i]]), times = length(Vector.columns)))
                # Filter warming time
                if (!is.null(ind.warm.out[i][[1]])) {
                    #Vector.columns <- paste0(c("Out.", "Out.Warm.", "Out.Warm.TRh.", "Out.Warm.TRh.Inv."),i)
                    i.Rows <- as.integer(ind.warm.out[[i]])
                    data.table::set(General.DT, i = i.Rows, j = Vector.columns, value = rep(list(rep(NA, times = length(i.Rows))), times = length(Vector.columns)))}
                # Filter T/RH
                if (!is.null(ind.TRh.out$ind.TR[[Sensor.i]]) && length(ind.TRh.out$ind.TR[[Sensor.i]]) > 0) {
                    #Vector.columns <- paste0(c("Out.", "Out.TRh.", "Out.Warm.TRh.", "Out.Warm.TRh.Inv."),i)
                    data.table::set(General.DT,i = ind.TRh.out$ind.TRh[Sensor.i][[1]], j = Vector.columns,
                                    value = rep(list(rep(NA, times = length(ind.TRh.out$ind.TRh[Sensor.i][[1]]))), times = length(Vector.columns)))}
                # Filter Invalid
                if (!is.null(ind.Invalid.out[[2]][[Sensor.i]]) && length(ind.Invalid.out[[2]][[Sensor.i]]) > 0) {
                    #Vector.columns <- paste0(c("Out.", "Out.Invalid." , "Out.Warm.TRh.Inv."),i)
                    i.Rows <- which(General.DT$date %in% ind.Invalid.out[[2]][[Sensor.i]])
                    data.table::set(General.DT,i = i.Rows, j = Vector.columns, value = rep(list(rep(NA, times = length(i.Rows))), times = length(Vector.columns)))}
                # index (1, 2,3, 4  or 1,2,3, 6 ... comng from  selection of control uiFiltering, Calib and SetTime)
                k <- match(x = i, table = list.gas.sensor)
                iters <- boxConfig$sens2ref$Sens.iterations[k]
                
                # deleting bigger iterations
                repeat {
                    if (paste0("Out.",i,".", iters + 1) %in% names(General.DT)) {
                        data.table::set(General.DT, j = paste0("Out.",i,".", iters + 1), value = NULL)
                        iters <- iters + 1
                    } else break} # leaving the repeat loop if there are no higher iterations
                if (boxConfig$sens2ref$Sens.Inval.Out[k]) {
                    for (j in 1:iters) { # number of iterations
                        if (all(is.na(General.DT[[i]]))) {
                            futile.logger::flog.error(paste0("[Outliers_Sens] ", ASE.name, " no sensor data for filtering outliers"))
                        } else {
                            # Setting the columns of sensor data previous to detect outliers
                            Y <- General.DT[[paste0("Out.",i)]] 
                            # setting Y for the outliers of previous iterations to NA. If null then stop outlier detection
                            if (j > 1) {
                                if (length(which(return.ind.sens.out[[paste0(i,".",(j - 1))]]$Outliers)) != 0) {
                                    if (class(Y)[1] == "tbl_df") {
                                        Y[as.numeric(paste(unlist(sapply(return.ind.sens.out[c(paste0(i,".",1:(j - 1)))],function(x) which(x$Outliers))))),] <- NA
                                    } else Y[as.numeric(paste(unlist(sapply(return.ind.sens.out[c(paste0(i,".",1:(j - 1)))],function(x) which(x$Outliers)))))] <- NA
                                } else break}
                            Outli <- My.rm.Outliers(ymin         = boxConfig$sens2ref$Sens.Ymin[k],
                                                    ymax         = boxConfig$sens2ref$Sens.Ymax[k],
                                                    ThresholdMin = boxConfig$sens2ref$Sens.ThresholdMin[k],
                                                    date         = General.DT[["date"]],
                                                    y            = Y,
                                                    window       = boxConfig$sens2ref$Sens.window[k],
                                                    threshold    = boxConfig$sens2ref$Sens.threshold[k],
                                                    plotting     = FALSE)
                            nameInd      <- paste0(i,".",j)
                            OutlinameInd <- paste0(i,".",j,".Outli")
                            assign(nameInd , data.frame(date = Outli$date, Outliers = apply(Outli[,c("Low_values","High_values","OutliersMin","OutliersMax")], 1, any), stringsAsFactors = FALSE))
                            if (exists("return.ind.sens.out")) return.ind.sens.out[[nameInd]] <- get(nameInd) else {
                                return.ind.sens.out <- list(get(nameInd))
                                names(return.ind.sens.out) <- nameInd}
                            return.ind.sens.out[[OutlinameInd]] <- Outli
                            # Discarding outliers if requested for the compound
                            Row.Index <- which(return.ind.sens.out[[paste0(i,".",j)]]$Outliers)
                            if (paste0(i,".",j) %in% names(return.ind.sens.out) && length(Row.Index) > 0) {
                                data.table::set(General.DT,i = Row.Index, j = paste0("Out."      ,i), value = list(rep(NA, times = length(Row.Index))))
                            }}}}}}
        if (exists("return.ind.sens.out")) {
            list.save(x = return.ind.sens.out, file = ind.sens.out.file)
            futile.logger::flog.info(paste0("[Outliers_Sens] ",ASE.name, " A new ind_sens_out.RDS was saved. Conv.Forced is set to TRUE"))} else return.ind.sens.out <- NULL
    } else {
        if (file.exists(ind.sens.out.file)) return.ind.sens.out <- list.load(ind.sens.out.file) else return.ind.sens.out <- NULL}
    
    # deleting unnecessary outlier replicates
    #for (i in 1:length(list.gas.sensor)) for (j in 1:boxConfig$sens2ref$Sens.iterations[i]) assign(paste0(list.gas.sensor[i],".",j),NULL)
    
    return(list(ind.sens.out = return.ind.sens.out, General.DT = General.DT))
}
Sens_Conv <- function(Conv.Forced  = TRUE, General.DT, list.gas.sensor, list.name.sensor, boxConfig, ASEDir, 
                      DIR_Config  = "Configuration", DIR_General = "General_data", Shield) {
    if (Conv.Forced) {
        # digits2volt conversion for whole data in nA or V
        futile.logger::flog.info(paste0("[Sens_Conv] ",basename(ASEDir), " digital to volt conversion for all sensors on the shields"))
        # Checking Filtering of sensor and reference data
        Sensors_Cal <- merge(x = boxConfig$sens2ref[,c("name.gas","gas.sensor","name.sensor","Sens.raw.unit")], 
                             y = Shield[,c("name.gas","name.sensor","gas.sensor","RefAD","Ref","board.zero.set","GAIN","Rload")],
                             by = c("name.gas", "gas.sensor", "name.sensor"),
                             all = TRUE)
        # order Sensors_Cal as Calib_data()Goo
        # Values converted in volt or nA of sensors in Shield only if sensor data exist
        if (all(paste0("Out.",Shield$gas.sensor) %in% names(General.DT))){
            data.table::set(General.DT,  j = paste0(Shield$name.sensor,"_volt"),
                            value = ASEDigi2Volt(Sensors_Cal = Sensors_Cal[Sensors_Cal$name.gas %in% Shield$name.gas,],
                                                 Digital = General.DT[,paste0("Out.",Shield$gas.sensor), with = FALSE]))
        }
        
        # No conversion for the sensors which are not in the Shield only if sensor data exist
        No.Shield.gas.Sensors <- setdiff(list.gas.sensor, Shield$gas.sensor)
        No.Shield.gas.Sensors <- No.Shield.gas.Sensors[which(c(paste0("Out.",No.Shield.gas.Sensors) %in% names(General.DT) ))]
        if (length(No.Shield.gas.Sensors) > 0) {
            No.Shield.name.Sensors <- list.name.sensor[list.gas.sensor %in% No.Shield.gas.Sensors]
            x <- General.DT[,paste0("Out.",No.Shield.gas.Sensors), with = FALSE]
            data.table::set(General.DT,  j =  paste0(No.Shield.name.Sensors,"_volt"),
                            value = lapply(seq_len(ncol(x)), function(i) x[[i]]))}
        
        # Adding Absolute Humidity and T_Deficit
        if (all(c("Out.Temperature", "Out.Relative_humidity") %in% names(General.DT))){
            General.DT <- Add_AbsHum_Tdef(General.DT = General.DT, name.Temp = "Out.Temperature", name.RH    = "Out.Relative_humidity",
                                          name.AbsHR = "Absolute_humidity", name.Tdef = "Td_deficit")}
        if (all(c("Out.Temperature_int", "Out.Relative_humidity_int") %in% names(General.DT))){
            General.DT <- Add_AbsHum_Tdef(General.DT = General.DT, name.Temp = "Out.Temperature_int", name.RH = "Out.Relative_humidity_int",
                                          name.AbsHR = "Absolute_humidity_int", name.Tdef = "Td_deficit_int")}
    }
    return(General.DT)
}

#' Filter and convert sensor data in data.table General.DT
#' 
#' @param ASE.ID list, output of function Identify_ASE or Identify_ASE_Dir with information on AirSensEUR configuration. 
#' If ASE.ID is NULL, General.DT, boxConfig, list.gas.sensor, list.name.sensor, ASEDir, Shield shall be passed.
#' @param General.DT a data.table with all ASE Box and reference data. Default is NULL. if NULL, Generalcsv loaded with Identify_ASE is loaded
#' @param list.gas.sensor a vector of string representing the names of sensors on which to apply the indexing of dates within warming times. default is NULL. if NULL, boxConfig is use to list gas sensor
#' @param list.gas.sensor a vector of string representing the names of sensors on which to apply the indexing of dates within warming times.
#' @return : f.General.DT the data.table with new columns for filtering of data if needed
Filter_Sensor_Data <- function(ASE.ID = NULL, General.DT = NULL, boxConfig = NULL, list.gas.sensor = NULL, list.name.sensor = NULL, ASEDir = NULL, Shield = NULL) {
    futile.logger::flog.info(paste0("[Filter_Sensor_Data] ", basename(ASEDir), " starting filtering and converting sensor data"))
    if (is.null(General.DT))       General.DT       <- ASE.ID$General.DT
    if (is.null(list.gas.sensor))  list.gas.sensor  <- ASE.ID$list.gas.sensor
    if (is.null(list.name.sensor)) list.name.sensor <- ASE.ID$list.sensors
    if (is.null(ASEDir))           ASEDir           <- ASE.ID$ASEDir
    if (is.null(Shield))           Shield           <- ASE.ID$Config$sens2ref.shield
    if (is.null(boxConfig))        boxConfig        <- ASE.ID$Config
    ind.warm.out     <- Warm_Index(General.DT = General.DT, list.gas.sensor  = list.gas.sensor, boxConfig = boxConfig, ASEDir = ASEDir)
    ind.TRh.out      <- TRh_Index(General.DT  = General.DT, list.gas.sensor  = list.gas.sensor, boxConfig = boxConfig, ASEDir = ASEDir)
    ind.Invalid.out  <- Inv_Index(General.DT  = General.DT, list.name.sensor = list.name.sensor,boxConfig = boxConfig, ASEDir = ASEDir)
    ind.send.out     <- Outliers_Sens(General.DT = General.DT, list.gas.sensor  = list.gas.sensor, list.name.sensor = list.name.sensor, boxConfig = boxConfig, ASEDir = ASEDir,
                                      ind.warm.out = ind.warm.out, ind.TRh.out = ind.TRh.out, ind.Invalid.out = ind.Invalid.out)
    General.DT       <- ind.send.out$General.DT
    ind.send.out     <- ind.send.out$ind.sens.out
    General.DT       <- Sens_Conv(General.DT = General.DT, list.gas.sensor  = list.gas.sensor, list.name.sensor = list.name.sensor, boxConfig = boxConfig, ASEDir = ASEDir,
                                  Shield = Shield)
    return(list(General.DT = General.DT, ind.warm.out = ind.warm.out, ind.TRh.out = ind.TRh.out, ind.Invalid.out = ind.Invalid.out))}

#' Filtering of reference data. All reference parameters that are not filtered yet, are going to be filtered. If reference are duplicated the filtering is based on the first sensor calibrated with the reference parameter
#' @param ASE.ID mandatory, list, output of function Identifiy_ASE or Identify_ASE_Dir with information on AirSensEUR configuration
#' If ASE.ID is NULL, General.DT, boxConfig, list.gas.sensor, list.name.sensor, ASEDir, Shield shall be passed.
#' @param General.DT a data.table with all ASE Box and reference data. Default is NULL. if NULL, General.csv in ASE.ID$General.DT is used
#' @param list.reference optional, default is NULL. A vector of string representing the names of reference column on which to apply the indexing of dates for reference measurements outliers.
#'  if NULL, ASE.ID$list.reference is used to list reference measurements.
#' @param list.name.sensor optional, default is NULL. Not used any more kept for consistency with previous version. It used to be a vector of string representing the names of sensors
#'  on which to apply the indexing of dates of sensor measurement outliers.
#' @param ASEDir optional, default is NULL. Directory of the ASE used in. if NULL, ASE.ID$ASEDir is used.
#' @param ASE.cfg optional, default is NULL. Datable with configuration of sensor for the ASE box. if NULL, ASE.ID$ASE.cfg is used.
#' @param DIR_General optional, default is "General_data". SubDirectory of the ASE.ID containing sensor data and filtering data files("General.csv", ...). This is the subdirectory 
#' where the file "ind_ref_out.RDS" could be saved. See argument "Save".
#' @param Save optional, logical with default value TRUE. If Save is TRUE, return.ind.ref.out the RDS file with outliers info is save in file.path(ASEDir, DIR_General).
#' as "ind_ref_out.RDS". If Save is FALSE the RDS file is not saved.
#' @return a data.table corresponfing to General.DT with a columns paste0("Out.",list.reference) where outliers are set to NA:
Filter_Ref_Data <- function(ASE.ID = NULL, General.DT = NULL, list.reference = NULL, list.name.sensor = NULL, ASEDir = NULL, ASE.cfg = NULL,
                            DIR_General = "General_data", Save = TRUE) {
    if (is.null(General.DT))       General.DT       <- ASE.ID$General.DT
    if (is.null(list.reference))   list.reference   <- ASE.ID$list.reference
    if (is.null(ASEDir))           ASEDir           <- ASE.ID$ASEDir
    if (is.null(ASE.cfg))          ASE.cfg          <- ASE.ID$ASE.cfg
    ASE.name <- basename(ASEDir)
    # list of index of negative values
    ind.neg <- apply(X = General.DT[, .SD, .SDcols = list.reference[list.reference %in% names(General.DT)]], MARGIN = 2, function(x) {which(x < 0)})
    for (i in seq_along(list.reference)) {
        # Column name of filtered reference parameter
        Vector.columns <- paste0(c("Out."),list.reference[i])
        
        # check that all reference parameters exists
        if (list.reference[i] %in% names(General.DT)) {
            # resetting to initial values
            futile.logger::flog.info(paste0("[Filter_Ref_Data] ", ASE.name," Initialising filtered reference data columns for ", list.reference[i]))
            
            # Re-initialising Out.Ref... values: setting Out.Ref... column the initial values of Ref...
            General.DT[,(Vector.columns) := rep(list(General.DT[[list.reference[i]]]), times = length(Vector.columns))]
            
            # number index of reference pollutant in the names(ASE.cfg) based on the first name of reference parameter and not on sensor
            k <- which(ASE.cfg[ASE.cfg$name.gas == "gas.reference2use"] == list.reference[i])[1]
            # discarding negative values if needed
            if (as.logical(ASE.cfg[ASE.cfg$name.gas == "remove.neg", k, with = F][[1]])) {
                if (exists("ind.neg") && length(ind.neg) > 0 && length(ind.neg[[list.reference[i]]]) > 0) {
                    futile.logger::flog.info(paste0("[Filter_Ref_Data] ", ASE.name," Discarding negative reference data for ", list.reference[i]))
                    data.table::set(General.DT,i = ind.neg[[list.reference[i]]], j = Vector.columns, 
                                    value = rep(list(rep(NA, times = length(ind.neg[[list.reference[i]]]))), times = length(Vector.columns)))}}
            # Initial reference data in outlier column
            futile.logger::flog.info(paste0("[Filter_Ref_Data] ", ASE.name," initialising outlier reference data for ", list.reference[i]))
            iters  <- as.numeric(ASE.cfg[ASE.cfg$name.gas == "Ref.iterations", k, with = F][[1]])
            
            # deleting bigger iterations
            repeat {
                if (paste0("Out.",list.reference[i],".", iters + 1) %in% names(General.DT)) {
                    data.table::set(General.DT, j = paste0("Out.",list.reference[i],".",iters + 1), value = NULL)
                    iters <- iters + 1
                } else break # leaving the repeat loop if there are no higher iterations
            }
            # Index of outliers for reference data
            futile.logger::flog.info(paste0("[Filter_Ref_Data] ", ASE.name," detecting row indexes of outliers in reference data for ", list.reference[i]))
            if (as.logical(ASE.cfg[ASE.cfg$name.gas == "Ref.rm.Out", k, with = F][[1]])) {
                # number of iterations
                for (j in 1:as.numeric(ASE.cfg[ASE.cfg$name.gas == "Ref.iterations", k, with = F][[1]])) {
                    if (list.reference[i] %in% names(General.DT)) {
                        if (all(is.na(General.DT[[list.reference[i]]]))) {
                            futile.logger::flog.error("[Filter_Ref_Data] ", ASE.name," no reference data for filtering outliers")
                        } else {
                            #Y <- General.DT[[paste0("Out.Neg.",list.reference[i])]]
                            Y <- General.DT[[paste0("Out.",list.reference[i])]]
                            # setting the outliers of previous iterations to NA. If null then stop outlier detection
                            if (j > 1) {
                                if (length(which(return.ind.ref.out[[paste0(list.reference[i],".",(j - 1))]]$Outliers)) != 0) {
                                    Y[as.numeric(paste(unlist(sapply(return.ind.ref.out[c(paste0(list.reference[i],".",1:(j - 1)))], function(x) which(x$Outliers)))))] <- NA
                                }  else break
                            }
                            futile.logger::flog.info(paste0("[Filter_Ref_Data] ", ASE.name," Reference: ",list.reference[i],", iteration: ",j))
                            Outli <- My.rm.Outliers(ymin         = as.numeric(ASE.cfg[ASE.cfg$name.gas == "Ref.Ymin", k, with = F][[ 1]]),
                                                    ymax         = as.numeric(ASE.cfg[ASE.cfg$name.gas == "Ref.Ymax", k, with = F][[1]]),
                                                    ThresholdMin = as.numeric(ASE.cfg[ASE.cfg$name.gas == "Ref.ThresholdMin", k, with = F][[1]]),
                                                    date         = General.DT[["date"]],
                                                    y            = Y,
                                                    window       = as.numeric(ASE.cfg[ASE.cfg$name.gas == "Ref.window", k, with = F][[1]]),
                                                    threshold    = as.numeric(ASE.cfg[ASE.cfg$name.gas == "Ref.threshold", k, with = F][[1]]),
                                                    plotting     = FALSE
                            )
                            nameInd      <- paste0(list.reference[i],".",j)
                            OutlinameInd <- paste0(list.reference[i],".",j,".Outli")
                            assign(nameInd , data.frame(date = Outli$date,
                                                        Outliers = unlist(apply(Outli[,c("Low_values","High_values","OutliersMin","OutliersMax")], MARGIN = 1, function(x) any(x))),
                                                        stringsAsFactors = FALSE))
                            if (exists("return.ind.ref.out")) {
                                return.ind.ref.out[[nameInd]] <- get(nameInd)   
                            } else {
                                return.ind.ref.out <- list(get(nameInd))
                                names(return.ind.ref.out) <- nameInd}
                            return.ind.ref.out[[OutlinameInd]] <- Outli
                            # Discarding outliers
                            if (paste0(list.reference[i],".",j) %in% names(return.ind.ref.out)) {
                                i.rows <- which(return.ind.ref.out[[paste0(list.reference[i],".",j)]]$Outliers)
                                if (length(i.rows) > 0) {
                                    data.table::set(General.DT, i = which(return.ind.ref.out[[paste0(list.reference[i],".",j)]]$Outliers), j = paste0("Out.",list.reference[i]),
                                                    value = list(rep(NA, times = length(which(return.ind.ref.out[[paste0(list.reference[i],".",j)]]$Outliers)))))
                                }}
                        }
                    } else futile.logger::flog.error("[Filter_Ref_Data] ", ASE.name," Warning no reference values impossible to discard outliers")
                }
            }
            # Every time data is filtered or not filtered update of paste0("Lag.Out.",list.reference[i]) 
            data.table::set(General.DT, j = paste0("Lag.Out.",list.reference[i]),
                            value = General.DT[[paste0("Out.",list.reference[i])]])
            
            ##### checking for periodicity >= 1 h and replacing the "Out.Ref" and ' Lag.Out.Ref." values with 'Ref.' values for these periodicity,
            ##### if any (Mostly >=1 h periodicity causes filtering the reference values out)
            ##### THIS IS NO MORE NECESARY SINCE My.rm.Outliers takes  care of the Time resolution/Periodicity of data >= 60 minutes
            #General.DT <- DF_sd(DF=General.DT, Col.for.sd = list.reference[i], width = 60L)$DT
            
        } else General.DT[,(Vector.columns) := rep(list(rep(NA, times = nrow(General.DT))), times = length(Vector.columns))]
    }

    
    # Adding Absolute humidity and T.Deficit
    if (all(c("Out.Ref.Temp", "Out.Ref.RH") %in% names(General.DT))){
        General.DT <- Add_AbsHum_Tdef(General.DT = General.DT, name.Temp = "Out.Ref.Temp", name.RH = "Out.Ref.RH",
                                      name.AbsHR = "Ref.Absolute_humidity", name.Tdef = "Ref.Td_deficit")}
    # saving the outlier list
    if (exists("return.ind.ref.out")) {
        if (Save) {
            ind.ref.out.file <- file.path(ASEDir, DIR_General, "ind_ref_out.RDS")
            list.save(x = return.ind.ref.out, file = ind.ref.out.file)
            futile.logger::flog.info(paste0("[Filter_Ref_Data] ", ASE.name," A new ind.ref.out.RDS was saved."))}}
    
    # Returning the data.table
    return(General.DT)
}

#' Completing General.DT with columns related to filtering data for sensors ("sensor_volt") and reference data ("Out.Ref") 
#' and filtering if any index file for warming, TRh, Invalid and sensor, refernce outlier is missing
#' 
#' @param f.list.name.sensor a vector of sensor names to be completed if needed with columns for filtering data
#' @param f.list.sensors a vector of sensors to be completed if needed with columns for filtering data
#' @param f.General.DT the data.table to be completed
#' @param f.Config A List returned by function CONFIG(). The easiest way to get is to use function Identify_ASE()/Identify_ASE_Dir().
#' @param f.ASEDir A character vector with the file path of the ASE box to be submitted to the function Identify.ASE
#' @param f.Shield  A data.table, configuration of chemical sensors, returned by function CONFIG(), named shield. The easiest way to get is to use function Identify_ASE()/Identify_ASE_Dir().
#' @param f.list.reference A character vector, corresponding to the names of reference varaiable for which filtering will take place. These names shall be among the header of f.General.DT and withing ASE.cfg column Reference2use.
#' @param f.ASE.cfg A data.table, configuration of AirSensEUR for calibration, filtering..., returned by function CONFIG(), named ASE.cfg The easiest way to get is to use function Identify_ASE()/Identify_ASE_Dir().
#' @param DIR_Models File path of the sub-directory of f.ASEDir where are the calibration models.
#' @param DIR_General File path of the sub-directory of f.ASEDir where is the file General.csv.
#' @param Filter.Sens logical default is TRUE. If FALSE  filtering of sensor data is not performed.
#' @param Filter.Ref logical default is TRUE. If FALSE  filtering of reference data is not performed.
#' @return : f.General.DT the data.table with new columns for filtering of data if needed
Complete_General <- function(f.list.name.sensor, f.list.gas.sensor, f.General.DT, f.Config, f.ASEDir, f.Shield, 
                             f.list.reference, f.ASE.cfg, DIR_Models = "Models", DIR_General = "General_data", Filter.Sens = TRUE, Filter.Ref = TRUE) {
    ASE.name <- basename(f.ASEDir)
    Save.General <- FALSE
    # be sure that sensor data are filtered if needed only if sensor exist
    Missing.Sensor <- which(!paste0(f.list.name.sensor, "_volt") %in% names(f.General.DT) & f.list.gas.sensor %in% names(f.General.DT))
    index.files    <- file.path(f.ASEDir, DIR_General,c("ind_warm.RDS","ind_TRh.RDS","ind_Invalid.RDS","ind_sens_out.RDS"))
    Missing.index.files <- which(!sapply(index.files, file.exists))
    if (Filter.Sens && (length(Missing.Sensor) > 0 || length(Missing.index.files) > 0)) {
        if (length(Missing.Sensor) > 0) {
            futile.logger::flog.warn(paste0("[Complete_General] ",ASE.name," ",paste(paste0("Out.",f.list.name.sensor[Missing.Sensor]), collapse = ", "), " is (are) missing."))}
        if (length(Missing.index.files) > 0) {
            futile.logger::flog.warn(paste0("[Complete_General] ",ASE.name," ", basename(index.files)[Missing.index.files], " file is (are) missing."))}
        futile.logger::flog.info(paste0("[Complete_General] ",ASE.name," the Filtering of sensor data is going to be carried out."))
        f.General.DT <- Filter_Sensor_Data(General.DT = f.General.DT, list.gas.sensor = f.list.gas.sensor, list.name.sensor = f.list.name.sensor, 
                                           boxConfig = f.Config, ASEDir = f.ASEDir, Shield = f.Shield)$General.DT
        Save.General <- TRUE}
    # be sure that reference data are filtered if needed
    Missing.Ref <- which(!paste0("Out.", unique(f.list.reference)) %in% names(f.General.DT) & paste0(unique(f.list.reference)) %in% names(f.General.DT))
    index.files    <- file.path(f.ASEDir, DIR_General,c("ind_ref_out.RDS"))
    Missing.index.files <- which(!sapply(index.files, file.exists))
    if (Filter.Ref && (length(Missing.Ref) > 0 || length(Missing.index.files) > 0)) {
        futile.logger::flog.warn(paste0("[Complete_General] ",ASE.name," ",paste(paste0("Out.",unique(f.list.reference)[Missing.Ref]), collapse = ", "), " are missing. Reference data data are not filtered."))
        futile.logger::flog.info(paste0("[Complete_General] ",ASE.name," The Filtering of reference data is going to be carried out."))
        f.General.DT <- Filter_Ref_Data(General.DT = f.General.DT, list.reference = unique(f.list.reference), ASEDir = f.ASEDir, ASE.cfg = f.ASE.cfg)
        Save.General <- TRUE}
    # Saving new General.DT
    if (Save.General) {
        Missing.Modelled   <- !paste0(f.list.gas.sensor, "_modelled") %in% names(f.General.DT) & f.list.gas.sensor %in% names(f.General.DT)
        Available.Cal.func <- !is.na(f.Config$sens2ref$Cal.func) & f.Config$sens2ref$Cal.func != ""
        Sensor2Calibrate   <- sort(which(Missing.Modelled & Available.Cal.func), decreasing = T)
        if (length(Sensor2Calibrate) > 0) {
            for (k in Sensor2Calibrate) {
                #if(f.list.gas.sensor[k] == "Nitric_oxide") browser()
                Model <- file.path(f.ASEDir, "Models", paste0(basename(f.ASEDir), "__", f.list.name.sensor[k], "__",f.Config$sens2ref$Cal.func[k]))
                if (file.exists(Model)) f.General.DT <- Apply_Model(Model = Model, General.DT = f.General.DT, ASE.cfg = f.ASE.cfg, Config =  f.Config, Shield =  f.Config$sens2ref.shield)}}
        data.table::fwrite(f.General.DT, file.path(f.ASEDir, DIR_General,"General.csv"), showProgress = T)
        futile.logger::flog.info(paste0("[Complete_General] A new General.csv file was saved."))}
    return(f.General.DT)
}


#' Apply Multi calibration models in order to predict sensor measurements for one sensor of one ASE box
#'
#' @param MultiCal logical, default is TRUE. If TRUE a table with data.dates with in/End dates and calibration models by row of dates is used in order to Apply calibration models
#' @param MultiCal.file full file path and name of file including the data.table with columns "In", "End", "Model" and Enabled.
#' @param ASE.ID list, output of function Identifiy_ASE or Identify_ASE_Dir with information on AirSensEUR configuration
#' @param General.DT a data.table with all ASE Box and reference data. Default is NULL. if NULL, Generalcsv loaded with Identify_ASE is loaded

Apply_MultiCal_Model <- function(MultiCal = TRUE, MultiCal.file = NULL, ASE.ID = NULL, Model, Mod_type = NULL, name.sensor = NULL, Variables = NULL, General.DT = NULL, ASE.cfg = NULL, SetTime = NULL, Config = NULL,
                                 Shield = NULL, Filter.Sens = TRUE, Filter.Ref = TRUE){
    # Model for Identifying in case of MultiCal
    if (MultiCal){
        stopifnot(file.exists(MultiCal.file))
        MultiCal.DT <- data.table::fread(file = MultiCal.file)
        stopifnot(names(all("In", "End", "Model", "Enabled") %in% names(MultiCal.DT)))
        if(!lubridate::is.POSIXct(MultiCal.DT$In))  set(MultiCal.DT, j = "In"     , value = lubridate::ymd(MultiCal$In,  tz = "UTC"))
        if(!lubridate::is.POSIXct(MultiCal.DT$ENd)) set(MultiCal.DT, j = "End"    , value = lubridate::ymd(MultiCal$End, tz = "UTC"))
        set(MultiCal.DT, j = "Model"  , value = file.path(basename(MultiCal.file), "Models", MultiCal$Model))
        if(!is.logical(MultiCal.DT$Enabled)) set(MultiCal.DT, j = "Enabled", value = as.logical(MultiCal$Enabled))
        Model <- Multical.DT$Model[1]}
    
    # Identify ASE box characteristics
    if (is.null(ASE.ID) & (is.null(General.DT) || is.null(ASE.cfg))) {
        ASE.ID <- Identify_ASE(Model = Model, General.DT = General.DT, ASE.cfg = ASE.cfg, SetTime = SetTime, 
                               Config = Config, Shield = Shield, Filter.Sens = Filter.Sens, Filter.Ref = Filter.Ref)}
    
    # Extract General.DT
    if (is.null(General.DT)) General.DT <- ASE.ID$General.DT
    # Extract ASE.cfg
    if (is.null(ASE.cfg)) ASE.cfg <- ASE.ID$ASE.cfg
    # Extract Mod_type
    if (is.null(Mod_type)) {
        if (!is.null(ASE.ID))  Mod_type <- ASE.ID$Mod_type else Mod_type <- unlist(strsplit(basename(Model), "__"))[4]}
    # Extract name.sensor
    if (is.null(name.sensor)) {
        if (!is.null(ASE.ID)) name.sensor <- ASE.ID$name.sensor else name.sensor <- unlist(strsplit(basename(Model), "__"))[2]} 
    # Extract k, nameGasVolt, nameGasMod
    if (is.null(ASE.ID)) {
        nameGasVolt <- paste0(name.sensor,"_volt")      # sensor gas in volt or nA or Count
        k           <- as.integer(which(unlist(ASE.cfg[ASE.cfg$name.gas == "name.sensor"]) == name.sensor))
        gas.sensor  <- unlist(ASE.cfg[ASE.cfg$name.gas == "gas.sensor", k, with = FALSE])
        nameGasMod  <- paste0(gas.sensor,"_modelled")   # modelled sensor gas
    } else {
        k           <- ASE.ID$k
        nameGasVolt <- ASE.ID$nameGasVolt
        nameGasMod  <- ASE.ID$nameGasMod}
    
    # Applying calibration model for each raw of MultiCal.DT
    for (Row in 1:nrow(MultiCal.DT)){
        # Extract Covariates
        if (is.null(Variables)) {
            if (!is.null(ASE.ID)) Variables <- ASE.ID$Variables else Variables <- unlist(strsplit(basename(Model), "__"))[7]}
        if (!is.null(Variables) && Variables != "") {
            CovMod  <- unlist(strsplit(x = Variables, split = "&", fixed = T))
            # Checking if there are "-" in the CovMod, deleting degrees of polynomial
            if (any(grepl(pattern = "-", x = CovMod[1]))) {
                Model.CovMod <- unlist(strsplit(x = CovMod , split = "-"))
                CovMod  <- Model.CovMod[ seq(from = 1, to = length(Model.CovMod), by = 2) ]
                Degrees <- Model.CovMod[ seq(from = 2, to = length(Model.CovMod), by = 2) ]
            } else {
                CovMod  <- CovMod
                Degrees <- rep(1, length(CovMod))}
            # take only the one that is not NA of y = General.DT[!is.na(General.DT[, nameGasVolt]), nameGasVolt]
            No.CovMod <- which(!CovMod %in% names(General.DT))
            if (length(No.CovMod) > 0) futile.logger::flog.error(paste0("[Apply_MultiCal_Model] ", paste0(CovMod[No.CovMod], collapse = ", "), " is not included into General.DT. Please check names of covariates."))
            is.not.NA.y <- which(complete.cases(General.DT[, .SD, .SDcols = c(nameGasVolt, CovMod)]))}
        # Loading calibration Model
        Model.i <- load_obj(file.path(Model))
        # Preparing the matrix of covariates
        if (Mod_type == "MultiLinear") {
            Matrice     <- data.frame(General.DT[is.not.NA.y, ..CovMod], row.names = row.names(General.DT[is.not.NA.y,]), stringsAsFactors = FALSE)
            names(Matrice) <- CovMod
        } else if (any(mod.eta.model.type %in% c("NO2_Lab", "NO2_Lab_decay_inc"))) {
            if (!shiny::isTruthy(Covariates) || any(Covariates == "")) Covariates <- c("Out.Relative_humidity", "Out.Temperature")
            Degrees          <- rep(1, length(Covariates))
            namesCovariates  <- paste0(paste(Covariates,Degrees, sep = "-"),collapse = "&")
            Matrice          <- General[is.not.NA.y, .SD, .SDcols = c(Covariates, "date")]
            #names(Matrice)  <- namesCovariates
        } else if (Mod_type %in% c("Peaks_baseline","exp_kT_NoC","exp_kT", "exp_kTn", "exp_kK","T_power", "K_power")) {
            Matrice     <- data.frame(General.DT[is.not.NA.y, ..CovMod], row.names = row.names(General.DT[is.not.NA.y,]), stringsAsFactors = FALSE)
            names(Matrice) <- CovMod
            Matrice$T.min <- caTools::runmin(Matrice[,CovMod], 1440)
            Matrice$date <- General.DT[is.not.NA.y][["date"]]
        } else if (Mod_type %in% c("BeerLambert")) {
            # take only the one that is nor NA of y = General.DT[!is.na(General.DT[, nameGasVolt]), nameGasVolt]
            is.not.NA.y <- which(complete.cases(General.DT[, .SD, .SDcols = c(nameGasVolt, "Out.Temperature", "Out.Atmospheric_pressure")]))
            Matrice     <- data.frame(General.DT[is.not.NA.y, c("Out.Temperature", "Out.Atmospheric_pressure")], row.names = row.names(General.DT[is.not.NA.y,]), stringsAsFactors = FALSE)
            names(Matrice) <- c("Out.Temperature", "Out.Atmospheric_pressure")
        } else if (Mod_type %in% c("Kohler", "Kohler_modified","Kohler_lit", "Kohler_only", "Ridge")) {
            # take only the one that is nor NA of y = General.DT[!is.na(General.DT[, nameGasVolt]), nameGasVolt]
            is.not.NA.y <- which(complete.cases(General.DT[, .SD, .SDcols = c(nameGasVolt, CovMod)]))
            Matrice     <- data.frame(General.DT[is.not.NA.y, CovMod, with = F], row.names = row.names(General.DT[is.not.NA.y,]), stringsAsFactors = FALSE)
            names(Matrice) <- CovMod
        } else {
            # Removing na for nameGasMod for nameGasVolt missing
            is.not.NA.y    <- General.DT[is.finite(get(nameGasVolt)), which = T]
            Matrice <- NULL
        }
        # Using the reverse calibration function (measuring function) to extrapolate calibration
        data.table::set(General.DT, j = nameGasMod, value = rep(NA_real_, nrow(General.DT)))
        data.table::set(General.DT, i = is.not.NA.y, j = nameGasMod,
                        value = list(Meas_Function(y          = General.DT[[nameGasVolt]][is.not.NA.y],
                                                   Mod_type   = Mod_type ,
                                                   covariates = CovMod,
                                                   Degrees    = Degrees,
                                                   Model      = Model.i,
                                                   Matrice    = Matrice)))
    }
}


#' Apply a calibration model in order to predict sensor measurements for one sensor of one ASE box
#'
#' @param ASE.ID list, output of function Identifiy_ASE or Identify_ASE_Dir with information on AirSensEUR configuration
#' @param Model full file path and name of model of the calibration model. The extension of the model file shall be .Rdata only, no .RDS file.
#' @param name.sensor character vector with the name(s) of sensor (e. g. CO_A4_P1) to look for models. Default is NULL. 
#' If NULL name.sensor is extracted from Model using Identify_ASE function
#' @param Mod_type Character vector, type of models of Model.i. Default is NULL. If NULL the model type is extracted from Model.i.
#' @param Variables Character vector giving the covariates of the calibration model. Covariates are separated with &. Default is NULL. if NULL Variables is extracted from Model.
#' @param General.DT a data.table with all ASE Box and reference data. Default is NULL. if NULL, Generalcsv loaded with Identify_ASE is loaded
#' @param ASE.cfg A data.table with all ASE box configuration. Default is null. If NULL the ASE.cfg file is loaded
#' @param SetTime A data.table with all ASE box SetTime configuration. Default is null. If NULL the ASE_SETTIME.cfg file is loaded
#' @param Filter.Sens logical, default is TRUE. If FALSE  filtering of sensor data is not performed.
#' @param Filter.Ref logical, default is TRUE. If FALSE  filtering of reference data is not performed.
#' @return General.DT with a column of predited sensor data after apply the calibration model 
#' @examples
#' General.DT <- Apply_Model(Model = Model, General.DT = General.DT, ASE.cfg = ASE.cfg)
Apply_Model <- function(ASE.ID = NULL, Model, Mod_type = NULL, name.sensor = NULL, Variables = NULL, General.DT = NULL, ASE.cfg = NULL, SetTime = NULL, Config = NULL,
                        Shield = NULL, Filter.Sens = TRUE, Filter.Ref = TRUE) {
    # Identify ASE box characteristics
    if (is.null(ASE.ID) & (is.null(General.DT) || is.null(ASE.cfg))) {
        ASE.ID <- Identify_ASE(Model = Model, General.DT = General.DT, ASE.cfg = ASE.cfg, SetTime = SetTime, 
                               Config = Config, Shield = Shield, Filter.Sens = Filter.Sens, Filter.Ref = Filter.Ref)}
    # Extract General.DT
    if (is.null(General.DT)) General.DT <- ASE.ID$General.DT
    # Extract ASE.cfg
    if (is.null(ASE.cfg)) ASE.cfg <- ASE.ID$ASE.cfg
    # Extract Mod_type
    if (is.null(Mod_type)) {
        if (!is.null(ASE.ID))  Mod_type <- ASE.ID$Mod_type else Mod_type <- unlist(strsplit(basename(Model), "__"))[4]}
    # Extract name.sensor
    if (is.null(name.sensor)) {
        if (!is.null(ASE.ID)) name.sensor <- ASE.ID$name.sensor else name.sensor <- unlist(strsplit(basename(Model), "__"))[2]} 
    # Extract nameGasVolt
    if (is.null(ASE.ID)) {
        nameGasVolt <- paste0(name.sensor,"_volt")      # sensor gas in volt or nA or Count
        k           <- as.integer(which(unlist(ASE.cfg[ASE.cfg$name.gas == "name.sensor"]) == name.sensor))
        gas.sensor  <- unlist(ASE.cfg[ASE.cfg$name.gas == "gas.sensor", k, with = FALSE])
        nameGasMod  <- paste0(gas.sensor,"_modelled")   # modelled sensor gas
    } else {
        k           <- ASE.ID$k
        nameGasVolt <- ASE.ID$nameGasVolt
        nameGasMod  <- ASE.ID$nameGasMod}
    # Extract Covariates
    if (!shiny::isTruthy(Variables)) {
        if (!is.null(ASE.ID)) Variables <- ASE.ID$Variables else Variables <- unlist(strsplit(basename(Model), "__"))[7]}
    if (!is.null(Variables) && Variables != "") {
        CovMod  <- unlist(strsplit(x = Variables, split = "&", fixed = T))
        # Checking if there are "-" in the CovMod, deleting degrees of polynomial
        if (any(grepl(pattern = "-", x = CovMod[1]))) {
            Model.CovMod <- unlist(strsplit(x = CovMod , split = "-"))
            CovMod  <- Model.CovMod[ seq(from = 1, to = length(Model.CovMod), by = 2) ]
            Degrees <- Model.CovMod[ seq(from = 2, to = length(Model.CovMod), by = 2) ]
        } else {
            CovMod  <- NULL
            Degrees <- NULL}
        # Replace K96_Water_vapour with K96_MPLCPC
        if (any(grepl("K96_Water_vapour",CovMod))) {browser(); CovMod <- sub("K96_Water_vapour", "K96_MPLCPC", CovMod)}
        # take only raws wihtout NA for c(nameGasVolt, CovMod)
        No.CovMod <- which(!CovMod %in% names(General.DT))
        if (length(No.CovMod) > 0) futile.logger::flog.error(paste0("[Meas_Func] ", paste0(CovMod[No.CovMod], collapse = ", "), " is not included into General.DT. Please check names of covariates."))
        is.not.NA.y <- which(is.finite(rowSums(General.DT[, .SD, .SDcols = c(nameGasVolt, CovMod[!sapply(General.DT[,..CovMod], inherits, "POSIXct")])])))
        if(length(is.not.NA.y) == 0) futile.logger::flog.warn("[Apply_Model] no complete row to apply calibration model. lengt(is.not.NA.y) = 0")}
    # Loading calibration Model
    Model.i <- load_obj(file.path(Model))
    # Preparing the matrix of covariates
    if (Mod_type == "MultiLinear") {
        Matrice     <- data.frame(General.DT[is.not.NA.y, ..CovMod], row.names = row.names(General.DT[is.not.NA.y,]), stringsAsFactors = FALSE)
        names(Matrice) <- CovMod
    } else if (any(Mod_type %in% c("NO2_Lab", "NO2_Lab_decay_inc"))) {
        if (is.null(CovMod) || length(CovMod) == "0" || CovMod == "")  CovMod <- c("Out.Relative_humidity", "Out.Temperature")
        Degrees          <- rep(1, length(CovMod))
        Matrice          <- General.DT[is.not.NA.y, .SD, .SDcols = c(CovMod, "date")]
    } else if (Mod_type %in% c("Peaks_baseline","exp_kT_NoC","exp_kT", "exp_kTn", "exp_kK","T_power", "K_power", "Yatkin")) {
        name.Temperature <- grep("emp" , CovMod, value = T)[1]
        name.Humidity    <- grep("umid", CovMod, value = T)[1]
        Matrice     <- data.frame(General.DT[is.not.NA.y, ..CovMod], row.names = row.names(General.DT[is.not.NA.y,]), stringsAsFactors = FALSE)
        names(Matrice) <- CovMod
        Matrice$T.min <- caTools::runmin(Matrice[,name.Temperature], 1440)
        Matrice$date <- General.DT[is.not.NA.y][["date"]]
    } else if (Mod_type %in% c("BeerLambert")) {
        # take only the one that is nor NA of y = General.DT[!is.na(General.DT[, nameGasVolt]), nameGasVolt]
        is.not.NA.y <- which(is.finite(rowSums(General.DT[, .SD, .SDcols = c(nameGasVolt, "Out.Temperature", "Out.Atmospheric_pressure")])))
        Matrice     <- data.frame(General.DT[is.not.NA.y, c("Out.Temperature", "Out.Atmospheric_pressure")], row.names = row.names(General.DT[is.not.NA.y,]), stringsAsFactors = FALSE)
        names(Matrice) <- c("Out.Temperature", "Out.Atmospheric_pressure")
    } else if (Mod_type %in% c("Kohler", "Kohler_modified", "Kohler_lit", "Kohler_only", "Ridge")) {
        # take only the one that is nor NA of y = General.DT[!is.na(General.DT[, nameGasVolt]), nameGasVolt]
        is.not.NA.y <- which(is.finite(rowSums(General.DT[, .SD, .SDcols = c(nameGasVolt, CovMod)])))
        Matrice     <- data.frame(General.DT[is.not.NA.y, CovMod, with = F], row.names = row.names(General.DT[is.not.NA.y,]), stringsAsFactors = FALSE)
        names(Matrice) <- CovMod
    } else {
        # Removing na for nameGasMod for nameGasVolt missing
        is.not.NA.y    <- General.DT[is.finite(get(nameGasVolt)), which = T]
        Matrice <- NULL}
    # Using the reverse calibration function (measuring function) to extrapolate calibration
    data.table::set(General.DT, j = nameGasMod, value = rep(NA_real_, nrow(General.DT)))
    Predicted <- Meas_Function(y          = General.DT[[nameGasVolt]][is.not.NA.y],
                               Mod_type   = Mod_type ,
                               covariates = CovMod,
                               Degrees    = Degrees,
                               Model      = Model.i,
                               Matrice    = Matrice,
                               name.sensor= name.sensor,
                               name.Model = Model,
                               Verbose    = FALSE)
    if(Mod_type == "Yatkin"){
        is.not.NA.y <- which(General.DT$date %in% Predicted[!is.na(date)]$date) # which(General.DT$date %in% Predicted$date) 
        data.table::set(General.DT, i = is.not.NA.y, j = nameGasMod, value = Predicted[!is.na(date)]$x.fitted)
    } else data.table::set(General.DT, i = is.not.NA.y, j = nameGasMod, value = Predicted)
    
    # # Setting na for nameGasMod when nameGasVolt or CovMod are missing - Not necessary initially NA_Real
    # is.NA.y     <- setdiff(1:nrow(General.DT), is.not.NA.y)
    # if (length(is.NA.y) > 0) data.table::set(General.DT, i = is.NA.y, j = nameGasMod, value = list(rep(NA_real_, times = length(is.NA.y))))
    # setting negative values to NA
    if (unlist(ASE.cfg[ASE.cfg$name.gas == "Neg.mod", k, with = FALSE]) == TRUE) {
        data.table::set(General.DT, i = which(General.DT[, nameGasMod, with = FALSE] < 0), j = nameGasMod,
                        value = list(rep(NA, times = length(which(General.DT[, nameGasMod, with = FALSE] < 0)))))}
    
    # Adding Absolute humidity and T.deficit
    if (nameGasMod %in% c("Temperature_modelled", "Temperature_modelled") && all(c("Temperature_modelled", "Temperature_modelled") %in% names(General.DT))){
        General.DT <- Add_AbsHum_Tdef(General.DT = General.DT, name.Temp = "Temperature_modelled", name.RH = "Relative_humidity_modelled",
                                      name.AbsHR = "Absolute_humidity_modelled", name.Tdef = "Td_deficit_modelled")}
    
    return(General.DT)}

#' Fit a new calibration model
#'
#' @param ASEDir file path of the ASE Box used to save the Models as in file.path(ASEDir, DIR_Models).
#' @param General.DT a data.table with all ASE Box and reference data. Default is NULL. if NULL, Generalcsv loaded with Identify_ASE is loaded
#' @param ASE.cfg A data.table with all ASE box configuration. Default is null. If NULL the ASE.cfg file is loaded
#' @param SetTime A data.table with all ASE box SetTime configuration. Default is null. If NULL the ASE_SETTIME.cfg file is loaded
#' @param DIR_General character vector, default is "General_data". It sets a sub-directory that shall exist at ASEDir where is the file General.csv, .. are stored
#' @param DIR_Config Character vector giving the subdirectory of ASEDir where is the file ASE.name.cfg.
#' @param DIR_Models Character vector giving the subdirectory of ASEDir where are the calibration models are stored.
#' @param Cal.DateIN Starting date for calibration, POSIXct or Date type. Default is NULL If NULL, Cal.DateIN is extracted from SetTime.
#' @param Cal.DateEND Ending date for calibration, POSIXct or Date type. Default is NULL If NULL, Cal.DateEND is extracted from SetTime.
#' @param Meas.DateIN Starting date for prediction, POSIXct or Date type. Default is NULL If NULL, Meas.DateIN is extracted from SetTime.   NOT USED
#' @param Meas.DateEND Starting date for prediction, POSIXct or Date type. Default is NULL If NULL, Meas.DateEND is extracted from SetTime. NOT USED
#' @param name.sensor Character vector with the sensor name (e. g. CO_A4_P1) for which to fit a new calibration model. 
#' @param Mod_type Character vector, type of models of the model type to be fitted. Default is "Linear.Robust".
#' @param Probs numeric,  only use if Mod_type is "Linear.Robust". the quantile(s) to be estimated, it shall be a number strictly between 0 and 1, but if specified strictly outside this range, it is presumed that the solutions for all values of tau in (0,1) are desired. An object of class"rq"is returned, in the latter. Asof version 3.50, tau can also be a vector of values between 0 and 1. If NULL the 90th pqualtile will be used for "Linear.Robust".I not de
#' @param namesCovariates Character vector with parameters of calibration model separated with &. Default is NULL.
#' @param Plot_Line Logical, default is TRUE. If TRUE the calibration line is added using par(new=TRUE) to an existaing scatterplot.
#' @param PlotCal Logical, default TRUE, if TRUE plot the calibrated data (scatterplot and timeseries) are plotted after calibration.
#' @param Sync Logical, default is NULL, if NULL Sync.Cal in ASE.cfg is used. If False, lag is not evaluated and corrected. If TRUE Lag is evaluated and corrected.
#' @param DateCal default value NULL. If not, numeric vector giving the indexes of dates in ASE.ID$General.DT used for calibration
#' @return a calibration model which is also saved
#' @examples Fit_New_Model(ASEDir = ASEDir, name.sensor = "CO_A4_P1", Mod_type = "Linear.Robust", 
#'                              Cal.DateIN = DateIN, Cal.DateEND = DateEND, Meas.DateIN = ASE.ID$Meas.DateIN, Meas.DateEND = ASE.ID$Meas.DateEND,
#'                              Plot_Line = FALSE, PlotCal = FALSE, Verbose = FALSE)
Fit_New_Model <- function(ASEDir, General.DT = NULL, ASE.cfg = NULL, SetTime = NULL,
                          DIR_Config = "Configuration", DIR_Models = "Models", DIR_General = "General_data",
                          Cal.DateIN = NULL, Cal.DateEND = NULL, Meas.DateIN = NULL, Meas.DateEND = NULL,
                          name.sensor, Mod_type = "Linear.Robust", Probs = NULL, namesCovariates = "", degrees = rep("1", length(namesCovariates)+1),
                          Plot_Line = FALSE, PlotCal = FALSE, Verbose = TRUE, Include.Model = FALSE, SAVE = TRUE, Weighted = FALSE, Sync = NULL, DateCal = NULL) {
    # name of ASE box
    ASE.name <- basename(ASEDir)
    # init 
    # Extracting data 
    if (is.null(General.DT)) {
        General.DT     <- data.table::fread(file.path(ASEDir, DIR_General,"General.csv"), showProgress = T)
        data.table::set(General.DT, j = "date", value =  Set_date2POSIXct(General.DT[["date"]], tz = "UTC"))}
    if (is.null(ASE.cfg)) ASE.cfg <- data.table::fread(file.path(ASEDir, DIR_Config, paste0(ASE.name,".cfg")), showProgress = F)
    if (is.null(SetTime)){
        SetTime <- data.table::fread(file.path(ASEDir, DIR_Config, paste0(ASE.name,"_SETTIME.cfg")), showProgress = F)
        # Correcting old naming of DateCal.IN in Cal.IN and Out.Ref.IN in Ref.IN
        if(any(grepl("Out\\.", SetTime$name.gas)) || any(grepl("Date", SetTime$name.gas)) || any(grepl("Date\\.", SetTime$name.gas))){
            if (any(grepl("Date.", SetTime$name.gas))) data.table::set(SetTime, j = "name.gas", value = gsub("Date\\.","", SetTime$name.gas))
            if (any(grepl("Date", SetTime$name.gas)))  data.table::set(SetTime, j = "name.gas", value = gsub("Date","", SetTime$name.gas))
            if (any(grepl("Out.", SetTime$name.gas)))  data.table::set(SetTime, j = "name.gas", value = gsub("Out\\.","", SetTime$name.gas))
            data.table::fwrite(SetTime, file = file.path(ASEDir, DIR_Config, paste0(ASE.name,"_SETTIME.cfg")))}}
    k              <- as.integer(which(unlist(ASE.cfg[ASE.cfg$name.gas == "name.sensor"]) == name.sensor))
    name.gas       <- names(which(unlist(ASE.cfg[ASE.cfg$name.gas == "name.sensor"]) == name.sensor))
    gas.sensor     <- ASE.cfg[ASE.cfg$name.gas == "gas.sensor", k, with = FALSE]
    gas.reference2use <- ASE.cfg[ASE.cfg$name.gas == "gas.reference2use", k, with = FALSE]
    if (is.null(Cal.DateIN))   Cal.DateIN     <- max(c(as.Date(SetTime[name.gas == "Cal.IN"  ][[k]], optional = T), as.Date(SetTime[name.gas == "DatePlotCal.IN"  ][[k]], optional = T)), na.rm = TRUE)
    if (is.null(Cal.DateEND))  Cal.DateEND    <- min(c(as.Date(SetTime[name.gas == "Cal.END" ][[k]], optional = T), as.Date(SetTime[name.gas == "DatePlotCal.END" ][[k]], optional = T)), na.rm = TRUE)
    # if not all degrees are one, create MultiFile
    if (Mod_type == "MultiLinear" && any(degrees != "1")) {
        name.Multi  <- file.path(ASEDir, DIR_Config, paste0(ASE.name,"_Multi_",name.sensor,".cfg"))
        delete.Multi <- TRUE
        if (file.exists(name.Multi)) {
            file.copy(name.Multi, paste0(name.Multi,"_copy"))
            delete.Copy <- TRUE}
        Multi.df    <- data.frame("Covariates" = c(namesCovariates, "Intercept"), "Enabled" = TRUE, "degree" = c(degrees, NA), "Forced" = FALSE, "a0_an" = rep(1, length(namesCovariates) + 1), stringsAsFactors = F)
        write.table(Multi.df, file = name.Multi, row.names = FALSE)
    } else {
        delete.Multi <- FALSE
        name.Multi <- NULL
    } 
    # Fit Model
    Model <- Validation.tool(General            = General.DT,
                             DateIN             = Cal.DateIN,
                             DateEND            = Cal.DateEND,
                             name.gas           = name.gas,
                             model.log          = TRUE ,
                             nameGasRef         = paste0("Out.",gas.reference2use), # Gas reference
                             nameGasVolt        = paste0(name.sensor,"_volt"),      # sensor gas in volt or nA or Count
                             nameGasMod         = paste0(gas.sensor,"_modelled"),   # modelled sensor gas
                             unit.ref           = unlist(ASE.cfg[ASE.cfg$name.gas == "ref.unitgas"  , k, with = FALSE]),
                             unit.sensor        = unlist(ASE.cfg[ASE.cfg$name.gas == "Sens.unit"    , k, with = FALSE]),
                             Sens.raw.unit      = unlist(ASE.cfg[ASE.cfg$name.gas == "Sens.raw.unit", k, with = FALSE]),
                             Reference.name     = "",
                             AirSensEur.name    = ASE.name,
                             name.sensor        = name.sensor,
                             timeseries.display = FALSE,
                             WDoutputMod        = file.path(ASEDir,DIR_Models),
                             WDoutput           = file.path(ASEDir,"Calibration"),
                             WDoutputStats      = file.path(ASEDir,"Statistics"),
                             process.step       = "Calibration",
                             mod.eta.model.type = Mod_type,
                             Probs              = Probs,
                             Multi.File         = name.Multi,
                             eta.model.type     = unlist(ASE.cfg[ASE.cfg$name.gas == "eta.model.type", k, with = FALSE]),
                             remove.neg         = as.logical(unlist(ASE.cfg[ASE.cfg$name.gas == "remove.neg", k, with = FALSE])),
                             Covariates         = namesCovariates,
                             Plot_Line          = Plot_Line,
                             PlotCal            = PlotCal,
                             Auto.Lag           = ifelse(is.null(Sync), as.logical(unlist(ASE.cfg[ASE.cfg$name.gas == "Sync.Cal"  , k, with = FALSE])), Sync),
                             Verbose            = Verbose,
                             Include.Model      = Include.Model,
                             SAVE               = SAVE,
                             Weighted           = Weighted,
                             DateCal            = DateCal)
    # resume MUlti
    if (delete.Multi) {
        file.copy(paste0(name.Multi,"_copy"), name.Multi)
        unlink(paste0(name.Multi,"_copy"))
    }
    return(Model)
}
#' Fitting and saving calibration models rolling over a date interval
#'
#' @param ASEDir A character vector with the list of all filepaths of ASE boxes to be submitted to the function Roll_Fit_New_Model
#' @param DIR_Models File path of the subdirectory of ASEDir where  the calibration models are stored.
#' @param Interval Integer, default value is NULL. A number of days between Cal.DateIN and Cal.DateEND for rollling calibration models. 
#'                          IF NULL determined using SetTime file
#' @param DateIN Date, default is NULL, A date for the beginning of the rolling calibration intervals. If NULL DateIN is determined using Identify.ASE().
#' @param DateEN Date, default is NULL, A date for the end of the rolling calibration intervals. If NULL DateIN is determined using Identify.ASE().
#' @param name.sensors Character vector, with the sensors names for which rolling models are going to be fitted. Default is NULL, in this case models are fillted for all sensors
#' @param Mod_type Character vector, type of models to be fitted as defined in Cal_Line(). Default is "Linear.Robust".
#' @param namesCovariates Character vector with the explanatory variables in case of multiLinear model. Default is "".
#' @param degrees Character vector with the degrees of the explanatory variables in case of multiLinear model. Default is "".
#' @param DIR_Models Character vector, subdirectory of ASEDir where are the calibration models.
#' @param Weighted Logical default is FALSE. If TRUE calibration will use a weighted algorithm reducing input data in class of x and covariates.
#' @param Sync Logical, default is NULL, if NULL Sync.Cal in ASE.cfg is used. If False, lag is not evaluated and corrected. If TRUE Lag is evaluated and corrected.
#' @param DateCal default value NULL. If not, numeric vector giving the indexes of dates in ASE.ID$General.DT used for calibration
#' @return A character vector with the list of all new fitted models.
#' @examples
#' Roll_Fit_New_Model(List.ASE = List.ASE[1], ASEDir = ASEDir, Interval = NULL, DateIN = as.Date("2020-01-17"))
Roll_Fit_New_Model <- function(ASEDir, ASE.ID = NULL, DIR_Models = "Models", Interval = NULL, DateIN = NULL, DateEND = NULL,
                               name.sensors = NULL, Mod_type = "Linear.Robust", Probs = NULL, namesCovariates = "", degrees = "1", Verbose = TRUE, 
                               Weighted = FALSE, Sync = NULL, DateCal = NULL) {
    for (ASEDir in ASEDir) {
        # Identify ASE based on model name if any existing or on ASEDir
        ASE.name <- basename(ASEDir)
        if (is.null(ASE.ID)) ASE.ID <- Identify_ASE_Dir(ASEDir = ASEDir, name.sensor = name.sensors[1])
        # Determining names of sensors in ASE box if name.sensors is NULL
        if (is.null(name.sensors)) name.sensors <- ASE.ID$list.sensors
        for (name.sensor in name.sensors) {
            if (ASE.ID$ASEDir != ASEDir) {
                ASE.ID <- Identify_ASE_Dir(ASEDir = ASEDir, name.sensor = name.sensor, General.DT = ASE.ID$General.DT, ASE.cfg = ASE.ID$ASE.cfg, SetTime = ASE.ID$SetTime)
            } else if (ASE.ID$name.sensor != name.sensor) ASE.ID <- Identify_ASE_Dir(ASEDir = ASEDir, name.sensor = name.sensor)
            if (is.null(Interval)) Cal.Interval <- as.integer(ASE.ID$Cal.DateEND - ASE.ID$Cal.DateIN) else Cal.Interval <- Interval
            if (is.null(DateIN))   Meas.DateIN  <- ASE.ID$Meas.DateEND else Meas.DateIN  <- as.Date(DateIN) # as.Date added in case DateIn is a POSIXct
            Cal.DateIN  <- Meas.DateIN 
            if (is.null(DateEND)) {
                Meas.DateEND <- ASE.ID$Meas.DateEND 
            } else {
                Meas.DateEND <- as.Date(DateEND) # as.Date added in case DateIn is a POSIXct
            }
            Cal.DateEND <- min(Cal.DateIN + Cal.Interval, Meas.DateEND)
            
            # Checking that data are not empty
            if (!shiny::isTruthy(namesCovariates)) {
                List.Columns <- c(ASE.ID$nameGasVolt,ASE.ID$nameGasRef)
            } else List.Columns <- c(ASE.ID$nameGasVolt,ASE.ID$nameGasRef, namesCovariates)
            # Creating DateCal if null
            DateCal <- ASE.ID$General.DT[date > DateIN & date <= DateEND + 1, which = TRUE]
            
            while (Cal.DateEND <= Meas.DateEND) {
                #checking if there are data
                nameModel  <- file.path(ASEDir, DIR_Models, 
                                        paste0(c(paste0(c(ASE.name,name.sensor,ASE.ID$ASESens.raw.unit,Mod_type, 
                                                          format(Cal.DateIN,"%Y%m%d"),format(Cal.DateEND,"%Y%m%d"),
                                                          ifelse(any(namesCovariates != ""),paste(paste0(namesCovariates, paste0("-",degrees)),collapse = "&"),""), Weighted),"__"),".rdata"), collapse = ""))
                # Selecting windows slice date within DateCal
                Data.num <- ASE.ID$General.DT[DateCal,.SD, .SDcols = c("date", List.Columns)][date > Cal.DateIN & date <= Cal.DateEND + 1, which = TRUE]
                # remove inf, NA, NaN from Data.num
                Data.num <- Data.num[is.finite(rowSums(ASE.ID$General.DT[DateCal][Data.num, ..List.Columns]))]
                
                if (length(Data.num) > 10) {
                    if ((!file.exists(nameModel))) {
                        futile.logger::flog.info(paste0("Fitting: ", nameModel))
                        Fit_New_Model(ASEDir = ASEDir, DIR_Models = DIR_Models, General.DT = ASE.ID$General.DT[DateCal], name.sensor = name.sensor, ASE.cfg = ASE.ID$ASE.cfg, SetTime = ASE.ID$SetTime,
                                      Mod_type = Mod_type, Probs = Probs, namesCovariates = namesCovariates, 
                                      degrees = ifelse(length(degrees)>length(namesCovariates)||length(degrees)<length(namesCovariates), degrees[seq_along(namesCovariates)], degrees),
                                      Cal.DateIN  = Cal.DateIN , Cal.DateEND  = Cal.DateEND,
                                      Plot_Line = FALSE, PlotCal = FALSE, Verbose = FALSE, Weighted = Weighted, Sync = Sync,
                                      DateCal = Data.num)
                    } else {
                        futile.logger::flog.warn(paste0("Exists:  ", nameModel))
                        if (!exists("Models.Already")) Models.Already <- nameModel else Models.Already <- c(Models.Already,nameModel)
                    } 
                    if (!exists("List.Added.Models")) List.Added.Models <- nameModel else List.Added.Models <- c(List.Added.Models,nameModel)
                } else {
                    futile.logger::flog.warn(paste0("[Roll_Fit_New_Model] ASE box ", ASE.name,", sensor ", str_pad(name.sensor, width = max(str_length(ASE.ID$list.sensors),na.rm = T)), ", no data to fit ", basename(nameModel)))
                }
                Cal.DateIN  <- Cal.DateIN  + 1 
                Cal.DateEND <- Cal.DateEND + 1
                # Jump to the next available data if any
                if (!is.null(DateCal)) {
                    new.Cal.Interval <- lubridate::interval(Cal.DateIN, Cal.DateEND)
                    # test if there are any data left available
                    if (length(which(ASE.ID$General.DT[DateCal][["date"]] %within% new.Cal.Interval)) == 0) {
                        if (ASE.ID$General.DT[DateCal][date > as.Date(Cal.DateIN)[1], .N] > 0) {
                            Cal.DateIN <- as.Date(ASE.ID$General.DT[DateCal][["date"]][as.Date(ASE.ID$General.DT[DateCal][["date"]]) > as.Date(Cal.DateIN)][1])
                            Cal.DateEND <- Cal.DateIN + Cal.Interval
                        } else {
                            #End of Fitting , there are no more data from Cal.DateIN
                            Cal.DateEND <- Meas.DateEND + 1}}}
            }
            rm(Cal.Interval, Cal.DateIN, Cal.DateEND)}}
    if (!exists("Models.Already")) Models.Already <- character(0)
    if (!exists("List.Added.Models")) List.Added.Models <- character(0)
    return(list(List.Added.Models = List.Added.Models, Models.Already = Models.Already))
}
#' Compare performance of a list of calibration model for one sensor of one ASE box
#'
#' @param List.models a character vector with full file.paths of <- model files to be compared. Combine ASEDIR and output of function List_models() to create List.models.
#' @param ASE.ID object returned by Identify_ASE or Identifiy_ASE_Dir
#' @param General.DT Only used if ASE.ID is NULL. A data.table with all ASE Box and reference data. Default is NULL. If NULL, General.csv loaded using function Identify_ASE()
#' @param ASE.cfg Only used if ASE.ID is NULL. File path of the subdirectory of ASEDir where is the file General.csv. If NULL the ASE_cfg.cfg file is loaded using function Identify_ASE(). If General.DT is given, ASE.cfg shall be given as well.
#' @param SetTime Only used if ASE.ID is NULL. A data.table with all ASE box SetTime configuration. Default is null. If NULL the ASE_SETTIME.cfg file is loaded using function Identify_ASE().If General.DT is given, SetTime shall be given as well.
#' @param Config Only used if ASE.ID is NULL. A List, default is null, returned by function CONFIG(). If NULL, Config is loaded using function Identify_ASE(). If General.DT is given, Config shall be given as well.
#' @param Shield Only used if ASE.ID is NULL. A data.table, default is null, returned by function CONFIG(). If NULL, Shield is loaded using function Identify_ASE(). If General.DT is given, Shield shall be given as well.
#' @param DIR_Config character vector, default is "Configuration". It sets a sub-directory that shall exist at file.path(WD, Project, ASE) where is the file ASE.name.cfg, ...  are stored
#' @param DIR_General character vector, default is "General_data". It sets a sub-directory that shall exist at file.path(WD, Project, ASE) where is the file General.csv, .. are stored
#' @param DIR_Config Character vector giving the subdirectory of ASEDir where is the file ASE.name.cfg.
#' @param DIR_Models Character vector giving the subdirectory of ASEDir where are the calibration models are stored.
#' @param Val.Interval Not used for now. Default value is NULL. If not NULL, Val.Interval is added to the returned table
#' @param Exclude.Cal logical, default is TRUE. If TRUE data during calibration are excluded for prediction
#' @param DateCal default value NULL. If not, numeric vector giving the indexes of dates in ASE.ID$General.DT used for calibration
#' @return return a list with statistics of all models per gas.sensor + comparison of calibration and prediction data. In details:
#'  - Table.compare: a data.table with statistics of calibration, prediction and uncertainties
#'  - U.Pred: the returned data.table of function U_orth_DF()
#' @examples
#' CO.Compare  <- Compare_Models(file.path(ASEDir, "Models", List.models))
################################# CAREFUL, change code to rbindlist() U.Pred in case of several Models #####################################################################
Compare_Models <- function(List.models, ASE.ID = NULL, General.DT = NULL, ASE.cfg = NULL, SetTime = NULL, Config = NULL, Shield = NULL, 
                           Cal.DateIN = NULL, Cal.DateEND = NULL, Meas.DateIN = NULL, Meas.DateEND = NULL,
                           DIR_Config = "Configuration", DIR_Models = "Models", DIR_General = "General_data", Val.Interval = "NULL", Verbose = FALSE,
                           Exclude.Cal = TRUE, Sync.Cal = TRUE, Sync.Pred = TRUE, Simulation = F, ASE.name = NULL, DateCal = NULL) {
    # Identify ASE box charateristics from first Model if needed
    if (is.null(ASE.ID)){
        if (!is.null(General.DT)) {
            ASE.ID <- Identify_ASE(Model = List.models[1], General.DT = General.DT, ASE.cfg = ASE.cfg, SetTime = SetTime, Config = Config, Shield = Shield, ASE.name = ASE.name) 
        } else ASE.ID <- Identify_ASE(Model = List.models[1], ASE.name = strsplit(basename(List.models[1]), "__")[[1]][1])} 
    # Adding a row of statistics for each Model
    for (Model in List.models) {
        if (Verbose) futile.logger::flog.info(paste0("[Compare_Models] Model : ", basename(Model), ""))
        # Identify ASE box characteristics taking into consideration possible ASE box change
        if (ASE.ID$ASE.name == basename(dirname(dirname(Model))) || (grepl("Simulations", DIR_Models) && grepl(ASE.ID$ASE.name, Model))) {
            if (is.null(Config) && !is.null(ASE.ID) && "Config" %in% names(ASE.ID)) Config <- ASE.ID$Config
            if (is.null(Shield) && !is.null(ASE.ID) && "Config" %in% names(ASE.ID) && "sens2ref.shield" %in% names(ASE.ID$Config)) Shield <- ASE.ID$Config$sens2ref.shield
            ASE.ID <- Identify_ASE(Model = Model, General.DT = ASE.ID$General.DT, ASE.cfg = ASE.ID$ASE.cfg, SetTime = ASE.ID$SetTime, Config = Config, Shield = Shield, ASE.name = ASE.name)
            if(Verbose) futile.logger::flog.warn(paste0("[Compare_Models] General.csv for ",ASE.ID$ASE.name, " already loaded."))
        } else {
            ASE.ID <- Identify_ASE(Model = Model)
            if (exists("Cal.DateIN")) rm(Cal.DateIN)
            if (exists("Cal.DateEND")) rm(Cal.DateEND)
            if(Verbose) futile.logger::flog.info(paste0("[Compare_Models] General.csv for ",ASE.ID$ASE.name, " loaded."))
        } 
        # Calibration Model
        if (!exists("Cal.DateIN") || is.null(Cal.DateIN))  Cal.DateIN  <- ASE.ID$Cal.DateIN
        if (!exists("Cal.DateEND") || is.null(Cal.DateEND)) Cal.DateEND <- ASE.ID$Cal.DateEND
        Variables   <- ASE.ID$Variables
        Model.i     <- load_obj(Model)
        
        # Managing double Model of Yatkin and all others
        if (length(Model.i) == 2 || ASE.ID$Mod_type == "Yatkin") Models <- names(Model.i) else Models <- "Unique"
        # if (length(Model.i) == 2) Models <- names(Model.i) else Models <- "Unique"
        
        for (Sub.Model in Models){
            if (Sub.Model == "Unique") Model_i <- Model.i else Model_i <- Model.i[[Sub.Model]]
            
            R2raw       <- ifelse("r.squared" %in% names(Model_i$Glance), Model_i$Glance$r.squared, NA)
            AICraw      <- ifelse("AIC"       %in% names(Model_i$Glance), Model_i$Glance$AIC      , NA)
            BICraw      <- ifelse("BIC"       %in% names(Model_i$Glance), Model_i$Glance$BIC      , NA)
            # Fitting during Calibration period except for Yatkin (we don't call Apply_Model for model Yatkin)
            if(ASE.ID$Mod_type != "Yatkin") {
                # Starting by applying the calibration model
                #ASE.ID$General.DT <- ASE.ID$General.DT[as.Date(date) >= as.Date("2021-03-01") & as.Date(date) <= as.Date("2021-04-09")]
                ASE.ID$General.DT <- Apply_Model(Model = Model, General.DT = ASE.ID$General.DT, ASE.cfg = ASE.ID$ASE.cfg, Config =  ASE.ID$Config, Shield =  ASE.ID$Config$sens2ref.shield)
                # Delagging for all periods after applying model
                ASE.ID$General.DT <- Var_Delag_Pred(DT.General = ASE.ID$General.DT,  ColSens = ASE.ID$nameGasMod, ColSensRaw = ASE.ID$gas.sensor,
                                                ColRef = ASE.ID$nameGasRef, ColRefRaw = gsub("Out.", "", ASE.ID$nameGasRef), Sync.Pred = Sync.Pred, Sync.Cal = ifelse(Sync.Pred, FALSE, Sync.Cal),
                                                Meas.IN = ASE.ID$Meas.DateIN, Meas.END = ASE.ID$Meas.DateEND, Cal.IN = ASE.ID$Cal.DateIN, Cal.END = ASE.ID$Cal.DateEND)
                
                
                if (all(c(ASE.ID$nameGasRef,ASE.ID$nameGasMod) %in% names(ASE.ID$General.DT))) {
                    if(!is.null(DateCal)) {
                        if (data.table::is.data.table(DateCal)) DateCal <- ASE.ID$General.DT[date > DateCal[[1]] & date <= DateCal[[2]], which = TRUE]
                        Calibration.DT <- ASE.ID$General.DT[DateCal, .SD, .SDcols = c("date",ASE.ID$nameGasRef,ASE.ID$nameGasMod)]
                    }  else Calibration.DT <- ASE.ID$General.DT[date > Cal.DateIN & date <= Cal.DateEND + 1, .SD, .SDcols = c("date",ASE.ID$nameGasRef,ASE.ID$nameGasMod)]
                } else return(futile.logger::flog.error("[Compare_Models] sensor or reference data are missing!"))
                # No need to DeLag_Cal since we do a Var_Delag after applying the model
                if(!Sync.Pred) Calibration.DT <- DeLag_Cal(DT.General = Calibration.DT, ColRef = ASE.ID$nameGasRef, ColSens = ASE.ID$nameGasMod, DateIN = ASE.ID$Cal.DateIN, DateEND = ASE.ID$Cal.DateEND, 
                                            Sync = ASE.ID$Sync.Cal, ASE.name = ASE.ID$ASE.name, name.sensor = ASE.ID$name.sensor)
                Calibration.DT <- Calibration.DT[is.finite(rowSums(Calibration.DT[,c(ASE.ID$nameGasRef,ASE.ID$nameGasMod), with =F]))]
                N.Cal          <- Calibration.DT[,.N]
                if (N.Cal > 10) {
                    if(Verbose){
                        plot(Calibration.DT[, .SD, .SDcols = c("date", ASE.ID$nameGasMod)], type = "l", col = "blue", xaxt = "n",
                             main = paste0("[Compare_Models] Calibration ",basename(Model)), cex.main = 0.8)
                        r <- as.POSIXct(round(range(Calibration.DT$date), "day"))
                        graphics::axis.POSIXct(1, at = seq(r[1], r[2], by = "day"), format = "%m-%d")
                        lines(Calibration.DT[, .SD, .SDcols = c("date", ASE.ID$nameGasRef)], col = "red")
                        plot(Calibration.DT[, .SD, .SDcols = c(ASE.ID$nameGasRef, ASE.ID$nameGasMod)], type = "p", col = "blue", main = paste0("[Compare_Models] Calibration ",basename(Model)), cex.main = 0.8); grid()
                        Calibration <- Cal_Line(x = Calibration.DT[[ASE.ID$nameGasRef]], s_x = NULL,
                                                y = Calibration.DT[[ASE.ID$nameGasMod]], s_y = NULL,
                                                Mod_type      = ifelse(ASE.ID$eta.model.type == "Orthogonal", "TLS", ASE.ID$eta.model.type),
                                                Matrice       = NULL,
                                                Weighted      = FALSE,
                                                Auto.Lag      = ifelse(Sync.Cal, as.logical(unlist(ASE.ID$ASE.cfg[ASE.ID$ASE.cfg$name.gas == "Sync.Cal", ASE.ID$k, with = FALSE])), FALSE),
                                                Plot_Line     = TRUE, 
                                                Verbose       = Verbose)
                    } else {
                        Calibration <- Cal_Line(x = Calibration.DT[[ASE.ID$nameGasRef]], s_x = NULL,
                                                y = Calibration.DT[[ASE.ID$nameGasMod]], s_y = NULL,
                                                Mod_type      = ifelse(ASE.ID$eta.model.type == "Orthogonal", "TLS", ASE.ID$eta.model.type),
                                                Matrice       = NULL,
                                                Weighted      = FALSE,
                                                Auto.Lag      = ifelse(Sync.Cal, as.logical(unlist(ASE.ID$ASE.cfg[ASE.ID$ASE.cfg$name.gas == "Sync.Cal", ASE.ID$k, with = FALSE])), FALSE),
                                                Plot_Line     = FALSE, 
                                                Verbose       = Verbose)
                    }
                } else return(futile.logger::flog.error("[Compare_Models] less than 10 data for calibration, change Cal.DateIN and Cal.DateEND dates in config file."))
            }
            # Validation period
            if (is.null(Meas.DateIN))  Meas.DateIN  <- ASE.ID$Meas.DateIN
            if (is.null(Meas.DateEND)) Meas.DateEND <- ASE.ID$Meas.DateEND
            # Preparing Prediction.DT
            Prediction.DT <- ASE.ID$General.DT[, .SD, .SDcols = c("date",ASE.ID$nameGasRef, ASE.ID$nameGasMod, gsub("Out.", "", ASE.ID$nameGasRef), gsub("_modelled", "", ASE.ID$nameGasMod))]
            # Prediction.DT <- ASE.ID$General.DT[, .SD, .SDcols = c("date",ASE.ID$nameGasRef,ASE.ID$nameGasMod)]
            
            # discarding incomplete rows, there must be enough data otherwise impossible to calibrate
            Prediction.DT <- Prediction.DT[is.finite(rowSums(Prediction.DT[,names(Prediction.DT)[grep("date", names(Prediction.DT), invert = T)], with = F]))]
            
            # selecting only Prediction date, check if enough data
            if (Prediction.DT[date > Meas.DateIN & date <= Meas.DateEND + 1, .N] < 10) {
                futile.logger::flog.warn(("[Compare_Models] no data for prediction, using dates of calibration."))
                if (is.null(Meas.DateIN))  Meas.DateIN  <- Cal.DateIN
                if (is.null(Meas.DateEND)) Meas.DateEND <- Cal.DateEND} 
            Prediction.DT <- Prediction.DT[date > Meas.DateIN & date <= Meas.DateEND + 1]
            
            # discarding calibration data if requested and some data are remaining
            if (Exclude.Cal) {
                # if (is.null(DateCal)) {
                # checking if remaining data after excluding calibration
                if (Prediction.DT[!date %within% lubridate::interval(Cal.DateIN, Cal.DateEND + 1), .N] >= 120) {
                    Prediction.DT <- Prediction.DT[!date %within% lubridate::interval(Cal.DateIN, Cal.DateEND + 1)]
                } else futile.logger::flog.warn("[Compare_Models] not enough data if calibration period is excluded when predicting. Data for Calibration are kept for prediction as well")}
            # # No need to Var_Delag_Pred since we do a Var_Delag after applying the model
            # # Delag Prediction
            # Prediction.DT <- Var_Delag_Pred(DT.General = Prediction.DT,  ColSens = ASE.ID$nameGasMod, ColSensRaw = ASE.ID$gas.sensor,
            #                                 ColRef = ASE.ID$nameGasRef, ColRefRaw = gsub("Out.", "", ASE.ID$nameGasRef), Sync.Pred = Sync.Pred,
            #                             Meas.IN = ASE.ID$Meas.DateIN, Meas.END = ASE.ID$Meas.DateEND, Cal.IN = ASE.ID$Cal.DateIN, Cal.END = ASE.ID$Cal.DateEND)
            # Prediction.DT <- DeLag_Pred(DT.General = Prediction.DT, ColRef = ASE.ID$nameGasRef, ColSens = ASE.ID$nameGasMod, 
            #                             Meas.DateIN = ASE.ID$Meas.DateIN, Meas.DateEND = ASE.ID$Meas.DateEND, Cal.DateIN = ASE.ID$Cal.DateIN, Cal.DateEND = ASE.ID$Cal.DateEND, 
            #                             Sync.Pred = ASE.ID$Sync.Pred, Sync.Cal = ASE.ID$Sync.Cal, ASE.name = ASE.ID$ASE.name, name.sensor = ASE.ID$name.sensor)
            
            # remove NA due to delagging
            Prediction.DT <- Prediction.DT[is.finite(rowSums(Prediction.DT[, c(ASE.ID$nameGasRef,ASE.ID$nameGasMod), with = FALSE]))]
            
            # Averaging data for Prediction if needed and compute average only if the number of rows within "width" is at least Min.Perc =  75% * width
            if (ASE.ID$Config$Server$UserMinsAvg != ASE.ID$Config$Server$UserMins) {
                Prediction.DT <- DF_avg(Prediction.DT, 
                                        width = ASE.ID$Config$sens2ref[name.sensor==ASE.ID$name.sensor][["UserMinsAvg"]],
                                        hour_start = ASE.ID$Config$sens2ref[name.sensor==ASE.ID$name.sensor][["hour_start"]],
                                        Apply.Min.Perc = TRUE)}
            N.Predict <- Prediction.DT[,.N]
            if(Verbose) {
                plot(Prediction.DT[, .SD, .SDcols = c("date", ASE.ID$nameGasMod)], type = "l", col = "blue", xaxt = "n", main = paste0("[Compare_Models] Prediction ",basename(Model)), cex.main = 0.8)
                r <- as.POSIXct(round(range(Prediction.DT$date), "month"))
                graphics::axis.POSIXct(1, at = seq(r[1], r[2], by = "month"), format = "%Y-%m")
                lines(Prediction.DT[, .SD, .SDcols = c("date", ASE.ID$nameGasRef)], col = "red")
                plot(Prediction.DT[, .SD, .SDcols = c(ASE.ID$nameGasRef, ASE.ID$nameGasMod)], type = "p", col = "blue",
                     main = paste0("[Compare_Models] Prediction ",basename(Model)), cex.main = 0.8); grid()
                Prediction <- Cal_Line(x = Prediction.DT[[ASE.ID$nameGasRef]], s_x = NULL,
                                       y = Prediction.DT[[ASE.ID$nameGasMod]], s_y = NULL,
                                       Mod_type      = ifelse(ASE.ID$eta.model.type == "Orthogonal", "TLS", ASE.ID$eta.model.type),
                                       Matrice       = NULL, 
                                       Weighted      = FALSE,
                                       Auto.Lag      = ifelse(Sync.Pred, as.logical(unlist(ASE.ID$ASE.cfg[ASE.ID$ASE.cfg$name.gas == "Sync.Pred", ASE.ID$k, with = FALSE])), FALSE),
                                       Plot_Line     = TRUE, 
                                       Verbose       = FALSE)
            } else Prediction <- Cal_Line(x = Prediction.DT[[ASE.ID$nameGasRef]], s_x = NULL,
                                          y = Prediction.DT[[ASE.ID$nameGasMod]], s_y = NULL,
                                          Mod_type      = ifelse(ASE.ID$eta.model.type == "Orthogonal", "TLS", ASE.ID$eta.model.type),
                                          Matrice       = NULL, 
                                          Weighted      = FALSE,
                                          Auto.Lag      = ifelse(Sync.Pred, as.logical(unlist(ASE.ID$ASE.cfg[ASE.ID$ASE.cfg$name.gas == "Sync.Pred", ASE.ID$k, with = FALSE])), FALSE),
                                          Plot_Line     = FALSE, 
                                          Verbose       = FALSE)
            
            # returning part number of sensor
            if ("Sensors.cfg" %in% names(ASE.ID$Config)) {
                SN.Cal <- unique(ASE.ID$Config$Sensors.cfg[time <= Meas.DateEND & name == ASE.ID$name.sensor][["serial"]])
                if (length(SN.Cal) == 0) {
                    SN.Cal = "unknown"   
                } else if (length(SN.Cal) > 1) SN.Cal <- paste(SN.Cal[!is.na(SN.Cal)], collapse = ", ")
            } else SN.Cal = "unknown"
            if ("Sensors.cfg" %in% names(ASE.ID$Config)) {
                SN.Pred <- unique(ASE.ID$Config$Sensors.cfg[time <= Meas.DateEND & name == ASE.ID$name.sensor][["serial"]])
                if (length(SN.Pred) == 0) {
                    SN.Pred = "unknown"   
                } else if (length(SN.Pred) > 1) SN.Pred <- paste(SN.Pred[!is.na(SN.Pred)], collapse = ", ")
            } else SN.Pred = "unknown"
            
            if (exists('Calibration')) {
                cal.row     <- data.table::data.table(ASE.name = ASE.ID$ASE.name, name.sensor = ASE.ID$name.sensor, Unit = ASE.ID$ASESens.raw.unit, Mod_type = ASE.ID$Mod_type, 
                                                      Cal.DateIN = ASE.ID$Cal.DateIN, Cal.DateEND = ASE.ID$Cal.DateEND, Variables = Variables, 
                                                      R2raw = R2raw, AICraw = AICraw, BICraw = BICraw,
                                                      R2Cal = summary(Calibration)$r.squared, Intcal = Calibration$coefficients[1], SlopeCal = Calibration$coefficients[2],
                                                      RMSECal = sqrt(sum(Calibration$residuals^2)/(length(Calibration$residuals) - 2)), AICCal = AIC(Calibration), BICCal = BIC(Calibration), Calibration = Calibration$Equation,
                                                      Prediction.IN = Meas.DateIN, Prediction.END = Meas.DateEND,
                                                      R2Pred = summary(Prediction)$r.squared, IntPred = Prediction$coefficients[1], SlopePred = Prediction$coefficients[2],
                                                      RMSEPred = sqrt(sum(Prediction$residuals^2)/(length(Prediction$residuals) - 2)),
                                                      AICPred = AIC(Prediction), BICPred = BIC(Prediction), Prediction = Prediction$Equation, N.Cal = N.Cal, N.Predict = N.Predict, SN.Cal = SN.Cal, SN.Pred = SN.Pred)
                
                cal.row[, Range.ref.Cal := list(quantile(Calibration.DT[[ASE.ID$nameGasRef]], probs = c(0,0.5,0.90,0.98,1)))]
                cal.row[, Range.U.Cal := list(2*sqrt(cal.row$RMSECal^2 + (cal.row$Intcal + (cal.row$SlopeCal - 1) * cal.row$Range.ref.Cal[[1]])^2))]
            } else {
                
                cal.row     <- data.table::data.table(ASE.name = ASE.ID$ASE.name, name.sensor = ASE.ID$name.sensor, Unit = ASE.ID$ASESens.raw.unit, Mod_type = ASE.ID$Mod_type, 
                                                      Cal.DateIN = ASE.ID$Cal.DateIN, Cal.DateEND = ASE.ID$Cal.DateEND, Variables = Variables, 
                                                      R2raw = R2raw, AICraw = AICraw, BICraw = BICraw,
                                                      Prediction.IN = Meas.DateIN, Prediction.END = Meas.DateEND,
                                                      R2Pred = summary(Prediction)$r.squared, IntPred = Prediction$coefficients[1], SlopePred = Prediction$coefficients[2],
                                                      RMSEPred = sqrt(sum(Prediction$residuals^2)/(length(Prediction$residuals) - 2)),
                                                      AICPred = AIC(Prediction), BICPred = BIC(Prediction), Prediction = Prediction$Equation, N.Predict = N.Predict, SN.Pred = SN.Pred)
                
                # cal.row[, Range.ref.Cal := NA]
                # cal.row[, Range.U.Cal := NA]
                # 
            }
            # Getting LV
            DQO <- get.DQO(name.gas = ASE.ID$name.gas, unit.ref = ASE.ID$unit.ref)
            # Adding range x and y and uncertainty
            cal.row[, Range.ref.Pred := list(c(quantile(Prediction.DT[[ASE.ID$nameGasRef]], probs = c(0,0.5,0.90,0.98,1)), DQO$LV))]
            cal.row[, Range.Pred := list(quantile(Prediction.DT[[ASE.ID$nameGasMod]], probs = c(0,0.5,0.90,0.98,1)))]
            
            # Compute uncertainty with mtd method
            #Creating the data set with ref > 0
            data4U <- data.table::data.table(1:Prediction.DT[,.N],
                                             date = Prediction.DT$date,
                                             xis = Prediction.DT[[ASE.ID$nameGasRef]],
                                             yis = Prediction.DT[[ASE.ID$nameGasMod]])
            data4U[,ubsRM := UbsRM_Expert(ASE.ID$name.sensor)$Slope.UbsRM * data4U$xis + UbsRM_Expert(ASE.ID$name.sensor)$Intercep.UbsRM]
            data4U[,ubss  := rep(Ubss(name.sensor= ASE.ID$name.sensor), times = Prediction.DT[,.N])]
            data4U <- data4U[xis>0]
            
            U.Pred <- U_orth_DF(
                Mat              = data4U,
                #ubsRM           = if (ASE.ID$name.sensor %in% c("NO_B4_P1", "NO2_B43F_P1", "OX_A431_P1", "CO_A4_P1")) 0.01 * data4U$xis else 0.02 * data4U$xis,
                variable.ubsRM   = TRUE, 
                #perc.ubsRM      = if (ASE.ID$name.sensor %in% c("NO_B4_P1", "NO2_B43F_P1", "OX_A431_P1", "CO_A4_P1")) 0.01 else 0.02,
                # ubss           = Ubss(name.sensor = ASE.ID$name.sensor),
                Add.ubss         = FALSE,
                variable.ubss    = FALSE,
                Fitted.RS        = TRUE,
                Forced.Fitted.RS = FALSE,
                Regression       = ifelse(ASE.ID$eta.model.type == "Linear", "OLS", ASE.ID$eta.model.type),
                Verbose          = Verbose)
            
            # Checking if the reference range covers the LV, interpolating U, exactly at the LV using 5 xis < LV and at max 5 xis > LV
            if (DQO$LV < max(Prediction.DT[[ASE.ID$nameGasRef]])) {
                # Calculate the distance from the LV for each xis
                # Sort distances and get the original indexes
                # Filter out only those indexes where the values are lower or higher than the LV
                # Select the first 5 indexes from each
                distances        <- abs(U.Pred$Mat$xis - DQO$LV)
                sorted.indexes   <- order(distances)
                closest.lower.5  <- head(sorted.indexes[U.Pred$Mat$xis[sorted.indexes] <= DQO$LV],  min(c(5, length(sorted.indexes[U.Pred$Mat$xis[sorted.indexes] <= DQO$LV])), na.rm = T))
                closest.higher.5 <- head(sorted.indexes[U.Pred$Mat$xis[sorted.indexes] >= DQO$LV], min(c(5, length(sorted.indexes[U.Pred$Mat$xis[sorted.indexes] >= DQO$LV])),na.rm = T))
                closest.5        <- c(closest.lower.5, closest.higher.5)
                
                # adding percentiles of U including U(LV) by interpolation
                cal.row[, Range.U.Pred := list(c(quantile(U.Pred$Mat$U, probs = c(0,0.5,0.90,0.98,1)),
                                                 approx(U.Pred$Mat$xis[closest.5], U.Pred$Mat$U[closest.5], method = "linear", xout = DQO$LV, ties = "ordered")[[2]]))]
            } else {
                # adding percentiles of U and U(LV) by estimation from rmse, UbsRM and bias
                # Checking if the uncertainty calculation returns a NaN
                if ((U.Pred$rmse^2 - (UbsRM_Expert(ASE.ID$name.sensor)$Slope.UbsRM * DQO$LV + 
                                      UbsRM_Expert(ASE.ID$name.sensor)$Intercep.UbsRM)^2 + (U.Pred$b0 + (U.Pred$b1 - 1) * DQO$LV)^2) > 0) 
                    cal.row[, Range.U.Pred := list(c(quantile(U.Pred$Mat$U, probs = c(0,0.5,0.90,0.98,1)), 
                                                     2*sqrt(U.Pred$rmse^2 - (UbsRM_Expert(ASE.ID$name.sensor)$Slope.UbsRM * DQO$LV +
                                                                                 UbsRM_Expert(ASE.ID$name.sensor)$Intercep.UbsRM)^2 + (U.Pred$b0 + (U.Pred$b1 - 1) * DQO$LV)^2)))]
                else{
                    # UbsRM removed  
                    cal.row[, Range.U.Pred := list(c(quantile(U.Pred$Mat$U, probs = c(0,0.5,0.90,0.98,1)), 
                                                     2*sqrt(U.Pred$rmse^2 + (U.Pred$b0 + (U.Pred$b1 - 1) * DQO$LV)^2)))] # UbsRM removed  
                } 
            }
            rm(data4U)
            Number.Columns <- length(names(cal.row))
            # matrix is the case of model rqs
            if (any(grepl("matrix",class(Model_i$Coef)))) {
                for (tau in 1:ncol(Model_i$Coef)) {
                    cal.row[,paste0(row.names(Model_i$Coef)[1], "_", colnames(Model_i$Coef)[tau]) := Model_i$Coef[1,tau]]
                    cal.row[, paste0("std.error.", row.names(Model_i$Coef)[1], "_", colnames(Model_i$Coef)[tau]) := Model_i$Tidy[Model_i$Tidy$term == row.names(Model_i$Coef)[1],"std.error"][tau,]]
                    cal.row[,paste0(row.names(Model_i$Coef)[2], "_", colnames(Model_i$Coef)[tau]) := Model_i$Coef[2,tau]]
                    cal.row[, paste0("std.error.", row.names(Model_i$Coef)[2], "_", colnames(Model_i$Coef)[tau]) := Model_i$Tidy[Model_i$Tidy$term == row.names(Model_i$Coef)[2],"std.error"][tau,]]
                }
            } else if (any(grepl("dgCMatrix",class(Model_i$Coef)))) {
                Cal.Coefficients <- Model_i$Coef[,1]
                for (Coeffs in 1:length(Cal.Coefficients)) {
                    cal.row[,(names(Cal.Coefficients)[Coeffs]) := Cal.Coefficients[Coeffs]]
                    cal.row[,(paste0("std.error.",names(Model_i$Coef)[Coeffs])) := Model_i$Tidy$std.error[Coeffs]]
                }
            } else {
                for (Coeffs in seq_along(Model_i$Coef)) {
                    cal.row[,(names(Model_i$Coef)[Coeffs]) := Model_i$Coef[Coeffs]]
                    cal.row[,(paste0("std.error.",names(Model_i$Coef)[Coeffs])) := Model_i$Tidy$std.error[Coeffs]]
                } 
            }
            if (exists("cal.row")) {
                if (exists("Table.compare")) Table.compare <- data.table::rbindlist(list(Table.compare,cal.row), use.names = T, fill = T) else Table.compare <- cal.row
                rm(cal.row)}
            Number.Columns <- length(names(Table.compare))
            if (any(grepl("matrix",class(Model_i$Coef)))) {
                Number.Columns.Coeffs  <- unlist(lapply(row.names(Model_i$Coef), function(i) intersect(grep(i, names(Table.compare)), grep(paste(c("std.error", "Interval"), collapse = "|"), names(Table.compare), invert =T))))
            } else {
                Number.Columns.Coeffs  <- sapply(names(Model_i$Coef), function(i) which(names(Table.compare) %in% i))
            }
            setcolorder(Table.compare, c(1:7,Number.Columns.Coeffs,setdiff(1:Number.Columns, c(1:7,Number.Columns.Coeffs))))
            # if (!shiny::isTruthy(Val.Interval)) Table.compare$Interval <- Val.Interval else {
            # if (!is.null(Val.Interval)) Table.compare$Interval <- Val.Interval else {
            #     if (grepl("__Median", Model)) browser() #Table.compare$Interval <- 
            # }
        }
    }
    return(list(Table.compare = Table.compare, U.Pred = U.Pred))
}
#' Confidence interval of coefficents of calibration Models for sensors and Model type
#'
#' @param All.Compare A data.table for all ASE_Boxes and sensors with comparison data of all calibration models per sensor. 
#' List_All_Compare shall be the output of function List_All_Compare().
#' @param Mod_type character vector, default is "Linear.Robust", the model type to be fitted, e.g. "Linear.Robust"
#' @return A data.table with medians of Sintercept, Sintercept and slope, x ? Sx,  one row per sensor of the ASE boxes
#' @examples
#' Confidence_Coeffs(All.Compare)
Confidence_Coeffs <- function(All.Compare, Mod_type = "Linear.Robust") {
    if ("Table.Coeffs" %in% ls(pos = ".GlobalEnv")) rm(Table.Coeffs, pos = ".GlobalEnv")
    for (name.sensor in unique(All.Compare$name.sensor)) {
        Coeffs      <- data.table::data.table(name.sensor = name.sensor)
        Co_variates <- names(All.Compare)[(which(names(All.Compare) == "Variables") + 1) : (which(names(All.Compare) == "R2raw") - 1)]
        for (i in Co_variates) {
            
            # Adding St.error, MAD and Weighted base on st.error of covariates
            name.Wi     <- paste0("wi.",i)
            name.i.Wi   <- paste0(i,".",name.Wi)
            Wi.values   <- 1/All.Compare[[paste0("std.error.", i)]]^2
            #Wi.values   <- 1/All.Compare[["RMSECal"]]^2
            All.Compare[, (name.Wi)   := eval(Wi.values)]
            i.Wi.values <- All.Compare[[name.Wi]] * All.Compare[[i]]
            All.Compare[, (name.i.Wi) := eval(i.Wi.values)]
            Coeffs[,(i):= median(All.Compare[[i]][All.Compare$name.sensor == name.sensor & All.Compare$Mod_type == Mod_type])] 
            Coeffs[,(paste0("std.error.",i)):= sd(All.Compare[[i]][All.Compare$name.sensor == name.sensor & All.Compare$Mod_type == Mod_type])]
            Coeffs[,(paste0("Mad.",i)):= mad(All.Compare[[i]][All.Compare$name.sensor == name.sensor & All.Compare$Mod_type == Mod_type], na.rm = T)] 
            Coeffs[,(paste0("Weighted.",i)):= sum(All.Compare[[name.i.Wi]][All.Compare$name.sensor == name.sensor & All.Compare$Mod_type == Mod_type], na.rm = T)/
                       sum(All.Compare[[name.Wi]][All.Compare$name.sensor == name.sensor & All.Compare$Mod_type == Mod_type], na.rm = T)]
            
            # Adding weighting based on RMSE.cal
            All.Compare[, wi.RMSECal   := 1/All.Compare[["RMSECal"]]^2]
            All.Compare[, (paste0(i,".","wi.RMSECal")) := All.Compare[["wi.RMSECal"]] * All.Compare[[i]]]
            Coeffs[,(paste0("Weighted.RMSECal.",i)):= sum(All.Compare[[paste0(i,".","wi.RMSECal")]][All.Compare$name.sensor == name.sensor & All.Compare$Mod_type == Mod_type], na.rm = T)/
                       sum(All.Compare[["wi.RMSECal"]][All.Compare$name.sensor == name.sensor & All.Compare$Mod_type == Mod_type], na.rm = T)]
            
            # Adding Algorithm A if the range of estimate is sufficient
            if (FALSE && diff(range(All.Compare[[i]])) > 0) {
                Coeffs[,(paste0("AlgA",i)):= metRology::algA(All.Compare[[i]], na.rm = T)[[1]]]
                #Coeffs[,(paste0("AlgS",i)):= metRology::algS(s = All.Compare[[paste0("std.error.",i)]], degfree = All.Compare[,N.Cal], na.rm = T)]
            }
            # Model with smallest U.Pred for percentiles 2 and 3: 50 and 90 %
            if (shiny::isTruthy(All.Compare$Range.U.Cal)) {
                Min.U.Cal  <- which.min(colSums(data.table::rbindlist(list(All.Compare$Range.U.Cal))[2:3]))
                Coeffs[,(paste0("min.U.Cal.",i))          := All.Compare[Min.U.Cal][[i]]]}
            #    Coeffs[,(paste0("min.U.Cal.std.error.",i)):= All.Compare[Min.U.Cal][[paste0("std.error.",i)]]]
            Min.U.Pred <- which.min(colSums(data.table::rbindlist(list(All.Compare$Range.U.Pred))[2:3]))
            Coeffs[,(paste0("min.U.Pred.",i))          := All.Compare[Min.U.Pred][[i]]]
            #    Coeffs[,(paste0("min.U.Pred.std.error.",i)):= All.Compare[Min.U.Pred][[paste0("std.error.",i)]]]
        }
        
        if (!exists("Table.Coeffs")) Table.Coeffs <- Coeffs else Table.Coeffs <- data.table::rbindlist(list(Table.Coeffs,Coeffs), use.names = T, fill = T)
    }
    return(Table.Coeffs)}

#' Comparing all models of selected boxes in a vector of filepaths (ASEDir), saving and returning the comparisons
#'
#' @param ASEDir A character vector with the list of filepaths of ASE boxes to be submitted to the function List.All.Compare
#' @param name.sensor Character vector, character vector with the name(s) of sensors to compare models. Default is "ALL" that compare the models for all sensors.
#' @param DIR_Models Character vector, subdirectory of ASEDir where are the calibration models.
#' @param All.Models Character vector, all file paths of model to be compared. Default is NULL. If NULL all existing models per sensor are compared.
#' @param DateCal default value NULL. If not, numeric vector giving the indexes of dates in ASE.ID$General.DT used for calibration
#' @return A list of data.tables, one per sensor of the ASE boxes with comparison data of all models per sensor
#' @examples
#' List.All.Compare(ASEDir)
List_All_Compare <- function(ASEDir, ASE.ID = NULL, name.sensors = "ALL", DIR_Models = "Models", All.Models = NULL, Save = FALSE, Verbose = FALSE,
                             DateIN = NULL, DateEND = NULL, Meas.DateIN = DateIN, Meas.DateEND = DateEND, Sync.Cal = TRUE, Sync.Pred = TRUE, Exclude.Cal = TRUE,
                             DateCal = NULL) {
    if (exists("All.Compare.ASEDirs")) rm(All.Compare.ASEDirs)
    for (ASEDir in ASEDir) {
        if (is.null(All.Models)) All.Models  <- List_models(ASEDir, name.sensors)
        if (is.null(ASE.ID)) ASE.ID <- Identify_ASE_Dir(ASEDir = ASEDir)
        All.Compare <- lapply(All.Models, function(i) {
            if(grepl("2025-03-16", i)) browser()
            Compare_Models(file.path(ASEDir, DIR_Models, i), DIR_Models = DIR_Models, ASE.ID = ASE.ID, 
                           General.DT = ASE.ID$General.DT, ASE.cfg = ASE.ID$ASE.cfg, SetTime = ASE.ID$SetTime, Verbose = Verbose,
                           Cal.DateIN = DateIN, Cal.DateEND = DateEND, Meas.DateIN = Meas.DateIN, Meas.DateEND = Meas.DateEND,
                           Config = ASE.ID$Config, Shield = ASE.ID$Config$sens2ref.shield, Sync.Cal = Sync.Cal, Sync.Pred = Sync.Pred, Exclude.Cal = Exclude.Cal,
                           DateCal = DateCal)$Table.compare}) 
        All.Compare <- data.table::rbindlist(All.Compare[which(sapply(1:length(All.Compare), function(i) class(All.Compare[[i]])[1]=="data.table"))], use.names = T, fill = T)
        if (Save) data.table::fwrite(All.Compare,file.path(ASEDir, DIR_Models, "All_compare.rdata"))
        if (!exists("All.Compare.ASEDirs")) All.Compare.ASEDirs <- All.Compare else All.Compare.ASEDirs <- c(All.Compare.ASEDirs,All.Compare)
    }
    return(All.Compare.ASEDirs)
}
#' Measuring function
#'
#' @param Model full filepath and name of model of the calibration model.
Meas_Function_complete <- function(Model, ASE.ID, Shiny = F) {
    if (is.null(ASE.ID)) ASE.ID <- Identify_ASE(Model = Model)
    # Preparing the matrix of covariates
    # Removing na for nameGasMod for nameGasVolt missing
    is.not.NA.y <- which(!is.na(ASE.ID$General.DT[[ASE.ID$nameGasVolt]]))
    is.NA.y     <- which( is.na(ASE.ID$General.DT[[ASE.ID$nameGasVolt]]))
    CovMod <- ifelse(is.null(covariates), "Out.Temperature", covariates)
    covariates <- ASE.ID$Covariates
    if (exists(covariates) && length(covariates) > 0  ) 
        if (ASE.ID$Mod_type == "MultiLinear") {
            CovMod  <- unlist(strsplit(x = unlist(strsplit(x = sub(pattern = paste(c(".rds",".rdata"), collapse = "|"), replacement = "", x = Model), split = "__"))[7], split = "&", fixed = T))
            # Checking if there are "-" in the CovMod, deleting degrees of polynomial
            if (any(grepl(pattern = "-", x = CovMod[1]))) {
                Model.CovMod  <- unlist(strsplit(x = CovMod , split = "-"))
                CovMod  <- Model.CovMod[ seq(from = 1, to = length(Model.CovMod), by = 2) ]
                Degrees <- Model.CovMod[ seq(from = 2, to = length(Model.CovMod), by = 2) ]
            }
            # checking that all CovMod are included in ASE.ID$General.DT
            if (!all(CovMod %in% names(ASE.ID$General.DT))) {
                my_message <- paste0("[ASE_App, Cal$Forced] ERROR, not all Covariates are available, something missing in ", CovMod,
                                     ". The app  may crash.\n")
                cat(my_message)
                if (Shiny) shinyalert(title = "ERROR missing covariates",
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
                # take only the one that is nor NA of y = ASE.ID$General.DT[!is.na(ASE.ID$General.DT[, nameGasVolt]), nameGasVolt]
                is.not.NA.y <- which(is.finite(rowSums(ASE.ID$General.DT[, .SD, .SDcols = c(nameGasVolt,CovMod)])))
                is.NA.y     <- setdiff(1:nrow(ASE.ID$General.DT), is.not.NA.y)
                Matrice <- data.frame(ASE.ID$General.DT[is.not.NA.y, CovMod, with = FALSE],
                                      row.names = row.names(ASE.ID$General.DT[is.not.NA.y,]),
                                      stringsAsFactors = FALSE)
                names(Matrice) <- CovMod}
        } else if (ASE.ID$Mod_type %in% c("Peaks_baseline","exp_kT_NoC")) {
            # take only the one that is nor NA of y = ASE.ID$General.DT[!is.na(ASE.ID$General.DT[, nameGasVolt]), nameGasVolt]
            is.not.NA.y <- which(is.finite(rowSums(ASE.ID$General.DT[, .SD, .SDcols = c(nameGasVolt, ASE.ID$Covariates)])))
            is.NA.y     <- setdiff(1:nrow(ASE.ID$General.DT), is.not.NA.y)
            Matrice <- data.frame(ASE.ID$General.DT[is.not.NA.y, Temperature],
                                  row.names = row.names(ASE.ID$General.DT[is.not.NA.y,]),
                                  stringsAsFactors = FALSE)
            names(Matrice) <- ASE.ID$Covariates
        }  else if (ASE.ID$Mod_type %in% c("exp_kT","exp_kK","T_power", "K_power")){
            # take only the one that is nor NA of y = ASE.ID$General.DT[!is.na(ASE.ID$General.DT[, nameGasVolt]), nameGasVolt]
            is.not.NA.y <- which(is.finite(rowSums(ASE.ID$General.DT[, .SD, .SDcols = c(nameGasVolt, "Out.Temperature")])))
            is.NA.y     <- setdiff(1:nrow(ASE.ID$General.DT), is.not.NA.y)
            Matrice <- data.frame(ASE.ID$General.DT[is.not.NA.y, Temperature],
                                  row.names = row.names(ASE.ID$General.DT[is.not.NA.y,]),
                                  stringsAsFactors = FALSE)
            names(Matrice) <- "Out.Temperature"
        } else if (ASE.ID$Mod_type %in% c("BeerLambert")) {
            # take only the one that is nor NA of y = ASE.ID$General.DT[!is.na(ASE.ID$General.DT[, nameGasVolt]), nameGasVolt]
            is.not.NA.y <- which(is.finite(rowSums(ASE.ID$General.DT[, .SD, .SDcols = c(nameGasVolt, "Out.Temperature", "Out.Atmospheric_pressure")])))
            is.NA.y     <- setdiff(1:nrow(ASE.ID$General.DT), is.not.NA.y)
            Matrice <- data.frame(ASE.ID$General.DT[is.not.NA.y, c("Out.Temperature", "Out.Atmospheric_pressure")],
                                  row.names = row.names(ASE.ID$General.DT[is.not.NA.y,]),
                                  stringsAsFactors = FALSE)
            names(Matrice) <- c("Out.Temperature", "Out.Atmospheric_pressure")
        } else if (ASE.ID$Mod_type %in% c("Kohler", "Kohler_modified", "Kohler_lit", "Kohler_only")) {
            # take only the one that is nor NA of y = ASE.ID$General.DT[!is.na(ASE.ID$General.DT[, nameGasVolt]), nameGasVolt]
            is.not.NA.y <- which(is.finite(rowSums(ASE.ID$General.DT[, .SD, .SDcols = c(nameGasVolt, "Out.Relative_humidity")])))
            is.NA.y     <- setdiff(1:nrow(ASE.ID$General.DT), is.not.NA.y)
            Matrice <- data.frame(ASE.ID$General.DT[is.not.NA.y, c("Out.Relative_humidity")],
                                  row.names = row.names(ASE.ID$General.DT[is.not.NA.y,]),
                                  stringsAsFactors = FALSE)
            names(Matrice) <- c("Out.Relative_humidity")
        } else {
            Matrice <- NULL
        }
    # Using the reverse calibration function (measuring function) to extrapolate calibration
    if (ASE.ID$Mod_type != "MultiLinear" || (ASE.ID$Mod_type == "MultiLinear" && all(CovMod %in% names(ASE.ID$General.DT)))) {
        data.table::set(ASE.ID$General.DT, i = is.not.NA.y, j = nameGasMod,
                        value = list(Meas_Function(y          = ASE.ID$General.DT[[nameGasVolt]][is.not.NA.y],
                                                   Mod_type   = ASE.ID$Mod_type ,
                                                   covariates = CovMod,
                                                   Degrees    = Degrees,
                                                   Model      = Model_i,
                                                   Matrice    = Matrice)))
        # Removing na for nameGasMod either nameGasVolt missing or CovMod missing
        data.table::set(ASE.ID$General.DT, i = is.NA.y, j = nameGasMod, value = list(rep(NA, times = length(is.NA.y))))
        # setting negative values to NA
        if (input[[paste0("Neg.mod",k)]]) {
            data.table::set(ASE.ID$General.DT, i = which(ASE.ID$General.DT[, nameGasMod, with = FALSE] < 0), j = nameGasMod,
                            value = list(rep(NA, times = length(which(ASE.ID$General.DT[, nameGasMod, with = FALSE] < 0)))))
        }
    }
}

#' Set calibration model from the median of coefficients in Table.Coeffs for one ASE box
#'
#' @param ASEDir A character vector with the list of all filepaths of ASE boxes to be submitted to the function Median_Model
#' @param name.sensors Character vector, with the sensors names for which rolling models are going to be fitted. Default is NULL, in this case models are fitted for all sensors
#' @param Table.Coeffs A data.table with medians of intercept ... Sintercept and slope, x ? Sx,  one row per sensor of the ASE boxes. Output of Confidence_Coeffs().
#' @param Mod_type character vector, default is "Linear.Robust", the model type to be fitted, e.g. "Linear.Robust"
#' @param DIR_Models Character vector, subdirectory of ASEDir where are the calibration models.
#' @param All.Compare output of function List_All_Compare used to create the Median Model.
#' @param List.Models vector of strings, corresponding to the file paths of the models used as input of function List_All_Compare.
#' @param Interval numeric the windows size in days of the the rolling calibration models, default is 5 days
#' @param Robust.coeff character vector, default is "Weighted.". Method of selecting the best coefficients of models. It can be median, Weighed, AlgA or Min.U.Pred., min.U.Cal.
#' Possible values are  Weighted., Median, AlgA and min.U.
#' @return A data.table with medians of intercept and slope, x ? Sx,  one row per sensor of the ASE boxes
#' @examples
#' Median_Model(ASEDir, name.sensors = NULL, Table.Coeffs) 
Median_Model <- function(ASEDir, ASE.ID = NULL, name.sensors = NULL, Table.Coeffs, Mod_type = "Linear.Robust", 
                         DIR_Models = "Models", All.Compare, List.Models, Interval = 5, DateCal = NULL, Robust.coeff = "Weighted", Weighted = FALSE, ...) {
    # Identify ASE base on ASEDir
    if (is.null(ASE.ID)) ASE.ID   <- Identify_ASE_Dir(ASEDir = ASEDir, name.sensor = name.sensors[1])
    # Determining names of sensors in ASE box if name.sensors is NULL
    if (is.null(name.sensors)) name.sensors <- intersect(ASE.ID$list.sensors,Table.Coeffs$name.sensor)
    # Fitting model for set of name.sensors
    for (sensor in name.sensors) {
        # create/save the model with median of coefficients
        if (is.null(ASE.ID)) ASE.ID <- Identify_ASE_Dir(ASEDir = ASEDir, name.sensor = sensor, General.DT = ASE.ID$General.DT, 
                                                        ASE.cfg = ASE.ID$ASE.cfg, SetTime = ASE.ID$SetTime, Config = ASE.ID$Config, Shield = ASE.ID$Config$sens2ref.shield)
        Cal.DateIN   <- min(All.Compare[name.sensor == sensor][["Cal.DateIN"]] , na.rm = T)
        Cal.DateEND  <- max(All.Compare[name.sensor == sensor][["Cal.DateEND"]], na.rm = T)
        futile.logger::flog.info(paste0("[Median_Model] ASE box ", ASE.ID$ASE.name,", sensor ", str_pad(sensor, width = max(str_length(ASE.ID$list.sensors),na.rm = T)),
                                        ", creating rolling ", Mod_type, " models between ", Cal.DateIN, "\nand ", Cal.DateEND," with median of coefficients of models."))
        # clearing namesCovariates (split for & and set degrees)
        namesCovariates <- unique(All.Compare[name.sensor == sensor][["Variables"]])[1]
        if (grepl("&", namesCovariates)) namesCovariates <- strsplit(namesCovariates, "&")[[1]]
        if (any(grepl("-", namesCovariates))) {
            degrees         <- sapply(namesCovariates, function(i) strsplit(i, "-")[[1]][2], USE.NAMES = F)
            namesCovariates <- sapply(namesCovariates, function(i) strsplit(i, "-")[[1]][1], USE.NAMES = F)
        } else {
            degrees         <- rep("1",length(namesCovariates))}
        
        # Initial Calibration model on the whole time interval
        Best.model <- List.Models$List.Added.Models[which.min(All.Compare$RMSEPred)]
        Cal.IN.Best.model <- as.Date(strsplit(Best.model, "__")[[1]][5], format = "%Y%m%d")
        Cal.END.Best.model <- as.Date(strsplit(Best.model, "__")[[1]][6], format = "%Y%m%d")
        Median.Model     <- Fit_New_Model(ASEDir = ASEDir, General.DT = ASE.ID$General.DT[date >= Cal.DateIN & date< Cal.DateEND],name.sensor = sensor, Mod_type = Mod_type,
                                          Cal.DateIN  = Cal.IN.Best.model, Cal.DateEND = Cal.END.Best.model,
                                          namesCovariates = namesCovariates, 
                                          degrees = ifelse(length(degrees) > length(namesCovariates) || length(degrees)<length(namesCovariates), degrees[seq_along(namesCovariates)], degrees), Weighted = Weighted,
                                          Plot_Line = FALSE, PlotCal = FALSE, Verbose = FALSE, Include.Model = TRUE, SAVE = FALSE)
        # Saving initial parameters coefficients for later correction of equation
        Old.Coeffs    <- format(Median.Model$Coef , digits = 4, scientific = T)
        Median.Model$Augment <- na.omit(Median.Model$Augment)
        N             <- nrow(Median.Model$Augment)
        SSres         <- sum((Median.Model$Augment$.resid)^2)
        SSTot         <- sum((Median.Model$Augment$y - mean(Median.Model$Augment$y))^2)
        if (SSres > SSTot) futile.logger::flog.warn("[Median model] Something is wrong with the model: SSres > SSTot")
        RMSE          <- sqrt(SSres/(N - length(Median.Model$Coef) - 1))
        r.squared     <- rsquare(true = Median.Model$Augment$y, predicted = Median.Model$Augment$.fitted)
        adj.r.squared <- r.squared * (N - length(Median.Model$Coef) - 1) /(N - 1)
        
        # Tidy: Correcting Calibration model with rolling models
        Row           <- which(Table.Coeffs$name.sensor == sensor)
        Co_variates   <- names(All.Compare)[(which(names(All.Compare) == "Variables") + 1) : (which(names(All.Compare) == "R2raw") - 1)]
        if (Mod_type == "Linear.Robust.rqs") {
            for (j in Co_variates) {
                tau <- as.numeric(strsplit(j, " ")[[1]][2])
                if (grepl("Intercept", j)) {
                    Tidy.Rows <- which(Median.Model$Tidy$term == "(Intercept)" & format(round(Median.Model$Tidy$tau, digits=2), nsmall = 2) == format(round(tau, digits=2), nsmall = 2))
                    Median.Model$Coef[1,strsplit(j, "_")[[1]][2]] <- Table.Coeffs[[paste0(Robust.coeff,j)]][Row]
                } else {
                    Tidy.Rows <- which(Median.Model$Tidy$term == "x" & format(round(Median.Model$Tidy$tau, digits=2), nsmall = 2) == format(round(tau, digits=2), nsmall = 2))
                    Median.Model$Coef[2,strsplit(j, "_")[[1]][2]] <- Table.Coeffs[[paste0(Robust.coeff,j)]][Row]
                }
                Median.Model$Tidy[Tidy.Rows, "estimate"]  <- Table.Coeffs[[paste0(Robust.coeff,j)]][Row]
                Median.Model$Tidy[Tidy.Rows, "MAD"]       <- Table.Coeffs[[paste0("Mad.",j)]][Row]
                Median.Model$Tidy[Tidy.Rows, "conf.low"]  <- Table.Coeffs[[j]][Row] - ifelse(!is.na(Table.Coeffs[[paste0("Mad.",j)]][Row]), 1.4826 * Table.Coeffs[[paste0("Mad.",j)]][Row],NA)
                Median.Model$Tidy[Tidy.Rows, "conf.high"] <- Table.Coeffs[[j]][Row] + ifelse(!is.na(Table.Coeffs[[paste0("Mad.",j)]][Row]), 1.4826 * Table.Coeffs[[paste0("Mad.",j)]][Row],NA)
                Median.Model$Tidy[Tidy.Rows, "std.error"] <- Table.Coeffs[[paste0("std.error.",j)]][Row]
                Median.Model$Tidy[Tidy.Rows, "t value"]   <- Median.Model$Tidy[Tidy.Rows, "estimate"]/Median.Model$Tidy[Tidy.Rows, "std.error"]
                #https://stackoverflow.com/questions/46186115/calculating-p-values-for-given-t-value-in-r
                Median.Model$Tidy[Tidy.Rows, "Pr(>|t|)"]  <- 2*pt(q = abs(Median.Model$Tidy[["t value"]][Tidy.Rows]), df = nrow(All.Compare) - 2, lower = FALSE)}
        } else {
            for (j in Co_variates) {
                Median.Model$Tidy[Median.Model$Tidy$term == j, "estimate"]  <- Table.Coeffs[[paste0(Robust.coeff,j)]][Row]
                Median.Model$Tidy[Median.Model$Tidy$term == j, "MAD"]       <- Table.Coeffs[[paste0("Mad.",j)]][Row]
                Median.Model$Tidy[Median.Model$Tidy$term == j, "conf.low"]  <- Table.Coeffs[[j]][Row] - ifelse(!is.na(Table.Coeffs[[paste0("Mad.",j)]][Row]), 1.4826 * Table.Coeffs[[paste0("Mad.",j)]][Row],NA)
                Median.Model$Tidy[Median.Model$Tidy$term == j, "conf.high"] <- Table.Coeffs[[j]][Row] + ifelse(!is.na(Table.Coeffs[[paste0("Mad.",j)]][Row]), 1.4826 * Table.Coeffs[[paste0("Mad.",j)]][Row],NA)
                Median.Model$Tidy[Median.Model$Tidy$term == j, "std.error"] <- Table.Coeffs[[paste0("std.error.",j)]][Row]
                Median.Model$Tidy[Median.Model$Tidy$term == j, "t value"]   <- Median.Model$Tidy[Median.Model$Tidy$term == j, "estimate"]/Median.Model$Tidy[Median.Model$Tidy$term == j, "std.error"]
                #https://stackoverflow.com/questions/46186115/calculating-p-values-for-given-t-value-in-r
                Median.Model$Tidy[Median.Model$Tidy$term == j, "Pr(>|t|)"]  <- 2*pt(q = abs(Median.Model$Tidy[["t value"]][Median.Model$Tidy$term == j]), df = nrow(All.Compare) - 2, lower = FALSE)
                Median.Model$Coef[which(names(Median.Model$Coef) == j)]     <- Table.Coeffs[[paste0(Robust.coeff,j)]][Row]}
            Median.Model$Tidy$tau <- paste0("Weighted coefficients of rolling calibration models using : ", Robust.coeff, " method with interval of ", Interval, " days")
        }
        # Augment: changing coefficients of initModel with the weighted mean of coefficients
        Median.Model$InitModel$coefficients <- Median.Model$Coef
        Median.Model$Augment                <- ASE.ID$General.DT[DateCal, list(date)]
        Median.Model$Augment[, x := ASE.ID$General.DT[DateCal, ASE.ID$nameGasRef , with = F]]
        Median.Model$Augment[, y := ASE.ID$General.DT[DateCal, ASE.ID$nameGasVolt, with = F]]
        # Adding covariates and selecting only complete cases
        if (all(namesCovariates != "")) {
            Median.Model$Augment[, (namesCovariates) := ASE.ID$General.DT[DateCal, .SD, .SDcols = namesCovariates]]
            Median.Model$Augment <- Median.Model$Augment[is.finite(rowSums(Median.Model$Augment[,.SD,.SDcols= c("x","y",namesCovariates)]))]
        } else Median.Model$Augment <- Median.Model$Augment[is.finite(rowSums(Median.Model$Augment[,c("x","y")]))]
        if (Mod_type != "Linear.Robust.rqs") {
            if (Mod_type == "Ridge"){
                Median.Model$Augment[,".fitted" := as.matrix(cbind(rep(1, nrow(Median.Model$Augment)), Median.Model$Augment[, c("x",namesCovariates), with = F])) %*% Median.Model$Coef]
            } else Median.Model$Augment[,".fitted" := predict(Median.Model$InitModel, newdata = Median.Model$Augment)]
            Median.Model$Augment$.resid  <- Median.Model$Augment$y - Median.Model$Augment$.fitted
            Median.Model$Augment$.tau    <- paste0("Weighted coefficients of rolling calibration models using : ", Robust.coeff, " method with interval of ", Interval, " days")
        } else {
            #Significant numbers in y
            #Median.Model$Augment[, signif.y := nchar(strsplit(as.character(y), "\\.")[[1]][2])]
            
            Median.Model$Augment[, .tau := cut(y, breaks = stats::quantile(y, probs = seq(0.1,0.9,0.1)))]
            levels(Median.Model$Augment$.tau) <- 1:ncol(Median.Model$Coef)
            # Setting .tau for min and max of y 
            data.table::set(Median.Model$Augment, i = which(Median.Model$Augment$y == quantile(Median.Model$Augment$y, 0.1)), j = ".tau", value = 1)
            data.table::set(Median.Model$Augment, i = which(Median.Model$Augment$y == quantile(Median.Model$Augment$y, 0.9)), j = ".tau", value = ncol(Median.Model$Coef))
            
            Median.Model$Augment[, slope     := Median.Model$Coef[2,.tau]]
            Median.Model$Augment[, intercept := Median.Model$Coef[1,.tau]]
            Median.Model$Augment[, .fitted    := x * slope + intercept]
            Median.Model$Augment[, .resid     := y - .fitted]
            Median.Model$Augment[, .predicted := (y - intercept) / slope]
        }
        if ("tau" %in% names(Median.Model$Glance)) Median.Model$Glance$tau <- paste0("Weighted coefficients of rolling calibration models using : ", Robust.coeff, " method with interval of ", Interval, " days")
        N     <- nrow(Median.Model$Augment)
        Median.Model$Glance$r.squared     <- rsquare(true = Median.Model$Augment$y, predicted = Median.Model$Augment[,".fitted"])
        Median.Model$Glance$adj.r.squared <- Median.Model$Glance$r.squared * (N - (length(Median.Model$Coef)-1) - 1)/(N - 1)
        SSres <- sum((Median.Model$Augment$.resid)^2, na.rm = T)
        SSTot <- sum((Median.Model$Augment$y - mean(Median.Model$Augment$y))^2, na.rm = T)
        if (SSres > SSTot) futile.logger::flog.warn("[Median model] Something is wrong with the model: SSres > SSTot")
        Median.Model$Glance$RMSE          <- sqrt(SSres/(N - length(Median.Model$Coef) - 1))
        #https://www.oreilly.com/library/view/practical-statistics-for/9781491952955/ch04.html
        #https://stats.stackexchange.com/questions/87345/calculating-aic-by-hand-in-r
        k.original <- length(Median.Model$Coef)
        if (Mod_type != "Linear.Robust.rqs") df.ll <- k.original + 1 else df.ll <- k.original
        Median.Model$Glance$AIC           <- ((df.ll) * 2) + N*(log(2 * pi) + 1 + log((SSres/N)))
        # https://stackoverflow.com/questions/35131450/calculating-bic-manually-for-lm-object
        # Corrected when weighing is used
        w <- rep(1,N)
        Median.Model$Glance$logLik        <- 0.5 * (sum(log(w)) - N * (log(2 * pi) + 1 - log(N) + log(sum(w * Median.Model$Augment$.resid^2))))
        Median.Model$Glance$BIC           <- -2 * Median.Model$Glance$logLik + log(N) * df.ll
        Median.Model$Glance$AIC           <- -2 * Median.Model$Glance$logLik + 2 * df.ll
        # Delete uncalculated statistics
        del.Glance <- c("df.residual", "deviance", "df", "p.value", "statistic", "sigma")
        for (j in del.Glance) if (j %in% names(Median.Model$Glance)) Median.Model$Glance[, j] <- NULL
        Median.Model$Call     <- paste0("Rolling ", Interval," day of ", paste(deparse(Median.Model$Call), collapse = ""))
        Median.Model$Equation <- paste0("Rolling ", Interval," day: ", paste(Median.Model$Equation, collapse = ""))
        # Correcting the coefficients in equation
        for (i in seq_along(Old.Coeffs)){
            Median.Model$Equation <- sub(pattern     = as.character(format(Old.Coeffs[i]       , digits = 4, scientific = T)), 
                                         replacement = as.character(format(Median.Model$Coef[i], digits = 4, scientific = T)), Median.Model$Equation, fixed = T)
        } 
        # Correcting AIC in equation
        New.AIC   <- paste0(",AIC=",format(Median.Model$Glance$AIC, digits = 6, scientific = T))
        if (Mod_type != "Linear.Robust.rqs" && grepl("AIC", Median.Model$Equation)) {
            Start.AIC <- str_locate(Median.Model$Equation, "AIC")[1]
            End.AIC   <- str_locate(substr(Median.Model$Equation, Start.AIC, nchar(Median.Model$Equation)), ",")[1]
            Text.AIC  <- substr(Median.Model$Equation, Start.AIC - 1, ifelse(is.na(End.AIC),nchar(Median.Model$Equation),End.AIC - 1 ))
            Median.Model$Equation <- sub(pattern     = Text.AIC, replacement = New.AIC, Median.Model$Equation, fixed = T)
        } else Median.Model$Equation <- paste0(Median.Model$Equation, New.AIC)
        # Correcting RMSE in equation
        if (Mod_type != "Linear.Robust.rqs" && grepl("RMSE", Median.Model$Equation)) {
            Median.Model$Equation <- sub(pattern     = as.character(format(RMSE                   , digits = 4, scientific = T)), 
                                         replacement = as.character(format(Median.Model$Glance$RMSE, digits = 4, scientific = T)), Median.Model$Equation, fixed = T)
        } else Median.Model$Equation <- paste0(Median.Model$Equation, ", RMSE = ",format(Median.Model$Glance$RMSE, digits = 4, scientific = T))
        # Adding R.square and adjusted R2 in equation
        if (Mod_type != "Linear.Robust.rqs" && grepl("R2", Median.Model$Equation)) {
            Median.Model$Equation <- sub(pattern     = as.character(format(r.squared                       , digits = 4, scientific = T)), 
                                         replacement = as.character(format(Median.Model$Glance$adj.r.squared, digits = 4, scientific = T)), Median.Model$Equation, fixed = T)
        } else Median.Model$Equation <- paste0(Median.Model$Equation, ", R2 = ",format(Median.Model$Glance$r.squared, digits = 4, scientific = T))
        if (Mod_type != "Linear.Robust.rqs" && grepl("adj.R2", Median.Model$Equation)) {
            Median.Model$Equation <- sub(pattern     = as.character(format(adj.r.squared                   , digits = 4, scientific = T)), 
                                         replacement = as.character(format(Median.Model$Glance$adj.r.squared, digits = 4, scientific = T)), Median.Model$Equation, fixed = T)
        } else Median.Model$Equation <- paste0(Median.Model$Equation, ", Adj.R2 = ",format(Median.Model$Glance$adj.r.squared, digits = 4, scientific = T))
        
        # Saving Median Model
        nameModel <- strsplit(basename(List.Models$List.Added.Models[1]),"__")[[1]]
        nameModel <- paste0(paste(c(nameModel[1:4],
                                    format(Cal.DateIN,"%Y%m%d"),
                                    format(Cal.DateEND,"%Y%m%d"),
                                    nameModel[7], paste0("Median-", Interval),
                                    nameModel[8], Robust.coeff), collapse = "__"), "rdata")
        # discarding initModel that may be huge
        Median.Model <- Median.Model[which(names(Median.Model) != "InitModel")]
        list.save(Median.Model, file = file.path(ASE.ID$ASEDir, DIR_Models,nameModel))
        if (!exists("List.NewModels")) List.NewModels <- basename(nameModel) else List.NewModels <- c(List.NewModels,basename(nameModel))
    }
    return(list(List.NewModels = List.NewModels, Median.Model = Median.Model))
}
#' Determining significant covariates after applying a model
#'
#' @param Median.Models A vector of pathfiles of models to evaluate 
#' @param ASE.ID list, output of function Identifiy_ASE or Identify_ASE_Dir with information on AirSensEUR configuration
#' @param lhei numerical vector see heatmap.2, default value c(1.3,4.2) 
#' @param lwid numerical vector see heatmap.2, default value c(1.5,8)
#' @param Relationships a vector of character strings. Default is NULL. If not NULL these parameters are considered to be included in the 4 most significant covariates.
#' @param VIF logical, default is TRUE, If TRUE the variance inflator factors of dependent variables are computed and returned for MultiLinear models.
#' @param Relationships vector, default is NULL. It not NULL it shall give the covariates to be considered for selection in calibration model. 
#'        It shall not included: "SHT31TE_volt", "SHT31HE_volt", "BMP280_volt".
#' @param Thresh.R2 numeric, default is 0.00, difference between coefficient of determination of covariate/Residuals and covariates/Reference values to select covaristes
#' @param Add.Covariates logical default is FALSE, update Relationships if TRUE.
#' @param Plot.Cluster logical default is TRUE. If TRUE a cluster is plotted.
#' @param Dev.off logical default is TRUE. If TRUE the cluster plot showing relationships between co-variates is deleted from display.
#' @return A list with character vectors of up to 4 significant covariates, plot a heatmap with dendogram to evidence clusters of covariates
#' @examples List_Covariates(Median.Model.1, Relationships = Relationships, Add.Covariates = Add.Covariates)
List_Covariates <- function(Median.Models, ASE.ID = NULL, lmat = rbind(c(0,0),c(2,1), c(3,4)), lhei = c(0.3,5,0.8), lwid = c(1.5,6), 
                            Relationships = NULL, Thresh.R2 = 0.00, Add.Covariates = F, Plot.Cluster = FALSE, Dev.off = T, DateCal) {
    for (Model in Median.Models) {
        # Identy ASE boxe from model name
        if (is.null(ASE.ID)) ASE.ID <- Identify_ASE(Model) else ASE.ID <- Identify_ASE(Model, General.DT = ASE.ID$General.DT, ASE.cfg = ASE.ID$ASE.cfg, SetTime = ASE.ID$SetTime, Config = ASE.ID$Config, Shield = ASE.ID$Config$sens2ref.shield)
        # Compute the correlation matrix for this model
        futile.logger::flog.info(paste0("[List_Covariates] ASE box ", ASE.ID$ASE.name,", sensor ", str_pad(ASE.ID$name.sensor, width = max(str_length(ASE.ID$list.sensors),na.rm = T)),
                                        ", matrix of correlation between ", ASE.ID$Cal.DateIN, " and ", ASE.ID$Cal.DateEND))
        # Fist apply the selected Model
        General.DT     <- Apply_Model(Model = Model, Variables = ASE.ID$Variables, General.DT = ASE.ID$General.DT, ASE.cfg = ASE.ID$ASE.cfg, Config = ASE.ID$Config, Shield = ASE.ID$Config$sens2ref.shield)
        # Create Residuals and drift
        data.table::set(General.DT, j = "Residuals", value = General.DT[[ASE.ID$nameGasMod]] - General.DT[[ASE.ID$nameGasRef]])
        data.table::set(General.DT, j = "DRIFT",     value = difftime(General.DT$date, ymd(ASE.ID$Cal.DateIN, tz = "UTC"), units = "days"))
        # Adding DRIFT", "Absolute_humidity", "Td_deficit", ASE.ID$var.names.meteo
        if (!exists("Relationships") || is.null(Relationships) || is.na(Relationships) || Relationships == ""|| Add.Covariates) {
            namesCovariates <- names(General.DT[,grep(pattern = paste(c("_volt", "_modelled"), collapse = "|"), names(General.DT)), with = F])
            # Adding common names
            namesCovariates <- intersect(unique(c("DRIFT", "Absolute_humidity", "Td_deficit", ASE.ID$var.names.meteo, 
                                                  namesCovariates), fromLast  = T), names(General.DT))
        } else if (exists("Relationships")) namesCovariates <- Relationships
        # Discarding meteo volt names
        if (any(c("SHT31TE_volt", "SHT31HE_volt", "BMP280_volt","SHT31HI_volt","SHT31TI_volt") %in% namesCovariates)) namesCovariates <- namesCovariates[-grep(paste(c("SHT31TE_volt", "SHT31HE_volt", "BMP280_volt","SHT31HI_volt","SHT31TI_volt"), collapse = "|"), namesCovariates)]
        # Discarding the steps of outliers discarding if any
        Last2Char <- substr(namesCovariates, nchar(namesCovariates) - 2 + 1, nchar(namesCovariates))
        if (".1" %in% Last2Char) namesCovariates <- namesCovariates[-which(Last2Char %in% c('.1', ".2", ".3"))]
        if (length(namesCovariates) > 0) {
            # Adding ASE.ID$nameGasRef,"Residuals"
            namesCovariates <- unique(c(namesCovariates, ASE.ID$nameGasVolt, ASE.ID$nameGasMod, ASE.ID$nameGasRef,"Residuals"), fromLast  = T)
            # filter date and rows with NA, COmpute correlation matrix
            General.DT      <- ASE.ID$General.DT[DateCal, ..namesCovariates]
            #Sum.NA          <- sapply(General.DT, function(i) sum(is.na(i)))
            Columns2keep <- which(!sapply(names(General.DT), function(i) all(!is.finite(General.DT[[i]]) | General.DT[[i]] == 0)))
            Rows2keep    <- which(is.finite(General.DT[["Residuals"]])) 
            Matrix          <- data.table::as.data.table(Hmisc::rcorr(data.matrix(General.DT[Rows2keep, ..Columns2keep]))$r)
            # Checking if any variable has no data and dropping them
            Covariates2Drop <- which(!is.finite(rowSums(Matrix[, .SD, .SDcols = c(ASE.ID$nameGasRef, "Residuals")])))
            if (length(Covariates2Drop) > 0 ) Matrix <- Matrix[-Covariates2Drop,-Covariates2Drop, with = F]
            # Selecting the covariates with high correlation
            # filter only positive correlation with residuals - nameGasRef > Threshold where name is not "Residuals"
            namesCovariates <- names(Matrix)[Matrix$Residuals^2 - Matrix[[ASE.ID$nameGasRef]]^2 > Thresh.R2 & !names(Matrix) %in% c(ASE.ID$nameGasVolt, ASE.ID$nameGasMod, ASE.ID$nameGasRef,"Residuals")]
            # Then order namesCovariates, starting with the highest corelation with residuals and least with reference data
            index.namesCovariates <- match(namesCovariates, names(Matrix))
            namesCovariates <- namesCovariates[order(Matrix$Residuals[index.namesCovariates]^2 - Matrix[[ASE.ID$nameGasRef]][index.namesCovariates]^2, decreasing = T)]
            # Be sure to include Residuals, nameGasRef, nameGasMod and nameGasVolt a the beginning of vector namesCovariates
            namesCovariates <- unique(c("Residuals",ASE.ID$nameGasRef, ASE.ID$nameGasMod, ASE.ID$nameGasVolt, namesCovariates))
            # Keep maximum 8 elements
            namesCovariates8 <- namesCovariates[1:min(8:length(namesCovariates))]
            # update index.namesCovariates with the final namesCovariates
            index.namesCovariates8 <- match(namesCovariates8, names(Matrix))
            # Heat map with dendogram
            if (length(namesCovariates) > 4) { # the 5 element is the 1st covariate
                if (Plot.Cluster && length(namesCovariates) > 5) { # Heatmap needs at least 2 covariates
                    col           <- colorRampPalette(c("lightblue", "white", "orangered"))(20)
                    par(cex.main = 0.75, cex.lab = 0.7, cex.axis = 0.8)
                    dev.new(width = 800, height = 600, unit = "px",noRStudioGD = TRUE)
                    gplots::heatmap.2(as.matrix(Matrix[index.namesCovariates8,..namesCovariates8]),
                                      cellnote     = round(as.matrix(Matrix[index.namesCovariates8,..namesCovariates8]),2),  # same data set for cell labels
                                      notecol      = "black",                                                                # change font color of cell labels to black
                                      density.info = "none",                                                                 # turns off density plot inside color legend
                                      trace        = "none",                                                                 # turns off trace lines inside the heat map
                                      margins      = c(10,3),                                                                # widens margins around plot c(bottom, left)
                                      col          = col,                                                                    # use on color palette defined earlier
                                      dendrogram   = "row",                                                                  # only draw a row dendrogram
                                      Colv         = "Rowv",                                                                 # columns should be treated identically to the rows
                                      cexCol       = 0.75,
                                      revC         = T,
                                      lmat         = lmat,                                                                   # Controlling position of all elements, color key, dendogram, heatmap
                                      lhei = lhei, lwid = lwid,
                                      key = FALSE                                                                            # Do not plot keyColor scale
                    )
                    title(main = basename(Model), outer = T, line = -1)
                    if (Dev.off) dev.off()}
                # Compute returning Matrix
                Co.Variates <- c(ASE.ID$nameGasVolt, ASE.ID$nameGasMod, ASE.ID$nameGasRef, "Residuals")
                return.Matrix <- Matrix[match(namesCovariates[5:length(namesCovariates)], names(Matrix)), Co.Variates, with = F]
                # Compute R^2
                return.Matrix[, (Co.Variates) := lapply(Co.Variates, function(i) return.Matrix[[i]]^2)]
                # Add row names giving covariates names
                return.Matrix[,row.names := namesCovariates[5:length(namesCovariates)]]
                # Add row names giving covariates names
                return(list(r.Matrix = Matrix, covariates.Matrix = return.Matrix[, c(5,1:4)]))
            } else {
                futile.logger::flog.trace("[List_Covariates] There are no significant covariates.")
                return(list(r.Matrix = NA, covariates.Matrix = NA))}
        } else {
            futile.logger::flog.INFO("[List_Covariates] You entered ony 1 covariates that cannot be considered.")
            return(list(r.Matrix = NA, covariates.Matrix = NA))
        }
    }
}

Formula.Degrees <- function(Mod_type = "Linear.Robust", ASE.ID, DateINPlot, DateENDPlot, Covariates, Multi.File = NULL) {
    if (Mod_type == "MultiLinear") {
        Matrice         <- General[date > DateINPlot & date <= DateENDPlot + 1, .SD, .SDcols = Covariates]
        names(Matrice)  <- Covariates
        if (!is.null(Multi.File)) {
            if (file.exists(Multi.File)) {
                # read Multi.File
                Multi.File.df <-  data.table::data.table(file = Multi.File, comment.char     = "#")
                
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
    } else if (any(mod.eta.model.type %in% c("NO2_Lab", "NO2_Lab_decay_inc"))) {
        if (!shiny::isTruthy(Covariates) || any(Covariates == "")) Covariates <- c("Out.Relative_humidity", "Out.Temperature")
        Degrees          <- rep(1,length(Covariates))
        namesCovariates  <- paste0(paste(Covariates,Degrees, sep = "-"),collapse = "&")
        Matrice          <- General[, .SD, .SDcols = c(Covariates, "date")]
        #names(Matrice)  <- namesCovariates
    } else if (any(mod.eta.model.type %in% c("Peaks_baseline", "exp_kT_NoC", "exp_kT", "exp_kTn", "exp_kK", "T_power", "K_power"))) {
        if (!shiny::isTruthy(Covariates) || any(Covariates == "")) namesCovariates <- "Out.Temperature"
        Matrice         <- General[date > DateINPlot & date <= DateENDPlot + 1, .SD, .SDcols = Covariates]
        #names(Matrice)  <- namesCovariates
    } else if (any(mod.eta.model.type %in% c("BeerLambert"))) {
        Covariates <- c("Out.Temperature", "Out.Atmospheric_pressure")
        Degrees <-  c(1,-1)
        namesCovariates <- paste0(Covariates,collapse = "&")
        Matrice         <- General[date > DateINPlot & date <= DateENDPlot + 1, .SD, .SDcols = Covariates]
    } else if (any(mod.eta.model.type %in% c("Kohler", "Kohler_modified", "Kohler_lit", "Kohler_only"))) {
        Covariates      <- c("Out.Relative_humidity")
        Degrees         <- 1
        namesCovariates <- paste0(Covariates,collapse = "&")
        Matrice         <- General[date > DateINPlot & date <= DateENDPlot + 1, .SD, .SDcols = Covariates]
        #names(Matrice)  <- namesCovariates
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
                        Sensor_name   = name.sensor,
                        lim           = EtalLim,
                        Auto.Lag      = Auto.Lag,
                        Plot_Line     = Plot_Line,
                        Verbose       = Verbose)
    return(list(Model.i = Model.i, namesCovariates = namesCovariates))
    
}
# Auto.Cal: Automatic calibration of 1 sensor ====
#' Auto.Cal: Automatic calibration of 1 sensor
#'
#' @param ASEDir A character vector with the list of all filepaths of ASE boxes to be submitted to the function Median_Model
#' @param ASE.ID list, output of function Identify_ASE or Identify_ASE_Dir with information on AirSensEUR configuration
#' @param name.sensors Character vector, default is "CO_A4_P1", with the sensors name for which a calibration model is set. 
#' @param Interval numeric the windows size in days of the the rolling calibration models, default is 5 days
#' @param DateIN Date, default is NULL, A date for the beginning of the rolling calibration intervals. If NULL DateIN is determined using Identify.ASE().
#' @param DateEN Date, default is NULL, A date for the end of the rolling calibration intervals. If NULL DateIN is determined using Identify.ASE().
#' @param Meas.DateIN  Starting date for prediction, POSIXct or Date type. Default is DateIN. Meas.DateIn is use for DateIN if DateIn is NULL
#' @param Meas.DateEND Starting date for prediction, POSIXct or Date type. Default is DateEND. Meas.DateEND is use for DateEND if DateEND is NULL
#' @param DRIFT logical, default is FALSE. If TRUE, DRIFT can be used as a covariate of the calibration model and conversely.
#' @param volt logical, default is FALSE. If TRUE, parameter names including "_volt" can be used as a covariate of the calibration and conversely.
#' @param modelled logical, default is FALSE. If TRUE, parameter names including "_modelled" can be used as a covariate of the calibration and conversely.
#' @param Verbose logical, default TRUE. If TRUE print some messages are sent to the console
#' @param VIF logical, default is TRUE. If TRUE the variance inflator factors of dependent variables are computed for each possible covariates. The covariate with VIF > Threshold.VIF are not retained for calibration.
#' @param Treshold.VIF numeric, default is 10. Threshold that If the threshold is exceeded, the selected covariate is dropped.
#' @param Conf.level numeric, default is 0.05, threshold not to be exceeded for demonstrating that the coefficients of models are significantly different from 0. If exceeded the tested calibration model is rejected. 
#' @param Thresh.R2 numeric, default is 0.00, difference between coefficient of determination of covariate/Residuals and covariates/Reference values to select covariates to be included in the calibration model.
#' @param Mod_type Character vector, default is "Linear.Robust", the model type to be fitted, e.g. "Linear.Robust". If Add.Covariates is TRUE, Mod_type shall be set to Linear.Robust.
#' @param Probs numerical, default is NULL. If not NULL, used as tau when performing rq fitting.
#' @param Relationships vector, default is NULL. If not NULL it shall give the covariates to be considered in priority for including in calibration model. It shall not include: "SHT31TE_volt", "SHT31HE_volt", "BMP280_volt" (check if this last sentence is TRUE).
#' @param degrees Character vector with the degrees of the covariates of multi variable calibration models. Default is NULL.
#' @param Add.Covariates logical default is TRUE, highest corolated covariates are added to the calibration honouring the selected Relationships first.
#' @param Weighted Logical default is FALSE. If TRUE calibration will use a weighted algorithm reducing input data in class of x and covariates.
#' @param DateCal default value NULL. If not, numeric vector  giving the index of dates in ASE.ID$General.DT used for calibration
#' @ ... additional parameters passed using ellipsis ... including Exclude.Cal, DIR_Config, DIR_Models, DIR_General Robust.coeff and del.Rolling
#' @param Exclude.Cal logical, default is TRUE. If TRUE data during calibration are excluded for prediction
#' @param DIR_Config character vector, default is "Configuration". It sets a sub-directory that shall exist at file.path(WD, Project, ASE) where is the file ASE.name.cfg, ...  are stored
#' @param DIR_Models Character vector of the subdirectory of ASEDir where are the calibration models.
#' @param DIR_General character vector, default is "General_data". It sets a sub-directory that shall exist at file.path(WD, Project, ASE) where is the file General.csv, .. are stored
#' @param Robust.coeff character vector, default is "Weighted.". Method of selecting the best coefficients of models. It can be median, Weighed, AlgA or Min.U.Pred., min.U.Cal. Possible values are  Weighted., Median, AlgA and min.U.
#' @param del.Rolling logical, default is TRUE. If TRUE, all intermediary calibration models of rolling window with Interval duration are deleted at the end of calibration and conversely. del.Rolling being FALSE is generally used for simulations.
#' @param del.Intermediary logical, default is FALSE. If TRUE, alll intermediary calibration models of with new covariates, only valid for Add.Covariates TRUE, are deleted at the end of calibration when new covariates is added to a calibration model.
#' @return A list with all steps of calibration 
#' @description Delagging during calibration and prediction is only carried out if if the parameters Sync.Cal and Sync.Pred are set to TRUE into ASE.cfg
#' @usage If Add.Covariates = TRUE, Mod_type will be set to "Linear.Robust" before the first model is established.
#' @examples Auto.Cal(ASEDir, name.sensor = "NO_B4_P1", Interval = 1, DateIN = as.Date("2020-01-21"), DateEND = NULL, 
#'                    DRIFT = FALSE, volt = TRUE, modelled = FALSE, Mod_type = "Linear.Robust", Relationships = c("Temperature_int"), degrees = "ExpGrowth", Add.Covariates = TRUE)
Auto.Cal <- function(ASEDir, ASE.ID = NULL, name.sensor = "CO_A4_P1", Interval = 5L, DateIN = NULL, DateEND = NULL, Meas.DateIN = DateIN, Meas.DateEND = DateEND,
                     DRIFT = FALSE, volt = FALSE, modelled = FALSE, Discarded.covariates = NULL,
                     Verbose = TRUE, 
                     VIF = TRUE, Treshold.VIF = 10, Conf.level = 0.05, Thresh.R2 = 0.00, Mod_type = "Linear.Robust", Probs = NULL, Relationships = NULL, degrees = "1",
                     Add.Covariates = TRUE, Weighted = FALSE, Sink.Files = T, DateCal = NULL,  ...) {
    arguments <- list(...)
    if ("Exclude.Cal" %in% names(arguments)) Exclude.Cal <- arguments$Exclude.Cal else Exclude.Cal <- TRUE
    if ("DIR_Config"  %in% names(arguments)) DIR_Config  <- arguments$DIR_Config  else DIR_Config  <- "Configuration"
    if ("DIR_Models"  %in% names(arguments)) DIR_Models  <- arguments$DIR_Models  else DIR_Models  <- "Models"
    if ("DIR_General" %in% names(arguments)) DIR_General <- arguments$DIR_General else DIR_General <- "General_data"
    if ("Robust.coeff" %in% names(arguments)) Robust.coeff <- arguments$Robust.coeff else  Robust.coeff = "min.U.Cal."
    if ("del.Rolling" %in% names(arguments)) del.Rolling <- arguments$del.Rolling else  del.Rolling = TRUE
    if ("del.Intermediary" %in% names(arguments)) del.Intermediary <- arguments$del.Rolling else  del.Intermediary = FALSE
    
    # sending console to a file in the directory three (script log) and to variable Console for shiny TextOutput ####
    if (Sink.Files) while (sink.number() > 0) {
        print(paste0("Number of sink channels opened: ", sink.number(), ". Closing opened channels"))
        sink(file = NULL)}
    if (length(Relationships) > 0 && shiny::isTruthy(Relationships)) namesCovariates <- paste0(paste(Relationships,degrees, sep = "-"),collapse = "&") else namesCovariates <- ""
    if (is.null(DateIN))  DateIN  = ASE.ID$Meas.DateIN
    if (is.null(DateEND)) DateEND = ASE.ID$Meas.DateEND
    Auto.cal.name <- paste0(c("Auto.Cal",name.sensor, format(DateIN,"%Y%m%d"),format(DateEND,"%Y%m%d"),
                              Mod_type,Add.Covariates,namesCovariates,paste0("Median-",Interval), Weighted, Robust.coeff),collapse = "__")
    sink(file.path(ASEDir, DIR_Models, paste0(Auto.cal.name, ".log")),
         type = c("output", "message"), split = TRUE, append = F ) # with split = TRUE we get the file on the screen and in log file
    # starting calibration model
    if (is.null(ASE.ID)) {
        futile.logger::flog.info("[Auto.Cal] identifying ASE box")
        List.models  <- List_models(ASEDir, name.sensor)
        if (!any(grepl(pattern = name.sensor, x = List.models))) {
            ASE.ID <- Identify_ASE_Dir(ASEDir, name.sensor, General.DT = NULL, ASE.cfg = NULL, SetTime = NULL, Config = NULL, Shield = NULL,
                                       DIR_Config = DIR_Config, DIR_Models = DIR_Models, DIR_General = DIR_General)
        } else ASE.ID <- Identify_ASE(Model = file.path(ASEDir,DIR_Models, List.models[grep(pattern = name.sensor, x = List.models)][1]))}
    # Correcting for possible Lag in the calibration dataSet
    # de lagging if requested in config file
    ASE.ID$General.DT <- DeLag_Cal(DT.General = ASE.ID$General.DT, ColRef = ASE.ID$nameGasRef, ColSens = ASE.ID$nameGasVolt, DateIN = DateIN, DateEND = DateEND, 
                                   Sync = ASE.ID$Sync.Cal, ASE.name = ASE.ID$ASE.name, name.sensor = ASE.ID$name.sensor)
    # Fitting all Mod_type models over the whole period set 1st Model to Linear.Robust if Add.Covariates is set to TRUE
    if (Add.Covariates) {
        if (Mod_type == "Ridge") {
            Init.Mod_type <- "Ridge"
        } else if (Mod_type %in% c("Linear.Robust", "MultiLinear")) {
            Init.Mod_type <- "MultiLinear"
        } else stop("Add. covariates == TRUE is only possible with model type Linear.Robust, MultiLinear or Ridge")
        Mod_type <- "Linear.Robust"}
    cat("#######################\n")
    futile.logger::flog.info(paste0("[Auto.Cal] ", basename(ASEDir),", sensor ",name.sensor, " fitting \"", Mod_type, "\" models."))
    if (Mod_type != "Linear.Robust") {
        if (!is.null(Relationships)) namesCovariates = Relationships else  namesCovariates =""
    } else namesCovariates = ""
    List.1 <- Roll_Fit_New_Model(ASEDir = ASEDir, DIR_Models = DIR_Models, ASE.ID = ASE.ID, Interval = Interval, name.sensors = ASE.ID$name.sensor, 
                                 DateIN = DateIN, DateEND = DateEND, Verbose = FALSE,
                                 Mod_type = Mod_type, Probs = Probs, namesCovariates =  namesCovariates, # correction, it use to be Relationships[1]
                                 degrees = ifelse(Mod_type != "Linear.Robust", ifelse(!is.null(degrees),degrees[1],"1"), "1"), Weighted = Weighted, Sync = FALSE,
                                 DateCal = DateCal)
    # Comparing all models of selected boxes in a vector of filepaths (ASEDir), saving and returning the comparisons
    cat("-----------------------\n")
    futile.logger::flog.info(paste0("[Auto.Cal] Comparing ", Mod_type, " models:"))
    # Correcting for possible Lag in the Prediction dataSet if requested
    ASE.ID$General.DT <- Var_Delag_Pred(DT.General = ASE.ID$General.DT, ColSens = ASE.ID$nameGasMod,  ColSensRaw = ASE.ID$gas.sensor,
                                        ColRef = ASE.ID$nameGasRef, ColRefRaw = gsub("Out.", "", ASE.ID$nameGasRef),
                                    Meas.IN = Meas.DateIN, Meas.END = Meas.DateEND, Cal.IN = DateIN, Cal.END = DateEND, 
                                    Sync.Pred = ASE.ID$Sync.Pred, Sync.Cal = ASE.ID$Sync.Cal)
    # ASE.ID$General.DT <- DeLag_Pred(DT.General = ASE.ID$General.DT, ColRef = ASE.ID$nameGasRef, ColSens = ASE.ID$nameGasVolt, 
    #                                 Meas.DateIN = Meas.DateIN, Meas.DateEND = Meas.DateEND, Cal.DateIN = DateIN, Cal.DateEND = DateEND, 
    #                                 Sync.Pred = ASE.ID$Sync.Pred, Sync.Cal = ASE.ID$Sync.Cal, ASE.name = ASE.ID$ASE.name, name.sensor = ASE.ID$name.sensor)
    browser()
    All.Compare.1 <- List_All_Compare(ASEDir, DIR_Models = DIR_Models, ASE.ID = ASE.ID, name.sensors = ASE.ID$name.sensor, 
                                      DateIN = DateIN, DateEND = DateEND, Meas.DateIN = Meas.DateIN, Meas.DateEND = Meas.DateEND,
                                      All.Models = basename(List.1$List.Added.Models), Save = FALSE, Verbose = TRUE, Sync.Cal = FALSE, Sync.Pred = FALSE, 
                                      Exclude.Cal = Exclude.Cal, DateCal = DateCal)
    # Confidence interval of coefficents of Models for Calibration models
    Table.Coeffs.1 <- Confidence_Coeffs(All.Compare = All.Compare.1, Mod_type = Mod_type)
    cat("-----------------------\n")
    futile.logger::flog.info("[Auto.Cal] Creating calibration Median Model with the median of coefficients of the rolling Linear.Robust models.")
    Median.Model.1 <- Median_Model(ASEDir, DIR_Models = DIR_Models, ASE.ID = ASE.ID, name.sensors = NULL, Table.Coeffs = Table.Coeffs.1, 
                                   Mod_type = Mod_type, All.Compare = All.Compare.1, List.Models = List.1, Interval = Interval, DateCal = DateCal, Robust.coeff = Robust.coeff, Weighted = Weighted)
    print(Median.Model.1$Median.Model$Tidy, quote = F)
    print(Median.Model.1$Median.Model$Glance, quote = F)
    cat("-----------------------\n")
    futile.logger::flog.info("[Auto.Cal] Looking for significant covariates that could be added to the Linear.Robust calibration model:")
    Co_variates.1  <- List_Covariates(file.path(ASE.ID$ASEDir,DIR_Models,Median.Model.1$List.NewModels), ASE.ID = ASE.ID,
                                      Relationships = Relationships, Add.Covariates = Add.Covariates, Thresh.R2 = Thresh.R2, DateCal = DateCal)
    if (shiny::isTruthy(Co_variates.1$covariates.Matrix) && nrow(Co_variates.1$covariates.Matrix) > 0) {
        futile.logger::flog.info(paste0("[Auto.Cal] the list of significant covariates that could be added to the Linear.Robust calibration model of ", name.sensor, " is:"))
        print(Co_variates.1$covariates.Matrix, quote = F)
    } else futile.logger::flog.info(paste0("[Auto.Cal] there are no (more) covariates to add to the calibration mode for sensor ", name.sensor,"."))
    # Deleting rolling calibration models
    if (del.Rolling && exists("List.1")) {
        if ("Models.Already" %in% names(List.1)) {
            Del.Models <- setdiff(List.1$List.Added.Models, List.1$Models.Already)
        } else  Del.Models <- List.1$List.Added.Models
        if (length(Del.Models) > 0) unlink(Del.Models)}
    # Preparing returning object
    Returned.List <- list(List.1 = List.1, All.Compare.1 = All.Compare.1, Table.Coeffs.1 = Table.Coeffs.1, Median.Model.1 = Median.Model.1, Co_variates.1 = Co_variates.1, Final_median_Model = Median.Model.1)
    # fitting other co_variates if requested
    if (shiny::isTruthy(Co_variates.1$covariates.Matrix) && Add.Covariates) {
        n.loop = 2
        # initial list of covariates
        repeat {
            cat("-----------------------\n")
            futile.logger::flog.info(paste0("[Auto.Cal] Looking for covariate ", n.loop - 1, " to be added to the calibration function."))
            # initial list of covariates with relationship on first places if exist
            if (!exists("First.covariate")) {
                futile.logger::flog.info("[Auto.Cal] setting new First.covariates using the covariates correlated with residuals of the previous calibration model.")
                Init.covariates <- get(paste0("Co_variates.",n.loop - 1))$covariates.Matrix[["row.names"]]
                if (!is.null(Relationships) && any(Relationships %in% names(ASE.ID$General.DT))) {
                    futile.logger::flog.info("[Auto.Cal] Adding the requested Relationships on top of the new First.covariates")
                    First.covariate <- base::unique(c(Relationships[Relationships %in% names(ASE.ID$General.DT)], Init.covariates))
                } else First.covariate <- Init.covariates
                # discarding DRIFT, -modelled, volt covariates, Discarded.covariates if requested and dropped for vif/AIC/Coefficients 
                if (!DRIFT && length(First.covariate) >= 0 && "DRIFT" %in% First.covariate) {
                    futile.logger::flog.info("[Auto.Cal] request to drop parameter \"DRIFT\" from possible significant covariates")
                    First.covariate <-  First.covariate[-which(First.covariate == "DRIFT")]} 
                if (!volt && length(First.covariate) >= 0 && any(grepl("_volt", First.covariate))) {
                    futile.logger::flog.info("[Auto.Cal] request to drop parameters ending with \"_volt\" from possible significant covariates")
                    First.covariate <-  First.covariate[grep(pattern = "_volt", First.covariate, invert = T)]}
                if (!modelled && length(First.covariate) >= 0 && any(grepl("_modelled", First.covariate))) {
                    futile.logger::flog.info("[Auto.Cal] request to drop parameters ending with \"_modelled\" from possible significant covariates")
                    First.covariate <-  First.covariate[grep(pattern = "_modelled", First.covariate, invert = T)]}
                if (!is.null(Discarded.covariates) && length(First.covariate) >= 0) {
                    futile.logger::flog.info(paste0("[Auto.Cal] request to drop parameters ", paste(Discarded.covariates, collapse = ", ")," from possible significant covariates"))
                    Covariate.to.Discard <- unlist(sapply(Discarded.covariates, function(i) which(First.covariate %in% i)))
                    if (length(Covariate.to.Discard) > 0) First.covariate <-  First.covariate[-Covariate.to.Discard]} 
                # Dropping covariate with multicolinearity, not reduced AIC or insignificant parameters of model
                if (exists("Dropped.covariates") && length(First.covariate) >= 0) {
                    futile.logger::flog.info(paste0("[Auto.Cal] dropped covariates (already used or VIF or AIC or coefficients): ", paste(Dropped.covariates, collapse = ", ")))
                    Covariate.to.Discard <- unlist(sapply(Dropped.covariates, function(i) which(First.covariate %in% i)))
                    if (length(Covariate.to.Discard) > 0) First.covariate <-  First.covariate[-Covariate.to.Discard]}
                if (exists("Added.Covariates") && !is.null(Added.Covariates)) First.covariate <- base::unique(c(Added.Covariates, First.covariate))} 
            futile.logger::flog.info(paste0("[Auto.Cal] Filtered, ordered by relationship covariates: ", paste0(First.covariate, collapse = ", ")))
            if (length(First.covariate) >= n.loop - 1) {
                # Adding Variance Inflation Factors to check Multilinearity and to switch between covariates if needed
                if ("DRIFT" %in% First.covariate && !"DRIFT" %in% names(ASE.ID$General.DT)) data.table::set(ASE.ID$General.DT, j = "DRIFT", value = difftime(ASE.ID$General.DT$date, ymd(DateIN, tz = "UTC"), units = "days"))
                if (VIF) {
                    # looping until one covariates shows no multicollinearity
                    for (i in First.covariate[(n.loop - 1):length(First.covariate)]) {
                        # https://stats.stackexchange.com/questions/112442/what-are-aliased-coefficients
                        if (length(degrees) < length(First.covariate[n.loop - 1])) {
                            Formula.degrees <- c(degrees, rep("1", length(First.covariate) - length(degrees)))
                        } else Formula.degrees <- degrees
                        # https://stackoverflow.com/questions/16674045/as-formula-in-r-doesnt-seem-to-accept-a-name-that-starts-with-a-number-followed
                        addq <- function(x) paste0("`", x, "`")
                        Formula <- as.formula(paste0(addq(ASE.ID$nameGasVolt)," ~ ",ASE.ID$nameGasRef, " + ", 
                                                     paste(First.covariate[1:(n.loop - 1)], collapse = " + ")))
                        # here we can use parameter y.name to limit vif only to the new covariate
                        nVIF <- HH::vif(lm(Formula, data = data.frame(ASE.ID$General.DT[date > DateIN & date <= DateEND + 1], check.names = F, stringsAsFactors = F), 
                                           x = TRUE), singular.ok = TRUE)
                        cat("-----------------------\n")
                        if (any(is.infinite(nVIF)) || any(nVIF > Treshold.VIF)) {
                            futile.logger::flog.warn(paste0("[Auto.Cal] Covariate ", i," gives Variance Inflation factors of ", paste(round(nVIF, digits = 1), collapse = ", "), ", with at least one VIF higher than threshold: ", Treshold.VIF, ",\n",
                                                            i, " does suffer from multicolinearity with other dependent variables. It cannot be included into the MultiLinear calibration model."))
                            if (exists("Dropped.covariates")) Dropped.covariates <- c(Dropped.covariates, i) else Dropped.covariates <- i
                            First.covariate <- First.covariate[-which(First.covariate %in% i)]
                            cat("-----------------------\n")
                            if (length(First.covariate) >= (n.loop - 1)) {
                                futile.logger::flog.info("[Auto.Cal] next Covariate to be considered for MultiLinear model is ", First.covariate[(n.loop - 1)]," ")
                                cat("-----------------------\n")
                                next
                            } else {
                                futile.logger::flog.warn("[Auto.Cal] There are no more covariates to be added to the calibration model")
                                cat("-----------------------\n")
                                break} 
                        } else {
                            futile.logger::flog.info(paste0("[Auto.Cal] Covariate \"", i,"\" gives Variance Inflation factors of ", paste(round(nVIF, digits = 1), collapse = ", "), ", lower than threshold: ", Treshold.VIF, ",\n",
                                                            "\"",i, "\" does not show multicolinearity with other independent variables. It can be included into the calibration model."))
                            cat("-----------------------\n")
                            break}}} 
                if (length(First.covariate) >= (n.loop - 1)) {
                    Select.covariates <- First.covariate[seq(1,(n.loop - 1))]
                    # Complete degrees with 1 for added covariates
                    if (length(degrees) < length(Select.covariates)) degrees <- c(degrees, rep("1", length(Select.covariates) - length(degrees)))
                    cat("-----------------------\n")
                    futile.logger::flog.info(paste0("[Auto.Cal] Fitting calibration model with ", n.loop - 1, " covariate(s): ", paste(Select.covariates, collapse = ", ")))
                    assign(paste0("List.Covariate.", n.loop), Roll_Fit_New_Model(ASEDir = ASEDir, DIR_Models = DIR_Models, ASE.ID = ASE.ID, Interval = Interval, DateIN = DateIN, DateEND = DateEND, 
                                                                                 name.sensors = ASE.ID$name.sensor, Mod_type = Init.Mod_type, 
                                                                                 namesCovariates = Select.covariates, 
                                                                                 degrees = ifelse(length(degrees)>length(Select.covariates)||length(degrees)<length(Select.covariates), degrees[seq_along(Select.covariates)], degrees),
                                                                                 Verbose = FALSE, Sync = FALSE, DateCal = DateCal, Probs = Probs, Weighted = Weighted))
                    Returned.List[[paste0("List.Covariate.", n.loop)]] <- get(paste0("List.Covariate.", n.loop))
                    # Comparing all models of selected boxes in a vector of filepaths (ASEDir), saving and returning the comparisons
                    cat("-----------------------\n")
                    futile.logger::flog.info("[Auto.Cal] Comparing rolling models.")
                    assign(paste0("All.Compare.", n.loop), List_All_Compare(ASEDir, DIR_Models = DIR_Models, ASE.ID = ASE.ID, name.sensors = ASE.ID$name.sensor,
                                                                            DateIN = DateIN, DateEND = DateEND, Meas.DateIN = Meas.DateIN, Meas.DateEND = Meas.DateEND,
                                                                            All.Models = basename(get(paste0("List.Covariate.", n.loop))$List.Added.Models), 
                                                                            Save = FALSE, Verbose = TRUE, Sync.Cal = FALSE, Sync.Pred = FALSE, Exclude.Cal = Exclude.Cal, DateCal = DateCal))
                    
                    Returned.List[[paste0("All.Compare.", n.loop)]] <- get(paste0("All.Compare.", n.loop))
                    # Confidence model with median of coefficents of Models
                    cat("-----------------------\n")
                    futile.logger::flog.info("[Auto.Cal] Creating Median Model with the median of coefficents of rolling models.")
                    assign(paste0("Table.Coeffs.", n.loop), Confidence_Coeffs(All.Compare = get(paste0("All.Compare.", n.loop)), Mod_type = Init.Mod_type))
                    Returned.List[[paste0("Table.Coeffs.", n.loop)]] <- get(paste0("Table.Coeffs.", n.loop))
                    cat("-----------------------\n")
                    assign(paste0("Median.Model.", n.loop), Median_Model(ASEDir, DIR_Models = DIR_Models, ASE.ID = ASE.ID, name.sensors = NULL, Table.Coeffs = get(paste0("Table.Coeffs.", n.loop)), 
                                                                         Mod_type = Init.Mod_type, All.Compare = get(paste0("All.Compare.", n.loop)), List.Models = get(paste0("List.Covariate.", n.loop)), Weighted = Weighted,
                                                                         Interval = Interval, DateCal = DateCal, Robust.coeff = Robust.coeff))
                    print(get(paste0("Median.Model.", n.loop))$Median.Model$Tidy, quote = F)
                    print(get(paste0("Median.Model.", n.loop))$Median.Model$Glance, quote = F)
                    Returned.List[[paste0("Median.Model.", n.loop)]] <- get(paste0("Median.Model.", n.loop))
                    # Checking improvement of model with AIC
                    current.AIC   <- get(paste0("Median.Model.", n.loop))$Median.Model$Glance$AIC
                    previous.AIC  <- Returned.List$Final_median_Model$Median.Model$Glance$AIC
                    if (current.AIC < previous.AIC) {
                        cat("-----------------------\n")
                        futile.logger::flog.info(paste0("[Auto.Cal] The Akaike Information Criterion (AIC) of the current model is ", current.AIC, ". It is lower than the AIC of the precedent model ", previous.AIC,"."))
                        futile.logger::flog.info(paste0("[Auto.Cal] Adding of covariate ", First.covariate[n.loop - 1]," improves the fit of the calibration model."))
                        cat("-----------------------\n")
                        futile.logger::flog.info("[Auto.Cal] Checking if any coefficient of model is not significant, except for the intercept")
                        # The first coefficient is not tested for Conf.level, hoping that it is the intercept of the model. BE CAREFUL IF the FIRST Coefficient is not the intercept 
                        names.coeff    <- names(get(paste0("Median.Model.", n.loop))$Median.Model$Coef)[2:length(names(get(paste0("Median.Model.", n.loop))$Median.Model$Coef))]
                        p.value.Coeffs <- sapply(names.coeff, function(name.coeff) SIGN.test(sort(get(paste0("All.Compare.", n.loop))[[name.coeff]]), md = 0, alternative = "two.sided")$p.value)
                        Equal.0.Coeffs <- p.value.Coeffs >= Conf.level | get(paste0("Median.Model.", n.loop))$Median.Model$Tidy$`Pr(>|t|)`[-1] > Conf.level#(1 - Conf.level)
                        futile.logger::flog.info(paste0("[Auto.Cal] Probabilities that the weighted mean of coefficients be equal to 0 with Sign test:"))
                        print(p.value.Coeffs)
                        if (!any(Equal.0.Coeffs)) { #all(get(paste0("Median.Model.", n.loop))$Median.Model$Tidy$`Pr(>|t|)`[-1] < Conf.level)
                            futile.logger::flog.info(paste0("[Auto.Cal] All coefficients of the model with covariate(s) ", paste0(Select.covariates, collapse =", "), " are significantly different from 0"))
                            futile.logger::flog.info(paste0("############# [Auto.Cal] covariate: ", First.covariate[n.loop - 1], " is included into the model."))
                            if (exists("Added.Covariates")) Added.Covariates <- c(Added.Covariates,First.covariate[n.loop - 1]) else Added.Covariates <- First.covariate[n.loop - 1]
                            assign(paste0("Co_variates.", n.loop), List_Covariates(file.path(ASE.ID$ASEDir, DIR_Models,get(paste0("Median.Model.", n.loop))$List.NewModels), 
                                                                                   Relationships = Relationships, Add.Covariates = Add.Covariates, Thresh.R2 = Thresh.R2, DateCal = DateCal))
                            Returned.List[[paste0("Co_variates.", n.loop)]] <- get(paste0("Co_variates.", n.loop))
                            # delete last Valid model if argument del.Intermediary is TRUE
                            if (del.Intermediary) unlink(file.path(ASE.ID$ASEDir, DIR_Models, Returned.List$Final_median_Model$List.NewModels), force = T)
                            # Adding new valid model
                            Del.covariate = FALSE
                            Returned.List[["Final_median_Model"]] <- get(paste0("Median.Model.", n.loop))
                            if (shiny::isTruthy(get(paste0("Co_variates.", n.loop))$covariates.Matrix) && nrow(get(paste0("Co_variates.", n.loop))$covariates.Matrix) > 0) {
                                futile.logger::flog.info(paste0("[Auto.Cal] ordered list of covariates that are correlated with residuals of the current calibration model for ", name.sensor))
                                print(get(paste0("Co_variates.", n.loop))$covariates.Matrix, quote = F)
                            } else {
                                futile.logger::flog.warn(paste0("[Auto.Cal] there are no covariates to add to the calibration mode for sensor ", name.sensor,"."))
                                cat("-----------------------\n")
                                break}
                        } else {
                            #Invalid.Coefs <- get(paste0("Median.Model.", n.loop))$Median.Model$Tidy$term[which(get(paste0("Median.Model.", n.loop))$Median.Model$Tidy$`Pr(>|t|)` > Conf.level)]
                            Invalid.Coefs <- names.coeff[Equal.0.Coeffs]
                            futile.logger::flog.warn(paste0("[Auto.Cal] The coefficient of parameter(s) " , paste(Invalid.Coefs, collapse = " and ")," of the current model is(are) not significantly different from 0."))
                            futile.logger::flog.warn(paste0("############# [Auto.Cal] The calibration model resulting from adding covariate ", First.covariate[n.loop - 1]," is not valid."))
                            futile.logger::flog.warn("[Auto.Cal] Either parameters are unstable when rolling the calibration models or it/they does not influence significantly the sensor responses. Looking for other covariates.")
                            Del.covariate = TRUE
                            # delete last rejected model
                            unlink(file.path(ASE.ID$ASEDir, DIR_Models, Returned.List[[paste0("Median.Model.", n.loop)]]$List.NewModels), force = T)}
                    } else {
                        futile.logger::flog.warn(paste0("[Auto.Cal] The Akaike information criterion (AIC) of the current model ", current.AIC, " is not lower that the AIC of the precedent model ", previous.AIC,"."))
                        futile.logger::flog.warn(paste0("############# [Auto.Cal] Adding of covariate ", First.covariate[n.loop - 1]," does not improve the fitting of calibration model. Looking for other covariates."))
                        Del.covariate = TRUE
                        # delete last rejected model
                        unlink(file.path(ASE.ID$ASEDir, DIR_Models, Returned.List[[paste0("Median.Model.", n.loop)]]$List.NewModels), force = T)} 
                } else {
                    futile.logger::flog.info("[Auto.Cal] There are no more covariates to be added to the calibration model.")
                    break} 
                if (del.Rolling && exists(paste0("List.Covariate.", n.loop))) {
                    if ("Models.Already" %in% names(get(paste0("List.Covariate.", n.loop)))) {
                        Del.Models <- setdiff(get(paste0("List.Covariate.", n.loop))$List.Added.Models, get(paste0("List.Covariate.", n.loop))$Models.Already)
                    } else  Del.Models <- get(paste0("List.Covariate.", n.loop))$List.Added.Models
                    if (length(Del.Models) > 0) unlink(Del.Models)}
            } else {
                futile.logger::flog.info("[Auto.Cal] There are no (more) covariates to be added to the calibration model.")
                break}
            # Dropping covariate with multicolinearity or unsignificant parameters of model
            if (Del.covariate == TRUE) {
                futile.logger::flog.warn(paste0("[Auto.Cal] ", First.covariate[n.loop - 1], " is discarded from the list of possible covariates because AIC is not improved using it or coefficients of model shows rolling variability."))
                if (exists("Dropped.covariates")) {
                    Dropped.covariates <- c(Dropped.covariates, First.covariate[n.loop - 1])   
                } else Dropped.covariates <- First.covariate[n.loop - 1]
                First.covariate <- First.covariate[grep(First.covariate[n.loop - 1], First.covariate, invert = T)]
            } else {
                # add another covariate, restart with new First.covariate
                n.loop = n.loop + 1
                # To make a new list of First covariate
                rm(First.covariate)}}
    } else futile.logger::flog.info("[Auto.Cal] It is not necessary or not requested to add other covariates to the calibration model.")
    sink()
    cat("-----------------------\n")
    futile.logger::flog.info(paste0("[Auto.Cal] Final model is ", Returned.List[["Final_median_Model"]]$Median.Model$Equation))
    if (exists("Returned.List")) return(Returned.List) else return()
}

# Function to save configuration and data of an AirSensEUR boxes after a new calibration model is set. ####
#' Function to save configuration and data of an AirSensEUR boxes after a new calibration model is set.
#' @param Model Calibration model to be applied
#' @param DIR_Config character vector, default is "Configuration". It sets a sub-directory that shall exist at file.path(WD, Project, ASE) where is the file ASE.name.cfg, ...  are stored
#' @param DIR_General character vector, default is "General_data". It sets a sub-directory that shall exist at file.path(WD, Project, ASE) where is the file General.csv, .. are stored
#' @Save logical, default is TRUE. If TRUE, ASE.cfg, SetTime, CovMod, Covariates and DT.General are saved.
#' @param name.sensors Character vector, with the sensors name for which a calibration model is applied and saved. 
#' @param General.DT A data.table with all ASE box data.
#' @param ASE.ID object returned by Identify_ASE or Identifiy_ASE_Dir.
Register.Model <- function(Model, DIR_Config = "Configuration", DIR_General = "General_data", Save = TRUE, name.sensor, DT.General, ASE.ID) {
    if (file.exists(Model)) {
        # Extract ASEDir - strip Model
        ASEDir        <- dirname(Model)
        Base.Model    <- basename(Model)
        # Extracting information from Base.Model
        # name of ASE box
        Stripped.Model <- unlist(strsplit(Base.Model, split = "__"))
        # Extract ASE.name
        ASE.name <- Stripped.Model[1]
        # Extract name.sensor
        name.sensor <- Stripped.Model[2]
        # Extract Sens.raw.unit
        Sens.raw.unit  <- Stripped.Model[3]
        # Extract Mod_type
        Mod_type <- Stripped.Model[4]
        # Extract Cal.DateIN and Cal.DateEND
        Cal.DateIN     <- as.Date(Stripped.Model[5], format = " %Y%m%d", optional = T)
        Cal.DateEND    <- as.Date(Stripped.Model[6], format = " %Y%m%d", optional = T)
        # Extract Variables
        Variables   <- ifelse(Stripped.Model[7] != ".rdata", sub(pattern = ".rdata", replacement = "",Stripped.Model[7]), NA)
        
        # Reading configuration files 
        ASE.cfg.file <- file.path(dirname(ASEDir), DIR_Config, paste0(ASE.name,".cfg"))
        if (file.exists(ASE.cfg.file)) {
            ASE.cfg <- data.table::fread(ASE.cfg.file, showProgress = F)  
        } else return(futile.logger::flog.info("[Config.Model] model ",ASE.cfg.file," does not exist."))
        SetTime.file <- file.path(dirname(ASEDir), DIR_Config, paste0(ASE.name,"_SETTIME.cfg"))
        if (file.exists(SetTime.file)) {
            SetTime <- data.table::fread(SetTime.file, showProgress = F)
        } else return(futile.logger::flog.info("[Config.Model] model ",ASE.cfg.file," does not exist."))
        CovMod.file <- file.path(dirname(ASEDir), DIR_Config, paste0(ASE.name,"_CovMod_",name.sensor,".cfg"))
        if (file.exists(CovMod.file)) {
            CovMod <- data.table::fread(CovMod.file, showProgress = F)
        } else return(futile.logger::flog.info("[Config.Model] model ",CovMod.file," does not exist."))
        Covariates.file <- file.path(dirname(ASEDir), DIR_Config, paste0(ASE.name,"_Covariates_",name.sensor,".cfg"))
        if (file.exists(Covariates.file)) {
            Covariates <- data.table::fread(Covariates.file, showProgress = F)
        } else return(futile.logger::flog.info("[Config.Model] model ",Covariates.file," does not exist."))
        
        # Set ASE.cfg with Model information
        ASE.cfg.column <- which(ASE.cfg[ASE.cfg$name.gas == "name.sensor"] == name.sensor)
        Model.saved <- sub(pattern = paste0(name.sensor, "__"), replacement = "", sub(pattern = paste0(ASE.name, "__"), replacement = "",Base.Model))
        data.table::set(ASE.cfg, i = which(ASE.cfg$name.gas == "Cal.func")     ,      j = ASE.cfg.column, value = Model.saved)
        data.table::set(ASE.cfg, i = which(ASE.cfg$name.gas == "Sens.raw.unit"),      j = ASE.cfg.column, value = Sens.raw.unit)
        data.table::set(ASE.cfg, i = which(ASE.cfg$name.gas == "mod.eta.model.type"), j = ASE.cfg.column, value = Mod_type)
        # Set SetTime with Model information
        SetTime.column <- which(SetTime[name.gas == "name.sensor"] == name.sensor)
        data.table::set(SetTime, i = which(SetTime$name.gas == "Cal.IN"),      j = SetTime.column, value = format(Cal.DateIN, "%Y-%m-%d"))
        data.table::set(SetTime, i = which(SetTime$name.gas == "DatePlotCal.IN"),  j = SetTime.column, value = format(Cal.DateIN, "%Y-%m-%d"))
        data.table::set(SetTime, i = which(SetTime$name.gas == "Cal.END"),     j = SetTime.column, value = format(Cal.DateEND, "%Y-%m-%d"))
        data.table::set(SetTime, i = which(SetTime$name.gas == "DatePlotCal.END"), j = SetTime.column, value = format(Cal.DateEND, "%Y-%m-%d"))
        # Discaring "&" and degrees from Variables
        if (Variables != "") {
            if (grepl("&", Variables)) Variables <- strsplit(Variables, "&")[[1]]
            if (any(grepl("-", Variables))) Variables <- as.character(sapply(Variables, function(i) strsplit(i, "-")[[1]][1]))
        } 
        # Set Covariates with Model information
        #data.table::set(Covariates, j = "Effects", value = unique(c(Covariates$Effects,Variables)))
        Covariates <- data.table::data.table(Effects = unique(c(Covariates$Effects,Variables)))
        # Set CovMod with Model information
        CovMod <- data.table::data.table(Effects = Variables)
        # Apply the new model to DT.General
        if (Mod_type != 'Yatkin') {
            DT.General <- Apply_Model(Model = Model, General.DT = DT.General, ASE.cfg = ASE.cfg, 
                                      Config =  ASE.ID$Config, Shield =  ASE.ID$Config$sens2ref.shield)}
        
        # Saving if requested
        if (Save) {
            data.table::fwrite(ASE.cfg   , ASE.cfg.file)
            data.table::fwrite(SetTime   , SetTime.file)
            data.table::fwrite(CovMod    , CovMod.file)
            data.table::fwrite(Covariates, Covariates.file)
            data.table::fwrite(DT.General, file.path(dirname(ASEDir), DIR_General, "General.csv"))
        }
        return(list(ASE.cfg, SetTime, CovMod, Covariates))
    } else return(futile.logger::flog.error("[Config.Model] model ", Model," does not exist."))
}
#' @param List.ASE (mandatory) character vector. These are the IDs of AirSensEUR boxes and  correspond to directories of AirSensEUR Boxes that shall exist at file.path(WD, Project)
#' @param ASE.ID list, output of function Identifiy_ASE or Identify_ASE_Dir with information on AirSensEUR configuration
#' @param Project character vector, default is "ASE_Boxes". It sets a subdirectory that shall exist at file.path(Dir) in which all sensor boxes data are stored
#' @param DIR_Models Character vector corresponding to the subdirectory of file.path(Dir,Project, List.ASE) where are the calibration models are stored.
#' @param DIR_Models Character vector, subdirectory of ASEDir where are the calibration models.
#' @param Interval Integer, default value is 1L. A number of days between Cal.DateIN and Cal.DateEND for rollling calibration models. 
#' @param DateIN  Date, default is NULL, A date for the beginning of the rolling calibration intervals. If NULL DateIN is determined using Identify.ASE().
#' @param DateEND Date, default is NULL, A date for the end of the rolling calibration intervals. If NULL DateIN is determined using Identify.ASE().
#' @param Meas.DateIN  Starting date for prediction, POSIXct or Date type. Default is DateIN
#' @param Meas.DateEND Starting date for prediction, POSIXct or Date type. Default is DateEND
#' @param Mod_type Character vector, type of models to be fitted as defined in Cal_Line(). Default is "Linear.Robust".
#' @param Relationships vector, default is NULL. If not NULL it shall give the covariates to be considered in priority for including in calibration model. 
#'        It shall not include: "SHT31TE_volt", "SHT31HE_volt", "BMP280_volt" (check if this last sentence is TRUE).
#' @param degrees Character vector with the degrees of the covariates of multi variable calibration models. Default is NULL.
#' @param Add.Covariates logical default is TRUE, the best covariates (in terms of correlation with residuals) are added to the calibration model, honouring the selected Relationships in priority
#' @param VIF logical, default is TRUE. If TRUE the variance inflator factors of dependent variables are computed and returned for MultiLinear models.
#' @param Treshold.VIF numeric, default is 10. If the threshold is exceeded, the selected covariate is dropped.
#' @param Conf.level numeric, default is 0.05, threshold not to be exceeded for demonstrating that the coefficients of models are significantly different from 0. If exceeded the tested calibration model is rejected. 
#' @param Thresh.R2 numeric, default is 0.00, difference between coefficient of determination of covariate/Residuals and covariates/Reference values to select covariates to be included in the calibration model.
#' @param Register logical, default is TRUE. If TRUE, the Model, configuration and data of an AirSensEUR boxes after a new calibration model is save.
#' @param Verbose logical, default TRUE. If TRUE print some messages are sent to the console
#' @param Rdata.name.file character vector default is NULL. Name of file where the outpout of AutoCal.Boxes.Sensor is saved. If NULL Rdata.name.file is 
#'        paste0(c("Auto.Cal",name.sensor, format(DateIN,"%Y%m%d"), format(DateEND,"%Y%m%d"), Mod_type,Add.Covariates,namesCovariates, Weighted, Robust.coeff),collapse = "__")
#' @param Weighted Logical default is FALSE. If TRUE calibration will use a weighted algorithm reducing input data in class of x and covariates according to the standard deviation of sensor data.
#' @usage If Add.Covariates = TRUE, Mod_type will be set to "Linear.Robust" before the first model is established.
AutoCal.Boxes.Sensor <- function(List.ASE, ASE.ID = NULL, Dir = "S:/Box Sync/AirSensEUR/Fieldtests/Shiny", Project = "ASE_Boxes", DIR_Models = "Models", name.sensor = "CO_A4_P1",
                                 Interval = 1L, DateIN = NULL, DateEND = NULL, Meas.DateIN = DateIN, Meas.DateEND = DateEND,
                                 Mod_type = "Linear.Robust", Probs = NULL, Relationships = NULL, degrees = NULL, Add.Covariates = TRUE, 
                                 VIF = TRUE, Treshold.VIF = 10, Conf.level = 0.05, DRIFT = TRUE, volt = TRUE, modelled = FALSE, Discarded.covariates = NULL,
                                 Thresh.R2 = 0.00, Register = TRUE, Verbose = TRUE, Rdata.name.file = NULL, Weighted = FALSE, DateCal = NULL, ...){
    Return.list <- list()
    arguments <- list(...)
    if ("Exclude.Cal" %in% names(arguments)) Exclude.Cal <- arguments$Exclude.Cal else Exclude.Cal <- TRUE
    if ("Usermins" %in% names(arguments)) Usermins <- arguments$Usermins else Usermins = NULL
    if ("Robust.coeff" %in% names(arguments)) Robust.coeff <- arguments$Robust.coeff else  Robust.coeff = "min.U.Cal."
    if ("del.Rolling" %in% names(arguments)) del.Rolling <- arguments$del.Rolling else del.Rolling = TRUE
    if ("del.Intermediary" %in% names(arguments)) del.Intermediary <- arguments$del.Intermediary else  del.Intermediary = FALSE
    for (i in List.ASE) {
        ASEDir   <- file.path(Dir, Project, i)
        if (!dir.exists(ASEDir)) return(futile.logger::flog.warn(paste0("[AutoCal.Boxes.Sensor] The directory ", ASEDir, " doesnot exist. Check List.ASE.")))
        # Looking for existin calibration model
        List.models <- List_models(ASEDir, name.sensor)
        # Identify box and sensor
        if (is.null(ASE.ID)) {
            if (length(List.models) > 0) ASE.ID <- Identify_ASE(Model = file.path(ASEDir,DIR_Models, List.models[1])) else ASE.ID <- Identify_ASE_Dir(ASEDir = ASEDir, name.sensor = name.sensor)
        } else {
            if (length(List.models) > 0) {
                ASE.ID <- Identify_ASE(Model = file.path(ASEDir,DIR_Models, List.models[1]), General.DT = ASE.ID$General.DT, ASE.cfg = ASE.ID$ASE.cfg, SetTime = ASE.ID$SetTime, Config = ASE.ID$Config, Shield = ASE.ID$Config$sens2ref.shield)
            } else ASE.ID <- Identify_ASE_Dir(ASEDir = ASEDir, name.sensor = name.sensor, General.DT = ASE.ID$General.DT, ASE.cfg = ASE.ID$ASE.cfg, SetTime = ASE.ID$SetTime, Config = ASE.ID$Config, Shield = ASE.ID$Config$sens2ref.shield)}
        # Averaging for TEOM for example for hourly data
        if (!is.null(Usermins)) ASE.ID$General.DT <- DF_avg(ASE.ID$General.DT, width = Usermins)
        # calibrate, checking if this box as data for calibration
        if (!is.null(DateCal)){
            # Set DateIN and DateEND if DateCal exists for the purpose of naming calibration and log files
            DateIN  <- min(DateCal[[1]], na.rm = T)
            DateEND <- max(DateCal[[2]], na.rm = T)
            for (Row in 1:nrow(DateCal)) {
                Row.Interval <- lubridate::interval(DateCal[Row][[1]],DateCal[Row][[2]])
                if (any(ASE.ID$General.DT$date %within% Row.Interval)) {
                    Row.Dates <- which(ASE.ID$General.DT$date %within% Row.Interval)
                    if (exists("All.dates")) All.dates <- c(All.dates, Row.Dates) else All.dates <- Row.Dates
                    rm(Row.Dates)
                } else {
                    futile.logger::flog.warn(paste0("[AutoCal.Boxes.Sensor] no data for calibration between ", DateCal[Row][[1]], " and ", DateCal[Row][[2]]))
                    next}}
            if (!exists("All.dates") || is.null(All.dates) || length(All.dates) == 0) {
                futile.logger::flog.warn(paste0("[AutoCal.Boxes.Sensor] ",ASE.ID$ASE.name, " ", ASE.ID$name.sensor, " nodata between ", DateIN, " and ", DateEND, ". Impossible to calibrate"))
                next
            } else if(all(is.na(ASE.ID$General.DT[All.dates][[paste0("Out.",ASE.ID$gas.sensor)]]))) {
                futile.logger::flog.warn(paste0("[AutoCal.Boxes.Sensor] ",ASE.ID$ASE.name, " ", ASE.ID$name.sensor, " nodata between ", DateIN, " and ", DateEND, ". Impossible to calibrate"))
                next
            } else {
                DateCal <- All.dates
                rm(All.dates)}
        } else {
            if (is.null(DateIN) || is.null(DateEND)) stop(futile.logger::flog.error(paste0("[AutoCal.Boxes.Sensor] please define DateIN and DateEND if you do not pass DateCal for ASE ",i)))
            DateCal <- which(ASE.ID$General.DT$date > DateIN & ASE.ID$General.DT$date < DateEND +1)
            if(all(is.na(ASE.ID$General.DT[date > DateIN & date <= DateEND +1][[paste0("Out.",ASE.ID$gas.sensor)]]))) {
                futile.logger::flog.warn(paste0("[AutoCal.Boxes.Sensor] ",ASE.ID$ASE.name, " ", ASE.ID$name.sensor, " nodata between ", DateIN, " and ", DateEND, ". Impossible to calibrate"))
                next}}
        if(Mod_type == "Yatkin"){
            nameGasVolt       <-  paste0(name.sensor,"_volt")
            nameGasMod        <-  paste0(ASE.ID$gas.sensor,"_modelled")
            k                 <- as.integer(which(unlist(ASE.ID$ASE.cfg[ASE.ID$ASE.cfg$name.gas == "name.sensor"]) == name.sensor))
            name.gas          <- names(which(unlist(ASE.ID$ASE.cfg[ASE.ID$ASE.cfg$name.gas == "name.sensor"]) == name.sensor))
            gas.sensor        <- ASE.ID$ASE.cfg[ASE.ID$ASE.cfg$name.gas == "gas.sensor", k, with = FALSE]
            gas.reference2use <- ASE.ID$ASE.cfg[ASE.ID$ASE.cfg$name.gas == "gas.reference2use", k, with = FALSE]
            nameModel         <- paste0(paste0(c(i,name.sensor,unlist(ASE.ID$ASE.cfg[ASE.ID$ASE.cfg$name.gas == "Sens.raw.unit", k, with = FALSE]),
                                                 Mod_type,format(DateIN,"%Y%m%d"),format(DateEND,"%Y%m%d"),
                                                 paste0(paste(Relationships, Degrees, sep = "-"),collapse = "&"), Weighted), "__", collapse = ""), ".rdata")
            if (!file.exists(file.path(ASEDir,DIR_Models, nameModel))) {
                assign(paste0("Fit.",strsplit(name.sensor,"_")[[1]][1],"_", i),
                       Validation.tool(General            = ASE.ID$General.DT,
                                       DateIN             = DateIN,
                                       DateEND            = DateEND,
                                       name.gas           = name.gas,
                                       model.log          = TRUE ,
                                       nameGasRef         = paste0("Out.",gas.reference2use), # Gas reference
                                       nameGasVolt        = nameGasVolt,      # sensor gas in volt or nA or Count
                                       nameGasMod         = nameGasMod,   # modelled sensor gas
                                       unit.ref           = unlist(ASE.ID$ASE.cfg[ASE.ID$ASE.cfg$name.gas == "ref.unitgas"  , k, with = FALSE]),
                                       unit.sensor        = unlist(ASE.ID$ASE.cfg[ASE.ID$ASE.cfg$name.gas == "Sens.unit"    , k, with = FALSE]),
                                       Sens.raw.unit      = unlist(ASE.ID$ASE.cfg[ASE.ID$ASE.cfg$name.gas == "Sens.raw.unit", k, with = FALSE]),
                                       Reference.name     = "",
                                       AirSensEur.name    = i,
                                       name.sensor        = name.sensor,
                                       timeseries.display = FALSE,
                                       WDoutputMod        = file.path(ASEDir,DIR_Models),
                                       WDoutput           = file.path(ASEDir,"Calibration"),
                                       WDoutputStats      = file.path(ASEDir,"Statistics"),
                                       process.step       = "Calibration",
                                       mod.eta.model.type = Mod_type,
                                       Probs              = Probs,
                                       Multi.File         = NULL,
                                       eta.model.type     = unlist(ASE.ID$ASE.cfg[ASE.ID$ASE.cfg$name.gas == "eta.model.type", k, with = FALSE]),
                                       remove.neg         = as.logical(unlist(ASE.ID$ASE.cfg[ASE.ID$ASE.cfg$name.gas == "remove.neg", k, with = FALSE])),
                                       Covariates         = Relationships,
                                       Plot_Line          = FALSE,
                                       PlotCal            = FALSE,
                                       Auto.Lag           = as.logical(unlist(ASE.ID$ASE.cfg[ASE.ID$ASE.cfg$name.gas == "Sync.Cal"  , k, with = FALSE])),
                                       Verbose            = FALSE,
                                       Include.Model      = FALSE,
                                       SAVE               = TRUE,
                                       Weighted           = Weighted,
                                       DateCal            = DateCal))} else {
                                           assign(paste0("Fit.",strsplit(name.sensor,"_")[[1]][1],"_", i), load_obj(file.path(ASEDir,DIR_Models, nameModel)))
                                       }
            
            is.not.NA.y <- which(is.finite(rowSums(ASE.ID$General.DT[date >= lubridate::ymd(Meas.DateIN) & date <= lubridate::ymd(Meas.DateEND)+1][, .SD, .SDcols = c(nameGasVolt, Relationships)])))
            # since the RH increasing goes beyond the Meas.DateEND we increase 1 day
            predicted.data <- Meas_Function(y = ASE.ID$General.DT[date >= lubridate::ymd(lubridate::ymd(Meas.DateIN)) & date <= lubridate::ymd(Meas.DateEND)+1][[nameGasVolt]][is.not.NA.y],
                                            name.sensor = name.sensor,
                                            name.Model = file.path(ASEDir,DIR_Models, nameModel),
                                            Mod_type   = Mod_type ,
                                            covariates = Relationships,
                                            Degrees    = Degrees,
                                            Model      = load_obj(file.path(ASEDir,DIR_Models, nameModel)),
                                            Matrice    = ASE.ID$General.DT[date >= lubridate::ymd(Meas.DateIN) & date <= lubridate::ymd(Meas.DateEND)+1][is.not.NA.y, .SD, .SDcols = c("date", Relationships)], Verbose = Verbose)
            
            # Updating General.DT with predicted sensor values
            is.not.NA.y.pred <- which(ASE.ID$General.DT$date %in% predicted.data[!is.na(date)]$date)  
            data.table::set(ASE.ID$General.DT, i = is.not.NA.y.pred, j = nameGasMod, value = predicted.data[!is.na(date)]$x.fitted)
            
            # Plotting prediction if requested (when only Register = FALSE)
            if (!Register) {
                Prediction.DT <- ASE.ID$General.DT[is.not.NA.y.pred, .SD, .SDcols = c("date",ASE.ID$nameGasRef,ASE.ID$nameGasMod, gsub("Out.", "", ASE.ID$nameGasRef), gsub("_modelled", "", ASE.ID$nameGasMod))]
                # Prediction.DT <- ASE.ID$General.DT[is.not.NA.y.pred, .SD, .SDcols = c("date",ASE.ID$nameGasRef,ASE.ID$nameGasMod)]
                
                # discarding incomplete rows, there must be enough data otherwise impossible to calibrate
                Prediction.DT <- Prediction.DT[is.finite(rowSums(Prediction.DT[,names(Prediction.DT)[grep("date", names(Prediction.DT), invert = T)], with = F]))]
                
                # selecting only Prediction date, check if enough data
                if (Prediction.DT[date > lubridate::ymd(Meas.DateIN) & date <= lubridate::ymd(Meas.DateEND) + 1, .N] < 10) {
                    futile.logger::flog.warn(("[Compare_Models] no data for prediction, using dates of calibration."))
                    if (is.null(Meas.DateIN))  Meas.DateIN  <- Cal.DateIN
                    if (is.null(Meas.DateEND)) Meas.DateEND <- Cal.DateEND} 
                Prediction.DT <- Prediction.DT[date > lubridate::ymd(Meas.DateIN) & date <= lubridate::ymd(Meas.DateEND) + 1]
                
                # discarding calibration data if requested and some data are remaining
                if (Exclude.Cal) {
                    # if (is.null(DateCal)) {
                    # checking if remaining data after excluding calibration
                    if (Prediction.DT[!date %within% lubridate::interval(Cal.DateIN, Cal.DateEND + 1), .N] >= 120) {
                        Prediction.DT <- Prediction.DT[!date %within% lubridate::interval(Cal.DateIN, Cal.DateEND + 1)]
                    } else futile.logger::flog.warn("[Compare_Models] not enough data if calibration period is excluded when predicting. Data for Calibration are kept for prediction as well")}
                # Delag Prediction
                Prediction.DT <- Var_Delag_Pred(DT.General = Prediction.DT, ColSens = ASE.ID$nameGasMod,  ColSensRaw = ASE.ID$gas.sensor,
                                                ColRef = ASE.ID$nameGasRef, ColRefRaw = gsub("Out.", "", ASE.ID$nameGasRef),
                                            Meas.IN = ASE.ID$Meas.DateIN, Meas.END = ASE.ID$Meas.DateEND, Cal.IN = ASE.ID$Cal.DateIN, Cal.END = ASE.ID$Cal.DateEND, 
                                            Sync.Pred = ASE.ID$Sync.Pred, Sync.Cal = ASE.ID$Sync.Cal)
                # Prediction.DT <- DeLag_Pred(DT.General = Prediction.DT, ColRef = ASE.ID$nameGasRef, ColSens = ASE.ID$nameGasMod, 
                #                             Meas.DateIN = ASE.ID$Meas.DateIN, Meas.DateEND = ASE.ID$Meas.DateEND, Cal.DateIN = ASE.ID$Cal.DateIN, Cal.DateEND = ASE.ID$Cal.DateEND, 
                #                             Sync.Pred = ASE.ID$Sync.Pred, Sync.Cal = ASE.ID$Sync.Cal, ASE.name = ASE.ID$ASE.name, name.sensor = ASE.ID$name.sensor)
                
                # remove NA due to delagging
                Prediction.DT <- Prediction.DT[is.finite(rowSums(Prediction.DT[, c(ASE.ID$nameGasRef,ASE.ID$nameGasMod), with = FALSE]))]
                
                # Averaging data for Prediction if needed and compute average only if the number of rows within "width" is at least Min.Perc =  75% * width
                if (ASE.ID$Config$Server$UserMinsAvg != ASE.ID$Config$Server$UserMins) {
                    Prediction.DT <- DF_avg(Prediction.DT, 
                                            width = ASE.ID$Config$sens2ref[name.sensor==ASE.ID$name.sensor][["UserMinsAvg"]],
                                            hour_start = ASE.ID$Config$sens2ref[name.sensor==ASE.ID$name.sensor][["hour_start"]],
                                            Apply.Min.Perc = TRUE)}
                N.Predict <- Prediction.DT[,.N]
                plot(Prediction.DT[, .SD, .SDcols = c("date", ASE.ID$nameGasMod)], type = "l", col = "blue", xaxt = "n", main = paste0("[Compare_Models] Prediction ",basename(file.path(ASEDir,DIR_Models, nameModel))), cex.main = 0.8)
                if (diff(range(Prediction.DT$date)) > 31) {
                    r <- as.POSIXct(round(range(Prediction.DT$date), "month"))
                    graphics::axis.POSIXct(1, at = seq(r[1], r[2], by = "month"), format = "%Y-%m")
                } else {
                    r <- as.POSIXct(round(range(Prediction.DT$date), "days"))
                    graphics::axis.POSIXct(1, at = seq(r[1], r[2], by = "days"), format = "%Y-%m-%d")}
                lines(Prediction.DT[, .SD, .SDcols = c("date", ASE.ID$nameGasRef)], col = "red")
                plot(Prediction.DT[, .SD, .SDcols = c(ASE.ID$nameGasRef, ASE.ID$nameGasMod)], type = "p", col = "blue", main = paste0("[Compare_Models] Prediction ",basename(file.path(ASEDir,DIR_Models, nameModel))), cex.main = 0.8); grid()
                Prediction <- Cal_Line(x = Prediction.DT[[ASE.ID$nameGasRef]], s_x = NULL,
                                       y = Prediction.DT[[ASE.ID$nameGasMod]], s_y = NULL,
                                       Mod_type      = ASE.ID$eta.model.type,
                                       Matrice       = NULL, Weighted      = FALSE,
                                       Auto.Lag      = ifelse(ASE.ID$Sync.Pred, as.logical(unlist(ASE.ID$ASE.cfg[ASE.ID$ASE.cfg$name.gas == "Sync.Pred", ASE.ID$k, with = FALSE])), FALSE),
                                       Plot_Line     = TRUE, Verbose = FALSE)
            }
            
            
        } else {
            assign(paste0("Fit.",strsplit(name.sensor,"_")[[1]][1],"_", i), 
                      Auto.Cal(ASEDir, ASE.ID = ASE.ID, name.sensor = name.sensor, Interval = Interval, 
                               DateIN = DateIN, DateEND = DateEND, Meas.DateIN = Meas.DateIN, Meas.DateEND = Meas.DateEND,
                               DRIFT = DRIFT, volt = volt, modelled = modelled, Discarded.covariates = Discarded.covariates, 
                               #del.Rolling = del.Rolling,
                               VIF = VIF, Treshold.VIF = Treshold.VIF, Conf.level = Conf.level, Thresh.R2 = Thresh.R2,
                               Mod_type = Mod_type, Probs = Probs, Relationships = Relationships, degrees = degrees, 
                               Add.Covariates = Add.Covariates, Weighted = Weighted, DateCal = DateCal,
                               Exclude.Cal = Exclude.Cal, DIR_Models = DIR_Models, Robust.coeff = Robust.coeff, ...)) # dateCal is now a vector of index of selected dates for calibration
        }
        # Saving
        if (!is.null(get(paste0("Fit.",strsplit(name.sensor,"_")[[1]][1],"_", i)))) {
            Saved.List <- get(paste0("Fit.",strsplit(name.sensor,"_")[[1]][1],"_", i))
            if (is.null(Rdata.name.file)) {
                namesCovariates <- if (length(Relationships) > 0 && shiny::isTruthy(Relationships)) paste0(paste(Relationships,degrees, sep = "-"),collapse = "&") else ""
                Rdata.name.file <- paste0(paste0(c("Auto.Cal",name.sensor, format(DateIN,"%Y%m%d"), format(DateEND,"%Y%m%d"),
                                                   Mod_type,Add.Covariates,namesCovariates, paste0("Median-",Interval), Weighted, Robust.coeff),collapse = "__"), ".rdata")}
            save(Saved.List, file = file.path(ASEDir,DIR_Models = DIR_Models ,Rdata.name.file))
            rm(Saved.List)}
        if (exists(paste0("Fit.",strsplit(name.sensor,"_")[[1]][1],"_", i))) {
            Return.list[paste0("Fit.",strsplit(name.sensor,"_")[[1]][1],"_", i)] <- list(get(paste0("Fit.",strsplit(name.sensor,"_")[[1]][1],"_", i)))}
        # Updating ASE configuration with new Median Calibration Model
        if (Register){
            if(Mod_type == "Yatkin"){
                Local.CONFIG <- Register.Model(Model = file.path(ASEDir,DIR_Models, nameModel),
                                               name.sensor = name.sensor, DT.General = ASE.ID$General.DT[is.not.NA.y.pred], ASE.ID = ASE.ID)
            } else Local.CONFIG <- Register.Model(Model = file.path(ASEDir, DIR_Models ,get(paste0("Fit.",strsplit(name.sensor,"_")[[1]][1],"_", i))$Final_median_Model$List.NewModels),
                                                  name.sensor = name.sensor, DT.General = ASE.ID$General.DT, ASE.ID = ASE.ID)}
    }
    # return list of object
    if (exists("Return.list")) return(Return.list) else return()}
#================================================================CR
### functions to update and calibrate for LiberaIntentio and 52 North ####
#================================================================CR
# Configuration of AirSensEUR box ====
#' Configuration of AirSensEUR box
#' 
#' @description
#' 
#' Only downloaded reference data found in the file Project/ASE_name/General_data/RefData.csv will be merged with InfluxData.csv before
#' being push to the InfluxDB server only if argument Add.Ref is set to TRUE
#' Reference data will be updated only if boxConfig$Server$Down.Ref is et to TRUE before end in the Project/ASE_name/Configuration/ASE_name_Severs.cfg file, 
#' row Down.Ref
#' 
#'
#' @param boxName A character vector, the name of AirSenEUR box
#' @param rootWorkingDirectory character sting, default is null, reprsenting the directory of shiny. If null getwd()
#' @param Project a subdirectory where to AirSensEUR configuration and data
#' @return A list with the following data.tables "Server", "sens2ref", "CovPlot", "CovMod", "sens2ref.shield", "Sensors.cfg", "board.cfg" (2 last ones only if existing)
#' @examples influx.getConfig(boxName = i)
influx.getConfig <- function(boxName, Project = "ASE_Boxes", rootWorkingDirectory = NULL) {
    if(is.null(rootWorkingDirectory)) rootWorkingDirectory <- getwd()
    boxDirectory         <- file.path(rootWorkingDirectory, Project, boxName)
    local.CONFIG <- CONFIG(DisqueFieldtestDir = boxDirectory,
                           DisqueFieldtest    = rootWorkingDirectory,
                           shiny              = FALSE)
    if (gsub(pattern = "ASE", replacement = "",local.CONFIG$Server$Dataset) != local.CONFIG$Server$AirSensEur.name) {
        futile.logger::flog.warn("[influx.getConfig] The downloaded dataset has a different name than the ASE box. Please check _server.cfg file")}
    return(local.CONFIG)}

#' Convert to POSIXct
#' Convert a vector to POSIXct from Date, numeric and character. Character can be a numeric converted to character or a string with format %Y-%m-%d %H:%M:%OS or a mixture of both.
#' If the vector is already a POSIXct, the function returns the vector without changes.
#' @param Date.Vector mandatory, it can be a vector of numeric, character, Date or POSIXct
#' @param tz mandatory character vector time zone
#' @param boxNameoptional, character vector name of the AirSensEUR box to which Date.Vectorbelongs
#' @param Verbose optional, logical default is FALSE. If TRUE warning mesages are displayed
#' @details if Date.Vector is as character that cannot be converted to numeric, its format shall be %Y-%m-%d %H:%M:%OS
Set_date2POSIXct <- function(Date.Vector, tz = NULL, boxName = NA, Verbose = FALSE) {
    stopifnot(!is.null(Date.Vector))
    require (lubridate)
    require(futile.logger)
    
    if(lubridate::is.POSIXct(Date.Vector)) {
        if(Verbose) futile.logger::flog.warn(paste0("[Set_date2POSIXct] ",boxName, " Date.Vector already POSIXct, no need to convert"))
        return(Date.Vector)
    } else if (lubridate::is.Date(Date.Vector)) {
        if(Verbose) futile.logger::flog.warn(paste0("[Set_date2POSIXct] ",boxName, " Date.Vector is class Date converting to POSIXct"))
        return(as.POSIXct(Date.Vector,format="%Y-%m-%d"))
    } else {
        # Checking tz
        if (is.null(tz) || !shiny::isTruthy(tz)) {
            tz <- "UTC"
            if(Verbose) futile.logger::flog.warn("[Set_date2POSIXct] ",boxName, " ref.tzone is not defined and set to \"UTC\"")}
        
        # Initialising Date.Vector.Conv https://stackoverflow.com/questions/27052188/how-to-make-an-empty-vector-of-posixct
        Date.Vector.Conv <- .POSIXct(rep(NA,length(Date.Vector)), tz = tz)
        if(class(Date.Vector) %in% c("integer", "numeric")){
            Date.Vector.Conv <-  as.POSIXct(Date.Vector, origin = "1970-01-01", tz = tz)
        } else if(class(Date.Vector) == "character"){
            # Converting possible numeric in character to POSIXct
            Is.as.numeric   <- which(varhandle::check.numeric(Date.Vector, na.rm = T))
            if(length(Is.as.numeric) > 0) Date.Vector.Conv[Is.as.numeric] <- as.POSIXct(as.numeric(Date.Vector[Is.as.numeric]), origin = "1970-01-01", tz = tz)
            # Converting character to POSIXct expecting format %Y-%m-%d %H:%M:%OS
            Is.as.character <- base::setdiff(1:length(Date.Vector), Is.as.numeric)
            if(length(Is.as.character) > 0) Date.Vector.Conv[Is.as.character] <- lubridate::ymd_hms(Date.Vector[Is.as.character], tz = tz)}
        return(Date.Vector.Conv)}}

###############################################################
#' Function for plotting monitored paramerters during last Last.Days days of data to check if any variable is missing ####
###############################################################
#' @description
#' This function plots with a colored horizontal line plot missing data for the columns of a data.table. 
#' Red segments in the bar line indicate missing data and green segments indicate available data.
#' The function also checks in time frame of defined days if the number of missing hours exceeded a defined threshold.
#' The function returns a list with a boolean flag that indicates if the threshold of hours of missing data is exceeded or not by any of the columns being monitored in the data.table and 
#' a character string listing the column exceeding the threshold. This string can be used to send warning emails.
#' @param DT.data mandatory data.table with column date POSIXct and other columns the numeric variables to be monitored
#' @param Last.Days optional integer, default value is 3, time frame in number of days from the last date in DT.data to be monitored
#' @param Until.Day optional POSIXCt, default value is Null, last DateTime to check missing data. If Null value is given by Sys.time(), LAst:days are counted from Until.Day
#' @param Max.hours.missing, optional integer default value is 3, number of hours in the last part of Last.Days with missing value to be considered missing value
#' @param Discarded.Variables optional vector of strings, default value is NA. List of columns of DT.Data not to be monitored. 
#' If none of the Discarded.Variables are included in DT.Data no error is returned and no columns of DT.data are discarded
#' @param In.Dir mandatory if Save is TRUE, file.path where to dave the audit plot
#' @param Save optional boolean default value TRUE. If TRUE the plot is saved in Indir name  lastnDays.png otherwise it is plotted
#' @param file optional character vector, name of plot file to be saved, default value is Null. If null file is "lastnDays.png".
#' @param Web.Service optional character string default is NA. Web service is added on top of the returned
#' @return a list with 4 elements:  the calling arguments Max.hours.missing and Last.Days, Send.Mail a boolean which is TRUE if some columns of DT.Data are missing more than Max.hours.missing hours in Last.Days days
#' and Missing.Var a string that can be used for sending emails with the list of columns of DT.Data with missing data for how mnay hours. 
#' For identification purposes Web.Service is added at the begining of Missing.Var.
#' @example 
#' Data_reception(DT.Data = RefData, Last.Days = 3L, Max.hours.missing = 5L, 
#' Discarded.Variables <- c("Sample_air temperature", "temperature cpu", "manifold flow", "pressure caps", "temperature caps", "rel. humidity indoor", "temperature indoor"),
#' In.Dir = "S:/Box Sync/AirSensEUR/Fieldtests/Shiny/OPAS/ubis.data", Save = TRUE, Web.Service = "OPAS Service")
Data_reception <- function(DT.Data, Last.Days = 3L, Until.Day = NULL, Max.hours.missing = 3L, Discarded.Variables = NA, In.Dir = NA, Save = TRUE, file = NULL, Web.Service = "OPAS Service"){
    
    # Checking consistency
    stopifnot(!Save || !is.na(In.Dir))
    if(!is.na(In.Dir)) stopifnot(dir.exists(In.Dir))
    stopifnot("date" %in% names(DT.Data) && lubridate::is.POSIXct(DT.Data$date))
    stopifnot(length(names(DT.Data)) > 1)
    
    # Discarded.Variables are not monitored nor plotted
    # Index to be monitored in DT.Data
    if(!any(is.na(Discarded.Variables)) && length(intersect(Discarded.Variables, names(DT.Data))) > 0){
        Var2Plot <- grep(paste0(c("date", Discarded.Variables), collapse = "|"), names(DT.Data), invert = T, value = T)
    } else {
        Var2Plot <- grep("date", names(DT.Data), invert = T, value =T)}
    # Checking is all columns to be monitored are numeric
    futile.logger::flog.warn(paste0("[Data_reception] droppping non numeric columns of DT.Data"))
    # names of variables to be monitored
    Var2Plot <- names(Filter(is.numeric, DT.Data[,Var2Plot, with=FALSE]))
    stopifnot(length(Var2Plot) > 0)
    Ind.Var2Plot.Num <- match(Var2Plot,names(DT.Data))
    # adding a row at date Last.Days in case there are no data since Last.Days until current sys.time() (all data acquisition is frozen)
    if(is.null(Until.Day)) Until.Day <- Sys.time()
    if (DT.Data[date >= Until.Day - Last.Days * 24 * 60 *60, .N] < 1){
        DT.Last.Days <- DT.Data[.N]
        data.table::set(DT.Last.Days, i = 1L, j =names(DT.Last.Days), value = lapply(names(DT.Last.Days), function(i) return(NA)))
        data.table::set(DT.Last.Days, i = 1L, j ="date", value = Until.Day - Last.Days * 24 * 60 *60)
        DT.Data <- data.table::rbindlist(list(DT.Data, DT.Last.Days))}
    
    # adding an empty last row at current date in order to start counting Max.hours.missing from current date if Until.day not at the last date
    # We cannot use syst.time() directly because there will be a few seconds more than in Until.day, a few seconds may mean a minute
    if(format(Until.Day, "%Y-%m-%d %H") == format(Sys.time(), "%Y-%m-%d %H")){
        DT.add.Row <- DT.Data[.N]
        data.table::set(DT.add.Row, i = 1L, j =names(DT.add.Row), value = lapply(names(DT.add.Row), function(i) return(NA)))
        data.table::set(DT.add.Row, i = 1L, j ="date", value = Until.Day)
        DT.Data <- data.table::rbindlist(list(DT.Data, DT.add.Row))}
    
    # Plotting
    if(is.null(file)) file <- "lastnDays.png"
    if(Save) png(filename = file.path(In.Dir, file), width = 1920, height = 1080,)
    
    # Plotting empty frame
    par(mar=c(2,8,0.5,0.5))
    plot(c(Until.Day - Last.Days *24 * 60 * 60, Until.Day),c(1,length(Ind.Var2Plot.Num)),type="n",xlab="",ylab="",yaxt="n", xaxt="n")
    # Controlling Tickmarks
    axis(2,1:length(Var2Plot),Var2Plot,las=2, cex.axis = 0.6)
    axis.POSIXct(side = 1, x = c(Until.Day - Last.Days *24 * 60 * 60,Until.Day),
                 las=1, cex.axis = 0.6, at = pretty(c(Until.Day - Last.Days *24 * 60 * 60,Until.Day), n = min(30, 12 * Last.Days)), cex.axis = 0.8)
    # Adding vertical gridlines
    abline(v= pretty( c(Until.Day - Last.Days *24 * 60 * 60,Until.Day), n = min(30, 12 * Last.Days)), col = 'lightgrey', lty = "dotted")
    
    # Initalising parameters: mssage of mail and sending mail
    Send.Mail <- FALSE
    if(is.na(Web.Service)) Missing.Var <- NA else Missing.Var <- paste0(Web.Service, "\n")
    
    # Plotting segment for each vriable, seeting mail message
    for(Ind.Var in Ind.Var2Plot.Num){
        
        
        # Position of the segment of missing data, name of the variable
        Variable <- names(DT.Data)[Ind.Var]
        Line2plot <- match(Variable,Var2Plot)
        futile.logger::flog.info(Variable)
        
        # Creating data.table with measurements
        if(exists("Essai")){
            if(exists("DT.Last.Days")){
                Essai[date >= DT.Last.Days$date, (Variable) := DT.Data[date >= max(date) - Last.Days *24 * 60 * 60][[Variable]]]
            } else Essai[date >= Until.Day - Last.Days * 24 * 60 *60 & date <= Until.Day , (Variable) := DT.Data[date >= Until.Day - Last.Days * 24 * 60 *60 & date <= Until.Day][[Variable]]]
        } else {
            if(exists("DT.Last.Days")){
                Essai <- DT.Data[date >= DT.Last.Days$date,.SD,.SDcols = c("date",Variable)]
            } else Essai <- DT.Data[date >= Until.Day - Last.Days * 24 * 60 *60 & date <= Until.Day,.SD,.SDcols = c("date",Variable)]
        }
        # Setting valid data for Ind.Var
        Essai[, (paste0("is.", Variable)) := is.finite(Essai[[Variable]])]
        # Setting color "green" for valid data and "red" for missing data
        Essai[, (paste0("Col.", Variable)) := ifelse(is.finite(Essai[[Variable]]), "green", "red")]
        
        # plotting the change of color
        Change.Color <- Essai[which(diff(Essai[[paste0("is.", Variable)]]) != 0), .SD,.SDcols = c("date",Variable, paste0("is.", Variable), paste0("Col.", Variable))]
        if(nrow(Change.Color) > 0){
            for(Segment in seq_along(Change.Color$date)){
                
                if(Segment == 1 && nrow(Change.Color) > 1){
                    
                    # 1st segment but there are other segments
                    lines(c(Essai$date[1], Change.Color[Segment]$date), rep(Line2plot,2), col = Change.Color[Segment][[paste0("Col.",Variable)]], type = "l", cex = 0.2)
                } else if(Segment != nrow(Change.Color)){
                    
                    # Not the last segment
                    lines(c(Change.Color[Segment-1]$date + 1 * 60, Change.Color[Segment]$date), rep(Line2plot,2), col = Change.Color[Segment][[paste0("Col.",Variable)]], type = "l", cex = 0.2)
                } else if(Segment == nrow(Change.Color)){
                    
                    # Last segment
                    if(Segment == 1){
                        
                        # only one Segment
                        lines(c(Essai$date[1], Change.Color[Segment]$date), rep(Line2plot,2), col = Change.Color[Segment][[paste0("Col.",Variable)]], type = "l", cex = 0.2)
                    } else {
                        
                        # There are previous segment
                        lines(c(Change.Color[Segment-1]$date + 1 * 60, Change.Color[Segment]$date), rep(Line2plot,2), col = Change.Color[Segment][[paste0("Col.",Variable)]], type = "l", cex = 0.2)}
                    
                    # and last segment until the end of Essai
                    lines(c(Change.Color[Segment]$date + 1 * 60, Essai[.N]$date), rep(Line2plot,2), col = ifelse(Change.Color[Segment][[paste0("Col.",Variable)]] == "red","green","red"), type = "l", cex = 0.2)}
                
                # Returning info about missing Variable on the last segment
                # We only check the last segment. Could it be that a previous segement is missing more time? Likely not, it would have been identified before.
                if(Segment == nrow(Change.Color) && Change.Color[Segment][[paste0("Col.",Variable)]] == "green" && difftime(Essai[.N]$date, Change.Color[Segment]$date + 1 * 60, units = "hours") > Max.hours.missing){
                    if(exists("Missing.Var")){
                        Missing.Var <- paste0(Missing.Var,"\n",
                                              paste0(Variable, ": data missing for at least the last ", round(difftime(Essai[.N]$date, Change.Color[Segment]$date + 1 * 60, units = "hours"), digit=0), " hours"))
                    } else Missing.Var <- paste0(Variable, ": data missing for at least the last ", round(difftime(Essai[.N]$date, Change.Color[Segment]$date + 1 * 60, units = "hours"), digit=0), " hours")
                    Send.Mail <- TRUE}
            }
        } else {
            lines(range(Essai$date), rep(Line2plot, 2), col = Essai[1][[paste0("Col.",Variable)]], type = "l", cex = 0.2)
            # Checking missing data
            if(Essai[1][[paste0("Col.",Variable)]] == "red" && difftime(range(Essai$date)[2], range(Essai$date)[1], units = "hours") > Max.hours.missing){
                if(exists("Missing.Var")){
                    Missing.Var <- paste0(Missing.Var,"\n",
                                          paste0(Variable, ": data missing for at least the last ", round(difftime(range(Essai$date)[2], range(Essai$date)[1], units = "hours"), digit=0), " hours"))
                } else Missing.Var <- paste0(Variable, ": data missing for at least the last ", round(difftime(range(Essai$date)[2], range(Essai$date)[1], units = "hours"), digit=0), " hours")
                Send.Mail <- TRUE}
        }
        if(exists("Change.Color")) rm(Change.Color)
    }
    if(Save) dev.off()
    
    return(list(Send.Mail = Send.Mail, Missing.Var = Missing.Var, Max.hours.missing = Max.hours.missing, Last.Days = Last.Days))
}


###############################################################
#' Sensing email if needed                              ####
###############################################################
#' @description
#' This function send an email to List.Mails using authorised email sender (send.by) on the unauthorised smtp server smtp_server provided that Send.Mail is TRUE.
#' The value of Last.Days, Max.hours.missing, Send.Mail and the body message (Missing.Var) shall be returned by function Data_reception().
#' @param Data_reception mandatory, a list with 4 elements named: Last.Days integer, time frame in number of days from the last date of data to be monitored, 
#' Max.hours.missing integer the number of hours of missing data in Last.Days to send a warning email, Send.Mail boolean if TRUE a warning email is sent to List.Mails and Missing.Var character string the body meassage of emails
#' @param Send.Mail optional boolean, default value is TRUE. If TRUE an alarm email is sent to List.Mails
#' @param List.Mails optional vector of strings, default value c('michel.gerboles@ec.europa.eu', 'sinan.yatkin@ec.europa.eu'). List of email adresses to whcih to send emails.
#' @param send.by optional vector of strings, default value c('jrc-erlap@ec.europa.eu'). Authorised email sender (send.by) on the unauthorised smtp server smtp_server
#' @param smtp_server optional character string, default value 'smtpconnector.jrc.ec.europa.eu:25'. Unauthorised smtp server smtp_server
Send_Mail <- function(Data_reception, List.Mails = c('michel.gerboles@ec.europa.eu', 'sinan.yatkin@ec.europa.eu'), smtp_server = 'smtpconnector.jrc.ec.europa.eu:25', send.by = 'jrc-erlap@ec.europa.eu') {
    
    
    if(Data_reception$Send.Mail){
        
        for(email in List.Mails){
            # Set sender and recipients (email addresses only)
            Recipients <- email
            Sender     <- send.by
            
            # Full email message in RFC2822 format
            Message <- paste0('From: "AirSensEUR Influx (curl package)" <',send.by,'>
      To: "',strsplit(email,"[.]")[[1]][1],' Recipient" <',email, '>
      Subject: Hello AirSensEUR user!\n\n',
                              format(lubridate::ymd_hms(Sys.time()),"%Y-%m-%d %H:%M"), '\n',
                              'Dear AirSensEUR user,\nI am sending this email to let you know that the following pollutant/parameter(s) is(are) missing for at least ', Data_reception$Max.hours.missing,' hours, in the last ', Data_reception$Last.Days,' days.\n',
                              Data_reception$Missing.Var)
            # Send the email
            curl::send_mail(mail_from = Sender,
                            mail_rcpt   = Recipients,
                            message     = Message,
                            smtp_server = smtp_server,
                            username    = '',
                            password    = '')
        }
    }
}

#' download of AirSensEUR raw data at Influx server and application of calibration function (Prediction of AirSensEUR data)
#' @param boxName A character vector, the name of AirSenEUR box.
#' @param Project a subdirectory where to insert AirSensEUR configuration and data
#' @param boxConfig A list as created using function Identify_ASE(_Dir)
#' @param subDirConfig File path of the subdirectory of boxName where is the file ASE.name.cfg. Default value: "Configuration".
#' @param subDirModels File path of the subdirectory of boxName where are the calibration models. Default value: "Models".
#' @param subDirData File path of the subdirectory of boxName where is the file General.csv. Default value: "General_data".
#' @param Down.Influx Logical, default is TRUE. If TRUE InfluxDB data are downloaded provided that Download.Sensor$Retrieve.data.Influx is TRUE.
#' @param Add.Ref Logical, default is FALSE. If TRUE existing RefData.csv in directory subDirData will be included into General.file. If you want to download new Reference data set flag Down.Ref to TRUE in ASE_Server.cfg in addition to Ad.Ref TRUE.
#' @param Sensor.Date chararter vector with 2 strings, default is NULL, Range of date for Influx download, 1st string is the starting date (YYY-MM-DD) for Influx download, 2nd string is the ending date.
#' Sensor.Date is always used for Influx data download. For RefData is there are existing data the last date of RefData is used. if RefData is NULL and Sensor.Date exists then Sensor.Date is used.
#' If Sensor.Date does not exist then the minimum date of Influx data is used.
#' @param shiny logical, default value is FALSE If TRUE the function uses in a Shiny reactive context and shinyalert message can be returned.

#' @return a list with two elements: 1 the data.table DT.General with calibrated sensors ending with "_modelled",
#'                                   2 the data.table SetTime created with function SETTIME for boxName
#' @examples
#' ASE.Download <- influx.downloadAndPredict(boxName = List.ASE[i], boxConfig = ASE.ID.init$Config, Project = Project,
#'                                           DT.General = ASE.ID.init$General.DT,  ASE.ID.init = ASE.ID.init,
#'                                           subDirData = DIR_General, subDirModels = "Models", subDirConfig = DIR_Config,
#'                                           Add.Ref = Add.Ref, Down.Influx = Down.Influx )
influx.downloadAndPredict <- function(boxName, boxConfig, Project = "ASE_Boxes", DT.General = NULL, Download.Sensor = NULL, ASE.ID.init = NULL,
                                      subDirData = "General_data", subDirModels = "Models", subDirConfig = "Configuration",
                                      Down.Influx = TRUE, Sensor.Date = NULL, Add.Ref = FALSE, shiny = FALSE) {
    # remove Sos, Ref, isensors, list.gas.reference, list.gas.reference2use variables
    # saving all RDS filtering list files and DT.General after each changes.
    # remove columns of Calib_data that are not useful
    # adding futile.logger messages (missing update of DateTime)
    
    # Preparing configuration as in cfg files
    futile.logger::flog.info(paste0("[influx.downloadAndPredict] ", boxName, " Setting Initial values and file paths"))
    futile.logger::flog.info(paste0("[influx.downloadAndPredict] ", boxName, " Initial SetTime"))
    if (is.null(ASE.ID.init)){
        rootWorkingDirectory <- getwd()
        boxDirectory         <- file.path(rootWorkingDirectory, Project, boxName)
        list.gas.sensor      <- boxConfig[["sens2ref"]]$gas.sensor[!is.na(boxConfig[["sens2ref"]]$gas.sensor) & boxConfig[["sens2ref"]]$gas.sensor  != ""]
        list.name.sensor     <- boxConfig[["sens2ref"]]$name.sensor[!is.na(boxConfig[["sens2ref"]]$name.sensor) & boxConfig[["sens2ref"]]$name.sensor != ""]
        list.name.gas        <- boxConfig[["sens2ref"]]$name.gas[!is.na(boxConfig[["sens2ref"]]$name.sensor) & boxConfig[["sens2ref"]]$name.sensor != ""]
        list.name.reference  <- boxConfig[["sens2ref"]]$gas.reference2use[!is.na(boxConfig[["sens2ref"]]$gas.reference2use) & boxConfig[["sens2ref"]]$gas.reference2use != ""]
        ASE.cfg              <- data.table::transpose(boxConfig$sens2ref, make.names = 1, keep.names = "name.gas")
        if (shiny::isTruthy(boxConfig$Server$UserMins)) UserMins <- boxConfig$Server$UserMins else UserMins = 1
        Set.Time <- SETTIME(DisqueFieldtestDir = boxDirectory,
                            General.t.Valid    = DT.General,
                            Influx.TZ          = boxConfig[["Server"]]$Influx.TZ,
                            SOS.TZ             = boxConfig[["Server"]]$SOS.TZ,
                            Ref.TZ             = boxConfig[["Server"]]$ref.tzone,
                            DownloadSensor     = Download.Sensor,
                            Config             = boxConfig,
                            sens2ref.shield    = boxConfig$sens2ref.shield,
                            shiny              = FALSE)
        if (!is.null(ASE.ID.init$Config$Server$UserMins)) UserMins <- ASE.ID.init$Config$Server$UserMins else UserMins <- 1
    } else {
        boxDirectory         <- ASE.ID.init$ASEDir
        list.gas.sensor      <- ASE.ID.init$list.gas.sensor
        list.name.sensor     <- ASE.ID.init$list.sensors
        list.name.gas        <- names(ASE.ID.init$list.sensors)
        list.name.reference  <- ASE.ID.init$list.reference
        ASE.cfg              <- ASE.ID.init$ASE.cfg
        if (shiny::isTruthy(ASE.ID.init$Config$Server$UserMins)) UserMins <- ASE.ID.init$Config$Server$UserMins else UserMins = 1
        Set.Time <- ASE.ID.init$SetTime
        if (!is.null(boxConfig$Server$UserMins)) UserMins = boxConfig$Server$UserMins else UserMins = 1
    }
    # # discarding name.gas  when transposing in order to keep format of date date , other
    # if ("name.gas" %in% names(Set.Time)) Set.Time[, name.gas := NULL]
    # Set.Time <- data.table::transpose(Set.Time, make.names = 1, keep.names = "name.gas")
    
    DT.NULL    <- FALSE
    #DT.General <- NULL
    Influx     <- NULL
    Ref        <- NULL
    Sos        <- NULL
    General.file <- file.path(boxDirectory, subDirData, "General.csv")
    Influx.file  <- file.path(boxDirectory, subDirData, "InfluxData.csv")
    Ref.file     <- file.path(boxDirectory, subDirData, "RefData.csv")
    futile.logger::flog.info(paste0("[influx.downloadAndPredict] ", boxName, " loading initial Influx and Ref data if any"))
    if (file.exists(Influx.file)) {
        if (tools::file_ext(Influx.file) == "csv") {
            Influx <- data.table::fread(file = Influx.file, na.strings = c("","NA", "<NA>"), showProgress = T)
            data.table::set(Influx, j = "date", value = Set_date2POSIXct(Influx$date, tz = boxConfig$Server$Influx.TZ, boxName = boxName, Verbose = T))
        } else if (tools::file_ext(Influx.file) == "Rdata") {
            Influx <- load_obj(Influx.file)
            if (!data.table::is.data.table(Influx)) Influx <- data.table::data.table(Influx, key = "date")}
        if ("V1" %in% names(Influx)) Influx[, V1 := NULL]} else Influx <- NA
    if (Add.Ref && file.exists(Ref.file)) {
        if (tools::file_ext(Ref.file) == "csv") {
            Ref <- data.table::fread(file = Ref.file, na.strings = c("","NA", "<NA>"), showProgress = T)
            data.table::set(Ref, j = "date", value = Set_date2POSIXct(Ref$date, tz = boxConfig$Server$ref.tzone, boxName = boxName, Verbose = T))
        } else if (tools::file_ext(Ref.file) == "Rdata") {
            Ref <- load_obj(Ref.file)
            if (!data.table::is.data.table(Ref)) Ref <- data.table::data.table(Ref, key = "date")}
        if ("V1" %in% names(Ref)) Ref[, V1 := NULL]} else Ref <- NA_real_
    if (!shiny::isTruthy(DT.General)) {
        if (!shiny::isTruthy(DT.General) && file.exists(General.file)) {
            if (tools::file_ext(General.file) == "csv") {
                DT.General <- data.table::fread(General.file, na.strings = c("","NA", "<NA>"), showProgress = T) #, na.strings = getOption("","NA")
                # Convert date to POSIXct
                if (shiny::isTruthy(boxConfig$Server$Influx.TZ) || shiny::isTruthy(boxConfig$Server$SOS.TZ)) {
                    if (shiny::isTruthy(boxConfig$Server$Influx.TZ)) {
                        data.table::set(DT.General, j = "date"         , value =  Set_date2POSIXct(DT.General[["date"]]         , tz = boxConfig$Server$Influx.TZ))
                        if("date_PreDelay" %in% names(DT.General)) data.table::set(DT.General, j = "date_PreDelay", value =  Set_date2POSIXct(DT.General[["date_PreDelay"]], tz = boxConfig$Server$Influx.TZ))
                    } else {
                        data.table::set(DT.General, j = "date"         , value =  Set_date2POSIXct(DT.General[["date"]]         , tz = boxConfig$Server$SOS.TZ))
                        if("date_PreDelay" %in% names(DT.General)) data.table::set(DT.General, j = "date_PreDelay", value =  Set_date2POSIXct(DT.General[["date_PreDelay"]], tz = boxConfig$Server$SOS.TZ))}
                } else {
                    data.table::set(DT.General, j = "date"         , value =  Set_date2POSIXct(DT.General[["date"]]         , tz = "UTC"))
                    if("date_PreDelay" %in% names(DT.General)) data.table::set(DT.General, j = "date_PreDelay", value =  Set_date2POSIXct(DT.General[["date_PreDelay"]], tz = "UTC"))}
            } else if (tools::file_ext(General.file) == "Rdata") {
                DT.General <- load_obj(General.file)
                if (!data.table::is.data.table(DT.General)) DT.General <- data.table::data.table(DT.General, key = "date")}
            if ("V1" %in% names(DT.General)) DT.General[, V1 := NULL]
            
            # if some sensors of Influx are not included in DT.General, DT.General is re-created, checking that the sensors are also in Sensors.cfg. Careful with the change of name of sensors in INLUXDB
            t.ASE.cfg <- data.table::transpose(ASE.cfg, make.names = T)
            Name.gas.ASE.cfg <- t.ASE.cfg$gas.sensor
            if ("Sensors.cfg" %in% names(boxConfig)){
                Name.Sensors.cgf <- unique(boxConfig$Sensors.cfg$name)
                Name.gas.ASE.cfg <-t.ASE.cfg[name.sensor %in% Name.Sensors.cgf]$gas.sensor}
            if (!all(Name.gas.ASE.cfg %in% names(DT.General)) ) {
                futile.logger::flog.warn(paste0("[influx.downloadAndPredict] sensor ", Name.gas.ASE.cfg[!Name.gas.ASE.cfg %in% names(DT.General)], " missing, general is goeing to be recreated"))
                DT.General <- NA_real_
                DT.NULL    <- TRUE}
        } else {
            DT.General <- NA_real_
            DT.NULL    <- TRUE}}
    
    futile.logger::flog.info(paste0("[influx.downloadAndPredict] ",boxName, " loading initial DT.General data if any, DT.NULL is set to ", DT.NULL))
    if (!exists("DT.General") || !shiny::isTruthy(DT.General)) DT.General <- NA_real_
    if (!exists("Ref")        || !shiny::isTruthy(Ref))        Ref        <- NA_real_
    if (!exists("Influx")     || !shiny::isTruthy(Influx))     Influx     <- NA_real_
    if (!exists("Sos")        || !shiny::isTruthy(Sos))        Sos        <- NA_real_
    
    if(is.null(Download.Sensor)) Download.Sensor <- Check_Download(Influx.name = boxConfig$Server$Dataset,
                                                                   WDinput     = file.path(boxDirectory, subDirData),
                                                                   UserMins    = UserMins,
                                                                   General.df  = DT.General,
                                                                   RefData     = Ref,
                                                                   InfluxData  = Influx,
                                                                   SOSData     = Sos)
    futile.logger::flog.info(paste0("[influx.downloadAndPredict] ",boxName, " Initial DownloadSensor parameters"))
    
    # Downloading new Influx Data if requested, initial status values
    New.Influx  <- FALSE
    New.RefData <- FALSE
    
    futile.logger::flog.info(paste0("[influx.downloadAndPredict] ",boxName, " Setting downloading of Influx data is set to ", Download.Sensor$Retrieve.data.Influx))
    if (Download.Sensor$Retrieve.data.Influx) {
        # Downloading new Influx Data if requested
        INFLUX <- INFLUXDB(
            WDoutput        = file.path(boxDirectory,subDirData),
            DownloadSensor  = Download.Sensor,
            UserMins        = boxConfig$Server$UserMins,
            PROXY           = boxConfig$Server$PROXY,
            URL             = boxConfig$Server$URL,
            PORT            = boxConfig$Server$PORT,
            LOGIN           = boxConfig$Server$LOGIN,
            PASSWORD        = boxConfig$Server$PASSWORD,
            Down.Influx     = Down.Influx,
            Host            = boxConfig$Server$Host,
            Port            = boxConfig$Server$Port,
            User            = boxConfig$Server$User,
            Pass            = boxConfig$Server$Pass,
            Db              = boxConfig$Server$Db,
            Dataset         = boxConfig$Server$Dataset,
            Influx.TZ       = boxConfig$Server$Influx.TZ,
            name.SQLite     = file.path(boxDirectory,subDirData,"airsenseur.db"),
            name.SQLite.old = file.path(boxDirectory,subDirData,"airsenseur_old.db"),
            sens2ref        = boxConfig[["sens2ref"]],
            InfluxData      = Influx[],
            Sensor.Date     = Sensor.Date,
            Parallel        = TRUE)
        
        # Is there any new Influx data
        if (shiny::isTruthy(INFLUX[["InfluxData"]]) && (!shiny::isTruthy(Influx) || INFLUX[["InfluxData"]][.N]$date > Influx$date[nrow(Influx)]))  {
            futile.logger::flog.info(paste0("[influx.downloadAndPredict] ",boxName, ", there are new Influx data"))
            New.Influx <- TRUE
            Influx     <- INFLUX[["InfluxData"]]
            
            # Correcting time for asdebug
            if (List.ASE[i] == "asdebug"){
                setkey(Influx,date)
                setkey(Ref, date)
                
                # 1st lag
                Start <- lubridate::ymd_hm("2021-12-21 17:40")
                End   <- lubridate::ymd_hm("2022-02-06 00:00")
                Rows.2.Correct <-  which(Influx$date >= Start & Influx$date <= End)
                # Create the data.table to look for the Lag
                Delag.Interval <- lubridate::interval(Start, End)
                DataXY <- merge(Influx[date %between% c(Start,End), c("date","K96_SPLCPC")], Ref[date %between% c(Start,End), c("date", "Ref.CO2")], by = "date", all = FALSE)
                Lag <- Find_Max_CCF(DataXY[["K96_SPLCPC"]],DataXY[["Ref.CO2"]], Lag.max = 15000)
                # Correcting lag - lag is given in minutes
                futile.logger::flog.info(paste0("There is a lag of InfluxDB of: ", Lag$lag," row(s) of data which gives a better correlation, if lag is <> 0"))
                if (length(Rows.2.Correct) > 0 && Lag$lag != 0) data.table::set(Influx, i = Rows.2.Correct, j =  "date", value = Influx[Rows.2.Correct, date] - Lag$lag * 60)
                
                # 2nd lag
                Start <- lubridate::ymd_hm("2022-11-04 03:23")
                End   <- lubridate::ymd_hm("2023-01-15 23:06")
                Rows.2.Correct <-  which(Influx$date >= Start & Influx$date <= End)
                # Create the data.table to look for the Lag
                Delag.Interval <- lubridate::interval(Start, End)
                DataXY <- merge(Influx[date %between% c(Start,End), c("date","K96_SPLCPC")], Ref[date %between% c(Start,End), c("date", "Ref.CO2")], by = "date", all = FALSE)
                Lag <- Find_Max_CCF(DataXY[["K96_SPLCPC"]],DataXY[["Ref.CO2"]], Lag.max = 15000)
                # Correcting lag - lag is given in minutes
                futile.logger::flog.info(paste0("There is a lag of InfluxDB of: ", Lag$lag," row(s) of data which gives a better correlation, if lag is <> 0"))
                if (length(Rows.2.Correct) > 0 && Lag$lag != 0) data.table::set(Influx, i = Rows.2.Correct, j =  "date", value = Influx[Rows.2.Correct, date] - Lag$lag * 60)
            }
            data.table::fwrite(Influx, file = Influx.file, na = "NA")
            
            futile.logger::flog.info(paste0("[influx.downloadAndPredict] ",boxName, " updating Download.Sensor with new Influx data"))
            Download.Sensor <- INFLUX$DownloadSensor
            
            rm(INFLUX)
        } else New.Influx <- FALSE
        
        # Downloading REF Data if needed
        if (boxConfig$Server$Down.Ref) {
            # Checking if there are several ftp url
            if (boxConfig$Server$FTPMode == "ftp"){
                if(any(grepl(pattern = ",", x = boxConfig$Server$urlref))){
                    urlref = unlist(strsplit(gsub(pattern = " ","",x = boxConfig$Server$urlref), split = ",")  )
                } else urlref = gsub(pattern = " ","",x = boxConfig$Server$urlref)}
            # updating RefDATA
            # Determine initial date for REFDATA,last row without all data being NA
            if (shiny::isTruthy(Ref)){
                Begin.Date <- Ref$date[max(Ref[,lapply(.SD,function(x) tail(which(!is.na(x)),1)), .SDcols = names(Ref)[-which(names(Ref) == "date")]], na.rm = T)]
            } else {
                if(is.null(Sensor.Date)){
                    if(shiny::isTruthy(Influx)) Begin.Date <- min(Influx$date, na.rm = T) else lubridate::ymd("1970-01-01")
                } else Begin.Date <- Sensor.Date[1]}
            
            # Downloading RefData
            REFDATA <- REF(DownloadSensor     = Download.Sensor,
                           AirSensEur.name    = boxConfig$Server$AirSensEur.name,
                           DisqueFieldtestDir = boxDirectory,
                           UserMins           = as.numeric(boxConfig$Server$UserMins),
                           Down.Ref           = Add.Ref,
                           ref.tzone          = boxConfig$Server$ref.tzone,
                           InfluxData         = Influx,
                           SOSData            = Sos,
                           Reference.name     = boxConfig$Server$Reference.name,
                           urlref             = urlref,
                           sens2ref           = Config$all[["sens2ref"]],
                           FTPMode            = boxConfig$Server$FTPMode,
                           RefSOSname         = boxConfig$Server$RefSOSname,
                           Ref.SOS.name       = boxConfig$Server$Ref.SOS.name,
                           RefSOSPollutants   = boxConfig$Server$RefPollutants,
                           RefSOSDateIN       = Begin.Date,
                           RefSOSDateEND      = Sys.time(),
                           Ref__a_i_p__name         = boxConfig$Server$Ref__a_i_p__name,
                           User__a_i_p__            = boxConfig$Server$User__a_i_p__,
                           Pass__a_i_p__            = boxConfig$Server$Pass__a_i_p__,
                           Ref__a_i_p__Organisation = boxConfig$Server$Ref__a_i_p__Organisation,
                           Ref__a_i_p__Station      = strsplit(boxConfig$Server$Ref__a_i_p__Station, "!")[[1]],
                           Ref__a_i_p__Pollutants   = strsplit(boxConfig$Server$Ref__a_i_p__Pollutants, "!")[[1]],
                           Ref__a_i_p__DateIN       = Begin.Date,
                           Ref__a_i_p__DateEND      = Sys.time(),
                           csvFile            = ifelse(is.null(boxConfig$Server$csvFile),boxConfig$Server$urlref,boxConfig$Server$csvFile),
                           csvFile.sep        = boxConfig$Server$csvFile.sep, #",",
                           csvFile.quote      = boxConfig$Server$csvFile.quote, #FALSE,
                           csvFile.DateIN     = Begin.Date,
                           csvFile.DateEND    = Sys.time(),
                           coord.ref          = base::trimws(x= boxConfig$Server$coord.ref),
                           Ref.Type           = "Ref", #boxConfig$Server$Ref.Type,
                           RefData            = Ref[date <= Begin.Date],
                           shiny              = shiny)
            
            # updating Ref if new data are available
            if (!data.table::is.data.table(Ref) || (shiny::isTruthy(REFDATA[[1]]) && (REFDATA[[1]][.N]$date > Ref[.N]$date  || # 1st used to be is.na(Ref)
                                                                                      !all(names(REFDATA[[1]]) %in% names(Ref))))) {
                futile.logger::flog.info(paste0("[influx.downloadAndPredict] ",boxName, " there are new Reference data"))
                New.RefData     <- TRUE
                Ref             <- REFDATA[["RefData"]]
                Download.Sensor <- REFDATA[["DownloadSensor"]]
                data.table::fwrite(Ref, file = Ref.file, na = "NA")
                rm(REFDATA)
            } else New.RefData <- FALSE
        } else New.RefData <- FALSE
    }
    
    # Creating General Data
    futile.logger::flog.info(paste0("[influx.downloadAndPredict] ",boxName, " Creating or updating of General Data set to ", DT.NULL || New.Influx || New.RefData))
    if (DT.NULL || New.Influx || New.RefData) {
        
        # getting sensor and reference data together with filtering and calibration to later compare with what is in DT.General
        DT.General <- GENERAL(WDoutput            = file.path(boxDirectory, subDirData),
                              UserMins            = boxConfig$Server$UserMins,
                              Delay               = boxConfig$Server$Delay,
                              RefData             = Ref,
                              InfluxData          = Influx,
                              SOSData             = Sos,
                              var.names.Pollusens = list.gas.sensor  ,
                              DownloadSensor      = Download.Sensor,
                              Change.Delay        = FALSE,
                              Change.UserMins     = FALSE)
        
        # replacing NaN by NA before saving
        Cols.for.Avg <- names(DT.General)[-which(names(DT.General) == "date")]
        data.table::set(DT.General, j = Cols.for.Avg, value = lapply(DT.General[,..Cols.for.Avg], nan.to.na))
        # saving New General data
        if (tools::file_ext(General.file) == "csv") {
            data.table::fwrite(DT.General, file = General.file, na = "NA")
        } else if (tools::file_ext(General.file) == "Rdata") save(DT.General, file = General.file)
    }
    
    # Updating SetTime with last dates of new data per sensor
    # it could be that ASE.ID.init$Meas.DateEND coming from SetTime_ASE.cfg is earlier that the actual data in DT.General$date
    # As a result, the delay (DeLag_Pred) will not take into consideration data between ASE.ID.init$Meas.DateEND and max(DT.General$date).
    # Updating Set.Time from which to compute ASE.ID.init$Meas.DateEND
    for(Sensor in data.table::transpose(Set.Time, make.names = 1)[[1]]){
        # update last date of valid sensor data
        Last.Date.Sensor <- max(DT.General[is.finite(DT.General[[paste0(Sensor,"_volt")]])]$date, na.rm = T)
        if(is.finite(Last.Date.Sensor)) data.table::set(Set.Time, i = which(Set.Time$name.gas %in% c("Valid.END","Meas.END")), j = which(Set.Time[name.gas == "name.sensor"] == Sensor), value = as.Date(Last.Date.Sensor))
        rm(Last.Date.Sensor)   
        
        # update last date of valid reference data. If not available use Last.Date.Sensor
        Ref.name <- ASE.cfg[name.gas=="gas.reference2use"][[which(ASE.cfg[name.gas == "name.sensor"] == Sensor)]]
        if(Ref.name %in% names(DT.General)){
            Last.Date.Ref <- max(DT.General[is.finite(DT.General[[Ref.name]])]$date, na.rm = T)
            if(is.finite(Last.Date.Ref)){
                data.table::set(Set.Time, i = which(Set.Time$name.gas %in% c("Ref.END")), j = which(Set.Time[name.gas == "name.sensor"] == Sensor), value = as.Date(Last.Date.Ref))   
            } else  data.table::set(Set.Time, i = which(Set.Time$name.gas %in% c("Ref.END")), j = which(Set.Time[name.gas == "name.sensor"] == Sensor), value = as.Date(Last.Date.Sensor))
            rm(Ref.name, Last.Date.Ref)   
        }
    }
    
    # releasing memory of Influx, SOS and reference
    for (j in c("Influx", "Ref")) if (exists(j)) rm(list=j)
    
    #deleting garbage
    if (.Platform$OS.type == "windows") {
        gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)   
    } else gc(verbose = getOption("verbose"), reset = FALSE)
    
    return(list(data = DT.General, timeConfig = Set.Time))}

#================================================================CR
# Functions for RMardown reporting ####
#================================================================CR
# Adding several parameters to caption for table in the report of evaluation of calibration models ====
#' Adding several parameters to caption for table in the report of evaluation of calibration models
#'
#' @param Table A datatable with row of compared models
#' @param List.models.Final A string of calibration models for each ASE box to be compared
#' @return the table caption
#' @examples
Add_caption.Table <- function(Table, List.models.Final) {
    if (length(unique(Table$name.sensor)) ==1) {
        name.sensor <- unique(Table$name.sensor)   
    } else name.sensor <- ""
    Interval       <- str_split(str_split(List.models.Final, pattern = "__")[[1]][8], pattern = "-")[[1]][2]
    Cal.DateIN     <- unique(Table$Cal.DateIN)
    Cal.DateEND    <- unique(Table$Cal.DateEND)
    Pred.DateIN    <- unique(Table$Prediction.IN)
    Pred.DateEND   <- unique(Table$Prediction.END)
    Sensor.unit    <- unique(Table$Unit)
    Reference.Unit <- unique(Table$Unit)
    Model          <- unique(Table$Mod_type)
    if (length(Cal.DateIN) > 1 || length(Cal.DateEND) > 1) {
        return(paste0("Calibration period different for some ASE boxes. No caption. ", str_split(List.models.Final, pattern = "__")[[1]]))
    } else if (length(Pred.DateIN) > 1 || length(Pred.DateEND) > 1) {
        return(paste0("Prediction period different for some ASE boxes. No caption ", str_split(List.models.Final, pattern = "__")[[1]]))
    } else if (length(Sensor.unit) > 1) {
        return(paste0("Sensor unit different for some ASE boxes. No caption. ", str_split(List.models.Final, pattern = "__")[[1]]))
    } else if (length(Reference.Unit) > 1) {
        return(paste0("Sensor unit different for some ASE boxes. No caption. ", str_split(List.models.Final, pattern = "__")[[1]]))
    } else if (length(Model) > 1) {
        cat("Model Type different for some ASE boxes. ")
        Caption <- paste0("**Table XXX.** Comparison of ", name.sensor," calibrated data between ", unique(Table$Cal.DateIN), " and ",
                          unique(Table$Cal.DateEND)," (rolling window of ", Interval," days).")
    } else Caption <- paste0("**Table XXX**  Comparison of ", name.sensor, " data calibrated with ", Model, " model between ", 
                             unique(Table$Cal.DateIN), " and ",unique(Table$Cal.DateEND)," (Rolling window of ", Interval," days).")
    if (Cal.DateIN != Pred.DateIN || Cal.DateEND != Pred.DateEND)  Caption <- paste0(Caption, " Prediction was carried out between ", Pred.DateIN, " and ", Pred.DateEND, ").")
    caption <- c(caption,"\n")
    return(Caption)}
# # Adding row with median of coefficients ====
#' # Adding row with median of coefficients
#'
#' @param Table A datatable with row of compared models with coefficients of models between columns "Variables" and "R2raw"
#' @param New.names.coeffs A character vector with name of coefficients of the model. Default is c("a0, nA", "a1, nA/ppb") as for a linear model.
#' @param List.models.Final A list as created using function influx.getConfig for boxName
#' @return the data.table Table.report adding the row with median of coefficients of calibration models
#' @examples
Add_median_coeffs <- function(Table, New.names.coeffs = c("a0, nA", "a1, nA/ppb"), Table.report, Digits = c(1, 2), For.all = FALSE) {
    if (length(Digits) != length(New.names.coeffs)) stop("Check number of coefficients and length of vector of digits")
    # Current names of coefficients
    Names.coeffs <- seq(which(names(Table) == "Variables") + 1,which(names(Table) == "R2raw") - 1)
    
    if (For.all){
        # Table with median of coefficients
        Table.med <- Table.report[, lapply(.SD, function(i) {round(median(i, na.rm = T), 2)}), .SDcols = names(Table.report)[sapply(Table.report, is.numeric)]]
        # Table with MAD of coefficients
        Table.mad <- Table.report[, lapply(.SD, function(i) {round(mad(i, na.rm = T), 2)}), .SDcols = names(Table.report)[sapply(Table.report, is.numeric)]]
        # Adding  Table.mad to Table.med
        Table.med <- data.table::rbindlist(list(Table.med, Table.mad), use.names = T, fill = T)
    } else{
        # Table with median of coefficients
        Table.med <- Table[, lapply(.SD, function(i) {round(median(i, na.rm = T), 2)}), .SDcols = Names.coeffs]
        # Table with MAD of coefficients
        Table.mad <- Table[, lapply(.SD, function(i) {round(mad(i, na.rm = T), 2)}), .SDcols = Names.coeffs]
        # Adding  Table.mad to Table.med
        Table.med <- data.table::rbindlist(list(Table.med, Table.mad), use.names = T, fill = T)
        Table.med[, (New.names.coeffs) := .SD]
        Table.med <- Table.med[, ..New.names.coeffs]
    }
    Table.report <- data.table::rbindlist(list(Table.report, Table.med), use.names = T, fill = T)
    data.table::set(Table.report, i = nrow(Table.report)-1, j = 1L , value = "Median")
    data.table::set(Table.report, i = nrow(Table.report), j = 1L , value = "MAD")
    for (i in seq(length(New.names.coeffs))) {
        cols <- New.names.coeffs[i]
        Table.report[,(cols) := round(.SD,Digits[i]), .SDcols=cols]}
    for (i in names(Table.report)[sapply(Table.report, is.numeric)]) {
        if (!i %in% New.names.coeffs) Table.report[,(i) := round(.SD,Digits[2]), .SDcols=i]}
    return(Table.report)}

# Creating the table for reporting of comparison of models ====
#' Creating the table for reporting of comparison of models
#'
#' @param Table A datatable with row of compared models with coefficients of models between columns "Variables" and "R2raw"
#' @param New.names.coeffs A character vector with name of coefficients of the model. defaults is c("a0, nA", "a1, nA/ppb") for a linear model.
#' @param Digits A numeric vector with number of digits for each coefficient of the calibration model. Default is c(1,2) for a linear model. 
#' @return the data.table Table.report to be printed
#' @examples
Create_Table_comparison <- function(Table, New.names.coeffs = c("a0, nA", "a1, nA/ppb"), Digits = c(1, 2), N = FALSE, AIC = FALSE, ULV = T) {
    if (length(Digits) != length(New.names.coeffs)) stop("Check number of coefficients and length of vector of digits")
    
    # Reoder columns of coefficients of models if placed at the right of the table
    # Columns to move 
    Number.Columns.Coeffs <- names(Table)[(grep("Range.U.Pred", names(Table))+1):length(names(Table))]
    Number.Columns.Coeffs <- Number.Columns.Coeffs[!grepl("error|Interval", Number.Columns.Coeffs)]
    Number.Columns.Coeffs <- which(names(Table) %in% Number.Columns.Coeffs)
    if (length(Number.Columns.Coeffs) > 0) {
        # where to move them
        I.R2Raw <- grep("R2raw", names((Table)))
        setcolorder(Table, c(1:(I.R2Raw-1),Number.Columns.Coeffs,I.R2Raw,setdiff((I.R2Raw + 1):length(names(Table)), Number.Columns.Coeffs)))}
    
    # Initial names of coeffs in the first part of the table, grabing units of sensors if available
    Names.coeffs <- (which(names(Table) == "Variables") + 1):(which(names(Table) == "R2raw") - 1)
    columns.Nb1 <- match( c("ASE.name","SN.Cal"), names(Table))
    columns.Nb2 <- match(c("R2raw","Intcal","SlopeCal","R2Cal","RMSECal", "IntPred","SlopePred","R2Pred","RMSEPred"), names(Table))
    if ("Unit" %in% names(Table)) Sensor.Unit <- unique(Table$Unit)[1] else Sensor.Unit <- "ppb"
    New.names   <- c("ASE.IDs", "S/N", New.names.coeffs, "R2", paste0("Int.c, ",Sensor.Unit), paste0("Slope.c, ",Sensor.Unit,"/", Sensor.Unit), 
                     "R2.c", paste0("RMSE.c, ",Sensor.Unit), paste0("Int.p, ",Sensor.Unit), paste0("Slope.p, ",Sensor.Unit,"/", Sensor.Unit), "R2.p", paste0("RMSE.p, ",Sensor.Unit))
    if (length(unique(Table$Mod_type)) > 1) {
        columns.Nb1 <- match( c("ASE.name","SN.Cal", "Mod_type"), names(Table))
        New.names   <- insert(New.names, 3, "Model")}
    if (N) {
        columns.Nb1 <- match( c("ASE.name","SN.Cal", "N.Cal", "N.Predict","SN.Pred"), names(Table))
        New.names   <- insert(New.names, 3, "N")
        New.names   <- insert(New.names, 4, "N.pred")
        New.names   <- insert(New.names, 5, "S/N.pred")
    }
    if (AIC) {
        columns.Nb2 <- match(c("R2raw","Intcal","SlopeCal","R2Cal","RMSECal","AICCal", "IntPred","SlopePred","R2Pred","RMSEPred","AICPred"), names(Table))
        New.names   <- insert(New.names, match("RMSE, ppb", New.names) + 1, "AIC")
        New.names   <- c(New.names, "AIC")
    } 
    Table.report <- Table[, c(columns.Nb1,Names.coeffs,columns.Nb2), with = F]
    names(Table.report) <- New.names
    cols <- c(paste0("Int.c, ",Sensor.Unit), paste0("Slope.c, ",Sensor.Unit,"/", Sensor.Unit))
    Table.report[,(cols) := round(.SD,2), .SDcols=cols]
    for (i in seq(length(New.names.coeffs))) {
        cols <- New.names.coeffs[i]
        Table.report[,(cols) := round(.SD,Digits[i]), .SDcols=cols]}
    if (ULV) set(Table.report, j = "ULV", value = sapply(seq_along(Table$Range.U.Pred), function(i) Table$Range.U.Pred[[i]][6]))
    return(Table.report)}
# Creating the table for reporting of comparison of models ====
#' Creating the table for reporting of comparison of models
#'
#' @param Table A datatable with row of compared models with coefficients of models between columns "Variables" and "R2raw"
#' @param New.names.coeffs A character vector with name of coefficients of the model. defaults is c("a0, nA", "a1, nA/ppb") for a linear model.
#' @param Digits A numeric vector with number of digits for each coefficient of the calibration model. Default is c(1,2) for a linear model. 
#' @return the data.table Table.report to be printed
#' @examples
Create_Table_comparison_name.sensor <- function(Table, New.names.coeffs = c("a0, nA", "a1, nA/ppb"), Digits = c(1, 2), N = FALSE, AIC = FALSE) {
    if (length(Digits) != length(New.names.coeffs)) stop("Check number of coefficients and length of vector of digits")
    Names.coeffs <- (which(names(Table) == "Variables") + 1):(which(names(Table) == "R2raw") - 1)
    columns.Nb1 <- match( c("ASE.name","SN.Cal", "name.sensor"), names(Table))
    columns.Nb2 <- match(c("R2raw","Intcal","SlopeCal","R2Cal","RMSECal", "IntPred","SlopePred","R2Pred","RMSEPred"), names(Table))
    New.names   <- c("ASE.IDs", "S/N", "name.sensor", New.names.coeffs, "R2", "Int.c, ppb", "Slope, ppb/ppb", "R2", "RMSE, ppb", "Int.p, ppb", "Slope, ppb/ppb", "R2", "RMSE, ppb")
    if (length(unique(Table$Mod_type)) > 1) {
        columns.Nb1 <- match( c("ASE.name","SN.Cal", "Mod_type"), names(Table))
        New.names   <- insert(New.names, 3, "Model")}
    if (N) {
        columns.Nb1 <- columns.Nb1 <- match( c("ASE.name","SN.Cal", "name.sensor", "N.Cal"), names(Table))
        New.names   <- insert(New.names, 4, "N")}
    if (AIC) {
        columns.Nb2 <- match(c("R2raw","Intcal","SlopeCal","R2Cal","RMSECal","AICCal", "IntPred","SlopePred","R2Pred","RMSEPred","AICPred"), names(Table))
        New.names   <- insert(New.names, match("RMSE, ppb", New.names) + 1, "AIC")
        New.names   <- c(New.names, "AIC")}  
    Table.report <- Table[, c(columns.Nb1,Names.coeffs,columns.Nb2), with = F]
    names(Table.report) <- New.names
    cols <- c("Int.c, ppb", "Int.p, ppb")
    Table.report[,(cols) := round(.SD,2), .SDcols=cols]
    for (i in seq(length(New.names.coeffs))) {
        cols <- New.names.coeffs[i]
        Table.report[,(cols) := round(.SD,Digits[i]), .SDcols=cols]}
    return(Table.report)}
# Create_ASE: Inserting new ASE box in DirShiny ====
#' Inserting new ASE box in DirShiny
#' 
#' @description 
#' Create file system structure. check for General.Rdata availability, sourcing ASEConfig_xx.R ####
#' Check directories existence or create , create log file
#' Populate Project/New_ASE/Configuration with all cfg from cloning ASE
#' Update ASE_Servers.cfg with correct dataset name and correct ASE name
#' Copy all calibration models present in the Cloning ASE.cfg file into the Project/New_ASE/Models directory
#' 
#' @param DirShiny A File path where R script and Shiny script stay
#' @param Project character vector, default is "ASE_Boxes". It sets a directory that shall exist at file.path(DirShiny) in which all sensor boxe data are stored
#' @param Project a sub-directory name where to insert AirSensEUR configuration and data
#' @param New.ASE.Name A character vector with name of new ASE box to be cloned.
#' @param Cloning.path A file path of ASE box from which configuration files and calibration models will be copied
#' @param Shiny A logical, default is FALSE If TRUE shiny alert message are displayed
#' @param Also.existing.ASE A logical, default is FALSE. The function is run if Also.existing.ASE is TRUE or if the directory New.ASE.Name does not exist
#' @return TRUE if a ASE boxe is created, FALSE otherwise
Create_ASE <- function(DirShiny, Project = "ASE_Boxes", New.ASE.Name, Cloning.path, Shiny = FALSE, Also.existing.ASE = FALSE) {
    if (!dir.exists(file.path(DirShiny, Project, New.ASE.Name)) | Also.existing.ASE) {
        old_ASE_name       <- basename(Cloning.path)
        if (New.ASE.Name != "") {
            New.Dir <- file.path(DirShiny, Project, New.ASE.Name)
            # Creating the the working directory of the AirSensEUR box
            cat("-----------------------------------------------------------------------------------\n")
            futile.logger::flog.info(paste0("[Create_ASE] Creating the directory: ",New.Dir))
            if (!dir.exists(New.Dir)) dir.create(New.Dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
            # Creating File structure
            futile.logger::flog.info(paste0("[Create_ASE] creating the file system for data treatment at ", New.Dir))
            List.Dirs <- c("Calibration","Configuration","Drift","Estimated_coef","General_data","Models","Modelled_gas","Outliers","scriptsLog",
                           "SensorData","Statistics","Verification_plots", "MarkDown")
            for (i in List.Dirs) {
                if (!dir.exists(file.path(New.Dir, i))) {
                    dir.create(file.path(New.Dir, i), showWarnings = TRUE, recursive = TRUE, mode = "0777")
                    futile.logger::flog.info(paste0("[Create_ASE] Dir. created: ", file.path(New.Dir,i)))
                } else futile.logger::flog.info(paste0("[Create_ASE] Dir. already exists: ", file.path(New.Dir,i)))
            }
            # Populating the configuration information with cfg and effect files
            New.File.Conf   <- file.path(New.Dir, "Configuration")
            cfg_Files       <- list.files(path = file.path(Cloning.path), pattern = ".cfg", recursive = TRUE)
            if (any(grepl(pattern= paste(c("Boards.cfg", "Sensors.cfg"), collapse = "|"), cfg_Files))) cfg_Files <- cfg_Files[-grep(pattern= paste(c("Boards.cfg", "Sensors.cfg"), collapse = "|"), cfg_Files)]
            if (length(cfg_Files) > 0) for (i in cfg_Files) {
                futile.logger::flog.info(paste0("[Create_ASE] copying ", basename(i), " at ", New.File.Conf))
                file.copy(from = file.path(Cloning.path,i), 
                          to = file.path(New.Dir, "Configuration",gsub(pattern = old_ASE_name, replacement = New.ASE.Name, basename(i))),
                          overwrite = TRUE, copy.mode = TRUE, copy.date = FALSE)}
            # Update xx_Server.cfg with name of the new ASE box
            File_Server_cfg <- file.path(New.File.Conf, paste0(New.ASE.Name,"_Servers.cfg"))
            cfg <- data.table::transpose(data.table::fread(file = File_Server_cfg, header = FALSE, na.strings=c("","NA", "<NA>")), fill = NA, make.names = 1)
            data.table::set(cfg, j = "AirSensEur.name", value = New.ASE.Name)
            data.table::set(cfg, j = "Dataset"        , value = paste0("ASE",New.ASE.Name))
            data.table::fwrite(data.table::setDT(as.data.frame(t(cfg)), keep.rownames = "name.gas")[], file = File_Server_cfg, row.names = FALSE,col.names = FALSE)
            # copy models
            File_cfg <- file.path(New.File.Conf, paste0(New.ASE.Name,".cfg"))
            if (file.exists(File_cfg)) {
                sens2ref     <- data.table::transpose(data.table::fread(file = File_cfg, header = FALSE, na.strings = c("","NA", "<NA>"), fill = TRUE), fill = NA, make.names = 1)
                New.File.Mod <- file.path(New.Dir, "Models")
                List.Models  <- sens2ref$Cal.func
                for (i in seq(List.Models)) {
                    Cal.func <- paste0(old_ASE_name,"__",sens2ref$name.sensor[i],"__",List.Models[i])
                    if (file.exists(file.path(Cloning.path, "Models", Cal.func))) {
                        futile.logger::flog.info(paste0("[Create_ASE] copying ", List.Models[i], " to ", New.File.Mod))
                        file.copy(from = file.path(Cloning.path, "Models", Cal.func), 
                                  to   = file.path(New.File.Mod, gsub(pattern = old_ASE_name, replacement = New.ASE.Name, Cal.func)),
                                  overwrite = TRUE, copy.mode = TRUE, copy.date = FALSE)}}}
            # Returning success
            return(TRUE)
        } else if (Shiny) {
            shinyalert(
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
            return(FALSE)
        } else {
            futile.logger::flog.warn(paste0("[Create_ASE] AirSensEUR box name cannot be empty"))
            return(FALSE)} 
    } else if (Shiny) {
        shinyalert(
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
        return(FALSE)
    } else {
        futile.logger::flog.warn(paste0("[Create_ASE] ASE box already exist. ", New.ASE.Name))
        return(FALSE)}}

# Plot_ASE_reg: Plotting regression, Uncertainty and time-series for the report of calibration of AQSens - SensEURCity ====
#' Plotting regression, Uncertainty, time-series and Target diagrams for the report of calibration of AQSens - SensEURCity
#' 
#' @description 
#' Create a sheet report with title, authors, date, TOC, Plots of regressions and Target diagram
#' @param Compound charactervector : Name of Gas Compound: CO, NO, NO2...
#' @Intrument : name of reference analyser used to determine UbsRM. Default is NULL
#' @param ASE.ID object returned by Identify_ASE or Identifiy_ASE_Dir
#' @param U.orth.List object returned by U_orth
#' @param ubsRM numeric. Random standard uncertainty of the results of the reference method, xis, given as a constant value for all xis reference values
#' @param ubss numeric. Random standard uncertainty of the sensor, yis, given as a constant value for all yis sensor values
#' @param Add.Ubss Logical, default is FALSE. If TRUE Ubss is added into the the y-axis label
#' @param Add.Target Logical, default is FALSE. If TRUE a Target Digram is added.
#' @param Ref_Analysers name of reference analyser, default is NULL. If not null the name is added in Target_diagram
#' @return nothing
t_col <- function(color, percent = 50, name = NULL) {
    # https://www.dataanalytics.org.uk/make-transparent-colors-in-r/
    #    color = color name
    #    percent = % transparency
    #    name = an optional name for the color
    
    ## Get RGB values for named color
    rgb.val <- col2rgb(color)
    
    ## Make new color using input color as base and alpha set by transparency
    t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
                 max = 255,
                 alpha = (100 - percent) * 255 / 100,
                 names = name)
    
    ## Save the color
    invisible(t.col)
}
Plot_ASE_reg <- function(Compound = NULL, ASE.ID, U.orth.List, UbsRM, Ubss, Add.Ubss = FALSE, Add.Target = FALSE, Ref_Analysers = NULL,
                         Target.for = c(c("CO_A4_P1","D300","NO_B4_P1", "NO2_B43F_P1", "OX_A431_P1", "5310CAT", "5325CAT", "5301CAT", "5310CST", "5325CST", "5301CST", "OPCN3PM10", "OPCN3PM25", "OPCN3PM1"))) {
    if (Add.Target) layout(mat = matrix(c(1,2,3,3,4,4), 3, 2, byrow = TRUE)) else layout(mat = matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
    
    # Scatterplot
    # saving the original graphical parameters
    op <- par(no.readonly = TRUE)
    # Restoring graphical parameters on exit of function, even if an error occurs
    on.exit(par(op)) # it reset the par(mfrow) allways plotting on the upper left plot
    par(mar = c(2.3,2.4,1.5,0.1))
    par(mgp = c(1.5,0.3,0))
    
    Xlab <- paste0(Compound, " reference measurements in ", ASE.ID$unit.ref)
    if (!is.null(Ref_Analysers)) Xlab <- paste0(Xlab, ", ",Ref_Analysers)
    Ylab <- paste0("ASE, ", ASE.ID$ASE.name, ", sensor: ", ASE.ID$name.sensor,", ", ASE.ID$unit.sensor)
    Ylim <- c(min(c(U.orth.List$Mat$xis, U.orth.List$Mat$yis), na.rm = T), max(c(U.orth.List$Mat$xis, U.orth.List$Mat$yis), na.rm = T)) # U.orth.List_Orth$Mat$U,U.orth.List_Deming$Mat$U,
    {plot(U.orth.List$Mat[order(xis),.(xis,yis)], type = "p", xlab = Xlab, ylab = Ylab, xlim = Ylim, ylim = Ylim, col = "blue")
        abline(a = unname(U.orth.List$b0), b = unname(U.orth.List$b1), col = "red")
        mtext(paste0(U.orth.List$Regression,": y = ", format(U.orth.List$b0, digits = 3), " + ", format(U.orth.List$b1, digits = 3), " x, R2 = ", format(U.orth.List$Correlation^2, digits = 2)), line = 0, adj = 1, col = "red")
        grid()}
    
    # Uncertainty
    Xlab <- paste0(Compound, " reference measurements, u(bs,RM)= ", format(UbsRM, digit = 1), " ", ASE.ID$unit.ref)
    if (!is.null(Ref_Analysers)) Xlab <- paste0(Xlab, ", ",Ref_Analysers)
    Ylab <- "ASE expanded uncertainty, "
    if (Add.Ubss) Ylab <- paste0(Ylab, "u(bs,s) = ", format(Ubss, digits = 1), " ", ASE.ID$unit.sensor) else Ylab <- paste0(Ylab, ASE.ID$unit.ref)
    Xlim <- c(min(U.orth.List$Mat$xis, na.rm = T), max(U.orth.List$Mat$xis, na.rm = T)) # U.orth.List_Orth$Mat$U,U.orth.List_Deming$Mat$U,
    Ylim <- c(0, max(U.orth.List$Mat$U, na.rm = T)) # U.orth.List_Orth$Mat$U,U.orth.List_Deming$Mat$U,
    # browser()
    plot(U.orth.List$Mat[order(xis),.(xis,U)], type =  "l",xlab = Xlab, ylab = Ylab, xlim = Xlim, ylim = Ylim, col = "blue")
    grid()
    
    # Time-series
    par(mar = c(1.7,2.4,0.5,0.1))
    Ylab <- paste0("ASE in blue, reference in red, ", ASE.ID$unit.sensor)
    Ylim <- c(min(U.orth.List$Mat[, .(xis, yis)], na.rm = T), max(U.orth.List$Mat[, .(xis, yis)], na.rm = T))
    plot(U.orth.List$Mat[order(Date),.(Date,xis)], type = "l", col = "red", ylab = Ylab, ylim = Ylim)
    lines(U.orth.List$Mat[order(Date),.(Date,yis)], col = "blue")
    grid()
    # Adding calibration period 7 cases : the 2 ones with sensor data outside Cal_interval means nothing is plotted
    if (!(ASE.ID$Cal.DateIN > max(U.orth.List$Mat$Date, na.rm = T) || ASE.ID$Cal.DateEND < min(U.orth.List$Mat$Date, na.rm = T)) ) {
        # Cal_interval within sensor data 
        if (ASE.ID$Cal.DateIN > min(U.orth.List$Mat$Date, na.rm = T)) {
            Ind.CalIN <- which(U.orth.List$Mat[, Date] == min(U.orth.List$Mat[Date > ASE.ID$Cal.DateIN, Date], na.rm = T))[1]
            if (ASE.ID$Cal.DateEND < max(U.orth.List$Mat$Date, na.rm = T)) {
                Ind.CalEND <- which(U.orth.List$Mat[, Date] == max(U.orth.List$Mat[Date <= as.POSIXct(ASE.ID$Cal.DateEND) + 1, Date], na.rm = T))[1]
            } else {
                Ind.CalEND <- which(U.orth.List$Mat[, Date] == max(U.orth.List$Mat[, Date], na.rm = T))[1]
            }
        } else { 
            Ind.CalIN <- which(U.orth.List$Mat$Date == min(U.orth.List$Mat$Date, na.rm = T))[1]
            if (ASE.ID$Cal.DateEND < max(U.orth.List$Mat$Date, na.rm = T)){
                Ind.CalEND <-nrow(U.orth.List$Mat)
            } else {
                Ind.CalEND <- which(U.orth.List$Mat$Date <= as.POSIXct(ASE.ID$Cal.DateEND) + 1)
                Ind.CalEND <- Ind.CalEND[length(Ind.CalEND)]
            } }
        polygon(x = c(U.orth.List$Mat$Date[c(Ind.CalIN,Ind.CalEND)],U.orth.List$Mat$Date[Ind.CalEND],U.orth.List$Mat$Date[Ind.CalIN]), 
                y = c(rep(par()$usr[4], 2 ), par()$usr[3], par()$usr[3]), 
                fillOddEven = T,
                col =  t_col("pink", perc = 75, name = "lt.pink"), 
                lty = 1, lwd = 2, border = "black")}
    
    # Target Diagram
    if (Add.Target) {
        if (ASE.ID$name.sensor %in% Target.for) {
            if (class(U.orth.List) == "character") {
                plot.new()
            } else {
                par(mar = c(5.1, 4.1, 4.1, 2.1))
                # Plotting Target.diagram
                DQO <- get.DQO(gas.sensor = ASE.ID$gas.sensor, unit.ref = ASE.ID$unit.ref)
                Target.return <- Target.Diagram(Sensor_name = ASE.ID$name.sensor,
                                                Mat         = U.orth.List[["Mat"]],
                                                ubsRM       = UbsRM, #UbsRM(name.sensor= NULL, ubsRM = as.numeric(ASE.ID$ubsRM)),
                                                ubss        = Ubss,
                                                Unit.Ref    = ASE.ID$unit.ref,
                                                Unit.sensor = ASE.ID$unit.sensor,
                                                b0          = U.orth.List[["b0"]],
                                                b1          = U.orth.List[["b1"]],
                                                xAxisLabel  = NULL,
                                                yAxisLabel  = NULL,
                                                DQO.1       = DQO$DQO.1 / DQO$LV, DQO.2 = DQO$DQO.2 / DQO$LV, DQO.3 = DQO$DQO.3 / DQO$LV,
                                                LAT         = DQO$LAT, UAT = DQO$UAT, LV = DQO$LV, AT = DQO$AT,
                                                sdm_sdo     = U.orth.List[["sdm"]] > U.orth.List[["sdo"]],
                                                Model.used  = ASE.ID$Config$sens2ref$Cal.func[which(ASE.ID$Config$sens2ref$name.sensor == ASE.ID$name.sensor)],
                                                BeginEnd    = NULL, #c(format(ASE.ID$Meas.DateIN, "%Y%m%d"), format(ASE.ID$Meas.DateEND, "%Y%m%d")),
                                                with.ubss   = FALSE,
                                                Ref_Analysers = Ref_Analysers)
                if (!is.null(Target.return)) plot.new()}
            cat("\n")
        } else{
            futile.logger::flog.warn(paste0("Sensor: ",name.sensor, " no DQO defined for this sensor"))
            plot.new()}
    }
    cat("\n")}
# Compound: Name of compound determined using sensor name ====
#'Name of compound determined using sensor name
#' 
#' @description 
#' Stop code if name.sensor is unknown
#' @param name.sensor character, default is NULL. Name of gas sensor: "D300", "CO_A4_P1", "NO_B4_P1"...
#' @return a character vector as "CO2", "CO", "NO", "NO2"...
Compound <- function(name.sensor) {
    if (name.sensor %in% c("D300", "CO_A4_P1", "NO_B4_P1", "NO2_B43F_P1", "OX_A431_P1", "OPCN3PM10", "OPCN3PM25", "OPCN3PM1", "5310CAT", "5310CST", "5325CAT", "5325CST", "5301CAT", "5301CST")) {
        return(switch(name.sensor, 
                      "D300"        = "CO2",
                      "CO_A4_P1"    = "CO" , "NO_B4_P1"    = "NO", 
                      "NO2_B43F_P1" = "NO2", "OX_A431_P1"  = "O3",
                      "OPCN3PM10"   = "PM10" , "OPCN3PM25"   = "PM2.5", "OPCN3PM1"   = "PM1",
                      "5310CAT"     = "PM10" , "5310CST"     = "PM10",
                      "5325CAT"     = "PM2.5", "5325CST"     = "PM2.5",
                      "5301CAT"     = "PM1"  , "5301CST"     = "PM1"))
    } else stop(paste0("[Compound] unknown sensor ", name.sensor))}
# Ubss: Random standard uncertainty of the sensor ====
#' Random standard uncertainty of the sensor
#' 
#' @param name.sensor character, default is NULL. Name of gas sensor: "D300", "CO_A4_P1", "NO_B4_P1"...
#' @param ubss numeric default is NULL. If not NULL, ubss is returned. ubss shall be the random standard uncertainty of the sensor
#' @return numeric value u(bs,s)
Ubss <- function(name.sensor=NULL, ubss = NULL) {
    if (is.null(ubss)) {
        if (name.sensor %in% c("D300", "CO_A4_P1", "NO_B4_P1", "NO2_B43F_P1", "OX_A431_P1",
                               "S30PM01", "S30PM25", "S30PM10", "NPMPM01", "NPMPM25", "NPMPM10",
                               "OPCN3PM10", "OPCN3PM25", "OPCN3PM1", "5310CAT", "5310CST", "5325CAT", "5325CST", "5301CAT", "5301CST", "SHT31TE", "SHT31HE")) {
            return(switch(name.sensor, 
                          "D300"        = 50,
                          "CO_A4_P1"    = 0.3, "NO_B4_P1"    = 4, 
                          "NO2_B43F_P1" = 4, "OX_A431_P1"    = 6,
                          "5301CAT"     = 3  , "5301CST"     = 3, "S30PM01" =3, "NPMPM01" =3,
                          "5325CAT"     = 3  , "5325CST"     = 3, "S30PM25" =3, "NPMPM25" =3,
                          "5310CAT"     = 3  , "5310CST"     = 3, "S30PM10" =3, "NPMPM10" =3,
                          "OPCN3PM10"   = 3  , "OPCN3PM25"   = 3, 
                          "OPCN3PM1"    = 3  , "SHT31TE"     = 2,
                          "SHT31HE"     =2))
        } else stop(paste0("[Ubss] unknown sensor ", name.sensor))
    } else return(ubss)}
# Ubss: Random standard uncertainty of the results of the reference method ====
#' Random standard uncertainty of the results of the reference method
#' 
#' @param name.sensor character, default is NULL. Name of gas sensor: "D300", "CO_A4_P1", "NO_B4_P1"...
#' @param ubsRM numeric default is NULL. If not NULL, ubsRM is returned. ubsRM shall be the Random standard uncertainty of the reference method
#' @return numeric value u(bs,s)
#' return variable u(bs,RM), the uncertainty of the reference analyser, calculated using slope and intercept
UbsRM_Expert <- function(name.sensor, ubsRM = NULL) {
    if (is.null(ubsRM)) {
        if (name.sensor %in% c("SHT31HE", "SHT31TE", "CO_A4_P1")){
            Slope.UbsRM      <- 0.01
            Intercep.UbsRM   <- 0.020
        } else if (name.sensor %in% c("NO_B4_P1", "NO2_B43F_P1", "OX_A431_P1")){
            Slope.UbsRM      <- 0.01
            Intercep.UbsRM <- 0.5
        } else if (name.sensor %in% c("5310CAT", "5325CAT", "5301CAT", "5310CST", "5325CST", "5301CST", "OPCN3PM10", "OPCN3PM25", "OPCN3PM1", "S30PM01", "S30PM25", "S30PM10","NPMPM01", "NPMPM25", "NPMPM10")){
            Slope.UbsRM      <- 0.02
            Intercep.UbsRM  <- 0.5 
        } else if (name.sensor == "D300"){
            Slope.UbsRM      <- 0.00
            Intercep.UbsRM  <- 6}
        
        if (!name.sensor %in% c("D300", "CO_A4_P1", "NO_B4_P1", "NO2_B43F_P1", "OX_A431_P1", "OPCN3PM10", "OPCN3PM25", "OPCN3PM1", 
                                "5310CAT", "5310CST", "5325CAT", "5325CST", "5301CAT", "S30PM01", "S30PM25", "S30PM10", "5301CST","NPMPM01", "NPMPM25", "NPMPM10", "SHT31HE", "SHT31TE")) {
            stop(paste0("[UbsRM] unknown sensor ", name.sensor))
        } else return(list(Slope.UbsRM = Slope.UbsRM, Intercep.UbsRM = Intercep.UbsRM))}}
#' return u(bs,RM) the uncertainty of the reference analyser
UbsRM <- function(name.sensor, ubsRM = NULL) {
    if (is.null(ubsRM)) {
        if (name.sensor %in% c("D300", "CO_A4_P1", "NO_B4_P1", "NO2_B43F_P1", "OX_A431_P1", "OPCN3PM10", "OPCN3PM25", "OPCN3PM1", "5310CAT", "5310CST", "5325CAT", "5325CST", "5301CAT", "5301CST")) {
            return(switch(name.sensor, 
                          "D300"        = 6,
                          "CO_A4_P1"    = 0.91/1.16, "NO_B4_P1"    = 0.35/1.34, 
                          "NO2_B43F_P1" = 1.18/1.91, "OX_A431_P1"  = 1.10/2.05,
                          "5301CAT"     = 2.2      , "5301CST"     = 2.2,
                          "5310CAT"     = 2.2      , "5310CST"     = 2.2,
                          "5325CAT"     = 2.2      , "5325CST"     = 2.2,
                          "OPCN3PM10"   = 2.2      , "OPCN3PM25"   = 2.2, "OPCN3PM1"   = 2.2))
        } else stop(paste0("[UbsRM] unknown sensor ", name.sensor))
    } else return(ubsRM)}
#' return u(bs,RM) the uncertainty of the reference analyser
UbsRM_WG42 <- function( Instrument, name.sensor = NULL,Unit.ppb = FALSE) {
    # Values in approval tets https://qal1.de/en/index.htm
    # Checking if it is a NOx instrument for which u(bs,RM) exists both for NO and NO2
    if (Instrument %in% c("APNA_370", "42i")) {
        if (is.null(name.sensor)) {
            return(flog.error("[UbsRM_WG42] for NO and NO2, the name of sensor is needed. Cannot return u(bs,RM) if the name of sensor is unknown"))
        } else {
            Pollutant <- Compound(name.sensor)
            ubsRM <- switch(Pollutant,
                            "NO2" = switch(Instrument,
                                           "APNA_370"    = 1.26,
                                           "42i"         = 1.18),
                            "NO"  = switch(Instrument,
                                           "APNA_370"     = 1.33,
                                           "42i"          = 0.35))}
    } else if (Instrument %in% c("FIDAS", "GRIMM", "TOPAS", "TEOM", "Leckel", "LVS", "ESM.FH.62", "BAM")) {
        if (is.null(name.sensor)) {
            return(flog.error("[UbsRM_WG42] for PM, the name of sensor is needed. Cannot return u(bs,RM) if the name of sensor is unknown"))
        } else {
            Pollutant <- Compound(name.sensor)
            ubsRM <- switch(Pollutant,
                            "PM10"  = switch(Instrument,
                                             "GRIMM"                = 2.09,  # p56,  http://expo.fmi.fi/aqes/public/PM_Equivalence%20report%20Kuopio_2017.pdf
                                             "BAM"                  = 1.32,  # p53,  http://expo.fmi.fi/aqes/public/PM_Equivalence%20report%20Kuopio_2017.pdf
                                             "FIDAS"                = 1.35,  # at  50 table 27 p 120 https://qal1.de/report/0000040212_21227195C_palas_Fidas200S_en.pdf
                                             "TEOM"                 = 1.42,  # at 50, page 100 https://qal1.de/report/0000035016_21209885B_thermo_Teom1405f_pm10_en.pdf),
                                             "Leckel"               = 0.9,   # p15, Evaluation of the Field Comparison Exercise for PM10 and PM2.5 Ispra, 18 January to 14 March 2018, EUR 29939 EN 
                                             "LVS"                  = 0.9,   # as Leckel?
                                             "ESM.FH.62"            = 2.7),  # p55, http://expo.fmi.fi/aqes/public/PM_Equivalence%20report%20Kuopio_2017.pdf
                            "PM2.5" = switch(Instrument,
                                             "GRIMM"                = 0.60, # p60,  http://expo.fmi.fi/aqes/public/PM_Equivalence%20report%20Kuopio_2017.pdf
                                             "BAM"                  = 0.64, # p67,  http://expo.fmi.fi/aqes/public/PM_Equivalence%20report%20Kuopio_2017.pdf
                                             "FIDAS"                = 0.8,  # at 25 table 27 p 120 https://qal1.de/report/0000040212_21227195C_palas_Fidas200S_en.pdf
                                             "TEOM"                 = 0.7,  # at 25 p 9 p 120 https://qal1.de/report/0000036949_21209885C_thermo_Teom1405f_pm25_en.pdf
                                             "Leckel"               = 0.8,  # p15, Evaluation of the Field Comparison Exercise for PM10 and PM2.5 Ispra, 18 January to 14 March 2018, EUR 29939 EN
                                             "ESM.FH.62"            = 1.07, # p69, http://expo.fmi.fi/aqes/public/PM_Equivalence%20report%20Kuopio_2017.pdf
                                             "LVS"                  = 0.8), # as Leckel?
                            "PM1"  = switch(Instrument,
                                            "FIDAS"                = 0.5))} # Guess
    } else {
        # values in ug.m-3 or mg.m-3
        ubsRM <- switch(Instrument,
                        # NO2
                        "Serinus 40"   = 5.21,
                        "AC32e"        = 0.48,
                        "AC32M"        = 1.47,
                        #"AS32M"        = 0.67,
                        "AR500"        = 0.54,
                        "T200P"        = 0.81,
                        "200E / T200"  = 0.72,
                        "T500U"        = 0.49,
                        "42iQ"         = 0.66,
                        #NO
                        "Model 405 nm" = 1.39,
                        # SO2
                        "Serinus 50"   = 5.08,
                        "AF22e"        = 1.28,
                        "AF22M"        = 0.91,
                        "APSA_370"     = 1.22,
                        "AR500"        = 1.33,
                        "100E / T100"  = 0.55,
                        "43i"          = 1.24,
                        "43iQ"         = 0.42,
                        # O3
                        "Serinus 10"   = 2.44,
                        "O342e"        = 1.56,
                        "O342M"        = 0.99,
                        "APOA_370"     = 0.41, 
                        "AR500"        = 1.16,
                        "400E / T400"  = 1.35,
                        "49i"          = 1.10,
                        "49iQ"         = 1.37,
                        # CO
                        "Serinus 30"   = 3.51,
                        "CO12e"        = 1.13,
                        "CO12M"        = 0.68,
                        "APMA_370"     = 0.28,
                        "300E / T300"  = 0.76,
                        "48i"          = 0.91,
                        "48iQ"         = 0.45,
                        # Benzene
                        "GC5000 BTX FID"       = 0.07,
                        "GC5000 BTX PID"       = 0.10,
                        "VOC72M"               = 0.02,
                        "GC 955 Model 601 PID" = 0.05,
                        # CO2
                        "Picarro"              = 0.085, # https://amt.copernicus.org/articles/12/5717/2019/
                        "Sick Sidor"           = 10
        )
        if (is.null(ubsRM)) stop(paste0("[UbsRM_WG42] ERROR unknown analyser: ",Instrument))} 
    # Convert to ppb or ppm if requested
    if (Unit.ppb && Compound(ASE.ID$name.sensor) != "CO2") {
        if (Instrument %in% c("Serinus 40", "AC32e", "AC32M", "AR500", "T200P", "200E / T200", "T500U", "42iQ")) {
            Pollutant = "NO2"
        } else if (Instrument %in% c("Model 405 nm")) {
            Pollutant = "NO"
        } else if (Instrument %in% c("Serinus 50", "AF22e", "AF22M", "APSA_370", "AR500", "100E / T100", "43i", "43iQ")) {
            Pollutant = "SO2"
        } else if (Instrument %in% c("Serinus 10", "O342e", "O342M", "APOA_370", "AR500", "400E / T400", "49i", "49iQ")) {
            Pollutant = "O3"
        } else if (Instrument %in% c("Serinus 30", "CO12e", "CO12M", "APMA_370", "300E / T300", "48i", "48iQ")) {
            Pollutant = "CO"
        } else if (Instrument %in% c("GC5000 BTX FID", "VOC72M", "GC 955 Model 601 PID")) {
            Pollutant = "Benzene"}
        Coef.Convert <- switch(Pollutant,
                               "CO"      = 1.16, 
                               #"CO2"     = 1.91 * 44 / 48, 
                               "NO"      = 1.34, 
                               "NO2"     = 1.91, 
                               "O3"      = 2.05,
                               "SO2"     = 2.66,
                               "Benzene" = 3.24)
        ubsRM <- ubsRM / Coef.Convert}
    return(ubsRM)}

# Date_reporting: Date interval for plotting regression, Uncertainty, time-series and Target Diagram with Plot_ASE_reg() and Target.Diagram() ====
#' Date interval for plotting regression, Uncertainty, time-series and Target Diagram with Plot_ASE_reg() and Target.Diagram()
#' 
#' @description 
#' The date Interval can be set in 3 different ways: from the calibration date interval in ASE.ID  (if Objective = "calibration"), from the Measuring interval in ASE.ID (if Objective = "Measuring") or by passing desired date intervals (if Objective = "Free").
#' @param Objective character vector default is "Measuring". Possible values "Measuring","Calibration","Free". If "Measuring", it uses the date interval of Config file for measuring. If "Calibration" it uses the date interval for calibration in config file. If "Free" it uses Date_interval.
#' @param Date_interval vector of 2 character vector representing start and ending dates. Mandatory if Objective == "Free
#' @param ASE.ID object returned by Identify_ASE or Identifiy_ASE_Dir
#' @return a vector of 2 character vectors as POSIXct or Date (format %Y-%m-%d ).
Date_reporting <- function(Objective = "Measuring",  Date_interval = NULL, ASE.ID = NULL){
    if (!Objective %in% c("Measuring","Calibration","Free")) {
        return(futile.logger::flog.error(paste0("[Date_reporting] unknown Objective argument for calibration period: ",Objective)))
    } else if (Objective == "Free" && is.null(Date_interval)) {
        return(futile.logger::flog.error("[Date_reporting] the charter vector of calibration date interval is missing"))
    } else if (Objective == "Measuring" && is.null(ASE.ID)) {
        return(futile.logger::flog.error(paste0("[Date_reporting] unknown Objective argument: ",Objective)))
    } else if (Objective == "Measuring") {
        return(c(ASE.ID$Meas.DateIN, ASE.ID$Meas.DateEND))
    } else if (Objective == "Calibration" && is.null(ASE.ID)) {
        return(futile.logger::flog.error("[Date_reporting] ASE.ID mssing for Objective \"calibration\""))
    } else if (Objective == "Calibration") {
        return(c(ASE.ID$Cal.DateIN, ASE.ID$Cal.DateEND))
    } else if (Objective == "Free" && is.null(Date_interval)) {
        return(futile.logger::flog.error("[Date_reporting] Date_interval mssing for Objective \"Free\""))
    } else if (Objective == "Free") return(c(as.Date(Date_interval[1]), as.Date(Date_interval[2])))}

# Upload2Influx: Upload calibrated data of a list of AirSensEURs to InfluxDB ####
#' Upload calibrated data of a list of AirSensEURs to InfluxDB
#' 
#' @description 
#' The aim of this function is to uploaded calibrated data that are included into the Dir_general/General.csv file. to an InfluxDB server.
#' The function starts by identifying each AirSensEUR box, load existing data and config files. #' Please read description of function Identify_ASE() in Function4ASE.R.
#' All data are averaged according to UserMinsAvg in ASE_Server.cfg.
#' Then it extract the name of all sensors that have been calibrated and it adds "Temperature", "Relative_humidity" and "Atmospheric_pressure".
#' It also adds the raw data of the carbon dioxide sensor if this one has not been calibrated
#' 
#' The main purpose of the function is to create a data.table with the following columns:
#' "time", "ID_ASE", "latitude"(decimal degrees), "longitude"(decimal degrees), "variable", "name.sensor", "serial","value" and "Sens.unit"
#' in long format using "time" (UTC time), "latitude"(decimal degrees), "longitude"(decimal degrees), "variable" (pollutant name) and"value" (calibrated data) in file "General.csv" in DIR_General and
#' "name.sensor" (brand names) and "Sens.unit" in file "ASE.cfg" file in DIR_Config and 
#' "serial" in file "Sensors.cfg" in DIR_Config. The part numbers of sensors are correctly referred when a sensor changeover occurs.
#' Another data.table is created: JSON.File, whose format is consistent to post data to InfluxDB
#' 
#' The Upload2Influx connects to the InfluxDB server defined into ASE_Server.cfg using Proxy if defined in ASE_Server.cfg
#' with credentials and within the defined database in the config file. The credentials may be only for reading data
#' It checks if a Table (Measurements in influxDB vocabulary) ASE_Calibrated exists.
#' If it does not exists all data are uploaded in InfluxDB.
#' If it exists the file only new or modified (re calibrated) data are uploaded
#' 
#' Finally 2 files are saved:
#' - DIR_Results/ASE_Calibrated.csv, a copy of the data.table described above (to be made available to stackeholder)
#' - DIR_General/JSON_File.csv, a copy of the JSON uploaded to the Influxdb.
#' 
#' @param WD character vector, file root where the shiny App is stored.
#' @param Project character vector, default is "ASE_Boxes". It sets a directory that shall exist at file.path(WD) in which all sensor boxe data are stored
#' @param List.ASE character vector. They correspond to directories that shall exist at file.path(WD, Project)
#' @param DIR_Config character vector, default is "General_data". It sets a sub-directory that shall exist at file.path(WD, Project, ASE) where is the file ASE.name.cfg
#' @param DIR_General character vector, default is "General_data". It sets a sub-directory that shall exist at file.path(WD, Project, ASE) where is the file General.csv
#' @param DIR_Results character vector, default is "Results". It sets a sub-directory where a csv file of calibrated results is stored. DIR_Results is placed under Project
#' @param User character vector, user with write credential in the InfluxDB wher the upload is performed
#' @param Pass character vector, password of User with write credential in the InfluxDB wher the upload is performed
#' @param General.DT A data.table with all ASE box data. Default is null. If NULL the General.csv file is loaded
#' @param ASE.cfg A data.table with all ASE box configuration. Default is null. If NULL the ASE.cfg file is loaded
#' @param SetTime A data.table with all ASE box SetTime configuration. Default is null. If NULL the ASE_SETTIME.cfg file is loaded, the shape shall be in the shape of function Identify_ASE_Dir
#' @param Config A List, default is null, returned by function CONFIG(). If NULL, Config is loaded using function Identify_ASE_Dir().
#' @param Only.from.Valid.IN Logical, default is TRUE. If TRUE only data after the minimum Valid.IN date are uuploaded
#' @param Ref_Analysers data.table with 2 columns (variable and name.sensor) giving pollutants and reference analysers, see default with function header
#' where the files paste0("ASE",ASE.ID.init$ASE.name,"_calibrated.csv") and "JSON_file.csv".
#' @return Nothing but check console messsage, use sink() if needed to create a log.
Upload2Influx <- function(WD, Project, List.ASE, DIR_Config = "Configuration", DIR_General = "General_data", DIR_Results = "Results", User, Pass, 
                          General.DT = NULL, ASE.cfg = NULL, SetTime = NULL, Config = NULL, Shield = NULL, Only.from.Valid.IN = TRUE,
                          Ref_Analysers = data.table::data.table(variable    = c("Ref.CO_ppm", "Ref.NO", "Ref.NO2", "Ref.NOx","Ref.O3", "Ref.SO2", "Ref.CH4"    , "Ref.CO2"    , "Ref.PM10", "Ref.PM2.5", "Ref.PM1","Ref.PM1.Fidas","Ref.PM10.Fidas","Ref.PM2.5.Fidas","Ref.Temp", "Ref.RH"),
                                                                 name.sensor = c("APMA-370"  , "42i"   , "42i"    , "42i"    ,"49i"   , "43i"    , "Picarro_CH4", "Picarro_CO2",    "FIDAS",     "FIDAS",   "FIDAS",        "FIDAS",         "FIDAS",          "FIDAS",   "FIDAS",  "FIDAS")), ...) {
    Return.list <- list()
    arguments <- list(...)
    if ("Filter.Ref" %in% names(arguments)) Filter.Ref <- arguments$Filter.Ref else Filter.Ref <- TRUE
    for (ASE in List.ASE) {
        cat("===============================\n")
        futile.logger::flog.info(paste0("[Upload2Influx] ", ASE, " starting uploading calibrated data of ASE "))
        
        # Initial identifying characteristic of sensors ----
        ASEDir      <- file.path(WD, Project, ASE)
        if (!dir.exists(ASEDir)) {
            futile.logger::flog.warn(paste0("[Upload2Influx] ", ASE, " directory ", ASEDir," does not exist. Going to next ASE."))
            next}
        ASE.ID.init <- Identify_ASE_Dir(ASEDir = ASEDir, DIR_Config = DIR_Config, DIR_General = DIR_General, Select.Columns = T,
                                        General.DT = General.DT, ASE.cfg = ASE.cfg, SetTime = SetTime, Config = Config, Shield = Shield, Filter.Ref = Filter.Ref)
        
        # Selecting data only from smaller Valid.IN
        if (Only.from.Valid.IN) {
            # Looking for the minimum Valid.IN
            Set.Time <- data.table::transpose(ASE.ID.init$SetTime, make.names = "name.gas")
            min.Valid.IN <- Set.Time$Valid.IN
            if (!lubridate::is.Date(min.Valid.IN) || !lubridate::is.POSIXct(min.Valid.IN)) min.Valid.IN <- as.POSIXct(min.Valid.IN, tz = ASE.ID.init$Config$Server$Influx.TZ, tryFormats = c("%Y-%m-%d %H:%M:%OS","%Y-%m-%d"))
            min.Valid.IN <- min(min.Valid.IN, na.rm = T)
            if (min.Valid.IN <= ASE.ID.init$General.DT[.N]$date){
                ASE.ID.init$General.DT <- ASE.ID.init$General.DT[date >= min.Valid.IN]}}
        
        # Info on Modeled Pollutant from ASE.cfg
        Modelled.Pollutants <- grep("_modelled", names(ASE.ID.init$General.DT), value = T)
        # Adding Temperature, Relative humidity, Atmospheric_pressure and Carbon_dioxide if they are not within the modeled variables
        for (i in c("Temperature", "Relative_humidity", "Atmospheric_pressure")) {
            if (!any(grepl(pattern = i, Modelled.Pollutants)) && paste0("Out." , i) %in% names(ASE.ID.init$General.DT)) Modelled.Pollutants <- c(Modelled.Pollutants, paste0("Out." , i))}
        if (!any(grepl(pattern = "Carbon_dioxide", Modelled.Pollutants)) && "Out.D300_volt" %in% names(ASE.ID.init$General.DT)) Modelled.Pollutants <- c(Modelled.Pollutants, "Out.D300_volt")
        # Determining columns in ASE.cfg
        Gas.sensors  <- gsub(pattern = paste(c("_modelled","Out."), collapse = "|"), replacement = "", Modelled.Pollutants)
        Col.modelled <- c(1,which(ASE.ID.init$ASE.cfg[ASE.ID.init$ASE.cfg$name.gas == "gas.sensor"] %in% Gas.sensors))
        
        # Adding reference data names without replicate filtered columns (.1,.2,), DeLagging reference if requested
        if (Filter.Ref) {
            Ref.Pollutants.val <- grep("Out.Ref.", names(ASE.ID.init$General.DT), value = T)
            Ref.Pollutants.val <- grep("\\.[1234]",Ref.Pollutants.val, value = T, invert = T)
            # Keeping only the reference with data
            Ref.Pollutants.val <- Ref.Pollutants.val[sapply(Ref.Pollutants.val, function(Ref) length(which(is.finite(ASE.ID.init$General.DT[[Ref]]))) > 0)]
            Ref.Pollutants     <- gsub(pattern = paste(c("Out."), collapse = "|"), replacement = "", Ref.Pollutants.val)
            Col.References     <- c(1,which(ASE.ID.init$ASE.cfg[ASE.ID.init$ASE.cfg$name.gas == "gas.reference2use"] %in% Ref.Pollutants))
            
            # DeLaging reference by comparison of the 1st sensor using the reference name, new Delag when Ref is missing for more than 3 hours
            for(Reference in Ref.Pollutants){
                futile.logger::flog.info(paste0("[Upload2Influx] delagging ", Reference))
                # 1st sensor using reference
                k <- which(Reference == Config$sens2ref$gas.reference2use)[1]
                Sync.Pred <- Config$sens2ref$Sync.Pred[k]
                Sync.Cal  <- Config$sens2ref$Sync.Cal[k]
                ColRef    <- paste0("Out.", Reference)
                ColSens   <- paste0(Config$sens2ref$gas.sensor[k], "_modelled")
                
                # Delaggging
                if(Sync.Pred && Reference %in% Config$sens2ref$gas.reference2use && all(c(ColRef,ColSens) %in% names(ASE.ID.init$General.DT))){
                    
                    # detecting the stops of Reference for at least 3 hours
                    Essai <- ASE.ID.init$General.DT[is.finite(ASE.ID.init$General.DT[[Reference]]),.SD,.SDcols = c("date",Reference)][which(diff(ASE.ID.init$General.DT[is.finite(ASE.ID.init$General.DT[[Reference]])][["date"]])>180)]
                    if(nrow(Essai) > 0){
                        Essai[, Diff := diff(ASE.ID.init$General.DT[is.finite(ASE.ID.init$General.DT[[Reference]])][["date"]])[which(diff(ASE.ID.init$General.DT[is.finite(ASE.ID.init$General.DT[[Reference]])][["date"]])>180)]]
                        Essai[, Index := match(date, ASE.ID.init$General.DT$date)]
                        
                        for(Period in seq_along(Essai$date)){
                            #if(Reference == "Ref.PM1.Fidas" && Period ==4) browser()
                            #if(Reference == "Ref.Press"     && Period ==6) browser()
                            if(Period == 1){# 1st period
                                Start.Period <-  max(lubridate::ymd(ASE.ID.init$Meas.DateIN, tz = Config$Server$Influx.TZ), min(ASE.Download$data$date, na.rm =T), na.rm = T)
                            } else { # period between 2 and nrow
                                Start.Period <-  ASE.ID.init$General.DT$date[Essai$Index[Period - 1] + 1]}
                            futile.logger::flog.info(paste0("[Upload2Influx] delagging between ", Start.Period, " and ", Essai$date[Period]))
                            ASE.ID.init$General.DT <- Var_Delag_Pred(DT.General = ASE.ID.init$General.DT, ColRef = ColRef, ColSens = ColSens, 
                                                                     ColSensRaw = Config$sens2ref$gas.sensor[k], ColRefRaw = Reference,
                                                                     Meas.IN = Start.Period, Meas.END = Essai$date[Period], 
                                                                     Cal.IN = ASE.ID.init$Cal.DateIN, Cal.END = ASE.ID.init$Cal.DateEND, 
                                                                     Sync.Pred = Sync.Pred, Sync.Cal = Sync.Cal)
                            # ASE.ID.init$General.DT <- DeLag_Pred(DT.General = ASE.ID.init$General.DT, ColRef = ColRef, ColSens = ColSens, 
                            #                                      Meas.DateIN = Start.Period,
                            #                                      Meas.DateEND = Essai$date[Period], 
                            #                                      Cal.DateIN = ASE.ID.init$Cal.DateIN, Cal.DateEND = ASE.ID.init$Cal.DateEND, 
                            #                                      Sync.Pred = Sync.Pred, Sync.Cal = Sync.Cal, ASE.name = ASE.ID.init$ASE.name)
                            if(Period == nrow(Essai)){ # Completing from last period to end of dates
                                futile.logger::flog.info(paste0("[Upload2Influx] delagging between ", ASE.ID.init$General.DT$date[Essai$Index[Period] + 1], " and ",
                                                                min(lubridate::ymd(ASE.ID.init$Meas.DateEND+1, tz = Config$Server$Influx.TZ), max(ASE.Download$data$date, na.rm =T), na.rm = T)))
                                ASE.ID.init$General.DT <- Var_Delag_Pred(DT.General   = ASE.ID.init$General.DT, ColRef = ColRef, ColSens = ColSens, 
                                                                         ColSensRaw = Config$sens2ref$gas.sensor[k], ColRefRaw = Reference,
                                                                         Meas.IN  = ASE.ID.init$General.DT$date[Essai$Index[Period] + 1],
                                                                         Meas.END = min(lubridate::ymd(ASE.ID.init$Meas.DateEND+1, tz = Config$Server$Influx.TZ), max(ASE.Download$data$date, na.rm =T), na.rm = T), 
                                                                         Cal.IN   = ASE.ID.init$Cal.DateIN, Cal.DateEND = ASE.ID.init$Cal.DateEND, 
                                                                         Sync.Pred    = Sync.Pred, Sync.Cal = Sync.Cal)}
                            # ASE.ID.init$General.DT <- DeLag_Pred(DT.General   = ASE.ID.init$General.DT, ColRef = ColRef, ColSens = ColSens, 
                            #                                          Meas.DateIN  = ASE.ID.init$General.DT$date[Essai$Index[Period] + 1],
                            #                                          Meas.DateEND = min(lubridate::ymd(ASE.ID.init$Meas.DateEND+1, tz = Config$Server$Influx.TZ), max(ASE.Download$data$date, na.rm =T), na.rm = T), 
                            #                                          Cal.DateIN   = ASE.ID.init$Cal.DateIN, Cal.DateEND = ASE.ID.init$Cal.DateEND, 
                            #                                          Sync.Pred    = Sync.Pred, Sync.Cal = Sync.Cal, ASE.name = ASE.ID.init$ASE.name)}
                        }
                    } else {
                        ASE.ID.init$General.DT <- Var_Delag_Pred(DT.General = ASE.ID.init$General.DT, ColRef = ColRef, ColSens = ColSens,
                                                                 ColSensRaw = Config$sens2ref$gas.sensor[k], ColRefRaw = Reference,
                                                                 Meas.IN = ASE.ID.init$Meas.DateIN, Meas.END = ASE.ID.init$Meas.DateEND, Cal.IN = ASE.ID.init$Cal.DateIN, Cal.END = ASE.ID.init$Cal.DateEND, 
                                                                 Sync.Pred = Sync.Pred, Sync.Cal = Sync.Cal)}}}}
        # ASE.ID.init$General.DT <- DeLag_Pred(DT.General = ASE.ID.init$General.DT, ColRef = ColRef, ColSens = ColSens, 
        #                                                      Meas.DateIN = ASE.ID.init$Meas.DateIN, Meas.DateEND = ASE.ID.init$Meas.DateEND, Cal.DateIN = ASE.ID.init$Cal.DateIN, Cal.DateEND = ASE.ID.init$Cal.DateEND, 
        #                                                      Sync.Pred = Sync.Pred, Sync.Cal = Sync.Cal, ASE.name = ASE.ID.init$ASE.name)}}}}
        
        # Averaging hourly data or other average. Do it only after DeLagging, afterwards it is impossible
        if (ASE.ID.init$Config$Server$UserMins != ASE.ID.init$Config$Server$UserMinsAvg) ASE.ID.init$General.DT <- DF_avg(ASE.ID.init$General.DT, width = ASE.ID.init$Config$Server$UserMinsAvg)
        
        # Melting reference data names without replicate filtered columns (.1,.2,)
        if (Filter.Ref) {
            # data.table, from wide to long format: melting: https://cran.r-project.org/web/packages/data.table/vignettes/datatable-reshape.html
            if (length(Ref.Pollutants) > 0){
                Melted.data.References <- data.table::melt(ASE.ID.init$General.DT, id.vars = c("date", "Ref.Lat", "Ref.Long"), measure.vars = Ref.Pollutants.val)
                # discarding NA
                na.value <- Melted.data.References[is.na(value), which = TRUE]
                if(length(na.value) > 0 ) Melted.data.References <- Melted.data.References[-na.value]
                rm(na.value)}}
        
        # data.table, from wide to long format: melting: https://cran.r-project.org/web/packages/data.table/vignettes/datatable-reshape.html
        Melted.data.Sensors    <- data.table::melt(ASE.ID.init$General.DT, id.vars = c("date", "latitude", "longitude"), measure.vars = Modelled.Pollutants[Modelled.Pollutants %in% names(ASE.ID.init$General.DT)])
        # The rows below discard sensor na values. However we should not discard them otherwise if after a certain data treatment values becomes NA it is impissble to update them anymore. NO lets keep doing it and we will drop the entire table before uploading
        na.value <- Melted.data.Sensors[is.na(value), , which = TRUE]
        Melted.data.Sensors <- Melted.data.Sensors[-na.value]
        #rm(na.value)
        
        # Dropping General.DT to release memory
        ASE.ID.init <- ASE.ID.init[-which(names(ASE.ID.init) == "General.DT")]
        
        # Binding Melted.data.Sensors and Melted.data.References
        if (nrow(Melted.data.Sensors) > 0) {
            if (Filter.Ref && nrow(Melted.data.References) > 0) {
                if("Ref.Lat"  %in% names(Melted.data.References)) data.table::setnames(Melted.data.References, "Ref.Lat" , "latitude")
                if("Ref.Long" %in% names(Melted.data.References)) data.table::setnames(Melted.data.References, "Ref.Long", "longitude")
                Melted.data <- data.table::rbindlist(list(Melted.data.Sensors, Melted.data.References))
                rm(Melted.data.References)
            } else Melted.data <- Melted.data.Sensors
            rm(Melted.data.Sensors)
        } else {
            futile.logger::flog.warn(paste0("[Upload2Influx] ", ASE, ", no calibrated data to upload to InfluxDB server"))
            next}
        # renaming columns of Melted.data
        data.table::set(Melted.data, j = "variable", value = gsub(pattern = "_modelled", replacement = "", Melted.data$variable))
        data.table::set(Melted.data, j = "variable", value = gsub(pattern = "Out.", replacement = "", Melted.data$variable))
        
        # Adding ASE name column
        Melted.data[, ID_ASE := ASE.ID.init$ASE.name]
        
        # Extracting Meta.data.Sensors for Sensor data
        Meta.data.Sensors <- data.table::transpose(ASE.ID.init$ASE.cfg[ASE.ID.init$ASE.cfg$name.gas %in% c("name.sensor","gas.sensor","Sens.unit"),
                                                                       Col.modelled, with = F], fill = NA, make.names = 1)
        data.table::setnames(Meta.data.Sensors, "gas.sensor", "variable")
        
        # joining Melted.data with Meta.data for sensors
        Melted.data[Meta.data.Sensors, on = "variable", ":=" (name.sensor = i.name.sensor, Sens.unit = i.Sens.unit)]
        
        # Extracting Meta.data.Sensors for reference data
        if (Filter.Ref) {
            Meta.data.Ref <- data.table::transpose(ASE.ID.init$ASE.cfg[ASE.ID.init$ASE.cfg$name.gas %in% c("gas.reference2use","ref.unitgas"), Col.References, with = F], fill = NA, make.names = 1)
            data.table::setnames(Meta.data.Ref, "gas.reference2use", "variable")
            data.table::setnames(Meta.data.Ref, "ref.unitgas", "Sens.unit")
            Meta.data.Ref[Ref_Analysers, on = "variable", ":=" (name.sensor = i.name.sensor)]
            
            # joining Melted.data with Meta.data for references
            Melted.data[Meta.data.Ref, on = "variable", ":=" (name.sensor = i.name.sensor, Sens.unit = i.Sens.unit)]
            rm(Meta.data.Ref)}
        
        # joining with ID Sensors, taking care of sensor replacements
        if ("name" %in% names(ASE.ID.init$Config$Sensors.cfg)) data.table::setnames(ASE.ID.init$Config$Sensors.cfg, "name", "name.sensor")
        ID_Sensors <- unique(ASE.ID.init$Config$Sensors.cfg[!is.na(serial) & serial != "" & name.sensor %in% Meta.data.Sensors$name.sensor,c("name.sensor", "serial")])
        if (any(duplicated(ID_Sensors$name.sensor))) {
            # managing duplicated sensors
            Duplicated.sensors <- ID_Sensors[which(duplicated(ID_Sensors$name.sensor))]$name.sensor
            futile.logger::flog.warn(paste0("[Upload2Influx] ", ASE, " duplicated sensors: some sensors were replaced: ", paste(Duplicated.sensors, collapse = ", ")))
            Melted.data[ID_Sensors[!name.sensor %in% Duplicated.sensors], on = "name.sensor", serial := i.serial]
            
            # date for sensor change over
            ID_Sensors.Duplicated <- unique(ASE.ID.init$Config$Sensors.cfg[!is.na(serial) & name.sensor %in% Duplicated.sensors,c("time","name.sensor", "serial")])
            data.table::setnames(ID_Sensors.Duplicated, "time", "date")
            for (Sens in Duplicated.sensors) {
                ID_Sensors.Duplicated.Sens <- ID_Sensors.Duplicated[name.sensor == Sens]
                for (Row in nrow(ID_Sensors.Duplicated.Sens)) {
                    if (Row < nrow(ID_Sensors.Duplicated.Sens)) {
                        Rows <- which(Melted.data$name.sensor == Sens & Melted.data$date >= ID_Sensors.Duplicated.Sens[Row]$date & Melted.data$date < ID_Sensors.Duplicated.Sens[Row+1]$date)
                    } else Rows <- which(Melted.data$name.sensor == Sens & Melted.data$date >= ID_Sensors.Duplicated.Sens[Row]$date)
                    if (length(Rows) > 0) data.table::set(Melted.data, i = Rows, j = "serial",value = ID_Sensors.Duplicated.Sens[Row]$serial)
                }}
        } else Melted.data[ID_Sensors, on = "name.sensor", serial := i.serial]
        rm(ID_Sensors, Meta.data.Sensors)
        
        # ordering columns
        data.table::setcolorder(Melted.data, c("date", "ID_ASE", "latitude", "longitude", "variable", "name.sensor", "serial","value", "Sens.unit"))
        data.table::setnames(Melted.data, "date", "time")
        
        # Checking connection to Influx    ####
        if (ASE.ID.init$Config$Server$PROXY) {
            if (is.null(ASE.ID.init$Config$Server$LOGIN)) {
                httr::set_config(use_proxy(url=ASE.ID.init$Config$Server$URL, port=ASE.ID.init$Config$Server$PORT))
            } else httr::set_config( use_proxy(url=ASE.ID.init$Config$Server$URL, port=ASE.ID.init$Config$Server$PORT, username = ASE.ID.init$Config$Server$LOGIN, password = ASE.ID.init$Config$Server$PASSWORD))
        } else httr::reset_config()
        Influx.con <- httr::GET(paste0("http://",ASE.ID.init$Config$Server$Host,":",ASE.ID.init$Config$Server$Port,"/ping"),
                                config = authenticate(user = User, password = Pass, type = "basic"))
        if (Influx.con$status_code != 204) {
            return(stop(futile.logger::flog.error(paste0("[Upload2Influx] ", ASE, " Influx server is down. Stopping the script."))))
        } else futile.logger::flog.info(paste0("[Upload2Influx] ", ASE, " Influx server is up; connected to server"))
        
        # table dataset, time must be a numeric in nano seconds, https://docs.influxdata.com/influxdb/cloud/write-data/developer-tools/line-protocol/
        Dataset <- paste0(ASE.ID.init$Config$Server$Dataset, "_Calibrated")
        # create the JSON see https://influxdbcom.readthedocs.io/en/latest/content/docs/v0.9/guides/writing_data/
        futile.logger::flog.info(paste0("[Upload2Influx] ", ASE, " Preparing JSON data for uploading"))
        no.Na.value       <- which(!is.na(Melted.data$value) | is.na(Melted.data$value)| is.null(Melted.data$value))
        no.Na.name.sensor <- which(!is.na(Melted.data$name.sensor) & Melted.data$name.sensor != "")
        no.Na.latitude    <- which(!is.na(Melted.data$latitude) & Melted.data$latitude != "")
        no.Na.longitude   <- which(!is.na(Melted.data$longitude) & Melted.data$longitude != "")
        # complete
        no.NA <- intersect(intersect(no.Na.value,no.Na.name.sensor), intersect(no.Na.latitude, no.Na.longitude))
        if (length(no.NA) > 0) Melted.data[no.NA, POST := gsub("= ","=", paste0(Dataset, ",",
                                                                                sprintf("ASE=%s,",ASE.ID.init$ASE.name),sprintf("name_sensor=%s,",name.sensor),sprintf("variable=%s,",variable),sprintf("Sens_unit=%s ",Sens.unit),
                                                                                sprintf("latitude=%s," ,latitude),sprintf("longitude=%s," ,longitude),sprintf("value=%.3f "  ,as.numeric(value)),
                                                                                sprintf("%.0f", as.numeric(as.POSIXct(time))*1e9)))]
        no.name <- intersect(intersect(no.Na.value,which(is.na(Melted.data$name.sensor))), intersect(no.Na.latitude, no.Na.longitude))
        if (length(no.name) > 0) Melted.data[no.name, POST := gsub("= ","=",paste0(Dataset, ",",
                                                                                   sprintf("ASE=%s,",ASE.ID.init$ASE.name),sprintf("variable=%s,",variable),sprintf("Sens_unit=%s ",Sens.unit),
                                                                                   sprintf("latitude=%s," ,latitude),sprintf("longitude=%s," ,longitude),sprintf("value=%.3f "  ,as.numeric(value)),
                                                                                   sprintf("%.0f", as.numeric(as.POSIXct(time))*1e9)))]
        no_name_latitude <- intersect(intersect(no.Na.value,which(is.na(Melted.data$name.sensor))), intersect(which(is.na(Melted.data$latitude)), no.Na.longitude))
        if (length(no_name_latitude) > 0) Melted.data[no_name_latitude, POST := gsub("= ","=", paste0(Dataset, ",",
                                                                                                      sprintf("ASE=%s,",ASE.ID.init$ASE.name),sprintf("variable=%s,",variable),sprintf("Sens_unit=%s ",Sens.unit),
                                                                                                      sprintf("longitude=%s," ,longitude),sprintf("value=%.3f "  ,as.numeric(value)),
                                                                                                      sprintf("%.0f", as.numeric(as.POSIXct(time))*1e9)))]
        no_name_longitude <- intersect(intersect(no.Na.value, which(is.na(Melted.data$name.sensor))), intersect(no.Na.latitude, which(is.na(Melted.data$longitude))))
        if (length(no_name_longitude) > 0) Melted.data[no_name_longitude, POST := gsub("= ","=", paste0(Dataset, ",",
                                                                                                        sprintf("ASE=%s,",ASE.ID.init$ASE.name),sprintf("variable=%s,",variable),sprintf("Sens_unit=%s ",Sens.unit),
                                                                                                        sprintf("latitude=%s," ,latitude),sprintf("value=%.3f "  ,as.numeric(value)),
                                                                                                        sprintf("%.0f", as.numeric(as.POSIXct(time))*1e9)))]
        no_name_latitude_longitude <- intersect(intersect(no.Na.value,which(is.na(Melted.data$name.sensor))), intersect(which(is.na(Melted.data$latitude)), which(is.na(Melted.data$longitude))))
        if (length(no_name_latitude_longitude) > 0) Melted.data[no_name_latitude_longitude, POST := gsub("= ","=", paste0(Dataset, ",",
                                                                                                                          sprintf("ASE=%s,",ASE.ID.init$ASE.name),sprintf("variable=%s,",variable),sprintf("Sens_unit=%s ",Sens.unit),
                                                                                                                          sprintf("value=%.3f ",as.numeric(value)),
                                                                                                                          sprintf("%.0f", as.numeric(as.POSIXct(time))*1e9)))]
        no_latitude  <- intersect(intersect(no.Na.value,no.Na.name.sensor), intersect(which(is.na(Melted.data$latitude)), no.Na.longitude))
        if (length(no_latitude) > 0) Melted.data[no_latitude, POST := gsub("= ","=",paste0(Dataset, ",",
                                                                                           sprintf("ASE=%s,",ASE.ID.init$ASE.name),sprintf("name_sensor=%s,",name.sensor),sprintf("variable=%s,",variable),sprintf("Sens_unit=%s ",Sens.unit),
                                                                                           sprintf("longitude=%s," ,longitude),sprintf("value=%.3f ",as.numeric(value)),
                                                                                           sprintf("%.0f", as.numeric(as.POSIXct(time))*1e9)))]
        no_longitude <- intersect(intersect(no.Na.value,no.Na.name.sensor), intersect(no.Na.latitude, which(is.na(Melted.data$longitude))))
        if (length(no_longitude) > 0) Melted.data[no_longitude, POST := gsub("= ","=", paste0(Dataset, ",",
                                                                                              sprintf("ASE=%s,",ASE.ID.init$ASE.name),sprintf("name_sensor=%s,",name.sensor),sprintf("variable=%s,",variable),sprintf("Sens_unit=%s ",Sens.unit),
                                                                                              sprintf("latitude=%s," ,latitude),sprintf("value=%.3f ",as.numeric(value)),
                                                                                              sprintf("%.0f", as.numeric(as.POSIXct(time))*1e9)))]
        no_latitude_longitude <- intersect(intersect(no.Na.value,no.Na.name.sensor), intersect(which(is.na(Melted.data$latitude)), which(is.na(Melted.data$longitude))))
        if (length(no_latitude_longitude) > 0) Melted.data[no_latitude_longitude, POST := gsub("= ","=", paste0(Dataset, ",",
                                                                                                                sprintf("ASE=%s,",ASE.ID.init$ASE.name),sprintf("name_sensor=%s,",name.sensor),sprintf("variable=%s,",variable),sprintf("Sens_unit=%s ",Sens.unit),
                                                                                                                sprintf("value=%.3f ",as.numeric(value)),
                                                                                                                sprintf("%.0f", as.numeric(as.POSIXct(time))*1e9)))]
        # Selecting the JSON phrases
        JSON.File <- Melted.data[!is.na(Melted.data$POST),"POST"]
        Melted.data[,POST := NULL]
        
        # Checking if "ASE_Calibrated table exists on INFLUX (delete file on "Results" to restart the files)
        Influx.Tables <- httr::GET(utils::URLencode(paste0("http://", ASE.ID.init$Config$Server$Host,":", ASE.ID.init$Config$Server$Port,"/query?db=", ASE.ID.init$Config$Server$Db)),
                                   config = authenticate(user = User, password = Pass, type = "basic"),
                                   query = list(q = paste0("SHOW SERIES ON ", ASE.ID.init$Config$Server$Db)))
        Influx.Tables <- data.table::rbindlist(apply(Json_To_df(Influx.Tables), MARGIN = 1, function(i) {
            JSON <- strsplit(i,",")[[1]]
            return(data.table::data.table(Table = JSON[1], Channel = JSON[2], SensorID = JSON[3]))}))
        if (!Dataset %in% Influx.Tables$Table) {
            futile.logger::flog.info(paste0("[Upload2Influx] ", ASE, " Table ", Dataset, " does not exist in InfluxDB database ",ASE.ID.init$Config$Server$Db))
            Index_to_add <- seq_along(JSON.File$POST)
        } else {
            futile.logger::flog.info(paste0("[Upload2Influx] ", ASE, " Table ", Dataset, " exists in InfluxDB database ",ASE.ID.init$Config$Server$Db))
            # loading previous version of data
            Old.file <- file.path(ASE.ID.init$ASEDir, DIR_General, "JSON_file.csv")
            if (file.exists(Old.file)) {
                JSON_Old <- data.table::fread(file = Old.file, sep = NULL, quote = FALSE)
                # Identifying index of added or modified data
                Index_to_add <- which(!JSON.File$POST %in% JSON_Old[[1]])    
            } else {
                Index_to_add <- seq_along(JSON.File$POST)
                
                # If JSON file does not exists, it may indicate that a new calibration is applie so delete the table with calibrated data, admin priviledge are needed
                response <- httr::POST(url = utils::URLencode(paste0("http://", ASE.ID.init$Config$Server$Host,":", ASE.ID.init$Config$Server$Port,"/query?db=", ASE.ID.init$Config$Server$Db)),
                                       config = authenticate(user = User, password = Pass, type = "basic"),
                                       body = list(q = paste0("DROP MEASUREMENT ", Dataset), db = ASE.ID.init$Config$Server$Db),
                                       encode = "form")
                if (status_code(response) == 200) {
                    futile.logger::flog.info(paste("[Upload2Influx] Measurement", Dataset, "has been deleted successfully"))
                } else {
                    futile.logger::flog.warn(paste("[Upload2Influx] Failed to delete measurement", Dataset, ". Admin priviledges are need"))
                    print(content(response, "text"))}}}
        
        if (length(Index_to_add) > 0) futile.logger::flog.info(paste0("[Upload2Influx] ", ASE, " Ading ", length(Index_to_add), " lines to Table ", Dataset))
        if (nrow(JSON.File[Index_to_add]) > 0) {
            Start_line <- 0
            Step       <- 5000 # number of points to add
            repeat {
                if (Start_line + Step > nrow(JSON.File[Index_to_add])) Step <- nrow(JSON.File[Index_to_add]) - Start_line
                Loaded.Table <- httr::POST(url = utils::URLencode(paste0("http://", ASE.ID.init$Config$Server$Host,":", ASE.ID.init$Config$Server$Port,"/write?db=", ASE.ID.init$Config$Server$Db)),
                                           config = authenticate(user = User, password = Pass, type = "basic"),
                                           body = JSON.File[Index_to_add][(Start_line+1):(Start_line+Step)]$POST,
                                           encode = "json")
                if (Loaded.Table$status_code == 204) {
                    futile.logger::flog.info(paste0("[Upload2Influx] ",ASE, ", lines ", (Start_line+1), " to ",(Start_line+Step), " loaded to InfluxDB server"))
                    if (exists("Loaded.Lines")) Loaded.Lines <- c(Loaded.Lines, (Start_line+1):(Start_line+Step)) else Loaded.Lines <- (Start_line+1):(Start_line+Step)
                    # Init trial counter
                    if (exists("Trial.count") && Trial.count != 1) rm(Trial.count)
                    if (Start_line+Step + 1 >= nrow(JSON.File[Index_to_add])) {
                        if (!dir.exists(file.path(file.path(WD, Project, DIR_Results)))) dir.create(file.path(WD, Project, DIR_Results))
                        if (file.exists(file.path(ASE.ID.init$ASEDir, DIR_General, paste0("ASE",ASE.ID.init$ASE.name,"_calibrated.csv")))) unlink(file.path(ASE.ID.init$ASEDir, DIR_General, paste0("ASE",ASE.ID.init$ASE.name,"_calibrated.csv")), force = T)
                        # Saving Melted.data
                        data.table::fwrite(Melted.data, file = file.path(WD, Project, DIR_Results, paste0("ASE",ASE.ID.init$ASE.name,"_calibrated.csv")))
                        data.table::fwrite(JSON.File, file = file.path(ASE.ID.init$ASEDir, DIR_General, "JSON_file.csv"), col.names = TRUE, quote = FALSE)
                        futile.logger::flog.info(paste0("[Upload2Influx] ", ASE, " saved new ", file.path(WD, Project, DIR_Results, paste0("ASE",ASE.ID.init$ASE.name,"_calibrated.csv"))))
                        futile.logger::flog.info(paste0("[Upload2Influx] ", ASE, " saved new ", file.path(ASE.ID.init$ASEDir, DIR_General, "JSON_file.csv")))
                        break
                    } else {
                        Start_line <- Start_line + Step
                    } 
                } else {
                    futile.logger::flog.warn(paste0(paste0("[Upload2Influx] ",ASE, ", Lines :", (Start_line+1), " to ",(Start_line+Step), " not loaded. Trying again. Error:\n",Loaded.Table)))
                    if (!exists("Trial.count")) {
                        Trial.count <- 2
                    } else {
                        if (Trial.count <= 3) {
                            Trial.count <- Trial.count + 1
                            futile.logger::flog.warn(paste0("[Upload2Influx] ", ASE, " upload Tentative: ", Trial.count))
                        } else {
                            futile.logger::flog.error(paste0("[Upload2Influx] ", ASE, " After 3 tentatives, it is impossible to load lines between "(Start_line+1), " and ",(Start_line+Step),". The script is stopped"))
                            # saving last loaded data if any
                            if (exists("Loaded.Lines") && length(Loaded.Lines) > 0) {
                                JSON_file_error <- data.table::rbindlist(JSON_Old, JSON.File[Index_to_add][Loaded.Lines])
                                data.table::fwrite(JSON_file_error, file = file.path(ASE.ID.init$ASEDir, DIR_General, "JSON_file.csv"), col.names = FALSE, quote = FALSE)
                                futile.logger::flog.warn(paste0("[Upload2Influx] ", ASE, " saved new partial", file.path(ASE.ID.init$ASEDir, DIR_General, "JSON_file.csv")))
                            }
                            stop(futile.logger::flog.error(paste0("[Upload2Influx] ", ASE, " After 3 tentatives, it is impossible to load lines between "(Start_line+1), " and ",(Start_line+Step),". The script is stopped")))  
                        }}
                }
            }
        } else futile.logger::flog.warn(paste0("[Upload2Influx] ",ASE, ", no calibrated data to upload to InfluxDB server"))
        # Dropping all variables, connections, data ...
        To.rm <- c("Loaded.Lines", "Step", "Start_line", "Trial.count", "Loaded.Table", "ASEDir", "ASE.ID.init",
                   "Modelled.Pollutants", "Gas.sensors", "Col.modelled",
                   "Melted.data", "Meta.data.Sensors", "ID_Sensors", "JSON_file_error", "JSON.File", "JSON_Old", "Index_to_add", "Influx.Tables", "Dataset")
        Index.To.rm <- which(sapply(To.rm, exists))
        if (length(Index.To.rm) > 0) rm(list = To.rm[Index.To.rm])
    }
    # deleting garbage
    if (.Platform$OS.type == "windows") {
        gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)   
    } else gc(verbose = getOption("verbose"), reset = FALSE)
    
    return()
}

#' Create a simulations with calibration models along a time series with rolling windows of defined width and intervals ####
#' @description This function save simulations of calibration models for one sensor
#' using one model type with a calibration interval 
#' @param DIR (mandatory) Root directory for Project inclusding R scripts
#' @param Project (mandatory) Root directory for AirSensEUR box directory ASE that shall be under Dir
#' @param DIR_Models Optionale, default is "Simulations". Character vector, subdirectory of ASE where are saved the simulated calibration models.
#' @param ASE (mandatory) character vector with 1 ID of ASE box for which simulation will be performed
#' @param Place (optional) character vector, place where the calibration takes places, default is Antwerp  with ID of ASE boxes for which simulation will be performed

#' @param Predict.Interval (mandatory) vector of 2 dates (start, End), indicating the whole time series used by Simul_Cal_Models
#' @param Start.Intervals (optional) integer, default value is 4, increment of date of the calibration sequence seq(Predict.Interval[1], Predict.Interval[2], by = Start.Intervals) 
#'        for the 1st day of calibration simulations
#' @param i.Start (optional) integer, default value is 1, index of the first calibration sequences seq(Predict.Interval[1], Predict.Interval[2], by = Start.Intervals)  
#'        for the 1st day of calibration simulations
#' @param Times (optional) vector of integers, default is 2 c(7L, 14L, 21L, 28L, 35L, 42L), indicating the length in days of the simulated calibrations
#' @param Intervals (optional) vector of integers, default is c(1L, 3L,5L,7L), indicating the number of days for each window slice of calibration simulation (rolling window width)
#' @param Sensor_name (mandatory) Character vector, name(s) of sensors to simulate calibration models.
#' @param Sens.Model (optional) Character vector, default is "Kohler_modified". Type of models to be fitted using Cal_Line(). 
#' @param Relationships (optional) character vector, default is Relative_humidity_modelled If not NULL it shall give the covariates to be considered for selection in calibration model in priority. 
#'        It shall not included: "SHT31TE_volt", "SHT31HE_volt", "BMP280_volt" (check if this last sentence is TRUE).
#' @param Discarded.covariates (optional) character vectors, default is "". If not NULL parameter names included in Discarded.covariates cannot be used as covariates of the calibration and conversely.
#' @param degrees(optional) Character vector with the degrees of the covariates of multi variable calibration models. Default is "1"
#' @param Add.Covariates (optional) logical default is FALSE, the best covariates (in terms of correlation with residuals) are added to the calibration model, honouring the selected Relationships in priority
#' @param VIF (optional) logical, default is TRUE. If TRUE the variance inflator factors of dependent variables are computed and returned for MultiLinear models.
#' @param Treshold.VIF (optional) numeric, default is 10. Threshold that if exceeded that a covariates to be added to the current calibration model is correlated with the alredy used covariates. 
#'       If the threshold is exceeded, the selected coavriate is dropped.
#' @param Robust.coeff (optional) character vector, default is "min.U.Cal.". Method of selecting the best coefficients of models. It can be: median, Weighed, AlgA, Min.U.Pred. or min.U.Cal.
#' @param Conf.level numeric, default is 0.05, threshold to be exceeded for coefficients of models that are not significantly different from 0. If exceeded the tested calibration model is rejected. 
#' @param Thresh.R2 (optional) numeric, default is 0.00, difference between coefficient of determination of covariate/Residuals and covariates/Reference values to select covariates ot include in calibration model.
#' @param Weighted (optional) Logical default is FALSE. If TRUE calibration will use a weighted algorithm reducing input data in class of x and covariates according to the standard deviation of sensor data.
#' @param Register (optional) logical, default is TRUE. If TRUE, the Model, configuration and data of an AirSensEUR boxes after a new calibration model is save.
Simul_Cal_Models <- function(Dir, Project,  ASE, DIR_Models = "Simulations", Place = "Antwerp",
                             Predict.Interval, Start.Intervals = 4, i.Start = 1, Times = c(7L, 14L, 21L, 28L, 35L, 42L), Intervals = c(1L, 3L,5L,7L),
                             Sensor_name, Sens.Model = "Kohler_modified", Relationships = "Relative_humidity_modelled", Discarded.covariates = "", Degrees = "1", Add.Covariates = FALSE,
                             VIF = TRUE, Treshold.VIF = 10, Robust.coeff = "min.U.Cal.", Conf.level = 0.05, Thresh.R2 = 0.00, Weighted = FALSE,
                             Register = TRUE,  Parallel = F, ...) {
    # Fixed parameters
    if (any(class(Predict.Interval) == "Date")) {
        Start.Dates <- seq(Predict.Interval[1], Predict.Interval[2], by = Start.Intervals)   
    } else Start.Dates <- seq(Predict.Interval[1], Predict.Interval[2], by = Start.Intervals * 1440 *60) # in case of POSIXct
    
    # i.Start    <- 1 default value
    i.Time     <- 1
    i.Interval <- 1
    
    # List of simulations
    Param.combined <- data.table::data.table(ASE        = rep(ASE, times = length(Start.Dates) * length(Times) * length(Intervals)), 
                                             i.Start    = unlist(lapply(seq_along(Start.Dates), rep, times = length(Times) * length(Intervals))), 
                                             i.Time     = rep(unlist(lapply(seq_along(Times), rep, times = length(Intervals))), times = length(Start.Dates)), 
                                             i.Interval = rep(seq_along(Intervals), times = length(Start.Dates) * length(i.Time)))
    # only the ones which makes senses
    Param.combined$Senseful <- Start.Dates[Param.combined$i.Start] + Intervals[Param.combined$i.Interval] <= Predict.Interval[2]
    
    # Changing parameters
    if (length(Relationships) > 0 && shiny::isTruthy(Relationships)) namesCovariates <- paste0(paste(Relationships,Degrees, sep = "-"),collapse = "&") else namesCovariates <- ""
    ASE.ID <- Identify_ASE_Dir(ASEDir = ASEDir   <- file.path(Dir, Project, ASE), name.sensor = Sensor_name)
    # Delagging
    ASE.ID$General.DT <- Var_Delag_Pred(DT.General = ASE.ID$General.DT,  ColSens = ASE.ID$nameGasMod, ColSensRaw = ASE.ID$gas.sensor,
                                        ColRef = ASE.ID$nameGasRef, ColRefRaw = gsub("Out.", "", ASE.ID$nameGasRef), Sync.Pred = ASE.ID$Sync.Pred, Sync.Cal = FALSE,
                                        Meas.IN = ASE.ID$Meas.DateIN, Meas.END = ASE.ID$Meas.DateEND, Cal.IN = ASE.ID$Cal.DateIN, Cal.END = ASE.ID$Cal.DateEND, Verbose = TRUE)
    if (Parallel) {
        Simulutations <- future_apply(Param.combined[which(Senseful)], MARGIN =  1, function(Simul){
            
            print(Simul)
            i.Start    <- as.numeric(Simul[["i.Start"]])
            i.Time     <- as.numeric(Simul[["i.Time"]])
            i.Interval <- as.numeric(Simul[["i.Interval"]])
            # Setting Calibration interval
            Cal.Interval <- c(Start.Dates[i.Start], min(Start.Dates[i.Start] + Times[i.Time], Predict.Interval[2]))
            if (i.Time > 1 && Cal.Interval[2] - Cal.Interval[1] <= Times[i.Time-1]) return()
            # Setting the file name
            Auto.cal.name <- paste0(c(Simul[["ASE"]],"Auto.Cal",Sensor_name, Place, format(Cal.Interval[1],"%Y%m%d"), format(Cal.Interval[2],"%Y%m%d"), Intervals[i.Interval],
                                      Sens.Model,Add.Covariates,namesCovariates, paste0("Median-", Intervals[i.Interval]),Weighted, Robust.coeff),collapse = "__")
            message("Starting ",paste0(Auto.cal.name, " for ", paste(Simul, collapse = ","), "\n"))
            if(!dir.exists(file.path(Dir,Project,Simul[["ASE"]],DIR_Models,Sensor_name))) dir.create(file.path(Dir,Project,Simul[["ASE"]],DIR_Models,Sensor_name), recursive = TRUE)
            return(AutoCal.Boxes.Sensor(List.ASE = Simul[["ASE"]], Dir = Dir, Project = Project, DIR_Models = file.path(DIR_Models,Sensor_name), name.sensor = Sensor_name,
                                        Interval = Intervals[i.Interval], DateIN = Cal.Interval[1], DateEND = Cal.Interval[2],
                                        Meas.DateIN = Predict.Interval[1], Meas.DateEND = Predict.Interval[2],
                                        Mod_type = Sens.Model, Probs = Probs, Relationships = Relationships, degrees = Degrees, Add.Covariates = Add.Covariates,
                                        DRIFT = FALSE, volt = TRUE, modelled = FALSE, Discarded.covariates = Discarded.covariates,
                                        Weighted = Weighted, Thresh.R2 = Thresh.R2, VIF = TRUE, Treshold.VIF = 10, Robust.coeff = Robust.coeff,
                                        Register = Register, Rdata.name.file = paste0(Auto.cal.name,".rdata"), del.Rolling = FALSE, ...))
        }, future.seed = NULL, future.conditions = "message")
    } else {
        Simulutations <- apply(Param.combined, MARGIN =  1, function(Simul){
            print(Simul)
            i.Start    <- as.numeric(Simul[["i.Start"]])
            i.Time     <- as.numeric(Simul[["i.Time"]])
            i.Interval <- as.numeric(Simul[["i.Interval"]])
            
            # Setting Calibration interval
            Cal.Interval <- c(Start.Dates[i.Start], min(Start.Dates[i.Start] + Times[i.Time], Predict.Interval[2]))
            if(Cal.Interval[1] + Times[i.Time] >= as.Date("2025-03-16")) browser()
            if (i.Time > 1 && Cal.Interval[2] - Cal.Interval[1] <= Times[i.Time-1]) return()
            # Setting the Auto.cal.name file name
            Auto.cal.name <- paste0(c(Simul[["ASE"]],"Auto.Cal",Sensor_name, Place, format(Cal.Interval[1],"%Y%m%d"), format(Cal.Interval[2],"%Y%m%d"),Intervals[i.Interval],
                                      Sens.Model,Add.Covariates,namesCovariates, paste0("Median-", Intervals[i.Interval]),Weighted, Robust.coeff),collapse = "__")
            if(file.exists(file.path(Dir,Project,Simul[["ASE"]],DIR_Models,Sensor_name,paste0(Auto.cal.name,".Rdata")))) {
                futile.logger::flog.info(paste0("Exists: ", Auto.cal.name))
                return()
            } else {
                futile.logger::flog.info(paste0("Fitting: ", Auto.cal.name))
                browser()
            }
            message("Starting ",paste0(Auto.cal.name, " for ", paste(Simul, collapse = ","), "\n"))
            if(!dir.exists(file.path(Dir,Project,Simul[["ASE"]],DIR_Models,Sensor_name))) dir.create(file.path(Dir,Project,Simul[["ASE"]],DIR_Models,Sensor_name), recursive = TRUE)
            return(AutoCal.Boxes.Sensor(List.ASE = Simul[["ASE"]],
                                        ASE.ID = ASE.ID, 
                                        Dir = Dir, 
                                        Project  = Project, 
                                        DIR_Models = file.path(DIR_Models,Sensor_name),
                                        name.sensor = Sensor_name,
                                        Interval = Intervals[i.Interval], DateIN = Cal.Interval[1], DateEND = Cal.Interval[2],
                                        Meas.DateIN = Predict.Interval[1], Meas.DateEND = Predict.Interval[2],
                                        Mod_type = Sens.Model, Probs = Probs, Relationships = Relationships, degrees = Degrees, Add.Covariates = Add.Covariates,
                                        DRIFT = FALSE, volt = TRUE, modelled = FALSE, Discarded.covariates = Discarded.covariates,
                                        Weighted = Weighted, Thresh.R2 = Thresh.R2, VIF = TRUE, Treshold.VIF = 10, Robust.coeff = Robust.coeff,
                                        Register = Register, Rdata.name.file = paste0(Auto.cal.name,".rdata"), del.Rolling = FALSE, ...))
        })}
    
    # Purging time windows .rdata calibration files that are no more necessary 
    for (Directory_ASE in file.path(Dir,Project,ASE,DIR_Models,Sensor_name)) {
        Purged.files <- list.files(path = Directory_ASE, pattern = "-1__.rdata")
        Purged.files <- Purged.files[grep("Median", Purged.files, invert = T)]
        if (length(Purged.files) > 0) {
            for (i in Purged.files) {
                Del.Status <- unlink(file.path(Directory_ASE,i), force = T)
                if (!Del.Status) cat(paste0(i, " deleted\n")) else cat(paste0("Can't delete ",i,"\n"))}}}
    
    # Returning
    return(Simulutations)
}

#' Compare calibration models for ASE boxes whose calibration model is borrowed from another ASE box
#' @description This is a replacement of Compare_Models for ASE boxes for which the calibration is borrowed from another ASE box.
#' This function can work in parallel computing that speed up computation.
#' @param f.List.models.Final is the list of the models to be compared. Derived from function Final_ASE_Cal (see below)
#' @param f.Project is the name of the folder where all models placed
#' @param Exclude.Cal logical, default is TRUE. If TRUE data during calibration are excluded for prediction
#' @param Verbose logical, default TRUE. If TRUE some messages are sent to the console
#' @param f.Predict_Interval vector of Dates. This is the dates to which the calibrationmodels will be applied for, Starting and ending dates for prediction. 
#' If NULL Meas.DateIN and Meas.DateIN are extracted from SetTime.
#' @param Cal.DateIN Starting date for calibration, POSIXct or Date type. Default is NULL If NULL, Cal.DateIN is extracted from SetTime.
#' @param Cal.DateEND Ending date for calibration, POSIXct or Date type. Default is f.Predict_Interval[1] If NULL, Cal.DateEND is extracted from SetTime.
#' @param Cal.DateIN,Cal.DateEND class POSIXct or Date type, default values are f.Predict_Interval[1] and f.Predict_Interval[2]. 
#' They are the interval dates of which calibration metrics (R2, RMSE, slop and cal) are determined. If NULL theare set using the claibration model name. 
#' This is the case if the model is set using data of the ASE box. I the model of another box is used Cal.DateIN,Cal.DateEND shall be set.
#' @param Free.cores integer, default value is 3. Number of cores that are not included into the cluster for parallel computing
#' @param Free.cores integer, default value is 3. Max number of cores that are not included into the cluster for parallel computing
#' @param Parallel logical, default is TRUE. If TRUE parallel computing is performed using the future_apply function. If FALSE sequential computing is performed.
compare.DT <- function(f.List.models.Final, f.Project, Exclude.Cal = T, Verbose = T,
                       f.Predict_Interval, Cal.DateIN = Cal_Interval[1], Cal.DateEND = Cal_Interval[2],
                       Free.cores = 3L, Max.cores  = 5L, Parallel = TRUE) {
    if (Parallel) {
        
        # Set cluster of cores
        cores <- min(c(detectCores()- Free.cores, length(f.List.models.Final), Max.cores), na.rm = T)
        plan(multisession, workers = cores)
        
        
        Compared <- data.table::rbindlist(future_lapply(f.List.models.Final, function(ASE, Project, Predict_Interval, Cal.DateIN, Cal.DateEND, Exclude.Cal) {
            ASE.name <- basename(dirname(dirname(substring(ASE,gregexpr(pattern = Project,ASE)[[1]][1]))))
            f.compared <- Compare_Models(Exclude.Cal= Exclude.Cal, List.models =  ASE, Verbose = Verbose,
                                         Meas.DateIN = Predict_Interval[1], Meas.DateEND = Predict_Interval[2], 
                                         Cal.DateIN = Cal.DateIN, Cal.DateEND = Cal.DateEND,
                                         ASE.name = ASE.name )$Table.compare
            return(f.compared)
        }, Project = f.Project, Predict_Interval = f.Predict_Interval, Cal.DateIN = Cal.DateIN, Cal.DateEND = Cal.DateEND, Exclude.Cal = Exclude.Cal, 
        future.seed = NULL, future.conditions = "message"), fill=TRUE, use.names = T)
        
        # Close the cluster
        future:::ClusterRegistry("stop")
        
    } else {
        Compared <- data.table::rbindlist(lapply(f.List.models.Final, function(ASE, Project, Predict_Interval, Cal.DateIN, Cal.DateEND) {
            ASE.name <- basename(dirname(dirname(substring(ASE,gregexpr(pattern = Project,ASE)[[1]][1]))))
            f.compared <- Compare_Models(Exclude.Cal= Exclude.Cal, List.models =  ASE, Verbose = Verbose,
                                         Meas.DateIN = Predict_Interval[1], Meas.DateEND = Predict_Interval[2], 
                                         Cal.DateIN = Cal.DateIN, Cal.DateEND = Cal.DateEND,
                                         ASE.name = ASE.name )$Table.compare
            return(f.compared)
        }, Project = f.Project, Predict_Interval = f.Predict_Interval, Cal.DateIN = Cal.DateIN, Cal.DateEND = Cal.DateEND), fill=TRUE, use.names = T)}
    # Total number of columns
    Number.Columns <- length(names(Compared))
    # Columns to move 
    Number.Columns.Coeffs <- names(Compared)[(grep("Range.U.Pred", names(Compared))+1):Number.Columns]
    Number.Columns.Coeffs <- Number.Columns.Coeffs[!grepl("error|Interval", Number.Columns.Coeffs)]
    Number.Columns.Coeffs <- which(names(Compared) %in% Number.Columns.Coeffs)
    if (length(Number.Columns.Coeffs) > 0) {
        # where to move them
        I.R2Raw <- grep("R2raw", names((Compared)))
        setcolorder(Compared, c(1:(I.R2Raw-1),Number.Columns.Coeffs,I.R2Raw,setdiff((I.R2Raw + 1):Number.Columns, Number.Columns.Coeffs)))}
    return(Compared)
}

#' Full file.path and name of final calibration models to be used by compare.DT when the calibration model of ASE boxes is borrowed from other ASE box
#' @description This function return a character vector with the full file path and name of the  calibration model of an ASE box
#' when the ASE boxes use calibration models from other ASE boxes.
#' @param i character vector, name of ASE box (ASE_name)
#' @param Dir character vector, file.path of the R scripts that includes directory name Project
#' @param Project is the name of the directory where all the ASE box three is placed
#' @param Place character vector, geograohical place name where the calibration took place (should be in the Auto_cal.Rdata file name )
#' @param Sensor_name Character vector, character vector with the name(s) of sensors to compare models, e.g "CO_A4_P1" ...
#' @param Cal_Interval vector of POSIXCt or Dates
#' @param DIR_Models Name of the subdirectory of ASEDir where are the calibration models. Default is "Models".
#' @param Cal.Param Data.table with all parameters of calibration with columns: 
#' @param with.Interval.Robust logical, default is FALSE. If TRUE Auto.cal.name includes median, Weighted and robust.coeff.
#' @param Interval integer, default is NULL, width of sub calibration used for median calibration. If NULL the value in Cal.Param is used.
#' @param Robust.coeff character string, default is NULL, method used for the aggregation of  parameters of calibration models. If NULL the value in Cal.Param is used.
#' Sensor, Mod_Type, Add.Covariates, Interval, Relationships, Degrees, Discarded.covariates, Treshold.VIF, VIF, Conf.level, Thresh.R2, DRIFT, volt, modelled, Register, Exclude.Cal
#' Name of the subdirectory of ASEDir where are the calibration models. Default is "Models".
Final_ASE_Cal <- function(i, Dir, Project, Sensor_name, Place, Cal_Interval, DIR_Models, Cal.Param, with.Interval.Robust = FALSE, Interval = NULL,
                          Weighted = NULL, Robust.coeff = NULL, Verbose = TRUE){
    
    ASEDir <- file.path(Dir, Project, i)
    if (length(Cal.Param[Sensor == Sensor_name][["Relationships"]][[1]]) > 0 && shiny::isTruthy(Cal.Param[Sensor == Sensor_name][["Relationships"]][[1]])) {
        # if (length(Cal.Param[Sensor == Sensor_name][["Relationships"]][[1]]) > 0 && Cal.Param[Sensor == Sensor_name][["Relationships"]][[1]] != "") {
        namesCovariates <- paste0(paste(Cal.Param[Sensor == Sensor_name]$Relationships[[1]],Cal.Param[Sensor == Sensor_name]$Degrees[[1]], sep = "-"),collapse = "&") 
    } else namesCovariates <- ""
    if(with.Interval.Robust){
        if (is.null(Interval))     Interval     <- Cal.Param[Sensor == Sensor_name]$Interval
        if (is.null(Robust.coeff)) Robust.coeff <- Cal.Param[Sensor == Sensor_name]$Robust.coeff
        if (is.null(Weighted))     Weighted     <- Cal.Param[Sensor == Sensor_name]$Weighted
        Auto.cal.name <- paste0(c("Auto.Cal",Sensor_name, Place, format(Cal_Interval[1],"%Y%m%d"), format(Cal_Interval[2],"%Y%m%d"), 
                                  Cal.Param[Sensor == Sensor_name]$Mod_Type,Cal.Param[Sensor == Sensor_name]$Add.Covariates, namesCovariates,
                                  paste0("Median-", Interval),Weighted, Robust.coeff),collapse = "__")
    } else Auto.cal.name <- paste0(c("Auto.Cal",Sensor_name, Place, format(Cal_Interval[1],"%Y%m%d"), format(Cal_Interval[2],"%Y%m%d"), 
                                     Cal.Param[Sensor == Sensor_name]$Mod_Type,Cal.Param[Sensor == Sensor_name]$Add.Covariates, namesCovariates),collapse = "__")
    if (file.exists(file.path(ASEDir,DIR_Models, paste0(Auto.cal.name,".rdata")))){
        if (Cal.Param[Sensor == Sensor_name]$Mod_Type != "Yatkin") Model    <- load_obj(file.path(ASEDir,DIR_Models, paste0(Auto.cal.name,".rdata")))
        if (Verbose) futile.logger::flog.info(paste0("[Final_ASE_Cal] final model in ",paste0(Auto.cal.name,".rdata")))
        if (Cal.Param[Sensor == Sensor_name]$Mod_Type != "Yatkin") {
            return(file.path(ASEDir,"Models",Model$Final_median_Model$List.NewModels))
        } else return(file.path(ASEDir,"Models", paste0(paste0(c(i,Sensor_name, 'nA', Cal.Param[Sensor == Sensor_name]$Mod_Type, format(Cal_Interval[1],"%Y%m%d"), 
                                                                 format(Cal_Interval[2],"%Y%m%d"), namesCovariates, Weighted),collapse = "__"), "__.rdata")))
    } else {
        futile.logger::flog.error(paste0("[Final_ASE_Cal] can't find ",file.path(ASEDir,DIR_Models, paste0(Auto.cal.name,".rdata"))))
        return()}
}

# Functions for calibration in Oslo ====
Oslo_Cal <- function(List.ASE, ...) {
    arguments <- list(...)
    for (i in List.ASE) {
        Place            <- ifelse("Place" %in% names(arguments), arguments$Place, "Oslo")
        if ("ASE.ID" %in% names(arguments)) ASE.ID <- arguments$ASE.ID else ASE.ID <- NULL
        name.sensor      <- ifelse("name.sensor" %in% names(arguments), arguments$name.sensor, "CO_A4_P1")
        Mod_type         <- ifelse("Mod_type" %in% names(arguments), arguments$Mod_type, "Linear.Robust")
        Add.Covariates   <- ifelse("Add.Covariates" %in% names(arguments), arguments$Add.Covariates, FALSE)
        if ("Relationships" %in% names(arguments)) Relationships <- arguments$Relationships else Relationships <- NULL
        if("Degrees"        %in% names(arguments)) Degrees       <- arguments$Degrees       else Degrees       <- 1
        if("Interval"       %in% names(arguments)) Interval      <- arguments$Interval      else Interval      <- 5
        if ("Robust.coeff"  %in% names(arguments)) Robust.coeff  <- arguments$Robust.coeff  else Robust.coeff  <- "min.U.Cal."
        if ("del.Rolling" %in% names(arguments)) del.Rolling <- arguments$del.Rolling else  del.Rolling = FALSE
        if ("del.Intermediary" %in% names(arguments)) del.Intermediary <- arguments$del.Intermediary else  del.Intermediary = FALSE
        namesCovariates  <- if (length(Relationships) > 0 && shiny::isTruthy(Relationships)) paste0(paste(Relationships,Degrees, sep = "-"),collapse = "&") else ""
        Cal_Interval     <- Cal_Inter(i, name.sensor)
        Predict_Interval <- Pred_Inter(i, name.sensor)
        if ("Discarded.covariates" %in% names(arguments)) Discarded.covariates <- arguments$Discarded.covariates else Discarded.covariates <- NULL
        if ("Dir" %in% names(arguments))                  Dir                  <- arguments$Dir                  else Dir                  <- "S:/Box Sync/AirSensEUR/Fieldtests/Shiny"
        if ("Weighted" %in% names(arguments))             Weighted             <- arguments$Weighted             else Weighted             <- FALSE
        if ("Robust.coeff" %in% names(arguments))         Robust.coeff         <- arguments$Robust.coeff         else Robust.coeff         <- "min.U.Cal."
        Auto.cal.name    <- paste0(c("Auto.Cal",name.sensor, Place, format(Cal_Interval[1],"%Y%m%d"), format(Cal_Interval[2],"%Y%m%d"),
                                     Mod_type, Add.Covariates, namesCovariates, paste0("Median-",Interval), Weighted, Robust.coeff),collapse = "__")
        if ("Usermins" %in% names(arguments))             Usermins             <- arguments$Usermins             else Usermins             <- NULL
        if ("Probs" %in% names(arguments))                Probs                <- arguments$Probs                else Probs                <- NULL
        Oslo.cal  <- AutoCal.Boxes.Sensor(List.ASE        = i,
                                          ASE.ID          = ASE.ID,
                                          name.sensor     = name.sensor,
                                          Interval        = Interval,
                                          DateIN          = Cal_Interval[1],
                                          DateEND         = Cal_Interval[2],
                                          Meas.DateIN     = Predict_Interval[1],
                                          Meas.DateEND    = Predict_Interval[2],
                                          DRIFT           = ifelse("DRIFT" %in% names(arguments), arguments$DRIFT, FALSE),
                                          volt            = ifelse("volt" %in% names(arguments), arguments$volt, FALSE), 
                                          modelled        = ifelse("modelled" %in% names(arguments), arguments$modelled, FALSE),
                                          Discarded.covariates = Discarded.covariates,
                                          Project         = ifelse("Project" %in% names(arguments), arguments$Project, "ASE_Boxes"),
                                          Verbose         = ifelse("Verbose" %in% names(arguments), arguments$Verbose, TRUE), 
                                          VIF             = ifelse("VIF" %in% names(arguments), arguments$VIF, TRUE), 
                                          Treshold.VIF    = ifelse("Treshold.VIF" %in% names(arguments), arguments$Treshold.VIF, 10), 
                                          Conf.level      = ifelse("Conf.level" %in% names(arguments), arguments$Conf.level, 0.05), 
                                          Mod_type        = Mod_type, 
                                          Probs           = Probs, 
                                          Relationships   = Relationships, 
                                          degrees         = ifelse("Degrees" %in% names(arguments), arguments$degrees, "1"), 
                                          Thresh.R2       = ifelse("Thresh.R2" %in% names(arguments), arguments$Conf.level, 0.01),
                                          Add.Covariates  = ifelse("Add.Covariates" %in% names(arguments), arguments$Add.Covariates, FALSE), 
                                          Rdata.name.file = paste0(Auto.cal.name,".rdata"), 
                                          Register        = ifelse("Register" %in% names(arguments), arguments$Register, TRUE),
                                          Usermins        = Usermins,
                                          Dir             = Dir,
                                          Weighted        = Weighted,
                                          Robust.coeff    = Robust.coeff,
                                          del.Rolling     = del.Rolling,
                                          del.Intermediary= del.Intermediary)}}
Cal_Inter <-  function(i, name.sensor) {
    if(name.sensor =="NO_B4_P1") {
        if (i == "425FB3") return (c(as.Date("2021-03-10"), as.Date("2021-03-23"))) else return (c(as.Date("2021-03-27"), as.Date("2021-04-09")))
    } else if (i == "64FD11") {
        if (name.sensor %in% c("5310CST","5310CAT")) return(c(as.Date("2020-09-12"), as.Date("2020-09-16"))) else return(c(as.Date("2020-09-11"), as.Date("2020-09-21")))
    } else if (i == "65325E") {
        return(c(as.Date("2021-03-29"), as.Date("2021-04-09")))
    } else if (i == "64B082") {
        return(c(as.Date("2021-03-23"), as.Date("2021-04-07")))
    } else if ((i %in% c("40458D", "4065ED")) & !(name.sensor %in% c("5301CAT", "5301CST", "OPCN3PM1"))) {
        return(c(as.Date("2021-03-23"), as.Date("2021-04-07")))
    } else if (i == "426179") {
        return(c(as.Date("2020-09-04"), as.Date("2020-09-18")))
    } else if (i == "65326C") {
        return(c(as.Date("2020-09-11"), as.Date("2020-09-25")))
    } else if (i == "425FB4" || i == "64A292" ||  i == "64FD0A") {
        return(c(as.Date("2020-10-01"), as.Date("2020-10-14")))
        # } else if (i %in% c("40642E", "40816F", "425FB3", "40458D", "4065ED") && (name.sensor == "CO_A4_P1" || name.sensor == "NO_B4_P1" || name.sensor == "NO2_B43F_P1")) {
        #     return(c(as.Date("2020-10-01"), as.Date("2020-10-14")))
    } else if (name.sensor %in% c("CO_A4_P1")) {
        return(c(as.Date("2020-10-01"), as.Date("2020-10-14")))
    } else {
        return(c(as.Date("2020-09-16"), as.Date("2020-09-30")))}}

Pred_Inter <-  function(i, name.sensor = NULL) {
    if (name.sensor =="NO_B4_P1") {
        if (i %in% c("40458D", "4065ED", "64B082", "40642E", "64FD11", "40817F", "64E9C5", "65063E", "64CB6D", "40816F", "6517DD")) {
            return(c(as.Date("2021-03-13"), as.Date("2021-04-08")))
        } else if (i == "65325E") { return(c(as.Date("2021-03-29"), as.Date("2021-04-08")))
        } else return(c(as.Date("2021-03-10"), as.Date("2021-04-08")))
    } else if (i == "64FD11") {
        return(c(as.Date("2020-09-11"), as.Date("2020-09-21")))
    } else if (i == "65325E") {
        return(c(as.Date("2021-03-29"), as.Date("2021-04-09")))
    } else if (i == "64B082") {
        return(c(as.Date("2021-03-13"), as.Date("2021-04-08")))
    } else if ((i %in% c("40458D", "4065ED")) & !(name.sensor %in% c("5301CAT", "5301CST", "OPCN3PM1"))) {
        return(c(as.Date("2021-03-13"), as.Date("2021-04-08")))
    } else if (i == "426179") {
        return(c(as.Date("2020-08-26"), as.Date("2020-09-18")))
    } else if (i == "65326C") {
        return(c(as.Date("2020-09-09"), as.Date("2020-10-05")))
    } else if (i == "649526") {
        return(c(as.Date("2020-09-01"), as.Date("2020-10-16")))
    } else if ((i %in% c("40642E", "40816F", "425FB3") && name.sensor == "NO2_B43F_P1")) {
        return(c(as.Date("2020-08-26"), as.Date("2020-10-14")))
    } else {
        return(c(as.Date("2020-08-26"), as.Date("2020-10-28")))}}
Exclude_Cal_Time <-  function(i, Cal.Param = NULL, Sensor = NULL) {
    if (i == "64FD11" || i == "64A292" || i == "425FB4" || i == "65326C"){
        return(FALSE)
    } else {
        if (!is.null(Cal.Param)) {
            if (!is.null(Sensor)) {
                return(Cal.Param[eval(Cal.Param$Sensor == Sensor), Exclude.Cal])
            } else return(TRUE)
        } else return(TRUE)}}
# Comparing all calibration models
Re_ordering_Compared <- function(Compared.Table) {
    Number.Columns <- length(names(Compared.Table))
    Index.Last.std.error.x  <- which(names(Compared.Table) %in% c("std.error.x", "std.error.a1"))[1]
    Index.R2raw             <- which(names(Compared.Table) == "R2raw")
    change.order.cols       <- which(names(Compared.Table) %in% grep("std.error.", names(Compared.Table)[(Index.Last.std.error.x+1):length(names(Compared.Table))], invert = T, value = T))
    if (length(change.order.cols) > 0){
        New.Order <- c(1:(Index.R2raw-1),change.order.cols,setdiff(Index.R2raw:length(names(Compared.Table)),change.order.cols))
        setcolorder(Compared.Table, New.Order)} 
    return(Compared.Table)}
Oslo_Compared <- function(List.ASE, Dir, name.sensor, Place, Sens.Model, Add.Covariates, namesCovariates, Cal_Inter, Pred_Inter, Cal.Param = NULL, Interval = 3, Weighted = FALSE, 
                          Robust.Coeff = "Weighted.") {
    List.models.Final <- sapply(List.ASE, function(i){
        ASEDir   <- file.path(Dir, "ASE_Boxes", i)
        Auto.cal.name        <- paste0(c("Auto.Cal",name.sensor, Place, format(Cal_Inter[1],"%Y%m%d"), format(Cal_Inter[2],"%Y%m%d"), 
                                         Sens.Model,Add.Covariates,namesCovariates, paste0("Median-",Interval),Weighted, Robust.Coeff),collapse = "__")
        Model    <- load_obj(file.path(ASEDir,"Models", paste0(Auto.cal.name,".rdata")))
        return(file.path(ASEDir,"Models",Model$Final_median_Model$List.NewModels))
    })
    ComparedOslo <- lapply(List.models.Final, function(Dir_Model){
        i <- strsplit(basename(Dir_Model), "__")[[1]][1]
        Pred_Inter(i = i, name.sensor = name.sensor)[1]
        Comparing <- Compare_Models(Exclude.Cal = Exclude_Cal_Time(i, Cal.Param, name.sensor), List.models =  Dir_Model, Verbose = T, 
                                    Meas.DateIN = Pred_Inter(i, name.sensor = name.sensor)[1], Meas.DateEND = Pred_Inter(i, name.sensor)[2])$Table.compare
        return(Compare_Models(Exclude.Cal = Exclude_Cal_Time(i, Cal.Param, name.sensor), List.models =  Dir_Model, Verbose = T, 
                              Meas.DateIN = Pred_Inter(i, name.sensor = name.sensor)[1], Meas.DateEND = Pred_Inter(i, name.sensor)[2]))$Table.compare
    })
    if (length(ComparedOslo) > 1) ComparedOslo <- data.table::rbindlist(ComparedOslo, use.names = T, fill = T)
    if (length(List.ASE) > 1) ComparedOslo <- Re_ordering_Compared(ComparedOslo)
    return(ComparedOslo)}
##########################################C

#' Adding absolute humidity and temperature deficit to a data.table if relative humidity and temperature are available
#' #' @details if absolute humidity and/or temperature deficit cvannot be computed, the default is value is NA_real_
#' @param General.DT data.table to which to add name.AbsHR and absolute humidity and temperature deficit
#' @param name.Temp character vector, default is "Out.Temperature". Header name for the columns of General.DT holding temperature in Celsius degrees.
#' @param name.RH character vector, default is "Out.Relative_humidity". Header name for the columns of General.DT holding relative humidity in percentage.
#' @param name.AbsHR character vector, default is "Absolute_humidity". Header name for the columns of General.DT that will have absolute humidity in g/m?.
#' @param name.Tdef character vector, default is "Td_deficit". Header name for the columns of General.DT that will have Temperature deficit in ?C.
Add_AbsHum_Tdef <- function(General.DT, name.Temp = "Out.Temperature", name.RH = "Out.Relative_humidity", name.AbsHR = "Absolute_humidity", name.Tdef = "Td_deficit") {
    if (all(c(name.Temp, name.RH) %in% names(General.DT))) {
        
        # Adding initial NA_Real_ values to columns name.AbsHR and name.Tdef
        data.table::set(General.DT, j = name.AbsHR, value = rep(NA_real_, nrow(General.DT)))
        data.table::set(General.DT, j = name.Tdef , value = rep(NA_real_, nrow(General.DT)))
        both.Temp.Hum <- which(is.finite(rowSums(General.DT[, c(name.Temp, name.RH), with = FALSE])))
        if (length(both.Temp.Hum) > 0) {
            data.table::set(General.DT, i = both.Temp.Hum, j = name.AbsHR, value =  threadr::absolute_humidity(General.DT[both.Temp.Hum][[name.Temp]], General.DT[both.Temp.Hum][[name.RH]]))
            Td <- weathermetrics::humidity.to.dewpoint(rh = General.DT[both.Temp.Hum][[name.RH]], t = General.DT[both.Temp.Hum][[name.Temp]], temperature.metric = "celsius")
            data.table::set(General.DT, i = both.Temp.Hum, j = name.Tdef, value = General.DT[both.Temp.Hum][[name.Temp]] - Td)}}
    
    # returning Datable iwth AbsHum and tdef
    return(General.DT)}


#' discard duplicate time in Dataset and save table
#' @param SQLite.con SQLite connection returned by dbConnect(SQLite(), dbname = name.SQLite) connection
#' @param Dataset AirSensEUR data table in SQLite.con
#' @Save Logical default is TRUE. If TRUE the table paste0(Dataset,"_Cast") in SQLite.con is overwritten with data.table without dupicated time is over
Unique.SQL.time <- function(SQLite.con, Dataset, Save = T) {
    
    # reading paste0(Dataset,"_Cast") from SQLite DB SQLite.con
    DT.SQL <- data.table::data.table(DBI::dbGetQuery(SQLite.con, paste0("SELECT * FROM \"", paste0(Dataset,"_Cast"),"\"  order by rowid")))
    
    # returning data.table without dupicated time
    DT.SQL <- Unique.DT.time(DT = DT.SQL)
    
    # saving SQLite table
    if (Save){
        # Before saving the table delete the existing one
        DBI::dbRemoveTable(conn = SQLite.con, name = paste0(Dataset,"_Cast"))
        # Vaccum to update the DB size
        DBI::dbExecute(conn = SQLite.con, "VACUUM;")
        # Save the table
        RSQLite::dbWriteTable(conn = SQLite.con, name = paste0(Dataset,"_Cast"), value = DT.SQL, overwrite = T)
    }
    
    # returning data.table without dupicated time
    returning(DT.SQL)
}

#' discard duplicate time in data.table
#' time can be of class POSIXct or character in the format %Y-%m-%d %H:%M:%OS only
#' @param DT data.table 
#' discard duplicate time in data.table
#' time can be of class POSIXct or character in the format %Y-%m-%d %H:%M:%OS only
#' @param DT data.table 
Unique.DT.time <- function(DT, tz = "UTC") {
    
    # in order to avoid to change the inital DT in memory. THis double the memory used by the function. BE CAREFUL
    DT.change <- data.table::copy(DT)
    
    # Convert time from character to POSIXct in order to be able to compute averages
    if(!lubridate::is.POSIXct(DT.change$time)) data.table::set(DT.change, j="time", value = Set_date2POSIXct(DT.change$time, tz = tz))
    
    if(length(which(duplicated(DT.change$time))) > 0){
        
        # Discarding rowid, it does not make sense to make an average out ot rowid
        if("rowid" %in% names(DT.change)) DT.change[, rowid:=NULL]
        
        # Creating unique index of time
        DT.change[, duplo.time := .GRP, by = time]
        
        # averaging all columns except duplo.time by duplo.time to remove duplicated
        DT.change <- DT.change[, lapply(.SD, mean, na.rm = TRUE), by = duplo.time, .SDcols= names(DT.change)[-which(names(DT.change) == "duplo.time")]]
        
        # Checking if duplo.time is in DT.Change and drop it
        if("duplo.time" %in% names(DT.change)) DT.change[, duplo.time:= NULL]
        
        # Convert Nan to na, Nan are created by mean() when all values are NAs
        data.table::set(DT.change, j = names(DT.change), value = lapply(DT.change, nan.to.na))}
    
    #returning DT.change with unique time
    return(DT.change)
}




