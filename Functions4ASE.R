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
#   Eike Hinderk Jürrens   , e.h.juerrens@52north.org
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
#             DateIN.Influx.prev if it exists then to DateIN.SOS.prev  if it exists otherwise it is set to "UTC"
# 181012 MG   my.rm.outliers: the computing time has been divided by two, comput ing the min and max of interval of tolerance within one rollapply
# 190110 MG   Etalonnage: an error has been colved when all s_y are NA
#
# TO BE DONE:
#             Funtion Down-Influx repeats the download of last 4 weeks of date each time it is called (for now, unique() is used)
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
    # return      : the function return a dataframe/data.table with the voltages or currents for all sensors
    # Sensors_Cal : dataframe with the following column vectors:
    #               name.ga       : a vector of strings with the molecule symbols of sensors mounted on the AirSensEUR (CO...)
    #               gas.sensor    : a vector of strings with the molecule name that are measured by the AirSensEUR (Carbon_monoxide...)
    #               name.sensor   : a vector of strings with the names of sensors mounted on the AirSensEUR (COA4...)
    #               Ref           : a float vector, one value per sensor in Sensors_Cal$Sensors, giving the voltage in the middle of the analogue to digital converter (ADC) on the shield
    #               RefAD         : a float vector, one value per sensor in Sensors_Cal$Sensors, corresponding to the range of the ADC conversion (ref ? RefAD)
    #               Intercept     : a float vector, one value per sensor in Sensors_Cal$Sensors, intercept of conversion equation Conc[] = Intercept + Slope x Voltage
    #               Slope         : a float vector, one value per sensor in Sensors_Cal$Sensors, intercept of conversion equation Conc[] = Intercept + Slope x Voltage
    #               Sens.raw.unit : vector of strings, one value per sensor with the Unit names after conversion of Digital values into raw data
    # ADC         : number of bits of the Aanalogue to digital conversion
    # Digital     : a dataframe of numerics with the Digital values to be converted into Voltages or Currents (Out.Nitrogen_dioxide, ...
    # Volt.Convert: logical, default is TRUE. If TRUE, the data in Digital dataFrame need conversion from Digital to V/nA. If FALSE data are already in volts and conversion is not necessary.
    # reorder Sensors_Cal as Digital for gas sensors - create empty data.table of results
    # The name of the sensors are given by the names of the column in Digital
    Sensors_Cal  <- Sensors_Cal[as.vector(sapply(gsub(pattern = "Out.", replacement = "", names(Digital)),
                                                 function(i) grep(i, Sensors_Cal$gas.sensor))),]
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
        #MyVectornA  <- 10^9/(Sensors_Cal$GAIN[indexA] * Sensors_Cal$Rload[indexA])
        MyVectornA  <- 10^9/(Sensors_Cal$GAIN[indexA])
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
                dyOptions(labelsUTC = T) # plot in UTC
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
                dyOptions(labelsUTC = T) # plot in UTC
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
            plot_Warm <- dygraph(data = data, ylab = Ylab, main = Title, height = "auto", width="100%") %>%
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
            plot_Warm <- dygraph(data = data, ylab = Ylab, main = Title, height = "auto", width="100%") %>%
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
    # dependences           = ping()
    #------------------------------------------------------------------------------CR
    # Sensor Data retrieving at apiEndpoint
    #------------------------------------------------------------------------------CR
    cat("\n")
    cat("-----------------------------------------------------------------------------------\n")
    futile.logger::flog.info(paste0("[Down_SOS] ", AirSensEur.name," sensor data retrieving"), sep = "\n")
    # Checking internet connection availability
    if (curl::has_internet()) {
        URL <- unlist(strsplit(unlist(strsplit(gsub('http://', '', AirsensWeb), split = '/'))[1], split = ':'))[1]
        if (PingThisSite(URL)) {
            futile.logger::flog.info(paste0("[Down_SOS] ping to ", AirsensWeb, " Ok"), sep = "\n")
        } else{
            # return(cat(paste0("[Down_SOS] ERROR: you have an internet connection but cannot ping to ",AirsensWeb,". SOS download cannot be carried out."), sep = "\n"))
        }
    } else {
        return(futile.logger::flog.error(paste0("[Down_SOS] no internet connection. SOS download cannot be carried out.")))
    }
    # connect
    apiEndpoint <- sensorweb4R::Endpoint(AirsensWeb)
    # number of category at the apiEndpoint
    futile.logger::flog.info(paste0("[Down_SOS] in total ", length(sensorweb4R::timeseries(apiEndpoint)), " Sensors at the SOS client."), sep = "\n")
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
    futile.logger::flog.info(paste0("[Down_SOS] Position of station ", AirSensEur.name, ":", head(geom@coords)),sep = "\n")
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
    # Set the Rdata file of input data
    airsenseur.db.file  = file.path(WDinput, "airsenseur.db")
    Ref.file            = file.path(WDinput, "RefData.csv")
    Influx.file         = file.path(WDinput, "InfluxData.csv")
    SOS.file            = file.path(WDinput, "SOSData.csv")
    General.file        = file.path(WDinput, "General.csv")
    cat("-----------------------------------------------------------------------------------\n")
    futile.logger::flog.info(paste0("[Check_Download] Checking \n",
                                    airsenseur.db.file, "\n",
                                    General.file, "\n",
                                    Ref.file, "\n",
                                    SOS.file, " \n",
                                    Influx.file,"\n in ", WDinput, ""))
    # Checking if the directory exist
    if (!dir.exists(WDinput)) {
        futile.logger::flog.info(paste0("[Check_Download] Directory", WDinput, "does not exist. It is going to be created. All sensor and reference data are going to be downloaded."), sep = "\n")
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
                futile.logger::flog.info(paste0("[Check_Download] sensor data are going to be retrieved. Start date for data download: ", DateEND.General.prev), sep = "\n")
            } else {
                Retrieve.data.General  = FALSE
                futile.logger::flog.info(paste0("[Check_Download] no sensor data are going to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."))
            }
        } else {
            # General.df is NULL
            if (!file.exists(General.file)) {
                # General.file does not exist
                ExistFil.data.General = FALSE
                Retrieve.data.General  = TRUE
                DateIN.General.prev    = NULL
                DateEND.General.prev   = NULL
                futile.logger::flog.info(paste0("[Check_Download] ", General.file, " does not exist. It is going to be created, sensor data will be retrieved."))
            } else {
                # General.file exists
                ExistFil.data.General = TRUE
                futile.logger::flog.info(paste0("[Check_Download] ", General.file, " exists."), sep = "\n")
                if (extension(General.file) == ".csv") {
                    General.df <- fread(file = General.file, na.strings = c("","NA", "<NA>"), select = "date")
                } else if (extension(General.file) == ".Rdata") load(General.file)
                if (is.null(General.df)) {
                    DateIN.General.prev    = NULL
                    DateEND.General.prev   = NULL
                    Retrieve.data.General  = TRUE
                    futile.logger::flog.info(paste0("[Check_Download] ", General.file, " is NULL (no values). It is going to be created, data, if any, will be retrieved."))
                } else {
                    # General.df exists and is not NULL
                    DateIN.General.prev  <- min(General.df$date, na.rm = TRUE)
                    DateEND.General.prev <- max(General.df$date, na.rm = TRUE)
                    # Checking if Download of General.df is necessary
                    if (difftime(Sys.time(), max(General.df$date, na.rm = TRUE), units = "mins") > UserMins) {    ### MG , I doubt about the tz here, I think all is changed to UTM, as it is a difference maybe it does not matter
                        Retrieve.data.General  = TRUE
                        futile.logger::flog.info(paste0("[Check_Download] sensor data are going to be retrieved. Start date for data download: ", DateEND.General.prev), sep = "\n")
                    } else {
                        Retrieve.data.General  = FALSE
                        futile.logger::flog.info(paste0("[Check_Download] no sensor data are going to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."))
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
                futile.logger::flog.info(paste0("[Check_Download] reference data are going to be retrieved. Start new reference data at : ", DateEND.Ref.prev, "."))
            } else {
                Retrieve.data.Ref = FALSE
                futile.logger::flog.info(paste0("[Check_Download] reference data are going to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."))
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
                futile.logger::flog.info(paste0("[Check_Download] ", Ref.file, " does not exist. It is going to be created, data will be retrieved."))
            } else {
                # Ref.file exists
                ExistFil.data.Ref = TRUE
                futile.logger::flog.info(paste0("[Check_Download] ", Ref.file, " exists."))
                if (extension(Ref.file) == ".csv") {
                    RefData <- fread(file = Ref.file, na.strings = c("","NA", "<NA>"))
                    if (class(RefData$date) == "integer") data.table::set(RefData, j = "date", value = as.POSIXct(RefData$date, origin = "1970-01-01", TZ = "UTC"))
                } else if (extension(Ref.file) == ".Rdata") load(Ref.file)
                if (!is.null(RefData)) {
                    # RefData exists and is not NULL
                    # # Not considering end rows with only NA values for sensors
                    ind <- apply(RefData[, which(names(RefData) != "date"), with = FALSE], 1, function(x) !all(is.na(x)))
                    DateIN.Ref.prev  <- min(RefData$date[ind], na.rm = TRUE)
                    DateEND.Ref.prev <- max(RefData$date[ind], na.rm = TRUE)
                    Var.Ref.prev     <- names(RefData)
                    # Checking if Download of RefData is necessary
                    if (difftime(Sys.time(), DateEND.Ref.prev, units = "mins") > UserMins) {    ### MG , I doubt about the tz here, I think all is changed to UTC, as it is a difference maybe it does not matter
                        Retrieve.data.Ref = TRUE
                        futile.logger::flog.info(paste0("[Check_Download] reference data are should retrieved. Start new reference data at : ", DateEND.Ref.prev, "."))
                    } else {
                        Retrieve.data.Ref = FALSE
                        futile.logger::flog.info(paste0("[Check_Download] reference data are going to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."))
                    }
                } else {
                    # RefData exists but it is NULL
                    Retrieve.data.Ref <- TRUE
                    DateIN.Ref.prev   <- NULL
                    DateEND.Ref.prev  <- NULL
                    Var.Ref.prev      <- NULL
                    futile.logger::flog.info(paste0("[Check_Download] ", Ref.file, " is NULL (no values). It is going to be created, data will be retrieved."))
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
                futile.logger::flog.info(paste0("[Check_Download] sensor data are going to be retrieved. Start date for data download: ", DateEND.Influx.prev, "."))
            } else {
                Retrieve.data.Influx  = FALSE
                futile.logger::flog.info(paste0("[Check_Download] no sensor data are going to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."))
            }
        } else {
            if (!file.exists(Influx.file)) {
                # InfluxData does not exist
                ExistFil.data.Influx  = FALSE
                Retrieve.data.Influx  = TRUE
                DateIN.Influx.prev    = NULL
                DateEND.Influx.prev   = NULL
                futile.logger::flog.info(paste0("[Check_Download] ", Influx.file, " does not exist. It is going to be created, sensor data will be retrieved."))
            } else {
                # Influx.file exists
                ExistFil.data.Influx  = TRUE
                futile.logger::flog.info(paste0("[Check_Download] ", Influx.file, " exists."))
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
                        futile.logger::flog.info(paste0("[Check_Download] sensor data are going to be retrieved. Start date for data download: ", DateEND.Influx.prev, "."))
                    } else {
                        Retrieve.data.Influx  = FALSE
                        futile.logger::flog.info(paste0("[Check_Download] no sensor data are going to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."))
                    }
                } else {
                    # InfluxData exists but it is NULL
                    Retrieve.data.Ref   = TRUE
                    DateIN.Influx.prev  = NULL
                    DateEND.Influx.prev = NULL
                    futile.logger::flog.info(paste0("[Check_Download] ", Influx.file, " is NULL (no values). It is going to be created, data will be retrieved."))
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
                futile.logger::flog.info(paste0("[Check_Download] SOS sensor data are going to be retrieved. Start date for data download: ", DateEND.Influx.prev, "."))
            } else {
                Retrieve.data.SOS   = FALSE
                futile.logger::flog.info(paste0("[Check_Download] no SOS sensor data are going to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."))
            }
        } else {
            if (!file.exists(SOS.file)) {
                # SOS.file does not exist
                ExistFil.data.SOS     = FALSE
                Retrieve.data.SOS   = TRUE
                DateIN.SOS.prev     = NULL
                DateEND.SOS.prev    = NULL
                futile.logger::flog.info(paste0("[Check_Download] ", SOS.file, " does not exist. It should be created, SOS sensor data should be retrieved."))
            } else {
                # SOS.file exists
                ExistFil.data.SOS     = TRUE
                futile.logger::flog.info(paste0("[Check_Download] ", SOS.file, " exists."))
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
                        futile.logger::flog.info(paste0("[Check_Download] SOS sensor data are going to be retrieved. Start date for data download: ", DateEND.Influx.prev, "."))
                    } else {
                        Retrieve.data.SOS   = FALSE
                        futile.logger::flog.info(paste0("[Check_Download] no SOS sensor data are going to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."))
                    }
                } else {
                    # SOSData exists but it is NULL
                    Retrieve.data.SOS   = TRUE
                    DateIN.SOS.prev     = NULL
                    DateEND.SOS.prev    = NULL
                    futile.logger::flog.info(paste0("[Check_Download] ", SOS.file, " is NULL (no values). It is going to be created, data will be retrieved."))
                }
            }
        }
        if (!file.exists(airsenseur.db.file)) {
            # airsenseur.db.file does not exist
            ExistFil.data.db      = FALSE
            Retrieve.data.db      = TRUE
            DateIN.db.prev        = NULL
            DateEND.db.prev       = NULL
            futile.logger::flog.info(paste0("[Check_Download] ", airsenseur.db.file, " does not exist. It should be created, SOS sensor data should be retrieved."))
        } else {
            # airsenseur.db.file exists
            ExistFil.data.db      = TRUE
            futile.logger::flog.info(paste0("[Check_Download] ", airsenseur.db.file, " exists."))
            # Checking table Dataset in airsenseur.db
            SQLite.con <- dbConnect(SQLite(), dbname = airsenseur.db.file)
            # Checking if the SQLite.con database and the table Dataset exists?
            list.Tables       <- dbListTables(SQLite.con)
            if (any(grepl("_Cast", list.Tables))) Table <- list.Tables[grep("_Cast", list.Tables)] else {
                if (any(grepl(Influx.name, list.Tables))) Table <- list.Tables[grepl(Influx.name, list.Tables)] else stop(paste0("Tables with ",Influx.name, "not found\n"))
            }
            if (length(Table) > 0) {
                if (length(Table) == 1) {
                    futile.logger::flog.info(paste0("[Check_Download] The database ", airsenseur.db.file, " includes the table ", Table," with the columns: ",
                                                    paste0(dbListFields(SQLite.con, Table), collapse = ", ") ))
                } else {
                    futile.logger::flog.warn(paste0("[Check_Download] ", airsenseur.db.file, " has been mixed up with several datasets. It includes the table ", paste(Table, collapse = ", ")))
                    if (any(grepl(pattern = Influx.name, Table))) {
                        Table <- Table[grep(pattern = Influx.name, Table)]
                        futile.logger::flog.info(paste0("[Check_Download] only Table ",Table, " is considered"))
                    } else {
                        stop("[Check_Download] ", airsenseur.db.file, " includes the several datasets. None of them being", Influx.name,".\n The script is stopped. Delete airsenseur.db and restart influx downalod")}}
                # DataSet in airsenseur.db exists and is not NULL
                DateIN.db.prev      <- dbGetQuery(SQLite.con, paste0("SELECT min(time) FROM \"", Table, "\""))[1,1]
                DateEND.db.prev     <- dbGetQuery(SQLite.con, paste0("SELECT max(time) FROM \"", Table, "\""))[1,1]
                dbDisconnect(conn = SQLite.con)
                # Checking if Download of InfluxData is necessary
                if (difftime(Sys.time(), ymd_hms(DateEND.db.prev) , units = "mins") > UserMins) {    ### MG , I doubt about the tz here, I think all is changed to UTM, as it is a difference maybe it does not matter
                    Retrieve.data.db   = TRUE
                    futile.logger::flog.info(paste0("[Check_Download] sensor data are going to be retrieved. Start date for data download: ", DateEND.Influx.prev, ""))
                } else {
                    Retrieve.data.db   = FALSE
                    futile.logger::flog.info(paste0("[Check_Download] no sensor data are going to be retrieved. The latest data are already downloaded, please restart in at least ", UserMins, "mins."))
                }
            } else {
                # DataSet in airsenseur.db does not exist
                Retrieve.data.db      = TRUE
                DateIN.db.prev        = NULL
                DateEND.db.prev       = NULL
                futile.logger::flog.info(paste0("[Check_Download] ", airsenseur.db.file, " is NULL (no values). It is going to be created, data will be retrieved."))
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
        if (verbose) futile.logger::flog.info("[Json_To_df] Response to ",Error.Message,"\n")
        return(futile.logger::flog.error(paste0("[Json_To_df] ERROR, query returning error status code ",JSON$status_code, ". The query is wrong or the Influx server may be down.")))
    } else {
        JSON <- jsonlite::fromJSON(content(JSON, "text", encoding = "ISO-8859-1"), simplifyVector = TRUE, flatten = T)
        # Checking that JSON is not empty, e. g. there was no data for the Key or tag selected
        if (any(names(JSON) %in% "results")) {
            if (any(names(JSON$results) %in% "series")) {
                # delete "mean_" in case of query mean
                JSON.Colnames <- gsub(pattern = "mean_", replacement= "",JSON$results$series[[1]]$columns[[1]])
                if (verbose) futile.logger::flog.info(paste0("[Json_To_df] columns in JSON object: ",paste0(JSON.Colnames, collapse = ", "),""))
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
    # Return                          : create the local database airsenseur.db in name.SQLite and 
    #                                   return the time zone determined by find_tz(LastLong, Lastlat, use_google = TRUE) and LastDate in airsenseur.db
    # Dependences                     : Ping(), Load_Packages()
    cat("\n")
    cat("-----------------------------------------------------------------------------------\n")
    #------------------------------------------------------------------------------CR
    # Checking internet connection availability
    #------------------------------------------------------------------------------CR
    if (curl::has_internet()) {
        if (PingThisSite(gsub('http://', '', Host))) {
            futile.logger::flog.info(paste0("[Down_Influx] ping to ", Host, " Ok"), sep = "\n")
        } else return(futile.logger::flog.error(paste0("[Down_Influx] you have internet connection but can't ping to ",Host,". InfluxDB download cannot be carried out."), sep = ""))
    } else return(futile.logger::flog.error(paste0("[Down_Influx] no internet connection. InfluxDB download cannot be carried out."), sep = ""))
    #------------------------------------------------------------------------------CR
    # create influx connection object and getting number of records
    #------------------------------------------------------------------------------CR
    if (PROXY) {
        if (is.null(LOGIN)) set_config(use_proxy(url=URL, port=PORT)) else set_config( use_proxy(url=URL, port=PORT, username    = LOGIN, password = PASSWORD))
    } else reset_config()
    Influx.con <- httr::GET(paste0("http://",Host,":",Port,"/ping"),
                            config = authenticate(user = User, password = Pass, type = "basic"))
    if (Influx.con$status_code != 204) {
        futile.logger::flog.error("[Down_Influx] ERROR Influx server is down. Stopping the script.", "/n")
        return(futile.logger::flog.error("[Down_Influx] Influx server is down. Stopping the script."))
    } else futile.logger::flog.info("[Down_Influx] Influx server is up; connected to server")
    # total number of rows
    Influx.Total.N <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)),
                                config = authenticate(user = User, password = Pass, type = "basic"),
                                query = list(q = paste0("SELECT count(boardTimeStamp) FROM \"", Dataset,"\"")))
    Influx.Total.N <- Json_To_df(Influx.Total.N, Numeric = "boardTimeStamp")$count
    # Last GPS time (and the timestamp together)
    Influx.Last <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)),
                             config = authenticate(user = User, password = Pass, type = "basic"),
                             query = list(q = paste0("SELECT  LAST(boardTimeStamp) FROM \"", Dataset,"\"")))
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
        futile.logger::flog.info(paste0("[Down_Influx] ", name.SQLite, " already exists."))
    } else {
        # airsenseur.db does not exist
        futile.logger::flog.error(paste0("[Down_Influx] ", name.SQLite, " does not exist and it is going to be created."))
    }
    SQLite.con <- dbConnect(SQLite(), dbname = name.SQLite)
    #------------------------------------------------------------------------------CR
    # table dataset exists?
    #------------------------------------------------------------------------------CR
    if (dbExistsTable(conn = SQLite.con, name = Dataset) || dbExistsTable(conn = SQLite.con, name = paste0(Dataset,"_Cast"))) { 
        # the table Dataset exists in airsenseur.db
        futile.logger::flog.info(paste0("[Down_Influx] the table ", Dataset, " already exists in airsenseur.db."))
        if (dbExistsTable(conn = SQLite.con, name = paste0(Dataset,"_Cast"))) {
            Dataset.N               <- DBI::dbGetQuery(SQLite.con, paste0("SELECT max(rowid) FROM \"", paste0(Dataset,"_Cast"), "\""))[1,1]
            SQL.time.Last           <- DBI::dbGetQuery(SQLite.con, paste0("SELECT * FROM \"", paste0(Dataset,"_Cast"),"\"  order by rowid desc limit 1;"))$time
            SQL.gpsTimestamp.Last   <- DBI::dbGetQuery(SQLite.con, paste0("SELECT * FROM \"", paste0(Dataset,"_Cast"),"\"  order by rowid desc limit 1;"))$gpsTimestamp
            # deleteing the table DataSet, not used. Now DataSet_cast is used
            if (dbExistsTable(conn = SQLite.con, name = Dataset)) {
                DBI::dbRemoveTable(conn = SQLite.con, name = Dataset)
                DBI::dbExecute(conn = SQLite.con, "VACUUM;")
            } 
        } else {
            DT.SQL.query <- data.table::data.table(DBI::dbGetQuery(SQLite.con, paste0("SELECT * FROM \"", Dataset,"\"  order by rowid")))
            # remove duplicated if any
            Duplicated.rows <- which(duplicated(DT.SQL.query))
            if (length(Duplicated.rows) > 0) DT.SQL.query <- DT.SQL.query[-Duplicated.rows]
            DT.SQL.query <- dcast(DT.SQL.query, time + altitude + boardTimeStamp + gpsTimestamp + latitude + longitude ~ name, value.var = "sampleEvaluatedVal", fill = NA)
            #DBI::dbRemoveTable( conn = SQLite.con, name = paste0(Dataset,"_Cast"))
            RSQLite::dbWriteTable(conn = SQLite.con, name = paste0(Dataset,"_Cast"), value = DT.SQL.query)
            # Counting the number of records in AirSensEUR$Dataset - This will work provided that all rowid exist.
            Dataset.N               <- DBI::dbGetQuery(SQLite.con, paste0("SELECT max(rowid) FROM \"", Dataset, "\""))[1,1]
            SQL.time.Last           <- DBI::dbGetQuery(SQLite.con, paste0("SELECT * FROM \"", Dataset,"\"  order by rowid desc limit 1;"))$time
            SQL.gpsTimestamp.Last   <- DBI::dbGetQuery(SQLite.con, paste0("SELECT * FROM \"", Dataset,"\"  order by rowid desc limit 1;"))$gpsTimestamp
            # deleteing the table DataSet, not used. Now DataSet_cast is used
            DBI::dbRemoveTable(conn = SQLite.con, name = Dataset)
            DBI::dbExecute(conn = SQLite.con, "VACUUM;")
        }
        # Error Message and stop the script if there more data in the airsenseur.db than in InfluxDB
        if (difftime(ymd_hms(Influx.Last$time), ymd_hms(SQL.time.Last), units = "mins") < Mean) {
            futile.logger::flog.info("[Down_Influx] Downloading is up to date. No need for data download.")
        } else futile.logger::flog.info(paste0("[Down_Influx] records between ",format(ymd_hms(SQL.time.Last),"%Y-%m-%d %H:%M")," and ",format(ymd_hms(Influx.Last$time),"%Y-%m-%d %H:%M"), 
                                               " are added into the table ", paste0(Dataset,"_cast"), " in airsenseur.db."))
        Dataset.index   <- FALSE # if airsenseur.db exists then the indexes were already created, then no need to create the indexes
    } else {# the table Dataset does not exist in airsenseur.db
        # There are no records in AirSensEUR$Dataset
        futile.logger::flog.info(paste0("[Down_Influx] the table ", Dataset, " does not exist in airsenseur.db. It is going to be created."))
        Dataset.N       <- 0
        # Get SQL.time.Last as First GPS time (and the timestamp together) of InfluxDB
        SQL.time.Last <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)),
                                   config = authenticate(user = User, password = Pass, type = "basic"),
                                   query = list(q = paste0("SELECT  FIRST(sampleEvaluatedVal) FROM \"", Dataset, "\"")))
        if (SQL.time.Last$status_code != 200) futile.logger::flog.error("[Down_Influx] query first sampleEvaluatedValin airsenseur.db. Influx server may be down./n")
        SQL.time.Last <- Json_To_df(SQL.time.Last)$time
        Dataset.index   <- TRUE # if true indexes will be created
    } 
    #SQL.time.Last <- ymd_hms(SQL.time.Last, tz = Influx.TZ)
    #------------------------------------------------------------------------------CR
    # Downloading InfluxDB data and add them to the airsenseur.db
    #------------------------------------------------------------------------------CR
    # List of sensors to download
    # Sensors names, it seems that I cannot query directly name or channel (strings). Adding SELECT of a float field it works. Selecting the first 50 ones. Use SHOW TAG VALUES INSTEAD
    Influx.Sensor <-  httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)),
                                config = authenticate(user = User, password = Pass, type = "basic"),
                                query = list(q = paste0("SHOW TAG VALUES FROM \"", Dataset,"\" WITH KEY IN ( \"name\") ; ")))
    Influx.Sensor <- Json_To_df(Influx.Sensor)
    colnames(Influx.Sensor)[colnames(Influx.Sensor) == "value"] <- "name";
    # Adding channel for each Sensor names
    # deleting rows with name in c("L2942CUR","L2942VOL","L4156STA") which have periodicity of 10 minutes making the calculation of boardtime not continuous evey 11 minutes
    if (any(grepl(pattern = paste(c("L2942CUR","L2942VOL","L4156STA"), collapse = "|"),Influx.Sensor$name))) Influx.Sensor <- Influx.Sensor[-which(Influx.Sensor$name %in% c("L2942CUR","L2942VOL","L4156STA")),]
    for (i in Influx.Sensor$name) {
        Influx.Channel.number <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)),
                                           config = authenticate(user = User, password = Pass, type = "basic"),
                                           query = list(q = paste0("SELECT altitude, channel, \"name\" FROM \"", Dataset,"\" WHERE \"name\" = '",i,"' LIMIT 1;")))
        Influx.Channel.number <- Json_To_df(Influx.Channel.number)
        Influx.Sensor[which(Influx.Sensor$name == i),"channel"] <- Influx.Channel.number$channel
        Influx.Sensor[which(Influx.Sensor$name == i),"time"]    <- Influx.Channel.number$time
    }
    Influx.Sensor <- Influx.Sensor[order(Influx.Sensor$time),]
    print(Influx.Sensor, quote = FALSE)
    # Adding to database
    RSQLite::dbWriteTable(conn = SQLite.con, name = "Channel.names", value = Influx.Sensor, overwrite = TRUE)
    
    # Downloading always in increasing date, max download data in InfuxDB: chunks of 10000
    NbofDays.For10000data <- 10000/(24*60/Mean)/length(Influx.Sensor$name)
    # Number of seconds corresponding to NbofDays.For10000data
    Step <- NbofDays.For10000data * 24 * 60 * 60
    while (difftime(ymd_hms(Influx.Last$time), ymd_hms(SQL.time.Last), units = "mins") > Mean) {
        # Downloading from Influx server using httr, query different for Mean = 1 min <> 1 min (average needed, takes more time)
        if ( Mean == 1) {
            # Do not use LIMIT 10000 it is slow
            Mean.Query <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)),
                                    config = authenticate(user = User, password = Pass, type = "basic"),
                                    query = list(q = paste0("SELECT * FROM \"", Dataset,"\" WHERE ",
                                                            paste(c(paste0(paste0("\"name\" = '",Influx.Sensor$name,"'"), collapse = " OR ")), collapse = ""),
                                                            " AND time >  '",SQL.time.Last,"'",
                                                            " AND time <= '",format(ymd_hms(SQL.time.Last)+Step,"%Y-%m-%d %H:%M:%S"),"'")))
        } else {
            Mean.Query <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)),
                                    config = authenticate(user = User, password = Pass, type = "basic"),
                                    query = list(q = paste0("SELECT mean(*) FROM \"", Dataset, "\" WHERE ",
                                                            paste(c(paste0(paste0("\"name\" = '",Influx.Sensor$name,"'"), collapse = " OR ")), collapse = ""),
                                                            " AND time > '",format(ymd_hms(SQL.time.Last),"%Y-%m-%d %H:%M:%S"),
                                                            "' AND time <= '", format(ymd_hms(SQL.time.Last) + Step,"%Y-%m-%d %H:%M:%S"),"' GROUP BY time(",Mean,"m);")))
            # LIMIT " , format(round(1440/Mean), scientific = FALSE), " OFFSET ", format(0, scientific = FALSE)
        }
        # Checking good query status code
        if (Mean.Query$status_code != 200) {
            futile.logger::flog.error(paste0("[Down_Influx] does not succeed to query the influxDB with status_code <> 200."))
        } else {
            # extracting lists from json
            if (length(colnames(Json_To_df(Mean.Query))) > 1 ) {
                # calibrated is not necessary, we will create a new table in influx in cloud
                Adding <- Json_To_df(Mean.Query,
                                     Numeric = c("altitude", "boardTimeStamp", "gpsTimestamp", "latitude", "longitude", "sampleEvaluatedVal", "sampleRawVal"),
                                     Discard = c("Calibrated","calibrated"))
            }
        }
        # updating Adding
        if (exists("Adding")) {
            Adding <- data.table(Adding)
            if (exists("All.Sensors.Adding")) All.Sensors.Adding <- rbindlist(list(All.Sensors.Adding,Adding), use.names = TRUE, fill = TRUE) else All.Sensors.Adding <- Adding
            remove(Adding)
        }
        # adding data to airSensEUR.db
        if (exists("All.Sensors.Adding") && All.Sensors.Adding[.N, time] != SQL.time.Last) {
            # discarding rows with all Na Values
            NA.values <- which(
                rowSums(
                    is.na(All.Sensors.Adding[,-which(names(All.Sensors.Adding) %in% c("time","channel","name")), with = FALSE])) ==
                    ncol(All.Sensors.Adding[,-which(names(All.Sensors.Adding) %in% c("time","channel","name")), with = FALSE]
                    )
            )
            if (length(NA.values) > 0) All.Sensors.Adding <- All.Sensors.Adding[-NA.values,]
            # Appending to Dataset" 
            # RSQLite::dbWriteTable(conn = SQLite.con, name = Dataset, value = All.Sensors.Adding, append = TRUE)
            futile.logger::flog.info(paste0("[Down_Influx] ", format(nrow(All.Sensors.Adding), scientific = FALSE), " records added between ",
                                            format(ymd_hms(All.Sensors.Adding[1,time]),"%Y-%m-%d %H:%M")," and ",format(ymd_hms(All.Sensors.Adding[.N,time]),"%Y-%m-%d %H:%M")
                                            ," added to table ", paste0(Dataset,"_Cast")))
            # Appending to (Dataset,"_Cast" 
            cast.All.Sensors.Adding <- dcast(All.Sensors.Adding, time + altitude + boardTimeStamp + gpsTimestamp + latitude + longitude ~ name, fill = NA, value.var = "sampleEvaluatedVal")
            if (dbExistsTable(SQLite.con, paste0(Dataset,"_Cast"))) {
                Names.db <- dbListFields(SQLite.con, paste0(Dataset,"_Cast"))
                Names    <- names(cast.All.Sensors.Adding)
                if (!all(Names %in% Names.db)) {
                    # adding new columns
                    New.Columns <- Names[!Names %in% Names.db]
                    for (i in New.Columns) dbExecute(SQLite.con, paste0("ALTER TABLE \"", paste0(Dataset,"_Cast"),"\" ADD COLUMN \"",i,"\" REAL;"))
                }
            }
            RSQLite::dbWriteTable(conn = SQLite.con, name = paste0(Dataset,"_Cast"), value = cast.All.Sensors.Adding, append = TRUE)
            # updating SQL.time.Last for while loop
            SQL.time.Last  <- cast.All.Sensors.Adding[.N, time]
            if (nrow(All.Sensors.Adding) < 10000) Step <- Step * 2
            remove(All.Sensors.Adding, cast.All.Sensors.Adding)
        } else {
            futile.logger::flog.warn(paste0("[Down_Influx] No influx data between ", format(ymd_hms(SQL.time.Last),"%Y-%m-%d %H:%M")," and ",format(ymd_hms(SQL.time.Last) + Step,"%Y-%m-%d %H:%M"),"."))
            # updating SQL.time.Last for while loop with next row
            Next.date.Query <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)),
                                         config = authenticate(user = User, password = Pass, type = "basic"),
                                         query = list(q = paste0("SELECT * FROM \"", Dataset,
                                                                 "\" WHERE time > '",SQL.time.Last, 
                                                                 "' ORDER BY time ASC LIMIT 1 ")))
            # Checking good query status code
            if (Next.date.Query$status_code != 200) {
                futile.logger::flog.warn(paste0("[Down_Influx] does not succed to query the influxDB with status_code <> 200. Likely there is no more data for ",Influx.Sensor[j,"name"],""))
                # updating SQL.time.Last for while loop
                SQL.time.Last  <- ymd_hms(Influx.Last$time)
            } else {
                # extracting lists from json
                Next.date <- Json_To_df(Next.date.Query,
                                        Numeric = c("altitude", "boardTimeStamp", "gpsTimestamp", "latitude", "longitude", "sampleEvaluatedVal", "sampleRawVal"),
                                        Discard = c("Calibrated","calibrated"))
                # updating SQL.time.Last for while loop
                SQL.time.Last  <- ymd_hms(Next.date$time) - Mean * 60
            }
        }
    }
    futile.logger::flog.info(paste0("[Down_Influx] the downloading of sensor data from the Influx server is finished."))
    # I need to add index ?!?
    # Counting the number of records in AirSensEUR$Dataset
    Dataset.N   <- DBI::dbGetQuery(SQLite.con, paste0("SELECT max(rowid) FROM \"", paste0(Dataset,"_Cast"), "\""))[1,1]
    # getting the time zone, port 443 of the Browser shall be opened
    if (is.null(Influx.TZ) || Influx.TZ == "Local time") {
        futile.logger::flog.info(paste0("[Down_influx] determining the time zone with the last valid latitude and longitude of ", Dataset, " in airsenseur.db."))
        Offset <- Dataset.N
        repeat {
            Coord.lat.long   <- DBI::dbGetQuery(SQLite.con, paste0("SELECT time, longitude, latitude FROM \"", Dataset, "\" WHERE rowid > ", Offset - 500, " AND rowid <= ", Offset, " ;"))
            if (all(is.na.data.frame(Coord.lat.long[,c("longitude","latitude")])) ||
                all(Coord.lat.long[!is.na.data.frame(Coord.lat.long[,c("longitude")]),c("longitude","latitude")] == 0)) {
                if (Offset > 500) {
                    Offset <- Offset - 500
                } else {
                    futile.logger::flog.warn(paste0("[Down_influx] impossible to determine the time zone of the sensor data. TZ is kept as ", Influx.TZ))
                    break
                }
            } else {
                Lastlat     <- tail(na.omit(Coord.lat.long$latitude[Coord.lat.long$latitude != 0]), n = 1)
                LastLong    <- tail(na.omit(Coord.lat.long$longitude[Coord.lat.long$longitude != 0]), n = 1)
                Influx.TZ <- find_tz(LastLong, Lastlat, use_google = use_google)
                futile.logger::flog.info(paste0("[Down_influx] the time zone of the sensor data is ", Influx.TZ))
                break
            }
        }
    }
    # # getting the last date, latitude and longitude in name.SQLite.
    # Last date to add only new data, latitude and longitude to get the time zone
    # checking for non zero values and no NA()
    LastDate <- DBI::dbGetQuery(SQLite.con, paste0("SELECT time FROM \"", paste0(Dataset,"_Cast"),"\" ORDER BY rowid DESC LIMIT 1;"))$time
    # InfluxDB gives everything in UTC not in local time zone - Well by observation in grafana it seems that the dates are in Local Time
    if (is.null(Influx.TZ) || Influx.TZ == "Local time") {
        LastDate <- ymd_hms(LastDate, tz = "UTC")
    } else {
        LastDate <- ymd_hms(LastDate, tz = Influx.TZ)
    }
    # Creating index to speed up select in function SQLite2df, it seems that this is not used anymore so it is commented
    # if (Dataset.index) {
    #     DBI::dbGetQuery(SQLite.con, paste0("CREATE INDEX IDtime ON "   , Dataset, " (time);"))
    #     DBI::dbGetQuery(SQLite.con, paste0("CREATE INDEX IDchanne ON " , Dataset, " (channel);"))
    #     DBI::dbGetQuery(SQLite.con, paste0("CREATE INDEX IDname ON "   , Dataset, " (name);"))
    # }
    # looking for table _Board and _Sensors, saving in directory Confiiguration
    series <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)),
                        config = authenticate(user = User, password = Pass),
                        query = list(q = "SHOW SERIES"))
    series <- jsonlite::fromJSON(content(series, "text", encoding = "ISO-8859-1"), simplifyVector = TRUE, flatten = TRUE)
    series <- series$results$series[[1]]$values[[1]]
    series <- unique(sapply(strsplit(x = series, split = ","),function(x) x[1]))
    series <- series[grepl(pattern = paste(c("_Boards", "_Sensors"), collapse = "|"), x = series)]
    if (any(grepl(pattern = Dataset, series))) {
        Boards <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)),
                            config = authenticate(user = User, password = Pass, type = "basic"),
                            query = list(q = paste0("SELECT * FROM \"", Dataset,"_Boards\"")))
        Boards <- jsonlite::fromJSON(content(Boards, "text", encoding = "ISO-8859-1"), simplifyVector = TRUE, flatten = TRUE)
        names.Boards <- Boards$results$series[[1]]$columns[[1]]
        Boards <- data.table(Boards$results$series[[1]]$values[[1]])
        setnames(Boards,names.Boards)
        fwrite(Boards, file.path(dirname(dirname(SQLite.con@dbname)), "Configuration","Boards.cfg"))
        Sensors <- httr::GET(URLencode(paste0("http://",Host,":",Port,"/query?db=", Db)),
                             config = authenticate(user = User, password = Pass, type = "basic"),
                             query = list(q = paste0("SELECT * FROM \"", Dataset,"_Sensors\"")))
        Sensors <- jsonlite::fromJSON(content(Sensors, "text", encoding = "ISO-8859-1"), simplifyVector = TRUE, flatten = TRUE)
        names.Sensors <- Sensors$results$series[[1]]$columns[[1]]
        Sensors <- data.table(Sensors$results$series[[1]]$values[[1]])
        setnames(Sensors,names.Sensors)
        fwrite(Sensors, file.path(dirname(dirname(SQLite.con@dbname)), "Configuration","Sensors.cfg"))
    }
    # Disconnect SQLite.con
    dbDisconnect(conn = SQLite.con)
    futile.logger::flog.info(paste0("[Down_Influx] the dates airsenseur.db go until ", format(LastDate, "%Y-%m-%d %H:%M"), ", with ", Dataset.N, " records for the table ", paste0(Dataset,"_cast")))
    cat("-----------------------------------------------------------------------------------\n")
    cat("\n")
    return(list(Influx.TZ = Influx.TZ,LastDate = LastDate))
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
    # Dependence        : Load_Packages
    ### Still need adding when the AirSensEUR is switched on and off, when the name of sensors are changed and when it is at the Reference Stations
    cat("\n")
    cat("-----------------------------------------------------------------------------------\n")
    #------------------------------------------------------------------------------CR
    # Installing necessary packages
    #------------------------------------------------------------------------------CR
    # Both RSQLite and sqldf (and others too) are loaded by library(sqldf), so it is enough to instal sqldf
    list.Packages <- c("sqldf", "openair", "reshape","tidyverse", "data.table")
    Load_Packages(list.Packages)
    #------------------------------------------------------------------------------CR
    # AirsensEur.db exists? creating the db or just the connect to the db
    #------------------------------------------------------------------------------CR
    if (file.exists(name.SQLite)) # airsenseur.db exists
        futile.logger::flog.info(paste0("[Sqlite2df] ", name.SQLite, " exists.")) else  # airsenseur.db does not exist
            stop(futile.logger::flog.error(paste0("[Sqlite2df] ", name.SQLite, " does not exist. The script is stopped.")))
    #------------------------------------------------------------------------------CR
    # Checking table Dataset in airsenseur.db
    #------------------------------------------------------------------------------CR
    SQLite.con <- dbConnect(SQLite(), dbname = name.SQLite)
    # Checking if the SQLite.con database and the table Dataset exists?
    if (dbExistsTable(SQLite.con, paste0(Dataset,"_Cast"))) {
        futile.logger::flog.info(paste0("[Sqlite2df] ", name.SQLite, " includes the table ", paste0(Dataset,"_Cast")," with columns: ",
                                        paste0(dbListFields(SQLite.con, paste0(Dataset,"_Cast")), collapse = ", ")))
    } else stop(futile.logger::flog.error(paste0("[Sqlite2df] There is no table called ", paste0(Dataset,"_Cast"), " in ", name.SQLite, ". The scipt is stoped.")))
    #------------------------------------------------------------------------------CR
    # Reading local airsenseur.db in slice of Page records - from last data of InfluxData in DownloadSensor to only add the necessary data
    #------------------------------------------------------------------------------CR
    futile.logger::flog.info(paste0("[SQLite2df] reading table ", paste0(Dataset,"_Cast")))
    # Initial values
    Download.N  <- 0
    SQL.Total.N <- DBI::dbGetQuery(SQLite.con, paste0("SELECT max(rowid) FROM \"", paste0(Dataset,"_Cast"), "\""))[1,1]
    if (!is.null(DownloadSensor$DateEND.Influx.prev)) { 
        # the table paste0(Dataset,"_Cast") exists in airsenseur.db
        futile.logger::flog.info(paste0("[SQLite2df] InfluxData already exists."))
        # Counting the row number where to add records in paste0(Dataset,"_Cast")
        futile.logger::flog.info(paste0("[SQLite2df] looking for the first date in InfluxData.Rdata to append to airsenseur.db. This can be very long with large datasets ... TZ must be set"))
        if (!exists("Influx.TZ")) {
            futile.logger::flog.warn("[SQLite2df] the time zone TZ is not defined. It is set to \"UTC\"")
            Influx.TZ <- "UTC"
        } 
        # changing FirstDate timezone from the value in DownloadSensor to the local timezone called Influx.TZ
        FirstDate <- as.POSIXct(DownloadSensor$DateEND.Influx.prev, tz = "UTC", usetz = TRUE) # attr(FirstDate, "tzone") <- "UTC"
        FirstDate <- format(FirstDate, tz = Influx.TZ, usetz = TRUE)
        Dataset.N <- DBI::dbGetQuery(SQLite.con, paste0("SELECT min(rowid) FROM \"", paste0(Dataset,"_Cast"), "\" WHERE datetime(time) >= '", # >= instead of > to recalculate the last average
                                                        FirstDate,"';"))[1,1]
        if (is.na(Dataset.N)) return(futile.logger::flog.warn("[SQLite2df] there are no new data in airSenseur.db to add to InfluxData.Rdata and InfluxData.csv. The script is stopped"))
    } else {# the table Dataset exists in airsenseur.db
        # There are no records in AirSensEUR$Dataset
        futile.logger::flog.info("[SQLite2df] INFO, file InfluxData.csv does not exist. Reading all data of airsenseur.db.")
        Dataset.N  <- 0
    }
    #DBI::dbGetQuery(SQLite.con, paste0("CREATE INDEX IDtime ON ",Dataset, "(time);"))
    # # getting the default Page of data to download
    if (is.null(Page)) Page <- 500000
    futile.logger::flog.info(paste0("[SQLite2df] reading ",(SQL.Total.N - Dataset.N)," records"))
    Values_db <- data.table(dbReadTable(SQLite.con, paste0(Dataset,"_Cast"),check.names = FALSE))
    data.table::set(Values_db, j = "time", value =  ymd_hms(Values_db[["time"]], tz = Influx.TZ))
    setkey(Values_db, "time")
    # resuming Page for tabulating values
    if (exists("Old_Page")) Page <- Old_Page
    # dbReadTable(SQLite.con, Dataset) # dbReadTable loses the decimal of coordinates
    futile.logger::flog.info(paste0("[SQLite2df] Disconnecting ", name.SQLite))
    futile.logger::flog.info(paste0("[SQLite2df] The table ", paste0(Dataset,"_Cast"), " has ", nrow(Values_db), " new records and ", length(unique(Values_db$time)), " new unique TimeStamps"))
    # reading Channel.names
    # for recognizing names of sensors
    Channel.names <- unique(data.table::data.table(dbReadTable(SQLite.con, name = "Channel.names", check.names = FALSE))[, c("channel","name")])
    if (exists("Adding"))      rm(Adding)
    if (exists("Download.N"))  rm(Download.N)
    if (exists("SQL.Total.N")) rm( SQL.Total.N)
    # Closing connection
    dbDisconnect(SQLite.con)
    #------------------------------------------------------------------------------CR
    # Defining names and variables for meteo
    #------------------------------------------------------------------------------CR
    Meteo.names.change  <- data.frame(Influx.names  = c(          "SHT31HE",             "Humid",     "SHT31TE", "Tempe"      ,"Temp"        ,                "Press", "BMP280"              ),
                                      General.names = c("Relative_humidity", "Relative_humidity", "Temperature", "Temperature", "Temperature", "Atmospheric_pressure", "Atmospheric_pressure"),
                                      stringsAsFactors = FALSE)
    ASE_Status          <- data.frame(Influx.names  = c("SHT31TI"        , "SHT31HI"              , "L2942CUR"             , "L2942VOL"    , "L4156STA"),
                                      General.names = c("Temperature_int", "Relative_humidity_int", "Batt_Cumulated_Charge", "Batt_Voltage", "Batt_Charge_Status"),
                                      stringsAsFactors = FALSE)
    #------------------------------------------------------------------------------CR
    # Adding final name (Temperature, Relative ...)
    #------------------------------------------------------------------------------CR
    futile.logger::flog.info("[SQLite2df] looking for the names of meteorological sensors and ASE Status using the sensor names")
    for (i in Meteo.names.change$Influx.names[Meteo.names.change$Influx.names %in% Channel.names$name]) {
        i.rows <- which(Channel.names$name == i)
        # setting column variables in Channel.names with names of Meteo.names.change
        if (!"Variables" %in% names(Channel.names)) {
            Channel.names[i = i.rows, Variables := Meteo.names.change[Meteo.names.change$Influx.names == i,"General.names"]]
        } else set(Channel.names, i = i.rows, j = "Variables", value = Meteo.names.change[Meteo.names.change$Influx.names == i,"General.names"])
    }
    for (i in ASE_Status$Influx.names[ASE_Status$Influx.names %in% Channel.names$name]) {
        i.rows <- which(Channel.names$name == i)
        # setting column variables in Channel.names with names of Meteo.names.change
        if (!"Variables" %in% names(Channel.names)) {
            Channel.names[i = i.rows, Variables := ASE_Status[ASE_Status$Influx.names == i,"General.names"]]
        } else set(Channel.names, i = i.rows, j = "Variables", value = ASE_Status[ASE_Status$Influx.names == i,"General.names"])
    }
    futile.logger::flog.info("[SQLite2df] INFO, looking for the names of all sensors and channel numbers from a list of sensor names")
    if (is.null(asc.File)) {
        # In case of having more than one sensor name on the same channel number in Values_db and thus Channel.names
        for (i in Channel.names$channel) {
            futile.logger::flog.info(paste0("[SLite2df] looking for different sensor models at channel ",i))
            # Taking the last name if sensors have been replaced
            if (anyDuplicated(Channel.names$channel[Channel.names$channel == i])) {
                futile.logger::flog.warn(paste0("[SLite2df] There are different sensor names at channel",i,". Sensors ",c(Channel.names$name[Channel.names$channel == i])))
                data.table::set(Channel.names, i = i, j = "name", value = paste(Channel.names$name[Channel.names$channel == i], collapse = "!"))
                # Setting the name of the sensor to the last one with same channel number
                if (grepl(pattern = "!", x = Channel.names$Sensor.names[i])) {
                    Set(Channel.names, j = Sensor.names[i], value = tail(unlist(strsplit(Channel.names$name[i], split = "!")), n = 1))
                    cat(paste0("[SLite2df] WARNING the name of the sensor of channel ", i, " has been changed. The script is assuming that the sensor model type was not changed during use and it is  ",
                               Channel.names$Sensor.names[i], ", the last one."), sep = "\n")}}}
    } else {
        # Set Channel.names$name, Values_db$name and Values_db$Pollutants based on the shield config file of the chemical sensor board. Sensor shall be in channel 1, 2, 3 and 4!!!
        for (i in which(!is.na(asc.File$name.sensor))) {
            # Setting name of sensor from file base on channel number - 1
            cat(paste0("[SLite2df] INFO setting name.sensor and gas.sensor using the shield config file for sensor ", i), sep = "\n")
            # updating the model of sensor in df Channel.names corresponding to sensor in asc.File based on channel number
            set(Channel.names, i = which(Channel.names$channel == (i-1)), j = "name", value = asc.File$name.sensor[i])}}
    # Defining names and variables for gas sensors - Used the same names of variables as in SOS for compatibility reasons
    Sensor.names <- list(Nitrogen_dioxide      = c("NO2_B43F_P1", "NO2_A43F_P1","no2_b43f", "NO2-B43F", "NO2B43F", "NO2_B43","NO2_M20", "NO2_C1", "NO2_C25", "NO2/C-20", "NO2_3E50", "NO23E50", "NO2", "S1"),
                         Carbon_monoxide       = c("CO_A4_P1"   , "CO_B4_P1","CO-B4", "CO-A4", "CO_MF200","CO/MF-200", "CO/MF-20", "CO-MF200", "CO_C200", "CO_CF200", "CO_3E300","CO3E300", "CO","COMF200","CO-A4 O","COA4", "S2"),
                         Ozone                 = c("OX_A431_P1" , "OX_B431_P1","O3/M-5", "O3-B4", "AX-A431", "OX-A431", "OX_A431", "O3-A431", "O3_M5", "O3_C5", "O3_C100", "O3-M5", "o3_m_5", "O3_3E1F", "O33EF1", "O3", "O3E100", "S3"),
                         Nitric_oxide          = c("NO_B4_P1"   , "NO_A4_P1"  , "NO-B4", "NOB4_P1","NOB4", "NO_M25", "NO_C1", "NO_C25","NO/C-25", "NO3E100", "NO_3E100", "NO", "No Sensor", "S4"),
                         Sulfur_dioxide        = c("SO2_B4_P1"  , "SO2_A4_P1" , "SO2_M20", "SO2_M20", "SO2_MF20", "SO2_C1", "SO2_C20", "SO2_CF20"),
                         Ammonia               = c("NH3_MR100"  , "NH3_CR50") ,
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
    # The last sensor model type is used
    #------------------------------------------------------------------------------CR
    for (i in 1:length(Sensor.names)) {
        for (j in which(Sensor.names[[i]] %in% Channel.names$name)) {
            irows <- which(Channel.names$name == Sensor.names[[i]][j])
            if (length(irows) > 0) set(Channel.names, i = irows, j = "Variables", value = names(Sensor.names)[i])}}
    cat("[SQLite2df] INFO, sensors found in the airsenseur.db\n")
    print(cbind(Channel.names, lubridate::ymd_hms(Values_db$time[as.numeric(row.names(Channel.names))]) ))
    # Setting Values_db$Pollutants that gives the correct Polluants names even if the sensors are changed of position during use, not for chemical sensors, already done
    for (i in unique(Channel.names$name)) {
        # setting correct colnames in values_db for Meteo.names. delete if not recognized
        cat(paste0("[SLite2df] INFO setting Values_db$Pollutants to ", unique(Channel.names$Variables[which(Channel.names$name == i)])," using the shield config file for sensor ", i), sep = "\n")
        # Sensor.rows <- which(Values_db$name == i)
        Sensor.Columns <- which(names(Values_db) == i)
        if (length(Sensor.Columns) > 0) setnames(Values_db, i, unique(Channel.names$Variables[which(Channel.names$name == i)])) else Values_db[,(i) := NULL]
    }
    #------------------------------------------------------------------------------CR
    # Putting data in tabulated dataframe
    #------------------------------------------------------------------------------CR
    # Aggregating in tabulated form. Discarding 0s in coordinates and altitude to avoid error when averaging
    set(Values_db, i = which(Values_db$altitude  == 0), j = "altitude" , value = rep(NA, length(which(Values_db$altitude  == 0))))
    set(Values_db, i = which(Values_db$longitude == 0), j = "longitude", value = rep(NA, length(which(Values_db$longitude == 0))))
    set(Values_db, i = which(Values_db$latitude  == 0), j = "latitude" , value = rep(NA, length(which(Values_db$latitude  == 0))))
    if (!"POSIXct" %in% class(Values_db$time)) data.table::set(Values_db, j = "time", value =  ymd_hms(Values_db[["time"]], tz = Influx.TZ))
    if (!haskey(Values_db)) setkey(Values_db, "time")
    remove(Sensor.names, Channel.names, Meteo.names.change)
    # Transforming column time in POSIX with the correct time zone (UTC), changing name to date
    if (is.null(Influx.TZ)) {
        cat("[SQLite2df] INFO, Converting datetime from character to POSIX format, ERROR time zone is not set for InfluxDB. Using UTC.\n")
        if (!"POSIXct" %in% class(Values_db$time)) Values_db$time <- lubridate::ymd_hms(Values_db$time, tz = "UTC")
    } else{
        cat(paste0("[SQLite2df] INFO, Converting Values_db$time from character to POSIX format, timezone is ", Influx.TZ), sep = "\n")
        #Values_db$time <- as.POSIXct(strptime(Values_db$time, format = "%Y-%m-%d %H:%M:%S", tz = Influx.TZ))
        if (!"POSIXct" %in% class(Values_db$time)) Values_db$time <- lubridate::ymd_hms(Values_db$time, tz = Influx.TZ)
    }
    # Change "time" to "date" to use OpenAir
    setnames(Values_db, "time", "date")
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
        } else cat(paste0("[SQLite2df] INFO: there is no previously saved Influx.Rdata.file. Missing InfluxData and Down.Influx request of sensor data download ."), sep = "\n")
    }
    if (!Complete) {
        if (exists("Values_db_Mins") && !is.na(Values_db_Mins) && nrow(Values_db_Mins) > 0) {
            cat("[SQLite2df] INFO, returning newly downloaded sensor data.\n")
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
                return(Values_db_Mins)}}}
}
#=====================================================================================CR
# MG : Download Reference Data retrieving
#=====================================================================================CR
Down_Ref <- function(Reference.name, urlref, UserMins, DownloadSensor, AirsensWeb, naStrings = NULL, WDoutput = NULL, ref.tzone = "UTC",
                     FTPMode = "ftp",
                     RefSOSname = NULL, Ref.SOS.name = NULL, RefSOSPollutants = NULL, RefSOSDateIN = NULL, RefSOSDateEND = NULL,
                     Ref__a_i_p__name = NULL, User__a_i_p__ = NULL, Pass__a_i_p__ = NULL, Ref__a_i_p__Organisation = NULL,
                     Ref__a_i_p__Station = NULL, Ref__a_i_p__Pollutants = NULL, Ref__a_i_p__DateIN = NULL, Ref__a_i_p__DateEND = NULL,
                     csvFile = NULL, csvFile.sep = NULL, csvFile.quote = NULL, Old.Ref.Data = NULL, coord.ref = NULL, Ref.Type = "Ref", shiny = TRUE) {
    # Reference.name        = Name of for Reference station
    # urlref                = Vector of URIs linking to csv files with the reference data. Frst row: header with variable names as in ASEConfig.R
    #                         and one column of date (DateTime), errors and NAs as -999.99, only one column may include one of the string c("date","time","Date", "Time", "DATE", "TIME")
    #                         ftp uri for reference data retrievals. The link shall point to a vector of character with csv files, with headers with the names of variable
    #                         One header shall includes the string DateTime of dates
    # UserMins              = periodicity of data requested after final data treatment
    # nastring              = a string character giving wrong values
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
    # RefSOSname            = Reference station SOS Rest API URL
    # Ref.SOS.name          = SOS ID of the Reference station
    # RefSOSPollutants      = Character vector, list of pollutants to download. Default is NULL. In this case pollutants are downaloded.
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
    # return                = data.table Ref with the reference data
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
    # Add new reference to be recognized if needed
    Reference.names        <- list(date         = c("date","time","Date", "Time", "DATE", "TIME", "DateTime", "datetime"),
                                   time         = c("time" , "Time"),
                                   Ref.CO_ppm   = c("CO"   , "Carbon monoxide (air)", "co", "Ref.CO_ppm", "CO_ppm", "carbon monoxide", "Carbon Monoxide"),
                                   Ref.NO2      = c("NO2"  , "Nitrogen dioxide (air)", "Ref.NO2", "nitrogen dioxide", "nitrogen dioxide caps","nitrogen dioxide BLC", "Nitrogen dioxide"),
                                   Ref.O3       = c("O3"   , "Ozone (air)", "Ref.O3", "ozone", "Ozone"),
                                   Ref.NO       = c("NO"   , "Nitrogen monoxide (air)", "Ref.NO", "nitrogen monoxide", "nitrogen monoxide BLC", "Nitrogen monoxide")  ,
                                   Ref.NOx      = c("NOx"   , "nitrogen oxides", "nitrogen oxides BLC")  ,
                                   Ref.SO2      = c("SO2"  , "Sulfur dioxide (air)", "Ref.SO2", "sulfur dioxide", "Sulphur dioxide"),
                                   Ref.PM1      = c("PM1" , "Particulate matter < 1 ?m (aerosol)", "Particulate Matter < 1 ?m", "Ref.PM1"),
                                   Ref.PM2.5    = c("PM2.5", "Particulate matter < 2.5 ?m (aerosol)", "Particulate Matter < 2.5 ?m", "Ref.PM2.5"),
                                   Ref.PM10     = c("PM10" , "Particulate matter < 10 ?m (aerosol)", "Particulate Matter < 10 ?m", "Ref.PM10", "TEOM"),
                                   Ref.Temp     = c("Temperature", "Sample_air temperature", "AirTemp", "T1"), # T1 for VITO, it is in °C
                                   Ref.RH       = c("Relative_humidity", "RH", "relative humidity"),
                                   Ref.Press    = c("Atmospheric_pressure", "AirPress", "PP"), # PP for VITO, it is mbars
                                   Ref.SolarRad = "Solar Radiation",
                                   Ref.WV       = c("WSAve", "VM"), # VM for VITO it is in m/s
                                   Ref.WD       = c("WDAve", "DD"), # DD for VITO , it is in degrees
                                   Ref.Rain     = c("RainAcc", "RR"), # RR for VITO, it is in mm
                                   Ref.CO2      = c("CO2", "co2"))
    # set DateIN for data retrieving, either from mindateRef (min Influx and SOS date) or last date in previous DataFrame
    # download of Influx data exists
    if (!is.null(DownloadSensor$mindateRef)) {
        DateIN  <- DownloadSensor$mindateRef
        DateEND <- DownloadSensor$maxdateRef
    } else if (!is.null(DownloadSensor$DateIN.Ref.prev)) {
        DateIN  <- DownloadSensor$DateIN.Ref.prev
        DateEND <- DownloadSensor$DateIN.Ref.prev
    } else {
        DateIN  <- as.POSIXct("2015-12-01 00:00", tz = ref.tzone) 
        # Setting end date to curent date (the time zone of the refrence shall be in UTC, normally it is allways like that)
        if (exists("ref.tzone")) {
            DateEND <- as.POSIXct(Sys.time(), tz = ref.tzone)
        } else DateEND <- as.POSIXct(Sys.time(), tz = "UTC")
    } 
    #Set time interval, with function interval of package lubridate
    date <- lubridate::interval(DateIN, DateEND, tzone = ref.tzone) # tzone=user.tzone
    # the function  interval returns a variable of class lubridate
    cat(paste0("[Down_Ref] INFO, Time zone for reference data: ", date@tzone), sep = "\n")
    # creating returning data frame Ref
    Ref       <- setDT(data.frame(date = seq(date@start, length = date@.Data/minSec, by = paste0(toString(UserMins)," ","min")),
                                  row.names = NULL, check.rows = FALSE,
                                  check.names = TRUE,
                                  stringsAsFactors = FALSE))
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
        # Downloading according to FTPMode
        if (FTPMode == "ftp" || FTPMode == "csv") {
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
                                        animation = FALSE)
                                }
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
                                    animation = FALSE)
                            }
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
                            animation = FALSE)
                    }
                }
            } else {
                if (FTPMode == "csv") {
                    cat(paste0("[Down_Ref] INFO, loading local file for reference data, file ", csvFile, "\n"))
                    if (grepl(".csv", csvFile, fixed = T)) {
                        # if you load a .csv file:
                        Reference.i <- fread(file        = csvFile,
                                             header      = TRUE,
                                             na.strings  = naStrings,
                                             quote       = csvFile.quote,
                                             stringsAsFactors = FALSE)
                    } else if (grepl(".Rdata", csvFile, fixed = T)) {
                        # if you load a .Rdata file:
                        # loaded Rdata with unknown name dataframe
                        Reference.i <- load_obj(csvFile)
                        if (!is.data.table(Reference.i)) Reference.i <- data.table::data.table(Reference.i)
                        # removing un-necessary columns of Reference.i
                        # possible names
                        all.names <- character(0)
                        for (i in seq_along(Reference.names)) all.names <- c(all.names, unlist(Reference.names[[i]]))
                        Reference.i <- Reference.i[,which(names(Reference.i) %in% all.names)]
                    } else {
                        my_message <- paste0("[Down_Ref()] ERROR, unrecognized file type for \n reference data .\n")
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
                cat(paste0("[Down_Ref] INFO, ", Ref.SOS.name," reference data retrieving"), sep = "\n")
                # Checking internet connection availability
                if (curl::has_internet()) {
                    #URL <- unlist(strsplit(unlist(strsplit(gsub('http://', '', RefSOSname), split = '/'))[1], split = ':'))[1]
                    if (PingThisSite(RefSOSname)) {
                        cat(paste0("[Down_Ref] INFO; ping to ", RefSOSname, " Ok"), sep = "\n")
                    } else{
                        cat(paste0("[Down_Ref] ERROR: you have an internet connection but cannot ping to ",RefSOSname,
                                   ". It may be because of security reasons or download cannot be carried out.\n"))
                    }
                } else {
                    return(cat(paste0("[Down_Ref] ERROR: no internet connection. Download cannot be carried out."), sep = "\n"))
                }
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
                #------------------------------------------------------------------------------CR
                # Downloading sensor data
                #------------------------------------------------------------------------------CR
                # Determining DateIN and DateEND for data download with a lubridate::interval
                DateEND <- RefSOSDateEND
                cat(paste0("[Down_Ref] INFO, last date in Reference data to be downloaded is: ", DateEND, "\n"))
                # set DateIN for data retrieving, either from origin or last date in previous DataFrame
                # DownloadSensor exists: check if we have a "DateEND.SOS.prev"
                if ("DateEND.Ref.prev" %in% objects(DownloadSensor)) {
                    # DateEND.Ref.prev exists: Check if NULL
                    if (!is.null(DownloadSensor$DateEND.Ref.prev)) {
                        DateIN  <- max(DownloadSensor$DateEND.Ref.prev, RefSOSDateIN, na.rm = TRUE)
                    } else DateIN  <- RefSOSDateIN
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
                # SOS downloading, added + 1 to be able to download current day
                # Select the timeseries of the station Ref.SOS.name
                ts <- sensorweb4R::timeseries(Endpoint, station = sta)
                cat(paste0("Timeseries at the station: ", sensorweb4R::label(ts),"\n"))
                # Keeping only selected pollutants
                if (!is.null(RefSOSPollutants)) {
                    Keep.ts <- sapply(sensorweb4R::label(ts), function(i) {
                        for (j in RefSOSPollutants) if (grepl(j, i)) return(TRUE) else if (j == tail(RefSOSPollutants, n = 1)) return(FALSE)})
                    ts <- ts[Keep.ts]
                } 
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
                        Buffer.DT      <- data.table(date=Buffer[[1]]@time)
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
                            DateEND.partial <- DateEND.partial + 24 * 60 * 60 * Duration
                        }
                    }
                }
                # interval of time for the get next data of SOS
                date.partial <- lubridate::interval(DateIN.partial, DateEND.partial)
                cat(paste0("[Down_Ref] INFO, Time zone for reference data: ", date.partial@tzone), sep = "\n")
            } else if (FTPMode == "a_i_p") {
                # Sensor Data retrieving at apiEndpoint
                cat("\n")
                cat("-----------------------------------------------------------------------------------\n")
                cat(paste0("[Down_Ref] INFO, ", Ref__a_i_p__name," reference data retrieving"), sep = "\n")
                cat(paste0("[Down_Ref] INFO, downloading from ", Ref__a_i_p__DateIN, " to ", Ref__a_i_p__DateEND), sep = "\n")
                Reference.i <- a_i_p_data(URL          = Ref__a_i_p__name,
                                          username     = User__a_i_p__,
                                          password     = Pass__a_i_p__,
                                          organisation = Ref__a_i_p__Organisation,
                                          station      = Ref__a_i_p__Station,
                                          start        = Ref__a_i_p__DateIN,
                                          end          = Ref__a_i_p__DateEND + 1,
                                          param        = Ref__a_i_p__Pollutants,
                                          Time_zone    = ref.tzone)
            }
        }
        # Checking if data are available
        if (nrow(Reference.i) == 0) {
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
            if (any(c("date","time","Date", "Time", "DATE", "TIME", "DateTime") %in% names(Reference.i))) {
                # checking if there is more than 1 field with "date","time","Date", "Time", "DATE", "TIME"
                if (length(which(c("date","time","Date", "Time", "DATE", "TIME", "DateTime") %in% names(Reference.i))) == 1 ) {
                    # setting name of timedate column as "date" for openair
                    names(Reference.i)[which(c("date","time","Date", "Time", "DATE", "TIME", "DateTime") %in% names(Reference.i))] <- "date"
                    # convert date to POSIX with time zone set in shiny
                    if (!is.POSIXct(Reference.i$date)) {
                        if (grepl("Z", Reference.i$date[1])) {
                            data.table::set(Reference.i, j = "date", value = lubridate::ymd_hms(Reference.i$date, tz = ref.tzone))
                        } else Reference.i$date <- as.POSIXct(Reference.i$date,  tz = ref.tzone,
                                                              tryFormats = c("%Y-%m-%d %H:%M:%OS",
                                                                             "%Y/%m/%d %H:%M:%OS",
                                                                             "%Y-%m-%d %H:%M:%S",
                                                                             "%Y-%m-%d %H:%M",
                                                                             "%m/%d/%Y %H:%M",
                                                                             "%Y-%m-%d",
                                                                             "%m/%d/%Y")) # "%m/%d/%Y %H:%M", strptime removed with the format as this may cause a crash, but slower operation
                    }
                    # Convert all other columns to numeric if they are not excepts coordinates and keep known columns
                    for (j in names(Reference.i)[-which(names(Reference.i) %in% c("date", "Ref.Long", "Ref.Lat"))]) {
                        if (class(Reference.i[[j]]) != "numeric") Reference.i[[j]] <- as.numeric(Reference.i[[j]])
                        if (!j %in% unlist(Reference.names, use.names = F)) Reference.i[, (j):=NULL] else {
                            if (is.null(Ref.Type) || Ref.Type == "Ref") {
                                name.detected <- names(Reference.names)[which(sapply(Reference.names, function(i) any(i %in% j)))]
                                data.table::setnames(Reference.i, j, name.detected)
                            } 
                        }
                    } 
                    # aggregate Reference.i with mean over UserMins
                    Periodicity <- threadr::detect_date_interval(Reference.i$date, skip = 3, n = 50) / 60
                    if (Periodicity != UserMins) {
                        Reference.i <- DF_avg(Reference.i, width = UserMins)
                    } else if (is.POSIXct(DownloadSensor$DateIN.Ref.prev) && is.POSIXct(DownloadSensor$DateEND.Ref.prev) &&
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
                        Reference.i <- DF_avg(Reference.i, width = UserMins)
                    }
                    # Setting Reference names (change names of pollutants adding Ref.)
                    if (Ref.Type %in% c("Bin.DMPS", "Bin.APS", "GRIMM")) {
                        # Adding lable to pollutants which are not in Reference.names
                        names.not.Ref <- names(Reference.i)[grep(pattern = paste(c("date","Ref.", "Bin.DMPS.", "Bin.APS.", "GRIMM."), collapse = "|"), x = names(Reference.i), invert = TRUE)]
                        names(Reference.i)[which(names(Reference.i) %in% names.not.Ref)] <- sapply(seq_along(names.not.Ref), function(k) paste0(Ref.Type, ".", names.not.Ref[k]))
                    }
                    # matching dates,
                    
                    Common.dates <- which(Reference.i$date %within% date)
                    if (length(Common.dates) > 0) {
                        Ref <- data.table::merge.data.table(Ref, Reference.i[Common.dates], by = "date", all.x = T)
                        # replacing NA with last non Na values is UserMins is lower than the periodicty of the downaloded measurements
                        if (Periodicity > UserMins) {
                            for (i in seq_len(nrow(Reference.i[Common.dates]))) {
                                NA.Interval <- which(Ref$date %within% lubridate::interval(Reference.i[Common.dates, date][i] - (Periodicity - UserMins)*60,Reference.i[Common.dates, date][i] - UserMins*60))
                                if (length(NA.Interval) > 0) {
                                    cat(paste0("Filling Na rows",Reference.i[Common.dates, date][i], "\n"))
                                    Values <- Reference.i[Common.dates, .SD, .SDcols = names(Ref)[-which(names(Ref) == "date")]][i]
                                    Values <- lapply(Values, function(i) rep(i, length(NA.Interval)))
                                    data.table::set(Ref, i = NA.Interval, j = names(Ref)[-which(names(Ref) == "date")], value = Values)
                                    rm(Values)
                                }
                            }
                        }
                    } else rm(Ref)
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
                        animation = FALSE)
                }
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
                    animation = FALSE)
            }
        }
    }
    # removing raws filled with NA/NaN to Ref to avoid to add empty lines that will not be updated later with a new Download
    if (exists("Ref")) {
        Cols.All.NA <- names(Ref)[!names(Ref) %in% c("date","Ref.Long","Ref.Lat")]
        Full.Nas <- which(apply(Ref[,Cols.All.NA, with = F], MARGIN = 1, function(x) all(is.na(x) | is.nan(x))))
        if (!is.null(Full.Nas) & length(Full.Nas) > 0 ) Ref <- Ref[-Full.Nas,]
        print(str(Ref), Quote = FALSE)
    }
    if (exists("Ref")) {
        if (length(names(Ref)) > 0 ) {
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            if(!is.data.table(Ref)) Ref <- data.table(Ref, key = "date")
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
# 170609 MG : Pinging WEB site *** No more used since 2020-02-01 replaced with curl::has_internet()
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
# 160418 MGV: Validation.tool       function for validations
#=====================================================================================CR
Tidy_Model.i <- function(Model.i, WDoutputMod, nameModel, Mod_type = NULL, Include.Model = FALSE, SAVE = TRUE)
{
    # https://stackoverflow.com/questions/24559099/vectorize-environment-access-in-r
    ENV <- new.env()
    # https://stackoverflow.com/questions/42230920/saverds-inflating-size-of-object
    # Model.i$Equation includes an environment that might be big. use format to all keep the charater equation
    if (!is.null(Mod_type) && Mod_type == "Linear.Robust") {
        ENV[["Model"]] <- list(Tidy = tidy(Model.i), Augment = augment(Model.i, data = data.frame(x = Model.i$x[,2], y = Model.i$y)), Glance = glance(Model.i), Call = Model.i$call,
                               Coef = coef(Model.i), Equation = format(Model.i$Equation))
    } else ENV[["Model"]] <- list(Tidy = tidy(Model.i), Augment = augment(Model.i), Glance = glance(Model.i), Call = Model.i$call,
                                  Coef = coef(Model.i), Equation = format(Model.i$Equation))
    if (Include.Model) ENV[["Model"]][["InitModel"]] <- Model.i
    if (SAVE) list.save(ENV[["Model"]], file = file.path(WDoutputMod, paste0(nameModel,".rdata")))
    return(ENV[["Model"]])
}
Validation.tool <- function(General, DateIN, DateEND, DateINCal = DateIN, DateENDCal = DateEND, name.gas, model.log, nameGasRef, nameGasVolt, nameGasMod,
                            unit.ref, unit.sensor, Sens.raw.unit = NULL, Reference.name, AirSensEur.name, name.sensor,
                            timeseries.display, DateINPlot = DateIN, DateENDPlot = DateEND,
                            WDoutputMod, WDoutput, WDoutputStats, process.step, mod.eta.model.type, Multi.File = NULL,
                            eta.model.type, remove.neg = TRUE, Covariates = NULL, Plot_Line = TRUE, PlotCal = TRUE, Auto.Lag = FALSE, Verbose = TRUE, Include.Model = FALSE, SAVE = TRUE) {
    #INput:
    # General              : dataframe- containing all data within selected dates
    # DateIN/END           : as.POSIXct- datetime in and datetime out to start validation (adding 1 day to DateEND).
    # DateINCal/DateENDCal : Dates of previous calibration with which nameGasMod was calibrated. Default are DateIN/END.
    # DateINPlot/END       : as.POSIXct- datetime in and datetime out to plot time series, default are DateIN/END.
    # name.gas             : char() - gas component
    # model.log            : logic  - If true plotting times seires of raw data with timePlot()
    # nameGasRef           : char   - column of gas reference data
    # nameGasVolt          : char   - column of gas gas sensor data in volt or nA
    # nameGasMod           : char   - column of gas gas sensor data in same unit as reference
    # unit.ref/sensor      : char   - units (ppb or ppm)
    # Sens.raw.unit        : raw unit of sensors: V or nA
    # Reference.name       : char   - name of reference data
    # AirSensEur.name      : char   - name of airsensor data
    # timeseries.display   : logic  - True -> displays timeseries
    # name.sensor          : char   - name of specific gas sensor
    # WDoutputMod          : char   - directory to save computed models (not used if (!model.log))
    # WDoutput             : char   - directory to save plots from etalonnage
    # WDoutputStats        : char   - directory to save statistics for modelled data
    # process.step         : char   - variable to refer to the process status (e.g. calibration, modelling)
    # mod.eta.model.type   : char   - name for model in Etalonnage and Cal_line
    # Multi.File           : char, default is NULL, path.file of the config file used for calibration with multivariates
    # eta.model.type       : char   - name for evaluation in Etalonnage and Cal_line
    # Covariates           : List of covariates to calibrate, vector of characters, default is NULL
    # remove.neg           : logical, defaut TRUE, if TRUE discard negative from calibrated data after calibration
    # Plot_Line            : logical, default is TRUE. If TRUE the calibration line is added using par(new=TRUE) to an existaing scatterplot
    # PlotCal              : logical, defaut TRUE, if TRUE plot the calibrated data (scatterplot and timeseries) after calibration
    # Auto.Lag             : logical, default is FaLSE If Auto.Lag is TRUE, y is changed using the lag at which cross correlation between x and y is maximum using ccf( )
    # OUTPUT:
    # General               : with the whole data.table General modelled values
    
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
            if (!is.null(Relationships) && !identical(character(0), Relationships)) {
                timePlot(mydata = General[ date >= DateINPlot & date <= DateENDPlot + 1,], pollutant = Relationships, date.pad = TRUE, auto.text = FALSE, y.relation = "free",
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
        # checking completness of x, y and Covariates
        if (is.data.table(General)) {
            General <- General[date >= DateINCal & date < DateENDCal + 1]
            if (is.null(Covariates) || length(Covariates) == 0 || Covariates == "") {
                General <- General[complete.cases(General[,.SD,.SDcols = c(nameGasRef,nameGasVolt)])]
            } else General <- General[complete.cases(General[,.SD,.SDcols = c(nameGasRef,nameGasVolt,Covariates)])]
        } else if (is.data.frame(General)) {
            General <- General[date >= DateINCal & date < DateENDCal + 1]
            if (is.null(Covariates) || length(Covariates) == 0 || Covariates == "") {
                General <- General[complete.cases(General[,c(nameGasRef,nameGasVolt)])]
            } else General <- General[complete.cases(General[,c(nameGasRef,nameGasVolt,Covariates)])]}
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
        } else if (any(mod.eta.model.type %in% c("exp_kT_NoC", "exp_kT", "exp_kK", "T_power", "K_power"))) {
            if (is.null(Covariates) || length(Covariates) == "0" || Covariates == "") Covariates <- "Temperature"
            Degrees <-  1
            namesCovariates <- paste0(paste(Covariates,Degrees, sep = "-"),collapse = "&")
            Matrice         <- General[, .SD, .SDcols = Covariates]
            #names(Matrice)  <- namesCovariates
        } else if (any(mod.eta.model.type %in% c("BeerLambert"))) {
            Covariates <- c("Temperature", "Atmospheric_pressure")
            Degrees <-  c(1,-1)
            namesCovariates <- paste0(Covariates,collapse = "&")
            Matrice         <- General[, .SD, .SDcols = Covariates]
        } else if (any(mod.eta.model.type %in% c("Kohler"))) {
            namesCovariates <- "Relative_humidity"
            Matrice         <- General[, .SD, .SDcols = Covariates]
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
        # saving the model
        #nameModel  <- paste(AirSensEur.name,name.sensor,Sens.raw.unit,mod.eta.model.type,format(DateIN,"%Y%m%d"),format(DateEND,"%Y%m%d"),namesCovariates, sep = "__")
        nameModel  <- paste0(paste0(c(AirSensEur.name,name.sensor,Sens.raw.unit,mod.eta.model.type,format(DateIN,"%Y%m%d"),format(DateEND,"%Y%m%d"),namesCovariates), "__"), collapse = "")
        Model.i    <- Tidy_Model.i(Model.i, WDoutputMod, nameModel, Mod_type = mod.eta.model.type, Include.Model = Include.Model, SAVE = SAVE)
        # save scatterplots in files
        if (Plot_Line) {
            NameFile <- file.path(WDoutput, paste0(nameModel,"__",process.step,".png"))
            dev.copy(png, filename = NameFile, units = "cm", res = 300, width = 20, height = 20)
            dev.off() 
        }
    }
    if (PlotCal) {
        # Fill in General with modelled data, first reintialise nameGasMod
        if (nameGasMod %in% names(General)) General[, (nameGasMod) := NULL ] 
        Rows <- which(General$date >= DateIN  & General$date <= DateEND + 1 & !is.na(General[, nameGasVolt, with = F]))
        if (mod.eta.model.type == "Linear" || mod.eta.model.type == "Linear.Robust") {
            data.table::set(General, i = Rows, j = nameGasMod, value = (General[Rows,nameGasVolt,with = F] - Model.i$Coef[1]) / Model.i$Coef[2])
        } else if (mod.eta.model.type == "gam") {
            data.table::set(General, i = Rows, j = nameGasMod, 
                            value = predict(Model.i, newdata = data.frame(x = General[Rows, nameGasVolt, with = FALSE])), type = "response")
        }
        # Remove negative values when using the linear median regression (linear.robust). This is good to do
        if (remove.neg) {
            index.which <- which(General[(General$date >= DateIN & General$date <= DateEND + 1),nameGasMod, which = F] < 0., arr.ind = TRUE)
            if (length(index.which)>0) {
                data.table::set(General, i = index.which, j = nameGasMod, value = rep(NA_real_, times = length(index.which)))
                cat(paste0("Length of values < zero: ", length(index.which), " out of ",length(General[(General$date >= DateIN & General$date <= DateEND +1),nameGasMod])), sep = "\n")
                cat(paste0("Loss of ",format(length(index.which)/length(General[(General$date >= DateIN & General$date <= DateEND + 1),nameGasMod])*100), digit = 0, " [%] of data"), sep = "\n")
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
            chaine <- paste0(mod.eta.model.type, "_", format(DateINCal,"%Y%m%d"),"_",format(DateENDCal,"%Y%m%d"))    }
        if (remove.neg) negatif <- "_remove.neg_" else negatif <- "_"
        dev.copy(png,filename = file.path(WDoutput, paste0(AirSensEur.name,"_",name.sensor,"_","Calibrated", "_",eta.model.type,negatif,format(DateIN,"%Y%m%d"),
                                                           "_",format(DateEND,"%Y%m%d"),"_", chaine,".png"))
                 , units = "cm", res = 300, width = 25, height = 25)
        dev.off()
        # timeplots calibrated values
        if (timeseries.display) {
            if (model.log) {
                timePlot(General[ General$date >= DateINPlot & General$date <= DateENDPlot +1,], pollutant = c(nameGasRef,nameGasMod), group=TRUE, date.pad=TRUE
                         , main = paste0(AirSensEur.name, ": Calibrated ", name.sensor," from ",format(DateIN,"%d-%b-%y")," to "
                                         , format(DateEND,"%d-%b-%y"), " at ",Reference.name))
            } else {
                timePlot(General[ General$date >= DateINPlot & General$date <= DateENDPlot +1,], pollutant = c(nameGasRef,nameGasMod), group=TRUE, date.pad=TRUE
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
        gas.statistics <-modStats(General[ (General$date >= DateIN & General$date <= DateEND + 1),]
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
# 170609 MG : Selecting a fine name in window
#=====================================================================================CR
SearchFile <- function(dirCurrent = getwd(), Caption = "", Filters = matrix(c("ASEConfig", ".R"),1, 2, byrow = TRUE)) {
    # This function selects and return a file name in a window. It is compatible with Windows and Linux
    # The operating system is automatically detected.
    #
    # dirCurrent        : directory in which the window will list the available file
    # Filters           : a matrix of filename filters
    # return            : a file.path to the selected file
    # dependences       : Load_Packages
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
            Load_Packages("tcltk")
            FilePath <- tk_choose.files(default = file.path(dirCurrent,"ASEconfig*.R"), caption = Caption,
                                        multi = FALSE, filters = Filters,
                                        index = nrow(Filters))
        } else {
            # try selecting with JAVA jchoose.files()
            Load_Packages("rJava", "rChoiceDialogs")
            FilePath <- jchoose.files(default = file.path(dirCurrent,"ASEconfig*.R"), caption = Caption,
                                      multi = FALSE, filters = Filters,
                                      index = nrow(Filters), modal = canUseJavaModal())
        }
    }
    return(FilePath)
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
    # dependences       : Load_Packages
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
            Load_Packages(c("kimisc", "knitr")) # for thisfile()
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
    # dependences       : Load_Packages
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
    Load_Packages("BMS")
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
    Load_Packages("tcltk2")
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
                     PROXY, URL, PORT, LOGIN, PASSWORD,
                     Down.Influx = FALSE, Host, Port, User , Pass, name.SQLite, name.SQLite.old, Db, Dataset, Influx.TZ = NULL,
                     sens2ref, asc.File=NULL, InfluxData = NULL) {
    # DownloadSensor    : output of function Check_Download
    # Parameters PROXY:  PROXY, URL, PORT, LOGIN, PASSWORD see Down_INflux()
    # Parameters Influx: Down.Influx, Host, Port, User, Pass, name.SQLite, name.SQLite.old, Db, Dataset, Influx.TZ
    # Sqlite database  : name.SQLite,name.SQLite.old
    # sens2ref          : data.table or dataframe: Configuration of sensors, output of function CONFIG
    # asc.File          : dataframe, default is NULL, used for giving the correct name of the sensor
    # Influx.file       : charater vector, default is null, File path of Influx data file. Can be .csv or .Rdata
    # InfluxData        : data.table or dataframe, default is null. DataSet of sensor values
    cat("-----------------------------------------------------------------------------------\n")
    futile.logger::flog.info("[INFLUXDB] INFO: Downloading InfluxDB data")
    # Saving Influx Sensor data
    if (is.null(DownloadSensor$Influx.Rdata.file)) {
        DownloadSensor$Influx.Rdata.file <- file.path(WDoutput, "InfluxData.csv")
        # Influx.Rdata.file = file.path(WDoutput, "InfluxData.Rdata")
        # Influx.csv.file   = file.path(WDoutput, "InfluxData.csv"  )
    }
    if (DownloadSensor$Retrieve.data.Influx) {
        if (Down.Influx) {
            Init.DB <- lubridate::ymd_hms(DownloadSensor$DateEND.Influx.prev, tz = Influx.TZ)
            # downloading data from InfluxDB and updating airsenseur.db
            Influx.TZ <- Down_Influx(PROXY = PROXY, URL = URL  , PORT = as.numeric(PORT), LOGIN = LOGIN, PASSWORD = PASSWORD,
                                     Host = Host  , User = User, Port = as.numeric(Port), Pass = Pass, name.SQLite = name.SQLite, name.SQLite.old = name.SQLite.old,
                                     Db = Db      , Dataset = Dataset, Influx.TZ = Influx.TZ, use_google = FALSE, Page = 10000, Mean = as.numeric(UserMins))
            # if there are problems accessing port 443 for the google api to determine time zone add , use_google = FALSE
            # Sqlite2df returns only the new data from the AirSensEUR.db, if the whole set is needed add: Complete = TRUE in function Down_Influx
            if (file.exists(DownloadSensor$airsenseur.db.file) && 
                (length(Init.DB) == 0 || Influx.TZ[["LastDate"]] >= (Init.DB  + UserMins * 60) || !file.exists(DownloadSensor$Influx.Rdata.file))) {
                InfluxDataNew <- Sqlite2df(name.SQLite = name.SQLite, Dataset = Dataset, Influx.TZ = Influx.TZ[["Influx.TZ"]], 
                                           UserMins = UserMins, DownloadSensor = DownloadSensor, asc.File = asc.File, InfluxData = InfluxData)
            }
        } else cat(paste0("[INFLUXDB] INFO: Data download not requested.\n"))
    } else cat(paste0("[INFLUXDB] INFO: Data download is already up to date.\n"))
    # Trying to use the existing data or Influx.Rdata.file
    if (is.null(InfluxData) || is.na(InfluxData)) {
        if (file.exists(DownloadSensor$Influx.Rdata.file)) {
            if (extension(DownloadSensor$Influx.Rdata.file) == ".csv") {
                InfluxData <- fread(file = DownloadSensor$Influx.Rdata.file, na.strings = c("","NA", "<NA>"))
                if (!is.null(Influx.TZ) && Influx.TZ != "") {
                    data.table::set(InfluxData, j = "date", value =  ymd_hms(InfluxData[["date"]], tz = Influx.TZ[["Influx.TZ"]]))
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
            InfluxData <- rbindlist(list(InfluxData, InfluxDataNew), use.names = TRUE, fill = TRUE) # rbind.fill(InfluxData,InfluxDataNew)
            rm(InfluxDataNew)
        } else {
            InfluxData <- InfluxDataNew
            rm(InfluxDataNew)
        }
        if (extension(DownloadSensor$Influx.Rdata.file) == ".csv") {
            duplicated.Data <- which(duplicated(InfluxData$date, fromLast = T))
            if (length(duplicated.Data) > 0) InfluxData <- InfluxData[-duplicated.Data]
            fwrite(InfluxData     , file = DownloadSensor$Influx.Rdata.file, na = "NA")
        } else if (extension(DownloadSensor$Influx.Rdata.file) == ".Rdata") save(InfluxData, file = DownloadSensor$Influx.Rdata.file)
        cat(paste0("[INFLUXDB] INFO: Influx Sensor data saved in ", DownloadSensor$Influx.Rdata.file), sep = "\n")
    }
    # returning data
    if (!is.null(InfluxData) && !is.na(InfluxData)) {
        var.names.meteo <- c("Temperature", "Temperature_int","Relative_humidity", "Relative_humidity_int","Atmospheric_pressure")
        var.names.meteo <- var.names.meteo[var.names.meteo %in% names(InfluxData)]
        # setting the name of sensors
        var.names.sens  <- colnames(InfluxData)[-grep(pattern = paste0(c("date","_raw","gpsTimestamp","boardTimeStamp",  "channel", 
                                                                         "latitude", "longitude", "altitude"),collapse = "|"), x = colnames(InfluxData))]
        if (length(var.names.sens) == 0) {
            stop(paste0("[INFLUXDB] ERROR: no sensor variable downloaded for ", Dataset," InFluxDB. Please check in the INfluxDB client -> STOP"))
        } else cat(paste0("[INFLUXDB] INFO: Sensor variables existing in airsenseur.db: ", paste0(var.names.sens, collapse = ", "), ", with date timestamp and coordinates."), sep = "\n")
        # Setting the Sensor names
        var.names.Pollusens <- var.names.sens[-(which(var.names.sens %in% var.names.meteo))]
        fwrite(data.table(columns = var.names.meteo)    , file = file.path(dirname(DownloadSensor$WDinput), "Configuration","var.names.meteo.cfg"), na = "NA")
        fwrite(data.table(columns = var.names.Pollusens), file = file.path(dirname(DownloadSensor$WDinput), "Configuration","var.names.Pollusens.cfg"), na = "NA")
        fwrite(data.table(columns = var.names.sens)     , file = file.path(dirname(DownloadSensor$WDinput), "Configuration","var.names.sens.cfg"), na = "NA")
        cat("[INFLUXDB] INFO INFLUXDB returning list with InfluxData, var.names.meteo, var.names.Pollusens and var.names.sens\n")
        cat("-----------------------------------------------------------------------------------\n")
        return(list(InfluxData = InfluxData, var.names.meteo = var.names.meteo, var.names.Pollusens = var.names.Pollusens, var.names.sens = var.names.sens))
    } else {
        cat("-----------------------------------------------------------------------------------\n")
        return(cat("[INFLUXDB] ERROR no Influx data available\n"))
    }
}
#=====================================================================================CR
# 170721 MG : Downloading SOS data
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
                    if (extension(DownloadSensor$SOS.file) == ".csv") {
                        RefData <- fread(file = DownloadSensor$SOS.file, na.strings = c("","NA", "<NA>"))
                    } else if (extension(DownloadSensor$SOS.file) == ".Rdata") load(DownloadSensor$SOS.file)
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
            if (extension(DownloadSensor$SOS.file) == ".csv") {
                RefData <- fread(file = DownloadSensor$SOS.file, na.strings = c("","NA", "<NA>"))
            } else if (extension(DownloadSensor$SOS.file) == ".Rdata") load(DownloadSensor$SOS.file)
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
# 170721 MG : Downloading REFERENCE data
#=====================================================================================CR
REF      <- function(DownloadSensor, AirSensEur.name, DisqueFieldtestDir, UserMins,
                     Down.Ref, FTPMode, ref.tzone, InfluxData, SOSData, Reference.name, urlref, sens2ref,
                     RefSOSname = NULL, Ref.SOS.name = NULL, RefSOSPollutants = NULL, RefSOSDateIN = NULL, RefSOSDateEND = NULL,
                     Ref__a_i_p__name = NULL, User__a_i_p__ = NULL, Pass__a_i_p__ = NULL, Ref__a_i_p__Organisation = NULL,
                     Ref__a_i_p__Station = NULL, Ref__a_i_p__Pollutants = NULL, Ref__a_i_p__DateIN = NULL, Ref__a_i_p__DateEND = NULL,
                     csvFile = NULL, csvFile.sep = NULL, csvFile.quote = NULL, coord.ref = NULL,
                     Ref.Type = "Ref", RefData = NULL, shiny = TRUE) {
    # DownloadSensor        = Output of function DownloadSensor()
    # Down.ref              = logical, if true reference data are downloaded
    # FTPMode               = string, default = "ftp", type of download of reference data: "ftp" using a csv file on a ftp server, "csv" the same with a local file and SOS: SOS download
    # ref.tzone             = string, refernce time name of the reference data. Default = "UTC"
    # Ref.SOS.name          = SOS ID of the Reference station
    # RefSOSname            = Reference station SOS Rest API URL
    # RefSOSPollutants      = Character vector, list of pollutants to download. Default is NULL. In this case pollutants are downaloded.
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
    cat("[REF] INFO: Reading or downloading Reference data, save refData.csv with all reference values in directory General_Data\n")
    if (exists("InfluxData") && !is.null(InfluxData)) {
        minDateInflux <- min(InfluxData$date, na.rm = TRUE)
        maxDateInflux <- max(InfluxData$date, na.rm = TRUE)
        if (exists("SOSData") && !is.null(SOSData)) {
            minDateSOS    <- min(SOSData$date, na.rm = TRUE)
            maxDateSOS    <- max(SOSData$date, na.rm = TRUE)
            DownloadSensor$mindateRef <- min(c(minDateInflux,minDateSOS), na.rm = TRUE)
            DownloadSensor$maxdateRef <- max(c(maxDateInflux,maxDateSOS), na.rm = TRUE)
        } else {
            DownloadSensor$mindateRef <- minDateInflux
            DownloadSensor$maxdateRef <- maxDateInflux}
    } else {
        if (exists("SOSData") && !is.null(SOSData)) {
            DownloadSensor$mindateRef <- min(SOSData$date, na.rm = TRUE)
            DownloadSensor$maxdateRef <- max(SOSData$date, na.rm = TRUE)
        } else {
            DownloadSensor$mindateRef <- NULL
            DownloadSensor$maxdateRef <- NULL
        }
    }# The if statement should be added to DownloadSensor
    if (DownloadSensor$Retrieve.data.Ref) {
        if (Down.Ref) {
            cat("-----------------------------------------------------------------------------------\n")
            cat(paste0("[REF] INFO, Starting downloading data for ", Reference.name, sep = "\n"))
            RefDataNew  <- Down_Ref(Reference.name = Reference.name, UserMins = UserMins, DownloadSensor = DownloadSensor, urlref = urlref, ref.tzone = ref.tzone,
                                    naString = c("-999.99", "-999.98999", NaN, NA), WDoutput = file.path(DisqueFieldtestDir, "General_data"),
                                    FTPMode = FTPMode,
                                    RefSOSname = RefSOSname, Ref.SOS.name = Ref.SOS.name, RefSOSPollutants = RefSOSPollutants, RefSOSDateIN = RefSOSDateIN, RefSOSDateEND = RefSOSDateEND,
                                    Ref__a_i_p__name = Ref__a_i_p__name, User__a_i_p__ = User__a_i_p__, Pass__a_i_p__ = Pass__a_i_p__,
                                    Ref__a_i_p__Organisation = Ref__a_i_p__Organisation, Ref__a_i_p__Station = Ref__a_i_p__Station,
                                    Ref__a_i_p__Pollutants = Ref__a_i_p__Pollutants, Ref__a_i_p__DateIN = Ref__a_i_p__DateIN, Ref__a_i_p__DateEND = Ref__a_i_p__DateEND,
                                    csvFile = csvFile, csvFile.sep = csvFile.sep, csvFile.quote = csvFile.quote, coord.ref = trimws(x = coord.ref), Ref.Type = Ref.Type, shiny = shiny) # this return only new Data
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
                RefData <- load_obj(DownloadSensor$Ref.Rdata.file)
                if (!is.data.table(RefData)) RefData <- data.table(RefData)
            }
            if (!haskey(RefData)) setkey(RefData, "date") # very slow, avoid
        } else cat(paste0("[REF] INFO: there is no previously saved Ref data."), sep = "\n")
    }
    # merging RefData and RefDataNew if needed
    if (exists("RefDataNew") && !is.null(RefDataNew) && !is.na(RefDataNew)) {
        # # Checking if key is defined
        # data.table::haskey(RefDataNew)
        # Discarding dupicates, keeping last dates
        duplicated.Data <- which(duplicated(RefDataNew$date, fromLast = T))
        if (length(duplicated.Data) > 0) RefDataNew <- RefDataNew[-duplicated.Data]
        # Keep only the dates that are in Influx data
        Date.Ref.Influx <- which(RefDataNew$date %in% InfluxData$date)
        if (length(Date.Ref.Influx) > 0) {
            RefDataNew <- RefDataNew[Date.Ref.Influx]
            # Append new dates
            if (exists("RefData") && !is.null(RefData) && !is.na(RefData)) {
                # Replace existing dates for existing columns
                Date.RefDataNew.RefData <- which(RefDataNew$date %in% RefData$date)
                Date.RefData.RefDataNew <- which(RefData$date %in% RefDataNew$date)
                if (length(Date.RefDataNew.RefData) > 0) {
                    # Common Pollutants
                    Common.pollutants <- intersect(names(RefData), names(RefDataNew[Date.RefDataNew.RefData]))
                    Common.pollutants <- Common.pollutants[-which(Common.pollutants %in% c("date","Ref.Long","Ref.Lat"))]
                    if (length(Common.pollutants) > 0) {
                        data.table::set(RefData, i = Date.RefData.RefDataNew, j = Common.pollutants, 
                                        value = RefDataNew[Date.RefDataNew.RefData, Common.pollutants, with = FALSE])
                    }
                }
                # Add new columns for existing dates
                New.pollutants <-  names(RefDataNew[Date.RefDataNew.RefData])[!names(RefDataNew[Date.RefDataNew.RefData]) %in% names(RefData)]
                if (length(New.pollutants) > 0) {
                    # https://stackoverflow.com/questions/34834257/r-programming-merge-function-returns-column-names-with-x-and-y
                    RefData <- merge(x = RefData, 
                                     y = RefDataNew[Date.RefDataNew.RefData, c("date","Ref.Long","Ref.Lat", New.pollutants), with = F], 
                                     by = c("date","Ref.Long","Ref.Lat"), all = TRUE)
                }
                # Append New Dates
                New.Dates <- which(!RefDataNew$date %in% RefData$date)
                if (length(New.Dates) > 0) {
                    RefData <- rbindlist(list(RefData, RefDataNew[New.Dates]), use.names = TRUE, fill = TRUE)
                }
                # rbindlist returns a data.table
                rm(RefDataNew)
            } else {
                RefData <- RefDataNew
                if (!is.data.table(RefData)) RefData <- data.table::data.table(RefData)
                rm(RefDataNew)}
            if (!haskey(RefData)) setkey(RefData, Key = "date")
            if (extension(DownloadSensor$Ref.Rdata.file) == ".csv") {
                duplicated.Data <- which(duplicated(RefData$date, fromLast = T))
                if (length(duplicated.Data) > 0) RefData <- RefData[-duplicated.Data]
                fwrite(RefData, file = DownloadSensor$Ref.Rdata.file, na = "NA", dateTimeAs = "write.csv")
            } else if (extension(DownloadSensor$Ref.Rdata.file) == ".Rdata") save(RefData, file = DownloadSensor$Ref.Rdata.file)
            cat(paste0("[REF] INFO: reference data saved in ", DownloadSensor$Ref.Rdata.file), sep = "\n")
        }
    } else cat(paste0("[REF] WARNING, There is no new reference data for ",Reference.name,"\n"))
    # Preparing for returning data, setting var.names.ref
    if (exists("RefData") && !is.null(RefData) && !is.na(RefData)) {
        if (!identical(colnames(RefData)[-which(colnames(RefData) %in% c("date", "Ref.Long", "Ref.Lat"))],character(0))) {
            # List of Pollutants monitored at the Referencce stations
            var.names.ref <- colnames(RefData)[-which(colnames(RefData) %in% c("date", "Ref.Long", "Ref.Lat"))]
            if (length(var.names.ref) == 0) {
                cat(paste0("[REF] ERROR no reference data exisiting for ", AirSensEur.name," .Please check data at ", Reference.name))
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
GENERAL  <- function(WDoutput, UserMins, RefData, InfluxData, SOSData, Delay, var.names.Pollusens, DownloadSensor, Change.Delay = FALSE, Change.UserMins = FALSE, shiny = FALSE) {
    # input:
    #       WDoutput,
    #       UserMins,
    #       RefData,er
    #       InfluxData,
    #       SOSData,
    #       Delay,
    #       var.names.Pollusens
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
    futile.logger::flog.info("[GENERAL] Checking if there are more data in InfluxData or SOSData than in General.Rdata")
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
        futile.logger::flog.info("[GENERAL] Merging InfluxData or SOSData with RefData ")
        if (exists("RefData") && !is.null(RefData)) {
            if (exists("InfluxData") && !is.null(InfluxData)) {
                # Fine adjusting of InfluxData$date due to delays between sensor and reference data - The Delay should be the same for all sensors, this makes sense
                #  Adding field "date_PreDelay" in Influx and SOS data that will be saved into General but not in Influx and SOS.Rdata
                InfluxData$date_PreDelay <- InfluxData$date
                InfluxData$date          <- InfluxData$date + Delay * 60
                # Trying to rbind.fill InfluxData and SOSdata
                # we prefer InfluxData data over SOSData if they exist for the boardTImeStamp and gpsTimeStamp
                if (exists("SOSData") && !(is.null(SOSData))) { # RefData, InfluxData and SOSData exists
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
                        sensor    <- data.table(InfluxData[-grep(pattern = "_raw", x = colnames(InfluxData)),], key = "date")
                    } else {
                        sensor    <- data.table(InfluxData, key = "date")
                    }
                    if (any(duplicated(sensor$date)))    sensor <- sensor[-which(duplicated(sensor$date, fromLast = T))]
                    reference <- data.table(RefData, key = "date")
                    if (any(duplicated(reference$date))) reference <- reference[-which(duplicated(reference$date, fromLast = T))]
                    General   <- merge(sensor, reference, by = "date", all.x = TRUE)
                    rm(sensor, reference)
                    # Keeping only data with values in InfluxDB
                    General <- General[General$date >= min(InfluxData$date, na.rm = TRUE) & General$date <= max(InfluxData$date, na.rm = TRUE),]
                }
                futile.logger::flog.info("[GENERAL] General data table was created with sensor data and reference data ")
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
                        General <- merge(x = rbind.fill(SOSData, InfluxData)[rbind.fill(InfluxData,SOSData)$date >= as.POSIXct("2016-09-09",tz = "UTC"),],
                                         y = RefData[RefData$date >= as.POSIXct("2016-09-09",tz = "UTC"),], by = "date", all.x = TRUE )
                    }
                } else {
                    # RefData exists but no InfluxData and no SOSData
                    General <- RefData
                    futile.logger::flog.error("[GENERAL] General data table was created without sensor data and with reference data. the App is likely going to crash.")
                }
            }
        } else {
            # RefData does not exist
            if (exists("InfluxData") && !is.null(InfluxData)) { # RefData does not exist but InfluxData exists
                # Fine adjusting of InfluxData$date due to delays between sensor and reference data - The Delay should be the same for all sensors, this makes sense
                #  Adding field "date_PreDelay" in Influx and SOS data that will be saved into General but not in Influx and SOS.Rdata
                InfluxData$date_PreDelay <- InfluxData$date
                InfluxData$date          <- InfluxData$date + Delay * 60
                General <- InfluxData
                futile.logger::flog.info("[GENERAL] General datatable was created with sensor data and without reference data. It is impossible to calibrate the sensors.")
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
                    futile.logger::flog.error("[GENERAL] General cannot be created, there are no sensor and reference data. General is NA for now. The App is likely going to crash.")
                }
            }
        }
        # discarding rows with all NAs and NaNs for All gas sensors
        if (exists("General") && !is.null(General) & !all(is.na(General))) {
            futile.logger::flog.info("[GENERAL] Discarding rows with NA and NaN for all gas sensors")
            # replacing NaN with NA
            futile.logger::flog.info("[GENERAL] replacing sensors values which are not numbers (NaN) with NA for all sensors and reference data.")
            General[which(names(General) != "date")] <- purrr::map_dfc(General[which(names(General) != "date")], function(i) nan.to.na(i) )}
        # Averaging using UserMins
        General <- DF_avg(General, width = UserMins)
    } else {
        # Selecting General.Rdata if it exists
        if (file.exists(DownloadSensor$General.Rdata.file)) {
            General <- load_obj(DownloadSensor$General.Rdata.file)
            General <- DF_avg(General, width = UserMins)
        } else {
            cat("-----------------------------------------------------------------------------------\n")
            return(futile.logger::flog.error("[GENERAL] no General data allready saved and no data added."))}}
    if (exists("General") && !is.null(General)) {
        futile.logger::flog.info("[GENERAL] Returning General data.table")
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
        return(futile.logger::flog.error("[GENERAL] no data available"))
    }
}
#=====================================================================================CR
# Function View Scatter Plot of calibration function (Vs 170420)
#=====================================================================================CR
Etalonnage <- function(x, s_x, y, s_y, AxisLabelX, AxisLabelY, Title, Marker , Couleur,
                       ligne= NULL, XY_same, lim = NULL, steps = c(10,10), digitround = NULL, marges = NULL, PlotAxis = NULL, Verbose = TRUE) {
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
            if (Verbose) cat("Range of values on x and y axis\n")
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
CONFIG <- function(DisqueFieldtestDir, DisqueFieldtest , sens2ref.shield = NULL, shiny = TRUE, Dir.Config = "Configuration", 
                   Names.Influx = NULL, Names.ref = NULL, Verbose = TRUE) {
    # Return a list with the config of servers, sensors and effects
    # DisqueFieldtestDir : file.path of the data of AirSensEUR box with sub-directories General_data, configuration, Models ..., e.g. "S:/Box Sync/AirSensEUR/Fieldtests/Shiny/ASE_Boxes/4047D0"
    # DisqueFieldtest    : directory where is the file ASEconfig*.R file, which is not used anymore. It is now used because it it the directory of the Shiny App with directory Shield_files
    # sens2ref.shield    : dataframe, default is NULL, dataframe returned by function ASEPanel04Read giving the configuration of the chemical shield
    # shiny              : logical, default value is TRUE. If TRUE the function ised in a Shiny reactive context and shinyalert message can be returned.
    # Dir.Config         : character string, default value is "Configuration". Sub dirctory of DisqueFieldtestDir that includes the config fles (*.cfg)
    # Names.Influx, Names.ref: character vector, names of columns in data.tables Influx$DATA and Ref$DATA, default is null
    if (Verbose) cat("-----------------------------------------------------------------------------------\n")
    ASE_name           <- basename(DisqueFieldtestDir)
    #=====================================================================================CR
    #  "sensors.cfg" and board.cfg----
    #=====================================================================================CR
    sensor_cfg <- file.path(DisqueFieldtestDir, Dir.Config, paste0("Sensors.cfg"))
    if (file.exists(sensor_cfg)) {
        # reading the Server configuration files
        Sensors.cfg <- data.table::fread(file = sensor_cfg, header = TRUE, na.strings=c("","NA", "<NA>"))
        if (Verbose) futile.logger::flog.info(paste0("[CONFIG] the sensor file ", sensor_cfg, " exists."))
        # Converting to correct format
        set(Sensors.cfg, j = "time"   , value = lubridate::ymd_hms(Sensors.cfg$time, tz = "UTC"))
        set(Sensors.cfg, j = "enabled", value = as.logical(Sensors.cfg$enabled))}
    board_cfg <- file.path(DisqueFieldtestDir, Dir.Config, paste0("Boards.cfg"))
    if (file.exists(board_cfg)) {
        # reading the Server configuration files
        board.cfg <- data.table::fread(file = board_cfg, header = TRUE, na.strings=c("","NA", "<NA>"))
        if (Verbose) futile.logger::flog.info(paste0("[CONFIG] the boaed file ", board_cfg, " exists."))
        # Converting to correct format
        set(board.cfg, j = "time"   , value = lubridate::ymd_hms(board.cfg$time, tz = "UTC"))}
    #=====================================================================================CR
    #  ASE_name,"_Servers.cfg" ----
    #=====================================================================================CR
    # Read config file (TRUE)
    File_Server_cfg <- file.path(DisqueFieldtestDir, Dir.Config, paste0(ASE_name,"_Servers.cfg"))
    if (file.exists(File_Server_cfg)) {
        # reading the Server configuration files
        cfg <- data.table::transpose(fread(file = File_Server_cfg, header = FALSE, na.strings=c("","NA", "<NA>")), fill = NA, make.names = 1)
        if (Verbose) cat(paste0("[CONFIG] Info, the config file ", File_Server_cfg, " for the configuration of servers exists"), sep = "\n")
        # Changing names
        if ("AirsensEur.name" %in% names(cfg)) names(cfg)[which(names(cfg) == "AirsensEur.name")] <- "AirSensEur.name"
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
    }
    #=====================================================================================CR
    #  ASE_name,".cfg"
    #=====================================================================================CR
    # This is to insert both sensors and reference configuration into a dataframe and file
    File_cfg <- list.files(path = file.path(DisqueFieldtestDir,Dir.Config), pattern = paste0(ASE_name,".cfg"))
    if (!identical(File_cfg,character(0))) {
        # reading the configuration files sens2ref
        File_cfg <- file.path(DisqueFieldtestDir,Dir.Config, paste0(ASE_name,".cfg"))
        if (file.exists(File_cfg)) {
            if (Verbose) cat(paste0("[CONFIG] Info, the config file ", File_cfg, " for the configuration of AirSensEUR exists"), sep = "\n")
            sens2ref <- data.table::transpose(fread(file = File_cfg, header = FALSE, na.strings = c("","NA", "<NA>"), fill = TRUE), fill = NA, make.names = 1)
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
                    sens2ref[, uxi := NULL]}
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
        } else { 
            # if no ASE_name,".cfg", Message of error
            my_message <- paste0("[CONFIG] ERROR, no config file for the AirSensEUR box. \n",
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
        # updating names of sensors with the sensor schield config file
        if (is.null(sens2ref.shield)) {
            # Reading chemical shield config file and merging with sens2ref if the file exists
            Shield.file <- file.path(DisqueFieldtest,"Shield_Files",cfg$asc.File)
            if (file.exists(Shield.file)) {
                sens2ref.shield <- ASEPanel04Read(ASEPanel04File = Shield.file)
            } else {
                # if ASE_name,".cfg", Message of error
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
            if (!is.data.table(sens2ref.shield)) sens2ref.shield <- data.table::data.table(sens2ref.shield, key = "name.gas")
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
                fwrite(setDT(as.data.frame(t(sens2ref)), keep.rownames = "name.gas")[], file = file.path(DisqueFieldtestDir,Dir.Config,paste0(ASE_name,".cfg")), row.names = FALSE,col.names = FALSE)
            }
        }
    } else { # if File_cfg does not exist, , Message of error
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
            animation = FALSE)
        return(my_message)
    }
    # re-ordering columns
    setcolorder(sens2ref, Columns <- c("name.gas", "name.sensor", "gas.sensor", "Sens.raw.unit", "Sens.unit", "gas.reference", "gas.reference2use", "ref.unitgas", 
                                       "Cal.Line", "Cal.func", "mod.eta.model.type", "Neg.mod", "Slope", "Intercept", "ubsRM", "ubss", "Sync.Cal", "Sync.Pred", "eta.model.type", 
                                       "hoursWarming", "temp.thres.min", "temp.thres.max", "rh.thres.min", "rh.thres.max", "Sens.Inval.Out", "Sens.rm.Out", 
                                       "Sens.window", "Sens.threshold", "Sens.Ymin", "Sens.Ymax", "Sens.ThresholdMin", "Sens.iterations", "remove.neg", 
                                       "Ref.rm.Out", "Ref.window", "Ref.threshold", "Ref.Ymin", "Ref.Ymax", "Ref.ThresholdMin", "Ref.iterations",
                                       "Ref", "RefAD", "RefAFE", "Rload", "TIA_Gain", "GAIN", "Ref_Source", "Int_Z", "Bias_Sign", "Bias", "Fet_Short", "Mode", "board.zero.set", "BIAIS"))
    # Adding sensors if not included in sens2ref
    Columns.Text <- c("name.gas", "name.sensor", "gas.sensor"    , "Sens.raw.unit", "Sens.unit", 
                      "gas.reference", "gas.reference2use", "ref.unitgas", 
                      "Cal.Line", "Cal.func", "mod.eta.model.type", "eta.model.type",
                      "Ref_Source", "Fet_Short")
    Columns.Num  <- c( "Slope", "Intercept", "ubsRM", "ubss", 
                       "hoursWarming", "temp.thres.min", "temp.thres.max", "rh.thres.min", "rh.thres.max",
                       "Sens.window", "Sens.threshold", "Sens.Ymin", "Sens.Ymax", "Sens.ThresholdMin", "Sens.iterations",
                       "Ref.window", "Ref.threshold", "Ref.Ymin", "Ref.Ymax", "Ref.ThresholdMin", "Ref.iterations",
                       "Ref", "RefAD", "RefAFE", "Rload", "TIA_Gain", "GAIN", "Int_Z", "Bias_Sign", "Bias", "Mode", "board.zero.set", "BIAIS")
    Columns.Bool  <- c( "Neg.mod", "Sync.Cal", "Sync.Pred", "Sens.Inval.Out", "Sens.rm.Out", "remove.neg", "Ref.rm.Out")
    if (!is.null(Names.Influx) && !is.null(Names.ref) && 
        !"CO2" %in% sens2ref$name.gas && "Carbon_dioxide" %in% Names.Influx && "Ref.CO2" %in% Names.ref) {
        # adding the first row at the bottom of the table
        sens2ref <- rbind(sens2ref, sens2ref[1])
        # Correcting the last row
        Values.Text <- c("CO2", "D300", "Carbon_dioxide", "ppm", "ppm",    "CO2", "Ref.CO2", "ppm",    "Prediction with previous calibration","", "Linear.Robust", "Linear",    NA , NA)
        Values.num  <- c(1, 0, 3, 15    ,4, -20, 40, 0, 100,    181, 20, 200, 2000, 200, 1,    181, 75, 200, 2000, 200, 1,     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
        Values.Bool <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Text, value = lapply(Values.Text, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Num,  value = lapply(Values.num, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Bool, value = lapply(Values.Bool, function(i) i))
        # Saving new version
        fwrite(setDT(as.data.frame(t(sens2ref)), keep.rownames = "name.gas")[], file = file.path(DisqueFieldtestDir,Dir.Config,paste0(ASE_name,".cfg")), row.names = FALSE,col.names = FALSE)
    }
    # Adding PM10-OPC-N3 if not included
    if (!is.null(Names.Influx) && !is.null(Names.ref) && 
        !"OPCN3PM10" %in% sens2ref$name.sensor && "Particulate_Matter_10" %in% Names.Influx && "Ref.PM10" %in% Names.ref) {
        # adding the first row at the bottom of the table
        sens2ref <- rbind(sens2ref, sens2ref[1])
        # Correcting the last row
        Values.Text <- c("PM10", "OPCN3PM10", "Particulate_Matter_10", "ug/m3", "ug/m3",    "PM10", "Ref.PM10", "ug/m3",    "Prediction with previous calibration","", "Linear.Robust", "Linear",    NA , NA)
        Values.num  <- c(1, 0, 2.5, 5    ,4, -20, 40, 0, 75,    181, 20, 0, 2000, 0, 1,    181, 75, 0, 500, 0, 1,     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
        Values.Bool <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Text, value = lapply(Values.Text, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Num,  value = lapply(Values.num, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Bool, value = lapply(Values.Bool, function(i) i))
        # Saving new version
        fwrite(setDT(as.data.frame(t(sens2ref)), keep.rownames = "name.gas")[], file = file.path(DisqueFieldtestDir,Dir.Config,paste0(ASE_name,".cfg")), row.names = FALSE,col.names = FALSE)
    }
    # Adding PM10-PMS5003 if not included
    if (!is.null(Names.Influx) && !is.null(Names.ref) &&
        !"5310CAT" %in% sens2ref$name.sensor && "PM10_PMSCal" %in% Names.Influx && "Ref.PM10" %in% Names.ref) {
        # adding the first row at the bottom of the table
        sens2ref <- rbind(sens2ref, sens2ref[1])
        # Correcting the last row
        Values.Text <- c("PM10", "5310CAT", "PM10_PMSCal", "ug/m3", "ug/m3",    "PM10", "Ref.PM10", "ug/m3",    "Prediction with previous calibration","", "Linear.Robust", "Linear",    NA , NA)
        Values.num  <- c(1, 0, 2.5, 5    ,4, -20, 40, 0, 80,    181, 20, 0, 500, 0, 1,    181, 75, 0, 500, 0, 1,     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
        Values.Bool <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Text, value = lapply(Values.Text, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Num,  value = lapply(Values.num, function(i) i))
        data.table::set(sens2ref, i = nrow(sens2ref), j = Columns.Bool, value = lapply(Values.Bool, function(i) i))
        # Saving new version
        fwrite(setDT(as.data.frame(t(sens2ref)), keep.rownames = "name.gas")[], file = file.path(DisqueFieldtestDir,Dir.Config,paste0(ASE_name,".cfg")), row.names = FALSE,col.names = FALSE)
    }
    # reading the files with Covariates to plot and covariates to calibrate
    for (i in 1:length(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)])) {
        
        nameSens <- sens2ref$name.sensor[which(!is.na(sens2ref$name.sensor))][i]
        nameGas  <- sens2ref$name.gas[which(sens2ref$name.sensor == nameSens)]
        nameFile <- file.path(DisqueFieldtestDir,Dir.Config,paste0(ASE_name,"_Covariates_",nameSens,".cfg"))
        # Covariates to plot
        if (file.exists(nameFile)) {
            if (Verbose) cat(paste0("[CONFIG] INFO, the file with covariates to plot ", nameFile, " exists "), sep = "\n")
            assign(nameSens,
                   read.csv(file = nameFile,
                            header = TRUE,
                            comment.char = "",
                            stringsAsFactors = FALSE))
        } else {
            if (Verbose) cat(paste0("[CONFIG] ERROR, the file with covariates to plot ", nameFile, " does not exist. File is iniatized with the R script info."), sep = "\n")
            # DEFINE The lists of variables to plot in retrieving data () to understand the inerferences - use report of lab. tests
            if (nameGas == "CO") {
                assign(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],
                       data.frame(Effects = c(paste0(nameSens,"_volt"), "Out.Ref.CO_ppm" , "Relative_humidity", "Temperature"))) #####################################################
            } else if (nameGas == "O3" || nameGas == "NO2") {
                assign(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],
                       data.frame(Effects = c(paste0(nameSens,"_volt"), "Out.Ref.NO2", "Out.Ref.O3", "Relative_humidity", "Temperature")))
            } else if (nameGas == "NO") {
                assign(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],
                       data.frame(Effects = c(paste0(nameSens,"_volt"), "Out.Ref.NO", "Relative_humidity", "Temperature")))
            } else if (nameGas == "CO2") {
                assign(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],
                       data.frame(Effects = c(paste0(nameSens,"_volt"), "Out.Ref.CO2", "Relative_humidity", "Temperature")))
            } else if (nameGas == "PM10") {
                assign(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],
                       data.frame(Effects = c(paste0(nameSens,"_volt"), "Out.Ref.PM10", "Relative_humidity", "Temperature")))
            } else if (nameGas == "PM2.5") {
                assign(sens2ref$name.sensor[!is.na(sens2ref$name.sensor)][i],
                       data.frame(Effects = c(paste0(nameSens,"_volt"), "Out.Ref.PM2.5", "Relative_humidity", "Temperature")))
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
        nameFile <- file.path(DisqueFieldtestDir,Dir.Config,paste0(ASE_name,"_CovMod_",nameSens,".cfg"))
        if (file.exists(nameFile) && nrow(read_csv(nameFile, col_types = cols(Effects = col_character()))) > 0) {
            if (Verbose) cat(paste0("[CONFIG] INFO, the file with covariates to calibrate ", nameFile, " exists "), sep = "\n")
            assign(paste0(nameSens,"CovMod"),
                   read.csv(file = nameFile,
                            header = TRUE,
                            comment.char = "#",
                            stringsAsFactors = FALSE)
            )
        } else {
            if (Verbose) cat(paste0("[CONFIG] ERROR, the file with covariates to calibrate ", nameFile, " does not exist. File is inialized with Temperature."), sep = "\n")
            # DEFINE The lists of variables to plot in retrieving data () to understand the inerferences - use report of lab. tests
            assign(paste0(nameSens,"CovMod"),
                   data.frame(Effects = "Temperature"))
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
        New_General_dir <- file.path(DisqueFieldtestDir, "Configuration")
        if (!dir.exists(New_General_dir)) dir.create(New_General_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
        cfg_Files       <- list.files(path = file.path(DisqueFieldtestDir, "General_data"), pattern = ".cfg", recursive = TRUE, full.names = TRUE)
        for (i in cfg_Files) {
            if (Verbose) cat(paste0("[shiny, Create.New] INFO, copying ", basename(i), " at ", New_General_dir), sep = "\n")
            file.copy(from = i, to = file.path(New_General_dir, basename(i)), overwrite = TRUE, copy.mode = TRUE, copy.date = FALSE)
            file.remove(i)}
        
    }
    if (Verbose) cat("-----------------------------------------------------------------------------------\n")
    return.CONFIG <- list(cfg,sens2ref,Covariates,CovMod, sens2ref.shield)
    names(return.CONFIG) <- c("Server","sens2ref","CovPlot","CovMod", "sens2ref.shield")
    if (exists("Sensors.cfg")) return.CONFIG$Sensors.cfg <- Sensors.cfg
    if (exists("board.cfg"))   return.CONFIG$board.cfg   <- board.cfg
    return(return.CONFIG)
}
#=====================================================================================CR
# Valid Periods
#=====================================================================================CR
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
    ASE_name           <- basename(DisqueFieldtestDir)
    # Setting the General time zone to the one of DownloadSensor$DateIN.General.prev
    # or DateIN.Influx.prev or DateIN.SOS.prev otherwise it is set to "UTC"
    if (exists("DownloadSensor")) {
        if (!is.null(DownloadSensor$DateIN.General.prev) && !is.na(DownloadSensor$DateIN.General.prev)) {
            General.TZ <- lubridate::tz(DownloadSensor$DateIN.General.prev)
        } else {
            if (!is.null(DownloadSensor$DateIN.Influx.prev) && !is.na(DownloadSensor$DateIN.Influx.prev)) {
                General.TZ <- lubridate::tz(DownloadSensor$DateIN.Influx.prev)
            } else {
                if (!is.null(DownloadSensor$DateIN.SOS.prev) && !is.na(DownloadSensor$DateIN.SOS.prev)) {
                    General.TZ <- lubridate::tz(DownloadSensor$DateIN.SOS.prev)
                } else General.TZ <- "UTC"
            }
        }
    } else  General.TZ <- "UTC"
    # Read SetTime file
    Save.sens2ref <- FALSE
    File_SETTIME_cfg   <- list.files(path = file.path(DisqueFieldtestDir, Dir.Config), pattern = paste0(ASE_name,"_SETTIME.cfg")  )
    if (!identical(File_SETTIME_cfg,character(0))) {
        # reading the configuration files sens2ref
        File_SETTIME_cfg <- file.path(DisqueFieldtestDir, Dir.Config, paste0(ASE_name,"_SETTIME",".cfg"))
        if (file.exists(File_SETTIME_cfg)) {
            cat(paste0("[SETTIME] Info, the config file ", File_SETTIME_cfg, " for the configuration of AirSensEUR exists"), sep = "\n")
            sens2ref <- data.table::fread(file = File_SETTIME_cfg, header = TRUE, na.strings = c("","NA", "<NA>"))
            if ("CO2" %in% Config$sens2ref$name.gas && !"CO2" %in% names(sens2ref)) {
                # adding the last column the table as CO2
                sens2ref <- cbind(sens2ref, sens2ref[,length(names(sens2ref)), with = FALSE])
                setnames(sens2ref, length(names(sens2ref)), "CO2")
                sens2ref[which(sens2ref$name.gas == "name.sensor"), CO2 := "D300"]
                Save.sens2ref <- TRUE
            }
            if ("OPCN3PM10" %in% Config$sens2ref$name.sensor && !"OPCN3PM10" %in% sens2ref[which(name.gas == "name.sensor")]) {
                # adding the last column the table as PM10
                sens2ref <- cbind(sens2ref, sens2ref[,length(names(sens2ref)), with = FALSE])
                setnames(sens2ref, length(names(sens2ref)), "PM10")
                sens2ref[which(sens2ref$name.gas == "name.sensor"), PM10 := "OPCN3PM10"]
                # Correcting the last Column values
                Save.sens2ref <- TRUE
            }
            if ("5310CAT" %in% Config$sens2ref$name.sensor && !"5310CAT" %in% sens2ref[which(name.gas == "name.sensor")]) {
                # adding the last column the table as PM10
                sens2ref <- cbind(sens2ref, sens2ref[,length(names(sens2ref)), with = FALSE])
                setnames(sens2ref, length(names(sens2ref)), "PM10")
                sens2ref[which(sens2ref$name.gas == "name.sensor"), PM10 := "5310CAT"]
                # Correcting the last Column values
                Save.sens2ref <- TRUE
            }
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
            File_Server_cfg    <- file.path(DisqueFieldtestDir, Dir.Config, paste0(ASE_name,"_Servers.cfg"))
            if (file.exists(File_Server_cfg)) {
                cfg <- data.table::transpose(fread(file = File_Server_cfg, header = FALSE, na.strings=c("","NA", "<NA>")), fill = NA, make.names = 1)
                # Changes names for change Shiny App version 0.9 to 0.10
                Change.names <- rbind(c("TZ"           , "Influx.TZ"),     # Time zone of Influx data
                                      c("sens.tzone"   , "SOS.TZ"))         # Time zone of SOS data
                for (k in 1:nrow(Change.names)) if (Change.names[k,1] %in% colnames(cfg)) colnames(cfg)[colnames(cfg) == Change.names[k,1]] <- Change.names[k,2]
            } else cat(paste0("[SETTIME] The file of server configuration for AirSensEUR: ", File_Server_cfg, " does not exist.\n"))
        } else cfg <- Config[["Server"]]
        # Second read the shield config file to get the sensor names
        if (file.exists(file.path(DisqueFieldtest,"Shield_Files",cfg$asc.File))) {
            sens2ref.shield <- ASEPanel04Read(ASEPanel04File = file.path(DisqueFieldtest,"Shield_Files",cfg$asc.File))
        }  else cat("[SETTIME] ERROR shield file (asc.File) not found\n")}
    # update the name of sensors in the SETTIME.cfg
    if (!all(sens2ref.shield$name.sensor %in% na.omit(unlist(sens2ref[which(name.gas == "name.sensor"), .SD])[-1]))) {
        name.sensor2Change <- which(!sens2ref.shield$name.sensor %in% na.omit(unlist(sens2ref[which(name.gas == "name.sensor"), .SD])[-1]))
        for (i in name.sensor2Change) {
            set(sens2ref, i = which(sens2ref$name.gas  == "name.sensor"),
                j = which(names(sens2ref) %in% sens2ref.shield$name.gas[i]), value = sens2ref.shield$name.sensor[i])
        }
        # set to save file
        Save.sens2ref <- TRUE}
    # transpose the data.table
    sens2ref <- cbind(names(sens2ref)[-1],data.table::transpose(sens2ref, fill = NA, make.names =  "name.gas"))
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
        Save.sens2ref <- TRUE}
    # adding "Cov.Date.IN" "Cov.Date.END"if missing
    if (!"Cov.Date.IN"  %in% names(sens2ref)) {
        sens2ref <-  cbind(sens2ref, sens2ref[,Valid.IN])
        setnames(sens2ref, length(names(sens2ref)), "Cov.Date.IN")
        # set to save file
        Save.sens2ref <- TRUE}
    if (!"Cov.Date.END" %in% names(sens2ref)) {
        sens2ref <-  cbind(sens2ref, sens2ref[,Valid.END])
        setnames(sens2ref, length(names(sens2ref)), "Cov.Date.END")
        # set to save file
        Save.sens2ref <- TRUE}
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
    for (k in Vector.type) {
        #set(sens2ref, j = k, value = ymd_hms(sens2ref[[k]], tz = General.TZ))
        set(sens2ref, j = k, value = as.POSIXct(sens2ref[[k]], tz = General.TZ,
                                                tryFormats = c("%y-%m-%d %H:%M:%S",
                                                               "%y-%m-%d %H:%M",
                                                               "%y-%m-%d",
                                                               "%Y-%m-%d %H:%M:%S",
                                                               "%Y-%m-%d %H:%M",
                                                               "%Y-%m-%d"),
                                                optional = TRUE))}
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
        Check_Dates.IN <- c("Out.Ref.IN", "Out.Sens.IN", "Cov.Date.IN", "DateCal.IN", "DatePlotCal.IN", "DateMeas.IN", "DatePlotMeas.IN")
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
        Check_Dates.END <- c("Out.Ref.END", "Out.Sens.END", "Cov.Date.END", "DateCal.END", "DatePlotCal.END", "DateMeas.END", "DatePlotMeas.END")
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
panel.cor <- function(x, y, digits=3, prefix = "", cex.cor = 2) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r1  <- cor(x, y, use = "pairwise.complete.obs")
    r   <- abs(cor(x, y,use = "pairwise.complete.obs"))
    txt <- format(c(r1^2, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (missing(cex.cor)) cex <- 0.8/strwidth(txt) else cex = cex.cor
    text(0.5, 0.75, txt, cex = cex * 0.5 * r + cex * 0.5)
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
    # Model.used                : character, default is null. name of calibration model used
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
    if (!is.data.frame(Mat) || !is.data.table(Mat)) return(cat("Mat is not a data.table or dataFrame. Returning NAs.\n")) else {
        # convert to data.table if needed
        if (!is.data.table(Mat)) Mat <- data.table(Mat, key = "date")
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
    #          DateIN     : character strings, begining and ending dates of selected data, e.g DateIN  <- "2018-10-09" DateEND <- "2019-01-10"
    #          x, y       : character strings, names of columns x and y in General.df dataFrame, they will be usd for x and y axis labels
    #          Title      : optional chacter string of the scatterplot
    # Output : The linear comparison model
    # Subset General.df to selected date
    General    <- subset(General.df[, c("date", x, y)], date >= as.POSIXct(DateIN) & date <= as.POSIXct(DateEND))
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
                        allparams = "true", avgtime=600, shiny = TRUE) {
    # Mail "Erich Kitzmüller" <erich.kitzmueller@a-i-p.com>
    # please append the parameter "&allparams=true" to your URL. Without that, the request only returns the values of monitoring-site-parameters marked as default (see image).
    # Hint: For each unique combination of "Monitoring Site" and "Global Parameter", there can be only one monitoring-site-parameter marked as default.
    # Checking internet
    if (curl::has_internet()) {
        cat(paste0("[a_i_p_param] INFO: There is an internet connection.\n"))
    } else {
        my_message <- paste0("[a_i_p_param()] ERROR There is no internet connection\n",
                             "It is impossible to download data.\n")
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
        cat(my_message)
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
        return()
    } else {
        my_message <- paste0("[a_i_p_param] ERROR the parameter to contact the a_i_p server are wrong, please check\n")
        cat(my_message)
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
            animation = FALSE)
        return()
    }
}

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
    if (!all(list.Packages %in% installed.packages())) Load_Packages(list.Packages)
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
        if ("!" %in% param) param <- strsplit(param, "!")[[1]] 
        if ("Value" %in% names(Param.i) && Refi$Components[[1]]$Component %in% param) {
            cat(paste0("Component ",stringr::str_pad(which(Components %in% Refi$Components[[1]]$Component), 2, side = "left", pad = " "),"/",length(seq_along(Ref)),": ",
                       stringr::str_pad(Refi$Components[[1]]$Component, max(nchar(Components)), side = "right", pad = " ")," was correctly downloaded\n"))
            colnames(Param.i) <- c("date", Refi$Components[[1]]$Component)
            return(Param.i)
        } else {
            cat(paste0("Component ",stringr::str_pad(which(Components %in% Refi$Components[[1]]$Component), 2, side = "left", pad = " "),"/",length(seq_along(Ref)), ": ",
                       str_pad(Refi$Components[[1]]$Component, max(nchar(Components)), side ="right", pad=" "), " no data to be downloaded or parameter not requested\n"))
        }
    })
    # Creating ReferenceData
    RefData <- rbindlist(MeasuredValues[!sapply(MeasuredValues, is.null)], use.names = TRUE, fill = TRUE)
    data.table::set(RefData, j = "date", value =  ymd_hms(RefData[["date"]], tz = Time_zone))
    RefData <- DF_avg(RefData, width = avgtime)
    # returning
    return(RefData)
}
#================================================================CR
### functions to query find PM density from DMPS and APS
#================================================================CR
Distribution_Ref_TS <- function(RefData, DateBegin = NULL, DateEnd = NULL, Sensor_dates = NULL, Min_APS = NULL, Max_APS = NULL,
                                diameters_bins = NULL, units = NULL) {
    # RefData:      a data.table or dataframe that includes all the reference data (dates, coordinates, gas, PM mass concentration, bins 
    #               ("Bin.APS."+diameter in µm and "Bin.DMPS."+diameter in µm)...) 
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
    if (!is.data.table(RefData)) RefData <- data.table::data.table(RefData)
    if ("date" %in% names(RefData) && !all(is.null(c(DateBegin, DateEnd)))) {
        if (!(is.null(DateBegin))) RefData <- RefData[Startdate >= DateBegin]
        if (!(is.null(DateEnd)))   RefData <- RefData[date <= DateEnd]
    } else stop("RefData is missing a column \"date\"")
    if (!is.null(Sensor_dates))  RefData <- RefData[date %in% Sensor_dates]
    if (any(is.na(RefData[, .SD, .SDcols = -which(names(RefData) %in% c("date","Startdate"))]))) {
        cat(paste0("RefData has missing data at the selected date between ", DateBegin, " and ", DateEnd,"\n"))
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
                cat("Reference data is empty!\n")
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
        
        plot <-  ggplot() + 
            theme_bw() +
            geom_point(data = counts_Ref, aes((diameters), (counts), colour=Instrument), stat = "identity", fill = "gray") +
            #   geom_line(data = augmented, aes(exp(x), exp(.fitted),  col = "Modelled"), size = 2) +
            theme(axis.title.x = element_text(colour  = "black", size = 15),
                  axis.text.x  = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15, colour = "black")) +
            theme(axis.title.y = element_text(colour = "black", size = 15),
                  axis.text.y  = element_text(angle = 0, vjust = 0.5, size = 15, colour = "black")) +
            xlab(expression(paste("diameter (µm)"))) + 
            ylab(expression(paste("counts per ml"))) 
        
    } else {
        # if there is model filtted in log, all the plot shall be in log
        x <- names(Model.i$Augment)[2]
        y <- names(Model.i$Augment)[1]
        plot <-  ggplot() + 
            theme_bw() +
            geom_point(data = counts_Ref, aes(x = !!ensym(x), y = !!ensym(y), col = "Ref"), stat = "identity", fill = "gray") +
            geom_line(data = Model.i$Augment, aes(x = !!ensym(x), (.fitted),  col = "Modelled"), size = 1) +
            scale_color_manual(values = c("Modelled" = "blue", "Ref" = "black")) +
            theme(axis.title.x = element_text(colour = "black", size = 15),
                  axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size = 15, colour = "black")) +
            theme(axis.title.y = element_text(colour = "black", size = 15),
                  axis.text.y  = element_text(angle=0, vjust=0.5, size = 15, colour = "black"))
        #xlab(expression(paste("log of diameter (µm)"))) + 
        #ylab(expression(paste("log of tot # of counts"))) 
    }
    
    return(plot)
} 
Plot_Dist_Ref_log <- function(counts_Ref, Model.i = NULL, Count) {
    
    # Plotting distribution in log and modelled distribution if Model.i is not NULL
    
    # changes names of count_ref in case it uses x and y as for fitting model for density determination
    if (any(c("x", "y") %in% names(counts_Ref))) names(counts_Ref) <- c("diameters", "counts")
    
    if  (is.null(Model.i)) {
        
        plot <-  ggplot() + 
            theme_bw() +
            geom_point(data = counts_Ref, aes(log10(diameters), colour = Instrument, log10(counts)), stat = "identity", fill = "gray") +
            #   geom_line(data = augmented, aes(exp(x), exp(.fitted),  col = "Modelled"), size = 2) +
            theme(axis.title.x = element_text(colour  = "black", size = 15),
                  axis.text.x  = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15, colour = "black")) +
            theme(axis.title.y = element_text(colour = "black", size = 15),
                  axis.text.y  = element_text(angle = 0, vjust = 0.5, size = 15, colour = "black")) +
            xlab(expression(paste("log10 of diameter (µm)"))) + 
            ylab(expression(paste("log10 of counts / log of diameters"))) 
        
    } else {
        
        plot <-  ggplot() + 
            theme_bw() +
            geom_point(data = counts_Ref, aes(log10(diameters), colour = Instrument, log10(counts), col = "Ref"), stat = "identity", fill = "gray") +
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
    #               ("Bin.APS."+diameter in µm and "Bin.DMPS."+diameter in µm)...) 
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
                            Glance = glance(Model), 
                            Call = Model$call, Coef = coef(Model),
                            Model = Model)
            
            # fitted DMPS reference data
            Plot.DMPS       <- Plot_Dist_Ref(DataXY.DMPS, Model.i = Model.i)
            # add raw APS reference data
            if (Log10) {
                # add raw APS reference data
                Plot.DMPS.APS   <- Plot.DMPS + geom_point(data = DataXY.APS, aes(log.diam, log.counts), stat = "identity", col = "red")
                # raw data + minimized data (APS)
                Plot.DMPS.APS.d <- Plot.DMPS.APS + geom_point(data = DataXY.APS, aes(diam.eq, log.counts), stat = "identity", shape = 17, col = "red", size = 3)
            } else {
                # add raw APS reference data
                x <- names(DataXY.APS)[2]
                y <- names(DataXY.APS)[1]
                Plot.DMPS.APS   <- Plot.DMPS + geom_point(data = DataXY.APS, aes(x = !!ensym(x), y = !!ensym(y)), stat = "identity", col = "red")
                # raw data + minimized data (APS)
                Plot.DMPS.APS.d <- Plot.DMPS.APS + geom_point(data = DataXY.APS, 
                                                              aes(diam.eq, y = !!ensym(y)), stat = "identity", shape = 17, col = "red", size = 3)
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
### functions to update config of sensors from rhansomeTable
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
                              by = c("name.gas", "name.sensor"), all = TRUE, fill = T)} 
    if (!is.null(DT_Calib.cfg)) {
        Table_Config <- merge(hot_to_r(DT_Calib.cfg), 
                              Table_Config[, c("name.gas", "name.sensor", setdiff(names(Table_Config), names(hot_to_r(DT_Calib.cfg)))), with = FALSE],
                              by = c("name.gas", "name.sensor"), all = TRUE, fill = T)} 
    if (!is.null(DT_CalTime)) {
        Table_Config <- merge(hot_to_r(DT_CalTime), 
                              Table_Config[, c("name.gas", "name.sensor", setdiff(names(Table_Config), names(hot_to_r(DT_CalTime)))), with = FALSE],
                              by = c("name.gas", "name.sensor"), all = TRUE, fill = T)} 
    setcolorder(Table_Config, Columns <- c("name.gas", "name.sensor", "gas.sensor", "Sens.raw.unit", "Sens.unit", "gas.reference", "gas.reference2use", "ref.unitgas", 
                                           "Cal.Line", "Cal.func", "mod.eta.model.type", "Neg.mod", "Slope", "Intercept", "ubsRM", "ubss", "Sync.Cal", "Sync.Pred", "eta.model.type", 
                                           "hoursWarming", "temp.thres.min", "temp.thres.max", "rh.thres.min", "rh.thres.max", "Sens.Inval.Out", "Sens.rm.Out", 
                                           "Sens.window", "Sens.threshold", "Sens.Ymin", "Sens.Ymax", "Sens.ThresholdMin", "Sens.iterations", "remove.neg", 
                                           "Ref.rm.Out", "Ref.window", "Ref.threshold", "Ref.Ymin", "Ref.Ymax", "Ref.ThresholdMin", "Ref.iterations"))
    return(Table_Config)
}

#================================================================CR
# Functions calibration  ####
#================================================================CR
#' list of Calibration models for one or all sensors of one ASE box ---
#'
#' @param ASEDir A character vector with the file path of the ASE Box for which to look for models. It should be something like file.path("shiny", "ASE_Boxes", ASE.name).
#' @param name.sensor Character vector, character vector with the name(s) of sensors to look for models. Default is "ALL" that returns the models for all sensors of the ASE box.
#' @param DIR_Config File path of the subdirectory of ASEDir where is the file ASE.name.cfg.
#' @param DIR_Models File path of the subdirectory of ASEDir where are the calibration models.
#' @param Median logical, default FALSE. IF False all model names including "Median" are discarded.
#' @return a vector of model files in ASEdir without filepath.
#' @examples
#' List_models(ASEDir = ASEDir)
List_models <- function(ASEDir, name.sensor = "ALL", DIR_Config = "Configuration", DIR_Models = "Models", Median = FALSE, Verbose = TRUE, List.models = NULL) {
    if (!is.null(List.models)) rm(List.models)
    if (name.sensor == "ALL") {
        # name of ASE box
        ASE.name    <- basename(ASEDir)
        ASE.cfg     <- fread(file.path(ASEDir, DIR_Config, paste0(ASE.name,".cfg")), showProgress = F)
        name.sensor <- unlist(ASE.cfg[name.gas == "name.sensor"])[-1]
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

#' list of charateristics for one sensor of one ASE box determined using a caibration model name
#'
#' @param Model full filepath and name of model of the calibration model.
#' @param name.sensor Character vector, default is NULL. if NULL the first sensor in ASE.cfg will be used.
#' @param General.DT A data.table with all ASE box data. Default is null. If NULL the General.csv file is loaded
#' @param ASE.cfg A data.table with all ASE box configuration. Default is null. If NULL the ASE.cfg file is loaded
#' @param SetTime A data.table with all ASE box SetTime configuration. Default is null. If NULL the ASE_SETTIME.cfg file is loaded
#' @param DIR_Config File path of the subdirectory of ASEDir where is the file ASE.name.cfg.
#' @param DIR_Models File path of the subdirectory of ASEDir where are the calibration models.
#' @param DIR_General File path of the subdirectory of ASEDir where is the file General.csv.
#' @return a list of charateristics of a sensor in ASE box with data.table General.DT, config data.table ASE.cfg, k the index of sensor in the kist of sensors,
# the dates of calibration Cal.DateIN  and Cal.DateEND = Cal.DateEND, full date interval of measurements Meas.DateIN and Meas.DateEND, column names of 
# sensor raw data (nameGasVolt), sensor predicted data (nameGasMod), corresponding reference data (nameGasRef), sensor raw unit (Sens.raw.unit)
# vector of list of sensor (list.sensors), vector of list of meteorological parameters (var.names.meteoDates).
#' @examples
#' Identify.ASE(Model = Model)
Identify_ASE <- function(Model, name.sensor = NULL, General.DT = NULL, ASE.cfg = NULL, SetTime = NULL, Config = NULL, Shield = NULL,
                         DIR_Config = "Configuration", DIR_Models = "Models", DIR_General = "General_data") {
    # name of ASE box
    Stripped.Model <- unlist(strsplit(Model, split = "__"))
    # Extract ASE.name
    ASE.name <- basename(Stripped.Model[1])
    # Extract ASEDir
    ASEDir   <- substr(Stripped.Model[1], start = 1, stop = stringr::str_locate(pattern = ASE.name,Stripped.Model[1])[1,"end"])
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
    if (is.null(Config)) Config <- CONFIG(DisqueFieldtestDir = ASEDir, DisqueFieldtest = dirname(dirname(ASEDir)), Dir.Config = "Configuration", Verbose = F, shiny = F)
    if (is.null(Shield)) Shield <- ASEPanel04Read(ASEPanel04File = file.path(dirname(dirname(ASEDir)), "Shield_Files", Config$Server$asc.File))
    if (is.null(ASE.cfg)) ASE.cfg <- fread(file.path(ASEDir, DIR_Config, paste0(ASE.name,".cfg")), showProgress =F)
    if (is.null(SetTime)) SetTime <- fread(file.path(ASEDir, DIR_Config, paste0(ASE.name,"_SETTIME.cfg")), showProgress =F)
    k              <- as.integer(which(unlist(ASE.cfg[name.gas == "name.sensor"]) == name.sensor))
    name.gas       <- names(which(unlist(ASE.cfg[name.gas == "name.sensor"]) == name.sensor))
    gas.sensor     <- ASE.cfg[name.gas == "gas.sensor", k, with = FALSE]
    gas.reference2use <- ASE.cfg[name.gas == "gas.reference2use", k, with = FALSE]
    Meas.DateIN    <- max(c(as.Date(SetTime[name.gas == "DateMeas.IN" ][[k]], optional = T), as.Date(SetTime[name.gas == "DatePlotMeas.IN" ][[k]], optional = T)), na.rm = TRUE) 
    Meas.DateEND   <- min(c(as.Date(SetTime[name.gas == "DateMeas.END"][[k]], optional = T), as.Date(SetTime[name.gas == "DatePlotMeas.END"][[k]], optional = T)), na.rm = TRUE)
    nameGasVolt    <- paste0(name.sensor,"_volt")      # sensor gas in volt or nA or Count
    nameGasMod     <- paste0(gas.sensor,"_modelled")   # modelled sensor gas
    nameGasRef     <- paste0("Out.",gas.reference2use) # Gas reference
    unit.ref       <- unlist(ASE.cfg[name.gas == "ref.unitgas"  , k, with = FALSE])
    unit.sensor    <- unlist(ASE.cfg[name.gas == "Sens.unit"    , k, with = FALSE])
    remove.neg     <- as.logical(unlist(ASE.cfg[name.gas == "remove.neg", k, with = FALSE]))
    Sync.Cal       <- as.logical(unlist(ASE.cfg[name.gas == "Sync.Cal"  , k, with = FALSE]))
    var.names.meteo <- fread(file.path(ASEDir, DIR_Config, "var.names.meteo.cfg"), showProgress = F)$columns
    eta.model.type <- unlist(ASE.cfg[name.gas == "eta.model.type", k, with = FALSE])
    # lists for all sensors
    list.gas.sensor <- unlist(ASE.cfg[name.gas == "gas.sensor"])[-1]
    list.sensors    <- unlist(ASE.cfg[name.gas == "name.sensor"])[-1]
    list.reference  <- unlist(ASE.cfg[name.gas == "gas.reference2use"])[-1]
    # General.DT
    if (is.null(General.DT)) {
        General.DT     <- fread(file.path(ASEDir, DIR_General,"General.csv"), showProgress = T)
        data.table::set(General.DT, j = "date", value =  ymd_hms(General.DT[["date"]], tz = "UTC"))}
    Save.General <- FALSE
    if (!all(paste0(list.sensors, "_volt") %in% names(General.DT))) {
        Missing.Sensor <- which(!paste0(list.sensors, "_volt") %in% names(General.DT))
        cat(paste0("ERROR [Identify_ASE] ",paste(paste0(list.sensors[Missing.Sensor], "_volt"), collapse = ", "), " are missing. Sensor data are not filtered.\n"))
        cat(paste0("INFO [Identify_ASE] the Filtering of sensor data is going to be carried out.\n"))
        General.DT <- Filter.Sensor.Data(General.DT = General.DT, list.gas.sensor = list.gas.sensor, list.name.sensor = list.sensors, boxConfig = Config, ASEDir = ASEDir, Shield = Shield)
        Save.General <- TRUE}
    # be sure that sensor and reference data are filtered
    if (!all(paste0("Out.",list.reference) %in% names(General.DT))) {
        Missing.reference <- which(!paste0("Out.",list.reference) %in% names(General.DT))
        futile.logger::flog.warn(paste0("[Identify_ASE] ",paste(paste0("Out.",list.reference[Missing.reference]), collapse = ", "), " are missing. Reference data data are not filtered.\n"))
        futile.logger::flog.info(paste0("[Identify_ASE] The Filtering of reference data is going to be carried out.\n"))
        General.DT <- Filter.Ref.Data(General.DT = General.DT, list.reference = list.reference, list.name.sensor = list.sensors, boxConfig = Config, ASEDir = ASEDir, ASE.cfg = ASE.cfg)
        Save.General <- TRUE
    }
    # Saving General.DT for next read if modified
    if (Save.General) data.table::fwrite(General.DT, file.path(ASEDir, DIR_General,"General.csv"), showProgress = T)
    # returning
    return(list(General.DT = General.DT, ASE.cfg = ASE.cfg, SetTime = SetTime, k = k, Config = Config,
                Cal.DateIN = Cal.DateIN, Cal.DateEND = Cal.DateEND, Meas.DateIN = Meas.DateIN, Meas.DateEND = Meas.DateEND, 
                nameGasVolt = nameGasVolt, nameGasMod = nameGasMod, nameGasRef = nameGasRef,
                ASE.name = ASE.name, ASEDir = ASEDir, name.sensor = name.sensor, Mod_type = Mod_type, Variables = Variables,
                ASESens.raw.unit = Sens.raw.unit, list.gas.sensor = list.gas.sensor, list.sensors = list.sensors, var.names.meteo = var.names.meteo, 
                eta.model.type = eta.model.type, list.reference = list.reference))
}
#' Flagging the sensor data for warming time
#' This dataTreatment can only works if boardTimeStamp exists, meaning only in InfluxData. It will not work with SOSData
#' @return a list of 4 character vectors, corresponding to sensors with row index of General.DT corresponding to warming time of sensor data,
#       the names of the 4 elements are the ones of list.gas.sensor   in the same order
Warm_Index <- function(Warm.Forced  = TRUE, General.DT, list.gas.sensor, boxConfig, ind.warm.file = NULL, ASEDir, DIR_General = "General_data", Save = TRUE) {
    ind.warm.file <- file.path(ASEDir, DIR_General, "ind_warm.RDS")
    # setting index for warming, reset ind.warm.out
    if (Warm.Forced) {
        ind.warm.out <- NULL
        if (!is.null(General.DT[,"boardTimeStamp"]) ) { 
            # http://r.789695.n4.nabble.com/Replace-zeroes-in-vector-with-nearest-non-zero-value-td893922.html
            # https://stackoverflow.com/questions/26414579/r-replacing-zeros-in-dataframe-with-last-non-zero-value
            # replace every boardTimeStamp which does not change with NA so na.locf will works
            # Index of boardtimeStamp similar for consecutive boardtimeStamp
            Consecutive <- which(diff(General.DT$boardTimeStamp, lag = 1) == 0)
            # Values of indexes whith previous values that are non consecutive (Re-start)
            Re_start <- Consecutive[diff(Consecutive, lag = 1) > 1]
            # Setting NA boardTimeStamp to the last non-NA boardTimeStamp
            data.table::set(General.DT,  j = "boardTimeStamp", value = na.locf(General.DT[["boardTimeStamp"]], na.rm = FALSE, fromLast = FALSE))
            # detecting when boardTimeStamp decreases suddenly (re-boot)
            Re_boot <- which(diff(General.DT$boardTimeStamp, lag = 1) < 0)
            # Combining Re_start and reboot
            if (length(Re_start) > 0 | length(Re_boot) > 0) ind <- unique(c(Re_start, Re_boot)) else ind = numeric(0)
        } else {
            # This is for SOS
            ind <- apply(General.DT[, list.gas.sensor, with = FALSE  ], 1, function(i) !all(is.na(i)))
            ind <- which(ind[2:length(ind)] & !ind[1:(length(ind) - 1 )])
        }
        # Set the first discarded value as the value restarting from 0
        ind = ind + 1
        # Adding the first switch-on
        ind <- unique(c(1,ind))
        # Creating a  vector of index of value to discard in a list including all names of sensors
        for (n in seq_along(list.gas.sensor)) {
            # number of data to be discarded after all re-start and re-boot called ind
            indfull <- integer(length(ind) * boxConfig$sens2ref$hoursWarming[n] * 60 / as.integer(boxConfig$Server$UserMins))
            # developing IndFull with index to discard for each ind
            for (i in seq_along(ind)) {
                indfull[((i - 1) *  boxConfig$sens2ref$hoursWarming[n]*60/as.integer(boxConfig$Server$UserMins) + 1):((i) * boxConfig$sens2ref$hoursWarming[n]*60 / as.integer(boxConfig$Server$UserMins))] <- ind[i]:(ind[i] +  boxConfig$sens2ref$hoursWarming[n] * 60 / as.integer(boxConfig$Server$UserMins) - 1)
            }
            # removing duplicated in case of second Re-Boot before the end of the warming time
            indfull <- unique(indfull)
            # removing  indexes outside the number of rows of General.DT
            indfull <- indfull[indfull <= length(General.DT[[list.gas.sensor[n]]])]
            # creating the list of indfull for each sensor
            if (exists("ind.warm.out")) ind.warm.out[[n]] <- indfull else ind.warm.out <- list(indfull)
        } 
        names(ind.warm.out) <- list.gas.sensor
        if (Save) list.save(x = ind.warm.out, file = ind.warm.file)
        # Setting TRh.Forced to TRUE to be sure that it is done before ind.Sens
        if (!exists("TRh.Forced")           || !TRh.Forced)           TRh.Forced           <- TRUE
        if (!exists("Inv.Forced")           || !Inv.Forced)           Inv.Forced           <- TRUE
        if (!exists("Outliers.Sens") || !Outliers.Sens) Outliers.Sens <- TRUE
        if (!exists("Conv.Forced")          || !Conv.Forced)          Conv.Forced          <- TRUE
        if (!exists("Cal.Forced")           || !Cal.Forced)           Cal.Forced           <- TRUE
    } else if (file.exists(ind.warm.file)) ind.warm.out <- list.load(ind.warm.file) else ind.warm.out <- NULL
    return(ind.warm.out)   
}
#' Flagging the sensor data for temperature and humidity outside interval of tolerance
#' @return : list of NAs for discarded temperature and humidity with as many elements as in list.gas.sensor
#                          consisting of vector of integers of the index of rows of General.DT dataframe
TRh_Index <- function(TRh.Forced  = TRUE, General.DT, list.gas.sensor,  boxConfig, ind.TRh.file = NULL, ASEDir, DIR_General = "General_data", Save = TRUE) {
    ind.TRh.file            <- file.path(ASEDir, DIR_General, "ind_TRh.RDS"  )
    if (TRh.Forced) {
        # Always starting detection of outliers for T and RH from the dataframe set in General.DT
        index.temp <- which(colnames(General.DT) %in% "Temperature")
        index.rh   <- which(colnames(General.DT) %in% "Relative_humidity")
        return.ind.TRh    <- list()
        return.ind.T.min  <- list()
        return.ind.T.max  <- list()
        return.ind.Rh.min <- list()
        return.ind.Rh.max <- list()
        for (l in list.gas.sensor) {
            # Indexes of temperature and humidity exceeding min/max thresholds
            T.min  <- General.DT[, index.temp, with = FALSE] < boxConfig$sens2ref$temp.thres.min[match(x = l, table = list.gas.sensor)]
            T.max  <- General.DT[, index.temp, with = FALSE] > boxConfig$sens2ref$temp.thres.max[match(x = l, table = list.gas.sensor)]
            Rh.min <- General.DT[, index.rh, with = FALSE]   < boxConfig$sens2ref$rh.thres.min[match(x = l, table = list.gas.sensor)]
            Rh.max <- General.DT[, index.rh, with = FALSE]   > boxConfig$sens2ref$rh.thres.max[match(x = l, table = list.gas.sensor)]
            # Global index of temperature/humidity exceeding thresholds
            return.ind.TRh[[boxConfig[["sens2ref"]][which(gas.sensor == l), name.sensor]]] <- which(T.min | T.max | Rh.min | Rh.max)
            return.ind.T.min[[ paste0(boxConfig[["sens2ref"]][which(gas.sensor == l), name.sensor],"__Temp. < ",boxConfig$sens2ref$temp.thres.min[match(x = l, table = list.gas.sensor)])]] <- which(T.min)
            return.ind.T.max[[ paste0(boxConfig[["sens2ref"]][which(gas.sensor == l), name.sensor],"__Temp. > ",boxConfig$sens2ref$temp.thres.max[match(x = l, table = list.gas.sensor)])]] <- which(T.max)
            return.ind.Rh.min[[paste0(boxConfig[["sens2ref"]][which(gas.sensor == l), name.sensor],"__RH < "   ,boxConfig$sens2ref$rh.thres.min[match(x = l, table = list.gas.sensor)])]] <- which(Rh.min)
            return.ind.Rh.max[[paste0(boxConfig[["sens2ref"]][which(gas.sensor == l), name.sensor],"__RH > "   ,boxConfig$sens2ref$rh.thres.max[match(x = l, table = list.gas.sensor)])]] <- which(Rh.max)
        }
        ind.TRh.out <- list(ind.TRh = return.ind.TRh, T.min = return.ind.T.min, T.max = return.ind.T.max, Rh.min = return.ind.Rh.min, Rh.max = return.ind.Rh.max)
        if (Save) {
            list.save(x = ind.TRh.out, file = ind.TRh.file)
            futile.logger::flog.info("[TRh_Index] A new ind_TRh.RDS was saved. Inv.Forced is set to TRUE")}
        # Setting Invalid$Forced to True to be sure that it is carried out before ind.sens
        if (!exists("Inv.Forced")    || !Inv.Forced)    Inv.Forced           <- TRUE
        if (!exists("Outliers.Sens") || !Outliers.Sens) Outliers.Sens <- TRUE
        if (!exists("Conv.Forced")   || !Conv.Forced)   Conv.Forced          <- TRUE
        if (!exists("Cal.Forced")    || !Cal.Forced)    Cal.Forced           <- TRUE
    } else if (file.exists(ind.TRh.file)) ind.TRh.out <- list.load(ind.TRh.file) else ind.TRh.out <- NULL
    return(ind.TRh.out)
}
Inv_Index <- function(Inv.Forced  = TRUE, General.DT, list.name.sensor, boxConfig, ind.TRh.file = NULL, ASEDir, DIR_Config  = "Configuration", DIR_General = "General_data", Save = TRUE) {
    ind.Invalid.file    <- file.path(ASEDir, DIR_General, "ind_Invalid.RDS")
    if (Inv.Forced) {
        # min.General.date and max.General.date----
        if (!is.null(General.DT)) min.General.date <- min(General.DT$date, na.rm = TRUE) else min.General.date <- NULL
        if (!is.null(General.DT)) max.General.date <- max(General.DT$date, na.rm = TRUE) else max.General.date <- NULL
        if (!is.null(General.DT)) {
            # reading the files with period of valid data
            for (i in seq_along(list.name.sensor)) {
                nameFile <- file.path(ASEDir, DIR_Config, paste0(basename(ASEDir),"_Valid_",list.name.sensor[i],".cfg"))
                if (file.exists(nameFile)) {
                    assign(paste0("Valid_",list.name.sensor[i]), read.table(file = nameFile, header = TRUE, row.names = NULL, comment.char = "#", stringsAsFactors = FALSE))
                } else {
                    # There are no Valid files. Creates files with IN = END = min(General$date)
                    assign(paste0("Valid_",list.name.sensor[i]), rbind(c(strftime(min(General.DT$date, na.rm = TRUE)), strftime(min(General.DT$date, na.rm = TRUE)))))
                    write.table(x         = data.frame(In  = gsub(" UTC", "",strftime(min.General.date)),
                                                       End = gsub(" UTC", "",strftime(min.General.date)),
                                                       stringsAsFactors = FALSE),
                                file      = nameFile,
                                row.names = FALSE
                    )
                }
            }
            # Creating one list with invalid periods for all sensors
            Valid <- list()
            for (i in paste0("Valid_",list.name.sensor)) Valid[[i]] <- get(i)
            # Function to convert charater strings to POSIX
            NewValid <- function(x) {
                # making each element a dataframe of POSIXct
                x <- data.frame( x, stringsAsFactors = FALSE)
                colnames(x) <- c("In", "End")
                x$In  <- parse_date_time(x$In , tz = threadr::time_zone(General.DT$date[1]), orders = "YmdHMS")
                x$End <- parse_date_time(x$End, tz = threadr::time_zone(General.DT$date[1]), orders = "YmdHMS")
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
        # make sure that Outliers.Sens$Forced is run after Invalid, to discard outliers again and to apply invalid and outliers to General.DT
        if (!exists("Outliers.Sens") || !Outliers.Sens) Outliers.Sens <- TRUE
        if (!exists("Conv.Forced")          || !Conv.Forced)          Conv.Forced          <- TRUE
        if (!exists("Cal.Forced")           || !Cal.Forced)           Cal.Forced           <- TRUE
        futile.logger::flog.info("[Inv_Index()]  A new ind_Invalid.RDS was saved. Outliers.Sens is set to TRUE")
    } else if (file.exists(ind.Invalid.file)) ind.Invalid.out <- list.load(ind.Invalid.file) else ind.Invalid.out <- NULL
    return(ind.Invalid.out)
}
# discard outliers of sensors
Outliers_Sens <- function(Outliers.Sens  = TRUE, General.DT, list.gas.sensor, list.name.sensor, boxConfig, ASEDir, DIR_Config  = "Configuration", DIR_General = "General_data",
                          ind.warm.out, ind.TRh.out, ind.Invalid.out) {
    ind.sens.out.file       <- file.path(ASEDir, DIR_General, "ind_sens_out.RDS")
    if (Outliers.Sens) {
        if (!is.null(General.DT)) setalloccol(General.DT)
        for (i in list.gas.sensor) { ######################################################only chemical sensors############################################################
            # Checking if sensor data exists in General.DT
            if (i %in% names(General.DT)) {
                # Initialisation of columns of General.DT
                Sensor.i <- na.omit(boxConfig[["sens2ref"]][[which(boxConfig[["sens2ref"]][,"gas.sensor"] == i),"name.sensor"]])
                # resetting to initial values
                Vector.columns <- paste0(c("Out.", "Out.Warm.", "Out.TRh.", "Out.Invalid.", "Out.Warm.TRh.", "Out.Warm.TRh.Inv."),i)
                set(General.DT, j = Vector.columns, value = rep(list(General.DT[[i]]), times = length(Vector.columns)))
                if (!is.null(ind.warm.out[i][[1]])) {
                    Vector.columns <- paste0(c("Out.", "Out.Warm.", "Out.Warm.TRh.", "Out.Warm.TRh.Inv."),i)
                    i.Rows <- as.integer(ind.warm.out[[i]])
                    set(General.DT, i = i.Rows, j = Vector.columns, value = rep(list(rep(NA, times = length(i.Rows))), times = length(Vector.columns)))
                }
                if (!is.null(ind.TRh.out$ind.TR[[Sensor.i]]) && length(ind.TRh.out$ind.TR[[Sensor.i]]) > 0) {
                    Vector.columns <- paste0(c("Out.", "Out.TRh.", "Out.Warm.TRh.", "Out.Warm.TRh.Inv."),i)
                    set(General.DT,i = ind.TRh.out$ind.TRh[Sensor.i][[1]], j = Vector.columns,
                        value = rep(list(rep(NA, times = length(ind.TRh.out$ind.TRh[Sensor.i][[1]]))), times = length(Vector.columns)))
                }
                if (!is.null(ind.Invalid.out[[2]][[Sensor.i]]) && length(ind.Invalid.out[[2]][[Sensor.i]]) > 0) {
                    Vector.columns <- paste0(c("Out.", "Out.Invalid." , "Out.Warm.TRh.Inv."),i)
                    set(General.DT,i = which(General.DT$date %in% ind.Invalid.out[[2]][[Sensor.i]]), j = Vector.columns,
                        value = rep(list(rep(NA, times = length(which(General.DT$date %in% ind.Invalid.out[[2]][[Sensor.i]])))), times = length(Vector.columns)))
                }
                # index (1, 2,3, 4  or 1,2,3, 6 ... comng from  selection of control uiFiltering, Calib and SetTime)
                k <- match(x = i, table = list.gas.sensor)
                iters <- boxConfig$sens2ref$Sens.iterations[k]
                set(General.DT, j = paste0("Out.",i,".",1:iters), value = rep(list(General.DT[[paste0("Out.",i)]]), times = iters))
                # deleting bigger iterations
                repeat {
                    if (paste0("Out.",list.gas.sensor[i],".", iters + 1) %in% names(General.DT)) {
                        set(General.DT, j = paste0("Out.",i,".", iters + 1), value = NULL)
                        iters <- iters + 1
                    } else break # leaving the repeat loop if there are no higher iterations
                }
                if (boxConfig$sens2ref$Sens.Inval.Out[k]) {
                    for (j in 1:iters) { # number of iterations
                        if (all(is.na(General.DT[[i]]))) {
                            futile.logger::flog.error("[Outliers_Sens] no sensor data for filtering outliers")
                        } else {
                            # Setting the columns of sensor data previous to detect outliers
                            Y <- General.DT[[paste0("Out.Warm.TRh.Inv.",i)]]
                            # setting Y for the outliers of previous iterations to NA. If null then stop outlier detection
                            if (j > 1) {
                                if (length(which(return.ind.sens.out[[paste0(i,".",(j - 1))]]$Outliers)) != 0) {
                                    if (class(Y)[1] == "tbl_df") {
                                        Y[as.numeric(paste(unlist(sapply(return.ind.sens.out[c(paste0(i,".",1:(j - 1)))],function(x) which(x$Outliers))))),] <- NA
                                    } else Y[as.numeric(paste(unlist(sapply(return.ind.sens.out[c(paste0(i,".",1:(j - 1)))],function(x) which(x$Outliers)))))] <- NA
                                } else break
                            }
                            Outli <- My.rm.Outliers(ymin         = boxConfig$sens2ref$Sens.Ymin[k],
                                                    ymax         = boxConfig$sens2ref$Sens.Ymax[k],
                                                    ThresholdMin = boxConfig$sens2ref$Sens.threshold[k],
                                                    date         = General.DT[["date"]],
                                                    y            = Y,
                                                    window       = boxConfig$sens2ref$Sens.window[k],
                                                    threshold    = boxConfig$sens2ref$Sens.threshold[k],
                                                    plotting     = FALSE
                            )
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
                                set(General.DT,i = Row.Index, j = paste0("Out."      ,i),
                                    value = list(rep(NA, times = length(which(return.ind.sens.out[[paste0(i,".",j)]]$Outliers)))))
                                set(General.DT,i = Row.Index, j = paste0("Out.",i,".",j),
                                    value = list(rep(NA, times = length(which(return.ind.sens.out[[paste0(i,".",j)]]$Outliers)))))
                            }}}}
            }
        }
        if (exists("return.ind.sens.out")) {
            list.save(x = return.ind.sens.out, file = ind.sens.out.file)
            futile.logger::flog.info("[Outliers_Sens]  A new ind_sens_out.RDS was saved. Conv.Forced is set to TRUE")
            # Force conversion of sensors
            if (!exists("Conv.Forced") || !Conv.Forced) Conv.Forced <- TRUE
            if (!exists("Cal.Forced") || !Cal.Forced)   Cal.Forced  <- TRUE}}
    # deleting unnecessary outlier replicates
    for (i in 1:length(list.gas.sensor)) for (j in 1:boxConfig$sens2ref$Sens.iterations[i]) assign(paste0(list.gas.sensor[i],".",j),NULL)
    
    return(General.DT)
}
Sens_Conv <- function(Conv.Forced  = TRUE, General.DT, list.gas.sensor, list.name.sensor, boxConfig, ASEDir, DIR_Config  = "Configuration", DIR_General = "General_data", Shield) {
    if (Conv.Forced) {
        # digits2volt conversion for whole data in nA or V
        cat("\n")
        cat("-----------------------------------------------------------------------------------\n")
        cat("[Sens_Conv()] INFO, digital to volt conversion for all sensors on the shields\n")
        # Checking Filtering of sensor and reference data
        Sensors_Cal <- merge(x = boxConfig$sens2ref[,c("name.gas","gas.sensor","name.sensor","Sens.raw.unit")], 
                             y = Shield[,c("name.gas","name.sensor","gas.sensor","RefAD","Ref","board.zero.set","GAIN","Rload")],
                             by = c("name.gas", "gas.sensor", "name.sensor"),
                             all = TRUE)
        # order Sensors_Cal as Calib_data()Goo
        # Values converted in volt or nA of sensors in Shield only if sensor data exist
        data.table::set(General.DT,  j = paste0(Shield$name.sensor,"_volt"),
                        value = ASEDigi2Volt(Sensors_Cal = Sensors_Cal[Sensors_Cal$name.gas %in% Shield$name.gas,],
                                             Digital = General.DT[,paste0("Out.",Shield$gas.sensor), with = FALSE]))
        # Values converted in volt or nA - Board zero in Volt? change to V or nA
        data.table::set(General.DT,  j = paste0(Shield$name.sensor,"_DV"),
                        value = lapply(Shield$name.sensor, function(i) rep(Shield$Ref[Shield$name.sensor == i] - Shield$RefAD[Shield$name.sensor == i],
                                                                           times = nrow(General.DT))))
        # No conversion for the sensors which are not in the Shield only if sensor data exist
        No.Shield.gas.Sensors <- setdiff(list.gas.sensor, Shield$gas.sensor)
        No.Shield.gas.Sensors <- No.Shield.gas.Sensors[which(c(paste0("Out.",No.Shield.gas.Sensors) %in% names(General.DT) ))]
        if (length(No.Shield.gas.Sensors) > 0) {
            No.Shield.name.Sensors <- setdiff(list.name.sensor, Shield$name.sensor)
            x <- General.DT[,paste0("Out.",No.Shield.gas.Sensors), with = FALSE]
            data.table::set(General.DT,  j =  paste0(No.Shield.name.Sensors,"_volt"),
                            value = lapply(seq_len(ncol(x)), function(i) x[[i]]))
            # Force conversion of sensors
            if (!exists("Cal.Forced") || !Cal.Forced)   Cal.Forced  <- TRUE}}
    return(General.DT)}

Filter.Sensor.Data <- function(General.DT, list.gas.sensor, list.name.sensor, boxConfig, ASEDir, Shield) {
    ind.warm.out     <- Warm_Index(General.DT = General.DT, list.gas.sensor  = list.gas.sensor, boxConfig = boxConfig, ASEDir = ASEDir)
    ind.TRh.out      <- TRh_Index(General.DT  = General.DT, list.gas.sensor  = list.gas.sensor, boxConfig = boxConfig, ASEDir = ASEDir)
    ind.Invalid.out  <- Inv_Index(General.DT  = General.DT, list.name.sensor = list.name.sensor,boxConfig = boxConfig, ASEDir = ASEDir)
    General.DT       <- Outliers_Sens(General.DT = General.DT, list.gas.sensor  = list.gas.sensor, list.name.sensor = list.name.sensor, boxConfig = boxConfig, ASEDir = ASEDir,
                                      ind.warm.out = ind.warm.out, ind.TRh.out = ind.TRh.out, ind.Invalid.out = ind.Invalid.out)
    General.DT       <- Sens_Conv(General.DT = General.DT, list.gas.sensor  = list.gas.sensor, list.name.sensor = list.name.sensor, boxConfig = boxConfig, ASEDir = ASEDir,
                                  Shield = Shield)
    return(General.DT)
}
Filter.Ref.Data <- function(Outliers.Ref = TRUE, General.DT, list.reference, list.name.sensor, boxConfig, ASEDir, ASE.cfg, 
                            DIR_Config  = "Configuration", DIR_General = "General_data") {
    ind.ref.out.file <- file.path(ASEDir, DIR_General, "ind_ref_out.RDS")
    if (Outliers.Ref) {
        # list of index of negative values
        ################################ ADD a Test to check that all reference parameters exists ######################
        ind.neg <- apply(X = General.DT[, .SD, .SDcols = list.reference[list.reference %in% names(General.DT)]], MARGIN = 2, function(x) {which(x < 0)})
        for (i in seq_along(list.reference)) {
            # resetting to initial values
            cat("[Filter.Ref.Data] INFO, Initialising filtered reference data columns for ", list.reference[i], "\n")
            Vector.columns <- paste0(c("Out.", "Out.Neg."),list.reference[i])
            General.DT[,(Vector.columns) := rep(list(General.DT[[list.reference[i]]]), times = length(Vector.columns))]# discarding negative values if needed
            # number index of reference pollutant in the names(ASE.cfg)
            k <- which(ASE.cfg[name.gas == "name.sensor"] == list.name.sensor[i])
            # discarding negative values if needed
            if (as.logical(ASE.cfg[name.gas == "remove.neg", k, with = F][[1]])) {
                if (exists("ind.neg") && length(ind.neg) > 0 && length(ind.neg[[list.reference[i]]]) > 0) {
                    cat("[Filter.Ref.Data] INFO, Discarding sensor data for reference negative values\n")
                    set(General.DT,i = ind.neg[[list.reference[i]]], j = Vector.columns, 
                        value = rep(list(rep(NA, times = length(ind.neg[[list.reference[i]]]))), times = length(Vector.columns)))}}
            # Initial reference data in outlier column
            cat("[Filter.Ref.Data] INFO, initialising outlier reference data for ", list.reference[i], "\n")
            iters  <- as.numeric(ASE.cfg[name.gas == "Ref.iterations", k, with = F][[1]])
            General.DT[,(paste0("Out.",list.reference[i],".",1:iters)) := rep(list(General.DT[[paste0("Out.Neg.",list.reference[i])]]), times = iters)]
            # deleting bigger iterations
            repeat {
                if (paste0("Out.",list.reference[i],".", iters + 1) %in% names(General.DT)) {
                    set(General.DT, j = paste0("Out.",list.reference[i],".",iters + 1), value = NULL)
                    iters <- iters + 1
                } else break # leaving the repeat loop if there are no higher iterations
            }
            # Index of outliers for reference data
            cat("[Filter.Ref.Data()] INFO, detecting row indexes of outliers in reference data for ", list.reference[i], "\n")
            if (as.logical(ASE.cfg[name.gas == "Ref.rm.Out", k, with = F][[1]])) {
                # number of iterations
                for (j in 1:as.numeric(ASE.cfg[name.gas == "Ref.iterations", k, with = F][[1]])) {
                    if (list.reference[i] %in% names(General.DT)) {
                        if (all(is.na(General.DT[[list.reference[i]]]))) {
                            futile.logger::flog.error("[Filter.Ref.Data] no reference data for filtering outliers")
                        } else {
                            Y <- General.DT[[paste0("Out.Neg.",list.reference[i])]]
                            # setting the outliers of previous iterations to NA. If null then stop outlier detection
                            if (j > 1) {
                                if (length(which(return.ind.ref.out[[paste0(list.reference[i],".",(j - 1))]]$Outliers)) != 0) {
                                    Y[as.numeric(paste(unlist(sapply(return.ind.ref.out[c(paste0(list.reference[i],".",1:(j - 1)))], function(x) which(x$Outliers)))))] <- NA
                                }  else break
                            }
                            cat(paste0("[Filter.Ref.Data()] Reference: ",list.reference[i],", iteration: ",j,"\n"))
                            Outli <- My.rm.Outliers(ymin         = as.numeric(ASE.cfg[name.gas == "Ref.Ymin", k, with = F][[ 1]]),
                                                    ymax         = as.numeric(ASE.cfg[name.gas == "Ref.Ymax", k, with = F][[1]]),
                                                    ThresholdMin = as.numeric(ASE.cfg[name.gas == "Ref.ThresholdMin", k, with = F][[1]]),
                                                    date         = General.DT[["date"]],
                                                    y            = Y,
                                                    window       = as.numeric(ASE.cfg[name.gas == "Ref.window", k, with = F][[1]]),
                                                    threshold    = as.numeric(ASE.cfg[name.gas == "Ref.threshold", k, with = F][[1]]),
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
                                set(General.DT,i = which(return.ind.ref.out[[paste0(list.reference[i],".",j)]]$Outliers), j = paste0("Out.",list.reference[i]),
                                    value = list(rep(NA, times = length(which(return.ind.ref.out[[paste0(list.reference[i],".",j)]]$Outliers)))))
                                set(General.DT,i = which(return.ind.ref.out[[paste0(list.reference[i],".",j)]]$Outliers), j = paste0("Out.",list.reference[i],".",j),
                                    value = list(rep(NA, times = length(which(return.ind.ref.out[[paste0(list.reference[i],".",j)]]$Outliers)))))
                            }
                        }
                    } else cat("[Filter.Ref.Data()] ERROR, Warning no reference values impossible to discard outliers\n")}}}
        if (exists("return.ind.ref.out")) {
            list.save(x = return.ind.ref.out, file = ind.ref.out.file)
            futile.logger::flog.info("[Filter.Ref.Data]  A new ind.ref.out.RDS was saved. Cal.Forced is set to TRUE")
            # Force conversion of sensors
            if (!exists("Cal.Forced") || !Cal.Forced)   Cal.Forced  <- TRUE}}
    return(General.DT)
}

#' @param ASEDir A character vector with the filepath of the ASE boxe to be submitted to the function Identify.ASE
Identify_ASE_Dir <- function(ASEDir, name.sensor = NULL, General.DT = NULL, ASE.cfg = NULL, SetTime = NULL, Config = NULL, Shield = NULL,
                             DIR_Config = "Configuration", DIR_Models = "Models", DIR_General = "General_data") {
    # name of ASE box
    ASE.name <- basename(ASEDir)
    # Extracting data 
    if (is.null(Config)) Config <- CONFIG(DisqueFieldtestDir = ASEDir, DisqueFieldtest = dirname(dirname(ASEDir)), Dir.Config = "Configuration", Verbose = F, shiny = F)
    if (is.null(Shield)) Shield <- ASEPanel04Read(ASEPanel04File = file.path(dirname(dirname(ASEDir)), "Shield_Files", Config$Server$asc.File))
    if (is.null(ASE.cfg)) ASE.cfg <- fread(file.path(ASEDir, DIR_Config, paste0(ASE.name,".cfg")), showProgress = F)
    if (is.null(SetTime)) SetTime <- fread(file.path(ASEDir, DIR_Config, paste0(ASE.name,"_SETTIME.cfg")), showProgress = F)
    if (is.null(name.sensor)) name.sensor <- unlist(ASE.cfg[name.gas == "name.sensor"])[2]
    k              <- as.integer(which(unlist(ASE.cfg[name.gas == "name.sensor"]) == name.sensor))
    name.gas       <- names(which(unlist(ASE.cfg[name.gas == "name.sensor"]) == name.sensor))
    gas.sensor     <- ASE.cfg[name.gas == "gas.sensor", k, with = FALSE]
    gas.reference2use <- ASE.cfg[name.gas == "gas.reference2use", k, with = FALSE]
    Cal.DateIN     <- max(c(as.Date(SetTime[name.gas == "DateCal.IN"  ][[k]], optional = T), as.Date(SetTime[name.gas == "DatePlotCal.IN"  ][[k]], optional = T)), na.rm = TRUE)
    Cal.DateEND    <- min(c(as.Date(SetTime[name.gas == "DateCal.END" ][[k]], optional = T), as.Date(SetTime[name.gas == "DatePlotCal.END" ][[k]], optional = T)), na.rm = TRUE)
    Meas.DateIN    <- max(c(as.Date(SetTime[name.gas == "DateMeas.IN" ][[k]], optional = T), as.Date(SetTime[name.gas == "DatePlotMeas.IN" ][[k]], optional = T)), na.rm = TRUE) 
    Meas.DateEND   <- min(c(as.Date(SetTime[name.gas == "DateMeas.END"][[k]], optional = T), as.Date(SetTime[name.gas == "DatePlotMeas.END"][[k]], optional = T)), na.rm = TRUE)
    nameGasVolt    <- paste0(name.sensor,"_volt")      # sensor gas in volt or nA or Count
    nameGasMod     <- paste0(gas.sensor,"_modelled")   # modelled sensor gas
    nameGasRef     <- paste0("Out.",gas.reference2use) # Gas reference
    unit.ref       <- unlist(ASE.cfg[name.gas == "ref.unitgas"  , k, with = FALSE])
    unit.sensor    <- unlist(ASE.cfg[name.gas == "Sens.unit"    , k, with = FALSE])
    Sens.raw.unit  <- unlist(ASE.cfg[name.gas == "Sens.raw.unit", k, with = FALSE])
    remove.neg     <- as.logical(unlist(ASE.cfg[name.gas == "remove.neg", k, with = FALSE]))
    Sync.Cal       <- as.logical(unlist(ASE.cfg[name.gas == "Sync.Cal"  , k, with = FALSE]))
    var.names.meteo <- fread(file.path(ASEDir, DIR_Config, "var.names.meteo.cfg"), showProgress =F)$columns
    eta.model.type <- unlist(ASE.cfg[name.gas == "eta.model.type", k, with = FALSE])
    # lists for all sensors
    list.gas.sensor<- unlist(ASE.cfg[name.gas == "gas.sensor"])[-1]
    list.sensors   <- unlist(ASE.cfg[name.gas == "name.sensor"])[-1]
    list.reference <- unlist(ASE.cfg[name.gas == "gas.reference2use"])[-1]
    # General.DT
    if (is.null(General.DT)) {
        General.DT     <- fread(file.path(ASEDir, DIR_General,"General.csv"), showProgress = T)
        data.table::set(General.DT, j = "date", value =  ymd_hms(General.DT[["date"]], tz = "UTC"))
    }
    # # Checking Filtering of sensor and reference data
    # Sensors_Cal <- merge(x = Config$sens2ref[,c("name.gas","gas.sensor","name.sensor","Sens.raw.unit")], 
    #                      y = Shield[,c("name.gas","name.sensor","gas.sensor","RefAD","Ref","board.zero.set","GAIN","Rload")],
    #                      by = c("name.gas", "gas.sensor", "name.sensor"),
    #                      all = TRUE)
    Save.General <- FALSE
    if (!all(paste0(list.sensors, "_volt") %in% names(General.DT))) {
        Missing.Sensor <- which(!paste0("Out.",list.sensors) %in% names(General.DT))
        cat(paste0("ERROR [Identify_ASE_Dir] ",paste(paste0("Out.",list.sensors[Missing.Sensor]), collapse = ", "), " are missing. Sensor data are not filtered.\n
                        INFO [Identify_ASE_Dir] the Filtering of sensor data is going to be carried out.\n"))
        General.DT <- Filter.Sensor.Data(General.DT = General.DT, list.gas.sensor = list.gas.sensor, list.name.sensor = list.sensors, boxConfig = Config, ASEDir = ASEDir, Shield)
        Save.General <- TRUE
    }
    # be sure that sensor and reference data are filtered
    if (!all(paste0("Out.",list.reference) %in% names(General.DT))) {
        Missing.reference <- which(!paste0("Out.",list.reference) %in% names(General.DT))
        cat(paste0("ERROR [Identify_ASE_Dir] ",paste(paste0("Out.",list.reference[Missing.reference]), collapse = ", "), " are missing. Reference data data are not filtered.\n"))
        cat(paste0("INFO [Identify_ASE_Dir] The Filtering of reference data is going to be carried out.\n"))
        General.DT <- Filter.Ref.Data(General.DT = General.DT, list.reference = list.reference, list.name.sensor = list.sensors, boxConfig = Config, ASEDir = ASEDir, ASE.cfg = ASE.cfg)
        Save.General <- TRUE
    }
    if (Save.General) data.table::fwrite(General.DT, file.path(ASEDir, DIR_General,"General.csv"), showProgress = T)
    return(list(General.DT = General.DT, ASE.cfg = ASE.cfg, SetTime = SetTime, k = k, Config = Config, Shield = Shield, 
                Cal.DateIN = Cal.DateIN, Cal.DateEND = Cal.DateEND, Meas.DateIN = Meas.DateIN, Meas.DateEND = Meas.DateEND, 
                nameGasVolt = nameGasVolt, nameGasMod = nameGasMod, nameGasRef = nameGasRef,
                ASE.name = ASE.name, ASEDir = ASEDir, name.sensor = name.sensor,
                ASESens.raw.unit = Sens.raw.unit, list.gas.sensor = list.gas.sensor, list.sensors = list.sensors, var.names.meteo = var.names.meteo, 
                eta.model.type = eta.model.type, list.reference = list.reference))
}
#' Apply a calibration model for one sensor of one ASE box
#'
#' @param Model full filepath and name of model of the calibration model.
#' @param name.sensor character vector with the name(s) of sensor (e. g. CO_A4_P1) to look for models. Default is NULL. 
#' If NULL name.sensor is extracted from Model using Identify_ASE function
#' @param Mod_type Character vector, type of models of Model.i. Default is NULL. If NULL the model type is extracted from Model.i.
#' @param Variables Character vector with parameters of model separated with &. Default is NULL. if NULL Variables is extracted from.
#' @param General.DT a data.table with all ASE Box and reference data. Default is NULL. if NULL, Generalcsv loaded with Identify_ASE is loaded
#' @param ASE.cfg File path of the subdirectory of ASEDir where is the file General.csv.
#' @return a list of charateristics of a sensor in ASE box with data.table General.DT = General.DT, config data.table ASE.cfg, k = k, 
# dates of calibration Cal.DateIN  and Cal.DateEND = Cal.DateEND, full date interval of measurements Meas.DateIN and Meas.DateEND, column names of 
# sensor raw data (nameGasVolt), sensor predicted data (nameGasMod), corresponding reference data (nameGasRef), sensor raw unit (Sens.raw.unit)
# vector of list of sensor (list.sensors), vector of list of meteorological parameters (var.names.meteoDates).
#' @examples
#' General.DT <- Apply_Model(Model = Model, General.DT = General.DT, ASE.cfg = ASE.cfg)
Apply_Model    <- function(Model, Mod_type = NULL, name.sensor = NULL, Variables = NULL, General.DT = NULL, ASE.cfg = NULL, SetTime = NULL, Config = NULL, Shield = NULL) {
    # Identify ASE box charateristics
    ASE.ID <- Identify_ASE(Model = Model, General.DT = General.DT, ASE.cfg = ASE.cfg, SetTime = SetTime, Config = Config, Shield = Shield)
    # Extract Mod_type
    if (is.null(Mod_type)) Mod_type <- ASE.ID$Mod_type
    # Extract name.sensor
    if (is.null(name.sensor)) name.sensor <- ASE.ID$name.sensor
    # Extract Variables
    if (is.null(Variables)) Variables <- ASE.ID$Variables
    if (!is.null(Variables)) {
        CovMod  <- unlist(strsplit(x = Variables, split = "&", fixed = T))
        # Checking if there are "-" in the CovMod, deleting degrees of polynomial
        if (any(grepl(pattern = "-", x = CovMod[1]))) {
            Model.CovMod <- unlist(strsplit(x = CovMod , split = "-"))
            CovMod  <- Model.CovMod[ seq(from = 1, to = length(Model.CovMod), by = 2) ]
            Degrees <- Model.CovMod[ seq(from = 2, to = length(Model.CovMod), by = 2) ]
        }
        # take only the one that is nor NA of y = General.DT[!is.na(General.DT[, nameGasVolt]), nameGasVolt]
        if (any(!CovMod %in% names(General.DT))) futile.logger::flog.error(paste0("[Meas_Func] ", name.Temperature, " is not included into General.DT. Please check names of covariates."))
        is.not.NA.y <- which(complete.cases(General.DT[, .SD, .SDcols = c(ASE.ID$nameGasVolt, CovMod)]))}
    # Extract General.DT
    if (is.null(General.DT)) General.DT <- ASE.ID$General.DT
    # Extract ASE.cfg
    if (is.null(ASE.cfg)) ASE.cfg <- ASE.ID$ASE.cfg
    # Calibration Model
    Model.i <- load_obj(file.path(Model))
    # Preparing the matrix of covariates
    if (Mod_type == "MultiLinear") {
        Matrice     <- data.frame(General.DT[is.not.NA.y, CovMod, with = FALSE], row.names = row.names(General.DT[is.not.NA.y,]), stringsAsFactors = FALSE)
        names(Matrice) <- CovMod
    } else if (Mod_type %in% c("exp_kT_NoC","exp_kT","exp_kK","T_power", "K_power")) {
        Matrice     <- data.frame(General.DT[is.not.NA.y, ..CovMod], row.names = row.names(General.DT[is.not.NA.y,]), stringsAsFactors = FALSE)
        names(Matrice) <- CovMod
    } else if (Mod_type %in% c("BeerLambert")) {
        # take only the one that is nor NA of y = General.DT[!is.na(General.DT[, nameGasVolt]), nameGasVolt]
        is.not.NA.y <- which(complete.cases(General.DT[, .SD, .SDcols = c(ASE.ID$nameGasVolt, "Temperature", "Atmospheric_pressure")]))
        Matrice     <- data.frame(General.DT[is.not.NA.y, c("Temperature", "Atmospheric_pressure")], row.names = row.names(General.DT[is.not.NA.y,]), stringsAsFactors = FALSE)
        names(Matrice) <- c("Temperature", "Atmospheric_pressure")
    } else if (Mod_type %in% c("Kohler")) {
        # take only the one that is nor NA of y = General.DT[!is.na(General.DT[, nameGasVolt]), nameGasVolt]
        is.not.NA.y <- which(complete.cases(General.DT[, .SD, .SDcols = c(ASE.ID$nameGasVolt, "Relative_humidity")]))
        Matrice     <- data.frame(General.DT[is.not.NA.y, c("Relative_humidity")], row.names = row.names(General.DT[is.not.NA.y,]), stringsAsFactors = FALSE)
        names(Matrice) <- c("Relative_humidity")
    } else {
        # Removing na for nameGasMod for nameGasVolt missing
        is.not.NA.y    <- which(!is.na(General.DT[[ASE.ID$nameGasVolt]]))
        Matrice <- NULL
    }
    # Using the reverse calibration function (measuring function) to extrapolate calibration
    data.table::set(General.DT, i = is.not.NA.y, j = ASE.ID$nameGasMod,
                    value = list(Meas_Function(y          = General.DT[[ASE.ID$nameGasVolt]][is.not.NA.y],
                                               Mod_type   = Mod_type ,
                                               covariates = CovMod,
                                               Degrees    = Degrees,
                                               Model      = Model.i,
                                               Matrice    = Matrice)))
    # Setting na for nameGasMod when nameGasVolt or CovMod are missing
    is.NA.y     <- setdiff(1:nrow(General.DT), is.not.NA.y)
    if (length(is.NA.y) > 0) data.table::set(General.DT, i = is.NA.y, j = ASE.ID$nameGasMod, value = list(rep(NA, times = length(is.NA.y))))
    # setting negative values to NA
    if (unlist(ASE.cfg[name.gas == "Neg.mod", ASE.ID$k, with = FALSE]) == TRUE) {
        data.table::set(General.DT, i = which(General.DT[, ASE.ID$nameGasMod, with = FALSE] < 0), j = ASE.ID$nameGasMod,
                        value = list(rep(NA, times = length(which(General.DT[, ASE.ID$nameGasMod, with = FALSE] < 0)))))}
    return(General.DT)
}

#' Fit a new calibration model
#'
#' @param ASEDir file path of the ASE Box used to save the Models as in file.path(ASEDir, DIR_Models).
#' @param General.DT a data.table with all ASE Box and reference data. Default is NULL. if NULL, Generalcsv loaded with Identify_ASE is loaded
#' @param ASE.cfg A data.table with all ASE box configuration. Default is null. If NULL the ASE.cfg file is loaded
#' @param SetTime A data.table with all ASE box SetTime configuration. Default is null. If NULL the ASE_SETTIME.cfg file is loaded
#' @param DIR_Config File path of the subdirectory of ASEDir where is the file ASE.name.cfg.
#' @param DIR_Models File path of the subdirectory of ASEDir where are the calibration models.
#' @param DIR_General File path of the subdirectory of ASEDir where is the file General.csv.
#' @param Cal.DateIN Starting date for calibration, POSIXct or Date type. Default is NULL If NULL, Cal.DateIN is extracted from SetTime.
#' @param Cal.DateEND Ending date for calibration, POSIXct or Date type. Default is NULL If NULL, Cal.DateEND is extracted from SetTime.
#' @param Meas.DateIN Starting date for prediction, POSIXct or Date type. Default is NULL If NULL, Meas.DateIN is extracted from SetTime.   NOT USED
#' @param Meas.DateEND Starting date for prediction, POSIXct or Date type. Default is NULL If NULL, Meas.DateEND is extracted from SetTime. NOT USED
#' @param name.sensor Character vector with the sensor name (e. g. CO_A4_P1) for which to fit a new calibration model. 
#' @param Mod_type Character vector, type of models of the model type to be fitted. Default is "Linear.Robust".
#' @param namesCovariates Character vector with parameters of calibration model separated with &. Default is NULL.
#' @param Plot_Line Logical, default is TRUE. If TRUE the calibration line is added using par(new=TRUE) to an existaing scatterplot.
#' @param PlotCal Logical, defaut TRUE, if TRUE plot the calibrated data (scatterplot and timeseries) are plotted after calibration.
#' @return a calibration model that is also saved
#' @examples
#' New.General <- Fit_New_Model(ASEDir = ASEDir, name.sensor = "CO_A4_P1", Mod_type = "Linear.Robust", 
#'                              Cal.DateIN = DateIN, Cal.DateEND = DateEND, Meas.DateIN = ASE.ID$Meas.DateIN, Meas.DateEND = ASE.ID$Meas.DateEND,
#'                              Plot_Line = FALSE, PlotCal = FALSE, Verbose = FALSE)
Fit_New_Model <- function(ASEDir, General.DT = NULL, ASE.cfg = NULL, SetTime = NULL,
                          DIR_Config = "Configuration", DIR_Models = "Models", DIR_General = "General_data",
                          Cal.DateIN = NULL, Cal.DateEND = NULL, Meas.DateIN = NULL, Meas.DateEND = NULL,
                          name.sensor, Mod_type = "Linear.Robust", namesCovariates = "", degrees = rep("1", length(namesCovariates)+1),
                          Plot_Line = FALSE, PlotCal = FALSE, Verbose = TRUE, Include.Model = FALSE, SAVE = TRUE) {
    # Plot_Line     : logical, default is TRUE. If TRUE the calibration line is added using par(new=TRUE) to an existaing scatterplot
    # PlotCal              : logical, defaut TRUE, if TRUE plot the calibrated data (scatterplot and timeseries) after calibration
    # return Mthing but the calibration model is saved
    # 
    # name of ASE box
    ASE.name <- basename(ASEDir)
    # init 
    # Extracting data 
    if (is.null(General.DT)) {
        General.DT     <- fread(file.path(ASEDir, DIR_General,"General.csv"), showProgress = T)
        data.table::set(General.DT, j = "date", value =  ymd_hms(General.DT[["date"]], tz = "UTC"))}
    if (is.null(ASE.cfg)) ASE.cfg <- fread(file.path(ASEDir, DIR_Config, paste0(ASE.name,".cfg")), showProgress = F)
    if (is.null(SetTime)) SetTime <- fread(file.path(ASEDir, DIR_Config, paste0(ASE.name,"_SETTIME.cfg")), showProgress = F)
    k              <- as.integer(which(unlist(ASE.cfg[name.gas == "name.sensor"]) == name.sensor))
    name.gas       <- names(which(unlist(ASE.cfg[name.gas == "name.sensor"]) == name.sensor))
    gas.sensor     <- ASE.cfg[name.gas == "gas.sensor", k, with = FALSE]
    gas.reference2use <- ASE.cfg[name.gas == "gas.reference2use", k, with = FALSE]
    if (is.null(Cal.DateIN))   Cal.DateIN     <- max(c(as.Date(SetTime[name.gas == "DateCal.IN"  ][[k]], optional = T), as.Date(SetTime[name.gas == "DatePlotCal.IN"  ][[k]], optional = T)), na.rm = TRUE)
    if (is.null(Cal.DateEND))  Cal.DateEND    <- min(c(as.Date(SetTime[name.gas == "DateCal.END" ][[k]], optional = T), as.Date(SetTime[name.gas == "DatePlotCal.END" ][[k]], optional = T)), na.rm = TRUE)
    # if not all degrees are one, create MultiFile
    if (Mod_type == "MultiLinear" && any(degrees != "1")) {
        name.Multi  <- file.path(ASEDir,"Configuration", paste0(ASE.name,"_Multi_",name.sensor,".cfg"))
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
                             unit.ref           = unlist(ASE.cfg[name.gas == "ref.unitgas"  , k, with = FALSE]),
                             unit.sensor        = unlist(ASE.cfg[name.gas == "Sens.unit"    , k, with = FALSE]),
                             Sens.raw.unit      = unlist(ASE.cfg[name.gas == "Sens.raw.unit", k, with = FALSE]),
                             Reference.name     = "",
                             AirSensEur.name    = ASE.name,
                             name.sensor        = name.sensor,
                             timeseries.display = FALSE,
                             WDoutputMod        = file.path(ASEDir,DIR_Models),
                             WDoutput           = file.path(ASEDir,"Calibration"),
                             WDoutputStats      = file.path(ASEDir,"Statistics"),
                             process.step       = "Calibration",
                             mod.eta.model.type = Mod_type,
                             Multi.File         = name.Multi,
                             eta.model.type     = unlist(ASE.cfg[name.gas == "eta.model.type", k, with = FALSE]),
                             remove.neg         = as.logical(unlist(ASE.cfg[name.gas == "remove.neg", k, with = FALSE])),
                             Covariates         = namesCovariates,
                             Plot_Line          = Plot_Line,
                             PlotCal            = PlotCal,
                             Auto.Lag           = as.logical(unlist(ASE.cfg[name.gas == "Sync.Cal"  , k, with = FALSE])),
                             Verbose            = Verbose,
                             Include.Model      = Include.Model,
                             SAVE               = SAVE)
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
#' @param DIR_Models File path of the subdirectory of ASEDir where are the calibration models.
#' @param Interval Integer, default value is NULL. A number of days between Cal.DateIN and Cal.DateEND for rollling calibration models. 
#'                          IF NULL determined using SetTime file
#' @param DateIN Date, default is NULL, A date for the begining of the rolling calibration intervals. If NULL DateIN is determined using Identify.ASE().
#' @param DateEN Date, default is NULL, A date for the end of the rolling calibration intervals. If NULL DateIN is determined using Identify.ASE().
#' @param name.sensors Character vector, with the sensors names for which rolling models are going to be fitted. Default is NULL, in this case models are fillted for all sensors
#' @param Mod_type Character vector, type of models to be fitted as defined in Cal_Line(). Default is "Linear.Robust".
#' @param namesCovariates Character vector with the explanatory variables in case of multiLinear model. Default is "".
#' @param degrees Character vector with the degrees of the explanatory variables in case of multiLinear model. Default is "".
#' @param DIR_Models Character vector, subdirectory of ASEDir where are the calibration models.
#' @return A charater vector with the list of all new fitted models.
#' @examples
#' Roll_Fit_New_Model(List.ASE = List.ASE[1], ASEDir = ASEDir, Interval = NULL, DateIN = as.Date("2020-01-17"))
Roll_Fit_New_Model <- function(ASEDir, ASE.ID = NULL, DIR_Models = "Models", Interval = NULL, DateIN = NULL, DateEND = NULL,
                               name.sensors = NULL, Mod_type = "Linear.Robust", namesCovariates = "", degrees = "1", Verbose = TRUE) {
    for (ASEDir in ASEDir) {
        # Identify ASE based on model name if any existing or on ASEDir
        ASE.name <- basename(ASEDir)
        if (is.null(ASE.ID)) ASE.ID <- Identify_ASE_Dir(ASEDir = ASEDir)
        # Determining names of sensors in ASE box if name.sensors is NULL
        if (is.null(name.sensors)) name.sensors <- ASE.ID$list.sensors
        for (name.sensor in name.sensors) {
            if (ASE.ID$ASEDir != ASEDir) {
                ASE.ID <- Identify_ASE_Dir(ASEDir = ASEDir, name.sensor = name.sensor, General.DT = ASE.ID$General.DT, ASE.cfg = ASE.ID$ASE.cfg, SetTime = ASE.ID$SetTime)
            } else if (ASE.ID$name.sensor != name.sensor) ASE.ID <- Identify_ASE_Dir(ASEDir = ASEDir, name.sensor = name.sensor)
            if (is.null(Interval)) Cal.Interval <- as.integer(ASE.ID$Cal.DateEND - ASE.ID$Cal.DateIN) else Cal.Interval <- Interval
            if (is.null(DateIN))   Meas.DateIN  <- ASE.ID$Meas.DateEND else Meas.DateIN  <- DateIN
            Cal.DateIN  <- Meas.DateIN 
            Cal.DateEND <- Cal.DateIN + Cal.Interval 
            if (is.null(DateEND)) Meas.DateEND <- ASE.ID$Meas.DateEND else Meas.DateEND <- DateEND
            while (Cal.DateEND <= Meas.DateEND) {
                #checking if there are data
                nameModel  <- file.path(ASEDir, DIR_Models, 
                                        paste0(c(paste0(c(ASE.name,name.sensor,ASE.ID$ASESens.raw.unit,Mod_type, format(Cal.DateIN,"%Y%m%d"),format(Cal.DateEND,"%Y%m%d"),
                                                          ifelse(any(namesCovariates != ""),paste(paste0(namesCovariates, paste0("-",degrees)),collapse = "&"),"")),"__"),".rdata"), collapse = ""))
                # Checking that data are not empty
                Data.num <- which(complete.cases(ASE.ID$General.DT[date >= Cal.DateIN & date <= Cal.DateEND, .SD, .SDcol = c(ASE.ID$nameGasVolt,ASE.ID$nameGasRef)]))
                if (length(Data.num) > 10) {
                    if ((!file.exists(nameModel))) {
                        if (Verbose) cat(paste0("Fit: ", nameModel, "\n"))
                        Fit_New_Model(ASEDir = ASEDir, General.DT = ASE.ID$General.DT, name.sensor = name.sensor, ASE.cfg = ASE.ID$ASE.cfg, SetTime = ASE.ID$SetTime,
                                      Mod_type = Mod_type, namesCovariates = namesCovariates, degrees = degrees,
                                      Cal.DateIN  = Cal.DateIN , Cal.DateEND  = Cal.DateEND, 
                                      Plot_Line = FALSE, PlotCal = FALSE, Verbose = FALSE)
                    } else {
                        cat(paste0("ASE box ", ASE.name,", sensor ", str_pad(name.sensor, width = max(str_length(ASE.ID$list.sensors),na.rm = T)), ", ", nameModel, " exists\n"))
                        if (!exists("Models.Already")) Models.Already <- nameModel else Models.Already <- c(Models.Already,nameModel)
                    } 
                    if (!exists("List.Added.Models")) List.Added.Models <- nameModel else List.Added.Models <- c(List.Added.Models,nameModel)
                } else {
                    futile.logger::flog.error(paste0("ASE box ", ASE.name,", sensor ", str_pad(name.sensor, width = max(str_length(ASE.ID$list.sensors),na.rm = T)), ", no data to fit ", nameModel, "."))
                }
                Cal.DateIN  <- Cal.DateIN  + 1 
                Cal.DateEND <- Cal.DateEND + 1
            }
            rm(Cal.Interval, Cal.DateIN, Cal.DateEND)
        }
    }
    if (!exists("Models.Already")) Models.Already <- character(0)
    return(list(List.Added.Models = List.Added.Models, Models.Already = Models.Already))
}
#' Compare performance of a list of calibration model for one sensor of one ASE box
#'
#' @param List.models a character vector with full filepaths of calibration model files to be compared. Combine ASEDIR and output of function List_models() to create List.models.
#' @param General.DT A data.table with all ASE Box and reference data. Default is NULL. if NULL, Generalcsv loaded with Identify_ASE is loaded
#' @param ASE.cfg File path of the subdirectory of ASEDir where is the file General.csv. If is NULL the ASE_cfg.cfg file is loaded. If General.DT is given, ASE.cfg shall be given as well.
#' @param SetTime A data.table with all ASE box SetTime configuration. Default is null. If is NULL the ASE_SETTIME.cfg file is loaded.If General.DT is given, SetTime shall be given as well.
#' @param DIR_Config File path of the subdirectory of ASEDir where is the file ASE.name.cfg.
#' @param DIR_Models File path of the subdirectory of ASEDir where are the calibration models.
#' @param DIR_General File path of the subdirectory of ASEDir where is the file General.csv.
#' @param Val.Interval Not used for now. Default value is NULL
#' @return return a list with statistics of all models per gas.sensor.
#' @examples
#' CO.Compare  <- Compare_Models(file.path(ASEDir, "Models", List.models))
Compare_Models <- function(List.models, ASE.ID = NULL, General.DT = NULL, ASE.cfg = NULL, SetTime = NULL, Config = NULL, Shield = NULL, 
                           Cal.DateIN = NULL, Cal.DateEND = NULL, Meas.DateIN = NULL, Meas.DateEND = NULL,
                           DIR_Config = "Configuration", DIR_Models = "Models", DIR_General = "General_data", Val.Interval = "ALL", Verbose = FALSE) {
    # Identify ASE box charateristics from first Model if needed
    if (is.null(ASE.ID)){
        if (!is.null(General.DT)) {
            ASE.ID <- Identify_ASE(Model = List.models[1], General.DT = General.DT, ASE.cfg = ASE.cfg, SetTime = SetTime, Config = Config, Shield = Shield) 
        } else ASE.ID <- Identify_ASE(Model = List.models[1])} 
    # Adding a row of statistics for each Model
    for (Model in List.models) {
        if (Verbose) futile.logger::flog.info(paste0("Model : ", basename(Model), ""))
        # Identify ASE box charateristics taking into consideration possible ASE box change
        if (ASE.ID$ASE.name == basename(dirname(dirname(Model)))) {
            ASE.ID <- Identify_ASE(Model = Model, General.DT = ASE.ID$General.DT, ASE.cfg = ASE.ID$ASE.cfg, SetTime = ASE.ID$SetTime, Config = Config, Shield = Shield)
            futile.logger::flog.warn(paste0("[Compare_Models] General.csv for ",ASE.ID$ASE.name, " not loaded."))
        } else {
            ASE.ID <- Identify_ASE(Model = Model)
            futile.logger::flog.info(paste0("[Compare_Models] General.csv for ",ASE.ID$ASE.name, " loaded."))
        } 
        # Calibration Model
        if (is.null(Cal.DateIN))  Cal.DateIN  <- ASE.ID$Cal.DateIN
        if (is.null(Cal.DateEND)) Cal.DateEND <- ASE.ID$Cal.DateEND
        Variables   <- ASE.ID$Variables
        Model.i     <- load_obj(Model)
        R2raw       <- ifelse("r.squared" %in% names(Model.i$Glance), Model.i$Glance$r.squared, NA)
        # Starting by applying the calibration model
        ASE.ID$General.DT <- Apply_Model(Model = Model, General.DT = ASE.ID$General.DT, ASE.cfg = ASE.ID$ASE.cfg, Config =  ASE.ID$Config, Shield =  ASE.ID$Config$sens2ref.shield)
        # Fitting during Calibration period
        if (all(c(ASE.ID$nameGasRef,ASE.ID$nameGasMod) %in% names(ASE.ID$General.DT))) {
            N.Cal <- length(which(complete.cases(ASE.ID$General.DT[date >= Cal.DateIN & date <= Cal.DateEND + 1, .SD, .SDcols = c(ASE.ID$nameGasRef,ASE.ID$nameGasMod)])))
        } else return(futile.logger::flog.error("[Compare_Models] sensor or reference data are missing!\n")) 
        if (N.Cal > 10) {
            Calibration <- Cal_Line(x = ASE.ID$General.DT[date >= ymd(Cal.DateIN) & date <= ymd(Cal.DateEND) + 1][[ASE.ID$nameGasRef]], s_x = NULL,
                                    y = ASE.ID$General.DT[date >= ymd(Cal.DateIN) & date <= ymd(Cal.DateEND) + 1][[ASE.ID$nameGasMod]], s_y = NULL,
                                    Mod_type      = ASE.ID$eta.model.type,
                                    Matrice       = NULL, Weighted      = FALSE,
                                    Auto.Lag      = as.logical(unlist(ASE.ID$ASE.cfg[name.gas == "Sync.Cal", ASE.ID$k, with = FALSE])),
                                    Plot_Line     = FALSE, Verbose = FALSE)    
        } else return(futile.logger::flog.error("[Compare_Models] less than 10 data for calibration, change Cal.DateIN and Cal.DateEND dates in config file.\n"))
        # Validation period
        if (is.null(Meas.DateIN))  Meas.DateIN  <- ASE.ID$Meas.DateIN
        if (is.null(Meas.DateEND)) Meas.DateEND <- ASE.ID$Meas.DateEND
        N.Predict <- length(which(complete.cases(ASE.ID$General.DT[date >= Meas.DateIN & date <= Meas.DateEND + 1, .SD, .SDcols = c(ASE.ID$nameGasRef,ASE.ID$nameGasMod)])))
        if (!N.Predict > 10) {
            futile.logger::flog.warn(("[Compare_Models] no data for prediction, using dates of calibration."))
            Meas.DateIN  <- Cal.DateIN
            Meas.DateEND <- Cal.DateEND}
        # Averagind data for Prediction if needed
        if (ASE.ID$Config$Server$UserMinsAvg != ASE.ID$Config$Server$UserMins) {
            Prediction.DT <- DF_avg(ASE.ID$General.DT[, .SD, .SDcols = c("date",ASE.ID$nameGasRef,ASE.ID$nameGasMod)], width = ASE.ID$Config$Server$UserMinsAvg)
        } else Prediction.DT <- ASE.ID$General.DT
        Prediction <- Cal_Line(x = Prediction.DT[date >= Meas.DateIN & date <= Meas.DateEND + 1][[ASE.ID$nameGasRef]], s_x = NULL,
                               y = Prediction.DT[date >= Meas.DateIN & date <= Meas.DateEND + 1][[ASE.ID$nameGasMod]], s_y = NULL,
                               Mod_type      = ASE.ID$eta.model.type,
                               Matrice       = NULL, Weighted      = FALSE,
                               Auto.Lag      = as.logical(unlist(ASE.ID$ASE.cfg[name.gas == "Sync.Cal", ASE.ID$k, with = FALSE])),
                               Plot_Line     = FALSE, Verbose = FALSE)
        # returning part number of sensor
        if ("Sensors.cfg" %in% names(ASE.ID$Config)) {
            SN.Cal <- unique(ASE.ID$Config$Sensors.cfg[time <= Meas.DateEND & name == ASE.ID$name.sensor][["serial"]])
            if (length(SN.Cal) == 0) SN.Cal = "unknown" else if (length(SN.Cal) > 1) SN.Cal <- paste(SN.Cal, collapse = ", ")
        } else SN.Cal = "unknown"
        if ("Sensors.cfg" %in% names(ASE.ID$Config)) {
            SN.Pred <- unique(ASE.ID$Config$Sensors.cfg[time <= Meas.DateEND & name == ASE.ID$name.sensor][["serial"]])
            if (length(SN.Pred) == 0) SN.Pred = "unknown" else if (length(SN.Pred) > 1) SN.Pred <- paste(SN.Pred, collapse = ", ")
        } else SN.Pred = "unknown"
        cal.row     <- data.table(ASE.name = ASE.ID$ASE.name, name.sensor = ASE.ID$name.sensor, Unit = ASE.ID$ASESens.raw.unit, Mod_type = ASE.ID$Mod_type, 
                                  Cal.DateIN = Cal.DateIN, Cal.DateEND = Cal.DateEND, Variables = Variables, 
                                  R2raw = R2raw, AICraw = Model.i$Glance$AIC, BICraw = Model.i$Glance$BIC,
                                  R2Cal = summary(Calibration)$r.squared, Intcal = Calibration$coefficients[1], SlopeCal = Calibration$coefficients[2],
                                  RMSECal = sqrt(sum(Calibration$residuals^2)/(length(Calibration$residuals) - 2)), AICCal = AIC(Calibration), BICCal = BIC(Calibration), Calibration = Calibration$Equation,
                                  Prediction.IN = Meas.DateIN, Prediction.END = Meas.DateEND,
                                  R2Pred = summary(Prediction)$r.squared, IntPred = Prediction$coefficients[1], SlopePred = Prediction$coefficients[2],
                                  RMSEPred = sqrt(sum(Prediction$residuals^2)/(length(Prediction$residuals) - 2)),
                                  AICPred = AIC(Prediction), BICPred = BIC(Prediction), Prediction = Prediction$Equation, N.Cal = N.Cal, N.Predict = N.Predict, SN.Cal = SN.Cal, SN.Pred = SN.Pred)
        Number.Columns <- length(names(cal.row))
        for (Coeffs in seq_along(Model.i$Coef)) cal.row[,(names(Model.i$Coef)[Coeffs]) := Model.i$Coef[Coeffs]]
        if (exists("cal.row")) {
            if (exists("Table.compare")) Table.compare <- rbindlist(list(Table.compare,cal.row), use.names = T, fill = T) else Table.compare <- cal.row
            rm(cal.row)}
        Number.Columns <- length(names(Table.compare))
        Number.Columns.Coeffs  <- sapply(names(Model.i$Coef), function(i) which(names(Table.compare) %in% i))
        setcolorder(Table.compare, c(1:7,Number.Columns.Coeffs,setdiff(1:Number.Columns, c(1:7,Number.Columns.Coeffs))))
    }
    return(Table.compare)
}
#' Confidence interval of coefficents of calibration Models for sensors and Model type
#'
#' @param All.Compare A data.table for all ASE_Boxes and sensors with comparison data of all calibration models per sensor. 
#' List_All_Compare shall be the output of function List_All_Compare().
#' @param Mod_type Charater vector, default is "Linear.Robust", the model type to be fitted, e.g. "Linear.Robust"
#' @return A data.table with medians of intersep ? Sintercept and slope, x ? Sx,  one row per sensor of the ASE boxes
#' @examples
#' Confidence_Coeffs(All.Compare)
Confidence_Coeffs <- function(All.Compare, Mod_type = "Linear.Robust") {
    if ("Table.Coeffs" %in% ls(pos = ".GlobalEnv")) rm(Table.Coeffs, pos = ".GlobalEnv")
    for (name.sensor in unique(All.Compare$name.sensor)) {
        Coeffs      <- data.table(name.sensor = name.sensor)
        Co_variates <- names(All.Compare)[(which(names(All.Compare) == "Variables") + 1) : (which(names(All.Compare) == "R2raw") - 1)]
        for (i in Co_variates) {
            Coeffs[,(i):= median(All.Compare[[i]][All.Compare$name.sensor == name.sensor & All.Compare$Mod_type == Mod_type])] 
            Coeffs[,(paste0("S",i)):= sd(All.Compare[[i]][All.Compare$name.sensor == name.sensor & All.Compare$Mod_type == Mod_type])]
            Coeffs[,(paste0("Mad",i)):= mad(All.Compare[[i]][All.Compare$name.sensor == name.sensor & All.Compare$Mod_type == Mod_type], na.rm = T)] 
        }
        if (!exists("Table.Coeffs")) Table.Coeffs <- Coeffs else Table.Coeffs <- rbindlist(list(Table.Coeffs,Coeffs), use.names = T, fill = T)
    }
    return(Table.Coeffs)
}
#' Comparing all models of selected boxes in a vector of filepaths (ASEDir), saving and returning the comparisons
#'
#' @param ASEDir A character vector with the list of filepaths of ASE boxes to be submitted to the function List.All.Compare
#' @param name.sensor Character vector, character vector with the name(s) of sensors to compare models. Default is "ALL" that compare the models for all sensors.
#' @param DIR_Models Character vector, subdirectory of ASEDir where are the calibration models.
#' @param All.Models Character vector, all file paths of model to be compared. Defulat is NULL. If NULL all existing models per sensor are compared.
#' @return A list of data.tables, one per sensor of the ASE boxes with comparison data of all models per sensor
#' @examples
#' List.All.Compare(ASEDir)
List_All_Compare <- function(ASEDir, ASE.ID = NULL, name.sensors = "ALL", DIR_Models = "Models", All.Models = NULL, Save = FALSE, Verbose = FALSE,
                             DateIN = NULL, DateEND = NULL) {
    if (exists("All.Compare.ASEDirs")) rm(All.Compare.ASEDirs)
    for (ASEDir in ASEDir) {
        if (is.null(All.Models)) All.Models  <- List_models(ASEDir, name.sensors)
        if (is.null(ASE.ID)) ASE.ID <- Identify_ASE_Dir(ASEDir = ASEDir)
        All.Compare <- lapply(All.Models, function(i) {
            Compare_Models(file.path(ASEDir, DIR_Models, i), ASE.ID = ASE.ID,
                           General.DT = ASE.ID$General.DT, ASE.cfg = ASE.ID$ASE.cfg, SetTime = ASE.ID$SetTime, Verbose = Verbose,
                           Meas.DateIN = DateIN, Meas.DateEND = DateEND,
                           Config = ASE.ID$Config, Shield = ASE.ID$Config$sens2ref.shield)}) 
        All.Compare <- rbindlist(All.Compare[which(sapply(1:length(All.Compare), function(i) class(All.Compare[[i]])[1]=="data.table"))], use.names = T, fill = T)
        if (Save) fwrite(All.Compare,file.path(ASEDir, DIR_Models, "All_compare.rdata"))
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
    CovMod <- ifelse(is.null(covariates), "Temperature", covariates)
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
                is.not.NA.y <- which(complete.cases(ASE.ID$General.DT[, .SD, .SDcols = c(nameGasVolt,CovMod)]))
                is.NA.y     <- setdiff(1:nrow(ASE.ID$General.DT), is.not.NA.y)
                Matrice <- data.frame(ASE.ID$General.DT[is.not.NA.y, CovMod, with = FALSE],
                                      row.names = row.names(ASE.ID$General.DT[is.not.NA.y,]),
                                      stringsAsFactors = FALSE)
                names(Matrice) <- CovMod}
        } else if (ASE.ID$Mod_type %in% c("exp_kT_NoC")) {
            # take only the one that is nor NA of y = ASE.ID$General.DT[!is.na(ASE.ID$General.DT[, nameGasVolt]), nameGasVolt]
            is.not.NA.y <- which(complete.cases(ASE.ID$General.DT[, .SD, .SDcols = c(nameGasVolt, ASE.ID$Covariates)]))
            is.NA.y     <- setdiff(1:nrow(ASE.ID$General.DT), is.not.NA.y)
            Matrice <- data.frame(ASE.ID$General.DT[is.not.NA.y, Temperature],
                                  row.names = row.names(ASE.ID$General.DT[is.not.NA.y,]),
                                  stringsAsFactors = FALSE)
            names(Matrice) <- ASE.ID$Covariates
        }  else if (ASE.ID$Mod_type %in% c("exp_kT","exp_kK","T_power", "K_power")){
            # take only the one that is nor NA of y = ASE.ID$General.DT[!is.na(ASE.ID$General.DT[, nameGasVolt]), nameGasVolt]
            is.not.NA.y <- which(complete.cases(ASE.ID$General.DT[, .SD, .SDcols = c(nameGasVolt, "Temperature")]))
            is.NA.y     <- setdiff(1:nrow(ASE.ID$General.DT), is.not.NA.y)
            Matrice <- data.frame(ASE.ID$General.DT[is.not.NA.y, Temperature],
                                  row.names = row.names(ASE.ID$General.DT[is.not.NA.y,]),
                                  stringsAsFactors = FALSE)
            names(Matrice) <- "Temperature"
        } else if (ASE.ID$Mod_type %in% c("BeerLambert")) {
            # take only the one that is nor NA of y = ASE.ID$General.DT[!is.na(ASE.ID$General.DT[, nameGasVolt]), nameGasVolt]
            is.not.NA.y <- which(complete.cases(ASE.ID$General.DT[, .SD, .SDcols = c(nameGasVolt, "Temperature", "Atmospheric_pressure")]))
            is.NA.y     <- setdiff(1:nrow(ASE.ID$General.DT), is.not.NA.y)
            Matrice <- data.frame(ASE.ID$General.DT[is.not.NA.y, c("Temperature", "Atmospheric_pressure")],
                                  row.names = row.names(ASE.ID$General.DT[is.not.NA.y,]),
                                  stringsAsFactors = FALSE)
            names(Matrice) <- c("Temperature", "Atmospheric_pressure")
        } else if (ASE.ID$Mod_type %in% c("Kohler")) {
            # take only the one that is nor NA of y = ASE.ID$General.DT[!is.na(ASE.ID$General.DT[, nameGasVolt]), nameGasVolt]
            is.not.NA.y <- which(complete.cases(ASE.ID$General.DT[, .SD, .SDcols = c(nameGasVolt, "Relative_humidity")]))
            is.NA.y     <- setdiff(1:nrow(ASE.ID$General.DT), is.not.NA.y)
            Matrice <- data.frame(ASE.ID$General.DT[is.not.NA.y, c("Relative_humidity")],
                                  row.names = row.names(ASE.ID$General.DT[is.not.NA.y,]),
                                  stringsAsFactors = FALSE)
            names(Matrice) <- c("Relative_humidity")
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
                                                   Model      = Model.i,
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
#' @param Table.Coeffs A data.table with medians of intercept ? Sintercept and slope, x ? Sx,  one row per sensor of the ASE boxes. Output of Confidence_Coeffs().
#' @param Mod_type Charater vector, default is "Linear.Robust", the model type to be fitted, e.g. "Linear.Robust"
#' @param DIR_Models Character vector, subdirectory of ASEDir where are the calibration models.
#' @param All.Compare output of function List_All_Compare used to create the Median Model.
#' @param List.Models vector of strings, crresponding to the file paths of the modelds used as input of function List_All_Compare.
#' @param Interval numeric the windows size in days of the the rolling calibration models, default is 5 days
#' @return A data.table with medians of intersep ? Sintercept and slope, x ? Sx,  one row per sensor of the ASE boxes
#' @examples
#' Median_Model(ASEDir, name.sensors = NULL, Table.Coeffs) 
Median_Model <- function(ASEDir, ASE.ID = NULL, name.sensors = NULL, Table.Coeffs, Mod_type = "Linear.Robust", DIR_Models = "Models", All.Compare, List.Models, Interval = 5) {
    # Identify ASE base on ASEDir
    if (is.null(ASE.ID)) ASE.ID   <- Identify_ASE_Dir(ASEDir = ASEDir)
    # Determining names of sensors in ASE box if name.sensors is NULL
    if (is.null(name.sensors)) name.sensors <- intersect(ASE.ID$list.sensors,Table.Coeffs$name.sensor)
    # Fitting model for set of name.sensors
    for (sensor in name.sensors) {
        # create/save the model with median of coefficients
        ASE.ID       <- Identify_ASE_Dir(ASEDir = ASEDir, name.sensor = sensor, General.DT = ASE.ID$General.DT, 
                                         ASE.cfg = ASE.ID$ASE.cfg, SetTime = ASE.ID$SetTime, Config = ASE.ID$Config, Shield = ASE.ID$Config$sens2ref.shield)
        Cal.DateIN   <- min(All.Compare[name.sensor == sensor][["Cal.DateIN"]] , na.rm = T)
        Cal.DateEND  <- max(All.Compare[name.sensor == sensor][["Cal.DateEND"]], na.rm = T)
        futile.logger::flog.info(paste0("[Median_Model] ASE box ", ASE.ID$ASE.name,", sensor ", str_pad(sensor, width = max(str_length(ASE.ID$list.sensors),na.rm = T)),
                                        ", creating rolling ", Mod_type, " models between ", Cal.DateIN, "\nand ", Cal.DateEND," with median of coefficients of models."))
        # clearing namesCovariates (split for & and set degrees)
        namesCovariates <- unique(All.Compare[name.sensor == sensor][["Variables"]])[1]
        if (grepl("&", namesCovariates)) namesCovariates <- strsplit(namesCovariates, "&")[[1]]
        if (any(grepl("-", namesCovariates))) {
            degrees         <- sapply(namesCovariates, function(i) strsplit(i, "-")[[1]][2])
            namesCovariates <- sapply(namesCovariates, function(i) strsplit(i, "-")[[1]][1])
        } else {
            degrees         <- rep("1",length(namesCovariates))
        }
        #namesCovariates <- ifelse(is.na(namesCovariates), "", gsub(pattern = "-1", replacement = "", namesCovariates))
        #if (grepl("&", namesCovariates)) namesCovariates <- strsplit(split = "&",ifelse(is.na(namesCovariates), "", gsub(pattern = "-1", replacement = "", namesCovariates)))[[1]]
        # Initial Calibration model on the whole time interval
        New.General     <- Fit_New_Model(ASEDir = ASEDir, General.DT = ASE.ID$General.DT,name.sensor = sensor, Mod_type = Mod_type, 
                                         Cal.DateIN  = Cal.DateIN, Cal.DateEND = Cal.DateEND, 
                                         namesCovariates = namesCovariates, degrees = degrees,
                                         Plot_Line = FALSE, PlotCal = FALSE, Verbose = FALSE, Include.Model = TRUE, SAVE = FALSE)
        # Saving initial parameters coefficients for later correction of equation
        Old.Coeffs    <- New.General$Coef
        N             <- nrow(New.General$Augment)
        SSres         <- sum((New.General$Augment$.resid)^2)
        SSTot         <- sum((New.General$Augment$y - mean(New.General$Augment$y))^2)
        RMSE          <- sqrt(SSres/N)
        r.squared     <- 1 - SSres/SSTot
        adj.r.squared <- 1 - (SSres/(N - (length(New.General$Coef) - 1) - 1))/(SSTot/(N - 1))
        
        # Correcting Calibration model with rolling models
        Row           <- which(Table.Coeffs$name.sensor == sensor)
        Co_variates   <- names(All.Compare)[(which(names(All.Compare) == "Variables") + 1) : (which(names(All.Compare) == "R2raw") - 1)]
        for (j in Co_variates) {
            New.General$Tidy[New.General$Tidy$term == j, "estimate"]  <- Table.Coeffs[[j]][Row]
            New.General$Tidy[New.General$Tidy$term == j, "MAD"]       <- Table.Coeffs[[paste0("Mad",j)]][Row]
            New.General$Tidy[New.General$Tidy$term == j, "conf.low"]  <- Table.Coeffs[[j]][Row] - ifelse(!is.na(Table.Coeffs[[paste0("Mad",j)]][Row]), 1.4826 * Table.Coeffs[[paste0("Mad",j)]][Row],NA)
            New.General$Tidy[New.General$Tidy$term == j, "conf.high"] <- Table.Coeffs[[j]][Row] + ifelse(!is.na(Table.Coeffs[[paste0("Mad",j)]][Row]), 1.4826 * Table.Coeffs[[paste0("Mad",j)]][Row],NA)
            New.General$Tidy[New.General$Tidy$term == j, "std.error"] <- Table.Coeffs[[paste0("S",j)]][Row]
            New.General$Tidy[New.General$Tidy$term == j, "t value"]   <- New.General$Tidy[New.General$Tidy$term == j, "estimate"]/New.General$Tidy[New.General$Tidy$term == j, "std.error"]
            #https://stackoverflow.com/questions/46186115/calculating-p-values-for-given-t-value-in-r
            New.General$Tidy[New.General$Tidy$term == j, "Pr(>|t|)"]  <- 2*pt(q = abs(New.General$Tidy[["t value"]][New.General$Tidy$term == j]), df = nrow(All.Compare) - 2, lower = FALSE)
            New.General$Coef[which(names(New.General$Coef) == j)]     <- Table.Coeffs[[j]][Row]
        }
        New.General$Tidy$tau <- paste0("Median of coefficients of rolling calibration models with interval of ", Interval, " days")
        # changing coefficients of initModel with the median of coefficients
        New.General$InitModel$coefficients <- New.General$Coef
        #New.General$Augment$.fitted <- Table.Coeffs[["x"]][Row] * New.General$Augment$x + Table.Coeffs[["(Intercept)"]][Row]
        New.General$Augment <- ASE.ID$General.DT[date >= ASE.ID$Cal.DateIN & date < ASE.ID$Cal.DateEND + 1, list(date)]
        New.General$Augment[, x := ASE.ID$General.DT[date >= ASE.ID$Cal.DateIN & date < ASE.ID$Cal.DateEND + 1, ASE.ID$nameGasRef , with = F]]
        New.General$Augment[, y := ASE.ID$General.DT[date >= ASE.ID$Cal.DateIN & date < ASE.ID$Cal.DateEND + 1, ASE.ID$nameGasVolt, with = F]]
        # Adding covariates and selecting only complete cases
        if (all(namesCovariates != "")) {
            New.General$Augment[, (namesCovariates) := ASE.ID$General.DT[date >= ASE.ID$Cal.DateIN & date < ASE.ID$Cal.DateEND + 1, .SD, .SDcols = namesCovariates]]
            New.General$Augment <- New.General$Augment[complete.cases(New.General$Augment[,.SD,.SDcols= c("x","y",namesCovariates)])]
        } else New.General$Augment <- New.General$Augment[complete.cases(x,y)]
        New.General$Augment[,".fitted" := predict(New.General$InitModel, newdata = New.General$Augment)]
        New.General$Augment$.resid  <- New.General$Augment$y - New.General$Augment$.fitted
        New.General$Augment$.tau    <- paste0("Median of coefficients of rolling calibration models with interval of ", Interval, " days")
        if ("tau" %in% names(New.General$Glance)) New.General$Glance$tau <- "Median of coefficients of rolling calibration"
        SSres <- sum((New.General$Augment$.resid)^2)
        SSTot <- sum((New.General$Augment$y - mean(New.General$Augment$y))^2)
        New.General$Glance$r.squared     <- 1 - SSres/SSTot
        New.General$Glance$adj.r.squared <- 1 - (SSres/(N - (length(New.General$Coef)-1) - 1))/(SSTot/(N - 1))
        New.General$Glance$RMSE          <- sqrt(SSres/N)
        #https://www.oreilly.com/library/view/practical-statistics-for/9781491952955/ch04.html
        #https://stats.stackexchange.com/questions/87345/calculating-aic-by-hand-in-r
        k.original <- length(New.General$Coef)
        df.ll      <- k.original + 1
        New.General$Glance$AIC           <- ((df.ll) * 2) + N*(log(2 * pi) + 1 + log((SSres/N)))
        # https://stackoverflow.com/questions/35131450/calculating-bic-manually-for-lm-object
        # Corrected when weighing is used
        w <- rep(1,N)
        New.General$Glance$logLik        <- 0.5 * (sum(log(w)) - N * (log(2 * pi) + 1 - log(N) + log(sum(w * New.General$Augment$.resid^2)))) 
        New.General$Glance$BIC           <- -2 * New.General$Glance$logLik + log(N) * df.ll
        New.General$Glance$AIC           <- -2 * New.General$Glance$logLik + 2 * df.ll
        # Delete uncalculated statistics
        del.Glance <- c("df.residual", "deviance", "df", "p.value", "statistic", "sigma")
        for (j in del.Glance) if (j %in% names(New.General$Glance)) New.General$Glance[, j] <- NULL
        New.General$Call     <- paste0("Rolling ", Interval," day of ", paste(deparse(New.General$Call), collapse = ""))
        New.General$Equation <- paste0("Rolling ", Interval," day: ", paste(New.General$Equation, collapse = ""))
        # Correcting the coefficients in equation
        for (i in seq_along(Old.Coeffs)) New.General$Equation <- sub(pattern     = as.character(format(Old.Coeffs[i]      , digits = 4, scientific = T)), 
                                                                     replacement = as.character(format(New.General$Coef[i], digits = 4, scientific = T)), New.General$Equation, fixed = T)
        # Correcting AIC in equation
        New.AIC   <- paste0(",AIC=",format(New.General$Glance$AIC, digits = 6, scientific = T))
        if (grepl("AIC", New.General$Equation)) {
            Start.AIC <- str_locate(New.General$Equation, "AIC")[1]
            End.AIC   <- str_locate(substr(New.General$Equation, Start.AIC, nchar(New.General$Equation)), ",")[1]
            Text.AIC  <- substr(New.General$Equation, Start.AIC - 1, ifelse(is.na(End.AIC),nchar(New.General$Equation),End.AIC - 1 ))
            New.General$Equation <- sub(pattern     = Text.AIC, replacement = New.AIC, New.General$Equation, fixed = T)
        } else New.General$Equation <- paste0(New.General$Equation, New.AIC)
        # Correcting RMSE in equation
        if (grepl("RMSE", New.General$Equation)) {
            New.General$Equation <- sub(pattern     = as.character(format(RMSE                   , digits = 4, scientific = T)), 
                                        replacement = as.character(format(New.General$Glance$RMSE, digits = 4, scientific = T)), New.General$Equation, fixed = T)
        } else New.General$Equation <- paste0(New.General$Equation, ", RMSE = ",format(New.General$Glance$RMSE, digits = 4, scientific = T))
        # Adding R.square and adjusted R2 in equation
        if (grepl("R2", New.General$Equation)) {
            New.General$Equation <- sub(pattern     = as.character(format(r.squared                       , digits = 4, scientific = T)), 
                                        replacement = as.character(format(New.General$Glance$adj.r.squared, digits = 4, scientific = T)), New.General$Equation, fixed = T)
        } else New.General$Equation <- paste0(New.General$Equation, ", R2 = ",format(New.General$Glance$r.squared, digits = 4, scientific = T))
        if (grepl("adj.R2", New.General$Equation)) {
            New.General$Equation <- sub(pattern     = as.character(format(adj.r.squared                   , digits = 4, scientific = T)), 
                                        replacement = as.character(format(New.General$Glance$adj.r.squared, digits = 4, scientific = T)), New.General$Equation, fixed = T)
        } else New.General$Equation <- paste0(New.General$Equation, ", Adj.R2 = ",format(New.General$Glance$adj.r.squared, digits = 4, scientific = T))
        # Saving Median Model
        nameModel       <- sub(pattern = ".rdata", replacement = paste0("Median-", Interval,"__.rdata"), List.Models$List.Added.Models[1], fixed = T)
        nameModel       <- sub(pattern = format(Cal.DateIN + Interval,"%Y%m%d"), replacement = format(Cal.DateEND,"%Y%m%d"), nameModel)
        # discarding initModel that may be huge
        New.General <- New.General[which(names(New.General) != "InitModel")]
        list.save(New.General, file = nameModel)
        if (!exists("List.NewModels")) List.NewModels <- basename(nameModel) else List.NewModels <- c(List.NewModels,basename(nameModel))
    }
    return(list(List.NewModels = List.NewModels, New.General = New.General))
}
#' Determining significant covariates after applying a model
#'
#' @param Median.Models A vector of pathfiles of models to evaluate 
#' @param lhei numerical vector see heatmap.2, default value c(1.3,4.2) 
#' @param lwid numerical vector see heatmap.2, default value c(1.5,8)
#' @param Relationships a vector of character strings. Default is NULL. If not NULL these parameters are considered to be included in the 4 most significant covariates.
#' @param VIF logical, default is TRUE, If TRUE the variance inflator factors of dependent variables are computed and returned for MultiLinear models.
#' @param Relationships vector, default is NULL. It not NULL it shall give the covariates to be considered for selection in calibration model. 
#'        It shall not included: "SHT31TE_volt", "SHT31HE_volt", "BMP280_volt".
#' @param Thresh.R2 numeric, default is 0.00, difference between coefficient of determination of covariate/Residuals and covariates/Reference values to select covaristes
#' @param Add.Covariates logical default is FALSE, update Relationships if TRUE.
#' @return A list with character vectors of up to 4 significant covariates, plot a heatmap with dendogram to evidence clusters of covariates
#' @examples
#' #' List_Covariates(Median.Model.1, Relationships = Relationships, Add.Covariates = Add.Covariates)
List_Covariates <- function(Median.Models, lmat = rbind(c(0,0),c(2,1), c(3,4)), lhei = c(0.3,5,0.8), lwid = c(1.5,6), Relationships = NULL, Thresh.R2 = 0.00, Add.Covariates = FALSE) {
    for (Model in Median.Models) {
        # Identy ASE boxe from model name
        ASE.ID         <- Identify_ASE(Model)
        # Compute the correlation matrix for this model
        futile.logger::flog.info(paste0("[List_Covariates] ASE box ", ASE.ID$ASE.name,", sensor ", str_pad(ASE.ID$name.sensor, width = max(str_length(ASE.ID$list.sensors),na.rm = T)),
                                        ", matrix of correlation between ", ASE.ID$Meas.DateIN, " and ", ASE.ID$Meas.DateEND))
        # Fist apply the selected Model
        General.DT     <- Apply_Model(Model = Model, Variables = ASE.ID$Variables, General.DT = ASE.ID$General.DT, ASE.cfg = ASE.ID$ASE.cfg, Config = ASE.ID$Config, Shield = ASE.ID$Config$sens2ref.shield)
        # Create Residuals and drift
        data.table::set(General.DT, j = "Residuals", value = General.DT[[ASE.ID$nameGasMod]] - General.DT[[ASE.ID$nameGasRef]])
        data.table::set(General.DT, j = "DRIFT",     value = difftime(General.DT$date, ymd(ASE.ID$Cal.DateIN, tz = "UTC"), units = "days"))
        # Adding DRIFT", "Absolute_humidity", "Td_deficit", ASE.ID$var.names.meteo
        if (!exists("Relationships") || is.null(Relationships) || Add.Covariates) {
            namesCovariates <- names(General.DT[,grep(pattern = paste(c("_volt", "_modelled"), collapse = "|"), names(General.DT)), with = F])
            # Adding common names
            namesCovariates <- intersect(unique(c("DRIFT", "Absolute_humidity", "Td_deficit", ASE.ID$var.names.meteo, 
                                                  namesCovariates), fromLast  = T), names(General.DT))
        } else if (exists("Relationships")) namesCovariates <- Relationships
        # Discarding meteo volt names
        if (any(c("SHT31TE_volt", "SHT31HE_volt", "BMP280_volt") %in% namesCovariates)) namesCovariates <- namesCovariates[-grep(paste(c("SHT31TE_volt", "SHT31HE_volt", "BMP280_volt"), collapse = "|"), namesCovariates)]
        # Discarding the steps of outliers discarding if any
        Last2Char <- substr(namesCovariates, nchar(namesCovariates) - 2 + 1, nchar(namesCovariates))
        if (".1" %in% Last2Char) namesCovariates <- namesCovariates[-which(Last2Char %in% c('.1', ".2", ".3"))]
        if (length(namesCovariates) > 0) {
            # Adding ASE.ID$nameGasRef,"Residuals"
            namesCovariates <- unique(c(namesCovariates, ASE.ID$nameGasVolt, ASE.ID$nameGasMod, ASE.ID$nameGasRef,"Residuals"), fromLast  = T)
            # filter date and rows with NA, COmpute correlation matrix
            General.DT    <- ASE.ID$General.DT[date >= ASE.ID$Cal.DateIN & date <= ASE.ID$Cal.DateEND + 1, ..namesCovariates]
            Sum.NA        <- sapply(General.DT, function(i) sum(is.na(i)))
            Matrix        <- as.data.table(Hmisc::rcorr(data.matrix(General.DT))$r)
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
                if (length(namesCovariates) >= 6) { # Heatmap needs at least 2 covariates
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
                    title(main = basename(Model), outer = T, line = -1)}
                # Compute returning Matrix
                Co.Variates <- c(ASE.ID$nameGasVolt, ASE.ID$nameGasMod, ASE.ID$nameGasRef, "Residuals")
                return.Matrix <- Matrix[match(namesCovariates[5:length(namesCovariates)], names(Matrix)), Co.Variates, with = F]
                # Compute R^2
                return.Matrix[, (Co.Variates) := lapply(Co.Variates, function(i) return.Matrix[[i]]^2)]
                # Add row names giving covariates names
                return.Matrix[,row.names := namesCovariates[5:length(namesCovariates)]]
                # Add row names giving covariates names
                return(list(r.Matrix = Hmisc::rcorr(data.matrix(General.DT))$r, covariates.Matrix = return.Matrix[, c(5,1:4)]))
            } else {
                futile.logger::flog.trace("[List_Covariates] There are no significant covariates.\n")
                return(list(r.Matrix = NA, covariates.Matrix = NA))}
        } else {
            futile.logger::flog.INFO("[List_Covariates] You entered ony 1 covariates that cannot be considered.")
            return(list(r.Matrix = NA, covariates.Matrix = NA))
        }
    }
}

Formula.Degrees <- function(Mod_type = "Linear.Robust", ASE.ID, DateINPlot, DateENDPlot, Covariates, Multi.File = NULL) {
    if (Mod_type == "MultiLinear") {
        Matrice         <- General[date >= DateINPlot & date <= DateENDPlot + 1, .SD, .SDcols = Covariates]
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
    } else if (any(mod.eta.model.type %in% c("exp_kT_NoC", "exp_kT", "exp_kK", "T_power", "K_power"))) {
        if (is.null(Covariates) || length(Covariates) == "0" || Covariates == "") namesCovariates <- "Temperature"
        Matrice         <- General[date >= DateINPlot & date <= DateENDPlot + 1, .SD, .SDcols = Covariates]
        #names(Matrice)  <- namesCovariates
    } else if (any(mod.eta.model.type %in% c("BeerLambert"))) {
        Covariates <- c("Temperature", "Atmospheric_pressure")
        Degrees <-  c(1,-1)
        namesCovariates <- paste0(Covariates,collapse = "&")
        Matrice         <- General[date >= DateINPlot & date <= DateENDPlot + 1, .SD, .SDcols = Covariates]
    } else if (any(mod.eta.model.type %in% c("Kohler"))) {
        namesCovariates <- "Relative_humidity"
        Matrice         <- General[date >= DateINPlot & date <= DateENDPlot + 1, .SD, .SDcols = Covariates]
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
#' Automatic calibration of sensor
#'
#' @param ASEDir A character vector with the list of all filepaths of ASE boxes to be submitted to the function Median_Model
#' @param name.sensors Character vector, default is "CO_A4_P1", with the sensors name for which a claibration model is set. 
#' @param Interval numeric the windows size in days of the the rolling calibration models, default is 5 days
#' @param DateIN Date, default is NULL, A date for the begining of the rolling calibration intervals. If NULL DateIN is determined using Identify.ASE().
#' @param DateEN Date, default is NULL, A date for the end of the rolling calibration intervals. If NULL DateIN is determined using Identify.ASE().
#' @param Mod_type Charater vector, default is "Linear.Robust", the model type to be fitted, e.g. "Linear.Robust". If Add.Covariates is TRUE, Mod_type shall be set to Linear.Robust.
#' @return A list with all steps of calibration 
#' @examples
#' Fit.NO      <- Auto.Cal(ASEDir, name.sensor = "NO_B4_P1", Interval = 1, DateIN = as.Date("2020-01-21"), DateEND = NULL, DRIFT = FALSE, volt = TRUE, modelled = FALSE, 
#' Mod_type = "Linear.Robust", Relationships = c("Temperature_int"), degrees = "ExpGrowth", Add.Covariates = TRUE)
Auto.Cal <- function(ASEDir, ASE.ID = NULL, name.sensor = "CO_A4_P1", Interval = 5, DateIN = NULL, DateEND = NULL, 
                     DRIFT = FALSE, volt = FALSE, modelled = FALSE, Discarded.covariates = NULL,
                     del.Rolling = TRUE, Verbose = TRUE, 
                     VIF = TRUE, Treshold.VIF = 10, Conf.level = 0.10, Mod_type = "Linear.Robust", Relationships = NULL, degrees = "1", Add.Covariates = TRUE, DIR_Models = "Models") {
    # sending console to a file in the directory three (script log) and to variable Console for shiny TextOutput ####
    while (sink.number() > 0) {
        print(paste0("Number of sink channels opened: ", sink.number(), ". Closing opened channels"))
        sink(file = NULL)
    }
    sink(file.path(ASEDir, "Models", paste0("Auto_cal_",name.sensor, ".log")),
         type = c("output", "message"), split = TRUE, append = F ) # with split = TRUE we get the file on the screen and in log file
    # starting calibration model
    if (is.null(ASE.ID)) {
        futile.logger::flog.info("[Auto.Cal] identifying ASE box")
        List.models  <- List_models(ASEDir, name.sensor)
        if (!any(grepl(pattern = name.sensor, x = List.models))) {
            ASE.ID <- Identify_ASE_Dir(ASEDir, name.sensor, General.DT = NULL, ASE.cfg = NULL, SetTime = NULL, Config = NULL, Shield = NULL,
                                       DIR_Config = "Configuration", DIR_Models = "Models", DIR_General = "General_data")
        } else ASE.ID <- Identify_ASE(Model = file.path(ASEDir,"Models", List.models[grep(pattern = name.sensor, x = List.models)][1]))}
    if (is.null(DateIN))  DateIN  = ASE.ID$Meas.DateIN
    if (is.null(DateEND)) DateEND = ASE.ID$Meas.DateEND
    # Fitting all Mod_type models over the whole period set 1st Model to Linear.Robust if Add.Covariates is set to TRUE
    if (Add.Covariates) Mod_type <- "Linear.Robust"
    cat("#######################\n")
    futile.logger::flog.info(paste0("[Auto.Cal] ", basename(ASEDir),", sensor ",name.sensor, " fitting ", Mod_type, " models."))
    List.1 <- Roll_Fit_New_Model(ASEDir = ASEDir, ASE.ID = ASE.ID, Interval = Interval, name.sensors = ASE.ID$name.sensor, 
                                 DateIN = DateIN, DateEND = DateEND, Verbose = FALSE,
                                 Mod_type = Mod_type, namesCovariates = ifelse(Mod_type != "Linear.Robust",ifelse(!is.null(Relationships),Relationships[1],""),""), 
                                 degrees = ifelse(Mod_type != "Linear.Robust",ifelse(!is.null(degrees),degrees[1],"1")))
    # Comparing all models of selected boxes in a vector of filepaths (ASEDir), saving and returning the comparisons
    cat("-----------------------\n")
    futile.logger::flog.info("[Auto.Cal] Comparing Linear.Robust models:")
    All.Compare.1 <- List_All_Compare(ASEDir, ASE.ID = ASE.ID, name.sensors = ASE.ID$name.sensor, 
                                      DateIN = DateIN, DateEND = DateEND,
                                      All.Models = basename(List.1$List.Added.Models), Save = FALSE, Verbose = TRUE)
    # Confidence interval of coefficents of Models for Calibration models
    Table.Coeffs.1 <- Confidence_Coeffs(All.Compare = All.Compare.1, Mod_type = Mod_type)
    cat("-----------------------\n")
    futile.logger::flog.info("[Auto.Cal] Creating calibration Median Model with the median of coefficients of the rolling Linear.Robust models.")
    Median.Model.1 <- Median_Model(ASEDir, ASE.ID = ASE.ID, name.sensors = NULL, Table.Coeffs = Table.Coeffs.1, 
                                   Mod_type = Mod_type, All.Compare = All.Compare.1, List.Models = List.1, Interval = Interval)
    print(Median.Model.1$New.General$Tidy, quote = F)
    print(Median.Model.1$New.General$Glance, quote = F)
    cat("-----------------------\n")
    futile.logger::flog.info("[Auto.Cal] Looking for significant covariates that could be added to the Linear.Robust calibration model:")
    Co_variates.1  <- List_Covariates(file.path(ASE.ID$ASEDir,DIR_Models,Median.Model.1$List.NewModels), Relationships = Relationships, Add.Covariates = Add.Covariates)
    if (!is.na(Co_variates.1$covariates.Matrix) && nrow(Co_variates.1$covariates.Matrix) > 0) {
        futile.logger::flog.info(paste0("[Auto.Cal] the list of significant covariates that could be added to the Linear.Robust calibration model of ", name.sensor, " is:"))
        print(Co_variates.1$covariates.Matrix, quote = F)
    } else futile.logger::flog.info(paste0("[Auto.Cal] there are no covariates to add to the calibration mode for sensor ", name.sensor,"."))
    # Deleting rolling calibration models
    if (del.Rolling && exists("List.1")) {
        if ("Models.Already" %in% names(List.1)) {
            Del.Models <- setdiff(List.1$List.Added.Models, List.1$Models.Already)
        } else  Del.Models <- List.1$List.Added.Models
        if (length(Del.Models) > 0) unlink(Del.Models)}
    # Preparing returning object
    Returned.List <- list(List.1 = List.1, All.Compare.1 = All.Compare.1, Table.Coeffs.1 = Table.Coeffs.1, Median.Model.1 = Median.Model.1, Co_variates.1 = Co_variates.1, Final_median_Model = Median.Model.1)
    # fitting other co_variates if requested
    if (!is.na(Co_variates.1$covariates.Matrix) && Add.Covariates) {
        n.loop = 2
        # if (is.null(Relationships) || !any(Relationships %in% names(ASE.ID$General.DT))) First.covariate <- Co_variates.1$covariates.Matrix[["row.names"]] else First.covariate <- Relationships
        # discarding DRIFT and volt covariates if necessary
        repeat {
            cat("-----------------------\n")
            futile.logger::flog.info(paste0("[Auto.Cal] Looking for covariate ", n.loop - 1, " to be added to the calibration function."))
            # Put specified Relationships in first place
            if (is.null(Relationships) || length(Relationships) < (n.loop - 1) || !any(Relationships %in% names(ASE.ID$General.DT))) {
                if (exists("First.covariate")) First.covariate <- unique(c(First.covariate,get(paste0("Co_variates.",n.loop - 1))$covariates.Matrix[["row.names"]])) else First.covariate <- get(paste0("Co_variates.",n.loop - 1))$covariates.Matrix[["row.names"]]
            } else if (exists("First.covariate")) First.covariate <- c(First.covariate, Relationships[n.loop - 1]) else First.covariate <- Relationships[n.loop - 1]
            # discarding DRIFT, -modelled, volt covariates and Discarded.covariates if requested
            if (!DRIFT) {
                futile.logger::flog.info("[Auto.Cal] request to drop parameter \"DRIFT\" from significant covariates")
                First.covariate <-  First.covariate[grep(pattern = "DRIFT", First.covariate, invert = T)]} 
            if (length(First.covariate) >= n.loop - 1 && !volt) {
                futile.logger::flog.info("[Auto.Cal] request to drop parameters ending with \"_volt\" from significant covariates")
                First.covariate <-  First.covariate[grep(pattern = "_volt"    , First.covariate, invert = T)]}
            if (length(First.covariate) >= n.loop - 1 && !modelled) {
                futile.logger::flog.info("[Auto.Cal] request to drop parameters ending with \"_modelled\" from significant covariates")
                First.covariate <-  First.covariate[grep(pattern = "_modelled", First.covariate, invert = T)]} 
            if (length(First.covariate) >= n.loop - 1 && !is.null(Discarded.covariates)) {
                futile.logger::flog.info(paste0("[Auto.Cal] request to drop parameters ", paste(Discarded.covariates, collapse = ", ")," from significant covariates"))
                First.covariate <-  First.covariate[grep(pattern = paste(Discarded.covariates, collapse = "|"), First.covariate, invert = T)]} 
            # Dropping covariate with multicolinearity or unsignificant parameters of model
            if (exists("Dropped.covariates")) First.covariate <-  First.covariate[-grep(paste(Dropped.covariates, collapse = "|"), First.covariate)]
            if (length(First.covariate) >= n.loop - 1) {
                # Adding Variance Inflation Factors to check Multilinearity
                if ("DRIFT" %in% First.covariate && !"DRIFT" %in% names(ASE.ID$General.DT)) data.table::set(ASE.ID$General.DT, j = "DRIFT", value = difftime(ASE.ID$General.DT$date, ymd(DateIN, tz = "UTC"), units = "days"))
                if (VIF) {
                    for (i in First.covariate[(n.loop - 1):length(First.covariate)]) {
                        # https://stats.stackexchange.com/questions/112442/what-are-aliased-coefficients
                        if (length(degrees) < length(First.covariate[n.loop - 1])) {
                            Formula.degrees <- c(degrees, rep("1", length(First.covariate) - length(degrees)))} else Formula.degrees <- degrees
                            Formula <- as.formula(paste0(ASE.ID$nameGasVolt," ~ ",ASE.ID$nameGasRef, " + ", paste(First.covariate[1:(n.loop - 1)], collapse = " + ")))
                            nVIF <- HH::vif(lm(Formula, data = data.frame(ASE.ID$General.DT[date >= DateIN & date <= DateEND + 1]), x = TRUE), singular.ok = TRUE)
                            cat("-----------------------\n")
                            if (is.infinite(nVIF[length(nVIF)]) || nVIF[length(nVIF)] > Treshold.VIF) {
                                futile.logger::flog.warn(paste0("[Auto.Cal] Covariate ", i," has a Variance Inflation factor of ", nVIF[length(nVIF)], ", higher than threshold: ", Treshold.VIF, ",\n",
                                                                i, " does suffer from multicolinearity with other dependent variables. It cannot be included into the MultiLinear calibration model."))
                                First.covariate <- First.covariate[grep(i, First.covariate, invert = T)]
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
                                futile.logger::flog.info(paste0("[Auto.Cal] Covariate \"", i,"\" has a Variance Inflation factor of ", nVIF[length(nVIF)], ", lower than threshold: ", Treshold.VIF, ",\n",
                                                                "\"",i, "\" does not show multicolinearity with other dependent variables. It can be included into the MultiLinear calibration model."))
                                cat("-----------------------\n")
                                break}}} 
                if (length(First.covariate) >= (n.loop - 1)) {
                    First.covariate <- First.covariate[seq(1,(n.loop - 1))]
                    # Complete degrees with 1 for added covariates
                    if (length(degrees) < length(First.covariate)) degrees <- c(degrees, rep("1", length(First.covariate) - length(degrees)))
                    cat("-----------------------\n")
                    futile.logger::flog.info(paste0("[Auto.Cal] Fitting calibration model with ", n.loop - 1, " covariate(s): ", paste(First.covariate, collapse = ", ")))
                    assign(paste0("List.Covariate.", n.loop), Roll_Fit_New_Model(ASEDir = ASEDir, ASE.ID = ASE.ID, Interval = Interval, DateIN = DateIN, DateEND = DateEND, 
                                                                                 name.sensors = ASE.ID$name.sensor, Mod_type = "MultiLinear", 
                                                                                 namesCovariates = First.covariate, degrees = degrees, Verbose = FALSE))
                    Returned.List[[paste0("List.Covariate.", n.loop)]] <- get(paste0("List.Covariate.", n.loop))
                    # Comparing all models of selected boxes in a vector of filepaths (ASEDir), saving and returning the comparisons
                    cat("-----------------------\n")
                    futile.logger::flog.info("[Auto.Cal] Comparing rolling models.")
                    assign(paste0("All.Compare.", n.loop), List_All_Compare(ASEDir, ASE.ID = ASE.ID, name.sensors = ASE.ID$name.sensor,
                                                                            DateIN = DateIN, DateEND = DateEND,
                                                                            All.Models = basename(get(paste0("List.Covariate.", n.loop))$List.Added.Models), Verbose = TRUE))
                    Returned.List[[paste0("All.Compare.", n.loop)]] <- get(paste0("All.Compare.", n.loop))
                    # Confidence model with median of coefficents of Models
                    cat("-----------------------\n")
                    futile.logger::flog.info("[Auto.Cal] Creating Median Model with the median of coefficents of rolling models.")
                    assign(paste0("Table.Coeffs.", n.loop), Confidence_Coeffs(All.Compare = get(paste0("All.Compare.", n.loop)), Mod_type = "MultiLinear"))
                    Returned.List[[paste0("Table.Coeffs.", n.loop)]] <- get(paste0("Table.Coeffs.", n.loop))
                    cat("-----------------------\n")
                    assign(paste0("Median.Model.", n.loop), Median_Model(ASEDir, ASE.ID = ASE.ID, name.sensors = NULL, Table.Coeffs = get(paste0("Table.Coeffs.", n.loop)), 
                                                                         Mod_type = "MultiLinear", All.Compare = get(paste0("All.Compare.", n.loop)), List.Models = get(paste0("List.Covariate.", n.loop)), 
                                                                         Interval = Interval))
                    print(get(paste0("Median.Model.", n.loop))$New.General$Tidy, quote = F)
                    print(get(paste0("Median.Model.", n.loop))$New.General$Glance, quote = F)
                    Returned.List[[paste0("Median.Model.", n.loop)]] <- get(paste0("Median.Model.", n.loop))
                    # Checking improvement of model with AIC
                    current.AIC   <- get(paste0("Median.Model.", n.loop))$New.General$Glance$AIC
                    previous.AIC  <- Returned.List$Final_median_Model$New.General$Glance$AIC
                    if (current.AIC < previous.AIC) {
                        cat("-----------------------\n")
                        futile.logger::flog.info(paste0("[Auto.Cal] The Akaike Information Criterion (AIC) of the current model is ", current.AIC, ". It is lower than the AIC of the precedent model ", previous.AIC,"."))
                        futile.logger::flog.info(paste0("[Auto.Cal] Adding of covariate ", First.covariate[n.loop - 1]," improves the fit of the calibration model."))
                        cat("-----------------------\n")
                        futile.logger::flog.info("[Auto.Cal] Checking if any parameter of model is not significant")
                        #get(paste0("Median.Model.", n.loop))$New.General$Tidy$`Pr(>|t|)`[get(paste0("Median.Model.", n.loop))$New.General$Tidy$term == First.covariate[length(First.covariate)]]
                        if (all(get(paste0("Median.Model.", n.loop))$New.General$Tidy$`Pr(>|t|)` < Conf.level)) {
                            futile.logger::flog.info(paste0("[Auto.Cal] All parameters of the calibration model with covariate(s) ", First.covariate[1:(n.loop - 1)], " are significantly different from 0"))
                            assign(paste0("Co_variates.", n.loop), List_Covariates(file.path(ASE.ID$ASEDir,DIR_Models,get(paste0("Median.Model.", n.loop))$List.NewModels)))
                            Returned.List[[paste0("Co_variates.", n.loop)]] <- get(paste0("Co_variates.", n.loop))
                            if (!is.na(get(paste0("Co_variates.", n.loop))$covariates.Matrix) && nrow(get(paste0("Co_variates.", n.loop))$covariates.Matrix) > 0) {
                                futile.logger::flog.info(paste0("[Auto.Cal] the list of significant covariates that could be added to the Linear.Robust calibration model of ", name.sensor, " is:"))
                                print(get(paste0("Co_variates.", n.loop))$covariates.Matrix, quote = F)
                            } else futile.logger::flog.info(paste0("[Auto.Cal] there are no covariates to add to the calibration mode for sensor ", name.sensor,"."))
                            Next.covariate = FALSE
                            Returned.List[["Final_median_Model"]] <- get(paste0("Median.Model.", n.loop))
                        } else {
                            Invalid.Coefs <- get(paste0("Median.Model.", n.loop))$New.General$Tidy$term[which(get(paste0("Median.Model.", n.loop))$New.General$Tidy$`Pr(>|t|)` > Conf.level)]
                            futile.logger::flog.warn(paste0("[Auto.Cal] The coefficient of parameter(s) " , paste(Invalid.Coefs, collapse = " and ")," of the current model is(are) not significantly different from 0."))
                            futile.logger::flog.warn(paste0("[Auto.Cal] The calibration model resulting from adding covariate ", First.covariate[n.loop - 1]," is not valid."))
                            futile.logger::flog.warn("[Auto.Cal] Either parameters are unstable when rolling the calibration models or it/they does not influence significantly the sensor responses. Looking for other covariates.\n")
                            Next.covariate = TRUE}
                    } else {
                        futile.logger::flog.warn(paste0("[Auto.Cal] The Akaike information criterion (AIC) of the current model ", current.AIC, " is not lower that the AIC of the precedent model ", previous.AIC,"."))
                        futile.logger::flog.warn(paste0("[Auto.Cal] Adding of covariate ", First.covariate[n.loop - 1]," does not improve the fitting of calibration model. Looking for other covariates."))
                        Next.covariate = TRUE
                    } 
                } else {
                    futile.logger::flog.info("[Auto.Cal] There are no more covariates to be added to the calibration model.")
                    break} 
                if (del.Rolling && exists(paste0("List.Covariate.", n.loop))) {
                    if ("Models.Already" %in% names(get(paste0("List.Covariate.", n.loop)))) {
                        Del.Models <- setdiff(get(paste0("List.Covariate.", n.loop))$List.Added.Models, get(paste0("List.Covariate.", n.loop))$Models.Already)
                    } else  Del.Models <- get(paste0("List.Covariate.", n.loop))$List.Added.Models
                    if (length(Del.Models) > 0) unlink(Del.Models)
                }
            } else {
                futile.logger::flog.info("[Auto.Cal] There are no more covariates to be added to the calibration model.")
                break}
            # Dropping covariate with multicolinearity or unsignificant parameters of model
            if (Next.covariate == TRUE) {
                futile.logger::flog.warn(paste0("[Auto.Cal] ", First.covariate[n.loop - 1], " is discarded from the significant covariates because AIC is not improved using it or coefficients of model shows rolling variability."))
                if (exists("Dropped.covariates")) Dropped.covariates <- c(Dropped.covariates, First.covariate[n.loop - 1]) else Dropped.covariates <- First.covariate[n.loop - 1]
            } else {
                # add another covariate
                n.loop = n.loop + 1} 
        }
    } else futile.logger::flog.info("[Auto.Cal] It is not necessary or not requested to add other covariates to the calibration model.")
    sink()
    cat("-----------------------\n")
    if (exists("Returned.List")) return(Returned.List) else return()
}
#Bootstrap.Model <- function() {}

Register.Model <- function(Model, DIR_Config = "Configuration", Save = TRUE) {
    if (file.exists(Model)) {
        # Extract ASEDir - strip Model
        ASEDir   <- dirname(Model)
        Model    <- basename(Model)
        # Extracting information from Model
        # name of ASE box
        Stripped.Model <- unlist(strsplit(Model, split = "__"))
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
            ASE.cfg <- fread(ASE.cfg.file, showProgress = F)  
        } else return(futile.logger::flog.info("[Config.Model] model ",ASE.cfg.file," does not exist."))
        SetTime.file <- file.path(dirname(ASEDir), DIR_Config, paste0(ASE.name,"_SETTIME.cfg"))
        if (file.exists(SetTime.file)) {
            SetTime <- fread(SetTime.file, showProgress = F)
        } else return(futile.logger::flog.info("[Config.Model] model ",ASE.cfg.file," does not exist."))
        CovMod.file <- file.path(dirname(ASEDir), DIR_Config, paste0(ASE.name,"_CovMod_",name.sensor,".cfg"))
        if (file.exists(CovMod.file)) {
            CovMod <- fread(CovMod.file, showProgress = F)
        } else return(futile.logger::flog.info("[Config.Model] model ",CovMod.file," does not exist."))
        Covariates.file <- file.path(dirname(ASEDir), DIR_Config, paste0(ASE.name,"_Covariates_",name.sensor,".cfg"))
        if (file.exists(Covariates.file)) {
            Covariates <- fread(Covariates.file, showProgress = F)
        } else return(futile.logger::flog.info("[Config.Model] model ",Covariates.file," does not exist."))
        
        # Set ASE.cfg with Model information
        ASE.cfg.column <- which(ASE.cfg[name.gas == "name.sensor"] == name.sensor)
        Model.saved <- sub(pattern = paste0(name.sensor, "__"), replacement = "", sub(pattern = paste0(ASE.name, "__"), replacement = "",Model))
        set(ASE.cfg, i = which(ASE.cfg$name.gas == "Cal.func")     ,      j = ASE.cfg.column, value = Model.saved)
        set(ASE.cfg, i = which(ASE.cfg$name.gas == "Sens.raw.unit"),      j = ASE.cfg.column, value = Sens.raw.unit)
        set(ASE.cfg, i = which(ASE.cfg$name.gas == "mod.eta.model.type"), j = ASE.cfg.column, value = Mod_type)
        # Set SetTime with Model information
        SetTime.column <- which(SetTime[name.gas == "name.sensor"] == name.sensor)
        set(SetTime, i = which(SetTime$name.gas == "DateCal.IN"),      j = SetTime.column, value = format(Cal.DateIN, "%Y-%m-%d"))
        set(SetTime, i = which(SetTime$name.gas == "DatePlotCal.IN"),  j = SetTime.column, value = format(Cal.DateIN, "%Y-%m-%d"))
        set(SetTime, i = which(SetTime$name.gas == "DateCal.END"),     j = SetTime.column, value = format(Cal.DateEND, "%Y-%m-%d"))
        set(SetTime, i = which(SetTime$name.gas == "DatePlotCal.END"), j = SetTime.column, value = format(Cal.DateEND, "%Y-%m-%d"))
        # Discaring "&" and degrees from Variables
        if (Variables != "") {
            if (grepl("&", Variables)) Variables <- strsplit(Variables, "&")[[1]]
            if (any(grepl("-", Variables))) Variables <- as.character(sapply(Variables, function(i) strsplit(i, "-")[[1]][1]))
        } 
        # Set Covariates with Model information
        #set(Covariates, j = "Effects", value = unique(c(Covariates$Effects,Variables)))
        Covariates <- data.table(Effects = unique(c(Covariates$Effects,Variables)))
        # Set CovMod with Model information
        CovMod <- data.table(Effects = Variables)
        # Saving if requested
        if (Save) {
            fwrite(ASE.cfg   , ASE.cfg.file)
            fwrite(SetTime   , SetTime.file)
            fwrite(CovMod    , CovMod.file)
            fwrite(Covariates, Covariates.file)
        }
        return(list(ASE.cfg, SetTime, CovMod, Covariates))
    } else return(futile.logger::flog.error("[Config.Model] model ", Model," does not exist."))
}
AutoCal.Boxes.Sensor <- function(List.ASE, name.sensor = "CO_A4_P1",
                                 Interval = 1L, DateIN = as.Date("2020-01-19"), DateEND = as.Date("2020-01-31"), 
                                 Mod_type = "Linear.Robust", Relationships = NULL, degrees = NULL, Add.Covariates = TRUE, 
                                 VIF = TRUE, Treshold.VIF = 10, Conf.level = 0.10, DRIFT = TRUE, volt = TRUE, modelled = FALSE, Discarded.covariates = NULL,
                                 Register = TRUE, Verbolse = TRUE){
    Return.list <- list()
    for (i in List.ASE) {
        ASEDir   <- file.path(Dir, "ASE_Boxes", i)
        if (!dir.exists(ASEDir)) return(futile.logger::flog.warn("[AutoCal.Boxes.Sensor] The directory ", ASEDir, " doesnot exist. Check List.ASE."))
        # Looking for existin calibration model
        List.models <- List_models(ASEDir, name.sensor)
        # Identify box and sensor
        if (length(List.models) > 0) ASE.ID <- Identify_ASE(Model = file.path(ASEDir,"Models", List.models[1])) else ASE.ID <- Identify_ASE_Dir(ASEDir = ASEDir)
        # calibrate
        assign(paste0("Fit.",strsplit(name.sensor,"_")[[1]][1],"_", i), 
               Auto.Cal(ASEDir, ASE.ID = ASE.ID, name.sensor = name.sensor, Interval = Interval, DateIN = DateIN, DateEND = DateEND, 
                        Mod_type = Mod_type, Relationships = Relationships, degrees = degrees, Add.Covariates = Add.Covariates, 
                        VIF = VIF, Treshold.VIF = Treshold.VIF, Conf.level = Conf.level,
                        DRIFT = DRIFT, volt = volt, modelled = modelled, Discarded.covariates = Discarded.covariates))
        # Saving
        if (!is.null(get(paste0("Fit.",strsplit(name.sensor,"_")[[1]][1],"_", i)))) {
            Saved.List <- get(paste0("Fit.",strsplit(name.sensor,"_")[[1]][1],"_", i))
            save(Saved.List, file = file.path(ASEDir,"Models", paste0("Auto.Cal", name.sensor,".rdata")))
            rm(Saved.List)}
        if (exists(paste0("Fit.",strsplit(name.sensor,"_")[[1]][1],"_", i))) {
            Return.list[paste0("Fit.",strsplit(name.sensor,"_")[[1]][1],"_", i)] <- list(get(paste0("Fit.",strsplit(name.sensor,"_")[[1]][1],"_", i)))}
        # Updating ASE configuration with new Median Calibration Model
        if (Register) Local.CONFIG <- Register.Model(file.path(ASEDir,"Models",get(paste0("Fit.",strsplit(name.sensor,"_")[[1]][1],"_", i))$Final_median_Model$List.NewModels))
    }
    
    # return list of object
    if (exists("Return.list")) return(Return.list) else return()}

influx.getConfig <- function(boxName) {
    rootWorkingDirectory <- getwd()
    boxDirectory         <- file.path(rootWorkingDirectory, "ASE_Boxes", boxName)
    # Configuration and reading of data
    # Returning a list with 4 elements see below
    # Config[["Server"]]   : server parameters
    # Config[["sens2ref"]] : cfg parameters
    # Config[["CovPlot"]]  : covariates for plotting
    # Config[["CovMod"]]   : covariates for calibrating
    # Config[["sens2ref.shield"]] : configuration of checmical shield, Rload, gain ...
    local.CONFIG <- CONFIG(DisqueFieldtestDir = boxDirectory,
                           DisqueFieldtest = rootWorkingDirectory,
                           shiny           = FALSE)
    if (gsub(pattern = "ASE", replacement = "",local.CONFIG$Server$Dataset) != local.CONFIG$Server$AirSensEur.name) futile.logger::flog.warn("[influx.getConfig] The downloaded dataset has a different name than the ASE box. Please check _server.cfg file")
    return(local.CONFIG)
}

#' download of AirSensEUR raw data at Influx server and aplly calibration function
#'
#' @param boxName A character vector, the name of AirSenEUR box.
#' @param boxConfig A list as created using function influx.getConfig for boxName
#' @param subDirConfig File path of the subdirectory of boxName where is the file ASE.name.cfg. Default value: "Configuration".
#' @param subDirModels File path of the subdirectory of boxName where are the calibration models. Default value: "Models".
#' @param subDirData File path of the subdirectory of boxName where is the file General.csv. Default value: "General_data".
#' @param Add.Ref Logical, default is FALSE. If TRUE any RefData.csv esisting in directory subDirData will be included into General.file.
#' @return a list with two elements: 1 the data.table DT.General with calibrated sensors ending with "_modelled",
#'                                   2 the data.table SetTime created with function SETTIME for boxName
#' @examples
#' influx.downloadAndPredict(boxName = station, boxConfig = config)
influx.downloadAndPredict <- function(boxName, boxConfig, subDirData = "General_data", subDirModels = "Models", subDirConfig = "Configuration", Add.Ref = FALSE) {
    # remove Sos, Ref, isensors, list.gas.reference, list.gas.reference2use variables
    # resume inital directory at the end of the function
    # saving all RDS filtering list files and DT.General after each changes.
    # remove columns of Calib_data that are not useful
    # adding fullitle.logger messages (missing update of DateTime)
    rootWorkingDirectory <- getwd()
    boxDirectory         <- file.path(rootWorkingDirectory, "ASE_Boxes", boxName)
    
    futile.logger::flog.info("[influx.downloadAndPredict] Setting Initial values and file paths")
    DT.NULL    <- FALSE
    DT.General <- NULL
    Influx     <- NULL
    General.file            <- file.path(boxDirectory, subDirData, "General.csv")
    InfluxData.file         <- file.path(boxDirectory, subDirData, "InfluxData.csv")
    RefData.file            <- file.path(boxDirectory, subDirData, "RefData.csv")
    ind.warm.file           <- file.path(boxDirectory, subDirData, "ind_warm.RDS")
    ind.TRh.file            <- file.path(boxDirectory, subDirData, "ind_TRh.RDS"  )
    ind.Invalid.file        <- file.path(boxDirectory, subDirData, "ind_Invalid.RDS")
    ind.sens.out.file       <- file.path(boxDirectory, subDirData, "ind_sens_out.RDS")
    futile.logger::flog.info("[influx.downloadAndPredict] loading initial Influx data if any")
    if (file.exists(InfluxData.file)) {
        if (extension(InfluxData.file) == ".csv") {
            Influx <- fread(file = InfluxData.file, na.strings = c("","NA", "<NA>"))
            if (is.null(boxConfig$Server$Influx.TZ) || is.na(boxConfig$Server$Influx.TZ) || boxConfig$Server$Influx.TZ == "") {
                boxConfig$Server$Influx.TZ <- "UTC"
                futile.logger::flog.warn("[influx.downloadAndPredict] ref.tzone is not defined and set to \"UTC\"")}
            data.table::set(Influx, j = "date", value =  ymd_hms(Influx[["date"]], tz = boxConfig$Server$Influx.TZ))
        } else if (extension(InfluxData.file) == ".Rdata") {
            Influx <- load_obj(InfluxData.file)
            if (!"data.table" %in% class(Influx)) Influx <- data.table(Influx, key = "date")
        }
        if ("V1" %in% names(Influx)) Influx[, V1 := NULL]}
    if (Add.Ref && file.exists(RefData.file)) {
        if (extension(RefData.file) == ".csv") {
            Ref <- fread(file = RefData.file, na.strings = c("","NA", "<NA>"))
            if (is.null(boxConfig$Server$ref.tzone) || is.na(boxConfig$Server$ref.tzone) || boxConfig$Server$ref.tzone == "") {
                boxConfig$Server$ref.tzone <- "UTC"
                futile.logger::flog.warn("[influx.downloadAndPredict] ref.tzone is not defined and set to \"UTC\"")}
            data.table::set(Ref, j = "date", value =  ymd_hms(Ref[["date"]], tz = boxConfig$Server$ref.tzone))
        } else if (extension(SOSData.file()) == ".Rdata") {
            Ref <- load_obj(RefData.file)
            if (!"data.table" %in% class(Ref)) Ref <- data.table(Ref, key = "date")
        }
        if ("V1" %in% names(Ref)) Ref[, V1 := NULL]}
    if (file.exists(General.file)) {
        if (extension(General.file) == ".csv") {
            DT.General <- data.table::fread(General.file) #, na.strings = getOption("","NA")
            # Convert date to POSIXct
            if (!all("" %in% c(boxConfig$Server$Influx.TZ, boxConfig$Server$SOS.TZ))) {
                if (!"" %in% c(boxConfig$Server$Influx.TZ)) {
                    data.table::set(DT.General, j = "date"         , value =  ymd_hms(DT.General[["date"]]         , tz = boxConfig$Server$Influx.TZ))
                    data.table::set(DT.General, j = "date_PreDelay", value =  ymd_hms(DT.General[["date_PreDelay"]], tz = boxConfig$Server$Influx.TZ))
                } else {
                    data.table::set(DT.General, j = "date"         , value =  ymd_hms(DT.General[["date"]]         , tz = boxConfig$Server$SOS.TZ))
                    data.table::set(DT.General, j = "date_PreDelay", value =  ymd_hms(DT.General[["date_PreDelay"]], tz = boxConfig$Server$SOS.TZ))
                }
            } else {
                data.table::set(DT.General, j = "date"         , value =  ymd_hms(DT.General[["date"]]         , tz = "UTC"))
                data.table::set(DT.General, j = "date_PreDelay", value =  ymd_hms(DT.General[["date_PreDelay"]], tz = "UTC"))
            }
        } else if (extension(General.file) == ".Rdata") {
            DT.General <- load_obj(General.file)
            if (!"data.table" %in% class(DT.General)) DT.General <- data.table(DT.General, key = "date")
        }
        if ("V1" %in% names(DT.General)) DT.General[, V1 := NULL]
        # Checking that dew point deficit and absolute humidity are included for old download of AirSensEUR boxes
        # adding absolute humidity is relative humidity and temperature exist
        if (!all(c("Absolute_humidity", "Td_deficit") %in% names(DT.General))) {
            if (all(c("Temperature", "Relative_humidity") %in% names(DT.General))) {
                DT.General$Absolute_humidity <- NA_real_
                DT.General$Td_deficit        <- NA_real_
                both.Temp.Hum <- complete.cases(DT.General[, c("Temperature", "Relative_humidity")])
                DT.General[both.Temp.Hum, "Absolute_humidity"] <- threadr::absolute_humidity(DT.General[["Temperature"]][both.Temp.Hum], DT.General[["Relative_humidity"]][both.Temp.Hum])
                Td <- weathermetrics::humidity.to.dewpoint(rh = DT.General[["Relative_humidity"]][both.Temp.Hum], t = DT.General[["Temperature"]][both.Temp.Hum], temperature.metric = "celsius")
                DT.General[both.Temp.Hum, Td_deficit := DT.General[both.Temp.Hum, "Temperature"] - Td]
            }
        }
        
        # if some sensors of Influx are not included in DT.General, DT.General is re-created
        if (!all(names(Influx) %in% names(DT.General)) ) {
            DT.General <- NULL
            DT.NULL    <- TRUE}
    } else {
        DT.General <- NULL
        DT.NULL    <- TRUE}
    futile.logger::flog.info(paste0("[influx.downloadAndPredict] loading initial DT.General data if any, DT.NULL is set to ", DT.NULL))
    futile.logger::flog.info("[influx.downloadAndPredict] Initial DownloadSensor parameters")
    Download.Sensor <- Check_Download(Influx.name = boxConfig$Server$Dataset,
                                      WDinput     = file.path(boxDirectory, subDirData),
                                      UserMins    = if (!is.null(boxConfig$Server$UserMins)) boxConfig$Server$UserMins else boxConfig$Server$UserMins,
                                      General.df  = if (!is.null(DT.General))  DT.General else NA,
                                      RefData     = if (exists("Ref") && !is.null(Ref)) Ref else NULL,
                                      InfluxData  = if (!is.null(Influx)) Influx else NA,
                                      SOSData     = if (exists("Sos") && !is.null(Sos)) Sos else NULL)
    futile.logger::flog.info("[influx.downloadAndPredict] Initial SetTime")
    Set.Time <- SETTIME(DisqueFieldtestDir = boxDirectory,
                        General.t.Valid    = DT.General,
                        Influx.TZ          = boxConfig[["Server"]]$Influx.TZ,
                        SOS.TZ             = boxConfig[["Server"]]$SOS.TZ,
                        Ref.TZ             = boxConfig[["Server"]]$ref.tzone,
                        DownloadSensor     = Download.Sensor,
                        Config             = boxConfig,
                        sens2ref.shield    = boxConfig$sens2ref.shield,
                        shiny              = FALSE)
    Warm.Forced <- FALSE
    if (file.exists(ind.warm.file)) ind.warm.out <- list.load(ind.warm.file) else {
        ind.warm.out <- NULL
        Warm.Forced  <- TRUE
    }
    futile.logger::flog.info(paste0("[influx.downloadAndPredict] Indexes of data discarded during warming time of sensors, setting Warm.Forced to ", Warm.Forced))
    TRh.Forced <- FALSE
    if (file.exists(ind.TRh.file)) ind.TRh.out <- list.load(ind.TRh.file) else {
        ind.TRh.out <- NULL
        TRh.Forced  <- TRUE
    }
    futile.logger::flog.info(paste0("[influx.downloadAndPredict] Indexes of data discarded outside temperature and humidity tolerance, setting TRh.Forced to ", TRh.Forced))
    Inv.Forced <- FALSE
    if (file.exists(ind.Invalid.file)) ind.Invalid.out <- list.load(ind.Invalid.file) else {
        ind.Invalid.out <- NULL
        Inv.Forced      <- TRUE
    }
    futile.logger::flog.info(paste0("[influx.downloadAndPredict] Flagging the sensor data for Invalid sensor data, setting Inv.Forced to ", Inv.Forced))
    Outliers.Sens.Forced <- FALSE
    if (file.exists(ind.sens.out.file)) ind.sens.out <- list.load(ind.sens.out.file) else {
        ind.sens.out <- NULL
        Outliers.Sens.Forced <- TRUE
    }
    futile.logger::flog.info(paste0("[influx.downloadAndPredict] Indexes of outliers for sensor data, setting Outliers.Sens.Forced to ", Outliers.Sens.Forced))
    # Initialising for conversion and calibration
    futile.logger::flog.info("[influx.downloadAndPredict] Setting initial Conv.Forced and Cal.Forced to FALSE")
    Conv.Forced <- FALSE
    Cal.Forced  <- FALSE
    
    # Returning the indexes of valid sensors in boxName.cfg taking into account NAs
    list.gas.sensor  <- boxConfig[["sens2ref"]]$gas.sensor[!is.na(boxConfig[["sens2ref"]]$gas.sensor) &
                                                               boxConfig[["sens2ref"]]$gas.sensor  != "" &
                                                               boxConfig[["sens2ref"]]$name.sensor %in% boxConfig$sens2ref.shield$name.sensor]
    list.name.sensor <- boxConfig[["sens2ref"]]$name.sensor[!is.na(boxConfig[["sens2ref"]]$name.sensor) &
                                                                boxConfig[["sens2ref"]]$name.sensor != "" &
                                                                boxConfig[["sens2ref"]]$name.sensor %in% boxConfig$sens2ref.shield$name.sensor]
    list.name.gas    <- boxConfig[["sens2ref"]]$name.gas[!is.na(boxConfig[["sens2ref"]]$name.sensor) &
                                                             boxConfig[["sens2ref"]]$name.sensor != "" &
                                                             boxConfig[["sens2ref"]]$name.sensor %in% boxConfig$sens2ref.shield$name.sensor]
    
    # setting the current directory to the root of the file system with the name of the AirSensEUR
    wd <- getwd()
    setwd(boxDirectory)
    # InfluxDB ----
    # var.names.meteo     <- INFLUX[[2]]
    # var.name.GasSensors <- INFLUX[[3]]
    # var.names.sens      <- INFLUX[[4]]
    # InfluxData          <- INFLUX[[1]]
    futile.logger::flog.info(paste0("[influx.downloadAndPredict] Setting downloading of Influx data if needed is set to ", Download.Sensor$Retrieve.data.Influx))
    if (!is.null(Influx)) InfluxData <- Influx[] else InfluxData <- NA_real_
    if (Download.Sensor$Retrieve.data.Influx) {
        INFLUX <- INFLUXDB(
            WDoutput        = file.path(boxDirectory,subDirData),
            DownloadSensor  = Download.Sensor,
            UserMins        = boxConfig$Server$UserMins,
            PROXY           = boxConfig$Server$PROXY,
            URL             = boxConfig$Server$URL,
            PORT            = boxConfig$Server$PORT,
            LOGIN           = boxConfig$Server$LOGIN,
            PASSWORD        = boxConfig$Server$PASSWORD,
            Down.Influx     = TRUE,
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
            InfluxData      = InfluxData)
        Influx          <- INFLUX[[1]]
        rm(INFLUX, InfluxData)
        futile.logger::flog.info("[influx.downloadAndPredict] updating Download.Sensor with new Influx data")
        Download.Sensor <- Check_Download(Influx.name = boxConfig$Server$Dataset,
                                          WDinput     = file.path(boxDirectory, subDirData),
                                          UserMins    = if (!is.null(boxConfig$Server$UserMins)) boxConfig$Server$UserMins else 1,
                                          General.df  = if (!is.null(DT.General))  DT.General else NA,
                                          RefData     = if (exists("Ref") && !is.null(Ref))    Ref else NULL,
                                          InfluxData  = if (!is.null(Influx)) Influx else NA,
                                          SOSData     = if (exists("Sos") && !is.null(Sos))    Sos else NULL)
    }
    # Creating General Data
    # Checking that parameters for sensor download are complete or that they are new data
    futile.logger::flog.info(paste0("[influx.downloadAndPredict] Creating or updating of General Data set to ", 
                                    DT.NULL || isTRUE(Download.Sensor$DateEND.General.prev < Download.Sensor$DateEND.Influx.prev)))
    if (DT.NULL || isTRUE(Download.Sensor$DateEND.General.prev < Download.Sensor$DateEND.Influx.prev)) {
        
        # getting what it would be to put sensor and reference data together to later compare with what is in DT.General
        D <- GENERAL(WDoutput            = file.path(boxDirectory, subDirData),
                     UserMins            = boxConfig$Server$UserMins,
                     Delay               = boxConfig$Server$Delay,
                     RefData             = if (exists("Ref") && !is.null(Ref)) Ref else NULL,
                     InfluxData          = Influx,
                     SOSData             = if (exists("Sos") && !is.null(Sos)) Sos else NULL,
                     var.names.Pollusens = list.gas.sensor  ,
                     DownloadSensor      = Download.Sensor,
                     Change.Delay        = FALSE,
                     Change.UserMins     = FALSE)
        # saving New General data if needed
        save.General.df <- FALSE
        if (is.null(DT.General) || nrow(DT.General) == 0L ||
            (!is.null(DT.General) && !isTRUE(all.equal(D[,.SD, .SDcols = intersect(names(D),names(DT.General))],
                                                       DT.General[,.SD, .SDcols = intersect(names(D),names(DT.General))],
                                                       check.attributes = FALSE)))) {
            save.General.df <- TRUE
            DT.General      <- D
        }
        futile.logger::flog.info("[influx.downloadAndPredict] DT.General was updated. Warm.Forced is set to TRUE")
        if (save.General.df) {
            # Saving downloaded data in General_data Files
            # replacing nan by NA before saving
            Cols.for.Avg <- names(DT.General)[-which(names(DT.General) == "date")]
            data.table::set(DT.General, j = Cols.for.Avg, value = lapply(DT.General[,..Cols.for.Avg], nan.to.na))
            # saving New General data
            if (extension(General.file) == ".csv") {
                fwrite(DT.General, file = General.file, na = "NA")
            } else if (extension(General.file) == ".Rdata") save(DT.General, file = General.file)
            # if general is saved, it is necessary to run the detection of warming, T/RH out of tolerance, Negative Ref., Invalids and outlier detection, sensor data conversion and calibration.
            # It is sufficient to set to TRUE to run ind.warm then in.TRH ...
            Warm.Forced          <- TRUE
            futile.logger::flog.info("[influx.downloadAndPredict] updating Download.Sensor with new DT.General data")
            Download.Sensor <- Check_Download(Influx.name = boxConfig$Server$Dataset,
                                              WDinput     = file.path(boxDirectory, subDirData),
                                              UserMins    = if (!is.null(boxConfig$Server$UserMins)) boxConfig$Server$UserMins else 1,
                                              General.df  = if (!is.null(DT.General))  DT.General else NA,
                                              RefData     = if (exists("Ref") && !is.null(Ref)) Ref else NULL,
                                              InfluxData  = if (!is.null(Influx)) Influx else NA,
                                              SOSData     = if (exists("Sos") && !is.null(Sos)) Sos else NULL)
        }
        rm(D)
    }
    
    # Running filtering if needed
    # Flagging the sensor data for warming time
    # This dataTreatment can only works if boardTimeStamp exists, meaning only in InfluxData. It will not work with SOSData
    # output: a list of 4 character vectors, corresponding to sensors with row index of DT.General corresponding to warming time of sensor data,
    #       the names of the 4 elements are the ones of list.gas.sensor   in the same order
    if (Warm.Forced) {
        # Create a Progress object
        # setting index for warming
        if (!is.null(DT.General[,"boardTimeStamp"]) ) { # use to be class(DT.General) == "data.frame"
            # replace everythin boardTimeStamp which does not change with NA so na.locf will works
            # Index of boardtimeStamp similar for consecutive boardtimeStamp
            Consecutive <- which(diff(DT.General$boardTimeStamp, lag = 1) == 0)
            # Values of indexes whith previous values that are non consecutive (Re-start)
            Re_start <- Consecutive[diff(Consecutive, lag = 1) > 1]
            # Setting NA boardTimeStamp to the last non-NA boardTimeStamp
            data.table::set(DT.General,  j = "boardTimeStamp", value = na.locf(DT.General[["boardTimeStamp"]], na.rm = FALSE, fromLast = FALSE))
            # detecting when boardTimeStamp decreases suddenly (re-boot)
            Re_boot <- which(diff(DT.General$boardTimeStamp, lag = 1) < 0)
            # Combining Re_start and reboot
            ind <- unique(c(Re_start, Re_boot))
        } else {
            # This is for SOS
            ind <- apply(DT.General[, list.gas.sensor, with = FALSE  ], 1, function(i) !all(is.na(i)))
            ind <- which(ind[2:length(ind)] & !ind[1:(length(ind) - 1 )])
        }
        ind = ind + 1
        # Adding the first switch-on
        ind <- c(1,ind)
        for (n in seq_along(list.gas.sensor)) {
            indfull <- integer(length(ind)* boxConfig$sens2ref$hoursWarming[n] * 60 / as.integer(boxConfig$Server$UserMins))
            # developing IndFull
            for (i in seq_along(ind)) {
                indfull[((i - 1) *  boxConfig$sens2ref$hoursWarming[n]*60/as.integer(boxConfig$Server$UserMins) + 1):((i)* boxConfig$sens2ref$hoursWarming[n]*60 / as.integer(boxConfig$Server$UserMins))] <- ind[i]:(ind[i] +  boxConfig$sens2ref$hoursWarming[n] * 60 / as.integer(boxConfig$Server$UserMins) - 1)
            }
            # removing  indexes outside the number of rows of DT.General
            indfull <- indfull[indfull <= length(DT.General[[list.gas.sensor[n]]])]
            if (exists("return.ind.warm")) return.ind.warm[[n]] <- indfull else return.ind.warm <- list(indfull)
        }
        names(return.ind.warm) <- list.gas.sensor
        ind.warm.out <- return.ind.warm
        list.save(x = ind.warm.out, file = ind.warm.file)
        # Setting TRh.Forced to TRUE to be sure that it is done before ind.Sens
        TRh.Forced <- TRUE
        rm(return.ind.warm, ind)
        futile.logger::flog.info("[influx.downloadAndPredict] A new ind_warm.RDS was saved. TRh.Forced is set to TRUE")
    }
    
    # Flagging the sensor data for temperature and humidity outside interval of tolerance
    # Output:                : list of NAs for discarded temperature and humidity with as many elements as in list.gas.sensor
    #                          consisting of vector of integers of the index of rows of DT.General dataframe
    if (TRh.Forced) {
        # Always starting detection of outleirs for T and RH from the dataframe set in DT.General
        index.temp <- which(colnames(DT.General) %in% "Temperature")
        index.rh   <- which(colnames(DT.General) %in% "Relative_humidity")
        return.ind.TRh    <- list()
        return.ind.T.min  <- list()
        return.ind.T.max  <- list()
        return.ind.Rh.min <- list()
        return.ind.Rh.max <- list()
        for (l in list.gas.sensor) {
            # Global index of temperature/humidity exceeding thresholds
            ind <- (DT.General[, index.temp, with = FALSE]   < boxConfig$sens2ref$temp.thres.min[match(x = l, table = list.gas.sensor)]  |
                        DT.General[, index.temp, with = FALSE]   > boxConfig$sens2ref$temp.thres.max[match(x = l, table = list.gas.sensor)]) |
                (DT.General[, index.temp, with = FALSE]   < boxConfig$sens2ref$rh.thres.min[match(x = l, table = list.gas.sensor)]  |
                     DT.General[, index.temp, with = FALSE]   > boxConfig$sens2ref$rh.thres.max[match(x = l, table = list.gas.sensor)])
            # Global index of temperature/humidity exceeding thresholds
            T.min  <- DT.General[, index.temp, with = FALSE] < boxConfig$sens2ref$temp.thres.min[match(x = l, table = list.gas.sensor)]
            T.max  <- DT.General[, index.temp, with = FALSE] > boxConfig$sens2ref$temp.thres.max[match(x = l, table = list.gas.sensor)]
            Rh.min <- DT.General[, index.rh, with = FALSE]   < boxConfig$sens2ref$rh.thres.min[match(x = l, table = list.gas.sensor)]
            Rh.max <- DT.General[, index.rh, with = FALSE]   > boxConfig$sens2ref$rh.thres.max[match(x = l, table = list.gas.sensor)]
            # if (exists("return.ind.TRh")) {
            return.ind.TRh[[boxConfig[["sens2ref"]][which(gas.sensor == l), name.sensor]]] <- which(ind)
            return.ind.T.min[[ paste0(boxConfig[["sens2ref"]][which(gas.sensor == l), name.sensor],"__Temp. < ",boxConfig$sens2ref$temp.thres.min[match(x = l, table = list.gas.sensor)])]] <- which(T.min)
            return.ind.T.max[[ paste0(boxConfig[["sens2ref"]][which(gas.sensor == l), name.sensor],"__Temp. > ",boxConfig$sens2ref$temp.thres.max[match(x = l, table = list.gas.sensor)])]] <- which(T.max)
            return.ind.Rh.min[[paste0(boxConfig[["sens2ref"]][which(gas.sensor == l), name.sensor],"__RH < "   ,boxConfig$sens2ref$rh.thres.min[match(x = l, table = list.gas.sensor)])]] <- which(Rh.min)
            return.ind.Rh.max[[paste0(boxConfig[["sens2ref"]][which(gas.sensor == l), name.sensor],"__RH > "   ,boxConfig$sens2ref$rh.thres.max[match(x = l, table = list.gas.sensor)])]] <- which(Rh.max)
        }
        ind.TRh.out <- list(ind.TRh = return.ind.TRh, T.min = return.ind.T.min, T.max = return.ind.T.max, Rh.min = return.ind.Rh.min, Rh.max = return.ind.Rh.max)
        list.save(x = ind.TRh.out, file = ind.TRh.file)
        # Setting Invalid$Forced to True to be sure that it is carried out before ind.sens
        Inv.Forced <- TRUE
        rm(return.ind.TRh, return.ind.T.min, return.ind.T.max, return.ind.Rh.min, return.ind.Rh.max, T.min,T.max,Rh.min,Rh.max)
        futile.logger::flog.info("[influx.downloadAndPredict] A new ind_TRh.RDS was saved. Inv.Forced is set to TRUE")
    }
    
    # output: a list of n character vectors, corresponding to sensors with row index of DT.General corresponding to Invalid sensor data,
    #       the names of the n elements are the ones of list.gas.sensor   in the same order
    # min.General.date and max.General.date----
    if (!is.null(DT.General)) min.General.date <- min(DT.General$date, na.rm = TRUE) else min.General.date <- NULL
    if (!is.null(DT.General)) max.General.date <- max(DT.General$date, na.rm = TRUE) else max.General.date <- NULL
    if (Inv.Forced) {
        if (!is.null(DT.General)) {
            # reading the files with period of valid data
            for (i in seq_along(list.name.sensor)) {
                nameFile <- file.path(boxDirectory,subDirConfig,paste0(boxName,"_Valid_",list.name.sensor[i],".cfg"))
                if (file.exists(nameFile)) {
                    assign(paste0("Valid_",list.name.sensor[i]), read.table(file = nameFile, header = TRUE, row.names = NULL, comment.char = "#", stringsAsFactors = FALSE))
                } else {
                    # There are no Valid files. Creates files with IN = END = min(General$date)
                    assign(paste0("Valid_",list.name.sensor[i]), rbind(c(strftime(min(DT.General$date, na.rm = TRUE)), strftime(min(DT.General$date, na.rm = TRUE)))))
                    write.table(x         = data.frame(In = gsub(" UTC", "",strftime(min.General.date)),
                                                       End = gsub(" UTC", "",strftime(min.General.date)),
                                                       stringsAsFactors = FALSE),
                                file      = nameFile,
                                row.names = FALSE
                    )
                }
            }
            # Creating one list with invalid periods for all sensors
            Valid <- list()
            for (i in paste0("Valid_",list.name.sensor)) Valid[[i]] <- get(i)
            # Function to convert charater strings to POSIX
            NewValid <- function(x) {
                # making each element a dataframe of POSIXct
                x <- data.frame( x, stringsAsFactors = FALSE)
                colnames(x) <- c("In", "End")
                x$In  <- parse_date_time(x$In , tz = threadr::time_zone(DT.General$date[1]), orders = "YmdHMS")
                x$End <- parse_date_time(x$End, tz = threadr::time_zone(DT.General$date[1]), orders = "YmdHMS")
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
                    Valid.interval.j <- which(DT.General$date %within% lubridate::interval(Valid.date[[paste0("Valid_",i)]]$In[j], Valid.date[[paste0("Valid_",i)]]$End[j]))
                    if (length(Valid.interval.j) > 0) {
                        if (!(i %in% names(ind.Inval))) {
                            ind.Inval[[i]] <- DT.General$date[Valid.interval.j]
                        } else {
                            ind.Inval[[i]] <- c(ind.Inval[[i]], DT.General$date[Valid.interval.j])
                        }
                    }
                }
            }
        }
        
        ind.Invalid.out <- list(Valid.date,ind.Inval)
        list.save(x = ind.Invalid.out, file = ind.Invalid.file)
        # make sure that Outliers.Sens$Forced is run after Invalid, to discard outliers again and to apply invalid and outliers to DT.General
        Outliers.Sens.Forced <- TRUE
        rm(Valid.date,ind.Inval)
        rm(Valid)
        futile.logger::flog.info("[influx.downloadAndPredict] A new ind_Invalid.RDS was saved. Outliers.Sens.Forced is set to TRUE")
    }
    for (i in list.name.sensor) assign(paste0("Valid_",i),NULL)
    
    # discard outliers of sensors
    if (Outliers.Sens.Forced) {
        if (!is.null(DT.General)) setalloccol(DT.General)
        for (i in list.gas.sensor) {
            # Checking if sensor data exists in DT.General
            if (i %in% names(DT.General)) {
                # Initialisation of columns of DT.General
                Sensor.i <- na.omit(boxConfig[["sens2ref"]][[which(boxConfig[["sens2ref"]][,"gas.sensor"] == i),"name.sensor"]])
                # resetting to initial values
                Vector.columns <- paste0(c("Out.", "Out.Warm.", "Out.TRh.", "Out.Invalid.", "Out.Warm.TRh.", "Out.Warm.TRh.Inv."),i)
                set(DT.General, j = Vector.columns, value = rep(list(DT.General[[i]]), times = length(Vector.columns)))
                if (!is.null(ind.warm.out[i][[1]])) {
                    Vector.columns <- paste0(c("Out.", "Out.Warm.", "Out.Warm.TRh.", "Out.Warm.TRh.Inv."),i)
                    i.Rows <- as.integer(ind.warm.out[[i]])
                    set(DT.General, i = i.Rows, j = Vector.columns, value = rep(list(rep(NA, times = length(i.Rows))), times = length(Vector.columns)))
                }
                if (!is.null(ind.TRh.out$ind.TR[[Sensor.i]]) && length(ind.TRh.out$ind.TR[[Sensor.i]]) > 0) {
                    Vector.columns <- paste0(c("Out.", "Out.TRh.", "Out.Warm.TRh.", "Out.Warm.TRh.Inv."),i)
                    set(DT.General,i = ind.TRh.out$ind.TRh[Sensor.i][[1]], j = Vector.columns,
                        value = rep(list(rep(NA, times = length(ind.TRh.out$ind.TRh[Sensor.i][[1]]))), times = length(Vector.columns)))
                }
                if (!is.null(ind.Invalid.out[[2]][[Sensor.i]]) && length(ind.Invalid.out[[2]][[Sensor.i]]) > 0) {
                    Vector.columns <- paste0(c("Out.", "Out.Invalid." , "Out.Warm.TRh.Inv."),i)
                    set(DT.General,i = which(DT.General$date %in% ind.Invalid.out[[2]][[Sensor.i]]), j = Vector.columns,
                        value = rep(list(rep(NA, times = length(which(DT.General$date %in% ind.Invalid.out[[2]][[Sensor.i]])))), times = length(Vector.columns)))
                }
                # index (1, 2,3, 4  or 1,2,3, 6 ... comng from  selection of control uiFiltering, Calib and SetTime)
                k <- match(x = i, table = list.gas.sensor)
                set(DT.General, j = paste0("Out.",i,".",1:boxConfig$sens2ref$Sens.iterations[k]),
                    value = rep(list(DT.General[[paste0("Out.",i)]]), times = boxConfig$sens2ref$Sens.iterations[k]))
                # deleting bigger iterations
                j  <- boxConfig$sens2ref$Sens.iterations[k]
                repeat (
                    if (any(grepl(pattern = paste0("Out.",i,".", j + 1)      , x = names(DT.General)))) {
                        set(DT.General, j = paste0("Out.",i,".",j + 1), value = NULL)
                        j <- j + 1
                    } else break # leaving the Repeat if there are no higher iterations
                )
                if (boxConfig$sens2ref$Sens.Inval.Out[k]) {
                    for (j in 1:boxConfig$sens2ref$Sens.iterations[k]) { # number of iterations
                        if (all(is.na(DT.General[[i]]))) {
                        } else {
                            # Setting the columns of sensor data previous to detect outliers
                            Y <- DT.General[[paste0("Out.Warm.TRh.Inv.",i)]]
                            # setting Y for the outliers of previous iterations to NA. If null then stop outlier detection
                            if (j > 1) {
                                if (length(which(return.ind.sens.out[[paste0(i,".",(j - 1))]]$Outliers)) != 0) {
                                    if (class(Y)[1] == "tbl_df") {
                                        Y[as.numeric(paste(unlist(sapply(return.ind.sens.out[c(paste0(i,".",1:(j - 1)))],function(x) which(x$Outliers))))),] <- NA
                                    } else Y[as.numeric(paste(unlist(sapply(return.ind.sens.out[c(paste0(i,".",1:(j - 1)))],function(x) which(x$Outliers)))))] <- NA
                                } else break
                            }
                            Outli <- My.rm.Outliers(ymin         = boxConfig$sens2ref$Sens.Ymin[k],
                                                    ymax         = boxConfig$sens2ref$Sens.Ymax[k],
                                                    ThresholdMin = boxConfig$sens2ref$Sens.threshold[k],
                                                    date         = DT.General[["date"]],
                                                    y            = Y,
                                                    window       = boxConfig$sens2ref$Sens.window[k],
                                                    threshold    = boxConfig$sens2ref$Sens.threshold[k],
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
                        # Discading outliers
                        if (any(names(ind.sens.out) %in% paste0(i,".",j), na.rm = TRUE)) {
                            set(DT.General,i = which(return.ind.sens.out[[paste0(i,".",j)]]$Outliers), j = paste0("Out."      ,i),
                                value = list(rep(NA, times = length(which(return.ind.sens.out[[paste0(i,".",j)]]$Outliers)))))
                            set(DT.General,i = which(return.ind.sens.out[[paste0(i,".",j)]]$Outliers), j = paste0("Out.",i,".",j),
                                value = list(rep(NA, times = length(which(return.ind.sens.out[[paste0(i,".",j)]]$Outliers)))))
                        }
                    }
                }
            }
        }
        if (exists("return.ind.sens.out")) {
            ind.sens.out <- return.ind.sens.out
            list.save(x = ind.sens.out, file = ind.sens.out.file)
            # Force conversion of sensors
            Conv.Forced <- TRUE
            futile.logger::flog.info("[influx.downloadAndPredict] A new ind_sens_out.RDS was saved. Conv.Forced is set to TRUE")
        } 
        # reseting return.ind.sens.out
        if (exists("return.ind.sens.out")) {
            rm(return.ind.sens.out)
            rm(Outli)
        } 
    }
    # deleting unnecessary outlier replicates
    for (i in 1:length(list.gas.sensor)) for (j in 1:boxConfig$sens2ref$Sens.iterations[i]) assign(paste0(list.gas.sensor[i],".",j),NULL)
    
    # Conversion and calibration
    # list of possible model types
    Models <- c("Linear", "Linear.Robust","MultiLinear", "exp_kT_NoC", "exp_kT", "exp_kK", "T_power", "K_power", "RH_Hysteresis","gam", "Quadratic", "Cubic", "Michelis", "Sigmoid")
    Shield <- if (!is.null(boxConfig$Server$asc.File) & length(boxConfig$Server$asc.File) != 0) {
        ASEPanel04Read(ASEPanel04File = file.path(rootWorkingDirectory, "Shield_Files", boxConfig$Server$asc.File))
    } else return("ERROR, Config file of chemical shield not existing.\n")
    Calib_data <- data.frame(
        name.gas           = list.name.gas,
        name.sensor        = list.name.sensor,
        gas.sensor         = list.gas.sensor,
        Sens.raw.unit      = as.character(sapply(seq_along(list.gas.sensor), function(i) boxConfig$sens2ref$Sens.raw.unit[i])),
        stringsAsFactors = FALSE)
    
    if (Conv.Forced || Cal.Forced) {
        # Converting to nA or V
        if (Conv.Forced) {
            # Conversion to volts/A
            Sensors_Cal <- merge(x = Calib_data[c("name.gas","gas.sensor","name.sensor","Sens.raw.unit")], 
                                 y = Shield[,c("name.gas","name.sensor","gas.sensor","RefAD","Ref","board.zero.set","GAIN","Rload")],
                                 by = c("name.gas", "gas.sensor", "name.sensor"), all = TRUE)
            # Values converted in volt or nA of sensors in Shield only if sensor data exist
            data.table::set(DT.General,  j = paste0(Shield$name.sensor,"_volt"),
                            value = ASEDigi2Volt(Sensors_Cal = Sensors_Cal[Sensors_Cal$name.gas %in% Shield$name.gas,],
                                                 Digital = DT.General[,paste0("Out.",Shield$gas.sensor), with = FALSE]))
            # Values converted in volt or nA - Board zero in Volt? change to V or nA
            # # https://stackoverflow.com/questions/6819804/how-to-convert-a-matrix-to-a-list-of-column-vectors-in-r
            data.table::set(DT.General,  j = paste0(Shield$name.sensor,"_DV"),
                            value = lapply(Shield$name.sensor, function(i) rep(Shield$Ref[Shield$name.sensor == i] - Shield$RefAD[Shield$name.sensor == i],
                                                                               times = nrow(DT.General))))
            # No conversion for the sensors which are not in the Shield only if sensor data exist
            No.Shield.gas.Sensors <- setdiff(list.gas.sensor, Shield$gas.sensor)
            No.Shield.gas.Sensors <- No.Shield.gas.Sensors[which(c(paste0("Out.",No.Shield.gas.Sensors) %in% names(DT.General) ))]
            if (length(No.Shield.gas.Sensors) > 0) {
                No.Shield.name.Sensors <- setdiff(list.name.sensor, Shield$name.sensor)
                x <- DT.General[,paste0("Out.",No.Shield.gas.Sensors), with = FALSE]
                data.table::set(DT.General,  j =  paste0(No.Shield.name.Sensors,"_volt"),
                                value = lapply(seq_len(ncol(x)), function(i) x[,i]))
                rm(x)
            }
            Cal.Forced <- TRUE
            futile.logger::flog.info("[influx.downloadAndPredict] A new raw data were converted in DT.General. Cal.Forced is set to TRUE")
        }
        # Starting calibration
        if (Cal.Forced) {
            # Application of Calibration function to Complete data set
            if (!is.null(DT.General)) {
                # initial Calibration with values in input[[paste0("Cal",j)]])) provided that "Method of Prediction" is "Prediction with previous calibration"
                for (k in seq_along(list.name.sensor)) {
                    if (boxConfig$sens2ref$Cal.Line[k] == "Prediction with previous calibration") {
                        if (nchar(boxConfig$sens2ref$Cal.func[k]) > 0) {
                            # reading file
                            name.Model.i <- file.path(boxDirectory,subDirModels,
                                                      paste0(boxName,"__",list.name.sensor[k],"__",boxConfig$sens2ref$Cal.func[k]))
                            if (file.exists(name.Model.i)) {
                                Model.i <- load_obj(name.Model.i)
                                # sensor gas in volt or nA or Count
                                nameGasVolt <- paste0(list.name.sensor[k],"_volt")
                                # modelled sensor gas
                                nameGasMod  <- paste0(list.gas.sensor[k],"_modelled")
                                # Detecting the model type of the selected calibration model
                                Mod_type <- Models[grep(pattern = paste0("_",strsplit(name.Model.i, split = "__")[[1]][4],"_"),
                                                        x = paste0("_",Models,"_"))]
                                # Preparing the matrix of covariates
                                # Removing na for nameGasMod for nameGasVolt missing
                                is.not.NA.y <- which(!is.na(DT.General[[nameGasVolt]]))
                                is.NA.y     <- which( is.na(DT.General[[nameGasVolt]]))
                                if (Mod_type %in% c("MultiLinear", "exp_kT_NoC")) {
                                    CovMod  <- unlist(strsplit(x = unlist(strsplit(x = sub(pattern = paste(c(".rds",".rdata"), collapse = "|"),
                                                                                           replacement = "", x =  name.Model.i),
                                                                                   split = "__"))[7], split = "&", fixed = T))
                                    # Checking if there are "-" in the CovMod, deleting degrees of polynomial
                                    if (any(grepl(pattern = "-", x = CovMod[1]))) {
                                        Model.CovMod  <- unlist(strsplit(x = CovMod , split = "-"))
                                        CovMod  <- Model.CovMod[ seq(from = 1, to = length(Model.CovMod), by = 2) ]
                                        Degrees <- Model.CovMod[ seq(from = 2, to = length(Model.CovMod), by = 2) ]
                                    }
                                    # checking that all CovMod are included in DT.General
                                    if (!all(CovMod %in% names(DT.General))) {
                                        futile.logger::flog.error(paste0("[influx.downloadAndPredict] not all Covariates are available, something missing in ", CovMod,"."))
                                    } else {
                                        # take only the one that is nor NA of y = DT.General[!is.na(DT.General[, nameGasVolt]), nameGasVolt]
                                        is.not.NA.y <- which(complete.cases(DT.General[, .SD, .SDcols = c(nameGasVolt,CovMod)]))
                                        is.NA.y     <- setdiff(1:nrow(DT.General), is.not.NA.y)
                                        Matrice <- data.frame(DT.General[is.not.NA.y, CovMod, with = FALSE],
                                                              row.names = row.names(DT.General[is.not.NA.y,]),
                                                              stringsAsFactors = FALSE)
                                        names(Matrice) <- CovMod}
                                } else if (Mod_type %in% c("exp_kT","exp_kK","T_power", "K_power")) {
                                    # take only the one that is nor NA of y = DT.General[!is.na(DT.General[, nameGasVolt]), nameGasVolt]
                                    is.not.NA.y <- which(complete.cases(DT.General[, .SD, .SDcols = c(nameGasVolt, "Temperature")]))
                                    is.NA.y     <- setdiff(1:nrow(DT.General), is.not.NA.y)
                                    Matrice <- data.frame(DT.General[is.not.NA.y, Temperature],
                                                          row.names = row.names(DT.General[is.not.NA.y,]),
                                                          stringsAsFactors = FALSE)
                                    names(Matrice) <- "Temperature"
                                } else Matrice <- NULL
                                # Using the reverse calibration function (measuring function) to extrapolate calibration
                                if (Mod_type != "MultiLinear" || (Mod_type == "MultiLinear" && all(CovMod %in% names(DT.General)))) {
                                    data.table::set(DT.General, i = is.not.NA.y, j = nameGasMod,
                                                    value = list(Meas_Function(y          = DT.General[[nameGasVolt]][is.not.NA.y],
                                                                               Mod_type   = Mod_type ,
                                                                               covariates = CovMod,
                                                                               Degrees    = Degrees,
                                                                               Model      = Model.i,
                                                                               Matrice    = Matrice)))
                                    # Removing na for nameGasMod either nameGasVolt missing or CovMod missing
                                    data.table::set(DT.General, i = is.NA.y, j = nameGasMod, value = list(rep(NA, times = length(is.NA.y))))
                                    # setting negative values to NA
                                    if (boxConfig$sens2ref$remove.neg[k]) {
                                        data.table::set(DT.General, i = which(DT.General[, nameGasMod, with = FALSE] < 0), j = nameGasMod,
                                                        value = list(rep(NA, times = length(which(DT.General[, nameGasMod, with = FALSE] < 0)))))
                                    }
                                }
                            } else futile.logger::flog.warn(paste0("[influx.downloadAndPredict] there is no calibration file for sensors: ", list.name.sensor[k], ""))
                        } else futile.logger::flog.warn(paste0("[influx.downloadAndPredict] there is no calibration file for sensors: ", list.name.sensor[k], ""))
                    }
                }
                fwrite(DT.General, file = General.file, na = "NA")
                futile.logger::flog.info("[influx.downloadAndPredict] A new General.csv was saved data were converted in DT.General. Cal.Forced is set to TRUE")
                
            }
        } else futile.logger::flog.error("[influx.downloadAndPredict] THERE IS NO General.df or no sensor data converted to voltage or current.\n")
    }
    setwd(wd)
    return(list(data = DT.General, timeConfig = Set.Time))
}
