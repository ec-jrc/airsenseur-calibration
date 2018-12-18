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

#================================================================CR
# Version History ====
#================================================================CR
# New release v0.11
# 2018-11-11 : N66 - For all plots of MainTabPanel "PlotFiltering" (warming, Temperature and Humidity, NegValues, Invalid and Outliers) when the time span is lower or equal than the number of tick mark of X axis
#                    the time of day is added to the dates on the x axis labels.
# 2018-11-11 : N67 - Adapting the dimension of all plot windows to resolution 1920 x 1280 (see also N1)
# 2018-11-12 : N68 - NarBar Menu "Data Treatment". The first time that the tab is opened, the Merging of Influx, SOS and ref data is automatically launched. No need to click on button "Merge Inluc <- SOS <- Ref".
# 2018-11-13 : N69 - NarBar Menu "Data Treatment". Change of behaviour of checkBox "savePlot": it is necessary to check it everytime a plot must be saved. "Save Plot" also alows saving the ReportMarkDown (see below N76).
# 2018-11-13 : N70 - All scaterplots were modified so that the Tick marks of the X and Y axis used pretty values.
# 2018-11-13 : N71 - Navbar "Data Treatment", sideBar list box "List of covariates to plot" and "List of covariates to calibrate": the variables "date" and "sensor_modelled" were added. This allows observing the 
#                    drift of sensor values (raw and calibrated). It is then possible to use time and calibrated sensors in MultiLinear calibration. 
#                    Under MainTabPanel "Calibration", a new TabPanel called "Multivariates" allows setting the degree of polynomial of any covariates. Status of all Co_variates shall be "Enabled". 
#                    "Forced" will be used in future to set the coefficients of polynomial of Co-variates, it does not work yet, and should be set to "FALSE". WORK IN PROGRESS
#                    Plot.Covariates can use now data calibrated sensor data. It is also possible to calibrate sensor (e. g. O3) using values of calibrated sensors (e. g. NO2).
# 2018-11-25 : N73 - Navbar "Data Treatment", MainTabPanel "DataTable", the number of digits of all varaiables have been optimised in order to enhance reading
# 2018-11-25 : N74 - Navbar "Selected ASE", when using the button "Quit" to leave the shiny App, the RAM is purged from garbage, allowing to decrease RAM usage.
# 2018-11-25 : N76 - NavBar "dataTreatment", MainTabPanel "report Markdown". This is a 1st tentative of automatic reporting. Work in progress. It needs that the Polt of calibration and extrapolation, both scatterplots and
#                    timeSeries are saved using button "Save Plot". WORK IN PROGRESS
# 2018-11-25 : N77 - Dynamic allocation of sensors name in User Interface and config file. This will be helpful when CO2 and PM will be included into the App.
# 2018-12-02 : N78 - General improvement of reactivity and use of memory, allowing to manage larger datatests (e. g. minute values) with accepting time for data treatment except for the detection of outliers is still a long process.
#                    The datasets and config files are no more saved during data treatment allowing faster reaction. However, it is necessary to click of button "Save" to save data and config not to loose your work.
# 2018-12-03 :  N4 - NavBar "getData", new mainTabPanels to display of dataframes of downloaded inlfux, SOS and ref in navBar menu "getdata". It would be good to add the summary table shown in "Navbar "DataTeatment" - "Config".
# 2018-12-02 :  N1 - All plots use now the whole heigth of display and resize for any for plotting.
# 2018-12-03 :  N9 - The names of Model calibration are listed without the airsenseur name and sensor name in the list of Calibration model to ease reading
# 2018-12-06 : N79 - When the App is busy computing, a spinner is displayed to inform users about data treatment is going on
# 2018-12-10 : N80 - my.rm.outliers: the computing time for detecting outliers has been divided by two, computing the min and max of interval of tolerance within one rollapply
# 2018-12-11 : N81 - navbar "DataTreatment" - "PlotFltering", a new tab is added, called StatFiltering which gives the counts of all fileterd data (warming, temperature/humidity, Invalid, outliers, negative reference) for each sensor
# 2018-12-12 : Bug Correction: E2 - When the averaging time (GetData in time-shield tab) is decreased, it is necessary to repeat all downloading, merging and calculation (warming, T/RH, outiers, conv and cal)
#                                   because raw data with initial time average are no more available (input$UserMins)
#                                   In the NavBar menu "Get Data" of the SideBar Layout, thre is a new SelectInput "Averaging time in min for extrapolated data" that allow to change the averging time of the "Extrapolation mainTabPnale,
#                                   e. g. having all the calibration Datatremant with 1 minute averaging time and the extapolation with hourly values.
# 2018-11-02 : Bug correction: E19 - By mistake the detection of outliers is launched when the extent of slider input for dates (Set.Time())) is read: sliderInput of reference data are repaced with dateRange, easier to use
# 2018-11-09 : Bug Correction: reset button SavePlot after a plot is saved. It is necessary to check again "SavePlot" to save a new plot 
# 2018-11-13 : Bug Correction in SETTIME, the time zone of all dates were set to the time zone of RefData. This is changed to setting to the time zone of DownloadSensor$DateIN.General.prev if it exists then to 
#              DateIN.Influx.prev if it exists then to DateIN.SOS.prev  if it exists otherwise it is set to "UTC"
# 2018-11-13 : Bug Correction: when changing the "Delay in min, add minutes to sensor time" was changed General.df was recalculated and saved. However, the delay was not saved into the AsE-Name_Server.cfg file
#                              resulting into an error at the next Delay changes, see in reactive function General().
# 2018-11-16 : Bug Correction: NavBar "Data Treatmemt", sideBar button save, when clicking on save, General.df was saved without the outliers and calibrated sensor values.
# 2018-11-30 : BUG correction: up to now discarding sensor data for warming of sensor was carried out by detecting any AirSensEUR reset by checking that at each row, the previous boardTimeStamp was 
#                              smaller. However, When a few boardTimeStamps were missing (e. g. NA), the corresponding reset could not be detected. This was solved by setting NA value of boardTimeStamp to the last non-NA values
#                              for all missing bordTimeStamp
# 2018-12-10 : BUG Correction: E10 - Check that outliers of reference data (y> max values) are set to NAs, doubts on JRC_C5_ASE1 for O3. Ok, the code has been corrected to ensure that outliers are discarded at the end of each outliers
#                                    detection both for sensor and reference data.
# 2018-12-02 : Bug Correction: E14 - in MainTabPanel, TabPanel "DataTable", date-PreDelay (original date before Delay adjustement) does not show the month and day of each date.
#                                    DataTable" has been reformated and it is now possible to see the datePredelay correctly.
# 2018-12-11 : Bug Correction: E16 - Outliers filtering: when the number of iterations of detection decreases, the unused columns of dataframe General, ... called Out.gas.sensors.n 
#                                    should be deleted and the NAs in Out.gas.sensors should be replaced with original values
#                                    Now at each detection of outliers, unused column are deleted and initial
# 2018-12-05 : BUG Correction: E20 - Check that the median and MAD are computed using a data centered on the tested values, doubts for JRC_01 26-29/07/2015 (ex N23)
#                                    Parameter change in rollApply with with slidiing windows being centered on sliding row values. It seems it is all Ok.
# 2018-12-12 : Bug Correction:  E2 - When the averaging time (GetData in time-shield tab) is decreased, it is necessary to repeat all downloading, merging and calculation (warming, T/RH, outiers, conv and cal)
#                                    because raw data with initial time average are no more available (input$UserMins)
#                                    In the NavBar menu "Get Data" of the SideBar Layout, there is a new SelectInput "Averaging time in min for extrapolated data" that allow to change the averging time of the "Extrapolation mainTabPnale,
#                                    e. g. having all the calibration Datatremant with 1 minute averaging time and the extapolation with hourly values.
# 2018-12-15 : Bug Correction: E12 - when Delay is changed all data treatment (warming, T/RH, outliers) should be repeated? Outliers for sure because sensor and reference dates are changed.
#                                    Ok Warming has been enabled it will trigger, Temperature/humisidty, Invalid and sensor outliers detection. A save is carried out right after.
# 2018-12-15 : Bug Correction  E24 - when working with calibration and outliers, the size of DF$General starts increasing (up to 60 Gb on my PC). The App may crashes or become inresponsive. When this takes place millions of NA rows are added to DF$general. 
#                                    The error was in the the detection of NA and non.Na row when using MultiLinear mode. I solved it using the function complete.cases and SetDiff
#                                    is.not.NA.y <- which(complete.cases(DF$General[,c(nameGasVolt,CovMod)]))
#                                    is.NA.y     <- setdiff(1:nrow(DF$General), is.not.NA.y)
#                                    You can also use the NavBarMenu "Memory" to check if the size of DF$General increases. It should remain constant once calibration is carried out.
#              
# BUGS AND NEW FEATURES needed: ----#TO BE DONE  : ----
#ERRORS:       ----
#              E3 - The unit of y axis for the plot outliers of reference values is incorrect, it is not raw unit but ppb, ppm or ug/m3
#              E4 - It seems that the detection of directory from where the script is run detected using function Script_Dir() does not allways works, it should be made it transparent for user
#              E6 - General.conv(): x_DV Values are converted in volt or nA by substracting the zero.Board in Volt? This is an error if the conversion is carried out in nA. Change substraction to V or nA
#             E13 - All outlier window sizes changed to 19 data (3 hours correspond to 18 + 1 data of 10 min) for the detection of outliers, 
#             E15 - If the firmware of the sensor shield is changed during the use of an AirSensEUR box, the sensor data are wrongly converted to V or nA , e.g. ASE JRC-01 for NO23E50
#             E17 - update ui of uiFiltering and uiCalib when Config is changed (e. g. when a new AirSensEUR is selected)
#             E18 - input$hot used instead of input$table in saving valid file
#             E19 - input$Sens.rm.Invalid plotted and saved and some mistakes in sensor names in Plot.Invalid.Sens
#             E21 - BUG in create new, wrong Old_General_dir
#             E22 - Download of reference data (Down_Ref), if download is resumed, the downloaf of the last date of the dast download is repeated, check
#             E23 - Following a first data treatment with one ASE box When selecting a 2nd different ASE box in Navbar Menu SelectASE, there is generally a crash of the code.
#             E25 - Additionally, when changing the covariates in the UI that do not correspond to the list of covariates of the sensor_Multi file, it becomes impossible to calibrate. 
#             E26 - It seems that the detection of invalid data is not performed automatically when the file ind.Invalid.file does not exist or that it is performed after the detection of outliers 
#                   and hence not applied to DF$General. You can check on the mainTabPanel PlotFiltering - Invalid data appear.
#              
#NEW FEATURES needed: ----
#              N2 - Calibration with linear.robust: add RMSE on statterplot ...
#              N3 - Add model calibration: neural network model in the list of possible calibration method.
#              N5 - Add model resulting of laboratory experiments for calibration.
#              N6 - In NavBar menu "Help": add videos on how to use the shiny interface.
#              N7 - Do Filtering and conversion only for the selected sensor, not for all sensors.
#              N8 - add "Sensor" and "Reference" in front of covariates in the comboBox of SideBar "Calib"
#             N11 - Add evaluation tools: Sensor Evaluation Toolbox (SET) (Barak Fishbain).
#             N12 - For invalid data: allow to resume data to initial value if CheckBoxes "Enable Outlier discarding" set to FALSE. 
#             N13 - Detect nearest AQMS using GPS coodinates and and download with SOS
#             N15 - Add support for OPC-N2 and MOx sensor
#             N17 - In getData Time-shield add a 2nd time average to be applied after download, in order to avoid to modify the raw downloaded data if averaging time is changed
#             N19 - When downloading the SOS data make a query average to download less data as for InfluxQL
#             N20 - TabSet Calib, add unit for slope and intercept, 
#             N21 - Add automatic reporting, Markdown, knit (WORK IN PROGRESS)
#             N22 - Enter the width of rolling window for oulier detection in hours instead of numbers of data, e. g. 19 for a rolling window of 3 hours with 10 minutes average time
#             N27 - automatic order of rows of files "ASE_name"_valid_"sensor.name".cfg" based on the "in" dates
#             N28 - Add an observer to open the correct sideBar tabPanel according to the selected tabPanel in the mainPanel
#             N29 - There may be an error when adding dataFrame with subsequent download if a delay has been implemented before or if the Delay is modified between two downloads
#             N30 - add the log mainTabPanel in the GetData NavBar menu
#             N31 - Upload concentration levels after calibration to Client SOS and Influx (Grafana) servers
#             N51 - Every time a .png files for rawData, scatterPlots, time series, matrix, Unceratinty, drift and targetDiagram exists in Calibration, mModelled_gas, 
#                   General_Data should not create a new plot and rather uses the .png plot instead
#             N52 - Add the possibility to invalidate humidity transient
#             N53 - Create a button "Delete" of AirSensEUR in NavBar menu "SelectASE"
#             N54 - Finish Shiny App Manual
#             N82 - reduce the time for detection of outliers: go parallel computing with different version for linux and windows
# 

# New release v0.10
# 2018-09-06 : N63 - in the SideBarLayout, TabPanel "Calib.", every time a calbration model is selected the Raw units, Model for Calibration, list of covariates, Range of dates for calibration: and 
#                    Range of dates for plotting calibration are updated to the selected calibration model in order to be able to see the Calibration Scatterplot without changing these parameters.
# 2018-10-29 : N64 - In NavBar menu "Data Treatment", the "Calibrated" tab is added in MainTabPanel "calibration". It shows a scatterplot of calibrated sensors vs reference data. This is usedful if a multivariate 
#                    model is used for calibration because the mainTabPanle "ScatterPlot" only shows the sensor raw data vs reference. It allows to estimete the goodnes of fit (R2, slope, intercept and RMSE)
# 2018-10-31 : N65 - in NavbarMenu "DataTreatment", in MainTabPanel "Downloaded", the table of available data in airsenseur.db, InfluxData, SOSdata, referenceData and General is displayed before clicking on button 
#                    "Merge Influx <- SOS <- ref"
# 2018-09-06 : Bug Correction: in NavbarMenu "GetData", TabSetpanel "Ref Data", selected download "csv", the file Separator and Quote are not systematically intialized to avoid a crash of the App if missing.
#                              Seperator is now set to comma and Quote to double quote. Additionnally, instead of stopping the script in case "message box" with no proper date field, the code goes on without reference data. 
#                              (Check if this works).
#              Bug Correction: in SideBarlayout, TabPanel "Calib." for MultiLinear model (or other model with calibration), once a model with a list of covariates was already fitted, it was not more possible
#                              to set a model with a part of the List of covariates, an error message appeared with model existing. The bug is now solved, it is possible to set a new model even with covariates 
#                              that are already included into other models 
# 2018-10-30 : Bug correction: when CO reference unit was in ug/m3, the reactive function CalSet() was unable to calculate LV resulting in the app crashing. LV is now set in ug/m3 to 10000 (as 10 mg/m3)
# 2018-11-02 : Bug correction: Names of variables "TZ" and "sens.tzone" changed to "Influx.TZ" and "SOS.TZ" in config files ASE_name_Servers. Reading of old version names in config files is made consistent.
#                              In NavBarMenu "Get Data", SideBarLayout "Sensor Data", added time zone "Local time" for InfluxData to be able to detect tmeZone using coordinates and goofle_find functionnalities
#                              Mistake in function call  that had the arguments Influx.TZ = NULL forcing the timezone of influx data to be set using the x and y coordinates. The automatic detection of Influx data
#                              time zone is not only possible if "Influx.TZ" is null or "local time". The call to function INFLUXDB is modified in order to include Influx.TZ   = input$Influx.TZ.
#                              Mistake in Sql2df: string were converted to POSIXct with time zone UTC. It was added lubridate::ymd_hms(Values_db$time, tz = Influx.TZ).
#                              Removed from SQL2df: if (any(base::format(Values_db$time, format= "%Z") != "UTC")) attr(Values_db$time, "tzone") <- "UTC" that was imposing timeZone "UTC"

# 2018-08-17 : New release v0.9
# 2018-08-17 : N14 - upload of reference data using a local csv, dat or text file. No need to have all reference pollutants. reference pollutants names are recognized (e.g. CO, "co", Ref.CO_ppm")
#                    see naveBarMenu "Getdata", sidebar layout tabPanel "Reference" - "csv". Downloaded data are only printed when clicking on buttons "Download Influx Data" or "Download Reference Data" 
# 2018-08-17 : N62 - Two tabPanels: "Influx Sensor Data" and "reference Data" are added in the mainTab Panel of the NavBar Menu "Get data" to observe downloaded data for sensor and reference data.
# 2018-08-17 : N59 - Several modal information message boxes added (Connection to csv file and SOS server for downloading reference data)
# 2018-08-17 : Bug correction : error when the number of minute delay exceed 1000, e. g. UserMins = 1 min for data downaload corresponds to 2880 (from -1440 to 1440) minute delays.
#                               An error was issued when the number of delays was over 1000. Correction in all cases, the numer of minute delays is now limited to 1000, with 0 min being the central point.
#                               
# 2018-08-14 : New release v0.8
# 2018-08-09 : N14 - SOS download for reference data added. No need to have all reference pollutants.
# 2018-08-10 : N57 - New menu "Console Logs" in NavBar menu to see the last 1000 Lines of the console logs. When starting the last Console Logs is printed for debugging. 
#                    This slightly slow down the start of the App.
# 2018-08-13 : N58 - New TabPanel under NavBar Menu "getData" called "Sensor data" where the SOS and Influx sensor data download can be found.
# 2018-08-13 : N59 - Several modal information message boxes added (delete model, check presence of parameters for download of data)
# 2018-08-13 : Bug correction: in function Down_Influx, last row of downloaded influx data alwyas contain NA values when DAQ values are not yet available.
# 2018-08-14 : N60 - Navbar Menu "Data Treatment", mainTasbPanel "Downloaded", information is now reactive, no need to restart to see updated information on Onflux, SOS, reference 
#                    and General (Merged) data download
#                              These NA values were written in airsenseur.db without possibility to upload future values of when DAQ is carried out.
# 2018-08-13 : Bug correction: in funtion Down_Ref, DateIN and DateEND (begining end of Data Download) can be either class Date or POSIX, wich are incremented either in day or seconds, 
#                              creating some crashes somteimes. removed a few bugs when there is no new reference data to download.
# 2018-08-14 : Bug correction: in function GENERAL, it was not possible to add refrence data at dates when the General Dataframe already included sensor data and no reference data.
#                              Now every time that the date of reference data will exceed the last date of the dataframe General, the df General will be build from scratch 
#                              (correction in Function4ASE.R, function GENERAL)
#                              Although, the final data will not change if no sensor data are added, any change of the General dataframe will be now saved (use of identical() in App.R, function General)
# 
# 2018-08-03 : New release v 0.7
# 2018-04-04 : N11 - Add evaluation tools: uncertainty, orthogonal regression. 
#              N11 - The measurement uncertainty is estimated by fitting and orthogonal regression. The TabMainPanel is found under navBar menu "DataTreatment" - "Extrapolation" - "Uncertainty"). 
#                    The Method of the Guide for the demonstration of Equilavence of Measurement Methods (GDE) is used.
#                    The Random uncertainty of the reference method, u(xi) is set in sidebar tabPanel "Calib.".
# 2018-04-04 : N46 - Add summary of calibration model in NavBar "DataTreatmentt" mainTabPanel "Calibration" - "SummaryCal"
# 2018-04-04 : N47 - Add summary of extrapolation comparison model in NavBar "DataTreatmentt" mainTabPanel "Extrapolation" - "SummaryExtra"
# 2018-04-04 : N50 - in the sideBar, tabPanel "Refer." of navBar menu "GetData", the text input of "URL of the server with full name" is displayed with 6 rows and can be adjusted to show see several urls.
# 2018-04-07 : N48 - NavBar menu "SelectASE": The button "Create new AirSensEUR" is disable if a name of AirSensEUR is not entered in text input "New config file (ASEconfig*, * = SOS id)".
# 2018-04-07 : N49 - NavBar menu "GetData" - "Influx": The button "Download influx data" is disabled if the controlBox "Enable InfluxDB" is not enabled.
#                    NavBar menu "GetData" - "SOS": The button "Download SOS data" is disabled if the controlBox "Enable SOS" is not enabled.
#                    NavBar menu "GetData" - "Refer.": The button "Download Reference data" is disabled if the controlBox "Enable download Reference data" is not enabled.
#                    NavBar menu "Data Treatment" : The button "Merge influx <-SOS<-Ref" is disabled if the tabPanel "Calib" and SetTime" are not opened.
#                    NavBar menu "Data Treatment" : The button "Save" is disabled if the tabPanels "Calib" and "SetTime" are not opened and if the button "Merge" is not clicked
# 2018-04-07 :  N3 - Multilinear model calibration is added in navBar menu "DataTreatment" - "Calib". Select "Multilinear" in "Model for calibration" and add covariates in the appearing
#                    selectInput "List of covariates to calibrate". Do not add the sensor_volt which is selected by default in the list.
# 2018-04-12 :  E1 - sliderInput still oscillates if the slider is not realease before plots are updated. Add a delay?
#               E1 - Solved: sliderInput are replaced with dateRangeInput with Date picker calendars. The variables names for date ranges were also changed in navBar Menu "DataTreatment" , mainTabPanel "Config" - SteTimemain".
#               E1 - Buttons "CAL", "Cov" and "Ext" were added for the dataRangeInput "Ranges of dates for calibration". They allow the set the dates to the start/end dates of the calibration mode selected in
#                    Tabpanel "Calib", the dates selected in the dateRangeIput of covariates or Range of dates for plotting extrapolated data:, respectively.
#               E1 - Buttons "<" and ">" were added for all dateRangeInputs to allow to go backward of forward of 1 dateRange.
#               E1 - Buttons "<<" and ">>" were added for all dateRangeInput to select the first or last dates available.
#              E11 - in SideBar Layout, tab Panel Refer.: even if "Enable Download" data is not checked, clicking button Download data allow downloading reference data while it should not.
#              E11 - Solved: the button"Download reference data" is not enabled if the checkBox "Enable download of data" is not enabled.
#               E5 - It seems that when using a vector of FTP urls with several ftp sites for the download of reference data, only one url is saved in xxx_Server.cfg
#               E5 - Solved, saving several urls of reference data in the xxx_server.cfg file.
#               E8 - It is not possible to get the real min and max of $date in all sliderInput once the button "save config" is clicked, because the values saved in x_SetTime.cfg 
#               E8   file become the final min and max.
#               E8 - solved: sliderInputs replaced with dateRangeInputs.
#               E9 - Before clicking on button "Merge Influx, SOS - Ref), if the TabPaneld ""Calib" and "SetTime" are not opened, the script crashes.
#               E9 - solved: the "Merge" button is only enabled after opening the tabPanels "Calib" and "DataTreatment" 
#              N26 - Use the SliderInput Valid to limit the mi/max of sliderInputs for calibration, extrapolation, covariates, filtering ...
#              N26 - Done: the "Valid" sliderInput mow sets limit to the dateRangeInputs "Date", "DateCal", "DatePlotCal", "DateMeas" and "DatePlotMeas" 
#              N20 - inputSlider, add buttons to select time interval of 1, 2 days and 1, 2, 4 weeks, button to increase/ deacrease date by 1 time interval
#              N20 - soved with the dateRangeInput
#              N18 - Channel 4, there may be an inversion between calibration date and date for plotting calibration, check
#              N18 - Checked and fixed 
#              N24 - Update of min and max dates of sliderInput
#              N24 - Solved using the dateRangeInput and update using dates of "Valid" sliderInput
#              N16 - Number of steps in all sliderInput to be optimised, add button to set to the entire dates ranges at once
#              N16 - Solved : variables steps no more needed since dateRangeInput are replaced with dateRangeInputs. The buttons "<<" and ">>"  are added to use the full date ranges
#              N25 - The script should not re-do the outlier detection each time it starts, the outliers filtering has been already carried out
#              N25 - The flow of reactivity has been optmised. The detection of outliers is not performed if not requested (checkbox "apply outlier") or not already carried out
# 2018-04-13 : N42 - Added the detection of the directory of app.R works fine with the new function Script_Dir(). Script_Dir() is used into DisqueFiledtestDir() and DisqueFieldtest is 
#              N42 - replaced with dirname(DisqueFieldtestDir()) in the server() function.
# 2018-04-21 : N11 - Add evaluation tools: long-term drift and relative drift versus time and dose with trend line.
#              N52 - in NavBar "GetData" and "Data Treatment" the tabPanels of the sideBar layout are automatically opened, it is no more necessary to click on all tabPanels 
#                    (in "GetData": timeShield, Proxy, Influx, SOS and Refere. and in "Data Treatment": Filtering, Calib, SetTime)
# 2018-05-01   N54 - in NavBar menu "SelectASE", a new disabled testInput is added that shows the name of the selected AirSensEUR. The mainTabPanel still show the config date of the
#                    AirSensEUR selected in the upper list named "List of configured AirSensEUR". The textInput "Selected AirSensEUR" cannot be changed manually, it is necessary to click
#                    on button "Select AirSensEUR". This button also allows changing of AirSensEUR in a web session, but may causes crashes.
#              N11 - The calculation takes into accont non linearity of squares of residuals by fitting a generalised additive model. This is a modification of the method of estimation of the
#              N11 - Guide for the Demonstation of equivalence of nethod of measurements (GDE)
# 2018-06-28   N11 - Modified "Target Diagram" added under TabmainPanel   
# 2018-07-05   BUG corrected: the size of calibration model file has been considerably decreased as the R environment was saved in the files (>100 MB --> < 100 kB) in function Validation.Tool.
#                             When re-calibrating with MultiLinear model, identifying differences of Covariates Names in the calibration model name, in order to allow several sets of covariates
#                             for the multinlinear calibration of any sensor.
# 2018-07-05   BUG corrected: in the SetTime Tabpanel of the SideLayout, the "Ranges of dates for calibration" was always updated to the date of the selected calibration model of the "Calib" tabpanel
#                             even after a manual change (correction of ObserveEvent instead of ovserve) 
#              BUGS corrected: Several small bugs solved with the buttons "< and >" and ">> and <<" in the "SetTime" Tabpanel of the SideLayout and in the plot of "Range of dates for extrapolation:"
#                              and "Range of dates for plotting extrapolated data:" caused by confusion of "Range of dates for plotting covariates:"
# 2018-07-05   N10 - Add button Delete Model to update the list of calibration model 
#              N10 - Button "bin" added in "Calib" tab panel  in Sidebar Layout of the "Data Treatment" NavBar menu. This button delete the calibration model and related Calibration and extrapolation plots
# 2018-08-01   BUG Corrected: on the averaging of boarTimeStamp when more than one shield is used (AirSensEUR03). In the current situation, it is assumed that only one chemical shield is connected 
#                             on the channels 0, 1, 2 and 3 of the InfluxDB. The name of the sensors will be forced to the ones in the shield config file. boardTimeStamp is calculated for data of 
#                             the pollutants whose names are included in the shield config file (e. g. "carbon_monoside", "nitrogen_dioxide", "ozone" . without considering boardTimeStamp of other shields 
#                             (e. g. OPCs..). This is to avoid wrong timeBardStamp calculation considering boardTimeStamp of more than one shield at a time with effect on the warming time of sensors. 
#                             There is an exception when rows do not include valid data for pollutant names of chemical sensor shield. In this case an average of boardTimeStamp of all other shields is used. 
# 
#             
# 2018-03-24 : New release v 0.6
# 2018-02-16 : N31 - Adding new NavBar Menu "About" to give inofrmation on version history , errors, changes and functions needed to be added.
# 2018-02-17 : In the"Log" MaintabPanel, all lines of console are displayed without quote and line numbers, now the last 1000 lines are displayed.
#              N32 - In the "Filtering" SideBarLayout of navBar menu "DataTreatment", 3 checkboxes () are added to allow not to repeat the filtering data treatment and to avoid outlier detection, 
#              N32 - each time a digit is modified.
#              N33 - In the "Calib" SideBar, 2 checkboxes are added to force a digital to V/nA conversion and for the calibration from V/nA to ppb/ppm. These checkboxes are used to automate 
#              N33 - the script
#              N34 - A mainTabPanel Residual matrix is added under MainPanel Extrapolationin order to check the correlation of residuals on a claibration model is applied to a whole data series.
#              N38 - To increase speed of proccessing and reactivity of the tool, there has 3 important changes:
#              N38 -     1 - The outliers detection is carried out only if it is not done previously or if any filtering parameter (warming, temperature/humidity, invalid or outliers) has changed.
#              N38           It is no more carried out at each startup. 
#              N38 -     2 - Conversion from digital values into V or nA is carried out only if it is not done previously, if the unit (V or nA) is changed or if outlier deection is changed.
#              N38 -     3 - Extrapolation from V or nA is only carried out if if not done previously, if the conversion from digital to V or nA is changed or if a another calibration model 
#              N38 -         is selected 
# 2018-02-23 : E18 - Correction of mistake in matrix calculation of the conversion from digital to nA when more than 1 sensor is converted into nA  
# 2018-02-24 : N35 - An automatic update of min and max of the InputSliders for plotting outliers of sensor and reference data is added. This is usefule to select short data ranges.
# 2018-02-25 : N36 - A mainTabPanel "DataTable" is added in MainPanel. It shows a table of all values. The values are reactives to all new data tratment. The date dislayed can be change with 
#              N36 - the inputSlider of 1st sensor in the SideBar "Filtering" tab of the "DataTreatment" navBar menu.             
# 2018-02-27 : N37 - A NavBar menu "Help" is added to open the draft user manual of the Shiny interface in pdf. Be sure to set your browser in mode preview within your broser. 
#              E7  - When the range of plotted date is short, the data out of tolerance for temperature/humdity are not overlayed with sensor value, this is observed in the mainTabPanel "
#              E7  - PlotFiltering" - ""Temp&Humid".The same occurs in the mainTabPanel "Neg.values" under "PlotFiltering".
#              E7  - Bug corrected: there was a mistake in the GraphOut() function, the y vdata were selected using a numeric index instead of a Posix date.
#              E1  - The time series plots ("covariates", "calibration" and "extrapolation") may start oscilating when using the sliderInputs and changing them before the plots are updated.
#              E1  - I think this is caused by the automatic update of min and max of inputSliders. If this take place select another mainTabPanle to stop the oscilations
#              E1  - Bug corrected: the mistake was in functions min.DateRange() and max.DateRange() in file Functions4ASE.R, in which the date range was tested for +/- 25 % and set to +/- 50 % 
#              E1  -                after an update of sliderInputs
# 2018-03-03 : E19 - the list of avaialable calibration models does not appear correctly in SideBar tabPanel "Calib", selectInput "Select a previous calibration".
#              E19 - bug corrected: in function Plot.Calibration(): when setting the list of available calibration models, the test of the name of AirSensEUR is discarded in order to avoid 
#              E19 - confusion between Influx and SOS name. Additionnaly the separator of the elements of a calibration model (between unit, regression, dates ...) is changed from "_" to "__" 
#              E19 - because "_" can already used in the AirSensEUR names and create confusion when extratig the elements of the model name.
#              E20 - Date error when changing delay in minutes between snsors and reference data.
#              E20 - bug corrected: If the delay is changed, the General() function is run and it detects a change of last date that trigger a change of the General() data frame
#              E20 -                The next steps are also run: detection of warming, T/RH out of tolerance, invalids, outliers, sensor data conversion to V/nA and calibration. 
# 2018-03-04 : N38 - Delay of sensors vs reference data is now entered in seletInput as a multiple of the averaging time
#              Extrapolation (General.cal()) is now repeated when any of the checkboxes "Discard negative extrapolated data?" is changed. The checkboxes are in SideBar, TabPanel "Calib":
# 2018-03-05 : N12 - Use of non continuous validity date periods of sensor data. Invalid dates are read in file "ASE_name"_valid_"sensor.name".cfg".
#              N12 - Files are edited in mainTabPanel "PlotFiltering" - "Invalid" - "table": right click to add/delete rows and double click to edit. All changes must be saved 
#              N12 - using button save. Changes are applied by setting checkboxes "Apply validity periods" to TRUE in the "Filtering" tabPanel of the SideBar. 
#              N12 - The file \General_data\"ASE_name"_valid_"sensor.name".cfg" have headers "In", "End" and "Comments" for each time period.
#              N12 - Dates, between quotes, are separed by empty spaces, format "%Y-%m-%d %H:%M:%S". 
#              N12 - Invalid data are plotted in the Main Panel, tabPanel "PlotFiletring" - "Invalid" - "PLot". 
# 2018-03-17 ; N39 - the units of reference measurements in the sideBar, tabPanel "Filtering" are now selected from a list(ppb, ppm ...)
# 2018-03-18 : N40 -  mainTabPanel "Downloaded" added. It shows the  downloaded data in airsenseur.db, InfluxData, SOSData, Reference data and General data - all combined data).
#              N41 - Shiny function ASE.names.Influx changed, replacing "SELECT * FROM /.*/ limit 1" with SHOW SERIES to speed up the identification of datasets in the influx cloud database
#              N41 - Reorder sequence of iltering actions : WARMING -> TRh -> Negative referencce -> Invalid -> Outliers
# 2018-03-18 : Function to look for the directory of 151016 Sensor_Toolbox.R was removed since the file is in the same directory of app.R
#              N42 - Improvement of the detection of the current directory of app.R, adding setwd(".") and sys.calls()[[1]] [[2]] after 
#              N42 - kimisc::This_file() that seems not to work. [-which(ls() %in% c("DisqueFieldtest"))] delected from first command 
#              N43 - Initial line is modified: remove(list=ls()[-which(ls() %in% c("DisqueFieldtest"))]) for portability.
# 2018-03-24 : E21 - When creating a new AirSensEUR in SelectASE, the file xxxx_SETTIME.cfg maybe copied with dates outside downloaded data range. 
#              E21 - Bug solved: several functions (General(), observer(Set,Time())), observed(sliderINouts) were changed to set a correct date of the sliderInputs of sideBar
#              E21 -             tabPanel in navBar DataTreatment. It is necessary to save the confiration to save the file xxxx_SETTIME.cfg.
#              N44 - Icons added on buttons and MainTabPanels. Theme changed to Cerulean
#              N45 - It is possible to change of AirSensEUR without closing the Shiny App. When you want to change set the mainTabPanel of NavBar "SelectASE" to "Push data",
#              N45 - the SideBar TabPanel of the "GetData" NavBar to "Time-shield", and the mainTabPanel of navBar "DataTreatment" to "Config" - "Downloaded". 
#              N45 - Go to NavBar "SelectASE", in the Select input "Existing config files" select an AirSensEUR and 
#              N45 - do not click on button "Select AirSensEUR". Finally, Goto NavBar menu "GetData" and open all tabPanel of the SideBar,
#              N45 - go to navBar "DataTreatment" and open first all tabPanels of the SideBar and then all the mainTabPanels
# 2018-03-27 : N42 - Added function env_doc() for the detection of the directory of app.R in function Script_Dir(). Script_Dir() is only to detect DisqueFieldtest and DirShiny.
#              N42 - dirname(DisqueFieldtestDir()) in the server() function is replaced by DirShiny a static variable.
#              
# 2018-02-13 : New release v 0.5
# 2018-02-09 : Error in Down_Influx when selecting the variable names of the JSON downloded from Influx, wrong names were selected and the sscript crashed
#              Error of variable names when fitting the comparison of calibrated (extrapolated) variables names in "Extrapolation|Matrix"? and "Calibration|Matrix"
# 2018-02-12 : Plot of values outside temperature and humidity interval of tolerance shows         
# 2018-02-13 : The min and max of range of date for inputsliders of SideBarLayout "calibration", "extrapolation" and "plotting" is changed dynamically to be able 
#              to extend the dates ranges for short and long date ranges
# 
# 2018-02-06 : New release v 0.4 
# 2018-01-09 : Adding progress bars for a few plots (Warming, T-RH, Neg values ...)
# 2018-01-10 : Solving error when opening TabPanels, CaliBMain and SetTimeMain if TabSets Calib and SetTime were not opened before
#              Automatic setting of TabPanel "Filtering", "Calib" and "SetTime" when using MainPanel menu "Filtering", "Covariates" and "Extrapolation", NavBar menu "DataTreatment"
#              Added possibility to convert digital values in Volts or nA and to calibrate or extrapolate with the 2 units, TabPanel "Calib" of SideBarLayout in NavBar menu "DataTreatment"
#              Units added in calibration plot
#              Radiobuttons for selecting sensor names added (dynamic names from asc.file) for "Filtering", "Calib" and "SetTime" TabPanel of SideBarLayout for NavBar menu "Data Treatment"
# 2018-01    : Download of reference data (Down_Ref): it is now possible to use a vector of FTP urls in case of several ftp sites for the reference data, without ute, comma separated
# 2018-01-29 : The Averaging time in min (userMins) is now selected by a combo box and is converted to numeric before use. Possible values: 1,2,3,4,5,6,10,12,15,30,40,45,60 mins
# 2018-01-31 : The download of Influx data is now much faster because the Influx query asks for averaged data according to input$UserMins. The data are no more averaged in R
# 2018-02-02 : Labels of y axis and strip names are discarded from all timeplots. Adjust width of legend Still need to adjust Heigth
# 2018-02-04 : create and apply functions to detect date format, min amd max for sliderInput with dates
# 2018-02-06 : Units in time seires timePlots are added and legend is plotted after discarding suffixes "Out." and "modelled"
#              Date range for sliderInput optimised
#              Date range format for sliderInput optimised
#              Automatic saving of plots is now carried only out when check box "save  Plot" in NavBar "Data Treatment" menu is selected. The script is thus more reactive
#              added: general additive model for calibration ("gam") in SideBarLayout TabPanel "Calib"
#              added: one sliderInput ("DatePlotMeas") for plotting extrapolated data in SideBarLayout TabPanel "SetTime"

# 2018-12-14 : New release v 0.3
# 2017-12    : Sqlite2df setting sensor names from the shield config file, no need to remember or add them nanually
#              Accepting Influx data without sensor names in Down_Influx and Sqlite2df
#              
# 2018-11-28 : New release v 0.2

# 2018-11-21 : Release v 0.1
# 2017-05-05 : shiny App adapted from the ASE-OPER_SCRIPT
# 2017-06-01 : Select directories/file using choose.dir choose.files window
# 2017-06-07 : Adding support of tk_choose.files and tk_choose.dir to run the code under Linux as well. Ok it works
# 2017-06-10 : Adding check of internet connection in ASE_Script, Down_SOS and DOwn_Influx. Functions4ASE.R and ASEConfig_xx.R are selectied with choose.file windows Windows.
#              Sensor_Toolbox.R is selected automatically
# 2017-06-14 : Solving a error in Down_ref and Sqlite2 , of the addition of the last day of reference data and data of InfluxData when there is no new data 
#              Adding plots of Invalid data (warming, interval of temperature and humidity ...). Adding title to all plots.

#================================================================CR
# Content ====
#================================================================CR
#  0 - Clear memory and restart R-session
#  1 - Get configuration parameters in ASEconfig_MG.R - Create file system structure check for General.Rdata availbility, 
#      create log file
#  2 - Retrieve data if needed
#  3 - Linear Calibration

#================================================================CR
# 0 START ====
#================================================================CR

cat("-----------------------------------------------------------------------------------\n")

# Clear memory and restart R-session
remove(list=ls()) # [-which(ls() %in% c("DisqueFieldtest"))])

# checking if internet is available to access CRAN
havingIP <- function() {
    if (.Platform$OS.type == "windows") {
        ipmessage <- system("ipconfig", 
                            intern = TRUE)
    } else {
        ipmessage <- system("/sbin/ifconfig", 
                            intern = TRUE)
    }
    # validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]) {3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
    # any(grep(validIP, ipmessage))
    validIP <- "(?<=[^0-9.]|^)[1-9][0-9]{0,2}([.]([0-9]{0,3})){3}(?=[^0-9.]|$)"
    
    return(any(unlist(gregexpr( validIP, ipmessage, perl = TRUE) ) != -1))
}
isInternet <- TRUE # isInternet <- havingIP() # no use to test. If there is no Internet then it is not necessary to run the script

# detectiong the OS
isOS <- .Platform$OS.type 

# Checking if RStudio is used
isRStudio  <- Sys.getenv("RSTUDIO") == "1"

# Checking tcltk capability
isTcltk <- FALSE # isTcltk <- capabilities("tcltk")
if (isTcltk) {
    Hastcltk2 <- require(tcltk2)
    if (!Hastcltk2) { # kimisc needs to be installed ot use kimisc::thisfile(), checking if internet is available
        if (isInternet) {
            install.packages("tcltk2"); require(tcltk2)
        } else stop(cat("[shiny, isTcltk] ERROR, internet missing to install the package tcltk2."))    
    }
    # tkmessageBox(message = rbind(paste0("[Shiny] INFO, the OS platform is : ", isOS, "\n"),
    #                              if (isRStudio)  "[shiny, isTcltk] INFO, ASE_Script is run under Rstudio\n" else "[shiny, isTcltk] INFO, ASE_Script is not run under Rstudio\n",
    #                              if (isInternet) "[shiny, isTcltk] INFO, internet is available\n" else "[shiny, isTcltk] INFO, internet is not available\n",
    #                              "[shiny, isTcltk] INFO, the OS is able to run tcltk\n"),
    #              icon = "info", type = "ok")
}  else{
    cat(paste0("[shiny, isTcltk] INFO, the OS platform is : ", isOS), sep = "\n") 
    if (isRStudio)  cat("[shiny, isTcltk] INFO, ASE_Script is run under Rstudio\n") else cat("[shiny, isTcltk] INFO, ASE_Script is not run under Rstudio\n")
    if (isInternet) cat("[shiny, isTcltk] INFO, internet is  available\n") else cat("[shiny, isTcltk] INFO, internet is not available\n")
    cat("[shiny, isTcltk] INFO, the OS is not able to run tcltk\n")
}  

#================================================================CR
#  1 Config ====
#    .a Setting the Working Directory, checking file availability. Sourcing Functions4AES.R and SensorToolBox, geting path of ASEconfig_xx.R 
#    .b Loading packages
#    .c Create the directory three for the AirSensEUR device, change working directory to point to the directory of the AirSensEUR device, 
#       sending console to a file in the directory three (script log)
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
    
    #browser()
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
if (isTcltk) {
    # tkmessageBox(message = paste0("[shiny, isTcltk] INFO, directory from where the script is run: ", DisqueFieldtest, "\n"), 
    #              icon = "info", type = "ok")
}  else{
    cat(paste0("[shiny, isTcltk] INFO, directory from where the script is run: ", DisqueFieldtest), sep = "\n") 
}  

# checking presence of necessary files
cat("-----------------------------------------------------------------------------------\n")
cat("[Shiny] INFO, checking presence of necessary files (ASEconfig_xx.R and Functions4ASE.R). Then setting the Working Directory.\n")
Functions4ASE  <- file.path(DisqueFieldtest, "Functions4ASE.R")
if (!file.exists(c(Functions4ASE))) { 
    
    if (isTcltk) {
        
        # tkmessageBox(message = paste0("[Shiny] ERROR, file ", Functions4ASE, " not found, stopping the process\n"), 
        #              icon = "info", type = "ok")
    }  else cat(paste0("[Shiny] ERROR, file ", Functions4ASE, " not found, stopping the process\n")) 
    stop(cat(paste0("[Shiny] ERROR, file ", Functions4ASE), " not found, stopping the process\n"))
} else {
    
    if (isTcltk) {
        
        # tkmessageBox(message = paste0("[Shiny] INFO, file ", Functions4ASE , " found and ready to be sourced\n"), 
        #              icon = "info", type = "ok")
    }  else cat(paste0("[Shiny] INFO, file ", Functions4ASE , " found and ready to be sourced\n")) 
}
if (all(!grepl(pattern = glob2rx("ASEconfig*.R"), list.files(path = DisqueFieldtest, pattern = ".R")))) {
    
    
    if (isTcltk) {
        # tkmessageBox(message = paste0("[Shiny] ERROR, no AirSensEUR config file found (ASEconfig_*.R), stopping the process\n"), 
        #             icon = "info", type = "ok")
    }  else cat(paste0("[Shiny] ERROR, no AirSensEUR config file found (ASEconfig_*.R), stopping the process\n")) 
    stop(cat(paste0("[Shiny] ERROR, no AirSensEUR config file found (ASEconfig_*.R), stopping the process\n")))
} else {
    
    if (isTcltk) {
        # tkmessageBox(message = paste0("[Shiny] AirSensEUR config file found: ", 
        #                               list.files(path = DisqueFieldtest, pattern = glob2rx("ASEconfig*.R")),"\n"), 
        #              icon = "info", type = "ok")
    }  else cat(paste0("[Shiny] AirSensEUR config file found: ", list.files(path = DisqueFieldtest, pattern = glob2rx("ASEconfig*.R")), "\n")) 
    
} 
cat("-----------------------------------------------------------------------------------\n")

#----------------------------------------------------------------CR
# 1.a Sourcing SensorToolBox and Functions4AES.R----
#----------------------------------------------------------------CR
DisqueSensToolBox  <- file.path(DisqueFieldtest,"151016 Sensor_Toolbox.R")
cat("\n")
cat("-----------------------------------------------------------------------------------\n")
cat("[Shiny] INFO, checking presence of necessary file 151016 Sensor_Toolbox.R.\n")

if (!file.exists(c(DisqueSensToolBox))) { 
    
    if (isTcltk) {
        
        # tkmessageBox(message = paste0("[Shiny] ERROR, file ", DisqueSensToolBox, " not found, stopping the process\n"), 
        #              icon = "info", type = "ok")
    }  else cat(paste0("[Shiny] ERROR, file ", DisqueSensToolBox, " not found, stopping the process\n")) 
    stop(cat(paste0("[Shiny] ERROR, file ", DisqueSensToolBox), " not found, stopping the process\n"))
} else {
    if (isTcltk) {
        
        # tkmessageBox(message = paste0("[Shiny] INFO, file ", DisqueSensToolBox , " found and ready to be sourced\n"), 
        #              icon = "info", type = "ok")
    }  else cat(paste0("[Shiny] INFO, file ", DisqueSensToolBox , " found and ready to be sourced\n")) 
}

cat(paste0("[Shiny] INFO, sourcing 151016 Sensor_Toolbox.R and Funtions4ASE.R"), sep = "\n")

# Loading SensorToolBox
source(DisqueSensToolBox)
remove(DisqueSensToolBox) 

# Source Functions4ASE.R after SensorToolBox in order to update the last version of functions in Functions4ASE.R
source(Functions4ASE)
remove(Functions4ASE) 
cat("-----------------------------------------------------------------------------------\n")
cat("\n")

#----------------------------------------------------------CR
#  1.b. Install packages (CRAN + Github) ----
#----------------------------------------------------------CR
cat("-----------------------------------------------------------------------------------\n")
cat("[Shiny] INFO, Check or install packages needed to run the script\n")
# Packages to be loaded
# Clean and consistent tools to split-apply-combine pattern in R        --> plyr # use the function plyr::rbind.fill to add number of dataframes together
# To locate current file and dir (jchoose.files)                        --> R.utils
# To read sensor data, needed for senorweb4R, install before openair    --> stringi
# Date format for Influxdbr                                             --> xts         # 17-07-15: influxdrb is not used anymore maybe we can get rid of xts?
# When removing ouliers, using rollapply()                              --> zoo 
# Easier management of time interval                                    --> lubridate   
# To plot time series                                                   --> openair
# Package needed for devtools::install_github("52North/sensorweb4R")    --> curl
# Two packages needed for github sensorweb4R if you have a proxy        --> futile.options, lambda.r, 
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
# Grahical User Interface                                               --> shiny
# function close.window                                                 --> shinyjs
# change shiny theme                                                    --> shinythemes
# downloading data from influxdb server                                 --> influxdbr #17-07-15 : not used anymore because of mistakes, now using hhtr + JSONLIte
# downloading data from influxdb server, used instaed of influxdbr      --> httr, jsonlite
# transpose dataFrame, and rbindlist (faster than rbindfill)            --> data.table
# Correlation matrix                                                    --> corrplot 
# crating polynomial for solving the cubic equation                     --> polynom                                                 
# load packages for alphanumeric operations (shield config file)        --> BMS
# package for saving loading list (index for warming , outliers...)     --> rlist
# Plot data table in shiny web interface                                --> DT
# Edit dataTable                                                        --> rhandsontable
# rsqlite query name of tables                                          --> dplyr, dbplyr 
# small shiny button                                                    --> shinyBS
# legend with colorbar.plot                                             --> fields
# Better arrows for Target Diagram                                      --> shape
# file extension file_ext                                               --> tools
# modal message box                                                     --> shinyalert
# function str_detect, like grepl but for several pattern               --> stringr
# cross-platform dialog box to select file for uploading ref data       --> rChoiceDialogs uses rJava whcih does not install under linux rstudio-server. 
#                                                                           We could use tcltk instead for linux, but we cannot anymore upload tcltk on rstudio-server.
#                                                                           Anyhow this not important because it is not possible to upload local file to the shiny server, 
#                                                                           only server side files. Finally the functionality
# Automatic reporting                                                   --> rmarkdown, knitr, rmarkdown, xtable
# Add CSS Loading Animations to 'shiny' Outputs                         --> shinycssloaders
# library(bitops)
#
list.Packages <- c( "stringi"    , "plyr"         , "openair"     , "zoo"           , "futile.options", 
                    "lambda.r"   , "curl"         , "sp"          , "httr"          , "devtools",
                    "ggplot2"    , "corrplot"     ,   
                    "limSolve"   , "lubridate"    , "minpack.lm"  ,   
                    "quantreg"   , "RCurl"        , "reshape"     , "RJSONIO"       , "RODBC"   , "RSQLite" , 
                    "R.utils"    , "sqldf"        ,  "XML"        , "xts"           , "zoo"     , "mgcv"    , 
                    "shiny"      , "shinyjs"      , "jsonlite"    , "data.table"    , "shinythemes",
                    "corrplot"   , "polynom"      , "BMS"         , "rlist"         , "DT"      , "dplyr"   , "dbplyr"   ,
                    "shinyBS"    , "rhandsontable", "fields"      , "shape"         , "tools"   , "shinyalert",
                    "stringr"    , "rmarkdown"    , "xtable"      , "knitr"         , "shinycssloaders") 
Load.Packages(list.Packages)
# if error on plyr then type install.packages("plyr") at the console

# GitHub, this can crash the code if you have a PROXY, the lines cvan be commented
# list.packages.github <- c("52North/sensorweb4R")
# for (i in list.packages.github) {
#     
#     # removing author name anad version number
#     lib.i <- tail(unlist(strsplit(i, split = "/")), n = 1)
#     lib.i <- head(unlist(strsplit(lib.i, split = "@")), n = 1)
#     
#     if (!(lib.i %in% rownames(installed.packages()))) {
#         devtools::install_github(i)
#         cat(sprintf("Package ", lib.i, " installed"), sep = "\n")
#     } else cat(paste0("[Shiny] INFO, Package ", i, " already installed"), sep = "\n")
#     
#     do.call("library", as.list(lib.i))
#     cat(sprintf("[Shiny] INFO, Package %s loaded",i), sep = "\n")
# }

cat("[Shiny] INFO, List of installed packages\n")
print(search(), quote = FALSE)
cat("\n")

#----------------------------------------------------------------CR
#  1.c Getting the file path for ASEconfig_xx.R ----
#----------------------------------------------------------------CR
# Name of the configuration file, the extension shall be .R
cat("-----------------------------------------------------------------------------------\n")
cat(paste0("[Shiny] INFO, setting working directory to ", DisqueFieldtest), sep = "\n")
setwd(DisqueFieldtest)

#----------------------------------------------------------------CR
# Init Shiny ----
#----------------------------------------------------------------CR
choices.ASEconfig <- list.files(path = getwd(), pattern = glob2rx("ASEconfig*.R"))
Dir.Logs          <- grep(pattern = glob2rx("*scriptsLog*"), x = list.dirs(DirShiny), value = TRUE)
choices.Logs      <- list.files(Dir.Logs, full.names = TRUE)
Selected.Logs     <- choices.Logs[which.max(file.info(choices.Logs)$mtime)]
jscode            <- "shinyjs.closeWindow = function() { window.close(); }"
TimeZone          <- c("UTC", "Etc/GMT-1", "Europe/Amsterdam" , "Europe/Berlin", "Europe/Paris", "Europe/Rome") # Fill up with other Time Zone
Influx.TimeZone   <- c("UTC", "Etc/GMT-1", "Local time", "Europe/Amsterdam" , "Europe/Berlin", "Europe/Paris", "Europe/Rome") # Fill up with other Time Zone
TableTZ           <- as.data.frame(cbind( 1:length(TimeZone), TimeZone), stringsAsFactors = FALSE)
Influx.TableTZ    <- as.data.frame(cbind( 1:length(Influx.TimeZone), Influx.TimeZone), stringsAsFactors = FALSE)
choices.shield    <- list.files(path = file.path(getwd(), "Shield_Files"), pattern = "*.asc")
choices.Ref.unit  <- c("ppb","ppm", "ug/m3","mg/m3","counts")

# ui =============================================================
ui <- navbarPage(title = "AirSensEUR v0.11", id = "ASE", theme = shinytheme("cerulean"), selected = "SelectASE",
                 
                 # shinyjs must be initialized with a call to useShinyjs() in the app's ui.
                 useShinyjs(),
                 # Set up shinyalert
                 useShinyalert(),  
                 extendShinyjs(text = jscode, functions = c("closeWindow")),
                 
                 # Include the line below in ui.R so you can send messages
                 # https://stackoverflow.com/questions/32226331/r-shiny-pop-up-window-with-options
                 tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});')),
                           tags$style(".shiny-plot-output{height:82vh !important;}")),
                 
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
                                  textInput(inputId = "NewFile", label = "New config file (ASEconfig*, * = SOS id)", value = "ASEconfig"),
                                  actionButton(inputId = "Create.New", label = "Create new AirSensEUR", icon = icon("plus-circle")),
                                  hr(),
                                  actionButton(inputId = "Quit"   , label = "Quit", icon = icon("power-off"))
                                  , width = 3),
                              mainPanel(
                                  tabPanel("Selected_ASE",
                                           tabsetPanel(id = "tabMainPanel",
                                                       # do not add a Spinner on the next TableOutput as it will not update
                                                       tabPanel(title = "Push data", icon = icon("download"), tableOutput("Pushdata.cfg")),
                                                       tabPanel("Filtering", icon = icon("filter"),
                                                                h4("Filtering Sensors data")  , tableOutput("FilteringSensor"),
                                                                h4("Filtering Reference data"), tableOutput("FilteringRef")
                                                       ),
                                                       tabPanel(title = "Calibration", tableOutput("Calib.cfg"), icon = icon("tachometer")),
                                                       tabPanel(title = "SetTime"    , tableOutput("SetTime.cfg"), icon = icon("time", lib = "glyphicon"))
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
                                                       uiOutput("uiasc.File")
                                              ),
                                              # tabPanel("shield",
                                              # ),
                                              tabPanel(title = "Proxy", 
                                                       value = "tPProxy",
                                                       uiOutput("uiPROXY"), 
                                                       div(style = "display: inline-block;vertical-align:top; width: 74%;",uiOutput("uiURL")),
                                                       div(style = "display: inline-block;vertical-align:top; width: 25%;",uiOutput("uiPORT")),
                                                       div(style = "display: inline-block;vertical-align:top; width: 48%;",uiOutput("uiLOGIN")),
                                                       div(style = "display: inline-block;vertical-align:top; width: 48%;",uiOutput("uiPASSWORD"))
                                              ),
                                              tabPanel(title = "Sensor Data",
                                                       value = "tPSensordown",
                                                       tabsetPanel(id = "SensorDown",
                                                                   tabPanel(title = "SOS"   , 
                                                                            value = "tPSOS",
                                                                            uiOutput("uiDown.SOS"), 
                                                                            uiOutput("uiAirsensWeb"), 
                                                                            uiOutput("ASE.name"), 
                                                                            uiOutput("uiSOS.tzone"), 
                                                                            uiOutput("uiDown_SOS")
                                                                   ),
                                                                   tabPanel(title = "Influx", 
                                                                            value = "tPInflux",
                                                                            uiOutput("uiDown.Influx"),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 74%;",uiOutput("uiHost")),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 1%;",HTML("<br>")),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 23%;",uiOutput("uiPort")),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 48%;",uiOutput("uiUser")),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 48%;",uiOutput("uiPass")),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 48%;",uiOutput("uiDb")),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 48%;",uiOutput("ASE.Box")),
                                                                            uiOutput("uiInflux.TZ"),
                                                                            uiOutput("uiDown_Influx")
                                                                   )
                                                       )
                                              ),
                                              tabPanel(title = "Reference Data" , value = "tPRef",
                                                       uiOutput("uiDown.Ref"),
                                                       uiOutput("uiSelected"),
                                                       br(),
                                                       tabsetPanel(id = "DownloadMode", selected = "ftp",
                                                                   tabPanel("SOS",
                                                                            uiOutput("uiRefSOSname"),
                                                                            uiOutput("uiRef.SOS.name"), 
                                                                            uiOutput("uiRefPollutants"),
                                                                            uiOutput("uiRefDateDownload")
                                                                   ),
                                                                   # https://shiny.rstudio.com/reference/shiny/latest/fileInput.html
                                                                   # https://stackoverflow.com/questions/21813773/r-shiny-uploading-a-file-on-button-click
                                                                   tabPanel("csv",
                                                                            br(),
                                                                            p("reference data can only be in 1 csv file with headers: date(Y-m-d H:M:S), CO_ppm or CO/co,NO,NO2,O3,NOx,SO2, PM2.5, PM10"),
                                                                            textInput(inputId = "file1", 
                                                                                      label   = "Choose CSV File:"),
                                                                            actionButton(inputId = "browse", 
                                                                                         label   = "Browse"),
                                                                            # fileInput(inputId = "file1", 
                                                                            #           label = "Choose CSV File",
                                                                            #           accept = c(
                                                                            #               "text/csv",
                                                                            #               "text/comma-separated-values,text/plain",
                                                                            #               ".csv")
                                                                            # ),
                                                                            checkboxInput(inputId = "header", 
                                                                                          label   = "Header", 
                                                                                          value   = TRUE),
                                                                            radioButtons(inputId = 'sep', 
                                                                                         label = 'Separator',
                                                                                         choices = c(Comma = ",", Semicolon = ";", Tab = '\t'), 
                                                                                         selected = ",", 
                                                                                         inline = TRUE),
                                                                            radioButtons(inputId = 'quote', 
                                                                                         label = 'Quote',
                                                                                         choices = c(None = '', 'Double Quote'='"','Single Quote' = "'"), 
                                                                                         selected = '"', 
                                                                                         inline = TRUE)
                                                                   ),
                                                                   tabPanel("ftp",
                                                                            uiOutput("uiurlref"),
                                                                            p("Data can be in 1 or more url linking to 1 csv files with headers: 
                                                                              DateTime(Y-m-d H:M:S), CO_ppm,NO,NO2,O3,NOx,SO2, on a ftp site")
                                                                   )
                                                       ),
                                                       tags$hr(),
                                                       uiOutput("uiReference.name"),
                                                       uiOutput("uicoord.ref"), 
                                                       uiOutput("uialt.ref"), 
                                                       uiOutput("uiref.tzone"),
                                                       uiOutput("uiDown_Ref")
                                              )
                                  )
                                  , width = 3),
                              mainPanel(
                                  tabsetPanel(id = "InfoPrint",
                                              #tabPanel("Log", verbatimTextOutput("console")),
                                              tabPanel(title = "GetData Panel",
                                                       
                                                       h4("Time"), 
                                                       textOutput("UserMins"), 
                                                       textOutput("UserMinsAvg"), 
                                                       textOutput("Delay"),
                                                       hr(), h4("Shield Data"), 
                                                       textOutput("asc.File"), 
                                                       tableOutput("Shield"),
                                                       
                                                       hr(), h4("Updated Proxy values"), 
                                                       textOutput("PROXY"), 
                                                       textOutput("URL"), 
                                                       textOutput("PORT"), 
                                                       textOutput("LOGIN"), 
                                                       textOutput("PASSWORD"),
                                                       
                                                       hr(), h4("Influx"), 
                                                       textOutput("Down.Influx"), 
                                                       textOutput("Host"), 
                                                       textOutput("Port"), 
                                                       textOutput("User"), 
                                                       textOutput("Pass"), 
                                                       textOutput("Db"), 
                                                       textOutput("Dataset"), 
                                                       textOutput("Influx.TZ"),
                                                       
                                                       hr(), h4("SOS"), 
                                                       textOutput("Down.SOS"), 
                                                       textOutput("AirsensWeb"), 
                                                       textOutput("AirsensEur.name"), 
                                                       textOutput("SOS.TZ"),
                                                       
                                                       hr(), h4("Reference Data"), 
                                                       textOutput("Down.Ref"), 
                                                       textOutput("FTPMode"),
                                                       textOutput("urlref"), 
                                                       textOutput("Reference.name"), 
                                                       textOutput("GDPRefSOSname"),
                                                       textOutput("GDPRef.SOS.name"), 
                                                       textOutput("GDPRefPollutants"),
                                                       textOutput("GDPRefDateDownload"),
                                                       textOutput("coord.ref"), 
                                                       textOutput("alt.ref"), 
                                                       textOutput("ref.tzone")
                                              ),
                                              tabPanel(title = "Influx Sensor Data",  withSpinner(verbatimTextOutput('Influx.Content'), type = 8) ),
                                              tabPanel(title = "SOS Sensor Data",     withSpinner(verbatimTextOutput('SOS.Content'), type = 8) ),
                                              tabPanel(title = "Reference Data",      withSpinner(verbatimTextOutput('Ref.content'), type = 8) ),
                                              tabPanel(title = "General Sensor Data", withSpinner(verbatimTextOutput('General.Content'), type = 8) )
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
                                      checkboxInput(inputId = "SavePlot"   , label = "Save Plot", value = FALSE, width = NULL)
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
                                                                            h4("Filtering Reference data"), tableOutput("Outliers_Ref")
                                                                   ),
                                                                   tabPanel("CalibMain"    , tableOutput("Calib_data"), icon = icon("tachometer")),
                                                                   tabPanel("SetTimeMain"  , tableOutput("CalTime"), icon = icon("time", lib = "glyphicon") )
                                                       )
                                              ),
                                              tabPanel("RawData"      , icon = icon("signal"), withSpinner(plotOutput(outputId = "RawData"), type = 8) ),
                                              tabPanel("DataTable"    , icon = icon("bars")  , withSpinner(DT::dataTableOutput(outputId = "DataTable"), type = 8) ),
                                              tabPanel("Retrieved"    , icon = icon("signal"), withSpinner(plotOutput(outputId = "Retrieved"), type = 8) ),
                                              tabPanel("PlotFiltering", icon = icon("filter"),
                                                       tabsetPanel(id = "tabPlots",
                                                                   tabPanel("Warming"      ,  icon = icon("toggle-off")  , withSpinner(plotOutput(outputId = "Warming"), type = 8) ),
                                                                   tabPanel("Temp.&Humid." ,  icon = icon("tint")        , plotOutput(outputId = "Temp.Humid") ),
                                                                   tabPanel("Invalid", icon = icon("cut"),
                                                                            tabsetPanel(id = "TabInvalid",
                                                                                        #tabPanel("Table"     , DT::dataTableOutput(outputId = "Table.Inv")),
                                                                                        tabPanel("Table", icon = icon("bars"), 
                                                                                                 br(),
                                                                                                 helpText("Time periods of Invalid data for the selected sensor and reference data. ", 
                                                                                                          "Right-click on the table to delete/insert rows. ", 
                                                                                                          "Double-click on a cell to edit. The button \"Save\" only saves the file of invalid
                                                                                                          date time periods while discarding of invalids is carried out by setting the CheckBoxes 
                                                                                                          \"Apply validity periods\" to TRUE."),
                                                                                                 br(),
                                                                                                 actionButton(inputId = "Save.row.Valid", label = "Save",icon = icon("save")),
                                                                                                 br(),
                                                                                                 rHandsontableOutput("hot")),
                                                                                        tabPanel("Plot", icon = icon("signal"), plotOutput(outputId = "Invalid.Sens") )
                                                                            )
                                                                   ),
                                                                   tabPanel("Neg.values"   ,  icon = icon("minus-circle"), plotOutput(outputId = "Neg.values") ),
                                                                   tabPanel("Outliers",  icon = icon("log-out", lib = "glyphicon"), 
                                                                            tabsetPanel(id = "TabOutliers",
                                                                                        tabPanel("Sens.Outliers", icon = icon("thermometer"), plotOutput(outputId = "Sens.Outliers") ),
                                                                                        tabPanel("Ref.Outliers" , icon = icon("calculator", lib = "font-awesome"), plotOutput(outputId = "Ref.Outliers") )
                                                                            )
                                                                   ),
                                                                   tabPanel("StatFiltering",  icon = icon("eye"), tableOutput("StatFiltered"))
                                                       )
                                              ),
                                              tabPanel("Covariates", icon = icon("sort-by-alphabet", lib = "glyphicon"),
                                                       tabsetPanel(id = "TabCovariates",
                                                                   tabPanel("TimeSeries" , icon = icon("stats", lib = "glyphicon"), withSpinner(plotOutput("ValidCovarTS"), type = 8) ),
                                                                   tabPanel("Matrix"     , icon = icon("th"   , lib = "glyphicon"), withSpinner(plotOutput("ValidCovarMatrix"), type = 8) )
                                                       )
                                              ),
                                              tabPanel("Calibration", icon = icon("tachometer"),
                                                       tabsetPanel(id = "TabCalibration",
                                                                   tabPanel("Scatterplot"     , icon = icon("line-chart", lib = "font-awesome"), withSpinner(plotOutput("Calibration"), type = 8) ),
                                                                   tabPanel("SummaryCal"      , icon = icon("list-alt"  , lib = "glyphicon")   , verbatimTextOutput("SummaryCal")),
                                                                   tabPanel("Calibrated"      , icon = icon("line-chart", lib = "font-awesome"), withSpinner(plotOutput("Calibrated"), type = 8) ),
                                                                   tabPanel("TimeSeries"      , icon = icon("stats"     , lib = "glyphicon")   , withSpinner(plotOutput("CalibrationTS"), type = 8) ),       
                                                                   #                      tabPanel("Summary"         , tableOutput("Summary")),
                                                                   tabPanel("Residual Matrix" , icon = icon("th"        , lib = "glyphicon")   , withSpinner(plotOutput("ResCalMatrix"), type = 8) ),
                                                                   tabPanel("Multivariates"   , icon = icon("list-alt"  , lib = "glyphicon")   , 
                                                                            fluidRow(
                                                                                column(width = 5, offset = 0,
                                                                                       br(),
                                                                                       helpText("Coefficients and type of relationship between covariates and sensor data. ", 
                                                                                                "Double-click on a cell with values to edit. The button \"Save\"  saves the file of multivariate
                                                                                                fitting. It shall be saved in order to be used when calibrating fitting."),
                                                                                       br(),
                                                                                       actionButton(inputId = "New.row.Multi" , label = "New"   ,icon("list-alt"  , lib = "glyphicon")),
                                                                                       actionButton(inputId = "Save.row.Multi", label = "Save"  ,icon = icon("save")),
                                                                                       actionButton(inputId = "Del.row.Multi" , label = "Delete",icon = icon("Del")),
                                                                                       br(),
                                                                                       rHandsontableOutput("Multi")
                                                                                ),
                                                                                column(width = 4, offset = 0,
                                                                                       verbatimTextOutput("ListValid")
                                                                                )
                                                                            )
                                                                   )
                                                       ) 
                                              ),
                                              tabPanel("Extrapolation" , icon = icon("line-chart"), 
                                                       tabsetPanel(id = "TabExtrapolation",
                                                                   tabPanel("Scatterplot"     , icon = icon("line-chart", lib = "font-awesome"), withSpinner(plotOutput("Extrapolation"), type = 8) ),
                                                                   tabPanel("SummaryExtra."   , icon = icon("list-alt"  , lib = "glyphicon")   , verbatimTextOutput("SummaryExtra")),
                                                                   tabPanel("TimeSeries"      , icon = icon("stats"     , lib = "glyphicon")   , withSpinner(plotOutput("ExtrapolationTS"), type = 8) ),       
                                                                   tabPanel("Residual Matrix" , icon = icon("th"        , lib = "glyphicon")   , withSpinner(plotOutput("ResExtraMatrix"), type = 8) ),
                                                                   tabPanel("Uncertainty"     , icon = icon("stats"     , lib = "glyphicon")   , 
                                                                            fluidRow(
                                                                                column(width = 4, offset = 0,
                                                                                       tableOutput("U_Table")
                                                                                ),
                                                                                column(width = 4, offset = 0,
                                                                                       withSpinner(plotOutput("Uncertainty"), type = 8)
                                                                                ),
                                                                                column(width = 4, offset = 0,
                                                                                       plotOutput("SqrRes")
                                                                                )
                                                                            ),
                                                                            fluidRow(
                                                                                column(width = 4, offset = 0,
                                                                                       plotOutput("Scatter")
                                                                                )
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
                                              tabPanel("Report MarkDown", icon = icon("bars"), h3("work in progress..."), htmlOutput("renderedReport"),
                                                       downloadButton("report", "Generate report")),
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
                              verbatimTextOutput("VerionsInfo"), 
                              width = 9
                          )
                 )
                 ,
                 tabPanel("Help", value = "Help", icon = icon("question"),
                          sidebarPanel(titlePanel("User Manual")
                                       , width = 1
                          ),
                          mainPanel( 
                              tags$iframe(style = "height:900px; width:100%; scrolling=yes", src = "ShinyASE.pdf"),
                              #htmlOutput('pdfviewer'), 
                              #uiOutput("pdfview"), 
                              width = 11
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
    
    # initial config
    shinyjs::disable("Selected")
    
    # The "NewFile" field is mandatory and thus the "Create.New" button should not be enabled if there is no name
    # https://cran.r-project.org/web/packages/shinyjs/vignettes/shinyjs-example.html
    observe({
        if (is.null(input$NewFile) | input$NewFile == "" | input$NewFile == "ASEconfig") {
            shinyjs::disable("Create.New")
        } else {
            shinyjs::enable("Create.New")
        }
    })
    
    # Initial values and reactives ----
    # AirSensEUR name: The one given by the Selected config file
    # AirSensEUR name: The one selected in the list of configured AirSensEURs
    ASE_name           <- reactive({ 
        cc <-basename(input$Selected)
        for (i in c("\\.[[:alnum:]]+$","ASEconfig")) cc <- sub(pattern= i,replacement = '', basename(as.character(cc)))
        return(cc) 
    })
    ASE_name.List      <- reactive({ 
        cc <- basename(input$Config_Files)
        for (i in c("\\.[[:alnum:]]+$","ASEconfig")) cc <- sub(pattern = i,replacement = '', basename(as.character(cc)))
        return(cc) 
    })
    
    # Reactive DisqueFieldtestDir() ----
    # DisqueFieldtestDir     : The one of the Selected AirSensEUR
    # DisqueFieldtestDir.List: The one given by the Selected config file in the list of Configured AirSensEUR
    DisqueFieldtestDir      <- reactive({return(file.path(DirShiny,ASE_name())) })
    DisqueFieldtestDir.List <- reactive({return(file.path(DirShiny,ASE_name.List())) })
    
    # Reactive cfg_file() ----
    # cfg_file     : The one of the Selected AirSensEUR
    # cfg_file.List: The one given by the Selected config file in the list of Configured AirSensEUR
    cfg_file           <- reactive({file.path(DisqueFieldtestDir()     ,"General_data",paste0(ASE_name()     ,".cfg"))})
    cfg_file.List      <- reactive({file.path(DisqueFieldtestDir.List(),"General_data",paste0(ASE_name.List(),".cfg"))})
    
    # Reactive Config() ----
    # Config     : The one of the Selected AirSensEUR
    # Config.List: The one given by the Selected config file in the list of Configured AirSensEUR (# Configuration to show in MainTabPanel of NaBar menu "SelectASE")
    Config             <- reactive({
        # Reading of file ASE_name.cfg, ASE_name_Servers.cfg (server parameters including asc.file) and Covariates. cfg
        
        # make Config() dependent on input$asc.File and reactive to change of shield asc.File when reading asc.File in ASE_name_Servers.cfg
        input$asc.File 
        
        # make Config() reactive on input$Select to change the file each time input$select is clicked
        input$Select
        
        # only run CONFIG() if button input$Select is clicked once
        # Cannot use input$Selected as it is not yet updated
        if (input$Select > 0) return(CONFIG(DirShiny,isolate(input$Config_Files)))
    })
    Config.List           <- reactive({
        # Reading of file ASE_name.List.cfg, ASE_name.List_Servers.cfg (server parameters including asc.file) and Covariates. cfg
        
        # make Config() dependent on input$asc.File and reactive to change of shield asc.File when reading asc.File in ASE_name_Servers.cfg
        input$asc.File 
        
        # make Config() reactive on input$Select to change the file each time input$select is clicked
        input$Select
        
        # run CONFIG() of the list of configured AirSensEUR
        return(CONFIG(DirShiny,input$Config_Files))
    })
    
    # Reactive i.sensors ----
    i.sensors          <- reactive({
        # Returning the indexes of valid sensors in ASE_name.cfg with NAs taking into account
        
        # depends on input$asc.File, to make i.sensors() reactive on change of shield config file
        # I do not think it is necesary, the number of sensors should remain the same, but maybe a new sensor for a new compound could be installed
        input$asc.File 
        return(which(!is.na(Config()[[2]]$name.sensor)))
    })
    # Index valid sensors in ASE_name_Setime.cfg
    i.sensors.time <- reactive({
        # depends on input$asc.File, to make i.sensors.time() reactive on change of shield config file
        # I do not think it is necesary, the number of sensors should remain the same, but maybe a new sensor for a new compound could be installed
        input$asc.File # to make it reactive to change of shield config file
        which(!is.na(Set.Time()[[1]]$name.sensor))
    })
    list.gas.sensors       <- reactive({Config()[[2]]$gas.sensor[ !is.na(Config()[[2]]$gas.sensor)]}) 
    list.name.sensors      <- reactive({Config()[[2]]$name.sensor[!is.na(Config()[[2]]$name.sensor)]}) 
    list.gas.reference2use <- reactive({
        # return a vector with the names of gas reference using the file ASE.cfg
        # only return the list of Reference gas if they are  included into the RefData file (names(REFDATA()[[1]]))
        Config()[[2]]$gas.reference2use[!is.na(Config()[[2]]$gas.reference2use) & Config()[[2]]$gas.reference2use %in% names(REFDATA()[[1]])]
    })
    
    # Reactive DownloadSensor() ----
    # Getting info on last downloaded data, make i reactive to change in INFLUX(), SOS_T(), REFDATA(), DF$General
    DownloadSensor          <- reactive({ 
        Check_Download(Influx.name = input$Dataset,
                       WDinput     = file.path(DisqueFieldtestDir(), "General_data"), 
                       UserMins    = as.numeric(input$UserMins)
        )
    })
    
    # Reactive Set.Time ----
    Set.Time <- reactive({
        # Reading SetTime from Ase-name_SETTIME.cfg file
        # returning a stat.frame of setTime parameters
        # Reactive input:
        #       DisqueFieldtestDir(), DF$General, input$Influx.TZ , input$SOS.TZ, input$ref.tzone, DownloadSensor()
        # Updating dates of sliderInputs and discarding invalid data
        
        # Create a Progress object
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        progress$set(message = "[shiny, Set.Time()] INFO, Reading dates of SetTimes for the SetTime.cfg file", value = 0.2)
        
        D <- SETTIME(DisqueFieldtestDir = DisqueFieldtestDir(), 
                     General.t.Valid    = DF$General,
                     Influx.TZ          = input$Influx.TZ , 
                     SOS.TZ             = input$SOS.TZ,
                     Ref.TZ             = input$ref.tzone, 
                     DownloadSensor     = DownloadSensor())
        
        progress$set(message = "[shiny, Set.Time()] INFO, Reading dates of SetTimes for the SetTime.cfg file", value = 1)
        progress$close()
        
        return(D)
    })
    
    # Models for Calibration
    Covariates        <- reactive({c(paste0("Out.",list.gas.reference2use()), INFLUX()[[2]],"date", paste0(list.gas.sensors(),"_modelled"), paste0(list.name.sensors(),"_volt"), paste0("Out.",list.gas.sensors())) }) 
    Covariates.Model  <- reactive({c(paste0("Out.",list.gas.reference2use()), INFLUX()[[2]],"date", paste0(list.gas.sensors(),"_modelled"), paste0(list.name.sensors(),"_volt"), paste0("Out.",list.gas.sensors())) }) 
    Models            <- c("Linear", "Linear.Robust","MultiLinear","gam", "Quadratic", "Cubic", "Michelis", "Sigmoid")
    #Models           <- c("Linear", "Linear.Robust","MultiLinear","gam", "NeuralNet", "Lab. calibration", "Quadratic", "Cubic","Michelis", "Sigmoid")
    
    # Navbar menu "About"
    output$VerionsInfo <- renderPrint(AboutVersions(DisqueFieldtest = DirShiny, 
                                                    FirstLineText   = "# Version History ====", 
                                                    LastLineText    = "# Content ===="), width = getOption("width"))
    
    # Navbar menu "Console Logs"
    output$LogstextWithHTML <- renderPrint({
        
        ## Create connection
        con <- file(description= input$ConsoleLogsFile, 
                    open= "r")
        
        ## Reading App.R
        Com <- readLines(con, n=-1)
        
        # Close connection
        close(con)
        
        
        return(Com[(length(Com)-1000):length(Com)])
    })
    
    # NavBar"SelectASE", Button Create.New,  ----
    # After a click on button "Create New AirSensEUR"
    observeEvent(input$Create.New, {
        
        # https://deanattali.com/blog/advanced-shiny-tips/#plot-spinner
        # Stop the App when the broser tab is closed
        session$onSessionEnded(stopApp)
        
        # Create a Progress object
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        progress$set(message = "Creating New Config File of AirSensEUR Box", value = 0.5)
        
        if (!file.exists(file.path(DirShiny, paste0(input$NewFile,".R"))) ) { 
            
            # !grepl(pattern = "[[:blank:]]", x = input$NewFile) & !grepl(pattern = "[[:punct:]]", x = input$NewFile) & prevent from using _
            
            #----------------------------------------------------------CR
            #  1.c Create file system structure. check for General.Rdata availability, sourcing ASEConfig_xx.R ####
            #----------------------------------------------------------CR
            # Check directories existence or create , create log file
            # Setting the current directory to the root of the file system with the name of The AirSensEUR
            old_ASE_name       <- basename(input$Config_Files) ; for (i in c("\\.[[:alnum:]_]+$" ,"ASEconfig")) old_ASE_name <- sub(pattern = i,replacement = '', basename(as.character(old_ASE_name)))
            ASE_name           <- basename(input$NewFile)      ; for (i in c("\\.[[:alnum:]_]+$" ,"ASEconfig")) ASE_name     <- sub(pattern = i,replacement = '', basename(as.character(ASE_name)))
            if (ASE_name != "") {
                
                DisqueFieldtestDir <- file.path(DisqueFieldtest, ASE_name)
                
                # Creating the the working directory of the AirSensEUR box
                cat("-----------------------------------------------------------------------------------\n")
                cat(paste0("[shiny, Create.New] INFO, Creating the the working directory: ",DisqueFieldtestDir, ". Setting it as working directory."), sep = "\n")
                if (!dir.exists(DisqueFieldtestDir)) dir.create(DisqueFieldtestDir, showWarnings = TRUE, recursive = FALSE, mode = "0777") 
                
                # Creating File structure
                cat("-----------------------------------------------------------------------------------\n")
                cat(paste0("[shiny, Create.New] INFO creating the file system for data treatment at ", DisqueFieldtestDir), sep = "\n")
                List.Dirs <- c("Calibration","Drift","Estimated_coef","General_data","Models","Modelled_gas","Outliers","scriptsLog",
                               "SensorData","Retrieved_plots","Statistics","Verification_plots", "MarkDown")
                for (i in List.Dirs) {
                    if (!dir.exists(file.path(DisqueFieldtestDir, i))) {
                        dir.create(file.path(DisqueFieldtestDir, i), showWarning = TRUE, recursive = TRUE, mode = "0777")
                        cat(paste0("[shiny, Create.New] INFO Dir. created: ", file.path(DisqueFieldtestDir,i)), sep = "\n\r")
                    } else cat(paste0("[shiny, Create.New] INFO Dir. already exists: ", file.path(DisqueFieldtestDir,i)), sep = "\n")
                } 
                
                # Populating the configuration information, copy  old_ASE_name as new ASE_name
                cat(paste0("[shiny, Create.New] INFO copying ", paste0("ASEconfig", ASE_name,".R")," the file system for data treatment at ", DisqueFieldtest), sep = "\n")
                file.copy(from = input$Config_Files, to = paste0("ASEconfig", ASE_name,".R"), overwrite = TRUE, copy.mode = TRUE, copy.date = FALSE)
                
                # Populating the configuration intormation
                # cfg and effect files
                Old_General_dir <- file.path(DisqueFieldtest, old_ASE_name , "General_data")
                New_General_dir <- file.path(DisqueFieldtestDir            , "General_data")
                cfg_Files       <- list.files(path = Old_General_dir, pattern = ".cfg")
                for (i in cfg_Files) {
                    cat(paste0("[shiny, Create.New] INFO, copying ", gsub(pattern = old_ASE_name, replacement = ASE_name, i), " at ", New_General_dir), sep = "\n")
                    file.copy(from = file.path(Old_General_dir,i), 
                              to   = file.path(New_General_dir, gsub(pattern = old_ASE_name, replacement = ASE_name, i)), 
                              overwrite = TRUE, copy.mode = TRUE, copy.date = FALSE)
                } 
                # Updating list of ASE boxes and select the newly created one
                Newchoices = list.files(path = DisqueFieldtest, pattern = glob2rx("ASEconfig*.R"))
                updateSelectInput( session = session,inputId = "Config_Files", label = "Select config file",
                                   choices = Newchoices, 
                                   selected = Newchoices[Newchoices %in% paste0("ASEconfig", ASE_name,".R")])
            }
        } 
        
        progress$set(message = "[shiny, Create.New] INFO, Creating New Config File of AirSensEUR Box", value = 1)
    })
    
    # NavBar"SelectASE", Button "Select AirSensEUR" ----
    observeEvent(input$Select, {
        
        # update disabled inoutText "Selected"
        updateTextInput(session, inputId = "Selected", label = NULL, value = input$Config_Files)
        
        # Once an AirSensEUR box is selected disable the possibility to create new AirSensEUR boc config files 
        shinyjs::disable("NewFile")
        
    })
    observeEvent(input$Selected, {
        
        # not run without clicking once on button
        if (input$Selected > 0) {
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            progress$set(message = "Selecting AirSensEUR Box", value = 0.5)
            
            # disabel the possibility to uncheck hearders of csv file for reference data, local files
            shinyjs::disable("header")
            
            #----------------------------------------------------------CR
            #  1.c Create file system structure. check for General.Rdata availability, sourcing ASEConfig_xx.R ####
            #----------------------------------------------------------CR
            # Store the current directory
            initial.dir <- DirShiny
            # Check directory existence or create 
            cat("-----------------------------------------------------------------------------------\n")
            cat(paste0("[shiny, Selected] INFO creating the file system for data treatment at ", DisqueFieldtestDir())," if it does not exist\n")
            if (!(initial.dir == DisqueFieldtestDir())) {if (!dir.exists(DisqueFieldtestDir())) dir.create(DisqueFieldtestDir())}
            List.Dirs <- c("Calibration","Drift","Estimated_coef","General_data","Models","Modelled_gas","Outliers","scriptsLog","SensorData",
                           "Retrieved_plots","Statistics","Verification_plots")
            for (i in List.Dirs) {
                if (!dir.exists(file.path(DisqueFieldtestDir(), i))) {
                    dir.create(file.path(DisqueFieldtestDir(), i), 
                               showWarning = TRUE, recursive = TRUE)
                    cat(paste0("[shiny, Selected] INFO Dir. created: ", file.path(DisqueFieldtestDir(),i)), sep = "\n\r")
                } else cat(paste0("[shiny, Selected] INFO Dir. already exists: ", file.path(DisqueFieldtestDir(),i)), sep = "\n")
            } 
            remove(List.Dirs,i)
            
            # setting the current directory to the root of the file system with the name of the AirSensEUR
            cat("-----------------------------------------------------------------------------------\n")
            cat(paste0("[shiny, Selected] INFO Change the working directory to: ",DisqueFieldtestDir()), sep = "\n")
            setwd(DisqueFieldtestDir())
            
            #----------------------------------------------------------CR
            # 1.c sending console to a file in the directory three (script log) and to variable Console for shiny TextOutput ####
            #----------------------------------------------------------CR
            while (sink.number() > 0) {print(paste0("Number of sink channels opened: ", sink.number(), ". Closing opened channels"))
                sink(file = NULL)
            }
            sink(file.path(DisqueFieldtestDir(), "scriptsLog",paste0("console_", Sys.Date(),".log")), 
                 type = c("output", "message"), 
                 split = TRUE, append = TRUE ) # with split = TRUE we get the file on the screen and in log file
            cat("-----------------------------------------------------------------------------------\n")
            cat(paste0("[shiny, Selected] INFO Starting log file ", file.path(DisqueFieldtestDir(), "scriptsLog",paste0("console_", Sys.Date(),".log"))), sep = "\n")
            cat("-----------------------------------------------------------------------------------\n")
            
            # Now ASEConfig can be sourced with function CONFIG and SETTIME to update the config files
            source(file.path(initial.dir, input$Selected))
            
            #  NavBar"GetData", sideBar tabPanel "time-shield" ----
            output$uiUserMins       <- renderUI({
                selectInput(inputId = "UserMins", 
                            label = "Averaging time in min"            , 
                            choices = c("1","2","3","4","5","6","10","12","15","20","30","40","45","60", "480", "1440"), 
                            selected = Config()[[1]]$UserMins
                )
            })
            output$uiUserMinsAvg   <- renderUI({
                selectInput(inputId = "UserMinsAvg", 
                            label = "Averaging time in min for extrapolated data"            , 
                            choices = c("1", "10","15","20","30","60", "480", "1440"), 
                            selected = Config()[[1]]$UserMinsAvg
                )
            })
            output$uiDelay          <- renderUI({
                if (!is.null(input$UserMins)) {
                    med.Index    <- median(seq(seq(-1440,1440, by = as.integer(input$UserMins))))
                    length.Index <- length(seq(seq(-1440,1440, by = as.integer(input$UserMins))))
                    selectInput(inputId = "Delay"   , 
                                label = "Delay in min, add minutes to sensor time (automatic Save)", 
                                choices = seq(-1440, 1440, by = as.integer(input$UserMins))[max(med.Index - 500, 1):min(length.Index,med.Index + 500)], # also add a few minutes if not present 
                                selected = Config()[[1]]$Delay
                    )}
            })
            output$uiasc.File       <- renderUI({
                selectInput("asc.File"   , 
                            label = "Sensor shield config file *.asc", 
                            choices = choices.shield, 
                            selected = choices.shield[choices.shield %in% Config()[[1]]$asc.File]
                )
            })
            output$uiNameSensors <- renderUI({
                radioButtons(inputId = "Sensors", 
                             label   = "Select Sensor", 
                             choices = list.name.sensors(), inline = TRUE
                )
            })
            
            #  NavBar"GetData", sideBar tabPanel "Proxy" ----
            output$uiPROXY    <- renderUI({
                checkboxInput("PROXY", 
                              label = "Enable PROXY", 
                              value = as.logical(Config()[[1]]$PROXY)
                )
            })
            output$uiURL      <- renderUI({
                textInput("URL", 
                          label = "URL of your proxy", 
                          value = Config()[[1]]$URL
                )
            })
            output$uiPORT     <- renderUI({
                textInput("PORT", 
                          label = "PORT", 
                          value = Config()[[1]]$PORT
                )
            })
            output$uiLOGIN    <- renderUI({
                textInput("LOGIN", 
                          label = "LOGIN for the proxy", 
                          value = Config()[[1]]$LOGIN
                )
            })
            output$uiPASSWORD <- renderUI({
                textInput("PASSWORD", 
                          label = "PASSWORD of the proxy", 
                          value = Config()[[1]]$PASSWORD
                )
            })
            
            #  NavBar"GetData", sideBar tabPanel "Influx" ----
            output$uiDown.Influx <- renderUI({
                checkboxInput("Down.Influx", 
                              label = "Enable InFluxDB", 
                              value = as.logical(Config()[[1]]$Down.Influx)
                )
            })
            output$uiHost <- renderUI({
                textInput("Host", label = "Host URL", 
                          value = Config()[[1]]$Host
                )
            })
            output$uiPort <- renderUI({
                selectInput("Port", 
                            label = "Port",
                            choices = c("3000","8086"), 
                            selected = Config()[[1]]$Port
                )
            })
            output$uiUser <- renderUI({
                textInput("User", 
                          label = "Login for the Host", 
                          value = Config()[[1]]$User
                )
            })
            output$uiPass <- renderUI({
                textInput("Pass", 
                          label = "Password ", 
                          value = Config()[[1]]$Pass
                )
            })
            output$uiDb   <- renderUI({
                textInput("Db", 
                          label = "SQLite database", 
                          value = Config()[[1]]$Db
                )
            })
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
                    
                    if (length(input$Down.Influx)!=0) {
                        
                        if (input$Down.Influx & !is.null(input$Down.Influx)) { 
                            
                            # detect names only if Down.Influx is checked
                            if (input$Down.Influx) {
                                
                                if (input$PROXY) { 
                                    
                                    if (input$LOGIN == "") {
                                        
                                        set_config(use_proxy(url= input$URL, 
                                                             port=as.numeric(input$PORT))) 
                                    } else {
                                        
                                        set_config(use_proxy(url= input$URL, 
                                                             port=as.numeric(input$PORT), 
                                                             username = input$LOGIN, 
                                                             password = input$PASSWORD))} 
                                } else reset_config()
                                
                                # Check connection to InfluxDB server
                                Influx.con <- httr::GET(paste0("http://",input$Host,":",input$Port,"/ping"), 
                                                        config = authenticate(user = input$User,
                                                                              password = input$Pass, 
                                                                              type = "basic"))
                                if (Influx.con$status_code != 204) {
                                    
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
                                        series <- unique(sapply(strsplit(x =series, split = ","),function(x) x[1]))                 
                                        print(series)
                                        return(series)
                                    } 
                                }    
                                
                            } else return(Config()[[1]]$Dataset)
                        } else return(Config()[[1]]$Dataset)
                    } else return(Config()[[1]]$Dataset)
                } else return(Config()[[1]]$Dataset)
                
                progress$set(message = "Connecting to Influx server", value = 1)
                
            })
            output$ASE.Box       <- renderUI({
                selectInput("Dataset", 
                            label = "Available Datasets", 
                            choices = ASE.names.Influx(),
                            selected = Config()[[1]]$Dataset
                )
            })
            observeEvent(
                input$Down.Influx, {
                    if (input$Down.Influx ) {
                        
                        updateSelectInput(session = session,
                                          inputId  = "Dataset", 
                                          label    = NULL, 
                                          choices  = ASE.names.Influx(), 
                                          selected = Config()[[1]]$Dataset
                        )
                    } else {
                        
                        updateSelectInput(session = session,
                                          inputId  = "Dataset", 
                                          label    = NULL, 
                                          choices  = ASE.names.Influx(), 
                                          selected = NULL
                        )
                    }
                }
            )
            
            output$uiInflux.TZ          <- renderUI({
                selectInput("Influx.TZ", 
                            label = "Time Zone", 
                            choices = Influx.TimeZone,
                            selected = TimeZone[match(x = Config()[[1]]$Influx.TZ, 
                                                      table = Influx.TableTZ$TimeZone)]
                )
            })
            output$uiDown_Influx <- renderUI({
                actionButton(inputId = "Down_Influx" , 
                             label = "Download Influx data")
            })
            #  NavBar"GetData", sideBar tabPanel "SOS" ----
            output$uiDown.SOS   <- renderUI({
                checkboxInput(inputId = "Down.SOS", 
                              label = "Enable SOS", 
                              value = as.logical(Config()[[1]]$Down.SOS
                              )
                )
            })
            output$uiAirsensWeb <- renderUI({
                textInput("AirsensWeb", 
                          label = "SOS Rest API URL", 
                          value = Config()[[1]]$AirsensWeb
                )
            })
            output$uiPort       <- renderUI({
                selectInput("Port", label = "Port",
                            choices = c("3000","8086"), 
                            selected = Config()[[1]]$Port
                )
            })
            ASE.names.SOS       <- reactive({
                
                # Create a Progress object
                progress <- shiny::Progress$new()
                # Make sure it closes when we exit this reactive, even if there's an error
                on.exit(progress$close())
                progress$set(message = "Connecting to SOS server", value = 0.5)
                
                if (!is.null(input$PROXY)) {
                    
                    
                    if (length(input$Down.SOS) != 0) {
                        
                        # detect names only if Down.SOS is checked
                        if (input$Down.SOS & !is.null(input$Down.SOS)) { 
                            
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
                        } else return(Config()[[1]]$AirsensEur.name)
                    }
                }
                
                progress$set(message = "Connecting to SOS server", value = 1.0)
                
            })
            output$ASE.name     <- renderUI({
                selectInput("AirsensEur.name", 
                            label = "SOS ID of the AirSensEUR box", 
                            choices = ASE.names.SOS(), 
                            selected = Config()[[1]]$AirsensEur.name
                )
            })
            output$uiSOS.tzone <- renderUI({
                selectInput("SOS.TZ", 
                            label = "Time Zone", 
                            choices = TimeZone, 
                            selected = TimeZone[match(x = Config()[[1]]$SOS.TZ, 
                                                      table = TableTZ$TimeZone)]
                )
            })
            output$uiDown_SOS <- renderUI({
                actionButton("Down_SOS"    , 
                             label = "Download SOS data")
            })
            
            #  NavBar"GetData", sideBar tabPanel "Reference" ----
            output$uiDown.Ref       <- renderUI({
                checkboxInput("Down.Ref", 
                              label = "Enable download Reference data", 
                              value = as.logical(Config()[[1]]$Down.Ref)
                )
            })
            output$uiSelected       <- renderUI({
                radioButtons(inputId = "FTPMode", 
                             label = "Selected download", 
                             choices = c("SOS","csv", "ftp"), 
                             selected = Config()[[1]]$FTPMode, 
                             inline = TRUE
                )
            })
            output$uiurlref         <- renderUI({
                textAreaInput(inputId = "urlref", 
                              label   = "URL of the ftp server with full name", 
                              value   = Config()[[1]]$urlref,
                              rows    = 4, 
                              resize = "vertical"
                              #width = "4em"
                )
            })
            output$uiRefSOSname <- renderUI({
                textInput("RefSOSname", 
                          label = "Reference station SOS Rest API URL", 
                          value = Config()[[1]]$RefSOSname
                )
            })
            Ref.SOS.name        <- reactive({
                # return a dataframe of avaialbe stations at the rest API, 1st column station name, 2nd column pollutant
                # if there is not rest API or Down_ref FALSE, return character vector Config()[[1]]$Ref.SOS.name
                
                # Create a Progress object
                progress <- shiny::Progress$new()
                # Make sure it closes when we exit this reactive, even if there's an error
                on.exit(progress$close())
                progress$set(message = "Connecting to SOS server of Reference station", value = 0.5)
                
                if (!is.null(input$PROXY)) {
                    
                    if (length(input$Down.Ref)!=0) {
                        
                        # detect names only if Down.SOS is checked
                        if (input$Down.Ref & !is.null(input$Down.Ref) & input$FTPMode == "SOS") { 
                            
                            # Detect SOS rest API only if input$RefSOSname is not empty
                            if (!is.null(input$RefSOSname)) {
                                
                                if (!(input$RefSOSname == "")) {
                                    
                                    # Set PROXY
                                    if (input$PROXY) {
                                        if (is.null(input$LOGIN)) {
                                            set_config(use_proxy(url= input$URL, 
                                                                 port=as.numeric(input$PORT)))
                                        } else {
                                            set_config( use_proxy(url= input$URL, 
                                                                  port=as.numeric(input$PORT), 
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
                                                    nrow=length(label(stations(srv))), 
                                                    byrow=T
                                                ),
                                                stringsAsFactors = FALSE
                                            )
                                        )
                                    )
                                } else return(Config()[[1]]$Ref.SOS.name)
                            } else return(Config()[[1]]$Ref.SOS.name)
                        } else return(Config()[[1]]$Ref.SOS.name)
                    }  else return(Config()[[1]]$Ref.SOS.name)
                }
                
                progress$set(message = "Connecting to SOS server of Reference station", value = 1.0)
            })
            output$uiRef.SOS.name <- renderUI({
                selectInput(inputId  = "Ref.SOS.name", 
                            label    = "SOS ID of the Reference station", 
                            choices  = Config()[[1]]$Ref.SOS.name, 
                            selected = Config()[[1]]$Ref.SOS.name
                )
                
            })
            output$uiRefPollutants <- renderUI({
                selectInput("RefPollutants", 
                            label    = "List of pollutants at the Reference station", 
                            choices  = gsub(pattern = "!", replacement = " ", x = Config()[[1]]$RefPollutants), 
                            selected = gsub(pattern = "!", replacement = " ", x = Config()[[1]]$RefPollutants),
                            multiple = TRUE
                )
            })
            output$uicoord.ref      <- renderUI({
                textInput("coord.ref", 
                          label = "Longitude and latitude of the reference station", 
                          value = Config()[[1]]$coord.ref
                )
            })
            observeEvent(
                input$Down.Ref, {
                    if (input$Down.Ref & class(Ref.SOS.name()) == "data.frame") {
                        
                        updateSelectInput(session = session,
                                          inputId  = "Ref.SOS.name", 
                                          label    = NULL, 
                                          choices  = unique(Ref.SOS.name()[,1]), 
                                          selected = Config()[[1]]$Ref.SOS.name
                        )
                    } else {
                        
                        updateSelectInput(session = session,
                                          inputId  = "Ref.SOS.name", 
                                          label    = NULL, 
                                          choices  = Ref.SOS.name(), 
                                          selected = NULL
                        )
                    }
                    if (!is.null(input$RefPollutants)) {
                        
                        if (input$Down.Ref & class(Ref.SOS.name()) == "data.frame") {
                            
                            selectInput("RefPollutants", 
                                        label    = "List of pollutants at the Reference station", 
                                        choices  = Ref.SOS.name()[Ref.SOS.name()[,1] == input$Ref.SOS.name,2], 
                                        selected = Ref.SOS.name()[Ref.SOS.name()[,1] == input$Ref.SOS.name,2],
                                        multiple = TRUE
                            )
                        } else {
                            
                            selectInput("RefPollutants", 
                                        label    = "List of pollutants at the Reference station", 
                                        choices  = gsub(pattern = "!", replacement = " ", x = Config()[[1]]$RefPollutants), 
                                        selected = gsub(pattern = "!", replacement = " ", x = Config()[[1]]$RefPollutants),
                                        multiple = TRUE
                            )
                        }
                    }
                }
            )
            observeEvent(
                input$Ref.SOS.name, {
                    
                    if (!is.null(input$PROXY)) {
                        
                        if (input$Down.Ref & input$Ref.SOS.name != "") {
                            
                            # Updating list of pollutants every time another station is selected
                            updateSelectInput(session = session,
                                              inputId = "RefPollutants", 
                                              label = NULL, 
                                              choices  = Ref.SOS.name()[Ref.SOS.name()[,1] == input$Ref.SOS.name,2], 
                                              selected = Ref.SOS.name()[Ref.SOS.name()[,1] == input$Ref.SOS.name,2]
                            )
                            
                            # updating coordinates of SOS Ref station
                            # apiEndpoint
                            apiEndpoint <- sensorweb4R::Endpoint(input$RefSOSname)
                            # Selecting service 
                            srv <- sensorweb4R::services(apiEndpoint)
                            # Selecting Station
                            sta  <- sensorweb4R::stations(srv)[grep(pattern = input$Ref.SOS.name, x = label(sensorweb4R::stations(srv)))]
                            geom <- sp::geometry(sta[1])
                            updateTextInput(session = session, 
                                            inputId = "coord.ref", 
                                            label = NULL, 
                                            value = paste0(geom@coords[1,], collapse = " "),
                                            placeholder = NULL)
                        } else {
                            
                            # if no station selected use the ADE_server.cfg file
                            updateSelectInput(session  = session,
                                              inputId  = "RefPollutants", 
                                              label    = NULL, 
                                              choices  = gsub(pattern = "!", replacement = " ", x = Config()[[1]]$RefPollutants), 
                                              selected = gsub(pattern = "!", replacement = " ", x = Config()[[1]]$RefPollutants)
                            )
                            updateTextInput(session     = session, 
                                            inputId     = "coord.ref", 
                                            label       = NULL, 
                                            value       = Config()[[1]]$coord.ref,
                                            placeholder = NULL)
                        }
                    }
                }
            )
            output$uiRefDateDownload <- renderUI(
                dateRangeInput(inputId = "RefDateDownload", 
                               label   = "Range of dates for Downloading SOS data of Reference station:",
                               format  = "yyyy-mm-dd",
                               start   = Config()[[1]]$RefDateStart,
                               end     = Config()[[1]]$RefDateEnd, 
                               weekstart = 1,
                               min = as.Date(x      = "2015-01-01", 
                                             format = "%Y-%m-%d"), 
                               max = Sys.Date()
                )
            )
            # Navbar Menu "GetData", SideBar Panel, TabPanel "Reference"-"csv"
            observe({
                
                if (input$browse == 0) return()
                if (isOS == "windows") {
                    
                    updateTextInput(session = session, 
                                    inputId = "file1",  
                                    value = choose.files(default = paste0(getwd(),"/*.csv"),
                                                         caption = "Select csv, dat or txt file",
                                                         filters =rbind(c("csv files (*.csv)","*.csv"),
                                                                        c("dat files (*.dat)","*.txt"),
                                                                        c("Text files (*.txt)","*.txt"),
                                                                        c("All files (*.*)", "*.*")
                                                         ),
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
                                    
                                    rchoose.files(default = paste0(getwd(),"/*.csv"),
                                                  caption = "Select csv, dat or txt file",
                                                  filters = rbind(c("csv files (*.csv)","*.csv"),
                                                                  c("dat files (*.dat)","*.txt"),
                                                                  c("Text files (*.txt)","*.txt"),
                                                                  c("All files (*.*)", "*.*")),
                                                  multi = FALSE,
                                                  index = 1
                                    )
                                } else {
                                    
                                    updateTextInput(session = session, 
                                                    inputId = "file1",  
                                                    value = file.choose(),
                                                    multi = FALSE,
                                                    index = 1
                                    )
                                }
                                # tk_choose.files(default = paste0(getwd(),"/*.csv"),
                                #                     caption = "Select csv, dat or txt file",
                                #                     filters =rbind(c("csv files (*.csv)","*.csv"),
                                #                                    c("dat files (*.dat)","*.txt"),
                                #                                    c("Text files (*.txt)","*.txt"),
                                #                                    c("All files (*.*)", "*.*")
                                #                                    ),
                                #                     multi = FALSE,
                                #                     index = 1
                                #                     )
                                
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
                          value = Config()[[1]]$Reference.name
                )
            })
            output$uialt.ref        <- renderUI({
                textInput("alt.ref", 
                          label = "Altitude of the reference station",
                          value = Config()[[1]]$alt.ref
                )
            })
            output$uiref.tzone      <- renderUI({
                selectInput("ref.tzone", 
                            label = "Time Zone", 
                            choices = TimeZone, 
                            selected = TimeZone[match(x = Config()[[1]]$ref.tzone, 
                                                      table = TableTZ$TimeZone)]
                )
            })
            output$uiDown_Ref       <- renderUI({
                actionButton(inputId = "Down_Ref"   , 
                             label   = "Download Reference data"
                )
            })
            
            # The "Down.Influx" control is mandatory for downloading influx data and thus the "Down_Influx" button should not be enabled if it is FALSE
            # https://cran.r-project.org/web/packages/shinyjs/vignettes/shinyjs-example.html
            observe({
                if (!is.null(input$Down.Influx)) {
                    if (input$Down.Influx) {
                        shinyjs::enable("Down_Influx")
                    } else {
                        shinyjs::disable("Down_Influx")
                    }
                }
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
            # The "Down.Ref" control is mandatory for downloading influx data and thus the "Down_SOS" button should not be enabled if it is FALSE
            # https://cran.r-project.org/web/packages/shinyjs/vignettes/shinyjs-example.html
            observe({
                if (!is.null(input$Down.Ref)) {
                    if (input$Down.Ref) {
                        
                        if (input$FTPMode == "ftp") {
                            
                            if (!is.null(input$urlref)) {
                                
                                shinyjs::enable("Down_Ref")
                            } else {
                                
                                shinyjs::disable("Down_Ref")
                            }
                        } else {
                            
                            if (input$FTPMode == "csv") {
                                
                                if (!any(is.null(c(input$file1, input$sep, input$quote)))) {
                                    
                                    shinyjs::enable("Down_Ref")
                                } else {
                                    
                                    shinyjs::disable("Down_Ref")
                                }
                            } else {
                                
                                if (input$FTPMode == "SOS") {
                                    if (!is.null(input$RefPollutants)) {
                                        
                                        shinyjs::enable("Down_Ref")
                                    } else {
                                        
                                        shinyjs::disable("Down_Ref")
                                    }
                                }    
                            }
                        }
                    } else {
                        
                        shinyjs::disable("Down_Ref")
                    }
                }
            })
            
            #  NavBar"GetData", mainTabPanel "GetData Panel" reporting of data ----
            output$PROXY            <- renderText({ paste("Enable Proxy                      : "  , input$PROXY) })
            output$URL              <- renderText({ paste("URL of your proxy                 : "  , input$URL) })
            output$PORT             <- renderText({ paste("PORT proxy                        : "  , input$PORT) })
            output$LOGIN            <- renderText({ paste("Login for the proxy               : "  , input$LOGIN) })
            output$PASSWORD         <- renderText({ paste("Password of the proxy             : "  , input$PASSWORD) })
            
            output$Down.Influx      <- renderText({ paste("Enable Influx download            : "  , input$Down.Influx) })
            output$Host             <- renderText({ paste("URL of the Influx server          : "  , input$Host) })
            output$Port             <- renderText({ paste("Port URL                          : "  , input$Port) })
            output$User             <- renderText({ paste("Login for the Server              : "  , input$User) })
            output$Pass             <- renderText({ paste("Password for the Influx server    : "  , input$Pass) })
            output$Db               <- renderText({ paste("SQLite database at the Influx server (*.db): "    , input$Db) })
            output$Dataset          <- renderText({ paste("Table in the SQLite database      : "  , input$Dataset) })
            output$Influx.TZ        <- renderText({ paste("Influx data time zone             : "  , input$Influx.TZ) })
            
            output$Down.SOS         <- renderText({ paste("Enable SOS download               : "  , input$Down.SOS) })
            output$AirsensWeb       <- renderText({ paste("URL of the SOS server             : "  , input$AirsensWeb) })
            output$AirsensEur.name  <- renderText({ paste("AirsensEur ID at the SOS server   : "  , input$AirsensEur.name) })
            output$SOS.TZ           <- renderText({ paste("SOS data time zone                : "  , input$SOS.TZ) })
            
            output$Down.Ref         <- renderText({ paste("Enable Reference data download    : "  , input$Down.Ref) })
            output$FTPMode          <- renderText({ paste("Selected download of Ref data     : "  , input$FTPMode) })
            output$urlref           <- renderText({ paste("ftp of the reference server       : "  , input$urlref) })
            output$Reference.name   <- renderText({ paste("Name of the refence station       : "  , input$Reference.name) })
            output$GDPRefSOSname       <- renderText({ paste("Reference station SOS Rest API URL: "  , input$RefSOSname) })
            output$GDPRef.SOS.name     <- renderText({ paste("SOS ID of the Reference station   : "  , input$Ref.SOS.name) }) 
            output$GDPRefPollutants    <- renderText({ paste("Pollutants at the ref. SOS station: "  , input$RefPollutants) })
            output$GDPRefDateDownload  <- renderText({ paste("Dates for Downloading Ref SOS data: "  , input$RefDateDownload) })
            output$coord.ref        <- renderText({ paste("Lat. and long. of the ref. station: "  , input$coord.ref) })
            output$alt.ref          <- renderText({ paste("Altitude of the reference station : "  , input$alt.ref) })
            output$ref.tzone        <- renderText({ paste("Time Zone of the ref. station     : "  , input$ref.tzone) })
            
            output$UserMins         <- renderText({ paste("Averaging time in min             : "  , input$UserMins) })
            output$UserMinsAvg      <- renderText({ paste("Averaging time in min for extrapolation: "  , input$UserMinsAvg) })
            output$Delay            <- renderText({ paste("Delay in min sensor vs reference  : "  , input$Delay) })
            
            Shield                  <- reactive({
                if (!is.null(input$asc.File) & length(input$asc.File)!=0) {
                    ASEPanel04Read(ASEPanel04File = file.path(DirShiny, "Shield_Files", input$asc.File))     
                }
            })
            output$asc.File         <- renderText({ paste("Sensor shield config file *.asc   : "  , input$asc.File) })
            output$Shield           <- renderTable(Shield())
            
            # mainPanel - NavBarMenu "Get Data"
            output$Influx.Content    <- renderPrint({
                
                # Making reactive to pressing button Merge and Download Influx
                # input$Merge
                # input$Down_Influx
                
                # Create a Progress object
                progress <- shiny::Progress$new()
                # Make sure it closes when we exit this reactive, even if there's an error
                on.exit(progress$close())
                progress$set(message = "Loading data", value = 0.5)
                
                # Name of Rdata File
                WDoutput = file.path(DisqueFieldtestDir(), "General_data")
                InfluxData.Rdata.file  = file.path(WDoutput , "InfluxData.Rdata")
                
                # Checking if the Rdata File exists
                if (file.exists(file.path(InfluxData.Rdata.file))) {
                    
                    load(InfluxData.Rdata.file)
                    return(InfluxData)
                    
                } else return("[shiny, Influx.Content] INFO, file InfluxData.Rdata does not exist")
            })
            output$SOS.Content    <- renderPrint({
                # Making reactive to pressing button Merge and Download SOS
                # input$Merge
                # input$Down_SOS
                
                # Create a Progress object
                progress <- shiny::Progress$new()
                # Make sure it closes when we exit this reactive, even if there's an error
                on.exit(progress$close())
                progress$set(message = "Loading data", value = 0.5)
                
                # Name of Rdata File
                WDoutput = file.path(DisqueFieldtestDir(), "General_data")
                SOSData.Rdata.file  = file.path(WDoutput , "SOSData.Rdata")
                
                # Checking if the Rdata File exists
                if (file.exists(file.path(SOSData.Rdata.file))) {
                    
                    load(SOSData.Rdata.file)
                    return(SOSData.df)
                    
                } else return("[shiny, SOS.Content] INFO, file SOSData.Rdata does not exist")
            })
            output$Ref.content <- renderPrint({
                # Making reactive to pressing button Merge and Download Ref
                # input$Merge
                # input$Down_Ref
                
                # Create a Progress object
                progress <- shiny::Progress$new()
                # Make sure it closes when we exit this reactive, even if there's an error
                on.exit(progress$close())
                progress$set(message = "Loading data", value = 0.5)
                
                # Name of Rdata File
                WDoutput = file.path(DisqueFieldtestDir(), "General_data")
                RefData.Rdata.file  = file.path(WDoutput , "RefData.Rdata")
                
                # Checking if the Rdata File exists
                if (file.exists(file.path(RefData.Rdata.file))) {
                    
                    load(RefData.Rdata.file)
                    return(RefData)
                    
                } else return("[shiny, SOS. Ref.content] INFO, file RefData.Rdata does not exist")
            })
            output$General.Content <- renderPrint({
                
                # Making reactive to pressing button Merge
                #input$Merge
                
                # Create a Progress object
                progress <- shiny::Progress$new()
                # Make sure it closes when we exit this reactive, even if there's an error
                on.exit(progress$close())
                progress$set(message = "Loading data", value = 0.5)
                
                # Name of Rdata File
                WDoutput = file.path(DisqueFieldtestDir(), "General_data")
                General.Rdata.file  = file.path(WDoutput , "General.Rdata")
                
                # Checking if the Rdata File exists
                if (file.exists(file.path(General.Rdata.file))) {
                    
                    load(General.Rdata.file)
                    return(General.df)
                    
                } else return("[Shiny,General.Content] INFO, file General.Rdata does not exist")
            })
            
            # NavBar"DataTreatment", sidebar "Filtering" ----
            output$uiFiltering      <- renderUI({
                tabsetPanel(id = "Treatments",
                            tabPanel(title = "Sensors", icon = icon("thermometer"), 
                                     do.call(tabsetPanel, 
                                             c(id = 'Filtering.Sensors',
                                               lapply(1:length(list.name.sensors()), 
                                                      function(i) {
                                                          tabPanel(
                                                              title = paste0(list.name.sensors()[i]), 
                                                              div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                                                  checkboxInput(inputId = paste0("Apply.Warm",i)  , 
                                                                                label = "Apply filter for warming of sensors"     , 
                                                                                value = FALSE)
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                                                  numericInput(inputId  = paste0("Warming",i)     , 
                                                                               label = "Number of hours of warming"              , 
                                                                               min = 0  , 
                                                                               max = 1440, step = 1  , 
                                                                               value = Config()[[2]]$hoursWarming[i.sensors()][i])
                                                              ),
                                                              hr(),
                                                              checkboxInput(inputId  = paste0("Apply.TRh",i)   , 
                                                                            label = "Apply filter on Temp and Rel. Humidity"  , 
                                                                            value = FALSE
                                                              ),
                                                              sliderInput(inputId   = paste0("Temperature",i) , 
                                                                          label = "Range of accepted temperature (Celsius degrees):"  , 
                                                                          min = -20, 
                                                                          max = 60  , 
                                                                          step = 0.5, 
                                                                          value = c(Config()[[2]]$temp.thres.min[i.sensors()][i],
                                                                                    Config()[[2]]$temp.thres.max[i.sensors()][i])
                                                              ),
                                                              sliderInput(inputId   = paste0("Humidity",i)    , 
                                                                          label = "Range of accepted relative humidity (%):", 
                                                                          min = 0  , 
                                                                          max = 100 , 
                                                                          step = 0.5  , 
                                                                          value = c(Config()[[2]]$rh.thres.min[i.sensors()][i]  ,
                                                                                    Config()[[2]]$rh.thres.max[i.sensors()][i])
                                                              ),
                                                              hr(),
                                                              div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                                                  checkboxInput(inputId = paste0("Sens.Inval.Out", i), 
                                                                                label = "Discard Invalid data"  , 
                                                                                value = Set.Time()[[1]]$Sens.Inval.Out[i.sensors()][i])
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                                                  checkboxInput(inputId = paste0("Apply.Invalid", i), 
                                                                                label = "Apply validity periods", 
                                                                                value = FALSE)
                                                                  # bsButton(
                                                                  #     inputId  = paste0("Apply.Invalid", i), 
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
                                                                  checkboxInput(inputId = paste0("Sens.rm.Out", i), 
                                                                                label = "Enable Outlier discarding"               , 
                                                                                value = Config()[[2]]$Sens.rm.Out[i.sensors()][i]
                                                                  )
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                                                  checkboxInput(inputId = paste0("Apply.S.Out", i), 
                                                                                label = "Apply parameters of filter"              , 
                                                                                value = FALSE)
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                                                  numericInput(inputId  = paste0("Sens.window", i), 
                                                                               label = "Nb of data in rolling window"                       , 
                                                                               value = Config()[[2]]$Sens.window[i.sensors()][i])),
                                                              div(style = "display: inline-block;vertical-align:top; width: 1%;",HTML("<br>")),
                                                              div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                                                  numericInput(inputId  = paste0("Sens.threshold", i),
                                                                               label = "Threshold for MAD"                                , 
                                                                               value = Config()[[2]]$Sens.threshold[i.sensors()][i])
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                                                  numericInput(inputId  = paste0("Sens.ThresholdMin", i), 
                                                                               label = "Mininimum Value median-mad"       , 
                                                                               value = Config()[[2]]$Sens.ThresholdMin[i.sensors()][i])
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 1%;",HTML("<br>")),
                                                              div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                                                  numericInput(inputId  = paste0("Sens.iterations", i), 
                                                                               label = "Nb. of iterations of the Median Average Deviation", 
                                                                               value = Config()[[2]]$Sens.iterations[i.sensors()][i])
                                                              ),
                                                              
                                                              div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                                                  numericInput(inputId  = paste0("Sens.Ymin", i), 
                                                                               label = "Minimum values in digital data series"            , 
                                                                               value = Config()[[2]]$Sens.Ymin[i.sensors()][i])
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 1%;",HTML("<br>")),
                                                              div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                                                  numericInput(inputId  = paste0("Sens.Ymax", i), 
                                                                               label = "Maximum values in digital data series"            , 
                                                                               value = Config()[[2]]$Sens.Ymax[i.sensors()][i])
                                                              )
                                                          )
                                                      }
                                               )
                                             )
                                     )
                            ),
                            tabPanel(title =  "Reference", icon = icon("calculator"),
                                     do.call(tabsetPanel, 
                                             c(id='Filtering.References',
                                               lapply(1:length(Config()[[2]]$gas.reference),
                                                      function(i) {
                                                          tabPanel(
                                                              title =Config()[[2]]$gas.reference[i],
                                                              div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                                                  bsButton(inputId = paste0("left2Out.Ref.Date", i), 
                                                                           label = "<<", 
                                                                           icon = NULL, 
                                                                           style = "default",
                                                                           size = "extra-small", 
                                                                           type = "action", 
                                                                           block = FALSE, 
                                                                           disabled = FALSE,
                                                                           value = FALSE)
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                                                  bsButton(inputId = paste0("left1Out.Ref.Date", i), 
                                                                           label = "<", 
                                                                           icon = NULL, 
                                                                           style = "default",
                                                                           size = "extra-small", 
                                                                           type = "action", 
                                                                           block = FALSE, 
                                                                           disabled = FALSE,
                                                                           value = FALSE)
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 77%;", 
                                                                  dateRangeInput(inputId = paste0("Out.Ref.Date", i), 
                                                                                 label   = "Range of dates for plotting outliers and negative values for this pollutant:",
                                                                                 format  = "yyyy-mm-dd",
                                                                                 start   = Set.Time()[[1]][i,"Out.Ref.IN"],
                                                                                 end     = as.Date(Set.Time()[[1]][i,"Out.Ref.END"]),
                                                                                 weekstart = 1,
                                                                                 min = as.POSIXct(c("2015-01-01")), 
                                                                                 max = Sys.time())
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                                                  bsButton(inputId = paste0("right1Out.Ref.Date", i),
                                                                           label = ">", 
                                                                           icon = NULL, 
                                                                           style = "default",
                                                                           size = "extra-small", 
                                                                           type = "action", 
                                                                           block = FALSE, 
                                                                           disabled = FALSE,
                                                                           value = FALSE)
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                                                  bsButton(inputId = paste0("right2Out.Ref.Date", i), 
                                                                           label = ">>", 
                                                                           icon = NULL, 
                                                                           style = "default",
                                                                           size = "extra-small", 
                                                                           type = "action", 
                                                                           block = FALSE, 
                                                                           disabled = FALSE,
                                                                           value = FALSE)
                                                              ),
                                                              checkboxInput(inputId = paste0("rm.neg", i), 
                                                                            label = "Remove negative reference values"           , 
                                                                            value = Config()[[2]]$remove.neg[i]
                                                              ),
                                                              selectInput( inputId  = paste0("Ref.unit", i), 
                                                                           label = "Unit of reference data",
                                                                           choices = choices.Ref.unit, 
                                                                           selected = Config()[[2]]$ref.unitgas[i]
                                                              ),
                                                              hr(), 
                                                              div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                                                  checkboxInput(inputId = paste0("Ref.rm.Out", i), 
                                                                                label = "Enable Outlier discarding"                        , 
                                                                                value = Config()[[2]]$Ref.rm.Out[i])
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                                                  checkboxInput(inputId = paste0("Apply.R.Out", i), 
                                                                                label = "Apply parameters of filter"                       , 
                                                                                value = FALSE)
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                                                  numericInput(inputId  = paste0("Ref.window", i), 
                                                                               label = "Nb of data in rolling window"                       , 
                                                                               value = Config()[[2]]$Ref.window[i])
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 1%;",HTML("<br>")),
                                                              div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                                                  numericInput(inputId  = paste0("Ref.threshold", i), 
                                                                               label = "Threshold for MAD"                                , 
                                                                               value = Config()[[2]]$Ref.threshold[i])
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                                                  numericInput(inputId  = paste0("Ref.ThresholdMin", i), 
                                                                               label = "Mininimum Value median-mad", 
                                                                               value = Config()[[2]]$Ref.ThresholdMin[i])
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 1%;",HTML("<br>")),
                                                              div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                                                  numericInput(inputId  = paste0("Ref.iterations", i), 
                                                                               label = "Nb. of iterations of the Median Average Deviation", 
                                                                               value = Config()[[2]]$Ref.iterations[i])
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                                                  numericInput(inputId  = paste0("Ref.Ymin", i), 
                                                                               label = "Minimum values in digital data series"            , 
                                                                               value = Config()[[2]]$Ref.Ymin[i])
                                                              ),
                                                              div(style = "display: inline-block;vertical-align:top; width: 1%;",HTML("<br>")),
                                                              div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                                                  numericInput(inputId  = paste0("Ref.Ymax", i), 
                                                                               label = "Maximum values in digital data series"            , 
                                                                               value = Config()[[2]]$Ref.Ymax[i])
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
            output$uiSelectSensors <- renderUI({radioButtons(inputId = "SelSensors", label = "Sensor:", selected = list.name.sensors()[1],
                                                             c(list.name.sensors()[1],list.name.sensors()[2], list.name.sensors()[3], list.name.sensors()[4])
                                                             , inline = TRUE)})
            
            # NavBar"DataTreatment", sidebar "Calib." ----
            output$uiCalib         <- renderUI({
                do.call(tabsetPanel, 
                        c(id = 'Calib.Sensors',
                          lapply(1:length(list.name.sensors()), 
                                 function(i) {
                                     tabPanel(
                                         title = paste0(list.name.sensors()[i]),
                                         div(style = "display: inline-block;vertical-align:top; width: 1%;",HTML("<br>")),
                                         selectInput(  inputId  = paste0("Sens", i), 
                                                       label    = "List of covariates to plot"         , 
                                                       choices  = Covariates(), 
                                                       selected = as.vector(Config()[[3]][[list.name.sensors()[i]]]$Effects), 
                                                       multiple = TRUE
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                             selectInput(  inputId  = paste0("Sens.raw.unit", i), 
                                                           label    = "Raw unit of sensor data"                     , 
                                                           choices  = c("V","nA"), 
                                                           selected = Config()[[2]]$Sens.raw.unit[i.sensors()][i])
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                             selectInput(  inputId  = paste0("Calibration", i), 
                                                           label    = "Model for calibration"            , 
                                                           choices  = na.omit(Models) , 
                                                           selected = Config()[[2]]$mod.eta.model.type[i.sensors()][i])
                                         ),
                                         selectInput(  inputId  = paste0("CovMod", i), 
                                                       label    = "List of covariates to calibrate"         , 
                                                       choices  = Covariates.Model()[-which(Covariates.Model()   == paste0(list.name.sensors()[i],"_volt") |
                                                                                                Covariates.Model() == paste0(list.gas.sensors()[i],"_modelled") )], 
                                                       selected = as.vector(Config()[[4]][[list.name.sensors()[i]]]$Effects), 
                                                       multiple = TRUE
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 94%;",
                                             selectInput(  inputId  = paste0("Cal", i), 
                                                           label    = "Select a previous calibration "              , 
                                                           choices  = substr(list.files(path = file.path(DisqueFieldtestDir(),"Models"), 
                                                                                        pattern = glob2rx(paste0(Config()[[1]]$AirsensEur.name,"*",list.name.sensors()[i],"*",".rds"))), 
                                                                             start = nchar(paste0(Config()[[1]]$AirsensEur.name,"__",list.name.sensors()[i],"__")) + 1,
                                                                             stop  = nchar(list.files(path = file.path(DisqueFieldtestDir(),"Models"), 
                                                                                                      pattern = glob2rx(paste0(Config()[[1]]$AirsensEur.name,"*",list.name.sensors()[i],"*",".rds"))))
                                                           ), 
                                                           selected = Config()[[2]]$Cal.func[i.sensors()][i])
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             # checkboxInput(inputId  = paste0("DelModel", i), 
                                             #               label    = "Del", 
                                             #               value = FALSE, 
                                             #               width = NULL),
                                             bsButton(inputId  =  paste0("DelModel", i), 
                                                      label    = "Del", 
                                                      icon     = NULL, 
                                                      style    = "default",
                                                      size     = "extra-small", 
                                                      type     = "action", 
                                                      block    = FALSE, 
                                                      disabled = FALSE,
                                                      value    = FALSE)
                                         ),
                                         checkboxInput(inputId = paste0("Neg.mod", i), 
                                                       label   = "Discard negative extrapolated data?"         , 
                                                       value   = Config()[[2]]$Neg.mod[i.sensors()][i]
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                             checkboxInput(inputId = paste0("Apply.conv", i), 
                                                           label   = "Force Conversion to V/nA"                , 
                                                           value   = FALSE)
                                             # bsButton(inputId  =  paste0("Apply.conv", i), 
                                             #          label    = "Force Conversion to V/nA", 
                                             #          icon     = NULL, 
                                             #          style    = "default",
                                             #          size     = "small", 
                                             #          type     = "toggle", 
                                             #          block    = FALSE, 
                                             #          disabled = FALSE,
                                             #          value    = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 49%;",
                                             checkboxInput(inputId = paste0("Apply.cal", i), 
                                                           label   = "Apply Calibration"                          , 
                                                           value   = FALSE)
                                             # bsButton(inputId  =  paste0("Apply.cal", i), 
                                             #          label    = "Apply Calibration", 
                                             #          icon     = NULL, 
                                             #          style    = "default",
                                             #          size     = "small", 
                                             #          type     = "toggle", 
                                             #          block    = FALSE, 
                                             #          disabled = FALSE,
                                             #          value    = FALSE)
                                         ),
                                         br(),
                                         radioButtons( inputId  = paste0("Cal.Line", i), 
                                                       label    = "Method of extrapolation"                     , 
                                                       choices  = list("New calibration with current data","Previous calibration","Calibration with slope and intercept below"), 
                                                       selected = Config()[[2]]$Cal.Line[i.sensors()][i]),
                                         div(style = "display: inline-block;vertical-align:top; width: 33%;",
                                             selectInput(  inputId  = paste0("Sens.unit", i),
                                                           label = "Unit for calibrated sensor"              , 
                                                           choices = c("ppb","ug/m3","ppm","mg/m3"), 
                                                           selected = Config()[[2]]$Sens.unit[i.sensors()][i])
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 64%;",
                                             selectInput(  inputId  = paste0("uxi", i), 
                                                           label = "u(xi), random uncertainty of the reference data"              , 
                                                           choices = c(seq(from = 0, to = 0.2, by = 0.01), seq(from = 0.2, to = 10, by = 0.1)), 
                                                           selected = Config()[[2]]$uxi[i.sensors()][i])
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                             numericInput(inputId = paste0("Slope", i), 
                                                          label   = "Slope for linear calibration"                , 
                                                          value  = Config()[[2]]$Slope[i.sensors()][i], step = 0.000001)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 1%;",HTML("<br>")),
                                         div(style = "display: inline-block;vertical-align:top; width: 47%;",
                                             numericInput(inputId = paste0("Intercept", i), 
                                                          label = "Intercept for calibration"            , 
                                                          value = Config()[[2]]$Intercept[i.sensors()][i], step = 0.0001)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 100%;",
                                             selectInput(  inputId  = paste0("Comparison", i), 
                                                           label = "Model chosen for comparison with reference data"  , 
                                                           choices = "Linear", 
                                                           selected = Config()[[2]]$eta.model.type[i.sensors()][i])
                                         )
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
                          lapply(1:length(list.name.sensors()), 
                                 function(i) {
                                     tabPanel(
                                         title = paste0(list.name.sensors()[i]),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("left2Out.Valid", i), label = "<<", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                             bsButton(inputId = paste0("left1Out.Valid", i), label = "<", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 77%;", 
                                             dateRangeInput(inputId = paste0("Valid", i), 
                                                            label   = "Range of valid dates (invalid are hidden):",
                                                            format  = "yyyy-mm-dd",
                                                            start   = Set.Time()[[1]][i.sensors()[i],"Valid.IN"],
                                                            end     = as.Date(Set.Time()[[1]][i.sensors()[i],"Valid.END"]),
                                                            weekstart = 1,
                                                            min = Set.Time()[[1]][i.sensors()[i],"Valid.IN"], 
                                                            max = Set.Time()[[1]][i.sensors()[i],"Valid.END"] 
                                             )
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                             bsButton(inputId = paste0("right1Out.Valid", i), label = ">", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("right2Out.Valid", i), label = ">>", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         br(),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("left2Out.Sens.Date", i), label = "<<", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                             bsButton(inputId = paste0("left1Out.Sens.Date", i), label = "<", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 77%;", 
                                             dateRangeInput(inputId = paste0("Out.Sens.Date", i), 
                                                            label   = "Range of dates for plotting outliers:",                             #### Need the message for the i = 1, "Range of dates for plotting RawData, DataTable, Retrieved, Warming, Temp&Humid, Invalids and outliers:"
                                                            format  = "yyyy-mm-dd",
                                                            start   = Set.Time()[[1]]$Out.Sens.IN[i.sensors()][i], 
                                                            end     = as.Date(Set.Time()[[1]]$Out.Sens.END[i.sensors()][i]),
                                                            weekstart = 1,
                                                            min = Set.Time()[[1]][i.sensors()[i],"Valid.IN"], 
                                                            max = Set.Time()[[1]][i.sensors()[i],"Valid.END"] 
                                             )
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                             bsButton(inputId = paste0("right1Out.Sens.Date", i), label = ">", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("right2Out.Sens.Date", i), label = ">>", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("left2Date", i), label = "<<", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                             bsButton(inputId = paste0("left1Date", i), label = "<", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 77%;", 
                                             dateRangeInput(inputId = paste0("Date", i), 
                                                            label   = "Range of dates for plotting covariates:",
                                                            format  = "yyyy-mm-dd",
                                                            start   = Set.Time()[[1]]$Cov.Date.IN[i.sensors()][i], 
                                                            end     = Set.Time()[[1]]$Cov.Date.END[i.sensors()][i],
                                                            weekstart = 1,
                                                            min = Set.Time()[[1]]$Valid.IN[i.sensors()][i], 
                                                            max = Set.Time()[[1]]$Valid.END[i.sensors()][i] 
                                             )
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                             bsButton(inputId = paste0("right1Date", i), label = ">", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("right2Date", i), label = ">>", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         hr(), 
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("left2DateCal", i), label = "<<", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                             bsButton(inputId = paste0("left1DateCal", i), label = "<", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 57%;", 
                                             dateRangeInput(inputId = paste0("DateCal", i), 
                                                            label   = "Range of dates for calibration:",
                                                            format  = "yyyy-mm-dd",
                                                            start   = Set.Time()[[1]]$DateCal.IN[i.sensors()][i], 
                                                            end     = Set.Time()[[1]]$DateCal.END[i.sensors()][i],
                                                            weekstart = 1,
                                                            min = Set.Time()[[1]]$Valid.IN[i.sensors()][i], 
                                                            max = Set.Time()[[1]]$Valid.END[i.sensors()][i] 
                                             )
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 6%;",
                                             bsButton(inputId = paste0("DateCALCal", i), label = "Cal", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 6%;",
                                             bsButton(inputId = paste0("DateCALCovCal", i), label = "Cov", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 6%;",
                                             bsButton(inputId = paste0("DateCALExtCal", i), label = "Ext", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                             bsButton(inputId = paste0("right1DateCal", i), label = ">", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("right2DateCal", i), label = ">>", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("left2DatePlotCal", i), label = "<<", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                             bsButton(inputId = paste0("left1DatePlotCal", i), label = "<", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 77%;", 
                                             dateRangeInput(inputId = paste0("DatePlotCal", i), 
                                                            label   = "Range of dates for plotting calibration:",
                                                            format  = "yyyy-mm-dd",
                                                            start   = Set.Time()[[1]]$DatePlotCal.IN[i.sensors()][i], 
                                                            end     = Set.Time()[[1]]$DatePlotCal.END[i.sensors()][i],
                                                            weekstart = 1,
                                                            min = Set.Time()[[1]]$Valid.IN[i.sensors()][i], 
                                                            max = Set.Time()[[1]]$Valid.END[i.sensors()][i] 
                                             )
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                             bsButton(inputId = paste0("right1DatePlotCal", i), label = ">", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("right2DatePlotCal", i), label = ">>", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         hr(),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("left2DateMeas", i), label = "<<", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                             bsButton(inputId = paste0("left1DateMeas", i), label = "<", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 77%;", 
                                             dateRangeInput(inputId = paste0("DateMeas", i), 
                                                            label   = "Range of dates for extrapolation:",
                                                            format  = "yyyy-mm-dd",
                                                            start   = Set.Time()[[1]]$Datemeas.IN[i.sensors()][i], 
                                                            end     = Set.Time()[[1]]$Datemeas.END[i.sensors()][i],
                                                            weekstart = 1,
                                                            min = Set.Time()[[1]]$Valid.IN[i.sensors()][i], 
                                                            max = Set.Time()[[1]]$Valid.END[i.sensors()][i] 
                                             )
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                             bsButton(inputId = paste0("right1DateMeas", i), label = ">", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("right2DateMeas", i), label = ">>", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("left2DatePlotMeas", i), label = "<<", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                             bsButton(inputId = paste0("left1DatePlotMeas", i), label = "<", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 77%;", 
                                             dateRangeInput(inputId = paste0("DatePlotMeas", i), 
                                                            label   = "Range of dates for plotting extrapolated data:",
                                                            format  = "yyyy-mm-dd",
                                                            start   = Set.Time()[[1]]$DatePlotmeas.IN[i.sensors()][i], 
                                                            end     = Set.Time()[[1]]$DatePlotmeas.END[i.sensors()][i],
                                                            weekstart = 1,
                                                            min = Set.Time()[[1]]$Valid.IN[i.sensors()][i], 
                                                            max = Set.Time()[[1]]$Valid.END[i.sensors()][i] 
                                             )
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 4%;",
                                             bsButton(inputId = paste0("right1DatePlotMeas", i), label = ">", icon = NULL, style = "default",
                                                      size = "extra-small", type = "action", block = FALSE, disabled = FALSE,
                                                      value = FALSE)
                                         ),
                                         div(style = "display: inline-block;vertical-align:top; width: 5%;",
                                             bsButton(inputId = paste0("right2DatePlotMeas", i), label = ">>", icon = NULL, style = "default",
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
            updateTabsetPanel(session, 
                              inputId = "Calib_data", 
                              selected = "Filtering"
            )
            
            # Goto GetData in NavBar
            updateNavbarPage(session, 
                             inputId = "ASE", 
                             selected = "GetData"
            )
            
            # Opening at least once the TabPanel of sideBarLayout Getdata to update all parameters for download
            updateTabsetPanel(session, 
                              inputId = "ForServers", 
                              selected = "tPRef"
            )
            
            progress$set(message = "Selecting AirSensEUR Box", 
                         value = 1)
            
        }
        
    })
    
    # Reactive Shield(), NavBar"SelectASE" ----
    Shield                  <- reactive({
        if (!is.null(input$asc.File) & length(input$asc.File) != 0) {
            ASEPanel04Read(ASEPanel04File = file.path(DirShiny, "Shield_Files", input$asc.File))     
        }
    })
    
    # NavBar"SelectASE", mainTabPanel "Push data" ----
    output$Pushdata.cfg      <- renderTable(Pushdata_cfg())
    Pushdata_cfg             <- reactive({
        
        # Make it reactive to Save
        input$Save
        
        # Create a Progress object
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        progress$set(message = "Reading Server File", value = 0.5)
        
        
        if (file.exists(cfg_file.List())) {
            Servers_file       <- transpose(read.table(file = file.path(DisqueFieldtestDir.List(),"General_data", paste0(ASE_name.List(),"_Servers.cfg")), 
                                                       na.strings       = c("NA","NaN", " "), 
                                                       header           = FALSE, 
                                                       row.names        = NULL, 
                                                       stringsAsFactors = FALSE))
            names(Servers_file) <- Servers_file[1,]; 
            Servers_file <- Servers_file[-1,]; 
            row.names(Servers_file) <- NULL
            Servers_file <- Servers_file[,which(names(Servers_file) %in% c("UserMins","UserMinsAvg","Delay",
                                                                           "PROXY","URL","PORT","LOGIN","PASSWORD",
                                                                           "Down.Influx","Host","Port","User","Pass","Db","Dataset","Influx.TZ",
                                                                           "Down.SOS","AirsensWeb","AirsensEur.name","SOS.TZ",
                                                                           "Down.Ref","FTPMode","urlref","Reference.name",
                                                                           "RefSOSname","Ref.SOS.name","RefPollutants","RefDateDownload",
                                                                           "coord.ref","alt.ref","ref.tzone",
                                                                           "asc.File"))]
            F <- cbind(colnames(Servers_file),transpose(Servers_file))
        } 
        
        progress$set(message = "Reading Server File", value = 1)
        
        return(F)
    })
    # NavBar"SelectASE", mainTabPanel "Filtering" ----
    output$FilteringSensor   <- renderTable(Read_cfg()[[2]], 
                                            rownames = TRUE, 
                                            digits = 1)
    output$FilteringRef      <- renderTable(Read_cfg()[[3]], 
                                            rownames = TRUE,
                                            digits = 1)
    # NavBar"SelectASE", mainTabPanel "Calibration" ----
    # Reactive cfgValues
    Read_cfg          <- reactive({
        
        # Create a Progress object
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        progress$set(message = "Reading Config File", value = 0.5)
        
        if (file.exists(cfg_file.List())) {
            
            Config.List() # making Read_cfg() dependent on change of  Config.List() and hence input$asc.File
            cfg_file <- read.table(file = cfg_file.List(), 
                                   na.strings       = c("NA","NaN", " "), 
                                   header           = TRUE, 
                                   stringsAsFactors = FALSE)
            colnames(cfg_file) <- cfg_file[row.names(cfg_file)== "name.gas",""]
            G <- list(cfg_file[c("name.sensor","Cal.func","Cal.Line", "mod.eta.model.type", "Slope", "Intercept", "Sens.raw.unit", "Sens.unit","Neg.mod",
                                 "eta.model.type"),!is.na(cfg_file["name.sensor",])],
                      cfg_file[c("name.sensor","hoursWarming", "temp.thres.min", "temp.thres.max", "rh.thres.min", "rh.thres.max", "Sens.Inval.Out","Sens.rm.Out", 
                                 "Sens.window", "Sens.threshold", "Sens.Ymin", "Sens.Ymax", "Sens.ThresholdMin", "Sens.iterations"),!is.na(cfg_file["name.sensor",])],
                      cfg_file[c("remove.neg", "ref.unitgas", "uxi","Ref.rm.Out", "Ref.window", "Ref.threshold", "Ref.Ymin", "Ref.Ymax", "Ref.ThresholdMin", "Ref.iterations", 
                                 "gas.reference", "gas.reference2use"),]
            )
        }
        
        progress$set(message = "Reading Config File", 
                     value = 1)
        return(G)
        
    })
    output$Calib.cfg      <- renderTable(Read_cfg()[[1]], 
                                         rownames = TRUE, 
                                         digits = -4)
    # NavBar"SelectASE", mainTabPanel "SetTime" ----
    output$SetTime.cfg       <- renderTable(SetTime_cfg(), 
                                            rownames = TRUE)
    SetTime_cfg              <- reactive({
        
        # Create a Progress object
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        progress$set(message = "Reading Date/Time File", value = 0.5)
        
        if (file.exists(cfg_file.List())) {
            
            SETTIME_file        <- read.table(file = file.path(DisqueFieldtestDir.List(),"General_data", paste0(ASE_name.List(),"_SETTIME.cfg")), 
                                              na.strings       = c("NA","NaN", " "), 
                                              header           = TRUE, 
                                              stringsAsFactors = FALSE)
            colnames(SETTIME_file) <- SETTIME_file[rownames(SETTIME_file) == "name.gas",]
            E <- SETTIME_file[which(rownames(SETTIME_file) %in% c("name.sensor", "Sens.Inval.Out","Apply.Invalid",
                                                                  "Out.Sens.IN","Out.Sens.END",
                                                                  "Out.Ref.IN","Out.Ref.END", 
                                                                  "Valid.IN", "Valid.END",
                                                                  "Cov.Date.IN", "Cov.Date.END",
                                                                  "DateCal.IN", "DateCal.END", 
                                                                  "DatePlotCal.IN", "DatePlotCal.END",
                                                                  "Datemeas.IN", "Datemeas.END",
                                                                  "DatePlotmeas.IN", "DatePlotmeas.END")
            ),
            ]
        } else E <- NULL
        
        progress$set(message = "Reading Date/Time File", value = 1)
        return(E)
    })
    
    #  Observer asc.File of NavBar "Getdata", sideBarLayout: TimeShield - Proxy - InfluxDB - SOS -Refer.
    #  TimeShield
    observeEvent(
        input$asc.File, { # update the name of asc.File in ASE_name.cfg in order to update the name of sensors
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            progress$set(message = "Reading schield Config File", 
                         value = 0.5)
            
            # Reading the _Servers.cfg file
            File_Server_cfg <- file.path(DisqueFieldtestDir(), "General_data", paste0(ASE_name(),"_Servers.cfg"))
            if (file.exists(File_Server_cfg)) {
                
                cfg <- transpose(read.table(file = File_Server_cfg, 
                                            na.strings=c("NA","NaN", " "), 
                                            header = FALSE, 
                                            row.names = NULL, 
                                            stringsAsFactors = FALSE ))
                row.names(cfg) <- NULL; names(cfg) <- cfg[1,]; cfg <- cfg[-1,]; 
                cfg<-as.data.frame(cfg, stringsAsFactors = FALSE); 
                row.names(cfg) <- NULL
                cat(paste0("[CONFIG] Info, the config file ", File_Server_cfg, " for the configuration of servers  exists"), sep = "\n")
                Vector.type <- c("PROXY","Down.Influx","Down.SOS","Down.Ref"); 
                for (i in Vector.type) if (i %in% colnames(cfg)) cfg[,i] <- as.logical(cfg[,i])
                Vector.type <- c("PORT","Port","UserMins","UserMinsAvg","Delay");
                for (i in Vector.type) {if (i %in% colnames(cfg)) cfg[,i] <- as.numeric(cfg[,i])}
                
            } else stop(cat(paste0("[CONFIG] The file of server configuration for AirSensEUR: ", File_Server_cfg, " does not exist. Please change File_Sensor.config <- FALSE "), sep = "\n"))
            # Update
            cfg$asc.File <- input$asc.File
            # save new file
            write.table(t(cfg), 
                        file = file.path(DisqueFieldtestDir(),"General_data",paste0(ASE_name(),"_Servers.cfg")), 
                        col.names = FALSE)
            cat(paste0("[shiny, asc.File] INFO, ", paste0(ASE_name(),"_Servers.cfg")," config file  saved in directory General_data.\n"))
            
            # now update the config()
            source(file.path(DirShiny, input$Selected))
            
            # update the choice of sensor names for the radiotbutton selecting sensors
            updateRadioButtons(session, "Sensors", 
                               label = "Sensors", 
                               choices = list.name.sensors(), 
                               inline = TRUE)
            
            progress$set(message = "Reading schield Config File", value = 1)
            
        })
    
    # Reactive InfluxDB ----
    INFLUX <- reactive({
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        progress$set(message = "[shiny, INFLUX()] INFO, Checking/Downloading Connection to INFLUX", value = 0.5)
        
        # making the function reactive to Shield()
        asc.File <- Shield()
        # making the function reactive to action button "Download Reference data
        input$Down_Influx
        
        A <- INFLUXDB(
            WDoutput       = file.path(DisqueFieldtestDir(),"General_data"), 
            DownloadSensor = DownloadSensor(),
            UserMins    = as.numeric(input$UserMins), 
            PROXY       = input$PROXY,
            URL         = input$URL,
            PORT        = input$PORT,
            LOGIN       = input$LOGIN, 
            PASSWORD    = input$PASSWORD,
            Down.Influx = input$Down.Influx,
            Host        = input$Host,
            Port        = input$Port,
            User        = input$User, 
            Pass        = input$Pass, 
            Db          = input$Db, 
            Dataset     = input$Dataset,
            Influx.TZ   = input$Influx.TZ,
            name.SQLite = file.path(DisqueFieldtestDir(),"General_data","airsenseur.db"), 
            name.SQLite.old = file.path(DisqueFieldtestDir(),"General_data","airsenseur_old.db"), 
            sens2ref = Config()[[2]], 
            asc.File = asc.File)
        
        progress$set(message = "[shiny, INFLUX()] INFO, Checking/Downloading Connection to INFLUX", value = 1)
        return(A)
        
        # var.names.meteo     <- InfluxData[[2]]
        # var.name.GasSensors <- InfluxData[[3]]
        # var.names.sens      <- InfluxData[[4]]
        # InfluxData          <- InfluxData[[1]]
        # return(InfluxData)
    })
    
    observeEvent(input$Down_Influx, {
        # dependent on function INFLUX()
        str(INFLUX())
        updateTabsetPanel(session, "ForServers", selected = "tPRef")
    })
    # Reactive SOSData ----
    SOS_T <- reactive({
        # Create a Progress object
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        progress$set(message = "[shiny, SOS_T()] INFO, Checking/Downloading Connection to SOS", value = 0.5)
        
        # making the function reactive to action button "Download SOS data"
        input$Down_SOS
        
        B <- SOS(WDoutput            = file.path(DisqueFieldtestDir(), "General_data"),
                 DownloadSensor      = DownloadSensor(), 
                 Down.SOS            = input$Down.SOS, 
                 AirsensEur.name     = input$AirsensEur.name,
                 UserMins            = as.numeric(input$UserMins),
                 AirsensWeb          = input$AirsensWeb,
                 Duration            = 1,
                 sens2ref            = Config()[[2]]
        )
        
        progress$set(message = "[shiny, SOS_T()] INFO, Checking/Downloading Connection to SOS", value = 1)
        return(B)
        # SOSData             <- SOS_T()[[1]]
        # var.names.meteo     <- SOS_T()[[2]]
        # var.name.GasSensors <- SOS_T()[[3]]
        # var.names.sens      <- SOS_T()[[4]]
    })
    observeEvent(input$Down_SOS, {
        str(SOS_T())
        updateTabsetPanel(session, "ForServers", selected = "tpRef")
    })
    # Reactive REFDATA ----
    REFDATA <- reactive(
        # {
        #     input$Down_Ref
        #     input$Merge
        #     
        # }, 
        {
            
            # making the function reactive to action button "Download Reference data
            input$Down_Ref
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            progress$set(message = "[shiny, REFDATA()] INFO, Loading Reference data", value = 0.5)
            
            # Checking if there are several ftp url
            if (grepl(pattern = ",", x = input$urlref)) urlref = unlist(strsplit(gsub(pattern= " ","",x = input$urlref), split = ",")  ) else urlref = gsub(pattern = " ","",x = input$urlref)
            C <- REF(DownloadSensor     = DownloadSensor(), 
                     AirsensEur.name    = input$AirsensEur.name, 
                     DisqueFieldtestDir = (DisqueFieldtestDir()),
                     UserMins           = as.numeric(input$UserMins), 
                     Down.Ref           = input$Down.Ref, 
                     ref.tzone          = input$ref.tzone,
                     InfluxData         = INFLUX()[[1]], 
                     SOSData            = SOS_T()[[1]],
                     Reference.name     = input$Reference.name,
                     urlref             = urlref,
                     sens2ref           = Config()[[2]],
                     FTPMode            = input$FTPMode, 
                     Ref.SOS.name       = input$Ref.SOS.name, 
                     RefSOSname         = input$RefSOSname,
                     RefSOSDateIN       = as.Date(input$RefDateDownload[1], format = "%Y-%m-%d"),
                     RefSOSDateEND      = as.Date(input$RefDateDownload[2], format = "%Y-%m-%d"),
                     csvFile            = input$file1,
                     csvFile.sep        = input$sep,
                     csvFile.quote      = input$quote
            )
            progress$set(message = "[shiny, REFDATA()] INFO, Loading Reference data", value = 1)
            return(C)
            #RefData       <- REFDATA()[[3]]
            # variables in <- REFDATA()[[2]]
            
        })
    observeEvent(input$Down_Ref, {
        str(REFDATA())
        # Goto GetData in NavBar
        updateNavbarPage(session, "ASE", selected = "DataTreatment")
    })
    
    # NavBar"Data Treatment", SideBar Button "Merge" ----
    # The "Merge" button is not enable if sideBar tabPanel "Calib" and "SetTime" are not opened 
    # https://cran.r-project.org/web/packages/shinyjs/vignettes/shinyjs-example.html
    observe({
        if ( !is.null(input$Sens1) & !is.null(input$Valid1) ) {
            shinyjs::enable("Merge")
            
            # Automatic Merging Influx, SOS and Ref if General.Data File does not exists
            # if (input$Merge == 0 & 
            #    !Downloaded()[Downloaded()$DataSets == "General","Exists"] &
            #    Downloaded()[Downloaded()$DataSets == "ReferenceData","Exists"] & 
            #    (Downloaded()[Downloaded()$DataSets == "InfluxData","Exists"] | Downloaded()[Downloaded()$DataSets == "SOSData","Exists"])
            # ) 
            if (input$Merge == 0) click(id = "Merge")
            
        } else {
            shinyjs::disable("Merge")
        }
    })
    # The "Save" button is not enable if sideBar tabPanel "Calib" and "SetTime" are not opened  and the button "Merge" is not clicked to create the Genernal dataFrame
    # https://cran.r-project.org/web/packages/shinyjs/vignettes/shinyjs-example.html
    observe({
        if ( !is.null(input$Sens1) & !is.null(input$Valid1) & input$Merge > 0 ) {
            shinyjs::enable("Save")
        } else {
            shinyjs::disable("Save")
        }
    })
    
    # Checking that all Tabpanel of "getdata" are opened once
    observe({
        if (input$ASE ==  "GetData" ) {
            if (is.null(input$PROXY))                             updateTabsetPanel(session, inputId = "ForServers"  , selected = "tPProxy")
            if (is.null(input$SOS.TZ) | is.null(input$Influx.TZ)) updateTabsetPanel(session, inputId = "ForServers"  , selected = "tPSensordown")
            if (is.null(input$UserMins))                          updateTabsetPanel(session, inputId = "ForServers"  , selected = "tPTimeshield")
            if (is.null(input$Down.Ref))                          updateTabsetPanel(session, inputId = "ForServers"  , selected = "tPRef")
        }
    })
    observe({
        if (input$ForServers ==  "tPRef" ) {
            if (is.null(input$file1))      updateTabsetPanel(session, inputId = "DownloadMode", selected = "csv")
            if (is.null(input$RefSOSname)) updateTabsetPanel(session, inputId = "DownloadMode", selected = "SOS")
        }
    })
    observe({
        if (input$ForServers ==  "tPSensordown" ) {
            if (is.null(input$Host))       updateTabsetPanel(session, inputId = "SensorDown"  , selected = "tPInflux")
            if (is.null(input$Down.SOS))   updateTabsetPanel(session, inputId = "SensorDown"  , selected = "tPSOS")
        }
    })
    
    # Checking that NavBar "Data Treatment" is opened once
    # observe({
    #     if (input$ASE ==  "GetData" ) if (is.null(input$Out.Sens.Date1))    updateNavbarPage(session, inputId = "ASE", selected = "Data Treatment")
    # })
    
    # Checking that all Tabpanel of "Data Treatment" are opened once
    observe({
        if (input$ASE ==  "Data Treatment") {
            if (is.null(input$Sens1))                  updateTabsetPanel(session, inputId = "Calib_data", selected = "Calib")
            if (is.null(input$left2Out.Sens.Date1))    updateTabsetPanel(session, inputId = "Calib_data", selected = "SetTime")
        }
    })
    
    observeEvent(input$Merge,{
        
        # Hiding unecessary CovMod and Multivariates MainTabPanel ----
        observeEvent(input$Sensors, {
            
            # Detecting seleted sensor
            k    <- match(x = input$Sensors, table = list.name.sensors())
            
            if (!is.null(input[[paste0("Calibration",k)]])) {
                if (input[[paste0("Calibration",k)]] == "MultiLinear") {
                    
                    shinyjs::show(id = paste0("CovMod",k) )
                    shinyjs::show(id = "Multivariates" )
                    #showTab(inputId = "TabCalibration", target = "Multivariates")
                } else {
                    
                    shinyjs::hide(id = paste0("CovMod",k) )
                    shinyjs::hide(id = "Multivariates"  )
                    #hideTab(inputId = "TabCalibration", target = "Multivariates")
                } 
            }
        },
        ignoreNULL = TRUE
        )
        
        # Updating date of al dateRange when date of input$Valid is changed and moving buttons ----
        observeEvent({
            # inputs to react on
            unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("Valid", i)]]))
        },{
            
            # index k of selected sensor in list.namesensors(())
            Sens.Index    <- match(x = input$SetTime.Sensors, table = list.name.sensors())
            
            if (!any(is.na(input[[paste0("Valid", Sens.Index)]]))) {
                
                if (input[[paste0("Valid", Sens.Index)]][1] >= min.General.date() & input[[paste0("Valid", Sens.Index)]][1] <= max.General.date()) {
                    
                    MINI <- input[[paste0("Valid", Sens.Index)]][1] 
                    
                } else  MINI <- min.General.date()
                if (input[[paste0("Valid", Sens.Index)]][2] >= min.General.date() & input[[paste0("Valid", Sens.Index)]][2] <= max.General.date()) {
                    
                    MAXI <- input[[paste0("Valid", Sens.Index)]][2] 
                    
                } else MAXI <- max.General.date()
                
                dateRangeInput(inputId = paste0("Valid",Sens.Index), 
                               label   = NULL,
                               format  = "yyyy-mm-dd",
                               start   = NULL,
                               end     = NULL,
                               weekstart = 1,
                               min = min.dateRange(DF$General$date, 
                                                   Value1 = MINI,
                                                   Value2 = MAXI
                               ), 
                               max = max.dateRange(DF$General$date,
                                                   Value1 = MINI, 
                                                   Value2 = MAXI
                               ) 
                )
                
                # updating the dateRange according to input$valid, using only the date
                MINI <- as.Date(input[[paste0("Valid", Sens.Index)]][1])
                MAXI <- as.Date(input[[paste0("Valid", Sens.Index)]][2])
                List.dateRange <- c(paste0("Out.Ref.Date",Sens.Index),
                                    paste0("Out.Sens.Date",Sens.Index),
                                    paste0("Date",Sens.Index),
                                    paste0("DateCal",Sens.Index),
                                    paste0("DatePlotCal",Sens.Index),
                                    paste0("DateMeas",Sens.Index),
                                    paste0("DatePlotMeas",Sens.Index)
                )
                for (i in List.dateRange) {
                    
                    # input[[i]][1] outside date range and input[[i]][2] within date range
                    if (!any(is.na(input[[i]]))) {
                        if ((input[[i]][1] <  MINI | input[[i]][1] >  MAXI) &
                            (input[[i]][2] >= MINI &  input[[i]][2] <= MAXI) ) {
                            updateDateRangeInput(session, 
                                                 inputId = i, 
                                                 label =  NULL,
                                                 start   = MINI,
                                                 end     = NULL,
                                                 min     = MINI, 
                                                 max     = MAXI
                            ) 
                        } else {
                            
                            # input[[i]][1] and input[[i]][2] outside date range 
                            if ((input[[i]][1] < MINI | input[[i]][1] > MAXI) &
                                (input[[i]][2] < MINI | input[[i]][2] > MAXI) ) {
                                updateDateRangeInput(session,
                                                     inputId = i, 
                                                     label =  NULL,
                                                     min     = MINI, 
                                                     max     = MAXI, 
                                                     start   = MINI, 
                                                     end     = MAXI
                                ) 
                                
                            } else  {
                                
                                # input[[i]][1] within date range and input[[i]][2] outside date range 
                                if ((input[[i]][1] >=  MINI &  input[[i]][1] <=  MAXI) &
                                    (input[[i]][2] <   MINI | input[[i]][2] >   MAXI) ) {
                                    updateDateRangeInput(session,  
                                                         inputId = i, 
                                                         label =  NULL,
                                                         min     = MINI, 
                                                         max     = MAXI, 
                                                         start   = NULL,
                                                         end     = MAXI
                                    ) 
                                } 
                            }
                        }
                    }
                }
            }
        }, ignoreInit = TRUE)
        
        # Out.Ref.Date1: Update Valid1 date range with left and right buttons ----
        # What if the VALID1 of this pollutant does not exist? put min() ###############################################################################################################C
        observeEvent(
            {unlist(sapply(1:length(Config()[[2]]$gas.reference), function(i) input[[paste0("left2Out.Ref.Date", i)]]))
                
            }, {
                # index 1,2,3,4, 5, 6 of the reference gas selected in uiFiltering corresponding of rows of ASE_name.cfg
                Ref.Index <-  match(x = input$Filtering.References, table = Config()[[2]]$gas.reference)
                # Corresponding index for Valid
                # Corresponding index for input$Valid
                Sens.Index <-  match(x = Config()[[2]]$gas.sensor[Ref.Index], table = list.gas.sensors())
                
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
            unlist(sapply(1:length(Config()[[2]]$gas.reference), function(i) input[[paste0("left1Out.Ref.Date", i)]]))
        }, {
            # index 1,2,3,4, 5, 6 of the reference gas selected in uiFiltering corresponding of rows of ASE_name.cfg
            Ref.Index  <-  match(x = input$Filtering.References, table = Config()[[2]]$gas.reference)
            # Corresponding index for input$Valid
            Sens.Index <-  match(x = Config()[[2]]$gas.sensor[Ref.Index], table = list.gas.sensors())
            
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
            unlist(sapply(1:length(Config()[[2]]$gas.reference), function(i) input[[paste0("right1Out.Ref.Date", i)]]))
        }, {
            # index 1,2,3,4, 5, 6 of the reference gas selected in uiFiltering corresponding of rows of ASE_name.cfg
            Ref.Index <-  match(x = input$Filtering.References, table = Config()[[2]]$gas.reference)
            # Corresponding index for input$Valid
            Sens.Index <-  match(x = Config()[[2]]$gas.sensor[Ref.Index], table = list.gas.sensors())
            
            if (!is.na(Sens.Index)){
                
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
                                     max     = NULL)
            }
        }, 
        ignoreInit = TRUE
        )
        observeEvent({
            unlist(sapply(1:length(Config()[[2]]$gas.reference), function(i) input[[paste0("right2Out.Ref.Date", i)]]))
        }, {
            # index 1,2,3,4, 5, 6 of the reference gas selected in uiFiltering corresponding of rows of ASE_name.cfg
            Ref.Index <-  match(x = input$Filtering.References, table = Config()[[2]]$gas.reference)
            # Corresponding index for input$Valid
            Sens.Index <-  match(x = Config()[[2]]$gas.sensor[Ref.Index], table = list.gas.sensors())
            
            if (!is.na(Sens.Index)){
                
                # update correct dateRange
                if (!is.na(input[[paste0("Valid",Sens.Index)]][2])) updateDateRangeInput(session,
                                                                                        inputId = paste0("Out.Ref.Date",Ref.Index),
                                                                                        label   = NULL, 
                                                                                        start   = NULL,
                                                                                        end     = input[[paste0("Valid",Sens.Index)]][2],
                                                                                        min     = NULL,
                                                                                        max     = input[[paste0("Valid",Sens.Index)]][2])
            }
        }, 
        ignoreInit = TRUE
        )
        
        # Valid1: Update Valid1 date range with left and right buttons ----
        observeEvent({
            unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("left2Out.Valid", i)]]))
        }, {
            
            # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
            Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensors())
            
            updateDateRangeInput(session,
                                 inputId = paste0("Valid", Sens.Index),
                                 label   = NULL, 
                                 start   = as.Date(min.General.date()),
                                 end     = NULL,
                                 min     = as.Date(min.General.date()),
                                 max     = NULL
            )
        }, 
        ignoreInit = TRUE
        )
        observeEvent({
            unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("right2Out.Valid", i)]]))
        }, {
            
            # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
            Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensors())
            
            updateDateRangeInput(session,
                                 inputId = paste0("Valid",Sens.Index),
                                 label   = NULL, 
                                 start   = NULL,
                                 end     = as.Date(max.General.date()),
                                 min     = NULL,
                                 max     = as.Date(max.General.date())
            )
        }, 
        ignoreInit = TRUE
        )
        
        # Out.Sens.Date1: Range of dates for plotting RawData, DataTable, Retrieved, Warming, Temp&Humid, Invalids and outliers: ----
        observeEvent({
            unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("left2Out.Sens.Date", i)]]))
        }, {
            
            # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
            Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensors())
            
            updateDateRangeInput(session,
                                 inputId = paste0("Out.Sens.Date", Sens.Index),
                                 label   = NULL, 
                                 start   = as.Date(input[[paste0("Valid",Sens.Index)]][1]),
                                 end     = NULL,
                                 min     = as.Date(input[[paste0("Valid",Sens.Index)]][1]),
                                 max     = NULL)
        }, 
        ignoreInit = TRUE
        )
        observeEvent({
            unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("left1Out.Sens.Date", i)]]))
        }, {
            
            # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
            Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensors())
            
            range.date = input[[paste0("Out.Sens.Date",Sens.Index)]][2] - input[[paste0("Out.Sens.Date",Sens.Index)]][1]
            if (input[[paste0("Out.Sens.Date",Sens.Index)]][1] - range.date < as.Date(input[[paste0("Valid",Sens.Index)]][1])) {
                
                Start = as.Date(input[[paste0("Valid",Sens.Index)]][1])
                
            } else Start = input[[paste0("Out.Sens.Date",Sens.Index)]][1] - range.date
            End =  Start + range.date
            updateDateRangeInput(session,
                                 inputId = paste0("Out.Sens.Date", Sens.Index),
                                 label   = NULL, 
                                 start   = Start,
                                 end     = End,
                                 min     = NULL,
                                 max     = NULL)
        }, 
        ignoreInit = TRUE
        )
        observeEvent({
            unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("right1Out.Sens.Date", i)]]))
        }, {
            
            # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
            Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensors())
            
            range.date = input[[paste0("Out.Sens.Date",Sens.Index)]][2] - input[[paste0("Out.Sens.Date",Sens.Index)]][1]
            if (input[[paste0("Out.Sens.Date",Sens.Index)]][2] + range.date > as.Date(input$Valid1[2])) {
                
                End = as.Date(input[[paste0("Valid",Sens.Index)]][2])
                
            } else End = input[[paste0("Out.Sens.Date",Sens.Index)]][2] + range.date
            Start = End - range.date
            updateDateRangeInput(session,
                                 inputId = paste0("Out.Sens.Date", Sens.Index),
                                 label   = NULL, 
                                 start   = Start,
                                 end     = End,
                                 min     = NULL,
                                 max     = NULL)
        }, 
        ignoreInit = TRUE
        )
        observeEvent({
            unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("right2Out.Sens.Date", i)]]))
        }, {
            
            # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
            Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensors())
            
            updateDateRangeInput(session,
                                 inputId = paste0("Out.Sens.Date", Sens.Index),
                                 label   = NULL, 
                                 start   = NULL,
                                 end     = as.Date(input[[paste0("Valid",Sens.Index)]][2]),
                                 min     = NULL,
                                 max     = as.Date(input[[paste0("Valid",Sens.Index)]][2]))
        }, 
        ignoreInit = TRUE
        )
        
        # Date1: Range of dates for plotting covariates in UTC:, buttons left and write and checking rage of date
        observeEvent(
            {unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("left2Date", i)]]))}, 
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensors())
                
                updateDateRangeInput(session,
                                     inputId = paste0("Date", Sens.Index),
                                     label   = NULL, 
                                     start   = as.Date(input[[paste0("Valid",Sens.Index)]][1]),
                                     end     = NULL,
                                     min     = as.Date(input[[paste0("Valid",Sens.Index)]][1]),
                                     max     = NULL)
            },
            ignoreInit = TRUE
        )
        
        observeEvent(
            {unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("left1Date", i)]]))}, 
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensors())
                
                range.date = input[[paste0("Date",Sens.Index)]][2] - input[[paste0("Date",Sens.Index)]][1]
                if (input[[paste0("Date",Sens.Index)]][1] - range.date < as.Date(input[[paste0("Valid",Sens.Index)]][1])) {
                    
                    Start = as.Date(input[[paste0("Valid",Sens.Index)]][1])
                    
                } else Start = input[[paste0("Date",Sens.Index)]][1] - range.date
                End =  Start + range.date
                updateDateRangeInput(session,
                                     inputId = paste0("Date", Sens.Index),
                                     label   = NULL, 
                                     start   = Start,
                                     end     = End,
                                     min     = NULL,
                                     max     = NULL)
            }, 
            ignoreInit = TRUE
        )
        
        observeEvent(
            {unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("right1Date", i)]]))}, 
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensors())
                
                range.date = input[[paste0("Date",Sens.Index)]][2] - input[[paste0("Date",Sens.Index)]][1]
                if (input[[paste0("Date",Sens.Index)]][2] + range.date > as.Date(input$Valid1[2])) {
                    
                    End = as.Date(input[[paste0("Valid",Sens.Index)]][2])
                    
                } else End = input[[paste0("Date",Sens.Index)]][2] + range.date
                Start = End - range.date
                updateDateRangeInput(session,
                                     inputId = paste0("Date", Sens.Index),
                                     label   = NULL, 
                                     start   = Start,
                                     end     = End,
                                     min     = NULL,
                                     max     = NULL)
            }, 
            ignoreInit = TRUE
        )
        
        observeEvent(
            {unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("right2Date", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensors())
                
                updateDateRangeInput(session,
                                     inputId = paste0("Date", Sens.Index),
                                     label   = NULL, 
                                     start   = NULL,
                                     end     = as.Date(input[[paste0("Valid",Sens.Index)]][2]),
                                     min     = NULL,
                                     max     = as.Date(input[[paste0("Valid",Sens.Index)]][2]))
            }, 
            ignoreInit = TRUE
        )
        
        # DateCal1: Range of dates for calibration in UTC:, buttons left and write and checking rage of date
        observeEvent(
            {unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("left2DateCal", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensors())
                
                updateDateRangeInput(session,
                                     inputId = paste0("DateCal", Sens.Index),
                                     label   = NULL, 
                                     start   = as.Date(input[[paste0("Valid",Sens.Index)]][1]),
                                     end     = NULL,
                                     min     = as.Date(input[[paste0("Valid",Sens.Index)]][1]),
                                     max     = NULL)
            }, 
            ignoreInit = TRUE
        )
        
        observeEvent(
            {unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("Left1DateCal", i)]]))},
            {
                
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensors())
                
                range.date = input[[paste0("DateCal",Sens.Index)]][2] - input[[paste0("DateCal",Sens.Index)]][1]
                Class.date   <- class(input[[paste0("DateCal",Sens.Index)]]) == "date" 
                Correct.date <- input[[paste0("DateCal",Sens.Index)]][1] - range.date < as.Date(input[[paste0("Valid",Sens.Index)]][1])
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
            {unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("right1DateCal", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensors())
                
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
            {unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("right2DateCal", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensors())
                
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
            {unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("left2DatePlotCal", i)]]))},
            {
                
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensors())
                
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
            {unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("left1DatePlotCal", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensors())
                
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
            {unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("right1DatePlotCal", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensors())
                
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
            {unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("right2DatePlotCal", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensors())
                
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
        
        # DateMeas1: Range of dates for extrapolation in UTC:, buttons left and write and checking rage of date
        observeEvent(
            {unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("left2DateMeas", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensors())
                
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
            {unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("left1DateMeas", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensors())
                
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
            {unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("right1DateMeas", i)]]))},
            {
                
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensors())
                
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
            {unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("right2DateMeas", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensors())
                
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
        
        # DatePlotMeas1: Range of dates for plotting extrapolation in UTC:, buttons left and write and checking rage of date
        observeEvent(
            {unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("left2DatePlotMeas", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensors())
                
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
            {unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("left1DatePlotMeas", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensors())
                
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
            {unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("right1DatePlotMeas", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensors())
                
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
            {unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("right2DatePlotMeas", i)]]))},
            {
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                Sens.Index <-  match(x = input$SetTime.Sensors, table = list.name.sensors())
                
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
        mycallback <- function(value) {
            cat(paste0("value shinyalert :",value))
            if (value) {
                
                # Detect Selected Sensors
                # k is the index (1,2,3,4, of the selected  sensors in uiCalib corresponding of position in list.name.sensors()
                Sens.Index    <- match(x = input$Calib.Sensors, table = list.name.sensors())
                
                # Selecting compounds associated with selected sensor
                gas.sensor    <-  list.gas.sensors()[Sens.Index]
                
                # Detect Selected Model
                Cal  <- paste0(Config()[[1]]$AirsensEur.name,"__",list.name.sensors()[Sens.Index],"__",input[[paste0("Cal",Sens.Index)]])
                
                if (!is.null(Cal)) {
                    
                    # Delete Models, Modelled, calibration plots and Statistics
                    WDoutput      <- c(file.path(DisqueFieldtestDir(), "Calibration"), 
                                       file.path(DisqueFieldtestDir(), "Models"), 
                                       file.path(DisqueFieldtestDir(), "Modelled_gas"), 
                                       file.path(DisqueFieldtestDir(), "Statistics")
                    )
                    
                    #deleting
                    for (i in WDoutput) do.call(file.remove, list(list.files(path       = i,
                                                                             pattern    = Cal,
                                                                             full.names = TRUE)))
                    
                    # Update list of Models
                    choices <- substr(list.files(path = file.path(DisqueFieldtestDir(),"Models"), 
                                                 pattern = glob2rx(paste0(Config()[[1]]$AirsensEur.name,"*",list.name.sensors()[Sens.Index],"*",".rds"))), 
                                      start = nchar(paste0(Config()[[1]]$AirsensEur.name,"__",list.name.sensors()[Sens.Index],"__")) + 1,
                                      stop  = nchar(list.files(path = file.path(DisqueFieldtestDir(),"Models"), 
                                                               pattern = glob2rx(paste0(Config()[[1]]$AirsensEur.name,"*",list.name.sensors()[Sens.Index],"*",".rds"))))
                    )
                    
                    # Update Selected Model
                    updateSelectInput(session  = session,
                                      inputId  = paste0("Cal",Sens.Index), 
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
                        animation = FALSE
                    )
                }
            }
        }
        observeEvent(
            {sapply(1:length(list.name.sensors()), function(i) input[[paste0("DelModel",i)]])},
            {
                # Create a Progress object
                progress <- shiny::Progress$new()
                # Make sure it closes when we exit this reactive, even if there's an error
                on.exit(progress$close())
                progress$set(message = "[shiny, DelModel] INFO, Deleting Model and all related pictures. CLICK OK ONLY ONCE!", value = 0.5)
                
                if (!is.null(input$Calib.Sensors)) {
                    
                    if (!is.null(list.name.sensors())) {
                        
                        # k is the index (1,2,3,4, of the selected  sensors in uiCalib corresponding of position in list.name.sensors()
                        k    <- match(x = input$Calib.Sensors, table = list.name.sensors())
                        
                        if (any(sapply(1:length(list.name.sensors()), function(i) input[[paste0("DelModel",i)]] > 0))) {
                            
                            if (!is.null(input[[paste0("Cal",k)]])) {
                                
                                # Detect Selected Model
                                Cal  <- input[[paste0("Cal",k)]]
                                
                                # Show a modal message when the button "DelModel" is pressed
                                if (Cal != "") {
                                    
                                    shinyalert(
                                        title = "Confirm",
                                        text = paste0("Are you sure to delete the calibration model ",input[[paste0("Cal",k)]]," and relative plots?"),
                                        closeOnEsc = FALSE,
                                        closeOnClickOutside = FALSE,
                                        html = FALSE,
                                        type = "success",
                                        showConfirmButton = TRUE,
                                        showCancelButton = TRUE,
                                        confirmButtonText = "OK",
                                        confirmButtonCol = "#AEDEF4",
                                        cancelButtonText = "Cancel",
                                        timer = 0,
                                        imageUrl = "",
                                        animation = FALSE,
                                        callbackR = mycallback
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
                    }
                }
                progress$set(message = "[shiny, DelModel] INFO, Deleting Model and all related pictures. CLICK OK ONLY ONCE!", value = 1)
            }, 
            ignoreInit = TRUE, # Whether the action should be triggered (or value calculated, in the case of eventReactive) when the input is NULL. 
            ignoreNULL = TRUE  # If TRUE, then, when this observeEvent is first created/initialized, ignore the handlerExpr (the second argument), whether it is otherwise supposed to run or not. The default is FALSE. 
        )
        
        # REPORT SERVER ----
        output$renderedReport <- renderUI({   
            includeMarkdown(knitr::knit(file.path(DirShiny, 'report.Rmd')) )
        })
        
        # download report
        output$report <- downloadHandler(filename <- file.path(CalSet()$WDModelled_gas, paste0(AirsensEur.name(),"__",CalSet()$name.sensor,"__",CalSet()$Cal,"__.docx")),
                                         content <-
                                             function(file) {
                                                 file.remove(file.path(CalSet()$WDModelled_gas, paste0(AirsensEur.name(),"__",CalSet()$name.sensor,"__",CalSet()$Cal,"__.docx")))
                                                 renderedFile <- render(
                                                     input = file.path(DirShiny, "report.Rmd"),
                                                     output_file = paste0(DisqueFieldtestDir(), "/", "report.docx"))
                                                 markdown::markdownToHTML(paste0(DisqueFieldtestDir(),"/","report.md"),
                                                                          paste0(DisqueFieldtestDir(),"/","report.html"), options = c("use_xhml"))
                                             })
        
        
        # Reactive AirsensEur.name() name of ASE box ----
        AirsensEur.name <- reactive({
            old_ASE_name       <- basename(input$Selected) 
            for (i in c("\\.[[:alnum:]_]+$" ,"ASEconfig")) old_ASE_name <- sub(pattern = i,replacement = '', basename(as.character(old_ASE_name)))
            return(old_ASE_name)
            #if (input$AirsensEur.name == "") return(ASE_name()) else return(input$AirsensEur.name)
        })
        
        # NavBar"Console Logs", Plotting Console output ----
        output$console <- renderPrint({logText()})
        logText        <- reactive({
            input$UpdateLog # It updates each time we click of the button UpdateLog
            return(ReadLastLines(file.path(DisqueFieldtestDir(), "scriptsLog",paste0("console_", Sys.Date(),".log")),1000)) # only 1000 lines can be viewed
        })
        
        # Merging All data
        # Reactive Change.Delay ----
        # Detecting if input$Delay was changed to trigger a new DF$General dataFrame
        Change.Delay             <- reactive({
            if (as.integer(input$Delay) != Config()[[1]]$Delay) {
              
                return(TRUE)
                click(id = "Warm$Forced")
                click(id = "Save")
            } else return(FALSE)
        })
        # Reactive Change.UserMins ----
        # Detecting if input$UserMins was changed to trigger a new DF$General dataFrame
        Change.UserMins             <- reactive({
            if (as.integer(input$UserMins) != Config()[[1]]$UserMins) return(TRUE) else return(FALSE)
        })
        
        # Reactive DF ----
        # initial data in General.Rdata.file
        DF.NULL <- reactiveValues(Init = FALSE)
        General.Rdata.file  = file.path(DisqueFieldtestDir(), "General_data" , "General.Rdata")
        if (file.exists(General.Rdata.file)) {
            
            progress <- shiny::Progress$new()
            progress$set(message = "[shiny] INFO, Loading General data", value = 0.2)
            load(General.Rdata.file)
            #initial.DF <- General.df
            progress$set(message = "[shiny] INFO, Loading General data", value = 1)
            progress$close()
            
        } else {
            
            General.df <- NULL
            DF.NULL$Init <- TRUE
        } 
        DF <- reactiveValues(General = General.df)
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
            input$Down.SOS
            input$Down.Ref
            DF.NULL$Init
            input$Merge
        },{
            # depends :   
            #           DisqueFieldtestDir()
            #           input$UserMins
            #           input$Delay
            #           REFDATA()[[1]]
            #           INFLUX()[[1]]
            #           SOS_T()[[1]]
            #           list.gas.sensors()
            #           DownloadSensor()
            #           Change.Delay()
            #           Change.UserMins()
            
            # Checking that parameters for sensor download are complete
            #browser()
            if (DF.NULL$Init) {
                
                # Create a Progress object
                progress <- shiny::Progress$new()
                # Make sure it closes when we exit this reactive, even if there's an error
                on.exit(progress$close())
                progress$set(message = "[shiny, General()] INFO, Merging Influx, SOS and Reference data", value = 0.5)
                
                if (input$Down.Influx & any(is.null(c(input$Host, input$User, input$Pass, input$Db, input$Dataset))) ) {
                    
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
                    if (input$Down.SOS & input$AirsensWeb == "") {
                        
                        shinyalert(
                            title = "Error sensor data download",
                            text  = "SOS sensor data download is requested. However, you have to fill the text in put of the rest API.",
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
                        if (input$Down.Ref & 
                            ((input$FTPMode == "ftp" & input$urlref == "")) | 
                            ( input$FTPMode == "SOS" & any(is.null(c(input$Ref.SOS.name, input$RefSOSname)))
                            )
                        ) {
                            
                            shinyalert(
                                title = "Error SOS reference data download",
                                text  = "Download of sensor data is necessary but parameters are missing: \"URL of the ftp server with full name\" if ftp is selected or 
                                \"Reference station SOS Rest API URL\" and \"SOS ID of the Reference station\" if SOS is Selected.",
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
                            
                            if (input$Down.SOS & input$Down.Influx) {
                                
                                shinyalert(
                                    title = "Error sensor data download",
                                    text  = "It is not possible to allow both for SOS and Inlfux sensor data download in the same session, please check only one tyope of download.",
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
                                
                                D <- GENERAL(WDoutput            = file.path(DisqueFieldtestDir(), "General_data"), 
                                             UserMins            = as.numeric(input$UserMins), 
                                             Delay               = as.numeric(input$Delay), 
                                             RefData             = REFDATA()[[1]], 
                                             InfluxData          = INFLUX()[[1]], 
                                             SOSData             = SOS_T()[[1]],
                                             var.name.GasSensors = list.gas.sensors()  , 
                                             DownloadSensor      = DownloadSensor(), 
                                             Change.Delay        = Change.Delay(),
                                             Change.UserMins     = Change.UserMins()
                                )
                                
                                # saving New General data if needed
                                save.General.df <- FALSE 
                                if (!file.exists(General.Rdata.file) ) {
                                    save.General.df <- TRUE
                                    General.df <- D
                                    
                                    # Checking if the SETTIME are consistent with the avalable date in General()
                                    MINI = min(D$date, na.rm = T)
                                    MAXI = max(D$date, na.rm = T)
                                    List.dateRange <- c(paste0("Valid"        , c(1:length(list.name.sensors()))),
                                                        paste0("Out.Sens.Date", c(1:length(list.name.sensors()))),
                                                        paste0("Date"         , c(1:length(list.name.sensors()))),
                                                        paste0("DateCal"      , c(1:length(list.name.sensors()))),
                                                        paste0("DatePlotCal"  , c(1:length(list.name.sensors()))),
                                                        paste0("DateMeas"     , c(1:length(list.name.sensors()))),
                                                        paste0("DatePlotMeas" , c(1:length(list.name.sensors()))),
                                                        paste0("Out.Ref.Date" , c(1:length(list.gas.reference2use())))
                                    )
                                    
                                    for (i in List.dateRange) {
                                        
                                        # input[[i]][1] outside date range and input[[i]][2] within date range
                                        if ((input[[i]][1] <  MINI | input[[i]][1] >  MAXI) &
                                            (input[[i]][2] >= MINI &  input[[i]][2] <= MAXI) ) {
                                            updateDateRangeInput(session, 
                                                                 inputId = i, 
                                                                 start   = MINI,
                                                                 end     = input[[i]][2],
                                                                 min     = MINI, 
                                                                 max     = input$Valid1[2]
                                            ) 
                                        } else {
                                            
                                            # input[[i]][1] and input[[i]][2] outside date range 
                                            if ((input[[i]][1] < MINI | input[[i]][1] > MAXI) &
                                                (input[[i]][2] < MINI | input[[i]][2] > MAXI) ) {
                                                updateDateRangeInput(session,
                                                                     inputId = i, 
                                                                     min     = MINI, 
                                                                     max     = MAXI, 
                                                                     start   = MINI, 
                                                                     end     = MAXI
                                                ) 
                                                
                                            } else  {
                                                
                                                # input[[i]][1] within date range and input[[i]][2] outside date range 
                                                if ((input[[i]][1] >=  MINI &  input[[i]][1] <=  MAXI) &
                                                    (input[[i]][2] <   MINI | input[[i]][2] >   MAXI) ) {
                                                    updateDateRangeInput(session,  
                                                                         inputId = i, 
                                                                         min     = input[[i]][1], 
                                                                         max     = MAXI, 
                                                                         start   = input[[i]][1],
                                                                         end     =  MAXI
                                                    ) 
                                                } 
                                            }
                                        }
                                    }
                                    
                                    # The file *_SETTIME cannot be saved because the shiny interface is not updated yet
                                    # sens2ref <- CalTime()
                                    # row.names(sens2ref)  <- sens2ref[,"name.gas"] 
                                    # sens2ref             <- as.data.frame(t(sens2ref), stringsAsFactors = FALSE)
                                    # write.table(sens2ref, 
                                    #             file = file.path(DisqueFieldtestDir(),"General_data",paste0(ASE_name(),"_SETTIME.cfg")), 
                                    #             col.names = TRUE)
                                    # #save(sens2ref, file         = file.path(DisqueFieldtestDir(),"General_data",paste0("ASE_name(),"_SETTIME_cfg.Rdata")))
                                    # cat(paste0("[shiny, General()]Save, INFO, ", paste0(ASE_name(),"_SETTIME.cfg")," config file saved in directory General_data.\n"))
                                    
                                } else {
                                    
                                    progress$set(message = "[shiny, General()] INFO, Loading  General.Rdata", value = 0.5)
                                    # loading General and counting rows of initial data
                                    load(General.Rdata.file)
                                    
                                    # the condition that the last date of the new dataset is > to the last date of the old dataset does not allow to decrease the delay, 
                                    # It does not allow also to detect when only reference data are added
                                    # let's use identical
                                    if (!identical(D,General.df)) {
                                        save.General.df <- TRUE
                                        General.df <- D
                                    } 
                                }
                                
                                if (save.General.df) {
                                    # Saving downloaded data in General_data Files
                                    cat("-----------------------------------------------------------------------------------\n")
                                    
                                    # saving New General data 
                                    progress$set(message = "[shiny, General()] INFO, Saving General.Rdata", value = 0.6)
                                    save(General.df, file = General.Rdata.file)
                                    
                                    #progress$set(message = "Saving Calibrated/extrapolated General.csv", value = 0.7)
                                    #write.csv(General.df, file = General.csv.file)
                                    
                                    # if general is saved, it is necessary to run the detection of warming, T/RH out of tolerance, Negative Ref., Invalids and outlier detection, sensor data conversion and calibration.
                                    # It is sufficient to set to TRUE to run ind.warm then in.TRH, 
                                    progress$set(message = "[shiny, General()] INFO, Enabling detection of warming of sensors", value = 0.9)
                                    for (i in 1:length(list.name.sensors())) {
                                        if (!input[[paste0("Apply.Warm",i)]]) updateCheckboxInput(session, inputId = paste0("Apply.Warm",i), label = NULL, value = TRUE)
                                    }
                                    
                                    on.exit(progress$close())
                                    
                                    # We also need to save all config file since the ##################################################################################################C
                                }
                                
                                progress$set(message = "[shiny, General()] INFO, Merging Influx, SOS and Reference data", value = 1)
                                
                                # make sure to update DF$General
                                DF$General <- General.df
                                
                                return(General.df)
                            }
                        }
                    }
                }
            }
        }, priority = 300)
        # Reactive min.General.date() and max.General.date----
        min.General.date         <- reactive({
            if (!is.null(DF$General)) return(min(DF$General$date, na.rm = TRUE)) else return(NULL)
        })
        max.General.date         <- reactive({
            if (!is.null(DF$General)) return(max(DF$General$date, na.rm = TRUE)) else return(NULL)
        })
        
        # NavBar "SelectASE", mainTabPanel "Downloaded" ----
        # INformation on existin downloaded data in airsenseur.db, InfluxData, SOSData, RefData and General
        output$Downloaded      <- renderTable(Downloaded())
        # DownloadedSensor: only for merging status
        Downloaded <- reactive({
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            progress$set(message = "[shiny, Downloaded()] INFO, Detecting downloaded data in airsenseur.db, Influx, SOS and Reference and General files", value = 0.2)
            
            if (is.null(DownloadedSensor()[["DateIN.db.prev"]]))  DateIN.db.prev  <- "NULL" else DateIN.db.prev  <- format(ymd_hms(DownloadedSensor()[["DateIN.db.prev"]]) , "%Y-%m-%d %H:%M")
            if (is.null(DownloadedSensor()[["DateEND.db.prev"]])) DateEND.db.prev <- "NULL" else DateEND.db.prev <- format(ymd_hms(DownloadedSensor()[["DateEND.db.prev"]]), "%Y-%m-%d %H:%M")
            
            progress$set(message = "[shiny, Downloaded()] INFO, Detecting downloaded data in airsenseur.db, Influx, SOS and Reference and General files", value = 0.5)
            
            Downloaded <- data.frame( 
                DataSets = c("airsenseur.db","InfluxData", "SOSData", "ReferenceData","General"),
                Exists   = c(DownloadedSensor()[["ExistFil.data.db"]],
                             DownloadedSensor()[["ExistFil.data.Influx"]],
                             DownloadedSensor()[["ExistFil.data.SOS"]],
                             DownloadedSensor()[["ExistFil.data.Ref"]],
                             DownloadedSensor()[["ExistFil.data.General"]]),
                NeedRetrieve = c(DownloadedSensor()[["Retrieve.data.db"]],
                                 DownloadedSensor()[["Retrieve.data.Influx"]],
                                 DownloadedSensor()[["Retrieve.data.SOS"]],
                                 DownloadedSensor()[["Retrieve.data.Ref"]],
                                 DownloadedSensor()[["Retrieve.data.General"]]),
                INdate   = c(DateIN.db.prev,
                             format(DownloadedSensor()[["DateIN.Influx.prev"]]  , "%Y-%m-%d %H:%M"),
                             format(DownloadedSensor()[["DateIN.SOS.prev"]]     , "%Y-%m-%d %H:%M"),
                             format(DownloadedSensor()[["DateIN.Ref.prev"]]     , "%Y-%m-%d %H:%M"),
                             format(DownloadedSensor()[["DateIN.General.prev"]] , "%Y-%m-%d %H:%M")
                ),
                ENDdate  = c(DateEND.db.prev,
                             format(DownloadedSensor()[["DateEND.Influx.prev"]] , "%Y-%m-%d %H:%M"),
                             format(DownloadedSensor()[["DateEND.SOS.prev"]]    , "%Y-%m-%d %H:%M"),
                             format(DownloadedSensor()[["DateEND.Ref.prev"]]    , "%Y-%m-%d %H:%M"),
                             format(DownloadedSensor()[["DateEND.General.prev"]], "%Y-%m-%d %H:%M")
                ),
                row.names = NULL, 
                check.rows = FALSE,
                check.names = FALSE, 
                stringsAsFactors = FALSE
            )
            
            progress$set(message = "[shiny, Downloaded()] INFO, Detecting downloaded data in airsenseur.db, Influx, SOS and Reference and General files", value = 1)
            on.exit(progress$close())
            
            return(Downloaded)
        })
        DownloadedSensor          <- eventReactive( 
            {INFLUX()
                SOS_T()
                REFDATA()
                DF$General
            },{
                Check_Download(Influx.name = input$Dataset,
                               WDinput     = file.path(DisqueFieldtestDir(), "General_data"), 
                               UserMins    = as.numeric(input$UserMins))
            }
        )
        
        # NavBar"Data Treatment", mainTabPanel "FilteringMain" - "Config"  ----
        # Reactive Outliers_Sensor()
        Outliers_Sensor        <- reactive({
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            progress$set(message = "Reading sensor config file", value = 0.5)
            
            # Compose data frame
            name.gas.name.sensor <- data.frame(
                name.gas           =  Config()[[2]]$name.gas,
                name.sensor        = as.character(Config()[[2]]$name.sensor), 
                stringsAsFactors   = FALSE)
            Sensors.Outliers <- data.frame(     
                hoursWarming       = as.integer(sapply(1:length(list.gas.sensors()), function(i) input[[paste0("Warming",i)]])),
                temp.thres.min     = as.numeric(sapply(1:length(list.gas.sensors()), function(i) input[[paste0("Temperature",i)]][1])),
                temp.thres.max     = as.numeric(sapply(1:length(list.gas.sensors()), function(i) input[[paste0("Temperature",i)]][2])),
                rh.thres.min       = as.integer(sapply(1:length(list.gas.sensors()), function(i) input[[paste0("Humidity",i)]][1])),
                rh.thres.max       = as.integer(sapply(1:length(list.gas.sensors()), function(i) input[[paste0("Humidity",i)]][2])),
                Sens.Inval.Out     = as.logical(sapply(1:length(list.gas.sensors()), function(i) input[[paste0("Sens.Inval.Out",i)]])),
                Sens.rm.Out        = as.logical(sapply(1:length(list.gas.sensors()), function(i) input[[paste0("Sens.rm.Out",i)]])),
                Sens.window        = as.integer(sapply(1:length(list.gas.sensors()), function(i) input[[paste0("Sens.window",i)]])),
                Sens.threshold     = as.integer(sapply(1:length(list.gas.sensors()), function(i) input[[paste0("Sens.threshold",i)]])),
                Sens.Ymin          = as.integer(sapply(1:length(list.gas.sensors()), function(i) input[[paste0("Sens.Ymin",i)]])),
                Sens.Ymax          = as.integer(sapply(1:length(list.gas.sensors()), function(i) input[[paste0("Sens.Ymax",i)]])),
                Sens.ThresholdMin  = as.integer(sapply(1:length(list.gas.sensors()), function(i) input[[paste0("Sens.ThresholdMin",i)]])),
                Sens.iterations    = as.integer(sapply(1:length(list.gas.sensors()), function(i) input[[paste0("Sens.iterations",i)]])),
                stringsAsFactors   = FALSE)
            for (i in 1:length(Sensors.Outliers)) name.gas.name.sensor[which(!is.na(name.gas.name.sensor$name.sensor)), length(names(name.gas.name.sensor)) + 1] <- Sensors.Outliers[,i]
            names(name.gas.name.sensor)[3:length(names(name.gas.name.sensor))] <- names(Sensors.Outliers)
            
            progress$set(message = "Reading sensor config file", value = 1)
            return(name.gas.name.sensor)
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
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            progress$set(message = "Reading Referance data config file", value = 0.5)
            
            # Compose data frame
            Reference.Outliers <- data.frame(
                name.gas           = Config()[[2]]$name.gas, 
                name.sensor        = as.character(Config()[[2]]$name.sensor), 
                remove.neg         = as.logical(  sapply(1:length(Config()[[2]]$gas.reference2use), function(i) input[[paste0("rm.neg",i)]])),
                ref.unitgas        = as.character(sapply(1:length(Config()[[2]]$gas.reference2use), function(i) input[[paste0("Ref.unit",i)]])),
                Ref.rm.Out         = as.logical(  sapply(1:length(Config()[[2]]$gas.reference2use), function(i) input[[paste0("Ref.rm.Out",i)]])),
                Ref.window         = as.integer(  sapply(1:length(Config()[[2]]$gas.reference2use), function(i) input[[paste0("Ref.window",i)]])),
                Ref.threshold      = as.numeric(  sapply(1:length(Config()[[2]]$gas.reference2use), function(i) input[[paste0("Ref.threshold",i)]])),
                Ref.Ymin           = as.numeric(  sapply(1:length(Config()[[2]]$gas.reference2use), function(i) input[[paste0("Ref.Ymin",i)]])),
                Ref.Ymax           = as.numeric(  sapply(1:length(Config()[[2]]$gas.reference2use), function(i) input[[paste0("Ref.Ymax",i)]])),
                Ref.ThresholdMin   = as.numeric(  sapply(1:length(Config()[[2]]$gas.reference2use), function(i) input[[paste0("Ref.ThresholdMin",i)]])),
                Ref.iterations     = as.integer(  sapply(1:length(Config()[[2]]$gas.reference2use), function(i) input[[paste0("Ref.iterations",i)]])),
                stringsAsFactors = FALSE)
            
            progress$set(message = "Reading Referance data config file", value = 1)
            
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
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            progress$set(message = "Reading sensor calibration config file", value = 0.5)
            
            name.gas.name.sensor   <- data.frame(
                name.gas           = Config()[[2]]$name.gas, 
                name.sensor        = as.character(Config()[[2]]$name.sensor), 
                gas.reference      = Config()[[2]]$gas.reference,
                gas.reference2use  = Config()[[2]]$gas.reference2use,
                stringsAsFactors = FALSE)
            Calib_data.df          <- name.gas.name.sensor
            Sensors.Outliers       <- data.frame(
                Cal.Line           = as.character(sapply(1:length(list.gas.sensors()), function(i) input[[paste0("Cal.Line",i)]])),
                mod.eta.model.type = as.character(sapply(1:length(list.gas.sensors()), function(i) input[[paste0("Calibration",i)]])),
                Neg.mod            = as.logical  (sapply(1:length(list.gas.sensors()), function(i) input[[paste0("Neg.mod",i)]])),
                Cal.func           = as.character(sapply(1:length(list.gas.sensors()), function(i) input[[paste0("Cal",i)]])),
                Slope              = as.numeric(  sapply(1:length(list.gas.sensors()), function(i) input[[paste0("Slope",i)]])),
                Intercept          = as.numeric(  sapply(1:length(list.gas.sensors()), function(i) input[[paste0("Intercept",i)]])),
                Sens.raw.unit      = as.character(sapply(1:length(list.gas.sensors()), function(i) input[[paste0("Sens.raw.unit",i)]])),
                Sens.unit          = as.character(sapply(1:length(list.gas.sensors()), function(i) input[[paste0("Sens.unit",i)]])),
                uxi                = as.numeric(  sapply(1:length(list.gas.sensors()), function(i) input[[paste0("uxi",i)]])),
                eta.model.type     = as.character(sapply(1:length(list.gas.sensors()), function(i) input[[paste0("Comparison",i)]])),
                stringsAsFactors = FALSE)
            # Rearranging only according to rows of sensors
            if (nrow(Sensors.Outliers) > 0) {
                for (i in 1:length(Sensors.Outliers)) Calib_data.df[i.sensors(), length(names(Calib_data.df)) + 1]   <- Sensors.Outliers[,i]
                names(Calib_data.df)[(length(name.gas.name.sensor) + 1):length(names(Calib_data.df))] <- names(Sensors.Outliers)
            } 
            
            # To avoid ERROR/to update Calib_data make sure that the Tabset "Calib" is opened
            updateTabsetPanel(session, inputId = "Calib_data", selected = "Calib")
            progress$set(message = "Updatin sensor calibration using reactive values of the SideLayout", value = 1)
            
            return(Calib_data.df)
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
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            progress$set(message = "Reading sensor calibration date time of the SideBarLayout", value = 0.5)
            
            name.gas.name.sensor <- data.frame(
                name.gas           = Config()[[2]]$name.gas, 
                name.sensor        = as.character(Config()[[2]]$name.sensor), 
                Out.Ref.IN         = sapply(1:length(Config()[[2]]$gas.reference2use), function(i) format(input[[paste0("Out.Ref.Date",i)]][1], format = "%y-%m-%d %H:%M")),
                Out.Ref.END        = sapply(1:length(Config()[[2]]$gas.reference2use), function(i) format(input[[paste0("Out.Ref.Date",i)]][2], format = "%y-%m-%d %H:%M")),
                stringsAsFactors   = FALSE)
            Sensors.Outliers <- data.frame( 
                # in uiFiltering date plot for sensors and Referencces
                Out.Sens.IN         = sapply(1:length(list.gas.sensors()), function(i) format(input[[paste0("Out.Sens.Date",i)]][1], format = "%y-%m-%d %H:%M")),
                Out.Sens.END        = sapply(1:length(list.gas.sensors()), function(i) format(input[[paste0("Out.Sens.Date",i)]][2], format = "%y-%m-%d %H:%M")),
                
                # in uiSetTime Valid, Cal, Extrapolation and Plotting dates 
                Sens.Inval.Out     = sapply(1:length(list.gas.sensors()), function(i) input[[paste0("Sens.Inval.Out",i)]]),
                Apply.Invalid      = sapply(1:length(list.gas.sensors()), function(i) input[[paste0("Apply.Invalid",i )]]),
                
                # Valid date
                Valid.IN             = sapply(1:length(list.gas.sensors()), function(i) format(input[[paste0("Valid",i)]][1], format = "%y-%m-%d %H:%M")),
                Valid.END            = sapply(1:length(list.gas.sensors()), function(i) format(input[[paste0("Valid",i)]][2], format = "%y-%m-%d %H:%M")),
                
                # Date for plotting covariates
                Cov.Date.IN          = sapply(1:length(list.gas.sensors()), function(i) format(input[[paste0("Date",i)]][1], format = "%y-%m-%d %H:%M")),
                Cov.Date.END         = sapply(1:length(list.gas.sensors()), function(i) format(input[[paste0("Date",i)]][2], format = "%y-%m-%d %H:%M")),
                
                # Calibration dates
                DateCal.IN          = sapply(1:length(list.gas.sensors()), function(i) format(input[[paste0("DateCal",i)]][1], format = "%y-%m-%d %H:%M")),
                DateCal.END         = sapply(1:length(list.gas.sensors()), function(i) format(input[[paste0("DateCal",i)]][2], format = "%y-%m-%d %H:%M")), 
                # Plotting Calibration dates
                DatePlotCal.IN      = sapply(1:length(list.gas.sensors()), function(i) format(input[[paste0("DatePlotCal",i)]][1], format = "%y-%m-%d %H:%M")),
                DatePlotCal.END     = sapply(1:length(list.gas.sensors()), function(i) format(input[[paste0("DatePlotCal",i)]][2], format = "%y-%m-%d %H:%M")),
                
                # Extratpolation date
                Datemeas.IN         = sapply(1:length(list.gas.sensors()), function(i) format(input[[paste0("DateMeas",i)]][1], format = "%y-%m-%d %H:%M")),
                Datemeas.END        = sapply(1:length(list.gas.sensors()), function(i) format(input[[paste0("DateMeas",i)]][2], format = "%y-%m-%d %H:%M")),
                # Extrapolation date for plotting
                DatePlotmeas.IN     = sapply(1:length(list.gas.sensors()), function(i) format(input[[paste0("DatePlotMeas",i)]][1], format = "%y-%m-%d %H:%M")),
                DatePlotmeas.END    = sapply(1:length(list.gas.sensors()), function(i) format(input[[paste0("DatePlotMeas",i)]][2], format = "%y-%m-%d %H:%M")),
                
                stringsAsFactors = FALSE)
            if (nrow(Sensors.Outliers) > 0) {
                for (i in 1:length(Sensors.Outliers)) name.gas.name.sensor[i.sensors(), length(names(name.gas.name.sensor)) + 1] <- Sensors.Outliers[,i]
                names(name.gas.name.sensor)[5:length(names(name.gas.name.sensor))] <- names(Sensors.Outliers)
            } 
            
            progress$set(message = "Updating sensor calibration using reactive values of the SideLayout", value = 1)
            
            return((name.gas.name.sensor))
        })
        
        # NavBar"Data Treatment", mainTabPanel "RawData", ----
        output$RawData  <- renderPlot(Plot.RawData(), width = 'auto', height = 'auto')
        # NavBar"Data Treatment", Reactive Plot.RawData
        Plot.RawData    <- reactive({
            
            # Plotting raw downloaded data in DF$General
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat("[shiny, Plot.RawData()] INFO, plotting raw digital data series in directory General_Data, df General\n")
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            progress$set(message = "Plotting all raw data of AirSensEUR Box", value = 0.5)
            
            # Plot file name
            General.df <- DF$General
            WDoutput  <- file.path(DisqueFieldtestDir(), "General_data")
            name.File <- file.path(WDoutput, 
                                   paste0(AirsensEur.name(),"_Full_time_series_",
                                          format(min(General.df$date, na.rm = TRUE),"%Y%m%d"),"_",
                                          format(max(General.df$date, na.rm = TRUE),"%Y%m%d"),".png"))
            op <- par(no.readonly = TRUE)
            par(mfrow = c(1, 1))
            par(mar   = c(0,0,0,0))
            # Restoring graphical parameters on exit of function
            on.exit(par(op))
            
            if (nrow(General.df) > 1) {
                General.df <- selectByDate(General.df, start = input$Out.Sens.Date1[1], end = input$Out.Sens.Date1[2])
                Names.to.plot <- names(General.df)[
                    -c(which(names(General.df) %in% c("date", "gpsTimestamp", "date_PreDelay", "altitude")), 
                       grep(pattern = paste0(c("Out.", "_volt", "_DV", "_modelled"),
                                             collapse = "|"), 
                            x = names(General.df))
                    )]
                timePlot(mydata = General.df, 
                         pollutant   = Names.to.plot, 
                         date.pad    = TRUE, 
                         auto.text   = FALSE, 
                         y.relation  = "free",
                         key.columns = round(length(Names.to.plot)/3), 
                         Key         = TRUE, 
                         strip       = TRUE, 
                         ylab = "", # Key is the column of legend and strip is the list of ylabels 
                         main = paste0("Complete raw sensor digital values and AQMS station values for ", AirsensEur.name()))
                # Saving plot if requested
                if (input$SavePlot) {
                    dev.copy(png, filename = name.File , 
                             #units = "cm", 
                             #width = 35.55, 
                             #height = 20,
                             res = 300 
                    )
                    dev.off()
                    cat(paste0("[shiny] INFO, ", name.File," saved\n" ))
                    updateCheckboxInput(session, 
                                        inputId = "SavePlot", 
                                        label = NULL, 
                                        value = FALSE)
                }
            } else cat("[shiny, Plot.RawData()] WARMING, no raw digital data to plot from df General.\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            # Cleaning memory space
            remove(WDoutput)
            
            # Opening the Filtering TabSet for GUI consistency
            updateTabsetPanel(session, inputId = "Calib_data" , selected = "SetTime")
            updateTabsetPanel(session, inputId = "Treatments" , selected = "Sensors")
            updateTabsetPanel(session, inputId = "Filtering.Sensors", selected = isolate(input$Sensors))
            
            progress$set(message = "[shiny, Plot.RawData()] INFO, all raw data of AirSensEUR Box", value = 1)
            on.exit(progress$close())
            
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
            
            WDoutput <- file.path(DisqueFieldtestDir(), "Retrieved_plots")
            op <- par(no.readonly = TRUE)
            par(mfrow = c(1, 1))
            # Restoring graphical parameters on exit of function
            on.exit(par(op))
            
            General.df <- DF$General
            if (!is.null(DownloadSensor()$DateIN.General.prev)) {
                General.plot <- General.df[General.df$date > DownloadSensor()$DateIN.General.prev, ]
            } else General.plot <- General.df
            
            General.plot <-  selectByDate(General.plot,start = input$Out.Sens.Date1[1], end = input$Out.Sens.Date1[2])
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            WDoutput <- file.path(DisqueFieldtestDir(), "Retrieved_plots")
            
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
                             main = paste0("Last retrieved raw sensor digital values and AQMS station values for ", AirsensEur.name() ))
                    
                    # Saving plot if requested
                    if (input$SavePlot) {
                        dev.copy(png,
                                 filename = file.path(WDoutput, 
                                                      paste0(AirsensEur.name(),"_Retrieved_",
                                                             format(min(General.plot$date, na.rm = TRUE),"%Y%m%d"),"_",
                                                             format(max(General.plot$date, na.rm = TRUE),"%Y%m%d"),".png")), 
                                 #units = "cm", 
                                 #width = 35.55, 
                                 #height = 20,
                                 res = 300 
                        )
                        dev.off()
                        cat(paste0("[shiny, Plot.Retrieved()] INFO, ", AirsensEur.name(),"_Retrieved_",
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
            remove(WDoutput, General.plot)
            
            # Opening the Filtering TabSet for GUI consistency
            updateTabsetPanel(session, inputId = "Calib_data" , selected = "SetTime")
            updateTabsetPanel(session, inputId = "Treatments" , selected = "Sensors")
            updateTabsetPanel(session, inputId = "Filtering.Sensors", selected = isolate(input$Sensors))
            
            progress$set(message = "[shiny, Plot.Retrieved()] INFO, Plotting the last retrieved data", value = 1)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
        })
        
        # Reactive ind.warm ----
        Warm <- reactiveValues(Forced = FALSE)
        observeEvent({
            unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("Apply.Warm", i)]]))
            #Warm.NULL$Init
        },{
            # Reactive function to trigger for Warming time, temperature/humidity tolerance, negative reference values and invalid, DF$General
            # Warm$Forced is TRUE if Apply.Warm    is TRUE
            #                                 
            # depends: 
            #       list.gas.sensors()
            #       DF$General
            #       input$Apply.Warm
            
            if (any(unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("Apply.Warm", i)]])))   
            ) Warm$Forced <- TRUE else Warm$Forced <- FALSE
        }, 
        ignoreInit = TRUE,
        priority = 200
        )
        ind.warm.file <- file.path(DisqueFieldtestDir(), "General_data" , "ind_warm.RDS")
        if (file.exists(ind.warm.file)) init.ind.warm <- list.load(ind.warm.file) else {
            init.ind.warm <- NULL
            Warm$Forced = TRUE
        }
        ind.warm <- reactiveValues(out = init.ind.warm) 
        observeEvent(Warm$Forced, {
            
            # Flagging the sensor data for warming time
            # This dataTreatment can only works if boardTimeStamp exists, meaning only in InfluxData. It will not work with SOSData
            
            # depends: 
            #   input$Apply.Warm           : crosscheckboxes requiring to check DF$General for warming  time
            #   DF$General                 : reactive function passing a dataframe merging data from Influx, SOS and reference
            #   list.gas.sensors()         : character vector giving the gas compounds of sensors ("Carbon_monoxide", "Nitric_oxide","Nitrogen_dioxide" and "Ozone")
            #   input$Warming              : integer, number of hours of warming per sensors
            #   input$UserMins             : integer number of minutes to average raw data
            # output: a list of 4 character vectors, corresponding to sensors with row index of DF$General corresponding to warming time of sensor data,
            #       the names of the 4 elements are the ones of list.gas.sensors()   in the same order
            
            # Always starting detection of outliers for time warming using the dataframe set in DF$General
            # Only if "apply filter" for warming is selected
            if (Warm$Forced) {
                
                # Create a Progress object
                progress <- shiny::Progress$new()
                # Make sure it closes when we exit this reactive, even if there's an error
                on.exit(progress$close())
                progress$set(message = "[shiny, ind.warm()] INFO,Setting the index of dates for sensor warming time", value = 0.33)
                
                cat("[shiny, ind.warm()]  INFO, Setting index of sensor data during warming period\n")
                
                # setting index for warming
                if (!is.null(DF$General[,"boardTimeStamp"]) ) { # use to be class(DF$General) == "data.frame"
                    
                    # this is for INFLUX
                    # first setting NA boardTimeStamp to the last non-NA boardTimeStamp
                    DF$General[,"boardTimeStamp"] <- na.locf(DF$General[,"boardTimeStamp"], na.rm = FALSE, fromLast = FALSE)
                    ind <- which(DF$General[2:(nrow(DF$General)), "boardTimeStamp"] < DF$General[1:(nrow(DF$General) - 1 ), "boardTimeStamp"])
                    
                } else {
                    
                    # This is for SOS
                    ind <- apply(DF$General[,list.gas.sensors()  ], 1, function(i) !all(is.na(i)))
                    ind <- which(ind[2:length(ind)] & !ind[1:(length(ind) - 1 )])
                }
                ind = ind + 1
                # Adding the first switch on 
                # ind[length(ind)+1] <- 1
                ind <- c(1,ind)
                
                progress$set(message = "[shiny, ind.warm()] INFO, Setting the index of dates for sensor warming time", value = 0.66)
                
                # create the index of warming time for the sensors
                for (n in 1:length(list.gas.sensors())) {
                    indfull <- numeric(length(ind)*input[[paste0("Warming",n)]] * 60 / as.numeric(input$UserMins))
                    # developing IndFull
                    for (i in 1:length(ind)) {
                        indfull[((i - 1) * input[[paste0("Warming",n)]]*60/as.numeric(input$UserMins) + 1):((i)*input[[paste0("Warming",n)]]*60 / as.numeric(input$UserMins))] <- ind[i]:(ind[i] + input[[paste0("Warming",n)]] * 60 / as.numeric(input$UserMins) - 1)
                    }
                    if (exists("return.ind.warm")) return.ind.warm[[n]] <- indfull else return.ind.warm <- list(indfull)
                }
                names(return.ind.warm) <- list.gas.sensors()
                
                ind.warm$out <- return.ind.warm
                # Setting TRh$Forced to TRUE to be sure that it is done before ind.Sens 
                TRh$Forced - TRUE
                
                progress$set(message = "[shiny, ind.warm()] INFO, Setting the index of dates for sensor warming time", value = 1)
            }  
        },
        priority = 195)
        
        # NavBar"Data Treatment", MainTabPanel "Warming" - "PlotFiltering"  ----
        output$Warming <- renderPlot(Plot.Warming(), width = 'auto', height = 'auto')
        # NavBar"Data Treatment", Reactive Plot.Warming
        Plot.Warming   <- reactive({
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            progress$set(message = "[shiny, Plot.Warming()] INFO, Plotting raw data during warming in red dots", value = 0.5)
            
            # Preparing graphical parameters for the number of plots equals to the number of sensors
            op <- par(no.readonly = TRUE)
            par(mfrow = c(ceiling(length(list.name.sensors())/2), 2))
            # Restoring graphical parameters on exit of function
            on.exit(par(op))
            
            # Selecting date to be plotted
            General.df <- selectByDate(DF$General, start = input$Out.Sens.Date1[1], end = input$Out.Sens.Date1[2])
            
            cat("-----------------------------------------------------------------------------------\n")
            # checking the initial number of valid sensor values
            unlist(lapply(list.gas.sensors()[!is.na(list.name.sensors())], function(i) {
                cat(paste0("[shiny, Plot.Warming()] INFO, sensor ", 
                           Config()[[2]]$name.sensor[match(x = i, table = Config()[[2]]$gas.sensor)],
                           " starting with ", length(which(!is.na(General.df[,i]))), " valid measurements between ", 
                           format(input$Out.Sens.Date1[1],"%Y-%m-%d  %H:%M"), " and ",format(input$Out.Sens.Date1[2],"%Y-%m-%d  %H:%M"),".\n")
                )
            }))
            cat("-----------------------------------------------------------------------------------\n")
            
            # date in ind.warm$out : all dates even the invalid ones and the one valid that are not selected
            Index.warm     <- lapply(ind.warm$out, function(x) {DF$General$date[x]} ) 
            if (length(Index.warm) != 0) {
                # plotting the data discarded for all sensors
                for (i in list.gas.sensors()) {
                    if (!is.null(ind.warm$out[[i]])) {
                        # index of valid sensor corresponding to i
                        j <- match(x = i, table = Config()[[2]]$gas.sensor)
                        GraphOut(date  = General.df$date,  
                                 y     = General.df[,i], 
                                 Col   = "green", 
                                 Ylab  = "Raw Sensor values", 
                                 ind   = which(General.df$date %in% unlist(Index.warm[i])),
                                 Title = (paste0("Data invalidated during warming of sensor ", Config()[[2]]$name.sensor[j], 
                                                 " for ", isolate(input[[paste0("Warming",j)]]), " hours after each switch-on."))
                        )
                    } else {
                        cat(paste0("[shiny, Plot.Warming()] INFO, There is no warming data to discard for ", Config()[[2]]$name.sensor[j], "\n"))
                        plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                        text(1,1,paste0("[shiny, Plot.Warming()] INFO, There is no warming data to discard for ", Config()[[2]]$name.sensor[j], "\n"))
                    }
                    cat(paste0("[shiny, Plot.Warming()] INFO, sensor ", Config()[[2]]$name.sensor[j], 
                               ", ",length(which(!is.na(General.df[,paste0("Out.Warm.",i)]))),
                               " valid data after removing ",length(which(General.df$date %in% unlist(Index.warm[i]))),
                               " values during ", isolate(input[[paste0("Warming", j)]])," hours of warming at each switch-on\n"))
                }
                # Saving plot if requested
                if (input$SavePlot) {
                    WDoutput <- file.path(DisqueFieldtestDir(), "Outliers")
                    dev.copy(png,filename = file.path(WDoutput, paste0(AirsensEur.name(),"_Warming.png")), 
                             #units = "cm", 
                             #width = 35.55, 
                             #height = 20,
                             res = 300 
                    )
                    dev.off()
                    cat(paste0("[shiny] INFO, ", AirsensEur.name(),"_Warming.png saved in ", WDoutput, "\n" ))
                    updateCheckboxInput(session, 
                                        inputId = "SavePlot", 
                                        label = NULL, 
                                        value = FALSE)
                }
            } else {
                cat(paste0("[shiny, Plot.Warming()] INFO, There is no warming data to discard \n"))
                plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                text(1,1,paste0("[shiny, Plot.Warming()] INFO, There is no warming data to discard \n")) 
            }
            
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            
            progress$set(message = "[shiny, Plot.Warming()] INFO, Plotting raw data during warming in red dots", value = 1)
            on.exit(progress$close())
            
            # Opening the Filtering TabSet for GUI consistency
            isolate({
                #updateTabsetPanel(session, inputId = "Calib_data" , selected = "Filtering")
                updateTabsetPanel(session, inputId = "Treatments" , selected = "Sensors")
                updateTabsetPanel(session, inputId = "Filtering.Sensors", selected = isolate(input$Sensors))
            })
            
        })
        
        # Reactive ind.TRh ----
        # Flagging the sensor data for Temperature and Humidity
        TRh <- reactiveValues(Forced = FALSE)
        observeEvent({
            unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("Apply.TRh", i)]]))
        },{
            
            # Reactive function to trigger for temperature/humidity tolerance
            # TRh$Forced is TRUE if Apply.TRh     is TRUE
            #                                 
            # depends: 
            #       list.gas.sensors()
            #       DF$General
            #       input$Apply.TRh
            
            if (any(unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("Apply.TRh", i)]]))) 
            ) TRh$Forced <- TRUE else TRh$Forced <- FALSE
        }, 
        ignoreInit = TRUE,
        priority = 190
        )
        ind.TRh.file <- file.path(DisqueFieldtestDir(), "General_data" , "ind_TRh.RDS"  )
        if (file.exists(ind.TRh.file))  init.ind.TRh <- list.load(ind.TRh.file) else {init.ind.TRh <- NULL; TRh$Forced <- TRUE}
        ind.TRh <- reactiveValues(out = init.ind.TRh) 
        observeEvent(TRh$Forced, {
            # Input:
            #   General.df           : dataframe General created by reactive function DF$General merfing influx, SOS and reference data.
            #   INFLUX()[[2]]        : charater vector with the name of meteorological parameters ("Temperature","Relative_humidity" and "Atmospheric_pressure")
            #   list.gas.sensors()   : charater vector with the name of sensor gas compounds ("Carbon_monoxide","Nitric_oxide","Nitrogen_dioxide" and "Ozone")
            # Output:                : list of NAs for discarded temperature and humidity with as many elements as in list.gas.sensors() 
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
                for (l in list.gas.sensors()) {
                    # Global index of temperature/humidity exceeding thresholds
                    ind <- (DF$General[, index.temp]   < input[[paste0("Temperature",match(x = l, table = list.gas.sensors()))]][1]  | 
                                DF$General[, index.temp]   > input[[paste0("Temperature",match(x = l, table = list.gas.sensors()))]][2]) | 
                        (DF$General[, index.rh]     < input[[paste0("Humidity"   ,match(x = l, table = list.gas.sensors()))]][1]  | 
                             DF$General[, index.rh]     > input[[paste0("Humidity"   ,match(x = l, table = list.gas.sensors()))]][2])
                    
                    # Global index of temperature/humidity exceeding thresholds
                    T.min  <- DF$General[, index.temp] < input[[paste0("Temperature",match(x = l, table = list.gas.sensors()))]][1]
                    T.max  <- DF$General[, index.temp] > input[[paste0("Temperature",match(x = l, table = list.gas.sensors()))]][2]
                    Rh.min <- DF$General[, index.rh]   < input[[paste0("Humidity"   ,match(x = l, table = list.gas.sensors()))]][1]
                    Rh.max <- DF$General[, index.rh]   > input[[paste0("Humidity"   ,match(x = l, table = list.gas.sensors()))]][2]
                    
                    # if (exists("return.ind.TRh")) {
                    return.ind.TRh[[na.omit(Config()[[2]][Config()[[2]]$gas.sensor == l,"name.sensor"])]]    <- which(ind)
                    return.ind.T.min[[ paste0(na.omit(Config()[[2]][Config()[[2]]$gas.sensor == l,"name.sensor"]),"__Temp. < ",input[[paste0("Temperature",match(x = l, table = list.gas.sensors()))]][1])]] <- which(T.min)
                    return.ind.T.max[[ paste0(na.omit(Config()[[2]][Config()[[2]]$gas.sensor == l,"name.sensor"]),"__Temp. > ",input[[paste0("Temperature",match(x = l, table = list.gas.sensors()))]][2])]] <- which(T.max)
                    return.ind.Rh.min[[paste0(na.omit(Config()[[2]][Config()[[2]]$gas.sensor == l,"name.sensor"]),"__RH < "   ,input[[paste0("Humidity"   ,match(x = l, table = list.gas.sensors()))]][1])]] <- which(Rh.min)
                    return.ind.Rh.max[[paste0(na.omit(Config()[[2]][Config()[[2]]$gas.sensor == l,"name.sensor"]),"__RH > "   ,input[[paste0("Humidity"   ,match(x = l, table = list.gas.sensors()))]][2])]] <- which(Rh.max)
                }
                
                ind.TRh$out <- list(ind.TRh = return.ind.TRh, T.min = return.ind.T.min, T.max = return.ind.T.max, Rh.min = return.ind.Rh.min, Rh.max = return.ind.Rh.max)
                # Setting Invalid$Forced to True to be sure that it is carried out before ind.sens
                Inv$Forced <- TRUE
                
                progress$set(message = "[shiny, ind.TRh()] INFO, Setting the index temperature and Humidity out of tolerance", value = 1)
                progress$close()
            }  
        },
        priority = 185
        )
        
        # NavBar"Data Treatment", MainTabPanel "Temp&Humid" - "PlotFiltering"  ----
        output$Temp.Humid    <- renderPlot(Plot.Temp.Humid(), width = 'auto', height = 'auto')
        # Reactive Plot.Temp.Humid
        Plot.Temp.Humid      <- reactive({
            # Plot NA for the sensor data out of temperature and RH validity ranges for each raw sensors
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            progress$set(message = "[shiny, Plot.Temp.Humid()] INFO, Plotting data out of temperature/humidity tolerance", value = 0.20)
            
            WDoutput <- file.path(DisqueFieldtestDir(), "Outliers")
            op <- par(no.readonly = TRUE)
            par(mfrow = c(ceiling(length(list.name.sensors())/2), 2))
            # Restoring graphical parameters on exit of function
            on.exit(par(op))
            
            # Selecting date to be plotted
            General.df <- selectByDate(DF$General, start = input$Out.Sens.Date1[1], end = input$Out.Sens.Date1[2])
            # Creating a list of dates with values out of tolerance of T and RH : T.min, T.max, Rh.min and Rh.maxin in ind.TRh$out
            Index.TRh <- lapply(ind.TRh$out$ind.TRh, function(x) {DF$General$date[x]} ) 
            T.min     <- lapply(ind.TRh$out$T.min  , function(x) {DF$General$date[x]} ) 
            T.max     <- lapply(ind.TRh$out$T.max  , function(x) {DF$General$date[x]} ) 
            Rh.min    <- lapply(ind.TRh$out$Rh.min , function(x) {DF$General$date[x]} ) 
            Rh.max    <- lapply(ind.TRh$out$Rh.max , function(x) {DF$General$date[x]} )
            
            cat("-----------------------------------------------------------------------------------\n")
            if (length(Index.TRh) != 0) {
                for (i in list.gas.sensors()) {
                    # index of valid sensor corresponding to i
                    j <- match(x = i, table = Config()[[2]]$gas.sensor)
                    # progress$set(message = "Plotting data out of temperature/humidity tolerance", value = 0.40)
                    # name of sensors
                    name.sensor <-  Config()[[2]][j,"name.sensor"]
                    
                    if (!is.null(Index.TRh[name.sensor])) {
                        ind = list(
                            T.min[[ names(T.min)[ grep(name.sensor, x = names(T.min))] ]],
                            T.max[[ names(T.max)[ grep(name.sensor, x = names(T.max))] ]],
                            Rh.min[[names(Rh.min)[grep(name.sensor, x = names(Rh.min))] ]],
                            Rh.max[[names(Rh.max)[grep(name.sensor, x = names(Rh.max))] ]]
                            
                        )
                        names(ind) <-  c(unlist(strsplit(names(T.min)[  match(i, list.gas.sensors())], split = "__"))[2],
                                         unlist(strsplit(names(T.max)[  match(i, list.gas.sensors())], split = "__"))[2],
                                         unlist(strsplit(names(Rh.min)[ match(i, list.gas.sensors())], split = "__"))[2], 
                                         unlist(strsplit(names(Rh.max)[ match(i, list.gas.sensors())], split = "__"))[2])
                        GraphOut(date = General.df$date,
                                 y     = General.df[,i],
                                 Col   = "green",
                                 Ylab  = "Raw Sensor values",
                                 ind   = ind,
                                 Title = paste0("Data invalidated for temperature or humidity outside thresholds for sensor ", 
                                                Config()[[2]]$name.sensor[j])
                        )
                        Total.TRh <- length(which(General.df$date %in% unlist(Index.TRh[Config()[[2]][which(Config()[[2]]$gas.sensor == i),"name.sensor"]])))
                        cat(paste0("[shiny, Plot.Temp.Humid()] INFO, for sensor ", Config()[[2]]$name.sensor[j],": ",
                                   length(which(!is.na(General.df[,paste0("Out.Warm.TRh.",i)]))),
                                   " valid data after removing sensor warm up data and ", 
                                   length(which(General.df$date %in% unlist(Index.TRh[Config()[[2]][which(Config()[[2]]$gas.sensor == i),"name.sensor"]]))), 
                                   " values outside temperature/humidity intervals\n"))
                    } else{
                        cat(paste0("[shiny, Plot.Temp.Humid()] INFO, sensor ", Config()[[2]]$name.sensor[j], 
                                   " was not used outside temperature or humidity validity ranges."), sep = "\n")
                        plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                        text(1,1,paste0("[shiny, Plot.Temp.Humid()] INFO, INFO, sensor ", Config()[[2]]$name.sensor[j], 
                                        " was not used outside temperature or humidity validity ranges."))
                    }
                } 
                
                # Saving plot if requested
                if (input$SavePlot) {
                    dev.copy(png,filename = file.path(WDoutput, paste0(AirsensEur.name(),"_TRh.png")), 
                             #units = "cm", 
                             #width = 35.55, 
                             #height = 20,
                             res = 300
                    )
                    dev.off()
                    cat(paste0("[shiny] INFO, ", AirsensEur.name(),"_TRh.png saved in ", WDoutput, "\n" ))
                    updateCheckboxInput(session, 
                                        inputId = "SavePlot", 
                                        label = NULL, 
                                        value = FALSE)
                }
                
            } else {
                cat(paste0("[shiny, Plot.Temp.Humid()] INFO,, no data outside temperature or humidity validity ranges."), sep = "\n")
                plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                text(1,1,paste0("[shiny, Plot.Temp.Humid()] INFO, INFO, no data outside temperature or humidity validity ranges."))
            }
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            
            progress$set(message = "[shiny, Plot.Temp.Humid()] INFO, Plotting data out of temperature/humidity tolerance", value = 1)
            
            # Opening the Filtering TabSet for GUI consistency
            isolate({
                #updateTabsetPanel(session, inputId = "Calib_data" , selected = "Filtering")
                updateTabsetPanel(session, inputId = "Treatments" , selected = "Sensors")
                updateTabsetPanel(session, inputId = "Filtering.Sensors", selected = isolate(input$Sensors))
            })
        })
        
        # NavBar"Data Treatment, MmainTabPanel "Neg.values" - "PlotFiltering" ---- 
        output$Neg.values    <- renderPlot(Plot.Neg.values(), width = 'auto', height = 'auto')
        # reactive Plot.Neg.values
        Plot.Neg.values      <- reactive({
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            # # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            Tot.Iter <- length(list.gas.reference2use())
            rate <- 1/(Tot.Iter + 2)
            ValueRate <- rate
            progress$set(message = "[Shiny, Plot.Neg.values()] INFO, Plotting discarded negative reference data", value = ValueRate)
            
            # Removing negative reference values
            op <- par(no.readonly = TRUE)
            par(mfrow = c(ceiling(length(list.gas.reference2use())/2), 2))
            # Restoring graphical parameters on exit of function
            on.exit(par(op))
            
            cat("-----------------------------------------------------------------------------------\n")
            # Inital count of reference
            for (i in list.gas.reference2use()) cat(paste0("[Shiny]Plot.Neg.values, INFO, number of valid measurements for ", i,
                                                           " before removing negative values: ", length(which(!is.na(DF$General[,i])))), sep = "\n")
            # Discarding negative values
            # list of negative values for all pollutants
            ind.neg <- apply(X = DF$General[,list.gas.reference2use()], MARGIN = 2, function(x) {which(x < 0)})
            if (length(ind.neg) != 0) {
                for (i in list.gas.reference2use()) {
                    
                    # ValueRate <- ValueRate + rate
                    progress$set(message = "[Shiny, Plot.Neg.values()] INFO, Plotting discarded negative reference data", value = ValueRate)
                    
                    # number index of reference in the list of references
                    j <- match(x = i, table = list.gas.reference2use())
                    if (input[[paste0("rm.neg",j)]]) {
                        
                        if (any(ind.neg[[j]], na.rm = TRUE)) {
                            
                            General.df <- subset(DF$General, 
                                                 DF$General$date >= input[[paste0("Out.Ref.Date",j)]][1] & DF$General$date <= input[[paste0("Out.Ref.Date",j)]][2]
                            )
                            
                            if (!is.na(any(match(DF$General$date[ind.neg[[j]]],General.df$date)))) {
                                
                                GraphOut(date  = General.df$date,
                                         y     = General.df[,i],
                                         Col   = "green",
                                         Ylab  = "Reference values",
                                         ind   = match(DF$General$date[ind.neg[[j]]],General.df$date),
                                         Title = (paste0("Negative reference data invalidated for ", i))
                                )
                                cat(paste0("[Shiny]Plot.Neg.values, INFO, reference pollutant ", i, " number of negative data data ",
                                           length(ind.neg[[j]]), "\n"))
                            } else {
                                
                                cat(paste0("[Shiny]Plot.Neg.values, INFO, There is no negative values for ", i, "\n"))
                                plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                                text(1,1,paste0("[Shiny]Plot.Neg.values, INFO, There is no negative values for ", i, "\n"))
                            }
                            
                        } else {
                            
                            cat(paste0("[Shiny]Plot.Neg.values, INFO, There is no negative values for ", i, "\n"))
                            plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                            text(1,1,paste0("[Shiny]Plot.Neg.values, INFO, There is no negative values for ", i, "\n"))
                        } 
                        
                    } else {
                        
                        cat(paste0("[Shiny]Plot.Neg.values, INFO, Removing negative values not requested for ", i, "\n"))
                        plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                        text(1,1,paste0("[Shiny]Plot.Neg.values, INFO, Removing negative values not requested for ", i, "\n"))
                    }
                }
                # Saving plot if requested
                if (input$SavePlot) {
                    WDoutput <- file.path(DisqueFieldtestDir(), "Outliers")
                    dev.copy(png, filename = file.path(WDoutput, paste0(AirsensEur.name(),"_RefNeg.png")) , 
                             units = "cm", 
                             #res = 300, 
                             #width = 35.55, 
                             #height = 20,
                             res = 300 
                    )
                    dev.off()
                    cat(paste0("[shiny] INFO, ", AirsensEur.name(),"_RefNeg.png saved in ", WDoutput, "\n" ))
                    updateCheckboxInput(session, 
                                        inputId = "SavePlot", 
                                        label = NULL, 
                                        value = FALSE)
                }
            } else {
                
                cat(paste0("[Shiny]Plot.Neg.values, INFO, no negative reference values\n"))
                plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                text(1,1,paste0("[Shiny]Plot.Neg.values, INFO, no negative reference values\n"))
            }
            # New count of reference values
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            
            progress$set(message = "[Shiny, Plot.Neg.values()] INFO, Plotting discarded negative reference data", value = 1)
            on.exit(progress$close())
            
            # Opening the Filtering TabSet for GUI consistency
            isolate({
                updateTabsetPanel(session, inputId = "Calib_data", selected = "Filtering")
                updateTabsetPanel(session, inputId = "Treatments", selected = "Reference")
            })
        })
        
        # NavBar"Data Treatment", mainTabPanel "Invalid"-"PlotFiltering"  ,  ----
        output$Invalid.Sens    <- renderPlot(Plot.Invalid.Sens(), width = 'auto', height = 750)
        # Reactive Invalid.DF
        Invalid.DF <- reactive({
            # depends on: 
            #input$Sensors
            #ASE_name()
            #min.General.date()
            
            nameFile <- file.path(DisqueFieldtestDir(),"General_data",paste0(ASE_name(),"_Valid_",input$Sensors,".cfg"))
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
                                 Comments         = " ",
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
            nameFile <- file.path(DisqueFieldtestDir(),"General_data",paste0(ASE_name(),"_Valid_",input$Sensors,".cfg"))
            write.table(finalDF, file = nameFile, row.names = FALSE)
            
        })
        ## Delete 
        observeEvent(input$Del.row.Valid, {
            # rows must be in increasing order before saving otherwise the following error stops the script:
            # Warning: Error in seq.default: 'by' must be of length 1
            
            nameFile <- file.path(DisqueFieldtestDir(),"General_data",paste0(ASE_name(),"_Valid_",input$Sensors,".cfg"))
            if (file.exists(nameFile)) file.remove(nameFile)
            
            
            # Need to reset Invalid.DF  ################################################################################################
            
        })
        
        # Reactive ind.Invalid ----
        # Flagging the sensor data for Invalid ensor data
        Inv <- reactiveValues(Forced = FALSE)
        observeEvent({
            unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("Apply.Invalid", i)]]))
        },
        {
            
            # Reactive function to trigger for Warming time, temperature/humidity tolerance, negative reference values and invalid, DF$General
            # Inv$Forced is TRUE if Apply.Invalid is TRUE
            # depends: 
            #       list.gas.sensors()
            #       DF$General
            #       input$Apply.Invalid
            
            if (any(unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("Apply.Invalid", i)]])))
            ) Inv$Forced <- TRUE else Inv$Forced <- FALSE
        }, 
        ignoreInit = TRUE,
        priority = 170
        )
        ind.Invalid.file <- file.path(DisqueFieldtestDir(), "General_data" , "ind_Invalid.RDS")
        if (file.exists(ind.Invalid.file)) init.ind.Invalid <- list.load(ind.Invalid.file) else {init.ind.Invalid <- NULL; Inv$Forced <- TRUE}
        ind.Invalid <- reactiveValues(out = init.ind.Invalid)
        observeEvent(Inv$Forced, {
            
            # inputs: 
            #   input$Apply.Invalid1,2,3,4  : crosscheckboxes requiring to check DF$General for warming  time
            #   DF$General                  : reactive function passing a dataframe merging data from Influx, SOS and reference
            #   list.gas.sensors()          : character vector giving the gas compounds of sensors ("Carbon_monoxide", "Nitric_oxide","Nitrogen_dioxide" and "Ozone")
            #   input$UserMins              : integer number of minutes to average raw data
            #   
            # output: a list of 4 character vectors, corresponding to sensors with row index of DF$General corresponding to Invalidf sensor data,
            #       the names of the 4 elements are the ones of list.gas.sensors()   in the same order
            
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
                    
                    for (i in 1:length(list.name.sensors())) {
                        
                        nameFile <- file.path(DisqueFieldtestDir(),"General_data",paste0(AirsensEur.name(),"_Valid_",list.name.sensors()[i],".cfg"))
                        
                        if (file.exists(nameFile)) {
                            
                            cat(paste0("[shiny, ind.Invalid()] INFO, the file with valid periods of sensor data ", nameFile, " exists "), sep = "\n")
                            assign(paste0("Valid_",list.name.sensors()[i]), read.table(file = nameFile, header = TRUE, row.names = NULL, comment.char = "#", stringsAsFactors = FALSE))
                            cat(paste0("[shiny, ind.Invalid()] INFO, ", get(paste0("Valid_",list.name.sensors()[i]))), sep = "\n")
                            
                        } else {
                            
                            # There are no Valid files. Creates files with IN = END = min(General$date)
                            cat(paste0("[shiny, ind.Invalid()] INFO, the files with valid periods of sensor data ", nameFile, " do not exist. Set validity to the whole available time interval"), sep = "\n")
                            assign(paste0("Valid_",list.name.sensors()[i]), rbind(c(strftime(min(DF$General$date, na.rm = TRUE)), strftime(min(DF$General$date, na.rm = TRUE)))))
                            write.table(x         = data.frame(In = gsub(" UTC", "",strftime(min(DF$General$date, na.rm = TRUE))), 
                                                               End = gsub(" UTC", "",strftime(min(DF$General$date, na.rm = TRUE))), 
                                                               stringsAsFactors = FALSE), 
                                        file      = nameFile, 
                                        row.names = FALSE
                            )
                        }
                    }
                    
                    # Creating one list with invalid periods for all sensors
                    Valid <- list()
                    for (i in paste0("Valid_",list.name.sensors())) Valid[[i]] <- get(i)
                    
                    # Function to convert charater strings to POSIX
                    NewValid <- function(x) {
                        # making each element a dataframe of POSIXct
                        x <- data.frame( x, stringsAsFactors = FALSE)
                        colnames(x) <- c("In", "End")
                        x$In  <- parse_date_time(x$In , tz = input$ref.tzone, orders = "YmdHMS") 
                        x$End <- parse_date_time(x$End, tz = input$ref.tzone, orders = "YmdHMS") 
                        return(x)
                    }
                    Valid.date <- lapply(Valid, NewValid)
                    
                    cat("[shiny, ind.Invalid()] INFO, SET VALID TIME parameters\n")
                    
                    # Set inital date for data retrieving (i.e. maximum length time period for data retrieval). 
                    # These dates may change according to the data availability
                    # UserDateIN.0, SOS.TZ is set in ASEConfig_MG.R
                    # Set correct time zone
                    
                    # list of invalid date to be used for sensor Evaluation with reference values, a kind of life cycle of each sensor - time zone shall be the same as SOS.TZ (UTC?)
                    #browser()
                    # list of valid date per sensor
                    # seting invalid to NA and create a list for plotting invalids
                    ind.Inval <- list()        
                    for (i in gsub(pattern = "Valid_", replacement = "", names(Valid.date))) { 
                        for (j in 1:nrow(Valid.date[[paste0("Valid_",i)]])) {
                            
                            if (length(which(DF$General$date > Valid.date[[paste0("Valid_",i)]]$In[j] & 
                                             DF$General$date < Valid.date[[paste0("Valid_",i)]]$End[j])) > 0) {
                                
                                if (!(i %in% names(ind.Inval))) {
                                    ind.Inval[[i]] <- DF$General[which(DF$General$date > Valid.date[[paste0("Valid_",i)]]$In[j] & 
                                                                             DF$General$date < Valid.date[[paste0("Valid_",i)]]$End[j]),"date"]  
                                } else {
                                    ind.Inval[[i]] <- c(ind.Inval[[i]], DF$General[which(DF$General$date > Valid.date[[paste0("Valid_",i)]]$In[j] & 
                                                                                                 DF$General$date < Valid.date[[paste0("Valid_",i)]]$End[j]),"date"])
                                }
                            } 
                        }
                    }
                }
                cat("-----------------------------------------------------------------------------------\n")
                ind.Invalid$out <- list(Valid.date,ind.Inval)
                
                # make sure that ind.Sens() is run after Invalid, to discard outliers again and to apply invalid and outliers to DF$General
                Outliers.Sens$Forced <- TRUE
                
                progress$set(message = "Setting the index of dates for Invalid sensor data", value = 1)
            }  
        },
        ignoreInit = TRUE,
        priority = 165
        )
        # Reactive Plot.Invalid.Sens ----
        Plot.Invalid.Sens      <- reactive({
            
            # Plotting Invalid data for sensor data before Outlier detection
            # depends: Config()[[2]]$gas.sensor, input$Sensors, Config()[[2]]$name.sensor, i.sensors(), 
            #          DF$General : dataFrame with invalidated data, 
            #          ind.Invalid$out[[2]] : list with invalid date
            #          input[[paste0("Out.Sens.Date",k)]],DisqueFieldtestDir(),
            #           
            # isolate: input$Sensors
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            progress$set(message = "[Shiny, Plot.Inv()] INFO, Plotting discarded invalid sensor data", value = 0.2)
            
            # Selecting date to be plotted
            General.df <- subset(DF$General, DF$General$date >= input$Out.Sens.Date1[1] &  DF$General$date <= input$Out.Sens.Date1[2])
            
            
            cat("-----------------------------------------------------------------------------------\n")
            cat("[Shiny, Plot.Inv()] INFO, plotting invalid sensor data\n")
            
            # Preparing graphical parameters for the number of plots equals to the number of sensors
            op <- par(no.readonly = TRUE)
            par(mfrow = c(ceiling(length(list.gas.sensors())/2), 2))
            # Restoring graphical parameters on exit of function
            on.exit(par(op))
            
            # Checking if the list of invalid date is empty
            if (length(ind.Invalid$out[[2]]) != 0) {
                
                # plotting the data discarded for all sensors
                for (i in list.name.sensors()) {
                    
                    # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                    k <-  match(x = i, table = Config()[[2]]$name.sensor)
                    j <-  Config()[[2]]$gas.sensor[k]
                    
                    # checking that discarding of invalid data is requested
                    if (input[[paste0("Sens.Inval.Out", match(x = i, table = list.name.sensors()))]]) {
                        
                        if (!is.null(ind.Invalid$out[[2]][[i]])) {
                            
                            # Selecting the species associated with sensor in rows of ASE_name.cfg 
                            GraphOut(date  = General.df$date,
                                     y     = General.df[,j], 
                                     Col   = "green", 
                                     Ylab  = "Raw Sensor values", 
                                     ind   = which(General.df$date %in% unlist(ind.Invalid$out[[2]][i])),
                                     Title = paste0("Data invalidated, dates outside valid period, for sensor ", i)
                            )
                        } else {
                            cat(paste0("[Shiny, Plot.Inv()] INFO, There is no Invalid data to discard for sensor ", i, "\n"))
                            plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                            text(1,1,paste0("[Shiny, Plot.Inv()] INFO, There is no invalid data to discard for sensor ", i, "\n"))
                        }
                        
                    } else {
                        cat(paste0("[Shiny, Plot.Inv()] INFO, Discarding of invalid data not requested for sensor ",i,"\n"))
                        plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                        text(1,1,paste0("[Shiny, Plot.Inv()] INFO, Discarding of invalid data not requested for sensor ",i,"\n"))
                    }   
                    
                    cat(paste0("[Shiny, Plot.Inv()] INFO, sensor ", i, 
                               ", ",length(which(!is.na(DF$General[,paste0("Out.",j)]))),
                               " valid data within validity period removing ",length(which(General.df$date %in% ind.Invalid$out[[2]][[i]])),
                               " values outside validity period.\n"))
                }
                # Saving plot if requested
                if (input$SavePlot) {
                    WDoutput <- file.path(DisqueFieldtestDir(), "Outliers")
                    dev.copy(png,
                             filename = file.path(WDoutput, paste0(AirsensEur.name(),"_Invalid.png")), 
                             #units = "cm", 
                             #width = 35.55, 
                             #height = 20,
                             res = 300 
                    )
                    dev.off()
                    cat(paste0("[shiny] INFO, ", AirsensEur.name(),"_Invalid.png saved in ", WDoutput, "\n" ))
                    updateCheckboxInput(session, 
                                        inputId = "SavePlot", 
                                        label = NULL, 
                                        value = FALSE)
                }
            } else {
                cat(paste0("[Shiny, Plot.Inv()] INFO, There is no invalid data to discard \n"))
                plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                text(1,1,paste0("[Shiny, Plot.Inv()] INFO, There is no invalid data to discard \n")) 
            }
            
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            
            progress$set(message = "[Shiny, Plot.Inv()] INFO, Plotting discarded invalid sensor data", value = 1)
            on.exit(progress$close())
            
            # Opening the Filtering TabSet for GUI consistency
            isolate({
                updateTabsetPanel(session, inputId = "Calib_data" , selected = "Invalid")
                updateTabsetPanel(session, inputId = "Treatments" , selected = "Sensors")
                updateTabsetPanel(session, inputId = "Filtering.Sensors", selected = isolate(input$Sensors))
            })
            
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
        })
        
        # NavBar"Data Treatment", mainTabPanel "Outliers" - "PlotFiltering", ----
        # Flagging outliers
        # Reactive ind.sens.out ----
        Outliers.Sens <- reactiveValues(Forced = FALSE)
        observeEvent({
            unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("Apply.S.Out", i)]]))
            Inv$Forced
        },{
            # Reactive function to trigger detection of Outliers in sensor data
            # Outliers.Sens$Forced is TRUE if Warm$Forced | TRh$Forced | Inv$Forced is TRUE 
            #                         if Apply.S.Out is TRUE or 
            # depends: 
            #       list.gas.sensors()
            #       Warm$Forced | TRh$Forced | Inv$Forced
            #       input$Apply.S.Out
            
            if ( Warm$Forced | TRh$Forced | Inv$Forced |
                 any(unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("Apply.S.Out", i)]])))
            ) Outliers.Sens$Forced = TRUE else Outliers.Sens$Forced = FALSE
        },
        priority = 150)
        ind.sens.out.file   = file.path(DisqueFieldtestDir(), "General_data", "ind_sens_out.RDS"  )
        if (file.exists(ind.sens.out.file)) Init.ind.sens <- list.load(ind.sens.out.file) else {Init.ind.sens = NULL; Outliers.Sens$Forced <- TRUE}
        ind.sens <- reactiveValues(out = Init.ind.sens) 
        observeEvent({Outliers.Sens$Forced},{
            
            # Return a list:
            #   - dataframe : name.sensor.iteration: date + for each sensor and each iteration : lowvalues, highvalues, outlierMin and Outliers max 
            #                                         with values TRUE if sensor data are outliers and FALSE if not, + zmin and zmax the interval of tolerance for not being an outlier
            #   - dataframe : name.sensor.iteration: date + for each sensor and each iteration: TRUE if outliers and FALSE if not
            
            # Setting the index of outliers for sensors before calibration
            
            # Only if "apply filter" for outliers of sensors is selected
            if (Outliers.Sens$Forced) {
                
                progress <- shiny::Progress$new()
                on.exit(progress$close())
                progress$set(message = "[shiny, ind.sens()] INFO, purging for Warming, temperature/humidity tolerance and invalid", value = 0.10)
                cat("-----------------------------------------------------------------------------------\n")
                cat("[shiny, ind.sens()] INFO, purging for Warming, temperature/humidity tolerance and invalid\n")
               
                Tot.Iter <- sum(sapply(1:length(list.gas.sensors()), function(x) as.numeric(input[[paste0("Sens.iterations",x)]])))
                rate     <- 1/(Tot.Iter + 2)
                ValueRate <- rate
                progress$set(message = "[shiny, ind.sens()] INFO, Setting index of outliers in sensor data", value = ValueRate)
                cat("[shiny, ind.sens()] INFO, detecting row indexes of outliers in sensor data\n")
                
                for (i in list.gas.sensors()  ) {
                    
                    # Initialisation of columns of DF$General

                    Sensor.i <- na.omit(Config()[[2]][Config()[[2]][,"gas.sensor"] == i,"name.sensor"])
                    # resetting to initial values
                    progress$set(message = "[shiny, ind.sens()] INFO, Initialising filtered data columns", value = 0.165)
                    cat("[shiny, ind.sens()] INFO, Initialising filtered data columns for ", i, "\n")
                    Vector.columns <- c("Out.", "Out.Warm.", "Out.TRh.", "Out.Invalid.", "Out.Warm.TRh.", "Out.Warm.TRh.Inv.")
                    DF$General[, paste0(Vector.columns,i)] <- sapply(1:length(Vector.columns), function(x) return(DF$General[,i]))

                    progress$set(message = "[shiny, ind.sens()] INFO, Discarding sensor data for Warming", value = 0.33)
                    cat("[shiny, ind.sens()] INFO, Discarding sensor data for Warming for ", i, "\n")
                    if (!is.null(ind.warm$out[i][[1]])) {
                        Vector.columns <- c("Out.", "Out.Warm.", "Out.Warm.TRh.", "Out.Warm.TRh.Inv.")
                        DF$General[ind.warm$out[i][[1]], paste0(Vector.columns, i)] <- NA
                    }

                    progress$set(message = "[shiny, ind.sens()] INFO, Discarding data outside temperature and RH validity ranges", value = 0.66)
                    cat("[shiny, ind.sens()] INFO, Excluding sensor data outside temperature and RH validity ranges for ", i, "\n")
                    if (!is.null(ind.TRh$out$ind.TR[Sensor.i][[1]])) {
                        Vector.columns <- c("Out.", "Out.TRh.", "Out.Warm.TRh.", "Out.Warm.TRh.Inv.")
                        DF$General[ind.TRh$out$ind.TRh[Sensor.i][[1]], paste0(Vector.columns,i)] <- NA
                    }

                    progress$set(message = "[shiny, ind.sens()] INFO, Excluding invalid sensor data", value = 0.85)
                    cat("[shiny, ind.sens()] INFO, Excluding invalid sensor data for ", i, "\n")
                    if (!is.null(ind.Invalid$out[[2]][Sensor.i][[1]])) {
                        Vector.columns <- c("Out.", "Out.Invalid." , "Out.Warm.TRh.Inv.")
                        DF$General[which(DF$General$date %in% ind.Invalid$out[[2]][Sensor.i][[1]]), paste0(Vector.columns, i)] <- NA
                    }

                    progress$set(message = "[shiny, ind.sens()] INFO, initialising outlier sensor data", value = 1.0)
                    cat("[shiny, ind.sens()] INFO, initialising outlier sensor data for ", i, "\n")
                    # index (1, 2,3, 4  or 1,2,3, 6 ... comng from  selection of control uiFiltering, Calib and SetTime)
                    k <- match(x = i, table = list.gas.sensors())
                    for (j in 1:input[[paste0("Sens.iterations", k)]]) {
                        # initialising the column of outlier iteration j
                        DF$General[, paste0("Out.",i,".",j)] <- DF$General[,paste0("Out.",i)]
                    }
                    # deleting bigger iterations
                    j  <- input[[paste0("Sens.iterations", k)]]
                    repeat (
                        if (any(grepl(pattern = paste0("Out.",i,".", j + 1)      , x = names(DF$General)))) {
                            DF$General[,grep(pattern = paste0("Out.",i,".",j + 1), x = names(DF$General))] <- NULL
                            j <- j + 1
                        } else break # leaving the Repeat if there are no higher iterations
                    )

                    if (input[[paste0("Sens.rm.Out",k)]]) {

                        for (j in 1:input[[paste0("Sens.iterations",k)]]) { # numver of iterations
                            ValueRate <- ValueRate + rate
                            progress$set(message = "[shiny, ind.sens()] INFO, Setting index of outliers in sensor data", value = ValueRate)

                            if (all(is.na(DF$General[,i]))) {
                            } else {

                                # Setting the columns of sensor data previous to detect outliers
                                Y <- DF$General[,paste0("Out.Warm.TRh.Inv.",i)]

                                # setting Y for the outliers of previous iterations to NA. If null then stop outlier detection
                                if (j > 1) {

                                    if (length(which(return.ind.sens.out[[paste0(i,".",(j - 1))]]$Outliers)) != 0) {

                                        Y[as.numeric(paste(unlist(sapply(return.ind.sens.out[c(paste0(i,".",1:(j - 1)))],function(x) which(x$Outliers)))))] <- NA

                                    } else break
                                }

                                cat(paste0("[shiny, ind.sens()] INFO, sensor: ",i,", iteration: ",j,"\n"))
                                Outli <- My.rm.Outliers(ymin         = input[[paste0("Sens.Ymin",k)]],
                                                        ymax         = input[[paste0("Sens.Ymax",k)]],
                                                        ThresholdMin = input[[paste0("Sens.ThresholdMin",k)]],
                                                        date         = DF$General$date,
                                                        y            = Y,
                                                        window       = input[[paste0("Sens.window"   ,k)]],
                                                        threshold    = input[[paste0("Sens.threshold",k)]],
                                                        plotting     = FALSE
                                )
                                nameInd      <- paste0(i,".",j)
                                OutlinameInd <- paste0(i,".",j,".Outli")
                                assign(nameInd , data.frame(date = Outli$date, Outliers = apply(Outli[,c("Low_values","High_values","OutliersMin","OutliersMax")], 1, any), stringsAsFactors = FALSE))
                                #if (length(get(nameInd))==0) break # stopping if there are no outliers in the current iteration
                                if (exists("return.ind.sens.out")) return.ind.sens.out[[nameInd]] <- get(nameInd) else {
                                    return.ind.sens.out <- list(get(nameInd)); names(return.ind.sens.out) <- nameInd
                                }
                                return.ind.sens.out[[OutlinameInd]] <- Outli
                            }

                            # Discarding outliers if requested for the compound
                            progress$set(message = "[shiny, ind.sens()] INFO, Setting outlier values for sensors to NA", value = ValueRate)

                            # Discading outliers
                            if (any(names(ind.sens$out) %in% paste0(i,".",j), na.rm = TRUE)) {
                                DF$General[which(return.ind.sens.out[[paste0(i,".",j)]]$Outliers), paste0("Out."      ,i)] <- NA
                                DF$General[which(return.ind.sens.out[[paste0(i,".",j)]]$Outliers), paste0("Out.",i,".",j)] <- NA
                            }
                        }
                    }
                }
                
                if (exists("return.ind.sens.out"))  ind.sens$out <- return.ind.sens.out
                # reseting return.ind.sens.out
                if (exists("return.ind.sens.out")) rm(return.ind.sens.out)
                
                progress$set(message = "[shiny, ind.sens()] INFO, Setting index of outliers in sensor data", value = 1)
            } 
        },
        priority = 145
        )
        
        # Reactive ind.ref.out ----
        Outliers.Ref <- reactiveValues(Forced = FALSE)
        Neg <- reactiveValues(Forced = FALSE)
        observeEvent({
            unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("rm.neg", i)]]))
        },{
            # Reactive function to trigger detection of Outliers in referencer data with or without negative reference data discarded
            # Neg$Forced is TRUE if Apply$rm.neg changes 
            # depends: 
            #       Apply$rm
            
            Neg$Forced = TRUE
        },
        priority = 151)
        observeEvent({
            unlist(sapply(1:length(Config()[[2]]$gas.reference), function(i) input[[paste0("Apply.R.Out", i)]]))
            Neg$Forced
        },{
            # Reactive function to trigger detection of Outliers in Reference data
            # Outliers.Ref$Forced is TRUE if Neg$Forced is TRUE 
            #                             if Apply.R.Out is TRUE
            # depends: 
            #       list.gas.reference2use()
            #       Neg$Forced
            #       Config()[[2]]$gas.reference
            #       input$Apply.R.Out
            
            if ( Neg$Forced |
                 any(unlist(sapply(1:length(list.gas.reference2use()), function(i) input[[paste0("Apply.R.Out", i)]]))) 
            ) Outliers.Ref$Forced = TRUE else Outliers.Ref$Forced = FALSE
        },
        ignoreInit = TRUE,
        priority = 140)
        ind.ref.out.file <- file.path(DisqueFieldtestDir(), "General_data", "ind_ref_out.RDS")
        if (file.exists(ind.ref.out.file)) init.ind.ref <- list.load(ind.ref.out.file) else {init.ind.ref <- NULL; Outliers.Ref$Forced <- TRUE}
        ind.ref <- reactiveValues(out = init.ind.ref) 
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
                on.exit(progress$close())
                Tot.Iter <- sum(sapply(1:length(list.gas.reference2use()), function(x) as.numeric(input[[paste0("Ref.iterations",x)]])))
                rate     <- 1/(Tot.Iter + 2)
                ValueRate <- rate
                progress$set(message = "[shiny, ind.ref.out()] INFO, Setting index of outliers in reference data", value = ValueRate)
                
                # list of index of negative values
                ind.neg <- apply(X = DF$General[,list.gas.reference2use()], MARGIN = 2, function(x) {as.vector(which(x < 0))})
                
                for (i in list.gas.reference2use()  ) {
                    
                    # resetting to initial values
                    progress$set(message = "[shiny, ind.ref()] INFO, Initialising filtered reference data columns", value = 0.33)
                    cat("[shiny, ind.ref()] INFO, Initialising filtered reference data columns for ", i, "\n")
                    Vector.columns <- c("Out.", "Out.Neg.")
                    DF$General[, paste0(Vector.columns,i)] <- sapply(1:length(Vector.columns), function(x) return(DF$General[,i]))
                    
                    # discarding negative values if needed
                    # number index of reference pollutant in the list of references
                    k <- match(x = i, table = Config()[[2]]$gas.reference2use)
                    
                    
                    if (input[[paste0("rm.neg",k)]]) {
                        
                        if (length(ind.neg[[i]]) != 0) {
                            
                            progress$set(message = "[shiny, ind.ref()] INFO, Discarding sensor data for reference negative values", value = 0.66)
                            cat("[shiny, ind.ref()] INFO, Discarding sensor data for reference negative values\n")
                            DF$General[ind.neg[[i]], paste0(Vector.columns,i)] <- NA
                        }
                    }
                    
                    progress$set(message = "[shiny, ind.ref()] INFO, initialising outlier sensor data", value = 1.0)
                    cat("[shiny, ind.ref()] INFO, initialising outlier sensor data for ", i, "\n")
                    for (j in 1:input[[paste0("Ref.iterations", k)]]) {
                        # initialising the column of outlier iteration k 
                        DF$General[, paste0("Out.",i,".",j)] <- DF$General[,paste0("Out.Neg.",i)]
                    }
                    # deleting bigger iterations
                    j  <- input[[paste0("Ref.iterations", k)]]
                    repeat (
                        if (any(grepl(pattern = paste0("Out.",i,".", j + 1)      , x = names(DF$General)))) {
                            DF$General[,grep(pattern = paste0("Out.",i,".",j + 1), x = names(DF$General))] <- NULL
                            j <- j + 1
                        } else break # leaving the Repeat if there are no higher iterations
                    )
                    
                    # Index of outliers for reference data
                    cat("[shiny, ind.ref.out()] INFO, detecting row indexes of outliers in reference data for ", i, "\n")
                    
                    if (input[[paste0("Ref.rm.Out", k)]]) {
                        
                        for (j in 1:input[[paste0("Ref.iterations",k)]]) { # numver of iterations
                            
                            ValueRate <- ValueRate + rate
                            progress$set(message = "[shiny, ind.ref.out()] INFO, Setting index of outliers in reference data", value = ValueRate)
                            
                            if (any(list.gas.reference2use() %in% names(DF$General))) {
                                
                                if (all(is.na(DF$General[,i]))) {
                                } else {
                                    Y <- DF$General[,paste0("Out.Neg.",i)]
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
                                                            date         = DF$General$date, 
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
                                    #if (length(get(nameInd))==0) break # stopping if there are no outliers in the current iteration
                                    if (exists("return.ind.ref.out")) return.ind.ref.out[[nameInd]] <- get(nameInd) else {
                                        return.ind.ref.out <- list(get(nameInd)); names(return.ind.ref.out) <- nameInd
                                    } 
                                    return.ind.ref.out[[OutlinameInd]] <- Outli
                                }
                                
                            } else cat("[Shiny, ind.ref.out()] ERROR, Warning no reference values impossible to discard outliers\n")
                            
                            # Discarding outliers if requested for the compound
                            progress$set(message = "[shiny, ind.sens()] INFO, Setting outlier values for reference data to NA", value = ValueRate)
                            
                            # Discading outliers
                            if (any(names(return.ind.ref.out) %in% paste0(i,".",j), na.rm = TRUE)) {
                                DF$General[which(return.ind.ref.out[[paste0(i,".",j)]]$Outliers), paste0("Out."      ,i)] <- NA
                                DF$General[which(return.ind.ref.out[[paste0(i,".",j)]]$Outliers), paste0("Out.",i,".",j)] <- NA
                            }
                        }
                    }
                }
                
                if (exists("return.ind.ref.out")) ind.ref$out <- return.ind.ref.out
                # reseting return.ind.ref.out
                if (exists("return.ind.ref.out")) rm(return.ind.ref.out)
                
                progress$set(message = "[shiny, ind.ref.out()] INFO, Setting index of outliers in reference data", value = 1)
            } 
        },
        priority = 135)
        
        #nrows.DF.General <- reactive(nrow(DF$General))
        #observeEvent(nrows.DF.General, DF$General <- DF$General[!is.na(DF$General$date),], priority = 1000)
        
        # NavBar"Data Treatment", mainTabPanel "Sens.Outliers" - "PlotFiltering" ----
        output$Sens.Outliers <- renderPlot(Plot.Sens.Outliers(), width = 'auto', height = 'auto') 
        # Reactive FUN Plot.Sens.Outliers
        Plot.Sens.Outliers   <- reactive({
            # Plotting outliers for sensor data before calibration
            # depends: Config()[[2]]$gas.sensor, input$Sensors, Config()[[2]]$name.sensor, i.sensors(), DF$General, input[[paste0("Out.Sens.Date",k)]],DisqueFieldtestDir(),
            #           
            # isolate: number of iterations, "Treatments", "Filtering.Sensors"
            
            cat("-----------------------------------------------------------------------------------\n")
            cat("[Shiny]Plot.Sens.Outliers, INFO, plotting outliers of sensor data\n")
            
            # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
            k <-  match(x = input$Sensors, table = list.name.sensors())
            # Selecting species associated with sensor in rows of ASE_name.cfg 
            i <-  Config()[[2]]$gas.sensor[match(x = input$Sensors, table = Config()[[2]]$name.sensor)]
            # Executing DF$General ; created for the data series and selected for date for plotting
            General.df <- subset(DF$General, DF$General$date >= input[[paste0("Out.Sens.Date",k)]][1] & DF$General$date <= input[[paste0("Out.Sens.Date",k)]][2])
            
            WDoutput <- file.path(DisqueFieldtestDir(), "Outliers")
            op <- par(no.readonly = TRUE)
            # number of plot according to the number of iterations
            if (isolate(input[[paste0("Sens.iterations",k)]]) == 1) par(mfrow = c(1,1)) else {par(mfrow = c(ceiling(isolate(input[[paste0("Sens.iterations",k)]])/2), 2))}
            # Restoring graphical parameters on exit of function
            on.exit(par(op))
            
            if (input[[paste0("Sens.rm.Out",k)]]) {
                for (j in 1:isolate(input[[paste0("Sens.iterations",k)]])) { # number of iterations
                    if (all(is.na(General.df[,i]))) {
                        cat(paste0("[Shiny]Plot.Sens.Outliers, ERROR, All data sensor for ", input$Sensors, " are NAs, cannot filter outliers"), sep = "\n")
                        plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                        text(1,1,paste0("[Shiny]Plot.Sens.Outliers, ERROR, All outlier data sensor for ", input$Sensors, " are NAs, cannot filter outliers"))
                    } else {
                        # Checking if we have the data frame of outliers for in General.df, maybe there is no because there was no outliers. In this case next j.
                        if (!any(grepl(pattern = paste0("Out.",i,".",j), x = objects(General.df)))) {
                            cat(paste0("[Shiny]Plot.Sens.Outliers, INFO, There is no outliers to detect for ", input$Sensors, " iteration ", j, "\n"))
                            plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                            text(1,1,paste0("[Shiny]Plot.Sens.Outliers, INFO, There is no outliers to detect for ", input$Sensors, " iteration ", j, "\n"))
                            next
                        } else {
                            cat(paste0("[Shiny]Plot.Sens.Outliers, Plotting the outliers for ",input$Sensors, " iteration ", j, "\n"))
                            if (j == 1) { 
                                Y = General.df[,paste0("Out.Warm.TRh.Inv.",i)]
                            } else{
                                Y = General.df[,paste0("Out.",i,".",j - 1)] # we need to plot the data of j-1 iterations and add the points of outliers  
                            }
                            # Date
                            Date <- General.df[,"date"]
                            # Selecting ind.sens$out for sensor input$Sensors; created for the selected dates for plotting
                            ind.sens.out   <- selectByDate(ind.sens$out[[paste0(i,".",j,".Outli")]], start = input[[paste0("Out.Sens.Date",k)]][1], end = input[[paste0("Out.Sens.Date",k)]][2])
                            ind.sens.out.n <- selectByDate(ind.sens$out[[paste0(i,".",j)]]         , start = input[[paste0("Out.Sens.Date",k)]][1], end = input[[paste0("Out.Sens.Date",k)]][2])
                            
                            Outli <- My.rm.Outliers(ymin         = input[[paste0("Sens.Ymin",k)]],
                                                    ymax         = input[[paste0("Sens.Ymax",k)]],
                                                    ThresholdMin = input[[paste0("Sens.ThresholdMin",k)]],
                                                    date         = Date, 
                                                    y            = Y, 
                                                    window       = input[[paste0("Sens.window",k)]],
                                                    threshold    = input[[paste0("Sens.threshold",k)]],
                                                    ind          = ind.sens.out,
                                                    plotting     = TRUE, 
                                                    set.Outliers = FALSE,
                                                    Title        = paste0("Outliers for ", i, " , iteration ",j)
                            )
                            remove(Outli)
                            
                            cat(paste0("[Shiny]Plot.Sens.Outliers, INFO, sensor for ", input$Sensors, 
                                       ", number of valid measurements after removing outliers ",
                                       length(which(!is.na(General.df[,paste0("Out.",i)]))),". Number of outliers: ", 
                                       length(which(ind.sens.out.n$Outliers)) , "\n")) ################################################ Add plotting dates   ###################################################C
                            
                            # Opening the Filtering TabSet for GUI consistency
                            isolate({
                                #updateTabsetPanel(session, inputId = "Calib_data" , selected = "Filtering")
                                updateTabsetPanel(session, inputId = "Treatments" , selected = "Sensors")
                                updateTabsetPanel(session, inputId = "Filtering.Sensors", selected = input$Sensors)
                            })
                        }
                    }
                }
                # Saving plot if requested
                if (input$SavePlot) {
                    dev.copy(png,
                             filename = file.path(WDoutput, paste0(AirsensEur.name(),"_Outliers_",input$Sensors,"_",j,".png")), 
                             #          units = "cm", 
                             #          width = 35.55, 
                             #          height = 20
                             res = 300 
                    )
                    dev.off()
                    cat(paste0("[shiny] INFO, ", AirsensEur.name(), "_Outliers_", input$Sensors, "_", j, ".png saved in ", WDoutput, "\n" ))
                    updateCheckboxInput(session, 
                                        inputId = "SavePlot", 
                                        label = NULL, 
                                        value = FALSE)
                    # dev <- dev.prev() # https://stackoverflow.com/questions/21334573/copying-plot-with-type-n-in-r-to-pdf-doesnt-copy-plot-points-only-its-lines-a
                    # dev.copy2pdf(file = file.path(WDoutput, paste0(AirsensEur.name(),"_Outliers_",input$Sensors,"_",j,".pdf")), 
                    #          #width = 14,   # in inches
                    #          #height = 7.9, # in inches
                    #          out.type = "pdf")
                    # dev.off()
                }
            } else {
                cat(paste0("[Shiny]Plot.Sens.Outliers, INFO, discarding of outliers not requested for ", Config()[[2]]$name.sensor[k], " in the Graphical User Interface "), sep = "\n")
                plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                text(1,1,paste0("[Shiny]Plot.Sens.Outliers, INFO, discarding of outliers not requested for ", Config()[[2]]$name.sensor[k], " in the Graphical User Interface "))
            }
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            # Cleaning memory space
            remove(WDoutput)  
        })
        # NavBar"Data Treatment", mainTabPanel "Ref.Outliers" - "PlotFiltering", ----
        output$Ref.Outliers  <- renderPlot(Plot.Ref.Outliers() , width = 'auto', height = 'auto')
        # Reactive FUN Plot.Ref.Outliers
        Plot.Ref.Outliers    <- reactive({
            # depends: Config(), input$Filtering.References,input[[paste0("Ref.iterations",k)]], input[[paste0("Ref.rm.Out",k)]], input[[paste0("Out.Ref.Date",k)]]
            # isolate: number of iterations
            
            #browser()
            # index 1,2,3,4, 5, 6 of the reference gas selected in uiFiltering corresponding of rows of ASE_name.cfg
            k <-  match(x = input$Filtering.References, table = Config()[[2]]$gas.reference)
            # Selecting gas.reference2use corresponding to input$Filtering.References selected
            i <-  Config()[[2]]$gas.reference2use[k]
            
            # checking that the data of Reference pollutant exists
            if (i %in% list.gas.reference2use()) {
                
                # Executing DF$General and ind.ref$out; created for the data series and selected for date for plotting
                General.df <- subset(x = DF$General, 
                                     date >= input[[paste0("Out.Ref.Date",k)]][1] & date <= input[[paste0("Out.Ref.Date",k)]][2]
                )
                
                # Flagging outliers for reference data and all sensors 
                WDoutput <- file.path(DisqueFieldtestDir(), "Outliers")
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
                        
                        
                        if (all(is.na( General.df[,i]))) {
                            cat(paste0("[Shiny, Plot.Ref.Outliers()] ERROR All new reference data for ", input$Filtering.References, " are empty; cannot filter outliers"), sep = "\n")
                            plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                            text(1,1,paste0("[Shiny, Plot.Ref.Outliers()] ERROR All new reference data for ", input$Filtering.References, " are empty; cannot filter outliers"))
                        } else {
                            
                            # Checking if we have the data frame of outliers for in General.df, maybe there is no because there was no outliers. In this case next j.
                            if (!any(grepl(pattern = paste0("Out.",i,".",j), x = objects(General.df)))) {
                                
                                cat(paste0("[Shiny]Plot.Sens.Outliers, INFO, There is no outliers to detect for ", input$Filtering.References, " iteration ", j, "\n"))
                                plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                                text(1,1,paste0("[Shiny, Plot.Ref.Outliers()] INFO, There is no outliers to detect for ", input$Filtering.References, " iteration ", j, "\n"))
                                next
                            } else {
                                
                                cat(paste0("[Shiny, Plot.Ref.Outliers()] Plotting the outliers for ", input$Filtering.References, " iteration ", j), sep = "\n")
                                if (j == 1) {
                                    
                                    Y = General.df[,c(paste0("Out.Neg.",i))]  
                                } else{
                                    
                                    Y = General.df[,c(paste0("Out.",i,".",j - 1))] # we need to plot the data of j-1 iterations and add the points of outliers  
                                }
                                Date <- General.df[,"date"]
                                # Selecting ind.ref$out; created for the data series and selected for date for plotting
                                #ind.ref.out   <- selectByDate(ind.ref$out[[paste0(i,".",j,".Outli")]], start = input[[paste0("Out.Ref.Date",k)]][1], end = input[[paste0("Out.Ref.Date",k)]][2])
                                ind.ref.out   <- subset(ind.ref$out[[paste0(i,".",j,".Outli")]], 
                                                        ind.ref$out[[paste0(i,".",j,".Outli")]]$date >= input[[paste0("Out.Ref.Date",k)]][1] & 
                                                            ind.ref$out[[paste0(i,".",j,".Outli")]]$date <= input[[paste0("Out.Ref.Date",k)]][2]
                                )
                                #ind.ref.out.n <- selectByDate(ind.ref$out[[paste0(i,".",j         )]], start = input[[paste0("Out.Ref.Date",k)]][1], end = input[[paste0("Out.Ref.Date",k)]][2])
                                ind.ref.out.n <- subset(ind.ref$out[[paste0(i,".",j)]], 
                                                        ind.ref$out[[paste0(i,".",j)]]$date >= input[[paste0("Out.Ref.Date",k)]][1] &
                                                            ind.ref$out[[paste0(i,".",j)]]$date <= input[[paste0("Out.Ref.Date",k)]][2]
                                )
                                
                                Outli <- My.rm.Outliers(ymin         = input[[paste0("Ref.Ymin",k)]] ,
                                                        ymax         = input[[paste0("Ref.Ymax",k)]] ,
                                                        ThresholdMin = input[[paste0("Ref.ThresholdMin",k)]] ,
                                                        date         = Date,
                                                        y            = Y, 
                                                        window       = input[[paste0("Ref.window",k)]] ,
                                                        threshold    = input[[paste0("Ref.threshold",k)]],
                                                        plotting     = TRUE, 
                                                        set.Outliers = FALSE, 
                                                        ind          = ind.ref.out,    
                                                        Title        = paste0("Outliers of ",i, " iteration ", j)
                                )
                                # Saving plot if requested
                                if (input$SavePlot) {
                                    
                                    dev.copy(png,filename = file.path(WDoutput, paste0(AirsensEur.name(),"_Outliers_",i,".png")), 
                                             #units = "cm", 
                                             #width = 35.55, 
                                             #height = 20,
                                             res = 300 
                                    )
                                    dev.off()
                                    cat(paste0("[shiny] INFO, ",AirsensEur.name(),"_Outliers_",i,".png saved in ", WDoutput, "\n" ))
                                    updateCheckboxInput(session, 
                                                        inputId = "SavePlot", 
                                                        label = NULL, 
                                                        value = FALSE)
                                }
                                remove(Outli)
                                
                                cat(paste0("[Shiny, Plot.Ref.Outliers()] INFO, reference values for ", input$Filtering.References, " number of valid measurements after removing outliers ",
                                           length(which(!is.na(General.df[,paste0("Out.",i)]))),". Number of outliers: ", length(which(ind.ref.out.n$Outliers)), "\n"))
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
                    updateTabsetPanel(session, inputId = "Calib_data", selected = "Filtering")
                    updateTabsetPanel(session, inputId = "Treatments", selected = "Reference")
                })
            } else {
                
                cat(paste0("[Shiny]Plot.Ref.Outliers WARNING, No data existing for Reference pollutant ", i, " . Cannot discard outliers.\n"))
                plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                text(1,1,paste0("[Shiny]Plot.Ref.Outliers WARNING, No data existing for Reference pollutant ", i, " . Cannot discard outliers.\n"))
            }
            
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
            ind.neg <- apply(X = DF$General[,list.gas.reference2use()], MARGIN = 2, function(x) {which(x < 0)})
            
            StatFiltered   <- data.frame(
                #name.gas           = list.gas.sensors(), 
                name.sensor        = list.name.sensors(), 
                Initial.Total       = sapply(1:length(list.gas.sensors())      , function(i) length(na.omit(DF$General[,list.gas.sensors()[i]])) ),
                Warming             = sapply(1:length(list.gas.sensors())      , function(i) length(ind.warm$out[[i]]) ),
                T.min               = sapply(1:length(list.gas.sensors())      , function(i) length(unlist(ind.TRh$out$T.min[paste0(list.name.sensors()[i], "__Temp. < ", input[[paste0("Temperature",i)]][1])])) ),
                T.max               = sapply(1:length(list.gas.sensors())      , function(i) length(unlist(ind.TRh$out$T.max[paste0(list.name.sensors()[i], "__Temp. > ", input[[paste0("Temperature",i)]][2])])) ),
                RH.min              = sapply(1:length(list.gas.sensors())      , function(i) length(unlist(ind.TRh$out$Rh.min[paste0(list.name.sensors()[i], "__RH < ", input[[paste0("Humidity",i)]][1])])) ),
                RH.max              = sapply(1:length(list.gas.sensors())      , function(i) length(unlist(ind.TRh$out$Rh.max[paste0(list.name.sensors()[i], "__RH < ", input[[paste0("Humidity",i)]][2])])) ),
                Invalid             = sapply(1:length(list.gas.sensors())      , function(i) if (Outliers_Sensor()$Sens.Inval.Out[i.sensors()[i]]) length(unlist(lapply(ind.Invalid$out[[2]][[list.name.sensors()[i]]], is.numeric))) else NA),
                Sens.Outlier_Max    = sapply(1:length(list.gas.sensors())      , function(i) if (Outliers_Sensor()$Sens.rm.Out[i.sensors()[i]])    length(unlist(which(ind.sens$out[paste0(list.gas.sensors()[i],".1.Outli")][[1]][,"OutliersMax"]))) else NA),
                Sens.Outlier_Min    = sapply(1:length(list.gas.sensors())      , function(i) if (Outliers_Sensor()$Sens.rm.Out[i.sensors()[i]])    length(unlist(which(ind.sens$out[paste0(list.gas.sensors()[i],".1.Outli")][[1]][,"OutliersMin"]))) else NA),
                Sens.High_values    = sapply(1:length(list.gas.sensors())      , function(i) if (Outliers_Sensor()$Sens.rm.Out[i.sensors()[i]])    length(unlist(which(ind.sens$out[paste0(list.gas.sensors()[i],".1.Outli")][[1]][,"High_values"]))) else NA),
                Sens.Low_values     = sapply(1:length(list.gas.sensors())      , function(i) if (Outliers_Sensor()$Sens.rm.Out[i.sensors()[i]])    length(unlist(which(ind.sens$out[paste0(list.gas.sensors()[i],".1.Outli")][[1]][,"Low_values"]) )) else NA),
                Sens.Filtered.Total = sapply(1:length(list.gas.sensors())      , function(i) length(na.omit(DF$General[,paste0("Out.",list.gas.sensors()[i])]))),
                gas.reference2use   = list.gas.reference2use(),
                Negative.Reference  = sapply(1:length(list.gas.reference2use()), function(i) if (Outliers_Ref()$remove.neg[i.sensors()[i]]) length(ind.neg[[i]]) else NA),
                Refe.Outlier_Max    = sapply(1:length(list.gas.reference2use()), function(i) if (Outliers_Ref()$Ref.rm.Out[i.sensors()[i]]) length(unlist(which(ind.ref$out[paste0(list.gas.reference2use()[i],".1.Outli")][[1]][,"OutliersMax"]))) else NA),
                Refe.Outlier_Min    = sapply(1:length(list.gas.reference2use()), function(i) if (Outliers_Ref()$Ref.rm.Out[i.sensors()[i]]) length(unlist(which(ind.ref$out[paste0(list.gas.reference2use()[i],".1.Outli")][[1]][,"OutliersMin"]))) else NA),
                Refe.High_values    = sapply(1:length(list.gas.reference2use()), function(i) if (Outliers_Ref()$Ref.rm.Out[i.sensors()[i]]) length(unlist(which(ind.ref$out[paste0(list.gas.reference2use()[i],".1.Outli")][[1]][,"High_values"]))) else NA),
                Refe.Low_values     = sapply(1:length(list.gas.reference2use()), function(i) if (Outliers_Ref()$Ref.rm.Out[i.sensors()[i]]) length(unlist(which(ind.ref$out[paste0(list.gas.reference2use()[i],".1.Outli")][[1]][,"Low_values"]) )) else NA)
            )

            return(StatFiltered)
        })
        
        # NavBar"Data Treatment", mainTabPanel "Covariates" -"Plots", ----
        # Observer raw.unit ----
        observeEvent({
            # Making reactive changes in the conversion function according to the sensor raw units, 
            unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("Sens.raw.unit", i)]]))
        },{
            k <- match(x = input$Calib.Sensors, table = list.name.sensors())
            updateCheckboxInput(session, inputId = paste0("Apply.conv", k), label = NULL , value = TRUE)
        },
        ignoreInit = TRUE
        ) 
        # Reactive Force.Conv ----
        Conv <- reactiveValues(Forced = FALSE)
        observeEvent({
            unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("Apply.conv", i)]]))
            Outliers.Sens$Forced
            
        }, 
        {
            # Reactive function to trigger General.conv()
            # Force.Conv is TRUE if there is no _volt in General.conv() or 
            #                    if any Button Force.Conv is checked or if any CheckBoxes Neg.mod1 is checked or 
            #                    if Warm.TRh.Neg.Inv$Forced | Outliers.Ref$Forced are TRUE
            #                     
            # depends: 
            #       list.gas.sensors()
            #       DF$General
            #       input$Force.Conv
            
            if (Outliers.Sens$Forced |
                !any(grepl(pattern = paste0(list.name.sensors()[1],"_volt"), x = colnames(DF$General)))          |
                any(unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("Apply.conv", i)]]))) 
            ) Conv$Forced = TRUE else Conv$Forced = FALSE
        },
        priority = 140
        )
        
        # NavBar"Data Treatment", mainTabPanel "Time series" - "Covariates", ----
        output$ValidCovarTS     <- renderPlot(Plot.ValidCovarTS() , width = 'auto', height = 'auto')
        # Reactive FUN Plot.ValidCovarTS
        Plot.ValidCovarTS       <- reactive({
            #----------------------------------------------------------CR
            # plotting correlation in time series of validated data with covariates
            #----------------------------------------------------------CR
            # depends:
            #   input$Sensors,
            #   Config()[[2]]$name.sensor
            #   i.sensors()
            #   DF$General
            #   INFLUX()[[4]]
            #   input[[paste0("Sens",k)]]
            #   input[[paste0("Date",k)]]
            # isolates:
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "Plotting times series of covariates", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
            WDoutput <- file.path(DisqueFieldtestDir(), "Verification_plots")
            op <- par(no.readonly = TRUE)
            par(mfrow = c(1,1))
            # Restoring graphical parameters on exit of function
            on.exit(par(op))
            
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat("[shiny, Plot.RawData()] INFO,plotting time series of validated data with covariates\n")
            #a
            if (all(is.na(DF$General[,INFLUX()[[4]]]))) {
                cat("[Shiny]Plot.ValidCovar, ERROR, All sensor time series are empty, not plotting any times series\n")
            } else {
                # Sensor relationships with other variables
                cat(paste0("[shiny, Plot.RawData()] INFO,Plot sensor data in volt with covariates for Sensor ", 
                           input$Sensors, " in order to check relationships with other variables\n"))
                Relationships         <- na.omit(colnames(DF$General[which(colnames(DF$General) %in% input[[paste0("Sens",CalSet()$k)]]) ]))
                # removing variable date for time seris plotting
                if(any(grepl(pattern = "date", x = Relationships))) Relationships <- Relationships[-which(Relationships == "date")]
                # AddOut                <- which(Relationships %in% c(list.gas.sensors()))
                # Relationships[AddOut] <- paste0(Relationships[AddOut])
                
                # Plotting timeseries, changin names of variables
                Name.pol = gsub(pattern = "Out." , replacement = "", x = Relationships)
                Name.pol = gsub(pattern = "_volt", replacement = paste0(".Sensor.",CalSet()$Sens.raw.unit), x = Name.pol)
                # Using SelectByDate gives a mistake for the very small current 10-8, using subset instead
                timePlot(mydata = subset(DF$General[,c("date",Relationships)], date >= input[[paste0("Date",CalSet()$k)]][1] & date <= input[[paste0("Date",CalSet()$k)]][2]), 
                         pollutant  = Relationships, 
                         name.pol   = Name.pol,
                         date.pad   = TRUE, 
                         auto.text  = FALSE, 
                         y.relation = "free", 
                         strip      = TRUE, 
                         ylab = "", 
                         main = paste0("Covariates for ", list.name.sensors()[CalSet()$k], 
                                       " between ", input[[paste0("Date",CalSet()$k)]][1], 
                                       " and "    , input[[paste0("Date",CalSet()$k)]][2])
                )
                
                # save plots in files
                # Saving plot if requested
                if (input$SavePlot) {
                    dev.copy(png,
                             filename = file.path(WDoutput, 
                                                  paste0(list.name.sensors()[CalSet()$k],"_ts_",
                                                         format(input[[paste0("Date",CalSet()$k)]][1],"%Y%m%d"),"_",
                                                         format(input[[paste0("Date",CalSet()$k)]][2],"%Y%m%d"),".png")), 
                             #units = "cm", 
                             #width = 35.55, 
                             #height = 20,
                             res = 300 
                    )
                    dev.off()
                    cat(paste0("[shiny] INFO, ", list.name.sensors()[CalSet()$k],"_ts_",
                               format(input[[paste0("Date",CalSet()$k)]][1],"%Y%m%d"),"_",
                               format(input[[paste0("Date",CalSet()$k)]][2],"%Y%m%d"),".png saved in ", WDoutput, "\n" ))
                    updateCheckboxInput(session, 
                                        inputId = "SavePlot", 
                                        label = NULL, 
                                        value = FALSE)
                }
            }
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            
            # Opening the Calib TabSet for GUI consistency
            isolate({
                #if (isolate(input$Calib_data) != "Calib" & isolate(input$Calib_data) != "SetTime") updateTabsetPanel(session, inputId = "Calib_data", selected = "Calib")
                updateTabsetPanel(session, inputId = "Calib.Sensors", selected = input$Sensors)
                updateTabsetPanel(session, inputId = "SetTime.Sensors", selected = input$Sensors)
            })
            progress$set(message = "Plotting times series of covariates", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
        })
        # NavBar "Data Treatment", mainTabPanel "Matrix" - Covariates",  ----
        output$ValidCovarMatrix <- renderPlot(Plot.ValidCovarMatrix()   , width = 'auto', height = 'auto')
        # Reactive FUN Plot.ValidCovarMatrix
        Plot.ValidCovarMatrix   <- reactive({
            # Plotting correalation matrix using pairs()
            # depends:
            #   input$Sensors,
            #   Config()[[2]]$name.sensor
            #   i.sensors()
            #   DF$General
            #   INFLUX()[[4]]
            #   input[[paste0("Sens",k)]]
            #   input[[paste0("Date",k)]]
            # isolates:
            
            # Opening the Calib TabSet for GUI consistency
            isolate({
                #if (isolate(input$Calib_data) != "Calib" & isolate(input$Calib_data) != "SetTime") updateTabsetPanel(session, inputId = "Calib_data", selected = "Calib")
                updateTabsetPanel(session, inputId = "Calib.Sensors", selected = input$Sensors)
                updateTabsetPanel(session, inputId = "SetTime.Sensors", selected = input$Sensors)
            })
            WDoutput <- file.path(DisqueFieldtestDir(), "Verification_plots")
            op <- par(no.readonly = TRUE)
            # Restoring graphical parameters on exit of function
            on.exit(par(op))
            par(mfrow = c(1,1))
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "Plotting matrix of covariates", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat("[Shiny]Plot.ValidPairs, INFO, plotting correlation matrix of validated data with covariates\n")
            #
            if (all(is.na(DF$General[,INFLUX()[[4]]]))) {
                cat("[Shiny]Plot.ValidPairs, ERROR, All sensor time series are empty, not plotting any times series\n")
            } else {
                # Sensor relationships with other variables
                cat(paste0("[shiny, Plot.RawData()] INFO,Plot sensor data in volt with covariates for Sensor ", input$Sensors, " in order to check relationships with other variables\n"))
                # unique to avoid repeating sensors_modelled
                Relationships         <- unique(na.omit(colnames(DF$General)[colnames(DF$General) %in% input[[paste0("Sens",CalSet()$k)]] ]) )
                AddOut                <- which(Relationships %in% list.gas.sensors())
                Relationships[AddOut] <- paste0(Relationships[AddOut])
                
                # changing names of variables
                Pattern  <- rbind(c("Out.", ""),c("Ref.", paste0("Reference in ",CalSet()$unit.ref,", ")),c("_volt", paste0(" Sensor in ",CalSet()$Sens.raw.unit)))
                if (nrow(Pattern)>0) Labels <- gsub(pattern = Pattern[1,1], replacement = Pattern[1,2], x = Relationships) 
                if (nrow(Pattern)>1) for (i in 2:nrow(Pattern)) Labels <- gsub(pattern = Pattern[i,1], replacement = Pattern[i,2], x = Labels)  
                # Using SelectByDate gives a mistake for the very small current 1e-8 --> using subset
                pairs(x =subset(DF$General[,c("date",Relationships)], 
                                date >= input[[paste0("Date",CalSet()$k)]][1] & date <= input[[paste0("Date",CalSet()$k)]][2])[,Relationships], 
                      lower.panel = panel.smooth, 
                      upper.panel = panel.cor,
                      diag.panel  = panel.hist, 
                      labels = Labels, 
                      main = paste0("Correlation matrix of sensor data (R2 in bold) versus covariates for sensor ", input$Sensors,
                                    " between ", input[[paste0("Date",CalSet()$k)]][1], 
                                    " and "    , input[[paste0("Date",CalSet()$k)]][2]),
                      cex.labels = 1.8
                ) # cex.cor = 1.3
                
                # save plots in files
                # Saving plot if requested
                #isolate({
                if (input$SavePlot) {
                    dev.copy(png,
                             filename = file.path(WDoutput, 
                                                  paste0(list.name.sensors()[CalSet()$k],"_pairs_",
                                                         format(min(DF$General$date, na.rm = TRUE),"%Y%m%d"),"_",
                                                         format(max(DF$General$date, na.rm = TRUE),"%Y%m%d"),".png")), 
                             #units = "cm", 
                             #width = 35.55, 
                             #height = 20,
                             res = 300 
                    )
                    dev.off()
                    cat(paste0("[shiny] INFO, ", list.name.sensors()[CalSet()$k],"_pairs_",
                               format(min(DF$General$date, na.rm = TRUE),"%Y%m%d"),"_",
                               format(max(DF$General$date, na.rm = TRUE),"%Y%m%d"),".png saved in ", WDoutput, "\n" ))
                    
                    updateCheckboxInput(session, 
                                        inputId = "SavePlot", 
                                        label = NULL, 
                                        value = FALSE)
                }
                #})
                
                #} 
            }
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            
            progress$set(message = "Plotting matrix of covariates", value = 1)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
        })
        
        # Observer General.conv(), input$Cal and input$Neg.mod ----
        observeEvent({
            # Making reactive changes in the calibration if Negative shall be discarded 
            unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("Neg.mod",i)]]))
        },{
            k <- match(x = input$Calib.Sensors, table = list.name.sensors())
            updateCheckboxInput(session, inputId = paste0("Apply.cal", k), label = NULL , value = TRUE)
        },
        ignoreInit = TRUE
        )
        
        # update Raw unit, model for calibration and range for calibration when selecting a Calibration model
        # if not commented TimesSeries and Residual Matrix do not update when changing model
        observeEvent({
            unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("Cal", i)]]))
        },{
            
            # Detecting selected sensor in TabSet Calib
            k <- match(x = input$Calib.Sensors, table = list.name.sensors())
            
            # checking that input$Calx is not NULL
            if (!is.null(input[[paste0("Cal",k)]])) {
                
                # checking that input$Calx is not empty
                if (input[[paste0("Cal",k)]] != "") {
                    
                    Splitted.Cal <- unlist(strsplit(x = CalSet()$Cal, split = "__"))
                    Unit  <- Splitted.Cal[3]
                    Model <- Splitted.Cal[4]
                    Start <- strptime(Splitted.Cal[5], format = "%Y%m%d", tz = input$ref.tzone)
                    End   <- strptime(Splitted.Cal[6], format = "%Y%m%d", tz = input$ref.tzone)
                    
                    # Changing unit
                    if (Unit != input[[paste0("Sens.raw.unit",k)]]) {
                        
                        updateSelectInput(session = session,
                                          inputId  = paste0("Sens.raw.unit",k), 
                                          label    = NULL, 
                                          choices  = NULL, 
                                          selected = Unit)
                    } 
                    
                    # determinig model type
                    if (Model != input[[paste0("Calibration",k)]]) {
                        
                        updateSelectInput(session = session,
                                          inputId  = paste0("Calibration",k), 
                                          label    = NULL                     , 
                                          choices  = NULL, 
                                          selected = Model
                        )
                    } 
                    
                    # determining date interval
                    # Updating DateCal
                    if (Start != input[[paste0("DateCal",k)]][1] | End != input[[paste0("DateCal",k)]][2]) {
                        
                        updateDateRangeInput(session,
                                             inputId = paste0("DateCal",k),
                                             label   = NULL, 
                                             start   = Start,
                                             end     = End,
                                             min     = NULL,
                                             max     = NULL)  
                    } 
                    if (Start != input[[paste0("DatePlotCal",k)]][1] | End != input[[paste0("DatePlotCal",k)]][2]) {
                        
                        updateDateRangeInput(session,
                                             inputId = paste0("DatePlotCal",k),
                                             label   = NULL, 
                                             start   = Start,
                                             end     = End,
                                             min     = NULL,
                                             max     = NULL)  
                    } 
                    
                    # determining Covariates
                    if (Model == "MultiLinear") {
                        
                        # Co-Variates selected in UI
                        Covariates.CovMod    <- str_replace(Splitted.Cal[7], pattern = ".rds", replacement = "")
                        Covariates.CovMod    <- unlist(strsplit(x = Covariates.CovMod , split = "&"))
                        if (grepl(pattern = "-", x = Covariates.CovMod[1])) {
                            
                            Covariates.CovMod <- unlist(strsplit(x = Covariates.CovMod , split = "-"))
                            Covariates.CovMod <- Covariates.CovMod[ seq(from = 1, to = length(Covariates.CovMod), by = 2) ]
                        } 
                        
                        if (!all(Covariates.CovMod %in% input[[paste0("CovMod",k)]]) | !all(input[[paste0("CovMod",k)]] %in%  Covariates.CovMod )) {
                            
                            updateSelectInput(session = session,
                                              inputId  = paste0("CovMod",k), 
                                              label    = NULL                     , 
                                              choices  = NULL, 
                                              selected = Covariates.CovMod
                            )
                        }
                    }
                    
                    # Triggering a calibration
                    if (!input[[paste0("Apply.cal", k)]]) updateCheckboxInput(session, inputId = paste0("Apply.cal", k), label = "Apply Calibration" , value = TRUE)
                    
                }
            }
        }, ignoreInit = TRUE
        )
        
        # Reactive General.cal ----
        Cal <- reactiveValues(Forced = FALSE)
        observeEvent({
            unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("Apply.cal", i)]]))
            Outliers.Sens$Forced 
            Outliers.Ref$Forced
            Conv$Forced 
        },{
            # Reactive function to trigger DF$General
            # Cal$Forced is TRUE if any Button Apply.cal is checked or 
            #                    if there is no gas_modelled in DF$General or
            #                    if any Outliers.Sens$Forced | Outliers.Ref$Forced | Conv$Forced are TRUE
            
            # depends: 
            #       list.gas.sensors()
            #       DF$General
            #       input$Force.ConvN
            #       input$Apply.calN
            
            if (Outliers.Sens$Forced | Outliers.Ref$Forced | Conv$Forced |
                !any(grepl(pattern = paste0(list.gas.sensors(),"_modelled",collapse = "|"), x = colnames(DF$General)))    | 
                any(unlist(sapply(1:length(list.name.sensors()), function(i) input[[paste0("Apply.cal", i)]])))  
            ) Cal$Forced <- TRUE else Cal$Forced <- FALSE
        },
        priority = 130)
        #General.cal <- reactiveValues(General = DF$General)
        observeEvent({
            Outliers.Ref$Forced
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
            #    DF$General slos includes converted and calibrated sensor data (extrapolation by application of calibration models)
            
            # depends: 
            #       Warm.TRh.Neg.Inv
            #       Outliers.Ref$Forced
            #       Conv$Forced
            #       Cal$Forced
            
            # initial values Loading General.Rdata or General
            cat(paste0("[shiny, General.cal()] INFO, Outliers.Sens$Forced is : ", Outliers.Ref$Forced,"\n"))
            cat(paste0("[shiny, General.cal()] INFO, Outliers.Ref$Forced is : " , Outliers.Ref$Forced,"\n"))
            cat(paste0("[shiny, General.cal()] INFO, Conv$Forced is : "         , Conv$Forced,"\n"))
            cat(paste0("[shiny, General.cal()] INFO, Cal$Forced is : "          , Cal$Forced,"\n"))
            
            if (Outliers.Ref$Forced | Conv$Forced | Cal$Forced) {
                
                
                # input: 
                #   DF$General    :     dataframe created after discarding invalid data, 
                #   list.gas.sensors()        :     charater vector with the names of compounds
                
                # Output:
                #     Change DF$General starting from DF$General. Provided that outlier discarding is enabled, for each sensor compound name i in list.gas.sensors() 
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
                    Sensors_Cal <- merge(x = Calib_data()[c("name.sensor","Sens.raw.unit")][i.sensors(),], # if we use CaliB_data the file is saved every time we update Calid_data
                                         y = Shield()[,c("name.sensor","gas.sensor","RefAD","Ref","board.zero.set","GAIN","Rload")], 
                                         by = "name.sensor", 
                                         all = TRUE
                    )
                    # order Sensors_Cal as Calib_data()
                    Sensors_Cal <- Sensors_Cal[na.omit(match(list.gas.sensors(),Sensors_Cal$gas.sensor)),]
                    
                    # Values converted in volt or nA
                    DF$General[,paste0(list.name.sensors(),"_volt")] <- 
                        ASEDigi2Volt(Sensors_Cal = Sensors_Cal, Digital = DF$General[,paste0("Out.",list.gas.sensors())])
                    
                    # Values converted in volt or nA - Board zero in Volt? change to V or nA
                    DF$General[,paste0(list.name.sensors(),"_DV")] <- DF$General[,paste0(list.name.sensors(),"_volt")] - 
                        t(matrix(data = rep(x     = (Config()[[2]]$Ref - Config()[[2]]$RefAD), 
                                            times = nrow(DF$General)), 
                                 ncol = nrow(DF$General) ))[,i.sensors()]
                    
                    progress$set(message = "[shiny, General.conv()] INFO, Converting sensor digital data to V or nA", value = 1)
                    progress$close()
                }
                
                # Starting calibration
                if (Cal$Forced) { 
                    
                    # Create a Progress object
                    progress <- shiny::Progress$new()
                    # Make sure it closes when we exit this reactive, even if there's an error
                    on.exit(progress$close())
                    Tot.Iter <- length(list.gas.sensors())
                    rate <- 1 / (Tot.Iter + 2)
                    ValueRate <- rate
                    progress$set(message = "[shiny, General.cal()] INFO, Calibrating raw sensor values to ppb,ug.m-3 or ppm,mg.m-3", value = ValueRate)
                    
                    
                    # Application of Calibration function to Complete data set
                    if (!is.null(DF$General)) {
                        
                        # initial Calibration with values in input[[paste0("Cal",j)]])) provided that "Method of extrapolation" is "Previous calibration"
                        for (k in 1:length(list.name.sensors())) {
                            
                            # calibrating only one sensor at a time
                            #k <- CalSet()$k
                            
                            ValueRate <- ValueRate + rate
                            progress$set(message = "[shiny, General.cal()] INFO, Calibrating raw sensor values to ppb,ug.m-3 or ppm,mg.m-3", value = ValueRate)
                            
                            if (input[[paste0("Cal.Line",k)]] == "Previous calibration") {
                                
                                if (nchar(input[[paste0("Cal",k)]]) != 0) {
                                    
                                    # reading file
                                    if (file.exists(file.path(CalSet()$WDoutputMod, 
                                                              paste0(Config()[[1]]$AirsensEur.name,"__",
                                                                     list.name.sensors()[k],"__",
                                                                     input[[paste0("Cal",k)]])
                                    )
                                    ) # & input[[paste0("Cal",k)]] != ""
                                    ) {
                                        
                                        cat(paste0("[shiny, General.cal()] INFO, Calibrating raw values of sensor ", list.name.sensors()[k], " to ppb,ug.m-3 or ppm,mg.m-3\n"))
                                        Model.i <- readRDS(file = file.path(CalSet()$WDoutputMod, 
                                                                            paste0(Config()[[1]]$AirsensEur.name,"__",
                                                                                   list.name.sensors()[k],"__",
                                                                                   input[[paste0("Cal",k)]]
                                                                            )
                                        )
                                        )
                                        
                                        nameGasVolt        = paste0(list.name.sensors()[k],"_volt")         # sensor gas in volt
                                        nameGasMod         = paste0(list.gas.sensors()[k] ,"_modelled")     # modelled sensor gas 
                                        
                                        # Detecting the model type of the selected calibration model
                                        for (j in Models) {
                                            
                                            if (grepl(pattern = paste0("_",j,"_"), x = input[[paste0("Cal",k)]])) {
                                                
                                                Mod_type <- j
                                                break
                                            }
                                        } 
                                        # Preparing the matrix of covaraiates
                                        # Removing na for nameGasMod for nameGasVolt missing
                                        is.not.NA.y <- which(!is.na(DF$General[, nameGasVolt]))
                                        is.NA.y     <- which(is.na(DF$General[, nameGasVolt]))
                                        if (Mod_type == "MultiLinear") {
                                            CovMod  <- unlist(strsplit(unlist(strsplit(sub(pattern = ".rds", 
                                                                                           replacement = "", 
                                                                                           x = paste0(Config()[[1]]$AirsensEur.name,"__", 
                                                                                                      list.name.sensors()[k],"__",
                                                                                                      input[[paste0("Cal",k)]])
                                            ),
                                            split = "__"
                                            )
                                            )[7], 
                                            split = "&", fixed = T
                                            )
                                            ) 
                                            # Checking if there are "-" in the CovMod, deleting degrees of polynomial
                                            if (grepl(pattern = "-", x = CovMod[1])) {
                                                
                                                CovMod <- unlist(strsplit(x = CovMod , split = "-"))
                                                CovMod <- CovMod[ seq(from = 1, to = length(CovMod), by = 2) ]
                                            } 
                                            
                                            #??? take only the one that is nor NA of y = DF$General[!is.na(DF$General[, nameGasVolt]), nameGasVolt]
                                            is.not.NA.y <- which(complete.cases(DF$General[,c(nameGasVolt,CovMod)]))
                                            is.NA.y     <- setdiff(1:nrow(DF$General), is.not.NA.y)
                                            Matrice <- data.frame(DF$General[is.not.NA.y, CovMod],
                                                                  row.names = row.names(DF$General[is.not.NA.y,]),
                                                                  stringsAsFactors = FALSE)
                                            names(Matrice) <- CovMod
                                        } else {
                                            Matrice <- NULL
                                        }
                                        # Using the reverse calibration function (measuring function) to extrapolate calibration
                                        DF$General[is.not.NA.y, nameGasMod] <- Meas_Function(y        = DF$General[is.not.NA.y, nameGasVolt], 
                                                                                             Mod_type = Mod_type , 
                                                                                             Model    = Model.i,
                                                                                             Matrice  = Matrice
                                        )
                                        DF$General <- DF$General[!duplicated(DF$General$date),]
                                        
                                        # Removing na for nameGasMod either nameGasVolt missing or CovMod missing
                                        DF$General[is.NA.y    , nameGasMod] <- NA
                                        
                                        # Removing negative values if requested
                                        if (input[[paste0("Neg.mod",k)]]) DF$General[ which(DF$General[, nameGasMod] < 0), nameGasMod] <- NA
                                    } else cat(paste0("[shiny, General.cal()] INFO, there is no calibration function for sensors: ", list.name.sensors()[k], "\n"))       
                                }
                            }
                        }
                        
                        progress$set(message = "[shiny, General.cal()] INFO, Calibrating raw sensor values to ppb,ug.m-3 or ppm,mg.m-3", value = 1)
                        #progress$close()
                        
                    } else cat("[shiny, General.cal()] INFO, ADD A SHINY ALERT THAT THERE IS NO General.Rdata.file\n")
                    
                    cat("-----------------------------------------------------------------------------------\n")
                } 
                
                # Resetting CheckBoxes and Forces...
                if (Neg$Forced)              Neg$Forced              <- FALSE
                if (Outliers.Sens$Forced)    Outliers.Sens$Forced    <- FALSE
                if (Outliers.Ref$Forced)     Outliers.Ref$Forced     <- FALSE
                if (Conv$Forced)             Conv$Forced             <- FALSE
                if (Cal$Forced)              Cal$Forced              <- FALSE
                if (Warm$Forced) Warm$Forced <- FALSE
                if (TRh$Forced)  TRh$Forced  <- FALSE
                if (Inv$Forced)  Inv$Forced  <- FALSE
                
                for (i in 1:length(list.name.sensors())) {
                    if (input[[paste0("Apply.Warm"   ,i)]]) updateCheckboxInput(session, inputId = paste0("Apply.Warm"   ,i), label = NULL, value = FALSE)
                    if (input[[paste0("Apply.TRh"    ,i)]]) updateCheckboxInput(session, inputId = paste0("Apply.TRh"    ,i), label = NULL, value = FALSE)
                    if (input[[paste0("Apply.Invalid",i)]]) updateCheckboxInput(session, inputId = paste0("Apply.Invalid",i), label = NULL, value = FALSE)
                    if (input[[paste0("Apply.S.Out"  ,i)]]) updateCheckboxInput(session, inputId = paste0("Apply.S.Out"  ,i), label = NULL, value = FALSE)     
                    if (input[[paste0("Apply.conv"   ,i)]]) updateCheckboxInput(session, inputId = paste0("Apply.conv"   ,i), label = NULL, value = FALSE)
                    if (input[[paste0("Apply.cal"    ,i)]]) updateCheckboxInput(session, inputId = paste0("Apply.cal"    ,i), label = NULL, value = FALSE)
                }
                for (i in 1:length(Config()[[2]]$gas.reference)) {
                    if (input[[paste0("Apply.R.Out"  ,i)]]) updateCheckboxInput(session, inputId = paste0("Apply.R.Out"  ,i), label = NULL, value = FALSE)
                }
            }
        },
        priority = 120)
        
        # NavBar"Data Treatment", mainTabPanel "Calibration",  ----
        output$Calibration   <- renderPlot(Plot.Calibration()      , width = 'auto', height = 'auto')
        # Reactive FUN CalSet ----
        CalSet     <- reactive({
            
            # Get parameters for Selected Sensor
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            progress$set(message = "Select sensor parameters for calibration", value = 0.5)
            
            # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
            k    <- match(x = input$Sensors, table = list.name.sensors())
            # Selecting compounds associated with selected sensor
            gas.sensor    <-  list.gas.sensors()[k]
            # selecting the sensor name
            name.sensor <- input$Sensors
            # Determining the type of model use for calibration
            if (input[[paste0("Cal",k)]] == "") NewCalSet <- "" else {
                for (i in Models) {
                    if (grepl(pattern = paste0("_",i,"_"), x = input[[paste0("Cal",k)]])) {
                        NewCalSet <- i
                        break
                    }
                }
            }
            # Limit Value, DQOs, UAT, LAT
            if (gas.sensor == "Carbon_monoxide") {
                if (input[[paste0("Ref.unit" , i.sensors()[k])]] == "ppm") {
                    LV = 10/1.34
                    IT = NA
                    AT = NA
                } else if (input[[paste0("Ref.unit" , i.sensors()[k])]] == "ppb") { 
                    LV = 10000/1.34
                    IT = NA
                    AT = NA
                } else if (input[[paste0("Ref.unit" , i.sensors()[k])]] == "mg/m3") { 
                    LV = 10; IT = NA; AT = NA
                } else if (input[[paste0("Ref.unit" , i.sensors()[k])]] == "ug/m3") { 
                    LV = 10000; IT = NA; AT = NA
                }
                DQO.I = 0.25 * LV
                DQO.M = 0.50 * LV
                DQO.O = 0.75 * LV
                LAT   = 0.50 * LV
                UAT   = 0.70 * LV
            } else if (gas.sensor == "Nitrogen_dioxide") {
                if (input[[paste0("Ref.unit" , i.sensors()[k])]] == "ppb") {
                    LV = 200/1.91
                    IT = NA
                    AT = 400/1.91
                } else if (input[[paste0("Ref.unit" , i.sensors()[k])]] == "ug/m3") {LV = 200; IT = NA; AT = 400}
                DQO.I = 0.25 * LV
                DQO.M = 0.50 * LV
                DQO.O = 0.75 * LV
                LAT   = 0.50 * LV
                UAT   = 0.70 * LV
            } else if (gas.sensor == "Nitric_oxide") {
                LV    = NA
                IT    = NA
                AT    = NA
                DQO.I = NA
                DQO.M = NA
                DQO.O = NA
                LAT   = NA
                UAT   = NA
            } else if (gas.sensor == "Ozone") {
                if (input[[paste0("Ref.unit" , i.sensors()[k])]] == "ppb") {
                    LV = 120/2.05
                    IT = 180/2.05
                    AT = 240/2.05
                } else if (input[[paste0("Ref.unit" , i.sensors()[k])]] == "ug/m3") {
                    LV = 120
                    IT = 180
                    AT = 240
                } 
                DQO.I = 0.30 * LV
                DQO.M = 0.50 * LV
                DQO.O = 0.75 * LV
                LAT   = NA
                UAT   = NA
            } else if (gas.sensor == "Sulphur_dioxide") {
                # Using LV for 1 year time average
                if (input[[paste0("Ref.unit" , i.sensors()[k])]] == "ppb") {
                    LV = 120/2.05
                    IT = NA
                    AT = 500/2.05
                } else if (input[[paste0("Ref.unit" , i.sensors()[k])]] == "ug/m3") {LV = 350; IT = NA; AT = 500} 
                DQO.I = 0.25 * LV
                DQO.M = 0.50 * LV
                DQO.O = 0.75 * LV
                LAT   = 0.40 * LV
                UAT   = 0.60 * LV
            } else if (gas.sensor == "Benzene") {
                # Using LV for 1 year time average
                if (input[[paste0("Ref.unit" , i.sensors()[k])]] == "ppb") {
                    LV = 5/2.05
                    IT = NA
                    AT = NA
                } else if (input[[paste0("Ref.unit" , i.sensors()[k])]] == "ug/m3") {LV = 5; IT = NA; AT = NA} 
                DQO.I = 0.30 * LV
                DQO.M = 0.50 * LV
                DQO.O = 1.00 * LV
                LAT   = 0.40 * LV
                UAT   = 0.70 * LV
            } else if (gas.sensor == "PM10") {
                # Using LV for 24 hours time average
                LV    = 50
                IT    = NA
                AT    = NA
                DQO.I = 0.50 * LV
                DQO.M = 0.50 * LV
                DQO.O = 1.00 * LV
                LAT   = 0.50 * LV
                UAT   = 1.00 * LV
            }  else if (gas.sensor == "PM2.5") {
                # Using LV for 24 hours time average
                LV    = 25
                IT    = NA
                AT    = NA
                DQO.I = 0.50 * LV
                DQO.M = 0.50 * LV
                DQO.O = 1.00 * LV
                LAT   = 0.50 * LV
                UAT   = 1.00 * LV
            }
            
            CalSet <- data.frame(     
                # index (1,2,3,4, of the selected  sensors in uiFiltering, Calib and SetTime) corresponding of rows of ASE_name.cfg
                k                  = k,
                # Selecting compounds associated with selected sensor
                i                  = gas.sensor,
                # selecting the sensor
                name.gas           = Config()[[2]]$name.gas[match(x = input$Sensors, table = Config()[[2]]$name.sensor)],
                # Setting parameters
                nameGasRef         = paste0("Out.",Config()[[2]]$gas.reference2use[i.sensors()][k]),  # reference gas               
                nameGasVolt        = paste0(name.sensor,"_volt"),                                     # sensor gas in volt
                nameGasMod         = paste0(gas.sensor,"_modelled"),                                  # modelled sensor gas 
                unit.ref           = input[[paste0("Ref.unit" , i.sensors()[k])]],
                unit.sensor        = input[[paste0("Sens.unit", k)]],
                Sens.raw.unit      = input[[paste0("Sens.raw.unit", k)]],
                Reference.name     = input$Reference.name,
                AirsensEur.name    = AirsensEur.name(),
                name.sensor        = name.sensor,
                WDoutputMod        = file.path(DisqueFieldtestDir(),"Models"),
                WDoutput           = file.path(DisqueFieldtestDir(),"Calibration"),
                WDoutputStats      = file.path(DisqueFieldtestDir(),"Statistics"),
                WDModelled_gas     = file.path(DisqueFieldtestDir(),"Modelled_gas"),
                mod.eta.model.type = input[[paste0("Calibration",k)]],                                # Model for calibration"
                NewCalSet          = NewCalSet,                                                       # Model of the  CalSet()$Cal
                eta.model.type     = input[[paste0("Comparison",k)]],
                remove.neg         = input[[paste0("Neg.mod",k)]],
                Cal_Line           = input[[paste0("Cal.Line",k)]],                                   # "Method of calibation/extrapolation" 
                Cal                = paste0(Config()[[1]]$AirsensEur.name,"__",name.sensor,"__",input[[paste0("Cal",k)]]),# Selected calibration model  for the current sensor
                Multi.File         = file.path(DisqueFieldtestDir(),"General_data", paste0(ASE_name(),"_Multi_",input$Sensors,".cfg")), # Config file for calibration with Multivariables
                CovMod             = paste0(input[[paste0("CovMod",k)]], collapse = "&"),             # Selected List of covariates to calibrate
                LV                 = LV ,                                                             # limit for the gas.sensor
                LAT                = LAT ,                                                            # limit for the gas.sensor
                UAT                = UAT ,                                                            # limit for the gas.sensor
                AT                 = AT ,                                                             # limit for the gas.sensor
                DQO.I              = DQO.I,                                                           # Data quality Objective for the gas.sensor
                DQO.M              = DQO.M,                                                           # Data quality Objective for the gas.sensor
                DQO.O              = DQO.O,                                                           # Data quality Objective for the gas.sensor
                uxi                = as.numeric(input[[paste0("uxi",k)]]) ,                           # random uncertainty of the reference data
                Neg.mod            = as.logical(input[[paste0("Neg.mod",k)]]),                        # Remove negative extrapolated data
                stringsAsFactors = FALSE)
            
            progress$set(message = "Select sensor parameters for calibration", value = 0.5)
            return(CalSet)
        })
        # NavBar"Data Treatment", mainTabPanel "Invalid"-"PlotFiltering" ----
        # Reactive Multi.DF
        Multi.DF <- reactive({
            # DataFrame for calibration when MultiLinear model is used
            # depends on: 
            #  DisqueFieldtestDir(), input$Sensors, CalSet(), ASE_name()
            #  input$New.row.Multi
            
            #  Make it  reactive to input$New.row.Multi
            input$New.row.Multi
            
            nameFile <- file.path(DisqueFieldtestDir(),"General_data",paste0(ASE_name(),"_Multi_",input$Sensors,".cfg"))
            names.Covariates <- unlist(strsplit(CalSet()$CovMod, split = "&"))
            if (file.exists(nameFile)) {
                Multi.DF <- read.table(file             = nameFile, 
                                       header           = TRUE, 
                                       row.names        = NULL, 
                                       comment.char     = "#", 
                                       stringsAsFactors = FALSE
                )
                #DF <- dplyr::arrange(DF,Variables)
                
                # checking that the model covaraties and the covariates in the file are consistent
                if (length(names.Covariates) != length(Multi.DF$Covariates))  {
                    # not the same number of Covariates in Multi.file and selected -> new Multi.df
                    Multi.DF <- data.frame(Covariates = c(names.Covariates, "Intercept"), 
                                           Enabled    = rep(TRUE      , length(c(names.Covariates, "Intercept"))), 
                                           degree     = factor(c(rep(1, length(names.Covariates)), NA), 
                                                               levels = c(1,2,3,0), 
                                                               ordered = TRUE) ,
                                           Forced     = rep(FALSE     , length(c(names.Covariates, "Intercept"))),
                                           a0_an      = rep("1"       , length(c(names.Covariates, "Intercept"))),
                                           stringsAsFactors = FALSE)
                } else if (!all(names.Covariates %in% Multi.DF$Covariates & Multi.DF$Covariates %in% names.Covariates)) {
                    # not the same Covariates in Multi.file and selected -> new Multi.df
                    Multi.DF <- data.frame(Covariates = c(names.Covariates, "Intercept"), 
                                           Enabled    = rep(TRUE      , length(c(names.Covariates, "Intercept"))), 
                                           degree     = factor(c(rep(1, length(names.Covariates)), NA), 
                                                               levels = c(1,2,3,0), 
                                                               ordered = TRUE) ,
                                           Forced     = rep(FALSE     , length(c(names.Covariates, "Intercept"))),
                                           a0_an      = rep("1"       , length(c(names.Covariates, "Intercept"))),
                                           stringsAsFactors = FALSE)
                }
            } else {
                Multi.DF <- data.frame(Covariates = c(names.Covariates, "Intercept"), 
                                       Enabled    = rep(TRUE      , length(c(names.Covariates, "Intercept"))), 
                                       degree     = factor(c(rep(1, length(names.Covariates)), NA), 
                                                           levels = c(1,2,3,0), 
                                                           ordered = TRUE) ,
                                       Forced     = rep(FALSE     , length(c(names.Covariates, "Intercept"))),
                                       a0_an      = rep("1"       , length(c(names.Covariates, "Intercept"))),
                                       stringsAsFactors = FALSE)
            }
            return(Multi.DF)
        })
        output$Multi <- rhandsontable::renderRHandsontable({
            # converts Multi.DF() to rhandsontable object
            if (!is.null(Multi.DF())) rhandsontable::rhandsontable(Multi.DF()) %>% hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
        })
        output$ListValid <- renderText({
            # list of _Multi_ files of calibration for all sensors
            list.files(path    = file.path(DisqueFieldtestDir(),"General_data"), 
                       pattern = "_Multi_")
        })
        # Del
        observeEvent(input$Del.row.Multi, {
            # delete current Multi file 
            
            nameFile <- file.path(DisqueFieldtestDir(),"General_data",paste0(ASE_name(),"_Multi_",input$Sensors,".cfg"))
            file.remove(nameFile)
        })
        #new
        observeEvent(input$New.row.Multi, {
            # rows must be in increasing order before saving otherwise the following error stops the script:
            # Warning: Error in seq.default: 'by' must be of length 1
            
            click(id = "Del.row.Multi")

        })
        
        ## Save 
        observeEvent(input$Save.row.Multi, {
            # rows must be in increasing order before saving otherwise the following error stops the script:
            # Warning: Error in seq.default: 'by' must be of length 1
            
            finalDF <- hot_to_r(input$Multi)
            nameFile <- file.path(DisqueFieldtestDir(),"General_data",paste0(ASE_name(),"_Multi_",input$Sensors,".cfg"))
            write.table(finalDF, file = nameFile, row.names = FALSE)
            
        })
        
        # ObserveEvent Buttons to set the ranges of dates for calibration ----
        # Setting the date of DateCal1 according to the name of the calibration model when selecting another model 
        observeEvent({sapply(1:length(list.name.sensors()), function(i) input[[paste0("DateCALCal",i)]]) 
        }, {
            
            Splitted.Cal <- unlist(strsplit(x = paste0(Config()[[1]]$AirsensEur.name,"__",CalSet()$name.sensor,"__",input[[paste0("Cal",CalSet()$k)]]), split = "__"))
            # JRC_02__COMF200__nA__Linear.Robust__20170109__20170113__.rds
            Start <- strptime(Splitted.Cal[5], format = "%Y%m%d", tz = input$ref.tzone)
            End   <- strptime(Splitted.Cal[6], format = "%Y%m%d", tz = input$ref.tzone)
            updateDateRangeInput(session,
                                 inputId = paste0("DateCal",CalSet()$k),
                                 label   = NULL, 
                                 start   = Start,
                                 end     = End,
                                 min     = NULL,
                                 max     = NULL)
        },
        ignoreInit = TRUE
        )
        # Setting the dates DateCal1 to the one of input$Date1 (Covariates)
        observeEvent({sapply(1:length(list.name.sensors()), function(i) input[[paste0("DateCALCovCal",i)]])
        }, {
            #if (input$DateCALCovCal1>0) {
            Start <- input[[paste0("Date",CalSet()$k)]][1]
            End   <- input[[paste0("Date",CalSet()$k)]][2]
            updateDateRangeInput(session,
                                 inputId = paste0("DateCal",CalSet()$k),
                                 label   = NULL, 
                                 start   = Start,
                                 end     = End,
                                 min     = NULL,
                                 max     = NULL)
            updateDateRangeInput(session,
                                 inputId = paste0("DatePlotCal",CalSet()$k),
                                 label   = NULL, 
                                 start   = Start,
                                 end     = End,
                                 min     = NULL,
                                 max     = NULL)
        },
        ignoreInit = TRUE
        )
        # Setting the dates DateCal1 to the one of input$DatePlotMeas1
        observeEvent({sapply(1:length(list.name.sensors()), function(i) input[[paste0("DateCALExtCal",i)]])
        }, {
            #if (input$DateCALExtCal1>0) {
            Start <- input[[paste0("DatePlotMeas",CalSet()$k)]][1]
            End   <- input[[paste0("DatePlotMeas",CalSet()$k)]][2]
            updateDateRangeInput(session,
                                 inputId = paste0("DateCal",CalSet()$k),
                                 label   = NULL, 
                                 start   = Start,
                                 end     = End,
                                 min     = NULL,
                                 max     = NULL)
        },
        ignoreInit = TRUE
        )
        # Reactive FUN Plot.Calibration
        Plot.Calibration     <- reactive({
            
            # Setting calibration models and plotting calibration
            # depends: input[[paste0("DateCal",CalSet()$k)]],input[[paste0("Date",CalSet()$k)]],
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
            
            # Date range: intersection between the range for calibration and the range for plotting
            DateIN  <- max(c(input[[paste0("DateCal",CalSet()$k)]][1],input[[paste0("DatePlotCal",CalSet()$k)]][1]), na.rm = TRUE)
            DateEND <- min(c(input[[paste0("DateCal",CalSet()$k)]][2],input[[paste0("DatePlotCal",CalSet()$k)]][2]), na.rm = TRUE)
            
            op <- par(no.readonly = TRUE)
            # Restoring graphical parameters on exit of function
            par(mfrow = c(1,1))
            on.exit(par(op))
            
            # Method:
            # 1 - if "New calibration with current data" is selected
            #     1.1 Check that the model does not already exist for the same sensor, model type, unit, dateIN/date/END and covariates
            #     1.2 Check that the model does not already exist for Covariates. If model does not alrady exist then calibrate
            # 2 - if a "Previous calibration" is selected
            #     2.1 Check that CalSet()$Cal != "" is not empty, if empty message select model or New Calibration
            #     2.2 Check that the selected model is corect for model type, unit, dates and Covariates, if no message Select correct model of New Calibration
            #     2.3.1 Check covariates (no order)
            #     2.3.2 Check the start/end dates 
            #     2.4 Plot the existing calibration model
            # 3 - if "Calibration with slope and intercept below" is selected
            #     3.1 Check that the unit, slope and intercept are corrects
            #     3.1 Create Linear model based of the slope and intercept given
            
            # 1
            if (CalSet()$Cal_Line == "New calibration with current data") { 
                
                # 1.1 Check that the model does not already exist for the same sensor, model type, unit, dateIN/date/END
                # List of files for current AirSensEUR name, sensor name, unit, model type, start and end dates
                ModelFiles <-  list.files(path = CalSet()$WDoutputMod, 
                                          pattern = glob2rx(
                                              paste0(CalSet()$AirsensEur.name,"*", 
                                                     CalSet()$name.sensor,"*",
                                                     CalSet()$Sens.raw.unit,"*",
                                                     CalSet()$mod.eta.model.type,"*",
                                                     format(DateIN ,"%Y%m%d"),"*_",format(DateEND,"%Y%m%d"),"*__*"
                                              )
                                          )
                )
                
                # Flag to request new model
                NewModelFlag <- TRUE
                
                # Models exist with current AirSensEUR name, sensor name, unit, model type, start and end dates
                # ce
                if (!identical(ModelFiles,character(0))) {
                    
                    # Checking if within existing ModelFiles there is the same set of covariates only for MultiLinear Models
                    if (CalSet()$mod.eta.model.type == "MultiLinear") {
                        
                        # The model use covariates, it is fine to ask for a model with same AirSensEUR name, sensor name, unit, model type, start and end dates, if other covariates are requested
                        
                        # listing the Covariates
                        for (i in ModelFiles) {
                            
                            # Covariates in the selected model
                            Splitted.Model      <- unlist(strsplit(x = CalSet()$Cal, split = "__"))
                            Covariates.Model    <- str_replace(Splitted.Model[7], pattern = ".rds", replacement = "")
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
                                Degrees <-  Multi.File.df[Multi.File.df$Covariates == Covariates.CovMod, "degree"]
                                Covariates.CovMod <- paste(unlist(strsplit(split = "&", CalSet()$CovMod)), Degrees, sep = "-")
                                
                            } else if (grepl(pattern = "-", x = Covariates.Model)) {
                                
                                Degrees <-  base::rep(1, times = length(Covariates.CovMod))
                                Covariates.CovMod <- paste(unlist(strsplit(split = "&", CalSet()$CovMod)), Degrees, sep = "-")
                            } 
                            
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
                               format(DateIN ,"%Y%m%d")," and ",format(DateEND,"%Y%m%d"),"\n")) # ADD Calibration DATES
                    
                    # Setting parameters
                    model.log          = TRUE 
                    timeseries.display = FALSE 
                    process.step       = "Calibration"
                    
                    # Checking if there are data to calibrate
                    General            <- DF$General[ DF$General$date >= DateIN & DF$General$date <= DateEND,]
                    if (all(is.na(General[,CalSet()$nameGasRef])) | all(is.na(General[,CalSet()$nameGasVolt]))) {
                        
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
                            animation         = FALSE
                        )
                        plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                        text(1,1,my_message)
                    } else {
                        
                        # Calibration model
                        Validation.tool(General            = General,
                                        DateIN             = DateIN ,
                                        DateEND            = DateEND,
                                        name.gas           = CalSet()$name.gas,
                                        model.log          = model.log ,
                                        nameGasRef         = CalSet()$nameGasRef,   # reference gas               
                                        nameGasVolt        = CalSet()$nameGasVolt,  # sensor gas in volt (or nA)
                                        nameGasMod         = CalSet()$nameGasMod,   # modelled sensor gas 
                                        unit.ref           = CalSet()$unit.ref, 
                                        unit.sensor        = CalSet()$unit.sensor,
                                        Sens.raw.unit      = CalSet()$Sens.raw.unit,
                                        Reference.name     = CalSet()$Reference.name,
                                        AirsensEur.name    = CalSet()$AirsensEur.name,
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
                                        PlotCal            = FALSE
                        )
                        
                        # updating the calibration function in GUI and claibration to use "Previous calibration"
                        updateRadioButtons(session, inputId = paste0("Cal.Line", CalSet()$k), label = "Method of extrapolation", 
                                           choices = list("New calibration with current data","Previous calibration","Calibration with slope and intercept below"), 
                                           selected = "Previous calibration")
                        #Newchoices = list.files(path = CalSet()$WDoutputMod, pattern = glob2rx(paste0(CalSet()$AirsensEur.name,"_", CalSet()$name.sensor,"_*.rds")))
                        # Removing the name of AirSensEUR from the model because there may be a confusion between Influx name and SOS name
                        Newchoices <- substr(list.files(path    = CalSet()$WDoutputMod, 
                                                        pattern = glob2rx(paste0(Config()[[1]]$AirsensEur.name,"*",list.name.sensors()[CalSet()$k],"*",".rds"))), 
                                             start = nchar(paste0(Config()[[1]]$AirsensEur.name,"__",list.name.sensors()[CalSet()$k],"__")) + 1,
                                             stop  = nchar(list.files(path    = CalSet()$WDoutputMod, 
                                                                      pattern = glob2rx(paste0(Config()[[1]]$AirsensEur.name,"*",list.name.sensors()[CalSet()$k],"*",".rds"))))
                        )
                        
                        # Current model
                        # Use separator "__" because the character "_" maybe be used in the name of ASE
                        if (CalSet()$mod.eta.model.type == "MultiLinear") {
                            if (file.exists(CalSet()$Multi.File)) {
                                
                                # read Multi.File
                                Multi.File.df <-  read.table(file             = CalSet()$Multi.File, 
                                                             header           = TRUE, 
                                                             row.names        = NULL, 
                                                             comment.char     = "#", 
                                                             stringsAsFactors = FALSE
                                )
                                
                                # their degree of polynomial
                                Degrees <-  Multi.File.df[Multi.File.df$Covariates == unlist(strsplit(split = "&",CalSet()$CovMod)), "degree"]
                                
                            } else Degrees <-  base::rep(1, times = length(unlist(strsplit(split = "&",CalSet()$CovMod))) )
                            
                            namesCovariates <- paste0(paste(unlist(strsplit(split = "&", CalSet()$CovMod)), Degrees, sep = "-"), collapse = "&")  
                            
                        } else namesCovariates = ""
                        
                        NewModel  <- paste0(paste(CalSet()$Sens.raw.unit, CalSet()$mod.eta.model.type,
                                                  format(DateIN,"%Y%m%d"),format(DateEND,"%Y%m%d"),namesCovariates,sep = "__"),".rds")
                        # NewModel <- paste0(CalSet()$AirsensEur.name,"_",CalSet()$name.sensor,"__",CalSet()$Sens.raw.unit, "__",isolate(CalSet()$mod.eta.model.type),"__",
                        #                    format(DateIN,"%Y%m%d"),"__",format(DateEND,"%Y%m%d"),".rds")
                        updateSelectInput(session, inputId = paste0("Cal", CalSet()$k), label = "Select a previous calibration ", 
                                          choices = Newchoices, selected = NewModel[1])
                    }
                } else { 
                    
                    # 2.2
                    # Message Model already esists
                    my_message <- paste0("[shiny, Plot.Calibration()] ERROR, a calibration model with the same \"Raw unit of sensor data\", 
                                         \" Model for calibration\", date range of calibration in \"SetTime\" already exists click on \"Previous Calibration \" 
                                         and select it in \"Selected previous calibration\".\n")
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
                        animation         = FALSE
                    )
                    plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                    text(1,1,my_message)
                }
            } else  if (CalSet()$Cal_Line == "Previous calibration") {      
                
                # 2
                
                # plotting "Previous calibration" if CalSet()$Cal exists, the same CalSet()$Sens.raw.unit, same CalSet()$mod.eta.model.type, 
                # We keep floating DateIN and DateEnd to be able to play with them in SetTime
                # 2.1 Null calibration model
                if (CalSet()$Cal != "") {                                    
                    
                    # 2.2 Correct units and model type
                    if (grepl(pattern = glob2rx(paste0("*_",CalSet()$Sens.raw.unit,"*_",CalSet()$mod.eta.model.type,"_*",".rds")), x = CalSet()$Cal)) { 
                        
                        # 2.3.2 Checking Correct covariates for model with covariates
                        # Covariates in the selected model
                        Splitted.Cal      <- unlist(strsplit(x = CalSet()$Cal, split = "__"))
                        Covariates.Cal    <- str_replace(Splitted.Cal[7], pattern = ".rds", replacement = "")
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
                            Degrees <-  Multi.File.df[Multi.File.df$Covariates == Covariates.CovMod, "degree"]
                            Covariates.CovMod <- paste(unlist(strsplit(split = "&", CalSet()$CovMod)), Degrees, sep = "-")
                            
                        } else if (grepl(pattern = "-", x = Covariates.Cal[1])) {
                            
                            Degrees <-  base::rep(1, times = length(Covariates.CovMod))
                            Covariates.CovMod <- paste(unlist(strsplit(split = "&", CalSet()$CovMod)), Degrees, sep = "-")
                        } 
                        
                        if (!(CalSet()$mod.eta.model.type == "MultiLinear") | 
                            (CalSet()$mod.eta.model.type == "MultiLinear" & all(Covariates.Cal %in% Covariates.CovMod) & all(Covariates.CovMod %in%  Covariates.Cal))) {
                            
                            # Either the model has no covariates or the covariates of the model are correlctly selected in CovMod 
                            
                            
                            # 2.3.1 correct dates
                            if (grepl(pattern = glob2rx(paste0("*_",
                                                               format(input[[paste0("DateCal",CalSet()$k)]][1],"%Y%m%d"),"*_*",
                                                               format(input[[paste0("DateCal",CalSet()$k)]][2],"%Y%m%d"), "*.rds")), 
                                      x = CalSet()$Cal)
                            ) {
                                
                                
                                
                                # 2.4 
                                cat(paste0("[shiny, Plot.Calibration()], INFO, using previous calibration ",CalSet()$Cal, " with ", 
                                           CalSet()$NewCalSet, " calibration method\n"))
                                
                                # Loading previous model
                                Model.i         <- readRDS(file = file.path(CalSet()$WDoutputMod, CalSet()$Cal))
                                if (class(Model.i$x)== "matrix") x <- as.numeric(Model.i$x[,2]) else x <- Model.i$model[,c("x")]
                                y <-  Model.i$model[,c("y")]
                                # Changing axis labels
                                Pattern  <- rbind(c("Out.", ""),c("Ref.", "Reference "),c("ppm",""),c("ppb",""),c("_",""))
                                if (nrow(Pattern) > 0) A.Labels <- gsub(pattern = Pattern[1,1], replacement = Pattern[1,2], x = CalSet()$nameGasRef)
                                if (nrow(Pattern) > 1) for (i in 2:nrow(Pattern)) A.Labels <- gsub(pattern = Pattern[i,1], replacement = Pattern[i,2], x = A.Labels)
                                if (any(CalSet()$mod.eta.model.type %in% "gam")) {
                                    A.Labels.X <- paste0( CalSet()$AirsensEur.name, ", raw data of ", CalSet()$name.sensor," in ",CalSet()$Sens.raw.unit)
                                    A.Labels.Y <- paste0(A.Labels ," in ",CalSet()$unit.ref, " at ",CalSet()$Reference.name)
                                } else {
                                    A.Labels.X <- paste0(A.Labels ," in ",CalSet()$unit.ref, " at ",CalSet()$Reference.name)
                                    A.Labels.Y <- paste0( CalSet()$AirsensEur.name, ", raw data of ", CalSet()$name.sensor," in ",CalSet()$Sens.raw.unit)
                                }
                                EtalLim <- Etalonnage( x = x, 
                                                       s_x = NULL, 
                                                       y = y, 
                                                       s_y = NULL, 
                                                       AxisLabelX = A.Labels.X, 
                                                       AxisLabelY = A.Labels.Y, 
                                                       Title = paste0(CalSet()$AirsensEur.name, ": ","Calibrated ", CalSet()$name.sensor," data from ",
                                                                      format(DateIN,"%d-%b-%y")," to ",format(DateEND,"%d-%b-%y"), " at ",CalSet()$Reference.name
                                                                      , " using ", isolate(CalSet()$NewCalSet), " method"), 
                                                       Marker = 1, 
                                                       Couleur = "blue", 
                                                       ligne = 'p', 
                                                       XY_same = FALSE, 
                                                       lim = NULL, 
                                                       steps = c(10,10), 
                                                       digitround = c(2,3), 
                                                       marges = c(4,4,3,0.5)
                                )
                                
                                Cal_Line(x             = x, 
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
                                         Covariates    = NULL
                                ) 
                                
                            } else {
                                
                                # 2.3.2 start/end dates
                                cat("[shiny, Plot.Calibration()] ERROR, \"Range of date for calibration:\" or \"Range for plotting calibration\" not consistent with \"Selected previous calibration\". Change \"Range of date for calibration:\" or select  another previous calibration.\n")
                                plot(1,1,col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1.2)
                                text(1,1,"[shiny, Plot.Calibration()] ERROR, \"Range of date for calibration:\" or \"Range for plotting calibration\" not consistent with \"Selected previous calibration\". Change \"Range of date for calibration:\" or select  another previous calibration.\n")
                            }
                        } else {
                            
                            # 2.3.1 Incorrect covariate lists
                            my_message <- paste0("[shiny, Plot.Calibration()] ERROR, \"List of covariates to calibrate\" not consistent with \"Selected previous calibration\".
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
            } else if (CalSet()$Cal_Line == "Calibration with slope and intercept below") {
                
                #### TO BE CODED
                
            }
            
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            
            # Opening the Calib TabSet for GUI consistency select correct sensor in tabsetPanels Calib.Sensors and SetTime.Sensors
            isolate({
                #if (isolate(input$Calib_data) != "Calib" & isolate(input$Calib_data) != "SetTime") updateTabsetPanel(session, inputId = "Calib_data", selected = "Calib")
                updateTabsetPanel(session, inputId = "Calib.Sensors", selected = input$Sensors)
                updateTabsetPanel(session, inputId = "SetTime.Sensors"      , selected = input$Sensors)
            })
            progress$set(message = "[shiny, Plot.Calibration()] INFO, Plotting scatter plot of calibration", value = 1)
            on.exit(progress$close())
        })
        # NavBar"Data Treatment", mainTabPanel SummaryCal - Calibration  ----
        output$SummaryCal   <- renderPrint(Table.SummaryCal())
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
                    Model.i         <- readRDS(file = file.path(CalSet()$WDoutputMod, CalSet()$Cal))
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
            
            # Opening the Calib TabSet for GUI consistency select correct sensor in tabsetPanels Calib.Sensors and SetTime.Sensors
            isolate({
                #    if (isolate(input$Calib_data) != "Calib" & isolate(input$Calib_data) != "SetTime") updateTabsetPanel(session, inputId = "Calib_data", selected = "Calib")
                updateTabsetPanel(session, inputId = "Calib.Sensors", selected = input$Sensors)
                updateTabsetPanel(session, inputId = "SetTime.Sensors"      , selected = input$Sensors)
            })
            progress$set(message = "Summary table of calibration model", value = 1)
            on.exit(progress$close())
            
            print(return.SummaryCal)
            
        })
        # NavBar"Data Treatment", mainTabPanel Calibrated - Calibration ----
        output$Calibrated   <- renderPlot(Plot.Calibrated(), width = 'auto', height = 'auto')
        # Reactive FUN Plot.Calibrated
        Plot.Calibrated     <- reactive({
            
            # Plotting calibrated data
            # depends: input[[paste0("DateCal",CalSet()$k)]],input[[paste0("Date",CalSet()$k)]],
            #           CalSet()$Cal_Line, CalSet()$Cal)
            #          
            # Isolates: CalSet()$mod.eta.model.type, input[[paste0("Calibration",CalSet()$k)]] when calibrating
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "[shiny, Plot.Calibrated()] INFO,  plotting calibration of sensor", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat(paste0("[shiny, Plot.Calibrated()] INFO, plotting calibration of sensor ", input$Sensors, sep = "\n"))
            
            # Date range: intersection between the range for calibration and the range for plotting
            DateIN  <- max(c(input[[paste0("DateCal",CalSet()$k)]][1],input[[paste0("DatePlotCal",CalSet()$k)]][1]), na.rm = TRUE)
            DateEND <- min(c(input[[paste0("DateCal",CalSet()$k)]][2],input[[paste0("DatePlotCal",CalSet()$k)]][2]), na.rm = TRUE)
            
            op <- par(no.readonly = TRUE)
            # Restoring graphical parameters on exit of function
            par(mfrow = c(1,1))
            on.exit(par(op))
            
            if (CalSet()$Cal_Line == "Previous calibration") {      
                
                # 2
                
                # plotting "Previous calibration" if CalSet()$Cal exists, the same CalSet()$Sens.raw.unit, same CalSet()$mod.eta.model.type, 
                # We keep floating DateIN and DateEnd to be able to play with them in SetTime
                # 2.1 Null calibration model
                if (CalSet()$Cal != "") {                                    
                    
                    # 2.2 Correct units and model type
                    if (grepl(pattern = glob2rx(paste0("*_",CalSet()$Sens.raw.unit,"*_",CalSet()$mod.eta.model.type,"_*",".rds")), x = CalSet()$Cal)) { 
                        
                        # 2.3.2 Checking Correct covariates for model with covariates
                        # Covariates in the selected model
                        Splitted.Cal      <- unlist(strsplit(x = CalSet()$Cal, split = "__"))
                        Covariates.Cal    <- str_replace(Splitted.Cal[7], pattern = ".rds", replacement = "")
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
                            Degrees <-  Multi.File.df[Multi.File.df$Covariates == Covariates.CovMod, "degree"]
                            Covariates.CovMod <- paste(unlist(strsplit(split = "&", CalSet()$CovMod)), Degrees, sep = "-")
                            
                        } else if (grepl(pattern = "-", x = Covariates.Cal[1])) {
                            
                            Degrees <-  base::rep(1, times = length(Covariates.CovMod))
                            Covariates.CovMod <- paste(unlist(strsplit(split = "&", CalSet()$CovMod)), Degrees, sep = "-")
                        } 
                        
                        if (!(CalSet()$mod.eta.model.type == "MultiLinear") | 
                            (CalSet()$mod.eta.model.type == "MultiLinear" & all(Covariates.Cal %in% Covariates.CovMod) & all(Covariates.CovMod %in%  Covariates.Cal))) {
                            
                            # Either the model has no covariates or the covariates of the model are correlctly selected in CovMod 
                            
                            
                            # 2.3.1 correct dates
                            if (grepl(pattern = glob2rx(paste0("*_",
                                                               format(input[[paste0("DateCal",CalSet()$k)]][1],"%Y%m%d"),"*_*",
                                                               format(input[[paste0("DateCal",CalSet()$k)]][2],"%Y%m%d"), "*.rds")), 
                                      x = CalSet()$Cal)
                            ) {
                                
                                
                                
                                # 2.4 
                                cat(paste0("[shiny, Plot.Calibrated()] INFO, using previous calibration ",CalSet()$Cal, " with ", 
                                           CalSet()$NewCalSet, " calibration method\n"))
                                
                                # Loading previous model an updating General.cal
                                x <- DF$General[DF$General$date > DateIN & DF$General$date <= DateEND,c(CalSet()$nameGasRef)]
                                y <- DF$General[DF$General$date > DateIN & DF$General$date <= DateEND,c(CalSet()$nameGasMod)]
                                # Changing axis labels
                                Pattern  <- rbind(c("Out.", ""),c("Ref.", "Reference "),c("ppm",""),c("ppb",""),c("_",""))
                                if (nrow(Pattern) > 0) A.Labels <- gsub(pattern = Pattern[1,1], replacement = Pattern[1,2], x = CalSet()$nameGasRef)
                                if (nrow(Pattern) > 1) for (i in 2:nrow(Pattern)) A.Labels <- gsub(pattern = Pattern[i,1], replacement = Pattern[i,2], x = A.Labels)
                                A.Labels.X <- paste0(A.Labels ," in ",CalSet()$unit.ref, " at ",CalSet()$Reference.name)
                                A.Labels.Y <- paste0( CalSet()$AirsensEur.name, ", Calibrated data of ", CalSet()$name.sensor," in ",CalSet()$unit.sensor)
                                EtalLim <- Etalonnage( x = x, s_x = NULL, y = y, s_y = NULL, 
                                                       AxisLabelX = A.Labels.X, AxisLabelY = A.Labels.Y, 
                                                       Title = paste0(CalSet()$AirsensEur.name, ": ","Calibrated ", CalSet()$name.sensor," data, from ",
                                                                      format(DateIN,"%y-%m-%d")," to ",format(DateEND,"%y-%m-%d")
                                                       ), #, " at ",CalSet()$Reference.name, " using ", isolate(CalSet()$NewCalSet), " method"
                                                       Marker = 1, 
                                                       Couleur = "blue", 
                                                       ligne = 'p', 
                                                       XY_same = TRUE, 
                                                       lim = NULL, 
                                                       steps = c(10,10), 
                                                       digitround = c(1,1), 
                                                       marges = c(4,4,3,0.5)
                                ) # Add units
                                
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
                                                       Covariates    = NULL
                                ) 
                                
                                # Saving plot if requested
                                if (input$SavePlot) {
                                    dev.copy(png,
                                             filename = file.path(CalSet()$WDoutput, 
                                                                  paste0(CalSet()$Cal,"_Calibrated_",
                                                                         format(DateIN, "%Y%m%d"),"_",
                                                                         format(DateEND,"%Y%m%d"),".png")), 
                                             units = "cm", 
                                             width = 20, 
                                             height = 20,
                                             res = 300 
                                    )
                                    dev.off()
                                    cat(paste0("[shiny] INFO, ", CalSet()$Cal,"_Calibrated_",
                                               format(DateIN, "%Y%m%d"),"_",
                                               format(DateEND,"%Y%m%d"),".png saved in ", CalSet()$WDoutput, "\n" ))
                                    
                                    updateCheckboxInput(session, 
                                                        inputId = "SavePlot", 
                                                        label = NULL, 
                                                        value = FALSE)
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
            } else if (CalSet()$Cal_Line == "Calibration with slope and intercept below") {
                
                #### TO BE CODED
                
            }
            
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            
            # Opening the Calib TabSet for GUI consistency select correct sensor in tabsetPanels Calib.Sensors and SetTime.Sensors
            isolate({
                #if (isolate(input$Calib_data) != "Calib" & isolate(input$Calib_data) != "SetTime") updateTabsetPanel(session, inputId = "Calib_data", selected = "Calib")
                if (input$Calib.Sensors   != input$Sensors) updateTabsetPanel(session, inputId = "Calib.Sensors"  , selected = input$Sensors)
                if (input$SetTime.Sensors != input$Sensors) updateTabsetPanel(session, inputId = "SetTime.Sensors", selected = input$Sensors)
            })
            
            progress$set(message = "[shiny, Plot.Calibrated()] INFO,  plotting calibration of sensor", value = 1)
            on.exit(progress$close())
            
            if (exists("Comparison")) return(Comparison)
        })
        # NavBar"Data Treatment", mainTabPanel TimeSeries - Calibration ----
        output$CalibrationTS <- renderPlot(Plot.CalibrationTS()    , width = 'auto', height = 'auto')
        # Reactive FUN Plot.CalibrationTS
        Plot.CalibrationTS   <- reactive({
            
            #----------------------------------------------------------CR
            # plotting correlation in time series of validated data with Covariates
            #----------------------------------------------------------CR
            # depends:
            #   input$Sensors,
            #   Config()[[2]]$name.sensor
            #   i.sensors()
            #   DF$General
            #   INFLUX()[[4]]
            #   input[[paste0("Sens",k)]]
            #   input[[paste0("Date",k)]]
            # isolates:
            
            progress <- shiny::Progress$new()
            progress$set(message = "[shiny, Plot.CalibrationTS()] INFO, plotting time series of Calibration data", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
            # Opening the Calib TabSet for GUI consistency
            isolate({
                #if (input$Calib_data != "Calib" & input$Calib_data != "SetTime") updateTabsetPanel(session, inputId = "Calib_data", selected = "Calib")
                updateTabsetPanel(session, inputId = "Calib.Sensors", selected = input$Sensors)
                updateTabsetPanel(session, inputId = "SetTime.Sensors"      , selected = input$Sensors)
            })
            
            # Date range: intersection between the range for calibration and the range for plotting
            DateIN  <- max(c(input[[paste0("DateCal",CalSet()$k)]][1],input[[paste0("DatePlotCal",CalSet()$k)]][1]), na.rm = TRUE)
            DateEND <- min(c(input[[paste0("DateCal",CalSet()$k)]][2],input[[paste0("DatePlotCal",CalSet()$k)]][2]), na.rm = TRUE)
            
            op <- par(no.readonly = TRUE)
            par(mfrow=c(1,1))
            # Restoring graphical parameters on exit of function
            on.exit(par(op))
            
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat("[shiny, Plot.CalibrationTS()] INFO, plotting time series of Calibration data\n")
            
            if (all(is.na(DF$General[,c(CalSet()$nameGasRef,CalSet()$nameGasVolt)]),na.rm = TRUE)) {
                cat("[shiny, Plot.CalibrationTS()] ERROR, All data in calibration time series are empty, not plotting any times series\n")
            } else {
                # Sensor relationships with other variables
                cat(paste0("[shiny, Plot.CalibrationTS()] INFO, Plot calibrate sensor data and reference data for Sensor ", input$Sensors, " in order to check relationships with other variables\n"))
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
                timePlot(mydata    = subset(DF$General, date >= DateIN & date <= DateEND), 
                         pollutant = c(CalSet()$nameGasRef,CalSet()$nameGasMod), name.pol = Name.pol, group=TRUE, date.pad=TRUE, auto.text = FALSE, lty=c(1,1), col = c("red", "green"), ylab = "",
                         main      = paste0(CalSet()$AirsensEur.name, ": Calibration ", CalSet()$name.sensor," from ",format(DateIN,"%d-%b-%y")," to ", format(DateEND,"%d-%b-%y")
                                            , " with model ",isolate(CalSet()$NewCalSet)," at ",CalSet()$Reference.name),
                         ref.x     = list(v = c(input[[paste0("DateCal",CalSet()$k)]][1],input[[paste0("DateCal",CalSet()$k)]][2]), lty = c(1, 1), col = c("black", "black"), lwd = c(2,2))
                )
                # Saving plot if requested
                if (input$SavePlot) {
                    dev.copy(png,
                             filename = file.path(CalSet()$WDoutput, 
                                                  paste0(CalSet()$Cal,"_Calibration_ts_",
                                                         format(DateIN, "%Y%m%d"),"_",
                                                         format(DateEND,"%Y%m%d"),".png")), 
                             #units = "cm", 
                             #width = 35.55, 
                             #height = 20,
                             res = 300 
                    )
                    dev.off() 
                    cat(paste0("[shiny] INFO, ", CalSet()$Cal,"_Calibration_ts_",
                               format(DateIN, "%Y%m%d"),"_",
                               format(DateEND,"%Y%m%d"),".png saved in ", CalSet()$WDoutput, "\n" ))
                    updateCheckboxInput(session, 
                                        inputId = "SavePlot", 
                                        label = NULL, 
                                        value = FALSE)
                }
            }
            cat("-----------------------------------------------------------------------------------\n")
            
            progress$set(message = "[shiny, Plot.CalibrationTS()] INFO, plotting time series of Calibration data", value = 1)
            progress$close()
            
            cat("\n")
        })
        # NavBar"Data Treatment", mainTabPanel ResidualMatrix - Calibration  ----
        output$ResCalMatrix  <- renderPlot(Plot.ResCalMatrix()   , width = 'auto', height = 'auto')
        # Reactive FUN Plot.ResCalMatrix
        Plot.ResCalMatrix    <- reactive({
            # Plotting correalation matrix using pairs()
            # depends:
            #   input$Sensors,
            #   Config()[[2]]$name.sensor
            #   i.sensors()
            #   DF$General
            #   INFLUX()[[4]]
            #   input[[paste0("Sens",k)]]
            #   input[[paste0("Date",k)]]
            # isolates:
            
            #browser()
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            progress$set(message = "[Shiny]Plot.ResCalMatrix, INFO, Plotting matrix plots of calibration with residuals", value = 0.5)
            on.exit(progress$close())
            
            # Opening the Calib TabSet for GUI consistency
            isolate({
                #if (isolate(input$Calib_data) != "Calib" & isolate(input$Calib_data) != "SetTime") updateTabsetPanel(session, inputId = "Calib_data", selected = "Calib")
                updateTabsetPanel(session, inputId = "Calib.Sensors", selected = input$Sensors)
                updateTabsetPanel(session, inputId = "SetTime.Sensors"      , selected = input$Sensors)
            })
            
            # Date range: intersection between the range for calibration and the range for plotting
            DateIN  <- max(c(input[[paste0("DateCal",CalSet()$k)]][1],input[[paste0("DatePlotCal",CalSet()$k)]][1]), na.rm = TRUE)
            DateEND <- min(c(input[[paste0("DateCal",CalSet()$k)]][2],input[[paste0("DatePlotCal",CalSet()$k)]][2]), na.rm = TRUE)
            
            General.df   <- DF$General
            
            op <- par(no.readonly = TRUE)
            # Restoring graphical parameters on exit of function
            on.exit(par(op))
            par(mfrow = c(1,1))
            
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat("[Shiny]Plot.ResCalMatrix, INFO, plotting correlation matrix of calibration residuals data with Covariates\n")
            #
            if (all(is.na(General.df[,INFLUX()[[4]]]))) {
                cat("[Shiny]Plot.ResCalMatrix, ERROR, All sensor time series are empty, not plotting any times series\n")
            } else {
                # Sensor relationships with other variables
                cat(paste0("[shiny, Plot.RawData()] INFO,Plot calibrated sensor data in volt", input$Sensors, " in order to check relationships with other variables\n"))
                Relationships         <- na.omit(colnames(General.df)[colnames(General.df) %in% input[[paste0("Sens",CalSet()$k)]] ]) 
                AddOut                <- which(Relationships %in% c(list.gas.sensors()))
                Relationships[AddOut] <- paste0(Relationships[AddOut])
                # Adding the residuals to Reationships
                Relationships         <- c(Relationships,CalSet()$nameGasMod,"Residuals")
                # Adding the residuals to General.df
                General.df$Residuals <- General.df[,CalSet()$nameGasMod] - General.df[,CalSet()$nameGasRef]
                
                # changing names of variables
                Pattern  <- rbind(c("Out.", ""),c("Ref.", "Reference "),c("ppm",""),c("ppb",""),c("_modelled",paste0(" sensor in ",CalSet()$unit.sensor))
                                  ,c("_volt", paste0(" Sensor in ",CalSet()$Sens.raw.unit)),c("_"," "))
                if (nrow(Pattern) > 0) Labels <- gsub(pattern = Pattern[1,1], replacement = Pattern[1,2], x = Relationships) 
                if (nrow(Pattern) > 1) for (i in 2:nrow(Pattern)) Labels <- gsub(pattern = Pattern[i,1], replacement = Pattern[i,2], x = Labels)  
                
                # in panel smooth() change pch and cex, in panel.cor() change digits and cex.cor, in pairs() change cex.labels to fit the plots
                pairs(subset(General.df[,c("date",Relationships)], date >= DateIN & date <= DateEND)[,Relationships],
                      lower.panel = panel.smooth, 
                      upper.panel = panel.cor,
                      diag.panel  = panel.hist, 
                      labels      = Labels, 
                      main        = paste0("Correlation matrix of residuals of calibration data (R2 in bold) versus Covariates for sensors ", input$Sensors,
                                           " between ", DateIN, 
                                           " and "    , DateEND),
                      cex.labels  = 2) # cex.cor = 1.3
                
                # Saving plot if requested
                if (input$SavePlot) {
                    dev.copy(png,
                             filename = file.path(DisqueFieldtestDir(),"Calibration",
                                                  paste0(CalSet()$Cal,"_Res_pairs_",
                                                         format(min(General.df$date, na.rm = TRUE),"%Y%m%d"),"_",
                                                         format(max(General.df$date, na.rm = TRUE),"%Y%m%d"),".png")), 
                             #units = "cm", 
                             #width = 35.55, 
                             #Fheight = 20
                             res = 300 
                    )
                    dev.off()
                    cat(paste0("[shiny] INFO, ", CalSet()$Cal,"_Res_pairs_",
                               format(min(General.df$date, na.rm = TRUE),"%Y%m%d"),"_",
                               format(max(General.df$date, na.rm = TRUE),"%Y%m%d"),".png saved in ", file.path(DisqueFieldtestDir(),"Calibration"), "\n" ))
                    updateCheckboxInput(session, 
                                        inputId = "SavePlot", 
                                        label = NULL, 
                                        value = FALSE)
                }
                #} 
            }
            cat("-----------------------------------------------------------------------------------\n")
            
            progress$set(message = "[Shiny]Plot.ResCalMatrix, INFO, Plotting matrix plots of calibration with residuals", value = 1)
            
            on.exit(progress$close())
        })
        
        # NavBar"Data Treatment", mainTabPanel "DataTable", 
        React.General.cal <- reactive({
            if (!is.null(DF$General)) selectByDate(DF$General, start = input$Out.Sens.Date1[1], end   = input$Out.Sens.Date1[2])
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
                                   list.gas.sensors(),
                                   paste0("Out.",list.gas.sensors()),
                                   #paste0("Out.",list.gas.sensors(),".1"),
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
        
        # NavBar"Data Treatment", mainTabPanel Calbration - Extrapolation ----  
        if (input$UserMinsAvg == input$UserMins) {
            Init.DF.aggregated <- DF$General 
        } else {
            progress <- shiny::Progress$new()
            on.exit(progress$close())
            progress$set(message = "[shiny] INFO, averaging Summary table of calibration model", value = 0.2)
            
            # only keeping the complete cases by pairs of reference and sensor data
            Init.DF.aggregated <- DF$General
            #InComplete.Index   <- lapply(1:length(list.gas.sensors()), function(i) as.vector(which(!complete.cases(DF$General[,c(paste0("Out.",list.gas.reference2use()[i]),paste0(list.gas.sensors()[i],"_modelled"))]))))
            #for (i in 1:length(list.gas.sensors())) Init.DF.aggregated[InComplete.Index[[i]],c(paste0("Out.",list.gas.reference2use()))] <- NA
            Init.DF.aggregated <- data.frame(timeAverage(mydata     = Init.DF.aggregated, 
                                                         avg.time   = paste0(toString(input$UserMinsAvg)," ","min"), 
                                                         statistic  = "mean", 
                                                         start.date = round(min(DF$General$date, na.rm = TRUE), units = "hours"), 
                                                         end.date   = round(max(DF$General$date, na.rm = TRUE), units = "hours")))
            progress$set(message = "[shiny] INFO, averaging Summary table of calibration model", value = 1.0)
        }
        DF.aggregated <- reactiveValues(Avg = Init.DF.aggregated)
        if (exists("Init.DF.aggregated")) rm(Init.DF.aggregated)
        observeEvent({
            input$UserMinsAvg
            DF$General
        },{
            if (input$UserMinsAvg == input$UserMins) {
                DF.aggregated$Avg <- DF$General
            } else {
                progress <- shiny::Progress$new()
                on.exit(progress$close())
                progress$set(message = "[shiny] INFO, averaging Summary table of calibration model", value = 0.2)
                
                # only keeping the complete cases by pairs of reference and sensor data
                DF.aggregated$Avg <- DF$General
                #InComplete.Index  <- lapply(1:length(list.gas.sensors()), function(i) as.vector(which(!complete.cases(DF$General[,c(paste0("Out.",list.gas.reference2use()[i]),paste0(list.gas.sensors()[i],"_modelled"))]))))
                #for (i in 1:length(list.gas.sensors())) DF.aggregated$Avg[InComplete.Index[[i]],c(paste0("Out.",list.gas.reference2use()))] <- NA
                DF.aggregated$Avg <- data.frame(timeAverage(mydata     = DF.aggregated$Avg, 
                                                            avg.time   = paste0(toString(input$UserMinsAvg)," ","min"), 
                                                            statistic  = "mean", 
                                                            start.date = round(min(DF$General$date, na.rm = TRUE), units = "hours"), 
                                                            end.date   = round(max(DF$General$date, na.rm = TRUE), units = "hours")))
                progress$set(message = "[shiny] INFO, averaging Summary table of calibration model", value = 1.0)
            }
        }, priority = 50)
        output$Extrapolation   <- renderPlot(Plot.Extrapolation()  , width = 'auto', height = 'auto')
        # Reactive FUN Plot.Extrapolation
        Plot.Extrapolation     <- reactive({
            
            # return the linear model of calibration
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "[shiny, Plot.Extrapolation()] INFO, Plotting scatter plot of extrapolated sensor data", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
            # Set Measuring functions, plots GRAPHS AND STATISTICS ON MODELLED quantities
            WDoutput <- file.path(DisqueFieldtestDir(), "Modelled_gas")
            op <- par(no.readonly = TRUE)
            # par(mfrow=c(2,2))
            # Restoring graphical parameters on exit of function
            on.exit(par(op))
            
            # Date range: intersection between the range for extrapolation and the range for plotting
            DateIN  <- max(c(input[[paste0("DateMeas",CalSet()$k)]][1],input[[paste0("DatePlotMeas",CalSet()$k)]][1]), na.rm = TRUE)
            DateEND <- min(c(input[[paste0("DateMeas",CalSet()$k)]][2],input[[paste0("DatePlotMeas",CalSet()$k)]][2]), na.rm = TRUE)
            if (input$UserMinsAvg == input$UserMins) {
                General.df <- subset(DF$General, date >= DateIN & date <= DateEND) 
            } else {
                General.df <- subset(DF.aggregated$Avg, date >= DateIN & date <= DateEND)
            }
            
            # Setting Covariates_i for multi variable model
            if (CalSet()$mod.eta.model.type == "MultiLinear") {
                Covariates      <- input[[paste0("CovMod",CalSet()$k)]]
                
            } else {
                Covariates      <- NULL
            } 
            
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat(paste0("[shiny, Plot.Extrapolation()] INFO, Set Measuring functions for ", input$Calib_data), sep = "\n")
            
            # next in case no data to be calibrated
            if (all(is.na(General.df[, CalSet()$nameGasVolt]))) {
                cat(paste0("[shiny, Plot.Extrapolation()] ERROR, No data during interpolation date for sensor ",CalSet()$name.sensor, "\n"))
                #next
                
            } else {
                cat(paste0("[shiny, Plot.Extrapolation()] INFO, using calibration ",input[[paste0("Cal",CalSet()$k)]], " with ", input[[paste0("Calibration",i.sensors()[CalSet()$k])]], " calibration method\n"))
                
                #loading the image of calibration
                if (CalSet()$remove.neg) negatif <- "_remove.neg_" else negatif <- "_"
                
                # Loading previous model
                Model.i         <- readRDS(file = file.path(CalSet()$WDoutputMod, CalSet()$Cal))
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
                EtalLim <- Etalonnage( x = General.df[,CalSet()$nameGasRef], 
                                       s_x = NULL, 
                                       y = General.df[,CalSet()$nameGasMod], 
                                       s_y = NULL, 
                                       AxisLabelX = Name.pol[1], 
                                       AxisLabelY = Name.pol[2], 
                                       Title = paste0(CalSet()$AirsensEur.name, ": ",": calibrated ", CalSet()$name.sensor," data from ",
                                                      format(DateIN,"%y-%m-%d")," to ",format(DateEND,"%y-%m-%d")), # , " at ",CalSet()$Reference.name
                                       Marker = 1, 
                                       Couleur = "blue", 
                                       ligne = 'p', 
                                       XY_same = TRUE, 
                                       lim = NULL, 
                                       steps = c(10,10), 
                                       digitround = c(1,1), 
                                       marges = c(4,4,3,0.5))
                
                Comparison <- Cal_Line(x             = General.df[,CalSet()$nameGasRef], 
                                       s_x           = NULL, 
                                       y             = General.df[,CalSet()$nameGasMod], 
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
                                       Covariates    = NULL) 
                
                # Saving plot if requested
                if (input$SavePlot) {
                    
                    # adding minutes if DF$General is aggregated
                    if (input$UserMinsAvg == input$UserMins) {
                        File.name <- file.path(WDoutput, paste0(CalSet()$Cal,"_Calibrated_", format(DateIN, "%Y%m%d"),"_", format(DateEND,"%Y%m%d"),".png"))
                    } else File.name <- file.path(WDoutput, paste0(CalSet()$Cal,"_Calibrated_", format(DateIN, "%Y%m%d"),"_", format(DateEND,"%Y%m%d"),"_", input$UserMinsAvg,"mins.png"))
                    
                    dev.copy(png,
                             filename = File.name, 
                             units = "cm", 
                             width = 20, 
                             height = 20,
                             res = 300 
                    )
                    dev.off()
                    cat(paste0("[shiny] INFO, ", CalSet()$Cal,"_Calibrated_",
                               format(DateIN, "%Y%m%d"),"_",
                               format(DateEND,"%Y%m%d"),".png saved in ", WDoutput, "\n" ))
                    updateCheckboxInput(session, 
                                        inputId = "SavePlot", 
                                        label = NULL, 
                                        value = FALSE)
                    for (i in c("formula","terms","model")) {
                        if (any(i %in% names(Comparison))) {
                            rm(list = ls(envir = attr(Comparison[[i]], ".Environment")), envir = attr(Comparison[[i]], ".Environment")) 
                            break
                        }
                    }
                    saveRDS(object = Comparison, 
                            file   = file.path(CalSet()$WDModelled_gas, paste0(CalSet()$Cal, "__",
                                                                               CalSet()$eta.model.type, "__",
                                                                               format(DateIN, "%Y%m%d"),"__",
                                                                               format(DateEND,"%Y%m%d"),"__",
                                                                               ".rds"))
                    )
                }
            }
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            
            # Opening the Calib TabSet for GUI consistency
            isolate({
                #if (isolate(input$Calib_data) != "Calib" & isolate(input$Calib_data) != "SetTime") updateTabsetPanel(session, inputId = "Calib_data", selected = "SetTime")
                updateTabsetPanel(session, inputId = "Calib.Sensors", selected = input$Sensors)
                updateTabsetPanel(session, inputId = "SetTime.Sensors"      , selected = input$Sensors)
            })
            
            progress$set(message = "[shiny, Plot.Extrapolation()] INFO, Plotting scatter plot of extrapolated sensor data", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
            if (exists("Comparison")) return(Comparison) 
        })
        # NavBar"Data Treatment", mainTabPanel SummaryExtra - Extrapolation ----
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
            
            # Date range: intersection between the range for extrapolation and the range for plotting
            DateIN  <- max(c(input[[paste0("DateMeas",CalSet()$k)]][1],input[[paste0("DatePlotMeas",CalSet()$k)]][1]), na.rm = TRUE)
            DateEND <- min(c(input[[paste0("DateMeas",CalSet()$k)]][2],input[[paste0("DatePlotMeas",CalSet()$k)]][2]), na.rm = TRUE)
            
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat(paste0("[shiny, Table.SummaryExtra()] INFO, printing comparison model of extrapolated data vs reference data of sensor ", input$Sensors, sep = "\n"))
            
            # # Comparison model saved in plot.extrapolation
            # if (CalSet()$Neg.mod) Negative <- "Neg_mode_TRUE" else Negative <- "Neg_mode_FALSE"
            # Comparison = file.path(paste0(CalSet()$Cal, "__",
            #                               CalSet()$eta.model.type,"__",
            #                               Negative,"__",
            #                               format(DateIN,"%Y%m%d"),"__",format(DateEND,"%Y%m%d"),
            #                               ".rds"))
            
            # checking that CalSet()$Cal is not empty
            if (CalSet()$Cal != "") return.SummaryExtra <- summary(Plot.Extrapolation()) else  {
                
                cat(paste0("[shiny, Table.SummaryExtra()] ERROR, calibration model is empty\n"))
                return.SummaryExtra <- paste0("[Shiny]Table.SummaryExtra, ERROR, calibration model is empty\n")
            }    
            
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            
            progress$set(message = "[shiny, Table.SummaryExtra()] INFO, Summary table of comparison model", value = 1)
            on.exit(progress$close())
            
            print(return.SummaryExtra)
            
        })
        
        # NavBar"Data Treatment", mainTabPanel TimeSeries - Extrapolation ----  
        output$ExtrapolationTS <- renderPlot(Plot.ExtrapolationTS(), width = 'auto', height = 'auto')
        # Reactive FUN Plot.ExtrapolationTS
        Plot.ExtrapolationTS   <- reactive({
            
            #----------------------------------------------------------CR
            # plotting correlation in time series of validated data with Covariates
            #----------------------------------------------------------CR
            # depends:
            #   input$Sensors,
            #   Config()[[2]]$name.sensor
            #   i.sensors()
            #   DF$General
            #   INFLUX()[[4]]
            #   input[[paste0("Sens",k)]]
            #   input[[paste0("Date",k)]]
            # isolates:
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "[shiny, Plot.ExtrapolationTS()] INFO, Plotting time series of extrapolated sensor data", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
            # Opening the Calib TabSet for GUI consistency
            isolate({
                #if (isolate(input$Calib_data) != "Calib" & isolate(input$Calib_data) != "SetTime") updateTabsetPanel(session, inputId = "Calib_data", selected = "SetTime")
                updateTabsetPanel(session, inputId = "Calib.Sensors", selected = input$Sensors)
                updateTabsetPanel(session, inputId = "SetTime.Sensors"      , selected = input$Sensors)
            })            
            # Date range: intersection between the range for extrapolation and the range for plotting
            DateIN  <- max(c(input[[paste0("DateMeas",CalSet()$k)]][1],input[[paste0("DatePlotMeas",CalSet()$k)]][1]), na.rm = TRUE)
            DateEND <- min(c(input[[paste0("DateMeas",CalSet()$k)]][2],input[[paste0("DatePlotMeas",CalSet()$k)]][2]), na.rm = TRUE)
            if (input$UserMinsAvg == input$UserMins) {
                General.df <- subset(DF$General, date >= DateIN & date <= DateEND) 
            } else {
                General.df <- subset(DF.aggregated$Avg, date >= DateIN & date <= DateEND)
            }
            
            op <- par(no.readonly = TRUE)
            par(mfrow = c(1,1))
            # Restoring graphical parameters on exit of function
            on.exit(par(op))
            
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat("[shiny, Plot.ExtrapolationTS()] INFO, plotting time series of calibrated data\n")
            
            # 
            if (all(is.na(General.df[,c(CalSet()$nameGasRef,CalSet()$nameGasMod)]))) {
                cat("[shiny, Plot.ExtrapolationTS()] ERROR, All data in calibrated time series are empty, not plotting any times series\n")
            } else {
                # Sensor relationships with other variables
                cat(paste0("[shiny, Plot.ExtrapolationTS()] INFO, Plot calibrated sensor data and reference data for Sensor ", input$Sensors, " in order to check relationships with other variables\n"))
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
                timePlot(mydata = subset(General.df, date >= DateIN & date <= DateEND), 
                         pollutant = c(CalSet()$nameGasRef,CalSet()$nameGasMod), 
                         name.pol  = Name.pol, 
                         group     = TRUE, 
                         date.pad  = TRUE, 
                         auto.text = FALSE, 
                         lty       = c(1,1), 
                         col       = c("red", "green"), 
                         ylab      = "",
                         main      = paste0(CalSet()$AirsensEur.name, ": Calibrated ", CalSet()$name.sensor," from ",format(DateIN,"%d-%b-%y")," to ", format(DateEND,"%d-%b-%y")
                                            , " with model ",isolate(CalSet()$NewCalSet)," at ",CalSet()$Reference.name),
                         ref.x     = list(v = c(input[[paste0("DateCal",CalSet()$k)]][1],input[[paste0("DateCal",CalSet()$k)]][2]), lty = c(1, 1), col = c("black", "black"), lwd = c(2,2))
                )
                # Saving plot if requested
                if (input$SavePlot) {
                    
                    # Directory for saving plots
                    WDoutput <- file.path(DisqueFieldtestDir(), "Modelled_gas")
                    
                    # adding minutes if DF$General is aggregated
                    if (input$UserMinsAvg == input$UserMins) {
                        File.name <- file.path(WDoutput, paste0(CalSet()$Cal,"_Calibrated_ts_", format(DateIN, "%Y%m%d"),"_", format(DateEND,"%Y%m%d"),".png"))
                    } else File.name <- file.path(WDoutput, paste0(CalSet()$Cal,"_Calibrated_ts_", format(DateIN, "%Y%m%d"),"_", format(DateEND,"%Y%m%d"),"_", input$UserMinsAvg,"mins.png"))
                    
                    dev.copy(png,
                             filename = File.name, 
                             #units   = "cm", 
                             #width   = 35.55, 
                             #height  = 20,
                             res      = 300 
                    )
                    dev.off()
                    cat(paste0("[shiny] INFO, ", CalSet()$Cal,"_Calibrated_ts_",
                               format(DateIN, "%Y%m%d"),"_",
                               format(DateEND,"%Y%m%d"),".png saved in ", WDoutput, "\n" ))
                    updateCheckboxInput(session, 
                                        inputId = "SavePlot", 
                                        label = NULL, 
                                        value = FALSE)
                }
            }
            
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            
            progress$set(message = "[shiny, Plot.ExtrapolationTS()] INFO, Plotting time series of extrapolated sensor data", value = 1)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
        })
        # NavBar"Data Treatment", mainTabPanel residualMatrix - Extrapolation ---- 
        output$ResExtraMatrix  <- renderPlot(Plot.ResExtraMatrix()   , width = 'auto', height = 'auto')
        # Reactive FUN Plot.ResExtraMatrix
        Plot.ResExtraMatrix    <- reactive({
            # Plotting correalation matrix using pairs()
            # depends:
            #   input$Sensors,
            #   Config()[[2]]$name.sensor
            #   i.sensors()
            #   DF$General
            #   INFLUX()[[4]]
            #   input[[paste0("Sens",k)]]
            #   input[[paste0("Date",k)]]
            # isolates:
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            progress$set(message = "[shiny, Plot.ResExtraMatrix()] INFO, Plotting matrix plots of extrapolation with residuals", value = 0.5)
            on.exit(progress$close())
            
            # Date range: intersection between the range for extrapolation and the range for plotting
            DateIN  <- max(c(input[[paste0("DateMeas",CalSet()$k)]][1],input[[paste0("DatePlotMeas",CalSet()$k)]][1]), na.rm = TRUE)
            DateEND <- min(c(input[[paste0("DateMeas",CalSet()$k)]][2],input[[paste0("DatePlotMeas",CalSet()$k)]][2]), na.rm = TRUE)
            if (input$UserMinsAvg == input$UserMins) {
                General.df <- subset(DF$General, date >= DateIN & date <= DateEND) 
            } else {
                General.df <- subset(DF.aggregated$Avg, date >= DateIN & date <= DateEND)
            }
            
            op <- par(no.readonly = TRUE)
            # Restoring graphical parameters on exit of function
            on.exit(par(op))
            par(mfrow = c(1,1))
            
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat("[shiny, Plot.ResExtraMatrix()] INFO, plotting correlation matrix of calibration residuals data with Covariates\n")
            #
            if (all(is.na(General.df[,INFLUX()[[4]]]))) {
                cat("[shiny, Plot.ResExtraMatrix()] ERROR, All sensor time series are empty, not plotting any times series\n")
            } else {
                # Sensor relationships with other variables
                cat(paste0("[shiny, Plot.ResExtraMatrix()] INFO, Plot sensor data in volt with Covariates for Sensor ", input$Sensors, " in order to check relationships with other variables\n"))
                Relationships         <- na.omit(colnames(General.df)[colnames(General.df) %in% input[[paste0("Sens",CalSet()$k)]] ]) 
                AddOut                <- which(Relationships %in% c(list.gas.sensors()))
                Relationships[AddOut] <- paste0(Relationships[AddOut])
                # Adding the residuals to Reationships
                Relationships         <- c(Relationships,CalSet()$nameGasMod,"Residuals")
                # Adding the residuals to General.df
                General.df$Residuals <- General.df[,CalSet()$nameGasMod] - General.df[,CalSet()$nameGasRef]
                
                # changing names of variables
                Pattern  <- rbind(c("Out.", ""),c("Ref.", "Reference "),c("ppm",""),c("ppb",""),c("_modelled",paste0(" sensor in ",CalSet()$unit.sensor))
                                  ,c("_volt", paste0(" Sensor in ",CalSet()$Sens.raw.unit)),c("_"," "))
                if (nrow(Pattern) > 0) Labels <- gsub(pattern = Pattern[1,1], replacement = Pattern[1,2], x = Relationships) 
                if (nrow(Pattern) > 1) for (i in 2:nrow(Pattern)) Labels <- gsub(pattern = Pattern[i,1], replacement = Pattern[i,2], x = Labels)  
                
                # in panel smooth() change pch and cex, in panel.cor() change digits and cex.cor, in pairs() change cex.labels to fit the plots
                pairs(subset(General.df[,c("date",Relationships)], date >= DateIN & date <= DateEND)[,Relationships],
                      lower.panel = panel.smooth, 
                      upper.panel = panel.cor,
                      diag.panel  = panel.hist, 
                      labels = Labels, 
                      main = paste0("Correlation matrix of residuals of calibrated data (R2 in bold) versus Covariates for sensor ", input$Sensors,
                                    " between ", DateIN, 
                                    " and "    , DateEND), 
                      cex.labels = 2) # cex.cor = 1.3
                
                # Saving plot if requested
                if (input$SavePlot) {
                    
                    # Directory for saving plots
                    WDoutput <- file.path(DisqueFieldtestDir(), "Modelled_gas")
                    
                    # adding minutes if DF$General is aggregated
                    if (input$UserMinsAvg == input$UserMins) {
                        File.name <- file.path(WDoutput, paste0(CalSet()$Cal,"_Res_pairs_", format(DateIN, "%Y%m%d"),"_", format(DateEND,"%Y%m%d"),".png"))
                    } else File.name <- file.path(WDoutput, paste0(CalSet()$Cal,"_Res_pairs_", format(DateIN, "%Y%m%d"),"_", format(DateEND,"%Y%m%d"),"_", input$UserMinsAvg,"mins.png"))
                    
                    dev.copy(png,
                             filename = File.name, 
                             #units   = "cm", 
                             #width   = 35.55, 
                             #height  = 20,
                             res      = 300 
                    )
                    dev.off()
                    cat(paste0("[shiny] INFO, ", CalSet()$Cal,"_Res_pairs_",
                               format(min(General.df$date, na.rm = TRUE),"%Y%m%d"),"_",
                               format(max(General.df$date, na.rm = TRUE),"%Y%m%d"),".png saved in ", WDoutput, "\n" ))
                    updateCheckboxInput(session, 
                                        inputId = "SavePlot", 
                                        label = NULL, 
                                        value = FALSE)
                }
                #} 
            }
            
            # Opening the Calib TabSet for GUI consistency
            isolate({
                #if (isolate(input$Calib_data) != "Calib" & isolate(input$Calib_data) != "SetTime") updateTabsetPanel(session, inputId = "Calib_data", selected = "Calib")
                updateTabsetPanel(session, inputId = "Calib.Sensors", selected = input$Sensors)
                updateTabsetPanel(session, inputId = "SetTime.Sensors"      , selected = input$Sensors)
            })
            
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            
            progress$set(message = "[shiny, Plot.ResExtraMatrix()] INFO, Plotting matrix plots of extrapolation with residuals", value = 1)
            on.exit(progress$close())
            
        })
        
        # NavBar"Data Treatment", mainTabPanel Uncertainty - Extrapolation  ----
        output$U_Table      <- renderTable(
            Plot.Uncertainty()[[1]],
            rownames = TRUE, 
            width = 'auto'
        )
        output$Uncertainty  <- renderImage({
            
            # depends on changes of input$uxi and DF.aggregated$Avg
            input$uxi1
            input$uxi2
            input$uxi3
            input$uxi4
            DF.aggregated$Avg
            
            # Directory for saving plots
            WDoutput <- file.path(DisqueFieldtestDir(), "Modelled_gas")
            
            # Return a list containing the filename
            list(src = file.path(WDoutput,paste0(CalSet()$Cal, "_U.png" )),
                 contentType = 'image/png',
                 width = 450,
                 height = 450,
                 alt = "Measurement Uncertainty")
        }, deleteFile = FALSE)
        output$SqrRes  <- renderImage({
            
            # Directory for saving plots
            WDoutput <- file.path(DisqueFieldtestDir(), "Modelled_gas")
            
            # Return a list containing the filename
            list(src = file.path(WDoutput,paste0(CalSet()$Cal,"_SqrRes.png" )),
                 contentType = 'image/png',
                 width = 450,
                 height = 450,
                 alt = "Square of residuals")
        }, deleteFile = FALSE)
        output$Scatter  <- renderImage({
            
            # depends on changes of input$uxi and DF.aggregated$Avg
            input$uxi1
            input$uxi2
            input$uxi3
            input$uxi4
            DF.aggregated$Avg
            
            # Directory for saving plots
            WDoutput <- file.path(DisqueFieldtestDir(), "Modelled_gas")
            
            # Return a list containing the filename
            list(src = file.path(WDoutput,paste0(CalSet()$Cal,"_Scatter.png" )),
                 contentType = 'image/png',
                 width = 450,
                 height = 450,
                 alt = "Scatter plot")
        }, deleteFile = FALSE)
        
        
        # Reactive FUN U.orth.List
        U.orth.List <- reactive({
            
            #----------------------------------------------------------CR
            # Returning paraneters of the measurement uncertainty of the selected sensor using the method of the GDE
            #----------------------------------------------------------CR
            # depends:
            #   input$Sensors,
            #   DF$General
            #   input[[paste0("DateMeas"]]
            #   input[[paste0("DatePlotMeas"]]
            # isolates:
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            progress$set(message = "Calculating  measurement uncertainty of the selected sensor using the method of the GDE", value = 0.5)
            on.exit(progress$close())
            
            # Date range: intersection between the range for extrapolation and the range for plotting
            DateIN  <- max(c(input[[paste0("DateMeas",CalSet()$k)]][1],input[[paste0("DatePlotMeas",CalSet()$k)]][1]), na.rm = TRUE)
            DateEND <- min(c(input[[paste0("DateMeas",CalSet()$k)]][2],input[[paste0("DatePlotMeas",CalSet()$k)]][2]), na.rm = TRUE)
            if (input$UserMinsAvg == input$UserMins) {
                General.df <- subset(DF$General, date >= DateIN & date <= DateEND) 
            } else {
                General.df <- subset(DF.aggregated$Avg, date >= DateIN & date <= DateEND)
            }
            
            cat("-----------------------------------------------------------------------------------\n")
            
            if (all(is.na(General.df[,c(CalSet()$nameGasRef,CalSet()$nameGasMod)]))) {
                cat("[Shiny]U.orth.List, ERROR, All data in calibrated time series are empty, not plotting any times series\n")
            } else {
                
                # Sensor relationships with other variables
                cat(paste0("[Shiny]U.orth.List, INFO, Calculating measurement uncertainty of calibrated data for Sensor ", input$Sensors, "\n"))
                
                U.orth.List <- U.orth.DF(
                    Mat          = cbind(1:nrow(General.df), General.df[c("date", CalSet()$nameGasRef, CalSet()$nameGasMod)], rep(CalSet()$uxi,times = nrow(General.df))),
                    uxi          = as.numeric(CalSet()$uxi), 
                    variable.uxi = FALSE
                ) 
            }
            
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            
            progress$set(message = "Calculating the measurement uncertainty of the selected sensor using the method of the GDE", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
            return(U.orth.List)
            
        })
        
        # Reactive FUN Plot.Uncertainty
        Plot.Uncertainty    <- reactive({
            
            #----------------------------------------------------------CR
            # plotting correlation in time series of validated data with Covariates
            #----------------------------------------------------------CR
            # depends:
            #   input$Sensors,
            #   Config()[[2]]$name.sensor
            #   i.sensors()
            #   DF$General
            #   input[[paste0("Sens",k)]]
            #   input[[paste0("Date",k)]]
            # isolates:
            
            # depends on changes of input$uxi and DF.aggregated$Avg
            input$uxi1
            input$uxi2
            input$uxi3
            input$uxi4
            DF.aggregated$Avg
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            progress$set(message = "[shiny, Plot.Uncertainty()] INFO, Plotting the measurement uncertainty of the selected sensor using the method of the GDE", value = 0.5)
            on.exit(progress$close())
            
            # Date range: intersection between the range for extrapolation and the range for plotting
            DateIN  <- max(c(input[[paste0("DateMeas",CalSet()$k)]][1],input[[paste0("DatePlotMeas",CalSet()$k)]][1]), na.rm = TRUE)
            DateEND <- min(c(input[[paste0("DateMeas",CalSet()$k)]][2],input[[paste0("DatePlotMeas",CalSet()$k)]][2]), na.rm = TRUE)
            if (input$UserMinsAvg == input$UserMins) {
                General.df <- subset(DF$General, date >= DateIN & date <= DateEND) 
            } else {
                General.df <- subset(DF.aggregated$Avg, date >= DateIN & date <= DateEND)
            }
            
            # Directory for saving plots
            WDoutput <- file.path(DisqueFieldtestDir(), "Modelled_gas")
            
            op <- par(no.readonly = TRUE)
            par(mfrow = c(1,1))
            # Restoring graphical parameters on exit of function
            on.exit(par(op))
            
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat("[shiny, Plot.Uncertainty()] INFO, plotting measurement uncertainty of calibrated data\n")
            
            # 
            if (all(is.na(General.df[,c(CalSet()$nameGasRef,CalSet()$nameGasMod)]))) {
                cat("[shiny, Plot.Uncertainty()] ERROR, All data in calibrated time series are empty, not plotting any times series\n")
            } else {
                
                # Sensor relationships with other variables
                cat(paste0("[Shiny]Plot.Uncertainty, INFO, plotting measurement uncertainty of calibrated data for Sensor ", input$Sensors, "\n"))
                
                calib <- slope_orth(
                    Xlabel = paste0(CalSet()$i, " in ", CalSet()$unit.ref), 
                    Ylabel = paste0("Measurement uncertainty in ", CalSet()$unit.sensor),
                    Title  = paste0("Measurement uncertainty of sensor ",input$Sensors," between ",
                                    format(DateIN, "%y%m%d")," and ", format(DateEND,"%y%m%d")), 
                    DQO.I    = CalSet()$DQO.I, 
                    LV     = CalSet()$LV, 
                    Units  = CalSet()$unit.sensor, 
                    Dir    = WDoutput, 
                    Mat    = cbind(1:nrow(General.df), General.df[c("date", CalSet()$nameGasRef,CalSet()$nameGasMod)]),
                    uxi    = as.numeric(CalSet()$uxi), 
                    lim    = NULL, 
                    Sensor_name  = CalSet()$name.sensor, 
                    variable.uxi = FALSE, 
                    f_coef1 = "%.3f", 
                    f_coef2 = "%.3f", 
                    f_R2    = "%.4f",
                    nameModel = CalSet()$Cal,
                    SavePlot  = TRUE
                ) 
                
                # Saving plot if requested
                if (input$SavePlot) {
                    
                    # adding minutes if DF$General is aggregated
                    if (input$UserMinsAvg == input$UserMins) {
                        File.name <- file.path(WDoutput, paste0(CalSet()$Cal,"_Uncertainty_", format(DateIN, "%Y%m%d"),"_", format(DateEND,"%Y%m%d"),".png"))
                    } else File.name <- file.path(WDoutput, paste0(CalSet()$Cal,"_Uncertainty_", format(DateIN, "%Y%m%d"),"_", format(DateEND,"%Y%m%d"),"_", input$UserMinsAvg,"mins.png"))
                    
                    dev.copy(png,
                             filename = File.name, 
                             units    = "cm", 
                             width    = 38, 
                             height   = 38,
                             res      = 300 
                    )
                    dev.off()
                    cat(paste0("[shiny] INFO, ", CalSet()$Cal,"_Uncertainty_", format(DateIN, "%Y%m%d"),"_", format(DateEND,"%Y%m%d"),".png saved in ", WDoutput, "\n" ))
                    updateCheckboxInput(session, 
                                        inputId = "SavePlot", 
                                        label   = NULL, 
                                        value   = FALSE)
                }
            }
            
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            
            progress$set(message = "[shiny, Plot.Uncertainty()] INFO, Plotting the measurement uncertainty of the selected sensor using the method of the GDE", value = 1)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
            # Opening the SetTime TabSet for GUI consistency
            isolate({
                #if (isolate(input$Calib_data) != "Calib" & isolate(input$Calib_data) != "SetTime") updateTabsetPanel(session, inputId = "Calib_data", selected = "SetTime")
                updateTabsetPanel(session, inputId = "Calib.Sensors", selected = input$Sensors)
                updateTabsetPanel(session, inputId = "SetTime.Sensors"      , selected = input$Sensors)
            })
            
            return(
                list(data.frame(
                    values = sapply(calib[which(names(calib) %in% names(calib)[-which(names(calib) == "Mat")])],function(x) x),
                    # units of parameters:
                    # c("mo","sdo", "mm","sdm", "b1", "ub1", "b0", "ub0", "RSS","rmse", "mbe", "Correlation", "nb", "CRMSE", "NMSD", "Mat")
                    units  = c(paste0(CalSet()$unit.sensor),
                               paste0(CalSet()$unit.sensor),
                               paste0(CalSet()$unit.sensor),
                               paste0(CalSet()$unit.sensor),
                               paste0(CalSet()$unit.sensor,"/", CalSet()$unit.ref),
                               paste0(CalSet()$unit.sensor,"/", CalSet()$unit.ref),
                               paste0(CalSet()$unit.sensor),
                               paste0(CalSet()$unit.sensor),
                               paste0(CalSet()$unit.sensor,"^2"),
                               paste0(CalSet()$unit.sensor,"^2"),
                               paste0(CalSet()$unit.sensor),
                               paste0(""),
                               paste0(""),
                               paste0(CalSet()$unit.sensor,"^2"),
                               paste0(CalSet()$unit.sensor,"^2")
                    ),
                    
                    stringsAsFactors = TRUE
                ),
                calib[["Mat"]])
            )
        })
        
        # NavBar"Data Treatment", mainTabPanel Drift - Extrapolation ----  
        Drift.df <- reactive({
            
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
            
            # Date range: intersection between the range for extrapolation and the range for plotting
            DateIN  <- max(c(input[[paste0("DateMeas",CalSet()$k)]][1],input[[paste0("DatePlotMeas",CalSet()$k)]][1]), na.rm = TRUE)
            DateEND <- min(c(input[[paste0("DateMeas",CalSet()$k)]][2],input[[paste0("DatePlotMeas",CalSet()$k)]][2]), na.rm = TRUE)
            General.df <- data.frame(timeAverage(subset(isolate(DF$General[c("date", CalSet()$nameGasRef, CalSet()$nameGasMod)]), 
                                                        date >= DateIN & date <= DateEND),
                                                 avg.time = "day", 
                                                 statistic = "mean",
                                                 data.thresh = 0
            ))
            # Calculating the long-term relative drift
            General.df$rel.drift <- as.vector((General.df[,CalSet()$nameGasMod] - General.df[,CalSet()$nameGasRef])*100/General.df[,CalSet()$nameGasRef])
            General.df$drift     <- as.vector((General.df[,CalSet()$nameGasMod] - General.df[,CalSet()$nameGasRef]))
            # Removing rows with NaN
            General.df <- General.df[complete.cases(General.df),]
            # Adding duration
            General.df[, "duration"] <- as.numeric(difftime(General.df[,"date"],  General.df[1,"date"], units = "days"))
            # Adding dose
            General.df[2:nrow(General.df), "days"] <- as.numeric(difftime(General.df[2:nrow(General.df),"date"], General.df[1:(nrow(General.df) - 1),"date"], units = "days"))
            General.df[1, "days"] <- 0
            General.df$day.dose   <- General.df$days * General.df[,CalSet()$nameGasRef]
            General.df$add.dose   <- cumsum(General.df$day.dose)
            
            return(General.df)
        })
        output$Drift  <- renderPlot(plot.drift()   , width = 'auto', height = 'auto')
        plot.drift <- reactive({
            #----------------------------------------------------------CR
            # plotting drift in time series of calibrated sensor data
            #----------------------------------------------------------CR
            # depends:
            #   input$Sensors,
            #   Config()[[2]]$name.sensor
            #   i.sensors()
            #   DF$General
            #   input[[paste0("Sens",k)]]
            #   input[[paste0("Date",k)]]
            # isolates:
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "[shiny, plot.drift()] INFO, Plotting long term drift of calibratedd sensor data", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
            # Date range: intersection between the range for extrapolation and the range for plotting
            DateIN  <- max(c(input[[paste0("DateMeas",CalSet()$k)]][1],input[[paste0("DatePlotMeas",CalSet()$k)]][1]), na.rm = TRUE)
            DateEND <- min(c(input[[paste0("DateMeas",CalSet()$k)]][2],input[[paste0("DatePlotMeas",CalSet()$k)]][2]), na.rm = TRUE)
            
            # Directory for saving plots
            WDoutput <- file.path(DisqueFieldtestDir(), "Drift")
            
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
                     xlab = "Number of days from 1st data transfer or selected date for plotting extrapolation",
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
                    dev.copy(png,
                             filename = file.path(WDoutput, 
                                                  paste0(CalSet()$Cal,"_Drift_ts_",
                                                         format(DateIN, "%Y%m%d"),"_",
                                                         format(DateEND,"%Y%m%d"),".png")), 
                             #units = "cm", 
                             #width = 35.55, 
                             #height = 20,
                             res = 300 
                    )
                    dev.off()
                    cat(paste0("[shiny] INFO, ", CalSet()$Cal,"_Drift_ts_",
                               format(DateIN, "%Y%m%d"),"_",
                               format(DateEND,"%Y%m%d"),".png saved in ", WDoutput, "\n" ))
                    updateCheckboxInput(session, 
                                        inputId = "SavePlot", 
                                        label = NULL, 
                                        value = FALSE)
                }
            }
            
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            
            # Opening the Calib TabSet for GUI consistency
            isolate({
                #if (isolate(input$Calib_data) != "Calib" & isolate(input$Calib_data) != "SetTime") updateTabsetPanel(session, inputId = "Calib_data", selected = "SetTime")
                updateTabsetPanel(session, inputId = "Calib.Sensors", selected = input$Sensors)
                updateTabsetPanel(session, inputId = "SetTime.Sensors"      , selected = input$Sensors)
            })
            
            progress$set(message = "[shiny, plot.drift()] INFO, Plotting time series of extrapolated sensor datPlotting long term drift of calibratedd sensor data", value = 1)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
        })
        
        # NavBar"Data Treatment", mainTabPanel Relative drift - Extrapolation ----  
        output$Rel.Drift  <- renderPlot(plot.rel.drift()   , width = 'auto', height = 'auto')
        plot.rel.drift <- reactive({
            #----------------------------------------------------------CR
            # plotting relative drift in time series of calibrated sensor data
            #----------------------------------------------------------CR
            # depends:
            #   input$Sensors,
            #   i.sensors()
            #   DF$General
            # isolates:
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "Plotting long term relative drift of calibratedd sensor data", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
            # Date range: intersection between the range for extrapolation and the range for plotting
            DateIN  <- max(c(input[[paste0("DateMeas",CalSet()$k)]][1],input[[paste0("DatePlotMeas",CalSet()$k)]][1]), na.rm = TRUE)
            DateEND <- min(c(input[[paste0("DateMeas",CalSet()$k)]][2],input[[paste0("DatePlotMeas",CalSet()$k)]][2]), na.rm = TRUE)
            
            # Directory for saving plots
            WDoutput <- file.path(DisqueFieldtestDir(), "Drift")
            
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
                     xlab = "Number of days from 1st data transfer or selected date for plotting extrapolation",
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
                    dev.copy(png,
                             filename = file.path(WDoutput, 
                                                  paste0(CalSet()$Cal,"_Rel.Drift_ts_",
                                                         format(DateIN, "%Y%m%d"),"_",
                                                         format(DateEND,"%Y%m%d"),".png")), 
                             #units = "cm", 
                             #width = 35.55, 
                             #height = 20,
                             res = 300
                    )
                    dev.off()
                    cat(paste0("[shiny] INFO, ", CalSet()$Cal,"_Rel.Drift_ts_",
                               format(DateIN, "%Y%m%d"),"_",
                               format(DateEND,"%Y%m%d"),".png saved in ", WDoutput, "\n" ))
                    updateCheckboxInput(session, 
                                        inputId = "SavePlot", 
                                        label = NULL, 
                                        value = FALSE)
                }
            }
            
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            
            # Opening the Calib TabSet for GUI consistency
            isolate({
                #if (isolate(input$Calib_data) != "Calib" & isolate(input$Calib_data) != "SetTime") updateTabsetPanel(session, inputId = "Calib_data", selected = "SetTime")
                updateTabsetPanel(session, inputId = "Calib.Sensors", selected = input$Sensors)
                updateTabsetPanel(session, inputId = "SetTime.Sensors"      , selected = input$Sensors)
            })
            
            progress$set(message = "Plotting long term relative drift of calibratedd sensor data", value = 1)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
        })
        
        tabPanel("Relative Drift vs dose" , icon = icon("line-chart", lib = "font-awesome"), plotOutput("Rel.Dose.Drift"))
        # NavBar"Data Treatment", mainTabPanel Absolute Drift vs dose - Extrapolation ----  
        output$Dose.Drift  <- renderPlot(plot.Dose.Drift()   , width = 'auto', height = 'auto')
        plot.Dose.Drift <- reactive({
            
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
            #   DisqueFieldtestDir()
            #   Drift.df() ( Drift.df()$add.dose, y = Drift.df()$drift )
            #   input$Sensors
            #   CalSet()$Cal
            # isolates:
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "Plotting Absolute Drift vs dose of calibrated sensor data", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
            # Date range: intersection between the range for extrapolation and the range for plotting
            DateIN  <- max(c(input[[paste0("DateMeas",CalSet()$k)]][1],input[[paste0("DatePlotMeas",CalSet()$k)]][1]), na.rm = TRUE)
            DateEND <- min(c(input[[paste0("DateMeas",CalSet()$k)]][2],input[[paste0("DatePlotMeas",CalSet()$k)]][2]), na.rm = TRUE)
            
            # Directory for saving plots
            WDoutput <- file.path(DisqueFieldtestDir(), "Drift")
            
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
                     xlab = paste0("Dose in ", CalSet()$unit.ref, ".days from 1st data transfer or selected date for plotting extrapolation"),
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
                    dev.copy(png,
                             filename = file.path(WDoutput, 
                                                  paste0(CalSet()$Cal,"_Dose_",
                                                         format(DateIN, "%Y%m%d"),"_",
                                                         format(DateEND,"%Y%m%d"),".png")), 
                             #units = "cm", 
                             #width = 20, 
                             #height = 20,
                             res = 300 
                    )
                    dev.off()
                    cat(paste0("[shiny] INFO, ", CalSet()$Cal,"_Dose_",
                               format(DateIN, "%Y%m%d"),"_",
                               format(DateEND,"%Y%m%d"),".png saved in ", WDoutput, "\n" ))
                    updateCheckboxInput(session, 
                                        inputId = "SavePlot", 
                                        label = NULL, 
                                        value = FALSE)
                }
            }
            
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            
            # Opening the Calib TabSet for GUI consistency
            isolate({
                #if (isolate(input$Calib_data) != "Calib" & isolate(input$Calib_data) != "SetTime") updateTabsetPanel(session, inputId = "Calib_data", selected = "SetTime")
                updateTabsetPanel(session, inputId = "Calib.Sensors", selected = input$Sensors)
                updateTabsetPanel(session, inputId = "SetTime.Sensors"      , selected = input$Sensors)
            }
            )
            
            progress$set(message = "Plotting Absolute Drift vs dose of calibrated sensor data", value = 1)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
        })
        
        # NavBar"Data Treatment", mainTabPanel Relative Drift vs dose - Extrapolation ----  
        output$Rel.Dose.Drift  <- renderPlot(plot.Rel.Dose.Drift()   , width = 'auto', height = 'auto')
        plot.Rel.Dose.Drift <- reactive({
            
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
            #   DisqueFieldtestDir()
            #   Drift.df() ( Drift.df()$add.dose, y = Drift.df()$drift )
            #   input$Sensors
            #   CalSet()$Cal
            # isolates:
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "Plotting Relative Drift vs dose of calibrated sensor data", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
            # Date range: intersection between the range for extrapolation and the range for plotting
            DateIN  <- max(c(input[[paste0("DateMeas",CalSet()$k)]][1],input[[paste0("DatePlotMeas",CalSet()$k)]][1]), na.rm = TRUE)
            DateEND <- min(c(input[[paste0("DateMeas",CalSet()$k)]][2],input[[paste0("DatePlotMeas",CalSet()$k)]][2]), na.rm = TRUE)
            
            # Directory for saving plots
            WDoutput <- file.path(DisqueFieldtestDir(), "Drift")
            
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
                     xlab = paste0("Dose in ", CalSet()$unit.ref, ".days from 1st data transfer or selected date for plotting extrapolation"),
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
                    dev.copy(png,
                             filename = file.path(WDoutput, 
                                                  paste0(CalSet()$Cal,"_Rel.Dose_",
                                                         format(DateIN, "%Y%m%d"),"_",
                                                         format(DateEND,"%Y%m%d"),".png")), 
                             #units = "cm", 
                             #width = 20, 
                             #height = 20,
                             res = 300 
                    )
                    dev.off()
                    cat(paste0("[shiny] INFO, ", CalSet()$Cal,"_Rel.Dose_",
                               format(DateIN, "%Y%m%d"),"_",
                               format(DateEND,"%Y%m%d"),".png saved in ", WDoutput, "\n" ))
                    updateCheckboxInput(session, 
                                        inputId = "SavePlot", 
                                        label = NULL, 
                                        value = FALSE)
                }
            }
            
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
            
            # Opening the Calib TabSet for GUI consistency
            isolate({
                if (isolate(input$Calib_data) != "Calib" & isolate(input$Calib_data) != "SetTime") updateTabsetPanel(session, inputId = "Calib_data", selected = "SetTime")
                updateTabsetPanel(session, inputId = "Calib.Sensors", selected = input$Sensors)
                updateTabsetPanel(session, inputId = "SetTime.Sensors"      , selected = input$Sensors)
            })
            
            progress$set(message = "Plotting Relative Drift vs dose of calibrated sensor data", value = 1)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
        })
        
        # NavBar"Data Treatment", mainTabPanel Target - Extrapolation ----  
        Plot.Target.path <- reactive({
            # Return: WDoutput:                 Directory and file for saving plot
            
            WDoutput <- file.path(DisqueFieldtestDir(), "Modelled_gas")
            return(WDoutput)
        }) 
        Plot.Target.File <- function(Type = "pdf") {
            # return:       u.Target.File the name of the plot with path
            
            # Directory for saving plots
            WDoutput <- Plot.Target.path()
            
            # Date range: intersection between the range for extrapolation and the range for plotting
            DateIN  <- max(c(input[[paste0("DateMeas",CalSet()$k)]][1],input[[paste0("DatePlotMeas",CalSet()$k)]][1]), na.rm = TRUE)
            DateEND <- min(c(input[[paste0("DateMeas",CalSet()$k)]][2],input[[paste0("DatePlotMeas",CalSet()$k)]][2]), na.rm = TRUE)
            
            u.Target.File <- file.path(WDoutput, paste0(CalSet()$Cal,"_UTD_", format(DateIN,"%Y%m%d"),"_", format(DateEND,"%Y%m%d"),".",Type))
            return(u.Target.File)
        } 
        
        output$Target  <- renderText({
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "Plotting Target Diagram", value = 0.5)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            
            # name of plot file 
            u.Target.File <- Plot.Target.File(Type = "pdf")
            
            # checking file type 
            Type <- file_ext(u.Target.File)
            
            # Generate svg
            if (input$SavePlot | !file.exists(u.Target.File)) {
                
                unlink(u.Target.File, force = T)
                
                if (Type == "pdf") pdf(file = file.path(u.Target.File))
                if (Type == "svg") svg(filename = u.Target.File,
                                       width = 7, height = 7, pointsize = 12,
                                       onefile = TRUE, family = "sans", bg = "white")
                plot.Target()
                dev.off()
                updateCheckboxInput(session, 
                                    inputId = "SavePlot", 
                                    label = NULL, 
                                    value = FALSE)
            } 
            
            # Return the filename with path
            addResourcePath("TargetDiag", normalizePath(dirname(u.Target.File), winslash = "/"))
            width  = 900
            height = 900
            SRC    = paste0("TargetDiag/", basename(u.Target.File))
            
            # Opening the Calib TabSet for GUI consistency
            isolate({
                # if (isolate(input$Calib_data) != "Calib" & isolate(input$Calib_data) != "SetTime") updateTabsetPanel(session, inputId = "Calib_data", selected = "SetTime")
                updateTabsetPanel(session, inputId = "Calib.Sensors"  , selected = input$Sensors)
                updateTabsetPanel(session, inputId = "SetTime.Sensors", selected = input$Sensors)
            })
            
            progress$set(message = "Plotting Target Diagram", value = 1)
            # Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            return(paste0('<iframe style = "height:',height,'px; width:100%" src="',SRC,'"></iframe>'))
        })
        
        plot.Target    <- reactive({
            
            #----------------------------------------------------------CR
            # Plotting Target diagram
            #----------------------------------------------------------CR
            # depends:
            #   input$DateMeas
            #   input$DatePlotMeas
            #   DF$General
            #   DisqueFieldtestDir()
            # isolates:
            
            # Date range: intersection between the range for extrapolation and the range for plotting
            DateIN  <- max(c(input[[paste0("DateMeas",CalSet()$k)]][1],input[[paste0("DatePlotMeas",CalSet()$k)]][1]), na.rm = TRUE)
            DateEND <- min(c(input[[paste0("DateMeas",CalSet()$k)]][2],input[[paste0("DatePlotMeas",CalSet()$k)]][2]), na.rm = TRUE)
            General.df <- subset(DF$General, date >= DateIN & date <= DateEND)
            
            op <- par(no.readonly = TRUE)
            par(mfrow = c(1,1))
            # Restoring graphical parameters on exit of function
            on.exit(par(op))
            
            cat("\n")
            cat("-----------------------------------------------------------------------------------\n")
            cat("[Shiny]plot.Target, INFO, plotting Target diagram\n")
            
            # Plotting Modified Target Diagram
            Target.Diagram(Sensor_name = CalSet()$name.sensor, 
                           Mat         = U.orth.List()[["Mat"]], 
                           uxi         = as.numeric(CalSet()$uxi), 
                           Unit.Ref    = CalSet()$unit.ref,
                           b0          = U.orth.List()[["b0"]],
                           b1          = U.orth.List()[["b1"]],
                           xAxisLabel  = NULL, 
                           yAxisLabel  = NULL, 
                           DQO.I       = CalSet()$DQO.I / CalSet()$LV,
                           DQO.M       = CalSet()$DQO.M / CalSet()$LV,
                           DQO.O       = CalSet()$DQO.O / CalSet()$LV,
                           LAT         = CalSet()$LAT, 
                           UAT         = CalSet()$UAT, 
                           LV          = CalSet()$LV, 
                           AT          = CalSet()$AT,
                           sdm_sdo     = U.orth.List()[["sdm"]] > U.orth.List()[["sdo"]]
            )
            
            cat("-----------------------------------------------------------------------------------\n")
            cat("\n")
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
            # Saving file *.cfg (df sens2ref)
            progress$set(message = "[shiny, Save()] INFO,Saving config file Ase_name.cfg", value = 0.2)
            
            # Saving file *.cfg (df sens2ref)
            Outliers_Ref.added   <- c(which(colnames(Outliers_Ref()) == "name.gas"),which(!(names(Outliers_Ref()) %in% intersect(names(Outliers_Sensor()), names(Outliers_Ref())))))
            sens2ref             <- merge(x = Outliers_Sensor(), y = Outliers_Ref()[,Outliers_Ref.added], by = "name.gas", all.x = TRUE, sort = FALSE)
            Shield.added         <- c(which(colnames(Shield()) == "name.sensor"),which(!(names(Shield()) %in% intersect(names(sens2ref), names(Shield())))))
            sens2ref             <- merge(x = sens2ref, y = Shield()[,Shield.added], by = "name.sensor", all.x = TRUE, sort = FALSE)
            Calib_data.added     <- c(which(names(Calib_data()) == "name.gas"),which(!(names(Calib_data()) %in% intersect(names(sens2ref), names(Calib_data())))))
            sens2ref             <- merge(x = sens2ref, y = Calib_data()[,Calib_data.added], by = "name.gas", all.x = TRUE, sort = FALSE)
            sens2ref             <- sens2ref[,unique(names(sens2ref))]
            row.names(sens2ref)  <- sens2ref[,"name.gas"] 
            sens2ref             <- as.data.frame(t(sens2ref), stringsAsFactors = FALSE)
            write.table(sens2ref, file = cfg_file(),col.names = TRUE)
            #save(sens2ref, file         = file.path(DisqueFieldtestDir(),"General_data",paste0("ASE_name(),"_cfg.Rdata")))
            cat(paste0("[shiny, Save()] INFO,", ASE_name(),".cfg config file saved in directory General_data.\n"))
            
            # Saving ASE_name_Servers.cfg file
            progress$set(message = "[shiny, Save()] INFO,Saving ASE_name_Servers.cfg", value = 0.2)
            
            # Saving file *_Servers.R (df cfg)
            # in case Down.SOS is FALSE then input$airSensEUR is ""
            
            # replacing NULL varialbes with "" to be saved as string
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
                              AirsensEur.name = CalSet()$AirsensEur.name, 
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
                              coord.ref      = input$coord.ref, 
                              alt.ref        = input$alt.ref, 
                              ref.tzone      = input$ref.tzone, 
                              
                              asc.File       = input$asc.File, 
                              UserMins       = as.numeric(input$UserMins), 
                              UserMinsAvg    = as.numeric(input$UserMinsAvg), 
                              Delay          = as.numeric(input$Delay),
                              
                              stringsAsFactors = FALSE)
            write.table(t(cfg), file = file.path(DisqueFieldtestDir(),"General_data",paste0(ASE_name(),"_Servers.cfg")), col.names = FALSE)
            #save(cfg, file = file.path(DisqueFieldtestDir,"General_data",paste0(ASE_name,"_Servers_cfg.Rdata")))
            cat(paste0("[shiny, Save()] INFO,", ASE_name(),"_Servers.cfg config file saved in directory General_data.\n"))
            
            # Saving file *_SETTIME
            progress$set(message = "[shiny, Save()] INFO,Saving Calibrated/extrapolated data in General.df", value = 0.2)
            
            sens2ref <- CalTime()
            row.names(sens2ref)  <- sens2ref[,"name.gas"] 
            sens2ref             <- as.data.frame(t(sens2ref), 
                                                  stringsAsFactors = FALSE)
            write.table(sens2ref, 
                        file = file.path(DisqueFieldtestDir(),"General_data",paste0(ASE_name(),"_SETTIME.cfg")), 
                        col.names = TRUE)
            #save(sens2ref, file         = file.path(DisqueFieldtestDir(),"General_data",paste0("ASE_name(),"_SETTIME_cfg.Rdata")))
            cat(paste0("[Shiny, Save()] INFO: ", paste0(ASE_name(),"_SETTIME.cfg")," config file saved in directory General_data.\n"))
            
            
            # saving Covariates and CovMod config file
            # Make sure it closes when we exit this reactive, even if there's an error
            progress$set(message = "[shiny, Save()] INFO, Saving Covariates and CovMod Config Files", value = 0.2)
            
            for (i in 1:length(list.name.sensors())) {
                write.csv(data.frame(Effects = input[[paste0("Sens",i)]], 
                                     stringsAsFactors = FALSE), 
                          file = file.path(DisqueFieldtestDir(),"General_data",paste0(ASE_name(),"_Covariates_",list.name.sensors()[i],".cfg")), 
                          row.names = FALSE)
                if (input[[paste0("Calibration",i)]] == "MultiLinear") {
                    write.csv(data.frame(Effects = input[[paste0("CovMod",i)]], 
                                         stringsAsFactors = FALSE), 
                              file = file.path(DisqueFieldtestDir(),"General_data",paste0(ASE_name(),"_CovMod_",list.name.sensors()[i],".cfg")), 
                              row.names = FALSE)
                }
            }
            cat(paste0("[shiny, Save()] INFO,", paste0(ASE_name(),"_CovMod*.cfg")," config file  saved in directory General_data.\n"))
            
            #-----------------------------------------------------------------------------------CR
            # 2 - Saving data in General.csv and General.Rdata
            #-----------------------------------------------------------------------------------CR
            # Saving Calibrated/extrapolated data in General_data Files - Saved is put on quit otherwise it is too long
            progress$set(message = "[shiny, Save()] Saving Filtered, Calibrated and extrapolated data", value = 0.2)
            cat("-----------------------------------------------------------------------------------\n")
            cat("[shiny, Save()] Saving Filtered, Calibrated and extrapolated data in ", General.Rdata.file,"\n")
            #DF$General <- DF$General[!duplicated(DF$General$date),]
            General.df <- DF$General
            save(General.df, file = General.Rdata.file)
            rm(General.df)
            #General.csv.file    = file.path(DisqueFieldtestDir(), "General_data", "General.csv"  )
            #write.csv(General.df, file = General.csv.file)
            #Make.Old(File = General.Rdata.file)
            #Make.Old(File = General.csv.file)
            progress$set(message = "[shiny, Save()] Saving Filtered, Calibrated and extrapolated data", value = 1.0)
            
            #-----------------------------------------------------------------------------------CR
            # 3 - Saving list of Indexes for warming, Temperature/Humidity, Invalid sensor data, Negative reference values, Outliers
            #-----------------------------------------------------------------------------------CR
            # Saving Index for warming of sensors
            cat("-----------------------------------------------------------------------------------\n")
            cat("[shiny, Save()] INFO, Saving Index of warming of sensors\n")
            progress$set(message = "[shiny, Save()] INFO, Saving Index of warming of sensors", value = 0.2)
            list.save(x = ind.warm$out, file = ind.warm.file)
            
            # Saving the list of Index of temperature and humidity out of interval of tolerance
            cat("[shiny, Save()] INFO, Saving Index of temperature and humisity out of interval of tolerance\n")
            progress$set(message = "[shiny, Save()] INFO, Saving Index of temperature and humisity out of interval of tolerance", value = 0.4)
            list.save(x = ind.TRh$out, file = ind.TRh.file)
            
            # Saving the list of Index of invalid sensor data
            cat("[shiny, Save()] INFO, Saving Index of invalid sensor data\n")
            progress$set(message = "[shiny, Save()] INFO, Saving Index of invalid sensor data", value = 0.6)
            list.save(x = ind.Invalid$out, file = ind.Invalid.file)
            
            # Saving the list of Index of sensor data outliers
            cat("[shiny, Save()] INFO, Saving Index of sensor data Outliers\n")
            progress$set(message = "[shiny, Save()] INFO, Saving Index of sensor data Outliers", value = 0.6)
            list.save(x = ind.sens$out, file = ind.sens.out.file)
            
            # Saving the list of Index of reference data outliers
            cat("[shiny, Save()] INFO, Saving Index of reference data Outliers\n")
            progress$set(message = "[shiny, Save()] INFO, Saving Index of reference data Outliers", value = 0.6)
            list.save(x = ind.ref$out, file = ind.ref.out.file)
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

