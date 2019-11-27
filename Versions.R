#================================================================CR
# Version History ====
#================================================================CR
# New release V0.15 (V0.14 is not distributed, V0.15 includes both changes of v0.14 and v0.15)
# 2019-11-20   E51 - bug corection: When downloading InfluxDB data in minutes values, not all data are downloaded, only the 1st 10000 within 30 day period.
#                    Problem with downloading of a maximum of 10000 records (lines) from the InfluxDB. The maximum number (10000) of records
#                    has been defined ad 10000/(24*60/Mean) where mean is the "Averaging" to be set when downloading the sensor data from the Influx DB
#              E52 - bug corection: Impossible to calibrate with a linear model when a MultiLinear calibration model already exists. This is because the word "Linear" is included into the word "MultiLinear".
#                    Corrected.
#              E53 - bug corection: Mistake when plotting a "Drift" of calibrated sensor data: when computing the difference between calibrated sensor data minus reference data, this is called "residual",
#                    if any calibrated sensor data or reference data is missing, the residual should not be computed. Corrected using only complete.cases for estimating residuals.
#              E54 - bug corection: When calibrating a sensor using a multiLinear calibration model with another sensor (2) among the covariates. The app may crash if the the calibrated value of
#                    sensor 2 are not yet computed. An alert message is now displayed and the app does not crash anymore.
#              E55 - Bug Correction, When calibrating a sensor using a multiLinear calibration model with a list of "Covariates for calibration" of the sideBarLayout that is different from the
#                    covariate list of the MultiLinear, the App crashes. Now corrected with a shinyalert message. The app does not cash anymore.
#              N96 - Under NavBar Menu "Help", the user manual now directly shows the Google Doc document without need for downloading a pdf. It seems that the position of box and arrows
#                    in figures is messed up.
#              E56 - Bug correction impossible to save graphic file of matrix plot in calibration and prediction. Corrected by setting the dimension of the graphic file. The fixed
#                    dimension may create distorsion of the plotted R2 and equation equation
#              N97 - The process of reshaping the airsenseur.db is now much faster using function spread of tidyverse in function sqlite2df()
#              E57 - bug corection: when loading reference data using an rdata file there was a bug for the names of the fields in dataframe reference.i. Added support for temperature,
#                                   humidity and pressure in the reference data.
#              N98 - bug corection: Added message to keep UTC time zone when downloading sensor data using InfluxDB uner NavbarMenu "Getdata", tab "Sensor Data".
#              E58 - bug corection: When plotting a time series using Dygraphs, the last selected day was excluded from computation and plotting as selection returned a type Date
#                                   without time values. Corrected adding one day to the last selected date. All dygraph time series plots used the local time zone. Now set to UTC.
#              N99 - Checking if R runs in 32-bist system, advising to switch to a 64-bit system for efficiency
#             N100 - In the map showing the locations for calibration and prediction, all AirSensEUR and reference station sites with at least 0.0001 decimal degree of difference different
#                    are plotted with grey circles and blue markers. This can show the path in mobility when it will be needed or for calibration at multiple sites.
#                    The extent of the map is automatically set to the bounds of the GPS coordinates during the calibration or prediction time interval.
#             N101 - In order to avoid the confusion with separator of longitude and latitude of the reference station, the coordinates are entered in 2 different text input, see "GetData"
#              E59 - bug corection: In NavBarMenu "DataTreatment", mainTaPanel "PlotFiltering", the plots of Warming, Temp & Hmidity, Invalid and Outliers always used the date/time selected
#                                   for the first sensor, under "Range of dates for plotting outliers", whatever sensor being selected. Corrected: now the Range of dates for plotting outliers
#                                   is selected for each sensor.
#              N83 - When computing time averegae means, openAir::timeAverage is replaced with rcpp::roll_mean for save cpu time
#                    Lately: The speed of the code for averaging dataframe from minute to hours is improved using data.table package
#              E61 - Bug correction solved: crash when CovMod Config file is empty. Solved using read_csv instead of read.csv
#              E62 - bug corection: When the last date of General.Rdata ends before the last date of Refdata without any Influx Data available for the new dates in RefData, the reactive GENERAL function is triggered
#                    GENERAL is run and the resulting dataframe General.df does not include the outliers columns yet. Consequently the Outliers discarding module is run at each startup.
#                    This is now solved by using all.equal instead of identical and comparing only common colums of saved Genera.Rdata files and the returned dataframe of function GENERAL.?
#              E63 - bug corection: In function GENERAL, merge of data frames RefData and InfluxData is replaced with merge of data.tables to increase speed
#              N92 - improvement of download of reference data with a_i_p server for minutes values, managing, valid data, flagged data and aggregation
#             N102 - The "Cal" button was added in the "Range of dates for plotting covariates:", tab "SetTime" of the Sidebar Layout of the "Data Treatment" NavBar menu. Clicking this button will set the
#                    dates of "Range of dates for plotting covariates:" to the ones of "Range of dates for calibration:".
#              N82 - reduce the time for detection of outliers: go parallel computing with different version for linux and windows
#                    Instead of using parallel computing the roll_apply function to compute Median Average Deviation is replaced with caTools::runmad in function my.Rm.Outliers. Time gain: from 77 sec
#                    to less than 2 sec for big dataframes.
#              E64 - bug corection: When loading and saving being ojects (General.Rdata or models.RDS), the files and object soemtimes becomes very big because some garbage in the environment is loaded and saved.
#                    Typically it becomes impossible to work with quantile calibration model (called Linear.robust using rq()). This now solved by creating a new environment before loading
#                    or saving these obects. Serailizing and unserializing also helps removing the garbage.
#              N93 - Passwords are also hidden in the tabPanel GetData Panel
#              N89 - Dew point deficit added among the variables to matrix plots and calibration of model provided that reference data include temperature and humidity
#                    For reference data, Ref.Absolute_humidity and Ref.Td_deficit are added, and for AirSensEUR, Absolute_humidity and Td_deficit are added. This is consistent with previously created
#                    General.Rdata file, for which these variables are added if missing.
#              E60 - bug corection: The problems of plotting the uncertainty, scatter plot and orthogonal regression is solved using a TabSetPanel under menu "data treatment" - "Prediction" - "Uncertainty"
#              E61 - bug corection: Wrong calculation of the DateIn and DateEND selection for all plots: DateIN <- min(.. replaced with DateIN <- max(.. and DateEND <- max(.. replaced with DateEND <- min(..
#              E50 - bug corection: In "GetData" when changing sensor shield, the App start turning around exchanging shield file
#                    The use the config file of chemical shield has been completely changed with obligation to delete General data and re doing all data tretment.
#             N103 - General data is now saved in CSV rather than in Rdata. It is saved and opened with data.table fread and fwrite to speed up the process
#              E49 - bug corection: Add the between sampler uncertainty is the calculation of sensor uncertainty. This is now d
#                    A new parameter was added into the Side Layout , u(bs), between sensor uncertainty and U(xi) is renamed u(bsRM), between reference data uncertainty. This is for the calculation
#                    of the emasurement uncertainty, see CEN TC 264 WG 42 protocl of evaluation of gas sensors
#              E64 - bug corection: When SETTIME is read, a consistency check of available data dates and dates of outliers, validity, calibration and prediction is carried oout in order to avoid a crash of the App.
#                    Dates are corrected if needed
#             N104 - the TabPanel "Influx Sensor Data", "SOS Sensor Data", "Refernce Data" and "General Sensor Data" show now the begining and ending date of dataframes.
#             N105 - general improvement of data download and data treatment by using package data.table. Only the download of SOS data is still to be improved
#              E23 - bug corection: Following a first data treatment with one ASE box When selecting a 2nd different ASE box in Navbar Menu SelectASE, there is generally a crash of the code.
#                    The App has been rewritten, limiting the reactivity that was causing the crash of the App. uploading of of all data is now carried out when selecting an ASE box
#                    with resetting of all data set. It should be now possible to switch between ASE box depending how deep you go with the data treatment.
#              N21 - Add automatic reporting, Markdown, knit (WORK IN PROGRESS)
#              E15 - If the firmware of the sensor shield is changed during the use of an AirSensEUR box, the sensor data are wrongly converted to V or nA , e.g. ASE JRC-01 for NO23E50
#                    This is now solved, config fiels are updated when the shield config file is changed. The App stop and when restarting all data are correctly updated
#

#  ----#TO BE DONE  : ----
# BUG CORRECTIONS
#              E4 - It seems that the detection of directory from where the script is run detected using function Script_Dir() does not allways works, it should be made transparent for user
#              E6 - General.conv(): x_DV Values are converted in volt or nA by substracting the zero.Board in Volt? This is an error if the conversion is carried out in nA.
#                   Change substraction to V or nA
#             E26 - It seems that the detection of invalid data is not performed automatically when the file ind.Invalid.file does not exist or that it is performed after the detection of outliers
#                   and hence not applied to DF$General. You can check on the mainTabPanel PlotFiltering - Invalid data appear.
#             E36 - Some of the Spin Loaders keep on spining after updating of the plots. Others do not realized when they receive the updated plots and do not display them. Have a look.
#             E45 - The time zone used in the mainTabPanel "Plot Filtering" - "Invalid" - "Table" seems to use the local time zone instead of the data series ime zone ("UTC") when discarding values.
#             E60 - When calibrating NO2-B43F with a multilinear model including ExpGrowth of temperature and linear effect of humidity, the model fitting crash. IT is likely due to the startvalues

# NEW FEATURES needed: ----
#              N2 - Calibration with linear.robust: add RMSE on statterplot ...
#              N3 - Add model calibration: neural network model in the list of possible calibration method.
#              N5 - Add model resulting of laboratory experiments for calibration.
#              N6 - In NavBar menu "Help": add videos on how to use the shiny interface.
#              N7 - Do Filtering and conversion only for the selected sensor, not for all sensors.
#              N8 - add "Sensor" and "Reference" in front of covariates in the comboBox of SideBar "Calib"
#             N11 - Add evaluation tools: Sensor Evaluation Toolbox (SET) (Barak Fishbain).
#             N12 - For invalid data: allow to resume data to initial value if CheckBoxes "Enable Outlier discarding" set to FALSE.
#             N13 - Detect nearest AQMS using GPS coodinates and and download with SOS
#             N15 - Add support for OPC-N3-2 and MOx sensor
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
#             N51 - Every time a .png files for rawData, scatterPlots, time series, matrix, Uncertainty, drift and targetDiagram exists in Calibration, mModelled_gas,
#                   General_Data should not create a new plot and rather uses the .png plot instead
#             N52 - Add the possibility to invalidate humidity transient
#             N53 - Create a button "Delete" of AirSensEUR in NavBar menu "SelectASE"
#             N54 - Finish Shiny App Manual
#             N88 - Add interactive selection of points in the matrix plots of covariates, calibration and prediction
#             N89 - Add the possibility to set some of the coefficients of calibration models with plotty
#            N106 - Add the possibility to use several calibration models at different date interval
#            N107 - Add the possibility to fit RSS when computing uncertainty ("RSS.fitted")
#
# New release V0.13
# 2019-04-30 : E43 - In NavBarMenu "GetData", sideBarLayout tab "reference data", there was several errors when adding new reference data. Now new data with new dates can be added
#                    and new variables for existing dates can be added when using the "csv" download type. The time average that takes cpu is only carried out if the date interval of
#                    time series is different than the requested "Averaging time in min" in tab "Time-shield" or if the date in the reference data files do not fall on full hours
#                    (00:00 or 00:00:00).
#              E48 - It is now necessary to give the coordinates of the reference station every time reference data are downloaded + Correction of the transformation of spherical
#                    reference coordinates into decimal degrees.
#              N91 - Change: In function CONFIG() of Functions4ASE, if any config files is missing (server, filtering ...) an error message is displayed and the App wil crash.
#                    The App will not try to create the config file anymore
#              N92 - Download of reference data is now possible using the protocol of data download of the a_i_p company (Austrian company used in Austria and Germany + JRC EMP station)
#              N93 - Passwords are now hidden in SideBar Layout and reference download (Influx, a_i_p...)
#              E44 - A bug was corrected that resulted in a general crash of the App when creating a new ASE config file for a new box with empty SETTime file.
#              E19 - Plot and saving when input$Sens.rm.Invalid. Some mistakes in sensor names in Plot.Invalid.Sens. Solved.
#              E21 - BUG in create new, wrong Old_General_dir. Corrected.
#              E46 - Bug corrected: when adding new period of time of reference data, these were not added to RefData.rdata and RefData.csv if General.rdata already inluded sensor data,
#                                   even though the reference data were NA. Correction: in Down_ref(): max(Reference.i$date, na.rm = T) > DownloadSensor$DateEND.General.prev
#                                   replaced with max(Reference.i$date, na.rm = T) > DownloadSensor$DateEND.Ref.prev
#              E47 - Bug Corrected: When sensor or reference data are manually added into the GetData NavBarMenu, the dataFrame DF$General was not updated since the only possibility
#                    to update it was either that General dataframe is not yet read or existi (DF.NULL$Init = TRUE) and 2nd possibility was to change the time delay between sensor and reference
#                    (Change.Delay() = TRUE). The following tests are added to check if they are new reference or sensor data: isTRUE(DownloadSensor()$DateEND.General.prev <
#                    DownloadSensor()$DateEND.Ref.prev)
#                    isTRUE(DownloadSensor()$DateEND.General.prev < DownloadSensor()$DateEND.Influx.prev)  and isTRUE(DownloadSensor()$DateEND.General.prev < DownloadSensor()$DateEND.SOS.prev)
#              N94 - two new crosscheck Boxes are added into the menu bar "DataTreatment" - sideBar layout AND tab "Calib" called: "Sync.Cal" and "Sync.Pred". When calibrating if "Sync.cal" is
#                    checked, before calibration there is an automatic process that look for the lag that results in the best synchronisation of sensor and reference data (including cross-sensitivities).
#                    A message in the console #                    will indicate if the lag <> 0. " Sync.Pred" is used for the same purpose when predicting with an existing calibration model.
#                    Do not play too much with these parameters for no reasons, they consume a lot of cpu time. Suggestion: use "TimeSeries" to plotand check if a lag can be identified.
#                    Advise try to calibrate with a linear model and check "Sync.Back" or "Sunc.Pred" to check if any lag can be evidenced.
#              N95   menu bar "DataTreatment" - sideBar layout AND tab "Calib" called: A NEW SELECT INPUT IS ADDED "ubs(xi), between sensor uncertainty" with no effect for now.
#                    It will be used for uncertainty calibration
#
#
# New release V0.12
# 2019-01-15 : N71 - The MainTabPanel "MultiLinear" (before Multivariates) now shows the content of the MultiLinear file of the selected sensor. The display content of the MultiLinear is updated
#                    every time a file is saved.
#              E27 - Bug Correction in Ind.Sens.Out: Outliers.Sens$Forced was never set to TRUE when the columns c("Out.", "Out.Warm.", "Out.TRh.", "Out.Invalid.", "Out.Warm.TRh.", "Out.Warm.TRh.Inv."
#                    were missing from DF$General while the file Ind.Sens.Out.Rds existed. This is now solved see # Reactive ind.sens.out. The same bug was corrected for Ind.Ref.Out
#              E28 - Bug Correction in plot calibration after deleting a Multilinear calibration model, if the next selected calibration is automatically set to model without covariates an error occurs
#                    as Covariates.Model is character(=) and cannot be tested with if. This bug is solved by adding a test (if (identical(Covariates.Model, character(0))))  in plot.calibration()
#              E29 - Bug Correction in NavBarMenu "Data Treatment", MainTabPanle "Calibration"-"MultiLinear". The MultiLinear file was not correctly displayed if it existed. Now corrected.
#              E30 - Bug Correction in uploading reference data from a csv file under Linux. The name file was not correctly reported. Bug corrected
#              E31 - Bug Correction for selecting Valid dates when new reference or sensor data are uploaded. It was not possible to select the dates of new data in NavBarmenu "DataTreatment",
#                    sideBarLayout SetTime. Corrected.
#              E32 - Bug Correction: everytime that the App was launched, the file of reference data was saved even if no new reference data were added, wasting cpu time. Corrected: now the file
#                    is saved only if new data are added.
# 2019-02-06:  E33 - Bug Correction: updated the saving of the .config file (ASE_name.cfg) in order to add the PM10 sensor to the existing shield that in only for gaseous species
# 2019-02-06:  E34 - Bug Correction: unpdated the saving of the SETTIME.cfg file where all elements have been forced to be characters
# 2019-02-08   E35 - in nNavbarMenuu GetData" - "Reference Data" when using option "csv", the csv or Rdata file can now add new parameter for existing date, before it was only adding values for new dates
# 2019-01-31:  N84 - Added interactive maps calibration and Prediction that show the position of AirSensEUR and Reference monitoring station. Therefore, it is necessary to add the coordinates
#                    of reference station when downloading reference data.
# 2019-02-10:  N85 - Version history is now in a separate file called Versions.R. The configuration is now in file global.R wun before the UI. tcltk functions are removed
# 2019-02-14:  N86 - Adding new model for calibration of sensor: T_power(in °Celsius), K_power(in Kelvin), CexpkT (in °Celsius) and CexpkT (in Kelvin),
#                    Ri = a0 + a1 NO + a2 exp(a4 Temperature) and Ri = a0 + a1 NO + a2 exp(a3 Temperature).
#              E36 - Bug in the detection of outliers Out.my.Outliers (Fucntions4ASE.R), when the minimum of the interval of tolerance zmin was < Thresholdmin, zmin was not correctly set to ThresholdMin,
#                    with a mistake for zmax in plotting.
#              E37 - NavBarMenu "DataTreatment", menu "Retrieved" deleted as it was doing the same as "RawData"
#              E38 - MainTabPanel "Calibration" - "MultiLinear", the list of MultiLinear files was corrected: before all files weres shown, now only the file of the selected sensor is shown.
#                    The RHansomeTable is only shown when the MultiLinear type of calibration model is seleted in the SideBar Layout
#              E39 - In MainTabPanel "Calibration" and "Prediction", in the residuals matrix plots, the coefficients of correlation (r) were displayed instead of the Coefficients of determination (R2). Corrected
#              E40 - When saving calibration model in rds files, there was a mistake in the serialisation of objects, the whole parent environment was saved resuting in eneormous files. This has
#                    been solved saving rdata files and using the broom package.
#              E41 - When Calibrating one sensor and computing Predicted data, only the data of the selected sensor and calibration model are computed. Before, the Prediction for all sensors was
#                    recomputed, resulting in long cpu time.
#              E13 - All outlier window sizes changed to 19 data (3 hours correspond to 18 + 1 data of 10 min) for the detection of outliers. Done
#              E18 - input$hot used instead of input$table in saving valid file. Solved
#              E22 - Download of reference data (Down_Ref): if download is resumed, the download of the last date of the last download is repeated, check. Solved.
#              E17 - Error of User Interface for uiFiltering and uiCalib when Config is changed (e. g. when a new AirSensEUR is selected). Solved
#              E25 - Additionally, when changing the covariates in the UI that do not correspond to the list of covariates of the sensor_Multi file, it becomes impossible to calibrate. Solved
#              N87 - The times series plot in the mainTabPanels of Filtering, covariates, calibration, Prediction and RawData are now interactive, use mouse to select time ranges.
#               E3 - The unit of y axis for the plot outliers of reference values is incorrect, it is not raw unit but ppb, ppm or ug/m3. Solved
#              E42 - There was a error in the detection of warming time when an AirSensEUR box re-starts after a long inactive period. The wrming necessary after re-starting was not included.
#              Corrected.
#              N89 - Absolute humidity added among the variables to matrix plots and calibration of model.
#              N90 - Added detection of dates for each new Reference data to add to the previously downloaded Reference data. Only if the time-interval of the new Reference
#                    data is not equal to the UserMins, then TimeAverage is carried out as from the previous Reference data.

# New release v0.11
# 2018-11-11 : N66 - For all plots of MainTabPanel "PlotFiltering" (warming, Temperature and Humidity, NegValues, Invalid and Outliers) when the time span is lower or equal than the number of tick
#                    mark of X axis #                    the time of day is added to the dates on the x axis labels.
# 2018-11-11 : N67 - Adapting the dimension of all plot windows to resolution 1920 x 1280 (see also N1)
# 2018-11-12 : N68 - NarBar Menu "Data Treatment". The first time that the tab is opened, the Merging of Influx, SOS and ref data is automatically launched. No need to click on button
#                    "Merge Influx <- SOS <- Ref".
# 2018-11-13 : N69 - NarBar Menu "Data Treatment". Change of behaviour of checkBox "savePlot": it is necessary to check it everytime a plot must be saved. "Save Plot" also alows saving the
#                    ReportMarkDown (see below N76).
# 2018-11-13 : N70 - All scaterplots were modified so that the Tick marks of the X and Y axis used pretty values.
# 2018-11-13 : N71 - Navbar "Data Treatment", sideBar list box "List of covariates to plot" and "List of covariates to calibrate": the variables "date" and "sensor_modelled" were added.
#                    This allows observing the drift of sensor values (raw and calibrated). It is then possible to use time and calibrated sensors in MultiLinear calibration.
#                    Under MainTabPanel "Calibration", a new TabPanel called "Multivariates" allows setting the degree of polynomial of any covariates. Status of all Co_variates shall be "Enabled".
#                    "Forced" will be used in future to set the coefficients of polynomial of Co-variates, it does not work yet, and should be set to "FALSE". WORK IN PROGRESS
#                    Plot.Covariates can use now data calibrated sensor data. It is also possible to calibrate sensor (e. g. O3) using values of calibrated sensors (e. g. NO2).
# 2018-11-25 : N73 - Navbar "Data Treatment", MainTabPanel "DataTable", the number of digits of all varaiables have been optimised in order to enhance reading
# 2018-11-25 : N74 - Navbar "Selected ASE", when using the button "Quit" to leave the shiny App, the RAM is purged from garbage, allowing to decrease RAM usage.
# 2018-11-25 : N76 - NavBar "dataTreatment", MainTabPanel "report Markdown". This is a 1st tentative of automatic reporting. Work in progress. It needs that the Polt of calibration and extrapolation, both scatterplots and
#                    timeSeries are saved using button "Save Plot". WORK IN PROGRESS
# 2018-11-25 : N77 - Dynamic allocation of sensors name in User Interface and config file. This will be helpful when CO2 and PM will be included into the App.
# 2018-12-02 : N78 - General improvement of reactivity and use of memory, allowing to manage larger datatests (e. g. minute values) with accepting time for data treatment except for the detection
#                    of outliers is still a long process. The datasets and config files are no more saved during data treatment allowing faster reaction. However, it is necessary to click of
#                    button "Save" to save data and config not to loose your work.
# 2018-12-03 :  N4 - NavBar "getData", new mainTabPanels to display of dataframes of downloaded inlfux, SOS and ref in navBar menu "getdata". It would be good to add the summary table shown in
#                    "Navbar "DataTeatment" - "Config".
# 2018-12-02 :  N1 - All plots use now the whole heigth of display and resize for any for plotting.
# 2018-12-03 :  N9 - The names of Model calibration are listed without the airsenseur name and sensor name in the list of Calibration model to ease reading
# 2018-12-06 : N79 - When the App is busy computing, a spinner is displayed to inform users about data treatment is going on
# 2018-12-10 : N80 - my.rm.outliers: the computing time for detecting outliers has been divided by two, computing the min and max of interval of tolerance within one rollapply
# 2018-12-11 : N81 - navbar "DataTreatment" - "PlotFltering", a new tab is added, called StatFiltering which gives the counts of all fileterd data (warming, temperature/humidity, Invalid, outliers,
#                   negative reference) for each sensor
# 2018-12-12 : Bug Correction: E2 - When the averaging time (GetData in time-shield tab) is decreased, it is necessary to repeat all downloading, merging and calculation (warming, T/RH, outiers,
#                                   conv and cal) because raw data with initial time average are no more available (input$UserMins)
#                                   In the NavBar menu "Get Data" of the SideBar Layout, thre is a new SelectInput "Averaging time in min for extrapolated data" that allow to change the averging time of the "Extrapolation mainTabPnale,
#                                   e. g. having all the calibration Datatremant with 1 minute averaging time and the extapolation with hourly values.
# 2018-11-02 : Bug correction: E19 - By mistake the detection of outliers is launched when the extent of slider input for dates (Set.Time())) is read: sliderInput of reference data are repaced
#                                    with dateRange, easier to use
# 2018-11-09 : Bug Correction: reset button SavePlot after a plot is saved. It is necessary to check again "SavePlot" to save a new plot
# 2018-11-13 : Bug Correction in SETTIME, the time zone of all dates were set to the time zone of RefData. This is changed to setting to the time zone of DownloadSensor$DateIN.General.prev
#             if it exists then to DateIN.Influx.prev if it exists then to DateIN.SOS.prev  if it exists otherwise it is set to "UTC"
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
