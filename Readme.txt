The JAVA software , shell scripts, C applications, drivers and configuration files developed for the AirSensEUR sensor box is released under the European Public License, an open source license for software. The copyright notice is as follows: 
License
Copyright 2018 EUROPEAN UNION
Licensed under the EUPL, Version 1.2 or subsequent versions of the EUPL (the "License"); You may not use this work except in compliance with the License. A copy of the License is given here after. Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
Authors
- Michel Gerboles, michel.gerboles@jrc.ec.europa.eu; Laurent Spinelle, laurent.spinelle@jrc.ec.europa.eu, and Alexander Kotsev, alexander.kotsev@jrc.ec.europa.eu - European Commission - Joint Research Centre, and
- Marco Signorini, marco.signorini@liberaintentio.com

New release V0.13
2019-04-30 : E43 - In NavBarMenu "GetData", sideBarLayout tab "reference data", there was several errors when adding new reference data. Now new data with new dates can be added
                   and new variables for existing dates can be added when using the "csv" download type. The time average that takes cpu is only carried out if the date interval of 
                   time series is different than the requested "Averaging time in min" in tab "Time-shield" or if the date in the reference data files do not fall on full hours 
                   (00:00 or 00:00:00). 
             E48 - It is now necessary to give the coordinates of the reference station every time reference data are downloaded + Correction of the transformation of spherical 
                   reference coordinates into decimal degrees.
             N91 - Change: In function CONFIG() of Functions4ASE, if any config files is missing (server, filtering ...) an error message is displayed and the App wil crash.
                   The App will not try to create the config file anymore
             N92 - Download of reference data is now possible using the protocol of data download of the a_i_p company (Austrian company used in Austria and Germany + JRC EMP station)
             N93 - Passwords are now hidden in SideBar Layout and reference download (Influx, a_i_p...)
             E44 - A bug was corrected that resulted in a general crash of the App when creating a new ASE config file for a new box with empty SETTime file.
             E19 - Plot and saving when input$Sens.rm.Invalid. Some mistakes in sensor names in Plot.Invalid.Sens. Solved.
             E21 - BUG in create new, wrong Old_General_dir. Corrected.
             E46 - Bug corrected: when adding new period of time of reference data, these were not added to RefData.rdata and RefData.csv if General.rdata already inluded sensor data,
                                  even though the reference data were NA. Correction: in Down_ref(): max(Reference.i$date, na.rm = T) > DownloadSensor$DateEND.General.prev 
                                  replaced with max(Reference.i$date, na.rm = T) > DownloadSensor$DateEND.Ref.prev
             E47 - Bug Corrected: When sensor or reference data are manually added into the GetData NavBarMenu, the dataFrame DF$General was not updated since the only possibility
                   to update it was either that General dataframe is not yet read or existi (DF.NULL$Init = TRUE) and 2nd possibility was to change the time delay between sensor and reference 
                   (Change.Delay() = TRUE). The following tests are added to check if they are new reference or sensor data: isTRUE(DownloadSensor()$DateEND.General.prev < DownloadSensor()$DateEND.Ref.prev) 
                   isTRUE(DownloadSensor()$DateEND.General.prev < DownloadSensor()$DateEND.Influx.prev)  and isTRUE(DownloadSensor()$DateEND.General.prev < DownloadSensor()$DateEND.SOS.prev)
             N94 - two new crosscheck Boxes are added into the menu bar "DataTreatment" - sideBar layout AND tab "Calib" called: "Sync.Cal" and "Sync.Pred". When calibrating if "Sync.cal" is checked, before calibration
                   there is an automatic process that look for the lag that results in the best synchronisation of sensor and reference data (including cross-sensitivities). A message in the console 
                   will indicate if the lag <> 0. " Sync.Pred" is used for the same purpose when predicting with an existing calibration model. 
                   Do not play too much with these parameters for no reasons, they consume a lot of cpu time. Suggestion: use "TimeSeries" to plotand check if a lag can be identified.
                   Advise try to calibrate with a linear model and check "Sync.Back" or "Sunc.Pred" to check if any lag can be evidenced.
             N95   menu bar "DataTreatment" - sideBar layout AND tab "Calib" called: A NEW SELECT INPUT IS ADDED "ubs(xi), between sensor uncertainty" with no effect for now. It will be used for uncertainty calibration
