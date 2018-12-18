#================================================================
# Configuration of parameters for AirSensEUR calibration - file: ASEconfig.R
#================================================================
# Licence:
# Copyright 2017 EUROPEAN UNION
# Licensed under the EUPL, Version 1.2 or subsequent versions of the EUPL (the "License"); 
# You may not use this work except in compliance with the License. 
# You may obtain a copy of the License at: http://ec.europa.eu/idabc/eupl
# Unless required by applicable law or agreed to in writing, the software distributed 
# under the License is distributed on an "AS IS" basis, WITHOUT WARRANTIES OR CONDITIONS 
# OF ANY KIND, either express or implied. See the License for the specific language 
# governing permissions and limitations under the License.
# Date: 05/16/2017
# 
# Authors
# - Michel Gerboles        , michel.gerboles@ec.europa.eu  - European Commission - Joint Research Centre
# - Laurent Spinelle       , laurent.spinelle@ec.europa.eu - European Commission - Joint Research Centre
# - Maria Gabriella Villani,                                   - ENEA
# - Marco Signorini        , 
# - Alexander Kotsev       , 
#================================================================

#================================================================
# Version History
#================================================================
# 2017-01-26 First Commit
# 2017-02-03 Valid periods defined per sample and implementation in General.t.valid
# 2017-02-08 Temperature and humidity ranges defined per sample and implementation in General.t.valid
# 2017-02-08 Calibration model defined per sample and implementation in General.t.valid
# 2017-02-08 Calibration and extrapolation periods defined per sample and implementation in General.t.valid
# 2017-06-10 Added hoursWarming, the number of hours to invalidate sensor values after switching on sampling
# 2017-07-03 Use of shiny for GUI
# 2017-12-07 Functions Config() and SETTIME transfered to Function4ASE.R to simplify maintenance

#================================================================
# Contents ASEconfig.R
#================================================================
# USER CONFIG PARAGRAPH
# LIST OF PARAMETERS TO BE SET BY THE USER (1--9):
#  1. Configuring Proxy server
#  2. Sensor configuration for download for Influx and SOS. InfluxDB has more info and is preferred over SOS
#  3. Reference data, configuration for download, ftp                               
#  4. Create sensor configuration file and matching between reference and sensor names 
#  5. SET Average time for sensor data
#  9. SET temperature and relative humidity thresholds for sensors validity
# 11. Valid Periods                                                                 (NOT USED)
# 12. SET TIME PARAMETERS -> see in ASE_OPER_SCRIPT.R                               (NOT USED)
# 13. SET  Models for NO2 and O3 system resolution                                  (NOT USED)
#================================================================

cat("[ASEConfig.R] INFO 2017-12-07 Functions Config() and SETTIME transfered to Function4ASE.R to simplify maintenance\n")
cat("[ASEConfig.R] INFO 2017-12-07 SUGGESTION to manage the directories of ASE box instaed of file ASECOnfigxxxx.R\n")

