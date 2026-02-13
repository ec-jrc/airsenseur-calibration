#================================================================CR
# Licence:
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
#================================================================CR
#options(digits=16)
#install.packages("polynom")
#library(polynom) # for Meas_Funtion
# changes 141114
# Cal_Line: name of the plot added in  parameters
#           the fitted line is now correctly printed
#           for linear,cubic and quadratic models, the number of digits when printing the equation is now controlled by f_coef1, f_coef2, f_R2:
#           number of digit for intercept, slope and R2 using sprintf syntax.
#           f_coef1 is used both for intercept and s(Res), f_coef2 is used for all parameters of the model apart from the intercept
#           Caline can use defined limit  if not null
# Etalonnage: Etalonnage returns xlim and ylim that can later be used with <- (linear only for now)
#             Etalonnage has a new input variable lim, to pass xlim and ylim for plotting
#             Etalonnage has a new input variable steps, to pass the number of tickmark on x and y axis
#             remove as.integer of xlim and ylim if range < 1. To be done
#             lim must become Xlim and Ylim
#
# changes 150714: Cal_Line, the plot of the cal_line is correct for all models using the function estimated.y (previously plo_estimatedX()
#                 estimated.y returns 5000 points for x and y (estimated) that are later plotted in cal_line
#
# change 150730: in function Etalonnage, as.integer() was change for round(x,digit=0).
#     TO DO      An update is needed for the case in which the delta between min and max is <0 for both X and Y
#                Using the difference between max and min, we should find the position of the first not zero number
#                and use this position (rank?) as the digit of the round function
#
# change 150801: Etalonnage function with digitround variable to define the digit of the round function and Marges variable to define the margins of the plot
#
# change 151108: change Etalonnage with lines instead of dots (parameter ligne in the Draf_Sensor_Tools)
#
# change 160209: MGV added functions:
#               Digital2Volt_Sensor_response <- function(Sensor, gas) Converts digital sensor response citytech inresponse in Volt
#               citytech_no2_o3_model <- function(SNO2_volt, SO3_volt, Temp, RH, SO2_emep, time, parameters)
#
# change 160303 in Merge_mdb and Step_AVG the column of date is now called "date" to integrate with OPENAIR package
#                in StepAVG if the step number does not exist then the mean step average is not compute and the "for" loop goes to the next step
#
# change 160317 add function PlotDb, in Cal_Line add plotting of RMSE and AIC
# change 160423 Added Function Create.Sensors creating a dataframe (Sensors) for the automatic evaluation of calibration and inteference evaluation,
#               Added Function Calibration for the automatic evaluation of calibration and inteference evaluation,
#               Added Function PlotDb for the automatic plotting of time series of refernce values and sensor readings from Alpa3 Dbs in directories
#
# change 161025 in PlotDb, cancel na.omit(Calib.df[,c("date", X)]) from line 955 (X = References) and 980 (X = Sensors)
#
# change 160429 in Etalonnage, if any S_Y is null, the S_Y are not plotted
#
# change 161028 in Merge_mdb, before ordering by date, add the conversion of Client.df$date in as.POSIXct to avoid error if the column is something else.
#               This is due to the TirgerSelect dataframe in which Client.df$date is imported as factor.
#               in PlotDb, replace ClientDbSel[which(substring(ClientDbSel, 1 , 10) == substring(ServerDb, 1 , 10))] by ClientDbSel alone
#               in the case ClientDbSel is not NULL
#
# change 161109 in PlotDb, add variable for number of graphs in timeplot: numGraphRef and numGraphSens
#
# change 161110: in Calibration, add the ForceArrows condition to plot arrows on the plot even for calibration.
#                Also add DbCSV to save the minutes data for separated sensors.
#
# change 161111: in Cal_Line, add Sensor_name in the legend. (Draft)
#
# change 161113: add function to load packages Load.packages
#
# change 170328: slop_orth, plot uncertainty for Class 1 and Class 2 sensors as in CEN TC 264 WG42
#
# change 170420: Function PlotDb with plotting of vertical lines showing the steps
#                Functions Create.Sensors transfered to 151016 Sensor_Toolbox.R. Calibration was already transfered since a few weeks
#                Function Etalonnage, "ligne = NULL" added, for consistency with previous version of function Etalonnage if ligne is not given
#                Function Etalonnage transfered to 151016 Sensor_Toolbox.R.
#
# change 170428: Function Cal_Line moved to 151016 Sensor_Toolbox.R (change Sensor_Name, cex)
#                Function Table_Reference, Table_AllEffects , Table_1Effect moved to 151016 Sensor_Toolbox.R, MArDown documents
#                Functions f_lnormal_1par, f_lnorm_2Par, Mode, Fitting_distrib, Fit_Hist moved to 151016 Sensor_Toolbox.R. Fitting lognormal distributions
#
# change 170519: Function Response_Time moved from KeyVOCs_Toolbox.R
#                Function is.between moved from KeyVOCs_Toolbox.R
#                Function loadRData moved from KeyVOCs_Toolbox.R
#                Function THPW moved from KeyVOCs_Toolbox.R
#                Function Hysteresis moved from KeyVOCs_Toolbox.R
#
# change 170526: Function Drift moved from KeyVOCs_Toolbox.R
#
# change 170619: in Etalonnage, add the PlotAxis variable with default=NULL for the plot of the axis.
#			           in Cal_Line, add the default value NULL for the variable lim and the associated explanation + script
#			           in Cal_Line, add the marges variable with default=NULL to set the margins of the plot to a specific format (this is used in the function THPW with marges = "NO")
#
# change 170627: in Calibration, add the "date" in the csv saved dataframe _DataFrame.csv and _df_Full.csv
#
# change 170628: DriftSensors, Function to evaluate drift of sensors
#                ASEPanel04File, Looking for the sensor config file
#                ASEPanel04Read, Reading the sensor config file
#
# change 180118: ASEPanel04Read and ASEPanel04File functions existed in 151016 Sens_Toolbox.R and in Functions4ASE.R. We keept the ones in Functions4ASE.R only.
#
# change 180125: slope_Orth, solve problem of negative uncertainties, set-up different small bugs stopping the code, improve grafical plots
# change 190110: Cal_line: solve error when all s_y are NA, remove x= TRUE and y= TRUE of all nlsLM function since this argument is not allowed, error in "Michelis", replace MIN with intercept
# change 190301: Cal_Line: include weighted regression based on standard deviation of y's per lag
#
#================================================================CR
### Load.Packages: Function Load.Packages (170420)
### resetPar: Function reset graphical parameters
### sink.reset: Function reset sink number
### stopWhenError: Function reset sink and device errors
### slope_orth: Function Orthogonal regression
### Etalonnage: Function View Scatter Plot of calibration function (Vs 170420)
### f_log: Function Fitting a logarithmic model
### f_Unitec: Function Fitting a the Unitec model
### f_ExpDI: Function Fitting an exponential Decay (Increasing form) model
### f_ExpDI_Int: Function Fitting an exponential Decay (Increasing form) model with intercept (see wikipedia)
### f_Michelis: Function Fitting a Michaelis-Menten kinetics model with intercept
### f_ExpDD_Int: Function Fitting an exponential Decay (Decreasing form) model with intercept
### f_Sigmoid: Function Fitting a Logistic - Sigmoidal function
### Estimated.y: Function Plot estimated function
### Cal_Line: Function calibration function and plot calibration line (VS 170428)
#================================================================CR
### Function Load_Packages (170420) ====
#================================================================CR
Load_Packages <- function(list.Packages, verbose = FALSE) {
    # This function install packages if needed and load them
    # list.Packages                 vector of names of the packages to load
    # verbose                       logical default FALSE, return info message about error and installed packages
    if (verbose) cat("-----------------------------------------------------------------------------------\n")
    if (verbose) cat("[Load_Packages] INFO CHECK Installed packages and Toolbox to run the script\n")
    #
    # checking if internet is available and CRAN can be accessed
    isInternet <- curl::has_internet()
    if (verbose) if (isInternet) cat("[Load_Packages] Info: internet is available\n") else cat("[Load_Packages] Info: internet is not available\n")
    for (i in list.Packages) {
        if (i %in% rownames(installed.packages()) == FALSE) {
            if (verbose) cat(sprintf("[Load_Packages] INFO Installing %s", i), sep = "\n")
            install.packages(i)
        } else {
            if (verbose) cat(sprintf("[Load_Packages] INFO Package %s already installed",i), sep = "\n")
        }
        do.call("library", as.list(i))
        if (verbose) cat(sprintf("[Load_Packages] INFO Package %s loaded",i), sep = "\n")
    }
    # List of loaded packages
    if (verbose) cat("[Load_Packages] INFO List of installed packages\n")
    if (verbose) print(search(), quote = FALSE)
    if (verbose) cat("-----------------------------------------------------------------------------------\n")
}
#=====================================================================================CR
# 170609 MG : dirCurrent, returning the Current directory of a script ====
#=====================================================================================CR
CurrentDir <- function(Verbose = FALSE) {
    # This function returns the directory from where the current R script is run.
    # It uses two methods according if Rstudio is used or not.
    # Checking if RStudio is used
    isRStudio  <- Sys.getenv("RSTUDIO") == "1"
    if (isRStudio && Verbose) cat("[CurrentDir] INFO: ASE_Script is run under Rstudio\n")
    # Getting the current scritp
    if (isRStudio) {
        # checking if rstudioapi is loaded
        Load_Packages("rstudioapi")
        # get the direcctory of the ASE_Script using RStudio
        # https://stackoverflow.com/questions/3452086/getting-path-of-an-r-script
        #dirCurrent <- dirname(rstudioapi::getActiveDocumentContext()$path)
        dirCurrent <- dirname(rstudioapi::callFun("getActiveDocumentContext")$path)
    }  else {
        # checking if envDocument is loaded
        Load_Packages("envDocument")
        dirCurrent <- envDocument::getScriptInfo()
        if (!dir.exists(dirCurrent)) dirCurrent <- getSrcDirectory(function(x) {x})
    }
    if (Verbose) cat(paste0("[CurrentDir] INFO: the curent directory of execution of the script is ", dirCurrent), sep = "\n")
    return(dirCurrent)
}
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
#================================================================CR
### resetPar: Function reset graphical parameters ====
#================================================================CR
resetPar <- function() {
    # http://stackoverflow.com/questions/5789982/reset-par-to-the-default-values-at-startup
    # reset par by par(resetPar())
    dev.new()
    op <- par(no.readonly = TRUE)
    dev.off()
    op
}
#================================================================CR
### sink.reset: Function reset sink number ====
#================================================================CR
sink.reset <- function() {
    for (i in seq_len(sink.number())) {
        sink(NULL)
    }
}
#================================================================CR
### stopWhenError: Function reset sink and device errors ====
#================================================================CR
stopWhenError <- function(FUN) {
    # stopWhenError(sink) # for sink.
    # stopWhenError(dev.off) # close all open plotting devices.
    tryCatch({
        while(TRUE) {
            FUN()
        }
    }, warning = function(w) {
        print("All finished!")
    }, error = function(e) {
        print("All finished!")
    })
}
#================================================================CR
### slope_orth: Function Orthogonal regression ====
#================================================================CR
slope_orth <- function(Xlabel, Ylabel, Title, Sensor_name = NULL,
                       DQO.1 = NA, LV = NA, Units = NULL, Disk = NA, WD = NA, Dir = NA,
                       Mat, ubsRM = NULL, variable.ubsRM = FALSE, ubss = NULL, variable.ubss = FALSE,
                       lim = NULL, f_coef1 = NULL, f_coef2 = NULL, f_R2 = NULL, nameModel = NULL, SavePlot = TRUE, calib = NULL) {
    # Save graphic file of the orthogonal regression, square of residuals and uncertainty:
    #                 "mo","sdo", "mm","sdm", "b1", "ub1", "b0", "ub0", "RSS","rmse", "mbe", "Correlation", "nb", "CRMSE", "NMSD",
    #                 and "Mat": "case", "Date", "xis", "yis","ubsRM", "ubss", "RS", "Ur", "U", "Rel.bias", "Rel.RSS"
    # Xlabel, Ylabel : label On the x And y axis
    # Title          : title to appear On the top of the scatter plot of x And y values
    # Sensor_name    : name of the sensor to be written in front of the calibration equation. If NULL, do not print sensor name.
    # DQO.1          : numeric, data qualtiy objective for the expanded uncertainty, same unit as Mat$yis, defaul NA. If NA no DQO.1, horizontal line is plotted
    # LV             : numeric, limit value for Mat$xis, same unit as Mat$xis, default value = NA, plot a vertical line at LV if not NA
    # Units          : character vector, units for the expanded uncertainty, Xis, Yis
    # Disk, WD, Dir  : where you put the graphic files (Disk, working directory, directory), It is sufficient if only Dir is supplied
    # Mat            : Data.table or DataFrame of data including Case number, Date, x And y + optional ubsRM if ubsRM is not constant for all reference values. Idem for ubs.
    # ubsRM          : numeric (default = NULL ), random standard uncertainty of the reference data, xis, given as a constant value for all xis reference values
    # variable.ubsRM : logical, if FALSE (default = FALSE ), ubsRM is used as constant random standard uncertainties for all xis.
    #                  If TRUE ubsRM given in Mat and is used for xis
    # ubss           : numeric (default = NULL ), random standard uncertainty of the sensor data, yis, given as a constant value for all yis
    # variable.ubss  : logical, if FALSE (default = FALSE ), ubss is used as constant random standard uncertainties for all yis.
    #                  If TRUE ubss given in Mat and is used for each yis
    # lim            : passing variable for the limits of the Etalonnage function (cbind(c(minX,maxX),c(minY,maxY)) or NULL)
    # f_coef1,
    # f_coef2,
    # f_R2           : number of digit for intercept, slope and R2 using sprintf syntax.
    #                         f_coef1 is used both for intercept and s(Res), f_coef2 is used for all parameters of the model apart from the intercept
    # nameModel      : name of model to be used to save uncertainty plots, character, default NULL
    # SavePlot       : logical, default is TRUE if TRUE uncertainty plts are saved
    # calib          : Output of function U.orth.DF, default is null. If null, the orthoganl regression is recalculated
    #
    # Homogeneity of variance is tested For the calculation of RSS
    # adding Ur In a New field of Mat
    # returning a list with slope (b and ub), intercept (a and ua), the sum of square of residuals (RSS),
    # the root means square of error (RMSE), the mean bias error (mbe), the coefficeint of correlation (Correlation),
    # the number of valid measurment (nb), the centred rout mean square of error (CRMSE), the normalised mean standard deviation (NMSD)
    # and Mat with relative expanded uncertainty
    # checking if calib is alrady available in order not to redo the calculation
    if (is.null(calib)) {
        #checking that the mat dataFrame is not empty
        nb <- nrow(Mat)
        if (exists("Mat") && !is.null(Mat) && nrow(Mat) > 0) {
            Mat$Max.ubsRM <- sqrt(RSS/(nb - 2) + Mat$bias^2)
            Mat$Max.RSD   <- sqrt(RSS/(nb - 2) + Mat$bias^2)/Mat$xis
            # Printing
            cat("\n")
            cat("--------------------------------\n")
            cat(sprintf("mean of x   : %.1g +/- %.1g",mo,sdo),"\n")
            cat(sprintf("Intercept b0: %.4g +/- %.4g",mm,sdm), "\n")
            cat(sprintf("Slope b1    : %.4g +/- %.4g",b1,ub1),"\n")
            cat(sprintf("Intercept b0: %.4g +/- %.4g",b0,ub0), "\n")
            cat(sprintf("R2: %.4g",Correlation^2), "\n")
            if (Fitted.RS && rtest$p.value < 0.01) {
                cat("The residuals are not constant. RSS are fitted with a general Additive model (k=5) see in returned matrix. \n")
            } else
            {
                cat("The residuals are constant. RSS is calculated with equation for constant residuals:")
                cat(sprintf("RSS: %.4g ", Mat$RSS[1]), "\n")
            }
            cat(sprintf("RMSE : %.4g ",rmse), "\n")
            cat(sprintf("mbe  : %.4g ",mbe), "\n")
            cat(sprintf("CRMSE: %.4g ",CRMSE), "\n")
            cat(sprintf("NMSD : %.4g ",NMSD), "\n")
            cat(sprintf("n    : %.4g ",nb), "\n")
            calib <- list(mo,sdo, mm,sdm, b1, ub1, b0, ub0, RSS, rmse, mbe, Correlation, nb, CRMSE, NMSD, Mat)
            # resuming the par values
            on.exit(par(mar=op))
        } else{
            cat("The Mat dataFrame is empty. Returning NAs.")
            calib <- list(mo = NA,sdo = NA, mm = NA,sdm = NA, b1 = NA, ub1 = NA, b0 = NA, ub0 = NA, RSS = NA,rmse = NA, mbe = NA, Correlation = NA, nb = NA, CRMSE = NA, NMSD = NA, Mat = NA)
        }
    } else {
    }
    # Creating the Directory to save plots
    Dir <- file.path(c(Disk, WD , Dir)[which(!sapply(list(c(Disk, WD , Dir)), is.na))])
    if (!dir.exists(Dir)) dir.create(Dir, showWarning = TRUE, recursive = TRUE)
    # saving the original par values in case they would be modified in this function
    op <- par(no.readonly = TRUE)
    # Passing and resuming the par values
    on.exit(par(op))
    par(mar = c(4,4,2,0.5))
    par(new = FALSE)
    #-----------------[1]----------------------
    # Plots scatterplot of orthogonal regression
    #------------------------------------------
    Gamme <- Etalonnage(x          = calib$Mat[["xis"]],
                        s_x        = NULL,
                        y          = calib$Mat[["yis"]],
                        s_y        = NULL,
                        AxisLabelX = Xlabel,
                        AxisLabelY = Ylabel,
                        Title      = paste0("Orthogonal regression for sensor ", Sensor_name, " versus reference data") ,
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
    plot(calib$Mat[["xis"]], calib$b1 * calib$Mat[["xis"]] + calib$b0,
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
                  calib$b0,
                  calib$b1,
                  calib$Correlation^2,
                  #sd(b1 * Mat$xis + b0 - Mat$yis),
                  calib$rmse),
          line = 1,
          adj  = 1,
          padj = 0,
          col  = "red",
          cex  = 0.875)
    # Saving the plot of orthogonal regression
    dev.copy(png,filename = file.path(Dir,paste0(nameModel, "_Scatter.png")), units = "cm", res = 300, width = 22, height = 22);
    dev.off()
    #-----------------[2]----------------------
    # Plots square of absolute residuals versus xis
    #------------------------------------------
    if (!is.null(Units)) Ylab = paste0("Square of Residuals in (", Units,")^2") else Ylab = "Square of Residuals"
    gamme <- Etalonnage(x          = calib$Mat[["xis"]],
                        s_x        = NULL,
                        y          = calib$Mat[["RS"]],
                        s_y        = NULL,
                        AxisLabelX = Xlabel,
                        AxisLabelY = Ylab,
                        Title      = paste0("Square of residuals of orthogonal regression for sensor ", Sensor_name)  ,
                        Marker     = 19,
                        Couleur    = 'blue',
                        lim        = NULL,
                        XY_same    = FALSE,
                        digitround = NULL,
                        marges     = NULL,
                        PlotAxis   = "s"
    )
    # if  the square of residuals are fitted
    if (calib$RS.Fitted) {
        # plotting the line of the regression of the square of residuals
        # Overlay new plot
        par(new = TRUE)
        # Set same margin for the line and Etalonnage
        par(mar=c(gamme[,"mar12"],gamme[,"mar34"]))
        # Plotting orthogonal regression line
        order.xis <- order(calib$Mat$xis)
        plot(x    = calib$Mat[["xis"]][order.xis],
             y    = calib$Mat[["RS"]][order.xis],
             type = "l",
             col  = "red",
             xlim = gamme[,"Xlim"],
             ylim = gamme[,"Ylim"],
             axes = FALSE ,
             xlab = "",
             ylab = "")
        mtext("The correlation is significant. Fitting a Generalized additive model (k = 5)",
              line = 1, adj  = 1, padj = 0, col  = "red", cex  = 0.875)
    } else {
        mtext("The sum of square residual (RSS) set to constant, likely because the correlation between x and the squares of residuals is not significant",
              line = 1, adj  = 1, padj = 0, col  = "red", cex  = 1)
    }
    # saving plots of squares of residuals with fitted model
    dev.copy(png,filename = file.path(Dir,paste0(nameModel, "_SqrRes.png")), units = "cm", res = 300, width = 22, height = 22);
    dev.off()
    #-----------------[3]----------------------
    # Plots Expanded Uncertainty
    #------------------------------------------
    if (!is.null(Units)) Ylab = paste0("Expanded uncertainty in ", Units) else Ylab="Expanded uncertainty"
    order.xis <- order(calib$Mat$xis)
    plot(calib$Mat[["xis"]][order.xis],
         calib$Mat[["U"]][order.xis],
         xlab=Xlabel,
         ylab = Ylab,
         main=Title ,
         col='blue',
         type = "l",
         ylim= c(0, max(calib$Mat$U, na.rm = T))
    )
    if (!is.na(LV)) {
        abline(v=LV)
        text(x = LV,
             y = 0 + 0.05 * (max(calib$Mat[order.xis, "U"], na.rm = T) - 0),
             labels = "LV")
    }
    if (!is.na(DQO.1)) {
        abline(h=DQO.1) # in ppb
        text(x = min(calib$Mat[order.xis, "xis"], na.rm = T) + 0.05 * (max(calib$Mat[order.xis, "xis"], na.rm = T) - min(calib$Mat[order.xis, "xis"], na.rm = T)),
             y = DQO.1,
             labels = "DQO.1")
    }
    # UAT = 0.35 /1.91 * 78/46
    # LAT = 0.25 /1.91 * 78/46
    # lines(Mat[Mat$xis> LAT, "xis"], Mat[Mat$xis> LAT, "xis"]*DQO.1, col="black", lwd=3)
    # lines(Mat[Mat$xis< LAT, "xis"], rep(LAT*DQO.1, length.out = length(Mat[Mat$xis< LAT, "xis"])), col="black", lwd=3)
    # text(5, LAT*DQO.1+2, "Class 1")
    # lines(Mat[Mat$xis< UAT, "xis"], rep(UAT*DQO.1, length.out = length(Mat[Mat$xis< UAT, "xis"])), col="black")
    # text(5, UAT*DQO.1+2, "Class 2")
    grid(nx = NULL, ny = NULL, lty = 2, col = "grey")
    dev.copy(png,filename = file.path(Dir,paste0(nameModel,"_U.png")), units = "cm", res = 300, width = 22, height = 22);
    dev.off()
    return(calib)
}
#================================================================CR
### U_orth_DF: Function Orthogonal regression without plotting (Vs 180505) ====
#================================================================CR
#' Function Orthogonal (or Deming or OLS) regression for computing measurement uncertainty without plotting
#'
#' @param Mat : Data.table or DataFrame of data including Case number, Date, x, y + optional ubsRM and/or ubss if ubsRM and/or ubss are not constant for all xi.
#' if ubsRM and/or ubss are given in Mat, even if perc.ubsRM is given the column in Mat for ubsRM will be used.
#' #' The columns shall be in the order: "case", "Date", "xis", "yis","ubsRM", "ubss" with whatever column names.
#' xis cannot be 0, otherwise the function crashes due to Ur divided by 0.
#' @param ubsRM : numeric (default = NULL ), random standard uncertainty of reference measurements xis, given as a constant value for all xis reference values.
#' @param variable.ubsRM : logical, default is FALSE. If FALSE, ubsRM is used as constant random standard uncertainties for all xis reference values. If TRUE ubsRM can be given in Mat and is used for each raw of Mat xis or as a percentage (perc.ubsRM).
#' @param perc.ubsRM : numeric default value 0.02. Use to compute ubsRm in case variable.ubsRM is TRUE and ubsRM is not included in Mat. In this case, Mat$ubsRM = perc.ubsRM * Mat[["xis"]] 
#' @param ubss : numeric (default = NULL ), random standard uncertainty of sensor measurements, yis, given as a constant value for all yis sensor values
#' @param variable.ubss : logical, default is FALSE. If FALSE, ubss is used as constant random standard uncertainties for all yis sensor values. If TRUE ubss given in Mat and is used for each sensor value
#' @param perc.ubss : numeric default value 0.03. Use to compute ubss in case variable.ubss is TRUE as Mat$ubss = perc.ubss * Mat[["yis"]] 
#' @param Fitted.RS Optional, logical, default is FALSE. If TRUE the square residuals (RSi) are fitted using a General Additive Model, provided that the null hypothesis of no correlation between xis and RSi is rejected when the probability is lower than 0.05, (p < 0.05)
#' @param Forced.Fitted.RS : logical, default is FALSE. If TRUE even if the variance of residuals is constant, RS is Gam fitted.
#' @param Regression character, default is "Orthogonal", possible values are "OLS" ,"Deming" and "Orthogonal". For Orthogonal Delta  is 1 and for "Deming" Delta is ubss^2/ubsRM^2. See https://en.wikipedia.org/wiki/Deming_regression
#' @param Add.ubss : logical, default is TRUE If TRUE ubss is added to  Mat$Rel.RSS. If FALSE ubss is not added to  Mat$Rel.RSS.
#' @param Verbose : logical, default is FALSE. If TRUE messages are displayed during execution.
#' @param Versus character, default is NUL. If not NULL name of the column in data.table Mat which is used with the gam fitting to fit RSi. If NULL, RSi will befitted versus reference data (xis). 
#' @details: 
#' The Homogeneity of variance of residuals is tested for the computation of RSi adding Ur in a new column of Mat

#' @return a list with parameters: "mo","sdo", "mm","sdm", "b1", "ub1", "b0", "ub0", "RSS","rmse", "mbe", "Correlation", "nb", "CRMSE", "NMSD", "RS.Fitted", "Regression", "Add.ubss" and a data.table called "Mat" with columns: "case", "Date", "xis", "yis","ubsRM", "RS", "Ur", "U", "Rel.bias", "Rel.RSS"
#' returning a list with slope (b and ub), intercept (a and ua), the sum of square of residuals (RSS),
#' the root means square of error (RMSE), the mean bias error (mbe), the coefficient of correlation (Correlation),
#' the number of valid measurements (nb), the centered root mean square of error (CRMSE), the normalised mean standard deviation (NMSD) and Mat with (relative) expanded measurement uncertainty.
#' The list also include the parameters of computation: "RS.Fitted", "Regression" and "Add.ubss".
#' Negative Rel.RSS are set to 0.

#' @examples: empty
U_orth_DF <- function(Mat, variable.ubsRM = FALSE, ubsRM = NULL, perc.ubsRM = 0.02, ubss = NULL, variable.ubss = FALSE, perc.ubss = 0.05,
                      Fitted.RS = FALSE, Forced.Fitted.RS = FALSE, Regression = "Orthogonal", Verbose = FALSE, Add.ubss = TRUE, Versus = NULL) {
    #checking that the Mat is not empty
    if (exists("Mat") && !is.null(Mat) && nrow(Mat)>0) {
        
        # Setting Versus with xis if NULL
        if (is.null(Versus)) Versus <- "xis"
        
        # Setting column names and adding ubsRM and ubss as constant values
        colnames(Mat) <- c("case", "Date", Versus, "yis","ubsRM", "ubss")[1:length(colnames(Mat))]
        # Checking data are numeric
        not.numeric <- c( Versus, "yis","ubsRM", "ubss")[sapply(c( Versus, "yis","ubsRM", "ubss"), function(i) !is.numeric(Mat[[i]]))]
        if(length(not.numeric > 0)) data.table::set(Mat, j = not.numeric, value = lapply(not.numeric, function(i) as.numeric(Mat[[i]])))
        rm(not.numeric)
        
        # Convert Mat to data.table if needed, order on versus
        if (!"data.table" %in% class(Mat)) Mat <- data.table(Mat)
        data.table::setkeyv(Mat, Versus)
        
        # Filtering for the validation data only
        Mat <- Mat[is.finite(rowSums(Mat[, c(Versus,"yis"), with = FALSE]))]
        if (!nrow(Mat) > 0) return(futile.logger::flog.error("[U_orth_DF] Mat does not contains any complete rows with xis and yis"))
        
        # Setting ubsRM and ubss in Mat
        if (!variable.ubsRM) {
            stopifnot(!is.null(ubsRM))
            data.table::set(Mat,  j = "ubsRM", value = rep(ubsRM, nrow(Mat))) 
        } else if (!"ubsRM" %in% names(Mat)) {
            Mat[, ubsRM := perc.ubsRM * Mat[[Versus]]]
            if (Verbose) futile.logger::flog.info("[U_orth_DF] computing u(bs,RM) as ", 100 * perc.ubsRM," percents of reference data (Mat$xis).")
        }  else if (Verbose) futile.logger::flog.info(paste0("[U_orth_DF] using u(bs,RM) provided in Mat."))
        if (Add.ubss){
            if (!variable.ubss) {
                stopifnot(!is.null(ubss))
                data.table::set(Mat,  j = "ubss", value = rep(ubss, nrow(Mat)))
            } else if (!"ubss" %in% names(Mat)) {
                Mat[, ubss := perc.ubss * Mat$yis]
                if (Verbose) futile.logger::flog.info("[U_orth_DF] computing u(bs,s) as ", 100 * perc.ubss," percents of sensor data (Mat$xis).")
            } else if (Verbose) futile.logger::flog.info(paste0("[U_orth_DF] using u(bs,s) provided in Mat."))}
        
        # Determination of delta for Deming or orthogonal regression
        if (Regression == "Deming") {
            if (variable.ubsRM || variable.ubss) {
                return(futile.logger::flog.error("[U_orth_DF] regression cannot be set to \"Deming\" with variable u(bs,RM) and/or variable u(bs,s)"))
            } else if (is.null(ubsRM) || is.null(ubsRM)) {
                return(futile.logger::flog.error("[U_orth_DF] with \"Deming\" regression both u(bs,RM) and u(bs,s) cannot be null"))
            } else {
                Delta <- (ubss/ubsRM)^2
                if (Verbose) futile.logger::flog.info(paste0("[U_orth_DF] regression type: \"",Regression, "\" with Delta = ", Delta))}
        } else if (Regression == "Orthogonal") {
            Delta <- 1
            if (Verbose) futile.logger::flog.info(paste0("[U_orth_DF] regression type: \"",Regression, "\" (Delta = ", Delta,")"))
        } else if (Regression != "OLS") return(futile.logger::flog.error("[U_orth_DF] unknown regression type. Only \"OLS\",  \"Deming\" (Delta is ubss^2/ubsRM^2) or \"orthogonal\" (Delta is 1) regressions can be used"))
        
        # Common parameters
        nb  <- nrow(Mat)
        mo  <- mean(Mat[[Versus]])
        mm  <- mean(Mat[["yis"]])
        sdo <- sd(Mat[[Versus]])
        sdm <- sd(Mat[["yis"]])
        Sxx <- sum((Mat[[Versus]] - mo)^2)
        Syy <- sum((Mat[["yis"]] - mm)^2)
        Sxy <- sum((Mat[[Versus]] - mo) * (Mat[["yis"]] - mm))
        # Linear Regression Orthogonal regression ()
        if (Regression == "Orthogonal") {
            # as in annex b of Guide for The Demonstration of Equivalence
            m2  <- MethComp::Deming(x = Mat[[Versus]], y = Mat[["yis"]], vr = Delta)
            b1  <- (Syy - Sxx + sqrt((Syy- Sxx)^2 + 4*Sxy^2))/(2*Sxy)
            b0  <- mm - b1 * mo
            ub1 <- sqrt((Syy - (Sxy^2/Sxx))/((nb-2)*Sxx))
            ub0 <- sqrt(ub1^2 * sum(Mat[[Versus]]^2)/nb)
        } else if (Regression == "Deming"){
            #tls::tls(yis ~ xis, data = Mat[, .(xis, yis)])
            m2  <- MethComp::Deming(x = Mat[[Versus]], y = Mat[["yis"]], vr = Delta)
            b0  <- m2[1] 
            b1  <- m2[2]
        } else if (Regression == "OLS") {
            # b1 = Sxy/Sxx
            # b0 = mm - b1 * mo
            # Syx = sqrt(sum((Mat[["yis"]] - (b0 + b1 * Mat[[Versus]]))^2)/(nrow(Mat)-2))
            # ub = Syx/sqrt(Sxx)
            # ua = Syx * sqrt(sum((Mat[[Versus]]^2)/(nrow(Mat) * Sxx)))
            Formula <- as.formula(paste0("yis ~ ", Versus))
            m2 <- lm(Formula, data = Mat)
            b0  <- coef(m2)[1] 
            b1  <- coef(m2)[2]
            st.errors <- sqrt(diag(vcov(m2)))
            ub0 <- st.errors[1]
            ub1 <- st.errors[2]}
        
        # Regression statistics for Target Diagram (see delta tool user guide)
        rmse  <- sqrt((sum((Mat[["yis"]] - (b0 + b1 * Mat[[Versus]]))^2))/(nb-2))
        mbe   <- mean(Mat[["yis"]] - Mat[[Versus]])
        mae   <- mean(abs(Mat[["yis"]] - Mat[[Versus]]))
        CRMSE <- sqrt(mean(((Mat[["yis"]] - mm) - (Mat[[Versus]] - mo))^2))
        NMSD  <- (sd(Mat[["yis"]]) - sd(Mat[[Versus]])) / sd(Mat[[Versus]])
        Correlation <- cor(Mat[[Versus]],Mat[["yis"]])
        # Squares of Residuals and bias (vector of values)
        Mat[, fitted := (b0 + b1 * Mat[[Versus]])]
        Mat[,   bias := (b0 + (b1 - 1) * Mat[[Versus]])] # Bias from identity line x
        Mat[, residuals := (Mat[["yis"]] - Mat[["fitted"]])]
        Mat[,     RS := (Mat[["yis"]] - Mat[["fitted"]])^2]
        # # creating an OLS model to apply Breusch Pagan test and to compute ub0 and ub1
        Formula <- as.formula(paste0("yis ~ ", Versus))
        OLS <- lm(Formula, data = Mat)
        OLS$coefficients  <- c(b0,b1)
        OLS$residuals     <- Mat$residuals
        OLS$fitted.values <- Mat$fitted
        if (Regression == "Deming") {
            # https://bookdown.org/ccolonescu/RPoE4/heteroskedasticity.html
            ub0 <- lmtest::coeftest(OLS, vcov. = car::hccm(OLS, type = "hc1"))[1, "Std. Error"]
            ub1 <- lmtest::coeftest(OLS, vcov. = car::hccm(OLS, type = "hc1"))[2, "Std. Error"]}
        
        # testing for heteroscedacity with Breusch Pagan test
        # https://www.r-bloggers.com/2016/01/how-to-detect-heteroscedasticity-and-rectify-it/
        Breusch.Pagan     <- lmtest::bptest(formula = OLS)
        skedastic::breusch_pagan(OLS)
        # testing significance of correlation between s and square of absolute residuals - The calculation does not work only possibility the constrant RSS
        rtest <- Breusch.Pagan
        
        # if fitting the residuals is needed, ordering before
        if (Verbose) futile.logger::flog.info("[U_orth_DF] Breusch-Pagan: null hypothesis means constant variance of residuals along x axis (homoscedacity). The hyposthesis is rejected if p-value < 0.05 (heteroscedacity)")
        if (Verbose) futile.logger::flog.info(paste0("[U_orth_DF] and the residuals would be heteroscedastic with 0.95 level of statistical confidence. Finally, p-value =  ", format(rtest$p.value, digits = 4)))
        if (!is.na(rtest$p.value) && Fitted.RS && (rtest$p.value < 0.05 || Forced.Fitted.RS)) {
            if (Verbose) futile.logger::flog.info("[U_orth_DF] The variance of residuals is not constant. RSi are calculated after applying a General Additive Model fitting.")
            # Fitting with gam Vs Versus ("Xi")
            # if any y value is zero getting Warning: Error in eval: non-positive values not allowed for the 'gamma' family (we had 0.5 % of min(xis) to avoid this
            
            Formula <- as.formula(paste0("sqrt(RS) ~ s(", Versus, ")"))
            z <- mgcv::gam(Formula, data = Mat,family=Gamma(link=log) )
            
            # # see https://stats.stackexchange.com/questions/270124/how-to-choose-the-type-of-gam-parameters
            # z <- mgcv::gam(Formula, data = Mat, method = "REML", select = TRUE)
            
            Mat[, RS := fitted(z)^2]
            # Sum of squares of Residuals (one constant value)
            RSS     <- sum(Mat$RS)
            
            if (Verbose) print(summary(z))
        } else {
            if (Verbose) futile.logger::flog.info("[U_orth_DF] The variance of residuals is constant. Constant RSS is calculated.")
            if (Fitted.RS) {
                if (Verbose) futile.logger::flog.info(paste0("[U_orth_DF] Argument \"Fitted.RS\" in U_orth_DF(): ", Fitted.RS, ". If FALSE the square residuals are not fitted and constant RSS is computed."))
                if (Verbose) futile.logger::flog.info(paste0("[U_orth_DF] if Breusch-Pagan lagrange multiplier statistic p-value > 0.05, residuals are homoscedastic with 0.95 level of statistical confidence. p -value = ",
                                                             format(rtest$p.value, digits = 4)))}
            if (Verbose) futile.logger::flog.info("[U_orth_DF] RSS is calculated with equation for constant residuals.")
            # Sum of squares of Residuals (one constant value)
            RSS     <- sum(Mat$RS)
            if (Verbose) futile.logger::flog.info(paste0("[U_orth_DF] RSS is the square root of sum of squares of Residuals divided by n - 2 = ", format(sqrt(RSS/(nb-2)), digit = 3)))
            # No need to fit a line in this case
            Mat[, RS := rep(RSS/(nb-2), times = .N)]}
        
        # Plotting RS
        if (Verbose) {
            # See https://stackoverflow.com/questions/17093935/r-scatter-plot-symbol-color-represents-number-of-overlapping-points
            
            ## Use densCols() output to get density at each point
            x <- grDevices::densCols(Mat[[Versus]],sqrt(Mat$residuals^2), colramp=colorRampPalette(c("black", "white")))
            Mat$dens <- col2rgb(x)[1,] + 1L
            ## Map densities to colors
            cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", 
                                        "#FCFF00", "#FF9400", "#FF3100"))(256)
            
            Mat$col <- cols[Mat$dens]
            
            plot(sqrt(Mat$residuals^2) ~ get(Versus), data = Mat, type = "p", col = col, xlab = Versus, main = paste0("RS between", paste(range(Mat$Date), sep = " and ")))
            lines(Mat[[Versus]],sqrt(Mat[["RS"]]), col = "red", xlab = Versus); grid()}
        
        # Checking if RSS^2 - Mat$ubsRM^2 + Mat$ubss^2 < 0 that results in an error using sqrt(RSS^2 - Mat$ubsRM^2) of the rs.RSS. Replacing with 0
        # and Calculating parameters for modified Target diagram Rel.bias and Rel.RSS
        if (Add.ubss) neg.RSS <- which(Mat$RS - Mat[["ubsRM"]]^2 + Mat[["ubss"]]^2 < 0) else neg.RSS <- which(Mat$RS - Mat[["ubsRM"]]^2 < 0)
        if (length(neg.RSS) > 0) {
            if (Verbose) futile.logger::flog.warn("[U_orth_DF] Some \"RS - ubsRM^2\" are negative and square roots cannot be calculated.")
            if (Verbose) futile.logger::flog.info("[U_orth_DF] ubsRM maybe too high and could be modified.")
            if (Verbose) futile.logger::flog.info("[U_orth_DF] The \"RS - ubsRM^2\" that are negative will be set to 0 when computing uncertainties.")
            
            Mat[neg.RSS,  Rel.RSS := 0]
            Positives <- setdiff(1:nrow(Mat),neg.RSS)
            
            if (length(Positives) > 0){
                if (Add.ubss){
                    Mat[Positives, Rel.RSS := 2 * sqrt(Mat[Positives,ubss]^2 + Mat[Positives,RS] - Mat[Positives,ubsRM]^2) / Mat[Positives][[Versus]]]
                } else Mat[Positives, Rel.RSS := 2 * sqrt(Mat[Positives,RS] - Mat[Positives,ubsRM]^2) / Mat[Positives][[Versus]]]}
        }  else {
            # mat$RS are not changed and they are already calculated
            if (Verbose) futile.logger::flog.info("[U_orth_DF] All \"RSS/(nb - 2) or RSi - ubsRM^2\" are positives. ubsRM makes sence.")
            
            if (Add.ubss){
                Mat[, Rel.RSS := 2 * sqrt(Mat$ubss^2 + Mat$RS - Mat$ubsRM^2) / Mat[[Versus]]]
            } else Mat[, Rel.RSS := 2 * sqrt(Mat$RS - Mat$ubsRM^2) / Mat[[Versus]]]
        }
        Mat[, Rel.bias := 2 * (b0/Mat[[Versus]] + (b1 - 1))]
        
        #### Calculating uncertainty
        Mat[, Ur := sqrt(Mat$Rel.bias^2 + Mat$Rel.RSS^2) * 100]
        Mat[, U  := Mat$Ur / 100 * Mat[[Versus]]]
        
        # Indicators for ubsRM
        Mat[, Max.ubsRM := sqrt((Mat$Rel.RSS * Mat[[Versus]] / 2)^2 + Mat$ubsRM^2 + Mat$bias^2)]
        Mat[, Max.RSD   := Max.ubsRM / Mat[[Versus]]]
        
        # Printing
        if (Verbose) {
            cat("--------------------------------\n")
            cat(sprintf("mean of x   : %.1g +/- %.1g",mo,sdo),"\n")
            cat(sprintf("mean of y   : %.4g +/- %.4g",mm,sdm),"\n")
            cat(sprintf("b1 +/- u(b1): %.4g +/- %.4g",b1,ub1),"\n")
            cat(sprintf("b0 +/- u(b0): %.4g +/- %.4g",b0,ub0),"\n")
            cat(sprintf("R2: %.4g",Correlation^2), "\n")
            if (!is.na(rtest$p.value) && Fitted.RS && (rtest$p.value < 0.05 || Forced.Fitted.RS)) {
                cat("The residuals are not constant. RS are fitted with a general Additive model (k=5) see in returned matrix. \n")
            } else {
                cat("The residuals are constant. RSS is calculated with equation for constant residuals:")
                cat(sprintf("RSS: %.4g ",Mat$RSS[1]), "\n")}
            cat(sprintf("RMSE : %.4g ",rmse), "\n")
            cat(sprintf("mbe  : %.4g ",mbe), "\n")
            cat(sprintf("CRMSE: %.4g ",CRMSE), "\n")
            cat(sprintf("NMSD : %.4g ",NMSD), "\n")
            cat(sprintf("n    : %.4g ",nb), "\n")}
        calib <- list(mo,sdo, mm,sdm, b1, ub1, b0, ub0, RSS, rmse, mbe, Correlation, nb, CRMSE, NMSD, Mat, Fitted.RS, Regression, Add.ubss = Add.ubss)
    } else {
        cat("Mat is empty. Returning NAs.")
        calib <- list(mo = NA,sdo = NA, mm = NA,sdm = NA, b1 = NA, ub1 = NA, b0 = NA, ub0 = NA, RSS = NA,rmse = NA, mbe = NA, Correlation = NA, nb = NA,
                      CRMSE = NA, NMSD = NA, Mat = NA, RS.Fitted = Fitted.RS, Regression = Regression, Add.ubss = Add.ubss)}
    names(calib) <- c("mo","sdo", "mm","sdm", "b1", "ub1", "b0", "ub0", "RSS","rmse", "mbe", "Correlation", "nb", "CRMSE", "NMSD", "Mat", "RS.Fitted", "Regression", "Add.ubss")
    
    # if (Verbose) print(data.frame(x              = Mat[[Versus]],
    #                               ubsRM          = Mat$ubsRM,
    #                               Max.ubsRM      = sqrt(RSS/(nb-2) + Mat$bias^2),
    #                               Max.RSD        = sqrt(RSS/(nb-2) + Mat$bias^2)/Mat[[Versus]],
    #                               Decrease.ubsRM = (Mat$ubsRM - sqrt(RSS/(nb-2) + Mat$bias^2)) > 0))
    
    return(calib)
}
#================================================================CR
### f_Kohler: Function for Fitting a Kohler model ====
#================================================================CR
f_Kohler <- function(x, a0, a1, K, RH) {
    C <- 1 + (K/1.65/(100/RH - 1))
    return(a0 + a1 * x * C)
    #return(a0 + a1 * (x * (1 + (K/1.65)/((RH/100)^-1 - 1))))
    # Sensors 2018, 18(9), 2790; https://doi.org/10.3390/s18092790
    # is no information regarding the efflorescence point of the compound with K = 0.4, we have assumed it to be the same as Ammonium Sulphate (RH = 35%). Also shown are particle volumes as functions of particle size for
}
f_Kohler_only <- function(x, K, RH) {
    C <- 1 + (K/1.65/(100/RH - 1))
    return((x * C))
    # Sensors 2018, 18(9), 2790; https://doi.org/10.3390/s18092790
    # is no information regarding the efflorescence point of the compound with K = 0.4, we have assumed it to be the same as Ammonium Sulphate (RH = 35%). Also shown are particle volumes as functions of particle size for
}
f_Kohler_modified <- function(x, a0, a1, K, RH) {
    C <- 1 + (K/1.65/(100/RH - 1))
    return((a0 + a1 * x) * C)
    # Sensors 2018, 18(9), 2790; https://doi.org/10.3390/s18092790
}
#================================================================CR
### f_log: Function Fitting a logarithmic model ====
#================================================================CR
f_log <- function(x, a, b) {
    return(y = a + b * log(x))
    # Increases without bound to right
    # Passes through (1,a),
    # Very rapid growth, followed by slower growth,
    # Common log will grow slower than natural log
    # b controls the rate of growth
}
#================================================================CR
### f_Unitec: Function Fitting a the Unitec model ====
#================================================================CR
f_Unitec <- function(x, a, b,c) {
    # mVolt <- ((y-a)/b)^(1/c)
    return(y = (((x*1.91*(293.15/TK))-a)/b)^(1/c))
    # Y(?g/m3) (*1.91) = a+b*X(mV)^c
    # with a=-31.6  b=5330.9  -0.598, x en mV	min(x)=47.2	max(x)=5299.9	unitY=?g/m3	ymax=500?g/m3
    # Equation of Unitec
}
#================================================================CR
### f_Michelis: Function Fitting a Michaelis-Menten kinetics model with intercept ====
#================================================================CR
f_Michelis <- function(x, Vmax, km, intercept) {
    # Vmax is the max asymtoptic value on y axis
    # intercept is the min asymtoptic value on y axis
    # km is The Michaelis constant is the concentration at which the sensor response is half of V_\max
    return(Vmax * x / (km  + x) + intercept)
}
#================================================================CR
### f_ExpGrowth: Function Fitting an Exponential Growth model without intercept ====
#================================================================CR
#' @title Fitting exponential growth model
#' @description The following function fit a model to x data of the form y = C exp(kx), k > 0 see https://people.richland.edu/james/lecture/m116/logs/models.html
#' @param x numeric values on x axis
#' @param C numeric, C is the initial value when x = 0
#' @param k numeric rate of exponential growth
#' @details Features: Asymptotic to y = 0 to left, passes through (0,C), C is the initial value
f_ExpGrowth <- function(x, C, k) {
    return(C * (exp(k*x)))}

f_Exp_Translations <- function(x, C, k, x0) {
    # https://courses.lumenlearning.com/waymakercollegealgebra/chapter/horizontal-and-vertical-translations-of-exponential-functions/
    # k > 0 or y = k^(x-x0) + C
    # Features
    # Asymptotic to y = C to left, C is the vertical translation positive or negative (initial value)
    # at x = x0, y = C + 1, passes through (x0, C + 1)
    return(k^(x-x0) + C)}

f_exp_kT <- function(x, a0, a1, C, k, Temperature) {
    # https://people.richland.edu/james/lecture/m116/logs/models.html
    # y = a0 + a1 x + C exp(kT), k > 0
    # Features
    # Asymptotic to y = 0 to left
    # Passes through (0, a0 + C)
    # C is the initial value
    return(a0 + a1 * x + C * exp(k * Temperature) )
}
f_exp_kTn <- function(x, a0, a1, C, Temperature,n) {
    # https://people.richland.edu/james/lecture/m116/logs/models.html
    # y = a0 + a1 x + C exp(T^n)
    # Features
    # Asymptotic to y = 0 to left
    # Passes through (0, a0 + C)
    # C is the initial value
    return(a0 + a1 * x + C * exp(Temperature^n) )
}
f_exp_kT_NoC <- function(x, a1, C, k, Temperature) {
    # https://people.richland.edu/james/lecture/m116/logs/models.html
    # y = a1 x + C * e(kT), k > 0
    # Features
    # Asymptotic to y = 0 to left
    # Passes through (0,C)
    # C is the initial value
    return(a1 * x + C * exp(k * Temperature))
}
f_exp_kT_NoC_log.min <- function(x, a1, C, k, Temperature, log.min) {
    # https://people.richland.edu/james/lecture/m116/logs/models.html
    # y = a1 x + C * e(kT + log.min), k > 0
    # Features
    # Asymptotic to y = 0 to left
    # Passes through (0,C)
    # C is the initial value
    return(a1 * x + C * exp(k * Temperature) + log.min)
}
f_T_power <- function(x, a0, a1, a2, n, Temperature) {
    # https://people.richland.edu/james/lecture/m116/logs/models.html
    # y = a0 + a1 x + a2 T^1.75
    # Features
    # Asymptotic to y = 0 to left
    # Passes through (0,C)
    # C is the initial value
    return( a0 + a1 * x + a2 * Temperature^n)
}
#================================================================CR
### f_NO2Lab: Function for Fitting a NO2 model based on the lab experiments ====
#================================================================CR
f_NO2_Lab <- function(x, a0, a1, a2, relative_humidity, a3, k, Temperature){
    # y = a0 + a1 x + a2 RH - a3 exp(kT)
    return(a0 + a1 * x + a2 * relative_humidity - a3 * exp(k * Temperature))
}
f_NO2_Lab_decay_inc <- function(x, a0, a1, a2, relative_humidity, a3, k, Temperature, T0){
    # y = a0 + a1 x + a2 RH - exp(a3 * (1 - exp(k(T-T0))))
    return(a0 + a1 * x + a2 * relative_humidity - exp(a3 * (1 - exp(k * (Temperature - T0)))))
}
#================================================================CR
### f_BeerLambert: Beer lambert Law for CO2 sensprors ====
#================================================================CR
f_BeerLambert <- function(x, a0, a1, a2, a3, n, Temperature, Atmospheric_pressure){
    # log(I1/I0) = O3 (sigma na L/R) Temperature^n / pressure + a0
    # a1 is a1 (sigma na L/R)
    # y = a0 + a1 x * Tempe/ pressure 
    return( a0 + a1 * x * (Temperature + a2)^n / (Atmospheric_pressure + a3))
}
f_BeerLambert2 <- function(x, a0, aT, n, Temperature, Atmospheric_pressure){
    # log(I1/I0) = O3 (sigma na L/R) Temperature^n / pressure + a0
    # a1 is a1 (sigma na L/R)
    # y = a0 + a1 x * Tempe/ pressure
    return( a0 + x * (Temperature/aT)^n / (Atmospheric_pressure))}
#================================================================CR
### f_ExpDD_Int: Function Fitting an exponential Decay (Decreasing form) model with intercept ====
#================================================================CR
f_ExpDD_Int <- function(x, C, k,intercept) {
    # https://people.richland.edu/james/lecture/m116/logs/models.html
    # y = C e-kt + intercept, k > 0
    # Features
    # Asymptotic to y = intercept0 to right
    # Passes through (0,C+intercept)
    # C is the initial value
    # Decreasing, but bounded below by y=intercept
    return(C *(exp(-k*x)) + intercept)
}
#================================================================CR
### f_ExpDI: Function Fitting an exponential Decay (Increasing form) model ====
#================================================================CR
f_ExpDI <- function(x, C, k) {
    # https://people.richland.edu/james/lecture/m116/logs/models.html
    # y = C ( 1 - e-kx ), k > 0
    # Features
    # Asymptotic to y = C to right
    # Passes through (0,0)
    # C is the upper limit
    # Increasing, but bounded above by y=C
    return(C *(1-exp(-k*x)))
}
#================================================================CR
### f_ExpDI_Int: Function Fitting an exponential Decay (Increasing form) model with intercept (see wikipedia) ====
#================================================================CR
f_ExpDI_Int <- function(x, C, k,intercept) {
    # https://people.richland.edu/james/lecture/m116/logs/models.html
    # y = C ( 1 - e-kx + intercept), k > 0
    # Features
    # Asymptotic to y = C to right
    # Passes through (0,intercept)
    # C is the upper limit
    # Increasing, but bounded above by y = C
    # C is the max asymtoptic value on y axis
    # intercept is the min asymtoptic value on y axis
    return(C *(1-exp(-k*x)) + intercept)
}
#================================================================CR
### f_Sigmoid: Function Fitting a Logistic - Sigmoidal function ====
#================================================================CR
f_Sigmoid <- function(x, MIN, Asym, xmid, Hill) {
    #, Hill > 0, between 1a dn 10
    #Asymptotic to y = Max at right,
    #Asymptotic to y = Min at left,
    #Passes through (0, a/(1+b) )
    #x50 x value at the inflection point, in this equation x is not log transformed
    return(y = MIN + (Asym-MIN) / (1 + (xmid/x)^Hill))}
#================================================================CR
### f_normal: fitting normal distribution ====
#================================================================CR
f_normal <- function(x, mu, sigma) {
    #return the normal distribution in function of c, mu and sigma
    return( 1/(sqrt(2*pi * sigma^2)) * exp(-((x - mu)^2)/(2 * sigma^2)))}
#================================================================CR
### f_normal_density: fitting normal distribution (on logarithmic scale), This is for fitting a normal distribution on DMPS and APS ====
#================================================================CR
f_normal_density <- function(x, mu, sigma, Density = NULL, Diam_APS = NULL) {
    # x:        numeric vector, log decimal of all bin diameters in micrometer measured bith by the DPS and APS
    # mu:       numeric, the mean or expectation of the log normal distribution (and also its median and mode),
    # sigma:    numeric, the standard deviation of the log normal distribution
    # density:  density of particles (mean density?)
    # C         numeric: constant use to translate the distibution so that the values at left and rignt are qual to zero
    if (!is.null(Density)) {
        # index of diameters in x coming from the APS instrument to be corrected for the particle density
        # Diam_APS : numeric vector, diameter of the bins measured by the APS instrument in micrometer
        Which.APS <- which(x %in% log10(Dist_Ref.d$Diam_APS))
        #Correction of optical diameter, diving the dimeter in micrometer by sqrt(density). x must be given in log10(Diameter) at the end
        if (length(Which.APS) > 0) x[Which.APS] <- log10(10^(x[Which.APS])/sqrt(Density)) else stop("ERROR there are no APS diameter to correct for density")
    }
    # return(1/(sqrt(2*pi * sigma^2)  ) * exp(-((x - mu)^2) / (2 * sigma^2) ) )
    #Density, distribution function, quantile function and random generation for the normal distribution with mean equal to mean and standard deviation equal to sd.
    return(dnorm(x, mean = mu, sd = sigma))}
#================================================================CR
### f_log10normal_density: fitting decimal log normal distribution. This is for fitting a normal distribution on DMPS and APS ====
#================================================================CR
f_log10normal_density <- function(x, mu, sigma, Density = NULL) {
    # x:        numeric vector, all bin diameters in micrometer measured bith by the DPS and APS
    # mu:       numeric, the mean or expectation of the log normal distribution (and also its median and mode),
    # sigma:    numeric, the standard deviation of the log normal distribution
    # density:  density of particles (mean density?)
    if (!is.null(Density)) {
        # index of diameters in x coming from the APS instrument to be corrected for the particle density
        # Diam_APS : numeric vector, diameter of the bins measured by the APS instrument in micrometer
        Which.APS <- which(x %in% log10(Dist_Ref$Diam_APS))
        #Correction of optical diameter, diving the dimeter in micrometer by sqrt(density). x must be given in log10(Diameter) at the end
        if (length(Which.APS) > 0) x[Which.APS] <- log10(10^(x[Which.APS])/sqrt(Density)) else stop("ERROR there are no APS diameter to correct for density")
    }
    return( dlnorm(x, meanlog = mu, sdlog = sigma) )
}
#=================================================================================================CR
### f_Error: error function to minimize the differences between APS and DMPS count based on common diameters ====
#==================================================================================================CR
f_Error <- function(density, DataXY.APS, Model.i){
    DataXY.APS <- DataXY.APS %>% mutate(x = log10(10^x/sqrt(density)))
    DataXY.APS$Predicted_APS <- predict(Model.i$Model, DataXY.APS)
    return(sum(DataXY.APS$y - DataXY.APS$Predicted_APS)^2)}
#================================================================CR
### Fitting_distrib: fitting distribution to data ====
#================================================================CR
Fitting_distrib <- function(Air_Pol, ug, coeff_ppb, Dist_xlab, Dist_range, BandWidth, band, distribution, to_fit, lamb, q) {
    # fitting distribution manual Non Linear Regression based on the probability distribution funtion with only one parameter: the shape parameter
    # Air_Pol: variable for which the densit function is calculated
    # ug: if TRUE transform from ug/m3 to ppb or ppm if FALSE does not transform
    # coeff_ppb: used if ug = TRUE: conversion factor to divide  ug/m3 to get ppb (ex 1.91 for NO2)
    # Dist_xlab: Label of X axis of the distribution plot
    # Dist_range: min and max value for the distribution plot - NO WE USE QUANTILE of 99 %
    # BandWidth: if TRUE bandwidth is automatically estimated by the R's "density" function. if FALSE the coeff band is used (see below)
    # band : USed if bandwith is FALSE, If 0 then bw is set to "nrd0" (see help of density function). If anoother values is passed, the bandwidth is set to this value
    # distribution: type of distribution to be fitted: lognormal ...
    # to_fit: if TRUE fitted with nls, if FALSE: not fitted - value lamb is used for the shape factor
    # lamb: initial lambda value for the lognormal distribution
    # q the limit of possible data of log normal distribution
    # removing negative values of data
    # Air_Pol <- subset(Air_Pol, Air_Pol >0)
    # transforming in ppb
    if (ug) Air_Pol <- Air_Pol/coeff_ppb
    #-----------------------------------CR
    # calculating the reference density
    #-----------------------------------CR
    if (BandWidth == TRUE) {
        # calculating the band width of reference distribution
        h <- density(Air_Pol, kernel="gaussian")$bw
        # calculating weight that are higher at 0
        print(sprintf("Best bandwidth = %.3f", h), quote = FALSE)
        d_Air_Pol<- density(Air_Pol, bw = h, adjust = 1 , kernel = "gaussian", na.rm = TRUE) # returns the density data
    }
    else {
        if (band == 0) {
            d_Air_Pol<- density(Air_Pol, bw = "nrd0", adjust = 1 , kernel = "gaussian", na.rm = TRUE) # returns the density data
            h <- d_Air_Pol$bw
        }
        else {
            d_Air_Pol<- density(Air_Pol, bw = band, adjust = 1 , kernel = "gaussian", na.rm = TRUE) # returns the density data
            h <- d_Air_Pol$bw
        }
    }
    print(sprintf("Bandwith : %.6f",h))
    #-----------------------------------CR
    # Subtracting the minimum d_Air_Pol$x if negative - Defining "Air_Pol_final"
    #-----------------------------------CR
    if (min(d_Air_Pol$x, na.rm = T)<-10000) {
        Air_Pol_final <- Air_Pol - min(d_Air_Pol$x, na.rm = T) + 0.0001
        print(sprintf("minmum x_density: %.3f was subtracted",min(d_Air_Pol$x, na.rm = T)))
        d_Air_Pol<- density(Air_Pol_final, bw = h, adjust = 1, kernel = "gaussian", na.rm = TRUE) # returns the density data
        print(sprintf("minmum x_density: %.3f",min(d_Air_Pol$x, na.rm = T)))
    }
    else {
        print(sprintf("minmum x_density: %.1f was not added",min(d_Air_Pol$x, na.rm = T)))
        Air_Pol_final <- Air_Pol # do not add anything becasue the minimum value is not negative
    }
    #-----------------------------------CR
    ## density plot of d_Air_Pol - one parameter - reference density plot
    #-----------------------------------CR
    par(mar=c(5, 0.5, 0, 0.5))
    plot(d_Air_Pol, xlim = c(min(0,d_Air_Pol$x, na.rm = T),quantile(Air_Pol, probs = 0.98)), xlab = Dist_xlab, ylab = "", main = "", cex.lab=3, cex.axis=2.5)
    polygon(c(min(d_Air_Pol$x, na.rm = T),d_Air_Pol$x,quantile(Air_Pol, probs = 0.98)), c(0,d_Air_Pol$y,0), col="red", border="blue")
    #-----------------------------------CR
    # calculating the Mode of the data variable
    ## with the Fit of a log-normal distribution With MASS function fitdistr
    #-----------------------------------CR
    f_MASS <- fitdistr(Air_Pol_final, "log-normal")
    coef(f_MASS)
    M <- lnormMode(meanlog = coef(f_MASS)['meanlog'], sdlog = coef(f_MASS)['sdlog']) # need package modeest
    #transforamtion in log
    Log_Air_Pol_final <- log(Air_Pol_final)
    #-----------------------------------CR
    ## Histogram and density plots for the 2 parameters Lognormal distribution
    #-----------------------------------CR
    hist(Log_Air_Pol_final)
    mu <- coef(f_MASS)['meanlog']
    sigma <- coef(f_MASS)['sdlog']
    x <- seq( min(Air_Pol_final, na.rm = T), quantile(Air_Pol_final,probs = 0.98), length.out=250)
    y <- f_lnorm_2Par(x,mu,sigma)
    par(mar=c(5, 0,0, 0))
    plot(x,y, xlim = c(min(0,x,na.rm = T),max(x,na.rm = T)), xlab = Dist_xlab, ylab = "", main = "", type = "l", cex.lab=3, cex.axis=2.5)
    polygon(c(min(x),x,max(x)), c(0,y,0), col="red", border="blue")
    abline(v = M, lwd =2, col = "Black")
    print(sprintf("Mu (mean of log): %.3f", mu), quote = FALSE)
    print(sprintf("Sigma (sd of log): %.3f",sigma), quote = FALSE)
    print(sprintf("Mode: %.3f",exp(mu-sigma^2)), quote = FALSE)
    print(sprintf("Median: %.3f",median(x)), quote = FALSE)
    print(sprintf("u (adding the mode): %.3f",sqrt(exp(mu+1/2*sigma^2)^2 + exp(2*(mu+sigma^2)) - exp(2*mu+sigma^2))), quote = FALSE)
    # median
    m <- median(Air_Pol_final)
    #-----------------------------------CR
    # Fitting Distribution of NASA
    #-----------------------------------CR
    # if (distribution == "lognormal") {
    #
    #     # Estimating the Mode as the maximum of the density function - Document NASA
    #     M <- mlv(Air_Pol_final, method = "parzen", bw = h)
    #     # Preparing data for fitting the distribution without NA and negative values, adding weights according to distance from 0 to promote data at 0
    #     DataXY <- na.omit(data.frame(cbind(d_Air_Pol$y, d_Air_Pol$x, d_Air_Pol$y^-1)))
    #     colnames(DataXY) <- c('referenceY','x_to_fit', "w")
    #     DataXY$W <- d_Air_Pol$y^-1 / sum(d_Air_Pol$y^-1)
    #     # Fitting the distribution with non linear regression for NO2
    #     # initial value of lambda = 1.0
    #     if (to_fit) {
    #         fitted_Distribution <- nls(referenceY ~ f_lnormal_1par(x_to_fit, lambda, q, m)
    #                                    , data=DataXY, start=list(lambda = coef(f_MASS)['sdlog'])) # start=list(lambda = lamb)
    #         lambda <- coef(fitted_Distribution)
    #     }
    #     else {
    #         lambda <- lamb
    #     }
    #     DataXY$Fitted <- f_lnormal_1par(DataXY$x_to_fit, lambda, q, m)
    #     # if NA are created:
    #     DataXY <- na.omit(DataXY)
    #
    #     #-----------------------------------CR
    #     # calculating the Mode of the data variable
    #     ## with the Fit of a log-normal distribution With MASS function fitdistr
    #     #-----------------------------------CR
    #     f_MASS <- fitdistr(Air_Pol_final, "log-normal")
    #     coef(f_MASS)
    #     M <- lnormMode(meanlog = coef(f_MASS)['meanlog'], sdlog = coef(f_MASS)['sdlog']) # need package modeest
    #     # Plotting f_MASS
    #     par(mar=c(5, 0.5, 0, 0.5))
    #     curve(dlnorm(x, meanlog = coef(f_MASS)["meanlog"], sdlog = coef(f_MASS)["sdlog"]), from = 0, to = quantile(Air_Pol_final, probs = 0.98)
    #           , xlab = Dist_xlab, ylab = ""
    #           , main = "",cex.lab=3, cex.axis=2.5)
    #     abline(v = M, lwd =2, col = "Black")
    #     par(new=T)
    #
    #     # Printing characteristics of the lognormal distribution
    #     print(sprintf('Mode: M = %.3f', M), quote = FALSE)
    #     print(sprintf('Median: m = %.3f', m), quote = FALSE)
    #     print(sprintf('Lower limit of possible values: q = %.3f', q), quote = FALSE)
    #     print(sprintf('Shape factor: Lambda = %.4f',  lambda), quote = FALSE)
    #     print(sprintf('Standard uncertainty: u = %.3f',  sqrt(M^2 + (abs(m+q) * exp(lambda^2) * sqrt(exp(lambda^2-1)))^2), quote = FALSE))
    #     # plotting the refrence distribution and fittecd one
    #     print(sprintf('Max reference %.1f, max fitted %.1f', max(DataXY$referenceY), max(DataXY$referenceY,DataXY$Fitted)), quote = FALSE)
    #     ## density plot of d_Air_Pol - one parameter - reference density plot
    #     par(mar=c(5, 0.5, 0, 0.5))
    #     Ylim <- c(0, quantile(Air_Pol_final, probs = 0.98))
    #     plot(DataXY$x_to_fit, DataXY$referenceY
    #          , type = "l"
    #          , xlim = c(min(0,d_Air_Pol$x),quantile(Air_Pol, probs = 0.98))
    #          , ylim = Ylim
    #          , xlab = Dist_xlab, ylab = ""
    #          , main = "",cex.lab=3, cex.axis=2.5)
    #     polygon(c(min(DataXY$x_to_fit),DataXY$x_to_fit,max(DataXY$x_to_fit)), c(0,DataXY$referenceY,0), col="red", border="blue")
    #     lines(DataXY$x_to_fit, DataXY$Fitted ,
    #           xlim = Dist_range,
    #           ylim = c(min(0,d_Air_Pol$x),quantile(Air_Pol, probs = 0.98)),
    #           xlab = Dist_xlab,
    #           ylab = "",
    #           lwd =2,
    #           col = "Blue"
    #     )
    #     abline(v = M, lwd =2, col = "Black")
    # }
    #return(lambda)
    return(c(m, M,sqrt(exp(mu+1/2*sigma^2)^2 + exp(2*(mu+sigma^2)) - exp(2*mu+sigma^2))))
}
#================================================================CR
### Fit_His: fitting distribution to data ====
#================================================================CR
Fit_Hist <- function(Air_Pol, ug, coeff_ppb, Dist_range, bins, distribution, to_fit, Dist_xlab) {
    # fitting distribution manual Non Linear Regression based on the probability distribution funtion with only one parameter: the shape parameter
    # data: variable for whcih the densit function is calculated
    # ug: if TRUE transform from ug/m3 to ppb or ppm if FALSE does not transform
    # coeff_ppb: used if ug = TRUE: conversion factor to divide  ug/m3 to get ppb (ex 1.91 for NO2)
    # Dist_range: min and max value for the distribution plot and histogram
    # bins: number of breaks in the histogram
    # distribution: type of distribution to be fitted: lognormal ...
    # to_fit: if TRUE fitted with nls, if FALSE: not fitted
    # Dist_xlab: Label of X axis of the distribution plot
    # removing negative values of data - No we do not do it
    # Air_Pol <- subset(Air_Pol, Air_Pol >0)
    # transforming in ppb
    if (ug) Air_Pol <- Air_Pol/coeff_ppb
    # calculating the reference histrogram
    par(mar=c(5, 2,0, 0))
    histogram = hist(Air_Pol, breaks = bins, xlim =c(mini,maxi),xlab = Dist_xlab, ylab = "", main = "", cex.lab=3, cex.axis=2.5)
    # histogram <- subset(histogram, histogram$mids>0) # removing the negative values
    ## density plot for the 2 parameters lognormal distribution
    Log_Air_Pol <- log(Air_Pol)
    mu <- mean(Log_Air_Pol)
    sigma <- sd(Log_Air_Pol)
    x <- seq( min(Air_Pol, na.rm = T), max(Air_Pol), length.out=250)
    y <- f_lnorm_2Par(x,mu,sigma)
    par(mar=c(5, 2, 0, 0))
    plot(x, y, xlim = c(min(0,x),max(x, na.rm = T)), xlab = Dist_xlab, ylab = "", main = "", type = "l", cex.lab=3, cex.axis=2.5)
    polygon(c(min(x, na.rm = T),x,max(x, na.rm = T)), c(0,y,0), col="red", border="blue")
    print(sprintf("Mu (mean of log): %.1f", mu), quote = FALSE)
    print(sprintf("Sigma (sd of log): %.1f",sigma), quote = FALSE)
    print(sprintf("Mode: %.1f",exp(mu-sigma^2)), quote = FALSE)
    print(sprintf("Median: %.1f",median(Air_Pol)), quote = FALSE)
    print(sprintf("u (adding the mode): %.1f",sqrt(exp(mu-sigma^2)^2 + exp(2*(mu+sigma^2)) - exp(2*mu+sigma^2))), quote = FALSE)
    # Fitting Distribution
    if (distribution == "lognormal") {
        # Preparing data for fitting the distribution without NA and negative values, adding weights according to distance from 0 to promote data at 0
        DataXY <- na.omit(data.frame(cbind(histogram$density, histogram$mids)))
        colnames(DataXY) <- c('referenceY','x_to_fit')
        # Fitting the distribution with non linear regression with initial values of mu and sigma
        if (to_fit) {
            fitted_Distribution <- nls(referenceY ~ f_lnormal_2par(x_to_fit, m, s)
                                       , data=DataXY, start=list(m = mu, s = sigma))
            mu <- coef(fitted_Distribution)[1]
            sigma <- coef(fitted_Distribution)[2]
        }
        else {
            mu <- mean(Log_Air_Pol)
            sigma <- sd(Log_Air_Pol)
        }
        DataXY$Fitted <- f_lnormal_2par(DataXY$x_to_fit, mu, sigma)
        # if NA are created:
        DataXY <- na.omit(DataXY)
        # calculating the Mode of the data variable
        M <-  exp(mu - sigma^2)
        # Printing characteristics of the lognormal distribution
        # plotting the refrence distribution and fittecd one
        print(sprintf('Max reference %.1f, max fitted %.1f', max(DataXY$referenceY), max(DataXY$referenceY,DataXY$Fitted)), quote = FALSE)
        plot(DataXY$x_to_fit, DataXY$referenceY
             , type = "l"
             , xlim = Dist_range, ylim = c(0, max(DataXY$referenceY,DataXY$Fitted))
             , xlab = Dist_xlab, ylab = ""
             , main = "",cex.lab=3, cex.axis=2.5)
        lines(DataXY$x_to_fit, DataXY$Fitted , xlim = Dist_range, ylim = c(0, max(DataXY$referenceY,DataXY$Fitted)), xlab = Dist_xlab, ylab = ""
        )
        abline(v = M, col = 2)
    }
    return(fitted_Distribution)}
#================================================================CR
### f_lnormal_1par: fitting probability distribution funtion for Log-Normal distribution 1 parameter ====
#================================================================CR
f_lnormal_1par <- function(epsilon, lambda, q, m) {
    # epsilon: vector data for wwhich the log-normal distribution is fitted
    # lambda: vector shape parameter
    # q: lower limiting value
    # m: median
    return(1/(sqrt(2*pi)*lambda*abs(epsilon-q)) * exp(-(log((epsilon-q)/(m-q)))^2/(2*lambda^2)))}
#================================================================CR
### f_lnorm_2Par: Fitting probability distribution function for Log-Normal distribution 2 parameters ====
#================================================================CR
f_lnorm_2Par <- function(x, mu, sigma) {
    # x: vector data for which the log-normal distribution is calculated
    # mu: mean of the log of x
    # x: standard deviation of the log of x)
    # m: median
    return(1/(sqrt(2* pi)*sigma*x) * exp(-(log(x)-mu)^2/(2*sigma^2)))}
#================================================================CR
### Mode: Function returning mode of a distribution ====
#================================================================CR
Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}
#================================================================CR
### Estimated.y: Function Plot estimated function ====
#================================================================CR
Estimated.y <- function(x, Model, Length = NULL) {
    # This function estimates the y values at 5000 points of x from min(x) until max(x)
    # x      : Data used to establish the model (only x)
    # Model  : the Model to plot
    # Length : number of rows to be returned, default = NULL
    #
    # Return : data.frame estimated with variable x and y, each one with 5000 points in a x ascending order
    if (is.null(Length) || Length < 5000 ){
        Estimated <- data.frame(x = seq(min(x),max(x),length.out=5000), y= seq(min(x),max(x),length.out=5000))
    } else {
        Estimated <- data.frame(x = seq(min(x),max(x),length.out=Length), y= seq(min(x),max(x),length.out=Length))}
    if (inherits(Model, "gam")) { # checking if the model was fitted from gam function
        Estimated$y <- predict.gam(Model, newdata = Estimated, type = "response")
    } else  if (inherits(Model, "TLS") || inherits(Model, "deming")){
        Estimated$y <- coef(Model)[1]+coef(Model)[2]*Estimated$y
    } else {
        Estimated$y <- predict(Model, newdata = Estimated)
    }
    return(Estimated)}
#================================================================CR
### R1_rq: Function to compute pseudo-R^2 for quantile regression ====
#================================================================CR
#' @description The pseudo-R^2 measure suggested by Koenker and Machado's 1999 JASA paper gives a local measure of goodness of fit at the particular (??) quantile
#' by comparing the sum of weighted deviations for the model of interest with the same sum from a model in which only the intercept appears.
#' see https://stackoverflow.com/questions/19861194/extract-r2-from-quantile-regression-summary/27510106
#' @param x numeric vector x data
#' @param y numeric vector x data
#' @param probs numeric between 0 and 1, default is 0.5 the particular (??) quantile that shall be honoured by the quantile regression
#' @return The pseudo-R^2 measure suggested by Koenker and Machado's 1999 JASA paper
R1_rq <- function(x, y, probs = 0.5) {
    # loading libraries
    librarian::shelf(quantreg, data.table, futile.logger, cran_repo = "https://cran.r-project.org")
    
    # Preparing data and keeping full rows
    DataXY <- data.table::data.table(x = x, y = y)
    DataXY[is.finite(rowSums(DataXY))]
    
    if (DataXY[, .N] > 0) {
        fit0 <- quantreg::rq(y~1,tau=probs,data=DataXY)
        fit1 <- quantreg::rq(y~x,tau=probs,data=DataXY)
        
        rho <- function(u,tau=0.5)u*(tau - (u < 0))
        R1 <- 1 - fit1$rho/fit0$rho
        return(R1)
    } else return(futile.logger::flog.error("[] there is no complete data with x and y"))}
#================================================================CR
### Cal_Line: Function calibration function and plot calibration line (VS 170428) ====
#================================================================CR
#' Function to correct for the lag of reference data due to autocorelation of sensor data for the calibration period
#' @param DT.General Data.table with columns date (POSIXCt), ...
#' @param ColRef name of column to be delagged, reference data
#' @param ColSens name of column used to estimate the lag of ColRef
#' @param DateIN starting date for delagging, default is NULL. If NULL no data interval is selected
#' @param DateEND ending date for delagging, defautl is NULL.
#' @param Sync logical, default is TRUE. If TRUE lag time is estimated and corrected. If False, lag time is not estimated, initial ColRef data vector is re-used if available
#' @param Verbose logical, default is TRUE. If TRUE lag time is estimated and corrected. If False, lag time is not estimated, initial ColRef data vector is resumed if available
#' @return DT.General with ColRef being corrected for lag on column ColSens
DeLag_Cal <- function(DT.General, ColRef, ColSens, DateIN = NULL, DateEND = NULL, Sync = TRUE, Verbose = FALSE, ASE.name = NULL, name.sensor = NULL) {
    
    # Identify date of the calibration period
    if (!is.null(DateIN) && !is.null(DateEND)) Used.Date <- which(DT.General$date >= DateIN & DT.General$date <= DateEND + 1) else Used.Date <- NULL
    if (length(Used.Date) > 0 && length(which(is.finite(rowSums(DT.General[Used.Date, c(ColRef, ColSens), with = FALSE])))) > 0) {
        
        # restore original calibration data if column lag.out exists
        # Resuming original reference data if it exist
        if(paste0("Lag.", ColRef) %in% names(DT.General) && !all(is.na(DT.General[Used.Date][[paste0("Lag.", ColRef)]]))){
            Not.is.NA.Lag <- intersect(DT.General[is.finite(DT.General[[paste0("Lag.", ColRef)]]),which=TRUE], Used.Date)#1:max(which(!is.na(DT.General[[paste0("Lag.", ColRef)]])))
            if(length(Not.is.NA.Lag) > 0) data.table::set(DT.General, i =  Not.is.NA.Lag, j = ColRef, value = DT.General[Not.is.NA.Lag][[paste0("Lag.", ColRef)]])
        } else {
            # Saving original data.table ColRef column in totality to paste0("Lag.", ColRef). This serve if there were new sensor data in ColRef added after last copied to paste0("Lag.", ColRef)
            DT.General[, (paste0("Lag.", ColRef)) := DT.General[[ColRef]]]}
        
        if (Sync) {
            
            # Compute Lag
            Lag <- Find_Max_CCF(DT.General[Used.Date][[ColRef]],DT.General[Used.Date][[ColSens]])
            # exception for NO2 4047D0
            if (!is.null(ASE.name) && !is.null(name.sensor) && ASE.name == "4047D0" && name.sensor == "NO2_B43F_P1" && DateEND < as.Date("2020-02-01")) Lag$lag <- -60
            
            # DeLag data
            if (Lag$lag != 0) {
                if (Verbose) futile.logger::flog.info(paste0("[DeLag_Cal] there is a lag between x and y of ", Lag$lag," row of data which gives the best correlation between x and y"))
                
                # Saving original datatable only in totality if the column does not exists (only the first time)
                if(!paste0("Lag.", ColRef) %in% names(DT.General)) data.table::set(DT.General, j = paste0("Lag.", ColRef), value = DT.General[[ColRef]]) # i =  Used.Date, value = DT.General[Used.Date][[ColRef]]
                
                # Delagging
                if (Lag$lag > 0) {
                    #data.table::set(DT.General, i =  Used.Date, j =  ColRef, value = c(DT.General[Used.Date][[ColRef]][(Lag$lag+1):length(Used.Date)], rep(NA, Lag$lag)))
                    data.table::set(DT.General, j =  ColRef, value = c(DT.General[[ColRef]][(Lag$lag+1):DT.General[,.N]], rep(NA, Lag$lag)))
                } else {
                    #data.table::set(DT.General, i =  Used.Date, j =  ColRef, value = c(rep(NA, -Lag$lag), DT.General[Used.Date][[ColRef]][1:(length(Used.Date) - (-Lag$lag))]))
                    data.table::set(DT.General, j =  ColRef, value = c(rep(NA, -Lag$lag), DT.General[[ColRef]][1:(DT.General[,.N] - (-Lag$lag))]))
                }
                if (Verbose) futile.logger::flog.info(paste0("[DeLag_Cal] the reference data during the calibration period have been delagged."))
            } else if (Verbose) futile.logger::flog.info(paste0("[DeLag_Cal] there is no lag between x and y, original calibration data are restored."))
        } else if (Verbose) futile.logger::flog.info("[DeLag_Cal] Lag correction not requested. Original sensor data are restored.")
    } else if (Verbose) futile.logger::flog.warn(paste0("[DeLag_Cal] No data to delag in the selected calibration time period."))
    return(DT.General)}


#' 
#' #' @DT.General  mandatory data.table to be delagged for ColRef. Shall include column "date" (POSIXct in Influx.TZ timezone), ColRef and ColSens.
#' #' @ColRef mandatory column name for the reference measurements to be delagged. Shall be in DT.General
#' #' @ColSens mandatory column of the sensor measurements used to delag ColRef. Shall be in DT.General
#' #' @Influx.TZ optional default value is "UTC". Time Zone
#' #' 
#' 
#' Var_Delag_Pred <- function(DT.General, ColRef, Length.gap = 10, ColSens,
#'                            Meas.IN = NULL,  Meas.END = NULL, 
#'                            Influx.TZ = "UTC", Cal.IN, Cal.END, 
#'                            Sync.Pred = TRUE, Sync.Cal = FALSE,Verbose = TRUE) {
#'     # browser()
#'     # Checking consistency of DT.General, ColRef and ColSens
#'     stopifnot(shiny::isTruthy(DT.General) && shiny::isTruthy(ColRef) && shiny::isTruthy(ColSens) && all(c(ColRef, ColSens) %in% names(DT.General)))
#'     
#'     # Packages and libraries
#'     library(data.table)
#'     library(lubridate)
#'     
#'     
#'     # detecting the stops of Reference for at least Length.gap minutes in DT.General
#'     Ref.Stops <- DT.General[is.finite(DT.General[[ColRef]]),.SD, .SDcols = c("date", ColRef)][which(diff(DT.General[is.finite(DT.General[[ColRef]])][["date"]]) > Length.gap)]
#'     if(nrow(Ref.Stops) > 0){
#'         Ref.Stops[, Gap := diff(DT.General[is.finite(DT.General[[ColRef]])][["date"]])[which(diff(DT.General[is.finite(DT.General[[ColRef]])][["date"]]) > Length.gap)]]
#'         Ref.Stops[, Diff := diff(DT.General[is.finite(DT.General[[ColRef]])][["date"]])[which(diff(DT.General[is.finite(DT.General[[ColRef]])][["date"]]) > Length.gap)]]
#'         Ref.Stops[, Index := match(date, DT.General$date)]
#'         
#'         for(Period in seq_along(Ref.Stops$date)){
#'             cat("\n")
#'             # if(Period == 1){
#'             #     # 1st period
#'             #     Start.Period <-  max(c(min(DT.General$date, na.rm =T), lubridate::ymd(Meas.IN, tz = Influx.TZ)), na.rm = T)
#'             # } else {
#'             #     # period between Period 2 and nrow
#'             #     Start.Period <-  DT.General$date[Ref.Stops$Index[Period - 1] + 1]}
#'             if (Period == nrow(Ref.Stops)) {
#'                 Start.Period <-  DT.General$date[Ref.Stops$Index[Period] + 1]
#'                 End.Period <- max(DT.General[is.finite(DT.General[[ColRef]])]$date, na.rm =T) 
#'                 if (Verbose) futile.logger::flog.info(paste0("[Var_Delag_Pred] Period ", Period + 1, " from ", Start.Period, " to ",
#'                                                              End.Period))
#'             } else  {
#'                 Start.Period <-  DT.General$date[Ref.Stops$Index[Period - 1] + 1]
#'                 End.Period <- Ref.Stops$date[Period] 
#'                 if(Verbose) futile.logger::flog.info(paste0("[Var_Delag_Pred] Period ", Period, " from ", Start.Period, " to ", Ref.Stops$date[Period]))
#'             }
#'             DT.General <- DeLag_Pred(DT.General = DT.General, ColRef = ColRef, ColSens = ColSens, 
#'                                      Meas.DateIN  = Start.Period,
#'                                      Meas.DateEND = End.Period, 
#'                                      Cal.DateIN = Cal.IN, Cal.DateEND = Cal.END, 
#'                                      Sync.Pred = Sync.Pred, Sync.Cal = Sync.Cal, ASE.name = ASE.ID$ASE.name)
#'             
#'             # Data after the last row of Ref.Stops on the last Period
#'             # if(Period == nrow(Ref.Stops)){
#'             #     browser()
#'             #     # Delagging from last period to end of dates of DT.General
#'             #     cat("\n")
#'             #     futile.logger::flog.info(paste0("[Var_Delag_Pred] Period ", Period + 1, " from ", DT.General$date[Ref.Stops$Index[Period] + 1], " to ",
#'             #                                     min(lubridate::ymd(Meas.END+1, tz = Influx.TZ), max(DT.General$date, na.rm =T), na.rm = T)))
#'             #     DT.General <- DeLag_Pred(DT.General   = DT.General, ColRef = ColRef, ColSens = ColSens, 
#'             #                              Meas.DateIN  = DT.General$date[Ref.Stops$Index[Period] + 1],
#'             #                              Meas.DateEND = min(lubridate::ymd(Meas.END+1, tz = Influx.TZ), max(DT.General$date, na.rm =T), na.rm = T), 
#'             #                              Cal.DateIN   = Cal.IN, Cal.DateEND = Cal.END, 
#'             #                              Sync.Pred    = Sync.Pred, Sync.Cal = Sync.Cal, ASE.name = ASE.ID$ASE.name)}
#'         }
#'     } else {
#'         DT.General <- DeLag_Pred(DT.General = DT.General, ColRef = ColRef, ColSens = ColSens, 
#'                                  Meas.DateIN = Meas.IN, Meas.DateEND = Meas.END, Cal.DateIN = Cal.IN, Cal.DateEND = Cal.END, 
#'                                  Sync.Pred = Sync.Pred, Sync.Cal = Sync.Cal, ASE.name = ASE.ID$ASE.name)}
#'     # returning DT.General
#'     return(DT.General)
#' }




#' Function to correct for the lag of reference data between every sensor or reference analyzer stop higher than Length.gap (30 min default) for a date period of DT.General
#' @DT.General  mandatory data.table to be delagged for ColRef. Shall include column "date" (POSIXct in Influx.TZ timezone), ColRef, ColSens, ColRefRaw and ColSensRaw.
#' @ColRef mandatory column name for the reference measurements to be delagged.Must start with "Out". Shall be in DT.General
#' @ColSens mandatory column of the sensor measurements used to delag ColRef. Shall be in DT.General
#' @ColSensRaw mandatory column of the sensor raw measurements (nonfiltered) to determine the stops of sensor AND reference analyzer.Shall be in DT.General
#' @ColRefRaw mandatory column of the reference unfiltered measurements to determine the stops of sensor AND reference analyzer.Shall be in DT.General
#' @Meas.IN mandatory column starting date of interval where lagging will be checked and corrected ONLY if there is no stop of reference analyzer or sensor (in that case it goes to DeLag_Pred())
#' @Meas.END mandatory column end date of interval where lagging will be checked and corrected ONLY if there is no stop of reference analyzer or sensor (in that case it goes to DeLag_Pred())
#' @Cal.IN mandatory column starting date of calibration interval. Used ONLY if there is no stop of reference analyzer or sensor (in that case it goes to DeLag_Pred())
#' @Cal.END mandatory column starting date of calibration interval. Used ONLY if there is no stop of reference analyzer or sensor (in that case it goes to DeLag_Pred())
#' @Influx.TZ optional default value is "UTC". Time Zone

Var_Delag_Pred <- function(DT.General, ColRef = NULL,  Length.gap = 30, ColSens, ColSensRaw, ColRefRaw,
                           Meas.IN,  Meas.END, 
                           Influx.TZ = "UTC", Cal.IN, Cal.END, 
                           Sync.Pred = TRUE, Sync.Cal = FALSE, Verbose = FALSE) {
    if(!Sync.Pred) {
        return(DT.General)
    } else {
        # Packages and libraries
        library(data.table)
        library(lubridate)
        # if (is.null(ColSensRaw)) ColSensRaw <- gsub("_modelled","", ColSens)
        # if (is.null(ColRef))  {
        #     # if (!"sens2ref" %in% names(Config)) {
        #         ColRef  <- paste0('Out.', Config[which(Config$name.gas == 'gas.reference2use'), which(Config[which(Config$name.gas == 'gas.sensor')] == ColSensRaw), with = F][[1]])
        #     # } else {
        #     #     browser()            
        #     # }
        # } 
        if (!grepl('Out.', ColRef)) futile.logger::flog.error('[Var_Delag_Pred] ColRef does not contain Out. Check the ColRef' )
        # ColRefRaw <- gsub("Out.","", ColRef)
        # if (is.null(ColRef))  ColRef <- paste0('Out.', Config[which(Config$name.gas == 'gas.reference2use'), which(Config[which(Config$name.gas == 'gas.sensor')] == ColSensRaw), with = F][[1]])
        # Checking consistency of DT.General, ColRef and ColSens
        stopifnot(shiny::isTruthy(DT.General) && shiny::isTruthy(ColRef) && shiny::isTruthy(ColSens) && all(c(ColRef, ColSens) %in% names(DT.General)))
        # determination of reference data with 1 h resolution
        i.dates <- DF_sd(DF=DT.General, Col.for.sd = ColRefRaw, width = 60L)$i.dates
        # excluding the dates to determine reference stops for minute resolution reference data
        if(shiny::isTruthy(i.dates)) {
            # selecting the data where reference and raw sensor data is NA for minute resolution
            DT.General.min  <- DT.General[-i.dates][is.finite(DT.General[-i.dates][[ColRefRaw]]) & is.finite(DT.General[-i.dates][[ColSensRaw ]]),.SD, .SDcols = c("date", ColRefRaw, ColRef, ColSensRaw, ColSens)]
            DT.General.hour <- DT.General[ i.dates][is.finite(DT.General[ i.dates][[ColRefRaw]]) & is.finite(DT.General[ i.dates][[ColSensRaw]]), .SD, .SDcols = c("date", ColRefRaw, ColRef, ColSens, ColSensRaw )]
            Ref.Stops.hour  <- DT.General.hour[c(which(diff(DT.General.hour$date) >= 60), nrow(DT.General.hour))]
            if(nrow(Ref.Stops.hour) > 0){
                Ref.Stops.hour[, Period.Start := DT.General.hour[['date']][c(1, which(diff(DT.General.hour[["date"]]) >= 60) +1)]]
                Ref.Stops.hour[, Period.End   := DT.General.hour[['date']][c(which(diff(DT.General.hour[["date"]]) >= 60), nrow(DT.General.hour))]]
                Ref.Stops.hour[, Gap := c(diff(DT.General.hour[["date"]])[which(diff(DT.General.hour[["date"]]) >= 60)]-1,0)]
                Ref.Stops.hour[, Index.Start := sapply(seq_along(Ref.Stops.hour$date), function (i) DT.General[date == Ref.Stops.hour[i][["Period.Start"]], which =T])]
                Ref.Stops.hour[, Index.End := sapply(seq_along(Ref.Stops.hour$date), function (i) DT.General[date == Ref.Stops.hour[i][["Period.End"]], which =T])]
                Ref.Stops.hour[, diff.start.end := Ref.Stops.hour[["Index.End"]] - Ref.Stops.hour[["Index.Start"]]]
                # Ref.Stops.hour[, diff.start.end := sapply(seq_along(Ref.Stops.hour$date), function(i) {
                #     if(Ref.Stops.hour[i][["Index.End"]] - Ref.Stops.hour[i][["Index.Start"]] > 60) return(Ref.Stops.hour[i][["Index.End"]] - Ref.Stops.hour[i][["Index.Start"]]) else return(NA)})]
                
                
                # Checking if there is at least 60 data of Colsens and ColRef (if not discarded due to being < min or > max), or, length of period is equal to or less than 1 h. If not, we will exclude from Stops
                Ref.Stops.hour[, ColSens2use := sapply(seq_along(Ref.Stops.hour$date), function(i) { 
                    (Ref.Stops.hour$diff.start.end[i] > 60 &&
                        length(which(!is.na(DT.General.hour[DT.General.hour$date %between% c(Ref.Stops.hour$Period.Start[i],Ref.Stops.hour$Period.End[i])][[ColSens]]))) > 60 &&
                         length(which(!is.na(DT.General.hour[DT.General.hour$date %between% c(Ref.Stops.hour$Period.Start[i],Ref.Stops.hour$Period.End[i])][[ColRef]]))) > 60 
                         )})]
                # Ref.Stops.hour[, ColSens2use := sapply(seq_along(Ref.Stops.hour$date), function(i) {
                #     if (length(!is.na(DT.General.hour[DT.General.hour$date %between% c(Ref.Stops.hour$Period.Start[i],Ref.Stops.hour$Period.End[i])][[ColSens]])) > 60) {
                #         return(T)
                #     } else return(F)
                # })]
                Ref.Stops.hour <- Ref.Stops.hour[which(ColSens2use)]
                for(Period in seq_along(Ref.Stops.hour$date)){
                    if (Verbose) {
                        cat("\n")
                        futile.logger::flog.info(paste0("[Var_Delag_Pred] Hourly period ", Period, " from ",
                                                        format(Ref.Stops.hour$Period.Start[Period], "%y-%m-%d %H:%M"), " to ",
                                                        format(Ref.Stops.hour$Period.End[Period], "%y-%m-%d %H:%M"), " for ", ColRef))}
                    
                    Ref.period  <- DT.General[date %between% c(Ref.Stops.hour$Period.Start[Period], Ref.Stops.hour$Period.End[Period])][[ColRef]]
                    Sens.period <- DT.General[date %between% c(Ref.Stops.hour$Period.Start[Period], Ref.Stops.hour$Period.End[Period])][[ColSens]]
                    
                    # Checking if the ref or sensor data is the unique during period. If so, Find_Max_CCF crashes
                    if (sd(Ref.period, na.rm = T) == 0 || sd(Sens.period, na.rm = T) == 0) {
                        if(Verbose) futile.logger::flog.info(paste0("[Var_Delag_Pred] ColRef Or ColSens are constant, the autocorrelation cannot be calculated"))
                        next}
                    if (any(is.na(Ref.period)) || any(is.na(Sens.period))) {
                        # checking the longest period with modelled sensor data not NA
                        i.period    <- which(is.finite(Sens.period) & is.finite(Ref.period))
                        i.max       <- which.max(diff(c(i.period[1], which(diff(i.period) > 1), i.period[length(i.period)])))
                        if (length(i.max) > 1) {
                            longest.i   <- c(i.period[1], which(diff(i.period) > 1), i.period[length(i.period)])[i.max:(i.max+1)][1]:c(i.period[1], which(diff(i.period) > 1), i.period[length(i.period)])[i.max:(i.max+1)][2]
                        } else longest.i   <- c(i.period[1], i.period[length(i.period)])
                        Ref.period  <- Ref.period[longest.i]
                        Sens.period <- Sens.period[longest.i]
                        rm(i.period, i.max, longest.i)
                    }
                    # finding the hourly lag for hourly resolution data
                    if (is.error(Find_Max_CCF(Ref.period, Sens.period, Lag.max = 180))) {
                        if(Verbose) futile.logger::flog.info(paste0("[Var_Delag_Pred] Error in the autocorrelation function calculation of ColRef and ColSens"))
                        next}
                    Lag.h   <- Find_Max_CCF(Ref.period, Sens.period, Lag.max = 180)
                    if(!exists("Lag.h")) next
                    # We only care about lag bigger than 60 min and positive correlation
                    if (Lag.h$lag >= 60 && Lag.h$cor >= 0) {
                        if (Lag.h$lag == 180) futile.logger::flog.warn(paste0("[Var_Delag_Pred] Lag is equal to 3 h in hour resolution data. Carefully check if the lag is real"))
                        if (Verbose){
                            futile.logger::flog.info(paste0("[Var_Delag_Pred] a lag between ", ColRef, " and ", ColSens, " of ", Lag.h$lag,
                                                            " data row(s) gives the best correlation (r=", round(Lag.h$cor, digit=3),
                                                            ") compared to lagged data (r=", 
                                                            round(cor(DT.General[date %between% c(Ref.Stops.hour$Period.Start[Period], Ref.Stops.hour$Period.End[Period]),.SD,.SDcols = c(ColRef,ColSens)],
                                                                      use = "complete.obs")[1,2], digit = 3), ")"))}
                        
                        # New time for reference during the period
                        Ref.names <- c("date", ColRef) 
                        DT.General.ref <- DT.General[date %between% c(Ref.Stops.hour$Period.Start[Period], Ref.Stops.hour$Period.End[Period]),..Ref.names]
                        # if the lag > gap we limit the Lag with the minimum of gap lenght of the period and previous one (if exists)
                        # if (Period == 1) {
                        #     gap.len <- Ref.Stops.hour$Gap[Period][[1]]
                        # } else if (Period == nrow(Ref.Stops.hour)) {
                        #     if (Ref.Stops.hour$Gap[Period] == 0)  gap.len <- Ref.Stops.hour$Gap[Period-1][[1]] else  gap.len <- Ref.Stops.hour$Gap[Period][[1]]
                        # } else gap.len <- min(Ref.Stops.hour$Gap[Period][[1]], Ref.Stops.hour$Gap[Period-1][[1]], na.rm = T)
                        # 
                        # if (Lag.h$lag > gap.len) {
                        #     data.table::set(DT.General.ref, j = "date", value = DT.General.ref$date - gap.len*60)
                        # } else 
                        data.table::set(DT.General.ref, j = "date", value = DT.General.ref$date - Lag.h$lag*60)
                        # Merging DT.General with DT.General.ref, new time Ref for the Period
                        DT.General.period <- data.table::merge.data.table(
                            DT.General[date %between% c(Ref.Stops.hour$Period.Start[Period], Ref.Stops.hour$Period.End[Period]), .SD, .SDcols = names(DT.General)[names(DT.General)!= ColRef]],
                            DT.General.ref, by = "date", all.x = TRUE, all.y = FALSE)
                        # Replacing DT.General.period in the whole data.set DT.General
                        l <- list(DT.General[DT.General$date < Ref.Stops.hour$Period.Start[Period] | DT.General$date > Ref.Stops.hour$Period.End[Period]],
                                  DT.General.period)
                        DT.General <- data.table::rbindlist(l, use.names = TRUE)
                        #Resorting with date
                        setkey(DT.General,date)
                    } else if (Verbose){
                        futile.logger::flog.info(paste0("[Var_Delag_Pred] The hourly lag is 0 or the correlation is negative (", Lag.h$cor, ") between ",
                                                        ColRef, " and ", ColSens))}
                }
                rm(DT.General.hour, Ref.Stops.hour)
            }
        } else {
            # selecting the data where reference and raw sensor data is NA for minute resolution
            DT.General.min <- DT.General[is.finite(DT.General[[ColRefRaw]]) & is.finite(DT.General[[ColSensRaw]]),.SD, .SDcols = c("date", ColRefRaw, ColRef, ColSensRaw, ColSens)]
        }
        # detecting the stops of Reference for at least Length.gap minutes in DT.General
        Ref.Stops.min <- DT.General.min[c(which(diff(DT.General.min[["date"]]) > Length.gap), nrow(DT.General.min))]
        # Ref.Stops.min <- DT.General.min[which(diff(DT.General.min[["date"]]) > Length.gap)]
        if(nrow(Ref.Stops.min) > 0){
            Ref.Stops.min[, Index := match(date, DT.General.min$date)]
            Ref.Stops.min[, Gap := c(diff(DT.General.min[["date"]])[which(diff(DT.General.min[["date"]]) > Length.gap)],0)]
            Ref.Stops.min[, Period.Start := lapply(seq_along(Ref.Stops.min$date), function(i) {
                if (i == 1) {
                    return(min(DT.General.min$date, na.rm =T))
                } else {
                    return(unlist(DT.General.min[which(date == Ref.Stops.min$date[i-1]) + 1][['date']]))
                }
            })]
            # Ref.Stops.min[, Period.Start := lapply(seq_along(Ref.Stops.min$date), function(i) {
            #     if (i == 1) {
            #         return(min(DT.General.min$date, na.rm =T))
            #     } else if (i != nrow(Ref.Stops.min)) {
            #         return(unlist(DT.General.min[which(date == Ref.Stops.min$date[i-1]) + 1][['date']]))
            #     } else {
            #         return(unlist(Ref.Stops.min$date[nrow(Ref.Stops.min)]))
            #         # return(DT.General[['date']][which(DT.General$date == max(DT.General.min$date, na.rm =T))])
            #     }
            # })]
            Ref.Stops.min[, Period.End := lapply(seq_along(Ref.Stops.min$date), function(i) {
                if (i != nrow(Ref.Stops.min)) {
                    return(Ref.Stops.min$date[i])
                } else {
                    return(DT.General[['date']][which(DT.General$date == max(DT.General.min$date, na.rm =T))])
                }
            })]
            # checking if any period < 60 min, if so we will not check lag for these period(s)
            if (any(diff(Ref.Stops.min$Index) < 60)) {
                i.1min <- which(diff(Ref.Stops.min$Index) < 60)
                # checking if these period(s) are at the end of Ref.Stops
                if (any(i.1min) == nrow(Ref.Stops.min)) {
                    # removing the data with only 1 min from the Ref.stops
                    i.1min <-  i.1min[which(i.1min == nrow(Ref.Stops.min))]
                    # Checking if any gap after the 1 min gap
                    i.rest <- i.1min+1:nrow(DT.General.min)
                    i.gap  <- i.rest[which(diff(DT.General.min[i.rest][["date"]]) > Length.gap)]
                    if (length(i.gap) > 0) {
                        i.gap <- c(i.rest[1], i.gap, length(i.rest))
                        Ref.Stops.min <- Ref.Stops.min[-c(which(diff(Ref.Stops.min$Index) < 60)+1)] 
                    } else browser()
                    # else {
                    #     data.table::set(Ref.Stops.min, i = Ref.Stops.min[which(Index == i.1min), which =T], j = "date", value = DT.General.min[i.1min+1][['date']])
                    #     data.table::set(Ref.Stops.min, i = Ref.Stops.min[which(Index == i.1min), which =T], j = "Period.Start", value = DT.General.min[i.1min+1][['date']])
                    #     data.table::set(Ref.Stops.min, i = Ref.Stops.min[which(Index == i.1min), which =T], j = "Gap", value = 0)
                    #     data.table::set(Ref.Stops.min, i = Ref.Stops.min[which(Index == i.1min), which =T], j = ColRefRaw, value = ifelse(is.na(DT.General.min[i.1min+1][[ColRefRaw]]),9999, DT.General.min[i.1min+1][[ColRefRaw]]))
                    #     data.table::set(Ref.Stops.min, i = Ref.Stops.min[which(Index == i.1min), which =T], j = ColRef, value = ifelse(is.na(DT.General.min[i.1min+1][[ColRef]]), 9999, DT.General.min[i.1min+1][[ColRef]]))
                    #     data.table::set(Ref.Stops.min, i = Ref.Stops.min[which(Index == i.1min), which =T], j = ColSensRaw, value = ifelse(is.na(DT.General.min[i.1min+1][[ColSensRaw]]), 9999, DT.General.min[i.1min+1][[ColSensRaw]]))
                    #     data.table::set(Ref.Stops.min, i = Ref.Stops.min[which(Index == i.1min), which =T], j = ColSens, value = ifelse(is.na(DT.General.min[i.1min+1][[ColSens]]), 9999, DT.General.min[i.1min+1][[ColSens]]))
                    # }
                }
            }
            
            Ref.Stops.min[, diff.start.end := sapply(seq_along(Ref.Stops.min$date), function(i) difftime(Ref.Stops.min$Period.End[i][[1]], Ref.Stops.min$Period.Start[i][[1]], units = 'mins'))]
            # Ref.Stops.min[, diff.start.end := sapply(seq_along(Ref.Stops.min$date), function(i) {
            #     if (difftime(Ref.Stops.min$Period.End[i][[1]], Ref.Stops.min$Period.Start[i][[1]], units = 'mins') >= 60 ) {
            #         return(difftime(Ref.Stops.min$Period.End[i][[1]], Ref.Stops.min$Period.Start[i][[1]], units = 'mins'))
            #     } else {
            #         return(NA)
            #     }
            # })]
            Ref.Stops.min[, ColSens2use := sapply(seq_along(Ref.Stops.min$date), function(i) { 
                (length(which(!is.na(DT.General.min[date %between% c(Ref.Stops.min$Period.Start[i][[1]],Ref.Stops.min$Period.End[i][[1]])][[ColSens]]))) > 60 &&
                     Ref.Stops.min$diff.start.end[i] > 60)})]
            # Ref.Stops.min[, is.all.na.ColSens := sapply(seq_along(Ref.Stops.min$date), function(i) {
            #     if (all(is.na(DT.General.min[DT.General.min$date %between% c(Ref.Stops.min$Period.Start[i],Ref.Stops.min$Period.End[i])][[ColSens]]))) {
            #         return(T)
            #     } else return(F)
            # })]
            # data.table::setnames(Ref.Stops.min, 'date', "Period.End")
            Ref.Stops.min <- Ref.Stops.min[which(ColSens2use)]
            # Ref.Stops.min <- Ref.Stops.min[!which(is.all.na.ColSens)]
            for(Period in seq_along(Ref.Stops.min$Period.Start)){
                if (Verbose) {
                    cat("\n")
                    futile.logger::flog.info(paste0("[Var_Delag_Pred] Minute period ", Period, " from ",
                                                    format(Ref.Stops.min$Period.Start[Period][[1]], "%y-%m-%d %H:%M"), " to ",
                                                    format(Ref.Stops.min$Period.End[Period][[1]], "%y-%m-%d %H:%M"), " for ", ColRef))}
                # # to determine the gap lengths of the current period and previous period. In case of Lag > minimum of these lengths, we will limit the lag with the minimum lag length
                # if (Period == 1)  {
                #     Gap.len.period <- c(Ref.Stops.min$Gap[Period][[1]], NA)
                #     } else if (Period == nrow(Ref.Stops.min)) {
                #         if (Ref.Stops.min$Gap[Period][[1]] == 0) Gap.len.period <- c(Ref.Stops.min$Gap[Period-1][[1]], NA) else Gap.len.period <- c(Ref.Stops.min$Gap[Period][[1]], NA)
                #         } else Gap.len.period <- c(Ref.Stops.min$Gap[Period][[1]], Ref.Stops.min$Gap[Period - 1][[1]])
                DT.General <- DeLag_Pred(DT.General = DT.General, ColRef = ColRef, ColSens = ColSens, 
                                         Meas.DateIN  = Ref.Stops.min$Period.Start[Period][[1]], #Gap.len = Ref.Stops.min$Gap[Period][[1]],
                                         Meas.DateEND = Ref.Stops.min$Period.End[Period][[1]], 
                                         Cal.DateIN = Cal.IN, Cal.DateEND = Cal.END, Verbose = Verbose,
                                         Sync.Pred = Sync.Pred, Sync.Cal = Sync.Cal, ASE.name = ASE.ID$ASE.name)
                # DT.General <- DT$DT.General
                # if(exists('table.lag')) table.lag <- rbind(table.lag, DT$dt.all) else table.lag <- DT$dt.all
                
            }
        } else if (!exists('i.dates') && !identical(i.dates, which(is.finite(DT.General[[ColRefRaw]]) & is.finite(DT.General[[ColSensRaw]])))) { # checking if all the General.DT is in hourly resolution. 
            DT <- DeLag_Pred(DT.General = DT.General, ColRef = ColRef, ColSens = ColSens, , Verbose = Verbose,
                             Meas.DateIN = Meas.IN, Meas.DateEND = Meas.END, Cal.DateIN = Cal.IN, Cal.DateEND = Cal.END, 
                             Sync.Pred = Sync.Pred, Sync.Cal = Sync.Cal, ASE.name = ASE.ID$ASE.name)
            # DT.General <- DT$DT.General
            # table.lag <- DT$dt.all
        }
        # returning DT.General
        # return(list(DT.General = DT.General, table.lag = table.lag))
        return(DT.General)
    }
    
}
#' Function to correct for the lag of reference data due to autocorelation of sensor data
#' The synchonissation of the sensor and reference data uses an auto-correlation function between Meas.DateIN and Meas.DateEND
#' The functions starts by resuming the original ColRef data between Meas.DateIN and Meas.DateEND saved in paste0("Lag.", ColRef) if it esists.
#' If it does not exists the column paste0("Lag.", ColRef) is added to DT.General by copying the whole column ColRef.
#' If Sync.cal is TRUE, the data in column paste0("Lag.", ColRef) between Cal.DateIN and Cal.DateEND are not resumed.
#' @param DT.General Data.table with columns date (POSIXCt), ...
#' @param ColRef name of column to be delagged, reference data
#' @param Meas.DateIN starting date for delagging, default is NULL. if NULL the first date is used.
#' @param Meas.DateEND ending date for delagging, defautl is NULL. if NULL the last date is used.
#' @param CalDateIN starting date for delagging of the calibration interval, default is NULL. If NULL no data interval is selected
#' @param CalDateEND ending date for delagging of the calibration interval, defautl is NULL.
#' @param Sync.Pred logical, default is TRUE. If TRUE lag time is estimated and corrected for the prediction period between Meas.DateIN and Meas.DateEND. If False, lag time is not estimated, initial ColRef data vector is re-used if available
#' @param Sync.Cal  logical, default is TRUE. If TRUE lag time is estimated for the prediction period taking out the calibration period. 
#' @param Complete logical, default is TRUE. If TRUE the delagging of ColRef is applied to the whole time series whatever values of Meas.DateIN and Meas.DateEND
#' @param Verbose logical, default TRUE. If TRUE print some messages
#' @param ASE.name, name.sensor additional character strings that are no more used and are not needed
#' @Gap.len optional (Default is 180 min). The gap length of the current and previous periods, to check if the lag is bigger than Gap.len of the current period. if so, Lag will be limited with minimum of Gap.len

DeLag_Pred <- function(DT.General, ColRef, ColSens, Meas.DateIN = NULL, Meas.DateEND = NULL, Cal.DateIN = NULL, Cal.DateEND = NULL, # Gap.len = 180,
                       Sync.Pred = TRUE, Sync.Cal = TRUE, Complete = FALSE, Verbose = TRUE, ASE.name = NULL, name.sensor = NULL) {
    
    stopifnot(shiny::isTruthy(DT.General))
    
    # Identify indexes of DT.General for saving/restoring paste0("Lag.", ColRef)
    if(Complete){
        Used.Date <- 1:nrow(DT.General)
    } else Used.Date <- which(DT.General$date >= Meas.DateIN & DT.General$date <= Meas.DateEND + 1) 
    # if (!is.null(Meas.DateIN) && !is.null(Meas.DateEND)){
    #     Used.Date <- which(DT.General$date >= Meas.DateIN & DT.General$date <= Meas.DateEND + 1)   
    # } else Used.Date <- NULL
    # Discarding Used.Cal data if Sync.Cal is TRUE before restoring paste0("Lag.", ColRef)
    if(Sync.Cal & !is.null(Cal.DateIN) && !is.null(Cal.DateEND)){
        Used.Cal  <- which(DT.General$date >= Cal.DateIN & DT.General$date <= Cal.DateEND + 1)
        if(length(Used.Cal) > 0) Used.Date <- setdiff(Used.Date, Used.Cal)}
    
    # Restore original ColRef data only between the date of interest (from Meas.DateIN to Meas.DateEND  with or without calibration)
    # and if column paste0("Lag.", ColRef) exists otherwise create it
    if(length(Used.Date) > 0 && paste0("Lag.", ColRef) %in% names(DT.General)){ #  && !all(is.na(DT.General[Used.Date][[paste0("Lag.", ColRef)]]))
        # Not.is.NA.Lag <- intersect(DT.General[is.finite(DT.General[[paste0("Lag.", ColRef)]]),which=TRUE], Used.Date)
        # if(length(Not.is.NA.Lag) > 0) data.table::set(DT.General, i =  Not.is.NA.Lag, j = ColRef, value = DT.General[Not.is.NA.Lag][[paste0("Lag.", ColRef)]])
        data.table::set(DT.General, i =  Used.Date, j = ColRef, value = DT.General[Used.Date][[paste0("Lag.", ColRef)]])
    } else {
        # Saving original data.table ColRef column in totality to paste0("Lag.", ColRef).
        # This serve if there were new sensor data in ColRef added after last copied to paste0("Lag.", ColRef)
        ## it is not necessary any longer since we modified Filter_Ref_Data to always create "Lag.Out.Ref.'
        # DT.General[, (paste0("Lag.", ColRef)) := DT.General[[ColRef]]]
    }
    
    if (Sync.Pred) {
        
        if (length(Used.Date) > 0) { 
            
            # Estimating lag, by selecting only complete days of ColRef or ColSens. We check later each sub-period and select the most frequent lag of these sub-periods (Used to be : Dates is the longest vector of complete days)
            DT.Complete <- DT.General[Used.Date][is.finite(DT.General[Used.Date][[ColRef]]) | is.finite(DT.General[Used.Date][[ColSens]]), .N, by= as.Date(date)]
            if (nrow(DT.Complete) > 0) { # Just to ensure that we have enough data to check lag. Sometimes reference and raw sensor can be complete but all raw sensor data are filtered out.
                # For gas ref analyzers, due to calibration check of every 2-3 days, we can't have periods with > 10 days
                # if (nrow(DT.Complete) > 10) { # Just to ensure that we have enough data to check lag. Sometimes reference and raw sensor can be complete but all raw data are filtered out
                # to determine the sub-period 
                Used.Date.finite <- Used.Date[which(is.finite(DT.General[Used.Date][[ColRef]]) & is.finite(DT.General[Used.Date][[ColSens]]))]
                # we determine the sub-periods for missing ColRef or ColSens at least for 10 mins (usually when raw sensor data or ref data are filtered out, there will be a gap)
                Periods          <- c(Used.Date.finite[1], Used.Date.finite[which(diff(Used.Date.finite) > 10)], Used.Date.finite[length(Used.Date.finite)])
                
                if (any(diff(Periods) > 120)) { # we want at least 120 min periods to check for lag
                    # for (i in Periods[which(diff(Periods) > 120)]) {
                    for (i in seq_along(Periods[-length(Periods)])) {
                        if (length(Periods[i]:Periods[i+1]) < 120) {
                            if(Verbose) futile.logger::flog.info(paste0("[Var_Delag_Pred] the duration of the period is too short, the autocorrelation cannot be calculated"))
                            next}
                        # sometimes there is many filtered data making Find_Max_CCF crashes
                        if(is.error(Find_Max_CCF(DT.General[Periods[i]:Periods[i+1]][[ColRef]],DT.General[Periods[i]:Periods[i+1]][[ColSens]]))) next
                        Lag.i  <- Find_Max_CCF(DT.General[Periods[i]:Periods[i+1]][[ColRef]],DT.General[Periods[i]:Periods[i+1]][[ColSens]])
                        Lag.i  <- as.data.table(Lag.i)
                        Lag.i  <- data.table::set(Lag.i, j = "Ind.start", value = Periods[i])
                        Lag.i  <- data.table::set(Lag.i, j = "Ind.end", value = Periods[i+1])
                        Lag.i  <- data.table::set(Lag.i, j = "len", value = Periods[i+1] - Periods[i])
                        if (exists("Lag.all")) Lag.all <- rbind(Lag.all, Lag.i) else Lag.all <- Lag.i
                        rm(Lag.i)
                    }
                    Lag.all[, cor.len := len * cor]
                    Lag.all <- Lag.all[, .(cor.len = sum(cor.len, na.rm=T) , cor = max(cor), Ind.start = min(Ind.start, na.rm=T), Ind.end = max(Ind.end, na.rm=T), len = sum(len, na.rm=T)), by = lag]
                    Lag <- Lag.all[which.max(cor.len)]
                    
                    if (nrow(Lag) > 0 && Lag$lag != 0 && Lag$cor > 0) {
                        # if (nrow(Lag) > 0 && Lag$lag != 0) {
                        if (abs(Lag$lag) > Lag$len) {
                            if (Verbose){
                                futile.logger::flog.info(paste0("[DeLag_Pred] a lag of ", Lag$lag, " data row(s) between ColRef and ColSens for a sub-period from ",
                                                                format(DT.General[Lag$Ind.start][['date']], "%y-%m-%d %H:%M"), " to ",
                                                                format(DT.General[Lag$Ind.end][['date']], "%y-%m-%d %H:%M"),
                                                                paste0(" (", Lag$len, " mins)"),", applied to the entire period from ",
                                                                format(Meas.DateIN, "%y-%m-%d %H:%M"), ' to ', format(Meas.DateEND, "%y-%m-%d %H:%M"),
                                                                paste0(" (", difftime(Meas.DateEND,Meas.DateIN, unit = 'mins')[[1]], " mins)"),
                                                                " is bigger than period length, ", Lag$len, " (min). Original prediction data are restored."))}
                            
                        } else {
                            if (Verbose) {
                                futile.logger::flog.info(paste0("[DeLag_Pred] a lag of ", Lag$lag, " data row(s) between ColRef and ColSens for a sub-period from ",
                                                                format(DT.General[Lag$Ind.start][['date']], "%y-%m-%d %H:%M"), " to ",
                                                                format(DT.General[Lag$Ind.end][['date']], "%y-%m-%d %H:%M"),
                                                                paste0(" (", Lag$len, " mins)"),", applied to the entire period from ",
                                                                format(Meas.DateIN, "%y-%m-%d %H:%M"), " to ", format(Meas.DateEND, "%y-%m-%d %H:%M"),
                                                                paste0(" (", difftime(Meas.DateEND,Meas.DateIN, unit = 'mins')[[1]], " mins)"),
                                                                " gives the best correlation (r=", round(Lag$cor, digit=3),
                                                                ") compared to lagged data (r=", 
                                                                round(cor(DT.General[date %between% c(DT.General[Lag$Ind.start][['date']], DT.General[Lag$Ind.end][['date']]),.SD,.SDcols = c(ColRef,ColSens)],
                                                                          use = "complete.obs")[1,2], digit = 3), ")"))}
                            
                            # New time for reference during the period
                            Ref.names <- c("date", ColRef)
                            DT.General.ref <- DT.General[date >= Meas.DateIN & date <= Meas.DateEND + 1,..Ref.names]
                            data.table::set(DT.General.ref, j = "date", value = DT.General.ref$date - Lag$lag * 60)
                            # Merging DT.General with DT.General.ref, new time Ref for the Period
                            DT.General.period <- data.table::merge.data.table(
                                DT.General[date >= Meas.DateIN & date <= Meas.DateEND + 1, .SD, .SDcols = names(DT.General)[names(DT.General)!=ColRef]],
                                DT.General.ref, by = "date", all.x = TRUE, all.y = FALSE)
                            # Replacing DT.General.period in the whole data.set DT.General
                            l <- list(DT.General[DT.General$date < Meas.DateIN | DT.General$date > Meas.DateEND+1],
                                      DT.General.period)
                            DT.General <- data.table::rbindlist(l, use.names = TRUE)
                            #Resorting with date
                            setkey(DT.General,date)
                        }
                    } else if (Verbose) futile.logger::flog.info(paste0("[DeLag_Pred] there is no lag  or correlation is negative (", round(Lag$cor, digit = 3),
                                                                        ") between ColRef and ColSens, original prediction data are restored."))
                    
                } else if (Verbose) futile.logger::flog.info("[DeLag_Pred] No period with > 120 min to check for lag")
            } else if (Verbose) futile.logger::flog.info("[DeLag_Pred] No data to delag in the selected prediction time period, or, all raw sensor data have been filtered out")
        } else if (Verbose) futile.logger::flog.warn(paste0("[DeLag_Pred] No data to delag in the selected prediction time period."))
    } else if (Verbose) futile.logger::flog.info("[DeLag_Pred] Lag correction not requested. Original reference data during the prediction period are restored.")
    # return(list(DT.General = DT.General, dt.all = dt.all))
    return(DT.General)
}

# Finding Lag using autoccrelation function of 2 vectors
Find_Max_CCF <- function(x,y, Lag.max = 180, auto.sign = TRUE, Positive = TRUE) {
    # https://stackoverflow.com/questions/10369109/finding-lag-at-which-cross-correlation-is-maximum-ccf
    # This function will return the maximum CCF value along with corresponding lag value.
    # x,y      Input to this function are a and b which are nothing but two time series.
    # keep only complete cases
    DataXY <- data.frame(x = x, y = y)
    DataXY <- DataXY[complete.cases(DataXY),]
    d <- stats::ccf(DataXY$x, DataXY$y, plot = FALSE, lag.max = Lag.max) # , lag.max = length(x)/2
    
    cor     = d$acf[,,1]
    abscor  = abs(d$acf[,,1])
    lag     = d$lag[,,1]
    res     = data.frame(cor,lag)
    absres  = data.frame(abscor, lag)
    
    if(auto.sign){
        # automatic determination
        if(res[res$lag ==0,]$cor >= 0){
            absres_max = res[which.max(res$cor),]
        } else  absres_max = res[which.min(res$cor),]
    } else {
        # user define sign of r
        if(Positive){
            absres_max = res[which.max(res$cor),]
        } else {
            # r is negative
            absres_max = res[which.min(res$cor),]}}
    # in case Lag == Lag.max we print a warning message 
    if (abs(absres_max$lag) == Lag.max) futile.logger::flog.warn(paste0("[Find_Max_CCF] Lag is equal to lag.max (", Lag.max, ' min). Carefully check if the lag is real'))
    return(absres_max) 
}
insertRow <- function(existingDF, newrow, r) {
    # Add new row to dataframe, at specific row-index, not appended?
    # https://stackoverflow.com/questions/11561856/add-new-row-to-dataframe-at-specific-row-index-not-appended
    # existingDF   Dataframe to add a row
    # newrow       Added row
    # r             row number where to add newrow to  existingDF
    existingDF <- rbind(existingDF,newrow)
    existingDF <- existingDF[order(c(1:(nrow(existingDF) - 1), r - 0.5)),]
    row.names(existingDF) <- 1:nrow(existingDF)
    return(existingDF)
}
yf_min <- function(yf, DataXY, Verbose = FALSE, name.X = "Out.Temperature", name.Y = "y", r.Sign = "Positive"){ 
    # replaced duplicated name.X with their median
    if (any(duplicated(DataXY[[name.X]]))) DataXY <- DataXY[!duplicated(DataXY[[name.X]])]#
    # ordering in name.X order
    DataXY <- DataXY[order(DataXY[[name.X]]),]
    
    # Compute ratios of increase or decrease by step, assumption distance between xs shall be constant or 1 unit
    # estimate distance of steps
    Tau <- (tail(DataXY[[name.X]], -1) - head(DataXY[[name.X]], -1))
    
    # the subtraction by yf ((head(DataXY$y, -1) - yf)) is because the exponential growth of decay shall start of reach 0 on the assymptote
    # r is standardised by Tau to avoid that changing distances between steps on x.axis results on r discrepancies between steps
    #DataXY[, r := c((tail(DataXY[[name.Y]], -1) - head(DataXY[[name.Y]], -1))/(head(DataXY[[name.Y]], -1) - yf) / Tau, NA)]
    DataXY[, r := c(((tail(DataXY[[name.Y]], -1) - yf) /(head(DataXY[[name.Y]], -1) - yf) - 1)/ Tau, NA)]
    # discarding infinite and r according to r.Sign
    if (any(!is.finite(DataXY$r))) DataXY <- DataXY[-which(!is.finite(r))]
    # browser()
    # if (!(all(na.omit(DataXY$dname.y) > 0) || all(na.omit(DataXY$dname.y) < 0))) {
    #     if( median(DataXY$dname.y, na.rm = T) > 0) {
    #         DataXY <- DataXY[-which(DataXY$r < 0)]
    #     } else if(median(DataXY$dname.y, na.rm = T) < 0) {
    #        DataXY <- DataXY[-which(DataXY$r > 0)]}}
    
    # if("dname.y" %in% names(DataXY)){
    if(r.Sign == "Positive") {
        #if (median(DataXY$dname.y, na.rm = T) > 0){
        if(any(DataXY$r < 0)) DataXY <- DataXY[-which(DataXY$r < 0)]
        #} else if (median(DataXY$dname.y, na.rm = T) < 0){
        #    if(any(DataXY$r > 0)) DataXY <- DataXY[-which(DataXY$r > 0)]}
    } else if(r.Sign == "Negative") {
        #if (median(DataXY$dname.y, na.rm = T) > 0){
        #    if(any(DataXY$r < 0)) DataXY <- DataXY[-which(DataXY$r < 0)]
        #} else if (median(DataXY$dname.y, na.rm = T) < 0){
        if(any(DataXY$r > 0)) DataXY <- DataXY[-which(DataXY$r > 0)]}
    # }
    
    # Normalise r to avoid that lowest r values get an advantage
    DataXY[, r := r/mean(r)]
    
    # Computing unweighted variances between r by steps
    Var <- sqrt(Hmisc::wtd.var(DataXY$r, DataXY$wi[1:nrow(DataXY)],normwt=TRUE))/abs(Hmisc::wtd.mean(DataXY$r,DataXY$wi[1:nrow(DataXY)])) #sd(DataXY$r)^2
    
    if (Verbose) cat(paste0(signif(yf, 5), ", r: ", paste(signif(DataXY$r, 3), collapse = ", "), ", Var of r: ", signif(Var, 4),"\n"))
    return(Var)}

# Functions for model Total Least Square ####
# Define Deming regression function
My.TLS <- function(x, y, data, vr = 1, boot = TRUE, keep.boot = FALSE) {
    
    # using only complete data
    DataXY <- data.table::data.table(x= x, y=y)
    DataXY <- DataXY[is.finite(rowSums(DataXY))]
    stopifnot(nrow(DataXY) >= 3)
    
    # Fit TLS regression using MethComp::Deming and pracma::odregress
    # if boot = TRUE and keep.boot = FALSE, MethComp::Deming returns a matrix with coefficients, se and confidence interval
    fit.MethComp     <- MethComp::Deming(x = DataXY$x, y = DataXY$y, vr = vr, boot = boot, keep.boot = keep.boot)
    # if boot = TRUE and keep.boot = TRUE, MethComp::Deming returns a matrix with Coefficients each row being a subsample of x and y. Ideal to compute covaraince
    fit.MethComp.COV <- MethComp::Deming(x = DataXY$x, y = DataXY$y, vr = vr, boot = boot, keep.boot = TRUE)
    fit.pracma   <- pracma::odregress(x = DataXY$x, y = DataXY$y)
    
    # Create a custom object to store the TLS model
    model <- list(
        coefficients =  rev(fit.pracma$coeff),
        x = DataXY$x,
        y = DataXY$y,
        ssq = fit.pracma$ssq,
        err = fit.pracma$err,
        fitted.values = fit.pracma$fitted,
        residuals = fit.pracma$resid,
        rank = 2, normal = fit.pracma$normal,
        df.residuals = nrow(DataXY) - 1 - 1,
        terms = terms(lm(y~x, data=DataXY)),
        call = "pracma::odregress(x = DataXY$x, y = DataXY$y)",
        fit.MethComp = fit.MethComp,
        fit.MethComp.COV  = fit.MethComp.COV,
        fit.pracma   = fit.pracma
    )
    
    # Assign class to the model for later use
    class(model) <- "My.TLS"
    return(model)
}

# Coefficients accessor function
coefficients.My.TLS <- function(object, ...) {
    object$coefficients
}

# Residuals accessor function
residuals.My.TLS <- function(object, ...) {
    object$residuals
}

# Fitted values accessor function
fitted.My.TLS <- function(object, ...) {
    object$fitted.values
}

# Predict method for the odregres class
predict.My.TLS <- function(object, newdata) {
    
    intercept <- object$coefficients[1]
    slope <- object$coefficients[2]
    
    # Calculate predicted values
    predictions <- intercept + slope * newdata
    return(predictions)
}

# Effects accessor function (not typically used in TLS, so we'll return NA)
effects.My.TLS <- function(object, ...) {
    NA
}
AIC.My.TLS <- function(object, ...) {
    NA
}
BIC.My.TLS <- function(object, ...) {
    NA
}

# the summary() Method
summary.My.TLS <- function(object, ...) {
    
    # # Extract coefficients and residuals from the custom model object
    coefs         <- object$coefficients
    residuals     <- object$residuals
    fitted_values <- object$fitted.values
    # 
    # # Standard error calculation (simplified)
    # n <- length(object$x)
    # se <- sqrt(sum(residuals^2) / (n - 2))  # Standard error (simplified for this case)
    # 
    # # Calculate t-values and p-values
    # t_values <- coefs / se
    # p_values <- 2 * (1 - pt(abs(t_values), df = n - 2))
    # 
    # # Create a summary table for coefficients
    # summary_table <- data.frame(
    #     Estimate = coefs,
    #     `Std. Error` = se,
    #     `t value` = t_values,
    #     `Pr(>|t|)` = p_values
    # )
    # No rather use the output of MethCComp for summary_table
    summary_table <- object$fit.MethComp
    
    # Compute R-squared based on vertical residuals (y)
    #R2 <- 1 - sum(residuals^2) / sum((object$y - mean(object$y))^2)
    # https://stats.stackexchange.com/questions/86453/coefficient-of-determination-of-a-orthogonal-regression/180173
    # Orth.R2 = 1- SSres/SStot where SSres is the sum of squares of residuals, and SStot is the total sum of squares. To apply it in my case I considered:
    # SSres=b id((xi,yi);(x^,y^))B2 where d((xi,yi);(x^,y^)) is the distance of point (xi,yi) to the best fit line.
    # and SStot=b d((xi,yi);(xm,ym))B2 where xm is the mean of all the xi and ym is the mean of all the yi.
    SSres <- object$ssq
    SStot <- sum((object$y - mean(object$y, na.rm = T))^2 + (object$x - mean(object$x, na.rm = T))^2)
    orth.R2 <- 1 - SSres/SStot
    
    # Create a summary list
    result <- list(
        # Summary of MethComp with boot strapping for the estimation of error
        coefficients = summary_table,
        # distance to to xi,yi rather than yi 
        r.squared = orth.R2, 
        residuals = residuals,
        fitted.values = fitted_values
    )
    
    # Set the class to 'summary.My.TLS'
    class(result) <- "summary.My.TLS"
    return(result)
}

#' Fitting Calibration model of sensor data
#' @description This Function estimates calibration functions, plots the calibration line and write the equation above the plot at line_position
#' @description This Function also returns the estimated model as a list object
#' @description The regression equation can be weighted (e.g. 1/sy^2) or not if s_y = NULL
#' @description Lag can be corrected using autocorrelation setting Auto.Lag to TRUE
#' @param x (necessary) numerical vector, x-axis values
#' @param s_x, numerical vector, standard deviation of x values. Same length as x
#' @param y (necessary) numerical vector, y-axis values
#' @param s_y (optional) numerical vector, standard deviation of y values. Same lenght as x is mandatory.
#' @param Mod_type (necessary) type of calibration model: linear, ...
#' @param Multi.File (optional) character vector, default is NULL. If not NULL, File.path of config file used for calibration with "MultiLinear" models
#' @param Matrice (optional) Input Matrix of data (e. g. Pre_cal_df) when Covariates is not NULL
#' @param Covariates (optional) character vector, default is NULL. If not NULL  column names of the covariates found in Matrice and needed for calibration model
#' @param line_position (optional) numeric, default is 0. Line position in marfing at the top for printing the regression equation
#' @param Couleur (optional) character vector, default is "red". Color of the line and color font of the equation
#' @param Sensor_name (optional) character vector, default is NULL. Name of the sensor to be written in front of the calibration equation. If NULL, sensor name is not printed
#' @param f_coef1,f_coef2,f_R2 (optional) character vector, default are f_coef1 = "%.3e", f_coef2 = "%.3e", f_R2 = "%.4f". Number of digits for intercept, slope and R2 using sprintf syntax. f_coef1 is used both for intercept and s(Res), f_coef2 is used for all parameters of the model apart from the intercept
#' @param lim (optional) matrix of column vectors Xlim, YLIM: limits of the plots where to add the calibration line. When NULL the lim are automatically calculated.
#' @param marges(optional) margin of graph, default is c(4,4,3,0.5))
#' @param Weighted (optional) Logical, default is FALSE. If TRUE used weighted fitting base on standard deviations of y for each lag (sd^-2/sum(sd^-2))
#' @param Lag_interval (optional) numerical, double, default sqrt((max(x, na.rm = T) - min(x, na.rm = T)), width of each lag used for estimating lag interval if weighed is TRUE
#' @param Auto.Lag (optional) logical, default is FALSE. If Auto.Lag is TRUE, y is changed using the lag at which cross correlation between ColRef and ColSens is maximum using ccf( ) using auto-correlation and Lag of y versus x, adding NAs at the begining or end of x, y, s_y and Matrice
#' @param Plot_Line (optional) logical, default is TRUE. If TRUE the calibration line is added using par(new=TRUE) to an existing scatterplot
#' @param Ggplot (optional) ggplot object, created with Etalonnage(), to which to add the Cal_line and equation only if Plot_Line is TRUE. If Ggplot is NULL a baseplot is plotted otherwise a ggplot oject is returned and plotted
#' @param Verbose (optional) logical, default TRUE. If TRUE print some messages
#' @param Date (optional) vector of POSIXct, default is null. DateTime  corresponding to each row of x, y values. Used for plotting.
#' @param probs (optional) numeric, default is NULL converted to 0.9 if NULL. default tau value for quantile regression, Mod_type "Linear.robust".
#' @return a list with the estimated calibration model and plot a calibration if Plot_Line is TRUE, Calibration plot in list element "Ggplot".
Cal_Line <- function(x, s_x = NULL, y, s_y = NULL, Mod_type, Probs = NULL,  Multi.File = NULL, Matrice=NULL, Covariates = NULL, 
                     line_position = 0, Couleur = "red", Sensor_name = NULL,
                     f_coef1 = "%.3e", f_coef2 = "%.3e", f_R2 = "%.3f", lim = NULL, marges = c(4,4,3,0.5), 
                     Weighted = FALSE, Lag_interval = diff(range(x))/10, #sqrt(max(x, na.rm = T) - min(x, na.rm = T)), # see https://www.researchgate.net/post/What-is-the-best-sample-variogram-for-kriging
                     Auto.Lag = FALSE, Plot_Line = TRUE, Ggplot = NULL, Verbose = TRUE, Date = NULL) {
    if (Auto.Lag) {
        Lag <- Find_Max_CCF(x,y)
        if (Lag$lag != 0) {
            if (Verbose) futile.logger::flog.info(paste0("[Cal_Line] there is a lag between ColRef and ColSens, of ", Lag$lag," row of data which gives a better correlation between x and y"))
            if (Verbose) futile.logger::flog.info(paste0("[Cal_Line] The lag is corrected before establishing the calibration model."))
            if (Lag$lag > 0) {
                x <- c(x[(Lag$lag+1):length(y)], rep(NA, Lag$lag))
                if(!is.null(s_x)) s_x <- c(s_x[(Lag$lag+1):length(y)], rep(NA, Lag$lag))
            } else {
                x <- c(rep(NA, -Lag$lag),x[1:(length(y) - (-Lag$lag))])
                if(!is.null(s_x)) s_x <- c(rep(NA, Lag$lag), s_x[(Lag$lag+1):length(y)])}
        } else if (Verbose) futile.logger::flog.info(paste0("[Cal_Line] there is no lag between x and y, lag is ", Lag$lag))
    } else if (Verbose) futile.logger::flog.info("[Cal_Line] Lag correction not requested")  
    
    # Put reference and sensor responses and standard deviation of sensor responses in a matrix remove NA of x and y
    DataXY <- data.table(x = x, y = y)
    if (!is.null(s_y) && !any(s_y == 0) && all(!is.na(s_y))){
        DataXY[, s_y := s_y]
        DataXY[, wi := DataXY$s_y^-2/sum(DataXY$s_y^-2, na.rm = T)]}
    if (!is.null(s_x) && !any(s_x == 0) && all(!is.na(s_x))){
        DataXY[,s_x := s_x]}
    
    # Setting covariate names
    if (Mod_type %in% c("Peaks_baseline","exp_kT_NoC","exp_kT", "exp_kTn", "exp_kK","T_power","K_power", "BeerLambert", "Yatkin", "NO2_Lab", "NO2_Lab_decay_inc")){
        name.Temperature <- ifelse(is.null(Covariates), "Out.Temperature",       grep("emp" , Covariates, value = T))
        name.Humidity    <- ifelse(is.null(Covariates), "Out.Relative_humidity", grep("umid", Covariates, value = T))}
    
    # adding Covariates for multilinear model and multivariate models
    if (Mod_type %in% c("MultiLinear", "PLS", "Ridge")){
        DataXY[, (Covariates) := Matrice[, .SD, .SDcol = Covariates]]
    }  else if (Mod_type %in% c("Peaks_baseline","exp_kT_NoC","exp_kT", "exp_kTn", "exp_kK","T_power","K_power", "BeerLambert")) {
        if (is.null(Covariates)) {
            if (nrow(DataXY != nrow(Matrice))) futile.logger::flog.error(paste0("[Cal_line] x, y and Covariates do not have the same length."))
            if (data.table::is.data.table(DataXY))  DataXY[, (name.Temperature) := Matrice[[name.Temperature]]] else if (is.data.frame(DataXY)) DataXY$Temperature <- Matrice[[name.Temperature]]
        } else if (data.table::is.data.table(DataXY)) DataXY[, (Covariates) := Matrice[,..Covariates]] else if (is.data.frame(DataXY)) DataXY[, Covariates] <- Matrice[,..Covariates]
    }  else if (Mod_type %in% c("NO2_Lab", "NO2_Lab_decay_inc")) {
        if (is.null(Covariates)) {
            if (nrow(DataXY != nrow(Matrice))) futile.logger::flog.error(pate0("[Cal_line] x, y and Covariates do not have the same length."))
            if (data.table::is.data.table(DataXY)){
                DataXY[, (name.Temperature) := Matrice[[name.Temperature]]]
                DataXY[, (name.Humidity) := Matrice[[name.Humidity]]]
            } else if (is.data.frame(DataXY)){
                DataXY$Out.Temperature <- Matrice[[name.Temperature]]   
                DataXY$Out.Relative_humidity <- Matrice[["Out.Relative_humidity"]]}
        } else if (data.table::is.data.table(DataXY)) DataXY[, (Covariates) := Matrice[,..Covariates]] else if (is.data.frame(DataXY)) DataXY[, Covariates] <- Matrice[,..Covariates]
    } else if (Mod_type %in% c("BeerLambert")) {
        DataXY[, Out.Atmospheric_pressure := Matrice[["Out.Atmospheric_pressure"]]]
    } else if (Mod_type %in% c("Kohler", "Kohler_lit", "Kohler_only", "Kohler_modified")) {
        DataXY[, (Covariates) := Matrice[[Covariates]]]
    } else if (Mod_type %in% c("Yatkin")) {
        DataXY[, (name.Temperature) := Matrice[[name.Temperature]]]
        DataXY[, (name.Humidity) := Matrice[[name.Humidity]]]}
    
    # Adding date
    if (!is.null(Date)) data.table::set(DataXY, j = "date", value = Date)
    
    # removing NA, NaN, Inf of any variables in DATAXY except "date"
    DataXY <- DataXY[is.finite(rowSums(DataXY[, grep("date", names(DataXY), invert = T), with = F]))]
    
    # Creating weights from the standard deviations within lags
    if (Weighted) {
        #weighing cannot be performed on "date"
        if ("date" %in% names(DataXY)) DataXY[, date := NULL]
        #lag_distance <- round((range(DataXY$x, na.rm = T)[2] - range(DataXY$x, na.rm = T)[1])/Lag_numbers)
        DataXY$ID.x <- DataXY$x %/% (Lag_interval/2)
        setkey(DataXY, ID.x)
        # any covariates?
        if (!is.null(Covariates) && Covariates != "") {
            ID.Covariates <- sapply(Covariates, function(Param) {sqrt(max(DataXY[[Param]], na.rm = T) - min(DataXY[[Param]], na.rm = T))})
            for (Param in Covariates) DataXY[[paste0("ID.", Param)]] <- DataXY[[Param]] %/% (ID.Covariates[[Param]]/2)
            New.Columns <- c("x", "y", Covariates, "s_y", "Count")
            New.IDs     <- c("ID.x", paste0("ID.",names(ID.Covariates)))
            DataXY[, s_y := lapply(.SD, sd, na.rm = T), .SDcols = "y", by = New.IDs]
            DataXY[, Count := .N, by = New.IDs]
            DataXY[, (New.Columns) := lapply(.SD, mean, na.rm = T), .SDcols = New.Columns, by = New.IDs]
            #DataXY[, (New.IDs) := NULL]
        } else {
            DataXY <- DataXY[, .(x = mean(x, na.rm =T), y = mean(y, na.rm =T), s_y = sd(y, na.rm =T), count = .N), by = ID.x]
            #DataXY[, ID.x := NULL]
        }
        DataXY <- unique(DataXY[complete.cases(DataXY) & s_y > 0])
        DataXY[, wi := s_y^-2/sum(DataXY$s_y^-2)]
        #DataXY[, wi := rep(1, times = nrow(DataXY))]
        #DataXY[, c("Count") := NULL]
    }
    # Add ", " at the end of Sensor_name to be print with the legend
    if (!is.null(Sensor_name)) Sensor_name <- paste0(Sensor_name, ", ")
    # Fitting Models
    if (Mod_type == 'Ridge') {
        lambda_seq <- 10^seq(0, -5, by = -10^-1)
        # Trying with caret
        # lm.Crtl  <- caret::trainControl(method = "cv", number = 10)
        # lm.Fit   <- caret::train(y~., method = "lm", data = DataXY[,  c("x", "y", Covariates), with = F], trControl = lm.Crtl)
        # AIC.Fit  <- caret::train(y~., method = "glmStepAIC", direction = "backward", trace = 0,
        #                           data = as.matrix(DataXY[,  c("x", "y", Covariates), with = F]), trControl = lm.Crtl)
        # if s_y is not null calculate weights wi and use them in the regression
        Formula <- as.formula(paste0("y ~ x + ", paste(Covariates, collapse = " + ")))
        if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
            
            # glmnet
            set.seed(123)
            # perform k-fold cross-validation to find optimal lambda value
            Model.All <- glmnet::cv.glmnet(x = as.matrix(DataXY[, c("x", Covariates), with = F]), y = DataXY$y, alpha = 0, lambda = lambda_seq, standardize = T)
            best_lambda <- Model.All$lambda.min
            # Model <- glmnet::glmnet(x = as.matrix(DataXY[, c("x", Covariates), with = F]), y = DataXY$y, alpha = 0, lambda = best_lambda, standardize = T)
            # Best glmnet model - Not used already in cv_model
            # Model <- glmnet(x = as.matrix(DataXY[,  c("x", Covariates), with = F]), y = DataXY$y, alpha = 0, lambda  = best_lambda, standardize = T)
            # Model <- glmnetcr::glmnetcr(x = as.matrix(DataXY[,  c("x", Covariates), with = F]), y = DataXY$y, alpha = 0)
            
            # caret
            #find optimal lambda value that minimizes test MSE
            # glmnet.Fit <- caret::train(y~., method = "glmnet", trace = 0, data = DataXY[,  c("x", "y", Covariates), with = F], trControl = lm.Crtl, alpha = 0)
            
            # lmridge
            # Model.All <- lmridge::lmridge(y~., DataXY[,  c("x", Covariates), with = F], scaling = "sc", K = lambda_seq)
            # best_K <- lambda_seq[which.min(kest(Model.All)$GCV)]
            Model <- lmridge::lmridge(formula = Formula, data = DataXY[,  c("x","y", Covariates), with = F], scaling = "sc", K = best_lambda)
        } else {
            # glmnet
            set.seed(123)
            Model.All <- cv.glmnet(x = as.matrix(DataXY[, c("x", Covariates), with = F]), y = DataXY$y, alpha = 0, lambda = lambda_seq, standardize = T, weights = DataXY[["wi"]])
            best_lambda <- Model.All$lambda.min
            # lmridge
            Model <- lmridge::lmridge(formula = Formula, data = DataXY[,  c("x","y", Covariates), with = F], scaling = "sc", K = best_lambda, weights = DataXY[["wi"]])}
        
        # # glmnet
        # Model[["best_lambda"]] <- best_lambda
        # Model[["predict"]]     <- predict(Model, s = best_lambda, newx = as.matrix(DataXY[,  c("x", Covariates), with = F]))
        # SSE  <- sum((predict(Model, s = best_lambda, newx = as.matrix(DataXY[,  c("x", Covariates), with = F])) - DataXY$y)^2)
        # R2   <- rsquare(true = DataXY$y, predicted =  Model[["predict"]])
        # Rmse <- sqrt(SSE/ (nrow(DataXY) - (length(Covariates) + 1) - 1))
        # Equation <- paste0(Sensor_name, "Ridge: y = ", 
        #                    paste(apply(cbind(round(coef(Model), digits = 3), 
        #                                      gsub(pattern = paste(c("\\(Intercept\\)","Out\\.", "_volt", "_P1"), 
        #                                                           collapse = "|"), replacement = "", names(coef(Model)))),
        #                                MARGIN = 1, paste0, collapse = " "), collapse = " + "),
        #                    sprintf(paste0(",R2=",f_R2, ",RMSE=",f_R2), R2, Rmse))
        # plot(DataXY$y, predict(Model, s = best_lambda, newx = as.matrix(DataXY[,  c("x", Covariates), with = F])))
        
        # lmridge
        # display fitting of raw data, equations and R^2, add data
        Model[["Data"]] <- DataXY[, c("x", "y", Covariates), with = F]
        Model[["Data"]][, .fitted := predict(Model)]
        Model[["Data"]][, .resid  := resid(Model)]
        Equation <- sprintf(paste0(Sensor_name, "Ridge: ", paste(apply(cbind(round(coef(Model), digits = 3), 
                                                                             gsub(pattern = paste(c("\\(Intercept\\)","Out\\.", "_volt", "_P1"), 
                                                                                                  collapse = "|"), replacement = "", names(coef(Model)))),
                                                                       MARGIN = 1, paste0, collapse = " "), collapse = " + "),
                                   ",R2=%.2f,RMSE=%.2f,AIC=%.1f"), # ", s(Res)=",f_coef1,
                            summary(Model)$summaries[["summary  1"]][["stats"]][1,"R2"], sqrt(sum(Model$residuals^2) / Model$df), summary(Model)$summaries[["summary  1"]][["stats"]][1,"AIC"])
        if (Verbose) {
            cat(paste0(Equation, "\n"))
            plot(DataXY$y, predict(Model))}
    } else if (Mod_type == 'PLS') {
        browser()
        DataXY <- DataXY[, -which(names(DataXY) %in% c("date")), with = FALSE]
        training.samples <- createTimeSlices(DataXY$x, initialWindow = 0.5 * nrow(DataXY), horizon = 0.2 * nrow(DataXY), fixedWindow = TRUE, skip = 60)
        train.data <- DataXY[training.samples$train[[1]]]
        test.data  <- DataXY[training.samples$test[[1]]]
        Formula <- as.formula(paste0("y ~ x + ", paste(Covariates, collapse = " + ")))
        myTimeControl <- trainControl(method = "timeslice", initialWindow = 0.5 * nrow(DataXY), horizon = 0.2 * nrow(DataXY), 
                                      fixedWindow = TRUE, skip = 60)
        # PLS Model, if s_y is not null calculate weights wi and use them in the regression
        if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
            set.seed(123)
            Model <- caret::train(y ~ ., data = DataXY, method = "pls", 
                                  preProcess = c("center", "scale"), trControl = myTimeControl)
            #Model <- pls::plsr(Formula, data=train.data, scale=TRUE, validation="CV", model = TRUE, x = TRUE, y = TRUE, ncomp = length(Covariates))
        } else {
            Model <- caret::train(y ~ ., data = DataXY, method = "pls", 
                                  preProcess = c("center", "scale"), trControl = myTimeControl, weights = wi)
            #Model <- pls::plsr(Formula, data=DataXY, scale=TRUE, validation="CV", model = TRUE, x = TRUE, y = TRUE, weights = wi)
        }
        # Caret
        Model
        plot(Model)
        plot(varImp(Model))
        plot(train.data$y, predict(Model, ncomp = 1: Model$bestTune[1,1], train.data))
        plot(test.data$y, predict(Model, ncomp = 1: Model$bestTune[1,1], test.data))
        Model$finalModel$coefficients
        
        #Pls
        validationplot(Model)
        validationplot(Model, val.type="R2")
        validationplot(Model, val.type="MSEP")
        summary(Model)
        plot(RMSEP(Model), legendpos = "topright")
        selectNcomp(Model, "onesigma", plot = TRUE)
        plot(Model, ncomp = 3, asp = 1, line = TRUE)
        plot(Model, plottype = "scores", comps = 1:3)
        explvar(Model)
        plot(train.data$y, predict(Model, ncomp = 3, newdata = train.data))
        plot(test.data$y, predict(Model, ncomp = 3, newdata = test.data))
        Coef(Model)
        #   Equation <- sprintf(paste0(Sensor_name, "PLS: y= %.2f + %.2f x",",R2= %.2f,RMSE= %.2f,AIC= %.1f"), # ", s(Res)=",f_coef1,
        #                       coef(Model)[1], coef(Model)[2], summary(Model)$r.squared, sqrt(sum(Model$residuals^2) / Model$df), AIC(Model))
    } else if (Mod_type == 'GAM_GAUSS') {
        Model <- mgcv::gam(y ~ s(x), family = gaussian(link = identity), data = DataXY)
    } else if (Mod_type == 'TLS') {
        # # Total least square or orthogonal regression  Model, weigths not used
        # Model <- pracma::odregress(x = DataXY$x, y = DataXY$y)
        # browser()
        # Model <- list(coefficients =  rev(Model$coeff), ssq = Model$ssq, err = Model$err, fitted.values = Model$fitted, residuals = Model$resid, rank = 2, normal = Model$normal,
        #               df.residuals = nrow(DataXY) - 1 - 1, terms = terms(lm(y~x, data=DataXY)), call = "pracma::odregress(x = DataXY$x, y = DataXY$y)",
        #               summary = MethComp::Deming(x = DataXY$x, y = DataXY$y, vr = 1, boot = TRUE, keep.boot = FALSE))
        # class(Model) <- "TLS"
        # # https://stats.stackexchange.com/questions/86453/coefficient-of-determination-of-a-orthogonal-regression/180173
        # # R2= 1- SSres/SStot where SSres is the sum of squares of residuals, and SStot is the total sum of squares. To apply it in my case I considered:
        # # SSres=b id((xi,yi);(x^,y^))B2 where d((xi,yi);(x^,y^)) is the distance of point (xi,yi) to the best fit line.
        # # and SStot=b d((xi,yi);(xm,ym))B2 where xm is the mean of all the xi and ym is the mean of all the yi.
        # SSres <- Model$ssq
        # SStot <- sum((DataXY$y - mean(DataXY$y, na.rm = T))^2 + (DataXY$x - mean(DataXY$x, na.rm = T))^2)
        # R2 <- 1 - SSres/SStot
        
        # Using model My.TLS
        Model <- My.TLS(x = DataXY$x, y = DataXY$y)
        
        # display equations and R^2
        # Equation <- sprintf(paste0(Sensor_name, "TLS: y= %.2f + %.2f x",", R2* = %.2f, sd(err.d) = %.2f"),
        #                     coef(Model)[1], coef(Model)[2], R2, sd(Model$err, na.rm = T))
        Equation <- sprintf(paste0(Sensor_name, "TLS: y= %.2f + %.2f x",", R2* = %.2f, sd(err.d) = %.2f"),
                            coef(Model)[1], coef(Model)[2], summary(Model)$r.squared, sd(Model$err, na.rm = T))
        
        # returning Model with standard errors
    } else if (Mod_type == 'Deming') {
        # Fitting Deming regression, it takes care of constant or variable Errors on x and y, weigths not used
        Model <- deming::deming(as.formula("y ~ x"), data=DataXY, xstd=s_x, ystd=s_y, jackknife=T, dfbeta = F)
        
        # In case of Deming = TLS we can calculate a pseudo R2 otherwiser have al look to https://www.tandfonline.com/doi/pdf/10.1080/03610926.2022.2059678
        if(identical(DataXY$s_x, DataXY$s_y)){
            # https://stats.stackexchange.com/questions/86453/coefficient-of-determination-of-a-orthogonal-regression/180173
            # R2= 1- SSres/SStot where SSres is the sum of squares of residuals, and SStot is the total sum of squares. To apply it in my case I considered:
            # SSres=b id((xi,yi);(x^,y^))B2 where d((xi,yi);(x^,y^)) is the distance of point (xi,yi) to the best fit line.
            # and SStot=b d((xi,yi);(xm,ym))B2 where xm is the mean of all the xi and ym is the mean of all the yi.
            SSres <- Model$ssq
            SStot <- sum((DataXY$y - mean(DataXY$y, na.rm = T))^2 + (DataXY$x - mean(DataXY$x, na.rm = T))^2)
            R2 <- 1 - SSres/SStot
        } else R2 = NA 
        
        # display equations and R^2
        Equation <- sprintf(paste0(Sensor_name, "Deming: y= %.2f + %.2f x",", R2*= %.2f, sd(res)= %.2f"),
                            coef(Model)[1], coef(Model)[2], R2, sd(Model$residuals, na.rm = T))
        # returning Model with standard errors
    } else if (Mod_type == 'Identity') {
        # Linear Model, with slope = 1 and intercept = 0
        Model <- stats::nls(y~a0 + a1*x, algorithm="port", start=c(a0=0,a1=1),lower=c(a0=0,a1=1),upper=c(a=0,b=1), data = DataXY, model=TRUE)
        # display equations and R^2
        Equation <- sprintf(paste0(Sensor_name, "Identity: y= %.2f + %.2f x",",R2= %.2f,RMSE= %.2f,AIC= %.1f"), # ", s(Res)=",f_coef1,
                            coef(Model)[1], coef(Model)[2],
                            1 - sum((DataXY$y-coef(Model)[2]*DataXY$x)^2, na.rm=TRUE)/sum((DataXY$y-mean(DataXY$y, na.rm =T))^2, na.rm=TRUE),
                            qpcR::RMSE(Model), AIC(Model))
    } else if (Mod_type == 'Linear') {
        # Linear Model, if s_y is not null calculate weights wi and use them in the regression
        if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
            Model <- lm(y ~ x, data = DataXY, model = TRUE, x = TRUE, y = TRUE)
        } else {
            Model <- lm(y ~ x, data = DataXY, model = TRUE, x = TRUE, y = TRUE, weights = wi)}
        # display equations and R^2
        Equation <- sprintf(paste0(Sensor_name, "Linear: y= ",f_coef1,"+ ",f_coef2," x",",R2= %.2f,RMSE= %.2f,AIC= %.1f"), # ", s(Res)=",f_coef1,
                            coef(Model)[1], coef(Model)[2], summary(Model)$r.squared, sqrt(sum(Model$residuals^2) / Model$df), AIC(Model))
    } else if (Mod_type == 'Linear.Robust') {
        # MGV Robust Linear Model, (if s_y is not null calculate weights wi and use them in the regression
        # This models the median of y as a function of x, rather than modelling the mean of y as a function of x, in the case of least squares regression.
        if (is.null(Probs)) Probs = 0.9
        if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
            Model <- quantreg::rq(y ~ x, data = DataXY, tau = Probs, model = TRUE) # Model <- quantreg::rq(y ~ x, data = DataXY, tau = seq(0, 1, by = 0.1), model = TRUE, method = "br")
        } else {
            Model <- quantreg::rq(y ~ x, data = DataXY, weights = wi, tau = Probs, model = TRUE)}
        Equation <- sprintf(paste0("P%.0fth (%.1f) quantile reg.: y= ",f_coef1,"+ ",f_coef2," x", ", R1= %.2f, RMSE=",f_coef1,",AIC= %.1f"),
                            round(100*Probs), quantile(DataXY$x, probs = Probs),coef(Model)[1], coef(Model)[2], R1_rq(DataXY$x, DataXY$y, probs = Probs), sqrt(sum(Model$residuals^2) / summary(Model)$rdf), AIC(Model))
    } else if (Mod_type == 'Linear.Robust.rqs') {
        # MGV Robust Linear Model, (if s_y is not null calculate weights wi and use them in the regression
        # This models the a number of quantile of y as a function of x, rather than modelling the mean or median of y as a function of x, in the case of least squares regression.
        if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
            Model <- quantreg::rq(y ~ x, data = DataXY, tau = seq(0.1, 0.9, by = 0.1), model = TRUE, method = "br")
        } else {
            Model <- quantreg::rq(y ~ x, data = DataXY, weights = wi, tau = seq(0, 1, by = 0.1), model = TRUE)}
        Equation <- sprintf(paste0("Quantile regression tau 0,0.1...1: y= ",f_coef1,"+ ",f_coef2," x", ", RMSE=",f_coef1,",AIC= %.1f"),
                            coef(Model)[1], coef(Model)[2], sqrt(sum(Model$residuals^2) / summary(Model)$rdf), AIC(Model))
    } else if (Mod_type == 'gam') {
        Estimated <- data.frame(x = x, y = y)
        if (is.null(s_y) || any(s_y == 0) || all(is.na(s_y))) {
            # Let gam89 decide for k and estimate the best alpha
            Model <- mgcv::gam(y~s(x), family = Gamma(link = log), data = Estimated)
            #Model <- gam(y~s(x, k=5), family=Gamma(link=log), data = Estimated)
        } else {
            # Let gam decide for k and estimate the best alpha
            #Model <- gam(y~s(x, k=5), family=Gamma(link=log), weights = wi)
            Model <- mgcv::gam(y~s(x), family = Gamma(link = log), data = Estimated, weights = wi)}
        Equation <- sprintf(paste0("General Additive model"))
    } else if (Mod_type %in% c("Yatkin",'MassonO', 'Peaks_baseline','exp_kT_NoC','exp_kT','exp_kTn','exp_kK', 'T_power', 'K_power', 'Exp_Translations')) {
        # Setting Initial values of the linear relationship between sensor and reference using the smallest temperatures: T< Qantile(0.33) and T < 20 degrress
        if (Sensor_name %in% c("NO_B4_P1, ", "CO_A4_P1, ", "NO2_B43F_P1, ")) {
            # For NO
            LowT <- which(DataXY[[name.Temperature]] < quantile(DataXY[[name.Temperature]], probs = c(0.33)) & DataXY[[name.Temperature]] < 20)
            if (!length(LowT) > 0) LowT <- which(DataXY[[name.Temperature]] < quantile(DataXY[[name.Temperature]], probs = c(0.33)))
        } else if (Sensor_name == "NO2_B43F_P1, ") {
            # For NO2, possibly for others
            LowT <-     DataXY[DataXY[[name.Temperature]] < quantile(DataXY[[name.Temperature]], probs = c(0.25)), which = T]
            LowT.RH <-  DataXY[DataXY[[name.Temperature]] < quantile(DataXY[[name.Temperature]], probs = c(0.25)) &
                                   DataXY[[name.Humidity]] > quantile(DataXY[[name.Humidity]], probs = c(0.75)) & 
                                   DataXY[["Absolute_humidity"]]     > quantile(DataXY[["Absolute_humidity"]], probs = c(0.75)) &
                                   DataXY[["x"]]<  15, which = T]
            LowT <- LowT.RH}
        Linear.Model <- quantreg::rq(y ~ x, data = DataXY[LowT], tau = c(0.5))
        coeff <- round(Linear.Model$coefficients , 2)
        A0    <- unname(coef(Linear.Model)[1])
        A1    <- unname(coef(Linear.Model)[2])
        # Preparing for log(R - (a0+a1x)])
        DataXY[, y.corr := y - (A1 * x + A0)]
        # Plotting initial values A0 and A1 for high x values/low name.Temperature
        plot(DataXY[LowT,c("x","y")], 
             main = paste0("first a0 and a1 at low temperature, ", format(min(DataXY$date), format = "%Y%m%d"), " - ", format(max(DataXY$date), format = "%Y%m%d")))
        grid(); abline(a = A0, b = A1, col = "red")
        text(x = range(DataXY[LowT,x], na.rm = T)[1], y = range(DataXY[LowT,y], na.rm = T)[2], pos = 4, paste("rq: y = ",coeff[1] , " + " , coeff[2] , "*x" ))
        text(x = range(DataXY[LowT,x], na.rm = T)[2], y = range(DataXY[LowT,y], na.rm = T)[1], pos = 2, paste("R2 = ",round(summary(lm(y ~ x, data = DataXY[LowT]))$r.squared, digits = 2)))
        
        # determining initial k values at high temperature when x is low
        if (Weighted || Sensor_name == "NO2_B43F_P1, ") {
            LowX <- which(DataXY[["x"]] <= quantile(DataXY[["x"]], probs = c(0.25)))
            LowX.HighRH <- DataXY[x <= quantile(DataXY[["x"]], probs = c(0.10))  & 
                                      DataXY[[name.Humidity]] > quantile(DataXY[[name.Humidity]], probs = c(0.75)), which = T]
        } else if (Sensor_name == "NO_B4_P1, ") {
            #LowX <- which(DataXY[,x] <= quantile(DataXY[["x"]], probs = c(0.25)) & DataXY[[name.Temperature]] > quantile(DataXY[[name.Temperature]], probs = c(0.66)))
            LowX <- DataXY[x <= quantile(DataXY[["x"]], probs = c(0.10)), which = T]}
        
        # Choosing the correction model for name.Temperature
        if (Mod_type == 'Peaks_baseline') {
            # Baseline correction for temperature effect
            # checking for positives values for the log((RNo - a1 x)/a0)
            # Model in celsius degrees
            Positives <- which((DataXY[LowX]$y - A1 * DataXY[LowX]$x)/A0 > 0)
            DataXY[LowX[Positives],"Init.Coeffs"] <- log((DataXY[LowX][Positives,"y"] - A1 * DataXY[LowX][Positives,"x"])/A0)
            # Fit without intercept, only the slope k
            Model.0 <- quantreg::rq(Init.Coeffs ~ get(name.Temperature) - 1, data = DataXY[LowX[Positives],], tau = c(0.5))
            plot(DataXY[LowX][[name.Temperature]], DataXY[LowX,y])
            grid()
            DataXY[LowX, Predict := predict(Model.0, DataXY[LowX][[name.Temperature]])]
            lines(DataXY[LowX, .SD, .SDcols = c(name.Temperature, "Predict")][order(DataXY[LowX][[name.Temperature]])], col = "red")
            # Fitting model
            Formula <- as.formula(paste0("y ~ f_exp_kT_NoC(x, a0, a1, k, ",name.Temperature,") - Y0"))
            Starting.values <- list(a0 = unname(coef(Model.0)[1]), a1 = A1, k = unname(coef(Model.0)[2]), Y0 = unname(coef(Model.0)[3]))
            if (Verbose) print(Starting.values, quote = F)
            if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
                Model <- minpack.lm::nlsLM(Formula, data = DataXY, start = Starting.values, model = TRUE, control = nls.lm.control(maxiter = 1024, maxfev = 10000))
            } else {
                Model <- minpack.lm::nlsLM(Formula, data = DataXY, start = Starting.values, model = TRUE, control = nls.lm.control(maxiter = 1024, maxfev = 10000), weights = wi) # , lower = c(0, 0.0000000001, 0, 0.00000000001))
            }
            # display equations and R^2
            Equation <- sprintf(paste0(Sensor_name, "exp_kT_NoC: y = ",f_coef1," exp(",f_coef2," T_Celsius)+ ",f_coef2," x, RMSE=",f_coef1,",AIC= %.1f"),
                                coef(Model)[1],coef(Model)[3],coef(Model)[2],
                                sqrt(sum(resid(Model)^2) / df.residual(Model)),
                                AIC(Model))
        } else  if (Mod_type == 'Yatkin') {
            # Model in Celsius degrees RNO = a0 + a1 NO + b0 + b1 RH + C exp(kT)
            denHum  <- density(DataXY[[name.Humidity]])
            denTemp <- density(DataXY[[name.Temperature]])
            denRef  <- density(DataXY[["x"]])
            par(mfrow = c(2, 2))
            plot(denHum, frame = FALSE, col = "blue",main = "Humidity density plot");grid()
            plot(denTemp, frame = FALSE, col = "red",main = "Temperature density plot");grid()
            plot(denRef, frame = FALSE, col = "green",main = "Ref. density plot (95th prercent.)", xlim = c(min(DataXY[["x"]], na.rm = T), quantile(DataXY[["x"]], probs = 0.95)));grid()
            
            # Looking for the index of increasing and decreasing RH, if this serve anything?
            # Mov_avg_RH <- MazamaRollUtils::roll_mean(DataXY$Out.Relative_humidity, width = 31, align = "center") #  no, it adds NA
            #DataXY[, (paste0(name.Humidity, "_MA")) := signal::sgolayfilt(DataXY$Out.Relative_humidity)]
            DataXY[, (paste0(name.Humidity, "_MA")) := rollapply(DataXY[[name.Humidity]], width = 61, median, align = "center", partial = TRUE)]
            plot(DataXY[, c("date", name.Humidity), with = F], type="p", main = "determining increasing decreasing humidity", col = "blue", cex = 0.5); grid()
            name.Humidity <- paste0(name.Humidity, "_MA")
            lines(DataXY[, c("date", name.Humidity), with = F], col = "red")
            # print(ggplot(data = DataXY, aes(x = date)) +
            #     #geom_point(aes(y = Out.Relative_humidity),  color = "blue") +
            #     geom_point(aes(y = Out.Relative_humidity),  color = "RH")) +
            #     scale_color_manual(values = c("RH"="blue"))
            #     geom_line(aes(y = Out.Relative_humidity_MA, color = "RH_moving_median")) + 
            #     theme(legend.title=element_blank(),
            #           legend.text = element_text(family="Times", color = "black", size = 16,face="bold")) +
            #     scale_color_manual(values = c("RH"="blue",  "Temp"="red"))
            DataXY[, dRH := c(diff(DataXY[[name.Humidity]]), NA)] # BE CAREFUL WHEN THE RH sensor does not work or restart
            DataXY[, dRH :=rollapply(DataXY[["dRH"]], width = 11, median, align = "center", partial = TRUE)] # median avergae to avoid touching dRH/dt = 0
            cutt_off <- 0.000
            RH_Inc  <- DataXY[dRH > cutt_off, which = T]
            RH_Flat <- DataXY[dRH >= -cutt_off & dRH <= cutt_off, which = T]
            RH_Dec  <- DataXY[dRH <= -cutt_off, which = T]
            rm(cutt_off)
            data.table::set(DataXY, i = RH_Inc,  j = "RH_sign", value = "blue")
            data.table::set(DataXY, i = RH_Flat, j = "RH_sign", value = "green")
            data.table::set(DataXY, i = RH_Dec,  j = "RH_sign", value = "red")
            
            # Discarding noise of y and T as well
            DataXY[, y_MA := rollapply(DataXY[["y"]], width = 61, median, align = "center", partial = TRUE)]
            DataXY[, y.corr_MA := rollapply(DataXY[["y.corr"]], width = 61, median, align = "center", partial = TRUE)]
            DataXY[, (paste0(name.Temperature, "_MA")) := rollapply(DataXY[[name.Temperature]], width = 61, median, align = "center", partial = TRUE)]
            name.Temperature <- paste0(name.Temperature, "_MA")
            
            # Determining effect of temperature with decreasing, flat and increasing RH
            print(ggplot(data = DataXY) +
                      ylab(NULL) +
                      geom_line(aes(x = date, y = y_MA,                  color = "Raw.sensor_MA")) +
                      geom_line(aes(x = date, y = x,                     color = "Reference")) +
                      # see shquoate https://stackoverflow.com/questions/28777626/how-do-i-combine-aes-and-aes-string-options
                      geom_line(aes_string(x ="date", name.Temperature,  color = shQuote("Temp_MA"))) +
                      geom_line(aes_string(x ="date", y = name.Humidity, color = shQuote("RH_MA"))) +
                      geom_line(aes(x = date, y = 20 * dRH,              color = "20xdRH_dt")) +
                      ggtitle(paste0("Median average sensor, T and RH and slope of RH")) +
                      theme(legend.title=element_blank(),
                            legend.text = element_text(family="Times", color = "black", size = 16,face="bold")) +
                      scale_color_manual(values = c("Raw.sensor_MA"="orange", "Reference"="green", "Temp_MA"="red", "RH_MA"="blue", "20xdRH_dt" = "black")))
            
            RH_th <- 1.00
            T_th  <- 1.00
            x_th  <- 0.25
            par(mfrow = c(2, 2))
            LowX.LowRH <- DataXY[(x < quantile(DataXY[["x"]], probs = x_th) | x <= 2) & DataXY[[name.Humidity]] < quantile(DataXY[[name.Humidity]], probs = RH_th), which = T]
            plot(DataXY[, c("x", "y_MA")], col = "blue", main = "Raw sensor (Median average) vs reference, all data"); grid()
            plot(DataXY[LowX.LowRH][, c(name.Temperature, "y_MA"), with = F], col = DataXY[LowX.LowRH]$RH_sign,
                 main = paste0("Raw sensor (Median average) vs T for low x(<",sprintf("%0.0f%%", 100 * x_th),") and all RH(<", sprintf("%0.0f%%", 100 * RH_th),")")); grid()
            for(RH in c("RH_Inc","RH_Dec")){ # , "RH_Flat"
                LowX.TrendRH <- DataXY[get(RH)][(x <= quantile(DataXY[get(RH)][["x"]], probs = x_th) | x <= 3) & DataXY[get(RH)][[name.Humidity]] <= quantile(DataXY[get(RH)][[name.Humidity]], probs = RH_th), which = T]
                plot(DataXY[get(RH)][LowX.TrendRH][, c(name.Temperature, "y_MA"), with = F], col = DataXY[get(RH)]$RH_sign,
                     main = paste0("Temperature effect on sensor for low x(<",sprintf("%0.0f%%", 100 * x_th),") and RH(<",sprintf("%0.0f%%", 100 * RH_th),") ", RH)); grid()
                # browser()
                # Checking linearity or exponential model
                Formula <- as.formula(paste0("y_MA ~ ", name.Temperature))
                if(summary(lm(Formula, data = DataXY[get(RH)]))$r.squared < 0.95){
                    #compute yf corresponding to the lowest change of ratio per step, when relative variance of r per step is smallest
                    yf <- pracma::fminbnd(f = yf_min,
                                          a = ifelse(sign(min(DataXY[get(RH)][LowX.TrendRH]$y_MA, na.rm = T))==-1, 5 * min(DataXY[get(RH)][LowX.TrendRH]$y_MA, na.rm = T), 0.1 * min(DataXY[get(RH)][LowX.TrendRH]$y_MA, na.rm = T)),
                                          b = ifelse(sign(max(DataXY[get(RH)][LowX.TrendRH]$y_MA, na.rm = T))==-1, min(DataXY[get(RH)][LowX.TrendRH]$y_MA, na.rm = T)    , min(DataXY[get(RH)][LowX.TrendRH]$y_MA, na.rm = T)),
                                          DataXY = DataXY[get(RH)][LowX.TrendRH], name.X = name.Temperature, name.Y = "y_MA")$xmin
                    # initial estimate of r
                    r2 <- (tail(DataXY[get(RH)][LowX.TrendRH]$y_MA, -1) - head(DataXY[get(RH)][LowX.TrendRH]$y_MA, -1))/(head(DataXY[get(RH)][LowX.TrendRH]$y_MA, -1) - yf) /
                        (tail(DataXY[get(RH)][LowX.TrendRH][[name.Temperature]], -1) - head(DataXY[get(RH)][LowX.TrendRH][[name.Temperature]], -1))
                    r2 <- median(r2[!r2==0 & is.finite(r2)], na.rm = T)
                    # if the estimated r2 is smaller than -1 there is an impossibility 1 - r2 < 0 which cannot be. Let's try another method
                    if (r2 <= -1){
                        r1  <- median((tail(DataXY[get(RH)]$y_MA, -1) - head(DataXY[get(RH)]$y_MA, -1))/(head(DataXY[get(RH)]$y_MA, -1) - yf))  
                        Tau <- median(tail(DataXY[get(RH)][[name.Temperature]], -1) - head(DataXY[get(RH)][[name.Temperature]], -1), na.rm = T)
                        # convert r from tau to per degrees, not necessary anymore r2 is already divided by Tau changing per steps
                        r2 <- exp(log(1+r1)/Tau) - 1}
                    # Still check r2 > -1
                    stopifnot(r2 > -1)
                    T0 <- min(DataXY[get(RH)][[name.Temperature]], na.rm = T)
                    # take the median for TO as there might be several y_MA value at TO due to median averaging
                    y0 <- median(DataXY[get(RH)][DataXY[get(RH)][[name.Temperature]] == T0,]$y_MA, na.rm = T)
                    # Changing y0 at T = 20 ?C, to set T0 to constant and get a huge range of T variation
                    y0 <- yf+(y0-yf)*(1+r2)^(20-T0)
                    f_P_T <- function(T, yf, y0, r, T0) return(yf + (y0 - yf) * (1+r)^(T - 20))
                    Formula <- as.formula(paste0("y_MA ~ f_P_T(T = ",name.Temperature, ", yf, y0, r)"))
                    Starting.values <- list(yf = yf, y0 = y0, r = r2)
                    if(r2 > 0){ # Exponential growth
                        Lower.values  <- c(-Inf, -Inf,    0)
                        Upper.values  <- c(+Inf, +Inf, +Inf)
                    } else { # Exponential decay
                        Lower.values  <- c(-Inf, -Inf, -Inf)
                        Upper.values  <- c(+Inf, +Inf,    0)}
                    if (!any(grepl("wi", names(DataXY[get(RH)]))) || !all(is.finite(DataXY[get(RH)]$s_y))) {
                        assign(paste0("Model_", RH), tryCatch(minpack.lm::nlsLM(Formula, data = DataXY[get(RH)][LowX.TrendRH], start = Starting.values, model = TRUE, 
                                                                                control = nls.lm.control(maxiter = 1024, maxfev = 10000), 
                                                                                lower = Lower.values, upper = Upper.values),
                                                              error = function(e) nlsLM(Formula, data = DataXY[get(RH)][LowX.TrendRH], start = Starting.values, model = TRUE, 
                                                                                        control = nls.lm.control(maxiter = 1024, maxfev = 10000), 
                                                                                        lower = Lower.values, upper = Upper.values)))
                    } else {
                        assign(paste0("Model_", RH), tryCatch(minpack.lm::nlsLM(Formula, data = DataXY[get(RH)][LowX.TrendRH], start = Starting.values, model = TRUE, 
                                                                                control = nls.lm.control(maxiter = 1024, maxfev = 10000), 
                                                                                lower = Lower.values, upper = Upper.values, weights = wi), 
                                                              error = function(e) nlsLM(Formula, data = DataXY[get(RH)][LowX.TrendRH], start = Starting.values, model = TRUE, 
                                                                                        control = nls.lm.control(maxiter = 1024, maxfev = 10000), 
                                                                                        lower = Lower.values, upper = Upper.values, weights = wi)))}
                    
                    # Equation
                    Equation <- paste0("Power_T RH ",RH,": y = ",signif(coef(get(paste0("Model_", RH)))["yf"],3)," + (", 
                                       signif(coef(get(paste0("Model_", RH)))["y0"],3), " - ", signif(coef(get(paste0("Model_", RH)))["yf"],3),") ",signif(1+coef(get(paste0("Model_", RH)))["r"],3),
                                       "^(T - 20), s(Res)=",signif(sd(resid(get(paste0("Model_", RH)))), 3),", RMSE=",
                                       signif(sqrt(sum(resid(get(paste0("Model_", RH)))^2) / summary(get(paste0("Model_", RH)))$df[2]), 3),", AIC= ", signif(AIC(get(paste0("Model_", RH))),3))
                    
                    # Correcting sensor response y for Temperature
                    if(RH == "RH_Dec") All.i <- c(RH_Dec) else All.i <- get(RH) # It seems that all RH_Inc is exponential while RH_Dec is not, , RH_Flat
                    data.table::set(DataXY, i = All.i, j = "y.corr.T_MA",
                                    value = DataXY[All.i]$y_MA - (coef(get(paste0("Model_", RH)))["yf"] +
                                                                      (coef(get(paste0("Model_", RH)))["y0"] - coef(get(paste0("Model_", RH)))["yf"]) *
                                                                      (1 + coef(get(paste0("Model_", RH)))["r"])^(DataXY[All.i][[name.Temperature]] - 20)))
                } else {
                    if (!any(grepl("wi", names(DataXY[get(RH)]))) || !all(is.finite(DataXY[get(RH)]$s_y))) {
                        assign(paste0("Model_", RH), lm(Formula, data = DataXY[get(RH)][LowX.TrendRH], model = TRUE))
                    } else {
                        assign(paste0("Model_", RH), lm(Formula, data = DataXY[get(RH)][LowX.TrendRH], model = TRUE, weights = wi))}
                    
                    # Equation
                    Equation <- paste0("Linear_T RH ",RH,": y = ",signif(coef(get(paste0("Model_", RH)))[1],3)," + ", 
                                       signif(coef(get(paste0("Model_", RH)))[ name.Temperature],3), " T, R2=",signif(summary(lm(Formula, data = DataXY[get(RH)]))$r.squared, 3),
                                       ", s(Res)=",signif(sd(resid(get(paste0("Model_", RH)))), 3),", RMSE=",
                                       signif(sqrt(sum(resid(get(paste0("Model_", RH)))^2) / summary(get(paste0("Model_", RH)))$df[2]), 3),", AIC= ", signif(AIC(get(paste0("Model_", RH))),3))
                    
                    # Correcting sensor response y for Temperature
                    if(RH == "RH_Dec") All.i <- c(RH_Dec) else All.i <- get(RH) # It seems that all RH_Inc is exponential while RH_Dec is not, , RH_Flat
                    data.table::set(DataXY, i = All.i, j = "y.corr.T_MA",
                                    value = DataXY[All.i]$y_MA - (coef(get(paste0("Model_", RH)))[1] + coef(get(paste0("Model_", RH)))[name.Temperature] * DataXY[All.i][[name.Temperature]]))
                }
                print(summary(get(paste0("Model_", RH))))
                # browser()
                Estimated <- broom.mixed::augment(get(paste0("Model_", RH)))
                Estimated <- Estimated[order(Estimated[[name.Temperature]]),]
                lines(Estimated[,c(name.Temperature, ".fitted")], col = Couleur)
                
                # Plotting equation
                mtext(Equation, line = line_position, adj = 1, padj = 0, col = Couleur, cex = 0.875)}
            
            # Plotting the correction of T (and RH combined?)
            print(ggplot(data = DataXY) +
                      ylab(NULL) +
                      geom_line(aes(x = date, y = y_MA,                     color = "Raw.sensor_MA")) +
                      geom_line(aes(x = date, y = y.corr.T_MA,              color = "T_corr.sensor_MA")) +
                      geom_line(aes(x = date, y = x,                        color = "Reference")) +
                      geom_line(aes_string(x = "date", y = name.Temperature, color = shQuote("Temp_MA"))) +
                      geom_line(aes_string(x = "date", y = name.Humidity,    color = shQuote("RH_MA"))) + 
                      theme(legend.title=element_blank(),
                            legend.text = element_text(family="Times", color = "black", size = 16,face="bold")) +
                      scale_color_manual(values = c("Raw.sensor_MA"="orange", "Reference"="green", "Temp_MA"="red", "RH_MA"="blue","T_corr.sensor_MA"="black")))
            
            # Checking for any remaining effect of RH
            # browser()
            par(mfrow = c(2, 2))
            RH_th <- 1.0
            T_th  <- 0.33
            x_th  <- 0.25
            for(RH in c("RH_Inc","RH_Dec")){ # , "RH_Flat"
                LowX      <- DataXY[get(RH)][x <= quantile(DataXY[get(RH)][["x"]], probs = x_th) | x <= 3, which = T]
                plot(DataXY[get(RH)[LowX],c(name.Humidity,"y.corr.T_MA"), with = F], col = DataXY[get(RH)][LowX]$RH_sign,
                     main = paste0("Corr. T, low x(",x_th,"), all T and RH (", RH, "), ",
                                   format(min(DataXY[get(RH)[LowX]]$date), format = "%Y%m%d"), " - ", format(max(DataXY[get(RH)][LowX]$date), format = "%Y%m%d"))); grid()
                Formula = as.formula(paste0("y.corr.T_MA ~ ",name.Humidity))
                LM.Humidity <- lm(Formula, data = DataXY[get(RH)][LowX])
                assign(paste0("R2_", RH, "_all.T"), summary(LM.Humidity)$r.squared)
                abline(LM.Humidity) 
                legend("topleft",legend=paste("R2 is", format(summary(LM.Humidity)$r.squared,digits=3)))
                
                LowX.LowT <- DataXY[get(RH)][(x <= quantile(DataXY[get(RH)][["x"]], probs = x_th)  | x <= 1) & 
                                                 DataXY[get(RH)][[name.Temperature]] < quantile(DataXY[get(RH)][[name.Temperature]], probs = T_th), which = T]
                if(length(LowX.LowT) > 0){
                    plot(DataXY[get(RH)[LowX.LowT],c(name.Humidity,"y.corr.T_MA"), with = F],  col = DataXY[get(RH)][LowX.LowT]$RH_sign,
                         main = paste0("Corr. T, low x(",x_th,") and Low T(",T_th,"), all RH (", RH, "), ",
                                       format(min(DataXY[get(RH)[LowX.LowT]]$date), format = "%Y%m%d"), " - ", format(max(DataXY[get(RH)[LowX.LowT]]$date), format = "%Y%m%d"))); grid()
                    LM.Humidity <- lm(Formula, data = DataXY[get(RH)][LowX.LowT])
                    assign(paste0("R2_", RH, "_Low.T"), summary(LM.Humidity)$r.squared)
                    abline(LM.Humidity) 
                    legend("topleft",legend=paste("R2 is", format(summary(LM.Humidity)$r.squared,digits=3)))}}
            
            # Checking if residuals after T correction are correlated with RH
            R2_length <-c("R2_RH_Inc_all.T", "R2_RH_Inc_Low.T", "R2_RH_Dec_all.T", "R2_RH_Dec_Low.T") # , "R2_RH_Flat_all.T", "R2_RH_Flat_Low.T"
            for (R2 in R2_length){
                if (is.finite(R2) &&  R2> 0.6){
                    flog.warn(paste0("After T correction there is still a dependence on humidity", R2))
                } else flog.info(paste0("After T correction there is no dependence on humidity left on sensor responses for", R2))}
            
            # Determining A0 and A 1 after y correction for Temperature first at high humidity and low Temperature with increasing or decreasing RH, and then for all data
            for(RH in c("RH_Inc","RH_Dec")){
                plot(DataXY[get(RH)][,c("x","y.corr.T_MA")], col = DataXY[get(RH)]$RH_sign,
                     main = paste0("y correct. for T, all x, T and RH (", RH, "), ",
                                   format(min(DataXY$date), format = "%Y%m%d"), " - ", format(max(DataXY$date), format = "%Y%m%d"))); grid()
                
                # Fitting initial A0 and A1 for increasing or decreasing RH, it will be used in the final model
                assign(paste0("Linear.Model_",RH), Cal_Line(x = DataXY[get(RH)]$x, y = DataXY[get(RH)]$y.corr.T_MA, Mod_type = "Linear.Robust", Weighted = F, Probs = 0.5, line_position = -1))
                coeff <- round(get(paste0("Linear.Model_",RH))$coefficients , 2)
                a0    <- unname(coeff[1])
                a1    <- unname(coeff[2])
                # Correcting for NO effect after T
                data.table::set(DataXY, i = get(RH), j = "y.corr.T.NO", value = DataXY[get(RH)]$y.corr.T_MA - (a0 + a1 * DataXY[get(RH)]$x))
                
                # Fitting initial A0 and A1 for increasing or decreasing RH high x values/low name.Temperature
                LowT.HighRH <-  DataXY[get(RH)][DataXY[get(RH)][[name.Temperature]] < quantile(DataXY[get(RH)][[name.Temperature]], probs = 0.33) & 
                                                    DataXY[get(RH)][[name.Humidity]]    > quantile(DataXY[get(RH)][[name.Humidity]]   , probs = 0.66), which = T]
                plot(DataXY[get(RH)][LowT.HighRH][,c("x","y.corr.T_MA")], col = DataXY[get(RH)][LowT.HighRH]$RH_sign,
                     main = paste0("y correct. for T, all x, low T and high RH (", RH, "), ",
                                   format(min(DataXY$date), format = "%Y%m%d"), " - ", format(max(DataXY$date), format = "%Y%m%d"))); grid()
                # Fitting a0 and a1
                Linear.Model <- Cal_Line(x = DataXY[get(RH)[LowT.HighRH]]$x, y = DataXY[get(RH)[LowT.HighRH]]$y.corr.T_MA, Mod_type = "Linear.Robust", Weighted = F, Probs = 0.5, line_position = -1)}
            
            # Global determination of a0 and a1, finally not useful: a0 and a1 seems to be different if RH increases or decreases
            plot(DataXY[,c("x","y.corr.T_MA")],  col = DataXY[LowT.HighRH]$RH_sign,
                 main = paste0("y correct. for T, all x, T and RH , ",
                               format(min(DataXY$date), format = "%Y%m%d"), " - ", format(max(DataXY$date), format = "%Y%m%d"))); grid()
            Linear.Model <- Cal_Line(x = DataXY$x, y = DataXY$y.corr.T_MA, Mod_type = "Linear.Robust", Weighted = T, Probs = 0.5, line_position = -1)
            coeff <- round(Linear.Model$coefficients , 2)
            A0    <- unname(coef(Linear.Model)[1])
            A1    <- unname(coef(Linear.Model)[2])
            # Correcting for NO effect
            #data.table::set(DataXY, j = "y.corr.T.NO", value = DataXY$y.corr.T_MA - (A0 + A1 * DataXY$x))
            plot(DataXY[, c("x", "y.corr.T.NO")], main = "Y correct. for T and NO for all x, T and RH", col = DataXY$RH_sign,); grid()
            
            # in the following there is the initial method of determination of model, first a0 and a1 and then temperature/humidity.
            # It does not work because the effect of T and RH is bigger than the effect of pollutant
            # # Determining A0 and A 1 at high humidity and low Temperature
            # LowT.HighRH <-  DataXY[DataXY[[name.Temperature]] < quantile(DataXY[[name.Temperature]], probs = 0.2) &
            #                            DataXY[[name.Humidity]]    > quantile(DataXY[[name.Humidity]]   , probs = 0.75), which = T]
            # denHum  <- density(DataXY[LowT.HighRH][[name.Humidity]])
            # denTemp <- density(DataXY[LowT.HighRH][[name.Temperature]])
            # denRef  <- density(DataXY[LowT.HighRH][["x"]])
            # par(mfrow = c(2, 2))
            # plot(denHum, frame = FALSE, col = "blue",main = "High humidity density plot");grid()
            # plot(denTemp, frame = FALSE, col = "red",main = "Low temperature density plot");grid()
            # plot(denRef, frame = FALSE, col = "green",main = "Ref. density plot, Low T - high RH", xlim = c(min(DataXY[["x"]], na.rm = T), quantile(DataXY[["x"]], probs = 0.920)));grid()
            # 
            # # Plotting initial values A0 and A1 for high x values/low name.Temperature
            # plot(DataXY[LowT.HighRH,c("x","y")], 
            #      main = paste0("Initial a0 and a1 without T correction, ", format(min(DataXY$date), format = "%Y%m%d"), " - ", format(max(DataXY$date), format = "%Y%m%d"))); grid()
            # # Fitting initial A0 and A1
            # Linear.Model <- Cal_Line(x = DataXY[LowT.HighRH]$x, y = DataXY[LowT.HighRH]$y, Mod_type = "Linear.Robust", Weighted = T, Probs = 0.5, line_position = -1)
            # coeff <- round(Linear.Model$coefficients , 2)
            # a0    <- unname(coef(Linear.Model)[1])
            # a1    <- unname(coef(Linear.Model)[2])
            # # Correcting for NO effect
            # DataXY[, y.corr.NO := y - (A0 + A1 * x)]
            # 
            # # Looking for Temperature effect after NO corrected and at low RH only to get hthe high Temperature?
            # # Effect of Temperature with corrected NO, all RH and high RH with increasing and decreasing humidity
            # for(RH in c("RH_Inc","RH_Dec")){
            #     AllX       <- DataXY[get(RH)][x <= quantile(DataXY[get(RH)][["x"]], probs = c(1.00)), which = T] 
            #     AllX.LowRH <- DataXY[get(RH)][x <= quantile(DataXY[get(RH)][["x"]], probs = c(1.00)) &
            #                                       DataXY[get(RH)][[name.Humidity]] < quantile(DataXY[get(RH)][[name.Humidity]], probs = c(0.25)), which = T]
            #     plot(DataXY[get(RH)][AllX,c(name.Temperature,"y.corr.NO"), with = F], 
            #          main = paste0("Corr. NO, all RH ", ifelse(RH == "RH_Inc","increasing", "decreasing"), ", ",
            #                        format(min(DataXY[get(RH)][AllX]$date), format = "%Y%m%d"), " - ", format(max(DataXY[get(RH)][AllX]$date), format = "%Y%m%d"))); grid()
            #     plot(DataXY[get(RH)][AllX.LowRH,c(name.Temperature,"y.corr.NO"), with = F], 
            #          main = paste0("Corr. NO, Low RH ", ifelse(RH == "RH_Inc","increasing", "decreasing"), ", ",
            #                        format(min(DataXY[get(RH)][AllX.LowRH]$date), format = "%Y%m%d"), " - ", format(max(DataXY[get(RH)][AllX.LowRH]$date), format = "%Y%m%d"))); grid()}
            # par(mfrow = c(1, 1))
            # AllX.LowRH <- DataXY[x <= quantile(DataXY[["x"]], probs = c(1.00)) & DataXY[[name.Humidity]] < quantile(DataXY[[name.Humidity]], probs = c(0.25)), which = T]
            # DataXY.T <- DataXY[AllX.LowRH][order(DataXY[AllX.LowRH][[name.Temperature]]),]
            # # Weighing cannot be performed on "date"
            # if ("date" %in% names(DataXY.T)) DataXY.T[, date := NULL]
            # # Weighing
            # New.Columns <- names(DataXY.T)
            # New.ID <- paste0("ID.", name.Temperature)
            # DataXY.T[, paste0("ID.", name.Temperature) := DataXY.T[[name.Temperature]] %/% (1)]
            # DataXY.T[, s_y.corr := lapply(.SD, sd, na.rm = T), .SDcols = "y", by = New.ID]
            # DataXY.T[, Count := .N, New.ID]
            # DataXY.T[, (New.Columns) := lapply(.SD, median, na.rm = T), .SDcols = New.Columns, by = New.ID]
            # DataXY.T <- unique(DataXY.T)
            # DataXY.T[, wi := s_y.corr^-2/sum(DataXY.T$s_y.corr^-2, na.rm = T)]
            # 
            # #compute yf corresponding to the lowest change of ratio per step, when relative variance of r per step is smallest
            # yf <- pracma::fminbnd(f = yf_min,
            #                       a = ifelse(sign(min(DataXY.T$y.corr, na.rm = T))==-1, 5 * min(DataXY.T$y.corr, na.rm = T), 0.1 * min(DataXY.T$y.corr, na.rm = T)),
            #                       b = ifelse(sign(max(DataXY.T$y.corr, na.rm = T))==-1, min(DataXY.T$y.corr, na.rm = T)    , min(DataXY.T$y.corr, na.rm = T)),
            #                       DataXY = DataXY.T, name.X = name.Temperature, name.Y = "y.corr")$xmin
            # r2 <- median((tail(DataXY.T$y.corr, -1) - head(DataXY.T$y.corr, -1))/(head(DataXY.T$y.corr, -1) - yf) / (tail(DataXY.T[[name.Temperature]], -1) - head(DataXY.T[[name.Temperature]], -1)), na.rm = T)
            # # if the estimated r2 is smaller than -1 there is an impossibility 1 - r2 < 0 which cannot be. Let's try another method
            # if (r2 <= -1){
            #     r1  <- median((tail(DataXY.T$y.corr, -1) - head(DataXY.T$y.corr, -1))/(head(DataXY.T$y.corr, -1) - yf))  
            #     Tau <- median(tail(DataXY.T[[name.Temperature]], -1) - head(DataXY.T[[name.Temperature]], -1), na.rm = T)
            #     # convert r from tau to per degrees, not necessary anymore r2 is already divided by Tau changing per steps
            #     r2 <- exp(log(1+r1)/Tau) - 1}
            # # Still check r2 > -1
            # stopifnot(r2 > -1)
            # T0 <- min(DataXY.T[[name.Temperature]], na.rm = T)
            # y0 <- DataXY.T[DataXY.T[[name.Temperature]] == T0,]$y.corr
            # f_P_T <- function(T, yf, y0, r, T0) return(yf + (y0 - yf) * (1+r)^(T - T0))
            # Formula <- as.formula(paste0("y.corr ~ f_P_T(T = ",name.Temperature, ", yf, y0, r, T0 = ", T0,")"))
            # Starting.values <- list(yf = yf, y0 = y0, r = r2)
            # if(r2 > 0){ # Exponential growth
            #     Lower.values  <- c(-Inf, -Inf,    0)
            #     Upper.values  <- c(+Inf, +Inf, +Inf)
            # } else { # Exponential decay
            #     Lower.values  <- c(-Inf, -Inf, -Inf)
            #     Upper.values  <- c(+Inf, +Inf,    0)}
            # 
            # if (!any(grepl("wi", names(DataXY.T))) || !all(is.finite(DataXY.T$s_y.corr))) {
            #     Model <- minpack.lm::nlsLM(Formula, data = DataXY.T, start = Starting.values, model = TRUE, 
            #                                control = nls.lm.control(maxiter = 1024, maxfev = 10000), 
            #                                lower = Lower.values, upper = Upper.values)
            # } else {
            #     Model <- minpack.lm::nlsLM(Formula, data = DataXY.T, start = Starting.values, model = TRUE, 
            #                                control = nls.lm.control(maxiter = 1024, maxfev = 10000), 
            #                                lower = Lower.values, upper = Upper.values, weights = wi)}
            # plot(broom.mixed::augment(Model)[,c(name.Temperature, "y.corr")], xlab = name.Temperature, ylab = "sensor responses, NO corrected in nA", col = "blue", main = "Temperature effect on sensor response corrected for x"); grid()
            # lines(broom.mixed::augment(Model)[,c(name.Temperature, ".fitted")], col = Couleur)
            # DataXY[, y.corr.NO.T := y.corr - (coef(Model)["yf"] + (coef(Model)["y0"] - coef(Model)["yf"]) * (1 + coef(Model)["r"])^(DataXY[[name.Temperature]] - T0))]
            # 
            # # Effect of humidity at low NO, all T and low T with increasing and decreasing humidity
            # par(mfrow = c(1, 1))
            # for(RH in c("RH_Inc","RH_Dec")){
            #     # LowX      <- DataXY[get(RH)][x <= quantile(DataXY[get(RH)][["x"]], probs = c(0.10)), which = T]
            #     LowX.LowT <- DataXY[get(RH)][x <= quantile(DataXY[get(RH)][["x"]], probs = c(0.33))  &
            #                                      DataXY[get(RH)][[name.Temperature]] < quantile(DataXY[get(RH)][[name.Temperature]], probs = c(0.33)), which = T]
            #     # plot(DataXY[get(RH)][LowX,c(name.Humidity,"y.corr.NO"), with = F], 
            #     #      main = paste0("NO corr., all T, ", ifelse(RH == "RH_Inc","increasing", "decreasing"), " RH, ",
            #     #                    format(min(DataXY[get(RH)][LowX]$date), format = "%Y%m%d"), " - ", format(max(DataXY[get(RH)][LowX]$date), format = "%Y%m%d"))); grid()
            #     plot(DataXY[get(RH)][LowX.LowT][,c(name.Humidity,"y.corr.NO.T"), with = F], 
            #          main = paste0("NO and T corr., ", ifelse(RH == "RH_Inc","increasing", "decreasing"), " RH, ",
            #                        format(min(DataXY[get(RH)]$date), format = "%Y%m%d"), " - ", format(max(DataXY[get(RH)]$date), format = "%Y%m%d"))); grid()
            #     Formula = as.formula(paste0("y.corr.NO.T ~ ",name.Humidity))
            #     LM.Humidity <- lm(Formula, data = DataXY[get(RH)][,c(name.Humidity,"y.corr.NO.T"), with = F])
            #     assign(paste0("R2_", RH), summary(LM.Humidity)$r.squared)}
            # par(mfrow = c(1, 1))
            # 
            # determining initial a0, a1, yf, y0 and r for the whole dataset
            # if (all(c(R2_RH_Inc,R2_RH_Dec) < 0.25) && length(LowX) > 0) {
            #     
            #     
            #     if (Verbose) print(Starting.values, quote = F)
            #     if (is.null(DataXY.T$wi) || !is.finite(DataXY.T$wi) || any(DataXY.T$wi == 0) ) {
            #         Model <- minpack.lm::nlsLM(Formula, data = DataXY.T, start = Starting.values, model = TRUE, control = nls.lm.control(maxiter = 1024, maxfev = 10000))
            #     } else {
            #         Model <- minpack.lm::nlsLM(Formula, data = DataXY.T, start = Starting.values, model = TRUE, control = nls.lm.control(maxiter = 1024, maxfev = 10000), weights = wi) # , lower = c(0, 0.0000000001, 0, 0.00000000001))
            #     }
            #     plot(DataXY$y,f_P_T(DataXY$x, A0, Starting.values$a1, Starting.values$yf, Starting.values$y0, Starting.values$r, DataXY[[name.Temperature]], T0 = 11.9794766155489)); grid()
            #     plot(DataXY$x, (DataXY$y - (A0 + Starting.values$yf + (Starting.values$y0 - Starting.values$yf) * (1+Starting.values$r)^(DataXY$Out.Temperature - T0))) / Starting.values$a1); grid()
            #     plot(DataXY$y, A0 + Starting.values$a1 * DataXY$x); grid()
            #     plot(DataXY$y, A0 + Starting.values$a1 * DataXY$x + Starting.values$yf + (Starting.values$y0 - Starting.values$yf)*(1 + Starting.values$r)^( DataXY$Out.Temperature - Starting.values$T0)); grid()
            #     
            #     # checking for positives values for the log((RNo - a1 x)/a0)
            #     Positives <- which(DataXY[LowX]$y.corr/A0 > 0)
            #     if (length(Positives) > 0) {
            #         data.table::set(DataXY, i = LowX[Positives], j = "Init.Coeffs", value = log(DataXY[LowX][Positives]$y.corr/A0))
            #         T.min   <- min(DataXY[LowX][Positives]$Out.Temperature_int)
            #         init.name.Temperature <- name.Temperature
            #         name.Temperature      <- "Normalised.Temperature"
            #         data.table::set(DataXY, j = name.Temperature, value = DataXY$Out.Temperature_int - T.min)
            #         log.min <- coef(quantreg::rq(as.formula(paste0("Init.Coeffs ~ " , name.Temperature)), data = DataXY[LowX][Positives], tau = c(0.5)))["(Intercept)"]
            #         #log.min <- min(DataXY[LowX][Positives]$Init.Coeffs)
            #         name.Log <- "Normalised.Log"
            #         data.table::set(DataXY, j = name.Log, value = DataXY$Init.Coeffs - log.min)
            #         Formula <- as.formula(paste0(name.Log," ~ " , name.Temperature, " - 1"))
            #         #Formula <- as.formula(paste0("Init.Coeffs ~ " , name.Temperature, " - 1"))
            #         #Formula <- as.formula(paste0("Init.Coeffs  ~ name.Temperature -1"))
            #         Model.0 <- quantreg::rq(Formula, data = DataXY[LowX][Positives], tau = c(0.5))
            #         plot(DataXY[LowX][Positives, .SD, .SDcols = c(name.Temperature, name.Log)], xlab = paste0(name.Temperature, ", T - T.min"),ylab = "log ((y - a1 x)/a0) - log.min", 
            #              main = "Temperature effect at low x, Initial k value for log of sensor values")
            #         mtext(side=3, line=0, adj=1, cex=1, paste0("T.min = ", round(T.min, digits = 1), ", log.min = ", round(log.min, digits = 3), ", k = ",round (coef(Model.0), digits = 5),"; ",format(min(DataXY$date), format = "%Y%m%d"), " - ", format(max(DataXY$date), format = "%Y%m%d")))
            #         grid()
            #         abline(a = 0, b = coef(Model.0)[1], col=2, lwd=2)
            #         coeff <- round(Model.0$coefficients , 4)
            #         text(x = range(DataXY[LowX][Positives,..name.Temperature], na.rm = T)[1], y = range(DataXY[LowX][Positives,..name.Log], na.rm = T)[2], pos = 4,
            #              paste("Model : y = ",coeff[1] , "*x" ))
            #         # plotting y - a1 x = c exp(k(T-Tmin) + log.min)
            #         plot(DataXY[LowX][Positives, .SD, .SDcols = c(init.name.Temperature, "y.corr")], xlab = name.Temperature,ylab = "y - a1 x",
            #              main = paste0("Temperature at low x, k value ", format(min(DataXY$date), format = "%Y%m%d"), " - ", format(max(DataXY$date), format = "%Y%m%d")))
            #         grid()
            #         data.table::set(DataXY, j = "Cexp_kT", value = A0 * exp(unname(coef(Model.0)[1]) * (DataXY$Out.Temperature_int - T.min)))
            #         data.table::set(DataXY, j = "Cexp_kT_log.min", value = A0 * exp(coef(Model.0)[1] * (DataXY$Out.Temperature_int - T.min) + log.min))
            #         lines(setorderv(DataXY[LowX][Positives, .SD, .SDcols = c(init.name.Temperature, "Cexp_kT_log.min")], cols = init.name.Temperature), type = "l", col = "red")
            #         mtext(side=3, line=0, adj=1, cex=1, paste0("y - ", round(A1, digits = 3), " x = ", round(A0, digits = 3), 
            #                                                    " exp(", round (coef(Model.0), digits = 5), " (T - ", round(T.min, digits = 1),") + ", round(log.min, digits = 3), ")"))
            #         # initial values see https://stats.stackexchange.com/questions/160552/why-is-nls-giving-me-singular-gradient-matrix-at-initial-parameter-estimates
            #         Starting.values <- list(a1 = A1, C = A0 * exp(unname(log.min)), k = unname(coef(Model.0)[1]))
            #     } else {
            #         # checking for positives values for the log(RNo - a1 x)
            #         Positives <- which(DataXY[LowX]$y.corr > 0)
            #         if (length(Positives) > 0) {
            #             data.table::set(DataXY, i = LowX[Positives], j = "Init.Coeffs", value = log(DataXY[LowX][Positives]$y.corr))
            #             Formula <- as.formula(paste0("Init.Coeffs ~ ",name.Temperature))
            #             Model.0 <- quantreg::rq(Formula, data = DataXY[LowX][Positives], tau = c(0.5))
            #             plot(DataXY[LowX][Positives, .SD, .SDcols = c(name.Temperature, "Init.Coeffs")], 
            #                  xlab = name.Temperature,ylab = "log(y - a1 x)", main = "Temperature at low x, Initial k value")
            #             grid()
            #             abline(a = coef(Model.0)[1], b = coef(Model.0)[2], col=2, lwd=2)
            #             coeff <- round(Model.0$coefficients , 4)
            #             text(x = range(DataXY[LowX][Positives,..name.Temperature], na.rm = T)[1], y = range(DataXY[LowX][Positives,Init.Coeffs], na.rm = T)[2], pos = 4,
            #                  paste("Model : y = ", coef(Model.0)[1], " + ",coeff[2] , "*x" ))
            #             # initial values see https://stats.stackexchange.com/questions/160552/why-is-nls-giving-me-singular-gradient-matrix-at-initial-parameter-estimates
            #             Starting.values <- list(a1 = A1, C = exp(unname(coef(Model.0)[1])), k = unname(coef(Model.0)[2]))
            #         } else stop(futile.logger::flog.error("[Cal_Line] (y - a1 x)/a0 and y - a1 x are always negative at high ",name.Temperature,". Cannot compute logarithmic."))}
            # } else stop(futile.logger::flog.error("[Cal_Line] no high temperature sensor data with low x values."))
            # # Plotting 
            # car::scatter3d(y ~ x + Out.Temperature_int, data = DataXY[LowX], point.col = "blue", surface= F)
            # Sensor        <- DataXY[["y"]]
            # Fitted.Sensor <- coef(Model)["a1"] * DataXY$x + coef(Model)["C"] * exp(coef(Model)["k"] * (DataXY$Out.Temperature_int - T.min))
            # plot(Sensor, Fitted.Sensor, 
            #      main = paste0("Sensor calibrated ", format(min(DataXY$date), format = "%Y%m%d"), " - ", format(max(DataXY$date), format = "%Y%m%d")))
            # mtext(side=3, line=0, adj=1, cex=1, paste0("y = ", round(coef(Model)["a1"], digits = 3), " x + ", round(coef(Model)["C"], digits = 3), 
            #                                            " exp(", round (coef(Model)["k"], digits = 3), " (T - ", round(T.min, digits = 1),") + ", round(log.min, digits = 3), ")"))
            # grid();abline(a = 0, b = 1, col = "red")
            # text(x = range(Sensor, na.rm = T)[2], y = range(Fitted.Sensor, na.rm = T)[1], pos = 2, "Identity line", col = "red")
            # Linear.Fitted <- lm(Fitted.Sensor ~ Sensor)
            # Coeff <- round(Linear.Fitted$coefficients , 2)
            # text(x = range(Sensor, na.rm = T)[1], y = range(Fitted.Sensor, na.rm = T)[2], pos = 4, paste("y = ", Coeff[1], " + ",Coeff[2] , "*x" ))
            # text(x = range(Sensor, na.rm = T)[1], y = range(Fitted.Sensor, na.rm = T)[2] - 0.05 * diff(range(Fitted.Sensor, na.rm = T)) , 
            #      pos = 4, paste0("R2 = ", round(summary(Linear.Fitted)$r.squared, digits = 2)))
            # # display equations and R^2
            # Equation <- sprintf(paste0(Sensor_name, "exp_kT_NoC: y = ",f_coef2," x + ",f_coef1," exp(",f_coef2," (T_Celsius - ", round(T.min, digits = 2),"), RMSE=",f_coef1,", AIC= %.1f"),
            #                     coef(Model)[1],coef(Model)[2],coef(Model)[3],
            #                     sqrt(sum(Model$residuals^2) / Model$df),
            #                     AIC(Model))
            
            # Final Model
            for(RH in c("RH_Inc","RH_Dec")){
                # Weighing
                # The data are weighted because there are a few data changing with x and lot of them changing with T otherwise a0 and a1 will not be honored and T correction will be overwhelming
                DataXY.T <- DataXY[get(RH)][order(DataXY[get(RH)][[name.Temperature]]),c("x", "y", name.Temperature), with = F] #, name.Humidity
                # ID on x
                DataXY.T[, ID.x := DataXY.T$x %/% (Lag_interval/2)]
                setkey(DataXY.T, ID.x)
                # covariates T and RH
                Covariates <- c(name.Temperature) # , name.Humidity
                ID.Covariates <- sapply(Covariates, function(Param) {sqrt(max(DataXY.T[[Param]], na.rm = T) - min(DataXY.T[[Param]], na.rm = T))})
                for (Param in Covariates) DataXY.T[, paste0("ID.", Param) := DataXY.T[[Param]] %/% (ID.Covariates[[Param]]/2)]
                New.Columns <- c("x", "y", Covariates, "s_y", "Count")
                New.IDs     <- c("ID.x", paste0("ID.",names(ID.Covariates)))
                DataXY.T[, s_y := lapply(.SD, sd, na.rm = T), .SDcols = "y", by = New.IDs]
                DataXY.T[, Count := .N, by = New.IDs]
                DataXY.T[, (New.Columns) := lapply(.SD, mean, na.rm = T), .SDcols = New.Columns, by = New.IDs]
                DataXY.T <- unique(DataXY.T[complete.cases(DataXY.T) & s_y > 0 & Count > 1])
                DataXY.T[, wi := s_y^-2/sum(DataXY.T$s_y^-2)]
                
                # Model either nlsLM or lm for T correction
                if(any(grepl("nlsLM", get(paste0("Model_",RH))$call))){
                    f_P_T <- function(a0, a1, x, yf, y0, r, T) return(a0 + a1 * x + yf + (y0 - yf) * (1+r)^(T - 20))   
                    Formula <- as.formula(paste0("y ~ f_P_T(a0 = a0, a1 = a1, x = x, yf = yf, y0 = y0, r = r, T = ", name.Temperature, ")"))
                    Starting.values <- list(a0 = unname(coef(get(paste0("Linear.Model_",RH)))[1]),
                                            a1 = unname(coef(get(paste0("Linear.Model_",RH)))[2]), 
                                            yf = unname(coef(get(paste0("Model_", RH)))["yf"]),
                                            y0 = unname(coef(get(paste0("Model_", RH)))["y0"]),
                                            r  = unname(coef(get(paste0("Model_", RH)))["r"]))
                    # Upper lower interval for starting values allowing only 10 % of changes for a0 and a1
                    if(r2 > 0){ # Exponential growth
                        Lower.values  <- c(0.95*Starting.values$a0, 0.95*Starting.values$a1, -Inf, -Inf,    0)
                        Upper.values  <- c(1.10*Starting.values$a0, 1.10*Starting.values$a1, +Inf, +Inf, +Inf)
                    } else { # Exponential decay
                        Lower.values  <- c(0.95*Starting.values$a0, 0.95*Starting.values$a1, -Inf, -Inf, -1)
                        Upper.values  <- c(1.10*Starting.values$a0, 1.10*Starting.values$a1, +Inf, +Inf,  0)}
                    if (!any(grepl("wi", names(DataXY.T))) || !all(is.finite(DataXY.T$s_y))) {
                        assign(paste0("Model_", RH), tryCatch(minpack.lm::nlsLM(Formula, data = DataXY.T, start = Starting.values, model = TRUE, 
                                                                                control = nls.lm.control(maxiter = 1024, maxfev = 10000), 
                                                                                lower = Lower.values, upper = Upper.values), 
                                                              error = function(e) nlsLM(Formula, data = DataXY.T, start = Starting.values, model = TRUE, 
                                                                                        control = nls.lm.control(maxiter = 1024, maxfev = 10000), 
                                                                                        lower = Lower.values, upper = Upper.values)))
                        
                    } else {
                        assign(paste0("Model_", RH), tryCatch(minpack.lm::nlsLM(Formula, data = DataXY.T, start = Starting.values, model = TRUE, 
                                                                                control = nls.lm.control(maxiter = 1024, maxfev = 10000), 
                                                                                lower = Lower.values, upper = Upper.values, weights = wi),
                                                              error = function (e) nlsLM(Formula, data = DataXY.T, start = Starting.values, model = TRUE, 
                                                                                         control = nls.lm.control(maxiter = 1024, maxfev = 10000), 
                                                                                         lower = Lower.values, upper = Upper.values, weights = wi)))}
                    # Equation 
                    assign(paste0("Equation_", RH), paste0("Power_T RH ",RH,": y = ",
                                                           signif(coef(get(paste0("Model_", RH)))["a0"],3)," + ", signif(coef(get(paste0("Model_", RH)))["a1"],3),"  x + ",
                                                           signif(coef(get(paste0("Model_", RH)))["yf"],3)," + (", 
                                                           signif(coef(get(paste0("Model_", RH)))["y0"],3), " - ", signif(coef(get(paste0("Model_", RH)))["yf"],3),") ",signif(1+coef(get(paste0("Model_", RH)))["r"],3),
                                                           "^(T - 20), s(Res)=",signif(sd(resid(get(paste0("Model_", RH)))), 3),", RMSE=",
                                                           signif(sqrt(sum(resid(get(paste0("Model_", RH)))^2) / summary(get(paste0("Model_", RH)))$df[2]), 3),", AIC= ", signif(AIC(get(paste0("Model_", RH))),3)))
                    
                    # Correcting sensor response y for Temperature
                    data.table::set(DataXY, i = get(RH), j = "y.corr.T.NO",
                                    value = DataXY[get(RH)]$y - (coef(get(paste0("Model_", RH)))["a0"] + coef(get(paste0("Model_", RH)))["a1"] * DataXY[get(RH)]$x + 
                                                                     coef(get(paste0("Model_", RH)))["yf"] + (coef(get(paste0("Model_", RH)))["y0"] - coef(get(paste0("Model_", RH)))["yf"]) *
                                                                     (1 + coef(get(paste0("Model_", RH)))["r"])^(DataXY[get(RH)][[name.Temperature]] - 20)))
                    data.table::set(DataXY, i = get(RH), j = "x.fitted",
                                    value = (DataXY[get(RH)]$y - (coef(get(paste0("Model_", RH)))["a0"] + 
                                                                      coef(get(paste0("Model_", RH)))["yf"] + (coef(get(paste0("Model_", RH)))["y0"] - coef(get(paste0("Model_", RH)))["yf"]) *
                                                                      (1 + coef(get(paste0("Model_", RH)))["r"])^(DataXY[get(RH)][[name.Temperature]] - 20)))/ coef(get(paste0("Model_", RH)))["a1"])
                    
                } else {
                    f_T <- function(a0, a1, x, y1, T) return(a0 + a1 * x + y1 * T)   
                    Formula <- as.formula(paste0("y ~ f_T(a0 = a0, a1 = a1, x = x, y1 = y1, T = ", name.Temperature, ")"))
                    Starting.values <- list(a0 = unname(coef(get(paste0("Linear.Model_",RH)))[1] + coef(get(paste0("Model_", RH)))[1]),
                                            a1 = unname(coef(get(paste0("Linear.Model_",RH)))[2]), 
                                            y1 = unname(coef(get(paste0("Model_", RH)))[2]))
                    # Upper lower interval for starting values allowing only 10 % of changes for a0 and a1
                    Lower.values  <- c(-Inf, -Inf, -Inf)
                    Upper.values  <- c(+Inf, +Inf, +Inf)
                    browser()
                    if (!any(grepl("wi", names(DataXY.T))) || !all(is.finite(DataXY.T$s_y))) {
                        assign(paste0("Model_", RH), tryCatch(minpack.lm::nlsLM(Formula, data = DataXY.T, start = Starting.values, model = TRUE, 
                                                                                control = nls.lm.control(maxiter = 1024, maxfev = 10000), 
                                                                                lower = Lower.values, upper = Upper.values),
                                                              error = function(e) nlsLM(Formula, data = DataXY.T, start = Starting.values, model = TRUE, 
                                                                                        control = nls.lm.control(maxiter = 1024, maxfev = 10000), 
                                                                                        lower = Lower.values, upper = Upper.values)))
                    } else {
                        assign(paste0("Model_", RH), tryCatch(minpack.lm::nlsLM(Formula, data = DataXY.T, start = Starting.values, model = TRUE, 
                                                                                control = nls.lm.control(maxiter = 1024, maxfev = 10000), 
                                                                                lower = Lower.values, upper = Upper.values, weights = wi), 
                                                              error = function (e) nlsLM(Formula, data = DataXY.T, start = Starting.values, model = TRUE, 
                                                                                         control = nls.lm.control(maxiter = 1024, maxfev = 10000), 
                                                                                         lower = Lower.values, upper = Upper.values, weights = wi)))}
                    # Equation 
                    assign(paste0("Equation_", RH), paste0("Power_T RH ",RH,": y = ",
                                                           signif(coef(get(paste0("Model_", RH)))["a0"],3)," + ", signif(coef(get(paste0("Model_", RH)))["a1"],3),"  x + ",
                                                           signif(coef(get(paste0("Model_", RH)))["y1"],3)," * T, s(Res)=",signif(sd(resid(get(paste0("Model_", RH)))), 3),", RMSE=",
                                                           signif(sqrt(sum(resid(get(paste0("Model_", RH)))^2) / summary(get(paste0("Model_", RH)))$df[2]), 3),", AIC= ", signif(AIC(get(paste0("Model_", RH))),3)))
                    
                    data.table::set(DataXY, i = get(RH), j = "y.corr.T.NO",
                                    value = DataXY[get(RH)]$y - (coef(get(paste0("Model_", RH)))["a0"] + coef(get(paste0("Model_", RH)))["a1"] * DataXY[get(RH)]$x + 
                                                                     coef(get(paste0("Model_", RH)))["y1"] * DataXY[get(RH)][[name.Temperature]]))
                    data.table::set(DataXY, i = get(RH), j = "x.fitted",
                                    value = (DataXY[get(RH)]$y - (coef(get(paste0("Model_", RH)))["a0"] + 
                                                                      coef(get(paste0("Model_", RH)))["y1"] * DataXY[get(RH)][[name.Temperature]]))/ coef(get(paste0("Model_", RH)))["a1"])
                }
                print(summary(get(paste0("Model_", RH))))
                plot(broom.mixed::augment(get(paste0("Model_", RH)))[,c("x", "y")], xlab = "Reference in nA", ylab = "Raw sensor in nA", col = "blue", 
                     main = paste0("Final Calibration model, RH ", RH)); grid()
                Estimated <- broom.mixed::augment(get(paste0("Model_", RH)))
                Estimated <- Estimated[order(Estimated$x),]
                points(Estimated[,c("x", ".fitted")], col = Couleur)
                
                # Plotting the equation 
                mtext(get(paste0("Equation_", RH)), line = line_position, adj = 1, padj = 0, col = Couleur, cex = 0.875)
                
                # Correcting sensor response y for Temperature
                plot(DataXY[get(RH)][,c("x", "y.corr.T.NO")], xlab = "Reference (x)", ylab = "Sensor  responses in nA corrected for temperature and x, RH ",
                     col = DataXY[get(RH)]$RH_sign, main = paste0("Residuals of final Calibration model, RH ", RH)); grid()
                plot(DataXY[get(RH)][,c("x", "x.fitted")], xlab = "Reference (x)", ylab = "Predicted sensor  responses corrected for temperature and x, RH ", 
                     col = DataXY[get(RH)]$RH_sign, main = paste0("Calibrated sensor, final Calibration model, RH ", RH)); grid()}
            
            # Plotting the whole calibration
            plot(DataXY[, c("x", "x.fitted")], xlab = "Reference (x)", ylab = "Predicted sensor responses, same unit as x-axis", col =  DataXY[get(RH)]$RH_sign,
                 main = paste0("Calibrated sensor, final Calibration model, all data")); grid()
            
            # for returning the final model, putting the model RH_Inc and RH_Dec together in Model
            Model <- list(Model_RH_Inc = Model_RH_Inc, Model_RH_Dec = Model_RH_Dec, Equation_RH_Inc = Equation_RH_Inc, Equation_RH_Dec = Equation_RH_Dec, Data = DataXY)
            
        } else if (Mod_type == 'exp_kT_NoC') {
            # Model in Celsius degrees RNO = a1 NO + C exp(kT)
            # # Fit without intercept a0, only the slope a1 and k. C is a0
            DataXY[, y.corr := y - (A1 * x)]
            # determining initial C and k values at high temperature
            if (length(LowX) > 0) {
                # checking for positives values for the log((RNo - a1 x)/a0)
                Positives <- which(DataXY[LowX]$y.corr/A0 > 0)
                if (length(Positives) > 0) {
                    data.table::set(DataXY, i = LowX[Positives], j = "Init.Coeffs", value = log(DataXY[LowX][Positives]$y.corr/A0))
                    T.min   <- min(DataXY[LowX][Positives]$Out.Temperature_int)
                    init.name.Temperature <- name.Temperature
                    name.Temperature      <- "Normalised.Temperature"
                    data.table::set(DataXY, j = name.Temperature, value = DataXY$Out.Temperature_int - T.min)
                    log.min <- coef(quantreg::rq(as.formula(paste0("Init.Coeffs ~ " , name.Temperature)), data = DataXY[LowX][Positives], tau = c(0.5)))["(Intercept)"]
                    name.Log <- "Normalised.Log"
                    data.table::set(DataXY, j = name.Log, value = DataXY$Init.Coeffs - log.min)
                    Formula <- as.formula(paste0(name.Log," ~ " , name.Temperature, " - 1"))
                    Model.0 <- quantreg::rq(Formula, data = DataXY[LowX][Positives], tau = c(0.5))
                    plot(DataXY[LowX][Positives, .SD, .SDcols = c(name.Temperature, name.Log)], xlab = paste0(name.Temperature, ", T - T.min"),ylab = "log ((y - a1 x)/a0) - log.min", 
                         main = "Temperature effect at low x, Initial k value for log of sensor values")
                    mtext(side=3, line=0, adj=1, cex=1, paste0("T.min = ", round(T.min, digits = 1), ", log.min = ", round(log.min, digits = 3), ", k = ",round (coef(Model.0), digits = 5),"; ",format(min(DataXY$date), format = "%Y%m%d"), " - ", format(max(DataXY$date), format = "%Y%m%d")))
                    grid()
                    abline(a = 0, b = coef(Model.0)[1], col=2, lwd=2)
                    coeff <- round(Model.0$coefficients , 4)
                    text(x = range(DataXY[LowX][Positives,..name.Temperature], na.rm = T)[1], y = range(DataXY[LowX][Positives,..name.Log], na.rm = T)[2], pos = 4,
                         paste("Model : y = ",coeff[1] , "*x" ))
                    # plotting y - a1 x = c exp(k(T-Tmin) + log.min)
                    plot(DataXY[LowX][Positives, .SD, .SDcols = c(init.name.Temperature, "y.corr")], xlab = name.Temperature,ylab = "y - a1 x",
                         main = paste0("Temperature at low x, k value ", format(min(DataXY$date), format = "%Y%m%d"), " - ", format(max(DataXY$date), format = "%Y%m%d")))
                    grid()
                    data.table::set(DataXY, j = "Cexp_kT", value = A0 * exp(unname(coef(Model.0)[1]) * (DataXY$Out.Temperature_int - T.min)))
                    data.table::set(DataXY, j = "Cexp_kT_log.min", value = A0 * exp(coef(Model.0)[1] * (DataXY$Out.Temperature_int - T.min) + log.min))
                    lines(setorderv(DataXY[LowX][Positives, .SD, .SDcols = c(init.name.Temperature, "Cexp_kT_log.min")], cols = init.name.Temperature), type = "l", col = "red")
                    mtext(side=3, line=0, adj=1, cex=1, paste0("y - ", round(A1, digits = 3), " x = ", round(A0, digits = 3), 
                                                               " exp(", round (coef(Model.0), digits = 5), " (T - ", round(T.min, digits = 1),") + ", round(log.min, digits = 3), ")"))
                    # initial values see https://stats.stackexchange.com/questions/160552/why-is-nls-giving-me-singular-gradient-matrix-at-initial-parameter-estimates
                    Starting.values <- list(a1 = A1, C = A0 * exp(unname(log.min)), k = unname(coef(Model.0)[1]))
                } else {
                    # checking for positives values for the log(RNo - a1 x)
                    Positives <- which(DataXY[LowX]$y.corr > 0)
                    if (length(Positives) > 0) {
                        data.table::set(DataXY, i = LowX[Positives], j = "Init.Coeffs", value = log(DataXY[LowX][Positives]$y.corr))
                        Formula <- as.formula(paste0("Init.Coeffs ~ ",name.Temperature))
                        Model.0 <- quantreg::rq(Formula, data = DataXY[LowX][Positives], tau = c(0.5))
                        plot(DataXY[LowX][Positives, .SD, .SDcols = c(name.Temperature, "Init.Coeffs")], 
                             xlab = name.Temperature,ylab = "log(y - a1 x)", main = "Temperature at low x, Initial k value")
                        grid()
                        abline(a = coef(Model.0)[1], b = coef(Model.0)[2], col=2, lwd=2)
                        coeff <- round(Model.0$coefficients , 4)
                        text(x = range(DataXY[LowX][Positives,..name.Temperature], na.rm = T)[1], y = range(DataXY[LowX][Positives,Init.Coeffs], na.rm = T)[2], pos = 4,
                             paste("Model : y = ", coef(Model.0)[1], " + ",coeff[2] , "*x" ))
                        # initial values see https://stats.stackexchange.com/questions/160552/why-is-nls-giving-me-singular-gradient-matrix-at-initial-parameter-estimates
                        Starting.values <- list(a1 = A1, C = exp(unname(coef(Model.0)[1])), k = unname(coef(Model.0)[2]))
                    } else stop(futile.logger::flog.error("[Cal_Line] (y - a1 x)/a0 and y - a1 x are always negative at high ",name.Temperature,". Cannot compute logarithmic."))}
            } else stop(futile.logger::flog.error("[Cal_Line] no high temperature sensor data with low x values."))
            # Plotting 
            
            # Model.0 <- quantreg::rq(Init.Coeffs ~ get(name.Temperature) - 1, data = DataXY[LowX[Positives]], tau = c(0.5))
            # Fitting model
            Formula <- as.formula(paste0("y ~ f_exp_kT_NoC(x, a1, C, k, ",name.Temperature,")"))
            #Formula <- as.formula(paste0("y ~ f_exp_kT_NoC_log.min(x, a1, C, k, " , name.Temperature, ", log.min)"))
            if (Verbose) print(Starting.values, quote = F)
            if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
                Model <- minpack.lm::nlsLM(Formula, data = DataXY, start = Starting.values, model = TRUE, control = nls.lm.control(maxiter = 1024, maxfev = 10000))
            } else {
                Model <- minpack.lm::nlsLM(Formula, data = DataXY, start = Starting.values, model = TRUE, control = nls.lm.control(maxiter = 1024, maxfev = 10000), weights = wi) # , lower = c(0, 0.0000000001, 0, 0.00000000001))
            }
            car::scatter3d(y ~ x + Out.Temperature_int, data = DataXY[LowX], point.col = "blue", surface= F)
            Sensor        <- DataXY[["y"]]
            Fitted.Sensor <- coef(Model)["a1"] * DataXY$x + coef(Model)["C"] * exp(coef(Model)["k"] * (DataXY$Out.Temperature_int - T.min))
            plot(Sensor, Fitted.Sensor, 
                 main = paste0("Sensor calibrated ", format(min(DataXY$date), format = "%Y%m%d"), " - ", format(max(DataXY$date), format = "%Y%m%d")))
            mtext(side=3, line=0, adj=1, cex=1, paste0("y = ", round(coef(Model)["a1"], digits = 3), " x + ", round(coef(Model)["C"], digits = 3), 
                                                       " exp(", round (coef(Model)["k"], digits = 3), " (T - ", round(T.min, digits = 1),") + ", round(log.min, digits = 3), ")"))
            grid();abline(a = 0, b = 1, col = "red")
            text(x = range(Sensor, na.rm = T)[2], y = range(Fitted.Sensor, na.rm = T)[1], pos = 2, "Identity line", col = "red")
            Linear.Fitted <- lm(Fitted.Sensor ~ Sensor)
            Coeff <- round(Linear.Fitted$coefficients , 2)
            text(x = range(Sensor, na.rm = T)[1], y = range(Fitted.Sensor, na.rm = T)[2], pos = 4, paste("y = ", Coeff[1], " + ",Coeff[2] , "*x" ))
            text(x = range(Sensor, na.rm = T)[1], y = range(Fitted.Sensor, na.rm = T)[2] - 0.05 * diff(range(Fitted.Sensor, na.rm = T)) , 
                 pos = 4, paste0("R2 = ", round(summary(Linear.Fitted)$r.squared, digits = 2)))
            # display equations and R^2
            Equation <- sprintf(paste0(Sensor_name, "exp_kT_NoC: y = ",f_coef2," x + ",f_coef1," exp(",f_coef2," (T_Celsius - ", round(T.min, digits = 2),"), RMSE=",f_coef1,", AIC= %.1f"),
                                coef(Model)[1],coef(Model)[2],coef(Model)[3],
                                sqrt(sum(resid(Model)^2) / df.residual(Model)),
                                AIC(Model))
        } else  if (Mod_type == 'exp_kT') {
            
            # Model in Celsius degrees RNO = a0 + a1 NO + C exp(kT)
            # determining initial C and k values at high temperature
            if (length(LowX) > 0) {
                # only for positive logarithmic
                Positives <- which(DataXY[LowX]$y.corr > 0)
                if (length(Positives) > 0) {
                    data.table::set(DataXY, i = LowX[Positives], j = "Init.Coeffs", value = log(DataXY[LowX][Positives]$y.corr))
                } else stop(futile.logger::flog.error(paste0("[Cal_Line] y - (a0 + a1 x) is always negative at high ",name.Temperature,". Error on logarithmic computation. use Exp_kT_NoC model")))
                Formula <- as.formula(paste0("Init.Coeffs ~ ",name.Temperature))
                Model.0 <- quantreg::rq(Formula, data = DataXY[LowX][Positives], tau = c(0.5))
                plot(DataXY[LowX][Positives, .SD, .SDcols = c(name.Temperature, "Init.Coeffs")], xlab = name.Temperature,ylab = "log (y - (a1 x + a0))", 
                     main = "Temperature effect at low x, Initial k value")
                grid()
                abline(a = coef(Model.0)[1], b = coef(Model.0)[2], col=2, lwd=2)
                coeff <- round(Model.0$coefficients , 4)
                text(x = range(DataXY[LowX][Positives,..name.Temperature], na.rm = T)[1], y = range(DataXY[LowX][Positives,Init.Coeffs], na.rm = T)[2], pos = 4,
                     paste("Model : y = ",coeff[1] , "+", coeff[2],"*x" ))
                # initial values see https://stats.stackexchange.com/questions/160552/why-is-nls-giving-me-singular-gradient-matrix-at-initial-parameter-estimates
                Starting.values <- list(a0 = A0, a1 = A1, C = exp(unname(coef(Model.0)[1])), k = unname(coef(Model.0)[2]))
            } else {
                
                stop(futile.logger::flog.error("[Cal_Line] no high temperature sensor data with low x values."))
                #Starting.values <- list(C = 1, k = 0.13, Y0 = unname(A0))
                Formula <- as.formula(paste0("(y - (A0 + A1 * x)) ~ f_ExpGrowth(",name.Temperature,", C, k)"))
                Starting.values <- list(C = 1, k = 0.13)
                Model.0 <- minpack.lm::nlsLM(Formula, data = DataXY[LowX], start = Starting.values, model = TRUE,
                                             control = nls.lm.control(maxiter = 1024, maxfev = 10000))
                plot(DataXY[LowX][[name.Temperature]], DataXY[LowX,y], main = "Initial k0 value")
                plot(DataXY[LowX][Positives, .SD, .SDcols = c(name.Temperature, "Init.Coeffs")], ylab = "log (y - (a0 + a1 x))", main = "Temperature effect, Initial C and k value")
                grid()
                DataXY[LowX, Predict := A0 + A1 * x + predict(Model.0, DataXY[LowX][[name.Temperature]])]
                lines(DataXY[LowX, .SD, .SDcols = c(name.Temperature, "Predict")][order(DataXY[LowX][[name.Temperature]])], col = "red")
                abline(a = coef(Model.0)[1], b = coef(Model.0)[2], col=2, lwd=2)
                coeff <- round(Model.0$coefficients , 2)
                text(x = range(DataXY[LowX][Positives,..name.Temperature], na.rm = T)[1], y = range(DataXY[LowX][Positives,Init.Coeffs], na.rm = T)[2], pos = 4,
                     paste("Model : y = ",coeff[1] , " + " , coeff[2] , "*x" ))
                
                C0 <- exp(coef(Model.0)[1])
                k0 <- coef(Model.0)[2]
                #A0 <- coef(Model.0)[3]
                Starting.values <- list(a0 = unname(A0), a1 = unname(A1), C = unname(C0), k = unname(k0))} 
            # Fitting model
            Formula <- as.formula(paste0("y ~ f_exp_kT(x, a0, a1, C, k, ",name.Temperature,")"))
            if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
                Model <- minpack.lm::nlsLM(Formula, data = DataXY, start = Starting.values, model = TRUE, control = nls.lm.control(maxiter = 1024, maxfev = 10000),
                                           lower = c(-Inf,-Inf,-Inf,-Inf), upper = c(+Inf,+Inf, +Inf,+Inf))
            } else Model <- minpack.lm::nlsLM(Formula, data = DataXY, start = Starting.values, model = TRUE, weights = wi, control = nls.lm.control(maxiter = 1024, maxfev = 10000),
                                              lower = c(-Inf,-Inf,-Inf,-Inf), upper = c(+Inf,+Inf, +Inf,+Inf))
            plot(broom.mixed::augment(Model)[,c("y", ".fitted")], xlab = "sensor", ylab = "fitted model") 
            # display equations and R^2
            Equation <- sprintf(paste0(Sensor_name, "exp_kT: y = ",f_coef1,"+ ",f_coef2," x + ", f_coef2," exp(",f_coef2," T_Celsius), RMSE=", f_coef1,",AIC= %.1f"),
                                coef(Model)[1],coef(Model)[2],coef(Model)[3],coef(Model)[4],
                                sqrt(sum(resid(Model)^2) / df.residual(Model)),
                                AIC(Model))
        } else  if (Mod_type == 'exp_kTn') {
            # Model in Celsius degrees RNO = a0 + a1 NO + C log(T^n)
            # determining initial C and k values at high temperature
            if (length(LowX) > 0) {
                # only for positive logarithmic
                Positives <- which(DataXY[LowX]$y.corr > 0)
                if (length(Positives) > 0) {
                    data.table::set(DataXY, i = LowX[Positives], j = "Init.Coeffs", value = log(DataXY[LowX][Positives]$y.corr))
                } else stop(futile.logger::flog.error(paste0("[Cal_Line] y - (a0 + a1 x) is always negative at high ",name.Temperature,". Error on logarithmic computation. use Exp_kT_NoC model")))
                Formula <- as.formula(paste0("Init.Coeffs ~ ",name.Temperature))
                Model.0 <- quantreg::rq(Formula, data = DataXY[LowX][Positives], tau = c(0.5))
                plot(DataXY[LowX][Positives, .SD, .SDcols = c(name.Temperature, "Init.Coeffs")], xlab = name.Temperature,ylab = "log (y - (a1 x + a0))", 
                     main = "Temperature effect at low x, Initial k value")
                grid()
                abline(a = coef(Model.0)[1], b = coef(Model.0)[2], col=2, lwd=2)
                coeff <- round(Model.0$coefficients , 4)
                text(x = range(DataXY[LowX][Positives,..name.Temperature], na.rm = T)[1], y = range(DataXY[LowX][Positives,Init.Coeffs], na.rm = T)[2], pos = 4,
                     paste("Model : y = ",coeff[1] , "+", coeff[2],"*x" ))
                # initial values see https://stats.stackexchange.com/questions/160552/why-is-nls-giving-me-singular-gradient-matrix-at-initial-parameter-estimates
                Starting.values <- list(a0 = A0, a1 = A1, C = exp(unname(coef(Model.0)[1])), n = 1)
            } else {
                
                stop(futile.logger::flog.error("[Cal_Line] no high temperature sensor data with low x values."))
                #Starting.values <- list(C = 1, k = 0.13, Y0 = unname(A0))
                Formula <- as.formula(paste0("(y - (A0 + A1 * x)) ~ f_ExpGrowth(",name.Temperature,", C, k)"))
                Starting.values <- list(C = 1, k = 0.13)
                Model.0 <- minpack.lm::nlsLM(Formula, data = DataXY[LowX], start = Starting.values, model = TRUE,
                                             control = nls.lm.control(maxiter = 1024, maxfev = 10000))
                plot(DataXY[LowX][[name.Temperature]], DataXY[LowX,y], main = "Initial k0 value")
                plot(DataXY[LowX][Positives, .SD, .SDcols = c(name.Temperature, "Init.Coeffs")], ylab = "log (y - (a0 + a1 x))", main = "Temperature effect, Initial C and k value")
                grid()
                DataXY[LowX, Predict := A0 + A1 * x + predict(Model.0, DataXY[LowX][[name.Temperature]])]
                lines(DataXY[LowX, .SD, .SDcols = c(name.Temperature, "Predict")][order(DataXY[LowX][[name.Temperature]])], col = "red")
                abline(a = coef(Model.0)[1], b = coef(Model.0)[2], col=2, lwd=2)
                coeff <- round(Model.0$coefficients , 2)
                text(x = range(DataXY[LowX][Positives,..name.Temperature], na.rm = T)[1], y = range(DataXY[LowX][Positives,Init.Coeffs], na.rm = T)[2], pos = 4,
                     paste("Model : y = ",coeff[1] , " + " , coeff[2] , "*x" ))
                
                C0 <- exp(coef(Model.0)[1])
                k0 <- coef(Model.0)[2]
                #A0 <- coef(Model.0)[3]
                Starting.values <- list(a0 = unname(A0), a1 = unname(A1), C = unname(C0), n = 1)} 
            # Fitting model
            Formula <- as.formula(paste0("y ~ f_exp_kTn(x, a0, a1, C,",name.Temperature,", n)"))
            # Lower = c(0, 0.0000000001, 0, 0.00000000001)
            if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
                Model <- minpack.lm::nlsLM(Formula, data = DataXY, start = Starting.values, model = TRUE,
                                           control = nls.lm.control(maxiter = 1024, maxfev = 10000),
                                           lower = c(-Inf,0,-Inf, 1), upper = c(+Inf, +Inf, +Inf, 5))
            } else Model <- minpack.lm::nlsLM(Formula, data = DataXY, start = Starting.values, model = TRUE, weights = wi,
                                              control = nls.lm.control(maxiter = 1024, maxfev = 10000),
                                              lower = c(-Inf,0,-Inf, 1), upper = c(+Inf, +Inf, +Inf, 5))
            plot(broom.mixed::augment(Model)[,c("y", ".fitted")], xlab = "sensor", ylab = "fitted model") 
            # display equations and R^2
            Equation <- sprintf(paste0(Sensor_name, "exp_kTn: y = ",f_coef1,"+ ",f_coef2," x + ", f_coef2," exp(T_Celsius^",f_coef2,"), RMSE=", f_coef1,",AIC= %.1f"),
                                coef(Model)[1],coef(Model)[2],coef(Model)[3],coef(Model)[4],
                                sqrt(sum(resid(Model)^2) / df.residual(Model)),
                                AIC(Model))
        } else  if (Mod_type == 'exp_kK') {
            
            # temperature in Kelvin
            data.table::set(DataXY, j = name.Temperature, value = 273.15 + DataXY[[name.Temperature]])
            
            # Model RNO = a0 + a1 NO + C exp(kK)
            # Values for which log(DataXY$y - (A0 + A1 * DataXY$x)) can be calculated
            Positives <- which(DataXY[LowX]$y.corr > 0)
            
            if (length(Positives) > 0) {
                data.table::set(DataXY, i = LowX[Positives], j = "Init.Coeffs", value = log(DataXY[LowX][Positives]$y.corr))
            } else stop(futile.logger::flog.error(paste0("[Cal_Line] y - (a0 + a1 x) is always negative at high ",name.Temperature,". Error on logarithmic computation. use Exp_kT_NoC model")))
            Formula <- as.formula(paste0("Init.Coeffs ~ ",name.Temperature))
            Model.0 <- quantreg::rq(Formula, data = DataXY[LowX][Positives], tau = c(0.5))
            plot(DataXY[LowX][Positives, .SD, .SDcols = c(name.Temperature, "Init.Coeffs")], xlab = name.Temperature,ylab = "log (y - (a1 x + a0))", 
                 main = "Temperature effect at low x, Initial k value")
            grid()
            abline(a = coef(Model.0)[1], b = coef(Model.0)[2], col=2, lwd=2)
            coeff <- round(Model.0$coefficients , 4)
            text(x = range(DataXY[LowX][Positives,..name.Temperature], na.rm = T)[1], y = range(DataXY[LowX][Positives,Init.Coeffs], na.rm = T)[2], pos = 4,
                 paste("Model : y = ",coeff[1] , "+", coeff[2],"*x" ))
            # initial values see https://stats.stackexchange.com/questions/160552/why-is-nls-giving-me-singular-gradient-matrix-at-initial-parameter-estimates
            Starting.values <- list(a0 = A0, a1 = A1, C = exp(unname(coef(Model.0)[1])), k = unname(coef(Model.0)[2]))
            Formula <- as.formula(paste0("y ~ f_exp_kT(x, a0, a1, C, k, ",name.Temperature,")"))
            if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
                Model <- minpack.lm::nlsLM(Formula, data = DataXY, start = Starting.values, model = TRUE, control = nls.lm.control(maxiter = 1024, maxfev = 10000),
                                           lower = c(-Inf,-Inf,-Inf,-Inf), upper = c(+Inf,+Inf, +Inf,+Inf))
            } else Model <- minpack.lm::nlsLM(Formula, data = DataXY, start = Starting.values, model = TRUE, weights = wi, control = nls.lm.control(maxiter = 1024, maxfev = 10000),
                                              lower = c(-Inf,-Inf,-Inf,-Inf), upper = c(+Inf,+Inf, +Inf,+Inf))
            plot(broom.mixed::augment(Model)[,c("y", ".fitted")], xlab = "sensor", ylab = "fitted model") 
            # display equations and R^2
            # Equation <- sprintf(paste0(Sensor_name, "Power: y = ",f_coef1,"+ ",f_coef2," x + exp(",f_coef2," T_Kelvin + ", f_coef2,"), RMSE=",f_coef1,",AIC= %.1f"),
            #                     coef(Model)[1],coef(Model)[2],coef(Model)[3],coef(Model)[4],
            #                     sqrt(sum(resid(Model)^2) / df.residual(Model)),
            #                     AIC(Model))
            Equation <- sprintf(paste0(Sensor_name, "exp_kK: y = ",f_coef1,"+ ",f_coef2," x + exp(",f_coef2," T_Kelvin + ", f_coef2,"), RMSE=",f_coef1,",AIC= %.1f"),
                                coef(Model)[1],coef(Model)[2],coef(Model)[3],coef(Model)[4],
                                sqrt(sum(resid(Model)^2) / df.residual(Model)),
                                AIC(Model))
        } else if (any(Mod_type %in% c('T_power', 'K_power'))) {
            browser()
            # Model in Celsius degrees RNO = a0 + a1 NO + C T^n or C K^n
            
            # Transforming Celsius in Kelvin if needed
            if (Mod_type == 'K_power') data.table::set(DataXY, j = name.Temperature, value = 273.15 + DataXY[[name.Temperature]])
            
            # # index of row with max temperature
            # index.mdT <- which(DataXY[[name.Temperature]] == max(DataXY[[name.Temperature]], na.rm = TRUE))[1]
            # A2 <- (DataXY[index.mdT, "y"] - (A0 + A1 * DataXY[index.mdT, "x"])) / (DataXY[index.mdT, name.Temperature])^1.75
            
            # Setting Initial values of A2 and n at high temperature and low x
            LowX <- which(DataXY[,x] <= quantile(DataXY[["x"]], probs = c(0.25)) & DataXY[[name.Temperature]] > quantile(DataXY[[name.Temperature]], probs = c(0.66)))
            if (length(LowX) > 0) {
                # only for positive logarithmic
                Positives <- which(DataXY[LowX]$y.corr > 0 & DataXY[LowX][[name.Temperature]] > 0)
                # https://www.statology.org/power-regression-in-r/ or https://www.r-bloggers.com/2022/02/how-to-calculate-power-regression-in-r-step-by-step-guide/
                if (length(Positives) > 0) {
                    data.table::set(DataXY, i = LowX[Positives], j = "Log.y", value = log(DataXY[LowX][Positives]$y.corr))
                    data.table::set(DataXY, i = LowX[Positives], j = "Log.x", value = log(DataXY[LowX][Positives][[name.Temperature]]))
                } else stop(futile.logger::flog.error(paste0("[Cal_Line] y - (a0 + a1 x) is always negative at high ",name.Temperature,". Error on logarithmic computation. use Exp_kT_NoC model")))
                
                Model.0 <- quantreg::rq(as.formula(paste0("Log.y ~ Log.x")), data = DataXY[LowX][Positives], tau = c(0.5))
                plot(DataXY[LowX][Positives, .SD, .SDcols = c("Log.x", "Log.y")], ylab = "log (y - (a0 + a1 x))", xlab = paste0("log(",name.Temperature,")"),
                     main = "Temperature effect at low x, Initial a2 and n values")
                grid()
                coeff <- round(Model.0$coefficients , 4)
                abline(a = coeff[1], b = coeff[2], col=2, lwd=2)
                text(x = range(DataXY[LowX][Positives,Log.x], na.rm = T)[1], y = range(DataXY[LowX][Positives,Log.y], na.rm = T)[2], pos = 4,
                     paste("Model : y = ",coeff[1] , "+", coeff[2],"*x" ))
                # initial values see https://stats.stackexchange.com/questions/160552/why-is-nls-giving-me-singular-gradient-matrix-at-initial-parameter-estimates
                Starting.values <- list(a0 = unname(A0), a1 = unname(A1), a2 = unname(exp(coef(Model.0)[1])), n = unname(coef(Model.0)[2]))
            } else stop(futile.logger::flog.error("[Cal_Line] no high temperature sensor data with low x values.")) 
            
            # Fitting model
            Formula <- as.formula(paste0("y ~ f_T_power(x, a0, a1, a2, n,",name.Temperature,")"))
            if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
                Model <- minpack.lm::nlsLM(Formula, data = DataXY, start = Starting.values, model = TRUE, 
                                           control = nls.lm.control(maxiter = 1024, maxfev = 10000),
                                           lower = c(A0,A1,-Inf, 1), upper = c(A0, A1, +Inf, +Inf))
            } else Model <- minpack.lm::nlsLM(Formula, data = DataXY, start = Starting.values, model = TRUE, weights = wi,
                                              control = nls.lm.control(maxiter = 1024, maxfev = 10000),
                                              lower = c(A0,A1,-Inf, 1), upper = c(A0, A1, +Inf, +Inf))
            # display equations and R^2
            if (Mod_type == 'T_power') {
                Equation <- paste0(Sensor_name, "Power: y = ", f_coef1," + ",f_coef2," x + ",f_coef2," T_Celsius^", f_coef2, ", RMSE=",f_coef1,", AIC= %.1f")
            } else if (Mod_type == 'K_power') Equation <- paste0(Sensor_name, "Power: y = ", f_coef1," + ",f_coef2," x + ",f_coef2," T_Kelvins^", f_coef2, ", RMSE=",f_coef1,", AIC= %.1f")
            Equation <- sprintf(Equation,
                                coef(Model)[1],coef(Model)[2],coef(Model)[3],coef(Model)[4],
                                sqrt(sum(resid(Model)^2) / df.residual(Model)), AIC(Model))
        }
    } else if (Mod_type %in% c("NO2_Lab", "NO2_Lab_decay_inc")) {
        
        # Setting Initial values of the linear relationship between sensor and reference using the smallest temperatures: T< Qantile(0.33) and T < 20 degrress
        if (Sensor_name == "NO_B4_P1, ") {
            # For NO
            LowT <- which(DataXY[[name.Temperature]] < quantile(DataXY[[name.Temperature]], probs = c(0.33)) & DataXY[[name.Temperature]] < 20)
            if (!length(LowT) > 0) LowT <- which(DataXY[[name.Temperature]] < quantile(DataXY[[name.Temperature]], probs = c(0.33)))
        } else if (Sensor_name == "NO2_B43F_P1, ") {
            # For NO2, possibly for others
            LowT <-  DataXY[DataXY[[name.Temperature]] < quantile(DataXY[[name.Temperature]], probs = c(0.25)), which = T]}
        
        Formula <- as.formula(paste0("scale(y) ~ scale(x) + scale(", name.Humidity, ")"))
        Formula <- as.formula(paste0("y ~ x + ", name.Humidity))
        Linear.Model <- quantreg::rq(Formula, data = DataXY[LowT], tau = c(0.5))
        Linear.Model <- lm(Formula, data = DataXY[LowT])
        coeff <- round(Linear.Model$coefficients , 2)
        A0    <- unname(coef(Linear.Model)[1])
        A1    <- unname(coef(Linear.Model)[2])
        A2    <- unname(coef(Linear.Model)[3])
        # Plotting initial values A0 and A1 for high x values/low name.Temperature
        plot(DataXY[LowT]$y, predict(Linear.Model), 
             main = paste0("Fit a0, a1 and a2 at low temp, ", format(min(DataXY$date), format = "%Y%m%d"), " - ", format(max(DataXY$date), format = "%Y%m%d")))
        grid(); abline(lm("y ~ x", data = data.table(x = DataXY[LowT]$y, y = predict(Linear.Model))), col = "red")
        text(x = range(DataXY[LowT,y], na.rm = T)[1], y = range(predict(Linear.Model), na.rm = T)[2], pos = 4, 
             paste("rq: y = ",coeff[1] , " + " , coeff[2] , "*x", " + " , coeff[3] , "* RH"))
        text(x = range(DataXY[LowT,y], na.rm = T)[2], y = range(predict(Linear.Model), na.rm = T)[1], pos = 2, 
             paste("R2 = ",round(summary(Linear.Model)$r.squared, digits = 2)))
        
        # determining initial k values at high temperature when x is low
        if (Weighted || Sensor_name == "NO2_B43F_P1, ") {
            LowX <- which(DataXY[["x"]] <= quantile(DataXY[["x"]], probs = c(0.25)))
            LowX.HighRH <- DataXY[x <= quantile(DataXY[["x"]], probs = c(0.10))  & 
                                      DataXY[[name.Humidity]] > quantile(DataXY[[name.Humidity]], probs = c(0.75)), which = T]
        } else if (Sensor_name == "NO_B4_P1, ") {
            #LowX <- which(DataXY[,x] <= quantile(DataXY[["x"]], probs = c(0.25)) & DataXY[[name.Temperature]] > quantile(DataXY[[name.Temperature]], probs = c(0.66)))
            LowX <- DataXY[x <= quantile(DataXY[["x"]], probs = c(0.10)), which = T]}
        
        
        # Fitting model
        if (Mod_type == "NO2_Lab"){
            if (length(LowX) > 0) {
                # Preparing for log(R - (a0 + a1 x + a2 RH)])
                DataXY[, y.corr := y - (A0 + A1 * x + A2 * DataXY[[name.Humidity]])]
                browser()
                # only for positive logarithmic
                Negatives <- which(DataXY[LowX]$y.corr < 0)
                if (length(Negatives) > 0) {
                    data.table::set(DataXY, i = LowX[Negatives], j = "Init.Coeffs", value = log(-DataXY[LowX][Negatives]$y.corr))
                } else stop(futile.logger::flog.error(paste0("[Cal_Line] y - (a0 + a1 x +a2 RH) is always positives at high ",name.Temperature,". Error on logarithmic computation. use Exp_kT_NoC model")))
                Formula <- as.formula(paste0("Init.Coeffs ~ ",name.Temperature))
                Model.0 <- quantreg::rq(Formula, data = DataXY[LowX][Negatives], tau = c(0.5))
                Model.0 <- lm(Formula, data = DataXY[LowX][Negatives])
                plot(DataXY[LowX][Negatives][[name.Temperature]], DataXY[LowX][Negatives]$Init.Coeffs, xlab = name.Temperature,ylab = "log -(y - (a2 RH + a1 x + a0))", 
                     main = "Temperature effect at low x, Initial k value")
                grid()
                abline(a = coef(Model.0)[1], b = coef(Model.0)[2], col=2, lwd=2)
                coeff <- round(Model.0$coefficients , 4)
                text(x = range(DataXY[LowX][Negatives,..name.Temperature], na.rm = T)[1], y = range(DataXY[LowX][Negatives,Init.Coeffs], na.rm = T)[2], pos = 4,
                     paste("Model : y = ",coeff[1] , "+", coeff[2],"*x" ))
            } else stop(futile.logger::flog.error("[Cal_Line] no high temperature sensor data with low x values."))
            
            Formula <- as.formula(paste0("y ~ f_NO2_Lab(x, a0, a1, a2, ", name.Humidity, ", a3, k, ",name.Temperature,")"))
            # initial values see https://stats.stackexchange.com/questions/160552/why-is-nls-giving-me-singular-gradient-matrix-at-initial-parameter-estimates
            Starting.values <- list(a0 = A0, a1 = A1, a2 = A2, a3 = exp(unname(coef(Model.0)[1])), k = unname(coef(Model.0)[2]))
            #Starting.values[["k"]] <- 0.11
            #Starting.values <- list(a0 = A0, a1 = A1, a2 = A2, a3 = 1.3, k = 0.11)
            if (Verbose) print(Starting.values, quote = F)
            Lower.values  <- c(-Inf, A1, -Inf,    0, unname(coef(Model.0)[2]))
            Upper.values  <- c(+Inf, A1,    0,  Inf, unname(coef(Model.0)[2]))
            if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
                Model <- minpack.lm::nlsLM(Formula, data = DataXY, start = Starting.values, model = TRUE, 
                                           control = nls.lm.control(maxiter = 1024, maxfev = 10000), 
                                           lower = Lower.values, upper = Upper.values)
            } else {
                Model <- minpack.lm::nlsLM(Formula, data = DataXY, start = Starting.values, model = TRUE, control = nls.lm.control(maxiter = 1024, maxfev = 10000),
                                           lower = Lower.values, upper = Upper.values, weights = wi) 
            }
            # display equations and R^2
            Equation <- sprintf(paste0(Sensor_name, "NO2_Lab: y = ",f_coef1," + ",f_coef2, "x +",f_coef2, " RH +",f_coef2, " exp(",f_coef2, " T), RMSE=",f_coef1,",AIC= %.1f"),
                                coef(Model)[1],coef(Model)[2],coef(Model)[3],coef(Model)[4],coef(Model)[5],
                                sqrt(sum(resid(Model)^2) / df.residual(Model)),
                                AIC(Model))
        } else if (Mod_type == "NO2_Lab_decay_inc") {
            if (length(LowX) > 0) {
                # Preparing for log(R - (a0 + a1 x + a2 RH)])
                DataXY[, y.corr := y - (A0 + A1 * x + A2 * DataXY[[name.Humidity]])]
                # only for positive logarithmic
                Negatives <- which(DataXY[LowX]$y.corr < 0)
                if (length(Negatives) > 0) {
                    data.table::set(DataXY, i = LowX[Negatives], j = "Init.Coeffs", value = log(-DataXY[LowX][Negatives]$y.corr))
                } else stop(futile.logger::flog.error(paste0("[Cal_Line] y - (a0 + a1 x +a2 RH) is always positives at high ",name.Temperature,". Error on logarithmic computation. use Exp_kT_NoC model")))
                A3 <- quantile(DataXY[LowX][Negatives][["Init.Coeffs"]], probs = 0.95)
                T0 <- 15 # temperature when init.coeff = 0
                Formula <- as.formula(paste0("Init.Coeffs ~ a3 * (1 - exp(- k * (",name.Temperature," - t0)))"))
                Model.0 <- minpack.lm::nlsLM(Formula, data = DataXY, start = list(a3 = A3, t0 = T0, k = 0.11), model = TRUE, control = nls.lm.control(maxiter = 1024, maxfev = 10000))
                plot(DataXY[LowX][Negatives][[name.Temperature]], DataXY[LowX][Negatives]$Init.Coeffs, xlab = name.Temperature,ylab = "log -(y - (a2 RH + a1 x + a0))", 
                     main = "Temperature effect at low x, Initial a3, T0 and k value")
                grid()
                lines(DataXY[LowX][Negatives][[name.Temperature]], predict(Model.0), col = "red")
                coeff <- round(coef(Model.0), 3)
                text(x = range(DataXY[LowX][Negatives,..name.Temperature], na.rm = T)[1], y = range(DataXY[LowX][Negatives,Init.Coeffs], na.rm = T)[1], pos = 4,
                     paste("Model : y = ",coeff[1] , " (1 - exp(", coeff[3]," T - ",coeff[2], ")))" ))
            } else stop(futile.logger::flog.error("[Cal_Line] no high temperature sensor data with low x values."))
            
            Formula <- as.formula(paste0("y ~ f_NO2_Lab_decay_inc(x, a0, a1, a2, ", name.Humidity, ", a3, k, ",name.Temperature,", T0)"))
            Starting.values <- list(a0 = A0, a1 = A1, a2 = A2, a3 = unname(coef(Model.0)["a3.95%"]), k = unname(coef(Model.0)["k"]), T0 = unname(coef(Model.0)["t0"]))
            if (Verbose) print(Starting.values, quote = F)
            Lower.values  <- c(-Inf, A1, -Inf,coef(Model.0)["a3.95%"], coef(Model.0)["k"], coef(Model.0)["t0"])
            Upper.values  <- c(+Inf, A1,    0,coef(Model.0)["a3.95%"], coef(Model.0)["k"], coef(Model.0)["t0"])
            if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
                Model <- minpack.lm::nlsLM(Formula, data = DataXY, start = Starting.values, model = TRUE, control = nls.lm.control(maxiter = 1024, maxfev = 10000), 
                                           lower = Lower.values, upper = Upper.values)
            } else {
                Model <- minpack.lm::nlsLM(Formula, data = DataXY, start = Starting.values, model = TRUE, control = nls.lm.control(maxiter = 1024, maxfev = 10000),
                                           lower = Lower.values, upper = Upper.values, weights = wi) }
            
            # Plotting initial values A0 and A1 for high x values/low name.Temperature
            plot(DataXY$y, predict(Model), 
                 main = paste0("Final model for predicting y sensor, ", format(min(DataXY$date), format = "%Y%m%d"), " - ", format(max(DataXY$date), format = "%Y%m%d")))
            grid(); abline(lm("y ~ x", data = data.table(x = DataXY$y, y = predict(Model))), col = "red")
            text(x = range(DataXY[,y], na.rm = T)[1], y = range(predict(Model), na.rm = T)[1], pos = 4, 
                 paste("y = ",round(coef(Model)["a0"],1) , " + " , round(coef(Model)["a1"],3) , "* NO2", " + " , round(coef(Model)["a2"],3) , "* RH - exp(",
                       round(coef(Model)["a3"],3)," * (1 - exp(",round(coef(Model)["k"],3), " * (T - ",round(coef(Model)["T0"],1), "))))"))
            print(summary(Model))
            # display equations
            Equation <- sprintf(paste0(Sensor_name, "NO2_Lab_decay_inc: y = ",f_coef1," + ",f_coef2, "x +",f_coef2, " RH - exp(",f_coef1, " * (1 - exp(",f_coef2, " * (T - ",f_coef2, ")))), RMSE=",f_coef1,",AIC= %.1f"),
                                coef(Model)[1],coef(Model)[2],coef(Model)[3],coef(Model)[4],coef(Model)[5],coef(Model)[6],
                                sqrt(sum(resid(Model)^2) / df.residual(Model)),
                                AIC(Model))
        }
        
    } else if (any(Mod_type %in% c('BeerLambert'))) {
        # Transforming Celsius in Kelvin
        data.table::set(DataXY, j = name.temperature, value = 273.15 + DataXY[, name.temperature, with = F])
        # Setting Initial values
        #Linear.Model <- lm(y ~ x, data = DataXY)
        DataXY$X <- DataXY[["x"]] * DataXY[[name.Temperature]] / DataXY[["Out.Atmospheric_pressure"]]
        Linear.Model <- quantreg::rq(y ~ X, data = DataXY, tau = c(0.5))
        A0 <- coef(Linear.Model)[1]
        A1 <- coef(Linear.Model)[2]
        # Fitting model
        if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
            Model <- minpack.lm::nlsLM(y ~ f_BeerLambert2(x = x, a0 = a0, aT = aT, n = n, Temperature = Temperature, Atmospheric_pressure = Atmospheric_pressure), data = DataXY,
                                       start = list(a0 = A0, aT = mean(DataXY[[name.Temperature]], na.rm = T), n = 1),
                                       model = TRUE, control = nls.lm.control(maxiter = 1024, maxfev = 10000),
                                       lower = c(-Inf,-Inf,-Inf), upper = c(+Inf, +Inf, +Inf))
        } else Model <- minpack.lm::nlsLM(y ~ f_BeerLambert(x, a0, a1, a2, n, Temperature, Atmospheric_pressure), data = DataXY,
                                          start = list(a0 = A0, a1 = A1, a2 = 0, a3 = 0, n = 1),
                                          weights = wi, model = TRUE, control = nls.lm.control(maxiter = 1024, maxfev = 10000),
                                          lower = c(-Inf,-Inf,-Inf,-Inf,-Inf), upper = c(+Inf, +Inf, +Inf, +Inf, +Inf))
        # display equations and R^2
        Equation <- paste0(Sensor_name, "BeerLambert: y = ", f_coef1," + ",f_coef2," x Temperature^",f_coef1,"/pressure, RMSE=",f_coef1,", AIC= %.1f")
        Equation <- sprintf(Equation, coef(Model)[1],coef(Model)[2],coef(Model)[3], sqrt(sum(resid(Model)^2) / df.residual(Model)), AIC(Model))
    } else if (Mod_type == 'MultiLinear') {
        # Taking parameters from MultiFile of from passed variables
        if (!is.null(Multi.File)) {
            if (file.exists(Multi.File)) {
                # Flag
                is.MultiFile <- TRUE
                # read Multi.File
                Multi.File.df <-  read.table(file             = Multi.File,
                                             header           = TRUE,
                                             row.names        = NULL,
                                             comment.char     = "#"
                                             , stringsAsFactors = FALSE
                )
                # set Formula for all Covariates
                # Checking that some Covariates shall be fitted
                if (any(Multi.File.df$Enabled & !Multi.File.df$Forced)) {
                    # Covariates without Intercept, Enables and not forced
                    Cov.MinusInt <- Multi.File.df$Covariates[which(Multi.File.df$Enabled & !Multi.File.df$Forced)][Multi.File.df$Covariates != "Intercept"]
                    # their degree of polynomial
                    Degrees <-  Multi.File.df[Multi.File.df$Covariates %in% Cov.MinusInt, "degree"]
                    if (exists("Formula.Cov.MinusInt")) rm(Formula.Cov.MinusInt)
                    for (j in seq_along(Cov.MinusInt)) {
                        # Formaula of the covarariates depending if it is fitted with "ExpGrowth"
                        if (Degrees[j] == "ExpGrowth") {
                            Formula.Covar <- paste0("C.", Multi.File.df$Covariates[j] ," * exp(k.",Multi.File.df$Covariates[j] ," * ", Cov.MinusInt[j],")")
                        } else {
                            if (any("ExpGrowth" %in% Degrees)) {
                                Formula.Covar <- paste0("a.",Cov.MinusInt[j],seq(Degrees[j])," * I(",Cov.MinusInt[j],"^",seq(1:Degrees[j]),")",collapse = "+")
                            } else Formula.Covar <- paste0("I(",Cov.MinusInt[j],"^",seq(1:Degrees[j]),")",collapse = "+")
                        }
                        # adding the formuala for the covariates
                        if (exists("Formula.Cov.MinusInt")) Formula.Cov.MinusInt <- paste0(c(Formula.Cov.MinusInt, Formula.Covar), collapse = "+") else Formula.Cov.MinusInt <- Formula.Covar
                        # Deleting the formula of the current covariates
                        rm(Formula.Covar)
                    }
                    # final formula
                    if (any("ExpGrowth" %in% Degrees)) {
                        Formula.Covariates <- as.formula(paste("y ~ a0 + a1 * x ", Formula.Cov.MinusInt, sep = "+"))
                    } else Formula.Covariates <- as.formula(paste("y ~ x ", Formula.Cov.MinusInt, sep = "+"))
                    
                    # Checking if some Covariates are forced
                    if (any(Multi.File.df$Forced)) {
                        # Checking that the proper number of parameters is given according to the Model type of the variable
                        # set Formula
                        for (i in Multi.File.df[Multi.File.df$Forced,"Covariates"]) {
                            if ( Multi.File.df[Multi.File.df$Forced,"Covariates"] == "")
                                Multi.File.df[Multi.File.df$Covariates == i,"a0_an"]
                            Model.offset <- ""
                        }
                    } else if (Verbose) futile.logger::flog.info("[Cal_line] All covariates need fitting!")
                }
            } else {
                # Flag
                is.MultiFile <- FALSE
                # Set Formula
                #https://stackoverflow.com/questions/5251507/how-to-succinctly-write-a-formula-with-many-variables-from-a-data-frame
                Formula.Covariates <- as.formula(paste("y ~ ", paste(c("x",Covariates), collapse = "+")))
                Model.offset <- ""
            }
        } else {
            # Flag
            is.MultiFile <- FALSE
            # Set Formula
            Formula.Covariates <- as.formula(paste("y ~ ", paste(c("x",Covariates), collapse = "+")))
            Model.offset <- ""
        }
        # Fitting
        if (is.MultiFile) {
            if (!is.null(Degrees) && any("ExpGrowth" %in% Degrees)) {
                # Setting starting values of coefficients
                A0 = coef(lm(y ~ x, data = DataXY))[1]
                A1 = coef(lm(y ~ x, data = DataXY))[2]
                # select data to avoid undefined log (use >0) or negative log and hence negative k (use >1)
                Positives     <- which(DataXY$y - (A0 + A1 * DataXY$x) > 1)
                Start.Value   <- list(a0 = A0,a1 = A1)
                Lower.values  <- c(-Inf, -Inf)
                Upper.values  <- c( Inf,  Inf)
                Degrees.ExpGrowth <- which(Degrees == "ExpGrowth")
                for (j in Degrees.ExpGrowth) {
                    # Formula of the covarariates and final formula
                    if (exists("Formula.ExpGrowth")) rm(Formula.ExpGrowth)
                    Formula.ExpGrowth <- as.formula(paste("log(y[Positives] - (A0 + A1 * x[Positives])) ~ ", Multi.File.df$Covariates[j], "[Positives]"))
                    Model.0 <- lm(Formula.ExpGrowth, data = DataXY)
                    assign(paste0("C.", Multi.File.df$Covariates[j]), coef(Model.0)[1])
                    assign(paste0("k.", Multi.File.df$Covariates[j]), coef(Model.0)[2])
                    Start.Value[paste0("C.", Multi.File.df$Covariates[j])] <- exp(get(paste0("C.", Multi.File.df$Covariates[j])))
                    Start.Value[paste0("k.", Multi.File.df$Covariates[j])] <- get(paste0("k.", Multi.File.df$Covariates[j]))
                    Lower.values  <- c(Lower.values, -Inf, 0)
                    Upper.values  <- c(Upper.values,  Inf, Inf)}
                # polynoms
                for (j in seq_along(Multi.File.df$degree)[-c(Degrees.ExpGrowth, which(Multi.File.df$Covariates == "Intercept"))]) {
                    # degree of polynomial
                    Degrees <-  as.numeric(Multi.File.df[j,"degree"])
                    # Formula of the covarariates and final formula
                    if (exists("Formula.Poly")) rm(Formula.Poly)
                    Formula.Poly <- as.formula(paste("y ~ x ", paste0("I(",Multi.File.df[j,"Covariates"],"^",seq(1:Degrees),")",collapse = "+"), sep = "+"))
                    Model.0 <- lm(Formula.Poly, data = DataXY)
                    for (k in seq(1:Degrees)) {
                        assign(paste0("a.", Multi.File.df$Covariates[j], k), coef(Model.0)[2 + k])
                        Start.Value[paste0("a.", Multi.File.df$Covariates[j], k)] <- get(paste0("a.", Multi.File.df$Covariates[j], k))
                        Lower.values  <- c(Lower.values, -Inf)
                        Upper.values  <- c(Upper.values,  Inf)}}
                
                #fitting with Exponential growth
                if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
                    Model <- minpack.lm::nlsLM(Formula.Covariates, data = DataXY,
                                               start = Start.Value,
                                               lower = Lower.values, upper = Upper.values,
                                               control = nls.lm.control(maxiter = 1024, maxfev = 10000),
                                               model = TRUE, trace = F)
                } else {
                    Model <- minpack.lm::nlsLM(Formula.Covariates, data = DataXY,
                                               start = Start.Value,
                                               lower = Lower.values, upper = Upper.values,
                                               control = nls.lm.control(maxiter = 1024, maxfev = 10000),
                                               model = TRUE, trace = F, weights = wi)
                }
                Equation <- Formula.Covariates
            } else {
                # Fitting polynom only
                Model <- lm(Formula.Covariates, data = DataXY, model = TRUE, x = TRUE, y = TRUE)
                Equation <- paste0(Sensor_name, "MultiLinear: y = ", 
                                   paste(apply(cbind(format(coef(Model), digits = 4, scientific = T), 
                                                     gsub(pattern = paste(c("\\(Intercept\\)","Out\\.", "_volt", "_P1"), 
                                                                          collapse = "|"), replacement = "", names(coef(Model)))),
                                               MARGIN = 1, paste0, collapse = " "), collapse = " + "),
                                   sprintf(paste0(",R2=",f_R2, ",RMSE=",f_R2, ",AIC=%.1f"),
                                           summary(Model)$r.squared, sqrt(sum(resid(Model)^2) / df.residual(Model)), AIC(Model)))
            }
        } else {
            # Fitting polynom only
            if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
                Model <- lm(Formula.Covariates, data = DataXY, model = TRUE, x = TRUE, y = TRUE)
            }  else Model <- lm(Formula.Covariates, data = DataXY, model = TRUE, x = TRUE, y = TRUE, weights = wi)
            Equation <- paste0(Sensor_name, "MultiLinear: y = ", 
                               paste(apply(cbind(format(coef(Model) , digits = 4, scientific = T), 
                                                 gsub(pattern = paste(c("\\(Intercept\\)","Out\\.", "_volt", "_P1"), 
                                                                      collapse = "|"), replacement = "", names(coef(Model)))),
                                           MARGIN = 1, paste0, collapse = " "), collapse = " + "),
                               sprintf(paste0(",R2=",f_R2, ",RMSE=",f_R2, ",AIC=%.1f"),
                                       summary(Model)$r.squared, sqrt(sum(Model$residuals^2) / Model$df), AIC(Model)))
        }
        # plotting calibrationn lines without plotting axis - for multiLinear, it does not make sence since there are several input variables
        # # display equations and R^2
        # mtext(sprintf(paste0(Sensor_name, "Quadr.: y= ",f_coef1,"+ ",f_coef2,"x+ ",f_coef2,"x2",", R2=",f_R2,", s(Res)=",f_coef1,", RMSE=",f_coef1,",AIC= %.1f")
        #               ,coef(Model)[1],coef(Model)[2],coef(Model)[3] ,summary(Model)$r.squared,sd(resid(Model)), sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),AIC(Model))
        #       ,line=line_position,adj = 1,padj = 0, col = Couleur, cex = 0.875)
    } else if (Mod_type == 'Quadratic') {
        if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
            Model <- lm(y ~ poly(x, degree = 2, raw = TRUE), data = DataXY, model = TRUE, x = TRUE, y = TRUE)
        } else {
            Model <- lm(y ~ poly(x, degree = 2, raw = TRUE), data = DataXY , weights = wi, model = TRUE, x = TRUE, y = TRUE)}
        # display equations and R^2
        Equation <- sprintf(paste0(Sensor_name, "Quadr.: y= ",f_coef1,"+ ",f_coef2,"x+ ",f_coef2,"x2",", R2=",f_R2, ", RMSE=",f_coef1,",AIC= %.1f") # ", s(Res)=",f_coef1,
                            ,coef(Model)[1], coef(Model)[2], coef(Model)[3] , summary(Model)$r.squared, sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)), AIC(Model))
    } else if (Mod_type == 'Cubic') {
        if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
            Model <- lm(y ~ poly(x, 3, raw =TRUE), data = DataXY, model = TRUE, x = TRUE, y = TRUE)
        } else {
            Model <- lm(y ~ poly(x, 3, raw=TRUE) , data = DataXY, weights = wi, model = TRUE, x = TRUE, y = TRUE)}
        # display equations and R^2
        Equation <- sprintf(paste0(Sensor_name, "Cubic: y= ",f_coef1,"+",f_coef2,"x+",f_coef2,"x2+",f_coef2,"x3",",R2=",f_R2,",RMSE=", f_coef1,",AIC= %.1f"),
                            coef(Model)[1], coef(Model)[2], coef(Model)[3], coef(Model)[4], summary(Model)$r.squared, sqrt(sum(resid(Model)^2) / (length(resid(Model)) - 2)), AIC(Model))
    } else if (Mod_type == 'ExpDecayInc') {
        if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
            Model <- minpack.lm::nlsLM(y~f_ExpDI(x,C,k), data = DataXY, start = list(C = max(y), k = 0.05), model = TRUE)
        } else {
            Model <- minpack.lm::nlsLM(y~f_ExpDI(x,C,k), data = DataXY, start = list(C = max(y), k = 0.05), weights = wi, model = TRUE)}
        # display equations and R^2
        Equation <- sprintf(paste0(Sensor_name, "Exp. decay inc.: ys = ",f_coef1,"(1-exp(-",f_coef2,"x))", ",RMSE=",f_coef1,",AIC= %.1f"),
                            coef(Model)[1], coef(Model)[2], sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)), AIC(Model))
    } else if (Mod_type == 'ExpDecayInc_Int') {
        if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
            Model <- minpack.lm::nlsLM(y ~ f_ExpDI_Int(x, C, k,intercept), data = DataXY, start = list(C = max(y, na.rm = T), k = 0.05, intercept = min(y, na.rm = T)), model = TRUE)
        } else {
            Model <- minpack.lm::nlsLM(y ~ f_ExpDI_Int(x, C, k,intercept), data = DataXY, start = list(C = max(y, na.rm = T), k = 0.05, intercept = min(y, na.rm = T)), weights = wi, model = TRUE)}
        # display equations and R^2
        Equation <- sprintf(paste0(Sensor_name, "Exp. decay inc.: ys = ",f_coef2,"(1-exp(-",f_coef2,"x))+",f_coef1,",RMSE=", f_coef1,",AIC= %.1f"),
                            coef(Model)[1], coef(Model)[2], coef(Model)[3], sqrt(sum(resid(Model)^2) / df.residual(Model)), AIC(Model))
    } else if (Mod_type == 'ExpDecayDec_Int') {
        if (is.null(s_y) || any(s_y == 0) || all(is.na(s_y))) {
            Model <- nls(y~f_ExpDD_Int(x, C, k,intercept), data = DataXY, start=list(C = max(y, na.rm = T), k = 0.05, intercept = min(y, na.rm = T)), model = TRUE)
        } else {
            Model <- nls(y~f_ExpDD_Int(x, C, k,intercept), data = DataXY, start=list(C = max(y, na.rm = T), k = 0.05, intercept = min(y, na.rm = T)), weights = wi, model = TRUE)}
        # display equations and R^2
        Equation <- sprintf(paste0(Sensor_name, "Exp. decay dec: y = ",f_coef2,"exp(-",f_coef2,"x))+",f_coef1,",RMSE=", f_coef1,",AIC= %.1f"),
                            coef(Model)[1], coef(Model)[2], coef(Model)[3], sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)), AIC(Model))
    } else if (Mod_type == 'Michelis') {
        if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
            Model <- minpack.lm::nlsLM(y ~ MIN + f_Michelis(x, Vmax, km, MIN), data=DataXY, start=list(Vmax=max(y, na.rm = T), km=mean(y)/4, MIN = min(y, na.rm = T)), model = TRUE)
        } else {
            Model <- minpack.lm::nlsLM(y ~ f_Michelis(x, Vmax, km, MIN), data=DataXY, start=list(Vmax=max(y, na.rm = T), km=mean(y)/4, MIN = min(y, na.rm = T)), weights = wi, model = TRUE)}
        # display equations and R^2
        Equation <- sprintf(paste0(Sensor_name, "Michelis: y = ",f_coef2,"/(",f_coef2,"+x)+",f_coef1,",RMSE=",f_coef1,",AIC= %.1f"), # ", s(Res)=", f_coef1,
                            coef(Model)[1], coef(Model)[2], coef(Model)[3], sqrt(sum(resid(Model)^2) / df.residual(Model)), AIC(Model))
    } else if (Mod_type == 'Logarithmic') {
        if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
            Model <- nls(y ~ f_log(x,a,b), data = DataXY, start = list(a = min(y, na.rm = T), b = 10), model = TRUE)
        } else {
            Model <- nls(y ~ f_log(x,a,b), data = DataXY, start = list(a = min(y, na.rm = T), b = 10), weights = wi, model = TRUE)
        }
        # plotting calibrationn lines without plotting axis
        DataXY <- DataXY[order(DataXY$x),] # To avoid that the line go back for lower x
        Estimated.y(DataXY$x, Model)
        # display equations and R^2
        Equation <- sprintf(paste0(Sensor_name, "Log. model: y = ",f_coef1," + ",f_coef2," log(x)), ", ",RMSE=",f_coef1,",AIC= %.1f"), # " s(Res)=", f_coef1,
                            coef(Model)[1], coef(Model)[2], sqrt(sum(Model$residuals^2) / Model$df), AIC(Model))
    } else if (Mod_type == 'Sigmoid') {
        #nls.control(maxiter = 500, tol = 1e-05, minFactor = 1/4096, printEval = TRUE, warnOnly = TRUE)
        if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
            # Model <- nls(y~f_Sigmoid(x, MIN, MAX, X50, Hill), data=DataXY, start=list(MIN=min(y),MAX=max(y),X50=mean(x), Hill=3)
            #               , control = list(maxiter = 500, tol=1e-2, minFactor = 1/1024, printEval = TRUE, warnOnly=FALSE), trace=TRUE)
            Model <- minpack.lm::nlsLM(DataXY$y ~ f_Sigmoid(x, MIN, Asym, xmid, Hill), data = DataXY, start = list(MIN = min(y, na.rm = T), Asym = max(y), xmid = mean(x, na.rm = T), Hill = 3)
                                       , control = list(maxiter = 500, tol = 1e-2, minFactor = 1/1024, printEval = TRUE, warnOnly = FALSE), trace = FALSE, model = TRUE)
        } else {
            Model <- minpack.lm::nlsLM(y ~ f_Sigmoid(x, MIN, Asym, xmid, Hill), data = DataXY, start=list(MIN=min(y, na.rm = T),Asym=max(y),xmid=mean(x, na.rm = T), Hill = 3), weights = wi
                                       , control = list(maxiter = 500, tol = 1e-82, minFactor = 1/1024, printEval = TRUE, warnOnly = FALSE), trace = FALSE, model = TRUE)
            #Model <- minpack.lm::nlsLM(y ~ MIN + SSlogis(x, Asym, xmid, scal) , data=DataXY, start=list(MIN=min(y),Asym=max(y),xmid=mean(x), scal = 3), weights = wi ,
            #               control = list(maxiter = 500, tol=1e-2, minFactor = 1/(1024*32), printEval = TRUE, warnOnly=FALSE), trace=TRUE, model = TRUE)
        }
        # display equations and R^2
        Equation <- sprintf(paste0(Sensor_name, "Sigmoid: y=",f_coef1,"+",f_coef2,"/(1+(",f_coef2,"/x)^",f_coef2,"), ", ",RMSE=",f_coef1,",AIC= %.1f"), # "s(Res)=", f_coef1,
                            coef(Model)[1], coef(Model)[2] - coef(Model)[1], coef(Model)[3], coef(Model)[4], sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)), AIC(Model))
    } else if (Mod_type == 'Unitec') {
        #Put O3, sensor responses and standard deviation of sensor responses in a matrix: Not done for now
        a <- -31.6
        b <- 5330.9
        c <- -0.598
        DataXY <- data.frame(cbind(x*2.05, ((y-a)/b)^(1/c), s_y))
        colnames(DataXY) <- c("x","y")
        if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
            Model <- nls(y~f_Unitec(x, a, b, c), data=DataXY, start=list(a=-31.6,b=5330.9,-0.598), model = TRUE, x = TRUE, y = TRUE)
        } else {
            Model <- nls(y~f_Unitec(x, a, b, c), data=DataXY, start=list(a=-31.6,b=5330.9,-0.598), model = TRUE, x = TRUE, y = TRUE, weights = wi)
        }
        # display equations and R^2
        Equation <- sprintf(paste0(Sensor_name, "Unitec: y = ((1.91x(293.15/T)-",f_coef1,")/",f_coef2,")^(1/",f_coef2,"), ", ",RMSE=",f_coef1,",AIC= %.1f"),  # "s(Res)=", f_coef1,
                            coef(Model)[1], coef(Model)[2], coef(Model)[3], coef(Model)[4], sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)), AIC(Model))
    } else if (Mod_type == 'PM_Normal_Dist') {
        # Setting Initial values
        #MU <- log(dimatro) che corrisponde al max dei counts
        #SIGMA <- coef(Linear.Model)[2]
        # Fitting model
        if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
            Model <- minpack.lm::nlsLM(y ~ f_Normal(x, mu, sigma), data = DataXY, start = list(mu = MU, sigma = SIGMA), model = TRUE)
        } else Model <- minpack.lm::nlsLM(y ~ f_Normal(x, mu, sigma), data = DataXY, start = list(mu = MU, sigma = SIGMA), model = TRUE, weights = wi)
    } else if (Mod_type == 'bi_Normal') {
        Model <- minpack.lm::nlsLM(y ~  K1* dnorm(x, mu1, sigma1) + K2* dnorm(x, mu2, sigma2) + C ,
                                   data    = DataXY,
                                   start   = list(mu1 = MU1, sigma1 = 0.2, K1 = K1, mu2 = MU2, K2 = K2, sigma2 = 0.2, C = C),
                                   model   = TRUE, trace = T)
    } else if (Mod_type %in% c('Kohler_modified')) {
        # Selecting RH for setting Initial a0 and a1 values of Kohler_modified Model selecting RH < Min.RH where Min.RH is the 30th percentile of RH
        starting_probs <- 0.3
        Min.RH <- quantile(DataXY[[Covariates]], probs = starting_probs)
        repeat{
            # Model_a0_a1 <- lm(y ~ x, data = DataXY[DataXY[[Covariates]] < Min.RH], model = TRUE)
            Model_a0_a1 <- quantreg::rq(y ~ x, data = DataXY[DataXY[[Covariates]] < Min.RH], tau = 0.9, model = TRUE)
            # avoiding a1 near by 0: Upper and lower prevent a1 from varying and nlsLM may end in Singular Matrix. The model will be wrong but the code does not crash
            if(abs(coef(Model_a0_a1)[2]) > 1e-3){
                break
            } else {
                if(starting_probs < 0.9){
                    starting_probs <- starting_probs + 0.1
                    futile.logger::flog.info(paste0("[Cal_Line] model Kohloer_modified, percentile of Min.RH set to ", starting_probs, " between ", paste(range(as.Date(DataXY$date)), sep = " and ")))
                    Min.RH <- quantile(DataXY[[Covariates]], probs = starting_probs)
                } else stop(paste0("[Cal Line] ERROR cannot fit a1 != 0 between ", paste(range(DataXY$date), sep = " and ")))
            }
        }
        # Default values (Antwerp Median) in case a0 or a1 are out of intervall of confidence
        if(!is.null(Sensor_name)){
            if(grepl("5301CST", Sensor_name)){
                a0_Limits <- c(-2.01, 0.83);  a0_Median <-  0.14; a0_sd <- 0.97
                a1_Limits <- c(0.86, 1.85) ;  a1_Median <-  0.98; a1_sd <- 0.35
                K_Limits  <- c(0.013, 0.172); K_Median  <- 0.109; K_sd <- 0.047
            } else if(grepl("5325CST", Sensor_name)){
                a0_Limits <- c(-4.31, -0.94); a0_Median <- -3.14; a0_sd <- 0.84
                a1_Limits <- c(1.02, 1.55) ;  a1_Median <-  1.40; a1_sd <- 0.10
                K_Limits  <- c(0.066, 0.204); K_Median  <- 0.135; K_sd <- 0.028
            } else if(grepl("5310CST", Sensor_name)){
                a0_Limits <- c(-7.06, 0.16);  a0_Median <- -4.96; a0_sd <- 2.44
                a1_Limits <- c(0.16,1.91) ;   a1_Median <-  0.74; a1_sd <- 0.50
                K_Limits  <- c(0.037, 0.460); K_Median  <- 0.327; K_sd <- 0.062
            } else if(grepl("5301CAT", Sensor_name)){
                a0_Limits <- c(-0.44, 2.65);  a0_Median <- 1.66;  a0_sd <- 0.94
                a1_Limits <- c(0.16, 0.91) ;  a1_Median <- 0.82;  a1_sd <- 0.27
                K_Limits  <- c(0.067, 0.458); K_Median  <- 0.082; K_sd <- 0.136
            } else if(grepl("5325CAT", Sensor_name)){
                a0_Limits <- c(-2.81, -0.57); a0_Median <- -1.63; a0_sd <- 0.47
                a1_Limits <- c(0.99, 2.10) ;  a1_Median <-  1.22; a1_sd <- 0.17
                K_Limits  <- c(0.046, 0.168); K_Median  <- 0.077; K_sd <- 0.018
            } else if(grepl("5325CAT", Sensor_name)){
                a0_Limits <- c(-6.07, 5.73);  a0_Median <- -4.36; a0_sd <- 2.78
                a1_Limits <- c(0.10,1.30) ;   a1_Median <-  0.71; a1_sd <- 0.32
                K_Limits  <- c(0.023, 0.460); K_Median  <- 0.269; K_sd <- 0.108
            } else if(grepl("OPCN3PM1", Sensor_name)){
                a0_Limits <- c(-0.94, 0.58);  a0_Median <- -0.29; a0_sd <- 0.29
                a1_Limits <- c(0.09, 0.47) ;  a1_Median <-  0.26; a1_sd <- 0.09
                K_Limits  <- c(0.164, 0.919); K_Median  <- 0.346; K_sd <- 0.172
            } else if(grepl("OPCN3PM25", Sensor_name)){
                a0_Limits <- c(-2.15, 1.17); a0_Median <-  0.02; a0_sd <- 0.88
                a1_Limits <- c(0.07, 2.06);  a1_Median <-  0.29; a1_sd <- 0.56
                K_Limits  <- c(0.00, 1.09);  K_Median  <- 0.606; K_sd  <- 0.357
            } else if(grepl("OPCN3PM10", Sensor_name)){
                a0_Limits <- c(-21.75, 2.58); a0_Median <- -0.41; a0_sd <- 4.91
                a1_Limits <- c(0.14, 1.43) ;  a1_Median <-  0.44; a1_sd <- 0.37
                K_Limits  <- c(0.092, 1.744); K_Median  <- 0.588; K_sd <- 0.302
            } else if(grepl("NPMPM25", Sensor_name)){
                browser()
                a0_Limits <- c(0,1)
                a1_Limits <- c(0,1)
            } else if(grepl("NPMPM10", Sensor_name)){
                browser()
                a0_Limits <- c(0,1)
                a1_Limits <- c(0,1)
            } 
            if(coef(Model_a0_a1)[1] < a0_Limits[1] || coef(Model_a0_a1)[1] > a0_Limits[2] ||
               coef(Model_a0_a1)[2] < a1_Limits[1] || coef(Model_a0_a1)[2] > a1_Limits[2]) {
                browser()
                coef(Model_a0_a1)[1] <- a0_Median
                coef(Model_a0_a1)[2] <- a1_Median
            }
        } else {
            if(abs(coef(Model_a0_a1)[2]) < 1e-3) browser()
        }
        # Fitting for K 
        browser()
        Max.RH <- quantile(DataXY[[Covariates]][DataXY[[Covariates]] < 95], probs = 0.7)
        Start <- list(a0 = coef(Model_a0_a1)[1], a1 = coef(Model_a0_a1)[2], K = K_Median) # 0.41/1.65
        Lower <- c(coef(Model_a0_a1)[1], coef(Model_a0_a1)[2], 0)
        Upper <- c(coef(Model_a0_a1)[1], coef(Model_a0_a1)[2], 5)
        # Fitting model
        Formula <- as.formula(paste0("y ~ f_Kohler_modified(x, a0, a1, K, RH = ", Covariates, ")"))
        if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
            # if (c("Out.Ref.RH", "Relative_humidity_modelled") %in% names(DataXY)) {
            Nrows  <- which(DataXY[[Covariates]] != 0 & DataXY[[Covariates]] < 95)
            DataXY <- DataXY[Nrows,]
            # }
            tryCatch({
                Model <- minpack.lm::nlsLM(Formula, data = DataXY[DataXY[[Covariates]] > Max.RH], start = Start, model = TRUE,
                                           control = nls.lm.control(maxiter = 1024, maxfev = 10000),
                                           lower = Lower, upper = Upper)},
                error = function(e,DataXY) {
                    browser()
                    futile.logger::flog.warn(paste0("[Cal_Line] error when fitting a Koler_modified model, fitting with fixed starting values"))
                    Lower <- c(coef(Model_a0_a1)[1], coef(Model_a0_a1)[2], 0.41/1.65)
                    Upper <- c(coef(Model_a0_a1)[1], coef(Model_a0_a1)[2], 0.41/1.65)
                    
                    minpack.lm::nlsLM(Formula, data = DataXY[DataXY[[Covariates]] > Max.RH], start = Start, model = TRUE,
                                      control = nls.lm.control(maxiter = 1024, maxfev = 10000),
                                      lower = Lower, upper = Upper)})
        } else {
            Model <- minpack.lm::nlsLM(Formula, data = DataXY, start = Start, model = TRUE, weights = wi,
                                       control = nls.lm.control(maxiter = 1024, maxfev = 10000),
                                       lower = Lower, upper = Upper)
        }
        # display equations and R^2
        Equation <- paste0(Sensor_name, "Kohler: y = (", f_coef1, " + ", f_coef2, " * x) * (1 + 1.65 * ", f_coef2," / (1/RH-1)), RMSE=",f_coef1,", AIC= %.1f")
        Equation <- sprintf(Equation,
                            coef(Model)[1],coef(Model)[2],coef(Model)[3],
                            sqrt(sum(resid(Model)^2) / df.residual(Model)), AIC(Model))
    } else if (Mod_type %in% c('Kohler', 'Kohler_lit')) {
        # Setting Initial values of Kohler Model
        if(Mod_type == "Kohler"){
            Start <- list(a0 = 0, a1 = 1, K = 0.41)
        } else if(Mod_type == "Kohler_lit") {
            DataXY[, Cx := x * (1+0.38/(-1+100/DataXY[[Covariates]]))]
            Model_lit <- lm(y ~ Cx,data = DataXY)
            Start <- list(a0 = coef(Model_lit)[1], a1 = coef(Model_lit)[2], K = 0.38)
        }
        Lower <- c(-50, 0, 0)
        Upper <- c(+50, 5  , Inf)
        # Fitting model
        Formula <- as.formula(paste0("y ~ f_Kohler(x, a0, a1, K, RH = ", Covariates, ")"))
        if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
            if ("Out.Ref.RH" %in% names(DataXY)) {
                Nrows <- which(DataXY$Out.Ref.RH != 0 & DataXY$Out.Ref.RH != 100)
                DataXY <- DataXY[Nrows,]}
            
            Model <- minpack.lm::nlsLM(Formula, data = DataXY, start = Start, model = TRUE,
                                       control = nls.lm.control(maxiter = 1024, maxfev = 10000),
                                       lower = Lower, upper = Upper)
        } else {
            Model <- minpack.lm::nlsLM(Formula, data = DataXY, start = Start, model = TRUE, weights = wi,
                                       control = nls.lm.control(maxiter = 1024, maxfev = 10000),
                                       lower = Lower, upper = Upper)
        }
        # display equations and R^2
        Equation <- paste0(Sensor_name, "Kohler: y = ", f_coef1," + ",f_coef2," x * (1+(",f_coef2,"/1.65)/(1/RH-1) ), RMSE=",f_coef1,", AIC= %.1f")
        Equation <- sprintf(Equation,
                            coef(Model)[1],coef(Model)[2],coef(Model)[3],
                            sqrt(sum(resid(Model)^2) / df.residual(Model)), AIC(Model))
    } else if (Mod_type == 'Kohler_only') {
        # Setting Initial values of Kohler_only Model
        Start <- list(K = 0.41)
        Lower <- -5
        Upper <- 5
        # Fitting model
        Formula <- as.formula(paste0("y ~ f_Kohler_only(x, K, RH = ", Covariates, ")"))
        if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
            Model <- minpack.lm::nlsLM(Formula, data = DataXY, start = Start, model = TRUE,
                                       control = nls.lm.control(maxiter = 1024, maxfev = 10000),
                                       lower = Lower, upper = Upper)
        } else Model <- minpack.lm::nlsLM(Formula, data = Start, model = TRUE, weights = wi,
                                          control = nls.lm.control(maxiter = 1024, maxfev = 10000),
                                          lower = Lower, upper = Upper)
        # display equations and R^2
        Equation <- paste0(Sensor_name, "Kohler: y =  x * (1+(",f_coef2,"/1.65)/(1/RH-1) ), RMSE=",f_coef1,", AIC= %.1f")
        Equation <- sprintf(Equation, coef(Model)[1], sqrt(sum(resid(Model)^2) / df.residual(Model)), AIC(Model))
    } else if (Verbose) futile.logger::flog.warn(paste0("[Cal_Line] unknown calibration model ", Mod_type, "\n"))
    
    # Adding printing summary of Model
    if (Verbose){
        if(Mod_type  == "Deming"){
            print(Model)
        } else if(Mod_type == "TLS") {
            print(Model$summary)
        } else print(summary(Model))}
    
    # Plotting and resuming the par values
    if (Plot_Line && Mod_type != "Yatkin"){
        # Plotting
        if (is.null(Ggplot) || !any(grepl("ggplot", class(Ggplot)))){ 
            
            # saving the original par() values
            op <- par(no.readonly = TRUE)
            # resuming the par values
            #on.exit(par(op))
            
            #Define the limits of the graphs
            if (is.null(lim)) {
                Xrange <- c(min(x, na.rm = T), max(x, na.rm = T))
                Yrange <- c(min(y, na.rm = T), max(y, na.rm = T))
                lim = cbind(Xrange, Yrange)}
            
            # settings the margins
            if (!is.null(marges)) par(mar = marges) else if (all(c("mar12","mar34")%in% colnames(lim))) par(mar = c(lim[,"mar12"],lim[,"mar34"]))
            
            # plotting calibration lines
            if ("Xlim" %in% colnames(lim)) Xlim <- lim[,"Xlim"] else if("Limits" %in% names(lim)) Xlim <- lim[["Limits"]][,1] else  Xlim <- lim[,1]
            if ("Ylim" %in% colnames(lim)) Ylim <- lim[,"Ylim"] else if("Limits" %in% names(lim)) Xlim <- lim[["Limits"]][,2] else  Ylim <- lim[,2]
            if ("Xusr" %in% colnames(lim) && "Yusr" %in% colnames(lim)) par(usr = c(lim[,"Xusr"],lim[,"Yusr"]))
            
            # Base plot
            if (Mod_type %in% c("Identity","Linear","Linear.Robust")) {
                abline(a = coef(Model)[1], b= coef(Model)[2], col = Couleur)
                if (Weighted) {
                    points(DataXY$x, DataXY$y, col = Couleur, xlim = Xlim, ylim = Ylim, xaxt = "n", yaxt = "n" , xlab = "", ylab = "")
                    par(new = TRUE)
                    arrows(DataXY$x, DataXY$y - DataXY$s_y, DataXY$x, DataXY$y + DataXY$s_y, length = 0.05, angle = 90, code = 3, col = Couleur)}
            } else if (Mod_type %in% c("TLS","Deming")) {
                abline(a = coef(Model)[1], b= coef(Model)[2], col = Couleur)
                # adding error bars for Deming
                if (Mod_type == c("Deming")){
                    arrows(DataXY$x, DataXY$y - DataXY$s_y, DataXY$x, DataXY$y + DataXY$s_y, length = 0.05, angle = 90, code = 3, col = Couleur)
                    arrows(DataXY$x - DataXY$s_x, DataXY$y, DataXY$x + DataXY$s_x, DataXY$y, length = 0.05, angle = 90, code = 3, col = Couleur)
                }
            } else if (Mod_type %in% c('gam', 'Quadratic', 'Unitec', 'Sigmoid', 'Michelis', 'ExpDecayDec_Int', 'ExpDecayInc_Int', 'ExpDecayInc', 'Cubic')) {
                Estimated <- Estimated.y(DataXY$x, Model)
                Estimated <- Estimated[order(Estimated$x),]
                lines(Estimated$x,Estimated$y, col = Couleur)}
            
            # Adding equation text to basepplot
            if (!is.null(Equation) && Mod_type != "Yatkin") mtext(Equation, line = line_position, adj = 1, padj = 0, col = Couleur, cex = 0.875)
            
        } else {
            
            # adding line to ggplot()
            Ylim <- layer_scales(Ggplot)$y$range$range
            Xlim <- layer_scales(Ggplot)$x$range$range
            Estimated <- Estimated.y(DataXY$x, Model)
            # Discarding x and y outside Xlim and Ylim
            Identified.rows <- Estimated$x >= Xlim[1] & Estimated$x <= Xlim[2] & Estimated$y >= Ylim[1] & Estimated$y <= Ylim[2]
            Ggplot <- Ggplot +
                geom_line(data = Estimated[Identified.rows,], aes(x = x , y = y), color = Couleur)
            # Adding errors bars
            if(Weighted){
                Ggplot <- Ggplot +
                    geom_point(data = DataXY, aes(x=x, y = y), color = Couleur) +
                    geom_errorbar(data = DataXY, aes(ymin = y - s_y, ymax = y + s_y), color = Couleur)
                
            } else if (Mod_type %in% c("Deming")){
                
                # adding error bars for Deming
                Ggplot <- Ggplot +
                    geom_errorbar(aes(ymin = y - s_y, ymax = y + s_y), color = "blue") +
                    geom_errorbarh(aes(xmin = x -s_x, xmax = x + s_x), color = "blue")}
            
            # Adding other covariates values
            if(!is.null(Matrice)){
                Conditions <- round(Matrice[, lapply(.SD, mean), .SDcol = grep(paste(c("date","_StDev", "Step"), collapse = "|"), names(Matrice), value = T, invert = T)], digits = 1)
                Conditions2drop <- unname(unlist(sapply(names(Conditions), function(Column, Matrice, DataXY){
                    if (grepl(paste0(DataXY$x, collapse = ";"), paste(Matrice[[Column]], collapse = ";")) || grepl(paste0(DataXY$y, collapse = ";"), paste(Matrice[[Column]], collapse = ";"))) return(Column)
                }, Matrice, DataXY)))
                if(length(Conditions2drop) > 0) Conditions[, (Conditions2drop):= NULL]
                library(ggpmisc)
                Cov.Table <- data.frame(Covariates=names(Conditions), values = t(Conditions))
                # discard Absolute_humidity which is too long to plot
                if ("Absolute_humidity" %in% Cov.Table[,1]) Cov.Table <- Cov.Table[-which(Cov.Table[,1] == "Absolute_humidity"),]
                Ggplot <- Ggplot +
                    annotate(geom = "table", x = layer_scales(Ggplot)$x$range$range[1], y = mean(layer_scales(Ggplot)$y$range$range, na.rm = T),
                             label = list(Cov.Table))}
            
            # Adding equation text to pplot and ggplot()
            if (!is.null(Equation) && Mod_type != "Yatkin"){
                if (!is.null(Ggplot$labels$subtitle)){
                    #https://stackoverflow.com/questions/60387023/can-ggplot-titles-contain-line-breaks-when-used-with-ggtext
                    Ggplot <- Ggplot +
                        labs(subtitle = paste0(Ggplot$labels$subtitle, "<br>", "<span style = 'color:", Couleur,";'>",Equation,"</span>")) +
                        theme(plot.subtitle=ggtext::element_markdown(size=10, hjust=1, face="italic"))
                } else Ggplot <- Ggplot + labs(subtitle = paste0("<span style = 'color:", Couleur,";'>",Equation,"</span>")) +
                        theme(plot.subtitle=ggtext::element_markdown(size=10, hjust=1, face="italic"))}
            
            # Plotting Ggplot
            Ggplot
            
            # adding ggplot to Model
            Model$Ggplot <- Ggplot
        }
    } 
    # Adding the equation to the model
    if (!is.null(Equation) && Mod_type != "Yatkin") Model$Equation <- Equation
    return(Model)
}

#================================================================CR
### Meas_Function: Function Measurement Function x = f(y) once Calibration function (y = f(x) of sensor is established e.g with Cal_Line ====
#================================================================CR
#' This function estimates the x value using a calibration model (Model)
#' @param name  y Sensor data to be converted to concentration level using the reverse calibration function (Model)
#' @param Mod_type     : type of calibration function: Linear, Quadratic, Sigmoid
#' @param Model        : the calibration function
#' @param covariates   : vectors of strings representing the covariates when model needs covariates
#' @param degrees      : vector of string, degrees of the covariates for multiLinear calibration
#' @param Matrice      : Covariates values, data.frame, default is NULL
#' @param name.sensor character vector, default is NULL, optional name of sensor used to recognize relative humidity sensor
#' @param name.Model  character vector, default is NULL, optional, name of the file of calibration function. Use for Yatkin only to save Cal.RH.Inc and Cal.RH.Dec
Meas_Function <- function(y, Mod_type, Model, covariates = NULL, Degrees = NULL, Matrice = NULL, Date = NULL, name.sensor= NA_character_, name.Model = NULL, Verbose = FALSE) {
    if (Mod_type %in% c('Identity', 'Linear', 'Linear.Robust')) {
        return.cal <- (y - Model$Coef[1])/Model$Coef[2]
        if (name.sensor %in% "SHT31HE"){
            RH_95 <- which(return.cal > 95)
            if (length(RH_95) > 0) return.cal[RH_95] <- 95} 
        return(return.cal)
    } else if (Mod_type == 'TLS') {
        browser()
        return.cal <- (y - Model$Coef[1])/Model$Coef[2]
        if (name.sensor %in% "SHT31HE"){
            RH_95 <- which(return.cal > 95)
            if (length(RH_95) > 0) return.cal[RH_95] <- 95} 
        return(return.cal)
    } else if (Mod_type == 'Linear.Robust.rqs') {
        DataXY <- data.table(y = y)
        DataXY[, quantile := cut(y, quantile(Model$Augment$y, probs = seq(0.1,0.9,0.1)))]
        levels(DataXY$quantile) <- 1:ncol(Model$Coef)
        # adding levels for y outside values of calibration
        Which.Low  <- which(y<quantile(Model$Augment$y, probs = 0.1))
        Which.High <- which(y>quantile(Model$Augment$y, probs = 0.9))
        if (length(Which.Low)  > 0) data.table::set(DataXY, i = Which.Low,  j = "quantile", value = 1)
        if (length(Which.High) > 0) data.table::set(DataXY, i = Which.High, j = "quantile", value = ncol(Model$Coef))
        DataXY[, slope := Model$Coef[2,DataXY$quantile]]
        DataXY[, intercept := Model$Coef[1,DataXY$quantile]]
        DataXY[, x := (DataXY$y - DataXY$intercept) / DataXY$slope]
        return(DataXY$x)
    } else if (Mod_type == 'MultiLinear') {
        # convert any column of Matrice that is not numeric (Date) to numeric
        if (!all(grepl(pattern = "numeric", x = sapply(lapply(Matrice, class), "[", 1)))) {
            Col.no.numeric <- grep(pattern = "numeric", x = sapply(lapply(Matrice, class), "[", 1), invert = TRUE)
            futile.logger::flog.warn(paste0("[Meas_Function] \"",paste(names(Matrice)[Col.no.numeric], collapse = ", "), "\" is(are) not numeric. Converting to numeric."))
            Matrice[,Col.no.numeric] <- sapply(Matrice[,Col.no.numeric],as.numeric)
        }
        # matrix of covariates
        # Remove NewMatrix if it exists
        if (exists("NewMatrice")) rm(NewMatrice)
        # Checking if there are polynomials
        if (any(grepl(pattern = paste0("I\\("), x = names(Model$Coef)[3:length(Model$Coef)]) )) {
            # checking for the order of poly
            for (j in names(Matrice)) {
                # Checking if any Covariates are poly() create a new Matrice
                if (any(grepl(pattern = paste0("I\\(",j), x = names(Model$Coef)[3:length(Model$Coef)]) )) {
                    # Getting the degrees of j in poly() (e. g. I(Temperature^1) )
                    Cov.Index <- grep(pattern = j, x = names(Model$Coef)[3:length(Model$Coef)])
                    Power = as.numeric(sub(pattern = ")",
                                           replacement = "",
                                           x = sub(pattern = ".*\\^",
                                                   replacement = "",
                                                   x = names(Model$Coef)[3:length(Model$Coef)][Cov.Index]
                                           )
                    )
                    )
                    # Add the Covariates to Matrice, it is poly
                    for (k in Power) {
                        if (exists("NewMatrice")) {
                            NewMatrice <- cbind(NewMatrice, Matrice[,j]^k)
                        } else NewMatrice  <- as.data.frame(Matrice[,j]^k)
                    }
                } else {
                    # Add the Covariates to Matrice, it is not poly
                    if (exists("NewMatrice")) NewMatrice  <- cbind(NewMatrice, Matrice[,j]) else NewMatrice  <- Matrice[,j]
                }
            }
            # update Matrice if needed
            Matrice <- NewMatrice
            rm(NewMatrice)
        }
        if (class(Matrice) != "matrix") Matrice <- as.matrix(Matrice)
        if (any("ExpGrowth" %in% Degrees)) {
            Degrees.Exp <- which("ExpGrowth" == Degrees)
            for (j in Degrees.Exp) {
                Coef.Growth <- grep(pattern = paste0("k.",covariates[j]), x = Model$Tidy$term)
                M.Cov.Growth <- Model$Coef[Coef.Growth - 1] * exp(Model$Coef[Coef.Growth] * Matrice[,covariates[j]])
                if (exists("M.Cov")) M.Cov <- M.Cov + M.Cov.Growth else M.Cov <- M.Cov.Growth
            }
            for (j in seq_along(covariates)[-Degrees.Exp]) {
                Coef.Not.Growth  <- grep(pattern = paste0("a.",covariates[j]), x = Model$Tidy$term)
                M.Cov.Not.Growth <- Matrice[, grep(pattern = covariates[j], colnames(Matrice))] %*% t(Model$Coef[Coef.Not.Growth])
                if (exists("M.Cov")) M.Cov <- M.Cov + M.Cov.Not.Growth else M.Cov <- M.Cov.Not.Growth
            }
        } else M.Cov <- Matrice %*% Model$Coef[3:length(Model$Coef)]
        Estimated <- as.vector((y - (Model$Coef[1] + M.Cov ))/Model$Coef[2])
        return(Estimated)
    } else if (Mod_type == 'Ridge') {
        # convert any column of Matrice that is not numeric (Date) to numeric
        if (!all(grepl(pattern = "numeric", x = sapply(lapply(Matrice, class), "[", 1)))) {
            Col.no.numeric <- grep(pattern = "numeric", x = sapply(lapply(Matrice, class), "[", 1), invert = TRUE)
            futile.logger::flog.warn(paste0("[Meas_Function] \"",paste(names(Matrice)[Col.no.numeric], collapse = ", "), "\" is(are) not numeric. Converting to numeric."))
            Matrice[,Col.no.numeric] <- sapply(Matrice[,Col.no.numeric],as.numeric)
        }
        # Class transformation
        if (class(Matrice) != "matrix") Matrice <- as.matrix(Matrice)
        if (class(Model$Coef) == "dgCMatrix") Model$Coeff <- Model$Coef[,1]
        # Inverse of calibration model
        M.Cov <- Matrice %*% Model$Coef[3:length(Model$Coef)]
        Estimated <- as.vector((y - (Model$Coef[1] + M.Cov ))/Model$Coef[2])
        return(Estimated)
    } else if (Mod_type == 'Quadratic') {
        # le choix de la racine du polynome depend du signe du coefficient du monome de 2eme degre
        # et si l'on utilise la partie croissante ou decroissante de la parabole
        # Si le determinat est negatif : NA
        x <- data.frame(y = y,determinant = Model$Coef[2]^2 - 4*Model$Coef[3]*(Model$Coef[1] - y), root = NA, stringsAsFactors = FALSE)
        if (Model$Coef[2] > 0) {
            x[x$determinant > 0 ,"root"] <- (-Model$Coef[2] + sqrt(x[x$determinant > 0,"determinant"]))/(2*Model$Coef[3])
        } else {
            x[x$determinant > 0 ,"root"] <- (-Model$Coef[2] - sqrt(x[x$determinant > 0,"determinant"]))/(2*Model$Coef[3])
        }
        return(x$root)
    } else if (Mod_type == 'Cubic') {
        require(polynom)
        CubicRootValue <- function(y) {
            # Solving cubic equation using the eigen numbers of package polynom
            # for x^3-8 = 0 , it is the same as:
            # a <- c(0,0,8)
            # m <- matrix(c(0,0,-a[1],1,0,-a[2],0,1,-a[3]), byrow=T, nrow=3)
            # roots <- eigen(m, symm=F, only.values=T)$values
            # see https://stackoverflow.com/questions/2003465/fastest-numerical-solution-of-a-real-cubic-polynomial
            p <- polynom::polynomial(c(Model$Coef[1] - y,Model$Coef[2],Model$Coef[3],Model$Coef[4]))
            Roots <- solve(p)
            # Discarding complex number, and negative values
            Roots <- sapply(1:3, function(nRoots) if (Im(Roots[nRoots]) != 0 || Re(Roots[nRoots]) < 0) Roots[nRoots] <- NA else Roots[nRoots] <- as.numeric(Re(Roots[nRoots])) )
            # Returning only one root, if more than one, take the smallest one non negative, root in position 1
            if (any(!is.na(Roots[1:3]))) {
                if (length(which(!is.na(Roots[1:3]))) == 1) Roots[1] <- Roots[which(!is.na(Roots[1:3]))] else Roots[1] <- min(Roots[ which(!is.na(Roots[1:3]))], na.rm = TRUE)
            }
            return(Roots[1])
        }
        return(sapply(y, CubicRootValue))
    } else if (Mod_type == "gam") { # checking if the model was fitted from gam function
        return(predict(Model, newdata = data.frame(x = y[!is.na(y)], y = rep(numeric(0), length = length(y[!is.na(y)]))), type = "response"))
    } else if (Mod_type == 'Exp_Translations') {
        # y = C + k^(x-x0), k > 0
        # rewrite k^x = (y-C)/k(-x0) and x = log((y - C)/ k^-x0)/log(k())
        return(log((y - coef(Model)["C"]) / coef(Model)["k"]^-coef(Model)["x0"]) / log(coef(Model)["k"]))
    } else if (Mod_type == 'Yatkin') {
        # detect the increasing and decreasing RH and apply model accordingly
        # rewrite k^x = (y-C)/k(-x0) and x = log((y - C)/ k^-x0)/log(k())
        name.Temperature <- grep("emp" , covariates, value = T)
        name.Humidity    <- grep("umid", covariates, value = T)
        if(!data.table::is.data.table(Matrice)) setDT(Matrice)
        # Adding y to Matrice and discarding data until 2020-04-04
        data.table::set(Matrice, j = "y", value = y)
        
        #----------------------------------------C
        # Filtered y, T and RH: 1st using median average,  for RH and and T as well
        #----------------------------------------C
        #Window.size <- 61
        #Matrice[, (paste0(name.Humidity, "_MA")) := caTools::runquantile(Matrice[[name.Humidity]], k = Window.size, probs= 0.5, align = "center", endrule = "quantile")] # faster
        #Window.size <- 61
        #Matrice[, (paste0(name.Temperature, "_MA")) := caTools::runquantile(Matrice[[name.Temperature]], k = Window.size, probs= 0.5, align = "center", endrule = "quantile")] # faster
        #Window.size <- 61
        #Matrice[, y_MA := caTools::runmean(y, k = Window.size, align = "center", endrule = "mean")] # faster
        # Using baseline package for smoothing, the dates shall be completed, no missing dates
        # All.Dates <- seq(from = min(Matrice$date), to = max(Matrice$date), by = "mins")
        All.Dates <- seq(from = first(Matrice$date), to = last(Matrice$date), by = "mins")
        Missing.Dates <- data.table(date = as.POSIXct(setdiff(All.Dates, Matrice$date), origin = "1970-01-01")) #All.Dates[!All.Dates %in% Matrice$date]
        Matrice <- rbindlist(list(Matrice,Missing.Dates), use.names = T, fill = T)
        setkey(Matrice,"date")
        # Filtered variables
        # bc.medianWindow <- baseline::baseline(as.matrix(t(Matrice[,.SD, .SDcols =  c(name.Humidity, name.Temperature, "y")])), hwm=12, method = "medianWindow")
        # bc.medianWindow <- baseline::baseline(as.matrix(t(Matrice[,.SD, .SDcols =  c("y")])), left=300, right=300, lwin=50, rwin=50
        #                                       , method = "peakDetection")
        # gfilt <- gsignal::sgolayfilt(as.matrix(Matrice[,.SD, .SDcols =  c(name.Humidity, name.Temperature, "y")]), p=3, n=5)
        # dates.pred <- c(as.Date('2020-04-16'), as.Date('2020-04-19'))
        # Matrice <- Matrice[date >= dates.pred[1] & date <= dates.pred[2]]
        Matrice <- Matrice[!is.na(Matrice$y)]
        # adjusting the negative nA values (if exists) for sensors other than NO2_B43F
        if (!name.sensor %in% c("NO2_B43F_P1") && min(Matrice$y) < -5) {
            off.set <- min(Matrice$y)
            data.table::set(Matrice, j = 'y', value = Matrice$y - off.set)
        }
        #smoothing for sensor response, 30 min
        FIR_smoothed_y    <- FIR_y_T_RH(General=Matrice, "y",  Verbose = FALSE, fs = 6, n = 30) # fs = 8, n = 45
        #smoothing for sensor RH and T)
        FIR_smoothed_RH    <- FIR_y_T_RH(General=Matrice, name.Humidity, Verbose = FALSE, fs = 15, n = 150) #fs = 9, n = 90
        FIR_smoothed_T     <- FIR_y_T_RH(General=Matrice, name.Temperature, Verbose = FALSE, fs = 9, n = 90)
        # Adding RH, T and y filtered and 1st derivative of RH
        Matrice[, (paste0(name.Humidity, "_MA")) := FIR_smoothed_RH[[name.Humidity]]] # faster
        name.Humidity <- paste0(name.Humidity, "_MA")
        Matrice[, (paste0(name.Temperature, "_MA")) := FIR_smoothed_T[[name.Temperature]]] # faster
        name.Temperature <- paste0(name.Temperature, "_MA")
        # Discarding noise of y in y_MA
        Matrice[, y_MA := FIR_smoothed_y[["y"]]]
        # 1st derivative for humidity
        Matrice[, dRH := c(diff(Matrice[[name.Humidity]]), NA)] # BE CAREFUL WHEN THE RH sensor does not work or restart
        Matrice[, dT  := c(diff(Matrice[[name.Temperature]]), NA)] # BE CAREFUL WHEN THE T sensor does not work or restart
        # replacing NA of dRH and dT with the latest values
        if (any(is.na(Matrice$dRH))) {
            data.table::set(Matrice, i = which(is.na(Matrice$dRH)), j = 'dRH', value = Matrice$dRH[which(is.na(Matrice$dRH))-1])
        }
        # replacing NA of dT with the latest values
        if (any(is.na(Matrice$dT))) {
            data.table::set(Matrice, i = which(is.na(Matrice$dT)), j = 'dT', value = Matrice$dRH[which(is.na(Matrice$dT))-1])
        }
        # Discarding the missing dates
        if(length(Matrice[-which(date %in% Missing.Dates$date), which = T ]) > 0) Matrice <- Matrice[-which(date %in% Missing.Dates$date)]
        # selecting when RH is increasing or decreasing
        cutt_off <- 0.000
        T.Inc   <- Matrice[dT > cutt_off , which = T]
        T.Dec   <- Matrice[dT <= cutt_off , which = T]
        RH.Inc  <- Matrice[dRH > cutt_off, which = T]
        RH.Dec  <- Matrice[dRH <= cutt_off, which = T]
        
        RH.Inc  <- unique(c(T.Dec, RH.Inc))
        RH.Dec  <- unique(c(T.Inc, RH.Dec))
        
        # Color for plotting
        Matrice[RH.Inc, RH.Col := "blue"]
        Matrice[RH.Dec, RH.Col := "red"]
        # Adding the T_eff, RH_effect and sum
        rT  <-  1.10
        yfT = -8.42
        y0T = 6.8
        rRH_inc=0.970
        yfRH_inc = 25.6
        y0RH_inc=46.4
        rRH_dec = 1.033
        yfRH_dec = 51.5
        y0RH_dec = 46.2
        data.table::set(Matrice, i = RH.Dec,  j = "RH_eff", value = yfRH_dec + (y0RH_dec-yfRH_dec)  * (rRH_dec^(Matrice[RH.Dec][[name.Humidity]] - 40)))
        data.table::set(Matrice, i = RH.Inc,  j = "RH_eff", value = yfRH_inc + (y0RH_inc-yfRH_inc)  * (rRH_inc^(Matrice[RH.Inc][[name.Humidity]] - 40)))
        data.table::set(Matrice,   j = "T_eff", value = yfT + (y0T-yfT) * (rT^(Matrice[[name.Temperature]] - 9.15)))
        data.table::set(Matrice,   j = "Sum_eff", value = Matrice$RH_eff + Matrice$T_eff)
        # Ensuring the continuity of RH_eff
        ## copying RH_eff column since we use the original values later
        data.table::set(Matrice, j = 'RH_eff.tmp', value = Matrice$RH_eff)
        # ## Determining where RH_eff.tmp jumps
        RH.limits <- c(which(abs(diff(Matrice$RH_eff.tmp)) > 1)+1)
        RH.jumps <- data.table::data.table(Reg.Start = sapply(seq_along(RH.limits), function(k) {
            RH.limits[k]}))
        
        RH.jumps[, Reg.End := sapply(1:(nrow(RH.jumps)), function(k) {
            if(k != nrow(RH.jumps)) {
                RH.jumps$Reg.Start[k+1]-1
            } else nrow(Matrice)})]
        
        RH.jumps[, jump := sapply(1:(nrow(RH.jumps)), function(k) {
            Matrice$RH_eff.tmp[RH.jumps$Reg.Start[k]] - Matrice$RH_eff.tmp[RH.jumps$Reg.Start[k]-1]
        })]
        
        for (k in seq_along(RH.jumps$Reg.Start)) {
            val2change  <- Matrice$RH_eff.tmp[RH.jumps$Reg.Start[k]] - Matrice$RH_eff.tmp[RH.jumps$Reg.Start[k]-1]
            data.table::set(Matrice, i = RH.jumps$Reg.Start[k]:RH.jumps$Reg.End[k], j = 'RH_eff.tmp', value = Matrice$RH_eff.tmp[RH.jumps$Reg.Start[k]:RH.jumps$Reg.End[k]] - val2change)
        }
        data.table::set(Matrice,   j = "Sum_eff.tmp", value = Matrice$RH_eff.tmp + Matrice$T_eff)
        # checking and replacing any NA of Sum_eff.tmp with the previous one
        if (any(is.na(Matrice$Sum_eff.tmp))) {
            i.wh.na <- which(is.na(Matrice$Sum_eff.tmp))
            for (i in i.wh.na) {
                if (i == 1) val <- Matrice$Sum_eff.tmp[i+1] else val <- Matrice$Sum_eff.tmp[i-1]
                data.table::set(Matrice, i = i, j = 'Sum_eff.tmp', value = val)
                rm(val)
            }
        }
        # if (hour(Matrice$date[nrow(Matrice)]) == 0) Matrice <- Matrice[-nrow(Matrice)]
        rm(cutt_off)
        # index end of day
        Matrice[, i.date.max := .I[which.max(date)], by = as.Date(date)]
        # when the last row of Matrice is the midnight, it creates a problem. we add this to the last day
        if (hour(Matrice$date[nrow(Matrice)]) == 0) data.table::set(Matrice, i = nrow(Matrice), j = "i.date.max", value = Matrice$i.date.max[nrow(Matrice)-1])
        # Matrice[, i.date.max := .I[which.max(date)], by = sapply(date, function (i) { if (i==date[length(date)] && hour(Matrice$date[nrow(Matrice)]) == 0) return(as.Date(i-1)) else return(as.Date(i))})]
        Matrice[, i.date.min := .I[which.min(date)], by = as.Date(date)]
        # RH.Inc: Finding row of daily max temp and min humidity (max of y_MA)
        ############################# What is RH.INC? ist not in Matrice! ################################################################################################################
        Matrice[intersect(RH.Inc, Matrice[hour(date) > 9, which = T]), RH.min     := min(.SD, na.rm = T),  .SDcols = name.Humidity, by = as.Date(date)] #RH.Inc[hour(date) > 11]
        Matrice[intersect(RH.Inc, Matrice[hour(date) > 9, which = T]), i.RH.min   := .I[min(which(get(name.Humidity) == min(get(name.Humidity))))], by = as.Date(date)]
        # Matrice[RH.Inc[hour(date) > 11], i.RH.min   := .I[which.min(get(name.Humidity))], by = as.Date(date)]
        # Matrice[RH.Inc[hour(date) > 11], i.RH.min   := .I[which.min(Out.Relative_humidity_MA)], by = as.Date(date)]       #RH.Inc[hour(date) > 11]
        Matrice[intersect(RH.Inc, Matrice[hour(date) > 9, which = T]), T.max      := max(.SD, na.rm = T),  .SDcols = name.Temperature, by = as.Date(date)]
        Matrice[intersect(RH.Inc, Matrice[hour(date) > 9, which = T]), i.T.max    := .I[min(which(get(name.Temperature) == max(get(name.Temperature))))], by = as.Date(date)]
        Matrice[intersect(RH.Inc, Matrice[hour(date) > 9, which = T]), Eff.max := max(.SD, na.rm = T),  .SDcols = "Sum_eff.tmp", by = as.Date(date)] 
        Matrice[intersect(RH.Inc, Matrice[hour(date) > 9, which = T]), i.Eff.max := .I[which.max(Sum_eff.tmp)], by = as.Date(date)]
        Cal.RH.Inc <- Matrice[intersect(RH.Inc, Matrice[hour(date) > 9, which = T]), c("date", "i.date.min","i.date.max","RH.min", "i.RH.min", "T.max", "i.T.max", 
                                                                                       "Eff.max", "i.Eff.max")] #, "RH.max",  "i.RH.max", "T.min", "i.T.min")]
        Cal.RH.Inc[, date := as.Date(date)]
        Cal.RH.Inc[, Count := .N, by = date]
        Cal.RH.Inc <- unique(Cal.RH.Inc)
        Cal.RH.Inc <- Cal.RH.Inc[order(Cal.RH.Inc$date, decreasing = F)]
        Cal.RH.Inc <- na.omit(Cal.RH.Inc)
        # RH.Dec: Finding row of daily max humidity, min temp and min of y_MA
        Matrice[intersect(RH.Dec, Matrice[hour(date) < 13, which = T]), RH.max   := max(.SD, na.rm = T),  .SDcols = name.Humidity, by = as.Date(date)]
        Matrice[intersect(RH.Dec, Matrice[hour(date) < 13, which = T]), i.RH.max := .I[min(which(get(name.Humidity) == max(get(name.Humidity))))], by = as.Date(date)]
        # Matrice[intersect(RH.Dec, Matrice[hour(date) < 13, which = T]), i.RH.max := .I[which.max(get(name.Humidity))], by = as.Date(date)]
        Matrice[intersect(RH.Dec, Matrice[hour(date) < 13, which = T]), T.min    := min(.SD, na.rm = T),  .SDcols = name.Temperature, by = as.Date(date)]
        Matrice[intersect(RH.Dec, Matrice[hour(date) < 13, which = T]), i.T.min  := .I[min(which(get(name.Temperature) == min(get(name.Temperature))))], by = as.Date(date)]
        
        # # RH.Dec: Finding row of daily min humidity, max temp and min of y_MA 
        Matrice[intersect(RH.Dec, Matrice[hour(date) < 13, which = T]), RH.min := min(.SD, na.rm = T),  .SDcols = name.Humidity, by = as.Date(date)]
        Matrice[intersect(RH.Dec, Matrice[hour(date) < 13, which = T]), i.RH.min   := .I[min(which(get(name.Humidity) == min(get(name.Humidity))))], by = as.Date(date)]
        Matrice[intersect(RH.Dec, Matrice[hour(date) < 13, which = T]), T.max := max(.SD, na.rm = T),  .SDcols = name.Temperature, by = as.Date(date)]
        Matrice[intersect(RH.Dec, Matrice[hour(date) < 13, which = T]), i.T.max    := .I[min(which(get(name.Temperature) == max(get(name.Temperature))))], by = as.Date(date)]
        Matrice[intersect(RH.Dec, Matrice[hour(date) < 13, which = T]), Eff.min := min(.SD, na.rm = T),  .SDcols = "Sum_eff.tmp", by = as.Date(date)]
        Matrice[intersect(RH.Dec, Matrice[hour(date) < 13, which = T]), i.Eff.min := .I[which.min(Sum_eff.tmp)], by = as.Date(date)]
        # Matrice[, Sum_eff.tmp := NULL]
        Matrice[, RH_eff.tmp := NULL]
        # RH.Dec: Finding row of daily max humidity and max temp
        Cal.RH.Dec <- Matrice[intersect(RH.Dec, Matrice[hour(date) < 13, which = T]), c("date", "i.date.min","i.date.max", "RH.max", "i.RH.max", "T.min", "i.T.min", "T.max", "i.T.max", "RH.min", "i.RH.min",
                                                                                        "Eff.min", "i.Eff.min")] # , "T.max", "i.T.max" #RH.min", "i.RH.min","yMA.min", "i.yMA.min",
        Cal.RH.Dec[, date := as.Date(date)]
        Cal.RH.Dec[, i.Tmin_RHmax := i.T.min - i.RH.max]
        Cal.RH.Dec[, Count := .N, by = date]
        Cal.RH.Dec <- unique(Cal.RH.Dec)
        Cal.RH.Dec <- Cal.RH.Dec[order(Cal.RH.Dec$date, decreasing = F)]
        Cal.RH.Dec <- na.omit(Cal.RH.Dec)
        # Adding columns
        Cal.RH.Dec[, Meas_Function := NA_character_]
        Cal.RH.Dec[, Var           := NA_character_]
        Cal.RH.Inc[, Meas_Function := NA_character_]
        Cal.RH.Inc[, Var           := NA_character_]
        # # Dividing ViewPort to plot
        # #https://stackoverflow.com/questions/14124373/combine-base-and-ggplot-graphics-in-r-figure-window
        # vp.TopRight <- viewport(height=unit(.5, "npc"), width=unit(0.5, "npc"), just=c("right","top"), y=1, x=1)
        # vp.TopLeft <- viewport(height=unit(.5, "npc"), width=unit(0.5, "npc"), just=c("right","top"), y=1, x=0.5)
        # vp.BottomLeft <- viewport(height=unit(.5, "npc"), width=unit(0.5, "npc"), just=c("right","top"), y=0.5, x=0.5)
        # vp.BottomRight <- viewport(height=unit(.5, "npc"), width=unit(0.5, "npc"), just=c("right","top"), y=0.5, x=1)
        # Daily drift of T and RH
        # for (Date in seq(as.Date("2021-03-01"), as.Date("2021-03-02"),1)) { # seq(as.Date("2020-04-15"), as.Date("2021-03-05"),1), c(as.Date("2021-02-28"), as.Date("2021-03-01"))  sort(unique(c(Cal.RH.Inc$date, Cal.RH.Dec$date)))
        dates    <- sort(unique(c(as.Date(Cal.RH.Inc$date), as.Date(Cal.RH.Dec$date))))
        max.date <- max(dates)
        # we need to remove the max date where available
        if (max.date %in% Cal.RH.Dec$date) Cal.RH.Dec <-  Cal.RH.Dec[date != max.date]
        if (max.date %in% Cal.RH.Inc$date) Cal.RH.Inc <-  Cal.RH.Inc[date != max.date]
        # removing the max date from dates
        dates    <- dates[-which(dates == max.date)]
        if (length(dates) < 1) stop(futile.logger::flog.error(paste0("[Meas_Function] No data for ", name.sensor, " / ",  unlist(strsplit(name.Model, split="/"))[which(grepl("ASE", unlist(strsplit(name.Model, split="/"))))+1], 
                                                                     " in requested Predict_Interval")))
        for (Date in seq(dates[1], dates[length(dates)], 1)) {
            cat(paste0(as.Date(Date), "\n"))
            # Checking that there are data on that day
            if(nrow(Matrice[as.Date(date) == as.Date(Date)]) == 0) next
            
            # Daily data
            Daily.data <- Matrice[as.Date(Matrice$date) == as.Date(Date), which = T]
            if (as.Date(Date) %in% Cal.RH.Inc$date){
                Row.RH.Inc  <- Cal.RH.Inc[date == as.Date(Date), which = T]
                Do.RH.Inc <- TRUE
            } else Do.RH.Inc <- FALSE
            if (as.Date(Date) %in% Cal.RH.Dec$date){
                Do.RH.Dec <- TRUE
                if (as.Date(Date) < Cal.RH.Dec[1]$date) Row.RH.Dec <- 0 else Row.RH.Dec <- Cal.RH.Dec[date == as.Date(Date), which=T]
            } else Do.RH.Dec <- FALSE
            # Cal.RH is the model to check the continuity. Creating for the first time
            if (!exists('Cal.RH')) Cal.RH <- NULL
            # The last date of predicted data to check the time difference with the next date. If >60 min, continuity will be not checked
            if (!exists('Last.Pred.Date')) Last.Pred.Date <- NULL
            # if (!exists('min.Prev.y_MA'))  min.Prev.y_MA  <- NULL
            # Data RH.Inc
            if(Do.RH.Inc){ # normally from midday until morning of the day after
                # starting row
                if(length(Row.RH.Inc) == 1 && Row.RH.Inc != 0){
                    if(abs(Cal.RH.Inc[Row.RH.Inc]$i.RH.min - Cal.RH.Inc[Row.RH.Inc]$i.T.max) < 60){
                        # Row.Min.RH    <- round(mean(c(Cal.RH.Inc[Row.RH.Inc]$i.RH.min, Cal.RH.Inc[Row.RH.Inc]$i.T.max)),0)
                        # Row.Min.RH    <- Cal.RH.Inc[Row.RH.Inc]$i.RH.min
                        # Row.Min.RH    <- unique(min(Cal.RH.Inc[Row.RH.Inc]$i.RH.min, Cal.RH.Inc[Row.RH.Inc]$i.T.max, Cal.RH.Inc[Row.RH.Inc]$i.yMA.max))
                        # Row.Min.RH    <- Cal.RH.Inc[Row.RH.Inc]$i.T.max
                        Row.Min.RH    <- Cal.RH.Inc[Row.RH.Inc]$i.Eff.max
                    } else if (hour(Matrice[Cal.RH.Inc[Row.RH.Inc]$i.RH.min]$date) > 11 && hour(Matrice[Cal.RH.Inc[Row.RH.Inc]$i.RH.min]$date) <= 16){
                        # we check only where the min RH in the next day is
                        Row.Min.RH    <- Cal.RH.Inc[Row.RH.Inc]$i.Eff.max
                        # Row.Min.RH <- Cal.RH.Inc[Row.RH.Inc]$i.T.max   
                        # Row.Min.RH <- Cal.RH.Inc[Row.RH.Inc]$i.RH.min # Cal.RH.Inc[Row.RH.Inc]$i.T.max
                        # Row.Min.RH    <- unique(min(Cal.RH.Inc[Row.RH.Inc]$i.RH.min, Cal.RH.Inc[Row.RH.Inc]$i.T.max, Cal.RH.Inc[Row.RH.Inc]$i.yMA.max))
                        # } else  Row.Min.RH <- Cal.RH.Inc[Row.RH.Inc]$i.RH.min  # Row.Min.RH  <- unique(min(Cal.RH.Inc[Row.RH.Inc]$i.RH.min, Cal.RH.Inc[Row.RH.Inc]$i.T.max, Cal.RH.Inc[Row.RH.Inc]$i.yMA.max)) #
                    } else  Row.Min.RH    <- Cal.RH.Inc[Row.RH.Inc]$i.Eff.max
                    # Checking if there is higher sum effect after 60 min of Row.Min.RH
                    # if (as.Date(Date)==as.Date("2020-10-21")) browser()
                    # if (max(Matrice$Sum_eff.tmp[Row.Min.RH:(Row.Min.RH + 120)]) > Matrice$Sum_eff.tmp[Row.Min.RH]) {
                    #     Row.Min.RH <- max(Matrice[Sum_eff.tmp == max(Matrice$Sum_eff.tmp[Row.Min.RH:(Row.Min.RH+120)]), which = T])
                    # }
                } else {
                    # No data on that day. Take data from next day, 1st date hoping in the night, before 10:00
                    if ((as.Date(Date) + 1) %in% Cal.RH.Inc$date && hour(Matrice[Cal.RH.Inc[date == as.Date(Date)+1]$i.date.min]$date) < 10){
                        Row.Min.RH <- Cal.RH.Inc[date == as.Date(Date)+1]$i.date.min} else next}
                # Last row, preferably on next day in the morning when T is minimum, saving to use for RH.Dec
                if (Do.RH.Dec && length(Row.RH.Dec)!= 1 || as.Date(Date) == max(c(Cal.RH.Inc$date, Cal.RH.Dec$date), na.rm = T)){
                    # last row: if Date is the last day: take last available datetime in Cal.RH.Inc of that row
                    # idem if there is not that date in Cal.RH.Dec, it means the last date in the max date in Cal.RH.Inc
                    Row.Max.RH.next <- Cal.RH.Inc[Row.RH.Inc]$i.date.max
                    # save Row.Max.RH.next to use for RH.Dec
                    data.table::set(Cal.RH.Inc, i =Row.RH.Inc, j = "Row.Max.RH.next", value = Row.Max.RH.next)
                } else if(max(Cal.RH.Dec$date) != as.Date(Date) && # to check if the next date exists in Cal.RH.Dec
                          as.Date(Matrice[Cal.RH.Dec[date > as.Date(Date)][1]$i.Eff.min]$date) == as.Date(Date) + 1){
                    # as.Date(Matrice[Cal.RH.Dec[date > as.Date(Date)][1]$i.RH.max]$date) == as.Date(Date) + 1){
                    # } else if(as.Date(Matrice[Cal.RH.Dec[Row.RH.Dec+1]$i.T.min]$date) == as.Date(Date) + 1){ # 
                    # } else if(Do.RH.Dec && as.Date(Matrice[Cal.RH.Dec[Row.RH.Dec+1]$i.RH.max]$date) == as.Date(Date) + 1){ # as.Date(Matrice[Cal.RH.Dec[Row.RH.Dec+1]$i.T.min]$date) == as.Date(Date) + 1
                    # The next day is in Cal.RH.Dec. 
                    # if (hour(Matrice[Cal.RH.Dec[Row.RH.Dec+1]$i.Eff.min]$date) < 11){ 
                    # if (hour(Matrice[Cal.RH.Dec[Row.RH.Dec+1]$i.T.min]$date) < 11){
                    if (hour(Matrice[Cal.RH.Dec[date > as.Date(Date)][1]$i.Eff.min]$date) <= 12){ # hour(Matrice[Cal.RH.Dec[Row.RH.Dec+1]$i.T.min]$date) < 10
                        # if (hour(Matrice[Cal.RH.Dec[date > as.Date(Date)][1]$i.RH.max]$date) <= 12){ # hour(Matrice[Cal.RH.Dec[Row.RH.Dec+1]$i.T.min]$date) < 10
                        # the min T is before 10:00. It can be used. NOW WE CHECK WHERE THE MAX RH IS
                        # Row.Max.RH.next <- Cal.RH.Dec[Row.RH.Dec+1]$i.Eff.min
                        Row.Max.RH.next <- Cal.RH.Dec[date > as.Date(Date)][1]$i.Eff.min
                        # Row.Max.RH.next <- Cal.RH.Dec[Row.RH.Dec+1]$i.T.min
                        # Checking if the RH.max at least 1 hour before T.min. if so, we use T.min
                        if (Cal.RH.Dec[date > as.Date(Date)][1]$i.Tmin_RHmax < -60 ||
                            Cal.RH.Dec[date > as.Date(Date)][1]$i.Tmin_RHmax > 60) {
                            # if (Cal.RH.Dec[Row.RH.Dec+1]$i.Tmin_RHmax > 60) {
                            # Row.Max.RH.next <- Cal.RH.Dec[date > as.Date(Date)][1]$i.RH.max
                            Row.Max.RH.next <- Cal.RH.Dec[date > as.Date(Date)][1]$i.Eff.min
                            # Row.Max.RH.next <- unique(max(Cal.RH.Dec[date > as.Date(Date)][1]$i.RH.max, Cal.RH.Dec[date > as.Date(Date)][1]$i.T.min,
                            #                               Cal.RH.Dec[date > as.Date(Date)][1]$i.yMA.min))
                            # Row.Max.RH.next <- Cal.RH.Dec[Row.RH.Dec+1]$i.T.min
                        } else {
                            # Row.Max.RH.next <- Cal.RH.Dec[date > as.Date(Date)][1]$i.RH.max
                            Row.Max.RH.next <- Cal.RH.Dec[date > as.Date(Date)][1]$i.Eff.min
                            # Row.Max.RH.next <- unique(max(Cal.RH.Dec[date > as.Date(Date)][1]$i.RH.max, Cal.RH.Dec[date > as.Date(Date)][1]$i.T.min,
                            #                               Cal.RH.Dec[date > as.Date(Date)][1]$i.yMA.min))
                        }# Cal.RH.Dec[Row.RH.Dec+1]$i.T.min
                        # checking if there are lower name.Y values before
                        # Row.Max.RH.next <- which(Matrice$date==Matrice[Row.Min.RH:Row.Max.RH.next][order(y_MA)]$date[1])
                        if (Row.Min.RH == Row.Max.RH.next){
                            Row.Max.RH.next <- Cal.RH.Dec[date > as.Date(Date)][1]$i.Eff.min
                            Exp.T.Dec <- list(Model = "Previous Model")
                        } 
                    } else {
                        # the min T is after 11:00. It cannot be used. Use the index of max RH instead. Try to find a better solution
                        # the max RH after 12.00 pm. we use minimum T of this day
                        Row.Max.RH.next <- Cal.RH.Dec[date > as.Date(Date)][1]$i.Eff.min
                    } # Cal.RH.Dec[Row.RH.Dec+1]$i.Eff.min} #
                    # save Row.Max.RH.next to use for RH.Dec
                    # checking if there is a lower y_MA than the one at the Row.Max.RH.next within 3 hours. if so we select the index of minyMA
                    if (Matrice$y_MA[Row.Max.RH.next] > min(Matrice$y_MA[(Row.Max.RH.next-180):(Row.Max.RH.next + 180)]) &&
                        Row.Min.RH != max(Matrice[y_MA == min(Matrice$y_MA[(Row.Max.RH.next-180):(Row.Max.RH.next+180)]), which = T])) {
                        Row.Max.RH.next <- max(Matrice[y_MA == min(Matrice$y_MA[(Row.Max.RH.next-180):(Row.Max.RH.next+180)]), which = T])
                    }
                } else {
                    # The next day is not in Cal.RH.Dec. 
                    Row.Max.RH.next <- Cal.RH.Inc[Row.RH.Inc]$i.date.max
                    if (Row.Min.RH == Row.Max.RH.next) {
                        Row.Min.RH <- Row.Max.RH.next - Cal.RH.Inc$Count[Row.RH.Inc] + 1
                    }
                }
                data.table::set(Cal.RH.Inc, i =Row.RH.Inc, j = "Row.Max.RH.next", value = Row.Max.RH.next)
                # Selecting data from the highest temperature/lowest humidity until the lowest temperature/highest humidity on the next day (don't filter for increasing humdity)
                Rows.cal.Inc    <- Row.Min.RH:Row.Max.RH.next #intersect(get("RH.Inc"),Row.Min.RH:Row.Max.RH.next)
                if (length(Rows.cal.Inc) < 2) {
                    Do.RH.Inc <- FALSE  
                } else Daily.RH.Inc    <- Matrice[Rows.cal.Inc]
            }
            # RH.Dec
            if(Do.RH.Dec) {
                if(length(Row.RH.Dec) == 1 && Row.RH.Dec !=0) {
                    # browser()
                    # starting row, checking if the day before exist in Cal.RH.Inc
                    # Selecting data from the lowest temperature/highest humidity until the highest temperature/lowest humidity on the day before (don't filter for decreasing humidity)
                    Day.Before <- as.Date(Date)-1
                    if (any(grepl(Day.Before, Cal.RH.Inc$date)) && shiny::isTruthy(Cal.RH.Inc[date == Day.Before]$Row.Max.RH.next)){
                        Row.Max.RH <- unique(Cal.RH.Inc[as.Date(date) == Day.Before]$Row.Max.RH.next)
                    } else if (as.Date(Date) == min(c(Cal.RH.Inc$date, Cal.RH.Dec$date), na.rm = T)){
                        if (exists("Row.RH.Inc")) Row.Max.RH <- min(c(Cal.RH.Inc[Row.RH.Inc]$i.date.min, Cal.RH.Dec[Row.RH.Dec]$i.date.min), na.rm = T) else Row.Max.RH  <- Cal.RH.Dec[Row.RH.Dec]$i.RH.max
                    } else if(abs(Cal.RH.Dec[Row.RH.Dec]$i.RH.max - Cal.RH.Dec[Row.RH.Dec]$i.T.min) < 50){
                        Row.Max.RH     <- Cal.RH.Dec[Row.RH.Dec]$i.RH.max #Cal.RH.Dec[Row.RH.Dec]$i.Eff.min
                        # Row.Max.RH     <- round(mean(c(Cal.RH.Dec[Row.RH.Dec]$i.RH.max, Cal.RH.Dec[Row.RH.Dec]$i.T.min)),0)
                    } else if (hour(Matrice[Cal.RH.Dec[Row.RH.Dec]$i.RH.max]$date) < 12){
                        Row.Max.RH <-Cal.RH.Dec[Row.RH.Dec]$i.Eff.min # Cal.RH.Dec[Row.RH.Dec]$i.RH.max
                    } else Row.Max.RH <- Cal.RH.Dec[Row.RH.Dec]$i.Eff.min #Cal.RH.Dec[Row.RH.Dec]$i.RH.max 
                    
                    if (!exists("Row.Min.RH"))  Row.Min.RH     <- round(mean(c(Cal.RH.Dec[Row.RH.Dec]$i.RH.min,Cal.RH.Dec[Row.RH.Dec]$i.T.max)),0) # round(mean(c(Cal.RH.Dec[Row.RH.Dec]$i.RH.min,Cal.RH.Dec[Row.RH.Dec]$i.T.max)),0)
                    if(is.na(Row.Max.RH) || is.na(Row.Min.RH)) browser()
                    Rows.cal.Dec <- Row.Max.RH:Row.Min.RH
                    Daily.RH.Dec <- Matrice[Rows.cal.Dec]
                } else {
                    Rows.cal.Dec <- NULL}
                if (length(Rows.cal.Dec) > 1) {
                    # Estimating baseline using the linear model between Sum_effect and y_MA
                    Daily.Matrice.Dec  <-  Daily.RH.Dec[, .SD, .SDcols = c("date","y_MA",  'RH_eff', 'T_eff', 
                                                                           name.Temperature, name.Humidity, "dRH", 'dT', "y", "Sum_eff")]
                    if (exists("Regions.Dec")) rm(Regions.Dec) 
                    if (!exists("Dec.Last.nls.model")) Dec.Last.nls.model <- NULL 
                    if (!exists("Inc.Last.nls.model")) Inc.Last.nls.model <- NULL
                    if (!exists("Last.Bline")) Last.Bline <- NULL
                    Regions.Dec <- Find.Region(Daily.Matrice = Daily.Matrice.Dec, name.RH = name.Humidity, Last.RH_eff = NULL, thrs.bline = 5,
                                               Dec.Last.nls.model = Dec.Last.nls.model, Inc.Last.nls.model = Inc.Last.nls.model, thrs.DeltaRH = 5,
                                               Last.Bline = Last.Bline)
                    var4outlier.dec <- Regions.Dec$Regions.Dec$var
                    Last.RH_eff.Dec <- Regions.Dec$Daily.Matrice$RH_eff[nrow(Regions.Dec$Daily.Matrice)]
                    if (exists("Peaks.dec")) rm(Peaks.dec)
                    Peaks.dec <- Find.Peaks2(Daily.Matrice = Regions.Dec$Daily.Matrice , name.y = "y_MA", name.T = name.Temperature, name.RH = name.Humidity)
                    if (is.null(Regions.Dec$Regions)) {
                        data.table::set(Cal.RH.Dec, i = Row.RH.Dec, j = "Meas_Function",   value = 'insuff.region data')
                        next
                    }
                    if (nrow(Regions.Dec$Regions) > 1) {
                        date2copy  <- Cal.RH.Dec[date == Date]
                        for (i in 2:nrow(Regions.Dec$Regions)) {
                            Cal.RH.Dec <- data.table::rbindlist(list(Cal.RH.Dec, date2copy), use.names = T)[order(date, decreasing = F)]}}
                    for (row.reg in seq_along(Regions.Dec$Regions$Reg.Start)) {
                        i.which <- Regions.Dec$Regions$Reg.Start[row.reg]:Regions.Dec$Regions$Reg.End[row.reg]
                        data.table::set(Cal.RH.Dec, i = as.integer(Row.RH.Dec + row.reg - 1), j = "Reg.str",   value = Daily.RH.Dec[i.which[1]]$date)
                        data.table::set(Cal.RH.Dec, i = as.integer(Row.RH.Dec + row.reg - 1), j = "Reg.end",   value = Daily.RH.Dec[i.which[length(i.which)]]$date)
                        RH.sign <- Regions.Dec$Regions$RH.chg[row.reg]
                        var4Out.dec <- Regions.Dec$Regions$var4Out[row.reg]
                        LMmodel <- Set_nls_Model2(Daily.Matrice = Regions.Dec$Daily.Matrice, i.which = i.which, i.nls = NULL, Last.Bline = Last.Bline,
                                                  RH.sign = RH.sign, Dec.Last.nls.model = Dec.Last.nls.model, thrs.bline = 0, name.T = name.Temperature, 
                                                  name.RH = name.Humidity, Inc.Last.nls.model = Inc.Last.nls.model, row.reg = row.reg, Regions = Regions.Dec$Regions,
                                                  var = var4Out.dec, thrs.neg = -5)  
                        Dec.Last.nls.model <- LMmodel$Dec.Last.nls.model
                        Inc.Last.nls.model <- LMmodel$Inc.Last.nls.model
                        # Filling the table
                        if (LMmodel$Meas_Funct != "no prev.bline") {
                            if (grepl("bline.cont", LMmodel$Meas_Funct)) {
                                data.table::set(Cal.RH.Dec, i = as.integer(Row.RH.Dec + row.reg - 1), j = "yf_RH", value = ifelse(RH.sign == 'incr', yfRH_inc, yfRH_dec))
                                data.table::set(Cal.RH.Dec, i = as.integer(Row.RH.Dec + row.reg - 1), j = "y0_RH",  value = ifelse(RH.sign == 'incr', y0RH_inc, y0RH_dec))
                                data.table::set(Cal.RH.Dec, i = as.integer(Row.RH.Dec + row.reg - 1), j = "r_RH", value = ifelse(RH.sign == 'incr', rRH_inc, rRH_dec))
                                data.table::set(Cal.RH.Dec, i = as.integer(Row.RH.Dec + row.reg - 1), j = "yf_T", value = yfT)
                                data.table::set(Cal.RH.Dec, i = as.integer(Row.RH.Dec + row.reg - 1), j = "y0_T",  value = y0T)
                                data.table::set(Cal.RH.Dec, i = as.integer(Row.RH.Dec + row.reg - 1), j = "r_T", value = rT)
                            } else if (!is.null(LMmodel$nls.model)) {
                                if (grepl("Poly", LMmodel$Meas_Funct)) {
                                    data.table::set(Cal.RH.Dec, i = as.integer(Row.RH.Dec + row.reg - 1), j = "a0", value = round(broom::tidy(LMmodel$nls.model)[[2]][1], 4))
                                    data.table::set(Cal.RH.Dec, i = as.integer(Row.RH.Dec + row.reg - 1), j = "a1",  value = round(broom::tidy(LMmodel$nls.model)[[2]][2], 4))
                                    data.table::set(Cal.RH.Dec, i = as.integer(Row.RH.Dec + row.reg - 1), j = "a2", value = round(broom::tidy(LMmodel$nls.model)[[2]][3], 4))
                                } else {
                                    data.table::set(Cal.RH.Dec, i = as.integer(Row.RH.Dec + row.reg - 1), j = "yf_RH",  value = round(coef(LMmodel$nls.model)[[1]], 1))
                                    data.table::set(Cal.RH.Dec, i = as.integer(Row.RH.Dec + row.reg - 1), j = "y0_RH",  value = round(coef(LMmodel$nls.model)[[2]], 1))
                                    data.table::set(Cal.RH.Dec, i = as.integer(Row.RH.Dec + row.reg - 1), j = "r_RH",   value = round(coef(LMmodel$nls.model)[[3]], 3))
                                    data.table::set(Cal.RH.Dec, i = as.integer(Row.RH.Dec + row.reg - 1), j = "yf_T",   value = round(coef(LMmodel$nls.model)[[4]], 1))
                                    data.table::set(Cal.RH.Dec, i = as.integer(Row.RH.Dec + row.reg - 1), j = "y0_T",   value = round(coef(LMmodel$nls.model)[[5]], 1))
                                    data.table::set(Cal.RH.Dec, i = as.integer(Row.RH.Dec + row.reg - 1), j = "r_T",    value = round(coef(LMmodel$nls.model)[[6]], 3))
                                }
                            }
                            # data.table::set(Cal.RH.Dec, i = as.integer(Row.RH.Dec + row.reg - 1), j = "Meas_Function",   value = LMmodel$Meas_Funct)
                            data.table::set(Cal.RH.Dec, i = as.integer(Row.RH.Dec + row.reg - 1), j = "rmse",    value = LMmodel$rmse)
                            data.table::set(Daily.RH.Dec, i = i.which, j = "Sum_eff", value = LMmodel$Daily.Matrice$Sum_eff[i.which])
                            Last.Bline <- LMmodel$pred.bline[length(LMmodel$pred.bline)]
                            # }
                        } else {
                            # data.table::set(Cal.RH.Dec, i = as.integer(Row.RH.Dec + row.reg - 1), j = "Meas_Function",   value = "no prev.bline")
                            Last.Bline <- NULL
                        }
                        data.table::set(Cal.RH.Dec, i = as.integer(Row.RH.Dec + row.reg - 1), j = "Meas_Function",   value = LMmodel$Meas_Funct)
                        data.table::set(Cal.RH.Dec, i = as.integer(Row.RH.Dec + row.reg - 1), j = "RH.sign", value = ifelse(RH.sign == 'decr', "RH_Dec", "RH_Inc"))
                        data.table::set(Cal.RH.Dec, i = as.integer(Row.RH.Dec + row.reg - 1), j = "var4Out", value = var4Out.dec)
                        data.table::set(Cal.RH.Dec, i = as.integer(Row.RH.Dec + row.reg - 1), j = "Last.bline", value = LMmodel$pred.bline[length(LMmodel$pred.bline)])
                        data.table::set(Matrice, i = Rows.cal.Dec[i.which], j = "nls_estimated",  value = LMmodel$pred.bline)
                        data.table::set(Daily.RH.Dec, i = i.which, j = "nls_estimated", value = LMmodel$pred.bline)
                        data.table::set(Daily.RH.Dec, i = i.which, j = "Inf.col", value = ifelse(LMmodel$Daily.Matrice$Outliers[i.which], "green", 'red'))
                        # adding the last nls-estimated date. It might be used later when there is no data to fit nls for the 1st region after 3 hours sensor stop
                        if (!exists('Value.last.date')) Value.last.date <- Daily.RH.Dec[i.which][!is.na(LMmodel$pred.bline)]$date[length(!is.na(LMmodel$pred.bline))]
                        data.table::set(Cal.RH.Dec, i = as.integer(Row.RH.Dec + row.reg - 1), j = "last.date", value = Value.last.date)
                        rm(Value.last.date)
                    }
                    # Checking if there is any previous region with missing predicted baseline due to insufficient data for nls fitting. 
                    # baseline of such region will be predicted either with baseline continuity of corresponding nls model set after this region (min rmse of either option)
                    if (any(na.omit(Cal.RH.Dec[date <= Date]$Meas_Function) == "no prev.bline") && na.omit(Cal.RH.Dec[Row.RH.Dec]$Meas_Function) != "no prev.bline") {
                        # Determining the date of which Meas Function is "no prev.bline"
                        no.prev.bline <- sort(which(Cal.RH.Dec[date <= Date]$Meas_Function == "no prev.bline"), decreasing = T)
                        # checking if there is any region with the same date and nls_estimated
                        for (Reg in no.prev.bline) {
                            # Checking if the 1st date BEFORE this region has a predicted baseline. it is within 2 h, we will calculate the baseline continuity
                            ## checking if there is any date before the start of this region
                            if (Matrice[date == Cal.RH.Dec[Reg]$Reg.str, which = T] > 1 && 
                                (difftime(Cal.RH.Dec[Reg]$Reg.str, max(Matrice[date < Cal.RH.Dec[Reg]$Reg.str & !is.na(Matrice$nls_estimated)]$date, na.rm = T), 
                                          units ='hours') < 2)) {
                                i.Last.nls.est <- Matrice[date == max(Matrice[date < Cal.RH.Dec[Reg]$Reg.str & !is.na(Matrice$nls_estimated)]$date, na.rm = T), which = T]
                            } else if (difftime(Cal.RH.Dec[Reg]$Reg.str, 
                                                min(Matrice[date > Cal.RH.Dec[Reg]$Reg.end & !is.na(Matrice$nls_estimated)]$date, na.rm = T), units ='hours') < 2) {
                                i.Last.nls.est <- Matrice[date == min(Matrice[date > Cal.RH.Dec[Reg]$Reg.end & !is.na(Matrice$nls_estimated)]$date, na.rm = T), which = T]
                            }
                            Last.nls.est  <- Matrice[i.Last.nls.est][['nls_estimated']]
                            Last.Sum_eff   <- Matrice[i.Last.nls.est][['Sum_eff']]
                            i.dates <- Matrice[date %between% c(Cal.RH.Dec[Reg]$Reg.str, Cal.RH.Dec[Reg]$Reg.end), which = T]
                            # prediction with the last corresponding nls.model, if not NULL
                            if (Cal.RH.Dec[Reg]$RH.sign == 'decr') {
                                if (!is.null(Dec.Last.nls.model)) {
                                    bline.lastModel <- predict(Dec.Last.nls.model, newdata =  Matrice[i.dates])
                                } else next
                            } else if (!is.null(Inc.Last.nls.model)) {
                                bline.lastModel <- predict(Inc.Last.nls.model, newdata =  Matrice[i.dates])
                            } else next
                            rmse.lastModel  <- sqrt(sum((bline.lastModel - Matrice$y_MA[i.dates])^2)/(length(i.dates)-2))
                            sum.eff.nor      <- Matrice[i.dates]$Sum_eff - (Matrice[date == Cal.RH.Dec[Reg]$Reg.str]$Sum_eff - Last.Sum_eff) 
                            # estimated baseline with continuity
                            cont.bline     <- sum.eff.nor - (Last.Sum_eff - Last.nls.est)
                            # rmse of continued baseline
                            rmse.contBaseline <- sqrt(sum((cont.bline - Matrice$y_MA[i.dates])^2)/(length(i.dates)-2))
                            if (rmse.contBaseline < rmse.lastModel) {
                                estimated.bline <- cont.bline
                                Meas_Function   <- "recalc.Bline Conti."
                            } else {
                                estimated.bline <- bline.lastModel
                                Meas_Function   <- "recalc.Bline Prev.Mod"
                            }
                            data.table::set(Cal.RH.Dec, i = Reg, j = "Meas_Function",   value = Meas_Function)
                            data.table::set(Matrice, i = i.dates, j = "nls_estimated",  value = estimated.bline)
                            data.table::set(Matrice, i = i.dates, j = "y.cor.T",  value = Matrice[i.dates][["y"]] - Matrice[i.dates][["nls_estimated"]])
                            data.table::set(Matrice, i = which(Matrice[i.dates, y.cor.T < -20]), j = "y.cor.T",  value = NA)
                            data.table::set(Matrice, i = i.dates, j = "x.fitted", value = Matrice[i.dates][["y.cor.T"]]/ 0.54)
                        }
                    }
                    # Adding min and max Sum_eff to the table
                    data.table::set(Cal.RH.Dec, i = Cal.RH.Dec[date == Date, which = T], j = "Var", value = 'nls_estimated')
                    data.table::set(Cal.RH.Dec, i = Cal.RH.Dec[date == Date, which = T], j = "max.Est", value = ifelse(all(is.na(Daily.RH.Dec$nls_estimated)), NA_real_,max(Daily.RH.Dec$nls_estimated, na.rm = T)))
                    data.table::set(Cal.RH.Dec, i = Cal.RH.Dec[date == Date, which = T], j = "min.Est", value = ifelse(all(is.na(Daily.RH.Dec$nls_estimated)), NA_real_,min(Daily.RH.Dec$nls_estimated, na.rm = T)))
                    
                    #     ################################################C
                    #     # Applying the Calibration model to Predict data
                    #     ################################################C
                    #     # "RH.Dec"
                    # checking number of identical date
                    # updating Sum effect in Matrice 
                    data.table::set(Matrice, i = Rows.cal.Dec, j = "Sum_eff",  value = Daily.RH.Dec$Sum_eff)
                    # Correcting sensor response y for Temperature with exponential effect of the day, before of
                    data.table::set(Matrice, i = Rows.cal.Dec, j = "y.cor.T",  value = Matrice[Rows.cal.Dec][["y"]] - Matrice[Rows.cal.Dec][["nls_estimated"]])
                    data.table::set(Matrice, i = Rows.cal.Dec[Matrice[Rows.cal.Dec, y.cor.T < -20]], j = "y.cor.T",  value = NA)
                    if (exists('LMmodel') && any(is.na(LMmodel$Daily.Matrice$y))) {
                        na.which <- which(Matrice$date %in% LMmodel$Daily.Matrice[which(is.na(LMmodel$Daily.Matrice$y))]$date)
                        data.table::set(Matrice, i = na.which, j = "y.cor.T",  value = NA)
                        rm('na.which')}
                    data.table::set(Matrice, i = Rows.cal.Dec, j = "x.fitted", value = Matrice[Rows.cal.Dec][["y.cor.T"]]/ 0.54) #"RH_Dec"
                    
                } else {
                    Do.RH.Dec <- FALSE
                    Rows.cal.Dec <- NULL
                }
            } 
            # RH.Inc for temperature or RH effect (AFTERNOON)
            #########################################################################################################################################################################################################################
            ############################################################################## SAVE THE CASE WITH NOT ENOUGH ROWS OF DATA ###############################################################################################
            #########################################################################################################################################################################################################################
            if (Do.RH.Inc) {
                # Estimating baseline using the linear model between Sum_effect and y_MA
                Daily.Matrice.Inc  <-  Daily.RH.Inc[, .SD, .SDcols = c("date","y_MA", 'RH_eff', 'T_eff',
                                                                       name.Temperature, name.Humidity, "dRH", 'dT', "y", "Sum_eff")]
                if (!exists('Last.RH_eff.Dec')) Last.RH_eff.Dec <- NULL
                if (exists("Regions.Inc")) rm(Regions.Inc)
                if (!exists("Dec.Last.nls.model")) Dec.Last.nls.model <- NULL 
                if (!exists("Inc.Last.nls.model")) Inc.Last.nls.model <- NULL
                if (!exists("Last.Bline")) Last.Bline <- NULL
                Regions.Inc <- Find.Region(Daily.Matrice = Daily.Matrice.Inc, name.RH = name.Humidity, Last.RH_eff = Last.RH_eff.Dec, thrs.bline = 5, 
                                           Dec.Last.nls.model = Dec.Last.nls.model, Inc.Last.nls.model = Inc.Last.nls.model, thrs.DeltaRH = 5, Last.Bline = Last.Bline)
                if(exists("Peaks.inc")) rm(Peaks.inc)
                if (is.null(Regions.Inc$Regions)) {
                    data.table::set(Cal.RH.Inc, i = Row.RH.Inc, j = "Meas_Function",   value = 'insuff.region data')
                    next
                }
                Peaks.inc <- Find.Peaks2(Daily.Matrice = Regions.Inc$Daily.Matrice , name.y = "y_MA", name.T = name.Temperature, name.RH = name.Humidity)
                if (nrow(Regions.Inc$Regions) > 1) {
                    date2copy  <- Cal.RH.Inc[date == Date]
                    for (i in 2:nrow(Regions.Inc$Regions)) {
                        Cal.RH.Inc <- data.table::rbindlist(list(Cal.RH.Inc, date2copy), use.names =T)[order(date, decreasing = F)]}}
                for (row.reg in seq_along(Regions.Inc$Regions$Reg.Start)) {
                    i.which <- Regions.Inc$Regions$Reg.Start[row.reg]:Regions.Inc$Regions$Reg.End[row.reg]
                    data.table::set(Cal.RH.Inc, i = as.integer(Row.RH.Inc + row.reg - 1), j = "Reg.str",   value = Daily.RH.Inc[i.which[1]]$date)
                    data.table::set(Cal.RH.Inc, i = as.integer(Row.RH.Inc + row.reg - 1), j = "Reg.end",   value = Daily.RH.Inc[i.which[length(i.which)]]$date)
                    if (!exists("Inc.Last.nls.model")) Inc.Last.nls.model <- NULL 
                    if (!exists("Dec.Last.nls.model")) Dec.Last.nls.model <- NULL
                    RH.sign <- Regions.Inc$Regions$RH.chg[row.reg]
                    var4Out.inc <- Regions.Inc$Regions$var4Out[row.reg]
                    LMmodel <- Set_nls_Model2(Daily.Matrice = Peaks.inc$Daily.Matrice, i.which = i.which, i.nls = NULL, Last.Bline = Last.Bline,
                                              RH.sign = RH.sign, Inc.Last.nls.model = Inc.Last.nls.model, thrs.bline = 0, name.T = name.Temperature, 
                                              name.RH = name.Humidity, Dec.Last.nls.model = Dec.Last.nls.model, row.reg = row.reg, Regions = Regions.Inc$Regions,
                                              var = var4Out.inc, thrs.neg = -5) 
                    Dec.Last.nls.model <- LMmodel$Dec.Last.nls.model
                    Inc.Last.nls.model <- LMmodel$Inc.Last.nls.model
                    if (LMmodel$Meas_Funct != "no prev.bline") {
                        # filling the data table
                        if (grepl("bline.cont", LMmodel$Meas_Funct)) {
                            data.table::set(Cal.RH.Inc, i = as.integer(Row.RH.Inc + row.reg - 1), j = "yf_RH", value = ifelse(RH.sign == 'incr', yfRH_inc, yfRH_dec))
                            data.table::set(Cal.RH.Inc, i = as.integer(Row.RH.Inc + row.reg - 1), j = "y0_RH",  value = ifelse(RH.sign == 'incr', y0RH_inc, y0RH_dec))
                            data.table::set(Cal.RH.Inc, i = as.integer(Row.RH.Inc + row.reg - 1), j = "r_RH", value = ifelse(RH.sign == 'incr', rRH_inc, rRH_dec))
                            data.table::set(Cal.RH.Inc, i = as.integer(Row.RH.Inc + row.reg - 1), j = "yf_T", value = yfT)
                            data.table::set(Cal.RH.Inc, i = as.integer(Row.RH.Inc + row.reg - 1), j = "y0_T",  value = y0T)
                            data.table::set(Cal.RH.Inc, i = as.integer(Row.RH.Inc + row.reg - 1), j = "r_T", value = rT)
                        } else if (!is.null(LMmodel$nls.model)) {
                            if (grepl("Poly", LMmodel$Meas_Funct)) {
                                data.table::set(Cal.RH.Inc, i = as.integer(Row.RH.Inc + row.reg - 1), j = "a0", value = round(broom::tidy(LMmodel$nls.model)[[2]][1], 4))
                                data.table::set(Cal.RH.Inc, i = as.integer(Row.RH.Inc + row.reg - 1), j = "a1",  value = round(broom::tidy(LMmodel$nls.model)[[2]][2], 4))
                                data.table::set(Cal.RH.Inc, i = as.integer(Row.RH.Inc + row.reg - 1), j = "a2", value = round(broom::tidy(LMmodel$nls.model)[[2]][3], 4))
                            } else {
                                data.table::set(Cal.RH.Inc, i = as.integer(Row.RH.Inc + row.reg - 1), j = "yf_RH",  value = round(coef(LMmodel$nls.model)[[1]], 1))
                                data.table::set(Cal.RH.Inc, i = as.integer(Row.RH.Inc + row.reg - 1), j = "y0_RH",  value = round(coef(LMmodel$nls.model)[[2]], 1))
                                data.table::set(Cal.RH.Inc, i = as.integer(Row.RH.Inc + row.reg - 1), j = "r_RH",   value = round(coef(LMmodel$nls.model)[[3]], 3))
                                data.table::set(Cal.RH.Inc, i = as.integer(Row.RH.Inc + row.reg - 1), j = "yf_T",   value = round(coef(LMmodel$nls.model)[[4]], 1))
                                data.table::set(Cal.RH.Inc, i = as.integer(Row.RH.Inc + row.reg - 1), j = "y0_T",   value = round(coef(LMmodel$nls.model)[[5]], 1))
                                data.table::set(Cal.RH.Inc, i = as.integer(Row.RH.Inc + row.reg - 1), j = "r_T",    value = round(coef(LMmodel$nls.model)[[6]], 3))
                            }
                        }
                        # data.table::set(Cal.RH.Inc, i = as.integer(Row.RH.Inc + row.reg - 1), j = "Meas_Function",   value = LMmodel$Meas_Funct)
                        data.table::set(Cal.RH.Inc, i = as.integer(Row.RH.Inc + row.reg - 1), j = "rmse",    value = LMmodel$rmse)
                        data.table::set(Daily.RH.Inc, i = i.which, j = "Sum_eff", value = LMmodel$Daily.Matrice$Sum_eff[i.which])
                        Last.Bline <- LMmodel$pred.bline[length(LMmodel$pred.bline)]
                        # }
                    } else {
                        # data.table::set(Cal.RH.Inc, i = as.integer(Row.RH.Inc + row.reg - 1), j = "Meas_Function",   value = "no prev.bline")
                        Last.Bline <- NULL
                    }
                    data.table::set(Cal.RH.Inc, i = as.integer(Row.RH.Inc + row.reg - 1), j = "Meas_Function",   value = LMmodel$Meas_Funct)
                    data.table::set(Cal.RH.Inc, i = as.integer(Row.RH.Inc + row.reg - 1), j = "RH.sign", value = ifelse(RH.sign == 'decr', "RH_Dec", "RH_Inc"))
                    data.table::set(Cal.RH.Inc, i = as.integer(Row.RH.Inc + row.reg - 1), j = "var4Out",   value = var4Out.inc)
                    data.table::set(Cal.RH.Inc, i = as.integer(Row.RH.Inc + row.reg - 1), j = "Last.bline",   value = LMmodel$pred.bline[length(LMmodel$pred.bline)])
                    data.table::set(Matrice, i = Rows.cal.Inc[i.which], j = "nls_estimated",  value = LMmodel$pred.bline)
                    data.table::set(Daily.RH.Inc, i = i.which, j = "nls_estimated", value = LMmodel$pred.bline)
                    data.table::set(Daily.RH.Inc, i = i.which, j = "Inf.col", value = ifelse(LMmodel$Daily.Matrice$Outliers[i.which], "green", 'red'))
                    # adding the last nls-estimated date. It might be used later when there is no data to fit nls for the 1st region after 3 hours sensor stop
                    if (!exists('Value.last.date')) Value.last.date <- Daily.RH.Inc[i.which][!is.na(LMmodel$pred.bline)]$date[length(!is.na(LMmodel$pred.bline))]
                    data.table::set(Cal.RH.Inc, i = as.integer(Row.RH.Inc + row.reg - 1), j = "last.date", value = Value.last.date)
                    rm(Value.last.date)
                }
                if (any(na.omit(Cal.RH.Inc[date <= Date]$Meas_Function) == "no prev.bline") && na.omit(Cal.RH.Inc[Row.RH.Inc]$Meas_Function != "no prev.bline")) {
                    # checking if there is any region with the same date and nls_estimated
                    no.prev.bline <- sort(which(Cal.RH.Inc[date <= Date]$Meas_Function == "no prev.bline"), decreasing = T)
                    for (Reg in no.prev.bline) {
                        # Checking if the 1st date BEFORE this region has a predicted baseline. if it is within 2 h, we will calculate the baseline continuity
                        ## checking if there is any date before the start of this region
                        if (Matrice[date == Cal.RH.Inc[Reg]$Reg.str, which = T] > 1 && 
                            (difftime(Cal.RH.Inc[Reg]$Reg.str, max(Matrice[date < Cal.RH.Inc[Reg]$Reg.str & !is.na(Matrice$nls_estimated)]$date, na.rm = T), 
                                      units ='hours') < 2)) {
                            i.Last.nls.est <- Matrice[date == max(Matrice[date < Cal.RH.Inc[Reg]$Reg.str & !is.na(Matrice$nls_estimated)]$date, na.rm = T), which = T]
                        } else if (difftime(Cal.RH.Inc[Reg]$Reg.str, 
                                            min(Matrice[date > Cal.RH.Inc[Reg]$Reg.end & !is.na(Matrice$nls_estimated)]$date, na.rm = T), units ='hours') < 2) {
                            i.Last.nls.est <- Matrice[date == min(Matrice[date > Cal.RH.Inc[Reg]$Reg.end & !is.na(Matrice$nls_estimated)]$date, na.rm = T), which = T]
                        }
                        Last.nls.est  <- Matrice[i.Last.nls.est][['nls_estimated']]
                        Last.Sum_eff   <- Matrice[i.Last.nls.est][['Sum_eff']]
                        i.dates <- Matrice[date %between% c(Cal.RH.Inc[Reg]$Reg.str, Cal.RH.Inc[Reg]$Reg.end), which = T]
                        # prediction with the last corresponding nls.model, if not NULL
                        if (Cal.RH.Inc[Reg]$RH.sign == 'decr') {
                            if (!is.null(Dec.Last.nls.model)) {
                                bline.lastModel <- predict(Dec.Last.nls.model, newdata =  Matrice[i.dates])
                            } else next
                        } else if (!is.null(Inc.Last.nls.model)) {
                            bline.lastModel <- predict(Inc.Last.nls.model, newdata =  Matrice[i.dates])
                        } else next
                        rmse.lastModel  <- sqrt(sum((bline.lastModel - Matrice$y_MA[i.dates])^2)/(length(i.dates)-2))
                        sum.eff.nor      <- Matrice[i.dates]$Sum_eff - (Matrice[date == Cal.RH.Inc[Reg]$Reg.str]$Sum_eff - Last.Sum_eff) 
                        # estimated baseline with continuity
                        cont.bline     <- sum.eff.nor - (Last.Sum_eff - Last.nls.est)
                        # rmse of continued baseline
                        rmse.contBaseline <- sqrt(sum((cont.bline - Matrice$y_MA[i.dates])^2)/(length(i.dates)-2))
                        if (rmse.contBaseline < rmse.lastModel) {
                            estimated.bline <- cont.bline
                            Meas_Function   <- "recalc.Bline Conti."
                        } else {
                            estimated.bline <- bline.lastModel
                            Meas_Function   <- "recalc.Bline Prev.Mod"
                        }
                        data.table::set(Cal.RH.Inc, i = Reg, j = "Meas_Function",   value = Meas_Function)
                        data.table::set(Matrice, i = i.dates, j = "nls_estimated",  value = estimated.bline)
                        data.table::set(Matrice, i = i.dates, j = "y.cor.T",  value = Matrice[i.dates][["y"]] - Matrice[i.dates][["nls_estimated"]])
                        data.table::set(Matrice, i = which(Matrice[i.dates, y.cor.T < -20]), j = "y.cor.T",  value = NA)
                        data.table::set(Matrice, i = i.dates, j = "x.fitted", value = Matrice[i.dates][["y.cor.T"]]/ 0.54)
                    }
                }
                data.table::set(Cal.RH.Inc, i = Cal.RH.Inc[date == Date, which = T], j = "Var", value = 'nls_estimated')
                data.table::set(Cal.RH.Inc, i = Cal.RH.Inc[date == Date, which = T], j = "max.Est", value = ifelse(all(is.na(Daily.RH.Inc$nls_estimated)), NA_real_,max(Daily.RH.Inc$nls_estimated, na.rm = T)))
                data.table::set(Cal.RH.Inc, i = Cal.RH.Inc[date == Date, which = T], j = "min.Est", value = ifelse(all(is.na(Daily.RH.Inc$nls_estimated)), NA_real_,min(Daily.RH.Inc$nls_estimated, na.rm = T)))
                
                ################################################C
                # Applying the Calibration model to Predict data
                ################################################C
                # RH.Inc
                # Applying daily drift correction
                # updating Sum effect in Matrice 
                data.table::set(Matrice, i = Rows.cal.Inc, j = "Sum_eff",  value = Daily.RH.Inc$Sum_eff)
                # Correcting sensor response y for Temperature with exponential effect of the day
                data.table::set(Matrice, i = Rows.cal.Inc, j = "y.cor.T",  value = Matrice[Rows.cal.Inc][["y"]] - Matrice[Rows.cal.Inc][["nls_estimated"]])
                data.table::set(Matrice, i = Rows.cal.Inc[Matrice[Rows.cal.Inc, y.cor.T < -20]], j = "y.cor.T",  value = NA)
                if (exists('LMmodel') && any(is.na(LMmodel$Daily.Matrice$y))) {
                    na.which <- which(Matrice$date %in% LMmodel$Daily.Matrice[which(is.na(LMmodel$Daily.Matrice$y))]$date)
                    data.table::set(Matrice, i = na.which, j = "y.cor.T",  value = NA)
                    rm('na.which')}
                data.table::set(Matrice, i = Rows.cal.Inc, j = "x.fitted", value = Matrice[Rows.cal.Inc][["y.cor.T"]]/ 0.54) #RH_Inc
            }
            # Plotting all
            # if (as.Date(Date) %in% c(as.Date('2020-04-28'), as.Date('2020-04-29'), as.Date('2020-06-03'), as.Date('2020-06-04'), as.Date('2020-10-10')
            #                          , as.Date('2020-10-11'), as.Date('2020-12-13'))) Verbose <- T else Verbose <- F
            if (Verbose){
                # if((exists("Rows.cal.Inc") && (any(Matrice[Rows.cal.Inc][["y.cor.T"]] < -50) || any(Matrice[Rows.cal.Inc][["y.cor.T"]] > 500))) || # < -50,  > 150
                # (exists("Rows.cal.Dec") && (any(Matrice[Rows.cal.Dec][["y.cor.T"]] < -50) || any(Matrice[Rows.cal.Dec][["y.cor.T"]] > 500)))) 
                { # < -50,  > 150
                    #plotting All Daily Data
                    # Adding data while RH is increasing until the morning of the next day
                    if ((exists("Rows.cal.Inc") && shiny::isTruthy(Rows.cal.Inc) && length(Rows.cal.Inc) > 2) & (exists("Rows.cal.Dec") && !is.null(Rows.cal.Dec) && length(Rows.cal.Dec) > 2)){
                        Daily.Alldata <- sort(unique(c(Daily.data, Rows.cal.Inc, Rows.cal.Dec)))
                    } else if (exists("Rows.cal.Inc") && !is.null(Rows.cal.Inc) && length(Rows.cal.Inc) > 2){
                        Daily.Alldata <- sort(unique(c(Daily.data, Rows.cal.Inc)))
                    } else if(exists("Rows.cal.Dec") && !is.null(Rows.cal.Dec) && length(Rows.cal.Dec) > 2){
                        Daily.Alldata <- sort(unique(c(Daily.data, Rows.cal.Dec)))}
                    
                    if (exists("Regions.Dec")) {
                        n.row.dec <- nrow(Regions.Dec$Regions)
                        if (n.row.dec > 1) {
                            for (ii in 2:n.row.dec) {
                                assign(paste0("vline.dec.", ii-1), Peaks.dec$Daily.Matrice[Regions.Dec$Regions[ii-1]$Reg.End]$date)
                                if (exists('lines.dec')) lines.dec <- c(lines.dec, get(paste0("vline.dec.", ii-1))) else lines.dec <- get(paste0("vline.dec.", ii-1))
                            }} else lines.dec <- NULL
                    } else lines.dec <- NULL
                    if (exists("Regions.Inc")) { 
                        n.row.inc <- nrow(Regions.Inc$Regions)
                        if (n.row.inc > 1) {
                            for (ii in 2:n.row.inc) {
                                assign(paste0("vline.inc.", ii-1), Peaks.inc$Daily.Matrice[Regions.Inc$Regions[ii-1]$Reg.End]$date)
                                if (exists('lines.inc')) lines.inc <- c(lines.inc, get(paste0("vline.inc.", ii-1))) else lines.inc <- get(paste0("vline.inc.", ii-1))
                            }} else lines.inc <- NULL
                    } else lines.inc <- NULL
                    if(length(Daily.Alldata) > 2){
                        Day <- Day_Plot(Matrice, Daily.Alldata, Date, name.Temperature, name.Humidity, lines.inc = lines.inc, lines.dec = lines.dec, 
                                        Rows.cal.Inc    = if(exists("Rows.cal.Inc"))    Rows.cal.Inc    else NA,
                                        Rows.cal.Dec    = if(exists("Rows.cal.Dec"))    Rows.cal.Dec    else NA,
                                        Row.Min.RH      = if(exists("Row.Min.RH"))      Row.Min.RH      else NA,
                                        Row.Max.RH.next = if(exists("Row.Max.RH.next")) Row.Max.RH.next else NA,
                                        Row.Max.RH      = if(exists("Row.Max.RH"))      Row.Max.RH      else NA) }
                    
                    if(Do.RH.Dec) {
                        for (row.plot in  Cal.RH.Dec[date == as.Date(Date), which=T]) {
                            if (is.null(Cal.RH.Dec[row.plot]$yf_RH)) {
                                assign(paste0("Title", row.plot), "No nls model, will be predicted when corresponding model established")
                            } else if (is.na(Cal.RH.Dec[row.plot]$yf_RH)) {
                                assign(paste0("Title", row.plot), paste0(Cal.RH.Dec[row.plot]$RH.sign, " , " ,Cal.RH.Dec[row.plot]$Meas_Function))
                            } else {
                                assign(paste0("Title", row.plot), paste0(Cal.RH.Dec[row.plot]$RH.sign, ", yf_RH =", round(Cal.RH.Dec[row.plot]$yf_RH,1),
                                                                         ", y0_RH = ", round(Cal.RH.Dec[row.plot]$y0_RH,1),
                                                                         ", r_RH = ", round(Cal.RH.Dec[row.plot]$r_RH,3),
                                                                         ", yf_T =", round(Cal.RH.Dec[row.plot]$yf_T,1),
                                                                         ", y0_T = ", round(Cal.RH.Dec[row.plot]$y0_T,1),
                                                                         ", r_T = ", round(Cal.RH.Dec[row.plot]$r_T,3),
                                                                         ", rmse = ", round(Cal.RH.Dec[row.plot]$rmse,1), "," ,
                                                                         Cal.RH.Dec[row.plot]$Meas_Function, ",", Cal.RH.Dec[row.plot]$var4Out)) 
                            }
                            if (!exists("Title_dec")) Title_dec <- get(paste0("Title", row.plot)) else Title_dec <- c(Title_dec, get(paste0("Title", row.plot)))
                            rm(list = paste0("Title", row.plot)) 
                        }                                                                            
                        Morning <- Plot_Baseline(Peaks = Peaks.dec$Peaks,  Daily.RH = Daily.RH.Dec, Cal.RH = Cal.RH.Dec, Row.RH = row.plot, Title = Title_dec,
                                                 Covariate = "nls_estimated", Daytime = "morning", vlines = lines.dec)
                        rm("Title_dec")
                        T_eff_dec <- Peaks.dec$Daily.Matrice$T_eff
                        RH_eff_dec <- Peaks.dec$Daily.Matrice$RH_eff
                        Sum_eff_dec <- Peaks.dec$Daily.Matrice$Sum_eff
                        Day <- Add_peak(Day, Peaks = Peaks.dec$Peaks, Daily.RH = Daily.RH.Dec, Var = 'y_MA')
                    } else {
                        Title <- 'No Exp.T.Dec, no data'
                        Morning <- ggplot() + theme_void() +
                            geom_text(aes(0,0,label= Title)) +
                            xlab(NULL) #optional, but safer in case another theme is applied later
                    }
                    
                    if(Do.RH.Inc) {
                        for (row.plot in  Cal.RH.Inc[date == as.Date(Date), which=T]) {
                            if (is.null(Cal.RH.Inc[row.plot]$yf_RH)) {
                                assign(paste0("Title", row.plot), "No nls model, will be predicted when corresponding model established")
                            } else if (is.na(Cal.RH.Inc[row.plot]$yf_RH)) {
                                assign(paste0("Title", row.plot), paste0(Cal.RH.Inc[row.plot]$RH.sign, " , ",Cal.RH.Inc[row.plot]$Meas_Function))
                            } else {
                                assign(paste0("Title", row.plot), paste0(Cal.RH.Inc[row.plot]$RH.sign, ", yf_RH =", round(Cal.RH.Inc[row.plot]$yf_RH,1),
                                                                         ", y0_RH = ", round(Cal.RH.Inc[row.plot]$y0_RH,1),
                                                                         ", r_RH = ", round(Cal.RH.Inc[row.plot]$r_RH,3),
                                                                         ", yf_T =", round(Cal.RH.Inc[row.plot]$yf_T,1),
                                                                         ", y0_T = ", round(Cal.RH.Inc[row.plot]$y0_T,1),
                                                                         ", r_T = ", round(Cal.RH.Inc[row.plot]$r_T,3),
                                                                         ", rmse = ", round(Cal.RH.Inc[row.plot]$rmse,1), "," ,
                                                                         Cal.RH.Inc[row.plot]$Meas_Function, ",", Cal.RH.Inc[row.plot]$var4Out))
                            }
                            if (!exists("Title_inc")) Title_inc <- get(paste0("Title", row.plot)) else Title_inc <- c(Title_inc, get(paste0("Title", row.plot)))
                            rm(list = paste0("Title", row.plot)) 
                        }
                        Afternoon <- Plot_Baseline(Peaks = Peaks.inc$Peaks,  Daily.RH = Daily.RH.Inc, Cal.RH = Cal.RH.Inc, Row.RH = row.plot, Title = Title_inc,
                                                   Covariate = "nls_estimated", Daytime = "evening", vlines = lines.inc)
                        rm("Title_inc")
                        T_eff_inc     <- Peaks.inc$Daily.Matrice$T_eff
                        RH_eff_inc    <- Peaks.inc$Daily.Matrice$RH_eff
                        Sum_eff_inc <- Peaks.inc$Daily.Matrice$Sum_eff
                        Day <- Add_peak(Day, Peaks = Peaks.inc$Peaks, Daily.RH = Daily.RH.Inc, Var = 'y_MA')
                    } else {
                        Title <- 'No Exp.T.Inc, no data'
                        Morning <- ggplot() + theme_void() +
                            geom_text(aes(0,0,label= Title)) +
                            xlab(NULL) #optional, but safer in case another theme is applied later
                    }
                    if (Do.RH.Dec && Do.RH.Inc) {
                        Day <- Day + 
                            geom_line(data= Daily.RH.Dec, aes(x = date, y = T_eff_dec), color = 'red', linewidth = 1, lty = 'dashed') +
                            geom_line(data= Daily.RH.Inc, aes(x = date, y = T_eff_inc), color = 'red', linewidth = 1, lty = 'dashed') +
                            geom_line(data= Daily.RH.Dec, aes(x = date, y = RH_eff_dec), color = 'blue', linewidth = 1, lty = 'dashed') +
                            geom_line(data= Daily.RH.Inc, aes(x = date, y = RH_eff_inc), color = 'blue', linewidth = 1, lty = 'dashed') +
                            geom_line(data= Daily.RH.Dec, aes(x = date, y = Sum_eff_dec), color = 'violet', linewidth = 1, lty = 'dashed') +
                            geom_line(data= Daily.RH.Inc, aes(x = date, y = Sum_eff_inc), color = 'violet', linewidth = 1, lty = 'dashed') +
                            geom_line(data= Daily.RH.Dec, aes(x = date, y = nls_estimated), color = "yellow", linewidth = 1, lty = 'dashed') +
                            geom_line(data= Daily.RH.Inc, aes(x = date, y = nls_estimated), color = "yellow", linewidth = 1, lty = 'dashed')
                    } else if (Do.RH.Dec) {
                        Day <- Day +
                            geom_line(data= Daily.RH.Dec, aes(x = date, y = T_eff_dec), color = 'red', linewidth = 1, lty = 'dashed') +
                            geom_line(data= Daily.RH.Dec, aes(x = date, y = RH_eff_dec), color = 'blue', linewidth = 1, lty = 'dashed') +
                            geom_line(data= Daily.RH.Dec, aes(x = date, y = Sum_eff_dec), color = 'violet', linewidth = 1, lty = 'dashed') +
                            geom_line(data= Daily.RH.Dec, aes(x = date, y = nls_estimated), color = "yellow", linewidth = 1, lty = 'dashed')
                    } else {
                        Day <- Day +
                            geom_line(data= Daily.RH.Inc, aes(x = date, y = T_eff_inc), color = 'red', linewidth = 1, lty = 'dashed') +
                            geom_line(data= Daily.RH.Inc, aes(x = date, y = RH_eff_inc), color = 'blue', linewidth = 1, lty = 'dashed') +
                            geom_line(data= Daily.RH.Inc, aes(x = date, y = Sum_eff_inc), color = 'violet', linewidth = 1, lty = 'dashed') +
                            geom_line(data= Daily.RH.Inc, aes(x = date, y = nls_estimated), color = "yellow", linewidth = 1, lty = 'dashed')
                    }
                    # Plotting T/RH corrections
                    Corrected <- ggplot(data = Matrice[Daily.Alldata]) +
                        geom_point(aes(x = date, y = y), color = Matrice[Daily.Alldata]$RH.Col, show.legend = FALSE, size = 0.75) + 
                        labs(title = paste0("Correction T and RH", " ", as.Date(Date)))  +
                        theme(plot.title = element_text(size=12)) +
                        ylim(c(min(min(Matrice[Daily.Alldata,][[name.Temperature]], na.rm = T), 
                                   min(Matrice[Daily.Alldata,][["y.cor.T"]], na.rm = T)),
                               max(max(Matrice[Daily.Alldata,][["y"]], na.rm = T),
                                   max(Matrice[Daily.Alldata,][[name.Temperature]], na.rm = T)))) +
                        geom_line(aes(x = date, y = get(name.Temperature)), color = "red") + 
                        geom_hline(yintercept = 0, color = "black")
                    if(Do.RH.Dec){
                        if (!is.null(Rows.cal.Dec) && length(Rows.cal.Dec) > 2){
                            Corrected <- Corrected + geom_vline(xintercept = c(Matrice[Row.Max.RH]$date, Matrice[Row.Min.RH]$date), color = c("green4", "red")) + 
                                geom_vline(xintercept = lines.dec, color = "yellow", linewidth = 1L)}
                        if(length(Row.RH.Dec) == 1 && Row.RH.Dec !=0){
                            Corrected <- Corrected + geom_line(data = Matrice[Rows.cal.Dec], aes(x = date, y = y.cor.T), color = "red")}}
                    if(Do.RH.Inc){
                        if (!is.null(Rows.cal.Inc) && length(Rows.cal.Inc) > 2){
                            Corrected <- Corrected + geom_vline(xintercept = c(Matrice[Row.Min.RH]$date, Matrice[Row.Max.RH.next]$date), color = c("green4", "red"), lty = c(2,2))}
                        Corrected <- Corrected + geom_line(data = Matrice[Rows.cal.Inc], aes(x = date, y = y.cor.T), color = "blue") +
                            geom_vline(xintercept = lines.inc, color = "yellow", linewidth = 1L)
                    }
                    
                    ListPlot <- lapply(c("Day", "Morning", "Afternoon", "Corrected"), function(GPlot){if(exists(GPlot)) return(get(GPlot))})
                    Total.plot <- ggpubr::ggarrange(plotlist  = ListPlot, ncol = 2, nrow = 2, align = 'h')
                    plot(Total.plot)
                }}
            for (Variable in c("LMmodel","Day", "Morning", "Afternoon", "Corrected", "Title")) if (exists(Variable)) rm(list = Variable)
            
            # Resetting variables
            for (Variable in c("Row.RH.Inc", "Row.RH.Dec", "Rows.cal.Inc", "Daily.RH.Inc", "Rows.cal.Dec", "Daily.RH.Dec", "Daily.data", "Daily.Alldata", "Daily.Matrice.Dec", 'Daily.Matrice.Inc',
                               "Row.Min.RH", "Row.Max.RH")) if (exists(Variable)) rm(list = Variable)
        }
        # combining Cal.RH.Inc and Cal.RH.Dec
        if (exists("Cal.RH.Dec") && exists("Cal.RH.Inc")) Cal.RH <- rbindlist(list(Cal.RH.Dec, Cal.RH.Inc), use.names = T, fill=T)
        # ordering by date
        Cal.RH <- Cal.RH[order(Cal.RH$date, decreasing = F)]
        #Saving
        if (file.exists(file.path(dirname(name.Model), paste0(name.sensor, "_", Mod_type, "_CalRH.csv")))) {
            # checking if the dates of Cal.RH are already in the dates of existing csv file. if not, the csv file will be updated
            if (!all(unique(as.Date(Cal.RH$date)) %in% unique(as.Date(data.table::fread(file.path(dirname(name.Model), paste0(name.sensor, "_", Mod_type, "_CalRH.csv")))$date)))) {
                data.table::fwrite(Cal.RH, file.path(dirname(name.Model), paste0(name.sensor, "_", Mod_type, "_CalRH.csv")))
            }
        } else data.table::fwrite(Cal.RH, file.path(dirname(name.Model), paste0(name.sensor, "_", Mod_type, "_CalRH.csv")))
        # if (exists(data.table::fread(file.path(dirname(name.Model), paste0(name.sensor, "_", Mod_type, "_Cal.RH.csv")))))
        # if (exists("Cal.RH") && exists("name.Model") && unique(as.Date(Matrice$date)) %in% as.Date(data.table::fread(file.path(dirname(name.Model), paste0(name.sensor, "_", Mod_type, "_CalRH.csv")))$date) {
        #     data.table::fwrite(Cal.RH, file.path(dirname(name.Model), paste0(name.sensor, "_", Mod_type, "_CalRHDec.csv")))
        # }
        # 
        # Saving Cal.RH.Inc and Cal.RH.Dec
        # if (exists("Cal.RH.Dec") && exists("name.Model")) {
        #     data.table::fwrite(Cal.RH.Dec, file.path(dirname(name.Model), paste0(name.sensor, "_", Mod_type, "_CalRHDec.csv")))
        # }
        # if (exists("Cal.RH.Inc") && exists("name.Model")) {
        #     data.table::fwrite(Cal.RH.Inc, file.path(dirname(name.Model), paste0(name.sensor, "_", Mod_type, "_CalRHInc.csv")))
        # }
        # returning
        return(Matrice[,.SD, .SDcols = c("date", "x.fitted")])
        
    } else if (Mod_type == 'Exponential') {
        return(-log(1 - (y - Model$Coef[3]) / Model$Coef[1]) / Model$Coef[2])
    } else if (Mod_type == 'ExpDecayInc_Int') {
        if (any(!is.na(y) & y < Model$Coef[3] ) | any(!is.na(y) & y > Model$Coef[1] + Model$Coef[3])) { # in case value out of bound and value is not NA
            print("Some Y value out of limits of the model", quote = FALSE)
        }
        x <- log(1 - ((y - Model$Coef[3])/Model$Coef[1]))/-Model$Coef[2]
        #  yy <- Model$Coef[1] * (1-exp(-Model$Coef[2] * xx)) + Model$Coef[3]
        return(x)
    } else if (Mod_type == 'exp_kT_NoC') {
        CovMod <- ifelse(is.null(covariates), "Out.Temperature", covariates)
        # Extracting T.min from the equation
        pattern <- "T_Celsius - \\s*(.*?)\\s*), RMSE"
        T.min <- as.numeric(regmatches(Model$Equation, regexec(pattern, Model$Equation))[[1]][2])
        setDT(Matrice)
        #data.table::set(Matrice, j = "T.min", value = caTools::runmin(Matrice$Out.Temperature_int, 1440))
        #Estimated <- as.vector((y - (Model$Coef["C"] * exp(Model$Coef["k"] * (Matrice[[CovMod]] - Matrice$T.min)))) /Model$Coef["a1"] ) #  - Model$Coef[4]
        Estimated <- as.vector((y - (Model$Coef["C"] * exp(Model$Coef["k"] * (Matrice[[ CovMod]] - T.min)))) /Model$Coef["a1"] ) #  - Model$Coef[4]
        # T.biger.25 <- which(Matrice[, CovMod] > 25)
        # if (length(T.biger.25) > 0) Estimated[T.biger.25] <- NA
        return(Estimated)
    } else if (Mod_type == 'exp_kT') {
        # model f_exp_kT: return( (y - (C + exp(k * Temperature - Tmin)))/ a1) )
        CovMod <- ifelse(is.null(covariates), "Out.Temperature", covariates)
        # model f_exp_kT: return( (y - (a0 + C exp(k * Temperature)))/ a1) )
        Estimated <- as.vector( (y - (Model$Coef[1] + Model$Coef[3] * exp(Model$Coef[4] * Matrice[, CovMod]))) / Model$Coef[2] )
        return(Estimated)
    } else if (Mod_type == 'exp_kTn') {
        # model f_exp_kT: return( (y - (C + exp(Temperature ^n)))/ a1) )
        CovMod <- ifelse(is.null(covariates), "Out.Temperature", covariates)
        Estimated <- as.vector( (y - (Model$Coef[1] + Model$Coef[3] * exp(Matrice[[CovMod]] ^ Model$Coef[4]))) / Model$Coef[2] )
        return(Estimated)
    } else if (Mod_type == 'exp_kK') {
        # model f_exp_kT: return( (y - (a0 + C.exp(k * Temperature)))/ a1) )
        CovMod <- ifelse(is.null(covariates), "Out.Temperature", covariates)
        Estimated <- as.vector((y - (Model$Coef[1] + Model$Coef[3] * exp(Model$Coef[4] * (273.15 + Matrice[[CovMod]]))))/Model$Coef[2])
        return(Estimated)
    } else if (Mod_type == 'T_power') {
        # model T_power: return( (y - (a0 + a2 T^n))/ a1)
        CovMod <- ifelse(is.null(covariates), "Out.Temperature", covariates)
        return(as.vector((y - (Model$Coef[1] + Model$Coef[3] * Matrice[[CovMod]]^Model$Coef[4])) / Model$Coef[2]))
    } else if (Mod_type == 'K_power') {
        # model K_power: return( (y - (a0 + a2 T^n))/ a1) T in Kevin
        CovMod <- ifelse(is.null(covariates), "Out.Temperature", covariates)
        return(as.vector((y - (Model$Coef[1] + Model$Coef[3] * (273.15 + Matrice[[CovMod]])^Model$Coef[4])) / Model$Coef[2]))
    } else if (Mod_type == 'NO2_Lab') {
        # model NO2_Lab: return( (y - (a0 + a2 RH - a3 exp(kt)))/ a1) T in Celsius degree
        if(is.null(covariates)) covariates <- c("out.Relative_humidity", "Out.Temperature")
        name.Temperature <- grep("Temp", covariates, value = T)
        name.Humidity    <- grep("umid", covariates, value = T)
        return(as.vector((y - (Model$Coef["a0"] + Model$Coef["a2"] * Matrice[[name.Humidity]] - Model$Coef["a3"] * exp(Model$Coef["k"] * Matrice[[name.Temperature]]))) / Model$Coef["a1"]))
    } else if (Mod_type == 'NO2_Lab_decay_inc') {
        # model NO2_Lab: return( (y - (a0 + a2 RH - exp(a3 (1 - exp(k(t - T0))))))/ a1) T in Celsius degree
        if(is.null(covariates)) covariates <- c("out.Relative_humidity", "Out.Temperature")
        name.Temperature <- grep("Temp", covariates, value = T)
        name.Humidity    <- grep("umid", covariates, value = T)
        return(as.vector((y - (Model$Coef["a0"] + Model$Coef["a2"] * Matrice[[name.Humidity]] - exp(Model$Coef["a3"] * (1 - exp(Model$Coef["k"] * (Matrice[[name.Temperature]] - Model$Coef["T0"])))))) / Model$Coef["a1"]))
    } else if (Mod_type == 'BeerLambert') {
        # model T_power: return( (y - a0)/ a1 * Pressure / Temperature^a2)
        #return(as.vector((y - Model$Coef[1]) / Model$Coef[2] * (Matrice[["Out.Atmospheric_pressure"]] + Model$Coef[4]) / (273.15 + Matrice[["Out.Temperature"]])^Model$Coef[3]))
        return(as.vector((y - Model$Coef[1]) / Model$Coef[2] * (Matrice[["Out.Atmospheric_pressure"]]) / ((273.15 + Matrice[["Out.Temperature"]])/Model$Coef[2])^Model$Coef[3]))
    } else if (Mod_type %in% c("Kohler", "Kohler_lit")) {
        # model return C <- 1 + (K/1.65/(100/RH - 1)) return(a0 + a1 * (x /C))
        C <- 1 + (Model$Coef[3]/1.65/(100/Matrice[[covariates]] - 1))
        return(as.vector((y - Model$Coef[1]) /(Model$Coef[2] * C)))
    } else if (Mod_type %in% c("Kohler_modified")) {
        # model return C <- 1 + (K/1.65/(100/RH - 1)) return(a0 + a1 * (x /C))
        # Filtering for RH > 95 %
        Matrice[Matrice[[covariates]] > 95, covariates] <- 95
        C <- 1 + (Model$Coef[3]/1.65/(100/Matrice[[covariates]] - 1))
        return(as.vector((y/C - Model$Coef[1]) /(Model$Coef[2])))
    } else if (Mod_type == 'Kohler_only') {
        # model return C <- 1 + (K/1.65/(100/RH - 1)) return((x /C))
        C <- 1 + (Model$Coef[1]/1.65/(100/Matrice[[covariates]] - 1))
        return(as.vector(y / C))
    } else if (Mod_type == 'Michelis') {
        # model f_Michelis: return(Vmax*x/(km +x) + intercept)
        if (any(!is.na(y) & y < Model$Coef[3] ) | any(!is.na(y) & y > Model$Coef[1])) { # in case value out of bound and value is not NA
            print("Some Y value out of limits of the model", quote = FALSE)
        }
        x <- (-Model$Coef[2]*(y - Model$Coef[3])) / (y - Model$Coef[3] - Model$Coef[1])
        #  yy <- Model$Coef[1] * (1-exp(-Model$Coef[2] * xx)) + Model$Coef[3]
        return(x)
    } else if (Mod_type == 'ExpDecayDec_Int') {
        # return(y = C *(exp(-k*x)) + intercept)
        if (any(!is.na(y) & y < Model$Coef[3] ) | any(!is.na(y) & y > Model$Coef[1] + Model$Coef[3])) { # in case value out of bound and value is not NA
            print("Some Y value out of limits of the model", quote = FALSE)
        }
        x <- log((y - Model$Coef[3])/Model$Coef[1])/-Model$Coef[2]
        return(x)
    } else if (Mod_type == "Sigmoid") {
        if (any(!is.na(y) & y < Model$Coef[1]) | any(!is.na(y) & y > Model$Coef[2])) { # in case value out of bound and value is not NA
            print("Some Y value out of the limits of the model", quote = FALSE)
        }
        #  return(y = MIN + (MAX-MIN) / (1 + (X50/x)^Hill))
        x <- Model$Coef[3]*((y-Model$Coef[1])/(Model$Coef[2]-y))^(1/Model$Coef[4])
        return(x)
    } else if (Mod_type=="Logarithmic") {
        if (y<Model$Coef[1] )  {
            #  return(y = MIN + (MAX-MIN) / (1 + (X50/x)^Hill))
            x <- exp((y-Model$Coef[1])/Model$Coef[2])
            return(x)
        } else {
            print("y values out of limits", quote = FALSE)
        }
    }
}
#================================================================CR
### MeasLinear: Function Measurement Function x = f(y) once the Linear Calibration function (y = f(x) of sensor is established e.g with Cal_Line ====
#================================================================CR
MeasLinear <- function(x, ModLinear) {
    (x-coef(ModLinear)[1])/coef(ModLinear)[2]
}
#================================================================CR
### MeasParab: Function Measurement Function x = f(y) once the Quadratic Calibration function (y = f(x) of sensor is established e.g with Cal_Line ====
#================================================================CR
MeasParab <- function(x, ModParab) {
    if (coef(ModParab)[2] > 0) {
        return((-coef(ModParab)[2]+sqrt(coef(ModParab)[2]^2-4*coef(ModParab)[3]*(coef(ModParab)[1]-x)))/(2*coef(ModParab)[3]))
    } else {
        return((-coef(ModParab)[2]-sqrt(coef(ModParab)[2]^2-4*coef(ModParab)[3]*(coef(ModParab)[1]-x)))/(2*coef(ModParab)[3]))
    }
}
#================================================================CR
### MeasSigmoid: Function Measurement Function x = f(y) once the Sigmoidal Calibration function (y = f(x) of sensor is established e.g with Cal_Line ====
#================================================================CR
MeasSigmoid <- function(x, ModSigmoid) {
    (log((coef(ModSigmoid)[1]-coef(ModSigmoid)[4])/(x-coef(ModSigmoid)[4])-1)/(-coef(ModSigmoid)[2])+coef(ModSigmoid)[3])
}
#================================================================CR
### Adjust: Function Measurement Function using reverse calibration ====
#================================================================CR
Adjust <- function(model_type, datafr, model_cal, zero_limit, Zero_drift_frame) {
    for (g in 1:nrow(datafr)) {
        if (datafr$O3_Sensor[g] > zero_limit) {
            if (model_type =="Sigmoid") {
                datafr$c_O3_Sensor[g]   <- MeasSigmoid(datafr$O3_Sensor[g],model_cal)
                datafr$s_c_O3_Sensor[g] <- MeasSigmoid(datafr$O3_Sensor[g]+datafr$s_O3_Sensor[g],model_cal)-datafr$c_O3_Sensor[g]
            }
            if (model_type =="Quadratic") {
                datafr$c_O3_Sensor[g]   <- MeasParab(datafr$O3_Sensor[g],model_cal)
                datafr$s_c_O3_Sensor[g] <- MeasParab(datafr$O3_Sensor[g]+datafr$s_O3_Sensor[g],model_cal)-datafr$c_O3_Sensor[g]
            }
            if (model_type =="Linear") {
                datafr$c_O3_Sensor[g]   <- MeasLinear(datafr$O3_Sensor[g],model_cal)
                datafr$s_c_O3_Sensor[g] <- MeasLinear(datafr$O3_Sensor[g]+datafr$s_O3_Sensor[g],model_cal)-datafr$c_O3_Sensor[g]
            }
        } else {
            datafr$c_O3_Sensor[g] <- datafr$O3_Sensor[g] - mean(Zero_drift_frame$O3_Sensor)
            datafr$s_c_O3_Sensor[g] <- datafr$s_O3_Sensor[g]
        }
        print(sprintf(
            #" g = %.0f, O3_sensor = %.1f, c_O3_sensor = %.1f, NO2_sensor = %.1f, c_NO2_sensor = %.1f"
            " g = %.0f, O3_sensor = %.1f, c_O3_sensor = %.1f, s_c_O3_sensor = %.1f"
            , g
            , datafr$O3_Sensor[g], datafr$c_O3_Sensor[g], datafr$s_c_O3_Sensor[g]
            #, datafr$NO2_Sensor[g]
            #, datafr$c_NO2_Sensor[g]
        ), quote = FALSE)
    }
    return(datafr)
}
#================================================================CR
### sigdigss: Function to estimate Significant numbers ====
#================================================================CR
sigdigss <- function(n) {
    i <- 0
    # Check for decimal point is present
    if (length(grep("\\.", numstr[n])) > 0) { # real number
        # Separate integer and fractional parts
        intfrac <- unlist(strsplit(numstr[n], "\\."))
        digstring <- paste(intfrac[1], intfrac[2], sep = "")
        numfigs <- nchar(digstring)
        while (i < numfigs) {
            # Find index of 1st non-zero digit from LEFT
            if (substr(digstring,i+1,i+1) == "0") {
                i <- i + 1
                next
            } else {
                sigfigs <- numfigs - i
                break
            }
        }
    } else {  # must be an integer
        digstring <- numstr[n]
        numfigs <- nchar(digstring)
        while (i < numfigs) {
            # Find index of 1st non-zero digit from RIGHT
            if (substr(digstring, numfigs - i, numfigs - i) == "0") {
                i <- i + 1
                next
            } else {
                sigfigs <- numfigs - i
                break
            }
        }
    }
    return(sigfigs)
}
#================================================================CR
### polyfit: Function to fit the best polynomial fitting (Vs > 151016) ====
#================================================================CR
polyfit <- function(XY, i) {
    # The function returns the degree of the polynomila with first minimum of AIC for a set of n-1 degree polynomials with maximum degree i:
    # may not have a single minimum. check this with something like
    # XY: dataframe with x and Y values
    # removing NA
    XY <- na.omit(XY)
    #limiting i to the number of row in XY -1
    i  <- min(i, nrow(XY)-1)
    polyfit <- function(i) x <- AIC(lm(XY[,2] ~ poly(XY[,1],i))) # Should be outside the function
    degre <- as.integer(optimize(polyfit,interval = c(1,i))$minimum)
    cat(sprintf("Best degree of polynomial y = f8x): %.0f: ", degre), "\n")
    for (j in 1:i) {
        cat(sprintf("Polynomial of degree %.0f: AIC %.3f", j, polyfit(j)), "n")
    }
    return(degre)
}
#================================================================CR
### citytech_no2_o3_model_delta: Function to calibrate from lab (Vs > 151016)-not working, not finish, delete? ====
#================================================================CR
citytech_no2_o3_model_delta <- function(DRS_NO2_V, DRS_O3_V, DTemp, DRH, SO2_emep, Dtime,time, parameters) {
    # Author:Gabri
    #
    # Input variables -vector kind: Should have the same length
    # DSNO2_volt           :: DSensor_NO2 (V)
    # DSO3_volt            :: DSensor_O3 (V)
    # DTemp                :: Dtemperature (Celsius degree)
    # DRH                  :: Drelative humidity (%)
    # SO2_emep             :: SO2 concentration (ppb)
    # Dtime                :: Dtime (in days)
    #
    #
    # Input variables -vector type containing 1D parameters:
    # parameters$time0    :: time when the calibration occurred
    # parameters$T0       :: reference temperature set for calibration
    # parameters$RH0      :: reference RH set for calibration
    # parameters$a1       :: angolar coeff. for linear regression of S_NO2 vs NO2_ppb
    # parameters$b1       :: angolar coeff. for linear regression of S_NO2 vs O3_ppb
    # parameters$c1       :: angolar coeff. for linear regression of S_NO2 vs SO2_ppb
    # parameters$e1       :: angolar coeff. for linear regression of S_O3 vs NO2_ppb
    # parameters$f1       :: angolar coeff. for linear regression of S_O3 vs O3_ppb
    # parameters$S_RH_NO2 :: NO2 Sensor response in Volt due to RH variation [V RH(%)-1]
    # parameters$S_RH_O3  :: O3 Sensor response in Volt due to RH variation [V RH(%)-1]
    # parameters$S_T_NO2  :: NO2 Sensor response in Volt due to T variation [V Cdeg-1]
    # parameters$S_T_O3   :: O3 Sensor response in Volt due to T variation [V Cdeg-1]
    # parameters$cost_NO2   :: NO2 cost  in Volt [V]
    # parameters$cost_O3   :: O3 cost  in Volt [V]
    #
    #
    #
    #
    # Output:
    # Xres :: time, NO2 and O3 concentration in ppb.  time  <- Xres[1,]: NO2 <- Xres[2,],  O3 <- Xres[3,]
    #
    # Model to be solved: -------------------------------------------------------------------------CR
    #  A x X = C, where:
    # the matrix  A=(a_1 b_1)
    #               (e_1 f_1 ),
    #
    # The unknown vector  X=(NO2_ppb)
    #                       (O3_ppb)
    # The constant vector
    #    C=( DRS_NO2_V - (c1*SO2_ppb  + (T-To)*S_T_NO2 + (RH-RHo)*S_RH_NO2 +cost_NO2) )
    #      ( DRS_O3_V  - ( (T-To)*S_T_O3  + (RH-RHo)*S_RH_O3) +cost_O3                )
    #
    #difftime: function to calculate difference between dates
    #
    # Function starts
    #
    # ----------------------------------------------------------------------------------------------CR
    #
    nlen <- length(DRH) # Define length matrices
    print(length(DRH))
    print(nlen)
    # -----------------------------------------------------------------------------------------------CR
    # Building system of equations
    #
    # Unknown vector  X=(NO2_ppb) ,
    #                   (O3_ppb)
    # Define X: NO2 <- X[1,],  O3 <- X[2,]
    X <- matrix(data=NA, nrow=2, ncol=nlen)
    # Define matrix A
    # A=(a_1 b_1)
    #   (e_1 f_1)
    A <- matrix(data =NA, nrow=2,ncol=2)
    A[1,c(1:2)] <- c(parameters$a1, parameters$b1)
    A[2,c(1:2)] <- c(parameters$e1, parameters$f1)
    #print(A) this is OK
    # calculate C:
    ctmp10 <- DRS_NO2_V
    ctmp11 <- parameters$c1*SO2_emep
    ctmp12 <- DTemp*parameters$S_T_NO2
    ctmp13 <- DRH*parameters$S_RH_NO2
    ctmp14 <- rep(parameters$cost_no2, length.out=length(DRS_NO2_V ))
    ctmp1 <- ctmp10-(ctmp11+ctmp12+ctmp13+ctmp14)
    ctmp20 <- DRS_O3_V
    ctmp21 <- DTemp * parameters$S_T_O3
    ctmp22 <- DRH*parameters$S_RH_O3
    ctmp23 <- rep(parameters$cost_o3, length.out=length(DRS_O3_V))
    ctmp2 <- ctmp20 -(ctmp21+ctmp22+ctmp23)
    inC <- rbind(ctmp1,ctmp2)
    C <- matrix(data=inC, nrow=2, ncol=nlen)
    Xres <-cbind(as.POSIXct(time),X[1,],X[2,])
    Xres <-as.data.frame(Xres)
    colnames(Xres) <- c("date","NO2", "O3")
    Xres$date <- as.POSIXct(time)
    str(Xres)
    rm(list=ls(pattern="ctmp"))
    rm(inC,nlen)
    return <-Xres
}
#================================================================CR
### Tables for Rmarkdown reporting(Vs 170428) ====
#================================================================CR
Table_Reference <- function(df) {
    df <- df[,c("BeginDate", "date", "Step" , "O3f_ppb", "NO2f_ppb", "NOf_ppb", "COf_ppm", "SO2f_ppb", "Tempf", "RHf", "P_hPa", "Wind_Speed", "RHf.1", "Step")]
    # format date and time
    df$BeginDate <- strptime(df$BeginDate  , "%Y-%m-%d %H:%M:%S")
    df$BeginDate <- as.POSIXct(df$BeginDate, tz = "Europe/Rome")
    df$date      <- strptime(df$date       , "%Y-%m-%d %H:%M:%S")
    df$date      <- as.POSIXct(df$date     , tz = "Europe/Rome")
    table_data <- data.frame( Begin              = character(0)
                              , End              = character(0)
                              , Step             = numeric(0)
                              , O3f_ppb          = numeric(0)
                              , NO2f_ppb         = numeric(0)
                              , NOf_ppb          = numeric(0)
                              , COf_ppm          = numeric(0)
                              , SO2f_ppb        = numeric(0)
                              , Tempf            = numeric(0)
                              , RHf              = numeric(0)
                              , P_hPa            = numeric(0)
                              , Wind_Speed       = numeric(0)
                              , stringsAsFactors = FALSE
                              ,  check.names     = FALSE)
    table_data[1:nrow(df),"Begin"]  <- format(df$BeginDate, "%Y-%m-%d %H:%M")
    table_data[,"End"]              <- format(df$date     , "%Y-%m-%d %H:%M")
    table_data[,"Step"]             <- df$Step
    table_data[,"O3f_ppb"]          <- formattable(df$O3f_ppb   , digits=1, format = "f")
    table_data[,"NO2f_ppb"]         <- formattable(df$NO2f_ppb  , digits=1, format = "f")
    table_data[,"NOf_ppb"]          <- formattable(df$NOf_ppb   , digits=1, format = "f")
    table_data[,"COf_ppm"]          <- formattable(df$COf_ppm   , digits=3, format = "f")
    table_data[,"SO2f_ppb"]         <- formattable(df$SO2f_ppb  , digits=1, format = "f")
    table_data[,"Tempf"]            <- formattable(df$Tempf     , digits=1, format = "f")
    table_data[,"RHf"]              <- formattable(df$RHf.1     , digits=1, format = "f")
    table_data[,"P_hPa"]            <- formattable(df$P_hPa     , digits=0, format = "f")
    table_data[,"Wind_Speed"]       <- formattable(df$Wind_Speed, digits=1, format = "f")
    colnames(table_data)        <- c("Begin","End","Step","O3, ppb","NO2, ppb","NO, ppb","CO, ppm","SO2, ppb","Temp., ?C","RH, %","Pressure, hPa","Wind_Speed, m/s")
    table_data <- table_data[,-which(colnames(table_data) == "Step")]
    row.names(table_data) <- NULL
    return(table_data)
}
Table_AllEffects <- function(df) {
    mm <- df[,c("Intercept", "s_Intercept", "Pr_Intercept", "Slope", "s_Slope", "Pr_Slope" , "R2", "Adj.R2", "Sigma", "u_lof" )]
    mm.means <- aggregate(mm, by = list(df$Sensor_type, df$Compounds), FUN = mean)
    mm.SD    <- aggregate(mm, by = list(df$Sensor_type, df$Compounds), FUN = sd)
    table_data <- data.frame( SensorTypes = character(0)
                              , Compounds = character(0)
                              , Intercept= character(0)
                              , P_Intercept = numeric(0)
                              , Slope = character(0)
                              , P_Slope = numeric(0)
                              , R.sq = numeric(0)
                              , AjustR.sq = numeric(0)
                              , Sigma = numeric(0)
                              , u_lof = numeric(0)
                              , stringsAsFactors = FALSE
                              ,  check.names = FALSE)
    table_data[1:length(mm.means$Group.1),"SensorTypes"]  <- mm.means$Group.1
    table_data[,"Compounds"   ] <- mm.means$Group.2
    table_data[,"Intercept"   ] <- paste0(formattable(mm.means$Intercept, digits = 5, format = "f"), " ? "
                                          , formattable(mm.SD$Intercept , digits = 5, format = "f"))
    table_data[,"P_Intercept"]  <- formattable(mm.means$Pr_Intercept    , digits = 3, format = "f")
    table_data[,"Slope"       ] <- paste0(formattable(mm.means$Slope    , digits = 8, format = "f"), " ? "
                                          , formattable(mm.SD$Slope     , digits = 8, format = "f"))
    table_data[,"P_Slope"    ]  <- formattable(mm.means$Pr_Slope        , digits = 3, format = "f")
    table_data[,"R.sq"        ] <- formattable(mm.means$R2              , digits = 4, format = "f")
    table_data[,"AjustR.sq"   ] <- formattable(mm.means$Adj.R2          , digits = 4, format = "f")
    table_data[,"Sigma"       ] <- formattable(mm.means$Sigma           , digits = 5, format = "f")
    table_data[,"u_lof"      ]  <- formattable(mm.means$u_lof           , digits = 1, format = "f")
    table_data <- table_data[with(table_data, order(SensorTypes, Compounds)),]
    colnames(table_data)        <- c("Sensors","Compounds", "Interc.", "P(Interc.)", "Slope", "P(Slope)","R2","Ajust. R2","RMSE","u(lof)")
    # add units if they are in the table caption: table_data[1,] <- c("", "", ", V", "", ", V/ppb or v/ppm", "", "", "",", V/ppb or v/ppm", "ppb or ppm")
    row.names(table_data) <- NULL
    table_data <- table_data[,-which(colnames(table_data) == "Ajust. R2")]
    return(table_data)
}
Table_1Effect <- function(df,Sensor_type,Compounds, Outliers, Table_Number) {
    df <- df[which(df$Sensor_type == Sensor_type & df$Compounds == Compounds),]
    df <- df[c(1,3,4,5,6,7,8,9,10,2),]
    Sensor_names <- df[,1]
    df <- df[,c("Intercept", "s_Intercept", "Pr_Intercept", "Slope", "s_Slope", "Pr_Slope" , "R2", "Adj.R2", "Sigma", "u_lof" )]
    #Numbers.df <- apply(Numbers.df,c(1,2),as.numeric)
    #rbind(Numbers.df,colMeans(Numbers.df),SD(Numbers.df))
    if (is.null(Outliers)) df.means <- colMeans(df, na.rm = TRUE) else df.means <- colMeans(df[-which(Sensor_names %in% Outliers),], na.rm = TRUE)
    if (is.null(Outliers)) df.SD    <- SD(df      , na.rm = TRUE) else df.SD    <- SD(      df[-which(Sensor_names %in% Outliers),], na.rm = TRUE)
    table_data <- data.frame( SensorTypes = character(0)
                              , Intercept= character(0)
                              , P_Intercept = numeric(0)
                              , Slope = character(0)
                              , P_Slope = numeric(0)
                              , R.sq = numeric(0)
                              , AjustR.sq = numeric(0)
                              , Sigma = numeric(0)
                              , u_lof = numeric(0)
                              , stringsAsFactors = FALSE
                              ,  check.names = FALSE)
    table_data[1:length(Sensor_names),"SensorTypes"]  <- Sensor_names
    table_data[,"Intercept"   ] <- paste0(formattable(df$Intercept, digits=5, format = "f"), " ? ", formattable(df$s_Intercept, digits=5, format = "f"))
    table_data[,"P_Intercept"]  <- format(round(df$Pr_Intercept   , digits=3), digits= 3, trim = FALSE, scientific= FALSE)
    table_data[,"Slope"       ] <- paste0(formattable(df$Slope    , digits=7, format = "f"), " ? ", formattable(df$s_Slope    , digits=7, format = "f"))
    table_data[,"P_Slope"    ]  <- formattable(df$Pr_Slope        , digits=3, format = "f")
    table_data[,"R.sq"        ] <- formattable(df$R2              , digits=4, format = "f")
    table_data[,"AjustR.sq"   ] <- formattable(df$Adj.R2          , digits=4, format = "f")
    table_data[,"Sigma"       ] <- formattable(df$Sigma           , digits=6, format = "f")
    table_data[,"u_lof"      ]  <- formattable(df$u_lof           , digits=1, format = "f")
    colnames(table_data)        <- c("Sensors", "Interc.", "P(Interc.)", "Slope", "P(Slope)","R2","Ajust. R2","RMSE","u(lof)")
    table_data[nrow(table_data)+1,]  <- c("Means"
                                          , paste0(formattable(df.means[1], digits=5, format = "f"), " ? "
                                                   , formattable(df.SD[1], digits=5, format = "f"))
                                          , ""
                                          , paste0(formattable(df.means[4], digits=7, format = "f"), " ? "
                                                   , formattable(df.SD[4], digits=7, format = "f"))
                                          , ""
                                          , paste0(formattable(df.means[7], digits=4, format = "f"))
                                          , ""
                                          , paste0(formattable(df.means[9] , digits=7, format = "f"))
                                          , paste0(formattable(df.means[10] , digits=1, format = "f"))
    )
    table_data[nrow(table_data)+1,]  <- c("RSD", paste0(formattable(abs(df.SD[1]/df.means[1]), digits=3, format = "f")), ""
                                          , paste0(formattable(abs(df.SD[4]/df.means[4]) , digits=3, format = "f")), "", "", "", "", "" )
    row.names(table_data) <- NULL
    table_data <- table_data[,-which(colnames(table_data) == "Ajust. R2")]
    return(table_data)
}

#================================================================CR
### change names of files ====
### see http://stackoverflow.com/questions/10758965/how-do-i-rename-files-using-r#10759083
#================================================================CR
umxRenameFile <- function(baseFolder = "Finder", Missing = FALSE, findStr = NA, missingStr = NA, replaceStr = NA
                          , listPattern = NA, test = T, overwrite = F, Split = "_", Position = 2) {
    # If Missing = TRUE adding replaceStr at the positiona and split of the filename, If Missing = FALSE replacing findStr with replaceStr in the filenames
    # Split = "_", Position = 2 to lacate the position where to add test if Missing = TRUE
    # uppercase = u$1
    if (baseFolder == "Finder") {
        baseFolder = system(intern = T, "osascript -e 'tell application \"Finder\" to get the POSIX path of (target of front window as alias)'")
        message("Using front-most Finder window:", baseFolder)
    } else if (baseFolder == "") {
        baseFolder = paste(dirname(file.choose(new = FALSE)), "/", sep = "") ## choose a directory
        message("Using selected folder:", baseFolder)
    }
    if (is.na(listPattern)) {
        listPattern = findStr
    }
    a = list.files(baseFolder, pattern = listPattern)
    changed = 0
    if (!Missing) { # it is a replacement not a missing String
        message("found ", length(a), " possible files")
        for (fn in a) {
            findB = grepl(pattern = findStr, fn) # returns 1 if found
            cat(strsplit(fn, split = "_")[[1]][2], sep = "\n")
            if (findB) {
                fnew = gsub(findStr, replace = replaceStr, fn) # replace all instances
                if (test) {
                    message("would change ", fn, " to ", fnew)
                } else {
                    if ((!overwrite) & file.exists(paste(baseFolder, fnew, sep = ""))) {
                        message("renaming ", fn, " to ", fnew, "failed as already exists. To overwrite set T")
                    } else {
                        file.rename(file.path(baseFolder, fn), file.path(baseFolder, fnew))
                        changed = changed + 1;
                        message("renamed ", fn, " to ", fnew, ".")
                    }
                }
            }else{
                if (test) {
                    message(paste("bad file",fn))
                }
            }
        }
    }  else {
        # it is a missing String added at position 2 splitted with Split of the file name
        a = a[grep(pattern = replaceStr, x = a, invert = TRUE)]
        message("found ", length(a), " possible files")
        for (fn in a) {
            #cat(strsplit(fn, split = "_")[[1]][2], sep = "\n")
            fnew = gsub(pattern = strsplit(fn, split = "_")[[1]][1], replace = paste0(strsplit(fn, split = "_")[[1]][1],"_",replaceStr), fn) # replace all instances
            if (test) {
                message("would change ", fn, " to ", fnew)
            } else {
                if ((!overwrite) & file.exists(paste(baseFolder, fnew, sep = ""))) {
                    message("renaming ", fn, " to ", fnew, "failed as already exists. To overwrite set T")
                } else {
                    file.rename(file.path(baseFolder, fn), file.path(baseFolder, fnew))
                    changed = changed + 1;
                    message("renamed ", fn, " to ", fnew, ".")
                }
            }
        }
    }
    message("changed ", changed)
}
#================================================================CR
### Function checking if a value is within a range ====
#================================================================CR
is.between <- function(x, mini, maxi) {
    ## This function check if the value x is found within a and b.
    ## copyright: https://stat.ethz.ch/pipermail/r-help/2008-August/170749.html
    ## x: value to be checked
    ## mini: min of the range, usually min() can be used to automatise the process
    ## maxi: mas of the range, usually max() can be used to automatise the process
    (x - mini)  *  (maxi - x) > 0
}
#================================================================CR
### Function loading an RData file and returning it into a variable ====
#================================================================CR
loadRData <- function(fileName) {
    ## This function loads an RData file and returns it into a variable
    ## This function is particularly usefull if you don't know the name of the dataframe included in the .RData
    ## or if this name change from file to file.
    ## copyright: http://stackoverflow.com/questions/5577221/how-can-i-load-an-object-into-a-variable-name-that-i-specify-from-an-r-data-file
    ## fileName: file path of the RData file
    load(fileName)
    get(ls()[ls() != "fileName"])
}
# transform NaN into Na ====
nan.to.na <- function(x) {x[which(is.nan(x))] <- NA; return(x)}
#================================================================CR
# Checking if there are coordinates for the reference data separated by a comma and project if needed
#================================================================CR
get_Coord.Ref  <- function(Coordinates.chr, ShinyUpdate = FALSE, session = NULL, ID.Long = NULL, ID.Lat = NULL) { # feed back of coordinates into the ui
    # Coordinates.chr       : character string with long and lat coordinates separated with comma
    # ShinyUpdate           : logical default is FALSE, if TRUE update 2 Shiny select input with decimaldegree of LOng and Lat
    # ID.Long               : Shiny SelectInput ID for longitude
    # ID.Lat                : Shiny SelectInput ID for latitude
    # return a numeric vector with decimal degrees of long and lat coordinates after projection from degree-minute-second notation if needed
    # checking if the separator is ","
    if (grepl(pattern =  ",", x = Coordinates.chr)) {
        # Checking is the coordinates are in spherical or decimal format, projection to OpenStreet map
        if (grepl(pattern = paste0(c("N","S", "E", "W", "d"), collapse = "|" ), x = Coordinates.chr)) {
            # extract spherical coordinates
            Ref.coord_LON  <- unlist(strsplit(x = Coordinates.chr, split = ","))[1]
            Ref.coord_LAT  <- unlist(strsplit(x = Coordinates.chr, split = ","))[2]
            # transform spherical coordinates to decimal degrees for later projection
            Ref.coord_d    <- OSMscale::degree(Ref.coord_LAT, Ref.coord_LON, digits = 5)
            # Project the spherical coordinates in Mercator web WS84 of OPenStreet view - This is not needed, map correct without projection
            #Ref.coord_p    <- OSMscale::projectPoints(Ref.coord_d[1], Ref.coord_d[2], to=OSMscale::pll())
            Ref.coord_LAT  <- Ref.coord_d[1,1]
            Ref.coord_LON  <- Ref.coord_d[1,2]
            # updating coordinates of reference station
            if (ShinyUpdate) {
                updateTextInput(session = session,
                                inputId = ID.Long,
                                value   = Ref.coord_LON)
                updateTextInput(session = session,
                                inputId = ID.Lat,
                                value   = Ref.coord_LAT)}
        } else {
            Ref.coord_LON <- as.numeric(strsplit(x = Coordinates.chr, split = ",")[[1]][1])
            Ref.coord_LAT <- as.numeric(strsplit(x = Coordinates.chr, split = ",")[[1]][2])
        }
        # update Shiny Select input s
        return(paste0(Ref.coord_LON, ",", Ref.coord_LAT))
    } else {
        futile.logger::flog.warn("[get_Coord.Ref] ERROR Coordinates are not comma separated")
        return(paste0(NA,NA))}}
#================================================================CR
# DF_avg: Aggregation of Dataframe/datatable in datatable base on POSIXCt column====
#================================================================CR
#' @description aggregate/average data.table based on a POSIXct column.
#' Previous code used timeAverage which is slow and later RcppRoll::roll_mean which is faster but needs a complete dataframe with all date before computing.
#' Data.table is now used which is faster and easier
#' @param DF (necessary) data.table or dataframe to be aggregated, shall include the POSIXCt column "date" (default), or another name of POSIXct column used to aggregate
#' @param Cols.for.Avg  (optional) character vector, default is NULL. If not NULL the columns Cols.for.Avg in DF, adding keyDate, are returned. Do not include keyDate and be sure that the columns are somehow numeric.
#' @param width (optional) numeric, default is 60. Size in minutes of the time window used for averaging
#' @param keyDate (optional) chr, default is "date", name of the column of DF which hold the POSIXct/Date used for averaging
#' @param hour_start (optional) chr, default is "00:00", when width = 1440 (1 day), hour_start is the starting hour for the daily average, It can be useful when comparing reference PM measured by gravimetry.
#' @param BY (optional) character vector, default is "NULL. If not NULL, the aggregation is performed taking into consideration the column names given in BY. BY shall be in Cols.for.Avg or not but shall be included into DF
#' @param SameClass (optional) not used
#' @param Apply.Min.Perc (optional) logical default is FALSE. If FALSE, no data are discarded for incomplete data. If TRUE, Min.Perc% of data is applied to exclude rows without Min.Perc percentage of data in width
#' @param Min.Perc (optional) numeric default is 0.75 (75% as in the Air Quality Directive), minimum percentage of row of data that shall be include into width rows of DF 
#' @param Col.Min.Perc (optional) logical default is NULL If TRUE Min.Perc is applied to exclude rows without Apply.Min.Perc percentage of data in width
#' @param Agg.min (optional) numeric default is 1, minimum percentage of row of data that shall be include into width rows of DF 
#' to compute the average.
#' @return aggregated data.table starting on the first full hour discarding empty rows and transforming nan to na
DF_avg <- function(DF, Cols.for.Avg = NULL, width = 60L, keyDate = "date", SameClass = NULL, hour_start = "00:00", BY = NULL,
                   Apply.Min.Perc = FALSE, Min.Perc = 0.75, Col.Min.Perc = NULL, Agg.min = 1L) {
    
    # Copy DF to avoid changing it by reference
    DT <- data.table::copy(DF)
    
    # checking the size of window, at least two times the width
    if (exists("DT") && !is.null(DT) && difftime(max(DT$date, na.rm = T), min(DT$date, na.rm = T), units = "mins") > width * 2) {
        
        # Select columns
        if (!is.null(Cols.for.Avg)) {
            if(all(Cols.for.Avg %in% names(DT))) {
                
                # avoid adding date if already in Cols.for.Avg
                if (keyDate %in% Cols.for.Avg) Columns <- Cols.for.Avg else Columns <- c(keyDate, Cols.for.Avg)
                
                # Checking if any of the BY parameters is not included in Cols.for.Avg
                if(!is.null(BY)){
                    if (keyDate %in% BY) BY.no.date <- BY[!BY %in% keyDate] else BY.no.date <- BY
                    if (any(!BY.no.date %in% Cols.for.Avg)){
                        if(all(BY.no.date %in% names(DT))) Columns <- c(Columns, base::intersect(BY.no.date, names(DT))) else {
                            stop(futile.logger::flog.error(paste0("[DF_avg] column ", paste(setdiff(BY.no.date, names(DT)), collapse = ", "), " requested for BY not present in DF")))}}}
                
            } else stop(futile.logger::flog.error(paste0("[DF_avg] column ", paste(setdiff(Cols.for.Avg, names(DT)), collapse = ", "), " requested for Cols.for.Avg not present in DF")))
        } else {
            #Selecting only numeric or POSIX columns of DT
            Columns <- unique(names(DT)[sapply(DT, is.numeric) | sapply(DT, lubridate::is.POSIXct) | sapply(DT, lubridate::is.Date)])
            
            if(!is.null(BY)){
                
                # Checking if non numeric BY Column is missing
                if (keyDate %in% BY) BY.no.date <- BY[!BY %in% keyDate] else BY.no.date <- BY
                if (any(!BY.no.date %in% Columns)){
                    if(all(BY.no.date %in% names(DT))) Columns <- c(Columns, base::intersect(BY.no.date, names(DT))) else {
                        stop(futile.logger::flog.error(paste0("[DF_avg] column ", paste(setdiff(BY.no.date, names(DT)), collapse = ", "), " requested for BY not present in DF")))}}}}
        # Selecting
        DT <- DT[, ..Columns]
        
        # Changing date for averaging
        if (data.table::is.data.table(DT)) {
            #if (!haskey(DT)) setkey(DT, "date")
            if(width != 1440){
                DT[, Agg := lubridate::ceiling_date(DT[[keyDate]], unit = ifelse(width <= 60, paste0(width,"minute"), paste0(width/60, "hour")))]
            } else DT[, Agg := lubridate::ceiling_date(DT[[keyDate]] + lubridate::hm(hour_start), unit = "day")]
            DT[, (keyDate) := NULL]
            data.table::setnames(DT, c("Agg"), keyDate)
            data.table::setkeyv(DT, keyDate)
        } else {
            if (is.data.frame(DT)) {
                # Add aggregated time
                if(width != 1440){
                    DT$Agg <- lubridate::ceiling_date(DT[[keyDate]], unit = ifelse(width <= 60, paste0(width,"minute"), paste0(width/60, "hour")))
                } else DT$Agg <- lubridate::ceiling_date(DT[[keyDate]] + lubridate::hm(hour_start), unit = "day")
                
                # Rename Agg as date
                DT[[keyDate]] <- NULL
                names(DT) <- sub(pattern = "Agg", replacement = keyDate, x = names(DT))
                # convert to data time
                DT <- data.table::data.table(DT, key = keyDate)
            } else {
                cat("Unknow class of DF\n")
                return()}}
        
        # Aggregate mean of Aggreagted time
        # https://stackoverflow.com/questions/12603890/pass-column-name-in-data-table-using-variable
        date <- quote(get(keyDate))
        # https://stackoverflow.com/questions/26663053/getting-na-when-summarizing-by-columns-in-data-table
        if (!is.null(BY) && all(BY %in% names(DT))){
            if (keyDate %in% BY) all.BY <- BY else all.BY <- c(quote(get(keyDate)), BY)
            DT.Agg.DT <- DT[, lapply(.SD, mean, na.rm = TRUE), by = all.BY, .SDcols=Columns[grep(paste(all.BY, collapse = "|"), Columns, invert = T)]]
        }  else {
            if (!is.null(BY)) futile.logger::flog.warn(paste0("[DF_avg] Columns set in BY are not all included in DT, aggregation on date only. Available: ", intersect(BY, names(DT))))
            DT.Agg.DT <- DT[, lapply(.SD, mean, na.rm = TRUE), by = eval(date), .SDcols=Columns[grep(keyDate, Columns, invert = T)]]}
        
        # Discard date without less than min.Perc data if requested
        if (Apply.Min.Perc){
            # browser()
            if(is.null(Col.Min.Perc)){
                Dates2Delete <- unique(DT[which(DT[, .N, by = date]$N < width * Min.Perc)]$date)
            } else if (Agg.min == 1L) {
                # discard not complete rows for ..Col.Min.Perc
                DT.date <- DT[is.finite(rowSums(DT[,..Col.Min.Perc])), .N, by = date]
                # Dates to discard
                Dates2Delete <- DT.date[N < width * Min.Perc]$date}
            
            if (exists("Dates2Delete") && length(Dates2Delete) > 0) DT.Agg.DT <- DT.Agg.DT[-which(date %in% Dates2Delete)]}
        
        # Sorting by date
        DT.Agg.DT[, (keyDate) := date]
        # replace nan with NA
        DT.Agg.DT <- DT.Agg.DT[, lapply(.SD, nan.to.na)]
        return(DT.Agg.DT)
    } else {
        futile.logger::flog.warn(paste0("[DF_avg] the data table does not exist or is too short for averaging, width: ", width, ", number of rows: ", nrow(DT)))
        return(DT)   
    }
}

#================================================================CR
# DF_sd Determination of reference data with hour resolution or more====
#================================================================CR
#' @description Reference data with resolution 1 h or higher can not be filtered. The aim of this function is to determine such reference data by computing hourly standard deviations which must be zero. 
#' As a return we will replace the "Out.Ref" and "Lag.Out.Ref." values with values in "Ref."
#' Data.table is now used which is faster and easier
#' @param DF (necessary) data.table or dataframe, shall include the POSIXCt column "date" (default), or another name of POSIXct column used to aggregate.
#' @param Col.for.sd (necessary) Character. Name of the columns of DF to be checked for standard deviation of zero. Usually column of "Ref." 
#' @param width (optional) integer, default is 60L. 
#' @return  a list with element DT corresponding to DF with original reference data wherethese data have one hour periodicity and i.dates of dates in DF that have resolution equal to "width"
DF_sd <- function(DF, Col.for.sd, width = 60L) {
    
    # Copy DF to avoid changing it by reference
    DT <- data.table::copy(DF)
    # checking the size of window, at least two times the width
    if (exists("DT") && !is.null(DT) && difftime(max(DT$date, na.rm = T), min(DT$date, na.rm = T), units = "mins") > width * 2) {
        #Selecting only numeric or POSIX columns of DT
        Columns <- unique(names(DT)[sapply(DT, is.numeric) | sapply(DT, lubridate::is.POSIXct) | sapply(DT, lubridate::is.Date)])
        keyDate <- unique(names(DT)[sapply(DT, lubridate::is.POSIXct) | sapply(DT, lubridate::is.Date)])
        # Takes only "date" in case several columns are Date or POISIXct
        keyDate <- keyDate[ ifelse("date" %in% keyDate, which(keyDate == "date"), 1)]
        # Columns <- c(keyDate, Col.for.sd)
        # Selecting
        DT <- DT[, ..Columns]
        
        # Changing date for averaging
        if (data.table::is.data.table(DT)) {
            if(width != 1440){
                DT[, Agg := lubridate::ceiling_date(DT[[keyDate]], unit = ifelse(width <= 60, paste0(width,"minute"), paste0(width/60, "hour")))]
            } else DT[, Agg := lubridate::ceiling_date(DT[[keyDate]] + lubridate::hm(hour_start), unit = "day")]
            # DT[, (keyDate) := NULL]
            # data.table::setnames(DT, c("Agg"), keyDate)
            # data.table::setkeyv(DT, keyDate)
        } else {
            if (is.data.frame(DT)) {
                # Add aggregated time
                if(width != 1440){
                    DT$Agg <- lubridate::ceiling_date(DT[[keyDate]], unit = ifelse(width <= 60, paste0(width,"minute"), paste0(width/60, "hour")))
                } else DT$Agg <- lubridate::ceiling_date(DT[[keyDate]] + lubridate::hm(hour_start), unit = "day")
                
                # Rename Agg as date
                DT[[keyDate]] <- NULL
                names(DT) <- sub(pattern = "Agg", replacement = keyDate, x = names(DT))
                # convert to data time
                DT <- data.table::data.table(DT, key = keyDate)
            } else {
                cat("Unknow class of DF\n")
                return()}}
        
        # Aggregate sd of Aggregated time
        # https://stackoverflow.com/questions/26663053/getting-na-when-summarizing-by-columns-in-data-table
        for (REF in Col.for.sd) {
            
            # Select columns
            if (!REF %in% names(DT)) {
                stop(futile.logger::flog.error(paste0("[DF_sd] column ", paste(setdiff(REF, names(DT)), collapse = ", "), " requested for reference data with hour resolution or higher not present in DF")))
            } else {
                
                DT.Agg.DT <-   DT[, .SD, .SDcols =  c(keyDate, REF, 'Agg')][, lapply(.SD, sd, na.rm = TRUE), by = "Agg", .SDcols=c(REF)]
                
                data.table::setnames(DT.Agg.DT, REF, paste0('sd.', REF))
                
                # DT.Agg.DT <- unique(DT[, .SD, .SDcols = "Agg"])
                # DT.Agg.DT[, (paste0("sd.",REFs)) := sapply(DT.Agg.DT$Agg, function(Unique.Agg, DT) sd(DT[Agg == Unique.Agg][[REF]], na.rm = T), DT = DT)]
                # joining based on aggregated date
                DT <- DT[DT.Agg.DT, on= .(Agg = Agg)]
                
                # DT.Agg.DT <- DT[, lapply(.SD, sd, na.rm = TRUE), by = "Agg", .SDcols=Columns[grep(paste(c('Agg', keyDate), collapse = '|'), Columns, invert = T)]]
                # dates with sd = 0
                # browser()
                if (REF == "Ref.CO_ppm") i.dates <- which(DT[[paste0('sd.', REF)]] < 0.0001) else i.dates <- which(DT[[paste0('sd.', REF)]] < 0.001)
                # browser()
                if (length(i.dates) > 0) {
                    # replacing "Out.Ref" and "Lag.OUt.Ref" with "Ref"
                    data.table::set(DT, i = i.dates, j = paste0('Out.', REF),     value = DT[i.dates][[REF]])
                    data.table::set(DT, i = i.dates, j = paste0('Lag.Out.', REF), value = DT[i.dates][[REF]])
                    # removing sd.Ref column
                }
                DT <- DT[, paste0('sd.', REF) := NULL]
            }
        }
        # removing Agg
        DT <- DT[, Agg := NULL]
        
        DT[, (keyDate) := date]
        # replace nan with NA
        DT <- DT[, lapply(.SD, nan.to.na)]
        return(list(DT = DT, i.dates = i.dates))
    } else {
        futile.logger::flog.warn(paste0("[DF_sd] the data table does not exist or is too short for sd calculation, width: ", width, ", number of rows: ", nrow(DT)))
        return(list(DT = DT, i.dates = NULL))   
    }
}

#================================================================CR
# OS-independent way to select directory interactively in R
# https://stackoverflow.com/questions/48218491/os-independent-way-to-select-directory-interactively-in-r
#================================================================CR
# First a helper function to load packages, installing them first if necessary
# Returns logical value for whether successful
ensure_library = function (lib.name){
    x = require(lib.name, quietly = TRUE, character.only = TRUE)
    if (!x) {
        install.packages(lib.name, dependencies = TRUE, quiet = TRUE)
        x = require(lib.name, quietly = TRUE, character.only = TRUE)
    }
    x
}
select_directory_method = function() {
    # Tries out a sequence of potential methods for selecting a directory to find one that works
    # The fallback default method if nothing else works is to get user input from the console
    if (!exists('.dir.method')){  # if we already established the best method, just use that
        # otherwise lets try out some options to find the best one that works here
        if (exists('utils::choose.dir')) {
            .dir.method = 'choose.dir'
        } else if (rstudioapi::isAvailable() & rstudioapi::getVersion() > '1.1.287') {
            .dir.method = 'RStudioAPI'
            ensure_library('rstudioapi')
        } else if(ensure_library('tcltk') &
                  class(try({tt  <- tktoplevel(); tkdestroy(tt)}, silent = TRUE)) != "try-error") {
            .dir.method = 'tcltk'
        } else if (ensure_library('gWidgets2') & ensure_library('RGtk2')) {
            .dir.method = 'gWidgets2RGtk2'
        } else if (ensure_library('rJava') & ensure_library('rChoiceDialogs')) {
            .dir.method = 'rChoiceDialogs'
        } else {
            .dir.method = 'console'
        }
        assign('.dir.method', .dir.method, envir = .GlobalEnv) # remember the chosen method for later
    }
    return(.dir.method)
}
choose_directory = function(method = select_directory_method(), title = 'Select data directory') {
    # https://stackoverflow.com/questions/48218491/os-independent-way-to-select-directory-interactively-in-r
    switch (method,
            'choose.dir' = choose.dir(caption = title),
            'RStudioAPI' = selectDirectory(caption = title),
            'tcltk' = tk_choose.dir(caption = title),
            'rChoiceDialogs' = rchoose.dir(caption = title),
            'gWidgets2RGtk2' = gfile(type = 'selectdir', text = title),
            readline('Please enter directory path: ')
    )
}
choose_file = function(method = select_directory_method(), title = 'Select file') {
    # https://stackoverflow.com/questions/48218491/os-independent-way-to-select-directory-interactively-in-r
    switch (method,
            'choose.dir' = choose.file(caption = title),
            'RStudioAPI' = selectFile(caption = title),
            'tcltk' = tk_choose.files(caption = title),
            'rChoiceDialogs' = rchoose.files(caption = title),
            'gWidgets2RGtk2' = gfile(type = 'open', text = title),
            readline('Please enter file: ')
    )
}

# Standard errors for glmnet
# See reference at https://www.reddit.com/r/statistics/comments/1vg8k0/standard_errors_in_glmnet/

ridge_se <- function(xs,y,yhat,my_mod){
    # Note, you can't estimate an intercept here
    n <- dim(xs)[1]
    k <- dim(xs)[2]
    sigma_sq <- sum((y-yhat)^2)/ (n-k)
    lam <- my_mod$lambda.min
    if(is.null(my_mod$lambda.min)==TRUE){lam <- 0}
    i_lams <- Matrix(diag(x=1,nrow=k,ncol=k),sparse=TRUE)
    xpx <- t(xs)%*%xs
    xpxinvplam <- solve(xpx+lam*i_lams)
    var_cov <- sigma_sq * (xpxinvplam %*% xpx %*% xpxinvplam)
    se_bs <- sqrt(diag(var_cov))
    #print('NOTE: These standard errors are very biased.')
    return(se_bs)
}

# Compute R^2 from true and predicted values
rsquare <- function(true, predicted) {
    DataXY <- data.table::data.table(true = true, predicted = predicted)
    DataXY <- DataXY[is.finite(rowSums(DataXY))]
    mean.true <- mean(DataXY$true, na.rm = T)
    sse <- sum((DataXY$predicted - DataXY$true)^2)
    sst <- sum((DataXY$true - mean.true)^2)
    rsq <- 1 - sse / sst
    # For this post, impose floor...
    if (rsq < 0) rsq <- 0
    return (rsq)
}

# FIR smoothing
#' @param n filter order (1 less than the length of the filter
#' @param fs frequency used to sample phenomena ( n/10?)
FIR_y_T_RH <- function(General, Var, Verbose = FALSE, fs = 3, n = 30) {
    
    library(data.table)
    librarian::shelf(gsignal, cran_repo = "https://cran.r-project.org")
    librarian::shelf(ggplot2, cran_repo = "https://cran.r-project.org")
    
    # check taht variable are included into General
    stopifnot(Var %in% names(General))
    # stopifnot(name.y %in% names(General))
    # stopifnot(name.T %in% names(General))
    # stopifnot(name.RH %in% names(General))
    
    # Set variables
    t  <- General[["date"]]
    #  Create the filter
    h <- gsignal::fir1(n = n, w = 0.1/(fs / 2), type = "low") # create filter
    hn <- h / sum(h) # scale the filter by dividing by the sum of coefficients
    myzi <- gsignal::filter_zi(hn) # create the starting point values
    
    #for(Var in c(name.y, name.T, name.RH)){
    x1 <- General[[Var]]
    
    # apply filter with initial value
    assign(paste0("Smoothed.Delay", Var), gsignal::filter(hn, x1, myzi * x1[1]))
    # find the filter delay so it can be compensated for
    gd <- gsignal::grpdelay(hn)
    npts <- length(x1)
    delay <- round(mean(gd$gd))
    assign(paste0("Smoothed", Var), c(get(paste0("Smoothed.Delay", Var))$y[(delay +1):npts], rep(NA, delay))) # filtered data with delay compensation applied
    
    if(Verbose){
        # plot the data for visual inpection
        GG <- ggplot2::ggplot() + 
            geom_line(data = General, aes( x = date, y = .data[[Var]])) +
            #geom_line(data = data.frame(t = t, x2 = get(paste0("Smoothed.Delay", Var))$y), aes( x = t, y = x2), color = "blue") +
            geom_line(data = data.frame(t = t, x3 = get(paste0("Smoothed", Var))), aes( x = t, y = x3), color = "red")
        print(GG)}#}
    
    # Return the smoothed data
    # if (length(General$y) != length(get(paste0("Smoothed", Var)))) browser()
    Smoothed <- data.table::data.table(date = t)
    if (length(General[[Var]]) == length(get(paste0("Smoothed", Var)))) Smoothed[, (Var) := get(paste0("Smoothed", Var))] else Smoothed[, (Var) := get(paste0("Smoothed.Delay", Var))$y]
    # Smoothed[, (Var) := get(paste0("Smoothed", Var))]
    # Smoothed[, (name.y) := get(paste0("Smoothed", name.y))]
    # Smoothed[, (name.T) := get(paste0("Smoothed", name.T))]
    # Smoothed[, (name.RH) := get(paste0("Smoothed", name.RH))]
    # Replacing NA with the Var
    i.which <- Smoothed[,which(is.na(get(Var)))]
    if (length(i.which) > 0) {
        value.na <- baseline::baseline(as.matrix(t(General[i.which, .SD, .SDcols =  c(Var)])), hwm = floor(delay/2) - 1, method = "medianWindow")@baseline[1,] # floor(delay/2) gives warning when rounded to a higher number
        data.table::set(Smoothed, i = i.which, j = Var, value = value.na)}
    # data.table::set(Smoothed, i = i.which, j = Var, value = General[[Var]][i.which])
    
    return(Smoothed)
}

## Finds regions based on Sum_effect  and RH change
#' @param Daily.Matrice The data table where the regions will be determined
#' @param name.RH relative humidity
#' @param Last.RH_eff the last previously calculated RH effect to ensure the continuity of RH effect between regions
#' @param Last.Bline the last previously calculated baseline
#' @param Inc.Last.nls.model and Dec.Last.nls.model the last previously set RH increasing and decreasing models
#' @param thrs.DeltaRH and thrs.bline the threshold for the sum-effect and RH to deciding establish a new region. If lower than those, they will be combined with another region
Find.Region <- function(Daily.Matrice, name.RH, Last.RH_eff = NULL, Inc.Last.nls.model, Dec.Last.nls.model, thrs.DeltaRH, thrs.bline, Last.Bline) {
    # Ensuring the continuity of RH_eff
    if (length(which(abs(diff(Daily.Matrice$RH_eff)) > 0.5)) > 0) {
        # if (var(Regions$Sign.RH) != 0) {
        ## Determining where RH_eff jumps
        # RH.limits <- c(which(diff(sign(Daily.Matrice$dRH)) != 0)+1)
        RH.limits <- c(which(abs(diff(Daily.Matrice$RH_eff)) > 0.5)+1)
        RH.jumps <- data.table::data.table(Reg.Start = sapply(seq_along(RH.limits), function(k) {
            RH.limits[k]}))
        
        RH.jumps[, Reg.End := sapply(1:(nrow(RH.jumps)), function(k) {
            if(k != nrow(RH.jumps)) {
                RH.jumps$Reg.Start[k+1]-1
            } else nrow(Daily.Matrice)})]
        
        for (k in seq_along(RH.jumps$Reg.Start)) {
            val2change  <- Daily.Matrice$RH_eff[RH.jumps$Reg.Start[k]] - Daily.Matrice$RH_eff[RH.jumps$Reg.Start[k]-1]
            data.table::set(Daily.Matrice, i = RH.jumps$Reg.Start[k]:RH.jumps$Reg.End[k], j = 'RH_eff', value = Daily.Matrice$RH_eff[RH.jumps$Reg.Start[k]:RH.jumps$Reg.End[k]] - val2change)
        }
        #updating the Sum_eff
        data.table::set(Daily.Matrice, j="Sum_eff", value = Daily.Matrice$T_eff + Daily.Matrice$RH_eff)
    }
    # adding d(sum_eff)
    data.table::set(Daily.Matrice, j="d.Sum_eff", value = diff(c(Daily.Matrice$Sum_eff, NA)))
    # replacing NA with the last value
    data.table::set(Daily.Matrice, i = which(is.na(Daily.Matrice$d.Sum_eff)), j="d.Sum_eff", value = Daily.Matrice$d.Sum_eff[which(is.na(Daily.Matrice$d.Sum_eff))-1] )
    ######################################################### CHANGED ##################################################################################################################
    # if (as.Date('2020-06-02') %in% unique(as.Date(Daily.Matrice$date)) || as.Date('2020-06-04') %in% unique(as.Date(Daily.Matrice$date)) ||
    #     as.Date('2020-06-06') %in% unique(as.Date(Daily.Matrice$date)) || as.Date('2020-06-07') %in% unique(as.Date(Daily.Matrice$date))) browser()
    region.Limits <- c(1, which(diff(sign(Daily.Matrice$d.Sum_eff)) != 0), which(diff(sign(abs(Daily.Matrice$d.Sum_eff) > 1.5)) != 0), which(diff(sign(abs(Daily.Matrice$dRH) > 0.40)) != 0)) 
    if (!nrow(Daily.Matrice) %in% region.Limits) region.Limits  <- c(region.Limits, nrow(Daily.Matrice))
    
    region.Limits <- sort(unique(region.Limits))
    Regions <- unique(data.table::data.table(Reg.Start = sapply(seq_along(region.Limits), function(k) {
        if (k == 1L || (k == length(region.Limits) && region.Limits[k] == nrow(Daily.Matrice))){
            region.Limits[k]}
        else region.Limits[k]+1})))
    Regions[, Reg.End := sapply(1:(nrow(Regions)), function(k) {
        if(!k %in% c(nrow(Regions)-1, nrow(Regions))){
            Regions$Reg.Start[k+1]-1
        } else if(k %in% c(nrow(Regions)-1, nrow(Regions))) Regions$Reg.Start[nrow(Regions)]})]
    # Dropping last row of regions which has always duration of 0 minute
    Regions <- Regions[-nrow(Regions)]
    Regions[, Reg.dur    := Regions$Reg.End - Regions$Reg.Start]
    Regions[, Delta.RH   := sapply(seq_along(Regions$Reg.Start), function (k) diff(range(na.omit(Daily.Matrice[[name.RH]][Regions[k]$Reg.Start:Regions[k]$Reg.End]))))]
    Regions[, mean.dRH   := sapply(seq_along(Regions$Reg.Start), function (k) mean(Daily.Matrice$dRH[Regions[k]$Reg.Start:Regions[k]$Reg.End], na.rm = T))]
    Regions[, Delta.Sum_eff   := sapply(seq_along(Regions$Reg.Start), function (k) diff(range(na.omit(Daily.Matrice[['Sum_eff']][Regions[k]$Reg.Start:Regions[k]$Reg.End]))))]
    Regions[, mean.dSumEff   := sapply(seq_along(Regions$Reg.Start), function (k) mean(Daily.Matrice$d.Sum_eff[Regions[k]$Reg.Start:Regions[k]$Reg.End], na.rm = T))]
    Regions[, Sign.RH    := sapply(seq_along(Regions$Reg.Start), function (k){
        if(length(which(Daily.Matrice[Regions[k]$Reg.Start:Regions[k]$Reg.End]$dRH > 0)) > length(Regions[k]$Reg.Start:Regions[k]$Reg.End)/2) return(1) else return(-1)})]
    # Adjusting the RH_eff in the afternoon to ensure the continuity
    if (!is.null(Last.RH_eff)) {
        data.table::set(Daily.Matrice, j = "RH_eff",  value = Daily.Matrice$RH_eff + Last.RH_eff - Daily.Matrice$RH_eff[1])
    }
    # Managing the rows of DT Regions with only one index Likely this does not take place anymore because we added unique(regions.Limits)
    if (any(Regions$Reg.dur < 15)) {  
        for (Reg in which(Regions$Reg.dur < 15)) {
            if (Reg == 1L || all(is.na(Regions$Reg.dur[1:Reg-1]))) {
                if (Reg == nrow(Regions)) {
                    next
                } else {
                    data.table::set(Regions, i = as.integer(Reg+1), j = "Reg.Start",  value = Regions[Reg]$Reg.Start)
                    data.table::set(Regions, i = as.integer(Reg+1), j = "Reg.dur",  value = Regions[Reg+1]$Reg.End - Regions[Reg+1]$Reg.Start)
                    data.table::set(Regions, i = as.integer(Reg + 1), j ="Sign.RH", 
                                    value = ifelse(length(which(Daily.Matrice[Regions[Reg+1]$Reg.Start:Regions[Reg+1]$Reg.End]$dRH > 0)) > length(Regions[Reg+1]$Reg.Start:Regions[Reg+1]$Reg.End)/2, 1, -1))
                    data.table::set(Regions, i = as.integer(Reg), j = "Reg.dur", value = NA)
                    # Checking if the RH.sign of the combined new region is the same of the next region. if so, they will be merged
                    # if (Reg + 2 <= nrow(Regions) && Regions$Sign.RH[Reg + 1] == Regions$Sign.RH[Reg + 2]) {
                    #     data.table::set(Regions, i = as.integer(Reg+1), j = "Reg.End",  value = Regions[Reg+2]$Reg.End)
                    #     data.table::set(Regions, i = as.integer(Reg+1), j = "Reg.dur",  value = Regions[Reg+1]$Reg.End - Regions[Reg+1]$Reg.Start)
                    #     data.table::set(Regions, i = as.integer(Reg+1), j ="Sign.RH",
                    #                     value = ifelse(length(which(Daily.Matrice[Regions[Reg+1]$Reg.Start:Regions[Reg+1]$Reg.End]$dRH > 0)) > length(Regions[Reg+1]$Reg.Start:Regions[Reg+1]$Reg.End)/2, 1, -1))
                    #     data.table::set(Regions, i = as.integer(Reg + 2), j = "Reg.dur", value = NA)
                    # }
                    # 
                }
            } else {
                Reg.min <- as.integer(max(which(!is.na(Regions$Reg.dur[1:Reg-1]))))
                data.table::set(Regions, i = Reg.min, j = "Reg.End",  value = Regions[Reg]$Reg.End)
                data.table::set(Regions, i = Reg.min, j = "Reg.dur",  value = Regions[Reg.min]$Reg.End - Regions[Reg.min]$Reg.Start)
                data.table::set(Regions, i = Reg.min, j ="Sign.RH", 
                                value = ifelse(length(which(Daily.Matrice[Regions[Reg.min]$Reg.Start:Regions[Reg.min]$Reg.End]$dRH > 0)) > length(Regions[Reg.min]$Reg.Start:Regions[Reg.min]$Reg.End)/2, 1, -1))
                data.table::set(Regions, i = Reg, j = "Reg.dur", value = NA)
                # Checking if the RH.sign of the combined new region is the same of the next region. if so, they will be merged
                # if (Reg + 1 <= nrow(Regions) && Regions$Sign.RH[Reg.min] == Regions$Sign.RH[Reg + 1]) {
                #     data.table::set(Regions, i = Reg.min, j = "Reg.End",  value = Regions[Reg+1]$Reg.End)
                #     data.table::set(Regions, i = Reg.min, j = "Reg.dur",  value = Regions[Reg.min]$Reg.End - Regions[Reg.min]$Reg.Start)
                #     data.table::set(Regions, i = Reg.min, j ="Sign.RH",
                #                     value = ifelse(length(which(Daily.Matrice[Regions[Reg.min]$Reg.Start:Regions[Reg.min]$Reg.End]$dRH > 0)) > length(Regions[Reg.min]$Reg.Start:Regions[Reg.min]$Reg.End)/2, 1, -1))
                #     data.table::set(Regions, i = as.integer(Reg + 1), j = "Reg.dur", value = NA)
                # }
            }
        }
    }
    
    Regions <- na.omit(Regions)
    if (nrow(Regions) > 1) {
        for (row.reg in seq_along(Regions$Reg.Start)) {
            # if (as.Date('2021-02-04') %in% unique(as.Date(Daily.Matrice$date))) browser()
            if (is.na(Regions[row.reg]$Reg.dur) || nrow(na.omit(Regions)) == 1) next
            # 1st region
            if (Regions[row.reg]$Reg.Start == 1) {
                # Short duration or low RH change within 1st and 2nd regions. combining with the next region
                if (Regions[row.reg]$Delta.Sum_eff < thrs.bline) { # ||
                    # Regions[row.reg]$Sign.RH == Regions[row.reg+1]$Sign.RH) {
                    data.table::set(Regions, i = as.integer(row.reg + 1), j = "Reg.Start", value = Regions$Reg.Start[row.reg])
                    Regions[as.integer(row.reg + 1), Reg.dur  := Regions[row.reg + 1]$Reg.End - Regions[row.reg + 1]$Reg.Start +1]
                    Regions[as.integer(row.reg + 1), mean.dRH := mean(Daily.Matrice$dRH[Regions[row.reg + 1]$Reg.Start:Regions[row.reg + 1]$Reg.End])]
                    Regions[as.integer(row.reg + 1), Delta.RH := diff(range(Daily.Matrice[[name.RH]][Regions[row.reg + 1]$Reg.Start:Regions[row.reg + 1]$Reg.End], na.rm = T))]
                    Regions[as.integer(row.reg + 1), Delta.Sum_eff := diff(range(Daily.Matrice[["Sum_eff"]][Regions[row.reg + 1]$Reg.Start:Regions[row.reg + 1]$Reg.End], na.rm = T))]
                    data.table::set(Regions, i = as.integer(row.reg + 1), j ="Sign.RH", 
                                    value = ifelse(length(which(Daily.Matrice[Regions[row.reg+1]$Reg.Start:Regions[row.reg+1]$Reg.End]$dRH > 0)) > length(Regions[row.reg+1]$Reg.Start:Regions[row.reg+1]$Reg.End)/2, 1, -1))
                    data.table::set(Regions, i = as.integer(row.reg), j = "Reg.dur", value = NA)
                    # } else if (row.reg != nrow(Regions) && Regions[row.reg]$Sign.RH == Regions[row.reg+1]$Sign.RH && 
                    #            Regions[row.reg+1]$Delta.Sum_eff < thrs.bline) {
                    #     data.table::set(Regions, i = as.integer(row.reg + 1), j = "Reg.Start", value = Regions$Reg.Start[row.reg])
                    #     Regions[as.integer(row.reg + 1), Reg.dur  := Regions[row.reg + 1]$Reg.End - Regions[row.reg + 1]$Reg.Start +1]
                    #     Regions[as.integer(row.reg + 1), mean.dRH := mean(Daily.Matrice$dRH[Regions[row.reg + 1]$Reg.Start:Regions[row.reg + 1]$Reg.End])]
                    #     Regions[as.integer(row.reg + 1), Delta.RH := diff(range(Daily.Matrice[[name.RH]][Regions[row.reg + 1]$Reg.Start:Regions[row.reg + 1]$Reg.End]))]
                    #     Regions[as.integer(row.reg + 1), Delta.Sum_eff := diff(range(Daily.Matrice[["Sum_eff"]][Regions[row.reg + 1]$Reg.Start:Regions[row.reg + 1]$Reg.End]))]
                    #     data.table::set(Regions, i = as.integer(row.reg + 1), j ="Sign.RH", 
                    #                     value = ifelse(length(which(Daily.Matrice[Regions[row.reg+1]$Reg.Start:Regions[row.reg+1]$Reg.End]$dRH > 0)) > length(Regions[row.reg+1]$Reg.Start:Regions[row.reg+1]$Reg.End)/2, 1, -1))
                    #     data.table::set(Regions, i = as.integer(row.reg), j = "Reg.dur", value = NA)
                } else next
            } else {  # Short duration and >= 2nd region. We add to the last non-NA region
                # finding the previous non-NA region
                i.min <- as.integer(max(which(!is.na(Regions$Reg.dur[1:row.reg-1]))))
                # Checking if the variation of RH and SumEff in the region is too low
                if (Regions[row.reg]$Delta.Sum_eff < thrs.bline) {
                    if ( # Regions[row.reg]$Sign.RH == Regions[i.min]$Sign.RH || #Checking the sign of the previous region being the same of region of interest. If so, the region will be merged with the previous region
                        Regions[i.min]$Delta.Sum_eff < thrs.bline) { # Delta.RH and Delta.SumEffect of previous region are below threshold
                        data.table::set(Regions, i = row.reg, j = "Reg.Start", value = Regions$Reg.Start[i.min])
                        data.table::set(Regions, i = row.reg, j = "Reg.dur", value = Regions[row.reg]$Reg.End - Regions[row.reg]$Reg.Start +1)
                        data.table::set(Regions, i = row.reg, j ="Sign.RH", 
                                        value = ifelse(length(which(Daily.Matrice[Regions[row.reg]$Reg.Start:Regions[row.reg]$Reg.End]$dRH > 0)) > length(Regions[row.reg]$Reg.Start:Regions[row.reg]$Reg.End)/2, 1, -1))
                        data.table::set(Regions, i = row.reg, j = "Delta.RH", value = diff(range(Daily.Matrice[[name.RH]][Regions[row.reg]$Reg.End:Regions[row.reg]$Reg.Start], na.rm = T)))
                        data.table::set(Regions, i = row.reg, j = "mean.dRH", value = mean(Daily.Matrice$dRH[Regions[row.reg]$Reg.End:Regions[row.reg]$Reg.Start], na.rm = T))
                        data.table::set(Regions, i = row.reg, j = "Delta.Sum_eff", value = diff(range(Daily.Matrice[['Sum_eff']][Regions[row.reg]$Reg.End:Regions[row.reg]$Reg.Start], na.rm = T)))
                        data.table::set(Regions, i = i.min, j = "Reg.dur", value = NA)
                    } else if (row.reg != nrow(Regions) && (Regions[row.reg+1]$Delta.Sum_eff < thrs.bline)) { # checking if both delta RH and delta sum effect next region below threshold, OR, 
                        # ||  Regions[row.reg]$Sign.RH == Regions[row.reg+1]$Sign.RH)) { # the signs of both regions are the same
                        data.table::set(Regions, i = row.reg, j = "Reg.End", value = Regions$Reg.End[row.reg+1])
                        data.table::set(Regions, i = row.reg, j = "Reg.dur", value = Regions[row.reg]$Reg.End - Regions[row.reg]$Reg.Start +1)
                        data.table::set(Regions, i = row.reg, j ="Sign.RH", 
                                        value = ifelse(length(which(Daily.Matrice[Regions[row.reg]$Reg.Start:Regions[row.reg]$Reg.End]$dRH > 0)) > length(Regions[row.reg]$Reg.Start:Regions[row.reg]$Reg.End)/2, 1, -1))
                        data.table::set(Regions, i = row.reg, j = "Delta.RH", 
                                        value = diff(range(Daily.Matrice[[name.RH]][Regions[row.reg]$Reg.End:Regions[row.reg]$Reg.Start], na.rm = T)))
                        data.table::set(Regions, i = row.reg, j = "mean.dRH", value = mean(Daily.Matrice$dRH[Regions[row.reg]$Reg.End:Regions[row.reg]$Reg.Start], na.rm = T))
                        data.table::set(Regions, i = row.reg, j = "Delta.Sum_eff", 
                                        value = diff(range(Daily.Matrice[['Sum_eff']][Regions[row.reg]$Reg.End:Regions[row.reg]$Reg.Start], na.rm = T)))
                        data.table::set(Regions, i = as.integer(row.reg+1), j = "Reg.dur", value = NA)
                    } else { # we will combine with the region least RH and SumEff variable
                        if (row.reg != nrow(Regions)) {
                            i.min.sumeff <- Regions[Delta.Sum_eff == min(Regions$Delta.Sum_eff[i.min], Regions$Delta.Sum_eff[row.reg+1]), which = T]
                            #  if by case returns 2 values (one must be NA)
                            if (length(i.min.sumeff) > 1) i.min.sumeff <- i.min.sumeff[which(!is.na(Regions$Reg.dur[i.min.sumeff]))]
                        } else i.min.sumeff <- i.min
                        if (i.min.sumeff < row.reg) {
                            data.table::set(Regions, i = row.reg, j = "Reg.Start", value = Regions$Reg.Start[i.min.sumeff])
                        } else {
                            data.table::set(Regions, i = row.reg, j = "Reg.End", value = Regions$Reg.End[i.min.sumeff])
                        }
                        data.table::set(Regions, i = as.integer(i.min.sumeff), j = "Reg.dur", value = NA)
                        data.table::set(Regions, i = row.reg, j = "Reg.dur", value = Regions[row.reg]$Reg.End - Regions[row.reg]$Reg.Start +1)
                        data.table::set(Regions, i = row.reg, j ="Sign.RH", 
                                        value = ifelse(length(which(Daily.Matrice[Regions[row.reg]$Reg.Start:Regions[row.reg]$Reg.End]$dRH > 0)) > length(Regions[row.reg]$Reg.Start:Regions[row.reg]$Reg.End)/2, 1, -1))
                        data.table::set(Regions, i = row.reg, j = "Delta.RH", value = diff(range(Daily.Matrice[[name.RH]][Regions[row.reg]$Reg.End:Regions[row.reg]$Reg.Start], na.rm = T)))
                        data.table::set(Regions, i = row.reg, j = "mean.dRH", value = mean(Daily.Matrice$dRH[Regions[row.reg]$Reg.End:Regions[row.reg]$Reg.Start], na.rm = T))
                        data.table::set(Regions, i = row.reg, j = "Delta.Sum_eff", value = diff(range(Daily.Matrice[['Sum_eff']][Regions[row.reg]$Reg.End:Regions[row.reg]$Reg.Start], na.rm = T)))
                        
                    }
                } 
                # if (Regions$Sign.RH[i.min] == Regions$Sign.RH[row.reg]) {
                #     data.table::set(Regions, i = row.reg, j = "Reg.Start", value = Regions$Reg.Start[i.min])
                #     data.table::set(Regions, i = row.reg, j = "Reg.dur", value = Regions[row.reg]$Reg.End - Regions[row.reg]$Reg.Start +1)
                #     data.table::set(Regions, i = row.reg, j ="Sign.RH",
                #                     value = ifelse(length(which(Daily.Matrice[Regions[row.reg]$Reg.Start:Regions[row.reg]$Reg.End]$dRH > 0)) > length(Regions[row.reg]$Reg.Start:Regions[row.reg]$Reg.End)/2, 1, -1))
                #     data.table::set(Regions, i = row.reg, j = "Delta.RH", value = diff(range(Daily.Matrice[[name.RH]][Regions[row.reg]$Reg.End:Regions[row.reg]$Reg.Start])))
                #     data.table::set(Regions, i = row.reg, j = "mean.dRH", value = mean(Daily.Matrice$dRH[Regions[row.reg]$Reg.End:Regions[row.reg]$Reg.Start]))
                #     data.table::set(Regions, i = row.reg, j = "Delta.Sum_eff", value = diff(range(Daily.Matrice[['Sum_eff']][Regions[row.reg]$Reg.End:Regions[row.reg]$Reg.Start])))
                #     data.table::set(Regions, i = i.min, j = "Reg.dur", value = NA)
                # }
                if (Regions$Reg.dur[row.reg] < 30) {
                    if (row.reg == nrow(Regions)) {
                        if (all(is.na(Regions$Reg.dur[1:row.reg-1])))  {
                            next
                        } else {
                            i.min <- as.integer(max(which(!is.na(Regions$Reg.dur[1:row.reg-1]))))
                            # data.table::set(Regions, i = row.reg, j = "Sign.RH", 
                            #                 value = Regions[max(which(Regions[1:(row.reg)]$Reg.dur == max(Regions[i.min:(row.reg)]$Reg.dur, na.rm = T)))]$Sign.RH) 
                            data.table::set(Regions, i = row.reg, j = "Reg.Start", value = Regions$Reg.Start[i.min])
                            data.table::set(Regions, i = row.reg, j = "Reg.dur", value = Regions[row.reg]$Reg.End - Regions[row.reg]$Reg.Start +1)
                            data.table::set(Regions, i = row.reg, j ="Sign.RH", 
                                            value = ifelse(length(which(Daily.Matrice[Regions[row.reg]$Reg.Start:Regions[row.reg]$Reg.End]$dRH > 0)) > length(Regions[row.reg]$Reg.Start:Regions[row.reg]$Reg.End)/2, 1, -1))
                            data.table::set(Regions, i = row.reg, j = "Delta.RH", value = diff(range(Daily.Matrice[[name.RH]][Regions[row.reg]$Reg.End:Regions[row.reg]$Reg.Start], na.rm = T)))
                            data.table::set(Regions, i = row.reg, j = "mean.dRH", value = mean(Daily.Matrice$dRH[Regions[row.reg]$Reg.End:Regions[row.reg]$Reg.Start], na.rm = T))
                            data.table::set(Regions, i = row.reg, j = "Delta.Sum_eff", value = diff(range(Daily.Matrice[['Sum_eff']][Regions[row.reg]$Reg.End:Regions[row.reg]$Reg.Start], na.rm = T)))
                            data.table::set(Regions, i = i.min, j = "Reg.dur", value = NA)
                        }
                    } else if (all(is.na(Regions$Reg.dur[1:row.reg-1])) && !is.na(Regions$Reg.dur[row.reg+1])) {
                        # data.table::set(Regions, i = as.integer(row.reg+1), j = "Sign.RH", 
                        #                 value = Regions[max(which(Regions[1:(row.reg+1)]$Reg.dur == max(Regions[row.reg:(row.reg+1)]$Reg.dur, na.rm = T)))]$Sign.RH) 
                        data.table::set(Regions, i = as.integer(row.reg+1), j = "Reg.Start", value = Regions$Reg.Start[row.reg])
                        data.table::set(Regions, i = as.integer(row.reg+1), j = "Reg.dur", value = Regions[row.reg+1]$Reg.End - Regions[row.reg+1]$Reg.Start +1)
                        data.table::set(Regions, i = as.integer(row.reg + 1), j ="Sign.RH", 
                                        value = ifelse(length(which(Daily.Matrice[Regions[row.reg+1]$Reg.Start:Regions[row.reg+1]$Reg.End]$dRH > 0)) > length(Regions[row.reg+1]$Reg.Start:Regions[row.reg+1]$Reg.End)/2, 1, -1))
                        data.table::set(Regions, i = as.integer(row.reg+1), j = "Delta.RH", 
                                        value = diff(range(Daily.Matrice[[name.RH]][Regions[row.reg+1]$Reg.End:Regions[row.reg+1]$Reg.Start], na.rm = T)))
                        data.table::set(Regions, i = as.integer(row.reg+1), j = "mean.dRH", 
                                        value = mean(Daily.Matrice$dRH[Regions[row.reg+1]$Reg.End:Regions[row.reg+1]$Reg.Start], na.rm = T))
                        data.table::set(Regions, i = as.integer(row.reg+1), j = "Delta.Sum_eff", 
                                        value = diff(range(Daily.Matrice[['Sum_eff']][Regions[row.reg+1]$Reg.End:Regions[row.reg+1]$Reg.Start], na.rm = T)))
                        data.table::set(Regions, i = row.reg, j = "Reg.dur", value = NA)
                    } else if (all(is.na(Regions$Reg.dur[1:row.reg-1])) && is.na(Regions$Reg.dur[row.reg+1])) {
                        next
                    } else if (!all(is.na(Regions$Reg.dur[1:row.reg-1])) && !is.na(Regions$Reg.dur[row.reg+1])) {
                        i.min <- as.integer(max(which(!is.na(Regions$Reg.dur[1:row.reg-1]))))
                        if (Regions$Sign.RH[i.min] == Regions$Sign.RH[row.reg] || # in case the signs are the same, OR,
                            Regions$Sign.RH[row.reg] != Regions$Sign.RH[row.reg+1]) { # in case signs of the next region is different
                            data.table::set(Regions, i = row.reg, j = "Reg.Start", value = Regions$Reg.Start[i.min])
                            data.table::set(Regions, i = row.reg, j = "Reg.dur", value = Regions[row.reg]$Reg.End - Regions[row.reg]$Reg.Start +1)
                            data.table::set(Regions, i = row.reg, j ="Sign.RH", 
                                            value = ifelse(length(which(Daily.Matrice[Regions[row.reg]$Reg.Start:Regions[row.reg]$Reg.End]$dRH > 0)) > length(Regions[row.reg]$Reg.Start:Regions[row.reg]$Reg.End)/2, 1, -1))
                            data.table::set(Regions, i = row.reg, j = "mean.dRH", value = mean(Daily.Matrice$dRH[Regions[row.reg]$Reg.End:Regions[row.reg]$Reg.Start], na.rm = T))
                            data.table::set(Regions, i = row.reg, j = "Delta.RH", value = diff(range(Daily.Matrice[[name.RH]][Regions[row.reg]$Reg.End:Regions[row.reg]$Reg.Start], na.rm = T)))
                            data.table::set(Regions, i = row.reg, j = "Delta.Sum_eff", value = diff(range(Daily.Matrice[['Sum_eff']][Regions[row.reg]$Reg.End:Regions[row.reg]$Reg.Start], na.rm = T)))
                            data.table::set(Regions, i = i.min, j = "Reg.dur", value = NA)
                        } else { #if (Regions$Sign.RH[row.reg] == Regions$Sign.RH[row.reg+1]) { in case signs of the next region is the same
                            data.table::set(Regions, i = row.reg, j = "Sign.RH", value = Regions$Sign.RH[row.reg]) 
                            data.table::set(Regions, i = row.reg, j = "Reg.End", value = Regions$Reg.End[row.reg+1])
                            data.table::set(Regions, i = row.reg, j = "Reg.dur", value = Regions[row.reg]$Reg.End - Regions[row.reg]$Reg.Start +1)
                            data.table::set(Regions, i = row.reg, j = "Delta.RH", 
                                            value = diff(range(Daily.Matrice[[name.RH]][Regions[row.reg]$Reg.End:Regions[row.reg]$Reg.Start], na.rm = T)))
                            data.table::set(Regions, i = row.reg, j = "mean.dRH", value = mean(Daily.Matrice$dRH[Regions[row.reg]$Reg.End:Regions[row.reg]$Reg.Start], na.rm = T))
                            data.table::set(Regions, i = row.reg, j = "Delta.Sum_eff", 
                                            value = diff(range(Daily.Matrice[['Sum_eff']][Regions[row.reg]$Reg.End:Regions[row.reg]$Reg.Start], na.rm = T)))
                            data.table::set(Regions, i = as.integer(row.reg+1), j = "Reg.dur", value = NA)
                        } 
                    }  else if (!all(is.na(Regions$Reg.dur[1:row.reg-1])) && is.na(Regions$Reg.dur[row.reg+1])) {
                        if (row.reg + 1 == nrow(Regions)) {
                            i.min <- as.integer(max(which(!is.na(Regions$Reg.dur[1:row.reg-1]))))
                            # data.table::set(Regions, i = row.reg, j = "Sign.RH", 
                            #                 value = Regions[max(which(Regions[1:(row.reg)]$Reg.dur == max(Regions[i.min:(row.reg)]$Reg.dur, na.rm = T)))]$Sign.RH) 
                            data.table::set(Regions, i = row.reg, j = "Reg.Start", value = Regions$Reg.Start[i.min])
                            data.table::set(Regions, i = row.reg, j = "Reg.dur", value = Regions[row.reg]$Reg.End - Regions[row.reg]$Reg.Start +1)
                            data.table::set(Regions, i = row.reg, j ="Sign.RH", 
                                            value = ifelse(length(which(Daily.Matrice[Regions[row.reg]$Reg.Start:Regions[row.reg]$Reg.End]$dRH > 0)) > length(Regions[row.reg]$Reg.Start:Regions[row.reg]$Reg.End)/2, 1, -1))
                            data.table::set(Regions, i = row.reg, j = "Delta.RH", value = diff(range(Daily.Matrice[[name.RH]][Regions[row.reg]$Reg.End:Regions[row.reg]$Reg.Start], na.rm = T)))
                            data.table::set(Regions, i = row.reg, j = "mean.dRH", value = mean(Daily.Matrice$dRH[Regions[row.reg]$Reg.End:Regions[row.reg]$Reg.Start], na.rm = T))
                            data.table::set(Regions, i = row.reg, j = "Delta.Sum_eff", value = diff(range(Daily.Matrice[['Sum_eff']][Regions[row.reg]$Reg.End:Regions[row.reg]$Reg.Start], na.rm = T)))
                            data.table::set(Regions, i = i.min, j = "Reg.dur", value = NA)
                        } 
                    }
                }
            } 
        }
        Regions <- na.omit(Regions)
    }
    
    Regions[, RH.chg := sapply(seq_along(Regions$Reg.Start), function(ii) {
        if (length(which(Daily.Matrice[Regions[ii]$Reg.Start:Regions[ii]$Reg.End]$dRH > 0)) > length(Regions[ii]$Reg.Start:Regions[ii]$Reg.End)/2) {
            RH.chg <- "incr"
        } else RH.chg <- "decr"
    })]
    ## Estimating baseline using previous model 
    for (row.reg in seq_along(Regions$RH.chg)) {
        i.reg <- Regions$Reg.Start[row.reg]:Regions$Reg.End[row.reg]
        if (Regions$RH.chg[row.reg] == 'incr' && !is.null(Inc.Last.nls.model)) {
            pseu.bline <- predict(Inc.Last.nls.model, newdata = Daily.Matrice[i.reg])
        } else if (!is.null(Dec.Last.nls.model)) {
            pseu.bline <- predict(Dec.Last.nls.model, newdata = Daily.Matrice[i.reg])
        } else { # calculating pseudo-baseline using Sum_eff passing through the min(y_MA) of the region
            pseu.bline <- Daily.Matrice[i.reg]$Sum_eff - (Daily.Matrice[i.reg][which(y_MA == min(y_MA, na.rm = T))]$Sum_eff - min(Daily.Matrice[i.reg]$y_MA, na.rm = T))
        }
        data.table::set(Daily.Matrice, i = i.reg, j ='pseu.bline', value = pseu.bline)
        data.table::set(Regions, i = row.reg, j ='Delta.pseu.bline', value = diff(range(Daily.Matrice$pseu.bline[i.reg])))
        # Finding the best variable among Sum_eff and pseu.bline resulting in higher R2 between y_MA and variable. 
        if (diff(range(Daily.Matrice[i.reg]$pseu.bline)) < 5 || diff(range(Daily.Matrice[i.reg]$Sum_eff)) < 5) { # when tiny change in T and RH, pseu.baseline may yield unrealistic results, therefore we continue with Sum_eff
            var2LM <- "Sum_eff"  
        } else if (summary(lm(y_MA ~ pseu.bline + I(pseu.bline^2), data = Daily.Matrice[i.reg]))$r.squared >  summary(lm(y_MA ~ Sum_eff +I(Sum_eff^2), data = Daily.Matrice[i.reg]))$r.squared) { #1.10 *
            var2LM <- "pseu.bline"
        } else var2LM <- "Sum_eff"
        
        data.table::set(Regions, i = row.reg, j ='var4Out', value = var2LM)
    }
    # if (as.Date('2021-02-04') %in% unique(as.Date(Daily.Matrice$date))) browser()
    if(is.null(Regions$RH.chg) || any(length(Regions$RH.chg)== 0) || any(is.na(Regions$RH.chg))) {
        browser()
        Regions <- NULL}
    return(list(Regions = Regions, Daily.Matrice = Daily.Matrice))
}

#' plot Daily profile of sensor data
#' #' @description  plot Daily profile of sensor raw data with temperature increase in red rectangle and temperature decrease in blue rectangle. 
#' The red line shows the smoothed temperature profile and the blue line shows the smoothed relative humidity profile. the black dot gives the row sensor data and the blue/red line gives the smoothed raw sensor data.
#' the red part of this line indicates when the first derivative decreases and blue when it increases
#' @param Matrice data.table, all data with date, temperature, RH, and sensor data, raw and smoothed (_MA added), index of minimum sensor data (i.date.max, i.date.min), value and index of max temperature 
#' (T.max, i.T.max, T.min and i.T.min) and RH (RH.max, i.RH.max, RH.min i.RH.min), min and max dates (i.date.max, i.date.min) and 1st derivaties of T, RH and sensor smoothed data. RH.Col is color used when RH increase and decreases.
#' @param Daily.Alldata raw indexes of Matrice to be plot
#' @param name.Temperature column name of data.table Matrice holding the raw temperatures
#' @param name.Humidity column name of data.table Matrice holding the raw RHs
#' @param Rows.cal.Inc row indexes of Matrice with RH increasing. Set to NA if not existing.
#' @param Rows.cal.Dec row indexes of Matrice with RH decreasing. Set to NA if not existing.
#' @param Row.Min.RH row index of Matrice with minimum RH and max T on the same day
#' @param Row.Max.RH.next row index of Matrice with minimum T and max RH on the next day
#' @param Row.Max.RH row index of Matrice with maximum RH and Min T on the same day
#' @param lines.dec The dates of regions when RH decreasing to display as dotted-lines (first and last dates excluded since they are already displayed) 
#' @param lines.inc The dates of regions when RH increasing to display as dotted-lines (first and last dates excluded since they are already displayed) 
#' @param lines.inc The dates of regions when RH increasing to display as dotted-lines (first and last dates excluded since they are already displayed) 
Day_Plot <- function(Matrice, Daily.Alldata, Date, name.Temperature, name.Humidity, Rows.cal.Inc = NA, lines.dec = NULL, lines.inc = NULL,
                     Rows.cal.Dec = NA, Row.Min.RH, Row.Max.RH.next, Row.Max.RH) {
    # Converting T and RH on the same y axis as y (sensor)
    Max.RH <- max(Matrice[Daily.Alldata][[name.Humidity]], na.rm = T)
    Min.RH <- min(Matrice[Daily.Alldata][[name.Humidity]], na.rm = T)
    Max.T  <- max(Matrice[Daily.Alldata][[name.Temperature]], na.rm = T)
    Min.T  <- min(Matrice[Daily.Alldata][[name.Temperature]], na.rm = T)
    Max.y  <- max(Matrice[Daily.Alldata][["y_MA"]],Matrice[Daily.Alldata][["y"]], na.rm = T)
    Min.y  <- min(Matrice[Daily.Alldata][["y_MA"]],Matrice[Daily.Alldata][["y"]], na.rm = T)
    Slope <- (Max.y - Min.y) / (max(Max.T,Max.RH) - min(Min.T, Min.RH))
    Offset.T  <- Min.y - Min.T  * Slope 
    Offset.RH <- Max.y - Max.RH * Slope
    # Secondary axis https://r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html
    a <- (Max.RH - Min.T)/(Max.y - Min.y)
    b <- Min.T - a * Min.y
    Day <- ggplot(data = Matrice[Daily.Alldata]) +
        geom_point( aes(x = date, y = y), color =  "black", size = 0.25) +
        #geom_point( aes(x = date, y = y_MA), color =  Matrice[Daily.Alldata]$RH.Col, size = 0.35) +
        labs(title = paste0("Daily data, ", as.Date(Date)))  +
        theme(plot.title = element_text(size=12)) + 
        # Divide by 10 to get the same range than the temperature
        scale_y_continuous(
            # limits = c(min(Matrice[Daily.Alldata]$T_eff, Matrice[Daily.Alldata]$RH_eff, Matrice[Daily.Alldata]$y_MA), max(Matrice[Daily.Alldata]$T_eff, Matrice[Daily.Alldata]$RH_eff, Matrice[Daily.Alldata]$y_MA)),
            # Features of the first axis
            name = "Raw sensor data",
            # Add a second axis and specify its features
            sec.axis = sec_axis(~.*a + b, name="T and RH")) + 
        geom_line(aes(x = date, y = get(name.Temperature) * Slope + Offset.T), color = "red") + 
        geom_line(aes(x = date, y = get(name.Humidity)    * Slope + Offset.RH), color = "blue") 
    if (all(!is.na(Rows.cal.Inc)) && all(!is.null(Rows.cal.Inc)) && length(Rows.cal.Inc) > 2){
        Day <- Day + geom_vline(xintercept = c(Matrice[Row.Min.RH]$date, Matrice[Row.Max.RH.next]$date), color = c("green", "red"))
        Day <- Day + annotate("rect",
                              xmin = Matrice[Row.Min.RH]$date, xmax = Matrice[Row.Max.RH.next]$date,
                              ymin = layer_scales(Day)$y$range$range[1], ymax = layer_scales(Day)$y$range$range[2],
                              alpha = 0.2, color =  "blue", fill = "blue") 
        if (!is.null(lines.inc)) Day <- Day + geom_vline(xintercept = lines.inc, color = "yellow", linewidth = 1L)
    }
    if (all(!is.na(Rows.cal.Dec)) && all(!is.null(Rows.cal.Dec)) && length(Rows.cal.Dec) > 2){
        Day <- Day + geom_vline(xintercept = c(Matrice[Row.Max.RH]$date, Matrice[Row.Min.RH]$date), color = c("green", "red"))
        Day <- Day + annotate("rect",
                              xmin = Matrice[Row.Min.RH]$date, xmax = Matrice[Row.Max.RH]$date, 
                              ymin = layer_scales(Day)$y$range$range[1], ymax = layer_scales(Day)$y$range$range[2],
                              alpha = 0.2, color =  "red", fill = "red")
        if (!is.null(lines.dec)) Day <- Day + geom_vline(xintercept = lines.dec, color = "yellow", linewidth = 1L)
    }
    return(Day)
}

#' Plot the half day profile of baseline fitting
#' @param Covariate Mandatory, String with covariates names used for baseline correction some type of temperature or humidity. Covariate shall contain or in order to be recognized. Otherwise Plot_Baseline returns an error. 
#' @param Daytime
#' @param vlines the dates of where regions start
Plot_Baseline <- function(Peaks, Daily.RH, Cal.RH, Row.RH, Title, Covariate, Daytime, vlines) {
    # Detecting if Model use temperature or humidity
    if (grepl("emp" , Covariate)){
        if(Daytime == "morning") Var <- "min.T" else Var <- "max.T"
    } else if (grepl("umid", Covariate) || grepl("Int", Covariate)){
        if(Daytime == "morning") Var <- "max.RH" else Var <- "min.RH"
    } else if (grepl("Sum_eff", Covariate)){
        if(Daytime == "morning") Var <- "max.Sum_eff" else Var <- "min.Sum_eff"
    } else if (grepl("_estimated", Covariate)){
        if(Daytime == "morning") {
            Var <- "max.Est"
        } else Var <- "min.Est"
        if (exists("Peaks") && !is.null(Peaks) && data.table::is.data.table(Peaks) && nrow(Peaks) > 0) {
            library(RColorBrewer)
            n.Baseline.Col <- colorRampPalette(brewer.pal(n = 9, name = "Oranges")[9:2])(Peaks[which(Baseline) , .N])
            n.Pollutant.Col <- colorRampPalette(brewer.pal(n = 9, name = "Greens")[9:2])(Peaks[which(!Baseline) , .N])
            # Peak Color
            n.Baseline.Cumul <- 0; n.Pollutant.Cumul <-  0
            if (grepl("Est",Var)) {
                data.table::set(Peaks, j = Var, value = Daily.RH$nls_estimated[Peaks$Ind.Peak])
            } else data.table::set(Peaks, j = Var, value = Daily.RH$Estimated[Peaks$Ind.Peak])}
    } else stop("[Plot_Baseline] ERROR unknown Covariate name")
    # Set Pallette Color see https://library.virginia.edu/data/articles/setting-up-color-palettes-in-r
    # and https://www.datanovia.com/en/blog/easy-way-to-expand-color-palettes-in-r/
    #Plotting
    Baseline.Fit <- ggplot(data = Daily.RH) +
        # Initial color for missing colors, no peaks
        # geom_point(aes(x = get(Cal.RH[Row.RH]$Var) , y = y_MA), color ="violet") +
        # geom_point(aes(x = get(Cal.RH[Row.RH]$Var) , y = y_MA), color = Daily.RH$Color) + 
        geom_point(aes(x = get(Cal.RH[Row.RH]$Var) , y = y_MA), color = Daily.RH$Inf.col) + 
        # labs(title = Title, x = Cal.RH[Row.RH]$Var) +
        labs(title = "", x = Cal.RH[Row.RH]$Var) +
        # geom_point(aes(x = get(Cal.RH[Row.RH]$Var) , y = Estimated), color = "black", size = 0.5) +
        geom_line(aes(x = get(Cal.RH[Row.RH]$Var) , y = nls_estimated), color = "black", linewidth = 1) +
        theme(plot.title = element_text(size=1))
    # adding LM estimation
    if ('LM_estimated' %in% names(Daily.RH)) {
        Baseline.Fit <- Baseline.Fit + geom_line(aes(x = get(Cal.RH[Row.RH]$Var) , y = LM_estimated), color = 'yellow', linewidth = 1, lty = 'dashed')}
    # Changing background panel color for increasing and decreasing raw sensor data
    if((Daytime == "morning")){
        Baseline.Fit    <- Baseline.Fit + theme(panel.background = element_rect(fill = "#F5D0BA"))
    } else Baseline.Fit <- Baseline.Fit + theme(panel.background = element_rect(fill = "#B9C1EB"))
    
    if (!is.null(vlines)) {
        for (i in seq_along(vlines)) {
            Baseline.Fit <- Baseline.Fit + geom_vline(xintercept = Daily.RH[date == vlines[i]][, get(Cal.RH[Row.RH]$Var)], color = "yellow", linewidth = 1L)  
        }
    }
    # Adding peak numbers
    # vjust to see better the label, label above line
    Vjust <- 0.04 * diff(layer_scales(Baseline.Fit)$y$range$range)    
    if (exists("Peaks") && !is.null(Peaks) && data.table::is.data.table(Peaks) && nrow(Peaks) > 0) {
        for(n.Peak in seq_along(Peaks$Ind.Peak)){
            Range <- Peaks$Ind.Start[n.Peak]:Peaks$Ind.End[n.Peak]
            data.table::set(Daily.RH, i = Range, j = "n.Peak", value = rep(n.Peak,length(Range)))
            if(Peaks$Baseline[n.Peak]){
                n.Baseline.Cumul <- n.Baseline.Cumul + 1
                data.table::set(Daily.RH, i = Range, j = "Color", value = rep(n.Baseline.Col[n.Baseline.Cumul],length(Range)))
            } else {
                n.Pollutant.Cumul <- n.Pollutant.Cumul + 1
                data.table::set(Daily.RH, i = Range, j = "Color", value = rep(n.Pollutant.Col[n.Pollutant.Cumul],length(Range)))}
            # }
            # 
            # # Adding peak number
            # if (exists("Peaks") && !is.null(Peaks) && data.table::is.data.table(Peaks) && nrow(Peaks) > 0) {
            #     for(n.Peak in seq_along(Peaks$Ind.Peak)){
            #         # Label <- paste0("Peak: ", round(Peaks$r.sqr.yMA_Sum_eff[n.Peak],3),ifelse(Peaks$r.sqr.yMA_Sum_eff[n.Peak] > 0.99,">", "<"), 0.99)
            Label <- Peaks$Label[n.Peak]
            
            # Label <- paste0("Peak: ", round(Peaks$Sum.Peak[n.Peak],1),ifelse(Peaks$Sum.Peak[n.Peak]>Peaks$Sum.Delta.Eff[n.Peak],">", "<"), round(Peaks$Sum.Delta.Eff[n.Peak],1),
            #                 ", baseline: ", round(Peaks$End.Peak[n.Peak] - Peaks$RefPoint[n.Peak],1),
            #                 ifelse(Peaks$End.Peak[n.Peak] - Peaks$RefPoint[n.Peak] > Peaks$Sum.Delta.Eff[n.Peak],">", "<"), round(Peaks$Sum.Delta.Eff[n.Peak],1))
            # 
            # Label <- paste0("Peak: ", round(Peaks$Delta.Peak[n.Peak],1),ifelse(Peaks$Delta.Peak[n.Peak]>Peaks$Sum.eff[n.Peak],">", "<"), round(Peaks$Sum.eff[n.Peak],1),
            #                 ", baseline: ", round(Peaks$End.Peak[n.Peak] - Peaks$min.Peak[n.Peak],1),
            #                 ifelse(Peaks$End.Peak[n.Peak] - Peaks$min.Peak[n.Peak]>Peaks$Total.Sum.Effect[n.Peak],">", "<"), round(Peaks$Total.Sum.Effect[n.Peak],1))
            # Position of metrics
            if(Peaks$Str.Peak[n.Peak] > diff(layer_scales(Baseline.Fit)$y$range$range)*0.5 + layer_scales(Baseline.Fit)$y$range$range[1]) Position <- 0.25 else Position <- 0.75
            Baseline.Fit <- Baseline.Fit + annotate(geom = "text", x = Peaks[[Var]][n.Peak], y = Peaks$Str.Peak[n.Peak] + Vjust,label = n.Peak,size = 4, colour = "darkblue")
            Baseline.Fit <- Baseline.Fit + annotate(geom = "text", x = Peaks[[Var]][n.Peak], y = diff(layer_scales(Baseline.Fit)$y$range$range)* Position + layer_scales(Baseline.Fit)$y$range$range[1],
                                                    label = Label, size = 4, colour = "darkblue", angle = 90)
            
            # if (Peaks$Baseline[n.Peak]){
            #     Baseline.Fit <- Baseline.Fit + annotate(geom = "text", x = Peaks[[Var]][n.Peak], y = Peaks$Str.Peak[n.Peak] + Vjust,label = n.Peak,size = 5, colour = "red")
            #     Baseline.Fit <- Baseline.Fit + annotate(geom = "text", x = Peaks[[Var]][n.Peak], y = diff(layer_scales(Baseline.Fit)$y$range$range)* Position + layer_scales(Baseline.Fit)$y$range$range[1],
            #                                             label = Label, size = 4, colour = "red", angle = 90)
            # } else {
            #     Baseline.Fit <- Baseline.Fit + annotate(geom = "text", x = Peaks[[Var]][n.Peak], y = Peaks$Str.Peak[n.Peak] + Vjust,label = n.Peak,size = 5, colour = "green4")
            #     Baseline.Fit <- Baseline.Fit + annotate(geom = "text", x = Peaks[[Var]][n.Peak], y = diff(layer_scales(Baseline.Fit)$y$range$range)* Position + layer_scales(Baseline.Fit)$y$range$range[1],
            #                                             label = Label, size = 4, colour = "green4", angle = 90)
            # }
        }  
    }
    for (ii in 1:length(Title)) {
        m = 1-0.04*ii
        Baseline.Fit <- cowplot::ggdraw(Baseline.Fit) + 
            cowplot::draw_label(Title[ii], x = 0.5, y = m, size = 10)} 
    Baseline.Fit <- Baseline.Fit + theme(plot.title = element_text(size=1))
    return(Baseline.Fit)
}

#' Adding peak numbers to Daily profile
#' @param Day ggplot with daily profile to which to add peak number
#' @param Daily.RH data.table,  increasing or decreasing daily data with date, temperature, RH, and sensor data, raw and smoothed (_MA added), index of minimum sensor data (i.date.max, i.date.min), value and index of max temperature 
#' (T.max, i.T.max, T.min and i.T.min) and RH (RH.max, i.RH.max, RH.min i.RH.min), min and max dates (i.date.max, i.date.min) and 1st derivaties of T, RH and sensor smoothed data. RH.Col is color used when RH increase and decreases.

Add_peak <- function(Day, Peaks, Daily.RH, Var = NULL){
    if (exists("Peaks") && data.table::is.data.table(Peaks) && nrow(Peaks) > 0){
        # vjust to see better the label, label above line
        Vjust <- 0.04 * diff(layer_scales(Day)$y$range$range)
        # Add peak color
        # colors <- data.table::data.table(color = rep(NA_character_, times = nrow(Daily.RH)))
        # for (peak in seq_along(Peaks$Peak.values)) {
        #     data.table::set(colors, i = Peaks$Ind.Start[peak]:Peaks$Ind.End[peak], j = 'color', value = ifelse(Peaks$Baseline[peak], "red", 'green') )
        # }
        # Day <-  Day + geom_line(data = Daily.RH, aes(x = date, y = y_MA), color = colors$color, linewidth = 1) #color = Daily.RH$Inf.col
        # Day <-  Day + geom_point(data = Daily.RH, aes(x = date, y = LM_estimated), color = Daily.RH$Color, size = 0.5) 
        # Day <-  Day + geom_point(data = Daily.RH, aes(x = date, y = y_MA), color = Daily.RH$Color, size = 0.5) 
        # Add peak number with color
        Day <-  Day + geom_point(data = Daily.RH, aes(x = date, y = Daily.RH[[Var]]), color = Daily.RH$Inf.col, size = 0.5)
        for(n.Peak in seq_along(Peaks$Ind.Peak)){
            Day <- Day + annotate(geom = "text", x = Daily.RH$date[Peaks$Ind.Peak[n.Peak]], y = Peaks$Peak.values[n.Peak] + Vjust,label = n.Peak,size = 4, colour = "darkblue")}
        # Day <-  Day + geom_point(data = Daily.RH, aes(x = date, y = Daily.RH[[Var]]), color = Daily.RH$Inf.col, size = 0.5)
        # for(n.Peak in seq_along(Peaks$Ind.Peak)){
        #     if (Peaks$Baseline[n.Peak]){
        #         Day <- Day + annotate(geom = "text", x = Daily.RH$date[Peaks$Ind.Peak[n.Peak]], y = Peaks$Peak.values[n.Peak] + Vjust,label = n.Peak,size = 5, colour = "red")
        #     } else {
        #         Day <- Day + annotate(geom = "text", x = Daily.RH$date[Peaks$Ind.Peak[n.Peak]], y = Peaks$Peak.values[n.Peak] + Vjust,label = n.Peak,size = 5, colour = "green4")}}
    } else {
        Day <-  Day + geom_point(data = Daily.RH, aes(x = date, y = Daily.RH[[Var]]), color = Daily.RH$Inf.col, size = 0.5) 
        # + geom_point(data = Daily.RH, aes(x = date, y = nls_estimated), color = 'brown', size = 0.5)
        
    }
    return(Day)
}

# To find the peak using pracma. it is used for finding peaks for residuals of a model 
#' @param data4peaks the data to determine the peaks
Find.Peaks <- function(data4peaks, var, ...) {
    Peaks    <- data.table::data.table(pracma::findpeaks(data4peaks[[var]],  nups = 5, ndowns = 5, minpeakdistance = 5)) #, minpeakheight = 0.0025
    # are there peak at the end without downs?
    Peaks.ups <- data.table::data.table(pracma::findpeaks(data4peaks[[var]], nups = 3, ndowns = 0, minpeakdistance = 5)) #, minpeakheight = 0.0025
    Peaks.ups <- Peaks.ups[order(Peaks.ups$V4)]
    Peaks.down <- data.table::data.table(pracma::findpeaks(data4peaks[[var]], nups = 0, ndowns = 3, minpeakdistance = 5)) #, minpeakheight = 0.0025
    Peaks.down <- Peaks.down[order(Peaks.down$V4)]
    if(nrow(Peaks.ups)  > nrow(Peaks))  Add.ups <- TRUE else Add.ups <- FALSE
    if(nrow(Peaks.down) > nrow(Peaks)) Add.Down <- TRUE else Add.Down <- FALSE
    if(Add.ups)  {
        if (nrow(Peaks) == 0) {
            Peaks <- Peaks.ups
        } else Peaks <- merge(Peaks, Peaks.ups, by = c("V1","V2","V3","V4"), all = T)}
    if (Add.Down) {
        if (nrow(Peaks) == 0) {
            Peaks <- Peaks.down
        } else Peaks <- merge(Peaks, Peaks.down, by = c("V1","V2","V3","V4"), all = T)}
    # checking if any missing data at the beginning of end of peaks
    Peaks <- Peaks[order(Peaks$V3)]
    if(!shiny::isTruthy(Peaks$V3[1])) browser()
    if (Peaks$V3[1] != 1) {
        # checking if the missing part increasing or decreasing
        peak.peak <- max(data4peaks[[var]][1:Peaks$V3[1]])
        ind.peak  <- which(data4peaks[[var]][1:Peaks$V3[1]] == peak.peak)
        if (Peaks[1]$V3 < 3) { # we cannot set a new peak when we have only 2 data since the slope of Sum effects, Peaks$slp.yMA_Sum_eff, might return NA. we combine with the 1st peak
            data.table::set(Peaks, i = 1L, j = "V3", value = 1)
            data.table::set(Peaks, i = 1L, j = "V1", value = peak.peak)
            data.table::set(Peaks, i = 1L, j = "V2", value = ind.peak)
        } else {
            Peaks     <- rbindlist(list(Peaks, data.table::data.table("V1" = peak.peak, "V2" = ind.peak, "V3" = 1, "V4" = Peaks$V3[1])), use.names = T)
            Peaks <- Peaks[order(Peaks$V3)]} 
    }
    if (nrow(Peaks) > 1) {
        # checking for any gab between peaks
        for (i in 1:(nrow(Peaks)-1)) {
            data.table::set(Peaks, i = i, j = "gap", value = Peaks$V4[i] == Peaks$V3[i+1])
            if (!Peaks[i]$gap) data.table::set(Peaks, i = i, j = "V4", value = Peaks$V3[i+1])
        }
        Peaks <- Peaks[, gap := NULL]
    }
    # checking the end of last peak being the length of data
    if (Peaks$V4[nrow(Peaks)] < length(data4peaks[[var]])) {
        data.table::set(Peaks, i = as.integer(nrow(Peaks)), j = "V4", value = length(data4peaks[[var]]))
        # peak.peak <- which(data4peaks[[var]] == max(data4peaks[[var]][Peaks$V4[nrow(Peaks)]:length(data4peaks[[var]])]))
        # ind.peak  <- min(data4peaks[[var]][data4peaks[[var]] == peak.peak, which = T])
        # if (length(data4peaks[[var]]) -  Peaks$V4[nrow(Peaks)] < 3) {
        #     data.table::set(Peaks, i = as.integer(nrow(Peaks)), j = "V4", value = length(data4peaks[[var]]))
        #     data.table::set(Peaks, i = as.integer(nrow(Peaks)), j = "V1", value = peak.peak)
        #     data.table::set(Peaks, i = as.integer(nrow(Peaks)), j = "V2", value = ind.peak)
        # } else {
        #     Peaks     <- rbindlist(list(Peaks, data.table::data.table("V1" = peak.peak, "V2" = ind.peak, "V3" = Peaks[nrow(Peaks)]$V4, "V4" = length(data4peaks[[var]]))), use.names = T)}
    }
    data.table::setnames(Peaks, names(Peaks), c("Peak.values", "Ind.Peak", "Ind.Start", "Ind.End"))
    
    rm('Peaks.ups', "Peaks.down")
    return(Peaks = Peaks) 
}

# To find the peak using pracma. used for finding peaks in smoothed sensor response
#' @param Daily.Matrice the half-day data with nA of RH to smooth data, determine the peaks and to select for baseline fitting per region.
#' @Region Peaks are assigned per region 
#' @param name.y sensor response in nA
#' @param name.T Temperature. Smoothed with lower frequency than name.y
#' @param name.RH Relative humidity. Smoothed with lower frequency than name.y
Find.Peaks2 <- function(Daily.Matrice, name.y, name.T, name.RH, ...) {
    # to check the number of data below -10. if too many, we will not check the criteria for min_yMA of Daily.Matrice
    # if (count(Daily.Matrice$y_MA < -10) > 0.1*nrow(Daily.Matrice)) check.minyMA <- FALSE else check.minyMA <- TRUE
    # too many negative y_MA ruins the finding peaks and prediction of baseline. If so, we add the min.yMA to all to get rid of negatives
    # if (count(Daily.Matrice$y_MA < 0) > 0.5*nrow(Daily.Matrice)) {
    #     minyMA <- min(Daily.Matrice$y_MA)
    #     data.table::set(Daily.Matrice, j = "y_MA", value = Daily.Matrice$y_MA - minyMA)
    # }
    list.nn <- c(10, 16, 20, 30)
    # if (nrow(Daily.Matrice) > 45) list.nn <- c(30) else list.nn <- c(10, 16, 20, 30)
    for (nn in seq_along(list.nn)) {
        Peaks    <- data.table::data.table(pracma::findpeaks(Daily.Matrice[[name.y]], nups = list.nn[nn], ndowns = list.nn[nn], minpeakdistance = 5))
        # are there peak at the end without downs?
        Peaks.ups <- data.table::data.table(pracma::findpeaks(Daily.Matrice[[name.y]], nups = round(list.nn[nn]/2,0), ndowns = 0, minpeakdistance = 5))
        Peaks.ups <- Peaks.ups[order(Peaks.ups$V4)]
        Peaks.down <- data.table::data.table(pracma::findpeaks(Daily.Matrice[[name.y]], nups = 0, ndowns = round(list.nn[nn]/2,0), minpeakdistance = 5))
        Peaks.down <- Peaks.down[order(Peaks.down$V4)]
        if(nrow(Peaks.ups)  > nrow(Peaks))  Add.ups <- TRUE else Add.ups <- FALSE
        if(nrow(Peaks.down) > nrow(Peaks)) Add.Down <- TRUE else Add.Down <- FALSE
        if(Add.ups)  {
            if (nrow(Peaks) == 0) {
                Peaks <- Peaks.ups
            } else Peaks <- merge(Peaks, Peaks.ups, by = c("V1","V2","V3","V4"), all = T)}
        if(Add.Down) {
            if (nrow(Peaks) == 0) {
                Peaks <- Peaks.down
            } else Peaks <- merge(Peaks, Peaks.down, by = c("V1","V2","V3","V4"), all = T)}
        # if (nn == 1) Peaks.10 <- Peaks
        if ((nrow(Peaks) <= 20 || nn == length(list.nn)) && nrow(Peaks) > 0)  {
            # checking if any missing data at the beginning of end of peaks
            Peaks <- Peaks[order(Peaks$V3)]
            if(!shiny::isTruthy(Peaks$V3[1])) browser()
            if (Peaks$V3[1] != 1) {
                # checking if the missing part increasing or decreasing
                peak.peak <- max(Daily.Matrice[1:Peaks$V3[1]][[name.y]])
                ind.peak  <- which(Daily.Matrice[1:Peaks$V3[1]][[name.y]] == peak.peak)
                if (Peaks[1]$V3 < 3) { # we cannot set a new peak when we have only 2 data since the slope of Sum effects, Peaks$slp.yMA_Sum_eff, might return NA. we combine with the 1st peak
                    data.table::set(Peaks, i = 1L, j = "V3", value = 1)
                    data.table::set(Peaks, i = 1L, j = "V1", value = peak.peak)
                    data.table::set(Peaks, i = 1L, j = "V2", value = ind.peak)
                } else {
                    Peaks     <- rbindlist(list(Peaks, data.table::data.table("V1" = peak.peak, "V2" = ind.peak, "V3" = 1, "V4" = Peaks$V3[1])), use.names = T)
                    Peaks <- Peaks[order(Peaks$V3)]} 
            } 
            if (Peaks$V4[nrow(Peaks)] < nrow(Daily.Matrice)) {
                peak.peak <- max(Daily.Matrice[Peaks$V4[nrow(Peaks)]:nrow(Daily.Matrice)][[name.y]])
                ind.peak  <- min(Daily.Matrice[Daily.Matrice[[name.y]] == peak.peak, which = T])
                if (nrow(Daily.Matrice) -  Peaks$V4[nrow(Peaks)] < 3) {# we cannot set a new peak since the slope of Sum effects, Peaks$slp.yMA_Sum_eff, might return NA
                    data.table::set(Peaks, i = as.integer(nrow(Peaks)), j = "V4", value = nrow(Daily.Matrice))
                    data.table::set(Peaks, i = as.integer(nrow(Peaks)), j = "V1", value = peak.peak)
                    data.table::set(Peaks, i = as.integer(nrow(Peaks)), j = "V2", value = ind.peak)
                } else {
                    Peaks     <- rbindlist(list(Peaks, data.table::data.table("V1" = peak.peak, "V2" = ind.peak, "V3" = Peaks[nrow(Peaks)]$V4, "V4" = nrow(Daily.Matrice))), use.names = T)}
            }
            break}
        rm('Peaks.ups', "Peaks.down")
    }
    
    if (nrow(Peaks) > 0) {
        data.table::setnames(Peaks, names(Peaks), c("Peak.values", "Ind.Peak", "Ind.Start", "Ind.End"))
        Peaks <- Peaks[order(Peaks$Ind.Start)]
        n.peaks <- seq_along(Peaks$Peak.values)
        # to check the continuity of peaks. if not, the missing part to be added to the previous peak
        if (nrow(Peaks) > 1) {
            for (ii in 2:(nrow(Peaks))) {
                if (Peaks$Ind.Start[ii] != Peaks$Ind.End[ii-1]) data.table:: set(Peaks, i = as.integer(ii-1), j = "Ind.End", value = Peaks$Ind.Start[ii])
            }
        }
        Peaks[, Start := as.POSIXct(sapply(n.peaks, function (k) Daily.Matrice[Peaks$Ind.Start[k]][["date"]]))]
        Peaks[, End := as.POSIXct(sapply(n.peaks, function (k) Daily.Matrice[Peaks$Ind.End[k]][["date"]]))]
        Peaks[, Str.Peak := sapply(n.peaks, function (k) Daily.Matrice[Peaks$Ind.Start[k]][[name.y]])]
        Peaks[, End.Peak := sapply(n.peaks, function (k) Daily.Matrice[Peaks$Ind.End[k]][[name.y]])]
        if (any(Peaks$Str.Peak < -5, Peaks$End.Peak < -5) && count(Daily.Matrice$y_MA < 0) < 0.10*nrow(Daily.Matrice)) {
            peak2discard  <- which(Peaks$Str.Peak < -5 | Peaks$End.Peak < -5)
            for (i in peak2discard) {
                if (length(peak2discard) == 1 && i == 1) {
                    i.na <- Peaks$Ind.Start[i]:(Peaks$Ind.End[i]-1)
                } else if (length(peak2discard) == 1 && i == length(Peaks)) {
                    i.na <- (Peaks$Ind.Start[i] +1):Peaks$Ind.End[i]
                } else if (i == peak2discard[1]) i.na <- (Peaks$Ind.Start[i] +1):Peaks$Ind.End[i] else i.na <- Peaks$Ind.Start[i]:(Peaks$Ind.End[i]-1)
                data.table::set(Daily.Matrice, i = i.na, j = "y", value = NA) 
            }}
        
        for (Peak.row in n.peaks) {
            if (which.min(Daily.Matrice[Peaks$Ind.Start[Peak.row]:Peaks$Ind.End[Peak.row]][[name.y]]) >
                which.max(Daily.Matrice[Peaks$Ind.Start[Peak.row]:Peaks$Ind.End[Peak.row]][[name.y]])) {
                sign.Peak <- -1
            } else {
                sign.Peak <- 1
            }
            
            data.table::set(Peaks, i = Peak.row, j = "Delta.Peak", 
                            value = sign.Peak*(Peaks$Peak.values[Peak.row] - min(Daily.Matrice[Peaks$Ind.Start[Peak.row]:Peaks$Ind.End[Peak.row]][[name.y]], na.rm =T)))
            data.table::set(Peaks, i = Peak.row, j = "Net.Peak", 
                            value = sign.Peak*(Peaks$Peak.values[Peak.row] - max(Daily.Matrice[Peaks$Ind.Start[Peak.row]][[name.y]], Daily.Matrice[Peaks$Ind.End[Peak.row]][[name.y]], na.rm =T)))
            data.table::set(Peaks, i = Peak.row, j = "Variation.Peak",
                            value = ifelse(!is.na(coef(lm(as.formula(paste0(name.y, " ~ date")), data = Daily.Matrice[Peaks$Ind.Start[Peak.row]:Peaks$Ind.End[Peak.row]]))[2]),
                                           coef(lm(as.formula(paste0(name.y, " ~ date")), data = Daily.Matrice[Peaks$Ind.Start[Peak.row]:Peaks$Ind.End[Peak.row]]))[2],
                                           Peaks$Delta.Peak[Peak.row]/(Peaks$Ind.End[Peak.row]-Peaks$Ind.Start[Peak.row])))
            data.table::set(Peaks, i = Peak.row, j = "Delta.T", value = diff(range(Daily.Matrice[Peaks$Ind.Start[Peak.row]:Peaks$Ind.End[Peak.row]][[name.T]], na.rm =T)))
            data.table::set(Peaks, i = Peak.row, j = "min.T", value = min(Daily.Matrice[Peaks$Ind.Start[Peak.row]:Peaks$Ind.End[Peak.row]][[name.T]], na.rm =T))
            data.table::set(Peaks, i = Peak.row, j = "max.T", value = max(Daily.Matrice[Peaks$Ind.Start[Peak.row]:Peaks$Ind.End[Peak.row]][[name.T]], na.rm =T))
            data.table::set(Peaks, i = Peak.row, j = "Variation.T",
                            value = ifelse(!is.na(coef(lm(as.formula(paste0(name.T, " ~ date")), data = Daily.Matrice[Peaks$Ind.Start[Peak.row]:Peaks$Ind.End[Peak.row]]))[2]),
                                           coef(lm(as.formula(paste0(name.T, " ~ date")), data = Daily.Matrice[Peaks$Ind.Start[Peak.row]:Peaks$Ind.End[Peak.row]]))[2],
                                           Peaks$Delta.T[Peak.row]/(Peaks$Ind.End[Peak.row]-Peaks$Ind.Start[Peak.row])))
            data.table::set(Peaks, i = Peak.row, j = "Delta.RH", value = diff(range(Daily.Matrice[Peaks$Ind.Start[Peak.row]:Peaks$Ind.End[Peak.row]][[name.RH]], na.rm =T)))
            data.table::set(Peaks, i = Peak.row, j = "min.RH", value = min(Daily.Matrice[Peaks$Ind.Start[Peak.row]:Peaks$Ind.End[Peak.row]][[name.RH]], na.rm =T))
            data.table::set(Peaks, i = Peak.row, j = "max.RH", value = max(Daily.Matrice[Peaks$Ind.Start[Peak.row]:Peaks$Ind.End[Peak.row]][[name.RH]], na.rm =T))
            data.table::set(Peaks, i = Peak.row, j = "Variation.RH",
                            value = ifelse(!is.na(coef(lm(as.formula(paste0(name.RH, " ~ date")), data = Daily.Matrice[Peaks$Ind.Start[Peak.row]:Peaks$Ind.End[Peak.row]]))[2]),
                                           coef(lm(as.formula(paste0(name.RH, " ~ date")), data = Daily.Matrice[Peaks$Ind.Start[Peak.row]:Peaks$Ind.End[Peak.row]]))[2],
                                           Peaks$Delta.RH[Peak.row]/(Peaks$Ind.End[Peak.row]-Peaks$Ind.Start[Peak.row])))
            
            i.which <- Peaks$Ind.Start[Peak.row]:Peaks$Ind.End[Peak.row]
            if (is.na(coef(lm(Daily.Matrice[i.which][[name.y]]  ~ Daily.Matrice[i.which][["Sum_eff"]]))[2])) {
                value.r   <- 0
                value.slp <- 0
            } else {
                value.r <- round(summary(lm(Daily.Matrice[i.which][[name.y]]  ~ Daily.Matrice[i.which][["Sum_eff"]]))$r.squared, 3)
                value.slp <- round(coef(lm(Daily.Matrice[i.which][[name.y]]  ~ Daily.Matrice[i.which][["Sum_eff"]]))[2], 2)
            }
            data.table::set(Peaks, i = Peak.row, j = "R2.Sum_eff", value = value.r)
            data.table::set(Peaks, i = Peak.row, j = "slp.Sum_eff", value = value.slp)
            if ('pred.by.last.model' %in% names(Daily.Matrice) && !all(is.na(Daily.Matrice[i.which]$pred.by.last.model))) {
                # R2 of LR between y_MA and baseline predicted using the previous nls model
                if (is.na(coef(lm(y_MA ~ pred.by.last.model, data = Daily.Matrice[i.which]))[2])) {
                    r.prev   <- 0
                    slp.prev <- 0
                } else {
                    r.prev   <- round(summary(lm(y_MA ~ pred.by.last.model, data = Daily.Matrice[i.which]))$r.squared, 3)
                    slp.prev <- round(coef(lm(y_MA ~ pred.by.last.model, data = Daily.Matrice[i.which]))[2], 2)
                }
            } else {
                r.prev   <- value.r
                slp.prev <- value.slp
            }
            data.table::set(Peaks, i = Peak.row, j = "R2.prevPred", value = r.prev)
            data.table::set(Peaks, i = Peak.row, j = "slp.prevPred", value = slp.prev)
            
            # assigning baseline peaks
            ## If the y_MA and Sum effect are in opposite direction (negative slope), its is baseline
            if (sign(Peaks$slp.Sum_eff[Peak.row]) == -1 ) {
                BLine <- FALSE
            } else if (Peaks[Peak.row]$R2.Sum_eff < 0.80) { # weak association
                BLine <- FALSE
            } else if (abs(Peaks[Peak.row]$Net.Peak) > 5) { # for some long peaks we check the net peak
                BLine <- FALSE
            } else BLine <- TRUE
            data.table::set(Peaks, i = Peak.row, j = "Baseline", value = BLine)
            data.table::set(Peaks, i = Peak.row, j = "Baseline.Sum_eff", value = Peaks[Peak.row]$R2.Sum_eff * sign(Peaks[Peak.row]$slp.Sum_eff) > 0.80)
            data.table::set(Peaks, i = Peak.row, j = "Baseline.prevPred", value = Peaks[Peak.row]$R2.prevPred* sign(Peaks[Peak.row]$slp.prevPred) > 0.80)
            # to be baseline either T_eff + RH_eff or baseline with previous model must satisfy
            if ((Peaks[Peak.row]$Baseline.Sum_eff || Peaks[Peak.row]$Baseline.prevPred) &&
                (Peaks[Peak.row]$slp.Sum_eff < 10 && Peaks[Peak.row]$slp.prevPred < 10)) { # sometimes R2 is too high with actually weak correlation due to high slope and very low negative intercept
                bline.value <- TRUE
            } else bline.value <- FALSE
            data.table::set(Peaks, i = Peak.row, j = "BASELINE", value = bline.value)
            #  data.table::set(Daily.Matrice, i = Peaks$Ind.Start[Peak.row]:Peaks$Ind.End[Peak.row], j = "Baseline", value = Peaks$BASELINE[Peak.row])
            
            if (Peaks[Peak.row]$BASELINE) {
                if (exists("i.nls")) i.nls <- c(i.nls, Peaks[Peak.row]$Ind.Start:Peaks[Peak.row]$Ind.End) else i.nls <- Peaks[Peak.row]$Ind.Start:Peaks[Peak.row]$Ind.End
            }
            
        }
        
    } else {
        Peaks      <- NULL
    }
    # if (as.Date(Daily.Matrice$date[1]) == as.Date('2021-02-24')) browser()
    if (!exists('i.nls')) i.nls <- as.integer(0)
    return(list(Peaks = Peaks, i.nls = unique(i.nls), Daily.Matrice = Daily.Matrice)) 
}

###Set_nls_Model: Function to predict the baseline using the pseudo basline (from either previous model of Sum effect) vs yMA linear models at yMA possibly Baseline ====
#================================================================CR
#' This function sets the baseline based on the RH regions, nls model fitted in the best estimation of baseline determined by MLR between yMA and T eff + RH eff  
#' @param Daily.Matrice  the time series of sensor response, T, RH and RH-derivatives of half day
#' @param i.which  the start-end of region
#' @param i.nls  the start-end of baseline peaks (R2 between y_MA and sum_eff > 0.80)
#' @param RH.sign  the sign of region's RH, decreasing of increasing
#' @param Inc.Last.nls.model  The last nls model where RH increasing. we pass this model to Set.nls.Model function
#' @param Dec.Last.nls.model  The last nls model where RH decreasing. we pass this model to Set.nls.Model function
#' @param row.reg  the index of the region
#' @param Last.Bline  the last predicted baseline
#' @param thrs.bline  the threshold of Delta Sum Effect where we evaluate if the variation of sum effect is enough for an LRM and nls models
#' @param var the variable, 'pseu.bline' or 'Sum_eff', which is used to evaluate the variation of expected baseline in the dataset against thrs.bline
#' @param cook.resid  the threshold of residual of MLR cooks distance for influential
#' @param r.cook.resid  the threshold of residual of MLR cooks distance for influential normalized to corresponding y_MA 
#' @return the baseline model
Set_nls_Model <- function(Daily.Matrice, i.which, i.nls, RH.sign, Inc.Last.nls.model,Dec.Last.nls.model, row.reg, Regions, Last.Bline,
                          thrs.bline, name.T, name.RH, var,...) { #cook.resid = 3, r.cook.resid = 3, 
    # if the delta pseudo baseline of the half day is too low, then we don't try to fit a nls model
    if (diff(range(na.omit(Daily.Matrice[[var]]))) > thrs.bline) {
        outliers.BlineFalse <- Find.outlier2(i.which = i.which, Daily.Matrice = Daily.Matrice, var2LM = var)
        if (exists("outliers.BlineFalse")) Daily.Matrice <- outliers.BlineFalse$Daily.Matrice
        ## determining the curvature factor for T at 20 C, based on the lab results
        r_T <- 1.08
        ## adding curvatured T and RH effects to Daily Matrice
        data.table::set(Daily.Matrice, i = i.which, j='cur.T', value = r_T^(Daily.Matrice[i.which][[name.T]]-9.15))
        if (nrow(Daily.Matrice[i.which][dRH < 0]) > length(i.which)/2){ # RH decreasing
            r_RH <- 1.033
        } else {
            r_RH <- 0.97
        }
        data.table::set(Daily.Matrice, i = i.which, j='cur.RH', value = r_RH^(Daily.Matrice[i.which][[name.RH]]-40))
        if (!'Outliers' %in% names(Daily.Matrice)) data.table::set(Daily.Matrice, j='Outliers', value = !Daily.Matrice$Baseline)
        # If peak evaluation (peaks to be baseline or not) was previously done, we don't check for the outliers by cook's distance
        ## Determining the Baseline TRUE part of the region
        if (!is.null(i.nls))  i.bline <- base::intersect(i.nls, i.which) else i.bline <- intersect(i.which, Daily.Matrice[Outliers != TRUE, which = TRUE]) # Daily.Matrice[i.which][Outliers != TRUE, which = TRUE] # i.which[which(!Daily.Matrice$Outliers[i.which])]
        if (exists('i.bline')) {
            if (length(i.bline) > 15 && length(i.bline)/length(i.which) > 0.20) {# checking if we have at least 1 h AND 25% of region that baseline is TRUE
                # if (length(i.bline) > 60 || length(i.bline)/length(i.which) > 0.25) {# checking if we have at least 1 h or 25% of region that baseline is TRUE
                if (diff(range(Daily.Matrice[[var]][i.bline])) > thrs.bline) { # sum eff is higher the threshold
                    if (nrow(Daily.Matrice[i.which][dRH < 0]) > length(i.which)/2) RH.sign <- 'decr' else RH.sign <- 'incr'
                    Starting.values <- Starting_values(Daily=Daily.Matrice, RH.sign = RH.sign, i.which = i.bline,  r_T = r_T, r_RH = r_RH, 
                                                       name.T = name.T, name.RH = name.RH)
                    #weights (equally distributed)
                    wi <- rep(1/length(i.bline), times = length(i.bline))
                    model.nls <- nls_model(Daily = Daily.Matrice[i.bline], RH.sign = RH.sign, Dec.Last.nls.model = Dec.Last.nls.model, wi = wi,
                                           Inc.Last.nls.model = Inc.Last.nls.model, Starting.values = Starting.values, name.T = name.T, name.RH = name.RH)
                    # Checking if any of model residuals < -10. If so we will re-model using the weights
                    if (any(resid(model.nls) < -10)) {
                        #calculating the weights
                        wi <- abs(resid(model.nls))/sum(abs(resid(model.nls)))
                        # re-fitting nls model with weights
                        model.nls <- nls_model(Daily = Daily.Matrice[i.bline], RH.sign = RH.sign, Dec.Last.nls.model = Dec.Last.nls.model, wi = wi,
                                               Inc.Last.nls.model = Inc.Last.nls.model, Starting.values = Starting.values, name.T = name.T, name.RH = name.RH)
                    }
                    # predicting using nls.model
                    pred.bline <- predict(model.nls, newdata = Daily.Matrice[i.which])
                    if (all(Daily.Matrice$y_MA[i.which] > 0)) {
                        if (count(pred.bline < -10) > 10 || # checking if too many predicted bline < -10 OR # checking if too many baseline subtracted y_MA < -10
                            count(Daily.Matrice$y_MA[i.which] - pred.bline < -10) > 10) negatives <- TRUE else negatives <- FALSE
                    } else { # in case of any y_MA is negative we don't test the negativeness of baseline since is expected to be negative
                        negative.yMA <- TRUE
                        if (count(Daily.Matrice$y_MA[i.which] - pred.bline < -10) > 10) negatives <- TRUE else negatives <- FALSE
                    }
                    if (negatives) { # checking if too many baseline subtracted y_MA < -10
                        # Checking if the predicting using previous model improves negatives
                        if (RH.sign == 'decr') prev.model <- Dec.Last.nls.model else prev.model <- Inc.Last.nls.model
                        if (!is.null(prev.model)) {
                            pred.bline.prev.model <- predict(prev.model, newdata=Daily.Matrice[i.which])
                            if (count(pred.bline.prev.model < -10) > 10 || # checking if too many predicted bline < -10
                                count(Daily.Matrice$y_MA[i.which] - pred.bline.prev.model < -10) > 10) { # checking if too many baseline subtracted y_MA < -10
                                # Checking if fitting polynomial model improves negative residuals
                                model.poly <- lm(y_MA ~ get(var) + I(get(var)^2), data = Daily.Matrice[i.bline])
                                # predicting using model.poly (LMmodel)
                                pred.poly <-  predict(model.poly, newdata=Daily.Matrice[i.which])
                                if (count(pred.poly < -10) > 10 || # checking if too many predicted bline < -10
                                    count(Daily.Matrice$y_MA[i.which] - pred.poly < -10) > 10) { # checking if too many baseline subtracted y_MA < -10
                                    Meas_Funct <- "nls"
                                    do.sub.model <- TRUE
                                } else {
                                    pred.bline <- pred.poly
                                    model.nls  <- model.poly
                                    Meas_Funct <- "Polynomial"
                                    # adjust.negatives <- FALSE
                                }
                            } else {
                                pred.bline <- pred.bline.prev.model
                                model.nls  <- prev.model
                                Meas_Funct <- "Prev.model"
                                # adjust.negatives <- FALSE
                            }
                        } else {
                            Meas_Funct <- "nls"
                        }
                    } else {
                        Meas_Funct <- "nls"
                    }
                } else {
                    do.prev.model <- TRUE
                }
            } else {
                do.prev.model <- TRUE}
        } else {
            do.prev.model <- TRUE}
    } else {
        do.prev.model <- TRUE}
    
    if (exists("do.prev.model")) { 
        # browser ()
        if (any(Daily.Matrice$y_MA[i.which] < 0)) negative.yMA <- TRUE 
        if (!'Outliers' %in% names(Daily.Matrice)) data.table::set(Daily.Matrice, j = 'Outliers', value = FALSE)
        if (RH.sign == 'decr') {
            if (!is.null(Dec.Last.nls.model)) model.nls  <- Dec.Last.nls.model else model.nls  <- NULL
        } else if (!is.null(Inc.Last.nls.model)) model.nls  <- Inc.Last.nls.model else model.nls  <- NULL
        if (!is.null(model.nls)) {
            pred.bline <- predict(model.nls, newdata = Daily.Matrice[i.which])
            # checking if the baseline is too negative
            if (!exists('negative.yMA') && count(pred.bline < -10) > 10) {
                # we will fit nls model only for baseline < -10
                if (exists("i.bline") && !is.null(i.bline) &&
                    length(i.bline) > 15 && length(i.bline)/length(i.which) > 0.20) {
                    i.bline.rev <- i.which[which(pred.bline < -10)]
                } else i.bline.rev <- i.which
                do.sub.model <- TRUE
            } else if (count(Daily.Matrice$y_MA[i.which] - pred.bline < -10) > 10 || any(Daily.Matrice$y_MA[i.which] - pred.bline < -20)) { # checking if baseline subtraction is too negative
                if (exists("i.bline") && !is.null(i.bline) &&
                    length(i.bline) > 15 && length(i.bline)/length(i.which) > 0.20) {
                    i.bline.rev <- i.which[which(Daily.Matrice$y_MA[i.which] - pred.bline < -10)]
                } else i.bline.rev <- i.which
                do.sub.model <- TRUE
            } else if (!is.null(Last.Bline) && abs(pred.bline[1] - Last.Bline) > 10) { # checking if the 1st predicted baseline if too different from the last bline of previous region
                i.bline.rev <- i.which
                do.sub.model <- TRUE
                do_not_check.prev.model <- TRUE
            } else {
                do.sub.model <- FALSE
            }
            Meas_Funct <- "prev.model"
        } else { # no previous model
            do.sub.model <- FALSE
            Meas_Funct   <- "no prev.bline"
            rmse         <- 0  
            pred.bline   <-  rep(NA_real_, times = length(i.which))
            nls.model    <- NULL
        }
    }
    if (exists("do.sub.model") && do.sub.model) {
        # checking if the baseline continuity satisfies
        # normalizing the Sum_eff of region according the sum eff of last estimated baseline
        # browser()
        if (!is.null(Last.Bline)) bline.cont <- Daily.Matrice$Sum_eff[i.which] - Daily.Matrice$Sum_eff[i.which][1] + Last.Bline
        if (is.null(Last.Bline) || (!exists('negative.yMA') && count(bline.cont < -10) > 10) || 
            count(Daily.Matrice$y_MA[i.which] - bline.cont < -10) > 10 ) {
            # resetting nls model with assigning all pred.bline < -10 outlier false
            Starting.values <- Starting_values(Daily=Daily.Matrice, RH.sign = RH.sign, i.which = i.bline.rev,  r_T = r_T, r_RH = r_RH, 
                                               name.T = name.T, name.RH = name.RH)
            #weights (equally distributed)
            wi <- rep(1/length(i.bline.rev), times = length(i.bline.rev))
            model.nls.rev <- nls_model(Daily = Daily.Matrice[i.bline.rev], RH.sign = RH.sign, Dec.Last.nls.model = Dec.Last.nls.model, wi = wi,
                                       Inc.Last.nls.model = Inc.Last.nls.model, Starting.values = Starting.values, name.T = name.T, name.RH = name.RH)
            # predict
            pred.bline.rev <- predict(model.nls.rev, newdata = Daily.Matrice[i.bline.rev])
            # checking if < -10
            if ((!exists('negative.yMA') && count(pred.bline.rev < -10) > 10) || count(Daily.Matrice$y_MA[i.bline.rev] - pred.bline.rev < -10) > 10) {
                # selecting baseline based on minimum negative values
                if (exists('do_not_check.prev.model')) {
                    i.min <- min(which.min(c(count(Daily.Matrice$y_MA[i.bline.rev] - pred.bline.rev < 0), count(Daily.Matrice$y_MA[i.bline.rev] - bline.cont < 0))))
                } else {
                    i.min <- min(which.min(c(count(Daily.Matrice$y_MA[i.bline.rev] - pred.bline.rev < 0), count(Daily.Matrice$y_MA[i.bline.rev] - bline.cont < 0),
                                             count(Daily.Matrice$y_MA[i.bline.rev] - pred.bline < 0))))
                }
                if (i.min == 1L) {
                    pred.bline <- pred.bline.rev
                    Meas_Funct <- "prev.model"
                } else if (i.min == 2L) {
                    pred.bline <- bline.cont
                    Meas_Funct <- "bline.cont."}
                adjust.negatives <- TRUE
            } else {
                pred.bline[match(i.bline.rev, i.which)] <- pred.bline.rev
                model.nls  <- model.nls.rev
                Meas_Funct <- "rev.nls"
                adjust.negatives <- FALSE
                rm(pred.bline.rev, i.bline.rev, Starting.values, model.nls.rev)
            }
        } else {
            pred.bline <- bline.cont
            Meas_Funct <- "bline.cont."
        }
    }
    # adjusting the negative predicted baseline (< -10), if exists
    if (exists('adjust.negatives') && adjust.negatives) {
        # linearly correcting the baseline where residuals < -10
        ## determining index of <-10
        i.too.low <- sort(unique(c(i.which[which(Daily.Matrice$y_MA[i.which] - pred.bline < -10)], i.which[which(pred.bline < -10)])))
        if (identical(i.too.low, i.which)) {
            if (exists('bline.cont')) {
                bline.tmp <- bline.cont
            } else {
                bline.tmp <- Daily.Matrice$Sum_eff[i.which] - Daily.Matrice$Sum_eff[i.which][1] + Last.Bline
            }
            if (count(Daily.Matrice$y_MA[i.which] - bline.tmp < -10) > 10 || any(Daily.Matrice$y_MA[i.which] - bline.tmp < -20)) {
                ## determining index of <-10
                i.too.low <- sort(unique(c(i.which[which(Daily.Matrice$y_MA[i.which] - bline.tmp < -10)], i.which[which(bline.tmp < -10)])))
                pred.bline <- bline.tmp
                Meas_Funct <- "bline.cont."
                rm(bline.tmp)
            } else {
                pred.bline <- bline.cont
                Meas_Funct <- "bline.cont. & adj4negatives"
                break
            }
        } 
        limit.too.low <- c(min(i.too.low), i.too.low[which(diff(i.too.low) != 1)], i.too.low[which(diff(i.too.low) != 1)+1], max(i.too.low))
        i.jumps <- data.table::data.table(i.Start = sapply(seq_along(limit.too.low)[seq(from = 1, to = length(limit.too.low), by = 2)], function(k) {limit.too.low[k]}))
        i.jumps[, i.End := sapply(seq_along(limit.too.low)[seq(from = 2, to = length(limit.too.low), by = 2)], function(k) {limit.too.low[k]})]
        for (tlow in seq_along(i.jumps$i.Start)) {
            i.tlow <- i.jumps[tlow]$i.Start:i.jumps[tlow]$i.End
            # checking if the < -10 part starts at the beginning of the region
            # baseline continuity along with the -10 part
            if (base::min(i.which) %in% i.tlow) {
                i.calc <- base::max(i.tlow) + 1
                bline.new <- pred.bline[match(i.calc, i.which)]/Daily.Matrice[i.calc][[var]]*Daily.Matrice[i.tlow][[var]]
            } else if (base::max(i.which) %in%  i.tlow) {
                i.calc <- base::min(i.tlow) - 1
                bline.new <- pred.bline[match(i.calc, i.which)]/Daily.Matrice[i.calc][[var]]*Daily.Matrice[i.tlow][[var]]
            } else {
                i.calc.min    <- base::min(i.tlow) - 1
                bline.new.min <- pred.bline[match(i.calc.min, i.which)]/Daily.Matrice[i.calc.min][[var]]*Daily.Matrice[i.tlow][[var]]
                i.calc.max    <- base::max(i.tlow) + 1
                bline.new.max <- pred.bline[match(i.calc.max, i.which)]/Daily.Matrice[i.calc.max][[var]]*Daily.Matrice[i.tlow][[var]]
                # the min of two
                bline.new     <- matrixStats::rowMins(as.matrix(data.table::data.table(min = bline.new.min, max = bline.new.max)))
                rm(bline.new.min, bline.new.max)
            }
            # Updating the values in the predicted baseline
            pred.bline[match(i.tlow, i.which)] <- bline.new
            rm(bline.new)
        }
        # }
        Meas_Funct <- paste0(Meas_Funct, " & adj4negatives")
        # }
    }
    
    # deciding on whether we assign this model as the last good model to be utilized to predict when there is no data to fit nls model in the future
    if (!berryFunctions::is.error((broom::tidy(model.nls))) &&
        Meas_Funct == "nls" && length(i.bline) > 150) { # data used to fit nls should be at least 150 to avoid assigning last model with too few data
        if (RH.sign == 'decr') {
            if (broom::tidy(model.nls)[[2]][1] > broom::tidy(model.nls)[[2]][2] && # yf_RH should be > y0_RH
                broom::tidy(model.nls)[[2]][5] > broom::tidy(model.nls)[[2]][4]) { # yf_T should be < y0_T
                Dec.Last.nls.model <- model.nls
            } 
        } else {
            if (broom::tidy(model.nls)[[2]][1] < broom::tidy(model.nls)[[2]][2] && # yf_RH < y0_RH
                broom::tidy(model.nls)[[2]][5] > broom::tidy(model.nls)[[2]][4]) { # yf_T should be < y0_T
                Inc.Last.nls.model <- model.nls
            }
        }
    }
    if (berryFunctions::is.error(broom::glance(model.nls)[["sigma"]])) rmse <- 1000 else rmse <- broom::glance(model.nls)[["sigma"]]
    return(list(nls.model = model.nls, Daily.Matrice =  Daily.Matrice, Meas_Funct = Meas_Funct, rmse = rmse, pred.bline = pred.bline,
                Inc.Last.nls.model = Inc.Last.nls.model, Dec.Last.nls.model = Dec.Last.nls.model)) 
}

###Set_nls_Model: Function to predict the baseline using the pseudo basline (from either previous model of Sum effect) vs yMA linear models at yMA possibly Baseline ====
#================================================================CR
#' This function sets the baseline based on the RH regions, nls model fitted in the best estimation of baseline determined by MLR between yMA and T eff + RH eff  
#' @param Daily.Matrice  the time series of sensor response, T, RH and RH-derivatives of half day
#' @param i.which  the start-end of region
#' @param i.nls  the start-end of baseline peaks (R2 between y_MA and sum_eff > 0.80)
#' @param RH.sign  the sign of region's RH, decreasing of increasing
#' @param Inc.Last.nls.model  The last nls model where RH increasing. we pass this model to Set.nls.Model function
#' @param Dec.Last.nls.model  The last nls model where RH decreasing. we pass this model to Set.nls.Model function
#' @param row.reg  the index of the region
#' @param Last.Bline  the last predicted baseline
#' @param thrs.bline  the threshold of Delta Sum Effect where we evaluate if the variation of sum effect is enough for an LRM and nls models
#' @param var the variable, 'pseu.bline' or 'Sum_eff', which is used to evaluate the variation of expected baseline in the dataset against thrs.bline
#' @param thrs.neg  the negative threshold of baseline or baseline subtracted-yMA that we dont want to go below
#' @return the baseline model
Set_nls_Model2 <- function(Daily.Matrice, i.which, i.nls, RH.sign, Inc.Last.nls.model,Dec.Last.nls.model, row.reg, Regions, Last.Bline,
                           thrs.bline, name.T, name.RH, var, thrs.neg, ...) { 
    # if the delta pseudo baseline of the half day is too low, then we don't try to fit a nls model
    # if (diff(range(na.omit(Daily.Matrice[['Sum_eff']])))  > thrs.bline ||
    #     diff(range(na.omit(Daily.Matrice[['pseu.bline']]))) > thrs.bline) {
    if (!is.null(Last.Bline) && is.na(Last.Bline)) Last.Bline <- NULL
    # if (!exists("off.set") && any(Daily.Matrice$y_MA < thrs.neg)) { # Checking if any yMA < 0. If so, we will offset-correct using the min yMA
    #     # if (!exists("off.set") && any(Daily.Matrice$y_MA[i.which] < 0)) { # Checking if any yMA < 0. If so, we will offset-correct using the min yMA
    #     # off.set <- min(Daily.Matrice$y_MA[i.which], na.rm = T)
    #     off.set <- min(Daily.Matrice$y_MA, na.rm = T)
    #     data.table::set(Daily.Matrice, i = i.which, j = 'y_MA', value = Daily.Matrice$y_MA[i.which] - off.set)
    # } else off.set <- NULL
    
    if (diff(range(na.omit(Daily.Matrice[['Sum_eff']])))  > thrs.bline ||
        diff(range(na.omit(Daily.Matrice[['pseu.bline']])))  > thrs.bline) {
        if (nrow(Daily.Matrice[i.which][dRH < 0]) > length(i.which)/2){ # RH decreasing
            r_RH <- 1.033
            RH.sign <- 'decr'
            Last.Model <- Dec.Last.nls.model
        } else {
            r_RH <- 0.97
            RH.sign <- 'incr'
            Last.Model <- Inc.Last.nls.model
        }
        # if (!exists("off.set") && any(Daily.Matrice$y_MA[i.which] < 0)) { # Checking if any yMA < 0. w=If so, we will offset-correct using the min yMA
        #     off.set <- min(Daily.Matrice$y_MA[i.which], na.rm = T)
        #     data.table::set(Daily.Matrice, i = i.which, j = 'y_MA', value = Daily.Matrice$y_MA[i.which] - off.set)
        outliers.BlineFalse <- Find.outlier3(i.which = i.which, Daily.Matrice = Daily.Matrice, var2LM = var, Last.Bline = Last.Bline, thrs.neg = thrs.neg,
                                             Last.Model = Last.Model, RH.sign = RH.sign, name.T = name.T, name.RH = name.RH) #, off.set = off.set
        # } else {
        #     outliers.BlineFalse <- Find.outlier3(i.which = i.which, Daily.Matrice = Daily.Matrice, var2LM = var, Last.Bline = Last.Bline, thrs.neg = thrs.neg,
        #                                          Last.Model = Last.Model, RH.sign = RH.sign, name.T = name.T, name.RH = name.RH, off.set = NULL)
        # }
        if (exists("outliers.BlineFalse")) Daily.Matrice <- outliers.BlineFalse$Daily.Matrice
        ## determining the curvature factor for T at 20 C, based on the lab results
        r_T <- 1.08
        ## adding curvetured T and RH effects to Daily Matrice
        data.table::set(Daily.Matrice, i = i.which, j='cur.T', value = r_T^(Daily.Matrice[i.which][[name.T]]-9.15))
        data.table::set(Daily.Matrice, i = i.which, j='cur.RH', value = r_RH^(Daily.Matrice[i.which][[name.RH]]-40))
        if (!'Outliers' %in% names(Daily.Matrice)) data.table::set(Daily.Matrice, j='Outliers', value = !Daily.Matrice$Baseline)
        # If peak evaluation (peaks to be baseline or not) was previously done, we don't check for the outliers by cook's distance
        ## Determining the Baseline TRUE part of the region
        if (!is.null(i.nls))  i.bline <- base::intersect(i.nls, i.which) else i.bline <- intersect(i.which, Daily.Matrice[Outliers != TRUE, which = TRUE]) # Daily.Matrice[i.which][Outliers != TRUE, which = TRUE] # i.which[which(!Daily.Matrice$Outliers[i.which])]
        if (exists('i.bline')) {
            if (length(i.bline) > 15 && length(i.bline)/length(i.which) > 0.20) {# checking if we have at least 1 h AND 25% of region that baseline is TRUE
                # if (length(i.bline) > 60 || length(i.bline)/length(i.which) > 0.25) {# checking if we have at least 1 h or 25% of region that baseline is TRUE
                if (diff(range(Daily.Matrice[['Sum_eff']][i.bline], na.rm = T)) > thrs.bline || # sum eff is higher the threshold
                    diff(range(Daily.Matrice[['pseu.bline']][i.bline], na.rm = T)) > thrs.bline || # pseu baseline is higher the threshold
                    min(Daily.Matrice$y_MA[i.which], na.rm = T) - min(Daily.Matrice$y_MA, na.rm = T) > 2*thrs.bline) {  # to address the increase (recovery) of yMA after a sudden decrease due to sharp change in T and/or RH
                    if (nrow(Daily.Matrice[i.which][dRH < 0]) > length(i.which)/2) RH.sign <- 'decr' else RH.sign <- 'incr'
                    Starting.values <- Starting_values(Daily=Daily.Matrice, RH.sign = RH.sign, i.which = i.bline,  r_T = r_T, r_RH = r_RH, 
                                                       name.T = name.T, name.RH = name.RH)
                    #weights (equally distributed)
                    wi <- rep(1/length(i.bline), times = length(i.bline))
                    model.nls <- nls_model(Daily = Daily.Matrice[i.bline], RH.sign = RH.sign, Dec.Last.nls.model = Dec.Last.nls.model, wi = wi,
                                           Inc.Last.nls.model = Inc.Last.nls.model, Starting.values = Starting.values, name.T = name.T, name.RH = name.RH)
                    # Checking if any of model residuals < thrs.neg. If so we will re-model using the weights
                    if (any(resid(model.nls) < thrs.neg)) {
                        #calculating the weights
                        wi <- abs(resid(model.nls))/sum(abs(resid(model.nls)))
                        # re-fitting nls model with weights
                        model.nls <- nls_model(Daily = Daily.Matrice[i.bline], RH.sign = RH.sign, Dec.Last.nls.model = Dec.Last.nls.model, wi = wi,
                                               Inc.Last.nls.model = Inc.Last.nls.model, Starting.values = Starting.values, name.T = name.T, name.RH = name.RH)
                    }
                    
                    Meas_Funct <- "nls"
                    # predicting using nls.model
                    pred.bline <- predict(model.nls, newdata = Daily.Matrice[i.which])
                    if (all(Daily.Matrice$y_MA[i.which] >= 0)) {
                        # checking if too many baseline subtracted y_MA < thrs.neg
                        if (count(Daily.Matrice$y_MA[i.which] - pred.bline < thrs.neg) > 10) negatives <- TRUE else negatives <- FALSE
                    } else { # in case of any y_MA is negative we don't test the negativeness of baseline since is expected to be negative
                        negative.yMA <- TRUE
                        if (count(Daily.Matrice$y_MA[i.which] - pred.bline < thrs.neg) > 10 || any(Daily.Matrice$y_MA[i.which] - pred.bline < -20)) negatives <- TRUE else negatives <- FALSE
                    }
                    if (count(pred.bline < thrs.neg) > 10) { # in case there are too many baseline below the threshold, we will check if the previous model (and then baseline continuity) works better  
                        neg.bline <- TRUE 
                    } else neg.bline <- FALSE
                    if (negatives || neg.bline) { # checking if too many baseline subtracted y_MA < thrs.neg
                        if (negatives) {
                            i.negatives <- sort(unique(c(i.which[which(Daily.Matrice$y_MA[i.which] - pred.bline < thrs.neg)])))
                        } else if (count(pred.bline < thrs.neg)/length(i.which) > 0.50) {
                            i.negatives <- i.which
                        } else i.negatives <- sort(unique(c(i.which[which(pred.bline < thrs.neg)])))
                        # Checking if the predicting using previous model improves negatives
                        if (RH.sign == 'decr') prev.model <- Dec.Last.nls.model else prev.model <- Inc.Last.nls.model
                        if (!is.null(prev.model)) {
                            pred.bline.prev.model <- predict(prev.model, newdata=Daily.Matrice[i.negatives])
                            if (count(Daily.Matrice$y_MA[i.negatives] - pred.bline.prev.model < thrs.neg) > 10 || # checking if too many baseline subtracted y_MA < thrs.neg
                                count(pred.bline.prev.model < thrs.neg) > 10 || # checking if too many baseline < thrs.neg
                                any(pred.bline.prev.model < -20)) { # checking if any  baseline < -20
                                # Checking if fitting polynomial model improves negative residuals
                                model.poly <- lm(y_MA ~ get(var) + I(get(var)^2), data = Daily.Matrice[i.which])
                                # predicting using model.poly (LMmodel)
                                pred.poly <-  predict(model.poly, newdata=Daily.Matrice[i.which])
                                if (count(pred.poly < thrs.neg) > 10 || # checking if too many predicted bline < thrs.neg
                                    count(Daily.Matrice$y_MA[i.which] - pred.poly < thrs.neg) > 10) { # checking if too many baseline subtracted y_MA < thrs.neg
                                    do.sub.model <- TRUE
                                } else {
                                    pred.bline <- pred.poly
                                    model.nls  <- model.poly
                                    Meas_Funct <- "Polynomial"
                                    # adjust.negatives <- FALSE
                                }
                            } else if (max(pred.bline[match(i.negatives, i.which)] - pred.bline.prev.model) > 10) { # checking if the revised baseline decreases too much compared to the original one (that would result in enormously high over-prediction)
                                if (!is.null(Last.Bline)) { # we will establish the within-region-baseline-continuity for the negatives
                                    if (length(i.negatives)/length(i.which) > 0.50) do.bline.cont <- TRUE else do.bline.cont <- FALSE
                                    # do.bline.cont <- FALSE
                                    do.within_reg_bline_cont <- TRUE
                                } else do.bline.cont <- FALSE
                            } else {
                                pred.bline[match(i.negatives, i.which)] <- pred.bline.prev.model
                                Meas_Funct <- paste0( Meas_Funct,"&PrevModel")
                                # checking if too many predicted bline < thrs.neg
                                rm(pred.bline.prev.model)
                                # adjust.negatives <- FALSE
                            }
                        } else if (!is.null(Last.Bline)) { # we will establish the within-region-baseline-continuity for the negatives
                            if (length(i.negatives)/length(i.which) > 0.50) do.bline.cont <- TRUE else do.bline.cont <- FALSE
                            # do.bline.cont <- FALSE
                            do.within_reg_bline_cont <- TRUE
                        } else do.bline.cont <- FALSE
                    } 
                } else {
                    do.prev.model <- TRUE
                }
            } else {
                do.prev.model <- TRUE}
        } else {
            do.prev.model <- TRUE}
    } else {
        do.prev.model <- TRUE}
    # if (as.Date('2020-10-12') %in% unique(as.Date(Daily.Matrice$date[i.which])) ||
    #     as.Date('2020-08-19') %in% unique(as.Date(Daily.Matrice$date[i.which])) ||
    #     as.Date('2021-02-27') %in% unique(as.Date(Daily.Matrice$date[i.which])) ||
    #     as.Date('2020-04-08') %in% unique(as.Date(Daily.Matrice$date[i.which]))) browser()
    if (exists("do.prev.model")) {
        if (!'Outliers' %in% names(Daily.Matrice)) data.table::set(Daily.Matrice, j = 'Outliers', value = TRUE)
        if (RH.sign == 'decr') {
            if (!is.null(Dec.Last.nls.model)) model.nls  <- Dec.Last.nls.model else model.nls  <- NULL
        } else if (!is.null(Inc.Last.nls.model)) model.nls  <- Inc.Last.nls.model else model.nls  <- NULL
        if (!is.null(model.nls)) {
            Meas_Funct <- "prev.model"
            pred.bline <- predict(model.nls, newdata = Daily.Matrice[i.which])
            # if (!is.null(off.set)) pred.bline <- pred.bline - off.set
            # offset correction only when the predicted baseline is from prev.model
            if (!is.null(Last.Bline)) {
                # if (is.null(off.set)) {
                if (Last.Bline > thrs.neg/2) {
                    if (abs(pred.bline[1] - Last.Bline) < 2*-thrs.neg) { # we don't want the start of the predicted baseline by previous model being too different than the last baseline
                        # if (abs(pred.bline[1] - Last.Bline) < 2*-thrs.neg) { # we don't want the start of the predicted baseline by previous model being too different than the last baseline
                        pred.bline <- pred.bline + Last.Bline - pred.bline[1]
                        Meas_Funct <- paste0(Meas_Funct ,"_offsetCorr.")
                    } else { # we check if the min y_MA is too high 
                        
                    }
                } else {
                    do.bline.cont <- FALSE
                    offset.corr <- FALSE}
                # } else {
                #     pred.bline <- pred.bline + Last.Bline - pred.bline[1] - off.set
                #     Meas_Funct <- paste0(Meas_Funct ,"_offsetCorr.")
                # }
                # 
            } else {
                # if (!is.null(off.set)) {
                #     pred.bline <- pred.bline - pred.bline[1] - off.set
                #     Meas_Funct <- paste0(Meas_Funct ,"_offsetCorr.")
                # }
                do.bline.cont <- FALSE
                offset.corr <- FALSE
            }
            
            # if (!is.null(Last.Bline)  && 
            #     Last.Bline > thrs.neg/2) { # we don't want to correct offset when the last baseline is too negative
            #     if (abs(pred.bline[1] - Last.Bline) < 2*-thrs.neg) { # we don't want the start of the predicted baseline by previous model being too different than the last baseline
            #         # if (abs(pred.bline[1] - Last.Bline) < 2*-thrs.neg) { # we don't want the start of the predicted baseline by previous model being too different than the last baseline
            #         pred.bline <- pred.bline + Last.Bline - pred.bline[1]
            #         Meas_Funct <- paste0(Meas_Funct ,"_offsetCorr.")
            #     }
            # } else {
            #     do.bline.cont <- FALSE
            #     offset.corr <- FALSE}
            # 
            # checking if the baseline or baseline-corrected-yMA is too negative
            if (count(Daily.Matrice$y_MA[i.which] - pred.bline < thrs.neg) > 10 || 
                any(Daily.Matrice$y_MA[i.which] - pred.bline < -20)) {
                i.negatives <- sort(unique(c(i.which[which(Daily.Matrice$y_MA[i.which] - pred.bline < thrs.neg)])))
                if (length(i.negatives)/length(i.which) > 0.50) {
                    do.sub.model <- FALSE
                    do.bline.cont <- TRUE
                } else {
                    do.sub.model <- TRUE
                    if (!exists("do.bline.cont")) {
                        do.bline.cont <- TRUE
                        if (identical(i.negatives, i.which)) {
                            # checking the baseline continuity for the entire region
                            do.within_reg_bline_cont <- FALSE
                        } else {
                            do.within_reg_bline_cont <- TRUE
                        }
                    }
                }
            } else if (!is.null(Last.Bline) && Last.Bline > thrs.neg && abs(pred.bline[1] - Last.Bline) > 2*-thrs.neg) { # checking if the 1st predicted baseline if too different from the last bline of previous region.
                # } else if (!is.null(Last.Bline) && abs(pred.bline[1] - Last.Bline) > 2*-thrs.neg) { # checking if the 1st predicted baseline if too different from the last bline of previous region.
                too.high.reg.trans <- TRUE
                do.bline.cont <- TRUE
                do.within_reg_bline_cont <- FALSE
            } 
        } else if (!is.null(Last.Bline)) { # no previous model but baseline continuity possible
            do.bline.cont <- TRUE
            do.within_reg_bline_cont <- FALSE
        } else { # no previous model and baseline continuity
            Meas_Funct   <- "no prev.bline"
            rmse         <- 0  
            pred.bline   <-  rep(NA_real_, times = length(i.which))
            nls.model    <- NULL
        }
        # checking if the baseline below threshold is too many. If so, we will try the baseline continuity
        if (exists('pred.bline') && !all(is.na(pred.bline)) && 
            (count(pred.bline < thrs.neg)/length(pred.bline) > 0.50 || min(pred.bline, na.rm = T) < 2*thrs.neg)) {
            too.neg.bline <- TRUE
            do.bline.cont <- TRUE
            do.within_reg_bline_cont <- FALSE
        }
        
    }
    
    if (!exists("Meas_Funct") || Meas_Funct != "no prev.bline") {
        # checking for baseline continuity
        if (exists("do.bline.cont")) {
            if (do.bline.cont) {
                bline.cont <- Daily.Matrice$Sum_eff[i.which] - Daily.Matrice$Sum_eff[i.which][1] + Last.Bline
                # if (!is.null(off.set)) bline.cont <- bline.cont - off.set
                if (!exists("pred.bline")) pred.bline <- bline.cont
                if (!exists("Meas_Funct")) Meas_Funct <- "bline.cont"
                if (count(Daily.Matrice$y_MA[i.which] - bline.cont < thrs.neg) > 10 || 
                    any(Daily.Matrice$y_MA[i.which] - bline.cont < -20)) {
                    if (exists("too.high.reg.trans") && !exists("too.neg.bline")) { # it means previous model satisfies all requirements for negatives, but the 1st value is too different from the last baseline
                        do.sub.model <- FALSE
                    } else { # we will select either pred.bline or baseline continuity to continue with.
                        # we select among previous-model-predicted baseline and baseline continuity yielding the minimum negative baseline and baseline-subtracted values
                        if (count(Daily.Matrice$y_MA[i.which] - bline.cont < 0) + count(bline.cont < 0) < # counting baseline subtracting yMA < 0 and baseline < 0
                            count(Daily.Matrice$y_MA[i.which] - pred.bline < 0) + count(pred.bline < 0)) { # counting baseline-continuity subtracting yMA < 0 and baseline-continuity < 0
                            
                            
                            # if (count(Daily.Matrice$y_MA[i.which] - bline.cont < 0) <
                            #     count(Daily.Matrice$y_MA[i.which] - pred.bline < 0)) { # we select among previous-model-predicted baseline and baseline continuity yielding the minimum negative baseline subtracted values
                            pred.bline <- bline.cont
                            Meas_Funct <- "bline.cont"
                            # updating the indexes of negatives
                            i.negatives <- sort(unique(c(i.which[which(Daily.Matrice$y_MA[i.which] - pred.bline < thrs.neg)])))
                        }
                    }
                    # checking if the number of negatives is too big compared to the region. if so, we will not do submodel
                    if (exists("i.negatives")) {
                        if (length(i.negatives)/length(i.which) > 0.50) {
                            do.sub.model <- FALSE
                        } else if (length(i.negatives) > 10) do.sub.model <- TRUE
                    }
                } else {
                    do.sub.model <- FALSE
                    pred.bline <- bline.cont
                    Meas_Funct <- "bline.cont"
                    if (exists("do.within_reg_bline_cont")) rm(do.within_reg_bline_cont)
                    if (exists("do.sub.model")) rm(do.sub.model)
                }
            }
            if (exists("do.within_reg_bline_cont") && do.within_reg_bline_cont && 
                !identical(i.negatives, i.which)) {
                limit.negatives <- sort(c(min(i.negatives), i.negatives[which(diff(i.negatives) != 1)], i.negatives[which(diff(i.negatives) != 1)+1], max(i.negatives)))
                i.jumps <- data.table::data.table(i.Start = sapply(seq_along(limit.negatives)[seq(from = 1, to = length(limit.negatives), by = 2)], function(k) {limit.negatives[k]}))
                i.jumps[, i.End := sapply(seq_along(limit.negatives)[seq(from = 2, to = length(limit.negatives), by = 2)], function(k) {limit.negatives[k]})]
                for (tlow in seq_along(i.jumps$i.Start)) {
                    i.tlow <- i.jumps[tlow]$i.Start:i.jumps[tlow]$i.End
                    # checking if the < thrs.neg part starts at the beginning of the region
                    # baseline continuity along with the thrs.neg part
                    if (base::min(i.which) %in% i.tlow) {
                        i.calc <- base::max(i.tlow) + 1
                        bline.tlow <- pred.bline[match(i.calc, i.which)]/Daily.Matrice[i.calc][[var]]*Daily.Matrice[i.tlow][[var]]
                    } else if (base::max(i.which) %in%  i.tlow) {
                        i.calc <- base::min(i.tlow) - 1
                        bline.tlow <- pred.bline[match(i.calc, i.which)]/Daily.Matrice[i.calc][[var]]*Daily.Matrice[i.tlow][[var]]
                    } else {
                        i.calc.min    <- base::min(i.tlow) - 1
                        bline.tlow.min <- pred.bline[match(i.calc.min, i.which)]/Daily.Matrice[i.calc.min][[var]]*Daily.Matrice[i.tlow][[var]]
                        i.calc.max    <- base::max(i.tlow) + 1
                        bline.tlow.max <- pred.bline[match(i.calc.max, i.which)]/Daily.Matrice[i.calc.max][[var]]*Daily.Matrice[i.tlow][[var]]
                        # the min of two
                        bline.tlow     <- matrixStats::rowMins(as.matrix(data.table::data.table(min = bline.tlow.min, max = bline.tlow.max)))
                        rm(bline.tlow.min, bline.tlow.max)
                    }
                    if (exists('bline.cont.neg')) bline.cont.neg <- c(bline.cont.neg, bline.tlow) else bline.cont.neg <- bline.tlow
                    rm(bline.tlow)
                }
                # checking if the yMA corrected by baseline continuity < thrs.neg
                if (count(Daily.Matrice$y_MA[i.negatives] - bline.cont.neg < thrs.neg) > 10) {
                    do.sub.model <- TRUE
                } else {
                    pred.bline[match(i.negatives, i.which)] <- bline.cont.neg
                    Meas_Funct <- "bline.cont"
                    do.sub.model <- FALSE
                }
                rm(bline.cont.neg)
            } 
        }
        
        if (exists("do.sub.model") && do.sub.model) {
            # if (any(Daily.Matrice$y_MA[i.which] < 0)) negative.yMA <- TRUE 
            i.bline.rev <- sort(unique(c(i.which[which(Daily.Matrice$y_MA[i.which] - pred.bline < thrs.neg)], i.which[which(pred.bline < thrs.neg)])))
            # resetting nls model with assigning all pred.bline < thrs.neg outlier false
            if (!"cur.T" %in% names(Daily.Matrice) || !"cur.RH" %in% names(Daily.Matrice)) {
                ## determining the curvature factor for T at 20 C, based on the lab results
                r_T <- 1.08
                ## adding curvatured T and RH effects to Daily Matrice
                data.table::set(Daily.Matrice, i = i.bline.rev, j='cur.T', value = r_T^(Daily.Matrice[i.bline.rev][[name.T]]-9.15))
                if (nrow(Daily.Matrice[i.which][dRH < 0]) > length(i.which)/2){ # RH decreasing
                    r_RH <- 1.033
                } else {
                    r_RH <- 0.97
                }
                data.table::set(Daily.Matrice, i = i.bline.rev, j='cur.RH', value = r_RH^(Daily.Matrice[i.bline.rev][[name.RH]]-40))   
            }
            Starting.values <- Starting_values(Daily=Daily.Matrice, RH.sign = RH.sign, i.which = i.bline.rev,  r_T = r_T, r_RH = r_RH, 
                                               name.T = name.T, name.RH = name.RH)
            #weights (equally distributed)
            wi <- rep(1/length(i.bline.rev), times = length(i.bline.rev))
            model.nls.rev <- nls_model(Daily = Daily.Matrice[i.bline.rev], RH.sign = RH.sign, Dec.Last.nls.model = Dec.Last.nls.model, wi = wi,
                                       Inc.Last.nls.model = Inc.Last.nls.model, Starting.values = Starting.values, name.T = name.T, name.RH = name.RH)
            # predict
            pred.bline.rev <- predict(model.nls.rev, newdata = Daily.Matrice[i.bline.rev])
            # offset correction only when the predicted baseline is from prev.model and if submodel contains the beginnig of the region (already offset corrected the rest)
            if (!exists("offset.corr") && Meas_Funct == "prev.model" && i.which[1] %in% i.bline.rev && !is.null(Last.Bline) ) {
                # browser()
                pred.bline.rev <- pred.bline.rev + Last.Bline - pred.bline.rev[1] 
            }
            # checking if sub-model satisfies the criteria
            if (count(Daily.Matrice$y_MA[i.bline.rev] - pred.bline.rev < thrs.neg) < 10 && 
                all(Daily.Matrice$y_MA[i.bline.rev] - pred.bline.rev > -20)) {
                pred.bline[match(i.bline.rev, i.which)] <- pred.bline.rev
                # model.nls  <- model.nls.rev
                # if (grepl('prev.model', Meas_Funct)) browser()
                Meas_Funct <- paste0(Meas_Funct ,"_rev")
                # adjust.negatives <- FALSE
                if (!"Outliers" %in% names(Daily.Matrice) || all(is.na(Daily.Matrice$Outliers[i.bline.rev]))) {
                    data.table::set(Daily.Matrice, i = i.bline.rev, j ="Outliers", value = FALSE)
                }
            }
            rm(pred.bline.rev, i.bline.rev, Starting.values, model.nls.rev)
        } 
        
        # # offset correction only when the predicted baseline is from prev.model
        # if (exists("Meas_Funct") && Meas_Funct %in% c("prev.model", "prev.model_rev") && !is.null(Last.Bline)) { 
        #     pred.bline <- pred.bline + Last.Bline - pred.bline[1]
        #     Meas_Funct <- paste0(Meas_Funct ,"_offsetCorr.")
        # }
        # 
        # adjusting the negative baseline (< thrs.neg) to yMA, if exists
        if (!exists('negative.yMA') && count(pred.bline < thrs.neg) > 10 ||
            count(pred.bline < -20) > 10) { # && all(pred.bline > 0)
            # if (!exists("bline.cont"))  bline.cont <- Daily.Matrice$Sum_eff[i.which] - Daily.Matrice$Sum_eff[i.which][1] + Last.Bline
            # # checking if baseline continuity satisfies all criteria
            # if (!exists('negative.yMA') && count(bline.cont < thrs.neg) > 10 ||
            #     count(bline.cont < -20) > 10 || count(Daily.Matrice$y_MA[i.which] - bline.cont < thrs.neg) > 10) bline.cont.satisfy <- FALSE else bline.cont.satisfy <- TRUE
            # if (all(Daily.Matrice$y_MA > 0)) {
            if (all(pred.bline < 0)) {
                # replace with the minimum yMA or minimum bline than > thrs.neg (if exists)
                # browser()
                # if (!is.null(off.set)) {
                #     val2replace <- off.set 
                # } else 
                if (max(pred.bline) > thrs.neg) {
                    val2replace <- max(pred.bline, na.rm = T)
                } else val2replace <- min(Daily.Matrice$y_MA[i.which], na.rm = T)
                pred.bline <- rep(val2replace, times = length(pred.bline))
                Meas_Funct <- paste0(Meas_Funct, " & adj4neg.min_yMA")
                adj.neg <- FALSE
            } else {
                ## determining index of <thrs.neg
                i.bline.low <-  which(pred.bline < thrs.neg)
                # linearly correcting the baseline where residuals < thrs.neg
                if (length(i.bline.low) == length(i.which)) { # checking if all predicted bline is lower than thrs.neg
                    if (exists('bline.cont')) {
                        if (identical(bline.cont, pred.bline)) { # we will replace with min yMA
                            # replace with the minimum yMA or minimum bline than > thrs.neg (if exists)
                            if (max(pred.bline) > thrs.neg) val2replace <- max(pred.bline, na.rm = T) else val2replace <- min(Daily.Matrice$y_MA[i.which], na.rm = T)
                            pred.bline[match(i.bline.low, i.which)] <- rep(min(Daily.Matrice$y_MA[i.which], na.rm = T), times = length(i.bline.low))
                            Meas_Funct <- paste0(Meas_Funct, " & adj4neg.min_yMA")
                        }
                    } else {
                        bline.tmp <- Daily.Matrice$Sum_eff[i.which] - Daily.Matrice$Sum_eff[i.which][1] + Last.Bline
                        if (count(bline.tmp < thrs.neg) > 10 || any(bline.tmp < -20)) {
                            ## determining index of <thrs.neg
                            i.bline.low <- sort(unique(c(bline.tmp < thrs.neg)))
                            # pred.bline <- bline.tmp
                            Meas_Funct <- "bline.cont."
                        } else {
                            # pred.bline <- bline.tmp
                            Meas_Funct <- "bline.cont. & adj4neg.bline"
                            rm(i.bline.low)
                        }
                        pred.bline <- bline.tmp
                        # if (!is.null(off.set)) pred.bline <- pred.bline - off.set
                        rm(bline.tmp)
                    }
                }
            }
            # }
            if (exists("i.bline.low")) {
                if (!(grepl('prev.model', Meas_Funct) && var == 'pseu.bline')) { # in such case, the baseline predicted by the previous model and baseline continuity will be identical. No need to make the negative adjustment
                    limit.bline.low <- sort(c(min(i.bline.low), i.bline.low[which(diff(i.bline.low) != 1)], i.bline.low[which(diff(i.bline.low) != 1)+1], max(i.bline.low)))
                    i.jumps <- data.table::data.table(i.Start = sapply(seq_along(limit.bline.low)[seq(from = 1, to = length(limit.bline.low), by = 2)], function(k) {limit.bline.low[k]}))
                    i.jumps[, i.End := sapply(seq_along(limit.bline.low)[seq(from = 2, to = length(limit.bline.low), by = 2)], function(k) {limit.bline.low[k]})]
                    for (b.low in seq_along(i.jumps$i.Start)) {
                        i.b.low <- i.jumps[b.low]$i.Start:i.jumps[b.low]$i.End
                        # checking if the < thrs.neg part starts at the beginning of the region
                        # baseline continuity along with the thrs.neg part
                        if (base::min(i.which) %in% i.which[i.b.low]) {
                            i.calc <- base::max(i.b.low) + 1
                            if (all(Daily.Matrice[i.which[i.b.low]][[var]] < 0)) { # if everything is negative, division instead of multiplication to make baseline around thrs.neg
                                bline.new <- pred.bline[i.calc]/Daily.Matrice[i.which[i.calc]][[var]]/Daily.Matrice[i.which[i.b.low]][[var]]
                            } else bline.new <- pred.bline[i.calc]/Daily.Matrice[i.which[i.calc]][[var]]*Daily.Matrice[i.which[i.b.low]][[var]]
                        } else if (base::max(i.which) %in%  i.which[i.b.low]) {
                            i.calc <- base::min(i.b.low) - 1
                            if (all(Daily.Matrice[i.which[i.b.low]][[var]] < 0)) { # if everything is negative, division instead of multiplication to make baseline around thrs.neg
                                bline.new <- pred.bline[i.calc]/Daily.Matrice[i.which[i.calc]][[var]]/Daily.Matrice[i.which[i.b.low]][[var]]
                            } else bline.new <- pred.bline[i.calc]/Daily.Matrice[i.which[i.calc]][[var]]*Daily.Matrice[i.which[i.b.low]][[var]]
                            
                        } else {
                            i.calc.min    <- base::min(i.b.low) - 1
                            i.calc.max    <- base::max(i.b.low) + 1
                            if (all(Daily.Matrice[i.which[i.b.low]][[var]] < 0)) { # if everything is negative, division instead of multiplication to make baseline around thrs.neg
                                bline.new.min <- pred.bline[i.calc.min]/Daily.Matrice[i.which[i.calc.min]][[var]]/Daily.Matrice[i.which[i.b.low]][[var]]
                                bline.new.max <- pred.bline[i.calc.max]/Daily.Matrice[i.which[i.calc.max]][[var]]/Daily.Matrice[i.which[i.b.low]][[var]]
                            } else {
                                bline.new.min <- pred.bline[i.calc.min]/Daily.Matrice[i.which[i.calc.min]][[var]]*Daily.Matrice[i.which[i.b.low]][[var]]
                                bline.new.max <- pred.bline[i.calc.max]/Daily.Matrice[i.which[i.calc.max]][[var]]*Daily.Matrice[i.which[i.b.low]][[var]]}
                            # the max of two
                            # browser()
                            bline.new     <- matrixStats::rowMaxs(as.matrix(data.table::data.table(min = bline.new.min, max = bline.new.max)))
                            rm(bline.new.min, bline.new.max)
                        }
                        if (exists('bline.neg')) bline.neg <- c(bline.neg, bline.new) else bline.neg <- bline.new
                        rm(bline.new)
                    }
                    
                    # Updating the values in the predicted baseline
                    if(length(pred.bline[i.bline.low]) != length(bline.neg)) browser()
                    pred.bline[i.bline.low] <- bline.neg
                    rm(bline.neg)
                    Meas_Funct <- paste0(Meas_Funct, " & adj4neg.Bline")
                } else { # replacing with the minimum baseline that higher than threshold
                    # if (min(pred.bline[-i.bline.low], na.rm = T) > thrs.neg) val2change <- min(pred.bline[-i.bline.low], na.rm = T) else val2change <- min(Daily.Matrice$y_MA[i.which], na.rm = T)
                    pred.bline[i.bline.low] <- rep(min(pred.bline[-i.bline.low], na.rm = T), times = length(i.bline.low))
                    Meas_Funct <- paste0(Meas_Funct, " & adj4neg.min_yMA")
                }
            }
        }
        # adjusting the too negative predicted-baseline-subtracted-yMA (< thrs.neg), if exists
        if (count(Daily.Matrice$y_MA[i.which] - pred.bline < thrs.neg) > 10) {
            # determining index of <thrs.neg
            i.too.low <-  sort(unique(c(i.which[which(Daily.Matrice$y_MA[i.which] - pred.bline < thrs.neg)])))
            
            # if (count(Daily.Matrice$y_MA[i.which] - pred.bline < 0)/length(i.which) > 0.75) {
            if (identical(i.too.low, i.which)) {
                if (Meas_Funct == 'bline.cont') { # we establish the baseline at the minimum y_MA
                    # if (!is.null(off.set)) {
                    #     val2change <- off.set
                    # } else 
                    if (min(Daily.Matrice$y_MA[i.which], na.rm = T) > thrs.neg) {
                        val2change <- min(Daily.Matrice$y_MA[i.which], na.rm = T) 
                    } else val2change <- thrs.neg
                    pred.bline <- rep(val2change, times = length(i.too.low))
                    # browser()
                    Meas_Funct <- "AllNeg.repl.by.min_yMA"
                    adj.neg <- FALSE
                } else adj.neg <- TRUE
            } else adj.neg <- TRUE
            
            if (adj.neg) {
                if (Meas_Funct != 'bline.cont' && !is.null(Last.Bline)) {
                    if (exists('bline.cont')) { 
                        bline.tmp <- bline.cont
                    } else  {
                        bline.tmp <- Daily.Matrice$Sum_eff[i.which] - Daily.Matrice$Sum_eff[i.which][1] + Last.Bline
                    }
                    # browser()
                    if (max(pred.bline - bline.tmp) < 20) { # checking if the baseline continuity decreases too much compared to the original predicted baseline (that would result in enormously high over-prediction)
                        if (count(Daily.Matrice$y_MA[i.which] - bline.tmp < thrs.neg) > 10 || any(Daily.Matrice$y_MA[i.which] - bline.tmp < -20)) {
                            if (count(Daily.Matrice$y_MA[i.which] - bline.tmp < 0) <
                                count(Daily.Matrice$y_MA[i.which] - pred.bline < 0)) { # we select among previous-model-predicted baseline and baseline continuity yielding the minimum negative baseline subtracted values
                                # pred.bline <- bline.tmp
                                Meas_Funct <- "bline.cont"
                                # updating the indexes of negatives
                                i.too.low <- sort(unique(c(i.which[which(Daily.Matrice$y_MA[i.which] - bline.tmp < thrs.neg)])))
                            }
                        } else {
                            # pred.bline <- bline.tmp
                            Meas_Funct <- "bline.cont. & adj4negatives"
                        }
                        pred.bline <- bline.tmp
                        # if (!is.null(off.set)) pred.bline <- pred.bline - off.set
                    } else {
                        # we will replace only negatives with baseline continuity
                        ## checking if baseline continuity yields negative values
                        if (count(Daily.Matrice$y_MA[i.too.low] - bline.tmp[match(i.too.low, i.which)] < thrs.neg) < 10 || all(Daily.Matrice$y_MA[i.too.low] - bline.tmp[match(i.too.low, i.which)] > -20)) {
                            pred.bline[match(i.too.low, i.which)] <- bline.tmp[match(i.too.low, i.which)]
                        }
                    }
                    rm(bline.tmp)
                }
                
                # in case the negatives are too many compared to the region, it will be hard to correct. Instead we assign baseline as min yMA
                if (count(Daily.Matrice$y_MA[i.which] - pred.bline < 0)/length(i.which) > 0.75 &&
                    length(i.too.low) > 60) {
                    # if (!is.null(off.set)) {
                    #     val2change <- off.set
                    # } else 
                    if (min(Daily.Matrice$y_MA[i.which], na.rm = T) > thrs.neg) {
                        val2change <- min(Daily.Matrice$y_MA[i.which], na.rm = T) 
                    } else val2change <- thrs.neg
                    pred.bline <- rep(val2change, times = length(i.which))
                    Meas_Funct <- "AllNeg.repl.by.min_yMA"
                } else if (Meas_Funct != "bline.cont. & adj4negatives") {
                    limit.too.low <- c(min(i.too.low), i.too.low[which(diff(i.too.low) != 1)], i.too.low[which(diff(i.too.low) != 1)+1], max(i.too.low))
                    i.jumps <- data.table::data.table(i.Start = sapply(seq_along(limit.too.low)[seq(from = 1, to = length(limit.too.low), by = 2)], function(k) {limit.too.low[k]}))
                    i.jumps[, i.End := sapply(seq_along(limit.too.low)[seq(from = 2, to = length(limit.too.low), by = 2)], function(k) {limit.too.low[k]})]
                    for (tlow in seq_along(i.jumps$i.Start)) {
                        # if (as.Date("2020-07-16") %in% unique(as.Date(Daily.Matrice$date[i.which]))) browser() # check if we need match(i.tlow, i.which)
                        i.tlow <- i.jumps[tlow]$i.Start:i.jumps[tlow]$i.End
                        if (all(Daily.Matrice$y_MA[i.tlow] < 0)) { # if all yMA is negative, we assign the minimum yMA as baseline
                            # if (!is.null(off.set)) {
                            #     val2change <- off.set
                            # } else 
                            if (min(Daily.Matrice$y_MA[i.tlow], na.rm = T) > thrs.neg) {
                                val2change <- min(Daily.Matrice$y_MA[i.tlow], na.rm = T) 
                            } else val2change <- thrs.neg
                            bline.new <- rep(val2change, times = length(i.tlow))
                        } else {
                            # checking if the < thrs.neg part starts at the beginning of the region
                            # baseline continuity along with the thrs.neg part
                            if (base::min(i.which) %in% i.tlow) {
                                i.calc.1st <- base::max(i.tlow) + 1
                                bline.new <- pred.bline[match(i.calc.1st, i.which)]/Daily.Matrice[i.calc.1st][[var]]*Daily.Matrice[i.tlow][[var]]
                                if (any(Daily.Matrice$y_MA[i.tlow] - bline.new < thrs.neg) || any(bline.new < thrs.neg)) bline.new <- NULL
                            } else if (base::max(i.which) %in%  i.tlow) {
                                i.calc.last <- base::min(i.tlow) - 1
                                bline.new <- pred.bline[match(i.calc.last, i.which)]/Daily.Matrice[i.calc.last][[var]]*Daily.Matrice[i.tlow][[var]]
                                if (any(Daily.Matrice$y_MA[i.tlow] - bline.new < thrs.neg) || any(bline.new < thrs.neg)) bline.new <- NULL
                            } else {
                                i.calc.min    <- base::min(i.tlow) - 1
                                bline.new.min <- pred.bline[match(i.calc.min, i.which)]/Daily.Matrice[i.calc.min][[var]]*Daily.Matrice[i.tlow][[var]]
                                if (any(Daily.Matrice$y_MA[i.tlow] - bline.new.min < thrs.neg) || any(bline.new.min < thrs.neg)) bline.new.min <- NULL
                                i.calc.max    <- base::max(i.tlow) + 1
                                bline.new.max <- pred.bline[match(i.calc.max, i.which)]/Daily.Matrice[i.calc.max][[var]]*Daily.Matrice[i.tlow][[var]]
                                if (any(Daily.Matrice$y_MA[i.tlow] - bline.new.max < thrs.neg) || any(bline.new.max < thrs.neg)) bline.new.max <- NULL
                                # the min of two
                                if (all(is.null(bline.new.min), is.null(bline.new.max))) bline.new <- NULL else bline.new <- matrixStats::rowMins(as.matrix(data.table::data.table(min = bline.new.min, max = bline.new.max)))
                                rm(bline.new.min, bline.new.max)
                            }
                            # checking if fixes the negatives
                            if (is.null(bline.new) || all(Daily.Matrice$y_MA[i.tlow] - bline.new < thrs.neg)) { # we correct negatives. The goal is to increase the baseline where yMA - baseline is negative
                                #     pred.bline[match(i.tlow, i.which)] <- bline.new
                                # } else { 
                                bline.new <- Daily.Matrice$y_MA[i.tlow] - thrs.bline
                            }  
                        }
                        # Updating the values in the predicted baseline
                        pred.bline[match(i.tlow, i.which)] <- bline.new
                        for (Variable in c("i.calc.last",  "bline.new", "i.calc.1st", "i.calc.min", "i.calc.max", "val2change")) if (exists(Variable)) rm(list = Variable)
                    }
                    Meas_Funct <- paste0(Meas_Funct, " & adj4negatives")
                }   
            }
        }
        # the original yMA if the negative yMA has been offset corrected
        # if (!is.null(off.set)) {
        #     data.table::set(Daily.Matrice, i = i.which, j = 'y_MA', value = Daily.Matrice$y_MA[i.which] + off.set)
        #     pred.bline <- pred.bline + off.set
        #     #Checking if the baseline continuity is not satisfied
        #     # if (grepl('bline.cont', Meas_Funct)) { 
        #     #     if (grepl("adj4neg", Meas_Funct)) { # adjusting for negatives is prone to return too negative baseline, therefore we don't do it ( we put the correction for negatives back)
        #     #         if (grepl('bline.cont', Meas_Funct)) {
        #     #             pred.bline <- bline.cont
        #     #         } else {
        #     #             browser()
        #     #             pred.bline <- Daily.Matrice$Sum_eff[i.which] - Daily.Matrice$Sum_eff[i.which][1] + pred.bline[1]
        #     #         }
        #     #     }
        #     #     if (!is.null(Last.Bline) && abs(Last.Bline - pred.bline[1]) > abs(2*thrs.neg)) {
        #     #         pred.bline <- pred.bline + min(off.set, Last.Bline) - pred.bline[1]
        #     #     } else pred.bline <- min(off.set, Last.Bline)
        #     # } else pred.bline <- pred.bline + off.set #else pred.bline <- pred.bline + min(off.set, Last.Bline)
        #     # if (grepl("adj4negative", Meas_Funct)) pred.bline <- pred.bline - pred.bline[1] + Last.Bline
        #     Meas_Funct <- paste0(Meas_Funct, " & off.set corr.")
        #     rm(off.set)
        # }
        
        if (!is.null(model.nls)) {
            # deciding on whether we assign this model as the last good model to be utilized to predict when there is no data to fit nls model in the future
            if (!berryFunctions::is.error((broom::tidy(model.nls))) &&
                Meas_Funct %in% c("nls") && length(i.bline) > 150) { # data used to fit nls should be at least 150 to avoid assigning last model with too few data  c("nls", "nls & off.set corr.")
                if (RH.sign == 'decr') {
                    if (broom::tidy(model.nls)[[2]][1] > broom::tidy(model.nls)[[2]][2] && # yf_RH should be > y0_RH
                        broom::tidy(model.nls)[[2]][5] > broom::tidy(model.nls)[[2]][4]) { # yf_T should be < y0_T
                        Dec.Last.nls.model <- model.nls
                    } 
                } else {
                    if (broom::tidy(model.nls)[[2]][1] < broom::tidy(model.nls)[[2]][2] && # yf_RH < y0_RH
                        broom::tidy(model.nls)[[2]][5] > broom::tidy(model.nls)[[2]][4]) { # yf_T should be < y0_T
                        Inc.Last.nls.model <- model.nls
                    }
                }
            }
            if (berryFunctions::is.error(broom::glance(model.nls)[["sigma"]])) rmse <- 1000 else rmse <- broom::glance(model.nls)[["sigma"]]
        } else rmse <- 1000
        
        
    }
    
    return(list(nls.model = model.nls, Daily.Matrice =  Daily.Matrice, Meas_Funct = Meas_Funct, rmse = rmse, pred.bline = pred.bline,
                Inc.Last.nls.model = Inc.Last.nls.model, Dec.Last.nls.model = Dec.Last.nls.model)) 
}

#' This function sets the nls model
#' @param Daily  the time series of sensor response, T, RH and RH-derivatives of outlier discarded region
#' @Starting.values list, returned by function Starting_values
#' @param RH.sign  RH sign of region, decreasing or increasing
#' @param Dec.Last.nls.model  the previous nls model for RH decreasing. If no data to fit nls, it returns this model (we changed to return as such to calculate baseline using continuity)
#' @param Inc.Last.nls.model  the previous nls model for RH increasing If no data to fit nls, it returns this model (we changed to return as such to calculate baseline using continuity)
#' @param wi  the weights
#' @return the predicted baseline model
nls_model <- function(Daily, RH.sign, Dec.Last.nls.model, Inc.Last.nls.model, Starting.values, name.T, name.RH, wi = NULL) {
    f_P_T <- function(RH, yf_RH, y0_RH,  r_RH,RH_0, Tmp, yf_T, y0_T, r_T, Tmp_0){
        return(yf_RH  + (y0_RH - yf_RH) * (r_RH)^(RH - min(RH, na.rm = T)) * r_RH^(min(RH, na.rm = T)-RH_0) + yf_T + (y0_T - yf_T) * (r_T)^(Tmp - min(Tmp, na.rm = T)) * r_T^(min(Tmp, na.rm = T)-Tmp_0))}
    Formula <- as.formula(paste0("y_MA ~ f_P_T(RH = ", name.RH, ", Tmp = ", name.T, ", yf_RH = yf_RH, y0_RH = y0_RH, 
                                 r_RH = r_RH, RH_0 = 40, yf_T = yf_T, y0_T = y0_T, r_T = r_T, Tmp_0 = 9.15)"))
    if (RH.sign == 'decr') {
        Lower.values  <- c(0.001, 0.0011, 0.9,-Inf, -Inf, 0.9) # if nls returns yf_RH = y0_RH_, then glance() crashes
        Upper.values  <- c(+Inf, +Inf, 5,  +Inf, +Inf, 2)
    } else {
        Lower.values  <- c(-Inf, -Inf, 0.8, -Inf, -Inf, 0.9)
        Upper.values  <- c(+Inf, +Inf, 1.001, +Inf, +Inf, 2)
    }
    # calculating RSS and then ftol
    ## calculating RSS
    RSS.0 <- sum((f_P_T(RH=Daily[[name.RH]], yf_RH=Starting.values$yf_RH, y0_RH = Starting.values$y0_RH, r_RH = Starting.values$r_RH, RH_0 = 40, 
                        Tmp = Daily[[name.T]], yf_T = Starting.values$yf_T, y0_T = Starting.values$y0_T, r_T = Starting.values$r_T, Tmp_0 = 9.15) - 
                      Daily$y_MA)^2)
    ftol <- max(c(100/RSS.0, 0.02))
    # }
    # f_P_T <- function(RH, yf_RH, y0_RH,  r_RH,RH_0, Tmp, yf_T, y0_T, r_T, Tmp_0) return(yf_RH  + (y0_RH - yf_RH) * (r_RH)^(RH - RH_0) +
    #                                                                                                yf_T + (y0_T - yf_T) * (r_T)^(Tmp - Tmp_0))
    Model.nls <- tryCatch(
        nlsLM(Formula, data = Daily, start = Starting.values, model = TRUE, weights = wi,
              control = nls.lm.control(maxiter = 1024, maxfev = 10000, ftol = ftol, factor = 100), #ptol=1e-5, 
              lower = Lower.values, upper = Upper.values, trace = F),
        error = function(e)            
            nls.multstart::nls_multstart(y_MA ~ f_P_T.alt(RH = name.RH, Tmp = name.T, yf_RH = yf_RH, y0_RH = y0_RH, 
                                                          r_RH = r_RH, RH_0 = 40, yf_T = yf_T, y0_T = y0_T, r_T = r_T, Tmp_0 = 9.15),
                                         
                                         # nls.multstart::nls_multstart(y_MA ~ f_P_T.alt(RH = Out.Relative_humidity_MA, Tmp = Temperature_modelled_MA, yf_RH = yf_RH, y0_RH = y0_RH, 
                                         #                  r_RH = r_RH, RH_0 = 40, yf_T = yf_T, y0_T = y0_T, r_T = r_T, Tmp_0 = 9.15),
                                         data = Daily,
                                         iter = 50,
                                         start_lower = c(yf_RH = Starting.values$yf_RH*0.5, y0_RH=Starting.values$y0_RH*0.5, r_RH = 0.8, yf_T = Starting.values$yf_T*0.5, y0_T = Starting.values$y0_T*0.5, r_T = 0.8),
                                         start_upper = c(yf_RH = Starting.values$yf_RH*2, y0_RH=Starting.values$y0_RH*2, r_RH = 2, yf_T =  Starting.values$yf_T*2, y0_T = Starting.values$y0_T*2, r_T = 2),
                                         control = nls.lm.control(maxiter = 1024, maxfev = 10000, ftol = 1e-2, factor = 100),
                                         convergence_count = 100,
                                         lhstype = 'improved',
                                         lower = c(yf_RH = -100, y0_RH=0, r_RH = 0.8, yf_T = -100, y0_T = 0, r_T = 0.8),
                                         upper = c(yf_RH = 100, y0_RH=100, r_RH = 1.5, yf_T = 100, y0_T = 100, r_T = 2),
                                         supp_errors = 'T', trace=F))
    return(Model.nls = Model.nls)
}

Starting_values <- function(RH.sign, Daily, i.which, r_RH, r_T, name.T, name.RH, ... ){ 
    if (RH.sign == 'decr'){ # RH decreasing
        max.cur.RH <- max(Daily$cur.RH[i.which], na.rm = T)
        data.table::set(Daily, i = i.which, j ='cur.RH', value = max.cur.RH - Daily$cur.RH[i.which])
    }
    # Determining of the starting values of cur.T and cur.RH keeping the curT/curRH ratio constant. 
    BCratio <- diff(range(Daily$RH_eff[i.which]))/diff(range(Daily$T_eff[i.which]))*diff(range(Daily$cur.T[i.which]))/diff(range(Daily$cur.RH[i.which]))
    cur.total <- BCratio*Daily$cur.RH[i.which] + Daily$cur.T[i.which]
    lm.init   <- lm(Daily$y_MA[i.which] ~ cur.total)
    ## assumption 1: at the beginning(where y_MA is minimum), T effect is zero and RH effect equals to min y_MA
    yf_T   <- - coef(lm.init)[[2]] * r_T^(Daily[max(which(Daily$y_MA == min(Daily$y_MA, na.rm=T)))][[name.T]]-9.15)
    y0_T   <- coef(lm.init)[[2]] + yf_T
    ## assumption 1: at the beginning(where y_MA is minimum), RH effect equals to min y_MA
    if (RH.sign == 'decr'){ # RH decreasing
        yf_RH  <- mean(c(min(Daily$y_MA, na.rm=T) + coef(lm.init)[[2]]*BCratio*r_RH^(max(Daily[[name.RH]])-40),
                         coef(lm.init)[[1]] + BCratio*coef(lm.init)[[2]] - yf_T))
        y0_RH  <-  yf_RH - BCratio*coef(lm.init)[[2]]
    } else { # RH increasing
        yf_RH  <- mean(c(coef(lm.init)[[1]] - yf_T, (min(Daily$y_MA, na.rm=T) - coef(lm.init)[[2]]*BCratio*r_RH^(max(Daily[[name.RH]])-40))))
        y0_RH  <- BCratio*coef(lm.init)[[2]] + yf_RH}
    return(Starting.values = list(yf_RH = yf_RH, y0_RH = y0_RH, r_RH = r_RH, yf_T = yf_T,  y0_T = y0_T, r_T = r_T))
}


#' This function fits non-linear model using Temperature and RH vs nA of sensor. adapted from minpack::nlsLM
#' @param formula  the equation of non-linear model
#' @param data  the data containing all parameters described in formula
#' @param start is the starting values calculated by the Starting_values function
#' @param nls.lm.control is the control parameters of nls, ftol and ptol are specifically important and shall be calculated based on the data----> make a new function??? or add to Starting_values
nlsLM <- function (formula, data = parent.frame(), start, jac = NULL, 
                   algorithm = "LM", control = nls.lm.control(), lower = NULL, 
                   upper = NULL, trace = FALSE, subset, weights, na.action, 
                   model = FALSE, ...) 
{
    formula <- as.formula(formula)
    if (!is.list(data) && !is.environment(data)) 
        stop("'data' must be a list or an environment")
    mf <- match.call()
    varNames <- all.vars(formula)
    if (length(formula) == 2L) {
        formula[[3L]] <- formula[[2L]]
        formula[[2L]] <- 0
    }
    form2 <- formula
    form2[[2L]] <- 0
    varNamesRHS <- all.vars(form2)
    mWeights <- missing(weights)
    if (trace) 
        control$nprint <- 1
    pnames <- if (missing(start)) {
        if (!is.null(attr(data, "parameters"))) {
            names(attr(data, "parameters"))
        }
        else {
            cll <- formula[[length(formula)]]
            func <- get(as.character(cll[[1L]]))
            if (!is.null(pn <- attr(func, "pnames"))) 
                as.character(as.list(match.call(func, call = cll))[-1L][pn])
        }
    }
    else names(start)
    env <- environment(formula)
    if (is.null(env)) 
        env <- parent.frame()
    if (length(pnames)) 
        varNames <- varNames[is.na(match(varNames, pnames))]
    lenVar <- function(var) tryCatch(length(eval(as.name(var), 
                                                 data, env)), error = function(e) -1)
    if (length(varNames)) {
        n <- sapply(varNames, lenVar)
        if (any(not.there <- n == -1)) {
            nnn <- names(n[not.there])
            if (missing(start)) {
                warning("No starting values specified for some parameters.\n", 
                        "Initializing ", paste(sQuote(nnn), collapse = ", "), 
                        " to '1.'.\n", "Consider specifying 'start' or using a selfStart model")
                start <- as.list(rep(1, length(nnn)))
                names(start) <- nnn
                varNames <- varNames[i <- is.na(match(varNames, 
                                                      nnn))]
                n <- n[i]
            }
            else stop("parameters without starting value in 'data': ", 
                      paste(nnn, collapse = ", "))
        }
    }
    else {
        if (length(pnames) && any((np <- sapply(pnames, lenVar)) == 
                                  -1)) {
            message("fitting parameters ", paste(sQuote(pnames[np == 
                                                                   -1]), collapse = ", "), " without any variables")
            n <- integer()
        }
        else stop("no parameters to fit")
    }
    respLength <- length(eval(formula[[2L]], data, env))
    if (length(n) > 0L) {
        varIndex <- n%%respLength == 0
        if (is.list(data) && diff(range(n[names(n) %in% names(data)])) > 
            0) {
            mf <- data
            if (!missing(subset)) 
                warning("argument 'subset' will be ignored")
            if (!missing(na.action)) 
                warning("argument 'na.action' will be ignored")
            if (missing(start)) 
                start <- getInitial(formula, mf)
            startEnv <- new.env(hash = FALSE, parent = environment(formula))
            for (i in names(start)) assign(i, start[[i]], envir = startEnv)
            rhs <- eval(formula[[3L]], data, startEnv)
            n <- NROW(rhs)
            wts <- if (mWeights) 
                rep(1, n)
            else eval(substitute(weights), data, environment(formula))
        }
        else {
            mf$formula <- as.formula(paste("~", paste(varNames[varIndex], 
                                                      collapse = "+")), env = environment(formula))
            mf$start <- mf$control <- mf$algorithm <- mf$trace <- mf$model <- NULL
            mf$lower <- mf$upper <- NULL
            mf[[1L]] <- as.name("model.frame")
            mf <- eval.parent(mf)
            n <- nrow(mf)
            mf <- as.list(mf)
            wts <- if (!mWeights) 
                model.weights(mf)
            else rep(1, n)
        }
        if (any(wts < 0 | is.na(wts))) 
            stop("missing or negative weights not allowed")
    }
    else {
        varIndex <- logical()
        mf <- list(0)
        wts <- numeric()
    }
    if (missing(start)) 
        start <- getInitial(formula, mf)
    for (var in varNames[!varIndex]) mf[[var]] <- eval(as.name(var), 
                                                       data, env)
    varNamesRHS <- varNamesRHS[varNamesRHS %in% varNames[varIndex]]
    mf <- c(mf, start)
    lhs <- eval(formula[[2L]], envir = mf)
    m <- match(names(start), names(mf))
    .swts <- if (!missing(wts) && length(wts)) 
        sqrt(wts)
    FCT <- function(par) {
        mf[m] <- par
        rhs <- eval(formula[[3L]], envir = mf, environment(formula))
        res <- lhs - rhs
        res <- .swts * res
        res
    }
    # browser()
    NLS <- nls.lm(par = start, fn = FCT, jac = jac, control = control, 
                  lower = lower, upper = upper, ...)
    start <- NLS$par
    m <- nlsModel(formula, mf, start, wts)
    if (NLS$info %in% c(1, 2, 3, 4)) 
        isConv <- TRUE
    else isConv <- FALSE
    finIter <- NLS$niter
    finTol <- nls.lm.control()$ftol
    convInfo <- list(isConv = isConv, finIter = finIter, finTol = finTol, 
                     stopCode = NLS$info, stopMessage = NLS$message)
    nls.out <- list(m = m, convInfo = convInfo, data = substitute(data), 
                    call = match.call())
    nls.out$call$algorithm <- algorithm
    nls.out$call$control <- nls.control()
    nls.out$call$trace <- FALSE
    nls.out$call$lower <- lower
    nls.out$call$upper <- upper
    nls.out$na.action <- attr(mf, "na.action")
    nls.out$dataClasses <- attr(attr(mf, "terms"), "dataClasses")[varNamesRHS]
    if (model) 
        nls.out$model <- mf
    if (!mWeights) 
        nls.out$weights <- wts
    nls.out$control <- control
    class(nls.out) <- "nls"
    nls.out
}

#' This function is a sub-function of nlsLM. Adapted from mipack, however checking single gradient matrix was cancelled (minpack nlsModel stops in case of single matrix)
#' @param form  the equation of non-linear model. Passed by nlsLM
#' @param data  the data containing all parameters described in formula
#' @param start is the starting values calculated by nls.lm in nlsLM
nlsModel <- function (form, data, start, wts, upper = NULL) {
    thisEnv <- environment()
    env <- new.env(hash = TRUE, parent = environment(form))
    for (i in names(data)) assign(i, data[[i]], envir = env)
    ind <- as.list(start)
    parLength <- 0
    for (i in names(ind)) {
        temp <- start[[i]]
        storage.mode(temp) <- "double"
        assign(i, temp, envir = env)
        ind[[i]] <- parLength + seq_along(start[[i]])
        parLength <- parLength + length(start[[i]])
    }
    getPars.noVarying <- function() unlist(setNames(lapply(names(ind), 
                                                           get, envir = env), names(ind)))
    getPars <- getPars.noVarying
    internalPars <- getPars()
    if (!is.null(upper)) 
        upper <- rep_len(upper, parLength)
    useParams <- rep(TRUE, parLength)
    lhs <- eval(form[[2L]], envir = env)
    rhs <- eval(form[[3L]], envir = env)
    .swts <- if (!missing(wts) && length(wts)) 
        sqrt(wts)
    else rep_len(1, length(rhs))
    assign(".swts", .swts, envir = env)
    resid <- .swts * (lhs - rhs)
    dev <- sum(resid^2)
    if (is.null(attr(rhs, "gradient"))) {
        getRHS.noVarying <- function() {
            if (is.null(upper)) 
                numericDeriv(form[[3L]], names(ind), env)
            else numericDeriv(form[[3L]], names(ind), env, ifelse(internalPars < 
                                                                      upper, 1, -1))
        }
        getRHS <- getRHS.noVarying
        rhs <- getRHS()
    }
    else {
        getRHS.noVarying <- function() eval(form[[3L]], envir = env)
        getRHS <- getRHS.noVarying
    }
    dimGrad <- dim(attr(rhs, "gradient"))
    marg <- length(dimGrad)
    if (marg > 0L) {
        gradSetArgs <- vector("list", marg + 1L)
        for (i in 2L:marg) gradSetArgs[[i]] <- rep(TRUE, dimGrad[i - 
                                                                     1])
        useParams <- rep(TRUE, dimGrad[marg])
    }
    else {
        gradSetArgs <- vector("list", 2L)
        useParams <- rep(TRUE, length(attr(rhs, "gradient")))
    }
    npar <- length(useParams)
    gradSetArgs[[1L]] <- (~attr(ans, "gradient"))[[2L]]
    gradCall <- switch(length(gradSetArgs) - 1L, call("[", gradSetArgs[[1L]], 
                                                      gradSetArgs[[2L]], drop = FALSE), call("[", gradSetArgs[[1L]], 
                                                                                             gradSetArgs[[2L]], gradSetArgs[[2L]], drop = FALSE), 
                       call("[", gradSetArgs[[1L]], gradSetArgs[[2L]], gradSetArgs[[2L]], 
                            gradSetArgs[[3L]], drop = FALSE), call("[", gradSetArgs[[1L]], 
                                                                   gradSetArgs[[2L]], gradSetArgs[[2L]], gradSetArgs[[3L]], 
                                                                   gradSetArgs[[4L]], drop = FALSE))
    getRHS.varying <- function() {
        ans <- getRHS.noVarying()
        attr(ans, "gradient") <- eval(gradCall)
        ans
    }
    # browser()
    QR <- qr(.swts * attr(rhs, "gradient"))
    qrDim <- min(dim(QR$qr))
    # if (QR$rank < qrDim) 
    #   stop("singular gradient matrix at initial parameter estimates")
    getPars.varying <- function() unlist(setNames(lapply(names(ind), 
                                                         get, envir = env), names(ind)))[useParams]
    setPars.noVarying <- function(newPars) {
        assign("internalPars", newPars, envir = thisEnv)
        for (i in names(ind)) assign(i, unname(newPars[ind[[i]]]), 
                                     envir = env)
    }
    setPars.varying <- function(newPars) {
        internalPars[useParams] <- newPars
        for (i in names(ind)) assign(i, unname(internalPars[ind[[i]]]), 
                                     envir = env)
    }
    setPars <- setPars.noVarying
    on.exit(remove(i, data, parLength, start, temp, m))
    m <- list(resid = function() resid, fitted = function() rhs, 
              formula = function() form, deviance = function() dev, 
              lhs = function() lhs, gradient = function() .swts * 
                  attr(rhs, "gradient"), conv = function() {
                      if (npar == 0) return(0)
                      rr <- qr.qty(QR, resid)
                      sqrt(sum(rr[1L:npar]^2)/sum(rr[-(1L:npar)]^2))
                  }, incr = function() qr.coef(QR, resid), setVarying = function(vary = rep(TRUE, 
                                                                                            length(useParams))) {
                      assign("useParams", if (is.character(vary)) {
                          temp <- logical(length(useParams))
                          temp[unlist(ind[vary])] <- TRUE
                          temp
                      } else if (is.logical(vary) && length(vary) != length(useParams)) stop("setVarying : 'vary' length must match length of parameters") else {
                          vary
                      }, envir = thisEnv)
                      gradCall[[length(gradCall) - 1L]] <<- useParams
                      if (all(useParams)) {
                          assign("setPars", setPars.noVarying, envir = thisEnv)
                          assign("getPars", getPars.noVarying, envir = thisEnv)
                          assign("getRHS", getRHS.noVarying, envir = thisEnv)
                          assign("npar", length(useParams), envir = thisEnv)
                      } else {
                          assign("setPars", setPars.varying, envir = thisEnv)
                          assign("getPars", getPars.varying, envir = thisEnv)
                          assign("getRHS", getRHS.varying, envir = thisEnv)
                          assign("npar", length(seq_along(useParams)[useParams]), 
                                 envir = thisEnv)
                      }
                  }, setPars = function(newPars) {
                      setPars(newPars)
                      assign("resid", .swts * (lhs - assign("rhs", getRHS(), 
                                                            envir = thisEnv)), envir = thisEnv)
                      assign("dev", sum(resid^2), envir = thisEnv)
                      assign("QR", qr(.swts * attr(rhs, "gradient")), 
                             envir = thisEnv)
                      return(QR$rank < min(dim(QR$qr)))
                  }, getPars = function() getPars(), getAllPars = function() getPars(), 
              getEnv = function() env, trace = function() {
                  cat(format(dev), ": ", format(getPars()))
                  cat("\n")
              }, Rmat = function() qr.R(QR), predict = function(newdata = list(), 
                                                                qr = FALSE) eval(form[[3L]], as.list(newdata), env))
    class(m) <- "nlsModel"
    m
}

#' This function calculates the outliers of a region or any data set by setting LM, determining the peaks of residuals and evaluate them
#' @param i.which  the start:end of data region
#' @param Daily.Matrice the half-day data
#' @param var2LM the variable which LM will be set against y_MA
#' @param thrs is the threshold to assess rmse of LM model
#' @param returns Daily.Matrice with updated Outliers, decision of fitting nls model later, and indexes of where to fit nls 
Find.outlier <- function(i.which, Daily.Matrice, var2LM, ...) {
    ## assigning all data outlier to be FALSE. if we find any data to be outlier TRUE, we will assign it TRUE in the 'repeat'
    data.table::set(Daily.Matrice, i = i.which, j = "Outliers", value = FALSE)
    repeat {
        if (!exists('i.false.init')) i.false.init <- i.which
        if (length(i.false.init) > 15 && length(i.false.init)/length(i.which) > 0.20) { # at least 15 data and 20% of the region
            LMmodel <- lm(y_MA ~ get(var2LM) + I(get(var2LM)^2), data = Daily.Matrice[i.false.init])
            if (is.na(coef(LMmodel)[3]))  LMmodel <- lm(y_MA ~ get(var2LM), data = Daily.Matrice[i.false.init])
            R2.LM   <- summary(LMmodel)$r.squared
            pred.LM <- predict(LMmodel, newdata = Daily.Matrice[i.which])
            resid.LM <- data.table::data.table(resid = Daily.Matrice$y_MA[i.which] -pred.LM)
            # if (exists('which.out')) {
            sd.LM <- sd(resid.LM[which(i.which %in% i.false.init)]$resid)
            # } else {
            #     sd.LM <- sd(resid.LM$resid)
            # }
            
            if (diff(range(Daily.Matrice[i.false.init][[var2LM]])) > 100) {
                c.crtc <- 0.06
                resid.LM.mad <- mad(resid.LM[which(i.which %in% i.false.init)]$resid, constant = 2)
            } else {
                if (diff(range(Daily.Matrice[i.false.init][[var2LM]])) < 20) {
                    c.crtc <- 0.10
                } else if (diff(range(Daily.Matrice[i.false.init][[var2LM]])) < 50) {
                    c.crtc <- 0.04
                } else if (diff(range(Daily.Matrice[i.false.init][[var2LM]])) < 100) {
                    c.crtc <- 0.025
                } 
                resid.LM.mad <- mad(resid.LM[which(i.which %in% i.false.init)]$resid)  
            }
            
            thr.crtc <- R2.LM * c.crtc * diff(range(Daily.Matrice[i.false.init][[var2LM]]))
            crtc.val <- R2.LM * (max(c(sd.LM, resid.LM.mad, max(Daily.Matrice[i.false.init][[var2LM]], na.rm = T)*0.03))) 
            # crtc.val <- R2.LM * (max(min(sd.LM, resid.LM.mad), max(Daily.Matrice[i.false.init][[var2LM]], na.rm = T)*0.03))
            if (count(Daily.Matrice$y_MA[i.false.init] < 0)/length(i.false.init) > 0.5) { # in case of negative yMA being more than 50% of data used to establish lm, the values will be enlarged
                thr.crtc <- 1.5 * thr.crtc
                crtc.val <- 1.5 * crtc.val 
            }
            if (resid.LM.mad/sd.LM > 5) { # in such a case, the residuals are likely non-normally distributed. Thus we need less stringent critical value to discard more peaks
                # browser()
                crtc.val <- min(2*sd.LM, 0.4 * crtc.val)
            }
            if (as.Date('2020-09-22') %in% unique(as.Date(Daily.Matrice$date[i.which])) ) browser()
            # if (as.Date('2020-09-30') %in% unique(as.Date(Daily.Matrice$date[i.which])) ) browser() #||
            #     as.Date('2020-12-10') %in% unique(as.Date(Daily.Matrice$date[i.which])) || as.Date('2020-12-11') %in% unique(as.Date(Daily.Matrice$date[i.which])) ||
            #     as.Date('2020-12-19') %in% unique(as.Date(Daily.Matrice$date[i.which])) || as.Date('2020-12-21') %in% unique(as.Date(Daily.Matrice$date[i.which]))) browser()
            if ((sd.LM/mean(c(Daily.Matrice$y_MA[i.false.init] , predict(LMmodel))) < 0.05 && sd.LM < thr.crtc) || 
                (R2.LM > 0.99 && sd.LM/mean(c(Daily.Matrice$y_MA[i.false.init] , predict(LMmodel))) < 0.025 && resid.LM.mad/mean(c(Daily.Matrice$y_MA[i.false.init] , predict(LMmodel))) < 0.025) ||
                # (R2.LM > 0.99 && sd.LM/mean(c(Daily.Matrice$y_MA[i.false.init] , predict(LMmodel))) < 0.025) ||
                # (R2.LM %between% c(0.98, 0.99) &&
                #  sd.LM/mean(c(Daily.Matrice$y_MA[i.false.init] , predict(LMmodel))) < 0.05)
                (R2.LM %between% c(0.98, 0.99) &&
                 sd.LM/mean(c(Daily.Matrice$y_MA[i.false.init] , predict(LMmodel))) < 0.05 && resid.LM.mad/mean(c(Daily.Matrice$y_MA[i.false.init] , predict(LMmodel))) < 0.05)) {
                if (!exists('Peaks')) {
                    Peaks.tmp <- Find.Peaks(resid.LM, var = "resid")
                    # adding pseudo baseline at the peak values
                    Peaks.tmp[, pseu.bline := sapply(seq_along(Peaks.tmp$Peak.values), function(i) {
                        return(Daily.Matrice$pseu.bline[i.which[Peaks.tmp$Ind.Peak[i]]])
                    })]
                    # checking if the peaks are too high from the expected baseline (pseu baseline). This is to avoid assigning peaks that occur during long-pollution episodes
                    Peaks.tmp[, poten.NO := sapply(seq_along(Peaks.tmp$Peak.values), function(i) {
                        if (Peaks.tmp$Peak.values[i] < 0) {
                            return(FALSE)
                        } else {
                            if (Peaks.tmp$pseu.bline[i] < 0 || R2.LM > 0.98) {  
                                val <- pred.LM[Peaks.tmp$Ind.Peak[i]]
                            } else val <- Peaks.tmp$pseu.bline[i]
                            return(Daily.Matrice$y_MA[i.which[Peaks.tmp$Ind.Peak[i]]] - val > 20)
                        }
                    })]
                    # in addition to the above criteria, we introduce more stringent criterion to avoid assigning small peaks baseline
                    if (all(abs(na.omit(Peaks.tmp$Peak.values)) <  crtc.val) && # < 1.25, 1.10 *
                        all(!Peaks.tmp[!is.na(Peaks.tmp$Peak.values)]$poten.NO)) break
                    # in addition to the above criteria, we introduce more stringent criterion to avoid assigning small peaks baseline
                } else if (all(abs(na.omit(Peaks$Peak.values)) < crtc.val) && # < 1.25, 1.10 * 
                           all(!Peaks[!is.na(Peaks$Peak.values)]$poten.NO)) {
                    break
                } 
            }
            # data.table::set(resid.LM, j ='rel.res', value = resid.LM$resid/pred.LM*100)
            if (exists('Peaks.tmp')) {
                Peaks <- Peaks.tmp
                rm(Peaks.tmp)
            } else {
                Peaks <- Find.Peaks(resid.LM, var = "resid")
                # adding pseudo baseline at the peak values
                Peaks[, pseu.bline := sapply(seq_along(Peaks$Peak.values), function(i) {
                    return(Daily.Matrice$pseu.bline[i.which[Peaks$Ind.Peak[i]]])
                })]
                # checking if the peaks are too high from the expected baseline (pseu baseline). This is to avoid assigning peaks that occur during long-pollution episodes
                Peaks[, poten.NO := sapply(seq_along(Peaks$Peak.values), function(i) {
                    if (Peaks$Peak.values[i] < 0) {
                        return(FALSE)
                    } else if (Daily.Matrice$Outliers[i.which[Peaks$Ind.Peak[i]]]) {
                        i.pk <- Peaks$Ind.Start[i]:Peaks$Ind.End[i]
                        if (all(Daily.Matrice$Outliers[i.which[i.pk]])) {
                            return(FALSE)
                        } else if (any(Daily.Matrice$y_MA[i.which[i.pk]] - Daily.Matrice$pseu.bline[i.which[i.pk]] > 20)) {
                            return(TRUE)
                        } else return(FALSE)
                    } else {
                        if (Peaks$pseu.bline[i] < 0 || R2.LM > 0.98) {  
                            val <- pred.LM[Peaks$Ind.Peak[i]]
                        } else val <- Peaks$pseu.bline[i]
                        return(Daily.Matrice$y_MA[i.which[Peaks$Ind.Peak[i]]] - val > 20)
                    }
                })]
            }
            
            if (exists('which.out')) {
                discd <- sort(as.integer(unique(unlist(sapply(seq_along(which.out$str), function(i) {c(which.out$str[i]:which.out$end[i])})))))
                Peaks[, Peak.values := sapply(seq_along(Peaks$Peak.values), function(i) {
                    len.test <- setdiff(Peaks$Ind.Start[i]:Peaks$Ind.End[i], discd)
                    if (length(len.test) == 0) {
                        return(NA_real_)
                    } else if (length(len.test) == length(Peaks$Ind.Start[i]:Peaks$Ind.End[i])) {
                        return(Peaks$Peak.values[i])
                    } else if (all(Daily.Matrice$Outliers[i.which[len.test]])) {
                        return(NA_real_)
                    } else {
                        return(max(resid.LM$resid[len.test], na.rm = T))}
                    # return(max(resid.LM$resid[Peaks$Ind.Start[i]:Peaks$Ind.End[i]], na.rm = T))}
                })]
                # re-checking f the peaks are too high from the expected baseline (pseu baseline). This is to avoid assigning peaks that occur during long-pollution episodes
                Peaks[, poten.NO := sapply(seq_along(Peaks$Peak.values), function(i) {
                    if (is.na(Peaks$Peak.values[i])) {
                        return(FALSE)
                    } else if (Peaks$Peak.values[i] < 0) {
                        return(FALSE)
                    } else if (Daily.Matrice$Outliers[i.which[Peaks$Ind.Peak[i]]]) {
                        i.pk <- Peaks$Ind.Start[i]:Peaks$Ind.End[i]
                        if (all(Daily.Matrice$Outliers[i.which[i.pk]])) {
                            return(FALSE)
                        } else if (any(Daily.Matrice$y_MA[i.which[i.pk]] - Daily.Matrice$pseu.bline[i.which[i.pk]] > 20)) {
                            return(TRUE)
                        } else return(FALSE)
                    } else {
                        if (Peaks$pseu.bline[i] < 0 || R2.LM > 0.98) { # if the pseu baseline predicted 
                            val <- pred.LM[Peaks$Ind.Peak[i]]
                        } else val <- Peaks$pseu.bline[i]
                        return(Daily.Matrice$y_MA[i.which[Peaks$Ind.Peak[i]]] - val > 20)
                    }
                })]
            }
            if (exists('fast.SumEff')) {
                i.fast.SumEff <- as.integer(unique(unlist(sapply(seq_along(fast.SumEff$str), function(i) {c(fast.SumEff$str[i]:fast.SumEff$end[i])}))))
                Peaks[, Peak.values := sapply(seq_along(Peaks$Peak.values), function(i) {
                    len.test <- setdiff(Peaks$Ind.Start[i]:Peaks$Ind.End[i], i.fast.SumEff)
                    if (length(len.test) == 0) {
                        return(NA_real_)
                    } else if (all(Daily.Matrice$Outliers[i.which[len.test]])) {
                        return(NA_real_)
                    } else return(Peaks$Peak.values[i])
                })]
            }
            if (all(abs(na.omit(Peaks$Peak.values)) < crtc.val)) { 
                if (all(!Peaks[!is.na(Peaks$Peak.values)]$poten.NO)) {
                    break 
                } else {
                    # browser()
                    for (i in which(Peaks$poten.NO)) {
                        i.peak.high <- Peaks$Ind.Start[i]:Peaks$Ind.End[i]
                        # assigning only yMA - pseudo bline > 20 as Outlier TRUE
                        i.peak.high <- i.peak.high[which(Daily.Matrice$y_MA[i.which[i.peak.high]] - Daily.Matrice$pseu.bline[i.which[i.peak.high]] > 20)]
                        data.table::set(Daily.Matrice, i = i.which[i.peak.high], j = 'Outliers', value = TRUE)
                    }
                    break
                }
            } else {
                i.rem <- max(Peaks[abs(Peak.values) == max(abs(Peaks$Peak.values), na.rm = T), which = T])
                # checking if this peak has a fast changing Sum_effect. If so, we will also exclude the neighboring peak to this peak
                if (count(abs(Daily.Matrice[i.which[Peaks$Ind.Start[i.rem]:Peaks$Ind.End[i.rem]]][['d.Sum_eff']]) > 0.5)/length(Peaks$Ind.Start[i.rem]:Peaks$Ind.End[i.rem]) > 0.50) { 
                    if (exists('discd')) {
                        i.upd <- i.which[-c(Peaks$Ind.Start[i.rem]:Peaks$Ind.End[i.rem], discd)]
                    } else i.upd <- i.which[-c(Peaks$Ind.Start[i.rem]:Peaks$Ind.End[i.rem])]
                    if (exists("fast.SumEff")) {
                        fast.SumEff <-  data.table::rbindlist(list(fast.SumEff, data.table::data.table(str = Peaks$Ind.Start[i.rem], end = Peaks$Ind.End[i.rem])), use.names = T) 
                    } else {
                        fast.SumEff <- data.table::data.table(str = Peaks$Ind.Start[i.rem], end = Peaks$Ind.End[i.rem])
                    }
                    if (i.rem != nrow(Peaks)) {
                        fast.SumEff <- data.table::rbindlist(list(fast.SumEff, data.table::data.table(str = Peaks$Ind.Start[i.rem+1], end = Peaks$Ind.End[i.rem+1])), use.names = T)
                        Peaks <- Peaks[-c(i.rem, i.rem+1)]
                    } else if (i.rem > 1) {
                        fast.SumEff <- data.table::rbindlist(list(fast.SumEff, data.table::data.table(str = Peaks$Ind.Start[i.rem-1], end = Peaks$Ind.End[i.rem-1])), use.names =T)
                        Peaks <- Peaks[-c(i.rem, i.rem-1)]
                    } else Peaks <- Peaks[-c(i.rem)]
                    ## Checking if the i.rem is at the end of the region and the end of the peak is negative. If so, we will not include it in the discarded peaks,
                    ## since the top of the peak can be over the critical value, in particular for long peaks
                } else if (i.rem == nrow(Peaks) || i.rem == 1) { # checking if the i.rem is at the beginning or end of the region
                    # We will assign the residual exceeding the critical value as Outlier TRUE
                    i.peak.upd <- Peaks$Ind.Start[i.rem]:Peaks$Ind.End[i.rem]
                    if (any(i.peak.upd[which(abs(resid.LM$resid[i.peak.upd]) > crtc.val)])) { # checking which part of the peak exceeding the critical value
                        if (any(i.peak.upd[which(resid.LM$resid[i.peak.upd] > crtc.val)])) {
                            i.rem.disc <- sort(c(i.peak.upd[which(resid.LM$resid[i.peak.upd] > crtc.val)]))
                        } else i.rem.disc <- sort(c(i.peak.upd[which(abs(resid.LM$resid[i.peak.upd]) > crtc.val)]))
                    } else i.rem.disc <- sort(c(i.peak.upd[which(Daily.Matrice$y_MA[i.which[i.peak.upd]] - Peaks$pseu.bline[i.rem] > 20)])) # means that the y_MA of this peak are at least 20 nA higher that the expected baseline
                    # Making the part of peak a data table to add to the outlier table in order later to exclude from the data set of residual (and peak)
                    # if (as.Date('2020-11-04') %in% unique(as.Date(Daily.Matrice$date[i.which]))) browser()
                    i.disc.gap <- i.rem.disc[which(diff(i.rem.disc) > 1)]
                    # checking if the discarded limits start with the 1st value of peak and after there is a gap. thus, 1st value repeated two times
                    if (length(i.disc.gap) > 0 && i.disc.gap[1] == i.rem.disc[1]) i.disc.gap <- c(i.disc.gap, i.rem.disc[which(i.disc.gap[1] == i.rem.disc[1])+1])
                    if (length(i.disc.gap) > 0 && i.disc.gap[length(i.disc.gap)] == i.rem.disc[length(i.rem.disc) - 1]) i.disc.gap <- c(i.disc.gap, i.rem.disc[length(i.rem.disc)])
                    disc.limits <- c(i.rem.disc[1], i.disc.gap)
                    disc.jumps <- data.table::data.table(Reg.Start = sapply(disc.limits, function(k) {
                        if (k == disc.limits[1] || k == i.rem.disc[length(i.rem.disc)]) {
                            return(k)
                        } else { 
                            if (length(disc.limits) > 2 && which(k == disc.limits) > 2) {
                                # in case the discarded limits start with the 1st value of peak and after there is a gap.
                                if (disc.limits[which(k == disc.limits)-1] == i.rem.disc[1] && 
                                    disc.limits[which(k == disc.limits)-2] == i.rem.disc[1]) {
                                    return(k)
                                } else return(i.rem.disc[which(k == i.rem.disc)+1])
                            } else return(i.rem.disc[which(k == i.rem.disc)+1])
                        }     
                    }))
                    disc.jumps[, Reg.End := sapply(1:(nrow(disc.jumps)), function(k) {
                        if (is.na(disc.jumps$Reg.Start[k])) {
                            return(NA_real_)
                        } else if (k != nrow(disc.jumps)) {
                            if (disc.jumps$Reg.Start[k] == disc.jumps$Reg.Start[k+1]) {
                                data.table::set(disc.jumps, i = as.integer(k+1), j = "Reg.Start", value = NA_real_)
                                return(disc.jumps$Reg.Start[k]) 
                            } else return(i.rem.disc[which(disc.jumps$Reg.Start[k+1] == i.rem.disc)-1])
                        } else return(i.rem.disc[length(i.rem.disc)])})]
                    data.disc <- data.table::data.table(str = as.integer(na.omit(disc.jumps)$Reg.Start), end = as.integer(na.omit(disc.jumps)$Reg.End))
                    
                    if (exists('which.out')) {
                        # Assigning outlier TRUE the resid > critical of that peak
                        which.out  <- data.table::rbindlist(list(which.out, data.disc), use.names = T)
                        i.upd      <- i.which[-c(i.which[i.rem.disc], i.which[discd])]
                    } else {
                        which.out <- data.disc
                        i.upd     <- i.which[-c(i.which[i.rem.disc])]
                    }
                    rm(data.disc, disc.limits, disc.jumps)
                    # Assigning outlier TRUE the resid < 0 or resid < critical value of that peak
                    data.table::set(Daily.Matrice, i = i.which[i.rem.disc], j = "Outliers", value = TRUE)
                    # re-defining the critical value by excluding the peak(s) found either while fast changing RH, or, first or last peak
                    # # to discard the outlier TRUE of the region (it may happen that the peak start - end are not all outlier TRUE or FALSE)
                    i.peak.discd <- which(!Daily.Matrice$Outliers[i.which])
                    if (length(i.peak.discd) > 1) {
                        
                        if (diff(range(Daily.Matrice[i.upd][[var2LM]])) > 100) {
                            c.crtc <- 0.06
                            resid.LM.mad <- mad(resid.LM[i.peak.discd]$resid, constant = 2)
                        } else {
                            if (diff(range(Daily.Matrice[i.upd][[var2LM]])) < 20) {
                                c.crtc <- 0.10
                            } else if (diff(range(Daily.Matrice[i.upd][[var2LM]])) < 50) {
                                c.crtc <- 0.04
                            } else if (diff(range(Daily.Matrice[i.upd][[var2LM]])) < 100) {
                                c.crtc <- 0.025
                            } 
                            resid.LM.mad <- mad(resid.LM[i.peak.discd]$resid)  
                        }
                        
                        sd.LM <- sd(resid.LM[i.peak.discd]$resid)
                        crtc.val <- min(crtc.val, max(c(sd.LM, resid.LM.mad, max(Daily.Matrice[i.upd][[var2LM]], na.rm = T)*0.03)))
                        # crtc.val <- min(crtc.val, max(min(sd.LM, resid.LM.mad), max(Daily.Matrice[i.upd][[var2LM]], na.rm = T)*0.03))
                        # if (as.Date('2021-01-03') %in% unique(as.Date(Daily.Matrice$date[i.which]))) browser()
                        if (resid.LM.mad/sd.LM > 5) { # in such a case, the residuals are likely non-normally distributed. Thus we need less stringent critical value to discard more peaks
                            crtc.val <- min(crtc.val, min(2*sd.LM, 0.4 * crtc.val))
                        }
                    }
                    # browser()
                    if (length(i.peak.discd) < 1 || (all(abs(na.omit(Peaks$Peak.values)) < crtc.val) &&
                                                     all(!Peaks[!is.na(Peaks$Peak.values)]$poten.NO))) {
                        break
                    } else {
                        # browser()
                        if (identical(i.false.init, Daily.Matrice[intersect(i.which, which(Outliers != TRUE)), which = TRUE])) { # it means the residuals peaks all < critical value but > than pseu.baseline at leasy by 20
                            break
                        } else {
                            i.false.init <- Daily.Matrice[intersect(i.which, which(Outliers != TRUE)), which = TRUE]
                            if (length(i.false.init) == 0) {
                                break}
                        }
                        rm(i.upd)
                    }
                } else {
                    # assigning baseline TRUE only for the values exceeding the critical value. Alternatively we can exclude all peak
                    i.peak <- Peaks$Ind.Start[i.rem]:Peaks$Ind.End[i.rem]
                    # exceeding critical value
                    i.crtc <- i.peak[which(abs(resid.LM$resid[i.peak]) > crtc.val)]
                    if (length(i.peak)/length(i.crtc) > 0.5) {
                        data.table::set(Daily.Matrice, i = i.which[i.peak], j = "Outliers", value = TRUE)
                    } else data.table::set(Daily.Matrice, i = i.which[i.crtc], j = "Outliers", value = TRUE)
                    data.table::set(Peaks, i = i.rem, j = "Peak.values", value = NA_real_)
                    if (exists("which.out")){
                        which.out <- data.table::rbindlist(list(which.out, data.table::data.table(str = i.peak[1], end = i.peak[length(i.peak)])), use.names = T)
                    } else {
                        which.out <- data.table::data.table(str = i.peak[1], end = i.peak[length(i.peak)])}
                    if (identical(i.false.init, Daily.Matrice[intersect(i.which, which(Outliers != TRUE)), which = TRUE])) { # it means the residuals peaks all < critical value but > than pseu.baseline at leasy by 20
                        break
                    } else {
                        i.false.init <- Daily.Matrice[intersect(i.which, which(Outliers != TRUE)), which = TRUE]
                        if (length(i.false.init) == 0) {
                            break}
                    }
                }
            }
        } else {
            data.table::set(Daily.Matrice, i = i.which, j = "Outliers", value = TRUE)
            break
        }
    }
    return(list(Daily.Matrice = Daily.Matrice)) #, i.false.init = i.false.init, fit.nls = fit.nls, var2LM = var2LM, LMmodel = LMmodel
}

#' This function calculates the outliers of a region or any data set by setting LM, determining the peaks of residuals and evaluate them
#' @param i.which  the start:end of data region
#' @param Daily.Matrice the half-day data
#' @param var2LM the variable which LM will be set against y_MA
#' @param thrs is the threshold to assess rmse of LM model
#' @param returns Daily.Matrice with updated Outliers, decision of fitting nls model later, and indexes of where to fit nls 
Find.outlier2 <- function(i.which, Daily.Matrice, var2LM, ...) {
    ## assigning all data outlier to be FALSE. if we find any data to be outlier TRUE, we will assign it TRUE in the 'repeat'
    data.table::set(Daily.Matrice, i = i.which, j = "Outliers", value = FALSE)
    repeat {
        if (!exists('i.false.init')) i.false.init <- i.which
        if (length(i.false.init) > 15 && length(i.false.init)/length(i.which) > 0.20) { # at least 15 data and 20% of the region
            LMmodel <- lm(y_MA ~ get(var2LM) + I(get(var2LM)^2), data = Daily.Matrice[i.false.init])
            if (is.na(coef(LMmodel)[3]))  LMmodel <- lm(y_MA ~ get(var2LM), data = Daily.Matrice[i.false.init])
            R2.LM   <- summary(LMmodel)$r.squared
            pred.LM <- predict(LMmodel, newdata = Daily.Matrice[i.which])
            resid.LM <- data.table::data.table(resid = Daily.Matrice$y_MA[i.which] -pred.LM)
            # if (exists('which.out')) {
            # sd.LM  <- sd(resid.LM[i.false.init]$resid)
            sd.LM  <- sd(resid.LM[which(i.which %in% i.false.init)]$resid)
            rsd.LM <- sd.LM/mean(c(Daily.Matrice$y_MA[i.false.init] , predict(LMmodel)), na.rm = T)
            
            if (diff(range(Daily.Matrice[i.false.init][[var2LM]], na.rm = T)) > 100) {
                c.crtc <- 0.06
                # resid.LM.mad <- mad(resid.LM[i.false.init]$resid, constant = 2)
                resid.LM.mad <- mad(resid.LM[which(i.which %in% i.false.init)]$resid, constant = 2)
            } else {
                if (diff(range(Daily.Matrice[i.false.init][[var2LM]], na.rm = T)) < 20) {
                    c.crtc <- 0.10
                } else if (diff(range(Daily.Matrice[i.false.init][[var2LM]], na.rm = T)) < 50) {
                    c.crtc <- 0.04
                } else if (diff(range(Daily.Matrice[i.false.init][[var2LM]], na.rm = T)) < 100) {
                    c.crtc <- 0.025
                } 
                # resid.LM.mad <- mad(resid.LM[i.false.init]$resid)
                resid.LM.mad <- mad(resid.LM[which(i.which %in% i.false.init)]$resid)
            }
            rmad         <- resid.LM.mad/median(c(Daily.Matrice$y_MA[i.false.init] , predict(LMmodel)))
            thr.crtc <- R2.LM * c.crtc * diff(range(Daily.Matrice[i.false.init][[var2LM]], na.rm = T))
            # we adjust the crtc.value based on R2 and rsd and mad/median
            crtc.val <- R2.LM * (max(c(min(sd.LM, resid.LM.mad), max(Daily.Matrice[i.false.init][[var2LM]], na.rm = T)*0.03))) * (1 - max(c(rsd.LM, rmad)))
            if (count(Daily.Matrice$y_MA[i.false.init] < 0)/length(i.false.init) > 0.5) { # in case of negative yMA being more than 50% of data used to establish lm, the values will be enlarged
                thr.crtc <- 1.5 * thr.crtc
                crtc.val <- 1.5 * crtc.val 
            }
            if (resid.LM.mad/sd.LM > 5) { # in such a case, the residuals are likely non-normally distributed. Thus we need less stringent critical value to discard more peaks
                # browser()
                crtc.val <- min(2*sd.LM, 0.4 * crtc.val)
            }
            
            
            Peaks <- Find.Peaks(resid.LM, var = "resid")
            # adding pseudo baseline at the peak values
            Peaks[, pseu.bline := sapply(seq_along(Peaks$Peak.values), function(i) {
                return(Daily.Matrice$pseu.bline[i.which[Peaks$Ind.Peak[i]]])
            })]
            # checking if the peaks are too high from the expected baseline (pseu baseline). This is to avoid assigning peaks that occur during long-pollution episodes
            Peaks[, poten.NO := sapply(seq_along(Peaks$Peak.values), function(i) {
                if (Peaks$Peak.values[i] < 0) {
                    return(FALSE)
                } else if (Daily.Matrice$Outliers[i.which[Peaks$Ind.Peak[i]]]) { # checking if the peak of the peak is already Outlier TRUE
                    i.pk <- Peaks$Ind.Start[i]:Peaks$Ind.End[i]
                    if (all(Daily.Matrice$Outliers[i.which[i.pk]])) { # Checking if all peak is outlier TRUE
                        return(FALSE)
                    } else if (any(Daily.Matrice$y_MA[i.which[i.pk]] - Daily.Matrice$pseu.bline[i.which[i.pk]] > 20)) {
                        return(TRUE)
                    } else return(FALSE)
                } else {
                    if (Peaks$pseu.bline[i] < 0 || R2.LM > 0.98) {  
                        val <- pred.LM[Peaks$Ind.Peak[i]]
                    } else val <- Peaks$pseu.bline[i]
                    return(Daily.Matrice$y_MA[i.which[Peaks$Ind.Peak[i]]] - val > 20)
                }
            })]
            if (exists('which.out')) {
                discd <- sort(as.integer(unique(unlist(sapply(seq_along(which.out$str), function(i) {c(which.out$str[i]:which.out$end[i])})))))
                Peaks[, Peak.values := sapply(seq_along(Peaks$Peak.values), function(i) {
                    len.test <- setdiff(Peaks$Ind.Start[i]:Peaks$Ind.End[i], discd)
                    if (length(len.test) == 0) {
                        return(NA_real_)
                    } else if (length(len.test) == length(Peaks$Ind.Start[i]:Peaks$Ind.End[i])) {
                        return(Peaks$Peak.values[i])
                    } else if (all(Daily.Matrice$Outliers[i.which[len.test]])) {
                        return(NA_real_)
                    } else {
                        return(max(resid.LM$resid[len.test], na.rm = T))}
                    # return(max(resid.LM$resid[Peaks$Ind.Start[i]:Peaks$Ind.End[i]], na.rm = T))}
                })]
                # re-checking f the peaks are too high from the expected baseline (pseu baseline). This is to avoid assigning peaks that occur during long-pollution episodes
                Peaks[, poten.NO := sapply(seq_along(Peaks$Peak.values), function(i) {
                    if (is.na(Peaks$Peak.values[i])) {
                        return(FALSE)
                    } else if (Peaks$Peak.values[i] < 0) {
                        return(FALSE)
                    } else if (Daily.Matrice$Outliers[i.which[Peaks$Ind.Peak[i]]]) {
                        i.pk <- Peaks$Ind.Start[i]:Peaks$Ind.End[i]
                        if (all(Daily.Matrice$Outliers[i.which[i.pk]])) {
                            return(FALSE)
                        } else if (any(Daily.Matrice$y_MA[i.which[i.pk]] - Daily.Matrice$pseu.bline[i.which[i.pk]] > 20)) {
                            return(TRUE)
                        } else return(FALSE)
                    } else if (Daily.Matrice$y_MA[i.which[Peaks$Ind.Peak[i]]] - Peaks$pseu.bline[i] > 50) { # if the yMA at the peak value if way higher than the expected (pseudo) baseline
                        return(TRUE) 
                    } else {
                        if (Peaks$pseu.bline[i] < 0 || R2.LM > 0.98) { # if the pseu baseline predicted 
                            val <- pred.LM[Peaks$Ind.Peak[i]]
                        } else val <- Peaks$pseu.bline[i]
                        return(Daily.Matrice$y_MA[i.which[Peaks$Ind.Peak[i]]] - val > 20)
                    }
                })]
            }
            if (exists('fast.SumEff')) {
                i.fast.SumEff <- as.integer(unique(unlist(sapply(seq_along(fast.SumEff$str), function(i) {c(fast.SumEff$str[i]:fast.SumEff$end[i])}))))
                Peaks[, Peak.values := sapply(seq_along(Peaks$Peak.values), function(i) {
                    len.test <- setdiff(Peaks$Ind.Start[i]:Peaks$Ind.End[i], i.fast.SumEff)
                    if (length(len.test) == 0) {
                        return(NA_real_)
                    } else if (all(Daily.Matrice$Outliers[i.which[len.test]])) {
                        return(NA_real_)
                    } else return(Peaks$Peak.values[i])
                })]
            }
            if (as.Date('2020-05-29') %in% unique(as.Date(Daily.Matrice$date[i.which])) ) browser() #||
            #     as.Date('2020-12-10') %in% unique(as.Date(Daily.Matrice$date[i.which])) || as.Date('2020-12-11') %in% unique(as.Date(Daily.Matrice$date[i.which])) ||
            #     as.Date('2020-12-19') %in% unique(as.Date(Daily.Matrice$date[i.which])) || as.Date('2020-12-21') %in% unique(as.Date(Daily.Matrice$date[i.which]))) browser()
            
            if (all(abs(na.omit(Peaks$Peak.values)) < crtc.val) ||
                ((R2.LM > 0.995 && sd.LM < 2 && resid.LM.mad < 2) ||
                 (resid.LM.mad < thr.crtc && sd.LM < thr.crtc) || 
                 (rsd.LM < 0.05 && sd.LM < thr.crtc) || 
                 (R2.LM > 0.99 && rsd.LM < 0.025 && rmad < 0.025) ||
                 (R2.LM %between% c(0.98, 0.99) &&
                  rsd.LM < 0.05 && rmad < 0.05))) {
                if (all(!Peaks[!is.na(Peaks$Peak.values)]$poten.NO)) {
                    break 
                } else {
                    for (i in which(Peaks$poten.NO)) {
                        i.peak.high <- Peaks$Ind.Start[i]:Peaks$Ind.End[i]
                        # checking if the peak is already Outlier TRUE
                        if (all(Daily.Matrice$Outliers[i.which[i.peak.high]])) {
                            if (i != which(Peaks$poten.NO)[length(which(Peaks$poten.NO))]) next
                        } else {
                            # assigning only yMA - pseudo bline > 20 as Outlier TRUE
                            i.high.yMA <- i.peak.high[which(Daily.Matrice$y_MA[i.which[i.peak.high]] - Daily.Matrice$pseu.bline[i.which[i.peak.high]] > 20)]
                            if (all(Daily.Matrice$Outliers[i.which[i.high.yMA]])) {
                                if (i != which(Peaks$poten.NO)[length(which(Peaks$poten.NO))]) next  
                            } else data.table::set(Daily.Matrice, i = i.which[i.peak.high], j = 'Outliers', value = TRUE)
                        }
                    }
                    if (identical(i.false.init, Daily.Matrice[intersect(i.which, which(Outliers != TRUE)), which = TRUE])) {
                        break
                    }
                    i.false.init <- Daily.Matrice[intersect(i.which, which(Outliers != TRUE)), which = TRUE]
                    if (length(i.false.init) == 0) {
                        break}
                }
            } else {
                i.rem <- max(Peaks[abs(Peak.values) == max(abs(Peaks$Peak.values), na.rm = T), which = T])
                # checking if this peak has a fast changing Sum_effect. If so, we will also exclude the neighboring peak to this peak
                if (count(abs(Daily.Matrice[i.which[Peaks$Ind.Start[i.rem]:Peaks$Ind.End[i.rem]]][['d.Sum_eff']]) > 0.5)/length(Peaks$Ind.Start[i.rem]:Peaks$Ind.End[i.rem]) > 0.50) { 
                    if (exists('discd')) {
                        i.upd <- i.which[-c(Peaks$Ind.Start[i.rem]:Peaks$Ind.End[i.rem], discd)]
                    } else i.upd <- i.which[-c(Peaks$Ind.Start[i.rem]:Peaks$Ind.End[i.rem])]
                    if (exists("fast.SumEff")) {
                        fast.SumEff <-  data.table::rbindlist(list(fast.SumEff, data.table::data.table(str = Peaks$Ind.Start[i.rem], end = Peaks$Ind.End[i.rem])), use.names = T) 
                    } else {
                        fast.SumEff <- data.table::data.table(str = Peaks$Ind.Start[i.rem], end = Peaks$Ind.End[i.rem])
                    }
                    if (i.rem != nrow(Peaks)) {
                        fast.SumEff <- data.table::rbindlist(list(fast.SumEff, data.table::data.table(str = Peaks$Ind.Start[i.rem+1], end = Peaks$Ind.End[i.rem+1])), use.names = T)
                        Peaks <- Peaks[-c(i.rem, i.rem+1)]
                    } else if (i.rem > 1) {
                        fast.SumEff <- data.table::rbindlist(list(fast.SumEff, data.table::data.table(str = Peaks$Ind.Start[i.rem-1], end = Peaks$Ind.End[i.rem-1])), use.names =T)
                        Peaks <- Peaks[-c(i.rem, i.rem-1)]
                    } else Peaks <- Peaks[-c(i.rem)]
                    ## Checking if the i.rem is at the end of the region and the end of the peak is negative. If so, we will not include it in the discarded peaks,
                    ## since the top of the peak can be over the critical value, in particular for long peaks
                } else if (i.rem == nrow(Peaks) || i.rem == 1) { # checking if the i.rem is at the beginning or end of the region
                    # We will assign the residual exceeding the critical value as Outlier TRUE
                    i.peak.upd <- Peaks$Ind.Start[i.rem]:Peaks$Ind.End[i.rem]
                    if (any(i.peak.upd[which(abs(resid.LM$resid[i.peak.upd]) > crtc.val)])) { # checking which part of the peak exceeding the critical value
                        if (any(i.peak.upd[which(resid.LM$resid[i.peak.upd] > crtc.val)])) {
                            i.rem.disc <- sort(c(i.peak.upd[which(resid.LM$resid[i.peak.upd] > crtc.val)]))
                        } else i.rem.disc <- sort(c(i.peak.upd[which(abs(resid.LM$resid[i.peak.upd]) > crtc.val)]))
                    } else i.rem.disc <- sort(c(i.peak.upd[which(Daily.Matrice$y_MA[i.which[i.peak.upd]] - Peaks$pseu.bline[i.rem] > 20)])) # means that the y_MA of this peak are at least 20 nA higher that the expected baseline
                    # Making the part of peak a data table to add to the outlier table in order later to exclude from the data set of residual (and peak)
                    # if (as.Date('2020-11-04') %in% unique(as.Date(Daily.Matrice$date[i.which]))) browser()
                    i.disc.gap <- i.rem.disc[which(diff(i.rem.disc) > 1)]
                    # checking if the discarded limits start with the 1st value of peak and after there is a gap. thus, 1st value repeated two times
                    if (length(i.disc.gap) > 0 && i.disc.gap[1] == i.rem.disc[1]) i.disc.gap <- c(i.disc.gap, i.rem.disc[which(i.disc.gap[1] == i.rem.disc[1])+1])
                    if (length(i.disc.gap) > 0 && i.disc.gap[length(i.disc.gap)] == i.rem.disc[length(i.rem.disc) - 1]) i.disc.gap <- c(i.disc.gap, i.rem.disc[length(i.rem.disc)])
                    disc.limits <- c(i.rem.disc[1], i.disc.gap)
                    disc.jumps <- data.table::data.table(Reg.Start = sapply(disc.limits, function(k) {
                        if (k == disc.limits[1] || k == i.rem.disc[length(i.rem.disc)]) {
                            return(k)
                        } else { 
                            if (length(disc.limits) > 2 && which(k == disc.limits) > 2) {
                                # in case the discarded limits start with the 1st value of peak and after there is a gap.
                                if (disc.limits[which(k == disc.limits)-1] == i.rem.disc[1] && 
                                    disc.limits[which(k == disc.limits)-2] == i.rem.disc[1]) {
                                    return(k)
                                } else return(i.rem.disc[which(k == i.rem.disc)+1])
                            } else return(i.rem.disc[which(k == i.rem.disc)+1])
                        }     
                    }))
                    disc.jumps[, Reg.End := sapply(1:(nrow(disc.jumps)), function(k) {
                        if (is.na(disc.jumps$Reg.Start[k])) {
                            return(NA_real_)
                        } else if (k != nrow(disc.jumps)) {
                            if (disc.jumps$Reg.Start[k] == disc.jumps$Reg.Start[k+1]) {
                                data.table::set(disc.jumps, i = as.integer(k+1), j = "Reg.Start", value = NA_real_)
                                return(disc.jumps$Reg.Start[k]) 
                            } else return(i.rem.disc[which(disc.jumps$Reg.Start[k+1] == i.rem.disc)-1])
                        } else return(i.rem.disc[length(i.rem.disc)])})]
                    data.disc <- data.table::data.table(str = as.integer(na.omit(disc.jumps)$Reg.Start), end = as.integer(na.omit(disc.jumps)$Reg.End))
                    
                    if (exists('which.out')) {
                        # Assigning outlier TRUE the resid > critical of that peak
                        which.out  <- data.table::rbindlist(list(which.out, data.disc), use.names = T)
                        i.upd      <- i.which[-c(i.which[i.rem.disc], i.which[discd])]
                    } else {
                        which.out <- data.disc
                        i.upd     <- i.which[-c(i.which[i.rem.disc])]
                    }
                    rm(data.disc, disc.limits, disc.jumps)
                    # Assigning outlier TRUE the resid < 0 or resid < critical value of that peak
                    data.table::set(Daily.Matrice, i = i.which[i.rem.disc], j = "Outliers", value = TRUE)
                    # re-defining the critical value by excluding the peak(s) found either while fast changing RH, or, first or last peak
                    # # to discard the outlier TRUE of the region (it may happen that the peak start - end are not all outlier TRUE or FALSE)
                    i.peak.discd <- which(!Daily.Matrice$Outliers[i.which])
                    if (length(i.peak.discd) > 1) {
                        
                        if (diff(range(Daily.Matrice[i.upd][[var2LM]], na.rm = T)) > 100) {
                            c.crtc <- 0.06
                            resid.LM.mad <- mad(resid.LM[i.peak.discd]$resid, constant = 2)
                        } else {
                            if (diff(range(Daily.Matrice[i.upd][[var2LM]], na.rm = T)) < 20) {
                                c.crtc <- 0.10
                            } else if (diff(range(Daily.Matrice[i.upd][[var2LM]], na.rm = T)) < 50) {
                                c.crtc <- 0.04
                            } else if (diff(range(Daily.Matrice[i.upd][[var2LM]], na.rm = T)) < 100) {
                                c.crtc <- 0.025
                            } 
                            resid.LM.mad <- mad(resid.LM[i.peak.discd]$resid)  
                        }
                        
                        sd.LM <- sd(resid.LM[i.peak.discd]$resid)
                        crtc.val <- min(crtc.val, max(c(sd.LM, resid.LM.mad, max(Daily.Matrice[i.upd][[var2LM]], na.rm = T)*0.03)))
                        # crtc.val <- min(crtc.val, max(min(sd.LM, resid.LM.mad), max(Daily.Matrice[i.upd][[var2LM]], na.rm = T)*0.03))
                        # if (as.Date('2021-01-03') %in% unique(as.Date(Daily.Matrice$date[i.which]))) browser()
                        if (resid.LM.mad/sd.LM > 5) { # in such a case, the residuals are likely non-normally distributed. Thus we need less stringent critical value to discard more peaks
                            crtc.val <- min(crtc.val, min(2*sd.LM, 0.4 * crtc.val))
                        }
                    }
                    if (length(i.peak.discd) < 1 || (all(abs(na.omit(Peaks$Peak.values)) < crtc.val) &&
                                                     all(!Peaks[!is.na(Peaks$Peak.values)]$poten.NO))) {
                        break
                    } else {
                        # browser()
                        if (identical(i.false.init, Daily.Matrice[intersect(i.which, which(Outliers != TRUE)), which = TRUE])) { # it means the residuals peaks all < critical value but > than pseu.baseline at leasy by 20
                            break
                        } else {
                            i.false.init <- Daily.Matrice[intersect(i.which, which(Outliers != TRUE)), which = TRUE]
                            if (length(i.false.init) == 0) {
                                break}
                        }
                        rm(i.upd)
                    }
                } else {
                    # assigning baseline TRUE only for the values exceeding the critical value. Alternatively we can exclude all peak
                    i.peak <- Peaks$Ind.Start[i.rem]:Peaks$Ind.End[i.rem]
                    # exceeding critical value
                    i.crtc <- i.peak[which(abs(resid.LM$resid[i.peak]) > crtc.val)]
                    if (length(i.peak)/length(i.crtc) > 0.5) {
                        data.table::set(Daily.Matrice, i = i.which[i.peak], j = "Outliers", value = TRUE)
                    } else data.table::set(Daily.Matrice, i = i.which[i.crtc], j = "Outliers", value = TRUE)
                    data.table::set(Peaks, i = i.rem, j = "Peak.values", value = NA_real_)
                    if (exists("which.out")){
                        which.out <- data.table::rbindlist(list(which.out, data.table::data.table(str = i.peak[1], end = i.peak[length(i.peak)])), use.names = T)
                    } else {
                        which.out <- data.table::data.table(str = i.peak[1], end = i.peak[length(i.peak)])}
                    if (identical(i.false.init, Daily.Matrice[intersect(i.which, which(Outliers != TRUE)), which = TRUE])) { # it means the residuals peaks all < critical value but > than pseu.baseline at leasy by 20
                        break
                    } else {
                        i.false.init <- Daily.Matrice[intersect(i.which, which(Outliers != TRUE)), which = TRUE]
                        if (length(i.false.init) == 0) {
                            break}
                    }
                }
            }
        } else {
            data.table::set(Daily.Matrice, i = i.which, j = "Outliers", value = TRUE)
            break
        }
    }
    return(list(Daily.Matrice = Daily.Matrice)) #, i.false.init = i.false.init, fit.nls = fit.nls, var2LM = var2LM, LMmodel = LMmodel
}

#' This function calculates the outliers of a region or any data set by setting LM, determining the peaks of residuals and evaluate them. Offset correction was added to lm-predicted data before calculating the residuals
#' @param i.which  the start:end of data region
#' @param Daily.Matrice the half-day data
#' @param var2LM the variable which LM will be set against y_MA
#' @param Last.Bline is the last predicted baseline in the previous region
#' @param Last.model is the last established good model
#' @param RH.sign is whether RH is iscreasing or decreasing
#' @param thrs.neg is the lower threshold to assess the baseline. we don't want the baseline go lower than it 
#' @param name.T and name.RH are the T and RH used in fitting the nls model
#' @param off.set if any yMA < 0, we do offset correction 
#' @param returns Daily.Matrice with updated Outliers, decision of fitting nls model later, and indexes of where to fit nls 
Find.outlier3 <- function(i.which, Daily.Matrice, var2LM, Last.Bline, Last.Model, RH.sign, thrs.neg, # off.set,
                          name.T, name.RH, ...) {
    ## assigning all data outlier to be FALSE. if we find any data to be outlier TRUE, we will assign it TRUE in the 'repeat'
    data.table::set(Daily.Matrice, i = i.which, j = "Outliers", value = FALSE)
    repeat {
        if (!exists('i.false.init')) i.false.init <- i.which
        if (length(i.false.init) > 15 && length(i.false.init)/length(i.which) > 0.20) { # at least 15 data and 20% of the region
            if (diff(range(Daily.Matrice[[var2LM]][i.false.init])) < 5) { #in case such a low range, polynomial model may end up prediction of bline1 in critic.val function enormously different from bline0
                LMmodel <- lm(y_MA ~ get(var2LM) , data = Daily.Matrice[i.false.init])
            } else {
                LMmodel <- lm(y_MA ~ get(var2LM) + I(get(var2LM)^2), data = Daily.Matrice[i.false.init])
                if (is.na(coef(LMmodel)[3]))  LMmodel <- lm(y_MA ~ get(var2LM), data = Daily.Matrice[i.false.init])
            }
            
            # LMmodel <- lm(y_MA ~ get(var2LM) + I(get(var2LM)^2), data = Daily.Matrice[i.false.init])
            # if (is.na(coef(LMmodel)[3]))  LMmodel <- lm(y_MA ~ get(var2LM), data = Daily.Matrice[i.false.init])
            R2.LM   <- summary(LMmodel)$r.squared
            if (R2.LM > 0.995) break
            # offset correction by modifying the intercept of model
            # if (!is.null(Last.Bline)) LMmodel$coefficients[1] <- coef(LMmodel)[1] + Last.Bline - predict(LMmodel, newdata = Daily.Matrice[i.which][1])
            pred.LM <- predict(LMmodel, newdata = Daily.Matrice[i.which])
            # if (as.Date('2020-05-18') %in% unique(as.Date(Daily.Matrice$date[i.which])) ||
            #     as.Date('2020-05-20') %in% unique(as.Date(Daily.Matrice$date[i.which]))) browser()
            # if (!is.null(Last.Bline) && abs(1 - Last.Bline/pred.LM[1]) > 0.1 || abs(Last.Bline - pred.LM[1]) > 5) browser()
            # if (!is.null(Last.Bline) &&  abs(Last.Bline - pred.LM[1]) > 5) browser()
            # if (!is.null(Last.Bline)) pred.LM <- pred.LM + Last.Bline - pred.LM[1]
            resid.LM <- data.table::data.table(resid = Daily.Matrice$y_MA[i.which] - pred.LM)
            # sd.LM  <- sd(resid.LM[which(i.which %in% i.false.init)]$resid)
            # rsd.LM <- sd.LM/mean(c(Daily.Matrice$y_MA[i.false.init] , predict(LMmodel)), na.rm = T)
            
            Peaks <- Find.Peaks(resid.LM, var = "resid")
            # adding pseudo baseline at the peak values
            Peaks[, pseu.bline := sapply(seq_along(Peaks$Peak.values), function(i) {
                return(Daily.Matrice$pseu.bline[i.which[Peaks$Ind.Peak[i]]])
            })]
            Peaks[, delta.var := sapply(seq_along(Peaks$Peak.values), function(i) {
                return(diff(range(Daily.Matrice[[var2LM]][i.which[Peaks$Ind.Start[i]:Peaks$Ind.End[i]]])))
            })]
            if (!exists('which.out')) which.out <- NULL 
            if (!exists('fast.SumEff')) fast.SumEff <- NULL 
            Peaks <- critic.val(Peaks = Peaks, i.which = i.which, Daily.Matrice = Daily.Matrice, var2LM = var2LM, pred.LM = pred.LM, resid.LM = resid.LM, Last.Bline = Last.Bline,
                                Last.Model = Last.Model, RH.sign = RH.sign, LMmodel = LMmodel, R2.LM = R2.LM, name.RH = name.RH, which.out = which.out, thrs.neg = thrs.neg,
                                fast.SumEff = fast.SumEff, name.T = name.T)$Peaks #, off.set = off.set
            # if (as.Date('2020-06-03') %in% unique(as.Date(Daily.Matrice$date[i.which])) ||
            #     as.Date('2020-08-20') %in% unique(as.Date(Daily.Matrice$date[i.which])) ||
            #     as.Date('2021-02-27') %in% unique(as.Date(Daily.Matrice$date[i.which])) ||
            #     as.Date('2021-04-11') %in% unique(as.Date(Daily.Matrice$date[i.which]))) browser()
            # browser()
            discd <- Peaks$discd
            if (all(Peaks[!is.na(Peaks$Peak.values)]$poten.bline)) {
                if (all(!Peaks[!is.na(Peaks$Peak.values)]$poten.NO)) {
                    break 
                } else {
                    
                    if (identical(i.false.init, Daily.Matrice[intersect(i.which, which(Outliers != TRUE)), which = TRUE])) { # it means the residuals peaks all < critical value but > than pseu.baseline at least by 20
                        if (all(!Peaks[!is.na(Peaks$Peak.values)]$poten.NO)) {
                            break
                        } else { # we will assign all the potential NO peaks as outlier TRUE
                            which.NO <- unlist(sapply(which(Peaks$poten.NO), function (i) i.which[Peaks$Ind.Start[i]:Peaks$Ind.End[i]]))
                            if (all(Daily.Matrice$Outliers[which.NO])) break
                            data.table::set(Daily.Matrice, i = which.NO, j = "Outliers", value = TRUE)
                            i.false.init <- Daily.Matrice[intersect(i.which, which(Outliers != TRUE)), which = TRUE]
                        }
                    } else {
                        i.false.init <- Daily.Matrice[intersect(i.which, which(Outliers != TRUE)), which = TRUE]
                        if (length(i.false.init) == 0) {
                            break}
                    }
                }
            } else {
                i.rem <- max(Peaks[Peak.values == max(Peaks[!Peaks$poten.bline]$Peak.values, na.rm = T), which = T])
                # checking if this peak has a fast changing Sum_effect. If so, we will also exclude the neighboring peak to this peak
                if (count(abs(Daily.Matrice[i.which[Peaks$Ind.Start[i.rem]:Peaks$Ind.End[i.rem]]][['d.Sum_eff']]) > 0.5)/length(Peaks$Ind.Start[i.rem]:Peaks$Ind.End[i.rem]) > 0.50) { 
                    if (!is.null(discd)) {
                        i.upd <- i.which[-c(Peaks$Ind.Start[i.rem]:Peaks$Ind.End[i.rem], discd)]
                    } else i.upd <- i.which[-c(Peaks$Ind.Start[i.rem]:Peaks$Ind.End[i.rem])]
                    if (!is.null(fast.SumEff)) {
                        fast.SumEff <-  data.table::rbindlist(list(fast.SumEff, data.table::data.table(str = Peaks$Ind.Start[i.rem], end = Peaks$Ind.End[i.rem])), use.names = T) 
                    } else {
                        fast.SumEff <- data.table::data.table(str = Peaks$Ind.Start[i.rem], end = Peaks$Ind.End[i.rem])
                    }
                    if (i.rem != nrow(Peaks)) {
                        fast.SumEff <- data.table::rbindlist(list(fast.SumEff, data.table::data.table(str = Peaks$Ind.Start[i.rem+1], end = Peaks$Ind.End[i.rem+1])), use.names = T)
                        Peaks <- Peaks[-c(i.rem, i.rem+1)]
                    } else if (i.rem > 1) {
                        fast.SumEff <- data.table::rbindlist(list(fast.SumEff, data.table::data.table(str = Peaks$Ind.Start[i.rem-1], end = Peaks$Ind.End[i.rem-1])), use.names =T)
                        Peaks <- Peaks[-c(i.rem, i.rem-1)]
                    } else Peaks <- Peaks[-c(i.rem)]
                    ## Checking if the i.rem is at the end of the region and the end of the peak is negative. If so, we will not include it in the discarded peaks,
                    ## since the top of the peak can be over the critical value, in particular for long peaks
                } else if (i.rem == nrow(Peaks) || i.rem == 1) { # checking if the i.rem is at the beginning or end of the region
                    # We will assign the residual exceeding the critical value as Outlier TRUE
                    i.peak.upd <- Peaks$Ind.Start[i.rem]:Peaks$Ind.End[i.rem]
                    if (any(i.peak.upd[which(abs(resid.LM$resid[i.peak.upd]) > Peaks$crtc.val[i.rem])])) { # checking which part of the peak exceeding the critical value
                        if (any(i.peak.upd[which(resid.LM$resid[i.peak.upd] > Peaks$crtc.val[i.rem])])) {
                            i.rem.disc <- sort(c(i.peak.upd[which(resid.LM$resid[i.peak.upd] > Peaks$crtc.val[i.rem])]))
                        } else i.rem.disc <- sort(c(i.peak.upd[which(abs(resid.LM$resid[i.peak.upd]) > Peaks$crtc.val[i.rem])]))
                    } else i.rem.disc <- sort(c(i.peak.upd[which(Daily.Matrice$y_MA[i.which[i.peak.upd]] - Peaks$pseu.bline[i.rem] > 20)])) # means that the y_MA of this peak are at least 20 nA higher that the expected baseline
                    # Making the part of peak a data table to add to the outlier table in order later to exclude from the data set of residual (and peak)
                    # if (as.Date('2020-11-04') %in% unique(as.Date(Daily.Matrice$date[i.which]))) browser()
                    i.disc.gap <- i.rem.disc[which(diff(i.rem.disc) > 1)]
                    # checking if the discarded limits start with the 1st value of peak and after there is a gap. thus, 1st value repeated two times
                    if (length(i.disc.gap) > 0 && i.disc.gap[1] == i.rem.disc[1]) i.disc.gap <- c(i.disc.gap, i.rem.disc[which(i.disc.gap[1] == i.rem.disc[1])+1])
                    if (length(i.disc.gap) > 0 && i.disc.gap[length(i.disc.gap)] == i.rem.disc[length(i.rem.disc) - 1]) i.disc.gap <- c(i.disc.gap, i.rem.disc[length(i.rem.disc)])
                    disc.limits <- c(i.rem.disc[1], i.disc.gap)
                    disc.jumps <- data.table::data.table(Reg.Start = sapply(disc.limits, function(k) {
                        if (k == disc.limits[1] || k == i.rem.disc[length(i.rem.disc)]) {
                            return(k)
                        } else { 
                            if (length(disc.limits) > 2 && which(k == disc.limits) > 2) {
                                # in case the discarded limits start with the 1st value of peak and after there is a gap.
                                if (disc.limits[which(k == disc.limits)-1] == i.rem.disc[1] && 
                                    disc.limits[which(k == disc.limits)-2] == i.rem.disc[1]) {
                                    return(k)
                                } else return(i.rem.disc[which(k == i.rem.disc)+1])
                            } else return(i.rem.disc[which(k == i.rem.disc)+1])
                        }     
                    }))
                    disc.jumps[, Reg.End := sapply(1:(nrow(disc.jumps)), function(k) {
                        if (is.na(disc.jumps$Reg.Start[k])) {
                            return(NA_real_)
                        } else if (k != nrow(disc.jumps)) {
                            if (disc.jumps$Reg.Start[k] == disc.jumps$Reg.Start[k+1]) {
                                data.table::set(disc.jumps, i = as.integer(k+1), j = "Reg.Start", value = NA_real_)
                                return(disc.jumps$Reg.Start[k]) 
                            } else return(i.rem.disc[which(disc.jumps$Reg.Start[k+1] == i.rem.disc)-1])
                        } else return(i.rem.disc[length(i.rem.disc)])})]
                    data.disc <- data.table::data.table(str = as.integer(na.omit(disc.jumps)$Reg.Start), end = as.integer(na.omit(disc.jumps)$Reg.End))
                    
                    if (!is.null(which.out)) {
                        # Assigning outlier TRUE the resid > critical of that peak
                        which.out  <- data.table::rbindlist(list(which.out, data.disc), use.names = T)
                        i.upd      <- i.which[-c(i.which[i.rem.disc], i.which[discd])]
                    } else {
                        which.out <- data.disc
                        i.upd     <- i.which[-c(i.which[i.rem.disc])]
                    }
                    rm(data.disc, disc.limits, disc.jumps)
                    # Assigning outlier TRUE the resid < 0 or resid < critical value of that peak
                    data.table::set(Daily.Matrice, i = i.which[i.rem.disc], j = "Outliers", value = TRUE)
                    # re-defining the critical value by excluding the peak(s) found either while fast changing RH, or, first or last peak
                    # # to discard the outlier TRUE of the region (it may happen that the peak start - end are not all outlier TRUE or FALSE)
                    i.peak.discd <- which(!Daily.Matrice$Outliers[i.which])
                    # updating the peak value and critical value of that peak
                    data.table::set(Peaks, i = i.rem, j = "Peak.values", value = ifelse(identical(i.peak.upd, i.rem.disc), NA_real_, max(resid.LM$resid[i.peak.upd[-i.rem.disc]], na.rm = T)))
                    data.table::set(Peaks, i = i.rem, j = "poten.bline", value = ifelse(is.na(Peaks$Peak.values[i.rem]), FALSE, Peaks$Peak.values[i.rem] < Peaks$crtc.val[i.rem]))
                    # data.table::set(Peaks, i = i.rem, j = "poten.bline", value = TRUE)
                    
                    # browser()
                    if (length(i.peak.discd) < 1 || (all(Peaks[!is.na(Peaks$Peak.values)]$poten.bline) &&
                                                     all(!Peaks[!is.na(Peaks$Peak.values)]$poten.NO))) {
                        break
                    } else {
                        # browser()
                        if (identical(i.false.init, Daily.Matrice[intersect(i.which, which(Outliers != TRUE)), which = TRUE])) { # it means the residuals peaks all < critical value but > than pseu.baseline at leasy by 20
                            if (all(!Peaks[!is.na(Peaks$Peak.values)]$poten.NO)) {
                                break
                            } else { # we will assign all the potential NO peaks as outlier TRUE
                                which.NO <- unlist(sapply(which(Peaks$poten.NO), function (i) i.which[Peaks$Ind.Start[i]:Peaks$Ind.End[i]]))
                                if (all(Daily.Matrice$Outliers[which.NO])) break
                                data.table::set(Daily.Matrice, i = which.NO, j = "Outliers", value = TRUE)
                                i.false.init <- Daily.Matrice[intersect(i.which, which(Outliers != TRUE)), which = TRUE]
                            }
                        } else {
                            i.false.init <- Daily.Matrice[intersect(i.which, which(Outliers != TRUE)), which = TRUE]
                            if (length(i.false.init) == 0) {
                                break}
                        }
                        rm(i.upd)
                    }
                } else {
                    # assigning baseline TRUE only for the values exceeding the critical value. Alternatively we can exclude all peak
                    i.peak <- Peaks$Ind.Start[i.rem]:Peaks$Ind.End[i.rem]
                    # exceeding critical value
                    i.crtc <- i.peak[which(abs(resid.LM$resid[i.peak]) > Peaks$crtc.val[i.rem])]
                    if (length(i.crtc)/length(i.peak) > 0.75 || Peaks$poten.NO[i.rem]) {
                        data.table::set(Daily.Matrice, i = i.which[i.peak], j = "Outliers", value = TRUE)
                    } else data.table::set(Daily.Matrice, i = i.which[i.crtc], j = "Outliers", value = TRUE)
                    data.table::set(Peaks, i = i.rem, j = "Peak.values", value = NA_real_)
                    if (!is.null(which.out)){
                        which.out <- data.table::rbindlist(list(which.out, data.table::data.table(str = i.peak[1], end = i.peak[length(i.peak)])), use.names = T)
                    } else {
                        which.out <- data.table::data.table(str = i.peak[1], end = i.peak[length(i.peak)])}
                    if (identical(i.false.init, Daily.Matrice[intersect(i.which, which(Outliers != TRUE)), which = TRUE])) { # it means the residuals peaks all < critical value but > than pseu.baseline at leasy by 20
                        break
                    } else {
                        i.false.init <- Daily.Matrice[intersect(i.which, which(Outliers != TRUE)), which = TRUE]
                        if (length(i.false.init) == 0) {
                            break}
                    }
                }
            }
        } else {
            data.table::set(Daily.Matrice, i = i.which, j = "Outliers", value = TRUE)
            break
        }
    }
    
    return(list(Daily.Matrice = Daily.Matrice)) #, i.false.init = i.false.init, fit.nls = fit.nls, var2LM = var2LM, LMmodel = LMmodel
}


#' This function calculates the critical values of residuals peak by modifying the T and RH 
#' @param Peaks is the residuals peak table 
#' @param Daily.Matrice the data where we identify baseline
#' @param i.which is the indexes of start and end of region
#' @param var2LM the variable 
#' @param Last.model the previous model to estimate the baseline when var2LM is 'pseu.bline' 
#' @param RH.sign is RH being increasing or decreasing
#' @param fast.SumEff is the indexes of RH changing fast in the region, if any
#' @param which.out is the indexes of already discarded data, if any
#' @param resid.LM is the residuals of LM model
#' @param thrs.neg is the threshold that we want the baseline to go lower than it
#' @param Last.Bline is the last estimated baseline of previous region
#' @param off.set if any yMA < 0, we do offset correction 
#' @param returns Daily.Matrice with updated Outliers, decision of fitting nls model later, and indexes of where to fit nls 
critic.val <- function(Peaks, i.which, Daily.Matrice, var2LM, Last.Model, RH.sign, LMmodel, R2.LM, pred.LM, name.T, name.RH,fast.SumEff, which.out, resid.LM = resid.LM, thrs.neg,
                       Last.Bline, rT =  1.10, yfT = -8.42, y0T = 6.8, rRH_inc=0.970, yfRH_inc = 25.6, y0RH_inc=46.4, rRH_dec = 1.033, yfRH_dec = 51.5,y0RH_dec = 46.2, # off.set,
                       ...) {
    # selecting the variation of T and RH to create a change in prediction
    varT  <- matrixStats::rowMaxs(as.matrix(data.table::data.table(perc = Daily.Matrice[[name.T]][i.which[Peaks$Ind.Peak]]*1.03, plus = Daily.Matrice[[name.T]][i.which[Peaks$Ind.Peak]] + 0.25))) # min(c(0.25, 0.02*diff(range(Daily.Matrice[[name.T]][i.which])))))))
    varRH <- matrixStats::rowMins(as.matrix(data.table::data.table(perc = Daily.Matrice[[name.RH]][i.which[Peaks$Ind.Peak]]*0.98, plus = Daily.Matrice[[name.RH]][i.which[Peaks$Ind.Peak]] - 1))) # min(c(1, 0.02*diff(range(Daily.Matrice[[name.T]][i.which])))))))
    
    ## Selecting the model based on var2LM
    if (var2LM == "Sum_eff" || is.null(Last.Model)) {
        if (RH.sign == "decr") {
            y0RH <- y0RH_dec
            yfRH <- yfRH_dec
            rRH  <- rRH_dec
        } else {
            y0RH <- y0RH_inc
            yfRH <- yfRH_inc
            rRH  <- rRH_inc
        }
        # estimating the expected baseline using the smoothed T and RH
        bline0 <- yfRH_dec + (y0RH_dec-yfRH_dec)  * ((rRH_dec)^(Daily.Matrice[[name.RH]][i.which[Peaks$Ind.Peak]] - 40)) +
            yfT + (y0T-yfT) * ((rT)^(Daily.Matrice[[name.T]][i.which[Peaks$Ind.Peak]] - 9.15))
        # estimating the expected baseline using the smoothed T and RH modified 2%
        bline1 <- yfRH_dec + (y0RH_dec-yfRH_dec)  * ((rRH_dec)^(varRH - 40)) + 
            yfT + (y0T-yfT) * ((rT)^(varT - 9.15))
    } else {
        # estimating the expected baseline 
        bline0 <- predict(Last.Model, newdata = Daily.Matrice[i.which[Peaks$Ind.Peak]])
        # estimating the expected baseline using T and RH modified 2%
        bline1 <- predict(Last.Model, newdata = data.frame(setNames(list(varT), name.T), 
                                                           setNames(list(varRH), name.RH)))
        
    }
    # Adding critical value
    
    if (diff(range(Daily.Matrice[i.which][["Sum_eff"]])) < 5) { # if the variation of var2LM along the region is too low, the critical value is too sensitive to the tiny variation in T and RH, in particular when LM model coefficients are too big
        # if (diff(range(Daily.Matrice[i.which][[var2LM]])) < 5) { # if the variation of var2LM along the region is too low, the critical value is too sensitive to the tiny variation in T and RH, in particular when LM model coefficients are too big
        Peaks[, crtc.val := sapply(seq_along(Peaks$Ind.Peak), function(i) {
            return(max(c(1, abs(bline0[i] - bline1[i]))))
        })]
    } else {
        Peaks[, crtc.val := sapply(seq_along(Peaks$Ind.Peak), function(i) { # if the variation of var2LM along the region is too low, we add min(0.5, 5% of bline0) nA to the predicted baseline to create a range for critical value
            if (Peaks$delta.var[i] < 1) {
                # return(max(c(0.5, abs(bline0[i] - bline1[i]))))
                return(max(c(0.5, abs(predict(LMmodel, newdata = data.frame(setNames(list(bline0[i]), var2LM))) -
                                          predict(LMmodel, newdata = data.frame(setNames(list(bline1[i] + min(1, 0.05*bline1[i])), var2LM))))))) # min(0.5, 0.05*bline1[i])
            } else {
                return(max(c(0.5, abs(predict(LMmodel, newdata = data.frame(setNames(list(bline0[i]), var2LM))) - 
                                          predict(LMmodel, newdata = data.frame(setNames(list(bline1[i]), var2LM)))))))
                # return(max(c(1, abs(predict(LMmodel, newdata = data.frame(setNames(list(bline0[i]), var2LM))) - 
                #                         predict(LMmodel, newdata = data.frame(setNames(list(bline1[i]), var2LM)))))))
            }
        })]
    }
    # checking for potential baseline
    Peaks[, poten.bline := sapply(seq_along(Peaks$Peak.values), function(i) {
        return(Peaks$Peak.values[i] < Peaks$crtc.val[i])
    })]
    
    if (!is.null(which.out)) {
        discd <- sort(as.integer(unique(unlist(sapply(seq_along(which.out$str), function(i) {c(which.out$str[i]:which.out$end[i])})))))
        Peaks[, Peak.values := sapply(seq_along(Peaks$Peak.values), function(i) {
            len.test <- setdiff(Peaks$Ind.Start[i]:Peaks$Ind.End[i], discd)
            if (length(len.test) == 0) {
                return(NA_real_)
            } else if (length(len.test) == length(Peaks$Ind.Start[i]:Peaks$Ind.End[i])) {
                return(Peaks$Peak.values[i])
            } else if (all(Daily.Matrice$Outliers[i.which[len.test]])) {
                return(NA_real_)
            } else {
                return(max(resid.LM$resid[len.test], na.rm = T))}
            # return(max(resid.LM$resid[Peaks$Ind.Start[i]:Peaks$Ind.End[i]], na.rm = T))}
        })]
    }
    Peaks[, poten.NO := sapply(seq_along(Peaks$Peak.values), function(i) {
        if (is.na(Peaks$Peak.values[i])  || 
            all(Daily.Matrice$Outliers[i.which[Peaks$Ind.Start[i]:Peaks$Ind.End[i]]])) { # all peak is already outlier TRUE
            return(FALSE)
            # } else if (any(Daily.Matrice$y_MA[i.which[Peaks$Ind.Start[i]:Peaks$Ind.End[i]]] < 0)) {
            #     return (FALSE)
        } else if (Daily.Matrice$pseu.bline[i.which[Peaks$Ind.Peak[i]]] - Daily.Matrice$y_MA[i.which[Peaks$Ind.Peak[i]]] > 20) {
            return (FALSE)
        } else {
            if (any(Daily.Matrice$pseu.bline[i.which[Peaks$Ind.Peak]] < 0)) {
                if (!is.null(Last.Bline)) {
                    if (Last.Bline > thrs.neg && Last.Bline < Daily.Matrice$y_MA[i.which[Peaks$Ind.Peak[i]]]) {
                        # ensuring the baseline continuity
                        val <- Last.Bline + Daily.Matrice$Sum_eff[i.which[Peaks$Ind.Peak[i]]] - Daily.Matrice$Sum_eff[i.which[1]]
                        if (R2.LM > 0.99) val <- max(val, pred.LM[Peaks$Ind.Peak[i]])
                        if (val < thrs.neg) {
                            val <- min(Daily.Matrice$y_MA[i.which[Peaks$Ind.Start[i]:Peaks$Ind.End[i]]])*0.70
                        } 
                    } else {
                        val <- min(Daily.Matrice$y_MA[i.which[Peaks$Ind.Start[i]:Peaks$Ind.End[i]]])*0.70 
                    }
                } else {
                    val <- Peaks$pseu.bline[i]
                }
            } else {
                if (!is.null(Last.Bline) && Last.Bline > thrs.neg) {
                    # if (!is.null(Last.Bline) && Last.Bline > thrs.neg && Last.Bline < Daily.Matrice$y_MA[i.which[Peaks$Ind.Peak[i]]]) {
                    val <- Last.Bline + Daily.Matrice$Sum_eff[i.which[Peaks$Ind.Peak[i]]] - Daily.Matrice$Sum_eff[i.which[1]]
                    if (R2.LM > 0.99) val <- max(val, pred.LM[Peaks$Ind.Peak[i]])
                    if (abs(val - Peaks$pseu.bline[i]) > 5) { # > 2*-thrs.neg # if the expected baseline is too different from the last baseline, we will ensure baseline continuity
                        val <- max(c(val, Peaks$pseu.bline[i]))
                    } else val <- mean(c(Peaks$pseu.bline[i],val)) # val <- Peaks$pseu.bline[i],val
                } else {
                    val <- Peaks$pseu.bline[i]
                }
            }
            if (val < 5) {
                val.crtc <- max(5, 2*val)
            } else if (val < 10) {
                if (diff(range(Daily.Matrice[i.which][["Sum_eff"]])) < 5) val.crtc <- max(5, 0.75*val) else val.crtc <- max(5, 1.5*val)
            } else if (val < 50) {
                val.crtc <- max(10, 0.50*val) 
            } else if (val < 100) {
                val.crtc <- max(20, 0.25*val) 
            } else val.crtc <- max(30, 0.20*val)
            # checking if the yMA being higher than expected baseline at least by 50% of the peak length
            # return(count(Daily.Matrice$y_MA[i.which[Peaks$Ind.Start[i]:Peaks$Ind.End[i]]] - val > val.crtc)/length(Peaks$Ind.Start[i]:Peaks$Ind.End[i]) > 0.50)
            return(Daily.Matrice$y_MA[i.which[Peaks$Ind.Peak[i]]] - val > val.crtc)
        } 
    })]
    
    # }
    
    # checking if the peaks are too high from the expected baseline (pseu baseline). This is to avoid assigning peaks that occur during long-pollution episodes
    # Peaks[, poten.NO := sapply(seq_along(Peaks$Peak.values), function(i) {
    #     if (is.na(Peaks$Peak.values[i])  || 
    #         all(Daily.Matrice$Outliers[i.which[Peaks$Ind.Start[i]:Peaks$Ind.End[i]]])) { # all peak is already outlier TRUE
    #         return(FALSE)
    #     } else if (any(Daily.Matrice$y_MA[i.which[Peaks$Ind.Start[i]:Peaks$Ind.End[i]]] < 0)) {
    #         return (FALSE)
    #     } else if (Daily.Matrice$pseu.bline[i.which[Peaks$Ind.Peak[i]]] - Daily.Matrice$y_MA[i.which[Peaks$Ind.Peak[i]]] > 20) {
    #         return (FALSE)
    #     } else {
    #         if (Daily.Matrice$pseu.bline[i.which[Peaks$Ind.Peak[i]]] < 0) { # in case the pseudo baseline estimated by the previous model it too negative, we will check how much yMA is over 0
    #             # if (Daily.Matrice$pseu.bline[i.which[Peaks$Ind.Peak[i]]] < thrs.neg) {
    #                 if (!is.null(Last.Bline) && Last.Bline > thrs.neg && Last.Bline < Daily.Matrice$y_MA[i.which[Peaks$Ind.Peak[i]]]) {
    #                     # ensuring the baseline continuity
    #                     val <- Last.Bline + Daily.Matrice$Sum_eff[i.which[Peaks$Ind.Peak[i]]] - Daily.Matrice$Sum_eff[i.which[1]] 
    #                 } else val <- min(Daily.Matrice$y_MA[i.which[Peaks$Ind.Start[i]:Peaks$Ind.End[i]]])*0.70
    #                 val.crtc <- 10
    #             # } else {
    #             #     # val <- min(Daily.Matrice$y_MA[i.which[Peaks$Ind.Start[i]:Peaks$Ind.End[i]]])*0.70
    #             #     val <- Peaks$pseu.bline[i]/2
    #             #     val.crtc <- 10
    #             #     # val.crtc <- 10-thrs.neg # thrs.neg is negative
    #             # }
    #         } else if (Daily.Matrice$pseu.bline[i.which[Peaks$Ind.Peak[i]]] < 10) { # the expected baseline range is too low. polynomial model may assign some high peaks as baseline based on similarities of yMA and var2LM, 
    #             # but the ranges of yMA and var2LM are too high
    #             # val <- mean(c(pred.LM[Peaks$Ind.Peak[i]], Peaks$pseu.bline[i])) 
    #             val <- Peaks$pseu.bline[i]
    #             val.crtc <- 10
    #         } else if (!is.null(Last.Bline) && abs(Last.Bline - Daily.Matrice$pseu.bline[i.which[1]]) > 2*-thrs.neg) { # if the expected baseline is too different from the last baseline, we will ensure baseline continuity
    #             val <- Last.Bline + Daily.Matrice$Sum_eff[i.which[Peaks$Ind.Peak[i]]] - Daily.Matrice$Sum_eff[i.which[1]] 
    #             if (val < thrs.neg) val <- Peaks$pseu.bline[i] 
    #             val.crtc <- 10 
    #         } else {
    #             val.crtc <- 10 #val.crtc <- 20
    #             val <- mean(c( pred.LM[Peaks$Ind.Peak[i]], Peaks$pseu.bline[i]))
    #             # if (Peaks$pseu.bline[i] < 0 || R2.LM > 0.98) {  
    #             #     val <- pred.LM[Peaks$Ind.Peak[i]]
    #             # } else val <- Peaks$pseu.bline[i]
    #         }
    #         if (Peaks$pseu.bline[i] > 0) val.crtc <- val.crtc + 0.1* Peaks$pseu.bline[i]
    #         if (val < 10) val.crtc <- max(5, 0.75*val)
    #         # checking if the yMA being higher than expected baseline at least by 50% of the peak length
    #         # return(count(Daily.Matrice$y_MA[i.which[Peaks$Ind.Start[i]:Peaks$Ind.End[i]]] - val > val.crtc)/length(Peaks$Ind.Start[i]:Peaks$Ind.End[i]) > 0.50)
    #         return(Daily.Matrice$y_MA[i.which[Peaks$Ind.Peak[i]]] - val > val.crtc)
    #     } 
    # })]
    
    # # checking the sign of sumeff and y_MA at each peak. If the signs are opposite, this peak is potentially NO
    # Peaks[, sign.sumeff := sapply(seq_along(Peaks$Ind.Peak), function (i) {
    #     if (count(Daily.Matrice$d.Sum_eff[i.which[Peaks$Ind.Start[i]:Peaks$Ind.End[i]]] > 0) > length(Peaks$Ind.Start[i]:Peaks$Ind.End[i])/2 + 1) return(1) else return(-1)
    # })]
    # Peaks[, sign.yMA := sapply(seq_along(Peaks$Ind.Peak), function (i) {
    #     if (count(diff(Daily.Matrice$y_MA[i.which[Peaks$Ind.Start[i]:Peaks$Ind.End[i]]]) > 0) > length(Peaks$Ind.Start[i]:Peaks$Ind.End[i])/2 + 1) { # yMA increasing
    #         return(1)
    #     } else return (-1)
    # })]
    # 
    # for (i in Peaks[!is.na(Peaks$Peak.values) & !Peaks$poten.NO, which = T]) {
    #     if ( all(Daily.Matrice$y_MA[i.which[Peaks$Ind.Start[i]:Peaks$Ind.End[i]]] > 0) &&  
    #         Peaks$sign.sumeff[i] != Peaks$sign.yMA[i]) data.table::set(Peaks, i = i, j = 'poten.NO', value = TRUE)
    # }
    
    if (!is.null(fast.SumEff)) {
        i.fast.SumEff <- as.integer(unique(unlist(sapply(seq_along(fast.SumEff$str), function(i) {c(fast.SumEff$str[i]:fast.SumEff$end[i])}))))
        Peaks[, Peak.values := sapply(seq_along(Peaks$Peak.values), function(i) {
            len.test <- setdiff(Peaks$Ind.Start[i]:Peaks$Ind.End[i], i.fast.SumEff)
            if (length(len.test) == 0) {
                return(NA_real_)
            } else if (all(Daily.Matrice$Outliers[i.which[len.test]])) {
                return(NA_real_)
            } else return(Peaks$Peak.values[i])
        })]
    }
    if (!exists('discd')) discd <- NULL
    return(list(Peaks = Peaks, discd = discd)) 
}

