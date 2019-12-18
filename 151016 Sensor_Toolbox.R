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
# Etalonnage: Etalonnage returns xlim and ylim that can later be used with Cal_line (linear only for now)
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
### Function Load.Packages (170420)
#================================================================CR
Load.Packages <- function(list.Packages) {
    # list.Packages                 vector of names of the packages to load
    #
    cat("", sep = "\n")
    cat("[Load.Packages] INFO CHECK Installed packages and Toolbox to run the script", sep = "\n")
    #
    for (i in list.Packages) {
        # Installing packages
        if (i %in% rownames(installed.packages()) == FALSE) {
            cat(sprintf("[Load.Packages] INFO Installing %s", i), sep = "\n")
            install.packages(i)
        } else {
            cat(sprintf("[Load.Packages] INFO Package %s already installed",i), sep = "\n")
        }
        # cat(i,quote=FALSE)
        do.call("library", as.list(i))
        #library(i, character.only = TRUE)
        cat(sprintf("[Load.Packages] INFO Package %s loaded",i), sep = "\n")
    }
    #
    # List of loaded packages
    cat("[Load.Packages] INFO List of installed packages", sep = "\n")
    print(search(), quote = FALSE)
    cat("", sep = "\n")
}
#================================================================CR
### resetPar: Function reset graphical parameters
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
### sink.reset: Function reset sink number
#================================================================CR
sink.reset <- function() {
    for (i in seq_len(sink.number())) {
        sink(NULL)
    }
}
#================================================================CR
### stopWhenError: Function reset sink and device errors
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
### slope_orth: Function Orthogonal regression
#================================================================CR
slope_orth <- function(Xlabel, Ylabel, Title, Sensor_name = NULL,
                       DQO.1 = NA, LV = NA, Units = NULL, Disk = NA, WD = NA, Dir = NA,
                       Mat, ubsRM = NULL, variable.ubsRM = FALSE, ubss = NULL, variable.ubss = FALSE,
                       lim = NULL, f_coef1 = NULL, f_coef2 = NULL, f_R2 = NULL, nameModel = NULL, SavePlot = TRUE,
                       calib = NULL) {
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
        if (exists("Mat") && !is.null(Mat) && nrow(Mat)>0) {
            Mat$Max.ubsRM <- sqrt(RSS/(nb-2) + Mat$bias^2)
            Mat$Max.RSD   <- sqrt(RSS/(nb-2) + Mat$bias^2)/Mat$xis
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
### U.orth.DF: Function Orthogonal regression without plotting (Vs 180505)
#================================================================CR
U.orth.DF <- function(Mat, ubsRM = NULL, variable.ubsRM = FALSE, ubss = NULL, variable.ubss = FALSE, Fitted.RS = FALSE) {
    # Mat            : Data.table or DataFrame of data including Case number, Date, x And y + optional ubsRM if ubsRM is not constant for all reference values.
    #                  Idem for ubs.
    # Mat            : DataFrame of data including Case number, Date, x And y + optional ubsRM if ubsRM is not constant for all reference values
    # ubsRM            : numeric (default = NULL ), random standard uncertainty of the results of the reference method, xis, given as a constant value for all xis reference values
    # variable.ubsRM   : logical, if FALSE (default = FALSE ), ubsRM is used as constant random standard uncertainties for all xis reference values.
    #                  If TRUE ubsRM given in Mat and is used for each reference values
    # Fitted.RS      : logical, default is FALSE. If TRUE the square residuals (RS) can be fitted using a General Additive Model, provided that the probability that the
    #                  correlation between xis and RS is null is lower than 0.01, (p < 0.01)
    # return a list with the orthogonal regression:
    #                 "mo","sdo", "mm","sdm", "b1", "ub1", "b0", "ub0", "RSS","rmse", "mbe", "Correlation", "nb", "CRMSE", "NMSD",
    #                 and "Mat": "case", "Date", "xis", "yis","ubsRM", "RS", "Ur", "U", "Rel.bias", "Rel.RSS"
    #
    # Homogeneity of variance is tested For the calculation of RSS
    # adding Ur In a New field of Mat
    # returning a list with slope (b and ub), intercept (a and ua), the sum of square of residuals (RSS),
    # the root means square of error (RMSE), the mean bias error (mbe), the coefficeint of correlation (Correlation),
    # the number of valid measurment (nb), the centred rout mean square of error (CRMSE), the normalised mean standard deviation (NMSD)
    # and Mat with relative expanded uncertainty
    #checking that the mat dataFrame is not empty
    if (exists("Mat") && !is.null(Mat) && nrow(Mat)>0) {
        # Setting column names and adding ubsRM and ubss as constant values
        colnames(Mat) <- c("case", "Date", "xis", "yis","ubsRM", "ubss")[1:length(colnames(Mat))]
        if (!"data.table" %in% class(Mat)) Mat <- data.table(Mat, key = "Date")
        if (!variable.ubsRM) {
            if (!is.null(ubsRM)) {
                Mat$ubsRM <- rep(ubsRM, nrow(Mat))
            } else {
                if ("ubsRM" %in% names(Mat)) return("[U.orth.DF] ERRROR, value of u(ubsRM) not given. Stopping the function. \n")
            }
        }
        if (!variable.ubss) {
            if (!is.null(ubss)) {
                Mat$ubss   <- rep(ubss, nrow(Mat))
            } else {
            if (!"ubs" %in% names(Mat)) return("[slope_orth] ERROR, value of u(ubs) not given. Stopping the function. \n")
            }
        }
        # Filtering for the validation data only
        Mat <- Mat[complete.cases(Mat[,.(xis,yis)])]
        #Orthogonal regression (see annex b of equivalence method)
        nb     <- nrow(Mat)
        mo     <- mean(Mat[["xis"]])
        mm     <- mean(Mat[["yis"]])
        sdo    <- sd(Mat[["xis"]])
        sdm    <- sd(Mat[["yis"]])
        Sxx    <- sum((Mat[["xis"]] - mo)^2)
        Syy    <- sum((Mat[["yis"]] - mm)^2)
        Sxy    <- sum((Mat[["xis"]] - mo) * (Mat[["yis"]] - mm))
        b1     <- (Syy - Sxx + sqrt((Syy- Sxx)^2 + 4*Sxy^2))/(2*Sxy)
        b0     <- mm - b1 * mo
        ub1    <- sqrt((Syy - (Sxy^2/Sxx))/((nb-2)*Sxx))
        ub0    <- sqrt(ub1^2 * sum(Mat$xis^2)/nb)
        # Regression statistics for Target Diagram (see delta tool user guide)
        rmse  <- sqrt((sum((Mat[["yis"]] - (b0 + b1 * Mat[["xis"]]))^2))/(nb-2))
        mbe   <- mean(Mat[["yis"]] - Mat[["xis"]])
        mae   <- mean(abs(Mat[["yis"]] - Mat[["xis"]]))
        CRMSE <- sqrt(mean(((Mat[["yis"]] - mm) - (Mat[["xis"]] - mo))^2))
        NMSD  <- (sd(Mat[["yis"]]) - sd(Mat$xis)) / sd(Mat[["xis"]])
        Correlation <- cor(Mat[["xis"]],Mat[["yis"]])
        # Squares of Residuals and bias (vector of values)
        Mat[,   RS := (Mat[["yis"]] - (b0 + b1 * Mat[["xis"]]))^2]
        Mat[, bias := (b0 + (b1 - 1) * Mat[["xis"]])]
        # Squares of Residuals and bias (vector of values)
        Mat$RS   <- (Mat$yis - (b0 + b1 * Mat$xis))^2
        Mat$bias <- (b0+(b1-1)*Mat$xis)
        # Sum of squares of Residuals (one constant value)
        RSS     <- sum(Mat$RS)
        # Checking if RS - Mat$ubsRM^2 < 0 that make an error using sqrt(RS - Mat$ubsRM^2) of the ree.RSS. Replacing with 0
        neg.RSS <- which(RSS/(nb-2) - Mat[["ubsRM"]]^2 <= 0)
        if (length(neg.RSS) > 0) {
            # adding 0.1 % of min(xis) to avoid problem of 0 with gam fitting
            Mat[neg.RSS, RS := Mat[neg.RSS, ubsRM]^2 + 0.001 * min(Mat[["xis"]])]
            # Recalculating RSS when Mat$RS are changed
            RSS     <- sum(Mat[["xis"]])
        }  else {
            # mat$RS are not changed and they are already calculated
            cat("The \"Sum of Squares of Residuals - u(ubsRM)^2\" is always negative \"sqrt(RSS/(n-2) - u(ubsRM)^2\" can be calculated, check in df Mat: x and max(ubsRM).\n")
        }
        # tesing significance of correlation between s and square of absolute reisduals - The calculation does not work only possibility the constrant RSS
        rtest <- cor.test(Mat$xis, Mat$RS)
        print(rtest, quote = FALSE)
        cat(sprintf("probability of H0 (r=0): %f, if <0.05, correlation is demonstrated, if > 0.95 there is no correlation",rtest$p.value), "\n")
        # if fitting the square of residuals is needed
        if (Fitted.RS && rtest$p.value < 0.01) {
            RS.Fitted <- TRUE
            cat("The residuals are not constant. RSS is calculated after applying a gam fitting .\n")
            #z <- lm((Mat$yis - (b0 + b1 * Mat$xis))^2 ~ Mat$xis)
            # Fitting with gam
            # if any y value is zero getting Warning: Error in eval: non-positive values not allowed for the 'gamma' family (we had 0.5 % of min(xis) to avoid this
            z <- gam( Mat$RS ~ s(Mat$xis), family=Gamma(link=log) )
            Mat$RS <- fitted(z)
            #### Calculating uncertainty
            Mat$Ur <- 2 * sqrt(Mat$ubss^2 + Mat$RS - Mat$ubsRM^2 + Mat$bias^2)/Mat$xis * 100
            Mat$U  <- 2 * sqrt(Mat$ubss^2 + Mat$RS - Mat$ubsRM^2 + Mat$bias^2)
            #### Calculating parameters for modified Target diagram
            Mat$Rel.bias <- 2 * (b0/Mat$xis + (b1 - 1))
            Mat$Rel.RSS  <- 2 * (sqrt(Mat$ubss^2 + Mat$RS - ubsRM^2) / Mat$xis)
        } else {
            RS.Fitted <- FALSE
            cat("The residuals are constant. RSS is calculated with equation for constant residuals.\n")
            cat(sprintf("RSS is the square root of sum of squares of Residuals divided by n - 2: %f", sqrt(sum((Mat$yis/(b0+b1*Mat$xis)-1)^2))/nb^2), "\n")
            # No need to lot a fitted line in this case
            #### Calculating uncertainty
            Mat$Ur <- 2 * sqrt(Mat$ubss^2 + RSS/(nb-2) - Mat$ubsRM^2 + Mat$bias^2)/Mat$xis * 100
            Mat$U  <- 2 * sqrt(Mat$ubss^2 + RSS/(nb-2) - Mat$ubsRM^2 + Mat$bias^2)
            #### Calculating parameters for modified Target diagram
            Mat$Rel.bias <- 2 * (b0/Mat$xis + (b1 - 1))
            Mat$Rel.RSS  <- 2 * (sqrt(Mat$ubss^2 + RSS/(nb - 2) - ubsRM^2) / Mat$xis)
        }
        Mat$Max.ubsRM <- sqrt(RSS/(nb-2) + Mat$bias^2)
        Mat$Max.RSD   <- sqrt(RSS/(nb-2) + Mat$bias^2)/Mat$xis
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
            cat(sprintf("RSS: %.4g ",Mat$RSS[1]), "\n")
        }
        cat(sprintf("RMSE : %.4g ",rmse), "\n")
        cat(sprintf("mbe  : %.4g ",mbe), "\n")
        cat(sprintf("CRMSE: %.4g ",CRMSE), "\n")
        cat(sprintf("NMSD : %.4g ",NMSD), "\n")
        cat(sprintf("n    : %.4g ",nb), "\n")
        calib <- list(mo,sdo, mm,sdm, b1, ub1, b0, ub0, RSS, rmse, mbe, Correlation, nb, CRMSE, NMSD, Mat, RS.Fitted)
    } else {
        cat("The Mat dataFrame is empty. Returning NAs.")
        calib <- list(mo = NA,sdo = NA, mm = NA,sdm = NA, b1 = NA, ub1 = NA, b0 = NA, ub0 = NA, RSS = NA,rmse = NA, mbe = NA, Correlation = NA, nb = NA, CRMSE = NA, NMSD = NA, Mat = NA)
    }
    names(calib) <- c("mo","sdo", "mm","sdm", "b1", "ub1", "b0", "ub0", "RSS","rmse", "mbe", "Correlation", "nb", "CRMSE", "NMSD", "Mat", "RS.Fitted")
    print(data.frame(x              = Mat$xis,
                     ubsRM          = Mat$ubsRM,
                     Max.ubsRM      = sqrt(RSS/(nb-2) + Mat$bias^2),
                     Max.RSD        = sqrt(RSS/(nb-2) + Mat$bias^2)/Mat$xis,
                     Decrease.ubsRM = (Mat$ubsRM - sqrt(RSS/(nb-2) + Mat$bias^2)) > 0))
    return(calib)
}
#================================================================CR
### f_log: Function Fitting a logarithmic model
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
### f_Unitec: Function Fitting a the Unitec model
#================================================================CR
f_Unitec <- function(x, a, b,c) {
    # mVolt <- ((y-a)/b)^(1/c)
    return(y = (((x*1.91*(293.15/TK))-a)/b)^(1/c))
    # Y(?g/m3) (*1.91) = a+b*X(mV)^c
    # with a=-31.6  b=5330.9  -0.598, x en mV	min(x)=47.2	max(x)=5299.9	unitY=?g/m3	ymax=500?g/m3
    # Equation of Unitec
}
#================================================================CR
### f_Michelis: Function Fitting a Michaelis-Menten kinetics model with intercept
#================================================================CR
f_Michelis <- function(x, Vmax, km, intercept) {
    # Vmax is the max asymtoptic value on y axis
    # intercept is the min asymtoptic value on y axis
    # km is The Michaelis constant is the concentration at which the sensor response is half of V_\max
    return(Vmax * x / (km  + x) + intercept)
}
#================================================================CR
### f_ExpGrowth: Function Fitting an Exponential Growth model without intercept
#================================================================CR
f_ExpGrowth <- function(x, C, k) {
    # https://people.richland.edu/james/lecture/m116/logs/models.html
    # y = C ekx, k > 0
    # Features
    # Asymptotic to y = 0 to left
    # Passes through (0,C)
    # C is the initial value
    return(C * (exp(k*x)))
}
f_exp_kT <- function(x, a0, a1, C, k, Temperature) {
    # https://people.richland.edu/james/lecture/m116/logs/models.html
    # y = a0 + a1 x + C ekT, k > 0
    # Features
    # Asymptotic to y = 0 to left
    # Passes through (0,C)
    # C is the initial value
    return(a0 + a1 * x + exp(k * Temperature + C) )
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
f_normal <- function(x, mu, sigma) {
    #return the normal distribution in function of c, mu and sigma
    return( 1/(sqrt(2*pi * sigma^2)) * exp(-((x - mu)^2)/(2 * sigma^2)))
}
#================================================================CR
### f_ExpDD_Int: Function Fitting an exponential Decay (Decreasing form) model with intercept
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
### f_ExpDI: Function Fitting an exponential Decay (Increasing form) model
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
### f_ExpDI_Int: Function Fitting an exponential Decay (Increasing form) model with intercept (see wikipedia)
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
### f_Sigmoid: Function Fitting a Logistic - Sigmoidal function
#================================================================CR
f_Sigmoid <- function(x, MIN, Asym, xmid, Hill) {
    return(y = MIN + (Asym-MIN) / (1 + (xmid/x)^Hill))
    #, Hill > 0, between 1a dn 10
    #Asymptotic to y = Max at right,
    #Asymptotic to y = Min at left,
    #Passes through (0, a/(1+b) )
    #x50 x value at the inflection point, in this equation x is not log transformed
}
#================================================================CR
### # normal distribution (on logarithmic scale) Be careful I think it is a duplicated
#================================================================CR
f_normal <- function(x, mu, sigma) {
    #return the normal distribution in function of c, mu and sigma
    return( 1/( sqrt(2*pi * sigma^2)  ) * exp(-(( x -mu)^2 ) / (2 * sigma^2) ))
}
#================================================================CR
# normal distribution (on logarithmic scale), This is for fitting a normal distribution on DMPS and APS
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
    return(dnorm(x, mean = mu, sd = sigma))
}
#================================================================CR
# Decimal log normal distribution. This is for fitting a normal distribution on DMPS and APS
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
# error function to minimize the differences between APS and DMPS count based on common diameters
#==================================================================================================CR
f_Error <- function(density, DataXY.APS, Model.i){
    DataXY.APS <- DataXY.APS %>% mutate(x = log10(10^x/sqrt(density)))
    DataXY.APS$Predicted_APS <- predict(Model.i$Model, DataXY.APS)
    return(sum(DataXY.APS$y - DataXY.APS$Predicted_APS)^2)
}
#================================================================CR
### Estimated.y: Function Plot estimated function
#================================================================CR
Estimated.y <- function(x, Model, Length = NULL) {
    # This function estimates the y values at 5000 points of x from min(x) until max(x)
    # x      : Data used to establish the model (only x)
    # Model  : the Model to plot
    # Length : number of rows to be returned, default = NULL
    #
    # Return : data.frame estimated with variable x and y, each one with 5000 points in a x ascending order
    if (is.null(Length) || Length < 5000 ) Estimated <- data.frame(x = seq(min(x),max(x),length.out=5000), y= seq(min(x),max(x),length.out=5000)) else {
        Estimated <- data.frame(x = seq(min(x),max(x),length.out=Length), y= seq(min(x),max(x),length.out=Length))
    }
    if (inherits(Model, "gam")) { # checking if the model was fitted from gam function
        Estimated$y <- predict.gam(Model, newdata = Estimated, type = "response")
    }
    else{
        Estimated$y <- predict(Model, newdata = Estimated)
    }
    return(Estimated)
}
#================================================================CR
### Cal_Line: Function calibration function and plot calibration line (VS 170428)
#================================================================CR
Find_Max_CCF <- function(x,y) {
    # https://stackoverflow.com/questions/10369109/finding-lag-at-which-cross-correlation-is-maximum-ccf
    # This function will return the maximum CCF value along with corresponding lag value.
    # x,y      Input to this function are a and b which are nothing but two time series.
    # keep only complete cases
    DataXY <- data.frame(x = x, y = y)
    DataXY <- DataXY[complete.cases(DataXY),]
    d <- ccf(DataXY$x, DataXY$y, plot = FALSE) # , lag.max = length(x)/2
    cor     = d$acf[,,1]
    abscor  = abs(d$acf[,,1])
    lag     = d$lag[,,1]
    res     = data.frame(cor,lag)
    absres  = data.frame(abscor, lag)
    absres_max = res[which.max(absres$abscor),]
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
Cal_Line <- function(x, s_x, y, s_y, Mod_type,  Multi.File = NULL, Matrice=NULL, line_position, Couleur, Sensor_name = NULL, f_coef1, f_coef2, f_R2,
                     lim = NULL, marges = NULL, Covariates = NULL, Weighted = FALSE, Lag_interval = sqrt((max(x, na.rm = T) - min(x, na.rm = T))),
                     Auto.Lag = FALSE, Plot_Line = TRUE, Verbose = TRUE) {
    # This Function estimates the calibration function, plots the calibration line and write the equation above the plot at line_position
    # The regression equation can be weithed (1/sy^2) or not if s_y = Null
    # Inputs:
    # x, s_x, y, s_y: x and y values with their standard devation that can be equal to Null
    # Mod_type      : type of calibration model: linear, ...
    # Multi.File    : char, default is NULL, path.file of the config file used for calibration with multivariates
    # Matrice       : Input Matrix of data (e. g. Pre_cal_df)
    # Covariates    : character vectors with column names of the covariates found in Matrice and needed for calibration model
    # line position : for mtext of the regression equation
    # Couleur       : color of the line and color font of the equation
    # Sensor_name   : name of the sensor to be written in front of the calibration equation. If NULL, sensor name is not printed
    # f_coef1,
    # f_coef2,
    # f_R2          : number of digit for intercept, slope and R2 using sprintf syntax.
    #                 f_coef1 is used both for intercept and s(Res), f_coef2 is used for all parameters of the model apart from the intercept
    # lim           : matrix of column vectors Xlim, YLIM: limits of the plots where to add the calibration line. When NULL the lim are automatically calculated.
    # marges        : margin of graph, if NULL set as c(4,4,3,0.5)). If you don't want to set the margin, set to "NO"
    # Weighted      : Logical, default is false. If true used weighted fitting base on standard deviations of y for each lag (sd^2/sum(sd^2))
    # Lag_interval  : numerical, double, default sqrt((max(x, na.rm = T) - min(x, na.rm = T)), width of each lag used for estimating laf interval
    # Auto.Lag      : logical, default is FaLSE If Auto.Lag is TRUE, y is changed using the lag at which cross correlation between x and y is maximum using ccf( )
    # Plot_Line     : logical, default is TRUE. If TRUE the calibration line is added using par(new=TRUE) to an existaing scatterplot
    # Return: the estimated model , plot a calibration is Plot_Line is TRUE
    # saving the original par() values
    if (Plot_Line) {
        op <- par(no.readonly = TRUE)
        # resuming the par values
        on.exit(par(op))
        # settings the margins
        if (is.null(marges)) {
            Margin <- c(4,4,3,0.5)
            par(mar = c(4,4,3,0.5))
        } else {
            Margin <- par("mar")
        }
        #Define the limits of the graphs
        if (is.null(lim)) {
            Xrange <- c(min(x, na.rm = T), max(x, na.rm = T))
            Yrange <- c(min(y, na.rm = T), max(y, na.rm = T))
            lim = cbind(Xrange, Yrange)
        }
    }
    # check autocorelation and Lag of y versus x, adding NAs at the begining or end of x, y, s_y and Matrice
    if (Auto.Lag) {
        Lag <- Find_Max_CCF(x,y)
        if (Verbose) cat(paste0("[Cal_line] INFO, there is a lag between x and y, of ", Lag$lag," row of data which gives a better correlation between x and y, if lag is <> 0\n"))
        if (Lag$lag != 0) {
            if (Lag$lag > 0) {
                x <- c(x, rep(NA, Lag$lag))
                y <- c(rep(NA, Lag$lag), y)
                if (!is.null(s_y)) s_y <- c(rep(NA, Lag$lag), s_y)
                if (!is.null(Matrice)) Matrice <- berryFunctions::insertRows(Matrice, seq_along(Lag$lag))
            } else {
                x <- c(rep(NA, Lag$lag, x))
                y <- c(y, rep(NA, Lag$lag))
                if (!is.null(s_y)) s_y <- c(s_y, rep(NA, Lag$lag))
                if (!is.null(Matrice)) Matrice <- berryFunctions::insertRows(Matrice, seq(from = nrow(Matrice) - Lag$lag, to = nrow(Matrice)))
            }
        }
    } else Lag <- "Lag correction not requested"
    # Put reference and sensor reponses and standard deviation of sensor responses in a matrix remove NA of x and y
    if (is.null(s_y) || any(s_y == 0) || all(is.na(s_y))) DataXY <- data.table(x = x, y = y) else {
        DataXY    <- data.table(x = x, y = y, s_y = s_y)
        DataXY[ wi := DataXY$s_y^-2/sum(DataXY$s_y^-2)]
        # colnames(DataXY) <- c("x","y","s_y","wi")
    }
    # adding Covariates for multilinear model
    if (Mod_type == "MultiLinear")                                    DataXY <- cbind(DataXY, Matrice[, ..Covariates])
    if (any(Mod_type %in% c("exp_kT", "exp_kK","T_power","K_power"))) DataXY[, Temperature := Matrice[["Temperature"]]]
    # removing NA of any variables in DATAXY
    DataXY <- DataXY[complete.cases(DataXY[, .SD])]
    # Creating weights from the standard deviations within lags
    if (Weighted) {
        #lag_distance <- round((range(DataXY$x, na.rm = T)[2] - range(DataXY$x, na.rm = T)[1])/Lag_numbers)
        # any co-variates?
        if (!is.null(Covariates)) {
            covariates <- sym(Covariates)
            # creating index, group by and take the mean of x and y
            # https://stackoverflow.com/questions/26724124/standard-evaluation-in-dplyr-summarise-on-variable-given-as-a-character-string
            DataXY <- DataXY %>%
                dplyr::mutate(ID = round(x/Lag_interval)*Lag_interval) %>%
                dplyr::group_by(ID) %>%
                dplyr::summarise(SD = sd(y, na.rm = T), x = mean(x, na.rm = T), Count = n(), y = mean(y, na.rm = T), !!covariates := mean(!!covariates, na.rm = T)) %>%
                dplyr::mutate(SumWI = sum(SD^2, na.rm = T)) %>%
                dplyr::mutate(wi = SumWI/SD^2)  %>%
                dplyr::select(-c(ID, SumWI))
        } else {
            # creating index, group by and take the mean of x and y
            # https://stackoverflow.com/questions/26724124/standard-evaluation-in-dplyr-summarise-on-variable-given-as-a-character-string
            DataXY <- DataXY %>%
                dplyr::mutate(ID = round(x/Lag_interval)*Lag_interval) %>%
                dplyr::group_by(ID) %>%
                dplyr::summarise(SD = sd(y, na.rm = T), x = mean(x, na.rm = T), Count = n(), y = mean(y, na.rm = T)) %>%
                dplyr::mutate(SumWI = sum(SD^2, na.rm = T)) %>%
                dplyr::mutate(wi = SumWI/SD^2)  %>%
                dplyr::select(-c(ID, Count, SumWI))
        }
    }
    # removing NA of any variables in DATAXY
    DataXY <- DataXY %>%
        dplyr::filter(complete.cases(DataXY))
    # Add ", " at the end of Sensor_name to be print with the legend
    if (!is.null(Sensor_name)) {
        Sensor_name <- paste0(Sensor_name, ", ")
    }
    if (Mod_type == 'GAM_GAUSS') {
        Model <- gam(y ~ s(x), family = gaussian(link = identity), data = DataXY)
    } else if (Mod_type == 'Linear') {
        # Linear Model, if s_y is not null calculate weights wi and use them in the regression
        if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
            Model <- lm(y ~ x, data = DataXY, model = TRUE, x = TRUE, y = TRUE)
        } else {
            Model <- lm(y ~ x, data = DataXY, model = TRUE, x = TRUE, y = TRUE, weights = wi)}
        # plotting calibrationn lines without plotting axis
        par(new = TRUE)
        plot(DataXY$x, Model$fitted.values, col = Couleur, xlim = lim[,1], ylim = lim[,2], type = "l",xaxt = "n", yaxt = "n" , xlab = "", ylab = "")
        if (Weighted) {
            points(DataXY$x, DataXY$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], xaxt = "n", yaxt = "n" , xlab = "", ylab = "")
            par(new = TRUE)
            arrows(DataXY$x, DataXY$y - DataXY$SD, DataXY$x, DataXY$y + DataXY$SD, length = 0.05, angle = 90, code = 3, col = Couleur)}
        # display equations and R^2
        Equation <- sprintf(paste0(Sensor_name, "Linear: y= ", f_coef1,"+ ", f_coef2," x",", R2=", f_R2,", RMSE=", f_coef1,",AIC= %.1f"), # ", s(Res)=",f_coef1,
                            coef(Model)[1],
                            coef(Model)[2],
                            summary(Model)$r.squared,
                            sqrt(sum(resid(Model)^2)/(length(resid(Model)) - 2)),
                            AIC(Model))
    } else if (Mod_type == 'Linear.Robust') {
        # MGV Robust Linear Model, (if s_y is not null calculate weights wi and use them in the regression
        # This models the median of y as a function of x, rather than modelling the mean of y as a function of x, in the case of least squares regression.
        if ("quantreg" %in% rownames(installed.packages()) == FALSE) {install.packages("quantreg")}
        library("quantreg")
        if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
            Model <- rq(y ~ x, data = DataXY, tau = 0.5, model = TRUE)
        } else {
            Model <- rq(y ~ x, data = DataXY, weights = wi, tau = 0.5, model = TRUE)}
        # plotting calibration lines without plotting axis
        par(new = TRUE)
        plot(DataXY$x,Model$fitted.values, col = Couleur, xlim = lim[,1], ylim = lim[,2], type = "l",xaxt = "n", yaxt = "n" , xlab = "", ylab = "")
        # display equations and R^2
        par(bg = "blue")
        Equation <- sprintf(paste0("Quantile regression tau 0.5: y= ",f_coef1,"+ ",f_coef2," x", ", RMSE=",f_coef1),
                            coef(Model)[1],
                            coef(Model)[2],
                            sqrt(sum(resid(Model)^2)/(length(resid(Model)) - 2)))
    } else if (Mod_type == 'gam') {
        Estimated <- data.frame(x = x, y = y)
        if (is.null(s_y) || any(s_y == 0) || all(is.na(s_y))) {
            # Let gam89 decide for k and estimate the best alpha
            Model <- gam(y~s(x), family = Gamma(link = log), data = Estimated)
            #Model <- gam(y~s(x, k=5), family=Gamma(link=log), data = Estimated)
        } else {
            # Let gam89 decide for k and estimate the best alpha
            #Model <- gam(y~s(x, k=5), family=Gamma(link=log), weights = wi)
            Model <- gam(y~s(x), family = Gamma(link = log), data = Estimated, weights = wi)}
        # plotting calibration lines without plotting axis
        Estimated <- Estimated.y(DataXY$x, Model)
        Estimated <- Estimated[order(Estimated$x),]
        if (Plot_Line) {
            par(new = TRUE)
            #plot(x,Model$fitted.values, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt = "n", yaxt = "n" , xlab= "", ylab = "")
            plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type = "l",xaxt = "n", yaxt = "n" , xlab = "", ylab = "")}
        # display equations and R^2
        par(bg = "blue")
        Equation <- sprintf(paste0("General Additive model"))
    } else if (Mod_type == 'exp_kT' | Mod_type == 'exp_kK') {
        # Setting Initial values
        Linear.Model <- lm(y ~ x, data = DataXY)
        A0 <- coef(Linear.Model)[1]
        A1 <- coef(Linear.Model)[2]
        # initial values see https://stats.stackexchange.com/questions/160552/why-is-nls-giving-me-singular-gradient-matrix-at-initial-parameter-estimates
        # Values for which log(DataXY$y - (A0 + A1 * DataXY$x)) can be calculated
        Positives <- which(DataXY$y - (A0 + A1 * DataXY$x) > 0)
        if (Mod_type == 'exp_kT') {
            # Model in celsius degrees
            DataXY$Temperature_inv <- DataXY$Temperature^-1
            Model.0 <- lm( log(y[Positives] - (A0 + A1 * x[Positives])) ~ Temperature[Positives], data = DataXY)
            C0 <- coef(Model.0)[1]
            k0 <- coef(Model.0)[2]
            # Fitting model
            if (is.null(s_y ) || any(s_y == 0) || all(is.na(s_y))) {
                Model <- nlsLM(y ~ f_exp_kT(x, a0, a1, C, k, Temperature),
                               data = DataXY,
                               start = list(a0 = A0, a1 = A1, C = C0, k = k0),
                               model = TRUE,
                               control = nls.lm.control(maxiter = 1024, maxfev = 10000)) # , lower = c(0, 0.0000000001, 0, 0.00000000001)
                #Model <- nlsLM(y ~ f_exp_kT(x, a1, C, k, Temperature), data = DataXY, start = list(a1 = A1/A0, C = A0, k = 0.12), model = TRUE, lower = c(0.0000000001/A0, 0/A0, 0.00000000001/A0))
            } else Model <- nlsLM(y ~ f_exp_kT(x,a0,a1,C,k,Temperature, n),
                                  data = DataXY, start = list(a0 = A0, a1 = A1, C = A0, k = 0.12, n = 1),
                                  weights = wi,
                                  model = TRUE,
                                  lower = c(0, 0.0000000001, 0, 0.00000000001),
                                  control = nls.lm.control(maxiter = 1024, maxfev = 10000)) # , lower = c(0, 0.0000000001, 0, 0.00000000001))
            # display equations and R^2
            Equation <- sprintf(paste0(Sensor_name, "Power: y = ",f_coef1,"+ ",f_coef2," x + exp(",f_coef2," T_Celsius + ", f_coef2,"), RMSE=",f_coef1,",AIC= %.1f"),
                                coef(Model)[1],coef(Model)[2],coef(Model)[3],coef(Model)[4],
                                sqrt(sum(resid(Model)^2)/(length(resid(Model)) - 2)),
                                AIC(Model))
        } else  if (Mod_type == 'exp_kK') {
            # Model in Kelvin
            DataXY$Kelvin     <- (273.15 + DataXY$Temperature)
            DataXY$Kelvin_inv <- DataXY$Kelvin^-1
            Model.0 <- lm( log(y[Positives] - (A0 + A1 * x[Positives])) ~ Kelvin[Positives], data = DataXY)
            C0 <- coef(Model.0)[1]
            k0 <- coef(Model.0)[2]
            if (is.null(s_y ) || any(s_y == 0) || all(is.na(s_y))) {
                Model <- nlsLM(y ~ f_exp_kT(x, a0, a1, C, k, Kelvin),
                               data = DataXY,
                               start = list(a0 = A0, a1 = A1, C = C0, k = k0),
                               model = TRUE,
                               control = nls.lm.control(maxiter = 1024, maxfev = 10000)) # , lower = c(0, 0.0000000001, 0, 0.00000000001)
            } else Model <- nlsLM(y ~ y ~ f_exp_kT(x, a0, a1, C, k, Kelvin),
                                  data = DataXY, start = list(a0 = A0, a1 = A1, C = C0, k = k0),
                                  weights = wi,
                                  model = TRUE,
                                  lower = c(0, 0.0000000001, 0, 0.00000000001),
                                  control = nls.lm.control(maxiter = 1024, maxfev = 10000)) # , lower = c(0, 0.0000000001, 0, 0.00000000001))
            # display equations and R^2
            Equation <- sprintf(paste0(Sensor_name, "Power: y = ",f_coef1,"+ ",f_coef2," x + exp(",f_coef2," T_Kelvin + ", f_coef2,"), RMSE=",f_coef1,",AIC= %.1f"),
                                coef(Model)[1],coef(Model)[2],coef(Model)[3],coef(Model)[4],
                                sqrt(sum(resid(Model)^2)/(length(resid(Model)) - 2)),
                                AIC(Model))
        }
    } else if (any(Mod_type %in% c('T_power', 'K_power'))) {
        # Setting Initial values
        #Linear.Model <- lm(y ~ x, data = DataXY)
        Linear.Model <- rq(y ~ x, data = DataXY, tau = c(0.5))
        A0 <- coef(Linear.Model)[1]
        A1 <- coef(Linear.Model)[2]
        # Transforming Celsius in Kelvin if needed
        if (Mod_type == 'K_power') DataXY[, "Temperature"] <- 273.15 + DataXY[, "Temperature"]
        # index of row with max temperature
        index.mdT <- which(DataXY$Temperature == max(DataXY$Temperature, na.rm = TRUE))[1]
        A2 <- (DataXY[index.mdT, "y"] - (A0 + A1 * DataXY[index.mdT, "x"])) / (DataXY[index.mdT, "Temperature"])^1.75
        # Fitting model
        if (is.null(s_y ) || any(s_y == 0) || all(is.na(s_y))) {
            Model <- nlsLM(y ~ f_T_power(x, a0, a1, a2, n, Temperature), data = DataXY,
                           start = list(a0 = A0, a1 = A1, a2 = A2, n = 1.75),
                           model = TRUE, control = nls.lm.control(maxiter = 1024, maxfev = 10000),
                           lower = c(-Inf,-Inf,-Inf, 0.1), upper = c(+Inf, +Inf, +Inf, 5))
        } else Model <- nlsLM(y ~ f_T_power(x, a0, a1, a2, n, Temperature), data = DataXY,
                              start = list(a0 = A0, a1 = A1,  a2 = A2, n = 1.75),
                              weights = wi, model = TRUE, control = nls.lm.control(maxiter = 1024, maxfev = 10000),
                              lower = c(-Inf,-Inf,-Inf, 0.1), upper = c(+Inf, +Inf, +Inf, 5))
        # display equations and R^2
        if (Mod_type == 'T_power') {
            Equation <- paste0(Sensor_name, "Power: y = ", f_coef1," + ",f_coef2," x + ",f_coef2," T_Celsius^", f_coef2, ", RMSE=",f_coef1,", AIC= %.1f")
        } else if (Mod_type == 'K_power') Equation <- paste0(Sensor_name, "Power: y = ", f_coef1," + ",f_coef2," x + ",f_coef2," T_Kelvins^", f_coef2, ", RMSE=",f_coef1,", AIC= %.1f")
        Equation <- sprintf(Equation,
                            coef(Model)[1],coef(Model)[2],coef(Model)[3],coef(Model)[4],
                            sqrt(sum(resid(Model)^2)/(length(resid(Model)) - 2)), AIC(Model))
    } else if (Mod_type == 'MultiLinear') {
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
                    # Covariates whithout Intercept, Enables and not forced
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
                    } else {
                        if (Verbose) cat("[Cal_line] INFO, All covariates need fitting!\n")
                    }
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
        if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
            if (is.MultiFile) {
                if (!is.null(Degrees) && any("ExpGrowth" %in% Degrees)) {
                    # Setting starting values of coefficients
                    A0 = coef(lm(y ~ x, data = DataXY))[1]
                    A1 = coef(lm(y ~ x, data = DataXY))[2]
                    # select data to avoid undefined log (use >0) or negative log and hence negative k (use >1)
                    Positives <- which(DataXY$y - (A0 + A1 * DataXY$x) > 1)
                    Start.Value   <- list(a0 = A0,a1 = A1)
                    Lower.values  <- c(-Inf, -Inf)
                    Upper.values  <- c(Inf, Inf)
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
                        Upper.values  <- c(Upper.values,  Inf, Inf)
                    }
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
                            Upper.values  <- c(Upper.values,  Inf)
                        }
                    }
                    #fitting with Exponential growth
                    Model <- nlsLM(Formula.Covariates, data = DataXY,
                                   start = Start.Value,
                                   # start = list(a0 = A0, #coef(lm(y ~ x, data = DataXY))[1],
                                   #              a1 = A1, # coef(lm(y ~ x, data = DataXY))[2],
                                   #              C = C0,
                                   #              k = k0,
                                   #              aRelative_humidity1 = 0),
                                   lower = Lower.values, upper = Upper.values,
                                   control = nls.lm.control(maxiter = 1024, maxfev = 10000),
                                   model = TRUE, trace = F)
                } else Model <- lm(Formula.Covariates, data = DataXY, model = TRUE, x = TRUE, y = TRUE)
            } else Model <- lm(Formula.Covariates, data = DataXY, model = TRUE, x = TRUE, y = TRUE)
            Equation <- Formula.Covariates
        } else {
            if (is.MultiFile) {
                if (any("ExpGrowth" %in% Degrees)) {
                    #fitting with Exponential growth
                    nlsLM(Formula.Covariates, data = DataXY,
                          start = Start.Value,
                          # start = list(a0 = A0, #coef(lm(y ~ x, data = DataXY))[1],
                          #              a1 = A1, # coef(lm(y ~ x, data = DataXY))[2],
                          #              C = C0,
                          #              k = k0,
                          #              aRelative_humidity1 = 0),
                          lower = Lower.values, upper = Upper.values,
                          control = nls.lm.control(maxiter = 1024, maxfev = 10000),
                          model = TRUE, trace = F, weights = wi)
                } else Model <- lm(Formula.Covariates, data = DataXY, model = TRUE, x = TRUE, y = TRUE, weights = wi)
            } else Model <- lm(Formula.Covariates, data = DataXY, model = TRUE, x = TRUE, y = TRUE, weights = wi)
            Equation <- Formula.Covariates
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
        # plotting calibrationn lines without plotting axis
        Estimated <- Estimated.y(DataXY$x, Model)
        par(new = TRUE)#, mar=Margin)
        plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type = "l",xaxt = "n", yaxt = "n" , xlab = "", ylab = "")
        # display equations and R^2
        Equation <- sprintf(paste0(Sensor_name, "Quadr.: y= ",f_coef1,"+ ",f_coef2,"x+ ",f_coef2,"x2",", R2=",f_R2, ", RMSE=",f_coef1,",AIC= %.1f") # ", s(Res)=",f_coef1,
                            ,coef(Model)[1],
                            coef(Model)[2],
                            coef(Model)[3] ,
                            summary(Model)$r.squared,
                            #sd(resid(Model)),
                            sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),
                            AIC(Model))
    } else if (Mod_type == 'Cubic') {
        if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
            Model <- lm(y ~ poly(x, 3, raw =TRUE), data = DataXY, model = TRUE, x = TRUE, y = TRUE)
        } else {
            Model <- lm(y ~ poly(x, 3, raw=TRUE) , data = DataXY, weights = wi, model = TRUE, x = TRUE, y = TRUE)}
        # plotting calibrationn lines without plotting axis
        Estimated <- Estimated.y(DataXY$x, Model)
        par(new = TRUE)#, mar=Margin)
        plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l", xaxt = "n", yaxt = "n" , xlab= "", ylab = "")
        # display equations and R^2
        Equation <- sprintf(paste0(Sensor_name, "Cubic: y= ",f_coef1,"+",f_coef2,"x+",f_coef2,"x2+",f_coef2,"x3",",R2=",f_R2,",RMSE=", f_coef1,",AIC= %.1f"),
                            coef(Model)[1],
                            coef(Model)[2],
                            coef(Model)[3],
                            coef(Model)[4],
                            summary(Model)$r.squared,
                            sqrt(sum(resid(Model)^2) / (length(resid(Model)) - 2)),
                            AIC(Model))
    } else if (Mod_type == 'ExpDecayInc') {
        if (is.null(s_y ) || any(s_y == 0) || all(is.na(s_y))) {
            Model <- nlsLM(y~f_ExpDI(x,C,k), data = DataXY, start = list(C = max(y), k = 0.05), model = TRUE)
        } else {
            Model <- nlsLM(y~f_ExpDI(x,C,k), data = DataXY, start = list(C = max(y), k = 0.05), weights = wi, model = TRUE)}
        # plotting calibrationn lines without plotting axis
        Estimated <- Estimated.y(DataXY$x, Model)
        par(new = TRUE)#, mar=Margin)
        plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l", xaxt = "n", yaxt = "n" , xlab= "", ylab = "")
        # display equations and R^2
        Equation <- sprintf(paste0(Sensor_name, "Exp. decay inc.: ys = ",f_coef1,"(1-exp(-",f_coef2,"x))", ",RMSE=",f_coef1,",AIC= %.1f"),
                            coef(Model)[1],
                            coef(Model)[2],
                            sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),
                            AIC(Model))
    } else if (Mod_type == 'ExpDecayInc_Int') {
        if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
            Model <- nlsLM(y ~ f_ExpDI_Int(x, C, k,intercept), data = DataXY, start = list(C = max(y, na.rm = T), k = 0.05, intercept = min(y, na.rm = T)), model = TRUE)
        } else {
            Model <- nlsLM(y ~ f_ExpDI_Int(x, C, k,intercept), data = DataXY, start = list(C = max(y, na.rm = T), k = 0.05, intercept = min(y, na.rm = T)), weights = wi, model = TRUE)}
        # plotting calibrationn lines without plotting axis
        Estimated <- Estimated.y(DataXY$x, Model)
        par(new = TRUE)#, mar=Margin)
        plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type = "l",xaxt = "n", yaxt = "n" , xlab = "", ylab = "")
        # display equations and R^2
        Equation <- sprintf(paste0(Sensor_name, "Exp. decay inc.: ys = ",f_coef2,"(1-exp(-",f_coef2,"x))+",f_coef1,",RMSE=", f_coef1,",AIC= %.1f"),
                            coef(Model)[1],
                            coef(Model)[2],
                            coef(Model)[3],
                            sqrt(sum(resid(Model)^2)/(length(resid(Model)) - 2)),
                            AIC(Model))
    } else if (Mod_type == 'ExpDecayDec_Int') {
        if (is.null(s_y) || any(s_y == 0) || all(is.na(s_y))) {
            Model <- nls(y~f_ExpDD_Int(x, C, k,intercept), data = DataXY, start=list(C = max(y, na.rm = T), k = 0.05, intercept = min(y, na.rm = T)), model = TRUE)
        } else {
            Model <- nls(y~f_ExpDD_Int(x, C, k,intercept), data = DataXY, start=list(C = max(y, na.rm = T), k = 0.05, intercept = min(y, na.rm = T)), weights = wi, model = TRUE)}
        # plotting calibrationn lines without plotting axis
        Estimated <- Estimated.y(DataXY$x, Model)
        par(new = TRUE)#, mar=Margin)
        plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type = "l",xaxt = "n", yaxt = "n" , xlab = "", ylab = "")
        # display equations and R^2
        Equation <- sprintf(paste0(Sensor_name, "Exp. decay dec: y = ",f_coef2,"exp(-",f_coef2,"x))+",f_coef1,",RMSE=", f_coef1,",AIC= %.1f"),
                            coef(Model)[1],
                            coef(Model)[2],
                            coef(Model)[3],
                            sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),
                            AIC(Model))
    } else if (Mod_type == 'Michelis') {
        if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
            Model <- nlsLM(y ~ MIN + f_Michelis(x, Vmax, km, MIN), data=DataXY, start=list(Vmax=max(y, na.rm = T), km=mean(y)/4, MIN = min(y, na.rm = T)), model = TRUE)
        } else {
            Model <- nlsLM(y ~ f_Michelis(x, Vmax, km, MIN), data=DataXY, start=list(Vmax=max(y, na.rm = T), km=mean(y)/4, MIN = min(y, na.rm = T)), weights = wi, model = TRUE)}
        # plotting calibrationn lines without plotting axis
        Estimated <- Estimated.y(DataXY$x, Model)
        par(new = TRUE)#, mar=Margin)
        plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type = "l",xaxt = "n", yaxt = "n" , xlab = "", ylab = "")
        # display equations and R^2
        Equation <- sprintf(paste0(Sensor_name, "Michelis: y = ",f_coef2,"/(",f_coef2,"+x)+",f_coef1,",RMSE=",f_coef1,",AIC= %.1f"), # ", s(Res)=", f_coef1,
                            coef(Model)[1],
                            coef(Model)[2],
                            coef(Model)[3],
                            sqrt(sum(resid(Model)^2)/(length(resid(Model)) - 2)),
                            AIC(Model))
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
                            coef(Model)[1],
                            coef(Model)[2],
                            #sd(resid(Model)),
                            sqrt(sum(resid(Model)^2)/(length(resid(Model)) - 2)),
                            AIC(Model))
    } else if (Mod_type == 'Sigmoid') {
        #nls.control(maxiter = 500, tol = 1e-05, minFactor = 1/4096, printEval = TRUE, warnOnly = TRUE)
        if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
            # Model <- nls(y~f_Sigmoid(x, MIN, MAX, X50, Hill), data=DataXY, start=list(MIN=min(y),MAX=max(y),X50=mean(x), Hill=3)
            #               , control = list(maxiter = 500, tol=1e-2, minFactor = 1/1024, printEval = TRUE, warnOnly=FALSE), trace=TRUE)
            Model <- nlsLM(DataXY$y ~ f_Sigmoid(x, MIN, Asym, xmid, Hill), data = DataXY, start = list(MIN = min(y, na.rm = T), Asym = max(y), xmid = mean(x, na.rm = T), Hill = 3)
                           , control = list(maxiter = 500, tol = 1e-2, minFactor = 1/1024, printEval = TRUE, warnOnly = FALSE), trace = FALSE, model = TRUE)
        } else {
            Model <- nlsLM(y ~ f_Sigmoid(x, MIN, Asym, xmid, Hill), data = DataXY, start=list(MIN=min(y, na.rm = T),Asym=max(y),xmid=mean(x, na.rm = T), Hill = 3), weights = wi
                           , control = list(maxiter = 500, tol = 1e-82, minFactor = 1/1024, printEval = TRUE, warnOnly = FALSE), trace = FALSE, model = TRUE)
            #Model <- nlsLM(y ~ MIN + SSlogis(x, Asym, xmid, scal) , data=DataXY, start=list(MIN=min(y),Asym=max(y),xmid=mean(x), scal = 3), weights = wi ,
            #               control = list(maxiter = 500, tol=1e-2, minFactor = 1/(1024*32), printEval = TRUE, warnOnly=FALSE), trace=TRUE, model = TRUE)
        }
        # plotting calibrationn lines without plotting axis
        Estimated <- Estimated.y(DataXY$x, Model)
        par(new = TRUE)#, mar=Margin)
        plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt = "n", yaxt = "n" , xlab = "", ylab = "")
        # display equations and R^2
        Equation <- sprintf(paste0(Sensor_name, "Sigmoid: y=",f_coef1,"+",f_coef2,"/(1+(",f_coef2,"/x)^",f_coef2,"), ", ",RMSE=",f_coef1,",AIC= %.1f"), # "s(Res)=", f_coef1,
                            coef(Model)[1],
                            coef(Model)[2] - coef(Model)[1],
                            coef(Model)[3],
                            coef(Model)[4],
                            sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),
                            AIC(Model))
    } else if (Mod_type == 'Unitec') {
        #Put O3, sensor reponses and standard deviation of sensor responses in a matrix: Not done for now
        a=-31.6
        b=5330.9
        c=-0.598
        DataXY <- data.frame(cbind(x*2.05, ((y-a)/b)^(1/c), s_y))
        colnames(DataXY) <- c("x","y")
        if (is.null(DataXY$wi) || any(DataXY$wi == 0) || all(is.na(DataXY$wi))) {
            Model <- nls(y~f_Unitec(x, a, b,c), data=DataXY, start=list(a=-31.6,b=5330.9,-0.598), model = TRUE, x = TRUE, y = TRUE)
        } else {
            Model <- nls(y~f_Unitec(x,a,b,c),data=DataXY, start=list(a=-31.6,b=5330.9,-0.598), weights = wi, model = TRUE, x = TRUE, y = TRUEi)
        }
        # plotting calibrationn lines without plotting axis
        Estimated <- Estimated.y(DataXY$x, Model)
        par(new = TRUE)#, mar=Margin)
        plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt = "n", yaxt = "n" , xlab= "", ylab = "")
        # display equations and R^2
        Equation <- sprintf(paste0(Sensor_name, "Unitec: y = ((1.91x(293.15/T)-",f_coef1,")/",f_coef2,")^(1/",f_coef21,"), ", ",RMSE=",f_coef1,",AIC= %.1f"),  # "s(Res)=", f_coef1,
                            coef(Model)[1],
                            coef(Model)[2],
                            coef(Model)[3],
                            coef(Model)[4],
                            sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),
                            AIC(Model))
    } else if (Mod_type == 'PM_Normal_Dist') {
        # Setting Initial values
        #MU <- log(dimatro) che corrisponde al max dei counts
        #SIGMA <- coef(Linear.Model)[2]
        # Fitting model
        if (is.null(s_y ) || any(s_y == 0) || all(is.na(s_y))) {
            Model <- nlsLM(y ~ f_Normal(x, mu, sigma), data = DataXY, start = list(mu = MU, sigma = SIGMA), model = TRUE)
        } else Model <- nlsLM(y ~ f_Normal(x, mu, sigma), data = DataXY, start = list(mu = MU, sigma = SIGMA), model = TRUE, weights = wi)
    } else if (Mod_type == 'bi_Normal') {
        Model <- nlsLM(y ~  K1* dnorm(x, mu1, sigma1) + K2* dnorm(x, mu2, sigma2) + C ,
                       data    = DataXY,
                       start   = list(mu1 = MU1, sigma1 = 0.2, K1 = K1, mu2 = MU2, K2 = K2, sigma2 = 0.2, C = C),
                       model   = TRUE, trace = T)
    } else if (Verbose) cat("[Cal_Line] unknown calibration model\n")
    # Adding printing summary of Model
    if (Verbose) print(summary(Model))
    # Adding equation text
    if (!is.null(Equation)) mtext(Equation, line = line_position, adj = 1, padj = 0, col = Couleur, cex = 0.875)
    # resuming the par values
    if(Plot_Line) on.exit(par(op))
    # Adding the equation to the model
    if (!is.null(Equation)) Model$Equation <- Equation
    return(Model)
}
#================================================================CR
### Function Measurement Function x = f(y) once Calibration function (y = f(x) of sensor is established e.g with Cal_Line
#================================================================CR
Meas_Function <- function(y, Mod_type, Model, covariates = NULL, Degrees = NULL, Matrice = NULL) {
    # This function estimates the x value using a calibration model (Model)
    # y            : Sensor data to be converted to concentration level using the reverse calibration function (Model)
    # Mod_type     : type of calibration function: Linear, Quadratic, Sigmoid
    # Model        : the calibration function
    # covariates   : vectors of strings representing the covariates when model needs covariates
    # degrees      : vector of string, degrees of the covariates for multiLinear calibration
    # Matrice      : Covariates values, data.frame, default is NULL
    if (Mod_type == 'Linear' || Mod_type == 'Linear.Robust') {
        return((y - Model$Coef[1])/Model$Coef[2])
    } else if (Mod_type == 'MultiLinear') {
        # convert any column of Matrice that is not numeric (Date) to numeric
        if (!all(grepl(pattern = "numeric", x = sapply(lapply(Matrice, class), "[", 1)))) {
            cat("[Meas_Function] INFO, some covariates are not numeric. Converting to numeric.\n")
            Col.no.numeric <- grep(pattern = "numeric", x = sapply(lapply(Matrice, class), "[", 1), invert = TRUE)
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
    } else if (Mod_type == 'Exponential') {
        return(-log(1 - (y - Model$Coef[3]) / Model$Coef[1]) / Model$Coef[2])
    } else if (Mod_type == 'ExpDecayInc_Int') {
        if (any(!is.na(y) & y < Model$Coef[3] ) | any(!is.na(y) & y > Model$Coef[1] + Model$Coef[3])) { # in case value out of bound and value is not NA
            print("Some Y value out of limits of the model", quote = FALSE)
        }
        x <- log(1 - ((y - Model$Coef[3])/Model$Coef[1]))/-Model$Coef[2]
        #  yy <- Model$Coef[1] * (1-exp(-Model$Coef[2] * xx)) + Model$Coef[3]
        return(x)
    } else if (Mod_type == 'exp_kT') {
        # model f_exp_kT: return( (y - (a0 + C.exp(k * Temperature)))/ a1) )
        Estimated <- as.vector( (y - (Model$Coef[1] + exp(Model$Coef[4] * Matrice$Temperature +  Model$Coef[3])) ) / Model$Coef[2])
        return(Estimated)
    } else if (Mod_type == 'exp_kK') {
        # model f_exp_kT: return( (y - (a0 + C.exp(k * Temperature)))/ a1) )
        Estimated <- as.vector((y - (Model$Coef[1] + exp(Model$Coef[4] * (273.15 + Matrice$Temperature) +  Model$Coef[3])) )/Model$Coef[2])
        return(Estimated)
    } else if (Mod_type == 'T_power') {
        # model T_power: return( (y - (a0 + a2 T^n))/ a1)
        return(as.vector((y - (Model$Coef[1] + Model$Coef[3] * Matrice$Temperature^Model$Coef[4])) / Model$Coef[2]))
    } else if (Mod_type == 'K_power') {
        # model T_power: return( (y - (a0 + a2 T^n))/ a1)
        return(as.vector((y - (Model$Coef[1] + Model$Coef[3] * (273.15 + Matrice$Temperature)^Model$Coef[4])) / Model$Coef[2]))
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
### MeasLinear: Function Measurement Function x = f(y) once the Linear Calibration function (y = f(x) of sensor is established e.g with Cal_Line
#================================================================CR
MeasLinear <- function(x, ModLinear) {
    (x-coef(ModLinear)[1])/coef(ModLinear)[2]
}
#================================================================CR
### MeasParab: Function Measurement Function x = f(y) once the Quadratic Calibration function (y = f(x) of sensor is established e.g with Cal_Line
#================================================================CR
MeasParab <- function(x, ModParab) {
    if (coef(ModParab)[2] > 0) {
        return((-coef(ModParab)[2]+sqrt(coef(ModParab)[2]^2-4*coef(ModParab)[3]*(coef(ModParab)[1]-x)))/(2*coef(ModParab)[3]))
    } else {
        return((-coef(ModParab)[2]-sqrt(coef(ModParab)[2]^2-4*coef(ModParab)[3]*(coef(ModParab)[1]-x)))/(2*coef(ModParab)[3]))
    }
}
#================================================================CR
### MeasSigmoid: Function Measurement Function x = f(y) once the Sigmoidal Calibration function (y = f(x) of sensor is established e.g with Cal_Line
#================================================================CR
MeasSigmoid <- function(x, ModSigmoid) {
    (log((coef(ModSigmoid)[1]-coef(ModSigmoid)[4])/(x-coef(ModSigmoid)[4])-1)/(-coef(ModSigmoid)[2])+coef(ModSigmoid)[3])
}
#================================================================CR
### Adjust: Function Measurement Function using reverse calibration
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
### Function Significant numbers
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
### Function to fit the best polynomial fitting (Vs > 151016)
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
### Function to calibrate from lab (Vs > 151016)
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
### Tables for Rmarkdown reporting(Vs 170428)
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
### Functions for fiting distributions
#================================================================CR
#================================================================CR
# Function probability distribution funtion for Log-Normal distribution 1 parameter
#================================================================CR
f_lnormal_1par <- function(epsilon, lambda, q, m) {
    # epsilon: vector data for wwhich the log-normal distribution is fitted
    # lambda: vector shape parameter
    # q: lower limiting value
    # m: median
    return(
        1/(sqrt(2*pi)*lambda*abs(epsilon-q)) * exp(-(log((epsilon-q)/(m-q)))^2/(2*lambda^2))
    )
}
#================================================================CR
# Function probability distribution funtion for Log-Normal distribution 2 parameters
#================================================================CR
f_lnorm_2Par <- function(x, mu, sigma) {
    # x: vector data for wwhich the log-normal distribution is calculated
    # mu: mean of the log of x
    # x: standard devaition of the log of x)
    # m: median
    return(1/(sqrt(2* pi)*sigma*x) * exp(-(log(x)-mu)^2/(2*sigma^2)))
}
#================================================================CR
# Function returning mode of a distribution
#================================================================CR
Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}
#================================================================CR
# Fitting distribution to data
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
### Fitting the distribution to data
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
    return(fitted_Distribution)
}
#================================================================CR
### change names of files
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
### Function checking if a value is within a range
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
### Function loading an RData file and returning it into a variable
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
#================================================================CR
### 170609 MG : ASEPanel04File Looking for the sensor config file --> 180118 transfered to Function4ASE.R
#================================================================CR
#================================================================CR
### 170609 MG : ASEPanel04Read Reading the sensor config file     --> 180118 transfered to Function4ASE.R
#================================================================CR
# transform NaN into Na
nan.to.na <- function(x) {x[which(is.nan(x))] <- NA; return(x)}
#================================================================CR
# Checking if there are coordinates for the reference data separated by a comma and project if needed
#================================================================CR
get_Coord.Ref  <- function(Coordinates.chr, ShinyUpdate = False, session = NULL, ID.Long = NULL, ID.Lat = NULL) { # feed back of coordinates into the ui
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
            updateTextInput(session = session,
                            inputId = ID.Long,
                            value   = Ref.coord_LON)
            updateTextInput(session = session,
                            inputId = ID.Lat,
                            value   = Ref.coord_LAT)
        } else {
            Ref.coord_LON <- as.numeric(strsplit(x = Coordinates.chr, split = ",")[[1]][1])
            Ref.coord_LAT <- as.numeric(strsplit(x = Coordinates.chr, split = ",")[[1]][2])
        }
        # update Shiny Select input s
        return(paste0(Ref.coord_LON, ",", Ref.coord_LAT))
    } else {
        cat("[get_Coord.Ref] ERROR Coordinates are not comma separated")
        return(paste0(NA,NA))
    }
}
#================================================================CR
# Aggreation of time series in dataframe
#================================================================CR
DF_avg <- function(DF, Cols.for.Avg = NULL, width = 60, keyDate = "date", SameClass = NULL) {
    # Use data.table to average a data frame
    # DF            : dataframe to be aggregated, shall include the PosixCT column "date" (default), or another name of POSIXct column on which to aggregate
    # Cols.for.Avg  : vector of charaters, default NULL. If not NULL the returned columns Cols.for.Avg + "date" is returned
    # width         : numeric, default is 60. Size in minutes of the window used for averaging
    # keyDate       : chr, defalt "date, name of the column of DF which hold the POSIXct on which
    #
    # return        : aggregated data.table starting on the first full hour discarding empty rows and transforming nan to na
    # Previous code used timeAverage and later RcppRoll::roll_mean whcih but needed to complete dataframes with all date.
    # We use now data.table which is faster
    # checking the size of window, at least two times the width
    if (exists("DF") && !is.null(DF) && nrow(DF) > width * 2) {
        # Select columns
        if (!is.null(Cols.for.Avg)) {
            Columns <- c(keyDate, Cols.for.Avg)
            DF <- DF[,Columns, with = FALSE]
        }
        if (is.data.table(DF)) {
            DF[, Agg := lubridate::ceiling_date(data.frame(DF)[, keyDate], unit = ifelse(width <= 60, paste0(width,"minute"), paste0(width/60, "hour")))]
            DF[, (keyDate) := NULL]
            data.table::setnames(DF, c("Agg"), keyDate)
            data.table::setkeyv(DF, keyDate)
        } else {
            if (is.data.frame(DF)) {
                # Add aggregated time
                DF$Agg <- lubridate::ceiling_date(DF[[keyDate]], unit = paste0(width,"minute"))
                # Rename Agg as date
                DF[[keyDate]] <- NULL
                names(DF) <- sub(pattern = "Agg", replacement = keyDate, x = names(DF))
                # convert to data time
                DF <- data.table::data.table(DF, key = keyDate)
            } else {
                cat("Unknow class of DF\n")
                return()
            }
        }
        # Aggregate mean of Aggreagted time
        # https://stackoverflow.com/questions/12603890/pass-column-name-in-data-table-using-variable
        date <- quote(get(keyDate))
        # https://stackoverflow.com/questions/26663053/getting-na-when-summarizing-by-columns-in-data-table
        DF.Agg.DT <- DF[, lapply(.SD, mean, na.rm = TRUE), by = eval(date)]
        DF.Agg.DT[, (keyDate) := date]
        #DF.Agg.DT[, date := NULL] # if keydate = "date", this will delete date?
        # replace nan with
        DF.Agg.DT <- DF.Agg.DT[, lapply(.SD, nan.to.na)]
        return(DF.Agg.DT)
        # # For RcppRoll::roll_mean: makes complete row of times series
        # DF <- DF %>%
        #     tidyr::complete(date = seq(round(min(date, na.rm = TRUE), units = "hours"),
        #                                round(max(date, na.rm = TRUE), units = "hours"), width)) %>%
        #     filter(date >= round(min(date, na.rm = TRUE), units = "hours"))
        #
        # # Aggregating using the mean with window size
        # DF.avg   <- DF
        # DF.avg[, -which(names(DF) == "date")] <- lapply(DF[, -which(names(DF) == "date")], function(x) RcppRoll::roll_mean(x , n = width, na.rm = TRUE, by = width, fill = NA, align = "left"))
        # #DF.avg$date <- DF$date
        # DF.avg   <- DF.avg[seq(1, nrow(DF.avg), width),]
        # DF.avg[] <- lapply(DF.avg, nan.to.na)
        # rm(DF)
        # # discarding rows with all NAs added to complete the minute time series
        # Index.DF.avg   <- which(sapply(seq_along(DF.avg$date), function(i) all(is.na(DF.avg[i, -which(names(DF.avg) == "date")]))))
        # if (length(Index.DF.avg) > 0 ) DF.avg <- DF.avg[-Index.DF.avg,]
        # return(data.frame(DF.avg))
    } else return(cat("[DF_avg] ERROR, the selected dataframe does not exist or is too short for averaging"))
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
