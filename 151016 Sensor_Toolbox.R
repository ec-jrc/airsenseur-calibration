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
# change 180125: slope_Orth, solve problem of egative uncertainties, set-up different small bugs stopping the code, improve grafical plots
#
#================================================================CR
### Load.Packages: Function Load.Packages (170420)
### resetPar: Function reset graphical parameters (Vs 141114)
### sink.reset: Function reset sink number (Vs 141114)
### stopWhenError: Function reset sink and device errors (Vs 141114)
### slope_orth: Function Orthogonal regression (Vs 141114)
### Etalonnage: Function View Scatter Plot of calibration function (Vs 170420)
### f_log: Function Fitting a logarithmic model (Vs 141114)
### f_Unitec: Function Fitting a the Unitec model (Vs 141114)
### f_ExpDI: Function Fitting an exponential Decay (Increasing form) model (Vs 141114)
### f_ExpDI_Int: Function Fitting an exponential Decay (Increasing form) model with intercept (see wikipedia) (Vs 141114)
### f_Michelis: Function Fitting a Michaelis-Menten kinetics model with intercept (Vs 141114)
### f_ExpDD_Int: Function Fitting an exponential Decay (Decreasing form) model with intercept (Vs 141114)
### f_Sigmoid: Function Fitting a Logistic - Sigmoidal function (Vs 141114)
### Estimated.y: Function Plot estimated function (Vs 141114)  
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
    for(i in list.Packages) {
        
        # Installing packages
        if(i %in% rownames(installed.packages()) == FALSE) {
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
### resetPar: Function reset graphical parameters (Vs 141114)
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
### sink.reset: Function reset sink number (Vs 141114)
#================================================================CR
sink.reset <- function(){
    for(i in seq_len(sink.number())){
        sink(NULL)
    }
}

#================================================================CR
### stopWhenError: Function reset sink and device errors (Vs 141114)
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
### slope_orth: Function Orthogonal regression (Vs 141114)
#================================================================CR
slope_orth <- function(Xlabel, Ylabel, Title, DQO.I = NA, LV = NA, Units = NULL, Disk = NA, WD = NA, Dir = NA, Mat, uxi = NULL, 
                       lim = NULL, Sensor_name = NULL, variable.uxi = FALSE, f_coef1 = NULL, f_coef2 = NULL, f_R2 = NULL, nameModel = NULL, SavePlot = TRUE) {
    # Xlabel, Ylabel : label On the x And y axis
    # Title          : title to appear On the top of the scatter plot of x And y values
    # DQO.I          : numeric, data qualtiy objective for the expanded uncertainty, same unit as Mat$yis, defaul NA. If NA no DQO.I, horizontal line is plotted
    # LV             : numeric, limit value for Mat$xis, same unit as Mat$xis, default value = NA, plot a vertical line at LV if not NA
    # Units          : character vector, units for the expanded uncertainty, Xis, Yis
    # Disk, WD, Dir  : where you put the graphic files (Disk, working directory, directory), It is sufficient if only Dir is supplied
    # lim            : passing variable for the limits of the Etalonnage function (cbind(c(minX,maxX),c(minY,maxY)) or NULL)
    # Sensor_name    : name of the sensor to be written in front of the calibration equation. If NULL, do not print sensor name.
    # Mat            : DataFrame of data including Case number, Date, x And y + optional uxi if uxi is not constant for all reference values
    # uxi            : numeric (default = NULL ), random standard uncertainty of the results of the reference method, xis, given as a constant value for all xis reference values
    # variable.uxi   : logical, if FALSE (default = FALSE ), uxi is used as constant random standard uncertainties for all xis reference values. 
    #                  If TRUE uxi given in Mat and is used for each reference values
    # f_coef1, f_coef2, f_R2: number of digit for intercept, slope and R2 using sprintf syntax. 
    #                         f_coef1 is used both for intercept and s(Res), f_coef2 is used for all parameters of the model apart from the intercept
    # nameModel      : name of model to be used to save uncertainty plots, character, default NULL
    # SavePlot       : logical, default is TRUE if TRUE uncertainty plts are saved
    # return a list with the orthogonal regression: 
    #                 "mo","sdo", "mm","sdm", "b1", "ub1", "b0", "ub0", "RSS","rmse", "mbe", "Correlation", "nb", "CRMSE", "NMSD", 
    #                 and "Mat": "case", "Date", "xis", "yis","uxi", "RS", "Ur", "U", "Rel.bias", "Rel.RSS"
    
    # 
    # Homogeneity of variance is tested For the calculation of RSS
    # adding Ur In a New field of Mat
    # returning a list with slope (b and ub), intercept (a and ua), the sum of square of residuals (RSS), 
    # the root means square of error (RMSE), the mean bias error (mbe), the coefficeint of correlation (Correlation), 
    # the number of valid measurment (nb), the centred rout mean square of error (CRMSE), the normalised mean standard deviation (NMSD)
    # and Mat with relative expanded uncertainty
    
    #checking that the mat dataFrame is not empty
    if(nrow(Mat)>0){
        
        # Creating the Directory to save plots
        Dir <- file.path(c(Disk, WD , Dir)[which(!sapply(list(c(Disk, WD , Dir)), is.na))])
        if(!dir.exists(Dir)) dir.create(Dir, showWarning = TRUE, recursive = TRUE)
        
        # saving the original par values in case they would be modified in this function
        op <- par(no.readonly = TRUE)
        # Passing and resuming the par values
        on.exit(par(op))
        par(mar=c(4,4,2,0.5))
        par(new=FALSE)
        
        # Setting coloumn names and adding uxi as constant values
        colnames(Mat) <- c("case", "Date", "xis", "yis","uxi")[1:length(colnames(Mat))]
        if(!variable.uxi) if(!is.null(uxi)) Mat$uxi <- rep(uxi, nrow(Mat)) else{ cat("[slope_orth] ERRROR, value of u(xi) not given. Stopping the function"); return(0) } 
        
        # Filtering for the validation data only
        Mat <- subset(Mat, !is.na(Mat$xis) & !is.na(Mat$yis))
        
        #Orthogonal regression (see annex b of equivalence method)
        nb     <- nrow(Mat)
        mo     <- mean(Mat$xis)
        mm     <- mean(Mat$yis)
        sdo    <- sd(Mat$xis)
        sdm    <- sd(Mat$yis)
        Sxx    <- sum((Mat$xis - mo)^2)
        Syy    <- sum((Mat$yis - mm)^2)
        Sxy    <- sum((Mat$xis - mo) * (Mat$yis - mm))
        b1     <- (Syy - Sxx + sqrt((Syy- Sxx)^2 + 4*Sxy^2))/(2*Sxy)
        b0     <- mm - b1 * mo
        ub1    <- sqrt((Syy - (Sxy^2/Sxx))/((nb-2)*Sxx))
        ub0    <- sqrt(ub1^2 * sum(Mat$xis^2)/nb)
        
        # Regression statistics for Target Diagram (see delta tool user guide)
        rmse  <- sqrt((sum((Mat$yis - (b0 + b1 * Mat$xis))^2))/nb)
        mbe   <- mean(Mat$yis - Mat$xis)
        mae   <- mean(abs(Mat$yis - Mat$xis))
        CRMSE <- sqrt(mean(((Mat$yis - mm) - (Mat$xis - mo))^2))
        NMSD  <- (sd(Mat$yis) - sd(Mat$xis)) / sd(Mat$xis)
        Correlation <- cor(Mat$xis,Mat$yis)
        
        #Plots scatterplot of orthogonal regression
        Gamme <- Etalonnage(x          = Mat$xis, 
                            s_x        = NULL, 
                            y          = Mat$yis, 
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
        par(new=TRUE)
        # Set same margin for the line and Etalonnage
        par(mar=c(Gamme[,"mar12"],Gamme[,"mar34"]))
        # Plotting orthogonal regression line
        plot(Mat$xis, b1 * Mat$xis + b0, 
             type = "l", 
             col  = "red",
             xlim = Gamme[,1], 
             ylim = Gamme[,2],
             axes = FALSE ,
             xlab = "",
             ylab =""
        )
        if(is.null(f_coef1)) f_coef1 <- "%.2f"
        if(is.null(f_coef2)) f_coef2 <- "%.2f"
        if(is.null(f_R2))    f_R2    <- "%.4f"
        mtext(sprintf(paste0(Sensor_name, ", y= ",f_coef1,"+ ",f_coef2," x",", R2=",f_R2,", RMSE=",f_coef1), # ", s(Res)=",f_coef1,
                      b0, 
                      b1, 
                      cor(Mat$xis,Mat$yis)^2, 
                      #sd(b1 * Mat$xis + b0 - Mat$yis), 
                      sqrt(sum((b1 * Mat$xis + b0 - Mat$yis)^2)/(length(b1 * Mat$xis + b0 - Mat$yis) - 2))
        ),
        line = 1,
        adj  = 1,
        padj = 0,
        col  = "red",
        cex  = 0.875
        )
        
        # Saving the plot of orthogonal regression
        dev.copy(png,filename = file.path(Dir,paste0(nameModel, "_Scatter.png")), units = "cm", res = 300, width = 22, height = 22);
        dev.off()
        
        # Plots square of absolute residuals versus xis
        # Squares of Residuals and bias (vector of values)
        Mat$RS   <- (Mat$yis - (b0 + b1 * Mat$xis))^2
        Mat$bias <- (b0 + (b1 - 1) * Mat$xis)
        # Sum of squares of Residuals (one constant value)
        RSS     <- sum(Mat$RS)
        
        if(!is.null(Units)) Ylab = paste0("Square of Residuals in (", Units,")^2") else Ylab = "Square of Residuals"
        gamme <- Etalonnage(x          = Mat$xis, 
                            s_x        = NULL, 
                            y          = Mat$RS, 
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
        
        # Checking if RS - Mat$uxi^2 < 0 that make an error using sqrt(RS - Mat$uxi^2) of the ree.RSS. Replacing with 0
        if(any((Mat$RS - Mat$uxi^2) <= 0)){
            
            cat(" Some \"Square or residuals - u(xi)^2\" are negative. \"sqrt(RSS/(n-2) - u(xi)^2\" cannot be calculated nor ux(i) can be > RSS.\n 
                You can decrease uxi, check in df Mat: x and max(uxi).\n
                For now, RSS/(nb-2) - Mat$uxi^2 is set to 0 when negative.\n")
            neg.RS <- which(RSS/(nb-2) - Mat$uxi^2 <= 0)
            Mat[neg.RS, "RS"] <- Mat[neg.RS, "uxi"]^2 + 0.001 * min(Mat$xis) # adding 0.1 % of min(xis) to avoid problem of 0 with gam fitting
            
            # Recalculating RSS when Mat$RS are changed
            RSS     <- sum(Mat$RS)
            
            # plotting an error
            # plot(1,1,col="white", xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 1)
            # text(1,1,"slope_orth, ERROR: random uncertainty of the reference method too big, some uncertainties are negative.")
            # dev.copy(png,filename = file.path(Dir,paste0(nameModel, "_U.png")), units = "cm", res = 300, width = 22, height = 22);
            # dev.off()
            
        }  else {
            
            # mat$RS are not changed and they are already calculated
            cat("All \"Squares of Residuals - u(xi)^2\" are positive. \"sqrt(RSS/(n-2) - u(xi)^2\" can be calculated, check in df Mat: x and max(uxi).\n")
            
        }
        
        # tesing significance of correlation between s and square of absolute reisduals - The calculation does not work only possibility the constrant RSS
        rtest <- cor.test(Mat$xis, Mat$RS)
        print(rtest, quote = FALSE)
        cat(sprintf("probability of H0 (r=0): %f, if <0.05, correlation is demonstrated, if > 0.95 there is no correlation",rtest$p.value), "\n")
        
        # if fitting the square of residuals is needed
        if(rtest$p.value < 0.05) { 
            
            cat("The residuals are not constant. RSS is calculated after applying a gam fitting .\n")
            
            #z <- lm((Mat$yis - (b0 + b1 * Mat$xis))^2 ~ Mat$xis)
            # Fitting with gam
            # if any y value is zero getting Warning: Error in eval: non-positive values not allowed for the 'gamma' family (we had 0.5 % of min(xis) to avoid this
            z <- gam( Mat$RS ~ s(Mat$xis), family=Gamma(link=log) )
            Mat$RS <- fitted(z) 
            
            # plotting the line of the regression of the square of residuals
            # Overlay new plot
            par(new=TRUE)
            # Set same margin for the line and Etalonnage
            par(mar=c(gamme[,"mar12"],gamme[,"mar34"]))
            # Plotting orthogonal regression line
            order.xis <- order(Mat$xis)
            plot(x    = Mat[order.xis, "xis"], 
                 y    = Mat[order.xis, "RS"], 
                 type = "l", 
                 col  = "red",
                 xlim = gamme[,"Xlim"], 
                 ylim = gamme[,"Ylim"],
                 axes = FALSE ,
                 xlab = "",
                 ylab =""
            )
            mtext(sprintf(paste0("The correlation is significant (p = %.3f), Fitting a Generalized additive model (k = 5)"), rtest$p.value),
                  line = 1,
                  adj  = 1,
                  padj = 0,
                  col  = "red",
                  cex  = 0.875
            )
            #lines (x = Mat$xis, y = Mat$RS, type = "l", col = "red")
            
            #### Calculating uncertainty
            Mat$Ur <- 2 * sqrt(Mat$RS - Mat$uxi^2 + Mat$bias^2)/Mat$xis * 100
            Mat$U  <- 2 * sqrt(Mat$RS - Mat$uxi^2 + Mat$bias^2)  
            
            #### Calculating parameters for modified Target diagram
            Mat$Rel.bias <- 2 * (b0/Mat$xis + (b1 - 1))
            Mat$Rel.RSS  <- 2 * (sqrt( Mat$RS - uxi^2) / Mat$xis)
            
            
        } else {
            
            cat("The residuals are constant. RSS is calculated with equation for constant residuals.\n")
            cat(sprintf("RSS is the square root of sum of squares of Residuals divided by n - 2: %f", sqrt(sum((Mat$yis/(b0+b1*Mat$xis)-1)^2))/nb^2), "\n")
            # No need to lot a fitted line in this case
            
            mtext(sprintf(paste0("The correlation between x and the squares of residuals is not significant p = %.3f, RSS = "), rtest$p.value),
                  line = 1,
                  adj  = 1,
                  padj = 0,
                  col  = "red",
                  cex  = 0.875
            )
            
            #### Calculating uncertainty
            Mat$Ur <- 2 * sqrt(RSS/(nb-2) - Mat$uxi^2 + Mat$bias^2)/Mat$xis * 100
            Mat$U  <- 2 * sqrt(RSS/(nb-2) - Mat$uxi^2 + Mat$bias^2)  
            
            #### Calculating parameters for modified Target diagram
            Mat$Rel.bias <- 2 * (b0/Mat$xis + (b1 - 1))
            Mat$Rel.RSS  <- 2 * (sqrt( RSS/(nb - 2) - uxi^2) / Mat$xis)
            
            
        }    
        
        # saving plots of squares of residuals with fitted model
        dev.copy(png,filename = file.path(Dir,paste0(nameModel, "_SqrRes.png")), units = "cm", res = 300, width = 22, height = 22);
        dev.off()
        
        #Plots Expanded Uncertainty
        if(!is.null(Units)) Ylab = paste0("Expanded uncertainty in ", Units) else Ylab="Expanded uncertainty"
        order.xis <- order(Mat$xis)
        plot(Mat[order.xis, "xis"], 
             Mat[order.xis, "U"], 
             xlab=Xlabel, 
             ylab = Ylab, 
             main=Title , 
             col='blue', 
             type = "l", 
             ylim= c(0, max(Mat$U, na.rm = T))
        )
        
        if(!is.na(LV)) {
            abline(v=LV)
            text(x = LV, 
                 y = 0 + 0.05 * (max(Mat[order.xis, "U"], na.rm = T) - 0), 
                 labels = "LV")  
        } 
        if(!is.na(DQO.I)) {
            abline(h=DQO.I) # in ppb
            text(x = min(Mat[order.xis, "xis"], na.rm = T) + 0.05 * (max(Mat[order.xis, "xis"], na.rm = T) - min(Mat[order.xis, "xis"], na.rm = T)), 
                 y = DQO.I, 
                 labels = "DQO.I")  
        } 
        # UAT = 0.35 /1.91 * 78/46
        # LAT = 0.25 /1.91 * 78/46
        # lines(Mat[Mat$xis> LAT, "xis"], Mat[Mat$xis> LAT, "xis"]*DQO.I, col="black", lwd=3)
        # lines(Mat[Mat$xis< LAT, "xis"], rep(LAT*DQO.I, length.out = length(Mat[Mat$xis< LAT, "xis"])), col="black", lwd=3)
        # text(5, LAT*DQO.I+2, "Class 1")
        # DQO.I = 0.75
        # lines(Mat[Mat$xis> UAT, "xis"], Mat[Mat$xis> UAT, "xis"]*DQO.I, col="black")
        # lines(Mat[Mat$xis< UAT, "xis"], rep(UAT*DQO.I, length.out = length(Mat[Mat$xis< UAT, "xis"])), col="black")
        # text(5, UAT*DQO.I+2, "Class 2")
        grid(nx = NULL, ny = NULL, lty = 2, col = "grey")
        dev.copy(png,filename = file.path(Dir,paste0(nameModel,"_U.png")), units = "cm", res = 300, width = 22, height = 22);
        dev.off()
        
        Mat$Max.uxi <- sqrt(RSS/(nb-2) + Mat$bias^2)
        Mat$Max.RSD <- sqrt(RSS/(nb-2) + Mat$bias^2)/Mat$xis
        
        # Printing
        cat("\n")
        cat("--------------------------------\n")
        cat(sprintf("mean of x   : %.1g +/- %.1g",mo,sdo),"\n")
        cat(sprintf("Intercept b0: %.4g +/- %.4g",mm,sdm), "\n")
        cat(sprintf("Slope b1    : %.4g +/- %.4g",b1,ub1),"\n")
        cat(sprintf("Intercept b0: %.4g +/- %.4g",b0,ub0), "\n")
        cat(sprintf("R2: %.4g",Correlation^2), "\n")
        if (rtest$p.value < 0.05) { 
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
        
        
        calib <- list(mo,sdo, mm,sdm, b1, ub1, b0, ub0, RSS, rmse, mbe, Correlation, nb, CRMSE, NMSD, Mat)
        
        # resuming the par values
        on.exit(par(mar=op))
        
    } else{
        cat("The Mat dataFrame is empty. Returning NAs.")
        calib <- list(mo = NA,sdo = NA, mm = NA,sdm = NA, b1 = NA, ub1 = NA, b0 = NA, ub0 = NA, RSS = NA,rmse = NA, mbe = NA, Correlation = NA, nb = NA, CRMSE = NA, NMSD = NA, Mat = NA)
    } 
    
    names(calib) <- c("mo","sdo", "mm","sdm", "b1", "ub1", "b0", "ub0", "RSS","rmse", "mbe", "Correlation", "nb", "CRMSE", "NMSD", "Mat")
    print(data.frame(x=Mat$xis, 
                     uxi = Mat$uxi, 
                     Max.uxi=sqrt(RSS/(nb-2) + Mat$bias^2), 
                     Max.RSD = sqrt(RSS/(nb-2) + Mat$bias^2)/Mat$xis, 
                     Decrease.uxi = (Mat$uxi - sqrt(RSS/(nb-2) + Mat$bias^2)) > 0))
    #browser()
    return(calib)
}

#================================================================CR
### U.orth.DF: Function Orthogonal regression without plotting (Vs 180505)
#================================================================CR
U.orth.DF <- function(Mat, uxi = NULL, variable.uxi = FALSE) {
    # Mat            : DataFrame of data including Case number, Date, x And y + optional uxi if uxi is not constant for all reference values
    # uxi            : numeric (default = NULL ), random standard uncertainty of the results of the reference method, xis, given as a constant value for all xis reference values
    # variable.uxi   : logical, if FALSE (default = FALSE ), uxi is used as constant random standard uncertainties for all xis reference values. 
    #                  If TRUE uxi given in Mat and is used for each reference values
    # return a list with the orthogonal regression: 
    #                 "mo","sdo", "mm","sdm", "b1", "ub1", "b0", "ub0", "RSS","rmse", "mbe", "Correlation", "nb", "CRMSE", "NMSD", 
    #                 and "Mat": "case", "Date", "xis", "yis","uxi", "RS", "Ur", "U", "Rel.bias", "Rel.RSS"
    
    # 
    # Homogeneity of variance is tested For the calculation of RSS
    # adding Ur In a New field of Mat
    # returning a list with slope (b and ub), intercept (a and ua), the sum of square of residuals (RSS), 
    # the root means square of error (RMSE), the mean bias error (mbe), the coefficeint of correlation (Correlation), 
    # the number of valid measurment (nb), the centred rout mean square of error (CRMSE), the normalised mean standard deviation (NMSD)
    # and Mat with relative expanded uncertainty
    
    #checking that the mat dataFrame is not empty
    if(nrow(Mat)>0){
        
        # saving the original par values in case they would be modified in this function
        op <- par(no.readonly = TRUE)
        # Passing and resuming the par values
        on.exit(par(op))
        par(mar=c(4,4,2,0.5))
        par(new=FALSE)
        
        # Setting coloumn names and adding uxi as constant values
        colnames(Mat) <- c("case", "Date", "xis", "yis","uxi")[1:length(colnames(Mat))]
        if(!variable.uxi) if(!is.null(uxi)) Mat$uxi <- rep(uxi, nrow(Mat)) else{ cat("[slope_orth] ERRROR, value of u(xi) not given. Stopping the function"); return(0) } 
        
        # Filtering for the validation data only
        Mat <- subset(Mat, !is.na(Mat$xis) & !is.na(Mat$yis))
        
        #Orthogonal regression (see annex b of equivalence method)
        nb     <- nrow(Mat)
        mo     <- mean(Mat$xis)
        mm     <- mean(Mat$yis)
        sdo    <- sd(Mat$xis)
        sdm    <- sd(Mat$yis)
        Sxx    <- sum((Mat$xis - mo)^2)
        Syy    <- sum((Mat$yis - mm)^2)
        Sxy    <- sum((Mat$xis - mo) * (Mat$yis - mm))
        b1     <- (Syy - Sxx + sqrt((Syy- Sxx)^2 + 4*Sxy^2))/(2*Sxy)
        b0     <- mm - b1 * mo
        ub1    <- sqrt((Syy - (Sxy^2/Sxx))/((nb-2)*Sxx))
        ub0    <- sqrt(ub1^2 * sum(Mat$xis^2)/nb)
        
        # Regression statistics for Target Diagram (see delta tool user guide)
        rmse  <- sqrt((sum((Mat$yis - (b0 + b1 * Mat$xis))^2))/nb)
        mbe   <- mean(Mat$yis - Mat$xis)
        mae   <- mean(abs(Mat$yis - Mat$xis))
        CRMSE <- sqrt(mean(((Mat$yis - mm) - (Mat$xis - mo))^2))
        NMSD  <- (sd(Mat$yis) - sd(Mat$xis)) / sd(Mat$xis)
        Correlation <- cor(Mat$xis,Mat$yis)
        
        # Plots square of absolute residuals versus xis
        # Squares of Residuals and bias (vector of values)
        Mat$RS   <- (Mat$yis - (b0 + b1 * Mat$xis))^2
        Mat$bias <- (b0+(b1-1)*Mat$xis)
        # Sum of squares of Residuals (one constant value)
        RSS     <- sum(Mat$RS)
        
        # Checking if RS - Mat$uxi^2 < 0 that make an error using sqrt(RS - Mat$uxi^2) of the ree.RSS. Replacing with 0
        if(any((Mat$RS - Mat$uxi^2) <= 0)){
            
            cat(" Some \"Square or residuals - u(xi)^2\" are negative. \"sqrt(RSS/(n-2) - u(xi)^2\" cannot be calculated nor ux(i) can be > RSS.\n 
                You can decrease uxi, check in df Mat: x and max(uxi).\n
                For now, RSS/(nb-2) - Mat$uxi^2 is set to 0 when negative.\n")
            neg.RS <- which(RSS/(nb-2) - Mat$uxi^2 <= 0)
            Mat[neg.RS, "RS"] <- Mat[neg.RS, "uxi"]^2 + 0.001 * min(Mat$xis) # adding 0.1 % of min(xis) to avoid problem of 0 with gam fitting
            
            # Recalculating RSS when Mat$RS are changed
            RSS     <- sum(Mat$RS)
            
        }  else {
            
            # mat$RS are not changed and they are already calculated
            cat("All \"Squares of Residuals - u(xi)^2\" are positive. \"sqrt(RSS/(n-2) - u(xi)^2\" can be calculated, check in df Mat: x and max(uxi).\n")
            
        }
        
        # tesing significance of correlation between s and square of absolute reisduals - The calculation does not work only possibility the constrant RSS
        rtest <- cor.test(Mat$xis, Mat$RS)
        print(rtest, quote = FALSE)
        cat(sprintf("probability of H0 (r=0): %f, if <0.05, correlation is demonstrated, if > 0.95 there is no correlation",rtest$p.value), "\n")
        
        # if fitting the square of residuals is needed
        if(rtest$p.value < 0.05) { 
            
            cat("The residuals are not constant. RSS is calculated after applying a gam fitting .\n")
            
            #z <- lm((Mat$yis - (b0 + b1 * Mat$xis))^2 ~ Mat$xis)
            # Fitting with gam
            # if any y value is zero getting Warning: Error in eval: non-positive values not allowed for the 'gamma' family (we had 0.5 % of min(xis) to avoid this
            z <- gam( Mat$RS ~ s(Mat$xis), family=Gamma(link=log) )
            Mat$RS <- fitted(z) 
            
            #### Calculating uncertainty
            Mat$Ur <- 2 * sqrt(Mat$RS - Mat$uxi^2 + Mat$bias^2)/Mat$xis * 100
            Mat$U  <- 2 * sqrt(Mat$RS - Mat$uxi^2 + Mat$bias^2)  
            
            #### Calculating parameters for modified Target diagram
            Mat$Rel.bias <- 2 * (b0/Mat$xis + (b1 - 1))
            Mat$Rel.RSS  <- 2 * (sqrt( Mat$RS - uxi^2) / Mat$xis)
            
            
        } else {
            
            cat("The residuals are constant. RSS is calculated with equation for constant residuals.\n")
            cat(sprintf("RSS is the square root of sum of squares of Residuals divided by n - 2: %f", sqrt(sum((Mat$yis/(b0+b1*Mat$xis)-1)^2))/nb^2), "\n")
            # No need to lot a fitted line in this case
            
            #### Calculating uncertainty
            Mat$Ur <- 2 * sqrt(RSS/(nb-2) - Mat$uxi^2 + Mat$bias^2)/Mat$xis * 100
            Mat$U  <- 2 * sqrt(RSS/(nb-2) - Mat$uxi^2 + Mat$bias^2)  
            
            #### Calculating parameters for modified Target diagram
            Mat$Rel.bias <- 2 * (b0/Mat$xis + (b1 - 1))
            Mat$Rel.RSS  <- 2 * (sqrt( RSS/(nb - 2) - uxi^2) / Mat$xis)
            
            
        }    
        
        Mat$Max.uxi <- sqrt(RSS/(nb-2) + Mat$bias^2)
        Mat$Max.RSD <- sqrt(RSS/(nb-2) + Mat$bias^2)/Mat$xis
        
        # Printing
        cat("\n")
        cat("--------------------------------\n")
        cat(sprintf("mean of x   : %.1g +/- %.1g",mo,sdo),"\n")
        cat(sprintf("Intercept b0: %.4g +/- %.4g",mm,sdm), "\n")
        cat(sprintf("Slope b1    : %.4g +/- %.4g",b1,ub1),"\n")
        cat(sprintf("Intercept b0: %.4g +/- %.4g",b0,ub0), "\n")
        cat(sprintf("R2: %.4g",Correlation^2), "\n")
        if (rtest$p.value < 0.05) { 
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
        
        
        calib <- list(mo,sdo, mm,sdm, b1, ub1, b0, ub0, RSS, rmse, mbe, Correlation, nb, CRMSE, NMSD, Mat)
        
        # resuming the par values
        on.exit(par(mar=op))
        
    } else {
        
        cat("The Mat dataFrame is empty. Returning NAs.")
        calib <- list(mo = NA,sdo = NA, mm = NA,sdm = NA, b1 = NA, ub1 = NA, b0 = NA, ub0 = NA, RSS = NA,rmse = NA, mbe = NA, Correlation = NA, nb = NA, CRMSE = NA, NMSD = NA, Mat = NA)
    } 
    
    names(calib) <- c("mo","sdo", "mm","sdm", "b1", "ub1", "b0", "ub0", "RSS","rmse", "mbe", "Correlation", "nb", "CRMSE", "NMSD", "Mat")
    # print(data.frame(x            = Mat$xis, 
    #                  uxi          = Mat$uxi, 
    #                  Max.uxi      = sqrt(RSS/(nb-2) + Mat$bias^2), 
    #                  Max.RSD      = sqrt(RSS/(nb-2) + Mat$bias^2)/Mat$xis, 
    #                  Decrease.uxi = (Mat$uxi - sqrt(RSS/(nb-2) + Mat$bias^2)) > 0))
    return(calib)
}

#================================================================CR
### f_log: Function Fitting a logarithmic model (Vs 141114)
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
### f_Unitec: Function Fitting a the Unitec model (Vs 141114)
#================================================================CR
f_Unitec <- function(x, a, b,c) {
    # mVolt <- ((y-a)/b)^(1/c) 
    return(y = (((x*1.91*(293.15/TK))-a)/b)^(1/c))
    # Y(?g/m3) (*1.91) = a+b*X(mV)^c 
    # with a=-31.6  b=5330.9  -0.598, x en mV	min(x)=47.2	max(x)=5299.9	unitY=?g/m3	ymax=500?g/m3
    # Equation of Unitec
}

#================================================================CR
### f_ExpDI: Function Fitting an exponential Decay (Increasing form) model (Vs 141114)
#================================================================CR
f_ExpDI <- function(x, C, k) {
    # C is the asymtoptic
    # data shall pass by 0,0
    return(C *(1-exp(-k*x)))
}

#================================================================CR
### f_ExpDI_Int: Function Fitting an exponential Decay (Increasing form) model with intercept (see wikipedia) (Vs 141114)
#================================================================CR
f_ExpDI_Int <- function(x, C, k,intercept) {
    # C is the max asymtoptic value on y axis
    # intercept is the min asymtoptic value on y axis
    return(C *(1-exp(-k*x)) + intercept)
}

#================================================================CR
### f_Michelis: Function Fitting a Michaelis-Menten kinetics model with intercept (Vs 141114)
#================================================================CR
f_Michelis <- function(x, Vmax, km, intercept) {
    # Vmax is the max asymtoptic value on y axis
    # intercept is the min asymtoptic value on y axis
    # km is The Michaelis constant is the concentration at which the sensor response is half of V_\max
    return(Vmax*x/(km +x) + intercept)
}

#================================================================CR
### f_ExpDD_Int: Function Fitting an exponential Decay (Decreasing form) model with intercept (Vs 141114)
#================================================================CR
f_ExpDD_Int <- function(x, C, k,intercept) {
    # C is the max value on y axis at x= 0
    # intercept is the min asymtoptic value on y axis
    return(C *(exp(-k*x)) + intercept)
}

#================================================================CR
### f_Sigmoid: Function Fitting a Logistic - Sigmoidal function (Vs 141114)
#================================================================CR
f_Sigmoid <- function(x, MIN, Asym, xmid, Hill) {
    return(y = MIN + (Asym-MIN) / (1 + (xmid/x)^Hill))
    #, Hill > 0, between 1a dn 10
    #Asymptotic to y = Max to right,
    #Asymptotic to y = Min to left,
    #Passes through (0, a/(1+b) )
    #x50 x value at the inflection point, in this equation x is not log transformed
}

#================================================================CR
### Estimated.y: Function Plot estimated function (Vs 141114)  
#================================================================CR
Estimated.y <- function(x, Model) {
    # This function estimates the y values at 5000 points of x from min(x) until max(x)
    # Return : data.frame estimated with variable x and y, each one with 5000 points in a x ascending order
    # x: Data used to establish the model (only x)
    # Model: the Model to plot
    
    Estimated <- data.frame(x = seq(min(x),max(x),length.out=5000), y= seq(min(x),max(x),length.out=5000))
    
    if(inherits(Model, "gam")){ # checking if the model was fitted from gam function
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
Cal_Line <- function(x, s_x, y, s_y, Mod_type,  Multi.File = NULL, Matrice=NULL, line_position, Couleur, Sensor_name = NULL, f_coef1, f_coef2, f_R2, 
                     lim = NULL, marges = NULL, Covariates = NULL) {
    # This Function estimates the calibration function, plots the calibration line and write the equation above the plot at line_position
    # The regression equation can be weithed (1/sy^2) or not if s_y = Null
    
    # Inputs:
    # x, s_x, y, s_y: x and y values with their standard devation that can be equal to Null
    # Mod_type      : type of calibration model: linear, ...
    # Multi.File    : char, default is NULL, path.file of the config file used for calibration with multivariates
    # Matrice       : Input Matrix of data (e. g. Pre_cal_df)
    # line position : for mtext of the regression equation
    # Couleur       : color of the line and color font of the equation
    # Sensor_name   : name of the sensor to be written in front of the calibration equation. If NULL, sensor name is not printed
    # f_coef1, 
    # f_coef2, 
    # f_R2          : number of digit for intercept, slope and R2 using sprintf syntax. 
    #                 f_coef1 is used both for intercept and s(Res), f_coef2 is used for all parameters of the model apart from the intercept
    # lim           : matrix of column vectors Xlim, YLIM: limits of the plots where to add the calibration line. When NULL the lim are automatically calculated.
    # marges        : margin of graph, if NULL set as c(4,4,3,0.5)). If you don't want to set the margin, write "NO"
    # Output:     
    
    # Return: the estimated model 
    
    # saving the original par() values
    op <- par(no.readonly = TRUE)
    # resuming the par values
    on.exit(par(op))
    
    # settings the margins
    if(is.null(marges)){
        Margin <- c(4,4,3,0.5)
        par(mar = c(4,4,3,0.5))	
    } else {
        Margin <- par("mar")
    }
    
    #Define the limits of the graphs
    if(is.null(lim)){
        Xrange <- c(min(x), max(x))
        Yrange <- c(min(y), max(y))
        lim = cbind(Xrange, Yrange)
    }
    
    #Put O3, sensor reponses and standard deviation of sensor responses in a matrix remove NA of x and y
    if(is.null(s_y) || any(s_y == 0)) {
        
        DataXY <- data.frame(x = x, y = y)
        # # removing NA of x and y
        # DataXY <- subset(DataXY, !is.na(x) & !is.na(y))
    } else {
        DataXY <- data.frame(x = x, y = y, s_y = s_y)
        DataXY$wi <- DataXY$s_y^-2/sum(DataXY$s_y^-2)
        # colnames(DataXY) <- c("x","y","s_y","wi")
        # # removing NA of s_y
        # DataXY <- subset(DataXY, !is.na(x) & !is.na(y) & !is.na(s_y) & !s_y==0)
    }
    # adding Covariates for multilinear model
    #browser()
    if(Mod_type == "MultiLinear") DataXY[,Covariates] <- Matrice[,Covariates]
    
    # removing NA of any variables in DATAXY
    DataXY <- DataXY[complete.cases(DataXY),]
    
    # Add ", " at the end of Sensor_name to be print with the legend
    if(!is.null(Sensor_name)){
        Sensor_name <- paste0(Sensor_name, ", ")
    }
    
    # Linear Model, if s_y is not null calculate weights wi and use them in the regression
    if(Mod_type == 'Linear') {
        #browser()
        if(is.null(s_y) || any(s_y == 0)) {
            Model <- lm(y ~ x, data = DataXY, model = TRUE, x = TRUE, y = TRUE)
        } else {
            Model <- lm(y ~ x, data = DataXY, model = TRUE, x = TRUE, y = TRUE, weights = wi)
        }
        print(summary(Model))
        
        # plotting calibrationn lines without plotting axis
        Estimated <- Estimated.y(DataXY$x, Model) 
        if(is.null(marges)){
            par(new=TRUE)
        } else {
            par(new=TRUE)#, mar=Margin)
        }
        plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt ="n", yaxt = "n" , xlab= "", ylab = "") 
        
        # display equations and R^2
        mtext(sprintf(paste0(Sensor_name, "Linear: y= ", f_coef1,"+ ", f_coef2," x",", R2=", f_R2,", RMSE=", f_coef1,",AIC= %.1f") # ", s(Res)=",f_coef1,
                      , coef(Model)[1], 
                      coef(Model)[2], 
                      summary(Model)$r.squared,
                      #sd(resid(Model)), 
                      sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),
                      AIC(Model)), 
              line=line_position,adj=1,padj=0,col=Couleur,cex=0.875) 
        
    } else if(Mod_type == 'Linear.Robust') {
        
        # MGV Robust Linear Model, (if s_y is not null calculate weights wi and use them in the regression
        # This models the median of y as a function of x, rather than modelling the mean of y as a function of x, in the case of least squares regression.
        if("quantreg" %in% rownames(installed.packages()) == FALSE) {install.packages("quantreg")}
        library("quantreg")
        if(is.null(s_y) || any(s_y == 0)) {
            Model <- rq(y ~ x, data = DataXY, tau=0.5, model = TRUE)
        } else {
            Model <- rq(y ~ x, data = DataXY, weights = wi, tau=0.5, model = TRUE)
        }
        print(summary(Model))
        
        # plotting calibration lines without plotting axis
        Estimated <- Estimated.y(DataXY$x, Model) 
        par(new=TRUE)#, mar=Margin)
        plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt ="n", yaxt = "n" , xlab= "", ylab = "") 
        
        # display equations and R^2
        par(bg="blue")
        #mtext(sprintf(paste0("Robust Linear: y= ",f_coef1,"+ ",f_coef2," x, Std.error= ",f_R2,", t value= ",f_coef1),coef(Model)[1], coef(Model)[2]
        #              , summary(Model)$r.squared,sd(resid(Model))),line=line_position,adj=1,padj=0,col=Couleur,cex=0.9) 
        #             , summary(Model)$Std.error,summary(Model)$t valuesd(resid(Model))),line=line_position,adj=1,padj=0,col=Couleur,cex=0.9) 
        mtext(sprintf(paste0("Quantile regression tau 0.5: y= ",f_coef1,"+ ",f_coef2," x"),coef(Model)[1], coef(Model)[2])
              ,line=line_position,adj=1,padj=0,col=Couleur,cex=0.9) 
        
    } else if(Mod_type == 'gam') {
        
        #     if("quantreg" %in% rownames(installed.packages()) == FALSE) {install.packages("quantreg")}
        #     library("quantreg")
        Estimated <- data.frame(x=x, y=y)
        if(is.null(s_y) || any(s_y == 0)) {
            # Let gam89 decide for k and estimate the best alpha
            Model <- gam(y~s(x), family=Gamma(link=log), data = Estimated) 
            #Model <- gam(y~s(x, k=5), family=Gamma(link=log), data = Estimated) 
        } else {
            # Let gam89 decide for k and estimate the best alpha
            Model <- gam(y~s(x), family=Gamma(link=log), weights = wi) 
            #Model <- gam(y~s(x, k=5), family=Gamma(link=log), weights = wi) 
        }
        print(summary(Model))
        
        # plotting calibration lines without plotting axis
        Estimated <- Estimated.y(DataXY$x, Model)
        Estimated <- Estimated[order(Estimated$x),]
        par(new=TRUE)#, mar=Margin)
        plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt ="n", yaxt = "n" , xlab= "", ylab = "") 
        #plot(x,Model$fitted.values, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt ="n", yaxt = "n" , xlab= "", ylab = "")
        
        # display equations and R^2
        par(bg="blue")
        #mtext(sprintf(paste0("Robust Linear: y= ",f_coef1,"+ ",f_coef2," x, Std.error= ",f_R2,", t value= ",f_coef1),coef(Model)[1], coef(Model)[2]
        #              , summary(Model)$r.squared,sd(resid(Model))),line=line_position,adj=1,padj=0,col=Couleur,cex=0.9) 
        #             , summary(Model)$Std.error,summary(Model)$t valuesd(resid(Model))),line=line_position,adj=1,padj=0,col=Couleur,cex=0.9) 
        mtext(sprintf(paste0("General Additive model"))
              ,line = line_position,adj = 1, padj = 0, col = Couleur, cex = 0.9) 
        
    } else if (Mod_type == 'MultiLinear') {
        #browser()
        if (!is.null(Multi.File)) { 
            
            if (file.exists(Multi.File)) {
                
                # read Multi.File
                Multi.File.df <-  read.table(file             = Multi.File, 
                                             header           = TRUE, 
                                             row.names        = NULL, 
                                             comment.char     = "#", 
                                             stringsAsFactors = FALSE
                )
                
                # set Formula for all Covariates
                # Checking that some Covariates shall be fitted
                if (any(Multi.File.df$Enabled & !Multi.File.df$Forced)) {
                    
                    # Covariates whithout Intercept, Enables and not forced
                    Cov.MinusInt <- Multi.File.df$Covariates[which(Multi.File.df$Enabled & !Multi.File.df$Forced)][Multi.File.df$Covariates != "Intercept"]
                    # their degree of polynomial
                    Degrees <-  Multi.File.df[Multi.File.df$Covariates == Cov.MinusInt, "degree"]
                    if (exists("Formula.Cov.MinusInt")) rm(Formula.Cov.MinusInt)
                    for (j in 1:length(Cov.MinusInt)) {
                        Formula.Covar <- paste0("I(",Cov.MinusInt[j],"^",seq(1:Degrees[j]),")",collapse = "+")
                        if (exists("Formula.Cov.MinusInt")) Formula.Cov.MinusInt <- paste0(Formula.Cov.MinusInt, Formula.Covar, collapse = "+") else Formula.Cov.MinusInt <- Formula.Covar
                        rm(Formula.Covar)
                    }
                    Formula.Covariates <- as.formula(paste("y ~ x ", Formula.Cov.MinusInt, sep = "+"))
                    
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
                        cat("[Cal_line] INFO, no covariate needs fitting!\n")
                    } 
                }
            } else {
                
                # Set Formula
                #https://stackoverflow.com/questions/5251507/how-to-succinctly-write-a-formula-with-many-variables-from-a-data-frame
                Formula.Covariates <- as.formula(paste("y ~ ", paste(c("x",Covariates), collapse = "+")))
                Model.offset <- ""
            }
        } else {
            
            # Set Formula
            Formula.Covariates <- as.formula(paste("y ~ ", paste(c("x",Covariates), collapse = "+")))
            Model.offset <- ""
        }  
        
        # Fitting
        if (is.null(s_y) || any(s_y == 0)) {
            Model <- lm(Formula.Covariates, data = DataXY, model = TRUE, x = TRUE, y = TRUE)
        } else {
            Model <- lm(Formula.Covariates, data = DataXY, model = TRUE, x = TRUE, y = TRUE, weights = wi)
        }
        print(summary(Model))
        
        # # plotting calibrationn lines without plotting axis - for multiLinear, it does not make sence since there are several input variables
        # Estimated <- Estimated.y(DataXY$x, Model) 
        # par(new=TRUE)#, mar=Margin)
        # plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt ="n", yaxt = "n" , xlab= "", ylab = "") 
        # 
        # # display equations and R^2
        # mtext(sprintf(paste0(Sensor_name, "Quadr.: y= ",f_coef1,"+ ",f_coef2,"x+ ",f_coef2,"x2",", R2=",f_R2,", s(Res)=",f_coef1,", RMSE=",f_coef1,",AIC= %.1f")
        #               ,coef(Model)[1],coef(Model)[2],coef(Model)[3] ,summary(Model)$r.squared,sd(resid(Model)), sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),AIC(Model))
        #       ,line=line_position,adj=1,padj=0,col=Couleur,cex=0.875) 
        
    } else if (Mod_type == 'Quadratic') {
        
        if (is.null(s_y) || any(s_y == 0)) {
            Model <- lm(y ~ poly(x, degree = 2, raw = TRUE), data = DataXY, model = TRUE, x = TRUE, y = TRUE)
        } else {
            Model <- lm(y ~ poly(x, degree = 2, raw = TRUE), data = DataXY , weights = wi, model = TRUE, x = TRUE, y = TRUE)
        }
        print(summary(Model))
        
        # plotting calibrationn lines without plotting axis
        Estimated <- Estimated.y(DataXY$x, Model) 
        par(new = TRUE)#, mar=Margin)
        plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type = "l",xaxt = "n", yaxt = "n" , xlab = "", ylab = "") 
        
        # display equations and R^2
        mtext(sprintf(paste0(Sensor_name, "Quadr.: y= ",f_coef1,"+ ",f_coef2,"x+ ",f_coef2,"x2",", R2=",f_R2, ", RMSE=",f_coef1,",AIC= %.1f") # ", s(Res)=",f_coef1,
                      ,coef(Model)[1],
                      coef(Model)[2],
                      coef(Model)[3] ,
                      summary(Model)$r.squared,
                      #sd(resid(Model)), 
                      sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),
                      AIC(Model))
              ,line=line_position,adj=1,padj=0,col=Couleur,cex=0.875) 
        
    } else if(Mod_type=='Cubic') {
        
        if(is.null(s_y)|| any(s_y == 0)) {
            Model <- lm(y ~ poly(x, 3, raw =TRUE), data =DataXY, model = TRUE, x = TRUE, y = TRUE)
        } else {
            Model <- lm(y ~ poly(x, 3, raw=TRUE) , data =DataXY, weights = wi, model = TRUE, x = TRUE, y = TRUE)
        }
        print(summary(Model))
        
        # plotting calibrationn lines without plotting axis
        Estimated <- Estimated.y(DataXY$x, Model) 
        par(new=TRUE)#, mar=Margin)
        plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt ="n", yaxt = "n" , xlab= "", ylab = "") 
        
        # display equations and R^2
        mtext(sprintf(paste0(Sensor_name, "Cubic: y= ",f_coef1,"+",f_coef2,"x+",f_coef2,"x2+",f_coef2,"x3",",R2=",f_R2,",RMSE=", f_coef1,",AIC= %.1f"),  # ", s(Res)=",f_coef1,
                      coef(Model)[1],
                      coef(Model)[2],
                      coef(Model)[3],
                      coef(Model)[4],
                      summary(Model)$r.squared,
                      #sd(resid(Model)), 
                      sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),
                      AIC(Model)),
              line=line_position,adj=1,padj=0,col=Couleur,cex=0.875) 
        
    } else if(Mod_type=='ExpDecayInc') {
        
        if(is.null(s_y)|| any(s_y == 0)) {
            Model <- nlsLM(y~f_ExpDI(x,C,k), data=DataXY, start=list(C=max(y), k=0.05), model = TRUE, x = TRUE, y = TRUE)
        } else {
            Model <- nlsLM(y~f_ExpDI(x,C,k), data=DataXY, start=list(C=max(y), k=0.05), weights = wi, model = TRUE, x = TRUE, y = TRUE)
        }
        print(summary(Model))
        
        # plotting calibrationn lines without plotting axis
        Estimated <- Estimated.y(DataXY$x, Model) 
        par(new=TRUE)#, mar=Margin)
        plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt ="n", yaxt = "n" , xlab= "", ylab = "") 
        
        # display equations and R^2
        mtext(sprintf(paste0(Sensor_name, "Exp. decay inc.: ys = ",f_coef1,"(1-exp(-",f_coef2,"x))", ",RMSE=",f_coef1,",AIC= %.1f"), # "s(Res)=",f_coef1,
                      coef(Model)[1],
                      coef(Model)[2],
                      #sd(resid(Model)), 
                      sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),
                      AIC(Model))
              ,line=line_position,adj=1,padj=0,col=Couleur,cex=0.875)
        
    } else if(Mod_type=='ExpDecayInc_Int') {
        if(is.null(s_y) || any(s_y == 0)) {
            Model <- nlsLM(y~f_ExpDI_Int(x, C, k,intercept), data=DataXY, start=list(C=max(y), k=0.05, intercept=min(y)), model = TRUE, x = TRUE, y = TRUE)
        } else {
            Model <- nlsLM(y~f_ExpDI_Int(x, C, k,intercept), data=DataXY, start=list(C=max(y), k=0.05, intercept=min(y)), weights = wi, model = TRUE, x = TRUE, y = TRUE)
        }
        print(summary(Model))
        
        # plotting calibrationn lines without plotting axis
        Estimated <- Estimated.y(DataXY$x, Model) 
        par(new=TRUE)#, mar=Margin)
        plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt ="n", yaxt = "n" , xlab= "", ylab = "") 
        
        # display equations and R^2
        mtext(sprintf(paste0(Sensor_name, "Exp. decay inc.: ys = ",f_coef2,"(1-exp(-",f_coef2,"x))+",f_coef1,",RMSE=",    #", s(Res)=",f_coef1,
                             f_coef1,",AIC= %.1f"),
                      coef(Model)[1],
                      coef(Model)[2],
                      coef(Model)[3], 
                      #sd(resid(Model)), 
                      sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),
                      AIC(Model)), 
              line=line_position,adj=1,padj=0,col=Couleur,cex=0.875)
        
    } else if(Mod_type=='ExpDecayDec_Int') {
        
        if(is.null(s_y)|| any(s_y == 0)) {
            Model <- nls(y~f_ExpDD_Int(x, C, k,intercept), data=DataXY, start=list(C=max(y), k=0.05, intercept=min(y)), model = TRUE, x = TRUE, y = TRUE)
        } else {
            Model <- nls(y~f_ExpDD_Int(x, C, k,intercept), data=DataXY, start=list(C=max(y), k=0.05, intercept=min(y)), weights = wi, model = TRUE, x = TRUE, y = TRUE)
        }
        print(summary(Model))
        
        # plotting calibrationn lines without plotting axis
        Estimated <- Estimated.y(DataXY$x, Model) 
        par(new=TRUE)#, mar=Margin)
        plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt ="n", yaxt = "n" , xlab= "", ylab = "") 
        
        # display equations and R^2
        mtext(sprintf(paste0(Sensor_name, "Exp. decay dec: y =",f_coef2,"exp(-",f_coef2,"x))+",f_coef1,",RMSE=",   #", s(Res)=",f_coef1,
                             f_coef1,",AIC= %.1f"), 
                      coef(Model)[1],
                      coef(Model)[2],
                      coef(Model)[3], 
                      #sd(resid(Model)), 
                      sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),
                      AIC(Model)), 
              line=line_position,adj=1,padj=0,col=Couleur,cex=0.875)
        
    } else if(Mod_type=='Michelis') {
        
        if(is.null(s_y) || any(s_y == 0)) {
            Model <- nlsLM(y ~ MIN + f_Michelis(x, Vmax, km, intercept), data=DataXY, start=list(Vmax=max(y), km=mean(y)/4, MIN = min(y)), model = TRUE, x = TRUE, y = TRUE)
            #Model <- nlsLM(y~ MIN + SSmicmen(x, Vmax, km), data=DataXY, start=list(Min = min(y),getInitial(y~ SSmicmen(x, Vmax, km), data=DataXY)))
        } else {
            Model <- nlsLM(y ~ f_Michelis(x, Vmax, km, MIN), data=DataXY, start=list(Vmax=max(y), km=mean(y)/4, MIN = min(y)), weights = wi, model = TRUE, x = TRUE, y = TRUE)
            #Model <- nlsLM(y~ MIN + SSmicmen(x, Vmax, km), data=DataXY, weights = wi)
        }
        print(summary(Model))
        
        # plotting calibrationn lines without plotting axis
        Estimated <- Estimated.y(DataXY$x, Model) 
        par(new=TRUE)#, mar=Margin)
        plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt ="n", yaxt = "n" , xlab= "", ylab = "") 
        
        # display equations and R^2
        mtext(sprintf(paste0(Sensor_name, "Michelis: y =",f_coef2,"/(",f_coef2,"+x)+",f_coef1,",RMSE=",f_coef1,",AIC= %.1f"), # ", s(Res)=", f_coef1,
                      coef(Model)[1],
                      coef(Model)[2],
                      coef(Model)[3],
                      #sd(resid(Model)), 
                      sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),
                      AIC(Model))
              ,line=line_position,adj=1,padj=0,col=Couleur,cex=0.875)
        
    } else if(Mod_type=='Logarithmic') {
        
        if(is.null(s_y) || any(s_y == 0)) {
            Model <- nls(y~f_log(x,a,b), data=DataXY, start=list(a=min(y), b=10), model = TRUE, x = TRUE, y = TRUE)
        } else {
            Model <- nls(y~f_log(x,a,b), data=DataXY, start=list(a=min(y), b=10), weights = wi, model = TRUE, x = TRUE, y = TRUE)
        }
        print(summary(Model))
        
        # plotting calibrationn lines without plotting axis
        DataXY <- DataXY[order(DataXY$x),] # To avoid that the line go back for lower x
        Estimated.y(DataXY$x, Model) 
        
        # display equations and R^2
        mtext(sprintf(paste0(Sensor_name, "Log. model: y = ",f_coef1," + ",f_coef2," log(x)), ", ",RMSE=",f_coef1,",AIC= %.1f"), # " s(Res)=", f_coef1,
                      coef(Model)[1],
                      coef(Model)[2], 
                      #sd(resid(Model)), 
                      sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),
                      AIC(Model)),
              line=line_position,adj=1,padj=0,col=Couleur,cex=0.875)
        
    } else if(Mod_type=='Sigmoid') {
        
        #nls.control(maxiter = 500, tol = 1e-05, minFactor = 1/4096, printEval = TRUE, warnOnly = TRUE)
        if(is.null(s_y) || any(s_y == 0)) {
            # Model <- nls(y~f_Sigmoid(x, MIN, MAX, X50, Hill), data=DataXY, start=list(MIN=min(y),MAX=max(y),X50=mean(x), Hill=3)
            #               , control = list(maxiter = 500, tol=1e-2, minFactor = 1/1024, printEval = TRUE, warnOnly=FALSE), trace=TRUE)
            Model <- nlsLM(y~ f_Sigmoid(x, MIN, Asym, xmid, Hill), data=DataXY, start=list(MIN=min(y),Asym=max(y),xmid=mean(x), Hill=3)
                           , control = list(maxiter = 500, tol=1e-2, minFactor = 1/1024, printEval = TRUE, warnOnly=FALSE), trace=TRUE, model = TRUE, x = TRUE, y = TRUE)
        } else {
            Model <- nlsLM(y~ f_Sigmoid(x, MIN, Asym, xmid, Hill), data=DataXY, start=list(MIN=min(y),Asym=max(y),xmid=mean(x), Hill=3), weights = wi
                           , control = list(maxiter = 500, tol=1e-2, minFactor = 1/1024, printEval = TRUE, warnOnly=FALSE), trace=TRUE, model = TRUE, x = TRUE, y = TRUE)
            # Model <- nls(y~ MIN + SSlogis(x, Asym, xmid, scal) , data=DataXY, start=list(MIN=min(y)), weights = wi
            #             , control = list(maxiter = 500, tol=1e-2, minFactor = 1/(1024*32), printEval = TRUE, warnOnly=FALSE), trace=TRUE)
        }
        print(summary(Model))
        
        # plotting calibrationn lines without plotting axis
        Estimated <- Estimated.y(DataXY$x, Model) 
        par(new=TRUE)#, mar=Margin)
        plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt ="n", yaxt = "n" , xlab= "", ylab = "") 
        
        # display equations and R^2
        mtext(sprintf(paste0(Sensor_name, "Sigmoid: y=",f_coef1,"+",f_coef2,"/(1+(",f_coef2,"/x)^",f_coef2,"), ", ",RMSE=",f_coef1,",AIC= %.1f"), # "s(Res)=", f_coef1,
                      coef(Model)[1],
                      coef(Model)[2] - coef(Model)[1],
                      coef(Model)[3],
                      coef(Model)[4],
                      #sd(resid(Model)), 
                      sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),
                      AIC(Model)),
              line=line_position,adj=1,padj=0,col=Couleur,cex=0.875)
        
    } else if(Mod_type=='Unitec') {
        
        #Put O3, sensor reponses and standard deviation of sensor responses in a matrix: Not done for now
        a=-31.6
        b=5330.9
        c=-0.598
        DataXY <- data.frame(cbind(x*2.05, ((y-a)/b)^(1/c), s_y))
        colnames(DataXY) <- c("x","y")
        if(is.null(s_y) || any(s_y == 0)) {
            Model <- nls(y~f_Unitec(x, a, b,c), data=DataXY, start=list(a=-31.6,b=5330.9,-0.598), model = TRUE, x = TRUE, y = TRUE)
        } else {
            Model <- nls(y~f_Unitec(x,a,b,c),data=DataXY, start=list(a=-31.6,b=5330.9,-0.598), weights = w, model = TRUE, x = TRUE, y = TRUEi)
        }
        print(summary(Model))
        
        # plotting calibrationn lines without plotting axis
        Estimated <- Estimated.y(DataXY$x, Model) 
        par(new=TRUE)#, mar=Margin)
        plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt ="n", yaxt = "n" , xlab= "", ylab = "") 
        
        # display equations and R^2
        mtext(sprintf(paste0(Sensor_name, "Unitec: y = ((1.91x(293.15/T)-",f_coef1,")/",f_coef2,")^(1/",f_coef21,"), ", ",RMSE=",f_coef1,",AIC= %.1f"),  # "s(Res)=", f_coef1,
                      coef(Model)[1],
                      coef(Model)[2],
                      coef(Model)[3],
                      coef(Model)[4],
                      #sd(resid(Model)), 
                      sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),
                      AIC(Model))
              ,line=line_position,adj=1,padj=0,col=Couleur,cex=0.875)
    }
    summary(Model)
    # resuming the par values
    on.exit(par(op))
    
    return(Model)
}

#================================================================CR
### Function Measurement Function x = f(y) once Calibration function (y = f(x) of sensor is established e.g with Cal_Line (Vs 141114)
#================================================================CR
Meas_Function <- function (y, Mod_type, Model, covariates = NULL, Matrice = NULL) {
    # This function estimates the x value using a calibration model (Model)
    # y            : Sensor data to be converted to concentration level using the reverse calibration function (Model)
    # Mod_type     : type of calibration function: Linear, Quadratic, Sigmoid
    # Model        : the calibration function
    # Covariates   : vectors of strings representing the covariates when model needs covariates
    # Matrice      : Covariates values, data.frame, default is NULL
    
    #browser()
    if(Mod_type == 'Linear' || Mod_type == 'Linear.Robust') {
        
        return((y-coef(Model)[1])/coef(Model)[2])
        
    } else if(Mod_type == 'MultiLinear') {
        #browser()
        # convert any column of Matrice that is not numeric (Date) to numeric
        if(!all(grepl(pattern = "numeric", x = sapply(lapply(Matrice, class), "[", 1)))){
            cat("[Meas_Function] INFO, some covariates are not numeric. Converting to numeric.\n")
            Col.no.numeric <- grep(pattern = "numeric", x = sapply(lapply(Matrice, class), "[", 1), invert = TRUE)
            Matrice[,Col.no.numeric] <- sapply(Matrice[,Col.no.numeric],as.numeric)
        }
        # matrix of covariates
        # Remove NewMatrix if it exists
        if(exists("NewMatrice")) rm(NewMatrice)
        # Checking if there are polynomials
        if(any(grepl(pattern = paste0("I\\("), x = names(coef(Model))[3:length(coef(Model))]) )){
            
            # checking for the order of poly
            for(j in names(Matrice)){
                
                # Checking if any Covariates are poly() create a new Matrice
                if(any(grepl(pattern = paste0("I\\(",j), x = names(coef(Model))[3:length(coef(Model))]) )){
                    
                    # Getting the degrees of j in poly() (e. g. I(Temperature^1) )
                    Cov.Index <- grep(pattern = j, x = names(coef(Model))[3:length(coef(Model))]) 
                    Power = as.numeric(sub(pattern= ")", 
                                           replacement = "",
                                           x = sub(pattern = ".*\\^", 
                                                   replacement = "", 
                                                   x = names(coef(Model))[3:length(coef(Model))][Cov.Index]
                                           )
                    )
                    )
                    # Add the Covariates to Matrice, it is poly
                    for(k in Power){
                        
                        if(exists("NewMatrice")) {
                            
                            NewMatrice <- cbind(NewMatrice, Matrice[,j]^k)
                            
                        } else NewMatrice  <- as.data.frame(Matrice[,j]^k)
                    }
                } else {
                    
                    # Add the Covariates to Matrice, it is not poly
                    if(exists("NewMatrice")) NewMatrice  <- cbind(NewMatrice, Matrice[,j]) else NewMatrice  <- Matrice[,j]
                }
            }
            
            
            # update Matrice if needed
            Matrice <- NewMatrice
            rm(NewMatrice)
        }
        #browser()
        if(class(Matrice) != "matrix") Matrice <- as.matrix(Matrice)
        M.Cov <- Matrice %*% coef(Model)[3:length(coef(Model))]
        Estimated <- as.vector((y - (coef(Model)[1] + M.Cov ))/coef(Model)[2])
        return(Estimated)
        
    } else if(Mod_type == 'Quadratic') {
        
        # le choix de la racine du polynome depend du signe du coefficient du monome de 2eme degre
        # et si l'on utilise la partie croissante ou decroissante de la parabole
        # Si le determinat est negatif : NA
        
        x <- data.frame(y=y,determinant = coef(Model)[2]^2-4*coef(Model)[3]*(coef(Model)[1]-y), root = NA, stringsAsFactors = FALSE)
        if(coef(Model)[2] > 0){
            x[x$determinant>0 ,"root"] <- (-coef(Model)[2]+sqrt(x[x$determinant>0,"determinant"]))/(2*coef(Model)[3])
        } else {
            x[x$determinant>0 ,"root"] <- (-coef(Model)[2]-sqrt(x[x$determinant>0,"determinant"]))/(2*coef(Model)[3])
        }
        return(x$root)
        
    } else if(Mod_type=='Cubic') {
        require(polynom)
        xroot  <- data.frame(y=y, root1=NA, root2=NA, root3=NA, stringsAsFactors = FALSE)
        
        xroot[,2] <- apply(xroot, MARGIN = 1, function(x){
            # Solving cubic equation using the eigen numbers of package polynom
            # for x^3-8 = 0 , it is the same as:
            # a <- c(0,0,8)
            # m <- matrix(c(0,0,-a[1],1,0,-a[2],0,1,-a[3]), byrow=T, nrow=3)
            # roots <- eigen(m, symm=F, only.values=T)$values
            # see https://stackoverflow.com/questions/2003465/fastest-numerical-solution-of-a-real-cubic-polynomial
            
            p <- polynom::polynomial(c(coef(Model)[1]-x[1],coef(Model)[2],coef(Model)[3],coef(Model)[4]))
            x[2:4] <- solve(p)
            #print(x)
            
            # Discarding complex number, and negative values
            for(j in 2:4) {
                if(Im(x[j])!=0 || Re(x[j]) < 0) x[j] <- NA else x[j] <- as.numeric(Re(x[j]))
            }
            #print(x)
            
            # Returning only one root, if more than one, take the smallest one non negative
            #browser()
            if(any(!is.na(x[2:4]))){
                if(length(which(!is.na(x[2:4])))==1) x[2] <- x[which(!is.na(x[2:4]))] else x[2] <- min(x[which(!is.na(x[2:4]))], na.rm = TRUE)  
            } 
            #print(Re(x[2]))
            return(Re(x[2]))
        })
        return(xroot[,2])
        
    } else if(Mod_type=="gam"){ # checking if the model was fitted from gam function
        
        #browser()
        return(predict(Model, newdata = data.frame(x = y[!is.na(y)], y = rep(numeric(0), length = length(y[!is.na(y)]))), type = "response"))
        
    } else if(Mod_type=='Exponential') {
        
        return(-log(1-(y - coef(Model)[3])/coef(Model)[1])/coef(Model)[2])
        
    } else if(Mod_type=='ExpDecayInc_Int') {
        
        if(any(!is.na(y) & y<coef(Model)[3] ) | any(!is.na(y) & y>coef(Model)[1] + coef(Model)[3])) { # in case value out of bound and value is not NA
            print("Some Y value out of limits of the model", quote = FALSE)
        }
        x <- log(1-((y - coef(Model)[3])/coef(Model)[1]))/-coef(Model)[2]
        #  yy <- coef(Model)[1] * (1-exp(-coef(Model)[2] * xx)) + coef(Model)[3]
        return(x)
        
    } else if(Mod_type=='Michelis') {
        # modele f_Michelis: return(Vmax*x/(km +x) + intercept)
        if(any(!is.na(y) & y<coef(Model)[3] ) | any(!is.na(y) & y>coef(Model)[1])) { # in case value out of bound and value is not NA
            print("Some Y value out of limits of the model", quote = FALSE)
        }
        x <- (-coef(Model)[2]*(y - coef(Model)[3])) / (y - coef(Model)[3] - coef(Model)[1])
        #  yy <- coef(Model)[1] * (1-exp(-coef(Model)[2] * xx)) + coef(Model)[3]
        return(x)
        
    } else if(Mod_type=='ExpDecayDec_Int') {
        # return(y = C *(exp(-k*x)) + intercept)
        if(any(!is.na(y) & y<coef(Model)[3] ) | any(!is.na(y) & y>coef(Model)[1] + coef(Model)[3])) { # in case value out of bound and value is not NA
            print("Some Y value out of limits of the model", quote = FALSE)
        }
        x <- log((y - coef(Model)[3])/coef(Model)[1])/-coef(Model)[2]
        return(x)
        
    } else if(Mod_type=="Sigmoid") {
        if(any(!is.na(y) & y<coef(Model)[1]) | any(!is.na(y) & y > coef(Model)[2])) { # in case value out of bound and value is not NA
            print("Some Y value out of the limits of the model", quote = FALSE)
        }
        #  return(y = MIN + (MAX-MIN) / (1 + (X50/x)^Hill))
        x <- coef(Model)[3]*((y-coef(Model)[1])/(coef(Model)[2]-y))^(1/coef(Model)[4])
        return(x)
        
    } else if(Mod_type=="Logarithmic") {
        if(y<coef(Model)[1] )  {
            #  return(y = MIN + (MAX-MIN) / (1 + (X50/x)^Hill))
            x <- exp((y-coef(Model)[1])/coef(Model)[2])
            return(x)
        } else {
            print("y values out of limits", quote = FALSE)
        }
    }
}

#================================================================CR
### MeasLinear: Function Measurement Function x = f(y) once the Linear Calibration function (y = f(x) of sensor is established e.g with Cal_Line (Vs 141114)
#================================================================CR
MeasLinear <- function(x, ModLinear) {
    (x-coef(ModLinear)[1])/coef(ModLinear)[2]
}

#================================================================CR
### MeasParab: Function Measurement Function x = f(y) once the Quadratic Calibration function (y = f(x) of sensor is established e.g with Cal_Line (Vs 141114)
#================================================================CR
MeasParab <- function(x, ModParab) {
    if(coef(ModParab)[2] > 0){
        return((-coef(ModParab)[2]+sqrt(coef(ModParab)[2]^2-4*coef(ModParab)[3]*(coef(ModParab)[1]-x)))/(2*coef(ModParab)[3]))
    } else {
        return((-coef(ModParab)[2]-sqrt(coef(ModParab)[2]^2-4*coef(ModParab)[3]*(coef(ModParab)[1]-x)))/(2*coef(ModParab)[3]))
    }
}

#================================================================CR
### MeasSigmoid: Function Measurement Function x = f(y) once the Sigmoidal Calibration function (y = f(x) of sensor is established e.g with Cal_Line (Vs 141114)
#================================================================CR
MeasSigmoid <- function(x, ModSigmoid) {
    (log((coef(ModSigmoid)[1]-coef(ModSigmoid)[4])/(x-coef(ModSigmoid)[4])-1)/(-coef(ModSigmoid)[2])+coef(ModSigmoid)[3])
}

#================================================================CR
### Adjust: Function Measurement Function using reverse calibration (Vs 141114)
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
### Function Significant numbers (Vs 141114)
#================================================================CR
sigdigss<-function(n) {
    i <- 0
    # Check for decimal point is present
    if(length(grep("\\.", numstr[n])) > 0) { # real number
        # Separate integer and fractional parts
        intfrac <- unlist(strsplit(numstr[n], "\\."))
        digstring <- paste(intfrac[1], intfrac[2], sep = "")
        numfigs <- nchar(digstring)
        while(i < numfigs) {
            # Find index of 1st non-zero digit from LEFT
            if(substr(digstring,i+1,i+1) == "0") {
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
        while(i < numfigs) {
            # Find index of 1st non-zero digit from RIGHT
            if(substr(digstring, numfigs-i, numfigs-i) == "0") {
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
polyfit <- function (XY, i) {
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
Table_Reference <- function(df){
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
Table_AllEffects <- function(df){
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
    table_data[,"Intercept"   ] <- paste0(formattable(mm.means$Intercept, digits=5, format = "f"), " ? "
                                          , formattable(mm.SD$Intercept, digits=5, format = "f"))
    table_data[,"P_Intercept"]  <- formattable(mm.means$Pr_Intercept    , digits=3, format = "f")
    table_data[,"Slope"       ] <- paste0(formattable(mm.means$Slope    , digits=8, format = "f"), " ? "
                                          , formattable(mm.SD$Slope    , digits=8, format = "f"))
    table_data[,"P_Slope"    ]  <- formattable(mm.means$Pr_Slope        , digits=3, format = "f")
    table_data[,"R.sq"        ] <- formattable(mm.means$R2              , digits=4, format = "f")
    table_data[,"AjustR.sq"   ] <- formattable(mm.means$Adj.R2          , digits=4, format = "f")
    table_data[,"Sigma"       ] <- formattable(mm.means$Sigma           , digits=5, format = "f")
    table_data[,"u_lof"      ]  <- formattable(mm.means$u_lof           , digits=1, format = "f")
    table_data <- table_data[with(table_data, order(SensorTypes, Compounds)),]
    colnames(table_data)        <- c("Sensors","Compounds", "Interc.", "P(Interc.)", "Slope", "P(Slope)","R2","Ajust. R2","RMSE","u(lof)")
    # add units if they are in the table caption: table_data[1,] <- c("", "", ", V", "", ", V/ppb or v/ppm", "", "", "",", V/ppb or v/ppm", "ppb or ppm")
    row.names(table_data) <- NULL
    table_data <- table_data[,-which(colnames(table_data) == "Ajust. R2")]
    
    return(table_data)
}
Table_1Effect <- function(df,Sensor_type,Compounds, Outliers, Table_Number){
    df <- df[which(df$Sensor_type == Sensor_type & df$Compounds == Compounds),]
    df <- df[c(1,3,4,5,6,7,8,9,10,2),]
    Sensor_names <- df[,1]
    df <- df[,c("Intercept", "s_Intercept", "Pr_Intercept", "Slope", "s_Slope", "Pr_Slope" , "R2", "Adj.R2", "Sigma", "u_lof" )]
    #Numbers.df <- apply(Numbers.df,c(1,2),as.numeric)
    #rbind(Numbers.df,colMeans(Numbers.df),SD(Numbers.df))
    if(is.null(Outliers)) df.means <- colMeans(df, na.rm = TRUE) else df.means <- colMeans(df[-which(Sensor_names %in% Outliers),], na.rm = TRUE)
    if(is.null(Outliers)) df.SD    <- SD(df      , na.rm = TRUE) else df.SD    <- SD(      df[-which(Sensor_names %in% Outliers),], na.rm = TRUE)
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
f_lnorm_2Par <- function(x, mu, sigma){
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
    if (min(d_Air_Pol$x)<-10000) {
        Air_Pol_final <- Air_Pol - min(d_Air_Pol$x) + 0.0001
        print(sprintf("minmum x_density: %.3f was subtracted",min(d_Air_Pol$x)))
        d_Air_Pol<- density(Air_Pol_final, bw = h, adjust = 1, kernel = "gaussian", na.rm = TRUE) # returns the density data 
        print(sprintf("minmum x_density: %.3f",min(d_Air_Pol$x)))
    }
    else {
        print(sprintf("minmum x_density: %.1f was not added",min(d_Air_Pol$x)))
        Air_Pol_final <- Air_Pol # do not add anything becasue the minimum value is not negative
    }
    
    #-----------------------------------CR
    ## density plot of d_Air_Pol - one parameter - reference density plot
    #-----------------------------------CR
    par(mar=c(5, 0.5, 0, 0.5))
    plot(d_Air_Pol, xlim = c(min(0,d_Air_Pol$x),quantile(Air_Pol, probs = 0.98)), xlab = Dist_xlab, ylab = "", main ="", cex.lab=3, cex.axis=2.5)
    polygon(c(min(d_Air_Pol$x),d_Air_Pol$x,quantile(Air_Pol, probs = 0.98)), c(0,d_Air_Pol$y,0), col="red", border="blue")
    
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
    plot(x,y, xlim = c(min(0,x,na.rm = T),max(x,na.rm = T)), xlab = Dist_xlab, ylab = "", main ="", type = "l", cex.lab=3, cex.axis=2.5)
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
    #           , xlab = Dist_xlab, ylab =""
    #           , main ="",cex.lab=3, cex.axis=2.5)
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
    #          , type ="l"
    #          , xlim = c(min(0,d_Air_Pol$x),quantile(Air_Pol, probs = 0.98)) 
    #          , ylim = Ylim
    #          , xlab = Dist_xlab, ylab =""
    #          , main ="",cex.lab=3, cex.axis=2.5) 
    #     polygon(c(min(DataXY$x_to_fit),DataXY$x_to_fit,max(DataXY$x_to_fit)), c(0,DataXY$referenceY,0), col="red", border="blue")
    #     lines(DataXY$x_to_fit, DataXY$Fitted , 
    #           xlim = Dist_range, 
    #           ylim = c(min(0,d_Air_Pol$x),quantile(Air_Pol, probs = 0.98)), 
    #           xlab = Dist_xlab, 
    #           ylab ="", 
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
    histogram = hist(Air_Pol, breaks = bins, xlim =c(mini,maxi),xlab = Dist_xlab, ylab = "", main ="", cex.lab=3, cex.axis=2.5)
    # histogram <- subset(histogram, histogram$mids>0) # removing the negative values
    
    
    ## density plot for the 2 parameters lognormal distribution
    Log_Air_Pol <- log(Air_Pol)
    mu <- mean(Log_Air_Pol)
    sigma <- sd(Log_Air_Pol)
    x <- seq( min(Air_Pol), max(Air_Pol), length.out=250)
    y <- f_lnorm_2Par(x,mu,sigma)
    par(mar=c(5, 2, 0, 0))
    plot(x, y, xlim = c(min(0,x),max(x)), xlab = Dist_xlab, ylab = "", main ="", type = "l", cex.lab=3, cex.axis=2.5)
    polygon(c(min(x),x,max(x)), c(0,y,0), col="red", border="blue")
    
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
             , type ="l"
             , xlim = Dist_range, ylim = c(0, max(DataXY$referenceY,DataXY$Fitted))
             , xlab = Dist_xlab, ylab =""
             , main ="",cex.lab=3, cex.axis=2.5) 
        lines(DataXY$x_to_fit, DataXY$Fitted , xlim = Dist_range, ylim = c(0, max(DataXY$referenceY,DataXY$Fitted)), xlab = Dist_xlab, ylab =""
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
    if(baseFolder == "Finder"){
        baseFolder = system(intern = T, "osascript -e 'tell application \"Finder\" to get the POSIX path of (target of front window as alias)'")
        message("Using front-most Finder window:", baseFolder)
    } else if(baseFolder == "") {
        baseFolder = paste(dirname(file.choose(new = FALSE)), "/", sep = "") ## choose a directory
        message("Using selected folder:", baseFolder)
    }
    if(is.na(listPattern)){
        listPattern = findStr
    }
    a = list.files(baseFolder, pattern = listPattern)
    changed = 0
    if(!Missing){ # it is a replacement not a missing String
        message("found ", length(a), " possible files")
        for (fn in a) {
            findB = grepl(pattern = findStr, fn) # returns 1 if found
            cat(strsplit(fn, split = "_")[[1]][2], sep = "\n")
            if(findB){
                fnew = gsub(findStr, replace = replaceStr, fn) # replace all instances
                if(test){
                    message("would change ", fn, " to ", fnew)  
                } else {
                    if((!overwrite) & file.exists(paste(baseFolder, fnew, sep = ""))){
                        message("renaming ", fn, " to ", fnew, "failed as already exists. To overwrite set T")
                    } else {
                        file.rename(file.path(baseFolder, fn), file.path(baseFolder, fnew))
                        changed = changed + 1;
                        message("renamed ", fn, " to ", fnew, ".")
                    }
                }
            }else{
                if(test){
                    message(paste("bad file",fn))
                }
            }
        }    
    }  else { # it is a missing String added at position 2 splitted with Split of the file name
        a = a[grep(pattern = replaceStr, x = a, invert = TRUE)]
        message("found ", length(a), " possible files")
        for(fn in a) {
            #cat(strsplit(fn, split = "_")[[1]][2], sep = "\n")
            fnew = gsub(pattern = strsplit(fn, split = "_")[[1]][1], replace = paste0(strsplit(fn, split = "_")[[1]][1],"_",replaceStr), fn) # replace all instances
            if(test){
                message("would change ", fn, " to ", fnew)  
            } else {
                if((!overwrite) & file.exists(paste(baseFolder, fnew, sep = ""))){
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
loadRData <- function(fileName){
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
