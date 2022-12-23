##########################################################################
################## Econometric Analysis of Time-Series Data ##############
##########################################################################

##########################################################
################## Description ###########################
##########################################################

  # This group project performs an econometric analysis of time-series data for a sample dataset. The aim is to 
  # learn how to inspact time-series data for stationarity and to provide two examples how non-stationary 
  # time-series can be transformed into stationary ones, which is necessary in econometrics before being able to 
  # make any meaningful statistical inferences.

  # The project includes the following statistical analyses:
  # - Visual inspection of time-series
  # - ACF/PACF plots
  # - Augmented Dickey-Fuller tests
  # - ARMA(p,q) model selection with AIC and BIC
  # - Coefficient tests
  # - Ljung-Box test
  # - Likelihood ratio tests

  # The code is well commented as my aim was that anyone who finds my code on GitHub would be able to understand, 
  # modify and use it. It is split into a lot of sections which can be collapsed. Experience R coders can therefore 
  # easily skip sections that are not relevant for them.

  # The project covers many of the skills taught in class such as loading data, modifying it, visualizing data, 
  # fitting statistical models and many more. However, in many parts, the project goes far beyond what was taught in 
  # class. Especially, this project requires solid pre-existing knowledge in econometrics and time-series analysis. 
  # If you want to brush up you knowledge in any of these areas, please consult the material in the references 
  # section. Or, if you're lazy, have a quick into Ben Lambert's introduction to ARMA(1,1) models video on YouTube: 
  # https://www.youtube.com/watch?v=Pg0RnP1uLVc

##########################################################
################## How to run the code ###################
##########################################################

  # Running the code is very simple. Just perform the following steps:
  #   1) Download the "Econometric Analysis of Time-Series Data.R" file to the desired destination on your computer
  #   2) Download the "Data.csv" file to the desired destination on your computer
  #   3) Open the "Econometric Analysis of Time-Series Data.R" and just start running the code, no adjustment to 
  #      any line of code should be needed

##########################################################
################## Clean R ###############################
##########################################################

  # I follow a general routine before any session, which mainly includes loading the libraries necessary and
  # setting my working directory

  # The getwd function shows your current working directory
  getwd()

  # This function lets you set your working directory to wherever you're saving this R script
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  
  # We create a vector with some packages that we will need for this project
  pkgs <- c("AER", "dynlm", "forecast", "readxl", "stargazer", "scales", "quantmod", "tidyverse", "urca", "sandwich", "tseries", "astsa", "stats")
  
  # We create a loop which will go through each of the packages
  for(pkg in pkgs){
    
    # In case you have never used a certain package, you first need to install it. To do that, simply remove the
    # hashtag in front of the line of code below before you run it
        # install.packages(pkg,character.only = TRUE)
    
    # We loop through each package and install it so we can later use it
    library(pkg, 
            # Make sure that only the characters from the packages vector are selected for installing
            character.only = T)}
  
  # Finally, we use this function to clean the global environment before really getting started
  rm(list=ls())
  
##########################################################
################## Load data #############################
##########################################################
  
  # We load the dataset which I uploaded on GitHub by using the read.csv(function)
  data <- read.csv("Data.csv", 
                   # We want to keep our headers
                   header = T,
                   # And specify the separator which is different from our 
                   sep = ";")
  
  # We get a brief overview of the data
  summary(data)
  # >>> We notice that the values for the "date" column are incorrectly formatted
  
  # We reformat the date column
  data$date <- as.Date(data$date, tryFormats = c("%Y-%m-%d"))
  
  # We double check and see that the formatting is now correct
  class(data$date)
  
##########################################################
################## Series 1 ##############################
##########################################################
  
###############################
##### Visual inspection #######
###############################
  
  # We plot series 1 using a line chart. We use the pipe operator %>% to reference our data for following commands
  data %>%
    # We use the ggplot2 package for all  visualizations and first specify what is plotted
               # Display the date on the x axis
    ggplot(aes(x = date,
               # And the series' values on the y axis
               y = series1))  +
    # Then, we specify which graph we want, in this case a line chart
    geom_line() +
    # We add a line for the mean, which helps us with our visual inspection
    geom_hline(yintercept = mean(data$series1), 
               # Color the line in blue
               color="blue", 
               # Specify a dashed line
               linetype="dashed", 
               # And select a feasible size
               size=0.5) +
    # Then, we add some simple labels to the plot to make it look nice. We first specify the x axis as "Date"
    labs(x = "Date",
         # The y axis as "Value"
         y = "Value",
         # The corresponding title
         title = "Series 1 line chart",
         # And the caption for the class
         caption = "Group Project: Programming with Advanced Computer Languages")  +
    # Finally, we select one of the ggplot2 themes that I like
    theme_light() 
  
  # >>> We can easily see that the time-series is non-stationary and that we should thus introduce some type of
  #     ARMA model. Neither the mean or covariance of the distribution is constant.
  
  ###############################
  ##### ACF/PACF plots ##########
  ###############################
  
  # Next, we plot the acf and pacf plots of series 1. 
  # If you are not familiar with these two plots, please checkout: https://towardsdatascience.com/identifying-ar-and-ma-terms-using-acf-and-pacf-plots-in-time-series-forecasting-ccb9fd073db8
  # The ACF plot is a bar chart of coefficients of correlation between a time series and it lagged values.
  # The PACF plot is the partial autocorrelation function that explains the partial correlation between the series
  # and lags itself.
  
  # The acf2 function gives us both plots in one
  acf2(data$series1) 
  
  # >>> The ACF plot displays positive and significant spikes in the first three periods. The PACF plot displays an
  #     oscillating decay. Hence, we seem to have an MA(3) model. If you're unfamiliar with interpreting P/ACF plots,
  #     please consult table 2.1 in Enders (2008) in the references section
  
  ###############################
  ##### Unit root check #########
  ###############################
  
  # We quickly do an augmented Dickey-Fuller test, to make sure the process is not driven by a unit root, 
  adf.test(data$series1, alternative = "stationary")
  
  # >>> The p-value is 0.01, so we do not have a unit root.
  
  ###############################
  ##### Find best ARMA model ####
  ###############################
  
  # After the visual inspection and the inspection of the P/ACF plots hinted towards an MA(3) model, we now want to 
  # statistically find the best possible ARMA model. Thus, we run ARMA(p,q) models, for all combinations from AR(1),
  # MA(1) to ARMA(5,5). We do this with the help of the Akaike information criteria (AIC) and the Bayes Information
  # criteria (BIC). Using the commands AIC() and BIC(), we will store the information criteria in an object.
  
  ###############################
  ##### AIC #####################
  ###############################
  
  # We first create an empty 6 by 6 matrix
  AICmatrix_series1 = matrix(NA,6,6)
  
  # We create a loop for our AR components
  for (i in 0:5){
    # As well as for our MA components
    for (j in 0:5){
      # We specify an ARMA model for our series 2
      model = arima(x = data$series1,
                    # With the orders that we will loop through
                    order = c(i,0,j),
                    # We include the mean
                    include.mean = T, 
                    # And specify the iteration limit maxit to 10k as the list of control parameters
                    optim.control = list(maxit = 1000)
                    )
      # We store the AIC of each model in the empty 6 by 6 matrix
      AICmatrix_series1[i+1,j+1]<-AIC(model)
    }
  }
  
  # The value in the first row and first column essentially denotes the AIC of an ARMA(0,0) model, which makes no
  # sense so we set it back to NA
  AICmatrix_series1[1,1] = NA
  
  # We create a vector which will denote the orders of the final model
  pq <- as.vector(
          # We choose from the following values, which are...
          which(
            # ... converted to a matrix, the smallest value of the AIC
            as.matrix(AICmatrix_series1) == min(as.matrix(AICmatrix_series1), 
                    # Remove missing values                            
                    na.rm = T), 
                    # Specify that array indices should be returned
                    arr.ind = T))
  
  # As a final step, we have to subtract 1 from both values of our vector because we are working with a 6 by 6
  # matrix and the first row and column signify an AR(0) and MA(0) component, respectively. We store these final
  # values of the ARMA(p.q) in a vector called order
  order_series1_AIC <- pq-1
  
  # We print the p,q orders of the best possible ARMA model according to the AIC
  order_series1_AIC 

  # >>> We see that, indeed, the MA(3) model is our best possible model.

###############################
##### BIC #####################
###############################
  
  # We first create an empty 6 by 6 matrix
  BICmatrix_series1 = matrix(NA,6,6)
  
  # We create a loop for our AR components
  for (i in 0:5){
    # As well as for our MA components
    for (j in 0:5){
      # We specify an ARMA model for our series 2
      model = arima(x = data$series1,
                    # With the orders that we will loop through
                    order = c(i,0,j),
                    # We include the mean
                    include.mean = T, 
                    # And specify the iteration limit maxit to 10k as the list of control parameters
                    optim.control = list(maxit = 1000)
      )
      # We store the BIC of each model in the empty 6 by 6 matrix
      BICmatrix_series1[i+1,j+1]<-BIC(model)
    }
  }
  
  # The value in the first row and first column essentially denotes the BIC of an ARMA(0,0) model, which makes no
  # sense so we set it back to NA
  BICmatrix_series1[1,1] = NA
  
  # We create a vector which will denote the orders of the final model
  pq <- as.vector(
    # We choose from the following values, which are...
    which(
      # ... converted to a matrix, the smallest value of the BIC
      as.matrix(BICmatrix_series1) == min(as.matrix(BICmatrix_series1), 
                                          # Remove missing values                            
                                          na.rm = T), 
      # Specify that array indices should be returned
      arr.ind = T))
  
  # As a final step, we have to subtract 1 from both values of our vector because we are working with a 6 by 6
  # matrix and the first row and column signify an AR(0) and MA(0) component, respectively. We store these final
  # values of the ARMA(p.q) in a vector called order
  order_series1_BIC <- pq-1
  
  # We print the p,q orders of the best possible ARMA model according to the BIC
  order_series1_BIC 
  
  # >>> We see that, indeed, the MA(3) model is our best possible model.
  
###############################
##### AIC vs. BIC #############
###############################
  
  # We display the p,q coeffiecients for the best possible model according to the AIC
  order_series1_AIC
  # As well as according to the BIC
  order_series1_BIC
  
  # >>> We see that, both information criteria suggest the same model.
  
###############################
##### Coefficient tests #######
###############################
  
  # Next, we run coefficient tests for all possible AR and MA models, to verify further that we picked the right
  # model.
  
  # AR(1)
  coeftest(arima(data$series1,order=c(1,0,0)))
  # AR(2)
  coeftest(arima(data$series1,order=c(2,0,0)))
  # AR(3)
  coeftest(arima(data$series1,order=c(3,0,0)))
  # AR(4)
  coeftest(arima(data$series1,order=c(4,0,0)))
  # AR(5)
  coeftest(arima(data$series1,order=c(5,0,0)))
  # MA(1)
  coeftest(arima(data$series1,order=c(0,0,1)))
  # MA(2)
  coeftest(arima(data$series1,order=c(0,0,2)))
  # MA(3)
  coeftest(arima(data$series1,order=c(0,0,3)))
  # MA(4)
  coeftest(arima(data$series1,order=c(0,0,4)))
  # MA(5)
  coeftest(arima(data$series1,order=c(0,0,5)))

  # >>> We see from the coefficient tests that indeed, an MA(3) model is the only consistent model for series 2.
  
###############################
##### Residuals ###############
###############################
  
  # Finally, we test if our series can now be considered stationary by inspecting the model's residuals. In order
  # to do so, we run a Ljung-Box test 
  
  # We store the MA(3) model in a separate list
  best_model_series1 <- arima(data$series1, 
                              # Specify the MA(3) component
                              order = c(0,0,3))
  
  # We run a Ljung-Box test on our best possible model
  Box.test(resid(best_model_series1),
           # Set the lag length to 3 as we have an MA(3) model
           lag = 3, 
           # Specify Ljung-Box as the type of the test
           type = "Ljung-Box") 
  
  # >>> The p-value of the test is 0.76 and our H0 is that there is no autocorrelation in the residuals. This means 
  #     that our MA(3) model captures all the informative parameters in the series. The residuals are thus White 
  #     Noise, which means that they are uncorrelated with each other and have a constant and homoscedastic variance. 
  #     In turn, we now that our MA(3) model can be considered stationary and we could now go along with making 
  #     statistical inferences.

###############################
##### Conclusion ##############
###############################
  
  # >>> By finding the best possible ARMA model, we now have a stationary series that we can use for further 
  #     statistical inferences. If you don't understand why stationarity is so important in econometrics, please
  #     consult the material in the references section.
  
  # If you've been wondering where the data we used actually came from, the answer is very simple: I just simulated
  # it directly in R and exported it as a csv file. You can simulate your own time-series using the arima.sim
  # command
  
##########################################################
################## Series 2 ##############################
##########################################################
  
###############################
##### Visual inspection #######
###############################
  
  # We plot series 2 using a line chart. We use the pipe operator %>% to reference our data for following commands
  data %>%
    # We use the ggplot2 package for all  visualizations and first specify what is plotted
    # Display the date on the x axis
    ggplot(aes(x = date,
               # And the series' values on the y axis
               y = series2))  +
    # Then, we specify which graph we want, in this case a line chart
    geom_line() +
    # We add a line for the mean, which helps us with our visual inspection
    geom_hline(yintercept = mean(data$series2), 
               # Color the line in blue
               color="blue", 
               # Specify a dashed line
               linetype="dashed", 
               # And select a feasible size
               size=0.5) +
    # Then, we add some simple labels to the plot to make it look nice. We first specify the x axis as "Date"
    labs(x = "Date",
         # The y axis as "Value"
         y = "Value",
         # The corresponding title
         title = "Series 2 line chart",
         # And the caption for the class
         caption = "Group Project: Programming with Advanced Computer Languages")  +
    # Finally, we select one of the ggplot2 themes that I like
    theme_light() 
  
  # >>> We can easily see that the time-series is non-stationary and that we should thus introduce some type of
  #     ARMA model. Neither the mean or covariance of the distribution is constant.
  
###############################
##### ACF/PACF plots ##########
###############################
  
  # Next, we plot the acf and pacf plots of series 2. 
  # If you are not familiar with these two plots, please checkout: https://towardsdatascience.com/identifying-ar-and-ma-terms-using-acf-and-pacf-plots-in-time-series-forecasting-ccb9fd073db8
  # The ACF plot is a bar chart of coefficients of correlation between a time series and it lagged values.
  # The PACF plot is the partial autocorrelation function that explains the partial correlation between the series
  # and lags itself.
  
  # The acf2 function gives us both plots in one
  acf2(data$series2) 
  
  # >>> For series 2, it is more difficult to identify to correct model from the P/ACF plots. We see an oscillating
  #     decay in the PACF plot, but no easily identifiable pattern in the ACF plot. It seems like we have an ARMA
  #     model, i.e. a modle with both an AR and an MA component. However, we will need further analysis to define
  #     the true orders of the model
  
###############################
##### Unit root check #########
###############################
  
  # We quickly do an augmented Dickey-Fuller test, to make sure the process is not driven by a unit root, 
  adf.test(data$series2, alternative = "stationary")
  
  # >>> The p-value is 0.01, so we do not have a unit root.
  
###############################
##### Find best ARMA model ####
###############################
  
  # After the visual inspection and the inspection of the P/ACF plots hinted towards an MA(3) model, we now want to 
  # statistically find the best possible ARMA model. Thus, we run ARMA(p,q) models, for all combinations from AR(1),
  # MA(1) to ARMA(5,5). We do this with the help of the Akaike information criteria (AIC) and the Bayes Information
  # criteria (BIC). Using the commands AIC() and BIC(), we will store the information criteria in an object.
  
###############################
##### AIC #####################
###############################
  
  # We first create an empty 6 by 6 matrix
  AICmatrix_series2 = matrix(NA,6,6)
  
  # We create a loop for our AR components
  for (i in 0:5){
    # As well as for our MA components
    for (j in 0:5){
      # We specify an ARMA model for our series 2
      model = arima(x = data$series2,
                    # With the orders that we will loop through
                    order = c(i,0,j),
                    # We include the mean
                    include.mean = T, 
                    # And specify the iteration limit maxit to 10k as the list of control parameters
                    optim.control = list(maxit = 1000)
      )
      # We store the AIC of each model in the empty 6 by 6 matrix
      AICmatrix_series2[i+1,j+1]<-AIC(model)
    }
  }
  
  # The value in the first row and first column essentially denotes the AIC of an ARMA(0,0) model, which makes no
  # sense so we set it back to NA
  AICmatrix_series2[1,1] = NA
  
  # We create a vector which will denote the orders of the final model
  pq <- as.vector(
    # We choose from the following values, which are...
    which(
      # ... converted to a matrix, the smallest value of the AIC
      as.matrix(AICmatrix_series2) == min(as.matrix(AICmatrix_series2), 
                                          # Remove missing values                            
                                          na.rm = T), 
      # Specify that array indices should be returned
      arr.ind = T))
  
  # As a final step, we have to subtract 1 from both values of our vector because we are working with a 6 by 6
  # matrix and the first row and column signify an AR(0) and MA(0) component, respectively. We store these final
  # values of the ARMA(p.q) in a vector called order
  order_series2_AIC <- pq-1
  
  # We print the p,q orders of the best possible ARMA model according to the AIC
  order_series2_AIC 
  
  # >>> We see that according to the AIC, an ARMA(3,5) is the best model.
  
###############################
##### BIC #####################
###############################
  
  # We first create an empty 6 by 6 matrix
  BICmatrix_series2 = matrix(NA,6,6)
  
  # We create a loop for our AR components
  for (i in 0:5){
    # As well as for our MA components
    for (j in 0:5){
      # We specify an ARMA model for our series 2
      model = arima(x = data$series2,
                    # With the orders that we will loop through
                    order = c(i,0,j),
                    # We include the mean
                    include.mean = T, 
                    # And specify the iteration limit maxit to 10k as the list of control parameters
                    optim.control = list(maxit = 1000)
      )
      # We store the BIC of each model in the empty 6 by 6 matrix
      BICmatrix_series2[i+1,j+1]<-BIC(model)
    }
  }
  
  # The value in the first row and first column essentially denotes the BIC of an ARMA(0,0) model, which makes no
  # sense so we set it back to NA
  BICmatrix_series2[1,1] = NA
  
  # We create a vector which will denote the orders of the final model
  pq <- as.vector(
    # We choose from the following values, which are...
    which(
      # ... converted to a matrix, the smallest value of the BIC
      as.matrix(BICmatrix_series2) == min(as.matrix(BICmatrix_series2), 
                                          # Remove missing values                            
                                          na.rm = T), 
      # Specify that array indices should be returned
      arr.ind = T))
  
  # As a final step, we have to subtract 1 from both values of our vector because we are working with a 6 by 6
  # matrix and the first row and column signify an AR(0) and MA(0) component, respectively. We store these final
  # values of the ARMA(p.q) in a vector called order
  order_series2_BIC <- pq-1
  
  # We print the p,q orders of the best possible ARMA model according to the BIC
  order_series2_BIC 
  
  # >>> We see that according to the BIC, an ARMA(2,2) is the best model.
  
###############################
##### AIC vs. BIC #############
###############################
  
  # We display the p,q coeffiecients for the best possible model according to the AIC
  order_series2_AIC
  # As well as according to the BIC
  order_series2_BIC
  
  # >>> This time, they do not suggest the same model. While the AIC suggests an ARMA(3,5) model, the BIC suggests
  #     an ARMA(2,2) model. Hence, we will need further statistical tests to select the very best model.
  
###############################
##### Coefficient tests #######
###############################
  
  # Next, we run coefficient tests for both possible models, to see if the coefficients are significant.
  
  # ARMA(3,5)
  coeftest(arima(data$series2,order=c(3,0,5)))
  # ARMA(2,2)
  coeftest(arima(data$series2,order=c(2,0,2)))
  
  # >>> We see from the coefficient tests that the third lag of the MA component in the ARMA (3,5) model is 
  #     insignificant. Also, some of the other components in this model are significant only at the 5% or even 10%
  #     levels, different from the ARMA(2,2) model where each component is highly significant. We keep this
  #     observation in mind but will try to deliver further arguments which allows us to clear choose one model
  #     over the other.
  
###############################
##### Residuals ###############
###############################
  
  # Next, we test if both our models  can now be considered stationary by inspecting the model's residuals. In order
  # to do so, we run Ljung-Box tests
  
  # We store the ARMA(3,5) model in a separate list
  arma35_model_series2 <- arima(data$series2, 
                              # Specify the AR(3) and the MA(5) component
                              order = c(3,0,5))
  
  # We run a Ljung-Box test on the ARMA (3,5) model
  Box.test(resid(arma35_model_series2),
           # Set the lag length to 5 as we have an MA(5) model
           lag = 5, 
           # Specify Ljung-Box as the type of the test
           type = "Ljung-Box") 
  
  # >>> The p-value of the test is 0.99 and our H0 is that there is no autocorrelation in the residuals. This means 
  #     that our ARMA(3,5) model captures all the informative parameters in the series. The residuals are thus White 
  #     Noise, which means that they are uncorrelated with each other and have a constant and homoscedastic variance. 
  #     In turn, we now that our ARMA(3,5) model can be considered stationary
  
  # We store the ARMA(2,2) model in a separate list
  arma22_model_series2 <- arima(data$series2, 
                                # Specify the AR(2) and the MA(2) component
                                order = c(2,0,2))
  
  # We run a Ljung-Box test on the ARMA (2,2) model
  Box.test(resid(arma22_model_series2),
           # Set the lag length to 2 as we have an MA(2) model
           lag = 2, 
           # Specify Ljung-Box as the type of the test
           type = "Ljung-Box") 
  
  # >>> The p-value of the test is 0.95, so exactly the same conclusion applies to the ARMA(2,2) model, which we
  #     now know can also be considered stationary.
  
  # >>> As both possible mdoels pass the Ljung-Box test, we will need further tests to identify the truly best model.
 
###############################
##### Likelihood ratio ########
############################### 

  # As a final test available, we now run a likelihood ratio test to see whether the sparse ARMA(2,2) model (with 
  # less parameters) gets a log-likelihood which is indistinguishable from the more complicated ARMA(3,5) model. 
  # If this is the case, we might as well take the model with less parameters.
  
  # If your unfamiliar with the likelihood ratio equation, please consult the material in the references section.
  # We use the loglik arguments from our previously stored models to get their difference, multiply it with -2 and
  # store it in a new argument LR
  LR = -2*(arma22_model_series2$loglik - arma35_model_series2$loglik)
  # We use the new argument LR to get its Chi-squared distributed probability and subtract that value from 1
  LR.arma = (1-pchisq(LR, 
                      # We specify the degrees of freedom to 4 in order to fitour models
                      df = 4))
  # We print the obtained value
  LR.arma
  
  # >>> We get a p-value of 0.0813, so we fail to reject H0, which states that the difference in likelihood ratios
  #     between the two models is 0 (due to sampling error). Hence, we rather take the simpler, less restricted
  #     ARMA(2,2) model for further statistical inferences. 
  
###############################
##### Conclusion ##############
###############################
  
  # >>> By comparing the two different ARMA models suggested by the AIC and the BIC and running further tets, we 
  #     found the very best possible ARMA model. We now have a stationary series that we can use for further 
  #     statistical inferences. If you don't understand why stationarity is so important in econometrics, please
  #     consult the material in the references section.
  
  # If you've been wondering where the data we used actually came from, the answer is very simple: I just simulated
  # it directly in R and exported it as a csv file. You can simulate your own time-series using the arima.sim
  # command

##########################################################
################## References ############################
##########################################################

  # 1) Enders, W. (2008). Applied econometric time series. John Wiley & Sons.
  # 2) Ranaldo, A. (2022). Seminar "7,160: Quantitative Methods (MBF)". Chapter "Introduction to time series analysis" [Slide deck].
  # 3) Stock, J. H., & Watson, M. W. (2020). Introduction to econometrics 4th ed.
  # 4) Wooldridge, J. M. (2015). Introductory econometrics: A modern approach. Cengage learning.
  