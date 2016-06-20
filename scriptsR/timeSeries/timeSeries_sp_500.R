#######
#######
#######
#######
#######
#######
#######
####### Time Series Model for S&P 500
#######
#######
#######
#######
#######
#######
#######



##
####
######
#     LOADING DATA, PACKAGES
######
####
##
print("Time Series Model for S&P 500")
# Add the directory path of data_1/data_master_1.csv file
wd <- getwd()
setwd("../../")
parent <- getwd()
setwd(wd)
print(parent)
# Load the data file
dataMaster <- read.csv(file.path(parent, "data_1/data_master_1.csv"))


attach(dataMaster)


# here is a snapshot of our variables
# tons of financial data:

# m1
# m2
# consumerSentiment
# inflation
# imports
# oilPrices
# ppi
# exports
# cpi
# unemploymentRate
# fedFunds
# capUtilization
# sp_500Dividends
# nasdaq
# nyse
# sp_500
# gdp_us
# housingIndex

# install.packages() the following packages, run this on the terminal

# ggplot2
# forecast
# astsa
# car
# MTS
# plm


# install.packages("ggplot2")
# install.packages("forecast")
# install.packages("astsa")
# install.packages("car")
# install.packages("MTS")

# load the packages
require(data.table)
require(ggplot2)
require(forecast)
require(astsa)
require(car)
library(caret)
require(plotly)

# outputting work

pdf("timeSeries_sp_500.pdf")


###########################################################
print(" ")
print(" ")
print(" ")
###########################################################

##
####
######
#     TESTING that data loads properly
######
####
##

head(dataMaster)
str(dataMaster)

# assigning data column to variable container

gdp_us_nominal <- dataMaster$gdp_us
gdp_us_nominal

# printing string and mean of gdp_us_nominal

mean(gdp_us_nominal)


###########################################################
print(" ")
print(" ")
print(" ")
###########################################################

##
####
######
#     CREATING TIME-SERIES OBJECTS WITH FINANCIAL DATA 
######
####
##


sp_500 <- ts(dataMaster$sp_500, start=c(1995, 1), freq=12)


###########################################################
print(" ")
print(" ")
print(" ")
###########################################################



print(" ")
print(" ")
print(" ")

##
####
######
#     TIME SERIES STUFF
######
####
##

sp_500
# First we plot the time series plot to get an understanding of the necessary modeling
plot.ts(sp_500)
acf2(sp_500)
# Next we created some plots to get a "feel" for our data 
# This next plot takes a closer look at the seasonal components of our time series
seasonplot(sp_500,ylab="S&P Closing Values", xlab="Year",
           main="Seasonal plot: S&P Monthly Closing Values",
           year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19)

# the plot following plot decomposes the time series into its seasonal, trend and irregular components!
plot(stl(sp_500, s.window = "periodic"), main = "Decomposition for S&P 500")

# Notice that this plot is not stationary so an appropriate transformation must be made
# The variability can not be seen at first glance but one the transformation is made, we can see if the model
# is heteroskedastic

# Once we plot the newly transformed data we see that it is now weakly stationary
# Upon inspection of the ACF and PACF plot we can deduce the model we will be fitting will
# be a combination of the AR and MA models
# Using the auto.arima function to output a model we get as follows:

auto.arima(sp_500)
for_sp500_ts <- forecast(auto.arima(sp_500))
plot(for_sp500_ts) 

# CSV file for residuals 
resARIMA <- for_sp500_ts$residuals
write.csv(resARIMA, file = "Residuals.csv")
# Here we plot the histogram along with the line that best fit!
hist(resARIMA, prob = TRUE)
lines(density(resARIMA))
# declaring act_sp500_2015 vector with actual sp500 values for year 2015, for comparison purposes
dataMaster_TS <- dataMaster[-c(1:240),]
dataMaster_2014df <- dataMaster[-c(241:252),]
dataMaster_TR <- ts(dataMaster_2014df, start = c(1995, 1), freq = 12) 
sp500_TR <- dataMaster_TR[, 'sp_500']
sp500_TR
# turning this vector into ts object
act_sp500_2015_ts <- ts(dataMaster_TS$sp_500, start=c(2015, 1), freq=12)
act_sp500_2015_ts

#  attributes(for_nasdaq_mod) 
for_sp500_all <- forecast(auto.arima(sp500_TR), 12)
for_sp500_all
# Here we do some residual diagnostics to make sure we have white noise!
plot(for_sp500_all$residuals, main = "Residual Plots for Forecast")
acf(for_sp500_all$residuals, main = "ACF plot for Residuals")
resARIMA_all <- for_sp500_all$residuals
hist(resARIMA_all, prob = TRUE, main = "Histogram of ARIMA Training Set")
lines(density(resARIMA_all))
# From these two plots we see that the residuals look like white noise so we're good to go on forecasting

# Here we extract the forecast information to create better visual demonstration of the forecast vs actual values!
for_sp500_df <- data.frame(for_sp500_all)
for_sp500_vals <- for_sp500_df$Point.Forecast
for_sp500_2015_ts <- ts(for_sp500_vals, start = c(2015, 1), freq = 12) 
for_sp500_2015_ts
###Forecast data for 2015 using ARIMA model
#only getting the information for the year 2015


#Including lower 95 % CI Lines
lo <- data.frame(for_sp500_all$lower)
lo_df <- lo$X95.
for_sp500_lo95CI_ts <- ts(lo_df, start=c(2015, 1), freq = 12)
for_sp500_lo95CI_ts
plot(for_sp500_lo95CI_ts)
#including upper 95 % CI Lines
up <- data.frame(for_sp500_all$upper)
up_df <- up$X95. 
for_sp500_up95CI_ts <- ts(up_df, start = c(2015, 1), freq = 12)
for_sp500_up95CI_ts
plot(for_sp500_up95CI_ts)
#real time data for 2015
plot.ts(act_sp500_2015_ts)
together_df <- data.frame(for_sp500_2015_ts, act_sp500_2015_ts, for_sp500_lo95CI_ts, for_sp500_up95CI_ts, date = seq.Date(as.Date("2015-01-01"), by="1 month", length.out=12))
colnames(together_df) <- c("forecastedValues2015", "actualValues2015", "lower95CI", "upper95CI", "Date")
head(together_df)

# CSV file outputting all four values: Forecasted, actual, lower 95 and upper 95. (Includes the dates as well)
write.csv(together_df, file = "myData.csv")


par(mar = rep(2, 4))
ggplot(together_df, aes(Date)) + 
  geom_ribbon(aes(x=Date, ymax= up_df, ymin= lo_df), fill="gray", alpha=.5) +
  geom_line(aes(y = forecastedValues2015, colour = "Forecasted Values for 2015")) + 
  geom_line(aes(y = actualValues2015, colour = "Actual Values for 2015")) +
  geom_line(aes(y = lower95CI, colour = "Lower 95% Bound"), linetype = 'dotted') +
  geom_line(aes(y = upper95CI, colour = "Upper 95% Bound"), linetype = 'dotted') +
  scale_color_manual(name="Groups" ,values=c("green", "blue", "red", "red"))



##
####
######
#     ARIMA MODEL PREDICTIONS FOR 2014-15
######
####
##


dataMaster_TS1 <- dataMaster[-c(1:228),]
dataMaster_TS1
dataMaster_2014df <- dataMaster[-c(229:252),]
dataMaster_TR1 <- ts(dataMaster_2014df, start = c(1995, 1), freq = 12) 
sp500_TR1 <- dataMaster_TR1[, 'sp_500']
sp500_TR1
# turning this vector into ts object
act_sp500_2014_ts <- ts(dataMaster_TS1$sp_500, start=c(2014, 1), freq=12)
act_sp500_2014_ts
plot(act_sp500_2014_ts)

#  attributes(for_nasdaq_mod) 
for_sp500_all_2014 <- forecast(auto.arima(sp500_TR1), 24)
for_sp500_all_2014

# CSV file for residual values
res <- for_sp500_all_2014$residuals
write.csv(res, file = "resids2014.csv")
# Here we do some residual diagnostics to make sure we have white noise!
plot(for_sp500_all_2014$residuals, main = "Residual Plots for Forecast")
acf(for_sp500_all_2014$residuals, main = "ACF plot for Residuals")
hist(res, prob = TRUE)
lines(density(res))
# Here we extract the forecast information to create better visual demonstration of the forecast vs actual values!
for_sp500_2014_df <- data.frame(for_sp500_all_2014)
for_sp500_2014_vals <- for_sp500_2014_df$Point.Forecast
for_sp500_2014_ts <- ts(for_sp500_2014_vals, start = c(2014, 1), freq = 12) 
for_sp500_2014_ts
plot(for_sp500_2014_ts)

###Forecast data for 2015 using ARIMA model
#only getting the information for the year 2015



#Including lower 95 % CI Lines
lo <- data.frame(for_sp500_all_2014$lower)
lo_df <- lo$X95.
for_sp500_lo95CI_14ts <- ts(lo_df, start=c(2014, 1), freq = 12)
for_sp500_lo95CI_14ts
plot(for_sp500_lo95CI_14ts)
#including upper 95 % CI Lines
up <- data.frame(for_sp500_all_2014$upper)
up_df <- up$X95. 
for_sp500_up95CI_14ts <- ts(up_df, start = c(2014, 1), freq = 12)
for_sp500_up95CI_14ts
plot(for_sp500_up95CI_14ts)
#real time data for 2014-15
plot.ts(act_sp500_2014_ts)
together_df2 <- data.frame(for_sp500_2014_ts, act_sp500_2014_ts, for_sp500_lo95CI_14ts, for_sp500_up95CI_14ts, date = seq.Date(as.Date("2014-01-01"), by="1 month", length.out=24))
colnames(together_df2) <- c("forecastedValues", "actualValues", "lowerCI", "upperCI", "Date")
head(together_df2)

# CSV file doing the same as mentioned for the model done before this one
write.csv(together_df2, file = "sp500_2014.csv")


par(mar = rep(2, 4))
ggplot(together_df2, aes(Date)) + 
  geom_ribbon(aes(x=Date, ymax= upperCI, ymin= lowerCI), fill="gray", alpha=.5) +
  geom_line(aes(y = forecastedValues, colour = "Forecasted Values for 2015")) + 
  geom_line(aes(y = actualValues, colour = "Actual Values for 2015")) +
  geom_line(aes(y = lowerCI, colour = "Lower 95% Bound"), linetype = 'dotted') +
  geom_line(aes(y = upperCI, colour = "Upper 95% Bound"), linetype = 'dotted') +
  scale_color_manual(name="Groups" ,values=c("green", "blue", "red", "red"))

# Conclusion for this model: The forecast follows the actual data pretty well. It's able to capture the trend although of course it can't capture the volitaty very well we found this
# model to be satisfactory, although we will test others and measure their goodness of fit



##
####
######
#     BOX COX TRANSFORMATION
######
####
##



lambda <- BoxCox.lambda(sp500_TR)
fit_sp500_BC <- ar(BoxCox(sp500_TR,lambda))
fit_sp500_BC
attributes(fit_sp500_BC)
# Here we do some residual plots to make sure our residuals are white noise and uncorrelated
plot(fit_sp500_BC$resid, main = "Residual plot for Box Cox Transformation")
acf(fit_sp500_BC$resid,  na.action=na.pass, main = "ACF plot for residuals")



plot(forecast(fit_sp500_BC,h=12,lambda=lambda))
for_sp500_BC <- forecast(fit_sp500_BC,h=12,lambda=lambda)
attributes(for_sp500_BC)

# CSV File for residuals of BC model
resBC <- for_sp500_BC$residuals
hist(resBC, prob = TRUE)
lines(density(resBC))
write.csv(resBC, file = "resBC.csv")
#Creating the predicted values for the Box Cox model for 2015
for_sp500_BCdf <- data.frame(for_sp500_BC)
for_sp500_BCvals <- for_sp500_BCdf$Point.Forecast
for_sp500_BC2015_ts <- ts(for_sp500_BCvals, start = c(2015, 1), freq = 12)
#Creating Upper and Lower 95% CI lines for Box Cox model
lo_BC <- data.frame(for_sp500_BC$lower)
lo_BCdf <- lo_BC$X95.
for_sp500_loBC95CI_ts <- ts(lo_BCdf, start = c(2015, 1), freq = 12)
plot(for_sp500_loBC95CI_ts)

up_BC <- data.frame(for_sp500_BC$upper)
up_BCdf <- up_BC$X95.
for_sp500_upBC95CI_ts <- ts(up_BCdf, start = c(2015, 1), freq = 12)
plot(for_sp500_upBC95CI_ts)
#Representing Forecasted vs Actual 2015 data with Box Cox transformation
together_BCdf <- data.frame(for_sp500_BC2015_ts, act_sp500_2015_ts, for_sp500_loBC95CI_ts, for_sp500_upBC95CI_ts, date = seq.Date(as.Date("2015-01-01"), by="1 month", length.out=12))
together_BCdf

write.csv(together_BCdf, file = "sp500_BC.csv")

ggplot(together_BCdf, aes(date)) + 
  geom_ribbon(aes(x=date, ymax= for_sp500_upBC95CI_ts, ymin= for_sp500_loBC95CI_ts), fill="gray", alpha=.5) +
  geom_line(aes(y = for_sp500_BC2015_ts, colour = "Forecasted Values for 2015")) + 
  geom_line(aes(y = act_sp500_2015_ts, colour = "Actual Values for 2015")) +
  geom_line(aes(y = for_sp500_loBC95CI_ts, colour = "Lower 95% Bound"), linetype = 'dotted') +
  geom_line(aes(y = for_sp500_upBC95CI_ts, colour = "Upper 95% Bound"), linetype = 'dotted') +
  scale_color_manual(name="Groups" ,values=c("green", "blue", "red", "red"))

# Conclusions: Box Cox transformations are usually done with data that is heteroskedastic so the forecast didn't perform as well as the ARIMA model, but we wanted to include it just in case
# anyone wants to use our methodology with data that has a non-constant variance!

# Here we're plotting other forecasts that aren't as good predictors for this data so we are keeping them as simple plots the same steps would be followed as done before if you wanted a 
# detailed plot of these methods
dev.off()
plot(forecast(meanf(sp500_TR, h = 12)), main = "Forecasts Using Mean Methods", xlim = c(2015, 2016))
lines(act_sp500_2015_ts, col = "red")
plot(forecast(naive(sp500_TR, h = 12)),  xlim = c(2015, 2016), ylim = c(1500, 2400))
lines(act_sp500_2015_ts, col = "red")
plot(forecast(snaive(sp500_TR, h = 12)), xlim = c(2015, 2016))
lines(act_sp500_2015_ts, col = "red")
plot(forecast(ets(sp500_TR), h = 12), xlim = c(2015, 2016), ylim = c(1500, 2700))
lines(act_sp500_2015_ts, col = "red")
#Forecast for the year 2016!!
plot(forecast(auto.arima(sp_500), h = 12))
###Measuring accuracy between models 
accuracy(for_sp500_ts)
accuracy(for_sp500_all)
accuracy(for_sp500_BC)
accuracy(meanf(sp_500, h = 12))
accuracy(naive(sp_500, h = 12))
accuracy(snaive(sp_500, h = 12))
accuracy(forecast(ets(sp_500), h = 12))

# Thus we concluded that the ARIMA model produced the best forecast!
