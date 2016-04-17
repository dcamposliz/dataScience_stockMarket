#######
#######
#######
#######
#######
#######
#######
####### Time Series Model for NASDAQ
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
print("Time Series Model for NASDAQ")

dataMaster <- read.csv("/home/dc/myProjects/learn/learnCode/dataScience_stockMarket/data_1/data_master_1.csv")
# dataMaster <- read.csv("C:/Users/Raul/Documents/PSTAT/Data Science/data_master_1.csv")
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

require(ggplot2)
require(forecast)
require(astsa)
require(car)
require(MTS)
require(plm)
library(caret)
# required for cross-validation
require(caret)
require(DAAG)

# outputting work

pdf("timeSeries_nyse.pdf")


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

m1 <- ts((dataMaster$m1)*(dataMaster$billion), start=c(1995, 1), freq=12)
m2 <- ts((dataMaster$m2)*(dataMaster$billion), start=c(1995, 1), freq=12)
consumerSentiment <- ts(dataMaster$consumerSentiment, start=c(1995, 1), freq=12)
imports <- ts(dataMaster$imports, start=c(1995, 1), freq=12)
inflation <- ts(dataMaster$inflation, start=c(1995, 1), freq=12)
oilPrices <- ts(dataMaster$oilPrices, start=c(1995, 1), freq=12)
ppi <- ts(dataMaster$ppi, start=c(1995, 1), freq=12)
exports <- ts(dataMaster$exports, start=c(1995, 1), freq=12)
cpi <- ts(dataMaster$cpi, start=c(1995, 1), freq=12)
unemploymentRate <- ts(dataMaster$unemploymentRate, start=c(1995, 1), freq=12)
fedFunds <- ts(dataMaster$fedFunds, start=c(1995, 1), freq=12)
capUtilization <- ts(dataMaster$capUtilization, start=c(1995, 1), freq=12)
sp_500Dividends <- ts(dataMaster$sp_500Dividends, start=c(1995, 1), freq=12)
nasdaq <- ts(dataMaster$nasdaq, start=c(1995, 1), freq=12)
nyse <- ts(dataMaster$nyse, start=c(1995, 1), freq=12)
sp_500 <- ts(dataMaster$sp_500, start=c(1995, 1), freq=12)
gdp_us <- ts((dataMaster$gdp_us)*(dataMaster$trillion), start=c(1995, 1), freq=12)
housingIndex <- ts(dataMaster$housingIndex, start=c(1995, 1), freq=12)


###########################################################
print(" ")
print(" ")
print(" ")
###########################################################

##
####
######
#     PLOTTING FINANCIAL DATA
######
####
##

plot.ts(m1)
plot.ts(m2)
plot.ts(consumerSentiment)
plot.ts(inflation)
plot.ts(imports)
plot.ts(oilPrices)
plot.ts(ppi)
plot.ts(exports)
plot.ts(cpi)
plot.ts(unemploymentRate)
plot.ts(fedFunds)
plot.ts(capUtilization)
plot.ts(sp_500Dividends)
plot.ts(nasdaq)
plot.ts(nyse)
plot.ts(sp_500)
plot.ts(gdp_us)
plot.ts(housingIndex)

##
####
######
#     MAKING DATA.FRAME WITH FINANCIAL DATA OBJECTS
######
####
##

dataMaster_df <- data.frame(m1, m2, consumerSentiment, imports, inflation, oilPrices, ppi, exports, cpi, unemploymentRate, fedFunds, capUtilization , sp_500Dividends, nasdaq, nyse, sp_500, gdp_us, housingIndex)


# printing out structure of our new data frame

str(dataMaster_df)


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


# First we plot the time series plot to get an understanding of the necessary modeling
plot.ts(nyse)

# Notice that this plot is not stationary so an appropriate transformation must be made
# The variability can not be seen at first glance but one the transformation is made, we can see if the model
# is heteroskedastic
nyse
# Once we plot the newly transformed data we see that it is now weakly stationary
# Upon inspection of the ACF and PACF plot we can deduce the model we will be fitting will
# be a combination of the AR and MA models
# Using the auto.arima function to output a model we get as follows:

auto.arima(nyse)
for_nyse_ts <- forecast(auto.arima(nyse))
plot(for_nyse_ts) 


# declaring actual_nasdaq_2015 vector with actual nasdaq values for year 2015, for comparison purposes
dataMaster_TS <- dataMaster_df[-c(1:240),]
dataMaster_2014df <- dataMaster_df[-c(241:252),]
dataMaster_TR <- ts(dataMaster_2014df, start = c(1995, 1), freq = 12) 
nyse_TR <- dataMaster_TR[, 'nyse']
nyse_TR
# turning this vector into ts object
act_nyse_2015_ts <- ts(dataMaster_TS$nyse, start=c(2015, 1), freq=12)
act_nyse_2015_ts


#  attributes(for_nasdaq_mod) 
for_nyse_all <- forecast(auto.arima(nyse_TR), 12)
for_nyse_all
for_nyse_df <- data.frame(for_nyse_all)
for_nyse_vals <- for_nyse_df$Point.Forecast
for_nyse_2015_ts <- ts(for_nyse_vals, start = c(2015, 1), freq = 12) 
for_nyse_2015_ts

###Forecast data for 2015 using ARIMA model
#only getting the information for the year 2015



#Including lower 95 % CI Lines
lo <- data.frame(for_nyse_all$lower)
lo_df <- lo$X95.
for_nyse_lo95CI_ts <- ts(lo_df, start=c(2015, 1), freq = 12)
for_nyse_lo95CI_ts
plot(for_nyse_lo95CI_ts)
#including upper 95 % CI Lines
up <- data.frame(for_nyse_all$upper)
up_df <- up$X95. 
for_nyse_up95CI_ts <- ts(up_df, start = c(2015, 1), freq = 12)
for_nyse_up95CI_ts
plot(for_nyse_up95CI_ts)
#real time data for 2015
plot.ts(act_nyse_2015_ts)
together_df <- data.frame(for_nyse_2015_ts, act_nyse_2015_ts, for_nyse_lo95CI_ts, for_nyse_up95CI_ts, date = seq.Date(as.Date("2015-01-01"), by="1 month", length.out=12))
together_df



par(mar = rep(2, 4))
ggplot(together_df, aes(date)) + 
  geom_ribbon(aes(x=date, ymax= up_df, ymin= lo_df), fill="gray", alpha=.5) +
  geom_line(aes(y = for_nyse_2015_ts, colour = "Forecasted Values for 2015")) + 
  geom_line(aes(y = act_nyse_2015_ts, colour = "Actual Values for 2015")) +
  geom_line(aes(y = lo_df, colour = "Lower 95% Bound"), linetype = 'dotted') +
  geom_line(aes(y = up_df, colour = "Upper 95% Bound"), linetype = 'dotted') +
  scale_color_manual(name="Groups" ,values=c("green", "blue", "red", "red"))

##
####
######
#     ARIMA MODEL PREDICTIONS FOR 2014-15
######
####
##


dataMaster_TS1 <- dataMaster_df[-c(1:228),]
dataMaster_TS1
dataMaster_2014df <- dataMaster_df[-c(229:252),]
dataMaster_TR1 <- ts(dataMaster_2014df, start = c(1995, 1), freq = 12) 
nyse_TR <- dataMaster_TR1[, 'nyse']
nyse_TR
# turning this vector into ts object
act_nyse_2014_ts <- ts(dataMaster_TS1$nyse, start=c(2014, 1), freq=12)
act_nyse_2014_ts
plot(act_nyse_2014_ts)

#  attributes(for_nasdaq_mod) 
for_nyse_all <- forecast(auto.arima(nyse_TR), 24)
for_nyse_all
for_nyse_df <- data.frame(for_nyse_all)
for_nyse_vals <- for_nyse_df$Point.Forecast
for_nyse_2014_ts <- ts(for_nyse_vals, start = c(2014, 1), freq = 12) 
for_nyse_2014_ts
plot(for_nyse_2014_ts)
###Forecast data for 2015 using ARIMA model
#only getting the information for the year 2015



#Including lower 95 % CI Lines
lo <- data.frame(for_nyse_all$lower)
lo_df <- lo$X95.
for_nyse_lo95CI_14ts <- ts(lo_df, start=c(2014, 1), freq = 12)
for_nyse_lo95CI_14ts
plot(for_nyse_lo95CI_14ts)
#including upper 95 % CI Lines
up <- data.frame(for_nyse_all$upper)
up_df <- up$X95. 
for_nyse_up95CI_14ts <- ts(up_df, start = c(2014, 1), freq = 12)
for_nyse_up95CI_14ts
plot(for_nyse_up95CI_14ts)
#real time data for 2014-15
plot.ts(act_nyse_2014_ts)
together_df <- data.frame(for_nyse_2014_ts, act_nyse_2014_ts, for_nyse_lo95CI_14ts, for_nyse_up95CI_14ts, date = seq.Date(as.Date("2014-01-01"), by="1 month", length.out=24))
together_df

par(mar = rep(2, 4))
ggplot(together_df, aes(date)) + 
  geom_ribbon(aes(x=date, ymax= up_df, ymin= lo_df), fill="gray", alpha=.5) +
  geom_line(aes(y = for_nyse_2014_ts, colour = "Forecasted Values for 2015")) + 
  geom_line(aes(y = act_nyse_2014_ts, colour = "Actual Values for 2015")) +
  geom_line(aes(y = lo_df, colour = "Lower 95% Bound"), linetype = 'dotted') +
  geom_line(aes(y = up_df, colour = "Upper 95% Bound"), linetype = 'dotted') +
  scale_color_manual(name="Groups" ,values=c("green", "blue", "red", "red"))

##
####
######
#     BOX COX TRANSFORMATION
######
####
##



lambda <- BoxCox.lambda(nyse_TR)
fit_nyse_BC <- ar(BoxCox(nyse_TR,lambda))
fit_nyse_BC
plot(forecast(fit_nyse_BC,h=12,lambda=lambda))
for_nyse_BC <- forecast(fit_nyse_BC,h=12,lambda=lambda)
attributes(for_nyse_BC)
#Creating the predicted values for the Box Cox model for 2015
for_nyse_BCdf <- data.frame(for_nyse_BC)
for_nyse_BCvals <- for_nyse_BCdf$Point.Forecast
for_nyse_BC2015 <- ts(for_nyse_BCvals, start = c(2015, 1), freq = 12)
#Creating Upper and Lower 95% CI lines for Box Cox model
lo_BC <- data.frame(for_nyse_BC$lower)
lo_BCdf <- lo_BC$X95.
for_nyse_loBC95CI <- ts(lo_BCdf, start = c(2015, 1), freq = 12)
plot(for_nyse_loBC95CI)

up_BC <- data.frame(for_nyse_BC$upper)
up_BCdf <- up_BC$X95.
for_nyse_upBC95CI <- ts(up_BCdf, start = c(2015, 1), freq = 12)
plot(for_nyse_upBC95CI)
#Representing Forecasted vs Actual 2015 data with Box Cox transformation
together_BCdf <- data.frame(for_nyse_BC2015, act_nyse_2015_ts, for_nyse_loBC95CI, for_nyse_upBC95CI, date = seq.Date(as.Date("2015-01-01"), by="1 month", length.out=12))
together_BCdf

ggplot(together_BCdf, aes(date)) + 
  geom_ribbon(aes(x=date, ymax= for_nyse_upBC95CI, ymin= for_nyse_loBC95CI), fill="gray", alpha=.5) +
  geom_line(aes(y = for_nyse_BC2015, colour = "Forecasted Values for 2015")) + 
  geom_line(aes(y = act_nyse_2015_ts, colour = "Actual Values for 2015")) +
  geom_line(aes(y = for_nyse_loBC95CI, colour = "Lower 95% Bound"), linetype = 'dotted') +
  geom_line(aes(y = for_nyse_upBC95CI, colour = "Upper 95% Bound"), linetype = 'dotted') +
  scale_color_manual(name="Groups" ,values=c("green", "blue", "red", "red"))