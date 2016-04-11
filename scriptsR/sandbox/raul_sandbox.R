### Predicting using Regression 

###New fitting since I removed the data from 2015 to predict the values 
fit_nasdaq_1 <- lm(nasdaq ~ ., data = dataMaster_df)
summary(fit_nasdaq_1)

# eliminating non-significant variables using BIC procedure

n <- nrow(dataMaster_df)  	
drop1(fit_nasdaq_1, k=log(n))		
# sp_500Dividends has smallest AIC so we drop it

fit_nasdaq_2 <- lm(nasdaq ~ . - sp_500Dividends, data = dataMaster_df)
summary(fit_nasdaq_2)
drop1(fit_nasdaq_2, k=log(n))
# ppi has smallest AIC so we drop it

fit_nasdaq_3 <- lm(nasdaq ~ . - sp_500Dividends - ppi, data = dataMaster_df)
summary(fit_nasdaq_3)
drop1(fit_nasdaq_3, k=log(n))
# consumerSentiment has smallest AIC so we drop it

fit_nasdaq_4 <- lm(nasdaq ~ . - sp_500Dividends - ppi - consumerSentiment, data = dataMaster_df)
summary(fit_nasdaq_4)
drop1(fit_nasdaq_4, k=log(n))
# exports has smallest AIC so we drop it
fit_nasdaq_5 <- lm(nasdaq ~ . - sp_500Dividends - ppi - consumerSentiment - exports, data = dataMaster_df)
summary(fit_nasdaq_5)
drop1(fit_nasdaq_5, k=log(n))
# cpi has smallest AIC so we drop it

fit_nasdaq_6 <- lm(nasdaq ~ . - sp_500Dividends - ppi - consumerSentiment - exports - cpi, data = dataMaster_df)
summary(fit_nasdaq_6)
drop1(fit_nasdaq_6, k=log(n))
# inflation has smallest AIC so we drop it

fit_nasdaq_7 <- lm(nasdaq ~ . - sp_500Dividends - ppi - consumerSentiment - exports - cpi - imports, data = dataMaster_df)
summary(fit_nasdaq_7)
drop1(fit_nasdaq_7, k=log(n))
# inflation has smallest AIC so we drop it

fit_nasdaq_8 <- lm(nasdaq ~ . - sp_500Dividends - ppi - consumerSentiment - exports - cpi - imports - inflation, data = dataMaster_df)
summary(fit_nasdaq_8)
drop1(fit_nasdaq_8, k=log(n))
# fedFunds has smallest AIC so we drop it

fit_nasdaq_9 <- lm(nasdaq ~ . - sp_500Dividends - ppi - consumerSentiment - exports - cpi - imports - inflation - fedFunds, data = dataMaster_df)
summary(fit_nasdaq_9)
drop1(fit_nasdaq_9, k=log(n))

# looks like we found a model !
fit_nasdaq <- fit_nasdaq_9
fit_nasdaq1<-lm(nasdaq ~ m1  + m2 +oilPrices + unemploymentRate + capUtilization + nyse +  sp_500 +gdp_us + housingIndex, data = dataMaster_df)
plot(fit_nasdaq1)
# let's get the confidence intervals for fit_nasdaq
confint(fit_nasdaq)

# let's plot our model
plot(fit_nasdaq)	
###Predicting for NASDAQ Model with variables m1 m2 oilPrices unemploymentRate capUtilization nyse sp_500 gdp_us housingIndex
###This new data frame has the actual data for all our significant parameters for the months Jan-Dec for 2015
fit_nasdaq1<-lm(nasdaq ~  m2 + unemploymentRate + oilPrices + capUtilization + sp_500 + gdp_us + housingIndex, data = dataMaster_df)
newData = data.frame(#m1 = c(2929.575, 2980.95, 2992.2, 2995.425, 2993.2, 3008.12, 3028.075, 3046.14, 3058.025, 3033.8, 3084.12, 3073.775)
                      m2 = c(11685.8, 11790.3, 11833.8, 11887, 11915.4, 11965.5, 12030.8, 12091.4, 12157.7, 12165, 12255.1, 12298.6)
                     , oilPrices = c(48.42, 57.93, 55.79, 59.39, 64.56, 62.35, 55.87, 46.99, 47.23, 48.12, 44.42, 37.72)
                     , unemploymentRate = c(0.057, 0.055, 0.055, 0.054, 0.055, 0.05, 0.053, 0.051, 0.051, 0.05, 0.05, 0.05)
                     , capUtilization = c(0.787, 0.784, 0.782, 0.78, 0.776, 0.775, 0.78, 0.779, 0.779, 0.777, 0.77, 0.764)
                     , sp_500 = c(2028.592499, 2050.415039, 2082.582459, 2081.859925, 2099.3549, 2089.485046  , 2086.920044, 2014.084992, 1945.722504, 1996.757538, 2074.259979, 2056.099976)
                     , gdp_us = c( 1.618e+13, 1.618e+13, 1.618e+13, 1.633e+13, 1.633e+13, 1.633e+13, 1.641e+13, 1.641e+13, 1.641e+13, 1.644e+13, 1.644e+13, 1.644e+13)
                     , housingIndex = c(351.19, 351.19, 351.19, 358.18, 358.18, 358.18, 364.37, 364.37, 364.37, 367.13, 367.13, 367.13))

p<-predict.lm(fit_nasdaq1, newData)      
p
predicted_vals_ts<-ts(p, start=c(2015,1), freq=12)
predicted_vals_ts
plot(predicted_vals_ts)
actual_vals <- data.frame(nasdaq = c(4683.900024, 4795.959961, 4935.595093, 4950, 5009.014893, 5050.599975, 5072.694824, 4844.562378, 4685.425049, 4831.559937, 5061.610108, 5046.352539))
actual_vals_ts<-ts(actual_vals$nasdaq, start=c(2015, 1), freq=12)
together_dataframe <- data.frame(predicted_vals_ts, actual_vals_ts, date = seq.Date(as.Date("2015-01-01"), by="1 month", length.out=12))
plot(actual_vals_ts)


###Using the ggplot2 package I graph them on the same axis 
ggplot(together_dataframe, aes(date)) + 
  geom_line(aes(y = predicted_vals_ts, colour = "Predicted Values")) + 
  geom_line(aes(y = actual_vals_ts, colour = "Actual Values"))



###Cross Validation
require(caret)
require(DAAG)

c <-CVlm(data = dataMaster_df, form.lm = formula(nasdaq ~ m2 + unemploymentRate 
    + oilPrices + capUtilization + sp_500 + gdp_us + housingIndex),
     m = 3, dots = FALSE, seed = 29, plotit = c("Observed","Residual"),
     main="Small symbols show cross-validation predicted values",
     legend.pos="topleft", printit = TRUE)
summary(c)
attributes(c)
#MS is 69850
CVlm(data = dataMaster_df, form.lm = formula(nasdaq ~ m1  + m2 
    + unemploymentRate + nyse +  sp_500 + gdp_us + housingIndex),
     m = 3, dots = FALSE, seed = 29, plotit = c("Observed","Residual"),
     main="Small symbols show cross-validation predicted values",
     legend.pos="topleft", printit = TRUE)
#divide by 80 since we did three folders 
#MS is 53108, Gives a smaller Mean Squared Error so more favorable