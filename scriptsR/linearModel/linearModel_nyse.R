##
####
######
# 		LOADING DATA, PACKAGES
######
####
##
	print("hello, this is some linear model stuff!!")

	dataMaster <- read.csv("/home/dc/myProjects/learn/learnCode/dataScience_stockMarket/data_1/data_master_1.csv")

	attach(dataMaster)

# here is a snapshot of our variables
# tons of financial data:

	#	m1
	#	m2
	#	consumerSentiment
	#	inflation
	#	imports
	#	oilPrices
	#	ppi
	#	exports
	#	cpi
	#	unemploymentRate
	#	fedFunds
	#	capUtilization
	#	sp_500Dividends
	#	nasdaq
	#	nyse
	#	sp_500
	#	gdp_us
	#	housingIndex

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

	pdf("linearModel_nyse.pdf")


###########################################################
print(" ")
print(" ")
print(" ")
###########################################################

##
####
######
# 		TESTING that data loads properly
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
# 		CREATING TIME-SERIES OBJECTS WITH FINANCIAL DATA 
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
# 		PLOTTING FINANCIAL DATA
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
# 		MAKING DATA.FRAME WITH FINANCIAL DATA OBJECTS
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
###########################################################

##
####
######
# 		DEVELOPING MODEL FOR NYSE ---- linear regression model on nyse as a function of economic indicators
######
####
##

	fit_nyse_1 <- lm(nyse ~ ., data = dataMaster_df)
	summary(fit_nyse_1)

	# eliminating non-significant variables using BIC procedure

		n <- nrow(dataMaster_df)		
		drop1(fit_nyse_1, k=log(n))		
		# ppi has smallest AIC so we drop it

		fit_nyse_2 <- lm(nyse ~ . - consumerSentiment - ppi, data = dataMaster_df)
		# summary(fit_nyse_2)
		drop1(fit_nyse_2, k=log(n))
		# fedFunds has smallest AIC so we drop it

		fit_nyse_3 <- lm(nyse ~ . - consumerSentiment - ppi - fedFunds, data = dataMaster_df)
		# summary(fit_nyse_3)
		drop1(fit_nyse_3, k=log(n))
		# imports has smallest AIC so we drop it

		fit_nyse_4 <- lm(nyse ~ . - consumerSentiment - ppi - fedFunds - imports, data = dataMaster_df)
		# summary(fit_nyse_4)
		drop1(fit_nyse_4, k=log(n))
		# inflation has smallest AIC so we drop it

		fit_nyse_5 <- lm(nyse ~ . - consumerSentiment - ppi - fedFunds - imports - inflation, data = dataMaster_df)
		# summary(fit_nyse_5)
		drop1(fit_nyse_5, k=log(n))
		# cpi has smallest AIC so we drop it

		fit_nyse_6 <- lm(nyse ~ . - consumerSentiment - ppi - fedFunds - imports - inflation - cpi, data = dataMaster_df)
		# summary(fit_nyse_6)
		drop1(fit_nyse_6, k=log(n))

	# looks like we found a model !
		fit_nyse <- fit_nyse_6
	# let's get the confidence intervals for fit_nyse
		confint(fit_nyse)
	# let's plot our model
		plot(fit_nyse)


	#scatterplot matrix for fit_nyse_6

		panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
		{
		usr <- par("usr"); on.exit(par(usr))
		par(usr = c(0, 1, 0, 1))
		r <- abs(cor(x, y))
		txt <- format(c(r, 0.123456789), digits=digits)[1]
		txt <- paste(prefix, txt, sep="")
		if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
		text(0.5, 0.5, txt, cex = cex.cor * r)
		}
		pairs(~ nyse + m1 + m2 + oilPrices + exports + unemploymentRate + capUtilization + sp_500Dividends + nasdaq + sp_500 + gdp_us + housingIndex, data=dataMaster_df,
		lower.panel=panel.smooth, upper.panel=panel.cor, 
		pch=20, main="NYSE Scatter Plot Matrix for fit_nyse_6")



##########################################################
print(" ")
print(" ")
###########################################################

##
####
######
# 		CROSS-VALIDATION
######
####
##

		ny8_CV <- CVlm (data = dataMaster_df, form.lm = formula(fit_nyse_5), m = 3, dots = FALSE, seed = 29, plotit = c("Observed", "Residual"), main="Cross-validation for fit_nyse_5", legend.pos="topleft", printit=TRUE)
		summary(ny8_CV)
		attributes(ny8_CV)
		# mean squared: 27066

		ny9_CV <- CVlm (data = dataMaster_df, form.lm = formula(fit_nyse_6), m = 3, dots = FALSE, seed = 29, plotit = c("Observed", "Residual"), main="Cross-validation for fit_nyse_6, the WINNER", legend.pos="topleft", printit=TRUE)
		summary(ny9_CV)
		attributes(ny9_CV)
		# mean squared: 26519

	# cross-validation confirms that fit_nyse_6 is the best model, out of all the models listed in this file


###########################################################
print(" ")
print(" ")
###########################################################

##
####
######
# 		NOW PREDICTING 2015 BASED ON 1995-2014 WITH model: fit_nyse_6
######
####
##

	# reducing our dataset to the significant variables we found for model fit_nyse_6
	dataMaster_fitData <- subset(dataMaster_df, select = c(nyse, m1, m2, oilPrices, exports, unemploymentRate, capUtilization, sp_500Dividends, nasdaq, sp_500, gdp_us, housingIndex, housingIndex))
	# outputting the attributes of our new dataset to confirm we are using the desired variables
	attributes(dataMaster_fitData)

	# cutting out dataset into training and testing datasets
	dataMaster_TR <- dataMaster_fitData[-c(241:252),]
	# dataMaster_TR
	dataMaster_TS <- dataMaster_fitData[-c(1:240),]
	# dataMaster_TS


	# removing nasdaq variable from testing dataset
	dataMaster_TS_wo_nyse <- dataMaster_TS
	dataMaster_TS_wo_nyse$nyse <- NULL
	# confirming that nyse variable is not in the testing dataset
	attributes(dataMaster_TS_wo_nyse)

	# developing fit_nyse_6_2 with new training dataset
	fit_nyse_6_2 <- lm(nyse ~ ., data = dataMaster_TR)

	# linear prediction model predict_nyse_2015 with fit_nyse_6_2 and testing dataMaster_TS_wo_nyse dataset
	predict_nyse_2015 <- predict.lm(fit_nyse_6_2, dataMaster_TS_wo_nyse)  
	# running the prediction model
	predict_nyse_2015

	# turning prediction vector into time series object
	predict_nyse_2015_ts <- ts(predict_nyse_2015, start=c(2015,1), freq=12)
	# outputting the prediction vector to terminal
	predict_nyse_2015_ts

	# plotting ts prediction vector to pdf 
	plot(predict_nyse_2015_ts)

	# declaring actual_nyse_2015 vector with actual nyse values for year 2015, for comparison purposes
	actual_nyse_2015 <- dataMaster_TS$nyse
	# turning this vector into ts object
	actual_nyse_2015_ts <- ts(actual_nyse_2015, start=c(2015, 1), freq=12)

	# plotting vector into pdf
	plot(actual_nyse_2015_ts)

	# creating dataframe with both predicted and actual ts nyse objects
	predicted_and_actual_nyse <- data.frame(predict_nyse_2015_ts, actual_nyse_2015_ts, date = seq.Date(as.Date("2015-01-01"), by="1 month", length.out=12))

	# yaaaay -- outputting this shaaait
	# prediction graph to pdf with dataframe predictions and actual values :)
	ggplot(predicted_and_actual_nyse, aes(date)) + geom_line(aes(y = predict_nyse_2015_ts, colour = "Predicted Values")) + geom_line(aes(y = actual_nyse_2015_ts, colour = "Actual Values"))

