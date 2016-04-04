##
####
######
# 		LOADING DATA, PACKAGES
######
####
##
	print("hello, this is some linear model stuff!!")

	dataMaster <- read.csv("/home/dxc/myProjects/dataScience_stockMarket/data_1/data_master_1.csv")

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
# 		DEVELOPING MODEL FOR NASDAQ ---- linear regression model on nasdaq as a function of economic indicators
######
####
##

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
		# imports has smallest AIC so we drop it

		fit_nasdaq_5 <- lm(nasdaq ~ . - sp_500Dividends - ppi - consumerSentiment - imports, data = dataMaster_df)
		summary(fit_nasdaq_5)
		drop1(fit_nasdaq_5, k=log(n))
		# exports has smallest AIC so we drop it

		fit_nasdaq_6 <- lm(nasdaq ~ . - sp_500Dividends - ppi - consumerSentiment - imports - exports, data = dataMaster_df)
		summary(fit_nasdaq_6)
		drop1(fit_nasdaq_6, k=log(n))
		# cpi has smallest AIC so we drop it

		fit_nasdaq_7 <- lm(nasdaq ~ . - sp_500Dividends - ppi - consumerSentiment - imports - exports - cpi, data = dataMaster_df)
		summary(fit_nasdaq_7)
		drop1(fit_nasdaq_7, k=log(n))
		# inflation has smallest AIC so we drop it

		fit_nasdaq_8 <- lm(nasdaq ~ . - sp_500Dividends - ppi - consumerSentiment - imports - exports - cpi - inflation, data = dataMaster_df)
		summary(fit_nasdaq_8)
		drop1(fit_nasdaq_8, k=log(n))
		# fedFunds has smallest AIC so we drop it

		fit_nasdaq_9 <- lm(nasdaq ~ . - sp_500Dividends - ppi - consumerSentiment - imports - exports - cpi - inflation - fedFunds, data = dataMaster_df)
		summary(fit_nasdaq_9)
		drop1(fit_nasdaq_9, k=log(n))

	# looks like we found a model !
		fit_nasdaq <- fit_nasdaq_9
	# let's get the confidence intervals for fit_nasdaq
		confint(fit_nasdaq)
	# let's plot our model
		plot(fit_nasdaq)	

###########################################################
print(" ")
print(" ")
print(" ")
###########################################################

##
####
######
# 		DEVELOPING MODEL FOR SP_500 ---- linear regression model on nasdaq as a function of economic indicators
######
####
##

	fit_sp_500_1 <- lm(sp_500 ~ ., data = dataMaster_df)
	summary(fit_sp_500_1)

	# eliminating non-significant variables using BIC procedure

		n <- nrow(dataMaster_df)		
		drop1(fit_sp_500_1, k=log(n))		
		# consumerSentiment and ppi have smallest AIC so we drop them

		fit_sp_500_2 <- lm(sp_500 ~ . - consumerSentiment - ppi, data = dataMaster_df)
		summary(fit_sp_500_2)
		drop1(fit_sp_500_2, k=log(n))
		# imports has smallest AIC so we drop it

		fit_sp_500_3 <- lm(sp_500 ~ . - consumerSentiment - ppi - imports, data = dataMaster_df)
		summary(fit_sp_500_3)
		drop1(fit_sp_500_3, k=log(n))
		# cpi has smallest AIC so we drop it

		fit_sp_500_4 <- lm(sp_500 ~ . - consumerSentiment - ppi - imports - cpi, data = dataMaster_df)
		summary(fit_sp_500_4)
		drop1(fit_sp_500_4, k=log(n))
		# inflation has smallest AIC so we drop it

		fit_sp_500_5 <- lm(sp_500 ~ . - consumerSentiment - ppi - imports - cpi - inflation, data = dataMaster_df)
		summary(fit_sp_500_5)
		drop1(fit_sp_500_5, k=log(n))
		# sp_500Dividends has smallest AIC so we drop it

		fit_sp_500_6 <- lm(sp_500 ~ . - consumerSentiment - ppi - imports - cpi - inflation - sp_500Dividends, data = dataMaster_df)
		summary(fit_sp_500_6)
		drop1(fit_sp_500_6, k=log(n))
		# m2 has smallest AIC so we drop it

		fit_sp_500_7 <- lm(sp_500 ~ . - consumerSentiment - ppi - imports - cpi - inflation - sp_500Dividends - m2, data = dataMaster_df)
		summary(fit_sp_500_7)
		drop1(fit_sp_500_7, k=log(n))

	# looks like we found a model !
		fit_sp_500 <- fit_sp_500_7
	# let's get the confidence intervals for fit_sp_500
		confint(fit_sp_500)
	# let's plot our model
		plot(fit_sp_500)	

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
		summary(fit_nyse_2)
		drop1(fit_nyse_2, k=log(n))
		# fedFunds has smallest AIC so we drop it

		fit_nyse_3 <- lm(nyse ~ . - consumerSentiment - ppi - fedFunds, data = dataMaster_df)
		summary(fit_nyse_3)
		drop1(fit_nyse_3, k=log(n))
		# imports has smallest AIC so we drop it

		fit_nyse_4 <- lm(nyse ~ . - consumerSentiment - ppi - fedFunds - imports, data = dataMaster_df)
		summary(fit_nyse_4)
		drop1(fit_nyse_4, k=log(n))
		# inflation has smallest AIC so we drop it

		fit_nyse_5 <- lm(nyse ~ . - consumerSentiment - ppi - fedFunds - imports - inflation, data = dataMaster_df)
		summary(fit_nyse_5)
		drop1(fit_nyse_5, k=log(n))
		# cpi has smallest AIC so we drop it

		fit_nyse_6 <- lm(nyse ~ . - consumerSentiment - ppi - fedFunds - imports - inflation - cpi, data = dataMaster_df)
		summary(fit_nyse_6)
		drop1(fit_nyse_6, k=log(n))

	# looks like we found a model !
		fit_nyse <- fit_nyse_6
	# let's get the confidence intervals for fit_nyse
		confint(fit_nyse)
	# let's plot our model
		plot(fit_nyse)
