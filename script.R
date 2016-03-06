# loading our data
	
	dataMaster <- read.csv("/home/dc/myProjects/learn/dataScience_stockMarket/data_1/data_master_1.csv")

	# here is a snapshot of our variables:

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

# outputting data head and structure
	
	head(dataMaster)
	str(dataMaster)

# assigning data column to variable container
	
	gdp <- dataMaster$gdp_us
	gdp

# printing string and mean of gdp
	
	mean(gdp)

# running ts function on data column, with start paramenter, and frequency
	
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

#plotting some stoof

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

	dataMaster_df <- data.frame(m1, m2, consumerSentiment, imports, inflation, oilPrices, ppi, exports, cpi, unemploymentRate, fedFunds, capUtilization , sp_500Dividends, nasdaq, nyse, sp_500, gdp_us)

	str(dataMaster_df)

#linear regression models on stock indices as a function of economic indicators

	fit_nasdaq_1 <- lm(nasdaq ~ ., data = dataMaster_df)

	summary(fit_nasdaq_1)

# ELIMINATING NON-SIGNIFICANT VARIABLES IN OUR MODEL
	# there are various procedures for this, let's try the BIC procedure

	#BIC procedure on our fit_nasdaq_1 model, then on subsequent ones
		n <- nrow(dataMaster_df)
		#this produces an anova table
		drop1(fit_nasdaq_1, k=log(n))
		#consumerSentiment has smallest AIC so we drop it

	fit_nasdaq_2 <- lm(nasdaq ~ . - consumerSentiment, data = dataMaster_df)
	summary(fit_nasdaq_2)
		drop1(fit_nasdaq_2, k=log(n))
		#gdp_us has smallest AIC so we drop it

	fit_nasdaq_3 <- lm(nasdaq ~ . - consumerSentiment - gdp_us, data = dataMaster_df)
	summary(fit_nasdaq_3)
		drop1(fit_nasdaq_3, k=log(n))
		#we drop ppi as it has smallest AIC

	fit_nasdaq_4 <- lm(nasdaq ~ . - consumerSentiment - gdp_us - ppi, data = dataMaster_df)
	summary(fit_nasdaq_4)
		drop1(fit_nasdaq_4, k=log(n))
		#we drop cpi as it has smallest AIC

	fit_nasdaq_5 <- lm(nasdaq ~ . - consumerSentiment - gdp_us - ppi - cpi, data = dataMaster_df)	
	summary(fit_nasdaq_5)
		drop1(fit_nasdaq_5, k=log(n))
		#we drop m2 as it has smallest AIC

	fit_nasdaq_6 <- lm(nasdaq ~ . - consumerSentiment - gdp_us - ppi - cpi - m2, data = dataMaster_df)	
	summary(fit_nasdaq_6)
		drop1(fit_nasdaq_6, k=log(n))
		#we drop inflation as it has smallest AIC

	fit_nasdaq_7 <- lm(nasdaq ~ . - consumerSentiment - gdp_us - ppi - cpi - m2 - inflation, data = dataMaster_df)	
	summary(fit_nasdaq_7)
		drop1(fit_nasdaq_7, k=log(n))
		#we drop inflation as it has smallest AIC

	# it looks like we have a model for nasdaq, as far as ml() is concerned

	fit_nasdaq <- fit_nasdaq_7

	# let's get the confidence intervals for fit_nasdaq

	confint(fit_nasdaq)

#####
	
	# we're plotting to 

	plot(fit_nasdaq)

	### if we check our Rplot.pdf file, we see a bunch of residual things :D

	# LEARN RESIDUAL DIAGNOSTICS

	# we should be able to explain these to people w little to no understanding of stats

#####

	# Next we will do the linear fit for the S & P 500
	# This time we will use the BIC Method
	# We will also be using dot notation for the initial fit
	# The dot represents every variable in the file

		fit_sp_500_1 <- lm(sp_500 ~ ., data = dataMaster_df)
		summary(fit_sp_500_1)

	# BIC Method
		
		n <- nrow(dataMaster_df)
		drop1(fit_sp_500_1, k=log(n))

	# Using the BIC method, we will find and eliminate the variable with the lowest AIC value 
	
	# cpi has the lowest AIC value, cpi will be dropped
		fit_sp_500_2 <- lm(sp_500 ~ . - cpi,  data = dataMaster_df)
		summary(fit_sp_500_2)
		drop1(fit_sp_500_2, k=log(n))

	# m2 has the lowest AIC value and will be dropped
		fit_sp_500_3 <- lm(sp_500 ~ . - cpi - m2, data = dataMaster_df)
		summary(fit_sp_500_3)
		drop1(fit_sp_500_3, k=log(n))

	# ppi has the lowest AIC value and will be dropped
		fit_sp_500_4 <- lm(sp_500 ~ . - cpi - m2 - ppi, data = dataMaster_df)
		summary(fit_sp_500_4)
		drop1(fit_sp_500_4, k=log(n))

	# customerSentiment has the lowest AIC value and will be dropped
		fit_sp_500_5 <- lm(sp_500 ~ . - cpi - m2 - ppi - consumerSentiment, data = dataMaster_df)
		summary(fit_sp_500_5)
		drop1(fit_sp_500_5, k=log(n))

	# inflation is the next variable with the lowest AIC value, so it will be dropped
		fit_sp_500_6 <- lm(sp_500 ~ . - cpi - m2 - ppi - consumerSentiment - inflation, data = dataMaster_df)
		summary(fit_sp_500_6)
		drop1(fit_sp_500_6, k=log(n))

	# then we drop the gdp_us
		fit_sp_500_7 <- lm(sp_500 ~ . - cpi - m2 - ppi - consumerSentiment - inflation - gdp_us, data = dataMaster_df)
		summary(fit_sp_500_7)
		drop1(fit_sp_500_7, k=log(n))


	# it looks like we have a model for nasdaq, as far as ml() is concerned

	fit_sp_500 <- fit_sp_500_7
	confint(fit_sp_500)
	plot(fit_sp_500)


# Last we will analyze the nyse 
# linear model on nyse as function of every economic indicator

		fit_nyse_1 <- lm(nyse ~ ., data = dataMaster_df)
		summary(fit_nyse_1)

	# We will use backwards elimination for the nyse
	# According to the summary, cpi has the highest p value
	# cpi will be dropped

		fit_nyse_2<- lm(nyse ~ . - cpi, data = dataMaster_df)
		summary(fit_nyse_2)

	# According to the summary, consumerSentiment has the highest p value and will be dropped

		fit_nyse_3<- lm(nyse ~ . - cpi - consumerSentiment, data = dataMaster_df)
		summary(fit_nyse_3)

	# According to the summary, sp_500 dividends have the highest p value and will be dropped

		fit_nyse_4<- lm(nyse ~ . - cpi - consumerSentiment - sp_500Dividends, data = dataMaster_df)
		summary(fit_nyse_4)

	# According to the summary, imports have the highest p value and will be dropped

		fit_nyse_5<- lm(nyse ~ . - cpi - consumerSentiment - sp_500Dividends - imports, data = dataMaster_df)
		summary(fit_nyse_5)

	# According to the summary, US GDP has the highest p value and will be removed

		fit_nyse_6<- lm(nyse ~ . - cpi - consumerSentiment - sp_500Dividends - imports - gdp_us, data = dataMaster_df)
		summary(fit_nyse_6)

	# According to the summary, inflation has the highest p value and will be dropped

		fit_nyse_7<- lm(nyse ~ . - cpi - consumerSentiment - sp_500Dividends - imports - gdp_us - inflation, data = dataMaster_df)
		summary(fit_nyse_7)

	# it looks like we have a model for nyse, as far as ml() is concerned

	fit_nyse <- fit_nyse_7
	confint(fit_nyse)
	plot(fit_nyse)
	