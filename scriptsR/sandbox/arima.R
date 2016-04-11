##
####
######
# 		LOADING DATA, PACKAGES
######
####
##
	print("hello - this is some !!")

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
# 		PLAYING WITH ARIMA MODELS
######
####
##

# what is auto.arima? some witchcraft?

	print(" ")
	print("running auto.arima() -- on the nasdaq, sp_500, nyse, housingIndex")
	print(" ")
	
	# this is auto-regression integrating moving average

	auto.arima(dataMaster$nasdaq)
	auto.arima(dataMaster$sp_500)
	auto.arima(dataMaster$nyse)
	auto.arima(dataMaster$housingIndex)

# what are these things and how are they relevant?

	# acf
	# pacf

# LOOK UP: sarima()

	print(" ")
	print("running sarima() -- on the nasdaq")
	print(" ")

	sarima_nasdaq_model <- sarima(dataMaster$nasdaq, p = 1, q = 1, d = 2)
	sarima_sp_500_model <- sarima(dataMaster$sp_500, p = 1, q = 1, d = 2)
	sarima_nyse_model <- sarima(dataMaster$nyse, p = 1, q = 1, d = 2)
	sarima_housingIndex_model <- sarima(dataMaster$housingIndex, p = 1, q = 1, d = 2)

		# p is autoregressive coefficient
		# q is whether it's stationary or not
		# d is moving average

# LOOK UP: sarima.for()

	print(" ")
	print("running sarima.for() -- on the nasdaq")
	print("we are predicting the value of nasdaq based on itself")
	print(" ")

	# forecasting
	sarima.for(dataMaster$nasdaq, n.ahead = 12, p = 1, q = 1, d = 2)
	sarima.for(dataMaster$sp_500, n.ahead = 12, p = 1, q = 1, d = 2)
	sarima.for(dataMaster$nyse, n.ahead = 12, p = 1, q = 1, d = 2)
	sarima.for(dataMaster$housingIndex, n.ahead = 12, p = 1, q = 1, d = 2)

	#multi-variate time-series is cool
	
# r-squared is to regression just as aic (akaine information criterion) is to time-series

	# further action items
	# learn ts(), lm(), summary(), arima(), 
	# http://people.duke.edu/~rnau/411arim.htm


# TRIED THESE BUT NEED TO LEARN MORE ON Package ‘MTS’:

#	https://cran.r-project.org/web/packages/MTS/MTS.pdf

	#	nasdaq_exogeouns <- c(m1, m2, consumerSentiment, inflation, imports, oilPrices, ppi, exports, cpi, unemploymentRate, fedFunds, capUtilization, sp_500Dividends, nyse, sp_500, gdp_us, housingIndex)
	#	
	#	Mlm(nasdaq, nasdaq_exogeouns, constant=TRUE, output=TRUE)
	#	 
	#	MTSplot(dataMaster_df, caltime = NULL)