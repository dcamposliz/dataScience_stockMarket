# loading our data
	
	dataMaster <- read.csv("/home/dc/myProjects/dataScience_stockMarket/data_1/data_master_1.csv")

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

# outputting data head and structure
	
	head(dataMaster)
	str(dataMaster)

# assigning data column to variable container
	
	gdp <- dataMaster$gdp_us
	gdp

# printing string and mean of gdp
	
	print("This is the average GDP of the US from years 1995 to 2015 ---------------------------------------------------")
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

# printing some data	

	print("Here are S&P 500s values for 1995-2015! ---------------------------------------------------")
	sp_500
	
	print("Here are nasdaq values for 1995-2015! ---------------------------------------------------")
	nasdaq

	dataMaster_df <- data.frame(m1, m2, consumerSentiment, imports, inflation, oilPrices, ppi, exports, cpi, unemploymentRate, fedFunds, capUtilization , sp_500Dividends, nasdaq, nyse, sp_500, gdp_us)

	str(dataMaster_df)

#linear regression models on stock indices as a function of economic indicators

	fit_nasdaq <- lm(nasdaq ~ m1 + m2 + consumerSentiment + inflation + imports + oilPrices + ppi + exports + cpi + unemploymentRate + fedFunds + capUtilization + gdp_us, data = dataMaster_df)
	fit_nyse <- lm(nyse ~ m1 + m2 + consumerSentiment + inflation + imports + oilPrices + ppi + exports + cpi + unemploymentRate + fedFunds + capUtilization + gdp_us, data = dataMaster_df)
	fit_sp_500 <- lm(sp_500 ~ m1 + m2 + consumerSentiment + inflation + imports + oilPrices + ppi + exports + cpi + unemploymentRate + fedFunds + capUtilization + gdp_us, data = dataMaster_df)
	fit_sp_500Dividends <- lm(nasdaq ~ m1 + m2 + consumerSentiment + inflation + imports + oilPrices + ppi + exports + cpi + unemploymentRate + fedFunds + capUtilization + gdp_us + sp_500, data = dataMaster_df)

	summary(fit_nasdaq)
	summary(fit_nyse)
	summary(fit_sp_500)
	summary(fit_sp_500Dividends)

#confidence intervals

	confint(fit_nasdaq)
	confint(fit_nyse)
	confint(fit_sp_500)
	confint(fit_sp_500Dividends)

# what is auto.arima? some witchcraft?
	
	# this is auto-regression integrating moving average

	auto.arima(dataMaster$nasdaq)
	auto.arima(dataMaster$nyse)
	auto.arima(dataMaster$sp_500)
	auto.arima(dataMaster$sp_500Dividends)

# what are these things and how are they relevant?

	# acf
	# pacf

# LOOK UP: sarima()

	sarima_nasdaq_model <- sarima(dataMaster$nasdaq, p = 1, q = 1, d = 2)

		# p is autoregressive coefficient
		# q is whether it's stationary or not
		# d is moving average

# LOOK UP: sarima.for()

	# forecasting
	sarima.for(dataMaster$nasdaq, n.ahead = 12, p = 1, q = 1, d = 2)

	#multi-variate time-series is cool
	
# r-squared is to regression just as aic (akaine information criterion) is to time-series

print(" ")
print("now we really don't know what the fuck we are doing")
print(" ")

	apca(dataMaster_df, m=3)

	# what the hell is m=3?

	# further action items
	# learn ts(), lm(), summary(), arima(), 
	# http://people.duke.edu/~rnau/411arim.htm