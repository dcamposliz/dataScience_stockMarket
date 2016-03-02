# loading our data
	
	dataMaster <- read.csv("/home/dc/myProjects/dataScience_stockMarket/data_1/data_master_1.csv")

# loading libraries
	
	require(forecast)
	require(astsa)
	require(ts)

# outputting data head and structure
	
	head(dataMaster)
	str(dataMaster)

# assigning data column to variable container
	
	gdp <- dataMaster$gdp_us

# printing string and mean of gdp
	
	print("This is the average GDP of the US from years 1995 to 2015 ---------------------------------------------------")
	mean(gdp)

# running ts function on data column, with start paramenter, and frequency
	
	sp_500 <- ts(dataMaster$sp_500, start=c(1995, 1), freq=12)
	nasdaq <- ts(dataMaster$nasdaq, start=c(1995, 1), freq=12)
	nyse <- ts(dataMaster$nyse, start=c(1995, 1), freq=12)
	gdp_us <- ts(dataMaster$gdp_us, start=c(1995, 1), freq=12)
	cpi <- ts(dataMaster$cpi, start=c(1995, 1), freq=12)
	ppi <- ts(dataMaster$ppi, start=c(1995, 1), freq=12)


# printing some data

	print("Here are S&P 500s values for 1995-2015! ---------------------------------------------------")
	sp_500
	
	print("Here are nasdaq values for 1995-2015! ---------------------------------------------------")
	nasdaq

	print("Here are gdp_us values for 1995-2015! ---------------------------------------------------")
	gdp_us

	print("Here are cpi values for 1995-2015! ---------------------------------------------------")
	cpi

	print("Here are ppi values for 1995-2015! ---------------------------------------------------")
	ppi

# printing mean of sp_500

	print("This is the average sp_500 value from years 1995 to 2015 ---------------------------------------------------")
	mean(sp_500)

# printing standard deviation of sp_500
	

	print("This is the standard deviation of sp_500 values from years 1995 to 2015 ---------------------------------------------------")
	sd(sp_500)

# plotting sp_500
	
	print("Outputting plot of sp_500 ---------------------------------------------------")
	plot.ts(sp_500, main="Plotting sp_500 values")

# linear model on sp_500 as a function of other variables

	fit1 <- lm(sp_500 ~ m1 + m2 + consumerSentiment + imports + oilPrices + ppi + exports + cpi + unemploymentRate + fedFunds + capUtilization + nyse, data = dataMaster)
	print("Printing summary of fit1 ---------------------------------------------------")
	summary(fit1)

# linear model on sp_500 as a function of fewer variables

	fit2 <- lm(sp_500 ~ m1 + consumerSentiment + ppi + exports + unemploymentRate + capUtilization + nyse,  data = dataMaster)
	print("Printing summary of fit2 ---------------------------------------------------")
	summary(fit2)


# further action items
# learn ts(), lm(), summary(), arima(), 
# http://people.duke.edu/~rnau/411arim.htm