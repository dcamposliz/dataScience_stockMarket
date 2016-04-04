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


	dataMaster_matrix <- matrix(dataMaster_df, ncol = 18, byrow=TRUE)
	print("here goes the matrix:")
	dataMaster_matrix

#	MATRIX EXAMPLE
#	box_office_all <- c(461, 314.4, 290.5, 247.9, 309.3, 165.8)
#	movie_names <- c("A New Hope","The Empire Strikes Back","Return of the Jedi")
#	col_titles <- c("US","non-US")
#	star_wars_matrix <- matrix(box_office_all, nrow = 3, byrow = TRUE, dimnames = list(movie_names, col_titles))
#	star_wars_matrix


	dataMaster_matrix_fromDF <- as.matrix(dataMaster_df)
	dataMaster_matrix_fromDF