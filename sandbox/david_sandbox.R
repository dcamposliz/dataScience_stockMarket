
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
			
			print("some quick summary")
			summary(fit_nasdaq_0)


#####################
#####################
#####################
#####################
#####################
#####################
#####################
#####################
#####################
#####################
#####################
#####################
#####################


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

#######################################
#######################################
#######################################

#	making a matrix for kim

	# Construct matrix
	box_office_all <- c(461, 314.4, 290.5, 247.9, 309.3, 165.8)
	movie_names <- c("A New Hope","The Empire Strikes Back","Return of the Jedi")

	
	col_titles <- c("US","non-US")
	

	star_wars_matrix <- matrix(box_office_all, nrow = 3, byrow = TRUE, dimnames = list(movie_names, col_titles))



	dataMaster_matrix <- matrix()