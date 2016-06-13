# dataScience_stockMarket

WHY:

Data is awesome stuff. We love playing with data, stats, models...

We want to prefict things.

--

WHAT:

This project studies the effect that economic indicators have on stock indices including the Nasdaq Composite Index, S&P 500 Index, and New York Stock Exchange Index.

--

HOW:

This project uses R, HTML, CSS, and JavaScript.

Using tons of data, linear models, time series models, and other stuff!

--

WHO:

 - David Campos

 - Raul Eulogio

 - Kim Specht

 - Nathan Fritter

--

TERMINOLOGY & CONCEPTS

https://en.wikipedia.org/wiki/Probability

https://en.wikipedia.org/wiki/Statistics

https://en.wikipedia.org/wiki/Computer_programming

https://en.wikipedia.org/wiki/Finance

- What is the mean?

https://en.wikipedia.org/wiki/Mean

- What is probability distribution?

https://en.wikipedia.org/wiki/Probability_distribution

- What is variance?

https://en.wikipedia.org/wiki/Variance

- What is standard deviation?

https://en.wikipedia.org/wiki/Standard_deviation

- What is T statistic?

https://en.wikipedia.org/wiki/Test_statistic

- What is standard score?

https://en.wikipedia.org/w/index.php?title=Standard_score&redirect=no

- What is CHI SQUARED test?

https://en.wikipedia.org/wiki/Chi-squared_test

- What is a confidence interval?

https://en.wikipedia.org/wiki/Confidence_interval

- What is factor analysis?

https://en.wikipedia.org/wiki/Factor_analysis

- What is a quantile?  

https://en.wikipedia.org/wiki/Quantile

- What is a percentile?

https://en.wikipedia.org/wiki/Percentile

- What is the central limit theorem?

https://en.wikipedia.org/wiki/Central_limit_theorem

- What is R?

https://www.r-project.org/

- What is time series?  

https://en.wikipedia.org/wiki/Time_series

- What is distributed lag?  

https://en.wikipedia.org/wiki/Distributed_lag

- What is auto regressive model?

https://en.wikipedia.org/wiki/Autoregressive_model

- What is moving average?  

https://en.wikipedia.org/wiki/Moving_average

- What is auto correlation?  

https://en.wikipedia.org/wiki/Autocorrelation

- What is exponential smoothing?

https://en.wikipedia.org/wiki/Exponential_smoothing

- What is ACF?

http://www.inside-r.org/r-doc/stats/acf

- What is PACF?

https://onlinecourses.science.psu.edu/stat510/node/62

- What is multi variate time series?

http://www.itl.nist.gov/div898/handbook/pmc/section4/pmc451.htm

http://faculty.washington.edu/ezivot/econ584/notes/multivariatetimeseries.pdf

http://lipas.uwasa.fi/~bepa/Multivariate.pdf

- What are multi variate forecasting methods?

http://homepage.univie.ac.at/robert.kunst/prognos4.pdf

http://www.robots.ox.ac.uk/~sjrob/Pubs/TechReport_PARG0803.pdf

https://cran.r-project.org/web/packages/MTS/MTS.pdf

--

RUN THIS YOURSELF (specific instructions for UBUNTU 14.04)

- Clone this project in your machine using Github

https://help.github.com/articles/cloning-a-repository/

- Install R on Ubuntu 14.04. Open your terminal, and enter the following commands:
		
		sudo add-apt-repository ppa:marutter/rrutter

		sudo apt-get update

		sudo apt-get install r-base r-base-dev

http://askubuntu.com/questions/496788/you-have-held-broken-package-while-trying-to-install-r

- Try R

	- Check out datacamp.com or google "R tutorial" to learn basic R commands

	- You can also just enter the following

		2 + 2

- Run test project. General form to run a file directly from the terminal:
		
		Rscript fileName.R

- Now test using testScript.R

	- cd to your working directory - google "basic linux terminal commands" if you don't know what I meant... here is a link anyway

http://www.tecmint.com/useful-linux-commands-for-newbies/


- Enter this command to test sample R script:

		Rscript testScript.R

- Now run the actual project, for this, go to the scriptsR directory and run the linear and time series models

- Explore the data in data_1 directory

- Explore the other directories

- Give feedback and contribute to this project :)


--

PROCESS, ANALYSIS, & SOME THEORY

--

- First we re-searched similar projects to read & learn from

- Then we collected data that we thought might be relevant in estimating / predicting our stock market variables

	- m1

	- m2

	- consumerSentiment

	- inflation

	- imports

	- oilPrices

	- ppi

	- exports

	- cpi

	- unemploymentRate

	- fedFunds

	- capUtilization

	- sp_500Dividends

	- nasdaq

	- nyse

	- sp_500

	- gdp_us

- Then we cleaned and formatted our data. Check data_1 directory to see the data files used in the analysis

https://docs.google.com/spreadsheets/d/1STR_wkkIUQVZT-PK0XAdEtmQQnjOzn0DkUPhTyWfolM/edit?usp=sharing

- Then we fit various statistical models and ran several tests

	- LINEAR MODEL

		- T STATISTIC

			- we use the t-statistic to test the significance of our variables in the model. 

			- the t-statistic is a ratio of the departure of an estimated parameter from its notional value and its standard error. it is used in hypothesis testing, for example, in the Student's t-test, in the augmented Dickey-Fuller test, and in bootstrapping.

		- CONFIDENCE INTERVAL

			- we used confidence interval selection to further test the signficance of our variables in the models.

			- in statistics, a confidence interval (CI) is a type of interval estimate of a population parameter. It is an observed interval (i.e. it is calculated from the observations), in principle different from sample to sample, that frequently includes the value of an unobserved parameter of interest if the experiment is repeated.

			- confidence intervals consist of a range of values (interval) that act as good estimates of the unknown population parameter; however, the interval computed from a particular sample does not necessarily include the true parameter. 

			- when we say "we are 99% confident that the true value of the parameter is in our confidence interval", we express that the 99% of the hypothetically observed confidence intervals will hold the true value of the parameter.

			- confidence intervals were introduced to statistics by Jerzy Neyman in a paper published in 1937.

		- BACKWARDS ELIMINATION (of non-significant variables)

			- basically, we include all the variables in the available data set and drop variables until P VALUE for the intercept coefficient is the largest (with respect to other variables' p value)

		- BIC Procedure

			- in statistics, the bayesian information criterion (BIC) os a criterion for model selection among a finite set of models; the model with the lowest BIC is preferred. It is based, in part, on the likelyhood function and it is closely related to the akaike information criterion (AIC).

			- when fitting models, it is possible to increase the likelihood by adding parameter5s, but doing so may result in overfitting. Both BIC and AIC resolve this problem by introducing a penalty term for the number of parameters. in the model. The penalty term is larger in BIC than in AIC.

			- the BIC method was developed by Gideon E. Schwarz and published in a 1978 paper, where he gave a Bayesian argument for adopting it.

			- basically, when the AIC value for the intercept is the smallest (with respect to other variables' AIC value), we stop dropping variables.

	- TIME SERIES MODEL

		- a time series is a sequence of data points made:

			- over a continuous time interval.

			- out of successive measurements over an interval.

			- using equal spacing between every two consecutive measurements.

			- with each time unit within the time interal having at most one data point.

		- examples include ocean tides, counts of sunspots, and the daily closing value of the Dow Jones Industrial Average.

		- panel data is the general class, a multidimensional data set, whereas a time series data set is a one-dimensional panel (as is a cross-sectional dataset).

		- time series analysis comprises methods for analysing time series data in order to extract meaningful statistics and other characteristics of the data. time series forecasting is the use of a model to predict future values based on previously observed values. while regression analysis is often employed in such a way to test theories tha the current values of one of more independent time series affect the current value of another time series, this type of analysis of time series is not called "time series analysis", which focuses on comparing values of a single time series or multiple dependent time series at different points in time.

		- methods for time series analyses may be divided into two classes: frequency domain methods (which includes spectral analysis and wavelet analysis) and time domain methods (which includes auto-correlation and cross-correlation analysis). time series analysis techniques may be divided into parametric (which assume that underlying stationary stochastic process has a certain strcucture which can be explained using a small number of parameters (e.g. autoregressive or moving average model)) and non-parametric methods (which explicitly estimate the covariance or the spectrum of the process without assuming that the process has any particular structure). methods of time series may also be divided into linear and non-linear, univariate and multivariate.

		- auto.arima method

			- returns best ARIMA model according to either AIC, AICc or BIC value. The function conducts a search over possible model within the order constraints provided. ARIMA stands for autoregressive integrated moving average, and it is a generalization of the ARMA model. These models are fitted to time series data either to understand the data or to predict future points (forecasting).

		- box cox transformation

			- normally distributed data is needed to use a number of statistical analysis tools, such as individuals control charts, Cp/Cpk analysis, t-tests and analysis of variance (ANOVA). When data is not normally distributedm the cause for non-normality should be determined and appropriate remedial actions should be taken. 

			- here is an intro of remedial actions for non-normal data: https://www.isixsigma.com/tools-templates/normality/dealing-non-normal-data-strategies-and-tools/

			- data transformation, and particularly, the Box-Cox power transformation, is one of those remedial actions that may help to make data normal.

			- the statisticians George Box and David Cox developed a procedure to identify an appropriate exponent (Lambda = l) to use to transform data into a "normal shape". The Lambda value indicates the power to which all data should be raised. In order to do this, the Box-Cox power transformation searches from Lambda =-5 to Lambda = 5 until the best value is found.


https://en.wikipedia.org/wiki/Autoregressive_integrated_moving_average

https://en.wikipedia.org/wiki/Power_transform

http://onlinestatbook.com/2/transformations/box-cox.html

https://www.isixsigma.com/tools-templates/normality/making-data-normal-using-box-cox-power-transformation/

- Then we made some predictions with both models, outputting relevant plots and other graphics to PDF files

- Finally, we wrote some basic d3.js code to visualize our data

--

We listened to some of this music, we thought you might like it :)

 - lost in music by cosmic gate

 - SOL by solar fiends

 - concept 1 by komodo

 - monday paracetemol by ukrich schnauss

 - feeling good by nina simone

 - the prudence of evolution by ernest gonzales

 - trophies by GANZ

--