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

Using tons of data, linear models, time series models, and other random stuff!

--
--


GET AN UNDERSTANDING OF THE FOLLOWING TERMS & CONCEPTS IN ORDER TO BETTER UNDERSTAND THIS PROJECT :)

--

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
--


GETTING ENVIRONMENT RUNNING ON YOUR SYSTEM (specific instructions for UBUNTU 14.04)

--

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
--


PROJECT PROCESS

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

- Cleaning / formatting data

https://docs.google.com/spreadsheets/d/1STR_wkkIUQVZT-PK0XAdEtmQQnjOzn0DkUPhTyWfolM/edit?usp=sharing

- Fitting statistical models using our data and a variety of tests

	- LINEAR MODEL

		- T test significance

		- Confidence interval selection

		- Backwards elimination (of non-significant variables)

			- when the p value for the intercept is the largest (with respect to other variables' p value), we stop dropping variables

		- BIC Procedure

			- in statistics, the bayesian information criterion (BIC) os a criterion for model selection among a finite set of models; the model with the lowest BIC is preferred. It is based, in part, on the likelyhood function and it is closely related to the akaike information criterion (AIC).

			- when fitting models, it is possible to increase the likelihood by adding parameter5s, but doing so may result in overfitting. Both BIC and AIC resolve this problem by introducing a penalty term for the number of parameters. in the model. The penalty term is larger in BIC than in AIC.

			- the BIC method was developed by Gideon E. Schwarz and published in a 1978 paper, where he gave a Bayesian argument for adopting it.

			- basically, when the AIC value for the intercept is the smallest (with respect to other variables' AIC value), we stop dropping variables.

	- TIME SERIES MODEL


		- auto.arima method

			- returns best ARIMA model according to either AIC, AICc or BIC value. The function conducts a search over possible model within the order constraints provided. ARIMA stands for autoregressive integrated moving average, and it is a generalization of the ARMA model. These models are fitted to time series data either to understand the data or to predict future points (forecasting).

https://en.wikipedia.org/wiki/Autoregressive_integrated_moving_average


		- box cox transformation

			- normally distributed data is needed to use a number of statistical analysis tools, such as individuals control charts, Cp/Cpk analysis, t-tests and analysis of variance (ANOVA). When data is not normally distributedm the cause for non-normality should be determined and appropriate remedial actions should be taken. 

			- here is an intro of remedial actions for non-normal data: https://www.isixsigma.com/tools-templates/normality/dealing-non-normal-data-strategies-and-tools/

			- data transformation, and particularly, the Box-Cox power transformation, is one of those remedial actions that may help to make data normal.

			- the statisticians George Box and David Cox developed a procedure to identify an appropriate exponent (Lambda = l) to use to transform data into a "normal shape". The Lambda value indicates the power to which all data should be raised. In order to do this, the Box-Cox power transformation searches from Lambda =-5 to Lambda = 5 until the best value is found.

https://en.wikipedia.org/wiki/Power_transform

http://onlinestatbook.com/2/transformations/box-cox.html

https://www.isixsigma.com/tools-templates/normality/making-data-normal-using-box-cox-power-transformation/

- Run a ton of models and do a bunch of tests until we find the best one

- Do some cool predictions and visualizations

- Mount it all on the web



--
--
