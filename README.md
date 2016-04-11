# dataScience_stockMarket

WHY:

Data is awesome stuff. We like playing with data, models, and more stuff.

We want to prefict things.

WHAT:

This project studies the effect that economic indicators have on stock indices including the Nasdaq Composite Index, S&P 500 Index, and New York Stock Exchange Index.

HOW:

This project uses R, HTML, CSS, and JavaScript.


--
--


CONTEXT FOR THIS PROJECT

--

To understand this project, you should answer these questions for yourself:

- Do I love data? Why?

- Do I have a basic understanding of probability? statistics? programming? finance?

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


PROJECT PROCESS

--

- Finding similar existing projects to read & learn from

- Collecing data

- Selecting variables to play with

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

- Fitting dependent and independent variables (data) into statistic models, using statistical tools in R

	- T test significance

	- Confidence interval selection

	- Backwards elimination of insignificant variables

		- when the p value for the intercept is the largest (with respect to other variables' p value), we stop dropping variables

	- BIC Procedure

		- when the AIC value for the intercept is the smallest (with respect to other variables' AIC value), we stop dropping variables

- Run a fuckload of models and do a bunch of tests until we find the best one

- Do some kickass predictions and visualizations

- Mount it all on the web


--
--


MAYBE THIS IS OUR MODEL?

--

http://stackoverflow.com/questions/1714280/multivariate-time-series-modelling-in-r

https://cran.r-project.org/web/views/TimeSeries.html

https://cran.r-project.org/web/packages/MTS/MTS.pdf


--
--


GETTING THIS RUNNING ON YOUR SYSTEM

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

- Now run the actual project:

		Rscript script.R

- Read the code. Open this file with sublime:

		dataScience_stockMarket/script.R

- Read the code, write down questions, answer your questions from doing several google searches

- Read the data. Open this file with sublime:

		dataScience_stockMarket/data_1/data_master_1.csv

	- Read the data, write down questions, run your own R scripts to answer your own questions

- Make contributions to this project :)


--
--


WHAT IS NEXT?

--

- Learn statistical models

https://www3.nd.edu/~steve/Rcourse/Lecture7v1.pdf

http://arxiv.org/pdf/1302.6613.pdf

- Pull stock data in "real-time"

http://finance.yahoo.com/

http://finance.yahoo.com/q?s=^ixic

http://finance.yahoo.com/q?s=^nya

http://finance.yahoo.com/q?s=^gspc

https://www.youtube.com/watch?v=sXyXciMYZZw

http://stackoverflow.com/questions/10040954/alternative-to-google-finance-api

http://stackoverflow.com/questions/417453/best-most-comprehensive-api-for-stocks-financial-data

https://www.quandl.com/blog/api-for-stock-data

https://github.com/hongtaocai/googlefinance

- Awesome shit

https://github.com/markitondemand/DataApis/blob/master/MarkitQuoteServiceSample.js#L46

http://dev.markitondemand.com/MODApis/

https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest

- Visualizations:

https://github.com/arnauddri/d3-stock