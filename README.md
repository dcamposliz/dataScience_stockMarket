# dataScience_stockMarket

This project studies the effect that economic indicators have on stock indices including the Nasdaq Composite Index, S&P 500 Index, and New York Stock Exchange Index.

This project uses R, HTML, CSS, and JavaScript.


To understand this project, you should answer these questions for yourself:

- Do I like data, statistics, probability?

- Why?

- What is the mean?

- What is variance?

- What is standard error?

- What is T value?

- What is Z value?

- What is CHI SQUARED value?

- What is a confidence interval?

- What is level?

- What is a quantile?  

https://en.wikipedia.org/wiki/Quantile

- What is a percentile?

- What is the central limit theorem?

- What is R?

- What is time series?  

https://en.wikipedia.org/wiki/Time_series

- What is distributed lag?  

https://en.wikipedia.org/wiki/Distributed_lag

- What is auto regressive?

- What is moving average?  

https://en.wikipedia.org/wiki/Moving_average

- What is auto correlation?  

https://en.wikipedia.org/wiki/Autocorrelation

- What is exponential smoothing?

- What is ACF?

- What is PACF?

- What is real analysis?  

https://en.wikipedia.org/wiki/Real_analysis

- What is multi variate time series?

- What are multi variate forecasting methods?

http://homepage.univie.ac.at/robert.kunst/prognos4.pdf
http://www.robots.ox.ac.uk/~sjrob/Pubs/TechReport_PARG0803.pdf
https://cran.r-project.org/web/packages/MTS/MTS.pdf


--

The process to yield this project includes:

	- Collecing data
	- Cleaning / formatting data
	- Fitting dependent and independent variables (data) into statistic models, using statistical tools with R
		- T-test significance
		- Confidence interval selection
		- Backwards elimination of insignificant variables
			- - when the p-value for the intercept is the largest (with respect to other variables' p-value), we stop dropping variables
		- BIC Procedure
			- when the AIC value for the intercept is the smallest (with respect to other variables' AIC value), we stop dropping variables

--

 - Clone this project in your machine using Github
	- You gotta google the "how to" for this one :)

--

 - Install R on Ubuntu 14.04. Open your terminal, and enter the following commands:
		sudo add-apt-repository ppa:marutter/rrutter
		sudo apt-get update
		sudo apt-get install r-base r-base-dev
 - source: http://askubuntu.com/questions/496788/you-have-held-broken-package-while-trying-to-install-r

--

 - Try R
	 - Check out datacamp.com or google "R tutorial" to learn basic R commands
	 - You can also just enter the following
		2 + 2

--

 - Run test project. General form to run a file directly from the terminal:
		Rscript fileName.R

 - Now test using testScript.R
	- cd to your working directory - google "basic linux terminal commands" if you don't know what I meant... 
	- you gotta learn to google shit

 - Rnter the following command to test sample R script:
		Rscript testScript.R

--

 - Now run the actual project 
		Rscript script.R

--

 - Read the code. Open this file with sublime: 
		dataScience_stockMarket/script.R

 - Read the code, write down questions, answer your questions from doing several google searches

--

 - Read the data. Open this file with sublime: 
		dataScience_stockMarket/data_1/data_master_1.csv

Read the data, write down questions, run your own R scripts to answer your own questions
	
--

Make contributions to this project :)

--

WHAT IS NEXT?

 - LEARN STATISTICAL MODELS:
 	- https://www3.nd.edu/~steve/Rcourse/Lecture7v1.pdf
 	- http://arxiv.org/pdf/1302.6613.pdf
 	- 

 - pulling stock data in "real-time"
 	- http://finance.yahoo.com/
 	- http://finance.yahoo.com/q?s=^ixic
 	- http://finance.yahoo.com/q?s=^nya
 	- http://finance.yahoo.com/q?s=^gspc
 	- https://www.youtube.com/watch?v=sXyXciMYZZw

 - HOW TO DO IT:
 	- http://stackoverflow.com/questions/10040954/alternative-to-google-finance-api
 	- http://stackoverflow.com/questions/417453/best-most-comprehensive-api-for-stocks-financial-data
 	- https://www.quandl.com/blog/api-for-stock-data
 	- https://github.com/hongtaocai/googlefinance

 	Awesome:
 	- https://github.com/markitondemand/DataApis/blob/master/MarkitQuoteServiceSample.js#L46
 	- http://dev.markitondemand.com/MODApis/
 	- https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest

 	Visualizations:
 	- https://github.com/arnauddri/d3-stock

 	Plot to work on ubuntu:
 	- http://ubuntuforums.org/showthread.php?t=317490
 	- http://stackoverflow.com/questions/6838851/how-to-copy-plots-in-r-when-working-in-ubuntu
