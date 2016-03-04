> setwd("/home/dc/myProjects/dataScience_stockMarket/data_1/data_master_1.csv")
> master<-read.csv("data_master_1.csv")

#BIC prodecure: drop variable with smallest AIC value until <none> has smallest AIC

#Backwards Elimination: drop variable with smallest |t-value| until all variables have p-value < 0.01

#Start with model with all variables
fit.1<-lm(nasdaq~m1+m2+consumerSentiment+imports+inflation+oilPrices+ppi+exports+cpi+unemploymentRate+fedFunds+capUtilization+gdp_us, data=master)

#Backwards Elimination
summary(fit.1)

    # OUTPUT

        #   Call:
        #   lm(formula = nasdaq ~ m1 + m2 + consumerSentiment + imports + 
        #       inflation + oilPrices + ppi + exports + cpi + unemploymentRate + 
        #       fedFunds + capUtilization + gdp_us, data = master)
        #   
        #   Residuals:
        #       Min      1Q  Median      3Q     Max 
        #   -999.03 -196.38  -40.68  158.67 1912.82 
        #   
        #   Coefficients:
        #                       Estimate Std. Error t value Pr(>|t|)    
        #   (Intercept)        1.479e+04  4.050e+03   3.652 0.000320 ***
        #   m1                 9.518e-01  6.873e-01   1.385 0.167392    
        #   m2                 1.261e-01  3.963e-01   0.318 0.750636    
        #   consumerSentiment  2.638e+01  4.724e+00   5.584 6.40e-08 ***
        #   imports           -2.989e-03  7.080e-03  -0.422 0.673263    
        #   inflation          2.643e+02  6.889e+01   3.837 0.000160 ***
        #   oilPrices         -5.056e+00  3.987e+00  -1.268 0.205955    
        #   ppi                7.511e+00  1.380e+01   0.544 0.586709    
        #   exports            6.622e-02  7.731e-03   8.566 1.36e-15 ***
        #   cpi               -1.190e+02  3.178e+01  -3.746 0.000226 ***
        #   unemploymentRate  -1.172e+04  3.637e+03  -3.222 0.001453 ** 
        #   fedFunds           1.424e+04  2.899e+03   4.912 1.68e-06 ***
        #   capUtilization    -1.560e+04  2.292e+03  -6.805 8.12e-11 ***
        #   gdp_us             9.246e+02  2.250e+02   4.109 5.46e-05 ***
        #   ---
        #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
        #   
        #   Residual standard error: 344.3 on 238 degrees of freedom
        #   Multiple R-squared:  0.8935,	Adjusted R-squared:  0.8876 
        #   F-statistic: 153.5 on 13 and 238 DF,  p-value: < 2.2e-16

#Drop variable with smallest |t-value| >> drop M2


#BIC procedure
n <- nrow(master)
drop1(fit.1, k=log(n))

    # OUTPUT

        #   Single term deletions
        #   
        #   Model:
        #   nasdaq ~ m1 + m2 + consumerSentiment + imports + inflation + 
        #       oilPrices + ppi + exports + cpi + unemploymentRate + fedFunds + 
        #       capUtilization + gdp_us
        #                     Df Sum of Sq      RSS    AIC
        #   <none>                         28215373 3007.2
        #   m1                 1    227364 28442737 3003.6
        #   m2                 1     12001 28227374 3001.7
        #   consumerSentiment  1   3696364 31911737 3032.6
        #   imports            1     21132 28236505 3001.8
        #   inflation          1   1745107 29960480 3016.7
        #   oilPrices          1    190680 28406053 3003.3
        #   ppi                1     35129 28250502 3001.9
        #   exports            1   8697914 36913287 3069.3
        #   cpi                1   1663488 29878861 3016.1
        #   unemploymentRate   1   1230450 29445823 3012.4
        #   fedFunds           1   2859904 31075277 3025.9
        #   capUtilization     1   5490460 33705833 3046.4
        #   gdp_us             1   2002008 30217381 3018.9

#Drop variable with smallest AIC value >> Drop M2


#update model to drop M2
> fit.1a <- update(fit.1,.~.-m2)

#Backwards Elimination
> summary(fit.1a)

Call:
lm(formula = nasdaq ~ m1 + consumerSentiment + imports + inflation + 
    oilPrices + ppi + exports + cpi + unemploymentRate + fedFunds + 
    capUtilization + gdp_us, data = master)

Residuals:
    Min      1Q  Median      3Q     Max 
-989.66 -199.00  -35.46  156.06 1913.10 

Coefficients:
                    Estimate Std. Error t value Pr(>|t|)    
(Intercept)        1.389e+04  2.896e+03   4.797 2.84e-06 ***
m1                 1.161e+00  1.998e-01   5.810 1.98e-08 ***
consumerSentiment  2.588e+01  4.451e+00   5.815 1.93e-08 ***
imports           -3.518e-03  6.869e-03  -0.512 0.608957    
inflation          2.643e+02  6.877e+01   3.844 0.000155 ***
oilPrices         -5.598e+00  3.598e+00  -1.556 0.121105    
ppi                6.615e+00  1.348e+01   0.491 0.624122    
exports            6.729e-02  6.943e-03   9.692  < 2e-16 ***
cpi               -1.121e+02  2.316e+01  -4.841 2.32e-06 ***
unemploymentRate  -1.151e+04  3.571e+03  -3.223 0.001446 ** 
fedFunds           1.411e+04  2.867e+03   4.924 1.59e-06 ***
capUtilization    -1.565e+04  2.280e+03  -6.866 5.66e-11 ***
gdp_us             9.483e+02  2.119e+02   4.475 1.18e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 343.7 on 239 degrees of freedom
Multiple R-squared:  0.8934,	Adjusted R-squared:  0.8881 
F-statistic: 166.9 on 12 and 239 DF,  p-value: < 2.2e-16

#PPI has smallest |t-value| >> drop


#BIC procedure
> drop1(fit.1a, k=log(n))
Single term deletions

Model:
nasdaq ~ m1 + consumerSentiment + imports + inflation + oilPrices + 
    ppi + exports + cpi + unemploymentRate + fedFunds + capUtilization + 
    gdp_us
                  Df Sum of Sq      RSS    AIC
<none>                         28227374 3001.7
m1                 1   3987460 32214834 3029.5
consumerSentiment  1   3994021 32221396 3029.6
imports            1     30990 28258364 2996.5
inflation          1   1744938 29972312 3011.3
oilPrices          1    285838 28513212 2998.7
ppi                1     28433 28255807 2996.4
exports            1  11094475 39321849 3079.7
cpi                1   2768296 30995670 3019.8
unemploymentRate   1   1226760 29454134 3006.9
fedFunds           1   2863101 31090475 3020.5
capUtilization     1   5567563 33794937 3041.6
gdp_us             1   2365516 30592891 3016.5
# PPI has smallest AIC >> drop PPI

#update model to drop PPI
> fit.1b <- update(fit.1a,.~.-ppi)

#Backwards Elimination
> summary(fit.1b)

Call:
lm(formula = nasdaq ~ m1 + consumerSentiment + imports + inflation + 
    oilPrices + exports + cpi + unemploymentRate + fedFunds + 
    capUtilization + gdp_us, data = master)

Residuals:
    Min      1Q  Median      3Q     Max 
-985.81 -197.27  -33.69  152.47 1901.02 

Coefficients:
                    Estimate Std. Error t value Pr(>|t|)    
(Intercept)        1.364e+04  2.846e+03   4.792 2.89e-06 ***
m1                 1.145e+00  1.967e-01   5.819 1.89e-08 ***
consumerSentiment  2.539e+01  4.330e+00   5.864 1.49e-08 ***
imports           -2.335e-03  6.421e-03  -0.364 0.716466    
inflation          2.603e+02  6.817e+01   3.818 0.000171 ***
oilPrices         -4.676e+00  3.064e+00  -1.526 0.128313    
exports            6.734e-02  6.931e-03   9.716  < 2e-16 ***
cpi               -1.040e+02  1.614e+01  -6.443 6.36e-10 ***
unemploymentRate  -1.115e+04  3.491e+03  -3.195 0.001587 ** 
fedFunds           1.432e+04  2.833e+03   5.054 8.57e-07 ***
capUtilization    -1.521e+04  2.090e+03  -7.279 4.78e-12 ***
gdp_us             8.893e+02  1.742e+02   5.104 6.77e-07 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 343.1 on 240 degrees of freedom
Multiple R-squared:  0.8933,	Adjusted R-squared:  0.8884 
F-statistic: 182.7 on 11 and 240 DF,  p-value: < 2.2e-16

#Imports has smallest |t-value| >> drop Imports

#BIC Procedure
> drop1(fit.1b,k=log(n))
Single term deletions

Model:
nasdaq ~ m1 + consumerSentiment + imports + inflation + oilPrices + 
    exports + cpi + unemploymentRate + fedFunds + capUtilization + 
    gdp_us
                  Df Sum of Sq      RSS    AIC
<none>                         28255807 2996.4
m1                 1   3985969 32241776 3024.2
consumerSentiment  1   4048164 32303971 3024.7
imports            1     15566 28271373 2991.1
inflation          1   1716633 29972440 3005.8
oilPrices          1    274181 28529988 2993.4
exports            1  11114550 39370357 3074.5
cpi                1   4886882 33142689 3031.1
unemploymentRate   1   1201672 29457479 3001.4
fedFunds           1   3007542 31263349 3016.4
capUtilization     1   6237909 34493716 3041.2
gdp_us             1   3066947 31322754 3016.9

#Imports had smallest AIC >> drop Imports

#update model to drop Imports
> fit.1c <- update(fit.1b,.~.-imports)

#Backwards Elimination
> summary(fit.1c)

Call:
lm(formula = nasdaq ~ m1 + consumerSentiment + inflation + oilPrices + 
    exports + cpi + unemploymentRate + fedFunds + capUtilization + 
    gdp_us, data = master)

Residuals:
    Min      1Q  Median      3Q     Max 
-994.98 -205.24  -39.33  151.03 1893.83 

Coefficients:
                    Estimate Std. Error t value Pr(>|t|)    
(Intercept)        1.431e+04  2.163e+03   6.617 2.35e-10 ***
m1                 1.155e+00  1.944e-01   5.940 9.92e-09 ***
consumerSentiment  2.585e+01  4.137e+00   6.248 1.87e-09 ***
inflation          2.630e+02  6.764e+01   3.889  0.00013 ***
oilPrices         -5.205e+00  2.693e+00  -1.933  0.05440 .  
exports            6.640e-02  6.417e-03  10.348  < 2e-16 ***
cpi               -1.042e+02  1.610e+01  -6.471 5.41e-10 ***
unemploymentRate  -1.120e+04  3.483e+03  -3.216  0.00148 ** 
fedFunds           1.438e+04  2.822e+03   5.094 7.07e-07 ***
capUtilization    -1.568e+04  1.646e+03  -9.527  < 2e-16 ***
gdp_us             8.512e+02  1.390e+02   6.126 3.65e-09 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 342.5 on 241 degrees of freedom
Multiple R-squared:  0.8932,	Adjusted R-squared:  0.8888 
F-statistic: 201.6 on 10 and 241 DF,  p-value: < 2.2e-16

#Oil Prices has smallest |t-value| >> drop Oil Prices

> drop1(fit.1c,k=log(n))
Single term deletions

Model:
nasdaq ~ m1 + consumerSentiment + inflation + oilPrices + exports + 
    cpi + unemploymentRate + fedFunds + capUtilization + gdp_us
                  Df Sum of Sq      RSS    AIC
<none>                         28271373 2991.1
m1                 1   4138625 32409998 3020.0
consumerSentiment  1   4578874 32850247 3023.4
inflation          1   1773846 30045219 3000.9
oilPrices          1    438349 28709722 2989.4
exports            1  12561758 40833131 3078.2
cpi                1   4911416 33182789 3025.9
unemploymentRate   1   1213206 29484579 2996.1
fedFunds           1   3044423 31315796 3011.3
capUtilization     1  10647865 38919238 3066.1
gdp_us             1   4401890 32673264 3022.0

#Oil Prices has smallest AIC >> drop oil prices

#update model to drop Oil Prices
> fit.1d <- update(fit.1c,.~.-oilPrices)

#Backwards Elimination
> summary(fit.1d)

Call:
lm(formula = nasdaq ~ m1 + consumerSentiment + inflation + exports + 
    cpi + unemploymentRate + fedFunds + capUtilization + gdp_us, 
    data = master)

Residuals:
    Min      1Q  Median      3Q     Max 
-943.58 -209.00  -40.78  174.67 1914.23 

Coefficients:
                    Estimate Std. Error t value Pr(>|t|)    
(Intercept)        1.527e+04  2.117e+03   7.212 7.02e-12 ***
m1                 1.358e+00  1.647e-01   8.244 1.06e-14 ***
consumerSentiment  2.722e+01  4.099e+00   6.642 2.03e-10 ***
inflation          2.124e+02  6.273e+01   3.387 0.000825 ***
exports            5.998e-02  5.522e-03  10.863  < 2e-16 ***
cpi               -1.080e+02  1.607e+01  -6.718 1.31e-10 ***
unemploymentRate  -1.173e+04  3.491e+03  -3.359 0.000910 ***
fedFunds           1.554e+04  2.773e+03   5.604 5.69e-08 ***
capUtilization    -1.662e+04  1.581e+03 -10.511  < 2e-16 ***
gdp_us             8.770e+02  1.391e+02   6.306 1.35e-09 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 344.4 on 242 degrees of freedom
Multiple R-squared:  0.8916,	Adjusted R-squared:  0.8876 
F-statistic: 221.1 on 9 and 242 DF,  p-value: < 2.2e-16

#All p-values are < 0.01 so all terms are significant, do not need to drop anymore

> drop1(fit.1d,k=log(n))
Single term deletions

Model:
nasdaq ~ m1 + consumerSentiment + inflation + exports + cpi + 
    unemploymentRate + fedFunds + capUtilization + gdp_us
                  Df Sum of Sq      RSS    AIC
<none>                         28709722 2989.4
m1                 1   8063275 36772997 3046.3
consumerSentiment  1   5233357 33943079 3026.1
inflation          1   1360913 30070635 2995.6
exports            1  13999803 42709525 3084.0
cpi                1   5353706 34063428 3027.0
unemploymentRate   1   1338182 30047904 2995.4
fedFunds           1   3725711 32435433 3014.6
capUtilization     1  13108134 41817856 3078.7
gdp_us             1   4716861 33426583 3022.2

# AIC under <none> is smallest, all terms are significant. Do not need to drop anymore.