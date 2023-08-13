### Data Cleaning & Validation

Initially, all essential package for the time-series manipulation,
analysis and forecast will be installed

``` r
library(tidyverse)
library(dplyr)
library(magrittr)
library(knitr)
library(anytime)
library(pastecs)
library(tsibble)
library(DescTools)
library(forecast)
library(fabletools)
library(feasts)
library(stats)
library(tseries)
library(fable)
```

The historical data of ACB and EIB were downloaded from Investing.com
then imported to R:

``` r
ACB_raw <- read.csv("/Users/phuoctran/Desktop/Personal Projects/Forecasting Stock Returns using ARIMA/ACB Historical Data.csv")
EIB_raw <- read.csv("/Users/phuoctran/Desktop/Personal Projects/Forecasting Stock Returns using ARIMA/EIB Historical Data.csv")
```

Examine whether two datasets contain any null value:

``` r
is.null(ACB_raw)  
```

    ## [1] FALSE

``` r
is.null(EIB_raw)
```

    ## [1] FALSE

Since the console returns “FALSE” for both datasets, there is no null
value

Next, check class type of each variables:

``` r
str(ACB_raw)
```

    ## 'data.frame':    574 obs. of  7 variables:
    ##  $ Date    : chr  "Jul 31, 2022" "Jul 24, 2022" "Jul 17, 2022" "Jul 10, 2022" ...
    ##  $ Price   : chr  "24,850.0" "24,600.0" "24,350.0" "24,000.0" ...
    ##  $ Open    : chr  "24,650.0" "24,200.0" "24,100.0" "23,850.0" ...
    ##  $ High    : chr  "25,250.0" "24,700.0" "24,900.0" "24,450.0" ...
    ##  $ Low     : chr  "24,500.0" "23,900.0" "23,800.0" "23,500.0" ...
    ##  $ Vol.    : chr  "21.59M" "11.39M" "13.32M" "8.56M" ...
    ##  $ Change..: chr  "1.02%" "1.03%" "1.46%" "0.00%" ...

``` r
str(EIB_raw)
```

    ## 'data.frame':    573 obs. of  7 variables:
    ##  $ Date    : chr  "Jul 31, 2022" "Jul 24, 2022" "Jul 17, 2022" "Jul 10, 2022" ...
    ##  $ Price   : chr  "30,400.0" "30,000.0" "30,800.0" "30,950.0" ...
    ##  $ Open    : chr  "30,500.0" "30,800.0" "30,700.0" "31,200.0" ...
    ##  $ High    : chr  "31,150.0" "30,900.0" "31,000.0" "31,400.0" ...
    ##  $ Low     : chr  "29,900.0" "29,750.0" "30,450.0" "30,500.0" ...
    ##  $ Vol.    : chr  "1.25M" "1.21M" "738.80K" "768.70K" ...
    ##  $ Change..: chr  "1.33%" "-2.60%" "-0.48%" "-0.64%" ...

Owing to all variables are classifed as character, which are unable to
be calculated and plotted, a class transformation is needed. Initially,
the Date column will be transformed to class Date by using as.Date
function:

``` r
ACB_asdate <- anydate(ACB_raw$Date) %>% as.Date(format("%d/%m/%Y"))
EIB_asdate <- anydate(EIB_raw$Date) %>% as.Date(format("%d/%m/%Y"))
```

Next, the Price, Open, High, Low columns are turned into numeric class
with as.Numeric function. Nevertheless, the comma in the figure needs to
be removed first by using gsub() function:

``` r
ACB_numeric <- as.data.frame(apply(ACB_raw[,c(2:5)], 2, function(ACB_raw) {as.numeric(gsub(",", "", ACB_raw))}))
EIB_numeric <- as.data.frame(apply(EIB_raw[,c(2:5)], 2, function(EIB_raw) {as.numeric(gsub(",", "", EIB_raw))}))
```

Similarly, the “Change %” column is transformed into numeric class after
removing the “%”, and formatting into deccimal by dividing with 100:

``` r
ACB_change_numeric <- gsub("%","",ACB_raw$Change..) %>% as.numeric()/100
EIB_change_numeric <- gsub("%","",EIB_raw$Change..) %>% as.numeric()/100
```

After the class transformation, all the variables will be re-joint into
a new dataframe:

``` r
ACB <- data.frame("Date" = ACB_asdate,
                  "Price" = ACB_numeric$Price,
                  "Open" = ACB_numeric$Open,
                  "High" = ACB_numeric$High,
                  "Low" = ACB_numeric$Low,
                  "Change %" = ACB_change_numeric)

EIB <- data.frame("Date" = EIB_asdate,
                  "Price" = EIB_numeric$Price,
                  "Open" = EIB_numeric$Open,
                  "High" = EIB_numeric$High,
                  "Low" = EIB_numeric$Low,
                  "Change %" = EIB_change_numeric)
```

Double check the structure of the new dataframes:

``` r
str(ACB)
```

    ## 'data.frame':    574 obs. of  6 variables:
    ##  $ Date    : Date, format: "2022-07-31" "2022-07-24" ...
    ##  $ Price   : num  24850 24600 24350 24000 24000 ...
    ##  $ Open    : num  24650 24200 24100 23850 24400 ...
    ##  $ High    : num  25250 24700 24900 24450 24600 ...
    ##  $ Low     : num  24500 23900 23800 23500 23600 ...
    ##  $ Change..: num  0.0102 0.0103 0.0146 0 -0.0021 0.0105 0.0303 -0.0833 -0.004 -0.0055 ...

``` r
str(EIB)
```

    ## 'data.frame':    573 obs. of  6 variables:
    ##  $ Date    : Date, format: "2022-07-31" "2022-07-24" ...
    ##  $ Price   : num  30400 30000 30800 30950 31150 ...
    ##  $ Open    : num  30500 30800 30700 31200 32750 ...
    ##  $ High    : num  31150 30900 31000 31400 32750 ...
    ##  $ Low     : num  29900 29750 30450 30500 30250 ...
    ##  $ Change..: num  0.0133 -0.026 -0.0048 -0.0064 -0.0386 0.0302 0.0032 0.0313 -0.0349 -0.0762 ...

Reverse the dataframe order so that the Date can be from oldest to
latest:

``` r
ACB_sorted <-  ACB[nrow(ACB):1, ]
EIB_sorted  <- EIB[nrow(EIB):1, ]
```

Plotting the price chart of two stocks  

``` r
#ACB Price plot
ACB_sorted %>% ggplot() + aes(x = `Date`, y =`Price`,group = 1) + 
  geom_line(color = "blue", size = 0.5) +
  ggtitle("ACB Weekly Price from 03/07/2011 to 31/07/2022") +
  theme_bw() + xlab("Time [1 Week]") + ylab("ACB Price (VND)")
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](Forecasting_Stock_Price_using_ARIMA_model_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
#EIB Price plot
EIB_sorted %>% ggplot() + aes(x = `Date`, y =`Price`,group = 1) + 
  geom_line(color = "navy", size = 0.5) +
  ggtitle("EIB Weekly Price from 03/07/2011 to 31/07/2022") +
  theme_bw() + xlab("Time [1 Week]") + ylab("EIB Price (VND)")
```

![](Forecasting_Stock_Price_using_ARIMA_model_files/figure-markdown_github/unnamed-chunk-10-2.png)

It seems that ACB price data has invalid data point according to its
abnormal spike in early 2015. Consequently, that invalid value needs to
be located and removed.

``` r
summary(ACB_sorted$Price[ACB_sorted$Date > "2015-01-01" & ACB_sorted$Date < "2016-01-01"]) 
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    4146    4400    5252    5312    5613   16800

``` r
#Determine the invalid value
print(ACB_sorted$Date[ACB_sorted$Price == 16800])
```

    ## [1] "2015-02-15"

``` r
#Locating the invalid value, which at "2015-02-15"
```

Next, the row at 2015-02-15 will be dropped:

``` r
ACB_sorted <- subset(ACB_sorted,Date != "2015-02-15")
#Drop the row with Date == "2015-02-15"
```

Replot the ACB price after removing row with invalid value:

``` r
ACB_sorted %>% ggplot() + aes(x = `Date`, y =`Price`,group = 1) + 
  geom_line(color = "blue", size = 0.5) +
  ggtitle("ACB Weekly Price from 03/07/2011 to 31/07/2022") +
  theme_bw() + xlab("Time [1 Week]") + ylab("ACB Price (VND)")
```

![](Forecasting_Stock_Price_using_ARIMA_model_files/figure-markdown_github/unnamed-chunk-13-1.png)

Moreover, the spike in stock price owing to invalid value might also
resulted in following invalid Stock Returns value, as Returns are
calculated from stock prices. Consequently, it is better to validate the
Return (“Change..”) variable:

``` r
summary(ACB_sorted$Change..[ACB_sorted$Date > "2015-01-01" & ACB_sorted$Date < "2016-01-01"])
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -0.736200 -0.015700  0.000000 -0.008061  0.018150  0.198800

``` r
print(ACB_sorted$Date[ACB_sorted$Change.. == min(ACB_sorted$Change..)])
```

    ## [1] "2015-02-22"

``` r
#The invalid negative return is at week 2015-02-22
```

According to the summary result, there is an abnormal negative weekly
return of -73.62%, which is clearly resulted from invalid data. Hence,
the row which contains the invalid return should also be removed.

``` r
ACB_sorted <- subset(ACB_sorted,Date != "2015-02-22")
```

### I. Introduction

The goal of this research is to examine the relationship between future
and past (lagged) stock returns of Asia Commercial Bank (ACB) and
Vietnam Export Import Commercial Joint Stock Bank (EIB). These banks are
both created in the 1990s, currently two of the ten largest banks in
Vietnam (Vietnam Plus 2022), serving commercial banking, institutional
banking, and consumer banking (Vietstock n.d). Then, it provides
trustworthy forecast model for substantial-monetary-benefits.

To investigate the relationship, we chose to use return data from
10-July-2011 to 31-July-2022 in Weekly intervals for the most sufficient
dataset, which later will be effective for our finding. Firstly, we plot
the ACF from the 2 banks’ returns for comparisons and discussion based
on the findings from the Descriptive Statistic. Finally, the predictive
power of past return (RETLAG) on future return (RET) will be tested
using the Arima regression model. Macroeconomic indicators reveal a
country’s overall economic strength or weakness (Alam 2014), therefore
they serve as leading indications of stock returns. Tu (2012) stated in
the banking sector, because stock prices rise when inflation rates do,
increasing dividend payments and providing shareholders with a higher
return, the inflation rate has a positive but insignificant impact on
the ROE (Return-On-Equity). The money supply (M2) has an insignificant
but negative influence on the stock return, as money supply growth will
result in higher interest rates, which will encourage more saving and
deposits (Garnia et al 2022). Additionally, the interest rate has a
negative and significant effect on the stock return since as interest
rates rise, people will prefer saving money than investing (Paul et al
2003). Changes in market return are statistically significant and have a
positive impact on stock returns. The exchange rate and GDP also have a
positive relationship with the stock return because capital inflows will
flood into stocks if the national currency increases, and investors will
wait for a suitable opportunity to receive higher market returns. Gold
price has a significant positive relationship with the stock return, as
the findings of Ismi (2018) show that when the gold price is high,
investors will also buy stocks so that their portfolio varies with the
purpose of decreasing risk because gold is recognized to have minimal
risk (Erna et al 2022).

Besides the effect of external factors, internal factors such as company
finance performance and dividend policy could reflect the stock price.
Particularly, ROA (Return-on-Assets) indicates how profitable a company
is in relation to all owed assets. The higher ROA ratio shows that firms
are increasingly effective in using assets to gain net income after tax
and this is a good signal about financial performance of firm (Subing et
al 2017). This attracts more potential investors due to a higher return,
leading to an increase in banking stock’s price. Regards dividend
policy, those companies that have a high dividend payment would be
valued more highly, in comparison to the companies with a low dividend
payment (Chu 2020). Specifically, an increase in dividend payment might
be good news for the financial situation of firm and brighter prospects.
Thus, the increase in expectation of investors boosts of stock market
price.

### II. Selection of Time Interval and Time Horizon

For our time-series regression, weekly return is our time interval
selection, according to following reasons. Firstly, owing to the
characteristic t+3 of Vietnam Stock Market, in which the successful
order execution at current (t+0) must wait for 3 days (t+3) to settle
money/shares. Thus, the stock current price (t+0) is partially reflected
by the market’s 3-day-before (t-3) demand; therefore, due to difference
between t of RET, and t-3 demand of daily stock price, the regression
result might not fully reflect the stock RET-RETLAG relationship if the
time interval selection is daily price/return. Secondly, monthly return
might be a good time interval, as it has less noise, and less
nonGaussian distributed than daily returns (Fama 1976). However, monthly
intervals will result in less observations, especially when cross
validation is applied, in which the regression model will be fit into a
train data with less observations to forecast and test for accuracy.
Thus, monthly return is not suitable for our case, leaving weekly return
as our only time interval selection, which does not have much noise like
daily, and has more observations for our regression. Regarding time
horizon, our selection is 11 years of weekly return, a total of 572
observations, consisting of 10-year training and 1-year testing data
sets. With 10-year train data, our forecast model can fully learn the
stock return distribution across the whole economy cycle, which usually
has 10-year periodicity (Harvey, Trimbur & Van Dijk 2007).

### III. Stock Charting and Descriptive Statistics

1.  **Stock Return Time Plot**

    ``` r
    ACB_sorted %>% ggplot() + aes(y=`Change..`, x= `Date`) +
      geom_line(color = "blue", size = 0.5) +
      ggtitle("ACB Weekly Return from 10/07/2011 to 31/07/2022") +
      theme_bw() +xlab("Time [1 Week]") + ylab("ACB Return (%)")
    ```

    ![](Forecasting_Stock_Price_using_ARIMA_model_files/figure-markdown_github/unnamed-chunk-16-1.png)

    ![](Forecasting_Stock_Price_using_ARIMA_model_files/figure-markdown_github/unnamed-chunk-17-1.png)

2.  **Descriptive Statistic of Stock Weekly Returns**

-   **Descriptive Statistic Result**

``` r
summary(ACB_sorted$Change..)
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -0.155000 -0.013000  0.000000  0.003644  0.021050  0.198800

``` r
summary(EIB_sorted$Change..)
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -0.178000 -0.016800  0.000000  0.002428  0.018000  0.224600

The descriptive statistics table are based on the weekly return of ACB
and EIB stock for the period of July 2011 to July 2022. Therefore, there
are 572 return observations in total of each stock as shown by the
count.  
  
Both ACB and EIB stock witness a zero median return, meaning that 50
percent of weeks with weekly returns result in negative, and vice versa.
The average return of ACB is given by the mean and is equal 0.36%.
Combining with its standard deviation of 0.037, it indicates the data
are clustered around the mean as it is close to zero. Meanwhile, the
mean of EIB weekly return is slightly higher than ACB, accounting for
0.24%. Since the standard deviation of EIB is a small number (0.042),
this value shows a low volatility in weekly return of EIB. Especially,
EIB weekly return has a higher standard deviation, in comparison to ACB
weekly return, thus, the return of EIB fluctuates more than the return
of ACB in weekly. Furthermore, the histogram of both ACB and EIB
represents a tight and steep distribution, which also confirms that much
of the data observed is dispersed around the mean.  
  
In term of skewness, both ACB and EIB return result in positive figure,
describing that the data set have a long tail to the right.
Specifically, the skewness of EIB weekly return is 2 times higher than
the skewness of ACB (0.80 compared to 0.44). Since the ACB skewness is
less than 0.5, the data are fairly symmetrical, according to Dugar
(2018). At the same time, the skewness of EIB is between 0.5 and 1,
meaning that the data are moderately skewed. As a result, the outliers
of EIB distribution curve are further outwards the right and closer to
the mean of the left.  
Overall, the performance of ACB and EIB returns in weekly time frame are
stable and less risky, making them a suitable investment for
conservative investors.

-   **Histogram of Stock Weekly Returns**

![](Forecasting_Stock_Price_using_ARIMA_model_files/figure-markdown_github/unnamed-chunk-19-1.png)![](Forecasting_Stock_Price_using_ARIMA_model_files/figure-markdown_github/unnamed-chunk-19-2.png)

-   **Box Plot of Stock Weekly Returns**

![](Forecasting_Stock_Price_using_ARIMA_model_files/figure-markdown_github/unnamed-chunk-20-1.png)![](Forecasting_Stock_Price_using_ARIMA_model_files/figure-markdown_github/unnamed-chunk-20-2.png)

### IV. ARIMA modelling

1.  **Model Selection**

This paper aims to apply the ARIMA model to examine the predictive power
of lagged return (RETLAG) on stock returns (RET). Particularly, ARIMA
model, which intensively developed by George Box and Gwilym Jerkins, is
a forecasting method which produces predictions by studying the patterns
of historical data (Arsyad 1995). The ARIMA stands for the AR, I and MA
which also the (p, q, d) order of the model, respectively. The model
assumes that time series is stationary (time-constant average variance),
however, most of the economics’ data are not stationary, but integrated
(I). Thus, non-stationary time series needs to go through differencing
process for d times before fitting ARIMA. Since ARIMA is a combination
of both MA, and AR, the method is effective in explaining stationary
random process, as MA and AR cannot merely explain the process
individually. Therefore, the ARIMA formula consists of the value of the
past, present, and past errors (Newbold 1983):

![](images/paste-6D7734E2.png)

1.  **Preparing Data**

-   **Outlier Treatment**:

Outliers should be paid attention to as they can have a disproportionate
effect on statistical results, leading to misleading interpretations.
However, outliers should be winsorized in this case instead of removing
them, because extreme stock return value is important for investors who
are prone to speculation and also for risk management (Welch 2017).

``` r
#Minimize the outlier effects by replacing outliers with according 5% and 95% values
ACB_RET_winsorized <- ACB_sorted$Change.. %>% Winsorize() %>% ts() %>% as_tsibble()
EIB_RET_winsorized <- EIB_sorted$Change.. %>% Winsorize() %>% ts() %>% as_tsibble()
```

-   **Transformation**:

Since the Stock Return’s variation is not proportional with time,
transformation is not required, as mathematical or box-cox
transformation would be a weak one with negligible effect. Additionally,
calendar, population and inflation adjustment are not needed, because
the data does not have calendar variation and are not affected by
population changes or the value of money.

-   **Decomposition**:

As the Stock Return time plots show no trend, seasonality or cyclic
behavior, decomposition is also not necessary.

-   **Train/Test split**:

For the predictive accuracy measuring purpose, ACB’s and EIB’s
time-series weekly stock returns are split into two periods: train and
test data. Specifically, train data has a total of 520 observations (10
year of observations), to be studied and used to forecast the next 52
weeks (1 year ahead).

``` r
ACB_RET_train <- ACB_RET_winsorized %>% filter(index <= 343)
ACB_RET_test <- ACB_RET_winsorized %>% filter(index > 343)

EIB_RET_train <- EIB_RET_winsorized %>% filter(index <= 344)
EIB_RET_test <- EIB_RET_winsorized %>% filter(index > 344) 
```

Here is the visualization for the Train/Test split:

![](Forecasting_Stock_Price_using_ARIMA_model_files/figure-markdown_github/unnamed-chunk-23-1.png)![](Forecasting_Stock_Price_using_ARIMA_model_files/figure-markdown_github/unnamed-chunk-23-2.png)

-   **Data Differencing:**

When comes to Time-series analysis and forecasting, it is crucial to
check whether the data is stationary or not, as to fit the ARIMA model,
the data should be stationary. Specifically, the ARIMA model will based
on the statistical properties of the time series then predict that the
data will be the same in the future as they have been in the past.
Therefore, it would be easier for the model to make forecast on
stationary time-series whose means and variance remain constant over
time. In this case, both the statistical test and graphical examination
will be taken by applying KPSS unitroot test and plotting ACF plots,
respectively.

``` r
#Unitroot KPSS test
ACB_RET_train %>% features(value, unitroot_kpss)
```

    ## # A tibble: 1 × 2
    ##   kpss_stat kpss_pvalue
    ##       <dbl>       <dbl>
    ## 1     0.561      0.0278

``` r
EIB_RET_train %>% features(value, unitroot_kpss)
```

    ## # A tibble: 1 × 2
    ##   kpss_stat kpss_pvalue
    ##       <dbl>       <dbl>
    ## 1     0.141         0.1

![](Forecasting_Stock_Price_using_ARIMA_model_files/figure-markdown_github/unnamed-chunk-25-1.png)![](Forecasting_Stock_Price_using_ARIMA_model_files/figure-markdown_github/unnamed-chunk-25-2.png)

  
With a KPSS p-value of merely less than the significant level (0.02 \<
0.05), and a significant spike at lag 60th, the ACB train is
non-stationary; therefore, a first order differencing is needed (d = 1).
On the other hand, EIB train is stationary with white-noise behavior of
ACF plot and 0.1 KPSS p-value, meaning no differencing needed (d =0).  

``` r
ACB_RET_train_diff <- diff(ACB_RET_train$value) %>% ts() %>% as_tsibble()
```

``` r
#Retake the KPSS unitroot test after taking a difference:
ACB_RET_train_diff %>% features(value, unitroot_kpss)
```

    ## # A tibble: 1 × 2
    ##   kpss_stat kpss_pvalue
    ##       <dbl>       <dbl>
    ## 1    0.0150         0.1

  
With a KPSS’s p-value of 0.1 after a first-order differencing, ACB
Return time-series now is stationary and ready to be fitted to ARIMA
model.

1.  **Fitting ARIMA Model**

-   **Identify potential (p, d, q) combinations**

![](Forecasting_Stock_Price_using_ARIMA_model_files/figure-markdown_github/unnamed-chunk-28-1.png)![](Forecasting_Stock_Price_using_ARIMA_model_files/figure-markdown_github/unnamed-chunk-28-2.png)

![](Forecasting_Stock_Price_using_ARIMA_model_files/figure-markdown_github/unnamed-chunk-29-1.png)![](Forecasting_Stock_Price_using_ARIMA_model_files/figure-markdown_github/unnamed-chunk-29-2.png)

  
By examining the ACF plots, the terms of AR and MA of ARIMA model can be
determined. Regarding ACB, the ACF’s significant lag 1 suggest a MA(1)
components, while multiple lags of the pACF suggest AR(1), AR(2), and
AR(3) components. By having a first order differencing, the I components
of ACB’s ARIMA is d = 1. Contrastively, in the case of EIB, both the
plots show no significant autocorrelation, suggesting only a constant
term for the model. Nevertheless, it is better to try out the simple
combinations from 1-3 for EIB’s ARIMA model.

``` r
ACB_ARIMA <- auto.arima(ACB_RET_train$value, stationary = FALSE, 
                        ic = c("aicc", "aic", "bic"),
                        max.p = 3, max.d = 1, max.q = 3, trace = TRUE)
```

    ## 
    ##  Fitting models using approximations to speed things up...
    ## 
    ##  ARIMA(2,1,2) with drift         : Inf
    ##  ARIMA(0,1,0) with drift         : -1273.419
    ##  ARIMA(1,1,0) with drift         : -1372.685
    ##  ARIMA(0,1,1) with drift         : -1495.127
    ##  ARIMA(0,1,0)                    : -1275.441
    ##  ARIMA(1,1,1) with drift         : Inf
    ##  ARIMA(0,1,2) with drift         : -1493.103
    ##  ARIMA(1,1,2) with drift         : Inf
    ##  ARIMA(0,1,1)                    : -1496.808
    ##  ARIMA(1,1,1)                    : -1495.342
    ##  ARIMA(0,1,2)                    : -1494.796
    ##  ARIMA(1,1,0)                    : -1374.714
    ##  ARIMA(1,1,2)                    : -1493.349
    ## 
    ##  Now re-fitting the best model(s) without approximations...
    ## 
    ##  ARIMA(0,1,1)                    : -1501.912
    ## 
    ##  Best model: ARIMA(0,1,1)

``` r
EIB_ARIMA <- auto.arima(EIB_RET_train$value, stationary = TRUE, 
                        ic = c("aicc", "aic", "bic"),
                        max.p = 3, max.d = 0, max.q = 3,
                                  trace = TRUE)
```

    ## 
    ##  Fitting models using approximations to speed things up...
    ## 
    ##  ARIMA(2,0,2) with non-zero mean : -1408.974
    ##  ARIMA(0,0,0) with non-zero mean : -1413.973
    ##  ARIMA(1,0,0) with non-zero mean : -1414.988
    ##  ARIMA(0,0,1) with non-zero mean : -1416.176
    ##  ARIMA(0,0,0) with zero mean     : -1415.266
    ##  ARIMA(1,0,1) with non-zero mean : -1413.323
    ##  ARIMA(0,0,2) with non-zero mean : -1414.261
    ##  ARIMA(1,0,2) with non-zero mean : Inf
    ##  ARIMA(0,0,1) with zero mean     : -1417.254
    ##  ARIMA(1,0,1) with zero mean     : -1414.39
    ##  ARIMA(0,0,2) with zero mean     : -1415.313
    ##  ARIMA(1,0,0) with zero mean     : -1416.12
    ##  ARIMA(1,0,2) with zero mean     : Inf
    ## 
    ##  Now re-fitting the best model(s) without approximations...
    ## 
    ##  ARIMA(0,0,1) with zero mean     : -1417.243
    ## 
    ##  Best model: ARIMA(0,0,1) with zero mean

``` r
checkresiduals(ACB_ARIMA)
```

![](Forecasting_Stock_Price_using_ARIMA_model_files/figure-markdown_github/unnamed-chunk-31-1.png)

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from ARIMA(0,1,1)
    ## Q* = 14.871, df = 9, p-value = 0.09454
    ## 
    ## Model df: 1.   Total lags used: 10

``` r
checkresiduals(EIB_ARIMA)
```

![](Forecasting_Stock_Price_using_ARIMA_model_files/figure-markdown_github/unnamed-chunk-31-2.png)

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from ARIMA(0,0,1) with zero mean
    ## Q* = 5.4977, df = 9, p-value = 0.7889
    ## 
    ## Model df: 1.   Total lags used: 10

``` r
ACB_ARIMA_011 <- ACB_RET_train %>% model(ARIMA(value ~ pdq(0,1,1)))
report(ACB_ARIMA_011)
```

    ## Series: value 
    ## Model: ARIMA(0,1,1) 
    ## 
    ## Coefficients:
    ##           ma1
    ##       -0.9731
    ## s.e.   0.0151
    ## 
    ## sigma^2 estimated as 0.0007124:  log likelihood=752.97
    ## AIC=-1501.95   AICc=-1501.91   BIC=-1494.28

``` r
ACB_fc <- forecast(ACB_ARIMA_011, h = 229)
```

1.  **Forecast**

``` r
ACB_fc %>% autoplot(ACB_RET_train) + 
  ggtitle("ARIMA(0,1,1) 52-Week Forecast of ACB Weekly Return") + xlab("Time[1W]") + ylab("Weekly Return(%)") + 
  theme_bw() + autolayer(ACB_RET_test,.vars = value, color = "violetred", alpha = 0.5)
```

![](Forecasting_Stock_Price_using_ARIMA_model_files/figure-markdown_github/unnamed-chunk-33-1.png)

``` r
EIB_ARIMA_001 <- ACB_RET_train %>% model(ARIMA(value ~ pdq(0,0,1)))
report(EIB_ARIMA_001)
```

    ## Series: value 
    ## Model: ARIMA(0,0,1) w/ mean 
    ## 
    ## Coefficients:
    ##          ma1  constant
    ##       0.0352    0.0031
    ## s.e.  0.0518    0.0015
    ## 
    ## sigma^2 estimated as 0.0007161:  log likelihood=756.26
    ## AIC=-1506.51   AICc=-1506.44   BIC=-1495

``` r
EIB_fc <- forecast(EIB_ARIMA_001, h = 229)
```

``` r
EIB_fc %>% autoplot(EIB_RET_train) + 
  ggtitle("ARIMA(0,0,1) with mean's 52-Week Forecast of EIB Weekly Return") + xlab("Time[1W]") + ylab("Weekly Return(%)") + 
  theme_bw() + autolayer(EIB_RET_test,.vars = value, color = "forestgreen", alpha = 0.7)
```

![](Forecasting_Stock_Price_using_ARIMA_model_files/figure-markdown_github/unnamed-chunk-35-1.png)

  
5. **Measure the model accuracy:**

-   **Cross validation**

``` r
accuracy(ACB_fc,ACB_RET_test)
```

    ## # A tibble: 1 × 10
    ##   .model             .type       ME   RMSE    MAE   MPE  MAPE  MASE RMSSE   ACF1
    ##   <chr>              <chr>    <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
    ## 1 ARIMA(value ~ pdq… Test  -0.00956 0.0344 0.0281  -Inf   Inf   NaN   NaN 0.0444

``` r
accuracy(EIB_fc,EIB_RET_test)
```

    ## # A tibble: 1 × 10
    ##   .model            .type       ME   RMSE    MAE   MPE  MAPE  MASE RMSSE    ACF1
    ##   <chr>             <chr>    <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
    ## 1 ARIMA(value ~ pd… Test  -8.30e-4 0.0344 0.0260  -Inf   Inf   NaN   NaN -0.0229

Root Mean Square Error (RMSE) measures the spread between the forecast
value and the actual (observed) values. In another terms, RMSE informs
accuracy level of the forecast model: the lower the RMSE, the better a
model can “fit” the dataset (Barnston 1992). In this case, the RMSE of
ACB and EIB ARIMA models are 3.0542 and 4.1899, respectively.
Additionally, in both models, the test accuracy is higher than train
accuracy, indicating potential overfitting in models (Ying 2019).
Nevertheless, as different ARIMA models applied to different datasets,
it is irrelevant to compare RMSE between the two-stock return forecast,
instead, RMSE should be used in comparing predictive power of different
forecast model that fitted to the same dataset. - **Multiple model
comparison** It is noticeable ARIMA models almost outperform other
models in terms of predictive accuracy. ACB and EIB ARIMA models are
still less accurate than the TSLM method, and the Mean method,
respectively (Table above). Nevertheless, ARIMA models have taken data
stationarity into account, whereas the Mean and TSLM methods do not.

### **V. Conclusion**

To examine the relationship between future and past stock returns of ACB
and EIB, the dataset of those weekly returns from July 2011 to July 2022
with 572 observations of each bank was collected and examined.
Graphically speaking, the two weekly return plots show no trend,
seasonality, or cyclic behavior; yet random fluctuations and outliers
require winsorization, which means limiting extreme values to lessen the
impact of erroneous outliers on the statistical data. Moreover, they
look like white noise series, indicating all variables have the same
variance and each value is uncorrelated with others. Statically
speaking, EIB’s stock has a higher average return and larger volatility
compared to ACB’s. Besides, since the distribution of both ACB and EIB
returns are tight and steep, the data observed tend to be close and
clustered around the mean. Furthermore, the skewness statistics indicate
that the dataset of ACB is fairly symmetrical whereas the dataset of EIB
is moderately skewed, thus, the distribution of EIB returns skewed
towards the right at the lower ends of values. The regression result
shows that both ACB and EIB have a negative RET-RETLAG relationship.
Specifically, while the return of ACB and EIB at t is correlated to
lagged error of earlier one day return (t-1). Furthermore, almost all
actual observation of test data is covered by 90-85% of both ARIMA
results; and thereby, the predictive power of RET-RETLAG exists in ARIMA
model with more accuracy than others. Regarding recommendations, there
are two objectives: (i) For research-focused stakeholders: Regarding the
predictive power, the RET-RETLAG relationship is established, based on
the lagged error of one-day-return before. However, the stock market is
distributed by many social and economic factors. Hence, for forecasting
purposes, the internal and external factors (mentioned above) should be
explored in-depth to build up a more accurate model. (ii) For
investing-focused stakeholders: As aforementioned, standard deviation
from descriptive statistics of EIB is higher than that of ACB,
indicating the price of EIB is more fluctuated than ACB’s price.
Besides, RET-RETLAG relationship of both stock return exists in terms of
lagged error of RETLAG. Non-technical stakeholders should pay attention
on the forecasting result of the ARIMA model for investing
decision-making. Also, the consideration of other factors affecting the
stock market is crucial for investing-decision, especially macro-
factors that strongly affect the return as outliers. Overall, the result
of this paper suggests EIB is potential for traders to make profit in
short-term due to its significant volatility, compared to ACB;
meanwhile, long-term investors should hold and buy more ACB shares
instead of EIB because ACB is more stable than EIB. For limitations,
according to Petrică, Stancu & Tindeche (2016), ARIMA model poorly
handle fat-tails time series, and volatility clustering. Particularly,
fat tails are outliers (significant losses or gains) that are coming at
a higher likelihood than Gaussian distributions, which usually appear in
stock return time series. During our ARIMA modeling, outlier effect has
been minimized by winsorization; however, by replacing outliers with 5%
and 95% value, our ARIMA model was unable to produce large gains/losses
to signal/warn the stakeholders (investors/risk mangers) in the future.
Regarding volatility clustering, stock market is volatile by nature as
its volatility is impacted by multiple factors such as changes in
exchange and interest rates. Thus, the constant variance assumption of
ARIMA has limited itself in forecasting changes in stock return
volatility, especially when the COVID-19 Quantitative Easing monetary
policy of central banks has significantly increased the volatility of
global stock market. Thus, for accuracy improvement, our solution is to
apply the GARCH model simultaneously with ARIMA, as the model is proved
to have better fit in modelling the return volatility (Begu, Sparatu &
Martin 2012).
