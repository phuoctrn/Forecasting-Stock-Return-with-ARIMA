
############### Input data into R ####################
ACB_weekly_price_data <-read_excel("/Users/phuoctran/Desktop/QA ASM2/ACB & EIB Weekly Price.xlsx", sheet = 1)
EIB_weekly_price_data <-read_excel("/Users/phuoctran/Desktop/QA ASM2/ACB & EIB Weekly Price.xlsx", sheet = 2)

ACB_weekly_return_cal <- diff(ACB_weekly_price_data$Price)/lag(ACB_weekly_price_data$Price) %>% na.omit() %>% ts() %>% as_tsibble()
ACB_RET_multiply <- ACB_weekly_return_cal$value*100
ACB_RET <-ACB_RET_multiply %>% ts() %>% as_tsibble()
ACB_Return_Date <- ACB_weekly_price_data[2:nrow(ACB_weekly_price_data), ]
ACB_RET_Timeplot <- data.frame("index" = ACB_Return_Date$Date, "value" = ACB_RET$value)


EIB_weekly_return_cal <- diff(EIB_weekly_price_data$Price)/lag(EIB_weekly_price_data$Price) %>% na.omit() %>% ts() %>% as_tsibble()
EIB_RET_multiply <- EIB_weekly_return_cal$value*100
EIB_RET <-EIB_RET_multiply %>% ts() %>% as_tsibble()
EIB_Return_Date <- EIB_weekly_price_data[2:nrow(EIB_weekly_price_data), ]
EIB_RET_Timeplot <- data.frame("index" = EIB_Return_Date$Date, "value" = EIB_RET$value)
##################### Timeplot ##########################

#ACB Price Time plot
ACB_weekly_price_data %>% ggplot() + aes(x = `Date`, y =`Price`) + 
  geom_line(color = "blue", size = 0.5) +
  ggtitle("ACB Weekly Price from 03/07/2011 to 31/07/2022") +
  theme_bw() + xlab("Time [1 Week]") + ylab("ACB Price (VND)")

#ACB Return Time plot
ACB_RET_Timeplot %>% ggplot() + aes(y=`value`, x= `index`) +
  geom_line(color = "slateblue3", size = 0.5) +
  ggtitle("ACB Weekly Return from 10/07/2011 to 31/07/2022") +
  theme_bw() +xlab("Time [1 Week]") + ylab("ACB Return (%)")

#EIB Price Time plot
EIB_weekly_price_data %>% ggplot() + aes(x = `Date`, y = `Price`) +
  geom_line(color = "palegreen3", size = 0.5) + 
  ggtitle("EIB Weekly Price from 03/07/2011 to 31/07/2022") +
  theme_bw() + xlab("Time [1 Week]") + ylab("EIB Price (VND)")

#EIB Return Timeplot
EIB_RET_Timeplot %>% ggplot() + aes(x = `index`, y =`value`) +
  geom_line(color = "green4", size = 0.4) + 
  ggtitle("EIB Weekly Return from 10/07/2011 to 31/07/2022 ")+
  theme_bw() + xlab("Time[1 Week]") + ylab("EIB Return(%)")

#ACF Plot for ACB & EIB (Before stationarization for ARIMA model)

ggAcf(ACB_RET, lag.max = 100) + ggtitle("ACF Plot for ACB Weekly Return") + xlab("Lag[1W]")
ggAcf(EIB_RET, lag.max =100) + ggtitle("ACF Plot for EIB Weekly Return") + xlab("Lag[1W]")


# Descriptive Statistic of Stock Returns

library(pastecs)

stat.desc(ACB_RET$value) %>% view() #Statistical Result of ACB Return
stat.desc(EIB_RET$value) %>% view() #Statistical Result of EIB Return

#Histogram

#Histogram of ACB Returns
ACB_RET %>% ggplot()+aes(x=`value`) +
  geom_histogram(bins = 11, color = "black", fill = "blue", alpha = 0.7) + theme_bw() + ylab("Frequency") +
  ggtitle("Histogram of ACB Weekly Return")

#Histogram of EIB Returns
EIB_RET %>% ggplot()+aes(x=`value`) +
  geom_histogram(bins = 11, color = "black", fill = "palegreen3", alpha = 0.7) + theme_bw() + ylab("Frequency") +
  ggtitle("Histogram of EIB Weekly Return")

#Box plot

library(RColorBrewer)

#ACB & EIB Return box plot

Stock_return <- data.frame("ACB.RETURN" = ACB_RET$value,"EIB.RETURN"=EIB_RET$value) %>% ts() %>% as_tsibble(pivot_longer = TRUE)

Stock_return %>% ggplot() + aes(x=`key`, y =`value`) +
  geom_boxplot(fill = "white", outlier.colour = "orangered", outlier.shape = 21, outlier.fill = "red", alpha = 0.5) +
  stat_boxplot(geom = 'errorbar', width = 0.2) +
  xlab("Stock") + ylab("Weekly Return(%)") +
  ggtitle("Boxploxs of ACB & EIB Weekly Returns") + 
  coord_flip() + theme_bw() + scale_fill_brewer(palette = "Dark2") + 
  stat_summary(fun = mean, geom = "errorbar",aes(ymax = ..y.., ymin = ..y..),width = 0.75, linetype = "dashed", color = "royalblue1")

#Mirror Density Chart of ACB & EIB Weekly Return

library(hrbrthemes)

Stock_return_density <- data.frame("ACB.RETURN" = ACB_RET$value,"EIB.RETURN"=EIB_RET$value)
ggplot(Stock_return_density) + aes(x= `key`) +
  #Top
  geom_density(aes(x = `ACB.RETURN`, y = ..density..), fill ="blue", alpha =0.4) +
  geom_label(aes(x = 10, y = 0.1, label = "ACB Weekly Return"), color = "blue", size = 3) +
  #Bottom
  geom_density(aes(x= `EIB.RETURN`, y = -..density..), fill = "palegreen3", alpha = 0.4) +
  geom_label(aes(x = 10, y = -0.1, label ="EIB Weekly Return"), color = "palegreen4", size = 3) +
  xlab("% Return") + theme_ipsum() + ggtitle("Mirror Density Chart of ACB & EIB Weekly Returns")


####################################  ARIMA MODELLING PART ########################################
# Winsorize Outlier to prepare data for ARIMA forecast

library(DescTools)

ACB_RET_winsorized <- ACB_RET$value %>% Winsorize() %>% ts() %>% as_tsibble()
EIB_RET_winsorized <- EIB_RET$value %>% Winsorize() %>% ts() %>% as_tsibble()

########################## ACB ARIMA ############################

  #Forming train dataset for forecast (Weekly return from 10/07/2011 - 25/07/2011)

ACB_RET_train <- ACB_RET_winsorized %>% filter(index <= 520)

ACB_RET_test <- ACB_RET_winsorized %>% filter(index > 520)


#Identify trend & seasonality in train dataset 

ACB_RET_train %>% model(STL()) %>% components() %>% autoplot() #No seasonality in data

#ACB TRAIN & TEST PLOT
ggplot() + aes(x = ACB_RET_train$index, y = ACB_RET_train$value) + geom_line() + 
  geom_line(aes(x=ACB_RET_test$index, y = ACB_RET_test$value), color ="darkblue") +   ggtitle("Plot of ACB Weekly Return Train & Test data sets") + 
  xlab("Time[1W]") + ylab("ACB Weekly Return (%)") +
  theme_bw() + geom_label(aes(x = 555, y = -6, label = "ACB Test"), color = "darkblue", size = 3.5) + 
  geom_label(aes(x = 300, y = -6, label = "ACB Train"), color = "black", size = 3.5)
  
#ACF and pACF plot for ACB train dataset

ggAcf(ACB_RET_train$value, lag.max = 100) + ggtitle("ACF Plot of ACB Train Data Set (520 obs)")
ggPacf(ACB_RET_train$value, lag.max =100) + ggtitle("Partial ACF Plot of ACB Train Data Set (520 obs)")

# Stationary test
ACB_RET_train %>% features(value, unitroot_kpss) # kpss_pvalue 0.0433 < 0.05. Therefore, ARIMA's d value will be 1

# Fitting ARIMA Model into the ACB train dataset

ACB_ARIMA_MODEL_TYPE <-auto.arima(ACB_RET_train$value, stationary = FALSE, ic = c("aicc", "aic", "bic"), 
                                  trace = TRUE)
summary(ACB_ARIMA_MODEL_TYPE)
ARIMA_fit_ACB_RET_train <- ACB_RET_train%>% model(ARIMA(value))
report(ARIMA_fit_ACB_RET_train)

# ACB ARIMA residual normality test
checkresiduals(ACB_ARIMA_MODEL_TYPE)

#Fitting TSLM Model into ACB train dataset 
#TSLM equivalent to ARIMA w drift (1,0,0), as level of ACB train differencing =1 so d =1
TSLM_fit_ACB_RET_train <- ACB_RET_train %>% model(tslm = ARIMA(value~ 1+pdq(1,0,0)))
report(TSLM_fit_ACB_RET_train)

#ACB ARIMA Forecast

forecast(ARIMA_fit_ACB_RET_train, h =52) %>% autoplot(ACB_RET_train) + 
  ggtitle("ARIMA 52-Week Forecast of ACB Weekly Return") + xlab("Time[1W]") + ylab("Weekly Return(%)") + 
  theme_bw() + autolayer(ACB_RET_test,.vars = value, color = "violetred", alpha = 0.5)
#Measure ARIMA predictive power

accuracy(forecast(ARIMA_fit_ACB_RET_train, h =52), ACB_RET_test) %>% view()
accuracy(ARIMA_fit_ACB_RET_train) %>%view()
coeftest(ACB_ARIMA_MODEL_TYPE)

#Other Forecast Models for predictive power comparison

Comparison_fit_ACB_RET_train <- ACB_RET_train %>% 
  model(
    Naive = NAIVE(value),
    Drift = RW(value ~ drift()),
    Mean = MEAN(value),
    Arima = ARIMA(value~pdq(3,1,1)),
    TSLM = ARIMA(value ~ 1+pdq(1,0,0)))
report(Comparison_fit_ACB_RET_train)
forecast(Comparison_fit_ACB_RET_train, h =52) %>% autoplot(ACB_RET_train)
accuracy(forecast(Comparison_fit_ACB_RET_train, h =52), ACB_RET_test) %>% view()


########################   EIB ARIMA   #################################

# Unit Root Square test to indicate stationary
EIB_RET_winsorized%>% features(value, unitroot_kpss) # kpss p-value 0.1 => d value for ARIMA is 0 and no differencing is needed

#Forming train dataset for forecast (Weekly return from 10/07/2011 - 25/07/2011)

EIB_RET_train <- EIB_RET_winsorized %>% filter(index <= 520) 

EIB_RET_test <- EIB_RET_winsorized %>% filter(index > 520) 

EIB_RET_train %>% model(STL()) %>% components() %>% autoplot() #No seasonality in data

#Plot the EIB Train&Test

ggplot() + aes(x = EIB_RET_train$index, y = EIB_RET_train$value) + 
  geom_line() + geom_line(aes(x=EIB_RET_test$index, y = EIB_RET_test$value), color ="darkslateblue") +
  ggtitle("Plot of EIB Weekly Return Train & Test data sets") + xlab("Time[1W]") + ylab("EIB Weekly Return (%)") +
  theme_bw() + geom_label(aes(x = 555, y = -6.5, label = "EIB Test"), color = "darkslateblue", size = 3) + 
  geom_label(aes(x = 300, y = -6.5, label = "EIB Train"), color = "black", size = 3)

#ACF and pACF plot for EIB train dataset

ggAcf(EIB_RET_train$value, lag.max = 520) + ggtitle("ACF Plot of EIB Train Data (520 obs)")
ggPacf(EIB_RET_train$value, lag.max = 520) + ggtitle("Partial ACF Plot of EIB Train Data (520 obs)")

# Stationary Test
EIB_RET_train %>% features(value, unitroot_kpss) #KPSS

EIB_ARIMA_MODEL_TYPE <-auto.arima(EIB_RET_train$value, stationary = TRUE, ic = c("aicc", "aic", "bic"), 
                                  trace = TRUE)
# Fitting ARIMA Model to EIB train
ARIMA_fit_EIB_RET_train <- EIB_RET_train%>% model(ARIMA(value))
report(ARIMA_fit_EIB_RET_train)

#Significant test
coeftest(EIB_ARIMA_MODEL_TYPE)
#ARIMA EIB train predictive power measure
forecast(ARIMA_fit_EIB_RET_train, h =52) %>% autoplot(EIB_RET_train) + 
  ggtitle("ARIMA 52-Week Forecast of EIB Weekly Return") + xlab("Time[1W]") + ylab("Weekly Return(%)") + theme_bw() +
  autolayer(EIB_RET_test, color = "magenta3")
#EIB ARIMA predictive power measurement
accuracy(forecast(ARIMA_fit_EIB_RET_train, h =52),EIB_RET_test) %>% view()
accuracy(ARIMA_fit_EIB_RET_train) %>% view()

#EIB Residual Normality Test
checkresiduals(EIB_ARIMA_MODEL_TYPE)

#Other Forecast Models for predictive power comparison
Comparison_fit_EIB_RET_train <- EIB_RET_train %>% 
  model(
    Naive = NAIVE(value),
    Drift = RW(value ~ drift()),
    Mean = MEAN(value),
    Arima = ARIMA(value),
    TSLM = ARIMA(value ~ 1+ pdq(1,0,0)))
report(Comparison_fit_EIB_RET_train)
forecast(Comparison_fit_EIB_RET_train, h =52) %>% autoplot(EIB_RET_train)
accuracy(forecast(Comparison_fit_EIB_RET_train, h =52), EIB_RET_test) %>% view()



