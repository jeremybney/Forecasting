# Test1

library('ggplot2')
library('forecast')
library('tseries')

FB_data = read.csv('FB.csv', header=TRUE, stringsAsFactors=FALSE)

FB_data$Date = as.Date(FB_data$dteday) #turn date column into a usable date format
count_ts = ts(FB_data[, c('cnt')]) #use the column that you want use in your time series (I changed the closing price column to cnt)
FB_data$clean_cnt = tsclean(count_ts)
FB_data$cnt_ma = ma(FB_data$clean_cnt, order=7) # using the clean count with no outliers, this creates a weekly moving average
FB_data$cnt_ma30 = ma(FB_data$clean_cnt, order=30) # using the clean count with no outliers, this creates a monthly moving average

count_ma = ts(na.omit(FB_data$cnt_ma), frequency=30) #set the frequency parameter to 30 observations per month. With stock data, can set this to 20
decomp = stl(count_ma, s.window="periodic") #adjusts for seasonality
deseasonal_cnt <- seasadj(decomp) #seasadj is a program in the forecast package
plot(decomp)

fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
seas_fcast <- forecast(fit_w_seasonality, h=30) #forecast 30days out
autoplot(seas_fcast) #using ggplot autoplot, show the 80/95 degree confidence levels
