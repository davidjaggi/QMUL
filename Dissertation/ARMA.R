###### Insert other files ######################################################
source('Dissertation/Setup.R')

##### Creat ARMA Model and get parameters ######################################
# Create the ARMA Model
arima.model <- auto.arima(is)
arima.model


ar.comp <- arimaorder(object = arima.model)[1]
ma.comp <- arimaorder(object = arima.model)[3]

##### Fit the model ############################################################
fit = arima(is, order = c(2,0,0), include.mean = FALSE)
summary(fit)

##### Forecast the model #######################################################
arima.forecast <- forecast(object = fit, h = 30)

# extract first 30 observations of oos
actual <- oos[1:30,1]


##### Plot the estimated data ##################################################
autoplot(arima.forecast)

ggplot(data = fortify(actual), aes(x = Index)) +
  geom_line(aes(y = actual), color = 'red') +
  labs(title = 'ARMA Forecast 30-steps ahead', xlab = 'Time',
       ylab = 'Log-return') +
  theme_minimal()

##### Plot with the fitted values ##############################################
fit <- data.frame(data=as.matrix(fitted(arima.forecast)), 
                  date=time(fitted(arima.forecast)))
autoplot(arima.forecast) + geom_line(data = fit,aes(date,data), col = "red")


forc <- tail(fortify(arima.forecast),60)
# extract first 30 observations of oos
actual <- fortify(oos[1:30,1])
actual$Index <- tail(forc$Index, 30)
colnames(actual) <- c('Index','OOS')

ggplot(data = forc, aes(x = Index)) +
  geom_line(aes(y = 'Point Forecast')) +
  geom_line(data = actual, aes(x = Index, y = OOS))
