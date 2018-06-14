###### Insert other files ######################################################
source('Dissertation/Prep/Setup.R')

##### Creat ARMA Model and get parameters ######################################
# Create the ARMA Model
arima.model <- auto.arima(is)
arima.model


ar.comp <- arimaorder(object = arima.model)[1]
ma.comp <- arimaorder(object = arima.model)[3]

##### Fit the model ############################################################
fit = arima(is, order = c(ar.comp,0,ma.comp), include.mean = FALSE)
summary(fit)

##### Forecast the model #######################################################
arima.forecast <- forecast(object = fit, h = 30)

# extract first 30 observations of oos
obs <- fortify(oos[1:30,1])
rowind <- rownames(data.frame(arima.forecast))

actual$Index <- rowind
colnames(obs) <- c('Index','OOS')

##### Plot the estimated data ##################################################
autoplot(arima.forecast) + 
  scale_x_continuous(limits = c(500000,NA)) +
  theme_minimal()

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

##### Analyse residuals ########################################################
res.arma <- arima.forecast$residuals
sq.res.arma <- res.arma^2
autoplot(sq.res.arma) + 
  labs(title = 'Squared residuals', ylab = 'Squared residuals') +
  theme_minimal()

# Make acf of squared residuals
autoplot(forecast::Acf(sq.res.arma)) +
  theme_minimal()

# Make pacf of squared residuals
autoplot(forecast::Pacf(sq.res.arma)) +
  theme_bw()

##### Try with the rugarch gackage #############################################
model <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(0, 1)),
  mean.model = list(armaOrder = c(ar.comp, ma.comp), include.mean = FALSE),
  distribution.model = "norm"
)

modelfit <- ugarchfit(spec = model, data = ret, solver = 'hybrid')

coef(modelfit)
