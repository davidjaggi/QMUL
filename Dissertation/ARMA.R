source('Dissertation/Setup.R')


# Create the ARMA Model
arima.model <- auto.arima(is)
arima.model


ar.comp <- arimaorder(object = arima.model)[1]
ma.comp <- arimaorder(object = arima.model)[3]

fit = arima(is, order = c(2,0,0), include.mean = FALSE)
summary(fit)

arima.forecast <- forecast(object = fit, h = 30)

# extract first 30 observations
actual <- ret[1:30,1]


autoplot(arima.forecast) + theme_minimal()

ggplot(data = fortify(actual), aes(x = Index)) +
  geom_line(aes(y = actual), color = 'red') +
  labs(title = 'ARMA Forecast 30-steps ahead', xlab = 'Time',
       ylab = 'Log-return') +
  theme_minimal()


merge.xts(actual, arima.forecast)
length(actual)
length(arima.forecast)

dforc <- funggcast

p1a<-ggplot(data=pd,aes(x=date,y=observed)) 
p1a<-p1a+geom_line(col='red')
p1a<-p1a+geom_line(aes(y=fitted),col='blue')
p1a<-p1a+geom_line(aes(y=forecast))+geom_ribbon(aes(ymin=lo95,ymax=hi95),alpha=.25)
p1a<-p1a+scale_x_date(name='',breaks='1 year',minor_breaks='1 month',labels=date_format("%b-%y"),expand=c(0,0))
p1a<-p1a+scale_y_continuous(name='Units of Y')
p1a<-p1a+opts(axis.text.x=theme_text(size=10),title='Arima Fit to Simulated Data\n (black=forecast, blue=fitted, red=data, shadow=95% conf. interval)')
