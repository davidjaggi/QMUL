##### Test the series for its properties #######################################
# This file contains test which help testing the series
# Test for stationarity
# H0: unit root in the time series
# H1: stationarity or trend stationarity
adf.test(ret)

# Sharpiro Wilks test tests for normality of the data
shapiro_sample <- c(coredata(ret[(length(ret)-4999):length(ret)]))
shapiro.test(x = shapiro_sample)

q <- shapiro.test(x = shapiro_sample)
sinker(q, 'SP500ShapiroWilks')

# Make a qq plot to see the properties
# ggplot(data = fortify(ret), aes(x = Index, y = ret)) + geom_line()
params <- as.list(MASS::fitdistr(fortify(ret)$ret, "normal")$estimate)
ggplot(data = fortify(ret), aes(sample = ret)) + 
  stat_qq() +
  theme_minimal() +
  ggtitle('QQ - Plot of the S&P 500 log-returns')


##### Test series for autocorrelation ##########################################
# Return ACF
q <-autoplot(forecast::Acf(ret)) +
  ggtitle('ACF: Return') +
  xlab('Lag') +
  ylab('ACF') +
  theme_minimal()
printer(plot = q, path = 'SP500ACF')

autoplot(forecast::Acf(abs(ret))) +
  ggtitle('ACF: Absolute Return') +
  xlab('Lag') +
  ylab('ACF') +
  theme_minimal()

q <- autoplot(forecast::Acf(ret^2)) +
  ggtitle('ACF: Squared Return') +
  xlab('Lag') +
  ylab('ACF') +
  theme_minimal()
printer(q, 'SP500ACF2')

# Return PAcf
# Take care of x axis
autoplot(forecast::Pacf(ret, lag.max = 32)) +
  ggtitle('Partial autocorrelation function') +
  xlab('Lag') +
  ylab('ACF') +
  theme_minimal()

autoplot(forecast::Pacf(abs(ret), lag.max = 32)) +
  ggtitle('Partial autocorrelation function') +
  xlab('Lag') +
  ylab('ACF') +
  theme_minimal()

# Return PAcf
# Take care of x axis
autoplot(forecast::Pacf(ret^2, lag.max = 32)) +
  ggtitle('Partial autocorrelation function') +
  xlab('Lag') +
  ylab('ACF') +
  theme_minimal()

# Test for ARCH effects
Box.test(coredata(ret^2), type = 'Ljung-Box', lag = 12)
# Reject the null hypothesis of no ARCH effects - need to control
# for remaining conditional heteroscedasticity.

  