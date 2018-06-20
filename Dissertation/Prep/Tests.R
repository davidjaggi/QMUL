##### Plot the series ##########################################################
# ARCH effects can be seen in squared residuals of the mean equation.

# log-returns
ggplot(fortify(ret)) +
  geom_line(aes(x = Index, y = ret)) +
  labs(title = 'Log-returns', x = 'Time', y = 'log-returns') +
  theme_bw()

# absolute log-returns
ggplot(fortify(ret)) +
  geom_line(aes(x = Index, y = abs(ret))) +
  labs(title = 'Log-returns', x = 'Time', y = 'absolute log-returns') +
  theme_bw()

# squared log-returns
ggplot(fortify(ret)) +
  geom_line(aes(x = Index, y = ret^2)) +
  labs(title = 'Log-returns', x = 'Time', y = 'squared log-returns') +
  theme_bw()

##### Test the series for its properties #######################################
# This file contains test which help testing the series
# Test for stationarity
# H0: unit root in the time series
# H1: stationarity or trend stationarity
adf.test(ret)

# Sharpiro Wilks test tests for normality of the data
# We can only perform the test with 5000 data points so we crop the data
# H0: The return series is normal distributed
# H1: The series is not normally distributed
shapiro_sample <- c(coredata(ret[(length(ret)-4999):length(ret)]))
shapiro.test(x = shapiro_sample)

q <- shapiro.test(x = shapiro_sample)
sinker(q, 'SP500ShapiroWilks')
# Delete the series again
rm(shapiro_sample)
rm(q)

# Make a qq plot to see the properties
# ggplot(data = fortify(ret), aes(x = Index, y = ret)) + geom_line()
params <- as.list(MASS::fitdistr(fortify(ret)$ret, "normal")$estimate)
dpar <- list('mean' = params[[1]], 'sd' = params[[2]])
ggplot(data = fortify(ret), aes(sample = ret)) + 
  stat_qq() +
  # stat_qq_line(distribution = 'norm', dparams = dpar) +
  theme_minimal() +
  ggtitle('QQ - Plot of the S&P 500 log-returns')
rm(params)
rm(dpar)


##### decompose the series #####################################################
is.decomp <- decompose(x = ts(is, frequency = 5), type = 'multiplicative')
plot(is.decomp)

##### Test series for autocorrelation ##########################################
# Return ACF
q <-autoplot(forecast::Acf(ret)) +
  labs(title = 'ACF: S&P 500 log-returns', x = 'Lag', y = 'ACF') +
  theme_bw()
q
# printer(plot = q, path = 'SP500ACF')

autoplot(forecast::Acf(abs(ret))) +
  labs(title = 'ACF: S&P 500 absolute log-returns', x = 'Lag', y = 'ACF') +
  theme_bw()

q <- autoplot(forecast::Acf(ret^2)) +
  labs(title = 'ACF: S&P 500 squared log-returns', x = 'Lag', y = 'ACF') +
  theme_bw()
q
# printer(q, 'SP500ACF2')

# Return PAcf
# Take care of x axis
autoplot(forecast::Pacf(ret, lag.max = 32)) +
  labs(title = 'PACF: S&P 500 log-returns', x = 'Lag', y = 'PACF') +
  theme_bw()

autoplot(forecast::Pacf(abs(ret), lag.max = 32)) +
  labs(title = 'PACF: S&P 500 absolute log-returns', x = 'Lag', y = 'PACF') +
  theme_bw()

# Return PAcf
# Take care of x axis
autoplot(forecast::Pacf(ret^2, lag.max = 32)) +
  labs(title = 'PACF: S&P 500 squared log-returns', x = 'Lag', y = 'PACF') +
  theme_bw()

##### Run an Ljung-Box test ####################################################
# Test for ARCH effects using the Ljung-Box test
# H0: The data are independently distributed (i.e. the correlations in the 
# population from which the sample is taken are 0, so that any observed 
# correlations in the data result from randomness of the sampling process).
# H1: The data are not independently distributed; they exhibit serial correlation.
q <- Box.test(coredata(ret), type = 'Ljung-Box', lag = 5)
# Reject the null hypothesis of no ARCH effects - need to control
# Save the outcome
# sinker(output = q, name = 'SPret_LjungBox')

##### Run an ADF test ##########################################################
q <- adf.test(coredata(ret), nlag = 5)
# sinker(q, name = 'SPret_adf')

##### Run an ADF test ##########################################################
q <- kpss.test(coredata(ret))
# sinker(q, name = 'SPret_kpss')
