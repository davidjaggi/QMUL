##### Plot the series ##########################################################
# ARCH effects can be seen in squared residuals of the mean equation.

# log-returns
q <- ggplot(fortify(ret)) +
  geom_line(aes(x = Index, y = ret)) +
  labs(title = 'S&P 500 Log-returns', x = 'Time', y = 'log-returns') +
  theme_bw()
# printer(q, paste0(name,'_ret_plot'))

# absolute log-returns
q <- ggplot(fortify(ret)) +
  geom_line(aes(x = Index, y = abs(ret))) +
  labs(title = 'S&P 500 Absolute Log-returns', x = 'Time', y = 'absolute log-returns') +
  theme_bw()
# printer(q, paste0(name,'_ret_abs_plot'))

# squared log-returns
q <- ggplot(fortify(ret)) +
  geom_line(aes(x = Index, y = ret^2)) +
  labs(title = 'S&P 500 Squared Log-returns', x = 'Time', y = 'squared log-returns') +
  theme_bw()
# printer(q, paste0(name,'_ret_2_plot'))
##### Make a histogram of the returns ##########################################
q <- gghistogram(fortify(ret)$ret) +
  labs(title = 'S&P 500 Histogram of log-returns', x = 'log-return', y = 'Count') +
  theme_bw()
# printer(q, paste0(name,'_ret_hist'))
q

##### Test the series for its properties #######################################
# Make a qq plot to see the properties

y <- quantile(fortify(ret)$ret, c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
int <- y[1L] - slope * x[1L]

q <- ggplot(data = fortify(ret), aes(sample = ret)) + 
  stat_qq() +
  geom_abline(slope = slope, intercept = int) +
  theme_bw() +
  ggtitle('QQ - Plot of the S&P 500 log-returns')
# printer(q, paste0(name, '_ret_qq'))
rm(y,x,slope,int)

##### Test series for autocorrelation  ACF #####################################
# Return acf
q <- ggAcf(ret) +
  labs(title = 'ACF: S&P 500 log-returns', x = 'Lag', y = 'ACF') +
  theme_bw()
# printer(plot = q, paste0(name,'_acf'))

# Absolute return acf
q <- ggAcf(abs(ret)) +
  labs(title = 'ACF: S&P 500 absolute log-returns', x = 'Lag', y = 'ACF') +
  theme_bw()
# printer(q, paste0(name,'_acf_abs'))

# Squared return acf  
q <- ggAcf(ret^2) +
  labs(title = 'ACF: S&P 500 squared log-returns', x = 'Lag', y = 'ACF') +
  theme_bw()
# printer(q, paste0(name,'_acf_2'))

##### Test series for autocorrelation  PACF ####################################
# Return PAcf
# Take care of x axis
q <- ggPacf(ret) +
  labs(title = 'PACF: S&P 500 log-returns', x = 'Lag', y = 'PACF') +
  theme_bw()
# printer(q, paste0(name,'_pacf'))

# Absolute return pacf
q <- ggPacf(abs(ret)) +
  labs(title = 'PACF: S&P 500 absolute log-returns', x = 'Lag', y = 'PACF') +
  theme_bw()
# printer(q, paste0(name,'_pacf_abs'))

# Squared return pacf
# Take care of x axis
q <- ggPacf(ret^2) +
  labs(title = 'PACF: S&P 500 squared log-returns', x = 'Lag', y = 'PACF') +
  theme_bw()
# printer(q, paste0(name,'_pacf_2'))

##### Run an Ljung-Box test ####################################################
# Test for ARCH effects using the Ljung-Box test
# H0: The data are independently distributed (i.e. the correlations in the 
# population from which the sample is taken are 0, so that any observed 
# correlations in the data result from randomness of the sampling process).
# H1: The data are not independently distributed; they exhibit serial correlation.
Box.test(coredata(ret^2), type = 'Ljung-Box', lag = 12)
# Reject the null hypothesis of no ARCH effects - need to control
# Save the outcome
# sinker(output = Box.test(coredata(ret^2), type = 'Ljung-Box', lag = 12), name = paste0(name,'_ret_ljungbox_2'))
##### Run an ADF test ##########################################################
# This file contains test which help testing the series
# Test for stationarity
# H0: unit root in the time series
# H1: stationarity or trend stationarity
aTSA::adf.test(coredata(ret), nlag = 5)
# sinker(aTSA::adf.test(coredata(ret), nlag = 5), name = paste0(name,'_ret_adf'))

##### Run an KPSS test #########################################################
aTSA::kpss.test(coredata(ret))
# sinker(aTSA::kpss.test(coredata(ret)), name = paste0(name,'_ret_kpss'))

##### Rund shapiro wilks test ##################################################
# Sharpiro Wilks test tests for normality of the data
# We can only perform the test with 5000 data points so we crop the data
# H0: The return series is normal distributed
# H1: The series is not normally distributed
shapiro_sample <- c(coredata(ret[(length(ret)-4999):length(ret)]))
shapiro.test(x = shapiro_sample)

# sinker(shapiro.test(x = shapiro_sample), paste0(name,'_ret_shapiro'))
closeAllConnections()

# Delete the series again
rm(shapiro_sample)
rm(q)

##### stationarity test ########################################################
aTSA::stationary.test(ret)
# sinker(aTSA::stationary.test(ret), paste0(name,'_ret_stat'))

##### trend test ###############################################################
aTSA::trend.test(ret)
# sinker(aTSA::trend.test(coredata(ret)), paste0(name,'_ret_trend'))

##### Phillips perron test #####################################################
aTSA::pp.test(ret)
# sinker(aTSA::pp.test(ret), paste0(name,'_ret_pp'))

##### Jarque Bera test #########################################################
jarque.bera.test(ret)
# sinker(jarque.bera.test(ret), paste0(name,'_ret_jarque'))

##### bds test #################################################################
bds.test(ret)
# sinker(bds.test(ret), paste0(name,'_ret_bds'))

##### qqplot for returns #######################################################
# ggplot(data = fortify(ret), aes(sample = ret)) +
#   stat_qq() +
#   qqplotr::stat_qq_line()
# 
# ggplot(data = fortify(ret), aes(sample = ret)) +
#   stat_qq()

