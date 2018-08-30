subfolder <- 'Tests'

##### Plot the series ##########################################################
# ARCH effects can be seen in squared residuals of the mean equation.

# log-returns
test1 <- ggplot(fortify(ret)) +
  geom_line(aes(x = Index, y = ret)) +
  labs(title = paste0(ser_name, ': Returns'), x = 'Time', y = 'Log-Return') +
  theme_bw()
printer(test1, folder, subfolder, paste0(name,'_ret_plot'))

# absolute log-returns
test2 <- ggplot(fortify(ret)) +
  geom_line(aes(x = Index, y = abs(ret))) +
  labs(title = paste0(ser_name,': Absolute returns'), x = 'Time', y = 'Absolute Log-Returns') +
  theme_bw()
printer(test2, folder, subfolder, paste0(name,'_ret_abs_plot'))

# squared log-returns
test3 <- ggplot(fortify(ret)) +
  geom_line(aes(x = Index, y = ret^2)) +
  labs(title = paste0(ser_name,': Squared returns'), x = 'Time', y = 'Squared Log-Returns') +
  theme_bw()
printer(test3, folder, subfolder, paste0(name,'_ret_2_plot'))
##### Make a histogram of the returns ##########################################
test4 <- gghistogram(fortify(ret)$ret) +
  labs(title = paste0(ser_name,': Histogram of the returns'), x = 'Log-Return', y = 'Count') +
  theme_bw()
printer(test4,folder, subfolder, paste0(name,'_ret_hist'))

##### Test the series for its properties #######################################
# Make a qq plot to see the properties

y <- quantile(fortify(ret)$ret, c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
int <- y[1L] - slope * x[1L]

test5 <- ggplot(data = fortify(ret), aes(sample = ret)) + 
  stat_qq() +
  geom_abline(slope = slope, intercept = int) +
  theme_bw() +
  labs(title = paste0(ser_name,': QQ - plot of the returns'), x = 'Theoretical',
       y = 'Sample')
printer(test5, folder, subfolder, paste0(name, '_ret_qq'))
rm(y,x,slope,int)

##### Test series for autocorrelation  ACF #####################################
# Return acf
test6 <- ggAcf(ret) +
  labs(title = paste0(ser_name,': ACF of the returns'), x = 'Lag', y = 'ACF') +
  theme_bw()
printer(test6, folder, subfolder, paste0(name,'_acf'))

# Absolute return acf
test7 <- ggAcf(abs(ret)) +
  labs(title = paste0(ser_name,': ACF of the absolute returns'), x = 'Lag', y = 'ACF') +
  theme_bw()
printer(test7, folder, subfolder, paste0(name,'_acf_abs'))

# Squared return acf  
test8 <- ggAcf(ret^2) +
  labs(title = paste0(ser_name,': ACF of the squared returns'), x = 'Lag', y = 'ACF') +
  theme_bw()
printer(test8, folder, subfolder, paste0(name,'_acf_2'))

##### Test series for autocorrelation  PACF ####################################
# Return PAcf
# Take care of x axis
test9 <- ggPacf(ret) +
  labs(title = paste0(ser_name,': PACF of the returns'), x = 'Lag', y = 'PACF') +
  theme_bw()
printer(test9, folder, subfolder, paste0(name,'_pacf'))

# Absolute return pacf
test10 <- ggPacf(abs(ret)) +
  labs(title = paste0(ser_name, ': PACF of the absolute returns'), x = 'Lag', y = 'PACF') +
  theme_bw()
printer(test10, folder, subfolder, paste0(name,'_pacf_abs'))

# Squared return pacf
# Take care of x axis
test11 <- ggPacf(ret^2) +
  labs(title = paste0(ser_name,': PACF of the squared returns'), x = 'Lag', y = 'PACF') +
  theme_bw()
printer(test11, folder, subfolder, paste0(name,'_pacf_2'))

##### Run an Ljung-Box test ####################################################
# Test for ARCH effects using the Ljung-Box test
# H0: The data are independently distributed (i.e. the correlations in the 
# population from which the sample is taken are 0, so that any observed 
# correlations in the data result from randomness of the sampling process).
# H1: The data are not independently distributed; they exhibit serial correlation.
# Box.test(coredata(ret^2), type = 'Ljung-Box', lag = 12)
# Reject the null hypothesis of no ARCH effects - need to control
# Save the outcome
# Make Ljung Box of 10 Lags because of 2*Seasonality
sinker(output = Box.test(coredata(ret), type = 'Ljung-Box', lag = 10), folder, subfolder, name = paste0(name,'_ret_ljungbox'))
sinker(output = Box.test(coredata(ret^2), type = 'Ljung-Box', lag = 10), folder, subfolder, name = paste0(name,'_ret_ljungbox_2'))
##### Run an ADF test ##########################################################
# This file contains test which help testing the series
# Test for stationarity
# H0: unit root in the time series
# H1: stationarity or trend stationarity
# aTSA::adf.test(coredata(ret), nlag = 5)
sinker(aTSA::adf.test(coredata(ret), nlag = 5), folder, subfolder, name = paste0(name,'_ret_adf'))

##### Run an KPSS test #########################################################
# aTSA::kpss.test(coredata(ret))
sinker(aTSA::kpss.test(coredata(ret)), folder, subfolder, name = paste0(name,'_ret_kpss'))

##### Rund shapiro wilks test ##################################################
# Sharpiro Wilks test tests for normality of the data
# We can only perform the test with 5000 data points so we crop the data
# H0: The return series is normal distributed
# H1: The series is not normally distributed
shapiro_sample <- c(coredata(ret[(length(ret)-4999):length(ret)]))
shapiro.test(x = shapiro_sample)

sinker(shapiro.test(x = shapiro_sample), folder, subfolder, paste0(name,'_ret_shapiro'))

# Delete the series again
rm(shapiro_sample)
##### stationarity test ########################################################
# aTSA::stationary.test(ret)
sinker(aTSA::stationary.test(ret), folder, subfolder, paste0(name,'_ret_stat'))

##### trend test ###############################################################
# aTSA::trend.test(ret)
sinker(aTSA::trend.test(coredata(ret)), folder, subfolder, paste0(name,'_ret_trend'))

##### Phillips perron test #####################################################
# aTSA::pp.test(ret)
sinker(aTSA::pp.test(ret), folder, subfolder, paste0(name,'_ret_pp'))

##### Jarque Bera test #########################################################
# jarque.bera.test(ret)
sinker(jarque.bera.test(ret), folder, subfolder, paste0(name,'_ret_jarque'))

##### bds test #################################################################
# bds.test(ret)
sinker(bds.test(ret), folder, subfolder, paste0(name,'_ret_bds'))

##### qqplot for returns #######################################################
# ggplot(data = fortify(ret), aes(sample = ret)) +
#   stat_qq() +
#   qqplotr::stat_qq_line()
# 
# ggplot(data = fortify(ret), aes(sample = ret)) +
#   stat_qq()

rm(test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, 
   subfolder)
