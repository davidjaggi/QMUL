##### Plot time series #########################################################
# Plot the data
# Troubleshoot why I need an autoplot.zoo

# Save as png
# png(filename="Dissertation/Figures/SP500.png",
#     height = 960, width = 1440, res = 300)
# autoplot.zoo(x) + 
#   ggtitle('SP500') + 
#   xlab('Time') + 
#   ylab('Price') + 
#   theme_light()
# dev.off()
# Make summary

##### Analyse the time series ##################################################
summary(x)
xtable(summary(x))
# Now check for NA's
sum(is.na(x)) # in this series we have 91 NA's

# Delete the NA's
# x <- na.omit(x)
# Fill the NA's with the previous values
x <- na.locf(x)
sum(is.na(x))

##### Analyse the series for autocorrelation ###################################
# check for heteroscedasticity
# H0: There is no autoregressive conditional heteroscedasticity
# H1: There is autoregressive heteroscedaticity
McLeod.Li.test(y = ret)

# Show a corrolelogram
# ACF
autoplot(forecast::Acf(x)) +
  ggtitle('Autocorrelation function') +
  xlab('Lag') +
  ylab('ACF') +
  theme_minimal()

autoplot(forecast::Acf(x^2)) +
  ggtitle('Autocorrelation function') +
  xlab('Lag') +
  ylab('ACF') +
  theme_minimal()

autoplot(forecast::Acf(abs(x))) +
  ggtitle('Autocorrelation function') +
  xlab('Lag') +
  ylab('ACF') +
  theme_minimal()

# PAcf
autoplot(stats::pacf(x)) +
  ggtitle('Partial autocorrelation function') +
  xlab('Lag') +
  ylab('ACF') +
  theme_minimal()


##### Analyse returns ##########################################################
summary(ret)
xtable(summary(ret))

median(ret)
kurtosis(ret)
skewness(ret)
sd(ret)

# Plot the returns
# png(filename="C:/Users/David Jaggi/Desktop/QMUL/Dissertation/Dissertation/Figures/SP500Return.png",
#     height = 960, width = 1440, res = 300)
# autoplot.zoo(ret) + 
#   ggtitle('Log-Returns of the SP500') + 
#   xlab('Time') + 
#   ylab('Return') + 
#   theme_minimal()
# dev.off()

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


# Plot histogram of returns
q <- ggplot(data=ret, aes(ret)) + geom_histogram(bins = 100) + 
  theme_minimal() + 
  ggtitle('Histogram of S&P 500 Returns') + 
  xlab('Returns') + 
  ylab('Number of observations') +
  geom_vline(xintercept = mean(ret), col = 'red') +
  geom_vline(xintercept = mean(ret) - sd(ret), col = 'blue') +
  geom_vline(xintercept = mean(ret) + sd(ret), col = 'blue') +
  geom_vline(xintercept = mean(ret) - 2*sd(ret), col = 'green') +
  geom_vline(xintercept = mean(ret) + 2*sd(ret), col = 'green')
# printer(q, 'SP500Hist')

# Plot the log returns
ggplot(data = fortify(ret), aes(x = Index, y = ret)) + 
  geom_line() + 
  ggtitle('Log-returns') +
  xlab('Time') +
  ylab('log-return') +
  theme_minimal()

# Plot the absolute log returns
ggplot(data = fortify(ret), aes(x = Index, y = abs(ret))) + 
  geom_line() + 
  ggtitle('Absolute log-returns') +
  xlab('Time') +
  ylab('log-return') +
  theme_minimal()

# Plot the squared log returns
ggplot(data = fortify(ret), aes(x = Index, y = ret^2)) + 
  geom_line() + 
  ggtitle('Squared log-returns') +
  xlab('Time') +
  ylab('log-return') +
  theme_minimal()
