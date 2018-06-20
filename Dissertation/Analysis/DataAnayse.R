##### Plot time series #########################################################
# Plot the data
# Troubleshoot why I need an autoplot.zoo

# Save as png
q <- autoplot.zoo(x) +
  ggtitle('SP500') +
  xlab('Time') +
  ylab('Price') +
  theme_bw()
# printer(q, name = 'SP_Series')

##### Analyse the time series ##################################################
# Delete the NA's
# x <- na.omit(x)
# Fill the NA's with the previous values
x <- na.locf(x)
sum(is.na(x))

# Generate the summary
summary(x)
xtable(summary(x))
# sinker(xtable(summary(x)), name = 'SP_Summary')


# Now check for NA's
sum(is.na(x)) # in this series we have 91 NA's

##### Analyse the series for autocorrelation ###################################
# check for heteroscedasticity
# H0: There is no autoregressive conditional heteroscedasticity
# H1: There is autoregressive heteroscedaticity
t <- McLeod.Li.test(y = ret)$p.values
i <- seq(1,34) 
df <- data.frame(i,t)

q <- ggplot(data = df, aes(x = i)) +
  geom_point(y = t) +
  labs(title = 'McLeod-Li Test', x = 'Lag', y = 'P-Value') +
  geom_hline(yintercept = 0.05, colour = 'red', linetype = 'dashed') +
  ylim(0,1) +
  xlim(0,35) +
  theme_bw()
# printer(plot = q, name = 'SP_McLeod')

# Show a corrolelogram
# ACF
q <- autoplot(forecast::Acf(x)) +
  ggtitle('Autocorrelation function') +
  xlab('Lag') +
  ylab('ACF') +
  theme_bw()
# printer(q, name = 'SP_Acf')

q <- autoplot(forecast::Acf(x^2)) +
  ggtitle('Autocorrelation function') +
  xlab('Lag') +
  ylab('ACF') +
  theme_bw()
# printer(q, name = 'SP_Acf2')

q <- autoplot(forecast::Acf(abs(x))) +
  ggtitle('Autocorrelation function') +
  xlab('Lag') +
  ylab('ACF') +
  theme_bw()
# printer(q, name = 'SP_AcfAbs')
##### Analyse returns ##########################################################
summary(ret)
xtable(summary(ret))
# sinker(xtable(summary(ret)), name = 'SPret_Summary')

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
  theme_bw()
# printer(plot = q, name = 'SPret_Acf')

q <- autoplot(forecast::Acf(abs(ret))) +
  ggtitle('ACF: Absolute Return') +
  xlab('Lag') +
  ylab('ACF') +
  theme_bw()
# printer(q, name = 'SPret_AcfAbs')

q <- autoplot(forecast::Acf(ret^2)) +
  ggtitle('ACF: Squared Return') +
  xlab('Lag') +
  ylab('ACF') +
  theme_bw()
# printer(q, 'SPret_Acf2')

##### Plot the PACF of the return series #######################################
# Return ACF
# q <-autoplot(forecast::Pacf(ret)) +
#   ggtitle('PACF: Return') +
#   xlab('Lag') +
#   ylab('PACF') +
#   theme_bw()
# printer(plot = q, name = 'SPret_Pacf')
# 
# q <- autoplot(forecast::Pacf(abs(ret))) +
#   ggtitle('PACF: Absolute Return') +
#   xlab('Lag') +
#   ylab('PACF') +
#   theme_bw()
# printer(q, name = 'SPret_PacfAbs')
# 
# q <- autoplot(forecast::Pacf(ret^2)) +
#   ggtitle('PACF: Squared Return') +
#   xlab('Lag') +
#   ylab('PACF') +
#   theme_bw()
# printer(q, 'SPret_Pacf2')

##### Make a histogram of the return series ####################################
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
  theme_bw()

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
