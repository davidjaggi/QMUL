##### Creat ARMA Model and get parameters ######################################
source('Dissertation/Prep/Setup.R')


# Create the ARMA Model
arima.model <- autoarfima(data = as.numeric(is), ar.max = 3, ma.max = 3, 
                          criterion = 'AIC', method = 'partial', arfima = FALSE, 
                          include.mean = NULL, distribution.model = 'norm', 
                          solver = 'hybrid', cluster=cl)
# Show the different models
show(head(arima.model$rank.matrix))
arima.model
# suggests a ARIMA(3,0,3) with a non zero mean


ar.comp <- arimaorder(object = arima.model)[1]
ma.comp <- arimaorder(object = arima.model)[3]

arima.spec <- arfimaspec(arfima = FALSE, distribution.model = 'norm')
##### Fit the data to the in sample ############################################
arima.fit <- arfimafit(spec = arima.spec, data = is)  
coef(arima.fit)
show(arima.fit)

arima.fit@fit$matcoef
##### Fix the variables to filter the oos data #################################
arima.spec.fixed <- getspec(arima.fit)
setfixed(arima.spec.fixed) <- as.list(coef(arima.fit))
arima.filt <- arfimafilter(spec = arima.spec.fixed, 
                           data = oos)

plot(arima.filt)

##### Extract varaibles ########################################################
arima.resid <- residuals(arima.filt)

# Plot the residuals
ggplot(data = fortify(arima.resid), aes(x = Index, y=arima.resid)) +
  geom_line() +
  labs(title = 'ARIMA Residuals', x='Time',y='Residuals') +
  theme_bw()

##### Analyse the forecast #####################################################
# create a time series with the estimated and the real values
# We use the relized volatility as the squared returns
arima.result <- oos.rv
colnames(arima.result) <- c('RV')
arima.result$Sigma <- sigma(arima.filt)
arima.result$Sigma.sq <- arima.result$Sigma^2

# Plot the estimation
ggplot(data = fortify(arima.result), aes(x = Index)) +
  geom_line(aes(y = RV)) +
  geom_line(aes(y = Sigma.sq), colour = 'red') +
  labs(title = 'Realized vs estimated volatility', x = 'Time', y = 'Volatility') +
  theme_bw()

##### Test the volatility forecast #############################################
# Show the correlation between the forecast and the realized volatility
cor(arima.result$RV, arima.result$Sigma.sq, 
    method = "spearman")

# Show the accuracy of our estimate
accuracy(ts(arima.result$RV), ts(arima.result$Sigma.sq))

# make a regression
arima.lm <- lm(formula = oos.sq ~ arima.sigma.sq)
plot(arima.lm)
summary(arima.lm)
accuracy(arima.lm)
# xtable(accuracy(arima.lm))
error_MAD(arima.result$OOS, arima.result$Sigma)



ggplot(fortify(arima.result), aes(x = Index)) +
  # geom_line(aes(y = abs(OOS))) +
  # geom_line(aes(y = Sigma)) +
  geom_line(aes(y = Resid)) +
  theme_bw() +
  
  
  plot(arima.filt@filter$z)
infocriteria(arima.filt)
plot(sigma(arima.filt))

# conditional variance
arima.sigma <- sigma(arima.filt)

# Plot conditional variance
ggplot(data = fortify(arima.sigma), aes(x = Index, y = arima.sigma)) +
  geom_line() +
  labs(title = 'Conditional variance out-of-sample', x = 'Time', y = 'Cond. variance') +
  theme_bw()

arima.resid <- residuals(arima.filt)
arima.stresid <- residuals(arima.filt, standardize = TRUE)

# calculate approximate z value
arima.resid/arima.sigma

##### 
ggplot(data = fortify(oos),aes(x = Index, y = oos)) + geom_line()

plot(fitted(arima.filt))
report(arima.filt)
