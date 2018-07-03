##### Creat ARMA Model and get parameters ######################################
source('Dissertation/Prep/Setup.R')

# 
# # Create the ARMA Model
# arima.model <- autoarfima(data = as.numeric(ret), ar.max = 3, ma.max = 3, 
#                           criterion = 'AIC', method = 'partial', arfima = FALSE, 
#                           include.mean = NULL, distribution.model = 'norm', 
#                           solver = 'hybrid')
# # Show the different models
# show(head(arima.model$rank.matrix))
# 
# arima.model
# # suggests a ARIMA(3,0,3) with a non zero mean
# 
# 
# ar.comp <- arimaorder(object = arima.model)[1]
# ma.comp <- arimaorder(object = arima.model)[3]

##### Specify ARIMA model ######################################################
arima.spec <- arfimaspec(arfima = FALSE, distribution.model = 'norm')

##### Fit the data to the in sample ############################################
arima.fit <- arfimafit(spec = arima.spec, data = ret, out.sample = oos.num)
# sinker(show(arima.fit), 'arima_fit')
coef(arima.fit)

q <- show(arima.fit)
sinker(show(arima.fit), name = paste0(name,'_arima_fit'))

arima.fit@fit$matcoef
# sinker(arima.fit@fit$matcoef, paste0(name,'_arima_matcoef'))

##### Fix the variables to forecast the oos data ###############################
arima.spec.fixed <- getspec(arima.fit)
setfixed(arima.spec.fixed) <- as.list(coef(arima.fit))
arima.forc <- arfimaforecast(arima.spec.fixed, data = ret, n.ahead = 1, 
                             n.roll = oos.num-1, out.sample = oos.num-1)

##### Extract varaibles ########################################################
arima.fitted <- t(fitted(arima.forc))

# Calculate the residuals
arima.resid <- oos - arima.fitted

# Plot the residuals
q <- ggplot(data = fortify(arima.resid), aes(x = Index, y=arima.resid)) +
  geom_line() +
  labs(title = 'ARMA Residuals', x='Time',y='Residuals') +
  theme_bw()
printer(q, paste0(name,'_arima_resid'))

##### Analyse the forecast #####################################################
# create a time series with the estimated and the real values
# We use the relized volatility as the squared returns
arima.result <- oos.sq
colnames(arima.result) <- c('RV')
arima.result$Sigma <- t(fitted(arima.forc))
arima.result$Sigma.sq <- arima.result$Sigma^2

# Plot the estimation
q <- ggplot(data = fortify(arima.result), aes(x = Index)) +
  geom_line(aes(y = RV)) +
  geom_line(aes(y = Sigma.sq), colour = 'red') +
  labs(title = 'Realized vs estimated volatility', x = 'Time', y = 'Volatility') +
  theme_bw()
# printer(q, paste0(name,'_arima_realvsestd'))

##### Test the volatility forecast #############################################
# Show the correlation between the forecast and the realized volatility
cor(arima.result$RV, arima.result$Sigma.sq, 
    method = "spearman")

# Show the accuracy of our estimate
accuracy(ts(arima.result$RV), ts(arima.result$Sigma.sq))

# make a regression
arima.lm <- lm(formula = arima.result$RV ~ arima.result$Sigma.sq)
plot(arima.lm)

##### Analyse the residuals ####################################################
summary(arima.lm)
# sinker(summary(arima.lm), paste0(name,'_arima_lm'))
arima.result$StdRes <- rstandard(arima.lm)
arima.result$Res <- residuals(arima.lm)

# Time series of the standardized residuals
q <- ggplot(data = fortify(arima.result), aes(x = Index)) +
  geom_line(aes(y = StdRes)) +
  labs(title = 'Standardized Residuals of ARMA Forecast', x = 'Time', 
       y = 'Standardized Residuals') +
  theme_bw()
q
# printer(q, paste0(name,'_arima_stdres'))

q <- ggplot(data = fortify(arima.result), aes(sample = StdRes)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = 'QQ-Plot of standardized Residuals', y = 'Standardized Residuals') +
  theme_bw()
# printer(q, paste0(name,'_arima_stdres_qq'))
q


accuracy(arima.lm)
# sinker(accuracy(arima.lm),paste0(name'_arima_accuracy'))
xtable(accuracy(arima.lm))

##### Perform further tests on residuals #######################################
q <- gghistogram(arima.result$Res) +
  labs(title = 'Histogram of ARMA residuals', x = 'Residuals', y = 'Count') +
  theme_bw()
# printer(q, paste0(name'_arima_res_hist'))

# Box test
q <- Box.test(x = arima.result$Res, type = 'Ljung-Box', lag = 5)
# sinker(q, paste0(name,'_arima_lm_box')

# Auto correlation plot
q <- ggAcf(arima.result$Res) + 
  labs(title = 'ACF: ARMA Residuals') +
  theme_bw()
q
# printer(q, 'ARIMA_Res_Acf')


