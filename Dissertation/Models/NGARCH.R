##### Specify the model ########################################################
ngarch.spec = ugarchspec(variance.model=list(model="fGARCH", garchOrder=c(1,1), 
                                             submodel = 'NGARCH'), 
                         mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                         distribution.model="norm")

##### Fit the data to the in sample ############################################
ngarch.fit <- ugarchfit(spec = ngarch.spec, data = ret, out.sample = oos.num)  
plot(ngarch.fit)
coef(ngarch.fit)
confint(ngarch.fit)
show(ngarch.fit)
# sinker(show(ngarch.fit), name = paste0(name,'_ngarch_fit'))

ngarch.fit@fit$matcoef
persistence(ngarch.fit)
##### Fix the variables to filter the oos data #################################
ngarch.spec.fixed <- getspec(ngarch.fit)
setfixed(ngarch.spec.fixed) <- as.list(coef(ngarch.fit))
ngarch.forc <- ugarchforecast(ngarch.spec.fixed, data = ret, n.ahead = 1, 
                              n.roll = oos.num-1, out.sample = oos.num-1)

plot(ngarch.forc)
##### Analyse the forecast #####################################################
# create a time series with the estimated and the real values
# We use the relized volatility as the squared returns
ngarch.result <- oos.sq
colnames(ngarch.result) <- c('RV')
ngarch.result$Sigma <- t(sigma(ngarch.forc))
ngarch.result$Sigma.sq <- ngarch.result$Sigma^2

# Plot the estimation
q <- ggplot(data = fortify(ngarch.result), aes(x = Index)) +
  geom_line(aes(y = RV)) +
  geom_line(aes(y = Sigma.sq), colour = 'red') +
  labs(title = 'Realized vs estimated volatility', x = 'Time', y = 'Volatility') +
  theme_bw()
# printer(q, paste0(name,'_ngarch_realvsestd'))

##### Test the volatility forecast #############################################
# Show the correlation between the forecast and the realized volatility
cor(ngarch.result$RV, ngarch.result$Sigma.sq, 
    method = "spearman")
# sinker(cor(ngarch.result$RV, ngarch.result$Sigma.sq,
# method = "spearman"), paste0(name,'_ngarch_cor'))

# Show the accuracy of our estimate
accuracy(ts(ngarch.result$Sigma.sq), ts(ngarch.result$RV))
mse(ts(ngarch.result$Sigma.sq), ts(ngarch.result$RV))
QLIKE(ngarch.result$Sigma.sq, ngarch.result$RV)

# sinker(accuracy(ts(ngarch.result$Sigma.sq), ts(ngarch.result$RV)), paste0(name,'_ngarch_accuracy'))
##### Analyse the residuals ####################################################
ngarch.lm <- lm(formula = RV ~ 0 + Sigma.sq, data = ts(ngarch.result))
plot(ngarch.lm)
summary(ngarch.lm)
# sinker(summary(ngarch.lm), name = paste0('_ngarch_lm'))
# accuracy(ngarch.lm)

# Extract residuals
ngarch.result$StdRes <- rstandard(ngarch.lm)
ngarch.result$Res <- residuals(ngarch.lm)

# sinker(accuracy(ngarch.lm),paste0(name,'_ngarch_accuracy'))
xtable(accuracy(ngarch.lm))

# Time series of the standardized residuals
q <- ggplot(data = fortify(ngarch.result), aes(x = Index)) +
  geom_line(aes(y = StdRes)) +
  labs(title = 'Standardized Residuals of ngarch Forecast', x = 'Time', 
       y = 'Standardized Residuals') +
  theme_bw()
# printer(q, paste0(name,'_ngarch_stdres'))
q

# QQ-Plot of standardized residuals
q <- ggplot(data = fortify(ngarch.result), aes(sample = StdRes)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = 'QQ-Plot of standardized Residuals', y = 'Standardized Residuals') +
  theme_bw()
# printer(q, paste0(name,'_ngarch_qq'))
q

# conditional variance

# Plot conditional variance
ggplot(data = fortify(ngarch.result), aes(x = Index, y = Sigma)) +
  geom_line() +
  labs(title = 'Conditional variance out-of-sample', x = 'Time', y = 'Cond. variance') +
  theme_bw()

##### Perform further tests on residuals #######################################
q <- gghistogram(ngarch.result$Res) +
  labs(title = 'Histogram of ngarch residuals', x = 'Residuals', y = 'Count') +
  theme_bw()
# printer(q, paste0(name,'_ngarch_res_hist'))
q

# Jarque Bera test for normality
q <- jarque.bera.test(ngarch.result$Res)
# sinker(jarque.bera.test(ngarch.result$Res), paste0(name, '_ngarch_res_jb'))
q
# Box test
q <- Box.test(x = ngarch.result$StdRes, type = 'Ljung-Box', lag = 1)
# sinker(q, paste0(name,'_ngarch_res_box'))
q

# Auto correlation plot
q <- ggAcf(ngarch.result$Res) + 
  labs(title = 'ACF: ngarch Residuals') +
  theme_bw()
# printer(q, paste0(name,'_ngarch_res_acf'))
q

# Auto correlation plot
q <- ggPacf(ngarch.result$Res^2) + 
  labs(title = 'PACF: ngarch Squared Residuals') +
  theme_bw()
# printer(q, paste0(name,'_ngarch_res_acf_2'))
q
