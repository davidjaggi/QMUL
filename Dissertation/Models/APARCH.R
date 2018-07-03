##### Specify the model ########################################################
aparch.spec = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1)), 
                         mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                         distribution.model="norm")

##### Fit the data to the in sample ############################################
aparch.fit <- ugarchfit(spec = aparch.spec, data = ret, out.sample = oos.num)  
plot(aparch.fit)
coef(aparch.fit)
confint(aparch.fit)
show(aparch.fit)
# sinker(show(aparch.fit), name = paste0(name,'_aparch_fit'))

aparch.fit@fit$matcoef
persistence(aparch.fit)
##### Fix the variables to filter the oos data #################################
aparch.spec.fixed <- getspec(aparch.fit)
setfixed(aparch.spec.fixed) <- as.list(coef(aparch.fit))
aparch.forc <- ugarchforecast(aparch.spec.fixed, data = ret, n.ahead = 1, 
                              n.roll = oos.num-1, out.sample = oos.num-1)

plot(aparch.forc)
##### Analyse the forecast #####################################################
# create a time series with the estimated and the real values
# We use the relized volatility as the squared returns
aparch.result <- oos.sq
colnames(aparch.result) <- c('RV')
aparch.result$Sigma <- t(sigma(aparch.forc))
aparch.result$Sigma.sq <- aparch.result$Sigma^2

# Plot the estimation
q <- ggplot(data = fortify(aparch.result), aes(x = Index)) +
  geom_line(aes(y = RV)) +
  geom_line(aes(y = Sigma.sq), colour = 'red') +
  labs(title = 'Realized vs estimated volatility', x = 'Time', y = 'Volatility') +
  theme_bw()
# printer(q, paste0(name,'_aparch_realvsestd'))

##### Test the volatility forecast #############################################
# Show the correlation between the forecast and the realized volatility
cor(aparch.result$RV, aparch.result$Sigma.sq, 
    method = "spearman")
# sinker(cor(aparch.result$RV, aparch.result$Sigma.sq,
# method = "spearman"), paste0(name,'_aparch_cor'))

# Show the accuracy of our estimate
accuracy(ts(aparch.result$Sigma.sq), ts(aparch.result$RV))
mse(ts(aparch.result$Sigma.sq), ts(aparch.result$RV))
QLIKE(aparch.result$Sigma.sq, aparch.result$RV)

# sinker(accuracy(ts(aparch.result$Sigma.sq), ts(aparch.result$RV)), paste0(name,'_aparch_accuracy'))
##### Analyse the residuals ####################################################
aparch.lm <- lm(formula = RV ~ 0 + Sigma.sq, data = ts(aparch.result))
plot(aparch.lm)
summary(aparch.lm)
# sinker(summary(aparch.lm), name = paste0('_aparch_lm'))
# accuracy(aparch.lm)

# Extract residuals
aparch.result$StdRes <- rstandard(aparch.lm)
aparch.result$Res <- residuals(aparch.lm)

# sinker(accuracy(aparch.lm),paste0(name,'_aparch_accuracy'))
xtable(accuracy(aparch.lm))

# Time series of the standardized residuals
q <- ggplot(data = fortify(aparch.result), aes(x = Index)) +
  geom_line(aes(y = StdRes)) +
  labs(title = 'Standardized Residuals of aparch Forecast', x = 'Time', 
       y = 'Standardized Residuals') +
  theme_bw()
# printer(q, paste0(name,'_aparch_stdres'))
q

# QQ-Plot of standardized residuals
q <- ggplot(data = fortify(aparch.result), aes(sample = StdRes)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = 'QQ-Plot of standardized Residuals', y = 'Standardized Residuals') +
  theme_bw()
# printer(q, paste0(name,'_aparch_qq'))
q

# conditional variance

# Plot conditional variance
ggplot(data = fortify(aparch.result), aes(x = Index, y = Sigma)) +
  geom_line() +
  labs(title = 'Conditional variance out-of-sample', x = 'Time', y = 'Cond. variance') +
  theme_bw()

##### Perform further tests on residuals #######################################
q <- gghistogram(aparch.result$Res) +
  labs(title = 'Histogram of aparch residuals', x = 'Residuals', y = 'Count') +
  theme_bw()
# printer(q, paste0(name,'_aparch_res_hist'))
q

# Jarque Bera test for normality
q <- jarque.bera.test(aparch.result$Res)
# sinker(jarque.bera.test(aparch.result$Res), paste0(name, '_aparch_res_jb'))
q
# Box test
q <- Box.test(x = aparch.result$StdRes, type = 'Ljung-Box', lag = 1)
# sinker(q, paste0(name,'_garch_res_box'))
q

# Auto correlation plot
q <- ggAcf(aparch.result$Res) + 
  labs(title = 'ACF: APARCH Residuals') +
  theme_bw()
# printer(q, paste0(name,'_aparch_res_acf'))
q

# Auto correlation plot
q <- ggPacf(aparch.result$Res^2) + 
  labs(title = 'PACF: APARCH Squared Residuals') +
  theme_bw()
# printer(q, paste0(name,'_aparch_res_acf_2'))
q
