##### Specify the model ########################################################
tgarch.spec = ugarchspec(variance.model=list(model="fGARCH", garchOrder=c(1,1), 
                                             submodel = 'TGARCH'), 
                         mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                         distribution.model="norm")

##### Fit the data to the in sample ############################################
tgarch.fit <- ugarchfit(spec = tgarch.spec, data = ret, out.sample = oos.num)  
plot(tgarch.fit)
coef(tgarch.fit)
confint(tgarch.fit)
show(tgarch.fit)
# sinker(show(tgarch.fit), name = paste0(name,'_tgarch_fit'))

tgarch.fit@fit$matcoef
persistence(tgarch.fit)
##### Fix the variables to filter the oos data #################################
tgarch.spec.fixed <- getspec(tgarch.fit)
setfixed(tgarch.spec.fixed) <- as.list(coef(tgarch.fit))
tgarch.forc <- ugarchforecast(tgarch.spec.fixed, data = ret, n.ahead = 1, 
                              n.roll = oos.num-1, out.sample = oos.num-1)

plot(tgarch.forc)
##### Analyse the forecast #####################################################
# create a time series with the estimated and the real values
# We use the relized volatility as the squared returns
tgarch.result <- oos.sq
colnames(tgarch.result) <- c('RV')
tgarch.result$Sigma <- t(sigma(tgarch.forc))
tgarch.result$Sigma.sq <- tgarch.result$Sigma^2

# Plot the estimation
q <- ggplot(data = fortify(tgarch.result), aes(x = Index)) +
  geom_line(aes(y = RV)) +
  geom_line(aes(y = Sigma.sq), colour = 'red') +
  labs(title = 'Realized vs estimated volatility', x = 'Time', y = 'Volatility') +
  theme_bw()
# printer(q, paste0(name,'_tgarch_realvsestd'))

##### Test the volatility forecast #############################################
# Show the correlation between the forecast and the realized volatility
cor(tgarch.result$RV, tgarch.result$Sigma.sq, 
    method = "spearman")
# sinker(cor(tgarch.result$RV, tgarch.result$Sigma.sq,
# method = "spearman"), paste0(name,'_tgarch_cor'))

# Show the accuracy of our estimate
accuracy(ts(tgarch.result$Sigma.sq), ts(tgarch.result$RV))
mse(ts(tgarch.result$Sigma.sq), ts(tgarch.result$RV))
QLIKE(tgarch.result$Sigma.sq, tgarch.result$RV)

# sinker(accuracy(ts(tgarch.result$Sigma.sq), ts(tgarch.result$RV)), paste0(name,'_tgarch_accuracy'))
##### Analyse the residuals ####################################################
tgarch.lm <- lm(formula = RV ~ 0 + Sigma.sq, data = ts(tgarch.result))
plot(tgarch.lm)
summary(tgarch.lm)
# sinker(summary(tgarch.lm), name = paste0('_tgarch_lm'))
# accuracy(tgarch.lm)

# Extract residuals
tgarch.result$StdRes <- rstandard(tgarch.lm)
tgarch.result$Res <- residuals(tgarch.lm)

# sinker(accuracy(tgarch.lm),paste0(name,'_tgarch_accuracy'))
xtable(accuracy(tgarch.lm))

# Time series of the standardized residuals
q <- ggplot(data = fortify(tgarch.result), aes(x = Index)) +
  geom_line(aes(y = StdRes)) +
  labs(title = 'Standardized Residuals of TGARCH Forecast', x = 'Time', 
       y = 'Standardized Residuals') +
  theme_bw()
# printer(q, paste0(name,'_tgarch_stdres'))
q

# QQ-Plot of standardized residuals
q <- ggplot(data = fortify(tgarch.result), aes(sample = StdRes)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = 'QQ-Plot of standardized Residuals', y = 'Standardized Residuals') +
  theme_bw()
# printer(q, paste0(name,'_tgarch_qq'))
q

# conditional variance

# Plot conditional variance
ggplot(data = fortify(tgarch.result), aes(x = Index, y = Sigma)) +
  geom_line() +
  labs(title = 'Conditional variance out-of-sample', x = 'Time', y = 'Cond. variance') +
  theme_bw()

##### Perform further tests on residuals #######################################
q <- gghistogram(tgarch.result$Res) +
  labs(title = 'Histogram of TGARCH residuals', x = 'Residuals', y = 'Count') +
  theme_bw()
# printer(q, paste0(name,'_tgarch_res_hist'))
q

# Jarque Bera test for normality
q <- jarque.bera.test(tgarch.result$Res)
# sinker(jarque.bera.test(tgarch.result$Res), paste0(name, '_garch_res_jb'))
q
# Box test
q <- Box.test(x = tgarch.result$StdRes, type = 'Ljung-Box', lag = 1)
# sinker(q, paste0(name,'_garch_res_box'))
q

# Auto correlation plot
q <- ggAcf(tgarch.result$Res) + 
  labs(title = 'ACF: tgarch Residuals') +
  theme_bw()
# printer(q, paste0(name,'_tgarch_res_acf'))
q

# Auto correlation plot
q <- ggPacf(tgarch.result$Res^2) + 
  labs(title = 'PACF: tgarch Squared Residuals') +
  theme_bw()
# printer(q, paste0(name,'_tgarch_res_acf_2'))
q
