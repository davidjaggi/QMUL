##### Specify the model ########################################################
egarch.spec = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)), 
                        mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                        distribution.model="norm")

##### Fit the data to the in sample ############################################
egarch.fit <- ugarchfit(spec = egarch.spec, data = ret, out.sample = oos.num)  
plot(egarch.fit)
coef(egarch.fit)
confint(egarch.fit)
show(egarch.fit)
# sinker(show(egarch.fit), name = paste0(name,'_egarch_fit'))

egarch.fit@fit$matcoef
persistence(egarch.fit)
##### Fix the variables to filter the oos data #################################
egarch.spec.fixed <- getspec(egarch.fit)
setfixed(egarch.spec.fixed) <- as.list(coef(egarch.fit))
egarch.forc <- ugarchforecast(egarch.spec.fixed, data = ret, n.ahead = 1, 
                             n.roll = oos.num-1, out.sample = oos.num-1)

plot(egarch.forc)
##### Analyse the forecast #####################################################
# create a time series with the estimated and the real values
# We use the relized volatility as the squared returns
egarch.result <- oos.sq
colnames(egarch.result) <- c('RV')
egarch.result$Sigma <- t(sigma(egarch.forc))
egarch.result$Sigma.sq <- egarch.result$Sigma^2

# Plot the estimation
q <- ggplot(data = fortify(egarch.result), aes(x = Index)) +
  geom_line(aes(y = RV)) +
  geom_line(aes(y = Sigma.sq), colour = 'red') +
  labs(title = 'Realized vs estimated volatility', x = 'Time', y = 'Volatility') +
  theme_bw()
# printer(q, paste0(name,'_egarch_realvsestd'))

##### Test the volatility forecast #############################################
# Show the correlation between the forecast and the realized volatility
cor(egarch.result$RV, egarch.result$Sigma.sq, 
    method = "spearman")
# sinker(cor(egarch.result$RV, egarch.result$Sigma.sq,
           # method = "spearman"), paste0(name,'_egarch_cor'))

# Show the accuracy of our estimate
accuracy(ts(egarch.result$Sigma.sq), ts(egarch.result$RV))
mse(ts(egarch.result$Sigma.sq), ts(egarch.result$RV))
QLIKE(egarch.result$Sigma.sq, egarch.result$RV)

sinker(accuracy(ts(egarch.result$RV), ts(egarch.result$Sigma.sq)))
##### Analyse the residuals ####################################################
egarch.lm <- lm(formula = RV ~ 0 + Sigma.sq, data = ts(egarch.result))
plot(egarch.lm)
summary(egarch.lm)
# sinker(summary(egarch.lm), name = paste0('_egarch_lm'))
accuracy(egarch.lm)

# Extract residuals
egarch.result$StdRes <- rstandard(egarch.lm)
egarch.result$Res <- residuals(egarch.lm)

# sinker(accuracy(egarch.lm),paste0(name,'_egarch_accuracy'))
xtable(accuracy(egarch.lm))

# Time series of the standardized residuals
q <- ggplot(data = fortify(egarch.result), aes(x = Index)) +
  geom_line(aes(y = StdRes)) +
  labs(title = 'Standardized Residuals of GARCH Forecast', x = 'Time', 
       y = 'Standardized Residuals') +
  theme_bw()
# printer(q, paste0(name,'_garch_stdres'))
q

# QQ-Plot of standardized residuals
q <- ggplot(data = fortify(egarch.result), aes(sample = StdRes)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = 'QQ-Plot of standardized Residuals', y = 'Standardized Residuals') +
  theme_bw()
# printer(q, paste0(name,'_garch_qq'))
q

# conditional variance

# Plot conditional variance
ggplot(data = fortify(egarch.result), aes(x = Index, y = Sigma)) +
  geom_line() +
  labs(title = 'Conditional variance out-of-sample', x = 'Time', y = 'Cond. variance') +
  theme_bw()

##### Perform further tests on residuals #######################################
q <- gghistogram(egarch.result$Res) +
  labs(title = 'Histogram of GARCH residuals', x = 'Residuals', y = 'Count') +
  theme_bw()
# printer(q, paste0(name,'_garch_res_hist'))
q

# Jarque Bera test for normality
q <- jarque.bera.test(egarch.result$Res)
# sinker(jarque.bera.test(egarch.result$Res), paste0(name, '_garch_res_jb'))
q
# Box test
q <- Box.test(x = egarch.result$StdRes, type = 'Ljung-Box', lag = 1)
# sinker(q, paste0(name,'_garch_res_box'))
q

# Auto correlation plot
q <- ggAcf(egarch.result$Res) + 
  labs(title = 'ACF: EGARCH Residuals') +
  theme_bw()
# printer(q, paste0(name,'_egarch_res_acf'))
q

# Auto correlation plot
q <- ggPacf(egarch.result$Res^2) + 
  labs(title = 'PACF: EGARCH Squared Residuals') +
  theme_bw()
# printer(q, paste0(name,'_egarch_res_acf_2'))
q
