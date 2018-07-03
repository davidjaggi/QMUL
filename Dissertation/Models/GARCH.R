##### Specify the model ########################################################
garch.spec = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                       mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                       distribution.model="norm")

##### Fit the data to the in sample ############################################
garch.fit <- ugarchfit(spec = garch.spec, data = ret, out.sample = oos.num)  
plot(garch.fit)
coef(garch.fit)
confint(garch.fit)
show(garch.fit)
# sinker(show(garch.fit), name = paste0(name,'_garch_fit'))

garch.fit@fit$matcoef
persistence(garch.fit)
##### Fix the variables to filter the oos data #################################
garch.spec.fixed <- getspec(garch.fit)
setfixed(garch.spec.fixed) <- as.list(coef(garch.fit))
garch.forc <- ugarchforecast(garch.spec.fixed, data = ret, n.ahead = 1, 
                            n.roll = oos.num-1, out.sample = oos.num-1)

plot(garch.forc)
##### Analyse the forecast #####################################################
# create a time series with the estimated and the real values
# We use the relized volatility as the squared returns
garch.result <- oos.sq
colnames(garch.result) <- c('RV')
garch.result$Sigma <- t(sigma(garch.forc))
garch.result$Sigma.sq <- garch.result$Sigma^2

# Plot the estimation
q <- ggplot(data = fortify(garch.result), aes(x = Index)) +
  geom_line(aes(y = RV)) +
  geom_line(aes(y = Sigma.sq), colour = 'red') +
  labs(title = 'Realized vs estimated volatility', x = 'Time', y = 'Volatility') +
  theme_bw()
# printer(q, paste0(name,'_garch_realvsestd'))

##### Test the volatility forecast #############################################
# Show the correlation between the forecast and the realized volatility
cor(garch.result$RV, garch.result$Sigma.sq, 
    method = "spearman")
# sinker(cor(garch.result$RV, garch.result$Sigma.sq, 
#            method = "spearman"), paste0(name,'_garch_cor'))

# Show the accuracy of our estimate
accuracy(ts(garch.result$Sigma.sq), ts(garch.result$RV))

##### Analyse the residuals ####################################################
garch.lm <- lm(formula = garch.result$RV ~ garch.result$Sigma.sq)
plot(garch.lm)
summary(garch.lm)
# sinker(summary(garch.lm), name = paste0('_garch_lm'))
accuracy(garch.lm)


# Extract residuals
garch.result$StdRes <- rstandard(garch.lm)
garch.result$Res <- residuals(garch.lm)

# sinker(accuracy(garch.lm), paste0(name,'_garch_accuracy'))
xtable(accuracy(garch.lm))

# Time series of the standardized residuals
q <- ggplot(data = fortify(garch.result), aes(x = Index)) +
  geom_line(aes(y = StdRes)) +
  labs(title = 'Standardized Residuals of GARCH Forecast', x = 'Time', 
       y = 'Standardized Residuals') +
  theme_bw()
# printer(q, paste0(name,'_garch_stdres'))
q

# QQ-Plot of standardized residuals
q <- ggplot(data = fortify(garch.result), aes(sample = StdRes)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = 'QQ-Plot of standardized Residuals', y = 'Standardized Residuals') +
  theme_bw()
# printer(q, paste0(name,'_garch_qq'))
q

# conditional variance

# Plot conditional variance
ggplot(data = fortify(garch.result), aes(x = Index, y = Sigma)) +
  geom_line() +
  labs(title = 'Conditional variance out-of-sample', x = 'Time', y = 'Cond. variance') +
  theme_bw()

##### Perform further tests on residuals #######################################
q <- gghistogram(garch.result$Res) +
  labs(title = 'Histogram of GARCH residuals', x = 'Residuals', y = 'Count') +
  theme_bw()
# printer(q, paste0(name,'_garch_res_hist'))
q

# Jarque Bera test for normality
q <- jarque.bera.test(garch.result$Res)
# sinker(jarque.bera.test(garch.result$Res), paste0(name, '_garch_res_jb'))
q
# Box test
q <- Box.test(x = garch.result$StdRes, type = 'Ljung-Box', lag = 1)
# sinker(q, paste0(name,'_garch_res_box'))
q

# Auto correlation plot
q <- ggAcf(garch.result$Res) + 
  labs(title = 'ACF: GARCH Residuals') +
  theme_bw()
# printer(q, 'GARCH_Res_Acf')
q
