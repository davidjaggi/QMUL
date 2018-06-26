##### Specify the model ########################################################
arch.spec = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(0,1)), 
                       mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                       distribution.model="norm")

##### Fit the data to the in sample ############################################
arch.fit <- ugarchfit(spec = arch.spec, data = ret, out.sample = oos.num)  
plot(arch.fit)
coef(arch.fit)
confint(arch.fit)
show(arch.fit)
# sinker(show(arch.fit), name = 'ARCH_fit')

arch.fit@fit$matcoef
persistence(arch.fit)
##### Fix the variables to filter the oos data #################################
arch.spec.fixed <- getspec(arch.fit)
setfixed(arch.spec.fixed) <- as.list(coef(arch.fit))
arch.forc <- ugarchforecast(arch.spec.fixed, data = ret, n.ahead = 1, 
                            n.roll = oos.num-1, out.sample = oos.num-1)

plot(arch.forc)
##### Analyse the forecast #####################################################
# create a time series with the estimated and the real values
# We use the relized volatility as the squared returns
arch.result <- oos.sq
colnames(arch.result) <- c('RV')
arch.result$Sigma <- t(sigma(arch.forc))
arch.result$Sigma.sq <- arch.result$Sigma^2

# Plot the estimation
q <- ggplot(data = fortify(arch.result), aes(x = Index)) +
  geom_line(aes(y = RV)) +
  geom_line(aes(y = Sigma.sq), colour = 'red') +
  labs(title = 'Realized vs estimated volatility', x = 'Time', y = 'Volatility') +
  theme_bw()
# printer(q, 'ARCH_realvsestd')
q

##### Test the volatility forecast #############################################
# Show the correlation between the forecast and the realized volatility
cor(arch.result$RV, arch.result$Sigma.sq, 
    method = "spearman")

# Show the accuracy of our estimate
accuracy(ts(arch.result$RV), ts(arch.result$Sigma.sq))

##### Analyse the residuals ####################################################
arch.lm <- lm(formula = arch.result$RV ~ arch.result$Sigma.sq)
plot(arch.lm)
summary(arch.lm)
# sinker(summary(arch.lm), name = 'arch_lm')

# Extract residuals
arch.result$StdRes <- rstandard(arch.lm)
arch.result$Res <- residuals(arch.lm)

accuracy(arch.lm)
# sinker(accuracy(arch.lm),'arch_accuracy')
xtable(accuracy(arch.lm))

# Time series of the standardized residuals
q <- ggplot(data = fortify(arch.result), aes(x = Index)) +
  geom_line(aes(y = StdRes)) +
  labs(title = 'Standardized Residuals of ARMA Forecast', x = 'Time', 
       y = 'Standardized Residuals') +
  theme_bw()
# printer(q, 'ARCH_Stdres')
q

# QQ-Plot of standardized residuals
q <- ggplot(data = fortify(arch.result), aes(sample = StdRes)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = 'QQ-Plot of standardized Residuals', y = 'Standardized Residuals') +
  theme_bw()
# printer(q, 'ARCH_Stdres_qq')
q

# conditional variance

# Plot conditional variance
ggplot(data = fortify(arch.result), aes(x = Index, y = Sigma)) +
  geom_line() +
  labs(title = 'Conditional variance out-of-sample', x = 'Time', y = 'Cond. variance') +
  theme_bw()

##### Perform further tests on residuals #######################################
q <- gghistogram(arch.result$Res) +
  labs(title = 'Histogram of ARCH residuals', x = 'Residuals', y = 'Count') +
  theme_bw()
# printer(q, 'ARCH_Res_Hist')
q

# Box test
q <- Box.test(x = arch.result$Res, type = 'Ljung-Box', lag = 5)
# sinker(q, 'arch_lm_box')
q

# Auto correlation plot
q <- ggAcf(arch.result$Res) + 
  labs(title = 'ACF: ARCH Residuals') +
  theme_bw()
# printer(q, 'ARCH_Res_Acf')
q
