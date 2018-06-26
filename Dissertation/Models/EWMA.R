##### Specify the model ########################################################
ewma.spec = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)), 
                       mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                       distribution.model="norm", fixed.pars=list(omega=0))

##### Fit the data to the in sample ############################################
ewma.fit <- ugarchfit(spec = ewma.spec, data = ret, out.sample = oos.num)  
plot(ewma.fit)
coef(ewma.fit)
confint(ewma.fit)
show(ewma.fit)
sinker(show(ewma.fit), name = 'GARCH_fit')

ewma.fit@fit$matcoef
persistence(ewma.fit)
##### Fix the variables to filter the oos data #################################
ewma.spec.fixed <- getspec(ewma.fit)
setfixed(ewma.spec.fixed) <- as.list(coef(ewma.fit))
ewma.forc <- ugarchforecast(garch.spec.fixed, data = ret, n.ahead = 1, 
                             n.roll = oos.num-1, out.sample = oos.num-1)

plot(ewma.forc)
##### Analyse the forecast #####################################################
# create a time series with the estimated and the real values
# We use the relized volatility as the squared returns
ewma.result <- oos.sq
colnames(ewma.result) <- c('RV')
ewma.result$Sigma <- t(sigma(ewma.forc))
ewma.result$Sigma.sq <- ewma.result$Sigma^2

# Plot the estimation
q <- ggplot(data = fortify(ewma.result), aes(x = Index)) +
  geom_line(aes(y = RV)) +
  geom_line(aes(y = Sigma.sq), colour = 'red') +
  labs(title = 'Realized vs estimated volatility', x = 'Time', y = 'Volatility') +
  theme_bw()
# printer(q, 'GARCH_realvsestd')
q

##### Test the volatility forecast #############################################
# Show the correlation between the forecast and the realized volatility
cor(ewma.result$RV, ewma.result$Sigma.sq, 
    method = "spearman")

# Show the accuracy of our estimate
accuracy(ts(ewma.result$RV), ts(ewma.result$Sigma.sq))

##### Analyse the residuals ####################################################
ewma.lm <- lm(formula = ewma.result$RV ~ ewma.result$Sigma.sq)
plot(ewma.lm)
summary(ewma.lm)
# sinker(summary(ewma.lm), name = 'ewma_lm')

# Extract residuals
ewma.result$StdRes <- rstandard(ewma.lm)
ewma.result$Res <- residuals(ewma.lm)

accuracy(ewma.lm)
# sinker(accuracy(ewma.lm),'ewma_accuracy')
xtable(accuracy(ewma.lm))

# Time series of the standardized residuals
q <- ggplot(data = fortify(ewma.result), aes(x = Index)) +
  geom_line(aes(y = StdRes)) +
  labs(title = 'Standardized Residuals of GARCH Forecast', x = 'Time', 
       y = 'Standardized Residuals') +
  theme_bw()
# printer(q, 'GARCH_Stdres')
q

# QQ-Plot of standardized residuals
q <- ggplot(data = fortify(ewma.result), aes(sample = StdRes)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = 'QQ-Plot of standardized Residuals', y = 'Standardized Residuals') +
  theme_bw()
# printer(q, 'GARCH_Stdres_qq')
q

# conditional variance

# Plot conditional variance
ggplot(data = fortify(ewma.result), aes(x = Index, y = Sigma)) +
  geom_line() +
  labs(title = 'Conditional variance out-of-sample', x = 'Time', y = 'Cond. variance') +
  theme_bw()

##### Perform further tests on residuals #######################################
q <- gghistogram(ewma.result$Res) +
  labs(title = 'Histogram of GARCH residuals', x = 'Residuals', y = 'Count') +
  theme_bw()
# printer(q, 'GARCH_Res_Hist')
q

# Box test
q <- Box.test(x = ewma.result$Res, type = 'Ljung-Box', lag = 5)
# sinker(q, 'ewma_lm_box')
q

# Auto correlation plot
q <- ggAcf(ewma.result$Res) + 
  labs(title = 'ACF: GARCH Residuals') +
  theme_bw()
# printer(q, 'GARCH_Res_Acf')
q
