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
sinker(show(garch.fit), name = 'GARCH_fit')

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
# printer(q, 'GARCH_realvsestd')
q

##### Test the volatility forecast #############################################
# Show the correlation between the forecast and the realized volatility
cor(garch.result$RV, garch.result$Sigma.sq, 
    method = "spearman")

# Show the accuracy of our estimate
accuracy(ts(garch.result$RV), ts(garch.result$Sigma.sq))

##### Analyse the residuals ####################################################
garch.lm <- lm(formula = garch.result$RV ~ garch.result$Sigma.sq)
plot(garch.lm)
summary(garch.lm)
# sinker(summary(garch.lm), name = 'garch_lm')

# Extract residuals
garch.result$StdRes <- rstandard(garch.lm)
garch.result$Res <- residuals(garch.lm)

accuracy(garch.lm)
# sinker(accuracy(garch.lm),'garch_accuracy')
xtable(accuracy(garch.lm))

# Time series of the standardized residuals
q <- ggplot(data = fortify(garch.result), aes(x = Index)) +
  geom_line(aes(y = StdRes)) +
  labs(title = 'Standardized Residuals of GARCH Forecast', x = 'Time', 
       y = 'Standardized Residuals') +
  theme_bw()
# printer(q, 'GARCH_Stdres')
q

# QQ-Plot of standardized residuals
q <- ggplot(data = fortify(garch.result), aes(sample = StdRes)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = 'QQ-Plot of standardized Residuals', y = 'Standardized Residuals') +
  theme_bw()
# printer(q, 'GARCH_Stdres_qq')
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
# printer(q, 'GARCH_Res_Hist')
q

# Box test
q <- Box.test(x = garch.result$Res, type = 'Ljung-Box', lag = 5)
# sinker(q, 'garch_lm_box')
q

# Auto correlation plot
q <- ggAcf(garch.result$Res) + 
  labs(title = 'ACF: GARCH Residuals') +
  theme_bw()
# printer(q, 'GARCH_Res_Acf')
q
