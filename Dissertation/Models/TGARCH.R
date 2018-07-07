##### Specify the model ########################################################
tgarch.spec = ugarchspec(variance.model=list(model="fGARCH", garchOrder=c(1,1), 
                                             submodel = 'TGARCH'), 
                         mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                         distribution.model="std")

##### Fit the data to the in sample ############################################
tgarch.fit <- ugarchfit(spec = tgarch.spec, data = ret, out.sample = oos.num, 
                       solver = 'hybrid')  
plot(tgarch.fit)
coef(tgarch.fit)
show(tgarch.fit)
# sinker(show(tgarch.fit), name = paste0(name,'_tgarch_fit'))

tgarch.fit@fit$matcoef
# sinker(tgarch.fit@fit$matcoef, name = paste0(name,'_tgarch_fit_matcoef'))
persistence(tgarch.fit)
##### analyse the is fit #######################################################
# sinker(signbias(tgarch.fit), paste0(name,'_tgarch_fit_sign'))
# sinker(infocriteria(tgarch.fit), paste0(name,'_tgarch_fit_info'))
# sinker(nyblom(tgarch.fit), paste0(name,'_tgarch_fit_nyblom'))
# sinker(gof(tgarch.fit,c(20,30,40,50)), paste0(name,'_tgarch_fit_gof'))

# Make the newsimpactcurve
tgarch.ni <- newsimpact(object = tgarch.fit, z = NULL)

q <- qplot(tgarch.ni$zx, tgarch.ni$zy, ylab=tgarch.ni$yexpr, xlab=tgarch.ni$xexpr, 
           geom="line", main = "News Impact Curve") +
  theme_bw()
# printer(q, paste0(name,'_tgarch_fit_news'))

tgarch.fit.stdres <- residuals(tgarch.fit, standardize = TRUE)

q <- ggAcf(tgarch.fit.stdres) + 
  labs(title = 'S&P 500 tgarch Standardized Residuals') +
  theme_bw()
# printer(q, paste0(name,'_tgarch_fit_acf'))
q <- ggAcf(tgarch.fit.stdres^2) + 
  labs(title = 'S&P 500 tgarch Squared Standardized Residuals') +
  theme_bw()
# printer(q, paste0(name,'_tgarch_fit_acf_2'))

# QQ-Plot of standardized residuals
q <- ggplot(data = fortify(tgarch.fit.stdres), aes(sample = tgarch.fit.stdres)) +
  stat_qq() +
  qqplotr::stat_qq_line() +
  labs(title = 'QQ-Plot of standardized Residuals', y = 'sample') +
  theme_bw()
# printer(q, paste0(name,'_tgarch_fit_qq'))
q

# Perform sharpiro wilks test
# sinker(shapiro.test(coredata(tgarch.fit.stdres[1:4999,])), paste0(name,'_tgarch_fit_sharpiro'))

##### Fix the variables to filter the oos data #################################
tgarch.spec.fixed <- getspec(tgarch.fit)
setfixed(tgarch.spec.fixed) <- as.list(coef(tgarch.fit))
tgarch.forc <- ugarchforecast(tgarch.spec.fixed, data = ret, n.ahead = 1, 
                             n.roll = oos.num-1, out.sample = oos.num-1)

plot(tgarch.forc)
show(tgarch.forc)

##### Analyse the forecast #####################################################
# create a time series with the estimated and the real values
# We use the relized volatility as the squared returns
tgarch.result <- oos.abs
colnames(tgarch.result) <- c('rv')
tgarch.result$sigma <- t(tgarch.forc@forecast$sigmaFor)
tgarch.result$sigma.sq <- tgarch.result$sigma^2

# Plot the estimation
q <- ggplot(data = fortify(tgarch.result), aes(x = Index)) +
  geom_line(aes(y = rv)) +
  geom_line(aes(y = sigma), colour = 'red') +
  labs(title = 'Realized vs estimated volatility out-of-sample', x = 'Time', y = 'Volatility') +
  theme_bw() 
# printer(q, paste0(name,'_tgarch_forc_realvsestd'))

##### Test the volatility forecast #############################################
# Show the correlation between the forecast and the realized volatility
cor(tgarch.result$rv, tgarch.result$sigma.sq, 
    method = "spearman")
# sinker(cor(tgarch.result$rv, tgarch.result$sigma.sq, method = "spearman"), paste0(name,'_tgarch_forc_cor'))

# Show the accuracy of our estimate
accuracy(ts(tgarch.result$sigma.sq), ts(tgarch.result$rv))

# The model is fitted to the absolute return
# Sigma can be squared to get to the volatility

# sinker(accuracy(ts(tgarch.result$sigma.sq), ts(tgarch.result$rv)), paste0(name,'_tgarch_forc_accuracy'))
# sinker(mse(ts(tgarch.result$sigma.sq), ts(tgarch.result$rv)), paste0(name,'_tgarch_forc_mse'))
# sinker(caret::postResample(tgarch.result$sigma.sq, tgarch.result$rv), paste0(name, '_tgarch_forc_r2'))
# sinker(fpm(tgarch.forc), paste0(name, '_tgarch_forc_fpm'))

##### Analyse residuals ########################################################
# Extract residuals
tgarch.result$stdres <- rstandard(tgarch.lm)
tgarch.result$res2 <- tgarch.result$rv - tgarch.result$sigma.sq
tgarch.result$stdres2 <- tgarch.result$res2/sd(tgarch.result$res2)

# Time series of the standardized residuals
q <- ggplot(data = fortify(tgarch.result), aes(x = Index)) +
  geom_line(aes(y = stdres)) +
  labs(title = 'Standardized Residuals of tgarch Forecast', x = 'Time', 
       y = 'Standardized Residuals') +
  theme_bw()
# printer(q, paste0(name,'_tgarch_forc_stdres'))
q

# QQ-Plot of standardized residuals
q <- ggplot(data = fortify(tgarch.result), aes(sample = stdres)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = 'QQ-Plot of standardized Residuals', y = 'sample') +
  theme_bw()
# printer(q, paste0(name,'_tgarch_forc_qq'))
q

# conditional variance

# Plot conditional variance
ggplot(data = fortify(tgarch.result), aes(x = Index, y = sigma.sq)) +
  geom_line() +
  labs(title = 'Conditional variance out-of-sample', x = 'Time', y = 'Cond. variance') +
  theme_bw()

##### Perform further tests on residuals #######################################
q <- gghistogram(tgarch.result$res) +
  labs(title = 'Histogram of tgarch residuals', x = 'Residuals', y = 'Count') +
  theme_bw()
# printer(q, paste0(name,'_tgarch_forc_res_hist'))
q

# Jarque Bera test for normality
q <- jarque.bera.test(tgarch.result$res)
# sinker(jarque.bera.test(tgarch.result$res), paste0(name, '_tgarch_forc_res_jb'))
q
# Box test
q <- Box.test(x = tgarch.result$stdres, type = 'Ljung-Box', lag = 12)
# sinker(q, paste0(name,'_tgarch_forc_res_box'))
q

# Auto correlation plot
q <- ggAcf(tgarch.result$stdres) + 
  labs(title = 'ACF: tgarch Standardized Residuals') +
  theme_bw()
# printer(q, paste0(name,'_tgarch_forc_res_acf'))
q

q <- ggAcf(tgarch.result$stdres^2) + 
  labs(title = 'ACF: tgarch Squared Standardized Residuals') +
  theme_bw()
# printer(q, paste0(name,'_tgarch_res_acf_2'))
q
