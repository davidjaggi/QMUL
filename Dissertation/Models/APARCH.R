##### Specify the model ########################################################
aparch.spec = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1)), 
                         mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                         distribution.model="std")

##### Fit the data to the in sample ############################################
aparch.fit <- ugarchfit(spec = aparch.spec, data = ret, out.sample = oos.num, 
                        solver = 'hybrid')  
plot(aparch.fit)
coef(aparch.fit)
show(aparch.fit)
# sinker(show(aparch.fit), name = paste0(name,'_aparch_fit'))

aparch.fit@fit$matcoef
# sinker(aparch.fit@fit$matcoef, name = paste0(name,'_aparch_fit_matcoef'))
persistence(aparch.fit)
##### analyse the is fit #######################################################
# sinker(signbias(aparch.fit), paste0(name,'_aparch_fit_sign'))
# sinker(infocriteria(aparch.fit), paste0(name,'_aparch_fit_info'))
# sinker(nyblom(aparch.fit), paste0(name,'_aparch_fit_nyblom'))
# sinker(gof(aparch.fit,c(20,30,40,50)), paste0(name,'_aparch_fit_gof'))

# Make the newsimpactcurve
aparch.ni <- newsimpact(object = aparch.fit, z = NULL)

q <- qplot(aparch.ni$zx, aparch.ni$zy, ylab=aparch.ni$yexpr, xlab=aparch.ni$xexpr, 
           geom="line", main = "News Impact Curve") +
  theme_bw()
# printer(q, paste0(name,'_aparch_fit_news'))

aparch.fit.stdres <- residuals(aparch.fit, standardize = TRUE)

q <- ggAcf(aparch.fit.stdres) + 
  labs(title = 'S&P 500 aparch Standardized Residuals') +
  theme_bw()
# printer(q, paste0(name,'_aparch_fit_acf'))
q <- ggAcf(aparch.fit.stdres^2) + 
  labs(title = 'S&P 500 aparch Squared Standardized Residuals') +
  theme_bw()
# printer(q, paste0(name,'_aparch_fit_acf_2'))

# QQ-Plot of standardized residuals
q <- ggplot(data = fortify(aparch.fit.stdres), aes(sample = aparch.fit.stdres)) +
  stat_qq() +
  qqplotr::stat_qq_line() +
  labs(title = 'QQ-Plot of standardized Residuals', y = 'sample') +
  theme_bw()
# printer(q, paste0(name,'_aparch_fit_qq'))
q

# Perform sharpiro wilks test
# sinker(shapiro.test(coredata(aparch.fit.stdres[1:4999,])), paste0(name,'_aparch_fit_sharpiro'))

##### Fix the variables to filter the oos data #################################
aparch.spec.fixed <- getspec(aparch.fit)
setfixed(aparch.spec.fixed) <- as.list(coef(aparch.fit))
aparch.forc <- ugarchforecast(aparch.spec.fixed, data = ret, n.ahead = 1, 
                             n.roll = oos.num-1, out.sample = oos.num-1)

plot(aparch.forc)
show(aparch.forc)

##### Analyse the forecast #####################################################
# create a time series with the estimated and the real values
# We use the relized volatility as the squared returns
aparch.result <- oos.abs
colnames(aparch.result) <- c('rv')
aparch.result$sigma <- t(aparch.forc@forecast$sigmaFor)
aparch.result$sigma.sq <- aparch.result$sigma^2

# Plot the estimation
q <- ggplot(data = fortify(aparch.result), aes(x = Index)) +
  geom_line(aes(y = rv)) +
  geom_line(aes(y = sigma), colour = 'red') +
  labs(title = 'Realized vs estimated volatility out-of-sample', x = 'Time', y = 'Volatility') +
  theme_bw() 
# printer(q, paste0(name,'_aparch_forc_realvsestd'))

##### Test the volatility forecast #############################################
# Show the correlation between the forecast and the realized volatility
cor(aparch.result$rv, aparch.result$sigma.sq, 
    method = "spearman")
# sinker(cor(aparch.result$rv, aparch.result$sigma.sq, method = "spearman"), paste0(name,'_aparch_forc_cor'))

# Show the accuracy of our estimate
accuracy(ts(aparch.result$sigma.sq), ts(aparch.result$rv))

# The model is fitted to the absolute return
# Sigma can be squared to get to the volatility

# sinker(accuracy(ts(aparch.result$sigma.sq), ts(aparch.result$rv)), paste0(name,'_aparch_forc_accuracy'))
# sinker(mse(ts(aparch.result$sigma.sq), ts(aparch.result$rv)), paste0(name,'_aparch_forc_mse'))
# sinker(caret::postResample(aparch.result$sigma.sq, aparch.result$rv), paste0(name, '_aparch_forc_r2'))
# sinker(fpm(aparch.forc), paste0(name, '_aparch_forc_fpm'))

##### Analyse residuals ########################################################
# Extract residuals
aparch.result$stdres <- rstandard(aparch.lm)
aparch.result$res2 <- aparch.result$rv - aparch.result$sigma.sq
aparch.result$stdres2 <- aparch.result$res2/sd(aparch.result$res2)

# Time series of the standardized residuals
q <- ggplot(data = fortify(aparch.result), aes(x = Index)) +
  geom_line(aes(y = stdres)) +
  labs(title = 'Standardized Residuals of aparch Forecast', x = 'Time', 
       y = 'Standardized Residuals') +
  theme_bw()
# printer(q, paste0(name,'_aparch_forc_stdres'))
q

# QQ-Plot of standardized residuals
q <- ggplot(data = fortify(aparch.result), aes(sample = stdres)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = 'QQ-Plot of standardized Residuals', y = 'sample') +
  theme_bw()
# printer(q, paste0(name,'_aparch_forc_qq'))
q

# conditional variance

# Plot conditional variance
ggplot(data = fortify(aparch.result), aes(x = Index, y = sigma.sq)) +
  geom_line() +
  labs(title = 'Conditional variance out-of-sample', x = 'Time', y = 'Cond. variance') +
  theme_bw()

##### Perform further tests on residuals #######################################
q <- gghistogram(aparch.result$res) +
  labs(title = 'Histogram of aparch residuals', x = 'Residuals', y = 'Count') +
  theme_bw()
# printer(q, paste0(name,'_aparch_forc_res_hist'))
q

# Jarque Bera test for normality
q <- jarque.bera.test(aparch.result$res)
# sinker(jarque.bera.test(aparch.result$res), paste0(name, '_aparch_forc_res_jb'))
q
# Box test
q <- Box.test(x = aparch.result$stdres, type = 'Ljung-Box', lag = 12)
# sinker(q, paste0(name,'_aparch_forc_res_box'))
q

# Auto correlation plot
q <- ggAcf(aparch.result$stdres) + 
  labs(title = 'ACF: aparch Standardized Residuals') +
  theme_bw()
# printer(q, paste0(name,'_aparch_forc_res_acf'))
q

q <- ggAcf(aparch.result$stdres^2) + 
  labs(title = 'ACF: APARCH Squared Standardized Residuals') +
  theme_bw()
# printer(q, paste0(name,'_aparch_res_acf_2'))
q
