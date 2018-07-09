subfolder <- 'APARCH'

##### Specify the model ########################################################
aparch.spec = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1)), 
                         mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                         distribution.model="std")

##### Fit the data to the in sample ############################################
aparch.fit <- ugarchfit(spec = aparch.spec, data = ret, out.sample = oos.num, 
                        solver = 'hybrid')  
# plot(aparch.fit)
coef(aparch.fit)
show(aparch.fit)
sinker(show(aparch.fit), folder, subfolder, name = paste0(name,'_aparch_fit'))

aparch.fit@fit$matcoef
sinker(aparch.fit@fit$matcoef, folder, subfolder, name = paste0(name,'_aparch_fit_matcoef'))
persistence(aparch.fit)
##### analyse the is fit #######################################################
sinker(signbias(aparch.fit), folder, subfolder, paste0(name,'_aparch_fit_sign'))
sinker(infocriteria(aparch.fit), folder, subfolder, paste0(name,'_aparch_fit_info'))
sinker(nyblom(aparch.fit), folder, subfolder, paste0(name,'_aparch_fit_nyblom'))
sinker(gof(aparch.fit,c(20,30,40,50)), folder, subfolder, paste0(name,'_aparch_fit_gof'))

# Make the newsimpactcurve
aparch.ni <- newsimpact(object = aparch.fit, z = NULL)

a1 <- qplot(garch.ni$zx, garch.ni$zy, ylab=garch.ni$yexpr, xlab=garch.ni$xexpr, 
           geom="line", main = paste0(ser_name," APARCH News Impact Curve")) +
  theme_bw()
printer(a1, folder, subfolder, paste0(name,'_aparch_fit_news'))


a2 <- ggAcf(aparch.fit.stdres) + 
  labs(title = paste0(ser_name,' APARCH Standardized Residuals')) +
  theme_bw()
printer(a2, folder, subfolder, paste0(name,'_aparch_fit_acf'))

a3 <- ggAcf(aparch.fit.stdres^2) + 
  labs(title = paste0(ser_name,' APARCH Squared Standardized Residuals')) +
  theme_bw()
printer(a3, folder, subfolder, paste0(name,'_aparch_fit_acf_2'))

# QQ-Plot of standardized residuals
a4 <- ggplot(data = fortify(aparch.fit.stdres), aes(sample = aparch.fit.stdres)) +
  stat_qq() +
  qqplotr::stat_qq_line() +
  labs(title = 'QQ-Plot of standardized Residuals', y = 'sample') +
  theme_bw()
printer(a4, folder, subfolder, paste0(name,'_aparch_fit_qq'))

# Perform sharpiro wilks test
aparch.fit.stdres <- residuals(aparch.fit, standardize = TRUE)
sinker(shapiro.test(coredata(aparch.fit.stdres)), folder, subfolder, paste0(name,'_aparch_fit_sharpiro'))

##### Fix the variables to filter the oos data #################################
aparch.spec.fixed <- getspec(aparch.fit)
setfixed(aparch.spec.fixed) <- as.list(coef(aparch.fit))
aparch.forc <- ugarchforecast(aparch.spec.fixed, data = ret, n.ahead = 1, 
                             n.roll = oos.num-1, out.sample = oos.num-1)

# plot(aparch.forc)
show(aparch.forc)

##### Analyse the forecast #####################################################
# create a time series with the estimated and the real values
# We use the relized volatility as the squared returns
aparch.result <- oos.abs
colnames(aparch.result) <- c('rv')
aparch.result$sigma <- t(aparch.forc@forecast$sigmaFor)
aparch.result$sigma.sq <- aparch.result$sigma^2

# Plot the estimation
a5 <- ggplot(data = fortify(aparch.result), aes(x = Index)) +
  geom_line(aes(y = rv)) +
  geom_line(aes(y = sigma), colour = 'red') +
  labs(title = paste0(ser_name,' Realized vs estimated volatility out-of-sample'), x = 'Time', y = 'Volatility') +
  theme_bw() 
printer(a5, folder, subfolder, paste0(name,'_aparch_forc_realvsestd'))

##### Test the volatility forecast #############################################
# Show the correlation between the forecast and the realized volatility
cor(aparch.result$rv, aparch.result$sigma.sq, 
    method = "spearman")
sinker(cor(aparch.result$rv, aparch.result$sigma.sq, method = "spearman"), folder, subfolder, paste0(name,'_aparch_forc_cor'))

# Show the accuracy of our estimate
accuracy(ts(aparch.result$sigma.sq), ts(aparch.result$rv))

# The model is fitted to the absolute return
# Sigma can be squared to get to the volatility

sinker(accuracy(ts(aparch.result$sigma.sq), ts(aparch.result$rv)),folder, subfolder, paste0(name,'_aparch_forc_accuracy'))
sinker(mse(ts(aparch.result$sigma.sq), ts(aparch.result$rv)), folder, subfolder, paste0(name,'_aparch_forc_mse'))
sinker(caret::postResample(aparch.result$sigma.sq, aparch.result$rv), folder, subfolder, paste0(name, '_aparch_forc_r2'))
sinker(fpm(aparch.forc), folder, subfolder, paste0(name, '_aparch_forc_fpm'))

rm(a1,a2,a3,a4,a5,subfolder)
rm(list = ls(pattern = '^aparch.'))
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
