##### Specify the model ########################################################
garch.spec = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                       mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                       distribution.model="std")

##### Fit the data to the in sample ############################################
garch.fit <- ugarchfit(spec = garch.spec, data = ret, out.sample = oos.num)  
plot(garch.fit)
coef(garch.fit)
confint(garch.fit)
show(garch.fit)
# sinker(show(garch.fit), name = paste0(name,'_garch_fit'))

garch.fit@fit$matcoef
# sinker(garch.fit@fit$matcoef, name = paste0(name,'_garch_fit_matcoef'))
persistence(garch.fit)
##### analyse the is fit #######################################################
# sinker(signbias(garch.fit), paste0(name,'_garch_fit_sign'))
# sinker(infocriteria(garch.fit), paste0(name,'_garch_fit_info'))
# sinker(nyblom(garch.fit), paste0(name,'_garch_fit_nyblom'))
# sinker(gof(garch.fit,c(20,30,40,50)), paste0(name,'_garch_fit_gof'))

# Make the newsimpactcurve
garch.ni <- newsimpact(object = garch.fit, z = NULL)

q <- qplot(garch.ni$zx, garch.ni$zy, ylab=garch.ni$yexpr, xlab=garch.ni$xexpr, 
      geom="line", main = "News Impact Curve") +
      theme_bw()
# printer(q, paste0(name,'_garch_fit_news'))

garch.fit.stdres <- residuals(garch.fit, standardize = TRUE)
q <- ggAcf(garch.fit.stdres) + 
      labs(title = 'S&P 500 GARCH Standardized Residuals') +
      theme_bw()
# printer(q, paste0(name,'_garch_fit_acf'))
q <- ggAcf(garch.fit.stdres^2) + 
  labs(title = 'S&P 500 GARCH Squared Standardized Residuals') +
  theme_bw()
# printer(q, paste0(name,'_garch_fit_acf_2'))

# QQ-Plot of standardized residuals
q <- ggplot(data = fortify(garch.fit.stdres), aes(sample = garch.fit.stdres)) +
  stat_qq() +
  qqplotr::stat_qq_line() +
  labs(title = 'QQ-Plot of standardized Residuals', y = 'sample') +
  theme_bw()
# printer(q, paste0(name,'_garch_fit_qq'))
q

# Perform sharpiro wilks test
# sinker(shapiro.test(coredata(garch.fit.stdres[1:4999,])), paste0(name,'_garch_fit_sharpiro'))

plot(garch.fit, which = 8)
##### Fix the variables to filter the oos data #################################
garch.spec.fixed <- getspec(garch.fit)
setfixed(garch.spec.fixed) <- as.list(coef(garch.fit))
garch.forc <- ugarchforecast(garch.spec.fixed, data = ret, n.ahead = 1, 
                            n.roll = oos.num-1, out.sample = oos.num-1)

plot(garch.forc)
show(garch.forc)

##### Analyse the forecast #####################################################
# create a time series with the estimated and the real values
# We use the relized volatility as the squared returns
garch.result <- oos.abs
colnames(garch.result) <- c('rv')
garch.result$sigma <- t(garch.forc@forecast$sigmaFor)
garch.result$sigma.sq <- garch.result$sigma^2

# Plot the estimation
q <- ggplot(data = fortify(garch.result), aes(x = Index)) +
  geom_line(aes(y = rv)) +
  geom_line(aes(y = sigma), colour = 'red') +
  labs(title = 'Realized vs estimated volatility out-of-sample', x = 'Time', y = 'Volatility') +
  theme_bw() 
# printer(q, paste0(name,'_garch_forc_realvsestd'))

##### Test the volatility forecast #############################################
# Show the correlation between the forecast and the realized volatility
cor(garch.result$rv, garch.result$sigma.sq, 
    method = "spearman")
# sinker(cor(garch.result$rv, garch.result$sigma.sq, method = "spearman"), paste0(name,'_garch_forc_cor'))

# Show the accuracy of our estimate
accuracy(ts(garch.result$sigma.sq), ts(garch.result$rv))

# The model is fitted to the absolute return
# Sigma can be squared to get to the volatility

# sinker(accuracy(ts(garch.result$sigma.sq), ts(garch.result$rv)), paste0(name,'_garch_forc_accuracy'))
# sinker(mse(ts(garch.result$sigma.sq), ts(garch.result$rv)), paste0(name,'_garch_forc_mse'))
##### Analyse the residuals ####################################################
garch.lm <- lm(formula = garch.result$rv ~ 0 + offset(garch.result$sigma.sq))
plot(garch.lm)
summary(garch.lm)
# sinker(summary(garch.lm), name = paste0(name,'_garch_forc_lm'))
accuracy(garch.lm)
caret::postResample(garch.result$sigma.sq, garch.result$rv)


##### Analyse residuals ########################################################
# Extract residuals
garch.result$stdres <- rstandard(garch.lm)
garch.result$res <- residuals(garch.lm)

# Time series of the standardized residuals
q <- ggplot(data = fortify(garch.result), aes(x = Index)) +
  geom_line(aes(y = stdres)) +
  labs(title = 'Standardized Residuals of GARCH Forecast', x = 'Time', 
       y = 'Standardized Residuals') +
  theme_bw()
# printer(q, paste0(name,'_garch_forc_stdres'))
q

# QQ-Plot of standardized residuals
q <- ggplot(data = fortify(garch.result), aes(sample = stdres)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = 'QQ-Plot of standardized Residuals', y = 'sample') +
  theme_bw()
# printer(q, paste0(name,'_garch_forc_qq'))
q

# conditional variance

# Plot conditional variance
ggplot(data = fortify(garch.result), aes(x = Index, y = sigma.sq)) +
  geom_line() +
  labs(title = 'Conditional variance out-of-sample', x = 'Time', y = 'Cond. variance') +
  theme_bw()

##### Perform further tests on residuals #######################################
q <- gghistogram(garch.result$res) +
  labs(title = 'Histogram of GARCH residuals', x = 'Residuals', y = 'Count') +
  theme_bw()
# printer(q, paste0(name,'_garch_forc_res_hist'))
q

# Jarque Bera test for normality
q <- jarque.bera.test(garch.result$res)
# sinker(jarque.bera.test(garch.result$res), paste0(name, '_garch_forc_res_jb'))
q
# Box test
q <- Box.test(x = garch.result$stdres, type = 'Ljung-Box', lag = 12)
# sinker(q, paste0(name,'_garch_forc_res_box'))
q

# Auto correlation plot
q <- ggAcf(garch.result$stdres) + 
  labs(title = 'ACF: GARCH Standardized Residuals') +
  theme_bw()
# printer(q, paste0(name,'_garch_forc_res_acf'))
q

q <- ggAcf(garch.result$StdRes^2) + 
  labs(title = 'ACF: GARCH Squared Standardized Residuals') +
  theme_bw()
# printer(q, paste0(name,'_garch_res_acf_2'))
q
