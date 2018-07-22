subfolder <- 'EGARCH'

##### Specify the model ########################################################
egarch.spec = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)), 
                        mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                        distribution.model="std")

##### Fit the data to the in sample ############################################
egarch.fit <- ugarchfit(spec = egarch.spec, data = ret, out.sample = oos.num,
                        solver = 'hybrid')  
# plot(egarch.fit)
# coef(egarch.fit)
# show(egarch.fit)
sinker(show(egarch.fit), folder, subfolder, name = paste0(name,'_egarch_fit'))

# egarch.fit@fit$matcoef
sinker(egarch.fit@fit$matcoef, folder, subfolder, name = paste0(name,'_egarch_fit_matcoef'))
sinker(persistence(egarch.fit), folder, subfolder, name = paste0(name,'_egarch_fit_pers'))
# persistence(egarch.fit)
##### analyse the is fit #######################################################
sinker(signbias(egarch.fit), folder, subfolder, paste0(name,'_egarch_fit_sign'))
sinker(infocriteria(egarch.fit), folder, subfolder, paste0(name,'_egarch_fit_info'))
sinker(nyblom(egarch.fit), folder, subfolder, paste0(name,'_egarch_fit_nyblom'))
sinker(gof(egarch.fit,c(20,30,40,50)), folder, subfolder, paste0(name,'_egarch_fit_gof'))

# Make the newsimpactcurve
egarch.ni <- newsimpact(object = egarch.fit, z = NULL)

e1 <- qplot(egarch.ni$zx, egarch.ni$zy, ylab = egarch.ni$yexpr, xlab = egarch.ni$xexpr, 
           geom="line", main = paste0(ser_name," EGARCH News Impact Curve")) +
  theme_bw()
printer(e1, folder, subfolder, paste0(name,'_egarch_fit_news'))

egarch.fit.stdres <- residuals(egarch.fit, standardize = TRUE)
e2 <- ggAcf(egarch.fit.stdres) + 
  labs(title = paste0(ser_name,' EGARCH Standardized Residuals')) +
  theme_bw()
printer(e2, folder, subfolder, paste0(name,'_egarch_fit_acf'))

e3 <- ggAcf(egarch.fit.stdres^2) + 
  labs(title = paste0(ser_name,' EGARCH Squared Standardized Residuals')) +
  theme_bw()
printer(e3, folder, subfolder, paste0(name,'_egarch_fit_acf_2'))

# QQ-Plot of standardized residuals
e4 <- ggplot(data = fortify(egarch.fit.stdres), aes(sample = egarch.fit.stdres)) +
  stat_qq() +
  qqplotr::stat_qq_line() +
  labs(title = 'QQ-Plot of standardized Residuals', y = 'sample') +
  theme_bw()
printer(e4, folder, subfolder, paste0(name,'_egarch_fit_qq'))

# Perform sharpiro wilks test
sinker(shapiro.test(coredata(egarch.fit.stdres)), folder, subfolder, paste0(name,'_egarch_fit_sharpiro'))

##### Fix the variables to filter the oos data #################################
egarch.spec.fixed <- getspec(egarch.fit)
setfixed(egarch.spec.fixed) <- as.list(coef(egarch.fit))
egarch.forc <- ugarchforecast(egarch.spec.fixed, data = ret, n.ahead = 1, 
                             n.roll = oos.num-1, out.sample = oos.num-1)

# plot(egarch.forc)
show(egarch.forc)

##### Analyse the forecast #####################################################
# create a time series with the estimated and the real values
# We use the relized volatility as the squared returns
egarch.result <- oos.abs
colnames(egarch.result) <- c('rv')
egarch.result$sigma <- t(egarch.forc@forecast$sigmaFor)
egarch.result$sigma.sq <- egarch.result$sigma^2

# Plot the estimation
e5 <- ggplot(data = fortify(egarch.result), aes(x = Index)) +
  geom_line(aes(y = rv)) +
  geom_line(aes(y = sigma), colour = 'red') +
  labs(title = paste0(ser_name,' Realized vs estimated volatility out-of-sample'), x = 'Time', y = 'Volatility') +
  theme_bw() 
printer(e5, folder, subfolder, paste0(name,'_egarch_forc_rve'))

e5.1 <- ggplot(data = fortify(egarch.result), aes(x = as.Date(Index))) +
  geom_line(aes(y = rv)) +
  geom_line(aes(y = sigma), colour = 'red') +
  scale_x_date(limits = c(as.Date('2018-01-01', format = '%Y-%m-%d'), as.Date('2018-06-31', format = '%Y-%m-%d'))) +
  labs(title = paste0(ser_name,' Realized vs estimated volatility out-of-sample zoomed in'), x = 'Time', y = 'Volatility') +
  theme_bw() 
printer(e5.1, folder, subfolder,paste0(name,'_egarch_forc_rve_zoom'))

##### Test the volatility forecast #############################################
# Show the correlation between the forecast and the realized volatility
cor(egarch.result$rv, egarch.result$sigma.sq, 
    method = "spearman")
sinker(cor(egarch.result$rv, egarch.result$sigma.sq, method = "spearman"), folder, subfolder, paste0(name,'_egarch_forc_cor'))

# Show the accuracy of our estimate
accuracy(ts(egarch.result$sigma.sq), ts(egarch.result$rv))

# The model is fitted to the absolute return
# Sigma can be squared to get to the volatility

sinker(accuracy(ts(egarch.result$sigma.sq), ts(egarch.result$rv)), folder, subfolder, paste0(name,'_egarch_forc_accuracy'))
sinker(rmse(ts(egarch.result$sigma.sq), ts(egarch.result$rv)), folder, subfolder, paste0(name,'_egarch_forc_rmse'))
sinker(caret::postResample(egarch.result$sigma.sq, egarch.result$rv), folder, subfolder, paste0(name, '_egarch_forc_r2'))
sinker(fpm(egarch.forc), folder, subfolder, paste0(name, '_egarch_forc_fpm'))

rm(e1,e2,e3,e4,e5, e5.1,subfolder)
rm(list = ls(pattern = '^egarch.'))
# ##### Analyse the residuals ####################################################
# egarch.lm <- lm(formula = egarch.result$rv ~ 0 + offset(egarch.result$sigma.sq))
# plot(egarch.lm)
# summary(egarch.lm)
# # sinker(summary(egarch.lm), name = paste0(name,'_egarch_forc_lm'))
# accuracy(egarch.lm)
# 
# 
# 
# ##### Analyse residuals ########################################################
# # Extract residuals
# egarch.result$stdres <- rstandard(egarch.lm)
# egarch.result$res <- residuals(egarch.lm)
# 
# # Time series of the standardized residuals
# q <- ggplot(data = fortify(egarch.result), aes(x = Index)) +
#   geom_line(aes(y = stdres)) +
#   labs(title = 'Standardized Residuals of egarch Forecast', x = 'Time', 
#        y = 'Standardized Residuals') +
#   theme_bw()
# # printer(q, paste0(name,'_egarch_forc_stdres'))
# q
# 
# # QQ-Plot of standardized residuals
# q <- ggplot(data = fortify(egarch.result), aes(sample = stdres)) +
#   stat_qq() +
#   stat_qq_line() +
#   labs(title = 'QQ-Plot of standardized Residuals', y = 'sample') +
#   theme_bw()
# # printer(q, paste0(name,'_egarch_forc_qq'))
# q
# 
# # conditional variance
# 
# # Plot conditional variance
# ggplot(data = fortify(egarch.result), aes(x = Index, y = sigma.sq)) +
#   geom_line() +
#   labs(title = 'Conditional variance out-of-sample', x = 'Time', y = 'Cond. variance') +
#   theme_bw()
# 
# ##### Perform further tests on residuals #######################################
# q <- gghistogram(egarch.result$res) +
#   labs(title = 'Histogram of egarch residuals', x = 'Residuals', y = 'Count') +
#   theme_bw()
# # printer(q, paste0(name,'_egarch_forc_res_hist'))
# q
# 
# # Jarque Bera test for normality
# q <- jarque.bera.test(egarch.result$res)
# # sinker(jarque.bera.test(egarch.result$res), paste0(name, '_egarch_forc_res_jb'))
# q
# # Box test
# q <- Box.test(x = egarch.result$stdres, type = 'Ljung-Box', lag = 12)
# # sinker(q, paste0(name,'_egarch_forc_res_box'))
# q
# 
# # Auto correlation plot
# q <- ggAcf(egarch.result$stdres) + 
#   labs(title = 'ACF: egarch Standardized Residuals') +
#   theme_bw()
# # printer(q, paste0(name,'_egarch_forc_res_acf'))
# q
# 
# q <- ggAcf(egarch.result$stdres^2) + 
#   labs(title = 'ACF: egarch Squared Standardized Residuals') +
#   theme_bw()
# # printer(q, paste0(name,'_egarch_res_acf_2'))
# q
