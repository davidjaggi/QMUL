subfolder <- 'NGARCH'

##### Specify the model ########################################################
ngarch.spec = ugarchspec(variance.model=list(model="fGARCH", garchOrder=c(1,1), 
                                             submodel = 'NGARCH'), 
                         mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                         distribution.model="sged")

##### Fit the data to the in sample ############################################
ngarch.fit <- ugarchfit(spec = ngarch.spec, data = ret, out.sample = oos.num, 
                       solver = 'hybrid')  
# plot(ngarch.fit)
# coef(ngarch.fit)
# show(ngarch.fit)
sinker(show(ngarch.fit), folder, subfolder, name = paste0(name,'_ngarch_fit'))

# ngarch.fit@fit$matcoef
sinker(ngarch.fit@fit$matcoef, folder, subfolder, name = paste0(name,'_ngarch_fit_matcoef'))
sinker(persistence(ngarch.fit), folder, subfolder, name = paste0(name,'_ngarch_fit_pers'))
# persistence(ngarch.fit)
##### analyse the is fit #######################################################
sinker(signbias(ngarch.fit), folder, subfolder, paste0(name,'_ngarch_fit_sign'))
sinker(infocriteria(ngarch.fit), folder, subfolder, paste0(name,'_ngarch_fit_info'))
sinker(nyblom(ngarch.fit), folder, subfolder, paste0(name,'_ngarch_fit_nyblom'))
sinker(gof(ngarch.fit,c(20,30,40,50)), folder, subfolder, paste0(name,'_ngarch_fit_gof'))

# Make the newsimpactcurve
ngarch.ni <- newsimpact(object = ngarch.fit, z = NULL)
impact.all$NGARCH <- as.data.frame(c(ngarch.ni$zy))

n1 <- qplot(ngarch.ni$zx, ngarch.ni$zy, ylab = ngarch.ni$yexpr, xlab = ngarch.ni$xexpr, 
           geom="line", main = paste0(ser_name," NGARCH News Impact Curve")) +
  theme_bw()
printer(n1, folder, subfolder, paste0(name,'_ngarch_fit_news'))

ngarch.fit.stdres <- residuals(ngarch.fit, standardize = TRUE)
n2 <- ggAcf(ngarch.fit.stdres) + 
  labs(title = paste0(ser_name,' NGARCH Standardized Residuals')) +
  theme_bw()
printer(n2, folder, subfolder, paste0(name,'_ngarch_fit_acf'))

n3 <- ggAcf(ngarch.fit.stdres^2) + 
  labs(title = paste0(ser_name,' NGARCH Squared Standardized Residuals')) +
  theme_bw()
printer(n3, folder, subfolder, paste0(name,'_ngarch_fit_acf_2'))

# QQ-Plot of standardized residuals
n4 <- ggplot(data = fortify(ngarch.fit.stdres), aes(sample = ngarch.fit.stdres)) +
  stat_qq() +
  qqplotr::stat_qq_line() +
  labs(title = 'QQ-Plot of standardized Residuals', y = 'sample') +
  theme_bw()
printer(n4, folder, subfolder, paste0(name,'_ngarch_fit_qq'))

# Perform sharpiro wilks test

sinker(shapiro.test(coredata(ngarch.fit.stdres)), folder, subfolder, paste0(name,'_ngarch_fit_sharpiro'))

##### Fix the variables to filter the oos data #################################
ngarch.spec.fixed <- getspec(ngarch.fit)
setfixed(ngarch.spec.fixed) <- as.list(coef(ngarch.fit))
ngarch.forc <- ugarchforecast(ngarch.spec.fixed, data = ret, n.ahead = 1, 
                             n.roll = oos.num-1, out.sample = oos.num-1)

# plot(ngarch.forc)
# show(ngarch.forc)

##### Analyse the forecast #####################################################
# create a time series with the estimated and the real values
# We use the relized volatility as the squared returns
ngarch.result <- oos.abs
colnames(ngarch.result) <- c('rv')
ngarch.result$sigma <- t(ngarch.forc@forecast$sigmaFor)
ngarch.result$sigma.sq <- ngarch.result$sigma^2
oos.all$NGARCH <- ngarch.result$sigma

# Plot the estimation
n5 <- ggplot(data = fortify(ngarch.result), aes(x = Index)) +
  geom_line(aes(y = rv)) +
  geom_line(aes(y = sigma), colour = 'red') +
  labs(title = paste0(ser_name,' Realized vs estimated volatility out-of-sample'), x = 'Time', y = 'Volatility') +
  theme_bw() 
printer(n5, folder, subfolder, paste0(name,'_ngarch_forc_rve'))

n5.1 <- ggplot(data = fortify(ngarch.result), aes(x = as.Date(Index))) +
  geom_line(aes(y = rv)) +
  geom_line(aes(y = sigma), colour = 'red') +
  scale_x_date(limits = c(as.Date('2018-01-01', format = '%Y-%m-%d'), as.Date('2018-06-31', format = '%Y-%m-%d'))) +
  labs(title = paste0(ser_name,' Realized vs estimated volatility out-of-sample zoomed in'), x = 'Time', y = 'Volatility') +
  theme_bw() 
printer(n5.1, folder, subfolder,paste0(name,'_ngarch_forc_rve_zoom'))

##### Test the volatility forecast #############################################
# Show the correlation between the forecast and the realized volatility
# cor(ngarch.result$rv, ngarch.result$sigma.sq, 
#     method = "spearman")
# sinker(cor(ngarch.result$rv, ngarch.result$sigma.sq, method = "spearman"), paste0(name,'_ngarch_forc_cor'))

# Show the accuracy of our estimate
# accuracy(ts(ngarch.result$sigma.sq), ts(ngarch.result$rv))

# The model is fitted to the absolute return
# Sigma can be squared to get to the volatility

sinker(accuracy(ts(ngarch.result$sigma.sq), ts(ngarch.result$rv)), folder, subfolder, paste0(name,'_ngarch_forc_accuracy'))
sinker(rmse(ts(ngarch.result$sigma.sq), ts(ngarch.result$rv)), folder, subfolder, paste0(name,'_ngarch_forc_rmse'))
sinker(caret::postResample(ngarch.result$sigma.sq, ngarch.result$rv), folder, subfolder, paste0(name, '_ngarch_forc_r2'))
sinker(fpm(ngarch.forc), folder, subfolder, paste0(name, '_ngarch_forc_fpm'))

rm(n1,n2,n3,n4,n5, n5.1, subfolder)
rm(list = ls(pattern = '^ngarch.'))
# ##### Analyse residuals ########################################################
# # Extract residuals
# ngarch.result$stdres <- rstandard(ngarch.lm)
# ngarch.result$res2 <- ngarch.result$rv - ngarch.result$sigma.sq
# ngarch.result$stdres2 <- ngarch.result$res2/sd(ngarch.result$res2)
# 
# # Time series of the standardized residuals
# q <- ggplot(data = fortify(ngarch.result), aes(x = Index)) +
#   geom_line(aes(y = stdres)) +
#   labs(title = 'Standardized Residuals of NGARCH Forecast', x = 'Time', 
#        y = 'Standardized Residuals') +
#   theme_bw()
# # printer(q, paste0(name,'_ngarch_forc_stdres'))
# q
# 
# # QQ-Plot of standardized residuals
# q <- ggplot(data = fortify(ngarch.result), aes(sample = stdres)) +
#   stat_qq() +
#   stat_qq_line() +
#   labs(title = 'QQ-Plot of standardized Residuals', y = 'sample') +
#   theme_bw()
# # printer(q, paste0(name,'_ngarch_forc_qq'))
# q
# 
# # conditional variance
# 
# # Plot conditional variance
# ggplot(data = fortify(ngarch.result), aes(x = Index, y = sigma.sq)) +
#   geom_line() +
#   labs(title = 'Conditional variance out-of-sample', x = 'Time', y = 'Cond. variance') +
#   theme_bw()
# 
# ##### Perform further tests on residuals #######################################
# q <- gghistogram(ngarch.result$res) +
#   labs(title = 'Histogram of ngarch residuals', x = 'Residuals', y = 'Count') +
#   theme_bw()
# # printer(q, paste0(name,'_ngarch_forc_res_hist'))
# q
# 
# # Jarque Bera test for normality
# q <- jarque.bera.test(ngarch.result$res)
# # sinker(jarque.bera.test(ngarch.result$res), paste0(name, '_ngarch_forc_res_jb'))
# q
# # Box test
# q <- Box.test(x = ngarch.result$stdres, type = 'Ljung-Box', lag = 12)
# # sinker(q, paste0(name,'_ngarch_forc_res_box'))
# q
# 
# # Auto correlation plot
# q <- ggAcf(ngarch.result$stdres) + 
#   labs(title = 'ACF: ngarch Standardized Residuals') +
#   theme_bw()
# # printer(q, paste0(name,'_ngarch_forc_res_acf'))
# q
# 
# q <- ggAcf(ngarch.result$stdres^2) + 
#   labs(title = 'ACF: ngarch Squared Standardized Residuals') +
#   theme_bw()
# # printer(q, paste0(name,'_nngarch_res_acf_2'))
# q
# 
