subfolder <- 'GARCH'

##### Specify the model ########################################################
garch.spec = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                       mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                       distribution.model="sged")

##### Fit the data to the in sample ############################################
garch.fit <- ugarchfit(spec = garch.spec, data = ret, out.sample = oos.num, 
                       solver = 'hybrid')  
# plot(garch.fit)
# coef(garch.fit)
# show(garch.fit)
sinker(show(garch.fit), folder, subfolder, name = paste0(name,'_garch_fit'))

# garch.fit@fit$matcoef
sinker(garch.fit@fit$matcoef, folder, subfolder, name = paste0(name,'_garch_fit_matcoef'))
sinker(persistence(garch.fit), folder, subfolder, name = paste0(name,'_garch_fit_pers'))
# persistence(garch.fit)
##### analyse the is fit #######################################################
sinker(signbias(garch.fit), folder, subfolder,paste0(name,'_garch_fit_sign'))
sinker(infocriteria(garch.fit),folder, subfolder, paste0(name,'_garch_fit_info'))
sinker(nyblom(garch.fit),folder, subfolder, paste0(name,'_garch_fit_nyblom'))
sinker(gof(garch.fit,c(20,30,40,50)),folder, subfolder, paste0(name,'_garch_fit_gof'))

# Make the newsimpactcurve
garch.ni <- newsimpact(object = garch.fit, z = NULL)

impact <- garch.ni

impact.all <- as.data.frame(c(garch.ni$zx))
colnames(impact.all) <- c('x')
impact.all$GARCH <- as.data.frame(c(garch.ni$zy))

g1 <- qplot(garch.ni$zx, garch.ni$zy, ylab = garch.ni$yexpr, 
            xlab = garch.ni$xexpr, 
      geom="line", main = paste0(ser_name,": GARCH news impact curve")) +
      theme_bw()
printer(g1,folder, subfolder,paste0(name,'_garch_fit_news'))

garch.fit.stdres <- residuals(garch.fit, standardize = TRUE)
g2 <- ggAcf(garch.fit.stdres) + 
      labs(title = paste0(ser_name,': GARCH residuals')) +
      theme_bw()
printer(g2, folder, subfolder,paste0(name,'_garch_fit_acf'))

g3 <- ggAcf(garch.fit.stdres^2) + 
  labs(title = paste0(ser_name,': GARCH squared residuals')) +
  theme_bw()
printer(g3, folder, subfolder,paste0(name,'_garch_fit_acf_2'))

# QQ-Plot of standardised residuals
g4 <- ggplot(data = fortify(garch.fit.stdres), aes(sample = garch.fit.stdres)) +
  stat_qq() +
  qqplotr::stat_qq_line() +
  labs(title = paste0(ser_name,': QQ-Plot of the GARCH residuals'), y = 'Sample', x = 'Theoretical') +
  theme_bw()
printer(g4, folder, subfolder,paste0(name,'_garch_fit_qq'))

# Perform sharpiro wilks test
sinker(shapiro.test(coredata(garch.fit.stdres)), folder, subfolder,paste0(name,'_garch_fit_sharpiro'))

##### Fix the variables to filter the oos data #################################
garch.spec.fixed <- getspec(garch.fit)
setfixed(garch.spec.fixed) <- as.list(coef(garch.fit))
garch.forc <- ugarchforecast(garch.spec.fixed, data = ret, n.ahead = 1, 
                            n.roll = oos.num-1, out.sample = oos.num-1)

# plot(garch.forc)
# show(garch.forc)

##### Analyse the forecast #####################################################
# create a time series with the estimated and the real values
# We use the relized volatility as the squared returns
garch.result <- oos.abs
colnames(garch.result) <- c('rv')
garch.result$sigma <- t(garch.forc@forecast$sigmaFor)
garch.result$sigma.sq <- garch.result$sigma^2
oos.all$GARCH <- garch.result$sigma

# Plot the estimation
g5 <- ggplot(data = fortify(garch.result), aes(x = Index)) +
  geom_line(aes(y = rv)) +
  geom_line(aes(y = sigma), colour = 'red') +
  labs(title = paste0(ser_name,': Realized vs estimated OOS'), x = 'Time', y = 'Volatility') +
  theme_bw() 
printer(g5, folder, subfolder,paste0(name,'_garch_forc_rve'))

g5.1 <- ggplot(data = fortify(garch.result), aes(x = as.Date(Index))) +
  geom_line(aes(y = rv)) +
  geom_line(aes(y = sigma), colour = 'red') +
  scale_x_date(limits = c(as.Date('2018-01-01', format = '%Y-%m-%d'), as.Date('2018-06-31', format = '%Y-%m-%d'))) +
  labs(title = paste0(ser_name,': Realized vs estimated OOS zoom'), x = 'Time', y = 'Volatility') +
  theme_bw() 
printer(g5.1, folder, subfolder,paste0(name,'_garch_forc_rve_zoom'))
##### Test the volatility forecast #############################################
# Show the correlation between the forecast and the realized volatility
# cor(garch.result$rv, garch.result$sigma.sq, 
#     method = "spearman")
sinker(cor(garch.result$rv, garch.result$sigma.sq, method = "spearman"), folder, subfolder,
       paste0(name,'_garch_forc_cor'))

# Show the accuracy of our estimate
# accuracy(ts(garch.result$sigma.sq), ts(garch.result$rv))

# The model is fitted to the absolute return
# Sigma can be squared to get to the volatility
sinker(accuracy(ts(garch.result$sigma.sq), ts(garch.result$rv)), folder, subfolder,paste0(name,'_garch_forc_accuracy'))
sinker(rmse(ts(garch.result$sigma.sq), ts(garch.result$rv)), folder, subfolder,paste0(name,'_garch_forc_rmse'))
sinker(caret::postResample(garch.result$sigma.sq, garch.result$rv), folder, subfolder,paste0(name,'_garch_forc_r2'))
sinker(fpm(garch.forc), folder, subfolder,paste0(name, '_garch_forc_fpm'))
# QLIKE(sigmafc = garch.result$sigma.sq, garch.result$rv)
# QLIKE(sigmafc = garch.result$sigma, garch.result$rv)

##### Save residuals ###########################################################
fit_res_garch <- residuals(garch.fit)
forc_res_garch <- garch.result$rv - garch.result$sigma.sq


##### Delete series ############################################################
rm(g1,g2,g3,g4,g5,g5.1,subfolder)
rm(list = ls(pattern = 'garch.'))

##### Analyse residuals ########################################################
# # Extract residuals
# garch.result$stdres <- rstandard(garch.lm)
# garch.result$res2 <- garch.result$rv - garch.result$sigma.sq
# garch.result$stdres2 <- garch.result$res2/sd(garch.result$res2)
# 
# # Time series of the standardised residuals
# q <- ggplot(data = fortify(garch.result), aes(x = Index)) +
#   geom_line(aes(y = stdres)) +
#   labs(title = 'standardised Residuals of GARCH Forecast', x = 'Time', 
#        y = 'standardised Residuals') +
#   theme_bw()
# # printer(q, paste0(name,'_garch_forc_stdres'))
# q
# 
# # QQ-Plot of standardised residuals
# q <- ggplot(data = fortify(garch.result), aes(sample = stdres)) +
#   stat_qq() +
#   stat_qq_line() +
#   labs(title = 'QQ-Plot of standardised Residuals', y = 'sample') +
#   theme_bw()
# # printer(q, paste0(name,'_garch_forc_qq'))
# q
# 
# # conditional variance
# 
# # Plot conditional variance
# ggplot(data = fortify(garch.result), aes(x = Index, y = sigma.sq)) +
#   geom_line() +
#   labs(title = 'Conditional variance out-of-sample', x = 'Time', y = 'Cond. variance') +
#   theme_bw()
# 
# ##### Perform further tests on residuals #######################################
# q <- gghistogram(garch.result$res) +
#   labs(title = 'Histogram of GARCH residuals', x = 'Residuals', y = 'Count') +
#   theme_bw()
# # printer(q, paste0(name,'_garch_forc_res_hist'))
# q
# 
# # Jarque Bera test for normality
# q <- jarque.bera.test(garch.result$res)
# # sinker(jarque.bera.test(garch.result$res), paste0(name, '_garch_forc_res_jb'))
# q
# # Box test
# q <- Box.test(x = garch.result$stdres, type = 'Ljung-Box', lag = 12)
# # sinker(q, paste0(name,'_garch_forc_res_box'))
# q
# 
# # Auto correlation plot
# q <- ggAcf(garch.result$stdres) + 
#   labs(title = 'ACF: GARCH standardised Residuals') +
#   theme_bw()
# # printer(q, paste0(name,'_garch_forc_res_acf'))
# q
# 
# q <- ggAcf(garch.result$stdres^2) + 
#   labs(title = 'ACF: GARCH Squared standardised Residuals') +
#   theme_bw()
# # printer(q, paste0(name,'_garch_res_acf_2'))
# q
