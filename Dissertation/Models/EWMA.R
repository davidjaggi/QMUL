##### Specify the model ########################################################
ewma.spec = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)), 
                       mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                       distribution.model="norm", fixed.pars=list(omega=0))

##### Fit the data to the in sample ############################################
ewma.fit <- ugarchfit(spec = ewma.spec, data = is)  
plot(ewma.fit)
coef(ewma.fit)
confint(ewma.fit)
show(ewma.fit)

ewma.fit@fit$matcoef
persistence(ewma.fit)
##### Fix the variables to filter the oos data #################################
ewma.spec.fixed <- getspec(ewma.fit)
setfixed(ewma.spec.fixed) <- as.list(coef(ewma.fit))
ewma.filt <- ugarchroll(spec = ewma.spec.fixed, 
                               data = oos, 
                               cluster = cl)

plot(ewma.filt)

##### Extract varaibles ########################################################
ewma.sigma <- sigma(ewma.filt)
ewma.sigma.sq <- ewma.sigma^2

##### Analyse the forecast #####################################################
# create a time series with the estimated and the real values
# We use the relized volatility as the squared returns
ewma.result <- oos.rv
colnames(ewma.result) <- c('RV')
ewma.result$Sigma <- sigma(ewma.filt)
ewma.result$Sigma.sq <- ewma.result$Sigma^2

# Plot the estimation
ggplot(data = fortify(ewma.result), aes(x = Index)) +
  geom_line(aes(y = RV)) +
  geom_line(aes(y = Sigma.sq), colour = 'red') +
  labs(title = 'Realized vs estimated volatility', x = 'Time', y = 'Volatility') +
  theme_bw()

##### Test the volatility forecast #############################################
# Show the correlation between the forecast and the realized volatility
cor(ewma.result$RV, ewma.result$Sigma.sq, 
    method = "spearman")

# Show the accuracy of our estimate
accuracy(ts(ewma.result$RV), ts(ewma.result$Sigma.sq))

# make a regression
ewma.lm <- lm(formula = oos.sq ~ ewma.sigma.sq)
plot(ewma.lm)
summary(ewma.lm)
accuracy(ewma.lm)
# xtable(accuracy(ewma.lm))
error_MAD(ewma.result$OOS, ewma.result$Sigma)



ggplot(fortify(ewma.result), aes(x = Index)) +
  # geom_line(aes(y = abs(OOS))) +
  # geom_line(aes(y = Sigma)) +
  geom_line(aes(y = Resid)) +
  theme_bw() +


plot(ewma.filt@filter$z)
infocriteria(ewma.filt)
plot(sigma(ewma.filt))

# conditional variance
ewma.sigma <- sigma(ewma.filt)

# Plot conditional variance
ggplot(data = fortify(ewma.sigma), aes(x = Index, y = ewma.sigma)) +
  geom_line() +
  labs(title = 'Conditional variance out-of-sample', x = 'Time', y = 'Cond. variance') +
  theme_bw()

ewma.resid <- residuals(ewma.filt)
ewma.stresid <- residuals(ewma.filt, standardize = TRUE)

# calculate approximate z value
ewma.resid/ewma.sigma

##### 
ggplot(data = fortify(oos),aes(x = Index, y = oos)) + geom_line()

plot(fitted(ewma.filt))
report(ewma.filt)
