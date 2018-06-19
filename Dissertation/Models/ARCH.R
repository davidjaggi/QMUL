##### Specify the model ########################################################
arch.spec = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(0,1)), 
                       mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                       distribution.model="norm")

##### Fit the data to the in sample ############################################
arch.fit <- ugarchfit(spec = arch.spec, data = is)  
plot(arch.fit)
coef(arch.fit)
confint(arch.fit)
show(arch.fit)

arch.fit@fit$matcoef
persistence(arch.fit)
##### Fix the variables to filter the oos data #################################
arch.spec.fixed <- getspec(arch.fit)
setfixed(arch.spec.fixed) <- as.list(coef(arch.fit))
arch.filt <- ugarchfilter(spec = arch.spec.fixed, 
                        data = oos, 
                        cluster = cl)

plot(arch.filt)

##### Extract varaibles ########################################################
arch.sigma <- sigma(arch.filt)
arch.sigma.sq <- arch.sigma^2

##### Analyse the forecast #####################################################
# create a time series with the estimated and the real values
# We use the relized volatility as the squared returns
arch.result <- oos.rv
colnames(arch.result) <- c('RV')
arch.result$Sigma <- sigma(arch.filt)
arch.result$Sigma.sq <- arch.result$Sigma^2

# Plot the estimation
ggplot(data = fortify(arch.result), aes(x = Index)) +
  geom_line(aes(y = RV)) +
  geom_line(aes(y = Sigma.sq), colour = 'red') +
  labs(title = 'Realized vs estimated volatility', x = 'Time', y = 'Volatility') +
  theme_bw()

##### Test the volatility forecast #############################################
# Show the correlation between the forecast and the realized volatility
cor(arch.result$RV, arch.result$Sigma.sq, 
    method = "spearman")

# Show the accuracy of our estimate
accuracy(ts(arch.result$RV), ts(arch.result$Sigma.sq))

# make a regression
arch.lm <- lm(formula = oos.sq ~ arch.sigma.sq)
plot(arch.lm)
summary(arch.lm)
accuracy(arch.lm)

ggplot(fortify(arch.result), aes(x = Index)) +
  # geom_line(aes(y = abs(OOS))) +
  # geom_line(aes(y = Sigma)) +
  geom_line(aes(y = Resid)) +
  theme_bw() +
  
infocriteria(arch.filt)

# conditional variance
arch.sigma <- sigma(arch.filt)

# Plot conditional variance
ggplot(data = fortify(arch.sigma), aes(x = Index, y = arch.sigma)) +
  geom_line() +
  labs(title = 'Conditional variance out-of-sample', x = 'Time', y = 'Cond. variance') +
  theme_bw()

arch.resid <- residuals(arch.filt)
arch.stresid <- residuals(arch.filt, standardize = TRUE)

# calculate approximate z value
arch.resid/arch.sigma

##### 
ggplot(data = fortify(oos),aes(x = Index, y = oos)) + geom_line()

plot(fitted(arch.filt))

