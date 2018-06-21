##### Specify the model ########################################################
arch.spec = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(0,1)), 
                       mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                       distribution.model="norm")

##### Fit the data to the in sample ############################################
arch.fit <- ugarchfit(spec = arch.spec, data = ret, out.sample = oos.num)  
plot(arch.fit)
coef(arch.fit)
confint(arch.fit)
show(arch.fit)
# sinker(show(arch.fit), name = 'ARCH_fit')

arch.fit@fit$matcoef
persistence(arch.fit)
##### Fix the variables to filter the oos data #################################
arch.spec.fixed <- getspec(arch.fit)
setfixed(arch.spec.fixed) <- as.list(coef(arch.fit))
arch.forc <- ugarchforecast(arch.spec.fixed, data = ret, n.ahead = 1, 
                            n.roll = oos.num-1, out.sample = oos.num-1)

plot(arch.filt)

##### Extract varaibles ########################################################
arch.sigma <- t(sigma(arch.forc))
arch.sigma.sq <- arch.sigma^2

##### Analyse the forecast #####################################################
# create a time series with the estimated and the real values
# We use the relized volatility as the squared returns
arch.result <- oos.sq
colnames(arch.result) <- c('RV')
arch.result$Sigma <- t(sigma(arch.forc))
arch.result$Sigma.sq <- arch.result$Sigma^2

# Plot the estimation
q <- ggplot(data = fortify(arch.result), aes(x = Index)) +
  geom_line(aes(y = RV)) +
  geom_line(aes(y = Sigma.sq), colour = 'red') +
  labs(title = 'Realized vs estimated volatility', x = 'Time', y = 'Volatility') +
  theme_bw()
# printer(q, 'ARCH_realvsestd')

##### Test the volatility forecast #############################################
# Show the correlation between the forecast and the realized volatility
cor(arch.result$RV, arch.result$Sigma.sq, 
    method = "spearman")

# Show the accuracy of our estimate
accuracy(ts(arch.result$RV), ts(arch.result$Sigma.sq))

# make a regression
arch.lm <- lm(formula = arch.result$RV ~ arch.result$Sigma.sq)
plot(arch.lm)
summary(arch.lm)
# sinker(summary(arch.lm), name = 'arch_lm')
accuracy(arch.lm)

ggplot(fortify(arch.result), aes(x = Index)) +
  # geom_line(aes(y = abs(OOS))) +
  # geom_line(aes(y = Sigma)) +
  geom_line(aes(y = Resid)) +
  theme_bw() +
  
infocriteria(arch.forc)

# conditional variance

# Plot conditional variance
ggplot(data = fortify(arch.result), aes(x = Index, y = Sigma)) +
  geom_line() +
  labs(title = 'Conditional variance out-of-sample', x = 'Time', y = 'Cond. variance') +
  theme_bw()

arch.resid <- residuals(arch.forc)
arch.stresid <- residuals(arch.filt, standardize = TRUE)

# calculate approximate z value
arch.resid/arch.sigma

##### 
ggplot(data = fortify(oos),aes(x = Index, y = oos)) + geom_line()

plot(fitted(arch.filt))

