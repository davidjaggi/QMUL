##### Specify the model ########################################################
ewma.spec = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)), 
                       mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                       distribution.model="norm", fixed.pars=list(omega=0))

##### Fit the data to the in sample ############################################
ewma.fit <- ugarchfit(spec = ewma.spec, data = ret[1:is.num,])  
plot(ewma.fit)
coef(ewma.fit)
confint(ewma.fit)
show(ewma.fit)

##### Fix the variables to make a rolling forecast #############################
spec <- getspec(ewma.fit)
setfixed(spec) <- as.list(coef(ewma.fit))
ewma.forc.roll <- ugarchroll(spec,
                             n.ahead = 1,
                                 data = ret,
                                 out.sample = os.num)
plot(ewma.forc)

##### Make an unconditional forecast ###########################################
ewma.for.uncond <- ugarchforecast(ewma.fit, data = ret, n.ahead = 1)
plot(ewma.for.uncond)

autoplot(arch.forc, which = 1)