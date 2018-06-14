##### Import the setup #########################################################
source('Dissertation/Prep/Setup.R')

arch.spec <- ugarchspec(mean.model = list(armaOrder = c(0,0), 
                                          include.mean = FALSE),
                        distribution.model = 'norm')

arch.fit <- ugarchfit(spec = arch.spec, data = is)  
plot(arch.fit)
coef(arch.fit)
confint(arch.fit)
show(arch.fit)

spec <- getspec(arch.fit)
setfixed(spec) <- as.list(coef(arch.fit))
arch.forc <- ugarchforecast(spec, n.ahead = 1, n.roll = is.num-1,
                            data = ret[1:is.num, , drop=FALSE], 
                            out.sample = is.num-1)

plot(arch.forc)
autoplot(arch.forc, which = 1)
