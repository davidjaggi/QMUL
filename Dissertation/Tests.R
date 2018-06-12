# Test for ARCH effects
Box.test(coredata(ret^2), type = 'Ljung-Box', lag = 12)
# Reject the null hypothesis of no ARCH effects - need to control
# for remaining conditional heteroscedasticity.

