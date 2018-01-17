# Source: http://cs.bme.hu/~adam.zlatniczki/education/stockforecast/08_volatility_modelling.py
# https://machinelearningmastery.com/arima-for-time-series-forecasting-with-python/
# https://machinelearningmastery.com/make-sample-forecasts-arima-python/
http://www.blackarbs.com/blog/time-series-analysis-in-python-linear-models-to-garch/11/1/2016
# -*- coding: utf-8 -*-
"""
Created on Sun Dec 06 20:58:52 2015

@author: Zlati
"""

import pandas as pd
import numpy as np
from statsmodels.graphics import tsaplots
from statsmodels.tsa.stattools import adfuller
from statsmodels.tsa.arima_model import ARIMA
import statsmodels
from scipy.stats.mstats import normaltest


from matplotlib import pylab

# Why is forecasting volatility important? Volatility usually affects risk premium

# Stock volatility is not directly observable from a daily return
# (intraday data may be used, but overnight data is still missing)

# Characteristics:
# - Volatility clustering (larger changes are followed by larger changes of either sign,
#                          and smaller changes are followed by smaller changes of either sign)
# - Volatility evolves continuously (jumps are rare)
# - Volatility doesn't diverge to infinity (it's stationary)
# - Leverage effect (reacts differently -assimetrically- to price increase and price decrease)


# Load the Intel Corporation dataset
df = pd.read_excel('Data/Correlation Data.xlsx',
                   sheet_name='Correlation Static')
# Clean it
df.index = pd.to_datetime(df.Date)
df = df.drop("Date", axis=1)

df = df['CO1 Comdty']
prices = df.sort_index()

# Explore the data a little
prices.plot()

prices.mean()  # mean
prices.std()   # standard deviation
prices.std()/prices.mean()  # coefficient of variation
# the stock is very volatile, there are big jumps; the std. is rather large
# compared to the mean (high coefficient of variation)


tsaplots.plot_acf(prices, lags=36) # just terrible
tsaplots.plot_pacf(prices, lags=36) # very significant at lag=1
# this is a typical sign of an integrated process, so let's derivate it
# (use returns instead of prices), which will hopefully yield a stationary process!



returns = prices.pct_change()
returns = returns[1:] # drop NA


returns.plot()

returns.mean()
returns.std()
returns.std()/returns.mean()

# verify stationarity
adfuller(returns, regression="c")
adfuller(returns, regression="ct")
adfuller(returns, regression="ctt")
adfuller(returns, regression="nc")
# a unit root does not exist - the process is now stationary

# test normality of returns
normaltest(returns)
# the series is not white noise, ARIMA models should be applied!

tsaplots.plot_acf(returns, lags=36) # seems quite okay, significant lag=2
tsaplots.plot_pacf(returns, lags=36) # significant at lag=2
# we can conclude that at most p=2 and q=2


# fit ARIMA models to the returns
m = statsmodels.tsa.arima_model.ARIMA(returns, order=(2,0,0)).fit()
m.summary()
# lag=2 and the constant seem insignificant so let's try an ARIMA(1,0,0)

# p-value for the second AR parameter is small, let's try it with AR(1)
m = statsmodels.tsa.arima_model.ARIMA(returns, order=(1,0,0)).fit()
m.summary()
# the AIC is smaller and the parameter is significant, but very close to 0,
# so its importance is at least questionable => let's try an ARIMA(0,0,0)

# try with a constant only
m = statsmodels.tsa.arima_model.ARIMA(returns, order=(0,0,0)).fit()
m.summary()
# best AIC so far (-31512.341)

# try with MA(q=1)
m = statsmodels.tsa.arima_model.ARIMA(returns, order=(0,0,1)).fit()
m.summary()
# AIC is worse

# try MA(q=2)
m = statsmodels.tsa.arima_model.ARIMA(returns, order=(0,0,2)).fit()
m.summary()
# AIC still did not beat the best

# try ARMA
m = statsmodels.tsa.arima_model.ARIMA(returns, order=(2,0,1)).fit()
m.summary()

# Based on the AICs and p-values the best model was the ARIMA(0,0,0) -

# from now on let's focus on the analysis of the residuals
m = statsmodels.tsa.arima_model.ARIMA(returns, order=(2,0,1)).fit()
residuals = m.resid

pylab.plot(residuals)

normaltest(residuals)
# the residuals do not come from a white noise process, check the ACF / PACF

tsaplots.plot_acf(residuals, lags=10)
tsaplots.plot_pacf(residuals, lags=10)


# The residuals seem like they're no more significantly autocorrelated
# (we tried to fit models on them, but they didn't help much), but
# they're far from being a white noise process, since they're surely not normally distributed

# volatility modelling comes in when we achieved our final model and the residual series is serially
# uncorrelated (or has only lower order serial correlation), but "something isn't quite right",
# some effects are not explained by the model (residuals are not white noise), so the residuals are still dependent
# (remember: zero correlation doesn't imply independence, only independence implies that the correlation is zero)


###############################################################################
##                      Steps of volatility modelling                        ##
###############################################################################
#
# 1) identify the mean equation (the model that leaves you with the uncorrelated
#    but dependent residuals - ARIMA(0,1,0) in the preceding example)
# 2) check the residuals for ARCH effects
#    ARCH: autoregressive conditional heteroscedastic
#          (autoregressive: we check ACF and PACF of the square/abs. residuals)
#          (conditional: we use the mean equations, the residuals are conditioned on its result)
#          (heteroscedastic: we analyse residuals of a regression and try to find patterns in it; like volatility clustering)
# 3) specify a volatility model (ARCH, GARCH, EGARCH, TGARCH, ... lots more)
# 4) evaluate the model


###############################################################################
## Checking for ARCH effect ##
###############################################################################
#
# - by plotting: volatility clustering, autocorrelation of sign-removed residuals
# - by tests: a) run Ljung-Box (or Box-Pierce) test on squared residuals to find out whether
#                the first m lags are significantly zero
#             b) run the White test on the residuals


###################################
# Check ARCH effects with plotting:

# to check volatility clustering, we should remove the sign of the changes
# (we can take the absolute value or the square of the residues for this purpose)
pylab.plot(np.abs(residuals))
pylab.plot(np.square(residuals))
# these two plots, most eye-catchingly the second, confirm volatility clustering!

# serieal dependence can be checked by plotting the ACF and PACF on the absolute or squared series
tsaplots.plot_acf(np.square(residuals), lags=20)
tsaplots.plot_pacf(np.square(residuals), lags=20) # lag=8
# since squaring very little values gives even less ones, which results in very small
# differences, we got zero autocorrelations; this might be misleading, try the abs.

tsaplots.plot_acf(np.abs(residuals), lags=20)
tsaplots.plot_pacf(np.abs(residuals), lags=20)
# lag=8 is okay, but others are promising as well (but in practice
# we should keepd the orders low to avoid overfit)

# we can conclude that even though the residual returns are uncorrelated,
# they are dependent!


###############################
# Check ARCH effects with tests

from statsmodels.stats import diagnostic

# H0: autocorrelations at lags are zero
lbvalue, lbpvalue, bpvalue, bppvalue = diagnostic.acorr_ljungbox(np.square(residuals), boxpierce=True)

pylab.plot(lbpvalue, label="Ljung-Box") # Ljung-Box test  p-values (better for small smaples)
pylab.plot(bppvalue, label="Box-Pierce") # Box-Pierce test p-values (better for larger samples)
pylab.axhline(0.05, color="r")
pylab.legend()


lbvalue, lbpvalue, bpvalue, bppvalue = diagnostic.acorr_ljungbox(np.abs(residuals), boxpierce=True)

pylab.plot(lbpvalue, label="Ljung-Box") # Ljung-Box test  p-values (better for small smaples)
pylab.plot(bppvalue, label="Box-Pierce") # Box-Pierce test p-values (better for larger samples)
pylab.axhline(0.05, color="r")
pylab.legend()


# We found that the null-hypothesis is rejected at every lag, which means there
# is significant autocorrelation, thus ARCH effect

# The arch package provides methods that can utilize the abs. of the residuals
# as well

#################################
# Fit ARCH models
from arch import arch_model

# !!! The arch package swtiches the meaning of p and q of the GARCH models
#     compared to the notation on Wikipedia !!!

np.mean(np.square(residuals))
# the mean of the squared residuals is very close to zero, so the mean equation
# of the ARCH model can be omitted

# If we already have the residuals we can fit the ARCH only on that part by
# setting the mean equation to be a constant zero; let's see an ARCH(q=1) on
# the squared residuals
# (If we wanted to work with absolute residuals power=1 shoudl be used)
am1 = arch_model(residuals, mean="Zero", vol="ARCH", p=1, dist="Normal", power=2.0)
res1 = am1.fit()
res1.plot()
res1.summary()

pylab.plot(np.square(residuals))

# previously we found from the PACF of the squared (and absolute) residuals
# that the lag should be 8
am2 = arch_model(residuals, mean="Zero", vol="ARCH", p=8, dist="Normal", power=2.0)
res2 = am2.fit()
res2.plot()
res2.summary()
# AIC is much better, the plot looks better as well


# If we do not have the residuals, only the returns, then we must specify a
# mean equation that will result in some kind of residuals
# By checking the returns we could say that they just oscillate around the mean,
# so let's set the mean equation to be constant

am3 = arch_model(returns, mean="Constant", vol="ARCH", p=1, dist="Normal", power=2.0)
res3 = am3.fit()
res3.plot()
res3.summary()
# quite high standard errors compared to the coeffs, omega is insignificant
# (H0: coeff is zero)


am4 = arch_model(returns, mean="Constant", vol="ARCH", p=8, dist="Normal", power=2.0)
res4 = am4.fit()
res4.plot()
res4.summary()


# let's try a different distribution for the innovation
am5 = arch_model(returns, mean="Constant", vol="ARCH", p=8, dist="StudentsT")
res5 = am5.fit()
res5.plot()
res5.summary()
# much better AIC

# plot their difference
pylab.plot(res4.conditional_volatility, label="ARCH(8), Normal innovation")
pylab.plot(res5.conditional_volatility, label="ARCH(8), Student innovation")
pylab.legend()


# If we assume that the residuals follow an ARMA process, then we are talking
# about a GARCH(p, q) model
am6 = arch_model(returns, mean="Constant", vol="GARCH", p=1, q=1)
res6 = am6.fit()
res6.plot()
res6.summary()
# significant coeffs

pylab.plot(res1.conditional_volatility, label="ARCH(1)", color="green")
pylab.plot(res6.conditional_volatility, label="GARCH(1,1)", color="blue")
pylab.legend()



am7 = arch_model(returns, mean="Constant", vol="GARCH", p=5, q=5)
res7 = am7.fit()
res7.plot()
res7.summary()

# If we want to forecast the volatility we can
res7.conditional_volatility

###############################################################################
##                               Applications                                ##
###############################################################################


## Portfolio optimization - CAL ##
# Problem: Our portfolio consists of two assets, a risk free and a risk. We want
#          the portfolio to have a stable risk, but the risk of the risky asset
#          might change from time to time, hence the weight of the risky asset
#          must be corrigated - but for that we need to forecast the volatility.


## Option trading - Black-Scholes model ##
# Problem: We need to find the price of an option tomorrow to determine whether
#          we want to buy it or not. The price of the option can be calculated
#          with the Black-Scholes formula, but it depends on the asset volatility,
#          so we have to forecast it.


## Stock trading ##
# Problem: We bought an asset today for 34$ and we want to know its risk, for
#          example the probability that next day it will fall under 34$. We can
#          forecast next day's expected price with the mean equation,
#          and we can forecast the variance with some GARCH model. This mean
#          and variance can be used to define a normal distribution, from
#          which we can calculate this probability.