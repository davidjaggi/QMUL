from sklearn.linear_model import LinearRegression
from sklearn.svm import SVR
from sklearn import datasets
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import quandl

oil = quandl.get("CHRIS/ICE_B1", authtoken="xQ7qTpFpjY1Wvq3zT5kf")
timeseries = oil['Settle']
from statsmodels.tsa.stattools import adfuller


def rolling(timeseries):
    # Determing rolling statistics
    timeseries = timeseries.dropna()
    rolmean = timeseries.rolling(window=20, center=False).mean()
    rolstd = timeseries.rolling(window=20, center=False).std()

    # Plot rolling statistics:
    orig = plt.plot(timeseries, color='blue', label='Original')
    mean = plt.plot(rolmean, color='red', label='Rolling Mean')
    std = plt.plot(rolstd, color='black', label='Rolling Std')
    plt.title('Rolling Mean & Standard Deviation')
    plt.show(block=False)

rolling(oil['Settle'])

def test_stationarity(timeseries):
    timeseries = timeseries.dropna()
    # Perform Dickey-Fuller test:
    print('Results of Dickey-Fuller Test:')
    dftest = adfuller(timeseries, autolag='AIC')
    dfoutput = pd.Series(dftest[0:4], index=['Test Statistic', 'p-value', '#Lags Used', 'Number of Observations Used'])
    for key,value in dftest[4].items():
        dfoutput['Critical Value (%s)'%key] = value
    print(dfoutput)

test_stationarity(oil['Settle'])


from statsmodels.tsa.arima_model import ARIMA
model = ARIMA(oil.Value, order=(2, 1, 3))
results_ARIMA = model.fit(disp=-1)
arima_pred = results_ARIMA.predict(oil['Value'].index[500], oil['Value'].index[-1], typ="levels")
predictions_ARIMA = arima_pred
plt.plot(oil.Value[500:])
plt.plot(predictions_ARIMA)
plt.title('RMSE: %.4f'% np.sqrt(sum((predictions_ARIMA-oil.Value[500:])**2)/len(oil.Value[500:])))

