
import pandas as pd
from matplotlib import pyplot as plt

# load dataset
dataset = pd.read_excel('Data/Facebook.xlsx', sheet_name='FB Static', index_col=0)
values = dataset.values

# plot
dataset.plot()

# split data into train and test
X = dataset.values
train, test = X[0:-12], X[-12:]

# Persistence forecast
from pandas import read_csv
from pandas import datetime
from sklearn.metrics import mean_squared_error
from math import sqrt
from matplotlib import pyplot
# split data into train and test
X = dataset.values
train, test = X[0:-100], X[-100:]
# walk-forward validation
history = [x for x in train]
predictions = list()
for i in range(len(test)):
	# make prediction
	predictions.append(history[-1])
    # observations
    history.append(test[i])
# report performance
rmse = sqrt(mean_squared_error(test, predictions))
print('RMSE: %.3f' % rmse)
# line plot of observed vs predicted
pyplot.plot(test)
pyplot.plot(predictions)
pyplot.show()


