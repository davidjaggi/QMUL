import numpy as np
import pandas as pd
from matplotlib import pyplot as plt
import seaborn as sns
import datetime

# Read the data
df = pd.read_excel(open('Data/Correlation Data.xlsx','rb'),
                   sheet_name= 'Correlation Static',
                   index_col='Date')
# Index to datetime
df.index = pd.to_datetime(df.index)
# Plot the data
df.head()
# Remove NAs
df = df.dropna()
# Check for NAs
df.isnull().sum()
# List names and correct names
list(df)
names = ['Brent','BNO','WTI','YXA1','RE1','USD Index','USD/CAD',
         'USD/NOK','S&P 500','EuroStoxx50']

ret = df.pct_change()
ret.columns = names
ret.head()
ret1y = ret.loc['2017-01-16':'2018-01-30']
ret6m = ret.loc['2017-07-16':'2018-01-30']
ret3m = ret.loc['2017-11-16':'2018-01-30']

corr1y = ret1y.corr().dropna()
corr6m = ret6m.corr().dropna()
corr3m = ret3m.corr().dropna()
# 1 Year
sns.clustermap(corr1y,
            annot=True,
            cmap = 'RdYlGn',
            xticklabels=corr.columns,
            yticklabels=corr.columns)