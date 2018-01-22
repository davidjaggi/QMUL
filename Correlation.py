import numpy as np
import pandas as pd
from matplotlib import pyplot as plt
import seaborn as sns
import datetime

# Read the data
df = pd.read_excel(open('Data/Correlation.xlsx','rb'),
                   sheet_name= 'Corr Static',
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
names = ['Facebook','Tencent','Twitter','Snap','Google','Netflix']

ret = df.pct_change()
ret.columns = names
ret.head()
ret1y = ret.loc['2017-01-16':'2018-01-30']
ret6m = ret.loc['2017-07-16':'2018-01-30']
ret3m = ret.loc['2017-11-16':'2018-01-30']

corr1y = ret1y.corr().dropna()
corr6m = ret6m.corr().dropna()
corr3m = ret3m.corr().dropna()
# Configuration
sns.set()
# 1 Year
sns.set(rc={'figure.figsize':(11.7,8.27)}, font_scale=1.5)
g1y = sns.heatmap(corr1y,
                        annot=True,
                        cmap = 'RdYlGn',
                        xticklabels=ret.columns,
                        yticklabels=ret.columns)
g1y.set_title('Correlation over 1 year')
loc, labels = plt.xticks()
g1y.set_xticklabels(labels, rotation=45)
g1y.set_yticklabels(labels[::-1], rotation=45) # reversed order for y
g1y = g1y.get_figure()
g1y.savefig('Graphs/Correlation1y.jpeg')
plt.close(g1y)

sns.set(rc={'figure.figsize':(11.7,8.27)}, font_scale=1.5)
g6m = sns.heatmap(corr6m,
                        annot=True,
                        cmap = 'RdYlGn',
                        xticklabels=ret.columns,
                        yticklabels=ret.columns)
g6m.set_title('Correlation over 6 months')
loc, labels = plt.xticks()
g6m.set_xticklabels(labels, rotation=45)
g6m.set_yticklabels(labels[::-1], rotation=45) # reversed order for y
g6m = g6m.get_figure()
g6m.savefig('Graphs/Correlation6m.jpeg')
plt.close(g6m)

sns.set(rc={'figure.figsize':(11.7,8.27)}, font_scale=1.5)
g3m = sns.heatmap(corr3m,
                        annot=True,
                        cmap = 'RdYlGn',
                        xticklabels=ret.columns,
                        yticklabels=ret.columns)
g3m.set_title('Correlation over 3 months')
loc, labels = plt.xticks()
g3m.set_xticklabels(labels, rotation=45)
g3m.set_yticklabels(labels[::-1], rotation=45) # reversed order for y
g3m = g3m.get_figure()
g3m.savefig('Graphs/Correlation3m.jpeg')
plt.close(g3m)