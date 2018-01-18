import quandl
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

cot = quandl.get('CFTC/BZ_F_L_ALL', returns='pandas')
cot.index = pd.to_datetime(cot.index)
df = pd.read_excel(open('Data/Correlation Data.xlsx','rb'),
                   sheet_name= 'Correlation Static',
                   index_col='Date')
# Index to datetime
df.index = pd.to_datetime(df.index)
# Plot the data
df.head()
# List names and correct names
list(df)
names = ['Brent','BNO','WTI','YXA1','RE1','USD Index','USD/CAD','USD/NOK','S&P 500','EuroStoxx50']
df.columns = names
brent = df['Brent']

data = cot.merge(brent.to_frame(), left_index=True, right_index=True)
data.plot()
data['Brent'].plot()

fig=plt.figure()
ax=data['Brent'].plot()
ax2=ax.twinx()
ax2=data['Noncommercial Long'].plot(c='green')
ax2=data['Noncommercial Short'].plot(c='red')
fig.suptitle('COT Report')
ax.set_xlabel('Date')
ax.set_ylabel('Brent Future Price')
ax2.set_ylabel('Noncommercial Positions')
fig.legend()

ax2.scatter(x_values2, y_values2, color="C1")
ax2.xaxis.tick_top()
ax2.yaxis.tick_right()
ax2.set_xlabel('x label 2', color="C1")
ax2.set_ylabel('y label 2', color="C1")
ax2.xaxis.set_label_position('top')
ax2.yaxis.set_label_position('right')
ax2.tick_params(axis='x', colors="C1")
ax2.tick_params(axis='y', colors="C1")

plt.show()