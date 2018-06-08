import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
sns.set(color_codes=True)

data = pd.read_excel('Data/LS.xlsx', sheet_name='LS Static', index_col=0)
data.head()

ret = data.pct_change()
ret = ret.dropna()

x = ret['SPX Index']
y = ret['FB US Equity']
# ret.plot()
fig, ax = plt.subplots()

fit = np.polyfit(x, y, deg = 1)
ax.plot(x, fit[0] * x + fit[1], color = 'red')

sns.set(rc={'figure.figsize':(11.7,8.27)}, style='white')
g = sns.jointplot(x="SPX Index", y="FB US Equity", data=ret, kind='reg',
                  scatter_kws={'s': 20}, xlim=(-0.04,0.03), stat_func=None,
                  marginal_kws={'hist_kws': {'edgecolor': "black"}})
g = g.fig
g.savefig('Graphs/LeastSquares.jpeg')
plt.close(g)



fig1 = plt.figure(figsize=(7,7))
ax1 = fig1.add_subplot(111)
ax1.hist(x, bins=25, fill = None, facecolor='none',
        edgecolor='black', linewidth = 1)



fig2 = plt.figure(figsize=(7,7))
ax2 = fig2.add_subplot(111)
ax2.hist(y, bins=25 , fill = None, facecolor='none',
        edgecolor='black', linewidth = 1)