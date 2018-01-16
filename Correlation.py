import numpy as np
import pandas as pd
from matplotlib import pyplot as plt
import seaborn as sns

df = pd.read_excel(open('Data/Correlation Data.xlsx','rb'), sheet_name= 'Correlation Static')
df.head()
df = df.dropna()
df.head()
list(df)
df.isnull().sum()
ret = df.pct_change()