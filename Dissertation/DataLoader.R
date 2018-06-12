# Load necessary package

library(quantmod)

# Load the sp500 data from the FRED database in order to test the first model
getSymbols.FRED(Symbols = 'SP500', env = globalenv())

# Write the data to a csv to store them
write.csv(x = as.data.frame(SP500), file = 'Dissertation/Data/SP500.csv')
saveRDS(object = sp500, file = 'Dissertation/Data/SP500.rds')
saveSymbols(Symbols = sp500, file.path = 'Dissertation/Data/sp500.csv')
