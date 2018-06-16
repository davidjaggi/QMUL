##### Load necessary packages ##################################################
source('Dissertation/Prep/Packages.R')

# Load the sp500 data from the FRED database in order to test the first model
# getSymbols.FRED(Symbols = 'SP500', env = globalenv())

# Write the data to a csv to store them
# write.csv(x = as.data.frame(SP500), file = 'Dissertation/Data/SP500.csv')
# saveRDS(object = sp500, file = 'Dissertation/Data/SP500.rds')
# saveSymbols(Symbols = sp500, file.path = 'Dissertation/Data/sp500.csv')

##### Import Data ##############################################################
# Load the data
sp500 <- read.csv(file = 'Dissertation/Data/SP500.csv')
# convoert it into an xts
colnames(sp500) <- c('Date','Close')
x <- as.xts(x = sp500$Close, order.by = as.POSIXct(sp500$Date, tryFormat = "%d/%m/%Y"), tz = 'UTC')
remove(sp500)

# crop the time series
x <- x['1990-01-01/']
# Check the series
head(x)

##### Calculate returns ########################################################
# Calculate returns
ret <- diff(x = log(x), lag = 1)
ret <- na.omit(ret)

# Squared returns are used as proxy for conditional volatility and 
# absolute returns for unconditional volatility
ret.sq <- ret^2
ret.abs <- abs(ret)

##### Make tidy series #########################################################
ret_tidy <- tidy(ret)
x_tidy <- tidy(x)

##### Split data into is and oos ###############################################
source('Dissertation/Prep/isos_Splitter.R')
