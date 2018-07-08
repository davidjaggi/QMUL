##### Load necessary packages ##################################################
source('Dissertation/Prep/Packages.R')
source('Dissertation/Prep/Functions.R')

# Load the sp500 data from the FRED database in order to test the first model
# getSymbols.FRED(Symbols = 'SP500', env = globalenv())

# Write the data to a csv to store them
# write.csv(x = as.data.frame(SP500), file = 'Dissertation/Data/SP500.csv')
# saveRDS(object = sp500, file = 'Dissertation/Data/SP500.rds')
# saveSymbols(Symbols = sp500, file.path = 'Dissertation/Data/sp500.csv')

##### Import SP500 #############################################################
name <- 'SP'
ser_name <- 'S&P 500'

# Load the data
sp500 <- read.csv(file = 'Dissertation/Data/SP500.csv')
# convoert it into an xts
colnames(sp500) <- c('Date','Close')
x <- as.xts(x = sp500$Close, order.by = as.POSIXct(sp500$Date, tryFormat = "%d/%m/%Y"))
remove(sp500)

##### Import EURUSD ############################################################
name <- 'EURUSD'
ser_name <- 'EUR/USD'

# Load the data
eurusd <- read.csv(file = 'Dissertation/Data/EURUSD.csv')
# convoert it into an xts
colnames(eurusd) <- c('Date','Close')
x <- as.xts(x = eurusd$Close, order.by = as.POSIXct(eurusd$Date, tryFormat = "%d/%m/%Y"))
remove(eurusd)

##### Adjust series ############################################################
# crop the time series
x <- x['1990-01-01/2018-05-31']
# Check the series
head(x)
summary(x)

q <- ggplot(fortify(x), aes(x = Index, y = x)) +
  geom_line() +
  labs(title = paste0(ser_name,' Series'), x = 'Time', y = 'Price') +
  theme_bw()
# printer(q, paste0(name,'_series'))
q

##### Calculate returns ########################################################
# Calculate returns
ret <- diff(x = log(x), lag = 1)
ret <- na.omit(ret)

# Squared returns are used as proxy for conditional volatility and 
# absolute returns for unconditional volatility
ret.sq <- ret^2
ret.abs <- abs(ret)

##### Make tidy series #########################################################
# ret_tidy <- tidy(ret)
# x_tidy <- tidy(x)

##### Split data into is and oos ###############################################
source('Dissertation/Prep/isos_Splitter.R')
