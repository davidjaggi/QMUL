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
if (tseries == 'SPX'){
name <- 'SPX'
ser_name <- 'S&P 500'
folder <- 'SPX'
subfolder <- 'DataLoader'

# Load the data
price <- read.csv(file = 'Dissertation/Data/Price_Data.csv')
# convoert it into an xts
# colnames(sp500) <- c('Date','Close')
x <- as.xts(x = price$SPX, order.by = as.POSIXct(price$Date,
                                                 tryFormat = "%d/%m/%Y"))
rm(price)
}

##### Import FTSE ##############################################################
if (tseries == 'FTSE'){
name <- 'FTSE'
ser_name <- 'FTSE 100'
folder <- 'FTSE'
subfolder <- 'DataLoader'

# Load the data
price <- read.csv(file = 'Dissertation/Data/Price_Data.csv')
# convoert it into an xts
# colnames(sp500) <- c('Date','Close')
x <- as.xts(x = price$UKX, order.by = as.POSIXct(price$Date, 
                                                 tryFormat = "%d/%m/%Y"))
rm(price)
}
##### Import DAX ###############################################################
if (tseries == 'DAX'){
name <- 'DAX'
ser_name <- 'DAX'
folder <- 'DAX'
subfolder <- 'DataLoader'

# Load the data
price <- read.csv(file = 'Dissertation/Data/Price_Data.csv')
# convoert it into an xts
# colnames(sp500) <- c('Date','Close')
x <- as.xts(x = price$DAX, order.by = as.POSIXct(price$Date,
                                                 tryFormat = "%d/%m/%Y"))
rm(price)
}
##### Adjust series ############################################################
# crop the time series
x <- x['1990-01-01/']
x <- x['/2018-06-30']
# Check the series
head(x)
tail(x)
summary(x)

dl1 <- ggplot(fortify(x), aes(x = Index, y = x)) +
  geom_line() +
  labs(title = paste0(ser_name,': Series'), x = 'Time', y = 'Price') +
  theme_bw()
printer(dl1, folder, subfolder, paste0(name,'_series'))


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
rm(dl1, subfolder)
