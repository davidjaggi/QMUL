# Load the data
sp500 <- read.csv(file = 'Dissertation/Data/SP500.csv')
# convoert it into an xts
sp500 <- as.xts(x = sp500$SP500, order.by = as.POSIXct(sp500$X))

plot(sp500)
