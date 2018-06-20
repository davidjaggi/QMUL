##### Split data into in-sample and oos ########################################
# In this file I will split the data into an in- and out-of-sample data

split_date <- '2010-01-01'
# first lets create the is sample
is <- ret[paste0('/',split_date)]
# Show summary
summary(is)
# sinker(xtable(summary(is)), name = 'is_Summary')


# now the oos data
oos <- ret[paste0(split_date,'/')]
oos.sq <- ret.sq[paste0(split_date,'/')]
# Show summary
summary(oos)
# sinker(xtable(summary(oos)), name='oos_Summary')

# there should be 2/3 of the observation oos
is.num <- nrow(is)
oos.num <- nrow(oos)

##### Plot the is and oos data #################################################
# now lets plot the data with the corresponding title

q <- ggplot(data = fortify(is), aes(x = Index, y = is)) +
  geom_line() +
  ggtitle('In-sample log-returns') +
  xlab('Time') +
  ylab('log-returns') +
  theme_bw()
# printer(q, name = 'SPret_is')
# 
# ggplot(data = fortify(oos), aes(x = Index, y = oos)) +
#   geom_line() +
#   ggtitle('In-sample log-returns') +
#   xlab('Time') +
#   ylab('log-returns') +
#   theme_minimal()
# 
# Plot both data series in one plot
# q <- ggplot() +
#   geom_line(data = fortify(is),aes(x = Index, y = is), color = 'black') +
#   geom_line(data = fortify(oos),aes(x = Index, y = oos), color = 'red') +
#   labs(title = 'In- and Out-of-Sample Split', x = 'Time', y = 'Log-return') +
#   theme_bw()
# printer(q, name = 'Is_Oos_Split')

# Plot both data series in one plot
# q <- ggplot() +
#   geom_line(data = fortify(ret.sq),aes(x = Index, y = ret.sq), color = 'black') +
#   labs(title = 'Squared daily returns as proxy', x = 'Time', y = 'Log-return') +
#   theme_bw()
# printer(q, name = 'SPret_sq')
