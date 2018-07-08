##### Split data into in-sample and oos ########################################
# In this file I will split the data into an in- and out-of-sample data
summary(ret)
# sinker(xtable(summary(ret)), name = paste0(name,'_ret_summary'))
# sinker(xtable(table.Stats(ret), digits = 4), name = paste0(name,'_ret_stats'))

split_date <- '2010-01-01'
# first lets create the is sample
is <- ret[paste0('/',split_date)]
is.sq <- ret.sq[paste0('/',split_date)]
is.abs <- abs(ret[paste0('/',split_date)])
# Show summary
summary(is)
# sinker(xtable(summary(is)), name = paste0(name,'_is_summary'))
# sinker(table.Stats(is), name = paste0(name,'_is_stats'))

# now the oos data
oos <- ret[paste0(split_date,'/')]
oos.sq <- ret.sq[paste0(split_date,'/')]
oos.abs <- abs(ret[paste0(split_date,'/')])
# Show summary
summary(oos)
# sinker(xtable(summary(oos)), name=paste0(name,'_oos_summary'))
# sinker(table.Stats(oos), name = paste0(name,'_oos_stats'))

# there should be 2/3 of the observation oos
is.num <- nrow(is)
oos.num <- nrow(oos)

remove(split_date)

##### Plot the is and oos data #################################################
# now lets plot the data with the corresponding title

q <- ggplot(data = fortify(is), aes(x = Index, y = is)) +
  geom_line() +
  ggtitle(paste0(ser_name,' In-sample log-returns')) +
  xlab('Time') +
  ylab('log-returns') +
  theme_bw()
# printer(q, name = paste0(name,'_ret_is'))

q <- ggplot(data = fortify(oos), aes(x = Index, y = oos)) +
  geom_line() +
  ggtitle(paste0(ser_name,' Out-of-sample log-returns')) +
  xlab('Time') +
  ylab('log-returns') +
  theme_bw()
# printer(q, name = paste0(name,'_ret_oos'))
# 
# Plot both data series in one plot
q <- ggplot() +
  geom_line(data = fortify(is),aes(x = Index, y = is), color = 'black') +
  geom_line(data = fortify(oos),aes(x = Index, y = oos), color = 'red') +
  labs(title = paste0(ser_name, ' In- and Out-of-Sample Split'), x = 'Time', y = 'Log-return') +
  theme_bw()
# printer(q, name = paste0(name,'_isos_split'))

# Plot both data series in one plot
q <- ggplot() +
  geom_line(data = fortify(ret.abs),aes(x = Index, y = ret.abs), color = 'black') +
  labs(title = paste0(ser_name,' Absolute daily returns as proxy'), x = 'Time', y = 'Log-return') +
  theme_bw()
# printer(q, name = paste0(name,'_ret_abs'))
