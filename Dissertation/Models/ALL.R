subfolder <- 'ALL'
##### Make conclusive plots ####################################################
all1 <- ggplot(data = fortify(oos.all), aes(x = as.Date(Index))) +
  geom_line(aes(y = RV, colour = 'RV')) +
  geom_line(aes(y = GARCH, colour = 'GARCH')) +
  geom_line(aes(y = EGARCH, colour = 'EGARCH')) +
  geom_line(aes(y = TGARCH, colour = 'TGARCH'))+
  geom_line(aes(y = NGARCH, colour = 'NGARCH')) +
  geom_line(aes(y = APARCH, colour = 'APARCH')) +
  scale_x_date(limits = c(as.Date('2018-01-01', format = '%Y-%m-%d'), as.Date('2018-06-31', format = '%Y-%m-%d'))) +
  labs(title = paste0(ser_name,' Realized vs estimated volatility out-of-sample zoom'), x = 'Time', y = 'Volatility') +
  theme_bw() +
  scale_colour_manual(name="Models",
                      values=c(RV="black", GARCH="red", EGARCH="green3", TGARCH = 'blue', NGARCH = 'magenta', APARCH = 'yellow'))
printer(all1, folder, subfolder,paste0(name,'_all_forc_rve_zoom'))

rm(all1)
rm(subfolder)
