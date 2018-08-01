subfolder <- 'ALL'
##### Make conclusive plots ####################################################
if (tseries == 'SPX'){
  xl <- 0.045
} else if (tseries == 'FTSE'){
  xl <- 0.03
} else if (tseries == 'DAX'){
  xl <- 0.035
}


all1 <- ggplot(data = fortify(oos.all), aes(x = as.Date(Index))) +
  geom_line(aes(y = RV, colour = 'RV')) +
  geom_line(aes(y = GARCH, colour = 'GARCH')) +
  geom_line(aes(y = EGARCH, colour = 'EGARCH')) +
  geom_line(aes(y = TGARCH, colour = 'TGARCH'))+
  geom_line(aes(y = NGARCH, colour = 'NGARCH')) +
  geom_line(aes(y = APARCH, colour = 'APARCH')) +
  ylim(0,xl) +
  scale_x_date(limits = c(as.Date('2018-01-01', format = '%Y-%m-%d'), as.Date('2018-06-31', format = '%Y-%m-%d'))) +
  labs(title = paste0(ser_name,' Realized vs estimated volatility OOS'), x = 'Time', y = 'Volatility') +
  theme_bw() +
  scale_colour_manual(name="Models",
                      values=c(RV="black", GARCH="red", EGARCH="green3", TGARCH = 'blue', NGARCH = 'magenta', APARCH = 'orange'))
printer(all1, folder, subfolder,paste0(name,'_all_forc_rve_zoom'))
rm(oos.all)

colnames(impact.all) <- c('x','GARCH','EGARCH','TGARCH','NGARCH','APARCH')
all2 <- ggplot(data = impact.all, aes(x = x)) +
  geom_line(aes(y = GARCH, colour = 'GARCH')) +
  geom_line(aes(y = EGARCH, colour = 'EGARCH')) +
  geom_line(aes(y = TGARCH, colour = 'TGARCH'))+
  geom_line(aes(y = NGARCH, colour = 'NGARCH')) +
  geom_line(aes(y = APARCH, colour = 'APARCH')) +
  labs(title = paste0(ser_name,' News Impact Curve'), x = 'epsilon[t - 1]', y = 'sigma[t]^2') +
  theme_bw() +
  scale_colour_manual(name="Models",
                      values=c(RV="black", GARCH="red", EGARCH="green3", TGARCH = 'blue', NGARCH = 'magenta', APARCH = 'orange'))

printer(all2, folder, subfolder,paste0(name,'_all_fit_news'))

models <- c('garch','egarch','tgarch','ngarch','aparch')
dm_res <- matrix(data = NA, nrow = 5, ncol = 5)
colnames(dm_res) <- models
rownames(dm_res) <- models
for(i in c(1:5)){
  for(j in c(1:5)){
    if(i == j){
      dm_res[i,j] <- '-'
    } else {
      e1 <- get(paste0('fit_res_', models[i]))
      e2 <- get(paste0('fit_res_', models[j]))
      dm_res[i,j] <- as.numeric(dm.test(e1,e2, alternative = 'two.sided')$p.value)
    }
  }
}
sinker(dm_res, folder, subfolder,paste0(name,'_all_fit_dm'))

models <- c('garch','egarch','tgarch','ngarch','aparch')
dm_res <- matrix(data = NA, nrow = 5, ncol = 5)
colnames(dm_res) <- models
rownames(dm_res) <- models
for(i in c(1:5)){
  for(j in c(1:5)){
    if(i == j){
      dm_res[i,j] <- '-'
    } else {
      e1 <- get(paste0('forc_res_', models[i]))
      e2 <- get(paste0('forc_res_', models[j]))
      dm_res[i,j] <- as.numeric(dm.test(e1,e2, alternative = 'two.sided')$p.value)
    }
  }
}
sinker(dm_res, folder, subfolder,paste0(name,'_all_forc_dm'))


rm(all1, all2, dm_res, e1,e2)
rm(impact, impact.all)
rm(subfolder)
rm(list = ls(pattern = '^fit.'))
rm(list = ls(pattern = '^forc.'))
rm(models,i,j,ser_name, name, folder, xl)
