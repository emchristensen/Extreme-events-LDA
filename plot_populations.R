library(dplyr)
library(tidyr)
library(ggplot2)

dat = read.csv('Rodent_table_dat.csv')
# dates
moondat = read.csv(text=RCurl::getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/moon_dates.csv"),stringsAsFactors = F)
moondat$date = as.Date(moondat$censusdate)

period_dates = filter(moondat,period %in% rownames(dat)) %>% select(period,date)
dates = period_dates$date

dat$date = dates

longdat = gather(dat, species, abundance, BA:SO, factor_key=TRUE)

granivores = dplyr::filter(longdat,species %in% c('BA','DM','DO','DS','PB','PE','PF','PH','PI','PL','PM','PP','RF','RM','RO'))


popplot <-ggplot(granivores, aes(x=date,y=abundance,colour=species)) +
  geom_line(size=1.5) +
  theme(axis.title.x=element_blank())
popplot
#ggsave('C:/Users/EC/Desktop/granivore_population_ts.png',popplot,width=4,height=2)


# ==================================================
# plot relative abundance?
relative = dat[,-22]/rowSums(dat[,-c(22)])

relative$date = dates

longdat_rel = gather(relative, species, abundance, BA:SO, factor_key=TRUE)

granivores_rel = dplyr::filter(longdat_rel,species %in% c('BA','DM','DO','DS','PB','PE','PF','PH','PI','PL','PM','PP','RF','RM','RO'))

relplot <-ggplot(granivores_rel, aes(x=date,y=abundance,colour=species)) +
  geom_line(size=1.5) +
  theme(axis.title.x=element_blank(),
        text=element_text(size=12),
        axis.title.y=element_text(size=18),
        axis.text.x=element_text(size=14)) +
  ylab('Relative Abundance')
relplot
ggsave('C:/Users/EC/Desktop/granivore_population_ts.png',relplot,width=11,height=4)
