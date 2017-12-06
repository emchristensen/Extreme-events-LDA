
# Plot NDVI over time to show drought
library(RCurl)
library(dplyr)
library(ggplot2)

ndvi = read.csv(text=getURL("https://raw.githubusercontent.com/weecology/PortalData/master/NDVI/monthly_NDVI.csv"),
                   na.strings=c(""), stringsAsFactors = FALSE)

ndvi$date = as.Date(paste(ndvi$date,'-15',sep=''),format='%Y-%m-%d')
ndvi$NDVI = as.numeric(ndvi$NDVI)
ndvi$year = format(ndvi$date,'%Y') %>% as.numeric()

plot(ndvi$date,ndvi$NDVI)
lines(ndvi$date,ndvi$NDVI)


# ===============
# yearly avg - from 1993 on there are no gaps in data

ndviyr = aggregate(ndvi$NDVI,by=list(year = ndvi$year),FUN=mean) %>% filter(year>1992)


# ggplot version - with long term mean
ggplot(ndviyr,aes(x=year,y=x)) +
  geom_line(size=1.5) +  
  geom_point(size=3) +
  geom_hline(yintercept = mean(ndviyr$x),linetype=2) +
  labs(x='',y='NDVI (yearly mean)') +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))
  
