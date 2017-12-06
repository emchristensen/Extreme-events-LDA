library(fitdistrplus)
library(stats)
library(dplyr)
# scripts for making supporting figures in the extreme events/ LDA project

# =========================================
# Annual average NDVI plot

ndvi = read.csv('../../../Portal/PortalData/NDVI/monthly_NDVI.csv',as.is=T)
ndvi$date = as.Date(paste(ndvi$date,"-15", sep=""), format="%Y-%m-%d")
ndvi$year = format(as.Date(ndvi$date, format="%Y-%m-%d"),"%Y")
annual_ndvi = aggregate(NDVI ~ year, ndvi, mean )
plot(annual_ndvi$year,annual_ndvi$NDVI, type='b')

# ==============================================
# Figure 5 in manuscript

# total abundance
dat = read.csv('Rodent_table_dat.csv',na.strings = '',as.is=T)
rdat = read.csv('../../../Portal/PortalData/Rodents/Portal_rodent.csv',as.is=T)
trappingdat = read.csv('../../../Portal/PortalData/Rodents/Portal_rodent_trapping.csv',as.is=T,na.strings = '')
trapdat = aggregate(trappingdat$sampled,by=list(period=trappingdat$period),FUN=sum)
fullcensus = trapdat[trapdat$x>20,]
perioddates = read.csv('../../../Portal/PortalData/Rodents/moon_dates.csv',as.is=T,na.strings = '')
perioddates$censusdate = as.Date(perioddates$censusdate)
fullcensus = merge(fullcensus,perioddates)

# data frame
abund_dat = data.frame(period = 1:436, n = rowSums(dat))
abund_dat = merge(abund_dat,fullcensus[,c('period','censusdate')])

# finding data in the lowest .15 fraction of the data. 
descdist(abund_dat$n, discrete=TRUE)
fit.negbin = fitdist(abund_dat$n, "nbinom")
plot(fit.negbin)
dist_size = fit.negbin$estimate[[1]]
dist_mu = fit.negbin$estimate[[2]]
crit_value = qnbinom(.15, size=dist_size, mu=dist_mu)
abund_dat$extreme = ifelse(abund_dat$n < crit_value, 1,0)

# plot
plot(abund_dat$censusdate,abund_dat$n,xlab='',ylab='Total Abundundance',pch=19,ylim=c(0,210),cex.axis=1.5,cex.lab=1.5)
rect(xleft = as.Date('1999-07-01'),xright = as.Date('1999-10-01'),ytop = 250,ybottom=0,col='gray',border=NA)
rect(xleft = as.Date('1983-08-01'),xright = as.Date('1983-11-01'),ytop = 250,ybottom=0,col='gray',border=NA)
rect(xleft = as.Date('1993-09-01'),xright = as.Date('1994-10-01'),ytop = 250,ybottom=0,col='gray',border=NA)
rect(xleft = as.Date('2009-01-01'),xright = as.Date('2009-12-31'),ytop = 250,ybottom=0,col='green',border=NA)
lines(abund_dat$censusdate,abund_dat$n)
points(abund_dat$censusdate,abund_dat$n,pch=16,col=as.factor(abund_dat$extreme))
abline(h=mean(abund_dat$n))
box(which='plot')

# line segments showing changepoint 95% intervals
segments(as.Date('1983-12-01'),200 , as.Date('1984-07-01'), 200, col='red', lwd=3, xpd = FALSE)
segments(as.Date('1988-10-01'),200 , as.Date('1996-01-01'), 200, col='red', lwd=3, xpd = FALSE)
segments(as.Date('1998-09-01'),200 , as.Date('1999-12-01'), 200, col='red', lwd=3, xpd = FALSE)
segments(as.Date('2009-06-01'),200 , as.Date('2010-09-01'), 200, col='red', lwd=3, xpd = FALSE)


par(xpd=F)


# ======================================================================
# make this plot piece-wise for presentation figure
plot(abund_dat$censusdate[1:96],log(abund_dat$n[1:96]),xlab='',ylab='Log Total Abund.',pch=19,ylim=c(2,6),xlim=range(abund_dat$censusdate),cex.axis=1.5,cex.lab=1.5)
rect(xleft = as.Date('1983-08-01'),xright = as.Date('1983-11-01'),ytop = 250,ybottom=0,col='gray',border=NA)
lines(abund_dat$censusdate[1:96],log(abund_dat$n[1:96]))
points(abund_dat$censusdate[1:96],log(abund_dat$n[1:96]),pch=16)
box(which='plot')

# line segments showing changepoint 95% intervals
segments(as.Date('1983-12-01'),5.8 , as.Date('1984-07-01'), 5.8, col='red', lwd=3, xpd = FALSE)
segments(as.Date('1988-10-01'),5.8 , as.Date('1996-01-01'), 5.8, col='red', lwd=3, xpd = FALSE)
segments(as.Date('1998-09-01'),5.8 , as.Date('1999-12-01'), 5.8, col='red', lwd=3, xpd = FALSE)
segments(as.Date('2009-06-01'),5.8 , as.Date('2010-09-01'), 5.8, col='red', lwd=3, xpd = FALSE)
