#read data
setwd('C:\\Users\\gm3u13\\Desktop\\Chapter2\\1st paper\\data from linux')
lon <- readRDS('caribbean_lon')
lat <- readRDS('caribbean_lat')
esa2mean <- readRDS('esa2mean')#mean sst 1985-2022
esa2sd <- readRDS('esa2sd')
esa2meanunc <- readRDS('esa2meanunc')
esa2meanuncmap <- readRDS('esa2meanuncmap')
ctmean <- readRDS('ctmean')
ctsd <- readRDS('ctsd')
esa2ctdifsd <-  readRDS('esa2ctdifsd')
esa2ctdifmeants <- readRDS('esa2ctdifmeants')
esa2jan.bel <- readRDS('esa2jan.bel')
esa2aug.bel <- readRDS('esa2aug.bel')
esa2jan.flo <- readRDS('esa2jan.flo')
esa2aug.flo <- readRDS('esa2aug.flo')
ctjan.bel <- readRDS('ctjan.bel')
ctaug.bel <- readRDS('ctaug.bel')
ctjan.flo <- readRDS('ctjan.flo')
ctaug.flo <- readRDS('ctaug.flo')
esa2clim <- readRDS('esa2clim')
ctclim <- readRDS('ctclim')

#Belize================================================================
a<-which(lon<=-84 & lon>=-89)
b<-which(lat<=17 & lat>=15)
#florida===============================================================
a<-which(lon<=-79 & lon>=-84)
b<-which(lat<=26 & lat>=24)

lon<-lon[a]
lat<- lat[b]
lon<-lon[c]
lat<- lat[d]
coords<- read.csv('C:\\Users\\gm3u13\\Desktop\\Chapter2\\1st paper\\logger data\\logger.coords.csv')
#range(ctjan.flo,na.rm = T)

#maps 1985-2004 and 2005-2022 (update figure)
#esa2_1<-readRDS('esa2_1.rds')
#esa2_2<-readRDS('esa2_2.rds')
#ct_1<-readRDS('ct_1.rds')
#ct_2<-readRDS('ct_2.rds')

difesa2<-esa2_2-esa2_1
difct<-ct_2-ct_1
range(difesa2,na.rm = T)
mean(difesa2,na.rm = T)
sd(difesa2,na.rm = T)
range(difct,na.rm = T)
mean(difct,na.rm = T)
sd(difct,na.rm = T)

#Plot maps
library(prettymapr)
library(maps)
library(fields)
color2<-colorRampPalette(c('white','cyan','deepskyblue','blue','blue3'))(1054)
#color2<-colorRampPalette(c('purple','blue1','blue','white','red','red2','red3'))(1054)
color2<-colorRampPalette(c('white','blue','blue3','red','red2','red3'))(1054)
#color2<-colorRampPalette(c('white','blue','orange','orangered','red'))(1054)
par( mar=c(3,3.1,2,5))
A <- esa2mean-ctmean
image(lon,lat,A[a,b], xlab = "", ylab = "", cex=1.5,col=color2,las=1,cex.lab=1.5,cex.axis=1.3)
image.plot( zlim=c(min(A[a,b],na.rm=T),max(A[a,b],na.rm=T)), nlevel=64,legend.only=TRUE,col=color2,smallplot = c(.9,.93,.11,.92))
            #,axis.args = list(at = round(seq(-2,1,0.2),1), labels=round(seq(-2,1,0.2),1)))
map(add=TRUE,fill=T,col='chocolate')
#points(coords$Longitude,coords$Latitude,pch=4,cex=1.2,lwd=1.8)#loggers
text(coords$Longitude[8:9],coords$Latitude[8:9], c("In.",'Off.'),cex=1.3)
addnortharrow( pos="topright", padin=c(0.15,0.15), scale=0.8, lwd=1, border="black",
               cols=c("white","black"), text.col="black" )
addscalebar( plotepsg=NULL, widthhint=0.15, htin=0.05,padin=c(0.05,0.05), tick.cex=0.7, 
labelpadin=0.08, label.cex=0.8, pos="bottomleft" )
#rect(lon[34]-0.025, lat[22]-0.025, lon[34]+0.025, lat[22]+0.025,lwd=1, lty=1,border='blue')
#which(esa2meanuncmap>1)

#high resolution
setwd('C:\\Users\\gm3u13\\Desktop\\Chapter2\\1st paper\\Paper plots')
png(filename = "esa2dif.png", width = 1100, height = 1100)
par( mar=c(6,6,6,10))
A <-difesa2
image(lon,lat,A, xlab = "longitude", ylab = "latitude",col=color2,las=1,cex.lab=2,cex.axis=2)
image.plot( zlim=c(min(A,na.rm=T),max(A,na.rm=T)), nlevel=64,legend.only=TRUE, col=color2,legend.width=2.5,
      axis.args=list(cex.axis=2, at = round(seq(-3,2,0.5),1), labels=round(seq(-3,2,0.5),1)),smallplot = c(.9,.93,.1,.9))
map(add=TRUE,fill=T,col='chocolate')
points(coords$Longitude,coords$Latitude, pch=1,cex=2)#loggers
addnortharrow( pos="topright", padin=c(0.15,0.15), scale=1.5, lwd=1, border="black",
               cols=c("white","black"), text.col="black" )
addscalebar( plotepsg=NULL, widthhint=0.15, htin=0.15,padin=c(0.05,0.05),labelpadin=0.1, label.cex=1.5, 
             pos="bottomleft" )
dev.off()

#TS of mean difs==============
par(mar = c(0, 7.1, 3.1, 2.1))
plot(dates.ct,esa2ctdifmeants,type = 'l',xlab = '',ylab='',xaxt='n',lwd=2,las=1,cex.lab=1.8,cex.axis=1.8, 
     ylim = c(-1,1),col='blueviolet',axes=F)
axis(2,las=1,cex.lab=1.8,cex.axis=1.8)
mtext(side = 2,line=5,cex = 2,text = 'Mean SST difference (°C)')
abline(h=0,col='red',lty=2)
polygon(x=c(dates.ct,rev(dates.ct)),y=c(esa2ctdifmeants+esa2meanunc,rev(esa2ctdifmeants-esa2meanunc)),
        col=rgb(1,0,0,0.3),border=NA)

par(mar = c(4.1, 7.1, 20.1, 2.1))
dif<-esa2ctdifmeants-esa2meanunc
out<-which(dif>0)
dif[-out]<-0
plot(dates.ct,dif,type = 'l',xlab = '',ylab='',lwd=2,las=1,cex.lab=1.8,cex.axis=1.8, col='red',ylim = c(0,0.4))
mtext(side = 3,line=-1,cex = 1.5,text = 'Difference out of uncertainty (°C)')
#(dates.ct[which(esa2ctdifmeants>esa2meanunc)],col=rgb(1,0,0,0.2))


#@@@@@@@@@@Logger inter-comparison@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#==============================================================
#sat-log difs==================================================
setwd('C:\\Users\\gm3u13\\Desktop\\Chapter2\\1st paper\\data from linux')
dates.ct <- seq( as.POSIXct('1985-01-01',tz='UTC'), as.POSIXct('2022-12-31',tz='UTC'), by = '1 day')
esa2logs <- readRDS('esa2logs')
ctlogs <- readRDS('ctlogs')
esa2ct.df <- data.frame(dates.ct,esa2logs,ctlogs)

#Florida=============================
floind <- which(dates.ct%in%days.flo)
esa2ct.df.flo <-esa2ct.df[floind,]

daily.esa2flo<-esa2ct.df.flo[,2:8]-daily.flo.df[,2:8]
daily.ctflo<-esa2ct.df.flo[,11:17]-daily.flo.df[,2:8]

#Belize=============================
days.in <-seq( as.POSIXct('2002-06-14',tz='UTC'), as.POSIXct('2007-11-13',tz='UTC'), by = '1 day')
days.off <-seq( as.POSIXct('2002-07-12',tz='UTC'), as.POSIXct('2007-12-10',tz='UTC'), by = '1 day')

inind <- which(dates.ct%in%days.in)
esa2ct.df.in <-esa2ct.df[inind,]

daily.esa2in<-esa2ct.df.in[,9]-daily.in
daily.ctin<-esa2ct.df.in[,18]-daily.in

offind <- which(dates.ct%in%days.off)
esa2ct.df.off <-esa2ct.df[offind,]

daily.esa2off<-esa2ct.df.off[,10]-daily.off
daily.ctoff<-esa2ct.df.off[,19]-daily.off


#combine all loggers
daily.esa2in <- c(daily.esa2in, rep(NA,2964))
daily.esa2off <- c(daily.esa2off, rep(NA,2965))
daily.ctin <- c(daily.ctin, rep(NA,2964))
daily.ctoff <- c(daily.ctoff, rep(NA,2965))
daily.esa2logs <- data.frame(daily.esa2flo,daily.esa2in,daily.esa2off)
daily.ctlogs <- data.frame(daily.ctflo,daily.ctin,daily.ctoff)

#Boxplots of difs
par(mar = c(7, 5, 2, 1))
bx.esa2<-boxplot(daily.esa2logs,plot=F)
bx.ct<-boxplot(daily.ctlogs,plot=F)
bxp(bx.esa2,at=seq(1,9,1)-0.15,boxwex=0.2,las=1,border="blue",xaxt="n",ylab='Difference (°C)',cex.axis=1.5,cex.lab=1.5)
axis(side=1,at=c(1:9),labels = names(daily.df) ,cex.axis=1.5,las=2)
bxp(bx.ct,at=seq(1,9,1)+0.15,boxwex=0.2,border="red",add=T,xaxt="n",yaxt="n")
legend("topright",c("ESA2","CoralTemp"),text.col=c("blue","red"),bg='transparent',cex = 1.2)
abline(h=0,col="red",lty=2)
axis(side=3,col = NA, col.ticks = NA, at = c(1:9),labels = log.obs,
     line=-1,cex.axis=1.2)


#Stats table
esa2mean <-colMeans(daily.esa2logs,na.rm=T)
ctmean <- colMeans(daily.ctlogs,na.rm=T)
esa2sd <- apply(daily.esa2logs,2,sd,na.rm=T)
ctsd <- apply(daily.ctlogs,2,sd,na.rm=T)
esa2med <- apply(daily.esa2logs,2,median,na.rm=T)
ctmed <-apply(daily.ctlogs,2,median,na.rm=T)

esa2rmse.flo <- numeric(7)
ctrmse.flo <- numeric(7)
for (i in 2:8) {
  esa2rmse.flo[i-1] <- sqrt(sum((esa2ct.df.flo[,i]-daily.flo.df[,i])^2,na.rm=T)/log.obs[i-1])
  ctrmse.flo[i-1] <- sqrt(sum((esa2ct.df.flo[,i+9]-daily.flo.df[,i])^2,na.rm=T)/log.obs[i-1])
  }

esa2rmse.in <- sqrt(sum((esa2ct.df.in[,9]-daily.in)^2,na.rm=T)/log.obs[8])
esa2rmse.off <- sqrt(sum((esa2ct.df.off[,10]-daily.off)^2,na.rm=T)/log.obs[9])
ctrmse.in <- sqrt(sum((esa2ct.df.in[,18]-daily.in)^2,na.rm=T)/log.obs[8])
ctrmse.off <- sqrt(sum((esa2ct.df.off[,19]-daily.off)^2,na.rm=T)/log.obs[9])

stats <- c(esa2mean,ctmean,esa2med,ctmed,esa2sd,ctsd,esa2rmse.flo,ctrmse.flo,esa2rmse.in,
esa2rmse.off,ctrmse.in,ctrmse.off)

#write.csv(stats,'stats.table.csv')



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Monthly avgs -Climatologies-Daily anomalies
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Monthly sat+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
months <- seq( as.POSIXct('1985-01-01',tz='UTC'), as.POSIXct('2023-01-01',tz='UTC'),by='1 month')
binsat<-cut(dates.ct,breaks=months)
esa2month<-matrix(NA,length(months)-1,9)
ctmonth<-matrix(NA,length(months)-1,9)
for (i in 1:9){
esa2month[,i]<-tapply(esa2logs[,i],binsat,mean,na.rm=TRUE)
ctmonth[,i]<-tapply(ctlogs[,i],binsat,mean,na.rm=TRUE)}


esa2ct.df <- data.frame(months[1:456],esa2month,ctmonth)

#Florida=============================
floind <- which(months%in%months.flo)
esa2ct.df.flo <-esa2ct.df[floind,]

month.esa2flo<-esa2ct.df.flo[,2:8]-month.flo.df[,2:8]
month.ctflo<-esa2ct.df.flo[,11:17]-month.flo.df[,2:8]


#Time series of monthly difs (logger minus sat)++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
par(mar = c(3, 4.5, 2.5, 2))
plot(months.flo,month.esa2flo[,4],type = 'l',col='blue',main = 'Molasses',xlab = '',ylab='',las=1,ylim=c(-1,1.2),
     cex.main=1.8,xaxt='n',cex.lab=1.8,cex.axis=1.8)
axis(1,at=months.flo[seq(10,163,12)],labels =seq(2010,2022,1) ,cex.lab=1.8,cex.axis=1.8)
lines(months.flo,month.ctflo[,4],col='red')
abline(h=0, lty=2)
abline(h=c(-1,1), lwd=2,lty=4,col='red')
#abline(v=months.flo[c( 11,12, 22)],col=rgb(1,0,0,0.4), lwd=2,lty=4)
legend('bottomleft',legend = c('ESA2','CoralTemp'), col = c('blue','red'),lty = 1,cex=1.3)
#which(abs(month.esa2flo[,1])>=1)

#Belize month==================
months.in <- seq(as.POSIXct('2002-07-01',tz='UTC'),as.POSIXct('2007-10-01',tz='UTC'),by='1 month')
months.off <- seq(as.POSIXct('2002-08-01',tz='UTC'),as.POSIXct('2007-11-01',tz='UTC'),by='1 month')

inind <- which(months%in%months.in)
esa2ct.df.in <-esa2ct.df[inind,]

month.esa2in<-esa2ct.df.in[,9]-monthly.in
month.ctin<-esa2ct.df.in[,18]-monthly.in

offind <- which(months%in%months.off)
esa2ct.df.off <-esa2ct.df[offind,]

month.esa2off<-esa2ct.df.off[,10]-monthly.off
month.ctoff<-esa2ct.df.off[,19]-monthly.off

plot(months.off,month.esa2off,type = 'l',col='blue',main = 'Belize Offshore',xlab = '',ylab='',las=1,ylim=c(-1,1),
     cex.main=1.8,cex.lab=1.8,cex.axis=1.8)
lines(months.off,month.ctoff,col='red')
abline(h=c(0,1,-1),col=c('black','magenta','magenta'), lty=2)


#combine all loggers
month.esa2in <- c(month.esa2in, rep(NA,99))
month.esa2off <- c(month.esa2off, rep(NA,99))
month.ctin <- c(month.ctin, rep(NA,99))
month.ctoff <- c(month.ctoff, rep(NA,99))
month.esa2logs <- data.frame(month.esa2flo,month.esa2in,month.esa2off)
month.ctlogs <- data.frame(month.ctflo,month.ctin,month.ctoff)


#month difs stats
esa2mean <-colMeans(month.esa2logs,na.rm=T)
ctmean <- colMeans(month.ctlogs,na.rm=T)
esa2sd <- apply(month.esa2logs,2,sd,na.rm=T)
ctsd <- apply(month.ctlogs,2,sd,na.rm=T)
esa2med <- apply(month.esa2logs,2,median,na.rm=T)
ctmed <-apply(month.ctlogs,2,median,na.rm=T)

esa2rmse.flo <- numeric(7)
ctrmse.flo <- numeric(7)
for (i in 2:8) {
  esa2rmse.flo[i-1] <- sqrt(sum((esa2ct.df.flo[,i]-month.flo.df[,i])^2,na.rm=T)/163)
  ctrmse.flo[i-1] <- sqrt(sum((esa2ct.df.flo[,i+9]-month.flo.df[,i])^2,na.rm=T)/163)
}

esa2rmse.in <- sqrt(sum((esa2ct.df.in[,9]-monthly.in)^2,na.rm=T)/64)
esa2rmse.off <- sqrt(sum((esa2ct.df.off[,10]-monthly.off)^2,na.rm=T)/64)
ctrmse.in <- sqrt(sum((esa2ct.df.in[,18]-monthly.in)^2,na.rm=T)/64)
ctrmse.off <- sqrt(sum((esa2ct.df.off[,19]-monthly.off)^2,na.rm=T)/64)

stats <- c(esa2mean,ctmean,esa2med,ctmed,esa2sd,ctsd,esa2rmse.flo,ctrmse.flo,esa2rmse.in,
           esa2rmse.off,ctrmse.in,ctrmse.off)

#write.csv(stats,'monthstats.csv')

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Climatology/Annual cycle+++++++++++++++++++++++++++++++++
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
month_ind <- as.numeric( format.POSIXct( months[-457], '%m') )
month.clim.df <- data.frame(esa2month,ctmonth,month_ind)
month_clim <- aggregate(month.clim.df,by=list(month_ind),FUN = 'mean',na.rm=T)

#extract max and min of clim
max.clim2 <- numeric(9)
min.clim2 <- numeric(9)
for (i in 1:9){
  max.clim2[i] <- which.max(month_clim[,i+10])
  min.clim2[i] <- which.min(month_clim[,i+10])
}

#max and min clim difs (esa-ct)
rng.dif <- matrix(NA,2,9)
for (i in 1:9){
  rng.dif[,i] <- range(month_clim[,i+1]) - range(month_clim[,i+10])
}

sites <-c('Fowey','Molasses','Crocker','Sombrero','Pulaski','Pulaski W.','Garden K.','Belize Inshore'
          ,'Belize Offshore')
par(mar = c(7, 6, 2.5, 2))
plot(1:9,rng.dif[1,],col='blue',pch=16,xlab = '',ylab='',xaxt='n',las=1,ylim=c(-1,1),cex.axis=1.8,cex=1.5)
points(1:9,rng.dif[2,],col='red',pch=16,cex=1.5)
axis(side=1,at=c(1:9),labels = sites ,cex.axis=1.5,las=2)
abline(h=0, lty=2,col='magenta')
mtext('Difference (°C)',side = 2,line = 4.5,cex=1.8)
legend('bottomleft',legend = c('Min climatology','Max climatology'), col = c('blue','red'),pch =16,cex=1.3)


#Anomalies======
esa2anom <- matrix(NA,length(dates.ct),9)
ctanom <- matrix(NA,length(dates.ct),9)
for (i in 1:length(dates.ct)) {
  for (j in 1:9) {
  mm <- format.POSIXct(dates.ct[i],'%m')
  mm <- as.numeric(mm)
  esa2anom[i,j] <- esa2logs[i,j] - month_clim[mm,j+1]
  ctanom[i,j] <- ctlogs[i,j] - month_clim[mm,j+10]
  }
}

anom.dif <- esa2anom-ctanom

ind <- c(1,3,4,5,8,9)
png(filename = "anom.difs.png", height=1100,width=1100)
par(mfrow = c(3, 2))
par(mar = c(3, 4, 2.5, 2))
for (i in ind){
plot(dates.ct,anom.dif[,i],type = 'l',col='blue',main = sites[i],xlab = '',ylab='',las=1,ylim=c(-3,3),
     cex.main=1.8,cex.lab=1.8,cex.axis=1.8)
#lines(dates.ct,ctanom[,1],col='red')
abline(h=0, lty=2,col='red')}

dev.off()



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Climatology and daily anomalies at 3 sites logger and sat for the periods stated below
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
months.fow <- months.flo[-c(1:5,151:163)]#09/2009-09/2021
#months.som <- months.flo[-c(1:4,162:163)]#08/2009-08/2022
#months.pul <- months.flo[4:163)]#07/2009-10/2022
month_ind <- as.numeric( format.POSIXct( months.fow, '%m') )
fow_clim <- aggregate(month.flo.df[-c(1:5,151:163),2],by=list(month_ind),FUN = 'mean',na.rm=T)

fowind <- which(months%in%months.fow)
esa2ct.df.fow <-esa2ct.df[fowind,c(1,2,11)]
esa2ctfow_clim <- aggregate(esa2ct.df.fow[,2:3],by=list(month_ind),FUN = 'mean',na.rm=T)

par(mar = c(3, 6, 2.5, 2))
names<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
plot(1:12,esa2ctfow_clim[,2],col='blue',xlab = '',ylab='',xaxt='n',las=1,cex.axis=1.8,type='l',
     main='Fowey',cex.main=2)
lines(1:12,esa2ctfow_clim[,3],col='red')
lines(1:12,fow_clim[,2],lwd=2)
mtext('SST (°C)',side=2,cex=1.8,line=4)
axis(side=1,at=c(1:12),labels = names ,cex.axis=1.5,las=1)
legend('bottomright',legend = c('ESA2','CT','Logger'), col = c('blue','red','black'),lty = 1,
       lwd=c(1,1,2),cex=1.3,bg='transparent')

a<-max(esa2ctfow_clim[,2])-max(fow_clim[,2])
b<-min(esa2ctfow_clim[,2])-min(fow_clim[,2])
c<-max(esa2ctfow_clim[,3])-max(fow_clim[,2])
d<-min(esa2ctfow_clim[,3])-min(fow_clim[,2])
#write.csv(c(a,b,c,d),file = 'fow.maxmin.csv')

#daily anomalies
anom.fow <- numeric(length(days.flo))

for (i in 1:length(days.flo)) {
  mm <- format.POSIXct(days.flo[i],'%m')
  mm <- as.numeric(mm)
  anom.fow[i] <- daily.fow[i] - fow_clim[mm,2]
}

floind <- which(dates.ct%in%days.flo)
esa2fow.anom <- esa2anom[floind,1]
ctfow.anom <- ctanom[floind,1]

esa2fow.anom.dif <- esa2fow.anom-anom.fow
ctfow.anom.dif <- ctfow.anom-anom.fow

plot(days.flo,esa2fow.anom.dif,type = 'l',xlab = '',ylab='',las=1,cex.lab=1.8,cex.axis=1.8, col='blue',
     main='Fowey',cex.main=2)
lines(days.flo,ctfow.anom.dif,col='red')
mtext(side = 2,line=4,cex = 1.8,text = 'Anomaly Difference (°C)')
abline(h=c(-1,1), lty=2)
legend('topright',legend = c('ESA2','CT'), col = c('blue','red'),lty = 1,cex=1.3)


#stats for anom difs
esaanomdif.m <- mean(esa2fow.anom.dif,na.rm = T)
ctanomdif.m <- mean(ctfow.anom.dif,na.rm = T)
esaanomdif.sd <- sd(esa2fow.anom.dif,na.rm = T)
ctanomdif.sd <- sd(ctfow.anom.dif,na.rm = T)

hspotsesa2 <- which(abs(esa2fow.anom.dif)>=1)
hs.nesa2 <- length(hspotsesa2)#number of hs
result <- rle(diff(hspotsesa2))
dhwesa2 <- which(result$lengths>=7 & result$values==1)
dhwesa2 <- length(dhwesa2)#no of >7 consecutive days

#mark DHWs=========
#lines(days.flo[3873:3881],esa2fow.anom.dif[3873:3881],lwd=5,col='green')
#lines(days.flo[307:314],esa2fow.anom.dif[307:314],lwd=5,col='green')
#consec. days >1: 307:314,921:928,3873:3881

hspotsct <- which(abs(ctfow.anom.dif)>=1)
hs.nct <- length(hspotsct)#number of hs
result <- rle(diff(hspotsct))
dhwct <- which(result$lengths>=7 & result$values==1)
dhwct <- length(dhwct)#no of >7 consecutive days
#304:312
#write.csv(c(esaanomdif.m,esaanomdif.sd,ctanomdif.m,ctanomdif.sd,hs.nesa2,dhwesa2,hs.nct,dhwct),'anomalystats.csv')



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@'
#Linear trends
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#monthly anomalies ref. 1985-2012
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
month_ind <- as.numeric( format.POSIXct( months[1:324], '%m') )
month.clim.df <- data.frame(esa2month[1:324,],ctmonth[1:324,],month_ind)
month_clim <- aggregate(month.clim.df,by=list(month_ind),FUN = 'mean',na.rm=T)

esa2anom <- matrix(NA,length(dates.ct),9)
ctanom <- matrix(NA,length(dates.ct),9)
for (i in 1:length(dates.ct)) {
  for (j in 1:9) {
    mm <- format.POSIXct(dates.ct[i],'%m')
    mm <- as.numeric(mm)
    esa2anom[i,j] <- esa2logs[i,j] - month_clim[mm,j+1]
    ctanom[i,j] <- ctlogs[i,j] - month_clim[mm,j+10]
  }
}

par(mar = c(3, 5, 2.5, 2))
plot(dates.ct,esa2anom[,9],type = 'l',col='blue',main = sites[9],xlab = '',ylab='',las=1,ylim=c(-3,3),
       cex.main=1.8,cex.lab=1.8,cex.axis=1.8,xaxt='n')
axis(1,at=months[seq(1,456,60)],labels =seq(1985,2022,5) ,cex.lab=1.8,cex.axis=1.8)
mtext(side = 2,line=3,cex = 2,text = 'SST Anomaly (°C)')
lines(dates.ct,ctanom[,9],col='red')
abline(h=0, lty=2)
#legend('topleft',legend = c('ESA2','CT'), col = c('blue','red'),lty = 1,cex=1.3,bg='transparent')
#saveRDS(ctanom,file='ctanom.rds')


esa2anom <- readRDS('esa2anom.rds')
ctanom <- readRDS('ctanom.rds')
esa2ctanom <- esa2anom-ctanom
colMeans(esa2ctanom)
apply(esa2ctanom,2,sd)
#esa minus ct anom. for 1985-2012 climatology
ind <- c(1,3,4,5,8,9)
png(filename = "anom.difs2.png", height=1100,width=1100)
par(mfrow = c(3, 2))
par(mar = c(3, 4, 2.5, 2))
for (i in ind){
  plot(dates.ct,esa2ctanom[,i],type = 'l',col='blue',main = sites[i],xlab = '',ylab='',las=1,ylim=c(-3,3),
       cex.main=1.8,cex.lab=1.8,cex.axis=1.8)
  #lines(dates.ct,ctanom[,1],col='red')
  abline(h=0, lty=2,col='red')}

dev.off()


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Linear trends=====================================
esa2trend.in <- readRDS('esa2trend.in')
cttrend.in <- readRDS('cttrend.in')
a<-esa2trend.in[1]
b<-esa2trend.in[2]
c<-cttrend.in[1]
d<-cttrend.in[2]
days <- seq(1,40,length.out=13879)

plot(days,esa2anom[,8],type = 'l',col=rgb(0,0,1,0.5),main = sites[8],xlab = '',ylab='',las=1,ylim=c(-3,3),
     cex.main=1.8,cex.lab=1.8,cex.axis=1.8,xaxt='n')
axis(1,at=seq(1,40,5),labels =seq(1985,2022,5) ,cex.axis=1.8)
mtext(side = 2,line=3,cex = 2,text = 'SST Anomaly (°C)')
lines(days,ctanom[,8],col=rgb(1,0,0,0.5))
abline(h=0, lty=2)
abline(a,b,lwd=3,lty=6,col='blue2')
abline(c,d,lwd=3,lty=4,col='green2')
legend('topleft',legend = c('ESA2','CT'), col = c('blue','red'),lty = 1,cex=1.3,bg='transparent')
legend('bottomright',legend = c('ESA2 trend','CT trend'), col = c('blue2','green2'),
       lty = c(6,4) , lwd=3,cex=1.3,bg='transparent')

#need todo it in linux! look below
#polygon(x=c(days,rev(days)),y=c(low,rev(high)),
        #col=rgb(1,0,0,0.3),border='red')

trends <- c(esa2trend.fow,esa2trend.som,esa2trend.in,esa2trend.off,
            cttrend.fow,cttrend.som,cttrend.in,cttrend.off)
#write.csv(trends,'trends.csv')

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#this had to be done in Linux due to memory
esa2ts.in <- ts(esa2anom[,8], frequency=365)
ctts.in <- ts(ctanom[,8], frequency=365)

library(nlme)
#ESA2=============================
dat <- data.frame(sst = as.numeric(esa2ts.in), Year = as.numeric(time(esa2ts.in)),
                  Time = seq_len(length(esa2ts.in)))

fit.gls <- gls(sst~Year, data=dat, correlation=corARMA(p=1), method="ML",control = list(singular.ok = TRUE))
summary(fit.gls)

#CT=============================
dat2 <- data.frame(sst = as.numeric(ctts.in), Year = as.numeric(time(ctts.in)),
                  Time = seq_len(length(ctts.in)))

fit.gls2 <- gls(sst~Year, data=dat2, correlation=corARMA(p=1), method="ML",control = list(singular.ok = TRUE))
summary(fit.gls2)


acf(resid(fit.gls), main="ACF of GLS-Residuals") #should look as AR(1) (Arma in general)
pacf(resid(fit.gls), main="PACF of GLS-Residuals")

summary(fit.gls)
anova(fit.gls)


plot(sst ~ Year, data = dat, type = "l",las=1,cex.lab=1.8,cex.axis=1.8,ylab = 'SST Anomaly (°C)',
main = 'Belize Inshore', cex.main=1.8,xlab='',col='blue',ylim=c(-3,3),xaxt='n')
axis(1,at=seq(1,40,5),labels =seq(1985,2022,5) ,cex.lab=1.8,cex.axis=1.8)
lines(sst ~ Year, data = dat2,col='green4')
lines(fitted(fit.gls2) ~ Year, data = dat2, col = "red",lty=4,lwd=2)
lines(fitted(fit.gls) ~ Year, data = dat,lwd=2,lty=6,col='cyan')
legend('topleft',legend = c('CoralTemp','ESA CCI2','trend CT','trend ESA2'),
       col = c('green4','blue','red','black'),lty=c(1,1,4,6),bg="transparent",cex=1.2)


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Belize in uncertainty
esa2unc.in <- readRDS('esa2unc.in')
par(mar = c(0, 7.1, 3.1, 2.1))
plot(dates.ct,esa2anom[,8],type = 'l',xlab = '',ylab='',lwd=2,las=1,cex.lab=1.8,cex.axis=1.8, 
  col='blueviolet')
axis(2,las=1,cex.lab=1.8,cex.axis=1.8)
mtext(side = 2,line=5,cex = 2,text = 'Mean SST difference (°C)')
abline(h=0,col='red',lty=2)
polygon(x=c(dates.ct,rev(dates.ct)),y=c(esa2anom[,8]+esa2unc.in,rev(esa2anom[,8]-esa2unc.in)),
        col=rgb(1,0,0,0.3),border=NA)
length(which(esa2anom[,8]-esa2unc.in>0))
       
boxplot(esa2unc.in[1:3650],xlab = '',ylab='',lwd=2,las=1,cex.lab=1.8,cex.axis=1.8, 
     col=rgb(0,1,0,0.5))
boxplot(esa2unc.in[10000:13650],xlab = '',ylab='',lwd=2,las=1,cex.lab=1.8,cex.axis=1.8, 
        add=T,col = rgb(0,0,1,0.5))
hist(esa2unc.in)
plot(dates.ct[1:3650],esa2unc.in[1:3650],xlab = '',ylab='',lwd=2,las=1,cex.lab=1.8,cex.axis=1.8, 
     col='blueviolet')
abline(v=dates.ct[which(esa2unc.in<0.7)])

summary(esa2unc.in)
sd(esa2unc.in)

years <- seq( as.POSIXct('1985-01-01',tz='UTC'), as.POSIXct('2023-01-01',tz='UTC'),by='1 year')
binsat<-cut(dates.ct,breaks=years)
esa2year<-numeric(length(years)-1)


esa2year<-tapply(esa2unc.in,binsat,mean,na.rm=TRUE)
