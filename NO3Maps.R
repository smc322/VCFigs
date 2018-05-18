##use TP maps script for NO3 maps, so read in different file but otherwise keep labels

require(ggplot2)
library(RColorBrewer)
library(maps)
require(akima)
require(rgdal)
library(dplyr)

## HU4polygon data from Sam's script
# get huc4 polygons from github, which was used as the regional spatial extent
# may not work on macs - link to data:
# https://github.com/limnoliver/CSI-Nutrient-Time-Series/tree/72c8269902e53c7ec6a2cfbe13a0239d13062dc8/Data

load(url("https://github.com/limnoliver/CSI-Nutrient-Time-Series/blob/72c8269902e53c7ec6a2cfbe13a0239d13062dc8/Data/huc4.RData?raw=true"))

#read data and calc regional and lake median/cv of TP

setwd("~/Dropbox/CSI&CL/CSI_LIMNO_Manuscripts-presentations/CSI_var-components/VarComp_data")

data<-read.csv("no3_dat.csv", header=T)

#only need hu4 identifier, lagoslakeid, date, response variable from this

data.lake <- data[, c("lagoslakeid", "no2no3", "sampleyear")]
data.reg <- data[,c("hu4_zoneid", "lagoslakeid", "no2no3", "sampleyear")]


#make median response by lake, first median for each year, then median of those
lakeyrmed<- data.lake %>% group_by(lagoslakeid, sampleyear) %>% summarise(ly.med=median(no2no3))
cv <- function(x) {return(sd(x)/mean(x))}
lakemed<- lakeyrmed %>% group_by(lagoslakeid) %>% summarise(l.med=median(ly.med), l.cv=cv(ly.med), count=n())

#only keep lakes with minimally 2 years of response values, calc cv of response by lake for lakes with 5 years
#define cv
cvs.lake<- filter(lakemed, count>1)
nocv.lake<-filter(lakemed, count<2)
cvs.lake$cv.map <- TRUE
nocv.lake$cv.map <- FALSE

lakemaps<-rbind(cvs.lake, nocv.lake)

#same thing for regions, except make lake yr med, then lake med, then reg med with count and CV if at least 5 lakes per hu4.
hu4lakeyrmed<- data.reg %>% group_by(hu4_zoneid, lagoslakeid, sampleyear) %>% summarise(ly.med=median(no2no3))
hu4lakemed<- hu4lakeyrmed %>% group_by(hu4_zoneid, lagoslakeid) %>% summarise(l.med=median(ly.med))
hu4med<- hu4lakemed %>% group_by(hu4_zoneid) %>% summarise(reg.med=median(l.med), reg.cv=cv(l.med), count=n())

cv.reg<- filter(hu4med, count>4)
nocv.reg<- filter(hu4med, count<5)

cv.reg$cv.map <-TRUE
nocv.reg$cv.map <-FALSE

regmaps<-rbind(cv.reg, nocv.reg)

##lakemaps and regmaps have information for making both median and cv maps for lakes and regions, respectively. add lat and long from lagos for the lakes file since those weren't in there.

library(LAGOS)
lagos<-lagos_load("1.087.1")
ll.l<-lagos$locus[,c(1,4,5)]

lmcords<-left_join(lakemaps, ll.l, by="lagoslakeid")

#merge regional data with shapefile from sam that got loaded at beginning
names(regmaps)[1]<-"ZoneID"
huc4 = merge(huc4, regmaps, by = "ZoneID", all.x = TRUE)

colors =  c(rgb(255,255,182,max=255),
            rgb(199,233,180,max=255),
            rgb(127,205,187,max=255),
            rgb(65,182,196,max=255),
            rgb(29,145,192,max=255),
            rgb(34,94,168,max=255),
            rgb(12,44,132,max=255))

#purple blue green
# #colors =  c(rgb(246,239,247,max=255),
#             rgb(208,209,230,max=255),
#             rgb(166,189,219,max=255),
#             rgb(103,169,207,max=255),
#             rgb(54,144,192,max=255),
#             rgb(2,129,138,max=255),
#             rgb(1,100,80,max=255))


get.col.bins.med <- function(slopes, alpha=255) {
  z=slopes
#  quants<-quantile(z, probs = c(.1, .2, .3, .4, .5, .6, .7, .8, .9), na.rm=T)
  
  #ii <- cut(z, breaks = c(-Inf, quants, Inf), 
  #          include.lowest = TRUE)
  
  ii <- cut(z, breaks = c(0,15, 30, 45, 60, 75, 90,Inf), 
            include.lowest = TRUE)
  
  #purple blue green
  # levels(ii) <- c(rgb(246,239,247,max=255, alpha=alpha),
  #                 rgb(208,209,230,max=255, alpha=alpha),
  #                 rgb(166,189,219,max=255, alpha=alpha),
  #                 rgb(103,169,207,max=255, alpha=alpha),
  #                 rgb(54,144,192,max=255, alpha=alpha),
  #                 rgb(2,129,138,max=255, alpha=alpha),
  #                 rgb(1,100,80,max=255, alpha=alpha))
  #yellow green blue
  levels(ii) <- c(rgb(255,255,182,max=255, alpha=alpha),
                  rgb(199,233,180,max=255, alpha=alpha),
                  rgb(127,205,187,max=255, alpha=alpha),
                  rgb(65,182,196,max=255, alpha=alpha),
                  rgb(29,145,192,max=255, alpha=alpha),
                  rgb(34,94,168,max=255, alpha=alpha),
                  rgb(12,44,132,max=255, alpha=alpha))

  
  ii = as.character(ii)
  ii[is.na(ii)==TRUE] <- rgb(255,255,255,max=255)
  return(ii)
}

get.col.bins.cv <- function(slopes, alpha=255) {
  z=slopes
  #  quants<-quantile(z, probs = c(.1, .2, .3, .4, .5, .6, .7, .8, .9), na.rm=T)
  
  #ii <- cut(z, breaks = c(-Inf, quants, Inf), 
  #          include.lowest = TRUE)
  
  ii <- cut(z, breaks = c(0,.41,.83,1.25,1.67,2.08,2.5,Inf), 
            include.lowest = TRUE)
  
  #purple bluegreen
  # levels(ii) <- c(rgb(246,239,247,max=255, alpha=alpha),
  #                 rgb(208,209,230,max=255, alpha=alpha),
  #                 rgb(166,189,219,max=255, alpha=alpha),
  #                 rgb(103,169,207,max=255, alpha=alpha),
  #                 rgb(54,144,192,max=255, alpha=alpha),
  #                 rgb(2,129,138,max=255, alpha=alpha),
  #                 rgb(1,100,80,max=255, alpha=alpha))
  
  levels(ii) <- c(rgb(255,255,182,max=255, alpha=alpha),
                  rgb(199,233,180,max=255, alpha=alpha),
                  rgb(127,205,187,max=255, alpha=alpha),
                  rgb(65,182,196,max=255, alpha=alpha),
                  rgb(29,145,192,max=255, alpha=alpha),
                  rgb(34,94,168,max=255, alpha=alpha),
                  rgb(12,44,132,max=255, alpha=alpha))
  ii = as.character(ii)
  ii[is.na(ii)==TRUE] <- rgb(255,255,255,max=255)
  return(ii)
}

setwd("~/Documents/LocalGitProjects/VarCompFigures")


png("no2no3_meanlegend.png", width=1.5, height=.5, units='in', res=300)
par(mar = c(0,0,0,0))
plot.new()
points(x = seq(from = 0.1, to = 0.9, by = (.8/6)), y= rep(.5,7), pch = 22, cex = 2.75, bg = colors, col="grey30")
#text(x = seq(from = .1, to = .9, by = (.8/9)), y = rep(.23,10), round(quantile(lmcords$l.med, probs = c(0.05, .15, .25, .35, .45, .55, .65, .75, .85, .95), na.rm=T)), cex = .5)
text(x = seq(from = .1, to = .9, by = (.8/6)), y = rep(.2,7), labels=c("15", "30", "45", "60", "75", "90", ">90"), cex = .5)
text(x=.5, y = .85, labels = "Median NO3", cex = .5)
dev.off()

png("no2no3_cvlegend.png", width=1.5, height=.5, units = 'in', res=300)
par(mar = c(0,0,0,0))
plot.new()
points(x = seq(from = 0.1, to = 0.9, by = (.8/6)), y= rep(.5,7), pch = 22, cex = 2.75, bg = colors, col="grey30")
text(x = seq(from = .1, to = .9, by = (.8/6)), y = rep(.2,7), labels=c("0.4", "0.8", "1.3", "1.7", "2.1", "2.5", ">2.5"), cex = .5)
text(x=.5, y = .85, labels = "CV of NO3", cex = .5)
dev.off()

# png("no2no3_regmeanlegend.png", width=2, height=.5, units = 'in', res=300)
# par(mar = c(0,0,0,0))
# plot.new()
# points(x = seq(from = 0.1, to = 0.9, by = (.8/9)), y= rep(.5,10), pch = 15, cex = 2.2, col = colors)
# text(x = seq(from = .1, to = .9, by = (.8/9)), y = rep(.23,10), round(quantile(huc4$reg.med, probs = c(0.05, .15, .25, .35, .45, .55, .65, .75, .85, .95), na.rm=T)), cex = .5)
# text(x=.5, y = .85, labels = "Regional Median no2no3", cex = .5)
# dev.off()

# png("no2no3_regcvlegend.png", width=2, height=.5, units = 'in', res=300)
# par(mar = c(0,0,0,0))
# plot.new()
# points(x = seq(from = 0.1, to = 0.9, by = (.8/9)), y= rep(.5,10), pch = 15, cex = 2.2, col = colors)
# text(x = seq(from = .1, to = .9, by = (.8/9)), y = rep(.23,10), round(quantile(huc4$reg.cv, probs = c(0.05, .15, .25, .35, .45, .55, .65, .75, .85, .95), na.rm=T), digits=1), cex = .5)
# text(x=.5, y = .85, labels = "Regional CV no2no3", cex = .5)
# dev.off()

pdf("no2no3_maps.pdf",width = 8,height = 5)
par(mfcol=c(2,2), cex = 1)
par(mar = c(0,0,0,0))

#plot lakemeans
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE, col="grey30",lwd=1,mar=c(0,0,1,0),oma=c(0,0,0,0))
points(lmcords$nhd_long, lmcords$nhd_lat, bg = get.col.bins.med(lmcords$l.med, 150), pch = 21, col = rgb(76, 76, 76, max=255, alpha=150) ,lwd = .3, cex=.7)

#plot mean of regions
plot(huc4, lty = 1, lwd=1,border="grey30",mar=c(0,0,3,1),oma=c(0,0,0,0))
plot(huc4, col=get.col.bins.med(huc4$reg.med), lty = 1, lwd=1,border="grey30", add = TRUE)
plot(huc4[which(is.na(huc4$cv.map)),], col = "grey30", density = 32,lty = 1, lwd=1,border="grey30", add = TRUE)

#plot lakecvs
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE, col="grey30", lwd=1,mar=c(0,0,1,0),oma=c(0,0,0,0))
points(lmcords$nhd_long[which(lmcords$cv.map==FALSE)], lmcords$nhd_lat[which(lmcords$cv.map==FALSE)],bg = rgb(220,220,220,max=255,alpha=100), col = rgb(180,180,180,max=255,alpha=100), pch=21, cex=.7, lwd=.01)
points(lmcords$nhd_long[which(lmcords$cv.map==TRUE)], lmcords$nhd_lat[which(lmcords$cv.map==TRUE)], 
       bg = get.col.bins.cv(lmcords$l.cv[which(lmcords$cv.map==TRUE)], 150), 
       pch = 21, col = rgb(76, 76, 76, max=255, alpha=150) ,lwd = .3, cex=.7)

#plot CV of regions
plot(huc4, lty = 1, lwd=1,border=TRUE,mar=c(0,0,3,1),oma=c(0,0,0,0))
plot(huc4[which(huc4$cv.map==FALSE),],col="lightgray", lty = 1, lwd=1,border="grey30",mar=c(0,0,3,1),oma=c(0,0,0,0), add = TRUE)
plot(huc4[which(huc4$cv.map==TRUE),],col=get.col.bins.cv(huc4$reg.cv)[which(huc4$cv.map==TRUE)], lty = 1, lwd=1,border="grey30", add = TRUE)
plot(huc4[which(is.na(huc4$cv.map)),], col = "grey30", density = 32,lty = 1, lwd=1,border="grey30", add = TRUE)

dev.off()