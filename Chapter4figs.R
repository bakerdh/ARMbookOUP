# set outputplot to 2 for saving as PDF (change to 1 if you want EPS)
outputplot <- 2
# set colours for all plots
pal2tone <- c('#8783CF','#10069F','#CFCDEC')  # blue 072

# Chapter 4 figures ---------------------------------------------------------------
chapter <- 4
figno <- 1

if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

DBH <- c(6.39,6.19,6.88,6.69,6.48,7.18,6.79,7.08,7.19,7.48,7.58,8.19,7.79,8.18,8.29,8.69,8.89,8.6,9.18,9.48,9.29,9.79,10.39,10.48,9.79,9.71,10.08,10.40,10.69,10.79)
culmheight <- c(7.92,8.39,8.03,8.74,9.06,8.33,9.37,9.54,10.11,9.64,9.44,9.74,10.29,10.32,10.09,10.96,11.16,12.35,11.93,11.38,12.32,11.54,11.66,12.25,12.87,13.13,13.25,13.05,13.50,13.35)
bamboodata <- data.frame(DBH,culmheight)

plot(DBH,culmheight,type='p',pch=21,xlim=c(5,12),ylim=c(7,14),xlab='Diameter at Breast Height (cm)',ylab='Culm Height (m)',bg=pal2tone[1])

bamboolm <- lm(culmheight ~ DBH, data=bamboodata)
xvals <- 6:11
lines(xvals,bamboolm$coefficients[1] + bamboolm$coefficients[2]*xvals,lwd=3)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)




figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 9)}

par(mfrow=c(1,2), las=1)

plot(x=NULL,y=NULL,xlim=c(5,12),ylim=c(7,14),xlab='Diameter at Breast Height (cm)',ylab='Culm Height (m)',main='Null model')
nullmodel <- lm(culmheight ~ 1, data=bamboodata)
for (n in 1:nrow(bamboodata)){lines(bamboodata$DBH[c(n,n)],c(bamboodata$culmheight[n],nullmodel$coefficients[1]),col='grey')}
lines(xvals,nullmodel$coefficients[1] + 0*xvals,lwd=3)
points(DBH,culmheight,pch=21,bg=pal2tone[1])

plot(x=NULL,y=NULL,xlim=c(5,12),ylim=c(7,14),xlab='Diameter at Breast Height (cm)',ylab='Culm Height (m)',main='Alternative model')
for (n in 1:nrow(bamboodata)){lines(bamboodata$DBH[c(n,n)],c(bamboodata$culmheight[n],bamboolm$coefficients[1] + bamboolm$coefficients[2]*bamboodata$DBH[n]),col='grey')}
lines(xvals,bamboolm$coefficients[1] + bamboolm$coefficients[2]*xvals,lwd=3)
points(DBH,culmheight,pch=21,bg=pal2tone[1])

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

sizegroup <- 0*(1:30)   # make an empty vector of zeros to store the group labels
sizegroup[which(bamboodata$DBH<8.5)] <- 1 # set the narrow plants to be in group 1
sizegroup[which(bamboodata$DBH>8.5)] <- 2 # set the wide plants to be in group 2
bamboodata$sizegroup <- as.factor(sizegroup)

group1 <- bamboodata$culmheight[bamboodata$sizegroup==1]
group2 <- bamboodata$culmheight[bamboodata$sizegroup==2]

barplot(c(mean(group1),mean(group2)),ylim=c(0,15),ylab='Culm Height (m)',xlab='Group')
mtext(text = c('Narrow (1)', 'Wide (2)'), side = 1, at=c(0.7,1.9))     # add the tick labels
arrows(0.7,mean(group1),x1=0.7, y1=mean(group1)-sd(group1), length=0.015, angle=90, lwd=2)
arrows(0.7,mean(group1),x1=0.7, y1=mean(group1)+sd(group1), length=0.015, angle=90, lwd=2)
arrows(1.9,mean(group2),x1=1.9, y1=mean(group2)-sd(group2), length=0.015, angle=90, lwd=2)
arrows(1.9,mean(group2),x1=1.9, y1=mean(group2)+sd(group2), length=0.015, angle=90, lwd=2)

points(0.7,mean(group1),pch=16,cex=2)
points(1.9,mean(group2),pch=16,cex=2)

points(0.4+(1:15)/25,group1,pch=21,bg=pal2tone[1])
points(1.6+(1:15)/25,group2,pch=21,bg=pal2tone[1])

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)




figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 9)}

par(mfrow=c(1,2), las=1)

barplot(c(mean(group1),mean(group2)),ylim=c(0,15),ylab='Culm Height (m)',main='Null model')
arrows(0.7,mean(group1),x1=0.7, y1=mean(group1)-sd(group1), length=0.015, angle=90, lwd=2)
arrows(0.7,mean(group1),x1=0.7, y1=mean(group1)+sd(group1), length=0.015, angle=90, lwd=2)
arrows(1.9,mean(group2),x1=1.9, y1=mean(group2)-sd(group2), length=0.015, angle=90, lwd=2)
arrows(1.9,mean(group2),x1=1.9, y1=mean(group2)+sd(group2), length=0.015, angle=90, lwd=2)
points(0.7,mean(group1),pch=16,cex=2)
points(1.9,mean(group2),pch=16,cex=2)
grandmean <- mean(bamboodata$culmheight)
for (n in 1:length(group1)){lines(c(0.4+n/25,0.4+n/25),c(group1[n],grandmean))}
for (n in 1:length(group2)){lines(c(1.6+n/25,1.6+n/25),c(group2[n],grandmean))}
points(0.4+(1:15)/25,group1,pch=21,bg=pal2tone[1])
points(1.6+(1:15)/25,group2,pch=21,bg=pal2tone[1])
lines(c(0.4,2.2),c(grandmean,grandmean),col='black',lwd=3)


barplot(c(mean(group1),mean(group2)),ylim=c(0,15),ylab='Culm Height (m)',main='Alternative model')
arrows(0.7,mean(group1),x1=0.7, y1=mean(group1)-sd(group1), length=0.015, angle=90, lwd=2)
arrows(0.7,mean(group1),x1=0.7, y1=mean(group1)+sd(group1), length=0.015, angle=90, lwd=2)
arrows(1.9,mean(group2),x1=1.9, y1=mean(group2)-sd(group2), length=0.015, angle=90, lwd=2)
arrows(1.9,mean(group2),x1=1.9, y1=mean(group2)+sd(group2), length=0.015, angle=90, lwd=2)
points(0.7,mean(group1),pch=16,cex=2)
points(1.9,mean(group2),pch=16,cex=2)
for (n in 1:length(group1)){lines(c(0.4+n/25,0.4+n/25),c(group1[n],mean(group1)))}
for (n in 1:length(group2)){lines(c(1.6+n/25,1.6+n/25),c(group2[n],mean(group2)))}
points(0.4+(1:15)/25,group1,pch=21,bg=pal2tone[1])
points(1.6+(1:15)/25,group2,pch=21,bg=pal2tone[1])
lines(c(0.7,1.9),c(mean(group1),mean(group2)),col='black',lwd=3)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)





figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 9)}

par(mfrow=c(1,2), las=1)

DBHgroup <- 0*(1:30)   # make an empty vector of zeros to store the group labels
DBHgroup[which(bamboodata$DBH<7)] <- 1 # set the narrow plants to be in group 1
DBHgroup[which(bamboodata$DBH>7 & bamboodata$DBH<8)] <- 2 # set the wide plants to be in group 2
DBHgroup[which(bamboodata$DBH>8 & bamboodata$DBH<9)] <- 3 # set the wide plants to be in group 2
DBHgroup[which(bamboodata$DBH>9 & bamboodata$DBH<10)] <- 4 # set the wide plants to be in group 2
DBHgroup[which(bamboodata$DBH>10)] <- 5 # set the narrow plants to be in group 1
bamboodata$DBHgroup <- as.factor(DBHgroup)

grandmean <- mean(bamboodata$culmheight)
groupmeans <- by(bamboodata$culmheight,bamboodata$DBHgroup,mean)
groupSDs <- by(bamboodata$culmheight,bamboodata$DBHgroup,sd)

xvals <- seq(-0.25,0.25,0.1)

bpout <- boxplot(culmheight ~ DBHgroup, data=bamboodata, plot=FALSE)

plot(1:5,groupmeans,type='p',pch=16,cex=0.2,xlim=c(0.5,5.5),ylim=c(6,14),xlab='DBH Group',ylab='Culm Height (m)',main='Null model')

for (group in 1:5){
  temp <- bamboodata$culmheight[which(bamboodata$DBHgroup==group)]
  
  arrows(group,groupmeans[group],x1=group, y1=bpout$stats[1,group], length=0.015, angle=90, lwd=2)
  arrows(group,groupmeans[group],x1=group, y1=bpout$stats[5,group], length=0.015, angle=90, lwd=2)
  
  polygon(group+c(-0.4,0.4,0.4,-0.4),bpout$stats[c(2,2,4,4),group],col='white')
  
  for (n in 1:length(temp)){lines(group+xvals[c(n,n)],c(temp[n],grandmean),col='grey')}
  points(group+xvals,temp,pch=21,bg=pal2tone[1])
}

lines(c(0.75,5.25),c(grandmean,grandmean),lwd=3)


plot(1:5,groupmeans,type='p',pch=16,cex=0.2,xlim=c(0.5,5.5),ylim=c(6,14),xlab='DBH Group',ylab='Culm Height (m)',main='Alternative model')

for (group in 1:5){
  temp <- bamboodata$culmheight[which(bamboodata$DBHgroup==group)]
  
  arrows(group,groupmeans[group],x1=group, y1=bpout$stats[1,group], length=0.015, angle=90, lwd=2)
  arrows(group,groupmeans[group],x1=group, y1=bpout$stats[5,group], length=0.015, angle=90, lwd=2)
  polygon(group+c(-0.4,0.4,0.4,-0.4),bpout$stats[c(2,2,4,4),group],col='white')
  
  lines(c(-0.4,0.4)+group,c(mean(temp),mean(temp)), lwd=2)
  
  for (n in 1:length(temp)){lines(group+xvals[c(n,n)],c(temp[n],mean(temp)),col='grey')}
  points(group+seq(-0.25,0.25,0.1),bamboodata$culmheight[which(bamboodata$DBHgroup==group)],pch=21,bg=pal2tone[1])
  
}
for (group in 1:4){lines(c(group,group+1),c(groupmeans[group],groupmeans[group+1]),col='black',lwd=3)}

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


