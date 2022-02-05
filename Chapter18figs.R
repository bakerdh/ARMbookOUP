
# Note: this chapter uses images from the Bank of Standardised Stimuli (BOSS)
# The images were downloaded from: https://sites.google.com/site/bosstimuli/
# I have reduced the resolution of all images to 256x256 pixels and converted to greyscale
# They are available under a Creative Commons Attribution-Share Alike 3.0 license


# check for any missing settings and install required packages and functions
if (!exists('outputplot')){outputplot <- 2}
if (!exists('nsims')){nsims <- 100} # increase to 100000 for final run
if (!exists('pal2tone')){pal2tone <- c('#8783CF','#10069F','#CFCDEC')}  # blue 072

packagelist <- c('pwr','rmeta','MAd','compute.es','lme4','lmerTest','MuMIn','knitr','Hotelling','tictoc','MASS','jpeg','amap','signal','pracma','lavaan','semPlot','caret','kernlab','e1071','graphics','RSNNS','psyphy','quickpsy','BayesFactor','pals','colorspace','grImport','PRISMAstatement','rsvg','DiagrammeRsvg','png','data.table','devtools','corrplot','DiagrammeR')
missingpackages <- packagelist[!packagelist %in% installed.packages()[,1]]
if (length(missingpackages)>0){install.packages(missingpackages)}
toinstall <- packagelist[which(!packagelist %in% (.packages()))]
invisible(lapply(toinstall,library,character.only=TRUE))

addalpha <- function(col, alpha=1){apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=alpha))}
flatalpha <- function(col, alpha=1){apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1]*alpha + 1-alpha, x[2]*alpha + 1-alpha, x[3]*alpha + 1-alpha))}


load('data/ERPdata.RData')
load('data/EEGmontage.RData')
load('data/TFdata.RData')

# Chapter 18 figures ---------------------------------------------------------------

chapter <- 18
figno <- 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

ori <- seq(0,90,15)
cond1 <- c(98.5,96.67,89,83,84.3,91.67,94.3)
cond2 <- c(90.3,72,58,54.5,51.5,58.67,68)
cond3 <- c(70,55,45.5,40.25,44.67,44.3,48.3)
cond1CI <- c(1.68,2.03,3.55,4.26,4.12,3.13,2.62)
cond2CI <- c(3.35,6.24,6.86,6.92,6.94,5.58,6.48)
cond3CI <- c(6.37,6.91,4.89,4.81,5.64,5.63,5.66)

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(0,90), ylim=c(0,100))

axis(1, at=ori, tck=-0.01, lab=F, lwd=2) 
axis(2, at=seq(0,100,10), tck=-0.01, lab=F, lwd=2)
mtext(text = ori, side = 1, at=ori)    
mtext(text = seq(0,100,10), side = 2, at=seq(0,100,10), line=0.2, las=1)  
title(xlab="ELEMENT ORIENTATION (deg)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1)  
title(ylab="PROPORTION CORRECT", col.lab=rgb(0,0,0), line=1.5, cex.lab=1)

lines(ori,cond1)
lines(ori,cond2)
lines(ori,cond3)

arrows(ori,cond1,x1=ori, y1=cond1-cond1CI, length=0.015, angle=90) 
arrows(ori,cond1,x1=ori, y1=cond1+cond1CI, length=0.015, angle=90)
arrows(ori,cond2,x1=ori, y1=cond2-cond2CI, length=0.015, angle=90) 
arrows(ori,cond2,x1=ori, y1=cond2+cond2CI, length=0.015, angle=90)
arrows(ori,cond3,x1=ori, y1=cond3-cond3CI, length=0.015, angle=90) 
arrows(ori,cond3,x1=ori, y1=cond3+cond3CI, length=0.015, angle=90)

points(ori,cond1,cex=0.8)
points(ori,cond2,pch=0,cex=0.8)
points(ori,cond3,pch=2,cex=0.8)

legend(0,35,c('0 Degrees','20 Degrees','40 Degrees'),lwd=1,pch=c(1,0,2),pt.cex=0.8)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(0,90), ylim=c(0,100))

axis(1, at=ori, tck=0.01, lab=F, lwd=2) 
axis(2, at=seq(0,100,10), tck=0.01, lab=F, lwd=2)
mtext(text = ori, side = 1, at=ori)    
mtext(text = seq(0,100,10), side = 2, at=seq(0,100,10), line=0.2, las=1)  
title(xlab="Orientation (deg)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)  
title(ylab="Percent correct", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

lines(ori,cond1,lwd=3,col='black')
lines(ori,cond2,lwd=3,lty=2,col=pal2tone[1])
lines(ori,cond3,lwd=3,lty=3,col='grey')

arrows(ori,cond1,x1=ori, y1=cond1-cond1CI, length=0.015, angle=90, lwd=2) 
arrows(ori,cond1,x1=ori, y1=cond1+cond1CI, length=0.015, angle=90, lwd=2)
arrows(ori,cond2,x1=ori, y1=cond2-cond2CI, length=0.015, angle=90, lwd=2)
arrows(ori,cond2,x1=ori, y1=cond2+cond2CI, length=0.015, angle=90, lwd=2)
arrows(ori,cond3,x1=ori, y1=cond3-cond3CI, length=0.015, angle=90, lwd=2)
arrows(ori,cond3,x1=ori, y1=cond3+cond3CI, length=0.015, angle=90, lwd=2)

points(ori,cond1,pch=21,cex=1.5,lwd=2,bg='white')
points(ori,cond2,pch=22,cex=1.5,lwd=2,bg=pal2tone[1])
points(ori,cond3,pch=24,cex=1.5,lwd=2,bg='grey')

legend(0,35,c('0 Degrees','20 Degrees','40 Degrees'),pch=c(21,22,24),pt.cex=1.5,pt.lwd=2,pt.bg=c('white',pal2tone[1],'grey'),box.lwd=2)
if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

par(mfrow=c(1,4), las=1)
n <- 1000

simdata <- rnorm(n,mean=log10(400),sd=0.1)

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(-0.2,1.2), ylim=c(0,1000))
axis(2, at=seq(0,1000,100), tck=0.02, lab=F, lwd=2)
mtext(text = seq(0,1000,100), side = 2, at=seq(0,1000,100), line=0.2, las=1)  
title(ylab="Reaction time (ms)", col.lab=rgb(0,0,0), line=3, cex.lab=1.5)  
points(seq(0,1,length.out=n),10^simdata,pch=16,cex=0.5,col=pal2tone[1])
lines(c(0,1),c(mean(10^simdata),mean(10^simdata)),lwd=3)
text(-0.2,950,'(a)',cex=1.8, pos=4)


plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(-0.2,1.2), ylim=c(2,3))
axis(2, at=log10(seq(100,1000,100)), tck=0.02, lab=F, lwd=2)
mtext(text = seq(100,1000,100), side = 2, at=log10(seq(100,1000,100)), line=0.2, las=1)  
title(ylab="Reaction time (ms)", col.lab=rgb(0,0,0), line=3, cex.lab=1.5)  
points(seq(0,1,length.out=n),simdata,pch=16,cex=0.5,col=pal2tone[1])
lines(c(0,1),c(mean(simdata),mean(simdata)),lwd=3)
text(-0.2,2.95,'(b)',cex=1.8, pos=4)


plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(-0.2,1.2), ylim=c(2,3.2))
axis(2, at=seq(2,3.2,0.4), tck=0.02, lab=F, lwd=2)
mtext(text = seq(2,3.2,0.4), side = 2, at=seq(2,3.2,0.4), line=0.2, las=1)  
title(ylab="Reaction time (log(ms))", col.lab=rgb(0,0,0), line=3, cex.lab=1.5)  
points(seq(0,1,length.out=n),simdata,pch=16,cex=0.5,col=pal2tone[1])
lines(c(0,1),c(mean(simdata),mean(simdata)),lwd=3)
text(-0.2,3.15,'(c)',cex=1.8, pos=4)


plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(-0.2,1.2), ylim=c(2,3.2))
axis(2, at=log10(c(100,200,400,800,1600)), tck=0.02, lab=F, lwd=2)
mtext(text = c(100,200,400,800,1600), side = 2, at=log10(c(100,200,400,800,1600)), line=0.2, las=1)  
title(ylab="Reaction time (ms)", col.lab=rgb(0,0,0), line=3, cex.lab=1.5)  
points(seq(0,1,length.out=n),simdata,pch=16,cex=0.5,col=pal2tone[1])
lines(c(0,1),c(mean(simdata),mean(simdata)),lwd=3)
text(-0.2,3.15,'(d)',cex=1.8, pos=4)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 9)}

par(mfrow=c(1,2), las=1)

set.seed(1311)
group1 <- rnorm(10,mean=6,sd=3)
group2 <- rnorm(10,mean=9,sd=3)

barplot(c(mean(group1),mean(group2))-4,ylim=c(4,10),ylab='Height (m)',offset=4)
text(0.3,9.5,'(a)',cex=2)

barplot(c(mean(group1),mean(group2)),ylim=c(0,14),ylab='Height (m)')
text(0.3,13,'(b)',cex=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 10, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 10, width = 9)}

par(mfrow=c(2,2), las=1)

barplot(c(mean(group1),mean(group2)),ylim=c(0,15),ylab='Height (m)')
text(0.3,13,'(a)',cex=2)

barplot(c(mean(group1),mean(group2)),ylim=c(0,15),ylab='Height (m)')
arrows(0.7,mean(group1),x1=0.7, y1=mean(group1)-sd(group1), length=0.015, angle=90, lwd=2)
arrows(0.7,mean(group1),x1=0.7, y1=mean(group1)+sd(group1), length=0.015, angle=90, lwd=2)
arrows(1.9,mean(group2),x1=1.9, y1=mean(group2)-sd(group2), length=0.015, angle=90, lwd=2)
arrows(1.9,mean(group2),x1=1.9, y1=mean(group2)+sd(group2), length=0.015, angle=90, lwd=2)
text(0.3,13,'(b)',cex=2)


barplot(c(mean(group1),mean(group2)),ylim=c(0,15),ylab='Height (m)')
arrows(0.7,mean(group1),x1=0.7, y1=mean(group1)-(sd(group1)/sqrt(length(group1))), length=0.015, angle=90, lwd=2)
arrows(0.7,mean(group1),x1=0.7, y1=mean(group1)+(sd(group1)/sqrt(length(group1))), length=0.015, angle=90, lwd=2)
arrows(1.9,mean(group2),x1=1.9, y1=mean(group2)-(sd(group2)/sqrt(length(group2))), length=0.015, angle=90, lwd=2)
arrows(1.9,mean(group2),x1=1.9, y1=mean(group2)+(sd(group2)/sqrt(length(group2))), length=0.015, angle=90, lwd=2)
text(0.3,13,'(c)',cex=2)


barplot(c(mean(group1),mean(group2)),ylim=c(0,15),ylab='Height (m)')
arrows(0.7,mean(group1),x1=0.7, y1=mean(group1)-(1.96*sd(group1)/sqrt(length(group1))), length=0.015, angle=90, lwd=2)
arrows(0.7,mean(group1),x1=0.7, y1=mean(group1)+(1.96*sd(group1)/sqrt(length(group1))), length=0.015, angle=90, lwd=2)
arrows(1.9,mean(group2),x1=1.9, y1=mean(group2)-(1.96*sd(group2)/sqrt(length(group2))), length=0.015, angle=90, lwd=2)
arrows(1.9,mean(group2),x1=1.9, y1=mean(group2)+(1.96*sd(group2)/sqrt(length(group2))), length=0.015, angle=90, lwd=2)
text(0.3,13,'(d)',cex=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){cairo_ps(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), onefile = FALSE, height = 4.5, width = 6.5, fallback_resolution = 600)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(0,90), ylim=c(0,100))

axis(1, at=ori, tck=0.01, lab=F, lwd=2) 
axis(2, at=seq(0,100,10), tck=0.01, lab=F, lwd=2)
mtext(text = ori, side = 1, at=ori)    
mtext(text = seq(0,100,10), side = 2, at=seq(0,100,10), line=0.2, las=1)  
title(xlab="Orientation (deg)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)  
title(ylab="Percent correct", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

polygon(ori[c(1:7,7:1)],c(cond1+cond1CI,cond1[7:1]-cond1CI[7:1]),col=addalpha('black',alpha=0.2),border=NA)
polygon(ori[c(1:7,7:1)],c(cond2+cond2CI,cond2[7:1]-cond2CI[7:1]),col=addalpha(pal2tone[1],alpha=0.2),border=NA)
polygon(ori[c(1:7,7:1)],c(cond3+cond3CI,cond3[7:1]-cond3CI[7:1]),col=addalpha('grey',alpha=0.2),border=NA)

lines(ori,cond1,lwd=3,col='black')
lines(ori,cond2,lwd=3,lty=2,col=pal2tone[1])
lines(ori,cond3,lwd=3,lty=3,col='grey')

points(ori,cond1,pch=21,cex=1.5,lwd=2,bg='white')
points(ori,cond2,pch=22,cex=1.5,lwd=2,bg=pal2tone[1])
points(ori,cond3,pch=24,cex=1.5,lwd=2,bg='grey')

legend(0,30,c('0 Degrees','20 Degrees','40 Degrees'),pch=c(21,22,24),pt.cex=1.5,pt.lwd=2,pt.bg=c('white',pal2tone[1],'grey'),box.lwd=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){cairo_ps(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), onefile = FALSE, height = 4.5, width = 6.5, fallback_resolution = 600)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

par(mfrow=c(1,4), las=1)
n <- 30
set.seed(1311)
simdata <- rnorm(n,mean=500,sd=100)

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(-0.2,1.2), ylim=c(0,1000))
axis(2, at=seq(0,1000,100), tck=0.01, lab=F, lwd=2)
mtext(text = seq(0,1000,100), side = 2, at=seq(0,1000,100), line=0.2, las=1)  
title(ylab="Reaction time (ms)", col.lab=rgb(0,0,0), line=3, cex.lab=1.5)
polygon(c(0,1,1,0),c(0,0,mean(simdata),mean(simdata)),col=flatalpha(pal2tone[1],alpha=0.5))
arrows(0.5,mean(simdata),x1=0.5, y1=mean(simdata)-(1.96*sd(simdata)/sqrt(length(simdata))), length=0.015, angle=90, lwd=3)
arrows(0.5,mean(simdata),x1=0.5, y1=mean(simdata)+(1.96*sd(simdata)/sqrt(length(simdata))), length=0.015, angle=90, lwd=3)
points(0.5,mean(simdata),pch=16,cex=1.5)
text(-0.2,950,'(a)',cex=1.8, pos=4)

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(-0.2,1.2), ylim=c(0,1000))
axis(2, at=seq(0,1000,100), tck=0.01, lab=F, lwd=2)
mtext(text = seq(0,1000,100), side = 2, at=seq(0,1000,100), line=0.2, las=1)  
title(ylab="Reaction time (ms)", col.lab=rgb(0,0,0), line=3, cex.lab=1.5)  
lines(c(0,1),c(mean(simdata),mean(simdata)),lwd=3)
points(0.5,mean(simdata),pch=16,cex=1.5)
arrows(0,mean(simdata),x1=0, y1=mean(simdata)-(1.96*sd(simdata)/sqrt(length(simdata))), length=0.015, angle=90, lwd=3)
arrows(0,mean(simdata),x1=0, y1=mean(simdata)+(1.96*sd(simdata)/sqrt(length(simdata))), length=0.015, angle=90, lwd=3)
arrows(1,mean(simdata),x1=1, y1=mean(simdata)-(1.96*sd(simdata)/sqrt(length(simdata))), length=0.015, angle=90, lwd=3)
arrows(1,mean(simdata),x1=1, y1=mean(simdata)+(1.96*sd(simdata)/sqrt(length(simdata))), length=0.015, angle=90, lwd=3)
points(seq(0.1,0.9,length.out=n),simdata,pch=16,cex=1.5,col=addalpha(pal2tone[1],alpha=0.5))
text(-0.2,950,'(b)',cex=1.8, pos=4)

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(-0.2,1.2), ylim=c(0,1000))
axis(2, at=seq(0,1000,100), tck=0.01, lab=F, lwd=2)
mtext(text = seq(0,1000,100), side = 2, at=seq(0,1000,100), line=0.2, las=1)  
title(ylab="Reaction time (ms)", col.lab=rgb(0,0,0), line=3, cex.lab=1.5)  
a <- density(simdata)  
a$y <- 0.4*(a$y/max(a$y))                  
polygon(c(0.5-a$y,a$y+0.5), c(a$x,a$x), col=addalpha(pal2tone[1],alpha=0.5),border=NA) 
lines(c(0,1),c(mean(simdata),mean(simdata)),lwd=3)
points(0.5,mean(simdata),pch=16,cex=1.5)
arrows(0,mean(simdata),x1=0, y1=mean(simdata)-(1.96*sd(simdata)/sqrt(length(simdata))), length=0.015, angle=90, lwd=3)
arrows(0,mean(simdata),x1=0, y1=mean(simdata)+(1.96*sd(simdata)/sqrt(length(simdata))), length=0.015, angle=90, lwd=3)
arrows(1,mean(simdata),x1=1, y1=mean(simdata)-(1.96*sd(simdata)/sqrt(length(simdata))), length=0.015, angle=90, lwd=3)
arrows(1,mean(simdata),x1=1, y1=mean(simdata)+(1.96*sd(simdata)/sqrt(length(simdata))), length=0.015, angle=90, lwd=3)
text(-0.2,950,'(c)',cex=1.8, pos=4)

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(-0.2,1.2), ylim=c(0,1000))
axis(2, at=seq(0,1000,100), tck=0.01, lab=F, lwd=2)
mtext(text = seq(0,1000,100), side = 2, at=seq(0,1000,100), line=0.2, las=1)  
title(ylab="Reaction time (ms)", col.lab=rgb(0,0,0), line=3, cex.lab=1.5)  
arrows(0.5,mean(simdata),x1=0.5, y1=mean(simdata)-(1.96*sd(simdata)/sqrt(length(simdata))), length=0.015, angle=90, lwd=3)
arrows(0.5,mean(simdata),x1=0.5, y1=mean(simdata)+(1.96*sd(simdata)/sqrt(length(simdata))), length=0.015, angle=90, lwd=3)
points(0.5,mean(simdata),pch=16,cex=1.5)
points(seq(0,0.3,length.out=n),simdata,pch=16,cex=1,col=addalpha(pal2tone[1],alpha=0.5))
polygon(a$y+0.6, a$x, col=addalpha(pal2tone[1],alpha=0.5),border=NA) 
text(-0.2,950,'(d)',cex=1.8, pos=4)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){cairo_ps(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), onefile = FALSE, height = 4.5, width = 6.5, fallback_resolution = 600)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

timevals <- -199:1000
ERPdiff <- ERPdata[,2,] - ERPdata[,1,]
meandiff <- colMeans(ERPdiff)
sediff <- apply(ERPdiff,2,sd)/sqrt(38)

plotlims <- c(-200,1000,-10,20)
ticklocsx <- seq(-200,1000,200)    # locations of tick marks on x axis
ticklocsy <- seq(-10,20,5)    # locations of tick marks on y axis
tickvalsy <- ticklocsy
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklocsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = tickvalsy, side = 2, at=ticklocsy, las=1, line=0.2)
title(xlab="Time (ms)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Difference (µV)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

lines(c(-200,1000),c(0,0),lty=3,lwd=2)
lines(c(0,0),c(-10,20),lty=3,lwd=2)

polygon(timevals[c(1:1200,1200:1)], c(meandiff+sediff,meandiff[1200:1]-sediff[1200:1]), col=rgb(0,0,0,alpha=0.2),border=NA)

for (s in 1:38){
  lines(timevals,ERPdiff[s,],lwd=1,col=addalpha('black',0.2))
}
lines(timevals,ERPdiff[10,],lwd=1,col=pal2tone[1])

lines(timevals,meandiff,lwd=3,col='black')

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

pal.safe(rainbow, n=256, main='Rainbow palette')

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

pal.safe(parula, n=256, main='Parula palette')

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 10, width = 10)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 10, width = 10)}

x <- matrix(rep(seq(-4,4,length.out=200),200),nrow=200,ncol=200)
y <- t(x)
z <- (x^2 + y - 11)^2 + (x + y^2 - 7)^2

rpal <- rainbow(256)
ppal <- parula(256)

plot(x=NULL,y=NULL,xlim=c(-8,8),ylim=c(-8,8),axes=FALSE, ann=FALSE, lwd=2)
image(x[,1]-4,y[1,]+4,-z,col=rpal, useRaster=TRUE, add=TRUE, axes=FALSE, ann=FALSE)
image(x[,1]+4,y[1,]+4,-z,col=ppal, useRaster=TRUE, add=TRUE, axes=FALSE, ann=FALSE)
image(x[,1]-4,y[1,]-4,-z,col=colorspace::desaturate(rpal), add=TRUE, useRaster=TRUE, axes=FALSE, ann=FALSE)
image(x[,1]+4,y[1,]-4,-z,col=colorspace::desaturate(ppal), add=TRUE, useRaster=TRUE, axes=FALSE, ann=FALSE)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 6.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 6.5, width = 6.5)}

par(mfrow=c(1,2), las=1)

colramp <- colorRamp(c("black","red","orange","yellow","white"))
kpal <- rgb(colramp(seq(0, 1, length = 256)), max = 255)
allvals <- col2rgb(kpal)/255
# calculate luminance from a desaturated conversion to greyscale
ygr <- col2rgb(colorspace::desaturate(kpal))['red',]

plotlims <- c(0,1,0,2)  # define the x and y limits of the plot 
ticklocsx <- seq(0,1,0.2)    # locations of tick marks on x axis
ticklocsy <- seq(0,1,0.2)   # locations of tick marks on y axis
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])  
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklocsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklocsy, side = 2, at=ticklocsy, line=0.2, las=1) 
title(xlab="Palette level", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5) 
title(ylab="Luminance                                  ", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)
title('Linear ramp',line=0.1)

xvals <- seq(0,1,length.out=256)
lines(xvals,allvals[1,],col='red',lwd=3)
lines(xvals,allvals[2,],col='darkgreen',lwd=3)
lines(xvals,allvals[3,],col='blue',lwd=3)
lines(xvals,ygr/255,col='black',lwd=3)

text(0.1,0.8,'R',cex=1.5,col='red')
text(0.3,0.65,'L',cex=1.5,col='black')
text(0.45,0.25,'G',cex=1.5,col='darkgreen')
text(0.8,0.5,'B',cex=1.5,col='blue')


z=matrix(1:256,ncol=1)
x=seq(0,1,len=256)

rgbpal <- col2rgb(kpal)/255
rpal <- rgb(rgbpal[1,],rgbpal[2,]*0,rgbpal[3,]*0)
gpal <- rgb(rgbpal[1,]*0,rgbpal[2,],rgbpal[3,]*0)
bpal <- rgb(rgbpal[1,]*0,rgbpal[2,]*0,rgbpal[3,])
ygr <- col2rgb(colorspace::desaturate(kpal))/255
ypal <- rgb(ygr[1,],ygr[2,],ygr[3,])
image(x,c(1.82,1.98),z,col=kpal,add=TRUE,useRaster=TRUE)
image(x,c(1.62,1.78),z,col=rpal,add=TRUE,useRaster=TRUE)
image(x,c(1.42,1.58),z,col=gpal,add=TRUE,useRaster=TRUE)
image(x,c(1.22,1.38),z,col=bpal,add=TRUE,useRaster=TRUE)
image(x,c(1.02,1.18),z,col=ypal,add=TRUE,useRaster=TRUE)



kpal <- kovesi.linear_kryw_5_100_c64(256)
allvals <- col2rgb(kpal)/255
# calculate luminance from a desaturated conversion to greyscale
ygr <- col2rgb(colorspace::desaturate(kpal))['red',]

plotlims <- c(0,1,0,2)  # define the x and y limits of the plot 
ticklocsx <- seq(0,1,0.2)    # locations of tick marks on x axis
ticklocsy <- seq(0,1,0.2)   # locations of tick marks on y axis
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])  
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklocsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklocsy, side = 2, at=ticklocsy, line=0.2, las=1) 
title(xlab="Palette level", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5) 
title(ylab="Luminance                                  ", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)
title('Perceptually uniform',line=0.1)

xvals <- seq(0,1,length.out=256)
lines(xvals,allvals[1,],col='red',lwd=3)
lines(xvals,allvals[2,],col='darkgreen',lwd=3)
lines(xvals,allvals[3,],col='blue',lwd=3)
lines(xvals,ygr/255,col='black',lwd=3)

text(0.3,0.8,'R',cex=1.5,col='red')
text(0.55,0.65,'L',cex=1.5,col='black')
text(0.45,0.25,'G',cex=1.5,col='darkgreen')
text(0.8,0.5,'B',cex=1.5,col='blue')

z=matrix(1:256,ncol=1)
x=seq(0,1,len=256)

rgbpal <- col2rgb(kpal)/255
rpal <- rgb(rgbpal[1,],rgbpal[2,]*0,rgbpal[3,]*0)
gpal <- rgb(rgbpal[1,]*0,rgbpal[2,],rgbpal[3,]*0)
bpal <- rgb(rgbpal[1,]*0,rgbpal[2,]*0,rgbpal[3,])
ygr <- col2rgb(colorspace::desaturate(kpal))/255
ypal <- rgb(ygr[1,],ygr[2,],ygr[3,])
image(x,c(1.82,1.98),z,col=kpal,add=TRUE,useRaster=TRUE)
image(x,c(1.62,1.78),z,col=rpal,add=TRUE,useRaster=TRUE)
image(x,c(1.42,1.58),z,col=gpal,add=TRUE,useRaster=TRUE)
image(x,c(1.22,1.38),z,col=bpal,add=TRUE,useRaster=TRUE)
image(x,c(1.02,1.18),z,col=ypal,add=TRUE,useRaster=TRUE)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 8)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 8)}

par(mfrow=c(1,2), las=1)

timevals <- t(TFtime[1,26:176])
fvals <- t(TFfreq[1,5:155])
ivals <- t(TFdata[6:155,26:175])

colramp <- colorRamp(c("black","red","orange","yellow","white"))
kpal <- rgb(colramp(seq(0, 1, length = 256)), max = 255)
image(timevals,fvals,ivals,col=kpal, useRaster=TRUE, axes=FALSE, ann=FALSE)
lines(c(0,0),c(min(fvals),max(fvals)),lwd=3,col='blue')
mtext(text = 1000*seq(0,1,0.5), side = 1, at=seq(0,1,0.5))
mtext(text = seq(4,52,8), side = 2, at=seq(4,52,8), line=0.2, las=1) 
title(xlab="Time (ms)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Frequency (Hz)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

kpal <- kovesi.linear_kryw_5_100_c64(256)
image(timevals,fvals,ivals,col=kpal, useRaster=TRUE, axes=FALSE, ann=FALSE)
lines(c(0,0),c(min(fvals),max(fvals)),lwd=3,col='blue')
mtext(text = 1000*seq(0,1,0.5), side = 1, at=seq(0,1,0.5))
mtext(text = seq(4,52,8), side = 2, at=seq(4,52,8), line=0.2, las=1) 
title(xlab="Time (ms)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Frequency (Hz)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){cairo_ps(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), onefile = FALSE, height = 10, width = 9, fallback_resolution = 600)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 10, width = 9)}

par(mfrow=c(2,2), las=1)

plotlims <- c(0,1,0,1)
ticklocs <- seq(0,1,0.2)

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocs, tck=0.01, lab=F, lwd=2) 
axis(2, at=ticklocs, tck=0.01, lab=F, lwd=2)
mtext(text = ticklocs, side = 1, at=ticklocs)     # add the tick labels
# the 'line' command moves away from the axis, the 'las' command rotates to vertical
mtext(text = ticklocs, side = 2, at=ticklocs, line=0.2, las=1)  
title(xlab="X axis title", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Y axis title", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)
text(-0.05,0.95,'(a)',cex=1.8, pos=4)

datax <- sort(runif(10,min=0,max=1))
datay <- sort(runif(10,min=0.2,max=0.8))
SEdata <- runif(10,min=0,max=0.1)
datax2 <- sort(runif(10,min=0,max=1))
datay2 <- sort(runif(10,min=0.2,max=0.8))
SEdata2 <- runif(10,min=0,max=0.1)

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocs, tck=0.01, lab=F, lwd=2) 
axis(2, at=ticklocs, tck=0.01, lab=F, lwd=2)
mtext(text = ticklocs, side = 1, at=ticklocs) 
mtext(text = ticklocs, side = 2, at=ticklocs, line=0.2, las=1)  
title(xlab="X axis title", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Y axis title", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

lines(datax,datay, col=pal2tone[1], lwd=3, lty=1)     
arrows(datax,datay,x1=datax, y1=datay-SEdata, length=0.015, angle=90, lwd=2)  
arrows(datax,datay,x1=datax, y1=datay+SEdata, length=0.015, angle=90, lwd=2) 
points(datax,datay, pch = 21, col='black', bg=pal2tone[1], cex=1.6, lwd=3) 

lines(datax2,datay2, col='grey', lwd=3, lty=2)     
arrows(datax2,datay2,x1=datax2, y1=datay2-SEdata2, length=0.015, angle=90, lwd=2)  
arrows(datax2,datay2,x1=datax2, y1=datay2+SEdata2, length=0.015, angle=90, lwd=2) 
points(datax2,datay2, pch = 22, col='black', bg='grey', cex=1.6, lwd=3) 

legend(0, 1, c('Condition 1','Condition 2'), col=c('black','black'), pt.cex=1.6, pt.bg=c(pal2tone[1],'grey'),lty=1, lwd=3, pch=21:22, pt.lwd=3, cex=1, box.lwd=2)
text(0.85,0.95,'(b)',cex=1.8, pos=4)


plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocs, tck=0.01, lab=F, lwd=2) 
axis(2, at=ticklocs, tck=0.01, lab=F, lwd=2)
mtext(text = ticklocs, side = 1, at=ticklocs) 
mtext(text = ticklocs, side = 2, at=ticklocs, line=0.2, las=1)  
title(xlab="X axis title", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Y axis title", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

colour1 <- addalpha(pal2tone[2],0.5)
colour2 <- addalpha('black',0.5)

polygon(c(0.2,0.8,0.8,0.2),c(0.2,0.2,0.6,0.6),col=colour1)
polygon(c(0.4,0.6,0.6,0.4),c(0.4,0.4,0.9,0.9),col=colour2)

text(-0.05,0.95,'(c)',cex=1.8, pos=4)

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocs, tck=0.01, lab=F, lwd=2) 
axis(2, at=ticklocs, tck=0.01, lab=F, lwd=2)
mtext(text = ticklocs, side = 1, at=ticklocs) 
mtext(text = ticklocs, side = 2, at=ticklocs, line=0.2, las=1)  
title(xlab="X axis title", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Y axis title", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

datax <- rnorm(1000,mean=0.5,sd=0.1)
datay <- rnorm(1000,mean=0.5,sd=0.1) + (datax-0.5)

a <- density(datax)
a$y <- 0.2*(a$y/max(a$y))
polygon(a$x, 1-a$y, col=colour1,border=NA)

a <- density(datay)
a$y <- 0.2*(a$y/max(a$y))
polygon(1-a$y, a$x, col=colour2,border=NA)

points(datax,datay,col=rgb(0,0,0,alpha=0.2),pch=16,cex=0.6)
text(-0.05,0.95,'(d)',cex=1.8, pos=4)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

pal.test(kovesi.linear_bgyw_15_100_c67)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

pal.test(rainbow)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 4.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 4.5)}

x <- matrix(rep(seq(-10,0,length.out=200),200),nrow=200,ncol=200)
y <- t(matrix(rep(seq(-6.5,0,length.out=200),200),nrow=200,ncol=200))
z <- sin(y)*exp((1 - cos(x))^2) + cos(x)*exp((1 - sin(y))^2) + (x - y)^2

rpal <- kovesi.diverging_bky_60_10_c30(256)
image(x[,1],y[1,],z,col=rpal, useRaster=TRUE, axes=FALSE, ann=FALSE)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



# generate the graph and save as postscript
postscript('outputfile.ps', horizontal = FALSE, onefile = FALSE, 
           paper = 'special', height = 5.5, width = 5.5)

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocs, tck=0.01, lab=F, lwd=2) 
axis(2, at=ticklocs, tck=0.01, lab=F, lwd=2)
mtext(text = ticklocs, side = 1, at=ticklocs) 
mtext(text = ticklocs, side = 2, at=ticklocs, line=0.2, las=1)  
title(xlab="X axis title", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Y axis title", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

datax <- rnorm(1000,mean=0.5,sd=0.1)
datay <- rnorm(1000,mean=0.5,sd=0.1) + (datax-0.5)

a <- density(datax)
a$y <- 0.2*(a$y/max(a$y))
polygon(a$x, 1-a$y, col=pal2tone[2],border=NA)

a <- density(datay)
a$y <- 0.2*(a$y/max(a$y))
polygon(1-a$y, a$x, col='black',border=NA)

points(datax,datay,col=rgb(0,0,0),pch=16,cex=0.6)

dev.off()

# import the figure
#the commented-out line below is a workaround if the following lines of code won't run
#simply replace the filepath with where your Ghostscript exe file is stored
#Sys.setenv(R_GSCMD = normalizePath("C:\\Program Files\\gs\\gs9.55.0\\bin\\gswin64c.exe"))
PostScriptTrace('outputfile.ps')
e1 <- readPicture('outputfile.ps.xml')

figno <- figno + 2
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 4.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 4.5)}

# create a new plot and add the graph at several scales
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
grid.picture(e1,x=0.8,y=0.2,width=0.2,height=1)
grid.picture(e1,x=0.6,y=0.4,width=0.4,height=1)
grid.picture(e1,x=0.25,y=0.75,width=0.5,height=1)

# clean up the external files we generated
file.remove(c('outputfile.ps','outputfile.ps.xml'))
if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){cairo_ps(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), onefile = FALSE, height = 5, width = 4.5, fallback_resolution = 600)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 4.5)}

for (n in 1:length(e1@paths)){  # loop through all features
  temp <- class(e1@paths[n]$path)[1] # look at just one feature
  # find only the filled shapes
  if (pmatch(temp,"PictureFill",nomatch=0)){
    # set any filled shape to be semi-transparent
    e1@paths[n]$path@rgb <- addalpha(e1@paths[n]$path@rgb,alpha=0.3)}}

# plot everything again
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
grid.picture(e1,x=0.8,y=0.2,width=0.2,height=1)
grid.picture(e1,x=0.6,y=0.4,width=0.4,height=1)
grid.picture(e1,x=0.25,y=0.75,width=0.5,height=1)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 10, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 10, width = 9)}

violin <- readJPEG('images/BOSSr/violin.jpg')

plotlims <- c(0,1,0,1)
ticklocs <- seq(0,1,0.2)

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocs, tck=0.01, lab=F, lwd=2)
axis(2, at=ticklocs, tck=0.01, lab=F, lwd=2)
mtext(text = ticklocs, side = 1, cex=2, at=ticklocs, line=1)     # add the tick labels
mtext(text = ticklocs, side = 2, cex=2, at=ticklocs, line=0.2, las=1)

rasterImage(violin,0.2,0.2,0.5,0.5)
rasterImage(violin,0.8,0.8,1.1,1.1,angle=180)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)
