
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

# Chapter 16 figures ---------------------------------------------------------------

chapter <- 16
figno <- 2
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4, width = 10)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4, width = 10)}

par(mfrow=c(1,3), las=1)

namelist <- c('Aya','Bia','Che')
pmatrix <- matrix(0,nrow=3,ncol=4)
pmatrix[1,] <- c(40,10,10,40)
pmatrix[2,] <- c(49,1,19,31)
pmatrix[3,] <- c(35,15,5,45)
plotlims <- c(0,1,0,1) 

for (n in 1:3){
  plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
  lines(c(0,1),c(0.25,0.25))
  lines(c(0,1),c(0.5,0.5))
  lines(c(0,1),c(0.75,0.75))
  lines(c(0.25,0.25),c(0,1))
  lines(c(0.5,0.5),c(0,1))
  lines(c(0.75,0.75),c(0,1))
  
  text(0.375,0.875,'F',cex=2)
  text(0.625,0.875,'M',cex=2)
  text(0.125,0.625,'F',cex=2)
  text(0.125,0.375,'M',cex=2)
  text(0.875,0.875,'Total',cex=1.5)
  text(0.125,0.125,'Total',cex=1.5)
  
  text(0.375,0.625,pmatrix[n,1],cex=2)
  text(0.625,0.625,pmatrix[n,2],cex=2)
  text(0.375,0.375,pmatrix[n,3],cex=2)
  text(0.625,0.375,pmatrix[n,4],cex=2)
  
  text(0.875,0.625,pmatrix[n,1]+pmatrix[n,2],cex=2,col='grey')
  text(0.875,0.375,pmatrix[n,3]+pmatrix[n,4],cex=2,col='grey')
  text(0.375,0.125,pmatrix[n,1]+pmatrix[n,3],cex=2,col='grey')
  text(0.625,0.125,pmatrix[n,2]+pmatrix[n,4],cex=2,col='grey')
  
  text(0.875,0.125,paste(pmatrix[n,1]+pmatrix[n,4],'%',sep=''),cex=2,col='black')
  text(0.875,0.05,'correct',cex=1.2,col='black')
  
  text(0.375,0.54,'Hits',cex=1.2,col='grey')
  text(0.625,0.54,'Misses',cex=1.2,col='grey')
  text(0.375,0.29,'FAs',cex=1.2,col='grey')
  text(0.625,0.29,'CRs',cex=1.2,col='grey')
  
  text(0.125,0.875,namelist[n],cex=2,font=2)
  
  mtext("Assigned sex", cex=1.5,side=3)
  title(ylab="True sex", line=0.25, cex.lab=2.5)
  
}

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4, width = 10)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4, width = 10)}

load('data/chickoutlines.RData')

par(mfrow=c(1,3), las=1)

nchicks <- 16
xvals <- seq(0,1,0.01)
plotlims <- c(0,1,0,1)  # define the x and y limits of the plot (minx,maxx,miny,maxy)
ticklocsx <- c(0,0.25,0.75,1)    # locations of tick marks on x axis
ticklocsy <- (0:5)/5    # locations of tick marks on y axis
ticklabelsx <- c("","Male","Female","")        # set labels for x ticks

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
mtext(text = ticklabelsx, side = 1, line=0.2, at=ticklocsx)
title(main='Adults')

dist1 <- dnorm(xvals,mean=1/4,sd=1/18)
dist1 <- dist1/max(dist1)
dist2 <- dnorm(xvals,mean=3/4,sd=1/18)
dist2 <- dist2/max(dist2)
lines(xvals, dist1, lwd=3) 
lines(xvals, dist2, lwd=3, lty=2, col=pal2tone[1]) 
points(rnorm(nchicks,mean=1/4,sd=1/18),runif(nchicks)/4,col='black',pch=16)
points(rnorm(nchicks,mean=3/4,sd=1/18),runif(nchicks)/4,col=pal2tone[1],pch=16)
text(0.05,0.95,'(a)',cex=1.5)
polygon(chickout[1,,1]/4+0.38,chickout[1,,2]/4+0.8,border=NA,col=pal2tone[1])

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
mtext(text = ticklabelsx, side = 1, line=0.2, at=ticklocsx)
title(xlab="Internal response", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)    # titles for axes
title(main='Eggs')

dist1 <- dnorm(xvals,mean=2/4,sd=1/10)
dist1 <- dist1/max(dist1)
dist2 <- dnorm(xvals,mean=2/4,sd=1/10)
dist2 <- dist2/max(dist2)
lines(xvals, dist1, lwd=3) 
lines(xvals, dist2, lwd=3, lty=2, col=pal2tone[1]) 
points(rnorm(nchicks,mean=2/4,sd=1/10),runif(nchicks)/4,col='black',pch=16)
points(rnorm(nchicks,mean=2/4,sd=1/10),runif(nchicks)/4,col=pal2tone[1],pch=16)
text(0.05,0.95,'(b)',cex=1.5)
polygon(chickout[2,,1]/3+0.6,chickout[2,,2]/3+0.8,border=NA,col=pal2tone[1])

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
mtext(text = ticklabelsx, side = 1, line=0.2, at=ticklocsx)
title(main='Chicks')

dist1 <- dnorm(xvals,mean=3/8,sd=1/10)
dist1 <- dist1/max(dist1)
dist2 <- dnorm(xvals,mean=5/8,sd=1/10)
dist2 <- dist2/max(dist2)
lines(xvals, dist1, lwd=3) 
lines(xvals, dist2, lwd=3, lty=2, col=pal2tone[1]) 
points(rnorm(nchicks,mean=3/8,sd=1/10),runif(nchicks)/4,col='black',pch=16)
points(rnorm(nchicks,mean=5/8,sd=1/10),runif(nchicks)/4,col=pal2tone[1],pch=16)
text(0.05,0.95,'(c)',cex=1.5)
polygon(chickout[3,,1]/4+0.7,chickout[3,,2]/4+0.8,border=NA,col=pal2tone[1])

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

plotlims <- c(0,1,0,1)  # define the x and y limits of the plot (minx,maxx,miny,maxy)
ticklocsx <- c(0,0.25,0.75,1)    # locations of tick marks on x axis
ticklocsy <- (0:5)/5    # locations of tick marks on y axis
ticklabelsx <- c("","Male","Female","")        # set labels for x ticks

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)
title(xlab="Internal response", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes

xvals <- seq(0,1,0.01)
dist1 <- dnorm(xvals,mean=3/8,sd=1/10)
dist1 <- 0.8*dist1/max(dist1)
dist2 <- dnorm(xvals,mean=5/8,sd=1/10)
dist2 <- 0.8*dist2/max(dist2)

polygon(c(xvals[which(xvals>=0.5)],0.5), c(dist1[which(xvals>=0.5)],0), col='gray',border=NA)
polygon(c(0.5,xvals[which(xvals<=0.5)]), c(0,dist2[which(xvals<=0.5)]), col=pal2tone[3],border=NA)

lines(xvals, dist1, lwd=3) 
lines(xvals, dist2, lwd=3, lty=2, col=pal2tone[1]) 

lines(c(0.5,0.5),c(0,0.7),lty=3)
text(0.5,0.75,'Criterion',cex=1.2)

text(0.5,0.95,"Sensitivity (d')",cex=1.2)
lines(c(3/8,5/8),c(0.9,0.9))
lines(c(3/8,3/8),c(0.85,0.9))
lines(c(5/8,5/8),c(0.85,0.9))

text(5/8,0.5,'Hits',cex=1.2)
text(3/8,0.54,'Correct',cex=1.2)
text(3/8,0.46,'Rejections',cex=1.2)
text(0.49,0.12,'False',cex=1.2,pos=4)
text(0.49,0.05,'Alarms',cex=1.2,pos=4)
text(0.51,0.09,'Misses',cex=1.2,pos=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 5)}

plotlims <- c(0,1,0,1) 
par(pty="s") 
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
lines(c(0,1),c(0.25,0.25))
lines(c(0,1),c(0.5,0.5))
lines(c(0,1),c(0.75,0.75))
lines(c(0.25,0.25),c(0,1))
lines(c(0.5,0.5),c(0,1))
lines(c(0.75,0.75),c(0,1))

text(0.375,0.875,'F',cex=1.5)
text(0.625,0.875,'M',cex=1.5)
text(0.125,0.625,'F',cex=1.5)
text(0.125,0.375,'M',cex=1.5)
text(0.875,0.875,'Total',cex=1.2)

text(0.375,0.625,'0.8',cex=1.5)
text(0.625,0.625,'0.2',cex=1.5)
text(0.375,0.375,'0.2',cex=1.5)
text(0.625,0.375,'0.8',cex=1.5)

text(0.875,0.625,'1',cex=1.5,col='grey')
text(0.875,0.375,'1',cex=1.5,col='grey')

text(0.375,0.53,'P(Hit)',cex=1,col='grey')
text(0.625,0.53,'P(Miss)',cex=1,col='grey')
text(0.375,0.28,'P(FA)',cex=1,col='grey')
text(0.625,0.28,'P(CR)',cex=1,col='grey')

text(0.125,0.875,'Aya',cex=1.5,font=2)
mtext("Assigned sex", cex=1.5,side=3)
mtext("True sex", cex=1.5,side=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

plotlims <- c(-5,5,0,1)  
ticklocsx <- seq(-5,5,1)    # locations of tick marks on x axis
ticklocsy <- seq(0,1,0.2)    # locations of tick marks on y axis
ticklabelsx <- ticklocsx        # set labels for x ticks
ticklabelsy <- ticklocsy        # set labels for x ticks

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="z (sd units)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="Cumulative probability", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)    # titles for axes

lines(c(-5,5),c(0.5,0.5),lty=2)
lines(c(0,0),c(0,1),lty=2)
lines(seq(-5,5,0.01),pnorm(seq(-5,5,0.01)),col='black',lwd=3)

points(qnorm(0.2),0.2,pch = 21, col='black', bg=pal2tone[1],cex=2)
points(qnorm(0.8),0.8,pch = 22, col='black', bg='white',cex=2)

arrows(qnorm(0.2),0.16,x1=qnorm(0.2), y1=0, length=0.1, angle=30, lwd=2, col=pal2tone[1]) 
arrows(-5,0.2,x1=-1, y1=0.2, length=0.1, angle=30, lwd=2, col=pal2tone[1]) 

arrows(qnorm(0.8),0.76,x1=qnorm(0.8), y1=0, length=0.1, angle=30, lwd=2, col='black') 
arrows(-5,0.8,x1=0.7, y1=0.8, length=0.1, angle=30, lwd=2, col='black') 

text(-1.6,0.26,'FA',col=pal2tone[1],cex=2)
text(1.8,0.8,'Hits',col='black',cex=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 9)}

ntrials <- 10000

par(mfrow=c(1,2), las=1)

plotlims <- c(0,1,0,1)  # define the x and y limits of the plot (minx,maxx,miny,maxy)
ticklocsx <- seq(0,1,0.2)     # locations of tick marks on x axis
ticklocsy <- seq(0,1,0.2)    # locations of tick marks on y axis
ticklabelsx <- ticklocsx        # set labels for x ticks
ticklabelsy <- ticklocsy        # set labels for x ticks

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="False alarm rate", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="Hit rate", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)    # titles for axes

lines(c(0,1),c(0,1),lwd=4)

nulldata <- rnorm(ntrials,mean=0,sd=1)
signaldata <- rnorm(ntrials,mean=1,sd=1)
criteriavals <- seq(-1,2,0.4)
pFA <- NULL
pHit <- NULL
for (cindex in 1:length(criteriavals)){
  criterion <- criteriavals[cindex]
  pFA[cindex] <- length(which(nulldata>criterion))/ntrials
  pHit[cindex] <-length(which(signaldata>criterion))/ntrials 
}

ramp <- colorRamp(c("white", "black"))  # create a ramp from one colour to another
colmatrix <- rgb(ramp(seq(0, 1, length = length(pFA))), max = 255)   # index the ramp at ten points
points(pFA,pHit,pch = 21, col='black', bg=colmatrix,cex=2)

text(0.05,0.95,'(a)',cex=1.6)

legend(0.4,0.35,c('Liberal','Conservative'),title='Criterion',cex=0.8,pt.cex=2,pch=21,col='black',pt.bg=c('white','black'),box.lwd=2)


par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="False alarm rate", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="Hit rate", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)    # titles for axes

lines(c(0,1),c(0,1),lwd=4)

nulldata <- rnorm(ntrials,mean=0,sd=1)
signaldata <- rnorm(ntrials,mean=0.5,sd=1)
criteriavals <- seq(-4,5,0.1)
pFA <- NULL
pHit <- NULL
for (cindex in 1:length(criteriavals)){
  criterion <- criteriavals[cindex]
  pFA[cindex] <- length(which(nulldata>criterion))/ntrials
  pHit[cindex] <-length(which(signaldata>criterion))/ntrials 
}
lines(pFA,pHit, col='black', lwd=2,lty=3)

nulldata <- rnorm(ntrials,mean=0,sd=1)
signaldata <- rnorm(ntrials,mean=1,sd=1)
criteriavals <- seq(-4,5,0.1)
pFA <- NULL
pHit <- NULL
for (cindex in 1:length(criteriavals)){
  criterion <- criteriavals[cindex]
  pFA[cindex] <- length(which(nulldata>criterion))/ntrials
  pHit[cindex] <-length(which(signaldata>criterion))/ntrials 
}
lines(pFA,pHit, col='black', lwd=2)

nulldata <- rnorm(ntrials,mean=0,sd=1)
signaldata <- rnorm(ntrials,mean=2,sd=1)
criteriavals <- seq(-4,5,0.1)
pFA <- NULL
pHit <- NULL
for (cindex in 1:length(criteriavals)){
  criterion <- criteriavals[cindex]
  pFA[cindex] <- length(which(nulldata>criterion))/ntrials
  pHit[cindex] <-length(which(signaldata>criterion))/ntrials 
}
lines(pFA,pHit, col='black', lwd=2,lty=2)

text(0.05,0.95,'(b)',cex=1.6)

legend(0.4,0.35,c("d' = 0.5","d' = 1","d' = 2"),cex=0.8,col='black',lty=c(3,1,2),box.lwd=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

plotlims <- c(0,1,0,1)  # define the x and y limits of the plot (minx,maxx,miny,maxy)
ticklocsx <- c(0,1)    # locations of tick marks on x axis
ticklocsy <- (0:5)/5    # locations of tick marks on y axis
ticklabelsx <- c("","")        # set labels for x ticks

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)
title(xlab="Internal response", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes

xvals <- seq(0,1,0.01)
dist1 <- dnorm(xvals,mean=3/8,sd=1/10)
dist1 <- 0.8*dist1/max(dist1)
dist2 <- dnorm(xvals,mean=5/8,sd=1/10)
dist2 <- 0.8*dist2/max(dist2)
lines(xvals, dist1, lwd=3) 
lines(xvals, dist2, col=pal2tone[1],lwd=3,lty=2) 

text(0.5,0.95,"Sensitivity (d')",cex=1.2)
lines(c(3/8,5/8),c(0.9,0.9))
lines(c(3/8,3/8),c(0.85,0.9))
lines(c(5/8,5/8),c(0.85,0.9))

arrows(0.3,0.5,x1=0.3, y1=0, length=0.1, angle=30, lwd=2, col='black') 
arrows(0.7,0.5,x1=0.7, y1=0, length=0.1, angle=30, lwd=2, col=pal2tone[1]) 

points(0.3,0.5,pch = 21, col='black', bg='black',cex=2)
points(0.7,0.5,pch = 22, col='black', bg=pal2tone[1],cex=2)

text(5/8,0.6,'Target',cex=1.2,col=pal2tone[1])
text(3/8,0.6,'Null',cex=1.2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){cairo_ps(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), onefile = FALSE, height = 10, width = 6.5, fallback_resolution = 600)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 10, width = 6.5)}

meanvals <- seq(0,1,0.2)
sdval <- 0.4
xvals <- seq(-2,3,0.01)

par(mfrow=c(2,1), las=1)

plotlims <- c(-1,2,0,1)  
ticklocsx <- seq(-1,3,1)    
ticklabelsx <- c("","0","","")  
ticklocsy <- c(0,1)

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
title(xlab="Internal response", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)   
title(ylab="Probability", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)    

lines(xvals, dnorm(xvals,mean=meanvals[1],sd=sdval), lwd=3)
lines(xvals, dnorm(xvals,mean=meanvals[2],sd=sdval), lwd=3, col=pal2tone[2])
lines(xvals, dnorm(xvals,mean=meanvals[3],sd=sdval), lwd=3, col='grey')
lines(xvals, dnorm(xvals,mean=meanvals[4],sd=sdval), lwd=3, col=pal2tone[1])
lines(xvals, dnorm(xvals,mean=meanvals[5],sd=sdval), lwd=3, col=pal2tone[3])
lines(xvals, dnorm(xvals,mean=meanvals[6],sd=sdval), lwd=3, lty=2)

lines(c(0,0),c(0,2),lty=2)
text(-0.9,0.95,'(a)',cex=2)

plotlims <- c(0,1,0,1) 
ticklocsx <- seq(0,1,0.2)     
ticklabelsx <- ticklocsx       
ticklocsy <- seq(0,1,0.2) 
ticklabelsy <- ticklocsy       

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     
mtext(text = ticklabelsx, side = 1, at=ticklocsx)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Signal strength", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5) 
title(ylab="Proportion correct", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5) 

dprimes <- meanvals/sdval
accuracy <- pnorm(dprimes/sqrt(2))

levelsC <- meanvals
Ntrials <- 100
Ncorrect <- round(accuracy * Ntrials)
datatofit <- data.frame(levelsC,Ncorrect,Ntrials)
pmffit <- quickpsy(datatofit, x=levelsC, k=Ncorrect, n=Ntrials, parini=list(c(0.1,1),c(1,10),c(0,0.01)), lapses=FALSE,guess=0.5, fun=weibull_fun, bootstrap = "none")
lev <- pmffit$curves$x[3:297]
lines(lev,pmffit$curves$y[3:297], col='black',lwd=3)

points(meanvals,accuracy,pch=21,cex=2,bg=c('black',pal2tone[2],'grey',pal2tone[1],pal2tone[3],'white'))
lines(c(0,1),c(0.5,0.5),lty=3)

lines(c(0,as.numeric(pmffit$par[1,2])),c(0.816,0.816),lty=2)
lines(c(as.numeric(pmffit$par[1,2]),as.numeric(pmffit$par[1,2])),c(0,0.816),lty=2)
text(0.05,0.95,'(b)',cex=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

proportions <- seq(0.1,0.9,0.1)

dprime2afc <- qnorm(proportions)*sqrt(2)
dprime3afc <- sapply(proportions,dprime.mAFC,3)
dprime4afc <- sapply(proportions,dprime.mAFC,4)
dprime5afc <- sapply(proportions,dprime.mAFC,5)

plot(proportions,dprime2afc,type='l',lwd=2,xlab='Proportion correct',ylab="d'")
lines(proportions,dprime3afc,col=pal2tone[2],lwd=2)
lines(proportions,dprime4afc,col=pal2tone[1],lwd=2)
lines(proportions,dprime5afc,col=pal2tone[3],lwd=2)

lines(c(0,1),c(0,0),lty=3)
lines(c(1/2,1/2),c(-2,2),lty=3)
lines(c(1/3,1/3),c(-2,2),lty=3,col=pal2tone[2])
lines(c(1/4,1/4),c(-2,2),lty=3,col=pal2tone[1])
lines(c(1/5,1/5),c(-2,2),lty=3,col=pal2tone[3])

legend(0.6,-0.25,c('m=2','m=3','m=4','m=5'),col=c(rgb(0,0,0),pal2tone[c(2,1,3)]),lty=1,lwd=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)
