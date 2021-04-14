
# Chapter 9 figures ---------------------------------------------------------------

chapter <- 9
figno <- 1

if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

xvals <<- seq(6,24,3)
yvals <<- c(65,66,69,71,76,80,87)
plot(xvals,yvals,type='p',pch=16,xlab='Age (months)',ylab='Height (cm)',xlim=c(6,24),ylim=c(60,90))
lmfit <- lm(yvals ~ xvals)
lines(xvals,lmfit$coefficients[1] + xvals*lmfit$coefficients[2],lwd=3,col=pal2tone[1])

for (n in 1:length(xvals)){lines(xvals[c(n,n)],c(yvals[n],lmfit$coefficients[1] + xvals[n]*lmfit$coefficients[2]))}

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

xvals2 <- xvals^2

plot(xvals,yvals,type='p',xlab='Age (months)',ylab='Height (cm)',pch=16,xlim=c(6,24),ylim=c(60,90))
lmfit2 <- lm(yvals ~ xvals2)
lines(xvals,lmfit2$coefficients[1] + xvals2*lmfit2$coefficients[2],lwd=3,col=pal2tone[1])

for (n in 1:length(xvals)){lines(xvals[c(n,n)],c(yvals[n],lmfit2$coefficients[1] + xvals2[n]*lmfit2$coefficients[2]))}

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

coronaUK <- read.csv('data/UKcorona.csv',header=FALSE)

fitexp <- function(p){
  pred <- p[1]*timeindays^p[2] + p[3]
  errorout <- sqrt(mean((pred-ncases)^2))
  return(errorout)
}

startvals <- c(1,2,40)
opt <- optimset(MaxIter=100000,MaxFunEvals=100000)

timeindays <<- 1:30
ncases <<- coronaUK[40:69,2]
startvals <- c(1,2,40)
sout <- neldermead::fminsearch(fitexp, startvals, opt)
p1 <- sout$optbase$xopt

timeindays <<- 1:44
ncases <<- coronaUK[40:83,2]
sout <- neldermead::fminsearch(fitexp, startvals, opt)
p2 <- sout$optbase$xopt

timeindays <- 1:50  
pred1 <- p1[1]*timeindays^p1[2] + p1[3]   
pred2 <- p2[1]*timeindays^p2[2] + p2[3]   

plot(x=NULL,y=NULL,xlim=c(0,50),ylim=c(0,120),xlab='Days from 1st March 2020',ylab='Thousands of cases')
lines(0:49,pred1/1000,lty=2,lwd=3)
lines(0:49,pred2/1000,col=pal2tone[1],lwd=3)
points(0:43,coronaUK[40:83,2]/1000,pch=16)
arrows(30,20,y1=0,length=0.1,angle=45,lwd=3)
legend(0,120,c('Fit first 30 days','Fit all data'),col=c('black',pal2tone[1]),lty=c(2,1),lwd=3)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

ramp <- colorRamp(c(pal2tone[3],"white","white"))
colmatrix2 <- rgb(ramp(seq(0, 1, length = 100)), max = 255)

mvect <- seq(-2,2,0.1)
cvect <- seq(0,100,2)
allerrors <- matrix(0,nrow=length(mvect),ncol=length(cvect))
for (mindex in 1:length(mvect)){
  for (cindex in 1:length(cvect)){
    pred <- mvect[mindex]*xvals + cvect[cindex]
    allerrors[mindex,cindex] <- sqrt(mean((pred-yvals)^2))
  }
}

plotlims <- c(-2,2,0,100)
ticklocsx <- seq(-2,2,1)    # locations of tick marks on x axis
ticklocsy <- seq(0,100,20)   # locations of tick marks on y axis
ticklabelsx <- ticklocsx       # set labels for x ticks
ticklabelsy <- ticklocsy    # set labels for y ticks

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])  
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab=expression(Slope~(beta[1])), col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5) 
title(ylab=expression(Intercept~(beta[0])), col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

image(mvect,cvect,allerrors,col=colmatrix2, add=TRUE, useRaster=TRUE, axes=FALSE, ann=FALSE)
contour(x=mvect,y=cvect,allerrors,add=TRUE)

pentx <- cos(pi/2+(0:5)*2*pi/5)/8
penty <- sin(pi/2+(0:5)*2*pi/5)*6
polygon(lmfit$coefficients[2]+pentx[c(1,3,5,2,4,6)],lmfit$coefficients[1]+penty[c(1,3,5,2,4,6)],col='black')

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

fitline <- function(p){
  pred <- p[1]*xvals + p[2]
  errorout <- sqrt(mean((pred-yvals)^2))
  return(errorout)
}

mvect <- seq(-2,2,0.1)
cvect <- seq(0,100,2)
allerrors <- matrix(0,nrow=length(mvect),ncol=length(cvect))
for (mindex in 1:length(mvect)){
  for (cindex in 1:length(cvect)){
    pred <- mvect[mindex]*xvals + cvect[cindex]
    allerrors[mindex,cindex] <- sqrt(mean((pred-yvals)^2))
  }
}

startvals <- c(-1.5,50)
opt <- optimset(MaxIter=1)
sout <- neldermead::fminsearch(fitline, startvals, opt)

xyvals0 <- sout$simplex0$x

plotlims <- c(-2,2,0,100)
ticklocsx <- seq(-2,2,1)    # locations of tick marks on x axis
ticklocsy <- seq(0,100,20)   # locations of tick marks on y axis
ticklabelsx <- ticklocsx       # set labels for x ticks
ticklabelsy <- ticklocsy    # set labels for y ticks

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])  
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab=expression(Slope~(beta[1])), col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5) 
title(ylab=expression(Intercept~(beta[0])), col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

image(mvect,cvect,allerrors,col=colmatrix2, add=TRUE, useRaster=TRUE, axes=FALSE, ann=FALSE)
contour(x=mvect,y=cvect,allerrors,add=TRUE)

xyvals <- sout$simplexopt$x
polygon(xyvals[,1],xyvals[,2],col=pal2tone[1])

for (n in 1:5){
  opt <- optimset(MaxIter=5*n)
  sout <- neldermead::fminsearch(fitline, startvals, opt)
  xyvals <- sout$simplexopt$x
  polygon(xyvals[,1],xyvals[,2],col=pal2tone[1])
}

points(mean(xyvals0[,1]),mean(xyvals0[,2]),pch=24,cex=1.5,bg='white')
polygon(lmfit$coefficients[2]+pentx[c(1,3,5,2,4,6)],lmfit$coefficients[1]+penty[c(1,3,5,2,4,6)],col='black')

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

x <- matrix(rep(seq(-5,5,length.out=200),200),nrow=200,ncol=200)
y <- t(x)
z <- (x^2 + y - 11)^2 + (x + y^2 - 7)^2

plotlims <- c(-5,5,-5,5)
ticklocsxy <- seq(-4,4,2)    # locations of tick marks on x axis

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])  
axis(1, at=c(-5,ticklocsxy,5), tck=0.01, lab=F, lwd=2)    
axis(2, at=c(-5,ticklocsxy,5), tck=0.01, lab=F, lwd=2)
mtext(text = ticklocsxy, side = 1, at=ticklocsxy)     # add the tick labels
mtext(text = ticklocsxy, side = 2, at=ticklocsxy, line=0.2, las=1)

image(x[,1],y[1,],z,zlim=c(0,150),col=colmatrix2, add=TRUE, useRaster=TRUE, axes=FALSE, ann=FALSE)

contour(x=x[,1],y=y[1,],z,nlevels=20,add=TRUE)
minpoint <- which(z==min(z),arr.ind=TRUE)

pentx <- 4*cos(pi/2+(0:5)*2*pi/5)/(8*9/5)
penty <- 4*sin(pi/2+(0:5)*2*pi/5)/(8)
polygon(x[minpoint[1],1]+pentx[c(1,3,5,2,4,6)],y[1,minpoint[2]]+penty[c(1,3,5,2,4,6)],col='black')

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

x <<- seq(-10,15,1)
p <- c(2,3)
ydata <- exp(-((x-p[1])^2)/(2*p[2]^2))
ydata <<- ydata + 0.05*rnorm(length(x))
plot(x,ydata,type='p')

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

p <- c(5,1)
pred <- exp(-((x-p[1])^2)/(2*p[2]^2))
plot(x,ydata,type='p')
lines(x,pred,lwd=2,col=pal2tone[1])

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

errorfit <- function(p){
  gaus <- exp(-((x-p[1])^2)/(2*p[2]^2))
  rms <- sqrt(sum((gaus-ydata)^2)/length(ydata))
  return(rms)}

sout <- neldermead::fminsearch(errorfit, c(1,1))  # fit the model to the data
p <- sout$optbase$xopt    # extract the parameter estimates

# get the model predictions for these parameters
pred <- exp(-((x-p[1])^2)/(2*p[2]^2))

plot(x,ydata,type='p')
lines(x,pred,lwd=2,col=pal2tone[1])  # plot the model fit as a line

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)
