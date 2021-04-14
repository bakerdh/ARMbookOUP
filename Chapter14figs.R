
# Chapter 14 figures ---------------------------------------------------------------

load('data/MRIdata.Rdata')

chapter <- 14
figno <- 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 9)}

par(mfrow=c(1,2), las=1)

setA <- rnorm(50,mean=14, sd=2)
setB <- rnorm(50,mean=18, sd=2)

plotlims <- c(0,101,0,30) 
ticklocsx <- c(25,75)    # locations of tick marks on x axis
ticklocsy <- seq(0,30,10)    # locations of tick marks on y axis
ticklabelsx <- c("Group A","Group B")        # set labels for x ticks
ticklabelsy <- ticklocsy    # set labels for y ticks
par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)  
points(1:50,setA, pch = 21, col='black', bg='grey', cex=1.6, lwd=1)   
points(51:100,setB, pch = 22, col='black', bg=pal2tone[1], cex=1.6, lwd=1)  
lines(c(1,50),c(mean(setA),mean(setA)),lwd=8)
lines(c(51,100),c(mean(setB),mean(setB)),lwd=8)

text(5,28,'(a)',cex=1.5)

trainingdata <- data.frame(c(setA,setB),0*rnorm(100,mean=0,sd=1))
colnames(trainingdata) <- c('data','noise')
traininglabels <- as.factor(ceiling((1:100)/50))

svmFit <- caret::train(trainingdata, traininglabels, method = "svmLinear", tuneLength = 5, scaled = FALSE)
boundaryxvals <- seq(0,30,0.1)
boundarydata <- data.frame(boundaryxvals,0*boundaryxvals)
boundarypredict <- predict(svmFit,newdata = boundarydata)

plotlims <- c(-25,75,0,30)  # define the x and y limits of the plot 
ticklocsy <- seq(0,30,10)    # locations of tick marks on y axis
ticklabelsy <- ticklocsy    # set labels for y ticks
par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)  
points(1:50,setA, pch = 21, col='black', bg='grey', cex=1.6, lwd=1) 
points(1:50,setB, pch = 22, col='black', bg=pal2tone[1], cex=1.6, lwd=1)   

boundaryyval <- (max(boundaryxvals[boundarypredict==1]) + min(boundaryxvals[boundarypredict==2]))/2
lines(c(-25,75),c(boundaryyval,boundaryyval),lwd=4,lty=2,col='black')

text(-18,28,'(b)',cex=1.5)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 5)}

dlist <- seq(0,4,0.1)

d_dprimetoprob <- function(d){
  erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
  normsdist <- function(z) 0.5*(1+erf(z/sqrt(2)))
  p <- normsdist(d/sqrt(2))
  return(p)
}

plotlims <- c(0,4,50,100)  # define the x and y limits of the plot (minx,maxx,miny,maxy)
ticklocsx <- seq(0,4,1)   # locations of tick marks on x axis
ticklocsy <- seq(50,100,10)    # locations of tick marks on y axis
ticklabelsx <- ticklocsx        # set labels for x ticks
ticklabelsy <- ticklocsy    # set labels for y ticks
par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)  # the 'line' command moves away from the axis, the 'las' command rotates to vertical
title(xlab="Difference in means/SD", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="Percentage correct", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

predp <- 100*d_dprimetoprob(dlist/sqrt(2))
lines(dlist,predp,lwd=3,col='black')

predp <- 100*d_dprimetoprob(dlist)
lines(dlist,predp,lwd=3,lty=2,col=pal2tone[1])

legend(2,65,c('One DV', 'Two DVs'),col=c('black',pal2tone[1]),lwd=3,lty=1:2,box.lwd=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 9)}

par(mfrow=c(1,2), las=1)

meanlist <- c(14,18,16,10)
setA <- rnorm(50,mean=meanlist[1], sd=2)
setB <- rnorm(50,mean=meanlist[2], sd=2)
setC <- rnorm(50,mean=meanlist[3], sd=2)
setD <- rnorm(50,mean=meanlist[4], sd=2)
trainingdata <- data.frame(c(setA,setB),c(setC,setD))
colnames(trainingdata) <- c('DV1','DV2')
traininglabels <- as.factor(ceiling((1:100)/50))

setA <- rnorm(50,mean=meanlist[1], sd=2)
setB <- rnorm(50,mean=meanlist[2], sd=2)
setC <- rnorm(50,mean=meanlist[3], sd=2)
setD <- rnorm(50,mean=meanlist[4], sd=2)
testdata <- data.frame(c(setA,setB),c(setC,setD))
colnames(testdata) <- c('DV1','DV2')

svmFit <- caret::train(trainingdata, traininglabels, method = "svmLinear")
p <- predict(svmFit,newdata = testdata)
# sum(traininglabels==p)

boundaryxvals <- as.vector(kronecker(matrix(1,1,length(seq(0,30,0.1))),seq(0,30,0.1)))
boundaryyvals <- as.vector(t(kronecker(matrix(1,1,length(seq(0,30,0.1))),seq(0,30,0.1))))
boundarydata <- data.frame(boundaryxvals,boundaryyvals)
boundarypredict <- as.numeric(predict(svmFit,newdata = boundarydata))

plotlims <- c(0,30,0,30)  
ticklocsx <- seq(0,30,10)  
ticklocsy <- seq(0,30,10) 
ticklabelsx <- ticklocsx        # set labels for x ticks
ticklabelsy <- ticklocsy    # set labels for y ticks
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])  
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)  

# calculate the convex hull of the smaller area to use in plotting
areaindex <- 3-round(mean(boundarypredict))
xyshape <- matrix(0,nrow=sum(boundarypredict==areaindex),ncol=2)
xyshape[,1] <- as.vector(boundaryxvals[boundarypredict==areaindex])
xyshape[,2] <- as.vector(boundaryyvals[boundarypredict==areaindex])
convhull <- chull(xyshape)
convhull <- c(convhull,convhull[1])

polygon(c(0,30,30,0,0),c(0,0,30,30,0),col=rgb(0.8,0.8,0.8),border=NA)
polygon(xyshape[convhull,1],xyshape[convhull,2],col=rgb(1,1,1),border=NA)
points(setA, setC, pch = 22, col='black', bg='white', cex=1, lwd=1)  
points(setB, setD, pch = 21, col='black', bg=pal2tone[1], cex=1, lwd=1)   
text(2,28,'(a)',cex=1.5)


svmFit <- caret::train(trainingdata, traininglabels, method = "svmRadial")
p <- predict(svmFit,newdata = testdata)
# sum(traininglabels==p)

boundaryxvals <- as.vector(kronecker(matrix(1,1,length(seq(0,30,0.1))),seq(0,30,0.1)))
boundaryyvals <- as.vector(t(kronecker(matrix(1,1,length(seq(0,30,0.1))),seq(0,30,0.1))))
boundarydata <- data.frame(boundaryxvals,boundaryyvals)
boundarypredict <- as.numeric(predict(svmFit,newdata = boundarydata))

plotlims <- c(0,30,0,30)
ticklocsx <- seq(0,30,10)   # locations of tick marks on x axis
ticklocsy <- seq(0,30,10)    # locations of tick marks on y axis
ticklabelsx <- ticklocsx        # set labels for x ticks
ticklabelsy <- ticklocsy    # set labels for y ticks
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])  
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)  # the 'line' command moves away from 

# calculate the convex hull of the smaller area to use in plotting
areaindex <- 3-round(mean(boundarypredict))
xyshape <- matrix(0,nrow=sum(boundarypredict==areaindex),ncol=2)
xyshape[,1] <- as.vector(boundaryxvals[boundarypredict==areaindex])
xyshape[,2] <- as.vector(boundaryyvals[boundarypredict==areaindex])
convhull <- chull(xyshape)
convhull <- c(convhull,convhull[1])

polygon(c(0,30,30,0,0),c(0,0,30,30,0),col=rgb(0.8,0.8,0.8),border=NA)
polygon(xyshape[convhull,1],xyshape[convhull,2],col=rgb(1,1,1),border=NA)
points(setA, setC, pch = 22, col='black', bg='white', cex=1, lwd=1)  
points(setB, setD, pch = 21, col='black', bg=pal2tone[1], cex=1, lwd=1)   
text(2,28,'(b)',cex=1.5)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 8)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 8)}

rotate <- function(x){t(apply(x,2,rev))}
fftshift <- function(im) {im * (-1)^(row(im) + col(im))}  

offsetgaus <- function(n,std,x,y){
  i <- matrix(data = (1-(n/2)):(n/2), nrow=n, ncol=n)
  j <- rotate(i)
  h <- exp(-(((i+x)^2) / (2 * std^2)) - (((j+y)^2) / (2 * std^2)))
  return(h)}

plot(x=NULL,y=NULL,xlim=c(-2,2),ylim=c(0,2),axes=FALSE, ann=FALSE, lwd=2)

vlength <- 6
for (n in 1:4){
  vangle <- (90+(n-1)*45)*pi/180
  g <- offsetgaus(512,2,vlength*cos(vangle),vlength*sin(vangle)) + offsetgaus(512,2,vlength*cos(vangle+pi),vlength*sin(vangle+pi))
  g <- g/max(g)
  spatialstim <- Re(fftshift(fft(fftshift(g), inverse=TRUE)))
  spatialstim <- spatialstim/max(abs(spatialstim))
  spatialstim <- (spatialstim + 1)/2
  spatialstim <- floor(spatialstim*255)/255
  rasterImage(spatialstim,n-3,0,n-2,1)
  rasterImage(spatialstim,n/2-1.5,1,n/2-1,1.5)
  rasterImage(spatialstim,n/4-0.75,1.5,n/4-0.5,1.75)
  rasterImage(spatialstim,n/8-0.375,1.75,n/8-0.25,1.875)
}

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

data(segmentationData)  # load the data set into the Environment
traininglabels <- segmentationData[which(segmentationData$Case=='Train'),3]
testlabels <- segmentationData[which(segmentationData$Case=='Test'),3]

perccor <- NULL
for (n in 1:19){
  trainingdata <- as.matrix(segmentationData[which(segmentationData$Case=='Train'),4:(n+4)])
  testdata <- as.matrix(segmentationData[which(segmentationData$Case=='Test'),4:(n+4)])
  svmFit <- caret::train(trainingdata, traininglabels, method = "svmLinear")
  p <- predict(svmFit,newdata = testdata)
  perccor[n] <- 100*(sum(testlabels==p)/length(testlabels))
}

plot(2:20,perccor,type='l',ylim=c(50,100),lwd=3)
points(2:20,perccor,pch=16)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)
