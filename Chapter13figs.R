
# Note: this chapter uses images from the Bank of Standardised Stimuli (BOSS)
# The images were downloaded from: https://sites.google.com/site/bosstimuli/
# I have reduced the resolution of all images to 256x256 pixels and converted to greyscale
# They are available under a Creative Commons Attribution-Share Alike 3.0 license


# check for any missing settings and install required packages and functions
if (!exists('outputplot')){outputplot <- 2}
if (!exists('nsims')){nsims <- 100000}
if (!exists('pal2tone')){pal2tone <- c('#8783CF','#10069F','#CFCDEC')}  # blue 072

packagelist <- c('pwr','rmeta','MAd','compute.es','lme4','lmerTest','MuMIn','knitr','Hotelling','tictoc','MASS','jpeg','amap','optimbase','optimsimplex','neldermead','signal','pracma','lavaan','semPlot','caret','kernlab','e1071','graphics','RSNNS','psyphy','quickpsy','BayesFactor','pals','colorspace','grImport','PRISMAstatement','rsvg','DiagrammeRsvg','png','data.table','devtools','corrplot','DiagrammeR')
missingpackages <- packagelist[!packagelist %in% installed.packages()[,1]]
if (length(missingpackages)>0){install.packages(missingpackages)}
toinstall <- packagelist[which(!packagelist %in% (.packages()))]
invisible(lapply(toinstall,library,character.only=TRUE))

addalpha <- function(col, alpha=1){apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=alpha))}
flatalpha <- function(col, alpha=1){apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1]*alpha + 1-alpha, x[2]*alpha + 1-alpha, x[3]*alpha + 1-alpha))}

# Chapter 13 figures ---------------------------------------------------------------

chapter <- 13
figno <- 1

if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 10, width = 7.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 10, width = 7.5)}

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(0,3), ylim=c(0,3))

axis(1, at=c(0,3), tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=c(0,2), tck=0.01, lab=F, lwd=2)

title(xlab="Dimension 1", line=1, cex.lab=1.5)
mtext(text = 'Dimension 2', side = 2, at=1, line=1, cex=1.5)

text(0.01,2.9,'(a)',cex=2)
text(0.01,1.9,'(b)',cex=2)

angles <- seq(0,2*pi,length=200)
eigVal <- c(0.05,0.2)
ellBase <- cbind(sqrt(eigVal[1])*cos(angles), sqrt(eigVal[2])*sin(angles)) 

colourlevels <- c(0,0.3,0.6)   
stripeangles <- c(45,75,140)
stripewidths <- c(4,3,2)
stripedense <- c(15,10,20)
eyecols <- c(2,3,1)

for (ccol in 1:3){
  
  for (leg in 1:3){        
    lines(ccol-c(0.5,0.85),2+c(0.25,0.35)*leg*0.6,lwd=3)
    lines(ccol-c(0.85,0.95),2+c(0.35,0.25)*leg*0.6,lwd=3)
    lines(ccol-c(0.5,0.15),2+c(0.25,0.35)*leg*0.6,lwd=3)
    lines(ccol-c(0.15,0.05),2+c(0.35,0.25)*leg*0.6,lwd=3)
  }
  polygon(ellBase[,1]+ccol-0.5, 2+ellBase[,2]+0.5, border=NA, col=pal2tone[4-ccol])
  
  polygon(ellBase[,1]+ccol-0.5, 2+ellBase[,2]+0.5, border=NA, angle=stripeangles[ccol], lwd=stripewidths[ccol], density=stripedense[ccol])
  
  lines(ellBase[,1]+ccol-0.5, 2+ellBase[,2]+0.5, lwd=3)
  
  points(ccol-0.65,2+0.85,pch=21,bg=pal2tone[eyecols[ccol]],cex=4,lwd=3)    
  points(ccol-0.35,2+0.85,pch=21,bg=pal2tone[eyecols[ccol]],cex=4,lwd=3)    
  
}

xvals <- c(rnorm(100,0.5,0.15),rnorm(100,1.5,0.15),rnorm(100,2.5,0.15),runif(100,0,3))
yvals <- c(rnorm(100,1.5,0.2),rnorm(100,0.5,0.2),rnorm(100,1.3,0.1),runif(100,0,2))

points(xvals,yvals,pch=16,col=rgb(0.8,0.8,0.8))

dataset <- data.frame(xvals,yvals)
clustersend <- kmeans(dataset[,1:2],dataset[c(50,150,250),1:2],iter.max=100,algorithm='Lloyd')

for (n in 1:3){arrows(clustersend$centers[n,1],clustersend$centers[n,2],n-0.5,2,lwd=3)}

points(clustersend$centers[,1],clustersend$centers[,2],pch=21,lwd=3,cex=2,bg=pal2tone)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 8, width = 7.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 8, width = 7.5)}

par(mfrow=c(2,2), las=1)

set.seed(1703)
npoints <- 25
gsd <- 0.2
offset<- 0.25
groupA <- matrix(c(rnorm(npoints,sd=gsd)-offset,rnorm(npoints,sd=gsd))-offset,nrow=npoints,ncol=2)
groupB <- matrix(c(rnorm(npoints,sd=gsd)+offset,rnorm(npoints,sd=gsd))+offset,nrow=npoints,ncol=2)
allgroups <- rbind(groupA,groupB)
truegroups <- rep(1:2,each=npoints)
dataset <- data.frame(allgroups,truegroups)
initialclusters <- sample(1:nrow(dataset),2)

plotlims <- c(-1,1,-1,1)  # define the x and y limits of the plot (minx,maxx,miny,maxy)
ticklocsx <- c(-1,1)    # locations of tick marks on x axis
ticklocsy <- c(-1,1)    # locations of tick marks on y axis

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
text(-0.9,0.9,'(a)',cex=1.5)
for (n in 1:2){points(dataset[which(dataset$truegroups==n),1],dataset[which(dataset$truegroups==n),2],pch=16,col=pal2tone[1])}
points(dataset[initialclusters[1],1],dataset[initialclusters[1],2],pch=21,bg='black',cex=2.5,lwd=3)
points(dataset[initialclusters[2],1],dataset[initialclusters[2],2],pch=21,bg='white',cex=2.5,lwd=3)


plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
text(-0.9,0.9,'(b)',cex=1.5)

# manually work out the closest centroid for each data point
initialclosest <- NULL
for (n in 1:nrow(dataset)){
  dist1 <- dist(rbind(dataset[n,1:2],dataset[initialclusters[1],1:2]))
  dist2 <- dist(rbind(dataset[n,1:2],dataset[initialclusters[2],1:2]))
  if (dist1<dist2){initialclosest[n] <- 1}
  if (dist1>dist2){initialclosest[n] <- 2}
}
for (n in 1:nrow(dataset)){lines(c(dataset[initialclusters[initialclosest[n]],1],dataset[n,1]),c(dataset[initialclusters[initialclosest[n]],2],dataset[n,2]),col=rgb(0.8,0.8,0.8))}

for (n in 1:nrow(dataset)){points(dataset[n,1],dataset[n,2],pch=16,col=pal2tone[1])}
points(dataset[initialclusters[1],1],dataset[initialclusters[1],2],pch=21,bg='black',cex=2.5,lwd=3)
points(dataset[initialclusters[2],1],dataset[initialclusters[2],2],pch=21,bg='white',cex=2.5,lwd=3)

newcentroids <- matrix(0,nrow=2,ncol=3)
newcentroids[1,] <- colMeans(dataset[which(initialclosest==1),])
newcentroids[2,] <- colMeans(dataset[which(initialclosest==2),])

# manually work out the closest centroid for each data point
newclosest <- NULL
for (n in 1:nrow(dataset)){
  dist1 <- dist(rbind(dataset[n,1:2],newcentroids[1,]))
  dist2 <- dist(rbind(dataset[n,1:2],newcentroids[2,]))
  if (dist1<dist2){newclosest[n] <- 1}
  if (dist1>dist2){newclosest[n] <- 2}
}

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
text(-0.9,0.9,'(c)',cex=1.5)

for (n in 1:nrow(dataset)){lines(c(newcentroids[newclosest[n],1],dataset[n,1]),c(newcentroids[newclosest[n],2],dataset[n,2]),col=rgb(0.8,0.8,0.8))}

for (n in 1:nrow(dataset)){points(dataset[n,1],dataset[n,2],pch=16,col=pal2tone[1])}

points(newcentroids[1,1],newcentroids[1,2],pch=21,bg='black',cex=2.5,lwd=3)
points(newcentroids[2,1],newcentroids[2,2],pch=21,bg='white',cex=2.5,lwd=3)

clustersend <- kmeans(dataset[,1:2],dataset[initialclusters,1:2],iter.max=100,algorithm='Lloyd')
allcentroid1 <- matrix(0,nrow=clustersend$iter+1,ncol=2)
allcentroid2 <- matrix(0,nrow=clustersend$iter+1,ncol=2)
allcentroid1[1,] <- as.numeric(dataset[initialclusters[1],1:2])
allcentroid2[1,] <- as.numeric(dataset[initialclusters[2],1:2])
for (n in 1:clustersend$iter){
  clusters <- kmeans(dataset[,1:2],dataset[initialclusters,1:2],iter.max=n,algorithm='Lloyd')
  allcentroid1[n+1,] <- clusters$centers[1,]
  allcentroid2[n+1,] <- clusters$centers[2,]
}

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
text(-0.9,0.9,'(d)',cex=1.5)

for (n in 1:nrow(dataset)){lines(c(clusters$centers[clusters$cluster[n],1],dataset[n,1]),c(clusters$centers[clusters$cluster[n],2],dataset[n,2]),col=rgb(0.8,0.8,0.8))}

for (n in 1:2){points(dataset[which(clusters$cluster==n),1],dataset[which(clusters$cluster==n),2],pch=16,col=pal2tone[1])}

lines(allcentroid1[,1],allcentroid1[,2],lwd=3,col='black')
lines(allcentroid2[,1],allcentroid2[,2],lwd=3,col='black')

points(allcentroid1[1:3,1],allcentroid1[1:3,2],pch=21,bg='black',cex=c(1.5,1.5,2.5),lwd=3)
points(allcentroid2[1:3,1],allcentroid2[1:3,2],pch=21,bg='white',cex=c(1.5,1.5,2.5),lwd=3)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 8, width = 7.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 8, width = 7.5)}

par(mfrow=c(2,2), las=1)

set.seed(1703)
npoints <- 25
gsd <- 0.2
offset<- 0.5
groupA <- matrix(c(rnorm(npoints,sd=gsd),rnorm(npoints,sd=gsd)),nrow=npoints,ncol=2)
groupB <- matrix(c(rnorm(npoints,sd=gsd),offset+rnorm(npoints,sd=gsd)),nrow=npoints,ncol=2)
groupC <- matrix(c(offset+rnorm(npoints,sd=gsd),rnorm(npoints,sd=gsd)),nrow=npoints,ncol=2)
groupD <- matrix(c(-offset+rnorm(npoints,sd=gsd),rnorm(npoints,sd=gsd)),nrow=npoints,ncol=2)
groupE <- matrix(c(rnorm(npoints,sd=gsd),-offset+rnorm(npoints,sd=gsd)),nrow=npoints,ncol=2)
allgroups <- rbind(groupA,groupB,groupC,groupD,groupE)
truegroups <- rep(1:5,each=npoints)
dataset <- data.frame(allgroups,truegroups)

# groupcolours <- NULL
# groupcolours[1] <- rgb(178/255,17/255,23/255)
# groupcolours[2] <- rgb(83/255,198/255,111/255)
# groupcolours[3] <- rgb(6/255,172/255,208/255)
# groupcolours[4] <- rgb(161/255,130/255,165/255)
# groupcolours[5] <- rgb(203/255,188/255,85/255)

groupcolours <- c('grey',pal2tone[1],'black',pal2tone[2:3])

plotlims <- c(-1,1,-1,1)  # define the x and y limits of the plot (minx,maxx,miny,maxy)
ticklocsx <- c(-1,1)    # locations of tick marks on x axis
ticklocsy <- c(-1,1)    # locations of tick marks on y axis
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
text(-0.9,0.9,'(a)',cex=1.5)
for (n in 1:5){points(dataset[which(dataset$truegroups==n),1],dataset[which(dataset$truegroups==n),2],pch=16,col=flatalpha(groupcolours[n],0.5))}

clusters <- kmeans(dataset[,1:2],5)
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
text(-0.9,0.9,'(b)',cex=1.5)

for (n in 1:(5*npoints)){lines(c(clusters$centers[clusters$cluster[n],1],dataset[n,1]),c(clusters$centers[clusters$cluster[n],2],dataset[n,2]),col=rgb(0.8,0.8,0.8))}

for (n in 1:5){points(dataset[which(dataset$truegroups==n),1],dataset[which(dataset$truegroups==n),2],pch=16,col=flatalpha(groupcolours[n],0.5))}

for (n in 1:5){points(clusters$centers[n,1],clusters$centers[n,2],pch=16,col='black',cex=2)}


clusters <- kmeans(dataset[,1:2],2)
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
text(-0.9,0.9,'(c)',cex=1.5)

for (n in 1:(5*npoints)){lines(c(clusters$centers[clusters$cluster[n],1],dataset[n,1]),c(clusters$centers[clusters$cluster[n],2],dataset[n,2]),col=rgb(0.8,0.8,0.8))}

for (n in 1:5){points(dataset[which(dataset$truegroups==n),1],dataset[which(dataset$truegroups==n),2],pch=16,col=flatalpha(groupcolours[n],0.5))}

for (n in 1:2){points(clusters$centers[n,1],clusters$centers[n,2],pch=16,col='black',cex=2)}


clusters <- kmeans(dataset[,1:2],10)
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
text(-0.9,0.9,'(d)',cex=1.5)

for (n in 1:(5*npoints)){lines(c(clusters$centers[clusters$cluster[n],1],dataset[n,1]),c(clusters$centers[clusters$cluster[n],2],dataset[n,2]),col=rgb(0.8,0.8,0.8))}

for (n in 1:5){points(dataset[which(dataset$truegroups==n),1],dataset[which(dataset$truegroups==n),2],pch=16,col=flatalpha(groupcolours[n],0.5))}

for (n in 1:10){points(clusters$centers[n,1],clusters$centers[n,2],pch=16,col='black',cex=2)}

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

resamples <- 1000
rsquared <- NULL
AIC <- matrix(0,nrow=resamples,ncol=10)
BIC <- matrix(0,nrow=resamples,ncol=10)
for (r in 1:resamples){
  groupA <- matrix(c(rnorm(npoints,sd=gsd),rnorm(npoints,sd=gsd)),nrow=npoints,ncol=2)
  groupB <- matrix(c(rnorm(npoints,sd=gsd),offset+rnorm(npoints,sd=gsd)),nrow=npoints,ncol=2)
  groupC <- matrix(c(offset+rnorm(npoints,sd=gsd),rnorm(npoints,sd=gsd)),nrow=npoints,ncol=2)
  groupD <- matrix(c(-offset+rnorm(npoints,sd=gsd),rnorm(npoints,sd=gsd)),nrow=npoints,ncol=2)
  groupE <- matrix(c(rnorm(npoints,sd=gsd),-offset+rnorm(npoints,sd=gsd)),nrow=npoints,ncol=2)
  allgroups <- rbind(groupA,groupB,groupC,groupD,groupE)
  truegroups <- rep(1:5,each=npoints)
  datasetr <- data.frame(allgroups,truegroups)
  
  for (n in 1:10){
    clusters <- kmeans(datasetr[,1:2],n)
    AIC[r,n] <- clusters$tot.withinss + 2*2*n
    BIC[r,n] <- clusters$tot.withinss + 0.5*log(npoints*5)*2*n
  }
}

ACIs <- matrix(0,nrow=2,ncol=10)
for (n in 1:10){ACIs[,n] <- quantile(AIC[,n],probs=c(0.025,0.975))}
BCIs <- matrix(0,nrow=2,ncol=10)
for (n in 1:10){BCIs[,n] <- quantile(BIC[,n],probs=c(0.025,0.975))}

plotlims <- c(1,10,20,50)  # define the x and y limits of the plot (minx,maxx,miny,maxy)
ticklocsx <- 1:10    # locations of tick marks on x axis
ticklocsy <- seq(20,50,10)    # locations of tick marks on y axis
ticklabelsx <- ticklocsx        # set labels for x ticks
ticklabelsy <- ticklocsy    # set labels for y ticks
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])  
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="k", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="AIC/BIC score", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

polygon(c(1:10,10:1),c(ACIs[1,],ACIs[2,10:1]),col=rgb(0.9,0.9,0.9),border=NA)
polygon(c(1:10,10:1),c(BCIs[1,],BCIs[2,10:1]),col=pal2tone[3],border=NA)

lines(1:10,colMeans(AIC),lwd=3,col='black')
lines(1:10,colMeans(BIC),lty=2,lwd=3,col=pal2tone[2])

legend(1,50, c("BIC","AIC"), cex=1, col=c(pal2tone[2],"black"),lty=c(2,1), lwd=3, box.lwd=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 13)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 13)}

par(mfrow=c(1,3), las=1)

load('data/dinooutlines.RData')
dinosaurs <- read.csv('data/dinosaurs.csv')
dinosaurs$sizeratio <- dinosaurs$Height/dinosaurs$Length

plotlims <- c(-1,0.2,-3,3)  # define the x and y limits of the plot (minx,maxx,miny,maxy)
ticklocsx <- seq(-1,0.2,0.2)    # locations of tick marks on x axis
ticklocsy <- seq(-3,3,1)    # locations of tick marks on y axis
ticklabelsx <- ticklocsx        # set labels for x ticks
ticklabelsy <- ticklocsy   # set labels for y ticks

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx,line=0.4)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Height/Length (log)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)    # titles for axes
title(ylab="Weight (log)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)
text(-0.9,2.5,'(a)',cex=2)

polygon(dinoout[1,,1]/5 - 0.65,1.2*dinoout[1,,2] + 2,border=NA,col=pal2tone[1])
polygon(dinoout[2,,1]/5 - 0.9,1.5*dinoout[2,,2] - 1.7,border=NA,col='black')
polygon(dinoout[3,,1]/5,dinoout[3,,2] - 2.8,border=NA,col='black')
polygon(dinoout[4,,1]/5 - 0.95,dinoout[4,,2] + 0.8,border=NA,col=pal2tone[1])

points(log10(dinosaurs$sizeratio[dinosaurs$Diet==1]),log10(dinosaurs$Weight[dinosaurs$Diet==1]),pch=15,cex=1,col='black')
points(log10(dinosaurs$sizeratio[dinosaurs$Diet==0]),log10(dinosaurs$Weight[dinosaurs$Diet==0]),pch=16,cex=1,col=pal2tone[1])

legend(-0.2,3, c("Herbivore","Carnivore"), cex=1, col=c(pal2tone[1],"black"),pch=16:15, box.lwd=2)



plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx,line=0.4)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Height/Length (log)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)    # titles for axes
title(ylab="Weight (log)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)
text(-0.9,2.5,'(b)',cex=2)

clusters <- kmeans(log10(dinosaurs[,6:5]),2)

for (n in 1:(nrow(dinosaurs))){lines(c(clusters$centers[clusters$cluster[n],1],log10(dinosaurs[n,6])),c(clusters$centers[clusters$cluster[n],2],log10(dinosaurs[n,5])),col=rgb(0.8,0.8,0.8))}

points(log10(dinosaurs$sizeratio[dinosaurs$Diet==1]),log10(dinosaurs$Weight[dinosaurs$Diet==1]),pch=15,cex=1,col='black')
points(log10(dinosaurs$sizeratio[dinosaurs$Diet==0]),log10(dinosaurs$Weight[dinosaurs$Diet==0]),pch=16,cex=1,col=pal2tone[1])

for (n in 1:2){points(clusters$centers[n,1],clusters$centers[n,2],pch=21,bg='white',cex=2)}


plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx,line=0.4)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Height/Length (log)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)    # titles for axes
title(ylab="Weight (log)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)
text(-0.9,2.5,'(c)',cex=2)

clusters <- kmeans(log10(dinosaurs[,6:5]),3)

for (n in 1:(nrow(dinosaurs))){lines(c(clusters$centers[clusters$cluster[n],1],log10(dinosaurs[n,6])),c(clusters$centers[clusters$cluster[n],2],log10(dinosaurs[n,5])),col=rgb(0.8,0.8,0.8))}

points(log10(dinosaurs$sizeratio[dinosaurs$Diet==1]),log10(dinosaurs$Weight[dinosaurs$Diet==1]),pch=15,cex=1,col='black')
points(log10(dinosaurs$sizeratio[dinosaurs$Diet==0]),log10(dinosaurs$Weight[dinosaurs$Diet==0]),pch=16,cex=1,col=pal2tone[1])

for (n in 1:3){points(clusters$centers[n,1],clusters$centers[n,2],pch=21,bg='white',cex=2)}

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 9)}

set.seed(1703)
par(mfrow=c(1,2), las=1)

edata <- matrix(runif(10),nrow=5,ncol=2)

plotlims <- c(0,1,0,1)  # define the x and y limits of the plot (minx,maxx,miny,maxy)
ticklocsx <- c(0,1)    # locations of tick marks on x axis
ticklocsy <- c(0,1)    # locations of tick marks on y axis
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
text(0.05,0.95,'(a)',cex=1.5)

points(edata[,1],edata[,2],pch=16)

for (n in 1:5){for (m in 1:5){lines(c(edata[n,1],edata[m,1]),c(edata[n,2],edata[m,2]))}}
xoffset <- c(0.05,0.05,0.05,0,0.05)
yoffset <- c(0,0,0,-0.05,0)
text(edata[,1]+xoffset,edata[,2]+yoffset,1:5,cex=1.5)

plotlims <- c(0.5,5.5,0.5,5.5)  # define the x and y limits of the plot (minx,maxx,miny,maxy)
ticklocs <- seq(1,5)    # locations of tick marks on x axis
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocs, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocs, tck=0.01, lab=F, lwd=2)
mtext(text = ticklocs, side = 1, at=ticklocs) 
mtext(text = ticklocs, side = 2, at=ticklocs[5:1], line=0.2, las=1) 
text(5.3,5.3,'(b)',cex=1.5)

vectorlength <- matrix(0,nrow=5,ncol=5)
for (n in 1:5){for (m in 1:5){
  vectorlength[n,m] <- dist(rbind(edata[n,],edata[m,]))
}}

vectorlength <- 1-(vectorlength/max(vectorlength))
for (n in 1:5){for (m in 1:n){
  polygon((6-n+c(-0.5,0.5,0.5,-0.5)),m+c(-0.5,-0.5,0.5,0.5),col=rgb(vectorlength[n,m],vectorlength[n,m],vectorlength[n,m]))
}}

ramp2 <- colorRamp(c("white","black"))  # create a ramp from one colour to another
colmatrix2 <- rgb(ramp2(seq(0, 1, length = 101)), max = 255)
z=matrix(1:101,nrow=1)
x=c(5,5.5)
y=seq(3,5,len=101)
image(x,y,z,col=colmatrix2,add=TRUE,useRaster=TRUE)
text(4.9,3,'0',cex=1.2)
text(4.9,5,'1',cex=1.2)
text(4.8,4,'Distance',cex=1,srt=90)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 9)}

par(mfrow=c(1,2), las=1)

npoints <- 1000

set.seed(1703)
colourdata <- matrix(runif(npoints*4),nrow=npoints,ncol=4)
coldist1 <- dist(colourdata[,1:3])
scaledxy1 <- cmdscale(coldist1)

plotlims <- c(-1,1,-1,1)  # define the x and y limits of the plot (minx,maxx,miny,maxy)
ticklocsx <- c(-1,1)    # locations of tick marks on x axis
ticklocsy <- c(-1,1)    # locations of tick marks on y axis

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
title(xlab="Dimension 1", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="Dimension 2", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)
title(expression('RGB'))
text(-0.9,0.9,'(a)',cex=1.5)
for (n in 1:npoints){points(scaledxy1[n,1],scaledxy1[n,2],pch=16,col=rgb(colourdata[n,1],colourdata[n,2],colourdata[n,3]),cex=0.5)}

coldist2 <- dist(colourdata)
scaledxy2 <- cmdscale(coldist2)

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
title(xlab="Dimension 1", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="Dimension 2", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)
title(expression(paste('RGB',alpha,sep='')))
text(-0.9,0.9,'(b)',cex=1.5)
for (n in 1:npoints){points(scaledxy2[n,1],scaledxy2[n,2],pch=16,col=flatalpha(rgb(colourdata[n,1],colourdata[n,2],colourdata[n,3]),colourdata[n,4]),cex=0.5)}

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 9)}

par(mfrow=c(1,2), las=1)

coldist1 <- dist(colourdata[1:500,1:3])
scaledxy1 <- cmdscale(coldist1)
shep1 <- Shepard(coldist1, scaledxy1)

coldist2 <- dist(colourdata[1:500,])
scaledxy2 <- cmdscale(coldist2)
shep2 <- Shepard(coldist2, scaledxy2)

plotlims <- c(0,1.6,0,1.6)  
ticklocsx <- c(0,1.6)    # locations of tick marks on x axis
ticklocsy <- c(0,1.6)    # locations of tick marks on y axis
# par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
title(xlab="Original distance", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5) 
title(ylab="Rescaled distance", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)
title(expression('RGB'))
text(0.1,1.5,'(a)',cex=1.5)
points(shep1$x,shep1$yf,pch='.')

plotlims <- c(0,1.6,0,1.6) 
ticklocsx <- c(0,1.6)    # locations of tick marks on x axis
ticklocsy <- c(0,1.6)    # locations of tick marks on y axis
# par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
title(xlab="Original distance", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)   
title(ylab="Rescaled distance", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)
title(expression(paste('RGB',alpha,sep='')))
text(0.1,1.5,'(b)',cex=1.5)
points(shep2$x,shep2$yf,pch='.')

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

viruses <- read.csv('data/viruses.csv')

virdist <- dist(scale(viruses[,3:7]))
scaledxy <- cmdscale(virdist)

plotlims <- c(-4,4,-4,4)  # define the x and y limits of the plot (minx,maxx,miny,maxy)
ticklocsx <- seq(-4,4,1)    # locations of tick marks on x axis
ticklocsy <- seq(-4,4,1)    # locations of tick marks on y axis
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
title(xlab="Dimension 1", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="Dimension 2", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)
points(scaledxy[,1],scaledxy[,2],pch=16,cex=1)
for (n in 1:nrow(viruses)){text(scaledxy[n,1],scaledxy[n,2],viruses[n,2],pos=4)}

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 5)}

shep3 <- Shepard(virdist, scaledxy)

plotlims <- c(0,6,0,6)  # define the x and y limits of the plot (minx,maxx,miny,maxy)
ticklocsx <- c(0,6)    # locations of tick marks on x axis
ticklocsy <- c(0,6)    # locations of tick marks on y axis
par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
title(xlab="Original distance", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="Rescaled distance", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

points(shep3$x,shep3$yf,pch=16,cex=0.8)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){cairo_ps(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), fallback_resolution=300, onefile = FALSE, height = 5, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 9)}

load('data/gistdata.RData')
set.seed(1703)

normgist <- gist
for (n in 1:ncol(normgist)){normgist[,n] <- normgist[,n]/sum(normgist[,n])}
for (n in 1:nrow(normgist)){normgist[n,] <- normgist[n,]/sum(normgist[n,])}

if (!file.exists('data/gistPCA.RData')){
pca <- prcomp(normgist,scale=TRUE)
save(file='data/gistPCA.RData',list='pca')
}
load('data/gistPCA.RData')

loadings <- pca$x[,1:20]
clusters <- kmeans(loadings,10)
npoints <- nrow(loadings)

alldists <- NULL
for (n in 1:npoints){
  clustcent <- clusters$centers[clusters$cluster[n],]
  thisvect <- loadings[n,]
  alldists[n] <- dist(rbind(clustcent,thisvect))
}

best24 <- matrix(0,nrow=10,ncol=24)
for (clust in 1:10){
  i <- which(clusters$cluster==clust)
  temp <- sort(alldists[i],index.return=TRUE)
  best24[clust,] <- i[temp$ix[1:24]]
}
clusterfiles <- gistnames[t(best24)]

reducedloadings <- loadings[t(best24),]
cordist <- Dist(reducedloadings,method='correlation')
# scaledxy <- cmdscale(cordist)
mdsout <- isoMDS(cordist)
scaledxy <- mdsout$points
npoints <- nrow(scaledxy)

eccentricity <- 1.1
clustermeansx <- NULL
clustermeansy <- NULL
for (n in 1:10){
  clustermeansx[n] <- mean(scaledxy[(24*(n-1)+1):(24*n),1])
  clustermeansy[n] <- mean(scaledxy[(24*(n-1)+1):(24*n),2])
}
clusterangles <- 90 - (atan2(clustermeansy,clustermeansx)*180/pi)
clusterangles[which(clusterangles<0)] <- clusterangles[which(clusterangles<0)] + 360
temp <- sort(clusterangles,index.return = TRUE)
clusterindicesordered <- temp$ix
imclustangs <- seq(15,360,36)
imclustx <- sin(imclustangs*pi/180)*eccentricity
imclusty <- cos(imclustangs*pi/180)*eccentricity
reverseangs <- 90-(atan2(imclusty,imclustx)*180/pi)
reverseangs[which(reverseangs<0)] <- reverseangs[which(reverseangs<0)] + 360

par(mfrow=c(1,2), las=1)


plotlims <- c(-1,1,-1,1)
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
text(-0.95,0.95,'(a)',cex=1.5)

toplot <- normgist/max(normgist)
toplot <- toplot * 100
toplot[which(toplot>1)] <- 1
colim <- array(0,dim=c(2761,4096,3))
colim[,,1] <- 1-toplot
colim[,,2] <- toplot
colim[,,3] <- 0
xcos <- c(-0.8,-0.4)
ycos <- c(-0.9,0.9)
rasterImage(colim,xcos[1],ycos[1],xcos[2],ycos[2])
text(-0.6,0.95,'Gist',cex=1,adj=0.5)
text(-0.6,-1,'4096',cex=1,adj=0.5)
text(-0.92,0,'2761',cex=1,srt=90,adj=0.5)

lines(c(-0.85,-0.85),c(-0.85,0.85),lwd=2)
lines(c(-0.85,-0.82),c(0.85,0.85),lwd=2)
lines(c(-0.85,-0.82),c(-0.85,-0.85),lwd=2)

lines(c(-0.78,-0.42),c(-0.95,-0.95),lwd=2)
lines(c(-0.78,-0.78),c(-0.95,-0.92),lwd=2)
lines(c(-0.42,-0.42),c(-0.95,-0.92),lwd=2)

toplot <- loadings-min(loadings)
toplot <- toplot/max(toplot)
colim <- array(0,dim=c(2761,20,3))
colim[,,1] <- toplot
colim[,,2] <- toplot
colim[,,3] <- 1-toplot
xcos <- c(-0.1,0.3)
ycos <- c(-0.9,0.9)
rasterImage(colim,xcos[1],ycos[1],xcos[2],ycos[2])
text(0.1,0.95,'PC 1-20',cex=1,adj=0.5)
text(0.1,-1,'20',cex=1,adj=0.5)
text(-0.22,0,'2761',cex=1,srt=90,adj=0.5)

lines(c(-0.15,-0.15),c(-0.85,0.85),lwd=2)
lines(c(-0.15,-0.12),c(0.85,0.85),lwd=2)
lines(c(-0.15,-0.12),c(-0.85,-0.85),lwd=2)

lines(c(-0.08,0.28),c(-0.95,-0.95),lwd=2)
lines(c(-0.08,-0.08),c(-0.95,-0.92),lwd=2)
lines(c(0.28,0.28),c(-0.95,-0.92),lwd=2)

toplot <- reducedloadings-min(reducedloadings)
toplot <- toplot/max(toplot)
colim <- array(0,dim=c(240,20,3))
colim[,,1] <- toplot
colim[,,2] <- toplot
colim[,,3] <- 1-toplot
xcos <- c(0.6,1)
ycos <- c(0.6,0.9)
rasterImage(colim[1:24,,],xcos[1],ycos[1],xcos[2],ycos[2])
ycos <- c(0.225,0.525)
rasterImage(colim[25:48,,],xcos[1],ycos[1],xcos[2],ycos[2])
ycos <- c(-0.15,0.15)
rasterImage(colim[49:72,,],xcos[1],ycos[1],xcos[2],ycos[2])
ycos <- c(-0.525,-0.225)
rasterImage(colim[73:96,,],xcos[1],ycos[1],xcos[2],ycos[2])
ycos <- c(-0.9,-0.6)
rasterImage(colim[97:120,,],xcos[1],ycos[1],xcos[2],ycos[2])

text(0.8,0.95,'Clusters',cex=1,adj=0.5)
text(0.8,-1,'20',cex=1,adj=0.5)
text(0.48,0,'24',cex=1,srt=90,adj=0.5)

lines(c(0.62,0.98),c(-0.95,-0.95),lwd=2)
lines(c(0.62,0.62),c(-0.95,-0.92),lwd=2)
lines(c(0.98,0.98),c(-0.95,-0.92),lwd=2)

lines(c(0.55,0.55),c(-0.15,0.15),lwd=2)
lines(c(0.55,0.58),c(0.15,0.15),lwd=2)
lines(c(0.55,0.58),c(-0.15,-0.15),lwd=2)


colvect <- c('red','darkgreen','blue','orange','purple','grey','pink','brown','black','yellow')

plotlims <- c(-1.5,1.5,-1.5,1.5)  # define the x and y limits of the plot (minx,maxx,miny,maxy)
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
for (n in 1:npoints){points(scaledxy[n,1],scaledxy[n,2],pch=16,col=colvect[ceiling(n/24)],cex=0.6)}
for (n in 1:10){lines(c(clustermeansx[clusterindicesordered[n]],imclustx[n]),c(clustermeansy[clusterindicesordered[n]],imclusty[n]),col=colvect[clusterindicesordered[n]],lwd=3)}
text(-1.4,1.4,'(b)',cex=1.5)

innerouter <- c(1.1,1.6)
for (n in 1:10){
  wedgeangs <- imclustangs[n] + (-15:15)
  innerx <- sin(wedgeangs*pi/180)*innerouter[1]
  innery <- cos(wedgeangs*pi/180)*innerouter[1]
  outerx <- sin(wedgeangs*pi/180)*innerouter[2]
  outery <- cos(wedgeangs*pi/180)*innerouter[2]
  lines(c(innerx,outerx[length(outerx):1],innerx[1]),c(innery,outery[length(outerx):1],innery[1]),col=colvect[clusterindicesordered[n]],lwd=3)
}

aspratio <- 1  # this is the aspect ratio of the output pdf
imwidth <- 0.12
clusterfilesmatrix <- matrix(clusterfiles,nrow=24,ncol=10)

ecclist <- c(1.2,1.35,1.5)
angles <- seq(6,360,6)
allx <- matrix(0,nrow=3,ncol=length(angles))
ally <- matrix(0,nrow=3,ncol=length(angles))
for (n in 1:3){
  allx[n,] <- sin(angles*pi/180)*ecclist[n]
  ally[n,] <- cos(angles*pi/180)*ecclist[n]}

n <- 0
i <- 0
for (clustno in 1:10){
  for (exampleno in 1:4){
    i <- i+1
    for (layer in 1:3){
      e1 <- readJPEG(paste('images/BOSSr/',clusterfilesmatrix[exampleno+((layer-1)*4),clusterindicesordered[clustno]],sep=''))
      colim <- array(0,dim=c(256,256,4))
      alphamask <- e1*0 + 1
      alphamask[which(e1==1)] <- 0
      colim[,,1] <- e1
      colim[,,2] <- e1
      colim[,,3] <- e1
      colim[,,4] <- alphamask
      rasterImage(colim,allx[layer,i]-0.5*imwidth,ally[layer,i]-0.5*imwidth,allx[layer,i]+0.5*imwidth*aspratio,ally[layer,i]+0.5*imwidth)
    }
  }
  i <- i + 2
}

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

clusters <- kmeans(dataset[,1:2],5)

# set up an empty plot axis
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(-1,1), ylim=c(-1,1))
axis(1, at=c(-1,1), tck=0.01, lab=F, lwd=2)
axis(2, at=c(-1,1), tck=0.01, lab=F, lwd=2)

# draw lines between each cluster centre and the assigned data point
for (n in 1:(nrow(dataset))){lines(c(clusters$centers[clusters$cluster[n],1],dataset[n,1]),c(clusters$centers[clusters$cluster[n],2],dataset[n,2]),col='grey')}

# draw individual data points
points(dataset[,1],dataset[,2],pch=16,col=pal2tone[1])

# draw the cluster centroids
points(clusters$centers[,1],clusters$centers[,2],pch=16,cex=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4, width = 11)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4, width = 11)}

par(mfrow=c(1,3), las=1)

algorithmlist <- c('Hartigan-Wong','Lloyd','MacQueen')
for (plt in 1:3){
  clusters <- kmeans(dataset[,1:2],5,algorithm = algorithmlist[plt])
  plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(-1,1), ylim=c(-1,1))
  axis(1, at=c(-1,1), tck=0.01, lab=F, lwd=2)
  axis(2, at=c(-1,1), tck=0.01, lab=F, lwd=2)
  title(paste(algorithmlist[plt],'algorithm'))
  for (n in 1:(nrow(dataset))){lines(c(clusters$centers[clusters$cluster[n],1],dataset[n,1]),c(clusters$centers[clusters$cluster[n],2],dataset[n,2]),col='grey')}
  points(dataset[,1],dataset[,2],pch=16,col=pal2tone[1])
  points(clusters$centers[,1],clusters$centers[,2],pch=16,cex=2)
}

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)

