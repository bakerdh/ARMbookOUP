

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

doclustcorr <- function(data,nresamples,clustformthresh,clustthresh){
  
  clustout <- NULL
  N <- nrow(data)    
  m <- ncol(data)
  
  allp <- NULL
  allt <- NULL
  for (n in 1:m){
    output <- t.test(data[,n])
    allp[n] <- output$p.value
    allt[n] <- output$statistic
  }
  
  clusterstarts <- NULL
  clusterends <- NULL
  nclusters <- 0
  incluster = 0
  for (n in 1:m){
    if (allp[n]<clustformthresh){
      if (incluster==0){
        nclusters <- nclusters + 1
        clusterstarts[nclusters] <- n
        incluster <- 1
      }
    }
    if (allp[n]>=clustformthresh){
      if (incluster==1){
        clusterends[nclusters] <- n-1
        incluster <- 0
      }
    }
  }
  if (incluster>0 & nclusters>0){clusterends[nclusters] <- m}
  
  if (nclusters>0){
    summedt <- NULL
    for (n in 1:nclusters){summedt[n] <- sum(allt[clusterstarts[n]:clusterends[n]])}
    
    biggestcluster <- which(summedt==max(summedt))
    cstart <- clusterstarts[biggestcluster]
    cend <- clusterends[biggestcluster]
    signlabels <- sign(c(-(1:(N/2)),(1:(N/2))))
    nullT <- NULL
    for (i in 1:nresamples){
      tsum <- 0
      randsigns <- sample(signlabels,replace=TRUE) 
      for (n in cstart:cend){
        tempdata <- data[,n]*randsigns
        tsum <- tsum + (mean(tempdata)/(sd(tempdata)/sqrt(N)))
      }
      nullT[i] <- tsum
    }
    distlims <- quantile(nullT,c(clustthresh/2,1-(clustthresh/2)))
    sigclusts <- c(which(summedt<distlims[1]),which(summedt>distlims[2]))
    
    if (length(sigclusts)>0){clustout <- matrix(0,nrow=2,ncol=length(sigclusts))
    clustout[1,] <- clusterstarts[sigclusts]
    clustout[2,] <- clusterends[sigclusts]}
  }
  
  return(clustout)}

load('data/ERPdata.RData')
load('data/EEGmontage.RData')


# Chapter 15 figures ---------------------------------------------------------------

chapter <- 15
figno <- 1

if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

m <- 1:100
alpha <- 0.05
alphahat <- 1-(1-alpha)^m

plotlims <- c(0,100,0,1)
ticklocsx <- seq(0,100,10)    # locations of tick marks on x axis
ticklocsy <- seq(0,1,0.2)    # locations of tick marks on y axis
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklocsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklocsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Number of tests (m)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Familywise error rate", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

lines(m,alphahat,lwd=3)
lines(c(0,100),c(0.05,0.05),lty=2,lwd=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 9)}

par(mfrow=c(1,2), las=1)

greyvect <- c(rgb(0,0,0),pal2tone[c(2,1,3)])
N <- 3:200
d <- 0.5
mvect <- c(1,3,10,100)

alpha <- 0.05
alphahat <- alpha/mvect

plotlims <- c(0,200,0,1)
ticklocsx <- seq(0,200,50)    # locations of tick marks on x axis
ticklocsy <- seq(0,1,0.2)    # locations of tick marks on y axis
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklocsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklocsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Sample size (N)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Power", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

lines(c(0,200),c(0.8,0.8),lty=2,lwd=2)

pvect <- pwr.t.test(n = N, d=d, sig.level=alphahat[1])
lines(N,pvect$power,lwd=3, col=greyvect[1])
pvect <- pwr.t.test(n = N, d=d, sig.level=alphahat[2])
lines(N,pvect$power,lwd=3, col=greyvect[2])
pvect <- pwr.t.test(n = N, d=d, sig.level=alphahat[3])
lines(N,pvect$power,lwd=3, col=greyvect[3])
pvect <- pwr.t.test(n = N, d=d, sig.level=alphahat[4])
lines(N,pvect$power,lwd=3, col=greyvect[4])

avect <- pwr.t.test(power=0.8, d=d, sig.level=alphahat[1])
arrows(ceiling(avect$n),0.8,ceiling(avect$n),0,lwd=2,length=0.1)
avect <- pwr.t.test(power=0.8, d=d, sig.level=alphahat[3])
arrows(ceiling(avect$n),0.8,ceiling(avect$n),0,lwd=2,col=greyvect[3],length=0.1)

text(-10,0.95,'(a)  d = 0.5',cex=1.5, pos=4)

legend(115,0.35, c("m = 1","m = 3","m = 10", "m = 100"), cex=1, col=greyvect, lty=1, lwd=3, box.lwd=2)


plotlims <- c(0,1,0,1)
ticklocsx <- seq(0,1,0.2)    # locations of tick marks on x axis
ticklocsy <- seq(0,1,0.2)    # locations of tick marks on y axis
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklocsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklocsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Effect size (d)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Power", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

lines(c(0,1),c(0.8,0.8),lty=2,lwd=2)

N <- 50
dvect <- seq(0,1,0.01)
pvect <- pwr.t.test(n = N, d=dvect, sig.level=alphahat[1])
lines(dvect,pvect$power,lwd=3, col=greyvect[1])
pvect <- pwr.t.test(n = N, d=dvect, sig.level=alphahat[2])
lines(dvect,pvect$power,lwd=3, col=greyvect[2])
pvect <- pwr.t.test(n = N, d=dvect, sig.level=alphahat[3])
lines(dvect,pvect$power,lwd=3, col=greyvect[3])
pvect <- pwr.t.test(n = N, d=dvect, sig.level=alphahat[4])
lines(dvect,pvect$power,lwd=3, col=greyvect[4])

avect <- pwr.t.test(power=0.8, n=N, sig.level=alphahat[1])
arrows(avect$d,0.8,avect$d,0,lwd=2,length=0.1)
avect <- pwr.t.test(power=0.8, n=N, sig.level=alphahat[4])
arrows(avect$d,0.8,avect$d,0,lwd=2,col=greyvect[4],length=0.1)

text(-0.05,0.95,'(b)  N = 50',cex=1.5, pos=4)

legend(110,0.35, c("m = 1","m = 3","m = 10", "m = 100"), cex=1, col=greyvect, lty=1, lwd=3, box.lwd=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

m <- 100
j <- 1:100
alpha <- 0.05
holm <- alpha/(m - (j-1))
bonf <- alpha/max(m)
benj <- (alpha*j)/m

plotlims <- c(0,100,-66,-24)
ticklocsx <- seq(0,100,10)    # locations of tick marks on x axis
ticklocsy <- seq(-66,-26,20)    # locations of tick marks on y axis
tickvalsy <- c('0.0005','0.005','0.05')
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklocsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = tickvalsy, side = 2, at=ticklocsy, line=0.2)
title(xlab="Ranked position of test (j)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Significance threshold", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

lines(c(0,100),20*log10(c(bonf,bonf)),lwd=3)
lines(j,20*log10(holm),lwd=3,lty=2)
lines(j,20*log10(benj),lwd=3,lty=3)

text(70,-63,'Bonferroni',cex=1.5, pos=4)
text(33,-53,'Holm-Bonferroni',cex=1.5, pos=4)
text(0,-30,'Benjamini-Hochberg',cex=1.5, pos=4)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

timevals <- -199:1000
allt <- NULL
allp <- NULL
for (n in 1:1200){
  output <- t.test(ERPdata[,2,n],ERPdata[,1,n],paired=TRUE)
  allt[n] <- output$statistic
  allp[n] <- output$p.value
}

meanERP <- apply(ERPdata,c(2,3),mean)
seERP <- apply(ERPdata,c(2,3),sd)/sqrt(38)

plotlims <- c(-200,1000,-5,5)
ticklocsx <- seq(-200,1000,200)    # locations of tick marks on x axis
ticklocsy <- seq(-5,5,1)    # locations of tick marks on y axis
tickvalsy <- c('',-4,'',-2,'',0,'',2,'',4,'')
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklocsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = tickvalsy, side = 2, at=ticklocsy, las=1, line=0.2)
title(xlab="Time (ms)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Voltage (µV)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

polygon(timevals[c(1:1200,1200:1)], c(meanERP[1,]+seERP[1,],meanERP[1,1200:1]-seERP[1,1200:1]), col=pal2tone[3],border=NA)

polygon(timevals[c(1:1200,1200:1)], c(meanERP[2,]+seERP[2,],meanERP[2,1200:1]-seERP[2,1200:1]), col=rgb(0.8,0.8,0.8),border=NA)

lines(timevals,meanERP[1,],lwd=3,col=pal2tone[1])
lines(timevals,meanERP[2,],lwd=3,col='black')

legend(600,5,c('Contralateral','Ipsilateral'),col=c(pal2tone[1],'black'),lwd=3,box.lwd=2)

xoffset <- -120
yoffset <- 3
xscale <- 190
yscale <- 3
lines(xoffset+montage$headoutline[,1]*xscale,yoffset+montage$headoutline[,2]*yscale, col='black', lwd=2, cex=0.5)
lines(xoffset+montage$noseoutline[,1]*xscale,yoffset+montage$noseoutline[,2]*yscale, col='black', lwd=2, cex=0.5)
lines(xoffset+montage$Rearoutline[,1]*xscale,yoffset+montage$Rearoutline[,2]*yscale, col='black', lwd=2, cex=0.5)
lines(xoffset+montage$Learoutline[,1]*xscale,yoffset+montage$Learoutline[,2]*yscale, col='black', lwd=2, cex=0.5) 

n <- which(unlist(montage$labels)=='P8')
points(xoffset+montage$electrodelocs[n,1]*xscale,yoffset+montage$electrodelocs[n,2]*yscale, pch = 16, col='grey', cex=1.6, lwd=2)

lines(c(-200,1000),c(0,0),lty=3,lwd=2)
lines(c(0,0),c(-5,5),lty=3,lwd=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

ERPdiff <- ERPdata[,2,] - ERPdata[,1,]
meandiff <- colMeans(ERPdiff)
sediff <- apply(ERPdiff,2,sd)/sqrt(38)

plotlims <- c(-200,1000,-5,5)
ticklocsx <- seq(-200,1000,200)    # locations of tick marks on x axis
ticklocsy <- seq(-5,5,1)    # locations of tick marks on y axis
tickvalsy <- c('',-4,'',-2,'',0,'',2,'',4,'')
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklocsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = tickvalsy, side = 2, at=ticklocsy, las=1, line=0.2)
title(xlab="Time (ms)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Difference (µV)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

lines(c(-200,1000),c(0,0),lty=3,lwd=2)
lines(c(0,0),c(-5,0),lty=3,lwd=2)

polygon(timevals[c(1:1200,1200:1)], c(meandiff+sediff,meandiff[1200:1]-sediff[1200:1]), col=rgb(0.9,0.9,0.9),border=NA)

lines(timevals,meandiff,lwd=3,col=greyvect[1])

pindices <- which(allp<0.05)
points(timevals[pindices],pindices*0-2,pch='.',cex=1.5,lwd=2,col=greyvect[2])

bindices <- which(p.adjust(allp,method='bonferroni')<0.05)
points(timevals[bindices],bindices*0-3,pch='.',cex=1.5,lwd=2,col=greyvect[3])

fdrindices <- which(p.adjust(allp,method='fdr')<0.05)
points(timevals[fdrindices],fdrindices*0-4,pch='.',cex=1.5,lwd=2,col=greyvect[4])

legend(-240,5,c('Difference','p < 0.05','Bonferroni','FDR'),col=greyvect,lwd=3,box.lwd=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

plotlims <- c(-5,0,0,1)
ticklocsx <- seq(-5,0,1)    # locations of tick marks on x axis
ticklocsy <- seq(0,1)    # locations of tick marks on y axis
tickvalsx <- c('0.00001','0.0001','0.001','0.01','0.1','1')
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
mtext(text = tickvalsx, side = 1, at=ticklocsx)     # add the tick labels
title(xlab="p", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)

a <- density(log10(allp)) 
a$y <- a$y/max(a$y)                
polygon(a$x, a$y, col=pal2tone[3] ,border=NA) 

lines(log10(c(0.05,0.05)),c(0,1),col='black',lwd=3)
lines(log10(c(0.05,0.05)/1200),c(0,1),col='black',lwd=3,lty=2)

text(-1.43,0.5,'Uncorrected',cex=1,srt=90)
text(-4.5,0.5,'Bonferroni corrected',cex=1,srt=90)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

clusterstarts <- NULL
clusterends <- NULL
nclusters <- 0
incluster = 0
for (n in 1:1200){
  if (allp[n]<0.05){
    if (incluster==0){
      nclusters <- nclusters + 1
      clusterstarts[nclusters] <- n
      incluster <- 1
    }
  }
  if (allp[n]>=0.05){
    if (incluster==1){
      clusterends[nclusters] <- n-1
      incluster <- 0
    }
  }
}

plotlims <- c(-200,1000,-5,5)
ticklocsx <- seq(-200,1000,200)    # locations of tick marks on x axis
ticklocsy <- seq(-5,5,1)    # locations of tick marks on y axis
tickvalsy <- c('',-4,'',-2,'',0,'',2,'',4,'')
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklocsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = tickvalsy, side = 2, at=ticklocsy, las=1, line=0.2)
title(xlab="Time (ms)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="t-statistic", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

lines(c(-200,1000),c(0,0),lty=3,lwd=2)
lines(c(0,0),c(-5,5),lty=3,lwd=2)

critt <- qt(1-0.05/2,37)
lines(c(-200,1000),c(critt,critt),col=pal2tone[3])
lines(c(-200,1000),c(-critt,-critt),col=pal2tone[3])

crittB <- qt(1-(0.05/1200)/2,37)
lines(c(-200,1000),c(crittB,crittB),lwd=2,col=pal2tone[1],lty=2)

for (n in 1:nclusters){
  polygon(timevals[c(clusterstarts[n],clusterstarts[n]:clusterends[n],clusterends[n])], c(0,allt[clusterstarts[n]:clusterends[n]],0), col=rgb(0.9,0.9,0.9) ,border=NA) 
}
lines(timevals,allt,lwd=3,col='black')

pindices <- which(allp<0.05)
points(timevals[pindices],pindices*0-4,pch='.',cex=1.5,lwd=2,col=pal2tone[2])

summedt <- NULL
for (n in 1:nclusters){summedt[n] <- sum(allt[clusterstarts[n]:clusterends[n]])
text((clusterstarts[n]+(clusterends[n]-clusterstarts[n])/2)-200,-3.5,n)}

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

biggestcluster <- which(summedt==max(summedt))
cstart <- clusterstarts[biggestcluster]
cend <- clusterends[biggestcluster]
datatoresample <- ERPdiff[,cstart:cend]
nsubjs <- nrow(ERPdiff)
signlabels <- sign(c(-(1:(nsubjs/2)),(1:(nsubjs/2))))
nullT <- NULL
for (i in 1:nsims){
  tsum <- 0
  randsigns <- sample(signlabels,replace=TRUE) 
  for (n in 1:ncol(datatoresample)){
    tempdata <- datatoresample[,n]*randsigns
    tsum <- tsum + (mean(tempdata)/(sd(tempdata)/sqrt(nsubjs)))
  }
  nullT[i] <- tsum
}
distlims <- quantile(nullT,c(0.025,0.975))
sigclusts <- c(which(summedt<distlims[1]),which(summedt>distlims[2]))
nonsigclusts <- which(summedt>distlims[1] & summedt<distlims[2])

plotlims <- c(-1500,2500,0,1)
ticklocsx <- seq(-1500,2500,500)
ticklocsy <- seq(0,1)    # locations of tick marks on y axis
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
mtext(text = ticklocsx, side = 1, at=ticklocsx)     # add the tick labels
title(xlab="Summed t-statistic", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)

a <- density(nullT) 
a$y <- a$y/max(a$y)                
polygon(a$x, a$y, col=pal2tone[3], border=NA) 

for (n in 1:length(sigclusts)){
  lines(summedt[sigclusts[c(n,n)]],c(0,0.6),lwd=3,col=pal2tone[1])
  text(summedt[sigclusts[n]],0.65,sigclusts[n])
}
for (n in 1:length(nonsigclusts)){
  lines(summedt[nonsigclusts[c(n,n)]],c(0,0.6),lwd=3)
  text(summedt[nonsigclusts[n]],0.65,nonsigclusts[n])
}

lines(distlims[c(1,1)],c(0,1),col='black',lwd=2,lty=2)
lines(distlims[c(2,2)],c(0,1),col='black',lwd=2,lty=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

plotlims <- c(-200,1000,-5,5)
ticklocsx <- seq(-200,1000,200)    # locations of tick marks on x axis
ticklocsy <- seq(-5,5,1)    # locations of tick marks on y axis
tickvalsy <- c('',-4,'',-2,'',0,'',2,'',4,'')
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklocsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = tickvalsy, side = 2, at=ticklocsy, las=1, line=0.2)
title(xlab="Time (ms)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Difference (µV)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

lines(c(-200,1000),c(0,0),lty=3,lwd=2)
lines(c(0,0),c(-5,0),lty=3,lwd=2)

polygon(timevals[c(1:1200,1200:1)], c(meandiff+sediff,meandiff[1200:1]-sediff[1200:1]), col=rgb(0.9,0.9,0.9),border=NA)

lines(timevals,meandiff,lwd=3,col='Black')

for (n in 1:length(sigclusts)){
  indices <- clusterstarts[sigclusts[n]]:clusterends[sigclusts[n]]
  points(timevals[indices],indices*0-3,pch='.',cex=1.5,lwd=2,col=pal2tone[1])}

legend(-240,5,c('Difference','Clusters'),col=c('Black',pal2tone[1]),lwd=3,box.lwd=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)
