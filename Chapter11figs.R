
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

# Chapter 11 figures ---------------------------------------------------------------

chapter <- 11
figno <- 1

if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 8, width = 7.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 8, width = 7.5)}

npoints <- 1000
par(mfrow=c(2,2), las=1)

plot(x=NULL,y=NULL,axes=FALSE,ann=FALSE, xlim=c(-1,1), ylim=c(-1,1))
simdatax <- rnorm(npoints,mean=0,sd=0.2)
simdatay <- rnorm(npoints,mean=0,sd=0.2)
points(simdatax,simdatay,pch=16,cex=0.6,col=pal2tone[3])
lines(c(-1,1),c(0,0),lty=2)
lines(c(0,0),c(-1,1),lty=2)
text(-0.95,0.1,'x',cex=1.5)
text(0,0.95,'y',pos=4,cex=1.5)
text(-0.95,0.95,'(a)',cex=1.5)


plot(x=NULL,y=NULL,axes=FALSE,ann=FALSE, xlim=c(-1,1), ylim=c(-1,1))
simdatax <- rnorm(npoints,mean=0.5,sd=0.2)
simdatay <- rnorm(npoints,mean=0,sd=0.2)
points(simdatax,simdatay,pch=16,cex=0.6,col=pal2tone[3])
lines(c(-1,1),c(0,0),lty=2)
lines(c(0,0),c(-1,1),lty=2)
text(-0.95,0.1,'x',cex=1.5)
text(0,0.95,'y',pos=4,cex=1.5)
text(-0.95,0.95,'(b)',cex=1.5)


plot(x=NULL,y=NULL,axes=FALSE,ann=FALSE, xlim=c(-1,1), ylim=c(-1,1))
simdatax <- rnorm(npoints,mean=0,sd=0.2)
simdatay <- rnorm(npoints,mean=0.5,sd=0.2)
points(simdatax,simdatay,pch=16,cex=0.6,col=pal2tone[3])
lines(c(-1,1),c(0,0),lty=2)
lines(c(0,0),c(-1,1),lty=2)
text(-0.95,0.1,'x',cex=1.5)
text(0,0.95,'y',pos=4,cex=1.5)
text(-0.95,0.95,'(c)',cex=1.5)


plot(x=NULL,y=NULL,axes=FALSE,ann=FALSE, xlim=c(-1,1), ylim=c(-1,1))
simdatax <- rnorm(npoints,mean=0.5,sd=0.2)
simdatay <- rnorm(npoints,mean=0.5,sd=0.2)
points(simdatax,simdatay,pch=16,cex=0.6,col=pal2tone[3])
lines(c(-1,1),c(0,0),lty=2)
lines(c(0,0),c(-1,1),lty=2)
text(-0.95,0.1,'x',cex=1.5)
text(0,0.95,'y',pos=4,cex=1.5)
text(-0.95,0.95,'(d)',cex=1.5)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 8, width = 7.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 8, width = 7.5)}

npoints <- 1000
par(mfrow=c(2,2), las=1)

plot(x=NULL,y=NULL,axes=FALSE,ann=FALSE, xlim=c(-1,1), ylim=c(-1,1))
simdatax <- rnorm(npoints,mean=0,sd=0.2)
simdatay <- rnorm(npoints,mean=0,sd=0.1) + simdatax
points(simdatax,simdatay,pch=16,cex=0.6,col=pal2tone[3])
lines(c(-1,1),c(0,0),lty=2)
lines(c(0,0),c(-1,1),lty=2)
text(-0.95,0.1,'x',cex=1.5)
text(0,0.95,'y',pos=4,cex=1.5)
text(-0.95,0.95,'(a)',cex=1.5)


plot(x=NULL,y=NULL,axes=FALSE,ann=FALSE, xlim=c(-1,1), ylim=c(-1,1))
simdatax <- rnorm(npoints,mean=0,sd=0.2)
simdatay <- rnorm(npoints,mean=0,sd=0.1) + simdatax
points(simdatax+0.5,simdatay,pch=16,cex=0.6,col=pal2tone[3])
lines(c(-1,1),c(0,0),lty=2)
lines(c(0,0),c(-1,1),lty=2)
text(-0.95,0.1,'x',cex=1.5)
text(0,0.95,'y',pos=4,cex=1.5)
text(-0.95,0.95,'(b)',cex=1.5)


plot(x=NULL,y=NULL,axes=FALSE,ann=FALSE, xlim=c(-1,1), ylim=c(-1,1))
simdatax <- rnorm(npoints,mean=0,sd=0.2)
simdatay <- rnorm(npoints,mean=0,sd=0.1) + simdatax
points(simdatax,simdatay+0.5,pch=16,cex=0.6,col=pal2tone[3])
lines(c(-1,1),c(0,0),lty=2)
lines(c(0,0),c(-1,1),lty=2)
text(-0.95,0.1,'x',cex=1.5)
text(0,0.95,'y',pos=4,cex=1.5)
text(-0.95,0.95,'(c)',cex=1.5)


plot(x=NULL,y=NULL,axes=FALSE,ann=FALSE, xlim=c(-1,1), ylim=c(-1,1))
simdatax <- rnorm(npoints,mean=0,sd=0.2)
simdatay <- rnorm(npoints,mean=0,sd=0.1) + simdatax
points(simdatax+0.5,simdatay+0.5,pch=16,cex=0.6,col=pal2tone[3])
lines(c(-1,1),c(0,0),lty=2)
lines(c(0,0),c(-1,1),lty=2)
text(-0.95,0.1,'x',cex=1.5)
text(0,0.95,'y',pos=4,cex=1.5)
text(-0.95,0.95,'(d)',cex=1.5)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4, width = 4)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4, width = 4)}

npoints <- 100
par(mar=c(0.1,0.1,0.1,0.1))
# par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE,ann=FALSE, xlim=c(-1,1), ylim=c(-1,1))
simdatax <- rnorm(npoints,mean=0.5,sd=0.2)
simdatay <- rnorm(npoints,mean=0.5,sd=0.2)
for (n in 1:npoints){lines(c(mean(simdatax),simdatax[n]),c(mean(simdatay),simdatay[n]),col=rgb(0.8,0.8,0.8))}
points(simdatax,simdatay,pch=16,cex=0.6,col=pal2tone[1])
lines(c(0,mean(simdatax)),c(0,mean(simdatay)),lwd=3,col='black')
points(mean(simdatax),mean(simdatay),pch=21,cex=1.2,bg='black')
lines(c(-1,1),c(0,0),lty=2)
lines(c(0,0),c(-1,1),lty=2)
text(-0.95,0.1,'x',cex=1.5)
text(0,0.95,'y',pos=4,cex=1.5)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 9)}

load('data/SSVEPdata.RData')

par(mfrow=c(1,2), las=1)

gm <- colMeans(baseline)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE,ann=FALSE, xlim=c(-0.33,0.33), ylim=c(-0.33,0.33))
lines(c(-0.3,0.3),c(0,0),lty=2)
lines(c(0,0),c(-0.3,0.3),lty=2)
points(baseline[,1],baseline[,2],pch=16,cex=0.6,col=pal2tone[1])

A <- cov(baseline)
ctr    <- colMeans(baseline) 
RR     <- chol(A)                               # Cholesky decomposition
angles <- seq(0, 2*pi, length.out=200)          # angles for ellipse
ell    <- 1 * cbind(cos(angles), sin(angles)) %*% RR  # ellipse scaled with factor 1
ellCtr <- sweep(ell, 2, ctr, "+")               # center ellipse to the data centroid
eigVal  <- eigen(A)$values
eigVec  <- eigen(A)$vectors
eigScl  <- eigVec  %*% diag(sqrt(eigVal))  # scale eigenvectors to length = square-root
xMat    <- rbind(ctr[1] + eigScl[1, ], ctr[1] - eigScl[1, ])
yMat    <- rbind(ctr[2] + eigScl[2, ], ctr[2] - eigScl[2, ])
ellBase <- cbind(sqrt(eigVal[1])*cos(angles), sqrt(eigVal[2])*sin(angles)) 
ellRot  <- eigVec %*% t(ellBase) 
matlines(xMat, yMat, lty=1, lwd=3, col="grey")
lines((ellRot+ctr)[1, ], (ellRot+ctr)[2, ], lwd=2)

lines(c(0,gm[1]),c(0,gm[2]),lwd=3,col='black')
points(gm[1],gm[2],pch=21,cex=1.2,bg='black')

text(0.3,0.05,'0.3',cex=1.2)
text(0,0.3,'0.3',pos=4,cex=1.2)
text(-0.28,0.05,'Re',cex=1.2)
text(0,-0.28,'Im',pos=4,cex=1.2)
text(-0.3,0.3,'(a)',pos=4,cex=1.2)
title(main='Baseline')

gm <- colMeans(data)

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE,ann=FALSE, xlim=c(-1.1,1.1), ylim=c(-1.1,1.1))
lines(c(-1,1),c(0,0),lty=2)
lines(c(0,0),c(-1,1),lty=2)
points(data[,1],data[,2],pch=16,cex=0.6,col=pal2tone[1])

A <- cov(data)
ctr    <- colMeans(data) 
RR     <- chol(A)                               # Cholesky decomposition
angles <- seq(0, 2*pi, length.out=200)          # angles for ellipse
ell    <- 1 * cbind(cos(angles), sin(angles)) %*% RR  # ellipse scaled with factor 1
ellCtr <- sweep(ell, 2, ctr, "+")               # center ellipse to the data centroid
eigVal  <- eigen(A)$values
eigVec  <- eigen(A)$vectors
eigScl  <- eigVec  %*% diag(sqrt(eigVal))  # scale eigenvectors to length = square-root
xMat    <- rbind(ctr[1] + eigScl[1, ], ctr[1] - eigScl[1, ])
yMat    <- rbind(ctr[2] + eigScl[2, ], ctr[2] - eigScl[2, ])
ellBase <- cbind(sqrt(eigVal[1])*cos(angles), sqrt(eigVal[2])*sin(angles)) 
ellRot  <- eigVec %*% t(ellBase) 
matlines(xMat, yMat, lty=1, lwd=3, col="grey")
lines((ellRot+ctr)[1, ], (ellRot+ctr)[2, ], lwd=2)

lines(c(0,gm[1]),c(0,gm[2]),lwd=3,col='black')
points(gm[1],gm[2],pch=21,cex=1.2,bg='black')

text(1,0.15,'1',cex=1.2)
text(0,1,'1',pos=4,cex=1.2)
text(-0.95,0.15,'Re',cex=1.2)
text(0,-0.95,'Im',pos=4,cex=1.2)
text(-1,1,'(b)',pos=4,cex=1.2)
title(main='32% target')

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 8)}

load('data/zebrafish.RData')
par(mfrow=c(1,2), las=1)

plot(x=NULL,y=NULL,axes=FALSE,ann=FALSE, xlim=c(-29,30), ylim=c(0,1.5))
ticklocsx <- seq(-30,30,10)
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
mtext(text = ticklocsx, side = 1, at=ticklocsx)     # add the tick labels
title(xlab="Time (s)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    

ticklocsy <- seq(0,0.06,0.02)
axis(2, at=0.75+10*ticklocsy, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
mtext(text = ticklocsy, side = 2, at=0.75+10*ticklocsy, line=0.2)

ticklocsy <- seq(0,0.6,0.2)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
mtext(text = ticklocsy, side = 2, at=ticklocsy, line=0.2)
title(ylab="                    BDI", col.lab=rgb(0,0,0), line=1.7, cex.lab=1.5)    
title(ylab="Count                        ", col.lab=rgb(0,0,0), line=1.7, cex.lab=1.5)

mean6 <- colMeans(burdur6)
mean9 <- colMeans(burdur9)
CI6 <- 1.96*apply(burdur6,2,sd)/sqrt(192)
CI9 <- 1.96*apply(burdur6,2,sd)/sqrt(192)

polygon(times[c(1:60,60:1)],0.75+10*c(mean6+CI6,mean6[60:1]-CI6[60:1]),border=NA,col=rgb(0.9,0.9,0.9))
polygon(times[c(1:60,60:1)],0.75+10*c(mean9+CI9,mean9[60:1]-CI9[60:1]),border=NA,col=rgb(0.9,0.9,1))
lines(times,0.75+10*mean6,lwd=3)
lines(times,0.75+10*mean9,lwd=3,col=pal2tone[1])

mean6 <- colMeans(burct6)
mean9 <- colMeans(burct9)
CI6 <- 1.96*apply(burct6,2,sd)/sqrt(192)
CI9 <- 1.96*apply(burct9,2,sd)/sqrt(192)

polygon(times[c(1:60,60:1)],c(mean6+CI6,mean6[60:1]-CI6[60:1]),border=NA,col=rgb(0.9,0.9,0.9))
polygon(times[c(1:60,60:1)],c(mean9+CI9,mean9[60:1]-CI9[60:1]),border=NA,col=rgb(0.9,0.9,1))
lines(times,mean6,lwd=3)
lines(times,mean9,lwd=3,col=pal2tone[1])
lines(c(0,0),c(0,1.5),lty=2)

legend(5,1.5,c('6 DPF','9 DPF'), col=c('black',pal2tone[1]),lwd=3,lty=1, box.lwd=2)
text(-27.5,1.5,'(a)',cex=1.5)


plot(x=NULL,y=NULL,axes=FALSE,ann=FALSE, xlim=c(-0.02,0.12), ylim=c(-0.2,1.2))
text(-0.01,1.2,'(b)',cex=1.5)

ticklocsx <- c(0,0.1)
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
mtext(text = ticklocsx, side = 1, at=ticklocsx)     # add the tick labels
title(xlab="BDI", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    

ticklocsy <- c(0,1)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
mtext(text = ticklocsy, side = 2, at=ticklocsy, line=0.2)
title(ylab="Activity count", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    

points(mean(burdur6[,30]),mean(burct6[,30]),pch=15)
points(mean(burdur9[,30]),mean(burct9[,30]),pch=15,col=pal2tone[1])
points(mean(burdur6[,31]),mean(burct6[,31]),pch=16)
points(mean(burdur9[,31]),mean(burct9[,31]),pch=16,col=pal2tone[1])


bothdata6 <- matrix(0,nrow=192,ncol=2)
bothdata9 <- bothdata6
bothdata6[,1] <- burdur6[,30]
bothdata6[,2] <- burct6[,30]
bothdata9[,1] <- burdur9[,30]
bothdata9[,2] <- burct9[,30]
output1 <- tsqh.test(bothdata6,bothdata9,paired=FALSE)

A <- cov(bothdata6)
ctr    <- colMeans(bothdata6) 
RR     <- chol(A)                               # Cholesky decomposition
angles <- seq(0, 2*pi, length.out=200)          # angles for ellipse
ell    <- 1 * cbind(cos(angles), sin(angles)) %*% RR  # ellipse scaled with factor 1
ellCtr <- sweep(ell, 2, ctr, "+")               # center ellipse to the data centroid
eigVal  <- eigen(A)$values
eigVec  <- eigen(A)$vectors
eigScl  <- eigVec  %*% diag(sqrt(eigVal))  # scale eigenvectors to length = square-root
xMat    <- rbind(ctr[1] + eigScl[1, ], ctr[1] - eigScl[1, ])
yMat    <- rbind(ctr[2] + eigScl[2, ], ctr[2] - eigScl[2, ])
ellBase <- cbind(sqrt(eigVal[1])*cos(angles), sqrt(eigVal[2])*sin(angles)) 
ellRot  <- eigVec %*% t(ellBase) 
lines((ellRot+ctr)[1, ], (ellRot+ctr)[2, ], lwd=2, col='lightgrey')

A <- cov(bothdata9)
ctr    <- colMeans(bothdata9) 
RR     <- chol(A)                               # Cholesky decomposition
angles <- seq(0, 2*pi, length.out=200)          # angles for ellipse
ell    <- 1 * cbind(cos(angles), sin(angles)) %*% RR  # ellipse scaled with factor 1
ellCtr <- sweep(ell, 2, ctr, "+")               # center ellipse to the data centroid
eigVal  <- eigen(A)$values
eigVec  <- eigen(A)$vectors
eigScl  <- eigVec  %*% diag(sqrt(eigVal))  # scale eigenvectors to length = square-root
xMat    <- rbind(ctr[1] + eigScl[1, ], ctr[1] - eigScl[1, ])
yMat    <- rbind(ctr[2] + eigScl[2, ], ctr[2] - eigScl[2, ])
ellBase <- cbind(sqrt(eigVal[1])*cos(angles), sqrt(eigVal[2])*sin(angles)) 
ellRot  <- eigVec %*% t(ellBase) 
lines((ellRot+ctr)[1, ], (ellRot+ctr)[2, ], lwd=2, col=pal2tone[3])


bothdata6 <- matrix(0,nrow=192,ncol=2)
bothdata9 <- bothdata6
bothdata6[,1] <- burdur6[,31]
bothdata6[,2] <- burct6[,31]
bothdata9[,1] <- burdur9[,31]
bothdata9[,2] <- burct9[,31]
output2 <- tsqh.test(bothdata6,bothdata9,paired=FALSE)


A <- cov(bothdata6)
ctr    <- colMeans(bothdata6) 
RR     <- chol(A)                               # Cholesky decomposition
angles <- seq(0, 2*pi, length.out=200)          # angles for ellipse
ell    <- 1 * cbind(cos(angles), sin(angles)) %*% RR  # ellipse scaled with factor 1
ellCtr <- sweep(ell, 2, ctr, "+")               # center ellipse to the data centroid
eigVal  <- eigen(A)$values
eigVec  <- eigen(A)$vectors
eigScl  <- eigVec  %*% diag(sqrt(eigVal))  # scale eigenvectors to length = square-root
xMat    <- rbind(ctr[1] + eigScl[1, ], ctr[1] - eigScl[1, ])
yMat    <- rbind(ctr[2] + eigScl[2, ], ctr[2] - eigScl[2, ])
ellBase <- cbind(sqrt(eigVal[1])*cos(angles), sqrt(eigVal[2])*sin(angles)) 
ellRot  <- eigVec %*% t(ellBase) 
lines((ellRot+ctr)[1, ], (ellRot+ctr)[2, ], lwd=2, col='lightgrey')

A <- cov(bothdata9)
ctr    <- colMeans(bothdata9) 
RR     <- chol(A)                               # Cholesky decomposition
angles <- seq(0, 2*pi, length.out=200)          # angles for ellipse
ell    <- 1 * cbind(cos(angles), sin(angles)) %*% RR  # ellipse scaled with factor 1
ellCtr <- sweep(ell, 2, ctr, "+")               # center ellipse to the data centroid
eigVal  <- eigen(A)$values
eigVec  <- eigen(A)$vectors
eigScl  <- eigVec  %*% diag(sqrt(eigVal))  # scale eigenvectors to length = square-root
xMat    <- rbind(ctr[1] + eigScl[1, ], ctr[1] - eigScl[1, ])
yMat    <- rbind(ctr[2] + eigScl[2, ], ctr[2] - eigScl[2, ])
ellBase <- cbind(sqrt(eigVal[1])*cos(angles), sqrt(eigVal[2])*sin(angles)) 
ellRot  <- eigVec %*% t(ellBase) 
lines((ellRot+ctr)[1, ], (ellRot+ctr)[2, ], lwd=2, col=pal2tone[3])

legend(0.045,0.2,c('Dark','Light'), pch=15:16, box.lwd=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


