
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
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 5)}

kinase <- read.csv('data/kinases.csv')
neutralindices <- which(kinase$Phenotype=='Neutral')
resistorindices <- which(kinase$Phenotype=='Resistor')
sensitizerindices <- which(kinase$Phenotype=='Sensitizer')

plotlims <- c(-25,25,-25,25)
ticklocsx <- seq(-25,25,25)    # locations of tick marks on x axis
ticklocsy <- seq(-25,25,25)   # locations of tick marks on y axis
ticklabelsx <- ticklocsx        # set labels for x ticks
ticklabelsy <- ticklocsy    # set labels for y ticks

par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4]) 
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Normalized % apoptosis - 2 hours", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.2)
title(ylab="Normalized % apoptosis - 4 hours", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.2)

points(kinase$Delta2h[neutralindices],kinase$Delta4h[neutralindices],pch=24,bg=pal2tone[1])
points(kinase$Delta2h[resistorindices],kinase$Delta4h[resistorindices],pch=21,bg='black')
points(kinase$Delta2h[sensitizerindices],kinase$Delta4h[sensitizerindices],pch=22,bg='white')

legend(-25,25, c("Neutral","Resistor","Sensitizer"), cex=1, pt.bg=c(pal2tone[1],"black","white"),  pch=c(24,21,22), box.lwd=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


