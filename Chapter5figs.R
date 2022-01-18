
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

# Chapter 5 figures ---------------------------------------------------------------
chapter <- 5
figno <- 1

if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 7, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 7, width = 9)}

par(mfrow=c(2,2), las=1)  # divide the plot into four panels

xvals <- seq(-4,4,0.01)
temp <- dnorm(xvals,mean=-0.5,sd=1)

plot(x=NULL,y=NULL,xlim=c(-4,4),ylim=c(0,1),axes=FALSE, ann=FALSE)
axis(1, at=seq(-4,4,2), tck=0.01, lab=F, lwd=2)
mtext(text=seq(-4,4,2), side = 1, at=seq(-4,4,2), line=0.2, las=1)
overlap <- pmin(dnorm(xvals,mean=-0.5,sd=1)/max(temp),dnorm(xvals,mean=0.5,sd=1)/max(temp))
polygon(xvals,overlap,col=pal2tone[3],border=NA)
lines(xvals,dnorm(xvals,mean=-0.5,sd=1)/max(temp),lwd=3)
lines(xvals,dnorm(xvals,mean=0.5,sd=1)/max(temp),lwd=3,col=pal2tone[1])
title(main='d = 1',cex.main=1.5)

plot(x=NULL,y=NULL,xlim=c(-4,4),ylim=c(0,1),axes=FALSE, ann=FALSE)
axis(1, at=seq(-4,4,2), tck=0.01, lab=F, lwd=2)
mtext(text=seq(-4,4,2), side = 1, at=seq(-4,4,2), line=0.2, las=1)
overlap <- pmin(dnorm(xvals,mean=-1,sd=1)/max(temp),dnorm(xvals,mean=1,sd=1)/max(temp))
polygon(xvals,overlap,col=pal2tone[3],border=NA)
lines(xvals,dnorm(xvals,mean=-1,sd=1)/max(temp),lwd=3)
lines(xvals,dnorm(xvals,mean=1,sd=1)/max(temp),lwd=3,col=pal2tone[1])
title(main='d = 2',cex.main=1.5)

temp <- dnorm(xvals,mean=-0.5,sd=0.5)
plot(x=NULL,y=NULL,xlim=c(-4,4),ylim=c(0,1),axes=FALSE, ann=FALSE)
axis(1, at=seq(-4,4,2), tck=0.01, lab=F, lwd=2)
mtext(text=seq(-4,4,2), side = 1, at=seq(-4,4,2), line=0.2, las=1)
overlap <- pmin(dnorm(xvals,mean=-0.5,sd=0.5)/max(temp),dnorm(xvals,mean=0.5,sd=0.5)/max(temp))
polygon(xvals,overlap,col=pal2tone[3],border=NA)
lines(xvals,dnorm(xvals,mean=-0.5,sd=0.5)/max(temp),lwd=3)
lines(xvals,dnorm(xvals,mean=0.5,sd=0.5)/max(temp),lwd=3,col=pal2tone[1])
title(main='d = 2',cex.main=1.5)

plot(x=NULL,y=NULL,xlim=c(-4,4),ylim=c(0,1),axes=FALSE, ann=FALSE)
axis(1, at=seq(-4,4,2), tck=0.01, lab=F, lwd=2)
mtext(text=seq(-4,4,2), side = 1, at=seq(-4,4,2), line=0.2, las=1)
overlap <- pmin(dnorm(xvals,mean=-1,sd=0.5)/max(temp),dnorm(xvals,mean=1,sd=0.5)/max(temp))
polygon(xvals,overlap,col=pal2tone[3],border=NA)
lines(xvals,dnorm(xvals,mean=-1,sd=0.5)/max(temp),lwd=3)
lines(xvals,dnorm(xvals,mean=1,sd=0.5)/max(temp),lwd=3,col=pal2tone[1])
title(main='d = 4',cex.main=1.5)
if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){cairo_ps(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), onefile = FALSE, height = 4.5, width = 7, fallback_resolution = 600)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 7)}

par(mfrow=c(1,2), las=1)

truemean <- 1
trueSD <- 2
nreps <- 1000
N <- 10
sampled <- NULL
allp <- NULL
issig <- (1:nreps)*0
for (i in 1:nreps){
  sampledata <- rnorm(N,mean=truemean,sd=trueSD)
  sampled[i] <- mean(sampledata)/sd(sampledata)
  output <- t.test(sampledata)
  allp[i] <- output$p.value
  if (output$p.value<0.05){issig[i] <- 1}
}

plot(x=NULL,y=NULL,xlim=c(-1,1),ylim=c(-1,3),axes=FALSE, ann=FALSE)
axis(2, at=-1:3, tck=0.01, lab=F, lwd=2)
mtext(text = -1:3, side = 2, at=-1:3, line=0.2, las=1)
title(ylab="Effect size (d)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

for (i in 1:nreps){
  if (issig[i]){points(rnorm(1)*0.2,sampled[i],pch=16,cex=0.5,col=addalpha(pal2tone[1],alpha=0.2))}
  if (!issig[i]){points(rnorm(1)*0.2,sampled[i],pch=16,cex=0.5,col=rgb(0,0,0,alpha=0.2))}
}
points(-0.1,truemean/trueSD,pch=23,cex=2,lwd=2,bg='white')
points(0.1,mean(sampled[which(issig>0)]),pch=23,cex=2,lwd=2,bg=pal2tone[1])
lines(c(-1,1),c(max(sampled[which(issig==0)]),max(sampled[which(issig==0)])),lwd=2)
text(0.7,-0.7,'N=10',cex=1.5)

N <- 50
sampled <- NULL
allp <- NULL
issig <- (1:nreps)*0
for (i in 1:nreps){
  sampledata <- rnorm(N,mean=truemean,sd=trueSD)
  sampled[i] <- mean(sampledata)/sd(sampledata)
  output <- t.test(sampledata)
  allp[i] <- output$p.value
  if (output$p.value<0.05){issig[i] <- 1}
}

plot(x=NULL,y=NULL,xlim=c(-1,1),ylim=c(-1,3),axes=FALSE, ann=FALSE)
axis(2, at=-1:3, tck=0.01, lab=F, lwd=2)
mtext(text = -1:3, side = 2, at=-1:3, line=0.2, las=1)
title(ylab="Effect size (d)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

for (i in 1:nreps){
  if (issig[i]){points(rnorm(1)*0.2,sampled[i],pch=16,cex=0.5,col=addalpha(pal2tone[1],alpha=0.2))}
  if (!issig[i]){points(rnorm(1)*0.2,sampled[i],pch=16,cex=0.5,col=rgb(0,0,0,alpha=0.2))}
}
points(-0.1,truemean/trueSD,pch=23,cex=2,lwd=2,bg='white')
points(0.1,mean(sampled[which(issig>0)]),pch=23,cex=2,lwd=2,bg=pal2tone[1])
lines(c(-1,1),c(max(sampled[which(issig==0)]),max(sampled[which(issig==0)])),lwd=2)
text(0.7,-0.7,'N=50',cex=1.5)

legend(-1,3,c('True effect size','Estimated effect size'),pch=23,pt.bg=c('white','cornflowerblue'),box.lwd=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 9)}

par(mfrow=c(1,2), las=1)

nlist <- 3:200
powersmall <- pwr.t.test(n=nlist,d=0.2,sig.level=0.05)
powermid <- pwr.t.test(n=nlist,d=0.5,sig.level=0.05)
powerlarge <- pwr.t.test(n=nlist,d=0.8,sig.level=0.05)
plot(x=NULL,y=NULL,xlim=c(0,max(nlist)),ylim=c(0,1),axes=FALSE, ann=FALSE)
axis(1, at=seq(0,200,50), tck=0.01, lab=F, lwd=2)
axis(2, at=seq(0,1,0.2), tck=0.01, lab=F, lwd=2)
mtext(text = seq(0,200,50), side = 1, at=seq(0,200,50), line=0.2, las=1)
mtext(text = seq(0,1,0.2), side = 2, at=seq(0,1,0.2), line=0.2, las=1)
lines(c(0,max(nlist)),c(0.8,0.8),lty=2)
lines(nlist,powersmall$power,col='black',lwd=3)
lines(nlist,powermid$power,col=pal2tone[1],lwd=3)
lines(nlist,powerlarge$power,col=pal2tone[3],lwd=3)
title(xlab="Sample size (N)", col.lab=rgb(0,0,0), line=2.2, cex.lab=1.5)    # titles for axes
title(ylab="Power", col.lab=rgb(0,0,0), line=2.2, cex.lab=1.5)
legend(125,0.3,c('d = 0.2','d = 0.5','d = 0.8'),lwd=3,col=c('black',pal2tone[1],pal2tone[3]),cex=0.8,box.lwd=2)


effectsizes <- seq(0,1,0.001)
powersmall <- pwr.t.test(n=20,d=effectsizes,sig.level=0.05)
powermid <- pwr.t.test(n=40,d=effectsizes,sig.level=0.05)
powerlarge <- pwr.t.test(n=80,d=effectsizes,sig.level=0.05)
plot(x=NULL,y=NULL,xlim=c(0,max(effectsizes)),ylim=c(0,1),axes=FALSE, ann=FALSE)
axis(1, at=seq(0,1,0.2), tck=0.01, lab=F, lwd=2)
axis(2, at=seq(0,1,0.2), tck=0.01, lab=F, lwd=2)
mtext(text = seq(0,1,0.2), side = 1, at=seq(0,1,0.2), line=0.2, las=1)
mtext(text = seq(0,1,0.2), side = 2, at=seq(0,1,0.2), line=0.2, las=1)
lines(c(0,max(effectsizes)),c(0.8,0.8),lty=2)
lines(c(0,max(effectsizes)),c(0.05,0.05),lty=3)
lines(effectsizes,powersmall$power,col='black',lwd=3)
lines(effectsizes,powermid$power,col=pal2tone[1],lwd=3)
lines(effectsizes,powerlarge$power,col=pal2tone[3],lwd=3)
title(xlab="Effect size (d)", col.lab=rgb(0,0,0), line=2.2, cex.lab=1.5)    # titles for axes
title(ylab="Power", col.lab=rgb(0,0,0), line=2.2, cex.lab=1.5)
legend(0.65,0.33,c('N = 20','N = 40','N = 80'),lwd=3,col=c('black',pal2tone[1],pal2tone[3]),cex=0.8,box.lwd=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 8, width = 8)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 8, width = 8)}

maxsubjs <- 200
maxtrials <- 100
intrapSD <- 10
interpSD <- 2
groupmean <- 1
alphalevel <- 0.05
nsubjs <- unique(round(seq(2,maxsubjs,length=40)))
nsamplevect <- unique(round(seq(2,maxtrials,length=40)))

powerlist <- matrix(0,nrow=length(nsubjs),ncol=length(nsamplevect))
for (smp in 1:length(nsamplevect)){
  for (s in 1:length(nsubjs)){
    sampleSD <- sqrt(interpSD^2 + (intrapSD^2)/nsamplevect[smp])
    destimate <- groupmean/sampleSD
    p <- pwr::pwr.t.test(d=destimate,n=nsubjs[s],sig.level=alphalevel,type="one.sample",alternative='two.sided')
    powerlist[s,smp] <- p$power
  }}
powerlist <- round(powerlist*1000)/1000
par(pty="s")  # make axis square

plotlims <- c(0,maxsubjs,0,maxtrials)  # define the x and y limits of the plot (minx,maxx,miny,maxy)
ticklocsx <- seq(0,maxsubjs,round(maxsubjs/10))    # locations of tick marks on x axis
ticklocsy <- seq(0,maxtrials,round(maxtrials/10))    # locations of tick marks on y axis
ticklabelsx <- ticklocsx        # set labels for x ticks
ticklabelsy <- ticklocsy    # set labels for y ticks

ramp <- colorRamp(c("black","darkblue","cornflowerblue"))  # create a ramp from one colour to another
colmatrix <- rgb(ramp(seq(0, 1, length = 101)), max = 255)
for (n in 1:length(colmatrix)){colmatrix[n] <- flatalpha(colmatrix[n],alpha=0.5)}

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)  
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)  
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)  
title(xlab="Sample size (N)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="Trials per participant (k)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)
title(main=paste('Mean difference = ', groupmean, '; Within SD = ', intrapSD, '; Between SD = ', interpSD, sep=''), line=0)
.filled.contour(nsubjs,nsamplevect,powerlist,levels=seq(0,1,0.01),col=colmatrix)

cl <- contourLines(nsubjs,nsamplevect,powerlist,levels=seq(0.1,1,0.1))
colvect <- rgb(ramp(seq(0, 1, length = 10)), max = 255)
for (n in 1:length(cl)){
  temp <- cl[n]
  lines(temp[[1]]$x,temp[[1]]$y,col=colvect[temp[[1]]$level*10],lwd=3)
  if (temp[[1]]$level==0.8){lines(temp[[1]]$x,temp[[1]]$y,col=colvect[temp[[1]]$level*10],lwd=9)}
}

legend(0.8*maxsubjs, 0.99*maxtrials, c("10%","20%","30%","40%","50%","60%","70%","80%","90%","100%"), title="Power", bg='white',cex=1, col=colvect, lty=1, lwd=c(3,3,3,3,3,3,3,9,3,3), box.lwd=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

N <- 4:100
R <- NULL
for (n in 1:length(N)){
  output <- pwr.r.test(n=N[n], power=0.8, sig.level=0.05) 
  R[n] <- output$r
}
plot(N,R,type='l',lwd=3,xlim=c(0,100),ylim=c(0,1))

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)
