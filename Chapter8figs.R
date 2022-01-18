
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

# Chapter 8 figures ---------------------------------------------------------------

chapter <- 8
figno <- 1

if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 9)}

par(mfrow=c(1,2), las=1)

a <- runif(100000)
b <- runif(100000)
hist(a, breaks = 100, col = 'white')
hist(b, breaks = 100, col = pal2tone[1])

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 5)}

hist(a+b, breaks = 100, col = pal2tone[3])

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 5)}

bigsum <- runif(100000)
for (n in 1:99){bigsum <- bigsum + runif(100000)}
hist(bigsum, breaks = 100, col = 'grey')

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 9, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 9, width = 9)}

par(mfrow=c(2,2), las=1)

hist(rnorm(1000, mean=0, sd=1), breaks = 50, main='rnorm',xlab='x',ylab='Frequency',xlim=c(-4,4),col=pal2tone[1])

plot(seq(-4,4,0.001),dnorm(seq(-4,4,0.001),mean=0,sd=1),type='l',lwd=3, main='dnorm',xlab='x',ylab='Density',col=pal2tone[1])

plot(seq(-4,4,0.001),pnorm(seq(-4,4,0.001),mean=0,sd=1),type='l',lwd=3, main='pnorm',xlab='x',ylab='Cumulative probability',col=pal2tone[1])

plot(seq(0,1,0.001),qnorm(seq(0,1,0.001),mean=0,sd=1),type='l',lwd=3, main='qnorm',xlab='Quantile',ylab='x',col=pal2tone[1])

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 9, width = 6)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 9, width = 6)}

set.seed(147298)

par(mfrow=c(2,1), las=1)

a <- rnorm(50, mean = 0, sd = 3)
b <- hist(a,breaks = 20,col=rgb(0.9,0.9,0.9),xlab=NULL,ylab=NULL,xlim=c(-10,10),main=NULL)

xvals <- seq(-10,10,0.1)
underlyingdist <- dnorm(xvals,mean=0,sd=3)
underlyingdist <- max(b$counts)*underlyingdist/max(underlyingdist)
lines(xvals,underlyingdist,lwd=5,lty=1)

samp95 <- 1.96*sd(a)/sqrt(length(a))
arrows(c(mean(a), mean(a)),2*c(max(b$counts),max(b$counts))/3,x1=c(mean(a)-samp95, mean(a)+samp95), y1=2*c(max(b$counts),max(b$counts))/3, length=0.15, angle=90, lwd=4)
points(mean(a),2*max(b$counts)/3,pch=21,bg='white',cex=2)

allmeans <- NULL
for (n in 1:10000){allmeans[n] <- mean(sample(a,replace=TRUE))}

bsdist <- density(allmeans)
bsdist$y <- max(b$counts)*(bsdist$y/max(bsdist$y))/2
polygon(bsdist$x, bsdist$y, col=pal2tone[1],border=NA)

ci <- quantile(allmeans, c(0.025, 0.975))
arrows(c(mean(a), mean(a)),c(max(b$counts),max(b$counts))/3,x1=ci, y1=c(max(b$counts),max(b$counts))/3, length=0.15, angle=90, lwd=4, col=pal2tone[2])
points(mean(a),max(b$counts)/3,pch=21,bg=pal2tone[2],cex=2)

lines(c(0,0),c(0,max(b$counts)),lwd=3,lty=2)

text(9,0.95*max(b$counts),'(a)',cex=2)


a <- rgamma(50, shape = 2, scale = 5)
b <- hist(a,breaks = 20,col=rgb(0.9,0.9,0.9),xlab=NULL,ylab=NULL,xlim=c(0,40),main=NULL)

xvals <- seq(0,40,0.1)
underlyingdist <- dgamma(xvals, shape = 2, scale = 5)
underlyingdist <- max(b$counts)*underlyingdist/max(underlyingdist)
lines(xvals,underlyingdist,lwd=5,lty=1)

samp95 <- 1.96*sd(a)/sqrt(length(a))
arrows(c(median(a), median(a)),2*c(max(b$counts),max(b$counts))/3,x1=c(median(a)-samp95, median(a)+samp95), y1=2*c(max(b$counts),max(b$counts))/3, length=0.15, angle=90, lwd=4)
points(median(a),2*max(b$counts)/3,pch=21,bg='white',cex=2)

allmedians <- NULL
for (n in 1:10000){allmedians[n] <- median(sample(a,replace=TRUE))}

bsdist <- density(allmedians)
bsdist$y <- max(b$counts)*(bsdist$y/max(bsdist$y))/2
polygon(bsdist$x, bsdist$y, col=pal2tone[1],border=NA)

ci <- quantile(allmedians, c(0.025, 0.975))
arrows(c(median(a), median(a)),c(max(b$counts),max(b$counts))/3,x1=ci, y1=c(max(b$counts),max(b$counts))/3, length=0.15, angle=90, lwd=4, col=pal2tone[2])
points(median(a),max(b$counts)/3,pch=21,bg=pal2tone[2],cex=2)

text(38,0.95*max(b$counts),'(b)',cex=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 9)}

par(mfrow=c(1,2), las=1)

set.seed(142978)

A <- round(100*runif(10,min=0.5))
B <- round(100*runif(10,min=0))
alldata <- c(A,B)
temp <- t.test(A,B)
rawt <- temp$statistic
rawp <- temp$p.value

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(0,10), ylim=c(0,11)) 
title(ylab="Original data", line=0, cex.lab=1.5)
text(0.5,10.5,'A',cex=1.2)
text(1.5,10.5,'B',cex=1.2)

polygon(c(0,1,1,0),c(0,0,10,10),col=pal2tone[3],border=NA)
lines(c(0,0),c(0,10),lwd=2)
lines(c(1,1),c(0,10),lwd=2)
lines(c(2,2),c(0,10),lwd=2)

for (n in 0:10){
  lines(c(0,2),c(n,n),lwd=2)
}
for (n in 1:10){
  text(0.5,n-0.5,A[n],cex=0.8)
  text(1.5,n-0.5,B[n],cex=0.8)
}
text(2.8,5,paste('t =',round(rawt,digits=2)),srt=270,cex=1.5)

text(7.5,10.5,"A'",cex=1.2)
text(8.5,10.5,"B'",cex=1.2)

ivals <- sample(1:length(alldata),length(alldata))

for (n in 1:10){
  if (ivals[n]<11){polygon(c(7,8,8,7),c(n-1,n-1,n,n),col=pal2tone[3],border=NA)}
  if (ivals[n+10]<11){polygon(c(8,9,9,8),c(n-1,n-1,n,n),col=pal2tone[3],border=NA)}
  
  text(7.5,n-0.5,alldata[ivals[n]],cex=0.8)
  text(8.5,n-0.5,alldata[ivals[n+10]],cex=0.8)
}

lines(c(7,7),c(0,10),lwd=2)
lines(c(8,8),c(0,10),lwd=2)
lines(c(9,9),c(0,10),lwd=2)
for (n in 0:10){lines(c(7,9),c(n,n),lwd=2)}

temp <- t.test(alldata[ivals[1:10]],alldata[ivals[11:20]])
bst <- temp$statistic
text(9.8,5,paste('t =',round(bst,digits=2)),srt=270,cex=1.5)
text(6.2,5.5,'Resampled data',srt=90,cex=1.5)


tpop <- NULL
for (n in 1:nsims){
  rdata <- sample(alldata,length(alldata))
  temp <- t.test(rdata[1:10],rdata[11:20])
  tpop[n] <- temp$statistic
}

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(-6,6), ylim=c(0,1))   
axis(1, at=seq(-6,6,2), tck=0.01, lab=F, lwd=2)   
mtext(text = seq(-6,6,2), side = 1, at=seq(-6,6,2), las=1) 
title(xlab="t-statistic", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)

tdist <- density(tpop)
tdist$y <- tdist$y/max(tdist$y)
polygon(tdist$x, tdist$y, col=rgb(0.7,0.7,0.7), border=NA)

lines(c(rawt,rawt),c(0,1),col=pal2tone[2],lwd=4)

bp1 <- length(which(tpop>rawt))/nsims
bp2 <- length(which(abs(tpop)>abs(rawt)))/nsims

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

data <- rnorm(100, mean=1, sd=3)   

# create an empty variable to store the resampled means
allmeans <- NULL    
# repeat the resampling lots of times
for (n in 1:10000){
  # use the sample function to resample the data, and store in rsdata
  rsdata <- sample(data,replace=TRUE)    
  # calculate the mean of the resampled data
  allmeans[n] <- mean(rsdata)           
}
CIs <- quantile(allmeans,c(0.025,0.975))

# plot a histogram of the resampled means
b <- hist(allmeans,breaks=20)   
# add a vertical line showing the true mean
lines(c(mean(data),mean(data)),c(0,max(b$counts)),col='black',lwd=8,lty=2)  
lines(CIs[c(1,1)],c(0,max(b$counts)/2),lty=3,lwd=4)  
lines(CIs[c(2,2)],c(0,max(b$counts)/2),lty=3,lwd=4)  

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

maint <- t.test(data,mu=0) # calculate a t-statistic instead of a mean

allT <- NULL    
for (n in 1:10000){
  allT[n] <- t.test(sample(data,replace=TRUE),mu=0)$statistic}
CIs <- quantile(allT,c(0.025,0.975))

b <- hist(allT,breaks=20)   
lines(c(maint$statistic,maint$statistic),c(0,max(b$counts)),lty=2,lwd=8)  
lines(CIs[c(1,1)],c(0,max(b$counts)/2),lty=3,lwd=4)  
lines(CIs[c(2,2)],c(0,max(b$counts)/2),lty=3,lwd=4)  

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

set.seed(4723985)
# generate a vector of 50 random values
var1 <- rnorm(50)
# generate a vector of 50 values that includes a fraction of var1
var2 <- rnorm(50) + 0.25*var1

# calculate the correlation coefficient for these two vectors
truecor <- cor(var1,var2)

nullR <- NULL
for (n in 1:10000){
  var1r <- sample(var1,50,replace=TRUE)
  var2r <- sample(var2,50,replace=TRUE)
  nullR[n] <- cor(var1r, var2r)
}

hist(nullR,breaks=20)
lines(c(truecor,truecor),c(0,1200),col=pal2tone[1],lwd=6)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)
