# set outputplot as 2 to save plots as PDfs
outputplot <- 2
# set n of simulations for figure 5
nsims <- 100  # global value for the whole script - increase to 100000 for final run
# set colours for all plots
pal2tone <- c('#8783CF','#10069F','#CFCDEC')  # blue 072

#  create some functions that are used later to identify outliers
d_chauv <- function(data){
  i <- NULL
  m <- mean(data)
  s <- sd(data)
  Zdata <- abs(data-m)/s
  dmax <- abs(qnorm(1/(4*length(data))))
  i <- which(Zdata>dmax)
  return(i)}

d_thom <- function(data){
  i <- NULL
  noutliers <- 0
  included <- 1:length(data)
  exitcode <- 0
  
  while (exitcode==0){
    m <- mean(data[included])
    s <- sd(data[included])
    Zdata <- abs(data[included]-m)/s
    target <- max(Zdata)
    ti <- included[which(Zdata==target)]
    n <- length(included)
    tcrit <- qt(0.975,n-2)
    tau <- tcrit*(n-1)/(sqrt(n)*sqrt(n-2+tcrit^2))
    exitcode <- 1
    if (target>tau){
      noutliers <- noutliers + 1
      i[noutliers] <- ti
      included <- included[which(included!=ti)]
      exitcode <- 0
    }
  }
  i <- sort(i)
  return(i)}


# Chapter 3 figures ---------------------------------------------------------------
chapter <- 3
figno <- 1

if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 7.5, width = 7.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 7.5, width = 7.5)}

par(mfrow=c(2,2), las=1)
set.seed(1982)

data <- rnorm(500)
hist(data,breaks=seq(-3,3,length=11),col='grey',main=NULL)
text(-2.5,110,'(a)')

hist(data,breaks=seq(-3,3,length=21),col=pal2tone[1],main=NULL)
text(-2.5,65,'(b)')

a <- density(data)
plot(a$x,a$y,type='l',lwd=2)
polygon(a$x,a$y,col=pal2tone[1],border=NA)
lines(a$x,a$y,lwd=2)
text(-3.5,0.35,'(c)')

data[499:500] <- c(6.1,4.5)
hist(data,breaks=seq(-3,7,length=41),main=NULL)
arrows(4.375,30,4.375,10,length=0.05,angle=45,lwd=2)
arrows(6.1,30,6.1,10,length=0.05,angle=45,lwd=2)
text(-2.5,55,'(d)')

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
# if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==1){cairo_ps(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), onefile = FALSE, height = 5, width = 9, fallback_resolution = 600)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 9)}

par(mfrow=c(1,2), las=1)
set.seed(1982)

data <- rnorm(500)
data2 <- data + 0.5*rnorm(500)

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(0,5), ylim=c(0,10))   
axis(1, at=c(0,5), tck=0.01, lab=F, lwd=2)   
axis(2, at=0:10, tck=0.01, lab=F, lwd=2)
mtext(text = c('A','B'), side = 1, at=c(1.5,3.5), las=1) 
mtext(text = 0:10, side = 2, at=0:10, line=0.2, las=1)  
title(xlab="Condition", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Measure", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

points(runif(50)+1,data[1:50]+3,pch=16,col='gray')
points(runif(50)+3,data[51:100]+6,pch=15,col=pal2tone[1])
lines(c(0.8,2.2),c(mean(data[1:50]+3),mean(data[1:50]+3)),lwd=4)
lines(c(2.8,4.2),c(mean(data[51:100]+6),mean(data[51:100]+6)),lwd=4)

text(0.2,9.5,'(a)')



plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(-3,3), ylim=c(-3,3))   
axis(1, at=-3:3, tck=0.01, lab=F, lwd=2)   
axis(2, at=-3:3, tck=0.01, lab=F, lwd=2)
mtext(text = -3:3, side = 1, at= -3:3, las=1) 
mtext(text = -3:3, side = 2, at= -3:3, line=0.2, las=1)  
title(xlab="Measure A", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Measure B", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

a <- density(c(data,1.5,1.6,1.4))
polygon(a$x,3-(3*a$y),col=rgb(0.9,0.9,0.9),border=NA)

a <- density(c(data2,-1.4,-1.2,-2))
polygon(-3+(3*a$y),a$x,col=rgb(0.9,0.9,0.9),border=NA)

points(data,data2,pch=16,col=rgb(0,0,0,alpha=0.3),cex=0.5)
points(c(1.5,1.6,1.4),c(-1.4,-1.2,-2),pch=16,col=pal2tone[1],cex=1)

text(-2.5,2.7,'(b)')

data <- c(data,1.5,1.6,1.4)
data2 <- c(data2,-1.4,-1.2,-2)

bdata <- data.frame(data,data2)
colnames(bdata) <- c('x','y')

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
# if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==1){cairo_ps(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), onefile = FALSE, height = 4.5, width = 6.5, fallback_resolution = 600)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

# this is daft, but I can't find a better way to do it
sigmastrings <- NULL
sigmastrings[1] <- expression(paste(-5,sigma,sep=''))
sigmastrings[2] <- expression(paste(-4,sigma,sep=''))
sigmastrings[3] <- expression(paste(-3,sigma,sep=''))
sigmastrings[4] <- expression(paste(-2,sigma,sep=''))
sigmastrings[5] <- expression(paste(-1,sigma,sep=''))
sigmastrings[6] <- expression(paste(0,sigma,sep=''))
sigmastrings[7] <- expression(paste(1,sigma,sep=''))
sigmastrings[8] <- expression(paste(2,sigma,sep=''))
sigmastrings[9] <- expression(paste(3,sigma,sep=''))
sigmastrings[10] <- expression(paste(4,sigma,sep=''))
sigmastrings[11] <- expression(paste(5,sigma,sep=''))


plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(-5,5), ylim=c(0,2.1))   
axis(1, at=-5:5, tck=0.01, lab=F, lwd=2)   
mtext(text = sigmastrings, side = 1, at=-5:5, las=1) 

xvals <- seq(-0.67,0.67,0.01)
normdist <- dnorm(xvals)
polygon(c(-0.67,xvals,0.67),c(0,normdist/max(normdist),0),col=pal2tone[3],border=NA)

lines(c(-0.67,-0.67),c(0,1.8),lty=3)
lines(c(0.67,0.67),c(0,1.8),lty=3)
lines(c(-2.698,-2.698),c(0,1.8),lty=2)
lines(c(2.698,2.698),c(0,1.8),lty=2)
lines(c(-4.7215,-4.7215),c(0,1.8),lty=1)
lines(c(4.7215,4.7215),c(0,1.8),lty=1)

xvals <- seq(-5,5,0.01)
normdist <- dnorm(xvals)
lines(xvals,normdist/max(normdist),lwd=3)

ndata <- rnorm(500)
ndata[1:3] <- c(-3.5,3.1,3.9)
yvals <- 1.2+runif(length(ndata))/3
points(ndata,yvals,pch=16,col=rgb(0,0,0,alpha=0.3),cex=0.5)
i <- which(abs(ndata)>2.698)
points(ndata[i],yvals[i],pch=16,col=pal2tone[1],cex=1)

polygon(c(-0.6745,0.6745,0.6745,-0.6745),c(1.7,1.7,1.9,1.9),col=pal2tone[3],lwd=3)
arrows(-0.6745,1.8,-2.698,1.8,length=0.05, angle=90, lwd=2)
arrows(0.6745,1.8,2.698,1.8,length=0.05, angle=90, lwd=2)

text(-0.67,1.99,'Q1',adj=0.5)
text(0,2.05,'IQR',adj=0.5)
text(0.67,1.99,'Q3',adj=0.5)

text(0,0.4,'50%',adj=0.5)
arrows(-0.4,0.4,-0.67,0.4,length=0.05)
arrows(0.4,0.4,0.67,0.4,length=0.05)


text(-3,0.6,'Inner fence',adj=0.5,srt=90)
text(3,0.6,'Inner fence',adj=0.5,srt=270)
text(-5,0.6,'Outer fence',adj=0.5,srt=90)
text(5,0.6,'Outer fence',adj=0.5,srt=270)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

boxplot(ndata)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

N <- 100
outliervals <- seq(0,4,0.1)

SD1count <- 0*outliervals
SD2count <- 0*outliervals
SD3count <- 0*outliervals
Tukeycount <- 0*outliervals
chcount <- 0*outliervals
taucount <- 0*outliervals

for (s in 1:nsims){
  for (k in 1:length(outliervals)){
    data <- rnorm(N)  # generate some random data
    data[1] <- outliervals[k]     # one value with an outlier
    
    if ((data[1]-mean(data))>(1*sd(data))){SD1count[k] <- SD1count[k]+1}
    if ((data[1]-mean(data))>(2*sd(data))){SD2count[k] <- SD2count[k]+1}
    if ((data[1]-mean(data))>(3*sd(data))){SD3count[k] <- SD3count[k]+1}
    if ((data[1]-mean(data))>(2*IQR(data))){Tukeycount[k] <- Tukeycount[k]+1}
    
    ch <- d_chauv(data)
    if (length(ch)){if (ch[1]==1){chcount[k] <- chcount[k]+1}}
    th <- d_thom(data)
    if (length(th)){if (th[1]==1){taucount[k] <- taucount[k]+1}}
    
  }}

SD1prop <- SD1count/nsims
SD2prop <- SD2count/nsims
SD3prop <- SD3count/nsims
Tukeyprop <- Tukeycount/nsims
chprop <- chcount/nsims
tauprop <- taucount/nsims

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(0,4), ylim=c(0,1))   
axis(1, at=0:4, tck=0.01, lab=F, lwd=2)   
axis(2, at=seq(0,1,0.2), tck=0.01, lab=F, lwd=2)
mtext(text = 0:4, side = 1, at= 0:4, las=1) 
mtext(text = seq(0,1,0.2), side = 2, at= seq(0,1,0.2), line=0.2, las=1)  
title(xlab="Outlier position (SD from true mean)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Proportion detected", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

lines(outliervals,SD1prop,lwd=2)
lines(outliervals,SD2prop,lwd=2,col='grey')
lines(outliervals,SD3prop,lwd=2,col='lightgrey')

lines(outliervals,tauprop,lwd=3,col=pal2tone[1],lty=2)
lines(outliervals,chprop,lwd=3,col=pal2tone[2],lty=3)
lines(outliervals,Tukeyprop,lwd=3,col=pal2tone[3],lty=4)

legend(0,1,c('Tukey','Thompson', 'Chauvenet'), col=pal2tone[c(3,1,2)], lwd=3, lty=c(4,2,3),box.lwd=2)

legend(3.1,0.35,c('1 SD', '2 SD', '3 SD'), col=c('black','grey','lightgrey'), lwd=2, box.lwd=2)


if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
# if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==1){cairo_ps(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), onefile = FALSE, height = 5, width = 9, fallback_resolution = 600)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 9)}

par(mfrow=c(1,2), las=1)

N <- 100

x <- rnorm(N)
y <- rnorm(N)*2

x[1:2] <- c(7,0)
y[1:2] <- c(0,7)

# par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(-8,8), ylim=c(-8,8))   
axis(1, at=seq(-8,8,4), tck=0.01, lab=F, lwd=2)   
axis(2, at=seq(-8,8,4), tck=0.01, lab=F, lwd=2)
mtext(text = seq(-8,8,4), side = 1, at= seq(-8,8,4), las=1) 
mtext(text = seq(-8,8,4), side = 2, at= seq(-8,8,4), line=0.2, las=1)  
title(xlab="x", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="y", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

xvals <- seq(-8,8,0.1)
dist <- dnorm(xvals)
dist <- 2*dist/max(dist)
polygon(xvals,dist-8,col=pal2tone[3],border=NA)
dist <- dnorm(xvals,sd=2)
dist <- 2*dist/max(dist)
polygon(dist-8,xvals,col=pal2tone[3],border=NA)

points(x,y,pch=16,cex=0.5,col=rgb(0,0,0,alpha=0.3))

lines(c(mean(x),x[1]),c(mean(y),y[1]),col=pal2tone[1],lwd=3)
lines(c(mean(x),x[2]),c(mean(y),y[2]),col=pal2tone[1],lwd=3)

points(x[1],y[1],pch=22,cex=1,bg='white',lwd=3)
points(x[2],y[2],pch=24,cex=1,bg='white',lwd=3)
points(mean(x),mean(y),pch=21,cex=1,bg='black',lwd=3)

text(-7,7,'(a)',cex=1.5)

amplitudes <- sqrt(x^2 + y^2)
angles <- atan2(y,x)

angles <- angles - pi/4

x2 <- cos(angles)*amplitudes
y2 <- sin(angles)*amplitudes

# par(pty="s")  # make axis square
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(-8,8), ylim=c(-8,8))   
axis(1, at=seq(-8,8,4), tck=0.01, lab=F, lwd=2)   
axis(2, at=seq(-8,8,4), tck=0.01, lab=F, lwd=2)
mtext(text = seq(-8,8,4), side = 1, at= seq(-8,8,4), las=1) 
mtext(text = seq(-8,8,4), side = 2, at= seq(-8,8,4), line=0.2, las=1)  
title(xlab="x", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="y", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

points(x2,y2,pch=16,cex=0.5,col=rgb(0,0,0,alpha=0.3))

lines(c(mean(x2),x2[1]),c(mean(y2),y2[1]),col=pal2tone[1],lwd=3)
lines(c(mean(x2),x2[2]),c(mean(y2),y2[2]),col=pal2tone[1],lwd=3)

points(x2[1],y2[1],pch=22,cex=1,bg='white',lwd=3)
points(x2[2],y2[2],pch=24,cex=1,bg='white',lwd=3)
points(mean(x2),mean(y2),pch=21,cex=1,bg='black',lwd=3)

text(-7,7,'(b)',cex=1.5)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

D2 <- mahalanobis(bdata, colMeans(bdata), cov(bdata))

xlabel <- expression(Mahalanobis~D^2)
hist(D2,breaks=seq(0,40,length=41),col='grey',main=NULL, xlab=xlabel)

for (n in 501:503){
  arrows(floor(D2[n])+0.5,50,floor(D2[n])+0.5,5,length=0.05,angle=45,lwd=2)
}

lines(c(9,9),c(0,200),lty=3,lwd=2)

text(32, 60, 'Outliers')

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

data <- rnorm(50, mean=10, sd=2)
xvals <- runif(50)

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(0,7), ylim=c(-10,20))  
axis(2, at=seq(-10,20,10), tck=0.01, lab=F, lwd=2)
mtext(text = seq(-10,20,10), side = 2, at=seq(-10,20,10), line=0.2, las=1)

points(xvals,data,pch=16,col='black')
points(xvals+2,scale(data,scale=FALSE),pch=16,col=pal2tone[1])
points(xvals+4,scale(data,center=FALSE),pch=16,col='grey')
points(xvals+6,scale(data),pch=16,col=pal2tone[3])

lines(c(0,7),c(0,0),lty=2)

text(0.5,-8,'Raw',adj=0.5)
text(2.5,-8,'Centred',adj=0.5)
text(4.5,-8,'Scaled',adj=0.5)
text(6.5,-7,'Centred &',adj=0.5)
text(6.5,-9,'Scaled',adj=0.5)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 10, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 10, width = 6.5)}

par(mfrow=c(2,1), las=1)

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(-3,4), ylim=c(0,1))
axis(1, at=c(-3,4), tck=0.01, lab=F, lwd=2)

data1 <- rlnorm(10000,0,sdlog=0.5)

a <- density(data1)
a$y <- (a$y/max(a$y))
polygon(a$x, a$y, col=rgb(0.8,0.8,0.8),border=NA)

a <- density(log10(data1))
a$y <- (a$y/max(a$y))
lines(a$x, a$y, col=pal2tone[1],lwd=5)

text(2.5,0.8,'Original data')
text(-1.5,0.8,'Log transform',col=pal2tone[1])

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=c(-1,3), ylim=c(0,1))
axis(1, at=c(-1,3), tck=0.01, lab=F, lwd=2)

data2 <- rweibull(10000,5,1)

a <- density(data2)
a$y <- (a$y/max(a$y))
polygon(a$x, a$y, col=rgb(0.8,0.8,0.8),border=NA)

a <- density(data2^2)
a$y <- (a$y/max(a$y))
lines(a$x, a$y, col=pal2tone[1],lwd=5)

text(2,0.8,'Original data')
text(-0.2,0.8,'Squaring',col=pal2tone[1])

data1 <- data1[1:100]

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 9)}

par(mfrow=c(1,2), las=1)

ndata <- rnorm(100)
qqnorm(ndata)
qqline(ndata)

qqnorm(data1)
qqline(data1)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 12)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 12)}

quizdata <- read.csv('data/qualtricsexample.csv')
quizdata <- quizdata[,10:20]

par(mfrow=c(1,3), las=1)

hist(quizdata$Q1)
text(5,14,'(a)',cex=2)

criterion <- 3*sd(quizdata$Q1) 
normdata <- abs(quizdata$Q1-mean(quizdata$Q1)) 
quizdata <- quizdata[which(normdata<criterion),]

quizscores <- rep(0,nrow(quizdata))  
for (participant in 1:nrow(quizdata)){  # loop through participants
  for (question in 1:10){  
    if (as.character(quizdata[participant,question+1])=='A'){
      quizscores[participant] <- quizscores[participant] + 1}
  }
}

hist(quizscores)
text(7.5,9.5,'(b)',cex=2)

plot(quizdata$Q1,quizscores,type='p')
text(43,7.7,'(c)',cex=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)

