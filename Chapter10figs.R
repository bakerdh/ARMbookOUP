
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

rotate <- function(x){t(apply(x,2,rev))}
fftshift <- function(im) {im * (-1)^(row(im) + col(im))}  

mkgrating <- function(regionsize, f, o, p, c){
  # ported from the Matlab function, originally by Tim Meese
  # generates a single component sine wave grating using the following inputs:
  # regionsize is the width of the stimulus
  # f is spatial frequency in cycles per image
  # o is orientation in degrees
  # p is phase in degrees relative to the centre of the image
  # c is contrast
  
  p <- p*(pi/180)   # convert phase from degrees to radians
  o <- o*(pi/180)   # convert orientation from degrees to radians
  f <- f/regionsize # scale frequency by image size
  x0 <- (regionsize+1)/2  # locate centre of image
  y0 <- x0
  
  u <- f * cos(o) * 2*pi
  v <- f * sin(o) * 2*pi
  
  gridout <- meshgrid(1:regionsize,1:regionsize)   # generate x and y coordinate systems, requires the pracma package
  xx <- gridout$X
  yy <- gridout$Y
  
  output <- matrix(0,nrow=regionsize,ncol=regionsize)
  output <- (c * sin(u * (xx-x0) + v*(yy-y0) + p))
  
  return(output)}


gausswindow <- function(n,std){
  i <- matrix(data = (1-(n/2)):(n/2), nrow=n, ncol=n)
  j <- rotate(i)
  h <- exp(-((i^2) / (2 * std^2)) - ((j^2) / (2 * std^2)))
  return(h)}


offsetgaus <- function(n,std,x,y){
  i <- matrix(data = (1-(n/2)):(n/2), nrow=n, ncol=n)
  j <- rotate(i)
  h <- exp(-(((i+x)^2) / (2 * std^2)) - (((j+y)^2) / (2 * std^2)))
  return(h)}

# Chapter 10 figures ---------------------------------------------------------------

chapter <- 10
figno <- 2

if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

times <- seq(0,1,length=1000)
plot(times, 3.5+sin(times * 2*pi), type='l',ylim=c(0,4.5),axes=FALSE, ann=FALSE, lwd=2)
lines(times, 1+sin(times * 10 * 2*pi),lwd=2)
axis(1, at=seq(0,1,0.2), tck=0.01, lab=F, lwd=2)
mtext(text = seq(0,1,0.2), side = 1, at=seq(0,1,0.2))     # add the tick labels
title(xlab="Time (s)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
text(0.01,4.2,'1Hz')
text(0.01,2.2,'10Hz')

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 9)}

load('data/EEGdata.RData')

times <- seq(0,1,length=1000)
toplot <- allwaves[1,]
toplot <- toplot - mean(toplot)
toplot <- toplot/max(abs(toplot))
plot(times, toplot+1, type='l',xlim=c(0,3),ylim=c(0,2),axes=FALSE, ann=FALSE, lwd=2)
axis(1, at=seq(0,1,0.2), tck=0.01, lab=F, lwd=2)
mtext(text = seq(0,1,0.2), side = 1, at=seq(0,1,0.2))     # add the tick labels
mtext(text = "Time (s)", side = 1, at=0.5, cex=1.5, line=2)     # add the tick labels
mtext(text = "Waveform", side = 3, at=0.5, cex=1.5)     # add the tick labels

fspec <- abs(Re(fft(toplot)))/length(toplot)
frequencies <- (0:999)/500
lines(frequencies[2:500]+2,fspec[2:500]*10,lwd=2)
axis(1, at=seq(2,3,0.2), tck=0.01, lab=F, lwd=2)
mtext(text = seq(0,500,100), side = 1, at=seq(2,3,0.2))     # add the tick labels
mtext(text = "Frequency (Hz)", side = 1, at=2.5, cex=1.5, line=2)     # add the tick labels
mtext(text = "Fourier spectrum", side = 3, at=2.5, cex=1.5)     # add the tick labels

text(1.5,1.58,'Fourier analysis')
text(1.5,1.45,'(Fourier transform)')
lines(c(1.2,1.8),c(1.3,1.3),lwd=4)
arrows(1.8,1.3,x1=1.75, y1=1.35, length=0, angle=90, lwd=4, col='black')  # add lower error bar
arrows(1.8,1.3,x1=1.75, y1=1.25, length=0, angle=90, lwd=4, col='black')  # add lower error bar

text(1.5,0.55,'Fourier synthesis')
text(1.5,0.42,'(Inverse transform)')
lines(c(1.2,1.8),c(0.7,0.7),lwd=4)
arrows(1.2,0.7,x1=1.25, y1=0.75, length=0, angle=90, lwd=4, col='black')  # add lower error bar
arrows(1.2,0.7,x1=1.25, y1=0.65, length=0, angle=90, lwd=4, col='black')  # add lower error bar

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

times <- seq(0,1,length=1000)
plot(times, 3.5+0.2*sin(times * 2 * 2*pi), type='l',ylim=c(0,4.5),axes=FALSE, ann=FALSE, lwd=2)
lines(times, 1+sin(times * 2 * 2*pi),lwd=2)
axis(1, at=seq(0,1,0.2), tck=0.01, lab=F, lwd=2)
mtext(text = seq(0,1,0.2), side = 1, at=seq(0,1,0.2))     # add the tick labels
title(xlab="Time (s)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
text(0.01,4.2,'Low amplitude',pos=4)
text(0.01,2.2,'High amplitude',pos=4)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

frequencies <- 0:999
toplot <- allwaves[1,]
toplot <- toplot - mean(toplot)
toplot <- toplot/max(abs(toplot))
plot(frequencies[1:31], fspec[1:31], type='l',xlim=c(0,30),ylim=c(0,max(fspec[1:31])),axes=FALSE, ann=FALSE, lwd=2)
axis(1, at=seq(0,30,5), tck=0.01, lab=F, lwd=2)
mtext(text = seq(0,30,5), side = 1, at=seq(0,30,5))     # add the tick labels
title(xlab="Frequency (Hz)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

times <- seq(0,1,length=1000)
plot(times, 3.5+sin(times * 2 * 2*pi), type='l',ylim=c(0,4.5),axes=FALSE, ann=FALSE, lwd=2)
lines(times, 1+sin(times * 2 * 2*pi + pi/2),lwd=2)
lines(c(0.5,0.5),c(0,4.5),lwd=2,lty=2)
lines(c(0,1),c(1,1),lty=3)
lines(c(0,1),c(3.5,3.5),lty=3)
axis(1, at=seq(0,1,0.2), tck=0.01, lab=F, lwd=2)
mtext(text = seq(0,1,0.2), side = 1, at=seq(0,1,0.2))     # add the tick labels
title(xlab="Time (s)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
text(0.5,3.3,'Sine phase',pos=4)
text(0.5,2.2,'Cosine phase',pos=4)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 10, width = 8)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 10, width = 8)}

par(mfrow=c(2,1), las=1)
load('data/batsounds.RData')
load('data/batoutlines.RData')

batfft1 <- abs(fft(bat1))/length(bat1)
batfft2 <- abs(fft(bat2))/length(bat2)
frequencies <- ((1:(44100*0.5))-1)/0.5
times <- 1000*(1:22050)/44100

plotlims <- c(0,500,-2,2) 
ticklocsx <- seq(0,500,100)
ticklocsy <- seq(-2,2,2)
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklocsx, side = 1, at=ticklocsx)   
# mtext(text = ticklocsy, side = 2, at=ticklocsy, line=0.2, las=1)  
title(xlab="Time (ms)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Amplitude", col.lab=rgb(0,0,0), line=0.8, cex.lab=1.5)

polygon(100*batout[1,,1]+400,1.5*batout[1,,2]+1,border=NA,col='black')
polygon(100*batout[2,,1]+400,1.5*batout[2,,2]-1,border=NA,col=pal2tone[2])

lines(times,bat1+1,col='black')
lines(times,bat2-1,col=pal2tone[2])
text(20,1.2,'Pipistrelle',cex=1.5,pos=4)
text(20,-0.8,'Noctule',col=pal2tone[2],cex=1.5,pos=4)
text(0,1.9,'(a)',cex=1.8,pos=4)

plotlims <- c(0,10000,0,0.02) 
ticklocsx <- seq(0,10000,2000)
ticklocsy <- seq(0,0.02,0.02)
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklocsx, side = 1, at=ticklocsx)   
title(xlab="Frequency (Hz)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)
title(ylab="Amplitude", col.lab=rgb(0,0,0), line=0.8, cex.lab=1.5)

polygon(2000*batout[1,,1]+6000,0.0075*batout[1,,2]+0.005,border=NA,col='black')
polygon(2000*batout[2,,1]+100,0.0075*batout[2,,2]+0.008,border=NA,col=pal2tone[2])

polygon(frequencies[1:5000],batfft1[1:5000],col=rgb(0.8,0.8,0.8),border=NA)
polygon(frequencies[1:5000],batfft2[1:5000],col=pal2tone[1],border=NA)

lines(frequencies[1:5000],batfft1[1:5000],col='black')
lines(frequencies[1:5000],batfft2[1:5000],col=pal2tone[2])

text(5000,0.01,'Pipistrelle',cex=1.5,pos=4)
text(200,0.015,'Noctule',col=pal2tone[2],cex=1.5,pos=4)
text(0,0.019,'(b)',cex=1.8,pos=4)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 12)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 12)}

plot(x=NULL,y=NULL,xlim=c(0,7),ylim=c(-1,1),axes=FALSE, ann=FALSE, lwd=2)
grating <- (mkgrating(256,3,90,0,1)+1)/2
rasterImage(grating,0,-1,2,1)
grating <- (mkgrating(256,10,90,0,1)+1)/2
rasterImage(grating,2.5,-1,4.5,1)
grating <- (mkgrating(256,10,45,0,1)+1)/2
rasterImage(grating,5,-1,7,1)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 8)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 8)}

bughouse <- readJPEG('images/bughouse.jpg')
bughouse <- bughouse[,,1]
shiftbug <- fftshift(bughouse)
fftdemo <- abs(Re(fft(shiftbug)))
fftdemo[257,257] <- 0           # remove the DC component
fftdemo <- fftdemo/max(fftdemo)

plot(x=NULL,y=NULL,xlim=c(0,4.5),ylim=c(-1,1),axes=FALSE, ann=FALSE, lwd=2)

rasterImage(bughouse,0,-1,2,1)
lines(2*(1:512)/512,bughouse[256,]-mean(bughouse[256,]),lwd=3,col=pal2tone[2])
rasterImage(fftdemo[225:288,225:288],2.5,-1,4.5,1)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 7.5, width = 6.75)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 7.5, width = 6.75)}

plot(x=NULL,y=NULL,xlim=c(-10,10),ylim=c(-10,10),axes=FALSE, ann=FALSE, lwd=2)
box(lwd=2) 
axis(1, at=seq(-10,10,2), tck=0.01, lab=F, lwd=2)
axis(2, at=seq(-10,10,2), tck=0.01, lab=F, lwd=2)
axis(3, at=seq(-10,10,2), tck=0.01, lab=F, lwd=2)
axis(4, at=seq(-10,10,2), tck=0.01, lab=F, lwd=2)
mtext(text = abs(seq(-10,10,2)), side = 1, at=seq(-10,10,2))     # add the tick labels
mtext(text = abs(seq(-10,10,2)), side = 2, at=seq(-10,10,2),las=1,line=0.2)     # add the tick labels
title(xlab="Frequency (cycles per image)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes

lines(c(-10,10),c(0,0),lty=3)
lines(c(0,0),c(-10,10),lty=3)

grating <- mkgrating(64,2,0,0,1)
grating <- (grating+1)/2
rasterImage(grating,1,-1,3,1)

grating <- mkgrating(64,2,0,180,1)
grating <- (grating+1)/2
rasterImage(grating,-3,-1,-1,1)

grating<- mkgrating(64,8,0,0,1)
grating <- (grating+1)/2
rasterImage(grating,7,-1,9,1)

grating<- mkgrating(64,8,0,180,1)
grating <- (grating+1)/2
rasterImage(grating,-9,-1,-7,1)

grating<- mkgrating(64,8,90,0,1)
grating <- (grating+1)/2
rasterImage(grating,-1,7,1,9)

x <- 8/sqrt(2)
y <- x
grating <- mkgrating(64,8,315,0,1)
grating <- (grating+1)/2
rasterImage(grating,x-1,y-1,x+1,y+1)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 10, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 10, width = 9)}

specrange <- 460:540  # centre of spectrum for display

im1 <- readPNG('images/goose1.png')
im2 <- readPNG('images/goose2.png')
topleft <- c(400,900)
im1 <- apply(im1[topleft[1]+(1:1000),topleft[2]+(1:1000),1:3],1:2,mean)
im2 <- apply(im2[topleft[1]+(1:1000),topleft[2]+(1:1000),1:3],1:2,mean)
bigg <- gausswindow(1000,500)
im1 <- im1 * bigg
im2 <- im2 * bigg

g <- 1-gausswindow(1000,4)
fim1 <- Re(fftshift(fft((fft(fftshift(im1))*g), inverse=TRUE)))
fim2 <- Re(fftshift(fft((fft(fftshift(im2))*g), inverse=TRUE)))
fim1 <- fim1 - min(fim1)
fim1 <- fim1/max(fim1)
fim2 <- fim2 - min(fim2)
fim2 <- fim2/max(fim2)

fim1[which(fim1>1)] <- 1
fim1[which(fim1<0)] <- 0
fim2[which(fim2>1)] <- 1
fim2[which(fim2<0)] <- 0

plot(x=NULL,y=NULL,xlim=c(-2,2),ylim=c(-2,2),axes=FALSE, ann=FALSE, lwd=2)

rasterImage(fim1[101:900,101:900],-2,0,0,2)
rasterImage(fim2[101:900,101:900],0,0,2,2)

i <- matrix(data = (1-(1000/2)):(1000/2), nrow=1000, ncol=1000)
j <- rotate(i)
f <- sqrt(i^2 + j^2)

fft1 <- abs(fft(fftshift(im1)))
fft2 <- abs(fft(fftshift(im2)))
frat <- fft2/fft1     # ratio of Fourier spectra
frat <- frat - 1
frat[which(frat<0)] <- 0
frat[which(frat>1)] <- 1

binmids <- seq(0,40,0.1)
stepsize <- 3
circspec1 <- NULL
circspec2 <- NULL
for (n in 1:length(binmids)){
  l <- data.table::between(f,binmids[n]-stepsize,binmids[n]+stepsize)
  circspec1[n] <- mean(fft1[l])
  circspec2[n] <- mean(fft2[l])
}

# smooth the spectrum
g <- gausswindow(1000,100)
srat <- Re(fftshift(fft((fft(fftshift(frat))*g), inverse=TRUE)))
srat <- srat - min(srat)
srat <- srat/max(srat)

rasterImage(srat[specrange,specrange],-1,-2,1,0)
lines(circspec2/circspec1,(binmids/40)-1,lwd=3,col=pal2tone[1])
lines(circspec2/circspec1,-(binmids/40)-1,lwd=3,col=pal2tone[1])

lines(c(0,0),c(0,2),lwd=3,lty=2)

text(-1,1.85,'Baseline',adj=0.5,cex=2)
text(1,1.85,'Goosebumps',adj=0.5,cex=2)
text(-1.1,-1,'Ratio of spectra',adj=0.5,srt=90,cex=2)

text(-1.8,1.85,'(a)',adj=0.5,cex=3)
text(0.2,1.85,'(b)',adj=0.5,cex=3)
text(-1.8,-0.15,'(c)',adj=0.5,cex=3)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 9)}

frequencies <- (0:999)/500
times <- seq(0,1,length=1000)

toplot <- allwaves[1,]
toplot <- toplot - mean(toplot)
toplot <- toplot/max(abs(toplot))
plot(times+2, toplot+1, type='l',xlim=c(0,3),ylim=c(0,2),axes=FALSE, ann=FALSE, lwd=1,col=pal2tone[1])
axis(1, at=seq(2,3,0.2), tck=0.01, lab=F, lwd=2)
mtext(text = seq(0,1,0.2), side = 1, at=seq(2,3,0.2))     # add the tick labels
mtext(text = "Time (s)", side = 1, at=2.5, cex=1.5, line=2)     # add the tick labels
mtext(text = "Waveform", side = 3, at=2.5, cex=1.5)     # add the tick labels

fspec <- abs(Re(fft(toplot)))
filter1 <- fir1(999,60/1000,type='low')
fftfilter <- abs(Re(fft(filter1)))
fftfilter <- (max(fspec[2:101])/100)*fftfilter/max(fftfilter)
polygon(c(0,frequencies[2:101]*5,0),c(0,fftfilter[2:101],0),col=pal2tone[3],border=NA)

lines(frequencies[2:101]*5,fftfilter[2:101],lwd=2,col=pal2tone[2])
lines(frequencies[2:101]*5,fspec[2:101]/100,lwd=2)
axis(1, at=seq(0,1,0.2), tck=0.01, lab=F, lwd=2)
mtext(text = seq(0,100,20), side = 1, at=seq(0,1,0.2))     # add the tick labels
mtext(text = "Frequency (Hz)", side = 1, at=0.5, cex=1.5, line=2)     # add the tick labels
mtext(text = "Fourier spectrum", side = 3, at=0.5, cex=1.5)     # add the tick labels

filteredwave <- Re(fft(fft(allwaves[1,])*abs(fft(filter1)),inverse=TRUE))
filteredwave <- filteredwave - mean(filteredwave)
filteredwave <- filteredwave/max(abs(filteredwave))
lines(times+2, filteredwave+1, col='black', lwd=3)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 10, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 10, width = 9)}

plot(x=NULL,y=NULL,xlim=c(-2,2),ylim=c(-2,2),axes=FALSE, ann=FALSE, lwd=2)

g <- gausswindow(512,12)
lowpassbug <- Re(fftshift(fft((fft(shiftbug)*g), inverse=TRUE)))
lowpassbug <- lowpassbug/max(lowpassbug)
rasterImage(g,-2,0,0,2)
rasterImage(lowpassbug,-2,-2,0,0)
lines(2*(1:512)/512-2,lowpassbug[256,]-mean(lowpassbug[256,])-1,lwd=3,col=pal2tone[2])

hipassbug <- Re(fftshift(fft((fft(shiftbug)*(1-g)), inverse=TRUE)))
hipassbug <- hipassbug - min(hipassbug)
hipassbug <- hipassbug/max(hipassbug)
rasterImage(1-g,0,0,2,2)
rasterImage(hipassbug,0,-2,2,0)
lines(2*(1:512)/512,hipassbug[256,]-mean(hipassbug[256,])-1,lwd=3,col=pal2tone[2])

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 10, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 10, width = 9)}

plot(x=NULL,y=NULL,xlim=c(-2,2),ylim=c(-2,2),axes=FALSE, ann=FALSE, lwd=2)

gridout <- meshgrid(1:512,1:512)   # generate x and y coordinate systems, requires the pracma package
xx <- gridout$X - 257
yy <- gridout$Y - 257
r <- atan2(yy,xx)
radfilt <- cos(2*r)
radfilt <- (radfilt+1)/2

vertbug <- Re(fftshift(fft((fft(shiftbug)*radfilt), inverse=TRUE)))
vertbug <- vertbug - min(vertbug)
vertbug <- vertbug/max(vertbug)
rasterImage(radfilt,-2,0,0,2)
rasterImage(vertbug,-2,-2,0,0)
lines(2*(1:512)/512-2,vertbug[256,]-mean(vertbug[256,])-1,lwd=3,col=pal2tone[2])

horbug <- Re(fftshift(fft((fft(shiftbug)*(1-radfilt)), inverse=TRUE)))
horbug <- horbug - min(horbug)
horbug <- horbug/max(horbug)
rasterImage(1-radfilt,0,0,2,2)
rasterImage(horbug,0,-2,2,0)
lines(2*(1:512)/512,horbug[256,]-mean(horbug[256,])-1,lwd=3,col=pal2tone[2])

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 9, width = 7.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 9, width = 7.5)}

par(mfrow=c(1,2), las=1)

plot(x=NULL,y=NULL,xlim=c(0,10),ylim=c(0,6),axes=FALSE, ann=FALSE, lwd=2)
duration <- 5
samplerate <- 100
sqspect <- (1:(duration*samplerate))*0  # create an empty spectrum
frequencies <- ((1:(duration*samplerate))-1)/duration
allsqs <- matrix(0,nrow=6,ncol=duration*samplerate)
phaselist <- (1:(duration*samplerate))*0

for (n in 1:5){
  findex <- 2*n - 1
  sqspect[findex*duration+1] <- 1/findex
  sqspect[(duration*samplerate)-(findex*duration-1)] <- 1/findex
  phaselist[findex*duration+1] <- -pi/2
  phaselist[(duration*samplerate)-(findex*duration-1)] <- pi/2
  lines(frequencies[1:51],6-n+sqspect[1:51]*0.9,lwd=2)
  allsqs[n,] <- (Re(fft(complex(modulus=sqspect,argument=phaselist),inverse=TRUE))/2 + 1)/2
}

for (n in 1:20){
  findex <- 9 + 2*n
  sqspect[findex*duration+1] <- 1/findex
  sqspect[(duration*samplerate)-(findex*duration-1)] <- 1/findex
  phaselist[findex*duration+1] <- -pi/2
  phaselist[(duration*samplerate)-(findex*duration-1)] <- pi/2
}
allsqs[6,] <- (Re(fft(complex(modulus=sqspect,argument=phaselist),inverse=TRUE))/2 + 1)/2

axis(1, at=c(0,seq(1,9,2),10), tck=0.01, lab=F, lwd=2)
mtext(text = seq(1,9,2), side = 1, at=seq(1,9,2))     # add the tick labels
mtext(text = "Frequency (Hz)", side = 1, cex=1.5, line=1.5)     # add the tick labels
text(10,5.5,'1F',pos=2)
text(10,4.5,'1F+3F',pos=2)
text(10,3.5,'1F+3F+5F',pos=2)
text(10,2.5,'1F+3F+5F+7F',pos=2)
text(10,1.5,'1F+3F+5F+7F+9F',pos=2)
text(10,0.5,'1F to 49F',pos=2)

plot(x=NULL,y=NULL,xlim=c(0,5),ylim=c(0,6),axes=FALSE, ann=FALSE, lwd=2)
times <- seq(0,duration,length=(duration*samplerate))
for (n in 1:6){lines(times,6-n+allsqs[n,],lwd=2)}

axis(1, at=seq(0,5,1), tck=0.01, lab=F, lwd=2)
mtext(text = seq(0,5,1), side = 1, at=seq(0,5,1))     # add the tick labels
mtext(text = "Time (s)", side = 1, cex=1.5, line=1.5)     # add the tick labels

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 10, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 10, width = 9)}

plot(x=NULL,y=NULL,xlim=c(-2,2),ylim=c(-2,2),axes=FALSE, ann=FALSE, lwd=2)

g <- offsetgaus(512,12,20,20) + offsetgaus(512,12,-20,-20)
g <- g/max(g)
spatialstim <- Re(fftshift(fft(fftshift(g), inverse=TRUE)))
spatialstim <- spatialstim/max(abs(spatialstim))
spatialstim <- (spatialstim + 1)/2
rasterImage(g,-2,0,0,2)
rasterImage(spatialstim,-2,-2,0,0)

g <- offsetgaus(512,4,20,20) + offsetgaus(512,4,-20,-20)
g <- g/max(g)
spatialstim <- Re(fftshift(fft(fftshift(g), inverse=TRUE)))
spatialstim <- spatialstim/max(abs(spatialstim))
spatialstim <- (spatialstim + 1)/2
rasterImage(g,0,0,2,2)
rasterImage(spatialstim,0,-2,2,0)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

load('data/EEGdata.RData')
thiswave <- allwaves[1,]
output <- fft(thiswave)/length(thiswave)
samplerate <- 1000
duration <- 1
frequencies <- ((1:(samplerate*duration))-1)/duration
plot(frequencies[2:500],abs(output[2:500]),type='l',lwd=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

cutfrequency <- 15
filter1 <- fir1(samplerate-1,2*cutfrequency/(samplerate/2),type='low')
plot(filter1,type='l',lwd=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

filteredspectrum <- output*abs(fft(filter1))    
# inverse transform and take the Real values
filteredwave <- Re(fft(filteredspectrum,inverse=TRUE)) 
plot(1:1000,filteredwave,type='l',lwd=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 8)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 8)}

bughouse <- readJPEG('images/bughouse.jpg')
bughouse <- bughouse[,,1]
bugspectrum <- fft(fftshift(bughouse))
g <- offsetgaus(512,8,20,20) + offsetgaus(512,8,-20,-20)

filteredimage <- Re(fftshift(fft((bugspectrum*g), inverse=TRUE)))
filteredimage <- filteredimage - min(filteredimage)
filteredimage <- filteredimage/max(filteredimage)
plot(x=NULL,y=NULL,xlim=c(0,4.5),ylim=c(-1,1),axes=FALSE, ann=FALSE, lwd=2)

rasterImage(filteredimage,0,-1,2,1)

rasterImage(bughouse,2.5,-1,4.5,1)
points(3.75,0.25,pch=1,col=pal2tone[2],cex=8,lwd=8)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)

