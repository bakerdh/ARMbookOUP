
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

# Chapter 17 figures ---------------------------------------------------------------

chapter <- 17
figno <- 1

if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

npoints <- 8
std <- 4
samplesize <- 2^((1:npoints)+1)

sigcount <- matrix(data = 0, nrow=3,ncol=npoints)
for (m in 1:3){
  for (s in 1:npoints){
    for (n in 1:nsims){
      sample <- rnorm(samplesize[s], mean = m-1, sd = std)
      testresults <- t.test(sample)
      if (testresults$p.value<0.05){sigcount[m,s] <- sigcount[m,s] + 1}
    }}}
sigcount <- sigcount/nsims

plotlims <- c(0.5,3,-2,0)
ticklocsx <- log10(samplesize)  
ticklocsy <- log10(c(0.01,0.02,0.05,0.1,0.2,0.5,1)) 
ticklabelsx <- c("4", "8", "16", "32", "64", "128", "256", "512")
ticklabelsy <- c("0.01 ", "0.02 ", "0.05 ", "0.1 ", "0.2 ", "0.5 ", "1 ")
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)  
title(xlab="Sample size (N)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.3)
title(ylab="Proportion significant", col.lab=rgb(0,0,0), line=2, cex.lab=1.3)

lines(log10(samplesize),log10(sigcount[1,]),col="black",lwd=4)
lines(log10(samplesize),log10(sigcount[2,]),col=pal2tone[1],lwd=4)
lines(log10(samplesize),log10(sigcount[3,]),col="black",lty=2,lwd=4)
points(log10(samplesize),log10(sigcount[1,]),pch=21,col='black', bg='black',lwd=2)
points(log10(samplesize),log10(sigcount[2,]),pch=22,col='black', bg=pal2tone[1],lwd=2)
points(log10(samplesize),log10(sigcount[3,]),pch=23,col='black', bg='white',lwd=2)

legend(2, -0.5, c("d=0.5","d=0.25", "d=0"), cex=1, col="black", pt.bg=c("white",pal2tone[1],"black"), pch=23:21, pt.lwd=2, pt.cex=1.2, box.lwd=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- 3
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5*1.2, width = 5*1.2)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5*1.2, width = 5*1.2)}

plotlims <- c(0,1,0,1) 
par(pty="s") 
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])
lines(c(0,1),c(0.25,0.25))
lines(c(0,1),c(0.5,0.5))
lines(c(0,1),c(0.75,0.75))
lines(c(0.25,0.25),c(0,1))
lines(c(0.5,0.5),c(0,1))
lines(c(0.75,0.75),c(0,1))

text(0.375,0.875,'Positive',cex=1.2)
text(0.625,0.875,'Negative',cex=1.2)
text(0.125,0.625,'Positive',cex=1.2)
text(0.125,0.375,'Negative',cex=1.2)
text(0.875,0.875,'Total',cex=1.2)
text(0.125,0.125,'Total',cex=1.2)

text(0.375,0.625,'9',cex=1.5)
text(0.625,0.625,'1',cex=1.5)
text(0.375,0.375,'50',cex=1.5)
text(0.625,0.375,'940',cex=1.5)

text(0.875,0.625,'10',cex=1.5,col='grey')
text(0.875,0.375,'990',cex=1.5,col='grey')
text(0.375,0.125,'59',cex=1.5,col='grey')
text(0.625,0.125,'941',cex=1.5,col='grey')

text(0.375,0.53,'Hits',cex=1,col='grey')
text(0.625,0.53,'Misses',cex=1,col='grey')
text(0.375,0.28,'FAs',cex=1,col='grey')
text(0.625,0.28,'CRs',cex=1,col='grey')

mtext("True status", cex=1.5,side=2)
mtext("Test result", cex=1.5,side=3)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

sensvals <- seq(0,1,0.01)
specval <- 0.9

plotlims <- c(0,1,0,1)  # define the x and y limits of the plot (minx,maxx,miny,maxy)
ticklocsx <- seq(0,1,0.2)    # locations of tick marks on x axis
ticklocsy <- seq(0,1,0.2)    # locations of tick marks on y axis
ticklabelsx <- ticklocsx       # set labels for x ticks
ticklabelsy <- ticklocsy    # set labels for y ticks
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)  
title(xlab="Sensitivity", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.3)
title(ylab="Probabilty of disease if positive", col.lab=rgb(0,0,0), line=2, cex.lab=1.3)

for (s in 1:9){
  baseline <- s/10
  outcome <- (sensvals*baseline)/(sensvals*baseline + (1-specval)*(1-baseline))
  lines(sensvals,outcome,lwd=4,col=rgb(1-(s/10),1-(s/10),1-(s/10)))
}

legend(0.55,0.25,c('Baseline = 0.9', 'Baseline = 0.1'),col=c(rgb(0.1,0.1,0.1),rgb(0.9,0.9,0.9)),lwd=4,box.lwd=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

xvals <- seq(-1,1,0.01)
priordist <- dcauchy(xvals, location=0, scale = 0.1)
datadist <- dnorm(xvals, 0.3, sd=0.1)

posteriordist <- priordist * datadist

plotlims <- c(-1,1,0,1)
ticklocsx <- seq(-1,1,0.5)  
# ticklocsy <- seq(0,1,0.2)   
ticklabelsx <- ticklocsx      
# ticklabelsy <- ticklocsy 
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)   
# axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)  
# mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)  
title(xlab="Effect size (d)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.3)
# title(ylab="Density", col.lab=rgb(0,0,0), line=2, cex.lab=1.3)

legend(-1,1,c('Prior', 'Evidence', 'Posterior'), lty=c(3,2,1), col=c('black','grey',pal2tone[1]), lwd=4, box.lwd=2)

lines(xvals,priordist/max(priordist),lwd=4,lty=3)
lines(xvals,datadist/max(datadist),lwd=4,lty=2,col='grey')
lines(xvals,posteriordist/max(posteriordist),lwd=4,col=pal2tone[1])

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

npoints <- 8
std <- 4
samplesize <- 2^((1:npoints)+1)

bftotal <- matrix(data = 0, nrow=3,ncol=npoints)
for (m in 1:3){
  for (s in 1:npoints){
    bflist <- NULL
    for (n in 1:nsims){
      sample <- rnorm(samplesize[s], mean = m-1, sd = std)
      bflist[n] <- as.numeric(extractBF(ttestBF(sample))[1])
    }
    bftotal[m,s] <- mean(bflist,trim = 0.1)
  }}

plotlims <- c(0.5,3,-1,3)  # define the x and y limits of the plot (minx,maxx,miny,maxy)
ticklocsx <- log10(samplesize)    # locations of tick marks on x axis
ticklocsy <- log10(c(0.1,1,10,100,1000))    # locations of tick marks on y axis
ticklabelsx <- c("4", "8", "16", "32", "64", "128", "256", "512")
ticklabelsy <- c("0.1", "1", "10", "100", "1000")
plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)  
title(xlab="Sample size (N)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.3)
title(ylab="Bayes Factor", col.lab=rgb(0,0,0), line=2, cex.lab=1.3)

lines(log10(samplesize),log10(bftotal[1,]),col="black",lwd=4)
lines(log10(samplesize),log10(bftotal[2,]),col=pal2tone[1],lwd=4)
lines(log10(samplesize),log10(bftotal[3,]),col="black",lty=2,lwd=4)
points(log10(samplesize),log10(bftotal[1,]),pch=21,col='black', bg='black',lwd=2)
points(log10(samplesize),log10(bftotal[2,]),pch=22,col='black', bg=pal2tone[1],lwd=2)
points(log10(samplesize),log10(bftotal[3,]),pch=23,col='black', bg='white',lwd=2)

legend(2, 0.5, c("d=0.5","d=0.25", "d=0"), cex=1, col="black", pt.bg=c("white",pal2tone[1],"black"), pch=23:21, pt.lwd=2, pt.cex=1.2, box.lwd=2)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

set.seed(1703)
data <- rnorm(20, mean=2, sd=3)
data <- rnorm(20, mean=0, sd=3)

dv1 <- rnorm(60, mean = 3, sd = 3) 
dv2 <- rnorm(60, mean = 4, sd = 3) 
dv3 <- rnorm(60, mean = 5, sd = 3) 
alldata <- c(dv1,dv2,dv3)
group <- gl(3,60,labels = c("DV1", "DV2", "DV3"))
dataset <- data.frame(group,alldata) 
plot(alldata ~ group)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)
