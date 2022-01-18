# install and load relevant packages
packagelist <- c('lme4','lmerTest')
missingpackages <- packagelist[!packagelist %in% installed.packages()[,1]]
if (length(missingpackages)>0){install.packages(missingpackages)}
toinstall <- packagelist[which(!packagelist %in% (.packages()))]
invisible(lapply(toinstall,library,character.only=TRUE))

# set outputplot to 2 for saving as PDF (change to 1 if you want EPS)
outputplot <- 2
# set colours for all plots
pal2tone <- c('#8783CF','#10069F','#CFCDEC')  # blue 072

# Chapter 7 figures ---------------------------------------------------------------

chapter <- 7
figno <- 1

if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

npergroup <- 100
group <- rep(1:5,each=npergroup)
IV <- NULL
DV <- NULL

set.seed(1703)
IV[1:npergroup] <- rnorm(npergroup)*10 + 30
DV[1:npergroup] <- rnorm(npergroup)*10 + 30 + IV[1:npergroup]

IV[(npergroup+1):(2*npergroup)] <- rnorm(npergroup)*10 + 40
DV[(npergroup+1):(2*npergroup)] <- rnorm(npergroup)*10 + IV[(npergroup+1):(2*npergroup)]

IV[(2*npergroup+1):(3*npergroup)] <- rnorm(npergroup)*10 + 50
DV[(2*npergroup+1):(3*npergroup)] <- rnorm(npergroup)*10 + IV[(2*npergroup+1):(3*npergroup)]

IV[(3*npergroup+1):(4*npergroup)] <- rnorm(npergroup)*10 + 60
DV[(3*npergroup+1):(4*npergroup)] <- rnorm(npergroup)*10 + 0.3*IV[(3*npergroup+1):(4*npergroup)]

IV[(4*npergroup+1):(5*npergroup)] <- rnorm(npergroup)*10 + 70
DV[(4*npergroup+1):(5*npergroup)] <- rnorm(npergroup)*10 - 20 + IV[(4*npergroup+1):(5*npergroup)]

groupdata <- data.frame(IV,DV,group)
simmodel1 <- lm(DV ~ 1, data=groupdata)
simmodel2 <- lm(DV ~ IV, data=groupdata)

xvals <- 0:100

plotlims <- c(0,100,0,100)  # define the x and y limits of the plot (minx,maxx,miny,maxy)
ticklocsx <- seq(0,100,20)    # locations of tick marks on x axis
ticklocsy <- seq(0,100,20)    # locations of tick marks on y axis
ticklabelsx <- ticklocsx        # set labels for x ticks
ticklabelsy <- ticklocsy    # set labels for y ticks

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4]) 
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="IV", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="DV", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)
title('Linear regression', cex.lab=1.5)

lines(xvals,xvals*0 + simmodel1$coefficients[1],col='darkgrey',lwd=6)

lines(xvals,xvals*simmodel2$coefficients[2] + simmodel2$coefficients[1],col='black',lty=2,lwd=3)

for (n in 1:5){points(IV[((n-1)*npergroup+1):(n*npergroup)],DV[((n-1)*npergroup+1):(n*npergroup)],pch=16,col=pal2tone[1],cex=0.5)}

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)





figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 9, width = 13)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 9, width = 13)}

# groupcolours <- NULL
# groupcolours[1] <- flatalpha(rgb(178/255,17/255,23/255),0.5)
# groupcolours[2] <- flatalpha(rgb(83/255,198/255,111/255),0.5)
# groupcolours[3] <- flatalpha(rgb(6/255,172/255,208/255),0.5)
# groupcolours[4] <- flatalpha(rgb(161/255,130/255,165/255),0.5)
# groupcolours[5] <- flatalpha(rgb(203/255,188/255,85/255),0.5)

groupcolours <- c('grey',pal2tone[1],'black',pal2tone[2:3])

par(mfrow=c(2,2), las=1)

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4]) 
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="IV", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="DV", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)
title('Data tagged by group', cex.lab=1.5)
text(1,95,'(a)',cex=1.5)

for (n in 1:5){points(IV[((n-1)*npergroup+1):(n*npergroup)],DV[((n-1)*npergroup+1):(n*npergroup)],pch=16,col=groupcolours[n],cex=0.5)}


simmodel3 <- lmer(DV ~ IV + (1|group), data=groupdata)
params <- coef(simmodel3)
globalparams <- coef(summary(simmodel3))[,"Estimate"]

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4]) 
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="IV", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="DV", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)
title('Random intercepts', cex.lab=1.5)
text(1,95,'(b)',cex=1.5)

lines(xvals,xvals*globalparams[2] + globalparams[1],col='black',lwd=4,lty=2)

for (n in 1:5){lines(xvals,xvals*params$group[n,2] + params$group[n,1],col=groupcolours[n],lwd=2)}

for (n in 1:5){points(IV[((n-1)*npergroup+1):(n*npergroup)],DV[((n-1)*npergroup+1):(n*npergroup)],pch=16,col=groupcolours[n],cex=0.5)}


simmodel4 <- lmer(DV ~ IV + (0 + IV|group), data=groupdata)
params <- coef(simmodel4)
globalparams <- coef(summary(simmodel4))[,"Estimate"]

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4]) 
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="IV", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="DV", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)
title('Random slopes', cex.lab=1.5)
text(1,95,'(c)',cex=1.5)

lines(xvals,xvals*globalparams[2] + globalparams[1],col='black',lwd=4,lty=2)

for (n in 1:5){lines(xvals,xvals*params$group[n,2] + params$group[n,1],col=groupcolours[n],lwd=2)}

for (n in 1:5){points(IV[((n-1)*npergroup+1):(n*npergroup)],DV[((n-1)*npergroup+1):(n*npergroup)],pch=16,col=groupcolours[n],cex=0.5)}


simmodel5 <- lmer(DV ~ IV + (1 + IV|group), data=groupdata)
params <- coef(simmodel5)
globalparams <- coef(summary(simmodel5))[,"Estimate"]

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4]) 
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="IV", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5)    # titles for axes
title(ylab="DV", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)
title('Random intercepts and slopes', cex.lab=1.5)
text(1,95,'(d)',cex=1.5)

lines(xvals,xvals*globalparams[2] + globalparams[1],col='black',lwd=4,lty=2)

for (n in 1:5){lines(xvals,xvals*params$group[n,2] + params$group[n,1],col=groupcolours[n],lwd=2)}

for (n in 1:5){points(IV[((n-1)*npergroup+1):(n*npergroup)],DV[((n-1)*npergroup+1):(n*npergroup)],pch=16,col=groupcolours[n],cex=0.5)}

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)


figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

dolphindata <- read.csv('data/Sarasota_dolphin.csv')
load('data/dolphinoutlines.RData')

vt <- c(dolphindata$expvt,dolphindata$inspvt+1)
bodymass <- c(dolphindata$mb,dolphindata$mb)
animal <- c(dolphindata$animal,dolphindata$animal)
direction <- rep(1:2,each=63)
rsdolphins <- data.frame(animal,bodymass,vt,direction)
dolphmod <- lmer(vt ~ bodymass + direction + (1|animal), data=rsdolphins)
params <- coef(summary(dolphmod))[,"Estimate"]

plotlims <- c(50,300,0,12)  # define the x and y limits of the plot (minx,maxx,miny,maxy)
ticklocsx <- seq(50,300,50)    # locations of tick marks on x axis
ticklocsy <- seq(0,12,2)   # locations of tick marks on y axis
ticklabelsx <- ticklocsx       # set labels for x ticks
ticklabelsy <- ticklocsy    # set labels for y ticks

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])  
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1)
title(xlab="Body mass (kg)", col.lab=rgb(0,0,0), line=1.2, cex.lab=1.5) 
title(ylab="Tidal volume (l)", col.lab=rgb(0,0,0), line=1.5, cex.lab=1.5)

polygon(36*dolphinout[,1]+120,6*dolphinout[,2]+8.5,border=NA,col=pal2tone[1])

points(dolphindata$mb, dolphindata$expvt, pch=21, col='black', bg='black')
points(dolphindata$mb, dolphindata$inspvt+1, pch=21, col='black', bg=pal2tone[1])

x <- 50:300
lines(x,params[1] + params[2]*x + 1*params[3],lty=1,lwd=2)
lines(x,params[1] + params[2]*x + 2*params[3],lty=2,lwd=2,col=pal2tone[2])

legend(50,12,c('Expiration','Inspiration'),pch=21,col='black',pt.bg=c('black',pal2tone[1]),box.lwd=2)


if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

subjectoffsets <- rnorm(20,mean=0,sd=50)
itemoffsets <- rnorm(40,mean=0,sd=25)

allnounRT <- matrix(0,nrow=20,ncol=20)
allnounRT2 <- matrix(0,nrow=20,ncol=20)
allverbRT <- matrix(0,nrow=20,ncol=20)
allverbRT2 <- matrix(0,nrow=20,ncol=20)

for (n in 1:20){
  for (m in 1:20){
    allnounRT[n,m] <- round(rnorm(1,mean=0,sd=40)) + 550 + subjectoffsets[n] + itemoffsets[m]
    allnounRT2[n,m] <- round(rnorm(1,mean=0,sd=40)) + 650 + subjectoffsets[n] + itemoffsets[m]
    allverbRT[n,m] <- round(rnorm(1,mean=0,sd=40)) + 600 + subjectoffsets[n] + itemoffsets[m+20]
    allverbRT2[n,m] <- round(rnorm(1,mean=0,sd=40)) + 650 + subjectoffsets[n] + itemoffsets[m+20]
  }}

nounRT <- round(allnounRT[1,])
nounRT2 <- round(allnounRT2[1,])
verbRT <- round(allverbRT[1,])
verbRT2 <- round(allverbRT2[1,])
nounRT[21] <- round(mean(nounRT))
nounRT2[21] <- round(mean(nounRT2))
verbRT[21] <- round(mean(verbRT))
verbRT2[21] <- round(mean(verbRT2))

meanRT <- c(rowMeans(allnounRT),rowMeans(allnounRT2),rowMeans(allverbRT),rowMeans(allverbRT2))
itemRT <- c(colMeans(allnounRT),colMeans(allnounRT2),colMeans(allverbRT),colMeans(allverbRT2))
subject <- rep(1:20,4)
item <- 1:80
wordtype <- rep(1:2,each=40)
validity <- rep(1:2,2,each=20)
RTdata <- data.frame(subject,item,wordtype,validity,meanRT,itemRT)

plotlims <- c(0,1,400,800)  # define the x and y limits of the plot (minx,maxx,miny,maxy)
ticklocsx <- seq(0,1,0.2)    # locations of tick marks on x axis
ticklocsy <- seq(400,800,100)    # locations of tick marks on y axis
ticklabelsx <- c("","Nouns","Non-nouns","Verbs","Non-verbs","")
ticklabelsy <- ticklocsy

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx,line=0.2,las=2)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1) 
title(ylab="Reaction time (ms)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

colvect <- c('black',pal2tone)

for (s in 1:20){
  lines((1:4)/5-0.05+s/200,meanRT[(0:3)*20+s],col=rgb(0.8,0.8,0.8))
}

for (cond in 1:4){
  lines((cond/5)+c(-0.05,0.05),c(mean(meanRT[(cond-1)*20+(1:20)]),mean(meanRT[(cond-1)*20+(1:20)])),lwd=2,col='black')
  arrows(cond/5-0.05,mean(meanRT[(cond-1)*20+(1:20)]),x1=cond/5-0.05, y1=mean(meanRT[(cond-1)*20+(1:20)])-sd(meanRT[(cond-1)*20+(1:20)]), length=0.015, angle=90, lwd=2, col='black')
  arrows(cond/5-0.05,mean(meanRT[(cond-1)*20+(1:20)]),x1=cond/5-0.05, y1=mean(meanRT[(cond-1)*20+(1:20)])+sd(meanRT[(cond-1)*20+(1:20)]), length=0.015, angle=90, lwd=2, col='black')
  arrows(cond/5+0.05,mean(meanRT[(cond-1)*20+(1:20)]),x1=cond/5+0.05, y1=mean(meanRT[(cond-1)*20+(1:20)])-sd(meanRT[(cond-1)*20+(1:20)]), length=0.015, angle=90, lwd=2, col='black')
  arrows(cond/5+0.05,mean(meanRT[(cond-1)*20+(1:20)]),x1=cond/5+0.05, y1=mean(meanRT[(cond-1)*20+(1:20)])+sd(meanRT[(cond-1)*20+(1:20)]), length=0.015, angle=90, lwd=2, col='black')
  for (s in 1:20){
    points(cond/5-0.05+s/200,meanRT[(cond-1)*20+s],pch=21,col='black',bg=colvect[cond])
  }
}

text(0,780,'By participants',cex=1.8,adj=0)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)




figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

plotlims <- c(0,1,400,800)  # define the x and y limits of the plot (minx,maxx,miny,maxy)
ticklocsx <- seq(0,1,0.2)    # locations of tick marks on x axis
ticklocsy <- seq(400,800,100)    # locations of tick marks on y axis
ticklabelsx <- c("","Nouns","Non-nouns","Verbs","Non-verbs","")
ticklabelsy <- ticklocsy

plot(x=NULL,y=NULL,axes=FALSE, ann=FALSE, xlim=plotlims[1:2], ylim=plotlims[3:4])   # create an empty axis of the correct dimensions
axis(1, at=ticklocsx, tck=0.01, lab=F, lwd=2)     # plot tick marks (no labels)
axis(2, at=ticklocsy, tck=0.01, lab=F, lwd=2)
mtext(text = ticklabelsx, side = 1, at=ticklocsx,line=0.2,las=2)     # add the tick labels
mtext(text = ticklabelsy, side = 2, at=ticklocsy, line=0.2, las=1) 
title(ylab="Reaction time (ms)", col.lab=rgb(0,0,0), line=1.8, cex.lab=1.5)

for (s in 1:20){
  lines((1:2)/5-0.05+s/200,itemRT[(0:1)*20+s],col=rgb(0.8,0.8,0.8))
  lines((3:4)/5-0.05+s/200,itemRT[(2:3)*20+s],col=rgb(0.8,0.8,0.8))
}

for (cond in 1:4){
  lines((cond/5)+c(-0.05,0.05),c(mean(itemRT[(cond-1)*20+(1:20)]),mean(itemRT[(cond-1)*20+(1:20)])),lwd=2,col='black')
  
  arrows(cond/5-0.05,mean(itemRT[(cond-1)*20+(1:20)]),x1=cond/5-0.05, y1=mean(itemRT[(cond-1)*20+(1:20)])-sd(itemRT[(cond-1)*20+(1:20)]), length=0.015, angle=90, lwd=2, col='black')
  arrows(cond/5-0.05,mean(itemRT[(cond-1)*20+(1:20)]),x1=cond/5-0.05, y1=mean(itemRT[(cond-1)*20+(1:20)])+sd(itemRT[(cond-1)*20+(1:20)]), length=0.015, angle=90, lwd=2, col='black')
  
  arrows(cond/5+0.05,mean(itemRT[(cond-1)*20+(1:20)]),x1=cond/5+0.05, y1=mean(itemRT[(cond-1)*20+(1:20)])-sd(itemRT[(cond-1)*20+(1:20)]), length=0.015, angle=90, lwd=2, col='black')
  arrows(cond/5+0.05,mean(itemRT[(cond-1)*20+(1:20)]),x1=cond/5+0.05, y1=mean(itemRT[(cond-1)*20+(1:20)])+sd(itemRT[(cond-1)*20+(1:20)]), length=0.015, angle=90, lwd=2, col='black')
  
  for (s in 1:20){
    points(cond/5-0.05+s/200,itemRT[(cond-1)*20+s],pch=22,bg=colvect[cond])
  }
}

text(0,780,'By items',cex=1.8,adj=0)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

allsubj <- NULL
allitem <- NULL
alltype <- NULL
allvalid <- NULL
allRT <- NULL
counter <- 0
for (s in 1:20){
  for (i in 1:20){
    counter <- counter + 1
    allsubj[counter] <- s
    allitem[counter] <- i
    alltype[counter] <- 1
    allvalid[counter] <- 1
    allRT[counter] <- allnounRT[s,i]
    
    counter <- counter + 1
    allsubj[counter] <- s
    allitem[counter] <- i
    alltype[counter] <- 1
    allvalid[counter] <- 2
    allRT[counter] <- allnounRT2[s,i]
    
    counter <- counter + 1
    allsubj[counter] <- s
    allitem[counter] <- i+20
    alltype[counter] <- 2
    allvalid[counter] <- 1
    allRT[counter] <- allverbRT[s,i]
    
    counter <- counter + 1
    allsubj[counter] <- s
    allitem[counter] <- i+20
    alltype[counter] <- 2
    allvalid[counter] <- 2
    allRT[counter] <- allverbRT2[s,i]
    
  }
}

RTlmm <- data.frame(allsubj,allitem,alltype,allvalid,allRT)
colnames(RTlmm) <- c('subject','item','wordtype','validity','RT')

model <- lmer(RT ~ wordtype * validity + (1|subject) + (1|item), data=RTlmm)

modelresiduals <- resid(model) # extract the residuals
qqnorm(modelresiduals)  # create the plot
qqline(modelresiduals)  # add the diagonal line

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)
