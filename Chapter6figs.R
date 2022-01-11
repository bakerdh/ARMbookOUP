# install & load necessary packages
packagelist <- c('rmeta','MAd','compute.es','PRISMAstatement', 'DiagrammeR',
                 'rsvg', 'DiagrammeRsvg')
missingpackages <- packagelist[!packagelist %in% installed.packages()[,1]]
if (length(missingpackages)>0){install.packages(missingpackages)}
toinstall <- packagelist[which(!packagelist %in% (.packages()))]
invisible(lapply(toinstall,library,character.only=TRUE))

# set outputplot to 2 for saving as PDF (change to 1 if you want EPS)
outputplot <- 2
# set colours for all plots
pal2tone <- c('#8783CF','#10069F','#CFCDEC')  # blue 072

# Chapter 6 figures ---------------------------------------------------------------
chapter <- 6
figno <- 1

graph <- prisma_graph(found = 401,
                      found_other = 13,
                      no_dupes = 412,
                      screened = 412,
                      screen_exclusions = 239,
                      full_text = 173,
                      full_text_exclusions = 108,
                      qualitative = 65,
                      quantitative = 65,
                      width = 800, height = 800)

graph2 <- grViz(graph)

if(outputplot==1){
graph2 %>%
  export_svg %>%
  charToRaw %>%
  rsvg_ps(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''))
}
if(outputplot==2){
  graph2 %>%
    export_svg %>%
    charToRaw %>%
    rsvg_pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''))
}


figno <- figno + 1

if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 9)}

eventcounts <- 1:99
N <- 100
riskscores <- eventcounts/N
oddsscores <- eventcounts/(N-eventcounts)

par(mfrow=c(1,2), las=1)

plot(eventcounts,riskscores,type='l',lwd=3,xlim=c(0,max(eventcounts)),ylim=c(0,10),ann=FALSE)
lines(eventcounts,oddsscores,col=pal2tone[1],lwd=3,lty=2)
title(xlab="Number of events", col.lab=rgb(0,0,0), line=2.2, cex.lab=1.5)    # titles for axes
title(ylab="Risk or odds", col.lab=rgb(0,0,0), line=2.2, cex.lab=1.5)
legend(0,10,c('Risk','Odds'),lwd=3,col=c('black',pal2tone[1]),cex=0.8,lty=1:2)

plot(log10(eventcounts),log10(riskscores),type='l',lwd=3,xlim=c(0,2),ylim=c(-2,2),ann=FALSE)
lines(log10(eventcounts),log10(oddsscores),col=pal2tone[1],lwd=3,lty=2)
title(xlab="Log10(Events)", col.lab=rgb(0,0,0), line=2.2, cex.lab=1.5)    # titles for axes
title(ylab="Log10(Risk or odds)", col.lab=rgb(0,0,0), line=2.2, cex.lab=1.5)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 4.5, width = 6.5)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 4.5, width = 6.5)}

data(cochrane)
steroid <- meta.MH(n.trt, n.ctrl, ev.trt, ev.ctrl,names=name, data=cochrane)
plot(steroid)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)



figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 9)}

par(mfrow=c(1,2), las=1)

nstudies <- 200
trued <- 1
truesd <- 1
alld <- NULL
allN <- NULL
for (n in 1:nstudies){
  allN[n] <- 3+round(50*abs(rnorm(1)))
  alld[n] <- mean(rnorm(allN[n],mean=trued,sd=truesd))
}
plot(c(trued,trued),c(0,100),type='l',lwd=2,xlim=c(0,2),ylim=c(0,100),ann=FALSE,col=rgb(0.5,0.5,0.5))
points(alld,allN,pch=16,cex=0.5,col=pal2tone[1])
lines(c(mean(alld),mean(alld)),c(0,100),lty=2,lwd=2)
title(xlab="Effect size (d)", col.lab=rgb(0,0,0), line=2.2, cex.lab=1.5)    # titles for axes
title(ylab="Sample size (N)", col.lab=rgb(0,0,0), line=2.2, cex.lab=1.5)


alld[1:nstudies] <- trued
allN <- NULL
for (n in 1:nstudies){
  allN[n] <- 3+round(50*abs(rnorm(1)))
  while (alld[n]<=trued){
    alld[n] <- mean(rnorm(allN[n],mean=trued,sd=truesd))
  }
}
plot(c(trued,trued),c(0,100),type='l',lwd=2,xlim=c(0,2),ylim=c(0,100),ann=FALSE,col=rgb(0.5,0.5,0.5))
points(alld,allN,pch=16,cex=0.5,col=pal2tone[1])
lines(c(mean(alld),mean(alld)),c(0,100),lty=2,lwd=2)
title(xlab="Effect size (d)", col.lab=rgb(0,0,0), line=2.2, cex.lab=1.5)    # titles for axes
title(ylab="Sample size (N)", col.lab=rgb(0,0,0), line=2.2, cex.lab=1.5)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)




figno <- figno + 1
if(outputplot==1){postscript(paste('figures/Figure ',chapter,'.',figno,'.eps',sep=''), horizontal = FALSE, onefile = FALSE, paper = "special", height = 5, width = 9)}
if(outputplot==2){pdf(paste('figures/Figure ',chapter,'.',figno,'.pdf',sep=''), bg="transparent", height = 5, width = 9)}

effectsizes <- c(0.7, 0.4, 2.1, 0.9, 1.6)
standarderrors <- c(0.2, 0.3, 0.9, 0.3, 0.5)

metaoutput <- meta.summaries(effectsizes,standarderrors,method='random')

# this line of code tells R to put the next two plots side by side
par(mfrow=c(1,2), las=1)  

metaplot(effectsizes,standarderrors,summn=metaoutput$summary,
         sumse=metaoutput$se.summary,sumnn= metaoutput$se.summary^-2,
         xlab='Effect size (d)',ylab="Study",summlabel='')

funnelplot(metaoutput, plot.conf=TRUE)

if(outputplot>0){dev.off()}  # this line goes after you've finished plotting (to output the example below, move it to the bottom of the script)
