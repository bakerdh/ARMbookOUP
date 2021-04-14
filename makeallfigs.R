# script to auto-generate all figures from the ARM book in EPS format
# all are saved to the figures directory with correct numbering

outputplot <- 1  # 0 draws to the Plots window, 1 exports as eps, 2 exports as pdf
nsims <- 100  # global value for any stochastic simulations - increase to 100000 for final run
# on my laptop, this takes around 3 minutes with 100 simulations, 6 minutes with 1000 simulations
# 40 minutes with 10000 simulations and ~6 hours with 100000 simulations

# in addition, the first time the Chapter 13 figures are created will take an extra ~3 minutes
# to do a large principle components analysis and save the results

# compact code to install and activate all the packages required to generate the figures
packagelist <- c('pwr','rmeta','MAd','compute.es','lme4','lmerTest','MuMIn','knitr','Hotelling','tictoc','MASS','jpeg','amap','optimbase','optimsimplex','neldermead','signal','pracma','lavaan','semPlot','caret','kernlab','e1071','graphics','RSNNS','psyphy','quickpsy','BayesFactor','pals','colorspace','grImport','PRISMAstatement','rsvg','DiagrammeRsvg','png','data.table','devtools','corrplot','DiagrammeR')
missingpackages <- packagelist[!packagelist %in% installed.packages()[,1]]
if (length(missingpackages)>0){install.packages(missingpackages)}
toinstall <- packagelist[which(!packagelist %in% (.packages()))]
invisible(lapply(toinstall,library,character.only=TRUE))

# install the FourierStats package from github using the devtools package
packagelist <- c('FourierStats')
missingpackages <- packagelist[!packagelist %in% installed.packages()[,1]]
if (length(missingpackages)>0){devtools::install_github("bakerdh/FourierStats")}
toinstall <- packagelist[which(!packagelist %in% (.packages()))]
invisible(lapply(toinstall,library,character.only=TRUE))

# helper functions that produce alpha transparency
addalpha <- function(col, alpha=1){apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=alpha))}
flatalpha <- function(col, alpha=1){apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1]*alpha + 1-alpha, x[2]*alpha + 1-alpha, x[3]*alpha + 1-alpha))}

# palette of colours used in the figures, based on Pantone blue 072
pal2tone <- c('#8783CF','#10069F','#CFCDEC')  # blue 072

tic()  # start a timer to see how long this takes

if(!dir.exists('figures/')){dir.create('figures/')} # make a directory to store the figures

# create figures for each chapter, one at a time
source('Chapter3figs.R')
source('Chapter4figs.R')
source('Chapter5figs.R')
source('Chapter6figs.R')
source('Chapter7figs.R')
source('Chapter8figs.R')
source('Chapter9figs.R')
source('Chapter10figs.R')
source('Chapter11figs.R')
source('Chapter12figs.R')
source('Chapter13figs.R')
source('Chapter14figs.R')
source('Chapter15figs.R')
source('Chapter16figs.R')
source('Chapter17figs.R')
source('Chapter18figs.R')

toc()  # report how long it took to make the figures



