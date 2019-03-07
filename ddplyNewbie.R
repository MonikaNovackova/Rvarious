rm(list=ls())
library(ggplot2);library(cvTools);library(MASS);library(plyr);library(dplyr); library(ggeffects);library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
load("dataFS/Main/DaTS.RData")
# load("Rcodes/DecemberNew/KEN11d_stepNice.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

ddply_Monika<-function(data,varname,groupnames)
{ require(plyr)
  summary_func <- function(x,col) { c(mean=mean(x[[col]],na.rm=TRUE ),sd=sd(x[[col]],na.rm=TRUE) ) }
  data_sum<-ddply(data, groupnames,.fun=summary_func,varname  )
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

test1<-ddply_Monika(DaTS,varname="SeasPr",groupnames="Year")
write.csv(test1, "writing/imagesCodes/experimental/test.csv" )

ddply(DaTS, groupnames,.fun=summary_func,.variables="SeasPr")

tapply(DaTS$SeasPr, DaTS$Year,mean)
aggregate(DaTS,by=list(DaTS$Year),FUN=mean)$SeasPr

t(aggregate(DaTS$SeasPr, FUN = mean, by=list(ScaledTS$Year)))
tapply(DaTS, )