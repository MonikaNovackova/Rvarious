rm(list=ls())
library(ggplot2);library(cvTools);library(MASS);library(plyr);library(dplyr); library(ggeffects);library(tseries); library(plm); library(nlme); library(lme4); library(lattice); library(car); library(lmerTest); library(optimx)
load("dataFS/Main/DaTS.RData")
# load("Rcodes/DecemberNew/KEN11d_stepNice.RData")
#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
tapply(DaTS$SeasPr, DaTS$Year, mean)
aggregate(DaTS$SeasPr,by=list(DaTS$ASAL,DaTS$Year),mean )
aggregate(DaTS$SeasPr,by=list(DaTS$Year),mean )

Tonka<-t(tapply(DaTS$SeasPr, list(DaTS$ASAL,DaTS$Year), mean))


write.csv(Tonka,"~/Rvarious/ddplyN2.csv")




ddplyM<-function(data,groupname, varname) {
  require(plyr)
  summaryFUNC<-function(x,col) { c(mean=mean(x[[col]],na.rm=TRUE),sd=sd(x[[col]],na.rm=TRUE))   }
    
  ddply_sum<-ddply(data, groupname,.fun=summaryFUNC,varname)
  ddply_sum<-rename(ddply_sum,c("mean"=varname))
  return(ddply_sum)
}

ddply_Monika<-function(data,varname,groupnames)
{ require(plyr)
  summary_func <- function(x,col) { c(mean=mean(x[[col]],na.rm=TRUE ),sd=sd(x[[col]],na.rm=TRUE) ) }
  data_sum<-ddply(data, groupnames,.fun=summary_func,varname  )
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}


exodus<-ddplyM(DaTS,"Year","SeasPr")


meanRM<-function(x) mean(x,na.rm=TRUE)

tonka2<-ddply(DaTS, .(Year), summarize,mean=mean(SeasPr,na.rm=TRUE))
#-----------------------------------------------------------------------------------------------------------------------------------------------------
.MySum<-function(x,y,z) {
  a= x+y  
  return (a+z)
}

.MySum(1,2,3)

  
.MySum2<-function(x,y,.fun) {
  require(plyr)
   innerF<-function(a,b,.fun) {.fun(a,b)}
  return (innerF)
}
.MySum2(1,2,.fun=sum)



myFun1<-function(a,b,funct){
  res<-funct(a,b)+3
  return(res-1)}

myFun1(2,2,sum)
  

#kkkkkkkkkkkkkkkkkkkkoooooooooooooooooooooeeeeeeeellllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllll
#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

funM<-function(.x,.y,.theFunct) {.theFunct(.x,.y)}

myFun1<-function(a,b){                          
  require(plyr)
  funct<-function(z,x) {z+x-3}
  someRes<-funM(a,b,.theFunct=funct)      
  return(someRes)}

myFun1(2,3)

# now will try without dplyr


funM2<-function(x,y,theFunct) {theFunct(x,y)}

myFun3<-function(a,b){                          
  funct<-function(z,x) {z+x-3}
  someRes<-funM2(a,b,theFunct=funct)      
  return(someRes)}

myFun3(80,10)

#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA





ddply_Monika<-function(data,varname,groupnames)
{ require(plyr)
  summary_func <- function(x,col) { c(mean=mean(x[[col]],na.rm=TRUE ),sd=sd(x[[col]],na.rm=TRUE) ) }
  data_sum<-ddply(data, groupnames,.fun=summary_func,varname  )
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------

write.csv(Tonka,"~/Rvarious/ddplyN2.csv")