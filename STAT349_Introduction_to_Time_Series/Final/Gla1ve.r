library(tseries)
library(timeSeries)
library(TSA)
library(Rdonlp2)
GARCH.Finding=function(x){
  SSE=matrix(NA,nrow = 4,ncol = 3)
  k=1
  for (i in 1:2) {
    for (j in 1:2) {
      l=sum(summary(garch(x,order = c(i,j),trace=F))$res^2)
      SSE[k,]=c(l,i,j)
      k=k+1
    }
  }
  return(SSE)
}
GARCH.Fitting=function(x){
  y=garch(x,order=GARCH.Finding(x)[which.min(GARCH.Finding(x)[,1]),c(2,3)],trace=F)
  return(summary(y)$coef)
}
GARCH.Model=function(x){
  return(garch(x,order=GARCH.Finding(x)[which.min(GARCH.Finding(x)[,1]),c(2,3)],trace=F))
}