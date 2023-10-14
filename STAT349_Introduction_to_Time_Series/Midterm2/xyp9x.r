library(TSA)
library(timeSeries)
newdata=function(a){
  c=timeSeries(a[,7],charvec = rownames(a))
  total=length(c)
  b.new=c[-total:-(total-4)]
  return(b.new)
}
tsdatadata=function(a){
  return(timeSeries(a[,7],charvec = rownames(a)))}
testdata=function(a){
  c=timeSeries(a[,7],charvec = rownames(a))
  total=length(c)
  b.test=c[(total-4):total]
  return(b.test)
}
totalobs=function(a){
  return(nrow(a))
}
auto.selection=function(c.new,c.test,total){
  i=1
  m=matrix(nrow = 32,ncol=4)
  for (p in 0:3) {
    for (d in 0:1) {
      for (q in 0:3) {
        model.1=arima(c.new,order = c(p,d,q))
        m[i,1]=p
        m[i,2]=d
        m[i,3]=q
        
        m[i,4]=mean((c.test-predict(model.1,n.ahead=5)$pred)^2)
        i=i+1
      }
      
    }
    
  }
  return(m[order(m[,4])[1],])
}