rm(list=ls())
library("FITSio")
path="./data"
fileNames=dir(path)
filePath=sapply(fileNames,function(x){paste(path,x,sep='/')})
data=lapply(filePath,function(x){readFrameFromFITS(x)})
cB58=readFrameFromFITS("cB58_Lyman_break.fit")
cB58_trough = cB58[1:351,]
cB58_local_min = which.min(cB58_trough[,2])
cB58_global = rank(cB58[,2])[1:10]
cB58_distance = cB58_global - cB58_local_min
data_new = NULL
for (i in 1:length(data)) {
data_new[[i]]=data[[i]][data[[i]][,4]==0,c(1,2)]
}
    data_new[[97]] = NULL
min_global =NULL
distance = NULL
for (i in 1:length(data_new)){
  for (j in 351: nrow(data_new[[i]])){
   distance[j-350]= cor(data_new[[i]][(j-350):j,1],cB58_trough[,2] )
  }
  min_global[[i]] = c(max(distance),which.max(distance)+350)
}
a=t(as.data.frame(min_global))
b=sort(a[,1],index.return =T)$ix[80:99]
data_new_distance = NULL
for(i in 1:length(b)){
  c =which.min(data_new[[b[i]]][a[b[i],2]:(a[b[i],2]+351),1])
  d = sort(data_new[[b[i]]][,1],index.return=T)$ix[1:10]
  data_new_distance[[i]] = d-c
}
data_new_distance[[8]] = NULL
e = NULL
for (i in 1:(length(b) - 1)) {
  e[i] = cor(data_new_distance[[i]],cB58_distance)
  
}
sort(e,decreasing = T,index.return = T)$ix
