rm(list=ls())
#Yuanyou Yao yyao93@wisc.edu
library("FITSio")
path="./data"
fileNames=dir(path)
filePath=sapply(fileNames,function(x){paste(path,x,sep='/')})
data=lapply(filePath,function(x){readFrameFromFITS(x)})
cB58=readFrameFromFITS("cB58_Lyman_break.fit")
cB58_new=  (cB58[,2] - mean(cB58[,2]))/sd(cB58[,2])
Euclidean_distance = vector()
e = vector()
f= vector()
output = matrix(,nrow = 100, ncol = 3)
for (i in 1:length(data)) {
  for (j in 1:(dim(data[[i]])[1]-2180)) {
    data_new=data[[i]][j:(j+2180),1]
    data_new = data_new - mean(data_new)
    data_new = data_new/sd(data_new)
    e[j]= sqrt(sum((data_new-cB58_new)^2))
  }
  Euclidean_distance[i] = min(e)
  f[i] = which.min(e)
  output[i,] =c(Euclidean_distance[i],fileNames[i],f[i])
}
output= output[order(output[,1]),]
write.csv(output,'hw2.csv')
