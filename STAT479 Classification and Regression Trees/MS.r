## Function to make P variable values positive
convert <- function(x,plen){
  if(!is.na(x)){
    if(x < 0){
      while(x < 0){x <- x+plen}
    } else if(x >= plen){
      while(x >= plen){x <- x-plen}
    }
  }
  return(x)
}
## Function for predicting class labels
predicted <- function(){
 if(is.na(PAP)){
   if(is.na(SSP)){
     nodeid <- 4
     predict <- "0"
   } else {
     if(!is.na(RETP) & RETP <= 1050.00000000 ){
       nodeid <- 10
       predict <- "1"
     } else {
       nodeid <- 11
       predict <- "0"
     }
   }
 } else {
   if(is.na(PINCP)){
     if(is.na(PERNP)){
       nodeid <- 12
       predict <- "1"
     } else {
       if(is.na(SSP)){
         nodeid <- 26
         predict <- "1"
       } else {
         if(is.na(RETP)){
           nodeid <- 54
           predict <- "1"
         } else {
           if(is.na(SSIP)){
             nodeid <- 110
             predict <- "1"
           } else {
             nodeid <- 111
             predict <- "0"
           }
         }
       }
     }
   } else {
     nodeid <- 7
     predict <- "1"
   }
 }
 return(c(nodeid,predict))
}
## end of function
##
##
## Use training data to test function; change file name if needed
## Missing value code is NA
newdata <- read.table("model.txt",header=TRUE,colClasses="character")
## node contains terminal node ID of each case
## pred contains predicted value of each case
node <- NULL
pred <- NULL
for(i in 1:nrow(newdata)){
    PAP <- as.numeric(newdata$PAP[i])
    RETP <- as.numeric(newdata$RETP[i])
    SSIP <- as.numeric(newdata$SSIP[i])
    SSP <- as.numeric(newdata$SSP[i])
    PERNP <- as.numeric(newdata$PERNP[i])
    PINCP <- as.numeric(newdata$PINCP[i])
    tmp <- predicted()
    node <- c(node,tmp[1])
    pred <- c(pred,tmp[2])
}
