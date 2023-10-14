rawdata = read.csv("psam_p28.csv", head = TRUE)
# Government Imputation
rawdataG = rawdata[-which(rawdata$AGEP<15),]
mu.gov = sum(rawdataG$PWGTP*rawdataG$INTP)/sum(rawdataG$PWGTP)
rawdataAl = rawdata
# Delete all rows with AGEP <15
rawdataAl = rawdataAl[-which(rawdataAl$AGEP<15),]
# Finding all the Allocation Flag variables
Flagname = c()
for (i in (grep("FAGEP",colnames(rawdataAl)):grep("FYOEP",colnames(rawdataAl)))) {
  Flagname[i+1-grep("FAGEP",colnames(rawdataAl))] = unlist(strsplit(colnames(rawdataAl[i]),split = "F",fixed = T))[2]
}
Flagname[17] = "FERP"
Flagname[18] = "FODP"
Flagname1 = data.frame(Rawname = rep(NA,length(Flagname)),Flagname = rep(NA,length(Flagname)),Rawnum = rep(0,length(Flagname)),Flagnum = c(grep("FAGEP",colnames(rawdataAl)):grep("FYOEP",colnames(rawdataAl))))
for (i in 1:length(Flagname)) {
  a = grep(Flagname[i],colnames(rawdataAl),value = T,fixed=T)
  b = grep(Flagname[i],colnames(rawdataAl),fixed=T)
  if(length(a) == 2){
    Flagname1$Rawname[i] = a[1]
    Flagname1$Flagname[i] = a[2]
    Flagname1$Rawnum[i] = b[1]
  }else if(length(a) == 1){
    Flagname1$Rawname[i] = NA
    Flagname1$Flagname[i] = a[1]
    print(isTRUE(b[1]==Flagname1$Flagnum[i]))
  }else{print(a)}
}
# 1) Finding the corresponding original variables for allocation flag variables 
#    according to the dictionary;
# 2) Regard variable FHINS3C,FHINS4C,FHINS5C and FMILPP and FFODP as Catogorical Variable;
Flagname1[which(Flagname1$Flagname=="FANCP"),1]="ANC"
Flagname1[which(Flagname1$Flagname=="FCITP"),1]="CIT"
Flagname1[which(Flagname1$Flagname=="FCOWP"),1]="COW"
Flagname1[which(Flagname1$Flagname=="FDDRSP"),1]="DDRS"
Flagname1[which(Flagname1$Flagname=="FDEARP"),1]="DEAR"
Flagname1[which(Flagname1$Flagname=="FDEYEP"),1]="DEYE"
Flagname1[which(Flagname1$Flagname=="FDISP"),1]="DIS"
Flagname1[which(Flagname1$Flagname=="FDOUTP"),1]="DOUT"
Flagname1[which(Flagname1$Flagname=="FDPHYP"),1]="DPHY"
Flagname1[which(Flagname1$Flagname=="FDRATP"),1]="DRAT"
Flagname1[which(Flagname1$Flagname=="FDRATXP"),1]="DRATX"
Flagname1[which(Flagname1$Flagname=="FDREMP"),1]="DREM"
Flagname1[which(Flagname1$Flagname=="FESRP"),1]="ESR"
Flagname1[which(Flagname1$Flagname=="FFERP"),1]="FER"
Flagname1[which(Flagname1$Flagname=="FGCLP"),1]="GCL"
Flagname1[which(Flagname1$Flagname=="FGCMP"),1]="GCM"
Flagname1[which(Flagname1$Flagname=="FGCRP"),1]="GCR"
Flagname1[which(Flagname1$Flagname=="FHICOVP"),1]="HICOV"
Flagname1[which(Flagname1$Flagname=="FHINS1P"),1]="HINS1"
Flagname1[which(Flagname1$Flagname=="FHINS2P"),1]="HINS2"
Flagname1[which(Flagname1$Flagname=="FHINS3P"),1]="HINS3"
Flagname1[which(Flagname1$Flagname=="FHINS4P"),1]="HINS4"
Flagname1[which(Flagname1$Flagname=="FHINS5P"),1]="HINS5"
Flagname1[which(Flagname1$Flagname=="FHINS6P"),1]="HINS6"
Flagname1[which(Flagname1$Flagname=="FHINS7P"),1]="HINS7"
Flagname1[which(Flagname1$Flagname=="FJWTRP"),1]="JWTR"
Flagname1[which(Flagname1$Flagname=="FLANXP"),1]="LANX"
Flagname1[which(Flagname1$Flagname=="FMARP"),1]="MAR"
Flagname1[which(Flagname1$Flagname=="FMARHDP"),1]="MARHD"
Flagname1[which(Flagname1$Flagname=="FMARHMP"),1]="MARHM"
Flagname1[which(Flagname1$Flagname=="FMARHTP"),1]="MARHT"
Flagname1[which(Flagname1$Flagname=="FMARHWP"),1]="MARHW"
Flagname1[which(Flagname1$Flagname=="FMILSP"),1]="MIL"
Flagname1[which(Flagname1$Flagname=="FPRIVCOVP"),1]="PRIVCOV"
Flagname1[which(Flagname1$Flagname=="FPUBCOVP"),1]="PUBCOV"
Flagname1[which(Flagname1$Flagname=="FSCHGP"),1]="SCHG"
Flagname1[which(Flagname1$Flagname=="FSCHLP"),1]="SCHL"
Flagname1[which(Flagname1$Flagname=="FSCHP"),1]="SCH"
Flagname1[which(Flagname1$Flagname=="FSEXP"),1]="SEX"
Flagname1[which(Flagname1$Flagname=="FWKLP"),1]="WKL"
Flagname1[which(Flagname1$Flagname=="FWKWP"),1]="WKW"
Flagname1[which(Flagname1$Flagname=="FWRKP"),1]="WRK"
for (i in 1:nrow(Flagname1)) {
  if(i %in% c(18,25,27,29,50)){next}
  if(Flagname1$Rawname[i] %in% colnames(rawdataAl)){Flagname1$Rawnum[i]= which(colnames(rawdataAl)==Flagname1$Rawname[i])}
}
# If Flag variable = 1, set corresponding variable = NA
# and keep the original NA values unchanged.
for (i in 1:nrow(Flagname1)){
  if(i %in% c(18,25,27,29,50)){next}
  for (j in 1:nrow(rawdataAl)) {
    if(rawdataAl[j,Flagname1$Flagnum[i]]==1){
      rawdataAl[j,Flagname1$Rawnum[i]] = NA
    }
  }
}

# Prepare datafile for GUIDE
dat = rawdataAl
dat$INTP_ = ifelse(is.na(dat$INTP), 0, 1)
dat0 = dat
dat = dat[,-c(97,128)]#"NAICSP""SOCP"???
write.table(dat, file = "model.txt",row.names=F)

# Creating GUIDE description file for classification in R
k = ncol(dat)
roles = rep("n",k)
b.vars = c("CIT","COW","DDRS","DEAR","DEYE","DOUT","DPHY","DRAT","DRATX","DREM","ENG","FER","GCL","GCM","GCR","HINS1","HINS2","HINS3","HINS4","HINS5","HINS6","HINS7",
           "JWTR","LANX","MAR","MARHD","MARHM","MARHT","MARHW","MIG","MIL","MLPA","MLPB","MLPCD","MLPE","MLPFG","MLPH","MLPI","MLPJ","MLPK","NWAB","NWAV","NWLA","NWLK",
           "NWRE","RELP","SCH","SCHG","SCHL","SEX","WKL","WKW","WRK","ANC","ANC1P","ANC2P","DECADE","DIS","DRIVESP","ESP","ESR","FOD1P","FOD2P","HICOV","HISP","INDP",
           "LANP","MIGPUMA","MIGSP","MSP","NATIVITY","NOP","OC","OCCP","PAOC","POBP","POWPUMA","POWSP","PRIVCOV","PUBCOV","QTRBIR","RAC1P","RAC2P","RAC3P",
           "RACAIAN","RACASN","RACBLK","RACNH","RACNUM","RACPI","RACSOR","RACWHT","RC","SCIENGP","SCIENGRLP","SFN","SFR","VPS","WAOB","FFODP","FHINS3C","FHINS4C","FHINS5C",
           "FMILPP") # Catogorial variables.,"NAICSP","SOCP"
roles[names(dat) %in% b.vars] = "b"
n.vars = c("AGEP","CITWP","JWMNP","JWRIP","MARHYP","OIP","PAP","RETP","SEMP","SSIP","SSP","WAGP","WKHP","YOEP","PERNP","PINCP","POVPIP") # Numeric variables
roles[names(dat) %in% n.vars] = "n"
p.vars1 = "JWAP"
p.vars2 = "JWDP"
#add 286 151
roles[names(dat) %in% p.vars1] = "p 286"
roles[names(dat) %in% p.vars2] = "p 151"
# Set Flags as excluded vairables
# Set PWGTP1-80 as excluded vairables
i = c(1:80)
PWGTPX = paste("PWGTP",i,sep = "")
x.vars = c("SPORDER","PUMA","RT","SERIALNO","DIVISION","REGION","ST","ADJINC","INTP",Flagname1[-c(18,25,27,29,50),][,2],PWGTPX) # Excluded variables
roles[names(dat) %in% x.vars] = "x"
d.var = "INTP_"
roles[names(dat) %in% d.var] = "d"
w.var = "PWGTP"
roles[names(dat) %in% w.var] = "w"
write("model.txt",file="desc.txt") #represent the name of data file
write("NA",file="desc.txt",append=TRUE) #add NA line in the desc file
write("2",file="desc.txt",append=TRUE) #add NA line in the desc file
write.table(cbind(1:k,names(dat),roles),file="desc.txt",
            row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
# Exclude ID, State, PWGTP1-80, Over-levelled catogorical variables, periodic variable
exclude1 = c("SPORDER","PUMA","RT","SERIALNO","DIVISION","REGION","ST","ADJINC")
i = c(1:80)
exclude2 = paste("PWGTP",i,sep = "")
exclude3 = c("INDP","MIGPUMA","MIGSP","OCCP","POBP")
exclude = c(exclude1,exclude2,exclude3)
datR = dat[,-which(names(dat) %in% exclude)]
# Replace NAs with constants and add missing value indicator variables
for (i in 1:ncol(datR)) {
  index = which(is.na(datR[,i]))
  datR[index,i]=min(datR[,i],na.rm = T)-1
}
a=lm(datR$INTP~datR$FOD1P+datR$FOD2P+datR$ANC1P+datR$ANC2P+datR$VPS+datR$LANP+datR$DRAT+datR$DECADE+datR$HINS2+datR$FHINS3C+datR$HINS5+datR$HINS6+datR$HINS7+datR$MLPFG+datR$MARHYP+datR$HINS4+datR$MLPE+datR$SCIENGP+datR$RETP+datR$SSP+datR$MLPH+datR$MLPJ+datR$SCIENGP+datR$PINCP+datR$DRATX+datR$PRIVCOV+datR$PUBCOV+datR$MLPCD+datR$MARHW+datR$OIP+datR$POVPIP+datR$SEMP+datR$MLPI+datR$HINS3+datR$AGEP+datR$HICOV+datR$MLPK+datR$HINS1+datR$MLPA+datR$MLPB+datR$RACBLK+datR$RACWHT+datR$COW+datR$MIL+datR$NWAV+datR$MARHT+datR$MSP+datR$CITWP+datR$MAR+datR$ANC+datR$FFODP+datR$MARHM+datR$RACASN+datR$WKL+datR$DIS+datR$PERNP+datR$WKHP+datR$FMILPP+datR$RACNH+datR$QTRBIR+datR$WAGP+datR$JWAP+datR$JWDP+datR$JWTR+datR$ESP+datR$ESR+datR$SFR+datR$RACNUM+datR$GCM+datR$DRIVESP+datR$CIT+datR$RELP+datR$SEX+datR$RAC1P+datR$RACPI+datR$ENG+datR$MIG+datR$SFN+datR$FHINS5C+datR$RACAIAN+datR$NWRE+datR$RACSOR+datR$DEAR+datR$OC+datR$SCH+datR$GCR+datR$DEYE+datR$DOUT+datR$LANX+datR$NWAB+datR$DREM+datR$DDRS+datR$NWLA+datR$DPHY+datR$PAP+datR$SSIP+datR$YOEP+datR$RC+datR$NWLK+datR$MARHD+datR$FER+datR$WKW+datR$JWRIP+datR$NATIVITY+datR$PAOC+datR$FHINS4C+datR$GCL+datR$WRK+datR$JWMNP)
summary(a)
z0 <- read.table("imp.scr",header=TRUE) 
par(mar=c(5,6,2,1),las=1) 
barplot(z0$Score[1:30],names.arg=z0$Variable[1:30],col="cyan",horiz=TRUE,xlab="Importance scores") 
abline(v=1,col="red",lty=2)
v.score = as.character(z0$Variable[1:100])
v = c(v.score,"INTP_")
datLogistic = datR[,which(names(datR) %in% v)]
for(i in which(names(datLogistic) %in% v)){
  datLogistic[,i] = as.factor(datLogistic[,i])
}
model = glm(INTP_ ~.,
           data=datLogistic,
          family = binomial(link = "logit")
)
summary(model)
 
pifile=read.table("MS.FIT",header = T)
regressionfile=read.table("regression.fit",header = T)
regression=regressionfile$predicted[regressionfile$train=="n"]
na=0
nonna=0
pwgtpna=dat$PWGTP[is.na(dat$INTP)]
pwgtpnonna=dat$PWGTP[is.na(dat$INTP)==F]
intpnonna=dat$INTP[is.na(dat$INTP)==F]
for (i in 1:3800) {
  na=na+regression[i]*pwgtpna[i]
}
for (i in 1:20238) {
  nonna=nonna+intpnonna[i]*pwgtpnonna[i]
}
uregression=(na+nonna)/sum(dat$PWGTP)
pi=pifile$X1/(pifile$X1+pifile$X0)
divide=0
multi=0
for (i in which(is.na(dat$INTP)==F)) {
  divide=divide+datR$PWGTP[i]/pi[i]
  multi=multi+(datR$PWGTP[i]*datR$INTP[i])/pi[i]
}
uclass=multi/divide