setwd("/Users/angywong/Downloads/csv_pms")
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

# Exclude ID, State, PWGTP1-80, Over-levelled catogorical variables, periodic variable
exclude1 = c("SPORDER","PUMA","RT","SERIALNO","DIVISION","REGION","ST","ADJINC")
i = c(1:80)
exclude2 = paste("PWGTP",i,sep = "")
exclude3 = c("INDP","MIGPUMA","MIGSP","OCCP","POBP")
exclude4= c("LANP","ANC1P","FOD1P","FOD2P","ANC2P","JWAP","JWDP")
exclude = c(exclude1,exclude2,exclude3,exclude4)
datR = dat[,-which(names(dat) %in% exclude)]
# Replace NAs with constants and add missing value indicator variables
for (i in 1:ncol(datR)) {
  index = which(is.na(datR[,i]))
  datR[index,i]=min(datR[,i],na.rm = T)-1
}

# factor
v.factor = c("CIT","COW","DDRS","DEAR","DEYE","DOUT","DPHY","DRAT","DRATX","DREM","ENG","FER","GCL","GCM","GCR","HINS1","HINS2","HINS3","HINS4","HINS5","HINS6","HINS7",
             "JWTR","LANX","MAR","MARHD","MARHM","MARHT","MARHW","MIG","MIL","MLPA","MLPB","MLPCD","MLPE","MLPFG","MLPH","MLPI","MLPJ","MLPK","NWAB","NWAV","NWLA","NWLK",
             "NWRE","RELP","SCH","SCHG","SCHL","SEX","WKL","WKW","WRK","ANC","ANC1P","ANC2P","DECADE","DIS","DRIVESP","ESP","ESR","FOD1P","FOD2P","HICOV","HISP","INDP",
             "LANP","MIGPUMA","MIGSP","MSP","NATIVITY","NOP","OC","OCCP","PAOC","POBP","POWPUMA","POWSP","PRIVCOV","PUBCOV","QTRBIR","RAC1P","RAC2P","RAC3P",
             "RACAIAN","RACASN","RACBLK","RACNH","RACNUM","RACPI","RACSOR","RACWHT","RC","SCIENGP","SCIENGRLP","SFN","SFR","VPS","WAOB","FFODP","FHINS3C","FHINS4C","FHINS5C",
             "FMILPP")
v.factor = v.factor[-which(v.factor %in% exclude3)]
v.factor = c(Flagname1[-c(18,25,27,29,50),][,2],v.factor)

z0 = read.table("imp.scr",header=TRUE) 
v.score = as.character(z0$Variable[1:20])
(v = c(v.score,"INTP_"))
datLogistic = datR[,which(names(datR) %in% v)]
for(i in which(names(datLogistic) %in% v.factor)){
  datLogistic[,i] = as.factor(datLogistic[,i])
}
model = glm(INTP_ ~.,
            data=datLogistic,
            family = binomial(link = "logit")
)
sink("resultLogistic20.txt")
summary(model)
# Mu calculation
datLogistic$pi = predict(model,type='response')
datLogistic$PWGTP = dat$PWGTP
datLogistic$INTP = dat$INTP
# Add the result to summary
sum(datLogistic$pi)
x1 = sum(datLogistic$PWGTP[-which(is.na(datLogistic$INTP))]*datLogistic$INTP[-which(is.na(datLogistic$INTP))]/datLogistic$pi[-which(is.na(datLogistic$INTP))])
x2 = sum(datLogistic$PWGTP[-which(is.na(datLogistic$INTP))]/datLogistic$pi[-which(is.na(datLogistic$INTP))])
x1/x2
# Accuracy
datLogistic$predict = ifelse(datLogistic$pi>0.5,1,0)
accuracy = sum(datLogistic$predict==datLogistic$INTP_)/nrow(datLogistic)
accuracy

