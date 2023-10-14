dat=read.csv("Data_24h_project.csv")
dat
#delete 4 and 20 line due to the question(Lancaster and York)
dat1=dat[c(1:3,5:19,21:60), ]
modelf=lm(Mort~Precip+Educ+NonWhite+NOX+SO2,data=dat1)
summary(modelf)

anova(lm(Mort~Precip+Educ+NonWhite,data = dat1),lm(Mort~Precip+Educ+NonWhite+NOX+SO2,data = dat1))

#check for multicollinearity
require(car)
vif(modelf)
# result shows no ...

#model selection.
#due to there is only 5 varibles, we can get through all subsets to 
#find the best model

if (!require("leaps")) {
  install.packages("leaps")
  stopifnot(require("leaps"))
}

myleaps <- regsubsets(Mort~Precip+Educ+NonWhite+NOX+SO2,data=dat1, nbest=8)
(myleaps.summary <- summary(myleaps)) # hard to interpret

# A better view:
bettertable <- cbind(myleaps.summary$which,
                     myleaps.summary$rsq, myleaps.summary$rss,
                     myleaps.summary$adjr2, myleaps.summary$cp, myleaps.summary$bic)
dimnames(bettertable)[[2]] <- c(dimnames(myleaps.summary$which)[[2]],
                                "rsq", "rss", "adjr2", "cp", "bic")
show(bettertable)
z#we use the smallest BIC to pick the best model:Mort~Precip+Educ+NonWhite+SO2

par(mfrow=c(1,3), pty="s")
plot(myleaps, scale = "adjr2")
plot(myleaps, scale = "Cp")
plot(myleaps, scale = "bic")
#all shows the same result.

#outliers,normality,equal variance
model=lm(Mort~Precip+Educ+NonWhite+SO2,data = dat1)
summary(model)
par(mfrow=c(2,2))
plot(model,which = 1:4)
dev.off()
plot(model,which=3)
#the first plot shows equal variance.
#the second plot shows normality assumption holds
#the third plot shows an outlier 7(Miami)(because r>3)
#the fourth plot 

# check studentized residuals
plot(model$fitted.values, rstudent(model))
plot(model$fitted.values, rstudent(model), ylim=c(-4,4))
abline(h=c(-3,3), col="red") # rule of thumb

p=5
n=58
plot(model, which = 4)
abline(h=qf(0.5, p, n-p), col="green")
abline(h=4/n, col="blue")
abline(h=4/(n-p-1-1), col="orange")
#7(Miami) and 60(New orleans) are influential points.

dat2=dat1[dat1$City!="Miami, FL" & dat1$City!="New Orleans, LA", ]
modelnew=lm(Mort~Precip+Educ+NonWhite+SO2,data = dat2)
summary(modelnew)
summary(model)

library(MASS)
boxcox(modelnew,seq(-4,4,1/10))
y=dat2$Mort
x1=dat2$Precip
x2=dat2$Educ
x3=dat2$NonWhite
x4=dat2$SO2
bc=boxcox(y~x1+x2+x3+x4,lambda = seq(-4,4,1/10))
(lambda <- bc$x[which.max(bc$y)])

model4=lm(y^3~x1+x2+x3+x4)
summary(model4)

if (!require("DAAG")) {
  install.packages("DAAG")
  stopifnot(require("DAAG"))
}
cv.lm(data = dat2, form.lm = formula(Mort~Precip+Educ+NonWhite+SO2), m=8)
cv.lm(data = dat2, form.lm = formula(Mort^3~Precip+Educ+NonWhite+SO2), m=8)
