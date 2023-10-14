library(car)
library(bestglm)
data=read.csv("24H_project2.csv",header = T,stringsAsFactors = F)
data$Status[data$Status=="Survived"]=1
data$Status[data$Status=="Perished"]=0
data$Status=as.numeric(data$Status)
table(data$Status)
prop.table(table(data$Status))
f <- as.formula(y ~ .*.)
y <- data$Status
x <- model.matrix(f, data[,-1])[, -1]
x=x[,-c(54,53,50,47,43,38,32,25,17,55,54,52,51,48,49,45,44,40,39,34,33,26,27,19,18,15,16,23,24,30,31,37,36,41,42,46,47,50)]
data_new=as.data.frame(cbind(y,x))
attach(data_new)
glm_1=glm(y~.,data = data_new,family = binomial("logit"))
summary(glm_1)
jpeg("glm_1.jpg")
par(mfrow=c(2,2), pty="s")
plot(glm_1)
dev.off()
cbind(resid(glm_1),rstandard(glm_1),rstudent(glm_1))
plot(glm_1, which=3)
plot(glm_1$fitted.values,sqrt(abs(rstandard(glm_1))), ylim=c(0,1.5))
plot(glm_1$fitted.values,rstandard(glm_1))
# check studentized residuals
jpeg("rstudent.jpg")
plot(glm_1$fitted.values,rstudent(glm_1), ylim=c(-4,4))
abline(h=c(-3,3), col="red")
alpha <- 0.05
t.critical <-qt(1-alpha/(2*n), n-p-1)# Bonferroni correction
abline(h=c(-t.critical, t.critical), col="green")
dev.off()
glm_1$fitted.values[max(rstudent(glm_1))]
# Identifying Influential Observations# 
dffits(glm_1)
plot(dffits(glm_1))
abline(h=1, col="red")
# Cook's distance
cooks.distance(glm_1)
plot(glm_1, which = 4)
jpeg("cook.jpg")
plot(cooks.distance(glm_1),ylim = c(0,1))
abline(h=1, col="red")
dev.off()
# DFBETAS
dfbetas(glm_1)
plot(dfbetas(glm_1)[,2])# DFBETAS_{1(i)}
abline(h=1, col="red")
data_new=data_new[-3,]
y=data_new$y
glm_1=glm(y~.,data = data_new,family = binomial("logit"))
step(glm_1,k=log(length(y)))#backward+BIC
step(glm_1)#backward+AIC
if(!require("pROC")) {
  install.packages("pROC")
  stopifnot(require("pROC"))}
glm_backward=glm(formula = y ~ TL + BH + HL + KL + `AG:AE` + `AG:BH` + `TL:AE` + 
                   `TL:WT`, family = binomial("logit"), data = data_new)
backward.pROC <-roc(y~ fitted(glm_backward))
jpeg("backward.jpg")
plot.roc(backward.pROC, legacy.axes=TRUE, print.auc=TRUE)
dev.off()
ci95 <-confint.default(glm_backward)
round(cbind(summary(glm_backward)$coeff, ci95), 3)
round(cbind(exp(glm_backward$coef),exp(ci95)), 3)
Anova(glm_backward, type="III")
attach(data_new)
glm0 <-glm(y~1, family=binomial("logit"))
step(glm0, scope =list(upper=glm_1), direction = "forward")#forward+AIC
step(glm0,scope = list(upper=glm_1),direction = "forward",k=log(length(y)))#forward+BIC
glm_forward_A=glm(formula = y ~ TL + HL + WT + KL+`AE:BH`, family = binomial("logit"), data = data_new)
glm_forward_B=glm(formula = y ~ TL + HL + WT + KL, family = binomial("logit"), data = data_new)
forward.pROC.A <-roc(y~ fitted(glm_forward_A))
forward.pROC.B <-roc(y~ fitted(glm_forward_B))
jpeg("forwardA.jpg")
plot.roc(forward.pROC.A, legacy.axes=TRUE, print.auc=TRUE)
dev.off()
jpeg("forwardB.jpg")
plot.roc(forward.pROC.B, legacy.axes=TRUE, print.auc=TRUE)
dev.off()
ci95 <-confint.default(glm_forward_A)
round(cbind(summary(glm_forward_A)$coeff, ci95), 3)
round(cbind(exp(glm_forward_A$coef),exp(ci95)), 3)
Anova(glm_forward_A, type="III")
model1.auc=vector()
model2.auc=vector()
for (i in 1:500) {
  p <- 0.75 # ratio btw train vs. valid, which you can decide
idx <-sample.int(n = nrow(data_new), size = floor(p*nrow(data_new)), replace = FALSE)
train_data <- data_new[idx,]
test_data <- data_new[-idx,]
#Fit the two models with train data.

model1.trained <- glm(formula = y ~ TL + HL + WT + KL +AE*BH, family = binomial("logit"), data=train_data)
model2.trained <- glm(formula = y ~ TL + BH + HL + KL + `AG:AE` + `AG:BH` + `TL:AE` + 
                        `TL:WT`, family = binomial("logit"), data=train_data) # the formula part must be different than model1.trained
#Predict your response with validation data.

model1.pred <- predict.glm(model1.trained, newdata = test_data, type="response")
model2.pred <- predict.glm(model2.trained, newdata = test_data, type="response")

#Compute the AUC and record it (because we will repeat this 500 times).

if (!require("pROC")) {
  install.packages("pROC")
  stopifnot(require("pROC"))
}
model1.auc[i] <- auc(roc(test_data$y, model1.pred))[[1]] 
model2.auc[i] <- auc(roc(test_data$y, model2.pred))[[1]] 
#Repeat a) to d) 500 hundred times and record your AUCs, and then average.
}
(model1.ave=mean(model1.auc))
(model2.ave=mean(model2.auc))
