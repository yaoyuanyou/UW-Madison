#####################################################################
# NAME:  Derek Bean                                                 #
# DATE:  4-1-120                                                    #
# PURPOSE: Inference, goodness of fit, model checking for GLMs      #
#                                                                   #
# NOTES: This is code for basic inference procedures and checking   #
# goodness of fit and residuals for GLMs. The data  is the horshoe  #
# crab data. (http://users.stat.ufl.edu/~aa/intro-cda/appendix.html) #
#####################################################################


#Load the data from the textfile

crab = read.table("HorseshoeCrabs.txt") #Make sure working directory is set to the directory containing the file!
crab #Check data read properly

color = crab[,1]
spine = crab[,2]
width = crab[,3]
satell = crab[,4]
weight = crab[,5]

#Fit a Poisson GLM with width only and inspect it

model.Poi = glm(satell~width, family = poisson(link = log))
summary(model.Poi)

#Wald type methods
alpha = 0.05

width.estimate = model.Poi$coef[2]

width.var = vcov(model.Poi)[2,2] #calculate variance-covariance matrix of coefficients. Variances on diagonal.
width.SE = sqrt(width.var) #extract estimated standard error of width coefficient

##Confidence interval

width.estimate-qnorm(1-alpha/2)*width.SE
width.estimate+qnorm(1-alpha/2)*width.SE #limits of 1-alpha CI

##Test statistics
z = width.estimate/width.SE

2*pnorm(abs(z), lower.tail=F)
pchisq(z^2, 1, lower.tail=F) #two ways of getting two-sided p-value for test of significance of width



#Likelihood ratio methods

resid.deviance = model.Poi$deviance
null.deviance = model.Poi$null.deviance #get residual and null deviances

df.residual = model.Poi$df.residual
df.null = model.Poi$df.null

lik.ratio = null.deviance-resid.deviance #likelihood ratio-based statistic for testing width

pchisq(lik.ratio, df.null-df.residual, lower.tail=F) #extract p-value

confint(model.Poi, level = 1-alpha) #confidence intervals based on likelihood ratio. Slightly different from Wald intervals!





#Model comparison using deviance

##Fit a model with width and weight

model.Poi.1 = update(model.Poi, .~. + weight) #what happens when you control for weight too?
summary(model.Poi.1)

anova(model.Poi, model.Poi.1)

resid.deviance.1 = model.Poi.1$deviance
df.residual.1 = model.Poi.1$df.residual

deviance = resid.deviance-resid.deviance.1
deviance #check

df.diff = df.residual - df.residual.1

pchisq(deviance, df.diff, lower.tail=F) #weight effect significant!


#Obtain Pearson and deviance residuals
residuals.pearson = residuals(model.Poi, "pearson")
residuals.deviance = residuals(model.Poi, "deviance") #deviance residuals output by default

#Obtain standardized Pearson and deviance residuals
hi = lm.influence(model.Poi)$hat #obtain the leverage values

residuals.pearson.st = residuals.pearson/sqrt(1-hi)
residuals.deviance.st = residuals.deviance/sqrt(1-hi)

qqnorm(residuals.pearson.st) #Do the residuals look like a sample from the standard normal? (Bear in mind they are not independent--we may want to *studentize* them!

summary(residuals.pearson.st) #Residuals look significantly right-skewed compared to a standard normal.





