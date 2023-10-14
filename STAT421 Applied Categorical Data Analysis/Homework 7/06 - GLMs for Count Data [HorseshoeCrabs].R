#####################################################################
# NAME:  Derek Bean                                                 #
# DATE:  3-25-20                                                    #
# PURPOSE: Fitting Poisson and negative binomial GLMs               #
#                                                                   #
# NOTES: This is code for fitting Poisson and negative binomial     #
# GLMs to the horseshoe crab data from Agresti. The data were       #
# pulled from Agresti's website at http://users.stat.ufl.edu/~aa/intro-cda/appendix.html #
# and stored in the file HorseshoeCrabs.txt.                        #
#####################################################################


#Load the data from the textfile

crab = read.table("HorseshoeCrabs.txt") #Make sure working directory is set to the directory containing the file!
crab #Check data read properly

color = crab[,1]
spine = crab[,2]
width = crab[,3]
satell = crab[,4]
weight = crab[,5]

#Set up a grid of shell widths to plot response curve later

grid = seq(from = min(width), to = max(width), length = 20)
plot(width, satell, xlab = "Width", ylab = "No. satellites")

#Fit a Poisson GLM with width only

model.Poi = glm(satell~width, family = poisson(link = log))
log.fit = model.Poi$coef[1] + model.Poi$coef[2]*grid
fit = exp(log.fit)
lines(grid, fit)

summary(model.Poi)

#Fit a Poisson GLM with width and weight

model.Poi.1 = update(model.Poi, .~. + weight) #what happens when you control for weight too?

summary(model.Poi.1)

anova(model.Poi, model.Poi.1)

#Compare with linear fit

model.lin = glm(satell~width, family = gaussian(link=identity))
lin.fit = model.lin$coef[1] + model.lin$coef[2]*grid
lines(grid, lin.fit, lty=2, col="red")

summary(model.lin)

#Fit a negative binomial model (THIS IS NOT COVERED IN DEPTH IN THE MAIN LECTURE NOTES,AND WILL NOT BE ON AN EXAM!)

library(MASS) #Note: need to load the mass library for this method

model.nb = glm.nb(satell~width)

summary(model.nb)

1/model.nb$theta #Check estimated dispersion parameter

log.nb.fit = model.nb$coef[1] + model.nb$coef[2]*grid
nb.fit = exp(log.nb.fit)
lines(grid,nb.fit,lty = 3, col = "blue")


#Add a legend

legend(x=31, y=14, legend = c("poisson", "gaussian", "neg. bin."), lty=c(1,2,3), col = c("black", "red", "blue"))




