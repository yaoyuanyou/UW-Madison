#####################################################################
# NAME:  Derek Bean                                                 #
# DATE:  3-24-20                                                    #
# PURPOSE: Fitting linear probability, logistic, and probit models  #
# and extracting fitted values from them.                           #
#                                                                   #
# NOTES: We produce a figure (Figure 3.1 in Agresti) comparing the  #
# estimated probabilities of heart disease from three binary GLMs.  #
#####################################################################


#Enter in the data
##Grouped: treat the data as binomial counts at each snoring level
yes = c(24, 35, 21, 30)
no = c(1355, 603, 192, 224)

total = yes + no

xlevs = c(0, 2, 4, 5) #scores assigned to snoring levels

#Plot the observed proportions

plot(xlevs, yes/total, xlab = "Snoring Level", ylab = "Estimated Probability of Heart Disease") #Looks linear!


##Ungrouped: create vectors of response and explanatory variables containing all 2484 subjects separately

x = c(rep.int(xlevs[1],total[1]), rep.int(xlevs[2], total[2]), rep.int(xlevs[3], total[3]), rep.int(xlevs[4], total[4]))

y = c(rep.int(1, yes[1]), rep.int(0, no[1]), rep.int(1, yes[2]), rep.int(0, no[2]), rep.int(1, yes[3]), rep.int(0, no[3]), rep.int(1, yes[4]), rep.int(0, no[4]))




#Fit linear probability model
##Ungrouped approach--fit each subject separately
lin.model = glm(y~x, family = gaussian)
lin.model$coef #Note: fit done by least squares, not MLE. Alternatively, we could use the lm function to fit.
abline(lin.model$coef) #Add to plot

summary(lin.model) #Note the output of standard errors and t statistics for coefficients. Slope is highly significant.

##Try grouped approach!
lin.model.grouped = glm(yes/total~xlevs, weights = total, family = gaussian) #Regress the sample proportions on snoring levels; it's important to weight by the group size!
summary(lin.model.grouped) #Parameter estimates and standard errors unchanged but notice change in deviance statistic: more on this later!



#Create a grid of x values to plot pi(x) for other two models
grid = seq(from=0, to = 5, length = 20)



#Fit logistic model
##Ungrouped approach
log.model = glm(y~x, family = binomial(link = logit))

logits = log.model$coef[1] + log.model$coef[2]*grid
log.probs = exp(logits)/(1+exp(logits))
lines(grid, log.probs, lty=2, col = "red")

summary(log.model) #Note standard errors and p-values here too. We will explore these and other aspects of the model summary later.

##Grouped approach
log.model.group = glm(yes/total~xlevs, weights = total, family = binomial(link = logit)) #grouped approach: regress the proportion of successes on levels of x, weighting by the number of trials
summary(log.model.group) #no change in parameter estimates from ungrouped approach

log.model.group.1 = glm(cbind(yes, no)~xlevs, family = binomial(link = logit)) #Another alternative to group the data: regress successes and failures on x
summary(log.model.group.1)


#Fit probit model
##Ungrouped approach
prob.model = glm(y~x, family = binomial(link = probit))
probits = prob.model$coef[1] + prob.model$coef[2]*grid
normal.probs = pnorm(probits)
lines(grid, normal.probs, lty=3, col = "green")

summary(prob.model)

##Grouped approach
prob.model.grouped = glm(yes/total~xlevs, weights = total, family = binomial(link = probit))
summary(prob.model.grouped)

#Create legend
legend(x=3.5, y=0.04, legend=c("linear", "logit", "probit"), lty = c(1, 2, 3), col = c("black", "red", "green"))