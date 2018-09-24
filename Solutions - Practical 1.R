# load package JM
library("JMbayes")

###############
# Practical 1 #
###############

# T1
## fit a linear mixed model
lmeFit.p1 <- lme(log(serBilir) ~ year + drug:year, data = pbc2,
    random = ~ year | id)

# T2
## create the indicator for the composite event
pbc2.id$status2 <- as.numeric(pbc2.id$status != "alive")

# T3
## fit a Cox model
survFit.p1 <- coxph(Surv(years, status2) ~ drug, data = pbc2.id, x = TRUE)

# T4
## fit a joint model
jointFit.p1 <- jointModelBayes(lmeFit.p1, survFit.p1, timeVar = "year", baseHaz = "P-splines")
    
# T5
## summarize the results of the joint model
summary(jointFit.p1)

# T6
## obtain the estimate and credible interval of the OR from the joint model
confint(jointFit.p1, parm = "Longitudinal")
exp(confint(jointFit.p1, parm = "Event"))

# T7
## inlude interaction term between serBilir and drug and update the joint model
intFact <- function(x, data) {
   cbind(x, "D-penicil" = x * (data$drug == "D-penicil"))
}

jointFit2.p1 <- update(jointFit.p1, transFun = intFact)

## summarize the results of the joint model
summary(jointFit2.p1)

# T8
## fit a joint model under the null H_0: beta_2 = 0
lmeFit2.p1 <- lme(log(serBilir) ~ year, data = pbc2,
    random = ~ year | id)
jointFit3.p1 <- update(jointFit2.p1, lmeObject = lmeFit2.p1)

## compare the 2 models
anova(jointFit3.p1, jointFit2.p1)

## fit a joint model under the null H_0: gamma = alpha_2 = 0
survFit2.p1 <- coxph(Surv(years, status2) ~ 1, data = pbc2.id, x = TRUE)
jointFit4.p1 <- update(jointFit2.p1, survObject = survFit2.p1, transFun = NULL)

## compare the 2 models
anova(jointFit4.p1, jointFit2.p1)

## fit a joint model under the null H_0: beta_2 = gamma = alpha_2 = 0
jointFit5.p1 <- update(jointFit2.p1, lmeObject = lmeFit2.p1, 
    survObject = survFit2.p1, transFun = NULL)

## compare the 2 models
anova(jointFit5.p1, jointFit2.p1)

# T9
## obtain the data of Patient 5
dataP5 <- pbc2[pbc2$id == 5, ]

# T10
## obtain survival probabilities using only the 1st measurement
sfit <- survfitJM(jointFit5.p1, newdata = dataP5[1, ])

## print those probabilities
sfit

## plot those probabilities
plot(sfit)

## plot those probabilities including also the longitudinal outcome in the plot
plot(sfit, include.y = TRUE)

# T11
## use a for-loop to update predictions at each visit

for (i in 1:nrow(dataP5)) {
  data.i <- dataP5[1:i, ]
  sfit.i <- survfitJM(jointFit5.p1, newdata = data.i)
  plot(sfit.i, estimator = "mean", include.y = TRUE,
       conf.int = TRUE, fill.area = TRUE, col.area = "lightgrey", 
       ylab2 = "log serBilir")
}

