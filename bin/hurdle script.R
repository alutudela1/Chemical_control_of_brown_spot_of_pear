
# fitting hurdle models to the lesion data to account for excess zero counts
# the idea is to compare the overall level of incidence [zero counts], mean for cultivars 
# also compare the fit of the models with hurdle with Poisson and hurdle with NB for potted/orchard
# trees.

library(MASS)
library(pscl)
library(lmtest)
library(tables)

# read in observed data

setwd("C:/R/Data")
newdata <- read.csv("all_lesions_for_R.csv")
attach(newdata)
newdata$Year <- as.factor(newdata$Year)
newdata$Cultivar <-as.factor(newdata$Cultivar)

newdata$infected <- newdata$Count > 0

tabular(Cultivar*Year~(n=1)+(as.numeric(infected)+Count)*(mean),data=newdata)

# Fitting hurdle models with poisson 

P_hpoisson1 <- hurdle(Count~Cultivar * Year, data=newdata)
summary(P_hpoisson1)
P_hpoisson2 <- hurdle(Count~Cultivar*Year|Cultivar, data=newdata)
summary(P_hpoisson2)

# Fitting hurdle models with negative binomial 

P_hnb1 <- hurdle(Count~Cultivar * Year, dist="negbin",data=newdata)
summary(P_hnb1)
P_hnb2 <- hurdle(Count~Cultivar*Year|Cultivar, dist="negbin", data=newdata)
summary(P_hnb2)

ml <- list("POS"=P_hpoisson2,"NB"=P_hnb2)
sapply(ml,function(x) coef(x)[1:6])               # all estimates from all models
sapply(ml,function(x) sqrt(diag(vcov(x)))[1:6])   # std err of all estimates
rbind(logLik=sapply(ml,function(x) round(logLik(x),digits=0)),df=sapply(ml,function(x) attr(logLik(x),"df")))


