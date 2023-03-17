data(spinach)
spinach$CURVE <- as.factor(spinach$CURVE)
### nlme
sm2 <- medrm(SLOPE ~ DOSE, 
             curveid=b + d + e ~ HERBICIDE, 
             data=spinach, 
             fct=LL.3(), 
             random = b + d + e ~ 1|CURVE,
             start=c(0.5, 1, 1.5, 1.5, 1.5, 0.3))
summary(sm2)

# The two fixed effect curves are compared by the ratio of effective dose estimates.

cmat <- rbind(c(1, 1), 
              c(2, 2), 
              c(3, 3))

library(dplyr)
library(tidyr)

# comparing effective dose levels for nlme
EDcomp(sm2, 
       percVec=c(15, 50, 85), 
       percMat=cmat, 
       interval="fieller")
## 
## Estimated ratios of effect doses
## 
##                        Estimate     Lower     Upper
## bentazon/diuron:15/15   0.80056   0.39510   1.23745
## bentazon/diuron:50/50   8.86013   6.90605  10.89971
## bentazon/diuron:85/85  98.05903  63.37847 135.24172

library(dplyr)
library(tidyr)

spinach %>% head

pdata <- spinach %>%
  group_by(CURVE, HERBICIDE) %>%
  tidyr::expand(DOSE=exp(seq(-5, 5, length=50)))

pdata$SLOPEind <- predict(sm2, newdata=pdata)
pdata$SLOPE <- predict(sm2, newdata=pdata, level=0)

ggplot(spinach, aes(x=log(DOSE), y=SLOPE, 
                    colour=HERBICIDE, group=CURVE, shape=HERBICIDE)) +
  geom_point() +
  geom_line(data=pdata) +
  geom_line(data=pdata, aes(y=SLOPEind), linetype=2) +
  theme_bw() +
  scale_x_continuous("DOSE", 
                     breaks=log(c(0.01, 0.1, 1, 10, 100)), 
                     labels=c(0.01, 0.1, 1, 10, 100))

                                  