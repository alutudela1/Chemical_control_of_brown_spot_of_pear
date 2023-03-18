# resources:

https://rpubs.com/TX-YXL/656451
https://doseresponse.github.io/medrc/reference/metadrm.html
https://doseresponse.github.io/drc/reference/LL.3.html

## Fitting model with lower limit equal 0
ryegrass %>% 
  ggplot() + 
  aes(x=conc, rootl) + 
  geom_point()

ryegrass.model1 <- drm(rootl ~ conc, data = ryegrass, fct = LL.3())
summary(ryegrass.model1)
ED(ryegrass.model1, respLev = c(50), interval="delta", type = "relative")

# -------
data(spinach) 
spinach$CURVE <- as.factor(spinach$CURVE)
head(spinach)
str(spinach)
str(data.frame(df))
ftable(xtabs(~ HERBICIDE + CURVE + DOSE, spinach))
ftable(xtabs(~ fungicide + strain + dose, df)) 

spinach %>% 
  ggplot()+
  aes(x=log(DOSE+.001), y=SLOPE, colour=HERBICIDE, group=CURVE) +
  # geom_point() + 
  geom_text(aes(label=CURVE))


bentazon <- spinach %>% filter(HERBICIDE=="bentazon")
drm(SLOPE~ DOSE, data = bentazon, fct = LL.3())

diuron <- spinach %>% filter(HERBICIDE=="diuron")
drm(SLOPE~ DOSE, data = diuron, fct = LL.3())

### meta

# meta analysis approach
sm1 <- metadrm(SLOPE ~ DOSE, 
               data=spinach,
               fct=LL.3(),
               ind=CURVE,
               cid2=HERBICIDE,
               struct="UN")
summary(sm1)

### nlme
sm2 <- medrm(SLOPE ~ DOSE, 
             curveid=b + d + e ~ HERBICIDE, 
             data=spinach, 
             fct=LL.3(), 
             random = b + d + e ~ 1|CURVE,
             start=c(0.5, 1, 1.5, #   0.4808 1/1.3000 1.5/1.7895  
                     1.5, 1.5, 0.3)) # 1.6605 1.9858 0.2086  
summary(sm2)
plot(sm2)
# The two fixed effect curves are compared by the ratio of effective dose estimates.

cmat <- rbind(c(1, 1), 
              c(2, 2), 
              c(3, 3))

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

spinach %>% head

pdata <- spinach %>%
  group_by(CURVE, HERBICIDE) %>%
  tidyr::expand(DOSE=exp(seq(-5, 5, length=50)))

pdata$SLOPEind <- predict(sm2, newdata=pdata)
pdata$SLOPE <- predict(sm2, newdata=pdata, level=0)

ggplot(spinach, 
       aes(x=log(DOSE), y=SLOPE, 
                    colour=HERBICIDE, group=CURVE, shape=HERBICIDE)) +
  geom_point() +
  geom_line(data=pdata) +
  geom_line(data=pdata, aes(y=SLOPEind), linetype=2) +
  theme_bw() +
  scale_x_continuous("DOSE", 
                     breaks=log(c(0.01, 0.1, 1, 10, 100)), 
                     labels=c(0.01, 0.1, 1, 10, 100))

                                  