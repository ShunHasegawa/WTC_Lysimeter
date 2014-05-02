###########
# Nitrate #
###########

## Shallow ##
range(lys$no[lys$depth == "shallow"])

bxplts(value = "no", data = subset(lys, depth == "shallow"))
  # power(1/3) seems slightly better

# different random factor structure
m1 <- lme(no^(1/3) ~ temp * time, random = ~1|chamber/location, subset = depth == "shallow", data = lys)
m2 <- lme(no^(1/3) ~ temp * time, random = ~1|chamber, subset = depth == "shallow", data = lys)
m3 <- lme(no^(1/3) ~ temp * time, random = ~1|id, subset = depth == "shallow", data = lys)
anova(m1, m2, m3)
  # m3 is slightly better

# autocorrelation
atcr.cmpr(m3, rndmFac= "id")
  # model3 is best

atml <- atcr.cmpr(m3, rndmFac= "id")[[3]]

Anova(atml)

# model simplification
MdlSmpl(atml)
  # interaction of temp x time and temp are removable

Fml <- MdlSmpl(atml)$model.reml

# the final model is
lme(no^(1/3) ~ time, random = ~1|id, correlation=corARMA(q=2), 
    subset = depth == "shallow", data = lys)

Anova(Fml)

summary(Fml)

plot(allEffects(Fml))

# model diagnosis
plot(Fml)
qqnorm(Fml, ~ resid(.)|id)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))
  # most of the masurements on a line but not very good...

###########
## Deep ###
###########

range(lys$no[lys$depth == "deep"])

bxplts(value = "no", data = subset(lys, depth == "deep"))
# log seems slightly better

# different random factor structure
m1 <- lme(log(no) ~ temp * time, random = ~1|chamber/location, subset = depth == "deep", data = lys)
m2 <- lme(log(no) ~ temp * time, random = ~1|chamber, subset = depth == "deep", data = lys)
m3 <- lme(log(no) ~ temp * time, random = ~1|id, subset = depth == "deep", data = lys)
anova(m1, m2, m3)
 # m1 is better

# autocorrelation
atcr.cmpr(m1, rndmFac= "chamber/location")
  # model4 is best

atml <- atcr.cmpr(m1, rndmFac= "chamber/location")[[4]]

Anova(atml)

# model simplification
MdlSmpl(atml)
# interaction of temp x time and temp are removable

Fml <- MdlSmpl(atml)$model.reml

# the final model is
lme(log(no) ~ time, random = ~1|chamber/location, correlation=corAR1(), 
    subset = depth == "deep", data = lys)

Anova(Fml)

summary(Fml)

plot(allEffects(Fml))

# model diagnosis
plot(Fml)
qqnorm(Fml, ~ resid(.)|chamber)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))
  # not very good

