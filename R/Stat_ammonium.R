###########
# Shallow #
###########
range(lys$nh[lys$depth == "shallow"])

bxplts(value = "nh", ofst= 0.0002, data = subset(lys, depth == "shallow"))
bxplts(value = "nh", ofst= .01, data = subset(lys, depth == "shallow"))
  # inverse looks better

# different random factor structure
m1 <- lme(1/(nh + .01) ~ temp * time, random = ~1|chamber/location, subset = depth == "shallow", data = lys)
m2 <- lme(1/(nh + .01) ~ temp * time, random = ~1|chamber, subset = depth == "shallow", data = lys)
m3 <- lme(1/(nh + .01) ~ temp * time, random = ~1|id, subset = depth == "shallow", data = lys)
anova(m1, m2, m3)
  # m2 is slightly better

# autocorrelation
atcr.cmpr(m2, rndmFac= "chamber")
  # model3 is best

atml <- atcr.cmpr(m2, rndmFac= "chamber")[[3]]

# model simplification
Anova(atml)
MdlSmpl(atml)
  # interaction of temp x time and temp are removed

Fml <- MdlSmpl(atml)$model.reml

# the final model is
lme(1/(nh + .01) ~ time, random = ~1|chamber, 
    correlation=corARMA(q=2),
    subset = depth == "shallow", data = lys)

Anova(Fml)

summary(Fml)

plot(allEffects(Fml))

# model diagnosis
plot(Fml)
  # wedge-shaped...
qqnorm(Fml, ~ resid(.)|id)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))

########
# Deep #
########
range(lys$nh[lys$depth == "deep"])

bxplts(value = "nh", ofst= 0.00271, data = subset(lys, depth == "deep"))
bxplts(value = "nh", ofst= .03, data = subset(lys, depth == "deep"))
  # inverse seems slightly better

# different random factor structure
m1 <- lme(1/(nh + .03) ~ temp * time, random = ~1|chamber/location, subset = depth == "deep", data = lys)
m2 <- lme(1/(nh + .03) ~ temp * time, random = ~1|chamber, subset = depth == "deep", data = lys)
m3 <- lme(1/(nh + .03) ~ temp * time, random = ~1|id, subset = depth == "deep", data = lys)
anova(m1, m2, m3)
  # m3 is slightly better

# autocorrelation
atcr.cmpr(m3, rndmFac= "id")
  # model4 looks better

atml <- atcr.cmpr(m3, rndmFac= "id")[[4]]

Anova(atml)

# model simplification
MdlSmpl(atml)
  # interaction of temp x time and temp are removable

Fml <- MdlSmpl(atml)$model.reml

# The final model is:
lme(1/(nh + .03) ~ time, random = ~1|id, 
    correlation=corAR1(),
    subset = depth == "deep", data = lys)

Anova(Fml)

summary(Fml)

plot(allEffects(Fml))

# model diagnosis
plot(Fml)
qqnorm(Fml, ~ resid(.)|id)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))
  # not great..