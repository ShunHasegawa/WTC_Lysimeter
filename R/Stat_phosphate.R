###########
# Shallow #
###########
range(lys$po[lys$depth == "shallow"])
bxplts(value = "po", ofst = 0.001, data = subset(lys, depth == "shallow"))
  #sqrt looks better

# different random factor structure
m1 <- lme(sqrt(po + .001) ~ temp * time, random = ~1|chamber/location, subset = depth == "shallow", data = lys)
m2 <- lme(sqrt(po + .001) ~ temp * time, random = ~1|chamber, subset = depth == "shallow", data = lys)
m3 <- lme(sqrt(po + .001) ~ temp * time, random = ~1|id, subset = depth == "shallow", data = lys)
anova(m1, m2, m3)
  # m3 is slightly better

# autocorrelation
atcr.cmpr(m3, rndmFac= "id")$models
  # model4 is best

atml <- atcr.cmpr(m3, rndmFac= "id")[[4]]

Anova(atml)

# model simplification
MdlSmpl(atml)
  # interaction of temp x time and temp are removable

Fml <- MdlSmpl(atml)$model.reml

# the final model is:
lme(sqrt(po + .001) ~ time, random = ~1|id, 
    correlation=corAR1(),
    subset = depth == "shallow", data = lys)

Anova(Fml)

summary(Fml)

plot(allEffects(Fml))

# model diagnosis
plot(Fml)
qqnorm(Fml, ~ resid(.)|id)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))

########
# Deep #
########
range(lys$po[lys$depth == "deep"])
bxplts(value = "po", data = subset(lys, depth == "deep"))
  # sqrt looks better

# different random factor structure
m1 <- lme(sqrt(po) ~ temp * time, random = ~1|chamber/location, subset = depth == "deep", data = lys)
m2 <- lme(sqrt(po) ~ temp * time, random = ~1|chamber, subset = depth == "deep", data = lys)
m3 <- lme(sqrt(po) ~ temp * time, random = ~1|id, subset = depth == "deep", data = lys)
anova(m1, m2, m3)
  # m1 is slightly better

# autocorrelation
atcr.cmpr(m1, rndmFac= "chamber/location")$models
  # no need for correlation

Anova(m1)

# model simplification
MdlSmpl(m1)
  # interaction of temp x time and temp are removable

Fml <- MdlSmpl(m1)$model.reml

# the final model is:
lme(sqrt(po) ~ time, random = ~1|chamber/location, subset = depth == "deep", data = lys)

Anova(Fml)

summary(Fml)

plot(allEffects(Fml))

# model diagnosis
plot(Fml)
qqnorm(Fml, ~ resid(.)|id)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))
  # not very great....

