###########
# Shallow #
###########
range(lys$nh[lys$depth == "shallow"])

bxplts(value = "nh", ofst= 0.0002, data = subset(lys, depth == "shallow"))
  # log is slightly better, but homogeneity of variabne 
  # is violated

# different random factor structure
m1 <- lme(log(nh + .0002) ~ temp * time, random = ~1|chamber/location, subset = depth == "shallow", data = lys)
m2 <- lme(log(nh + .0002) ~ temp * time, random = ~1|chamber, subset = depth == "shallow", data = lys)
m3 <- lme(log(nh + .0002) ~ temp * time, random = ~1|id, subset = depth == "shallow", data = lys)
anova(m1, m2, m3)
  # m3 is slightly better

# autocorrelation
atcr.cmpr(m3, rndmFac= "id")
  # model4 is best

atml <- atcr.cmpr(m3, rndmFac= "id")[[4]]

Anova(atml)

# model simplification
MdlSmpl(atml)
  # interaction of temp x time and temp are removable

Fml <- MdlSmpl(atml)$model.reml

# the final model is
lme(log(nh + .0002) ~ time, random = ~1|id,
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
  # not great....

########
# Deep #
########
range(lys$nh[lys$depth == "deep"])

bxplts(value = "nh", ofst= 0.00271, data = subset(lys, depth == "deep"))
  #remove the lower outlier

bxplts(value = "nh", ofst= 0.00271, data = subset(lys, depth == "deep" & nh > min(nh)))
  # improved a lot; log looks better

NHRmOl <- subset(lys, nh > min(nh))

# different random factor structure
m1 <- lme(log(nh + .00271) ~ temp * time, random = ~1|chamber/location, subset = depth == "deep", data = NHRmOl)
m2 <- lme(log(nh + .00271) ~ temp * time, random = ~1|chamber, subset = depth == "deep", data = NHRmOl)
m3 <- lme(log(nh + .00271) ~ temp * time, random = ~1|id, subset = depth == "deep", data = NHRmOl)
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
lme(log(nh + .00271) ~ time, random = ~1|id, 
    correlation=corAR1(),
    subset = depth == "deep", data = NHRmOl)

Anova(Fml)

summary(Fml)

plot(allEffects(Fml))

# model diagnosis
plot(Fml)
qqnorm(Fml, ~ resid(.)|id)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))
  # not great..