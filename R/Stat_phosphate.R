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

Iml_S <- atcr.cmpr(m3, rndmFac= "id")[[4]]

# The initial model is:
Iml_S$call
Anova(Iml_S)

# model simplification
MdlSmpl(Iml_S)
  # interaction of temp x time and temp are removable

Fml_S <- MdlSmpl(Iml_S)$model.reml

# The final model is:
Fml_S$call

Anova(Fml_S)

summary(Fml)

# model diagnosis
plot(Fml_S)
qqnorm(Fml_S, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_S))
qqline(residuals.lm(Fml_S))

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

Iml_D <- atcr.cmpr(m1, rndmFac= "chamber/location")[[1]]

# The initial model is:
Iml_D$call
Anova(Iml_D)

# model simplification
MdlSmpl(Iml_D)
  # interaction of temp x time and temp are removable

Fml_D <- MdlSmpl(Iml_D)$model.reml

# the final model is:
Fml_D$call

Anova(Fml_D)

summary(Fml_D)

# model diagnosis
plot(Fml_D)
qqnorm(Fml_D, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_D))
qqline(residuals.lm(Fml_D))
  # not very great....
