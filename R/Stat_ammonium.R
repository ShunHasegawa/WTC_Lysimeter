###########
# Shallow #
###########
range(lys$nh[lys$depth == "shallow"])

bxplts(value = "nh", ofst= 0.0002, data = subset(lys, depth == "shallow"))
bxcxplts(value = "nh", data = subset(lys, depth == "shallow"), sval = 0.01, fval = .06)
# adding constant value of 0.01 may improve
bxplts(value = "nh", ofst= .01, data = subset(lys, depth == "shallow"))
  # inverse looks better

# different random factor structure
m1 <- lme(1/(nh + .01) ~ temp * time, random = ~1|chamber/location, subset = depth == "shallow", data = lys)
m2 <- lme(1/(nh + .01) ~ temp * time, random = ~1|chamber, subset = depth == "shallow", data = lys)
m3 <- lme(1/(nh + .01) ~ temp * time, random = ~1|id, subset = depth == "shallow", data = lys)
anova(m1, m2, m3)
  # m2 is slightly better

# autocorrelation
atcr.cmpr(m2, rndmFac= "chamber")$models
  # model3 is best

Iml_S <- atcr.cmpr(m2, rndmFac= "chamber")[[3]]

# The initial model is:
Iml_S$call
Anova(Iml_S)

# model simplification
MdlSmpl(Iml_S)
  # interaction of temp x time and temp are removed

Fml_S <- MdlSmpl(atml)$model.reml

# the final model is
Fml_S$call

Anova(Fml_S)

summary(Fml_S)

# model diagnosis
plot(Fml_S)
  # wedge-shaped...
qqnorm(Fml_S, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_S))
qqline(residuals.lm(Fml_S))

########
# Deep #
########
range(lys$nh[lys$depth == "deep"])

bxplts(value = "nh", ofst= 0.00271, data = subset(lys, depth == "deep"))
bxcxplts(value = "nh", data = subset(lys, depth == "deep"), sval = 0.00271, fval = .03)
bxplts(value = "nh", ofst= .03, data = subset(lys, depth == "deep"))
  # inverse seems slightly better

# different random factor structure
m1 <- lme(1/(nh + .03) ~ temp * time, random = ~1|chamber/location, subset = depth == "deep", data = lys)
m2 <- lme(1/(nh + .03) ~ temp * time, random = ~1|chamber, subset = depth == "deep", data = lys)
m3 <- lme(1/(nh + .03) ~ temp * time, random = ~1|id, subset = depth == "deep", data = lys)
anova(m1, m2, m3)
  # m3 is slightly better

# autocorrelation
atcr.cmpr(m3, rndmFac= "id")$models
  # model4 looks better

Iml_D <- atcr.cmpr(m3, rndmFac= "id")[[4]]

# The initial model is:
Iml_D$call

Anova(Iml_D)

# model simplification
MdlSmpl(Iml_D)
  # interaction of temp x time and temp are removable

Fml_D <- MdlSmpl(Iml_D)$model.reml

# The final model is:
Fml_D$call

Anova(Fml_D)

summary(Fml_D)

# model diagnosis
plot(Fml_D)
qqnorm(Fml_D, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_D))
qqline(residuals.lm(Fml_D))
  # not great..