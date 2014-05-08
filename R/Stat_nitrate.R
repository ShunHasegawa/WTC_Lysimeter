## ----Stat_WTC_Lys_Nitrate_S

###########
# Shallow #
###########
range(lys$no[lys$depth == "shallow"])

bxplts(value = "no", data = subsetD(lys, depth == "shallow"))
bxcxplts(value = "no", data = subsetD(lys, depth == "shallow"), sval = 0, fval = 4.5)
# adding constatn value of 4.5 may improve

bxplts(value = "no", data = subsetD(lys, depth == "shallow"), ofst= 4.5)
# use box-cox lambda


# different random factor structure
m1 <- lme((no + 4.5)^(0.0202) ~ temp * time, random = ~1|chamber/location, subset = depth == "shallow", data = lys)
m2 <- lme((no + 4.5)^(0.0202) ~ temp * time, random = ~1|chamber, subset = depth == "shallow", data = lys)
m3 <- lme((no + 4.5)^(0.0202) ~ temp * time, random = ~1|id, subset = depth == "shallow", data = lys)
anova(m1, m2, m3)
# m3 is slightly better

# autocorrelation
atcr.cmpr(m3, rndmFac= "id")$models
# model3 is best

Iml_S <- atcr.cmpr(m3, rndmFac= "id")[[3]]

# The initial model is:
Iml_S$call
Anova(Iml_S)

# model simplification
MdlSmpl(Iml_S)
# interaction of temp x time and temp are removable

Fml_S <- MdlSmpl(Iml_S)$model.reml



# the final model is
Fml_S$call

Anova(Fml_S)

summary(Fml_S)

# model diagnosis
plot(Fml_S)
qqnorm(Fml_S, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_S))
qqline(residuals.lm(Fml_S))


## ----Stat_WTC_Lys_Nitrate_D

########
# Deep #
########

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
atcr.cmpr(m1, rndmFac= "chamber/location")$models
  # model4 is best

Iml_D <- atcr.cmpr(m1, rndmFac= "chamber/location")[[4]]

# The initial model is:
Iml_D$call

Anova(Iml_D)

# model simplification
MdlSmpl(Iml_D)
  # interaction of temp x time and temp are removable

Fml_D <- MdlSmpl(Iml_D)$model.reml

# The final model is
Fml_D$call

Anova(Fml_D)

summary(Fml_D)

# model diagnosis
plot(Fml_D)
qqnorm(Fml_D, ~ resid(.)|chamber)
qqnorm(residuals.lm(Fml_D))
qqline(residuals.lm(Fml_D))
  # not very good...

## ----Stat_WTC_Lys_Nitrate_S_Smmry
# The initial model is:
Iml_S$call

Anova(Iml_S)

# The final model is:
Fml_S$call

Anova(Fml_S)

## ----Stat_WTC_Lys_Nitrate_D_Smmry
# The initial model is:
Iml_D$call

Anova(Iml_D)

# The final model is:
Fml_D$call

Anova(Fml_D)