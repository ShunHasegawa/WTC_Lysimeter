## ----Stat_WTC_Lys_TN_S

###########
# Shallow #
###########
range(lys$tn[lys$depth == "shallow"], na.rm = TRUE)
bxcxplts(value = "tn", data = subset(lys, depth == "shallow"), sval = .1, fval = 100)
# adding constant value of 30 may improve

bxplts(value = "tn", ofst = 30, data = subset(lys, depth == "shallow"))
  # use box-cox lambda

# different random factor structure
m1 <- lme((tn + 30)^(-0.303)  ~ temp * time, random = ~1|chamber/location, subset = depth == "shallow", 
          data = lys, na.action = "na.omit")
m2 <- lme((tn + 30)^(-0.303) ~ temp * time, random = ~1|chamber, subset = depth == "shallow", 
          data = lys, na.action = "na.omit")
m3 <- lme((tn + 30)^(-0.303) ~ temp * time, random = ~1|id, subset = depth == "shallow", 
          data = lys, na.action = "na.omit")
anova(m1, m2, m3)
  # m3 is slightly better

# Autocorrelation
atcr.cmpr(m3, rndmFac= "id")$models
  # model 3 is best

Iml_S <- atcr.cmpr(m3, rndmFac= "id")[[3]]

# The initial model is:
Iml_S$call
Anova(Iml_S)

# model simpliftnation
MdlSmpl(Iml_S)

Fml_S <- MdlSmpl(Iml_S)$model.reml

# The final model is:
Fml_S$call

Anova(Fml_S)

# model diagnosis
plot(Fml_S)
qqnorm(Fml_S, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_S))
qqline(residuals.lm(Fml_S))

## ----Stat_WTC_Lys_TN_D

########
# Deep #
########
range(lys$tn[lys$depth == "deep"], na.rm = TRUE)
bxplts(value = "tn", data = subset(lys, depth == "deep"))
  # use box-cox lambda

# different random factor structure
m1 <- lme(tn^(0.101) ~ temp * time, random = ~1|chamber/location, subset = depth == "deep", 
          data = lys, na.action = "na.omit")
m2 <- lme(tn^(0.101) ~ temp * time, random = ~1|chamber, subset = depth == "deep", 
          data = lys, na.action = "na.omit")
m3 <- lme(tn^(0.101) ~ temp * time, random = ~1|id, subset = depth == "deep", 
          data = lys, na.action = "na.omit")
anova(m1, m2, m3)
  # m1 is slightly better

# autnorrelation
atcr.cmpr(m1, rndmFac= "chamber/location")$models
  # model4 is best

Iml_D <- atcr.cmpr(m1, rndmFac= "chamber/location")[[4]]

# The initial model is:
Iml_D$call
Anova(Iml_D)

# model simpliftnation
MdlSmpl(Iml_D)
  # interaction of temp x time, and temp is removed

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

## ----Stat_WTC_Lys_TN_S_Smmry
# The initial model is:
Iml_S$call

Anova(Iml_S)

# The final model is:
Fml_S$call

Anova(Fml_S)

## ----Stat_WTC_Lys_TN_D_Smmry
# The initial model is:
Iml_D$call

Anova(Iml_D)

# The final model is:
Fml_D$call

Anova(Fml_D)
