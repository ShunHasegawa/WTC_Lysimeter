## ----Stat_WTC_Lys_TOC_S

###########
# Shallow #
###########
range(lys$toc[lys$depth == "shallow"], na.rm = TRUE)

bxplts(value = "toc", ofst = 1, data = subset(lys, depth == "shallow"))

# remove lower outlier
nrow(subset(lys, depth == "shallow" & toc == 0))
  # there's only one 0; remove this

tocRmOl <- subset(lys, toc > 0)

bxplts(value = "toc", data = tocRmOl)
  # use box-cox lambda

# different random factor structure
m1 <- lme(toc^(-0.2626) ~ temp * time, random = ~1|chamber/location, subset = depth == "shallow", 
          data = tocRmOl, na.action = "na.omit")
m2 <- lme(toc^(-0.2626) ~ temp * time, random = ~1|chamber, subset = depth == "shallow", 
          data = tocRmOl, na.action = "na.omit")
m3 <- lme(toc^(-0.2626) ~ temp * time, random = ~1|id, subset = depth == "shallow", 
          data = tocRmOl, na.action = "na.omit")
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
  # interaction of temp x time; 
  # temp remained yet not significant

Fml_S <- MdlSmpl(Iml_S)$model.reml

# the final model is:
Fml_S$call

Anova(Fml_S)

summary(Fml_S)

# model diagnosis
plot(Fml_S)
qqnorm(Fml_S, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_S))
qqline(residuals.lm(Fml_S))

## ----Stat_WTC_Lys_TOC_D

########
# Deep #
########
range(lys$toc[lys$depth == "deep"], na.rm = TRUE)
bxplts(value = "toc", data = subset(lys, depth == "deep"))
  # log looks better

# different random factor structure
m1 <- lme(log(toc) ~ temp * time, random = ~1|chamber/location, subset = depth == "deep", 
          data = lys, na.action = "na.omit")
m2 <- lme(log(toc) ~ temp * time, random = ~1|chamber, subset = depth == "deep", 
          data = lys, na.action = "na.omit")
m3 <- lme(log(toc) ~ temp * time, random = ~1|id, subset = depth == "deep", 
          data = lys, na.action = "na.omit")
anova(m1, m2, m3)
  # m1 is slightly better

# autocorrelation
atcr.cmpr(m1, rndmFac= "chamber/location")$models
  # model4 is best

Iml_D <- atcr.cmpr(m1, rndmFac= "chamber/location")[[4]]

# The initial model is:
Iml_D$call

Anova(Iml_D)

# model simplification
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

## ----Stat_WTC_Lys_TOC_S_Smmry
# The initial model is:
Iml_S$call

Anova(Iml_S)

# The final model is:
Fml_S$call

Anova(Fml_S)

## ----Stat_WTC_Lys_TOC_D_Smmry
# The initial model is:
Iml_D$call

Anova(Iml_D)

# The final model is:
Fml_D$call

Anova(Fml_D)

