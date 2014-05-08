###########
# Shallow #
###########
range(lys$tc[lys$depth == "shallow"], na.rm = TRUE)
bxplts(value = "tc", ofst = 1, data = subset(lys, depth == "shallow"))

# remove lower outlier
tcRmOl <- subset(lys, depth == "shallow" & tc != min(tc, na.rm = TRUE))

bxplts(value = "tc", data = tcRmOl)
  # inverse looks better



# different random factor structure
m1 <- lme(1/(tc) ~ temp * time, random = ~1|chamber/location, subset = depth == "shallow", 
          data = tcRmOl, na.action = "na.omit")
m2 <- lme(1/(tc) ~ temp * time, random = ~1|chamber, subset = depth == "shallow", 
          data = tcRmOl, na.action = "na.omit")
m3 <- lme(1/(tc) ~ temp * time, random = ~1|id, subset = depth == "shallow", 
          data = tcRmOl, na.action = "na.omit")
anova(m1, m2, m3)
  # m3 is slightly better

# Autocorrelation
atcr.cmpr(m3, rndmFac= "id")$models
  # model 4 is best

Iml_S <- atcr.cmpr(m3, rndmFac= "id")[[4]]

# The initial model is:
Iml_S$call
Anova(Iml_S)

# model simplification
MdlSmpl(Iml_S)

Fml_S <- MdlSmpl(Iml_S)$model.reml

# The final model is:
Fml_S$call

Anova(Fml_S)

plot(allEffects(Fml_S))

# model diagnosis
plot(Fml_S)
qqnorm(Fml_S, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_S))
qqline(residuals.lm(Fml_S))

########
# Deep #
########
range(lys$tc[lys$depth == "deep"], na.rm = TRUE)
bxplts(value = "tc", data = subset(lys, depth == "deep"))
  # use box-cox lambda
  
# different random factor structure
m1 <- lme(tc^(-0.2222) ~ temp * time, random = ~1|chamber/location, subset = depth == "deep", 
          data = lys, na.action = "na.omit")
m2 <- lme(tc^(-0.2222) ~ temp * time, random = ~1|chamber, subset = depth == "deep", 
          data = lys, na.action = "na.omit")
m3 <- lme(tc^(-0.2222) ~ temp * time, random = ~1|id, subset = depth == "deep", 
          data = lys, na.action = "na.omit")
anova(m1, m2, m3)
  # m3 is slightly better

# autcorrelation
atcr.cmpr(m3, rndmFac= "id")$models
# model4 is best

Iml_D <- atcr.cmpr(m3, rndmFac= "id")[[4]]

# The initial model is:
Iml_D$call

Anova(Iml_D)

# model simplification
MdlSmpl(Iml_D)
  # interaction of temp x time, and temp is removed

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
