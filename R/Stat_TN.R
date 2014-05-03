###########
# Shallow #
###########
range(lys$tn[lys$depth == "shallow"], na.rm = TRUE)
bxplts(value = "tn", ofst = 10, data = subset(lys, depth == "shallow"))
  # log seems better

# different random factor structure
m1 <- lme(log(tn +10)  ~ temp * time, random = ~1|chamber/location, subset = depth == "shallow", 
          data = lys, na.action = "na.omit")
m2 <- lme(log(tn +10) ~ temp * time, random = ~1|chamber, subset = depth == "shallow", 
          data = lys, na.action = "na.omit")
m3 <- lme(log(tn +10) ~ temp * time, random = ~1|id, subset = depth == "shallow", 
          data = lys, na.action = "na.omit")
anova(m1, m2, m3)
  # m3 is slightly better

# Autocorrelation
atcr.cmpr(m3, rndmFac= "id")
  # model 3 is best

atml <- atcr.cmpr(m3, rndmFac= "id")[[3]]

# model simpliftnation
Anova(atml)

MdlSmpl(atml)

Fml <- MdlSmpl(atml)$model.reml

# The final model is:
lme(log(tn + 10) ~ time, random = ~1|id, 
    subset = depth == "shallow", 
    correlation=corARMA(q=2),
    data = lys, na.action = "na.omit")


Anova(Fml)

plot(allEffects(Fml))

# model diagnosis
plot(Fml)
qqnorm(Fml, ~ resid(.)|id)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))


########
# Deep #
########
range(lys$tn[lys$depth == "deep"], na.rm = TRUE)
bxplts(value = "tn", data = subset(lys, depth == "deep"))
# log looks better

# different random factor structure
m1 <- lme(log(tn) ~ temp * time, random = ~1|chamber/location, subset = depth == "deep", 
          data = lys, na.action = "na.omit")
m2 <- lme(log(tn) ~ temp * time, random = ~1|chamber, subset = depth == "deep", 
          data = lys, na.action = "na.omit")
m3 <- lme(log(tn) ~ temp * time, random = ~1|id, subset = depth == "deep", 
          data = lys, na.action = "na.omit")
anova(m1, m2, m3)
# m3 is slightly better

# autnorrelation
atnr.cmpr(m3, rndmFac= "id")
# model4 is best

atml <- atnr.cmpr(m3, rndmFac= "id")[[4]]

# model simpliftnation
Anova(atml)
MdlSmpl(atml)
# interaction of temp x time, and temp is removed

Fml <- MdlSmpl(atml)$model.reml

# the final model is:
lme(log(tn) ~ time, random = ~1|id, 
    correlation=corAR1(),
    subset = depth == "deep", 
    data = lys, na.action = "na.omit")

Anova(Fml)

summary(Fml)

plot(allEffects(Fml))

# model diagnosis
plot(Fml)
qqnorm(Fml, ~ resid(.)|id)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))
