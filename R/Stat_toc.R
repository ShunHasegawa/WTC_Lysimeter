###########
# Shallow #
###########
range(lys$toc[lys$depth == "shallow"], na.rm = TRUE)
boxplot(lys$toc)
bxplts(value = "toc", ofst = 1, data = subset(lys, depth == "shallow"))

# remove lower outlier
bxplts(value = "toc", ofst = 1, data = subset(lys, depth == "shallow" & toc > min(toc, na.rm = TRUE)))
  # log looks slightly better, but homogeneity of variance is violated

tocRmOl <- subset(lys, toc > min(toc, na.rm = TRUE))

# different random factor structure
m1 <- lme(log(toc + 1) ~ temp * time, random = ~1|chamber/location, subset = depth == "shallow", 
          data = tocRmOl, na.action = "na.omit")
m2 <- lme(log(toc + 1) ~ temp * time, random = ~1|chamber, subset = depth == "shallow", 
          data = tocRmOl, na.action = "na.omit")
m3 <- lme(log(toc + 1) ~ temp * time, random = ~1|id, subset = depth == "shallow", 
          data = tocRmOl, na.action = "na.omit")
anova(m1, m2, m3)
  # m3 is slightly better

# autocorrelation
atcr.cmpr(m3, rndmFac= "id")
  # model4 is best

atml <- atcr.cmpr(m3, rndmFac= "id")[[4]]

Anova(atml)

# model simplification
MdlSmpl(atml)
  # interaction of temp x time; 
  # temp remained yet not significant

Fml <- MdlSmpl(atml)$model.reml

# the final model is:
lme(log(toc + 1) ~ temp + time, random = ~1|id, 
    subset = depth == "shallow", 
    correlation=corAR1(),
    data = tocRmOl, na.action = "na.omit")

Anova(Fml)

summary(Fml)

plot(allEffects(Fml))

# model diagnosis
plot(Fml)
  # wedge-shaped: homogeity of variance is not met
qqnorm(Fml, ~ resid(.)|id)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))

# if not using autocorrelation
Anova(MdlSmpl(m3)$model.reml)
plot(MdlSmpl(m3)$model.reml)
  # but even more wedged...


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
atcr.cmpr(m1, rndmFac= "chamber/location")
  # model4 is best

atml <- atcr.cmpr(m1, rndmFac= "chamber/location")[[4]]

Anova(atml)

# model simplification
MdlSmpl(atml)
  # interaction of temp x time, and temp is removed

Fml <- MdlSmpl(atml)$model.reml

# the final model is:
lme(log(toc) ~ time, 
    random = ~1|chamber/location,
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
