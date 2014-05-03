###########
# Shallow #
###########
range(lys$toc[lys$depth == "shallow"], na.rm = TRUE)

bxplts(value = "toc", ofst = 1, data = subset(lys, depth == "shallow"))

# remove lower outlier
nrow(subset(lys, depth == "shallow" & toc == 0))
  # there's only one 0; remove this

bxplts(value = "toc", data = subset(lys, depth == "shallow" & toc > 0))
  # inverse looks slightly better

tocRmOl <- subset(lys, toc > 0)

# different random factor structure
m1 <- lme(1/(toc + 1) ~ temp * time, random = ~1|chamber/location, subset = depth == "shallow", 
          data = tocRmOl, na.action = "na.omit")
m2 <- lme(1/(toc + 1) ~ temp * time, random = ~1|chamber, subset = depth == "shallow", 
          data = tocRmOl, na.action = "na.omit")
m3 <- lme(1/(toc + 1) ~ temp * time, random = ~1|id, subset = depth == "shallow", 
          data = tocRmOl, na.action = "na.omit")
anova(m1, m2, m3)
  # m3 is slightly better

# autocorrelation
atcr.cmpr(m3, rndmFac= "id")$models
  # model4 is best

atml <- atcr.cmpr(m3, rndmFac= "id")[[4]]

# model simplification
Anova(atml)
MdlSmpl(atml)
  # interaction of temp x time; 
  # temp remained yet not significant

Fml <- MdlSmpl(atml)$model.reml

# the final model is:
lme(1/(toc + 1) ~ temp + time, random = ~1|id, 
    subset = depth == "shallow", 
    correlation=corAR1(),
    data = tocRmOl, na.action = "na.omit")

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
