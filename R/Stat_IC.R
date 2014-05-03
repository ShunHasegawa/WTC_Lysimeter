###########
# Shallow #
###########
range(lys$ic[lys$depth == "shallow"], na.rm = TRUE)
bxplts(value = "ic", ofst = .1, data = subset(lys, depth == "shallow"))
hist(subset(lys, depth == "shallow")$ic)
  # Too many 0s. any of transformations don't seem to work

# Mean vs variabne
df <- ddply(lys, .(temp, time), summarise, M = mean(ic, na.rm = TRUE), V = mean(ic, na.rm = TRUE))
plot(V ~ M, data = df)
  # seems poisson error but the response variable is not integer 

a <- subset(lys, depth == "shallow")$ic
hist(a, breaks= seq(0, 0.6, 0.01))
par(mfrow = c(3,3))
for (i in 0:6) {
  hist(round(a * 10^(i)), main = paste("10^", i, sep = ""))
}


# make the variable interger by multiplying 100
m1 <- glmer(round(ic * 100) ~ temp * time + (1|chamber/location), family = poisson, 
            data = subset(lys, depth == "shallow"), 
            na.action = "na.omit")

m2 <- glmer(round(ic * 100) ~ temp + time + (1|chamber/location), family = poisson, 
            data = subset(lys, depth == "shallow"), 
            na.action = "na.omit")

anova(m1, m2)
plot(m1)


########
# Deep #
########
range(lys$ic[lys$depth == "deep"], na.rm = TRUE)
bxplts(value = "ic", ofst=.01 ,data = subset(lys, depth == "deep"))
  # log looks better

# different random factor structure
m1 <- lme(log(ic+.1) ~ temp * time, random = ~1|chamber/location, subset = depth == "deep", 
          data = lys, na.action = "na.omit", method = "ML")
m2 <- lme(log(ic+.1) ~ temp * time, random = ~1|chamber, subset = depth == "deep", 
          data = lys, na.action = "na.omit", method = "ML")
m3 <- lme(log(ic+.1) ~ temp * time, random = ~1|id, subset = depth == "deep", 
          data = lys, na.action = "na.omit", method = "ML")
anova(m1, m2, m3)
  # m3 is slightly better

# auicorrelation
atcr.cmpr(m3, rndmFac= "id")
# model4 is best
  
atml <- atcr.cmpr(m3, rndmFac= "id")[[4]]

# model simplification
Anova(atml)
MdlSmpl(atml)
# interaction of temp x time, and temp is removed

Fml <- MdlSmpl(atml)$model.reml

# the final model is:
lme(log(ic + .1) ~ time, random = ~1|id, 
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
  # this result is not reliable....