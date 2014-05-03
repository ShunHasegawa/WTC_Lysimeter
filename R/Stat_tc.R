###########
# Shallow #
###########
range(lys$tc[lys$depth == "shallow"], na.rm = TRUE)
bxplts(value = "tc", ofst = 1, data = subset(lys, depth == "shallow"))

# remove lower outlier
bxplts(value = "tc", ofst = 1, data = subset(lys, depth == "shallow" & tc != min(tc, na.rm = TRUE)))
  # log looks slightly better, but homogeneity of variance is violated

# different random factor structure
m1 <- lme(log(tc + 1) ~ temp * time, random = ~1|chamber/location, subset = depth == "shallow", 
          data = tcRmOl, na.action = "na.omit")
m2 <- lme(log(tc + 1) ~ temp * time, random = ~1|chamber, subset = depth == "shallow", 
          data = tcRmOl, na.action = "na.omit")
m3 <- lme(log(tc + 1) ~ temp * time, random = ~1|id, subset = depth == "shallow", 
          data = tcRmOl, na.action = "na.omit")
anova(m1, m2, m3)
  # m3 is slightly better

# autcorrelation
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
lme(log(tc + 1) ~ temp + time, random = ~1|id, 
    subset = depth == "shallow", 
    correlation=corAR1(),
    data = tcRmOl, na.action = "na.omit")

Anova(Fml)

summary(Fml)

plot(allEffects(Fml))

# model diagnosis
plot(Fml)
  # wedge-shaped: homogeity of variance is not met
qqnorm(Fml, ~ resid(.)|id)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))

# if not using autcorrelation
Anova(MdlSmpl(m3)$model.reml)
plot(MdlSmpl(m3)$model.reml)
  # but even more wedged...

##############################
## try other transformation ##
##############################

# Inverse transformation

# plot mean vs variance
Rwdf <- ddply(tcRmOl, .(temp, time), summarise, M = mean(tc, na.rm = TRUE), V = var(tc, na.rm = TRUE))
Invdf <- ddply(tcRmOl, .(temp, time), summarise, M = mean(1/(tc+1), na.rm = TRUE), V = var(1/(tc+1), na.rm = TRUE))
dfs <- list('Raw' = Rwdf, 'Inv' = Invdf)

par(mfrow = c(2,2))
l_ply(1:2, function(x) plot(V ~ M, data = dfs[[x]], main = names(dfs)[x]))
boxplot(tc ~ temp * time, data = subset(tcRmOl, depth == "shallow"))
boxplot(1/(tc+1) ~ temp * time, data = subset(tcRmOl, depth == "shallow"))
par(mfrow = c(1,1))

  # mean-variabnce constancy is improved a lot

# different random factor structure
m1 <- lme(1/(tc+1) ~ temp * time, random = ~1|chamber/location, subset = depth == "shallow", 
          data = tcRmOl, na.action = "na.omit")
m2 <- lme(1/(tc+1) ~ temp * time, random = ~1|chamber, subset = depth == "shallow", 
          data = tcRmOl, na.action = "na.omit")
m3 <- lme(1/(tc+1) ~ temp * time, random = ~1|id, subset = depth == "shallow", 
          data = tcRmOl, na.action = "na.omit")
anova(m1, m2, m3)
  # m3 is slightly better

# Autocorrelation
atcr.cmpr(m3, rndmFac= "id")
  # model 4 is best

atml <- atcr.cmpr(m3, rndmFac= "id")[[4]]

# model simplification
MdlSmpl(atml)

Fml <- MdlSmpl(atml)$model.reml

# The final model is:
lme(1/(tc+1) ~ temp * time, random = ~1|id, 
    subset = depth == "shallow", 
    correlation=corAR1(),
    data = tcRmOl, na.action = "na.omit")


Anova(Fml)

plot(Fml)

# model diagnosis
plot(Fml)
  # homogeity of variance is improved a lot
qqnorm(Fml, ~ resid(.)|id)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))


########
# Deep #
########
range(lys$tc[lys$depth == "deep"], na.rm = TRUE)
bxplts(value = "tc", data = subset(lys, depth == "deep"))
# log looks better
  
# different random factor structure
m1 <- lme(log(tc) ~ temp * time, random = ~1|chamber/location, subset = depth == "deep", 
          data = lys, na.action = "na.omit")
m2 <- lme(log(tc) ~ temp * time, random = ~1|chamber, subset = depth == "deep", 
          data = lys, na.action = "na.omit")
m3 <- lme(log(tc) ~ temp * time, random = ~1|id, subset = depth == "deep", 
          data = lys, na.action = "na.omit")
anova(m1, m2, m3)
# m1 is slightly better

# autcorrelation
atcr.cmpr(m1, rndmFac= "chamber/location")
# model4 is best

atml <- atcr.cmpr(m1, rndmFac= "chamber/location")[[4]]

Anova(atml)

# model simplification
MdlSmpl(atml)
# interaction of temp x time, and temp is removed

Fml <- MdlSmpl(atml)$model.reml

# the final model is:
lme(log(tc) ~ time, 
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
