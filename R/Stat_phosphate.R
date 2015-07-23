## ----Stat_WTC_Lys_Phosphate_S

###########
# Shallow #
###########
range(Sdf$po)
bxplts(value = "po", ofst = 0.001, data = Sdf)
# power(1/3) looks better

m1 <- lmer((po + 0.01)^(1/3) ~ temp * time + (1|chamber), data = Sdf)
Anova(m1)
# no interaction so remove time1
Iml_S_po <- update(m1, data = Sdf2)
Anova(Iml_S_po)

Fml_S_po <- stepLmer(Iml_S_po)
Anova(Fml_S_po)
AnvF_S_po <- Anova(Fml_S_po, test.statistic = "F")
AnvF_S_po

# model diagnosis
plot(Fml_S_po)
qqnorm(resid(Fml_S_po))
qqline(resid(Fml_S_po))

# ANCOVA
scatterplotMatrix(~ po + moist + Temp5_Mean, data = Sdf2, diag = "boxplot", 
                  groups = Sdf2$temp, by.group = TRUE)
xyplot(po  ~ moist|chamber, groups = temp, type = c("r", "p"), data = Sdf2)
xyplot(po  ~ Temp5_Mean|chamber, groups = temp, type = c("r", "p"), data = Sdf2)

Iml_ancv_po <- lmer((po + 0.01)^(1/3) ~ temp * (moist + Temp5_Mean) + (1|chamber), data = Sdf2)
Anova(Iml_ancv_po)
Fml_ancv_po <- stepLmer(Iml_ancv_po, alpha.fixed = .1)
AnvF_ancv_po <- Anova(Fml_ancv_po, test.statistic = "F")
AnvF_ancv_po

# model diagnosis
plot(Fml_ancv_po)
qqnorm(resid(Fml_ancv_po))
qqline(resid(Fml_ancv_po))

# visualise
par(mfrow = c(1, 2))
TransVirsreg(visreg(Fml_ancv_po, xvar = "moist", plot = FALSE), 
             trans = function(x) x^3 + 0.01, 
             point = list(col = Sdf2$temp, cex = 1))
TransVirsreg(visreg(Fml_ancv_po, xvar = "Temp5_Mean", plot = FALSE), 
             trans = function(x) x^3 + 0.01, 
             point = list(col = Sdf2$temp, cex = 1))

## ----Stat_WTC_Lys_Phosphate_D

########
# Deep #
########
range(Ddf$po)
bxplts(value = "po", data = Ddf)

# use log
m1 <- lmer(log(po) ~ temp * time + (1|chamber), data = Ddf)
Anova(m1)

# no interaction
Iml_D_po <- update(m1, data = Ddf2)
Anova(Iml_D_po)

Fml_D_po <- stepLmer(Iml_D_po)
Anova(Fml_D_po)
AnvF_D_po <- Anova(Fml_D_po, test.statistic = "F")
AnvF_D_po

# model diagnosis
plot(Fml_D_po)
qqnorm(resid(Fml_D_po))
qqline(resid(Fml_D_po))

## ----Stat_WTC_Lys_Phosphate_S_Smmry
# The initial model is:
Iml_S_po@call

Anova(Iml_S_po)

# The final model is:
Fml_S_po@call

# Chi
Anova(Fml_S_po)

# F test
AnvF_S_po

# ANCOVA
Iml_ancv_po@call
Anova(Iml_ancv_po)

Fml_ancv_po@call
Anova(Fml_ancv_po)
AnvF_ancv_po

# visualise
par(mfrow = c(1, 2))
TransVirsreg(visreg(Fml_ancv_po, xvar = "moist", plot = FALSE), 
             trans = function(x) x^3 + 0.01, 
             point = list(col = Sdf2$temp, cex = 1))
TransVirsreg(visreg(Fml_ancv_po, xvar = "Temp5_Mean", plot = FALSE), 
             trans = function(x) x^3 + 0.01, 
             point = list(col = Sdf2$temp, cex = 1))

## ----Stat_WTC_Lys_Phosphate_D_Smmry
# The initial model is:
Iml_D_po@call

Anova(Iml_D_po)

# The final model is:
Fml_D_po@call

# Chi
Anova(Fml_D_po)

# F test
AnvF_D_po
