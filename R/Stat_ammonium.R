## ----Stat_WTC_Lys_Ammonium_S

###########
# Shallow #
###########
bxplts(value = "nh", ofst= 0.01, data = Sdf)
  # inverse looks better

# use inverse
m1 <- lmer(1/(nh + 0.01) ~ temp * time + (1|chamber), data = Sdf)
Anova(m1)
# no interaction so remove time1
Iml_S_nh <- update(m1, data = Sdf2)
Anova(Iml_S_nh)

Fml_S_nh <- stepLmer(Iml_S_nh)
Anova(Fml_S_nh)
AnvF_S_nh <- Anova(Fml_S_nh, test.statistic = "F")
AnvF_S_nh

# model diagnosis
plot(Fml_S_nh)
qqnorm(resid(Fml_S_nh))
qqline(resid(Fml_S_nh))

############
## Ancova ##
############
scatterplotMatrix(~ I(1/(nh + 0.01))  + moist + Temp5_Mean, data = Sdf2, diag = "boxplot", 
                  groups = Sdf2$temp, by.group = TRUE)
xyplot(1/(nh + 0.01)  ~ moist|chamber, groups = temp, type = c("r", "p"), data = Sdf2)
xyplot(1/(nh + 0.01)  ~ Temp5_Mean|chamber, groups = temp, type = c("r", "p"), data = Sdf2)

Iml_ancv_nh <- lmer(1/(nh + 0.01) ~ temp * (moist + Temp5_Mean) + (1|chamber), data = Sdf2)
Anova(Iml_ancv_nh)
Fml_ancv_nh <- stepLmer(Iml_ancv_nh, alpha.fixed = .1)
AnvF_ancv_nh <- Anova(Fml_ancv_nh, test.statistic = "F")
AnvF_ancv_nh

# model diagnosis
plot(Fml_ancv_nh)
qqnorm(resid(Fml_ancv_nh))
qqline(resid(Fml_ancv_nh))

# visualise
par(mfrow = c(1, 2))
rtr <- function(x) 1/x - 0.01
TransVirsreg(visreg(Fml_ancv_nh, xvar = "moist", by = "temp", plot = FALSE), 
             overlay = TRUE, trans = rtr, line = list(col = c(1, 2)), ylim = c(0, .4))
TransVirsreg(visreg(Fml_ancv_nh, xvar = "Temp5_Mean", by = "temp", plot = FALSE), 
             overlay = TRUE, trans = rtr, line = list(col = c(1, 2)), ylim = c(0, .4))

## ----Stat_WTC_Lys_Ammonium_D

########
# Deep #
########
bxplts(value = "nh", data = Ddf)

# use log
m1 <- lmer(log(nh) ~ temp * time + (1|chamber), data = Ddf)
Anova(m1)
# no interaction
Iml_D_nh <- update(m1, data = Ddf2)
Anova(Iml_D_nh)

Fml_D_nh <- stepLmer(Iml_D_nh)
Anova(Fml_D_nh)
AnvF_D_nh <- Anova(Fml_D_nh, test.statistic = "F")
AnvF_D_nh

## ----Stat_WTC_Lys_Ammonium_S_Smmry
# The initial model is:
Iml_S_nh@call

Anova(Iml_S_nh)

# The final model is:
Fml_S_nh@call

# Chi
Anova(Fml_S_nh)

# F test
AnvF_S_nh

# ANCOVA
Iml_ancv_nh@call
Anova(Iml_ancv_nh)

Fml_ancv_nh@call
Anova(Fml_ancv_nh)
AnvF_ancv_nh

# visualise
par(mfrow = c(1, 2))
rtr <- function(x) 1/x - 0.01
TransVirsreg(visreg(Fml_ancv_nh, xvar = "moist", by = "temp", plot = FALSE), 
             overlay = TRUE, trans = rtr, line = list(col = c(1, 2)), 
             point = list(col = c(1, 2), cex = 1),
             ylim = c(0, .4))
TransVirsreg(visreg(Fml_ancv_nh, xvar = "Temp5_Mean", by = "temp", plot = FALSE), 
             overlay = TRUE, trans = rtr, line = list(col = c(1, 2)), ylim = c(0, .4), 
             point = list(col = c(1, 2), cex = 1))

## ----Stat_WTC_Lys_Ammonium_D_Smmry
# The initial model is:
Iml_D_nh@call

Anova(Iml_D_nh)

# The final model is:
Fml_D_nh@call

# Chi
Anova(Fml_D_nh)

# F test
AnvF_D_nh

