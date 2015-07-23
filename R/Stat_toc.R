## ----Stat_WTC_Lys_TOC_S

###########
# Shallow #
###########
range(Sdf$toc, na.rm = TRUE)

bxplts(value = "toc", data = Sdf)

# use log
m1 <- lmer(log(toc) ~ temp * time + (1|chamber), data = Sdf)
Anova(m1)
Anova(m1, test.statistic = "F")

# no interaction so remove time1
Iml_S_toc <- update(m1, data = Sdf2)
Anova(Iml_S_toc)

# The final model is
Fml_S_toc <- stepLmer(Iml_S_toc)
Anova(Fml_S_toc)
AnvF_S_toc <- Anova(Fml_S_toc, test.statistic = "F")
AnvF_S_toc

# model diagnosis
plot(Fml_S_toc)
qqnorm(resid(Fml_S_toc))
qqline(resid(Fml_S_toc))

# ANCOVA
scatterplotMatrix(~ log(toc) + log(moist) + Temp5_Mean, data = Sdf2, diag = "boxplot", 
                  groups = Sdf2$temp, by.group = TRUE)
xyplot(log(toc)  ~ log(moist)|chamber, groups = temp, type = c("r", "p"), data = Sdf2)
xyplot(log(toc)  ~ Temp5_Mean|chamber, groups = temp, type = c("r", "p"), data = Sdf2)

Iml_ancv_toc <- lmer(log(toc) ~ temp * (moist + Temp5_Mean) + (1|chamber), data = Sdf2)
Anova(Iml_ancv_toc)
Fml_ancv_toc <- stepLmer(Iml_ancv_toc, alpha.fixed = .1)
AnvF_ancv_toc <- Anova(Fml_ancv_toc, test.statistic = "F")
AnvF_ancv_toc

# model diagnosis
plot(Fml_ancv_toc)
# higly wedged..
qqnorm(resid(Fml_ancv_toc))
qqline(resid(Fml_ancv_toc))

# visualise
par(mfrow = c(1, 2))
TransVirsreg(visreg(Fml_ancv_toc, xvar = "moist", by = "temp", plot = FALSE), 
             overlay = TRUE, trans = exp, 
             line = list(col = c(1, 2)), 
             point = list(col = c(1, 2), cex = 1))
TransVirsreg(visreg(Fml_ancv_toc, xvar = "Temp5_Mean", by = "temp", plot = FALSE), 
             overlay = TRUE, trans = exp, 
             line = list(col = c(1, 2)), 
             point = list(col = c(1, 2), cex = 1))

## ----Stat_WTC_Lys_TOC_D

########
# Deep #
########
range(Ddf$toc)
bxplts(value = "toc", data = Ddf)
  # log looks better

# The initial model is
m1 <- lmer(log(toc) ~ temp * time + (1|chamber), data = Ddf)
Anova(m1)

# no interaction
Iml_D_toc <- update(m1, data = Ddf2)
Anova(Iml_D_toc)

# The final model is
Fml_D_toc <- stepLmer(Iml_D_toc)
Anova(Fml_D_toc)
AnvF_D_toc <- Anova(Fml_D_toc, test.statistic = "F")
AnvF_D_toc

# model diagnosis
plot(Fml_D_toc)
qqnorm(resid(Fml_D_toc))
qqline(resid(Fml_D_toc))

## ----Stat_WTC_Lys_TOC_S_Smmry
# The initial model is:
Iml_S_toc@call

Anova(Iml_S_toc)

# The final model is:
Fml_S_toc@call

# Chi
Anova(Fml_S_toc)

# F test
AnvF_S_toc

# ANCOVA
Iml_ancv_toc@call
Anova(Iml_ancv_toc)

Fml_ancv_toc@call
Anova(Fml_ancv_toc)
AnvF_ancv_toc

# visualise
par(mfrow = c(1, 2))
TransVirsreg(visreg(Fml_ancv_toc, xvar = "moist", by = "temp", plot = FALSE), 
             overlay = TRUE, trans = exp, 
             line = list(col = c(1, 2)), 
             point = list(col = c(1, 2), cex = 1))
TransVirsreg(visreg(Fml_ancv_toc, xvar = "Temp5_Mean", by = "temp", plot = FALSE), 
             overlay = TRUE, trans = exp, 
             line = list(col = c(1, 2)), 
             point = list(col = c(1, 2), cex = 1))

## ----Stat_WTC_Lys_TOC_D_Smmry
# The initial model is:
Iml_D_toc@call

Anova(Iml_D_toc)

# The final model is:
Fml_D_toc@call

# Chi
Anova(Fml_D_toc)

# F test
AnvF_D_toc
