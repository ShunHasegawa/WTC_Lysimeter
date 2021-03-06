## ----Stat_WTC_Lys_Nitrate_S

###########
# Shallow #
###########
bxplts(value = "no", data = Sdf)

# use log
m1 <- lmer(log(no) ~ temp * time + (1|chamber), data = Sdf)
Anova(m1)
# no interaction so remove time1
Iml_S_no <- update(m1, data = Sdf2)
Anova(Iml_S_no)

Fml_S_no <- stepLmer(Iml_S_no)
Anova(Fml_S_no)
AnvF_S_no <- Anova(Fml_S_no, test.statistic = "F")

# model diagnosis
plot(Fml_S_no)
qqnorm(resid(Fml_S_no))
qqline(resid(Fml_S_no))

############
## Ancova ##
############
scatterplotMatrix(~ log(no) + log(moist) + Temp5_Mean, data = Sdf2, diag = "boxplot", 
                  groups = Sdf2$temp, by.group = TRUE)
xyplot(log(no) ~ log(moist)|chamber, groups = temp, type = c("r", "p"), data = Sdf2)
xyplot(log(no) ~ Temp5_Mean|chamber, groups = temp, type = c("r", "p"), data = Sdf2)

Iml_ancv_no <- lmer(log(no) ~ temp * (moist + Temp5_Mean) + (1|chamber), data = Sdf2)
Anova(Iml_ancv_no)
# nothing is significant
Fml_ancv_no <- Iml_ancv_no
AnvF_ancv_no <- Anova(Fml_ancv_no, test.statistic = "F")
AnvF_ancv_no

# model diagnosis
plot(Fml_ancv_no)
qqnorm(resid(Fml_ancv_no))
qqline(resid(Fml_ancv_no))

## ----Stat_WTC_Lys_Nitrate_D

########
# Deep #
########

bxplts(value = "no", data = Ddf)

# use power(1/3)
m1 <- lmer(no^(1/3) ~ temp * time + (1|chamber), data = Ddf)
Anova(m1)
# no interaction so remove time1
Iml_D_no <- update(m1, data = Ddf2)
Anova(Iml_D_no)

Fml_D_no <- stepLmer(Iml_D_no)
Anova(Fml_D_no)
AnvF_D_no <- Anova(Fml_D_no, test.statistic = "F")
AnvF_D_no

# model diagnosis
plot(Fml_D_no)
qqnorm(resid(Fml_D_no))
qqline(resid(Fml_D_no))

## ----Stat_WTC_Lys_Nitrate_S_Smmry
# The initial model is:
Iml_S_no@call

Anova(Iml_S_no)

# The final model is:
Fml_S_no@call

# Chi
Anova(Fml_S_no)

# F test
AnvF_S_no

# ANCOVA
Iml_ancv_no@call
Anova(Iml_ancv_no)

Fml_ancv_no@call
Anova(Fml_ancv_no)
AnvF_ancv_no


## ----Stat_WTC_Lys_Nitrate_D_Smmry
# The initial model is:
Iml_D_no@call

Anova(Iml_D_no)

# The final model is:
Fml_D_no@call

# Chi
Anova(Fml_D_no)

# F test
AnvF_D_no
