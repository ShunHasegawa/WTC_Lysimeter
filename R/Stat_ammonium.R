## ----Stat_WTC_Lys_Ammonium_S

###########
# Shallow #
###########
bxplts(value = "nh", ofst= 0.01, data = Sdf)
  # inverse looks better

# use inverse
Iml_S_nh <- lmer(1/(nh + 0.01) ~ temp * time + (1|chamber), data = Sdf)
Anova(Iml_S_nh)

Fml_S_nh <- stepLmer(Iml_S_nh)
Anova(Fml_S_nh)
AnvF_S_nh <- Anova(Fml_S_nh, test.statistic = "F")
AnvF_S_nh

# model diagnosis
plot(Fml_S_nh)
qqnorm(resid(Fml_S_nh))
qqline(resid(Fml_S_nh))

## ----Stat_WTC_Lys_Ammonium_D

########
# Deep #
########
bxplts(value = "nh", data = Ddf)

# use log
Iml_D_nh <- lmer(log(nh) ~ temp * time + (1|chamber), data = Ddf)
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
