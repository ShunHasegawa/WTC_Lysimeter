## ----Stat_WTC_Lys_Nitrate_S

###########
# Shallow #
###########
bxplts(value = "no", data = Sdf)

# use log
Iml_S_no <- lmer(log(no) ~ temp * time + (1|chamber), data = Sdf)
Anova(Iml_S_no)

Fml_S_no <- stepLmer(Iml_S_no)
Anova(Fml_S_no)
AnvF_S_no <- Anova(Fml_S_no, test.statistic = "F")

# model diagnosis
plot(Fml_S_no)
qqnorm(resid(Fml_S_no))
qqline(resid(Fml_S_no))

## ----Stat_WTC_Lys_Nitrate_D

########
# Deep #
########

bxplts(value = "no", data = Ddf)

# use power(1/3)
Iml_D_no <- lmer(no^(1/3) ~ temp * time + (1|chamber), data = Ddf)
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
