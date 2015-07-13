## ----Stat_WTC_Lys_Phosphate_S

###########
# Shallow #
###########
range(Sdf$po)
bxplts(value = "po", ofst = 0.001, data = Sdf)
# power(1/3) looks better

Iml_S_po <- lmer((po + 0.01)^(1/3) ~ temp * time + (1|chamber), data = Sdf)
Anova(Iml_S_po)

Fml_S_po <- stepLmer(Iml_S_po)
Anova(Fml_S_po)
AnvF_S_po <- Anova(Fml_S_po, test.statistic = "F")
AnvF_S_po

# model diagnosis
plot(Fml_S_po)
qqnorm(resid(Fml_S_po))
qqline(resid(Fml_S_po))

## ----Stat_WTC_Lys_Phosphate_D

########
# Deep #
########
range(Ddf$po)
bxplts(value = "po", data = Ddf)

# use log
Iml_D_po <- lmer(log(po) ~ temp * time + (1|chamber), data = Ddf)
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
