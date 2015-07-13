## ----Stat_WTC_Lys_TOC_S

###########
# Shallow #
###########
range(Sdf$toc, na.rm = TRUE)

bxplts(value = "toc", data = Sdf)

# use log
Iml_S_toc <- lmer(log(toc) ~ temp * time + (1|chamber), data = Sdf)
Anova(Iml_S_toc)
Anova(Iml_S_toc, test.statistic = "F")

# The final model is
Fml_S_toc <- stepLmer(Iml_S_toc)
Anova(Fml_S_toc)
AnvF_S_toc <- Anova(Fml_S_toc, test.statistic = "F")
AnvF_S_toc

# model diagnosis
plot(Fml_S_toc)
qqnorm(resid(Fml_S_toc))
qqline(resid(Fml_S_toc))

## ----Stat_WTC_Lys_TOC_D

########
# Deep #
########
range(Ddf$toc)
bxplts(value = "toc", data = Ddf)
  # log looks better

# The initial model is
Iml_D_toc <- lmer(log(toc) ~ temp * time + (1|chamber), data = Ddf)
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
