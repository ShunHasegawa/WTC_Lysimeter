## ----Stat_WTC_Lys_TOC_S

###########
# Shallow #
###########
range(Sdf$toc, na.rm = TRUE)

bxplts(value = "toc", ofst = 1, data = Sdf)

# remove lower outlier
nrow(subset(Sdf, toc == 0))
  # there's only one 0; remove this

tocRmOl <- subset(Sdf, toc > 0)

bxplts(value = "toc", data = tocRmOl)
  # use inverse

# The initial model is
Iml_S <- lmer(1/toc ~ temp * time + (1|chamber) + (1|id), data = tocRmOl)
Anova(Iml_S)

# The final model is
Fml_S <- stepLmer(Iml_S)
Anova(Fml_S)
AnvF_doc_S <- Anova(Fml_S, test.statistic = "F")
AnvF_doc_S

summary(Fml_S)

plot(allEffects(Fml_S))

# model diagnosis
plot(Fml_S)
qqnorm(resid(Fml_S))
qqline(resid(Fml_S))

## ----Stat_WTC_Lys_TOC_D

########
# Deep #
########
range(Ddf$toc, na.rm = TRUE)
bxplts(value = "toc", data = Ddf)
  # log looks better

# The initial model is
Iml_D <- lmer(log(toc) ~ temp * time + (1|chamber) + (1|id), data = Ddf)
Anova(Iml_D)

# The final model is
Fml_D <- stepLmer(Iml_D)
Anova(Fml_D)
AnvF_doc_D <- Anova(Fml_D, test.statistic = "F")
AnvF_doc_D

summary(Fml_D)

plot(allEffects(Fml_D))

# model diagnosis
plot(Fml_D)
qqnorm(resid(Fml_D))
qqline(resid(Fml_D))

## ----Stat_WTC_Lys_TOC_S_Smmry
# The initial model is:
Iml_S@call

Anova(Iml_S)

# The final model is:
Fml_S@call

Anova(Fml_S)
AnvF_doc_S

## ----Stat_WTC_Lys_TOC_D_Smmry
# The initial model is:
Iml_D@call

Anova(Iml_D)

# The final model is:
Fml_D@call

Anova(Fml_D)
AnvF_doc_D
