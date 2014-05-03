rm(list = ls(all = TRUE))

library(car)
library(gmodels)
library(lme4)
library(lubridate)
library(MASS)
library(nlme)
library(packrat)
library(plyr)
library(reshape)
library(xlsx)
library(contrast)
library(effects)
library(ggplot2)
library(gridExtra)

source("R//functions.R")

####################################
# Process & combine AQ2 & TOC data #
####################################
# source("R//ProcessLysimeter.R")
load("Output/Data/WTC_lysimeter.RData")

# time
lys$time <- factor(as.numeric(factor(lys$Date)))
  # make sure Date is in Date format!!

# temp
lys$temp <- factor(ifelse(as.numeric(as.character(lys$chamber)) %in% seq(2, 12, 2), "elev", "amb"))

# id
lys$id <- lys$chamber:lys$location

# remove non-informative rows (rows with all na for nutrient vaeiables)
ntrs <- c("no", "nh", "po", "toc", "tc", "ic", "tn")

lys <- lys[apply(lys[,ntrs], 1, function(x) !all(is.na(x))), ]


# reorder depth factor
levels(lys$depth)
lys$depth <- factor(lys$depth, levels = c("shallow", "deep"))


#################
# Summary table #
#################
source("R//SummaryExlTable.R")

########
# Figs #
########
source("R//Figs.R")

#########
# Stats #
#########
source("R/Stats.R")
