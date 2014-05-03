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

ntrs <- c("no", "nh", "po", "toc", "tc", "ic", "tn")
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
