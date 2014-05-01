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

source("R//functions.R")

####################################
# Process & combine AQ2 & TOC data #
####################################
# source("R//ProcessLysimeter.R")
load("Output/Data/WTC_lysimeter.RData")

# time
lys$time <- as.numeric(factor(lys$Date)) # make sure Date is date format!!

# temp
lys$temp <- factor(ifelse(as.numeric(as.character(lys$chamber)) %in% seq(2, 12, 2), "elev", "amb"))

# remove non-informative rows (rows with all na for nutrient vaeiables)
ntrs <- c("no", "nh", "po", "toc", "tc", "ic", "tn")

lys <- lys[apply(lys[,ntrs], 1, function(x) !all(is.na(x))), ]

#################
# Summary table #
#################
source("R//SummaryExlTable.R")

