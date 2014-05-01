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

# remove non-informative rows (rows with all na for nutrient vaeiables)
ntrs <- c("no", "po", "nh", "toc", "tc", "ic", "tn")

lys <- lys[apply(lys[,ntrs], 1, function(x) !all(is.na(x))), ]

#################
# Summary table #
#################
lysMlt <- melt(lys, id = c("Date", "chamber", "location", "depth"), na.rm = TRUE)
summary(lysMlt)

# chamber summary table & mean
Ch
