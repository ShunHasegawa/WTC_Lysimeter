rm(list = ls(all = TRUE))

source("R/packages.R")
source("R//functions.R")

####################################
# Process & combine AQ2 & TOC data #
####################################
# source("R//ProcessLysimeter.R")
load("Output/Data/WTC_lysimeter.RData")

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
