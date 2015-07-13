rm(list = ls(all = TRUE))

source("R/packages.R")
source("R//functions.R")

####################################
# Process & combine AQ2 & TOC data #
####################################
# source("R//ProcessLysimeter.R")
load("Output/Data/WTC_lysimeter.RData")

# remove negative nh
lys$nh[lys$nh<0] <- NA
lys$date <- lys$Date
lys$Date <- NULL

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

save.image("Output/Data/AllObj.RData")