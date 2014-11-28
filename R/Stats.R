# df for shallow and deep
Sdf <- subsetD(lys, depth == "shallow")
Ddf <- subsetD(lys, depth == "deep")

###########
# Nitrate #
###########
source("R//Stat_nitrate.R")

############
# Ammonium #
############
source("R//Stat_ammonium.R")

#############
# Phosphate #
#############
source("R//Stat_phosphate.R")

#######
# TOC #
#######
source("R//Stat_toc.R")

######
# TC #
######
source("R/Stat_tc.R")

######
# IC #
######
source("R/Stat_IC.R")

######
# TN #
######
source("R/Stat_tn.R")

