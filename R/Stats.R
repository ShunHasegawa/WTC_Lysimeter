# chamber mean
head(lys)
Lys_DF <- ddply(lys, .(date, time, chamber, depth, temp), 
                function(x) colMeans(x[, c("no", "po", "nh", "toc", "tc", "ic", "tn")], 
                                     na.rm = TRUE))

# df for shallow and deep
Sdf <- subsetD(Lys_DF, depth == "shallow")
Ddf <- subsetD(Lys_DF, depth == "deep")

# df without time1 (pre-plant installation)
Sdf2 <- subsetD(Sdf, time != 1)
Ddf2 <- subsetD(Ddf, time != 1)

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
# source("R/Stat_tc.R")

######
# IC #
######
# source("R/Stat_IC.R")

######
# TN #
######
# source("R/Stat_tn.R")

######################
# Summary stat table #
######################
summary(lys)
# create stat summary table for LMM with temp and time
TempTimeStatList <- list('no_shallow' = AnvF_S_no,
                         'no_deep'    = AnvF_D_no,
                        'nh_shallow' = AnvF_S_nh,
                        'nh_deep'    = AnvF_D_nh,
                        'po_shallow' = AnvF_S_po,
                        'po_deep'    = AnvF_D_po,
                        'toc_shallow'= AnvF_S_toc,
                        'toc_deep'   = AnvF_D_toc)

Stat_TempTime <- ldply(names(TempTimeStatList), 
                      function(x) StatTable(TempTimeStatList[[x]], variable = x))
# split variable to variable and depth
splitVar <- ldply(strsplit(as.character(Stat_TempTime$variable), split = "_"))

Stat_TempTime <- within(Stat_TempTime, {
  variable <- factor(splitVar[, 1])
  depth <- factor(splitVar[, 2])
})

save(Stat_TempTime, file = "Output/Data/WTC_lysimeter_TempxTime_Stats.RData")
