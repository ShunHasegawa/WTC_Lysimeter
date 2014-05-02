# summary data frame
ChMean <- ddply(lysMlt, .(time, Date, temp, chamber, depth, variable), Crt_SmryDF)
TrtMean <- ddply(ChMean, .(time, Date, temp, depth, variable), function(x) Crt_SmryDF(x, val = "Mean"))

# Create figures
palette(c("blue2", "goldenrod1", "firebrick2", "chartreuse4", "deepskyblue1", "darkorange1", 
          "darkorchid3", "darkgrey", "mediumpurple1", "orangered2", "chocolate", "burlywood3"))


theme_set(theme_bw())

###################################
## plot each nutrient separately ##
###################################
vars <- c("Nitrate", "Ammoinum", "Phosphate", "TOC", "TC", "IC", "TN")

# Chamber
ChFg <- dlply(ChMean, .(variable), PltChMean)
fls <- paste("Output//Figs/WTC_LysimeterChamber_", vars, sep = "")

# save as pdf and png
l_ply(1:7, function(x) ggsavePP(filename = fls[x], plot = ChFg[[x]], width = 6, height = 6))

# Temp trt
TrtFg <- dlply(TrtMean, .(variable), PltTempMean)
fls <- paste("Output//Figs/WTC_LysimeterTempTrt_", vars, sep = "")

# save as pdf and png
l_ply(1:7, function(x) ggsavePP(filename = fls[x], plot = TrtFg[[x]], width = 6, height = 6))

###################################
## plot all nutrients separately ##
###################################
ntrs
ylabs <- list(
  'no' = expression(NO[3]^"-"-N),
  'nh' = expression(NH[4]^"+"-N),
  'po' = expression(PO[4]^"3-"-P),
  'toc' = "TOC", 
  'tc' = "TC", 
  'ic' = "IC",
  'tn' = "TN")


ylab_label <- function(variable, value){
  return(ylabs[value])
}

p <- PltTempMean(TrtMean) + 
  facet_wrap( ~variable, ncol = 2, scale = "free_y")
?facet_wrap

