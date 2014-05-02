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
ChFg <- dlply(ChMean, .(variable), PltMean)
fls <- paste("Output//Figs/WTC_Lysimeterchamber_", vars, sep = "")

# save as pdf and png
l_ply(1:7, function(x) ggsavePP(filename = fls[x], plot = ChFg[[x]], width = 6, height = 6))

# Temp trt
