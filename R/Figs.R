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
  "TOC", "TC", "IC", "TN")


ylab_label <- function(variable, value){
  return(ylabs[value])
}


p <- ggplot(TrtMean, aes_string(x = "Date", y = "Mean", col = "temp"))
p2 <- p + geom_line(size = 1) +
  geom_errorbar(aes_string(ymin = "Mean - SE", ymax = "Mean + SE", col = "temp") , width = 5) + 
  labs(x = "Time", y = expression((mg~l^"-1"))) +
  scale_color_manual(values = c("blue", "red"), 
                     "Temp trt", labels = c("Ambient", "eTemp")) +
  facet_grid(depth ~ variable, scales= "free_y", labeller= ylab_label) +
  facet_wrap(variable ~ depth, nrow = 4, ncol = 4, scales= "free_y")
?facet_wrap



return(p2)

p <- 
  
  
  
  
  (data, colfactor = "temp") +
  scale_color_manual(values = c("blue", "red"), "Temp trt", 
                     labels = c("Ambient", "eTemp")) +
  facet_grid(depth~. )


l <- PltTempMean(TrtMean) +
  facet_grid(depth~variable)
+
  
  
  


p <- PltMean(TrtMean, colfactor = "temp") +
  scale_color_manual(values = c("blue", "red"), "Temp trt", 
                     labels = c("Ambient", "eTemp")) +
  


