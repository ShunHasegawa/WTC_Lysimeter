# summary data frame
ChMean <- ddply(lysMlt, .(time, date, temp, chamber, depth, variable), Crt_SmryDF)
TrtMean <- ddply(ChMean, .(time, date, temp, depth, variable), function(x) Crt_SmryDF(x, val = "Mean"))

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
l_ply(1:7, function(x) ggsavePP(filename = fls[x], plot = TrtFg[[x]], width = 6, height = 3))

###################################
## plot all nutrients separately ##
###################################
p <- PltTempMean(TrtMean) + 
  facet_wrap( ~variable, ncol = 2, scale = "free_y")

# modify labels
ylabs <- c(expression(NO[3]^"-"-N),
           expression(NH[4]^"+"-N),
           expression(PO[4]^"3-"-P),
           expression(TOC),
           expression(TC),
           expression(IC),
           expression(TN))

pl <- facet_wrap_labeller(p, labels = ylabs)


ggsavePP(filename = "Output/Figs/WTC_LysimeterTemp", plot = pl, width = 8, height = 8)


#######################
# Figs for manuscript #
#######################
# subset Nitrate, ammonium, phosphate and TOC
df <- subsetD(TrtMean, variable %in% c("no", "nh", "po", "toc"))

# change variable and depth labels for plotting
df <- within(df, {
  variable <- factor(variable, 
                     labels = c(expression(NO[3]^"-"),
                                expression(NH[4]^"+"),
                                expression(PO[4]^"3-"),
                                expression(DOC)))
  depth <- factor(depth, labels = c("Shallow", "Deep"))
})

###########################
## df for fig sub labels ##
###########################
subLabDF <- data.frame(xv = as.Date("2013-2-2"),
                       ddply(df, .(variable), summarise, 
                             yv = max(Mean + SE, na.rm = TRUE)),
                       temp = "amb")
# Add depth, note that I want the same yv value for each depth so repeat the
# above data frame for each depth
subLabDF <- expand.grid.df(subLabDF, data.frame(depth = c("Shallow", "Deep")))

## Add labels after sorting by variable
subLabDF <- subLabDF[order(subLabDF$variable), ]
subLabDF$labels <- paste("(", c("a", "e", "b", "f", "c", "g", "d", "h"), ")", sep = "")

#######################
## df for stat table ##
#######################
# load stat summary table; note that if you need the updated result, you need to
# run Stats.R first
load("Output/Data/WTC_lysimeter_TempxTime_Stats.RData")

## compute ylength and ymax for each variable
ylengthDF <- ddply(df, .(variable), 
                   function(x) 
                     data.frame(ylength = max(x$Mean +x$SE, na.rm = TRUE) -
                                  min(x$Mean - x$SE, na.rm = TRUE),
                                ymax = max(x$Mean +x$SE, na.rm = TRUE)))
# ylength is given as the difference between max and min

## relabel Stat_TempTime to be consistent with the data df
Stat_TempTime <- within(Stat_TempTime, {
  variable <- factor(variable, 
                     levels = c("no", "nh", "po", "toc"),
                     labels = c(expression(NO[3]^"-"),
                                expression(NH[4]^"+"),
                                expression(PO[4]^"3-"),
                                expression(DOC)))
  depth <- factor(depth, levels = c("shallow", "deep"), labels = c("Shallow", "Deep"))
})

statDF <- StatPositionDF(StatRes = Stat_TempTime, 
                         variable = levels(ylengthDF$variable), 
                         ytop = ylengthDF$ymax * 1.08,
                         ylength = ylengthDF$ylength,
                         gap = 0.11)


# x position for statDF
varDepDF <- unique(data.frame(statDF[, c("variable", "depth")]))
xvalDF <- data.frame(varDepDF,
                     xval = as.Date(c("2013-7-15", "2013-11-20", 
                                      "2013-7-15", "2013-11-20", 
                                      "2013-7-15", "2013-11-20",
                                      "2013-11-20", "2013-11-20")))
statDF <- merge(statDF, xvalDF, by = c("variable", "depth"))
statDF[statDF$variable == 'NO[3]^"-"' & statDF$depth == "Deep", "yval"] <- 
  statDF[statDF$variable == 'NO[3]^"-"' & statDF$depth == "Deep", "yval"] - 210

################
## plot theme ##
################
science_theme <- theme(panel.border = element_rect(color = "black"),
                       panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       axis.text.x  = element_text(angle=45, vjust= 1, hjust = 1),
                       axis.ticks.length = unit(-.2, "lines"),
                       axis.ticks.margin = unit(.5, "lines"),
                       legend.margin = unit(0, "lines"),
                       legend.title = element_blank(),
                       legend.key = element_blank(), 
                       legend.key.width = unit(2.5, "lines"),
                       legend.background = element_rect(fill = "transparent", colour = NA))

##################
## creat a plot ##
##################
p <- ggplot(df, aes(x = date, y = Mean, group = temp))

pl <- p + 
  geom_vline(xintercept = as.numeric(as.Date("2013-3-18")), 
             linetype = "dashed", col = "black") +
  geom_line(aes(linetype = temp), position = position_dodge(10)) + 
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                width = 0, size = .4,
                position = position_dodge(10)) + 
  geom_point(aes(fill = temp),
             shape = 21,
             size = 3,
             position = position_dodge(10)) +
  labs(x = "Month", y = expression(Dissolved~nutrients~'in'~soil~solution~(mg~l^"-1"))) +
  scale_x_date(breaks= date_breaks("3 month"),
               labels = date_format("%b-%y"),
               limits = as.Date(c("2013-2-1", "2014-2-15"))) +
  scale_fill_manual(values = c("black", "white"), 
                    labels = c("Ambient", "eTemp")) +
  scale_linetype_manual(values = c("solid", "dashed"), 
                        labels = c("Ambient", "eTemp")) +
  geom_text(aes(x = xv, y = yv * .95, label = labels), fontface = "bold", data = subLabDF) +
  facet_grid(variable ~ depth, scale = "free_y", labeller = label_parsed) +
  science_theme +
  geom_text(data = subset(statDF, predictor != ""), 
            aes(x = xval, y = yval, label = predictor),
            size = 3, hjust = 1, parse = TRUE) +
  # unless remove [" "] with predictor != "", labels will be messed up due to
  # this empty level
  geom_text(data = statDF, aes(x = xval + 40, y = yval, label = p), size = 3, parse = TRUE)  +
  theme(legend.position = c(.87, .95))
ggsavePP(plot = pl, filename = "Output/Figs/Manuscript/WTC_Lysimeter", 
         width = 6.65, height = 7.5)
