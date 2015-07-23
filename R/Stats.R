#############################
# Merge with soil variables #
#############################
diff(unique(lys$date))
# use 1month(28 days) mean

# soil variables
load("Data/WTC_soilMoistTemp_Chamber_DailySummary.RData")
# restructure
names(soilChmSmry)[c(1, 2, 4)] <- c("date", "chamber", "probe")
SoilChMlt <- melt(soilChmSmry, id = c("date", "chamber", "temp", "probe"))
SoilCh <- cast(SoilChMlt, date + chamber + temp ~ probe + variable)

# mean of soil vars during incubation period
SoilIncSampMean <- function(insertion, sampling, Chm, data = SoilCh){
  a <- subset(data, date >= insertion & date <= sampling & chamber == Chm)
  vars <- names(a)[which(!names(a) %in% c("date", "chamber", "temp"))]
  b <- ddply(a, .(chamber), function(x) colMeans(x[, vars], na.rm = TRUE))
  return(cbind(insertion, sampling, b))
}


# chamber mean
head(lys)
Lys_ChMean <- ddply(lys, .(date, time, chamber, depth, temp),  
                    function(x) colMeans(x[, c("no", "po", "nh", "toc", "tc", "ic", "tn")], 
                                         na.rm = TRUE))


# compute means of soil variabes for each incubation period (28 days), and merge
# with nutrient df
names(Lys_ChMean)
Lys_DF <- ddply(Lys_ChMean, .(time, date, chamber, depth, temp, no, nh, po, toc, tc, ic),
                 function(x) SoilIncSampMean(insertion= x$date - 14, sampling= x$date,
                                             Chm = x$chamber))
Lys_DF$moist <- Lys_DF$SoilVW_5_25_Mean

# df for shallow and deep
Sdf <- subsetD(Lys_DF, depth == "shallow")
Ddf <- subsetD(Lys_DF, depth == "deep")

# visudally check if the function works
p <- ggplot(SoilCh, aes(x = date, y = SoilVW_5_25_Mean))
p2 <- p + 
  geom_line() +
  geom_point(data = Ddf, aes(x = date - 14, y = SoilVW_5_25_Mean), 
             col = "red", size = 2)+
  facet_wrap( ~ chamber)+
  geom_vline(xintercept = as.numeric(unique(Ddf$date)), linetype = "dashed") +
  geom_vline(xintercept = as.numeric(unique(Ddf$date)) - 28, linetype = "dashed")
p2  

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

############
## ANCOVA ##
############
# change row names

AncvLst <- list('no' = AnvF_ancv_no, 
                'nh' = AnvF_ancv_nh, 
                'po' = AnvF_ancv_po,
                'toc' = AnvF_ancv_toc)
AncvRes <- AncvSmmryTbl(AncvRes = AncvLst, predictor = row.names(Anova(Iml_ancv_no)))
write.csv(AncvRes,  file = "Output/Table/SummaryANCOVA.csv", row.names = FALSE)
