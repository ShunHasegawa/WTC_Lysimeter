###############
# Process AQ2 #
###############
source("R//ProcessAQ2.R")

###############
# Process TOC #
###############
source("R//ProcessTOC.R")

#######################
# Merge all data sets #
#######################

# load("Output/Data/aq2Data.RData")
# load("Output//Data//tocDat.RData")

unique(aqDat$Date) # 2013-03-26
unique(tocDF$Date) # 2013-03-25

# change the date from 2013-03-25 to 2013-03-26 in aqDate
aqDat$Date <- as.Date(recode(as.character(aqDat$Date), "'2013-03-26' = '2013-03-25'"))

##################################################################
# combien with Feb2013 data which is stored in a different place #
##################################################################
prRes <- read.csv("Data/result.csv")
# I only need feb-2013 data from this dataset
feb13 <- subset(prRes, date == "15-Feb-13")
feb13$Date <- as.Date(as.character(feb13$date), format= "%d-%b-%y")



# combine aq2 & toc
tocAq <- merge(aqDat, tocDF, by = c("Date", "chamber", "location", "depth"), all = TRUE)

# combine this with feb2013 data
lys <- rbind.fill(feb13[names(tocAq)], tocAq)

# 2013-02-22 12.D.1 is actually from 2013-02-15
# so replace this with 2013-02-15 12.D.1 because Nitrate value 
# in this was corrected by CCV
lys$no[which(lys$chamber == "12" & 
               lys$location == "1" &
               lys$depth == "deep" &
               lys$Date == as.Date("2013-02-15"))] <- 
  lys$no[which(lys$chamber == "12"
               & lys$location == "1" & 
                 lys$depth == "deep" &
                 lys$Date == as.Date("2013-02-22"))]

# remove 2013-02-22 row
lys <- lys[-which(lys$chamber == "12" & 
                    lys$location == "1" & 
                    lys$depth == "deep" & 
                    lys$Date == as.Date("2013-02-22")),]

lys <- droplevels(lys)
lys$chamber <- factor(ifelse(as.numeric(lys$chamber) < 10, 
                             paste("0", lys$chamber, sep = ""), 
                             lys$chamber))
lys$location <- factor(lys$location)
# save
save(lys, file = "Output/Data/WTC_lysimeter.RData")
