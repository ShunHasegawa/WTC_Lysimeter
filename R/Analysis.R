rm(list = ls(all = TRUE))

####################################
# Process & combine AQ2 & TOC data #
####################################
load("Output/Data/aq2Data.RData")
load("Output//Data//tocDat.RData")

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

# what is data 2013-02-22????