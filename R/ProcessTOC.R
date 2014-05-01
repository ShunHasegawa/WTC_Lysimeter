rm(list = ls(all = TRUE))

library(car)


fls <- dir("Data//TOC", full.names= TRUE)

rowdf <- ldply(fls, function(x) read.table( x, skip = 11, fill = TRUE, sep = "\t", header = TRUE, 
                                         colClasses = c("Sample.ID" = "character")))

# remove standard & control
unique(rowdf$Type)
Ndf <- subset(rowdf, Type == "Unknown")

################################
# Inspect and organise dataset #
################################

## Sample.ID ##
unique(Ndf$Sample.ID)
  # same samples were rerun becase their TN were higher than the topstandard

# check rerun samples
Ndf[grep("RERUN", Ndf$Sample.ID), ]
  # 24-May-2013 2.s.1, 4.s.2, 10.s.1 were measured twice
subset(Ndf, Sample.ID %in% c("2.S.1", "4.S.2", "10.S.1") & Sample.Name == "24-May-2013")
  # They were pretty much same as the original values so just use the original values and remove rerun smaples

# remove rerun samples
Ndf <- Ndf[-grep("RERUN", Ndf$Sample.ID), ]


## Sample.Name ##
unique(Ndf$Sample.Name)
 # There is "Untitled"
Ndf[Ndf$Sample.Name == "Untitled", ]
  # This is from 28-Jun-2013 casue it wasn't measured properly first time

subset(Ndf, Sample.Name %in% c("28-Jun-2013", "Untitled") & Sample.ID == "12.D.1")
  # also remove the 1st measurement for this
  # and also fix "Untitled"

Ndf <- Ndf[-which(Ndf$Sample.Name == "28-Jun-2013" &  Ndf$Sample.ID == "12.D.1"), ]
Ndf$Sample.Name[which(Ndf$Sample.Name == "Untitled")] <- "28-Jun-2013"

# check if these are fixed properly
subset(Ndf, Sample.Name %in% c("28-Jun-2013", "Untitled") & Sample.ID == "12.D.1")

unique(Ndf$Sample.Name)
  #  need to replace 8-Oct-13 with 8-Oct-2013
Ndf$Sample.Name <- recode(Ndf$Sample.Name, "'8-Oct-13' =  '08-Oct-2013'")
unique(Ndf$Sample.Name)

# turn Sample.Name into date
Ndf$Date <- as.Date(dmy(Ndf$Sample.Name))

####################################
# Finalise the shape of data frame #
####################################
# create ring, depth factor
a <- ldply(strsplit(Ndf$Sample.ID, split = "[.]"))
colnames(a) <- c("chamber", "depth", "location")

# merge
Ndf <- cbind(Ndf, a)

# extract required coluns
names(Ndf)
tocDF <- Ndf[c("Result.TOC.", "Result.TC.", "Result.IC.", "Result.TN.", "Date", "chamber", "depth", "location")]
names(tocDF)[1:4] <- c("toc", "tc", "ic", "tn")

##################################################################
# combien with Feb2013 data which is stored in a different place #
##################################################################
prRes <- read.csv("Data/result.csv")
# I only need feb-2013 data from this dataset
feb13 <- subset(prRes, date == "15-Feb-13")
feb13$Date <- as.Date(dmy(feb13$date))
names(tocDF)
names(feb13)

# combine
tocDat <- rbind.fill(tocDF, feb13[names(tocDF)])
tocDat$depth <- factor(ifelse(tocDat$depth %in% c("S", "shallow"), "shallow", "deep"))

save(tocDat, file = "Output/Data/tocDat.RData")
