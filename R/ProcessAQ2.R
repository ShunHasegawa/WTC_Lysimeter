rm(list = ls(all = TRUE))

library(plyr)
library(lubridate)
library(reshape)

source("R/functions.R")

#############
# Correct P #
#############
# correct negative values
pfiles <- dir("Data//AQ2//NeedToCorrectP")

l_ply(pfiles, function(x) write.csv(CrrctNgtv(filename = x), 
            paste("Data//AQ2//ReadyToProcess/", "NgtvCorrected_", x, sep = ""),
            row.names = TRUE))

##############
# Correct NO #
##############
nofiles <- dir("Data//AQ2//NeedToCorrectNO")

l_ply(nofiles[c(1,3)], function(x) 
  write.csv(Crrtct.ccv.df(x), 
            paste("Data//AQ2//ReadyToProcess/", "CCV_Corrected_", x, sep = ""),
            row.names = TRUE))


Dec2013 <- read.csv("Data//AQ2//NeedToCorrectNO//WTC.Lysimeter_19DEC2014_NO_x4.dilution.csv")
  # add dilution factor of 4
Dec2013$Result <- Dec2013$Result * 4

write.csv(Dec2013, paste("Data//AQ2//ReadyToProcess/", "DilutionCorrected_", nofiles[2], sep = ""),
                         row.names = TRUE)

