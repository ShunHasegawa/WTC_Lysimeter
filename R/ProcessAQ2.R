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

ccvList <- nofiles[-grep("19DEC2013", nofiles)]

l_ply(ccvList, function(x) 
  write.csv(Crrtct.ccv.df(x), 
            paste("Data//AQ2//ReadyToProcess/", "CCV_Corrected_", x, sep = ""),
            row.names = TRUE))


Dec2013 <- read.csv("Data//AQ2//NeedToCorrectNO//WTC.Lysimeter_19DEC2013_NO_x4.dilution.csv")
  # add dilution factor of 4
Dec2013$Result <- Dec2013$Result * 4

write.csv(Dec2013, paste("Data//AQ2//ReadyToProcess/", "DilutionCorrected_", 
                         "WTC.Lysimeter_19DEC2013_NO_x4.dilution.csv", 
                         sep = ""),
                         row.names = TRUE)


#################################
# Process and combine all files #
#################################
fils <- dir(path = "Data/AQ2/ReadyToProcess/", pattern = ".csv$")
aqDat <- cmbn.fls(fils)
names(aqDat)[5:7] <- c("no", "po", "nh")
aqDat$depth <- factor(ifelse(aqDat$depth == "S", "shallow", "deep"))

save(aqDat, file = "Output//Data/aq2Data.RData") 

