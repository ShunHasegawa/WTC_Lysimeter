rm(list = ls(all = TRUE))

library(plyr)
library(lubridate)
library(reshape)

#############
# Correct P #
#############
# correct negative values
CrrctNgtv <- function(filename){
  d <- read.csv(paste("Data//AQ2//NeedToCorrectP/", filename, sep = ""))
  if(any(d$Result < 0)) d$Result <- d$Result - min(d$Result)
  return(d)
}

pfiles <- dir("Data//AQ2//NeedToCorrectP")

l_ply(pfiles, function(x) write.csv(CrrctNgtv(filename = x), 
            paste("Data//AQ2//ReadyToProcess/", "NgtvCorrected_", x, sep = ""),
            row.names = TRUE))
