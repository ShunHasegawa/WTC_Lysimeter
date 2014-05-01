rm(list = ls(all = TRUE))

library(car)
library(gmodels)
library(lme4)
library(lubridate)
library(MASS)
library(nlme)
library(packrat)
library(plyr)
library(reshape)
library(xlsx)
library(contrast)
library(effects)
library(ggplot2)

source("R//functions.R")

####################################
# Process & combine AQ2 & TOC data #
####################################
# source("R//ProcessLysimeter.R")
load("Output/Data/WTC_lysimeter.RData")

# time
lys$time <- as.numeric(factor(lys$Date)) # make sure Date is date format!!

# temp
lys$temp <- factor(ifelse(lys$chamber %in% seq(2, 12, 2), "elev", "amb"))

# remove non-informative rows (rows with all na for nutrient vaeiables)
ntrs <- c("no", "nh", "po", "toc", "tc", "ic", "tn")

lys <- lys[apply(lys[,ntrs], 1, function(x) !all(is.na(x))), ]

#################
# Summary table #
#################
lysMlt <- melt(lys, id = c("time", "Date", "temp", "chamber", "location", "depth"), na.rm = TRUE)
lysMlt$variable <- factor(lysMlt$variable, levels = c(ntrs)) # change the level order of variable 

# chamber summary table & mean
ChSmmryTbl <- dlply(lysMlt, .(variable, depth), function(x) CreateTable(x, fac = "chamber"))
ChMean <- ddply(lysMlt, .(time, Date, temp, chamber,depth, variable), summarise, value = mean(value, na.rm = TRUE)) 

# treat summary table $ mean
TrtSmmryTbl <- dlply(ChMean, .(variable, depth), function(x) CreateTable(x, fac = "temp"))

## create xcel workbook ##
wb <- createWorkbook()

# worksheet for rowdata and rowdata without outlier
sheet <- createSheet(wb,sheetName="row_data")

addDataFrame(lys, sheet, showNA=TRUE, row.names=FALSE, characterNA="NA")

# worksheets for chamber summary
shnames <- paste("ChamberMean", c("Nitrate", "Ammonium", "Phosphate", "TotalOrganicC", "TotalC", "InorganicC", "TotalN"), sep = "_")

l_ply(1:7, function(x) {
  lnames <- paste(ntrs[x], c("shallow", "deep"), sep = ".") 
  # names of the required data set in the list
  
  crSheet(sheetname = shnames[x], 
          datasetS = ChSmmryTbl[[ lnames[1] ]], 
          datasetD = ChSmmryTbl[[ lnames[2] ]])
})




# worksheets for temp trt summary
shnames <- paste("Temp_mean.", c("Nitrification", "N_mineralisation","P_mineralisation"), sep = "")
l_ply(1:3, function(x) crSheet(sheetname = shnames[x], dataset = TrtSmmryTbl[[x]]))
ChSmmryTbl[["no.deep"]]
#save file
saveWorkbook(wb,"Output/Table/WTC_Mineralisation.xlsx")

