ntrs <- c("no", "nh", "po", "toc", "tc", "ic", "tn")

# Melt data frame
lysMlt <- melt(lys, id = c("time", "date", "temp", "chamber", "location", "depth", "id"), na.rm = TRUE)
lysMlt$variable <- factor(lysMlt$variable, levels = c(ntrs)) # change the level order of variable 

# chamber summary table & mean
ChSmmryTbl <- dlply(lysMlt, .(variable, depth), function(x) CreateTable(x, fac = "chamber"))
ChMean <- ddply(lysMlt, .(time, date, temp, chamber,depth, variable), summarise, value = mean(value, na.rm = TRUE)) 

# treat summary table $ mean
TrtSmmryTbl <- dlply(ChMean, .(variable, depth), function(x) CreateTable(x, fac = "temp"))

########################
# create xcel workbook #
########################
wb <- createWorkbook()

# worksheet for rawdata
sheet <- createSheet(wb,sheetName="raw_data")
addDataFrame(lys, sheet, showNA=TRUE, row.names=FALSE, characterNA="NA")

# worksheets for chamber summary
vars <- c("Nitrate", "Ammonium", "Phosphate", "TotalOrganicC", "TotalC", "InorganicC", "TotalN")
shnames <- paste("ChamberMean", vars, sep = "_")
MltcrSheet(tbl = ChSmmryTbl, shnames = shnames, ntrs = ntrs)

# worksheets for temp trt summary
shnames <- paste("Temp_mean.", vars, sep = "_")
MltcrSheet(tbl = TrtSmmryTbl, shnames = shnames, ntrs = ntrs)

# save file
saveWorkbook(wb,"Output/Table/WTC_Lysimeter.xlsx")