###########################################
# COrrecte negative values in AQ2 results #
###########################################

CrrctNgtv <- function(filename){
  d <- read.csv(paste("Data//AQ2//NeedToCorrectP/", filename, sep = ""))
  if(any(d$Result < 0)) d$Result <- d$Result - min(d$Result)
  return(d)
}

###################################
# Coorect AQ2 result based on CCV #
###################################

# subset dataset beween each ccv
Crrct.ccv <- function(x, data, ccval = 7.5){
  # subset dataset between given ccvs
  ccv.ps <- grep("V$", as.character(data$Sample.ID))
  
  ts <- data[c(ccv.ps[x] : ccv.ps[x + 1]),]
  trng <- range(ts$times)
  xv <- as.numeric(trng)
  yv <- ts$Result[ts$times == trng[1] | ts$times == trng[2]] 
  
  b <- ccval * (1 / yv[1] - 1 / yv[2]) / (xv[1] - xv[2])
  a <- ccval / yv[1] - b * xv[1]
  
  ts$Result <- ts$Result * (a + b * as.numeric(ts$times))
  ts$times <- NULL
  return(ts)
}

# applied the function above for each subsets of ccvs
Crrtct.ccv.df <- function(filename, ccval = 7.5){
  data <- read.csv(paste("Data/AQ2/NeedToCorrectNO/", filename, sep = ""), header = TRUE)
  
  # make time factor as numeric
  a <- sapply(as.character(data$Time), strsplit, " ")
  b <- ldply(a, function(x) paste(x[c(5, 2, 3, 4)], collapse = "/"))
  
  b$V1 <- ymd_hms(b$V1)
  
  names(b) <- c("Time", "times")
  
  # merge
  mrg.res <- merge(data, b, by = "Time")
  
  # reorder accoding to time
  mrg.res <- mrg.res[order(mrg.res$times), ]
  
  # add the latest ccv value at the end of data frame to make sure all measured values are placed between ccvs
  # if the last line is not ccv, vlues after last ccv will be turned into NA
  ccv.ps <- grep("V$", as.character(mrg.res$Sample.ID))
  lstTime <- mrg.res$times[nrow(mrg.res)]
  mrg.res[nrow(mrg.res) + 1, ] <- mrg.res[max(ccv.ps), ] 
  mrg.res$times[nrow(mrg.res)] <- lstTime + 1 # make the last time latest by adding 1 to the actual latest time
  
  ccv.ps <- grep("V$", as.character(mrg.res$Sample.ID)) # update ccv.position
  
  # re-caluculate the results
  a <- ldply(1:(length(ccv.ps)-1), function(x) Crrct.ccv(x, data = mrg.res, ccval))
  return(a)
}