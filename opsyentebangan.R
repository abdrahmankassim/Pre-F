opsyen <- function(file, pd){
  library(dplyr)
  attach(file)
  
  file[,13] <- as.double(as.character(file[,13]))
  file[,8] <- as.double(as.character(file[,8]))
  file[,5] <- as.double(as.character(file[,5]))
  #dtasubset <- subset(file[,which(file[,13] >= pd & file[,8] >= 32 & file[,5] >= 28 & file[,5] <= 85)])
  dtasubset <- filter(file,file[,13] >= pd & file[,12] >= 32 & file[,5] >= 28 & file[,5] <= 85)
  return(dtasubset)
  detach(file)
}