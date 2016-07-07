kirastat <- function(inputdata, pct_sihat, pct_isipadu, saiz_petak, bil_petak){
  #SET JADUAL KOSONG UNTUK KUMP & KESELURUHAN
  Jadual <- matrix(nrow = 0, ncol = 12)
  Jadual <- as.data.frame(Jadual)
  names(Jadual) <- 1:12
  
  #SET DCAT ONE BAT VOLT SBG NUM
  inputdata$one <- as.character(inputdata$one)
  inputdata$dcat <- as.character(inputdata$dcat)
  inputdata$bat <- as.character(inputdata$bat)
  inputdata$volt <- as.character(inputdata$volt)
  
  inputdata$one <- as.integer(inputdata$one)
  inputdata$dcat <- as.integer(inputdata$dcat)
  inputdata$bat <- as.double(inputdata$bat)
  inputdata$volt <- as.double(inputdata$volt)
  
  #MULAKAN KIRAAN
  bil <- tapply(inputdata$one, inputdata$dcat, sum)
  bil <- data.frame(bil)
  bil_bersih <- bil*pct_sihat
  bil_bersih <- data.frame(bil_bersih)
  luas_pangkal <- tapply(inputdata$bat, inputdata$dcat, sum)
  luas_pangkal <- data.frame(luas_pangkal)
  isipadu <- tapply(inputdata$volt, inputdata$dcat, sum)
  isipadu <- data.frame(isipadu)
  isipadu_bersih <- isipadu * pct_isipadu
  semua <- cbind(bil, bil_bersih, isipadu, isipadu_bersih, luas_pangkal)
  
  #kira nilai metrik per hektar
  semua_ha <- semua/(saiz_petak*bil_petak)
  semua_ha <- round(semua_ha, 2)
  semua_ha <- t(semua_ha)
  cn <- colnames(semua_ha)
  semua_ha <- data.frame(semua_ha)
  names(semua_ha) <- cn
  
  library(dplyr)
  Jadual <- rbind.fill(Jadual,semua_ha)
  #row.names(Jadual) <- c("bil","bil_bersih","isipadu","isipadu_bersih","keluasan_pangkal")
  #Jadual[is.na(Jadual)] <- 0
  #Jadual <- round(Jadual, 2)
  
  return(Jadual)
}