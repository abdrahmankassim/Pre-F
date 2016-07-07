operasi <- function(file)
{
  # DATA PREPARATION ------------------------------------------------------------------------------
  
  # JADUAL 1 -------------------------------------------------------------------------------------- 
  
  attach(file)
  
  #kiraan individu pokok
  #bilangan petak mengikut saiz
  
  one <- rep(1,dim(file)[1])
  
  dcat <- as.numeric(cut(dbh,c(15, 30, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90,250),labels = 1:12))
  bat <- dbh^2*pi/40000
  
  # bilangan balak mengikut saiz dbh
  bil_log <- ifelse(dcat>1 & dcat<=5, 2*5, 0)
  bil_log <- ifelse(dcat>5 & dcat<=8, 3*5, bil_log)
  bil_log <- ifelse(dcat>8, 4*5, bil_log)
  
  # kiraan isipadu
  volt <- bat*bil_log*0.65
  
  # kiraan isipadu bersih
  volt_bersih <- ifelse(dbh>=60, volt*0.7, 0)
  volt_bersih <- ifelse(dbh<60, volt*0.6, volt_bersih)
  
  # kiraan mengikut julat dbh 
  
  # pct bersih = 1-peratus kerosakan 
  pct_sihat <-  c(0.5, rep(0.6,3), 0.7, rep(0.8,7))
  
  # isipadu bersih = 1- peratus kerosakan
  pct_isipadu <- c(rep(0.6,4), rep(0.7,8))
  
  #------------------------------loop untuk setiap kelas------------------------------------
  # dcat - kategori kelas diameter
  # parameter untuk disubset : dcat; one; bat; volt
  # set jumlah petak untuk setiap kategori
  jum_petak <- length(unique(nopega))
  saiz_petak <- c(0.05, rep(0.1,11))
  bil_petak <- rep(jum_petak,12)
  
  #import library dan function
  library(dplyr)
  library(knitr)
  source("kirastat.R")
  #set untuk yang tiada maklumat spesis
  dat7<-file
  dat7$kumpulan_kayu_1.9[is.na(dat7$kumpulan_kayu_1.9)]<-11
  dat7$nama_tempatan<-ifelse(is.na(dat7$nama_tempatan),"Tiada maklumat spesis",as.character(dat7$nama_tempatan))
  dat7$kumpulan_D_ND<-ifelse(is.na(dat7$kumpulan_D_ND),"XX",as.character(dat7$kumpulan_D_ND))
  #gabungkan maklumat yang diperlukan menjadi 1
  dat8<-cbind(dat7$kumpulan_D_ND,dat7$kumpulan_kayu_1.9,dat7$nama_tempatan,dcat,one,bat,volt)
  dat8<-data.frame(dat8)
  names(dat8)<-c("kumpulan_D_ND","kumpulan_kayu_1.9","nama_tempatan","dcat","one","bat","volt")
  dat8$kumpulan_D_ND <- ifelse(dat8$kumpulan_D_ND == 'BD', 'BUKAN DIPTEROKAP', as.character(dat8$kumpulan_D_ND))
  dat8$kumpulan_D_ND <- ifelse(dat8$kumpulan_D_ND == 'D', 'DIPTEROKAP', as.character(dat8$kumpulan_D_ND))
  dat8$kumpulan_D_ND <- ifelse(dat8$kumpulan_D_ND == 'XX', 'TIADA MAKLUMAT KUMPULAN', as.character(dat8$kumpulan_D_ND))
  
  #SET JADUAL KOSONG UNTUK KUMP DIPT & NON DIPT
  Jadual_DND <- matrix(nrow = 0, ncol = 12)
  Jadual_DND <- as.data.frame(Jadual_DND)
  names(Jadual_DND) <- 1:12
  #SET JADUAL KOSONG UNTUK KUMP I-IX
  Jadual_KK <- matrix(nrow = 0, ncol = 12)
  Jadual_KK <- as.data.frame(Jadual_KK)
  names(Jadual_KK) <- 1:12
  #SET JADUAL KOSONG UNTUK KUMP & KESELURUHAN
  Jadual_1 <- matrix(nrow = 0, ncol = 12)
  Jadual_1 <- as.data.frame(Jadual_1)
  names(Jadual_1) <- 1:12
  #SENARAI PARAMETER YANG DIUKUR
  LOP <- list("bil","bil_bersih","isipadu","isipadu_bersih","keluasan_pangkal")
  LOP_DND <- matrix(nrow = 0, ncol = 1)
  LOP_DND <- as.data.frame(LOP_DND)
  LOP_DND <- list()
  #SET JADUAL KOSONG UNTUK KATEGORI
  Jadual_headj1 <- matrix(nrow = 0, ncol = 1)
  Jadual_headj1 <- as.data.frame(Jadual_headj1)
  names(Jadual_headj1) <- "KATEGORI"
  
  #KIRA UNTUK DIPT & NON DIPT
  senaraidnd <- unique(dat8$kumpulan_D_ND)
  senaraidnd <- sort(senaraidnd)
  
  for (i in 1:length(senaraidnd)){
    LOP_DND <-rbind(LOP_DND, LOP)
    datadnd <- subset(dat8, dat8$kumpulan_D_ND == senaraidnd[i])
    jadualdnd <- kirastat(datadnd, pct_sihat, pct_isipadu, saiz_petak, bil_petak)
    Jadual_DND <- rbind.fill(Jadual_DND, jadualdnd)
  }
  
  #KIRA UNTUK JADUAL 1 DAN UNTUK KUMP KAYU
  senaraikk <- unique(dat8$kumpulan_kayu_1.9)
  senaraikk <- as.integer(as.character(senaraikk))
  senaraikk <- sort(senaraikk)
  
  mx <- 0
  for (j in 1:length(senaraikk)){
    datakk <- subset(dat8, dat8$kumpulan_kayu_1.9 == senaraikk[j])
    jadualkk <- kirastat(datakk, pct_sihat, pct_isipadu, saiz_petak, bil_petak)
    Jadual_KK <- rbind.fill(Jadual_KK, jadualkk)
    
    senaraisp <- unique(datakk$nama_tempatan)
    senaraisp <- as.character(senaraisp)
    senaraisp <- sort(senaraisp)
    for (k in 1:length(senaraisp)){
      Jadual_headj1[(mx+(k*5-4)),1] <- senaraisp[k]
      datasp <- subset(datakk, datakk$nama_tempatan == senaraisp[k])
      jadualsp <- kirastat(datasp, pct_sihat, pct_isipadu, saiz_petak, bil_petak)
      Jadual_1 <- rbind.fill(Jadual_1, jadualsp)
    }
    Jadual_headj1[((mx+((k+1)*5-4))),1] <- as.character(paste("JUMLAH KUMPULAN ", as.character(senaraikk[j])))
    mx <- dim(Jadual_headj1)[1] + 4
    Jadual_1 <- rbind.fill(Jadual_1, jadualkk)
  }
  
  #KIRA UNTUK KESELURUHAN DATA
  Jadual_all <- kirastat(dat8, pct_sihat, pct_isipadu, saiz_petak, bil_petak)
  #GABUNGKAN SEMUA DATA
  Jadual_1 <- rbind.fill(Jadual_1, Jadual_KK, Jadual_DND, Jadual_all)
  #KIRA JUMLAH UNTUK SETIAP ROW
  Jadual_1 <- transform(Jadual_1, sum=rowSums(Jadual_1, na.rm = TRUE))
  #ISIKAN NA MENJADI O
  Jadual_1[is.na(Jadual_1)] <- 0
  names(Jadual_1)<-c("15","30","45","50","55","60","65","70","75","80","85","90+","JUMLAH")
  #TAMBAH MAKLUMAT UNTUK KATEGORI DAN PARAMETER
  bilparam <- length(senaraidnd) + (length(senaraikk) * 2) + length(unique(dat8$nama_tempatan)) + 1
  LISTOFPARAM <- rep(LOP, bilparam)
  LISTOFPARAM <- data.frame(LISTOFPARAM, row.names = 'PARAMETER')
  names(LISTOFPARAM) <- 1:(bilparam * 5)
  LISTOFPARAM <- t(LISTOFPARAM)
  for (l in 1:length(senaraikk)){
    Jadual_headj1[mx+(5*l-4),1] <- as.character(paste("KUMPULAN ", as.character(senaraikk[l])))
  }
  mx <- dim(Jadual_headj1)[1] + 4
  for (l in 1:length(senaraidnd)){
    Jadual_headj1[mx+(5*l-4),1] <- senaraidnd[l]
  }
  mx <- dim(Jadual_headj1)[1] + 4
  Jadual_headj1[mx+1,1] <- "JUMLAH BESAR"
  Jadual_headj1[mx+5,1] <- "TAMAT"
  
  Jadual1_head <- cbind(Jadual_headj1,LISTOFPARAM)
  Jadual_1 <- cbind(Jadual1_head, Jadual_1)
  Jadual_1$KATEGORI <- ifelse(Jadual_1$KATEGORI=="TAMAT",NA,Jadual_1$KATEGORI)
  Jadual_1$KATEGORI <- ifelse(is.na(Jadual_1$KATEGORI),"",Jadual_1$KATEGORI)
  
  detach(file)
  
  # JADUAL 2 --------------------------------------------------------------------------------------
  
  library(knitr)
  Jadual_2 <- Jadual_1
  Jadual_2$`15`<- Jadual_2$JUMLAH
  for (i in 1:dim(Jadual_1)[1]){
    Jadual_2[i,4:13] <- Jadual_2$JUMLAH[i] - t(apply(Jadual_2[i,4:13],1,cumsum))
  }
  Jadual_2 <- Jadual_2[-15]
  names(Jadual_2) <- c("KATEGORI","PARAMETER","+15","+30","+45","+50","+55","+60","+65",
                       "+70","+75","+80","+85","+90")
  
  # JADUAL 3 --------------------------------------------------------------------------------------
  
  attach(file)
  
  
  dcat <- as.numeric(cut(dbh,c(15, 30, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90,250),labels = 1:12))
  bat <- dbh^2*pi/40000
  
  # bilangan balak mengikut saiz dbh
  bil_log <- ifelse(dcat>1 & dcat<=5, 2*5, 0)
  bil_log <- ifelse(dcat>5 & dcat<=8, 3*5, bil_log)
  bil_log <- ifelse(dcat>8, 4*5, bil_log)
  
  # kiraan isipadu
  volt <- bat*bil_log*0.65
  
  # kiraan isipadu bersih
  volt_bersih <- ifelse(dbh>=60, volt*0.7, 0)
  volt_bersih <- ifelse(dbh<60, volt*0.6, volt_bersih)
  
  cutD <- c(rep(90,9),rep(85,9),rep(80,9),rep(75,9),rep(70,9),rep(65,9),rep(60,9),rep(55,9),rep(50,9))
  cutND <- c(rep(c(seq(85,45,-5)),9))
  
  CL <- data.frame(cutD,cutND)
  
  
  # Jumlah isipadu bersih ditebang 
  
  out_Dvol <- data.frame()
  out_NDvol <- data.frame()
  for (i in 1:length(cutD))
  {  
    volD <-  sum(volt_bersih[dbh>cutD[i] & kumpulan_D_ND =="D"],na.rm=T)
    volND <- sum(volt_bersih[dbh>cutND[i] & kumpulan_D_ND =="BD"],na.rm=T)
    
    jum_petak <- length(unique(file$nopega))
    saiz_petak <- c(0.05, rep(0.1,11))
    bil_petak <- rep(jum_petak,12)
    
    saiz_petak <- 0.10
    out_Dvol <-append(out_Dvol,  volD/ (jum_petak*saiz_petak))
    out_NDvol<-append(out_NDvol, volND/(jum_petak*saiz_petak))
    
  } 
  vol <- cbind.data.frame(volD=as.numeric(out_Dvol),volND=as.numeric(out_NDvol))
  vol <- round((vol),2)
  vol$jumlah <- apply(vol,1,sum)
  names(vol)=c("volD","volND"," volJ")
  
  
  # Bilangan pokok ditebang
  
  out_Dbil <- data.frame()
  out_NDbil <- data.frame()
  
  one <- rep(1,dim(file)[1])
  
  for (i in 1:length(cutD))
  {  
    bilD <-  sum(one[dbh>cutD[i]  & kumpulan_D_ND =="D"],na.rm=T)
    bilND <- sum(one[ dbh>cutND[i] & kumpulan_D_ND =="BD"],na.rm=T)
    
    saiz_petak <- 0.10
    out_Dbil <-append(out_Dbil,  bilD/ (jum_petak*saiz_petak))
    out_NDbil<-append(out_NDbil, bilND/(jum_petak*saiz_petak))
    
  } 
  bil <- cbind.data.frame(bilD=as.numeric(out_Dbil),bilND=as.numeric(out_NDbil))
  bil <- round((bil),2)
  names(bil)=c("bilD","bilND")
  bil$jumlah <- apply(bil,1,sum)
  bil$peratusD <- bil[,1]/(bil[,1]+bil[,2])
  bil$peratusD <- round(bil$peratusD*100,2)
  names(bil)=c("bilD","bilND","bilJ", "pctD")
  
  # Bilangan pokok ditinggal dan setara tanpa mengambil kira 15-30 cm dbh
  
  out_Dbil1 <- data.frame()
  out_NDbil1 <- data.frame()
  
  for (i in 1:length(cutD))
  {  
    # equivalent rule 1 tree of 30-45 cm dbh equivalent to 2 trees of >= 45 cm dbh
    
    bilD2 <-  sum(one[dbh<cutD[i]  & dbh>=45 & kumpulan_D_ND =="D"],na.rm=T)
    bilND2 <- sum(one[dbh<cutND[i] & dbh>=45 & kumpulan_D_ND =="BD"],na.rm=T)
    
    bilD2 <- 2 * bilD2
    bilND2 <- 2 * bilND2
    
    # no equivalent rule for tree dbh 30-45 cm
    
    bilD1 <-  sum(one[dbh>=30 & dbh<45 & kumpulan_D_ND =="D"],na.rm=T)
    bilND1 <- sum(one[dbh>=30 & dbh<45 & kumpulan_D_ND =="BD"],na.rm=T)
    
    bilD <- bilD1 + bilD2
    bilND <- bilND1 + bilND2
    
    saiz_petak <- 0.10
    out_Dbil1 <-append(out_Dbil1,  bilD/ (jum_petak*saiz_petak))
    out_NDbil1<-append(out_NDbil1, bilND/(jum_petak*saiz_petak))
    
  } 
  bil1 <- cbind.data.frame(bilD=as.numeric(out_Dbil1),bilND=as.numeric(out_NDbil1))
  bil1 <- round((bil1),2)
  names(bil1)=c("bilD","bilND")
  bil1$jumlah <- apply(bil1,1,sum)
  bil1$peratusD <- bil1[,1]/(bil1[,1]+bil1[,2])
  bil1$peratusD <- round(bil1$peratusD*100,2)
  names(bil1)=c("bilD","bilND","bilJ", "pctD")
  bil_tara <- round(bil1, 2)
  
  Jadual_3 <- data.frame(cutD, cutND,vol,bil, bil_tara)
  names(Jadual_3) <- c("CL D","CL ND","Vol D","Vol ND","Vol Jum","F No D","F No ND","No Jum","F Pct D","R No D ","R ND No","R Jum No","R Pct D")
  
  # Peratus D 30 cm dbh keatas 
  
  pctD_preF <- bilD/(bilD+bilND) * 100
  
  pctD_preF <- round(pctD_preF,2)
  
  # JADUAL 4 --------------------------------------------------------------------------------------
  
  attach(file)
  
  
  dcat <- as.numeric(cut(dbh,c(15, 30, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90,250),labels = 1:12))
  bat <- dbh^2*pi/40000
  
  # bilangan balak mengikut saiz dbh
  bil_log <- ifelse(dcat>1 & dcat<=5, 2*5, 0)
  bil_log <- ifelse(dcat>5 & dcat<=8, 3*5, bil_log)
  bil_log <- ifelse(dcat>8, 4*5, bil_log)
  
  # kiraan isipadu
  volt <- bat*bil_log*0.65
  
  # kiraan isipadu bersih
  volt_bersih <- ifelse(dbh>=60, volt*0.7, 0)
  volt_bersih <- ifelse(dbh<60, volt*0.6, volt_bersih)
  
  cutD <- c(rep(90,9),rep(85,9),rep(80,9),rep(75,9),rep(70,9),rep(65,9),rep(60,9),rep(55,9),rep(50,9))
  cutND <- c(rep(c(seq(85,45,-5)),9))
  
  CL <- data.frame(cutD,cutND)
  
  one <- rep(1,dim(file)[1])
  
  
  # Jumlah isipadu bersih ditebang 
  
  out_Dvol <- data.frame()
  out_NDvol <- data.frame()
  for (i in 1:length(cutD))
  {  
    volD <-  sum(volt_bersih[dbh>cutD[i] & kumpulan_D_ND =="D"],na.rm=T)
    volND <- sum(volt_bersih[dbh>cutND[i] & kumpulan_D_ND =="BD"],na.rm=T)
    
    jum_petak <- length(unique(file$nopega))
    saiz_petak <- c(0.05, rep(0.1,11))
    bil_petak <- rep(jum_petak,12)
    
    saiz_petak <- 0.10
    out_Dvol <-append(out_Dvol,  volD/ (jum_petak*saiz_petak))
    out_NDvol<-append(out_NDvol, volND/(jum_petak*saiz_petak))
    
  } 
  vol <- cbind.data.frame(volD=as.numeric(out_Dvol),volND=as.numeric(out_NDvol))
  vol <- round((vol),2)
  vol$jumlah <- apply(vol,1,sum)
  names(vol)=c("volD","volND"," volJ")
  
  # Bilangan pokok ditebang
  
  out_Dbil <- data.frame()
  out_NDbil <- data.frame()
  
  
  for (i in 1:length(cutD))
  {  
    bilD <-  sum(one[dbh>cutD[i]  & kumpulan_D_ND =="D"],na.rm=T)
    bilND <- sum(one[dbh>cutND[i] & kumpulan_D_ND =="BD"],na.rm=T)
    
    saiz_petak <- 0.10
    out_Dbil <-append(out_Dbil,  bilD/ (jum_petak*saiz_petak))
    out_NDbil<-append(out_NDbil, bilND/(jum_petak*saiz_petak))
    
  } 
  bil <- cbind.data.frame(bilD=as.numeric(out_Dbil),bilND=as.numeric(out_NDbil))
  bil <- round((bil),2)
  names(bil)=c("bilD","bilND")
  bil$jumlah <- apply(bil,1,sum)
  bil$peratusD <- bil[,1]/(bil[,1]+bil[,2])
  bil$peratusD <- round(bil$peratusD*100,2)
  names(bil)=c("bilD","bilND","bilJ", "pctD")
  
  # Bilangan pokok ditinggal dan setara tanpa mengambil kira 15-30 cm dbh
  
  out_Dbil1 <- data.frame()
  out_NDbil1 <- data.frame()
  
  for (i in 1:length(cutD))
  {  
    # equivalent rule 1 tree of 30-45 cm dbh equivalent to 2 trees of >= 45 cm dbh
    
    bilD3 <-  sum(one[dbh<cutD[i] & dbh>=45 & kumpulan_D_ND =="D"],na.rm=T)
    bilND3 <- sum(one[dbh<cutND[i] & dbh>=45 & kumpulan_D_ND =="BD"],na.rm=T) 
    
    bilD3 <- 2 * bilD3
    bilND3 <- 2 * bilND3
    
    # equivalent rule 1 tree of 30-45 cm dbh equivalent to 3 trees of 30-45 cm dbh
    
    bilD2 <-  sum(one[dbh<cutD[i] & dbh>=15  & dbh<30 & kumpulan_D_ND =="D"],na.rm=T)
    bilND2 <- sum(one[dbh<cutND[i] & dbh>=15 & dbh<30 & kumpulan_D_ND =="BD"],na.rm=T)
    
    bilD2 <- 1/3 * bilD2
    bilND2 <- 1/3 * bilND2
    
    # no equivalent rule for tree dbh 30-45 cm
    
    bilD1 <-  sum(one[dbh>=30 & dbh<45 & kumpulan_D_ND =="D"],na.rm=T)
    bilND1 <- sum(one[dbh>=30 & dbh<45 & kumpulan_D_ND =="BD"],na.rm=T)
    
    bilD <- bilD1 + bilD2 + bilD3
    bilND <- bilND1 + bilND2 + bilND3
    
    saiz_petak <- 0.10
    out_Dbil1 <-append(out_Dbil1,  bilD/ (jum_petak*saiz_petak))
    out_NDbil1<-append(out_NDbil1, bilND/(jum_petak*saiz_petak))
    
  } 
  bil1 <- cbind.data.frame(bilD=as.numeric(out_Dbil1),bilND=as.numeric(out_NDbil1))
  bil1 <- round((bil1),2)
  names(bil1)=c("bilD","bilND")
  bil1$jumlah <- apply(bil1,1,sum)
  bil1$peratusD <- bil1[,1]/(bil1[,1]+bil1[,2])
  bil1$peratusD <- round(bil1$peratusD*100,2)
  names(bil1)=c("bilD","bilND","bilJ", "pctD")
  bil_tara <- round(bil1, 2)
  
  Jadual_4 <- data.frame(cutD, cutND,vol,bil, bil_tara)
  names(Jadual_4) <- c("CL D","CL ND","Vol D","Vol ND","Vol Jum","F No D","F No ND","No Jum","F Pct D","R No D ","R ND No","R Jum No","R Pct D")
  
  # Peratus D 30 cm dbh keatas 
  
  pctD_preF <- bilD/(bilD+bilND) * 100
  
  pctD_preF <- round(pctD_preF,2)
  
  # JADUAL X --------------------------------------------------------------------------------------
  return(c(dat8,Jadual_1,Jadual_2,Jadual_3,Jadual_4))
  
}