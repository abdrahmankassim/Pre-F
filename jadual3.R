
jadual3 <- function(file=file)
{
  
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

result <- data.frame(cutD, cutND,vol,bil, bil_tara)
names(result) <- c("CL D","CL ND","Vol D","Vol ND","Vol Jum","F No D","F No ND","No Jum","F Pct D","R No D ","R ND No","R Jum No","R Pct D")


# Peratus D 30 cm dbh keatas 

pctD_preF <- bilD/(bilD+bilND) * 100

pctD_preF <- round(pctD_preF,2)

return(result)

detach(file)
}

