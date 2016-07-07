prepare <- function(namafail)
{
dat1<-namafail

names(dat1) <- c("tahun","negeri","hs.daerah","nokomp_blok","nogarisan","nopetak","jenishutan","jenistanah","geologi",
                 "k.kawasan","k.tanah","aspek","cerun","t.cerun","dongakan","bertam","palma","resam",
                 "rotan_a","rotan_a","rotan_b","rotan_c","rotan_d","rotan_e","rotan_f","rotan_g","buluh_a","buluh_b",
                 "kod1","dbh1","bilangan1","kualiti1","dbh_pepanjat1","lingkar_pepanjat1",
                 "kod2","dbh2","bilangan2","kualiti2","dbh_pepanjat2","lingkar_pepanjat2",
                 "kod3","dbh3","subur3","dbh_pepanjat3","lingkar_pepanjat3",
                 "kod4","dbh4","subur4","dbh_pepanjat4","lingkar_pepanjat4",
                 "kod5","dbh5","subur5",
                 "kod6","bilangan6a","bilangan6b")



attach(dat1)

dat1$negeri <- as.factor(negeri)
dat1$hs.daerah <- as.factor(hs.daerah)
dat1$nokomp_blok <- as.factor(nokomp_blok)
dat1$nogarisan <- as.factor(nogarisan)
dat1$nopetak <- as.factor(nopetak)
dat1$jenishutan <- as.factor(jenishutan)
dat1$jenistanah <- as.factor(jenistanah)
dat1$geologi <- as.factor(geologi)
dat1$k.kawasan <- as.factor(k.kawasan)
dat1$k.tanah <- as.factor(k.tanah)


dat1$dbh1 <- (dbh1)/10
dat1$dbh2 <- (dbh2)/10
dat1$dbh3 <- (dbh3)/10
dat1$dbh4 <- (dbh4)/10
dat1$dbh5 <- (dbh5)/10

dat1$subur3 <- as.factor(subur3)
dat1$subur4 <- as.factor(subur4)
dat1$subur5 <- as.factor(subur5)

dat1$dbh_pepanjat1 <- dbh_pepanjat1/10
dat1$dbh_pepanjat2 <- dbh_pepanjat2/10
dat1$dbh_pepanjat3 <- dbh_pepanjat3/10
dat1$dbh_pepanjat4 <- dbh_pepanjat4/10


detach(dat1)


dat2 <- subset(dat1, !is.na(dat1$tahun)) # pilih data tiada NA

# cantum no garisan dan petak
dat2$nopega <- as.factor(paste(dat2$nogarisan,dat2$nopetak,sep="-"))

attach(dat2)
dat4 <- data.frame(dat2[,c(57,29:56)])
head(dat4)
detach(dat2)

dat3 <- read.delim("Kumpulan spesies master file new.txt")

attach(dat2)

dat4 <- rbind(data.frame(nopega,kod=kod1,dbh=dbh1, dbh_pj=dbh_pepanjat1, li_pj=lingkar_pepanjat1), 
              data.frame(nopega,kod=kod2,dbh=dbh2, dbh_pj=dbh_pepanjat2, li_pj=lingkar_pepanjat2),
              data.frame(nopega,kod=kod3,dbh=dbh3, dbh_pj=dbh_pepanjat3, li_pj=lingkar_pepanjat3))


names(dat4) <- c("nopega","kod","dbh", "dbh_pj", "li_pj")

detach(dat2)

attach(dat4)
b1 <- dim(dat4)[1]/3
dat4$saizpetak <-  as.factor(c(rep(0.10,b1),
                    rep(0.10,b1),
                    rep(0.05,b1)))
dat5 <- subset(dat4,dbh>=15) 
detach(dat4) 


library(plyr) 
dat6 <- join(dat5,dat3, by='kod', type='left', match='first') 
return(dat6)
} 

