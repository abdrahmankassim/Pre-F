jadual2 <- function(Jadual_1)
  
{
library(knitr)
Jadual_2 <- Jadual_1
Jadual_2$`15`<- Jadual_2$JUMLAH
for (i in 1:dim(Jadual_1)[1]){
  Jadual_2[i,4:13] <- Jadual_2$JUMLAH[i] - t(apply(Jadual_2[i,4:13],1,cumsum))
}
Jadual_2 <- Jadual_2[-15]
names(Jadual_2) <- c("KATEGORI","PARAMETER","+15","+30","+45","+50","+55","+60","+65",
                     "+70","+75","+80","+85","+90")
return((Jadual_2))

#------------------TAMAT KIRA MATRIKS------------------------------------------------------------

detach(file)
}
