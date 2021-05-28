
############
# 
# Gercek Verimiz uzerinden islemler yapalim (Sadece BILOG)
#
################
library(irtoys)

mattest <- read.table("mattest.dicho.csv", header=T, sep=",")

mattest.1pl <- est(mattest, model="1PL", engine="bilog")
mattest.2pl <- est(mattest, model="2PL", engine="bilog")
mattest.3pl <- est(mattest, model="3PL", engine="bilog")


mattest.1pl$est
mattest.2pl$est
mattest.3pl$est


plot(iif(mattest.2pl$est[c(1,8,25),]), co=NA)


plot(irf(mattest.2pl$est[c(1,8,25),]), co=NA)

plot(tif(mattest.2pl$est), co="red")


####### Bireylerin Yetenekleri (MLE)
mattest.sco <- mlebme(resp=mattest, ip=mattest.2pl$est)

hist(mattest.sco[,1])

####  MKE'ler


par(mar=c(1,1,1,1)) #Kenar bosluklari belirleme
par(mfrow=c(5,7))



for(jj in 1:33){
plot(irf(mattest.2pl$est[jj,]), co=NA)
}	 
