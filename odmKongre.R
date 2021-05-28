setwd("C:/Users/ozber/Desktop/OlcmeKongre/itemAnalysis")

###########################

rm(list = ls())

#install.packages("CTT")          ### 
library(CTT)




dat <- read.csv("https://raw.githubusercontent.com/rnzbrk/R-files-/master/matematik.csv", header = TRUE)
head(dat)

mat <- dat[c(2:143), ]   # madde cevap matrisi nrow(dat)
cevapA <- dat[1, ] 

insert_nas <- function(x) {
  len <- length(x)
  n <- sample(1:floor(0.2*len), 1)
  i <- sample(1:len, n)
  x[i] <- NA 
  x
}


mat <- sapply(mat, insert_nas)
mat

apply(is.na(mat),2,sum)


### Celdirici Analizi

celdiriciAnalizi <- distractorAnalysis(mat,cevapA, pTable = TRUE, csvReport="celdiriciAnalizi.csv")




mat[is.na(mat)] <- ""



### Puanlama-Madde Puan Matrisi

mod <- score(mat,cevapA,output.scored=TRUE,rel=TRUE)
mod$score
mod$reliability
mod$scored

str(mod$scored)

mpm <- data.frame(mod$scored)

str(mpm)

colnames(mpm) <- colnames(mat)

#### Madde analizi

maddeAnalizi <- itemAnalysis(mpm, itemReport = TRUE )
maddeAnalizi$itemReport


reliability(mpm)$pBis  #nokta cift serili
reliability(mpm)$bis    #Cift serili







library(mixRasch)
library(eRm)
library(irtoys)
library(ltm)
library(difR)


###################################
#Rasch Modeli
###################################


library(mixRasch)


mattest.dicho <- read.csv("mattest.dicho.csv", header=TRUE)

# Paketin icindeki ornek veri
library(mixRasch)
data(SimMix)

rasch1 <- mixRasch(mattest.dicho,1,50, conv.crit=.0001, n.c=1, info.fit=TRUE) #50 iterasyon, con. crit 0.0001 
rasch1$person.par[1,]   #infit outfit 0.7 to 1.3
rasch1$item.par
personItemPlot(rasch1)

rasch2 <- mixRasch(mattest.dicho,1,50, conv.crit=.0001, n.c=1, info.fit=TRUE)
getEstDetails(rasch2)  #analize ait degerler
getItemDetails(rasch2,1) #maddeye ait degerler
personItemPlot(rasch2,colTheme="dukes")
itemFitPlot(rasch2,colTheme="spartans")
rICC(rasch2$item.par$delta[,16], rasch2$person.par$theta, mattest.dicho[,16], empICC=TRUE, colTheme="cavaliers") #16 . madde MKE (ICC)	
#Ampirik MKE cizilebilir. Buradan da gorulecegi madde olmasi gerekenden daha az ayirt edici





##############################################
#
#   MTK
#
###############################################

eren3pl <- function(theta,a,b,c) 
c + (1-c)*(exp(a*(theta-b))/(1+(exp(a*(theta-b)))))

eren3pl(1, .5,1.2,.2)  #Theta'si 1 olan bir bireyin madde parametreleri belirlenen degerler oldugunda soruyu dogru cevaplama olasiligi

#Theta dagilimindaki her bir yetenek duzeyindeki bireylerin soruyu cozme olasiliklarina bakalim

theta_quad = seq(-4,4,.1)

#Theta dagilimindaki  her bir yetenek duzeyindeki bireylerin madde parametreleri belirlenen degerler oldugunda soruyu dogru cevaplama olasiligi

eren3pl(theta=theta_quad,a=.5,b=1.2,c=.2)

#Grafige bakalim (MKE - ICC)

 plot(theta_quad,eren3pl(theta=theta_quad,a=.5,b=1.2,c=.2), type="l", ylim=c(0,1), xlab="Yetenek", ylab="Olasilik", main="Madde X icin MKE")
 


 
####################################################
#
# irtoys paketi
#
#####################################################

#Paketteki ornek dosyaya bakalim
Scored  #ornek veri

head(Scored)
tail(Scored)
nrow(Scored)
ncol(Scored)

####################
# 1PLM Kestirimi
###########


icl.1pl <- est(Scored, model="1PL", engine="icl")   
bilog.1pl <- est(Scored, model="1PL", engine="bilog")

icl.1pl$est
bilog.1pl$est

######## a parametrelerini BILOG'ta 1e esitleme

icl.1pl <- est(Scored, model="1PL", engine="icl", rasch=FALSE)    # rasch=F kullanin
bilog.1pl <- est(Scored, model="1PL", engine="bilog", rasch=TRUE)

icl.1pl$est
bilog.1pl$est



####################
# 2PLM Kestirimi
###########


icl.2pl <- est(Scored, model="2PL", engine="icl")   
bilog.2pl <- est(Scored, model="2PL", engine="bilog")

icl.2pl$est
bilog.2pl$est

####################
# 3PLM Kestirimi
###########


icl.3pl <- est(Scored, model="3PL", engine="icl")   
bilog.3pl <- est(Scored, model="3PL", engine="bilog")

icl.3pl$est
bilog.3pl$est



############
# Madde Bilgi Fonksiyonu

################


plot(iif(icl.3pl$est[1,]), main="ICL")  #icl
plot(iif(bilog.3pl$est[1,]), main="BILOG") #bilog


#Madde Tepki Fonksiyonu (MKE-ICC)
plot(irf(icl.3pl$est[1,]), main="ICL")
plot(irf(bilog.3pl$est[1,]), main="BILOG")


####  Test Bilgi Fonksiyonu

plot(tif(icl.3pl$est), co="red", main="ICL")
plot(tif(bilog.3pl$est), co="red", main="BILOG")


####### Bireylerin Yetenekleri (MLE)
ability.est <- mlebme(resp=Scored, ip=bilog.3pl$est)

ability.est

#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################

############
# 
# difR Paketi ile DIF analizi
#
################


data(verbal)
attach(verbal)

verbal <- verbal[colnames(verbal)!="Anger"]  #Anger surekli degiskenini veriden cikarma

dichoDif(verbal, group = 25, focal.name = 1, method = c( "MH", "Logistic", "Lord",
"Raju"), correct = FALSE, purify = TRUE, model="2PL")



## Sadece DIF gosteren maddeler belirlendi ama maddelere ait istatistiklere ulasilamiyor

# MH yontemine gore DIF analizi

verbalMH <- difMH(verbal, group = 25, focal.name = 1)
verbalMH
plot(verbalMH)



# Lojistik Regresyon yontemine gore DIF analizi

verbalLR <- difLogistic(verbal, group = "Gender", focal.name = 1)
verbalLR
plot(verbalLR)
plot(verbalLR, plot = "itemCurve", item = 17)




# Lord'un Ki-kare yontemine gore DIF analizi

verbalLChi1PL <- difLord(verbal, group = 25, focal.name = 1, model = "1PL")
verbalLChi1PL
plot(verbalLChi1PL)
plot(verbalLChi1PL, plot = "itemCurve", item = 17)


verbalLChi2PL <- difLord(verbal, group = 25, focal.name = 1, model = "2PL")
verbalLChi2PL
plot(verbalLChi2PL)
plot(verbalLChi2PL, plot = "itemCurve", item = 17)




# Raju'nun Alan yontemine gore DIF analizi

verbalRaju1PL <- difRaju(verbal, group = 25, focal.name = 1, model = "1PL")
verbalRaju1PL
plot(verbalRaju1PL)


verbalRaju2PL <- difRaju(verbal, group = 25, focal.name = 1, model = "2PL")
verbalRaju2PL
plot(verbalRaju2PL)




#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################

############						############
############ Test Esitleme			############
############						############
############						############



library(psych)
library(equate)
library(ltm)
source("irf.R")


mattest <- read.table("mattest.dicho.csv", header=T, sep=",")

mattest1 <- mattest [1:500, ]
mattest2 <- mattest [(501:nrow(mattest)),]

#  Ortalama Esýtleme


# Her bir bireyin toplam puanini hesaplama
form1.sum <- apply(mattest1, 1, sum)
form2.sum <- apply(mattest2, 1, sum)

# Her bir formun ortalamasi
describe(form1.sum)$mean
describe(form2.sum)$mean

# Her iki form arasindaki ortalamalarin farkini bulalim 
describe(form1.sum)$mean - describe(form2.sum)$mean


# Kumulatif dagilimi bulalim (toplam puanlar uzerinde)
equate_f1_freq <- freqtab(xscale = c(0:33), form1.sum)
equate_f2_freq <- freqtab(xscale = c(0:33), form2.sum)


equate_f1_freq
equate_f2_freq


# Simdi ortalama esitleme yapabiliriz
eq_mean <- equate(equate_f2_freq, equate_f1_freq, type = "mean")
eq_mean


# Esitlenmis puanlar.
eq_mean$conc

############						############
############ Dogrusal Esitleme		############
############						############
############						############



# Esitlemenin slope hesaplayacagiz (standart sapma)
describe(form1.sum)$sd/describe(form2.sum)$sd


# Esitlemenin interceptini hesaplayacagiz
describe(form1.sum)$mean - ((describe(form1.sum)$sd/describe(form2.sum)$sd) * 
    describe(form2.sum)$mean)
	
	
# Dogrusal Esitleme
eq_linear <- equate(equate_f2_freq, equate_f1_freq, type = "linear")
eq_linear


# Esitlenmis puanlar.
eq_linear$conc


############							############
############ Esit Yuzdelikli Esitleme			############
############							############
############							############


# %75 lik dilimdeki puan degeri 
quantile(form1.sum, 0.75)
quantile(form2.sum, 0.75)

# Esityuzdelikli esitleme denklemi
eq_equipercentile <- equate(equate_f2_freq, equate_f1_freq, type = "equipercentile")

# Esitlenmis puanlar.
eq_equipercentile$conc




############							############
############ Concurrent Kalibrasyon		############
############							############
############							############

concurrent.1pl <- rasch(mattest)
# Item parameters for first 10 items.
coef(concurrent.1pl)[1:10, ]


f1eq <- c(1, 4, 5, 8, 9, 11, 14, 16, 18, 21, 22, 26, 27, 31, 33)  #formlardaki madde numaralari
f2eq <- c(1, 4, 5, 8, 9, 10, 14, 16, 18, 22, 23, 26, 27, 31, 33)

form1 <- 1:22   #ilk formumuzun maddeleri
form2 <- c(f1eq, 23:33)  #ikinci formumuzun maddeleri
n <- nrow(mattest)

a <- coef(concurrent.1pl)[, 2]
b <- coef(concurrent.1pl)[, 1]
cc <- rep(0, 33)
concurrent.1pl.theta <- factor.scores(concurrent.1pl, resp.patterns = mattest[1:n, 
   ])$score.dat$z1
	
	
equatemat <- matrix(0, ncol = 4, nrow = n)
colnames(equatemat) <- c("Score", "Theta", "T1", "T2")
equatemat[, 1] <- apply(mattest, 1, sum, na.rm = TRUE)
equatemat[, 2] <- concurrent.1pl.theta
for (j in 1:n) {

    equatemat[j, 3] <- sum(irf(concurrent.1pl.theta[j], a[form1], b[form1], 
        cc[form1]))

    equatemat[j, 4] <- sum(irf(concurrent.1pl.theta[j], a[form2], b[form2], 
        cc[form2]))
}

equatemat[1:5, ]




############									############
############ Ayri (Seperate) Kalibrasyon		############
############									############
############									############


mattest1.parms <- rasch(mattest1, constraint = cbind(ncol(mattest1) + 1, 1)) #Mattest1 formunu Rasch modeline gore parametre kestirimi yapalim a=1

#Madde parametreleri 

coef(mattest1.parms)[1:10, ]


# Yetenekleri kestirelim (theta)
separate1.theta <- factor.scores(mattest1.parms, resp.patterns = mattest1[1:nrow(mattest1), 
    ])$score.dat$z1


separate1.theta




#Form 2 icin madde parametresi kestirme 
mattest2.parms <- rasch(mattest2, constraint = cbind(c(f2eq, ncol(mattest2) + 
    1), c(coef(mattest1.parms)[f1eq, 1] * -1, 1)))

	
	# Form 2 icin madde parametreleri
coef(mattest2.parms)[1:10, ]


# Yetenekleri kestirelim
separate2.theta <- factor.scores(mattest2.parms, resp.patterns = mattest2[1:nrow(mattest2), 
    ])$score.dat$z1

	
	
	
# Ve esitleme islemi
equatemat <- matrix(0, ncol = 4, nrow = n)
colnames(equatemat) <- c("Score", "Theta", "T1", "T2")
equatemat[, 1] <- as.matrix(c(form1.sum, form2.sum), ncol = 1)
equatemat[, 2] <- as.matrix(c(separate1.theta, separate2.theta), ncol = 1)
equatemat[, 3] <- as.matrix(irf(equatemat[, 2], a = coef(mattest1.parms)[, 2], 
    b = coef(mattest1.parms)[, 1], cc = rep(0, 35)), ncol = 1)
equatemat[, 4] <- as.matrix(irf(equatemat[, 2], a = coef(mattest2.parms)[, 2], 
    b = coef(mattest2.parms)[, 1], cc = rep(0, 35)), ncol = 1)



equatemat[1:5, ]



#########################################################################
###################								#########################
###################			SON					#########################
###################								#########################
#########################################################################























