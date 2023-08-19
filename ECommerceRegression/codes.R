
#Bir e-ticaret sitesindeki urunlerin satislarini etkileyen faktorler

veri <- read.csv("~/Desktop/21821665_EcemCIRAKOGLU/21821665_EcemCIRAKOGLU(sonveri).txt", sep="")
veri <- as.data.frame(veri) #Converted to a data frame 
View(veri)
names(veri) <- c("fiyat","satis_miktari","stok_yenileme","oy_miktari","kategori")

attach(veri)
#detach(veri)

veri$kategori <- as.factor(veri$kategori)
summary(veri)


#NORMALLIK TESTI
#Grafik uzerinden gorelim
qqnorm(fiyat)
qqline(fiyat)

#Veri setinde n>45 oldugu icin kolmogrov simirnov testini kullandim
ks.test(fiyat,"pnorm")
#Shapiro testini de uyguladım
shapiro.test(fiyat)

#LOGARITMIK DONUSUM
yeni_fiyat <- log(fiyat)
yeni_veri <- cbind(yeni_fiyat,satis_miktari,stok_yenileme,oy_miktari,kategori)
qqnorm(yeni_fiyat)
qqline(yeni_fiyat)
shapiro.test(yeni_fiyat)
ks.test(yeni_fiyat,"pnorm")

#KAREKOK DONUSUMU
yeni_fiyat <- sqrt(fiyat)
yeni_veri <- cbind(yeni_fiyat,satis_miktari,stok_yenileme,oy_miktari,kategori)
qqnorm(yeni_fiyat)
qqline(yeni_fiyat)
shapiro.test(yeni_fiyat)
ks.test(yeni_fiyat,"pnorm")

#Aykiri deger
boxplot(fiyat)
yeni_veri <- veri[-c(5,14,75,13),]
shapiro.test(yeni_veri$fiyat)
ks.test(yeni_veri$fiyat,"pnorm")

#Dogrusallik incelemesi
pairs(yeni_veri)

#Regresyon denkleminin olusturulmasi
yeni_veri$kategori <- as.factor(yeni_veri$kategori)
attach(yeni_veri)
sonuc <- lm(fiyat~satis_miktari+stok_yenileme+oy_miktari+kategori,data=yeni_veri)
summary(sonuc)


#influence.measures(sonuc)
inf <- ls.diag(sonuc)
#Gozlem Uzakligi
x3 <- which(inf$hat>0.06849)
#Birim normal sapma
x1 <-which((inf$std.res< (-2)) | inf$std.res>2)
#Student turu artik
x2 <-which((inf$stud.res<(-3)) | inf$stud.res>3)
#Cook uzakligi
x4 <- which(inf$cooks>0.02739)


union_x1_x2 <- union(x1,x2)
union_x3_x4 <- union(x3,x4)
union_all <- union(union_x1_x2,union_x3_x4)

#Artiklar temizlendi
artik_temizlenmis_veri <- yeni_veri[-union_all,]
attach(artik_temizlenmis_veri)

yeni_sonuc <- lm(fiyat~satis_miktari+stok_yenileme+oy_miktari+kategori,data=artik_temizlenmis_veri)
summary(yeni_sonuc)
confint(yeni_sonuc,level=0.99)

###################################
#Artiklarin temizlenmis oldugu yeni modelde artik incelemesi
influence.measures(yeni_sonuc)
inf2 <- ls.diag(yeni_sonuc)
#Gozlem Uzakligi
x3 <- which(inf2$hat>0.0826)
x3
#Birim normal sapma
x1 <-which((inf2$std.res< (-2)) | inf2$std.res>2)
x1
#Student turu artik
x2 <-which((inf2$stud.res<(-3)) | inf2$stud.res>3)
x2
#Cook uzakligi
x4 <- which(inf2$cooks>0.0330)
x4
###################################

#Degisen varyanslik incelemesi
par(mfrow=c(1,1))
plot(predict(yeni_sonuc),inf2$stud.res,xlab="Predicted Value",ylab="Studentized Residuals")

install.packages("lmtest")
library(lmtest)
bptest(yeni_sonuc) 


###WHITE TEST######
# Artıkların karesi
res = residuals(yeni_sonuc)
sqres = res^2
sqx1=artik_temizlenmis_veri$satis_miktari*artik_temizlenmis_veri$satis_miktari
sqx2=artik_temizlenmis_veri$stok_yenileme*artik_temizlenmis_veri$stok_yenileme
x1x2=artik_temizlenmis_veri$satis_miktari*artik_temizlenmis_veri$stok_yenileme

# Artık karesi üzerinden regresyon modellemesi
WH <-  lm(sqres ~ artik_temizlenmis_veri$satis_miktari + artik_temizlenmis_veri$stok_yenileme+sqx1+sqx2+x1x2)
WHs <-  summary(WH)
# Lagrange Çarpımı Hesaplaması
WHts <-  WHs$r.squared*length(WH$residuals) 

# Ki-kare dağılımından p değeri hesaplaması (sd=2)
WHpv <-  1-pchisq(WHts,df=WH$rank-1)
# Güven aralıklarının elde edilmesi
if (WHpv < 0.05) {
  
  cat("We reject the null hypothesis of homoskedasticity.\n",
      
      "WH = ",WHts,"\n","p-value = ",WHpv)
  
} else {
  
  cat("We fail to reject the null hypothesis; implying homoskedasticity.\n",
      
     "WH = ",WHts,"\n","p-value = ",WHpv)
  
}
#########


#Oziliski sorunu
install.packages("lmtest")
library(lmtest)
dwtest(yeni_sonuc)

#Coklu Baglanti
artik_temizlenmis_veri$kategori <- as.factor(artik_temizlenmis_veri$kategori)
install.packages("DAAG")
library(DAAG)
detach("package:car", unload = TRUE)
vif(yeni_sonuc)
#once kosul sayilarina bak su kadar cbli degisken vardır de sonra da vifin hiçibiri 
#10dan büyük olmadıgı icin etkilenen degisken konusunda yorum
#yapamayacagimizi soyleyebiliriz.

install.packages("olsrr")
library(olsrr)
ols_eigen_cindex(yeni_sonuc)


install.packages("fastDummies")
library(fastDummies)
dummy <- dummy_cols(artik_temizlenmis_veri$kategori)
x41 <- dummy$.data_1
x42 <- dummy$.data_2
x43 <- dummy$.data_3

ort1<-mean(artik_temizlenmis_veri$satis_miktari)
kt1<-sum((artik_temizlenmis_veri$satis_miktari-ort1)^2)
skx1<-(artik_temizlenmis_veri$satis_miktari-ort1)/(kt1^0.5)
ort2<-mean(artik_temizlenmis_veri$stok_yenileme)
kt2<-sum((artik_temizlenmis_veri$stok_yenileme-ort2)^2)
skx2<-(artik_temizlenmis_veri$stok_yenileme-ort2)/(kt2^0.5)
ort3<-mean(artik_temizlenmis_veri$oy_miktari)
kt3<-sum((artik_temizlenmis_veri$oy_miktari-ort3)^2)
skx3<-(artik_temizlenmis_veri$oy_miktari-ort3)/(kt3^0.5)
ort42<-mean(x42)
kt42<-sum((x42-ort42)^2)
skx42<-(x42-ort42)/(kt42^0.5)
ort43<-mean(x43)
kt43<-sum((x43-ort43)^2)
skx43<-(x43-ort43)/(kt43^0.5)
x<-cbind(skx1,skx2,skx3,skx42,skx43)
sm<- eigen (t(x)%*%x)
signif(sm$values,3)
signif(sm$vectors,3)

#UYUM KESTIRIM
newdata = data.frame(satis_miktari=9.46,stok_yenileme=2.67,oy_miktari=3.34,kategori="3")
predict(yeni_sonuc.lm,newdata ,interval = "confidence")


#ON KESTIRIM
newdata = data.frame(satis_miktari=6,stok_yenileme=2,oy_miktari=2.5,kategori="1")
predict(yeni_sonuc.lm,newdata ,interval = "prediction")


#GUVEN ARALIGI UYUM KESTIRIMI
newdata = data.frame(satis_miktari=9.46,stok_yenileme=2.67,oy_miktari=3.34,kategori="3")
predict(yeni_sonuc.lm,newdata ,interval = "confidence",level=0.95)

#GUVEN ARALIGI ON KESTIRIMI
newdata = data.frame(satis_miktari=6,stok_yenileme=2,oy_miktari=2.5,kategori="1")
predict(yeni_sonuc.lm,newdata ,interval = "prediction",level=0.95)

#ILERİYE DOGRU SECIM-----------
install.packages("stats")
library(stats)
lm.null <- lm(artik_temizlenmis_veri$fiyat~1)
forward <-  step(lm.null, artik_temizlenmis_veri$fiyat~artik_temizlenmis_veri$satis_miktari+
                 artik_temizlenmis_veri$stok_yenileme+artik_temizlenmis_veri$oy_miktari+
                 artik_temizlenmis_veri$kategori, direction = "forward")

forward
summary(forward)


#GERIYE DOGRU SECIM
backward<-step(yeni_sonuc,direction="backward")
summary(backward)

#ADIMSAL YONTEM
install.packages("MASS")
library(MASS)
step.model <- stepAIC(yeni_sonuc, direction = "both", trace = FALSE)
summary(step.model)

#RIDGE REGRESYON
library(MASS)
ridge <- lm.ridge(artik_temizlenmis_veri$fiyat~artik_temizlenmis_veri$satis_miktari
                  +artik_temizlenmis_veri$stok_yenileme+artik_temizlenmis_veri$oy_miktari
                  +artik_temizlenmis_veri$kategori, lambda = seq(0,1,0.05))
matplot(ridge$lambda, t(ridge$coef),type="l",xlab=expression(lambda)
       ,ylab=expression(hat(beta)))
abline(h=0,lwd=2)
ridge$coef
select(ridge)
ridge$coef[,ridge$lam==0.4] 




