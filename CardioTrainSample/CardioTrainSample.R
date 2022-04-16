#IST ANALIZ FINAL

#KULLANILAN PAKETLER
install.packages("data.table")
library(data.table)
library(stats)
library(desctable)
install.packages("knitr")

cardio_train <-  fread("~/Desktop/cardio_train (1).csv", sep=";")
#cardio_train_sample <- as.data.table(sapply(cardio_train[], sample, 750))
cardio_train_sample <- cardio_train[,.SD[sample(.N, min(750,.N))],]

cardio_train_sample <- as.data.table(`cardio_train_sample(4)`)


attach(cardio_train_sample)
detach(cardio_train_sample)


#Kategorik degiskenler faktorize eden bir fonksiyon olusturuldu
cols=c("gender","cholesterol","smoke","alco","active")

did_recode_columns <- function(cardio_train_sample, cols,
                               type = c("as.numeric", "as.factor", "as.character", "as.interger", "as.double") ) 
{
  cardio_train_sample[,(cols) := lapply(.SD, type), .SDcols = cols]
  
}
did_recode_columns(cardio_train_sample, cols, type = "as.factor")


#Ozet tablosu olusturuldu. ID disindaki kategorik degiskenlerden kacar tane oldugu hesaplatildi
summary(cardio_train_sample[,!c("id"),with=FALSE])

#Sistolik kan bas,nci değgri normal aralikta olan 120-130 mmHg olan hastalari yazdirmak için %between% ifadesi kullanildi.
length(cardio_train_sample[ap_hi %between% c(120,130)])

cardio_train_sample[,.SD[1:5,c("id","age","cardio")],by=active]


#Kolesterol normal olan hastalar arasından en yuksek diastolik kan basinc degeri
cardio_train_sample[cholesterol=="1",.SD[which.max(ap_lo)]]

#Kolesterol normal olan hastalar arasından en yuksek diastolik kan basinc degeri
cardio_train_sample[cholesterol=="3",.SD[which.max(ap_lo)]]


##IKI GRUP AP_LO KARSILASTIRILMASI#####
#Simdi iki grubun ap_lo degerlerini konum yonunden karsilastiralim
normalCh_Aplo <- as.numeric(unlist(cardio_train_sample[cholesterol %like% "1",c("ap_lo")]))
wellabv_ApLo <- as.numeric(unlist(cardio_train_sample[cholesterol %like% "3",c("ap_lo")]))
ks.test(normalCh_Aplo,"pnorm",mean(normalCh_Aplo),sd=sd(normalCh_Aplo))
# One-sample Kolmogorov-Smirnov test
# 
# data:  normalCh_Aplo
# D = 0.45823, p-value < 2.2e-16
# alternative hypothesis: two-sided
ks.test(wellabv_ApLo,"pnorm",mean(wellabv_ApLo),sd=sd(wellabv_ApLo))
# One-sample Kolmogorov-Smirnov test
# 
# data:  wellabv_ApLo
# D = 0.49369, p-value < 2.2e-16
# alternative hypothesis: two-sided
wilcox.test(x=normalCh_Aplo,y=wellabv_ApLo,alternative="two.sided",exact=FALSE,paired=FALSE)
# Wilcoxon rank sum test with continuity correction
# 
# data:  normalCh_Aplo and wellabv_ApLo
# W = 24294, p-value = 0.1984
# alternative hypothesis: true location shift is not equal to 0
#######


weights_AsNumeric <- as.numeric(unlist(cardio_train_sample[cholesterol %like% "3",c(5)]))
ks.test(weights_AsNumeric,"pnorm",mean=mean(weights_AsNumeric),sd=sd(weights_AsNumeric))
    # One-sample Kolmogorov-Smirnov test
    # 
    # data:  weights_AsNumeric
    # D = 0.095221, p-value = 0.3615
    # alternative hypothesis: two-sided
#Kolesterolu normalin cok ustunde olan kisilerin agirliklarinin dagilimi normallik gostermektedir.
t.test(weights_AsNumeric,mu=65,alternative="greater",exact=FALSE)
# One Sample t-test
# 
# data:  weights_AsNumeric
# t = 7.065, df = 93, p-value = 1.443e-10
# alternative hypothesis: true mean is greater than 65
# 95 percent confidence interval:
#   72.24563      Inf
# sample estimates:
#   mean of x 
# 74.4734
#Buna gore Kolesterolu normalin cok ustunde olan kisilerin agirliklarinin 
#ortalamasi 65kg dan fazla oldugunu %5 anlamlilik duzeyinde soyleyebiliriz
mean(weights_AsNumeric)
#Gerçek ortalamaya bakıcak olursa 74.4734


######IKI KATEGORIK DEGISKENIN ILISKISI####
table(cardio_train_sample$smoke,cardio_train_sample$active)
#     0   1
# 0 141 543
# 1  14  52
chisq.test(cardio_train_sample$smoke,cardio_train_sample$active,correct = FALSE)
# Pearson's Chi-squared test
# 
# data:  cardio_train_sample$smoke and cardio_train_sample$active
# X-squared = 0.013132, df = 1, p-value = 0.9088
#Hipotez reddedilemez sigara içmek ile fiziksel aktivite yapmak birbirlerinden
#birbirlerini etkilemeyen iki degiskendir
######

#####ORAN TESTI KOLESTEROL VE ALCO####
normalCh_YesAlco <- nrow(cardio_train_sample[cholesterol %like% "1" & alco %like% "1"])
total_normalCh <- nrow(cardio_train_sample[cholesterol %like% "1"])
wellabvCh_YesAlco <- nrow(cardio_train_sample[cholesterol %like% "3" & alco %like% "1"])
total_wellabvCh <- nrow(cardio_train_sample[cholesterol %like% "3"])
prop.test(c(normalCh_YesAlco,wellabvCh_YesAlco),c(total_normalCh,total_wellabvCh),correct = FALSE)

# 2-sample test for equality of proportions without continuity correction
# 
# data:  c(normalCh_YesAlco, wellabvCh_YesAlco) out of c(total_normalCh, total_wellabvCh)
# X-squared = 0.095917, df = 1, p-value = 0.7568
# alternative hypothesis: two.sided
# 95 percent confidence interval:
#   -0.03717204  0.05206565
# sample estimates:
#   prop 1     prop 2 
# 0.05000000 0.04255319 



######

######ORAN TESTİ CARDIO VE SMOKE####
HaveCardio_YesSmoke <- nrow(cardio_train_sample[cardio %like% "1" & smoke %like% "1"])
total_haveCardio<- nrow(cardio_train_sample[cardio %like% "1"])
DontHaveCardio_YesSmoke <- nrow(cardio_train_sample[cardio %like% "0" & smoke %like% "1"])
total_donthaveCardio <- nrow(cardio_train_sample[cardio %like% "0"])

prop.test(c(HaveCardio_YesSmoke,DontHaveCardio_YesSmoke),c(total_haveCardio,total_donthaveCardio),alternative="greater",correct = FALSE)
# 2-sample test for equality of proportions without continuity correction
# 
# data:  c(HaveCardio_YesSmoke, DontHaveCardio_YesSmoke) out of c(total_haveCardio, total_donthaveCardio)
# X-squared = 0.069945, df = 1, p-value = 0.3957
# alternative hypothesis: greater
# 95 percent confidence interval:
#   -0.02866969  1.00000000
# sample estimates:
#   prop 1     prop 2 
# 0.09090909 0.08542714 
#####

####ALCO VE APHI####
ConsumeAlco_apHi <- as.numeric(unlist(cardio_train_sample[alco %like% 1,c("ap_hi")]))
NotConsumeAlco_apHi <- as.numeric(unlist(cardio_train_sample[alco %like% 0,c("ap_hi")]))
if(length(ConsumeAlco_apHi) < 50){
  shapiro.test(ConsumeAlco_apHi)
  # Shapiro-Wilk normality test
  # 
  # data:  ConsumeAlco_apHi
  # W = 0.81629, p-value = 3.488e-05
}else{  #Eger genislik 50den buyuk ise kolmogrov smirnov testi yap
  ks.test(ConsumeAlco_apHi,"pnorm",
          mean=mean(ConsumeAlco_apHi),sd=sd(ConsumeAlco_apHi))
  }

if(length(NotConsumeAlco_apHi) < 50){
  shapiro.test(NotConsumeAlco_apHi)
}else{  #Eger genislik 50den buyuk ise kolmogrov smirnov testi yap
  
  ks.test(NotConsumeAlco_apHi,"pnorm",
          mean=mean(NotConsumeAlco_apHi),
          sd=sd(NotConsumeAlco_apHi))
  # One-sample Kolmogorov-Smirnov test
  # 
  # data:  NotConsumeAlco_apHi
  # D = 0.21059, p-value < 2.2e-16
  # alternative hypothesis: two-sided

}
wilcox.test(x=ConsumeAlco_apHi,y=NotConsumeAlco_apHi,alternative="two.sided",paired = FALSE,exact=FALSE)
# Wilcoxon rank sum test with continuity correction
# 
# data:  ConsumeAlco_apHi and NotConsumeAlco_apHi
# W = 13940, p-value = 0.373
# alternative hypothesis: true location shift is not equal to 0
#######

######active ve ap_hi#####
Active_apHi <- as.numeric(unlist(cardio_train_sample[active %like% 1,c("ap_hi")]))
NotActive_apHi <- as.numeric(unlist(cardio_train_sample[active %like% 0,c("ap_hi")]))

ks.test(Active_apHi,"pnorm",mean(Active_apHi),sd=sd(Active_apHi))
# One-sample Kolmogorov-Smirnov test
# 
# data:  Active_apHi
# D = 0.21545, p-value < 2.2e-16
# alternative hypothesis: two-sided

ks.test(NotActive_apHi,"pnorm",mean(NotActive_apHi),sd=sd(NotActive_apHi))
# One-sample Kolmogorov-Smirnov test
# 
# data:  NotActive_apHi
# D = 0.21689, p-value = 9.288e-07
# alternative hypothesis: two-sided

wilcox.test(x=Active_apHi,y=NotActive_apHi,alternative="two.sided",exact=FALSE,paired=FALSE)
#######

#####ALCO VE AP_LO KARSILASTIRMASI#####
ConsumeAlco_apLo <- as.numeric(unlist(cardio_train_sample[alco %like% 1,c("ap_lo")]))
NotConsumeAlco_apLo <- as.numeric(unlist(cardio_train_sample[alco %like% 0,c("ap_lo")]))
if(length(ConsumeAlco_apLo) < 50){
  shapiro.test(ConsumeAlco_apLo)
  #Shapiro.test uygulandı cunku alkol kullananlarin sayisi sectigimiz rasgele orneklemde 36 yani <50
  # Shapiro-Wilk normality test
  # 
  # data:  as.numeric(unlist(cardio_train_sample[alco %like% 1, c("ap_lo")]))
  # W = 0.84879, p-value = 0.0001785
}
else{  #Eger genislik 50den buyuk ise kolmogrov smirnov testi yap
  ks.test(ConsumeAlco_apLo,"pnorm",
          mean=mean(ConsumeAlco_apLo),sd=sd(ConsumeAlco_apLo))
}

if(length(NotConsumeAlco_apLo) < 50){
  shapiro.test(NotConsumeAlco_apLo)
}
else{  #Eger genislik 50den buyuk ise kolmogrov smirnov testi yap
 
  ks.test(NotConsumeAlco_apLo,"pnorm",
          mean=mean(NotConsumeAlco_apLo),
          sd=sd(NotConsumeAlco_apLo))
  is.normal(NotConsumeAlco_apLo)
  # One-sample Kolmogorov-Smirnov test
  # 
  # data:  as.numeric(unlist(cardio_train_sample[alco %like% 0, c("ap_lo")]))
  # D = 0.46087, p-value < 2.2e-16
  # alternative hypothesis: two-sided
}

wilcox.test(x=ConsumeAlco_apLo,y=NotConsumeAlco_apLo,alternative="greater",paired = FALSE,exact=FALSE)
# Wilcoxon rank sum test with continuity correction
# 
# data:  ConsumeAlco_apLo and NotConsumeAlco_apLo
# W = 12748, p-value = 0.535
# alternative hypothesis: true location shift is greater than 0

# Buna göre alkol kullanan insanların diastolik kan basinclarini ortalamasinin
# alkol kullanmayan insanlarınkine göre daha fazla oldugunu %5 anlamlilik duzeyinde soyleyebiliriz

#######

#####3GRUP CHOLESTEROL AP_LO ORT KARSILASTIRMA####
normalCh_ApLo2 <- sample(as.numeric(unlist(cardio_train_sample[cholesterol %like% "1",c("ap_lo")])),20)
abvCh_ApLo <- sample(as.numeric(unlist(cardio_train_sample[cholesterol %like% "2",c("ap_lo")])),20)
wellabv_ApLo2 <- sample(as.numeric(unlist(cardio_train_sample[cholesterol %like% "3",c("ap_lo")])),20)

shapiro.test(normalCh_ApLo)
# Shapiro-Wilk normality test
# 
# data:  normalCh_ApLo
# W = 0.88311, p-value = 0.02013

shapiro.test(abvCh_ApLo)
# Shapiro-Wilk normality test
# 
# data:  abvCh_ApLo
# W = 0.84599, p-value = 0.00457

shapiro.test(wellabv_ApLo)
# Shapiro-Wilk normality test
# 
# data:  wellabv_ApLo
# W = 0.3675, p-value = 2.465e-08

concatened_ApLo <- data.frame(cholesterols=rep(c("Normal","Above Normal","Well Above Normal"),each=20),
                         ApLo = c(normalCh_ApLo,abvCh_ApLo,wellabv_ApLo))
kruskal.test(concatened_ApLo$ApLo,concatened_ApLo$cholesterols)
# Kruskal-Wallis rank sum test
# 
# data:  concatened_ApLo$ApLo and concatened_ApLo$cholesterols
# Kruskal-Wallis chi-squared = 9.2017, df = 2, p-value = 0.01004

pairwise.wilcox.test(concatened_ApLo$ApLo,concatened_ApLo$cholesterols,paired = FALSE,
                     p.adjust.method = "none",exact=FALSE)
# Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
# 
# data:  concatened_ApLo$ApLo and concatened_ApLo$cholesterols 
# 
#                    Above      Normal
# Normal             0.4384        -     
# Well Above Normal  0.0331     0.0041
# 
# P value adjustment method: none 













######




