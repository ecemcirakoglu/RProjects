

# ECEM ÇIRAKOĞLU
# KAAN GURALLAR
# BATUHAN BAHRİ ARSLANGİL
# MERVE MURATGÜL
# ALEYNA ÇOR

#Kullanılan Paketler
install.packages("base")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("faraway")
install.packages("moments")
install.packages("cowplot")

library(dplyr)
# Dplyr paketi, gruplar arası farklar, değişken gruplamaları, 
# yeni değişkenler oluşturma ve benzeri işlemleri 
# gerçekleştirerek bu gizli bilginin açığa çıkması
# için kullanılan R paketlerinden biridir
#https://www.newslabturkey.org/2018/10/28/r-dplyr-paketi-ile-veri-nasil-islenir-ve-analiz-edilir/

library(ggplot2)
# ggplot2, Hadley Wickham tarafindan verileri görselleştirmek
#icin yazılan bir R paketidir.
#https://rstudio-pubs-static.s3.amazonaws.com/552788_44438c53924a4037a2641c0e5800f831.html

library(faraway)
#Julian Faraway taradından yazılan
#doğrusal modelleme için kullanılan bir pakettir.
#https://cran.r-project.org/web/packages/faraway/index.html

library(moments)
# Momentler, basıklık, çarpıklık ve bunlarla ilişkili testleri
# içerisinde barındıran bir pakettir.
#https://cran.r-project.org/web/packages/moments/index.html

library(tseries)
# Zaman serisi ve hesaplamalı finans alanlarında kullanılan
# bir pakettir. Adrian Trapletti,Kurt Hornik ve Blake LeBaron 
# tarafından geliştirilmiştir.
#https://cran.r-project.org/web/packages/tseries/tseries.pdf

library(cowplot)
#ggplot ile yayın kalitesinde şekiller oluşturmaya yardımcı olan 
#bir dizi tema, grafikleri hizalama ve bunları karmaşık bileşik şekiller
#halinde düzenleme işlevleri ve çizimlere açıklama eklemeyi ve grafikleri 
#görüntülerle karıştırmaya kolaylaştıran işlevler gibi çeşitli özellikler sağlar.
#https://cran.r-project.org/web/packages/cowplot/index.html

customers <- read.delim("~/Downloads/musteri.csv")

customers_have_kid <-sample_n(customers,300)
#Veri setinden rastgele 300 veri çekildi.

customers_have_kid<-mutate(customers_have_kid,cocukvar=ifelse(Kidhome!=0,1,0))
attach(customers_have_kid)

summary_table<-customers_have_kid[,-c(1,2,3,4,6,7,8,30,29,28,27,26,25,24,23,22,21),drop=F]

str(customers_have_kid)

######9.1Summarizing Your Data#####
summary(summary_table)

######9.4Testing Categorical Variables for Independence#########

# Ki-kare Bağımsızlık Testi,
# değişkenlerden en az birinin nominal ya da ordinal olması
# durumunda kullanılan ve iki değişken arasında 
# ilişki olup olmadığını tespit etmeye yarayan bir testtir.

#Elimizde eğitim ve medeni durumu olmak üzere iki kategorik değişken bulunmaktadır.
#Bu soruda eğitim durumu ile medeni durum arasında bir ilikşi olup olmadığını Ki-Kare Bağımsızlık testi ile 
#test etmeliyiz.

#Education, Marital_Status two categorical variables

#bu kısmı yazmaya gerek yok
# class(Education) 
# class(Marital_Status)

summary(table(Education,Marital_Status))
#KULLANDIĞIMIZ FONKSİYONU AÇIKLAYABİLİRİZ
#  H0: Eğitim durumu ile Medeni durum arasında bir ilişki yoktur
#  H1: Eğitim durumu ile Medeni durum arasında bir ilişki vardır. 
# Number of cases in table: 300 
# Number of factors: 2 
# Test for independence of all factors:
#   Chisq = 23.961, df = 20, p-value = 0.2441
# Chi-squared approximation may be incorrect

#İki değişkenin birbirinden bağımsız olduğunu aralarında bir ilişki olmadığını söyleyebiliriz.


######9.6Inverting a Quantile######
mean(Income < 35000, na.rm = TRUE )
#Income değişkenimizin değerleri arasında kayıp gözlem olduğu için na.rm
#komutunı True değerine eşitleyerek kayıp gözlemleri işleme almadık.

#0.2583893
#Müşterilerin %25'inin yıllık hane gelirinin 35.000 den az olduğunu söyleyebiliriz.

mean(Recency < 40)
#Müşterilerin %37'si en son 40 günden önce alışveriş yapmıştır.

######9.7Converting Data to z-Scores#####

#Tüm sayısal değerler için z skoru hesaplatıldı.
scale(summary_table)
#Z skoru, değeri bize
#veri noktasının ortalamadan kaç standart sapma uzakta olduğunu söyler.

#scale fonksiyonu ile numeric değerlere sahip her bir değişkenin(sütunun)
#ortalaması ve standart sapmasını elde ettik.


#Sadece Recency değişkenine ilişkin z skoru hesaplattık.
scale(summary_table[,c(2)]) 
head(as.matrix(summary_table[,c(2)])) #Recency değerlerinin ilk 6 hanesi

#        Z - Scores              Recency
#          [,1]                    [,1]
# [1,] -1.0003405459        [1,]    23
# [2,]  0.0354798242        [2,]    52
# [3,] -1.5718276466        [3,]     7
# [4,] -0.8217508269        [4,]    28
# [5,] -0.1073919510        [5,]    48
# [6,]  1.0355822504        [6,]    80

# attr(,"scaled:center")
# [1] 51.00667 <- "Recency" değişkeninin ortalaması
# attr(,"scaled:scale")
# [1] 27.99713 <- "Recency" değişkeninin standart sapması

#Elde ettiğimiz z skorlarına göre
#yapılan alışverişten bu yana geçen gün sayısı "23" değerinin
#ortalamadan -1.0003 standart sapma uzakta ve sol tarafında olduğunu söyleyebiliriz

#Yapılan alışverişlerden bu yana geçen gün sayısının 23'ten fazla olması 
#olasılığını hesaplamak istersek
#P(X>23)
#scale fonksiyonundan elde ettiğimiz tabloya göre standartlaştırılmış hâli
#P(Z>-1.00034)
pnorm(scale(summary_table[,c(2)])[c(1)]) # Bize grafikte -1.0003 değerinin altında
#kalan kısmının olasılığını verir.

1-pnorm(scale(summary_table[,c(2)])[c(1)])
#[1] 0.8414271
#Alışverişlerden bu yana geçen gün sayısının 23'ten fazla olması olasılığının %84 olduğunu söyleyebiliriz.


######9.8Testing the Mean of a Sample (t Test)#####  

?t.test
x <-  sample(customers_have_kid[,"Recency"],20)

shapiro.test(x)
#  H0: Yapılan alışverişin üstünden geçen gün sayılarının dağılımı normal dağılıma uymaktadır.
#  H1: Yapılan alışverişin üstünden geçen gün sayılarının dağılımı normal dağılıma uymamaktadır.
# Shapiro-Wilk normality test
# 
# data:  x
# W = 0.91101, p-value = 0.06663
#Recency değerlerinden rasgele seçilen 20 değerin normallik varsayımını sağladığını
#%95 güven düzeyinde söyleyebiliriz

t.test(x,mu=50)
#     H0: Yapılan alışverişin üstünden geçen gün sayısı ortalaması  = 50
#     H1: Yapılan alışverişin üstünden geçen gün sayısı ortalaması != 50

# One Sample t-test
# data:  x
# t = 0.88716, df = 19, p-value = 0.3861
# alternative hypothesis: true mean is not equal to 50
# 95 percent confidence interval:
#   41.91246 69.98754
# sample estimates:
#   mean of x 
# 55.95 

#Recency değişkenin,n ortalamasını tahmin edebilmek için
#alınan rasgele 20 değer ile tek örneklem t- Testi yaptık
#Elde edilen sonuca göre p > 0.05
#Aynı zamanda çıktıda verilen güven aralığı 
#41.91236 < μ < 69.98754
#test değerimizi içermektedir bu nedenle hipotez reddedilemez.
#Yapılan alışverişin üstünden geçen gün sayısının
#ortalamasının 50 olduğunu %5 anlamlılık düzeyinde söyleyebiliriz.



######9.9Forming a Confidence Interval for a Mean#####
#Bir önceki kısımda bulduğumuz güven aralığının Recency ortalamasını
#içerme olasılığını arttırmak için güven düzeyini arttırmalıyız.

t.test(x,mu=50,conf.level=0.99)

# One Sample t-test
# data:  x
# t = 0.88716, df = 19, p-value = 0.3861
# alternative hypothesis: true mean is not equal to 50
# 99 percent confidence interval:
#   36.76222 75.13778
# sample estimates:
#   mean of x 
# 55.95 

#36.76222 < μ < 75.13778
#Yapılan alışverişin üstünden geçen gün sayısı ortalamasının 36.762 ve 75.137
#değerleri arasında olduğunu %1 anlamlılık düzeyinde söyleyebiliriz




######9.14Testing for Runs###### 
install.packages("tseries")
library(tseries)
runs.test(as.factor(cocukvar))

#    H0: Müşterilerinin çocuk sahibi olmalarının dizilimi rastgeledir
#    H1: Müşterilerinin çocuk sahibi olmalarının dizilimi rastgele değildir

#       Runs Test
# 
# data:  as.factor(cocukvar)
# Standard Normal = -1.0689, p-value = 0.2851
# alternative hypothesis: two.sided

#p > 0.05 olduğundan hipotezin reddedilemeyeceğini %5 anlamlılık düzeyinde söyleyebiliriz 


######9.15Comparing the Means of Two Samples#####

#İki örneklem t Testi - İki örneklemin ortalama yönünden karşılaştırılması

Master_Income <- sample(as.vector(na.omit(customers_have_kid[Education == "Master","Income"])),10)
#Eğitim durumu "Master" olanların gelirlerinin yer aldığı vektör içerisinden rastgele 10 değer çekilerek
#bir örneklem oluşturuldu
Graduation_Income <- sample(as.vector(na.omit(customers_have_kid[Education == "Graduation","Income"])),15)
#Eğitim durumu "Graduation" olanların gelirlerinin yer aldığı vektör içerisinden rastgele 10 değer çekilerek
#bir örneklem oluşturuldu

#Öncelikle iki örneklemin de normallik varsayımını sağlayıp sağlamadığını test ettik

#   H01: Eğitim durumu "Master" olan müşterilerin yıllık gelirlerinin dağılımı ile normal dağılım arasında fark yoktur
#   HS1: Eğitim durumu "Master" olan müşterilerin yıllık gelirlerinin dağılımı ile normal dağılım arasında fark vardır

#   H02: Eğitim durumu "Graduation" olan müşterilerin yıllık gelirlerinin dağılımı ile normal dağılım arasında fark yoktur
#   HS2: Eğitim durumu "Graduation" olan müşterilerin yıllık gelirlerinin dağılımı ile normal dağılım arasında fark vardır

shapiro.test(Master_Income)
#     Shapiro-Wilk normality test
# 
# data:  Master_Income
# W = 0.91529, p-value = 0.3193

# p=0.3193 > 0.05 olduğundan H01 reddedilemez.Eğitim durumu "Master" olanların gelirlerinin dağılımı ile 
#normal dağılım arasında fark olmadığını %5 anlamlılık düzeyinde söyleyebiliriz

shapiro.test(Graduation_Income)
#     Shapiro-Wilk normality test
# 
# data:  Graduation_Income
# W = 0.91262, p-value = 0.1485

#p=0.1485 > 0.05 olduğundan H02 reddedilemez.Eğitim durumu "Graduation" olanların gelirlerinin dağılımı ile 
#normal dağılım arasında fark olmadığını %5 anlamlılık düzeyinde söyleyebiliriz

#Ardından varyans homojenliği varsayımının sağlanıp sağlanmadığını test ettik.

#  H0: Varyanslar homojendir
#  H1: Varyanslar homojen değildir

var.test(Master_Income,Graduation_Income,alternative = "two.sided")
#      F test to compare two variances
# 
# data:  Master_Income and Graduation_Income
# F = 1.1646, num df = 9, denom df = 14, p-value = 0.7701
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.3628893 4.4231747
# sample estimates:
#   ratio of variances 
# 1.164621 

#p=0.7701 > 0.05 olduğundan H0 reddedilemez. Varyansaların homojen olduğunu
#%95 güven düzeyinde söyleyebiliriz.

#Varsayımlar sağlandığına göre iki örneklem t testini uygulayabiliriz.

#   H0:Eğitim durumu "Master" olan müşterilerin yıllık gelirlerinin ortalaması ile 
#     eğitim durumu "Graduation" olan müşterilerin yıllık gelirlerinin ortalaması arasında fark yoktur

#   H1:Eğitim durumu "Master" olan müşterilerin yıllık gelirlerinin ortalaması ile 
#      eğitim durumu "Graduation" olan müşterilerin yıllık gelirlerinin ortalaması arasında fark vardır.
t.test(Master_Income,Graduation_Income,var.equal=T)
#       Two Sample t-test
# data:  Master_Income and Graduation_Income
# t = -1.2178, df = 23, p-value = 0.2356
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -24654.521   6383.055
# sample estimates:
#   mean of x mean of y 
# 44641.40  53777.13 

# p = 0.2356 > 0.05 olduğundan H0 hipotezi reddedilemez buna göre eğitim durumu "Master"
#olan müşterilerin yıllık hane gelirleri ortalaması ile eğitim durumu "Graduation" olan müşterilerin
#yıllık hane gelirleri ortalaması arasında fark olmadığını
#%5 anlamlılık düzeyinde söyleyebiliriz



######9.18Testing Groups for Equal Proportions#####
more_income <- customers_have_kid[Income > mean(Income,na.rm=TRUE),] #geliri ortalamadan yüksek olanlar çekildi 
less_income <- customers_have_kid[Income < mean(Income,na.rm=TRUE),] #geliri ortalamadan düşük olanları çekildi

more_income_cmp4 <- na.omit(more_income[,c(22)]) 
#Geliri ortalamadan YÜKSEK olanların AcceptedCmp4 değerleri çekildi

more_income_cmp4_accepted<- more_income_cmp4[more_income_cmp4 == 1]
#Geliri ortalamadan YÜKSEK olup bu kampanyayı kabul edenler, more_income_cmp4 değerlerinden çekildi.

less_income_cmp4 <- na.omit(less_income[,c(22)])
#Geliri ortalamadan DÜŞÜK olanların AcceptedCmp4 değerleri çekildi

less_income_cmp4_accepted <- less_income_cmp4[less_income_cmp4 == 1]
#Geliri ortalamadan DÜŞÜK olup bu kampanyayı kabul edenler, less_income_cmp4 değerlerinden çekildi.

prop.test(c(length(more_income_cmp4_accepted),length(less_income_cmp4_accepted)),c(length(more_income_cmp4),length(less_income_cmp4)))

# 2-sample test for equality of proportions with continuity correction
# 
# X-squared = 14.549, df = 1, p-value = 0.0001365
# alternative hypothesis: two.sided
# 95 percent confidence interval:
#   0.06538731 0.21408079
# sample estimates:
#   prop 1     prop 2 
# 0.17241379 0.03267974 

#   H0: Gelirleri ortalamadan yüksek olup çocuk sahibi olanların oranı ile
#       gelirleri ortalamadan düşük olup çocuk sahibi olanların oranı arasında fark yoktur
#   H1:  """"""""""""" fark vardır

# Verilen çıktıya göre gelirleri ortalamadan yüksek olup çocuk sahibi olanların oranının(prop 1) = 0.17241379
#               ve gelirleri ortalamadan düşük olup çocuk sahibi olanların oranının(prop 2) = 0.03267974 
#               olduğunu söyleyebiliriz

# p < 0.05 oldukça küçük olduğundan H0 hipotezinin reddedileceğini %95 güven düzeyinde 
# söyleyebiliriz





######9.19Performing Pairwise Comparisons Between Group######

Master_Income <- as.vector(na.omit(customers_have_kid[Education == "Master","Income"]))
PhD_Income <- as.vector(na.omit(customers_have_kid[Education == "PhD","Income"]))
Graduation_Income <- as.vector(na.omit(customers_have_kid[Education == "Graduation","Income"]))
twoNCycle_Income <- as.vector(na.omit(customers_have_kid[Education == "2n Cycle","Income"]))

#Eğitim durumu "Master", "PhD" , "Graduation" ve "2n Cycle" olanların gelir verileri çekildi.
#Income verilerinin içerisinde kayıp gözlem olduğundan na.omit() fonksiyonu ile
# Master yapıp geliri belli olmayan kişiler teste alınmadı.

comb <- stack(list(Master= Master_Income,PhD = PhD_Income, 
                   Graduation = Graduation_Income, TwoNCycle = twoNCycle_Income))

#stack fonksiyonu ile eğitim durumu ve gelir olmak üzere iki sütun oluşturuldu
#Eğitim durumu faktör olarak belirlendi.

head(comb)
#    values  ind
# 1  57954 Master
# 2  26877 Master
# 3  44159 Master
# 4  88420 Master
# 5  31788 Master
# 6  59247 Master

#            ??pairwise.t.test
# Elimizde ikiden fazla grup olduğunda ve aralarında farklılık olduğunu saptadığımızda
# farklılığa sebep olan grupları çoklu karşılaştırma testi ile buluyoruz.

#           ??p.adjust.methods
# c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
#   "fdr", "none")


pairwise.t.test(comb$values,comb$ind,p.adjust.method = "none")

# Pairwise comparisons using t tests with pooled SD 
# 
# data:  comb$values and comb$ind 
# 
#            Master  PhD  Graduation
# PhD        0.554    -     0.078        
# Graduation 0.366  0.078     -         
# TwoNCycle  0.128  0.038   0.297     
# 
# P value adjustment method: none 

#Sadece TwoNCycle ve PhD gelirleri arasında fark olduğunu diğer gruplar arasında
#fark olmadığını %5 anlamlılık düzeyinde söyleyebiliriz. 











######10.3Adding (or Removing) a Grid#####
library(ggplot2)

g <- ggplot(customers_have_kid, aes(Income,na.rm=TRUE, MntWines)) +
  geom_point() +
  theme(panel.background = element_rect(fill = "white", colour = "Red"))+
  labs(title = "Veri: Income vs MntWines",
       x = "Income",
       y = "MntWines") +
  theme(panel.background = element_blank())
g1 <- g + theme(panel.grid.major =
                   element_line(color = "black", linetype = 3)) + 
  # linetype = 3 is dash
  theme(panel.grid.minor =
          element_line(color = "darkgrey", linetype = 4)) 
# linetype = 4 is dot dash
g1



######10.5Creating a Scatter Plot of Multiple Groups####
#Scatter
g2<-ggplot(data=customers_have_kid, aes(x=Income,y=MntWines, shape=Education , 
                                colour=Education))+ geom_point()
g2


######10.7Plotting the Regression Line of a Scatter Plot####

g3.1<- ggplot (customers_have_kid, aes(Income,na.rm=TRUE,MntWines )) +
  geom_point ()
g3.1
#Veri çiftlerini çizdi ve grafiğini çizdi.


g3.2 <- (g3.1 + geom_smooth (method = "lm", 
                      formula = y ~ x))
#Veri çiftlerine regresyon çizgisini ekledi

g3 <- lm (MntWines ~ Income, data = customers_have_kid)
g3
summary(g3)
#Modelin anlamlı olup olmadığını test etmek için regresyonunu hesapladı.


g3.3 <- (ggplot (customers_have_kid, aes(Income,na.rm=TRUE, MntWines)) +
       geom_point () +
       geom_abline (
         intercept = g3$coefficients [1],
         slope = g3$coefficients [2]
       ))
#Keşisim terimini ve eğimini hesaplayıp grafiğini ekledi.

cowplot::plot_grid(g3.2,g3.3, labels = "AUTO")
#Paketi ile 2 grafiği yanyana çizdirdi.


######10.9Creating One Scatter Plot for Each Group####
a <- data.frame(customers_have_kid, package = "MASS")
g4 <- ggplot(customers_have_kid, aes(MntWines, cocukvar)) +
  geom_point() +
  facet_wrap( ~ Marital_Status)
g4

######10.10Creating a Bar Chart####
g5 <- ggplot(customers_have_kid, aes(x=Year_Birth, y=NumWebVisitsMonth))+
  geom_bar(stat = "identity")
g5

g6 <- ggplot(customers_have_kid, aes(x=Year_Birth, y=MntWines))+
  geom_bar(stat = "identity")
g6


######10.12Coloring a Bar Chart####
g7 <-ggplot(data=customers_have_kid, aes(x=Year_Birth, y=NumWebVisitsMonth, fill = Year_Birth))+
  geom_bar(stat =  "identity", fill=Year_Birth)+
  labs(title = "Year Birth by NumWebVisitsMonth",
       x = "Year",
       y = "Visit Month")
g7

######10.13Plotting a Line from x and y Points####
g8 <-ggplot(customers_have_kid,aes(Income,MntWines))+geom_point()+geom_line()
g8

######10.14Changing the Type, Width, or Color of a Line####
g9 <- gggplot(customers_have_kid,aes(Income,MntWines))+
  geom_line(linetype=1,
            size=1,
            col="DarkGreen")
g9

######10.19Creating a Histogram####
library(ggplot2)
g10 <- ggplot(customers_have_kid)+geom_histogram(aes(Income),bins = 20)
g10

######10.20Adding a Density Estimate to a Histogram####

#bins = k sınıf
#k = 3.3*log(300)+1 = 20
g11 <- ggplot(customers_have_kid) + aes(x=Income) +
  geom_histogram(aes(y=..density..),bins=20) +
  geom_density()
g11

######10.21Creating a Normal Quantile–Quantile Plot####

g12 <- ggplot(customers_have_kid, aes(sample=Income)) +
  stat_qq() +
  stat_qq_line()

g12

######10.22Creating Other Quantile–Quantile Plots#####
g13<-ggplot(customers_have_kid)+
  aes(sample=Income)+
  geom_qq(distribution = qt,dparams =9.110103)+
  stat_qq_line(distribution = qt,dparams = 9.110103)

as.list(MASS::fitdistr(na.omit(customers_have_kid$Income), "t")$estimate)[["df"]]

######10.23Plotting a Variable in Multiple Colors#####

shade <- ifelse(customers_have_kid$Income >= 50813,"black","pink")

g14 <- ggplot(customers_have_kid) +
  aes(customers_have_kid$Year_Birth, customers_have_kid$Income) + labs(x ="YearBirth" ,y= "Income")+
  geom_point(color = shade)
g14

#Bu adımda ifelse fonksiyonu ile shade değişkenine ortalamadan [mean(Income,na.rm=T) = 50813.12] 
#yüksek gelire sahip müşteriler için siyah
#ortalamadan düşük gelirlere sahip müşteriler için pembe değerini atadık.

# Ardından ggplot fonksiyonu ile doğum yıllarına göre müşterilerin yıllık hane gelirini 
# gösteren saçılım grafiği olşturuldu. 

#En çok ortalama gelirin altında bir gelire sahip olan müşterilerin sayısı doğum yılının 
#1970-1980 olduğu aralıktadır.
#Grafiğe biraz daha dikkatli baktığımızda her yıl aralığından kısmen iki gruptan da eşit müşteriye
#sahip olduğumuzu söyleyebiliriz

######10.24Graphing a Function#####

install.packages("moments")
library(moments)

#Veri setinden müşterilerin yıllık hane geliri değişkeninin 
#değerleri çekildi ve kayıp veri bulundurma ihtimaline karşı
#na.rm komutuna True değeri atandı.
#Bu değişkene ilişkin normal dağılım oluşturabilmek için
#ggplot fonksiyonu kullanıldı.
#labs() fonksiyonu ile x ve y eksenlerine etiket eklendi.

#stat_funciton() fonskiyonu kullanıldı.Bu foknsiyon verilen bir fonksiyonun 
#grafiğini çizdirmemizi sağlar.

#ggtitle() fonksiyonu ile grafik başlığı eklendi
#ve tüm bu işlemler bir değişkene atandı.

#Master yapanların gelirlerinin dağılımının normal olup olmadığına bakmak için shapio.test()
#fonksiyonunu kullandık.
Master_Income_v <- as.vector(na.omit(customers_have_kid[Education == "Master","Income"]))
shapiro.test(Master_Income_v)
# Shapiro-Wilk normality test
# 
# data:  Master_Income
# W = 0.9793, p-value = 0.5363

#0.5363 > 0.05 olduğundan verilerin normal dağıldığını %5 anlamlılık düzeyinde söyleyebiliriz.

#Şimdi bir de Master yapanların gelirlerinin dağılımının grafiğini yazdıralım.
Master_Income_df <- as.data.frame(na.omit(customers_have_kid[Education == "Master","Income"]))
g15 <- ggplot(Master_Income_df, aes(Master_Income_df[,1])) +
      stat_function(fun=dnorm,
      args = with(Master_Income_df, c(mean = mean(Master_Income_df[,1]), sd = sd(Master_Income_df[,1])))) +
      labs(x = "Master's Income",y='Probability Density') +
      ggtitle("Normal Distribution")
g15

skewness(Master_Income_v)
# > skewness(Master_Income_v)
# [1] 0.438094
kurtosis(Master_Income_v)
# > kurtosis(Master_Income_v)
# [1] 2.704991

#Çarpıklık katsayısına göre elde ettiğimiz dağılımın sağa çarpık olduğunu ve
#sağ tarafta aykırı değerler olabileceğini söyleyebiliriz.
#Basıklık katsayısına baktığımızda ise dağılımın normal dağılıma göre daha sivri olduğunu söyleyebiliriz.

######10.26Writing Your Plot to a File####
??ggsave()

#ggsave() fonksiyonu grafikleri bilgisayara kaydetmemizi sağlar.
#width -> çizim çerçevesinin genişliği
#height -> çizim çerçevesinin yüksekliği 
#units -> genişlik ve yükseklik birimleri biz kendi grafiklerimiz için "in"(inç) kullandık.
ggsave("g1.jpeg", plot = g1, units = "in", width = 5, height = 4)
ggsave("g2.jpeg", plot = g2, units = "in", width = 5, height = 4)
ggsave("g3.1.jpeg", plot = g3.1, units = "in", width = 5, height = 4)
ggsave("g3.2.jpeg", plot = g3.2, units = "in", width = 5, height = 4)
ggsave("g3.3.jpeg", plot = g3.3, units = "in", width = 5, height = 4)
ggsave("g4.jpeg", plot = g4, units = "in", width = 5, height = 4)
ggsave("g5.jpeg", plot = g5, units = "in", width = 5, height = 4)
ggsave("g6.jpeg", plot = g6, units = "in", width = 5, height = 4)
ggsave("g7.jpeg", plot = g7, units = "in", width = 5, height = 4)
ggsave("g8.jpeg", plot = g8, units = "in", width = 5, height = 4)
ggsave("g9.jpeg", plot = g9, units = "in", width = 5, height = 4)
ggsave("g10.jpeg", plot = g10, units = "in", width = 5, height = 4)
ggsave("g11.jpeg", plot = g11, units = "in", width = 5, height = 4)
ggsave("g12.jpeg", plot = g12, units = "in", width = 5, height = 4)
ggsave("g13.jpeg", plot = g13, units = "in", width = 5, height = 4)
ggsave("g14.jpeg", plot = g14, units = "in", width = 5, height = 4)
ggsave("g15.jpeg", plot = g15, units = "in", width = 5, height = 4)

write.table(customers,file="customers.txt",sep=",")
#Bize verilen veri kümesi txt formatında yazıldı.



