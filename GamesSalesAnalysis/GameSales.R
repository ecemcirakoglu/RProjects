
#PARAMETRIK_OLMAYAN_ISTYONT_ODEV
#ECEMCIRAKOGLU
#KAANGURALLAR

library(dplyr)
library(DescTools)

PS4_GamesSales <- read.csv("~/Desktop/PS4_GamesSales.csv")
KisiBasinaOrtHarcamaTurizm <- read.table("~/Desktop/Enyeniveri (1).xlsx", header=TRUE, quote="\"")

attach(PS4_GamesSales)
attach(KisiBasinaOrtHarcamaTurizm)

filter1 <- PS4_GamesSales$Genre == "Action" & (PS4_GamesSales$Year != "N/A")
filter2 <- PS4_GamesSales$Genre == "Shooter" & (PS4_GamesSales$Year != "N/A")
filter3 <- PS4_GamesSales$Genre == "Sports" & (PS4_GamesSales$Year != "N/A")

action <- head(PS4_GamesSales[filter1,],n = 20) 
shooter <- head(PS4_GamesSales[filter2,],n = 20)
sports <- head(PS4_GamesSales[filter3,],n = 20)

summary(action[,"Global"])
summary(shooter[,"Global"])
summary(sports[,"Global"])
var(sports[,"Global"])

summary(Y2017)
summary(Y2018)
summary(Y2019)
var(Y2017)
var(Y2018)
var(Y2019)

######BAGIMSIZ VERI NORMALLLIK TEST######
shapiro.test(action[,"Global"])
# Shapiro-Wilk normality test
# 
# data:  action[, "Global"]
# W = 0.63263, p-value = 6.538e-06
# p=0.000006538

shapiro.test(shooter[,"Global"])
# Shapiro-Wilk normality test
# 
# data:  shooter[, "Global"]
# W = 0.80548, p-value = 0.001044

shapiro.test(sports[,"Global"])
# Shapiro-Wilk normality test
# 
# data:  sports[, "Global"]
# W = 0.76087, p-value = 0.0002384



######BAĞIMLI VERI NORMALLIK TEST######
shapiro.test(KisiBasinaOrtHarcamaTurizm[,"Y2017"])
# Shapiro-Wilk normality test
# 
# data:  `Enyeniveri.(1)`[, "Y2017"]
# W = 0.80565, p-value = 0.01087

shapiro.test(KisiBasinaOrtHarcamaTurizm[,"Y2018"])
# Shapiro-Wilk normality test
# 
# data:  `Enyeniveri.(1)`[, "Y2018"]
# W = 0.72547, p-value = 0.001499

shapiro.test(KisiBasinaOrtHarcamaTurizm[,"Y2019"])
# Shapiro-Wilk normality test
# 
# data:  `Enyeniveri.(1)`[, "Y2019"]
# W = 0.65226, p-value = 0.0003035




#########SORULAR############
#SORU3
wilcox.test(sports$Global,mu=5,alternative = "less")
# Wilcoxon signed rank exact test
# 
# data:  sports$Global
# V = 53, p-value = 0.02658
# alternative hypothesis: true location is less than 5

SignTest(x=sports$Global,mu=5,alternative = "less")
# One-sample Sign-Test
# 
# data:  sports$Global
# S = 4, number of differences = 20, p-value = 0.005909
# alternative hypothesis: true median is less than 5
# 97.9 percent confidence interval:
#   -Inf 3.43
# sample estimates:
#   median of the differences 
# 2.95 

#SORU4
wilcox.test(x=shooter$Global,y=action$Global,alternative = "two.sided",paired = FALSE,exact = FALSE)
# Wilcoxon rank sum test with continuity correction
# 
# data:  shooter$Global and action$Global
# W = 253, p-value = 0.1555
# alternative hypothesis: true location shift is not equal to 0

#soru4
birlesik <- data.frame(oyunlar=rep(c("Action","Shooter","Sports"),each=20),
                       global = c(action$Global,shooter$Global,sports$Global))

#####BUNLAR DENEME SORUYA DAHIL DEGIL####
# birlesik_sirali <- birlesik[order(birlesik$global,decreasing = FALSE),]
# #birlesik_sirali[,"sıra"] <- c(1:60)
# 
# 
# #sum(birlesik_sirali_sadece_a$sıra)
# birlesik_sirali[,"sıra"] <- rank(birlesik_sirali$global)
# 
# birlesik_sirali_sadece_a <- birlesik_sirali[birlesik_sirali$oyunlar=="Action",]
# sum(birlesik_sirali_sadece_a$sıra)
# 
# birlesik_sirali_sadece_sh <- birlesik_sirali[birlesik_sirali$oyunlar=="Shooter",]
# sum(birlesik_sirali_sadece_sh$sıra)
# 
# birlesik_sirali_sadece_sp <- birlesik_sirali[birlesik_sirali$oyunlar=="Sports",]
# sum(birlesik_sirali_sadece_sp$sıra)
# 
# birlesik_a_sh <- birlesik_sirali[birlesik_sirali$oyunlar=="Action" | birlesik_sirali$oyunlar=="Shooter",]
# birlesik_a_sh <- birlesik_a_sh[c(1,2)]
#birlesik_a_sh[,"sıra"] <-  rank(birlesik_a_sh[c(2)])
# birlesik_a_sh[,"oyunlar"]
# birlesik_a_sh[,"global"]

birlesik_sirali <- birlesik[order(birlesik$global,decreasing = FALSE),]
birlesik_sirali[,"sıra"] <- rank(birlesik_sirali$global)
sum(birlesik_sirali[birlesik_sirali$oyunlar=="Shooter","sıra"])

(454*454)/20
((19034.45+28804.05+10305.8)*0.003278689) - 3*61
12/(60*61)
kruskal.test(birlesik$global,birlesik$oyunlar)
??kruskal.test()
# Kruskal-Wallis rank sum test
# 
# data:  birlesik$global and birlesik$oyunlar
# Kruskal-Wallis chi-squared = 7.6375, df = 2, p-value = 0.02196

pairwise.wilcox.test(birlesik$global, birlesik$oyunlar,paired = FALSE,
                     p.adjust.method = "none",exact=FALSE)
# Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
# 
# data:  birlesik$global and birlesik$oyunlar 
# 
# Action Shooter
# Shooter 0.1555 -      
#   Sports  0.1075 0.0098 
# 
# P value adjustment method: none 

#SORU5
wilcox.test(x=`Enyeniveri.(1)`[,"Y2017"],y=`Enyeniveri.(1)`[,"Y2019"],alternative = "two.sided",paired = TRUE,exact=FALSE)
# Wilcoxon signed rank test with continuity correction
# 
# data:  `Enyeniveri.(1)`[, "Y2017"] and `Enyeniveri.(1)`[, "Y2019"]
# V = 34, p-value = 0.724
# alternative hypothesis: true location shift is not equal to 0


#SORU7
`Enyeniveri.(1)` <- read.table("~/Downloads/Enyeniveri (1).xlsx", header=TRUE, quote="\"")
attach(`Enyeniveri.(1)`)

data <-  data.frame(aylar = rep(Aylar,times=3),
                    yillar = rep(c("Y2017","Y2018","Y2019"),each=12),
                    maliyet= c(Y2017,Y2018,Y2019))

friedman.test(y=data$maliyet,groups=data$yillar,data$aylar)
friedman.test(maliyet~yillar|aylar,data=data)


















