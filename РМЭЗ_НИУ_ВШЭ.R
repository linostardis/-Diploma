library(haven)
library(sandwich)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(stargazer)
library(ggcorrplot)
library(car)
library(MASS)
library(dplyr)
library(lmtest)
library(corrplot)
library(gclus)
library(erer)
library(GGally)
library(foreign)

#### Загрузка данных ####
data_ind <- read.spss("/Users/bookmac/Downloads/IND_id_w_greater_15.sav", to.data.frame=TRUE)
data_hh <- read.spss("/Users/bookmac/Downloads/HH.sav", to.data.frame=TRUE)

#с 2015
data_hh0 <- filter(data_hh, id_w == '2015 год' | id_w == '2016 год'| id_w == '2017 год'|
                     id_w == '2018 год'| id_w == '2019 год'| id_w == '2020 год'|
                     id_w == '2021 год')
# 24 - 30 волны

#### Переменные ####
data_hh1 <- subset(data_hh0, select = c(
  id_w, id_h, origsam, 
  idind1, idind2, idind3, idind4, idind5, idind6, idind7, idind8, idind9, idind10,
  idind11, idind12, idind13, idind14, idind15, idind16, idind17, idind18, idind19, idind20,
  b2.9.1, 
  b3.9.1, b3.9.2,  
  b4.9.1, b4.9.2, b4.9.3,  
  b5.9.1, b5.9.2, b5.9.3, b5.9.4, 
  b6.9.1, b6.9.2, b6.9.3, b6.9.4, b6.9.5, 
  b7.9.1, b7.9.2, b7.9.3, b7.9.4, b7.9.5, b7.9.6,
  b8.9.1, b8.9.2, b8.9.3, b8.9.4, b8.9.5, b8.9.6, b8.9.7, 
  b9.9.1, b9.9.2, b9.9.3, b9.9.4, b9.9.5, b9.9.6, b9.9.7, b9.9.8,
  b10.9.1, b10.9.2, b10.9.3, b10.9.4, b10.9.5, b10.9.6, b10.9.7, b10.9.8, b10.9.9,
  b11.9.1, b11.9.2, b11.9.3, b11.9.4, b11.9.5, b11.9.6, b11.9.7, b11.9.8, b11.9.9, b119.10,
  b12.9.1, b12.9.2, b12.9.3, b12.9.4, b12.9.5, b12.9.6, b12.9.7, b12.9.8, b12.9.9, b129.10, b129.11,
  b13.9.1, b13.9.2, b13.9.3, b13.9.4, b13.9.5, b13.9.6, b13.9.7, b13.9.8, b13.9.9, b139.10, b139.11, b139.12,
  b14.9.1, b14.9.2, b14.9.3, b14.9.4, b14.9.5, b14.9.6, b14.9.7, b14.9.8, b14.9.9, b149.10, b149.11, b149.12, b149.13,
  b15.9.1, b15.9.2, b15.9.3, b15.9.4, b15.9.5, b15.9.6, b15.9.7, b15.9.8, b15.9.9, b159.10, b159.11, b159.12, b159.13, b159.14,
  b16.9.1, b16.9.2, b16.9.3, b16.9.4, b16.9.5, b16.9.6, b16.9.7, b16.9.8, b16.9.9, b169.10, b169.11, b169.12, b169.13, b169.14, b169.15,
  b17.9.1, b17.9.2, b17.9.3, b17.9.4, b17.9.5, b17.9.6, b17.9.7, b17.9.8, b17.9.9, b17.910, b17.911, b17.912, b17.913, b17.914, b17.915, b17.916,
  b18.9.1, b18.9.2, b18.9.3, b18.9.4, b18.9.5, b18.9.6, b18.9.7, b18.9.8, b18.9.9, b18.910, b18.911, b18.912, b18.913, b18.914, b18.915, b18.916, b18.917,
  b19.9.1, b19.9.2, b19.9.3, b19.9.4, b19.9.5, b19.9.6, b19.9.7, b19.9.8, b19.9.9, b19.910, b19.911, b19.912, b19.913, b19.914, b19.915, b19.916, b19.917, b19.918,
  b20.9.1, b20.9.2, b20.9.3, b20.9.4, b20.9.5, b20.9.6, b20.9.7, b20.9.8, b20.9.9, b20.910, b20.911, b20.912, b20.913, b20.914, b20.915, b20.916, b20.917, b20.918, b20.919
))

data_ind1 <- subset(data_ind, select = c(
  id_w, idind, year, id_h, origsm, born_m, adult, child, marst,
  educ, h5, h6, age, j60, 
  j72.172, j88, j196.1, j196.2, j196.3, j196.4, j200, k771.3a,
  k771.3b, k771.3c, k771.4a, k7.8.1, 
  h11, k3.2, k3.3, k3.4, psu, status 
))
#h11 - кто заполнял опросник

#оставляем репрезентативную
data_hh2 <- filter(data_hh1, origsam == 'Да, адрес репрезентативной выборки') %>% dplyr::select(-origsam)
data_ind2 <- filter(data_ind1, origsm == 'Да, адрес репрезентативной выборки') %>% dplyr::select(-origsm)

#взрослый и детский
data_child <- filter(data_ind2, child == 'Есть детский вопросник')
data_adult <- filter(data_ind2, adult == 'Есть взрослый вопросник')

#убираем переменные для взрослых
data_child <- subset(data_child, select = -c(
  child, adult, marst, educ, j72.172, j88, j196.1, j196.2, j196.3, j196.4, j200
))

#работаем с возрастом детей
class(data_child$age)
data_child$age <- as.numeric(as.character(data_child$age))
data_childplus6 <- filter(data_child, age >= 6)
data_childplus6_2 <- data_childplus6[!duplicated(data_childplus6), ] 
data_childless6 <- filter(data_child, age < 6)
dim(data_childless6)+dim(data_childplus6)

#убираем характеристики детей
data_adult <- subset(data_adult, select = -c(
  child, adult, k771.3a, k771.3b, k771.3c, k771.4a, k7.8.1,
  h11, k3.2
))
dim(data_adult)

####Объединяем детей с их родителями####

child_adult <- data.frame(matrix(nrow=0, ncol=5))
colnames(child_adult) <- c('id_hh','id_child','id_adult', 'connect', 'id_w')

sub_hh1 <- subset(data_hh2, b2.9.1 == 'сын/дочь')
for (row in 1:nrow(sub_hh1)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh1$id_h[row], sub_hh1$idind2[row], sub_hh1$idind1[row], sub_hh1$b2.9.1[row], sub_hh1$id_w[row])
}

sub_hh2 <- subset(data_hh2, b3.9.1 == 'сын/дочь')
for (row in 1:nrow(sub_hh2)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh2$id_h[row], sub_hh2$idind3[row], sub_hh2$idind1[row], sub_hh2$b3.9.1[row], sub_hh2$id_w[row])
}

sub_hh3 <- subset(data_hh2, b3.9.2 == 'сын/дочь')
for (row in 1:nrow(sub_hh3)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh3$id_h[row], sub_hh3$idind3[row], sub_hh3$idind2[row], sub_hh3$b3.9.2[row], sub_hh3$id_w[row])
}

sub_hh4 <- subset(data_hh2, b4.9.1 == 'сын/дочь')
for (row in 1:nrow(sub_hh4)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh4$id_h[row], sub_hh4$idind4[row], sub_hh4$idind1[row], sub_hh4$b4.9.1[row], sub_hh4$id_w[row])
}

sub_hh5 <- subset(data_hh2, b4.9.2 == 'сын/дочь')
for (row in 1:nrow(sub_hh5)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh5$id_h[row], sub_hh5$idind4[row], sub_hh5$idind2[row], sub_hh5$b4.9.2[row], sub_hh5$id_w[row])
}

sub_hh6 <- subset(data_hh2, b4.9.3 == 'сын/дочь')
for (row in 1:nrow(sub_hh6)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh6$id_h[row], sub_hh6$idind4[row], sub_hh6$idind3[row], sub_hh6$b4.9.3[row], sub_hh6$id_w[row])
}

sub_hh7 <- subset(data_hh2, b5.9.1 == 'сын/дочь')
for (row in 1:nrow(sub_hh7)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh7$id_h[row], sub_hh7$idind5[row], sub_hh7$idind1[row], sub_hh7$b5.9.1[row], sub_hh7$id_w[row])
}

sub_hh8 <- subset(data_hh2, b5.9.2 == 'сын/дочь')
for (row in 1:nrow(sub_hh8)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh8$id_h[row], sub_hh8$idind5[row], sub_hh8$idind2[row], sub_hh8$b5.9.2[row], sub_hh8$id_w[row])
}

sub_hh9 <- subset(data_hh2, b5.9.3 == 'сын/дочь')
for (row in 1:nrow(sub_hh9)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh9$id_h[row], sub_hh9$idind5[row], sub_hh9$idind3[row], sub_hh9$b5.9.3[row], sub_hh9$id_w[row])
}

sub_hh10 <- subset(data_hh2, b5.9.4 == 'сын/дочь')
for (row in 1:nrow(sub_hh10)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh10$id_h[row], sub_hh10$idind5[row], sub_hh10$idind4[row], sub_hh10$b5.9.4[row], sub_hh10$id_w[row])
}

sub_hh11 <- subset(data_hh2, b6.9.1 == 'сын/дочь')
for (row in 1:nrow(sub_hh11)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh11$id_h[row], sub_hh11$idind6[row], sub_hh11$idind1[row], sub_hh11$b6.9.1[row], sub_hh11$id_w[row])
}

sub_hh12 <- subset(data_hh2, b6.9.2 == 'сын/дочь')
for (row in 1:nrow(sub_hh12)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh12$id_h[row], sub_hh12$idind6[row], sub_hh12$idind2[row], sub_hh12$b6.9.2[row], sub_hh12$id_w[row])
}

sub_hh13 <- subset(data_hh2, b6.9.3 == 'сын/дочь')
for (row in 1:nrow(sub_hh13)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh13$id_h[row], sub_hh13$idind6[row], sub_hh13$idind3[row], sub_hh13$b6.9.3[row], sub_hh13$id_w[row])
}

sub_hh14 <- subset(data_hh2, b6.9.4 == 'сын/дочь')
for (row in 1:nrow(sub_hh14)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh14$id_h[row], sub_hh14$idind6[row], sub_hh14$idind4[row], sub_hh14$b6.9.4[row], sub_hh14$id_w[row])
}

sub_hh15 <- subset(data_hh2, b6.9.5 == 'сын/дочь')
for (row in 1:nrow(sub_hh15)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh15$id_h[row], sub_hh15$idind6[row], sub_hh15$idind5[row], sub_hh15$b6.9.5[row], sub_hh15$id_w[row])
}

sub_hh16 <- subset(data_hh2, b7.9.1 == 'сын/дочь')
for (row in 1:nrow(sub_hh16)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh16$id_h[row], sub_hh16$idind7[row], sub_hh16$idind1[row], sub_hh16$b7.9.1[row], sub_hh16$id_w[row])
}

sub_hh17 <- subset(data_hh2, b7.9.2 == 'сын/дочь')
for (row in 1:nrow(sub_hh17)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh17$id_h[row], sub_hh17$idind7[row], sub_hh17$idind2[row], sub_hh17$b7.9.2[row], sub_hh17$id_w[row])
}

sub_hh18 <- subset(data_hh2, b7.9.3 == 'сын/дочь')
for (row in 1:nrow(sub_hh18)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh18$id_h[row], sub_hh18$idind7[row], sub_hh18$idind3[row], sub_hh18$b7.9.3[row], sub_hh18$id_w[row])
}

sub_hh19 <- subset(data_hh2, b7.9.4 == 'сын/дочь')
for (row in 1:nrow(sub_hh19)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh19$id_h[row], sub_hh19$idind7[row], sub_hh19$idind4[row], sub_hh19$b7.9.4[row], sub_hh19$id_w[row])
}

sub_hh20 <- subset(data_hh2, b7.9.5 == 'сын/дочь')
for (row in 1:nrow(sub_hh20)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh20$id_h[row], sub_hh20$idind7[row], sub_hh20$idind5[row], sub_hh20$b7.9.5[row], sub_hh20$id_w[row])
}

sub_hh21 <- subset(data_hh2, b7.9.6 == 'сын/дочь')
for (row in 1:nrow(sub_hh21)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh21$id_h[row], sub_hh21$idind7[row], sub_hh21$idind6[row], sub_hh21$b7.9.6[row], sub_hh21$id_w[row])
}

sub_hh22 <- subset(data_hh2, b8.9.1 == 'сын/дочь')
for (row in 1:nrow(sub_hh22)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh22$id_h[row], sub_hh22$idind8[row], sub_hh22$idind1[row], sub_hh22$b8.9.1[row], sub_hh22$id_w[row])
}

sub_hh23 <- subset(data_hh2, b8.9.2 == 'сын/дочь')
for (row in 1:nrow(sub_hh23)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh23$id_h[row], sub_hh23$idind8[row], sub_hh23$idind2[row], sub_hh23$b8.9.2[row], sub_hh23$id_w[row])
}

sub_hh24 <- subset(data_hh2, b8.9.3 == 'сын/дочь')
for (row in 1:nrow(sub_hh24)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh24$id_h[row], sub_hh24$idind8[row], sub_hh24$idind3[row], sub_hh24$b8.9.3[row], sub_hh24$id_w[row])
}

sub_hh25 <- subset(data_hh2, b8.9.4 == 'сын/дочь')
for (row in 1:nrow(sub_hh25)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh25$id_h[row], sub_hh25$idind8[row], sub_hh25$idind4[row], sub_hh25$b8.9.4[row], sub_hh25$id_w[row])
}

sub_hh26 <- subset(data_hh2, b8.9.5 == 'сын/дочь')
for (row in 1:nrow(sub_hh26)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh26$id_h[row], sub_hh26$idind8[row], sub_hh26$idind5[row], sub_hh26$b8.9.5[row], sub_hh26$id_w[row])
}

sub_hh27 <- subset(data_hh2, b8.9.6 == 'сын/дочь')
for (row in 1:nrow(sub_hh27)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh27$id_h[row], sub_hh27$idind8[row], sub_hh27$idind6[row], sub_hh27$b8.9.6[row], sub_hh27$id_w[row])
}

sub_hh28 <- subset(data_hh2, b8.9.7 == 'сын/дочь')
for (row in 1:nrow(sub_hh28)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh28$id_h[row], sub_hh28$idind8[row], sub_hh28$idind7[row], sub_hh28$b8.9.7[row], sub_hh28$id_w[row])
}

sub_hh29 <- subset(data_hh2, b9.9.1 == 'сын/дочь')
for (row in 1:nrow(sub_hh29)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh29$id_h[row], sub_hh29$idind9[row], sub_hh29$idind1[row], sub_hh29$b9.9.1[row], sub_hh29$id_w[row])
}

sub_hh30 <- subset(data_hh2, b9.9.2 == 'сын/дочь')
for (row in 1:nrow(sub_hh30)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh30$id_h[row], sub_hh30$idind9[row], sub_hh30$idind2[row], sub_hh30$b9.9.2[row], sub_hh30$id_w[row])
}

sub_hh31 <- subset(data_hh2, b9.9.3 == 'сын/дочь')
for (row in 1:nrow(sub_hh31)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh31$id_h[row], sub_hh31$idind9[row], sub_hh31$idind3[row], sub_hh31$b9.9.3[row], sub_hh31$id_w[row])
}

sub_hh32 <- subset(data_hh2, b9.9.4 == 'сын/дочь')
for (row in 1:nrow(sub_hh32)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh32$id_h[row], sub_hh32$idind9[row], sub_hh32$idind4[row], sub_hh32$b9.9.4[row], sub_hh32$id_w[row])
}

sub_hh33 <- subset(data_hh2, b9.9.5 == 'сын/дочь')
for (row in 1:nrow(sub_hh33)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh33$id_h[row], sub_hh33$idind9[row], sub_hh33$idind5[row], sub_hh33$b9.9.5[row], sub_hh33$id_w[row])
}

sub_hh34 <- subset(data_hh2, b9.9.6 == 'сын/дочь')
for (row in 1:nrow(sub_hh34)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh34$id_h[row], sub_hh34$idind9[row], sub_hh34$idind6[row], sub_hh34$b9.9.6[row], sub_hh34$id_w[row])
}

sub_hh35 <- subset(data_hh2, b9.9.7 == 'сын/дочь')
for (row in 1:nrow(sub_hh35)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh35$id_h[row], sub_hh35$idind9[row], sub_hh35$idind7[row], sub_hh35$b9.9.7[row], sub_hh35$id_w[row])
}

sub_hh36 <- subset(data_hh2, b9.9.8 == 'сын/дочь')
for (row in 1:nrow(sub_hh36)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh36$id_h[row], sub_hh36$idind9[row], sub_hh36$idind8[row], sub_hh36$b9.9.8[row], sub_hh36$id_w[row])
}

sub_hh37 <- subset(data_hh2, b10.9.1 == 'сын/дочь')
for (row in 1:nrow(sub_hh37)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh37$id_h[row], sub_hh37$idind10[row], sub_hh37$idind1[row], sub_hh37$b10.9.1[row], sub_hh37$id_w[row])
}

sub_hh38 <- subset(data_hh2, b10.9.2 == 'сын/дочь')
for (row in 1:nrow(sub_hh38)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh38$id_h[row], sub_hh38$idind10[row], sub_hh38$idind2[row], sub_hh38$b10.9.2[row], sub_hh38$id_w[row])
}

sub_hh39 <- subset(data_hh2, b10.9.3 == 'сын/дочь')
for (row in 1:nrow(sub_hh39)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh39$id_h[row], sub_hh39$idind10[row], sub_hh39$idind3[row], sub_hh39$b10.9.3[row], sub_hh39$id_w[row])
}

sub_hh40 <- subset(data_hh2, b10.9.4 == 'сын/дочь')
for (row in 1:nrow(sub_hh40)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh40$id_h[row], sub_hh40$idind10[row], sub_hh40$idind4[row], sub_hh40$b10.9.4[row], sub_hh40$id_w[row])
}

sub_hh41 <- subset(data_hh2, b10.9.5 == 'сын/дочь')
for (row in 1:nrow(sub_hh41)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh41$id_h[row], sub_hh41$idind10[row], sub_hh41$idind5[row], sub_hh41$b10.9.5[row], sub_hh41$id_w[row])
}

sub_hh42 <- subset(data_hh2, b10.9.6 == 'сын/дочь')
for (row in 1:nrow(sub_hh42)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh42$id_h[row], sub_hh42$idind10[row], sub_hh42$idind6[row], sub_hh42$b10.9.6[row], sub_hh42$id_w[row])
}

sub_hh43 <- subset(data_hh2, b10.9.7 == 'сын/дочь')
for (row in 1:nrow(sub_hh43)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh43$id_h[row], sub_hh43$idind10[row], sub_hh43$idind7[row], sub_hh43$b10.9.7[row], sub_hh43$id_w[row])
}

sub_hh44 <- subset(data_hh2, b10.9.8 == 'сын/дочь')
for (row in 1:nrow(sub_hh44)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh44$id_h[row], sub_hh44$idind10[row], sub_hh44$idind8[row], sub_hh44$b10.9.8[row], sub_hh44$id_w[row])
}

sub_hh45 <- subset(data_hh2, b10.9.9 == 'сын/дочь')
for (row in 1:nrow(sub_hh45)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh45$id_h[row], sub_hh45$idind10[row], sub_hh45$idind9[row], sub_hh45$b10.9.9[row], sub_hh45$id_w[row])
}

sub_hh46 <- subset(data_hh2, b11.9.1 == 'сын/дочь')
for (row in 1:nrow(sub_hh46)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh46$id_h[row], sub_hh46$idind11[row], sub_hh46$idind1[row], sub_hh46$b11.9.1[row], sub_hh46$id_w[row])
}

sub_hh47 <- subset(data_hh2, b11.9.2 == 'сын/дочь')
for (row in 1:nrow(sub_hh47)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh47$id_h[row], sub_hh47$idind11[row], sub_hh47$idind2[row], sub_hh47$b11.9.2[row], sub_hh47$id_w[row])
}

sub_hh48 <- subset(data_hh2, b11.9.3 == 'сын/дочь')
for (row in 1:nrow(sub_hh48)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh48$id_h[row], sub_hh48$idind11[row], sub_hh48$idind3[row], sub_hh48$b11.9.3[row], sub_hh48$id_w[row])
}

sub_hh49 <- subset(data_hh2, b11.9.4 == 'сын/дочь')
for (row in 1:nrow(sub_hh49)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh49$id_h[row], sub_hh49$idind11[row], sub_hh49$idind4[row], sub_hh49$b11.9.4[row], sub_hh49$id_w[row])
}

sub_hh50 <- subset(data_hh2, b11.9.5 == 'сын/дочь')
for (row in 1:nrow(sub_hh50)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh50$id_h[row], sub_hh50$idind11[row], sub_hh50$idind5[row], sub_hh50$b11.9.5[row], sub_hh50$id_w[row])
}

sub_hh51 <- subset(data_hh2, b11.9.6 == 'сын/дочь')
for (row in 1:nrow(sub_hh51)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh51$id_h[row], sub_hh51$idind11[row], sub_hh51$idind6[row], sub_hh51$b11.9.6[row], sub_hh51$id_w[row])
}

sub_hh52 <- subset(data_hh2, b11.9.7 == 'сын/дочь')
for (row in 1:nrow(sub_hh52)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh52$id_h[row], sub_hh52$idind11[row], sub_hh52$idind7[row], sub_hh52$b11.9.7[row], sub_hh52$id_w[row])
}

sub_hh53 <- subset(data_hh2, b11.9.8 == 'сын/дочь')
for (row in 1:nrow(sub_hh53)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh53$id_h[row], sub_hh53$idind11[row], sub_hh53$idind8[row], sub_hh53$b11.9.8[row], sub_hh53$id_w[row])
}

sub_hh54 <- subset(data_hh2, b11.9.9 == 'сын/дочь')
for (row in 1:nrow(sub_hh54)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh54$id_h[row], sub_hh54$idind11[row], sub_hh54$idind9[row], sub_hh54$b11.9.9[row], sub_hh54$id_w[row])
}

sub_hh55 <- subset(data_hh2, b119.10 == 'сын/дочь')
for (row in 1:nrow(sub_hh55)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh55$id_h[row], sub_hh55$idind11[row], sub_hh55$idind10[row], sub_hh55$b119.10[row], sub_hh55$id_w[row])
}

sub_hh56 <- subset(data_hh2, b12.9.1 == 'сын/дочь')
for (row in 1:nrow(sub_hh56)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh56$id_h[row], sub_hh56$idind12[row], sub_hh56$idind1[row], sub_hh56$b12.9.1[row], sub_hh56$id_w[row])
}

sub_hh57 <- subset(data_hh2, b12.9.2 == 'сын/дочь')
for (row in 1:nrow(sub_hh57)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh57$id_h[row], sub_hh57$idind12[row], sub_hh57$idind2[row], sub_hh57$b12.9.2[row], sub_hh57$id_w[row])
}

sub_hh58 <- subset(data_hh2, b12.9.3 == 'сын/дочь')
for (row in 1:nrow(sub_hh58)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh58$id_h[row], sub_hh58$idind12[row], sub_hh58$idind3[row], sub_hh58$b12.9.3[row], sub_hh58$id_w[row])
}

sub_hh59 <- subset(data_hh2, b12.9.4 == 'сын/дочь')
for (row in 1:nrow(sub_hh59)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh59$id_h[row], sub_hh59$idind12[row], sub_hh59$idind4[row], sub_hh59$b12.9.4[row], sub_hh59$id_w[row])
}

sub_hh60 <- subset(data_hh2, b12.9.5 == 'сын/дочь')
for (row in 1:nrow(sub_hh60)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh60$id_h[row], sub_hh60$idind12[row], sub_hh60$idind5[row], sub_hh60$b12.9.5[row], sub_hh60$id_w[row])
}

sub_hh61 <- subset(data_hh2, b12.9.6 == 'сын/дочь')
for (row in 1:nrow(sub_hh61)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh61$id_h[row], sub_hh61$idind12[row], sub_hh61$idind6[row], sub_hh61$b12.9.6[row], sub_hh61$id_w[row])
}

sub_hh62 <- subset(data_hh2, b12.9.7 == 'сын/дочь')
for (row in 1:nrow(sub_hh62)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh62$id_h[row], sub_hh62$idind12[row], sub_hh62$idind7[row], sub_hh62$b12.9.7[row], sub_hh62$id_w[row])
}

sub_hh63 <- subset(data_hh2, b12.9.8 == 'сын/дочь')
for (row in 1:nrow(sub_hh63)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh63$id_h[row], sub_hh63$idind12[row], sub_hh63$idind8[row], sub_hh63$b12.9.8[row], sub_hh63$id_w[row])
}

sub_hh64 <- subset(data_hh2, b12.9.9 == 'сын/дочь')
for (row in 1:nrow(sub_hh64)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh64$id_h[row], sub_hh64$idind12[row], sub_hh64$idind9[row], sub_hh64$b12.9.9[row], sub_hh64$id_w[row])
}

sub_hh65 <- subset(data_hh2, b129.10 == 'сын/дочь')
for (row in 1:nrow(sub_hh65)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh65$id_h[row], sub_hh65$idind12[row], sub_hh65$idind10[row], sub_hh65$b129.10[row], sub_hh65$id_w[row])
}

sub_hh66 <- subset(data_hh2, b129.11 == 'сын/дочь')
for (row in 1:nrow(sub_hh66)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh66$id_h[row], sub_hh66$idind12[row], sub_hh66$idind11[row], sub_hh66$b129.11[row], sub_hh66$id_w[row])
}

# sub_hh67 <- subset(data_hh2, b13.9.1 == 'сын/дочь')
# for (row in 1:nrow(sub_hh67)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh67$id_h[row], sub_hh67$idind13[row], sub_hh67$idind1[row], sub_hh67$b13.9.1[row], sub_hh67$id_w[row])
# }

sub_hh68 <- subset(data_hh2, b13.9.2 == 'сын/дочь')
for (row in 1:nrow(sub_hh68)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh68$id_h[row], sub_hh68$idind13[row], sub_hh68$idind2[row], sub_hh68$b13.9.2[row], sub_hh68$id_w[row])
}

sub_hh69 <- subset(data_hh2, b13.9.3 == 'сын/дочь')
for (row in 1:nrow(sub_hh69)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh69$id_h[row], sub_hh69$idind13[row], sub_hh69$idind3[row], sub_hh69$b13.9.3[row], sub_hh69$id_w[row])
}

sub_hh70 <- subset(data_hh2, b13.9.4 == 'сын/дочь')
for (row in 1:nrow(sub_hh70)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh70$id_h[row], sub_hh70$idind13[row], sub_hh70$idind4[row], sub_hh70$b13.9.4[row], sub_hh70$id_w[row])
}

sub_hh71 <- subset(data_hh2, b13.9.5 == 'сын/дочь')
for (row in 1:nrow(sub_hh71)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh71$id_h[row], sub_hh71$idind13[row], sub_hh71$idind5[row], sub_hh71$b13.9.5[row], sub_hh71$id_w[row])
}

sub_hh72 <- subset(data_hh2, b13.9.6 == 'сын/дочь')
for (row in 1:nrow(sub_hh72)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh72$id_h[row], sub_hh72$idind13[row], sub_hh72$idind6[row], sub_hh72$b13.9.6[row], sub_hh72$id_w[row])
}

sub_hh73 <- subset(data_hh2, b13.9.7 == 'сын/дочь')
for (row in 1:nrow(sub_hh73)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh73$id_h[row], sub_hh73$idind13[row], sub_hh73$idind7[row], sub_hh73$b13.9.7[row], sub_hh73$id_w[row])
}
# 
# sub_hh74 <- subset(data_hh2, b13.9.8 == 'сын/дочь')
# for (row in 1:nrow(sub_hh74)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh74$id_h[row], sub_hh74$idind13[row], sub_hh74$idind8[row], sub_hh74$b13.9.8[row], sub_hh74$id_w[row])
# }

# sub_hh75 <- subset(data_hh2, b13.9.9 == 'сын/дочь')
# for (row in 1:nrow(sub_hh75)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh75$id_h[row], sub_hh75$idind13[row], sub_hh75$idind9[row], sub_hh75$b13.9.9[row], sub_hh75$id_w[row])
# }

sub_hh76 <- subset(data_hh2, b139.10 == 'сын/дочь')
for (row in 1:nrow(sub_hh76)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh76$id_h[row], sub_hh76$idind13[row], sub_hh76$idind10[row], sub_hh76$b139.10[row], sub_hh76$id_w[row])
}

sub_hh77 <- subset(data_hh2, b139.11 == 'сын/дочь')
for (row in 1:nrow(sub_hh77)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh77$id_h[row], sub_hh77$idind13[row], sub_hh77$idind11[row], sub_hh77$b139.11[row], sub_hh77$id_w[row])
}

sub_hh78 <- subset(data_hh2, b139.12 == 'сын/дочь')
for (row in 1:nrow(sub_hh78)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh78$id_h[row], sub_hh78$idind13[row], sub_hh78$idind12[row], sub_hh78$b139.12[row], sub_hh78$id_w[row])
}

# sub_hh79 <- subset(data_hh2, b14.9.1 == 'сын/дочь')
# for (row in 1:nrow(sub_hh79)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh79$id_h[row], sub_hh79$idind14[row], sub_hh79$idind1[row], sub_hh79$b14.9.1[row], sub_hh79$id_w[row])
# }

# sub_hh80 <- subset(data_hh2, b14.9.2 == 'сын/дочь')
# for (row in 1:nrow(sub_hh80)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh80$id_h[row], sub_hh80$idind14[row], sub_hh80$idind2[row], sub_hh80$b14.9.2[row], sub_hh80$id_w[row])
# }

sub_hh81 <- subset(data_hh2, b14.9.3 == 'сын/дочь')
for (row in 1:nrow(sub_hh81)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh81$id_h[row], sub_hh81$idind14[row], sub_hh81$idind3[row], sub_hh81$b14.9.3[row], sub_hh81$id_w[row])
}

sub_hh82 <- subset(data_hh2, b14.9.4 == 'сын/дочь')
for (row in 1:nrow(sub_hh82)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh82$id_h[row], sub_hh82$idind14[row], sub_hh82$idind4[row], sub_hh82$b14.9.4[row], sub_hh82$id_w[row])
}

# sub_hh83 <- subset(data_hh2, b14.9.5 == 'сын/дочь')
# for (row in 1:nrow(sub_hh83)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh83$id_h[row], sub_hh83$idind14[row], sub_hh83$idind5[row], sub_hh83$b14.9.5[row], sub_hh83$id_w[row])
# }

sub_hh84 <- subset(data_hh2, b14.9.6 == 'сын/дочь')
for (row in 1:nrow(sub_hh84)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh84$id_h[row], sub_hh84$idind14[row], sub_hh84$idind6[row], sub_hh84$b14.9.6[row], sub_hh84$id_w[row])
}

sub_hh85 <- subset(data_hh2, b14.9.7 == 'сын/дочь')
for (row in 1:nrow(sub_hh85)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh85$id_h[row], sub_hh85$idind14[row], sub_hh85$idind7[row], sub_hh85$b14.9.7[row], sub_hh85$id_w[row])
}

sub_hh86 <- subset(data_hh2, b14.9.8 == 'сын/дочь')
for (row in 1:nrow(sub_hh86)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh86$id_h[row], sub_hh86$idind14[row], sub_hh86$idind8[row], sub_hh86$b14.9.8[row], sub_hh86$id_w[row])
}

sub_hh87 <- subset(data_hh2, b14.9.9 == 'сын/дочь')
for (row in 1:nrow(sub_hh87)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh87$id_h[row], sub_hh87$idind14[row], sub_hh87$idind9[row], sub_hh87$b14.9.9[row], sub_hh87$id_w[row])
}

# sub_hh88 <- subset(data_hh2, b149.10 == 'сын/дочь')
# for (row in 1:nrow(sub_hh88)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh88$id_h[row], sub_hh88$idind14[row], sub_hh88$idind10[row], sub_hh88$b149.10[row], sub_hh88$id_w[row])
# }

# sub_hh89 <- subset(data_hh2, b149.11 == 'сын/дочь')
# for (row in 1:nrow(sub_hh89)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh89$id_h[row], sub_hh89$idind14[row], sub_hh89$idind11[row], sub_hh89$b149.11[row], sub_hh89$id_w[row])
# }

sub_hh90 <- subset(data_hh2, b149.12 == 'сын/дочь')
for (row in 1:nrow(sub_hh90)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh90$id_h[row], sub_hh90$idind14[row], sub_hh90$idind12[row], sub_hh90$b149.12[row], sub_hh90$id_w[row])
}

sub_hh91 <- subset(data_hh2, b149.13 == 'сын/дочь')
for (row in 1:nrow(sub_hh91)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh91$id_h[row], sub_hh91$idind14[row], sub_hh91$idind13[row], sub_hh91$b149.13[row], sub_hh91$id_w[row])
}

# sub_hh92 <- subset(data_hh2, b15.9.1 == 'сын/дочь')
# for (row in 1:nrow(sub_hh92)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh92$id_h[row], sub_hh92$idind15[row], sub_hh92$idind1[row], sub_hh92$b15.9.1[row], sub_hh92$id_w[row])
# }

# sub_hh93 <- subset(data_hh2, b15.9.2 == 'сын/дочь')
# for (row in 1:nrow(sub_hh93)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh93$id_h[row], sub_hh93$idind15[row], sub_hh93$idind2[row], sub_hh93$b15.9.2[row], sub_hh93$id_w[row])
# }

sub_hh94 <- subset(data_hh2, b15.9.3 == 'сын/дочь')
for (row in 1:nrow(sub_hh94)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh94$id_h[row], sub_hh94$idind15[row], sub_hh94$idind3[row], sub_hh94$b15.9.3[row], sub_hh94$id_w[row])
}

sub_hh95 <- subset(data_hh2, b15.9.4 == 'сын/дочь')
for (row in 1:nrow(sub_hh95)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh95$id_h[row], sub_hh95$idind15[row], sub_hh95$idind4[row], sub_hh95$b15.9.4[row], sub_hh95$id_w[row])
}

# sub_hh96 <- subset(data_hh2, b15.9.5 == 'сын/дочь')
# for (row in 1:nrow(sub_hh96)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh96$id_h[row], sub_hh96$idind15[row], sub_hh96$idind5[row], sub_hh96$b15.9.5[row], sub_hh96$id_w[row])
# }

# sub_hh97 <- subset(data_hh2, b15.9.6 == 'сын/дочь')
# for (row in 1:nrow(sub_hh97)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh97$id_h[row], sub_hh97$idind15[row], sub_hh97$idind6[row], sub_hh97$b15.9.6[row], sub_hh97$id_w[row])
# }

sub_hh98 <- subset(data_hh2, b15.9.7 == 'сын/дочь')
for (row in 1:nrow(sub_hh98)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh98$id_h[row], sub_hh98$idind15[row], sub_hh98$idind7[row], sub_hh98$b15.9.7[row], sub_hh98$id_w[row])
}

sub_hh99 <- subset(data_hh2, b15.9.8 == 'сын/дочь')
for (row in 1:nrow(sub_hh99)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh99$id_h[row], sub_hh99$idind15[row], sub_hh99$idind8[row], sub_hh99$b15.9.8[row], sub_hh99$id_w[row])
}

# sub_hh100 <- subset(data_hh2, b15.9.9 == 'сын/дочь')
# for (row in 1:nrow(sub_hh100)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh100$id_h[row], sub_hh100$idind15[row], sub_hh100$idind9[row], sub_hh100$b15.9.9[row], sub_hh100$id_w[row])
# }

# sub_hh101 <- subset(data_hh2, b159.10 == 'сын/дочь')
# for (row in 1:nrow(sub_hh101)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh101$id_h[row], sub_hh101$idind15[row], sub_hh101$idind10[row], sub_hh101$b159.10[row], sub_hh101$id_w[row])
# }

# sub_hh102 <- subset(data_hh2, b159.11 == 'сын/дочь')
# for (row in 1:nrow(sub_hh102)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh102$id_h[row], sub_hh102$idind15[row], sub_hh102$idind11[row], sub_hh102$b159.11[row], sub_hh102$id_w[row])
# }

sub_hh103 <- subset(data_hh2, b159.12 == 'сын/дочь')
for (row in 1:nrow(sub_hh103)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh103$id_h[row], sub_hh103$idind15[row], sub_hh103$idind12[row], sub_hh103$b159.12[row], sub_hh103$id_w[row])
}

sub_hh104 <- subset(data_hh2, b159.13 == 'сын/дочь')
for (row in 1:nrow(sub_hh104)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh104$id_h[row], sub_hh104$idind15[row], sub_hh104$idind13[row], sub_hh104$b159.13[row], sub_hh104$id_w[row])
}

# sub_hh105 <- subset(data_hh2, b159.14 == 'сын/дочь')
# for (row in 1:nrow(sub_hh105)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh105$id_h[row], sub_hh105$idind15[row], sub_hh105$idind14[row], sub_hh105$b159.14[row], sub_hh105$id_w[row])
# }

# sub_hh106 <- subset(data_hh2, b16.9.1 == 'сын/дочь')
# for (row in 1:nrow(sub_hh106)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh106$id_h[row], sub_hh106$idind16[row], sub_hh106$idind1[row], sub_hh106$b16.9.1[row], sub_hh106$id_w[row])
# }

# sub_hh107 <- subset(data_hh2, b16.9.2 == 'сын/дочь')
# for (row in 1:nrow(sub_hh107)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh107$id_h[row], sub_hh107$idind16[row], sub_hh107$idind2[row], sub_hh107$b16.9.2[row], sub_hh107$id_w[row])
# }

# sub_hh108 <- subset(data_hh2, b16.9.3 == 'сын/дочь')
# for (row in 1:nrow(sub_hh108)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh108$id_h[row], sub_hh108$idind16[row], sub_hh108$idind3[row], sub_hh108$b16.9.3[row], sub_hh108$id_w[row])
# }

# sub_hh109 <- subset(data_hh2, b16.9.4 == 'сын/дочь')
# for (row in 1:nrow(sub_hh109)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh109$id_h[row], sub_hh109$idind16[row], sub_hh109$idind4[row], sub_hh109$b16.9.4[row], sub_hh109$id_w[row])
# }
# 
# sub_hh110 <- subset(data_hh2, b16.9.5 == 'сын/дочь')
# for (row in 1:nrow(sub_hh110)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh110$id_h[row], sub_hh110$idind16[row], sub_hh110$idind5[row], sub_hh110$b16.9.5[row], sub_hh110$id_w[row])
# }
# 
# sub_hh111 <- subset(data_hh2, b16.9.6 == 'сын/дочь')
# for (row in 1:nrow(sub_hh111)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh111$id_h[row], sub_hh111$idind16[row], sub_hh111$idind6[row], sub_hh111$b16.9.6[row], sub_hh111$id_w[row])
# }

# sub_hh112 <- subset(data_hh2, b16.9.7 == 'сын/дочь')
# for (row in 1:nrow(sub_hh112)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh112$id_h[row], sub_hh112$idind16[row], sub_hh112$idind7[row], sub_hh112$b16.9.7[row], sub_hh112$id_w[row])
# }
# 
# sub_hh113 <- subset(data_hh2, b16.9.8 == 'сын/дочь')
# for (row in 1:nrow(sub_hh113)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh113$id_h[row], sub_hh113$idind16[row], sub_hh113$idind8[row], sub_hh113$b16.9.8[row], sub_hh113$id_w[row])
# }
# 
# sub_hh114 <- subset(data_hh2, b16.9.9 == 'сын/дочь')
# for (row in 1:nrow(sub_hh114)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh114$id_h[row], sub_hh114$idind16[row], sub_hh114$idind9[row], sub_hh114$b16.9.9[row], sub_hh114$id_w[row])
# }

sub_hh115 <- subset(data_hh2, b169.10 == 'сын/дочь')
for (row in 1:nrow(sub_hh115)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh115$id_h[row], sub_hh115$idind16[row], sub_hh115$idind10[row], sub_hh115$b169.10[row], sub_hh115$id_w[row])
}

# sub_hh116 <- subset(data_hh2, b169.11 == 'сын/дочь')
# for (row in 1:nrow(sub_hh116)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh116$id_h[row], sub_hh116$idind16[row], sub_hh116$idind11[row], sub_hh116$b169.11[row], sub_hh116$id_w[row])
# }

# sub_hh117 <- subset(data_hh2, b169.12 == 'сын/дочь')
# for (row in 1:nrow(sub_hh117)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh117$id_h[row], sub_hh117$idind16[row], sub_hh117$idind12[row], sub_hh117$b169.12[row], sub_hh117$id_w[row])
# }

sub_hh118 <- subset(data_hh2, b169.13 == 'сын/дочь')
for (row in 1:nrow(sub_hh118)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh118$id_h[row], sub_hh118$idind16[row], sub_hh118$idind13[row], sub_hh118$b169.13[row], sub_hh118$id_w[row])
}

# sub_hh119 <- subset(data_hh2, b169.14 == 'сын/дочь')
# for (row in 1:nrow(sub_hh119)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh119$id_h[row], sub_hh119$idind16[row], sub_hh119$idind14[row], sub_hh119$b169.14[row], sub_hh119$id_w[row])
# }

# sub_hh120 <- subset(data_hh2, b169.15 == 'сын/дочь')
# for (row in 1:nrow(sub_hh120)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh120$id_h[row], sub_hh120$idind16[row], sub_hh120$idind15[row], sub_hh120$b169.15[row], sub_hh120$id_w[row])
# }
# 
# sub_hh121 <- subset(data_hh2, b17.9.1 == 'сын/дочь')
# for (row in 1:nrow(sub_hh121)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh121$id_h[row], sub_hh121$idind17[row], sub_hh121$idind1[row], sub_hh121$b17.9.1[row], sub_hh121$id_w[row])
# }
# 
# sub_hh122 <- subset(data_hh2, b17.9.2 == 'сын/дочь')
# for (row in 1:nrow(sub_hh122)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh122$id_h[row], sub_hh122$idind17[row], sub_hh122$idind2[row], sub_hh122$b17.9.2[row], sub_hh122$id_w[row])
# }

sub_hh123 <- subset(data_hh2, b17.9.3 == 'сын/дочь')
for (row in 1:nrow(sub_hh123)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh123$id_h[row], sub_hh123$idind17[row], sub_hh123$idind3[row], sub_hh123$b17.9.3[row], sub_hh123$id_w[row])
}

# sub_hh124 <- subset(data_hh2, b17.9.4 == 'сын/дочь')
# for (row in 1:nrow(sub_hh124)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh124$id_h[row], sub_hh124$idind17[row], sub_hh124$idind4[row], sub_hh124$b17.9.4[row], sub_hh124$id_w[row])
# }

# sub_hh125 <- subset(data_hh2, b17.9.5 == 'сын/дочь')
# for (row in 1:nrow(sub_hh125)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh125$id_h[row], sub_hh125$idind17[row], sub_hh125$idind5[row], sub_hh125$b17.9.5[row], sub_hh125$id_w[row])
# }
# 
# sub_hh126 <- subset(data_hh2, b17.9.6 == 'сын/дочь')
# for (row in 1:nrow(sub_hh126)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh126$id_h[row], sub_hh126$idind17[row], sub_hh126$idind6[row], sub_hh126$b17.9.6[row], sub_hh126$id_w[row])
# }

sub_hh127 <- subset(data_hh2, b17.9.7 == 'сын/дочь')
for (row in 1:nrow(sub_hh127)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh127$id_h[row], sub_hh127$idind17[row], sub_hh127$idind7[row], sub_hh127$b17.9.7[row], sub_hh127$id_w[row])
}

sub_hh128 <- subset(data_hh2, b17.9.8 == 'сын/дочь')
for (row in 1:nrow(sub_hh128)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh128$id_h[row], sub_hh128$idind17[row], sub_hh128$idind8[row], sub_hh128$b17.9.8[row], sub_hh128$id_w[row])
}

# sub_hh129 <- subset(data_hh2, b17.9.9 == 'сын/дочь')
# for (row in 1:nrow(sub_hh129)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh129$id_h[row], sub_hh129$idind17[row], sub_hh129$idind9[row], sub_hh129$b17.9.9[row], sub_hh129$id_w[row])
# }

# sub_hh130 <- subset(data_hh2, b17.910 == 'сын/дочь')
# for (row in 1:nrow(sub_hh130)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh130$id_h[row], sub_hh130$idind17[row], sub_hh130$idind10[row], sub_hh130$b17.910[row], sub_hh130$id_w[row])
# }

# sub_hh131 <- subset(data_hh2, b17.911 == 'сын/дочь')
# for (row in 1:nrow(sub_hh131)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh131$id_h[row], sub_hh131$idind17[row], sub_hh131$idind11[row], sub_hh131$b17.911[row], sub_hh131$id_w[row])
# }
# 
# sub_hh132 <- subset(data_hh2, b17.912 == 'сын/дочь')
# for (row in 1:nrow(sub_hh132)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh132$id_h[row], sub_hh132$idind17[row], sub_hh132$idind12[row], sub_hh132$b17.912[row], sub_hh132$id_w[row])
# }

sub_hh133 <- subset(data_hh2, b17.913 == 'сын/дочь')
for (row in 1:nrow(sub_hh133)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh133$id_h[row], sub_hh133$idind17[row], sub_hh133$idind13[row], sub_hh133$b17.913[row], sub_hh133$id_w[row])
}

# sub_hh134 <- subset(data_hh2, b17.914 == 'сын/дочь')
# for (row in 1:nrow(sub_hh134)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh134$id_h[row], sub_hh134$idind17[row], sub_hh134$idind14[row], sub_hh134$b17.914[row], sub_hh134$id_w[row])
# }

# sub_hh135 <- subset(data_hh2, b17.915 == 'сын/дочь')
# for (row in 1:nrow(sub_hh135)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh135$id_h[row], sub_hh135$idind17[row], sub_hh135$idind15[row], sub_hh135$b17.915[row], sub_hh135$id_w[row])
# }

# sub_hh136 <- subset(data_hh2, b17.916 == 'сын/дочь')
# for (row in 1:nrow(sub_hh136)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh136$id_h[row], sub_hh136$idind17[row], sub_hh136$idind16[row], sub_hh136$b17.916[row], sub_hh136$id_w[row])
# }

# sub_hh137 <- subset(data_hh2, b18.9.1 == 'сын/дочь')
# for (row in 1:nrow(sub_hh137)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh137$id_h[row], sub_hh137$idind18[row], sub_hh137$idind1[row], sub_hh137$b18.9.1[row], sub_hh137$id_w[row])
# }

# sub_hh138 <- subset(data_hh2, b18.9.2 == 'сын/дочь')
# for (row in 1:nrow(sub_hh138)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh138$id_h[row], sub_hh138$idind18[row], sub_hh138$idind2[row], sub_hh138$b18.9.2[row], sub_hh138$id_w[row])
# }

sub_hh139 <- subset(data_hh2, b18.9.3 == 'сын/дочь')
for (row in 1:nrow(sub_hh139)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh139$id_h[row], sub_hh139$idind18[row], sub_hh139$idind3[row], sub_hh139$b18.9.3[row], sub_hh139$id_w[row])
}

# sub_hh140 <- subset(data_hh2, b18.9.4 == 'сын/дочь')
# for (row in 1:nrow(sub_hh140)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh140$id_h[row], sub_hh140$idind18[row], sub_hh140$idind4[row], sub_hh140$b18.9.4[row], sub_hh140$id_w[row])
# }

# sub_hh141 <- subset(data_hh2, b18.9.5 == 'сын/дочь')
# for (row in 1:nrow(sub_hh141)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh141$id_h[row], sub_hh141$idind18[row], sub_hh141$idind5[row], sub_hh141$b18.9.5[row], sub_hh141$id_w[row])
# }

# sub_hh142 <- subset(data_hh2, b18.9.6 == 'сын/дочь')
# for (row in 1:nrow(sub_hh142)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh142$id_h[row], sub_hh142$idind18[row], sub_hh142$idind6[row], sub_hh142$b18.9.6[row], sub_hh142$id_w[row])
# }

# sub_hh143 <- subset(data_hh2, b18.9.7 == 'сын/дочь')
# for (row in 1:nrow(sub_hh143)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh143$id_h[row], sub_hh143$idind18[row], sub_hh143$idind7[row], sub_hh143$b18.9.7[row], sub_hh143$id_w[row])
# }

# sub_hh144 <- subset(data_hh2, b18.9.8 == 'сын/дочь')
# for (row in 1:nrow(sub_hh144)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh144$id_h[row], sub_hh144$idind18[row], sub_hh144$idind8[row], sub_hh144$b18.9.8[row], sub_hh144$id_w[row])
# }
# 
# sub_hh145 <- subset(data_hh2, b18.9.9 == 'сын/дочь')
# for (row in 1:nrow(sub_hh145)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh145$id_h[row], sub_hh145$idind18[row], sub_hh145$idind9[row], sub_hh145$b18.9.9[row], sub_hh145$id_w[row])
# }
# 
# sub_hh146 <- subset(data_hh2, b18.910 == 'сын/дочь')
# for (row in 1:nrow(sub_hh146)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh146$id_h[row], sub_hh146$idind18[row], sub_hh146$idind10[row], sub_hh146$b18.910[row], sub_hh146$id_w[row])
# }

# sub_hh147 <- subset(data_hh2, b18.911 == 'сын/дочь')
# for (row in 1:nrow(sub_hh147)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh147$id_h[row], sub_hh147$idind18[row], sub_hh147$idind11[row], sub_hh147$b18.911[row], sub_hh147$id_w[row])
# }
# 
# sub_hh148 <- subset(data_hh2, b18.912 == 'сын/дочь')
# for (row in 1:nrow(sub_hh148)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh148$id_h[row], sub_hh148$idind18[row], sub_hh148$idind12[row], sub_hh148$b18.912[row], sub_hh148$id_w[row])
# }
# 
# sub_hh149 <- subset(data_hh2, b18.913 == 'сын/дочь')
# for (row in 1:nrow(sub_hh149)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh149$id_h[row], sub_hh149$idind18[row], sub_hh149$idind13[row], sub_hh149$b18.913[row], sub_hh149$id_w[row])
# }

# sub_hh150 <- subset(data_hh2, b18.914 == 'сын/дочь')
# for (row in 1:nrow(sub_hh150)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh150$id_h[row], sub_hh150$idind18[row], sub_hh150$idind14[row], sub_hh150$b18.914[row], sub_hh150$id_w[row])
# }
# 
# sub_hh151 <- subset(data_hh2, b18.915 == 'сын/дочь')
# for (row in 1:nrow(sub_hh151)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh151$id_h[row], sub_hh151$idind18[row], sub_hh151$idind15[row], sub_hh151$b18.915[row], sub_hh151$id_w[row])
# }

sub_hh152 <- subset(data_hh2, b18.916 == 'сын/дочь')
for (row in 1:nrow(sub_hh152)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh152$id_h[row], sub_hh152$idind18[row], sub_hh152$idind16[row], sub_hh152$b18.916[row], sub_hh152$id_w[row])
}

# sub_hh153 <- subset(data_hh2, b18.917 == 'сын/дочь')
# for (row in 1:nrow(sub_hh153)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh153$id_h[row], sub_hh153$idind18[row], sub_hh153$idind17[row], sub_hh153$b18.917[row], sub_hh153$id_w[row])
# }
# 
# sub_hh154 <- subset(data_hh2, b19.9.1 == 'сын/дочь')
# for (row in 1:nrow(sub_hh154)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh154$id_h[row], sub_hh154$idind19[row], sub_hh154$idind1[row], sub_hh154$b19.9.1[row], sub_hh154$id_w[row])
# }

# sub_hh155 <- subset(data_hh2, b19.9.2 == 'сын/дочь')
# for (row in 1:nrow(sub_hh155)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh155$id_h[row], sub_hh155$idind19[row], sub_hh155$idind2[row], sub_hh155$b19.9.2[row], sub_hh155$id_w[row])
# }

# sub_hh156 <- subset(data_hh2, b19.9.3 == 'сын/дочь')
# for (row in 1:nrow(sub_hh156)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh156$id_h[row], sub_hh156$idind19[row], sub_hh156$idind3[row], sub_hh156$b19.9.3[row], sub_hh156$id_w[row])
# }
# 
# sub_hh157 <- subset(data_hh2, b19.9.4 == 'сын/дочь')
# for (row in 1:nrow(sub_hh157)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh157$id_h[row], sub_hh157$idind19[row], sub_hh157$idind4[row], sub_hh157$b19.9.4[row], sub_hh157$id_w[row])
# }
# 
# sub_hh158 <- subset(data_hh2, b19.9.5 == 'сын/дочь')
# for (row in 1:nrow(sub_hh158)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh158$id_h[row], sub_hh158$idind19[row], sub_hh158$idind5[row], sub_hh158$b19.9.5[row], sub_hh158$id_w[row])
# }
# 
# sub_hh159 <- subset(data_hh2, b19.9.6 == 'сын/дочь')
# for (row in 1:nrow(sub_hh159)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh159$id_h[row], sub_hh159$idind19[row], sub_hh159$idind6[row], sub_hh159$b19.9.6[row], sub_hh159$id_w[row])
# }

sub_hh160 <- subset(data_hh2, b19.9.7 == 'сын/дочь')
for (row in 1:nrow(sub_hh160)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh160$id_h[row], sub_hh160$idind19[row], sub_hh160$idind7[row], sub_hh160$b19.9.7[row], sub_hh160$id_w[row])
}

sub_hh161 <- subset(data_hh2, b19.9.8 == 'сын/дочь')
for (row in 1:nrow(sub_hh161)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh161$id_h[row], sub_hh161$idind19[row], sub_hh161$idind8[row], sub_hh161$b19.9.8[row], sub_hh161$id_w[row])
}
# 
# sub_hh162 <- subset(data_hh2, b19.9.9 == 'сын/дочь')
# for (row in 1:nrow(sub_hh162)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh162$id_h[row], sub_hh162$idind19[row], sub_hh162$idind9[row], sub_hh162$b19.9.9[row], sub_hh162$id_w[row])
# }

# sub_hh163 <- subset(data_hh2, b19.910 == 'сын/дочь')
# for (row in 1:nrow(sub_hh163)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh163$id_h[row], sub_hh163$idind19[row], sub_hh163$idind10[row], sub_hh163$b19.910[row], sub_hh163$id_w[row])
# }
# 
# sub_hh164 <- subset(data_hh2, b19.911 == 'сын/дочь')
# for (row in 1:nrow(sub_hh164)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh164$id_h[row], sub_hh164$idind19[row], sub_hh164$idind11[row], sub_hh164$b19.911[row], sub_hh164$id_w[row])
# }
# 
# sub_hh165 <- subset(data_hh2, b19.912 == 'сын/дочь')
# for (row in 1:nrow(sub_hh165)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh165$id_h[row], sub_hh165$idind19[row], sub_hh165$idind12[row], sub_hh165$b19.912[row], sub_hh165$id_w[row])
# }

# sub_hh166 <- subset(data_hh2, b19.913 == 'сын/дочь')
# for (row in 1:nrow(sub_hh166)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh166$id_h[row], sub_hh166$idind19[row], sub_hh166$idind13[row], sub_hh166$b19.913[row], sub_hh166$id_w[row])
# }
# 
# sub_hh167 <- subset(data_hh2, b19.914 == 'сын/дочь')
# for (row in 1:nrow(sub_hh167)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh167$id_h[row], sub_hh167$idind19[row], sub_hh167$idind14[row], sub_hh167$b19.914[row], sub_hh167$id_w[row])
# }
# 
# sub_hh168 <- subset(data_hh2, b19.915 == 'сын/дочь')
# for (row in 1:nrow(sub_hh168)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh168$id_h[row], sub_hh168$idind19[row], sub_hh168$idind15[row], sub_hh168$b19.915[row], sub_hh168$id_w[row])
# }

# sub_hh169 <- subset(data_hh2, b19.916 == 'сын/дочь')
# for (row in 1:nrow(sub_hh169)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh169$id_h[row], sub_hh169$idind19[row], sub_hh169$idind16[row], sub_hh169$b19.916[row], sub_hh169$id_w[row])
# }
# 
# sub_hh170 <- subset(data_hh2, b19.917 == 'сын/дочь')
# for (row in 1:nrow(sub_hh170)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh170$id_h[row], sub_hh170$idind19[row], sub_hh170$idind17[row], sub_hh170$b19.917[row], sub_hh170$id_w[row])
# }
# 
# sub_hh171 <- subset(data_hh2, b19.918 == 'сын/дочь')
# for (row in 1:nrow(sub_hh171)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh171$id_h[row], sub_hh171$idind19[row], sub_hh171$idind18[row], sub_hh171$b19.918[row], sub_hh171$id_w[row])
# }
# 
# sub_hh172 <- subset(data_hh2, b20.9.1 == 'сын/дочь')
# for (row in 1:nrow(sub_hh172)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh172$id_h[row], sub_hh172$idind20[row], sub_hh172$idind1[row], sub_hh172$b20.9.1[row], sub_hh172$id_w[row])
# }
# 
# sub_hh173 <- subset(data_hh2, b20.9.2 == 'сын/дочь')
# for (row in 1:nrow(sub_hh173)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh173$id_h[row], sub_hh173$idind20[row], sub_hh173$idind2[row], sub_hh173$b20.9.2[row], sub_hh173$id_w[row])
# }
# 
# sub_hh174 <- subset(data_hh2, b20.9.3 == 'сын/дочь')
# for (row in 1:nrow(sub_hh174)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh174$id_h[row], sub_hh174$idind20[row], sub_hh174$idind3[row], sub_hh174$b20.9.3[row], sub_hh174$id_w[row])
# }
# 
# sub_hh175 <- subset(data_hh2, b20.9.4 == 'сын/дочь')
# for (row in 1:nrow(sub_hh175)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh175$id_h[row], sub_hh175$idind20[row], sub_hh175$idind4[row], sub_hh175$b20.9.4[row], sub_hh175$id_w[row])
# }

# sub_hh176 <- subset(data_hh2, b20.9.5 == 'сын/дочь')
# for (row in 1:nrow(sub_hh176)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh176$id_h[row], sub_hh176$idind20[row], sub_hh176$idind5[row], sub_hh176$b20.9.5[row], sub_hh176$id_w[row])
# }
# 
# sub_hh177 <- subset(data_hh2, b20.9.6 == 'сын/дочь')
# for (row in 1:nrow(sub_hh177)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh177$id_h[row], sub_hh177$idind20[row], sub_hh177$idind6[row], sub_hh177$b20.9.6[row], sub_hh177$id_w[row])
# }

sub_hh178 <- subset(data_hh2, b20.9.7 == 'сын/дочь')
for (row in 1:nrow(sub_hh178)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh178$id_h[row], sub_hh178$idind20[row], sub_hh178$idind7[row], sub_hh178$b20.9.7[row], sub_hh178$id_w[row])
}

sub_hh179 <- subset(data_hh2, b20.9.8 == 'сын/дочь')
for (row in 1:nrow(sub_hh179)) {
  child_adult[nrow(child_adult) + 1,] = c(sub_hh179$id_h[row], sub_hh179$idind20[row], sub_hh179$idind8[row], sub_hh179$b20.9.8[row], sub_hh179$id_w[row])
}

# sub_hh180 <- subset(data_hh2, b20.9.9 == 'сын/дочь')
# for (row in 1:nrow(sub_hh180)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh180$id_h[row], sub_hh180$idind20[row], sub_hh180$idind9[row], sub_hh180$b20.9.9[row], sub_hh180$id_w[row])
# }
# 
# sub_hh181 <- subset(data_hh2, b20.910 == 'сын/дочь')
# for (row in 1:nrow(sub_hh181)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh181$id_h[row], sub_hh181$idind20[row], sub_hh181$idind10[row], sub_hh181$b20.910[row], sub_hh181$id_w[row])
# }
# 
# sub_hh182 <- subset(data_hh2, b20.911 == 'сын/дочь')
# for (row in 1:nrow(sub_hh182)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh182$id_h[row], sub_hh182$idind20[row], sub_hh182$idind11[row], sub_hh182$b20.911[row], sub_hh182$id_w[row])
# }
# 
# sub_hh183 <- subset(data_hh2, b20.912 == 'сын/дочь')
# for (row in 1:nrow(sub_hh183)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh183$id_h[row], sub_hh183$idind20[row], sub_hh183$idind12[row], sub_hh183$b20.912[row], sub_hh183$id_w[row])
# }
# 
# sub_hh184 <- subset(data_hh2, b20.913 == 'сын/дочь')
# for (row in 1:nrow(sub_hh184)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh184$id_h[row], sub_hh184$idind20[row], sub_hh184$idind13[row], sub_hh184$b20.913[row], sub_hh184$id_w[row])
# }
# 
# sub_hh185 <- subset(data_hh2, b20.914 == 'сын/дочь')
# for (row in 1:nrow(sub_hh185)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh185$id_h[row], sub_hh185$idind20[row], sub_hh185$idind14[row], sub_hh185$b20.914[row], sub_hh185$id_w[row])
# }
# 
# sub_hh186 <- subset(data_hh2, b20.915 == 'сын/дочь')
# for (row in 1:nrow(sub_hh186)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh186$id_h[row], sub_hh186$idind20[row], sub_hh186$idind15[row], sub_hh186$b20.915[row], sub_hh186$id_w[row])
# }
# 
# sub_hh187 <- subset(data_hh2, b20.916 == 'сын/дочь')
# for (row in 1:nrow(sub_hh187)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh187$id_h[row], sub_hh187$idind20[row], sub_hh187$idind16[row], sub_hh187$b20.916[row], sub_hh187$id_w[row])
# }
# 
# sub_hh188 <- subset(data_hh2, b20.917 == 'сын/дочь')
# for (row in 1:nrow(sub_hh188)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh188$id_h[row], sub_hh188$idind20[row], sub_hh188$idind17[row], sub_hh188$b20.917[row], sub_hh188$id_w[row])
# }
# 
# sub_hh189 <- subset(data_hh2, b20.918 == 'сын/дочь')
# for (row in 1:nrow(sub_hh189)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh189$id_h[row], sub_hh189$idind20[row], sub_hh189$idind18[row], sub_hh189$b20.918[row], sub_hh189$id_w[row])
# }
# 
# sub_hh190 <- subset(data_hh2, b20.919 == 'сын/дочь')
# for (row in 1:nrow(sub_hh190)) {
#   child_adult[nrow(child_adult) + 1,] = c(sub_hh190$id_h[row], sub_hh190$idind20[row], sub_hh190$idind19[row], sub_hh190$b20.919[row], sub_hh190$id_w[row])
# }

#итог
dim(child_adult)
child_adult <- child_adult[!duplicated(child_adult), ] 
dim(child_adult)
# 46477 наблюдения

#### SAVE ####
write.csv(child_adult, "/Users/bookmac/Downloads/child_adult_2may.csv", row.names=FALSE)
child_adult_may <-  read.csv('/Users/bookmac/Downloads/child_adult_2may.csv')

child_adult$id_w <- ifelse(child_adult$id_w == 20, '2015 год',
                           ifelse(child_adult$id_w == 21, '2016 год',
                                  ifelse(child_adult$id_w == 22, '2017 год',
                                         ifelse(child_adult$id_w == 23, '2018 год',
                                                ifelse(child_adult$id_w == 24, '2019 год',
                                                       ifelse(child_adult$id_w == 25, '2020 год', '2021 год'))))))
child_adult$id_w <- factor(child_adult$id_w)

#### присоединили детей старше 6 к родителям ####
child_merged <- merge(x = data_childplus6, y = child_adult, by.x=c('idind', 'id_w'), by.y=c('id_child', 'id_w'))
dim(child_merged)

#проверим
child_merged2 <- child_merged[ ! duplicated(child_merged[c('idind', 'id_adult', 'id_w')]), ] #12918
# try2 <- child_merged[ ! duplicated(child_merged[c('idind', 'id_w')]), ]
# dim(try2) #7717
# dim(data_childplus6) #8004
# 287 наблюдений детей потеряли из-за того, что не нашлись их родители

#### смотрим все пары дети/родители, чтобы получить совокупный доход семьи ####
dim(data_adult)
data_adult2 <- data_adult[ ! duplicated(data_adult[c('idind', 'id_w')]), ] 
dim(data_adult2)
adult_merged <-  merge(x = data_adult2, y = child_adult, 
                       by.x=c('idind', 'id_w'), by.y=c('id_adult', 'id_w')) #45295 

adult_merged2 <- adult_merged[ ! duplicated(adult_merged[c('idind', 'id_child', 'id_w')]), ] #45295 откуда здесь могли появиться дубликаты? Их нет!
adult_merged2 <- subset(adult_merged2, select = -c(psu, status))

# оставим нужные переменные
adult_merged3 <- subset(adult_merged2, select = c(idind, id_hh, id_child, j60, id_w))

# посмотрим, сколько теряем
table(adult_merged3$j60 == 'ОТКАЗ ОТ ОТВЕТА') #1076 взрослых потеряли
table(adult_merged3$j60 == 'ЗАТРУДНЯЮСЬ ОТВЕТИТЬ') #357 взрослых потеряли
table(adult_merged3$j60 == 'НЕТ ОТВЕТА') #71 взрослого потеряли
table(!is.na(adult_merged3$j60)) #0

#  оставим только тех, у кого указана з/п
adult_merged4 <- filter(adult_merged3, j60 != 'ОТКАЗ ОТ ОТВЕТА', j60 != 'ЗАТРУДНЯЮСЬ ОТВЕТИТЬ', j60 != 'НЕТ ОТВЕТА')

#  переведём в численную
class(adult_merged4$j60)
adult_merged4$j60 <- as.numeric(as.character(adult_merged4$j60))
head(adult_merged4$j60)

# переменная для совокупного дохода семьи
df_income <- adult_merged4 %>%
  group_by(id_child, id_w) %>%
  summarise(hh_income = sum(j60)) # 28512 - здесь дети есть всех возрастов и не обязательно у них есть детский опросник

####что-то там делаем с зарплатой ####
real_income_df <- data.frame(Year  = c("2015 год", "2016 год", "2017 год", "2018 год", "2019 год", "2020 год", "2021 год"),
                             chain_CPI = c(1.0539*1.0251*1.0426*1.0304*1.0491*1.0839*1.1194, 1.0251*1.0426*1.0304*1.0491*1.0839*1.1194,
                                           1.0426*1.0304*1.0491*1.0839*1.1194, 1.0304*1.0491*1.0839*1.1194, 1.0491*1.0839*1.1194, 1.0839*1.1194, 1.1194))
#привели к ценам в декабре 2022 
#всего в обследовании
#'2015 год','2016 год','2017 год','2018 год', '2019 год','2020 год','2021 год'
# сами ИПЦ 1.1291, 1.0539, 1.0251, 1.0426, 1.0304, 1.0491, 1.0839, 1.1194
class(real_income_df$Year)
class(df_income$id_w)

df_income2 <- merge(x = df_income, y = real_income_df, 
                    by.x=c('id_w'), by.y=c('Year'))  

df_income2$present_hh_income <- df_income2$hh_income*df_income2$chain_CPI
df_income3 <- subset(df_income2, select = c(id_w, id_child, present_hh_income)) # 28512
df_income_final <- df_income3
df_income_final <- df_income_final[ ! duplicated(df_income_final), ]

#### объединяем детей с доходом семьи ####
df_merged <- merge(x = child_merged, y = df_income_final, 
                   by.x=c('idind', 'id_w'), by.y=c('id_child', 'id_w')) # 12535 - кикнули тех, кто не указал доход
dim(child_merged)
df_merged <- df_merged[ ! duplicated(df_merged), ]

#### нужно подсоединить характеристики родителей ####
adult_merged5 <- subset(adult_merged2, select = -c(year, id_h, born_m, h6, j60, connect, 
                                                   k3.3, k3.4, j196.1, j196.2, j196.3, j196.4, j88, j200))
adult_merged6 <- adult_merged5 %>% 
  rename(idadult = idind, num_child = j72.172, adult_sex = h5, adult_age = age)
adult_merged_final <- adult_merged6

dim(df_merged)
dim(adult_merged_final)
dim(adult_merged_final[! duplicated(adult_merged_final[c('idadult', 'id_w', 'id_hh', 'id_child')]), ]) #кол-во не меняется


# df_merged - дети + несколько родителей отдельными строчками + доход (12535)
# adult_merged_final - все родители даже с неуказанным доходом, но с указанным ребенком (45295)
df_merged2 <- merge(x = df_merged, y = adult_merged_final, 
                    by.x=c('id_hh', 'id_w', 'idind', 'id_adult'), by.y=c( 'id_hh', 'id_w', 'id_child', 'idadult')) 
colnames(df_merged2)
colnames(adult_merged_final)

df_merged2 <- df_merged2[! duplicated(df_merged2[c('id_adult', 'id_w', 'id_hh', 'idind')]), ] # 12316:разные родители в одних и тех же строчках


#почистим
colnames(df_merged2)
df_merged3 <- subset(df_merged2, select = -c(id_w, h11, id_h))

df_merged4 <- df_merged3 %>% 
  rename(child_sex = h5, year_birth = h6, child_age = age, hw_help = k771.3a, hw_hours = k771.3b, hw_minutes = k771.3c, add_knowl_help = k771.4a, born_m_child = born_m,
         id_child=idind,  grades = k3.4, school_type = k3.2, school_belong = k3.3, dist = k7.8.1)

df_merged5 <- df_merged4[ ! duplicated(df_merged4[c('id_child','id_adult','year')]), ]

df_merged5$grades <- replace(df_merged5$grades, df_merged5$grades == "НЕТ ОТВЕТА", NA)
df_merged5$grades <- replace(df_merged5$grades, df_merged5$grades == "ОТКАЗ ОТ ОТВЕТА", NA)
df_merged5$grades <- replace(df_merged5$grades, df_merged5$grades == "ЗАТРУДНЯЮСЬ ОТВЕТИТЬ", NA)
df_merged5$grades <- replace(df_merged5$grades, df_merged5$grades == "ОЦЕНКИ НЕ СТАВЯТ", NA)

df_merged5$add_knowl_help <- replace(df_merged5$add_knowl_help, df_merged5$add_knowl_help == "НЕТ ОТВЕТА", NA)
df_merged5$add_knowl_help <- replace(df_merged5$add_knowl_help, df_merged5$add_knowl_help == "ОТКАЗ ОТ ОТВЕТА", NA)
df_merged5$add_knowl_help <- replace(df_merged5$add_knowl_help, df_merged5$add_knowl_help == "ЗАТРУДНЯЮСЬ ОТВЕТИТЬ", NA)

df_merged5$hw_help <- replace(df_merged5$hw_help, df_merged5$hw_help == "НЕТ ОТВЕТА", NA)
df_merged5$hw_help <- replace(df_merged5$hw_help, df_merged5$hw_help == "ОТКАЗ ОТ ОТВЕТА", NA)
df_merged5$hw_help <- replace(df_merged5$hw_help, df_merged5$hw_help == "ЗАТРУДНЯЮСЬ ОТВЕТИТЬ", NA)

levels(df_merged5$add_knowl_help)
df_merged5$add_knowl_help <- replace(df_merged5$hw_help, df_merged5$grades == "ЗАТРУДНЯЮСЬ ОТВЕТИТЬ", NA)


df_merged5$hw_help <- ifelse(df_merged5$hw_help == "НЕ ЗАНИМАЕТСЯ ЭТИМ" | df_merged5$hw_help == "Нет", 0, 1)
df_merged5$add_knowl_help <- ifelse(df_merged5$add_knowl_help == "НЕ ЗАНИМАЕТСЯ ЭТИМ" | df_merged5$add_knowl_help == "Нет", 0, 1)

df_merged5$grades<- factor(df_merged5$grades)
df_merged5$add_knowl_help<- factor(df_merged5$add_knowl_help)
df_merged5$hw_help<- factor(df_merged5$hw_help)

summary(df_merged5)

# БЫЛО: 5785 людей оценки были отдел для ребенка, 9642 при чистке
# СТАЛО 7481 человек

dim(unique(select(df_merged5, id_child, year)))

# install.packages("writexl")
library("writexl")
write_xlsx(df_merged7,"/Users/bookmac/Downloads/2may.xlsx")

#### SAVE2 Создаём рабочий файл ####
df_merged5 <- dplyr::select(df_merged5, -connect, -j60)
children <- dplyr::select(df_merged5, id_child, year)
dim(unique(children))
try1 <- filter(df_merged5, adult_sex=="ЖЕНСКИЙ") %>% dplyr::select(-adult_sex) 
try1 <- try1 %>% 
  rename(id_mother = id_adult, marst_mot = marst, educ_mot = educ, mot_age = adult_age, mot_num = num_child)

try2 <- filter(df_merged5, adult_sex=="МУЖСКОЙ") %>% dplyr::select(-adult_sex) 
try2 <- try2 %>% 
  rename(id_father = id_adult, marst_fat = marst, educ_fat = educ, fat_age = adult_age, fat_num = num_child)

try <- merge(try1, try2, all =TRUE)
try <- try[! duplicated(try[c('id_child','year')]), ] %>% mutate(female=case_when(child_sex=="ЖЕНСКИЙ"~1, TRUE~0),
                                                                 num_child=case_when(mot_num!=0~mot_num, TRUE~fat_num)) %>% 
  dplyr::select(-mot_num, -fat_num)
try_new <- mutate(try, num_child=as.numeric(as.character(num_child)), num_child=case_when((id_child==26710)|(id_child==30669)~2, TRUE~case_when(id_child==30654~1, TRUE~num_child)))

sum(is.na(try_new$hw_hours))
sum(is.na(try_new$hw_minutes))
try_new$hw_hours <- ifelse(is.na(try_new$hw_hours), 0, try_new$hw_hours)
try_new$hw_minutes <- ifelse(is.na(try_new$hw_minutes), 0, try_new$hw_minutes)
try_new$hw_time_mun <- 60*try_new$hw_hours+try_new$hw_minutes
try_new$hw_hours <- NULL
try_new$hw_minutes <- NULL
try_new$hw_time_mun <- ifelse(try_new$hw_time_mun == 0, NA, try_new$hw_time_mun)  
try_new$female <- ifelse(try_new$child_sex == 'ЖЕНСКИЙ', 1, 0)  
try_new$female <- factor(try_new$female)
try_new$child_sex <- NULL

summary(try_new)

dim(try_new)
dim(unique(select(try_new, id_child, year)))
write_xlsx(try_new,"/Users/bookmac/Downloads/xls3may.xlsx")

#### К экселевскому новые переменные ####
df0 <- try_new
df0$status <- NULL
df0$psu <- NULL
insersection <- merge(x = df0, y = data_childplus6, by.x=c('id_child','year'), by.y=c('idind', 'year'), all = FALSE)
insersection <- subset(insersection, select = c('id_child', 'year', 'status', 'psu'))
######################################################################################
loc_coefs<- read.csv("/Users/bookmac/Downloads/loc3.csv",              
                     header = TRUE,       
                     sep = ";",            
                     encoding = "UTF-8") 

loc_coefs$coef_loc <- gsub(",", ".", as.character(loc_coefs$coef_loc)) %>% as.numeric()

insersection$psu_lvl <- as.numeric(insersection$psu)

insersection0 <- merge(x = loc_coefs, y = insersection, by.x = c('id_loc'), by.y = c('psu_lvl'), all = FALSE)
insersection0$psu <- NULL
######################################################################################

####добавляю переменные####
data_ind_add1 <- subset(data_ind, select = c(idind, id_w, year, j1, occup08, l89, j90, j6, origsm, adult, 
                                             j4.1, j22, j31, j29.1))
data_ind_add2 <- filter(data_ind_add1, origsm == 'Да, адрес репрезентативной выборки')
data_adult_add <- filter(data_ind_add2, adult == 'Есть взрослый вопросник')
data_adult_add <- filter(data_adult_add, id_w == '2015 год' | id_w == '2016 год'| id_w == '2017 год'|
                           id_w == '2018 год'| id_w == '2019 год'| id_w == '2020 год'|
                           id_w == '2021 год')
data_adult_add <- subset(data_adult_add, select = - c(origsm, adult, id_w))
data_adult_add$occup08 <- as.factor(data_adult_add$occup08)
summary(data_adult_add)
# sum(is.na(data_adult_add$j60.5a))

add0 <- data_adult_add %>% 
  rename(
    work_status = j1, industry = occup08, healthprobl12 = l89, main_activ = j90, chief = j6, 
    field_job = j4.1, certainfind = j22, jobloss_worry = j31, didchange = j29.1)

summary(add0)

add0$doeswork <- ifelse(add0$work_status == 'Вы сейчас работаете', 1,
                        ifelse((add0$work_status == 'ОТКАЗ ОТ ОТВЕТА') | (add0$work_status == 'НЕТ ОТВЕТА') | (add0$work_status == 'ЗАТРУДНЯЮСЬ ОТВЕТИТЬ'),
                               NA, 0))
add0$chief <- ifelse(add0$chief == 'Да', 1,
                     ifelse((add0$chief == 'ОТКАЗ ОТ ОТВЕТА') | (add0$chief == 'НЕТ ОТВЕТА') | (add0$chief == 'ЗАТРУДНЯЮСЬ ОТВЕТИТЬ'),
                            NA, 0))
add0$bluecollar <- ifelse(add0$industry == 'военнослужащие' | add0$industry == 'квалифицированные работники сельского, лесного хоз-ва и рыбоводс' |
                            add0$industry == 'квалифицированные рабочие, занятые ручным трудом' | add0$industry == 'квалифицированные рабочие, использующие машины и механизмы'
                          | add0$industry == 'неквалифицированные рабочие всех отраслей', 1, 
                          ifelse(add0$industry == 'ЗАТРУДНЯЮСЬ ОТВЕТИТЬ' | add0$industry == 'ОТКАЗ ОТ ОТВЕТА' | add0$industry == 'НЕТ ОТВЕТА', NA, "0"))

add0$jobloss_worry  <- replace(add0$jobloss_worry, add0$jobloss_worry == 'ОТКАЗ ОТ ОТВЕТА', NA)
add0$jobloss_worry  <- replace(add0$jobloss_worry, add0$jobloss_worry == 'НЕТ ОТВЕТА', NA)
add0$jobloss_worry  <- replace(add0$jobloss_worry, add0$jobloss_worry == 'ЗАТРУДНЯЮСЬ ОТВЕТИТЬ', NA)
add0$jobloss_worry <- as.character(add0$jobloss_worry)
add0$jobloss_worry <- ifelse(add0$jobloss_worry == "Очень беспокоит" | add0$jobloss_worry == "Немного беспокоит", 1, 0)
add0$jobloss_worry <- factor(add0$jobloss_worry)

add0$didchange  <- replace(add0$didchange, add0$didchange == 'ОТКАЗ ОТ ОТВЕТА', NA)
add0$didchange  <- replace(add0$didchange, add0$didchange == 'НЕТ ОТВЕТА', NA)
add0$didchange  <- replace(add0$didchange, add0$didchange == 'ЗАТРУДНЯЮСЬ ОТВЕТИТЬ', NA)
add0$didchange <- as.character(add0$didchange)
add0$didchange <- ifelse(add0$didchange == "Профессия и место работы остались прежними", 0, 1)
add0$didchange <- factor(add0$didchange)

add0$certainfind  <- replace(add0$certainfind, add0$certainfind == 'ОТКАЗ ОТ ОТВЕТА', NA)
add0$certainfind  <- replace(add0$certainfind, add0$certainfind == 'НЕТ ОТВЕТА', NA)
add0$certainfind  <- replace(add0$certainfind, add0$certainfind == 'ЗАТРУДНЯЮСЬ ОТВЕТИТЬ', NA)
add0$certainfind <- as.character(add0$certainfind)
add0$certainfind <- ifelse(add0$certainfind == "Полностью уверены" | add0$certainfind == "Скорее уверены", 1, 0)
add0$certainfind <- factor(add0$certainfind)

add0$field_job  <- replace(add0$field_job, add0$field_job == 'ОТКАЗ ОТ ОТВЕТА', NA)
add0$field_job  <- replace(add0$field_job, add0$field_job == 'НЕТ ОТВЕТА', NA)
add0$field_job  <- replace(add0$field_job, add0$field_job == 'ЗАТРУДНЯЮСЬ ОТВЕТИТЬ', NA)
add0$field_job <- factor(as.character(add0$field_job))

add0$ill <- add0$healthprobl12
add0$ill  <- replace(add0$ill, add0$ill == 'ОТКАЗ ОТ ОТВЕТА', NA)
add0$ill  <- replace(add0$ill, add0$ill == 'НЕТ ОТВЕТА', NA)
add0$ill  <- replace(add0$ill, add0$ill == 'ЗАТРУДНЯЮСЬ ОТВЕТИТЬ', NA)
add0$ill <- as.character(add0$ill)
add0$ill <- ifelse(add0$ill == "Да", 1, 0)
add0$ill <- factor(add0$ill)

add0$entrepreuner <- ifelse(add0$main_activ == 'Предприниматель', 1,
                            ifelse((add0$main_activ == 'ОТКАЗ ОТ ОТВЕТА') | (add0$main_activ == 'НЕТ ОТВЕТА') | (add0$main_activ == 'ЗАТРУДНЯЮСЬ ОТВЕТИТЬ'),
                                   NA, 0))

add <- subset(add0, select = - c(work_status, industry, main_activ, healthprobl12))
######################################################################################
df0 <- merge(x = df0, y = add, by.x=c('id_mother','year'), by.y = c('idind', 'year'), all.x = TRUE)
df0 <- df0 %>% 
  rename(
    chief_mot = chief, doeswork_mot = doeswork,
    bluecollar_mot= bluecollar, ill_mot = ill, entrep_mot = entrepreuner,
    field_job_mot = field_job, certainfind_mot = certainfind, jobloss_worry_mot = jobloss_worry, didchange_mot = didchange)

summary(df0$ill_mot)

df0 <- merge(x = df0, y = add, by.x=c('id_father','year'), by.y = c('idind', 'year'), all.x = TRUE)
df0$matleave <- NULL
df0 <- df0 %>% 
  rename(
    chief_fat = chief, doeswork_fat = doeswork,
    bluecollar_fat= bluecollar, ill_fat = ill, entrep_fat = entrepreuner,
    field_job_fat = field_job, certainfind_fat = certainfind, jobloss_worry_fat = jobloss_worry, didchange_fat = didchange)

df0 <- merge(x = df0, y = insersection0, by=c('id_child','year'), all = FALSE)

# df0$present_hh_income <- gsub(",", ".", as.character(df0$present_hh_income)) %>% as.numeric() #если экселевский
str(df0$present_hh_income)
df0$present_hh_income <- df0$present_hh_income * df0$coef_loc
df0$coef_loc <- NULL

df0$educ_mot <- ifelse(df0$educ_mot == "#Н/Д" | df0$educ_mot == "НЕТ ОТВЕТА", NA, 
                       ifelse(df0$educ_mot == "есть диплом о высшем образовании" | df0$educ_mot == "аспирантура и т.п. с дипломом" 
                              | df0$educ_mot == "аспирантура и т.п. без диплома", 1, 0))

df0$educ_fat <- ifelse(df0$educ_fat == "#Н/Д" | df0$educ_fat == "НЕТ ОТВЕТА", NA, 
                       ifelse(df0$educ_fat == "есть диплом о высшем образовании" | df0$educ_fat == "аспирантура и т.п. с дипломом" 
                              | df0$educ_fat == "аспирантура и т.п. без диплома", 1, 0))

df0$near <- ifelse(df0$dist == 'Пешком' | df0$dist ==  'ОБУЧАЕТСЯ ДОМА', 1,
                   ifelse(df0$dist == 'ЗАТРУДНЯЮСЬ ОТВЕТИТЬ' | df0$dist ==  'НЕТ ОТВЕТА', NA, 0))

df0$prof_school <- ifelse(df0$school_type == 'Школе с углубленным или профильным преподаванием предметов', 1, 
                          ifelse(df0$school_type == 'НЕТ ОТВЕТА', NA, 0))

df0$priv_school <- ifelse(df0$school_belong == 'Частным лицам', 1, 
                          ifelse(df0$school_type == 'НЕТ ОТВЕТА', NA, 0))

df0$city <- ifelse(df0$status == 'Областной центр', 'admin_center', 
                   ifelse(df0$status == 'Город', 'town', 'village'))

df0$dist <- NULL
df0$school_belong <- NULL
df0$school_type <- NULL
df0$status <- NULL

#МОМЕНТ ИЗМЕНЕНИЯ ИЗ СКРИПТА 23_04_23
# удаляю те, которые, оказались, не нужны в принципе
df0$communicate <- NULL
df0$mot_fire_worry <- NULL
df0$fat_fire_worry <- NULL
df0$wealth_change12_mot <- NULL
df0$wealth_change12_fat <- NULL
df0$fin_satisf_mot <- NULL
df0$fin_satisf_fat <- NULL
df0$capacity_abroad_mot <- NULL
df0$capacity_abroad_fat <- NULL
df0$credit12_mot <- NULL
df0$credit12_fat <- NULL
df0$fat_age <- NULL
df0$mot_age <- NULL
df0$mot_nation <- NULL
df0$fat_nation <- NULL

df0$log_hh_income<- log(df0$present_hh_income+1)

df0$grades <- factor(df0$grades, levels = c("Почти все - пятерки", "В основном пятерки и четверки",
                                            "В основном четверки", "В основном четверки и тройки", "В основном тройки")[5:1], ordered = TRUE)

df1 <- df0
df2 <- df0

df2$prof_school <- ifelse(df2$prof_school == 'НЕТ ОТВЕТА', NA, df2$prof_school)
df2$priv_school <- ifelse(df2$priv_school == 'НЕТ ОТВЕТА', NA, df2$priv_school)
df2$near <- ifelse(df2$near == 'НЕТ ОТВЕТА', NA, df2$near)
df2$bluecollar_fat <- ifelse(df2$bluecollar_fat == 'НЕТ ОТВЕТА', NA, df2$bluecollar_fat)
df2$bluecollar_mot <- ifelse(df2$bluecollar_mot == 'НЕТ ОТВЕТА', NA, df2$bluecollar_mot)


df2$id_child <- factor(df2$id_child)
df2$year <- factor(df2$year)
df2$id_mother <- factor(df2$id_mother)
df2$id_father <- factor(df2$id_father)
df2$id_hh <- factor(df0$id_hh)
df2$educ_mot <- factor(df2$educ_mot)
df2$educ_fat <- factor(df2$educ_fat)
df2$born_m_child <- factor(df2$born_m_child)
df2$female<- factor(df2$female)
df2$year_birth<- factor(df2$year_birth)
df2$hw_help<- factor(df2$hw_help)
df2$add_knowl_help <- factor(df2$add_knowl_help)
df2$near <- factor(df0$near)
df2$prof_school <- factor(df2$prof_school)
df2$priv_school <- factor(df2$priv_school)
df2$city <- factor(df2$city)
df2$marst_mot <- factor(df2$marst_mot)
df2$marst_fat <- factor(df2$marst_fat)
df2$bluecollar_mot <- factor(df2$bluecollar_mot)
df2$bluecollar_fat <- factor(df2$bluecollar_fat)
df2$chief_mot <- factor(df2$chief_mot)
df2$chief_fat <- factor(df2$chief_fat)
df2$entrep_mot <- factor(df2$entrep_mot)
df2$entrep_fat <- factor(df2$entrep_fat)
df2$doeswork_mot <- factor(df2$doeswork_mot)
df2$doeswork_fat <- factor(df2$doeswork_fat)
df2$id_loc <- factor(df2$id_loc)

df2$child_age <- as.numeric(df2$child_age)
df2$num_child <- as.numeric(df2$num_child)
df2$hw_time_mun <- as.numeric(df2$hw_time_mun)

id_1m <- df2 %>% filter(is.na(id_father))
id_1f <- df2 %>% filter(is.na(id_mother))
id_2p <- df2 %>% filter(!is.na(id_mother), !is.na(id_father))

dim(df2)
dim(id_1m) + dim(id_1f) + dim(id_2p)

id_1m_2raise <- id_1m
id_1m_2raise$marst <- ifelse(id_1m_2raise$marst_mot == "Состоите в зарегистрированном браке" | id_1m$marst_mot == "Живете вместе, но не зарегистрированы", 1,
                             ifelse(id_1m_2raise$marst_mot == "Разведены и в браке не состоите" | id_1m$marst_mot == "Никогда в браке не состояли" | id_1m$marst_mot == "Bдовец (вдова)",
                                    0, NA))
id_1f_2raise <- id_1f
id_1f_2raise$marst <- ifelse(id_1f_2raise$marst_fat == "Состоите в зарегистрированном браке" | id_1f_2raise$marst_fat == "Живете вместе, но не зарегистрированы", 1,
                             ifelse(id_1f_2raise$marst_fat == "Разведены и в браке не состоите" | id_1f_2raise$marst_fat == "Никогда в браке не состояли" | id_1m$marst_mot == "Bдовец (вдова)",
                                    0, NA))

table(id_2p$marst_fat) 
table(id_2p$marst_mot) 

id_2p_2raise <- id_2p 
id_2p_2raise$marst <- ifelse(id_2p_2raise$marst_mot == "Никогда в браке не состояли" | id_2p_2raise$marst_fat == "Никогда в браке не состояли" | 
                               id_2p_2raise$marst_mot == "Состоите в зарегистрированном браке" | id_2p_2raise$marst_fat == "Состоите в зарегистрированном браке" | 
                               id_2p_2raise$marst_mot == "Живете вместе, но не зарегистрированы" | id_2p_2raise$marst_fat == "Живете вместе, но не зарегистрированы", 1,
                             ifelse(id_2p_2raise$marst_mot == "Разведены и в браке не состоите" | id_2p_2raise$marst_fat == "Разведены и в браке не состоите" | 
                                      id_2p_2raise$marst_mot == "Bдовец (вдова)" | id_2p_2raise$marst_fat == "Bдовец (вдова)",
                                    0, NA))

df3 <- union(union(id_1m_2raise, id_1f_2raise), id_2p_2raise)
df3$marst <- factor(df3$marst)
df3$marst_mot <- NULL
df3$marst_fat <- NULL
df3$marst <- NULL
df3$didchange_fat <- NULL
df3$didchange_mot <- NULL
df3$jobloss_worry_mot <- NULL
df3$jobloss_worry_fat <- NULL
df3$certainfind_mot <- NULL
df3$certainfind_fat <- NULL
df3$field_job_mot <- NULL
df3$field_job_fat <- NULL

dim(df3)

summary(df2)
summary(df3)

# write.csv(df2, "/Users/bookmac/Downloads/categor5.csv", row.names=FALSE)
final <- df3 %>% filter(present_hh_income<250000)
########################################## РЕКОДИРОВКИ ##############################################################


############датасет с рекодировкой нормального распределения
data.frame(table(final$grades))
x <- c(data.frame(table(final$grades))[1,2], data.frame(table(final$grades))[2,2], data.frame(table(final$grades))[3,2],
       data.frame(table(final$grades))[4,2], data.frame(table(final$grades))[5,2])
y <- x/sum(x)
g <- cumsum(y)
z_score <- qnorm(g)
pdf_score <- dnorm(z_score)
cdf_score <- round(g, 2)

set.seed(123)
s <- rnorm(10^6)
# mean(s[s < z_score[1]]) %>% round(2)
# mean(s[z_score[1] < s & s < z_score[2]]) %>% round(2)
# mean(s[z_score[2] < s & s < z_score[3]]) %>% round(2)
# mean(s[z_score[3] < s & s < z_score[4]]) %>% round(2)
# mean(s[s > z_score[4]]) %>% round(2)

recode_norm <- final
recode_norm$grades <- ifelse(recode_norm$grades == "В основном тройки", mean(s[s < z_score[1]]) %>% round(2),
                             ifelse(recode_norm$grades == "В основном четверки и тройки", mean(s[z_score[1] < s & s < z_score[2]]) %>% round(2),
                                    ifelse(recode_norm$grades == "В основном четверки", mean(s[z_score[2] < s & s < z_score[3]]) %>% round(2),
                                           ifelse(recode_norm$grades == "В основном пятерки и четверки", mean(s[z_score[3] < s & s < z_score[4]]) %>% round(2),
                                                  mean(s[s > z_score[4]]) %>% round(2)))))
recode_norm$grades <- as.numeric(recode_norm$grades)
table(recode_norm$grades)
############датасет с рекодировкой в лоб
recode_lob <- final
recode_lob$grades <- ifelse(recode_lob$grades == "В основном тройки", 3.0,
                            ifelse(recode_lob$grades == "В основном четверки и тройки", 3.5,
                                   ifelse(recode_lob$grades == "В основном четверки", 4.0,
                                          ifelse(recode_lob$grades == "В основном пятерки и четверки", 4.5, 5.0))))
recode_lob$grades <- as.numeric(recode_lob$grades)

############переименую для удобства
categor <- final
####Итоговые датасеты####
dim(categor)
dim(recode_norm)
dim(recode_lob)

write_xlsx(categor,"/Users/bookmac/Downloads/categorial_marst3.xlsx")
write_xlsx(recode_norm,"/Users/bookmac/Downloads/recode_norm_marst3.xlsx")
write_xlsx(recode_lob,"/Users/bookmac/Downloads/recode_lob9_marst3.xlsx")

# write.csv(categor, "/Users/bookmac/Downloads/categorial_marst1.csv", row.names=FALSE)
# write.csv(recode_norm, "/Users/bookmac/Downloads/recode_norm_marst1.csv", row.names=FALSE)
# write.csv(recode_lob, "/Users/bookmac/Downloads/recode_lob_marst1.csv", row.names=FALSE)


#### Описательные статистики ####
categor <- final
forsummary <- categor

forsummary$id_child <- NULL
forsummary$id_hh <- NULL
forsummary$id_father <- NULL
forsummary$id_mother <- NULL
forsummary$year_birth <- NULL
forsummary$field_job_mot <- NULL
forsummary$field_job_fat <- NULL
forsummary$certainfind_mot <- NULL
forsummary$certainfind_fat <- NULL
forsummary$jobloss_worry_mot <- NULL
forsummary$jobloss_worry_fat <- NULL
forsummary$id_loc <- NULL
forsummary$marst_mot <- NULL
forsummary$marst_fat <- NULL
forsummary$didchange_fat <- NULL
forsummary$didchange_mot <- NULL
forsummary$log_hh_income <- NULL
forsummary$marst <- NULL
forsummary$year <- NULL
forsummary$born_m_child <- NULL
forsummary$hw_time_mun <- NULL
forsummary$hw_help <- NULL
str(forsummary$childage)
colnames(forsummary)

table(forsummary$city)
forsummary$city <- as.character(forsummary$city)
forsummary$city <- factor(ifelse(forsummary$city == 'admin_center', "Областной центр",
                                 ifelse(forsummary$city == 'town', "Город", "Село/ПГТ")))

library(tidyverse)
# install.packages("gtsummary")
library(gtsummary)
colnames(forsummary)
colnames(forsummary) <- c("Возраст ребёнка", "Занимается вместе с родителями какими-то предметами для получения дополнительных знаний",
                          "Оценка родителями успеваемости школьника", "Совокупный доход семьи", "Наличие диплома о высшем образовании у матери", "Наличие диплома о высшем образовании у отца", 
                          "Пол ребенка", "Количество детей в семье школьника", "У матери есть подчиненные на работе", "Наличие у мамы ребенка работы на момент проведения интервью", 
                          "Мать школьника – синий воротничок", "Пропуск рабочих/учебных дней матерью в течение последних 12 месяцев по болезни", "Мать – предприниматель", 
                          "У отца есть подчиненные на работе", "Наличие у отца ребенка работы на момент проведения интервью", "Отец школьника – синий воротничок",
                          "Пропуск рабочих/учебных дней отцом в течение последних 12 месяцев по болезни", "Отец – предприниматель", "Добирается до школы пешком/учится онлайн", 
                          "Учится в школе с углубленным или профильным преподаванием предметов", "Учится в частной школе", "Статус города") 
forsummary_factor <- dplyr::select(forsummary, -"Возраст ребёнка", -"Совокупный доход семьи", -"Количество детей в семье школьника")
forsummary_factor %>% tbl_summary()%>% modify_caption("**Описательная статистика факторных переменных РМЭЗ НИУ ВШЭ**") %>% bold_labels()


library(stargazer)
stargazer(forsummary %>% filter(present_hh_income!=0), type = "html", title="Описательная статистика для числовых переменных",
          digits=3, out="table2.html", covariate.labels = c("Возраст ребенка", "Совокупный доход семьи, руб", 
                                                            "Количество детей в семье"))

# par(mfrow=c(1,2))
# ggplot(categor, aes(x = year), stat = "count") +
#   geom_histogram(colour = "black",, fill="lightblue", binwidth=1) 
# ggplot(categor, aes(x = child_age), stat = "count") +
#   geom_histogram(colour = "black",, fill="lightblue", binwidth=1) 

#### Модели ####
norm <- read_excel("/Users/bookmac/Downloads/recode_norm_marst3.xlsx",
                   col_types = c("text", "text", "text",
                                 "text", "text", "text", "text", "numeric",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "text",
                                 "numeric"))

lob <- read_excel("/Users/bookmac/Downloads/recode_lob9_marst3.xlsx",
                  col_types = c("text", "text", "text",
                                "text", "text", "text", "text", "numeric",
                                "numeric", "numeric", "numeric",
                                "numeric", "numeric", "numeric", "numeric",
                                "numeric", "numeric", "numeric",
                                "numeric", "numeric", "numeric",
                                "numeric", "numeric", "numeric",
                                "numeric", "numeric", "numeric",
                                "numeric", "numeric", "numeric",
                                "numeric", "text",
                                "numeric"))

categor_data <- read_excel("/Users/bookmac/Downloads/categorial_marst3.xlsx",
                           col_types = c("text", "text", "text",
                                         "text", "text", "text", "text", "numeric",
                                         "numeric", "numeric", "text",
                                         "numeric", "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "text",
                                         "numeric"))
str(categor_data)

norm <- norm  %>% filter(priv_school == 0 | is.na(priv_school))
lob <- lob  %>% filter(priv_school == 0 | is.na(priv_school))
categor_data <- categor_data  %>% filter(priv_school == 0 | is.na(priv_school))

#### RLMS ####
####Пространственные ####
#### Кодировка 1 (нормальное) ####
norm$city <- factor(norm$city)
data <- norm
colnames(data)
data <- dplyr::select(data, grades, log_hh_income, educ_mot, educ_fat, child_age,
                      female, num_child, doeswork_mot, ill_mot, near, city)
attach(data)
dim(data)

#### OLS ####
data_fat <- data %>% na.omit() %>% filter(log_hh_income > 0)
dim(data_fat)
mod2 <- lm(grades ~ . , data_fat)
summary(mod2)

coeftest(mod2, vcov. = vcovHC(mod2, type = "HC1"))

#### панели ####
norm$id_child <- factor(norm$id_child)
norm$year <- factor(norm$year)
g <- table(norm$id_child) >=3
r <- names(g)[g]
str(norm)
for_panel <- filter(norm, id_child %in% r)
data_p <- dplyr::select(for_panel, id_child, grades, log_hh_income, educ_mot, educ_fat, child_age,
                        female, num_child, doeswork_mot, ill_mot, near, city, year) %>% filter(log_hh_income != 0)
data_p <- pdata.frame(data_p, index = c("id_child", "year"), drop.index = TRUE)

# modqq <- lm(grades ~ log_hh_income+educ_mot+educ_fat+child_age+
#               female+num_child+doeswork_mot+ill_mot+near+city, data_p)
# summary(modqq)
# coeftest(modqq, vcovHC(modqq, "HC1"))
# vif(modqq)
# crPlots(modqq)

mod_p <- plm(grades ~ log_hh_income+educ_mot+educ_fat+child_age+
               female+num_child+doeswork_mot+ill_mot+near+city, data=data_p, 
             model = "pooling", effect = "individual")
mod2_p <- plm(grades ~ log_hh_income+educ_mot+educ_fat+child_age+
                female+num_child+doeswork_mot+ill_mot+near+city,
              data_p, model = "random", index = c("id_child", "year"))
mod3_p <- plm(grades ~ log_hh_income+educ_mot+educ_fat+child_age+
                female+num_child+doeswork_mot+ill_mot+near+city,
              data_p, model = "within", index = c("id_child"))


pFtest(mod3_p, mod_p)
plmtest(mod2_p, type = "bp")
phtest(mod3_p, mod2_p)

#### Выдача ####
coef <- c("(константа)", "Логарифм совокупного дохода семьи",
          "Наличие высшего образования у матери",
          "Наличие высшего образования у отца",
          "Количество полных лет ребенка",
          "Пол ребёнка",
          "Кол-во детей в семье",
          "Наличие у мамы ребенка работы на момент проведения интервью",
          "Пропуск рабочих/учебных дней матерью в течение последних 12 месяцев по болезни",
          "Ребенок добирается до школы пешком или обучается дома",
          "Населённый пункт имеет статус города",
          "Населённый пункт имеет статус села/ПГТ")

ar=data.frame(rbind(c("F-test", " p-value < 0.05", "", ""),
                    c("Breusch-Pagan test", "", "p-value < 0.05", ""),
                    c("Hausman test", "", "", "p-value > 0.05")))

library(modelsummary)
msummary(models=list("Pooled"=mod_p, "Random" =mod2_p), 
         gof_omit = "AIC|RMSE|Log.Lik.|BIC", align=c("lcc"), output="gt", coef_rename=coef,
         vcov="HC1", stars = c('*' = .1, '**' = .05, '***' = .01)) %>%
  tab_header(title="Панельные данные")%>%
  cols_width(everything()~px(200))%>%
  tab_footnote(md("Зависимая переменная - успеваемость школьника, с нормировкой на нормальное рапсределение"),
               locations=cells_title(groups="title")) %>%
  tab_footnote(md(" переменная равна 1, если у матери ребенка есть диплом о высшем образовании/ мать училась в аспирантуре (вне зависимости от факта получения диплома)"),
               locations=cells_body(columns=1,rows=5)) %>%
  tab_footnote(md(" переменная равна 1, если у отца ребенка есть диплом о высшем образовании/ отец учился в аспирантуре (вне зависимости от факта получения диплома)"),
               locations=cells_body(columns=1,rows=7)) %>%
  tab_footnote(md(" переменная равна 1 для ребенка женского пола"),
               locations=cells_body(columns=1,rows=11)) %>%
  tab_footnote(md("в сравнении с областным центром"),
               locations=cells_body(columns=1,rows=c(21,23))) %>%
  tab_footnote(md("В скобках указаны стандартные ошибки"),
               locations=cells_title(groups="title"))


#### Кодировка 2 (обычная) ####
#загрузка
lob$city <- factor(lob$city)
data2 <- lob
str(data2)
data2 <- dplyr::select(data2, grades, log_hh_income, educ_mot, educ_fat, child_age,
                       female, num_child, doeswork_mot, ill_mot, near, city)
attach(data2)

#### OLS ####
data_fat_1 <- data2 %>% na.omit() %>% filter(log_hh_income > 0)
dim(data_fat_1)
mod2_1 <- lm(grades ~ . , data_fat_1)
summary(mod2_1)

coeftest(mod2_1, vcov. = vcovHC(mod2_1, type = "HC1"))

#### Порядковая (кодировка 3) ####
categ <- categor_data
categ$city <- factor(categ$city)
categ$grades <- factor(categ$grades, levels = c("Почти все - пятерки", "В основном пятерки и четверки",
                                                "В основном четверки", "В основном четверки и тройки", "В основном тройки")[5:1], ordered = TRUE)
data4 <- dplyr::select(categ, grades, log_hh_income, educ_mot, educ_fat, child_age,
                       female, num_child, doeswork_mot, ill_mot, near, city)
data_fat3 <- data4 %>% na.omit() %>% filter(log_hh_income > 0)
summary(data_mot4)
mod4 <- polr(grades ~ . , data_fat3)
summary(mod4)
coeftest(mod4)
vif(mod4)

#ROC-кривая
Pr_mn <- predict(mod4, type="class")
Pr_mn <- factor(Pr_mn$grades, levels = c("Почти все - пятерки", "В основном пятерки и четверки",
                                         "В основном четверки", "В основном четверки и тройки", "В основном тройки")[5:1], ordered=TRUE)
Pr <- predict(mod4, type="probs")
summary(Pr)
colnames(Pr) <- c('n_1', 'n_2', 'n_3', 'n_4', 'n_5')
y_true <- factor(data_fat3$grades, labels = c('n_1', 'n_2', 'n_3', 'n_4', 'n_5'))
y_predict <- factor(Pr_mn,labels = c('n_2', 'n_4', 'n_5'))


Pr_mn <- predict(mod4, type="class")
Pr_mn <- factor(Pr_mn$grades, levels = c("Почти все - пятерки", "В основном пятерки и четверки",
                                         "В основном четверки", "В основном четверки и тройки", "В основном тройки")[5:1], ordered=TRUE)
y_predict <- factor(Pr_mn,labels = c('n_2', 'n_4', 'n_5'))

install.packages("pscl")
library(pscl)
hitmiss(mod4_stepAIC)
mplot_roc(y_true, y_predict, multis = Pr)

summary(mod4)

#R^2 МакФаддена
PseudoR2(mod4)#по умолчанию МакФаддена
PseudoR2(mod4)


#### Выдача для трёх моделей ####
#добавим подписи к переменным

ar <- data.frame("95%-ный доверительный интервал для площади под ROC-кривой", " ", " ", "65.09-70.45")

ar_coef <- c("(константа)",
             "Наличие высшего образования у матери",
             "Наличие высшего образования у отца",
             "Кол-во полных лет ребенка",
             "Пол ребёнка",
             "Кол-во детей в семье",
             "Наличие у мамы ребенка работы на момент проведения интервью",
             "Ребенок добирается до школы пешком или обучается дома",
             "Населённый пункт имеет статус города",
             "Населённый пункт имеет статус села/ПГТ",
             "В основном тройки|В основном четверки и тройки",
             "В основном четверки и тройки|В основном четверки",
             "В основном четверки|В основном пятерки и четверки",
             "В основном пятерки и четверки|Почти все - пятерки")

msummary(models=list("(1)"=mod2_AIC, "(2)" = mod2_1_AIC, "(3)"= mod4_stepAIC), coef_rename = ar_coef,
         add_rows = ar, gof_omit = "AIC|RMSE|Log.Lik.|BIC", align=c("lccc"), output="gt", 
         vcov=c('HC1', 'HC1', "classical"), stars = c('*' = .1, '**' = .05, '***' = .01)) %>%
  tab_header(title="Регрессии на данных RLMS")%>%
  cols_width(everything()~px(200))%>%
  tab_footnote(md("Зависимая переменная для первой регрессии - успеваемость школьника с нормировкой на нормальное распределение,
для второй регрессии используется наивная кодировка. Третья модель - порядковая логистическая с порядковой (категориальной) зависимой
переменной"),
               locations=cells_title(groups="title")) %>%
  tab_footnote(md(" переменная равна 1, если у матери ребенка есть диплом о высшем образовании/ мать училась в аспирантуре (вне зависимости от факта получения диплома)"),
               locations=cells_body(columns=1,rows=3)) %>%
  tab_footnote(md(" переменная равна 1, если у отца ребенка есть диплом о высшем образовании/ отец учился в аспирантуре (вне зависимости от факта получения диплома)"),
               locations=cells_body(columns=1,rows=5)) %>%
  tab_footnote(md(" переменная равна 1 для ребенка женского пола"),
               locations=cells_body(columns=1,rows=9)) %>%
  tab_footnote(md("в сравнении с областным центром"),
               locations=cells_body(columns=1,rows=c(17,19))) %>%
  tab_footnote(md("В скобках указаны стандартные ошибки"),
               locations=cells_title(groups="title"))

#### Проверки ####
vif(mod2)
vif(mod2_1)
vif(mod4)
resettest(mod2)
resettest(mod2_1)
resettest(mod4)
#Рамсея прохожу и без квадрата

mod2_AIC <- stepAIC(mod2)
mod2_1_AIC <- stepAIC(mod2_1)
mod4_stepAIC <- stepAIC(mod4)

