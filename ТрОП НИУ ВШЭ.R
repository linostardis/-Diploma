#### Читаю данные ####
library(haven)
panel2011_1_4ru <- read_sav("~/Downloads/NP_1_spss.zip Folder/panel2011_1_4ru.sav")
# NP_2wave_RU <- read_sav("~/Downloads/NP_2_spss.zip Folder/NP 2wave RU.sav")
# NP_3wave_RU_UPD_2016_02_16 <- read_sav("~/Downloads/NP_3_spss.zip Folder/NP 3wave RU_UPD_2016.02.16.sav")
# NP_6_wave_RU_2020 <- read_sav("~/Downloads/NP_6_spss_2020UPD.rar Folder/NP 6_wave_RU_2020.sav")
# NP_8_wave_RU <- read_sav("~/Downloads/NP_8_spss/NP 8_wave_RU.sav")
# RUS_COG12_NATOPT <- read_sav("~/Downloads/PISA_spss.zip Folder/RUS_COG12_NATOPT.sav")
# RUS_COG12_S_NATOPT <- read_sav("~/Downloads/PISA_spss.zip Folder/RUS_COG12_S_NATOPT.sav")
# RUS_SCQ12_NATOPT_norm <- read_sav("~/Downloads/PISA_spss.zip Folder/RUS_SCQ12_NATOPT_norm.sav")
# RUS_STU12_NATOPT_norm <- read_sav("~/Downloads/PISA_spss.zip Folder/RUS_STU12_NATOPT_norm.sav")

#### Первая волна ####
wave1 <- subset(panel2011_1_4ru, select = c(aRegion, aQ1, aQ5_1, aQ5_2, aQ6, aQ7_1, aQ7_2, aQ11_1, aQ11_2, aQ11_3, aQ11_4, aQ11_5, 
                                            aQ11_6, aQ11_7, aQ11_8, aQ14, aQ16, aQ16d, aS2, aS3_a, aS3_b, aS4_a, aS4_b, aS7, aS10,
                                            aS13_1, aS13_2, aS13_3,  aS17, aS8_1, aS8_2, aS8_4,
                                            aS8_7, aS8_8, aQ27, aS12_3, aS12_4, aQ28_1, aQ8_1, aQ8_2,
                                            aQ8_3, aQ7_4, aQ18))

wave1_0 <- wave1 %>% 
  rename(
    region = aRegion, sex = aQ1, live_w_mot = aQ5_1, live_w_fat = aQ5_2, near = aQ6, school_spec = aQ7_1,
    extraord_learn = aQ7_2, gr_alg = aQ11_1, gr_geom = aQ11_2, gr_rus = aQ11_3, gr_liter = aQ11_4, gr_hist = aQ11_5, gr_phys = aQ11_6, 
    gr_chem = aQ11_7, gr_biol = aQ11_8, hour_hw = aQ14, hw_check_par = aQ16, do_check = aQ16d, num_child = aS2, educ_mot = aS3_a, educ_fat = aS3_b,
    job_mot = aS4_a, job_fat = aS4_b, hh_income = aS7, books = aS10, hw_help = aS13_1,
    add_knowl_help = aS13_3, repetit = aS13_2, expect_educ = aS17, hasTable = aS8_1, hasRoom = aS8_2,
    hasCompstudy = aS8_4, hasClassics = aS8_7, hasPoems = aS8_8, healthprobl = aQ27, 
    callteachers = aS12_3, speakteachers = aS12_4, doallneeded = aQ28_1, olimpwin = aQ7_4, orchestra = aQ8_1,
    teather = aQ8_2, schpaper = aQ8_3, chores = aQ18
  )

#затрудняюсь ответить в NA
wave1_1 <- wave1_0
wave1_1$gr_alg <- replace(wave1_1$gr_alg, wave1_1$gr_alg == 99, NA)
wave1_1$gr_geom <- replace(wave1_1$gr_geom, wave1_1$gr_geom == 99, NA)
wave1_1$gr_rus <- replace(wave1_1$gr_rus, wave1_1$gr_rus == 99, NA)
wave1_1$gr_liter <- replace(wave1_1$gr_liter, wave1_1$gr_liter == 99, NA)
wave1_1$gr_hist <- replace(wave1_1$gr_hist, wave1_1$gr_hist == 99, NA)
wave1_1$gr_phys <- replace(wave1_1$gr_phys, wave1_1$gr_phys == 99, NA)
wave1_1$gr_chem <- replace(wave1_1$gr_chem, wave1_1$gr_chem == 99, NA)
wave1_1$gr_biol <- replace(wave1_1$gr_biol, wave1_1$gr_biol == 99, NA)


#переконструирую переменные
wave1_2 <- wave1_1
wave1_2$educ_mot <- ifelse(wave1_2$educ_mot==6 | wave1_2$educ_mot==7, 1, 0)
wave1_2$educ_fat <- ifelse(wave1_2$educ_fat==6 | wave1_2$educ_fat==7, 1, 0)
wave1_2$mean_grade <- (wave1_2$gr_alg + wave1_2$gr_geom + wave1_2$gr_rus + wave1_2$gr_liter +
                         wave1_2$gr_hist + wave1_2$gr_phys + wave1_2$gr_chem + wave1_2$gr_biol)/8
wave1_2$books201 <- ifelse(wave1_2$books==6 | wave1_2$books==5, 1, 0)
wave1_2$chief_mot <- ifelse(wave1_2$job_mot == 9, 1, 0)
wave1_2$chief_fat <- ifelse(wave1_2$job_fat == 9, 1, 0)
wave1_2$near <- ifelse(wave1_2$near == 1, 1, 0)
wave1_2$school_spec <- ifelse(wave1_2$school_spec == 1, 1, 0)
wave1_2$extraord_learn <- ifelse(wave1_2$extraord_learn == 1, 1, 0)
wave1_2$live_w_mot <- ifelse(wave1_2$live_w_mot == 1, 1, 0)
wave1_2$live_w_fat <- ifelse(wave1_2$live_w_fat == 1, 1, 0)
wave1_2$do_check <- ifelse(wave1_2$do_check == 1, 1, 0)
wave1_2$repetit <- ifelse(wave1_2$repetit == 1, 1, 0)
wave1_2$add_knowl_help <- ifelse(wave1_2$add_knowl_help == 1, 1, 0)
wave1_2$hw_help <- ifelse(wave1_2$hw_help == 1, 1, 0)
wave1_2$expect_higher <- ifelse(wave1_2$expect_educ == 5 | wave1_2$expect_educ == 6, 1, 0)
wave1_2$hasClassicPoem <- ifelse(wave1_2$hasPoems == 1 & wave1_2$hasClassics == 1, 1, 0)
wave1_2$livetogether <- ifelse(wave1_2$live_w_mot == 1 & wave1_2$live_w_fat == 1, 1, 0)
wave1_2$hasCompstudy <- ifelse(wave1_2$hasCompstudy == 1, 1, 0)
wave1_2$hasRoom <- ifelse(wave1_2$hasRoom == 1, 1, 0)
wave1_2$hasTable <- ifelse(wave1_2$hasTable == 1, 1, 0)
wave1_2$healthprobl <- ifelse(wave1_2$healthprobl == 1 | wave1_2$healthprobl == 2, 1, 0)
wave1_2$callteachers <- ifelse(wave1_2$callteachers == 1, 1, 0)
wave1_2$speakteachers <- ifelse(wave1_2$speakteachers == 1, 1, 0)
wave1_2$doallneeded <- ifelse(wave1_2$doallneeded == 5, 1, 0)
wave1_2$olimpwin <- ifelse(wave1_2$olimpwin == 1, 1, 0)
wave1_2$developedsch <- ifelse(wave1_2$orchestra == 1 | wave1_2$teather == 1 | wave1_2$schpaper == 1, 1, 0)
wave1_2$chores <- ifelse(wave1_2$chores == 1 | wave1_2$chores == 2, 1, 0)
wave1_2$anychief <- ifelse(wave1_2$chief_fat == 1 | wave1_2$chief_mot == 1, 1, 0)

#удаляю исходники переконструируемых
wave1_2$sex<- NULL
wave1_2$gr_alg <- NULL
wave1_2$gr_geom <- NULL
wave1_2$gr_rus <- NULL
wave1_2$gr_liter <- NULL
wave1_2$gr_hist <- NULL
wave1_2$gr_phys <- NULL
wave1_2$gr_chem <- NULL
wave1_2$gr_biol <- NULL
wave1_2$books <- NULL
wave1_2$expect_educ <- NULL
wave1_2$hasClassics <- NULL
wave1_2$hasPoems <- NULL
wave1_2$live_w_mot <- NULL
wave1_2$live_w_fat <- NULL
wave1_2$orchestra <- NULL
wave1_2$teather <- NULL
wave1_2$schpaper <- NULL
wave1_2$job_mot <- NULL
wave1_2$job_fat <- NULL
wave1_2$chief_fat <- NULL
wave1_2$chief_mot <- NULL
wave1_2$doallneeded <- NULL
wave1_2$expect_higher <- NULL

wave1_2$hasTable <- NULL

wave1_2$region <- factor(wave1_2$region) 
wave1_2$hh_income <- factor(wave1_2$hh_income)
wave1_2$hour_hw <- as.numeric(wave1_2$hour_hw)
wave1_2$hw_check_par <- as.numeric(wave1_2$hw_check_par)
wave1_2$num_child  <- as.numeric(wave1_2$num_child)
summary(wave1_2)

# чистим для удаления выбросов
table(wave1_2$num_child)
wave1_3 <- wave1_2 %>% filter(num_child<=6, hw_check_par<=20)

find <- wave1_3 %>% filter(hh_income == 4)
summary(find$mean_grade) #2.250
table(find$mean_grade)

wave1_3 <- wave1_3[!(wave1_3$hh_income == 4 & wave1_3$mean_grade == 2.250), ]

write.csv(wave1_3, "/Users/bookmac/Downloads/wave1_3_4.csv", row.names=FALSE)


#### Описательная статистика ####
library(ggplot2)
ggplot(wave1_3, aes(x = hh_income, y = mean_grade, fill = hh_income))  + 
  stat_boxplot(geom = "errorbar") + 
  geom_boxplot() + 
  xlab("Доход домохозяйства") + 
  ylab("Средняя годовая оценка") + 
  ggtitle("Средняя годовая оценка") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

ggplot(wave1_3, aes(x = "", y = mean_grade))  + 
  geom_violin() + 
  ylab("оценки") + 
  ggtitle("Средняя годовая оценка") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

library(ggplot2)
ggplot(wave1_3, aes(x = mean_grade)) + geom_density(alpha = .3, fill = "blue", line = "#1F3552") + 
  scale_x_continuous(name = "Средняя годовая оценка") + scale_y_continuous(name = "Плотность распределения") +
  ggtitle("Плотность распределения средних годовых оценок") + theme_bw()

stargazer(wave1_3, type = "text", title="Описательная статистика для числовых переменных",
          digits=3, out="smth.text")


summary(wave1_3)
View(tables)

?geom_density

colnames(wave1_3)

library(gt)
# install.packages("gtExtras")
library(gtExtras)

?ols_width

summary(wave1_3)

#### OLS ####
# Модель 1 OLS содержательно важно контролировать все region, livetogether, num_child для доли дохода каждого
dfwave1 <- dplyr::select(wave1_3, mean_grade, hh_income, educ_mot, educ_fat,
                         region, livetogether, num_child) %>% na.omit()
dim(dfwave1)
wavveee<- data.frame(summary(dfwave1))
View(wavveee)

modt1 <- lm(mean_grade ~ hh_income + educ_mot + educ_fat + livetogether + num_child + region, dfwave1) # регистрируем различия в доходах регионов
summary(modt1)

modt1_AIC <- stepAIC(modt1)
summary(modt1_AIC)

modt1_plus <- lm(mean_grade ~ hh_income + educ_mot + educ_fat + livetogether + num_child + region, dfwave1) # регистрируем различия в доходах регионов
summary(modt1_plus)

# Модель 2 OLS c характеристиками школы
dfwave2 <- dplyr::select(wave1_3, mean_grade, hh_income, educ_mot, educ_fat,
                         region, livetogether, num_child, near, 
                         developedsch) %>% na.omit()
# school_spec убираем
dim(dfwave2)
summary(dfwave2)
modt2 <- lm(mean_grade ~ hh_income + educ_mot + educ_fat + livetogether + num_child + near
            + developedsch + region, dfwave2) 
summary(modt2)

modt2_AIC <- stepAIC(modt2)
summary(modt2_AIC)

modt2_plus <- lm(mean_grade ~ hh_income + educ_mot + educ_fat + livetogether + num_child 
                 + near + developedsch + region, dfwave2) # регистрируем различия в доходах регионов
summary(modt2_plus)

# Модель 3 OLS созданные условия для обучения в семье
dfwave3 <- dplyr::select(wave1_3, mean_grade, hh_income, educ_mot, educ_fat,
                         region, livetogether, num_child, chores, hasCompstudy, hasRoom, add_knowl_help,
                         near, developedsch) %>% na.omit()
dim(dfwave3)
summary(dfwave3)

modt3 <- lm(mean_grade ~ hh_income + educ_mot + educ_fat + livetogether + num_child + 
              chores + chores + hasCompstudy + hasRoom + add_knowl_help 
            + near + developedsch + region, dfwave3) 
summary(modt3)

modt3_AIC <- stepAIC(modt3)
summary(modt3_AIC)

modt3_plus <- lm(mean_grade ~ hh_income + educ_mot + educ_fat + livetogether + num_child
                 + chores + hasCompstudy + hasRoom + add_knowl_help + near + developedsch + region, dfwave3) # регистрируем различия в доходах регионов
summary(modt3_plus)

# Модель 4 OLS индивидуальные характеристики
dfwave4 <- dplyr::select(wave1_3, mean_grade, hh_income, educ_mot, educ_fat,
                         region, livetogether, num_child, chores, hasCompstudy, hasRoom, add_knowl_help,
                         near, developedsch, healthprobl, hour_hw) %>% na.omit()
dim(dfwave4)
summary(dfwave4)

modt4 <- lm(mean_grade ~ hh_income + educ_mot + educ_fat + livetogether + num_child + 
              chores + chores + hasCompstudy + hasRoom + add_knowl_help 
            + near + developedsch +
              healthprobl + hour_hw + region, dfwave4) 
summary(modt4)

modt4_AIC <- stepAIC(modt4)
summary(modt4_AIC)

modt4_plus <- lm(mean_grade ~ hh_income + educ_mot + educ_fat + livetogether + num_child +
                   chores + hasCompstudy + hasRoom + add_knowl_help + near + developedsch + healthprobl + hour_hw + region, dfwave4) # регистрируем различия в доходах регионов
summary(modt4_plus)

# Модель 5 OLS c прокси культурного капитала семьи
dfwave5 <- dplyr::select(wave1_3, mean_grade, hh_income, educ_mot, educ_fat,
                         region, livetogether, num_child, chores, hasCompstudy, hasRoom, add_knowl_help,
                         near, developedsch, healthprobl, hour_hw,
                         hasClassicPoem, books201) %>% na.omit()
dim(dfwave5)
summary(dfwave5)

modt5 <- lm(mean_grade ~ hh_income + educ_mot + educ_fat + livetogether + num_child + 
              chores + chores + hasCompstudy + hasRoom + add_knowl_help 
            + near + developedsch +
              healthprobl + hour_hw +
              hasClassicPoem + books201 + region , dfwave5) 
summary(modt5)

modt5_AIC <- stepAIC(modt5)
summary(modt5_AIC)

modt5_plus <- lm(mean_grade ~ hh_income + educ_mot + educ_fat + livetogether + num_child +
                   chores + hasCompstudy + hasRoom + near + developedsch + healthprobl +
                   hour_hw + hasClassicPoem + books201 + region, dfwave5) # регистрируем различия в доходах регионов
summary(modt5_plus)

# Модель 6 OLS ориентация на достижение = ИТОГО СОБРАЛИ ВСЕ ПЕРЕМЕННЫЕ
dfwave6 <- dplyr::select(wave1_3, mean_grade, hh_income, educ_mot, educ_fat,
                         region, livetogether, num_child, chores, hasCompstudy, hasRoom, add_knowl_help,
                         near, developedsch, healthprobl, hour_hw, 
                         hasClassicPoem, books201, anychief) %>% na.omit()
dim(dfwave6)
summary(dfwave6)
modt6 <- lm(mean_grade ~ ., dfwave6) 
summary(modt6)

modt6_AIC <- stepAIC(modt6)
summary(modt6_AIC)

modt6_plus <- lm(mean_grade ~ hh_income + educ_mot + educ_fat + livetogether + num_child +
                   chores + hasCompstudy + near + developedsch + healthprobl +
                   hour_hw + hasClassicPoem + books201 + region, dfwave6) # регистрируем различия в доходах регионов
summary(modt6_plus)

library(car)
vif(modt1)
vif(modt2)
vif(modt3)
vif(modt4)
vif(modt5)
vif(modt6)
vif(modt7)

################# ОБЪЕДИНИМ МОДЕЛИ

coef <- c("(константа)", "Доход семьи от 20 до 29 тыс. рублей", "Доход семьи от 30 до 49 тыс. рублей", "Доход семьи от 50 до 79 тыс. рублей",
          "Доход семьи свыше 80 тыс. рублей", "Наличие одного/двух высших образований или ученой степени у матери", "Наличие одного/двух высших образований или ученой степени у отца",
          "Живет с матерью и отцом", "Кол-во детей в семье", "Учится в ближайшей к месту жительства школе",
          "Наличие в школе ребенка хотя бы одного из списка: оркестр/хор/ансамбль, театр, ежегодник/журнал/газета",
          "Работа по дому обычно/иногда мешает учебе", "Наличие компьютера для учебных заданий", 
          "Наличие отдельной комнаты для ребенка", "Родитель дает ребенку доп. литературу", 
          "Наличие сложностей с учебой по здоровью: часто/время от времени", "Кол-во часов в день на выполнение д/з",
          "Наличие в семье классической литературы и сборников стихов",
          "Наличие в доме семьи не менее 201 книги", "По крайней мере один из родителей занимает руководящую должность")
length(coef)
ar <- data.frame("Region control", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")

# install.packages("gt")
# install.packages("modelsummary")
library(gt)
library(modelsummary)

msummary(models = list(modt1, modt2, modt3, modt4, modt5, modt6), coef_omit = "region",
         add_rows = ar, gof_omit = "AIC|RMSE|Log.Lik.|BIC", align=c("lcccccc"), output="gt", coef_rename = coef,
         vcov="HC1", stars = c('*' = .1, '**' = .05, '***' = .01)) %>%
  tab_header(title="Множественная регрессия на данных ТрОП") %>% 
  tab_footnote(md("Зависимая переменная - средняя годовая оценка школьника по следующем предметам: алгебра, геометрия, русский, литература, история, физика, химия, биология"),
               locations=cells_title(groups="title")) %>% tab_footnote(md("В сравнении с доходом менее 20 тыс. рублей"), locations=cells_body(columns=1,rows=c(3,5,7,9)))

# coef_rename = coef, add_rows = ar,
#   

coef_br <- c("(константа)", "Доход семьи от 20 до 29 тыс. рублей", "Доход семьи от 30 до 49 тыс. рублей", "Доход семьи от 50 до 79 тыс. рублей",
             "Доход семьи свыше 80 тыс. рублей", "Наличие одного/двух высших образований или ученой степени у матери", "Наличие одного/двух высших образований или ученой степени у отца",
             "Живет с матерью и отцом", "Учится в ближайшей к месту жительства школе",
             "Наличие в школе ребенка хотя бы одного из списка: оркестр/хор/ансамбль, театр, ежегодник/журнал/газета",
             "Работа по дому обычно/иногда мешает учебе", "Наличие компьютера для учебных заданий", 
             "Наличие отдельной комнаты для ребенка", "Родитель дает ребенку доп. литературу", 
             "Наличие сложностей с учебой по здоровью: часто/время от времени", "Кол-во часов в день на выполнение д/з",
             "Наличие в семье классической литературы и сборников стихов",
             "Наличие в доме семьи не менее 201 книги")


br <- data.frame("Region control", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
msummary(models = list(modt1_AIC, modt2_AIC, modt3_AIC, modt4_AIC, modt5_AIC, modt6_AIC), coef_omit = "region",
         gof_omit = "AIC|RMSE|Log.Lik.|BIC", align=c("lcccccc"), output="gt", add_rows = br, coef_rename = coef_br,
         vcov="HC1", stars = c('*' = .1, '**' = .05, '***' = .01)) %>%
  tab_header(title="Множественная регрессия на данных ТрОП") %>% 
  tab_footnote(md("Зависимая переменная - средняя годовая оценка школьника по следующем предметам: алгебра, геометрия, русский, литература, история, физика, химия, биология"),
               locations=cells_title(groups="title")) %>% tab_footnote(md("В сравнении с доходом менее 20 тыс. рублей"), locations=cells_body(columns=1,rows=c(3,5,7,9)))


#### Квантильная регрессия ####
ols_res <- lm(mean_grade~hh_income+ educ_mot+ educ_fat+
                region+ livetogether+ num_child+ chores+ hasCompstudy+
                hasRoom+add_knowl_help+ near+ developedsch+ healthprobl+
                hour_hw+hasClassicPoem+ books201+ anychief, timms)
ols_tidy <- tidy(ols_res)
ols_tidy <- ols_tidy[!grepl('region', ols_tidy$term),]
ols_tidy <- ols_tidy[!grepl('add_knowl_help', ols_tidy$term),]
ols_tidy <- ols_tidy[!grepl('anychief', ols_tidy$term),]
ols_tidy <- ols_tidy[!grepl('chores', ols_tidy$term),]
ols_tidy <- ols_tidy[!grepl('hasRoom', ols_tidy$term),]
ols_tidy <- ols_tidy[!grepl('hasClassicPoem', ols_tidy$term),]
ols_tidy <- ols_tidy[!grepl('near', ols_tidy$term),]
ols_tidy <- ols_tidy[!grepl('num_child', ols_tidy$term),]
ols_tidy <- ols_tidy[!grepl('livetogether', ols_tidy$term),]
ols_tidy <- ols_tidy[!grepl('healthprobl', ols_tidy$term),]
ols_tidy <- ols_tidy[!grepl('books201', ols_tidy$term),]

ols_tidy$term <- c("(константа)", "Доход семьи от 20 до 29 тыс. рублей", "Доход семьи от 30 до
49 тыс. рублей", "Доход семьи от 50 до 79 тыс. рублей",
                   "Доход семьи свыше 80 тыс. рублей", "Наличие одного/двух высших
образований или ученой степени у матери", "Наличие одного/двух высших
образований или ученой степени у отца",
                   "Наличие компьютера для учебных заданий", "Наличие в школе ребенка хотя бы одного из списка: оркестр/хор/ансамбль,
театр, ежегодник/журнал/газета",
                   "Кол-во часов в день на выполнение д/з")
mod_final <- rq(mean_grade~hh_income+ educ_mot+ educ_fat+
                  region+ livetogether+ num_child+ chores+ hasCompstudy+ hasRoom+ add_knowl_help+
                  near+ developedsch+ healthprobl+ hour_hw+
                  hasClassicPoem+ books201+ anychief, tau=seq(0.1, 0.9, 0.1), timms)
summary(mod_final, se="boot")
qr_tidy <- broom::tidy(mod_final, se.type = "boot", conf.int = TRUE, conf.level=.95) %>%
  bind_rows()
qr_tidy <- qr_tidy[!grepl('region', qr_tidy$term),]
qr_tidy <- qr_tidy[!grepl('add_knowl_help', qr_tidy$term),]
qr_tidy <- qr_tidy[!grepl('anychief', qr_tidy$term),]
qr_tidy <- qr_tidy[!grepl('chores', qr_tidy$term),]
qr_tidy <- qr_tidy[!grepl('hasRoom', qr_tidy$term),]
qr_tidy <- qr_tidy[!grepl('hasClassicPoem', qr_tidy$term),]
qr_tidy <- qr_tidy[!grepl('near', qr_tidy$term),]
qr_tidy <- qr_tidy[!grepl('num_child', qr_tidy$term),]
qr_tidy <- qr_tidy[!grepl('livetogether', qr_tidy$term),]
qr_tidy <- qr_tidy[!grepl('healthprobl', qr_tidy$term),]
qr_tidy <- qr_tidy[!grepl('books201', qr_tidy$term),]

qr_tidy$term <- rep(c("Доход семьи от 20 до 29 тыс. рублей", "Доход семьи от 30 до
49 тыс. рублей", "Доход семьи от 50 до 79 тыс. рублей",
                      "Доход семьи свыше 80 тыс. рублей", "Наличие одного/двух высших
образований или ученой степени у матери", "Наличие одного/двух высших
образований или ученой степени у отца",
                      "Наличие компьютера для учебных заданий", "Наличие в школе ребенка хотя бы одного из списка: оркестр/хор/ансамбль,
театр, ежегодник/журнал/газета",
                      "Кол-во часов в день на выполнение д/з"), 9)
qr_tidy %>%
  ggplot(aes(x = tau,
             y = estimate
  )) +
  geom_point(color = "black",
             size = 3) +
  geom_line(color="black",
            size = 1) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high),alpha=0.5, fill="#D3D3D3") +
  geom_hline(data = ols_tidy,
             aes(yintercept = 0),
             color = "#8B0000", size=1.25) +
  facet_wrap(~term,
             scales="free",
             ncol=2) + theme_minimal()+theme(text = element_text(size=20, face="bold.italic"), axis.title=element_text(size=24))
+ xlab("Квантиль")+ylab("Оценка коэффициента")
