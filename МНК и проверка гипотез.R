library(lmtest)       # тестирование гипотез
library(sandwich)     # робастные стандартные ошибки
library(stargazer) # представление результатов нескольких регрессий в одной таблице
library(dplyr)   # манипуляции с данными
library(tidyr)   # манипуляции с данными
library(ggplot2) # визуализация данных
library(psych)   # описательные статистики
library(broom)   # удобное представление результатов тестов
library(modelsummary) # представление результатов нескольких регрессий в одной таблице
library(readxl)


## Загружаем и готовим  датасет ================================================

our_data_ecox_upd <- read_excel("C:/Anastasia_/HSE/3 курс/Метрика/Проект/our_data_ecox_upd.xlsx")

# удаляем выбросы (без СПб и МСК)
data_ex_out <- our_data_ecox_upd %>% filter((Index_ves_19<50)&(Index_ves_19>25)) %>% filter(sum_coef_birth_19<2)


## Строим МНК-модели ===========================================================

# формализуем спецификации

spec_1 <- sum_coef_birth_19 ~ Index_ves_19 + Index_ves_19:is_ex

spec_2 <- sum_coef_birth_19 ~ Index_ves_19 + unemp_19 + Index_ves_19:is_ex

spec_3 <- sum_coef_birth_19 ~ Index_ves_19 + Index_ves_19:is_ex + 
                   unemp_19 + crime_19 + urban_19

spec_4 <- sum_coef_birth_19 ~ Index_ves_19 + Index_ves_19:is_ex + 
  unemp_19 + crime_19 + urban_19 + is_SKFO + is_UFO + 
  is_SZFO + is_PFO + is_UrFO + is_SFO + is_SZFO + is_DVFO

# оценим МНК на полном датасете

reg_1_all <- lm(spec_1, data = our_data_ecox_upd)
reg_2_all <- lm(spec_2, data = our_data_ecox_upd)
reg_3_all <- lm(spec_3, data = our_data_ecox_upd)
reg_4_all <- lm(spec_4, data = our_data_ecox_upd)

# посмотрим на робастные оценки коэффициентов

cov_1_all <- sandwich::vcovHC(reg_1_all, type = "HC0")
se_1_all <- sqrt(diag(cov_1_all))
coeftest(reg_1_all, df = Inf, vcov = cov_1_all)

cov_2_all <- sandwich::vcovHC(reg_2_all, type = "HC0")
se_2_all <- sqrt(diag(cov_2_all))
coeftest(reg_2_all, df = Inf, vcov = cov_2_all)

cov_3_all <- sandwich::vcovHC(reg_3_all, type = "HC0")
se_3_all <- sqrt(diag(cov_3_all))
coeftest(reg_3_all, df = Inf, vcov = cov_3_all)

cov_4_all <- sandwich::vcovHC(reg_4_all, type = "HC0")
se_4_all <- sqrt(diag(cov_4_all))
coeftest(reg_4_all, df = Inf, vcov = cov_4_all)

# оценим МНК на очищенном от выбросов датасете

reg_1_out <- lm(spec_1, data = data_ex_out)
reg_2_out <- lm(spec_2, data = data_ex_out)
reg_3_out <- lm(spec_3, data = data_ex_out)
reg_4_out <- lm(spec_4, data = data_ex_out)

# посмотрим на робастные оценки коэффициентов

cov_1_out <- sandwich::vcovHC(reg_1_out, type = "HC0")
se_1_out <- sqrt(diag(cov_1_out))
coeftest(reg_1_out, df = Inf, vcov = cov_1_out)

cov_2_out <- sandwich::vcovHC(reg_2_out, type = "HC0")
se_2_out <- sqrt(diag(cov_2_out))
coeftest(reg_2_out, df = Inf, vcov = cov_2_out)

cov_3_out <- sandwich::vcovHC(reg_3_out, type = "HC0")
se_3_out <- sqrt(diag(cov_3_out))
coeftest(reg_3_out, df = Inf, vcov = cov_3_out)

cov_4_out <- sandwich::vcovHC(reg_4_out, type = "HC0")
se_4_out <- sqrt(diag(cov_4_out))
coeftest(reg_4_out, df = Inf, vcov = cov_4_out)


## Сведём результаты в таблицы =================================================

stargazer(reg_1_all, reg_2_all, reg_3_all, reg_4_all,
          se = list(se_1_all,se_2_all,se_3_all,se_4_all),
          column.labels = c("Базовая", "С учётом U", "С учётом соцдем-факторов",
                            "С учётом геофакторов"),
          colnames = FALSE,
          type = "text",
          output = "results.docx")



stargazer(reg_1_out, reg_2_out, reg_3_out, reg_4_out,
                    se = list(se_1_out,se_2_out,se_3_out,se_4_out),
                    column.labels = c("Базовая", "С учётом U", "С учётом соцдем-факторов",
                                      "С учётом геофакторов"),
                    colnames = FALSE,
                    type = "text",
                    output = "results.docx")


cov_1_all


## Проверка гипотез =============================================================

# 1) Для очищенной выборки

margin1 <- reg_1_out %>% margins_summary(variables = 'Index_ves_19',
                                         at = list(is_ex = c(0, 1)),
                                         vcov = cov_1_out) # предельный эффект для 1й спецификации
margin1
margin1 %>%
  ggplot()+
  geom_point(aes(is_ex, AME))+
  geom_errorbar(aes(x=is_ex, ymin=lower, ymax = upper))+
  geom_hline(yintercept = 0, col = 'red', linetype = 'dashed')+
  xlab('Экстремальный регион')+
  ylab('Предельный эффект индекса доступности жилья') # график предельного эффекта для 1й спецификации

margin2 <- reg_2_out %>% margins_summary(variables = 'Index_ves_19',
                                         at = list(is_ex = c(0, 1)),
                                         vcov = cov_2_out) # предельный эффект для 2й спецификации
margin2
margin2 %>%
  ggplot()+
  geom_point(aes(is_ex, AME))+
  geom_errorbar(aes(x=is_ex, ymin=lower, ymax = upper))+
  geom_hline(yintercept = 0, col = 'red', linetype = 'dashed')+
  xlab('Экстремальный регион')+
  ylab('Предельный эффект индекса доступности жилья') # график предельного эффекта для 2й спецификации

margin3 <- reg_3_out %>% margins_summary(variables = 'Index_ves_19',
                                         at = list(is_ex = c(0, 1)),
                                         vcov = cov_3_out) # предельный эффект для 3й спецификации
margin3
margin3 %>%
  ggplot()+
  geom_point(aes(is_ex, AME))+
  geom_errorbar(aes(x=is_ex, ymin=lower, ymax = upper))+
  geom_hline(yintercept = 0, col = 'red', linetype = 'dashed')+
  xlab('Экстремальный регион')+
  ylab('Предельный эффект индекса доступности жилья') # график предельного эффекта для 3й спецификации

margin4 <- reg_4_out %>% margins_summary(variables = 'Index_ves_19',
                                         at = list(is_ex = c(0, 1)),
                                         vcov = cov_4_out) # предельный эффект для 4й спецификации
margin4
margin4 %>%
  ggplot()+
  geom_point(aes(is_ex, AME))+
  geom_errorbar(aes(x=is_ex, ymin=lower, ymax = upper))+
  geom_hline(yintercept = 0, col = 'red', linetype = 'dashed')+
  xlab('Экстремальный регион')+
  ylab('Предельный эффект индекса доступности жилья') # график предельного эффекта для 4й спецификации


# 2) Для полной выборки


margin1_all <- reg_1_all %>% margins_summary(variables = 'Index_ves_19',
                                         at = list(is_ex = c(0, 1)),
                                         vcov = cov_1_out) # предельный эффект для 1й спецификации
margin1_all
margin1_all %>%
  ggplot()+
  geom_point(aes(is_ex, AME))+
  geom_errorbar(aes(x=is_ex, ymin=lower, ymax = upper))+
  geom_hline(yintercept = 0, col = 'red', linetype = 'dashed')+
  xlab('Экстремальный регион')+
  ylab('Предельный эффект индекса доступности жилья') # график предельного эффекта для 1й спецификации

margin2_all <- reg_2_all %>% margins_summary(variables = 'Index_ves_19',
                                         at = list(is_ex = c(0, 1)),
                                         vcov = cov_2_out) # предельный эффект для 2й спецификации
margin2_all
margin2_all %>%
  ggplot()+
  geom_point(aes(is_ex, AME))+
  geom_errorbar(aes(x=is_ex, ymin=lower, ymax = upper))+
  geom_hline(yintercept = 0, col = 'red', linetype = 'dashed')+
  xlab('Экстремальный регион')+
  ylab('Предельный эффект индекса доступности жилья') # график предельного эффекта для 2й спецификации

margin3_all <- reg_3_all %>% margins_summary(variables = 'Index_ves_19',
                                         at = list(is_ex = c(0, 1)),
                                         vcov = cov_3_out) # предельный эффект для 3й спецификации
margin3_all
margin3_all %>%
  ggplot()+
  geom_point(aes(is_ex, AME))+
  geom_errorbar(aes(x=is_ex, ymin=lower, ymax = upper))+
  geom_hline(yintercept = 0, col = 'red', linetype = 'dashed')+
  xlab('Экстремальный регион')+
  ylab('Предельный эффект индекса доступности жилья') # график предельного эффекта для 3й спецификации

margin4_all <- reg_4_all %>% margins_summary(variables = 'Index_ves_19',
                                         at = list(is_ex = c(0, 1)),
                                         vcov = cov_4_out) # предельный эффект для 4й спецификации
margin4_all
margin4_all %>%
  ggplot()+
  geom_point(aes(is_ex, AME))+
  geom_errorbar(aes(x=is_ex, ymin=lower, ymax = upper))+
  geom_hline(yintercept = 0, col = 'red', linetype = 'dashed')+
  xlab('Экстремальный регион')+
  ylab('Предельный эффект индекса доступности жилья') # график предельного эффекта для 4й спецификации
