library(lmtest)       # òåñòèðîâàíèå ãèïîòåç
library(sandwich)     # ðîáàñòíûå ñòàíäàðòíûå îøèáêè
library(stargazer) # ïðåäñòàâëåíèå ðåçóëüòàòîâ íåñêîëüêèõ ðåãðåññèé â îäíîé òàáëèöå
library(dplyr)   # ìàíèïóëÿöèè ñ äàííûìè
library(tidyr)   # ìàíèïóëÿöèè ñ äàííûìè
library(ggplot2) # âèçóàëèçàöèÿ äàííûõ
library(psych)   # îïèñàòåëüíûå ñòàòèñòèêè
library(broom)   # óäîáíîå ïðåäñòàâëåíèå ðåçóëüòàòîâ òåñòîâ
library(modelsummary) # ïðåäñòàâëåíèå ðåçóëüòàòîâ íåñêîëüêèõ ðåãðåññèé â îäíîé òàáëèöå
library(readxl)


## Çàãðóæàåì è ãîòîâèì  äàòàñåò ================================================

our_data_ecox_upd <- read_excel("C:/Anastasia_/HSE/3 êóðñ/Ìåòðèêà/Ïðîåêò/our_data_ecox_upd.xlsx")

# óäàëÿåì âûáðîñû (áåç ÑÏá è ÌÑÊ)
data_ex_out <- our_data_ecox_upd %>% filter((Index_ves_19<50)&(Index_ves_19>25)) %>% filter(sum_coef_birth_19<2)


## Ñòðîèì ÌÍÊ-ìîäåëè ===========================================================

# ôîðìàëèçóåì ñïåöèôèêàöèè

spec_1 <- sum_coef_birth_19 ~ Index_ves_19 + Index_ves_19:is_ex

spec_2 <- sum_coef_birth_19 ~ Index_ves_19 + unemp_19 + Index_ves_19:is_ex

spec_3 <- sum_coef_birth_19 ~ Index_ves_19 + Index_ves_19:is_ex + 
                   unemp_19 + crime_19 + urban_19

spec_4 <- sum_coef_birth_19 ~ Index_ves_19 + Index_ves_19:is_ex + 
  unemp_19 + crime_19 + urban_19 + is_SKFO + is_UFO + 
  is_SZFO + is_PFO + is_UrFO + is_SFO + is_SZFO + is_DVFO

# îöåíèì ÌÍÊ íà ïîëíîì äàòàñåòå

reg_1_all <- lm(spec_1, data = our_data_ecox_upd)
reg_2_all <- lm(spec_2, data = our_data_ecox_upd)
reg_3_all <- lm(spec_3, data = our_data_ecox_upd)
reg_4_all <- lm(spec_4, data = our_data_ecox_upd)

# ïîñìîòðèì íà ðîáàñòíûå îöåíêè êîýôôèöèåíòîâ

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

# îöåíèì ÌÍÊ íà î÷èùåííîì îò âûáðîñîâ äàòàñåòå

reg_1_out <- lm(spec_1, data = data_ex_out)
reg_2_out <- lm(spec_2, data = data_ex_out)
reg_3_out <- lm(spec_3, data = data_ex_out)
reg_4_out <- lm(spec_4, data = data_ex_out)

# ïîñìîòðèì íà ðîáàñòíûå îöåíêè êîýôôèöèåíòîâ

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


## Ñâåä¸ì ðåçóëüòàòû â òàáëèöû =================================================

stargazer(reg_1_all, reg_2_all, reg_3_all, reg_4_all,
          se = list(se_1_all,se_2_all,se_3_all,se_4_all),
          column.labels = c("Áàçîâàÿ", "Ñ ó÷¸òîì U", "Ñ ó÷¸òîì ñîöäåì-ôàêòîðîâ",
                            "Ñ ó÷¸òîì ãåîôàêòîðîâ"),
          colnames = FALSE,
          type = "text",
          output = "results.docx")



stargazer(reg_1_out, reg_2_out, reg_3_out, reg_4_out,
                    se = list(se_1_out,se_2_out,se_3_out,se_4_out),
                    column.labels = c("Áàçîâàÿ", "Ñ ó÷¸òîì U", "Ñ ó÷¸òîì ñîöäåì-ôàêòîðîâ",
                                      "Ñ ó÷¸òîì ãåîôàêòîðîâ"),
                    colnames = FALSE,
                    type = "text",
                    output = "results.docx")


cov_1_all


## Ïðîâåðêà ãèïîòåç =============================================================

# 1) Äëÿ î÷èùåííîé âûáîðêè

margin1 <- reg_1_out %>% margins_summary(variables = 'Index_ves_19',
                                         at = list(is_ex = c(0, 1)),
                                         vcov = cov_1_out) # ïðåäåëüíûé ýôôåêò äëÿ 1é ñïåöèôèêàöèè
margin1
margin1 %>%
  ggplot()+
  geom_point(aes(is_ex, AME))+
  geom_errorbar(aes(x=is_ex, ymin=lower, ymax = upper))+
  geom_hline(yintercept = 0, col = 'red', linetype = 'dashed')+
  xlab('Ýêñòðåìàëüíûé ðåãèîí')+
  ylab('Ïðåäåëüíûé ýôôåêò èíäåêñà äîñòóïíîñòè æèëüÿ') # ãðàôèê ïðåäåëüíîãî ýôôåêòà äëÿ 1é ñïåöèôèêàöèè

margin2 <- reg_2_out %>% margins_summary(variables = 'Index_ves_19',
                                         at = list(is_ex = c(0, 1)),
                                         vcov = cov_2_out) # ïðåäåëüíûé ýôôåêò äëÿ 2é ñïåöèôèêàöèè
margin2
margin2 %>%
  ggplot()+
  geom_point(aes(is_ex, AME))+
  geom_errorbar(aes(x=is_ex, ymin=lower, ymax = upper))+
  geom_hline(yintercept = 0, col = 'red', linetype = 'dashed')+
  xlab('Ýêñòðåìàëüíûé ðåãèîí')+
  ylab('Ïðåäåëüíûé ýôôåêò èíäåêñà äîñòóïíîñòè æèëüÿ') # ãðàôèê ïðåäåëüíîãî ýôôåêòà äëÿ 2é ñïåöèôèêàöèè

margin3 <- reg_3_out %>% margins_summary(variables = 'Index_ves_19',
                                         at = list(is_ex = c(0, 1)),
                                         vcov = cov_3_out) # ïðåäåëüíûé ýôôåêò äëÿ 3é ñïåöèôèêàöèè
margin3
margin3 %>%
  ggplot()+
  geom_point(aes(is_ex, AME))+
  geom_errorbar(aes(x=is_ex, ymin=lower, ymax = upper))+
  geom_hline(yintercept = 0, col = 'red', linetype = 'dashed')+
  xlab('Ýêñòðåìàëüíûé ðåãèîí')+
  ylab('Ïðåäåëüíûé ýôôåêò èíäåêñà äîñòóïíîñòè æèëüÿ') # ãðàôèê ïðåäåëüíîãî ýôôåêòà äëÿ 3é ñïåöèôèêàöèè

margin4 <- reg_4_out %>% margins_summary(variables = 'Index_ves_19',
                                         at = list(is_ex = c(0, 1)),
                                         vcov = cov_4_out) # ïðåäåëüíûé ýôôåêò äëÿ 4é ñïåöèôèêàöèè
margin4
margin4 %>%
  ggplot()+
  geom_point(aes(is_ex, AME))+
  geom_errorbar(aes(x=is_ex, ymin=lower, ymax = upper))+
  geom_hline(yintercept = 0, col = 'red', linetype = 'dashed')+
  xlab('Ýêñòðåìàëüíûé ðåãèîí')+
  ylab('Ïðåäåëüíûé ýôôåêò èíäåêñà äîñòóïíîñòè æèëüÿ') # ãðàôèê ïðåäåëüíîãî ýôôåêòà äëÿ 4é ñïåöèôèêàöèè


# 2) Äëÿ ïîëíîé âûáîðêè


margin1_all <- reg_1_all %>% margins_summary(variables = 'Index_ves_19',
                                         at = list(is_ex = c(0, 1)),
                                         vcov = cov_1_out) # ïðåäåëüíûé ýôôåêò äëÿ 1é ñïåöèôèêàöèè
margin1_all
margin1_all %>%
  ggplot()+
  geom_point(aes(is_ex, AME))+
  geom_errorbar(aes(x=is_ex, ymin=lower, ymax = upper))+
  geom_hline(yintercept = 0, col = 'red', linetype = 'dashed')+
  xlab('Ýêñòðåìàëüíûé ðåãèîí')+
  ylab('Ïðåäåëüíûé ýôôåêò èíäåêñà äîñòóïíîñòè æèëüÿ') # ãðàôèê ïðåäåëüíîãî ýôôåêòà äëÿ 1é ñïåöèôèêàöèè

margin2_all <- reg_2_all %>% margins_summary(variables = 'Index_ves_19',
                                         at = list(is_ex = c(0, 1)),
                                         vcov = cov_2_out) # ïðåäåëüíûé ýôôåêò äëÿ 2é ñïåöèôèêàöèè
margin2_all
margin2_all %>%
  ggplot()+
  geom_point(aes(is_ex, AME))+
  geom_errorbar(aes(x=is_ex, ymin=lower, ymax = upper))+
  geom_hline(yintercept = 0, col = 'red', linetype = 'dashed')+
  xlab('Ýêñòðåìàëüíûé ðåãèîí')+
  ylab('Ïðåäåëüíûé ýôôåêò èíäåêñà äîñòóïíîñòè æèëüÿ') # ãðàôèê ïðåäåëüíîãî ýôôåêòà äëÿ 2é ñïåöèôèêàöèè

margin3_all <- reg_3_all %>% margins_summary(variables = 'Index_ves_19',
                                         at = list(is_ex = c(0, 1)),
                                         vcov = cov_3_out) # ïðåäåëüíûé ýôôåêò äëÿ 3é ñïåöèôèêàöèè
margin3_all
margin3_all %>%
  ggplot()+
  geom_point(aes(is_ex, AME))+
  geom_errorbar(aes(x=is_ex, ymin=lower, ymax = upper))+
  geom_hline(yintercept = 0, col = 'red', linetype = 'dashed')+
  xlab('Ýêñòðåìàëüíûé ðåãèîí')+
  ylab('Ïðåäåëüíûé ýôôåêò èíäåêñà äîñòóïíîñòè æèëüÿ') # ãðàôèê ïðåäåëüíîãî ýôôåêòà äëÿ 3é ñïåöèôèêàöèè

margin4_all <- reg_4_all %>% margins_summary(variables = 'Index_ves_19',
                                         at = list(is_ex = c(0, 1)),
                                         vcov = cov_4_out) # ïðåäåëüíûé ýôôåêò äëÿ 4é ñïåöèôèêàöèè
margin4_all
margin4_all %>%
  ggplot()+
  geom_point(aes(is_ex, AME))+
  geom_errorbar(aes(x=is_ex, ymin=lower, ymax = upper))+
  geom_hline(yintercept = 0, col = 'red', linetype = 'dashed')+
  xlab('Ýêñòðåìàëüíûé ðåãèîí')+
  ylab('Ïðåäåëüíûé ýôôåêò èíäåêñà äîñòóïíîñòè æèëüÿ') # ãðàôèê ïðåäåëüíîãî ýôôåêòà äëÿ 4é ñïåöèôèêàöèè
