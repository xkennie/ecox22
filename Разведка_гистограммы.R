glimpse(data_ecox)
summary(data_ecox)
data_ecox %>%
  ggplot(aes(x=brate_19)) +
  xlab("Уровень рождаемости, число родившихся на 1000 человек") + 
  ylab("") +
  ggtitle("Распределение переменной 'Уровень рождаемости'")+
  geom_histogram()
data_ecox %>% 
  ggplot(aes(x=med_wage_19)) + 
  xlab("Средняя зарплата, рублей в месяц") + 
  ylab("") + 
  ggtitle("Распределение переменной 'Средняя зарплата'")
  geom_histogram()
data_ecox %>%
  ggplot(aes(x=Index_ves_19)) +
  xlab("Индекс Сбербанка по всей недвижимости")+
  ylab("")+
  ggtitle("Распределение переменной 'Индекс Сбербанка по всей недвижимости'")+
  geom_histogram()
data_ecox %>%
  ggplot(aes(x=unemp_19)) +
  xlab("Безработица в регионе, %")+
  ylab("")+
  ggtitle("Распределение переменной 'Безработица в регионе'")+
  geom_histogram()
data_ecox %>%
  ggplot(aes(x=Index_vtor_19)) +
  xlab("Индекс Сбербанка по вторичной недвижимости")+
  ylab("")+
  ggtitle("Распределение переменной 'Индекс Сбербанка по вторичной недвижимости'")+
  geom_histogram()
data_ecox %>%
  ggplot(aes(x=sum_coef_birth_19)) +
  xlab("Суммарный коэффициент рождаемости, число родившихся на 1000 человек")+
  ylab("")+
  ggtitle("Распределение переменной 'Суммарный коэффициент рождаемости'")+
  geom_histogram()
data_ecox %>%
  ggplot(aes(x=primeRS_19)) +
  xlab("Средние цены на первичном рынке жилья, на конец года руб/кв.м")+
  ylab("")+
  ggtitle("Распределение переменной 'Средние цены на первичном рынке жилья'")+
  geom_histogram()
data_ecox %>%
  ggplot(aes(x=secRS_19)) +
  xlab("Средние цены на вторичном рынке жилья, на конец года руб/кв.м")+
  ylab("")+
  ggtitle("Распределение переменной 'Средние цены на вторичном рынке жилья'")+
  geom_histogram()