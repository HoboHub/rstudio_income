library(tidyverse)

View(adult)

#Вывод всех значений колонок и статистики
glimpse(adult)


#Отфильтруем людей пенсионного возраста
adult_age <- filter(adult, age >= 65 & sex == "Male" | age >= 60 & sex == "Female")
glimpse(adult_age)
View(adult_age)

#найдем процент пенсионеров от общего числа реципиентов
nrow(adult) #общее кол-во строк в датасете
nrow(adult_age) #кол-во строк - пенсионеры
pensioner_perc <- (nrow(adult_age) / nrow(adult)) * 100
pensioner_perc


#Сколько пенсионеров зарабатывают >50k $ в год
aged_income_counts <- adult_age %>% count(income) %>%
  mutate(Percentage = n / sum(n) * 100)

aged_income_counts

View(aged_income_counts)

# Создаем график столбцов пенсионеров
ggplot(aged_income_counts, aes(x = income, y = Percentage, fill = income)) +
  geom_col() +
  labs(x = "Доход", y = "Процент пенсионеров") +
  ggtitle("Распределение доходов") +
  theme_bw() +
  scale_fill_manual(values = c("#1b9e77", "#d95f02")) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 4, fontface = "bold")

#----------------------------------


#----?-------
#безработные (почему-то отрабатывают сколько-то ч в неделю)
adult_nojob <- filter(adult, workclass == "Never-worked")
View(adult_nojob)
#----------


#Статистика:
#сфера деятельности, 
#медианное время работы в неделю, 
#средний возраст сотрудника
adult %>%
  group_by(workclass) %>%
  summarise(median(hours.per.week), 
            mean(age))


#----------------------------------
#Сколько людей зарабатывает меньше и больше 50к долларов в год

# Создаем таблицу с количеством людей в каждой категории доходов и их процентным соотношением
income_counts <- adult %>% count(income) %>%
  mutate(Percentage = n / sum(n) * 100)

income_counts

# Создаем график столбцов
ggplot(income_counts, aes(x = income, y = Percentage, fill = income)) +
  geom_col() +
  labs(x = "Доход", y = "Процент людей") +
  ggtitle("Распределение доходов") +
  theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 4, fontface = "bold")

#-------------------------------------------------------
#Распределение доходов по возрасту


# Создаем функцию для определения возрастной группы
age_group <- function(x) {
  x <- as.integer(x)
  x <- abs(x)
  if (18 < x & x < 31) {
    return("19-30")
  } else if (30 < x & x < 41) {
    return("31-40")
  } else if (40 < x & x < 51) {
    return("41-50")
  } else if (50 < x & x < 61) {
    return("51-60")
  } else if (60 < x & x < 71) {
    return("61-70")
  }   else {
    return("Более 70")
  }
}
            
# Создаем столбец с возрастными группами на основе функции age_group
adult$age_group <- sapply(adult$age, age_group)

# Выводим график
ggplot(adult, aes(x = age_group, fill = income)) +
  geom_bar(position = "dodge") +
  theme_bw() +
  scale_fill_manual(values = c("#78C850", "#F08030")) +
  labs(title = "Доход разных возрастных категорий",
       x = "Возрастная группа",
       y = "Кол-во") +
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 14))




#---------------------------------------------------------
#Распределение доходов в зависимости от страны

#Убраны люди с неустановленной страной
adult_filtered_country <- subset(adult, native.country != "?")

# График: доход в зависимости от страны
ggplot(adult_filtered_country, aes(x = native.country, fill = income)) +
  geom_bar(position = "dodge") +
  theme_bw() +
  scale_fill_manual(values = c("#78C850", "#F08030")) +
  labs(title = "Доход разных стран",
       x = "Страны",
       y = "Кол-во") +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text(size = 5),
        legend.title = element_blank(),
        legend.text = element_text(size = 5))

#--------------------------------------------------------------
#Поиск % разницы между богатыми и бедными в разных странах

#Убраны люди с неустановленной страной и доходом
adult_filtered_country <- subset(adult, native.country != "?" & is.na(income) == FALSE)
#adult_filtered_country <- subset(adult, native.country != "?")

# Подсчет количества людей из каждой страны с доходом <=50K и >50K
country_summary <- adult_filtered_country %>%
  group_by(native.country, income) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = income, values_from = count, names_prefix = "count_", values_fill = list(count_0 = 0)) 


# Создание столбца с процентной разницей
country_summary <- country_summary %>%
  mutate(income_diff = ((`count_<=50K` - `count_>50K`) / (`count_<=50K` + `count_>50K`)))


# Печать новой таблицы
print(country_summary)
View(country_summary)


# Выводим график
ggplot(country_summary, aes(x = native.country, y = income_diff, fill="income_diff")) +
  geom_col() +
  labs(x = "Страны", y = "Отношение бедных к богатым в %") +
  ggtitle("Распределение доходов") +
  theme_bw() +
  scale_fill_manual(values = "#78C850") +
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 14))



#------------------------------------------------------------
#Распределение доходов в зависимости от сферы деятельности

#Создаем таблицу без неопределенной сферы деятельности
adult_filtered <- subset(adult, workclass != "?")

#View(adult_filtered)

# Выводим график
ggplot(adult_filtered, aes(x = workclass, fill = income)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#78C850", "#F08030")) +
  labs(title = "Доход в зависимости от сферы деятельности",
       x = "Сфера деятельности",
       y = "Кол-во") +
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 14))


#-----------------------------------------------------
#Распределение дохода в зависимости от образования

order_list <- c('Preschool', '1st-4th', '5th-6th', '7th-8th', '9th', '10th', '11th', '12th', 
                'HS-grad ', 'Some-college', 'Bachelors', 'Masters', 'Doctorate', 'Prof-school', 
                'Assoc-acdm', 'Assoc-voc')


#образование - кол-во человек
aged_edu <- adult %>% count(education) 
aged_edu
#

ggplot(adult, aes(x = education, fill = income)) + 
  geom_bar(position = "dodge") +
  labs(title = "Доход в зависимости от уровня образования",
       x = "Образование",
       y = "Кол-во") +
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 14)) +
  scale_x_discrete(limits=order_list)



#=====================================================
#Логистическая регрессия


ggplot(adult, aes(x = education.num, y = capital.gain, col=income)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


ggplot(adult, aes(x = education.num, y = capital.loss, col=income)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)





