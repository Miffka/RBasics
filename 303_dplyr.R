#setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RBasics")
library(tidyr)
library(dplyr)
library(stringr)

# Предобработка данных пакетами tidyr dplyr
# Концепция tidy data
#В статистике форма записи табличных данных:
#столбец - переменная, строка - наблюдение
# Это очень удобно для датафрейма в R
# Но не во всех областях анализа данных это так
# Hadley Wickham: tidyr, dplyr, ggplot2, ggvis


# Пакет tidyr
set.seed(1122)
df <- data.frame(Name = c("John", "Peter", "Mary", "Caroline"),
                 DrugA_T1 = runif(4, 35, 36),
                 DrugA_T2 = runif(4, 36, 39),
                 DrugB_T1 = runif(4, 36, 36.6),
                 DrugB_T2 = runif(4, 37, 38.5)
                 ); df
?runif
# Связка gather-spread
# wide to long data
gather(df, Variable, Temperature, -Name)
?gather
?spread

# Связка separate-unite
df1 <- gather(df, Variable, Temperature, -Name); df1
df2 <- separate(df1, Variable, c("DrugType", "Time"), "_")
df2 #вот то, что мы получили сейчас - это tidy data


# Пакет dplyr
# Функция select
select(df2, Time, Temperature) #указываем те столбцы, которые нужно оставить

select(df2, 3:4)
select(df2, starts_with("T")) #выделение при помощи регулярок, см. тж. contains
select(df2, -Name, -DrugType)
df2[, 3:4] 

# Функция filter
# Отличие от select - тот действует по колонкам, filter действует по наблюдениям
filter(df2, Temperature > 37, Name %in% c("John", "Mary"))
# Функции filter и select - тот же subset

# Функция arrange - сортирует датафрейм
arrange(df2, Name, -Temperature) 
#сначала - по имени по возрастанию, потом - по температуре по убыванию

# Функция mutate - создает переменные
mutate(df2, DrugType = gsub("Drug", "", DrugType))

# Функции group_by и summarise
summarize(group_by(df2, Time), AvgTemp = mean(Temperature))

# эти два пакета написаны, в целом, очень хорошо для того, чтобы эффективно работать с данными
?data.table

#Cheat sheet tidyr and dplyr
# https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
#Современный взгляд на предобработку данных: dplyr, tidyr, magritt
# https://github.com/tonytonov/spbr-1-dataproc


#    Задача 2 - выбираем столбцы при помощи dplyr
help("%>%")
df2 %>% select(c(1, 4))
select(df2, -3:4) #не работает
select(df2, matches("T{4,5}$"))
?matches
select(df2, contains("Name"), Time)


# Конвейеры - оператор %>%
df <- data.frame(type = c(1, 1, 2, 2, 3, 3), value = c(5, 10, 50, 100, 7, 7))
arrange(
  summarize(
    group_by(df, type),
    Total = sum(value)
  ),
  -Total #аргумент arrange, но он находится далеко!
)

#Можно написать то же самое в виде нескольких строк
a <- group_by(df, type)
b <- summarize(a, Total = sum(value))
c <- arrange(b, -Total)

# Конвейерная форма записи
df %>%
  group_by(type) %>%
  summarize(Total = sum(value)) %>%
  arrange(-Total)

# Строгая запись конвейера
# x %>% f == f(x)
# x %>% f(y) == f(x, y) # порядок аргументов - сначала входит аргумент x
# x %>% f(y, param = .) == f(y, param = x) # в этом случае сначала входит аргумент y


#    Задача 3 - ищем функции в dplyr
?transmute
?sample_n
?inner_join
?transform


# Применяем функции dplyr на avianhabitat
options(stringsAsFactors = F)
avian <- read.csv("https://raw.githubusercontent.com/tonytonov/Rcourse/master/R%20programming/avianHabitat.csv")

# Первый подход
avian <- subset(avian, PDB > 0 & DBHt > 0, c("Site", "Observer", "PDB", "DBHt"))
avian$Site <- factor(str_replace(avian$Site, "[:digit:]+", ""))
subset(
  aggregate(avian$DBHt, list(Site = avian$Site, Observer = avian$Observer), max),
  x >= 5
)

# Второй подход - конвейеры
avian <- read.csv("https://raw.githubusercontent.com/tonytonov/Rcourse/master/R%20programming/avianHabitat.csv")
avian <- 
  avian %>%
  subset(PDB > 0 & DBHt > 0, c("Site", "Observer", "PDB", "DBHt")) %>%
  transform(Site = factor(str_replace(.$Site, "[:digit:]+", ""))) 
# использовали точку вместо результата предыдущего вычисления

aggregate(avian$DBHt, list(Site = avian$Site, Observer = avian$Observer), max) %>%
  subset(x >= 5)

# Третий подход - используем и dplyr, и конвейеры
avian <- read.csv("avianHabitat.csv")

avian %>%
  filter(PDB > 0, DBHt > 0) %>% #фильтруем наблюдения
  select(Site, Observer, contains("DB")) %>% #фильтруем переменные
  mutate(Site = factor(str_replace(Site, "[:digit:]+", ""))) %>% #заменяем переменную
  group_by(Site, Observer) %>% #группируем 
  summarise(MaxHt = max(DBHt)) %>% #выводим какую-то статистику
  filter(MaxHt >= 5) #фильтруем еще раз


#    Задача 7 - анализируем avianHabitat
avian <- read.csv("avianHabitat.csv")
head(avian)
avian %>%
  select(Site, Observer, contains("Ht")) %>%
  mutate(Site = factor(str_replace(Site, "[:digit:]+", ""))) %>%
  group_by(Site, Observer) %>%
  summarise_each(funs(sum(.>0)))

?summarise_each
#summarise_each_(avian, max)
?mutate
str(avian$WHt)
?select
?filter
