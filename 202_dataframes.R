#setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RBasics")
library(dplyr)

# Датафреймы!!!
# двумерная таблица с данными
# стандартный способ хранения данных в формате наблюдения/переменные
# строки - наблюдения, столбцы - переменные
# датафреймы наследуют свойства матрицы и списка (переменные могут быть разных типов)

# Создание датафрейма
df <- data.frame(x = 1:4, y = LETTERS[1:4], z = c(T, F))
str(df)

# Имена
df <- data.frame(x = 1:4, y = LETTERS[1:4], z = c(T, F), row.names = c("Alpha", "Bravo", "Charlie", "Delta"))
str(df)
df
rownames(df); colnames(df); dimnames(df)

# Размерности
nrow(df); ncol(df); dim(df)
# Особенности
length(df) #возвращает количество столбцов (переменных)
names(df) #возвращает имена столбцов
# этих функций стоит избегать

# Индексация
#как для матрицы
df[3:4, -1] #по элементам
df[c(F, T), c("z", "x")] #логическая индексация и по именам
#ВНИМАНИЕ - происходит схлопывание размерности
df[, 1]; df[, 1, drop = F]

#как для списка
df$z; df[[3]]; df[["z"]]

#как для матриц - фильтрация по условию
df[df$x > 2,]

#функция subset
subset(df, x > 2) #можно не дублировать название датафрейма и не обращаться к переменной по имени
subset(df, x > 2, select = c(x, z)) #можем выбирать нужные столбцы по имени

# Комбинирование
rbind(df, data.frame(x = 5:6, y = c("K", "Z"), z = T, row.names = c("Kappa", "Zulu")))
# необходимо, чтобы имена у двух датафреймов совпадали в точности
cbind(df, data.frame(season = c("Summer", "Autumn", "Winter", "Spring"), temp = c(20, 5, -10, 5)))
# необходимо, чтобы длина столбцов совпадала в точности
df
df_salary <- data.frame(x = c(3, 2, 6, 1), salary = c(100, 1000, 300, 500))
#объединение по ключу x
merge(df, df_salary, by = "x")
#результат действия - все полные записи из обоих записей, которая определяется ключом x
# объединение разными способами - найти по запросу "r join"


#    Задача 2 - делаем из матрицы датафрейм
typeof(as.matrix(df))


#    Задача 3 - анализ данных attitude
str(attitude)
sort(attitude$learning, decr = T)
?arrange
task3 <- arrange(attitude, desc(learning))[1:5, ]
task3$task <- apply(task3, 1, function(x) sum(x[c(2, 5, 7)]))
task31 <- task3[task3$task == max(task3$task),]$learning
?which
rownames(attitude[attitude$learning == task31,])


# Примеры работы с данными

# Импорт данных

# Из файла
#csv or tab separated values
#readlines, scan - чтение неструктурированного текста
#xml, html - library(XML), library(httr)
#json, yaml - library(rjson), library(readxl)
#Excel - library(XLConnect), library(readxl)
#SAS, Stats, SPSS, MATLAB - library(foreign), library(sas7bdat)
# Из web - library(rvest)
# Из баз данных
#реляционные - library(DBI), library(RSQLite)
#нереляционные - library(rmongodb)

# Чтение табличных данных
#read.table
#file - имя файла
#header - наличие или отсутствие заголовка в первой строке
#sep - разделитель значений
#quote - символы, обозначающие кавычки (для строк)
#na.strings - строки, кодирующие пропущенное значение
#colClasses - типы столбцов (для быстродействия и указания типа - строка-фактор-дата/время)
#comment.char - символ, обозначающий комментарии
#skip - количество строк, пропускаемых с начала файла
# Функции read.csv, read.csv2, read.delim, read.delim2 - это тот же read.table с нужными умолчаниями

# Типичные шаги предобработки данных
#импорт в датафрейм
#очистка значений, проверка типов
#работа со строками - имена, переменные строкового типа, факторы
#пропущенные значения - идентификация, способ обработки
#манипулирование переменными - преобразование, создание, удаление
#подсчет описательных статистик - split-apply-combine
#визуализация данных
#экспорт

?split
?combine

# Очистка значение, проверка типов
#Типы переменных, на которых легко ошибиться при импорте
#числовые переменные становятся строковыми
# пропущенные значения отмечены не как NA (na.strings = c("NA", "Not Available", etc.))
# из-за неверно указанных разделителя, десятичного знака (sep = ",", dec = ".")
# из-за кавычек, сопроводительного текста или комментариев
#Строковые типы становятся факторами либо наоборот
# as.character, as.factor
#Тип дата/время остается строковым as.POSIXct, as.POSIXlt, as.Date
#  Функции str, summary, head и tail помогут определить, все ли в порядке

# Работа с переменными
#Удаление наблюдений с пропущенными значениями
# df[complete.cases(df),] 
# na.omit(df)
#Замена NA на некоторые значения может быть потенцильно опасным
# замена средним может вносить смещение в данные
# заполнение нулями в большинстве случаев некорректно в принципе!
#Создание, изменение, удаление переменных выполняется конструкциями
# df$new_var <- <...>
# df$old_var <- f(df$old_var)
# df$old_var <- NULL (удаляем переменную)
#Работа с большим количеством переменных
?within

# Экспорт
#write.table, write.csv, write.csv2
#Если массив большой, лучше отделять этап предобработки данных
# отдельным файлом .R - скрипт очистки и начальный файл
# отдельным файлом с предобработанными ("чистыми") данными

# Массив данных
# http://alaska.usgs.gov/products/data.php&dataid=5
# https://github.com/tonytonov/Rcourse/blob/master/avianHabitat.csv
#Датасет - растительность в местах обитания охраняемых видов птиц


#    Задача 4 - выбор корректных строк
attitude[attitude$rating < 50, names(attitude) != "rating"] #корректно
attitude[rating < 50, names(attitude) != "rating"] #не работает
subset(sel = -rating, sub = rating <50, attitude) #корректно
subset(attitude, rating < 50, -rating) #корректно
attitude[attitude$rating < 50, -"rating"] #не работает


#    Задача 5 - визуальная инспекция данных quakes

?quakes
str(quakes)
?median
sapply(quakes, function(x) c(median(x), mean(x), max(x), min(x)))
View(quakes)
quakes[nrow(quakes)-1,]


# Работаем с данными
avian <- read.csv("https://raw.githubusercontent.com/tonytonov/Rcourse/master/R%20programming/avianHabitat.csv")
str(avian)

#Проверка данных
summary(avian) #здесь можем заметить всякие косяки у данных
any(!complete.cases(avian)) #ищем пропуски
any(avian$PDB < 0) #вот так ищем значения не в диапазоне
any(avian$PDB > 100)
# пишем функцию для проверки любого вектора
check_percent_range <- function(x) {
  any(x < 0 | x > 100)
}
check_percent_range(avian$PW)

#Трансформация переменных
names(avian)
coverage_variables <- names(avian)[-(1:4)][c(T, F)]
coverage_variables # эта переменная содержит все имена процентных переменных
avian$total_cov <- rowSums(avian[, coverage_variables])
summary(avian$total_cov)


#    Задача 6 - добавление данных

task6 <- read.csv("/media/glycosylase/EC6A2F256A2EEBD0/Users/miffka/Documents/!DataMining/RBasics/202_task6.csv",
                  sep = ";", dec = ".", na.strings = "Don't remember", comment.char = "%")
?read.csv
str(task6)

task6$Observer <- factor(c("KL"), levels = c("JT", "RA", "RR", "KL"))
str(task6)

task60 <- rbind(avian, task6)
str(task60)
summary(task60)

coverage_variables <- names(task60)[-(1:4)][c(T, F)]
task60$totcov <- rowSums(task60[, coverage_variables])
summary(task60$totcov)


#    Задача 7 - сортировка растений по высоте (максимальной высоте)
avian <- read.csv("https://raw.githubusercontent.com/tonytonov/Rcourse/master/R%20programming/avianHabitat.csv")
str(avian)
heigth_var <- names(avian)[-(1:5)][c(T, F)]
avian[, heigth_var]
ans7 <- sapply(avian[, heigth_var], max)
sort(ans7)
