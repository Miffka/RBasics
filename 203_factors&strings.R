#setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RBasics")
library(stringr)

#  Строки
#В реальных данных бывают данные строкового типа, имена переменных, файлы и директории, факторы
# character - тип вектора, string - единичный элемент вектора character
# Большинство функций для строк векторизованы
s <- c("Терпение и труд все перетрут",
       "Кончил дело - гуляй смело",
       "Без труда не вытащишь и рыбку из пруда",
       "Работа не волк, в лес не убежит")
# Как правило, используются двойные кавычки, одинарные тоже можно использовать
'Операция "Ы"' # \" 

# paste/paste0
#Выполняют конкатенацию с учетом правил переписывания
#sep - разделитель между элементами (paste - пробел, paste0 - пустая строка)
#collapse - "схлопывает" вектор в одну строку
paste(c("углекислый", "веселящий"), "газ")

# strsplit
#Выполняет разбиение строкового вектора
#Результат - список
strsplit(s, " и ", fixed = T)
#аргумент fixed - если false, строка разбиения рассматривается как regexp
strsplit(s, "[[:punct:]]")

# Регулярные выражения
#Метаязык поиска и манипуляции подстрок
#Образец для поиска может включать обычные символы и wildcards
grep("труд", s) #указывает на элементы, которые содержат нужное
grepl("труд", s) #дает логический вектор, который делает то же самое
gsub("\\b[[:alpha:]]{4,5}\\b", "####", s)

# Пакет stringr
?stringr
#Содержит множество функций для работы со строками. Функции векторизованы и последовательны
str_extract(s, "н.")
str_replace(s, "[на]", "?")
str_extract_all(s, "н.")
str_replace_all(s, "[иае]", "?")

# Функции tolower/toupper
tolower(month.name)
toupper(month.abb)


#    Задача 1 - функция, которая возвращает длину строки
length("Аэрофотосъёмка ландшафта уже выявила земли богачей и процветающих крестьян.")
??"length"
?nchar
nchar("Аэрофотосъёмка ландшафта уже выявила земли богачей и процветающих крестьян.")
# говорят, лучше использовать stringr::str_length
stringr::str_length(NA)


#    Задача 2 - применяем всякое к строкам
?strsplit


#  Пути к файлам
getwd()
head(list.files()) #можно просмотреть файлы
list.dirs("..", recursive = F) #список директорий на уровень выше
#setwd() # меняет путь к директории

# Форматирование чисел
c(pi, exp(pi))
formatC(c(pi, exp(pi)), digits = 3) # общее количество знаков - 3
formatC(c(pi, exp(pi)), digits = 3, format = "e") # научный формат

# Функция cat
#Вывод на печать объектов "как они есть"
cat('Операция \"Ы\"'); print('Операция \"Ы\"')
#Функция распознает эскейп-последовательности - переход на новую строку и табуляцию
cat("Трус\tБалбес\nБывалый")


#    Задача 3 - работа с регулярными выражениями

hamlet <- "To be, or not to be: that is the question: 
  Whether 'tis nobler in the mind to suffer 
  The slings and arrows of outrageous fortune, 
  Or to take arms against a sea of troubles, 
  And by opposing end them?"

hamlet <- str_replace_all(hamlet, "[:punct:]", "")
hamlet <- tolower(unlist(str_split(hamlet, "[:space:]")))

?grep
sum(grepl("to", hamlet, fixed = T)) #количество слов "to"
sum(grepl("[fqw]", hamlet)) #количество слов, содержащих любую букву [fqw]
sum(grepl("b.+", hamlet)) #количество слов, содержащих букву b, после которой - любой другой символ
sum(grepl("\\<[[:alpha:]]{7}\\>", hamlet)) #количество слов длины 7
?regexpr


#    Задача 4 - проверяем функции
?strsplit
?str_detect
?str_split
?gsub
?str_replace


#    Факторы
# В статистике есть переменные количественные и качественные
# Для качественных и есть factor
# Фактор - это гибрид целочисленного и строкового вектора
set.seed(1337)
f <- factor(sample(LETTERS, 30, replace = T)); f

# Уровни фактора
as.numeric(f) #получаем числа от 1 до 17 по уровням градации фактора
as.character(f) #получаем строки - уровни
levels(f) #получаем все уровни - уникальные строки
nlevels(f) #то же самое, что length(levels(f))

# Индексирование фактора
f[f == "A"] <- "Z"; f #заменяем, но в уровнях переменная осталась
(f <- droplevels(f)) #выкидываем все пустые градации фактора
#заключая результат присвоения в круглые скобки, мы выводим переменную на печать

# Преобразование уровней фактора
levels(f) <- tolower(levels(f)); f
levels(f) <- letters[letters %in% levels(f)]; f
levels(f)[1] <- "bbb"; f

# Упорядоченные факторы
#ordered или ordered = T
temp <- c("freezing cold", "cold", "comfortable", "hot", "burning hot")
ft <- ordered(sample(temp, 14, replace = T), temp); ft
ft[ft >= "hot"]
which(ft >= levels(ft)[3])

# Преобразование количественной переменной в качественную
# сut делит вектор на интервалы
# table считает количество вхождений из категорий
cut(rnorm(10), -5:5)
table(cut(rnorm(1000), -5:5))

# Опции сессии
?options
options(stringsAsFactors = FALSE) #Опция очень важна при загрузке файла
options(digits = 5, width = 100)

# tapply
#Подсчет статистики по группам
str(warpbreaks)
?warpbreaks
tapply(warpbreaks$breaks, list(warpbreaks$wool, warpbreaks$tension), max)
?tapply
default.stringsAsFactors()


#    Задача 6 - делаем из numeric factor
str(quakes)
summary(quakes$mag)
?cut
?seq
seq(4, 6.5, by = 0.5)
table(cut(quakes$mag, ordered_result = T, breaks = seq(4, 6.5, by = 0.5), right = F))


# Возвращаемся к массиву данных avianHabitat - что-то делаем

avian <- read.csv("https://raw.githubusercontent.com/tonytonov/Rcourse/master/R%20programming/avianHabitat.csv")
str(avian)
?source

# Ищем csv и читаем низкоуровневой функцией заголовок
list.files()
list.files(pattern = ".*\\.csv")
readLines("avianHabitat.csv", 5)
avian <- read.table("avianHabitat.csv", header = T, sep = ",", dec = ".")
head(avian)

# Хотим сделать одну из переменных просто character
options(stringsAsFactors = F)
avian$Observer <- as.factor(avian$Observer)

# Еще раз получаем имена нужных переменных при помощи регэкспов
(coverage_vars <- names(avian)[str_detect(names(avian), "^P")])
check_percent_range <- function(x) {
  any(x < 0 | x > 100)
}
# вот это - хороший пример функции)
sapply(coverage_vars, function(name) check_percent_range(avian[[name]]))

# Удаляем цифру из названия локации
unique(avian$Site)
avian$site_name <- factor(str_replace(avian$Site, "[:digit:]+", ""))
?colSums
?aggregate
tapply(avian$DBHt, avian$site_name, mean)
str(avian)


#    Задача 7 - общее покрытие минимальное
avian$totcov <- rowSums(avian[, coverage_vars])
summary(avian$totcov)
tapply(avian$totcov, avian$site_name, mean)


#    Задача 8 - кто нашел какие наибольшие экземпляры

hei_vars <- names(avian)[str_detect(names(avian), "Ht$")]
avian[, hei_vars]
?tapply
avian[which(avian$DBHt == max(avian$DBHt)), ]$Observer
# sapply(hei_vars, function(x) avian[which(avian$x == max(avian$x)), ]$Observer)
hei_ind <- which(names(avian) %in% hei_vars)
all.equal(avian[, hei_ind[1]], avian[, 6])
avian[which(avian[, 6] == max(avian[, 6])), ]$Observer

#мое прекрасное решение 
sapply(hei_ind, function(x) 
  c(names(avian)[x], as.character(avian[which(avian[, x] == max(avian[, x])), ]$Observer)))

#Вот этот код дает максимумы каждого вида для каждого собирателя
sapply(hei_ind, function(x)
  tapply(avian[, x], avian$Observer, max))
