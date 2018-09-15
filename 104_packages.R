#setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RBasics")

#    Управляющие конструкции и пакеты 

#if (<condition>) (<do something>) else (<do another thing>)
# условие может быть посчитано на лету, TRUE или FALSE, не NA длины 1
# переносить else на новую строку нельзя!!!!!

if (c(TRUE, FALSE)) {
  print("Hmmm...")
}

# Проверка условия на целом векторе

ifelse(runif(8) > 0.5, "Орел", "Решка")
# runif - дает случайное число, равномерно распределенное на интервале (0, 1)

x <- runif(8)
ifelse(x > 2/3, "Камень", 
       ifelse(x > 1/3, "Ножницы", "Бумага"))
# для нескольких условий ветвим внутри

switch("product", #сюда пишем условие
       sum = 5 + 5, #сюда - действие, если условие равно строке "sum"
       product = 5 * 5, # -//- product
       factorial = factorial(5), # -//- factorial
       0) # другие значения
# аналог case в bash


#    Циклы

# repeat - всегда будет выполнен хотя бы один раз
i <- 0
repeat {
  i <- i + runif(1)
  print(i)
  if (i > 5) break
}

# while - может быть не выполнен ни разу
i <- 2^14
while (i > 1000) {
  i <- i/2
  print(i)
}

# for
for (i in 1:8) {
  if (i %% 2 == 0) print(i)
}

for (i in letters) {
  if (i == "b") next
  if (i == "d") break
  print(i)
}

for (i in 1:5) i #обязательно вызывать внутри цикла print

#  for против векторизации
v <- 1:1e5
system.time({
  x <- 0 #не указана длина результата - замедляется
  for (i in v) x[i] <- sqrt(v[i]) #цикл гораздо медленнее, чем функции
})

system.time({
  y <- sqrt(v)
})

identical(x, y)


#    Задача 2

set.seed(1337)
x <- runif(1e6, min = -1, max = 1)
length(x[x > -0.2 & x < 0.3])


#    Задача 3

floor(runif(100, min = 1, max = 7))

dice_roll <- function(n) {
  x <- runif(n, min = 1, max = 7)
  return(floor(x))
}

n3 <- dice_roll(1e4)


#    Пакеты, необходимые для работы - я люблю свою девушку, она самая лучшая! Это правда)

#модули расширений - это пакеты, а библиотеки - это место на диске, где хранятся доступные пакеты

.libPaths()
# CRAN: https://cran.r-project.org/
# github, bitbucket

?installed.packages()

?grid.newpage()
library(grid)
?grid.newpage
grid.newpage()
grid.lines()

# library(pkgname) - если пакета нет в системе, возвращена ошибка
# require(ggplot2) - если пакета нет в системе, возвращает FALSE

# установка пакета
install.packages("xts", dependencies = TRUE)
update.packages()
library(xts)

sessionInfo()
# дает информацию о сессии

install.packages("randtoolbox")
library(randtoolbox)
