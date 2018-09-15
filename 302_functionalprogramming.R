#setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RBasics")
library(stringi)
library(microbenchmark)

# Элементы функционального программирования
#  Объектно-ориентированные системы в R (т.е. классы, поля классов и методы этих классов)
#S3 - 
# нет формальной декларации класса,
# функция может иметь разное поведение в зависимости от класса (method dispatch)
# такие функции называются generic
#S4 - 
# строгое определение класса и его полей
# больше возможностей для поведения методов
#Reference classes
#  Про остальные возможности читаем "Advanced R"

# Generic функции
length(methods(print))
# 191 метод!!!
# если х - датафрейм, вызывается print.data.frame
# если х - функция, вызывается print.function и т.п.

# Функции без сторонних эффектов
#В R нет указателей на объекты, все объекты передаются "по значению" (есть нюансы!)
#При попытке изменить переданный объект заводится его копия в локальном окружении (copy-on-modify semantics)
f <- function(k) {
  k <- k + 1
  a <- a + k^2
  a
}
k <- 5
f(k)
a <- 10
c(f(k), k, a) #если R не находит a в локальном окружении, он идет в глобальное
# ни переменная k, ни переменная a не изменились глобально в ходе выполнения функции

# Функция replicate - вызывает функцию определенное количество раз
?replicate
get_status <- function(n, p = 0.1) {
  x <- rbinom(n, 1, p)
  sum(x)
}
replicate(5, get_status(100))

# Функция mapply
?mapply
mapply(seq, from = 1:4, to = 2:5, by = 1/(1 + 1:4))
?seq

# Функция outer
?outer
m <- outer(letters, LETTERS, paste0)
dim(m)
diag(m)

# Функция Vectorize - способ векторизовать невекторизованную функцию
lp_norm <- function(x, p = 2) {
  if (p >= 1) sum(abs(x)^p)^(1/p) else NaN
}
lp_norm(1:10, -1:4)
lp_norm <- Vectorize(lp_norm, "p")
lp_norm(1:10, -1:4)

# Функция do.call - вызов функции на списке аргументов
?do.call
df1 <- data.frame(id = 1:2, value = rnorm(2))
df2 <- data.frame(id = 3:4, value = runif(2))
df3 <- data.frame(id = 222, value = 7)
do.call(rbind, list(df1, df2, df3)) # rbind(df1, df2, df3)
# функция нужна, когда количество объектов неизвестно
# Например, считать все файлы в папке и связать в один датафрейм 
#do.call(rbind, lapply(list.files(), function(file) read.csv(file)))
?list.files


#    Задача 2 - ищем функцию семейства apply, которой нет в base
??dapply #{SparkR}
?rapply
?vapply
?eapply
?tapply


#    Задача 3 - клеим котиков
cat_temper <- c("задиристый", "игривый", "спокойный", "ленивый")
cat_color <- c("белый", "серый", "чёрный", "рыжий")
cat_age <- c("кот", "котёнок")
cat_trait <- c("с умными глазами", "с острыми когтями", "с длинными усами")
?paste
?outer
?expand.grid
task3 <- expand.grid(cat_temper, cat_color, cat_age, cat_trait, stringsAsFactors = F)
str(task3)
task31 <- as.vector(outer(outer(outer(cat_temper, cat_color, paste, sep = " "), 
            cat_age, paste, sep = " "),
      cat_trait, paste, sep = " "))
cat_catalogue <- stri_sort(task31)
?sort
head(cat_catalogue)
cat(cat_catalogue[42])


# Моделирование траектории случайного процесса
simulate_walk <- function(lower = -10, upper = 10, n_max = 200, p = 1e-3) {
  current_position <- (lower + upper) / 2
  for (i in 1:n_max) {
    is_absorbed <- rbinom(1, 1, p)
    if (is_absorbed) return(list(status = "absorbed",
                                 position = current_position,
                                 steps = i))
    current_position <- current_position + rnorm(1)
    if (current_position < lower) return(list(status = "Left breach", 
                                              position = current_position, 
                                              steps = i))
    if (current_position > upper) return(list(status = "Right breach", 
                                              position = current_position, 
                                              steps = i))

  }
  return(list(status = "Max steps reached", 
              position = current_position, 
              steps = n_max))  
}

result <- replicate(1000, simulate_walk(), simplify = F)
result <- data.frame(
  status = sapply(result, function(x) x$status),
  position = sapply(result, function(x) x$position),
  steps = sapply(result, function(x) x$steps)
) 

# Проверяем результаты
head(result)
tapply(result$position, result$status, length) 
tapply(result$steps, result$status, mean)


#    Задача 4 - задача simulate_walk на двумерном пространстве
# вероятность поглощения  - 0,01; выход - круг с радиусом 6
current_position <- c(x = 0, y = 0)
current_position <- current_position + c(rnorm(1), rnorm(1))
cur_radius <- sqrt(current_position[1]^2 + current_position[2]^2)
?sqrt

simulate_walk_d2 <- function(rmax = 6, n_max = 100, p = 1e-2) {
  current_position <- c(x = 0, y = 0)
  for (i in 1:n_max) {
    is_absorbed <- rbinom(1, 1, p)
    #проверка на поглощение
    if (is_absorbed) return(status = 1) #статус 1 - поглощена
    # если не поглощена - считаем текущую позицию и радиус
    current_position <- current_position + c(rnorm(1), rnorm(1))
    cur_radius <- sqrt(current_position[1]^2 + current_position[2]^2)
    # проверяем, не достигнута ли граница
    if (cur_radius > rmax) return(status = 2) #статус 2 - достигнута граница
  }
  return(status = 3) #статус 3 - максимум шагов
}

replicate(10, simulate_walk_d2())
replicate(100, simulate_walk_d2())
?system.time
system.time(result <- replicate(100000, simulate_walk_d2(), simplify = T))
table(result)


#    Задача 5 - найти, для каких классов определены функции
print.function
print.default
summary.matrix
summary.default
plot.function
plot.default


#    Задача 6 - выполнить код

f <- function(y) {
  y <- x + y
  y
}
g <- function(x) {
  y <- f(x)
  f <- function(x) {
    y - x
  }
  y - f(x)
}
x <- 10
y <- 1
f(x); f(y)
g(x); g(y)
x; y


#    Задача 7 - отсортировать функции по времени выполнения

?microbenchmark

m1 <- function(x, y) {
  m <- matrix(0, length(x), length(y))
  for (i in 1:length(x)) 
    for (j in 1:length(y)) {
      m[i, j] = x[i] * y[j]
    }
  m
}
m2 <- function(x, y) {
  vapply(y, function(i) i * x, numeric(length(x)))
}
m3 <- function(x, y) x %o% y

help("%o%") #дает быстрое умножение всех элементов одного вектора на все элементы другого вектора поочередно

x <- rnorm(100)
y <- runif(1000)
all.equal(m1(x, y), m2(x, y))
all.equal(m2(x, y), m3(x, y))

microbenchmark(m1(x, y), m2(x, y), m3(x, y))
