#setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RBasics")


#    Задача 1 - создать 3 вектора
 
?seq
seq(1, 100); seq(100, 1)
seq(0, 1, by = 0.01)
rep(seq(1, 3), 100)

vec1 <- c(seq(1, 100), seq(99, 1))
vec2 <- seq(0, 1, by = 0.01)
vec3 <- rep(seq(1, 3), 100)


#    Задача 2 - создать вектор через paste0

?paste0
my_vector <- sapply(seq(100, 1), function(x) paste0("element", x))
paste0("element", seq(100, 1))


#    Задача 3 - создать вектор через sapply

my_vector <- sapply(seq(1, 100), function(x) x^2*cos(x))
head(my_vector)


#    Задача 4 - создать вектор через tapply

?mapply
task4 <- seq(100, 1)
my_vector <- sapply(seq(1, 100), function(x) x^task4[x])
max_positions <- order(-my_vector)[1:10]


#    Задача 5 - расчитать сумму

my_sum <- sum(sapply(seq(1, 200), function(i) i^2+2*sqrt(i)))


#    Задача 6 - написать функцию изменить переменную в матрице

task6 <- matrix(c(seq(1:6), seq(1, 3)), ncol = 3)
task6[, ncol(task6)] <- apply(task6[, -ncol(task6)], 1, prod)

mutate_matrix <- function(m) {
  m[, ncol(m)] <- apply(m[, -ncol(m)], 1, prod)
  m
}

mutate_matrix(task6)


#    Задача 7 - создать матрицу

?"+"
5%/%2

create_matrix <- function(n) {
  if (n %% 2 == 0) {
    matrix(rep(c(seq(1, n), seq(n, 1)), n/2), ncol = n)
  } else {
    matrix(c(rep(c(seq(1, n), seq(n, 1)), n%/%2), seq(1, n)), ncol = n)
  }
}


#    Задача 8 - для каждого столбца расчитать среднее значение, вывести вектор с номерами
# колонок, в которых есть максимальные значения

set.seed(42)
mat <- matrix(rnorm(15), ncol = 3)
mat <- matrix(c(rep(seq(1, 3), 2), seq(0, 1, len = 3)), ncol = 3)
which(apply(mat, 2, mean) == max(apply(mat, 2, mean)))


max_mean_col <- function(m) {
  which(apply(m, 2, mean) == max(apply(mat, 2, mean)))
}


#    Задача 9 - для каждого элемента в первом векторе расчитать, сколько раз он 
# встречается во втором векторе. Если в первом векторе несколько значений с одинаковой
# частотой, вернуть все эти значения

t91 <- c(1, 2, 3, 4)
t92 <-  c(1, 1, 1, 2, 2, 2, 3, 3, 4, 4)

sum(v2 == v1[1])
task9 <- sapply(v1, function(x) sum(v2 == x))
v1[which(task9 == max(task9))]

get_max_element <- function(v1, v2) {
  t9 <- sapply(v1, function(x) sum(v2 == x))
  v1[which(t9 == max(t9))]
}

get_max_element(t91, t92)


#    Задача 10 - разбиваем строку на биграммы

?nchar
?substr

task10 <- "lesson"
substr(task10, 1, 2)
sapply(seq(1, nchar(task10) - 1), function(x) substr(task10, x, x+1))

cut_string <- function(x) {
  sapply(seq(1, nchar(x) - 1), function(i) substr(x, i, i+1))
}

cut_string("programm")


#    Задача 13 - змейковая матрица от числа n в квадрате

n = 4
task13 <- seq(1, n^2)
?rev
rev(task13[5:8])
task13[(n+1) : (2*n)]
?c
matrix(c(task13[1:n], rev(task13[(n+1) : (2*n)]), task13[(2*n+1) : (3*n)], rev(task13[(3*n+1) : (4*n)])), ncol = n)
c(seq(n+1, 2*n), seq(n+1, 2*n) + n)

n = 5
task13 <- matrix(seq(1, n^2), ncol = n)
apply(task13[, seq(2, n, by = 2)], 2, rev)

snake_matrix <- function(n) {
  if (n == 1) {
    return(matrix(n))
  } else if (n == 2) {
    return(matrix(c(1, 2, 3, 4), ncol = 2))
  } else if (n == 3) {
    return(matrix(c(seq(1, 3), seq(6, 4), seq(7, 9)), ncol = 3))
  } else {
    t13 <- matrix(seq(1, n^2), ncol = n)
    t13[, seq(2, n, by = 2)] <- apply(t13[, seq(2, n, by = 2)], 2, rev)
    return(t13)
  }
}

snake_matrix(3)
snake_matrix(2)
snake_matrix(1)
snake_matrix(6)


#    Задача 11 - отобрать из каждой группы два значения с минимальным V1

d <- read.csv("https://stepik.org/media/attachments/course/724/data.csv")
library(dplyr)
d <- as_data_frame(d)
d %>% 
  group_by(V3, V4) %>% 
  arrange(V1) %>% 
  slice(1:2)

library(dplyr)
get_min_cases <- function(df) {
  df %>% 
    group_by(V3, V4) %>% 
    arrange(V1) %>% 
    slice(1:2)
}

get_min_cases(d)


#    Задача 12 - сгруппировать данные, вывести статистику для групп:
# среднее, стандартное отклонение, максимум, минимум

d <- read.csv("https://stepik.org/media/attachments/course/724/data.csv")
d %>% 
  group_by(V3, V4) %>% 
  summarise(mean = mean(V1),
            sd = sd(V1),
            max = max(V1),
            min = min(V1))

aggregate_data <- function(df) {
  df %>% 
    group_by(V3, V4) %>% 
    summarise(mean = mean(V1),
              sd = sd(V1),
              max = max(V1),
              min = min(V1))
}

aggregate_data(d)
