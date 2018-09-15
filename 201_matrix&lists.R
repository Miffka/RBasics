#setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RBasics")

#    Матрицы и списки

# матрица - двумерный массив данных одного типа, приведение как у векторов
# это вектор, уложенный по столбцам

matrix(1:6, nrow=2)

matrix(1:6, nrow=2, byrow = TRUE)

matrix(7:8, nrow = 2, ncol = 5)
# работают правила переписывания как для векторов)

# атрибут dim
m <- matrix(1:6, ncol = 3, byrow = TRUE)
c(nrow(m), ncol(m))
dim(m) <- NULL # делаем матрицу вектором!
# и внезапно порядок изменяется!
dim(m) <- c(3, 2); m # а вот так получается и вовсе что-то невообразимое...

# арифметические операторы - поэлементные, если возможно - применяем правила переписывания

m1 <- matrix(1:4, nrow = 2)
m2 <- matrix(c(1, 2, 2, 3), nrow = 2)
m1 + m2
m1 + 5
m1 %*% m2 # вот этот оператор - умножение матриц из линейной алгебры!

# индексирование матриц

m <- matrix(1:10, ncol = 5)
m[1, 3] # выбираем один элемент, указывая два индекса
m[2, ] #выбираем все строку, указывая первый индекс
m[, 4] #выбираем весь столбец, указывая второй индекс

m[1, ] <- 0; m # заменяем соответствующие элеменнты
m[, -5] <- 11:18; m #отрицательная индексация тоже работает

#  Схлопывание размерности

m <- matrix(1:10, ncol = 5)
ind <- c(1, 3, 5)
m[, ind]
ind <- 3
m[, ind] #вот здесь и происходит схлопывание - вызывается не матрица, а вектор)
m[, ind, drop = F] #а так можем вызвать матрицу снова

# именованные матрицы
m <- matrix(1:10, ncol = 5)
rownames(m) <- c("row1", "row2")
colnames(m) <- paste0("column", 1:5) #можем склеивать аргументы в вектор строк!
View(m)

# присоединение матриц

rbind(m1, m2)
cbind(m1, m2)
# в этих функциях есть аргумент ..., который позволяет передавать любое количество объектов
cbind(m1, m2, 1:2, c(5, 3), m2[, 1], m1*3, cbind(m2, m1))
# примеры функций с такими аргументами - c, paste, paste0, sum

# Функция apply

m <- matrix(1:25, 5)
f <- function(x) sum(x^2)
#Три аргумента функции apply
# массив(матрица), индекс - 2 по столбцам, функция
apply(m, 1, f)
apply(m, 2, f)
apply(m, 1:2, function(i) if (i>13) i else 13) #применяем функцию к каждому элементу матрицы
m[m <= 13] <- 13; m #делаем абсолютно то же самое
# функция внутри apply - анонимная, она нигде в окружении не отображается

# rowSums, rowMeans, colSums, colMeans
m <- matrix(1:25, 5)
rowSums(m)
all.equal(rowSums(m), apply(m, 1, sum))
all.equal(colMeans(m), apply(m, 2, mean))


#    Задача 2 - есть integer вектор и число. Найти позицию элемента в векторе, который ближе
# всего к числу n. Если элементов несколько, указать все

find_closest <- function(v, n) {
  which(abs(v-n) == min(abs(v-n)))
}

find_closest(c(5, 2, 7, 7, 7, 2, 0, 0), 1)


# Составляем матрицы диагональным образом ...

bind_diag <- function(m1, m2, fill) {
  m3 <- matrix(fill, nrow = nrow(m1) + nrow(m2),
               ncol = ncol(m1) + ncol(m2))
  m3[1:nrow(m1), 1:ncol(m1)] <- m1
  m3[nrow(m1) + 1:nrow(m2), ncol(m1)+1:ncol(m2)] <- m2
  m3
}

m1 <- matrix(1:12, nrow = 3)
m2 <- matrix(10:15, ncol = 3)
bind_diag(m1, m2, fill = NA)
bind_diag(m2, m1, fill = 0)


#    Задача 3 - нужно построить зиккурат!

# Хорошее решение - без циклов
build_ziggurat <- function(n) {
  size <- n *2-1
  temp <- matrix(NaN, size, size)
  pmin(n-abs(n - row(temp)), n-abs(n-col(temp)))
}

build_ziggurat(4)
?pmin

?t
?apply
?ifelse
?min

# тупое решение "в лоб"
build_ziggurat <- function(n) {
  m <- matrix(1, nrow=2*n-1, ncol=2*n-1)
  for (i in 1:nrow(m)) {
    for (j in 1:ncol(m)) {
        m[i,j] <- min(i,j, 2*n-i, 2*n-j)
    }
  }
  return(m)
}

build_ziggurat(5)


#    Списки

# это индексированная структура, элементы - произвольные объекты
# обычно используют для данных разной длины или различных типов
list(1:5, "my_data", matrix(0, 2, 2))
# способ ключ - значение
list(a = 1, b = 1:3, "1to5" = 1:5, 42)
# список может быть рекурсивным (вложенным)
list(a = list(1, 2, 3), b = list(list(4), 5, 6))

# Конкатенация списков
l1 <- list(name = "john", salary = 1000)
l2 <- list(has_car = TRUE, car = "lamborgini")
c(l1, l2) #вот она!

# Конверсия между списком и вектором
v <- 1:7
list(v) # в сторону списка - легко!
l <- list(1:3, 4:5, last = 6)
unlist(l) # в сторону вектора - только иногда
unlist(c(l, "spy!")) # получаем вектор из строк

# Доступ к элементам списка
# 1 - индексирование: всегда возвращает подсписок
l[3:2]; l[-(1:2)]
l[c(F, T, F)]; l["last"] # возвращает всегда список

# 2 - доступ к конкретному элементу: получается сам элемент (вектор или что там еще)
l[[1]] # скалярный номер элемента
l[["last"]] 

# 3 - доступ по имени с частичным дополнением: получается сам элемент
l$last
l$l # можно указывать уникальный префикс!

# Замена элементов списка
l <- list(1:3, 4:5, last = 6)
l[[3]] <- NULL; l # элемент куда-то исчез)
l[[4]] <- 99; l # но он все еще существует, просто он "пустой"

l <- list(vec = 1:7, fun = sqrt)
l$fun(4) # вызывается просто функция!
names(l)

is.null(l$string) # этого элемента не существует)
l$string <- "Citius, altium, fortium"
l
l[[3]]

# Применение функций к списку: lapply

l <- list(a = c("12", "34"), b = LETTERS[5:10], c = 1:5)
lapply(l, length)
lapply(l, paste, collapse = "|")
# lapply(l, function(s) paste(s, collapse = "|"))
sapply(l, paste, collapse = "|") # если можно получается вектор, получается вектор или матрица

#  Частичное дополнение и зачем оно нужно

l <- list(some_name = 1, incredibly_long_name = 2)
l$inc * 4

f <- function(x, ridiculously_long_arg) x + ridiculously_long_arg
f(3, r = 5) #

?"apply"

# Разбор задачи на максимальную дистанцию между соседними элементами вектора

get_longest <- function(l) {
  len <- sapply(l, length) #находим длину элемента
  ind <- which.max(len) #находим индекс элемента(ов) с максимальной длиной
  return(list(number = ind, element = l[[ind]])) #возвращаем индекс и элемент
}

gen_list <- function(n_elements, max_len, seed = 111) {
  set.seed(seed) #устанавливаем генератор случайных чисел в одном положении
  len <- sample(1:max_len, n_elements) #делаем выборку из чисел от 1 до указанного значения
  # выдает столько случайных чисел, сколько хочется, из нормального распределения
  lapply(1:n_elements, function(i) rnorm(len[i]))
}
?sample

l1 <- gen_list(4, 10)
gl1 <- get_longest(l1)
gl1$element
l2 <- gen_list(4, 10, 777)
get_longest(l2)


#    Задача 5 - указать элементы и частоты

count_elements <- function(v) {
  rbind(sort(unique(v)), sapply(sort(unique(v)), function(x) length(v[v == x])))
}

x <- c(5, 2, 7, 7, 7, 2, 0, 0)
sort(unique(x))
sapply(sort(unique(x)), function(i) length(x[x == i]))

?rbind
count_elements(x)
typeof(count_elements(x))

str(table(x))
str(unlist(table(x)))
unlist(table(x))[[1]]

?table
?matrix
dimnames(table(x))[[1]]


#    Задача 6 - применяем функции, находим минимум

set.seed(1789)
bastille <- list(
  "La Chapelle Tower" = rbinom(5, 10, 1/2), 
  "Tresor Tower" = rbinom(8, 12, 1/4), 
  "Comte Tower" = rbinom(14, 3, 1/5) + 1,
  "Baziniere Tower" = rbinom(8, 4, 4/5), 
  "Bertaudiere Tower" = rbinom(4, 8, 2/3),
  "Liberte Tower" = rbinom(1, 100, 0.1), 
  "Puits Tower" = rbinom(5, 5, 0.7),
  "Coin Tower" = rbinom(3, 16, 0.4)
)
str(bastille)

sum(sapply(bastille, sum)) #сколько всего солдат в Бастилье
which.min(sapply(bastille, sum)) #какая башня хуже всего защищена
