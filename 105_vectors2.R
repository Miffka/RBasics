#setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RBasics")

#    Правила переписывания (recycling)
# длина результата равна длине большего из векторов
# меньший вектор дублируется (переписывается) несколько раз, чтобы длина 
#переписанного вектора совпала с длиной большего
# если длина большего вектора не делится нацело на длину меньшего, выдается предупреждение

1:5 + 0:1
# Зачем это нужно?
1:10 + 3 #вот для таких случаев)
(5:8)^2 #и для таких - помним о приоритете операторов
1:4 >= 3

#доступ к элементам вектора

x <- seq(10, 100, by = 10)
# оператор []
?"["
#x[ind]
#val <- x[ind]
#x[ind] <- val #присваивание значению вектора значения переменной val
x[]

# положительные индексы

x[1]
x[3:4]
x[c(8,7,3,6:8,x[1])] # можно обращаться рекурсивно и повторять элементы)

# отрицательные индексы - вернуть все элементы, кроме указанных
x[-5]
x[-(2:6)]
x[c(-3, -5, -length(x), -5)] # при повторении удаляется только один элемент

# логические индексы
x[rep(c(TRUE, FALSE), 5)] # возвращаем все нечетные значения
x[c(TRUE, FALSE)] # вступает в силу правило переписывания!
x[x > 77 & x < 99]

# индексация по имени
names(x) <- c("one", "two", "three", "four", "five", "six", "seven", "eigth", "nine", "ten")
str(x)
x[c("one", "three")]
x[c("one", "forty")]


# Функции all и any

all(x < 200)
all(x > 20)
any(x == 100)

# Функция which

which(x >= 50)
which.min(x)
which.max(x)

# Атрибуты объектов

# length - свойство объекта
# атрибуты могут присутсвовать, если это необходимо (names, dimnames, dim, etc.)
y <- c(5, 3, 9)
names(y) <- c("V", "III", "IX")
attr(y, "author") <- "Caesar" # присваиваем атрибут объекту
attributes(y)
str(y)
attributes(y) <- NULL #удаляем все атрибуты
str(y)

#    Задача 1 - правила переписывания

x <- c(-35:35)
x[1:length(x)%%7 != 0] #работает
x[c(rep(T, 6), F)] #работает
x[1:length(x)%%7 > 0] #работает
x[-(1:floor(length(x)/7)*7)] #работает
x[-(seq(7, length(x), by = 7))]


#    Задача 2 - свойства функций

n21 <- 5
n22 <- 1:10
n23 <- 22:19
n24 <- c("A", "BBB", "Z")
n25 <- c("1", "99", "HI")
n26 <- c(TRUE, FALSE)
max(n26)
which(n25)
which.max(n26)


#    Классическая задача программиста

# fizz-buzz, крайне неоптимизированное решение
y <- vector(mode = "character", length = 100)
y <- character(100) # эта строка делает ровно то же самое, что и предыдущая
for (i in 1:100) {
  if (i %% 15 == 0) {
    y[i] <- "fizz buzz"
  } else if (i %% 3 == 0) {
    y[i] <- "fizz"
  } else if (i %% 5 == 0) {
    y[i] <- "buzz"
  } else {
    y[i] <- i
  }
}

# fizz-buzz, вектор-ориентированное решение
x <- 1:100
z <- 1:100
x %% 5
x %% 5 == 0
z[x %% 5 == 0] <- "buzz"
z[x %% 3 == 0] <- "fizz"
z[x %% 15 == 0] <- "fizz buzz"
all.equal(y, z)


#    Задача 4 - расшифровать последовательность

letters[c(1,23,5,19,15,13,5)]

#    Сочетание функций с правилами переписывания

# геометрическая прогрессия
 x <- 2^(0:10)
 log2(x)
 
# Рандомные значения - иногда бывает удобно для отладки функций
 
set.seed(42)
x <- sample(1:100, 50)

# соседние числа с максимальной разницей
# шаг 1 - создаем вектор из всех разниц при помощи разности векторов
x[-1]
x[-length(x)]
x[-1] - x[-length(x)] # вектор попарных разностей
k <- which.max(abs(x[-1] - x[-length(x)]))
x[c(k, k+1)]

# множественные минимумы/максимумы
x <- sample(1:100, 50, replace = TRUE) # есть повторяющиеся значения
min(x)
which.min(x) # находит только одну позицию
which(x == min(x)) #находит все вхождения минимума x

# Упакова в функцию
maxdiff <- function(x) {
  y <- abs(x[-1] - x[-length(x)])
  k <- which(y == max(y))
  print("First neighbor(s):")
  print(x[k])
  print("Second neighbor(s):")
  print(x[k + 1])
  print("Maximum absolute diff is:")
  print(max(y))
}

xx <- sample(1:100, 1e4, replace = TRUE)
maxdiff(xx)


#    Задача 5 - нестрого возрастающие и убывающие вектора

n51 <- c(0, 0, 3, 4, 4, 8)

fun5 <- function(x) {
  return(all((x[-1]-x[-length(x)]) >= 0) | all((x[-1]-x[-length(x)]) <= 0))
}
fun5(n51)
fun5(rep(0,10))
fun5(5)
?diff
# diff(n51) - делает все, что нам нужно в этой задаче)


#    Задача 6 - комбинаторика

combin_count <- function(n, k, with_repetitions = FALSE) {
  if (with_repetitions == TRUE) {
    ans <- factorial(n+k-1)/(factorial(k)*factorial(n-1))
  } else {
    ans <- factorial(n)/(factorial(k)*factorial(n-k))
  }
  return(ans)
}

?`function`
?factorial
combin_count(5,2, with_repetitions = TRUE)
