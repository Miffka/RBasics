#setwd(as.character("C:/Users/miffka/Documents/!DataMining/RAnalysis"))
setwd("/media/glycosylase/EC6A2F256A2EEBD0/Documents and Settings/miffka/Documents/!DataMining/RBasics")

# Функции
#объект "первого класса" - ничем не хуже любого другого объекта
str(c(mean, max)) #просто список из двух элементов
fun_list <- c(mean, max)
sapply(fun_list, function(f) f(1:1000))
# воувоувоу, мы же только что просто обратились к функциям как к элементам вектора 0_____0

# Функция как аргумент
apply_f <- function(f, x) f(x)
sapply(fun_list, apply_f, x = 1:100)
apply_f(function(x) sum(x^2), 1:10) #охтыжнихрена себе, мы же создали функцию внутри вызова функции

# Функция как возвращаемое значение
square <- function() function(x) x^2
square()
square()(5) #чтоблджатмагиядвескобкифункциявнутрифункцииоргияфункций!!!!1111

f <- function(x) { #создаем внешнюю функцию
  g <- function(y) if (y > 0) 1 else if (y < 0) -1 else 0 #создаем функцию в функции
  sapply(x, g) #возвращаем ответ в виде применения внутренней функции к каждому элементу аргумента внешней функции
}
all.equal(f(-100:100), sign(-100:100))
# идеальный случай - функция нужна только внутри, и не очень громоздка
# функция внутри функции - один из вариантов инкапсуляции в R, чтоб не захламлять окружение

# Исходный код функции
(f <- function(x) x^5)
sd #вау, мы видим исходный код функции еманарот!!!!1111
# если в выводе есть .C, .Call, .Fortran, .External, .Internal, .Primitive - это
#обращение к скомпилированному коду - нужно смотреть исходный код R
var
#Если есть UseMethod или standardGeneric - это метод dispatch для классво S3/S4 (полиморфизм)
plot
methods(plot)

# Возвращаемое значение
# либо return
has_na <- function(v) {  
  for (k in v) if (is.na(k)) return(T)
  return(F)}
# либо последнее вычисленное значение
has_na <- function(v) any(is.na(v))

# Аргументы по умолчанию
seq(from = 1, to = 1, by = ((to - from)/length.out - 1), length.out = NULL, along.with = NULL)
# аргументы могут иметь значения по умолчанию
# значения могут вычисляться на лету
seq(1, 0, len = 5)

# Правила разбора аргументов
f <- function(arg1, arg2, remove_na = TRUE, ..., optional_arg) {}
f(1, arg2 = 2, remove = F, optional_arg = 42, do_magic = TRUE)
#сначала разбираются аргументы по полному совпадению, потом - по частичному (только до ...)
#потом аргументы разбираются по позиции
#неразобранные попадают в ... - do_magic

# Проброс аргументов
# ellipsis - "произвольное количество передаваемых объектов"
# также это "проброс аргументов"
f <- function(x, pow = 2) x^pow
integrate(f, 0, 1) #lower = 0, upper = 1, pow = 2
integrate(f, 0, 1, pow = 5) #то же самое, но pow = 5
# в многоточии указываются аргументы внутренней функции, которые не получили совпадений во внешней функции

# Бинарные операторы
# x %in% y
1:5 %in% c(1, 2, 5)

'%nin%' <- function(x, y) !(x %in% y)
1:5 %nin% c(1, 2, 5)


#    Задача 2 - забросить в функцию paste еще аргументов

paste
?paste
task20 <- "kokoko"
?rev
rev_pat <- paste(rev(strsplit(task20, NULL)[[1]]), collapse = "")

decorate_string <- function(pattern, ...) { 
  rev_pat <- paste(rev(strsplit(pattern, NULL)[[1]]), collapse = "")
  paste(pattern, paste(...), rev_pat, sep = "")
}

decorate_string(pattern = "123", "abc")
decorate_string(pattern = "123", "abc", "def")
decorate_string(pattern = "123", c("abc", "def"))
decorate_string(pattern = "123", "abc", "def", sep = "+")
decorate_string(pattern = "!", c("x", "x"), collapse = "_")
decorate_string(pattern = ".:", 1:2, 3:4, 5:6, sep = "&")


# Функция, назначающая функцию

# Генерируем колоду карт
values <-  c("Ace", 2:10, "Jack", "Queen", "King")
suits <- c("Clubs", "Diamonds", "Hearts", "Spades")
# следующая функция генерирует все сочетания этих двух векторов
card_deck <- outer(values, suits, paste, sep = " of ")
length(card_deck)

# Фабрика функций
generator <- function(set) function(n) sample(set, n, replace = T)

# определяем генераторы
card_generator <- generator(card_deck)
coin_generator <- generator(c("Heads", "Tails"))
str(card_generator)

# Начинаем играть!
card_generator(10)
coin_generator(5)


#    Задача 4 - пишем игру в рулетку: честную и не очень

generator <- function(set, prob = rep(1/length(set), length(set))) { 
  function(n) sample(set, n, replace = T, prob)
} 

roulette_values <- c("Zero!", 1:36)
# для нечестной рулетки можно завести новый вектор значений
#rig_roulette_values <- c(roulette_values, "Zero!")
fair_roulette <- generator(roulette_values)
rigged_roulette <- generator(roulette_values, prob = c(2/38, rep(1/38, 36)))
#rigged_roulette <- generator(rig_roulette_values)

?sample
?rep
# А еще можно завести новый вектор вероятностей
#prob = с(2/38, rep(1/38, 36))
fair_roulette
rigged_roulette
# Для нового вектора значений все работает нормально, как и для нового вектора вероятностей
#table(fair_roulette(10000))
table(rigged_roulette(10000))


#    Задача 5 - ищем функцию в norm

norm
?svd


#    Задача 6 - пишем бинарный оператор поэлементного складывания без правил переписывания

"%+%" <- function(x, y) {
  if (length(x) != length(y)) {
    if (length(x) > length(y)) {
      ans <- c(x[1:length(y)] + y, rep(NA, length(x) - length(y)))
    } else {
      ans <- c(y[1:length(x)] + x, rep(NA, length(y) - length(x)))
    }
  } else {
    ans <- x + y
  }
  return(ans)
}
1:5 %+% 1:2   # c(2, 4, NA, NA, NA)
5 %+% c(2, 6) # c(7, NA) 
1:5 %+% 2:6
