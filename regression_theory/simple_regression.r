library(DAAG) # пакет, в котором лежит база spam7
library(ggplot2) # очень хороший пакет для визуализации
library(GGally) # здесь мы берем функцию gpairs
library(dplyr) # кледезь функций для табличек 
library(car)

require(rpart) # пакет для дерева решений
library(rpart.plot) # пакет для визуализации дерева решений

intall.packages('DAAG') # так мы устанавливаем пакеты


df = cars # подгружаем базу cars

head(df) # показать первые 5 строк

df = mutate(df, dist = dist*0.3, speed = 1.67*speed) # переводим километры в час и просто километры
qplot(df$speed, df$dist, xlab = 'speed', ylab= 'dist')#+ stat_smooth(method = 'lm') # визуализация  

m1 = lm(data= df, dist~speed) 
m1
summary(m1)

df2 = swiss # следующая база "Швециарские контоны"

head(df2)# показать первые 5 строк

ggpairs(df2) # хорошая визуализация 

m2 = lm(data=df2, Fertility~Agriculture+Education+Catholic) # следующая модель
summary(m2)



###########################
spam = rpart(formula = yesno ~ crl.tot + dollar + bang + money + n000 + make, data=spam7) # дерево решений
rpart.plot(spam) # визуализация дерева