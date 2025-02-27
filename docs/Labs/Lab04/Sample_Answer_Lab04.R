library( car);library(MASS)
getwd()
Boston = read.csv('Boston.csv')

# Task 2.a
var_initial_model <- Boston[ , !names(Boston) %in% "lstat"]
initial_model = lm(medv~., data = Boston)
summary(initial_model)

#Task 2.b
vif(initial_model)

## Task 2.c
var_second_model <- var_initial_model[ , !names(var_initial_model) %in% "rm"]
second_model = lm(medv~., data = var_second_model)
summary(second_model)

## Task 2.e
par(mfrow = c(1,1))
plot(second_model$fitted.values, second_model$residuals)
#add a horizontal line at 0 
abline(0,0)

##Task 2.f
var_third_model = Boston[ , !names(Boston) %in% "total_area"]
third_model = lm(medv~., data = var_third_model)
anova(second_model, third_model)

##Task 2.g
car::influenceIndexPlot(third_model)

var_third_model[c(369,372,381,419), ]
