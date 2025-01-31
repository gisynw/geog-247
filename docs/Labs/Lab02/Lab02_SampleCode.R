library(MASS);library(car)

data('Boston')

biva_model = lm(medv~rm, data = Boston)

summary(powerTransform(Boston$medv~1))

hist(Boston$medv)
