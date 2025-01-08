##Task 1.a
prop.test(x=724, n = 1000, conf.level = 0.95)

prop.test(x=724, n = 1000, conf.level = 0.99)

##Task 2.a - b
library(MASS)
data('Boston')
ggplot(Boston, aes(x = rm, y = medv, colour = indus)) +
  geom_point()

##Task 2.c
Boston$chas = as.factor(Boston$chas)

hw_sp <- ggplot(Boston, aes(x = rm, y = medv, colour = chas)) +
  geom_point() +
  scale_colour_brewer(palette = "Set1")
hw_sp + geom_smooth()

##Task 3.a
library(carData)
data('MplsDemo')

scatterplotMatrix(~population + white + black + hhIncome, data = MplsDemo)

skewness(MplsDemo$population)
skewness(MplsDemo$white)
skewness(MplsDemo$black)
skewness(MplsDemo$hhIncome)



