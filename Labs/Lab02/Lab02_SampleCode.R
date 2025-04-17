library(MASS);library(car);library(AER);library(carData);library(stats)

##Task 1.a 
data('Boston')
boston_model = lm(medv~rm, data = Boston)

##Task 1.b
summary(boston_model)

##Task 1.c
confint(boston_model)

## Task 2.a
data('UN')
par(mfrow = c(1,1))
new_un = na.omit(UN)

##Task 2.b
## check skewness in independent variable and dependent variable
car::scatterplot(infantMortality~ppgdp, data = new_un,xlab = 'Gross Domestic Product per Capita', 
            ylab = 'Infant Mortality Rate (per 1000 births)', 
            regLine = list(col = 'darkgreen'), 
            smooth = list(col.smooth = "red"))

## Task 2.c 
## estimate if transformation needed for ppgdp
p1 = powerTransform(ppgdp~1, data = new_un, family = 'bcPower')
summary(p1)

##Task 2.d
## examine the data distribution for ppgdp based on three different value of lambda

lambda_1 = car::bcPower(new_un$ppgdp, lambda=1)
lambda_optimal = car::bcPower(new_un$ppgdp, lambda=0.019)
lambda_negative = car::bcPower(new_un$ppgdp, lambda=-1)

par(mfrow = c(1,3))
hist(lambda_1,breaks = 12,main = 'lambda = 1',xlab = 'x')
hist(lambda_optimal,breaks = 12,main = 'lambda = 0.019',xlab = 'x')
hist(lambda_negative,breaks = 12,main = 'lambda = -1',xlab = 'x')

shapiro.test(lambda_1)
shapiro.test(lambda_optimal)
shapiro.test(lambda_negative)


## Task 2.e
## Estimate if transformation needed for infantMortality
p2 = powerTransform(infantMortality~log(ppgdp), data = new_un, family = 'bcPower')
summary(p2)

## Task 2.f
car::scatterplot(log(infantMortality)~log(ppgdp), data = new_un,xlab = 'Gross Domestic Product per Capita', 
                 ylab = 'Infant Mortality Rate (per 1000 births)', 
                 regLine = list(col = 'darkgreen'), 
                 smooth = list(col.smooth = "red"))

## Task 2.g
log_lm_model = lm(log(infantMortality)~log(ppgdp), data = new_un)
summary(log_lm_model)

plot(log_lm_model$residuals~log_lm_model$fitted.values)

lm_model = lm(infantMortality~ppgdp, data = new_un)
plot(lm_model$residuals~lm_model$fitted.values)
