library(MASS);library(car);library(AER);library(carData);library(stats)

data('UN')
par(mfrow = c(1,1))
new_un = na.omit(UN)
un_model = lm(infantMortality~ppgdp, data = new_un)
summary(un_model)

res = un_model$residuals
plot(res,new_un$ppgdp)
abline(0,0)

##Task 2.a
## check skewness in independent variable and dependent variable
car::scatterplot(infantMortality~ppgdp, data = new_un,xlab = 'Gross Domestic Product per Capita', 
            ylab = 'Infant Mortality Rate (per 1000 births)', 
            regLine = list(col = 'darkgreen'), 
            smooth = list(col.smooth = "red"))

## Task 2.b 
## estimate if transformation needed for ppgdp
p1 = powerTransform(ppgdp~1, data = new_un, family = 'bcPower')
summary(p1)

##Task 2.c
## examine the data distribution for ppgdp based on three different value of lambda
par(mfrow = c(1,3))
lambda_0 = car::bcPower(new_un$ppgdp, lambda=0)
lambda_optimal = car::bcPower(new_un$ppgdp, lambda=0.026)
lambda_negative = car::bcPower(new_un$ppgdp, lambda=-1)


hist(lambda_0,breaks = 12,main = 'lambda = 0',xlab = 'x')
hist(lambda_optimal,breaks = 12,main = 'lambda = 0.026',xlab = 'x')
hist(lambda_negative,breaks = 12,main = 'lambda = -1',xlab = 'x')

shapiro.test(lambda_0)
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
 
log_lm_model = lm(log(infantMortality)~log(ppgdp), data = new_un)
summary(log_lm_model)

shapiro.test(un_model$residuals)
shapiro.test(log_lm_model$residuals)

??na.omit()

