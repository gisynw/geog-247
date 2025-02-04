library(MASS);library(car);library(AER);library(carData)

data('UN')

un_model = lm(infantMortality~ppgdp, data = UN)

plot(un_model)

## check skewness in independent variable and dependent variable
car::scatterplot(infantMortality~ppgdp, data = UN,xlab = 'Gross Domestic Product per Capita', 
            ylab = 'Infant Mortality Rate (per 1000 births)', 
            regLine = list(col = 'darkgreen'), 
            smooth = list(col.smooth = "red"))


## estimate if transformation needed for ppgdp
p1 = powerTransform(ppgdp~1, data = UN, family = 'bcPower')
summary(p1)

## examine the data distribution for ppgdp based on three different value of lambda
par(mfrow = c(1,3))
hist(car::bcPower(UN$ppgdp, lambda=0),breaks = 12,main = 'lambda = 0',xlab = 'x')
hist(car::bcPower(UN$ppgdp, lambda=0.026),breaks = 12,main = 'lambda = 0.026',xlab = 'x')
hist(car::bcPower(UN$ppgdp, lambda=-1),breaks = 12,main = 'lambda = -1',xlab = 'x')

## estimate if transformation needed for infantMortality
p2 = powerTransform(infantMortality~log(ppgdp), data = UN, family = 'bcPower')
summary(p2)


summary(powerTransform(infantMortality~log(ppgdp), data = UN, family = 'bcPower'))

 
