library(MASS);library(car);library(AER);library(carData)

data('UN')

un_model = lm(infantMortality~ppgdp, data = UN)

plot(un_model)

## check skewness in independent variable and dependent variable
scatterplot(infantMortality~ppgdp, data = UN,xlab = 'Gross Domestic Product per Capita', 
            ylab = 'Infant Mortality Rate (per 1000 births)', 
            regLine = list(col = 'darkgreen'), 
            smooth = list(col.smooth = "red"))


## estimate lambda for dependent variable
p1 = powerTransform(infantMortality~1, data = UN, family = 'bcPower')
summary(p1)

## 
