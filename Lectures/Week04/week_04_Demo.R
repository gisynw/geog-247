## Read Data
library(MASS);library(car)
data("Boston")
getwd()

####################################
                                  
# plot x and y                    
####################################
plot(Boston$dis, Boston$medv,xlab = "Distance", 
     ylab = "Median Housing Value",
     pch = 16, col = "blue")

# Add regression line
bivariate_model = lm(medv~dis, data = Boston)

abline(bivariate_model, col = "red", lwd = 2)

summary(bivariate_model)

## Residual plot based on y variable vs residuals
par(mfrow = c(1,1))
plot(bivariate_model$fitted.values, bivariate_model$residuals, xlab = 'Predicted Y', ylab = 'Residuals')
trend_line <- lm(bivariate_model$residuals ~ Boston$medv)
abline(h = 0, col = 'red')

####################################
## scatterplot visualization
####################################
car::scatterplot(medv~dis, data = Boston, smooth = list(span = 0.35,lty.smooth=1,
                                                        col.smooth = "red", col.var = "red"),
                 regLine =  list(col = "green"), 
                 xlab = 'Distance to Employment center',
                 ylab = 'Median Value of Home in $1000S')

hist(Boston$dis, breaks = 20, main = 'Univariate Distribution of Distance',xlab = 'Distance')

########################################
## shapiro.test to assess the normality
# H_0: sample comes from a normally distributed population
########################################
shapiro.test(Boston$dis)

####################################################
## Box-cox transformation for independent variable
####################################################
summary_boxcox_dis = summary(powerTransform(dis~1, data = Boston))
summary_boxcox_dis

####################################################
## if transformation needed and not in log (lambda != 0)
####################################################
bcpower_dis = bcPower(Boston$dis, lambda =  0)
par(mfrow = c(1,2))
hist(Boston$dis, main = 'Untransformed Distribution', xlab = 'Dist')
hist(bcpower_dis, main = "Transformed Distribution", xlab = 'Transformed Dist')

####################################################
## Box-cox transformation for dependent variable
####################################################
summary(powerTransform(medv~log(dis), data = Boston))

####################################################
## Linear model in transformed system
####################################################
transformed_bivariate_model = lm(log(medv) ~ log(dis), data=  Boston)
summary(transformed_bivariate_model)

car::scatterplot(log(medv)~log(dis), 
                 data = Boston, smooth = list(span = 0.35,lty.smooth=1,col.smooth = "red", col.var = "red"),
                 regLine =  list(col = "green"), 
                 xlab = 'Distance to Employment center(log)',
                 ylab = 'Median Value of Home in $1000S(log)')



