library(MASS)
data("Boston")
getwd()

CPS1985 = read.csv('cps1985.csv')

plot(Boston$dis, Boston$medv, 
     xlab = "Distance", 
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

## scatterplot visualization
car::scatterplot(medv~dis, data = Boston, smooth = list(span = 0.35,lty.smooth=1,
                                                        col.smooth = "red", col.var = "red"),
                 regLine =  list(col = "green"), 
                 xlab = 'Distance to Employment center',
                 ylab = 'Median Value of Home in $1000S')

## shapiro.test to assess the normality
# H_0: sample comes from a normally distributed population
shapiro.test(Boston$dis)

## Box-cox transformation for independent variable
summary_boxcox_dis = summary(powerTransform(dis~1, data = Boston))
summary_boxcox_dis
## if transformation needed and not in log (lambda != 0)
bcpower_dis = bcPower(Boston$dis, lambda =  0)
par(mfrow = c(1,2))
hist(Boston$dis, main = 'Untransformed Distribution', xlab = 'Dist')
hist(bcpower_dis, main = "Transformed Distribution", xlab = 'Transformed Dist')

## Box-cox transformation for dependent variable
summary(powerTransform(medv~log(dis), data = Boston))


## Linear model in transformed system
transformed_bivariate_model = lm(log(medv) ~ log(dis), data=  Boston)
summary(transformed_bivariate_model)

original_residual = Boston$medv - exp(transformed_bivariate_model$fitted.values)

plot(exp(transformed_bivariate_model$fitted.values), original_residual)
scatterplot(log(medv)~log(dis), data = Boston)

plotBoxCox(Boston$medv, Boston$dis, 0,0)

