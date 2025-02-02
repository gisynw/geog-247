
data('advertising')

bivariate_model = lm(sales~TV, data = advertising)

plot(bivariate_model)

summary(bivariate_model)

plot(advertising$TV, advertising$sales, 
     main = "Regression of Sales on TV Advertising",
     xlab = "TV Advertising Budget", 
     ylab = "Sales",
     pch = 16, col = "blue")

# Add regression line
abline(bivariate_model, col = "red", lwd = 2)

#############################################################
## Bivariate model based on UN Dataset                      #
#############################################################
library(carData);library(car);library(e1071);library(stats)

data('Boston')

biva_model = lm(medv~dis, data = Boston)

summary(powerTransform(Boston$dis~1))

scatterplot(medv~dis, data = Boston,xlab = 'Distance to Employment Center', 
            ylab = 'Median Value of Homes in $1000', 
            regLine = list(col = 'darkgreen'), 
            smooth = list(col.smooth = "red"))

hist(Boston$dis, breaks = 20, main = 'Distribution of Distance',
     xlab = 'Distance', probability = TRUE)
lines(density(Boston$dis), col = "blue", lwd = 2)

skewness(Boston$dis)
shapiro.test(Boston$dis)






