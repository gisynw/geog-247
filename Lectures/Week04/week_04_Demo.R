# library(glmtoolbox)

advertising = read.csv('D:\\Spring2025\\geog-247-statistics\\docs\\Lectures\\Week04\\advertising.csv')

plot(advertising$TV, advertising$sales, 
     main = "Regression of Sales on TV Advertising",
     xlab = "TV Advertising Budget", 
     ylab = "Sales",
     pch = 16, col = "blue")

# Add regression line
abline(bivariate_model, col = "red", lwd = 2)

bivariate_model = lm(sales~TV, data = advertising)

summary(bivariate_model)

## Residual plot based on x variable vs residuals
plot(advertising$TV, bivariate_model$residuals, xlab = 'TA Advertising Budget', ylab = 'Residuals')
trend_line <- lm(bivariate_model$residuals ~ advertising$TV)
abline(trend_line, col = "darkgreen", lwd = 2)

##Residual plot based on x variable vs fitted value
plot(bivariate_model$fitted.values, bivariate_model$residuals, xlab = 'Fitted values', ylab = 'Residuals')
trend_line <- lm(bivariate_model$residuals ~ advertising$TV)
abline(trend_line, col = "darkgreen", lwd = 2)






