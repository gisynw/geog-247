##Task 1.a

library(interpretCI)
x_95=propCI(n = 1000, p = 0.724, alpha = 0.05)
lower_95 = round(x_95$result$lower,3)
upper_95 = round(x_95$result$upper,3)
print(c('95% confidence interval: ',lower_95,upper_95 ))

x_99=propCI(n = 1000, p = 0.724, alpha = 0.01)
lower_99 = round(x_99$result$lower,3)
upper_99 = round(x_99$result$upper,3)
print(c('99% confidence interval: ',  lower_99, upper_99))

plot(0, 0, type = "n", xlim = c(0.65, 0.80), ylim = c(1, 2), 
     xlab = "Confidence Interval", ylab = "", 
     main = "Confidence Intervals", axes = FALSE)

# add x axis
axis(1, at = seq(0.65, 0.80, by = 0.01))

# plot two confidence interval
segments(lower_95, 1.2, upper_95, 1.2, col = "blue", lwd = 2)  
segments(lower_99, 1.1, upper_99, 1.1, col = "red", lwd = 2)      

# add start and end point of confidence interval
points(c(lower_95, upper_95), c(1.2, 1.2), pch = 16, col = "blue")
points(c(lower_99, upper_99), c(1.1, 1.1), pch = 16, col = "red")

# add legend
legend("topright", 
       legend = c(paste("CI 95%:",lower_95, "-", upper_95), paste("CI 99%: ", lower_99,"-",upper_99)), 
       col = c("blue", "red"), lwd = 2, bg = "white")

##Task 2.a - b
??MASS::Boston
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
library(carData); library(car);library(e1071)
data('MplsDemo')

scatterplotMatrix(~population + white + black + hhIncome, data = MplsDemo)

skewness(MplsDemo$population)
skewness(MplsDemo$white)
skewness(MplsDemo$black)
skewness(MplsDemo$hhIncome)



