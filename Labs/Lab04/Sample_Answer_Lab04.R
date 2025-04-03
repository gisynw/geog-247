library( car);library(MASS)
getwd()
Boston = read.csv('Boston.csv')

# Task 2.a
var_initial_model <- Boston[ , !names(Boston) %in% "lstat"]
initial_model = lm(medv~., data = var_initial_model)
summary(initial_model)

#Task 2.b
vif(initial_model)

## Task 2.c
var_second_model <- var_initial_model[ , !names(var_initial_model) %in% "total_area"]
second_model = lm(medv~., data = var_second_model)
summary(second_model)

vif(second_model)

## Task 2.e
par(mfrow = c(1,1))
plot(second_model$fitted.values, second_model$residuals)

plot(second_model$residuals, second_model$fitted.values )

#add a horizontal line at 0 
abline(0,0)

##Task 2.f
var_third_model = Boston[ , !names(Boston) %in% "total_area"]
third_model = lm(medv~., data = var_third_model)
anova(second_model, third_model)

##Task 2.g
par(mar = c(60, 6, 4, 2)) 
car::influenceIndexPlot(third_model, id= list(cex=2),cex.lab = 1.5,cex.axis = 1.5)
average_hat = 14/493
average_hat*3
var_third_model[c(369,372,381,419), ]

library(dplyr);library(ggplot2);library(tidyr)

# Assuming your data is stored in a dataframe called df
new_data = var_third_model[var_third_model$medv == 50,]
# Load necessary library
new_data$index <- as.numeric(rownames(new_data))  # Convert row names to numeric

# Convert data to long format for ggplot

long_data <- new_data %>%
  pivot_longer(cols = -c(index, medv), names_to = "variable", values_to = "value")

line_colors <- c("Other" = "black", "369" = "blue", "372" = "red")

# Plot all rows as lines
ggplot(long_data, aes(x = variable, y = value, group = index, color = factor(index))) +
  geom_line(data = subset(long_data, !(index %in% c(369, 372))), aes(color = "Other")) +  # Other rows in gray
  geom_line(data = subset(long_data, index == 369), aes(color = "369"), size = 1.2) +  # Row 369 in blue
  geom_line(data = subset(long_data, index == 372), aes(color = "372"), size = 1.2) +  # Row 372 in red
  theme_minimal() +
  scale_color_manual(values = line_colors, name = "Legend", labels = c("Other Observations", "Observation 369", "Observation 372")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Line Plot of Observations with Medv = 50",
       x = "x Variables",
       y = "Values")


# View the result
print(df_selected)


par(mar = c(8, 6, 4, 2))
hist(Boston$crim, breaks = 20)
hist(Boston$black, breaks = 20)
hist(Boston$lstat, breaks = 20)
hist(Boston$medv,breaks = 20)
