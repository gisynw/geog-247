# Working directory

# get working directory
getwd()

# check working directory
setwd('D:\\Teaching_Clark\\GitRepo\\Spring2025\\geog-247\\docs\\Lectures\\Week01')
getwd()


# Terminate script

i <- 1
while (i>0) {
  print('good')
}


# Get help for activate libraries

help('dplyr')
?dplyr

# get help for all installed libraries
??dplyr
help.search('dplyr')


# Variables

x <- 10 
y <- 10 
sum <- x + y  # Add x and y
product <- x * y  # Multiply x and y

print(x)  # Output the value of x
print(sum)  # Output the sum
print(product)  # Output the product


## Changing Variable Values

x <- 15  # Reassign a new value to x
print(x)  # Output the updated value of x

# Variable Types
name <- "R Programming"  # Character
is_great <- TRUE         # Logical
pi_value <- 3.14         # Numeric


# List

#creating a list
my_list <- list(
  name = "John Doe",
  age = 30,
  scores = c(85, 90, 95),
  passed = TRUE
)

# Print the list
print(my_list)

# Access by position
print(my_list[[1]])  # Outputs: "John Doe"

# Access by name
print(my_list[["age"]])  # Outputs: 30

# Using $
print(my_list$scores)  # Outputs: c(85, 90, 95)


# Clean environment

rm(list=ls())


# Read csv

data <- read.csv("parks_trees.csv")

# Display the first few rows of the dataset
head(data)

# Check the structure of the dataset
str(data)

# Summary statistics for each column
summary(data)


## Display column names

colnames(data)

# Inspecting a specific column
data$neighborhood

# add new columns
data$data_source <- "Boston_GIS"

