# get working directory
getwd()

# check working directory
setwd('D:\\Spring2025')
getwd()

# Terminate script
i <- 1
while (i>0) {
  print('good')
}

# get help for activate libraries
help('dplyr')
?dplyr

# get help for all installed libraries
??dplyr
help.search('dplyr')

# Assignment operator
x <- 10 
y <- 10 
sum <- x + y  # Add x and y
product <- x * y  # Multiply x and y

print(x)  # Output the value of x
print(sum)  # Output the sum
print(product)  # Output the product

# Changing Variable Values
x <- 15  # Reassign a new value to x
print(x)  # Output the updated value of x

# Variable Types
name <- "R Programming"  # Character
is_great <- TRUE         # Logical
pi_value <- 3.14         # Numeric

# clean environment
rm(list=ls())






