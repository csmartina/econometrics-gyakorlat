# ------------------------------------------------------------------------------
# Coding basics in R
# based on
#   - Ágoston Reguly, Gábor Békés: Introduction to Data Analysis with R - lecture materials
#   - Rafael Irizarry: Introduction to Data Science: Data Wrangling and Visualization with R

## useful short cuts -----------------------------------------------------------
# assignment: Alt + - / Option + -
# pipe operator: Ctrl + Shift + M / Command + Shift + M
# run current line/selection: Ctrl + Enter / Command + Enter
# comment/uncomment line/selection: Ctrl + Shift + C / Command + Shift + C

## cleaning the environment ----------------------------------------------------
rm(list = ls())

## packages/libraries ----------------------------------------------------------
# install packages
install.packages('tidyverse')
install.packages('wooldridge')

# load packages

library(tidyverse)
library(wooldridge)

## 1) coding basics ------------------------------------------------------------
# objects and variables
a <- 2 # numerical
b <- 3
c <- a*b

# integers and doubles
  num_val <- as.numeric(1.2)
  doub_val <- as.double(1.2)  
  int_val <- as.integer(1.2)  
is.numeric(num_val)  
is.double(num_val)
is.integer(num_val)

mystring <- 'Hello' # character/string
myotherstring <-'World'

# string concatenation
themstring <- mystring + myotherstring #not working
themstring <- paste0(mystring, " ", myotherstring)

male <- TRUE  # logical/boolean
female <- FALSE

  a == b
  a != b  
  equality <- a ==  b

  # built-in functions
sqrt(4)
typeof(male)
print(c)
print(themstring)
print(equality)

#vectors
v <- c(2, 5, 10, 13)
z <- c(3, 4, 7, 10)
u <- c("one", "two", "apple")
w <- c(v, z) # concatenation of vectors

#matrices
A = matrix(data = c(2, 5, 3, 8, 7, 1), nrow = 2)
B = matrix(data = c(7, 10, 1, 2, 5), nrow = 3)
# matrix with named rows and columns
  C = matrix( data = c(2, 5, 3, 8), nrow = 2,
              dimnames = list(c('row1', 'row2'), # first argument is for rows
                               c('col1', 'col2'))) #second argument is for columns
  #define a matrix using row binds and column binds
  # row binds
  d1 <- c(100, 47, 178)
  d2 <- c(45, 24, 176)  
  d3 <- c(57, 29, 189)
  rownames <- list(c('Alex', 'Bob', 'Charlie'))
  colnames <- list(c('wage', 'age', 'height'))
  M1 <- rbind(d1, d2, d3)
  dimnames(M1) <- c(rownames, colnames)
  
  # column bind
  e1 <- c(2, 5)
  e2 <- c(8,3)
  M2 <- cbind(e1, e2)

  C[1,2] #indexing  
C['row1', 'col2']  #indexing with names

#operation with vectors and matrices
v + z #element-wise addiction
v*z #element-wise multiplication
a+v #simple arithmetic with a vector
v%*%t(z) #matrix multiplicaiton
t(A) #transpose of a matrix
solve(C) #inverse of a matrix

#lists
my_list <- list('a', 2, 0 == 1)
my_list2 <- list(c('a', 'b'), c(1, 2, 3), sqrt(2)^2==2, my_list)

#list indexing
my_list2[1]
typeof(my_list2[1]) # list

my_list2[[1]]
typeof(my_list2[[1]]) # character vector

my_list2[[1]][2]
typeof(my_list2[[1]][2]) # character vector

# number of elements
length(v)
length(w) # number of elements in the new vector
length(A) # number of elements in the matrix
dim(A) # dimensions of the matrix

# indexing
v[1]
v[2:4]
v[c(2, 4)]

# special objects
null_vector <- c() # null vector
inf_vector <- c(Inf, 1, 2, 3, 4) # infinity
nan_vector <- c(NaN, 1, 2, 3, 4) # impossible values (e.g., dividing by zero) are represented by NaN (not a number)
na_vector <- c(NA, 1, 2, 3, 4) # missing values are represented by NA (not available)

# operations with special objects
null_vector + 3
nan_vector + 3
na_vector + 3
inf_vector + 3

# rounding
sqrt(2)^2 == 2 # why? -> numerical approximation
round(sqrt(2)^2) == 2 # fixing

# loops
# for loop
for(i in 1:5){
  print(paste0("i = ", i))
}

# while loop
x <- 100
while(x > 10){
  x <- x - 10
  print(paste0("There are ", x, " km left."))
}

# if conditions
price_apple <- 105
price_tesla <- 100

# if apple price is lower buy apple, if bigger buy tesla, if equal print equal
if(price_apple < price_tesla){
  print('Buy Apple!')
} else if(price_apple > price_tesla){
  print('Buy Tesla!')
} else {
  print('Prices are equal!')
}

## 2) case study: CEO salary ---------------------------------------------------
# Story: What's the connection between CEO salary and ROE?
# load CEO salary dataset
ceo <- ceosal1 # built-in data from wooldridge package

# first look on the data
str(ceo) # first few rows
head(ceo) # first five rows
view(ceo) # whole data

# visualisation in tidyverse
# simple scatter plot about ceo salary and roe
ggplot(data = ceo,
       mapping = aes(y = salary,
                     x = roe)) +
  geom_point()

# save "main settings"
roe_salary_plot <- ggplot(data = ceo,
                          mapping = aes(y = salary,
                                        x = roe))

# add a scatter plot and linear line to the main settings
roe_salary_plot +
  geom_point() +
  geom_smooth(method = lm)

# check which observations are outliers (over 99. percentile) by salary
# plot the distribution of the salary
ggplot(data = ceo,
       mapping = aes(x = salary)) +
  geom_histogram(binwidth = 100) +
  scale_x_continuous(breaks = seq(0, 15000, by = 3000))

quantile(ceo$salary, probs = 0.99)

# drop outliers, by salary
cutoff <- quantile(ceo$salary, probs = 0.99)
ceo <- ceo[ceo$salary < cutoff, ]

# how to use with
# generate industry variable
ceo$industry <- factor(with(ceo, ifelse(indus == 1, 'industrial',
                                        ifelse(finance == 1, 'financial',
                                               ifelse(consprod == 1, 'consumer products', 'transport and utility')))))

# we have to save main settings again as data has changed
roe_salary_plot <- ggplot(data = ceo,
                          mapping = aes(y = salary,
                                        x = roe))
# colour by industry
roe_salary_plot +
  aes(colour = industry) +
  geom_point() +
  geom_smooth(method = lm)

# colour only points
roe_salary_plot +
  geom_point(mapping = aes(colour = industry)) +
  geom_smooth(method = lm)

# colour only points but add new color to the line
roe_salary_plot +
  geom_point(mapping = aes(colour = industry)) +
  geom_smooth(method = lm,
              colour = 'black')

# add labels
roe_salary_plot +
  geom_point(mapping = aes(colour = industry)) +
  geom_smooth(method = lm,
              colour = 'black') +
  labs(y = 'Salary ($1000)',
       x = 'ROE',
       color = 'Industry')

# make the plot more fancy and save
roe_salary_figuresave <- roe_salary_plot +
  geom_point(mapping = aes(colour = industry),
             size = 3,
             alpha = 0.7) +
  geom_smooth(method = lm,
              colour = 'black',
              linetype = 'dashed',
              linewidth = 1) +
  labs(title = 'CEO Salary vs. Return on Equity (ROE)',
       y = 'Salary ($1000)',
       x = 'ROE (%)',
       color = '') +
  theme_minimal() +
  theme(text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.position = 'top') +
  scale_color_brewer(palette = 'Set1') +
  scale_x_continuous(breaks = seq(-20, 80, by = 10)) +
  scale_y_continuous(breaks = seq(0, 6000, by = 500))

# save the plot


