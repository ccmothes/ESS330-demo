# RStudio Demo
# Caitlin
### 2/1/23

library(tidyverse)

# data types and formats

## Vectors -------------------------------------
y <- 1:10

class(y)

str(y)

z <- c("apple", "pear", "grape")

z2 <- c("apple", 1:10)

## lists -----------------------
z2 <- list("apple", 1:10)


## data frame
df <- data.frame(fruit = c("apple", "pear"), numbers = 1:2)


## read in a csv
data <- read.csv("data/KEY_cleaned_survey.csv")


mean(data$Weight_grams, na.rm = TRUE)


data2 <- read_csv("data/KEY_cleaned_survey.csv")






