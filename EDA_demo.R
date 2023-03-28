# EDA
# 2/8/23

library(tidyverse)


## read in data

data <- read_csv("data/and_vertebrates.csv")


## data structure
str(data)


# print var names
names(data)


# see how many years
unique(data$year)

unique(data$species)


# use filter() to index rows

data_pittag <- filter(data, year >= 2007)


# use select() to index columns

select(data, year, section, species)

select(data, where(is.numeric))

select(data, contains("mm"))


select(data, species, everything())


# create and edit current variables with mutate

View(mutate(data, length_1_cm = length_1_mm/10))

data <- mutate(data, size_group = if_else(weight_g > 20, "large", "small"))


#
d1 <- filter(data, year >= 2010)
d2 <- group_by(d1, species)
d3 <- summarise(d2, avg_weight = mean(weight_g, na.rm = TRUE))


# the pipe
d4 <- data %>%
  filter(year >= 2010) %>%
  group_by(species) %>%
  summarise(avg_weight = mean(weight_g, na.rm = TRUE))


ggplot(data, aes(x = weight_g, y = length_1_mm)) +
  geom_point(aes(color = species))

















