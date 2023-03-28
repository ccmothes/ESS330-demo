# Intro stats Demo 1

library(tidyverse)
library(palmerpenguins)


data(penguins)


#flipper length
penguins %>% 
  ggplot(aes(x = flipper_length_mm))+
  geom_histogram(aes(fill = species))

# species counts
penguins %>% 
  group_by(species) %>% 
  summarise(abudance = n())


penguins %>% 
  count(species)

count(penguins, species)

# boxplot of flipper length

penguins %>% 
  filter(species == "Gentoo") %>% 
  drop_na(sex, flipper_length_mm) %>% 
  ggplot(aes(x=sex, y = flipper_length_mm))+
  geom_boxplot()


# test for variances

gentoo <- penguins %>% 
  filter(species == "Gentoo") %>% 
  drop_na(sex, flipper_length_mm)


male <- filter(gentoo, sex == "male") %>% pull(flipper_length_mm)

female <- filter(gentoo, sex == "female") %>% pull(flipper_length_mm)


var.test(male, female)


hist(male)
hist(female)


shapiro.test(male)
shapiro.test(female)


# t-test
t.test(gentoo$flipper_length_mm ~ gentoo$sex, var.equal = FALSE)


# correlation test


penguins %>% 
  ggplot(aes(x = bill_depth_mm, y = bill_length_mm))+
  geom_point(aes(color = species))



adelie <- penguins %>% 
  filter(species == "Adelie") %>% 
  drop_na(bill_depth_mm, bill_length_mm)


hist(adelie$bill_length_mm)
hist(adelie$bill_depth_mm)


shapiro.test(adelie$bill_depth_mm)
shapiro.test(adelie$bill_length_mm)


hist(log(adelie$bill_length_mm))


cor.test(adelie$bill_length_mm, adelie$bill_depth_mm)

# Pearson's product-moment correlation
# 
# data:  adelie$bill_length_mm and adelie$bill_depth_mm
# t = 5.1933, df = 149, p-value = 6.674e-07
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.2472226 0.5187796
# sample estimates:
#       cor 
# 0.3914917 


adelie %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm))+
  geom_point()+
  geom_smooth(method = "lm")
