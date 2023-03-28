# KEY for Week 5 Intro to Statistics Assignment
# Caitlin Mothes ; 02/14/2023


library(tidyverse)
library(lterdatasampler)


# import dataset
data(and_vertebrates)


# EXERCISE 1 : Chi-square ---------------------------

## Clean data - filter Coastal giant salamander and drop NAs
ex1_clean <- and_vertebrates %>% 
  filter(species == "Coastal giant salamander") %>% 
  drop_na(unittype, section)


## convert to contingency table
cont_table <- table(ex1_clean$section, ex1_clean$unittype)


## conduct the chi-square test
chisq.test(cont_table)

# RESULTS #

# Pearson's Chi-squared test
# 
# data:  cont_table
# X-squared = 200.71, df = 5, p-value < 2.2e-16


# EXERCISE 2 - t-test ------------------------------------------
## NOTE - students can either transform the data OR specify non-equal variances,
## I have both steps here

## clean data (they could use the cleaned data from EX 1, but would need to drop_na for weight)
ex2_clean <- and_vertebrates %>% 
  filter(species == "Coastal giant salamander") %>% 
  drop_na(weight_g)


## must test for equal variances first
cc_weight <- ex2_clean %>% 
  filter(section == "CC") %>% 
  pull(weight_g)

og_weight <- ex2_clean %>% 
  filter(section == "OG") %>% 
  pull(weight_g)

var.test(cc_weight, og_weight)


# RESULTS #
# F test to compare two variances
# 
# data:  cc_weight and og_weight
# F = 0.82901, num df = 3027, denom df = 3309, p-value = 1.439e-07
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.7732148 0.8889213
# sample estimates:
#   ratio of variances 
# 0.8290065 

## Variances are NOT EQUAL


# option 1 - variable transformation
t.test(log(ex2_clean$weight_g) ~ ex2_clean$section, var.equal = TRUE)

# Two Sample t-test
# 
# data:  log(ex2_clean$weight_g) by ex2_clean$section
# t = 10.091, df = 6336, p-value < 2.2e-16
# alternative hypothesis: true difference in means between group CC and group OG is not equal to 0
# 95 percent confidence interval:
#   0.2033610 0.3014243
# sample estimates:
#   mean in group CC mean in group OG 
# 1.828557         1.576164



# option 2 - Welch's test

t.test(ex2_clean$weight_g ~ ex2_clean$section, var.equal = FALSE)

# Welch Two Sample t-test
# 
# data:  ex2_clean$weight_g by ex2_clean$section
# t = 4.9255, df = 6335.9, p-value = 8.629e-07
# alternative hypothesis: true difference in means between group CC and group OG is not equal to 0
# 95 percent confidence interval:
#   0.8978633 2.0850725
# sample estimates:
#   mean in group CC mean in group OG 
# 9.810634         8.319166 



# EXERCISE 3 - Correlation ----------------------------------
## NOTE students can transform variables OR use Spearman test, I have both options here


## clean dataset
ex3_clean <- and_vertebrates %>% 
  filter(species == "Cutthroat trout") %>% 
  #Note they must use length_1 for snout to fork length
  drop_na(length_1_mm, weight_g)


## test for normal dist of variables (can do graphically or statistically)

## graphically
hist(ex3_clean$length_1_mm)
hist(ex3_clean$weight_g)


## statistically
shapiro.test(ex3_clean$length_1_mm[1:5000])
shapiro.test(ex3_clean$weight_g[1:5000])


## Both show non-normal variables

# option 1 - variable transformation
cor.test(log(ex3_clean$length_1_mm), log(ex3_clean$weight_g))

# Pearson's product-moment correlation

# data:  log(ex3_clean$length_1_mm) and log(ex3_clean$weight_g)
# t = 915.66, df = 12590, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.9923125 0.9928295
# sample estimates:
#       cor 
# 0.9925755 



# option 2 - Spearman non-parametric test
cor.test(ex3_clean$length_1_mm, ex3_clean$weight_g, method = "spearman")


# Spearman's rank correlation rho
# 
# data:  ex3_clean$length_1_mm and ex3_clean$weight_g
# S = 2669679446, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.9919772 