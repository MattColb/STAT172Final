#install.packages("sf")

library(sf)
library(dplyr)
library(tidyverse)
library(knitr)
library(tibble)
library(ggthemes)
library(logistf)
library(glmnet)
library(haven)

acs <- read_sas("./data/spm_pu_2022.sas7bdat") #This is big, takes a while to load
acs <- acs %>% filter(st=="19") %>%
  group_by(serialno = as.factor(serialno)) %>%
  arrange(desc(Sex), desc(Age)) %>%
  mutate(weight = first(wt)) %>% select(-wt) %>% ungroup()

acs_data <- acs %>% 
  mutate(
    SEX = Sex-1,
    CHILD = ifelse(Age < 18, 1, 0),
    ELDERLY = ifelse(Age > 59, 1, 0),
    BLACK = ifelse(Race==2, 1, 0),
    HISPANIC = ifelse(Hispanic > 0, 1, 0),
    EDUC = as.integer(Education %in% c(3,4)),
    MARRIED = as.integer(Mar %in% c(1)),
    PUMA = as.factor(PUMA)
  )

acs_data <- acs_data %>% group_by(serialno = as.factor(serialno)) %>%
  summarise(PUMA = first(PUMA),
            hhsize = length(serialno),
            female = sum(SEX),
            hispanic = sum(HISPANIC),
            black = sum(BLACK),
            kids = sum(CHILD),
            elderly = sum(ELDERLY),
            education = sum(EDUC),
            married = sum(MARRIED),
            weight = weight[1],
            faminc_cleaned = first(AGI))

head(acs_data)

#Not entirely sure that this is the way to go since they are samples
#But maybe we can use the weights?
data_by_PUMA <- acs_data %>% group_by(PUMA = as.factor(PUMA)) %>% 
  summarize(
    total_residents = sum(hhsize),
    avg_hhsize = mean(hhsize),
    total_female = sum(female),
    avg_female = mean(female),
    total_hispanic = sum(hispanic),
    avg_hispanic = mean(hispanic),
    total_black = sum(black),
    avg_black = mean(black),
    total_kids = sum(kids),
    avg_kids = mean(kids),
    total_elderly = sum(elderly),
    avg_elderly = mean(elderly),
    total_education = sum(education),
    avg_education = mean(education),
    total_married = sum(married),
    avg_married = mean(married),
  )

# hi