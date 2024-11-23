rm(list=ls())

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
    ELDERLY = ifelse(Age > 64, 1, 0),
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
            weight = weight[1])

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

lr_lasso_fsstmp <- readRDS("./models/fsstmp/lasso_model.RDS")
lr_lasso_fsstmp_pi_star <- readRDS("./models/fsstmp/lasso_pi_star.RDS")

test_data <- model.matrix(~hhsize + married + education + elderly +
                  kids + black + hispanic + female, data=acs_data)[,-1]

fsstmp_predictions <- predict(lr_lasso_fsstmp, test_data, type="response")[,1]

acs_predicted <- acs_data %>% mutate(
  fsstmp_prediction = ifelse(fsstmp_predictions > lr_lasso_fsstmp_pi_star, "On Assistance", "Not On Assistance")
)

#How does this adjust with the weights
summary_by_PUMA <- acs_predicted %>% group_by(PUMA = as.factor(PUMA)) %>% 
  summarise(
    sample_size = sum(hhsize),
    total_weights = sum(weight),
    total_weights_by_sample = sum(weight *hhsize),
    people_on_assistance = sum(ifelse(fsstmp_prediction == "On Assistance", 1, 0)),
    people_on_assistance_weighted = sum(ifelse(fsstmp_prediction == "On Assistance", 1, 0)*weight),
    proportion_on_assistance = people_on_assistance/sample_size
  ) %>% as.data.frame() %>% arrange(desc(proportion_on_assistance))


#https://www.geoplatform.gov/metadata/258db7ce-2581-4488-bb5e-e387b6119c7a
sf_data <- st_read("./data/tl_2023_19_puma20/tl_2023_19_puma20.shp")

colnames(sf_data)[colnames(sf_data) == "GEOID20"] = "PUMA"

map_data <- sf_data %>%
  left_join(summary_by_PUMA, by = "PUMA")

ggplot(data = map_data) +
  geom_sf(aes(fill = proportion_on_assistance)) +
  scale_fill_viridis_c(option = "plasma") +  # Adjust color palette as needed
  theme_minimal() +
  labs(title = "Iowa Residents on Food Stamps or SNAP",
       fill = "Proportion of\nResidents")
# hi