rm(list=ls())

library(ggplot2)
library(sf)
library(tidyverse)

#Loading in data from our predictions. We can source from other files, 
#but it just takes a lot of time.
#This is how you would do it if you want to use source.
#source("code/fsstamp_analysis.R")
#fsstmp <- fsstmp_acs_predicted_only_seniors

#source("code/FSWROUTY_variable.R")
#fswrouty_one_senior <- fswrouty_prediction_save_data

#source("code/fsfoods_analysis.R")
#fsfoods <- fsfoods_acs_predicted_only_seniors

fsstmp <- read.csv("./data/fsstmp_prediction.csv")
fsfoods <- read.csv("./data/fsfoods_prediction.csv")
fsfwrouty_one_senior <- read.csv("./data/single_senior_household.csv")

fswrouty_one_senior <- fswrouty_one_senior %>% select(
  "PUMA", "single_senior_with_fswrouty"
)


#Merge and rename
merged <- fsstmp %>%  left_join(fswrouty, by="serialno") %>% 
  select("serialno", "PUMA.x", "fswrouty_probs", "fsstmp_probabilities", "hhsize.x", "weight.x", "elderly.x")

merged <- merged %>%  left_join(fsfoods, by="serialno") %>% 
  select("serialno", "PUMA.x", "fswrouty_probs", "fsstmp_probabilities", "fsfoods_prop_preds",
         "hhsize.x", "weight.x", "elderly.x")

names(merged)[names(merged) == 'PUMA.x'] <- 'PUMA'
names(merged)[names(merged) == 'hhsize.x'] <- 'hhsize'
names(merged)[names(merged) == 'weight.x'] <- 'weight'
names(merged)[names(merged) == 'elderly.x'] <- 'elderly'

#Group into summary and merge into shape data
summary_by_PUMA <- merged %>% group_by(PUMA = as.factor(PUMA)) %>% 
  summarise(
    sample_size = sum(hhsize),
    fsstmp = weighted.mean(fsstmp_probabilities, weight),
    fsfoods = weighted.mean(fsfoods_prop_preds, weight),
    fswrouty = weighted.mean(fswrouty_probs, weight),
  ) %>% as.data.frame()

sf_data <- st_read("./data/tl_2023_19_puma20/tl_2023_19_puma20.shp")

colnames(sf_data)[colnames(sf_data) == "GEOID20"] = "PUMA"

map_data <- sf_data %>%
  left_join(summary_by_PUMA, by = "PUMA")

fswrouty_one_senior["PUMA"] = as.character(fswrouty_one_senior$PUMA)

#Join in the fsfoods senior data
map_data <- map_data %>% left_join(fswrouty_one_senior, by="PUMA")


#Load in data about number of seniors per PUMA in Iowa and merge
senior_data <- read.csv("./data/total_iowa_seniors_by_puma.csv")

senior_data <- senior_data %>% mutate("PUMA" = as.character(GEOID))

senior_data <- map_data %>% left_join(senior_data, by="PUMA")

senior_data <- senior_data %>% mutate(
  fsstmp_num_seniors = fsstmp * senior_population, 
  fsfoods_num_seniors = fsfoods * senior_population
)

#Rank each by how PUMA by how much food insecurity there is for each category then average
senior_data <- senior_data %>% 
  mutate(
    rank_fsstmp = rank(-fsstmp_num_seniors),
    rank_fsfoods = rank(-fsfoods_num_seniors),
    rank_fswrouty = rank(-single_senior_with_fswrouty)
  )

senior_data <- senior_data %>% 
  mutate(
    average_rank = (rank_fsstmp + rank_fsfoods + rank_fswrouty)/3
  )

#Remove any where there is no data
senior_data[is.na(senior_data$senior_population),"average_rank"] <- NA

ggplot(data = senior_data) +
  geom_sf(aes(fill = average_rank)) +
  scale_fill_viridis_c(option = "plasma", direction=-1) +  # Adjust color palette as needed
  theme_minimal() +
  labs(title = "Finding the average rank of counties with seniors with food insecurity",
       fill = "Average rank\namong other\ncounties")
ggsave("figures/average_rank_of_insecurity_seniors.png", width=8, height=5)
