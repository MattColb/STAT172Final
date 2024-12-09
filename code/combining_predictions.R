rm(list=ls())

library(ggplot2)
library(sf)
library(tidyverse)

fswrouty <- read.csv("./data/fswrouty_prediction.csv")

fsstmp <- read.csv("./data/fsstmp_prediction.csv")

fsfoods <- read.csv("./data/fsfoods_prediction.csv")

head(fsstmp)

merged <- fsstmp %>%  left_join(fswrouty, by="serialno") %>% 
  select("serialno", "PUMA.x", "fswrouty_probs", "fsstmp_probabilities", "hhsize.x", "weight.x", "elderly.x")

merged <- merged %>%  left_join(fsfoods, by="serialno") %>% 
  select("serialno", "PUMA.x", "fswrouty_probs", "fsstmp_probabilities", "fsfoods_prop_preds",
         "hhsize.x", "weight.x", "elderly.x")

names(merged)[names(merged) == 'PUMA.x'] <- 'PUMA'
names(merged)[names(merged) == 'hhsize.x'] <- 'hhsize'

names(merged)[names(merged) == 'weight.x'] <- 'weight'

names(merged)[names(merged) == 'elderly.x'] <- 'elderly'

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

fsfoods_one_senior <- read.csv("./data/single_senior_household.csv")

fsfoods_one_senior <- fsfoods_one_senior %>% select(
  "PUMA", "single_senior_with_fswrouty"
)

fsfoods_one_senior["PUMA"] = as.character(fsfoods_one_senior$PUMA)

map_data <- map_data %>% left_join(fsfoods_one_senior, by="PUMA")



senior_data <- read.csv("./data/iowa_seniors_by_puma.csv")

senior_data <- senior_data %>% mutate("PUMA" = as.character(GEOID))

senior_data <- map_data %>% left_join(senior_data, by="PUMA")

senior_data <- senior_data %>% mutate(
  fsstmp_num_seniors = fsstmp * senior_population, 
  fsfoods_num_seniors = fsfoods * senior_population, 
  fswrouty_num_seniors = fswrouty * senior_population 
)

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

senior_data[is.na(senior_data$senior_population),"average_rank"] <- NA

ggplot(data = senior_data) +
  geom_sf(aes(fill = average_rank)) +
  scale_fill_viridis_c(option = "plasma", direction=-1) +  # Adjust color palette as needed
  theme_minimal() +
  labs(title = "Finding the average rank of counties with seniors with food insecurity",
       fill = "Average rank\namong other\ncounties")
ggsave("figures/average_rank_of_insecurity_seniors.png", width=8, height=5)
