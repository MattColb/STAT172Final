rm(list=ls())

library(ggplot2)

fswrouty <- read.csv("./data/fswrouty_prediction.csv")

fsstmp <- read.csv("./data/fsstmp_prediction.csv")

head(fsstmp)

merged <- fsstmp %>%  left_join(fswrouty, by="serialno") %>% 
  select("serialno", "PUMA.x", "fswrouty_probs", "fsstmp_probabilities", "hhsize.x", "weight.x", "elderly.x")

names(merged)[names(merged) == 'PUMA.x'] <- 'PUMA'
names(merged)[names(merged) == 'hhsize.x'] <- 'hhsize'

names(merged)[names(merged) == 'weight.x'] <- 'weight'

names(merged)[names(merged) == 'elderly.x'] <- 'elderly'

merged_standard <- merged

merged_standard[,c("fswrouty_probs", "fsstmp_probabilities")] <- apply(merged, 2, function(x)(x - mean(x))/sd(x))[,c("fswrouty_probs", "fsstmp_probabilities")]

merged_standard <- merged_standard %>% mutate(
  sum_of_z_scores = (fswrouty_probs + fsstmp_probabilities)/2
)

ggplot(data=merged_standard) + geom_histogram(aes(x=sum_of_z_scores))

summary_by_PUMA <- merged_standard %>% group_by(PUMA = as.factor(PUMA)) %>% 
  summarise(
    sample_size = sum(hhsize),
    food_insecurity_metric = sum(sum_of_z_scores),
  ) %>% as.data.frame() %>% arrange(desc(food_insecurity_metric))

sf_data <- st_read("./data/tl_2023_19_puma20/tl_2023_19_puma20.shp")

colnames(sf_data)[colnames(sf_data) == "GEOID20"] = "PUMA"

map_data <- sf_data %>%
  left_join(summary_by_PUMA, by = "PUMA")

#Proportion of seniors that are on SNAP/Food Stamps
ggplot(data = map_data) +
  geom_sf(aes(fill = food_insecurity_metric)) +
  scale_fill_viridis_c(option = "plasma") +  # Adjust color palette as needed
  theme_minimal() +
  labs(title = "Proportion of Households with Seniors on SNAP/Food Stamps",
       fill = "Proportion on\nFood Stamps/SNAP")
ggsave("figures/propotion_of_seniors_predicted.png", width=6, height=5)
