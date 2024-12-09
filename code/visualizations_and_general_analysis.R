rm(list=ls())

library(ggplot2)
library(tidyverse)
library(knitr)
library(tibble)
library(ggthemes)
library(logistf)
library(glmnet)
library(haven)
library(pROC)
library(RColorBrewer)
library(randomForest)

source("./code/clean_cps.R")
source("./code/clean_acs.R")

table(cps_data$FSWROUTY, cps_data$FSSTMPVALC_bin)
#With this table, we can see that a little over half
#of people who are on food stamps are also worrying about 
#being able to afford food. This is compared to the ~1/4
#of people not on food stamps

table(cps_data$FSFOODS, cps_data$FSSTMPVALC_bin)
#We see that about 2/3 of the people who are on food stamps
#are not able to get enough to eat or get the types of foods
#that they want to eat.
#This is compared to about 1/6 of people not on food stamps

table(cps_data$FSFOODS, cps_data$FSWROUTY)
#We see that about 3/4 of people who worry about being able
#to afford food are also not getting enough to eat or not
#getting the types of food that they enjoy.
#Even without this, a little under half are not getting enough to eat
#or are able to eat the types of food that they enjoy
#Even without worrying about being able to afford food.

table(cps_data$FSFOODS, cps_data$FSWROUTY, cps_data$FSSTMPVALC_bin)
#About 216 people experience all 3.

cps_data <- cps_data %>% mutate(
  sum_of_food_insecurity = FSFOODS + FSWROUTY + FSSTMPVALC_bin,
  has_elderly_fact = as.factor(ifelse(elderly > 0, "Has elderly", "Doesn't have elderly"))
)

ggplot(cps_data) + 
  geom_bar(aes(x=as.factor(FSSTMPVALC_bin_char), fill=has_elderly_fact), position="fill") +
  scale_fill_brewer("Elders in household", palette="Dark2") +
  labs(y="Proportion of households", x="Are they on Food Stamps/SNAP?") + 
  ggtitle("Proportion of households with elderly people on Food Stamps/SNAP")+
  title("Proportion of households with elderly people on Food Stamps/SNAP")
ggsave("./figures/fsstmp_household_elderly.png", width=8, height=5)


ggplot(data=cps_data) +
  geom_histogram(aes(x=sum_of_food_insecurity, fill=has_elderly_fact), binwidth=1) + 
  labs(x="Number of food insecurity indicators", y="Count") +
  title("Total investigated food insecurity indicators") + 
  ggtitle("Total investigated food insecurity indicators") +
  scale_fill_brewer("Seniors in household", palette="Dark2")
ggsave("./figures/food_insecurity_indicators")

#######################################
#  Visualizing percentage of elderly  #
#######################################

acs_data <-acs_data %>%  mutate(
  has_senior = as.factor(ifelse(elderly > 0, "Has Elderly", "Doesn't have Elderly")),
  has_seniors_bin = ifelse(elderly > 0,1,0),
  only_seniors = as.factor(ifelse(elderly == hhsize, "Only Elderly", "Not only Elderly")),
  only_seniors_bin = ifelse(elderly==hhsize, 1, 0),
  only_one_senior_bin = ifelse(elderly ==hhsize & elderly==1, 1, 0),
  true_donut = as.factor(ifelse(donut != elderly, 1,0))
)

#Percentage of households with elderly
weighted.mean(acs_data$elderly >0, acs_data$weight)
#Percentage of households only elderly
weighted.mean(acs_data$elderly == acs_data$hhsize, acs_data$weight)
#Percentage of households with only one elderly
weighted.mean(acs_data$only_one_senior_bin, acs_data$weight)

sw_iowa_puma <- acs_data[acs_data$PUMA == 1902100,]
weighted.mean(sw_iowa_puma$elderly >0, sw_iowa_puma$weight)
#Percentage of households only elderly
weighted.mean(sw_iowa_puma$elderly == sw_iowa_puma$hhsize, sw_iowa_puma$weight)
#Percentage of households with only one elderly
weighted.mean(sw_iowa_puma$only_one_senior_bin, sw_iowa_puma$weight)


#About 10% of households have elderly people and non-elderly people

ggplot(data=acs_data) + 
  geom_histogram(aes(x=hhsize), binwidth=1)

ggplot(data=acs_data) +
  geom_histogram(aes(x=hhsize, fill=only_seniors), binwidth=1, position="fill")

ggplot(data=acs_data) +
  geom_histogram(aes(x=hhsize, fill=has_senior), binwidth=1) + 
  scale_fill_brewer("Does household\nhave elderly", palette="Dark2") +
  labs(x="Household Size", y="Count") + title("ACS Households with elderly")
ggsave("./figures/acs_elderly_population.png", width=6, height=5)
  
ggplot(data=acs_data) +
  geom_histogram(aes(x=hhsize, fill=has_senior), binwidth=1, position="fill") + 
  scale_fill_brewer("Does household\nhave elderly", palette="Dark2") +
  labs(x="Household Size", y="Proportion of elderly") + title("Proportion of ACS Households with elderly")
ggsave("./figures/acs_elderly_proportion.png", width=6, height=5)

ggplot(data=cps_data) +
  geom_boxplot(aes(x=faminc_cleaned, fill=elderly))

ggplot(data=acs_data) +
  geom_histogram(aes(x=hhsize, fill=true_donut), binwidth=1, position="fill") + title

cps_elderly <- cps_data[cps_data$elderly >= 1,]
cps_nonelderly <- cps_data[cps_data$elderly < 1,]

#Weighted mean for FSSTMP
weighted.mean(cps_data$FSSTMPVALC_bin, cps_data$weight)

weighted.mean(cps_elderly$FSSTMPVALC_bin, cps_elderly$weight)

#Weighted mean for FSFOODS
weighted.mean(cps_data$FSFOODS, cps_data$weight, na.rm=TRUE)

weighted.mean(cps_elderly$FSFOODS, cps_elderly$weight, na.rm=TRUE)

#Weighted mean for FSWROUTY
weighted.mean(cps_data$FSWROUTY, cps_data$weight, na.rm=TRUE)

weighted.mean(cps_elderly$FSWROUTY, cps_elderly$weight, na.rm=TRUE)

weighted.mean(acs_data$only_seniors_bin, acs_data$weight)

weighted.mean(acs_data$has_seniors_bin, acs_data$weight)

df <- as.factor(df)
data <- table(cps_data$FSFOOD, cps_data$FSWROUTY)
ggplot(cps_data, aes(x = FSFOODS, y = FSWROUTY, fill = Count)) +
  geom_tile() +
  scale_fill_brewer()
  labs(title = "Relationship Between FSFOODS, FSWROUTY, and FSSTMPVALC_bin",
       x = "FSFOODS",
       y = "FSWROUTY",
       fill = "Count")

d <- melt(table(FSFOODS=cps_data$FSFOODS, FSWROUTY=cps_data$FSWROUTY))
ggplot(data=d) +
  geom_tile(aes(x=FSFOODS, y=FSWROUTY, fill=value)) +
  scale_fill_brewer(palette="Dark2")

