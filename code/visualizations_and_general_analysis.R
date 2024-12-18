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

######################
# Y-Variable Overlap #
######################


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

#Adding whether or not there is elderly people as a factor
cps_data <- cps_data %>% mutate(
  has_elderly_fact = as.factor(ifelse(elderly > 0, "Has elderly", "Doesn't have elderly"))
)

#Creating visuals of y-variable overlap
ggplot(cps_data[!is.na(cps_data$FSWROUTY), ]) +
  geom_bar(aes(x=as.factor(FSSTMPVALC_bin_char), fill=
                             as.factor(ifelse(FSWROUTY==1, "Has food anxiety", "Doesn't have food anxiety"))), 
           position="fill") +
  labs(title = "Proportion of people with food anxiety based on SNAP status", 
       x="Are they receiving SNAP benefits?", y="Proportion") +
  ggtitle("Proportion of people with food anxiety based on SNAP status") +
  scale_fill_brewer("Do they\nworry about food?", palette = "Dark2")
ggsave("./figures/FSSTMP_FSWROUTY_OVERLAP_ANALYSIS.png", width=7, height=7.5)

ggplot(cps_data[!is.na(cps_data$FSFOODS), ]) +
  geom_bar(aes(x=as.factor(FSSTMPVALC_bin_char), fill=
                 as.factor(ifelse(FSFOODS==1, "Yes", "No"))), 
           position="fill") +
  labs(title = "Proportion of people not getting preferred food based on SNAP status", 
       x="Are they receiving SNAP benefits?", y="Proportion") +
  ggtitle("Proportion of people not getting preferred food based on SNAP status") +
  scale_fill_brewer("Do they\nget the food\nthey want?", palette = "Dark2")
ggsave("./figures/FSSTMP_FSFOODS_OVERLAP_ANALYSIS.png", width=7, height=7.5)

ggplot(cps_data[!is.na(cps_data$FSFOODS) & !is.na(cps_data$FSWROUTY), ]) +
  geom_bar(aes(x=as.factor(ifelse(FSWROUTY==1, "Has food anxiety", "Doesn't have food anxiety")), fill=
                 as.factor(ifelse(FSFOODS==1, "Yes", "No"))), 
           position="fill") +
  labs(title = "Proportion of people not getting preferred food based on food anxiety", 
       x="Do they have food anxiety?", y="Proportion") +
  ggtitle("Proportion of people not getting preferred food based on food anxiety") +
  scale_fill_brewer("Do they\nget the food\nthey want?", palette = "Dark2")
ggsave("./figures/FSWROUTY_FSFOODS_OVERLAP_ANALYSIS.png", width=7, height=7.5)

#Look at the proportion of people that are on/not on SNAP that are seniors
ggplot(cps_data) + 
  geom_bar(aes(x=as.factor(FSSTMPVALC_bin_char), fill=has_elderly_fact), position="fill") +
  scale_fill_brewer("Elders in household", palette="Dark2") +
  labs(y="Proportion of households", x="Are they on Food Stamps/SNAP?") + 
  ggtitle("Proportion of households with elderly people on Food Stamps/SNAP")+
  title("Proportion of households with elderly people on Food Stamps/SNAP")
ggsave("./figures/fsstmp_household_elderly.png", width=8, height=5)

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

#Look at it specifically in southwest Iowa
sw_iowa_puma <- acs_data[acs_data$PUMA == 1902100,]
weighted.mean(sw_iowa_puma$elderly >0, sw_iowa_puma$weight)
#Percentage of households only elderly
weighted.mean(sw_iowa_puma$elderly == sw_iowa_puma$hhsize, sw_iowa_puma$weight)
#Percentage of households with only one elderly
weighted.mean(sw_iowa_puma$only_one_senior_bin, sw_iowa_puma$weight)


#Checking percentages based on whether they have elderly vs whole population

cps_elderly <- cps_data[cps_data$elderly >= 1,]

#Weighted mean for FSSTMP
weighted.mean(cps_data$FSSTMPVALC_bin, cps_data$weight)

weighted.mean(cps_elderly$FSSTMPVALC_bin, cps_elderly$weight)

#Weighted mean for FSFOODS
weighted.mean(cps_data$FSFOODS, cps_data$weight, na.rm=TRUE)

weighted.mean(cps_elderly$FSFOODS, cps_elderly$weight, na.rm=TRUE)

#Weighted mean for FSWROUTY
weighted.mean(cps_data$FSWROUTY, cps_data$weight, na.rm=TRUE)

weighted.mean(cps_elderly$FSWROUTY, cps_elderly$weight, na.rm=TRUE)
