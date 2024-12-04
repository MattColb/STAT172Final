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

#######################################
#  Visualizing percentage of elderly  #
#######################################

acs_data <-acs_data %>%  mutate(
  has_senior = as.factor(ifelse(elderly > 0, "Has Elderly", "Doesn't have Elderly")),
  only_seniors = as.factor(ifelse(elderly == hhsize, "Only Elderly", "No Elderly"))
)

#Percentage of households with elderly
mean(acs_data$elderly >0)
#Percentage of households only elderly
mean(acs_data$elderly == acs_data$hhsize)

#About 10% of households have elderly people and non-elderly people

ggplot(data=acs_data) + 
  geom_histogram(aes(x=hhsize), binwidth=1)

ggplot(data=acs_data) +
  geom_histogram(aes(x=hhsize, fill=only_seniors), binwidth=1, position="fill")





