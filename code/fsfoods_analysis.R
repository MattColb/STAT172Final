#clean FSFOODS analysis file
rm(list=ls())

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

summary(cps_data)
str(cps_data)
head(cps_data)
#is there a point to making this a character and then a factor? 
#probably yes and i just need to remember it
cps_data <- cps_data %>% mutate(
  FSSTMPVALC_bin_fact = as.factor(FSSTMPVALC_bin_char)
)