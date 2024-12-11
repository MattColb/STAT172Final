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

cps <- read.csv("./data/cps_00006.csv")
head(cps[,c("CPSID","PERNUM", "FSSTATUS", "FSSTATUSMD", "RACE","EDUC")]) %>% kable

#https://cps.ipums.org/cps-action/variables/search

summary(cps)

cps <- cps %>% mutate(SEX = SEX-1,
                      CHILD = ifelse(AGE < 18, 1, 0),
                      ELDERLY = ifelse(AGE > 59, 1, 0),
                      BLACK = ifelse(RACE==200, 1, 0),
                      HISPANIC = ifelse(HISPAN>0,1,0),
                      EDUC = as.integer(EDUC %in% c(91 ,92, 111, 123, 124, 125)),
                      EMP = as.integer(EMPSTAT%in% c(1,10,12)),
                      MARRIED = as.integer(MARST %in% c(1,2)),
                      DIFF = ifelse(DIFFANY==2,1,0),
                      COUNT = as.factor(COUNTY)
                      )

#Not currently grouping properly
cps_data <- cps %>% group_by(CPSID=as.factor(CPSID)) %>%
  summarise(
    county = first(COUNTY),
    weight = first(HWTFINL),
    hhsize = n(),
    
    #Y variables
    #All should be the same for each person in the household
    #Search for more info about vars here: https://cps.ipums.org/cps-action/variables/search
    FSTOTXPNC_perpers = first(FSTOTXPNC)/hhsize, #Different from packet, but needed for summary
    FSSTATUS = first(FSSTATUS),
    FSSTATUSMD = first(FSSTATUSMD),
    FSFOODS = first(FSFOODS),
    FSWROUTY = first(FSWROUTY),
    FSBAL = first(FSBAL),
    FSRAWSCRA = first(FSRAWSCRA),
    FSTOTXPNC = first(FSTOTXPNC),
    FSSTMPVALC = first(FSSTMPVALC),
    
    female = sum(SEX),
    hispanic = sum(HISPANIC),
    black = sum(BLACK),
    kids = sum(CHILD),
    elderly = sum(ELDERLY),
    education = sum(EDUC),
    married = sum(MARRIED),
    faminc = first(FAMINC),
    donut = as.factor(ifelse(hhsize == (elderly+kids), 1, 0)) #This ended up being
    #is the house have only kids and elderly, not requiring one of both,
    #but it still worked pretty well to signify homes that only had elderly people
    #a lot of the time.
  ) %>% ungroup()

cps_data <- cps_data %>% mutate(
  faminc_cleaned = case_when(
    faminc == 100 ~ "<4999",
    faminc == 210 ~ "5000-7499",
    faminc == 300 ~ "7500-9999",
    faminc == 430 ~ "10000-12499",
    faminc == 470 ~ "12500-14999",
    faminc == 500 ~ "15000-19999",
    faminc == 600 ~ "20000-24999",
    faminc == 710 ~ "25000-29999",
    faminc == 720 ~ "30000-34999",
    faminc == 730 ~ "35000-39999",
    faminc == 740 ~ "40000-49999",
    faminc == 820 ~ "50000-59999",
    faminc == 830 ~ "60000-74999",
    faminc == 841 ~ "75000-99999",
    faminc == 842 ~ "100000-149999",
    faminc == 843 ~ "150000+",
    TRUE ~ NA_character_
  )
)


head(cps_data)
sum(cps_data$hhsize)

cps_data <- cps_data %>% 
  mutate(FSSTATUS = ifelse(FSSTATUS %in% c(98,99), NA, FSSTATUS),
         FSSTATUSMD = ifelse(FSSTATUSMD %in% c(98,99), NA, FSSTATUSMD),
         FSFOODS = ifelse(FSFOODS %in% c(98,99), NA, FSFOODS),
         FSWROUTY = ifelse(FSWROUTY %in% c(96,97,98,99), NA, FSWROUTY),
         FSBAL = ifelse(FSBAL %in% c(96,97,98,99), NA, FSBAL),
         FSRAWSCRA = ifelse(FSRAWSCRA %in% c(98,99), NA, FSRAWSCRA),
         FSTOTXPNC = ifelse(FSTOTXPNC %in% c(999), NA, FSTOTXPNC),
         FSSTMPVALC = ifelse(FSSTMPVALC %in% c(996, 997, 998, 999), 0, FSSTMPVALC)) %>% #The 1000 wasn't in given code, but was always an outlier
  mutate(FSSTATUS = ifelse(FSSTATUS > 1, 1, 0),
         FSSTATUSMD = ifelse(FSSTATUSMD >1, 1, 0),
         FSFOODS = ifelse(FSFOODS > 1, 1, 0),
         FSWROUTY = ifelse(FSWROUTY > 1, 1, 0),
         FSBAL = ifelse(FSBAL > 1, 1, 0),
         FSRAWSCRA = ifelse(FSRAWSCRA >1, 1, 0),
         FSTOTXPNC_perpers = ifelse(is.na(FSTOTXPNC), NA, FSTOTXPNC_perpers),
         FSSTMPVALC_bin = ifelse(FSSTMPVALC > 0, 1, FSSTMPVALC),
         FSSTMPVALC_bin_char = ifelse(FSSTMPVALC == 0, "No", "Yes")
  )

cps_data <- cps_data %>% mutate(
  county = as.factor(county)
) #Large number of levels, so might make sense to leave out, but our models might also tell us that

summary(cps_data)
str(cps_data)
head(cps_data)

table(cps_data$FSWROUTY, cps_data$FSSTMPVALC_bin)

table(cps_data$FSFOODS, cps_data$FSSTMPVALC_bin)

table(cps_data$FSFOODS, cps_data$FSWROUTY)

#PREDICTIVE VARIABLES
#hhsize, married, education, elderly, kids, black, hispanic, female, county(?)

#FOCUS Y VARIABLES:
#FSWROUTY - Phuong
#Binary snap no snap FSSTMPVALC - Matt (This has a very small number of positives)
#FSFOODS - Aria
