rm(list=ls())

library(tidyverse)
library(knitr)
library(tibble)
library(ggthemes)
library(logistf)
library(glmnet)
library(haven)

cps <- read.csv("./data/cps_00005.csv")
head(cps[,c("CPSID","PERNUM", "FSSTATUS", "FSSTATUSMD", "RACE","EDUC")]) %>% kable

#https://cps.ipums.org/cps-action/variables/search

summary(cps)

#map_chr(cps, ~attr(.x, "label")) %>% 
#  bind_cols(names=names(cps), question = .) %>%
#  rownames_to_column(var="Variable Name") %>% kable

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
    COUNTY = first(COUNTY),
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
    #FSSTATUS = first(FSSTATUS),
    
    #FSTMPVALC, FSRAWSCRM, 
    
    female = sum(SEX),
    hispanic = sum(HISPANIC),
    black = sum(BLACK),
    kids = sum(CHILD),
    elderly = sum(ELDERLY),
    education = sum(EDUC),
    married = sum(MARRIED)
  ) %>% ungroup()

#Non-included variables (Not in ACS):
#EMPSTAT, DIFFANY, VETSTAT

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
         FSSTMPVALC = ifelse(FSSTMPVALC %in% c(996, 997, 998, 999), NA, FSSTMPVALC)) %>% #The 1000 wasn't in given code, but was always an outlier
  mutate(FSSTATUS = ifelse(FSSTATUS > 1, 1, 0),
         FSSTATUSMD = ifelse(FSSTATUSMD >1, 1, 0),
         FSFOODS = ifelse(FSFOODS > 1, 1, 0),
         FSWROUTY = ifelse(FSWROUTY > 1, 1, 0),
         FSBAL = ifelse(FSBAL > 1, 1, 0),
         FSRAWSCRA = ifelse(FSRAWSCRA >1, 1, 0),
         FSTOTXPNC_perpers = ifelse(is.na(FSTOTXPNC), NA, FSTOTXPNC_perpers),
         FSSSTMPVALC_bin = ifelse(is.na(FSSTMPVALC), 0, 1)
  )



summary(cps_data)
str(cps_data)
head(cps_data)

ggplot(data=cps_data) + 
  geom_point(aes(x=hhsize, y=FSTOTXPNC_perpers))

ggplot(data=cps_data) + 
  geom_point(aes(x=kids, y=FSTOTXPNC_perpers))

ggplot(data=cps_data) + 
  geom_point(aes(x=elderly, y=FSTOTXPNC_perpers))

ggplot(data=cps_data, aes(x=kids, y=hhsize)) + 
  geom_count(aes(color=after_stat(n)))

ggplot(data=filter(cps_data, kids==0)) + 
  geom_point(aes(x=hhsize, y=FSTOTXPNC_perpers))

#PREDICTIVE VARIABLES
#hhsize, married, education, elderly, kids, black, hispanic, female, county(?)

#FOCUS Y VARIABLES:
#FSWROUTY - Phuong
#Binary snap no snap FSSTMPVALC - Matt (This has a very small number of positives)
#FSFOODS - Aria



#FORWARD REGRESSION (IMPROVE):
remove_na = filter(cps_data, !is.na(cps_data$FSWROUTY))

firths =logistf(FSWROUTY ~ hhsize + married + education + elderly + kids + black + hispanic + female,
        data=remove_na,
        weights = weight)

m1 = glm(FSWROUTY ~ hhsize + married + education + elderly + kids + black + hispanic + female,
         data=remove_na,
         weights = weight,
         family = binomial(link="logit"))
m0 = glm(FSWROUTY ~ 1,
         data=remove_na,
         weights = weight,
         family = binomial(link="logit"))

v1 = step(m0, scope=list(lower = m0, upper=m1, direction="both"))
summary(v1)
