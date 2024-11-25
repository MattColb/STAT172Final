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

#Cleaning and Exploring
summary(cps_data)
str(cps_data)
head(cps_data)
#is there a point to making this a character and then a factor? 
#probably yes and i just need to remember it
#there's a bunch of nulls in here and it's generally binary, currently num type

#try regressions (lasso probably) both with and without some meaningful interaction
#terms to look at the ways your X's could affect eachother and the outcome
table(cps_data$FSFOODS)

#Split the data into train/test df forms to use in lasso/ridge later
RNGkind(sample.kind = "default")
set.seed(26473)
train.idx <- sample(x = 1:nrow(cps_data), size = .7*nrow(cps_data))
train.df <- cps_data[train.idx,]
test.df <- cps_data[-train.idx,]

#----MLE Logistic Regression----

lr_mle_fsfoods <- glm(FSFOODS ~ hhsize + married + education + elderly +
                       kids + black + hispanic + female,
                     data=train.df,
                     family=binomial(link="logit"),
                     weights=weight
)
# Get warnings - algorithm did not converge, complete separation occurred
summary(lr_mle_fsfoods) #grossly high standard error on alll vars

#----Lasso and Ridge with Basic X vars----

