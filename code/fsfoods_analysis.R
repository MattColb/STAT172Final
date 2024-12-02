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
library(dplyr)

source("./code/clean_cps.R")
source("./code/clean_acs.R")

#Cleaning and Exploring
summary(cps_data)
str(cps_data)
head(cps_data)
#is there a point to making this a character and then a factor? 
##YES because random forest won't work on numeric, it works on factors
##because then it sees it as a classification, not regression, problem.
#probably yes and i just need to remember it
#there's a bunch of nulls in here and it's generally binary, currently num type

#just drop the na's and take note of how many obs you had and how many are left


#try regressions (lasso probably) both with and without some meaningful interaction
#terms to look at the ways your X's could affect eachother and the outcome

summary(cps_data$FSFOODS) #FSFOODS has 1755 NA's
cps_data_f <- cps_data[!is.na(cps_data$FSFOODS),]
summary(cps_data_f$FSFOODS) #new subset without NAs has 6665 obs, compared to 8420 originally
summary(cps_data_f)


#Split the data into train/test df forms to use in lasso/ridge later
RNGkind(sample.kind = "default")
set.seed(26473)
train.idx <- sample(x = 1:nrow(cps_data_f), size = .7*nrow(cps_data_f))
train.df <- cps_data_f[train.idx,]
test.df <- cps_data_f[-train.idx,]

#----MLE Logistic Regression----

lr_mle_fsfoods <- glm(FSFOODS ~ hhsize + married + education + elderly +
                       kids + black + hispanic + female,
                     data=train.df,
                     family=binomial(link="logit"),
                     weights=weight
)
# Get warnings - algorithm did not converge, complete separation occurred
summary(lr_mle_fsfoods) #grossly high standard error on alll vars
#look at the coefficients from the MLE logistic regression
beta <- lr_mle_fsfoods %>% coef()
beta

#---Firth's Penalized Likelihood----


#----Lasso and Ridge with Basic X vars----
## Make all necessary matrices and vectors
fsfoods.x.train <- model.matrix(FSFOODS~hhsize + married + education + elderly +
                                  kids + black + hispanic + female,
                                data = train.df)[,-1] 
fsfoods.x.test <- model.matrix(FSFOODS~hhsize + married + education + elderly +
                                 kids + black + hispanic + female,
                               data = test.df)[,-1]
fsfoods.y.train <- train.df$FSFOODS %>% as.vector()
fsfoods.y.test <- test.df$FSFOODS %>% as.vector()
train.weights <- as.vector(train.df$weight)
test.weights <- as.vector(test.df$weight) #not strictly necessary, for ease of reference

#Use cross validation to get tuning info for final regression
fsfoods_lasso_cv <- cv.glmnet(fsfoods.x.train, #MATRIX without our Y COLUMN
                         fsfoods.y.train, #VECTOR - our Y COLUMN
                         family = binomial(link = "logit"),
                         alpha = 1, 
                         weights = train.weights #1 for lasso, 0 for ridge
                         )
fsfoods_ridge_cv <- cv.glmnet(fsfoods.x.train, #MATRIX without our Y COLUMN
                         fsfoods.y.train, #VECTOR - our Y COLUMN
                         family = binomial(link = "logit"),
                         alpha = 0 #1 for lasso, 0 for ridge
)

#Find and extract minimizing lambda values
plot(fsfoods_lasso_cv)
plot(fsfoods_ridge_cv)

best_lasso_lambda <- fsfoods_lasso_cv$lambda.min
best_ridge_lambda <- fsfoods_ridge_cv$lambda.min

#fit final lasso + ridge models
fsfoods_lasso_f1 <- glmnet(fsfoods.x.train, fsfoods.y.train, 
                      family = binomial(link = "logit"), alpha = 1,
                      lambda = best_lasso_lambda) #this lambda is what actually tunes the model
fsfoods_ridge_f1 <- glmnet(fsfoods.x.train, fsfoods.y.train, 
                      family = binomial(link = "logit"), alpha = 0,
                      lambda = best_ridge_lambda) #this lambda is what actually tunes the model
#----Random Forest----

#----Clustering---

#----Compare Models' Performances----
#First, on the testing data split
test.df.cpspreds <- test.df %>% 
  mutate(
    mle_pred = predict(lr_mle, test.df, type = "response"),
    #note: lasso and ridge get the MATRIX x.test 
    lasso_pred = predict(final_lasso, x.test, type = "response")[,1],
    ridge_pred = predict(final_ridge, x.test, type = "response")[,1]
    #note: ALL NEED type = "response" so we don't get log-odds in our result
  )
#Then, on the ACS data
test.df.acspreds <- test.df %>% 
  mutate(
    mle_pred = predict(lr_mle, test.df, type = "response"),
    #note: lasso and ridge get the MATRIX x.test 
    lasso_pred = predict(final_lasso, x.test, type = "response")[,1],
    ridge_pred = predict(final_ridge, x.test, type = "response")[,1]
    #note: ALL NEED type = "response" so we don't get log-odds in our result
  )
#use her code or chatgpt code to scrape the number of elderly in each puma
