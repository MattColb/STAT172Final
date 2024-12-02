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
#source("./code/clean_acs.R") shouldn't I only have the cps source?

#Cleaning and Exploring
summary(cps_data)
str(cps_data)
head(cps_data)
#is there a point to making this a character and then a factor? 
##YES because random forest won't work on numeric, it works on factors
##because then it sees it as a classification, not regression, problem.
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

x_vars = c("hhsize", "female", "hispanic", "black", "faminc_cleaned",
           "kids", "elderly", "education", "married", "donut")
y_var = c("FSFOODS")


#----MLE Logistic Regression----

lr_mle_fsfoods <- glm(FSFOODS ~ hhsize + married + education + elderly +
                       kids + black + hispanic + female + faminc_cleaned,
                     data=train.df,
                     family=binomial(link="logit"),
                     weights=weight
)
# Get warnings - algorithm did not converge, complete separation occurred
summary(lr_mle_fsfoods) #grossly high standard error on all vars, confirms complete separation

#look at the coefficients from the MLE logistic regression
lr_mle_fsfoods_beta <- lr_mle_fsfoods %>% coef()


#---Firth's Penalized Likelihood----
lr_fmle_fsfoods <- logistf(FSFOODS ~ hhsize + married + education + elderly +
                             kids + black + hispanic + female + faminc_cleaned,
                           data=train.df,
                           weights=train.weights)

summary(lr_fmle_fsfoods)
#look at the coefficients from the MLE logistic regression
lr_fmle_fsfoods_beta <- lr_fmle_fsfoods %>% coef()

#----Lasso and Ridge with Basic X vars----
## Make all necessary matrices and vectors
fsfoods.x.train <- model.matrix(FSFOODS~hhsize + married + education + elderly +
                                  kids + black + hispanic + female+ faminc_cleaned,
                                data = train.df)[,-1] 
fsfoods.x.test <- model.matrix(FSFOODS~hhsize + married + education + elderly +
                                 kids + black + hispanic + female+ faminc_cleaned,
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
#lr_lasso_coefs_fsstmp <- coef(lr_lasso_fsstmp_cv, s="lambda.min") %>% as.matrix()
#lr_lasso_coefs_fsstmp, to see what vars go to zero

fsfoods_ridge_cv <- cv.glmnet(fsfoods.x.train, #MATRIX without our Y COLUMN
                         fsfoods.y.train, #VECTOR - our Y COLUMN
                         family = binomial(link = "logit"),
                         alpha = 0,
                         weights = train.weights#1 for lasso, 0 for ridge
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
    mle_pred = predict(lr_mle_fsfoods, test.df, type = "response"),
    fmle_pred = predict(lr_fmle_fsfoods, test.df, type = "response"),
    #note: lasso and ridge get the MATRIX x.test 
    lasso_pred = predict(fsfoods_lasso_f1, fsfoods.x.test, type = "response")[,1],
    ridge_pred = predict(fsfoods_ridge_f1, fsfoods.x.test, type = "response")[,1]
    #note: ALL NEED type = "response" so we don't get log-odds in our result
  )
#Then, on the acs data THIS NEEDS TO BE CHANGED TO REF THE ACTUAL ACS
test.df.acspreds <- test.df %>% 
  mutate(
    mle_pred = predict(lr_mle_fsfoods, test.df, type = "response"),
    #note: lasso and ridge get the MATRIX x.test 
    lasso_pred = predict(fsfoods_lasso_f1, fsfoods.x.test, type = "response")[,1],
    ridge_pred = predict(fsfoods_ridge_f1, fsfoods.x.test, type = "response")[,1]
    #note: ALL NEED type = "response" so we don't get log-odds in our result
  )

#Fit ROC Curves on CPS
mle_rocCurve <- roc(response = as.factor(test.df.cpspreds$FSFOODS), #TRUTH
                    predictor = test.df.cpspreds$mle_pred, #predicted probabilities of MLE
                    levels = c("0", "1"))
fmle_rocCurve<- roc(response = as.factor(test.df.cpspreds$FSFOODS), #TRUTH
                    predictor = test.df.cpspreds$fmle_pred, #predicted probabilities of MLE
                    levels = c("0", "1"))
lasso_rocCurve <- roc(response = as.factor(test.df.cpspreds$FSFOODS), #TRUTH
                      predictor = test.df.cpspreds$lasso_pred, #predicted probabilities of MLE
                      levels = c("0", "1"))
ridge_rocCurve <- roc(response = as.factor(test.df.cpspreds$FSFOODS), #TRUTH
                      predictor = test.df.cpspreds$ridge_pred, #predicted probabilities of MLE
                      levels = c("0", "1"))
#PLOT CPS PREDICTIONS
#make data frame of MLE ROC info 
mle_data <- data.frame(
  Model = "MLE",
  Specificity = mle_rocCurve$specificities,
  Sensitivity = mle_rocCurve$sensitivities,
  AUC = as.numeric(mle_rocCurve$auc)
)
#make data frame of Firth's ROC info
fmle_data<- data.frame(
  Model = "Firth's",
  Specificity = mle_rocCurve$specificities,
  Sensitivity = mle_rocCurve$sensitivities,
  AUC = as.numeric(mle_rocCurve$auc)
)
#make data frame of lasso ROC info
lasso_data <- data.frame(
  Model = "Lasso",
  Specificity = lasso_rocCurve$specificities,
  Sensitivity = lasso_rocCurve$sensitivities,
  AUC = lasso_rocCurve$auc %>% as.numeric
)
#make data frame of ridge ROC info
ridge_data <- data.frame(
  Model = "Ridge",
  Specificity = ridge_rocCurve$specificities,
  Sensitivity = ridge_rocCurve$sensitivities,
  AUC = ridge_rocCurve$auc%>% as.numeric
)

# Combine all the data frames
roc_data <- rbind(mle_data, fmle_data, lasso_data, ridge_data)


# Plot the data - all your model curves are on the same plot now!
ggplot() +
  geom_line(aes(x = 1 - Specificity, y = Sensitivity, color = Model),data = roc_data) +
  geom_text(data = roc_data %>% group_by(Model) %>% slice(1), 
            aes(x = 0.75, y = c(0.75, 0.65, 0.55, 0.45), colour = Model,
                label = paste0(Model, " AUC = ", round(AUC, 3)))) +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "1 - Specificity", y = "Sensitivity", color = "Model") +
  theme_minimal()
