rm(list=ls())

library(tidyverse)
library(knitr)
library(tibble)
library(ggthemes)
library(logistf)
library(glmnet)
library(rpart)
library(rpart.plot)
library(haven)
library(pROC)
library(RColorBrewer)
library(randomForest)

source("./code/clean_cps.R")
source("./code/clean_acs.R")

include_squared_interaction = TRUE

cps_data <- as.data.frame(cps_data)

cps_data <- cps_data %>% mutate(
  FSSTMPVALC_bin_fact = as.factor(FSSTMPVALC_bin_char)
)

###############################
#       Train Test Split      #
###############################

RNGkind(sample.kind = "default")
set.seed(159159)
train.idx <- sample(x=1:nrow(cps_data), size=.7*nrow(cps_data))
train.df <- cps_data[train.idx,]
test.df <- cps_data[-train.idx,]
test.df.preds <- test.df

###########################
#   Food Stamp Analysis   #
###########################

#Create more visualizations
#Choose a model to use
#Combine FSSTMP and WROUTY to see if there are big differences
#Integrate Phuong's Cluster?
#RF mtry

###########################################
##  Adding all interaction/squared terms ##
###########################################

reduced_train = train.df %>% 
  select(c("hhsize", "female", "hispanic", "black", "faminc_cleaned",
           "kids", "elderly", "education", "married", "FSSTMPVALC_bin"))

reduced_test = test.df %>% 
  select(c("hhsize", "female", "hispanic", "black", "faminc_cleaned",
           "kids", "elderly", "education", "married", "FSSTMPVALC_bin"))

#With or without interactions/squared terms
if(include_squared_interaction){
  for(i in 1:9){
    for (j in i:9){
      col1 = colnames(reduced_train)[i][1]
      col2 = colnames(reduced_train)[j][1]
      col_str = paste(col1, col2, sep="_")
      
      reduced_train = reduced_train %>% 
        mutate(interaction_term = (reduced_train[col1] * reduced_train[col2])[,1])
      reduced_test = reduced_test %>% 
        mutate(interaction_term = (reduced_test[col1] * reduced_test[col2])[,1])
      
      names(reduced_train)[names(reduced_train) == "interaction_term"] = col_str
      names(reduced_test)[names(reduced_test) == "interaction_term"] = col_str
    } 
  }
}

###########################
#   Train Test Split      #
###########################
FSSTMP.x.train <- model.matrix(FSSTMPVALC_bin ~ .
                               , data=reduced_train)[,-1]
FSSTMP.x.test <- model.matrix(FSSTMPVALC_bin ~ .
                              , data=reduced_test)[,-1]
FSSTMP.y.train <- as.vector(train.df$FSSTMPVALC_bin)
FSSTMP.y.test <- as.vector(test.df$FSSTMPVALC_bin)
train.weights <- as.vector(train.df$weight)
test.weights <- as.vector(test.df$weight)

###########################
# MLE Logistic Regression #
###########################

lr_mle_fsstmp <- glm(FSSTMPVALC_bin ~ .,
                     data=reduced_train,
                     family=binomial(link="logit"),
                     weights=train.weights
)

summary(lr_mle_fsstmp) #Complete separation because of EXTREMELY high standard errors
lr_mle_fsstmp_beta <- lr_mle_fsstmp %>% coef()

test.df.preds <- test.df.preds %>% 
  mutate(
    lr_mle_fsstmp_preds = predict(lr_mle_fsstmp, reduced_test, type="response")
  )

lr_mle_fsstmp_rocCurve <- roc(
  response=as.factor(test.df.preds$FSSTMPVALC_bin),
  predictor= test.df.preds$lr_mle_fsstmp_preds,
  levels=c("0","1"))

plot(lr_mle_fsstmp_rocCurve, print.thres=TRUE, print.auc=TRUE) #.514 AUC (.978, .049) THIS IS REALLY BAD

lr_mle_fsstmp_pi_star <- coords(lr_mle_fsstmp_rocCurve, "best", ref="threshold")$threshold[1]

################################
# Firth's Penalized Likelihood #
################################

lr_firths_fsstmp <- logistf(FSSTMPVALC_bin ~ .,
                            data=reduced_train,
                            weights=train.weights)

summary(lr_firths_fsstmp)

lr_firths_fsstmp_beta <- lr_firths_fsstmp %>% coef()

test.df.preds <- test.df.preds %>% mutate(
  lr_firths_fsstmp_preds = predict(lr_firths_fsstmp, reduced_test, type="response")
)

lr_firths_fsstmp_rocCurve <- roc(
  response=as.factor(test.df.preds$FSSTMPVALC_bin),
  predictor= test.df.preds$lr_firths_fsstmp_preds,
  levels=c("0","1"))

plot(lr_firths_fsstmp_rocCurve, print.thres=TRUE, print.auc=TRUE) #.798 AUC (.690, .803)

lr_firths_fsstmp_pi_star <- coords(lr_firths_fsstmp_rocCurve, "best", ref="threshold")$threshold[1]

################
#     Lasso    #
################

lr_lasso_fsstmp_cv <- cv.glmnet(FSSTMP.x.train, FSSTMP.y.train, 
                                family=binomial(link="logit"), alpha=1, weights=train.weights)

plot(lr_lasso_fsstmp_cv)

best_lasso_lambda_fsstmp <- lr_lasso_fsstmp_cv$lambda.min

lr_lasso_coefs_fsstmp <- coef(lr_lasso_fsstmp_cv, s="lambda.min") %>% as.matrix()

lr_lasso_coefs_fsstmp #hispanic and hhsize both went to 0

lr_lasso_fsstmp <- glmnet(FSSTMP.x.train, FSSTMP.y.train, family=binomial(link="logit"), 
                          alpha=1, lambda = best_lasso_lambda_fsstmp, weights=train.weights)

test.df.preds <- test.df.preds %>% mutate(
  lasso_fsstmp_preds = predict(lr_lasso_fsstmp, FSSTMP.x.test, type="response")[,1]
)

lasso_fsstmp_rocCurve <- roc(response = as.factor(test.df.preds$FSSTMPVALC_bin),
                             predictor =test.df.preds$lasso_fsstmp_preds,
                             levels=c("0", "1"))

plot(lasso_fsstmp_rocCurve, print.thres=TRUE, print.auc=TRUE) #Better at AUC = .798, (.681, .810)

lasso_fsstmp_pi_star <- coords(lasso_fsstmp_rocCurve, "best", ref="threshold")$threshold[1]

#############
#   Ridge   #
#############

lr_ridge_fsstmp_cv <- cv.glmnet(FSSTMP.x.train, FSSTMP.y.train, 
                                family=binomial(link="logit"), alpha=0, weights=train.weights)

plot(lr_ridge_fsstmp_cv)

best_ridge_lambda_fsstmp <- lr_lasso_fsstmp_cv$lambda.min

lr_ridge_coefs_fsstmp <- coef(lr_ridge_fsstmp_cv, s="lambda.min") %>% as.matrix()

lr_ridge_coefs_fsstmp

lr_ridge_fsstmp <- glmnet(FSSTMP.x.train, FSSTMP.y.train, family=binomial(link="logit"), 
                          alpha=0, lambda = best_ridge_lambda_fsstmp, weights=train.weights)

test.df.preds <- test.df.preds %>% mutate(
  ridge_fsstmp_preds = predict(lr_ridge_fsstmp, FSSTMP.x.test, type="response")[,1]
)

ridge_fsstmp_rocCurve <- roc(response = as.factor(test.df.preds$FSSTMPVALC_bin),
                             predictor =test.df.preds$ridge_fsstmp_preds,
                             levels=c("0", "1"))

plot(ridge_fsstmp_rocCurve, print.thres=TRUE, print.auc=TRUE) #.800 (.684,.810)

ridge_fsstmp_pi_star <- coords(ridge_fsstmp_rocCurve, "best", ref="threshold")$threshold[1]

#########################
#     Random Forest     #
#########################

rf_train <- reduced_train %>% select(-c("FSSTMPVALC_bin")) %>% 
  mutate(
    FSSTMPVALC_bin_fact = train.df$FSSTMPVALC_bin_fact
  )

rf_test <- reduced_test %>% select(-c("FSSTMPVALC_bin")) %>% 
  mutate(
    FSSTMPVALC_bin_fact = test.df$FSSTMPVALC_bin_fact
  )

rf_init_fsstmp <- randomForest(FSSTMPVALC_bin_fact ~ .,
                              data=rf_train,
                              mtry=3,
                              ntree=1000,
                              importance=TRUE)

#Multiple mtry

pi_hat <- predict(rf_init_fsstmp, rf_test, type="prob")[,"Yes"]

rf_rocCurve <- roc(response=rf_test$FSSTMPVALC_bin_fact,
                predictor=pi_hat,
                levels=c("No", "Yes"))

plot(rf_rocCurve, print.thres=TRUE, print.auc=TRUE)

rf_fsstmp_pi_star <- coords(rf_rocCurve, "best", ret="threshold")$threshold[1]

test.df.preds <- test.df.preds %>% mutate(
  rf_fsstmp_preds = as.factor(ifelse(pi_hat > rf_fsstmp_pi_star, "Yes", "No"))
)

varImpPlot(rf_init_fsstmp, type=1)

rf_vi <- as.data.frame(varImpPlot(rf_init_fsstmp, type=1))

rf_vi$Variable <- rownames(rf_vi)

rf_vi <- rf_vi %>% arrange(desc(MeanDecreaseAccuracy))

#####################
# Tree              #
#####################

ctree <- rpart(FSSTMPVALC_bin_fact ~ .,
               data=rf_train,
               method="class",
               control=rpart.control(cp=.0001, minsplit=1))

optimalcp <- ctree$cptable[which.min(ctree$cptable[,"xerror"]), "CP"]

ctree_optimal <- prune(ctree, cp=optimalcp)

rpart.plot(ctree_optimal)

pi_hat <- predict(ctree_optimal, rf_test, type="prob")[,"Yes"]

ctree_rocCurve <- roc(response=rf_test$FSSTMPVALC_bin_fact,
                   predictor=pi_hat,
                   levels=c("No", "Yes"))

plot(ctree_rocCurve, print.thres=TRUE, print.auc=TRUE)

ctree_fsstmp_pi_star <- coords(rf_rocCurve, "best", ret="threshold")$threshold[1]

test.df.preds <- test.df.preds %>% mutate(
  ctree_fsstmp_preds = as.factor(ifelse(pi_hat > ctree_fsstmp_pi_star, "Yes", "No"))
)

###################################
# PREDICTING FSWROUTY AND FSSTMP  #
###################################

################################
#   MAKING PREDICTIONS ON ACS  #
################################

#Add all squared/interaction terms to ACS data

acs_reduced_test = acs_data %>% 
  select(c("hhsize", "female", "hispanic", "black",
           "kids", "elderly", "education", "married", "faminc_cleaned"))

if(include_squared_interaction){
  for(i in 1:9){
    for (j in i:9){
      col1 = colnames(acs_reduced_test)[i][1]
      col2 = colnames(acs_reduced_test)[j][1]
      col_str = paste(col1, col2, sep="_")
      
      acs_reduced_test = acs_reduced_test %>% 
        mutate(interaction_term = (acs_reduced_test[col1] * acs_reduced_test[col2])[,1])
      
      names(acs_reduced_test)[names(acs_reduced_test) == "interaction_term"] = col_str
    } 
  }
}

acs_test_data <- model.matrix(~., data=acs_reduced_test)[,-1]

fsstmp_predictions <- predict(lr_lasso_fsstmp, acs_test_data, type="response")[,1]

acs_predicted <- acs_data %>% mutate(
  fsstmp_prediction = ifelse(fsstmp_predictions > lasso_fsstmp_pi_star, "On Assistance", "Not On Assistance")
)

#How does this adjust with the weights
summary_by_PUMA <- acs_predicted %>% group_by(PUMA = as.factor(PUMA)) %>% 
  summarise(
    sample_size = sum(hhsize),
    total_weights = sum(weight),
    total_weights_by_sample = sum(weight *hhsize),
    people_on_assistance = sum(ifelse(fsstmp_prediction == "On Assistance", 1, 0)),
    people_on_assistance_weighted = sum(ifelse(fsstmp_prediction == "On Assistance", 1, 0)*weight),
    proportion_on_assistance = people_on_assistance/sample_size,
    only_senior = sum(ifelse(elderly == hhsize, 1, 0)),
    proportion_only_senior = only_senior/sample_size,
    has_senior = sum(ifelse(elderly > 0, 1, 0)),
    proportion_has_senior = has_senior/sample_size
  ) %>% as.data.frame() %>% arrange(desc(proportion_on_assistance))


#https://www.geoplatform.gov/metadata/258db7ce-2581-4488-bb5e-e387b6119c7a
sf_data <- st_read("./data/tl_2023_19_puma20/tl_2023_19_puma20.shp")

colnames(sf_data)[colnames(sf_data) == "GEOID20"] = "PUMA"

map_data <- sf_data %>%
  left_join(summary_by_PUMA, by = "PUMA")

ggplot(data = map_data) +
  geom_sf(aes(fill = proportion_on_assistance)) +
  scale_fill_viridis_c(option = "plasma") +  # Adjust color palette as needed
  theme_minimal() +
  labs(title = "Proportion of Households on SNAP/Food Stamps",
       fill = "Proportion on\nFood Stamps/SNAP")

ggplot(data = map_data) +
  geom_sf(aes(fill = proportion_has_senior)) +
  scale_fill_viridis_c(option = "plasma") +  # Adjust color palette as needed
  theme_minimal() +
  labs(title = "Proportion of Households with Senior",
       fill = "Proportion of\nHouseholds with\nSeniors")

ggplot(data = map_data) +
  geom_sf(aes(fill = total_weights_by_sample)) +
  scale_fill_viridis_c(option = "plasma") +  # Adjust color palette as needed
  theme_minimal() +
  labs(title = "Population By PUMA",
       fill = "PUMA Population")

#Proportion of ACS Senior households
