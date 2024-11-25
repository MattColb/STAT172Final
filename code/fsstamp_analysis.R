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

#Things to think about/do
#Fit a random forest
#Fit a cluster?
#What are the weights doing?
#Think about what I could do with NA values
#Make plots/clean up ROC plots

###########################
#   Train Test Split      #
###########################
FSSTMP.x.train <- model.matrix(FSSTMPVALC_bin ~ hhsize + married + education + elderly +
                                 kids + black + hispanic + female
                               , data=train.df)[,-1]
FSSTMP.x.test <- model.matrix(FSSTMPVALC_bin ~ hhsize + married + education + elderly +
                                kids + black + hispanic + female
                              , data=test.df)[,-1]
FSSTMP.y.train <- as.vector(train.df$FSSTMPVALC_bin)
FSSTMP.y.test <- as.vector(test.df$FSSTMPVALC_bin)
train.weights <- as.vector(train.df$weight)
test.weights <- as.vector(test.df$weight)

###########################
# MLE Logistic Regression #
###########################

lr_mle_fsstmp <- glm(FSSTMPVALC_bin ~ hhsize + married + education + elderly +
                       kids + black + hispanic + female,
                     data=train.df,
                     family=binomial(link="logit"),
                     weights=weight
)

summary(lr_mle_fsstmp) #Complete separation because of EXTREMELY high standard errors
lr_mle_fsstmp_beta <- lr_mle_fsstmp %>% coef()

test.df.preds <- test.df.preds %>% 
  mutate(
    lr_mle_fsstmp_preds = predict(lr_mle_fsstmp, test.df, type="response")
  )

lr_mle_fsstmp_rocCurve <- roc(
  response=as.factor(test.df.preds$FSSTMPVALC_bin),
  predictor= test.df.preds$lr_mle_fsstmp_preds,
  levels=c("0","1"))

plot(lr_mle_fsstmp_rocCurve, print.thres=TRUE, print.auc=TRUE) #.514 AUC (.978, .049) THIS IS REALLY BAD

lr_mle_fsstmp_pi_star <- coords(lr_mle_fsstmp_rocCurve, "best", ref="threshold")$threshold[1]

#Since all variables are seen as significant because of complete separation

################################
# Firth's Penalized Likelihood #
################################

lr_firths_fsstmp <- logistf(FSSTMPVALC_bin ~ hhsize + married + education + elderly +
                              kids + black + hispanic + female,
                            data=train.df,
                            weights=weight)

summary(lr_firths_fsstmp)

lr_firths_fsstmp_beta <- lr_firths_fsstmp %>% coef()

test.df.preds <- test.df.preds %>% mutate(
  lr_firths_fsstmp_preds = predict(lr_firths_fsstmp, test.df, type="response")
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

saveRDS(lr_lasso_fsstmp, "./models/fsstmp/lasso_model.RDS")

saveRDS(lasso_fsstmp_pi_star, "./models/fsstmp/lasso_pi_star.RDS")

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

rf_init_fsstmp <- randomForest(FSSTMPVALC_bin_fact ~ hhsize + married + education + elderly +
                              kids + black + hispanic + female,
                              data=train.df,
                              mtry=3,
                              ntree=1000,
                              importance=TRUE)

#Multiple mtry

pi_hat <- predict(rf_init_fsstmp, test.df, type="prob")[,"Yes"]
rf_rocCurve <- roc(response=test.df$FSSTMPVALC_bin_fact,
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

model_building = data.frame(number_of_variables=rep(NA, nrow(rf_vi)), BIC=rep(NA, nrow(rf_vi)))
previous_BIC = 100000000

i <- 1
while (i == 1){
  i <- 0
}

#Build with best model so far from there



##########################
#       Clustering       #
##########################

#kmeans gives 10 different groups which we use in a random forest as categorical

#############################
# Visualizing Relationships #
#############################

#Proportion of food stamps for each elderly person in household
ggplot(data=cps_data) +
  geom_histogram(aes(x=elderly, fill=FSSTMPVALC_bin_char), binwidth = 1, position="fill") +
  scale_fill_brewer(palette="Dark2")

ggplot(data=cps_data) +
  geom_histogram(aes(x=married, fill=FSSTMPVALC_bin_char), binwidth=1, position="fill") +
  scale_fill_brewer(palette="Dark2")

#Questions: Writing model objects