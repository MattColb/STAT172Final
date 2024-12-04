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
library(rpart)
library(rpart.plot)
library(ggplot2)

source("./code/clean_cps.R")
source("./code/clean_acs.R") 

#Cleaning and Exploring
#summary(cps_data)
#str(cps_data)
#head(cps_data)
#FSFOODS indicates whether the household had enough to eat or enough of the kinds of foods they wanted to eat in the past twelve months.
#FSFOODS is 1 if Not Enough, 0 if Enough

##TASKS
#try regressions both with and without some meaningful interaction
#terms to look at the ways your X's could affect each other and the outcome
cps_data <- cps_data %>% mutate(
  donut = as.factor(donut)
)
#summary(cps_data$FSFOODS) FSFOODS has 1755 NA's
cps_data_f <- cps_data[!is.na(cps_data$FSFOODS),]
#summary(cps_data_f$FSFOODS) new subset without NAs has 6665 obs, compared to 8420 originally
#str(cps_data_f)

#Split the data into train/test df forms to use in lasso/ridge later
RNGkind(sample.kind = "default")
set.seed(26473)
train.idx <- sample(x = 1:nrow(cps_data_f), size = .7*nrow(cps_data_f))
train.df <- cps_data_f[train.idx,]
test.df <- cps_data_f[-train.idx,]

x_vars = c("hhsize", "female", "hispanic", "black", "faminc_cleaned",
           "kids", "elderly", "education", "married", "donut")
y_var = c("FSFOODS")

## Make all necessary matrices and vectors
fsfoods.x.train <- model.matrix(FSFOODS~hhsize + married + education + elderly +
                                  kids + black + hispanic + female+ faminc_cleaned + donut,
                                data = train.df)[,-1] 
fsfoods.x.test <- model.matrix(FSFOODS~hhsize + married + education + elderly +
                                 kids + black + hispanic + female+ faminc_cleaned + donut,
                               data = test.df)[,-1]
fsfoods.y.train <- train.df$FSFOODS %>% as.vector()
fsfoods.y.test <- test.df$FSFOODS %>% as.vector()
train.weights <- as.vector(train.df$weight)
test.weights <- as.vector(test.df$weight) #not strictly necessary, for ease of reference

#----MLE Logistic Regression----

lr_mle_fsfoods <- glm(FSFOODS ~ hhsize + married + education + elderly +
                       kids + black + hispanic + female + faminc_cleaned + donut,
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
                             kids + black + hispanic + female + faminc_cleaned + donut,
                           data=train.df,
                           weights=train.weights)

summary(lr_fmle_fsfoods)
#look at the coefficients from the MLE logistic regression
lr_fmle_fsfoods_beta <- lr_fmle_fsfoods %>% coef()

#----Lasso and Ridge with Basic X vars----

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

rf_train <- train.df %>% select(-c("FSFOODS")) %>% 
  mutate(
    FSFOODS_fact = as.factor(train.df$FSFOODS)
  )

rf_test <- test.df %>% select(-c("FSFOODS")) %>% 
  mutate(
    FSFOODS_fact = as.factor(test.df$FSFOODS)
  )

rf_init_fsfoods <- randomForest(FSFOODS_fact ~ hhsize + married + education + elderly +
                                  kids + black + hispanic + female + faminc_cleaned + donut,
                               data=rf_train,
                               mtry=floor(sqrt(length(x_vars))),
                               ntree=1000,
                               importance=TRUE)

#Multiple mtry
mtry = c(1:length(x_vars))
keeps <- data.frame(m=rep(NA, length(mtry)),
                    OOB_err_rate = rep(NA, length(mtry)))

for (idx in 1:length(mtry)){
  print(paste0("Trying m = ", mtry[idx]))
  tempforest <- randomForest(FSFOODS_fact ~hhsize + married + education + elderly +
                               kids + black + hispanic + female + faminc_cleaned + donut,
                             data=rf_train,
                             ntree=1000,
                             mtry=mtry[idx])
  keeps[idx, "m"] <- mtry[idx]
  keeps[idx, "OOB_err_rate"] <- mean(predict(tempforest) != rf_train$FSFOODS_fact) #Estimates out of sample error
}

best_m <- keeps[order(keeps$OOB_err_rate),"m"][1]

final_forest <- randomForest(FSFOODS_fact ~ hhsize + married + education + elderly +
                               kids + black + hispanic + female + faminc_cleaned + donut,
                             data=rf_train,
                             mtry=best_m,
                             ntree=1000,
                             importance=TRUE)

#plot(rf_rocCurve, print.thres=TRUE, print.auc=TRUE)

#rf_pi_star <- coords(rf_rocCurve, "best", ret="threshold")$threshold[1]
#pi star is 0.0055 idk what that means but im saving it just in case ig

#making a variable importance plot based on decrease in forest accuracy
varImpPlot(final_forest, type=1)
rf_vi <- as.data.frame(varImpPlot(final_forest, type=1))
rf_vi$Variable <- rownames(rf_vi)
rf_vi <- rf_vi %>% arrange(desc(MeanDecreaseAccuracy))

#----Random Tree----
#Create big tree, then prune
set.seed(578493768)
ctree <- rpart(FSFOODS ~ hhsize + married + education + elderly +
                 kids + black + hispanic + female+ faminc_cleaned + donut,
               data = train.df, #training data, NOT original data
               method = "class",
               control = rpart.control(cp = 0.0001, minsplit = 1))

printcp(ctree)

optimalcp <- ctree$cptable[which.min(ctree$cptable[,"xerror"]),"CP"]
#gives you the optimal complexity parameter (cp of tree with smallest xerror)
#prune to create tuned tree
ctree2 <- prune(ctree, cp = optimalcp)
rpart.plot(ctree2)

#----Compare Models' Performances----
#First, on the testing data split
test.df.cpspreds <- test.df %>% 
  mutate(
    mle_pred = predict(lr_mle_fsfoods, test.df, type = "response"),
    fmle_pred = predict(lr_fmle_fsfoods, test.df, type = "response"),
    lasso_pred = predict(fsfoods_lasso_f1, fsfoods.x.test, type = "response")[,1],
    ridge_pred = predict(fsfoods_ridge_f1, fsfoods.x.test, type = "response")[,1],
    rf_pi_hat = predict(final_forest, rf_test, type="prob")[,"1"],
    rt_pi_hat = predict(ctree2, test.df, type = "prob")[,"1"]
  )

#Fit ROC Curves on CPS
mle_rocCurve <- roc(response = as.factor(test.df.cpspreds$FSFOODS), 
                    predictor = test.df.cpspreds$mle_pred, 
                    levels = c("0", "1"))
fmle_rocCurve<- roc(response = as.factor(test.df.cpspreds$FSFOODS), 
                    predictor = test.df.cpspreds$fmle_pred, 
                    levels = c("0", "1"))
lasso_rocCurve <- roc(response = as.factor(test.df.cpspreds$FSFOODS), 
                      predictor = test.df.cpspreds$lasso_pred, 
                      levels = c("0", "1"))
ridge_rocCurve <- roc(response = as.factor(test.df.cpspreds$FSFOODS), 
                      predictor = test.df.cpspreds$ridge_pred, 
                      levels = c("0", "1"))
rf_rocCurve <- roc(response=rf_test$FSFOODS_fact,
                   predictor=test.df.cpspreds$rf_pi_hat,
                   levels=c("0", "1"))
rt_rocCurve <- roc(response = as.factor(test.df.cpspreds$FSFOODS), 
                   predictor = test.df.cpspreds$rt_pi_hat, 
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
  Specificity = fmle_rocCurve$specificities,
  Sensitivity = fmle_rocCurve$sensitivities,
  AUC = as.numeric(fmle_rocCurve$auc)
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
#make data frame of forest ROC info
rf_data <- data.frame(
  Model = "Forest",
  Specificity = rf_rocCurve$specificities,
  Sensitivity = rf_rocCurve$sensitivities,
  AUC = rf_rocCurve$auc%>% as.numeric
)
#make data frame of tree ROC info
rt_data <- data.frame(
  Model = "Tree",
  Specificity = rt_rocCurve$specificities,
  Sensitivity = rt_rocCurve$sensitivities,
  AUC = rt_rocCurve$auc%>% as.numeric
)
# Combine all the data frames
roc_data <- rbind(mle_data, fmle_data, lasso_data, ridge_data, rf_data, rt_data)


# Plot the data
ggplot() +
  geom_line(aes(x = 1 - Specificity, y = Sensitivity, color = Model),data = roc_data) +
  geom_text(data = roc_data %>% group_by(Model) %>% slice(1), 
            aes(x = 0.75, y = c(0.85, 0.75, 0.65, 0.55, 0.45, 0.35), colour = Model,
                label = paste0(Model, " AUC = ", round(AUC, 3)))) +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "1 - Specificity", y = "Sensitivity", color = "Model") +
  theme_minimal()

#Then, compare performance on the ACS data THIS NEEDS TO BE CHANGED TO REF THE ACTUAL ACS
#While we don't need to do a split, as all the ACS data is a "test" split
#we still need to create matrices so that we can use lasso and ridge on this data

#Split the data into train/test df forms to use in lasso: this is our best model
#with an AUS of 0.726
#let's get the lasso pi star, then.
lasso_fsfoods_pi_star <- coords(lasso_rocCurve, "best", ref="threshold")$threshold[1]

acs_reduced_test = acs_data %>% 
  select(all_of(x_vars)) %>% 
  mutate(
    donut = as.factor(donut)
  )

summary(acs_reduced_test)
acs_test_data <- model.matrix(~., data=acs_reduced_test)[,-1]

fsfoods_prop_preds <- predict(fsfoods_lasso_f1, acs_test_data, type="response")[,1]

acs_predicted <- acs_data %>% mutate(
  fsfoods_prediction = ifelse(fsfoods_prop_preds > lasso_fsfoods_pi_star, 1, 0),
  fsfoods_prop_preds = fsfoods_prop_preds
)
head(acs_predicted$fsfoods_prop_preds)

#BASICS WITHOUT SENIORS
#summary, not using weighted mean
summary_by_PUMA <- acs_predicted %>% group_by(PUMA = as.factor(PUMA)) %>% 
  summarise(
    sample_size = sum(hhsize),
    proportion_on_assistance = mean(fsfoods_prop_preds),
    only_senior = sum(ifelse(elderly == hhsize, 1, 0)),
    has_senior = sum(ifelse(elderly > 0, 1, 0))
  ) %>% as.data.frame() %>% arrange(desc(proportion_on_assistance))

#https://www.geoplatform.gov/metadata/258db7ce-2581-4488-bb5e-e387b6119c7a
sf_data <- st_read("./data/tl_2023_19_puma20/tl_2023_19_puma20.shp")

colnames(sf_data)[colnames(sf_data) == "GEOID20"] = "PUMA"

map_data <- sf_data %>%
  left_join(summary_by_PUMA, by = "PUMA")

#proportion of GENERAL households without enough
ggplot(data = map_data) +
  geom_sf(aes(fill = proportion_on_assistance)) +
  scale_fill_viridis_c(option = "plasma") +  # Adjust color palette as needed
  theme_minimal() +
  labs(title = "Proportion of Households without Enough Food",
       fill = "Proportion without\nEnough Food")

#Load in Senior Data
acs_predicted_only_seniors <- acs_predicted[acs_predicted$elderly > 0,]

senior_data <- read.csv("./data/iowa_seniors_by_puma.csv")

senior_data <- senior_data %>% mutate("PUMA" = as.character(GEOID))

senior_data <- map_data %>% left_join(senior_data, by="PUMA")

senior_data <- senior_data %>% mutate(
  seniors_with_fsfoods = floor(proportion_on_assistance*senior_population)) 

#No predictions, just the SENIORS in each PUMA
ggplot(data = senior_data) +
  geom_sf(aes(fill = senior_population)) +
  scale_fill_viridis_c(option = "plasma") +  # Adjust color palette as needed
  theme_minimal() +
  labs(title = "Total Population of Seniors by PUMA",
       fill = "Population of\nSeniors")

#Predicted number of SENIORS without enough food
ggplot(data = senior_data) +
  geom_sf(aes(fill = seniors_with_fsfoods)) +
  scale_fill_viridis_c(option = "plasma") +  # Adjust color palette as needed
  theme_minimal() +
  labs(title = "Predicted Seniors w/o Enough Food",
       fill = "Predicted number\nof Seniors w/o\nEnough Food")




