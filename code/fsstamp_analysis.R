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

#Can add or remove including all squared and interaction terms
#Found that in fsstmp, this didn't improve our model much
#These terms would often help the weaker models, such as taking a
#.5 AUC of a ctree up to a .6, but often saw small decreases in our
#models that were the best. Ridge and lasso, for example,
#generally saw their AUCs decrease by about .05.
include_squared_interaction = FALSE

cps_data <- as.data.frame(cps_data)

cps_data <- cps_data %>% mutate(
  FSSTMPVALC_bin_fact = as.factor(FSSTMPVALC_bin_char)
)

###############################
#       Train Test Split      #
###############################

#Set seed and break into training and testing
RNGkind(sample.kind = "default")
set.seed(1342141)
train.idx <- sample(x=1:nrow(cps_data), size=.7*nrow(cps_data))
train.df <- cps_data[train.idx,]
test.df <- cps_data[-train.idx,]
test.df.preds <- test.df

#Make list of x and y variables that we can use later.
#These are used in a lot of places, so easy to include/take out a variable
x_vars = c("hhsize", "female", "hispanic", "black", "faminc_cleaned",
           "kids", "elderly", "education", "married", "donut")
y_var = c("FSSTMPVALC_bin")

###########################################
##  Adding all interaction/squared terms ##
###########################################

#Subset to only include relevant columns
reduced_train = train.df %>% 
  select(c(x_vars, y_var))

reduced_test = test.df %>% 
  select(c(x_vars, y_var))

#With or without interactions/squared terms
if(include_squared_interaction){
  for(i in 1:length(x_vars)){
    for (j in i:length(x_vars)){
      col1 = colnames(reduced_train)[i][1]
      col2 = colnames(reduced_train)[j][1]
      col_str = paste(col1, col2, sep="_")
      if((sapply(reduced_train[col2], class) %in% c("integer", "numeric")) & 
         (sapply(reduced_train[col1], class) %in% c("integer", "numeric"))){
        reduced_train = reduced_train %>% 
          mutate(interaction_term = (reduced_train[col1] * reduced_train[col2])[,1])
        reduced_test = reduced_test %>% 
          mutate(interaction_term = (reduced_test[col1] * reduced_test[col2])[,1])
        
        names(reduced_train)[names(reduced_train) == "interaction_term"] = col_str
        names(reduced_test)[names(reduced_test) == "interaction_term"] = col_str
      }
    } 
  }
}

###########################
#   Train Test Split      #
###########################
#Break the train test into x and y model matrices
FSSTMP.x.train <- model.matrix(FSSTMPVALC_bin ~ .
                               , data=reduced_train)[,-1]
FSSTMP.x.test <- model.matrix(FSSTMPVALC_bin ~ .
                              , data=reduced_test)[,-1]
FSSTMP.y.train <- as.vector(reduced_train$FSSTMPVALC_bin)
FSSTMP.y.test <- as.vector(reduced_test$FSSTMPVALC_bin)
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

plot(lr_mle_fsstmp_rocCurve, print.thres=TRUE, print.auc=TRUE) 
#AUC = .641 (.949, .333)

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

plot(lr_firths_fsstmp_rocCurve, print.thres=TRUE, print.auc=TRUE) 
#AUC = .887 (.775, .863)

lr_firths_fsstmp_pi_star <- coords(lr_firths_fsstmp_rocCurve, "best", ref="threshold")$threshold[1]

################
#     Lasso    #
################

lr_lasso_fsstmp_cv <- cv.glmnet(FSSTMP.x.train, FSSTMP.y.train, 
                                family=binomial(link="logit"), alpha=1, weights=train.weights)

plot(lr_lasso_fsstmp_cv)

best_lasso_lambda_fsstmp <- lr_lasso_fsstmp_cv$lambda.min

lr_lasso_coefs_fsstmp <- coef(lr_lasso_fsstmp_cv, s="lambda.min") %>% as.matrix()

lr_lasso_coefs_fsstmp

lr_lasso_fsstmp <- glmnet(FSSTMP.x.train, FSSTMP.y.train, family=binomial(link="logit"), 
                          alpha=1, lambda = best_lasso_lambda_fsstmp, weights=train.weights)

test.df.preds <- test.df.preds %>% mutate(
  lasso_fsstmp_preds = predict(lr_lasso_fsstmp, FSSTMP.x.test, type="response")[,1]
)

lasso_fsstmp_rocCurve <- roc(response = as.factor(test.df.preds$FSSTMPVALC_bin),
                             predictor =test.df.preds$lasso_fsstmp_preds,
                             levels=c("0", "1"))

plot(lasso_fsstmp_rocCurve, print.thres=TRUE, print.auc=TRUE) 
#AUC = .887 (.775, .863)

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
#For an elderly, white, male, graduated high school, lives alone, makes 11,000 yearly
#odds <- exp(-.28568 -.07412 + 0.15556578 - 1.69023936 + 0.77745406)
#prob <- odds/(odds+1)
#prob = .2466

example <- data.frame(hhsize=c(1), female=c(0), hispanic=c(0), black=c(0), faminc_cleaned=c("10000-12499"),
           kids=c(0), elderly=c(1), education=c(0), married=c(0), donut=c(as.factor(1)))

lr_ridge_fsstmp <- glmnet(FSSTMP.x.train, FSSTMP.y.train, family=binomial(link="logit"), 
                          alpha=0, lambda = best_ridge_lambda_fsstmp, weights=train.weights)

test.df.preds <- test.df.preds %>% mutate(
  ridge_fsstmp_preds = predict(lr_ridge_fsstmp, FSSTMP.x.test, type="response")[,1]
)

ridge_fsstmp_rocCurve <- roc(response = as.factor(test.df.preds$FSSTMPVALC_bin),
                             predictor = test.df.preds$ridge_fsstmp_preds,
                             levels=c("0", "1"))

plot(ridge_fsstmp_rocCurve, print.thres=TRUE, print.auc=TRUE) #AUC = .888 (.802, .837)
#Specificity: When someone is on SNAP, our model correctly predicts that they are on
#snap 80.2% of the time.
#Sensitivity: When someone is not on SNAP, our model correcly predicts that
#they aren't on SNAP 83.7% of the time. 

ridge_fsstmp_pi_star <- coords(ridge_fsstmp_rocCurve, "best", ref="threshold")$threshold[1]

##############################################
# Some interpretations of best model (Ridge) #
##############################################

#For every one increase in the number of people in the household, the odds that
#the household is on SNAP increase by about 11%.
#For every woman added to the household, the odds that the household is on SNAP increase
#by about 27%
#The odds of someone who makes between 5000-7499 per year being on SNAP is
#about 3.9 times the odds of someone who makes 
#For every elderly person added to the household, the odds that they are on SNAP decrease
#by about 9%. 
#That is, the odds for a house with one elderly person to receive SNAP 
#is .09 times the odds of a house with no elderly people holding all other factors
#constant

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
                              mtry=floor(sqrt(length(x_vars))),
                              ntree=1000,
                              importance=TRUE)

#Multiple mtry
mtry = c(1:length(x_vars))
keeps <- data.frame(m=rep(NA, length(mtry)),
                     OOB_err_rate = rep(NA, length(mtry)))

for (idx in 1:length(mtry)){
  print(paste0("Trying m = ", mtry[idx]))
  tempforest <- randomForest(FSSTMPVALC_bin_fact ~.,
                             data=rf_train,
                             ntree=1000,
                             mtry=mtry[idx])
  keeps[idx, "m"] <- mtry[idx]
  keeps[idx, "OOB_err_rate"] <- mean(predict(tempforest) != rf_train$FSSTMPVALC_bin_fact) #Estimates out of sample error
}

best_m <- keeps[order(keeps$OOB_err_rate),"m"][1]

final_forest <- randomForest(FSSTMPVALC_bin_fact ~ .,
                             data=rf_train,
                             mtry=best_m,
                             ntree=1000,
                             importance=TRUE)

pi_hat <- predict(final_forest, rf_test, type="prob")[,"Yes"]

rf_rocCurve <- roc(response=rf_test$FSSTMPVALC_bin_fact,
                predictor=pi_hat,
                levels=c("No", "Yes"))

plot(rf_rocCurve, print.thres=TRUE, print.auc=TRUE) #AUC = .774 (.755, .745)

rf_fsstmp_pi_star <- coords(rf_rocCurve, "best", ret="threshold")$threshold[1]

test.df.preds <- test.df.preds %>% mutate(
  rf_fsstmp_preds = as.factor(ifelse(pi_hat > rf_fsstmp_pi_star, "Yes", "No"))
)

#Look at variable importance, but unlikely to be helpful
varImpPlot(final_forest, type=1)

rf_vi <- as.data.frame(varImpPlot(final_forest, type=1))

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

plot(ctree_rocCurve, print.thres=TRUE, print.auc=TRUE) # AUC=.5

ctree_fsstmp_pi_star <- coords(rf_rocCurve, "best", ret="threshold")$threshold[1]

test.df.preds <- test.df.preds %>% mutate(
  ctree_fsstmp_preds = as.factor(ifelse(pi_hat > ctree_fsstmp_pi_star, "Yes", "No"))
)

#######################
## COMPARE ALL AUCS ##
######################

mle_lr_auc <- data.frame(
  Model = "MLE LR",
  Specificity = lr_mle_fsstmp_rocCurve$specificities,
  Sensitivity = lr_mle_fsstmp_rocCurve$sensitivities,
  AUC = lr_mle_fsstmp_rocCurve$auc %>% as.numeric
)

firths_lr_auc <- data.frame(
  Model = "Firths LR",
  Specificity = lr_firths_fsstmp_rocCurve$specificities,
  Sensitivity = lr_firths_fsstmp_rocCurve$sensitivities,
  AUC = lr_firths_fsstmp_rocCurve$auc %>% as.numeric
)

lasso_lr_auc <- data.frame(
  Model = "Lasso LR",
  Specificity = lasso_fsstmp_rocCurve$specificities,
  Sensitivity = lasso_fsstmp_rocCurve$sensitivities,
  AUC = lasso_fsstmp_rocCurve$auc %>% as.numeric
)

ridge_lr_auc <- data.frame(
  Model = "Ridge LR",
  Specificity = ridge_fsstmp_rocCurve$specificities,
  Sensitivity = ridge_fsstmp_rocCurve$sensitivities,
  AUC = ridge_fsstmp_rocCurve$auc %>% as.numeric
)

ctree_auc <- data.frame(
  Model = "Categorical Tree",
  Specificity = ctree_rocCurve$specificities,
  Sensitivity = ctree_rocCurve$sensitivities,
  AUC = ctree_rocCurve$auc %>% as.numeric
)

rf_auc <- data.frame(
  Model = "Random Forest",
  Specificity = rf_rocCurve$specificities,
  Sensitivity = rf_rocCurve$sensitivities,
  AUC = rf_rocCurve$auc %>% as.numeric
)

roc_data <- rbind(mle_lr_auc, firths_lr_auc, lasso_lr_auc, ridge_lr_auc, ctree_auc, rf_auc)

ggplot() +
  geom_line(aes(x = 1 - Specificity, y = Sensitivity, color = Model),data = roc_data) +
  scale_colour_brewer("Model", palette = "Paired") +
  title("AUCs of all created models")
  labs(x = "1 - Specificity", y = "Sensitivity", color = "Model") +
  theme_minimal()

################
# LOAD IN ACS  #
################
#Transform acs in the same way that we did for CPS to make the data identitcal.
acs_reduced_test = acs_data %>% 
  select(x_vars) 

#Add all squared/interaction terms to ACS data
if(include_squared_interaction){
  for(i in 1:length(x_vars)){
    for (j in i:length(x_vars)){
      col1 = colnames(acs_reduced_test)[i][1]
      col2 = colnames(acs_reduced_test)[j][1]
      col_str = paste(col1, col2, sep="_")
      if((sapply(acs_reduced_test[col2], class) %in% c("integer", "numeric")) & 
         (sapply(acs_reduced_test[col1], class) %in% c("integer", "numeric"))){
      acs_reduced_test = acs_reduced_test %>% 
        mutate(interaction_term = (acs_reduced_test[col1] * acs_reduced_test[col2])[,1])
      
      names(acs_reduced_test)[names(acs_reduced_test) == "interaction_term"] = col_str
      }
    } 
  }
}

acs_test_data <- model.matrix(~., data=acs_reduced_test)[,-1]

##################################
## MAKE PREDICTIONS ON ACS DATA ##
##################################

#We chose the ridge model to make our predictions because it had the highest 
#specificity, which means that we can be more confident that it gets to people who 
#All of our AUCs for ridge, lasso, and firths were very close
fsstmp_predictions <- predict(lr_ridge_fsstmp, acs_test_data, type="response")[,1]

acs_predicted <- acs_data %>% mutate(
  fsstmp_probabilities = fsstmp_predictions
)

fsstmp_acs_predicted_only_seniors <- acs_predicted[acs_predicted$elderly > 0,]

#Write it out to be used for combining all data. 
#Just because model creation takes so long that we didn't want to have to rerun every time
write.csv(fsstmp_acs_predicted_only_seniors, "./data/fsstmp_prediction.csv")

#What percentage of Iowa Seniors are on SNAP/FSSTMP
weighted.mean(fsstmp_acs_predicted_only_seniors$fsstmp_probabilities, fsstmp_acs_predicted_only_seniors$weight)

summary_by_PUMA <- fsstmp_acs_predicted_only_seniors %>% group_by(PUMA = as.factor(PUMA)) %>% 
  summarise(
    sample_size = sum(hhsize),
    proportion_on_assistance = weighted.mean(fsstmp_probabilities, weight),
    only_senior = sum(ifelse(elderly == hhsize, 1, 0)),
    has_senior = sum(ifelse(elderly > 0, 1, 0))
  ) %>% as.data.frame() %>% arrange(desc(proportion_on_assistance))

#https://www.geoplatform.gov/metadata/258db7ce-2581-4488-bb5e-e387b6119c7a
#Loading in the Iowa PUMA shape data
sf_data <- st_read("./data/tl_2023_19_puma20/tl_2023_19_puma20.shp")

colnames(sf_data)[colnames(sf_data) == "GEOID20"] = "PUMA"

map_data <- sf_data %>%
  left_join(summary_by_PUMA, by = "PUMA")

#Proportion of seniors that are on SNAP/Food Stamps
#The Chloropleth map was created with the help of Generative AI
ggplot(data = map_data) +
  geom_sf(aes(fill = proportion_on_assistance)) +
  scale_fill_viridis_c(option = "plasma") + 
  theme_minimal() +
  labs(title = "Proportion of Households with Seniors on SNAP/Food Stamps",
       fill = "Proportion on\nFood Stamps/SNAP")
ggsave("figures/propotion_of_seniors_predicted_fsstmp.png", width=6, height=5)

#Load in Senior Data
senior_data <- read.csv("./data/total_iowa_seniors_by_puma.csv")

senior_data <- senior_data %>% mutate("PUMA" = as.character(GEOID))

senior_data <- map_data %>% left_join(senior_data, by="PUMA")

senior_data <- senior_data %>% mutate(
  seniors_on_fsstmp = floor(proportion_on_assistance*senior_population)
) 

#Number of seniors in Iowa by PUMA
ggplot(data = senior_data) +
  geom_sf(aes(fill = senior_population)) +
  scale_fill_viridis_c(option = "plasma") + 
  theme_minimal() +
  labs(title = "Total Population of Seniors by PUMA",
       fill = "Population of\nSeniors")
ggsave("figures/number_of_seniors_by_puma.png", width=6, height=5)

#Predicted number of seniors on SNAP by PUMA
ggplot(data = senior_data) +
  geom_sf(aes(fill = seniors_on_fsstmp)) +
  scale_fill_viridis_c(option = "plasma") + 
  theme_minimal() +
  labs(title = "Predicted Number of Seniors on SNAP by PUMA",
       fill = "Predicted number\nof Seniors\non SNAP")
ggsave("figures/number_of_seniors_on_fsstmp.png", width=6, height=5)