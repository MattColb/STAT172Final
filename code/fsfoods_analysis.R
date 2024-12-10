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
library(reshape2)

source("./code/clean_cps.R")
source("./code/clean_acs.R") 

#Cleaning and Exploring
#summary(cps_data)
#str(cps_data)
#head(cps_data)
#FSFOODS indicates whether the household had enough to eat or enough of the kinds of foods they wanted to eat in the past twelve months.
#FSFOODS is 1 if Not Enough, 0 if Enough

#Can add or remove including all squared and interaction terms, but
#Found that in fsstmp, this didn't improve our model much. So keep term = FALSE.
include_squared_interaction = FALSE

cps_data <- cps_data %>% mutate(
  donut = as.factor(donut)
)
#summary(cps_data$FSFOODS) 
#FSFOODS has 1755 NA's, remove those.
cps_data_f <- cps_data[!is.na(cps_data$FSFOODS),]
#summary(cps_data_f$FSFOODS): new subset without NAs has 6665 obs, compared to 8420 originally

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



##----Infrastructure to try all interaction/squared terms----
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



#----MLE Logistic Regression----

lr_mle_fsfoods <- glm(FSFOODS ~ hhsize + married + education + elderly +
                       kids + black + hispanic + female + faminc_cleaned + donut,
                     data=train.df,
                     family=binomial(link="logit"),
                     weights=weight
)
# Get warnings - algorithm did not converge, complete separation occurred
summary(lr_mle_fsfoods) #very high standard error on all vars, confirms complete separation

#look at the coefficients from the MLE logistic regression
lr_mle_fsfoods_beta <- lr_mle_fsfoods %>% coef()


#---Firth's Penalized Likelihood----
lr_fmle_fsfoods <- logistf(FSFOODS ~ hhsize + married + education + elderly +
                             kids + black + hispanic + female + faminc_cleaned + donut,
                           data=train.df,
                           weights=train.weights)

summary(lr_fmle_fsfoods)
#look at the coefficients from the FMLE logistic regression
lr_fmle_fsfoods_beta <- lr_fmle_fsfoods %>% coef()


#----Lasso and Ridge with Basic X vars----

#Use cross validation to get tuning info for final regression
fsfoods_lasso_cv <- cv.glmnet(fsfoods.x.train, 
                         fsfoods.y.train, 
                         family = binomial(link = "logit"),
                         alpha = 1, 
                         weights = train.weights 
                         )


fsfoods_ridge_cv <- cv.glmnet(fsfoods.x.train, 
                         fsfoods.y.train, 
                         family = binomial(link = "logit"),
                         alpha = 0,
                         weights = train.weights
                          )

#Find and extract minimizing lambda values
plot(fsfoods_lasso_cv)
plot(fsfoods_ridge_cv)

best_lasso_lambda <- fsfoods_lasso_cv$lambda.min
best_ridge_lambda <- fsfoods_ridge_cv$lambda.min

#fit final lasso + ridge models
fsfoods_lasso_f1 <- glmnet(fsfoods.x.train, fsfoods.y.train, 
                      family = binomial(link = "logit"), alpha = 1,
                      lambda = best_lasso_lambda) 
fsfoods_ridge_f1 <- glmnet(fsfoods.x.train, fsfoods.y.train, 
                      family = binomial(link = "logit"), alpha = 0,
                      lambda = best_ridge_lambda) 

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

#Then, compare performance on the ACS data suing lasso: this is our best model
#with an AUC of 0.729.
#Get the lasso pi star
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

#BASIC PREDICTION WITHOUT SENIORS
#summary using weighted mean
summary_by_PUMA <- acs_predicted %>% group_by(PUMA = as.factor(PUMA)) %>% 
  summarise(
    sample_size = sum(hhsize),
    proportion_on_assistance = weighted.mean(fsfoods_prop_preds),
    only_senior = sum(ifelse(elderly == hhsize, 1, 0)),
    has_senior = sum(ifelse(elderly > 0, 1, 0))
  ) %>% as.data.frame() %>% arrange(desc(proportion_on_assistance))
head(summary_by_PUMA)
#https://www.geoplatform.gov/metadata/258db7ce-2581-4488-bb5e-e387b6119c7a
sf_data <- st_read("./data/tl_2023_19_puma20/tl_2023_19_puma20.shp")

colnames(sf_data)[colnames(sf_data) == "GEOID20"] = "PUMA"

map_data <- sf_data %>%
  left_join(summary_by_PUMA, by = "PUMA")
map_data$NAMELSAD20
#proportion of GENERAL households without enough food
ggplot(data = map_data) +
  geom_sf(aes(fill = proportion_on_assistance)) +
  scale_fill_viridis_c(option = "plasma") +  # Adjust color palette as needed
  theme_minimal() +
  labs(title = "Proportion of Households without Enough Food",
       fill = "Proportion without\nEnough Food")

#Load in Senior Data
fsfood_acs_predicted_only_seniors <- acs_predicted[acs_predicted$elderly > 0,]

#write to csv file for further analysis
write.csv(fsfood_acs_predicted_only_seniors, "./data/fsfoods_prediction.csv")

senior_data <- read.csv("./data/total_iowa_seniors_by_puma.csv")

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
ggsave("figures/preidcted_seniors_fsfoods.png")
#Model-Specific Predictions and Plots for Analysis

plotting_data <- cps_data_f %>% mutate(
  sum_of_food_insecurity = FSFOODS + FSWROUTY + FSSTMPVALC_bin,
  has_elderly_fact = as.factor(ifelse(elderly > 0, "Has elderly", "Doesn't have elderly"))
)

ggplot(plotting_data) + 
  geom_bar(aes(x=as.factor(FSFOODS), fill=has_elderly_fact), position="fill") +
  scale_fill_brewer("Elders in household", palette="Dark2") +
  labs(y="Proportion of households", x="Do They Have Enough to Eat?") + 
  ggtitle("Proportion of households with elderly people with enough to eat")+
  title("Proportion of households with elderly people with enough to eat")+
  scale_x_discrete(labels = c("0" = "Yes", "1" = "No"))
ggsave("./figures/fsfoods_household_elderly.png", width=8, height=5)


#Use specificity from AUC graph to measure model performance
plot(lasso_rocCurve, print.thres=TRUE, print.auc=TRUE)
#gives (0.777, 0.570)
#0.57 rate of predicting correctly when people are in need
#0.78 rate of predicting correctly when people are not in need

head(acs_predicted$fsfoods_prop_preds)
weighted.mean(acs_predicted$fsfoods_prop_preds, acs_predicted$weight)
#0.1564382, this is the overall proportion of the state predicted to be without enough food.

head(fsfood_acs_predicted_only_seniors)
weighted.mean(fsfood_acs_predicted_only_seniors$fsfoods_prop_preds, fsfood_acs_predicted_only_seniors$weight)
#0.09212924, this is the overall prop of seniors predicted to be w/o enough food.

lr_lasso_coefs <- coef(fsfoods_lasso_f1, s = "lambda.min") %>% as.matrix()
#The elderly coefficient here is -0.109 - for every 1 elderly, the odds of
#having enough change by a factor of exp(-0.109) = 0.8967, or about an 11 percent decrease

#Percentage of households without enough
weighted.mean(cps_data_f$FSFOODS >0, cps_data_f$weight)
#0.2324573

#Percentage of elderly households without enough
elderly_cps <- subset(cps_data_f, cps_data_f$elderly > 0)
weighted.mean(elderly_cps$FSFOODS >0, elderly_cps$weight)
#0.1879351

#Predicting a specific individual:
#Coefficients
#black                        0.19681189*0
#elderly                     -0.10972093
#female                       0.17185938*0
#married                     -0.17834537*0
#education                   -0.28476890*0
#faminc_cleaned10000-12499    1.06139167
#donut1                      -0.41141270


-0.10972093+1.06139167-0.41141270 #0.540258
exp(0.540258)/ (1+exp(0.540258)) #0.63187 

