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
library(ggplot2)
library(reshape2)
library(randomForest)
library(logistf)
library(RColorBrewer)
library(rpart)
library(rpart.plot)

source("code/clean_cps.R")

############ Clean Y-Variable ############

# change y-variable into factor
cps_data$FSWROUTY_binchar <- ifelse(cps_data$FSWROUTY == 0, "No", "Yes")

# handle missing values
cps_data <- cps_data[!is.na(cps_data$FSWROUTY_binchar),]

# cps_data <- cps_data %>% mutate(FSWROUTY_bin = as.factor(FSWROUTY_binchar))

cps_data <- cps_data %>% mutate(FSWROUTY_bin = ifelse(FSWROUTY_binchar == "No", 0, 1))

# summary(cps_data)

############ Train Test Split ############

# only include certain variables
model_data <- cps_data[, c("FSWROUTY_bin", "weight", "hhsize",  
                           "female", "hispanic", "black", "kids", "elderly",
                           "education", "married", "faminc_cleaned")]

# splitting training and testing
RNGkind(sample.kind = "default")
set.seed(159159)
train.idx <- sample(x=1:nrow(model_data), size=.7*nrow(model_data))
train.df <- model_data[train.idx,]
test.df <- model_data[-train.idx,]

# only x variables
x_vars = c("hhsize", "female", "hispanic", "black", "kids", "elderly",
           "education", "married", "faminc_cleaned")

################ MLE Logistic Regression #########################

fswrouty_mle <- glm(FSWROUTY_bin ~ hhsize + married + education + elderly + kids 
                    + black + hispanic + female + faminc_cleaned,
                    data = train.df,
                    family = binomial(link = 'logit'),
                    weights = weight)

summary(fswrouty_mle)

#all variables are seen as significant because of complete separation
test_preds <- test.df %>% 
  mutate(
    mle_fswrouty_prob = predict(fswrouty_mle, test.df , type="response")
  )

mle_fswrouty_rocCurve <- roc(
  response=as.factor(test_preds$FSWROUTY_bin),
  predictor= test_preds$mle_fswrouty_prob,
  levels=c("0","1"))

plot(mle_fswrouty_rocCurve, print.thres=TRUE, print.auc=TRUE) 

################## FIRTH'S PENALIZED LIKELIHOOD ###################

firths_fswrouty <- logistf(FSWROUTY_bin ~ hhsize + married + education + elderly +
                              kids + black + hispanic + female + faminc_cleaned,
                            data=train.df,
                            weights=weight)

summary(firths_fswrouty)

firths_fswrouty_beta <- firths_fswrouty %>% coef()

test_preds <- test.df %>% 
  mutate(
    firths_fswrouty_prob = predict(firths_fswrouty, test.df , type="response")
  )

firths_fswrouty_rocCurve <- roc(
  response=test_preds$FSWROUTY_bin,
  predictor= test_preds$firths_fswrouty_prob,
  levels=c("0","1"))

plot(firths_fswrouty_rocCurve, print.thres=TRUE, print.auc=TRUE) 

############# RANDOM FOREST ###################
# Multiple mtry
mtry = c(1:length(x_vars))
keeps <- data.frame(m=rep(NA, length(mtry)),
                    OOB_err_rate = rep(NA, length(mtry)))

for (idx in 1:length(mtry)){
  print(paste0("Trying m = ", mtry[idx]))
  tempforest <- randomForest(as.factor(FSWROUTY_bin) ~ hhsize + married + education 
                             + elderly + kids + black + hispanic + female 
                             + faminc_cleaned,
                             data= train.df,
                             ntree=1000,
                             mtry=mtry[idx])
  keeps[idx, "m"] <- mtry[idx]
  keeps[idx, "OOB_err_rate"] <- mean(predict(tempforest) != train.df$FSWROUTY_bin) #Estimates out of sample error
}

best_m <- keeps[order(keeps$OOB_err_rate),"m"][1]

ggplot(keeps, aes(x = m, y = OOB_err_rate)) +
  geom_line() +
  geom_point() +
  labs(title = "OOB Error Rate vs mtry",
       x = "mtry",
       y = "OOB Error Rate") +
  theme_minimal()

# final RF model
rf_fswrouty <- randomForest(as.factor(FSWROUTY_bin) ~ hhsize + married + education 
                            + elderly + kids + black + hispanic + female 
                            + faminc_cleaned,
                                 data = train.df,
                                 ntree = 1000,
                                 mtry = 2,
                                 importance = TRUE)

# Validate model as predictive tool

pi_hat <- predict(rf_fswrouty, test.df, type = "prob")[, "1"] #Choose positive event column

rf_rocCurve <- roc(response = test.df$FSWROUTY_bin,
                predictor = pi_hat,
                levels = c("0", "1"))

plot(rf_rocCurve, print.thres = TRUE, print.ouc = TRUE)

# variable importance plot based on decrease in forest accuracy
varImpPlot(rf_fswrouty, type=1)
rf_vi <- as.data.frame(varImpPlot(rf_fswrouty, type=1))
rf_vi$Variable <- rownames(rf_vi)
rf_vi <- rf_vi %>% arrange(desc(MeanDecreaseAccuracy))

################ TREE #####################
ctree <- rpart(FSWROUTY_bin ~ hhsize + married + education + elderly +
                 kids + black + hispanic + female + faminc_cleaned,
               data= train.df,
               weights = weight,
               method="class",
               control=rpart.control(cp=.0001, minsplit=1))

# the optimal complexity parameter 
optimalcp <- ctree$cptable[which.min(ctree$cptable[,"xerror"]), "CP"]

# prune tree
ctree_optimal <- prune(ctree, cp=optimalcp)

rpart.plot(ctree_optimal)

pi_hat <- predict(ctree_optimal, test.df, type="prob")[,"1"]

# rocCurve
ctree_rocCurve <- roc(response=test.df$FSWROUTY_bin,
                      predictor=pi_hat,
                      levels=c("0", "1"))

plot(ctree_rocCurve, print.thres=TRUE, print.auc=TRUE)

###########################################

# train and test of x and y variables

x_train <- model.matrix(FSWROUTY_bin ~ hhsize + married + education + elderly + kids 
                  + black + hispanic + female + faminc_cleaned
                  , data=train.df)[,-1]
x_test <- model.matrix(FSWROUTY_bin ~ hhsize + married + education + elderly + kids 
                       + black + hispanic + female + faminc_cleaned
                       , data=test.df)[,-1]

y_test <- as.vector(test.df$FSWROUTY_bin)
y_train <- as.vector(train.df$FSWROUTY_bin)

########### LASSO ##################

fswrouty_lasso <- cv.glmnet(x_train, y_train, family=binomial(link="logit"), alpha = 1)
plot(fswrouty_lasso)
best_lambda_lasso <- fswrouty_lasso$lambda.min

coef(fswrouty_lasso, s="lambda.min") %>% as.matrix()

lasso_model <- glmnet(x_train, y_train, family=binomial(link="logit"), 
                      alpha = 1, 
                      lambda = best_lambda_lasso,
                      weights = as.vector(train.df$weight)
                      )
# predict probability on the test set
test_preds <- test.df %>% 
  mutate (
    lasso_prob = predict(lasso_model, x_test, type = "response"))

lasso_rocCurve <- roc(response = as.factor(test_preds$FSWROUTY_bin),
                      predictor = test_preds$lasso_prob,
                      levels = c("0", "1"))

plot(lasso_rocCurve, print.thres=TRUE, print.auc=TRUE)

########### RIDGE ##############

fswrouty_ridge <- cv.glmnet(x_train, y_train, family=binomial(link="logit"), alpha = 0)
plot(fswrouty_ridge)
best_lambda_ridge <- fswrouty_ridge$lambda.min
coef(fswrouty_ridge, s="lambda.min") %>% as.matrix()

ridge_model <- glmnet(x_train, y_train, family=binomial(link="logit"), 
                                     alpha = 0, 
                                     lambda = best_lambda_lasso,
                                     weights = as.vector(train.df$weight))

# predict probability on the test set
test_preds <- test.df %>% 
  mutate (
    ridge_prob = predict(ridge_model, x_test, type = "response"))

ridge_rocCurve <- roc(response = as.factor(test_preds$FSWROUTY_bin),
                      predictor = test_preds$ridge_prob,
                      levels = c("0", "1"))

plot(ridge_rocCurve, print.thres=TRUE, print.auc=TRUE)

########## COMBINE ROC CURVE ################

#make data frame of Firths ROC info
firths_data_fswrouty <- data.frame(
  Model = "Firths",
  Specificity = firths_fswrouty_rocCurve$specificities,
  Sensitivity = firths_fswrouty_rocCurve$sensitivities,
  AUC = as.numeric(firths_fswrouty_rocCurve$auc)
)

#make data frame of lasso ROC info
lasso_data_fswrouty <- data.frame(
  Model = "Lasso",
  Specificity = lasso_rocCurve$specificities,
  Sensitivity = lasso_rocCurve$sensitivities,
  AUC = lasso_rocCurve$auc %>% as.numeric
)
#make data frame of ridge ROC info
ridge_data_fswrouty <- data.frame(
  Model = "Ridge",
  Specificity = ridge_rocCurve$specificities,
  Sensitivity = ridge_rocCurve$sensitivities,
  AUC = ridge_rocCurve$auc%>% as.numeric
)
# make data frame of ctree ROC info
ctree_fswrouty <- data.frame(
  Model = "Categorical Tree",
  Specificity = ctree_rocCurve$specificities,
  Sensitivity = ctree_rocCurve$sensitivities,
  AUC = ctree_rocCurve$auc %>% as.numeric
)

rf_fswrouty <- data.frame(
  Model = "Random Forest",
  Specificity = rf_rocCurve$specificities,
  Sensitivity = rf_rocCurve$sensitivities,
  AUC = rf_rocCurve$auc %>% as.numeric
)

# Combine all the data frames
roc_data <- rbind(firths_data_fswrouty, lasso_data_fswrouty, 
                  ridge_data_fswrouty, rf_fswrouty, ctree_fswrouty)


# Plot the data
ggplot() +
  geom_line(aes(x = 1 - Specificity, y = Sensitivity, color = Model),data = roc_data) +
  geom_text(data = roc_data %>% group_by(Model) %>% slice(1), 
            aes(x = 0.75, y = c(0.75, 0.65, 0.55, 0.45, 0.35), colour = Model,
                label = paste0(Model, " AUC = ", round(AUC, 5)))) +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "1 - Specificity", y = "Sensitivity", color = "Model") +
  theme_minimal()

##################################################################

############# ACS ##############

# load acs data
source("./code/clean_acs.R")

#Transform acs in the same way that we did for CPS to make the data identitcal.
acs_reduced_test = acs_data %>% 
  select(x_vars) 

acs_test_data <- model.matrix(~., data=acs_reduced_test)[,-1]

## MAKE PREDICTIONS ON ACS DATA ##

fswrouty_predictions <- predict(lasso_model, acs_test_data, type="response")[,1]

# Check the first few predictions
head(fswrouty_predictions)

acs_predicted <- acs_data %>% mutate(
  fswrouty_probs = fswrouty_predictions
)

# predict on only seniors
acs_predicted_only_seniors <- acs_predicted[acs_predicted$elderly > 0,]

# percentage of senior having food anxiety
weighted.mean(acs_predicted_only_seniors$fswrouty_probs, acs_predicted_only_seniors$weight)

summary_by_PUMA <- acs_predicted_only_seniors %>% group_by(PUMA = as.factor(PUMA)) %>% 
  summarise(
    sample_size = sum(hhsize),
    proportion_on_assistance = weighted.mean(fswrouty_probs, weight),
    only_senior = sum(ifelse(elderly == hhsize, 1, 0)), #only seniors in the house
    has_senior = sum(ifelse(elderly > 0, 1, 0)), #house has senior
    one_senior = sum(ifelse(elderly == hhsize & hhsize == 1, 1, 0)) #only 1 senior in the house
  ) %>% as.data.frame() %>% arrange(desc(proportion_on_assistance))

#Loading in the Iowa PUMA shape data
#https://www.geoplatform.gov/metadata/258db7ce-2581-4488-bb5e-e387b6119c7a
sf_data <- st_read("./data/tl_2023_19_puma20/tl_2023_19_puma20.shp")

colnames(sf_data)[colnames(sf_data) == "GEOID20"] = "PUMA"

map_data <- sf_data %>%
  left_join(summary_by_PUMA, by = "PUMA")

#Proportion of seniors that have food anxiety
ggplot(data = map_data) +
  geom_sf(aes(fill = proportion_on_assistance)) +
  scale_fill_viridis_c(option = "plasma") +  # Adjust color palette as needed
  theme_minimal() +
  labs(title = "Proportion of Households",
       fill = "Proportion on\nFood Anxiety")
ggsave("figures/fswrouty_proportion_of_households.png", width=6, height=5)

#Load in Senior Data
senior_data <- read.csv("./data/total_iowa_seniors_by_puma.csv")

senior_data <- senior_data %>% mutate("PUMA" = as.character(GEOID))

senior_data <- map_data %>% left_join(senior_data, by="PUMA")

senior_data <- senior_data %>% mutate(
  seniors_with_fswrouty = floor(proportion_on_assistance*senior_population)
) 

# Map of seniors in Iowa by PUMA
ggplot(data = senior_data) +
  geom_sf(aes(fill = senior_population)) +
  scale_fill_viridis_c(option = "plasma") +  # Adjust color palette as needed
  theme_minimal() +
  labs(title = "Total Population of Seniors by PUMA",
       fill = "Population of\nSeniors")

# Predicted number of seniors with food anxiety
ggplot(data = senior_data) +
  geom_sf(aes(fill = seniors_with_fswrouty)) +
  scale_fill_viridis_c(option = "plasma") +  # Adjust color palette as needed
  theme_minimal() +
  labs(title = "Predicted Seniors w Food Anxiety by PUMA",
       fill = "Predicted number\nof Seniors\nwith Food Anxiety")
ggsave("figures/proportion_of_seniors_with_food_anxiety.png", width=6, height=5)


############# Prediction on Single Senior Household ###################

single_senior_data <- senior_data %>% mutate(
  single_senior_with_fswrouty = floor(proportion_on_assistance*one_senior)
) 

# population of single senior households
ggplot(data = senior_data) +
  geom_sf(aes(fill = one_senior)) +
  scale_fill_viridis_c(option = "plasma") +  # Adjust color palette as needed
  theme_minimal() +
  labs(title = "Total Population of Single-Senior Households by PUMA",
       fill = "Single Senior\nHouseholds")
ggsave("figures/acs_total_senior_single_households.png", width=6, height=5)

# Predicted number of single senior household with food anxiety
ggplot(data = single_senior_data) +
  geom_sf(aes(fill = single_senior_with_fswrouty)) +
  scale_fill_viridis_c(option = "plasma") +  # Adjust color palette as needed
  theme_minimal() +
  labs(title = "Predicted Food Anxiety in Single-Senior Households by PUMA",
       fill = "Predicted Single\nSenior w Food Anxiety")
ggsave("figures/acs_predicted_food_anxiety_single_senior_households.png", width=6, height=5)

######### Slides Interpretation

weighted.mean(acs_predicted_only_seniors$fswrouty_probs, acs_predicted_only_seniors$weight)
# 0.4790557, this is the overall prop of seniors predicted to have food anxiety.

coef(fswrouty_ridge, s="lambda.min") %>% as.matrix()
#The elderly coefficient here is -0.20418350 for every 1 elderly, the odds of
#having food anxiety decrease by 18.47%.

########## MORE VISUALIZATION ############

### visualization of household with seniors having food anxiety ###

# Calculate total households with seniors
total_households_with_seniors <- sum(summary_by_PUMA$has_senior)

# Calculate households with seniors having food anxiety
households_with_seniors_anxiety <- model_data %>%
  filter(FSWROUTY_bin == 1 & elderly > 0) %>%
  nrow()

# Proportion of households with seniors having food anxiety
proportion_with_anxiety <- households_with_seniors_anxiety / total_households_with_seniors

# Create a summary table for visualization
anxiety_proportions <- data.frame(
  Category = c("With Food Anxiety", "Without Food Anxiety"),
  Count = c(
    households_with_seniors_anxiety,
    total_households_with_seniors - households_with_seniors_anxiety
  ),
  Proportion = c(
    proportion_with_anxiety,
    1 - proportion_with_anxiety
  )
)

# Plot 
ggplot(anxiety_proportions, aes(x = Category, y = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Proportion of Households with Seniors Having Food Anxiety",
    x = "Household Type",
    y = "Proportion",
    fill = "Category"
  ) +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal()
ggsave("figures/fswrouty_proportion_of_senior_households.png", width=6, height=5)


### Stacked bar visualization of different senior groups ####
total_sample_size <- nrow(acs_data)

elderly_summary <- data.frame(
  Category = c("Households with seniors",
               "Households with seniors",
               "Households with only seniors",
               "Households with only seniors",
               "Households with only one seniors",
               "Households with only one seniors"),
  Count = c(
    sum(summary_by_PUMA$has_senior),
    total_sample_size - sum(summary_by_PUMA$has_senior),
    sum(summary_by_PUMA$only_senior),
    total_sample_size - sum(summary_by_PUMA$only_senior),
    sum(summary_by_PUMA$one_senior),
    total_sample_size - sum(summary_by_PUMA$one_senior)
  ),
  Type = c("Senior", "Remaining")
)

ggplot(elderly_summary, aes(x = Category, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  labs(
    title = "Household Composition with Senior Members",
    x = "Category",
    y = "Sample Size",
    fill = "Type"
  ) +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal()
ggsave("figures/households_with_seniors_composition.png", width=6, height=5)

