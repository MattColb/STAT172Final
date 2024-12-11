# Description: Below is the code to understand X-variables better by doing
# clustering. Then, using the clusters to build the model
# Result: We found that this methods do not work well, but still keep the code
# to get x-variable clusters anytime.

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

source("code/clean_cps.R")

# change y-variable into factor
cps_data$FSWROUTY_binchar <- ifelse(cps_data$FSWROUTY == 0, "No", "Yes")

# handle missing values
cps_data <- cps_data[!is.na(cps_data$FSWROUTY_binchar),]

# cps_data <- cps_data %>% mutate(FSWROUTY_bin = as.factor(FSWROUTY_binchar))

cps_data <- cps_data %>% mutate(FSWROUTY_bin = ifelse(FSWROUTY_binchar == "No", 0, 1))
summary(cps_data)
head(cps_data)

############# CLUSTERING ##############

# set up
data_X <- cps_data[, c( "hhsize", "female", "hispanic", "black", 
                        "kids", "elderly", "education", "married", "faminc_cleaned")]

# standardize all predictor columns 
data_stand <- apply(data_X, 2, function(x)(x - mean(x))/sd(x))

# compute observation-observation distance (for hierachical clustering) 
data_dist <- dist(data_stand, method = "euclidean")

# first, let's use the average method to measure cluster-to-cluster similarity 
data_clust <- hclust(data_dist, method = "average")

# the height of a bar is distance between cluster centers 
plot(data_clust, labels = cps_data$FSWROUTY, hang = -1)

rect.hclust(data_clust, k = 3, border ="red")

# Making sense of the clusters, obtaining "characterization" of clusters
data_X$h_cluster <- as.factor(cutree(data_clust, k = 5))
data_X_long <- melt(data_X, id.vars = c("h_cluster"))
head(data_X_long)
ggplot(data = data_X_long) +
  geom_boxplot(aes(x = h_cluster, y = value, fill = h_cluster)) +
  facet_wrap(~variable, scales= "free") + 
  scale_fill_brewer("Cluster \nMembership", palette = "Dark2") +
  ggtitle("Hierachical Clusters")

# Cluster 1: small households, elderly or retired individuals living alone or with a spouse
#           => might benefit us to look more into this cluster
# Cluster 2: young black families
# Cluster 3: large families with children 
# Cluster 4: medium size Hispanic families
#           => big family size with children and elderly  
# Cluster 5: Outliers or Minimal Engagement


############ Train Test Split ############

# Add cluster membership back to the main dataset
cps_data$h_cluster <- as.factor(cutree(data_clust, k = 5))

# Combine cluster memberships with other features
model_data <- cps_data[, c("FSWROUTY_bin", "h_cluster", "weight")]

# splitting training and testing
RNGkind(sample.kind = "default")
set.seed(159159)
train.idx <- sample(x=1:nrow(model_data), size=.7*nrow(model_data))
train.df <- model_data[train.idx,]
test.df <- model_data[-train.idx,]

#############################################

x_train <- model.matrix(FSWROUTY_bin ~ h_cluster
                        , data=train.df)[,-1]
x_test <- model.matrix(FSWROUTY_bin ~ h_cluster 
                       , data=test.df)[,-1]
y_test <- as.vector(test.df$FSWROUTY_bin)
y_train <- as.vector(train.df$FSWROUTY_bin)

################ MLE #########################

fswrouty_mle <- glm(FSWROUTY_bin ~ h_cluster,
                    data = train.df,
                    family = binomial(link = 'logit'),
                    weights = weight)

summary(fswrouty_mle)

#Since all variables are seen as significant because of complete separation


# Firth's Penalized Likelihood #

firths_fswrouty <- logistf(FSWROUTY_bin ~ h_cluster,
                           data=train.df,
                           weights=weight)

summary(firths_fswrouty)

firths_fswrouty_beta <- firths_fswrouty %>% coef()

test_preds <- test.df %>% 
  mutate(
    firths_fswrouty_prob = predict(firths_fswrouty, test.df , type="response")
  )

firths_fswrouty_rocCurve <- roc(
  response=as.factor(test_preds$FSWROUTY_bin),
  predictor= test_preds$firths_fswrouty_prob,
  levels=c("0","1"))

plot(firths_fswrouty_rocCurve, print.thres=TRUE, print.auc=TRUE) 

firth_fswrouty_pi_hat <- coords(firths_fswrouty_rocCurve, "best", ref="threshold")$threshold[1]

########### LASSO ##################

fswrouty_lasso <- cv.glmnet(x_train, y_train, family=binomial(link="logit"), alpha = 1)
plot(fswrouty_lasso)
best_lambda_lasso <- fswrouty_lasso$lambda.min
coef(fswrouty_lasso, s="lambda.min") %>% as.matrix()
# cluster 2, 4, and 5 went to 0

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

