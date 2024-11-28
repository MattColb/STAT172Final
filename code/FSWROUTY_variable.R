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


source("code/clean_cps.R")

# change y-variable into factor
cps_data$FSWROUTY_binchar <- ifelse(cps_data$FSWROUTY == 0, "No", "Yes")

# handle missing values
cps_data <- cps_data[!is.na(cps_data$FSWROUTY_binchar),]

cps_data <- cps_data %>% mutate(FSWROUTY_bin = as.factor(FSWROUTY_binchar))

summary(cps_data)
head(cps_data)

#######################################
############# CLUSTERING ##############
#######################################

# set up
data_X <- cps_data[, c( "weight", "hhsize", "female", "hispanic", "black", 
                       "kids", "elderly", "education", "married")]

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

##########################################
############ Train Test Split ############
#########################################

# Add cluster membership back to the main dataset
cps_data$h_cluster <- as.factor(cutree(data_clust, k = 5))

# Combine cluster memberships with other features
model_data <- cps_data[, c("FSWROUTY_bin", "h_cluster", "weight", "hhsize", "female", 
                           "hispanic", "black", "kids", "elderly", "education", "married")]

# splitting training and testing
RNGkind(sample.kind = "default")
set.seed(159159)
train.idx <- sample(x=1:nrow(model_data), size=.7*nrow(model_data))
train.df <- model_data[train.idx,]
test.df <- model_data[-train.idx,]

##############################################
################ MLE #########################
##############################################

fswrouty_mle <- glm(FSWROUTY_bin ~ h_cluster + weight +hhsize + married + 
                      education + elderly + kids + black + hispanic + female,
                    data = train.df,
                    family = binomial(link = 'logit'))

summary(fswrouty_mle)
# weight has a strong positive effect: Higher weights increase the likelihood of "Yes."
# education and elderly have negative effects
# black and hispanic have positive effects

# Evaluate Model Accuracy 
test_pred_probs <- predict(fswrouty_mle, newdata = test.df, type = "response")
test_pred_classes <- ifelse(test_pred_probs > 0.5, "Yes", "No")
library(caret)
confusionMatrix(as.factor(test_pred_classes), test.df$FSWROUTY_bin)

# the model correctly classifies 67.45% of all instances.
# sensitivity = 46/(232 + 46) = 16.55%
# specificity = 497/(497 + 30) = 94.31%

###############################################
############# RANDOM FOREST ###################
##############################################

rf_fswrouty <- randomForest(FSWROUTY_bin ~ h_cluster + weight +hhsize + married + 
                              education + elderly + kids + black + hispanic + female ,
                           data = train.df,
                           ntree = 1000,
                           mtry = 4,
                           importance = TRUE)

importance(rf_fswrouty)
varImpPlot(rf_fswrouty)
# Validate model as predictive tool

pi_hat <- predict(rf_fswrouty, test.df, type = "prob")[, "Yes"] #Choose positive event column

rocCurve <- roc(response = test.df$FSWROUTY_bin,
                predictor = pi_hat,
                levels = c("No", "Yes"))

plot(rocCurve, print.thres = TRUE, print.ouc = TRUE)

# 72.3% of positive cases are correctly classified
# 49.7% of negative cases are misclassified as positive

auc(rocCurve) # Area under the curve: 0.6225














