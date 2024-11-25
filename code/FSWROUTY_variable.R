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


cps_data <- read.csv("./data/interim/cps_data.csv")
summary(cps_data)
head(cps_data)

#######################################
############# CLUSTERING ##############
#######################################

# set up
data_X <- cps_data[, c("county", "weight", "hhsize", "female", "hispanic", "black", 
                       "kids", "elderly", "education", "married")]
# standardize all predictor columns 
data_stand <- apply(data_X, 2, function(x)(x - mean(x))/sd(x))

# compute observation-observation distance (for hierachical clustering) 
data_dist <- dist(data_stand, method = "euclidean")

# first, let's use the average method to measure clsuter-to-cluster similarity 
data_clust <- hclust(data_dist, method = "average")

# the height of a bar is distance between cluster centers 
plot(data_clust, labels = cps_data$FSWROUTY, hang = -1)

rect.hclust(data_clust, k = 3, border ="red")

# Making sene of the clusters, obtaining "characterization" of clusters
# save clusters
data_X$h_cluster <- as.factor(cutree(data_clust, k = 5))
data_X_long <- melt(data_X, id.vars = c("h_cluster"))
head(data_X_long)
ggplot(data = data_X_long) +
  geom_boxplot(aes(x = h_cluster, y = value, fill = h_cluster)) +
  facet_wrap(~variable, scales= "free") + 
  scale_fill_brewer("Cluster \nMembership", palette = "Dark2") +
  ggtitle("Hierachical Clusters")





