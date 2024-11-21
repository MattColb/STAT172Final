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

cps <- read.csv("./data/cps_00005.csv")
head(cps[,c("CPSID","PERNUM", "FSSTATUS", "FSSTATUSMD", "RACE","EDUC")]) %>% kable

#https://cps.ipums.org/cps-action/variables/search

summary(cps)

#map_chr(cps, ~attr(.x, "label")) %>% 
#  bind_cols(names=names(cps), question = .) %>%
#  rownames_to_column(var="Variable Name") %>% kable

cps <- cps %>% mutate(SEX = SEX-1,
                      CHILD = ifelse(AGE < 18, 1, 0),
                      ELDERLY = ifelse(AGE > 59, 1, 0),
                      BLACK = ifelse(RACE==200, 1, 0),
                      HISPANIC = ifelse(HISPAN>0,1,0),
                      EDUC = as.integer(EDUC %in% c(91 ,92, 111, 123, 124, 125)),
                      EMP = as.integer(EMPSTAT%in% c(1,10,12)),
                      MARRIED = as.integer(MARST %in% c(1,2)),
                      DIFF = ifelse(DIFFANY==2,1,0),
                      COUNT = as.factor(COUNTY)
                      )

#Not currently grouping properly
cps_data <- cps %>% group_by(CPSID=as.factor(CPSID)) %>%
  summarise(
    county = first(COUNTY),
    weight = first(HWTFINL),
    hhsize = n(),
    
    #Y variables
    #All should be the same for each person in the household
    #Search for more info about vars here: https://cps.ipums.org/cps-action/variables/search
    FSTOTXPNC_perpers = first(FSTOTXPNC)/hhsize, #Different from packet, but needed for summary
    FSSTATUS = first(FSSTATUS),
    FSSTATUSMD = first(FSSTATUSMD),
    FSFOODS = first(FSFOODS),
    FSWROUTY = first(FSWROUTY),
    FSBAL = first(FSBAL),
    FSRAWSCRA = first(FSRAWSCRA),
    FSTOTXPNC = first(FSTOTXPNC),
    FSSTMPVALC = first(FSSTMPVALC),
    #FSSTATUS = first(FSSTATUS),
    
    #FSTMPVALC, FSRAWSCRM, 
    
    female = sum(SEX),
    hispanic = sum(HISPANIC),
    black = sum(BLACK),
    kids = sum(CHILD),
    elderly = sum(ELDERLY),
    education = sum(EDUC),
    married = sum(MARRIED)
  ) %>% ungroup()

#Non-included variables (Not in ACS):
#EMPSTAT, DIFFANY, VETSTAT

head(cps_data)
sum(cps_data$hhsize)

cps_data <- cps_data %>% 
  mutate(FSSTATUS = ifelse(FSSTATUS %in% c(98,99), NA, FSSTATUS),
         FSSTATUSMD = ifelse(FSSTATUSMD %in% c(98,99), NA, FSSTATUSMD),
         FSFOODS = ifelse(FSFOODS %in% c(98,99), NA, FSFOODS),
         FSWROUTY = ifelse(FSWROUTY %in% c(96,97,98,99), NA, FSWROUTY),
         FSBAL = ifelse(FSBAL %in% c(96,97,98,99), NA, FSBAL),
         FSRAWSCRA = ifelse(FSRAWSCRA %in% c(98,99), NA, FSRAWSCRA),
         FSTOTXPNC = ifelse(FSTOTXPNC %in% c(999), NA, FSTOTXPNC),
         FSSTMPVALC = ifelse(FSSTMPVALC %in% c(996, 997, 998, 999), NA, FSSTMPVALC)) %>% #The 1000 wasn't in given code, but was always an outlier
  mutate(FSSTATUS = ifelse(FSSTATUS > 1, 1, 0),
         FSSTATUSMD = ifelse(FSSTATUSMD >1, 1, 0),
         FSFOODS = ifelse(FSFOODS > 1, 1, 0),
         FSWROUTY = ifelse(FSWROUTY > 1, 1, 0),
         FSBAL = ifelse(FSBAL > 1, 1, 0),
         FSRAWSCRA = ifelse(FSRAWSCRA >1, 1, 0),
         FSTOTXPNC_perpers = ifelse(is.na(FSTOTXPNC), NA, FSTOTXPNC_perpers),
         FSSTMPVALC_bin = ifelse(is.na(FSSTMPVALC), 0, 1),
         FSSTMPVALC_bin_char = ifelse(is.na(FSSTMPVALC), "No", "Yes")
  )

cps_data <- cps_data %>% mutate(
  county = as.factor(county)
) #Large number of levels, so might make sense to leave out, but our models might also tell us that

summary(cps_data)
str(cps_data)
head(cps_data)


#PREDICTIVE VARIABLES
#hhsize, married, education, elderly, kids, black, hispanic, female, county(?)

#FOCUS Y VARIABLES:
#FSWROUTY - Phuong
#Binary snap no snap FSSTMPVALC - Matt (This has a very small number of positives)
#FSFOODS - Aria

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
#Think about what I could do with NA values
#Make plots/clean up ROC plots
#

###########################
#   Train Test Split      #
###########################
FSSTMP.x.train <- model.matrix(FSSTMPVALC_bin ~ hhsize + married + education + elderly +
                                 kids + black + hispanic + female + county
                                 , data=train.df)[,-1]
FSSTMP.x.test <- model.matrix(FSSTMPVALC_bin ~ hhsize + married + education + elderly +
                                kids + black + hispanic + female + county
                                , data=test.df)[,-1]
FSSTMP.y.train <- as.vector(train.df$FSSTMPVALC_bin)
FSSTMP.y.test <- as.vector(test.df$FSSTMPVALC_bin)
train.weights <- as.vector(train.df$weight)
test.weights <- as.vector(test.df$weight)

###########################
# MLE Logistic Regression #
###########################

lr_mle_fsstmp <- glm(FSSTMPVALC_bin ~ hhsize + married + education + elderly +
                      kids + black + hispanic + female + county,
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
                              kids + black + hispanic + female + county,
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

#rf_init_fsstmp <- randomForest(FSSTMPVALC_bin_char ~ hhsize + married + education + elderly +
#                              kids + black + hispanic + female,
#                              data=train.df,
#                              mtry=3,
#                              ntree=1000,
#                              importance=TRUE,)
#
#rf_init_fsstmp

#############################
# Visualizing Relationships #
#############################

#Proportion of food stamps for each elderly person in household
ggplot(data=cps_data) +
  geom_histogram(aes(x=elderly, fill=FSSTMPVALC_bin_char), binwidth = 1, position="fill") +
  scale_fill_brewer(palette="Dark2")

ggplot(data=cps_data) +
  geom_histogram(aes(x=hhsize, fill=FSSTMPVALC_bin_char), binwidth=1, position="fill") +
  scale_fill_brewer(palette="Dark2")

#clustering?
#PCA ?



