rm(list=ls())

library(tidyverse)
library(knitr)
library(tibble)
library(ggthemes)
library(logistf)
library(glmnet)
library(haven)
library(pROC)

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
         FSSTMPVALC_bin = ifelse(is.na(FSSTMPVALC), 0, 1)
  )

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


###########################
#   Food Stamp Analysis   #
###########################
FSSTMP.x.train <- model.matrix(FSSTMPVALC_bin ~ hhsize + married + education + elderly +
                                 kids + black + hispanic + female + county
                                 , data=train.df)[,-1]
FSSTMP.x.test <- model.matrix(FSSTMPVALC_bin ~ hhsize + married + education + elderly +
                                kids + black + hispanic + female + county
                                , data=test.df)[,-1]
FSSTMP.y.train <- as.vector(train.df$FSSTMPVALC_bin)
FSSTMP.y.test <- as.vector(test.df$FSSTMPVALC_bin)

###########################
# MLE Logistic Regression #
###########################

lm_fsstmp_mle <- glm(FSSTMPVALC_bin ~ hhsize + married + education + elderly +
                      kids + black + hispanic + female + county,
                     data=train.df,
                     family=binomial(link="logit"),
                     weights=weight
                    )

summary(lm_fsstmp_mle)
lm_fsstmp_mle_beta <- lm_fsstmp_mle %>% coef()

test.df.preds <- test.df %>% 
  mutate(
    lm_fsstmp_mle_preds = predict(lm_fsstmp_mle, test.df, type="response")
  )

lm_fsstmp_mle_rocCurve <- roc(
  response=as.factor(test.df.preds$FSSTMPVALC_bin),
  predictor= test.df.preds$lm_fsstmp_mle_preds,
  levels=c("0","1"))

plot(lm_fsstmp_mle_rocCurve, print.thres=TRUE, print.auc=TRUE)

lm_fsstmp_mle_pi_star <- coords(lm_fsstmp_mle_rocCurve, "best", ref="threshold")$threshold[1]

################################
# Firth's Penalized Likelihood #
################################



################
#     Lasso    #
################

lr_lasso_fsstmp_cv <- cv.glmnet(FSSTMP.x.train, FSSTMP.y.train, family=binomial(link="logit"), alpha=1)

plot(lr_lasso_fsstmp_cv)

best_lasso_lambda_fsstmp <- lr_lasso_fsstmp_cv$lambda.min

lr_lasso_coefs_fsstmp <- coef(lr_lasso_fsstmp_cv, s="lambda.min") %>% as.matrix()

lr_lasso_coefs_fsstmp #Really interesting to see that there are no 0s for coefficients.

final_lasso_fsstmp <- glmnet(FSSTMP.x.train, FSSTMP.y.train, family=binomial(link="logit"), 
                            alpha=1, lambda = best_lasso_lambda_fsstmp)

test.df.preds <- test.df.preds %>% mutate(
  lasso_pred_fsstmp = predict(final_lasso_fsstmp, FSSTMP.x.test, type="response")[,1]
)

lasso_fsstmp_rocCurve <- roc(response = as.factor(test.df.preds$FSSTMPVALC_bin),
                             predictor =test.df.preds$lasso_pred_fsstmp,
                             levels=c("0", "1"))

plot(lasso_fsstmp_rocCurve, print.thres=TRUE, print.auc=TRUE) #Better at AUC = .799, (.677, .810)

#############
#   Ridge   #
#############

lr_ridge_fsstmp_cv <- cv.glmnet(FSSTMP.x.train, FSSTMP.y.train, family=binomial(link="logit"), alpha=0)

plot(lr_ridge_fsstmp_cv)

best_ridge_lambda_fsstmp <- lr_lasso_fsstmp_cv$lambda.min

lr_ridge_coefs_fsstmp <- coef(lr_ridge_fsstmp_cv, s="lambda.min") %>% as.matrix()

lr_ridge_coefs_fsstmp

final_ridge_fsstmp <- glmnet(FSSTMP.x.train, FSSTMP.y.train, family=binomial(link="logit"), 
                             alpha=0, lambda = best_ridge_lambda_fsstmp)

test.df.preds <- test.df.preds %>% mutate(
  ridge_pred_fsstmp = predict(final_ridge_fsstmp, FSSTMP.x.test, type="response")[,1]
)

ridge_fsstmp_rocCurve <- roc(response = as.factor(test.df.preds$FSSTMPVALC_bin),
                             predictor =test.df.preds$ridge_pred_fsstmp,
                             levels=c("0", "1"))

plot(ridge_fsstmp_rocCurve, print.thres=TRUE, print.auc=TRUE) #.799 (.679,.810)

#Note that these felt like they were pretty close to the maximum likelihood on their plots.
#Since Lasso didn't have any 0 coefficients, it should be very similar to ridge,
#Since the big difference between the two is that Lasso can set these coefs to 0 while ridge 
#Moves uniformly towards 0. 

#Random Forest w/AIC/BIC

#stepwise/backwards regression

#clustering?
#PCA ?



