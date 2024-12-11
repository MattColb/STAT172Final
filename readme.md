This contains some basic info for how to run our code. We each have our own readme file, but for the purposes of separating our individual readmes, we created this as a temporary way of informing how to run our code.

## Packages

To install the required packages, run the following code in R:

```----------
install.packages(c(
  "tidyverse", "caret", "ggthemes", 
  "glmnet", "haven", "knitr", 
  "logistf", "sf", "pROC", 
  "randomForest", "RColorBrewer", 
  "rpart", "rpart.plot", "reshape2"
))
```

## Data

We use four different data files. `/data/cps_00006.csv` contains all of the Current Population Survey Food Security Supplement survey data from the department of agriculture. `/data/tl_2023_19_puma20/` folder contains the shapefile and other relevant information used to create the maps of Iowa divided by PUMA. The `/data/total_iowa_seniors_by_puma.csv` was data that was scraped containing the number of seniors and each PUMA and was provided to us. Finally is `/data/spm_pu_2022.sas7bdat`, which is the data that we are using to predict. It is not tracked by Github due to its size, but you can download the file from here and place it in the correct path.

https://www.census.gov/data/datasets/time-series/demo/supplemental-poverty-measure/acs-research-files.html

We also save our predictions in the `/data/fswrouty_prediction.csv`, `/data/fsstamp_prediction.csv`, `/data/fsfoods_prediction.csv`, and `/data/single_senior_household.csv` to make reproduction of the `code/combining_predictions.R` quicker, but there are also comments at the top of the file if you would prefer to source just that file. 

## Recreation

To recreate our predictions, run the three files for each variable, `code/fsfoods_analysis.R`, `code/FSWROUTY_variable.R`, and `code/fsstamp_analysis.R`. You should run these files before you run the `code/combining_predictions.R` if you do not use the source functions at the top of the file. Finally, you can also run `code/visualizations_and_general_analysis.R` and `code/cluster_model_testing.R` to see more work that we have done in visualizing the data and trying to cluster some of our X variables. 