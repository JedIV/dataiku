###########################################################
# loader.R
# Jed Dougherty
# - Loads csvs of 1994-1995 us census data
# - Appropriately names columns using code and description
# - Performs various data-cleaning operations on both
#   the training and test sets
# - Performs summary descriptions and builds univariate
#   descriptive plots of variables
# Descriptions were built out from metadata included in
# census_income_metadata.txt
###########################################################

## Packages
library(reshape2)
library(plyr)
library(ggplot2)
library(scales)
library(caret)
library(ROCR)
library(randomForest)
library(rpart)
library(rpart.plot)

## Functions
source('outside_functions.r')

error_func <- function(pred, actual) { mean(pred != actual) }

conf_matrix <- function(pred, actual){
  table(pred, actual, dnn=c("predicted", "actual"))
}

#true positive rate
tpr         <- function(cm) {
  if (is.nan(p <- cm[2,2]/(cm[2,2] + cm[1,2]))) 1 else p
}

#false positive rate
fpr         <- function(cm) {
  if (is.nan(p <- cm[2,1]/(cm[2,1] + cm[1,1]))) 1 else p
}

#precision
precision   <- function(cm) {
  if (is.nan(p <- cm[2,2]/(cm[2,2] + cm[2,1]))) 1 else p
}

#specificity
specificity <- function(cm) {
  if (is.nan(p <- cm[1,1]/(cm[2,1] + cm[1,1]))) 1 else p
}

#cutoff
cutoff <- function(probs, cutoff) {
  factor(ifelse(probs[,2] < cutoff, 'lower', 'upper'))
}

## Read in data
census_train        <- read.csv('../us_census_full/census_income_learn.csv',
                                   header = FALSE)

census_test         <- read.csv('../us_census_full/census_income_test.csv',
                                   header = FALSE)

census_headers <- read.csv('../us_census_full/census_headers.csv')

names(census_train) <- census_headers$description
names(census_test ) <- census_headers$description

# First we should drop the instance column, since
# we see in the metadata file that it isn't useful.
# 
# "This attribute should *not* be used in the classifiers, so it is
# set to "ignore" in this file."  -- Metadata file

census_train$instance_weight <- NULL
census_test$instance_weight  <- NULL

# After comparing with the metadata file, we see that some factor variables
# have been incorrectly labeled as integers when loading into R. We change
# them back into factors

factor_vars <- c('industry_code',
                 'occupation_code',
                 'own_business_or_self_employed',
                 'veterans_benefits',
                 'survey_year') 

census_train[,factor_vars] <- lapply(factor_vars, function(x) factor(census_train[,x]))
census_test[,factor_vars] <- lapply(factor_vars, function(x) factor(census_test[,x]))

# R also did a pretty bad job converting the income column

census_train$income_over_under_fifty <- factor(ifelse(" 50000+." == census_train$income_over_under_fifty, 'upper', 'lower'),
                                               levels = c('lower','upper'))
census_test$income_over_under_fifty <- factor(ifelse(" 50000+." == census_test$income_over_under_fifty, 'upper', 'lower'),
                                               levels = c('lower','upper'))

# for further analysis we can split our data into integer and non-integer categories
integer_categories <- census_train[sapply(census_train, is.numeric)]
integer_categories$income_over_under_fifty <- census_train$income_over_under_fifty

factor_categories  <- census_train[sapply(census_train, is.factor)]
