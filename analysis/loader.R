###########################################################
# loader.R
# Jed Dougherty
# - Loads csvs of 1994-1995 us census data
# - Appropriately names columns using code and description
# - Performs various data-cleaning operations on both
#   the training and test sets
# Descriptions were built out from metadata included in
# census_income_metadata.txt
###########################################################

## Packages
library(reshape2)
library(ggplot2)
library(scales)
library(caret)

## Functions

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

precision   <- function(cm) {
  if (is.nan(p <- cm[2,2]/(cm[2,2] + cm[2,1]))) 1 else p
}

specificity <- function(cm) {
  if (is.nan(p <- cm[1,1]/(cm[2,1] + cm[1,1]))) 1 else p
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

# R also did a pretty bad job converting the income column

census_train$income_over_under_fifty <- factor(ifelse(" 50000+." == census_train$income_over_under_fifty, 'upper', 'lower'),
                                               levels = c('lower','upper'))

# we can use R's summary command to get a nice set of univariate summary
# statistics for our data:
summary(census_train)

# for further analysis we can split our data into integer and non-integer categories
integer_categories <- census_train[sapply(census_train, is.numeric)]
integer_categories$income_over_under_fifty <- census_train$income_over_under_fifty

factor_categories  <- census_train[sapply(census_train, is.factor)]


# standard deviations of integer categories
lapply(sd, integer_categories)
# prints all integer categories on same page
melted_integers <- melt(integer_categories)
ggplot(melted_integers, aes(x = value)) +
                           facet_wrap(~variable, scales = 'free_x') + 
                           geom_histogram()

# prints boxplots for integer categories
ggplot(melted_integers, aes(x = variable, y = value)) +
                           facet_wrap(~variable, scales = 'free') + 
                           geom_boxplot()

# four of our numerical variables have a very high number of zeroes
# we can take a look at them without their zeroed
# points to get a feel for their non-zero distributions
high_zeroes <- c('wage_per_hour', 'capital_gains', 'capital_losses','dividends_from_stocks')
hz <- melted_integers[melted_integers$variable %in% high_zeroes,]
ggplot(hz[hz$value != 0,], aes(x = value)) +
       facet_wrap(~variable, scales = 'free_x') + 
       geom_histogram()

# individually print bar charts of factor variables
# I flipped the axes here because the order doesn't matter
# and I find it easier to read the category names this way
for(name in names(factor_categories)){
  plot <- ggplot(factor_categories, aes_string(x = name)) +
          geom_bar(aes(y = (..count..)/sum(..count..))) +
          coord_flip() +
          scale_y_continuous('percent of total',labels=percent, limits = c(0,1)) + 
          ggtitle(name) 
  print(plot)
}
