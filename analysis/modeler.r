###########################################################
# modeler.R
# Jed Dougherty
# - Performs model creation and evaluation
# - Data sets available from loader.r:
#     census_train
#     census_test
#     integer_categories
#     factor_categories
###########################################################

source('loader.R')


# Here we are going to begin constructing a model for our problem
# First we will want to split our training set into subsets for 
# training and verification. We will do a 70/30 split
# Setting seed for reproducibility
set.seed(3141)
randomizer <- runif(nrow(census_train))
c_train    <- census_train[randomizer >= .3,]
c_verif    <- census_train[randomizer <  .3,]
c_small    <- census_train[randomizer >= .9,]

int_train    <- integer_categories[randomizer >= .3,]
int_verif    <- integer_categories[randomizer <  .3,]

fac_train    <- factor_categories[randomizer >= .3,]
fac_verif    <- factor_categories[randomizer <  .3,]

# We start with a simple logit model of only our numeric categories
logit_numeric <- glm(income_over_under_fifty ~., data = int_train, family = binomial)

# Immediately we have a problem: Linear Separation. Even with only our
# numeric variables we see that R returns the warning:
# Fitted probabilities 0 or 1 occurred. This happens when
# there is some combination of coefficients of our input variables
# that allows for complete separation of our income variable.
# This problem will only become worse when we include our factor
# variables in the model. We could have guessed this was going
# to be an issue when we look at the descriptive analysis and saw
# that not only were the vast majority of our income data clustered
# in the under 50k section, but many of our independent variables
# were also clustered around 0 in the case of numeric variables,
# or a single category in the case of our categorical variables.

# We can use the carat package to further investigate which variables 
# are likely problem causers:
nearZeroVar(int_train, saveMetrics=TRUE)

# We can see from this output that wages per hour, capital gains, 
# capital losses, and dividends from stock all have near zero variance.
# There are a few ways to get around this if we want to keep using
# Logistic regression, (a Bayesian prior, removing those variables, etc.) 
# I don't want to remove the variables for obvious reasons - they do seem
# to contain a lot of information that would be important to the model.
# And I'm not Andrew Gelman (and I don't have a lot of domain knowledge
# in census tracking.) so I'm not going to start throwing around
# priors willy nilly.

# Instead I am going to take a different tactic and use decision trees.
# Decision trees have the advantage of not caring about Linear Separation one
# way or another.

basic_tree <- rpart(income_over_under_fifty ~., c_train)
basic_tree_predictions <- predict(basic_tree, c_verif, type='c')

error_func(basic_tree_predictions, c_verif$income_over_under_fifty)
cm <- conf_matrix(basic_tree_predictions, c_verif$income_over_under_fifty)
basic_tree_tpr <- tpr(cm)
basic_tree_fpr <- fpr(cm)
precision(cm)
specificity(cm)

# Not too good. Our true positive rate sucks.
# Using ROCR we can construct a ROC graph of our tpr and fpr
basic_tree_probs <- predict(basic_tree, c_verif, type='prob')
rocr_pred <- prediction(basic_tree_probs[,2], c_verif$income_over_under_fifty)
rocr_perf <- performance(rocr_pred, "tpr", "fpr")
plot(rocr_perf)
points(basic_tree_fpr, basic_tree_tpr)

# Looks like we could up our tpr a lot by allowing for a bit higher fpr
cutoffs <- data.frame(cut = rocr_perf@alpha.values[[1]],
                      fpr = rocr_perf@x.values[[1]], 
                      tpr = rocr_perf@y.values[[1]])

# We can see from the above output that we can maximize tpr while keeping
# fpr under .1 with a cutoff around .139 instead of .5
optimal_cutoff       <- cutoffs[cutoffs$tpr == max(cutoffs[cutoffs$fpr < .1,]$tpr),]$cut
optimal_cutoff_preds <- cutoff(basic_tree_probs, optimal_cutoff)
optimal_cm <- conf_matrix(optimal_cutoff_preds, c_verif$income_over_under_fifty)
optimal_tree_tpr <- tpr(optimal_cm)
optimal_tree_fpr <- fpr(optimal_cm)


plot(rocr_perf)
points(basic_tree_fpr, basic_tree_tpr)
points(optimal_tree_fpr, optimal_tree_tpr)

# Thats a bit better. We can also weight our model's misclassification
# costs since we have
# so many more under fifty than over fifty.
# A nice rule of thumb is to weight by approximately the ratio
# of over to under. Here I weight at 5 to 1.

weighted_tree <- rpart(income_over_under_fifty ~., c_train,
                      weights = ifelse(c_train$income_over_under_fifty == 'upper', 5, 1))

# we perform the same analysis as before but with the weighted tree
weighted_tree_predictions <- predict(weighted_tree, c_verif, type='c')

error_func(weighted_tree_predictions, c_verif$income_over_under_fifty)
cm <- conf_matrix(weighted_tree_predictions, c_verif$income_over_under_fifty)
weighted_tree_tpr <- tpr(cm)
weighted_tree_fpr <- fpr(cm)
precision(cm)
specificity(cm)

weighted_tree_probs <- predict(weighted_tree, c_verif, type='prob')
weighted_rocr_pred <- prediction(weighted_tree_probs[,2], c_verif$income_over_under_fifty)
weighted_rocr_perf <- performance(weighted_rocr_pred, "tpr", "fpr")

plot(weighted_rocr_perf)
points(weighted_tree_fpr, weighted_tree_tpr)

# upon inspection of the confusion matrix and ROC curve we see a large improvement.
# if we were willing to raise our fpr  to a bit over .1 we could get a tpr over .8

# we could also possibly optimize our decision tree model by applying some
# pruning and feature selection, but instead I am going to compare our
# decision tree to a Random Forest model.

# Random forest in R also annoyingly requires less than
# 32 categories in each factor so we aggregate the smallest
# categories in our factor variables. I am also using a small subset (.2) of our
# training data to train the random forest so it doesn't take forever

c_agged          <- agg_all(income ~ ., c_small, 31)
c_rf             <- predict.agg(c_agged, c_small)
c_rf_verif       <- predict.agg(c_agged, c_verif)
census_test_agged <- predict.agg(c_agged, census_test)


c_imp          <-   imp.all(income ~ ., c_rf)
c_rf             <- predict.imp(c_imp, c_rf)
c_rf_verif  <- predict.imp(c_imp, c_rf_verif)
census_test_agged  <- predict.imp(c_imp, census_test_agged)

c_rf$income_over_under_fifty <- factor(c_rf$income_over_under_fifty)
c_rf_verif$income_over_under_fifty <- factor(c_rf_verif$income_over_under_fifty)
census_test_agged$income_over_under_fifty <- factor(census_test_agged$income_over_under_fifty)

c_rf_model       <- randomForest(income_over_under_fifty ~ ., c_rf, importance = TRUE)

# with our random forest model trained lets see how it does on our
# validation data.
rf_predictions <- predict(c_rf_model, c_rf_verif)

error_func(rf_predictions, c_rf_verif$income_over_under_fifty)
cm <- conf_matrix(rf_predictions, c_rf_verif$income_over_under_fifty)
rf_tpr <- tpr(cm)
rf_fpr <- fpr(cm)
precision(cm)
specificity(cm)

rf_probs <- predict(c_rf_model, c_rf_verif, type='prob')
rf_rocr_pred <- prediction(rf_probs[,2], c_rf_verif$income_over_under_fifty)
rf_rocr_perf <- performance(rf_rocr_pred, "tpr", "fpr")

plot(rf_rocr_perf)
points(rf_fpr, rf_tpr)

cutoffs <- data.frame(cut = rf_rocr_perf@alpha.values[[1]],
                      fpr = rf_rocr_perf@x.values[[1]], 
                      tpr = rf_rocr_perf@y.values[[1]])

# again we will calculate the optimal cutoff that keeps the fpr under 10%
optimal_cutoff       <- cutoffs[cutoffs$tpr == max(cutoffs[cutoffs$fpr < .1,]$tpr),]$cut
optimal_cutoff_preds <- cutoff(rf_probs, optimal_cutoff)
optimal_cm <- conf_matrix(optimal_cutoff_preds, c_rf_verif$income_over_under_fifty)
optimal_tree_tpr <- tpr(optimal_cm)
optimal_tree_fpr <- fpr(optimal_cm)


plot(rf_rocr_perf)
points(optimal_tree_fpr, optimal_tree_tpr)

# At the cost of a little bit of accuracy we can see that our tpr and fpr rates
# are quite good, and the model does a good job on our verification set.
# We are now ready to try applying both models to the test set.

###################################
# Decision Tree Model on Test data
###################################

weighted_tree_predictions <- predict(weighted_tree, census_test, type='c')

error_func(weighted_tree_predictions, census_test$income_over_under_fifty)
cm <- conf_matrix(weighted_tree_predictions, census_test$income_over_under_fifty)
weighted_tree_tpr <- tpr(cm)
weighted_tree_fpr <- fpr(cm)
precision(cm)
specificity(cm)

weighted_tree_probs <- predict(weighted_tree, census_test, type='prob')
weighted_rocr_pred <- prediction(weighted_tree_probs[,2], census_test$income_over_under_fifty)
weighted_rocr_perf <- performance(weighted_rocr_pred, "tpr", "fpr")

plot(weighted_rocr_perf)
points(weighted_tree_fpr, weighted_tree_tpr)

###################################
# Random Forest Model on Test Data
###################################

rf_predictions <- predict(c_rf_model, census_test_agged)

error_func(rf_predictions, census_test_agged$income_over_under_fifty)
cm <- conf_matrix(rf_predictions, census_test_agged$income_over_under_fifty)
rf_tpr <- tpr(cm)
rf_fpr <- fpr(cm)
precision(cm)
specificity(cm)

rf_probs <- predict(c_rf_model, census_test_agged, type='prob')
rf_rocr_pred <- prediction(rf_probs[,2], census_test_agged$income_over_under_fifty)
rf_rocr_perf <- performance(rf_rocr_pred, "tpr", "fpr")

plot(rf_rocr_perf)
points(rf_fpr, rf_tpr)

cutoffs <- data.frame(cut = rf_rocr_perf@alpha.values[[1]],
                      fpr = rf_rocr_perf@x.values[[1]], 
                      tpr = rf_rocr_perf@y.values[[1]])

# again we will calculate the optimal cutoff that keeps the fpr under 10%
optimal_cutoff       <- cutoffs[cutoffs$tpr == max(cutoffs[cutoffs$fpr < .1,]$tpr),]$cut
optimal_cutoff_preds <- cutoff(rf_probs, optimal_cutoff)
optimal_cm <- conf_matrix(optimal_cutoff_preds, census_test_agged$income_over_under_fifty)
optimal_tree_tpr <- tpr(optimal_cm)
optimal_tree_fpr <- fpr(optimal_cm)


plot(rf_rocr_perf)
points(optimal_tree_fpr, optimal_tree_tpr)

#####################################################
# Outcomes:
####################################################

# RF on test data:

# predicted lower upper
#     lower 84412  1350
#     upper  9164  4836

# Decision Tree on test data:

# actual
# predicted lower upper
#     lower 87288  2132
#     upper  6288  4054

# Both models performed relatively well. I personally prefer
# the decision tree model because more information can be pulled
# from it. See for example:

prp(weighted_tree) 

# shows that the most important deciding factors are, as expected
# occupation, education, capital_gains, dividends, and age
# with a nice breakdown of where we can expect dividing lines to come.
