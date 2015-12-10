###########################################################
# analysis.r
# Jed Dougherty
# - Performs summary descriptions and builds univariate
#   descriptive plots of variables
# - Data sets available from loader.r:
#     census_train
#     census_test
#     integer_categories
#     factor_categories
###########################################################

source('loader.R')

# we can use R's summary command to get a nice set of univariate summary
# statistics for our data:
summary(census_train)

# means and standard deviations of integer categories
ddply(integer_categories, .(income_over_under_fifty), numcolwise(mean))
ddply(integer_categories, .(income_over_under_fifty), numcolwise(sd))


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
# We will also log the x axis so we can see the spread without the long right tail
high_zeroes <- c('wage_per_hour', 'capital_gains', 'capital_losses','divdends_from_stocks')
hz <- melted_integers[melted_integers$variable %in% high_zeroes,]
ggplot(hz[hz$value != 0,], aes(x = value)) +
       facet_wrap(~variable, scales = 'free_x') + 
       scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels  = trans_format("log10", math_format(10^.x))) +
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
