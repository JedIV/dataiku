# These functions were used for aggregation
# and imputation of variable sets
# in preparation for randomForest
# They were grabbed from:
# Data Mining Algorithms Explained Using R by Pawel Cichosz

#get x variables
x.vars <- function(formula, data)
{
  attr(terms(formula, data=data), "term.labels")
}

#get mode
modal <- function(v)
{
  m <- which.max(table(v))
  if (is.factor(v))
    flevels(v)[m]
  else
    sort(unique(v))[m]
}

flevels <- function(v) { factor(levels(v), levels=levels(v)) }

# aggregate little used variables
agg <- function(v, m, comb.val="other")
  {
    list(retained=names(sort(table(v), decreasing=TRUE))[1:min(m, nlevels(v))],
         combined=comb.val)
}

transmod.all <- function(transm, condf=function(v) TRUE)
{
  function(formula, data, ...)
  {
    attributes <- x.vars(formula, data)
    sapply(attributes,
           function(a) if (condf(data[[a]])) transm(data[[a]], ...),
           simplify=FALSE)
  } 
}

agg_all <- transmod.all(agg, is.factor) 

predict.transmod <- function(pred.transm)
{
  function(model, data, ...)
    {
        as.data.frame(sapply(names(data),
                                 function(a)
                                 if (a %in% names(model) && !is.null(model[[a]]))
                                   pred.transm(model[[a]], data[[a]], ...)
                                   else data[[a]],
                                   simplify=FALSE))
    }
}

predict.agg <- predict.transmod(function(m, v)
                    factor(ifelse(v %in% m$retained,
                                  as.character(v),
                                    ifelse(is.na(v), NA, m$combined)),
                           levels=c(m$retained, m$combined)))

imp <- function(v, med=FALSE)
  {
    if (!is.numeric(v))
        modal(v)
    else if (med)
      median(v, na.rm=TRUE)
    else
      mean(v, na.rm=TRUE)
  }

## imputation for all attributes
imp.all <- transmod.all(imp)
## imputation model prediction
predict.imp <- predict.transmod(function(m, v) { v[is.na(v)] <- m; v } )

