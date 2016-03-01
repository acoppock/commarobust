#' Obtain the standard error of the mean
#'
#' @param x a numeric vector
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#'
#' @return a scalar equal to the standard error of the mean, computed as sd(x)/(sqrt(n)).
#' @export
#'
#' @examples
#'
#' x <- rnorm(100)
#' mean(x)
#' se_mean(x)
#'
#'
se_mean <- function(x, na.rm = FALSE){
  # Error handling from mean.default
  if (!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
    warning("argument is not numeric or logical: returning NA")
    return(NA_real_)
  }
  if (na.rm) {x <- x[!is.na(x)]}
  n <- length(x)
  return(sd(x)/(sqrt(n)))
}


#' Title
#'
#' @param fit A model object, typically created by lm, glm, or many other model fitting functions.
#' @param cluster An optional numeric vector of length N that describes the clusters that units are in. May not include NAs. Is NULL by default. If specified, cluster-robust standard errors will be estimated.
#' @param type A string indicating the type of heterskedasticty-robust standard errors to be estimated. Is ignored if cluster is specificed. Is "HC2" by default, because these are equivalent to Neyman standard errors (See Aronow and Samii).
#'
#' @return A matrix of coefficients, standard errors, t-statistics, and p-values
#' @export
#'
#' @examples
#'
#' library(randomizr)
#'
#' Y <- rnorm(100)
#' Z <- complete_ra(100)
#' fit <- lm(Y ~ Z)
#' commarobust(fit)
#'
#' # Clustered
#' Y <- rnorm(100)
#' clust <- rep(letters[1:10], 10)
#' Z <- cluster_ra(clust)
#'
#' fit <- lm(Y ~ Z)
#' commarobust(fit, cluster = clust)
#'
commarobust <- function(fit, cluster = NULL, type="HC2"){
  if(is.null(cluster)){
    return_obj <- lmtest::coeftest(fit,sandwich::vcovHC(fit, type=type))[]
  }else{
    M <- length(unique(cluster))
    N <- length(cluster)
    K <- fit$rank
    dfc <- (M/(M-1))*((N-1)/(N-K))
    uj  <- apply(sandwich::estfun(fit),2, function(x) tapply(x, cluster, sum));
    vcovCL <- dfc*sandwich::sandwich(fit, meat=crossprod(uj)/N)
    return_obj <- lmtest::coeftest(fit, vcovCL)[]
  }
  attr(return_obj, "method") <- NULL
  return(return_obj)
}


getrobustses <- function(fit, type = "HC2"){
  robustfit <- commarobust(fit, type = type)
  return(robustfit[,2])
}

getrobustps <- function(fit, type = "HC2"){
  robustfit <- commarobust(fit, type = type)
  ps <- robustfit[,4]
  ps["(Intercept)"] <- 1
  return(ps)
}

#' Prepare Ses for Stargazer
#'
#' This function will prepare heteroskedasticity-robust standard errors for stargazer. Currently, it does not handle cluster-robust standard errors because there is no nice way to pass model-specific vectors of clusters.
#'
#' @param ... a series of model fits, separated by commas
#' @param type A string indicating the type of heterskedasticty-robust standard errors to be estimated. Is ignored if cluster is specificed. Is "HC2" by default, because these are equivalent to Neyman standard errors (See Aronow and Samii).
#'
#' @return A list of vectors of robust standard errors.
#' @export
#'
#' @examples
#' library(randomizr)
#' library(stargazer)
#'
#' Z_1 <- complete_ra(100)
#' Y_1 <- 10 + 5*Z_1 + rnorm(100)
#' Z_2 <- complete_ra(100)
#' Y_2 <- 10 + 2*Z_2 + rnorm(100)
#'
#' fit_1 <- lm(Y_1 ~ Z_1)
#' fit_2 <- lm(Y_2 ~ Z_2)
#'
#' stargazer(fit_1, fit_2,
#'           se = makerobustseslist(fit_1, fit_2),
#'           p = makerobustpslist(fit_1, fit_2))
#'
makerobustseslist <- function(..., type = "HC2"){
  fitlist = list(...)
  return(lapply(fitlist, FUN=getrobustses, type = type) )
}

#' Prepare ps for Stargazer
#'
#' This function will prepare p-values for stargazer based on the robust standard errors. Stargazer will do this automatically, so the main purpose of this function is to set the p-value of the intercept to 1. In experimental research, the intercept is often the sample mean for the control group and a test of the null hypothesis that it is equal to do is silly to report.
#'
#' @param ... a series of model fits, separated by commas
#' @param type A string indicating the type of heterskedasticty-robust standard errors to be estimated. Is ignored if cluster is specificed. Is "HC2" by default, because these are equivalent to Neyman standard errors (See Aronow and Samii).
#'
#' @return A list of vectors of robust standard errors.
#' @export
#'
#' @examples
#' library(randomizr)
#' library(stargazer)
#'
#' Y_1 <- rnorm(100)
#' Z_1 <- complete_ra(100)
#' Y_2 <- rnorm(100)
#' Z_2 <- complete_ra(100)
#'
#' fit_1 <- lm(Y_1 ~ Z_1)
#' fit_2 <- lm(Y_2 ~ Z_2)
#'
#' stargazer(fit_1, fit_2,
#'           se = makerobustseslist(fit_1, fit_2),
#'           p = makerobustpslist(fit_1, fit_2))
#'
makerobustpslist <- function(..., type = "HC2"){
  fitlist = list(...)
  return(lapply(fitlist, FUN=getrobustps, type = type))
}
