getrobustses <- function(fit, type = "HC2") {
  robustfit <- commarobust(fit, type = type)
  return(robustfit[, 2])
}

getrobustps <- function(fit, type = "HC2") {
  robustfit <- commarobust(fit, type = type)
  ps <- robustfit[, 4]
  ps["(Intercept)"] <- 1
  return(ps)
}

#' Prepare Ses for Stargazer
#'
#' This function will prepare heteroskedasticity-robust standard errors for stargazer. Currently, it does not handle cluster-robust standard errors because there is no nice way to pass model-specific vectors of clusters.
#'
#' @param ... a series of model fits, separated by commas
#' @param type A string indicating the type of heterskedasticty-robust standard errors to be estimated. Is ignored if clust_var is specificed. Is "HC2" by default, because these are equivalent to Neyman standard errors (See Aronow and Samii).
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
makerobustseslist <- function(..., type = "HC2") {

  # check list type

  fitlist = list(...)

  if(length(fitlist) == 1){
    fit_list_classes <- sapply(fitlist[[1]], class)
    if(all(fit_list_classes == "lm")){
      fitlist <- fitlist[[1]]
    }
  }

  return(lapply(fitlist, FUN = getrobustses, type = type))
}

#' Prepare ps for Stargazer
#'
#' This function will prepare p-values for stargazer based on the robust standard errors. Stargazer will do this automatically, so the main purpose of this function is to set the p-value of the intercept to 1. In experimental research, the intercept is often the sample mean for the control group and a test of the null hypothesis that it is equal to do is silly to report.
#'
#' @param ... a series of model fits, separated by commas
#' @param type A string indicating the type of heterskedasticty-robust standard errors to be estimated. Is ignored if clust_var is specificed. Is "HC2" by default, because these are equivalent to Neyman standard errors (See Aronow and Samii).
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
makerobustpslist <- function(..., type = "HC2") {
  fitlist = list(...)
  if(length(fitlist) == 1){
    fit_list_classes <- sapply(fitlist[[1]], class)
    if(all(fit_list_classes == "lm")){
      fitlist <- fitlist[[1]]
    }
  }

  return(lapply(fitlist, FUN = getrobustps, type = type))
}

