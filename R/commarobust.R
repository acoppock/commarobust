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
se_mean <- function(x, na.rm = FALSE) {
  # Error handling from mean.default
  if (!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
    warning("argument is not numeric or logical: returning NA")
    return(NA_real_)
  }
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  n <- length(x)
  return(sd(x) / (sqrt(n)))
}


#' Robust Standard Errors
#'
#' @param fit A model object, typically created by lm, glm, or many other model fitting functions.
#' @param clust_var An optional numeric vector of length N that describes the clusters that units are in. May not include NAs. Is NULL by default. If specified, cluster-robust standard errors will be estimated.
#' @param type A string indicating the type of heterskedasticty-robust standard errors to be estimated. Is ignored if clust_var is specificed. Is "HC2" by default, because these are equivalent to Neyman standard errors (See Aronow and Samii).
#' @param alpha Used for construction of (1 - alpha) confidence intervals. Defaults to 0.05.
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
#' clust_var <- rep(letters[1:10], 10)
#' Z <- cluster_ra(clust_var = clust_var)
#'
#' fit <- lm(Y ~ Z)
#' commarobust(fit, clust_var = clust_var)
#'
commarobust <-
  function(fit,
           clust_var = NULL,
           type = "HC2",
           alpha = 0.05) {
    if (type == "BM") {
      bm <- BMlmSE(fit = fit, clust_var = clust_var)
      point_estimates <- coef(fit)
      critical_values <-
        qt(1 - alpha / 2, df = bm$dof)  # Critical value for 95% CI
      margins_of_error <- critical_values * bm$se
      ps <-
        2 * pt(q = abs(point_estimates) / bm$se,
               bm$dof,
               lower.tail = FALSE)
      ci_lower = point_estimates - margins_of_error
      ci_upper = point_estimates + margins_of_error

      return_obj <-
        cbind(
          "Estimate" = point_estimates,
          `Std. Error` = bm$se,
          `t value` = point_estimates / bm$se,
          `Pr(>|t|)` = ps,
          "ci_lower" = ci_lower,
          "ci_upper" = ci_upper,
          "dof" = bm$dof,
          "critical_value" = critical_values
        )
    } else{
      if (is.null(clust_var)) {
        return_obj <-
          lmtest::coeftest(fit, sandwich::vcovHC(fit, type = type))[]
      } else{
        M <- length(unique(clust_var))
        N <- length(clust_var)
        K <- fit$rank
        dfc <- (M / (M - 1)) * ((N - 1) / (N - K))
        uj  <-
          apply(sandwich::estfun(fit), 2, function(x)
            tapply(x, clust_var, sum))

        vcovCL <- dfc * sandwich::sandwich(fit, meat = crossprod(uj) / N)
        return_obj <- lmtest::coeftest(fit, vcovCL)[]
      }
    }
    attr(return_obj, "method") <- NULL
    return(return_obj)
  }


#' Tidy Robust Standard Errors
#'
#' @param fit A model object, typically created by lm, glm, or many other model fitting functions.
#' @param clust_var An optional numeric vector of length N that describes the clusters that units are in. May not include NAs. Is NULL by default. If specified, cluster-robust standard errors will be estimated.
#' @param type A string indicating the type of heterskedasticty-robust standard errors to be estimated. Is ignored if clust_var is specificed. Is "HC2" by default, because these are equivalent to Neyman standard errors (See Aronow and Samii).
#' @param alpha Used for construction of (1 - alpha) confidence intervals. Defaults to 0.05.
#'
#' @return A data.frame of coefficients, standard errors, t-statistics, and p-values
#' @export
#'
#' @examples
#'
#' library(randomizr)
#'
#' Y <- rnorm(100)
#' Z <- complete_ra(100)
#' fit <- lm(Y ~ Z)
#' commarobust_tidy(fit)
#'
#'
#'
#' # Clustered
#' Y <- rnorm(100)
#' clust_var <- rep(letters[1:10], 10)
#' Z <- cluster_ra(clust_var = clust_var)
#'
#' fit <- lm(Y ~ Z)
#' commarobust_tidy(fit, clust_var = clust_var)
#'
commarobust_tidy <- function(fit,
                               clust_var = NULL,
                               type = "HC2",
                               alpha = 0.05) {

  fit_r <- commarobust(fit, clust_var = clust_var, type = type, alpha = alpha)

  fit_df <- data.frame(fit_r)
  colnames(fit_df) <- c("est", "se", "t", "p")
  fit_df$term <- rownames(fit_r)
  fit_df$dv <- all.vars(formula(fit))[1]
  rownames(fit_df) <- NULL
  return(fit_df[,c("term", "est", "se", "t", "p", "dv")])
}




