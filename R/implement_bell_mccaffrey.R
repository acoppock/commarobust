## This is a slightly modified version of Michal Kolesar's BM_StandardErrors.R
## The original file was downloaded from:
##   https://github.com/kolesarm/Robust-Small-Sample-Standard-Errors
##   (Latest commit 2a80873 on Aug 26, 2015)
## We made minor changes for efficiency, as well as two changes that affect the
## code's functionality:
## 1) MatSqrtInverse() now stops with an error message if its argument is not of
##    full rank:
##    "Bell-McCaffrey SE undefined. This happens, e.g., when a dummy regressor is 1
##     for one cluster and 0 otherwise."
## 2) The list returned by BMlmSE() now includes "se".

# The MIT License (MIT)
#
# Copyright (c) 2015 Michal Kolesar
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

#' Compute Bell-McCaffrey Standard Errors
#'
#' @param fit A model object, typically created by lm, glm, or many other model fitting functions.
#' @param clust_var A vector of length N that describes the clusters to which each cluster belongs. If `clust_var` is supplied, the function uses Bell and McCaffrey (2002)'s bias-reduced cluster-robust variance estimator. If `clust_var` is left unspecified (meaning that the user does not want clustered SEs), the function uses the HC2 robust variance estimator.
#' @param ell An optional vector specifying a linear combination of coefficients to compute an SE and degrees of freedom for. `ell` must have the same length as the vector of regression coefficients. For example, if `fit` is an object returned by `lm(outcome ~ treatment + covariate)`, then `ell = c(0, 1, 0)` specifies that we are only interested in the coefficient on `treatment`. If `ell` is left unspecified, the function computes SEs and degrees of freedom for all coefficients. (The adjusted degrees of freedom will generally be different for each coefficient, unlike the classical OLS degrees of freedom.)
#' @param IK logical (optional, FALSE by default). If `clust_var` is unspecified, `IK` has no effect on the results. If `clust_var` is supplied, `IK` determines whether the degrees of freedom are computed using the method described in Imbens and Kolesár (forthcoming) (if `IK` is `TRUE` or unspecified) or the method in Bell and McCaffrey (2002) (if `IK` is `FALSE`).
#'
#' @return A list
#' @export
#'
#' @examples
#'
#' rnorm(1)
#'
BMlmSE <- function(fit,
                   clust_var = NULL,
                   ell = NULL,
                   IK = TRUE) {

  require(sandwich)

  X <- model.matrix(fit)
  sum.model <- summary.lm(fit)
  n <- sum(sum.model$df[1:2])
  K <- fit$rank
  XXinv <- sum.model$cov.unscaled # XX^{-1}
  u <- residuals(fit)

  df <- function(GG) {
    # Compute DoF given G'*Omega*G
    sum(diag(GG)) ^ 2 / sum(GG * GG)
  }

  if (is.null(clust_var)) {
    # no clustering
    Vhat <- vcovHC(fit, type = "HC2")
    Vhat.Stata <- Vhat * NA

    M <- diag(n) - X %*% XXinv %*% t(X)       # annihilator matrix
    GOG <- function(ell) {
      # G'*Omega*G
      Xtilde <- drop(X %*% XXinv %*% ell / sqrt(diag(M)))
      crossprod(M * Xtilde)
    }
  } else {
    if (!is.factor(clust_var))
      stop("'clust_var' must be a factor")

    ## Stata
    S <- length(levels(clust_var)) # number clusters
    uj <- apply(u * X, 2, function(x)
      tapply(x, clust_var, sum))
    Vhat.Stata <-
      S / (S - 1) * (n - 1) / (n - K) * sandwich(fit, meat = crossprod(uj) /
                                                   n)

    ## LZ2
    tXs <- function(s) {
      Xs <- X[clust_var == s, , drop = FALSE]
      MatSqrtInverse(diag(NROW(Xs)) - Xs %*% XXinv %*% t(Xs)) %*% Xs
    }
    tX <- lapply(levels(clust_var), tXs) # list of matrices

    tu <- split(u, clust_var)
    tutX <-
      sapply(seq_along(tu), function(i)
        crossprod(tu[[i]], tX[[i]]))
    Vhat <- sandwich(fit, meat = tcrossprod(tutX) / n)

    ## DOF adjustment
    tHs <- function(s) {
      Xs <- X[clust_var == s, , drop = FALSE]
      index <- which(clust_var == s)
      ss <- outer(rep(0, n), index)     # n x ns matrix of 0
      ss[cbind(index, 1:length(index))] <- 1
      ss - X %*% XXinv %*% t(Xs)
    }
    tH <- lapply(levels(clust_var), tHs) # list of matrices

    Moulton <- function() {
      ## Moulton estimates
      ns <- tapply(u, clust_var, length)
      ssr <- sum(u ^ 2)
      rho <- max((sum(sapply(seq_along(tu), function(i)
        sum(tu[[i]] %o% tu[[i]]))) - ssr) / (sum(ns ^ 2) - n), 0)
      c(sig.eps = max(ssr / n - rho, 0), rho = rho)
    }

    GOG <- function(ell) {
      G <- sapply(seq_along(tX),
                  function(i)
                    tH[[i]] %*% tX[[i]] %*% XXinv %*% ell)
      GG <- crossprod(G)

      if (IK == TRUE) {
        # IK method
        Gsums <-
          apply(G, 2, function(x)
            tapply(x, clust_var, sum)) # Z'*G
        GG <- Moulton()[1] * GG + Moulton()[2] * crossprod(Gsums)
      }
      GG
    }
  }

  if (!is.null(ell)) {
    est <- drop(coef(fit) %*% ell)
    se <- drop(sqrt(crossprod(ell, Vhat) %*% ell))
    dof <- df(GOG(ell))
    se.Stata <- drop(sqrt(crossprod(ell, Vhat.Stata) %*% ell))
  } else {
    est <- coef(fit)
    se <- sqrt(diag(Vhat))
    dof <- sapply(seq(K), function(k)
      df(GOG(diag(K)[, k])))
    se.Stata <- sqrt(diag(Vhat.Stata))
  }
  names(dof) <- names(se)

  return(list(
    vcov = Vhat,
    dof = dof,
    est = est,
    adj.se = se * qt(0.975, df = dof) / qnorm(0.975),
    se = se,
    se.Stata = se.Stata
  ))
}

#' @export
get_cis_BMlmSE <- function(object, alpha = 0.05){
  point_estimates <- object$est
  critical_values <- qt(1 - alpha/2, df = object$dof)  # Critical value for 95% CI
  margins_of_error <- critical_values * object$se
  ps <- 2 * pt(q = abs(point_estimates)/object$se, object$dof, lower.tail = FALSE)
  ci_lower = point_estimates - margins_of_error
  ci_upper = point_estimates + margins_of_error

  return_obj <-
    cbind("Estimate" = point_estimates,
          `Std. Error` = object$se,
          `t value` = point_estimates/object$se,
          `Pr(>|t|)` = ps,
          "ci_lower" = ci_lower,
          "ci_upper" = ci_upper,
          "dof" = object$dof,
          "critical_value" = critical_values)
  return(return_obj)
}



MatSqrtInverse <- function(A) {
  require(Matrix)
  ##  Compute the inverse square root of a matrix
  if (rankMatrix(A) < NROW(A)) {
    stop(
      paste0(
        'Bell-McCaffrey SE undefined. This happens, e.g., when a dummy regressor is 1 ',
        'for one cluster and 0 otherwise.'
      )
    )
  }
  ei <- eigen(A, symmetric = TRUE)
  d2 <- 1 / sqrt(ei$values)
  ## diag(d2) is d2 x d2 identity if d2 is scalar, instead we want 1x1 matrix
  ei$vectors %*% (if (length(d2) == 1)
    d2
    else
      diag(d2)) %*% t(ei$vectors)
}
