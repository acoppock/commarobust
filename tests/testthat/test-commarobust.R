test_that("se_mean", {
  x <- rnorm(100)
  se_mean(x)
})

test_that("BMlmSE",{
  library(randomizr)

  N <- 1000

  Z <- complete_ra(N)
  X <- rnorm(N)
  X_c <- X - mean(X)
  Y <- rnorm(N) + .5*Z + .4*X


  fit <- lm(Y ~ Z*X_c)

  predict.lm(fit, newdata = data.frame(Z = 1, X_c = 0)) -

    BMlmSE(fit, ell = c(0, 1, 0, 0))

  predict.lm(fit, newdata = data.frame(Z = 0, X_c = 0))
})


test_that("commarobusts",{


  fit_1 <- lm(Y ~ Z)
  fit_2 <- lm(Y ~ 1, subset = Z ==1)
  fit_3 <- lm(Y ~ 1, subset = Z ==0)

  commarobust(fit_1)
  commarobust(fit_2)
  commarobust(fit_3)
})

test_that("clustered",{
  # Clustered
  Y <- rnorm(100)
  clust <- rep(letters[1:10], 10)
  Z <- cluster_ra(clust)

  fit <- lm(Y ~ Z)
  commarobust(fit, clust_var = clust)

})


test_that("stargazer",{

  library(stargazer)
  library(randomizr)

  Z_1 <- complete_ra(100)
  Y_1 <- 10 + 5*Z_1 + rnorm(100)
  Z_2 <- complete_ra(100)
  Y_2 <- 10 + 2*Z_2 + rnorm(100)

  fit_1 <- lm(Y_1 ~ Z_1)
  fit_2 <- lm(Y_2 ~ Z_2)

  stargazer(fit_1, fit_2,
            se = makerobustseslist(fit_1, fit_2),
            p = makerobustpslist(fit_1, fit_2))

  fit_list <- list(fit_1, fit_2)

  stargazer(fit_list,
            se = makerobustseslist(fit_list),
            p = makerobustpslist(fit_list))


})
