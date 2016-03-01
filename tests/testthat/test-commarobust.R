test_that("commarobust works", {
  x <- rnorm(100)
  se_mean(x)


  library(randomizr)

  Y <- rnorm(100)
  Z <- complete_ra(100)
  fit <- lm(Y ~ Z)

  commarobust(fit)

  # Clustered
  Y <- rnorm(100)
  clust <- rep(letters[1:10], 10)
  Z <- cluster_ra(clust)

  fit <- lm(Y ~ Z)
  commarobust(fit, cluster = clust)


  library(randomizr)
  library(stargazer)



  Z_1 <- complete_ra(100)
  Y_1 <- 10 + 5*Z_1 + rnorm(100)
  Z_2 <- complete_ra(100)
  Y_2 <- 10 + 2*Z_2 + rnorm(100)

  fit_1 <- lm(Y_1 ~ Z_1)
  fit_2 <- lm(Y_2 ~ Z_2)

  stargazer(fit_1, fit_2,
            se = makerobustseslist(fit_1, fit_2),
            p = makerobustpslist(fit_1, fit_2))

})
