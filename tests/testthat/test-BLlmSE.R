## For this example, assume:
## - We have an experiment with simple or complete randomization of subjects.
##   10 subjects are assigned to treatment and 5 to control.
## - Our PAP specified that we would estimate ATE using an OLS regression of the
##   outcome on the treatment dummy and a single covariate, but did not specify
##   how we would estimate the SE or construct a CI.

# Generate fake data

library(testthat)
library(commarobust)

set.seed(1234567)
treatment <- c(rep(1, 10), rep(0, 5))
covariate <- rnorm(15)
outcome <- treatment + rnorm(15)

# Use lm() to fit an OLS regression

ols.fit <- lm(outcome ~ treatment* covariate)




# Use BMlmSE() to compute the BM SE and degrees of freedom

## The argument ell must have the same length as the vector of regression coefficients.
## In this example, ell = c(0, 1, 0) indicates that we only want to compute
## the Bell-McCaffrey SE and degrees of freedom for the linear combination
##   0 * intercept + 1 * (coef on treatment) + 0 * (coef on covariate),
## i.e., the coefficient on treatment.

commarobust(ols.fit)
bm <- BMlmSE(ols.fit)
bm$est
bm$se

bm <- BMlmSE(ols.fit, ell = c(1, 1, 0, 0))
bm$est
bm$se

as.numeric((bm$est))
predict(ols.fit, newdata = data.frame(treatment = 1, covariate = mean(covariate[treatment ==1])))

mean(outcome[treatment == 1])

# Construct a 95% confidence interval for the average treatment effect


point.estimates <- coef(ols.fit)

critical.value <- qt(0.975, df = bm$dof)  # Critical value for 95% CI
margin.of.error <- critical.value * bm$se



cis = cbind(point.estimates - margin.of.error, point.estimates + margin.of.error)
colnames(cis) <- c("lower", "upper")

point.estimates
bm$se           # HC2 robust SE
bm$dof          # Bell-McCaffrey degrees of freedom
cis              # Bell-McCaffrey confidence interval
