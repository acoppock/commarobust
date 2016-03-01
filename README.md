<!-- README.md is generated from README.Rmd. Please edit that file -->
The `commarobust` pacakge does two things:

1.  With the `commarobust()` function, you can easily estimate robust standard errors on your model objects. Almost as easy as Stata!

2.  You can easily prepare your standard errors for inclusion in a stargazer table with `makerobustseslist()`. I'm open to better names for this function.

Install from Github!

``` r
install.packages("devtools")
devtools::install_github("acoppock/commarobust")
```

Check it out:

``` r
library(commarobust)
library(randomizr) # For easy random assignments
Z <- complete_ra(100)
Y <- 5 + 10*Z + rnorm(100)
fit <- lm(Y ~ Z)
commarobust(fit)
#>              Estimate Std. Error  t value     Pr(>|t|)
#> (Intercept)  4.797376  0.1349279 35.55510 8.201968e-58
#> Z           10.290971  0.2028367 50.73526 3.696001e-72
```

And now in Stargazer. See how the intercept doesn't have stars even though the control group mean is statistically significantly larger than zero? Nice!

``` r
library(stargazer)
Z_1 <- complete_ra(100)
Y_1 <- 10 + 5*Z_1 + rnorm(100)
Z_2 <- complete_ra(100)
Y_2 <- 10 + 2*Z_2 + rnorm(100)

fit_1 <- lm(Y_1 ~ Z_1)
fit_2 <- lm(Y_2 ~ Z_2)

stargazer(fit_1, fit_2,
          se = makerobustseslist(fit_1, fit_2),
          p = makerobustpslist(fit_1, fit_2), type = "html")
```

<table style="text-align:center">
<tr>
<td colspan="3" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="2">
<em>Dependent variable:</em>
</td>
</tr>
<tr>
<td>
</td>
<td colspan="2" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
Y\_1
</td>
<td>
Y\_2
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(1)
</td>
<td>
(2)
</td>
</tr>
<tr>
<td colspan="3" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Z\_1
</td>
<td>
5.407<sup>\*\*\*</sup>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.206)
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Z\_2
</td>
<td>
</td>
<td>
1.899<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
(0.207)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Constant
</td>
<td>
9.767
</td>
<td>
10.039
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.150)
</td>
<td>
(0.151)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td colspan="3" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Observations
</td>
<td>
100
</td>
<td>
100
</td>
</tr>
<tr>
<td style="text-align:left">
R<sup>2</sup>
</td>
<td>
0.875
</td>
<td>
0.463
</td>
</tr>
<tr>
<td style="text-align:left">
Adjusted R<sup>2</sup>
</td>
<td>
0.874
</td>
<td>
0.458
</td>
</tr>
<tr>
<td style="text-align:left">
Residual Std. Error (df = 98)
</td>
<td>
1.030
</td>
<td>
1.033
</td>
</tr>
<tr>
<td style="text-align:left">
F Statistic (df = 1; 98)
</td>
<td>
688.468<sup>\*\*\*</sup>
</td>
<td>
84.532<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td colspan="3" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
<em>Note:</em>
</td>
<td colspan="2" style="text-align:right">
<sup>*</sup>p&lt;0.1; <sup>**</sup>p&lt;0.05; <sup>***</sup>p&lt;0.01
</td>
</tr>
</table>
