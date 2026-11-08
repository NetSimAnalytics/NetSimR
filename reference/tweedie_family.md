# Tweedie family for generalised linear models

A Tweedie family for [`stats::glm()`](https://rdrr.io/r/stats/glm.html),
for responses with a mass at zero and a continuous positive part, such
as the loss cost (claims amount per unit of exposure) of insurance
policies, where most policies have no claim. Its variance is \\\phi
\mu^p\\ for a power \\1 \< p \< 2\\: a compound Poisson distribution of
Gamma claim sizes, the Poisson frequency and Gamma severity of a pure
premium in one model.

## Usage

``` r
tweedie_family(power = 1.5, link = "log")
```

## Arguments

- power:

  The variance power \\p\\, between 1 and 2 (exclusive); 1.5 by default.
  Values near 1 behave like a Poisson model of the claim count, values
  near 2 like a Gamma model of the amount.

- link:

  The link: "log" (the default, giving multiplicative relativities),
  "identity", "sqrt" or "inverse".

## Value

A family object, as
[`stats::poisson()`](https://rdrr.io/r/stats/family.html) returns, with
the family name "Tweedie" and the power as `variance_power`.

## Details

The family is quasi-likelihood: the coefficients and their standard
errors are those of the Tweedie distribution, with the dispersion
\\\phi\\ estimated from the Pearson residuals, but its density has no
closed form, so [`AIC()`](https://rdrr.io/r/stats/AIC.html) is `NA`.
Compare Tweedie models by their deviance, ideally on rows they were not
fitted to. The unit deviance is \$\$2 \left(
\frac{y^{2-p}}{(1-p)(2-p)} - \frac{y \mu^{1-p}}{1-p} +
\frac{\mu^{2-p}}{2-p} \right).\$\$

## See also

[`run_shiny_glm_fitting_tool`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_glm_fitting_tool.md),
which fits it from a form.

## Examples

``` r
set.seed(1)
n <- 2000
age <- sample(18:80, n, replace = TRUE)
claims <- rpois(n, exp(-2 + 0.02 * age))
loss <- vapply(claims, function(k) sum(rgamma(k, shape = 2, scale = 500)), numeric(1))
fit <- glm(loss ~ age, family = tweedie_family(1.5))
exp(coef(fit))
#> (Intercept)         age 
#>  165.849246    1.017522 
summary(fit)$dispersion
#> [1] 80.73336
```
