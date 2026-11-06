# NetSimR

[![R-CMD-check](https://github.com/NetSimAnalytics/NetSimR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NetSimAnalytics/NetSimR/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/NetSimR)](https://CRAN.R-project.org/package=NetSimR)
[![Codecov test coverage](https://codecov.io/gh/NetSimAnalytics/NetSimR/graph/badge.svg)](https://app.codecov.io/gh/NetSimAnalytics/NetSimR)

NetSimR is an R package of actuarial functions for non-life insurance and
reinsurance pricing, reserving and capital modelling, with four Shiny tools
for simulating claims, fitting distributions and GLMs, and reserving. Its
website, with every help page and the vignettes, is
<https://netsimanalytics.github.io/NetSimR/>.

## Installation

```r
# the released version from CRAN
install.packages("NetSimR")

# the development version from GitHub
# install.packages("remotes")
remotes::install_github("NetSimAnalytics/NetSimR")
```

## What it does

- **Capped means, exposure curves and increased limit factors (ILFs)** for
  LogNormal, Gamma, Pareto, sliced LogNormal-Pareto and sliced Gamma-Pareto
  claim sizes.
- **Sliced distributions**: mean, density, cdf and quantile function of the
  sliced LogNormal-Pareto and Gamma-Pareto distributions.
- **Pure IBNR exposure** from a LogNormal or Gamma reporting delay.
- **Claims simulation** with a frequency-severity model, Pareto tail slices,
  a severity cap, each-and-every-loss layers with reinstatements and their
  premiums, aggregate layers, and each period's largest claim (OEP).
- **Claims reserving**: the chain ladder with Mack's standard errors and tail
  factors, Bornhuetter-Ferguson, Benktander and Cape Cod, a bootstrap of the
  reserves' distribution, the one-year view (Merz-Wuthrich) and the Munich
  chain ladder of paid and incurred claims.
- **Four Shiny tools**: a claims simulator with reports and run comparison; a
  distribution fitting tool for claim counts and sizes, including claims
  reported only above a threshold, whose fits it saves as simulator settings;
  a GLM fitting tool with a rating factor table; and a reserving tool.

## Examples

```r
library(NetSimR)

# mean claim size capped at 600 for a Pareto with scale 200 and shape 1.2
# (the older CamelCase names, such as ParetoCappedMean(), still work)
pareto_capped_mean(600, 200, 1.2)

# increased limit factor from 700 to 1,000 for Gamma claim sizes
ilf_gamma(700, 1000, 1, 0.0005)

# Gamma body sliced at 3,000 with a Pareto tail of shape 1.4
psliced_gamma_pareto(5000, 1.2, 0.0004, 3000, 1.4)

# pure IBNR exposure (in days) of three periods, with a Gamma reporting delay
pure_ibnr_gamma(
  as.Date(c("2006-01-01", "2006-07-01", "2007-01-01")),
  as.Date(c("2006-12-31", "2007-06-30", "2007-12-31")),
  as.Date("2007-10-30"),
  shape = 7, rate = 0.15
)

# 10,000 years of Poisson claim counts with LogNormal claim sizes, a Pareto tail
# above 100,000, and a layer of 50,000 excess of 20,000 on each claim with two
# reinstatements at 100% of an upfront premium of 10,000
ceded <- simulate_claims(
  10000, "Poisson", 3, "LogNormal", c(meanlog = 8, sdlog = 1.5), seed = 1,
  pareto_thresholds = 100000, pareto_alphas = 1.5,
  eel_layer = "limited", eel_deductible = 20000, eel_limit = 50000,
  eel_reinstatements = 2, eel_premium = 10000
)
mean(ceded$total_claims)
mean(ceded$reinstatement_premium)

# chain ladder reserves of the RAA triangle with Mack's standard errors and a
# fitted tail, then the 99.5th percentile of the bootstrapped total reserve
chain_ladder(raa_triangle, tail = "exponential")
bootstrap_chain_ladder(raa_triangle, n = 10000, seed = 1)$total[["p99.5"]]

# the one-year view of the same reserves, and paid and incurred claims projected together
merz_wuthrich(raa_triangle)$total
munich_chain_ladder(quarg_mack_paid, quarg_mack_incurred)$total
```

## Shiny tools

```r
run_shiny_simulator()                  # claims simulator
run_shiny_distribution_fitting_tool()  # frequency and severity fits
run_shiny_glm_fitting_tool()           # generalised linear models
run_shiny_reserving_tool()             # claims reserving
```

## Vignettes

Four vignettes set out the theory behind the functions and work through
examples:

- *Capped means, exposure curves and increased limit factors*
- *Sliced LogNormal-Pareto and Gamma-Pareto distributions*
- *Pure IBNR exposure from a reporting delay distribution*
- *Simulating claims with simulate_claims()*

They are on the package's [CRAN page](https://CRAN.R-project.org/package=NetSimR),
and `browseVignettes("NetSimR")` opens the copies installed with the package
(a development version installed from GitHub has them only when installed with
`build_vignettes = TRUE`).

## References

The methods are described in articles by Yiannis Parizas in *The Actuary*:
[Free for All](https://www.theactuary.com/2023/03/02/free-all) (2023),
[Escaping the triangle](https://www.theactuary.com/features/2019/06/2019/06/05/escaping-triangle)
(2019) and
[Taken to excess](https://www.theactuary.com/features/2019/03/2019/03/06/taken-excess)
(2019).
