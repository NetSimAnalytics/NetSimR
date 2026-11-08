# Simulate a portfolio of dependent lines

Simulates the annual losses of several lines of business, each with its
own frequency and severity, and joins them with a Gaussian or t copula,
for the capital of the portfolio: its value at risk and tail value at
risk, the diversification between the lines, and each line's share of
the capital.

## Usage

``` r
simulate_portfolio(
  lines,
  correlation = 0,
  copula = c("gaussian", "t"),
  df = 4,
  n = 10000,
  level = 0.995,
  seed = NULL
)
```

## Arguments

- lines:

  A named list with one element per line: a list of arguments of
  [`simulate_claims`](https://netsimanalytics.github.io/NetSimR/reference/simulate_claims.md)
  (without `n_sims` and `seed`), whose `total_claims` are the line's
  losses, or a numeric vector of `n` simulated losses.

- correlation:

  The correlation between the lines' copula draws: a matrix with a row
  and a column per line, symmetric and positive definite with ones on
  the diagonal, or one number for every pair of lines.

- copula:

  `"gaussian"` (the default) or `"t"`.

- df:

  The degrees of freedom of the t copula: the lower, the stronger the
  dependence in the tail.

- n:

  The number of simulations.

- level:

  The level of the value at risk and the tail value at risk, such as
  0.995 (the default) for 1 in 200 years.

- seed:

  A whole number for a reproducible run, or `NULL` (the default) to use
  the current random number stream. The stream of the R session is
  restored after a run with a seed.

## Value

An object of class `netsimr_portfolio`, a list with `losses`, a data
frame with a column per line and the `total`, one row per simulation;
`summary`, a data frame with the mean, standard deviation, VaR and TVaR
of each line and of the total, each line's capital (co-TVaR) and its
share; `diversification`, the benefit in amount and as a share of the
lines' VaR; and the copula settings.

## Details

Each line is simulated on its own, by
[`simulate_claims`](https://netsimanalytics.github.io/NetSimR/reference/simulate_claims.md)
(or given as a vector of simulated losses). The lines are then joined by
reordering: `n` draws from the copula give, for each line, the rank its
losses take in each simulation, and the line's simulated losses are put
in that order. Each line keeps exactly the distribution it was simulated
with, and the ranks of the lines follow the copula: with the Gaussian
copula, normal draws with the given correlation matrix; with the t
copula, the same divided by a shared \\\sqrt{W / \nu}\\, \\W\\
chi-squared with \\\nu\\ degrees of freedom, which makes large losses in
several lines at once more likely (tail dependence). This is the
reordering used in capital models (the method of Iman and Conover with
the copula's ranks).

The value at risk of the total at level \\p\\ is its \\p\\ quantile, and
the tail value at risk the mean of the totals at or above it. The
diversification benefit is the sum of the lines' VaR less the total's
VaR. Each line's capital is its mean loss in the simulations whose total
is at or above the total's VaR (co-TVaR, the Euler allocation of the
TVaR), so the lines' capitals add up to the total's TVaR.

## References

Iman, R. L. and Conover, W. J. (1982). A distribution-free approach to
inducing rank correlation among input variables. Communications in
Statistics - Simulation and Computation 11(3), 311-334.

## See also

[`simulate_claims`](https://netsimanalytics.github.io/NetSimR/reference/simulate_claims.md).

## Examples

``` r
lines <- list(
  motor = list(frequency = "Poisson", frequency_params = 50,
               severity = "LogNormal", severity_params = c(8, 1)),
  property = list(frequency = "Poisson", frequency_params = 5,
                  severity = "Pareto", severity_params = c(1.8, 20000))
)
portfolio <- simulate_portfolio(lines, correlation = 0.3, n = 20000, seed = 1)
portfolio
#> Portfolio of 2 lines, 20,000 simulations, Gaussian copula
#> 
#>      line    mean      sd VaR 99.5% TVaR 99.5%   capital share
#>     motor 245,673  57,400   431,722    474,597   307,704 0.134
#>  property 223,855 217,936 1,122,307  1,986,134 1,982,527 0.866
#>     total 469,528 236,837 1,410,849  2,290,232 2,290,232 1.000
#> 
#> Diversification benefit: 143,180 (9.2% of the lines' VaR of 1,554,029)

# stronger dependence in the tail with a t copula
tail <- simulate_portfolio(lines, correlation = 0.3, copula = "t", df = 3, n = 20000, seed = 1)
tail$diversification
#> stand_alone_var       total_var         benefit   benefit_share 
#>    1.554029e+06    1.444839e+06    1.091902e+05    7.026266e-02 
```
