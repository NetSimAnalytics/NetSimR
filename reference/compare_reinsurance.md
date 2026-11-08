# Compare reinsurance programmes

Simulates the claims of a portfolio once and puts them through each of
several reinsurance programmes (a quota share, an excess of loss layer
on each claim and an aggregate stop loss, alone or together), to compare
what each costs with the capital it saves: the expected ceded losses,
the reinsurance premiums, the margin paid to the reinsurers, the net
result, its value at risk and the capital it needs.

## Usage

``` r
compare_reinsurance(
  frequency,
  frequency_params,
  severity,
  severity_params,
  programmes,
  premium,
  n = 10000,
  level = 0.995,
  loading = 0.25,
  coc_rate = 0.1,
  seed = NULL
)
```

## Arguments

- frequency, frequency_params:

  The claim count distribution and its parameters, as in
  [`simulate_claims`](https://netsimanalytics.github.io/NetSimR/reference/simulate_claims.md),
  such as "Poisson" (lambda) or "Negative Binomial" (r, beta).

- severity, severity_params:

  The claim size distribution and its parameters, as in
  [`simulate_claims`](https://netsimanalytics.github.io/NetSimR/reference/simulate_claims.md),
  such as "LogNormal" (meanlog, sdlog) or "Pareto" (alpha, x_m).

- programmes:

  A named list of programmes, each a list with any of: `quota_share`
  (the share ceded) and `commission` (the ceding commission, a share of
  the ceded premium); `xl`, a list with the `deductible`, `limit`,
  `reinstatements` (`Inf`, the default, for unlimited and free),
  `reinstatement_rate` (1 by default) and optionally `premium`; and
  `stop_loss`, a list with the `attachment`, `limit` and optionally
  `premium`. "No reinsurance" is added as the first programme.

- premium:

  The subject premium of the year.

- n:

  The number of simulated years.

- level:

  The level of the value at risk, such as 0.995.

- loading:

  The share of a priced layer's or stop loss's premium for the
  reinsurer's expenses and profit.

- coc_rate:

  The cost of capital a year, for the value of the capital relief.

- seed:

  A whole number for a reproducible run, or `NULL`. The stream of the R
  session is restored after a run with a seed.

## Value

An object of class `netsimr_programmes`, a list with `summary`, a data
frame with a row per programme (the expected ceded loss, the reinsurance
premium and margin, the net mean result and its standard deviation, the
probability of a loss-making year, the value at risk of the net result,
the capital, the capital relief and its value), `results`, the simulated
net result of each programme (a column each), and the settings.

## Details

Every programme sees the same simulated claims. In each, the quota share
cedes its share of every claim first; the excess of loss layer then
recovers from each retained claim the part above its deductible, up to
its limit, and at most \\(1 + \textrm{reinstatements})\\ limits a year;
the stop loss last recovers the part of the year's retained claims above
its attachment, up to its limit.

The premium of a quota share is its share of the subject premium, less
its ceding commission. A layer's or stop loss's premium is given or,
when it is not, priced at the expected ceded loss with `loading` as the
share of the premium for the reinsurer's expenses and profit
([`technical_premium`](https://netsimanalytics.github.io/NetSimR/reference/technical_premium.md),
with the reinstatements the simulations use). The excess of loss layer's
reinstatement premiums are paid pro rata to the cover reinstated.

The net result of a year is the subject premium less the reinsurance
premiums paid, plus the ceding commission, less the retained claims
(before other expenses). The capital a programme needs is the mean net
result less its \\1 - \textrm{level}\\ quantile: what a bad year at that
level costs against an average one. The capital relief is the capital
without reinsurance less the programme's, and its value is `coc_rate` of
the relief less the margin paid for it: positive when the programme
saves more in the cost of capital than it costs.

## See also

[`simulate_claims`](https://netsimanalytics.github.io/NetSimR/reference/simulate_claims.md),
[`technical_premium`](https://netsimanalytics.github.io/NetSimR/reference/technical_premium.md)
and
[`run_shiny_reinsurance_pricing_tool`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_reinsurance_pricing_tool.md),
which compares programmes on its Programmes tab.

## Examples

``` r
programmes <- list(
  `Quota share 30%` = list(quota_share = 0.3, commission = 0.25),
  `XL 4m xs 1m` = list(xl = list(deductible = 1e6, limit = 4e6, reinstatements = 1)),
  `Stop loss 30m xs 45m` = list(stop_loss = list(attachment = 45e6, limit = 30e6))
)
cmp <- compare_reinsurance("Poisson", 200, "LogNormal", c(11, 1.5), programmes,
                           premium = 60e6, n = 5000, seed = 1)
cmp
#> Reinsurance programmes, 5,000 simulated years, capital at 99.5%
#> 
#>             programme      ceded    margin mean_result    capital     relief
#>        No reinsurance          0         0  23,166,213 30,032,694          0
#>       Quota share 30% 11,050,136 2,449,864  20,716,349 21,022,886  9,009,808
#>           XL 4m xs 1m  5,145,233 1,715,078  21,451,135 27,923,811  2,108,883
#>  Stop loss 30m xs 45m    785,492   261,831  22,904,382  8,951,704 21,080,989
#>       value
#>           0
#>  -1,548,883
#>  -1,504,190
#>   1,846,268
```
