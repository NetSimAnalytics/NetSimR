# Simulate claims with a frequency-severity model

A simpler interface to
[`simulate_function`](https://netsimanalytics.github.io/NetSimR/reference/simulate_function.md),
with short argument names and defaults for everything optional. Leaving
an option out (NULL) switches the feature off: no seed, no Pareto
slices, no cap, no reinstatement limit.

## Usage

``` r
simulate_claims(
  n_sims,
  frequency,
  frequency_params,
  severity,
  severity_params,
  seed = NULL,
  truncate_at_zero = FALSE,
  pareto_thresholds = NULL,
  pareto_alphas = NULL,
  severity_cap = NULL,
  eel_layer = "none",
  eel_deductible = NULL,
  eel_limit = NULL,
  eel_reinstatements = NULL,
  eel_premium = NULL,
  eel_reinstatement_rates = 1,
  agg_layer = "none",
  agg_deductible = NULL,
  agg_limit = NULL,
  parallel = FALSE,
  workers = NULL,
  chunk_size = NULL,
  gross = TRUE,
  shortcuts = TRUE,
  progress = NULL,
  largest_claims = FALSE
)
```

## Arguments

- n_sims:

  Number of simulations (e.g. years).

- frequency:

  Name of the claim count distribution; see Details.

- frequency_params:

  Parameters of the claim count distribution, either all unnamed in the
  order of Details or all named.

- severity:

  Name of the claim size distribution; see Details.

- severity_params:

  Parameters of the claim size distribution, either all unnamed in the
  order of Details or all named.

- seed:

  A whole number for a reproducible run. NULL (the default) uses the
  current random number stream, so
  [`set.seed()`](https://rdrr.io/r/base/Random.html) before the call
  also makes the run reproducible.

- truncate_at_zero:

  TRUE to draw Normal claim sizes from the Normal distribution truncated
  at zero, so no claim is negative. Only used with the Normal severity.

- pareto_thresholds:

  Increasing claim sizes above which the severity tail is replaced by
  Pareto slices, one per slice (at most six). NULL (the default) for no
  slices.

- pareto_alphas:

  The Pareto alpha of each slice, one per threshold.

- severity_cap:

  The largest amount a single claim can reach, or NULL (the default) for
  no cap.

- eel_layer:

  The each-and-every-loss layer: "none" (the default), "unlimited",
  "limited" or "exclude".

- eel_deductible:

  The deductible of the each-and-every-loss layer.

- eel_limit:

  The limit of a "limited" or "exclude" each-and-every-loss layer.

- eel_reinstatements:

  The number of reinstatements of a "limited" each-and-every-loss layer,
  so it pays at most `(eel_reinstatements + 1) * eel_limit` per
  simulation. NULL (the default) for unlimited reinstatements.

- eel_premium:

  The upfront premium of the each-and-every-loss layer, which prices the
  reinstatements used: given, the results gain the column
  `reinstatement_premium`. Needs `eel_reinstatements`. NULL (the
  default) for none.

- eel_reinstatement_rates:

  The premium rate of each reinstatement as a share of `eel_premium`:
  one rate for all of them, or one per reinstatement. Defaults to 1
  (100%). See
  [`reinstatement_premium`](https://netsimanalytics.github.io/NetSimR/reference/reinstatement_premium.md).

- agg_layer:

  The aggregate layer: "none" (the default), "unlimited", "limited" or
  "exclude".

- agg_deductible:

  The deductible of the aggregate layer.

- agg_limit:

  The limit of a "limited" or "exclude" aggregate layer.

- parallel:

  TRUE to run the chunks of simulations on parallel workers, FALSE (the
  default) to run them in this session, or "auto" to use workers only
  when that is expected to be at least a quarter faster, which for
  workers started by the call means a run of a few seconds or more.
  Results are the same either way. See
  [`simulate_function`](https://netsimanalytics.github.io/NetSimR/reference/simulate_function.md).

- workers:

  The workers of a parallel run: NULL (the default) for one per
  available core, a number of workers (both started for the call and
  shut down after it), or a cluster from
  [`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html),
  which is used and left running so that several calls share it. See
  [`simulate_function`](https://netsimanalytics.github.io/NetSimR/reference/simulate_function.md).

- chunk_size:

  The number of simulations per vectorised batch; NULL (the default)
  chooses it from the expected number of claims, aiming at about a
  million claims per batch but never fewer than 100 simulations, so
  batches are larger when the mean frequency exceeds 10,000. Results
  with a fixed seed depend on it.

- gross:

  TRUE (the default) to return the gross totals before the layers. FALSE
  allows a much faster run with an "unlimited" or "limited"
  each-and-every-loss layer, by drawing only the claims that reach it.

- shortcuts:

  TRUE (the default) to use exact shortcuts where the settings allow;
  see
  [`simulate_function`](https://netsimanalytics.github.io/NetSimR/reference/simulate_function.md).

- progress:

  An optional function called after each chunk of a sequential run with
  the fraction done and a short description.

- largest_claims:

  TRUE to add the column `largest_claim`: each simulation's largest
  single claim after the each-and-every-loss layer (the gross claim
  without one), for occurrence exceedance (OEP). Defaults to FALSE;
  every claim is then drawn, so it can be slower for very high
  frequencies.

## Value

A data frame with one row per simulation, at full precision:
`claim_counts`, `total_claims` (after the layers), `gross_claims`
(before them, unless `gross = FALSE`) and, with limited reinstatements,
`number_of_reinstatements_used`: the layer's recoveries in the
simulation divided by `eel_limit`, capped at `eel_reinstatements`. The
recoveries are taken after the aggregate deductible and limit of an
"unlimited" or "limited" aggregate layer, but before an "exclude"
aggregate layer is taken out: with an exclusion, the reinstatements are
counted on the each-and-every-loss recoveries (after the reinstatement
capacity). So three claims of 100 through a layer of 60 excess of 30
with two reinstatements and an "exclude" aggregate layer of 150 excess
of 50 give a total of 50 but 2 reinstatements used. With `eel_premium`,
`reinstatement_premium`: the premium for the reinstatements used, pro
rata to the amount reinstated.

## Details

Distributions are chosen by name; case, spaces and underscores are
ignored, so "Negative Binomial", "negative_binomial" and
"Negative_Binomial" are the same. Their parameters are given in the
order below, or named with these names:

|  |  |  |
|----|----|----|
| **Distribution** | **Type** | **Parameters** |
| Poisson | frequency | lambda |
| Negative_Binomial | frequency | r, beta (Gamma shape and scale of the Poisson mean) |
| Binomial | frequency | n, p |
| Fixed_number_of_Counts | frequency | count |
| Normal | severity | mean, sd |
| LogNormal | severity | meanlog, sdlog |
| Gamma | severity | shape, scale |
| Exponential | severity | rate |
| Pareto | severity | alpha, x_m (minimum) |
| Fixed_Severity | severity | amount |

Layers are "none", "unlimited" (everything above the deductible),
"limited" (the limit excess of the deductible) or "exclude" (the claims
with that layer removed). The each-and-every-loss layer applies to every
claim; the aggregate layer applies to each simulation's total after the
each-and-every-loss layer. The order is: severity cap -\>
each-and-every-loss layer per claim -\> sum over the simulation -\>
aggregate deductible -\> aggregate limit and reinstatement capacity. So
with a "limited" each-and-every-loss layer and an "unlimited" or
"limited" aggregate layer, the ceded total is
`min(max(S - agg_deductible, 0), agg_limit, (eel_reinstatements + 1) * eel_limit)`,
where S is the sum of the layer's recoveries; three claims of 100
through a layer of 100 excess of 0 with no reinstatements and an
aggregate deductible of 50 cede 100. An "exclude" aggregate layer is
taken out of the recoveries after the reinstatement capacity has capped
them. See
[`simulate_function`](https://netsimanalytics.github.io/NetSimR/reference/simulate_function.md)
for the details.

## See also

[`simulate_function`](https://netsimanalytics.github.io/NetSimR/reference/simulate_function.md),
which this calls, and
[`run_shiny_simulator`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_simulator.md)
for the same model in an app.

## Examples

``` r
# 10,000 years of Poisson claim counts with Log-Normal claim sizes
claims <- simulate_claims(
  10000, frequency = "Poisson", frequency_params = 3,
  severity = "LogNormal", severity_params = c(meanlog = 8, sdlog = 1.5), seed = 1
)
summary(claims$total_claims)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>       0    5029   14016   27029   32455 1387371 

# a Pareto tail above 100,000, and a layer of 50,000 excess of 20,000 on each
# claim with two reinstatements
ceded <- simulate_claims(
  10000, "Poisson", 3, "LogNormal", c(8, 1.5), seed = 1,
  pareto_thresholds = 100000, pareto_alphas = 1.5,
  eel_layer = "limited", eel_deductible = 20000, eel_limit = 50000,
  eel_reinstatements = 2, eel_premium = 10000
)
mean(ceded$total_claims)
#> [1] 6233.926
# the expected reinstatement premium, at 100% of the upfront premium of 10,000
mean(ceded$reinstatement_premium)
#> [1] 1246.349
```
