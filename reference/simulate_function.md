# Simulate insurance claims with reinsurance structures

A function to simulate frequency - severity of insurance claims using
chunked vectorisation. The function applies severity cap, reinsurance
structure for each and every loss claim, reinsurance structure for
aggregate claims, and allows for piecewise Pareto slices

## Usage

``` r
simulate_function(
  numOfSimulations,
  freq_params,
  sev_params,
  seedSetBinary = !is.null(seedValue),
  seedValue = NULL,
  freqDistr,
  sevDistr,
  paretoSlice = FALSE,
  pareto_slice_times = NULL,
  slice_pareto_alphas = NULL,
  slice_pareto_x_ms = NULL,
  sevCapBinary = FALSE,
  sev_cap_amount = NULL,
  reinsuranceStructureEEL = "No Reinsurance Structure",
  reinsurance_structure_eel_deductible_amount = NULL,
  reinsurance_structure_eel_limit_amount = NULL,
  reinsuranceStructureAL = "No Reinsurance Structure",
  reinsurance_structure_al_deductible_amount = NULL,
  reinsurance_structure_al_limit_amount = NULL,
  reinsuranceStructureLimitedReinstatements = FALSE,
  reinsuranceStructureReinstatementLimit = NULL,
  reinsurance_structure_eel_premium = NULL,
  reinsurance_structure_eel_reinstatement_rates = 1,
  multiprocessing = FALSE,
  workers = NULL,
  sevTruncateAtZero = FALSE,
  chunk_size = NULL,
  gross = TRUE,
  shortcuts = TRUE,
  progress = NULL,
  largest_claims = FALSE,
  reinsurance_structure_eel_dedctible_amount = NULL,
  reinsurance_structure_al_dedctible_amount = NULL
)
```

## Arguments

- numOfSimulations:

  The number of simulations to run.

- freq_params:

  A vector of the frequency distribution parameters.

- sev_params:

  A vector of the severity distribution parameters.

- seedSetBinary:

  True if there is a fixed seed (`seedValue`), otherwise false. Defaults
  to TRUE when a `seedValue` is given and FALSE otherwise, so a
  `seedValue` on its own makes the run reproducible; an explicit FALSE
  ignores `seedValue`.

- seedValue:

  The seed value, a whole number between `-.Machine$integer.max` and
  `.Machine$integer.max`, or NULL (the default) for no fixed seed.

- freqDistr:

  The frequency distribution: `"Poisson"`, `"Negative_Binomial"`,
  `"Binomial"` or `"Fixed_number_of_Counts"`. The parameters of each are
  listed in
  [`simulate_claims`](https://netsimanalytics.github.io/NetSimR/reference/simulate_claims.md).

- sevDistr:

  The severity distribution: `"Normal"`, `"LogNormal"`, `"Gamma"`,
  `"Exponential"`, `"Pareto"` or `"Fixed_Severity"`. The parameters of
  each are listed in
  [`simulate_claims`](https://netsimanalytics.github.io/NetSimR/reference/simulate_claims.md).

- paretoSlice:

  True if there is Pareto slicing.

- pareto_slice_times:

  The number of Pareto slices.

- slice_pareto_alphas:

  A vector of Pareto slices' alpha parameters.

- slice_pareto_x_ms:

  A vector of Pareto slices' x_m parameters.

- sevCapBinary:

  True if there is a severity cap.

- sev_cap_amount:

  The severity cap amount.

- reinsuranceStructureEEL:

  The chosen reinsurance structure for each and every loss claim.

- reinsurance_structure_eel_deductible_amount:

  The deductible for each and every loss reinsurance structure.

- reinsurance_structure_eel_limit_amount:

  The limit for each and every loss reinsurance structure.

- reinsuranceStructureAL:

  The chosen reinsurance structure for aggregate claims.

- reinsurance_structure_al_deductible_amount:

  The deductible for aggregate reinsurance structure.

- reinsurance_structure_al_limit_amount:

  The limit for aggregate reinsurance structure.

- reinsuranceStructureLimitedReinstatements:

  True if there is a limit in reinstatements, otherwise false.

- reinsuranceStructureReinstatementLimit:

  The reinstatement limit.

- reinsurance_structure_eel_premium:

  The upfront premium of the EEL layer, or NULL (the default) for none.
  Given with a limited number of reinstatements, the results gain the
  column `reinstatement_premium`; ignored otherwise.

- reinsurance_structure_eel_reinstatement_rates:

  The premium rate of each reinstatement as a share of the upfront
  premium: one rate for all of them, or one per reinstatement. Defaults
  to 1 (100%). See
  [`reinstatement_premium`](https://netsimanalytics.github.io/NetSimR/reference/reinstatement_premium.md).

- multiprocessing:

  TRUE to run the chunks in parallel on worker R processes (base R's
  parallel package), FALSE (the default) to run them in this session, or
  "auto" to run them on workers only when that is expected to be at
  least a quarter faster. Results are the same either way. "auto"
  estimates both times from the expected number of claims drawn, the
  number of simulations and chunks, the cores, and whether the workers
  are already running: starting them takes about a second, so a single
  run goes to them from a few seconds of work, while a cluster given as
  `workers` is used from a fraction of a second; with `workers = NULL`
  it also chooses how many to start. A run of a single chunk runs in
  this session, unless TRUE is given with a cluster.

- workers:

  The workers of a parallel run: `NULL` (the default) for one per
  available core (`getOption("mc.cores")` if set, otherwise
  [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html),
  at most 2 under `R CMD check`), or a number of workers; these are
  started for the call and shut down when it finishes, so each call pays
  the start-up cost. To reuse workers across calls, pass a cluster made
  with
  [`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html):
  it is used as it is and left running, to be stopped with
  [`parallel::stopCluster()`](https://rdrr.io/r/parallel/makeCluster.html).
  A function with no arguments that returns such a cluster is called
  only when the run goes to the workers (as "auto" decides), which lets
  a caller start them when they are first needed. Ignored unless
  `multiprocessing` is TRUE or "auto".

- sevTruncateAtZero:

  True to draw Normal severities from the Normal distribution truncated
  at zero, so that no claim is negative. Ignored for other severity
  distributions. Defaults to FALSE.

- chunk_size:

  The number of simulations processed per vectorised batch. By default
  (NULL) it is chosen from the expected number of claims per simulation,
  so that a batch holds about a million claims (between 100 and 10,000
  simulations). Because of the floor of 100 simulations, a batch holds
  more than a million claims when the mean frequency exceeds 10,000
  claims per simulation (about 100 million at a mean of a million), and
  memory use grows with it; give a smaller `chunk_size` to keep batches
  small. Results with a fixed seed depend on the chunk size.

- gross:

  True (the default) to return the gross total claims before
  reinsurance. Set it to FALSE when only the totals after the structures
  are needed: with an each-and-every-loss layer this allows drawing only
  the claims that reach the layer, which is much faster.

- shortcuts:

  True (the default) to use exact shortcuts where the settings allow:
  when no layer, cap, Pareto slice or truncation acts on individual
  claims, each simulation's total is drawn in one step for the Normal,
  Gamma, Exponential and fixed severities; with `gross = FALSE` and a
  layer, only the claims above the deductible are drawn. The results
  follow the same distribution as without shortcuts. Set it to FALSE to
  simulate every claim.

- progress:

  An optional function called after each chunk of a sequential run with
  the fraction done and a short description, e.g. to update a progress
  bar.

- largest_claims:

  True to add the column `largest_claim`: each simulation's largest
  single claim, for occurrence exceedance (OEP) as `total_claims` gives
  aggregate exceedance (AEP). Defaults to FALSE. Every claim is then
  drawn, so the one-step shortcut for totals (see `shortcuts`) is not
  used.

- reinsurance_structure_eel_dedctible_amount,
  reinsurance_structure_al_dedctible_amount:

  Deprecated misspellings of
  `reinsurance_structure_eel_deductible_amount` and
  `reinsurance_structure_al_deductible_amount`, the names before version
  0.4.0; they still work, with a warning.

## Value

A data frame with one row per simulation, at full precision:
`claim_counts`, the claim count; `total_claims`, the total claims after
the reinsurance structures; `gross_claims`, the gross total claims
before them (after Pareto slices and the severity cap; unless
`gross = FALSE`); and, when reinstatements are limited,
`number_of_reinstatements_used`: the EEL layer's recoveries in the
period divided by the EEL limit, capped at the number of reinstatements,
so reinstatements are counted pro rata to the amount recovered. The
recoveries are taken after the aggregate deductible and limit of an
aggregate 'Unlimited Layer' or 'Limited Layer', but before an aggregate
'Exclude Layer' is taken out: with an exclusion they are the EEL
recoveries after the reinstatement capacity. For example, three claims
of 100 through a layer of 60 excess of 30 with two reinstatements and an
aggregate exclusion of 150 excess of 50 give a total of 50 but 2
reinstatements used (180 / 60, capped at 2). With an upfront premium,
`reinstatement_premium`: the premium for the reinstatements used, pro
rata to the amount reinstated (see
[`reinstatement_premium`](https://netsimanalytics.github.io/NetSimR/reference/reinstatement_premium.md)).
With `largest_claims = TRUE`, `largest_claim`: the simulation's largest
single claim after the each-and-every-loss structure (the layer's loss
on that claim; the gross claim when there is no structure), before the
reinstatement limit and the aggregate layer, which act on the period's
total; 0 for a simulation without claims. Stops with an error that names
any required setting that is missing or invalid.

## Details

Order of the calculations, for each simulation (a period, e.g. a year):
claims are drawn from the severity distribution (with its Pareto slices)
and capped at the severity cap; the each-and-every-loss (EEL) structure
applies to each claim; the results are summed over the period; then the
aggregate deductible comes off that sum, and finally the aggregate limit
and the reinstatement capacity cap what is left. In short: cap -\> EEL
layer per claim -\> annual sum -\> aggregate deductible -\> aggregate
limit and reinstatement capacity.

The reinstatement capacity applies to a 'Limited Layer' EEL structure
with limited reinstatements, which pays at most
`(reinstatements + 1) * limit` in a period. With an aggregate 'Unlimited
Layer' or 'Limited Layer', the ceded total is
`min(max(S - aggregate deductible, 0), aggregate limit, (reinstatements + 1) * limit)`,
where S is the sum of the period's EEL recoveries before any capacity
(the market convention for an annual aggregate deductible). For example,
three claims of 100 through a layer of 100 excess of 0 with no
reinstatements and an aggregate deductible of 50 cede
`min(300 - 50, 100) = 100`. Without an aggregate structure, the capacity
caps S. With an aggregate 'Exclude Layer', the capacity caps S first and
the aggregate layer is then taken out of the capped amount. An unlimited
EEL layer, or limited reinstatements switched off, has no capacity cap.
(Before version 0.2.0 the capacity was applied before the aggregate
deductible, which gave smaller ceded totals when the two were combined.)

Totals are returned at full precision; round them only for display.

Random numbers: each chunk of simulations uses its own L'Ecuyer-CMRG
random stream, derived from one seed, so a run gives the same results
whether or not it runs in parallel. With `seedSetBinary = TRUE` (the
default when a `seedValue` is given) the run is reproducible from
`seedValue` and the caller's random number stream is left unchanged;
otherwise the seed is drawn from the caller's stream, so
[`set.seed()`](https://rdrr.io/r/base/Random.html) before the call also
makes it reproducible. The streams always use Inversion for normal draws
and Rejection sampling, so a seed gives the same results whatever the
caller's [`RNGkind()`](https://rdrr.io/r/base/Random.html), which is
restored afterwards. Results depend on the chunk size, which by default
adapts to the expected number of claims per simulation.

## See also

[`simulate_claims`](https://netsimanalytics.github.io/NetSimR/reference/simulate_claims.md),
a simpler interface with short argument names, and
[`run_shiny_simulator`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_simulator.md)
for the same model in an app.

## Examples

``` r
# 1,000 simulated years of Poisson claim counts with Normal claim sizes, no reinsurance
results <- simulate_function(
  numOfSimulations = 1000, freq_params = 3, sev_params = c(1000, 200),
  seedSetBinary = TRUE, seedValue = 1, freqDistr = "Poisson", sevDistr = "Normal"
)
summary(results$total_claims)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>       0    1640    2802    2949    4085    9109 

# the same claims ceded to a layer of 1,500 excess of 800 on each claim
layer <- simulate_function(
  numOfSimulations = 1000, freq_params = 3, sev_params = c(1000, 200),
  seedSetBinary = TRUE, seedValue = 1, freqDistr = "Poisson", sevDistr = "Normal",
  reinsuranceStructureEEL = "Limited Layer",
  reinsurance_structure_eel_deductible_amount = 800,
  reinsurance_structure_eel_limit_amount = 1500
)
mean(layer$total_claims)
#> [1] 639.0671
```
