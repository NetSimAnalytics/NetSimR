# A vector with the frequency distribution objects

A named list of `distributionClass` objects, one for each claim count
distribution of the simulator: `Poisson`, `Negative_Binomial`,
`Binomial` and `Fixed_number_of_Counts`. The names are the values
accepted by the `freqDistr` argument of
[`simulate_function()`](https://netsimanalytics.github.io/NetSimR/reference/simulate_function.md),
and the objects hold the parameter ids, labels and ranges of the app's
inputs and the functions that draw the claim counts.

## Usage

``` r
freq_dist_options
```

## Value

The frequency distribution objects.
