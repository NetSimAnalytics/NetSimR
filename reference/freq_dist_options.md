# A vector with the frequency distribution objects

A named list of `distributionClass` objects, one for each claim count
distribution of the simulator: `Poisson`, `Negative_Binomial`,
`Binomial`, `Zero_Inflated_Poisson`, `Zero_Inflated_Negative_Binomial`
and `Fixed_number_of_Counts`. The names are the values accepted by the
`freqDistr` argument of
[`simulate_function()`](https://netsimanalytics.github.io/NetSimR/reference/simulate_function.md),
and the objects hold the parameter ids, labels and ranges of the app's
inputs and the functions that draw the claim counts. A zero-inflated
count is zero with the probability of its last parameter, and otherwise
drawn from the Poisson or Negative Binomial of its other parameters:
more periods without a claim than those distributions give on their own.

## Usage

``` r
freq_dist_options
```

## Value

The frequency distribution objects.
