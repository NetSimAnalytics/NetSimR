# A vector with the severity distribution objects

A named list of `distributionClass` objects, one for each severity
distribution of the simulator: Normal, LogNormal, Gamma, Exponential,
Pareto and Fixed_Severity. The names are the values accepted by the
`sevDistr` argument of
[`simulate_function()`](https://netsimanalytics.github.io/NetSimR/reference/simulate_function.md).
The Normal distribution has its own parameter ids (normal_mean,
normal_sd), so that switching between the Normal and the Log-Normal in
the app does not carry values across.

## Usage

``` r
sev_dist_options
```

## Value

The severity distribution objects.
