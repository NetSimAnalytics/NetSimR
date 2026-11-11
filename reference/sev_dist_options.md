# A vector with the severity distribution objects

A named list of `distributionClass` objects, one for each severity
distribution of the simulator: Normal, LogNormal, Gamma, Exponential,
Pareto, Weibull, Burr, Generalised_Pareto and Fixed_Severity. The names
are the values accepted by the `sevDistr` argument of
[`simulate_function()`](https://netsimanalytics.github.io/NetSimR/reference/simulate_function.md).
The Normal distribution has its own parameter ids (normal_mean,
normal_sd), so that switching between the Normal and the Log-Normal in
the app does not carry values across, and so have the Weibull, the Burr
and the Generalised Pareto.

## Usage

``` r
sev_dist_options
```

## Value

The severity distribution objects.

## Details

The Burr (type XII) has P(X \> x) = (1 + (x / scale)^gamma)^(-alpha) for
x \> 0: a Pareto tail of index alpha \* gamma over a body shaped by
gamma (alpha = 1 is the log-logistic, gamma = 1 the Lomax). The
Generalised Pareto has P(X \> x) = (1 + shape \* (x - location) /
scale)^(-1 / shape) for x above its location: a Pareto tail of index 1 /
shape for a positive shape, the Exponential for shape 0 and a tail that
ends at location - scale / shape for a negative one.
