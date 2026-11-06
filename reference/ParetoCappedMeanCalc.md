# Pareto capped mean intermediary calculation

Evaluates the closed-form expression
`(shape * scale - cap * (scale / cap)^shape) / (shape - 1)` that the
Pareto capped mean is built on, for checking or reusing the intermediate
step.

## Usage

``` r
ParetoCappedMeanCalc(cap, scale, shape)

pareto_capped_mean_calc(cap, scale, shape)
```

## Arguments

- cap:

  A non-negative real number - the claim severity cap.

- scale:

  A positive real number - the scale parameter of the Claim Severity's
  Pareto distribution.

- shape:

  A positive real number - the shape parameter of the Claim Severity's
  Pareto distribution.

## Value

An interim calculation for the mean of the claim severity capped at
`cap` with a Pareto distribution with parameters `scale` and `shape`. It
is the closed form for `cap >= scale` and `shape != 1`; use
[`ParetoCappedMean`](https://netsimanalytics.github.io/NetSimR/reference/ParetoCappedMean.md)
for the capped mean itself. The arguments are recycled to a common
length.

## See also

Other capped mean functions:
[`GammaCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/GammaCappedMean.md),
[`LNormCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/LNormCappedMean.md),
[`ParetoCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/ParetoCappedMean.md),
[`SlicedGammaParetoCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/SlicedGammaParetoCappedMean.md),
[`SlicedLNormParetoCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/SlicedLNormParetoCappedMean.md)

## Examples

``` r
ParetoCappedMeanCalc(800,100,1.1)
#> [1] 287.7476
ParetoCappedMeanCalc(1000,500,0.9)
#> [1] 858.8673
```
