# Pareto capped mean

Gives the expected claim amount when each claim from a Pareto severity
distribution is capped at `cap`, as needed to price a policy limit or a
reinsurance layer.

## Usage

``` r
ParetoCappedMean(cap, scale, shape)

pareto_capped_mean(cap, scale, shape)
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

The mean of the claim severity capped at `cap` with a Pareto
distribution with parameters `scale` and `shape`. A cap at or below
`scale` is returned unchanged, as no claim is smaller than `scale`. The
arguments are recycled to a common length. A non-numeric argument, a
negative `cap` or a non-positive `scale` or `shape` is an error; `NA`
values give `NA`.

## See also

Other capped mean functions:
[`GammaCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/GammaCappedMean.md),
[`LNormCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/LNormCappedMean.md),
[`ParetoCappedMeanCalc()`](https://netsimanalytics.github.io/NetSimR/reference/ParetoCappedMeanCalc.md),
[`SlicedGammaParetoCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/SlicedGammaParetoCappedMean.md),
[`SlicedLNormParetoCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/SlicedLNormParetoCappedMean.md)

## Examples

``` r
ParetoCappedMean(600,200,1.2)
#> [1] 397.2584
ParetoCappedMean(800,100,1)
#> [1] 307.9442
ParetoCappedMean(1000,500,0.8)
#> [1] 871.7459
ParetoCappedMean(50,100,2)
#> [1] 50
```
