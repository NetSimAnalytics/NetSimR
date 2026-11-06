# Lognormal capped mean

Gives the expected claim amount when each claim from a LogNormal
severity distribution is capped at `cap`, as needed to price a policy
limit or a reinsurance layer.

## Usage

``` r
LNormCappedMean(cap, mu, sigma)

lnorm_capped_mean(cap, mu, sigma)
```

## Arguments

- cap:

  A non-negative real number - the claim severity cap.

- mu:

  A real number - the first parameter of the Claim Severity's LogNormal
  distribution.

- sigma:

  A positive real number - the second parameter of the Claim Severity's
  LogNormal distribution.

## Value

The mean of the claim severity capped at `cap` with a LogNormal
distribution with parameters `mu` and `sigma`. The arguments are
recycled to a common length. A non-numeric argument, a negative `cap` or
a non-positive `sigma` is an error; `NA` values give `NA`.

## See also

Other capped mean functions:
[`GammaCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/GammaCappedMean.md),
[`ParetoCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/ParetoCappedMean.md),
[`ParetoCappedMeanCalc()`](https://netsimanalytics.github.io/NetSimR/reference/ParetoCappedMeanCalc.md),
[`SlicedGammaParetoCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/SlicedGammaParetoCappedMean.md),
[`SlicedLNormParetoCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/SlicedLNormParetoCappedMean.md)

## Examples

``` r
LNormCappedMean(2000,6,1.5)
#> [1] 699.1615
LNormCappedMean(1000,5,1.6)
#> [1] 298.9925
```
