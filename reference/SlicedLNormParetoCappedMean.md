# Sliced LogNormal Pareto capped mean

Gives the expected claim amount when each claim from a sliced severity
distribution, with LogNormal attritional claims below the slice point
and a Pareto tail above it, is capped at `cap`.

## Usage

``` r
SlicedLNormParetoCappedMean(cap, mu, sigma, SlicePoint, shape)

sliced_lnorm_pareto_capped_mean(cap, mu, sigma, SlicePoint, shape)
```

## Arguments

- cap:

  A non-negative real number - the claim severity cap.

- mu:

  A real number - the first parameter of the attritional Claim
  Severity's LogNormal distribution.

- sigma:

  A positive real number - the second parameter of the attritional Claim
  Severity's LogNormal distribution.

- SlicePoint:

  A positive real number - the slice point and the scale parameter of
  the tail Claim Severity's Pareto distribution. An infinite slice point
  gives the LogNormal distribution.

- shape:

  A positive real number - the shape parameter of the tail Claim
  Severity's Pareto distribution.

## Value

The mean of the claim severity capped at `cap` with an attritional claim
LogNormal distribution with parameters `mu` and `sigma` and a large
claim Pareto distribution with parameters `SlicePoint` and `shape`. A
non-numeric argument, a negative `cap` or a non-positive `sigma`,
`SlicePoint` or `shape` is an error; `NA` values give `NA`.

## Details

`shape` is the Pareto shape parameter, usually written alpha; the sliced
Gamma-Pareto functions call the same parameter `PShape`.

## See also

Other capped mean functions:
[`GammaCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/GammaCappedMean.md),
[`LNormCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/LNormCappedMean.md),
[`ParetoCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/ParetoCappedMean.md),
[`ParetoCappedMeanCalc()`](https://netsimanalytics.github.io/NetSimR/reference/ParetoCappedMeanCalc.md),
[`SlicedGammaParetoCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/SlicedGammaParetoCappedMean.md)

## Examples

``` r
SlicedLNormParetoCappedMean(1200,6,1.5,1000,1.2)
#> [1] 551.7621
SlicedLNormParetoCappedMean(2500,6.5,1.4,2000,1.6)
#> [1] 999.829
SlicedLNormParetoCappedMean(4000,7,1.6,3000,1.4)
#> [1] 1663.648
```
