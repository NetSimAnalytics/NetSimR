# Sliced Gamma Pareto capped mean

Gives the expected claim amount when each claim from a sliced severity
distribution, with Gamma attritional claims below the slice point and a
Pareto tail above it, is capped at `cap`.

## Usage

``` r
SlicedGammaParetoCappedMean(cap, GShape, GRate, SlicePoint, PShape)

sliced_gamma_pareto_capped_mean(cap, GShape, GRate, SlicePoint, PShape)
```

## Arguments

- cap:

  A non-negative real number - the claim severity cap.

- GShape:

  A positive real number - the shape parameter of the attritional Claim
  Severity's Gamma distribution.

- GRate:

  A positive real number - the rate parameter of the attritional Claim
  Severity's Gamma distribution.

- SlicePoint:

  A positive real number - the slice point and the scale parameter of
  the tail Claim Severity's Pareto distribution. An infinite slice point
  gives the Gamma distribution.

- PShape:

  A positive real number - the shape parameter of the tail Claim
  Severity's Pareto distribution.

## Value

The mean of the claim severity capped at `cap` with an attritional claim
Gamma distribution with parameters `GShape` and `GRate` and a large
claim Pareto distribution with parameters `SlicePoint` and `PShape`. A
non-numeric argument, a negative `cap` or a non-positive parameter is an
error; `NA` values give `NA`.

## Details

`PShape` is the Pareto shape parameter, usually written alpha; the
sliced LogNormal-Pareto functions call the same parameter `shape`.

## See also

Other capped mean functions:
[`GammaCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/GammaCappedMean.md),
[`LNormCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/LNormCappedMean.md),
[`ParetoCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/ParetoCappedMean.md),
[`ParetoCappedMeanCalc()`](https://netsimanalytics.github.io/NetSimR/reference/ParetoCappedMeanCalc.md),
[`SlicedLNormParetoCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/SlicedLNormParetoCappedMean.md)

## Examples

``` r
SlicedGammaParetoCappedMean(3000,1,0.0005,1000,1.2)
#> [1] 1385.155
SlicedGammaParetoCappedMean(1000,1.1,0.0006,2000,1.6)
#> [1] 788.2556
SlicedGammaParetoCappedMean(2000,1.2,0.0004,3000,1.4)
#> [1] 1524.157
```
