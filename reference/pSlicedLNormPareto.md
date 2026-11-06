# The cumulative distribution function (cdf) of a Sliced LogNormal Pareto severity distribution

Gives the probability that a claim from a sliced severity distribution,
with LogNormal attritional claims below the slice point and a Pareto
tail above it, is at most `x`.

## Usage

``` r
pSlicedLNormPareto(x, mu, sigma, SlicePoint, shape)

psliced_lnorm_pareto(x, mu, sigma, SlicePoint, shape)
```

## Arguments

- x:

  A real number - the claim amount where the cumulative distribution
  function (cdf) will be evaluated. The cdf is 0 for negative `x`.

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

The value of the cumulative distribution function (cdf) at `x` with an
attritional claim LogNormal distribution with parameters `mu` and
`sigma` and a large claim Pareto distribution with parameters
`SlicePoint` and `shape`. A non-numeric argument or a non-positive
`sigma`, `SlicePoint` or `shape` is an error; `NA` values give `NA`.

## Details

`shape` is the Pareto shape parameter, usually written alpha; the sliced
Gamma-Pareto functions call the same parameter `PShape`.

## See also

Other sliced distribution functions:
[`SlicedGammaParetoMean()`](https://netsimanalytics.github.io/NetSimR/reference/SlicedGammaParetoMean.md),
[`SlicedLNormParetoMean()`](https://netsimanalytics.github.io/NetSimR/reference/SlicedLNormParetoMean.md),
[`dSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/dSlicedGammaPareto.md),
[`dSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/dSlicedLNormPareto.md),
[`pSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/pSlicedGammaPareto.md),
[`qSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/qSlicedGammaPareto.md),
[`qSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/qSlicedLNormPareto.md)

## Examples

``` r
pSlicedLNormPareto(1200,6,1.5,1000,1.2)
#> [1] 0.7810215
pSlicedLNormPareto(4000,7,1.6,3000,1.4)
#> [1] 0.823067
```
