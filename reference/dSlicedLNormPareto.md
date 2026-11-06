# The probability density function (pdf) of a Sliced LogNormal Pareto severity distribution

Gives the probability density at the claim amount `x` of a sliced
severity distribution, with LogNormal attritional claims below the slice
point and a Pareto tail above it.

## Usage

``` r
dSlicedLNormPareto(x, mu, sigma, SlicePoint, shape)

dsliced_lnorm_pareto(x, mu, sigma, SlicePoint, shape)
```

## Arguments

- x:

  A real number - the claim amount where the probability density
  function (pdf) will be evaluated. The pdf is 0 for negative `x`.

- mu:

  A real number - the first parameter of the attritional Claim
  Severity's LogNormal distribution.

- sigma:

  A positive real number - the second parameter of the attritional Claim
  Severity's LogNormal distribution.

- SlicePoint:

  A positive real number - the slice point and the scale parameter of
  the Claim Severity's Pareto distribution. An infinite slice point
  gives the LogNormal distribution.

- shape:

  A positive real number - the shape parameter of the Claim Severity's
  Pareto distribution.

## Value

The value of the probability density function (pdf) at `x` with an
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
[`pSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/pSlicedGammaPareto.md),
[`pSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/pSlicedLNormPareto.md),
[`qSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/qSlicedGammaPareto.md),
[`qSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/qSlicedLNormPareto.md)

## Examples

``` r
dSlicedLNormPareto(1200,6,1.5,1000,1.2)
#> [1] 0.0002189785
dSlicedLNormPareto(4000,7,1.6,3000,1.4)
#> [1] 6.192654e-05
```
