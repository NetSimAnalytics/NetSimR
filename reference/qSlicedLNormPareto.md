# The inverse cumulative distribution function of a Sliced LogNormal Pareto severity distribution

Gives the claim amount that a claim from a sliced severity distribution,
with LogNormal attritional claims below the slice point and a Pareto
tail above it, stays at or below with probability `q`; the inverse of
[`pSlicedLNormPareto`](https://netsimanalytics.github.io/NetSimR/reference/pSlicedLNormPareto.md).

## Usage

``` r
qSlicedLNormPareto(q, mu, sigma, SlicePoint, shape)

qsliced_lnorm_pareto(q, mu, sigma, SlicePoint, shape)
```

## Arguments

- q:

  A real number between 0 and 1 - the probability where the inverse
  cumulative distribution function will be evaluated. Values outside
  \[0, 1\] give `NaN` with a warning, as in
  [`qlnorm()`](https://rdrr.io/r/stats/Lognormal.html).

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

The value of the inverse cumulative distribution function at `q` with an
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
[`pSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/pSlicedLNormPareto.md),
[`qSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/qSlicedGammaPareto.md)

## Examples

``` r
qSlicedLNormPareto(0.5,6,1.5,1000,1.2)
#> [1] 403.4288
qSlicedLNormPareto(0.7,7,1.6,3000,1.4)
#> [1] 2537.769
```
