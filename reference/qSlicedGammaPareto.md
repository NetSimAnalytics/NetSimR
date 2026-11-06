# The inverse cumulative distribution function of a Sliced Gamma Pareto severity distribution

Gives the claim amount that a claim from a sliced severity distribution,
with Gamma attritional claims below the slice point and a Pareto tail
above it, stays at or below with probability `q`; the inverse of
[`pSlicedGammaPareto`](https://netsimanalytics.github.io/NetSimR/reference/pSlicedGammaPareto.md).

## Usage

``` r
qSlicedGammaPareto(q, GShape, GRate, SlicePoint, PShape)

qsliced_gamma_pareto(q, GShape, GRate, SlicePoint, PShape)
```

## Arguments

- q:

  A real number between 0 and 1 - the probability where the inverse
  cumulative distribution function will be evaluated. Values outside
  \[0, 1\] give `NaN` with a warning, as in
  [`qgamma()`](https://rdrr.io/r/stats/GammaDist.html).

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

The value of the inverse cumulative distribution function at `q` with an
attritional claim Gamma distribution with parameters `GShape` and
`GRate` and a large claim Pareto distribution with parameters
`SlicePoint` and `PShape`. A non-numeric argument or a non-positive
parameter is an error; `NA` values give `NA`.

## Details

`PShape` is the Pareto shape parameter, usually written alpha; the
sliced LogNormal-Pareto functions call the same parameter `shape`.

## See also

Other sliced distribution functions:
[`SlicedGammaParetoMean()`](https://netsimanalytics.github.io/NetSimR/reference/SlicedGammaParetoMean.md),
[`SlicedLNormParetoMean()`](https://netsimanalytics.github.io/NetSimR/reference/SlicedLNormParetoMean.md),
[`dSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/dSlicedGammaPareto.md),
[`dSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/dSlicedLNormPareto.md),
[`pSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/pSlicedGammaPareto.md),
[`pSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/pSlicedLNormPareto.md),
[`qSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/qSlicedLNormPareto.md)

## Examples

``` r
qSlicedGammaPareto(0.5,1,0.0005,1000,1.2)
#> [1] 1174.633
qSlicedGammaPareto(0.2,1.1,0.0006,2000,1.6)
#> [1] 456.946
qSlicedGammaPareto(0.8,1.2,0.0004,3000,1.4)
#> [1] 4736.752
```
