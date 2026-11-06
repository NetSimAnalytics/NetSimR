# Increased Limit Factor Curve from a Pareto severity distribution

Gives the ratio of the Pareto capped mean at `xHigh` to that at `xLow`,
the factor that takes the expected cost of a policy limit of `xLow` to
that of a limit of `xHigh`.

## Usage

``` r
ILFPareto(xLow, xHigh, scale, shape)

ilf_pareto(xLow, xHigh, scale, shape)
```

## Arguments

- xLow:

  A non-negative real number - the claim amount where the Increased
  Limit Factor Curve will be evaluated from.

- xHigh:

  A non-negative real number - the claim amount where the Increased
  Limit Factor Curve will be evaluated to.

- scale:

  A positive real number - the scale parameter of the Claim Severity's
  Pareto distribution.

- shape:

  A positive real number - the shape parameter of the Claim Severity's
  Pareto distribution.

## Value

The value of the Increased Limit Factor curve from `xLow` to `xHigh`
with Claim Severity from a Pareto distribution with parameters `scale`
and `shape`.

## See also

Other ILF functions:
[`ILFGamma()`](https://netsimanalytics.github.io/NetSimR/reference/ILFGamma.md),
[`ILFLNorm()`](https://netsimanalytics.github.io/NetSimR/reference/ILFLNorm.md),
[`ILFSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/ILFSlicedGammaPareto.md),
[`ILFSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/ILFSlicedLNormPareto.md)

## Examples

``` r
ILFPareto(700,1200,500,1.2)
#> [1] 1.360425
ILFPareto(1200,20000,200,1.1)
#> [1] 1.7764
```
