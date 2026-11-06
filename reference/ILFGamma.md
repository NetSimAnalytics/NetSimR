# Increased Limit Factor Curve from a Gamma severity distribution

Gives the ratio of the Gamma capped mean at `xHigh` to that at `xLow`,
the factor that takes the expected cost of a policy limit of `xLow` to
that of a limit of `xHigh`.

## Usage

``` r
ILFGamma(xLow, xHigh, shape, rate)

ilf_gamma(xLow, xHigh, shape, rate)
```

## Arguments

- xLow:

  A non-negative real number - the claim amount where the Increased
  Limit Factor Curve will be evaluated from.

- xHigh:

  A non-negative real number - the claim amount where the Increased
  Limit Factor Curve will be evaluated to.

- shape:

  A positive real number - the shape parameter of the Claim Severity's
  Gamma distribution.

- rate:

  A positive real number - the rate parameter of the Claim Severity's
  Gamma distribution.

## Value

The value of the Increased Limit Factor curve from `xLow` to `xHigh`
with Claim Severity from a Gamma distribution with parameters `shape`
and `rate`.

## See also

Other ILF functions:
[`ILFLNorm()`](https://netsimanalytics.github.io/NetSimR/reference/ILFLNorm.md),
[`ILFPareto()`](https://netsimanalytics.github.io/NetSimR/reference/ILFPareto.md),
[`ILFSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/ILFSlicedGammaPareto.md),
[`ILFSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/ILFSlicedLNormPareto.md)

## Examples

``` r
ILFGamma(700,1000,1,0.0005)
#> [1] 1.332386
ILFGamma(1000,1200,1.5,0.0006)
#> [1] 1.162651
```
