# Increased Limit Factor Curve from a LogNormal severity distribution

Gives the ratio of the LogNormal capped mean at `xHigh` to that at
`xLow`, the factor that takes the expected cost of a policy limit of
`xLow` to that of a limit of `xHigh`.

## Usage

``` r
ILFLNorm(xLow, xHigh, mu, sigma)

ilf_lnorm(xLow, xHigh, mu, sigma)
```

## Arguments

- xLow:

  A non-negative real number - the claim amount where the Increased
  Limit Factor Curve will be evaluated from.

- xHigh:

  A non-negative real number - the claim amount where the Increased
  Limit Factor Curve will be evaluated to.

- mu:

  A real number - the first parameter of the Claim Severity's LogNormal
  distribution.

- sigma:

  A positive real number - the second parameter of the Claim Severity's
  LogNormal distribution.

## Value

The value of the Increased Limit Factor curve from `xLow` to `xHigh`
with Claim Severity from a LogNormal distribution with parameters `mu`
and `sigma`.

## See also

Other ILF functions:
[`ILFGamma()`](https://netsimanalytics.github.io/NetSimR/reference/ILFGamma.md),
[`ILFPareto()`](https://netsimanalytics.github.io/NetSimR/reference/ILFPareto.md),
[`ILFSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/ILFSlicedGammaPareto.md),
[`ILFSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/ILFSlicedLNormPareto.md)

## Examples

``` r
ILFLNorm(1000,2000,6,1.5)
#> [1] 1.39007
ILFLNorm(1000,1500,5,1.6)
#> [1] 1.155086
```
