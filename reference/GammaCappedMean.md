# Gamma capped mean

Gives the expected claim amount when each claim from a Gamma severity
distribution is capped at `cap`, as needed to price a policy limit or a
reinsurance layer.

## Usage

``` r
GammaCappedMean(cap, shape, rate)

gamma_capped_mean(cap, shape, rate)
```

## Arguments

- cap:

  A non-negative real number - the claim severity cap.

- shape:

  A positive real number - the shape parameter of the Claim Severity's
  Gamma distribution.

- rate:

  A positive real number - the rate parameter of the Claim Severity's
  Gamma distribution.

## Value

The mean of the claim severity capped at `cap` with a Gamma distribution
with parameters `shape` and `rate`. The arguments are recycled to a
common length. A non-numeric argument, a negative `cap` or a
non-positive `shape` or `rate` is an error; `NA` values give `NA`.

## See also

Other capped mean functions:
[`LNormCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/LNormCappedMean.md),
[`ParetoCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/ParetoCappedMean.md),
[`ParetoCappedMeanCalc()`](https://netsimanalytics.github.io/NetSimR/reference/ParetoCappedMeanCalc.md),
[`SlicedGammaParetoCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/SlicedGammaParetoCappedMean.md),
[`SlicedLNormParetoCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/SlicedLNormParetoCappedMean.md)

## Examples

``` r
GammaCappedMean(700,1,0.0005)
#> [1] 590.6238
GammaCappedMean(1000,1.5,0.0006)
#> [1] 890.8109
```
