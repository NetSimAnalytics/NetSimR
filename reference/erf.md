# Error function

Evaluates the Gauss error function, `2 * pnorm(sqrt(2) * x) - 1`, which
appears in the closed form of the LogNormal capped mean.

## Usage

``` r
erf(x)
```

## Arguments

- x:

  A real number.

## Value

The value of the error function at `x`. A non-numeric `x` is an error.

## See also

[`LNormCappedMean`](https://netsimanalytics.github.io/NetSimR/reference/LNormCappedMean.md)

## Examples

``` r
erf(0.1)
#> [1] 0.1124629
erf(0.5)
#> [1] 0.5204999
```
