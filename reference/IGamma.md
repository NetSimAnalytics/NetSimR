# Upper incomplete gamma function

Evaluates the upper incomplete gamma function, the integral of
`t^(a - 1) * exp(-t)` from `x` to infinity, which appears in the closed
form of the Gamma capped mean.

## Usage

``` r
IGamma(a, x)

upper_incomplete_gamma(a, x)
```

## Arguments

- a:

  A positive real number - the shape parameter.

- x:

  A non-negative real number.

## Value

The value of the upper incomplete gamma function at `x` with shape
parameter `a`, i.e. `gamma(a) * pgamma(x, a, lower.tail = FALSE)`. The
arguments are recycled to a common length. A non-numeric or non-positive
`a` or a negative `x` is an error.

## See also

[`GammaCappedMean`](https://netsimanalytics.github.io/NetSimR/reference/GammaCappedMean.md)

## Examples

``` r
IGamma(1,1)
#> [1] 0.3678794
IGamma(0.1,2)
#> [1] 0.05397797
```
