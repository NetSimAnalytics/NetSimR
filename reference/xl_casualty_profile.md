# Risk profile of a casualty portfolio

The premium written at each policy limit by a simulated liability
portfolio: the example of
[`casualty_exposure_rating`](https://netsimanalytics.github.io/NetSimR/reference/property_exposure_rating.md).

## Usage

``` r
xl_casualty_profile
```

## Format

A data frame with 6 rows and 2 columns:

- policy_limit:

  The policy limit.

- premium:

  The premium of the policies with that limit.

## Source

Simulated.

## See also

[`casualty_exposure_rating`](https://netsimanalytics.github.io/NetSimR/reference/property_exposure_rating.md).

## Examples

``` r
xl_casualty_profile
#>   policy_limit  premium
#> 1        5e+05  8400000
#> 2        1e+06 14600000
#> 3        2e+06 12100000
#> 4        3e+06  6800000
#> 5        5e+06  4900000
#> 6        1e+07  2200000
```
