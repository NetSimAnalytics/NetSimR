# Subject premiums of an excess of loss treaty

The subject premium of each year 2015 to 2024 of the portfolio of
[`xl_large_losses`](https://netsimanalytics.github.io/NetSimR/reference/xl_large_losses.md),
with the rate index that brings it to the rates of 2025 and the
development factor of its layer losses.

## Usage

``` r
xl_premium
```

## Format

A data frame with 10 rows and 4 columns:

- year:

  The year.

- premium:

  The subject premium of the year.

- rate_index:

  The factor bringing the year's premium to the rates of 2025.

- development:

  The development factor of the year's layer losses, for the losses not
  reported yet and the development of those reported.

## Source

Simulated.

## See also

[`burning_cost`](https://netsimanalytics.github.io/NetSimR/reference/burning_cost.md).

## Examples

``` r
xl_premium
#>    year  premium rate_index development
#> 1  2015 41200000       0.94        1.00
#> 2  2016 42500000       0.96        1.00
#> 3  2017 43100000       0.99        1.00
#> 4  2018 44800000       1.02        1.01
#> 5  2019 46000000       1.05        1.02
#> 6  2020 47900000       1.06        1.04
#> 7  2021 50600000       1.04        1.08
#> 8  2022 53200000       1.03        1.15
#> 9  2023 55800000       1.01        1.30
#> 10 2024 58400000       1.00        1.65
```
