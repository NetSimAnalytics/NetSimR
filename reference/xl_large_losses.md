# Simulated large losses of an excess of loss treaty

The large losses of a simulated portfolio for the years 2015 to 2024,
those of at least 500,000 in the money of their year: the experience of
the examples of
[`burning_cost`](https://netsimanalytics.github.io/NetSimR/reference/burning_cost.md)
and
[`pareto_rating`](https://netsimanalytics.github.io/NetSimR/reference/pareto_rating.md),
with the premiums of
[`xl_premium`](https://netsimanalytics.github.io/NetSimR/reference/xl_premium.md).
In the money of 2025, at 4% claims inflation a year, they are Pareto
with alpha 1.7 above 500,000, about 0.16 per million of on-level
premium, with fewer in the recent years, whose losses are not all
reported yet. The script that made it is in the package's source, under
data-raw.

## Usage

``` r
xl_large_losses
```

## Format

A data frame with a row per loss and 3 columns:

- claim:

  The claim's reference.

- year:

  The year of the loss.

- amount:

  The loss, in the money of its year.

## Source

Simulated.

## See also

[`xl_premium`](https://netsimanalytics.github.io/NetSimR/reference/xl_premium.md),
[`burning_cost`](https://netsimanalytics.github.io/NetSimR/reference/burning_cost.md)
and
[`pareto_rating`](https://netsimanalytics.github.io/NetSimR/reference/pareto_rating.md).

## Examples

``` r
table(xl_large_losses$year)
#> 
#> 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 
#>    5    3    1    4    6    2    9    6    5    3 
```
