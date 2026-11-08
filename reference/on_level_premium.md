# Premiums at today's rates: the parallelogram method

Brings each calendar year's earned premium to the current rate level,
allowing for the rate changes made since: the on-level factor of a year
is the current rate level over the average rate level of the premium the
year earned.

## Usage

``` r
on_level_premium(premium, years, changes, term = 1)
```

## Arguments

- premium:

  The earned premium of each calendar year.

- years:

  The calendar years, such as 2020:2024.

- changes:

  A data frame of the rate changes: their `date` (a Date, or a decimal
  year such as 2023.5) and the `change` (0.05 for +5%), applying to the
  policies written from that date.

- term:

  The length of the policies in years: 1 (the default) for annual
  policies.

## Value

A data frame with each year's earned premium, its average rate level
(the rate level of the policies written before the first change being
1), the on-level factor and the premium at the current rate level; the
current rate level is in its attribute `current_level`.

## Details

Policies are taken to be written evenly over time, each for `term`
years, and to earn their premium evenly over their term. The premium a
calendar year earns comes from the policies written in the `term` years
before its end; its average rate level weighs the rate level at each
writing date by the part of that policy's term within the year. This is
the parallelogram method, worked out exactly for any dates of change and
any term (numerically, on a fine grid).

## See also

[`indicated_rate_change`](https://netsimanalytics.github.io/NetSimR/reference/indicated_rate_change.md)
and
[`buhlmann_straub`](https://netsimanalytics.github.io/NetSimR/reference/buhlmann_straub.md).

## Examples

``` r
changes <- data.frame(date = as.Date(c("2021-07-01", "2023-01-01", "2024-04-01")),
                      change = c(0.05, -0.02, 0.08))
on_level_premium(c(9.2e6, 9.8e6, 10.4e6, 11.1e6), 2021:2024, changes)
#>   year  premium average_level on_level_factor on_level_premium
#> 1 2021  9200000      1.006353        1.104304         10159598
#> 2 2022  9800000      1.043852        1.064633         10433407
#> 3 2023 10400000      1.039500        1.069091         11118545
#> 4 2024 11100000      1.052237        1.056150         11723265
```
