# The indicated rate change: the loss ratio method

Projects each year's ultimate losses to the period the new rates will
apply to, with a claims trend, divides them by the year's on-level
premium, and compares the projected loss ratio with the permissible one,
which leaves room for the expenses and the profit.

## Usage

``` r
indicated_rate_change(
  losses,
  premium,
  years,
  trend = 0,
  effective,
  rate_period = 1,
  term = 1,
  expense_ratio = 0,
  profit = 0,
  weights = NULL,
  credibility = 1,
  complement = NULL
)
```

## Arguments

- losses:

  The ultimate losses of each year, such as from
  [`chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md).

- premium:

  The on-level earned premium of each year
  ([`on_level_premium`](https://netsimanalytics.github.io/NetSimR/reference/on_level_premium.md)).

- years:

  The years.

- trend:

  The annual claims trend (inflation of the loss cost), as a share.

- effective:

  The date the new rates take effect: a Date or a decimal year.

- rate_period:

  How long the new rates will be in force, in years.

- term:

  The length of the policies in years.

- expense_ratio:

  The expenses (commissions and other costs varying with the premium) as
  a share of the premium.

- profit:

  The profit and contingencies provision, as a share of the premium.

- weights:

  Weights of the years, such as to give the latest more weight; by
  default their on-level premium.

- credibility:

  The credibility of the experience, from 0 to 1.

- complement:

  The loss ratio blended with the experience when its credibility is
  below 1.

## Value

A list with `by_year` (each year's losses, trend factor, trended losses,
premium and projected loss ratio), the experience loss ratio, the
blended loss ratio, the permissible loss ratio and the indicated rate
change.

## Details

Each year's losses are trended from the middle of the year, \\y + 0.5\\
for calendar or accident year \\y\\, to the average date of the claims
of the policies the new rates will price: the date they take effect,
plus half the period they will be in force, plus half a policy term. The
experience loss ratio weighs the years by their on-level premium (or
`weights`); with `credibility` below 1 it is blended with the
`complement`, such as the loss ratio the current rates assume. The
indicated change is that loss ratio over the permissible loss ratio,
\\1 - \textrm{expense ratio} - \textrm{profit}\\, less 1.

## See also

[`on_level_premium`](https://netsimanalytics.github.io/NetSimR/reference/on_level_premium.md),
[`buhlmann_straub`](https://netsimanalytics.github.io/NetSimR/reference/buhlmann_straub.md)
and
[`credibility_weight`](https://netsimanalytics.github.io/NetSimR/reference/credibility_weight.md).

## Examples

``` r
changes <- data.frame(date = as.Date(c("2021-07-01", "2023-01-01", "2024-04-01")),
                      change = c(0.05, -0.02, 0.08))
onl <- on_level_premium(c(9.2e6, 9.8e6, 10.4e6, 11.1e6), 2021:2024, changes)
rate <- indicated_rate_change(c(6.3e6, 6.6e6, 7.4e6, 7.6e6), onl$on_level_premium, 2021:2024,
                              trend = 0.04, effective = as.Date("2026-01-01"),
                              expense_ratio = 0.27, profit = 0.05)
rate[c("experience_loss_ratio", "indicated_change")]
#> $experience_loss_ratio
#> [1] 0.749686
#> 
#> $indicated_change
#> [1] 0.1024795
#> 
```
