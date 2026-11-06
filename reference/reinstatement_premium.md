# Reinstatement premiums of an excess of loss layer

The premium paid to reinstate the cover of an each-and-every-loss layer
after claims have used it, for each simulated period: pro rata to the
amount reinstated, as is usual in the market, and not to the time left
in the period.

## Usage

``` r
reinstatement_premium(reinstatements_used, premium, rates = 1)
```

## Arguments

- reinstatements_used:

  The number of reinstatements used in each period, non-negative and
  counted pro rata to the amount recovered; NA gives NA.

- premium:

  The upfront (deposit) premium of the layer, a non-negative number.

- rates:

  The premium rate of each reinstatement as a share of the upfront
  premium: one rate for every reinstatement, or one per reinstatement,
  the first reinstatement first. Defaults to 1 (100%).

## Value

A numeric vector with the reinstatement premium of each period.

## Details

Reinstatement \\k\\ restores the layer's limit once the recoveries pass
\\k\\ limits. With \\u\\ reinstatements used (the period's recoveries
divided by the limit, capped at the number of reinstatements, as
`number_of_reinstatements_used` of
[`simulate_function`](https://netsimanalytics.github.io/NetSimR/reference/simulate_function.md)
counts them), the share of reinstatement \\k\\ used is \\\min(\max(u -
(k - 1), 0), 1)\\, and its premium is that share times its rate times
the upfront premium. A rate of 1 charges the full upfront premium for a
full reinstatement: two reinstatements at 100% are `rates = 1`, and one
at 100% and one at 50% are `rates = c(1, 0.5)`.

## See also

[`simulate_claims`](https://netsimanalytics.github.io/NetSimR/reference/simulate_claims.md)
and
[`simulate_function`](https://netsimanalytics.github.io/NetSimR/reference/simulate_function.md),
which add these premiums to their results when given an upfront premium.

## Examples

``` r
# an upfront premium of 100,000 and reinstatements at 100%
reinstatement_premium(c(0, 0.25, 1, 1.5, 2), premium = 1e5)
#> [1]      0  25000 100000 150000 200000

# one reinstatement at 100% and one at 50%
reinstatement_premium(c(0.5, 1, 1.5, 2), premium = 1e5, rates = c(1, 0.5))
#> [1]  50000 100000 125000 150000
```
