# Rate reviews: on-level premiums, the indicated rate change and credibility

A rate review asks whether the current rates will pay for next year’s
claims. This vignette works through the loss ratio method with NetSimR:
the past premiums brought to today’s rates, the past losses brought to
next year’s cost, and the indicated change, with credibility for the
segments that have little data of their own.

``` r

library("NetSimR")
```

## Premiums at today’s rates

The premium a year earned was charged at the rates of its policies.
Rates rose 5% in July 2021, fell 2% in January 2023 and rose 8% in April
2024; with annual policies written evenly through the year, the premium
of each calendar year mixes the rates of two years of writing.
[`on_level_premium()`](https://netsimanalytics.github.io/NetSimR/reference/on_level_premium.md)
works out each year’s average rate level, the parallelogram method, and
the factor that brings its premium to today’s rates:

``` r

changes <- data.frame(date = as.Date(c("2021-07-01", "2023-01-01", "2024-04-01")),
                      change = c(0.05, -0.02, 0.08))
onl <- on_level_premium(premium = c(9.2e6, 9.8e6, 10.4e6, 11.1e6), years = 2021:2024, changes)
onl
```

    ##   year  premium average_level on_level_factor on_level_premium
    ## 1 2021  9200000      1.006353        1.104304         10159598
    ## 2 2022  9800000      1.043852        1.064633         10433407
    ## 3 2023 10400000      1.039500        1.069091         11118545
    ## 4 2024 11100000      1.052237        1.056150         11723265

## The indicated rate change

The ultimate losses of each year, from a reserving method such as the
chain ladder, are trended at 4% a year to the claims of the policies the
new rates will price: rates taking effect in January 2026 for a year, on
annual policies, whose average claim date is the start of 2027. The
experience loss ratio is compared with the permissible loss ratio, which
leaves 27% of the premium for expenses and 5% for profit:

``` r

rate <- indicated_rate_change(losses = c(6.3e6, 6.6e6, 7.4e6, 7.6e6), premium = onl$on_level_premium,
                              years = 2021:2024, trend = 0.04, effective = as.Date("2026-01-01"),
                              expense_ratio = 0.27, profit = 0.05)
rate$by_year
```

    ##   year  losses trend_factor trended_losses  premium loss_ratio
    ## 1 2021 6300000     1.240747        7816708 10159598  0.7693915
    ## 2 2022 6600000     1.193026        7873974 10433407  0.7546886
    ## 3 2023 7400000     1.147141        8488841 11118545  0.7634849
    ## 4 2024 7600000     1.103020        8382951 11723265  0.7150697

``` r

c(experience = rate$experience_loss_ratio, permissible = rate$permissible_loss_ratio,
  indicated_change = rate$indicated_change)
```

    ##       experience      permissible indicated_change 
    ##        0.7496860        0.6800000        0.1024795

A small portfolio’s experience is not fully credible: with `credibility`
below 1, its loss ratio is blended with a complement, such as the loss
ratio the current rates assume
([`credibility_weight()`](https://netsimanalytics.github.io/NetSimR/reference/credibility_weight.md)
gives the limited fluctuation credibility of a number of claims).

## Credibility between segments

Schemes, fleets or regions each have their own experience, more or less
of it. Buhlmann-Straub credibility gives each one’s estimate as its own
mean shrunk towards the collective, the more so the less premium it has
and the less the segments differ:

``` r

set.seed(1)
schemes <- data.frame(scheme = rep(c("A", "B", "C", "D", "E"), each = 4),
                      premium = round(runif(20, 1e5, 1.5e6)))
true <- c(A = 0.55, B = 0.62, C = 0.66, D = 0.75, E = 0.60)[schemes$scheme]
schemes$loss_ratio <- rgamma(20, shape = schemes$premium / 1e4, rate = schemes$premium / 1e4 / true)
bs <- buhlmann_straub(schemes$loss_ratio, schemes$premium, schemes$scheme)
bs$by_group
```

    ##   group  weight periods      mean credibility  estimate
    ## 1     A 3366171       4 0.5544676   0.8470481 0.5661548
    ## 2     B 4187763       4 0.6253037   0.8732521 0.6260103
    ## 3     C 1902804       4 0.6620445   0.7578974 0.6544991
    ## 4     D 3674134       4 0.7512478   0.8580485 0.7341612
    ## 5     E 4413807       4 0.5656745   0.8789576 0.5735670

The scheme with the least premium gets the least credibility, and its
estimate moves furthest towards the collective loss ratio of 63.1%. The
results match those of the actuar package’s
[`cm()`](https://rdrr.io/r/grDevices/cm.html).
