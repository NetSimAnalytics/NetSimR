# Buhlmann-Straub credibility

The credibility estimates of the risk of several groups (schemes,
fleets, regions, ...) from their experience over several periods, each
weighted by its volume: each group's own mean, shrunk towards the
collective by its credibility.

## Usage

``` r
buhlmann_straub(value, weight, group)
```

## Arguments

- value:

  The value of each group and period.

- weight:

  The weight of each value.

- group:

  The group of each value.

## Value

A list with `by_group` (each group's weight, number of periods, mean,
credibility and estimate), the collective mean, the within and between
variances (`s2`, `a`) and `k` \\= s^2 / a\\.

## Details

With \\X\_{ij}\\ the value (such as a loss ratio or a claim frequency)
of group \\i\\ in period \\j\\ and \\w\_{ij}\\ its weight (premium,
exposure), \\w_i = \sum_j w\_{ij}\\, \\\bar X_i\\ the weighted mean of
the group and \\\bar X\\ of all, the within-group variance is estimated
by \$\$s^2 = \frac{\sum_i \sum_j w\_{ij} (X\_{ij} - \bar X_i)^2}{\sum_i
(n_i - 1)}\$\$ and the variance between the groups by \$\$a =
\frac{\sum_i w_i (\bar X_i - \bar X)^2 - (I - 1) s^2}{w - \sum_i w_i^2 /
w},\$\$ with \\w = \sum_i w_i\\. A group's credibility is \\Z_i = w_i /
(w_i + s^2 / a)\\ (0 for all when \\a \le 0\\: the groups do not differ
more than chance), the collective mean \\\hat\mu = \sum_i Z_i \bar X_i /
\sum_i Z_i\\ (the weighted mean when every Z is 0), and its estimate
\\\hat\mu + Z_i (\bar X_i - \hat\mu)\\.

## References

Buhlmann, H. and Straub, E. (1970). Glaubwurdigkeit fur Schadensatze.
Mitteilungen der Vereinigung Schweizerischer Versicherungsmathematiker
70, 111-133.

## See also

[`credibility_weight`](https://netsimanalytics.github.io/NetSimR/reference/credibility_weight.md),
limited fluctuation credibility.

## Examples

``` r
set.seed(1)
schemes <- data.frame(scheme = rep(c("A", "B", "C", "D"), each = 5),
                      premium = round(runif(20, 1e5, 1e6)))
true <- c(A = 0.55, B = 0.65, C = 0.70, D = 0.80)[schemes$scheme]
schemes$loss_ratio <- rgamma(20, shape = schemes$premium / 2e4, rate = schemes$premium / 2e4 / true)
buhlmann_straub(schemes$loss_ratio, schemes$premium, schemes$scheme)$by_group
#>   group  weight periods      mean credibility  estimate
#> 1     A 2588339       5 0.5523689   0.5428446 0.6017959
#> 2     B 3375288       5 0.6839754   0.6076069 0.6747589
#> 3     C 2501149       5 0.6774360   0.5343293 0.6695435
#> 4     D 3528234       5 0.7176997   0.6181211 0.6958515
```
