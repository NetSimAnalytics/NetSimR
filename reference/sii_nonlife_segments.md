# The Solvency II standard formula's non-life segments

The twelve segments of the Solvency II standard formula for non-life
premium and reserve risk, with the standard deviations of their premium
(gross) and reserve risk, as Annex II of Commission Delegated Regulation
(EU) 2015/35 sets them after its amendment by Delegated Regulation (EU)
2019/981; and the correlations between the segments, Annex IV.

## Usage

``` r
sii_nonlife_segments

sii_nonlife_correlation
```

## Format

`sii_nonlife_segments`: a data frame with 12 rows and 4 columns: the
segment's number, its name, and the standard deviations of premium risk
and of reserve risk, as shares. `sii_nonlife_correlation`: the 12 by 12
matrix of correlations between the segments.

## Source

Commission Delegated Regulation (EU) 2015/35, Annexes II and IV, as
amended by Commission Delegated Regulation (EU) 2019/981.

## See also

[`sii_premium_reserve_risk`](https://netsimanalytics.github.io/NetSimR/reference/sii_premium_reserve_risk.md).
