# Solvency II standard formula: non-life premium and reserve risk

The capital requirement for non-life premium and reserve risk of the
Solvency II standard formula (Articles 115 to 117 of Commission
Delegated Regulation (EU) 2015/35), from the volume measures of each
segment.

## Usage

``` r
sii_premium_reserve_risk(
  volumes,
  np_adjustment = NULL,
  premium_sd = NULL,
  reserve_sd = NULL
)
```

## Arguments

- volumes:

  A data frame with a row per segment the undertaking writes: its
  `segment` (the number, 1 to 12, of
  [`sii_nonlife_segments`](https://netsimanalytics.github.io/NetSimR/reference/sii_nonlife_segments.md)),
  its `premium` and `reserve` volume measures, and optionally its
  geographical `diversification` (1 when not given).

- np_adjustment:

  The segments whose premium standard deviation takes the 80% adjustment
  for non-proportional reinsurance: `NULL` (the default) for none, or
  some of 1, 4 and 5.

- premium_sd, reserve_sd:

  Undertaking-specific standard deviations replacing the standard ones,
  named by segment number (such as `c("4" = 0.06)`), or `NULL`.

## Value

A list with `by_segment`, a data frame with each segment's volumes, its
standard deviations, its volume measure and its capital on its own (\\3
\sigma_s V_s\\); `total`, the volume measure, the standard deviation,
the capital requirement, the sum of the segments' capital and the
diversification between them; and `reserve_risk` and `premium_risk`, the
capital of each risk alone (the same formula with the other volume set
to zero).

## Details

The standard deviation of a segment combines those of its premium and
reserve risk with a correlation of 50%: \$\$\sigma_s =
\frac{\sqrt{(\sigma\_{p,s} V\_{p,s})^2 + \sigma\_{p,s} \sigma\_{r,s}
V\_{p,s} V\_{r,s} + (\sigma\_{r,s} V\_{r,s})^2}}{V\_{p,s} +
V\_{r,s}},\$\$ and its volume is \\V_s = (V\_{p,s} + V\_{r,s}) (0.75 +
0.25 DIV_s)\\, with \\DIV_s\\ its geographical diversification (1 for
business in a single region). The segments are joined with the
correlations \\CorrS\\ of Annex IV: \\\sigma = \sqrt{\sum\_{s,t}
CorrS\_{s,t} \sigma_s V_s \sigma_t V_t} / V\\, with \\V = \sum_s V_s\\,
and the capital requirement is \\3 \sigma V\\.

The premium volume of a segment is, under Article 116, the larger of the
earned premium of the next year and of the last year, plus the expected
premiums of existing and future contracts beyond the next year; the
reserve volume is its best estimate of the claims provision, net of
reinsurance. They are given here; the function does not derive them. The
adjustment for non-proportional reinsurance (80% of the premium standard
deviation of segments 1, 4 and 5, Article 117(3)) is applied to the
segments asked for, where the undertaking's reinsurance qualifies.

## References

Commission Delegated Regulation (EU) 2015/35, Articles 115 to 117 and
Annexes II and IV, as amended by Commission Delegated Regulation (EU)
2019/981.

## See also

[`sii_nonlife_segments`](https://netsimanalytics.github.io/NetSimR/reference/sii_nonlife_segments.md),
[`risk_adjustment`](https://netsimanalytics.github.io/NetSimR/reference/risk_adjustment.md).

## Examples

``` r
volumes <- data.frame(segment = c(1, 2, 4), premium = c(40e6, 25e6, 30e6),
                      reserve = c(60e6, 10e6, 20e6))
sii <- sii_premium_reserve_risk(volumes)
sii$total
#>          volume              sd         capital      standalone diversification 
#>    1.850000e+08    6.123274e-02    3.398417e+07    4.345331e+07    9.469133e+06 
sii$by_segment[, c("name", "sd", "volume", "capital")]
#>                                name         sd  volume  capital
#> 1           Motor vehicle liability 0.08170679 1.0e+08 24512038
#> 2                       Other motor 0.07137141 3.5e+07  7493998
#> 3 Fire and other damage to property 0.07631514 5.0e+07 11447270
```
