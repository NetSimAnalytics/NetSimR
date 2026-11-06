# Paid and incurred triangles of the Munich chain ladder paper

The cumulative paid and incurred claims of seven accident years with
which Quarg and Mack (2004) introduce the Munich chain ladder.

## Usage

``` r
quarg_mack_paid

quarg_mack_incurred
```

## Format

Numeric matrices with 7 rows (accident years) and 7 columns (development
years), `NA` for the future, with values in the same cells.

An object of class `matrix` (inherits from `array`) with 7 rows and 7
columns.

## Source

Quarg, G. and Mack, T. (2004). Munich chain ladder. Blatter DGVFM 26(4),
597-630.

## See also

[`munich_chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/munich_chain_ladder.md).

## Examples

``` r
munich_chain_ladder(quarg_mack_paid, quarg_mack_incurred)$by_origin
#>   origin latest_paid latest_incurred latest_ratio ultimate_paid
#> 1      1        2131            2174    0.9802208      2131.000
#> 2      2        2348            2454    0.9568052      2384.842
#> 3      3        4494            4644    0.9677003      4553.624
#> 4      4        5850            6142    0.9524585      6069.509
#> 5      5        4648            4852    0.9579555      4878.950
#> 6      6        4010            4406    0.9101226      4598.996
#> 7      7        2044            5022    0.4070092      7504.576
#>   ultimate_incurred ultimate_ratio chain_ladder_paid chain_ladder_incurred
#> 1          2174.000      0.9802208          2131.000              2174.000
#> 2          2443.222      0.9761052          2380.394              2445.003
#> 3          4634.358      0.9825792          4652.181              4581.514
#> 4          6182.347      0.9817483          6181.609              6126.363
#> 5          4957.805      0.9840948          5055.601              4839.018
#> 6          4672.402      0.9842894          4934.086              4476.118
#> 7          7655.378      0.9803012          6128.340              8428.838
```
