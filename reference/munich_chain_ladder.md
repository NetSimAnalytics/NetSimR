# Munich chain ladder of paid and incurred claims

Projects a paid and an incurred triangle together, so that their
ultimates agree: where an origin's paid to incurred ratio is below the
average, its paid claims develop faster and its incurred claims slower
than the chain ladder's, and the other way round.

## Usage

``` r
munich_chain_ladder(paid, incurred, sigma = c("log-linear", "Mack"))
```

## Arguments

- paid, incurred:

  Triangles of cumulative paid and incurred claims, as for
  [`chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md),
  with values in the same cells.

- sigma:

  How the chain ladders' \\\sigma\\ is found for a period with a single
  link ratio: `"log-linear"` or `"Mack"`.

## Value

An object of class `netsimr_reserve`: a list with `by_origin`, a data
frame with each origin's latest paid and incurred claims, their ratio,
the Munich chain ladder ultimates of both and their ratio, and the
separate chain ladder ultimates; `total`; the completed triangles
`paid_full` and `incurred_full`; `lambda` (paid and incurred), `q` (the
average paid to incurred ratio of each development period) and the two
chain ladders.

## Details

Following Quarg and Mack (2004), the chain ladder factor of each
triangle is corrected by the origin's deviation from the average paid to
incurred ratio (\\q_j\\, or its inverse for the paid triangle) of the
development period: \\\hat f^P_j + \lambda^P \hat\sigma^P_j /
\hat\rho^P_j (I\_{ij} / P\_{ij} - 1 / q_j)\\, and likewise for incurred,
where \\\lambda\\ is the slope of the link ratio residuals on the ratio
residuals. The \\\sigma\\ of a development period with a single link
ratio or ratio is extrapolated log-linearly, as in the ChainLadder
package's `MunichChainLadder()`, whose results the function reproduces.

## References

Quarg, G. and Mack, T. (2004). Munich chain ladder. Blatter DGVFM 26(4),
597-630.

## See also

[`chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md).

## Examples

``` r
munich_chain_ladder(quarg_mack_paid, quarg_mack_incurred)
#> Munich chain ladder 
#> 
#>  origin latest_paid latest_incurred latest_ratio ultimate_paid
#>       1       2,131           2,174        0.980         2,131
#>       2       2,348           2,454        0.957         2,385
#>       3       4,494           4,644        0.968         4,554
#>       4       5,850           6,142        0.952         6,070
#>       5       4,648           4,852        0.958         4,879
#>       6       4,010           4,406        0.910         4,599
#>       7       2,044           5,022        0.407         7,505
#>   Total      25,525          29,694        0.860        32,121
#>  ultimate_incurred ultimate_ratio chain_ladder_paid chain_ladder_incurred
#>              2,174          0.980             2,131                 2,174
#>              2,443          0.976             2,380                 2,445
#>              4,634          0.983             4,652                 4,582
#>              6,182          0.982             6,182                 6,126
#>              4,958          0.984             5,056                 4,839
#>              4,672          0.984             4,934                 4,476
#>              7,655          0.980             6,128                 8,429
#>             32,720          0.982            31,463                33,071
```
