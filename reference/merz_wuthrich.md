# The one-year view of the reserves: Merz and Wuthrich's claims development result

The standard error of the claims development result (CDR) of the next
year: how much the chain ladder's ultimate can move in one year, as the
year's claims emerge and the factors are estimated again. It is the
reserve risk over one year that Solvency II measures, against Mack's
standard error of the reserves until they are all paid.

## Usage

``` r
merz_wuthrich(triangle, sigma = c("log-linear", "Mack"))
```

## Arguments

- triangle:

  A numeric matrix of cumulative claims: one row per origin period,
  oldest first, and one column per development period, with `NA` for the
  future. A data frame is converted with
  [`as.matrix()`](https://rdrr.io/r/base/matrix.html); see
  [`as_triangle`](https://netsimanalytics.github.io/NetSimR/reference/as_triangle.md)
  to build one from a table of claims.

- sigma:

  How \\\sigma\\ is found for a period with a single link ratio:
  `"log-linear"` or `"Mack"`.

## Value

An object of class `netsimr_reserve`: a list with `by_origin`, a data
frame with each origin's latest claims, ultimate, IBNR, the standard
error of its one-year CDR and Mack's standard error until ultimate;
`total`, the same for all origins; and `chain_ladder`, the chain ladder
they come from.

## Details

The formulas of Merz and Wuthrich (2008) hold for the volume-weighted
chain ladder of every link ratio without a tail, which the function
fits; with Mack's \\\sigma_j\\ and the shares \\\alpha_j = C\_{I-j,j} /
\sum\_{i \le I-j} C\_{ij}\\, the mean squared error of each origin's CDR
is its next year's process error, \\\hat C\_{iJ}^2 \hat\sigma_j^2 /
(\hat f_j^2 C\_{ij})\\, plus the error of the factors, which counts each
later factor's error by the share \\\alpha\\ of its estimate that the
year's claims change; the total adds the correlation of the origins. The
results match the ChainLadder package's `CDR()`, and for the Taylor and
Ashe triangle Merz and Wuthrich's paper.

## References

Merz, M. and Wuthrich, M. V. (2008). Modelling the claims development
result for solvency purposes. CAS E-Forum, Fall 2008, 542-568.

## See also

[`chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md)
for Mack's standard error until ultimate, and
[`bootstrap_chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/bootstrap_chain_ladder.md).

## Examples

``` r
merz_wuthrich(taylor_ashe_triangle, sigma = "Mack")
#> Merz-Wuthrich one-year claims development result 
#> 
#>  origin     latest   ultimate       ibnr one_year_se   mack_se
#>       1  3,901,463  3,901,463          0           0         0
#>       2  5,339,085  5,433,719     94,634      75,535    75,535
#>       3  4,909,315  5,378,826    469,511     105,309   121,699
#>       4  4,588,268  5,297,906    709,638      79,846   133,549
#>       5  3,873,311  4,858,200    984,889     235,115   261,406
#>       6  3,691,712  5,111,171  1,419,459     318,427   411,010
#>       7  3,483,130  5,660,771  2,177,641     361,089   558,317
#>       8  2,864,498  6,784,799  3,920,301     629,681   875,328
#>       9  1,363,294  5,642,266  4,278,972     588,662   971,258
#>      10    344,014  4,969,825  4,625,811   1,029,925 1,363,155
#>   Total 34,358,090 53,038,946 18,680,856   1,778,968 2,447,095
```
