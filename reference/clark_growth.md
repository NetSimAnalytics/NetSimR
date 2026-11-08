# Clark's growth curve reserves

Reserves from a growth curve fitted to the incremental claims by maximum
likelihood, after Clark (2003): the LDF method, with an ultimate for
each origin, or the Cape Cod method, with one expected loss ratio on the
premiums. Its standard errors have a process part and a parameter part.

## Usage

``` r
clark_growth(
  triangle,
  curve = c("loglogistic", "weibull"),
  premium = NULL,
  method = c("ldf", "cape cod"),
  truncation = Inf,
  offset = 0.5
)
```

## Arguments

- triangle:

  A triangle of cumulative claims, as for
  [`chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md).

- curve:

  `"loglogistic"` (the default) or `"weibull"`.

- premium:

  The premium of each origin: needed by the Cape Cod method, and used
  for the loss ratios when given to the LDF method.

- method:

  `"ldf"` (the default) or `"cape cod"`.

- truncation:

  The age, in development periods, at which the curve is cut off and the
  claims are taken to be fully developed: `Inf` (the default) for none.

- offset:

  The average date of the claims in an origin period, as a share of a
  period from its start: 0.5 (the default) for claims spread evenly over
  it, 0 to measure ages from the start.

## Value

An object of class `netsimr_reserve`: a list with `by_origin`, a data
frame with each origin's latest claims, share developed, ultimate, IBNR,
standard error, coefficient of variation and its process and parameter
parts (and premium and ultimate loss ratio when there are premiums);
`total`; `parameters`, the curve's \\\omega\\ and \\\theta\\ (and the
loss ratio of the Cape Cod method), \\\sigma^2\\ and the degrees of
freedom; `growth`, the fitted curve as a function of the age; `fitted`
and `residuals`, the fitted increments and the Pearson residuals \\(c -
\mu) / \sqrt{\sigma^2 \mu}\\, as triangles; and `covariance`.

## Details

The share of an origin's ultimate developed by age \\x\\ is a growth
curve: the log-logistic \\G(x) = x^\omega / (x^\omega + \theta^\omega)\\
or the Weibull \\G(x) = 1 - e^{-(x / \theta)^\omega}\\. The age is
measured from the average date of the claims in an origin period,
`offset` of a period after its start (a half for claims spread evenly
over it), so the increment of development period \\t\\ is expected to be
\\\mu\_{it} = U_i (G(t - o) - G(t - 1 - o))\\, with \\G = 0\\ at ages up
to 0. \\U_i\\ is the ultimate of each origin (LDF method) or its premium
times one expected loss ratio (Cape Cod method). The increments are
taken to be over-dispersed Poisson: the parameters maximise \\\sum
c\_{it} \log \mu\_{it} - \mu\_{it}\\, so that each \\U_i\\ (or the loss
ratio) has a closed form given \\\omega\\ and \\\theta\\, which are
found numerically. The scale \\\sigma^2\\ is the sum of the squared
Pearson residuals over the degrees of freedom (the cells less the
parameters).

The reserve of an origin is \\U_i (G(T - o) - G(a_i - o))\\, from its
latest age \\a_i\\ to the truncation age \\T\\ (infinite by default; the
growth curves can have long tails, so a finite truncation is often used
with the LDF method). Its process variance is \\\sigma^2\\ times the
reserve; its parameter variance comes from the covariance of the
parameters, \\\sigma^2\\ times the inverse of minus the second
derivatives of the log-likelihood, by the delta method. The results
agree with the ChainLadder package's `ClarkLDF()` and `ClarkCapeCod()`.

## References

Clark, D. R. (2003). LDF curve-fitting and stochastic reserving: a
maximum likelihood approach. Casualty Actuarial Society Forum, Fall
2003, 41-92.

## See also

[`chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md)
and
[`cape_cod`](https://netsimanalytics.github.io/NetSimR/reference/cape_cod.md).

## Examples

``` r
clark <- clark_growth(raa_triangle)
clark
#> Clark LDF method, log-logistic curve 
#> 
#>  origin  latest developed ultimate    ibnr     se    cv process_se parameter_se
#>    1981  18,834     0.822   22,906   4,072  2,792 0.686      1,950        1,998
#>    1982  16,704     0.799   20,899   4,195  2,834 0.676      1,980        2,028
#>    1983  23,466     0.771   30,440   6,974  4,049 0.581      2,553        3,143
#>    1984  27,067     0.735   36,821   9,754  5,143 0.527      3,019        4,164
#>    1985  26,180     0.689   37,994  11,814  5,855 0.496      3,322        4,821
#>    1986  15,852     0.628   25,225   9,373  4,879 0.521      2,959        3,879
#>    1987  12,314     0.547   22,527  10,213  5,202 0.509      3,089        4,186
#>    1988  13,112     0.434   30,219  17,107  7,570 0.443      3,998        6,429
#>    1989   5,395     0.278   19,397  14,002  7,503 0.536      3,617        6,574
#>    1990   2,063     0.081   25,567  23,504 17,196 0.732      4,686       16,545
#>   Total 160,987     0.592  271,994 111,007 36,075 0.325     10,184       34,608
clark$parameters
#>      omega      theta     sigma2         df 
#>   1.346491   3.045890 934.249344  43.000000 

# the Cape Cod method with a Weibull curve
premium <- c(50000, 52000, 54000, 56000, 58000, 60000, 62000, 64000, 66000, 68000)
clark_growth(raa_triangle, curve = "weibull", premium = premium, method = "cape cod")$total
#>              latest           developed            ultimate                ibnr 
#>        1.609870e+05        6.742494e-01        2.387648e+05        7.777777e+04 
#>                  se                  cv          process_se        parameter_se 
#>        1.534206e+04        1.972550e-01        8.201597e+03        1.296582e+04 
#>             premium ultimate_loss_ratio 
#>        5.900000e+05        4.046860e-01 
```
