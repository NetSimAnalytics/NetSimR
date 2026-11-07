# MBBEFD exposure curves

The exposure curves of Bernegger's MBBEFD class, used to rate property
excess of loss treaties from a risk profile. `mbbefd_curve()` gives the
share of a risk's expected loss that falls below a deductible of `x`
times its sum insured (or maximum possible loss), for the parameters `b`
and `g`. `swiss_re_parameters()` gives `b` and `g` of the one-parameter
curves of Swiss Re, and `swiss_re_curve()` the curve itself: c = 1.5, 2,
3 and 4 are the curves Y1 to Y4, from personal lines to large industrial
risks, and c = 5 the curve used at Lloyd's for industrial risks.

## Usage

``` r
mbbefd_curve(x, b, g)

swiss_re_parameters(c)

swiss_re_curve(x, c)
```

## Arguments

- x:

  The deductible as a share of the sum insured: numbers from 0; those
  above 1 are taken as 1.

- b, g:

  The parameters of the curve: `b` at least 0 and `g` at least 1. The
  arguments are recycled to a common length.

- c:

  The parameter of a Swiss Re curve, at least 0, or the name of one:
  "Y1", "Y2", "Y3", "Y4" or "Lloyds".

## Value

`mbbefd_curve()` and `swiss_re_curve()`: the curve at `x`, between 0
and 1. `swiss_re_parameters()`: a data frame with the columns c, b and
g, a row for each value of `c`.

## Details

With \\g = 1\\ or \\b = 0\\ every loss is a total loss and \\G(x) = x\\;
with \\b = 1\\, \\G(x) = \log(1 + (g - 1)x) / \log(g)\\; with \\bg =
1\\, \\G(x) = (1 - b^x) / (1 - b)\\; otherwise \$\$G(x) =
\log\left(\frac{(g - 1)b + (1 - gb)b^x}{1 - b}\right) / \log(gb).\$\$
The probability of a total loss is \\1/g\\. The Swiss Re curves have \\b
= e^{3.1 - 0.15(1 + c)c}\\ and \\g = e^{(0.78 + 0.12c)c}\\.

A layer of `limit` excess of `deductible` on a risk with sum insured
\\M\\ takes the share \\G((d + l) / M) - G(d / M)\\ of the risk's
expected loss; see
[`property_exposure_rating`](https://netsimanalytics.github.io/NetSimR/reference/property_exposure_rating.md).

## References

Bernegger, S. (1997). The Swiss Re exposure curves and the MBBEFD
distribution class. ASTIN Bulletin 27(1), 99-111.

## See also

Other reinsurance pricing functions:
[`burning_cost()`](https://netsimanalytics.github.io/NetSimR/reference/burning_cost.md),
[`credibility_weight()`](https://netsimanalytics.github.io/NetSimR/reference/credibility_weight.md),
[`pareto_rating()`](https://netsimanalytics.github.io/NetSimR/reference/pareto_rating.md),
[`property_exposure_rating()`](https://netsimanalytics.github.io/NetSimR/reference/property_exposure_rating.md),
[`technical_premium()`](https://netsimanalytics.github.io/NetSimR/reference/technical_premium.md)

## Examples

``` r
# a layer of 4m excess of 1m on a risk of 10m, on the Y3 curve
swiss_re_curve(0.5, "Y3") - swiss_re_curve(0.1, "Y3")
#> [1] 0.3713214
swiss_re_parameters(c("Y1", "Y2", "Y3", "Y4", "Lloyds"))
#>     c         b          g
#> 1 1.5 12.648011   4.220696
#> 2 2.0  9.025013   7.690609
#> 3 3.0  3.669297  30.569415
#> 4 4.0  1.105171 154.470015
#> 5 5.0  0.246597 992.274716
mbbefd_curve(c(0, 0.25, 0.5, 1), b = 2, g = 10)
#> [1] 0.0000000 0.5090424 0.7285969 1.0000000
```
