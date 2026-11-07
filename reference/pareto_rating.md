# Pareto rating of an excess of loss layer

Experience rating of a layer with a Pareto distribution fitted to the
large losses: the losses above a `threshold`, brought to the money of
the rating year, give the frequency of large losses per unit of on-level
premium and the Pareto alpha; the layer's expected loss is the frequency
times the expected part of a large loss in the layer. Unlike the burning
cost it prices layers above the largest loss seen.

## Usage

``` r
pareto_rating(
  losses,
  premium,
  threshold,
  deductible,
  limit,
  inflation = 0,
  rating_year = NULL,
  development = 1,
  premium_index = 1,
  exclude = NULL,
  alpha = NULL,
  rating_premium = NULL
)
```

## Arguments

- losses:

  A data frame of the large losses with the columns `year` and `amount`
  (in the money of their year); other columns, such as a claim number,
  are kept.

- premium:

  The subject premium of each year: a data frame with the columns `year`
  and `premium`, or numbers named by year. Its years are those of the
  experience; a year without losses has none in the layer.

- threshold:

  The threshold of the Pareto, in the money of the rating year: the
  losses above it, indexed, are used. At most the deductible.

- deductible, limit:

  The layer, `limit` (or `Inf`) excess of `deductible`, for each loss.

- inflation:

  An annual rate (0.05 for 5%) or index values named by year, including
  the rating year.

- rating_year:

  The year being priced; by default the year after the last.

- development:

  One factor for all years, one per year, or factors named by year,
  applied to each year's number of large losses. Defaults to 1.

- premium_index:

  The factor that brings each year's premium to the rating year's rates
  and exposure, as `development`. Defaults to 1.

- exclude:

  Years left out of the burning cost (they are still shown).

- alpha:

  The Pareto alpha, to use a market or selected alpha instead of the one
  estimated; NULL (the default) estimates it.

- rating_premium:

  The subject premium of the rating year; the expected layer loss is the
  burning cost times it. NULL (the default) gives no expected loss.

## Value

An object of class `netsimr_pareto_rating`, a list with `alpha`,
`alpha_estimated` (the estimate, also when `alpha` is given), `n` (the
losses above the threshold in the years included), `frequency` (large
losses per unit of on-level premium), `expected_count` (at the rating
premium), `layer_severity` (the expected part of a large loss in the
layer), `rate`, `expected_loss`, `by_year` and `losses` (the indexed
losses above the threshold).

## Details

Alpha is estimated by maximum likelihood, corrected for its bias:
\\\hat\alpha = (n - 1) / \sum \log(x_i / t)\\ for the \\n\\ indexed
losses \\x_i\\ above the threshold \\t\\. The number of large losses of
each year is multiplied by its `development` factor, for the losses not
reported yet, and their total divided by the total on-level premium. The
expected part of a large loss in the layer is \\E\[\min(X, d + l)\] -
E\[\min(X, d)\]\\ for a Pareto with minimum \\t\\
([`ParetoCappedMean`](https://netsimanalytics.github.io/NetSimR/reference/ParetoCappedMean.md)),
which needs a deductible of at least the threshold. Aggregate terms are
not applied.

## References

Schmutz, M. and Doerr, R. R. (1998). The Pareto model in property
reinsurance: formulas and applications. Swiss Re.

## See also

[`burning_cost`](https://netsimanalytics.github.io/NetSimR/reference/burning_cost.md)
and
[`technical_premium`](https://netsimanalytics.github.io/NetSimR/reference/technical_premium.md).

Other reinsurance pricing functions:
[`burning_cost()`](https://netsimanalytics.github.io/NetSimR/reference/burning_cost.md),
[`credibility_weight()`](https://netsimanalytics.github.io/NetSimR/reference/credibility_weight.md),
[`mbbefd_curve()`](https://netsimanalytics.github.io/NetSimR/reference/mbbefd_curve.md),
[`property_exposure_rating()`](https://netsimanalytics.github.io/NetSimR/reference/property_exposure_rating.md),
[`technical_premium()`](https://netsimanalytics.github.io/NetSimR/reference/technical_premium.md)

## Examples

``` r
pareto_rating(xl_large_losses, xl_premium[, c("year", "premium")], threshold = 7.5e5,
              deductible = 1e6, limit = 4e6, inflation = 0.04,
              premium_index = setNames(xl_premium$rate_index, xl_premium$year),
              rating_premium = 60e6)
#> Pareto rating of 4,000,000 xs 1,000,000 for 2025
#>   Threshold:          750,000
#>   Losses above it:    34
#>   Alpha:              1.567
#>   Rate:               4.676% of subject premium
#>   Expected count:     4.17 losses above the threshold
#>   Expected layer loss: 2,805,527 
```
