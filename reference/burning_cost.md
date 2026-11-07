# Burning cost of an excess of loss layer

Experience rating of a layer of `limit` excess of `deductible`: the past
large losses are brought to the money of the rating year, put through
the layer as if it had been in force, developed to ultimate and divided
by the premium of their year, brought to the rating year's rates.

## Usage

``` r
burning_cost(
  losses,
  premium,
  deductible,
  limit,
  aggregate_deductible = 0,
  aggregate_limit = Inf,
  inflation = 0,
  rating_year = NULL,
  development = 1,
  premium_index = 1,
  threshold = NULL,
  exclude = NULL,
  weights = c("premium", "equal"),
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

- deductible, limit:

  The layer, `limit` (or `Inf`) excess of `deductible`, for each loss.

- aggregate_deductible, aggregate_limit:

  The annual aggregate deductible and limit of the layer's losses; the
  aggregate limit is often the limit times one plus the number of
  reinstatements.

- inflation:

  An annual rate (0.05 for 5%) or index values named by year, including
  the rating year.

- rating_year:

  The year being priced; by default the year after the last.

- development:

  One factor for all years, one per year, or factors named by year,
  applied to each year's layer losses. Defaults to 1.

- premium_index:

  The factor that brings each year's premium to the rating year's rates
  and exposure, as `development`. Defaults to 1.

- threshold:

  The reporting threshold of the losses, used for the warning; NULL (the
  default) for none.

- exclude:

  Years left out of the burning cost (they are still shown).

- weights:

  "premium" (the default) for the total layer losses over the total
  on-level premium, or "equal" for the mean of the years' burning costs.

- rating_premium:

  The subject premium of the rating year; the expected layer loss is the
  burning cost times it. NULL (the default) gives no expected loss.

## Value

An object of class `netsimr_burning_cost`, a list with `by_year` (each
year's number of losses and of losses in the layer, its indexed losses,
layer losses, development, developed and aggregate layer losses,
premium, premium index, on-level premium, burning cost and whether it is
included, and, with `rating_premium`, the layer losses at the rating
year's premium), `losses` (each loss indexed and in the layer), `rate`
(the burning cost), `expected_loss`, `rating_year`, `rating_premium`,
`n_layer_losses` (in the years included) and the layer.

## Details

Each loss is indexed to the rating year with `inflation`, and its part
in the layer, \\\min(\max(x - d, 0), l)\\, taken. Each year's layer
losses are multiplied by its `development` factor (for the late reported
claims and the development of the known ones, e.g. from
[`chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md)),
and then the aggregate deductible and limit apply. The burning cost of a
year is its layer losses over its subject premium times its
`premium_index` (the on-level premium). The burning cost of the layer is
the total over the total, weighting years by premium, or the mean of the
years' rates.

Losses are usually given above a reporting threshold. A loss just below
the threshold, indexed, reaches the layer when the indexed threshold
exceeds the deductible; a warning names the years where it does.

## See also

[`pareto_rating`](https://netsimanalytics.github.io/NetSimR/reference/pareto_rating.md),
[`property_exposure_rating`](https://netsimanalytics.github.io/NetSimR/reference/property_exposure_rating.md)
and
[`technical_premium`](https://netsimanalytics.github.io/NetSimR/reference/technical_premium.md).

Other reinsurance pricing functions:
[`credibility_weight()`](https://netsimanalytics.github.io/NetSimR/reference/credibility_weight.md),
[`mbbefd_curve()`](https://netsimanalytics.github.io/NetSimR/reference/mbbefd_curve.md),
[`pareto_rating()`](https://netsimanalytics.github.io/NetSimR/reference/pareto_rating.md),
[`property_exposure_rating()`](https://netsimanalytics.github.io/NetSimR/reference/property_exposure_rating.md),
[`technical_premium()`](https://netsimanalytics.github.io/NetSimR/reference/technical_premium.md)

## Examples

``` r
bc <- burning_cost(xl_large_losses, xl_premium[, c("year", "premium")],
                   deductible = 1e6, limit = 4e6, inflation = 0.04,
                   development = setNames(xl_premium$development, xl_premium$year),
                   premium_index = setNames(xl_premium$rate_index, xl_premium$year),
                   threshold = 5e5, rating_premium = 60e6)
bc
#> Burning cost of 4,000,000 xs 1,000,000 for 2025
#>   Years:              10 (2015 to 2024)
#>   Losses in the layer: 25 
#>   Burning cost:       4.972% of subject premium (premium weighted)
#>   Expected layer loss: 2,983,162 
bc$by_year[, c("year", "layer_losses", "onlevel_premium", "burning_cost")]
#>    year layer_losses onlevel_premium burning_cost
#> 1  2015   4213705.33        38728000  0.108802554
#> 2  2016    550147.77        40800000  0.013484014
#> 3  2017         0.00        42669000  0.000000000
#> 4  2018    441448.23        45696000  0.009757150
#> 5  2019   6278006.84        48300000  0.132579026
#> 6  2020     44983.18        50774000  0.000921387
#> 7  2021   4068163.53        52624000  0.083490738
#> 8  2022   4893266.79        54796000  0.102694664
#> 9  2023   1564784.64        56358000  0.036094610
#> 10 2024    366560.00        58400000  0.010356575
```
