# Simulated motor claims with premiums

The paid and incurred claims of a simulated motor portfolio, by accident
year 2014 to 2025 and development year, with each accident year's earned
premium: a table in the long form that
[`as_triangle`](https://netsimanalytics.github.io/NetSimR/reference/as_triangle.md)
turns into triangles. Each year's loss ratio is around 68%, and its
payments follow a development pattern with over-dispersed (gamma) noise;
the incurred claims add case reserves that each year sets a little high
or low. The script that made it is in the package's source, under
data-raw.

## Usage

``` r
simulated_motor_claims
```

## Format

A data frame with 78 rows, one per accident and development year of the
past, and 5 columns:

- accident_year:

  the accident year, 2014 to 2025;

- development_year:

  the development year, 1 for the accident year itself;

- paid:

  the claims paid in the development year (incremental);

- incurred:

  the change in the incurred claims (paid claims and case reserves) over
  the development year (incremental);

- earned_premium:

  the accident year's earned premium, on each of its rows.

## See also

[`as_triangle`](https://netsimanalytics.github.io/NetSimR/reference/as_triangle.md),
[`bornhuetter_ferguson`](https://netsimanalytics.github.io/NetSimR/reference/bornhuetter_ferguson.md),
[`cape_cod`](https://netsimanalytics.github.io/NetSimR/reference/cape_cod.md)
and
[`munich_chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/munich_chain_ladder.md).

## Examples

``` r
triangle <- as_triangle(simulated_motor_claims, "accident_year", "development_year", "paid",
                        cumulative = FALSE)
premium <- unique(simulated_motor_claims[c("accident_year", "earned_premium")])$earned_premium
cape_cod(triangle, premium)$total
#>              latest             premium          loss_ratio      prior_ultimate 
#>        6.929459e+07        1.204310e+08        6.971151e-01        8.395427e+07 
#>            ultimate                ibnr ultimate_loss_ratio 
#>        8.395427e+07        1.465967e+07        6.971151e-01 
```
