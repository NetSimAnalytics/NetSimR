# Simulated liability claims at their policy limits

2,500 simulated liability claims of four accident years, each paid up to
its policy limit, so the largest are cut at the limits: the example of
the severity fits of the distribution fitting tool, which fits them as
censored. The script that made it is in the package's source, under
data-raw.

## Usage

``` r
liability_claims
```

## Format

A data frame with 2,500 rows, one per claim, and 4 columns:

- claim_id:

  the claim's identifier;

- accident_year:

  2021 to 2024;

- amount:

  the amount paid, at most the policy limit;

- policy_limit:

  the policy's limit: 100,000, 250,000, 500,000 or 1,000,000.

## See also

[`run_shiny_distribution_fitting_tool`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_distribution_fitting_tool.md).

## Examples

``` r
# the claims paid at their limit
mean(liability_claims$amount >= liability_claims$policy_limit)
#> [1] 0.0816
```
