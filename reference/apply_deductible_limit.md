# Apply a deductible and limit to claims

Works out what a reinsurance structure cedes (or, for an excluded layer,
leaves) of each claim, for pricing a layer or checking the simulator's
figures by hand.

## Usage

``` r
apply_deductible_limit(
  gross_claims_data,
  reinsurance_structure,
  deductible,
  limit
)
```

## Arguments

- gross_claims_data:

  A vector of Claims.

- reinsurance_structure:

  The chosen reinsurance structure, a single string. Options are: 'No
  Reinsurance Structure', 'Unlimited Layer', 'Limited Layer', 'Exclude
  Layer'; anything else is an error.

- deductible:

  The deductible of the reinsurance structure, zero or more. Not used by
  'No Reinsurance Structure'.

- limit:

  The limit of the reinsurance structure, zero or more. Used only by
  'Limited Layer' and 'Exclude Layer'.

## Value

A vector with one value per claim: for 'Unlimited Layer' and 'Limited
Layer', the amount ceded to the layer; for 'Exclude Layer', the claims
with the layer taken out (the claims less what a 'Limited Layer' with
the same deductible and limit would cede); for 'No Reinsurance
Structure', the claims unchanged. Stops with an error when a deductible
or limit the structure uses is negative.

## Examples

``` r
apply_deductible_limit(c(100, 50, 20), 'Limited Layer', 40, 20)
#> [1] 20 10  0
apply_deductible_limit(c(100, 50, 20), 'Limited Layer', 10, 30)
#> [1] 30 30 10
apply_deductible_limit(c(100, 50, 20), 'Exclude Layer', 40, 20)
#> [1] 80 40 20
```
