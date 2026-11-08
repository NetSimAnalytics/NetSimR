# Simulated motor policies

A simulated year of 10,000 motor policies, with their rating factors,
exposure, claim counts and claim costs: the example of the GLM fitting
tool and of the claim count fits of the distribution fitting tool.
Claims are most frequent for the youngest drivers, in the North and for
motorcycles, and cost more for vans and dearer vehicles. The script that
made it is in the package's source, under data-raw.

## Usage

``` r
motor_policies
```

## Format

A data frame with 10,000 rows, one per policy, and 8 columns:

- policy_id:

  the policy's identifier;

- age:

  the driver's age, 18 to 85;

- region:

  North, South, East or West;

- vehicle:

  car, van or motorcycle;

- vehicle_value:

  the vehicle's value;

- exposure:

  the share of the year the policy was in force;

- claim_count:

  the number of claims in the year;

- claim_cost:

  the total cost of the claims, 0 without a claim.

## See also

[`run_shiny_glm_fitting_tool`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_glm_fitting_tool.md),
[`tweedie_family`](https://netsimanalytics.github.io/NetSimR/reference/tweedie_family.md).

## Examples

``` r
frequency <- glm(claim_count ~ offset(log(exposure)) + region + vehicle,
                 family = poisson(), data = motor_policies)
round(exp(coef(frequency)), 3)
#>       (Intercept)       regionNorth       regionSouth        regionWest 
#>             0.109             1.236             0.932             0.867 
#> vehiclemotorcycle        vehiclevan 
#>             1.574             1.154 
```
