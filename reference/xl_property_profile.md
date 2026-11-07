# Risk profile of a property portfolio

The bands of sum insured of a simulated property portfolio, with the
number of risks, average sum insured and premium of each: the example of
[`property_exposure_rating`](https://netsimanalytics.github.io/NetSimR/reference/property_exposure_rating.md).

## Usage

``` r
xl_property_profile
```

## Format

A data frame with 9 rows and 6 columns:

- band:

  The band, as a label.

- lower, upper:

  The band's lowest and highest sum insured.

- risks:

  The number of risks in the band.

- sum_insured:

  The average sum insured of the band's risks.

- premium:

  The premium of the band.

## Source

Simulated.

## See also

[`property_exposure_rating`](https://netsimanalytics.github.io/NetSimR/reference/property_exposure_rating.md).

## Examples

``` r
xl_property_profile
#>           band   lower   upper risks sum_insured  premium
#> 1    0 to 250k 0.0e+00 2.5e+05 42000      138000 11012000
#> 2 250k to 500k 2.5e+05 5.0e+05 18500      382000 12014000
#> 3   500k to 1m 5.0e+05 1.0e+06  9200      750000 10695000
#> 4   1m to 2.5m 1.0e+06 2.5e+06  3800     1715000  9124000
#> 5   2.5m to 5m 2.5e+06 5.0e+06  1150     3638000  5439000
#> 6    5m to 10m 5.0e+06 1.0e+07   420     7200000  3629000
#> 7   10m to 25m 1.0e+07 2.5e+07   160    16450000  2895000
#> 8   25m to 50m 2.5e+07 5.0e+07    48    34875000  1674000
#> 9  50m to 100m 5.0e+07 1.0e+08    12    69000000   745000
```
