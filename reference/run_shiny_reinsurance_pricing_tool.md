# Run the reinsurance pricing application

Opens NetTreaty, a shiny application for pricing an excess of loss
layer: it rates the layer on its loss experience, with the burning cost
and a Pareto model of the large losses, and on its exposure, with a risk
profile and an MBBEFD exposure curve (property) or a claim size
distribution (casualty), blends the two with a credibility weight, and
gives the technical premium, with reinstatements, rate on line and
payback.

## Usage

``` r
run_shiny_reinsurance_pricing_tool()
```

## Value

A shiny app object. Printing it, as happens when the function is called
at the console, opens the application; it can also be passed to
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## See also

[`burning_cost`](https://netsimr.netsimanalytics.com/reference/burning_cost.md),
[`pareto_rating`](https://netsimr.netsimanalytics.com/reference/pareto_rating.md),
[`property_exposure_rating`](https://netsimr.netsimanalytics.com/reference/property_exposure_rating.md),
[`casualty_exposure_rating`](https://netsimr.netsimanalytics.com/reference/property_exposure_rating.md)
and
[`technical_premium`](https://netsimr.netsimanalytics.com/reference/technical_premium.md),
the methods of the application;
[`run_shiny_simulator`](https://netsimr.netsimanalytics.com/reference/run_shiny_simulator.md),
[`run_shiny_distribution_fitting_tool`](https://netsimr.netsimanalytics.com/reference/run_shiny_distribution_fitting_tool.md),
[`run_shiny_glm_fitting_tool`](https://netsimr.netsimanalytics.com/reference/run_shiny_glm_fitting_tool.md)
and
[`run_shiny_reserving_tool`](https://netsimr.netsimanalytics.com/reference/run_shiny_reserving_tool.md),
the package's other applications.

## Examples

``` r
if (interactive()) {
  run_shiny_reinsurance_pricing_tool()
}
```
