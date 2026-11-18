# Run the distribution fitting application

Opens NetDisFit, a shiny application that fits frequency and severity
distributions to an uploaded claims file, compares them with the data,
and splices Pareto tails onto a LogNormal body or fits a piecewise
Pareto at chosen slicing points, ready for the NetSimR simulator.

## Usage

``` r
run_shiny_distribution_fitting_tool()
```

## Value

A shiny app object. Printing it, as happens when the function is called
at the console, opens the application; it can also be passed to
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## See also

[`run_shiny_simulator`](https://netsimr.netsimanalytics.com/reference/run_shiny_simulator.md)
and
[`run_shiny_glm_fitting_tool`](https://netsimr.netsimanalytics.com/reference/run_shiny_glm_fitting_tool.md),
two of the package's other applications;
[`SlicedLNormParetoMean`](https://netsimr.netsimanalytics.com/reference/SlicedLNormParetoMean.md),
[`SlicedLNormParetoCappedMean`](https://netsimr.netsimanalytics.com/reference/SlicedLNormParetoCappedMean.md),
[`SlicedGammaParetoMean`](https://netsimr.netsimanalytics.com/reference/SlicedGammaParetoMean.md)
and
[`SlicedGammaParetoCappedMean`](https://netsimr.netsimanalytics.com/reference/SlicedGammaParetoCappedMean.md)
for the moments of the sliced distributions the tool fits.

## Examples

``` r
if (interactive()) {
  run_shiny_distribution_fitting_tool()
}
```
