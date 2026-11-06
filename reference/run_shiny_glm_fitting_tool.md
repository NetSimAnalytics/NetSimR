# Run the generalised linear model fitting application

Opens a Shiny application that imports data from a CSV file or a
database query, fits generalised linear models to it, compares the AIC
of stored models, charts actual against predicted values and downloads
the model and its predictions.

## Usage

``` r
run_shiny_glm_fitting_tool()
```

## Value

A shiny app object. Printing it, as happens when the function is called
at the console, opens the application; it can also be passed to
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Details

The formula typed in the application, or read from one of its settings
files, is restricted to the columns of the imported data, numbers, the
formula operators and a small set of formula functions:
[`I()`](https://rdrr.io/r/base/AsIs.html),
[`log()`](https://rdrr.io/r/base/Log.html),
[`exp()`](https://rdrr.io/r/base/Log.html),
[`sqrt()`](https://rdrr.io/r/base/MathFun.html),
[`pmin()`](https://rdrr.io/r/base/Extremes.html),
[`pmax()`](https://rdrr.io/r/base/Extremes.html),
[`poly()`](https://rdrr.io/r/stats/poly.html),
[`factor()`](https://rdrr.io/r/base/factor.html),
[`as.factor()`](https://rdrr.io/r/base/factor.html),
[`interaction()`](https://rdrr.io/r/base/interaction.html) and
[`offset()`](https://rdrr.io/r/stats/offset.html). Any other call is
refused, so a settings file shared between colleagues cannot run code.

## See also

[`run_shiny_distribution_fitting_tool`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_distribution_fitting_tool.md)
and
[`run_shiny_simulator`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_simulator.md),
the package's other applications.

## Examples

``` r
if (interactive()) {
  run_shiny_glm_fitting_tool()
}
```
