# Run the claims reserving application

Opens NetReserve, a shiny application for claims reserving: it loads a
claims triangle (an example, or a file with a row per origin and
development period or a row per origin), lets the user choose the
development factors (their average, the calendar periods used, link
ratios left out, a tail factor or curve), compares the chain ladder,
Bornhuetter-Ferguson, Benktander and Cape Cod ultimates and loss ratios
of each origin, and measures the uncertainty of the reserves with Mack's
standard errors and a bootstrap.

## Usage

``` r
run_shiny_reserving_tool()
```

## Value

A shiny app object. Printing it, as happens when the function is called
at the console, opens the application; it can also be passed to
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## See also

[`chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md),
[`bornhuetter_ferguson`](https://netsimanalytics.github.io/NetSimR/reference/bornhuetter_ferguson.md),
[`cape_cod`](https://netsimanalytics.github.io/NetSimR/reference/cape_cod.md)
and
[`bootstrap_chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/bootstrap_chain_ladder.md),
the methods of the application;
[`run_shiny_simulator`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_simulator.md),
[`run_shiny_distribution_fitting_tool`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_distribution_fitting_tool.md)
and
[`run_shiny_glm_fitting_tool`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_glm_fitting_tool.md),
the package's other applications.

## Examples

``` r
if (interactive()) {
  run_shiny_reserving_tool()
}
```
