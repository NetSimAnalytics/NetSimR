# A function to run the shiny simulator application

Opens the claims simulator, a Shiny app for running the
frequency-severity model of
[`simulate_function`](https://netsimanalytics.github.io/NetSimR/reference/simulate_function.md)
from a form, with charts, a report and saved settings, without writing
any code.

## Usage

``` r
run_shiny_simulator()
```

## Value

A shiny app object (class `shiny.appobj`). Printing it, as happens when
`run_shiny_simulator()` is called at the console, opens the app; pass it
to [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) to
choose options such as the port.

## See also

[`simulate_claims`](https://netsimanalytics.github.io/NetSimR/reference/simulate_claims.md)
and
[`simulate_function`](https://netsimanalytics.github.io/NetSimR/reference/simulate_function.md),
which run the same model from R code.

## Examples

``` r
if (interactive()) {
  run_shiny_simulator()
}
```
