# Run the portfolio capital application

Opens NetCapital, a shiny application for the capital of a portfolio of
several lines of business: each line's annual losses are simulated from
a frequency and a severity, or uploaded (simulated by the claims
simulator or another model), and joined by a Gaussian or t copula with a
correlation for every pair of lines or a matrix. It gives the value at
risk and tail value at risk of each line and of the total, the
diversification benefit, each line's share of the capital (co-TVaR),
charts and a report.

## Usage

``` r
run_shiny_portfolio_tool()
```

## Value

A shiny app object. Printing it, as happens when the function is called
at the console, opens the application; it can also be passed to
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## See also

[`simulate_portfolio`](https://netsimanalytics.github.io/NetSimR/reference/simulate_portfolio.md),
the function behind the application;
[`run_shiny_simulator`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_simulator.md),
whose simulated losses it can take, and the package's other
applications.

## Examples

``` r
if (interactive()) {
  run_shiny_portfolio_tool()
}
```
