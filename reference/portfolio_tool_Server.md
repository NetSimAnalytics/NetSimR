# Server function of the portfolio capital tool

Reads the lines (a frequency and a severity for each, or uploaded
simulated losses) and their dependence, runs
[`simulate_portfolio`](https://netsimanalytics.github.io/NetSimR/reference/simulate_portfolio.md)
and shows the capital of the portfolio and of each line; paired with
`portfolio_tool_UI` by
[`run_shiny_portfolio_tool`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_portfolio_tool.md).

## Usage

``` r
portfolio_tool_Server(input, output, session)
```

## Arguments

- input:

  Input for the server function.

- output:

  Output for the server function.

- session:

  Session for the server function.

## Value

Called by shiny for its side effects, the outputs and observers of a
session; the value is not used.
