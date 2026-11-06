# Server function for the Distribution Fitting tool application

Reads the uploaded file, fits the frequency, severity, sliced and
piecewise Pareto models the user asks for and renders their tables and
charts; paired with `distribution_fitting_tool_UI` by
[`run_shiny_distribution_fitting_tool`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_distribution_fitting_tool.md).

## Usage

``` r
distribution_fitting_tool_Server(input, output, session)
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
