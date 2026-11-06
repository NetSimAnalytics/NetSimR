# Server function of the reserving tool

Builds the triangle from the example or the uploaded file, fits the
chain ladder with the user's development settings, computes the
ultimates of each method with the premiums and loss ratios of each
origin, and runs the bootstrap; paired with `reserving_tool_UI` by
[`run_shiny_reserving_tool`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_reserving_tool.md).

## Usage

``` r
reserving_tool_Server(input, output, session)
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
