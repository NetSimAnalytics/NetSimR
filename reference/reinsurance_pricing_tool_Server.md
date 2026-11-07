# Server function of the reinsurance pricing tool

Reads the layer and the rating year, rates the layer on the large losses
(burning cost and Pareto) and on the risk profile (exposure curve or
claim size distribution), blends the rates and prices the layer; paired
with `reinsurance_pricing_tool_UI` by
[`run_shiny_reinsurance_pricing_tool`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_reinsurance_pricing_tool.md).

## Usage

``` r
reinsurance_pricing_tool_Server(input, output, session)
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
