# Server function for the Shiny Simulator application

Builds the dynamic inputs of the claims simulator, runs the simulations
when asked and serves the results tabs, the downloads and the saving and
loading of settings; it is the server that
[`run_shiny_simulator`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_simulator.md)
pairs with `shiny_simulator_ui`.

## Usage

``` r
shiny_simulator_server(input, output, session)
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
