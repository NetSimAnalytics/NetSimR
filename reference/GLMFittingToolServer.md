# Server function for the GLM Fitting tool application

Runs a session of the GLM fitting tool: imports the data, fits the
models, compares the stored ones, draws the actual against predicted
chart and saves and loads the settings.

## Usage

``` r
GLMFittingToolServer(input, output, session)
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
