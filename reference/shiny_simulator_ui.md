# User interface of the Shiny NetSimR simulator

The page the simulator opens in: a welcome panel and the simulator
itself, where the claim, reinsurance and run settings are entered and
the results, charts and report are shown. The page is built each time
the application starts, not when the package is installed: its theme
compiles Bootstrap's SCSS files from where bslib is installed on the
machine running it, and a page built at installation kept the paths of
the machine that built the package (for a CRAN binary, CRAN's build
machine), where nobody else's bslib is.

## Usage

``` r
shiny_simulator_ui(request)
```

## Arguments

- request:

  The request of the browser, passed by shiny; not used.

## Value

The user interface of the application, a bslib navbar page, which
[`run_shiny_simulator`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_simulator.md)
pairs with `shiny_simulator_server`.
