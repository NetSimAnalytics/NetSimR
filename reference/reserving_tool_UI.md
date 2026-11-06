# User interface of the Shiny reserving tool

The page of the reserving application: a welcome tab, the data, the
development factors, the ultimates of each method and the uncertainty of
the reserves, with the settings beside the results;
[`run_shiny_reserving_tool`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_reserving_tool.md)
pairs it with `reserving_tool_Server`. Like the package's other
applications, the page is built each time the application starts, not
when the package is installed, so that its theme compiles from where
bslib is installed on the machine running it.

## Usage

``` r
reserving_tool_UI(request)
```

## Arguments

- request:

  The request of the browser, passed by shiny; not used.

## Value

The user interface of the application, a bslib navbar page.
