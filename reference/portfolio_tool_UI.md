# User interface of the Shiny portfolio capital tool

The page of the portfolio capital application: a welcome tab, the lines,
their dependence and the capital, with the settings beside the results;
[`run_shiny_portfolio_tool`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_portfolio_tool.md)
pairs it with `portfolio_tool_Server`. Like the package's other
applications, the page is built each time the application starts, not
when the package is installed.

## Usage

``` r
portfolio_tool_UI(request)
```

## Arguments

- request:

  The request of the browser, passed by shiny; not used.

## Value

The user interface of the application, a bslib navbar page.
