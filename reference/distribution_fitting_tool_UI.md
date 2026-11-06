# User interface of the Shiny distribution fitting tool

The page of the distribution fitting application: a welcome tab, the
data upload and one tab for each analysis, with the settings beside the
results;
[`run_shiny_distribution_fitting_tool`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_distribution_fitting_tool.md)
pairs it with `distribution_fitting_tool_Server`. The page is built each
time the application starts, not when the package is installed: its
theme compiles Bootstrap's SCSS files from where bslib is installed on
the machine running it, and a page built at installation kept the paths
of the machine that built the package (for a CRAN binary, CRAN's build
machine).

## Usage

``` r
distribution_fitting_tool_UI(request)
```

## Arguments

- request:

  The request of the browser, passed by shiny; not used.

## Value

The user interface of the application, a bslib navbar page.
