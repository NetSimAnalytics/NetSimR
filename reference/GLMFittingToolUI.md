# User interface of the Shiny GLM fitting tool

Builds the pages of the GLM fitting tool: Welcome, Data, Model, Actual
vs predicted and Save & load, with the theme switch.

## Usage

``` r
GLMFittingToolUI(request)
```

## Arguments

- request:

  The request, supplied by shiny.

## Value

The user interface of the application, a bslib navbar page.

## Details

A function of the request, so that it is built when the app starts,
after every helper of the package is defined.
