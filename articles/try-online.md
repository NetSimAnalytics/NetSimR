# Try the tools in your browser

The five Shiny tools of NetSimR run right here, in your browser, with
[webR](https://docs.r-wasm.org/webr/latest/), R compiled to WebAssembly,
and [shinylive](https://posit-dev.github.io/r-shinylive/). Nothing to
install, and nothing leaves your computer: the data you upload and the
models you fit stay in the page.

### Claims simulator

Frequency-severity simulation with Pareto tails, caps and reinsurance
layers, with return periods and a report.

[Open the
simulator](https://netsimanalytics.github.io/NetSimR/apps/simulator/)

### Distribution fitting

Fit claim frequency and severity distributions to your data, compare
them and send the fits to the simulator.

[Open the fitting
tool](https://netsimanalytics.github.io/NetSimR/apps/distribution/)

### GLM fitting

Fit generalised linear models to a CSV file, compare them and download
the rating factors.

[Open the GLM tool](https://netsimanalytics.github.io/NetSimR/apps/glm/)

### Claims reserving

The chain ladder with Mack's standard errors, Bornhuetter-Ferguson, Cape
Cod, the bootstrap and the Munich chain ladder.

[Open the reserving
tool](https://netsimanalytics.github.io/NetSimR/apps/reserving/)

### Reinsurance pricing

Price an excess of loss layer with the burning cost, a Pareto model and
exposure curves, and blend them.

[Open the pricing
tool](https://netsimanalytics.github.io/NetSimR/apps/reinsurance/)

## Good to know

- **The first start takes a little while**: the browser downloads R and
  the packages, a few tens of megabytes. Later starts are quicker, as
  the browser keeps them.
- **It runs on your computer.** Uploaded files are read by the R running
  in the page and are not sent anywhere. Closing the tab ends the
  session; save the settings of a tool to come back to it.
- **Some things need R on your computer**: the simulator’s parallel
  workers, and the GLM tool’s database connections, are not available in
  the browser. Large simulations also run faster in R.
- A recent version of Chrome, Edge, Firefox or Safari is needed.

## In R

The same tools run from R, after installing the package from CRAN or,
for the newest version, from GitHub:

``` r

install.packages("NetSimR")
# or: remotes::install_github("NetSimAnalytics/NetSimR")
library(NetSimR)
run_shiny_simulator()
run_shiny_distribution_fitting_tool()
run_shiny_glm_fitting_tool()
run_shiny_reserving_tool()
run_shiny_reinsurance_pricing_tool()
```
