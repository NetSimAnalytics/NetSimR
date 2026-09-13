## Release summary

NetSimR 0.2.0 fixes bugs in the Pareto capped mean and in the sliced
Gamma-Pareto and LogNormal-Pareto functions with vector parameters, adds
simulate_claims(), reduces the imported packages (rmarkdown, shinybusy,
data.table, scales, shinyjs, MASS and Pareto are no longer imported; the
database drivers moved to Suggests) and redesigns the three 'shiny' tools.
See NEWS for details.

## Test environments

* local Windows 11 x64, R 4.6.0 (ucrt), R CMD check --as-cran

## R CMD check results

0 errors | 0 warnings | 1 note

* The note is local only: "Files 'README.md' or 'NEWS.md' cannot be checked
  without 'pandoc' being installed." pandoc is not on the PATH of the test
  machine.

The references in the Description field point to articles on
www.theactuary.com. The site answers automated requests with HTTP 403 (a
Cloudflare browser check), so the URL check may flag them; the links open
normally in a web browser.

## Reverse dependencies

There are no reverse dependencies on CRAN.
