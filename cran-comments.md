## Release summary

NetSimR 0.3.0 follows 0.2.0 (published 2026-09-13). It:

* fixes functions that returned wrong results without an error: the pure
  IBNR functions with mixed Date and POSIXct dates (their results also no
  longer depend on the time zone), simulate_function(), which rounded its
  results to two decimals, and the Gamma fit of the distribution fitting tool
  for very large claims;
* adds input checks to the analytic functions, which returned
  plausible-looking numbers (e.g. negative probabilities) for invalid
  parameters;
* applies the market convention for an aggregate deductible combined with an
  each-and-every-loss layer;
* reduces the dependencies: plotly, reactable, fitdistrplus, future.apply and
  htmltools are no longer imported (35 packages installed instead of 82);
* adds a vignette for simulate_claims().

See NEWS for details.

## Test environments

* local Windows 11 x64, R 4.6.0 (ucrt), R CMD check --as-cran
* the test suite also run with TZ set to UTC, America/New_York,
  Australia/Sydney and Asia/Nicosia
* GitHub Actions: macOS (release), Windows (release), Ubuntu (devel, release
  and oldrel-1)

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
