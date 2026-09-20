## Release summary

NetSimR 0.3.1 is a bug-fix and hardening release that follows 0.3.0 quickly
because testing of the Shiny tools in a browser, and of the functions against
each other, found problems that we would rather not leave on CRAN. It is not
purely a bug-fix release: simulate_function() and simulate_claims(), and
PureIBNRGamma() and PureIBNRLNorm(), change results in documented cases, and
the Shiny tools no longer load settings files saved as .rds (see NEWS).

* Security: the GLM fitting tool's downloaded model (an RDS file) contained
  the Shiny session, including any database password typed in the tool. The
  model is now fitted in a clean environment and the file holds only the
  model.
* Wrong results: the Negative Binomial fit of the distribution fitting tool
  often fell back to the Poisson on overdispersed data; PureIBNRGamma() and
  PureIBNRLNorm() computed their ratios from a rounded duration, which lost
  precision for short periods.
* The charts of the three Shiny tools were drawn badly in the dark theme on
  Windows and overlapped after a resize; several smaller fixes to the claims
  simulator.
* Behaviour changes: simulate_function() and simulate_claims() take an
  aggregate deductible off the layer recoveries before the aggregate limit and
  the reinstatement capacity (the market convention), and return totals at
  full precision instead of rounded to two decimals; PureIBNRGamma() and
  PureIBNRLNorm() count days on each date's own calendar, so results no
  longer depend on the time zone; the Shiny tools save settings as plain text
  files, which cannot run code when loaded, and refuse the .rds files of
  earlier versions.

See NEWS for details.

## Test environments

* local Windows 11 x64, R 4.6.0 (ucrt), R CMD check --as-cran
* the test suite also run with TZ set to UTC, America/New_York,
  Australia/Sydney and Asia/Nicosia
* GitHub Actions: macOS (release), Windows (release), Ubuntu (devel, release
  and oldrel-1)

## R CMD check results

0 errors | 0 warnings | 1 note

* "Days since last update": this release follows 0.3.0 closely because of
  the security fix and the wrong results described above.

The references in the Description field point to articles on
www.theactuary.com. The site answers automated requests with HTTP 403 (a
Cloudflare browser check), so the URL check may flag them; the links open
normally in a web browser.

## Reverse dependencies

There are no reverse dependencies on CRAN.
