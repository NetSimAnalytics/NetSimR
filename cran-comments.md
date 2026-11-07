## Release summary

NetSimR 0.3.4 adds claims reserving to the package's pricing and capital
modelling functions. It follows 0.3.3 closely because 0.3.3 was an urgent fix
(two of the Shiny tools could not start from a CRAN binary); this is the
feature release that was in progress at the time.

* New reserving functions: chain_ladder() with Mack's standard errors and
  tail factors, bornhuetter_ferguson() (and Benktander), cape_cod(),
  bootstrap_chain_ladder() (England and Verrall's ODP bootstrap),
  merz_wuthrich() (the one-year view) and munich_chain_ladder(), with
  as_triangle(), four small data sets and a vignette. Their results were
  checked against the ChainLadder package; the tests compare with stored
  values, so ChainLadder is not a dependency.
* A fourth Shiny tool, run_shiny_reserving_tool(). Like the other three, its
  page is built when the tool starts, not when the package is installed.
* Fewer dependencies: parallel simulations use base R's parallel package
  instead of future, the vignettes fit with optim() instead of crch, and the
  GLM tool reads PostgreSQL with RPostgres instead of RPostgreSQL (Suggests).
* The misspelled deductible arguments of simulate_function() are corrected;
  the old names still work, with a deprecation warning.
* Every CamelCase function also has a snake_case name. The CamelCase names
  are unchanged, so existing code keeps working.

See NEWS for details.

## Test environments

* local Windows 11 x64, R 4.6.2 (ucrt), R CMD check --as-cran
* the test suite, including the browser tests of the four Shiny tools, also
  run with TZ set to UTC, America/New_York, Australia/Sydney and Asia/Nicosia
* win-builder: R-devel (2026-11-06 r90845 ucrt) and R-release (4.6.2)
* GitHub Actions: macOS (release), Windows (release), Ubuntu (devel, release
  and oldrel-1)

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Possibly misspelled words in DESCRIPTION: Benktander, Bornhuetter, Merz,
  Wuthrich.

  These are the surnames of the authors of the reserving methods
  (Benktander, Bornhuetter and Ferguson, Merz and Wuthrich), spelled
  correctly.

The references in the Description field point to articles on
www.theactuary.com. The site answers automated requests with HTTP 403 (a
Cloudflare browser check), so the URL check may flag them; the links open
normally in a web browser.

## Reverse dependencies

There are no reverse dependencies on CRAN.
