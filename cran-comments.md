## Release summary

NetSimR 0.4.0 adds claims reserving, excess of loss reinsurance pricing and
the capital of a portfolio of lines to the package's pricing and capital
modelling functions, with three new Shiny tools. It follows 0.3.3, an urgent
fix (two of the Shiny tools could not start from a CRAN binary); 0.3.4 was
never released, and its changes are part of this version.

* New reserving functions: chain_ladder() with Mack's standard errors and
  tail factors, bornhuetter_ferguson() (and Benktander), cape_cod(),
  bootstrap_chain_ladder() (England and Verrall's ODP bootstrap),
  merz_wuthrich() (the one-year view), munich_chain_ladder(), clark_growth()
  (Clark's growth curves), actual_vs_expected() and
  inflation_adjusted_chain_ladder(), with as_triangle(), small data sets and a
  vignette. Their results were checked against the ChainLadder package; the
  tests compare with stored values, so ChainLadder is not a dependency.
* Excess of loss pricing: burning_cost(), pareto_rating(),
  property_exposure_rating() and casualty_exposure_rating() with Bernegger's
  MBBEFD exposure curves (mbbefd_curve(), checked against the mbbefd package;
  the tests compare with stored values, so mbbefd is not a dependency),
  technical_premium() and credibility_weight(), with small data sets and a
  vignette.
* Capital: simulate_portfolio() joins lines of business with a Gaussian or t
  copula, with VaR, TVaR, diversification and a co-TVaR allocation.
* tweedie_family(), a Tweedie family for glm(), checked against statmod (by
  stored values, so statmod is not a dependency).
* Three new Shiny tools, run_shiny_reserving_tool(),
  run_shiny_reinsurance_pricing_tool() and run_shiny_portfolio_tool(). Like
  the other three, their pages are built when a tool starts, not when the
  package is installed.
* Dependencies: parallel simulations use base R's parallel package instead of
  future, the vignettes fit with optim() instead of crch, and the GLM tool
  reads PostgreSQL with RPostgres instead of RPostgreSQL (Suggests). glmnet
  is suggested, for the GLM tool's penalised variable selection; splines (part
  of R) is imported.
* The misspelled deductible arguments of simulate_function() are corrected;
  the old names still work, with a deprecation warning.
* Every CamelCase function also has a snake_case name. The CamelCase names
  are unchanged, so existing code keeps working.
* The tests of the Shiny tools' servers are skipped on CRAN, to keep the
  check short; they run on continuous integration.

See NEWS for details.

## Test environments

* local Windows 11 x64, R 4.6.2 (ucrt), R CMD check --as-cran
* the test suite, including the browser tests of the six Shiny tools, also
  run with TZ set to UTC, America/New_York, Australia/Sydney and Asia/Nicosia
* win-builder: R-devel (2026-11-06 r90845 ucrt) and R-release (4.6.2)
* GitHub Actions: macOS (release), Windows (release), Ubuntu (devel, release
  and oldrel-1)

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Possibly misspelled words in DESCRIPTION: Benktander, Bernegger,
  Bornhuetter, MBBEFD, Merz, Wuthrich.

  These are the surnames of the authors of the reserving and pricing methods
  (Benktander, Bernegger, Bornhuetter and Ferguson, Merz and Wuthrich), and
  MBBEFD is the name of Bernegger's class of distributions, all spelled
  correctly.

The references in the Description field point to articles on
www.theactuary.com. The site answers automated requests with HTTP 403 (a
Cloudflare browser check), so the URL check may flag them; the links open
normally in a web browser.

## Reverse dependencies

There are no reverse dependencies on CRAN.
