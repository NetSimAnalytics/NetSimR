## Release summary

NetSimR 0.3.3 fixes two of the package's three Shiny tools, which could not
start on any machine other than the one that built the package, and so did
not work for users installing a CRAN binary.

* run_shiny_simulator() and run_shiny_distribution_fitting_tool() stopped
  with "File to import not found or unreadable:
  D:\RCompile\CRANpkg\lib\4.6\bslib\lib\bs5\scss\_functions.scss" on a
  Windows machine that installed the CRAN binary. The two tools' pages were
  built when the package was installed, and their bslib theme kept absolute
  paths to Bootstrap's SCSS files on the build machine. The pages are now
  built when a tool starts, from the bslib installed where it runs, as the
  third tool's page already was.
* The checks could not show the problem, because they build and use the
  package on the same machine. A new test fails if any object of the package
  holds a path to bslib's SCSS files, and checks that each tool's page is
  built from the bslib of the session.

No function's results change. See NEWS for details.

## Test environments

* local Windows 11 x64, R 4.6.1 (ucrt), R CMD check --as-cran
* the package installed against one copy of bslib and used with another,
  after that copy was removed: the two tools failed before the fix and work
  after it
* the test suite also run with TZ set to UTC, America/New_York,
  Australia/Sydney and Asia/Nicosia
* GitHub Actions: macOS (release), Windows (release), Ubuntu (devel, release
  and oldrel-1)

## R CMD check results

0 errors | 0 warnings | 0 notes

The references in the Description field point to articles on
www.theactuary.com. The site answers automated requests with HTTP 403 (a
Cloudflare browser check), so the URL check may flag them; the links open
normally in a web browser.

## Reverse dependencies

There are no reverse dependencies on CRAN.
