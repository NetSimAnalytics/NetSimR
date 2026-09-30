## Release summary

NetSimR 0.3.2 fixes the test failure of the "Additional issues" noLD check
reported for 0.3.1, ahead of the 2026-10-20 deadline. Nothing else changes:
no function, no Shiny tool and no file format is touched.

* tests/testthat/test-settings-file.R asked for a double to come back from
  the text of a settings file bit for bit. The file names a double with 17
  significant digits, which is exact, but a platform built without long
  double reads such text back up to a bit out (the noLD check reported
  0.333333333333333259 for a third, rather than 0.333333333333333315), and
  no number of decimal digits can mend that. The test now allows that
  difference where capabilities("long.double") is FALSE, and still asks for
  an exact value on every other platform.

The settings files written by this version are identical to those of 0.3.1.

See NEWS for details.

## Test environments

* local Windows 11 x64, R 4.6.0 (ucrt), R CMD check --as-cran
* the test suite also run with TZ set to UTC, America/New_York,
  Australia/Sydney and Asia/Nicosia
* GitHub Actions: macOS (release), Windows (release), Ubuntu (devel, release
  and oldrel-1)

## R CMD check results

0 errors | 0 warnings | 1 note

* "Days since last update": this release only fixes the noLD test failure
  reported for 0.3.1, within the date CRAN gave.

The references in the Description field point to articles on
www.theactuary.com. The site answers automated requests with HTTP 403 (a
Cloudflare browser check), so the URL check may flag them; the links open
normally in a web browser.

## Reverse dependencies

There are no reverse dependencies on CRAN.
