## Release summary

NetSimR 0.2.1 is a bug-fix release that follows 0.2.0 (published
2026-09-13) quickly because a second round of testing found functions that
return wrong results without an error:

* PureIBNRGamma() and PureIBNRLNorm() reported the whole period as unearned
  when Date and POSIXct dates were mixed, and their results depended on the
  time zone of the machine (the documented example differed in the second
  decimal between time zones).
* simulate_function() rounded its results to two decimals, which distorted
  results for amounts in thousands or millions.
* The analytic functions returned plausible-looking numbers (e.g. negative
  probabilities) for invalid parameters; they now stop with a clear error.
* The distribution fitting tool gave a wrong Gamma fit for claims of 1e9 or
  more and misread numbers with a decimal comma and thousands separators.

It also applies the market convention for an aggregate deductible combined
with an each-and-every-loss layer. See NEWS for details.

## Test environments

* local Windows 11 x64, R 4.6.0 (ucrt), R CMD check --as-cran
* the test suite also run with TZ set to UTC, America/New_York,
  Australia/Sydney and Asia/Nicosia

## R CMD check results

0 errors | 0 warnings | 2 notes

* "Days since last update: 1". This release fixes functions that returned
  wrong results without an error (see the release summary above), so we
  would rather not wait before publishing it.
* The other note is local only: "Files 'README.md' or 'NEWS.md' cannot be
  checked without 'pandoc' being installed." pandoc is not on the PATH of the
  test machine.

The references in the Description field point to articles on
www.theactuary.com. The site answers automated requests with HTTP 403 (a
Cloudflare browser check), so the URL check may flag them; the links open
normally in a web browser.

## Reverse dependencies

There are no reverse dependencies on CRAN.
