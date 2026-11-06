# Pure IBNR exposure from a Gamma reporting delay distribution

Gives the unearned and pure IBNR exposure of each policy period at a
valuation date, from a Gamma reporting delay, as needed to reserve for
claims that have occurred but have not yet been reported.

## Usage

``` r
PureIBNRGamma(IncDate, ExpDate, ValDate, shape, rate)

pure_ibnr_gamma(IncDate, ExpDate, ValDate, shape, rate)
```

## Arguments

- IncDate:

  A `Date` or `POSIXct` vector - the inception dates of the periods.
  Numbers and character strings are not accepted; convert them with
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html) first.

- ExpDate:

  A `Date` or `POSIXct` vector - the expiry dates of the periods. Must
  not be before the inception dates.

- ValDate:

  A `Date` or `POSIXct` date - the valuation date.

- shape:

  A positive real number - the shape parameter of the reporting delay's
  Gamma distribution, with the delay measured in days.

- rate:

  A positive real number - the rate parameter (per day) of the reporting
  delay's Gamma distribution.

## Value

A data frame with the unearned and pure IBNR exposure of each period in
days (`UnearnedDuration`, and `PureIBNRDuration` rounded to 2 decimals)
and as proportions between 0 and 1 of the period's duration
(`UnearnedDurationRatio` and `PureIBNRDurationRatio`, rounded to 5
decimals), where the reporting delay has a Gamma distribution with
parameters `shape` and `rate`. The ratios are computed before the
durations are rounded. A period of zero length (`ExpDate` equal to
`IncDate`) gives ratios of 0. The dates and parameters are recycled to a
common length, one row each; lengths that do not recycle are an error.

## Details

Durations are counted in days on each date's own calendar and clock: a
`POSIXct` time of day counts as a fraction of a day, and daylight saving
changes do not add fractions of a day, so `Date` and `POSIXct` dates (or
a mix of them) give the same results.

## See also

Other pure IBNR functions:
[`PureIBNRLNorm()`](https://netsimanalytics.github.io/NetSimR/reference/PureIBNRLNorm.md)

## Examples

``` r
Dates = data.frame(
    inceptionDate = c("01/01/2006", "01/07/2006", "01/01/2007")
    ,expiryDate = c("31/12/2006", "30/06/2007", "31/12/2007")
)

Dates$inceptionDate<-as.POSIXct(Dates$inceptionDate, format="%d/%m/%Y")

Dates$expiryDate<-as.POSIXct(Dates$expiryDate, format="%d/%m/%Y")

ValuationDate<-as.POSIXct("30/10/2007", format="%d/%m/%Y")

PureIBNRGamma(Dates$inceptionDate,Dates$expiryDate,ValuationDate,7,0.15)
#>   UnearnedDuration PureIBNRDuration UnearnedDurationRatio PureIBNRDurationRatio
#> 1                0             0.00               0.00000               0.00000
#> 2                0             0.01               0.00000               0.00002
#> 3               62            46.67               0.17033               0.12821
```
