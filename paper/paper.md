---
title: 'NetSimR: actuarial methods and browser-based tools for non-life insurance pricing, reserving and capital'
tags:
  - R
  - actuarial science
  - insurance
  - reinsurance
  - claims reserving
  - Solvency II
  - IFRS 17
  - Shiny
authors:
  - name: Yiannis Parizas
    orcid: 0009-0000-0523-0751
    affiliation: 1
affiliations:
  # TO DO: the affiliation to show (an employer, or "Independent researcher")
  - name: Independent researcher, Cyprus
    index: 1
date: 8 November 2026
bibliography: paper.bib
---

# Summary

Non-life (general) insurers and reinsurers price policies, set aside reserves for the
claims they have not yet paid, buy reinsurance and hold capital against bad years. Each of
these tasks has standard actuarial methods, and most actuaries apply them in spreadsheets.
NetSimR is an R package that implements these methods, validated against established
packages and the regulations they follow, and puts them in six Shiny [@shiny] tools that run
on an actuary's computer, on a server, or in the web browser with webR, with nothing to
install:

- **Pricing**: generalised linear models with a Tweedie family, splines, held-out
  validation (Gini coefficient, lift and double lift charts), stepwise and penalised
  variable selection, and a rate table from the effects of the model's terms that a rating
  engine can load; and rate reviews by the loss ratio method, with on-levelling by the
  parallelogram method, trend and Buhlmann-Straub credibility [@buhlmann1970].
- **Claim distributions**: frequency and severity fits, including claims reported only
  above a threshold and claims censored at their policy limits.
- **Simulation and capital**: frequency-severity simulation with Pareto tails and
  reinsurance layers, run in parallel when that is faster; portfolios of lines joined by a
  Gaussian or t copula through rank reordering [@iman1982], with value at risk, tail value
  at risk and an Euler (co-TVaR) allocation of capital; and the Solvency II standard
  formula for non-life premium and reserve risk [@eu2015].
- **Reinsurance**: excess of loss pricing by the burning cost, a Pareto model of the large
  losses and exposure rating with MBBEFD curves [@bernegger1997]; and the comparison of
  reinsurance programmes by their cost and the capital they save.
- **Reserving**: the chain ladder with Mack's standard errors [@mack1993; @mack1999],
  Bornhuetter-Ferguson, Benktander and Cape Cod, the over-dispersed Poisson bootstrap
  [@england2002], the one-year view [@merz2008], the Munich chain ladder [@quarg2004],
  Clark's growth curves [@clark2003], an inflation-adjusted chain ladder, and for IFRS 17
  the discounting of the reserves and a risk adjustment by percentile or cost of capital.

The tools produce HTML reports for committees, auditors and regulators, read CSV and Excel
files, and save their settings to text files that colleagues can share.

# Statement of need

R has strong packages for parts of this work: ChainLadder [@chainladder] for reserving,
actuar [@actuar] for loss distributions and credibility, and others for GLMs and
simulation. They serve actuaries who write R. Most practising actuaries do not, and the
methods reach them through spreadsheets, which are hard to validate and to audit, or
through commercial software. NetSimR aims to close that gap in two ways. First, it covers
the chain of work of a non-life actuary, from the pricing of policies to the capital and
the reserves, with consistent inputs and outputs, so that one portfolio can be followed
through all of it (the package's vignette "From pricing to capital and reserves" does
this). Second, every method is available both as a function, for analysts who script, and
in a tool that needs no programming, which the package's website runs in the browser.

The results are tested against established implementations, so that the tools can be
trusted for real work: the reserving methods against ChainLadder, the censored severity
fits against the survival package, the credibility estimates against actuar, the Tweedie
family against statmod, the exposure curves against mbbefd, and the Solvency II parameters
against Delegated Regulation (EU) 2015/35 as amended in 2019. The test suite of over five
thousand expectations, including browser tests of the tools, runs on Windows, macOS and
Linux.

# Acknowledgements

<!-- TO DO: acknowledgements, if any. JOSS asks authors to disclose the use of generative AI:
     NetSimR's development since 2026 was assisted by Claude Code (Anthropic), with every method
     reviewed and validated as described above. Adjust this statement as you see fit. -->
The development of NetSimR was assisted by Claude Code (Anthropic). Every method was
reviewed by the author and validated against the implementations and sources cited.

# References
