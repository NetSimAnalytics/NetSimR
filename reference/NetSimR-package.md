# NetSimR: Non-Life Actuarial Pricing, Reserving, Reinsurance and Capital

The NetSimR package provides eight categories of functions:

1.  Capped means, Exposure and ILF curves from various severity
    distributions, and the special functions behind them

2.  Pure IBNR and UPR earned periods

3.  Sliced distributions

4.  Frequency-severity claims simulation, applying a reinsurance layer
    to simulated claims, and portfolios of lines joined by a copula

5.  Claims reserving: chain ladder with Mack's standard errors,
    Bornhuetter-Ferguson, Benktander, Cape Cod, a bootstrap, Clark's
    growth curves and a chain ladder for claims inflation; IFRS 17
    discounting and risk adjustment, and the Solvency II standard
    formula's premium and reserve risk

6.  Pricing models: a Tweedie family for generalised linear models, and
    rate reviews: on-level premiums, the indicated rate change and
    Buhlmann-Straub credibility

7.  Excess of loss pricing: burning cost, Pareto rating, exposure rating
    with MBBEFD curves, and the technical premium

8.  Shiny apps for simulating claims, fitting distributions, fitting
    GLMs, reserving, reinsurance pricing and portfolio capital

## NetSimR special functions

[`IGamma`](https://netsimanalytics.github.io/NetSimR/reference/IGamma.md)
[`erf`](https://netsimanalytics.github.io/NetSimR/reference/erf.md)

## NetSimR mean functions

[`SlicedGammaParetoMean`](https://netsimanalytics.github.io/NetSimR/reference/SlicedGammaParetoMean.md)
[`SlicedLNormParetoMean`](https://netsimanalytics.github.io/NetSimR/reference/SlicedLNormParetoMean.md)

## NetSimR capped mean functions

[`GammaCappedMean`](https://netsimanalytics.github.io/NetSimR/reference/GammaCappedMean.md)
[`LNormCappedMean`](https://netsimanalytics.github.io/NetSimR/reference/LNormCappedMean.md)
[`ParetoCappedMean`](https://netsimanalytics.github.io/NetSimR/reference/ParetoCappedMean.md)
[`ParetoCappedMeanCalc`](https://netsimanalytics.github.io/NetSimR/reference/ParetoCappedMeanCalc.md)
[`SlicedGammaParetoCappedMean`](https://netsimanalytics.github.io/NetSimR/reference/SlicedGammaParetoCappedMean.md)
[`SlicedLNormParetoCappedMean`](https://netsimanalytics.github.io/NetSimR/reference/SlicedLNormParetoCappedMean.md)

## NetSimR exposure curve functions

[`ExposureCurveGamma`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveGamma.md)
[`ExposureCurveLNorm`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveLNorm.md)
[`ExposureCurvePareto`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurvePareto.md)
[`ExposureCurveSlicedGammaPareto`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveSlicedGammaPareto.md)
[`ExposureCurveSlicedLNormPareto`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveSlicedLNormPareto.md)

## NetSimR ILF curve functions

[`ILFGamma`](https://netsimanalytics.github.io/NetSimR/reference/ILFGamma.md)
[`ILFLNorm`](https://netsimanalytics.github.io/NetSimR/reference/ILFLNorm.md)
[`ILFPareto`](https://netsimanalytics.github.io/NetSimR/reference/ILFPareto.md)
[`ILFSlicedGammaPareto`](https://netsimanalytics.github.io/NetSimR/reference/ILFSlicedGammaPareto.md)
[`ILFSlicedLNormPareto`](https://netsimanalytics.github.io/NetSimR/reference/ILFSlicedLNormPareto.md)

## NetSimR pure IBNR functions

[`PureIBNRGamma`](https://netsimanalytics.github.io/NetSimR/reference/PureIBNRGamma.md)
[`PureIBNRLNorm`](https://netsimanalytics.github.io/NetSimR/reference/PureIBNRLNorm.md)

## NetSimR Sliced distribution functions

[`dSlicedGammaPareto`](https://netsimanalytics.github.io/NetSimR/reference/dSlicedGammaPareto.md)
[`dSlicedLNormPareto`](https://netsimanalytics.github.io/NetSimR/reference/dSlicedLNormPareto.md)
[`pSlicedGammaPareto`](https://netsimanalytics.github.io/NetSimR/reference/pSlicedGammaPareto.md)
[`pSlicedLNormPareto`](https://netsimanalytics.github.io/NetSimR/reference/pSlicedLNormPareto.md)
[`qSlicedGammaPareto`](https://netsimanalytics.github.io/NetSimR/reference/qSlicedGammaPareto.md)
[`qSlicedLNormPareto`](https://netsimanalytics.github.io/NetSimR/reference/qSlicedLNormPareto.md)

## NetSimR claims simulation functions

[`simulate_claims`](https://netsimanalytics.github.io/NetSimR/reference/simulate_claims.md)
[`simulate_function`](https://netsimanalytics.github.io/NetSimR/reference/simulate_function.md)
[`simulate_portfolio`](https://netsimanalytics.github.io/NetSimR/reference/simulate_portfolio.md)
[`apply_deductible_limit`](https://netsimanalytics.github.io/NetSimR/reference/apply_deductible_limit.md)

## NetSimR reserving functions

[`chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md)
[`bornhuetter_ferguson`](https://netsimanalytics.github.io/NetSimR/reference/bornhuetter_ferguson.md)
[`cape_cod`](https://netsimanalytics.github.io/NetSimR/reference/cape_cod.md)
[`bootstrap_chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/bootstrap_chain_ladder.md)
[`merz_wuthrich`](https://netsimanalytics.github.io/NetSimR/reference/merz_wuthrich.md)
[`munich_chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/munich_chain_ladder.md)
[`actual_vs_expected`](https://netsimanalytics.github.io/NetSimR/reference/actual_vs_expected.md)
[`clark_growth`](https://netsimanalytics.github.io/NetSimR/reference/clark_growth.md)
[`inflation_adjusted_chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/inflation_adjusted_chain_ladder.md)
[`as_triangle`](https://netsimanalytics.github.io/NetSimR/reference/as_triangle.md)

## NetSimR pricing model functions

[`tweedie_family`](https://netsimanalytics.github.io/NetSimR/reference/tweedie_family.md)

## NetSimR reinsurance pricing functions

[`burning_cost`](https://netsimanalytics.github.io/NetSimR/reference/burning_cost.md)
[`pareto_rating`](https://netsimanalytics.github.io/NetSimR/reference/pareto_rating.md)
[`property_exposure_rating`](https://netsimanalytics.github.io/NetSimR/reference/property_exposure_rating.md)
[`casualty_exposure_rating`](https://netsimanalytics.github.io/NetSimR/reference/property_exposure_rating.md)
[`mbbefd_curve`](https://netsimanalytics.github.io/NetSimR/reference/mbbefd_curve.md)
[`technical_premium`](https://netsimanalytics.github.io/NetSimR/reference/technical_premium.md)
[`credibility_weight`](https://netsimanalytics.github.io/NetSimR/reference/credibility_weight.md)

## Function names

The older functions have CamelCase names (`ParetoCappedMean`); each also
has a snake_case name, the same function, as the newer functions are
named (`pareto_capped_mean`). Both work.

## NetSimR Shiny apps

[`run_shiny_simulator`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_simulator.md)
[`run_shiny_distribution_fitting_tool`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_distribution_fitting_tool.md)
[`run_shiny_glm_fitting_tool`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_glm_fitting_tool.md)
[`run_shiny_reserving_tool`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_reserving_tool.md)
[`run_shiny_reinsurance_pricing_tool`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_reinsurance_pricing_tool.md)
[`run_shiny_portfolio_tool`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_portfolio_tool.md)

## See also

Useful links:

- <https://netsimanalytics.github.io/NetSimR/>

- <https://github.com/NetSimAnalytics/NetSimR>

- Report bugs at <https://github.com/NetSimAnalytics/NetSimR/issues>

## Author

**Maintainer**: Yiannis Parizas <yiannis.parizas@gmail.com>
([ORCID](https://orcid.org/0009-0000-0523-0751))

Authors:

- Yiannis Parizas <yiannis.parizas@gmail.com>
  ([ORCID](https://orcid.org/0009-0000-0523-0751))

Other contributors:

- The Inter Project Authors (The Inter font, in inst/fonts) \[copyright
  holder\]

- The JetBrains Mono Project Authors (The JetBrains Mono font, in
  inst/fonts) \[copyright holder\]
