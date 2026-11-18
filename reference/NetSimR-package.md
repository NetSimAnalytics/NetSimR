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

[`IGamma`](https://netsimr.netsimanalytics.com/reference/IGamma.md)
[`erf`](https://netsimr.netsimanalytics.com/reference/erf.md)

## NetSimR mean functions

[`SlicedGammaParetoMean`](https://netsimr.netsimanalytics.com/reference/SlicedGammaParetoMean.md)
[`SlicedLNormParetoMean`](https://netsimr.netsimanalytics.com/reference/SlicedLNormParetoMean.md)

## NetSimR capped mean functions

[`GammaCappedMean`](https://netsimr.netsimanalytics.com/reference/GammaCappedMean.md)
[`LNormCappedMean`](https://netsimr.netsimanalytics.com/reference/LNormCappedMean.md)
[`ParetoCappedMean`](https://netsimr.netsimanalytics.com/reference/ParetoCappedMean.md)
[`ParetoCappedMeanCalc`](https://netsimr.netsimanalytics.com/reference/ParetoCappedMeanCalc.md)
[`SlicedGammaParetoCappedMean`](https://netsimr.netsimanalytics.com/reference/SlicedGammaParetoCappedMean.md)
[`SlicedLNormParetoCappedMean`](https://netsimr.netsimanalytics.com/reference/SlicedLNormParetoCappedMean.md)

## NetSimR exposure curve functions

[`ExposureCurveGamma`](https://netsimr.netsimanalytics.com/reference/ExposureCurveGamma.md)
[`ExposureCurveLNorm`](https://netsimr.netsimanalytics.com/reference/ExposureCurveLNorm.md)
[`ExposureCurvePareto`](https://netsimr.netsimanalytics.com/reference/ExposureCurvePareto.md)
[`ExposureCurveSlicedGammaPareto`](https://netsimr.netsimanalytics.com/reference/ExposureCurveSlicedGammaPareto.md)
[`ExposureCurveSlicedLNormPareto`](https://netsimr.netsimanalytics.com/reference/ExposureCurveSlicedLNormPareto.md)

## NetSimR ILF curve functions

[`ILFGamma`](https://netsimr.netsimanalytics.com/reference/ILFGamma.md)
[`ILFLNorm`](https://netsimr.netsimanalytics.com/reference/ILFLNorm.md)
[`ILFPareto`](https://netsimr.netsimanalytics.com/reference/ILFPareto.md)
[`ILFSlicedGammaPareto`](https://netsimr.netsimanalytics.com/reference/ILFSlicedGammaPareto.md)
[`ILFSlicedLNormPareto`](https://netsimr.netsimanalytics.com/reference/ILFSlicedLNormPareto.md)

## NetSimR pure IBNR functions

[`PureIBNRGamma`](https://netsimr.netsimanalytics.com/reference/PureIBNRGamma.md)
[`PureIBNRLNorm`](https://netsimr.netsimanalytics.com/reference/PureIBNRLNorm.md)

## NetSimR Sliced distribution functions

[`dSlicedGammaPareto`](https://netsimr.netsimanalytics.com/reference/dSlicedGammaPareto.md)
[`dSlicedLNormPareto`](https://netsimr.netsimanalytics.com/reference/dSlicedLNormPareto.md)
[`pSlicedGammaPareto`](https://netsimr.netsimanalytics.com/reference/pSlicedGammaPareto.md)
[`pSlicedLNormPareto`](https://netsimr.netsimanalytics.com/reference/pSlicedLNormPareto.md)
[`qSlicedGammaPareto`](https://netsimr.netsimanalytics.com/reference/qSlicedGammaPareto.md)
[`qSlicedLNormPareto`](https://netsimr.netsimanalytics.com/reference/qSlicedLNormPareto.md)

## NetSimR claims simulation functions

[`simulate_claims`](https://netsimr.netsimanalytics.com/reference/simulate_claims.md)
[`simulate_function`](https://netsimr.netsimanalytics.com/reference/simulate_function.md)
[`simulate_portfolio`](https://netsimr.netsimanalytics.com/reference/simulate_portfolio.md)
[`apply_deductible_limit`](https://netsimr.netsimanalytics.com/reference/apply_deductible_limit.md)

## NetSimR reserving functions

[`chain_ladder`](https://netsimr.netsimanalytics.com/reference/chain_ladder.md)
[`bornhuetter_ferguson`](https://netsimr.netsimanalytics.com/reference/bornhuetter_ferguson.md)
[`cape_cod`](https://netsimr.netsimanalytics.com/reference/cape_cod.md)
[`bootstrap_chain_ladder`](https://netsimr.netsimanalytics.com/reference/bootstrap_chain_ladder.md)
[`merz_wuthrich`](https://netsimr.netsimanalytics.com/reference/merz_wuthrich.md)
[`munich_chain_ladder`](https://netsimr.netsimanalytics.com/reference/munich_chain_ladder.md)
[`actual_vs_expected`](https://netsimr.netsimanalytics.com/reference/actual_vs_expected.md)
[`clark_growth`](https://netsimr.netsimanalytics.com/reference/clark_growth.md)
[`inflation_adjusted_chain_ladder`](https://netsimr.netsimanalytics.com/reference/inflation_adjusted_chain_ladder.md)
[`as_triangle`](https://netsimr.netsimanalytics.com/reference/as_triangle.md)

## NetSimR pricing model functions

[`tweedie_family`](https://netsimr.netsimanalytics.com/reference/tweedie_family.md)

## NetSimR reinsurance pricing functions

[`burning_cost`](https://netsimr.netsimanalytics.com/reference/burning_cost.md)
[`pareto_rating`](https://netsimr.netsimanalytics.com/reference/pareto_rating.md)
[`property_exposure_rating`](https://netsimr.netsimanalytics.com/reference/property_exposure_rating.md)
[`casualty_exposure_rating`](https://netsimr.netsimanalytics.com/reference/property_exposure_rating.md)
[`mbbefd_curve`](https://netsimr.netsimanalytics.com/reference/mbbefd_curve.md)
[`technical_premium`](https://netsimr.netsimanalytics.com/reference/technical_premium.md)
[`credibility_weight`](https://netsimr.netsimanalytics.com/reference/credibility_weight.md)

## Function names

The older functions have CamelCase names (`ParetoCappedMean`); each also
has a snake_case name, the same function, as the newer functions are
named (`pareto_capped_mean`). Both work.

## NetSimR Shiny apps

[`run_shiny_simulator`](https://netsimr.netsimanalytics.com/reference/run_shiny_simulator.md)
[`run_shiny_distribution_fitting_tool`](https://netsimr.netsimanalytics.com/reference/run_shiny_distribution_fitting_tool.md)
[`run_shiny_glm_fitting_tool`](https://netsimr.netsimanalytics.com/reference/run_shiny_glm_fitting_tool.md)
[`run_shiny_reserving_tool`](https://netsimr.netsimanalytics.com/reference/run_shiny_reserving_tool.md)
[`run_shiny_reinsurance_pricing_tool`](https://netsimr.netsimanalytics.com/reference/run_shiny_reinsurance_pricing_tool.md)
[`run_shiny_portfolio_tool`](https://netsimr.netsimanalytics.com/reference/run_shiny_portfolio_tool.md)

## See also

Useful links:

- <https://netsimr.netsimanalytics.com/>

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
