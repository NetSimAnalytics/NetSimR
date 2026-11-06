# NetSimR: Actuarial Functions for Non-Life Insurance Modelling

The NetSimR package provides six categories of functions:

1.  Capped means, Exposure and ILF curves from various severity
    distributions, and the special functions behind them

2.  Pure IBNR and UPR earned periods

3.  Sliced distributions

4.  Frequency-severity claims simulation, and applying a reinsurance
    layer to simulated claims

5.  Claims reserving: chain ladder with Mack's standard errors,
    Bornhuetter-Ferguson, Benktander, Cape Cod and a bootstrap

6.  Shiny apps for simulating claims, fitting distributions, fitting
    GLMs and reserving

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
[`apply_deductible_limit`](https://netsimanalytics.github.io/NetSimR/reference/apply_deductible_limit.md)

## NetSimR reserving functions

[`chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md)
[`bornhuetter_ferguson`](https://netsimanalytics.github.io/NetSimR/reference/bornhuetter_ferguson.md)
[`cape_cod`](https://netsimanalytics.github.io/NetSimR/reference/cape_cod.md)
[`bootstrap_chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/bootstrap_chain_ladder.md)
[`merz_wuthrich`](https://netsimanalytics.github.io/NetSimR/reference/merz_wuthrich.md)
[`munich_chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/munich_chain_ladder.md)
[`as_triangle`](https://netsimanalytics.github.io/NetSimR/reference/as_triangle.md)

## Function names

The older functions have CamelCase names (`ParetoCappedMean`); each also
has a snake_case name, the same function, as the newer functions are
named (`pareto_capped_mean`). Both work.

## NetSimR Shiny apps

[`run_shiny_simulator`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_simulator.md)
[`run_shiny_distribution_fitting_tool`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_distribution_fitting_tool.md)
[`run_shiny_glm_fitting_tool`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_glm_fitting_tool.md)
[`run_shiny_reserving_tool`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_reserving_tool.md)

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
