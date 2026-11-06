# Package index

## Shiny tools

Applications that run the package’s models without writing code.

- [`run_shiny_simulator()`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_simulator.md)
  : A function to run the shiny simulator application
- [`run_shiny_distribution_fitting_tool()`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_distribution_fitting_tool.md)
  : Run the distribution fitting application
- [`run_shiny_glm_fitting_tool()`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_glm_fitting_tool.md)
  : Run the generalised linear model fitting application
- [`run_shiny_reserving_tool()`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_reserving_tool.md)
  : Run the claims reserving application

## Claims simulation and reinsurance

Frequency-severity simulation with Pareto tails, caps and layers.

- [`simulate_claims()`](https://netsimanalytics.github.io/NetSimR/reference/simulate_claims.md)
  : Simulate claims with a frequency-severity model
- [`simulate_function()`](https://netsimanalytics.github.io/NetSimR/reference/simulate_function.md)
  : Simulate insurance claims with reinsurance structures
- [`apply_deductible_limit()`](https://netsimanalytics.github.io/NetSimR/reference/apply_deductible_limit.md)
  : Apply a deductible and limit to claims
- [`reinstatement_premium()`](https://netsimanalytics.github.io/NetSimR/reference/reinstatement_premium.md)
  : Reinstatement premiums of an excess of loss layer

## Claims reserving

Chain ladder with Mack’s standard errors, methods with premiums, the
bootstrap, the one-year view and the Munich chain ladder.

- [`chain_ladder()`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md)
  : Chain ladder reserves with Mack's standard errors
- [`bornhuetter_ferguson()`](https://netsimanalytics.github.io/NetSimR/reference/bornhuetter_ferguson.md)
  : Bornhuetter-Ferguson and Benktander reserves
- [`cape_cod()`](https://netsimanalytics.github.io/NetSimR/reference/cape_cod.md)
  : Cape Cod reserves
- [`bootstrap_chain_ladder()`](https://netsimanalytics.github.io/NetSimR/reference/bootstrap_chain_ladder.md)
  : Bootstrap of the chain ladder reserves
- [`merz_wuthrich()`](https://netsimanalytics.github.io/NetSimR/reference/merz_wuthrich.md)
  : The one-year view of the reserves: Merz and Wuthrich's claims
  development result
- [`munich_chain_ladder()`](https://netsimanalytics.github.io/NetSimR/reference/munich_chain_ladder.md)
  : Munich chain ladder of paid and incurred claims
- [`as_triangle()`](https://netsimanalytics.github.io/NetSimR/reference/as_triangle.md)
  : Build a claims triangle from a table

## Capped means

- [`GammaCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/GammaCappedMean.md)
  [`gamma_capped_mean()`](https://netsimanalytics.github.io/NetSimR/reference/GammaCappedMean.md)
  : Gamma capped mean
- [`LNormCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/LNormCappedMean.md)
  [`lnorm_capped_mean()`](https://netsimanalytics.github.io/NetSimR/reference/LNormCappedMean.md)
  : Lognormal capped mean
- [`ParetoCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/ParetoCappedMean.md)
  [`pareto_capped_mean()`](https://netsimanalytics.github.io/NetSimR/reference/ParetoCappedMean.md)
  : Pareto capped mean
- [`ParetoCappedMeanCalc()`](https://netsimanalytics.github.io/NetSimR/reference/ParetoCappedMeanCalc.md)
  [`pareto_capped_mean_calc()`](https://netsimanalytics.github.io/NetSimR/reference/ParetoCappedMeanCalc.md)
  : Pareto capped mean intermediary calculation
- [`SlicedGammaParetoCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/SlicedGammaParetoCappedMean.md)
  [`sliced_gamma_pareto_capped_mean()`](https://netsimanalytics.github.io/NetSimR/reference/SlicedGammaParetoCappedMean.md)
  : Sliced Gamma Pareto capped mean
- [`SlicedLNormParetoCappedMean()`](https://netsimanalytics.github.io/NetSimR/reference/SlicedLNormParetoCappedMean.md)
  [`sliced_lnorm_pareto_capped_mean()`](https://netsimanalytics.github.io/NetSimR/reference/SlicedLNormParetoCappedMean.md)
  : Sliced LogNormal Pareto capped mean

## Exposure curves and increased limit factors

- [`ExposureCurveGamma()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveGamma.md)
  [`exposure_curve_gamma()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveGamma.md)
  : Exposure Curve from a Gamma severity distribution
- [`ExposureCurveLNorm()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveLNorm.md)
  [`exposure_curve_lnorm()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveLNorm.md)
  : Exposure Curve from a LogNormal severity distribution
- [`ExposureCurvePareto()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurvePareto.md)
  [`exposure_curve_pareto()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurvePareto.md)
  : Exposure Curve from a Pareto severity distribution
- [`ExposureCurveSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveSlicedGammaPareto.md)
  [`exposure_curve_sliced_gamma_pareto()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveSlicedGammaPareto.md)
  : Exposure Curve from a Sliced Gamma Pareto severity distribution
- [`ExposureCurveSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveSlicedLNormPareto.md)
  [`exposure_curve_sliced_lnorm_pareto()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveSlicedLNormPareto.md)
  : Exposure Curve from a Sliced LogNormal Pareto severity distribution
- [`ILFGamma()`](https://netsimanalytics.github.io/NetSimR/reference/ILFGamma.md)
  [`ilf_gamma()`](https://netsimanalytics.github.io/NetSimR/reference/ILFGamma.md)
  : Increased Limit Factor Curve from a Gamma severity distribution
- [`ILFLNorm()`](https://netsimanalytics.github.io/NetSimR/reference/ILFLNorm.md)
  [`ilf_lnorm()`](https://netsimanalytics.github.io/NetSimR/reference/ILFLNorm.md)
  : Increased Limit Factor Curve from a LogNormal severity distribution
- [`ILFPareto()`](https://netsimanalytics.github.io/NetSimR/reference/ILFPareto.md)
  [`ilf_pareto()`](https://netsimanalytics.github.io/NetSimR/reference/ILFPareto.md)
  : Increased Limit Factor Curve from a Pareto severity distribution
- [`ILFSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/ILFSlicedGammaPareto.md)
  [`ilf_sliced_gamma_pareto()`](https://netsimanalytics.github.io/NetSimR/reference/ILFSlicedGammaPareto.md)
  : Increased Limit Factor Curve from a Sliced Gamma Pareto severity
  distribution
- [`ILFSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/ILFSlicedLNormPareto.md)
  [`ilf_sliced_lnorm_pareto()`](https://netsimanalytics.github.io/NetSimR/reference/ILFSlicedLNormPareto.md)
  : Increased Limit Factor Curve from a Sliced LogNormal Pareto severity
  distribution

## Sliced distributions

LogNormal or Gamma bodies with a Pareto tail.

- [`SlicedGammaParetoMean()`](https://netsimanalytics.github.io/NetSimR/reference/SlicedGammaParetoMean.md)
  [`sliced_gamma_pareto_mean()`](https://netsimanalytics.github.io/NetSimR/reference/SlicedGammaParetoMean.md)
  : Sliced Gamma Pareto mean
- [`SlicedLNormParetoMean()`](https://netsimanalytics.github.io/NetSimR/reference/SlicedLNormParetoMean.md)
  [`sliced_lnorm_pareto_mean()`](https://netsimanalytics.github.io/NetSimR/reference/SlicedLNormParetoMean.md)
  : Sliced LogNormal Pareto mean
- [`dSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/dSlicedGammaPareto.md)
  [`dsliced_gamma_pareto()`](https://netsimanalytics.github.io/NetSimR/reference/dSlicedGammaPareto.md)
  : The probability density function (pdf) of a Sliced Gamma Pareto
  severity distribution
- [`pSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/pSlicedGammaPareto.md)
  [`psliced_gamma_pareto()`](https://netsimanalytics.github.io/NetSimR/reference/pSlicedGammaPareto.md)
  : The cumulative distribution function (cdf) of a Sliced Gamma-Pareto
  severity distribution
- [`qSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/qSlicedGammaPareto.md)
  [`qsliced_gamma_pareto()`](https://netsimanalytics.github.io/NetSimR/reference/qSlicedGammaPareto.md)
  : The inverse cumulative distribution function of a Sliced Gamma
  Pareto severity distribution
- [`dSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/dSlicedLNormPareto.md)
  [`dsliced_lnorm_pareto()`](https://netsimanalytics.github.io/NetSimR/reference/dSlicedLNormPareto.md)
  : The probability density function (pdf) of a Sliced LogNormal Pareto
  severity distribution
- [`pSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/pSlicedLNormPareto.md)
  [`psliced_lnorm_pareto()`](https://netsimanalytics.github.io/NetSimR/reference/pSlicedLNormPareto.md)
  : The cumulative distribution function (cdf) of a Sliced LogNormal
  Pareto severity distribution
- [`qSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/qSlicedLNormPareto.md)
  [`qsliced_lnorm_pareto()`](https://netsimanalytics.github.io/NetSimR/reference/qSlicedLNormPareto.md)
  : The inverse cumulative distribution function of a Sliced LogNormal
  Pareto severity distribution

## Pure IBNR and unearned premium

- [`PureIBNRGamma()`](https://netsimanalytics.github.io/NetSimR/reference/PureIBNRGamma.md)
  [`pure_ibnr_gamma()`](https://netsimanalytics.github.io/NetSimR/reference/PureIBNRGamma.md)
  : Pure IBNR exposure from a Gamma reporting delay distribution
- [`PureIBNRLNorm()`](https://netsimanalytics.github.io/NetSimR/reference/PureIBNRLNorm.md)
  [`pure_ibnr_lnorm()`](https://netsimanalytics.github.io/NetSimR/reference/PureIBNRLNorm.md)
  : Pure IBNR exposure from a LogNormal reporting delay distribution

## Special functions

- [`IGamma()`](https://netsimanalytics.github.io/NetSimR/reference/IGamma.md)
  [`upper_incomplete_gamma()`](https://netsimanalytics.github.io/NetSimR/reference/IGamma.md)
  : Upper incomplete gamma function
- [`erf()`](https://netsimanalytics.github.io/NetSimR/reference/erf.md)
  : Error function

## Data sets

- [`raa_triangle`](https://netsimanalytics.github.io/NetSimR/reference/raa_triangle.md)
  : RAA triangle of incurred claims
- [`taylor_ashe_triangle`](https://netsimanalytics.github.io/NetSimR/reference/taylor_ashe_triangle.md)
  : Taylor and Ashe triangle of paid claims
- [`quarg_mack_paid`](https://netsimanalytics.github.io/NetSimR/reference/quarg_mack.md)
  [`quarg_mack_incurred`](https://netsimanalytics.github.io/NetSimR/reference/quarg_mack.md)
  : Paid and incurred triangles of the Munich chain ladder paper
- [`simulated_motor_claims`](https://netsimanalytics.github.io/NetSimR/reference/simulated_motor_claims.md)
  : Simulated motor claims with premiums
