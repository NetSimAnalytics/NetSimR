# Package index

## Shiny tools

Applications that run the package’s models without writing code.

- [`run_shiny_simulator()`](https://netsimr.netsimanalytics.com/reference/run_shiny_simulator.md)
  : Run the claims simulator application
- [`run_shiny_distribution_fitting_tool()`](https://netsimr.netsimanalytics.com/reference/run_shiny_distribution_fitting_tool.md)
  : Run the distribution fitting application
- [`run_shiny_glm_fitting_tool()`](https://netsimr.netsimanalytics.com/reference/run_shiny_glm_fitting_tool.md)
  : Run the generalised linear model fitting application
- [`run_shiny_reserving_tool()`](https://netsimr.netsimanalytics.com/reference/run_shiny_reserving_tool.md)
  : Run the claims reserving application
- [`run_shiny_reinsurance_pricing_tool()`](https://netsimr.netsimanalytics.com/reference/run_shiny_reinsurance_pricing_tool.md)
  : Run the reinsurance pricing application
- [`run_shiny_portfolio_tool()`](https://netsimr.netsimanalytics.com/reference/run_shiny_portfolio_tool.md)
  : Run the portfolio capital application

## Claims simulation and reinsurance

Frequency-severity simulation with Pareto tails, caps and layers, and
portfolios of lines joined by a copula.

- [`simulate_claims()`](https://netsimr.netsimanalytics.com/reference/simulate_claims.md)
  : Simulate claims with a frequency-severity model
- [`simulate_function()`](https://netsimr.netsimanalytics.com/reference/simulate_function.md)
  : Simulate insurance claims with reinsurance structures
- [`simulate_portfolio()`](https://netsimr.netsimanalytics.com/reference/simulate_portfolio.md)
  : Simulate a portfolio of dependent lines
- [`apply_deductible_limit()`](https://netsimr.netsimanalytics.com/reference/apply_deductible_limit.md)
  : Apply a deductible and limit to claims
- [`reinstatement_premium()`](https://netsimr.netsimanalytics.com/reference/reinstatement_premium.md)
  : Reinstatement premiums of an excess of loss layer
- [`compare_reinsurance()`](https://netsimr.netsimanalytics.com/reference/compare_reinsurance.md)
  : Compare reinsurance programmes

## Claims reserving

Chain ladder with Mack’s standard errors, methods with premiums, the
bootstrap, the one-year view, the Munich chain ladder, Clark’s growth
curves, inflation and actual vs expected.

- [`chain_ladder()`](https://netsimr.netsimanalytics.com/reference/chain_ladder.md)
  : Chain ladder reserves with Mack's standard errors
- [`bornhuetter_ferguson()`](https://netsimr.netsimanalytics.com/reference/bornhuetter_ferguson.md)
  : Bornhuetter-Ferguson and Benktander reserves
- [`cape_cod()`](https://netsimr.netsimanalytics.com/reference/cape_cod.md)
  : Cape Cod reserves
- [`bootstrap_chain_ladder()`](https://netsimr.netsimanalytics.com/reference/bootstrap_chain_ladder.md)
  : Bootstrap of the chain ladder reserves
- [`merz_wuthrich()`](https://netsimr.netsimanalytics.com/reference/merz_wuthrich.md)
  : The one-year view of the reserves: Merz and Wuthrich's claims
  development result
- [`munich_chain_ladder()`](https://netsimr.netsimanalytics.com/reference/munich_chain_ladder.md)
  : Munich chain ladder of paid and incurred claims
- [`actual_vs_expected()`](https://netsimr.netsimanalytics.com/reference/actual_vs_expected.md)
  : Actual against expected development
- [`clark_growth()`](https://netsimr.netsimanalytics.com/reference/clark_growth.md)
  : Clark's growth curve reserves
- [`inflation_adjusted_chain_ladder()`](https://netsimr.netsimanalytics.com/reference/inflation_adjusted_chain_ladder.md)
  : Chain ladder for claims inflation
- [`as_triangle()`](https://netsimr.netsimanalytics.com/reference/as_triangle.md)
  : Build a claims triangle from a table

## IFRS 17 and Solvency II

Discounted reserves, the risk adjustment, and the standard formula’s
non-life premium and reserve risk.

- [`discount_reserves()`](https://netsimr.netsimanalytics.com/reference/discount_reserves.md)
  : Discounted reserves from the chain ladder's payment pattern
- [`risk_adjustment()`](https://netsimr.netsimanalytics.com/reference/risk_adjustment.md)
  : Risk adjustment for non-financial risk
- [`sii_premium_reserve_risk()`](https://netsimr.netsimanalytics.com/reference/sii_premium_reserve_risk.md)
  : Solvency II standard formula: non-life premium and reserve risk
- [`sii_nonlife_segments`](https://netsimr.netsimanalytics.com/reference/sii_nonlife_segments.md)
  [`sii_nonlife_correlation`](https://netsimr.netsimanalytics.com/reference/sii_nonlife_segments.md)
  : The Solvency II standard formula's non-life segments

## Excess of loss pricing

Burning cost, Pareto rating, exposure rating with MBBEFD curves or a
claim size distribution, and the technical premium.

- [`burning_cost()`](https://netsimr.netsimanalytics.com/reference/burning_cost.md)
  : Burning cost of an excess of loss layer
- [`pareto_rating()`](https://netsimr.netsimanalytics.com/reference/pareto_rating.md)
  : Pareto rating of an excess of loss layer
- [`property_exposure_rating()`](https://netsimr.netsimanalytics.com/reference/property_exposure_rating.md)
  [`casualty_exposure_rating()`](https://netsimr.netsimanalytics.com/reference/property_exposure_rating.md)
  : Exposure rating of property and casualty excess of loss layers
- [`mbbefd_curve()`](https://netsimr.netsimanalytics.com/reference/mbbefd_curve.md)
  [`swiss_re_parameters()`](https://netsimr.netsimanalytics.com/reference/mbbefd_curve.md)
  [`swiss_re_curve()`](https://netsimr.netsimanalytics.com/reference/mbbefd_curve.md)
  : MBBEFD exposure curves
- [`technical_premium()`](https://netsimr.netsimanalytics.com/reference/technical_premium.md)
  : Technical premium of an excess of loss layer
- [`credibility_weight()`](https://netsimr.netsimanalytics.com/reference/credibility_weight.md)
  : Credibility of the loss experience

## Pricing models and rate reviews

A Tweedie family for generalised linear models of the loss cost,
on-level premiums, the indicated rate change and Buhlmann-Straub
credibility.

- [`tweedie_family()`](https://netsimr.netsimanalytics.com/reference/tweedie_family.md)
  : Tweedie family for generalised linear models
- [`on_level_premium()`](https://netsimr.netsimanalytics.com/reference/on_level_premium.md)
  : Premiums at today's rates: the parallelogram method
- [`indicated_rate_change()`](https://netsimr.netsimanalytics.com/reference/indicated_rate_change.md)
  : The indicated rate change: the loss ratio method
- [`buhlmann_straub()`](https://netsimr.netsimanalytics.com/reference/buhlmann_straub.md)
  : Buhlmann-Straub credibility

## Capped means

- [`GammaCappedMean()`](https://netsimr.netsimanalytics.com/reference/GammaCappedMean.md)
  [`gamma_capped_mean()`](https://netsimr.netsimanalytics.com/reference/GammaCappedMean.md)
  : Gamma capped mean
- [`LNormCappedMean()`](https://netsimr.netsimanalytics.com/reference/LNormCappedMean.md)
  [`lnorm_capped_mean()`](https://netsimr.netsimanalytics.com/reference/LNormCappedMean.md)
  : Lognormal capped mean
- [`ParetoCappedMean()`](https://netsimr.netsimanalytics.com/reference/ParetoCappedMean.md)
  [`pareto_capped_mean()`](https://netsimr.netsimanalytics.com/reference/ParetoCappedMean.md)
  : Pareto capped mean
- [`ParetoCappedMeanCalc()`](https://netsimr.netsimanalytics.com/reference/ParetoCappedMeanCalc.md)
  [`pareto_capped_mean_calc()`](https://netsimr.netsimanalytics.com/reference/ParetoCappedMeanCalc.md)
  : Pareto capped mean intermediary calculation
- [`SlicedGammaParetoCappedMean()`](https://netsimr.netsimanalytics.com/reference/SlicedGammaParetoCappedMean.md)
  [`sliced_gamma_pareto_capped_mean()`](https://netsimr.netsimanalytics.com/reference/SlicedGammaParetoCappedMean.md)
  : Sliced Gamma Pareto capped mean
- [`SlicedLNormParetoCappedMean()`](https://netsimr.netsimanalytics.com/reference/SlicedLNormParetoCappedMean.md)
  [`sliced_lnorm_pareto_capped_mean()`](https://netsimr.netsimanalytics.com/reference/SlicedLNormParetoCappedMean.md)
  : Sliced LogNormal Pareto capped mean

## Exposure curves and increased limit factors

- [`ExposureCurveGamma()`](https://netsimr.netsimanalytics.com/reference/ExposureCurveGamma.md)
  [`exposure_curve_gamma()`](https://netsimr.netsimanalytics.com/reference/ExposureCurveGamma.md)
  : Exposure Curve from a Gamma severity distribution
- [`ExposureCurveLNorm()`](https://netsimr.netsimanalytics.com/reference/ExposureCurveLNorm.md)
  [`exposure_curve_lnorm()`](https://netsimr.netsimanalytics.com/reference/ExposureCurveLNorm.md)
  : Exposure Curve from a LogNormal severity distribution
- [`ExposureCurvePareto()`](https://netsimr.netsimanalytics.com/reference/ExposureCurvePareto.md)
  [`exposure_curve_pareto()`](https://netsimr.netsimanalytics.com/reference/ExposureCurvePareto.md)
  : Exposure Curve from a Pareto severity distribution
- [`ExposureCurveSlicedGammaPareto()`](https://netsimr.netsimanalytics.com/reference/ExposureCurveSlicedGammaPareto.md)
  [`exposure_curve_sliced_gamma_pareto()`](https://netsimr.netsimanalytics.com/reference/ExposureCurveSlicedGammaPareto.md)
  : Exposure Curve from a Sliced Gamma Pareto severity distribution
- [`ExposureCurveSlicedLNormPareto()`](https://netsimr.netsimanalytics.com/reference/ExposureCurveSlicedLNormPareto.md)
  [`exposure_curve_sliced_lnorm_pareto()`](https://netsimr.netsimanalytics.com/reference/ExposureCurveSlicedLNormPareto.md)
  : Exposure Curve from a Sliced LogNormal Pareto severity distribution
- [`ILFGamma()`](https://netsimr.netsimanalytics.com/reference/ILFGamma.md)
  [`ilf_gamma()`](https://netsimr.netsimanalytics.com/reference/ILFGamma.md)
  : Increased Limit Factor Curve from a Gamma severity distribution
- [`ILFLNorm()`](https://netsimr.netsimanalytics.com/reference/ILFLNorm.md)
  [`ilf_lnorm()`](https://netsimr.netsimanalytics.com/reference/ILFLNorm.md)
  : Increased Limit Factor Curve from a LogNormal severity distribution
- [`ILFPareto()`](https://netsimr.netsimanalytics.com/reference/ILFPareto.md)
  [`ilf_pareto()`](https://netsimr.netsimanalytics.com/reference/ILFPareto.md)
  : Increased Limit Factor Curve from a Pareto severity distribution
- [`ILFSlicedGammaPareto()`](https://netsimr.netsimanalytics.com/reference/ILFSlicedGammaPareto.md)
  [`ilf_sliced_gamma_pareto()`](https://netsimr.netsimanalytics.com/reference/ILFSlicedGammaPareto.md)
  : Increased Limit Factor Curve from a Sliced Gamma Pareto severity
  distribution
- [`ILFSlicedLNormPareto()`](https://netsimr.netsimanalytics.com/reference/ILFSlicedLNormPareto.md)
  [`ilf_sliced_lnorm_pareto()`](https://netsimr.netsimanalytics.com/reference/ILFSlicedLNormPareto.md)
  : Increased Limit Factor Curve from a Sliced LogNormal Pareto severity
  distribution

## Sliced distributions

LogNormal or Gamma bodies with a Pareto tail.

- [`SlicedGammaParetoMean()`](https://netsimr.netsimanalytics.com/reference/SlicedGammaParetoMean.md)
  [`sliced_gamma_pareto_mean()`](https://netsimr.netsimanalytics.com/reference/SlicedGammaParetoMean.md)
  : Sliced Gamma Pareto mean
- [`SlicedLNormParetoMean()`](https://netsimr.netsimanalytics.com/reference/SlicedLNormParetoMean.md)
  [`sliced_lnorm_pareto_mean()`](https://netsimr.netsimanalytics.com/reference/SlicedLNormParetoMean.md)
  : Sliced LogNormal Pareto mean
- [`dSlicedGammaPareto()`](https://netsimr.netsimanalytics.com/reference/dSlicedGammaPareto.md)
  [`dsliced_gamma_pareto()`](https://netsimr.netsimanalytics.com/reference/dSlicedGammaPareto.md)
  : The probability density function (pdf) of a Sliced Gamma Pareto
  severity distribution
- [`pSlicedGammaPareto()`](https://netsimr.netsimanalytics.com/reference/pSlicedGammaPareto.md)
  [`psliced_gamma_pareto()`](https://netsimr.netsimanalytics.com/reference/pSlicedGammaPareto.md)
  : The cumulative distribution function (cdf) of a Sliced Gamma-Pareto
  severity distribution
- [`qSlicedGammaPareto()`](https://netsimr.netsimanalytics.com/reference/qSlicedGammaPareto.md)
  [`qsliced_gamma_pareto()`](https://netsimr.netsimanalytics.com/reference/qSlicedGammaPareto.md)
  : The inverse cumulative distribution function of a Sliced Gamma
  Pareto severity distribution
- [`dSlicedLNormPareto()`](https://netsimr.netsimanalytics.com/reference/dSlicedLNormPareto.md)
  [`dsliced_lnorm_pareto()`](https://netsimr.netsimanalytics.com/reference/dSlicedLNormPareto.md)
  : The probability density function (pdf) of a Sliced LogNormal Pareto
  severity distribution
- [`pSlicedLNormPareto()`](https://netsimr.netsimanalytics.com/reference/pSlicedLNormPareto.md)
  [`psliced_lnorm_pareto()`](https://netsimr.netsimanalytics.com/reference/pSlicedLNormPareto.md)
  : The cumulative distribution function (cdf) of a Sliced LogNormal
  Pareto severity distribution
- [`qSlicedLNormPareto()`](https://netsimr.netsimanalytics.com/reference/qSlicedLNormPareto.md)
  [`qsliced_lnorm_pareto()`](https://netsimr.netsimanalytics.com/reference/qSlicedLNormPareto.md)
  : The inverse cumulative distribution function of a Sliced LogNormal
  Pareto severity distribution

## Pure IBNR and unearned premium

- [`PureIBNRGamma()`](https://netsimr.netsimanalytics.com/reference/PureIBNRGamma.md)
  [`pure_ibnr_gamma()`](https://netsimr.netsimanalytics.com/reference/PureIBNRGamma.md)
  : Pure IBNR exposure from a Gamma reporting delay distribution
- [`PureIBNRLNorm()`](https://netsimr.netsimanalytics.com/reference/PureIBNRLNorm.md)
  [`pure_ibnr_lnorm()`](https://netsimr.netsimanalytics.com/reference/PureIBNRLNorm.md)
  : Pure IBNR exposure from a LogNormal reporting delay distribution

## Special functions

- [`IGamma()`](https://netsimr.netsimanalytics.com/reference/IGamma.md)
  [`upper_incomplete_gamma()`](https://netsimr.netsimanalytics.com/reference/IGamma.md)
  : Upper incomplete gamma function
- [`erf()`](https://netsimr.netsimanalytics.com/reference/erf.md) :
  Error function

## Data sets

- [`raa_triangle`](https://netsimr.netsimanalytics.com/reference/raa_triangle.md)
  : RAA triangle of incurred claims
- [`taylor_ashe_triangle`](https://netsimr.netsimanalytics.com/reference/taylor_ashe_triangle.md)
  : Taylor and Ashe triangle of paid claims
- [`quarg_mack_paid`](https://netsimr.netsimanalytics.com/reference/quarg_mack.md)
  [`quarg_mack_incurred`](https://netsimr.netsimanalytics.com/reference/quarg_mack.md)
  : Paid and incurred triangles of the Munich chain ladder paper
- [`simulated_motor_claims`](https://netsimr.netsimanalytics.com/reference/simulated_motor_claims.md)
  : Simulated motor claims with premiums
- [`xl_large_losses`](https://netsimr.netsimanalytics.com/reference/xl_large_losses.md)
  : Simulated large losses of an excess of loss treaty
- [`xl_premium`](https://netsimr.netsimanalytics.com/reference/xl_premium.md)
  : Subject premiums of an excess of loss treaty
- [`xl_property_profile`](https://netsimr.netsimanalytics.com/reference/xl_property_profile.md)
  : Risk profile of a property portfolio
- [`xl_casualty_profile`](https://netsimr.netsimanalytics.com/reference/xl_casualty_profile.md)
  : Risk profile of a casualty portfolio
- [`motor_policies`](https://netsimr.netsimanalytics.com/reference/motor_policies.md)
  : Simulated motor policies
- [`liability_claims`](https://netsimr.netsimanalytics.com/reference/liability_claims.md)
  : Simulated liability claims at their policy limits
