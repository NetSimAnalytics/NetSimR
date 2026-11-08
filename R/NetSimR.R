#' @description The NetSimR package provides eight categories of functions:
#' \enumerate{
#' \item Capped means, Exposure and ILF curves from various severity distributions, and the special functions behind them
#' \item Pure IBNR and UPR earned periods
#' \item Sliced distributions
#' \item Frequency-severity claims simulation, applying a reinsurance layer to simulated claims, and portfolios of lines joined by a copula
#' \item Claims reserving: chain ladder with Mack's standard errors, Bornhuetter-Ferguson, Benktander, Cape Cod, a bootstrap, Clark's growth curves and a chain ladder for claims inflation; IFRS 17 discounting and risk adjustment, and the Solvency II standard formula's premium and reserve risk
#' \item Pricing models: a Tweedie family for generalised linear models, and rate reviews: on-level premiums, the indicated rate change and Buhlmann-Straub credibility
#' \item Excess of loss pricing: burning cost, Pareto rating, exposure rating with MBBEFD curves, and the technical premium
#' \item Shiny apps for simulating claims, fitting distributions, fitting GLMs, reserving, reinsurance pricing and portfolio capital
#' }
#'
#' @section NetSimR special functions:
#' \code{\link{IGamma}}
#' \code{\link{erf}}
#'
#' @section NetSimR mean functions:
#' \code{\link{SlicedGammaParetoMean}}
#' \code{\link{SlicedLNormParetoMean}}
#'
#' @section NetSimR capped mean functions:
#' \code{\link{GammaCappedMean}}
#' \code{\link{LNormCappedMean}}
#' \code{\link{ParetoCappedMean}}
#' \code{\link{ParetoCappedMeanCalc}}
#' \code{\link{SlicedGammaParetoCappedMean}}
#' \code{\link{SlicedLNormParetoCappedMean}}
#'
#' @section NetSimR exposure curve functions:
#' \code{\link{ExposureCurveGamma}}
#' \code{\link{ExposureCurveLNorm}}
#' \code{\link{ExposureCurvePareto}}
#' \code{\link{ExposureCurveSlicedGammaPareto}}
#' \code{\link{ExposureCurveSlicedLNormPareto}}
#'
#' @section NetSimR ILF curve functions:
#' \code{\link{ILFGamma}}
#' \code{\link{ILFLNorm}}
#' \code{\link{ILFPareto}}
#' \code{\link{ILFSlicedGammaPareto}}
#' \code{\link{ILFSlicedLNormPareto}}
#'
#' @section NetSimR pure IBNR functions:
#' \code{\link{PureIBNRGamma}}
#' \code{\link{PureIBNRLNorm}}
#'
#' @section NetSimR Sliced distribution functions:
#' \code{\link{dSlicedGammaPareto}}
#' \code{\link{dSlicedLNormPareto}}
#' \code{\link{pSlicedGammaPareto}}
#' \code{\link{pSlicedLNormPareto}}
#' \code{\link{qSlicedGammaPareto}}
#' \code{\link{qSlicedLNormPareto}}
#'
#' @section NetSimR claims simulation functions:
#' \code{\link{simulate_claims}}
#' \code{\link{simulate_function}}
#' \code{\link{simulate_portfolio}}
#' \code{\link{apply_deductible_limit}}
#'
#' @section NetSimR reserving functions:
#' \code{\link{chain_ladder}}
#' \code{\link{bornhuetter_ferguson}}
#' \code{\link{cape_cod}}
#' \code{\link{bootstrap_chain_ladder}}
#' \code{\link{merz_wuthrich}}
#' \code{\link{munich_chain_ladder}}
#' \code{\link{actual_vs_expected}}
#' \code{\link{clark_growth}}
#' \code{\link{inflation_adjusted_chain_ladder}}
#' \code{\link{as_triangle}}
#'
#' @section NetSimR pricing model functions:
#' \code{\link{tweedie_family}}
#'
#' @section NetSimR reinsurance pricing functions:
#' \code{\link{burning_cost}}
#' \code{\link{pareto_rating}}
#' \code{\link{property_exposure_rating}}
#' \code{\link{casualty_exposure_rating}}
#' \code{\link{mbbefd_curve}}
#' \code{\link{technical_premium}}
#' \code{\link{credibility_weight}}
#'
#' @section Function names:
#' The older functions have CamelCase names (\code{ParetoCappedMean}); each also has a
#' snake_case name, the same function, as the newer functions are named
#' (\code{pareto_capped_mean}). Both work.
#'
#' @section NetSimR Shiny apps:
#' \code{\link{run_shiny_simulator}}
#' \code{\link{run_shiny_distribution_fitting_tool}}
#' \code{\link{run_shiny_glm_fitting_tool}}
#' \code{\link{run_shiny_reserving_tool}}
#' \code{\link{run_shiny_reinsurance_pricing_tool}}
#' \code{\link{run_shiny_portfolio_tool}}
#'
#' @keywords internal
"_PACKAGE"
