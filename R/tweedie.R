#' Tweedie family for generalised linear models
#'
#' A Tweedie family for \code{stats::glm()}, for responses with a mass at zero and a
#' continuous positive part, such as the loss cost (claims amount per unit of exposure) of
#' insurance policies, where most policies have no claim. Its variance is
#' \eqn{\phi \mu^p} for a power \eqn{1 < p < 2}: a compound Poisson distribution of Gamma
#' claim sizes, the Poisson frequency and Gamma severity of a pure premium in one model.
#'
#' The family is quasi-likelihood: the coefficients and their standard errors are those of
#' the Tweedie distribution, with the dispersion \eqn{\phi} estimated from the Pearson
#' residuals, but its density has no closed form, so \code{AIC()} is \code{NA}. Compare
#' Tweedie models by their deviance, ideally on rows they were not fitted to. The unit
#' deviance is
#' \deqn{2 \left( \frac{y^{2-p}}{(1-p)(2-p)} - \frac{y \mu^{1-p}}{1-p} + \frac{\mu^{2-p}}{2-p} \right).}
#'
#' @param power The variance power \eqn{p}, between 1 and 2 (exclusive); 1.5 by default.
#'   Values near 1 behave like a Poisson model of the claim count, values near 2 like a
#'   Gamma model of the amount.
#' @param link The link: "log" (the default, giving multiplicative relativities),
#'   "identity", "sqrt" or "inverse".
#' @return A family object, as \code{stats::poisson()} returns, with the family name
#'   "Tweedie" and the power as \code{variance_power}.
#' @seealso \code{\link{run_shiny_glm_fitting_tool}}, which fits it from a form.
#' @export
#' @examples
#' set.seed(1)
#' n <- 2000
#' age <- sample(18:80, n, replace = TRUE)
#' claims <- rpois(n, exp(-2 + 0.02 * age))
#' loss <- vapply(claims, function(k) sum(rgamma(k, shape = 2, scale = 500)), numeric(1))
#' fit <- glm(loss ~ age, family = tweedie_family(1.5))
#' exp(coef(fit))
#' summary(fit)$dispersion
tweedie_family <- function(power = 1.5, link = "log") {
  if (!is.numeric(power) || length(power) != 1 || !is.finite(power) || power <= 1 || power >= 2) {
    stop("power must be a number between 1 and 2 (exclusive).", call. = FALSE)
  }
  if (!is.character(link) || length(link) != 1 || !link %in% c("log", "identity", "sqrt", "inverse")) {
    stop("link must be \"log\", \"identity\", \"sqrt\" or \"inverse\".", call. = FALSE)
  }
  p <- power
  variance <- list(
    name = paste0("mu^", format(p)),
    varfun = function(mu) mu^p,
    validmu = function(mu) all(is.finite(mu)) && all(mu > 0),
    dev.resids = function(y, mu, wt) {
      # y^(2 - p) is 0 for a zero response
      2 * wt * (pmax(y, 0)^(2 - p) / ((1 - p) * (2 - p)) - y * mu^(1 - p) / (1 - p) + mu^(2 - p) / (2 - p))
    },
    initialize = expression({
      if (any(y < 0)) stop("the Tweedie family needs zero or positive responses")
      n <- rep.int(1, nobs)
      mustart <- y + 0.1 * (y == 0)
    })
  )
  family <- stats::quasi(link = link, variance = variance)
  family$family <- "Tweedie"
  family$variance_power <- p
  family
}
