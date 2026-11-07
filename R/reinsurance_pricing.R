##############################
#Excess of loss reinsurance pricing
##############################

# Checks shared by the pricing functions: a number (or numbers) within limits.
pricing_check_number <- function(x, arg, min = -Inf, max = Inf, strict_min = FALSE, length_one = TRUE,
                                 allow_inf = FALSE) {
  ok <- is.numeric(x) && length(x) > 0 && !anyNA(x) && (!length_one || length(x) == 1)
  if (ok && !allow_inf) ok <- all(is.finite(x))
  if (ok) ok <- all(if (strict_min) x > min else x >= min) && all(x <= max)
  if (!ok) {
    bound <- if (is.finite(min)) paste0(if (strict_min) " above " else " at least ", format(min)) else ""
    upper <- if (is.finite(max)) paste0(if (nzchar(bound)) " and" else "", " at most ", format(max)) else ""
    stop(arg, " must be ", if (length_one) "a number" else "numbers", bound, upper, ".", call. = FALSE)
  }
  invisible(x)
}

# A data frame argument with the numeric columns `columns`.
pricing_check_table <- function(x, arg, columns) {
  if (!is.data.frame(x)) stop(arg, " must be a data frame with the columns ", paste(columns, collapse = ", "), ".", call. = FALSE)
  missing <- setdiff(columns, names(x))
  if (length(missing) > 0) {
    stop(arg, " needs the column", if (length(missing) > 1) "s", " ", paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  for (column in columns) {
    if (!is.numeric(x[[column]]) || anyNA(x[[column]])) {
      stop("The ", column, " column of ", arg, " must be numbers, with none missing.", call. = FALSE)
    }
  }
  invisible(x)
}

# The value of each year from a single value, values in the order of the years, or
# values named by year.
pricing_by_year <- function(x, years, arg, min = 0) {
  if (!is.numeric(x) || length(x) == 0 || anyNA(x)) stop(arg, " must be numbers.", call. = FALSE)
  if (!is.null(names(x)) && all(nzchar(names(x)))) {
    at <- match(as.character(years), names(x))
    if (anyNA(at)) {
      stop(arg, " has no value for ", paste(years[is.na(at)], collapse = ", "), ".", call. = FALSE)
    }
    x <- unname(x[at])
  } else if (length(x) == 1) {
    x <- rep(x, length(years))
  } else if (length(x) != length(years)) {
    stop(arg, " must be one number, one per year (", length(years), "), or numbers named by year.", call. = FALSE)
  }
  if (any(!is.finite(x)) || any(x < min)) stop(arg, " must be finite numbers of at least ", min, ".", call. = FALSE)
  x
}

# The subject premium of each year, from a data frame with year and premium columns
# or a numeric vector named by year.
pricing_premium <- function(premium) {
  if (is.data.frame(premium)) {
    pricing_check_table(premium, "premium", c("year", "premium"))
    years <- premium$year
    values <- premium$premium
  } else if (is.numeric(premium) && !is.null(names(premium))) {
    years <- suppressWarnings(as.numeric(names(premium)))
    if (anyNA(years)) stop("The names of premium must be years.", call. = FALSE)
    values <- unname(premium)
  } else {
    stop("premium must be a data frame with year and premium columns, or numbers named by year.", call. = FALSE)
  }
  if (anyDuplicated(years)) stop("premium has more than one value for a year.", call. = FALSE)
  if (any(!is.finite(values)) || any(values <= 0)) stop("Every year's premium must be a positive number.", call. = FALSE)
  o <- order(years)
  list(year = years[o], premium = values[o])
}

# The factor that brings a loss of each year to the money of the rating year: from an
# annual rate, (1 + rate)^(rating year - year), or from index values named by year.
pricing_index_factors <- function(inflation, years, rating_year) {
  if (!is.numeric(inflation) || length(inflation) == 0 || anyNA(inflation)) {
    stop("inflation must be an annual rate or index values named by year.", call. = FALSE)
  }
  if (length(inflation) == 1 && is.null(names(inflation))) {
    pricing_check_number(inflation, "inflation", min = -1, strict_min = TRUE)
    return((1 + inflation)^(rating_year - years))
  }
  values <- pricing_by_year(inflation, c(years, rating_year), "inflation (index values)", min = 0)
  if (any(values <= 0)) stop("The index values in inflation must be positive.", call. = FALSE)
  values[length(values)] / values[-length(values)]
}

# The losses of the experience, a data frame with year and amount columns.
pricing_losses <- function(losses, years) {
  pricing_check_table(losses, "losses", c("year", "amount"))
  unknown <- setdiff(unique(losses$year), years)
  if (length(unknown) > 0) {
    stop("losses has years with no premium: ", paste(sort(unknown), collapse = ", "), ".", call. = FALSE)
  }
  if (any(losses$amount < 0)) stop("The loss amounts must not be negative.", call. = FALSE)
  losses
}

# The part of each claim in a layer of `limit` excess of `deductible`.
pricing_layer <- function(x, deductible, limit) pmin(pmax(x - deductible, 0), limit)

#' MBBEFD exposure curves
#'
#' @description The exposure curves of Bernegger's MBBEFD class, used to rate property
#'   excess of loss treaties from a risk profile. \code{mbbefd_curve()} gives the
#'   share of a risk's expected loss that falls below a deductible of \code{x} times
#'   its sum insured (or maximum possible loss), for the parameters \code{b} and
#'   \code{g}. \code{swiss_re_parameters()} gives \code{b} and \code{g} of the
#'   one-parameter curves of Swiss Re, and \code{swiss_re_curve()} the curve itself:
#'   c = 1.5, 2, 3 and 4 are the curves Y1 to Y4, from personal lines to large
#'   industrial risks, and c = 5 the curve used at Lloyd's for industrial risks.
#'
#' @details With \eqn{g = 1} or \eqn{b = 0} every loss is a total loss and
#'   \eqn{G(x) = x}; with \eqn{b = 1}, \eqn{G(x) = \log(1 + (g - 1)x) / \log(g)}; with
#'   \eqn{bg = 1}, \eqn{G(x) = (1 - b^x) / (1 - b)}; otherwise
#'   \deqn{G(x) = \log\left(\frac{(g - 1)b + (1 - gb)b^x}{1 - b}\right) / \log(gb).}
#'   The probability of a total loss is \eqn{1/g}. The Swiss Re curves have
#'   \eqn{b = e^{3.1 - 0.15(1 + c)c}} and \eqn{g = e^{(0.78 + 0.12c)c}}.
#'
#'   A layer of \code{limit} excess of \code{deductible} on a risk with sum insured
#'   \eqn{M} takes the share \eqn{G((d + l) / M) - G(d / M)} of the risk's expected
#'   loss; see \code{\link{property_exposure_rating}}.
#' @param x The deductible as a share of the sum insured: numbers from 0; those above
#'   1 are taken as 1.
#' @param b,g The parameters of the curve: \code{b} at least 0 and \code{g} at least 1.
#'   The arguments are recycled to a common length.
#' @param c The parameter of a Swiss Re curve, at least 0, or the name of one: "Y1",
#'   "Y2", "Y3", "Y4" or "Lloyds".
#' @return \code{mbbefd_curve()} and \code{swiss_re_curve()}: the curve at \code{x},
#'   between 0 and 1. \code{swiss_re_parameters()}: a data frame with the columns c,
#'   b and g, a row for each value of \code{c}.
#' @references Bernegger, S. (1997). The Swiss Re exposure curves and the MBBEFD
#'   distribution class. ASTIN Bulletin 27(1), 99-111.
#' @family reinsurance pricing functions
#' @export
#' @examples
#' # a layer of 4m excess of 1m on a risk of 10m, on the Y3 curve
#' swiss_re_curve(0.5, "Y3") - swiss_re_curve(0.1, "Y3")
#' swiss_re_parameters(c("Y1", "Y2", "Y3", "Y4", "Lloyds"))
#' mbbefd_curve(c(0, 0.25, 0.5, 1), b = 2, g = 10)
mbbefd_curve <- function(x, b, g) {
  pricing_check_number(x, "x", min = 0, length_one = FALSE, allow_inf = TRUE)
  pricing_check_number(b, "b", min = 0, length_one = FALSE)
  pricing_check_number(g, "g", min = 1, length_one = FALSE)
  n <- max(length(x), length(b), length(g))
  x <- rep_len(pmin(x, 1), n)
  b <- rep_len(b, n)
  g <- rep_len(g, n)
  out <- x
  curved <- g > 1 & b > 0
  b_one <- curved & abs(b - 1) < 1e-10
  bg_one <- curved & !b_one & abs(b * g - 1) < 1e-10
  general <- curved & !b_one & !bg_one
  out[b_one] <- log1p((g[b_one] - 1) * x[b_one]) / log(g[b_one])
  out[bg_one] <- (1 - b[bg_one]^x[bg_one]) / (1 - b[bg_one])
  bb <- b[general]
  gg <- g[general]
  out[general] <- log(((gg - 1) * bb + (1 - gg * bb) * bb^x[general]) / (1 - bb)) / log(gg * bb)
  pmin(pmax(out, 0), 1)
}

# The parameters c of the named Swiss Re curves.
swiss_re_named_curves <- c(Y1 = 1.5, Y2 = 2, Y3 = 3, Y4 = 4, Lloyds = 5)

# The parameter c of a Swiss Re curve from a number or a name.
swiss_re_c <- function(c) {
  if (is.character(c)) {
    key <- gsub("[^a-z0-9]", "", tolower(c))
    at <- match(key, tolower(names(swiss_re_named_curves)))
    if (anyNA(at)) {
      stop("c must be numbers of at least 0 or the names ", paste0("\"", names(swiss_re_named_curves), "\"", collapse = ", "),
           ".", call. = FALSE)
    }
    return(unname(swiss_re_named_curves[at]))
  }
  pricing_check_number(c, "c", min = 0, length_one = FALSE)
  c
}

#' @rdname mbbefd_curve
#' @export
swiss_re_parameters <- function(c) {
  c <- swiss_re_c(c)
  data.frame(c = c, b = exp(3.1 - 0.15 * (1 + c) * c), g = exp((0.78 + 0.12 * c) * c))
}

#' @rdname mbbefd_curve
#' @export
swiss_re_curve <- function(x, c) {
  p <- swiss_re_parameters(c)
  mbbefd_curve(x, p$b, p$g)
}

# An exposure curve from a name or number (a Swiss Re curve) or c(b = , g = ): its
# label, parameters and function.
pricing_curve <- function(curve) {
  if (is.numeric(curve) && length(curve) == 2 && !is.null(names(curve)) && setequal(names(curve), c("b", "g"))) {
    b <- curve[["b"]]
    g <- curve[["g"]]
    pricing_check_number(b, "The b of curve", min = 0)
    pricing_check_number(g, "The g of curve", min = 1)
    label <- paste0("MBBEFD (b = ", signif(b, 4), ", g = ", signif(g, 4), ")")
    c <- NA_real_
  } else if ((is.character(curve) || is.numeric(curve)) && length(curve) == 1 && !is.na(curve)) {
    c <- swiss_re_c(curve)
    p <- swiss_re_parameters(c)
    b <- p$b
    g <- p$g
    name <- names(swiss_re_named_curves)[match(c, swiss_re_named_curves)]
    label <- if (is.character(curve) && !is.na(name)) {
      paste0(if (name == "Lloyds") "Lloyd's" else paste("Swiss Re", name), " (c = ", c, ")")
    } else {
      paste0("Swiss Re c = ", c)
    }
  } else {
    stop("curve must be the name of a Swiss Re curve (\"Y1\" to \"Y4\", \"Lloyds\"), its parameter c, or c(b = , g = ).",
         call. = FALSE)
  }
  list(label = label, c = c, b = b, g = g, total_loss_probability = 1 / g,
       fn = function(x) mbbefd_curve(x, b, g))
}

# The loss ratio of each band or policy: the argument, or the profile's column.
pricing_loss_ratio <- function(loss_ratio, profile) {
  n <- nrow(profile)
  if (is.null(loss_ratio)) {
    if (!"loss_ratio" %in% names(profile)) {
      stop("Give loss_ratio, or a loss_ratio column in profile.", call. = FALSE)
    }
    loss_ratio <- profile$loss_ratio
  }
  if (!is.numeric(loss_ratio) || !(length(loss_ratio) %in% c(1, n)) || anyNA(loss_ratio) ||
      any(!is.finite(loss_ratio)) || any(loss_ratio < 0)) {
    stop("loss_ratio must be one number of at least 0, or one per row of profile.", call. = FALSE)
  }
  rep_len(loss_ratio, n)
}

# Checks the layer of an exposure rating or burning cost.
pricing_check_layer <- function(deductible, limit) {
  pricing_check_number(deductible, "deductible", min = 0)
  pricing_check_number(limit, "limit", min = 0, strict_min = TRUE, allow_inf = TRUE)
}

# The exposure rating object.
pricing_exposure_result <- function(type, bands, deductible, limit, extra) {
  layer <- sum(bands$layer_loss)
  premium <- sum(bands$premium)
  structure(c(list(
    type = type, bands = bands, deductible = deductible, limit = limit,
    subject_premium = premium, ground_up_loss = sum(bands$expected_loss), expected_loss = layer,
    rate = if (premium > 0) layer / premium else NA_real_
  ), extra), class = "netsimr_exposure_rating")
}

#' Exposure rating of property and casualty excess of loss layers
#'
#' @description Rates a layer of \code{limit} excess of \code{deductible} from a risk
#'   profile, without loss experience. Each band's (or policy's) expected loss, its
#'   premium times its loss ratio, is shared between the layer and the rest by an
#'   exposure curve. \code{property_exposure_rating()} uses an MBBEFD curve of the
#'   damage as a share of the sum insured (\code{\link{mbbefd_curve}}): the layer takes
#'   \eqn{G((d + l) / M) - G(d / M)} of a band with sum insured \eqn{M}.
#'   \code{casualty_exposure_rating()} uses the limited expected value of a claim
#'   size distribution: the layer takes
#'   \eqn{(LEV(\min(P, d + l)) - LEV(\min(P, d))) / LEV(P)} of a band with policy
#'   limit \eqn{P}.
#'
#' @param profile A data frame with a row per band of the risk profile (or per
#'   policy): for property, the columns \code{sum_insured} (the band's sum insured or
#'   maximum possible loss per risk, often its average) and \code{premium}; for
#'   casualty, \code{policy_limit} and \code{premium}. An optional
#'   \code{loss_ratio} column gives each band's expected loss ratio; other columns,
#'   such as a band label, are kept.
#' @param deductible The deductible of the layer, at least 0.
#' @param limit The limit of the layer, above 0; \code{Inf} for an unlimited layer.
#' @param curve The exposure curve: a Swiss Re curve by name ("Y1", "Y2", "Y3", "Y4"
#'   or "Lloyds") or by its parameter c, or an MBBEFD curve as \code{c(b = , g = )}.
#' @param severity The claim size distribution of the casualty business, as in
#'   \code{\link{simulate_claims}}: "LogNormal" (meanlog, sdlog), "Gamma" (shape,
#'   scale), "Exponential" (rate) or "Pareto" (alpha, x_m).
#' @param severity_params Its parameters, in that order or named.
#' @param loss_ratio The expected ground-up loss ratio: one for all bands, or one per
#'   band. NULL (the default) takes the profile's \code{loss_ratio} column.
#' @return An object of class \code{netsimr_exposure_rating}, a list with
#'   \code{bands} (the profile with each band's loss ratio, expected loss, the shares
#'   of the curve at the deductible and the exhaustion point, the layer's share and
#'   the expected layer loss), \code{expected_loss} (the layer's), \code{rate} (the
#'   expected layer loss as a share of the subject premium), \code{subject_premium},
#'   \code{ground_up_loss}, the layer and the curve or severity.
#' @seealso \code{\link{burning_cost}} and \code{\link{pareto_rating}}, the experience
#'   rating of a layer, and \code{\link{technical_premium}}.
#' @family reinsurance pricing functions
#' @export
#' @examples
#' rating <- property_exposure_rating(xl_property_profile, deductible = 2e6, limit = 8e6,
#'                                    curve = "Y3", loss_ratio = 0.55)
#' rating
#' rating$bands[, c("band", "sum_insured", "layer_share", "layer_loss")]
#'
#' casualty_exposure_rating(xl_casualty_profile, deductible = 1e6, limit = 4e6,
#'                          severity = "LogNormal", severity_params = c(10, 2), loss_ratio = 0.65)
property_exposure_rating <- function(profile, deductible, limit, curve = "Y3", loss_ratio = NULL) {
  pricing_check_table(profile, "profile", c("sum_insured", "premium"))
  if (nrow(profile) == 0) stop("profile has no rows.", call. = FALSE)
  if (any(profile$sum_insured <= 0) || any(!is.finite(profile$sum_insured))) {
    stop("The sums insured must be positive numbers.", call. = FALSE)
  }
  if (any(profile$premium < 0) || any(!is.finite(profile$premium))) stop("The premiums must not be negative.", call. = FALSE)
  pricing_check_layer(deductible, limit)
  lr <- pricing_loss_ratio(loss_ratio, profile)
  cv <- pricing_curve(curve)
  d <- pmin(deductible / profile$sum_insured, 1)
  e <- pmin((deductible + limit) / profile$sum_insured, 1)
  share <- cv$fn(e) - cv$fn(d)
  bands <- profile
  bands$loss_ratio <- lr
  bands$expected_loss <- profile$premium * lr
  bands$deductible_share <- d
  bands$exhaustion_share <- e
  bands$layer_share <- share
  bands$layer_loss <- bands$expected_loss * share
  pricing_exposure_result("property", bands, deductible, limit, list(curve = cv[c("label", "c", "b", "g", "total_loss_probability")]))
}

# The limited expected value of a claim size distribution with a closed-form capped
# mean in the package, from the simulator's distribution names and parameters.
pricing_lev <- function(severity, severity_params) {
  allowed <- c("LogNormal", "Gamma", "Exponential", "Pareto")
  options <- sev_dist_options[allowed]
  id <- claims_distribution_id(severity, options, "severity")
  p <- claims_distribution_params(severity_params, options[[id]], "severity_params")
  problems <- distribution_param_problems(options[[id]], p)
  if (length(problems) > 0) stop("severity_params: ", paste(problems, collapse = " "), call. = FALSE)
  fn <- switch(id,
    LogNormal = function(x) LNormCappedMean(x, p[1], p[2]),
    Gamma = function(x) GammaCappedMean(x, p[1], 1 / p[2]),
    Exponential = function(x) GammaCappedMean(x, 1, p[1]),
    Pareto = function(x) ParetoCappedMean(x, p[2], p[1])
  )
  list(id = id, label = options[[id]]@distr_label, params = stats::setNames(p, claims_param_names[[id]]), fn = fn)
}

#' @rdname property_exposure_rating
#' @export
casualty_exposure_rating <- function(profile, deductible, limit, severity, severity_params, loss_ratio = NULL) {
  pricing_check_table(profile, "profile", c("policy_limit", "premium"))
  if (nrow(profile) == 0) stop("profile has no rows.", call. = FALSE)
  if (any(profile$policy_limit <= 0)) stop("The policy limits must be positive.", call. = FALSE)
  if (any(profile$premium < 0) || any(!is.finite(profile$premium))) stop("The premiums must not be negative.", call. = FALSE)
  pricing_check_layer(deductible, limit)
  lr <- pricing_loss_ratio(loss_ratio, profile)
  sev <- pricing_lev(severity, severity_params)
  limits <- profile$policy_limit
  full <- sev$fn(limits)
  if (any(!is.finite(full))) {
    stop("The claim size distribution has an infinite mean up to a policy limit: give finite policy limits.", call. = FALSE)
  }
  low <- sev$fn(pmin(limits, deductible))
  high <- sev$fn(pmin(limits, deductible + limit))
  share <- (high - low) / full
  bands <- profile
  bands$loss_ratio <- lr
  bands$expected_loss <- profile$premium * lr
  bands$layer_share <- share
  bands$layer_loss <- bands$expected_loss * share
  pricing_exposure_result("casualty", bands, deductible, limit, list(severity = sev[c("id", "label", "params")]))
}

#' @export
print.netsimr_exposure_rating <- function(x, ...) {
  basis <- if (identical(x$type, "property")) x$curve$label else {
    paste0(x$severity$label, " (", paste(names(x$severity$params), signif(x$severity$params, 4), sep = " = ", collapse = ", "), ")")
  }
  cat("Exposure rating (", x$type, ") of ", format(x$limit, big.mark = ",", scientific = FALSE), " xs ",
      format(x$deductible, big.mark = ",", scientific = FALSE), "\n", sep = "")
  cat("  Curve:              ", basis, "\n", sep = "")
  cat("  Bands:              ", nrow(x$bands), "\n", sep = "")
  cat("  Subject premium:    ", formatC(round(x$subject_premium), format = "f", digits = 0, big.mark = ","), "\n", sep = "")
  cat("  Ground-up loss:     ", formatC(round(x$ground_up_loss), format = "f", digits = 0, big.mark = ","), "\n", sep = "")
  cat("  Expected layer loss:", formatC(round(x$expected_loss), format = "f", digits = 0, big.mark = ","), "\n", sep = " ")
  cat("  Rate:               ", formatC(100 * x$rate, format = "f", digits = 3), "% of subject premium\n", sep = "")
  invisible(x)
}

# The experience shared by the burning cost and the Pareto rating: the years and
# their premiums, the losses brought to the rating year, the factors of each year.
pricing_experience <- function(losses, premium, inflation, rating_year, development, premium_index, exclude) {
  p <- pricing_premium(premium)
  years <- p$year
  losses <- pricing_losses(losses, years)
  if (is.null(rating_year)) rating_year <- max(years) + 1
  pricing_check_number(rating_year, "rating_year")
  index <- pricing_index_factors(inflation, years, rating_year)
  development <- pricing_by_year(development, years, "development")
  premium_index <- pricing_by_year(premium_index, years, "premium_index")
  if (any(premium_index <= 0)) stop("premium_index must be positive.", call. = FALSE)
  if (!is.null(exclude)) {
    if (!is.numeric(exclude) || anyNA(exclude)) stop("exclude must be years.", call. = FALSE)
  }
  included <- !(years %in% exclude)
  if (!any(included)) stop("exclude leaves no years.", call. = FALSE)
  at <- match(losses$year, years)
  detail <- data.frame(year = losses$year, amount = losses$amount, index = index[at], indexed = losses$amount * index[at])
  extra <- setdiff(names(losses), c("year", "amount"))
  if (length(extra) > 0) detail <- cbind(losses[extra], detail)
  list(years = years, premium = p$premium, rating_year = rating_year, index = index, development = development,
       premium_index = premium_index, onlevel = p$premium * premium_index, included = included, detail = detail, at = at)
}

#' Burning cost of an excess of loss layer
#'
#' @description Experience rating of a layer of \code{limit} excess of
#'   \code{deductible}: the past large losses are brought to the money of the rating
#'   year, put through the layer as if it had been in force, developed to ultimate
#'   and divided by the premium of their year, brought to the rating year's rates.
#'
#' @details Each loss is indexed to the rating year with \code{inflation}, and its
#'   part in the layer, \eqn{\min(\max(x - d, 0), l)}, taken. Each year's layer
#'   losses are multiplied by its \code{development} factor (for the late reported
#'   claims and the development of the known ones, e.g. from
#'   \code{\link{chain_ladder}}), and then the aggregate deductible and limit apply.
#'   The burning cost of a year is its layer losses over its subject premium times
#'   its \code{premium_index} (the on-level premium). The burning cost of the layer
#'   is the total over the total, weighting years by premium, or the mean of the
#'   years' rates.
#'
#'   Losses are usually given above a reporting threshold. A loss just below the
#'   threshold, indexed, reaches the layer when the indexed threshold exceeds the
#'   deductible; a warning names the years where it does.
#' @param losses A data frame of the large losses with the columns \code{year} and
#'   \code{amount} (in the money of their year); other columns, such as a claim
#'   number, are kept.
#' @param premium The subject premium of each year: a data frame with the columns
#'   \code{year} and \code{premium}, or numbers named by year. Its years are those of
#'   the experience; a year without losses has none in the layer.
#' @param deductible,limit The layer, \code{limit} (or \code{Inf}) excess of
#'   \code{deductible}, for each loss.
#' @param aggregate_deductible,aggregate_limit The annual aggregate deductible and
#'   limit of the layer's losses; the aggregate limit is often the limit times one
#'   plus the number of reinstatements.
#' @param inflation An annual rate (0.05 for 5\%) or index values named by year,
#'   including the rating year.
#' @param rating_year The year being priced; by default the year after the last.
#' @param development One factor for all years, one per year, or factors named by
#'   year, applied to each year's layer losses. Defaults to 1.
#' @param premium_index The factor that brings each year's premium to the rating
#'   year's rates and exposure, as \code{development}. Defaults to 1.
#' @param threshold The reporting threshold of the losses, used for the warning; NULL
#'   (the default) for none.
#' @param exclude Years left out of the burning cost (they are still shown).
#' @param weights "premium" (the default) for the total layer losses over the total
#'   on-level premium, or "equal" for the mean of the years' burning costs.
#' @param rating_premium The subject premium of the rating year; the expected layer
#'   loss is the burning cost times it. NULL (the default) gives no expected loss.
#' @return An object of class \code{netsimr_burning_cost}, a list with
#'   \code{by_year} (each year's number of losses and of losses in the layer, its
#'   indexed losses, layer losses, development, developed and aggregate layer losses,
#'   premium, premium index, on-level premium, burning cost and whether it is
#'   included, and, with \code{rating_premium}, the layer losses at the rating year's
#'   premium), \code{losses} (each loss indexed and in the layer), \code{rate} (the
#'   burning cost), \code{expected_loss}, \code{rating_year}, \code{rating_premium},
#'   \code{n_layer_losses} (in the years included) and the layer.
#' @seealso \code{\link{pareto_rating}}, \code{\link{property_exposure_rating}} and
#'   \code{\link{technical_premium}}.
#' @family reinsurance pricing functions
#' @export
#' @examples
#' bc <- burning_cost(xl_large_losses, xl_premium[, c("year", "premium")],
#'                    deductible = 1e6, limit = 4e6, inflation = 0.04,
#'                    development = setNames(xl_premium$development, xl_premium$year),
#'                    premium_index = setNames(xl_premium$rate_index, xl_premium$year),
#'                    threshold = 5e5, rating_premium = 60e6)
#' bc
#' bc$by_year[, c("year", "layer_losses", "onlevel_premium", "burning_cost")]
burning_cost <- function(losses, premium, deductible, limit, aggregate_deductible = 0, aggregate_limit = Inf,
                         inflation = 0, rating_year = NULL, development = 1, premium_index = 1, threshold = NULL,
                         exclude = NULL, weights = c("premium", "equal"), rating_premium = NULL) {
  weights <- match.arg(weights)
  pricing_check_layer(deductible, limit)
  pricing_check_number(aggregate_deductible, "aggregate_deductible", min = 0)
  pricing_check_number(aggregate_limit, "aggregate_limit", min = 0, strict_min = TRUE, allow_inf = TRUE)
  if (!is.null(rating_premium)) pricing_check_number(rating_premium, "rating_premium", min = 0, strict_min = TRUE)
  ex <- pricing_experience(losses, premium, inflation, rating_year, development, premium_index, exclude)
  detail <- ex$detail
  detail$layer <- pricing_layer(detail$indexed, deductible, limit)
  sum_by_year <- function(x) {
    out <- numeric(length(ex$years))
    if (length(x) > 0) {
      sums <- tapply(x, ex$at, sum)
      out[as.integer(names(sums))] <- sums
    }
    out
  }
  layer <- sum_by_year(detail$layer)
  developed <- layer * ex$development
  aggregate <- pmin(pmax(developed - aggregate_deductible, 0), aggregate_limit)
  by_year <- data.frame(
    year = ex$years, losses = sum_by_year(rep(1, nrow(detail))), layer_count = sum_by_year(as.numeric(detail$layer > 0)),
    indexed_losses = sum_by_year(detail$indexed), index = ex$index, layer_losses = layer,
    development = ex$development, developed_losses = developed, aggregate_losses = aggregate,
    premium = ex$premium, premium_index = ex$premium_index, onlevel_premium = ex$onlevel,
    burning_cost = aggregate / ex$onlevel, included = ex$included
  )
  if (!is.null(rating_premium)) by_year$at_rating_premium <- aggregate * rating_premium / ex$onlevel
  used <- by_year[by_year$included, ]
  rate <- if (weights == "premium") sum(used$aggregate_losses) / sum(used$onlevel_premium) else mean(used$burning_cost)
  if (!is.null(threshold)) {
    pricing_check_number(threshold, "threshold", min = 0)
    short <- ex$years[threshold * ex$index > deductible]
    if (length(short) > 0) {
      warning("The reporting threshold, indexed to ", ex$rating_year, ", is above the deductible in ",
              paste(short, collapse = ", "), ": losses below the threshold could reach the layer and are missing.",
              call. = FALSE)
    }
  }
  structure(list(
    by_year = by_year, losses = detail, rate = rate,
    expected_loss = if (is.null(rating_premium)) NA_real_ else rate * rating_premium,
    rating_year = ex$rating_year, rating_premium = if (is.null(rating_premium)) NA_real_ else rating_premium,
    n_layer_losses = sum(used$layer_count), deductible = deductible, limit = limit,
    aggregate_deductible = aggregate_deductible, aggregate_limit = aggregate_limit, weights = weights
  ), class = "netsimr_burning_cost")
}

#' @export
print.netsimr_burning_cost <- function(x, ...) {
  years <- x$by_year$year[x$by_year$included]
  cat("Burning cost of ", format(x$limit, big.mark = ",", scientific = FALSE), " xs ",
      format(x$deductible, big.mark = ",", scientific = FALSE), " for ", x$rating_year, "\n", sep = "")
  cat("  Years:              ", length(years), " (", min(years), " to ", max(years), ")\n", sep = "")
  cat("  Losses in the layer:", x$n_layer_losses, "\n")
  cat("  Burning cost:       ", formatC(100 * x$rate, format = "f", digits = 3), "% of subject premium (",
      if (x$weights == "premium") "premium weighted" else "mean of the years", ")\n", sep = "")
  if (is.finite(x$expected_loss)) {
    cat("  Expected layer loss:", formatC(round(x$expected_loss), format = "f", digits = 0, big.mark = ","), "\n")
  }
  invisible(x)
}

#' Pareto rating of an excess of loss layer
#'
#' @description Experience rating of a layer with a Pareto distribution fitted to the
#'   large losses: the losses above a \code{threshold}, brought to the money of the
#'   rating year, give the frequency of large losses per unit of on-level premium and
#'   the Pareto alpha; the layer's expected loss is the frequency times the expected
#'   part of a large loss in the layer. Unlike the burning cost it prices layers above
#'   the largest loss seen.
#'
#' @details Alpha is estimated by maximum likelihood, corrected for its bias:
#'   \eqn{\hat\alpha = (n - 1) / \sum \log(x_i / t)} for the \eqn{n} indexed losses
#'   \eqn{x_i} above the threshold \eqn{t}. The number of large losses of each year is
#'   multiplied by its \code{development} factor, for the losses not reported yet, and
#'   their total divided by the total on-level premium. The expected part of a large
#'   loss in the layer is \eqn{E[\min(X, d + l)] - E[\min(X, d)]} for a Pareto with
#'   minimum \eqn{t} (\code{\link{ParetoCappedMean}}), which needs a deductible of at
#'   least the threshold. Aggregate terms are not applied.
#' @inheritParams burning_cost
#' @param threshold The threshold of the Pareto, in the money of the rating year: the
#'   losses above it, indexed, are used. At most the deductible.
#' @param development One factor for all years, one per year, or factors named by
#'   year, applied to each year's number of large losses. Defaults to 1.
#' @param alpha The Pareto alpha, to use a market or selected alpha instead of the
#'   one estimated; NULL (the default) estimates it.
#' @return An object of class \code{netsimr_pareto_rating}, a list with
#'   \code{alpha}, \code{alpha_estimated} (the estimate, also when \code{alpha} is
#'   given), \code{n} (the losses above the threshold in the years included),
#'   \code{frequency} (large losses per unit of on-level premium),
#'   \code{expected_count} (at the rating premium), \code{layer_severity} (the
#'   expected part of a large loss in the layer), \code{rate}, \code{expected_loss},
#'   \code{by_year} and \code{losses} (the indexed losses above the threshold).
#' @references Schmutz, M. and Doerr, R. R. (1998). The Pareto model in property
#'   reinsurance: formulas and applications. Swiss Re.
#' @seealso \code{\link{burning_cost}} and \code{\link{technical_premium}}.
#' @family reinsurance pricing functions
#' @export
#' @examples
#' pareto_rating(xl_large_losses, xl_premium[, c("year", "premium")], threshold = 7.5e5,
#'               deductible = 1e6, limit = 4e6, inflation = 0.04,
#'               premium_index = setNames(xl_premium$rate_index, xl_premium$year),
#'               rating_premium = 60e6)
pareto_rating <- function(losses, premium, threshold, deductible, limit, inflation = 0, rating_year = NULL,
                          development = 1, premium_index = 1, exclude = NULL, alpha = NULL, rating_premium = NULL) {
  pricing_check_number(threshold, "threshold", min = 0, strict_min = TRUE)
  pricing_check_layer(deductible, limit)
  if (deductible < threshold) stop("The deductible must be at least the threshold of the Pareto.", call. = FALSE)
  if (!is.null(rating_premium)) pricing_check_number(rating_premium, "rating_premium", min = 0, strict_min = TRUE)
  ex <- pricing_experience(losses, premium, inflation, rating_year, development, premium_index, exclude)
  detail <- ex$detail
  large <- detail$indexed > threshold
  counts <- tabulate(ex$at[large], nbins = length(ex$years))
  used_large <- large & ex$included[ex$at]
  n <- sum(used_large)
  logs <- sum(log(detail$indexed[used_large] / threshold))
  alpha_estimated <- if (n >= 2 && logs > 0) (n - 1) / logs else NA_real_
  if (is.null(alpha)) {
    if (!is.finite(alpha_estimated)) {
      stop("Estimating alpha needs at least two losses above the threshold in the years included.", call. = FALSE)
    }
    alpha <- alpha_estimated
  } else {
    pricing_check_number(alpha, "alpha", min = 0, strict_min = TRUE)
  }
  developed <- counts * ex$development
  frequency <- sum(developed[ex$included]) / sum(ex$onlevel[ex$included])
  severity <- ParetoCappedMean(deductible + limit, threshold, alpha) - ParetoCappedMean(deductible, threshold, alpha)
  rate <- frequency * severity
  by_year <- data.frame(year = ex$years, large_losses = counts, development = ex$development,
                        developed_count = developed, onlevel_premium = ex$onlevel,
                        frequency = developed / ex$onlevel, included = ex$included)
  structure(list(
    alpha = alpha, alpha_estimated = alpha_estimated, n = n, threshold = threshold, frequency = frequency,
    expected_count = if (is.null(rating_premium)) NA_real_ else frequency * rating_premium,
    layer_severity = severity, rate = rate,
    expected_loss = if (is.null(rating_premium)) NA_real_ else rate * rating_premium,
    rating_year = ex$rating_year, rating_premium = if (is.null(rating_premium)) NA_real_ else rating_premium,
    by_year = by_year, losses = detail[large, , drop = FALSE], deductible = deductible, limit = limit
  ), class = "netsimr_pareto_rating")
}

#' @export
print.netsimr_pareto_rating <- function(x, ...) {
  cat("Pareto rating of ", format(x$limit, big.mark = ",", scientific = FALSE), " xs ",
      format(x$deductible, big.mark = ",", scientific = FALSE), " for ", x$rating_year, "\n", sep = "")
  cat("  Threshold:          ", format(x$threshold, big.mark = ",", scientific = FALSE), "\n", sep = "")
  cat("  Losses above it:    ", x$n, "\n", sep = "")
  cat("  Alpha:              ", formatC(x$alpha, format = "f", digits = 3),
      if (!isTRUE(all.equal(x$alpha, x$alpha_estimated))) paste0(" (estimated ", formatC(x$alpha_estimated, format = "f", digits = 3), ")"),
      "\n", sep = "")
  cat("  Rate:               ", formatC(100 * x$rate, format = "f", digits = 3), "% of subject premium\n", sep = "")
  if (is.finite(x$expected_loss)) {
    cat("  Expected count:     ", formatC(x$expected_count, format = "f", digits = 2), " losses above the threshold\n", sep = "")
    cat("  Expected layer loss:", formatC(round(x$expected_loss), format = "f", digits = 0, big.mark = ","), "\n")
  }
  invisible(x)
}

#' Technical premium of an excess of loss layer
#'
#' @description The premium of a layer from its expected loss: the total premium
#'   covers the expected loss and a loading for expenses, brokerage and profit, and
#'   with paid reinstatements part of it comes as reinstatement premium, so the
#'   upfront premium is lower.
#'
#' @details The total premium is \eqn{EL / (1 - loading)}. With \eqn{n}
#'   reinstatements paid at the rate \eqn{r} of the upfront premium \eqn{P}, pro rata
#'   as to amount, the expected reinstatement premium is \eqn{P r u}, where \eqn{u} is
#'   the expected reinstated cover as a share of the limit,
#'   \eqn{E[\min(S, n l)] / l} for the annual layer loss \eqn{S}; so
#'   \eqn{P = EL / ((1 - loading)(1 + r u))}. Without \code{used}, \eqn{u} is taken as
#'   \eqn{\min(EL / l, n)}, which is exact when the annual losses never exceed the
#'   reinstated cover; the burning cost's layer losses at the rating premium give a
#'   better one (see the examples). For the reinstatement premiums of each simulated
#'   year, see \code{\link{reinstatement_premium}}.
#' @param expected_loss The expected loss of the layer.
#' @param limit The limit of the layer.
#' @param loading The share of the total premium for expenses, brokerage and profit,
#'   from 0 to below 1. Defaults to 0.
#' @param reinstatements The number of paid reinstatements; 0 (the default) for
#'   none, \code{Inf} for unlimited.
#' @param reinstatement_rate The rate of each reinstatement, as a share of the
#'   upfront premium. Defaults to 1 (100\%).
#' @param used The expected reinstated cover as a share of the limit; NULL (the
#'   default) for \eqn{\min(EL / l, n)}.
#' @return A list with \code{expected_loss}, \code{total_premium},
#'   \code{upfront_premium}, \code{reinstatement_premium} (expected),
#'   \code{rate_on_line} (the upfront premium over the limit), \code{payback} (the
#'   limit over the upfront premium, in years) and \code{used}.
#' @family reinsurance pricing functions
#' @export
#' @examples
#' technical_premium(expected_loss = 1.2e6, limit = 4e6, loading = 0.25,
#'                   reinstatements = 1, reinstatement_rate = 1)
#'
#' # the reinstated cover used in each past year, at the rating year's premium
#' bc <- burning_cost(xl_large_losses, xl_premium[, c("year", "premium")], 1e6, 4e6,
#'                    inflation = 0.04, rating_premium = 60e6)
#' used <- mean(pmin(bc$by_year$at_rating_premium, 1 * 4e6)) / 4e6
#' technical_premium(bc$expected_loss, 4e6, loading = 0.25, reinstatements = 1, used = used)
technical_premium <- function(expected_loss, limit, loading = 0, reinstatements = 0, reinstatement_rate = 1, used = NULL) {
  pricing_check_number(expected_loss, "expected_loss", min = 0)
  pricing_check_number(limit, "limit", min = 0, strict_min = TRUE)
  pricing_check_number(loading, "loading", min = 0)
  if (loading >= 1) stop("loading must be below 1.", call. = FALSE)
  pricing_check_number(reinstatements, "reinstatements", min = 0, allow_inf = TRUE)
  pricing_check_number(reinstatement_rate, "reinstatement_rate", min = 0)
  if (is.null(used)) used <- min(expected_loss / limit, reinstatements)
  pricing_check_number(used, "used", min = 0, max = reinstatements, allow_inf = TRUE)
  if (!is.finite(used)) stop("used must be finite.", call. = FALSE)
  total <- expected_loss / (1 - loading)
  upfront <- if (reinstatements > 0) total / (1 + reinstatement_rate * used) else total
  list(expected_loss = expected_loss, total_premium = total, upfront_premium = upfront,
       reinstatement_premium = total - upfront, rate_on_line = upfront / limit,
       payback = if (upfront > 0) limit / upfront else Inf, used = used)
}

#' Credibility of the loss experience
#'
#' @description The weight of the experience rating against the exposure rating, by
#'   limited fluctuation credibility: \eqn{Z = \min(1, \sqrt{n / n_{full}})} for
#'   \eqn{n} losses, with \eqn{n_{full}} the number of losses for full credibility.
#'   The blended rate is \eqn{Z} times the experience rate plus \eqn{1 - Z} times the
#'   exposure rate.
#' @param n The number of losses in the layer (or above the threshold).
#' @param full The number of losses for full credibility; the default, 1082, gives
#'   the observed frequency within 5\% of the true one with a probability of 90\%
#'   for Poisson counts.
#' @return The credibility weight, between 0 and 1.
#' @family reinsurance pricing functions
#' @export
#' @examples
#' credibility_weight(25)
#' credibility_weight(25, full = 100)
credibility_weight <- function(n, full = 1082) {
  pricing_check_number(n, "n", min = 0, length_one = FALSE)
  pricing_check_number(full, "full", min = 0, strict_min = TRUE)
  pmin(1, sqrt(n / full))
}
