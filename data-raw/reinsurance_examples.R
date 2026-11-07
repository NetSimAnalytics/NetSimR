# The example data sets of the reinsurance pricing functions and tool: a simulated
# portfolio's large losses, its subject premiums by year, and property and casualty
# risk profiles. Run from the package root: Rscript data-raw/reinsurance_examples.R
out <- if (length(commandArgs(TRUE)) > 0) commandArgs(TRUE)[1] else "data"

set.seed(20261107)
years <- 2015:2024
rating_year <- 2025
inflation <- 0.04
# the subject premium grows with the portfolio; the rate index brings each year's
# premium to the 2025 rates (rates fell from 2015 and rose again from 2020)
premium <- round(c(41.2, 42.5, 43.1, 44.8, 46.0, 47.9, 50.6, 53.2, 55.8, 58.4) * 1e6, -3)
rate_index <- c(0.94, 0.96, 0.99, 1.02, 1.05, 1.06, 1.04, 1.03, 1.01, 1.00)
# the development of the layer's losses: the recent years are not yet fully reported
development <- c(1.00, 1.00, 1.00, 1.01, 1.02, 1.04, 1.08, 1.15, 1.30, 1.65)
xl_premium <- data.frame(year = years, premium = premium, rate_index = rate_index, development = development)

# large losses: about 0.16 per million of on-level premium above 500,000 in 2025
# money, Pareto with alpha 1.7; the recent years lack the losses not reported yet
threshold <- 5e5
alpha <- 1.7
expected <- 0.16 * premium * rate_index / 1e6 / development
counts <- stats::rpois(length(years), expected)
losses <- do.call(rbind, lapply(seq_along(years), function(i) {
  if (counts[i] == 0) return(NULL)
  at_rating <- threshold * stats::runif(counts[i])^(-1 / alpha)
  # a loss of each year in its own money; some fall below the reporting threshold
  amount <- at_rating / (1 + inflation)^(rating_year - years[i])
  data.frame(year = years[i], amount = amount)
}))
losses <- losses[losses$amount >= threshold, ]
losses$amount <- round(pmin(losses$amount, 4.5e7), -2)
losses <- losses[order(losses$year, -losses$amount), ]
losses$claim <- sprintf("C%d-%02d", losses$year, stats::ave(losses$year, losses$year, FUN = seq_along))
xl_large_losses <- data.frame(claim = losses$claim, year = losses$year, amount = losses$amount)
rownames(xl_large_losses) <- NULL

# a property risk profile: bands of sum insured with the number of risks, their
# average sum insured and their premium
lower <- c(0, 2.5e5, 5e5, 1e6, 2.5e6, 5e6, 1e7, 2.5e7, 5e7)
upper <- c(2.5e5, 5e5, 1e6, 2.5e6, 5e6, 1e7, 2.5e7, 5e7, 1e8)
risks <- c(42000, 18500, 9200, 3800, 1150, 420, 160, 48, 12)
sum_insured <- round((lower + upper) / 2 * c(1.1, 1.02, 1, 0.98, 0.97, 0.96, 0.94, 0.93, 0.92), -3)
rate <- c(1.9, 1.7, 1.55, 1.4, 1.3, 1.2, 1.1, 1.0, 0.9) / 1000
# compact labels, such as "250k" and "2.5m"
label <- function(x) ifelse(x >= 1e6, paste0(x / 1e6, "m"), ifelse(x > 0, paste0(x / 1e3, "k"), "0"))
xl_property_profile <- data.frame(
  band = paste(label(lower), "to", label(upper)), lower = lower, upper = upper, risks = risks,
  sum_insured = sum_insured, premium = round(risks * sum_insured * rate, -3)
)

# a casualty (liability) risk profile: the premium written at each policy limit
xl_casualty_profile <- data.frame(
  policy_limit = c(5e5, 1e6, 2e6, 3e6, 5e6, 1e7),
  premium = c(8.4, 14.6, 12.1, 6.8, 4.9, 2.2) * 1e6
)

for (name in c("xl_large_losses", "xl_premium", "xl_property_profile", "xl_casualty_profile")) {
  e <- new.env()
  assign(name, get(name), envir = e)
  save(list = name, envir = e, file = file.path(out, paste0(name, ".rda")), compress = "xz", version = 2)
}
