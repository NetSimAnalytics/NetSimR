# The example data sets of the GLM and distribution fitting tools: a simulated year of
# motor policies (claim counts, costs and rating factors) and simulated liability claims
# capped at their policy limits. Run from the package root: Rscript data-raw/policy_examples.R
out <- if (length(commandArgs(TRUE)) > 0) commandArgs(TRUE)[1] else "data"

set.seed(20261108)
n <- 10000
age <- sample(18:85, n, replace = TRUE, prob = stats::dnorm(18:85, 45, 16))
region <- sample(c("North", "South", "East", "West"), n, replace = TRUE, prob = c(0.3, 0.25, 0.25, 0.2))
vehicle <- sample(c("car", "van", "motorcycle"), n, replace = TRUE, prob = c(0.72, 0.18, 0.10))
vehicle_value <- round(stats::rlnorm(n, log(ifelse(vehicle == "van", 22000, ifelse(vehicle == "motorcycle", 9000, 16000))), 0.45), -2)
exposure <- round(pmin(1, stats::rbeta(n, 3, 0.8) + 0.05), 2)
# claim frequency: highest for the young, falling to the middle ages and rising a little for
# the oldest drivers; more in the North and for motorcycles
age_effect <- 0.9 * exp(-(age - 18) / 7) + 0.0002 * pmax(age - 60, 0)^2
frequency <- exp(-2.3 + age_effect + 0.3 * (region == "North") - 0.1 * (region == "West") +
                   0.45 * (vehicle == "motorcycle") + 0.08 * log(vehicle_value / 16000))
claim_count <- stats::rpois(n, exposure * frequency)
# claim sizes: Gamma, larger for vans and dearer vehicles
mean_size <- 2800 * ifelse(vehicle == "van", 1.35, 1) * (vehicle_value / 16000)^0.3
claim_cost <- round(vapply(seq_len(n), function(i) {
  sum(stats::rgamma(claim_count[i], shape = 1.4, scale = mean_size[i] / 1.4))
}, numeric(1)), 2)
motor_policies <- data.frame(
  policy_id = sprintf("P%05d", seq_len(n)), age = age, region = region, vehicle = vehicle,
  vehicle_value = vehicle_value, exposure = exposure, claim_count = claim_count, claim_cost = claim_cost,
  stringsAsFactors = FALSE
)

# liability claims: LogNormal losses, paid up to the policy's limit, so the largest are cut
# at the limits; four accident years
m <- 2500
loss <- stats::rlnorm(m, 10.2, 1.5)
policy_limit <- sample(c(1e5, 2.5e5, 5e5, 1e6), m, replace = TRUE, prob = c(0.35, 0.3, 0.25, 0.1))
liability_claims <- data.frame(
  claim_id = sprintf("L%05d", seq_len(m)), accident_year = sample(2021:2024, m, replace = TRUE),
  amount = round(pmin(loss, policy_limit), 2), policy_limit = policy_limit
)

save(motor_policies, file = file.path(out, "motor_policies.rda"), compress = "xz")
save(liability_claims, file = file.path(out, "liability_claims.rda"), compress = "xz")
cat("motor policies:", nrow(motor_policies), "rows,", sum(claim_count), "claims;",
    "liability claims:", m, "of which", sum(loss >= policy_limit), "at their limits\n")
