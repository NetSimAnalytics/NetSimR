# Builds the example data sets of the reserving functions and application:
#   raa_triangle and taylor_ashe_triangle, two published triangles,
#   quarg_mack_paid and quarg_mack_incurred, the paid and incurred triangles of the
#   Munich chain ladder paper, and simulated_motor_claims, a simulated portfolio
#   with premiums and paid and incurred claims.
# Run from the package root: Rscript data-raw/reserving_examples.R

# Reinsurance Association of America, as in Mack (1994), "Measuring the variability
# of chain ladder reserve estimates": cumulative incurred claims, accident years 1981
# to 1990, in thousands.
raa <- c(5012, 8269, 10907, 11805, 13539, 16181, 18009, 18608, 18662, 18834,
         106, 4285, 5396, 10666, 13782, 15599, 15496, 16169, 16704, NA,
         3410, 8992, 13873, 16141, 18735, 22214, 22863, 23466, NA, NA,
         5655, 11555, 15766, 21266, 23425, 26083, 27067, NA, NA, NA,
         1092, 9565, 15836, 22169, 25955, 26180, NA, NA, NA, NA,
         1513, 6445, 11702, 12935, 15852, NA, NA, NA, NA, NA,
         557, 4020, 10946, 12314, NA, NA, NA, NA, NA, NA,
         1351, 6947, 13112, NA, NA, NA, NA, NA, NA, NA,
         3133, 5395, NA, NA, NA, NA, NA, NA, NA, NA,
         2063, NA, NA, NA, NA, NA, NA, NA, NA, NA)
raa_triangle <- matrix(raa, 10, 10, byrow = TRUE,
                       dimnames = list(origin = 1981:1990, development = 1:10))

# Taylor and Ashe (1983), as in Mack (1993): cumulative paid claims, ten accident
# years.
taylor_ashe <- c(357848, 1124788, 1735330, 2218270, 2745596, 3319994, 3466336, 3606286, 3833515, 3901463,
                 352118, 1236139, 2170033, 3353322, 3799067, 4120063, 4647867, 4914039, 5339085, NA,
                 290507, 1292306, 2218525, 3235179, 3985995, 4132918, 4628910, 4909315, NA, NA,
                 310608, 1418858, 2195047, 3757447, 4029929, 4381982, 4588268, NA, NA, NA,
                 443160, 1136350, 2128333, 2897821, 3402672, 3873311, NA, NA, NA, NA,
                 396132, 1333217, 2180715, 2985752, 3691712, NA, NA, NA, NA, NA,
                 440832, 1288463, 2419861, 3483130, NA, NA, NA, NA, NA, NA,
                 359480, 1421128, 2864498, NA, NA, NA, NA, NA, NA, NA,
                 376686, 1363294, NA, NA, NA, NA, NA, NA, NA, NA,
                 344014, NA, NA, NA, NA, NA, NA, NA, NA, NA)
taylor_ashe_triangle <- matrix(taylor_ashe, 10, 10, byrow = TRUE,
                               dimnames = list(origin = 1:10, development = 1:10))

# A simulated motor portfolio: twelve accident years of earned premium, a loss ratio
# for each year around 68%, and payments that follow a development pattern, each an
# over-dispersed draw (gamma, with a variance 20,000 times its mean) around it.
set.seed(2025)
years <- 2014:2025
premium <- round(8e6 * 1.04^(seq_along(years) - 1) * exp(stats::rnorm(length(years), 0, 0.03)), -3)
loss_ratio <- 0.68 * exp(stats::rnorm(length(years), 0, 0.06))
paid_share <- c(0.31, 0.57, 0.73, 0.83, 0.895, 0.935, 0.962, 0.978, 0.988, 0.994, 0.997, 0.999)
pattern <- diff(c(0, paid_share))
phi <- 20000
rows <- list()
for (i in seq_along(years)) {
  for (j in seq_len(length(years) - i + 1)) {
    mean_paid <- premium[i] * loss_ratio[i] * pattern[j]
    paid <- round(stats::rgamma(1, shape = mean_paid / phi, scale = phi))
    rows[[length(rows) + 1]] <- data.frame(accident_year = years[i], development_year = j, paid = paid,
                                           earned_premium = premium[i])
  }
}
simulated_motor_claims <- do.call(rbind, rows)
rownames(simulated_motor_claims) <- NULL

# Incurred claims: the paid claims plus case reserves for the share of each year's
# expected claims reported but not yet paid, by a reporting pattern. Each accident year's
# case reserves are set high or low by a factor of its own, which stays with it as it
# develops, and each estimate varies a little more; drawn after the payments, so that
# these are as before.
reported_share <- c(0.70, 0.86, 0.93, 0.965, 0.983, 0.992, 0.996, 0.998, 0.999, 1, 1, 1)
reserving_strength <- exp(stats::rnorm(length(years), 0, 0.12))
cumulative_paid <- ave(simulated_motor_claims$paid, simulated_motor_claims$accident_year, FUN = cumsum)
i <- match(simulated_motor_claims$accident_year, years)
j <- simulated_motor_claims$development_year
expected <- premium[i] * loss_ratio[i]
case <- pmax(0, expected * (reported_share[j] - paid_share[j])) * reserving_strength[i] *
  exp(stats::rnorm(nrow(simulated_motor_claims), 0, 0.05))
cumulative_incurred <- round(cumulative_paid + case)
simulated_motor_claims$incurred <- ave(cumulative_incurred, simulated_motor_claims$accident_year,
                                       FUN = function(x) c(x[1], diff(x)))
simulated_motor_claims <- simulated_motor_claims[c("accident_year", "development_year", "paid", "incurred",
                                                   "earned_premium")]

# Quarg and Mack (2004): cumulative paid and incurred claims, seven accident years.
quarg_mack_paid <- matrix(c(576, 1804, 1970, 2024, 2074, 2102, 2131,
                            866, 1948, 2162, 2232, 2284, 2348, NA,
                            1412, 3758, 4252, 4416, 4494, NA, NA,
                            2286, 5292, 5724, 5850, NA, NA, NA,
                            1868, 3778, 4648, NA, NA, NA, NA,
                            1442, 4010, NA, NA, NA, NA, NA,
                            2044, NA, NA, NA, NA, NA, NA), 7, 7, byrow = TRUE,
                          dimnames = list(origin = 1:7, development = 1:7))
quarg_mack_incurred <- matrix(c(978, 2104, 2134, 2144, 2174, 2182, 2174,
                                1844, 2552, 2466, 2480, 2508, 2454, NA,
                                2904, 4354, 4698, 4600, 4644, NA, NA,
                                3502, 5958, 6070, 6142, NA, NA, NA,
                                2812, 4882, 4852, NA, NA, NA, NA,
                                2642, 4406, NA, NA, NA, NA, NA,
                                5022, NA, NA, NA, NA, NA, NA), 7, 7, byrow = TRUE,
                              dimnames = list(origin = 1:7, development = 1:7))

save(raa_triangle, file = "data/raa_triangle.rda", compress = "xz", version = 2)
save(taylor_ashe_triangle, file = "data/taylor_ashe_triangle.rda", compress = "xz", version = 2)
save(simulated_motor_claims, file = "data/simulated_motor_claims.rda", compress = "xz", version = 2)
save(quarg_mack_paid, file = "data/quarg_mack_paid.rda", compress = "xz", version = 2)
save(quarg_mack_incurred, file = "data/quarg_mack_incurred.rda", compress = "xz", version = 2)
