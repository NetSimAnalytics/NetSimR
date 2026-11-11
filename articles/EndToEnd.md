# From pricing to capital and reserves: a motor portfolio

## 1. Introduction

This vignette follows one motor portfolio through the work of a non-life
actuary: pricing its policies with generalised linear models, modelling
its annual claims and an excess of loss treaty that protects it,
measuring the capital it needs alongside a property line, reserving its
claims, and its IFRS 17 and Solvency II figures. Each step uses the
package’s functions, and each has a Shiny tool that does the same
without code (section 8).

``` r

library("NetSimR")
```

## 2. The policies

The data is a simulated year of 20,000 policies: the driver’s age, the
region, the vehicle, the share of the year each policy was in force, its
number of claims, and the cost of its claims. Claims are more frequent
for young drivers and in the North, and cost more for vans:

``` r

set.seed(2026)
n <- 20000
policies <- data.frame(
  age = sample(18:80, n, replace = TRUE),
  region = sample(c("North", "South", "East", "West"), n, replace = TRUE),
  vehicle = sample(c("car", "van", "motorcycle"), n, replace = TRUE, prob = c(0.7, 0.2, 0.1)),
  exposure = round(runif(n, 0.25, 1), 2)
)
frequency <- exp(-2.2 - 0.012 * (policies$age - 45) + 0.35 * (policies$region == "North") +
                 0.5 * (policies$vehicle == "motorcycle"))
policies$claims <- rpois(n, policies$exposure * frequency)
severity_mean <- 3000 * ifelse(policies$vehicle == "van", 1.4, 1)
policies$cost <- vapply(seq_len(n), function(i) {
  sum(rgamma(policies$claims[i], shape = 1.5, scale = severity_mean[i] / 1.5))
}, numeric(1))
head(policies)
```

    ##   age region vehicle exposure claims cost
    ## 1  46  South     car     0.77      0    0
    ## 2  74   West     van     0.88      0    0
    ## 3  50   West     van     0.33      0    0
    ## 4  55  North     car     0.50      0    0
    ## 5  62   East     car     0.64      0    0
    ## 6  64   East     car     0.97      0    0

## 3. Pricing the policies

The classic model is a Poisson GLM of the claim count, with the log of
the exposure as an offset, and a Gamma GLM of the average claim size,
weighted by the number of claims. A part of the policies is held out, to
check the models on policies they were not fitted to:

``` r

test <- sample(n, n / 5)
training <- policies[-test, ]
frequency_model <- glm(claims ~ offset(log(exposure)) + age + region + vehicle, family = poisson(), data = training)
with_claims <- training[training$claims > 0, ]
severity_model <- glm(cost / claims ~ age + region + vehicle, family = Gamma(link = "log"),
                      weights = claims, data = with_claims)
round(exp(coef(frequency_model)), 3)
```

    ##       (Intercept)               age       regionNorth       regionSouth 
    ##             0.207             0.988             1.288             0.874 
    ##        regionWest vehiclemotorcycle        vehiclevan 
    ##             1.101             1.660             0.898

``` r

round(exp(coef(severity_model)), 3)
```

    ##       (Intercept)               age       regionNorth       regionSouth 
    ##          3010.461             1.001             0.979             0.912 
    ##        regionWest vehiclemotorcycle        vehiclevan 
    ##             0.948             0.982             1.309

The frequency relativities find the young drivers, the North and the
motorcycles; the severity relativities find the vans, and little else. A
Tweedie GLM,
[`tweedie_family()`](https://netsimanalytics.github.io/NetSimR/reference/tweedie_family.md),
models the cost per policy in one step: a compound Poisson-Gamma
distribution with a mass at zero, for the many policies without a claim.
Its relativities combine the two:

``` r

tweedie_model <- glm(cost ~ offset(log(exposure)) + age + region + vehicle, family = tweedie_family(1.5),
                     data = training)
round(exp(coef(tweedie_model)), 3)
```

    ##       (Intercept)               age       regionNorth       regionSouth 
    ##           646.276             0.989             1.197             0.771 
    ##        regionWest vehiclemotorcycle        vehiclevan 
    ##             1.022             1.647             1.187

On the policies held out, a good model ranks the risks: sorted by their
predicted cost per unit of exposure and cut into five bands of the same
exposure, the actual cost should rise from the first band to the last,
and the predictions should follow it:

``` r

held_out <- policies[test, ]
predicted <- predict(tweedie_model, held_out, type = "response")
rate <- predicted / held_out$exposure
band <- cut(cumsum(held_out$exposure[order(rate)]) / sum(held_out$exposure), seq(0, 1, 0.2), labels = 1:5)
lift <- data.frame(
  band = levels(band),
  actual = tapply(held_out$cost[order(rate)], band, sum) / tapply(held_out$exposure[order(rate)], band, sum),
  predicted = tapply(predicted[order(rate)], band, sum) / tapply(held_out$exposure[order(rate)], band, sum)
)
lift
```

    ##   band   actual predicted
    ## 1    1 220.7138  259.3533
    ## 2    2 335.8976  329.3004
    ## 3    3 548.9395  388.2911
    ## 4    4 415.1352  468.1651
    ## 5    5 538.1689  621.6659

The GLM fitting tool does this on its Validation tab, with the Gini
coefficient, the Lorenz curve and a double lift chart against another
model, and suggests the terms by stepwise AIC or BIC or by the lasso.

## 4. The annual claims and a reinsurance layer

Next year the portfolio is expected to keep its 20,000 policies for a
full year each. Its expected claim count is the sum of the frequency
model’s predictions:

``` r

next_year <- policies
next_year$exposure <- 1
claim_count <- sum(predict(frequency_model, next_year, type = "response"))
claim_count
```

    ## [1] 2579.098

The claim sizes are fitted with a Log-Normal distribution, by maximum
likelihood (the distribution fitting tool compares it with the Gamma,
Weibull and other fits, by the Kolmogorov-Smirnov distance, the
Anderson-Darling statistic and a Q-Q plot):

``` r

# the policies with one claim give the size of each claim
sizes <- training$cost[training$claims == 1]
meanlog <- mean(log(sizes))
sdlog <- sqrt(mean((log(sizes) - meanlog)^2))
c(meanlog = meanlog, sdlog = sdlog)
```

    ##   meanlog     sdlog 
    ## 7.7001608 0.9383796

[`simulate_claims()`](https://netsimanalytics.github.io/NetSimR/reference/simulate_claims.md)
simulates 5,000 years of claims. An excess of loss layer of 100,000 in
excess of 50,000, each and every loss, with two reinstatements, protects
the portfolio against its rare large claims; the results are the gross
and the ceded claims of each year, and what is retained is the
difference:

``` r

years <- simulate_claims(5000, frequency = "Poisson", frequency_params = claim_count,
                         severity = "LogNormal", severity_params = c(meanlog, sdlog),
                         eel_layer = "limited", eel_deductible = 50000, eel_limit = 100000,
                         eel_reinstatements = 2, seed = 1)
years$retained <- years$gross_claims - years$total_claims
summary_of <- function(x) round(c(mean = mean(x), sd = sd(x), quantile(x, c(0.5, 0.9, 0.99, 0.995))))
rbind(gross = summary_of(years$gross_claims), ceded = summary_of(years$total_claims),
      retained = summary_of(years$retained))
```

    ##             mean     sd     50%     90%     99%   99.5%
    ## gross    8850543 271331 8850126 9194773 9499484 9580611
    ## ceded      17868  25133    7586   52060  108596  119138
    ## retained 8832675 267299 8833968 9173311 9482781 9538298

The layer takes a small part of the mean, but a larger part of the bad
years. (A run of many more years can use `parallel = "auto"`, which runs
it on worker processes when that is faster.) Its expected loss,
`mean(years$total_claims)`, is the starting point for its price: the
reinsurance pricing tool and the vignette “Pricing an excess of loss
layer” price a layer from the cedant’s large losses and exposure, and
[`technical_premium()`](https://netsimanalytics.github.io/NetSimR/reference/technical_premium.md)
loads the expected loss for the reinstatements, expenses and profit.

Is the layer worth buying?
[`compare_reinsurance()`](https://netsimanalytics.github.io/NetSimR/reference/compare_reinsurance.md)
puts the same simulated claims through several programmes and sets what
each costs, the margin paid to the reinsurers above the expected
recoveries, against the capital it saves, the drop in the 1 in 200 bad
year’s shortfall from an average year. Here the layer and a 25% quota
share, on a premium that expects a 70% loss ratio:

``` r

gross_premium <- round(claim_count * exp(meanlog + sdlog^2 / 2) / 0.7, -4)
options <- list(`Layer 100k xs 50k` = list(xl = list(deductible = 50000, limit = 100000, reinstatements = 2)),
                `Quota share 25%` = list(quota_share = 0.25, commission = 0.30))
compare_reinsurance("Poisson", claim_count, "LogNormal", c(meanlog, sdlog), options,
                    premium = gross_premium, n = 5000, seed = 1)
```

    ## Reinsurance programmes, 5,000 simulated years, capital at 99.5%
    ## 
    ##          programme     ceded margin mean_result capital  relief  value
    ##     No reinsurance         0      0   3,787,185 723,053       0      0
    ##  Layer 100k xs 50k    18,506  6,169   3,781,016 715,154   7,899 -5,379
    ##    Quota share 25% 2,213,204 -1,204   3,788,389 542,290 180,763 19,280

The value column sets 10% a year of the capital saved against the
margin: positive when the capital relief is worth more than the
reinsurance costs. The reinsurance pricing tool compares up to five
programmes on its Programmes tab, a surplus treaty on its risk profile
among them;
[`compare_reinsurance()`](https://netsimanalytics.github.io/NetSimR/reference/compare_reinsurance.md)
takes one too, with the sums insured of the risks.

## 5. Capital with a property line

The insurer also writes property, whose annual losses come from a few
large Pareto claims (the severity parameters are alpha, then the
threshold x_m). The two lines are not independent: a severe winter
raises both.
[`simulate_portfolio()`](https://netsimanalytics.github.io/NetSimR/reference/simulate_portfolio.md)
simulates each line on its own and joins them with a copula, here a rank
correlation of about 0.3. With a Gaussian copula the lines rarely have
their worst years together; a t copula with few degrees of freedom makes
that more likely:

``` r

lines <- list(
  motor = years$retained,
  property = list(frequency = "Poisson", frequency_params = 8,
                  severity = "Pareto", severity_params = c(1.9, 50000))
)
gaussian <- simulate_portfolio(lines, correlation = 0.3, n = 5000, seed = 1)
gaussian
```

    ## Portfolio of 2 lines, 5,000 simulations, Gaussian copula
    ## 
    ##      line      mean      sd  VaR 99.5% TVaR 99.5%    capital share
    ##     motor 8,832,675 267,299  9,538,298  9,613,129  9,102,073 0.644
    ##  property   839,704 533,174  3,110,743  5,048,140  5,033,016 0.356
    ##     total 9,672,379 655,486 12,215,362 14,135,089 14,135,089 1.000
    ## 
    ## Diversification benefit: 433,679 (3.4% of the lines' VaR of 12,649,041)

``` r

t_copula <- simulate_portfolio(lines, correlation = 0.3, copula = "t", df = 3, n = 5000, seed = 1)
rbind(gaussian = gaussian$diversification, t = t_copula$diversification)
```

    ##          stand_alone_var total_var  benefit benefit_share
    ## gaussian        12649041  12215362 433679.3    0.03428555
    ## t               12649041  12361213 287828.2    0.02275494

The capital column allocates the total’s tail value at risk to the lines
(co-TVaR): each line’s mean loss in the years that make up the total’s
tail. The lines’ capitals add up to the total’s TVaR, and the t copula,
with its joint extremes, leaves less diversification.

## 6. Reserving the claims

`simulated_motor_claims` holds the paid and incurred claims of a motor
portfolio by accident year and development year, with its earned
premiums. The chain ladder of the paid claims, with Mack’s standard
errors:

``` r

paid <- as_triangle(simulated_motor_claims, "accident_year", "development_year", "paid", cumulative = FALSE)
premium <- unique(simulated_motor_claims[c("accident_year", "earned_premium")])$earned_premium
cl <- chain_ladder(paid)
cl$total
```

    ##       latest    developed     ultimate         ibnr           se           cv 
    ## 6.929459e+07 8.218718e-01 8.431314e+07 1.501854e+07 9.674698e+05 6.441835e-02 
    ##   process_se parameter_se 
    ## 7.722744e+05 5.827435e+05

Before relying on it, the chain ladder is checked against the latest
calendar period: the chain ladder of the triangle without its latest
diagonal predicts each accident year’s development in that period, which
is compared with what was paid. A standardised difference beyond about 2
either way is worth a look, as for 2019 here, which paid almost twice
what was expected:

``` r

ave <- actual_vs_expected(paid)
ave$by_origin[, c("origin", "expected", "actual", "actual_to_expected", "z")]
```

    ##    origin   expected  actual actual_to_expected            z
    ## 1    2014         NA    7638                 NA           NA
    ## 2    2015   10552.13    2292          0.2172074 -0.325261817
    ## 3    2016   73861.44   70738          0.9577121 -0.030780776
    ## 4    2017   70493.97   90235          1.2800385  0.626829751
    ## 5    2018   97916.02   98054          1.0014092  0.004688949
    ## 6    2019  153217.89  285080          1.8606182  3.869266302
    ## 7    2020  290745.20  264490          0.9096969 -0.422598690
    ## 8    2021  561656.54  445225          0.7926997 -1.076669037
    ## 9    2022  693113.36  699512          1.0092317  0.063289432
    ## 10   2023 1226081.11 1001064          0.8164745 -0.995559786
    ## 11   2024 2414826.02 2714510          1.1241017  1.010564208
    ## 12   2025         NA      NA                 NA           NA

Clark’s growth curves give a second view: a log-logistic curve of the
share paid by age, fitted by maximum likelihood, with an ultimate for
each accident year (the LDF method) or one loss ratio on the premiums
(the Cape Cod method). The Cape Cod method has far fewer parameters, so
its standard error is lower. Both reserve more than the chain ladder
here: the curve keeps developing after the triangle’s last development
year, up to the truncation at 20 years, while the chain ladder has no
tail:

``` r

ldf <- clark_growth(paid, truncation = 20)
cape_cod <- clark_growth(paid, premium = premium, method = "cape cod", truncation = 20)
rbind(chain_ladder = cl$total[c("ibnr", "se")], clark_ldf = ldf$total[c("ibnr", "se")],
      clark_cape_cod = cape_cod$total[c("ibnr", "se")])
```

    ##                    ibnr        se
    ## chain_ladder   15018544  967469.8
    ## clark_ldf      22069768 1611271.0
    ## clark_cape_cod 21423664 1428826.4

The chain ladder of nominal payments assumes the future inflation is the
past’s. If claims inflation was 3% a year but is expected to be 6%, the
past payments are restated in today’s money, developed, and the future
payments inflated at 6%:

``` r

inflated <- inflation_adjusted_chain_ladder(paid, past = 0.03, future = 0.06)
inflated$total
```

    ##           latest         ultimate             ibnr       ibnr_today 
    ##         69294594         85419250         16124656         13890611 
    ## future_inflation 
    ##          2234045

The reserve rises by the extra inflation on the payments still to come,
most of all for the recent accident years.

## 7. IFRS 17 and Solvency II

Under IFRS 17 the liability for incurred claims is the best estimate of
the future payments, discounted, plus a risk adjustment for their
uncertainty.
[`discount_reserves()`](https://netsimanalytics.github.io/NetSimR/reference/discount_reserves.md)
takes the chain ladder’s payments by future calendar year and discounts
them with a yield curve, here rising from 3% to 3.8% over five years:

``` r

be <- discount_reserves(cl, yield_curve = c(0.030, 0.032, 0.034, 0.036, 0.038))
be
```

    ## Reserve 15,018,544, discounted 14,035,937 (a discount of 982,607, mean term 2.06 years)

The risk adjustment can be a percentile of the bootstrapped reserves,
scaled for the discounting, or the cost of the capital held until the
claims are paid, as Solvency II’s risk margin is found; with the
bootstrap, the cost of capital method also gives the confidence level
IFRS 17 asks to disclose:

``` r

boot <- bootstrap_chain_ladder(paid, n = 2000, seed = 1)
percentile <- risk_adjustment("percentile", boot = boot, level = 0.75, discounted = be)
coc <- risk_adjustment("cost of capital", boot = boot, discounted = be,
                       capital = 3 * 0.09 * be$total[["discounted"]], coc_rate = 0.06)
rbind(percentile = c(percentile$risk_adjustment, percentile$confidence_level),
      cost_of_capital = c(coc$risk_adjustment, coc$confidence_level))
```

    ##                     [,1]  [,2]
    ## percentile      605291.1 0.750
    ## cost_of_capital 537018.9 0.729

The capital of the cost of capital method was the reserve risk of
Solvency II’s standard formula: three times the 9% standard deviation of
motor vehicle liability (segment 1) times the best estimate.
[`sii_premium_reserve_risk()`](https://netsimanalytics.github.io/NetSimR/reference/sii_premium_reserve_risk.md)
gives the standard formula’s premium and reserve risk of several
segments together, with their correlations; here the motor reserves with
next year’s motor premium, and a property segment:

``` r

volumes <- data.frame(segment = c(1, 4), premium = c(gross_premium, 1.2e6),
                      reserve = c(be$total[["discounted"]], 0.4e6))
sii <- sii_premium_reserve_risk(volumes)
sii$total
```

    ##          volume              sd         capital      standalone diversification 
    ##    2.827594e+07    7.858302e-02    6.666026e+06    6.929133e+06    2.631076e+05

The parameters are those of the Delegated Regulation (EU) 2015/35 as
amended in 2019, in `sii_nonlife_segments`. The reserving tool shows the
discounting, the risk adjustment and the reserve risk on its IFRS 17 &
Solvency II tab, and the portfolio tool the standard formula of every
segment.

## 8. The same in the tools

Each step has a Shiny tool, which runs on the computer or in the browser
on the package’s website:

- [`run_shiny_glm_fitting_tool()`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_glm_fitting_tool.md):
  the GLMs of section 3, with the Tweedie family, splines, holdout
  validation, lift and double lift charts, variable selection, and the
  rate table.
- [`run_shiny_distribution_fitting_tool()`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_distribution_fitting_tool.md):
  the claim size fits of section 4, sent on to the simulator and to the
  reinsurance pricing tool’s Programmes tab, with a report.
- [`run_shiny_simulator()`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_simulator.md):
  the simulation and the layer of section 4, with a report.
- [`run_shiny_reinsurance_pricing_tool()`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_reinsurance_pricing_tool.md):
  the price of the layer, and the comparison of programmes on the fitted
  claims, with a report.
- [`run_shiny_portfolio_tool()`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_portfolio_tool.md):
  the portfolio of section 5, with a correlation matrix, charts and a
  report, and the standard formula of section 7.
- [`run_shiny_reserving_tool()`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_reserving_tool.md):
  the chain ladder, actual vs expected, Clark’s curves and the claims
  inflation of section 6, and the discounting and risk adjustment of
  section 7, with a report.
