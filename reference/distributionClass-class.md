# The class of the distribution objects

Each object describes one frequency or severity distribution of the
simulator: its parameters (ids used as app input ids, labels, allowed
ranges and whether they must be whole numbers) and the functions the
simulator needs. `simulate_func` draws values; `moments_func` gives the
mean and standard deviation. Severity distributions also have
`survival_func` and `tail_quantile_func` (the upper-tail probability and
its inverse), used to splice Pareto tails and to draw only the claims
that reach a layer, and optionally `sum_func`, which draws the total of
a given number of claims in one step. Frequency distributions may have
`split_func`, which draws the number of claims and how many of them are
large. Allowed ranges are `param_min_values` and `param_max_values` (NA
for no bound), with `param_min_strict` and `param_max_strict` marking
bounds the value may not equal. `param_display_order` is the order the
app shows the parameters' inputs in, as positions in `paramIDs` (empty
for that order): the Pareto shows its minimum, x_m, before alpha, the
order actuaries enter them in, while its parameters stay `c(alpha, x_m)`
wherever they are passed as a vector. `param_amounts` marks the
parameters that are amounts of money (empty for none), which the app
shows with thousands separators.
