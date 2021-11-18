# test file

library(subscanr)
# out <- get_subscan_events(nobs = 150000, network = 'Karura', module = 'dex', call = 'Swap')
out <- get_subscan_events(nobs = 1300, network = 'Karura', module = 'dex', call = 'Swap')
out


out[, swap_price_1 := amt_0 / amt_1]
out[, swap_price_2 := amt_1 / amt_2]
out[, swap_price_3 := amt_2 / amt_3]
out[, swap_value := amt_1 * swap_price_1]
out[, swap_fee := .003 * swap_value] # need to * price(id_0)

min(out$time)
max(out$time)



out[, .N]
out[, .N, by = id_0]


