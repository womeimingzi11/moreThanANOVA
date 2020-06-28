glance_coin <- function(coin_obj) {
  tibble(statistic = statistic(coin_obj),
         p.value = pvalue(coin_obj) %>% as.numeric(),
         df = "",
         method = "Monte Carlo Permutation test")
}
