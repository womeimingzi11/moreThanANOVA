glance_coin <- function(coin_obj) {
  tibble(statistic = statistic(coin_obj),
         pvalue = pvalue(coin_obj) %>% as.character(),
         df = "",
         method = "Monte Carlo Permutation test")
}


