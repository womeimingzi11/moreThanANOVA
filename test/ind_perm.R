library(coin)

map2(analysis_method, df_data[,-1], function(x,y){
  if(x == 'Nonparametric tests'){
    # if(non_p_method == 'u-test'){
    #   kruskal.test(y, df_data[,1][[1]], na.action = na.omit) %>%
    #     broom::glance()
    # } else {
      independence_test(y ~ as.factor(df_data[,1][[1]])) %>%
      glance_coin()
    } else {
    aov(y~df_data[,1][[1]], na.action = na.omit) %>%
      broom::glance()
  }}) %>%
  bind_rows()

aov(y ~ df_data[, 1][[1]], na.action = na.omit) %>%
  broom::glance() %>%
  select('statistic', 'p.value', 'df') %>%
  mutate(method = 'ANOVA')
