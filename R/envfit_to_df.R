# Because the envfit can not be output as matrix or data.frame directly,
# we should convert envfit as data.frame,
# generally we consider the first two axes of RDA.
# Therefore, we choose the first two axes of envfit
# Here, env_obj indicates the result of envfit. In this case, it's the res_envfit.
# r2_dig is the significant figure of R2
# p_dig is the significant figure of p value
envfit_to_df <- function(env_obj,
                         r2_dig = 6,
                         p_dig = 3) {
  r2_fmt <- as.character(paste('%.', r2_dig, 'f', sep = ''))
  p_fmt <- as.character(paste('%.', p_dig, 'f', sep = ''))
  tibble(
    # the name of explainary variables
    factor = names(env_obj$vectors$r),
    # list or vector of R2
    r2 = env_obj$vectors$r,
    # list or vector of p values
    pvals = env_obj$vectors$pvals
  ) %>%
    # generate significant levels by p values
    mutate(sig = case_when(
      pvals <= 0.001 ~ '***',
      pvals <= 0.01 ~ '**',
      pvals <= 0.05 ~ '*',
      TRUE ~ ' '
    )) %>%
    # format the significant figure by format definition before.
    mutate(pvals = sprintf('%.3f', pvals),
           r2 = sprintf(r2_fmt, r2))
}