#include <Rcpp.h>
using namespace Rcpp;

// Helper to calculate F-statistic for K-sample (like one-way ANOVA)
double calculate_f_stat(NumericVector value, IntegerVector group, int n_groups) {
  int n = value.size();
  
  // Grand mean
  double grand_mean = mean(value);
  
  // Group statistics
  std::vector<double> group_sums(n_groups, 0.0);
  std::vector<int> group_counts(n_groups, 0);
  
  for(int i = 0; i < n; i++) {
    // group is 1-based from R factor
    int g_idx = group[i] - 1; 
    group_sums[g_idx] += value[i];
    group_counts[g_idx]++;
  }
  
  double ssb = 0.0; // Sum of Squares Between
  double ssw = 0.0; // Sum of Squares Within
  
  for(int g = 0; g < n_groups; g++) {
    if(group_counts[g] > 0) {
      double group_mean = group_sums[g] / group_counts[g];
      ssb += group_counts[g] * pow(group_mean - grand_mean, 2);
    }
  }
  
  for(int i = 0; i < n; i++) {
    int g_idx = group[i] - 1;
    double group_mean = group_sums[g_idx] / group_counts[g_idx];
    ssw += pow(value[i] - group_mean, 2);
  }
  
  int df_between = n_groups - 1;
  int df_within = n - n_groups;
  
  if (df_within == 0 || ssw == 0) return 0.0;
  
  double msb = ssb / df_between;
  double msw = ssw / df_within;
  
  return msb / msw;
}

// [[Rcpp::export]]
List perm_test_ksample(NumericVector value, IntegerVector group, int n_perms = 999) {
  // Determine number of groups from the integer vector (assuming 1..K)
  int n_groups = max(group);
  int n = value.size();
  
  double obs_stat = calculate_f_stat(value, group, n_groups);
  
  int count = 0;
  NumericVector perm_val = clone(value);
  
  // Use a fixed seed in R or set seed here? Rcpp usually respects set.seed in R
  
  for(int i = 0; i < n_perms; i++) {
    // Shuffle values
    perm_val = sample(perm_val, n, false);
    
    double perm_stat = calculate_f_stat(perm_val, group, n_groups);
    if (perm_stat >= obs_stat) {
      count++;
    }
  }
  
  double p_val = (double)(count + 1) / (n_perms + 1);
  
  return List::create(Named("statistic") = obs_stat,
                      Named("p.value") = p_val);
}

// [[Rcpp::export]]
List perm_test_2sample(NumericVector x, NumericVector y, int n_perms = 999) {
  int n_x = x.size();
  int n_y = y.size();
  int n_total = n_x + n_y;
  
  double mean_x = mean(x);
  double mean_y = mean(y);
  double obs_diff = std::abs(mean_x - mean_y);
  
  NumericVector combined(n_total);
  for(int i = 0; i < n_x; i++) combined[i] = x[i];
  for(int i = 0; i < n_y; i++) combined[n_x + i] = y[i];
  
  int count = 0;
  NumericVector permuted = clone(combined);
  
  for(int i = 0; i < n_perms; i++) {
    permuted = sample(combined, n_total, false);
    
    double sum_x = 0;
    for(int j = 0; j < n_x; j++) sum_x += permuted[j];
    double m_x = sum_x / n_x;
    
    double sum_y = 0;
    for(int j = 0; j < n_y; j++) sum_y += permuted[n_x + j];
    double m_y = sum_y / n_y;
    
    if (std::abs(m_x - m_y) >= obs_diff) {
      count++;
    }
  }
  
  double p_val = (double)(count + 1) / (n_perms + 1);
  
  return List::create(Named("statistic") = obs_diff,
                      Named("p.value") = p_val);
}
