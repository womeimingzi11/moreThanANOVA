#include <Rcpp.h>
using namespace Rcpp;

// Helper to calculate F-statistic for K-sample (like one-way ANOVA)
// Used for the observed statistic return value (we want the real F, not just the index)
double calculate_f_stat(NumericVector value, IntegerVector group, int n_groups) {
  int n = value.size();
  
  double grand_mean = mean(value);
  
  std::vector<double> group_sums(n_groups, 0.0);
  std::vector<int> group_counts(n_groups, 0);
  
  for(int i = 0; i < n; i++) {
    int g_idx = group[i] - 1; 
    group_sums[g_idx] += value[i];
    group_counts[g_idx]++;
  }
  
  double ssb = 0.0;
  double ssw = 0.0;
  
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
  int n_groups = max(group);
  int n = value.size();
  
  // 1. Calculate Observed F-statistic (to return to user)
  double obs_f = calculate_f_stat(value, group, n_groups);
  
  // 2. Prepare for Optimization (Monotonic Statistic T)
  // T = Sum( (Sum_i^2) / n_i )
  // We only need to check if T_perm >= T_obs
  
  // Pre-calculate n_i
  std::vector<int> group_counts(n_groups, 0);
  for(int i = 0; i < n; i++) group_counts[group[i] - 1]++;
  
  // Calculate Observed T
  std::vector<double> obs_group_sums(n_groups, 0.0);
  for(int i = 0; i < n; i++) obs_group_sums[group[i] - 1] += value[i];
  
  double obs_t = 0.0;
  for(int g = 0; g < n_groups; g++) {
    if(group_counts[g] > 0) obs_t += pow(obs_group_sums[g], 2) / group_counts[g];
  }
  
  // Loop variables
  int count = 0;
  NumericVector perm_val = clone(value);
  std::vector<double> perm_group_sums(n_groups);
  
  // For safety with epsilon issues, we check >= with a small tolerance or just strict?
  // Standard permutation uses >=.
  
  for(int i = 0; i < n_perms; i++) {
    // Shuffle
    perm_val = sample(perm_val, n, false);
    
    // Reset sums
    std::fill(perm_group_sums.begin(), perm_group_sums.end(), 0.0);
    
    // Compute Group Sums ONLY
    for(int j = 0; j < n; j++) {
       // group is 1-based
       int g_idx = group[j] - 1;
       perm_group_sums[g_idx] += perm_val[j];
    }
    
    // Compute T Statistic
    double perm_t = 0.0;
    for(int g = 0; g < n_groups; g++) {
       if(group_counts[g] > 0) perm_t += pow(perm_group_sums[g], 2) / group_counts[g];
    }
    
    if (perm_t >= obs_t) {
      count++;
    }
  }
  
  double p_val = (double)(count + 1) / (n_perms + 1);
  
  return List::create(Named("statistic") = obs_f,
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
  
  // Optimization:
  // Diff = | MeanX - MeanY |
  // Let SumTotal = SumX + SumY (Constant)
  // MeanX = SumX / n_x
  // MeanY = (SumTotal - SumX) / n_y
  // So Diff is purely a function of SumX.
  // We calculate ObsDiff directly, then in loop calculate PermDiff derived from SumX.
  
  double sum_total = sum(x) + sum(y);
  
  NumericVector combined(n_total);
  for(int i = 0; i < n_x; i++) combined[i] = x[i];
  for(int i = 0; i < n_y; i++) combined[n_x + i] = y[i];
  
  int count = 0;
  NumericVector permuted = clone(combined);
  
  for(int i = 0; i < n_perms; i++) {
    permuted = sample(combined, n_total, false);
    
    // Only sum first n_x elements
    double sum_x = 0;
    for(int j = 0; j < n_x; j++) sum_x += permuted[j];
    
    double m_x = sum_x / n_x;
    double m_y = (sum_total - sum_x) / n_y;
    
    if (std::abs(m_x - m_y) >= obs_diff) {
      count++;
    }
  }
  
  double p_val = (double)(count + 1) / (n_perms + 1);
  
  return List::create(Named("statistic") = obs_diff,
                      Named("p.value") = p_val);
}

// [[Rcpp::export]]
List levene_test_cpp(NumericVector value, IntegerVector group, bool center_mean = true) {
  int n = value.size();
  int n_groups = max(group);
  
  std::vector<std::vector<double>> group_vals(n_groups);
  for(int i=0; i<n; i++) {
    group_vals[group[i]-1].push_back(value[i]);
  }
  
  std::vector<double> centers(n_groups);
  for(int g=0; g<n_groups; g++) {
    NumericVector v = wrap(group_vals[g]);
    if (center_mean) {
      centers[g] = mean(v);
    } else {
      centers[g] = median(v);
    }
  }
  
  NumericVector z(n);
  for(int i=0; i<n; i++) {
    z[i] = std::abs(value[i] - centers[group[i]-1]);
  }
  
  double F = calculate_f_stat(z, group, n_groups);
  
  double df1 = n_groups - 1;
  double df2 = n - n_groups;
  
  double p_value = R::pf(F, df1, df2, false, false);
  
  return List::create(Named("statistic") = F,
                      Named("p.value") = p_value,
                      Named("df1") = df1,
                      Named("df2") = df2);
}
