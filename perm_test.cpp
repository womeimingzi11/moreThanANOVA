#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double permutation_test_cpp(NumericVector x, NumericVector y, int n_perms = 999) {
  int n_x = x.size();
  int n_y = y.size();
  int n_total = n_x + n_y;
  
  // Calculate observed difference in means
  double mean_x = mean(x);
  double mean_y = mean(y);
  double obs_diff = std::abs(mean_x - mean_y);
  
  // Combine data
  NumericVector combined(n_total);
  for (int i = 0; i < n_x; i++) combined[i] = x[i];
  for (int i = 0; i < n_y; i++) combined[n_x + i] = y[i];
  
  int count = 0;
  NumericVector permuted = clone(combined);
  
  for (int i = 0; i < n_perms; i++) {
    // Shuffle
    permuted = sample(combined, n_total, false);
    
    // Split and calculate means
    double sum_x = 0;
    for (int j = 0; j < n_x; j++) sum_x += permuted[j];
    double m_x = sum_x / n_x;
    
    double sum_y = 0;
    for (int j = 0; j < n_y; j++) sum_y += permuted[n_x + j];
    double m_y = sum_y / n_y;
    
    if (std::abs(m_x - m_y) >= obs_diff) {
      count++;
    }
  }
  
  return (double)(count + 1) / (n_perms + 1);
}
