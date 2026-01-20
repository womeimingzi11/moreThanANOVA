library(Rcpp)
library(dplyr)
sourceCpp("src/permutation.cpp")

# 1. Test 2-sample permutation
print("Testing perm_test_2sample...")
set.seed(123)
x <- rnorm(20)
y <- rnorm(20, mean = 2) # Significant difference

res_2 <- perm_test_2sample(x, y, n_perms = 999)
print(paste("2-Sample P-value:", res_2$p.value))
print(paste("Statistic:", res_2$statistic))

if (res_2$p.value < 0.05) {
    print("✅ 2-Sample test correctly detected difference.")
} else {
    print("❌ 2-Sample test failed to detect difference.")
}

# 2. Test K-sample permutation (F-stat)
print("\nTesting perm_test_ksample...")
group <- c(rep(1, 20), rep(2, 20), rep(3, 20))
val <- c(rnorm(20), rnorm(20, mean = 2), rnorm(20, mean = 4)) # Significant differences

res_k <- perm_test_ksample(val, group, n_perms = 999)
print(paste("K-Sample P-value:", res_k$p.value))
print(paste("F-Statistic:", res_k$statistic))

if (res_k$p.value < 0.05) {
    print("✅ K-Sample test correctly detected difference.")
} else {
    print("❌ K-Sample test failed to detect difference.")
}

# 3. Test Pairwise Helper (Simulating)
print("\nTesting pairwise_perm_cpp logic...")
pairwise_perm_cpp_test <- function(val, grp, method = "fdr") {
    groups <- unique(grp)
    combos <- combn(groups, 2)
    p_values <- numeric(ncol(combos))
    comparisons <- character(ncol(combos))

    for (i in 1:ncol(combos)) {
        g1 <- combos[1, i]
        g2 <- combos[2, i]
        val1 <- val[grp == g1]
        val2 <- val[grp == g2]
        res <- perm_test_2sample(val1, val2, n_perms = 999)
        p_values[i] <- res$p.value
        comparisons[i] <- paste(g1, "-", g2)
    }
    return(p.adjust(p_values, method = method))
}

res_pair <- pairwise_perm_cpp_test(val, group)
print("Pairwise P-values:")
print(res_pair)
