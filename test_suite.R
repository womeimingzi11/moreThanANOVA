library(Rcpp)
library(dplyr)
# Load C++ functions
sourceCpp("src/permutation.cpp")

# --- Helper Functions for R-side verification ---

# R implementation of 2-sample permutation test (Diff in Means)
perm_test_2sample_r <- function(x, y, n_perms = 999) {
    obs_diff <- abs(mean(x) - mean(y))
    combined <- c(x, y)
    nx <- length(x)
    count <- 0
    for (i in 1:n_perms) {
        shuffled <- sample(combined)
        mx <- mean(shuffled[1:nx])
        my <- mean(shuffled[(nx + 1):length(combined)])
        if (abs(mx - my) >= obs_diff) count <- count + 1
    }
    list(statistic = obs_diff, p.value = (count + 1) / (n_perms + 1))
}

# R implementation of F-statistic calculation
calc_f_stat_r <- function(val, grp) {
    fit <- aov(val ~ grp)
    summary(fit)[[1]][["F value"]][1]
}

# R implementation of K-sample permutation test (F-statistic)
perm_test_ksample_r <- function(val, grp, n_perms = 999) {
    grp <- factor(grp)
    obs_f <- calc_f_stat_r(val, grp)
    N <- length(val)
    count <- 0
    for (i in 1:n_perms) {
        shuffled_val <- sample(val)
        # Note: efficient way is just re-assigning groups, but aov is slow in loop
        # We will use a simplified F calculation to speed up R verification
        # or just accept R is slow (it's for verification)
        perm_f <- calc_f_stat_r(shuffled_val, grp)
        if (perm_f >= obs_f) count <- count + 1
    }
    list(statistic = obs_f, p.value = (count + 1) / (n_perms + 1))
}

# --- Test Runner ---

run_comparison <- function(name, x, y = NULL, group = NULL, type = "2-sample") {
    cat(paste0(strrep("-", 20), "\n"))
    cat(paste0("Testing Dataset: ", name, " (", type, ")\n"))

    if (type == "2-sample") {
        # 1. Statistic Consistency (C++ vs R)
        cpp_res <- perm_test_2sample(x, y, n_perms = 2000)
        r_stat <- abs(mean(x) - mean(y))

        cat(sprintf(
            "Statistic Change: R=%.5f, C++=%.5f ... ",
            r_stat,
            cpp_res$statistic
        ))
        if (abs(r_stat - cpp_res$statistic) < 1e-8) {
            cat("✅ MATCH\n")
        } else {
            cat("❌ MISMATCH\n")
        }

        # 2. P-value Consistency (approximate)
        # We run R version with fewer perms just for sanity check, or rely on statistic match + logic trust
        # Running expensive R loop for verification
        r_res <- perm_test_2sample_r(x, y, n_perms = 2000)

        cat(sprintf(
            "P-value: R=%.4f, C++=%.4f\n",
            r_res$p.value,
            cpp_res$p.value
        ))
    } else {
        # K-sample
        cpp_res <- perm_test_ksample(x, as.integer(group), n_perms = 2000)
        r_stat <- calc_f_stat_r(x, group)

        cat(sprintf(
            "Statistic Change: R=%.5f, C++=%.5f ... ",
            r_stat,
            cpp_res$statistic
        ))
        if (abs(r_stat - cpp_res$statistic) < 1e-5) {
            cat("✅ MATCH\n")
        } else {
            cat("❌ MISMATCH\n")
        }

        r_res <- perm_test_ksample_r(x, group, n_perms = 500) # Lower perms for R F-test speed

        cat(sprintf(
            "P-value: R=%.4f (500 perms), C++=%.4f (2000 perms)\n",
            r_res$p.value,
            cpp_res$p.value
        ))
    }
}

# --- Datasets ---

set.seed(42)

# Case 1: 2-Sample Normal (Significant)
x1 <- rnorm(50, mean = 0)
y1 <- rnorm(50, mean = 1)
run_comparison("2-Sample Normal (Sig)", x1, y1, type = "2-sample")

# Case 2: 2-Sample Skewed (Non-Significant)
x2 <- rexp(30)
y2 <- rexp(30)
run_comparison("2-Sample Skewed (Non-Sig)", x2, y2, type = "2-sample")

# Case 3: 2-Sample Small N (Sensitive)
x3 <- c(1, 2, 3, 4, 5)
y3 <- c(6, 7, 8, 9, 100) # Outlier
run_comparison("2-Sample Small w/ Outlier", x3, y3, type = "2-sample")

# Case 4: K-Sample ANOVA (Significant)
g4 <- factor(rep(1:3, each = 30))
v4 <- c(rnorm(30), rnorm(30, mean = 0.5), rnorm(30, mean = 1.0))
run_comparison("K-Sample Balanced (Sig)", v4, group = g4, type = "k-sample")

# Case 5: K-Sample Unbalanced (Non-Significant)
g5 <- factor(c(rep(1, 20), rep(2, 40), rep(3, 10)))
v5 <- rnorm(70)
run_comparison(
    "K-Sample Unbalanced (Non-Sig)",
    v5,
    group = g5,
    type = "k-sample"
)
