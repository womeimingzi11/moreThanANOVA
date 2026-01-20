library(Rcpp)
library(dplyr)
library(microbenchmark)

sourceCpp("src/permutation.cpp")

# --- Helper: Base R Levene Implementation ---
levene_test_base_r <- function(value, group, center_choice = "median") {
    group <- as.factor(group)
    # 1. Transform
    if (center_choice == "mean") {
        centers <- tapply(value, group, mean)
    } else {
        centers <- tapply(value, group, median)
    }
    z <- abs(value - centers[group])

    # 2. ANOVA
    fit <- aov(z ~ group)
    summ <- summary(fit)[[1]]

    list(statistic = summ$`F value`[1], p.value = summ$`Pr(>F)`[1])
}

# --- Verification & Benchmark ---

verify_levene_no_car <- function(n_rows = 1000, n_groups = 3) {
    set.seed(123)
    df <- data.frame(
        Group = factor(rep(1:n_groups, length.out = n_rows)),
        Value = c(rnorm(n_rows / 2), rnorm(n_rows / 2, sd = 2))
    )
    group_int <- as.integer(df$Group)

    print(paste("Dataset:", n_rows, "rows,", n_groups, "groups"))

    # 1. Levene (Mean)
    cat("\n--- Testing Levene (Mean) ---\n")
    base_res <- levene_test_base_r(df$Value, df$Group, center = "mean")
    cpp_res <- levene_test_cpp(df$Value, group_int, center_mean = TRUE)

    cat(sprintf(
        "Statistic: base=%.5f, cpp=%.5f ... ",
        base_res$statistic,
        cpp_res$statistic
    ))
    if (abs(base_res$statistic - cpp_res$statistic) < 1e-4) {
        cat("✅ MATCH\n")
    } else {
        cat("❌ MISMATCH\n")
    }

    # 2. Brown-Forsythe (Median)
    cat("\n--- Testing Levene (Median) ---\n")
    base_res_med <- levene_test_base_r(df$Value, df$Group, center = "median")
    cpp_res_med <- levene_test_cpp(df$Value, group_int, center_mean = FALSE)

    cat(sprintf(
        "Statistic: base=%.5f, cpp=%.5f ... ",
        base_res_med$statistic,
        cpp_res_med$statistic
    ))
    if (abs(base_res_med$statistic - cpp_res_med$statistic) < 1e-4) {
        cat("✅ MATCH\n")
    } else {
        cat("❌ MISMATCH\n")
    }

    # 3. Benchmark
    cat("\n--- Benchmarking ---\n")
    mb <- microbenchmark(
        baseR = levene_test_base_r(df$Value, df$Group, center = "median"),
        cpp = levene_test_cpp(df$Value, group_int, center_mean = FALSE),
        times = 100
    )
    print(mb)

    # Calculate speedup based on median time
    summary_mb <- summary(mb)
    median_base <- summary_mb$median[summary_mb$expr == "baseR"]
    median_cpp <- summary_mb$median[summary_mb$expr == "cpp"]
    speedup <- median_base / median_cpp
    cat(sprintf("\nSpeedup Factor: %.2fx 🚀\n", speedup))
}

verify_levene_no_car()
