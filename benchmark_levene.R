library(Rcpp)
library(car)
library(dplyr)
library(microbenchmark)

sourceCpp("src/permutation.cpp")

# --- Verification & Benchmark ---

verify_levene <- function(n_rows = 1000, n_groups = 3) {
    set.seed(123)
    df <- data.frame(
        Group = factor(rep(1:n_groups, length.out = n_rows)),
        Value = c(rnorm(n_rows / 2), rnorm(n_rows / 2, sd = 2)) # Heterogeneous variance
    )
    group_int <- as.integer(df$Group)

    print(paste("Dataset:", n_rows, "rows,", n_groups, "groups"))

    # 1. Standard Levene (Mean)
    cat("\n--- Testing Levene (Mean) ---\n")
    car_res <- leveneTest(Value ~ Group, data = df, center = mean)
    cpp_res <- levene_test_cpp(df$Value, group_int, center_mean = TRUE)

    cat(sprintf(
        "Statistic: car=%.5f, cpp=%.5f ... ",
        car_res$`F value`[1],
        cpp_res$statistic
    ))
    if (abs(car_res$`F value`[1] - cpp_res$statistic) < 1e-4) {
        cat("✅ MATCH\n")
    } else {
        cat("❌ MISMATCH\n")
    }

    cat(sprintf(
        "P-value:   car=%.5f, cpp=%.5f ... ",
        car_res$`Pr(>F)`[1],
        cpp_res$p.value
    ))
    if (abs(car_res$`Pr(>F)`[1] - cpp_res$p.value) < 1e-4) {
        cat("✅ MATCH\n")
    } else {
        cat("❌ MISMATCH\n")
    }

    # 2. Brown-Forsythe (Median) - Default for car::leveneTest
    cat("\n--- Testing Brown-Forsythe (Median) ---\n")
    car_res_med <- leveneTest(Value ~ Group, data = df, center = median)
    cpp_res_med <- levene_test_cpp(df$Value, group_int, center_mean = FALSE)

    cat(sprintf(
        "Statistic: car=%.5f, cpp=%.5f ... ",
        car_res_med$`F value`[1],
        cpp_res_med$statistic
    ))
    if (abs(car_res_med$`F value`[1] - cpp_res_med$statistic) < 1e-4) {
        cat("✅ MATCH\n")
    } else {
        cat("❌ MISMATCH\n")
    }

    # 3. Benchmark
    cat("\n--- Benchmarking ---\n")
    mb <- microbenchmark(
        car = leveneTest(Value ~ Group, data = df, center = median),
        cpp = levene_test_cpp(df$Value, group_int, center_mean = FALSE),
        times = 100
    )
    print(mb)

    # Calculate speedup based on median time
    summary_mb <- summary(mb)
    median_car <- summary_mb$median[summary_mb$expr == "car"]
    median_cpp <- summary_mb$median[summary_mb$expr == "cpp"]
    speedup <- median_car / median_cpp
    cat(sprintf("\nSpeedup Factor: %.2fx 🚀\n", speedup))
}

verify_levene()
