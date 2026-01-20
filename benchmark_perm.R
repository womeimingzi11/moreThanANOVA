# Benchmark Script for pairwisePermutationMatrix
library(rcompanion)
library(dplyr)
library(coin)

# Generate synthetic data (similar to large user datasets)
set.seed(123)
n_groups <- 10
n_per_group <- 50
total_n <- n_groups * n_per_group

df <- data.frame(
    Treatment = factor(rep(letters[1:n_groups], each = n_per_group)),
    Value = rnorm(total_n)
)

print(paste("Dataset size:", total_n, "rows,", n_groups, "groups"))

# Benchmark rcompanion::pairwisePermutationMatrix
print("Starting rcompanion benchmark (n=999)...")
start_time <- Sys.time()

pm_res <- pairwisePermutationMatrix(
    Value ~ Treatment,
    data = df,
    method = "fdr",
    n = 999
)

end_time <- Sys.time()
rcompanion_duration <- end_time - start_time
print(paste("rcompanion duration:", rcompanion_duration))
print(pm_res)

# Manual loop using coin (simulating what rcompanion might be doing or even lighter)
print("Starting manual coin loop benchmark (n=999)...")
start_time <- Sys.time()

groups <- levels(df$Treatment)
combos <- combn(groups, 2)
p_values <- numeric(ncol(combos))

for (i in 1:ncol(combos)) {
    g1 <- combos[1, i]
    g2 <- combos[2, i]

    sub_df <- df[df$Treatment %in% c(g1, g2), ]
    sub_df$Treatment <- factor(sub_df$Treatment) # Drop unused levels

    # coin::independence_test
    res <- coin::independence_test(
        Value ~ Treatment,
        data = sub_df,
        distribution = coin::approximate(nresample = 999)
    )
    p_values[i] <- coin::pvalue(res)
}

end_time <- Sys.time()
manual_coin_duration <- end_time - start_time
print(paste("Manual coin loop duration:", manual_coin_duration))
