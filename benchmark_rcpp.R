library(Rcpp)

# Compile the C++ function
sourceCpp("perm_test.cpp")

# R implementation for comparison
permutation_test_r <- function(x, y, n_perms = 999) {
    obs_diff <- abs(mean(x) - mean(y))
    combined <- c(x, y)
    n_x <- length(x)

    count <- 0
    for (i in 1:n_perms) {
        permuted <- sample(combined)
        m_x <- mean(permuted[1:n_x])
        m_y <- mean(permuted[(n_x + 1):length(combined)])
        if (abs(m_x - m_y) >= obs_diff) {
            count <- count + 1
        }
    }
    return((count + 1) / (n_perms + 1))
}

# Generate data
set.seed(42)
x <- rnorm(100)
y <- rnorm(100, mean = 0.5)

print("Starting benchmark...")
n_runs <- 50
n_perms <- 1000

# Benchmark R
start_r <- Sys.time()
for (i in 1:n_runs) {
    permutation_test_r(x, y, n_perms)
}
time_r <- Sys.time() - start_r
print(paste("R implementation time:", time_r))

# Benchmark C++
start_cpp <- Sys.time()
for (i in 1:n_runs) {
    permutation_test_cpp(x, y, n_perms)
}
time_cpp <- Sys.time() - start_cpp
print(paste("C++ implementation time:", time_cpp))

speedup <- as.numeric(time_r) / as.numeric(time_cpp)
print(paste("Speedup factor:", round(speedup, 2), "x"))
