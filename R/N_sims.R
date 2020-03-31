# Validation study simulations. To be run in batch mode with two arguments
# 1) seed and 2) prevalence value. Outputs data to be read/aggregated in file
# create_plot.R
rm(list = ls())

source("R/functions.R")

args <- commandArgs(TRUE)

seed <- args[1]
p <- as.numeric(args[2])

# List of sensitivity values. Each element is a vector of length k_max 
# of values giving sensitivity for group sizes 1 through k_max.
Se <- list(1 - 0.02 * (0:9),
           c(1, 1 - 0.02 * 2^(1:9 / 2)),
           f_hwang(p, 1:10, .1),
           f_hwang(p, 1:10, .3))

# Vector of specificity values for group sizes 1 through k_max
Sp <- rep(1, 10)

# Maximum group size
k_max <- 10

# Sensitivity threshold (e.g., minimum acceptable sensitivity)
Se_thresh <- 0.95

# Specificity threshold (e.g., minimum acceptable specificity)
Sp_thresh <- 0.95

# Desired accuracy threshold (e.g., P(estimated sensitivity > Se_thresh) > epsilon)
epsilon <- 0.95

# Initial guess for required validation sample size
N_init <- 10000

# Number of monte carlo iterations
M <- 50000

# Numerical accuracy for assessing convergence 
# (converge if |P(estimated sensitivity > Se_thresh) - epsilon| < delta)
delta <- 0.001

set.seed(seed)

res <- lapply(Se, function(i)
  get_val_N(p, k_max, i, Sp, Se_thresh, Sp_thresh, epsilon, N_init, M, delta))

saveRDS(res, paste0("Results/res_sim_", p, ".Rda"))
