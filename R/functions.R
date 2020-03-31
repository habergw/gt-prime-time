library(data.table)
library(Rcpp)

sourceCpp("src/functions.cpp")

# Calculate expected number of tests
ET <- function(p, k, Se, Sp)
{
    pmin(1, Se - (1 - p)^k * (Se + Sp - 1) + 1 / k)
}

# Find optimal k using unconstrained optimization
get_opt_k <- function(p, k_max, Se_true, Se_est, Se_thresh, Sp_thresh)
{
  vals_min <- data.table(k_min = 1:k_max, Se_min = Se_true)
  vals_min[, ET_min := ET(p, k_min, Se_min, 1)]
  vals_min <- vals_min[which.min(ET_min)]

  vals_est <- data.table(k_est = 1:k_max, Se_est = Se_est)
  vals_est[, ET_est := ET(p, k_est, Se_est, 1)]
  vals_est <- vals_est[which.min(ET_est)]
  

  vals <- cbind(vals_min, vals_est)
  vals[, c("p", "type", "Se_true") :=
           list(p, "free", Se_true[k_est])]
  vals[, ET_true := ET(p, k_est, Se_true, 1)]
  vals
}

# Find optimal k using constrained optimization
get_opt_k_const <- function(p, k_max, Se_true, Se_est, Se_thresh, Sp_thresh)
{
  vals_min <- data.table(k_min = 1:k_max, Se_min = Se_true)
  vals_min[, ET_min := ET(p, k_min, Se_min, 1)]
  vals_min <- vals_min[Se_min >= Se_thresh]
  vals_min <- vals_min[which.min(ET_min)]

  vals_est <- data.table(k_est = 1:k_max, Se_est = Se_est)
  vals_est[, ET_est := ET(p, k_est, Se_est, 1)]
  vals_est <- vals_est[Se_est >= Se_thresh]
  vals_est <- vals_est[which.min(ET_est)]
  
  vals <- cbind(vals_min, vals_est)
  vals[, c("p", "type", "Se_true") :=
           list(p, "const", Se_true[k_est])]
  vals[, ET_true := ET(p, k_est, Se_true, 1)]
  return(vals)
}

# Compare values across different assumed models
compare_ests <- function(p, k_max, Se_list, Se_true_ind, Se_est_ind,
                         Se_thresh, Sp_thresh)
{
  Se_true <- Se_list[[Se_true_ind]]
  Se_est <- Se_list[[Se_est_ind]]
  vals <- lapply(list(get_opt_k, get_opt_k_const),
                 function(x) x(p, k_max, Se_true, Se_est, Se_thresh, Sp_thresh))
  
  vals <- rbindlist(vals)
  vals[, Se_true_ind := Se_true_ind]
  vals[, Se_est_ind := Se_est_ind]

  return(vals)
}

## Sensitivity functions
# Linear
f_linear <- function(p, k, slope)
{
  pmax(0, 1 - slope * (k - 1))
}

# Exponential
f_exp <- Vectorize(function(p, k, slope)
{
  if (k == 1)
    return(1)
  else
    return(max(0, 1 - slope * 2^(k / 2)))
}, "k")


# Hwang dilution function
f_hwang <- function(p, k, d) {
  p / (1 - (1 - p)^(k^d))
}

# Find required validation sample size via simulation
get_val_N <- function(p, k_max, Se, Sp, Se_thresh, Sp_thresh, epsilon, N_init, 
                      M, delta)
{
  N_1 <- N_init
  N_min <- 0
  N_max <- 0

  while(1) {
    res <- as.data.table(get_N_sim(N_1, p, k_max, Se, Sp, Se_thresh, 
                                   Sp_thresh, M))
    setnames(res, c("k", "T", "Se_est", "Sp_est", "Se_true", "Sp_true",
                    "ET_est", "ET_true"))
    avg <- res[Se_true >= Se_thresh & Sp_true >= Sp_thresh, .N] / res[, .N]

    cat(paste(p, N_1, avg, "\n"))

    if (abs(avg - epsilon) < delta || 
        (N_max > 0 & (N_min == N_1 || N_max == 1))) {
      res[, N := N_1]
      return (res)
    }
    
    if (avg > epsilon) {
      N_max <- N_1
      N_1 <- floor((N_1 + N_min) / 2)
    } else if (avg < epsilon) {
      N_min <- N_1
      if (N_max == 0)
        N_1 <- 2 * N_1
      else 
        N_1 <- floor((N_max + N_1) / 2)
    }
  }
}

# Calculate minimum population size
get_N_star <- function(N, T, ET)
{
  ceiling((T - N * ET) / (1 - ET))
}

# Reduce simulations to single summary
red_sims <- function(sims)
{
  sim_sum <- sims[, .(N = mean(N), T = mean(T), ET = mean(ET_true))]
  sim_sum[, N_S := get_N_star(N, T, ET)]

  sim_sum
}

# Clean simulations across models and add identifiers
clean_sims <- function(file)
{
  p <- as.numeric(regmatches(file,
                             regexpr("[0-9].*[0-9]", file)))
  
  sims <- readRDS(paste0("../Results/", file))
  
  models <- letters[1:length(sims)]

  res <- rbindlist(lapply(sims, red_sims))
  res[, p := p]
  res[, model := models]

  res
}

# Read in simulated data and clean for plotting
read_sims <- function(files)
{
  rbindlist(lapply(files, clean_sims))
}
