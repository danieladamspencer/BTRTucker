# This is a script to perform the simulated data analysis from "Parsimonious
# Bayesian Sparse Tensor Regression Using the Tucker Tensor Decomposition" by
# Spencer, Guhaniyogi, and Prado (2021)

# # Simulate the data once ----
# save_dir <- "~/github/BTRTucker/results/data_simulations"
# # This package, available on GitHub at 'danieladamspencer/bayestensorreg' is
# # required for simulating data and performing analyses
# library(bayestensorreg)
# # Set seed for reproducibility
# set.seed(95064)
# # >> Generate data according to the specifications in the paper ----
# sim_data <-
#   TR_simulated_data(
#     subjects = 1000,
#     tensor_dims = c(50, 50),
#     CNR = 1,
#     num_active = 3,
#     other_covar = c(25, 3, 0.1)
#   )
# saveRDS(sim_data, file = file.path(save_dir,"1_simulated_data.rds"))
#
# # Perform analyses on the simulated data ----
# # > Bayesian Tucker ----
# library(parallel)
# cl <- makeCluster(4)
# rank_values <- expand.grid(r1 = 1:4, r2 = 1:4)
# rank_values <- split(rank_values,row(rank_values))
# parLapplyLB(cl, rev(rank_values), function(ranks) {
#   # This package, available on GitHub at 'danieladamspencer/bayestensorreg' is
#   # required for simulating data and performing analyses
#   library(bayestensorreg)
#   # Read in the data
#   save_dir <- "~/github/BTRTucker/results/data_simulations"
#   sim_data <- readRDS(file.path(save_dir,"1_simulated_data.rds"))
#   # >> Perform the analyses using the bayestensorreg package ----
#   bayes_result <-
#     try(BTRTucker(
#       input = sim_data,
#       ranks = as.numeric(ranks),
#       n_iter = 11000,
#       n_burn = 1000,
#       hyperparameters = NULL,
#       save_dir = NULL
#     ))
#   # >> Save the results for visualization and comparison ----
#   saveRDS(bayes_result,
#           file.path(
#             save_dir,
#             paste0(
#               "1_simulated_data_btrtucker_results_rank",
#               paste(as.numeric(ranks), collapse = ""),
#               ".rds"
#             )
#           ))
#   return(NULL)
# })
# stopCluster(cl)
#
# # > Bayesian CP ----
# library(parallel)
# cl <- makeCluster(4)
# parSapply(cl, seq(4), function(rank) {
#   # This package, available on GitHub at 'danieladamspencer/bayestensorreg' is
#   # required for simulating data and performing analyses
#   library(bayestensorreg)
#   # Read in the data
#   save_dir <- "~/github/BTRTucker/results/data_simulations"
#   sim_data <- readRDS(file.path(save_dir,"1_simulated_data.rds"))
#   # >> Perform the analyses using the bayestensorreg package ----
#   bayes_result <-
#     try(BTR_CP(
#       input = sim_data,
#       max_rank = as.numeric(rank),
#       n_iter = 11000,
#       n_burn = 1000,
#       hyperparameters = NULL,
#       save_dir = NULL
#     ))
#   # >> Save the results for visualization and comparison ----
#   saveRDS(bayes_result,
#           file.path(
#             save_dir,
#             paste0(
#               "1_simulated_data_btr_cp_results_rank",
#               paste(as.numeric(rank), collapse = ""),
#               ".rds"
#             )
#           ))
#   return(NULL)
# })
# stopCluster(cl)

# > Frequentist Tucker ----
library(parallel)
cl <- makeCluster(8)
# Set rank grid
rank_values <- expand.grid(r1 = 1:4, r2 = 1:4)
rank_values <- split(rank_values,row(rank_values))
parLapply(cl,rank_values, function(ranks) {
  # This package, available on GitHub at 'danieladamspencer/bayestensorreg' is
  # required for simulating data and performing analyses
  library(bayestensorreg)
  # Read in the data
  save_dir <- "~/github/BTRTucker/results/data_simulations"
  sim_data <- readRDS(file.path(save_dir,"1_simulated_data.rds"))
  # >> Perform the analyses using the bayestensorreg package ----
  ftr_result <-
    try(FTRTucker(
      input = sim_data,
      ranks = as.numeric(ranks),
      epsilon = 1e-3,
      betas_LASSO = F
    ))
  # >> Save the results for visualization and comparison ----
  saveRDS(ftr_result,
          file.path(
            save_dir,
            paste0(
              "1_simulated_data_ftrtucker_results_rank",
              paste(as.numeric(ranks), collapse = ""),
              ".rds"
            )
          ))
  return(NULL)
})
stopCluster(cl)

# # > Frequentist CP ----
# # This package, available on GitHub at 'danieladamspencer/bayestensorreg' is
# # required for simulating data and performing analyses
# library(bayestensorreg)
# # Read in the data
# save_dir <- "~/github/BTRTucker/results/data_simulations"
# sim_data <- readRDS(file.path(save_dir,"1_simulated_data.rds"))
# # Perform the analysis
# sapply(seq(4), function(rank) {
#   # >> Perform the analyses using the bayestensorreg package ----
#   ftr_result <-
#     FTR_CP(
#       input = sim_data,
#       rank = as.numeric(rank),
#       epsilon = 1e-4
#     )
#   # >> Save the results for visualization and comparison ----
#   saveRDS(ftr_result,
#           file.path(
#             save_dir,
#             paste0(
#               "1_simulated_data_ftr_cp_results_rank",
#               paste(as.numeric(rank), collapse = ""),
#               ".rds"
#             )
#           ))
#   return(NULL)
# })
