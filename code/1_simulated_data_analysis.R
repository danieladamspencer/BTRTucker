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

# # > Frequentist Tucker ----
# # library(parallel)
# # cl <- makeCluster(8)
# # Set rank grid
# # rank_values <- expand.grid(r1 = 1:4, r2 = 1:4)
# # rank_values <- split(rank_values,row(rank_values))
# # parLapply(cl,rank_values, function(ranks) {
#   # This package, available on GitHub at 'danieladamspencer/bayestensorreg' is
#   # required for simulating data and performing analyses
#   ranks <- c(6,6)
#   library(bayestensorreg)
#   # Read in the data
#   save_dir <- "~/github/BTRTucker/results/data_simulations"
#   sim_data <- readRDS(file.path(save_dir,"1_simulated_data.rds"))
#   # >> Perform the analyses using the bayestensorreg package ----
#   ftr_result <-
#     try(FTRTucker(
#       input = sim_data,
#       ranks = as.numeric(ranks),
#       epsilon = 1e-3,
#       betas_LASSO = F
#     ))
#   # >> Save the results for visualization and comparison ----
#   saveRDS(ftr_result,
#           file.path(
#             save_dir,
#             paste0(
#               "1_simulated_data_ftrtucker_results_rank",
#               paste(as.numeric(ranks), collapse = ""),
#               ".rds"
#             )
#           ))
#   # return(NULL)
# # })
# # stopCluster(cl)

# > Frequentist CP ----
# library(parallel)
# cl <- makeCluster(4)
# Perform the analysis
# parLapply(cl,seq(4), function(rank) {
  # This package, available on GitHub at 'danieladamspencer/bayestensorreg' is
  # required for simulating data and performing analyses
  rank <- 7
  library(bayestensorreg)
  # Read in the data
  save_dir <- "~/github/BTRTucker/results/data_simulations"
  sim_data <- readRDS(file.path(save_dir,"1_simulated_data.rds"))
  # >> Perform the analyses using the bayestensorreg package ----
  ftr_result <-
    try(FTR_CP(
      input = sim_data,
      rank = as.numeric(rank),
      epsilon = 1e-3
    ))
  # >> Save the results for visualization and comparison ----
  saveRDS(ftr_result,
          file.path(
            save_dir,
            paste0(
              "1_simulated_data_ftr_cp_results_rank",
              paste(as.numeric(rank), collapse = ""),
              ".rds"
            )
          ))
  # return(NULL)
# })
# stopCluster(cl)

# # > General Linear Model ----
# save_dir <- "~/github/BTRTucker/results/data_simulations"
# sim_data <- readRDS(file.path(save_dir,"1_simulated_data.rds"))
# ytil <- lm(sim_data$y ~ -1 + sim_data$eta)$residuals
# library(parallel)
# cl <- makeCluster(8)
# glm_B <- parApply(cl,sim_data$X,1:2,function(x,ytil) {
#   out <- lm(ytil ~ -1 + x)$coefficients
#   return(out)
# }, ytil = ytil)
# glm_pvals <- parApply(cl,sim_data$X,1:2,function(x,ytil) {
#   out <- summary(lm(ytil ~ -1 + x))$coefficients[4]
#   return(out)
# }, ytil = ytil)
# glm_pvals2 <- matrix(p.adjust(c(glm_pvals),"BH"),nrow = nrow(glm_pvals),ncol = ncol(glm_pvals))
# glm_active <- matrix(as.numeric(glm_pvals < 0.05),nrow = nrow(glm_pvals),ncol = ncol(glm_pvals))
# final_glm_B <- glm_B * glm_active
# reshape2::melt(final_glm_B) |> ggplot() + geom_raster(aes(x = Var1, y = Var2, fill = value))
# saveRDS(final_glm_B, file = file.path(save_dir, "1_glm_B.rds"))


# # Simulated data with banded nonzero regions ----
# library(bayestensorreg)
# subjects = 400
# tensor_dims = c(50, 50)
# CNR = 1
# num_active = 1
# other_covar = c(1, 1)
#
# B <- Reduce(`+`, sapply(seq(num_active), function(zz) {
#   neuRosim::specifyregion(
#     dim = tensor_dims,
#     coord = sapply(tensor_dims, function(z)
#       sample(seq(z), size = 1)),
#     radius = ceiling(min(tensor_dims) * runif(1, 0.02, 0.1)),
#     form = "sphere",
#     fading = runif(1, 0.5, 1)
#   )
# }, simplify = FALSE))
#
# B[10,] <- 0
# image(B)
# eta <-
#   matrix(rnorm(subjects * length(other_covar)), subjects, length(other_covar))
# gam <- other_covar
# X <-
#   array(rnorm(prod(tensor_dims) * subjects), dim = c(tensor_dims, subjects))
# y <-
#   apply(X, length(dim(X)), function(xx)
#     sum(xx * B * CNR)) + c(eta %*% gam) + rnorm(subjects)
# banded_signal <- list(
#   y = y,
#   X = X,
#   true_B = B,
#   eta = eta,
#   gam = gam
# )
#
# btrt_ranks <- expand.grid(R1 = 1:2, R2 = 1:2)
# library(parallel)
# cl <- makeCluster(4)
# clusterExport(cl,"banded_signal")
# parApply(cl,btrt_ranks,1,function(rr) {
#   library(bayestensorreg)
#   save_dir <- "~/github/BTRTucker/results/data_simulations"
#   res <- BTRTucker(input = banded_signal, ranks = as.numeric(rr),n_iter = 1100, n_burn = 100,hyperparameters = NULL,save_dir = NULL)
#   saveRDS(res, file.path(save_dir,paste0("1_banded_signal_btrt_rank",paste(as.numeric(rr),collapse = ""),".rds")))
# })
# stopCluster(cl)

# Simulate data with very different ranks ----
