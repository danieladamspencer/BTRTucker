# Analyze simulated data
# This is a script to perform the simulated data analysis from "Parsimonious
# Bayesian Sparse Tensor Regression Using the Tucker Tensor Decomposition" by
# Spencer, Guhaniyogi, and Prado (2021)
do_BTRTucker <- TRUE
do_BTRCP <- FALSE
do_FTRTucker <- FALSE
do_FTRCP <- FALSE
do_GLM <- FALSE

# BTRTucker ----
if(do_BTRTucker) {
  library(parallel)
  cl <- makeCluster(4)
  rank_values <- expand.grid(r1 = 4, r2 = 4)
  rank_values <- split(rank_values,row(rank_values))
  parLapplyLB(cl, rev(rank_values), function(ranks) {
    # This package, available on GitHub at 'danieladamspencer/bayestensorreg' is
    # required for simulating data and performing analyses
    library(bayestensorreg)
    # Read in the data
    data_dir <- "data/"
    save_dir <- "results/data_simulations_R3"
    sim_data <- readRDS(file.path(data_dir,"01_simulated_data.rds"))
    # Perform the analyses using the bayestensorreg package
    bayes_result <-
      try(BTRTucker(
        input = sim_data,
        ranks = as.numeric(ranks),
        n_iter = 1300,
        n_burn = 300,
        hyperparameters = NULL,
        save_dir = NULL
      ))
    # Save the results for visualization and comparison
    saveRDS(bayes_result,
            file.path(
              save_dir,
              paste0(
                "02_simulated_data_btrtucker_results_small_rank",
                paste(as.numeric(ranks), collapse = ""),
                ".rds"
              )
            ))
    return(NULL)
  })
  stopCluster(cl)
}


# BTR CP ----
if(do_BTRCP) {
  library(parallel)
  cl <- makeCluster(4)
  parSapply(cl, 7, function(rank) {
    # This package, available on GitHub at 'danieladamspencer/bayestensorreg' is
    # required for simulating data and performing analyses
    library(bayestensorreg)
    # Read in the data
    data_dir <- "data/"
    save_dir <- "results/data_simulations_R3"
    sim_data <- readRDS(file.path(data_dir,"01_simulated_data.rds"))
    # Perform the analyses using the bayestensorreg package
    bayes_result <-
      BTRTucker(
        input = sim_data,
        ranks = rep(rank, 2),
        n_iter = 1300,
        n_burn = 300,
        CP = TRUE,
        hyperparameters = NULL,
        save_dir = NULL
      )
    # Save the results for visualization and comparison
    saveRDS(bayes_result,
            file.path(
              save_dir,
              paste0(
                "02_simulated_data_btr_cp_results_rank",
                paste(as.numeric(rank), collapse = ""),
                ".rds"
              )
            ))
    return(NULL)
  })
  stopCluster(cl)
}

# Frequentist Tucker ----
if(do_FTRTucker) {
  library(parallel)
  cl <- makeCluster(8)
  # Set rank grid
  rank_values <- expand.grid(r1 = 7, r2 = 7)
  rank_values <- split(rank_values,row(rank_values))
  parLapply(cl,rank_values, function(ranks) {
    # This package, available on GitHub at 'danieladamspencer/bayestensorreg' is
    # required for simulating data and performing analyses
    # ranks <- c(6,6)
    library(bayestensorreg)
    # Read in the data
    data_dir <- "data/"
    save_dir <- "results/data_simulations_R3/"
    sim_data <- readRDS(file.path(data_dir,"01_simulated_data.rds"))
    # Perform the analyses using the bayestensorreg package
    ftr_result <-
      try(FTRTucker(
        input = sim_data,
        ranks = as.numeric(ranks),
        epsilon = 1e-3,
        betas_LASSO = F
      ))
    # Save the results for visualization and comparison
    saveRDS(ftr_result,
            file.path(
              save_dir,
              paste0(
                "02_simulated_data_ftrtucker_results_rank",
                paste(as.numeric(ranks), collapse = ""),
                ".rds"
              )
            ))
    return(NULL)
  })
  stopCluster(cl)
}

# FTR CP ----
if(do_FTRCP) {
  library(parallel)
  cl <- makeCluster(4)
  # Perform the analysis
  parLapply(cl,seq(5, 6), function(rank) {
    # This package, available on GitHub at 'danieladamspencer/bayestensorreg' is
    # required for simulating data and performing analyses
    # rank <- 7
    library(bayestensorreg)
    # Read in the data
    data_dir <- 'data/'
    save_dir <- "results/data_simulations_R3/"
    sim_data <- readRDS(file.path(data_dir,"01_simulated_data.rds"))
    # Perform the analyses using the bayestensorreg package
    ftr_result <-
      try(FTR_CP(
        input = sim_data,
        rank = as.numeric(rank),
        epsilon = 1e-3
      ))
    # Save the results for visualization and comparison
    saveRDS(ftr_result,
            file.path(
              save_dir,
              paste0(
                "02_simulated_data_ftr_cp_results_rank",
                paste(as.numeric(rank), collapse = ""),
                ".rds"
              )
            ))
    return(NULL)
  })
  stopCluster(cl)
}


# General Linear Model ----
if(do_GLM) {
  data_dir <- "data/"
  save_dir <- "results/data_simulations_R3"
  sim_data <- readRDS(file.path(data_dir,"01_simulated_data.rds"))
  ytil <- lm(sim_data$y ~ -1 + sim_data$eta)$residuals
  library(parallel)
  cl <- makeCluster(7)
  glm_B <- parApply(cl,sim_data$X,1:2,function(x,ytil) {
    out <- lm(ytil ~ -1 + x)$coefficients
    return(out)
  }, ytil = ytil)
  glm_pvals <- parApply(cl,sim_data$X,1:2,function(x,ytil) {
    out <- summary(lm(ytil ~ -1 + x))$coefficients[4]
    return(out)
  }, ytil = ytil)
  glm_pvals <- matrix(p.adjust(c(glm_pvals),"BH"),nrow = nrow(glm_pvals),ncol = ncol(glm_pvals))
  glm_active <- matrix(as.numeric(glm_pvals < 0.05),nrow = nrow(glm_pvals),ncol = ncol(glm_pvals))
  final_glm_B <- glm_B * glm_active
  saveRDS(final_glm_B, file = file.path(save_dir, "02_glm_B.rds"))
}


