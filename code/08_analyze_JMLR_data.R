# Analyze BTR CP JMLR data

do_model <- "ftrt"
# Libraries ----
library(bayestensorreg)

# Data ----
data_dir <- "data/"
sim_data <- readRDS(list.files(data_dir, pattern = "BTRCP_JMLR", full.names = TRUE))

# Analyses ----
result_dir <- "results/BTRCP_JMLR_simulations/"
## Make data ----{
# R3_dat <- list(y = c(sim_data$y_R3), X = sim_data$X)
# R5_dat <- list(y = c(sim_data$y_R5), X = sim_data$X)
# shapes_dat <- list(y = c(sim_data$y_shapes), X = sim_data$X)
# hawk_dat <- list(y = c(sim_data$y_hawk), X = sim_data$X)
# horse_dat <- list(y = c(sim_data$y_horse), X = sim_data$X)
# palm_dat <- list(y = c(sim_data$y_palm), X = sim_data$X)
## FTR CP ----
if(do_model == "ftrcp") {
  for(img_set in c("R3", "R5", "shapes", "hawk", "horse", "palm")) {
    cat("Analyzing", img_set, "data\n")
    input_data <- list(
      y = unlist(sim_data[grep(pattern = paste0("y_", img_set), names(sim_data))]),
      X = sim_data$X
    )
    for(R in 3:15) {
      cat("... rank", R, "\n")
      ftrcp_out <- FTR_CP(input_data, rank = R)
      saveRDS(ftrcp_out, file = file.path(result_dir, paste0("08_ftrcp_", img_set, "_rank", R, ".rds")))
    }
  }
}

## FTR Tucker ----
if(do_model == "ftrt") {
  cat("FTRTucker\n")
  for(img_set in c("R3", "R5", "shapes", "hawk", "horse", "palm")) {
    cat("Analyzing", img_set, "data\n")
    input_data <- list(
      y = unlist(sim_data[grep(pattern = paste0("y_", img_set), names(sim_data))]),
      X = sim_data$X
    )
    for(R in 3:15) {
      cat("... rank", R, "\n")
      ftrcp_out <- FTRTucker(input_data, ranks = rep(R, 2))
      saveRDS(ftrcp_out, file = file.path(result_dir, paste0("08_ftrt_hawk_rank",paste(sprintf("%02g", rep(R, 2)), collapse = ""),".rds")))
    }
  }
}
## BTR CP ----
if(do_model == "btrcp") {
  for(R in 1:2) {
    cat("BTR CP Rank", R, "\n")
    R3
    cat("R3 \n")
    btrcp_out <- BTRTucker(R3_dat, ranks = c(R,R), n_iter = 1300, n_burn = 300, CP = TRUE)
    saveRDS(btrcp_out, file = file.path(result_dir,
                                        paste0("1_btrcp_R3_rank",sprintf("%02g",R), ".rds")))
    R5
    cat("R5 \n")
    btrcp_out <- BTRTucker(R5_dat, ranks = c(R,R), n_iter = 1300, n_burn = 300, CP = TRUE)
    saveRDS(btrcp_out, file = file.path(result_dir,
                                        paste0("1_btrcp_R5_rank",sprintf("%02g",R), ".rds")))
    shapes
    cat("shapes \n")
    btrcp_out <- BTRTucker(shapes_dat, ranks = c(R,R), n_iter = 1300, n_burn = 300, CP = TRUE)
    saveRDS(btrcp_out, file = file.path(result_dir,
                                        paste0("1_btrcp_shapes_rank",sprintf("%02g",R), ".rds")))
    hawk
    cat("hawk \n")
    btrcp_out <- BTRTucker(hawk_dat, ranks = c(R,R), n_iter = 1300, n_burn = 300, CP = TRUE)
    saveRDS(btrcp_out, file = file.path(result_dir,
                                        paste0("1_btrcp_hawk_rank",sprintf("%02g",R), ".rds")))
    horse
    cat("horse \n")
    btrcp_out <- BTRTucker(horse_dat, ranks = c(R,R), n_iter = 1300, n_burn = 300, CP = TRUE)
    saveRDS(btrcp_out,
            file = file.path(result_dir, paste0("1_btrcp_horse_rank",sprintf("%02g",R), ".rds")))
    palm
    cat("palm \n")
    btrcp_out <- BTRTucker(palm_dat, ranks = c(R,R), n_iter = 1300, n_burn = 300, CP = TRUE)
    saveRDS(btrcp_out, file = file.path(result_dir, paste0(
      "1_btrcp_palm_rank", sprintf("%02g", R), ".rds"
    )))
  }
}


## BTR Tucker ----
if(do_model == "btrt") {
  for(R in 3:15) {
    cat("BTR Tucker Rank", R, R, "\n")
    R3
    cat("R3 \n")
    btrt_out <- BTRTucker(R3_dat, ranks = c(R, R), n_iter = 1300, n_burn = 300, CP = FALSE)
    saveRDS(btrt_out, file = file.path(result_dir, paste0("1_btrt_R3_rank",sprintf("%02g",R), sprintf("%02g",R), ".rds")))
    R5
    cat("R5 \n")
    btrt_out <- BTRTucker(R5_dat, ranks = c(R, R), n_iter = 1300, n_burn = 300, CP = FALSE)
    saveRDS(btrt_out, file = file.path(result_dir, paste0("1_btrt_R5_rank",sprintf("%02g",R), sprintf("%02g",R), ".rds")))
    shapes
    cat("shapes \n")
    btrt_out <- BTRTucker(shapes_dat, ranks = c(R, R), n_iter = 1300, n_burn = 300, CP = FALSE)
    saveRDS(btrt_out, file = file.path(result_dir, paste0("1_btrt_shapes_rank",sprintf("%02g",R), sprintf("%02g",R), ".rds")))
    hawk
    cat("hawk \n")
    btrt_out <- BTRTucker(hawk_dat, ranks = c(R, R), n_iter = 1300, n_burn = 300, CP = FALSE)
    saveRDS(btrt_out, file = file.path(result_dir, paste0("1_btrt_hawk_rank",sprintf("%02g",R), sprintf("%02g",R), ".rds")))
    horse
    cat("horse \n")
    btrt_out <- BTRTucker(horse_dat, ranks = c(R, R), n_iter = 1300, n_burn = 300, CP = FALSE)
    saveRDS(btrt_out, file = file.path(result_dir, paste0("1_btrt_horse_rank",sprintf("%02g",R), sprintf("%02g",R), ".rds")))
    palm
    cat("palm \n")
    btrt_out <- BTRTucker(palm_dat, ranks = c(R, R), n_iter = 1300, n_burn = 300, CP = FALSE)
    saveRDS(btrt_out, file = file.path(result_dir, paste0("1_btrt_palm_rank",sprintf("%02g",R), sprintf("%02g",R), ".rds")))
  }
}

