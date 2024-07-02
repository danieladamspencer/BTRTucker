# Analyze BTR CP JMLR data
# Libraries ----
library(bayestensorreg)

# Data ----
data_dir <- "~/github/BTRTucker/data/"
sim_data <- readRDS(list.files(data_dir, pattern = "BTRCP_JMLR", full.names = TRUE))

# Analyses ----
result_dir <- "~/github/BTRTucker/results/BTRCP_JMLR_simulations/"
## Make data ----
R3_dat <- list(y = c(sim_data$y_R3), X = sim_data$X)
R5_dat <- list(y = c(sim_data$y_R5), X = sim_data$X)
shapes_dat <- list(y = c(sim_data$y_shapes), X = sim_data$X)
hawk_dat <- list(y = c(sim_data$y_hawk), X = sim_data$X)
horse_dat <- list(y = c(sim_data$y_horse), X = sim_data$X)
palm_dat <- list(y = c(sim_data$y_palm), X = sim_data$X)
## FTR CP ----
hawk_dat_ftr <- hawk_dat
hawk_dat_ftr$eta <- rep(1,length(hawk_dat_ftr$y))
for(R in 3:15) {
  ftrcp_out <- FTR_CP(hawk_dat_ftr, rank = R)
  saveRDS(ftrcp_out, file = file.path(result_dir, paste0("1_ftrcp_hawk_rank",r,".rds")))
}

## FTR Tucker ----

## BTR CP ----
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

## BTR Tucker ----
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
