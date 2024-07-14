# Compare analysis of BTR CP JMLR data
# These simulations are based off of the same data that were used in
# Guhaniyogi, Qamar, and Dunson (2017). These have a number of shapes that are
# set as 2D true coefficients in the simulated data. These shapes are called:
# R3, R5, shapes, hawk, horse, palm
# Data ----
sim_data <- readRDS("data/1_BTRCP_JMLR_image_sim_data.rds")
result_dir <- "~/github/BTRTucker/results/BTRCP_JMLR_simulations/"
# Determine best result ----
# Use DIC for the BTR CP and BTR Tucker models
# for(img_name in c("R3", "R5", "shapes", "hawk", "horse", "palm")) {
best_model_info <- lapply(c("R3", "R5", "shapes", "hawk", "horse", "palm"), function(img_name) {
  # img_name <- "hawk"
  img <- as.matrix(sim_data[[img_name]])
  lapply(c("btrcp", "btrt"), function(model_name) {
    res_files <- list.files(result_dir, pattern = paste0(model_name, "_", img_name, full.names = TRUE))
    mdl_DIC <- sapply(res_files, function(x) {
      bt_res <- readRDS(x)
      DIC(bt_res$llik, burn_in = 300)
    })
    best_file <- res_files[which.min(mdl_DIC)]
    best_res <- readRDS(best_file)
    best_rank <- stringr::str_extract(best_files, pattern = "rank[0-9]+")
    best_rank <- sub("rank", "", best_rank)
    best_B <- BTRT_final_B(best_res)
    best_RMSE <- sqrt(mean((best_B - img)^2))
    list(B = best_B, Rank = best_rank, RMSE = best_RMSE)
  })
})
