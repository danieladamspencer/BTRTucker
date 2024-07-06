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
for(img_name in c("R3", "R5", "shapes", "hawk", "horse", "palm")) {
  img_name <- "hawk"
  img <- as.matrix(sim_data[[img_name]])
  # BTR CP
  res_files <- list.files(result_dir, pattern = paste0("btrcp_",img_name), full.names = TRUE)
  btrcp_dic <- sapply(res_files, function(x) {
    btrcp_res <- readRDS(x)
    DIC(btrcp_res$llik, burn_in = 300)})
  btrcp_res <- readRDS(res_files[which.min(btrcp_dic)])
  btrcp_B <- BTRT_final_B(btrcp_res)
  # BTR Tucker
  res_files <- list.files(result_dir, pattern = paste0("btrt_",img_name), full.names = TRUE)
  btrt_dic <- sapply(res_files, function(x) {
    btrt_res <- readRDS(x)
    DIC(btrt_res$llik, burn_in = 300)})
  btrt_res <- readRDS(res_files[which.min(btrt_dic)])
  btrt_B <- BTRT_final_B(btrt_res)
}
