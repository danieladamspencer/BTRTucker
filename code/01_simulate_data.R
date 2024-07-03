# Generate simulated data
# Setup ----
save_dir <- "data/"
# This package, available on GitHub at 'danieladamspencer/bayestensorreg' is
# required for simulating data and performing analyses
library(bayestensorreg)
# Set seed for reproducibility
set.seed(95064)
# General simulated data ----
sim_data <-
  TR_simulated_data(
    subjects = 1000,
    tensor_dims = c(50, 50),
    CNR = 1,
    num_active = 3,
    other_covar = c(25, 3, 0.1)
  )
saveRDS(sim_data, file = file.path(save_dir,"01_simulated_data.rds"))

