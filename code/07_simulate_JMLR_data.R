# Simulating the JMLR data
# Libraries ----
library(bayestensorreg)

# Data ----
data_dir <- "data/"
R3 <- read.table(list.files(data_dir, pattern = "R3_coefficient", full.names = TRUE))
R5 <- read.table(list.files(data_dir, pattern = "R5_coefficient", full.names = TRUE))
shapes <- read.table(list.files(data_dir, pattern = "shapes_coefficient", full.names = TRUE))
hawk <- read.table(list.files(data_dir, pattern = "hawk_coefficient.+\\.txt", full.names = TRUE))
horse <- read.table(list.files(data_dir, pattern = "horse_coefficient", full.names = TRUE))
palm <- read.table(list.files(data_dir, pattern = "palm_coefficient", full.names = TRUE))

# Simulate tensor covariate ----
n <- 1000
sig2 <- 1
p <- 64

set.seed(1)
X <- array(rnorm(p*p*n, sd = sqrt(sig2)), dim = c(p,p,n))
X_mat <- bayestensorreg:::mode_k_matriz(X, 3)

# Simulate responses ----
y_R3 <- X_mat %*% unlist(R3) + rnorm(n, sd = 1)
y_R5 <- X_mat %*% unlist(R5) + rnorm(n, sd = 1)
y_shapes <- X_mat %*% unlist(shapes) + rnorm(n, sd = 1)
y_hawk <- X_mat %*% unlist(hawk) + rnorm(n, sd = 1)
y_horse <- X_mat %*% unlist(horse) + rnorm(n, sd = 1)
y_palm <- X_mat %*% unlist(palm) + rnorm(n, sd = 1)

# Save values ----
image_sim_data <-
  list(
    X = X,
    y_R3 = y_R3,
    y_R5 = y_R5,
    y_shapes = y_shapes,
    y_hawk = y_hawk,
    y_horse = y_horse,
    y_palm = y_palm,
    R3 = R3,
    R5 = R5,
    shapes = shapes,
    hawk = hawk,
    horse = horse,
    palm = palm
  )
saveRDS(image_sim_data, file = file.path(data_dir, "1_BTRCP_JMLR_image_sim_data.rds"))
