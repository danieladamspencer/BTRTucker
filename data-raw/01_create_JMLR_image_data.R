# This is an elementary attempt to read in the hawk, horse, and palm tree images
# from Raj's CP paper for use in the Tucker paper. First, create the image
# matrices, then create the image coefficient data sets for testing.

# Before reading the data into R, follow these steps:
# 1. Take a screenshot of the figure from the paper in JMLR
# 2. Open the screenshot in GIMP
# 3. Crop out axes or extra image pieces
# 4. Image > Scale. Unlock the aspect ratio and set to 64 by 64.
# 5. Export image as a .png

# Libraries ----
library(png)

# Create image matrices ----
## Data ----
data_dir <- "~/Downloads/"
hawk_file <- list.files(data_dir, pattern = "hawk_image_scaled\\.png", full.names = TRUE)
horse_file <- list.files(data_dir, pattern = "horse_image_scaled\\.png", full.names = TRUE)
palm_file <- list.files(data_dir, pattern = "palm_image_scaled\\.png", full.names = TRUE)
R3_file <- list.files(data_dir, pattern = "R3_image_scaled\\.png", full.names = TRUE)
R5_file <- list.files(data_dir, pattern = "R5_image_scaled\\.png", full.names = TRUE)
shapes_file <- list.files(data_dir, pattern = "shapes_image_scaled\\.png", full.names = TRUE)

## Hawk ----
hawk_png <- readPNG(hawk_file)
str(hawk_png)
image(hawk_png[,,1])
image(hawk_png[,,2])
image(hawk_png[,,3])
# image(hawk_png[,,4])
hawk_mat <- hawk_png[,,1]
# image(t(hawk_mat))
# image(hawk_mat[5:475, 5:622])
# hawk_mat <- hawk_mat[6:475, 77:551]
image(hawk_mat)

library(tidyverse)
library(data.table)
thr_value <- 0.7
hawk_df <- as.data.table(reshape2::melt(hawk_mat))
hawk_df[value >= thr_value, value := 1]
hawk_df[value < thr_value, value := 0]
hawk_df[, logic_value := as.logical(value)]
hawk_df[, inv_value := as.numeric(!logic_value)]
hawk_df[, rev_Var1 := rev(Var1)]

ggplot(hawk_df, aes(x = Var2, y = rev_Var1, fill = inv_value)) +
  geom_raster() +
  # scale_fill_continuous("") +
  # scale_fill_distiller("", palette = "Greys") +
  scale_fill_gradient2("", low = "white", high = "black") +
  labs(x = "", y = "") +
  theme_minimal()

hawk_mat_out <- hawk_df[, .(rev_Var1, Var2, inv_value)]
setorder(hawk_mat_out, rev_Var1, Var2)
hawk_mat_out <- dcast(hawk_mat_out, rev_Var1 ~ Var2, value.var = "inv_value")
hawk_mat_out <- as.matrix(hawk_mat_out[, -1])
image(t(hawk_mat_out))
fwrite(hawk_mat_out, file = "data/hawk_coefficient_matrix.csv", col.names = FALSE)
write.table(hawk_mat_out, file = "data/hawk_coefficient_matrix.txt", row.names = FALSE, col.names = FALSE)

test <- read.csv("data/hawk_coefficient_matrix.csv")
image(t(as.matrix(test)))

## Horse ----
horse_png <- readPNG(horse_file)
horse_mat <- horse_png[,,1]
thr_value <- 0.7
horse_df <- as.data.table(reshape2::melt(horse_mat))
horse_df[value >= thr_value, value := 1]
horse_df[value < thr_value, value := 0]
horse_df[, logic_value := as.logical(value)]
horse_df[, inv_value := as.numeric(!logic_value)]
horse_df[, rev_Var1 := rev(Var1)]

ggplot(horse_df, aes(x = Var2, y = rev_Var1, fill = inv_value)) +
  geom_raster() +
  scale_fill_gradient2("", low = "white", high = "black") +
  labs(x = "", y = "") +
  theme_minimal()

horse_mat_out <- horse_df[, .(rev_Var1, Var2, inv_value)]
setorder(horse_mat_out, rev_Var1, Var2)
horse_mat_out <- dcast(horse_mat_out, rev_Var1 ~ Var2, value.var = "inv_value")
horse_mat_out <- as.matrix(horse_mat_out[, -1])
image(t(horse_mat_out))
write.table(horse_mat_out, file = "data/horse_coefficient_matrix.txt", row.names = FALSE, col.names = FALSE)

## Palm ----
palm_png <- readPNG(palm_file)
palm_mat <- palm_png[,,1]
thr_value <- 0.7
palm_df <- as.data.table(reshape2::melt(palm_mat))
palm_df[value >= thr_value, value := 1]
palm_df[value < thr_value, value := 0]
palm_df[, logic_value := as.logical(value)]
palm_df[, inv_value := as.numeric(!logic_value)]
palm_df[, rev_Var1 := rev(Var1)]

ggplot(palm_df, aes(x = Var2, y = rev_Var1, fill = inv_value)) +
  geom_raster() +
  scale_fill_gradient2("", low = "white", high = "black") +
  labs(x = "", y = "") +
  theme_minimal()

palm_mat_out <- palm_df[, .(rev_Var1, Var2, inv_value)]
setorder(palm_mat_out, rev_Var1, Var2)
palm_mat_out <- dcast(palm_mat_out, rev_Var1 ~ Var2, value.var = "inv_value")
palm_mat_out <- as.matrix(palm_mat_out[, -1])
image(t(palm_mat_out))
write.table(palm_mat_out, file = "data/palm_coefficient_matrix.txt", row.names = FALSE, col.names = FALSE)

## R3 ----
R3_png <- readPNG(R3_file)
R3_mat <- R3_png[,,1]
thr_value <- 0.6
R3_df <- as.data.table(reshape2::melt(R3_mat))
R3_df[value >= thr_value, value := 1]
R3_df[value < thr_value, value := 0]
R3_df[, logic_value := as.logical(value)]
R3_df[, inv_value := as.numeric(!logic_value)]
R3_df[, rev_Var1 := rev(Var1)]

ggplot(R3_df, aes(x = Var2, y = rev_Var1, fill = inv_value)) +
  geom_raster() +
  scale_fill_gradient2("", low = "white", high = "black") +
  labs(x = "", y = "") +
  theme_minimal()

R3_mat_out <- R3_df[, .(rev_Var1, Var2, inv_value)]
setorder(R3_mat_out, rev_Var1, Var2)
R3_mat_out <- dcast(R3_mat_out, rev_Var1 ~ Var2, value.var = "inv_value")
R3_mat_out <- as.matrix(R3_mat_out[, -1])
image(t(R3_mat_out))
write.table(R3_mat_out, file = "data/R3_coefficient_matrix.txt", row.names = FALSE, col.names = FALSE)

## R5 ----
R5_png <- readPNG(R5_file)
R5_mat <- R5_png[,,1]
thr_value <- 0.7
R5_df <- as.data.table(reshape2::melt(R5_mat))
R5_df[value >= thr_value, value := 1]
R5_df[value < thr_value, value := 0]
R5_df[, logic_value := as.logical(value)]
R5_df[, inv_value := as.numeric(!logic_value)]
R5_df[, rev_Var1 := rev(Var1)]

ggplot(R5_df, aes(x = Var2, y = rev_Var1, fill = inv_value)) +
  geom_raster() +
  scale_fill_gradient2("", low = "white", high = "black") +
  labs(x = "", y = "") +
  theme_minimal()

R5_mat_out <- R5_df[, .(rev_Var1, Var2, inv_value)]
setorder(R5_mat_out, rev_Var1, Var2)
R5_mat_out <- dcast(R5_mat_out, rev_Var1 ~ Var2, value.var = "inv_value")
R5_mat_out <- as.matrix(R5_mat_out[, -1])
image(t(R5_mat_out))
write.table(R5_mat_out, file = "data/R5_coefficient_matrix.txt", row.names = FALSE, col.names = FALSE)

## shapes ----
shapes_png <- readPNG(shapes_file)
shapes_mat <- shapes_png[,,1]
thr_value <- 0.5
shapes_df <- as.data.table(reshape2::melt(shapes_mat))
shapes_df[value >= thr_value, value := 1]
shapes_df[value < thr_value, value := 0]
shapes_df[, logic_value := as.logical(value)]
shapes_df[, inv_value := as.numeric(!logic_value)]
shapes_df[, rev_Var1 := rev(Var1)]

ggplot(shapes_df, aes(x = Var2, y = rev_Var1, fill = inv_value)) +
  geom_raster() +
  scale_fill_gradient2("", low = "white", high = "black") +
  labs(x = "", y = "") +
  theme_minimal()

shapes_mat_out <- shapes_df[, .(rev_Var1, Var2, inv_value)]
setorder(shapes_mat_out, rev_Var1, Var2)
shapes_mat_out <- dcast(shapes_mat_out, rev_Var1 ~ Var2, value.var = "inv_value")
shapes_mat_out <- as.matrix(shapes_mat_out[, -1])
image(t(shapes_mat_out))
write.table(shapes_mat_out, file = "data/shapes_coefficient_matrix.txt", row.names = FALSE, col.names = FALSE)

# Create data sets ----
## Libraries ----
library(bayestensorreg)

## Data ----
data_dir <- "data/"
R3 <- read.table(list.files(data_dir, pattern = "R3_coefficient", full.names = TRUE))
R5 <- read.table(list.files(data_dir, pattern = "R5_coefficient", full.names = TRUE))
shapes <- read.table(list.files(data_dir, pattern = "shapes_coefficient", full.names = TRUE))
hawk <- read.table(list.files(data_dir, pattern = "hawk_coefficient.+\\.txt", full.names = TRUE))
horse <- read.table(list.files(data_dir, pattern = "horse_coefficient", full.names = TRUE))
palm <- read.table(list.files(data_dir, pattern = "palm_coefficient", full.names = TRUE))

## Simulate tensor covariate ----
n <- 1000
sig2 <- 1
p <- 64

set.seed(1)
X <- array(rnorm(p*p*n, sd = sqrt(sig2)), dim = c(p,p,n))
X_mat <- bayestensorreg:::mode_k_matriz(X, 3)

## Simulate responses ----
y_R3 <- X_mat %*% unlist(R3) + rnorm(n, sd = 1)
y_R5 <- X_mat %*% unlist(R5) + rnorm(n, sd = 1)
y_shapes <- X_mat %*% unlist(shapes) + rnorm(n, sd = 1)
y_hawk <- X_mat %*% unlist(hawk) + rnorm(n, sd = 1)
y_horse <- X_mat %*% unlist(horse) + rnorm(n, sd = 1)
y_palm <- X_mat %*% unlist(palm) + rnorm(n, sd = 1)

## Save values ----
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
