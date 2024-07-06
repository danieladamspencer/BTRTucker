# Generate simulated data
# Setup ----
save_dir <- "data/"
# This package, available on GitHub at 'danieladamspencer/bayestensorreg' is
# required for simulating data and performing analyses
library(bayestensorreg)
# General simulated data ----
# Set seed for reproducibility
set.seed(95064)
sim_data <-
  TR_simulated_data(
    subjects = 1000,
    tensor_dims = c(50, 50),
    CNR = 1,
    num_active = 3,
    other_covar = c(25, 3, 0.1)
  )
saveRDS(sim_data, file = file.path(save_dir,"01_simulated_data.rds"))

# Simulated data with banded nonzero regions ----
# Set seed for reproducibility
set.seed(95064)
subjects = 400
tensor_dims = c(50, 50)
CNR = 1
num_active = 1
other_covar = c(1, 1)

B <- Reduce(`+`, sapply(seq(num_active), function(zz) {
  neuRosim::specifyregion(
    dim = tensor_dims,
    coord = sapply(tensor_dims, function(z)
      sample(seq(z), size = 1)),
    radius = ceiling(min(tensor_dims) * runif(1, 0.02, 0.1)),
    form = "sphere",
    fading = runif(1, 0.5, 1)
  )
}, simplify = FALSE))

B[10,] <- 0
image(B)
eta <-
  matrix(rnorm(subjects * length(other_covar)), subjects, length(other_covar))
gam <- other_covar
X <-
  array(rnorm(prod(tensor_dims) * subjects), dim = c(tensor_dims, subjects))
y <-
  apply(X, length(dim(X)), function(xx)
    sum(xx * B * CNR)) + c(eta %*% gam) + rnorm(subjects)
banded_signal <- list(
  y = y,
  X = X,
  true_B = B,
  eta = eta,
  gam = gam
)
saveRDS(banded_signal, file = file.path(save_dir,"01_banded_signal_data.rds"))

# Simulated data with different ranks ----
# Set seed for reproducibility
set.seed(95064)
subjects = 400
tensor_dims = c(50, 50, 50)
CNR = 1
num_active = 1
other_covar = c(1, 1)

ranks <- c(2, 5, 2)
set.seed(95064)
betas <- mapply(function(r, p) {
  out <- sapply(seq(r), function(rr){
    start_point <- round(runif(1, 0.1, 0.9) * p)
    lgth <- round(runif(1, 0.15, 0.2) * p)
    end_point <- start_point + lgth
    if(end_point > p) end_point <- p
    out <- rep(0, p)
    out[start_point:end_point] <- 1
    return(out)
  })
}, r = ranks, p = tensor_dims)

G <- array(runif(prod(ranks), 0.5, 1), dim = ranks)

B <- composeTuckerCore(betas, G)

# Exploring the highest-value margins
# (big.i <- which.max(apply(B, 1, function(x) sum(x > 0))))
# (big.j <- which.max(apply(B, 2, function(x) sum(x > 0))))
# (big.k <- which.max(apply(B, 3, function(x) sum(x > 0))))
# image(B[big.i,,])
# image(B[,big.j,])
# image(B[,,big.k])

eta <-
  matrix(rnorm(subjects * length(other_covar)), subjects, length(other_covar))
gam <- other_covar
X <-
  array(rnorm(prod(tensor_dims) * subjects), dim = c(tensor_dims, subjects))
y <-
  apply(X, length(dim(X)), function(xx)
    sum(xx * B * CNR)) + c(eta %*% gam) + rnorm(subjects)
diff_ranks <- list(
  y = y,
  X = X,
  true_B = B,
  eta = eta,
  gam = gam
)
saveRDS(diff_ranks, file = file.path(save_dir,"01_diff_ranks_data.rds"))
