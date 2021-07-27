# This is a script for running the hyperparameter sensitivity

# Set up the grid of hyperparameter values ----
var_exp <- c(0.1,1,10)
a.lam <- 3*var_exp
a.sig <- 3*var_exp
b.sig <- 20*var_exp
a.tau <- 1*var_exp
a.u <- 3*var_exp
a.z <- 1*var_exp

varying_hyper <-
  expand.grid(
    a.lam = a.lam,
    a.sig = a.sig,
    b.sig = b.sig,
    a.tau = a.tau,
    a.u = a.u,
    a.z = a.z
  )
varying_hyper$row <- as.numeric(row(varying_hyper)[,1])

set.seed(95064)
select_row <- sample(seq(nrow(varying_hyper)), size = 100, replace = F)
varying_hyper <- varying_hyper[select_row,]

# Set up the parallelization ----
library(parallel)
cl <- makeCluster(min(length(select_row),30, detectCores() - 1))
parApply(cl,varying_hyper,1,function(vh) {
  library(bayestensorreg)
  set.seed(831)
  sim_data <-
    TR_simulated_data(
      subjects = 1000,
      tensor_dims = c(50, 50),
      CNR = 1,
      num_active = 3,
      other_covar = c(25, 3, 0.1)
    )
  D <- 2
  test_hyperpars <- list(
    a.lam = vh$a.lam,
    b.lam = vh$a.lam^(1/(2*D)),
    a.sig = vh$a.sig,
    b.sig = vh$b.sig,
    a.tau = vh$a.tau,
    a.u = vh$a.u,
    b.u = vh$a.u^(1/(2*D)),
    a.z = vh$a.z
  )
  btrtucker_result <-
    BTRTucker(
      input = sim_data,
      ranks = c(3, 3),
      n_iter = 1100,
      n_burn = 100,
      hyperparameters = test_hyperpars,
      save_dir = NULL
    )
  saveRDS(btrtucker_result, paste0("results/2_btrtucker_hyperparam_row",vh$row,".rds"))
  return(NULL)
})
stopCluster(cl)
