# This is a script to perform the simulated data analysis from "Parsimonious
# Bayesian Sparse Tensor Regression Using the Tucker Tensor Decomposition" by
# Spencer, Guhaniyogi, and Prado (2021)
# # Simulated data with banded nonzero regions ----
# library(bayestensorreg)
# subjects = 400
# tensor_dims = c(50, 50)
# CNR = 1
# num_active = 1
# other_covar = c(1, 1)
#
# B <- Reduce(`+`, sapply(seq(num_active), function(zz) {
#   neuRosim::specifyregion(
#     dim = tensor_dims,
#     coord = sapply(tensor_dims, function(z)
#       sample(seq(z), size = 1)),
#     radius = ceiling(min(tensor_dims) * runif(1, 0.02, 0.1)),
#     form = "sphere",
#     fading = runif(1, 0.5, 1)
#   )
# }, simplify = FALSE))
#
# B[10,] <- 0
# image(B)
# eta <-
#   matrix(rnorm(subjects * length(other_covar)), subjects, length(other_covar))
# gam <- other_covar
# X <-
#   array(rnorm(prod(tensor_dims) * subjects), dim = c(tensor_dims, subjects))
# y <-
#   apply(X, length(dim(X)), function(xx)
#     sum(xx * B * CNR)) + c(eta %*% gam) + rnorm(subjects)
# banded_signal <- list(
#   y = y,
#   X = X,
#   true_B = B,
#   eta = eta,
#   gam = gam
# )
#
# btrt_ranks <- expand.grid(R1 = 1:2, R2 = 1:2)
# library(parallel)
# cl <- makeCluster(4)
# clusterExport(cl,"banded_signal")
# parApply(cl,btrt_ranks,1,function(rr) {
#   library(bayestensorreg)
#   save_dir <- "~/github/BTRTucker/results/data_simulations"
#   res <- BTRTucker(input = banded_signal, ranks = as.numeric(rr),n_iter = 1100, n_burn = 100,hyperparameters = NULL,save_dir = NULL)
#   saveRDS(res, file.path(save_dir,paste0("1_banded_signal_btrt_rank",paste(as.numeric(rr),collapse = ""),".rds")))
# })
# stopCluster(cl)

# # Simulated data with different ranks ----
# library(bayestensorreg)
# subjects = 400
# tensor_dims = c(50, 50, 50)
# CNR = 1
# num_active = 1
# other_covar = c(1, 1)
#
# ranks <- c(2, 5, 2)
# set.seed(95064)
# betas <- mapply(function(r, p) {
#   out <- sapply(seq(r), function(rr){
#     start_point <- round(runif(1, 0.1, 0.9) * p)
#     lgth <- round(runif(1, 0.15, 0.2) * p)
#     end_point <- start_point + lgth
#     if(end_point > p) end_point <- p
#     out <- rep(0, p)
#     out[start_point:end_point] <- 1
#     return(out)
#   })
# }, r = ranks, p = tensor_dims)
#
# G <- array(runif(prod(ranks), 0.5, 1), dim = ranks)
#
# B <- composeTuckerCore(betas, G)
#
# # Exploring the highest-value margins
# # (big.i <- which.max(apply(B, 1, function(x) sum(x > 0))))
# # (big.j <- which.max(apply(B, 2, function(x) sum(x > 0))))
# # (big.k <- which.max(apply(B, 3, function(x) sum(x > 0))))
# # image(B[big.i,,])
# # image(B[,big.j,])
# # image(B[,,big.k])
#
# eta <-
#   matrix(rnorm(subjects * length(other_covar)), subjects, length(other_covar))
# gam <- other_covar
# X <-
#   array(rnorm(prod(tensor_dims) * subjects), dim = c(tensor_dims, subjects))
# y <-
#   apply(X, length(dim(X)), function(xx)
#     sum(xx * B * CNR)) + c(eta %*% gam) + rnorm(subjects)
# diff_ranks <- list(
#   y = y,
#   X = X,
#   true_B = B,
#   eta = eta,
#   gam = gam
# )
#
# # auto_rank_btrt <- function(tr_data, n.iter = 1100, n.burn = 100) {
# #   # Debugging
# #   tr_data <- as.TR_data(y = diff_ranks$y, X = diff_ranks$X, eta = diff_ranks$eta)
# #   n.iter = 110
# #   n.burn = 10
# #   # Begin
# #   require(bayestensorreg, quietly = TRUE)
# #   D <- length(dim(tr_data$X)) - 1
# #   # First, do on-diagonals until the DIC increases
# #   cat("Beginning core-diagonal fits....\n")
# #   STOP <- FALSE
# #   R <- 1
# #   rr <- rep(R, D)
# #   old_btrt_result <- BTRTucker(tr_data, ranks = rr, n_iter = n.iter, n_burn = n.burn, hyperparameters = NULL, save_dir = NULL)
# #   old_dic <- DIC(old_btrt_result$llik, burn_in = n.burn)
# #   cat("Rank ", paste(rr, collapse = ","), " model DIC: ", old_dic,"\n")
# #   while(!STOP) {
# #     R <- R + 1
# #     rr <- rep(R, D)
# #     btrt_result <- BTRTucker(tr_data, ranks = rr, n_iter = n.iter, n_burn = n.burn, hyperparameters = NULL, save_dir = NULL)
# #     new_dic <- DIC(btrt_result$llik, burn_in = n.burn)
# #     cat("Rank ", paste(rr, collapse = ","), " model DIC: ", new_dic,"\n")
# #     if (new_dic > old_dic) {
# #       STOP <- TRUE
# #     } else {
# #       old_btrt_result <- btrt_result
# #       old_dic <- new_dic
# #     }
# #   }
# #   cat("Core diagonal fits completed! Starting off-diagonal fits...\n")
# #   diag_dic <- old_dic
# #   STOP <- FALSE
# #   while(!STOP) {
# #     rr_grp <- sapply(seq(D), function(lil_d) {
# #       out <- rr
# #       out[lil_d] <- rr[lil_d] - 1
# #       return(out)
# #     })
# #     for(j in seq(D)) {
# #       rr <- rr_grp[j,]
# #       btrt_result <- BTRTucker(tr_data, ranks = rr, n_iter = n.iter, n_burn = n.burn, hyperparameters = NULL, save_dir = NULL)
# #       new_dic <- DIC(btrt_result$llik, burn_in = n.burn)
# #       cat("Rank ", paste(rr, collapse = ","), " model DIC: ", new_dic,"\n")
# #       if (new_dic < old_dic) {
# #         old_btrt_result <- btrt_result
# #         old_dic <- new_dic
# #       }
# #     }
# #
# #   }
# #
# # }
#
# library(parallel)
# cl <- makeCluster(3)
# clusterExport(cl,"diff_ranks")
# parApply(cl,sapply(5:1,rep,3),2,function(rr) {
#   library(bayestensorreg)
#   save_dir <- "results/simulated_diff_rank252/"
#   res <- BTRTucker(input = diff_ranks, ranks = as.numeric(rr),n_iter = 1100, n_burn = 100,hyperparameters = NULL,save_dir = NULL)
#   saveRDS(res, file.path(save_dir,paste0("1_diff_rank_btrt_rank",paste(as.numeric(rr),collapse = ""),".rds")))
# })
# stopCluster(cl)
#
# save_dir <- "results/simulated_diff_rank252/"
# res_files <- list.files(save_dir, full.names = TRUE)
# library(bayestensorreg)
# all_dic <- sapply(res_files, function(x) {
#   res <- readRDS(x)
#   return(DIC(res$llik, burn_in = 100))
# })
# which.min(all_dic)
# # /home/dan/github/BTRTucker/results/simulated_diff_rank//1_diff_rank_btrt_rank555.rds
# #
# # Now run down the possibilities for the max rank 5 - 1
# # start_rank <- rep(5,3)
# # all_ranks <- sapply(1:3, function(j) {
# #   out <- start_rank
# #   out[j] <- out[j] - 1
# #   return(out)
# # })

# # > Off-diagonal exploration ----
# all_ranks <- expand.grid(5:1, 5:1, 5:1)
# # Keep only rows with at least one 5
# num_5 <- apply(all_ranks, 1, function(x) {return(sum(x == 5))})
# have_5 <- all_ranks[(num_5 > 0 & num_5 < 3),]
# # Reorder so the models with ranks closest to 5 go first
# have_5 <- have_5[order(apply(have_5, 1, sum), decreasing = TRUE),]
#
# library(parallel)
# cl <- makeCluster(3)
# # clusterExport(cl,"diff_ranks")
# parApply(cl,have_5,1,function(rr) {
#   library(bayestensorreg)
#   save_dir <- "~/github/BTRTucker/results/simulated_diff_rank/"
#   diff_ranks <- readRDS(list.files(save_dir, pattern = "diff_ranks_data.rds", full.names = TRUE))
#   set.seed(95064)
#   res <- BTRTucker(input = diff_ranks, ranks = as.numeric(rr),n_iter = 1100, n_burn = 100,hyperparameters = NULL,save_dir = NULL)
#   saveRDS(res, file.path(save_dir,paste0("1_diff_rank_btrt_rank",paste(as.numeric(rr),collapse = ""),".rds")))
# })
# stopCluster(cl)
#
# save_dir <- "~/github/BTRTucker/results/simulated_diff_rank/"
# res_files <- list.files(save_dir, pattern = "rank[1-5]{3}.rds", full.names = TRUE)
# library(bayestensorreg)
# all_dic <- sapply(res_files, function(x) {
#   res <- readRDS(x)
#   return(DIC(res$llik, burn_in = 100))
# })
# which.min(all_dic)

