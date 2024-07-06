# Perform the analysis of the ADNI TBM data using the different methods
# Bayes Tucker ----
ranks <- as.matrix(expand.grid(1:4,1:4))
library(parallel)
cl <- makeCluster(8) # Number of parallel cores
parApply(cl, ranks, 1, function(r) {
  # r <- c(3,3)
  S <- 11000
  B <- 1000
  library(bayestensorreg)
  data_dir <- "data/"
  result_dir <- "results/"
  tbm_data <- readRDS(list.files(data_dir, pattern = "slice080_TRdata", full.names = TRUE))
  tbm_data$y <- (tbm_data$y - mean(tbm_data$y))
  tbm_data$eta <- tbm_data$eta[,c(3,5)] # Education and APOE4
  apoe4_factor <- sapply(1:2, function(x) as.numeric(tbm_data$eta[,2] == x))
  colnames(apoe4_factor) <- paste0("APOE4_",1:2)
  tbm_data$eta <- cbind(tbm_data$eta, apoe4_factor)
  tbm_data$eta <- tbm_data$eta[,-2]
  set.seed(47408)
  btr_tucker <-
    BTRTucker(
      input = tbm_data,
      ranks = r,
      n_iter = S,
      n_burn = B,
      hyperparameters = NULL,
      save_dir = NULL
    )
  saveRDS(btr_tucker, file.path(result_dir,paste0("05_ADNI_TBM_EduAPOEfactor2_BTRTucker_",
                                                  (S-B)/1000,"k_rank",
                                                  paste(r,collapse = ""),".rds")))
  return(NULL)
})

# Bayes CP ----
library(parallel)
cl <- makeCluster(4) # Number of parallel cores
parSapply(cl, 1:4, function(r) {
# r <- 1
S <- 11000
B <- 1000
library(bayestensorreg)
data_dir <- "data/"
result_dir <- "results/"
tbm_data <- readRDS(file.path(data_dir,"04_ADNI_TBM_slice080_TRdata.rds"))
tbm_data$y <- (tbm_data$y - mean(tbm_data$y))
tbm_data$eta <- tbm_data$eta[,c(3,5)] # Education and APOE4
set.seed(47408)
btr_cp <-
  try(BTRTucker(
    input = tbm_data,
    ranks = rep(r, 2),
    n_iter = S,
    n_burn = B,
    CP = TRUE,
    hyperparameters = NULL,
    save_dir = NULL
  ))
saveRDS(btr_cp, file.path(result_dir,paste0("05_ADNI_TBM_BTR_CP_rank",r,".rds")))
return(NULL)
})
parallel::stopCluster(cl)

# FTR Tucker ----
ranks <- as.matrix(expand.grid(1:4,1:4))
ranks <- matrix(1:4,nrow = 4, ncol = 2)
library(parallel)
cl <- makeCluster(4) # Number of parallel cores
parApply(cl, ranks, 1, function(r) {
  library(bayestensorreg)
  data_dir <- "~/github/BTRTucker/data/ADNI/ADNI 11"
  result_dir <- "~/github/BTRTucker/results/ADNI/After_EDA"
  tbm_data <- readRDS(file.path(data_dir,"04_ADNI_TBM_slice080_TRdata.rds"))
  tbm_data$y <- (tbm_data$y - mean(tbm_data$y))
  tbm_data$eta <- tbm_data$eta[,-c(1,2,4)]
  set.seed(47408)
  ftr_tucker <-
    try(FTRTucker(
      input = tbm_data,
      ranks = r,
      epsilon = 1e-1,
      betas_LASSO = F
    ))
  saveRDS(ftr_tucker, file.path(result_dir,paste0("05_ADNI_TBM_FTRTucker_rank",paste(r,collapse = ""),".rds")))
  return(NULL)
})

# FTR CP ----
library(parallel)
cl <- makeCluster(4) # Number of parallel cores
parSapply(cl, 1:4, function(r) {
library(bayestensorreg)
data_dir <- "data/ADNI/ADNI 11"
result_dir <- "results/ADNI/After_EDA"
tbm_data <- readRDS(file.path(data_dir,"04_ADNI_TBM_slice080_TRdata.rds"))
tbm_data$y <- (tbm_data$y - mean(tbm_data$y))
tbm_data$eta <- tbm_data$eta[,-c(1,2,4)]
  set.seed(47408)
  ftr_cp <-
    FTR_CP(
      input = tbm_data,
      rank = r,
      epsilon = 1e-1
    )
  saveRDS(ftr_cp, file.path(result_dir,paste0("05_ADNI_TBM_FTR_CP_rank",paste(r,collapse = ""),".rds")))
return(NULL)
})

# GLM ----
library(bayestensorreg)
data_dir <- "data/ADNI/ADNI 11"
result_dir <- "results/ADNI/After_EDA"
tbm_data <- readRDS(file.path(data_dir,"04_ADNI_TBM_slice080_TRdata.rds"))
tbm_data$y <- (tbm_data$y - mean(tbm_data$y))
tbm_data$eta <- tbm_data$eta[,-c(1,2,4)]
glm_tbm_gam <- lm(tbm_data$y~-1 + tbm_data$eta)$coefficients
glm_tbm_ytil <- lm(tbm_data$y~-1 + tbm_data$eta)$residuals
library(parallel)
cl <- makeCluster(8)
glm_tbm_B <- parApply(cl,tbm_data$X, 1:2, function(x,ytil){
  return(lm(ytil~ -1 + x)$coefficients)
}, ytil = glm_tbm_ytil)
glm_tbm_pval <- parApply(cl,tbm_data$X, 1:2, function(x,ytil){
  return(summary(lm(ytil~ -1 + x))$coefficients[4])
}, ytil = glm_tbm_ytil)
stopCluster(cl)
glm_tbm_pval2 <- matrix(p.adjust(c(glm_tbm_pval),"BH"), nrow = nrow(glm_tbm_pval),ncol = ncol(glm_tbm_pval))
glm_tbm_signif <- array(as.numeric(glm_tbm_pval2 < 0.05), dim = dim(glm_tbm_pval2))
final_glm_tbm_B <- glm_tbm_B * glm_tbm_signif
glm_tbm_fitted <- c(tbm_data$eta %*% glm_tbm_gam) + c(crossprod(c(final_glm_tbm_B),t(kFold(tbm_data$X,3))))
glm_tbm_resid <- tbm_data$y - glm_tbm_fitted
glm_tbm_llik <- sum(dnorm(glm_tbm_resid,sd = sd(glm_tbm_resid), log = T))
glm_tbm <- list(
  B = glm_tbm_B,
  B_signif = glm_tbm_signif,
  gam = glm_tbm_gam,
  fitted = glm_tbm_fitted,
  llik = glm_tbm_llik
)
saveRDS(glm_tbm, file = file.path(result_dir,"05_ADNI_TBM_GLM.rds"))
