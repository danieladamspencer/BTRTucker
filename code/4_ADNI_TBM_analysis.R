# This is a script to analyze the ADNI TBM maps data
# The TBM maps are considered here because they come preprocessed, and
# may make the analysis more reproducible

# # Read in the data ----
# data_dir <- "~/github/BTRTucker/data/ADNI/ADNI 11"
# subjects <- list.files(data_dir) |> grep(pattern = "_S_", value = T)
# library(oro.nifti)
# library(fslr)
#
# # icbm_template <- readNIfTI("~/Downloads/ICBM_Template.nii.gz")
# # icbm_labels <- readNIfTI("~/Downloads/ICBM_labels.nii.gz")
#
# # source("https://neuroconductor.org/neurocLite.R")
# # neuro_install("MNITemplate")
# # library(MNITemplate)
#
# # mni_template <- readMNI()
#
# # job::job({
#   subject_imgs <- sapply(subjects, function(subject) {
#     subject_file <- list.files(file.path(data_dir, subject), full.names = T, recursive = T) |>
#       grep(pattern = ".nii", value = T)
#     print(subject_file)
#     subject_img <- try(readNIfTI(subject_file))
#     while(class(subject_img) == "try-error") subject_img <- try(readNIfTI(subject_file))
#     # resamp_img <- try(fsl_resample(subject_img,voxel_size = 2))
#     # while(class(resamp_img) == "try-error") resamp_img <- try(fsl_resample(subject_file,voxel_size = 2))
#     # return(resamp_img@.Data)
#     # use this to get a slice of full-res data
#     return(subject_img@.Data[,,80])
#   }, simplify = "array")
# # }, import = c(subjects, data_dir), packages = c("oro.nifti","fslr"))
# # # saveRDS(subject_imgs, file = file.path(data_dir,"4_ADNI_TBM_subject_imgs_2mm.rds"))
# saveRDS(subject_imgs, file = file.path(data_dir,"4_ADNI_TBM_subject_imgs_slice080.rds"))
# subject_imgs <- readRDS(file.path(data_dir, "4_ADNI_TBM_subject_imgs_slice080.rds"))
# # subject_imgs <- readRDS(file.path(data_dir, "4_ADNI_TBM_subject_imgs_2mm.rds"))
#
# library(ADNIMERGE)
# # ADNI EDA
# adni_df <- subset(adnimerge, PTID %in% subjects & !is.na(MMSE))
# adni_df <- adni_df[,!grepl(pattern = ".bl",names(adni_df))]
# adni_df <- subset(adni_df, VISCODE == "bl")
# missing_cols <- apply(adni_df, 2, function(x) sum(is.na(x)))
# adni_df <- adni_df[,missing_cols == 0]
# adni_df <-
#   subset(
#     adni_df,
#     select = -c(
#       RID, # Identifier
#       COLPROT, # Collection procedure
#       ORIGPROT, # Collection procedure
#       PTID, # Identifier
#       SITE, # Identifier
#       VISCODE, # Visit code
#       EXAMDATE, # Date
#       FLDSTRENG, # Collection procedure
#       FSVERSION, # Collection procedure
#       IMAGEUID, # Identifier
#       ICV, # Collection procedure
#       DX, # Diagnosis, not a predictor
#       mPACCdigit, # Another clinical score
#       mPACCtrailsB, # Another clinical score
#       Month, # Date
#       M, # Date
#       CDRSB, # Another clinical score
#       ADASQ4,# Another clinical score
#       LDELTOTAL,# Another clinical score
#       PTRACCAT, # racial category
#       PTETHCAT # ethnic category
#     )
#   )
# adni_lm <- lm(MMSE ~ ., data = adni_df)
# aic_select <- step(adni_lm)
# final_adni_df <- subset(adni_df, select = c(MMSE, AGE, PTEDUCAT, APOE4))
# final_adni_lm <- lm(MMSE ~ . - AGE, final_adni_df)
# summary(final_adni_lm) # Based on this, only include PTEDUCAT and APOE4
#
# adni_df <- subset(adnimerge, PTID %in% subjects & !is.na(MMSE), select = c(PTID,VISCODE,AGE,PTEDUCAT, PTGENDER,APOE4,MMSE))
# adni_bl <- subset(adni_df, VISCODE == "bl")
# adni_bl <- adni_bl[order(adni_bl$PTID),] # This is important. Make sure the order is the same
# adni_bl$PTGENDER <- ifelse(adni_bl$PTGENDER == "Male",0,1)
#
# library(bayestensorreg)
# tbm_data <- as.TR_data(adni_bl$MMSE,subject_imgs,eta = cbind(1,as.matrix(adni_bl[,3:6])))
# saveRDS(tbm_data, file.path(data_dir,"4_ADNI_TBM_slice080_TRdata.rds"))

# # > Mask the ADNI data ----
# data_dir <- "~/github/BTRTucker/data/"
# aal_mdt <- readNIfTI(file.path(data_dir, "aalMDT.nii.gz"))
# insideAAL <- aal_mdt@.Data[,,80]
# insideAAL[insideAAL > 0] <- 1
# tbm_data <- readRDS(list.files(data_dir, pattern = "slice080_TRdata", full.names = TRUE))
# n <- length(tbm_data$y)
# new_X <- tbm_data$X * (insideAAL %o% rep(1,n))
# new_X2 <- array(as.integer(new_X), dim = dim(new_X)) # Since the values are integers anyway, this saves space
# masked_tbm <- as.TR_data(y = tbm_data$y, X = new_X2, eta = tbm_data$eta)
# saveRDS(masked_tbm, file = file.path(data_dir,"4_ADNI_TBM_slice080_masked_TRdata,rds"))

# ANALYSIS ----
# > Bayes Tucker ----
# ranks <- as.matrix(expand.grid(1:4,1:4))
# library(parallel)
# cl <- makeCluster(8) # Number of parallel cores
# parApply(cl, ranks, 1, function(r) {
  r <- c(3,3)
  S <- 11000
  B <- 1000
  library(bayestensorreg)
  data_dir <- "data/"
  result_dir <- "results/"
  tbm_data <- readRDS(list.files(data_dir, pattern = "slice080_TRdata", full.names = TRUE))
  tbm_data$y <- (tbm_data$y - mean(tbm_data$y))
  tbm_data$eta <- tbm_data$eta[,c(3,5)] # Education and APOE4
  apoe4_factor <- sapply(0:2, function(x) as.numeric(tbm_data$eta[,2] == x))
  colnames(apoe4_factor) <- paste0("APOE4_",0:2)
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
  saveRDS(btr_tucker, file.path(result_dir,paste0("4_ADNI_TBM_EduAPOEfactor_BTRTucker_",
                                                  (S-B)/1000,"k_rank",
                                                  paste(r,collapse = ""),".rds")))
  # return(NULL)
# })

# # >  Bayes CP ----
# library(parallel)
# cl <- makeCluster(4) # Number of parallel cores
# parSapply(cl, 1:4, function(r) {
#   library(bayestensorreg)
#   data_dir <- "~/github/BTRTucker/data/ADNI/ADNI 11"
#   result_dir <- "~/github/BTRTucker/results/ADNI"
#   tbm_data <- readRDS(file.path(data_dir,"4_ADNI_TBM_slice080_TRdata.rds"))
#   tbm_data$y <- (tbm_data$y - mean(tbm_data$y))
#   tbm_data$eta <- tbm_data$eta[,-1]
#   set.seed(47408)
#   btr_cp <-
#     try(BTR_CP(
#       input = tbm_data,
#       max_rank = r,
#       n_iter = 100,
#       n_burn = 0,
#       hyperparameters = NULL,
#       save_dir = "~/Desktop"
#     ))
#   saveRDS(btr_cp, file.path(result_dir,paste0("4_ADNI_TBM_BTR_CP_rank",paste(r,collapse = ""),".rds")))
#   return(NULL)
# })

# # > FTR Tucker ----
# ranks <- as.matrix(expand.grid(1:4,1:4))
# ranks <- matrix(1:4,nrow = 4, ncol = 2)
# library(parallel)
# cl <- makeCluster(4) # Number of parallel cores
# parApply(cl, ranks, 1, function(r) {
#   library(bayestensorreg)
#   data_dir <- "~/github/BTRTucker/data/ADNI/ADNI 11"
#   result_dir <- "~/github/BTRTucker/results/ADNI/After_EDA"
#   tbm_data <- readRDS(file.path(data_dir,"4_ADNI_TBM_slice080_TRdata.rds"))
#   tbm_data$y <- (tbm_data$y - mean(tbm_data$y))
#   tbm_data$eta <- tbm_data$eta[,-c(1,2,4)]
#   set.seed(47408)
#   ftr_tucker <-
#     try(FTRTucker(
#       input = tbm_data,
#       ranks = r,
#       epsilon = 1e-1,
#       betas_LASSO = F
#     ))
#   saveRDS(ftr_tucker, file.path(result_dir,paste0("4_ADNI_TBM_FTRTucker_rank",paste(r,collapse = ""),".rds")))
#   return(NULL)
# })
# # > FTR CP ----
# library(parallel)
# cl <- makeCluster(4) # Number of parallel cores
# parSapply(cl, 1:4, function(r) {
# library(bayestensorreg)
# data_dir <- "~/github/BTRTucker/data/ADNI/ADNI 11"
# result_dir <- "~/github/BTRTucker/results/ADNI/After_EDA"
# tbm_data <- readRDS(file.path(data_dir,"4_ADNI_TBM_slice080_TRdata.rds"))
# tbm_data$y <- (tbm_data$y - mean(tbm_data$y))
# tbm_data$eta <- tbm_data$eta[,-c(1,2,4)]
# # for(r in 1:4) {
#   set.seed(47408)
#   ftr_cp <-
#     FTR_CP(
#       input = tbm_data,
#       rank = r,
#       epsilon = 1e-1
#     )
#   saveRDS(ftr_cp, file.path(result_dir,paste0("4_ADNI_TBM_FTR_CP_rank",paste(r,collapse = ""),".rds")))
# # }
# return(NULL)
# })
#
#   # return(NULL)
# # })
# # stopCluster(cl)
# # > GLM ----
# library(bayestensorreg)
# data_dir <- "~/github/BTRTucker/data/ADNI/ADNI 11"
# result_dir <- "~/github/BTRTucker/results/ADNI/After_EDA"
# tbm_data <- readRDS(file.path(data_dir,"4_ADNI_TBM_slice080_TRdata.rds"))
# tbm_data$y <- (tbm_data$y - mean(tbm_data$y))
# tbm_data$eta <- tbm_data$eta[,-c(1,2,4)]
# glm_tbm_gam <- lm(tbm_data$y~-1 + tbm_data$eta)$coefficients
# glm_tbm_ytil <- lm(tbm_data$y~-1 + tbm_data$eta)$residuals
# library(parallel)
# cl <- makeCluster(8)
# glm_tbm_B <- parApply(cl,tbm_data$X, 1:2, function(x,ytil){
#   return(lm(ytil~ -1 + x)$coefficients)
# }, ytil = glm_tbm_ytil)
# glm_tbm_pval <- parApply(cl,tbm_data$X, 1:2, function(x,ytil){
#   return(summary(lm(ytil~ -1 + x))$coefficients[4])
# }, ytil = glm_tbm_ytil)
# stopCluster(cl)
# glm_tbm_pval2 <- matrix(p.adjust(c(glm_tbm_pval),"BH"), nrow = nrow(glm_tbm_pval),ncol = ncol(glm_tbm_pval))
# glm_tbm_signif <- array(as.numeric(glm_tbm_pval2 < 0.05), dim = dim(glm_tbm_pval2))
# final_glm_tbm_B <- glm_tbm_B * glm_tbm_signif
# glm_tbm_fitted <- c(tbm_data$eta %*% glm_tbm_gam) + c(crossprod(c(final_glm_tbm_B),t(kFold(tbm_data$X,3))))
# glm_tbm_resid <- tbm_data$y - glm_tbm_fitted
# glm_tbm_llik <- sum(dnorm(glm_tbm_resid,sd = sd(glm_tbm_resid), log = T))
# glm_tbm <- list(
#   B = glm_tbm_B,
#   B_signif = glm_tbm_signif,
#   gam = glm_tbm_gam,
#   fitted = glm_tbm_fitted,
#   llik = glm_tbm_llik
# )
# saveRDS(glm_tbm, file = file.path(result_dir,"4_ADNI_TBM_GLM.rds"))
# # RESULTS ----
# # > Bayes Tucker ----
# result_dir <- "~/github/BTRTucker/results/ADNI"
# result_files <- list.files(result_dir, full.names = T) |>
#   grep(pattern = "TBM_BTRTucker", value = T)
# rankGrid <- as.matrix(expand.grid(1:4,1:4))
# library(bayestensorreg)
# dicVals <- apply(rankGrid,1, function(r) {
#   rFile <- grep(paste0("rank",paste(r,collapse = ""),".rds"),result_files, value = T)
#   res <- readRDS(rFile)
#   dicOut <- DIC(res$llik, burn_in = 500)
#   return(dicOut)
# })
# cbind(rankGrid,dicVals)[order(dicVals),]
# rankGrid[which.min(dicVals),]
# result <- readRDS(file.path(result_dir,"4_ADNI_TBM_BTRTucker_rank32.rds"))
# plot(result$llik, type= 'l')
# B <- BTRT_final_B(result)
# tile.plot(B)
# library(MNITemplate)
# mni_template <- readMNI()
# tile.plot(mni_template@.Data[,,91])
# library(oro.nifti)
# mdt_template <- readNIfTI("~/Downloads/ADNI_MDT/ADNI_ICBM9P_mni_4step_MDT.nii.gz")
# library(tidyverse)
# template_grob <-
#   # mni_template@.Data[,,91] |>
#   mdt_template@.Data[,,110] |>
#   reshape2::melt() |>
#   ggplot() +
#   geom_raster(aes(x = Var1, y = Var2, fill = value)) +
#   scale_fill_distiller(palette = "Greys") +
#   guides(fill = "none") +
#   theme_void() +
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0))
# template_grob <- ggplotGrob(template_grob)
#
# reshape2::melt(B) |>
#   # mutate(value = ifelse(value == 0, NA, value)) |>
#   ggplot() +
#   annotation_custom(grob = template_grob) +
#   geom_raster(aes(x = Var1, y = Var2, fill = value, alpha = value / max(abs(value),na.rm = T))) +
#   scale_fill_gradient2("") +
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   guides(fill = "none", alpha = "none") +
#   theme_void()
#
#
# mdt_out <- mdt_template
# mdt_out@.Data <- array(0,dim = dim(mdt_out@.Data))
# mdt_out@.Data[,,110] <- B
# writeNIfTI(mdt_out, filename = "~/Downloads/ADNI_MDT/BTRT_final_B_out")
#
# # > Bayes CP ----
#
# # > FTR Tucker ----
# result_dir <- "~/github/BTRTucker/results/ADNI"
# result_files <- list.files(result_dir, full.names = T) |>
#   grep(pattern = "TBM_FTRTucker", value = T)
# rankGrid <- as.matrix(expand.grid(1:4,1:4))
# library(bayestensorreg)
# llikVals <- apply(rankGrid,1, function(r) {
#   rFile <- grep(paste0("rank",paste(r,collapse = ""),".rds"),result_files, value = T)
#   res <- readRDS(rFile)
#   llik_out <- res$llik
#   return(llik_out)
# })
# rankGrid[which.min(llikVals),]
# ftr_out <- readRDS(file.path(result_dir,"4_ADNI_TBM_FTRTucker_rank24.rds"))
#
# reshape2::melt(ftr_out$B) |>
#   # mutate(value = ifelse(value == 0, NA, value)) |>
#   ggplot() +
#   annotation_custom(grob = template_grob) +
#   geom_raster(aes(x = Var1, y = Var2, fill = value, alpha = value / max(abs(value),na.rm = T))) +
#   scale_fill_gradient2("") +
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   guides(fill = "none", alpha = "none") +
#   theme_void()
