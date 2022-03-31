# This is a script to analyze the ADNI TBM maps data
# The TBM maps are considered here because they come preprocessed, and
# may make the analysis more reproducible

# Read in the data ----
# data_dir <- "~/github/BTRTucker/data/ADNI/ADNI 11"
# subjects <- list.files(data_dir) |> grep(pattern = "_S_", value = T)
# library(oro.nifti)
# library(fslr)

# icbm_template <- readNIfTI("~/Downloads/ICBM_Template.nii.gz")
# icbm_labels <- readNIfTI("~/Downloads/ICBM_labels.nii.gz")

# source("https://neuroconductor.org/neurocLite.R")
# neuro_install("MNITemplate")
# library(MNITemplate)

# mni_template <- readMNI()

# job::job({
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
#     return(subject_img@.Data[,,110])
#   }, simplify = "array")
# }, import = c(subjects, data_dir), packages = c("oro.nifti","fslr"))
# # saveRDS(subject_imgs, file = file.path(data_dir,"4_ADNI_TBM_subject_imgs_2mm.rds"))
# saveRDS(subject_imgs, file = file.path(data_dir,"4_ADNI_TBM_subject_imgs_slice110.rds"))
# subject_imgs <- readRDS(file.path(data_dir, "4_ADNI_TBM_subject_imgs_slice110.rds"))
# subject_imgs <- readRDS(file.path(data_dir, "4_ADNI_TBM_subject_imgs_2mm.rds"))

# library(ADNIMERGE)
# adni_df <- subset(adnimerge, PTID %in% subjects & !is.na(MMSE), select = c(PTID,VISCODE,PTEDUCAT,APOE4,MMSE))
# adni_bl <- subset(adni_df, VISCODE == "bl")
# adni_bl <- adni_bl[order(adni_bl$PTID),] # This is important. Make sure the order is the same
#
# library(bayestensorreg)
# tbm_data <- as.TR_data(adni_bl$MMSE,subject_imgs,eta = cbind(1,as.matrix(adni_bl[,3:4])))
# saveRDS(tbm_data, file.path(data_dir,"4_ADNI_TBM_slice110_TRdata.rds"))

# ANALYSIS ----
# > Tucker ----
ranks <- as.matrix(expand.grid(1:4,1:4))
library(parallel)
cl <- makeCluster(8) # Number of parallel cores
parApply(cl, ranks, 1, function(r) {
  library(bayestensorreg)
  data_dir <- "~/github/BTRTucker/data/ADNI/ADNI 11"
  result_dir <- "~/github/BTRTucker/results/ADNI"
  tbm_data <- readRDS(file.path(data_dir,"4_ADNI_TBM_slice110_TRdata.rds"))
  set.seed(47408)
  btr_tucker <-
    BTRTucker(
      input = tbm_data,
      ranks = r,
      n_iter = 1500,
      n_burn = 500,
      hyperparameters = NULL,
      save_dir = NULL
    )
  saveRDS(btr_tucker, file.path(result_dir,paste0("4_ADNI_TBM_BTRTucker_rank",paste(r,collapse = ""),".rds")))
  return(NULL)
})

# RESULTS ----
# > Tucker ----
result_dir <- "~/github/BTRTucker/results/ADNI"
result_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = ".rds", value = T)
rankGrid <- as.matrix(expand.grid(1:4,1:4))
library(bayestensorreg)
dicVals <- apply(rankGrid,1, function(r) {
  rFile <- grep(paste0("rank",paste(r,collapse = ""),".rds"),result_files, value = T)
  res <- readRDS(rFile)
  dicOut <- DIC(res$llik, burn_in = 500)
  return(dicOut)
})
cbind(rankGrid,dicVals)[order(dicVals),]
rankGrid[which.min(dicVals),]
result <- readRDS(file.path(result_dir,"4_ADNI_TBM_BTRTucker_rank32.rds"))
plot(result$llik, type= 'l')
B <- BTRT_final_B(result)
tile.plot(B)
