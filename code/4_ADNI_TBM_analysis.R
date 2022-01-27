# This is a script to analyze the ADNI TBM maps data
# The TBM maps are considered here because they come preprocessed, and
# may make the analysis more reproducible

# Read in the data ----
data_dir <- "~/github/BTRTucker/data/ADNI/ADNI 11"
subjects <- list.files(data_dir) |> grep(pattern = "_S_", value = T)
library(oro.nifti)
library(fslr)

icbm_template <- readNIfTI("~/Downloads/ICBM_Template.nii.gz")
icbm_labels <- readNIfTI("~/Downloads/ICBM_labels.nii.gz")

source("https://neuroconductor.org/neurocLite.R")
neuro_install("MNITemplate")
library(MNITemplate)

mni_template <- readMNI()


job::job({
  subject_imgs <- sapply(subjects, function(subject) {
    subject_file <- list.files(file.path(data_dir, subject), full.names = T, recursive = T) |>
      grep(pattern = ".nii", value = T)
    print(subject_file)
    subject_img <- try(readNIfTI(subject_file))
    while(class(subject_img) == "try-error") subject_img <- try(readNIfTI(subject_file))
    resamp_img <- try(fsl_resample(subject_img,voxel_size = 2))
    while(class(resamp_img) == "try-error") resamp_img <- try(fsl_resample(subject_file,voxel_size = 2))
    return(resamp_img@.Data)
  }, simplify = "array")
}, import = c(subjects, data_dir), packages = c("oro.nifti","fslr"))
saveRDS(subject_imgs, file = file.path(data_dir,"4_ADNI_TBM_subject_imgs_2mm.rds"))
subject_imgs <- readRDS(file.path(data_dir, "4_ADNI_TBM_subject_imgs.rds"))

library(ADNIMERGE)
adni_df <- subset(adnimerge, PTID %in% subjects & !is.na(MMSE), select = c(PTID,VISCODE,PTEDUCAT,APOE4,MMSE))
adni_bl <- subset(adni_df, VISCODE == "bl")
adni_last <- subset(adni_df, VISCODE != "bl") |>
  transform(VISCODEn = as.numeric(sub(pattern = "m",replacement = "",VISCODE)))

library(tidyverse)
adni_last <-
  adni_df %>%
  mutate(VISCODEn = ifelse(VISCODE == "bl",0,as.numeric(sub("m","",VISCODE)))) %>%
  group_by(PTID) %>%
  filter(VISCODEn == max(VISCODEn) | VISCODEn == 0) %>%
  mutate(start_end = ifelse(VISCODEn == 0,"start","end"))

adni_smc <- subset(adnimerge, DX.bl == "SMC" & VISCODE == "bl", select = c())

