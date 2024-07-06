# Process the ADNI TBM data
# This is a script to read in the ADNI TBM maps data
# The TBM maps are considered here because they come preprocessed, and
# may make the analysis more reproducible
# Read in the data ----
data_dir <- "~/github/BTRTucker/data/ADNI/ADNI 11"
subjects <- list.files(data_dir) |> grep(pattern = "_S_", value = T)
library(oro.nifti)
library(fslr)

subject_imgs <- sapply(subjects, function(subject) {
  subject_file <- list.files(file.path(data_dir, subject), full.names = T, recursive = T) |>
    grep(pattern = ".nii", value = T)
  print(subject_file)
  subject_img <- try(readNIfTI(subject_file))
  while(class(subject_img) == "try-error") subject_img <- try(readNIfTI(subject_file))
  # use this to get a slice of full-res data
  return(subject_img@.Data[,,80])
}, simplify = "array")
saveRDS(subject_imgs, file = file.path(data_dir,"4_ADNI_TBM_subject_imgs_slice080.rds"))
subject_imgs <- readRDS(file.path(data_dir, "4_ADNI_TBM_subject_imgs_slice080.rds"))

library(ADNIMERGE)
# ADNI EDA
adni_df <- subset(adnimerge, PTID %in% subjects & !is.na(MMSE))
adni_df <- adni_df[,!grepl(pattern = ".bl",names(adni_df))]
adni_df <- subset(adni_df, VISCODE == "bl")
missing_cols <- apply(adni_df, 2, function(x) sum(is.na(x)))
adni_df <- adni_df[,missing_cols == 0]
adni_df <-
  subset(
    adni_df,
    select = -c(
      RID, # Identifier
      COLPROT, # Collection procedure
      ORIGPROT, # Collection procedure
      PTID, # Identifier
      SITE, # Identifier
      VISCODE, # Visit code
      EXAMDATE, # Date
      FLDSTRENG, # Collection procedure
      FSVERSION, # Collection procedure
      IMAGEUID, # Identifier
      ICV, # Collection procedure
      DX, # Diagnosis, not a predictor
      mPACCdigit, # Another clinical score
      mPACCtrailsB, # Another clinical score
      Month, # Date
      M, # Date
      CDRSB, # Another clinical score
      ADASQ4,# Another clinical score
      LDELTOTAL,# Another clinical score
      PTRACCAT, # racial category
      PTETHCAT # ethnic category
    )
  )
adni_lm <- lm(MMSE ~ ., data = adni_df)
aic_select <- step(adni_lm)
final_adni_df <- subset(adni_df, select = c(MMSE, AGE, PTEDUCAT, APOE4))
final_adni_lm <- lm(MMSE ~ . - AGE, final_adni_df)
summary(final_adni_lm) # Based on this, only include PTEDUCAT and APOE4

adni_df <- subset(adnimerge, PTID %in% subjects & !is.na(MMSE), select = c(PTID,VISCODE,AGE,PTEDUCAT, PTGENDER,APOE4,MMSE))
adni_bl <- subset(adni_df, VISCODE == "bl")
adni_bl <- adni_bl[order(adni_bl$PTID),] # This is important. Make sure the order is the same
adni_bl$PTGENDER <- ifelse(adni_bl$PTGENDER == "Male",0,1)

library(bayestensorreg)
tbm_data <- as.TR_data(adni_bl$MMSE,subject_imgs,eta = cbind(1,as.matrix(adni_bl[,3:6])))
saveRDS(tbm_data, file.path(data_dir,"04_ADNI_TBM_slice080_TRdata.rds"))

## Mask the ADNI data ----
data_dir <- "~/github/BTRTucker/data/"
aal_mdt <- readNIfTI(file.path(data_dir, "aalMDT.nii.gz"))
insideAAL <- aal_mdt@.Data[,,80]
insideAAL[insideAAL > 0] <- 1
tbm_data <- readRDS(list.files(data_dir, pattern = "slice080_TRdata", full.names = TRUE))
n <- length(tbm_data$y)
new_X <- tbm_data$X * (insideAAL %o% rep(1,n))
new_X2 <- array(as.integer(new_X), dim = dim(new_X)) # Since the values are integers anyway, this saves space
masked_tbm <- as.TR_data(y = tbm_data$y, X = new_X2, eta = tbm_data$eta)
saveRDS(masked_tbm, file = file.path(data_dir,"04_ADNI_TBM_slice080_masked_TRdata,rds"))
