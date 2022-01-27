# This is a script for performing the analysis on ADNI data
data_dir <- "/Volumes/Macintosh HD/Users/danspen/github/BTRTucker/data/ADNI/ADNI 11"
# subject_labels <- list.files(data_dir)
# saveRDS(subject_labels, file = file.path(data_dir,"subject_labels.rds"))
subject_labels <- readRDS(file.path(data_dir,"subject_labels.rds"))
image_files <- list.files(data_dir, full.names = T, recursive = T) |>
  grep(pattern = ".nii",value = T)

library(ADNIMERGE)
library(oro.nifti)
demog_df <- subset(adnimerge, VISCODE == "bl") |> subset(PTID %in% subject_labels)

# imgs <- sapply(subject_labels, function(i) {
#   img_file <- grep(i, image_files, value = T)
#   out <- readNIfTI(img_file)@.Data[,,110]
#   return(out)
# }, simplify = "array")
# saveRDS(imgs, file = file.path(data_dir, "subject_imgs.rds"))
imgs <- readRDS(file.path(data_dir,"subject_imgs.rds"))
imgs <- imgs - 1000

library(bayestensorreg)

input_df <- subset(demog_df, select = c("PTID","APOE4","PTEDUCAT","MMSE"))
input_df <- input_df[order(input_df$PTID),]

input <- list(y = input_df$MMSE,
              X = imgs,
              eta = cbind(input_df$APOE4,input_df$PTEDUCAT))
class(input) <- "TR_data"

test <- BTRTucker(input = input,ranks = c(1,1), n_iter = 120,n_burn = 20,hyperparameters = NULL,save_dir = NULL)

B_est <- BTRT_final_B(test)
tile.plot(B_est)

# Classical GLM comparison ----
classical_coeffs <- apply(input$X,1:2,function(x) {
  lm_obj <- lm(input$y~-1 + x + input$eta)
  coeffs <- lm_obj$coefficients
  return(coeffs)
})
classical_B <- classical_coeffs[1,,]
tile.plot(classical_B)

# FTR comparisons ----
# >> CP ----
ftr_cp_test <- FTR_CP(input$y, input$X, input$eta, rank = 1)

# >> Tucker ----
ftr_tucker_test <- FTRTucker(input$y, input$X, input$eta, ranks = c(1,1))
