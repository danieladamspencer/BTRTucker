# Make exhibits to compare model performance in analysis of ADNI TBM data
# Bayes Tucker ----
result_dir <- "~/github/BTRTucker/results/ADNI"
result_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "TBM_BTRTucker", value = T)
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
library(MNITemplate)
mni_template <- readMNI()
tile.plot(mni_template@.Data[,,91])
library(oro.nifti)
mdt_template <- readNIfTI("~/Downloads/ADNI_MDT/ADNI_ICBM9P_mni_4step_MDT.nii.gz")
library(tidyverse)
template_grob <-
  # mni_template@.Data[,,91] |>
  mdt_template@.Data[,,110] |>
  reshape2::melt() |>
  ggplot() +
  geom_raster(aes(x = Var1, y = Var2, fill = value)) +
  scale_fill_distiller(palette = "Greys") +
  guides(fill = "none") +
  theme_void() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))
template_grob <- ggplotGrob(template_grob)

reshape2::melt(B) |>
  # mutate(value = ifelse(value == 0, NA, value)) |>
  ggplot() +
  annotation_custom(grob = template_grob) +
  geom_raster(aes(x = Var1, y = Var2, fill = value, alpha = value / max(abs(value),na.rm = T))) +
  scale_fill_gradient2("") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  guides(fill = "none", alpha = "none") +
  theme_void()


mdt_out <- mdt_template
mdt_out@.Data <- array(0,dim = dim(mdt_out@.Data))
mdt_out@.Data[,,110] <- B
writeNIfTI(mdt_out, filename = "~/Downloads/ADNI_MDT/BTRT_final_B_out")

# > Bayes CP ----

# > FTR Tucker ----
result_dir <- "~/github/BTRTucker/results/ADNI"
result_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "TBM_FTRTucker", value = T)
rankGrid <- as.matrix(expand.grid(1:4,1:4))
library(bayestensorreg)
llikVals <- apply(rankGrid,1, function(r) {
  rFile <- grep(paste0("rank",paste(r,collapse = ""),".rds"),result_files, value = T)
  res <- readRDS(rFile)
  llik_out <- res$llik
  return(llik_out)
})
rankGrid[which.min(llikVals),]
ftr_out <- readRDS(file.path(result_dir,"4_ADNI_TBM_FTRTucker_rank24.rds"))

reshape2::melt(ftr_out$B) |>
  # mutate(value = ifelse(value == 0, NA, value)) |>
  ggplot() +
  annotation_custom(grob = template_grob) +
  geom_raster(aes(x = Var1, y = Var2, fill = value, alpha = value / max(abs(value),na.rm = T))) +
  scale_fill_gradient2("") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  guides(fill = "none", alpha = "none") +
  theme_void()
