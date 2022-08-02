# This is a script to create figures for the BTRT project
# Plot TBM slices of data ----
data_dir <- "~/github/BTRTucker/data/ADNI/ADNI 11"
plot_dir <- "~/github/BTRTucker/plots"
tbm_data <- readRDS(file.path(data_dir,"4_ADNI_TBM_slice110_TRdata.rds"))
library(tidyverse)
library(oro.nifti)
library(fslr)

mdt_template <- readNIfTI("~/Downloads/ADNI_MDT/ADNI_ICBM9P_mni_4step_MDT.nii.gz")
mdt_template <- fslbet("~/Downloads/ADNI_MDT/ADNI_ICBM9P_mni_4step_MDT.nii.gz")
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

for(i in 1:3){
  X_plot <- reshape2::melt(tbm_data$X[,,i]) |>
    # mutate(value = ifelse(value == 0, NA, value)) |>
    ggplot() +
    annotation_custom(grob = template_grob) +
    geom_raster(aes(x = Var1, y = Var2, fill = value, alpha = value / max(abs(value),na.rm = T))) +
    scale_fill_gradient2("") +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    guides(fill = "none", alpha = "none") +
    theme_void()
  ggsave(
    filename = file.path(plot_dir, paste0(
      "5_subject", sprintf("%02g", i), "_slice110data.png"
    )),
    plot = X_plot,
    width = 4,
    height = 4
  )
}

# Simulated data coefficient ----
library(bayestensorreg)
# Set seed for reproducibility
set.seed(95064)
# >> Generate data according to the specifications in the paper ----
sim_data <-
  TR_simulated_data(
    subjects = 1000,
    tensor_dims = c(50, 50),
    CNR = 1,
    num_active = 3,
    other_covar = c(25, 3, 0.1)
  )

library(tidyverse)
reshape2::melt(sim_data$true_B) |>
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  scale_fill_gradient2(high = "blue",low = "red",mid = "white")

temp <- readRDS("~/github/BTRTucker/results/1_simulated_data_btrtucker_results_rank44.rds")
plot(temp$llik, type = 'l')
final_B <- BTRT_final_B(temp)

result_files <- list.files("~/github/BTRTucker/results/", full.names = T) |>
  grep(pattern = "1_simulated_data_btrtucker_results_rank", value = T)

all_dic <- sapply(result_files, function(fn){
  res <- readRDS(fn)
  DIC_out <- DIC(log_likelihood = res$llik, burn_in = 100)
  return(DIC_out)
})
which.min(all_dic)

reshape2::melt(final_B) |>
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  scale_fill_gradient2(high = "blue",low = "red",mid = "white")
