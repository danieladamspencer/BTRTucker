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
sim_coef_plot <- reshape2::melt(sim_data$true_B) |>
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  scale_fill_gradient2("",high = "blue",low = "red",mid = "white", limits = c(-1,1)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  guides(fill = "none", alpha = "none") +
  theme_void()

ggsave(filename = "~/github/BTRTucker/plots/5_sim_coefficient.png",plot = sim_coef_plot, width = 5, height = 5)

# Simulated data tensor coefficient ----
result_dir <- "~/github/BTRTucker/results/data_simulations"
# >> BTR Tucker ----
library(bayestensorreg)
btrt_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "btrtucker", value = T)

job::job({
  library(parallel)
  cl <- makeCluster(8)
  btrt_B <- parLapply(cl,btrt_files, function(fn){
    res <- readRDS(fn)
    out_B <- BTRT_final_B(res)
    return(out_B)
  })
  stopCluster(cl)
  saveRDS(btrt_B, file = "~/github/BTRTucker/results/data_simulations/5_btrt_B.rds")
}, import = c(btrt_files))

btrt_B <- readRDS("~/github/BTRTucker/results/data_simulations/5_btrt_B.rds")
library(tidyverse)
btrt_df <-
  reshape2::melt(btrt_B) |>
  mutate(R1 = substring(L1,97,97),
         R2 = substring(L1,98,98),
         matchRank = R1 == R2) |>
  select(-L1)
btrt_plt <- ggplot(btrt_df) +
  geom_tile(aes(x = Var1, y = Var2, fill = value))+
  scale_fill_gradient2("",high = "blue",low = "red",mid = "white", limits = c(-1,1)) +
  # guides(fill = "none", alpha = "none") +
  facet_grid(R1 ~ R2, scales = "free",
             labeller = label_bquote(rows = R1 == .(R1), cols = R2 == .(R2))) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 18),
        aspect.ratio = 1)
btrt_plt
ggsave(filename = "~/github/BTRTucker/plots/5_simulated_btrt_B.png", plot = btrt_plt, width = 11, height = 9)

# >> BTR CP ----
btrcp_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "btr_cp", value = T)

# btrcp_B <-sapply(btrcp_files, function(fn){
#   res <- readRDS(fn)
#   out_B <- btr_cp_final_B(res)
#   return(out_B)
# }, simplify = F)
# saveRDS(btrcp_B, file = "~/github/BTRTucker/results/data_simulations/5_btrcp_B.rds")
btrcp_B <- readRDS("~/github/BTRTucker/results/data_simulations/5_btrcp_B.rds")
library(tidyverse)
btrcp_df <-
  reshape2::melt(btrcp_B) |>
  mutate(R = substring(L1,94,94)) |>
  select(-L1)
btrcp_plt <- ggplot(btrcp_df) +
  geom_tile(aes(x = Var1, y = Var2, fill = value))+
  scale_fill_gradient2("",high = "blue",low = "red",mid = "white", limits = c(-1,1)) +
  # guides(fill = "none", alpha = "none") +
  facet_grid(~ R, scales = "free", labeller = label_bquote(cols = R == .(R))) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 18),
        aspect.ratio = 1)
btrcp_plt
ggsave(filename = "~/github/BTRTucker/plots/5_simulated_btrcp_B.png", plot = btrcp_plt, width = 16, height = 3)

# >> FTR Tucker ----
ftrt_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "ftrtucker", value = T) |>
  grep(pattern = "noLASSO", value = T, invert = T)

ftrt_B <-sapply(ftrt_files, function(fn){
  res <- readRDS(fn)
  return(res$B)
}, simplify = F)
library(tidyverse)
ftrt_df <-
  reshape2::melt(ftrt_B) |>
  mutate(R1 = substring(L1,97,97), # for with LASSO
         # R1 = substring(L1,105,105), # for noLASSO
         R2 = substring(L1,98,98), # for with LASSO
         # R2 = substring(L1,106,106), #for noLASSO
         matchRank = R1 == R2) |>
  select(-L1)
ftrt_plt <- ggplot(ftrt_df) +
  geom_tile(aes(x = Var1, y = Var2, fill = value))+
  scale_fill_gradient2("",high = "blue",low = "red",mid = "white", limits = c(-1,1)) +
  # guides(fill = "none", alpha = "none") +
  facet_grid(R1 ~ R2, scales = "free",
             labeller = label_bquote(rows = R1 == .(R1), cols = R2 == .(R2))) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 18),
        aspect.ratio = 1)
ftrt_plt
ggsave(filename = "~/github/BTRTucker/plots/5_simulated_ftrt_B.png", plot = ftrt_plt, width = 16, height = 9)

# >> FTR CP ----
ftrcp_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "ftr_cp", value = T)

ftrcp_B <-sapply(ftrcp_files, function(fn){
  res <- readRDS(fn)
  return(res$B)
}, simplify = F)
library(tidyverse)
ftrcp_df <-
  reshape2::melt(ftrcp_B) |>
  mutate(R = substring(L1,94,94)) |>
  select(-L1)
ftrcp_plt <- ggplot(ftrcp_df) +
  geom_tile(aes(x = Var1, y = Var2, fill = value))+
  scale_fill_gradient2("",high = "blue",low = "red",mid = "white", limits = c(-1,1)) +
  # guides(fill = "none", alpha = "none") +
  facet_grid(~ R, scales = "free", labeller = label_bquote(cols = R == .(R))) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 18),
        aspect.ratio = 1)
ftrcp_plt
ggsave(filename = "~/github/BTRTucker/plots/5_simulated_ftrcp_B.png", plot = ftrcp_plt, width = 16, height = 3)

# Simulated data RMSE table ----
library(tidyverse)
result_dir <- "~/github/BTRTucker/results/data_simulations"
sim_data <- readRDS(file.path(result_dir,"1_simulated_data.rds"))
true_B_df <- reshape2::melt(sim_data$true_B, value.name = "True")

btrt_B <- readRDS("~/github/BTRTucker/results/data_simulations/5_btrt_B.rds")

btrt_df <-
  reshape2::melt(btrt_B) |>
  mutate(R1 = substring(L1,97,97),
         R2 = substring(L1,98,98),
         matchRank = R1 == R2) |>
  select(-L1)

btrcp_B <- readRDS("~/github/BTRTucker/results/data_simulations/5_btrcp_B.rds")
btrcp_df <-
  reshape2::melt(btrcp_B) |>
  mutate(R = substring(L1,94,94)) |>
  select(-L1)

ftrt_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "ftrtucker", value = T) |>
  grep(pattern = "noLASSO", value = T, invert = T)

ftrt_B <-sapply(ftrt_files, function(fn){
  res <- readRDS(fn)
  return(res$B)
}, simplify = F)
ftrt_df <-
  reshape2::melt(ftrt_B) |>
  mutate(R1 = substring(L1,97,97), # for with LASSO
         # R1 = substring(L1,105,105), # for noLASSO
         R2 = substring(L1,98,98), # for with LASSO
         # R2 = substring(L1,106,106), #for noLASSO
         matchRank = R1 == R2) |>
  select(-L1)

ftrcp_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "ftr_cp", value = T)

ftrcp_B <-sapply(ftrcp_files, function(fn){
  res <- readRDS(fn)
  return(res$B)
}, simplify = F)
ftrcp_df <-
  reshape2::melt(ftrcp_B) |>
  mutate(R = substring(L1,94,94)) |>
  select(-L1)

glm_B <- readRDS(file.path(result_dir,"1_glm_B.rds"))
glm_B_rmse <- sqrt(mean((glm_B - sim_data$true_B)^2))

tucker_rmse <- full_join(
  mutate(ftrt_df, Model = "FTR Tucker"),
  mutate(btrt_df, Model = "BTR Tucker")
) |>
  left_join(true_B_df) |>
  group_by(Model,R1,R2) |>
  summarize(RMSE = sqrt(mean((value - True)^2)))

ftrt_rmse <-
  tucker_rmse |>
  filter(Model == "FTR Tucker") |>
  pivot_wider(names_from = R2, values_from = RMSE) |>
  ungroup() |>
  select(-Model)

btrt_rmse <-
  tucker_rmse |>
  filter(Model == "BTR Tucker") |>
  pivot_wider(names_from = R2, values_from = RMSE) |>
  ungroup() |>
  select(-Model)

cp_rmse <- full_join(
  mutate(ftrcp_df, Model = "FTR CP"),
  mutate(btrcp_df, Model = "BTR CP")
) |>
  left_join(true_B_df) |>
  group_by(Model,R) |>
  summarize(RMSE = sqrt(mean((value - True)^2)))

ftrcp_rmse <-
  cp_rmse |>
  filter(Model == "FTR CP") |>
  pivot_wider(names_from = R, values_from = RMSE) |>
  ungroup() |>
  select(-Model)

btrcp_rmse <-
  cp_rmse |>
  filter(Model == "BTR CP") |>
  pivot_wider(names_from = R, values_from = RMSE) |>
  ungroup() |>
  select(-Model)

library(knitr)
kable(ftrt_rmse, digits = 4, "latex")
kable(ftrcp_rmse, digits = 4, "latex")
kable(btrt_rmse, digits = 4, "latex")
kable(btrcp_rmse, digits = 4, "latex")
# Simulated data best coefficients ----
result_dir <- "~/github/BTRTucker/results/data_simulations"
library(bayestensorreg)
# > BTRT ----
btrt_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "btrtucker", value = T)
btrt_dic <- sapply(btrt_files, function(fn){
  library(bayestensorreg)
  res <- readRDS(fn)
  out <- DIC(res$llik, burn_in = 1000)
  return(out)
})
which.min(btrt_dic)
btrt_llik <- sapply(btrt_files, function(fn){
  res <- readRDS(fn)
  return(res$llik)
})
reshape2::melt(btrt_llik) |>
  mutate(Var2 = factor(as.numeric(Var2))) |>
  ggplot() +
  geom_line(aes(x = Var1, y = value, color = Var2))

# Ranks 4,4
# > BTR CP ----
btrcp_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "btr_cp", value = T)
btrcp_dic <- sapply(btrcp_files, function(fn){
  library(bayestensorreg)
  res <- readRDS(fn)
  out <- DIC(res$llik, burn_in = 1000)
  return(out)
})
which.min(btrcp_dic)
# Rank 1
# This is weird, so I'm going to look at the log-likelihood values
btrcp_llik <- sapply(btrcp_files, function(fn){
  res <- readRDS(fn)
  return(res$llik)
})
apply(btrcp_llik,2,sd)
reshape2::melt(btrcp_llik) |>
  mutate(Var2 = factor(as.numeric(Var2))) |>
  ggplot() +
  geom_line(aes(x = Var1, y = value, color = Var2))
# > FTR Tucker ----
ftrt_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "ftrtucker", value = T) |>
  grep(pattern = "noLASSO", invert = T, value = T)
ftrt_aic <- sapply(ftrt_files, function(fn){
  res <- readRDS(fn)
  num_params <- length(res$gam) + length(unlist(res$betas)) + length(c(res$G))
  aic_out <- 2 * num_params - 2 * res$llik
  return(aic_out)
})
ftrt_bic <- sapply(ftrt_files, function(fn){
  sample_size <- 1000
  res <- readRDS(fn)
  num_params <- length(res$gam) + length(unlist(res$betas)) + length(c(res$G))
  bic_out <-  log(sample_size) * num_params - 2 * res$llik
  return(bic_out)
})
which.min(ftrt_aic)
which.min(ftrt_bic)
# Ranks 1,1 # Ranks 2,2 now with better stopping rules # Ranks 4,4 after bugfix
# > FTR CP ----
ftrcp_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "ftr_cp", value = T)
ftrcp_aic <- sapply(ftrcp_files, function(fn){
  res <- readRDS(fn)
  num_params <- length(res$gam) + length(unlist(res$betas))
  aic_out <- 2 * num_params - 2 * res$llik
  return(aic_out)
})
which.min(ftrcp_aic)
# Rank 3
# > GLM ----
glm_B <- readRDS(file.path(result_dir,"1_glm_B.rds"))
glm_B_df <- reshape2::melt(glm_B)
# > Merge the best models dfs together ----
sim_data <- readRDS(file.path(result_dir,"1_simulated_data.rds"))
best_df <- filter(btrt_df,R1 == 4, R2 == 4) |>
  mutate(Model = "BTR Tucker 4,4") |>
  select(Var1, Var2, value, Model) |>
  full_join(
    filter(btrcp_df,R == 1) |>
      mutate(Model = "BTR CP 1") |>
      select(Var1, Var2, value, Model)
  ) |>
  full_join(
    filter(ftrt_df,R1 == 4, R2 == 4) |>
      mutate(Model = "FTR Tucker 4,4") |>
      select(Var1, Var2, value, Model)
  ) |>
  full_join(
    filter(ftrcp_df,R == 4) |>
      mutate(Model = "FTR CP 4") |>
      select(Var1, Var2, value, Model)
  ) |>
  full_join(
    mutate(glm_B_df, Model = "GLM")
  ) |>
  full_join(
    mutate(reshape2::melt(sim_data$true_B),Model = "Truth")
  ) |>
  mutate(Model = factor(Model, levels = c("GLM","BTR CP 1","FTR Tucker 4,4","FTR CP 4", "BTR Tucker 4,4","Truth")))

# > Plot ----
best_plt <-
  best_df |>
  left_join(
    reshape2::melt(sim_data$true_B, value.name = "Truth")
  ) |>
  group_by(Model) |>
  mutate(RMSE = sqrt(mean((value - Truth)^2)),
         RMSE = as.character(round(RMSE,4)),
         RMSE = paste0("(",RMSE,")"),
         RMSE = ifelse(RMSE == "(0)","",RMSE),
         # Model = paste0(Model," (",RMSE,")"),
         value = ifelse(value < -1, -1, value),
         value = ifelse(value > 1, 1, value)) |>
  ggplot() +
  # geom_text(aes(x = 20,y = -5, label = RMSE)) |>
  geom_tile(aes(x = Var1, y = Var2, fill = value))+
  scale_fill_gradient2("",high = "blue",low = "red",mid = "white", limits = c(-1,1)) +
  # guides(fill = "none", alpha = "none") +
  facet_grid(~ Model + RMSE, scales = "free") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        text = element_text(size = 18),
        aspect.ratio = 1)
best_plt

ggsave(filename = "~/github/BTRTucker/plots/5_simulated_best_B.png",
       plot = best_plt, width = 16, height = 4)

# Plot the estimates and densities of gamma ----
sim_data <- readRDS("~/github/BTRTucker/results/data_simulations/1_simulated_data.rds")
best_files <- c(btrt_files[16],btrcp_files[1],ftrt_files[16],ftrcp_files[4])
btrt_gam <- readRDS(best_files[1])$gam
btrcp_gam <- readRDS(best_files[2])$gam
ftrt_gam <- readRDS(best_files[3])$gam
ftrcp_gam <- readRDS(best_files[4])$gam
glm_gam <- lm(sim_data$y ~ -1 + sim_data$eta)$coefficients

ftr_df <-
  reshape2::melt(t(unname(ftrt_gam))) |>
  mutate(Model = "FTR Tucker 4,4") |>
  full_join(
    reshape2::melt(t(unname(ftrcp_gam))) |>
      mutate(Model = "FTR CP 4")
  ) |>
  full_join(
    reshape2::melt(t(unname(glm_gam))) |>
      mutate(Model = "GLM")
  )

true_df <- data.frame(
  Var1 = 1,
  Var2 = 1:3,
  value = c(25,3,0.1)
)

library(RColorBrewer)
my_pal <- c(brewer.pal(6,"Paired"),"grey50")

best_gam_plt <- reshape2::melt(btrt_gam) |>
  mutate(Model = "BTR Tucker 4,4") |>
  full_join(
    reshape2::melt(btrcp_gam) |>
      mutate(Model = "BTR CP 1")
  ) |>
  ggplot() +
  stat_density(aes(x = value, fill = Model, color = Model), alpha = 0.3,position = "identity") +
  geom_point(aes(x = value, y=0, color = Model, fill = Model), shape = 19, size = 4, data = ftr_df) +
  scale_fill_manual(values = my_pal[-(3:4)]) +
  scale_color_manual(values = my_pal[-(3:4)]) +
  geom_vline(aes(xintercept = value), data = true_df) +
  facet_grid(~Var2, scales = "free",labeller = label_bquote(cols = gamma[.(Var2)])) +
  labs(y = "Posterior Density") +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        # axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        text = element_text(size=18))
best_gam_plt

ggsave(filename = "~/github/BTRTucker/plots/5_simulated_best_gam.png",plot = best_gam_plt,width = 16, height =3)

# Simulated data log-likelihood ----
result_dir <- "~/github/BTRTucker/results/data_simulations"
btrt_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "1_simulated_data_btrtucker_results_rank", value = T)
btrcp_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "1_simulated_data_btr_cp_results_rank", value = T)
btrt_llik <- sapply(btrt_files, function(fn){
  return(readRDS(fn)$llik)
})
btrcp_llik <- sapply(btrcp_files, function(fn){
  return(readRDS(fn)$llik)
})

llik_df <-
  reshape2::melt(btrt_llik) |>
  mutate(R1 = substring(Var2,97,97),
         R2 = substring(Var2,98,98),
         Rank  = paste0(R1,",",R2),
         Model = "BTR Tucker") |>
  select(Var1, Rank, Model, value) |>
  full_join(
    reshape2::melt(btrcp_llik) |>
      mutate(Rank = substring(Var2,94,94),
             Model = "BTR CP") |>
      select(Var1, Rank, Model,value)
  )

llik_df |>
  group_by(Model, Rank) |>
  summarize(var(value))

llik_ends_df <- filter(llik_df, Var1 == max(Var1))

library(ggrepel)
llik_plt <-
  ggplot(llik_df) +
  geom_line(aes(x = Var1, y = value, color = Rank)) +
  geom_text_repel(aes(x = Var1, y = value, label = Rank),
                  nudge_x = 1000, direction = "x", nudge_y = 100,
                  data = llik_ends_df) +
  facet_grid(Model~.) +
  labs(y = "log-likelihood",x="MCMC Iteration") +
  guides(color = "none") +
  theme_bw() +
  theme(text = element_text(size = 18))

plot_dir <- "~/github/BTRTucker/plots"
ggsave(filename = file.path(plot_dir,"5_sim_llik.png"), plot = llik_plt,
       width = 8, height = 6)

# Plot the TBM B ----
result_dir <- "~/github/BTRTucker/results/ADNI/"
# > BTR Tucker ----
btrt_tbm_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "BTRTucker_slice080_rank", value = T)

# job::job({
#   library(parallel)
#   cl <- makeCluster(8)
#   btrt_tbm_B <- parLapply(cl,btrt_tbm_files, function(fn){
#     library(bayestensorreg)
#     res <- readRDS(fn)
#     out_B <- BTRT_final_B(res)
#   return(out_B)})
#   saveRDS(btrt_tbm_B, file = "~/github/BTRTucker/results/ADNI/5_btrt_slice080_tbm_B.rds")
# }, import = c(btrt_tbm_files))
btrt_tbm_B <- readRDS("~/github/BTRTucker/results/ADNI/5_btrt_slice080_tbm_B.rds")
names(btrt_tbm_B) <- btrt_tbm_files

library(tidyverse)
library(oro.nifti)
mdt_template <- readNIfTI("~/github/BTRTucker/data/ADNI/ADNI 11/ADNI_MDT/ADNI_ICBM9P_mni_4step_MDT.nii.gz")
library(fslr)
mdt_template <- fslbet(mdt_template)
template_grob <-
  # mni_template@.Data[,,91] |>
  mdt_template@.Data[,,80] |>
  reshape2::melt() |>
  ggplot() +
  geom_raster(aes(x = Var1, y = Var2, fill = value)) +
  scale_fill_distiller(palette = "Greys") +
  guides(fill = "none") +
  theme_void() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))
template_grob <- ggplotGrob(template_grob)
btrt_tbm_df <-
  reshape2::melt(btrt_tbm_B) |>
  mutate(R1 = substring(L1,81,81),
         R2 = substring(L1,82,82),
         value = value / (1e-5),
         value = ifelse(value > 1,1,value),
         value = value * (1e-5),
         matchRank = R1 == R2) |>
  select(-L1)
btrt_tbm_plt <- ggplot(btrt_tbm_df) +
  annotation_custom(grob = template_grob) +
  geom_raster(aes(x = Var1, y = Var2, fill = value,alpha = value))+
  scale_fill_gradient2("",high = "blue",low = "red",mid = "black",
                       limits = c(-1e-5,1e-5)
                       ) +
  guides(alpha = "none") +
  facet_grid(R1 ~ R2, scales = "free",
             labeller = label_bquote(rows = R1 == .(R1), cols = R2 == .(R2))) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 18),
        aspect.ratio = 1)
btrt_tbm_plt
ggsave(filename = "~/github/BTRTucker/plots/5_btrt_slice080_tbm_B.png", plot = btrt_tbm_plt, width = 11, height = 9)

# > BTR CP ----
btrcp_tbm_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "BTR_CP_rank", value = T)
# job::job({
#   library(parallel)
#   cl <- makeCluster(3)
#   btrcp_tbm_B <- parLapply(cl,btrcp_tbm_files, function(fn){
#     library(bayestensorreg)
#     res <- readRDS(fn)
#     out_B <- btr_cp_final_B(res)
#   return(out_B)})
#   saveRDS(btrcp_tbm_B, file = "~/github/BTRTucker/results/ADNI/5_btrcp_slice080_tbm_B.rds")
# }, import = c(btrcp_tbm_files))
btrcp_tbm_B <- readRDS("~/github/BTRTucker/results/ADNI/5_btrcp_slice080_tbm_B.rds")
names(btrcp_tbm_B) <- btrcp_tbm_files

n_samps <- sapply(btrcp_tbm_files, function(fn){
  res <- readRDS(fn)
  out <- sum(res$llik != 0)
  return(out)
})

library(tidyverse)
library(oro.nifti)
mdt_template <- readNIfTI("~/github/BTRTucker/data/ADNI/ADNI 11/ADNI_MDT/ADNI_ICBM9P_mni_4step_MDT.nii.gz")
library(fslr)
mdt_template <- fslbet(mdt_template)
template_grob <-
  # mni_template@.Data[,,91] |>
  mdt_template@.Data[,,80] |>
  reshape2::melt() |>
  ggplot() +
  geom_raster(aes(x = Var1, y = Var2, fill = value)) +
  scale_fill_distiller(palette = "Greys") +
  guides(fill = "none") +
  theme_void() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))
template_grob <- ggplotGrob(template_grob)
btrcp_tbm_df <-
  reshape2::melt(btrcp_tbm_B) |>
  mutate(Rank = substring(L1,69,69),
         value = value / (1e-5),
         value = ifelse(value > 1,1,value),
         value = value * (1e-5)
         ) |>
  select(-L1) #|>
  # filter(Rank == "1")
btrcp_tbm_plt <- ggplot(btrcp_tbm_df) +
  annotation_custom(grob = template_grob) +
  geom_raster(aes(x = Var1, y = Var2, fill = value,alpha = value))+
  scale_fill_gradient2("",high = "blue",low = "red",mid = "black",
                       limits = c(-1e-5,1e-5)
  ) +
  guides(alpha = "none") +
  facet_grid(~ Rank, scales = "free",
             labeller = label_bquote(cols = Rank == .(Rank))) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 18),
        aspect.ratio = 1)
btrcp_tbm_plt

# > FTR Tucker ----
ftrt_tbm_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "FTRTucker_rank", value = T)

ftrt_tbm_B <- sapply(ftrt_tbm_files, function(fn){
  res <- readRDS(fn)
  if(class(res) == "list"){
    return(res$B)
  }else{return(NULL)}
}, simplify = F)
ftrt_tbm_B <- ftrt_tbm_B[!sapply(ftrt_tbm_B,is.null)]

library(tidyverse)
library(oro.nifti)
mdt_template <- readNIfTI("~/github/BTRTucker/data/ADNI/ADNI 11/ADNI_MDT/ADNI_ICBM9P_mni_4step_MDT.nii.gz")
library(fslr)
mdt_template <- fslbet(mdt_template)
template_grob <-
  # mni_template@.Data[,,91] |>
  mdt_template@.Data[,,80] |>
  reshape2::melt() |>
  ggplot() +
  geom_raster(aes(x = Var1, y = Var2, fill = value)) +
  scale_fill_distiller(palette = "Greys") +
  guides(fill = "none") +
  theme_void() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))
template_grob <- ggplotGrob(template_grob)
ftrt_tbm_df <-
  reshape2::melt(ftrt_tbm_B) |>
  mutate(R1 = substring(L1,72,72),
         R2 = substring(L1,73,73),
         # value = value / (1e-3),
         # value = ifelse(abs(value) > 1,sign(value)*1,value),
         # value = value * (1e-3),
         matchRank = R1 == R2) |>
  select(-L1)
ftrt_lim <- quantile(ftrt_tbm_df$value, probs = c(0.99))
ftrt_tbm_df <-
  mutate(ftrt_tbm_df,
         value = value / ftrt_lim,
         value = ifelse(abs(value) > 1,sign(value)*1,value),
         value = value * ftrt_lim,
         )
ftrt_tbm_plt <- ggplot(ftrt_tbm_df) +
  annotation_custom(grob = template_grob) +
  geom_raster(aes(x = Var1, y = Var2, fill = value,alpha = value))+
  scale_fill_gradient2("",high = "blue",low = "red",mid = "black",na.value = "black",
                       limits = c(-ftrt_lim,ftrt_lim)
  ) +
  guides(alpha = "none") +
  facet_grid(R1 ~ R2, scales = "free",
             labeller = label_bquote(rows = R1 == .(R1), cols = R2 == .(R2))) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 18),
        aspect.ratio = 1)
ftrt_tbm_plt
ggsave(filename = "~/github/BTRTucker/plots/5_ftrt_slice080_tbm_B.png", plot = ftrt_tbm_plt, width = 11, height = 9)

# Best TBM B ----
library(bayestensorreg)
library(tidyverse)
result_dir <- "~/github/BTRTucker/results/ADNI"
# > BTR Tucker ----
btrt_tbm_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "BTRTucker_slice080_rank", value = T)
btrt_tbm_dic <- sapply(btrt_tbm_files, function(fn){
  res <- readRDS(fn)
  dic_out <- DIC(res$llik, burn_in = 500)
  return(dic_out)
})
which.min(btrt_tbm_dic) # Rank 3,2 # Rank 4,2 for slice 080 # Rank 1,2 for slice 080 with age and gender added in
btrt_tbm_best_res <- readRDS(btrt_tbm_files[which.min(btrt_tbm_dic)])
btrt_tbm_B <- readRDS("~/github/BTRTucker/results/ADNI/5_btrt_slice080_tbm_B.rds")
btrt_tbm_best_B <- btrt_tbm_B[[which.min(btrt_tbm_dic)]]

# > BTR CP ----
# all BTR CP models fail due to infinite values in the posterior precision of beta
# btrcp_tbm_best_B <- matrix(0,220,220)
btrcp_tbm_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "BTR_CP_rank", value = T)
library(bayestensorreg)
btrcp_tbm_dic <- sapply(btrcp_tbm_files, function(fn){
  res <- readRDS(fn)
  out_llik <- res$llik[res$llik != 0]
  dic_out <- DIC(out_llik, burn_in = 0)
  return(dic_out)
})
which.min(btrcp_tbm_dic)
btrcp_tbm_best_res <- readRDS(btrcp_tbm_files[which.min(btrcp_tbm_dic)])
btrcp_tbm_B <- readRDS("~/github/BTRTucker/results/ADNI/5_btrcp_slice080_tbm_B.rds")
btrcp_tbm_best_B <- btrcp_tbm_B[[which.min(btrcp_tbm_dic)]]

# > FTR Tucker ----
ftrt_tbm_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "FTRTucker_rank", value = T)
ftrt_tbm_aic <- sapply(ftrt_tbm_files, function(fn){
  res <- readRDS(fn)
  if(class(res) == "try-error"){
    return(Inf)
  }else{
    num_params <- length(res$gam) + length(unlist(res$betas)) + length(c(res$G))
    aic_out <- 2 * num_params - 2 * res$llik
    return(aic_out)
  }
})
which.min(ftrt_tbm_aic) # Rank 1,1 # Rank 4,4 when age and gender are included # Rank 1,1 after bugfix
ftrt_tbm_best_res <- readRDS(ftrt_tbm_files[which.min(ftrt_tbm_aic)])
ftrt_tbm_best_B <- ftrt_tbm_best_res$B

# > FTR CP ----
ftrcp_tbm_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "FTR_CP_rank", value = T)
ftrcp_tbm_aic <- sapply(ftrcp_tbm_files, function(fn){
  res <- readRDS(fn)
  num_params <- length(res$gam) + length(unlist(res$betas))
  aic_out <- 2 * num_params - 2 * res$llik
  return(aic_out)
})
which.min(ftrcp_tbm_aic) # Rank 2 # Rank 1 when age and gender are included
ftrcp_tbm_best_res <- readRDS(ftrcp_tbm_files[which.min(ftrcp_tbm_aic)])
ftrcp_tbm_best_B <- ftrcp_tbm_best_res$B

# > GLM ----
glm_tbm_res <- readRDS(file.path(result_dir,"4_ADNI_TBM_GLM.rds"))
glm_tbm_B <- glm_tbm_res$B * glm_tbm_res$B_signif

# > Plot ----
library(oro.nifti)
mdt_template <- readNIfTI("~/github/BTRTucker/data/ADNI/ADNI 11/ADNI_MDT/ADNI_ICBM9P_mni_4step_MDT.nii.gz")
library(fslr)
mdt_template <- fslbet(mdt_template)
template_grob <-
  # mni_template@.Data[,,91] |>
  mdt_template@.Data[,,80] |>
  reshape2::melt() |>
  ggplot() +
  geom_raster(aes(x = Var1, y = Var2, fill = value)) +
  scale_fill_distiller(palette = "Greys") +
  guides(fill = "none") +
  theme_void() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))
template_grob <- ggplotGrob(template_grob)

data_dir <- "~/github/BTRTucker/data/ADNI/ADNI 11"
# yeo7_mdt <- readNIfTI(file.path(data_dir, "yeo7_MDT_extrantsr.nii.gz"))
# yeo7_template <- yeo7_mdt@.Data[,,110] |>
#   reshape2::melt() |>
#   mutate(value = ifelse(value ==0, NA, value)) |>
#   ggplot() +
#   geom_raster(aes(x = Var1, y = Var2, fill = value)) +
#   scale_fill_distiller(palette = "Greys") +
#   guides(fill = "none") +
#   theme_void() +
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0))
# yeo7_template <- ggplotGrob(yeo7_template)
#
# Yeo7MDT <- readNIfTI(file.path(data_dir,"Yeo7MDT.nii.gz"))
# Yeo7_template <- Yeo7MDT@.Data[,,140] |>
#   reshape2::melt() |>
#   mutate(value = ifelse(value ==0, NA, value)) |>
#   ggplot() +
#   geom_raster(aes(x = Var1, y = Var2, fill = value)) +
#   scale_fill_distiller(palette = "Greys",na.value = "black") +
#   guides(fill = "none") +
#   theme_void() +
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0))
# Yeo7_template <- ggplotGrob(Yeo7_template)

aal_mdt <- readNIfTI(file.path(data_dir, "aalMDT.nii.gz"))
aal_template <- aal_mdt@.Data[,,80] |>
  reshape2::melt() |>
  mutate(value = ifelse(value ==0, NA, value),
         value = ifelse((value == 4201 | value == 4202),1,0) # Highlights the amygdala
         ) |>
  ggplot() +
  geom_raster(aes(x = Var1, y = Var2, fill = value)) +
  scale_fill_distiller(palette = "Greys") +
  guides(fill = "none") +
  theme_void() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))
aal_template <- ggplotGrob(aal_template)

insideAAL <- aal_mdt@.Data[,,80] |>
  reshape2::melt(value.name = "mask") |>
  mutate(mask = ifelse(mask == 0, 0, 1))

# insideYeo7 <- Yeo7MDT@.Data[,,110] |>
#   reshape2::melt(value.name = "mask") |>
#   mutate(mask = ifelse(mask == 0, 0, 1))

insideTemplate <-
  mdt_template@.Data[,,80] |>
  reshape2::melt(value.name = "mask") |>
  mutate(mask = ifelse(mask ==0,0,1))

tbm_best_B <-
  reshape2::melt(btrt_tbm_best_B) |>
  mutate(Model = "BTR Tucker 1,2") |>
  # full_join(
  #   reshape2::melt(btrcp_tbm_best_B) |>
  #     mutate(Model = "BTR CP 1")
  # ) |>
  full_join(
    reshape2::melt(ftrt_tbm_best_B) |>
      mutate(Model = "FTR Tucker 1,1")
  ) |>
  full_join(
    reshape2::melt(ftrcp_tbm_best_B) |>
      mutate(Model = "FTR CP 1")
  ) |>
  full_join(
    reshape2::melt(glm_tbm_B) |>
      mutate(Model = "GLM")
  )

tbm_best_B |>
  group_by(Model) |>
  summarize(min_v = min(value),
            max_v = max(value),
            num_large = sum(abs(value) > 1e-5))

tbm_best_plt <-
  mutate(tbm_best_B,
         value = ifelse(abs(value) > 1e-5,sign(value) * 1e-5,value),
         Model = factor(Model, levels = c("GLM","FTR CP 1","FTR Tucker 1,1",
                                          # "BTR CP 1",
                                          "BTR Tucker 1,2"))
         ) |>  # thresholding
  # filter(Model == "FTR Tucker 1,1") |>
  left_join(insideAAL) |>
  mutate(value = value * mask) |>
  ggplot() +
  annotation_custom(grob = aal_template) +
  geom_raster(aes(x = Var1, y = Var2, fill = value,
                  alpha = abs(value) / 1e-5
                  # alpha = abs(value) / 6e-2
              )) +
  scale_fill_gradient2("",high = "blue",low = "red",
                       mid = "black",
                       limits = c(-1e-5,1e-5),
                       na.value = "black"
                       ) +
  guides(alpha = "none") +
  facet_grid(~ Model, scales = "free") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        text = element_text(size = 18),
        aspect.ratio = 1)
tbm_best_plt

ggsave(filename = "~/github/BTRTucker/plots/5_tbm_best_B_slice080_template_masked.png",
       plot = tbm_best_plt, width = 16, height = 4)

# Best TBM gam ----
# BTR Tucker
btrt_tbm_best_gam <- btrt_tbm_best_res$gam

# BTR CP
btrcp_tbm_best_gam <- btrcp_tbm_best_res$gam

# FTR Tucker
ftrt_tbm_best_gam <- ftrt_tbm_best_res$gam

# FTR CP
ftrcp_tbm_best_gam <- ftrcp_tbm_best_res$gam

# GLM / No image
glm_tbm_gam <- glm_tbm_res$gam

gam_names <- c("Age","Education","Gender","APOE4")

ftr_tbm_best_gam_df <-
  reshape2::melt(t(unname(ftrt_tbm_best_gam))) |>
  mutate(Model = "FTR Tucker 1,1") |>
  full_join(
    reshape2::melt(t(unname(ftrcp_tbm_best_gam))) |>
      mutate(Model = "FTR CP 1")
  ) |>
  full_join(
    reshape2::melt(t(unname(glm_tbm_gam))) |>
      mutate(Model = "GLM")
  ) |>
  mutate(Var2 = gam_names[Var2]) |>
  filter(Var2 != "Intercept")

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
hue_pal <- gg_color_hue(4)
library(RColorBrewer)
# my_pal <- brewer.pal(4,"Paired")
my_pal <- c(brewer.pal(6,"Paired"),"grey50")

# Plot
tbm_best_gam_plt <-
  reshape2::melt(btrt_tbm_best_gam) |>
  mutate(Model = "BTR Tucker 1,2",
         Var2 = gam_names[Var2]) |>
  filter(Var2 != "Intercept") |>
  # full_join(
  #   reshape2::melt(btrcp_tbm_best_gam) |>
  #     mutate(Model = "BTR CP 1",
  #            Var2 = gam_names[Var2]) |>
  #     filter(Var2 != "Intercept")
  # ) |>
  ggplot() +
  stat_density(aes(x = value, fill = Model, color = Model), alpha = 0.3, position = "identity") +
  geom_point(aes(x = value, y=0, color = Model, fill = Model), shape = 19,size = 4, data = ftr_tbm_best_gam_df) +
  scale_fill_manual(values = my_pal[-c(1,3:4)])+
  scale_color_manual(values = my_pal[-c(1,3:4)])+
  # facet_grid(~Var2, scales = "free") +
  facet_wrap(~Var2, scales = "free", nrow = 1) +
  labs(y = "Posterior Density") +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        # axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        text = element_text(size=18))

tbm_best_gam_plt

ggsave(filename = "~/github/BTRTucker/plots/5_tbm_best_gam.png",plot = tbm_best_gam_plt,width = 16, height =3)

# Plot Yeo 7 on the MDT template ----
data_dir <- "~/github/BTRTucker/data/ADNI/ADNI 11"
library(fslr)
library(oro.nifti)
# View the Yeo 7 atlas in MNI space
yeo7_mni <-
  readnii(
    file.path(
      data_dir,
      "Yeo_JNeurophysiol11_MNI152",
      "Yeo2011_7Networks_MNI152_FreeSurferConformed1mm_LiberalMask.nii.gz"
    ),
    dtype = F,
    drop_dim = F
  )

yeo7_mdt <- fslr::flirt(
  infile = file.path(
    data_dir,
    "Yeo_JNeurophysiol11_MNI152",
    "Yeo2011_7Networks_MNI152_FreeSurferConformed1mm_LiberalMask.nii.gz"
  ),
  reffile = file.path(data_dir, "ADNI_MDT", "ADNI_ICBM9P_mni_4step_MDT.nii.gz"),
  outfile = file.path(data_dir, "yeo7_MDT"),
  retimg = F
)

yeo7_mdt <- readNIfTI(file.path(data_dir,"yeo7_MDT.nii.gz"))
orthographic(yeo7_mdt)

# Using Taki's suggested packages and functions
library(extrantsr)
library(fslr)
# Step 2: Register MNI to MDT
MNI2MDT <- extrantsr::registration(filename = file.path(fsldir(),"data","standard","MNI152_T1_1mm.nii.gz"),
                        template.file = file.path(data_dir, "ADNI_MDT", "ADNI_ICBM9P_mni_4step_MDT.nii.gz"),
                        typeofTransform = "SyN",
                        other_interpolator = "nearestNeighbor")
# Step 3: Apply warp to Yeo7 label map using nearest neighbor interpolation
Yeo7ToMDT <- ants_apply_transforms(fixed = file.path(data_dir, "ADNI_MDT", "ADNI_ICBM9P_mni_4step_MDT.nii.gz"),moving = file.path(
  data_dir,
  "Yeo_JNeurophysiol11_MNI152",
  "Yeo2011_7Networks_MNI152_FreeSurferConformed1mm_LiberalMask.nii.gz"
),transformlist = MNI2MDT$fwdtransforms, interpolator = "nearestNeighbor")
# Step 4: Visualize Yeo7 atlas in MDT space
library(fslr)
orthographic(Yeo7ToMDT)
writeNIfTI(Yeo7ToMDT,file.path(data_dir,"Yeo7MDT.nii.gz"))


yeo7_mdt <- extrantsr::registration(
  filename = file.path(
    data_dir,
    "Yeo_JNeurophysiol11_MNI152",
    "Yeo2011_7Networks_MNI152_FreeSurferConformed1mm_LiberalMask.nii.gz"
  ),
  outfile = file.path(data_dir, "yeo7_MDT_extrantsr.nii.gz"),
  template.file = file.path(data_dir, "ADNI_MDT", "ADNI_ICBM9P_mni_4step_MDT.nii.gz"),
  typeofTransform = "SyN",
  other_interpolator = "genericLabel"
)
yeo7_mdt <- readNIfTI(file.path(data_dir, "yeo7_MDT_extrantsr.nii.gz"))
library(fslr)
orthographic(yeo7_mdt)

library(tidyverse)
yeo7_template <- yeo7_mdt@.Data[,,110] |>
  reshape2::melt() |>
  mutate(value = ifelse(value ==0, NA, value)) |>
  ggplot() +
  geom_raster(aes(x = Var1, y = Var2, fill = value)) +
  scale_fill_distiller(palette = "Greys") +
  guides(fill = "none") +
  theme_void() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))
yeo7_template <- ggplotGrob(yeo7_template)

# TBM llik plots ----
result_dir <- "~/github/BTRTucker/results/ADNI"
btrt_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "BTRTucker_rank", value = T)
btrt_llik <- sapply(btrt_files, function(fn){
  res <- readRDS(fn)
  return(as.matrix(res$llik))
}, simplify = F)
tbm_llik_df <- reshape2::melt(btrt_llik) |>
  mutate(R1 = substring(L1, 71, 71),
         R2 = substring(L1, 72, 72)) |>
  select(Var1, value, R1, R2)

tbm_llik_plt <-
  ggplot(tbm_llik_df) +
  geom_line(aes(x = Var1, y = value)) +
  facet_grid(R1 ~ R2, labeller = label_bquote(rows = R[1] == .(R1), cols = R[2] == .(R2))) +
  labs(x = "MCMC Iteration", y = "log-likelihood") +
  theme_bw()

tbm_llik_plt
plot_dir <- "~/github/BTRTucker/plots"
ggsave(filename = file.path(plot_dir,"5_tbm_btrt_llik.png"), plot = tbm_llik_plt, width = 16, height = 9)

# TBM trace plots ----
library(bayestensorreg)
best_btrt_file <- grep(pattern = "rank32", btrt_files, value = T)
btrt_best_res <- readRDS(best_btrt_file)
best_btrt_all_B <- BTRT_all_B(btrt_best_res)
which(best_btrt_all_B == min(best_btrt_all_B), arr.ind = T)
# dim1 dim2 dim3
# [1,]  168  218  472
which(best_btrt_all_B == max(best_btrt_all_B), arr.ind = T)
# dim1 dim2 dim3
# [1,]  168  156  472
which((best_btrt_all_B - median(best_btrt_all_B))^2 == min((best_btrt_all_B - median(best_btrt_all_B))^2), arr.ind = T)
# dim1 dim2 dim3
# [1,]   60   74  212
# [2,]  136  189  265
# So I will select the trace plots for these four locations
# I'll try a different tactic
best_tbm_btrt_B <- btrt_tbm_B[[10]]
which(best_tbm_btrt_B == max(best_tbm_btrt_B), arr.ind = T)
# row col
# [1,] 160  85
which(best_tbm_btrt_B == quantile(best_tbm_btrt_B, probs = c(0.996)), arr.ind = T)
small3_trace_best_btrt_tbm_B <-
  reshape2::melt(best_btrt_all_B) |>
  filter((Var1 == 160 & Var2 == 85) | (Var1 == 60 & Var2 == 74) | (Var1 == 136 & Var2 == 189))
small3_trace_plt <-
  mutate(small3_trace_best_btrt_tbm_B, loc = paste(Var1, Var2, sep = " & ")) |>
  ggplot() +
  geom_line(aes(x = Var3, y = value, color = loc)) +
  guides(color = "none") +
  labs(x = "MCMC iteration (post burn-in)") +
  theme_bw()

small3_trace_plt

ggsave(filename = "~/github/BTRTucker/plots/5_small3_tbm_trace.png",plot = small3_trace_plt, width = 8, height = 5)

# Make AAL atlas and register to MDT ----
aal_dir <- "~/github/BTRTucker/data/ADNI/ADNI 11/aal_for_SPM12/atlas"
data_dir <- "~/github/BTRTucker/data/ADNI/ADNI 11"
library(fslr)
library(oro.nifti)
# View the Yeo 7 atlas in MNI space
aal_mni <-
  readnii(
    file.path(
      aal_dir,"aal.nii"
    ),
    dtype = F,
    drop_dim = F
  )

aal_mdt <- fslr::flirt(
  infile = file.path(
    aal_dir,"aal.nii"
  ),
  reffile = file.path(data_dir, "ADNI_MDT", "ADNI_ICBM9P_mni_4step_MDT.nii.gz"),
  outfile = file.path(data_dir, "aal_MDT"),
  retimg = F
)

# Using Taki's suggested packages and functions
library(extrantsr)
library(fslr)
library(aal) # This is for the aal atlas
# Step 2: Register MNI to MDT
MNI2MDT <- extrantsr::registration(filename = file.path(fsldir(),"data","standard","MNI152_T1_1mm.nii.gz"),
                                   template.file = file.path(data_dir, "ADNI_MDT", "ADNI_ICBM9P_mni_4step_MDT.nii.gz"),
                                   typeofTransform = "SyN",
                                   other_interpolator = "nearestNeighbor")
# Step 3: Apply warp to Yeo7 label map using nearest neighbor interpolation
aalToMDT <- ants_apply_transforms(fixed = file.path(data_dir, "ADNI_MDT", "ADNI_ICBM9P_mni_4step_MDT.nii.gz"),moving = aal::aal_fname(),transformlist = MNI2MDT$fwdtransforms, interpolator = "nearestNeighbor")
# Step 4: Visualize Yeo7 atlas in MDT space
library(fslr)
orthographic(aalToMDT)
writeNIfTI(aalToMDT,file.path(data_dir,"aalMDT.nii.gz"))

aal_mdt <- readNIfTI(file.path(data_dir, "aalMDT.nii.gz"))
aal_template <- aal_mdt@.Data[,,80] |>
  reshape2::melt() |>
  mutate(value = ifelse(value ==0, NA, value),
         value = ifelse((value == 4111 | value == 4112),1,0)) |>
  ggplot() +
  geom_raster(aes(x = Var1, y = Var2, fill = value)) +
  scale_fill_distiller(palette = "Greys") +
  guides(fill = "none") +
  theme_void() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))
aal_template <- ggplotGrob(aal_template)

# Compare performance of models for making tables ----
# this is to examine the predictive value of the models
library(bayestensorreg)
data_dir <- "~/github/BTRTucker/data/ADNI/ADNI 11"
result_dir <- "~/github/BTRTucker/results/ADNI"
tbm_data <- readRDS(file.path(data_dir,"4_ADNI_TBM_slice080_TRdata.rds"))
tbm_data$y <- (tbm_data$y - mean(tbm_data$y))
tbm_data$eta <- tbm_data$eta[,-1]
vecX  <- apply(tbm_data$X,3,identity)

btrt_fitted_values <- function(result,new_data){
  all_B <- BTRT_all_B(result)
  D <- length(dim(all_B)) - 1
  all_B_vec <- apply(all_B,D+1,identity)
  XB <- crossprod(apply(new_data$X,D+1,identity),all_B_vec)
  eta_gam <- tcrossprod(new_data$eta,result$gam)
  fits <- XB + eta_gam
  fits <- apply(fits,1,median)
  return(fits)
}

btrcp_fitted_values <- function(result,new_data){
  n_samp <- length(result$betas)
  all_B <- btr_cp_all_B(result)
  D <- length(dim(all_B)) - 1
  all_B_vec <- apply(all_B,D+1,identity)
  XB <- crossprod(apply(new_data$X,D+1,identity),all_B_vec)
  eta_gam <- tcrossprod(new_data$eta,result$gam[seq(n_samp),])
  fits <- XB + eta_gam
  fits <- apply(fits,1,median)
  return(fits)
}

noimage_fit <- lm(tbm_data$y ~ -1 + tbm_data$eta)$fitted.values

btrt_fit <- btrt_fitted_values(btrt_tbm_best_res,tbm_data)
# btrcp_fit <- btrcp_fitted_values(btrcp_tbm_best_res, tbm_data)
ftrt_fit <- c(c(ftrt_tbm_best_B) %*% vecX) + c(tbm_data$eta %*% ftrt_tbm_best_gam)
ftrcp_fit <- c(c(ftrcp_tbm_best_B) %*% vecX) + c(tbm_data$eta %*% ftrcp_tbm_best_gam)
summary(c(c(btrt_tbm_best_B) %*% vecX))

plot(tbm_data$y,noimage_fit)
plot(tbm_data$y,btrt_fit)
abline(0,1)
plot(tbm_data$y,btrcp_fit)
abline(0,1)
plot(tbm_data$y,ftrt_fit)
plot(tbm_data$y,ftrcp_fit)
abline(0,1)
plot(ftrcp_fit,btrt_fit)
abline(0,1)
cor(ftrcp_fit,btrt_fit)

cor(tbm_data$y,noimage_fit)
cor(tbm_data$y,ftrcp_fit)
cor(tbm_data$y,ftrt_fit)
cor(tbm_data$y,btrt_fit)
cor(tbm_data$y,btrcp_fit)

rmspe <- function(fits, vals) {
  out <- sqrt(mean((fits - vals)^2))
  return(out)
}

rmspe(tbm_data$y,noimage_fit)
rmspe(tbm_data$y,ftrcp_fit)
rmspe(tbm_data$y,ftrt_fit)
rmspe(tbm_data$y,btrt_fit)
rmspe(tbm_data$y,btrcp_fit)
# Make this into a table
tbm_fit_df <-
  data.frame(
    Model = c(
      "No image",
      "FTR CP Rank 1",
      "FTR Tucker Rank 1,1",
      # "BTR CP Rank 1",
      "BTR Tucker Rank 1,2"
    ),
    Cor = c(
      cor(tbm_data$y,noimage_fit),
      cor(tbm_data$y,ftrcp_fit),
      cor(tbm_data$y,ftrt_fit),
      # cor(tbm_data$y,btrcp_fit),
      cor(tbm_data$y,btrt_fit)
    ),
    RMSPE = c(
      rmspe(tbm_data$y,noimage_fit),
      rmspe(tbm_data$y,ftrcp_fit),
      rmspe(tbm_data$y,ftrt_fit),
      # rmspe(tbm_data$y,btrcp_fit),
      rmspe(tbm_data$y,btrt_fit)
    )
  )
tbm_fit_df

library(knitr)
tbm_fit_table <- kable(tbm_fit_df,digits = 3, "latex")
print(tbm_fit_table)
