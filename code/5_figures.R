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
# Now using the rank selection pattern to find the optimal rank
btrt_rank_sel <- c(sapply(1:5,function(x) paste(rep(x,2),collapse = "")),"34","43")
final_btrt_files <- character()
for(btrt_rank in btrt_rank_sel){
  final_btrt_files <- c(final_btrt_files,grep(pattern = btrt_rank, btrt_files,value = T))
}
btrt_files <- final_btrt_files

job::job({
  library(parallel)
  cl <- makeCluster(7)
  btrt_B <- parLapply(cl,btrt_files, function(fn){
    library(bayestensorreg)
    res <- readRDS(fn)
    out_B <- BTRT_final_B(res)
    return(out_B)
  })
  stopCluster(cl)
  saveRDS(btrt_B, file = "~/github/BTRTucker/results/data_simulations/5_btrt_B.rds")
}, import = c(btrt_files))

btrt_B <- readRDS("~/github/BTRTucker/results/data_simulations/5_btrt_B.rds")
names(btrt_B) <- btrt_files
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
        panel.grid = element_blank(),
        panel.background = element_rect(color = "grey50"),
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

# Simulated data median ESS ----
result_dir <- "~/github/BTRTucker/results/data_simulations"
btrt_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "btrtucker_results_rank", value = T)

library(bayestensorreg)
library(coda)
job::job({
  btrt_ess <- sapply(btrt_files, function(fn) {
    res <- readRDS(fn)
    allB <- BTRT_all_B(res)
    allB_vec <- t(apply(allB,3,identity))
    allESS <- coda::effectiveSize(allB_vec)
    return(median(allESS))
  })
},  import = c(btrt_files))

# Simulated data best coefficients ----
result_dir <- "~/github/BTRTucker/results/data_simulations"
library(bayestensorreg)
# > BTRT ----
btrt_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "btrtucker", value = T)
# Now using the rank selection pattern to find the optimal rank
btrt_rank_sel <- c(sapply(1:5,function(x) paste(rep(x,2),collapse = "")),"34","43")
final_btrt_files <- character()
for(btrt_rank in btrt_rank_sel){
  final_btrt_files <- c(final_btrt_files,grep(pattern = btrt_rank, btrt_files,value = T))
}
btrt_files <- final_btrt_files
btrt_dic <- sapply(btrt_files, function(fn){
  library(bayestensorreg)
  res <- readRDS(fn)
  out <- DIC(res$llik, burn_in = 1000)
  return(out)
})
which.min(btrt_dic) # Rank 4,4
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
which.min(btrcp_dic) # Rank 1
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
cbind(ftrt_aic,ftrt_bic)
which.min(ftrt_aic) # Ranks 1,1 # Ranks 2,2 now with better stopping rules # Ranks 4,4 after bugfix, extended to 5,5 with new rank selection algorithm
which.min(ftrt_bic) # Ranks 2,2

# > FTR CP ----
ftrcp_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "ftr_cp", value = T)
ftrcp_aic <- sapply(ftrcp_files, function(fn){
  res <- readRDS(fn)
  num_params <- length(res$gam) + length(unlist(res$betas))
  aic_out <- 2 * num_params - 2 * res$llik
  return(aic_out)
})
ftrcp_bic <- sapply(ftrcp_files, function(fn){
  sample_size <- 1000
  res <- readRDS(fn)
  num_params <- length(res$gam) + length(unlist(res$betas))
  bic_out <-  log(sample_size) * num_params - 2 * res$llik
  return(bic_out)
})
cbind(ftrcp_aic,ftrcp_bic)
which.min(ftrcp_aic) # Rank 4, Rank 6 with new rank selection algorithm
which.min(ftrcp_bic) # Rank 4, Rank 5 with new rank selection algorithm

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
# I changed this on 9-20-2022 to compare the results from the fours models with the highest ranks
best_files <- c(btrt_files[which.min(btrt_dic)],btrcp_files[4],ftrt_files[16],ftrcp_files[4])
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
      mutate(Model = "BTR CP 4")
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

ggsave(filename = "~/github/BTRTucker/plots/5_simulated_gam_w_btrcp4.png",plot = best_gam_plt,width = 16, height =3)

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
btrt_result_dir <- "~/github/BTRTucker/results/ADNI/BTRTucker_11k"
btrt_tbm_files <- list.files(btrt_result_dir, full.names = T) |>
  grep(pattern = "BTRTucker_rank", value = T)

# job::job({
#   library(parallel)
#   cl <- makeCluster(9)
#   btrt_tbm_B <- parLapply(cl,btrt_tbm_files, function(fn){
#     library(bayestensorreg)
#     res <- readRDS(fn)
#     out_B <- BTRT_final_B(res)
#   return(out_B)})
#   saveRDS(btrt_tbm_B, file = file.path(btrt_result_dir,"5_btrt_slice080_tbm_B.rds"))
# }, import = c(btrt_tbm_files))
btrt_tbm_B <- readRDS(file.path(btrt_result_dir,"5_btrt_slice080_tbm_B.rds"))
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
  mutate(#R1 = substring(L1,71,71),
         #R2 = substring(L1,72,72),
         R1 = substring(L1, 85,85),
         R2 = substring(L1,86,86),
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
             labeller = label_bquote(rows = R[1] == .(R1), cols = R[2] == .(R2))) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 18),
        aspect.ratio = 1)
btrt_tbm_plt
ggsave(filename = "~/github/BTRTucker/plots/5_btrt_slice080_11k_tbm_B.png", plot = btrt_tbm_plt, width = 11, height = 9)

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
  mutate(R1 = substring(L1,71,71),
         R2 = substring(L1,72,72),
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
             labeller = label_bquote(rows = R[1] == .(R1), cols = R[2] == .(R2))) +
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

# > FTR CP ----
ftrcp_tbm_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "TBM_FTR_CP_rank", value = T)

ftrcp_tbm_B <- sapply(ftrcp_tbm_files, function(fn){
  res <- readRDS(fn)
  if(class(res) == "list"){
    return(res$B)
  }else{return(NULL)}
}, simplify = F)
ftrcp_tbm_B <- ftrcp_tbm_B[!sapply(ftrcp_tbm_B,is.null)]

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
ftrcp_tbm_df <-
  reshape2::melt(ftrcp_tbm_B) |>
  mutate(Rank = substring(L1,68,68)) |>
         # value = value / (1e-3),
         # value = ifelse(abs(value) > 1,sign(value)*1,value),
         # value = value * (1e-3),
         # matchRank = R1 == R2) |>
  select(-L1)
ftrcp_lim <- quantile(ftrcp_tbm_df$value, probs = c(0.99))
ftrcp_tbm_df <-
  mutate(ftrcp_tbm_df,
         value = value / ftrcp_lim,
         value = ifelse(abs(value) > 1,sign(value)*1,value),
         value = value * ftrcp_lim,
  )
ftrcp_tbm_plt <- ggplot(ftrcp_tbm_df) +
  annotation_custom(grob = template_grob) +
  geom_raster(aes(x = Var1, y = Var2, fill = value,alpha = value))+
  scale_fill_gradient2("",high = "blue",low = "red",mid = "black",na.value = "black",
                       limits = c(-ftrcp_lim,ftrcp_lim)
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
ftrcp_tbm_plt
ggsave(filename = "~/github/BTRTucker/plots/5_ftrcp_slice080_tbm_B.png", plot = ftrcp_tbm_plt, width = 16, height = 3)

# Best TBM B ----
library(bayestensorreg)
library(tidyverse)
library(ggrepel)
library(oro.nifti)
result_dir <- "results/"
data_dir <- "data/"
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
# > BTR Tucker ----
# btrt_result_dir <- "~/github/BTRTucker/results/ADNI/BTRTucker_11k_EDA"
btrt_tbm_files <- list.files(result_dir, pattern = "factor2_BTRT.+_rank", full.names = TRUE)
# rep_ranks <- sapply(2:3, function(x) paste(rep(x,2),collapse = ""))
# final_btrt_tbm_files <- character()
# for(rr in rep_ranks) {
#   final_btrt_tbm_files <- c(final_btrt_tbm_files,grep(pattern = rr, btrt_tbm_files, value = T))
# }
# btrt_tbm_files <- final_btrt_tbm_files
# >> Verify log-likelihoods ----
btrt_tbm_lliks <- sapply(btrt_tbm_files, function(fn){
  res <- readRDS(fn)
  return(as.matrix(res$llik))
}, simplify = F)
library(stringr)
btrt_tbm_llik_df <-
  reshape2::melt(btrt_tbm_lliks) |>
  mutate(rank_names = substring(str_extract(L1, "rank(\\d+)"), first = 5),
         R1 = substring(rank_names, 1, 1),
         R2 = substring(rank_names, 2, 2),
         Rank = paste(R1,",",R2)) |>
  dplyr::select(Var1,R1,R2,Rank,value)
btrt_tbm_llik_ends <- filter(btrt_tbm_llik_df, Var1 == max(Var1))
ggplot(btrt_tbm_llik_df) +
  geom_line(aes(x = Var1, y = value, color = Rank)) +
  geom_text_repel(aes(x = Var1, y = value, label = Rank),
                  nudge_x = 100, direction = "x", nudge_y = 100,
                  data = btrt_tbm_llik_ends) +
  labs(y = "log-likelihood",x="MCMC Iteration") +
  facet_grid(R1 ~ R2) +
  guides(color = "none") +
  theme_bw() +
  theme(text = element_text(size = 18))
dplyr::group_by(btrt_tbm_llik_df, Rank) |>
  dplyr::summarize(mean_llik = mean(value),
            sd_llik = sd(value),
            var_llik = var(value)) |>
  ggplot(aes(x = mean_llik,y = sd_llik)) +
  geom_point() +
  geom_text(aes(label = Rank)) +
  scale_y_continuous(trans = "log10")
# Note that all the models with at least one R_j = 1 diverged when 11,000 samples are taken -> identifiability issue with low margin ranks
btrt_tbm_dic <- sapply(btrt_tbm_files, function(fn){
  # if(length(grep(pattern = "1",fn)) == 1){return(Inf)}else{
    res <- readRDS(fn)
    dic_out <- DIC(res$llik, burn_in = 1000)
    return(dic_out)
  # }
})
which.min(btrt_tbm_dic) # Rank 3,2 # Rank 4,2 for slice 080 # Rank 1,2 for slice 080 with age and gender added in # Rank 4,4 after 11000 samples # Rank 2, 3 after rerunning # Now rank 2,2 following the rank selection algorithm # And finally rank 3,3 after proper EDA
# Something to note here is that the DIC is a linear combination of mean and variance of the log-likelihood, so optimizing over both summary statistics is required.
btrt_tbm_best_res <- readRDS(btrt_tbm_files[which.min(btrt_tbm_dic)])
btrt_tbm_best_res <- readRDS(list.files(result_dir, pattern = "EduAPOE_*_BTRTucker_10k", full.names = TRUE))
btrt_tbm_B <- readRDS("~/github/BTRTucker/results/BTRTucker_11k/5_btrt_slice080_tbm_B.rds")
# btrt_tbm_best_B <- btrt_tbm_B[[which.min(btrt_tbm_dic)]]
btrt_tbm_best_B <- BTRT_final_B(btrt_tbm_best_res)
unmasked_result <- readRDS("~/github/BTRTucker/results/BTRTucker_11k/4_ADNI_TBM_BTRTucker_rank33.rds")
unmasked_B <- BTRT_final_B(unmasked_result)

btrt_tbm_all_B <- BTRT_all_B(btrt_tbm_best_res)
btrt_tbm_vec_B <- apply(btrt_tbm_all_B,3,identity)
btrt_tbm_all_eff_B <- coda::effectiveSize(t(btrt_tbm_vec_B))
quantile(btrt_tbm_all_eff_B,probs = c(.01,.99))

plt <- reshape2::melt(btrt_tbm_best_B) |>
  # mutate(Data = "Masked") |>
  # full_join(
  #   reshape2::melt(unmasked_B) |>
  #     mutate(Data = "Unmasked")
  # ) |>
  mutate(value = ifelse(abs(value) > 1e-5,sign(value) * 1e-5,value)) |>
  left_join(insideAAL) |>
  mutate(value = value * mask) |>
  ggplot() +
  annotation_custom(grob = aal_template) +
  geom_raster(aes(x = Var1, y = Var2, fill = value,
                  alpha = abs(value) / max(abs(value))
                  # alpha = abs(value) / 6e-2
  )) +
  scale_fill_gradient2("",high = "cornflowerblue",low = "red",
                       mid = "black",
                       limits = c(-1e-5,1e-5),
                       na.value = "black"
  ) +
  guides(alpha = "none") +
  # facet_grid(~ Data, scales = "fixed") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        text = element_text(size = 18),
        aspect.ratio = 1)
plt
ggsave("plots/5_APOE4factor2_btrt_B_rank33.png",
       plot = plt, width = 4.5, height = 3)

# > BTR CP ----
# all BTR CP models fail due to infinite values in the posterior precision of beta
btrcp_tbm_best_B <- matrix(0,220,220)
# btrcp_tbm_files <- list.files(result_dir, full.names = T) |>
#   grep(pattern = "BTR_CP_rank", value = T)
# library(bayestensorreg)
# btrcp_tbm_dic <- sapply(btrcp_tbm_files, function(fn){
#   res <- readRDS(fn)
#   out_llik <- res$llik[res$llik != 0]
#   dic_out <- DIC(out_llik, burn_in = 0)
#   return(dic_out)
# })
# which.min(btrcp_tbm_dic)
# btrcp_tbm_best_res <- readRDS(btrcp_tbm_files[which.min(btrcp_tbm_dic)])
# btrcp_tbm_B <- readRDS("~/github/BTRTucker/results/ADNI/5_btrcp_slice080_tbm_B.rds")
# btrcp_tbm_best_B <- btrcp_tbm_B[[which.min(btrcp_tbm_dic)]]

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
ftrt_tbm_bic <- sapply(ftrt_tbm_files, function(fn){
  sample_size <- 817
  res <- readRDS(fn)
  if(class(res) == "try-error"){
    return(Inf)
  }else{
    num_params <- length(res$gam) + length(unlist(res$betas)) + length(c(res$G))
    bic_out <-  log(sample_size) * num_params - 2 * res$llik
    return(bic_out)
  }
})
cbind(ftrt_tbm_aic,ftrt_tbm_bic)
which.min(ftrt_tbm_aic) # Rank 1,1 # Rank 4,4 when age and gender are included # Rank 1,1 after bugfix
which.min(ftrt_tbm_bic) # Rank 1,1 when gender and age are included and after bugfix
ftrt_tbm_best_res <- readRDS(ftrt_tbm_files[which.min(ftrt_tbm_aic)])
ftrt_tbm_best_B <- ftrt_tbm_best_res$B

# > FTR CP ----
ftrcp_tbm_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "FTR_CP_rank", value = T)
# This is the number of parameters used to estimate the tensor coefficient
# sapply(ftrcp_tbm_files, function(x){
#   res <- readRDS(x)
#   return(length(unlist(res$betas)))
# })
ftrcp_tbm_aic <- sapply(ftrcp_tbm_files, function(fn){
  res <- readRDS(fn)
  num_params <- length(res$gam) + length(unlist(res$betas))
  aic_out <- 2 * num_params - 2 * res$llik
  return(aic_out)
})
ftrcp_tbm_bic <- sapply(ftrcp_tbm_files, function(fn){
  sample_size <- 817
  res <- readRDS(fn)
  num_params <- length(res$gam) + length(unlist(res$betas)) +1
  bic_out <-  log(sample_size) * num_params - 2 * res$llik
  return(bic_out)
})
cbind(ftrcp_tbm_aic,ftrcp_tbm_bic)
which.min(ftrcp_tbm_aic) # Rank 2 # Rank 1 when age and gender are included
which.min(ftrcp_tbm_bic) # Rank 1 when age and gender are included
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
  mutate(Model = "BTR Tucker 3,3") |>
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
  dplyr::group_by(Model) |>
  dplyr::summarize(min_v = min(value),
            max_v = max(value),
            num_large = sum(abs(value) > 1e-5))

tbm_best_plt <-
  mutate(tbm_best_B,
         value = ifelse(abs(value) > 1e-5,sign(value) * 1e-5,value),
         Model = factor(Model, levels = c("GLM","FTR CP 1","FTR Tucker 1,1",
                                          # "BTR CP 1",
                                          "BTR Tucker 3,3"))
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

# This is to create the trace plots
reshape2::melt(btrt_tbm_best_gam) |>
  ggplot() +
  geom_line(aes(x = Var1, y = value)) +
  facet_wrap(~Var2, scales = "free")

# This checks the posterior densities
plt2 <-
  reshape2::melt(btrt_tbm_best_gam) |>
  mutate(Name = c("Education","APOE4_1", "APOE4_2")[Var2]) |>
  ggplot() +
  stat_density(aes(x = value, fill = Name, color = Name), position = "dodge",
               alpha = 0.3) +
  scale_color_discrete("") +
  scale_fill_discrete("") +
  labs(y = "Posterior Density") +
  # facet_wrap(~Name, scales = "free")
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        legend.position = "bottom")
plt2
ggsave("plots/5_APOE4factor2_btrt_gam_rank33.png",
       plot = plt2, width = 6.5, height = 3)

reshape2::melt(btrt_tbm_best_gam) |>
  group_by(Var2) |>
  dplyr::summarize(mean = mean(value),
            sd = sd(value),
            se = sd(value) / sqrt(1000),
            low = mean - 3 *se,
            high = mean + 3*se)

# BTR CP
# btrcp_tbm_best_gam <- btrcp_tbm_best_res$gam

# FTR Tucker
ftrt_tbm_best_gam <- ftrt_tbm_best_res$gam

# FTR CP
ftrcp_tbm_best_gam <- ftrcp_tbm_best_res$gam

# GLM / No image
glm_tbm_gam <- glm_tbm_res$gam

# gam_names <- c("Age","Education","Sex","APOE4")
gam_names <- c("Education","APOE4")

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
# hue_pal <- gg_color_hue(4)
hue_pal <- gg_color_hue(2)
library(RColorBrewer)
# my_pal <- brewer.pal(4,"Paired")
my_pal <- c(brewer.pal(6,"Paired"),"grey50")

# Plot
tbm_best_gam_plt <-
  reshape2::melt(btrt_tbm_best_gam) |>
  mutate(Model = "BTR Tucker 3,3",
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
data_dir <- "~/github/BTRTucker/data/"
# Step 2: Register MNI to MDT
MNI2MDT <- extrantsr::registration(filename = file.path(fsldir(),"data","standard","MNI152_T1_1mm.nii.gz"),
                                   template.file = file.path(data_dir, "ADNI_ICBM9P_mni_4step_MDT.nii.gz"),
                                   typeofTransform = "SyN",
                                   other_interpolator = "nearestNeighbor")
# Step 3: Apply warp to Yeo7 label map using nearest neighbor interpolation
aalToMDT <- ants_apply_transforms(fixed = file.path(data_dir, "ADNI_ICBM9P_mni_4step_MDT.nii.gz"),moving = aal::aal_fname(),transformlist = MNI2MDT$fwdtransforms, interpolator = "nearestNeighbor")
# Step 4: Visualize Yeo7 atlas in MDT space
library(fslr)
orthographic(aalToMDT)
writeNIfTI(aalToMDT,file.path(data_dir,"aalMDT"))

aal_mdt <- readNIfTI(file.path(data_dir, "aalMDT.nii.gz"))
library(tidyverse)
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
result_dir <- "~/github/BTRTucker/results/ADNI/After_EDA"
tbm_data <- readRDS(file.path(data_dir,"4_ADNI_TBM_slice080_TRdata.rds"))
tbm_data$y <- (tbm_data$y - mean(tbm_data$y))
tbm_data$eta <- tbm_data$eta[,-c(1,2,4)]
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
# cor(tbm_data$y,btrcp_fit)

rmspe <- function(fits, vals) {
  out <- sqrt(mean((fits - vals)^2))
  return(out)
}

rmspe(tbm_data$y,noimage_fit)
rmspe(tbm_data$y,ftrcp_fit)
rmspe(tbm_data$y,ftrt_fit)
rmspe(tbm_data$y,btrt_fit)
# rmspe(tbm_data$y,btrcp_fit)
# Make this into a table
tbm_fit_df <-
  data.frame(
    Model = c(
      "No image",
      "FTR CP Rank 1",
      "FTR Tucker Rank 1,1",
      # "BTR CP Rank 1",
      "BTR Tucker Rank 3,3"
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

# Compare banded signal ----
result_dir <- "~/github/BTRTucker/results/data_simulations"
banded_btrt_files <- list.files(result_dir, full.names = T) |>
  grep(pattern = "banded_signal", value = T)
library(bayestensorreg)
banded_btrt_B <- sapply(banded_btrt_files, function(fn){
  res <- readRDS(fn)
  out_B <- BTRT_final_B(res)
  return(out_B)
}, simplify = F)

reshape2::melt(banded_btrt_B) |>
  mutate(R1 = substring(L1, 83, 83),
         R2 = substring(L1, 84, 84)) |>
  select(Var1,Var2,value,R1,R2) |>
  ggplot() +
  geom_raster(aes(x = Var1, y = Var2, fill = value)) +
  facet_grid(R1~R2)

# Plot diff_ranks estimates ----
library(bayestensorreg)
subjects = 400
tensor_dims = c(50, 50, 50)
CNR = 1
num_active = 1
other_covar = c(1, 1)

ranks <- c(2, 5, 2)
set.seed(95064)
betas <- mapply(function(r, p) {
  out <- sapply(seq(r), function(rr){
    start_point <- round(runif(1, 0.1, 0.9) * p)
    lgth <- round(runif(1, 0.15, 0.2) * p)
    end_point <- start_point + lgth
    if(end_point > p) end_point <- p
    out <- rep(0, p)
    out[start_point:end_point] <- 1
    return(out)
  })
}, r = ranks, p = tensor_dims)

G <- array(runif(prod(ranks), 0.5, 1), dim = ranks)

B <- composeTuckerCore(betas, G)

eta <-
  matrix(rnorm(subjects * length(other_covar)), subjects, length(other_covar))
gam <- other_covar
X <-
  array(rnorm(prod(tensor_dims) * subjects), dim = c(tensor_dims, subjects))
y <-
  apply(X, length(dim(X)), function(xx)
    sum(xx * B * CNR)) + c(eta %*% gam) + rnorm(subjects)
diff_ranks <- list(
  y = y,
  X = X,
  true_B = B,
  eta = eta,
  gam = gam
)
save_dir <- "results/simulated_diff_rank252/"
res_files <- list.files(save_dir, full.names = TRUE)
library(bayestensorreg)
all_dic <- sapply(res_files, function(x) {
  res <- readRDS(x)
  return(DIC(res$llik, burn_in = 100))
})
which.min(all_dic)

# > Examine the results graphically ----
res <- readRDS(names(which.min(all_dic)))
png("plots/5_rank252_llik.png", width = 8, height = 4.5, res = 72, units = "in")
plot(res$llik, type ='l', ylab = "Log-likelihood", xlab = "MCMC iteration",
     main = "Rank 1,1,1")
abline(v = 100, lty = 2, col = 'red')
text(x = 100, y = -2300, labels = "burn-in", pos = 2, col = 'red')
dev.off()
final_B <- BTRT_final_B(res)

# diff_ranks <- readRDS("results/simulated_diff_rank/1_diff_ranks_data.rds")
B <- diff_ranks$true_B
(big.i <- which.max(apply(B, 1, function(x) sum(x > 0))))
(big.j <- which.max(apply(B, 2, function(x) sum(x > 0))))
(big.k <- which.max(apply(B, 3, function(x) sum(x > 0))))
png("plots/5_rank252_i_comparison.png", width = 8, height = 4.5, res = 72, units = "in")
par(mar=c(1,1,3,1), mfrow = c(1,2))
image(B[big.i,,], col = viridis::viridis(12), zlim = c(-2.5,2.5), xaxt = 'n',
      yaxt = 'n', main = "Truth, slice 1")
image(final_B[big.i,,], col = viridis::viridis(12), zlim = c(-2.5,2.5),
      xaxt = 'n', yaxt = 'n', main = "Estimate, slice 1")
dev.off()
png("plots/5_rank252_j_comparison.png", width = 8, height = 4.5, res = 72, units = "in")
par(mar=c(1,1,3,1), mfrow = c(1,2))
image(B[,big.j,], col = viridis::viridis(12), zlim = c(-2.5,2.5), xaxt = 'n',
      yaxt = 'n', main = "Truth, slice 2")
image(final_B[,big.j,], col = viridis::viridis(12), zlim = c(-2.5,2.5),
      xaxt = 'n', yaxt = 'n', main = "Estimate, slice 2")
dev.off()
png("plots/5_rank252_k_comparison.png", width = 8, height = 4.5, res = 72, units = "in")
par(mar=c(1,1,3,1), mfrow = c(1,2))
image(B[,,big.k], col = viridis::viridis(12), zlim = c(-2.5,2.5), xaxt = 'n',
      yaxt = 'n', main = "Truth, slice 3")
image(final_B[,,big.k], col = viridis::viridis(12), zlim = c(-2.5,2.5),
      xaxt = 'n', yaxt = 'n', main = "Estimate, slice 3")
dev.off()
err_B <- final_B - B
summary(c(err_B))
summary(c(B))
nz_B <- which(B != 0)
summary(c(err_B[nz_B] / B[nz_B]))
