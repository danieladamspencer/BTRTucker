# Figures and tables for simulated data analysis
# Libraries ----
library(bayestensorreg)
library(data.table)
library(ggplot2)
library(RColorBrewer)

# Directories ----
data_dir <- "data/"
plot_dir <- "plots/"
result_dir <- "results/data_simulations_R3/"

# Select best result ----
# For the Bayesian methods, use DIC. For Frequentist methods, use AIC.
model_names <- c("BTRT", "BTR_CP", "FTRT", "FTR_CP")
best_results <-
  lapply(model_names,
         function(mdl_nm) {
           result_files <- list.files(result_dir, pattern = tolower(mdl_nm), full.names = TRUE)
           result_files <- grep("rank[1-4]+", result_files, value = TRUE)
           if(grepl("BTR", mdl_nm)) {
             result_criterion <- sapply(result_files, function(res_fn) {
               res <- readRDS(res_fn)
               nsamp <- length(res$betas)
               nburn <- length(res$llik) - nsamp
               DIC(log_likelihood = res$llik, burn_in = nburn)
             })
           }
           if(grepl("FTR", mdl_nm)) {
             result_criterion <- sapply(result_files, function(res_fn) {
               res <- readRDS(res_fn)
               num_params <- length(res$gam) + length(unlist(res$betas)) + length(c(res$G))
               2 * num_params - 2 * res$llik
             })
           }
           best_fn <- names(result_criterion)[which.min(result_criterion)]
           best_rank <- stringr::str_extract(best_fn, pattern = "rank[1-9]*")
           best_res <- readRDS(best_fn)
           best_res$rank <- best_rank
           best_res
         })
names(best_results) <- model_names
# Print the ranks so we see which one is selected for each model
sapply(best_results, function(x) x$rank)

# Plot best tensor coefficient ----
plot_model_names <- c("BTR Tucker", "BTR CP", "FTR Tucker", "FTR CP")
B_dt <- Map(function(res, mdl_nm) {
  if(grepl("BTR", mdl_nm)) {
    res$B <- BTRT_final_B(res)
  }
  B_out <- as.data.table(reshape2::melt(res$B))
  B_out[, Model := mdl_nm]
  B_out[, Rank := sub("rank", "", res$rank)]
  B_out[nchar(Rank) > 1, Rank := paste(substring(Rank, 1, 1), substring(Rank, 2, 2), sep = ",")]
  B_out[]
}, best_results, plot_model_names)
GLM_B <- readRDS(list.files(result_dir, pattern = "glm", full.names = TRUE))
GLM_B <- as.data.table(reshape2::melt(GLM_B))
GLM_B[, Model := "GLM"]
GLM_B[, Rank := ""]
B_dt$GLM <- GLM_B
simulated_data <- readRDS(list.files(data_dir, pattern = "01_simulated_data", full.names = TRUE))
true_B <- as.data.table(reshape2::melt(simulated_data$true_B))
true_B[, Model := "Truth"]
true_B[, Rank := ""]
B_dt$Truth <- true_B
B_dt <- Reduce(rbind, B_dt)
# Calculate the RMSE to put that in the plot
true_B <- as.data.table(reshape2::melt(simulated_data$true_B, value.name = "truth"))
B_dt <- true_B[B_dt, on = c("Var1", "Var2")]
B_dt[, RMSE := round(sqrt(mean((value - truth)^2)), 4), by = c("Model", "Rank")]
B_dt[, RMSE := paste0("(", as.character(RMSE), ")")]
B_dt[RMSE == "(0)", RMSE := ""]
B_dt[, Model_Rank := paste(Model, Rank)]

simB_plot <-
  ggplot(B_dt) +
  geom_raster(aes(x = Var1, y = Var2, fill = value)) +
  facet_wrap(~ Model_Rank + RMSE) +
  scale_fill_gradient2("") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        text = element_text(size = 18),
        legend.title = element_blank(),
        aspect.ratio = 1)
simB_plot
ggsave(paste0(plot_dir, "03_simulated_best_B_maxrank4.png"), plot = simB_plot,
       width = 8, height = 6, units = "in")

# Plot best gam result ----
plot_model_names <- c("BTR Tucker", "BTR CP", "FTR Tucker", "FTR CP")
btr_gam_dt <- Map(function(res, mdl_nm) {
  gam_out <- as.data.table(reshape2::melt(res$gam))
  setnames(gam_out, c("Var1", "Var2"), c("iteration", "coefficient"))
  gam_out[, Model := mdl_nm]
  gam_out[, Rank := sub("rank", "", res$rank)]
  gam_out[nchar(Rank) > 1, Rank := paste(substring(Rank, 1, 1), substring(Rank, 2, 2), sep = ",")]
  gam_out[, Model_Rank := paste(Model, Rank)]
  gam_out[]
}, best_results[grep("BTR", model_names, value = TRUE)], grep("BTR", plot_model_names, value = TRUE))
btr_gam_dt <- Reduce(rbind, btr_gam_dt)
ftr_gam_dt <- Map(function(res, mdl_nm) {
  gam_out <- as.data.table(reshape2::melt(t(res$gam)))
  setnames(gam_out, c("Var1", "Var2"), c("iteration", "coefficient"))
  gam_out[, coefficient := as.numeric(coefficient)]
  gam_out[, Model := mdl_nm]
  gam_out[, Rank := sub("rank", "", res$rank)]
  gam_out[nchar(Rank) > 1, Rank := paste(substring(Rank, 1, 1), substring(Rank, 2, 2), sep = ",")]
  gam_out[, Model_Rank := paste(Model, Rank)]
  gam_out[]
}, best_results[grep("FTR", model_names, value = TRUE)], grep("FTR", plot_model_names, value = TRUE))
glm_gam <- data.table(iteration = 1, coefficient = 1:3, Model = "GLM", Rank = "",
                      value = lm(simulated_data$y ~ -1 + simulated_data$eta)$coefficients)
glm_gam[, Model_Rank := paste(Model, Rank)]
ftr_gam_dt$GLM <- glm_gam
ftr_gam_dt <- Reduce(rbind, ftr_gam_dt)
true_gam <- data.table(coefficient = 1:3, value = simulated_data$gam, Model = "Truth", Rank = "")


my_pal <- c(brewer.pal(6,"Paired"),"grey50")

simgam_plot <-
  ggplot() +
  geom_density(data = btr_gam_dt, aes(x = value, fill = Model_Rank, color = Model_Rank), alpha = 0.3) +
  geom_point(data = ftr_gam_dt, aes(x = value, y = 0, color = Model_Rank, fill = Model_Rank),
             shape = 19, size = 4,) +
  geom_vline(data = true_gam, aes(xintercept = value)) +
  scale_fill_manual(values = my_pal[-(3:4)]) +
  scale_color_manual(values = my_pal[-(3:4)]) +
  facet_grid(~coefficient, scales = "free",labeller = label_bquote(cols = gamma[.(coefficient)])) +
  labs(y = "Posterior Density") +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        # axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        text = element_text(size=18))
simgam_plot

ggsave(paste0(plot_dir, "03_simulated_best_gam_maxrank4.png"), plot = simgam_plot,
       width = 16, height = 3, units = "in")
