# ==============================================================================
# Script: Lambda Parameter Recovery Plots
# Description: Runs SEM models for compliance decisions across specific time 
# aggregations for both Unknown (T1) and Known (T2) out-group scenarios.
# Extracts recovered lambda parameters and visualizes them.
# ==============================================================================

library(lavaan)
library(ggplot2)
library(patchwork) # For combining plots
library(dplyr)     # For data wrangling
library(ggpubr)    # For extracting legend

rm(list = ls())

# --- 1. Setup: Paths, Data, Colors, and Shapes ---
# Ensure these paths are correct for your system
path_github <- "C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos  <- "C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

# Load data
load(paste0(path_datos, "/Datos_islitas_recode.Rdata")) # expects object 'df'

# Helper function to select columns by round
declare_get_columns <- function(prefix, suffix, start, end = NULL, data) {
  if (is.null(end)) end <- start
  rounds <- start:end
  cols <- unlist(lapply(rounds, function(r) {
    patt <- paste0("^", prefix, "\\.", r, "\\.player\\.", suffix, "$")
    grep(patt, names(data), value = TRUE)
  }))
  if (length(cols) == 0) message("No matching columns for ", prefix, suffix, " rounds ", start, "-", end)
  return(cols)
}

# --- 2. SEM Model Functions ---

# Stage 1: Shared Area - Unknown Out-group (T1)
coef_SA_T1_sem <- function(data, R_start, R_end) {
  cols <- declare_get_columns("T1juegoalgas", "T1_extraccion_libre", R_start, R_end, data)
  data$average_extraction_ini <- rowMeans(data[, cols, drop = FALSE], na.rm = TRUE)
  data$average_compliance_ini <- 1 - data$average_extraction_ini / 50
  data$belief_compliance_pm   <- 1 - data$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50
  data$belief_compliance_union<- 1 - data$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50
  
  data$confianza_caleta<- as.numeric(scale(data$survey1.1.player.confianza_caleta))
  data$conflicto_caleta<- as.numeric(scale(data$survey1.1.player.conflicto_caleta))
  data$confianza_pm<- as.numeric(scale(data$survey1.1.player.confianza_pm))
  data$conflicto_pm<- as.numeric(scale(data$survey1.1.player.conflicto_pm)) 
  
  if (R_start > 1) {
    cols_obs <- declare_get_columns("T1juegoalgas", "T1_extraccion_otros_libre", R_start - 1, R_start - 1, data)
    data$average_extraction_observed_ini <- rowMeans(data[, cols_obs, drop = FALSE], na.rm = TRUE)
    data$average_compliance_observed_ini_lag <- 1 - (data$average_extraction_observed_ini / 150)
  } else {
    data$average_compliance_observed_ini_lag <- NA
  } 
  
  sem_model <- '
    # Regressions
    belief_compliance_pm ~ confianza_pm + conflicto_pm
    belief_compliance_union ~ confianza_caleta + conflicto_caleta
    average_compliance_ini ~ c1*belief_compliance_pm + c2*belief_compliance_union + confianza_pm + conflicto_pm + confianza_caleta + conflicto_caleta + c3*average_compliance_observed_ini_lag
    
    # Intercepts
    belief_compliance_pm ~ 1
    belief_compliance_union ~ 1
    average_compliance_ini ~ 1

    # --- PARAMETER RECOVERY ---
    gamma    := c1 + c2 + c3
    lambda_1 := c1 / (c1 + c2 + c3)
    lambda_2 := c2 / (c1 + c2 + c3)
    lambda_3 := c3 / (c1 + c2 + c3)
    lambda_4 := (c1 + c2) / (c1 + c2 + c3)
  '
  
  fit <- sem(sem_model, data = data, estimator = "ML", se = "bootstrap", bootstrap = 1000, parallel = "multicore", ncpus = 4)
  return(fit)
}

# Stage 2: Shared Area - Known Out-group (T2)
coef_SA_T2_sem <- function(data, R_start, R_end) {
  cols <- declare_get_columns("T2juegoalgas", "T2_extraccion_metat", R_start, R_end, data)
  data$average_extraction_ini <- rowMeans(data[, cols, drop = FALSE], na.rm = TRUE)
  data$average_compliance_ini <- 1 - data$average_extraction_ini / 50
  data$belief_compliance_pm   <- 1 - data$beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini / 50
  data$belief_compliance_union <- 1 - (data$beliefsT2inicial.1.player.T2_belief_caleta_ini / 50)
  
  data$confianza_caleta<- as.numeric(scale(data$survey1.1.player.confianza_caleta))
  data$conflicto_caleta<- as.numeric(scale(data$survey1.1.player.conflicto_caleta))
  data$confianza_metat<- as.numeric(scale(data$survey2.1.player.confianza_caleta_conocida_mean))
  data$conflicto_metat<- as.numeric(scale(data$survey2.1.player.conflicto_caleta_conocida_mean)) 
  
  if (R_start > 1) {
    cols_obs <- declare_get_columns("T2juegoalgas", "T2_extraccion_otros_metat", R_start - 1, R_start - 1, data)
    data$average_extraction_observed_ini <- rowMeans(data[, cols_obs, drop = FALSE], na.rm = TRUE)
    data$average_compliance_observed_ini_lag <- 1 - (data$average_extraction_observed_ini / 150)
  } else {
    data$average_compliance_observed_ini_lag <- NA
  }
  
  sem_model <- '
    # Regressions
    belief_compliance_pm ~ confianza_metat + conflicto_metat
    belief_compliance_union ~ confianza_caleta + conflicto_caleta
    average_compliance_ini ~ c1*belief_compliance_pm + c2*belief_compliance_union + confianza_metat + conflicto_metat + confianza_caleta + conflicto_caleta + c3*average_compliance_observed_ini_lag
    
    # Intercepts
    belief_compliance_pm ~ 1
    belief_compliance_union ~ 1
    average_compliance_ini ~ 1

    # --- PARAMETER RECOVERY ---
    gamma    := c1 + c2 + c3
    lambda_1 := c1 / (c1 + c2 + c3)
    lambda_2 := c2 / (c1 + c2 + c3)
    lambda_3 := c3 / (c1 + c2 + c3)
    lambda_4 := (c1 + c2) / (c1 + c2 + c3)
  '
  
  fit <- sem(sem_model, data = data, estimator = "ML", se = "bootstrap", bootstrap = 1000, parallel  = "multicore", ncpus = 4)
  return(fit)
}

# --- 3. Execute Models for Specific Time Periods ---
set.seed(389)

# Define the requested time aggregations and individual rounds
time_periods <- list(
  list(start = 2, end = 8, label = "Mean rounds [2,8]"),
  list(start = 5, end = 8, label = "Mean rounds [5,8]"),
  list(start = 6, end = 8, label = "Mean rounds [6,8]"),
  list(start = 7, end = 8, label = "Mean rounds [7,8]"),
  list(start = 2, end = 2, label = "Round 2"),
  list(start = 3, end = 3, label = "Round 3"),
  list(start = 4, end = 4, label = "Round 4"),
  list(start = 5, end = 5, label = "Round 5"),
  list(start = 6, end = 6, label = "Round 6"),
  list(start = 7, end = 7, label = "Round 7"),
  list(start = 8, end = 8, label = "Round 8")
)

sem_results <- data.frame()

for (tp in time_periods) {
  # Run T1
  fit_T1 <- coef_SA_T1_sem(df, tp$start, tp$end)
  pe_T1  <- parameterEstimates(fit_T1) %>%
    filter(op == ":=", lhs %in% c("lambda_1", "lambda_2", "lambda_3", "lambda_4", "gamma")) %>%
    mutate(Time = tp$label, Stage = "Stage 1")
  
  # Run T2
  fit_T2 <- coef_SA_T2_sem(df, tp$start, tp$end)
  pe_T2  <- parameterEstimates(fit_T2) %>%
    filter(op == ":=", lhs %in% c("lambda_1", "lambda_2", "lambda_3", "lambda_4", "gamma")) %>%
    mutate(Time = tp$label, Stage = "Stage 2")
  
  sem_results <- bind_rows(sem_results, pe_T1, pe_T2)
}

# Label significance commonly
sem_results <- sem_results %>%
  mutate(Significance = ifelse(pvalue < 0.05, "p < 0.05", "p >= 0.05"))

# Split the data up for the respective plots
lambda_123_results <- sem_results %>% filter(lhs %in% c("lambda_1", "lambda_2", "lambda_3"))
gamma_results      <- sem_results %>% filter(lhs == "gamma")
lambda_34_results  <- sem_results %>% filter(lhs %in% c("lambda_3", "lambda_4"))

# ==============================================================================
# --- 4. Prepare and Plot Lambda 1, 2, 3 Data ---
# ==============================================================================

lambda_labels <- c(
  "lambda_2" = "λ₁ Weight Beliefs In-group",
  "lambda_1" = "λ₂ Weight Beliefs Out-group",
  "lambda_3" = "λ₃ Weight Observed Comp."
)

lambda_123_results <- lambda_123_results %>%
  mutate(
    Predictor = factor(lambda_labels[lhs], levels = unname(lambda_labels)),
    Time = factor(Time, levels = rev(sapply(time_periods, `[[`, "label")))
  )

# Compute mean of coefficients for each lambda to plot dotted vertical lines
lambda_means <- lambda_123_results %>%
  group_by(Stage, Predictor) %>%
  summarize(mean_est = mean(est), .groups = 'drop')

# Styling shapes (Colors are handled natively by scale_color_viridis_d)
lambda_shapes <- c(
  "λ₁ Weight Beliefs In-group"  = 16, # Circle
  "λ₂ Weight Beliefs Out-group" = 15, # Square
  "λ₃ Weight Observed Comp."    = 17  # Triangle
)

# --- 5. Generate Individual Lambda Plots ---
pd <- position_dodge(width = 0.4)

plot_stage_lambda <- function(data_stage, means_stage, title_text) {
  ggplot(data_stage, aes(x = est, y = Time, group = Predictor)) +
    geom_vline(data = means_stage, aes(xintercept = mean_est, color = Predictor), 
               linetype = "dotted", size = 1, alpha = 0.7) +
    geom_vline(xintercept = 0, linetype = "solid", color = "gray80", size = 0.5) +
    
    geom_errorbarh(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se, color = Predictor, 
                       alpha = Significance, size = Significance), height = 0.0, position = pd) +
    geom_point(aes(color = Predictor, shape = Predictor, alpha = Significance), position = pd, size = 6) +
    
    geom_text(data = subset(data_stage, Significance == "p < 0.05"),
              aes(label = round(est, 2)), position = pd, vjust = -1.2, size = 10, show.legend = FALSE) +
    
    coord_cartesian(xlim = c(-0.2, 1.0)) + 
    scale_color_viridis_d(name = "λ Parameters", end = 0.85) + 
    scale_shape_manual(values = lambda_shapes, name = "λ Parameters") +
    scale_alpha_manual(values = c("p < 0.05" = 1, "p >= 0.05" = 0.3), guide = "none") +
    scale_size_manual(values = c("p < 0.05" = 2.5, "p >= 0.05" = 0.8), guide = "none") +
    
    labs(title = title_text, x = "λ Proportional Weight (95% CI)", y = NULL) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 36, face = "bold"), 
      legend.position = "none",
      axis.text.x = element_text(size = 32),        # Kept x-axis label text at 24
      axis.text.y = element_text(size = 32),        # Changed y-axis label text to 40
      axis.title = element_text(size = 36),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )
}
# Create Lambda Plots
p1 <- plot_stage_lambda(lambda_123_results %>% filter(Stage == "Stage 1"), 
                        lambda_means %>% filter(Stage == "Stage 1"), 
                        " Shared Area (Stage 1)") +
  theme(axis.title.y = element_text(size = 36, margin = margin(r = 15))) + 
  labs(y = "Time Period Aggregation")

p2 <- plot_stage_lambda(lambda_123_results %>% filter(Stage == "Stage 2"), 
                        lambda_means %>% filter(Stage == "Stage 2"), 
                        "Shared Area (Stage 2)")

# --- 6. Final Assembly and Legend (Lambdas) ---
legend_plot <- ggplot(data.frame(Predictor = factor(names(lambda_shapes), levels = names(lambda_shapes)), est = 0, se = 1), 
                      aes(x = est, y = Predictor, color = Predictor, shape = Predictor)) +
  geom_point(size = 6) +
  geom_errorbarh(aes(xmin = est - se, xmax = est + se), size = 2.5) +
  scale_color_viridis_d(name = "Parameter", end = 0.85) + 
  scale_shape_manual(values = lambda_shapes, name = "Parameter") +
  theme_void() +
  theme(legend.position = "top", legend.title = element_text(size = 36, face="bold"), 
        legend.text = element_text(size = 36), legend.key.size = unit(4, "lines"))

final_lambda_plot <- (p1 | p2) / get_legend(legend_plot) + 
  plot_layout(heights = c(0.85, 0.15)) +
  plot_annotation(
    #title = 'λ Parameter Recovery over Time Aggregations',
    theme = theme(plot.title = element_text(size = 36, face = "bold", hjust = 0.5, margin = margin(b=30)))
  )

ggsave(paste0(path_github, "Outputs/lambda_coef_plot.jpg"), final_lambda_plot, width = 28, height = 30, dpi = 300)
message("Success! Lambda plot saved to: ", paste0(path_github, "Outputs/lambda_coef_plot.jpg"))


# ==============================================================================
# --- 7. Generate Gamma Plots (Horizontal) ---
# ==============================================================================

# Filter strictly for the individual rounds 2-8
gamma_rounds <- gamma_results %>%
  filter(grepl("Round", Time)) %>%
  mutate(Time = factor(Time, levels = paste("Round", 2:8)))

plot_gamma_stage <- function(data_stage, title_text) {
  ggplot(data_stage, aes(x = Time, y = est, group = 1)) +
    geom_hline(yintercept = 0, linetype = "solid", color = "gray80", size = 0.5) +
    geom_line(color = "#21908CFF", size = 1.2, alpha = 0.6) + # Viridis Teal Color
    geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se, alpha = Significance), 
                  width = 0.2, color = "#21908CFF", size = 1.2) +
    geom_point(aes(alpha = Significance), color = "#21908CFF", size = 6) +
    
    geom_text(data = subset(data_stage, Significance == "p < 0.05"),
              aes(label = round(est, 2)), vjust = -1.5, size = 8, show.legend = FALSE) +
    
    scale_alpha_manual(values = c("p < 0.05" = 1, "p >= 0.05" = 0.3), guide = "none") +
    scale_x_discrete(drop = FALSE) + # Ensures all rounds 2-8 explicitly appear on the x-axis
    
    labs(title = title_text, x = "Time Period (Rounds)", y = "γ Magnitude (95% CI)") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 30, face = "bold"), 
      axis.text = element_text(size = 24),
      axis.title = element_text(size = 26),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )
}

# Create Gamma Plots
p_gamma_T1 <- plot_gamma_stage(gamma_rounds %>% filter(Stage == "Stage 1"), "Shared Area (Stage 1)")
p_gamma_T2 <- plot_gamma_stage(gamma_rounds %>% filter(Stage == "Stage 2"), "Shared Area (Stage 2)")

# Combine using patchwork side-by-side
final_gamma_plot <- (p_gamma_T1 | p_gamma_T2) + 
  plot_layout(widths = c(1, 1)) +
  plot_annotation(
    title = 'Total Conditional Cooperation (γ) over Rounds 2-8',
    theme = theme(plot.title = element_text(size = 40, face = "bold", hjust = 0.5, margin = margin(b=30)))
  )

# Save Gamma Plot
ggsave(
  paste0(path_github, "Outputs/gamma_coef_plot.jpg"), 
  final_gamma_plot, 
  width = 28,
  height = 12, 
  dpi = 300
)

message("Success! Gamma plot saved to: ", paste0(path_github, "Outputs/gamma_coef_plot.jpg"))


# ==============================================================================
# --- 8. Generate Lambda 3 vs Lambda 4 Plot (Horizontal) ---
# ==============================================================================

l34_labels <- c(
  "lambda_3" = "λ₃ Weight Observed Comp.",
  "lambda_4" = "λ₄ Sum of Prior Beliefs"
)

# Filter strictly for the individual rounds 2-8
lambda_34_rounds <- lambda_34_results %>%
  filter(grepl("Round", Time)) %>%
  mutate(
    Time = factor(Time, levels = paste("Round", 2:8)),
    Predictor = factor(l34_labels[lhs], levels = unname(l34_labels))
  )

l34_shapes <- c(
  "λ₃ Weight Observed Comp." = 17, # Triangle
  "λ₄ Sum of Prior Beliefs"  = 18  # Diamond
)

pd_horiz <- position_dodge(width = 0.3)

plot_l34_stage <- function(data_stage, title_text) {
  ggplot(data_stage, aes(x = Time, y = est, group = Predictor, color = Predictor)) +
    geom_hline(yintercept = 0, linetype = "solid", color = "gray80", size = 0.5) +
    
    # Connecting Lines
    geom_line(position = pd_horiz, size = 1.2, alpha = 0.6) + 
    
    # Error bars and points
    geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se, alpha = Significance), 
                  position = pd_horiz, width = 0.2, size = 1.2) +
    geom_point(aes(shape = Predictor, alpha = Significance), position = pd_horiz, size = 6) +
    
    # Aesthetics scaling
    scale_alpha_manual(values = c("p < 0.05" = 1, "p >= 0.05" = 0.3), guide = "none") +
    scale_shape_manual(values = l34_shapes, name = "Parameter") +
    scale_color_viridis_d(name = "Parameter", end = 0.6) + # Adjusted end so colors are distinguishable
    scale_x_discrete(drop = FALSE) + 
    coord_cartesian(ylim = c(-0.2, 1.3)) + # Force the y-axis to be uniform across both stages
    
    # Labels and theme
    labs(title = title_text, x = "Time Period (Rounds)", y = "Proportional Weight (95% CI)") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 30, face = "bold"), 
      legend.position = "none",
      axis.text = element_text(size = 24),
      axis.title = element_text(size = 26),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )
}

# Create Lambda 3 & 4 Plots
p_l34_T1 <- plot_l34_stage(lambda_34_rounds %>% filter(Stage == "Stage 1"), "Shared Area (Stage 1)")
p_l34_T2 <- plot_l34_stage(lambda_34_rounds %>% filter(Stage == "Stage 2"), "Shared Area (Stage 2)")

# Create standalone legend for Lambda 3 & 4
legend_plot_34 <- ggplot(data.frame(Predictor = factor(names(l34_shapes), levels = names(l34_shapes)), est = 0, se = 1), 
                         aes(x = est, y = Predictor, color = Predictor, shape = Predictor)) +
  geom_point(size = 6) +
  geom_errorbarh(aes(xmin = est - se, xmax = est + se), size = 2.5) +
  scale_color_viridis_d(name = "Parameter", end = 0.6) + 
  scale_shape_manual(values = l34_shapes, name = "Parameter") +
  theme_void() +
  theme(legend.position = "top", legend.title = element_text(size = 28, face="bold"), 
        legend.text = element_text(size = 26), legend.key.size = unit(3, "lines"))

# Combine using patchwork
final_l34_plot <- (p_l34_T1 | p_l34_T2) / get_legend(legend_plot_34) + 
  plot_layout(heights = c(0.9, 0.1)) +
  plot_annotation(
    title = 'Observed Compliance vs. Sum of Prior Beliefs over Rounds 2-8',
    theme = theme(plot.title = element_text(size = 35, face = "bold", hjust = 0.5, margin = margin(b=30)))
  )

# Save Lambda 3 & 4 Plot
ggsave(
  paste0(path_github, "Outputs/lambda34_coef_plot.jpg"), 
  final_l34_plot, 
  width = 28,
  height = 14, 
  dpi = 300
)

message("Success! Lambda 3 & 4 plot saved to: ", paste0(path_github, "Outputs/lambda34_coef_plot.jpg"))