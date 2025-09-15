library(lavaan)
library(semPlot)
library(ggplot2)
library(viridis)
library(patchwork) # For combining plots
library(RColorBrewer) # For "autumn" color palette

rm(list = ls())

# --- 1. Setup: Paths, Data, Colors, and Shapes ---
# Ensure these paths are correct for your system
path_github <- "C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos  <- "C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

# Load data
load(paste0(path_datos, "/Datos_islitas_recode.Rdata")) # expects object 'df'
load(paste0(path_datos, "/Datos_islitas_long.Rdata"))   # expects object 'df_long'

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

# Define a consistent set of labels, colors, and shapes for all plots
all_predictor_labels <- c(
  "Trust Insiders", "Conflict Insiders",
  "Trust Outsiders", "Conflict Outsiders",
  "Prior Beliefs Insiders", "Prior Beliefs Outsiders",
  "Updated Beliefs"
)
# MODIFICATION: Changed to a high-contrast palette from RColorBrewer
high_contrast_palette <- brewer.pal(n = 8, name = "Paired")[c(1:4, 6:8)] # Select 7 distinct colors
predictor_colors <- setNames(
  high_contrast_palette,
  all_predictor_labels
)

# MODIFICATION: Added shape palette
predictor_shapes <- setNames(
  c(16, 17, 15, 18, 4, 3, 8), # A set of distinct shapes
  all_predictor_labels
)

#############################
# Figures for Stage unknown outsiders
###########################
# --- 2. Iterative Analysis for Compliance in TURF ---

################################################
#### TURF Rounds 2-8
################################################
set.seed(53698)

# SEM fit function for the full TURF model
coef_TURF_T1_sem <- function(data, R, N) {
  # Compute averages and compliance for the given window (R to N)
  cols_t1 <- declare_get_columns("T1juegoalgas", "T1_extraccion_amerb", R, N, data)
  subset_ini <- data[, cols_t1, drop = FALSE]
  data$average_extraction_ini  <- rowMeans(subset_ini, na.rm = TRUE)
  data$average_compliance_ini  <- 1 - data$average_extraction_ini / 50
  data$belief_compliance_amerb <- 1 - data$beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini / 50
  
  # Lagged observed compliance from the previous round (R-1)
  if (R > 1) {
    cols_obs <- declare_get_columns("T1juegoalgas", "T1_extraccion_otros_amerb", R - 1, R - 1, data)
    subset_obs <- data[, cols_obs, drop = FALSE]
    data$average_extraction_observed_ini      <- rowMeans(subset_obs, na.rm = TRUE)
    data$average_compliance_observed_ini_lag <- 1 - data$average_extraction_observed_ini / (50 * length(cols_obs))
  } else {
    data$average_compliance_observed_ini_lag <- NA
  }
  
  # SEM model specification for TURF
  sem_model <- ' 
    # Belief formation
    belief_compliance_amerb ~ survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
    
    # Compliance
    average_compliance_ini    ~ belief_compliance_amerb + survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta + average_compliance_observed_ini_lag
    
    # Covariance constraint
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_amerb
  '
  
  # Fit SEM with bootstrap standard errors
  fit <- sem(sem_model,
             data      = data,
             estimator = "ML",
             se        = "bootstrap",
             bootstrap = 200,
             parallel  = "multicore",
             ncpus     = 4)
  return(fit)
}

# Loop over rounds 2:8, collect ALL coefficients
round_vals <- 2:8
N <- 8
sem_results_TURF <- data.frame(
  round  = integer(),
  lhs    = character(),
  rhs    = character(),
  est    = numeric(),
  se     = numeric(),
  pvalue = numeric(),
  stringsAsFactors = FALSE
)

for (R in round_vals) {
  fit <- coef_TURF_T1_sem(df, R, N)
  pe  <- parameterEstimates(fit)
  all_coefs <- subset(pe, op == "~",
                      select = c("lhs", "rhs", "est", "se", "pvalue"))
  if (nrow(all_coefs) > 0) {
    all_coefs$round <- R
    all_coefs$endR <- N
    sem_results_TURF <- rbind(sem_results_TURF, all_coefs)
  }
}

sem_results_TURF$label <- paste0("Mean rounds ", sem_results_TURF$round, "-", sem_results_TURF$endR)

# Isolate results for the compliance equation
compliance_results_TURF <- subset(sem_results_TURF, lhs == "average_compliance_ini")

# MODIFICATION: Define the desired order for the y-axis and apply it
# ggplot builds the y-axis from the bottom up, so reversing the round order places 2-8 at the top.
label_order <- paste0("Mean rounds ", rev(round_vals), "-", N)
compliance_results_TURF$label <- factor(compliance_results_TURF$label, levels = label_order)

# MODIFICATION: Add a significance indicator for fading
compliance_results_TURF$Significance <- ifelse(compliance_results_TURF$pvalue < 0.05, "p < 0.05", "p >= 0.05")


# Create a factor for predictors with consistent labels
turf_compliance_labels <- c(
  "belief_compliance_amerb"             = "Prior Beliefs Insiders",
  "survey1.1.player.conflicto_caleta"   = "Conflict Insiders",
  "survey1.1.player.confianza_caleta"   = "Trust Insiders",
  "average_compliance_observed_ini_lag" = "Updated Beliefs"
)
compliance_results_TURF$Predictor <- factor(
  turf_compliance_labels[compliance_results_TURF$rhs],
  levels = all_predictor_labels
)

# Create the Compliance Coefficient Plot for TURF
p_compliance_TURF <- ggplot(compliance_results_TURF,
                            aes(x = est, y = label, color = Predictor, shape = Predictor, alpha = Significance)) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.6) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5) +
  geom_errorbarh(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se),
                 position = position_dodge(width = 0.5),
                 height = 0.2, size = 0.8) +
  scale_x_continuous(limits = c(-0.4, 0.7)) +
  scale_color_manual(values = predictor_colors, name = "Predictor", drop = FALSE) +
  scale_shape_manual(values = predictor_shapes, name = "Predictor", drop = FALSE) +
  scale_alpha_manual(values = c("p < 0.05" = 1.0, "p >= 0.05" = 0.4), guide = "none") +
  labs(title = "Compliance in TURF",
       y     = "Averaging Window",
       x     = "Coefficient (95% CI)") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 15))


# --- 3. Single Analysis for Belief Formation in TURF ---

sem_model_beliefs_TURF <- '
  belief_compliance_amerb ~ survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
'
df$belief_compliance_amerb <- 1 - df$beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini / 50
parallel_type <- if (.Platform$OS.type == "windows") "snow" else "multicore"
fit_bel_TURF <- sem(sem_model_beliefs_TURF, data = df, estimator = "ML", se = "bootstrap", bootstrap = 200, parallel = parallel_type, ncpus = 4)
pe_bel_TURF <- parameterEstimates(fit_bel_TURF)
bel_TURF <- subset(pe_bel_TURF, op == "~", select = c("lhs", "rhs", "est", "se", "pvalue"))

pred_labs_TURF <- c("survey1.1.player.confianza_caleta" = "Trust Insiders", "survey1.1.player.conflicto_caleta" = "Conflict Insiders")
bel_TURF$Predictor <- factor(pred_labs_TURF[bel_TURF$rhs], levels = all_predictor_labels)
bel_TURF$Outcome <- "Beliefs Insiders in TURF"



# --- 4. Iterative Analysis for Compliance in Shared Area ---

################################################
set.seed(478)

coef_SA_T1_sem <- function(data, R, N) {
  cols <- declare_get_columns("T1juegoalgas", "T1_extraccion_libre", R, N, data)
  subset_ini <- data[, cols, drop = FALSE]
  data$average_extraction_ini <- rowMeans(subset_ini, na.rm = TRUE)
  data$average_compliance_ini <- 1 - data$average_extraction_ini / 50
  data$belief_compliance_pm   <- 1 - data$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50
  data$belief_compliance_union<- 1 - data$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50
  
  if (R > 1) {
    cols_obs <- declare_get_columns("T1juegoalgas", "T1_extraccion_otros_libre", R - 1, R - 1, data)
    subset_obs <- data[, cols_obs, drop = FALSE]
    data$average_extraction_observed_ini      <- rowMeans(subset_obs, na.rm = TRUE)
    data$average_compliance_observed_ini_lag <- 1 - data$average_extraction_observed_ini / (50 * length(cols_obs))
  } else {
    data$average_compliance_observed_ini_lag <- NA
  }
  
  sem_model <- '
    belief_compliance_pm ~ survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm
    belief_compliance_union ~ survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
    average_compliance_ini ~ belief_compliance_pm + belief_compliance_union + survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm + survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta + average_compliance_observed_ini_lag
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_union
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_pm
  '
  
  fit <- sem(sem_model, data = data, estimator = "ML", se = "bootstrap", bootstrap = 200, parallel = "multicore", ncpus = 4)
  return(fit)
}

sem_results_SA <- data.frame(round=integer(), lhs=character(), rhs=character(), est=numeric(), se=numeric(), pvalue=numeric(), stringsAsFactors=FALSE)
for (R in round_vals) {
  fit <- coef_SA_T1_sem(df, R, N)
  pe  <- parameterEstimates(fit)
  all_coefs <- subset(pe, op == "~", select = c("lhs","rhs","est","se","pvalue"))
  if (nrow(all_coefs) > 0) {
    all_coefs$round <- R
    all_coefs$endR <- N
    sem_results_SA <- rbind(sem_results_SA, all_coefs)
  }
}
sem_results_SA$label <- paste0("Mean rounds ", sem_results_SA$round, "-", sem_results_SA$endR)
compliance_results_SA <- subset(sem_results_SA, lhs == "average_compliance_ini")

# MODIFICATION: Apply the same y-axis order to this plot as well
compliance_results_SA$label <- factor(compliance_results_SA$label, levels = label_order)

# MODIFICATION: Add a significance indicator for fading
compliance_results_SA$Significance <- ifelse(compliance_results_SA$pvalue < 0.05, "p < 0.05", "p >= 0.05")


sa_compliance_labels <- c(
  "belief_compliance_pm"                = "Prior Beliefs Outsiders",
  "belief_compliance_union"             = "Prior Beliefs Insiders",
  "survey1.1.player.conflicto_pm"       = "Conflict Outsiders",
  "survey1.1.player.conflicto_caleta"   = "Conflict Insiders",
  "survey1.1.player.confianza_pm"       = "Trust Outsiders",
  "survey1.1.player.confianza_caleta"   = "Trust Insiders",
  "average_compliance_observed_ini_lag" = "Updated Beliefs"
)
compliance_results_SA$Predictor <- factor(
  sa_compliance_labels[compliance_results_SA$rhs],
  levels = all_predictor_labels
)

p_compliance_SA <- ggplot(compliance_results_SA, aes(x = est, y = label, color = Predictor, shape = Predictor, alpha = Significance)) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.6) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5) +
  geom_errorbarh(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se),
                 position = position_dodge(width = 0.5), height = 0.2, size = 0.8) +
  scale_x_continuous(limits = c(-0.4, 0.7)) +
  scale_color_manual(values = predictor_colors, name = "Predictor", drop = FALSE) +
  scale_shape_manual(values = predictor_shapes, name = "Predictor", drop = FALSE) +
  scale_alpha_manual(values = c("p < 0.05" = 1.0, "p >= 0.05" = 0.4), guide = "none") +
  labs(title = "Compliance in Shared Area", y = "Averaging Window", x = "Coefficient (95% CI)") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 15))


# --- 5. Single Analysis for Belief Formation in Shared Area ---

sem_model_beliefs_SA <- '
  belief_compliance_pm    ~ survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm
  belief_compliance_union ~ survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
'
df$belief_compliance_pm   <- 1 - df$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50
df$belief_compliance_union <- 1 - df$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50
fit_bel_SA <- sem(sem_model_beliefs_SA, data = df, estimator = "ML", se = "bootstrap", bootstrap = 200, parallel = parallel_type, ncpus = 4)
pe_bel_SA <- parameterEstimates(fit_bel_SA)
bel_SA <- subset(pe_bel_SA, op == "~", select = c("lhs", "rhs", "est", "se", "pvalue"))

pred_labs_SA <- c("survey1.1.player.confianza_pm" = "Trust Outsiders", "survey1.1.player.conflicto_pm" = "Conflict Outsiders",
                  "survey1.1.player.confianza_caleta" = "Trust Insiders", "survey1.1.player.conflicto_caleta" = "Conflict Insiders")
bel_SA$Predictor <- factor(pred_labs_SA[bel_SA$rhs], levels = all_predictor_labels)
bel_SA$Outcome <- factor(c("belief_compliance_pm" = "Beliefs Outsiders in Shared Area", 
                           "belief_compliance_union" = "Beliefs Insiders in Shared Area")[bel_SA$lhs])


# --- 6. Combined Belief Formation Plot ---

all_beliefs <- rbind(bel_TURF, bel_SA)

# MODIFICATION: Set the desired order for the y-axis in the beliefs plot
# ggplot builds the y-axis from the bottom up, so we reverse the desired visual order.
outcome_order <- c("Beliefs Outsiders in Shared Area", 
                   "Beliefs Insiders in Shared Area",
                   "Beliefs Insiders in TURF")
all_beliefs$Outcome <- factor(all_beliefs$Outcome, levels = outcome_order)

# MODIFICATION: Add a significance indicator for fading
all_beliefs$Significance <- ifelse(all_beliefs$pvalue < 0.05, "p < 0.05", "p >= 0.05")


p_beliefs_combined <- ggplot(all_beliefs, aes(x = est, y = Outcome, color = Predictor, shape = Predictor, alpha = Significance)) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.6) +
  geom_point(position = position_dodge(width = 0.6), size = 2.5) +
  geom_errorbarh(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se),
                 position = position_dodge(width = 0.6), height = 0.3, size = 0.8) +
  scale_color_manual(values = predictor_colors, name = "Predictor", drop = FALSE) +
  scale_shape_manual(values = predictor_shapes, name = "Predictor", drop = FALSE) +
  scale_alpha_manual(values = c("p < 0.05" = 1.0, "p >= 0.05" = 0.4), guide = "none") +
  scale_x_continuous(limits = c(-0.4, 0.7)) +
  labs(title = "Belief Formation Across Scenarios", y = NULL, x = "Coefficient (95% CI)") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 15))

# --- 7. Combine All Plots into Final Figure ---

# Create a standalone legend plot
legend_plot <- ggplot(data.frame(Predictor = factor(all_predictor_labels, levels = all_predictor_labels)),
                      aes(x = 1, y = Predictor, color = Predictor, shape = Predictor)) +
  geom_point(size = 3) +
  scale_color_manual(values = predictor_colors, name = "Predictor", drop = FALSE) +
  scale_shape_manual(values = predictor_shapes, name = "Predictor", drop = FALSE) +
  theme_minimal() +
  guides(color = guide_legend(nrow=2), shape = guide_legend(nrow=2)) + # Arrange legend in 2 rows
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 15),
        legend.text = element_text(size = 14))

# Extract the legend graphical object
get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
shared_legend <- get_legend(legend_plot)

# Remove legends from individual plots
p_beliefs_combined <- p_beliefs_combined + theme(legend.position = "none")
p_compliance_TURF <- p_compliance_TURF + theme(legend.position = "none")
p_compliance_SA <- p_compliance_SA + theme(legend.position = "none")

# Arrange the three plots in a row
plots_row <- p_beliefs_combined + p_compliance_TURF + p_compliance_SA +
  plot_layout(widths = c(1, 1, 1))

# Combine the row of plots with the shared legend underneath
final_combined_plot <- plots_row / shared_legend +
  plot_layout(heights = c(1, 0.15)) + # Give a bit more space for two-row legend
  plot_annotation(
    title = 'SEM Results for TURF and Shared Area - Unknown Outsiders',
    caption = 'Left: Combined belief formation models (round 0). Center & Right: Compliance models iterated over multiple rounds.',
    theme = theme(plot.title = element_text(size = 22, face = "bold"),
                  plot.caption = element_text(size = 14))
  )

# Print and save the final combined image
print(final_combined_plot)
ggsave(file = paste0(path_github, "Outputs/Combined_All_SEM_Results_SideBySide_Full.pdf"),
       plot = final_combined_plot, device = "pdf", width = 24, height = 10)




########################
### Stage 2 rounds 9-16
########################

