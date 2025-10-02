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

############################################################
# Stage 1: Shared Area - Unknown Outsiders (T1)
############################################################
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

# --- Dynamic SEM for T1 ---
round_vals <- 2:8
N <- 8
sem_results_SA_T1 <- data.frame(round=integer(), lhs=character(), rhs=character(), est=numeric(), se=numeric(), pvalue=numeric(), stringsAsFactors=FALSE)
for (R in round_vals) {
  fit <- coef_SA_T1_sem(df, R, N)
  pe  <- parameterEstimates(fit)
  all_coefs <- subset(pe, op == "~", select = c("lhs","rhs","est","se","pvalue"))
  if (nrow(all_coefs) > 0) {
    all_coefs$round <- R
    all_coefs$endR <- N
    sem_results_SA_T1 <- rbind(sem_results_SA_T1, all_coefs)
  }
}

sem_results_SA_T1$label <- paste0("Mean rounds ", sem_results_SA_T1$round, "-", sem_results_SA_T1$endR)
compliance_results_SA <- subset(sem_results_SA_T1, lhs == "average_compliance_ini")

# Standardize T1 results for plotting
label_order <- paste0("Mean rounds ", round_vals, "-", N)
compliance_results_SA$label <- factor(compliance_results_SA$label, levels = label_order)
compliance_results_SA$Significance <- ifelse(compliance_results_SA$pvalue < 0.05, "p < 0.05", "p >= 0.05")

sa_T1_compliance_labels <- c(
  "belief_compliance_pm"                = "Prior Beliefs Outsiders",
  "belief_compliance_union"             = "Prior Beliefs Insiders",
  "survey1.1.player.conflicto_pm"       = "Conflict Outsiders",
  "survey1.1.player.conflicto_caleta"   = "Conflict Insiders",
  "survey1.1.player.confianza_pm"       = "Trust Outsiders",
  "survey1.1.player.confianza_caleta"   = "Trust Insiders",
  "average_compliance_observed_ini_lag" = "Updated Beliefs"
)
compliance_results_SA$Predictor <- factor(
  sa_T1_compliance_labels[compliance_results_SA$rhs],
  levels = all_predictor_labels
)

# --- Belief Formation SEM for T1 (Static) ---
sem_model_beliefs_SA_T1 <- '
  belief_compliance_pm    ~ survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm
  belief_compliance_union ~ survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
'
df$belief_compliance_pm   <- 1 - df$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50
df$belief_compliance_union <- 1 - df$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50
fit_bel_SA_T1 <- sem(sem_model_beliefs_SA_T1, data = df, estimator = "ML", se = "bootstrap", bootstrap = 200, parallel = "multicore", ncpus = 4)
pe_bel_SA_T1 <- parameterEstimates(fit_bel_SA_T1)
bel_SA_T1 <- subset(pe_bel_SA_T1, op == "~", select = c("lhs", "rhs", "est", "se", "pvalue"))

pred_labs_SA_T1 <- c("survey1.1.player.confianza_pm" = "Trust Outsiders", "survey1.1.player.conflicto_pm" = "Conflict Outsiders",
                     "survey1.1.player.confianza_caleta" = "Trust Insiders", "survey1.1.player.conflicto_caleta" = "Conflict Insiders")
bel_SA_T1$Predictor <- factor(pred_labs_SA_T1[bel_SA_T1$rhs], levels = all_predictor_labels)
bel_SA_T1$Outcome <- factor(c("belief_compliance_pm" = "Beliefs Outsiders (Unknown)", 
                              "belief_compliance_union" = "Beliefs Insiders (Unknown)")[bel_SA_T1$lhs])


############################################################
# Stage 2: Shared Area - Known Outsiders (T2)
############################################################
set.seed(4523)

coef_SA_T2_sem <- function(data, R, N) {
  # compute averages and compliances
  cols <- declare_get_columns("T2juegoalgas", "T2_extraccion_metat", R, N, data)
  subset_ini <- data[, cols, drop = FALSE]
  data$average_extraction_ini <- rowMeans(subset_ini, na.rm = TRUE)
  data$average_compliance_ini <- 1 - data$average_extraction_ini / 50
  data$belief_compliance_pm   <- 1 - data$beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini / 50
  data$belief_compliance_union <- 1 - (data$beliefsT2inicial.1.player.T2_belief_caleta_ini / 50)
  
  # lagged observed compliance
  if (R > 1) {
    cols_obs <- declare_get_columns("T2juegoalgas", "T2_extraccion_otros_metat", R - 1, R - 1, data)
    subset_obs <- data[, cols_obs, drop = FALSE]
    data$average_extraction_observed_ini     <- rowMeans(subset_obs, na.rm = TRUE)
    data$average_compliance_observed_ini_lag <- 1 - data$average_extraction_observed_ini / 50
  } else {
    data$average_compliance_observed_ini_lag <- NA
  }
  
  # SEM model specification
  sem_model <- '
    belief_compliance_pm ~ survey2.1.player.confianza_caleta_conocida_mean + survey2.1.player.conflicto_caleta_conocida_mean
    belief_compliance_union ~ survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
    average_compliance_ini ~ belief_compliance_pm + belief_compliance_union + survey2.1.player.confianza_caleta_conocida_mean + 
    survey2.1.player.conflicto_caleta_conocida_mean + survey1.1.player.confianza_caleta +
    survey1.1.player.conflicto_caleta + average_compliance_observed_ini_lag
    
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_union
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_pm
  '
  
  fit <- sem(sem_model, data = data, estimator = "ML", se = "bootstrap", bootstrap = 200, parallel  = "multicore", ncpus = 4)
  return(fit)
}

# --- Dynamic SEM for T2 ---
# Loop over rounds 1-8 for T2, using an expanding window
sem_results_SA_T2 <- data.frame(round=integer(), lhs=character(), rhs=character(), est=numeric(), se=numeric(), pvalue=numeric(), stringsAsFactors=FALSE)
for (R in round_vals) { # Using the same round_vals (2:8)
  fit <- coef_SA_T2_sem(df, R, N)
  pe  <- parameterEstimates(fit)
  # MODIFICATION: Get ALL coefficients, not just significant ones
  all_coefs <- subset(pe, op == "~", select = c("lhs","rhs","est","se","pvalue"))
  if (nrow(all_coefs) > 0) {
    all_coefs$round <- R
    all_coefs$endR <- N
    sem_results_SA_T2 <- rbind(sem_results_SA_T2, all_coefs)
  }
}

sem_results_SA_T2$label <- paste0("Mean rounds ", sem_results_SA_T2$round, "-", sem_results_SA_T2$endR)
compliance_results_SA_T2 <- subset(sem_results_SA_T2, lhs == "average_compliance_ini")

# --- Standardize T2 Results for Plotting ---
compliance_results_SA_T2$label <- factor(compliance_results_SA_T2$label, levels = label_order)
compliance_results_SA_T2$Significance <- ifelse(compliance_results_SA_T2$pvalue < 0.05, "p < 0.05", "p >= 0.05")

# Create standardized labels for the T2 predictors
sa_T2_compliance_labels <- c(
  "belief_compliance_pm"                          = "Prior Beliefs Outsiders",
  "belief_compliance_union"                       = "Prior Beliefs Insiders",
  "survey2.1.player.conflicto_caleta_conocida_mean" = "Conflict Outsiders",
  "survey1.1.player.conflicto_caleta"             = "Conflict Insiders",
  "survey2.1.player.confianza_caleta_conocida_mean" = "Trust Outsiders",
  "survey1.1.player.confianza_caleta"             = "Trust Insiders",
  "average_compliance_observed_ini_lag"           = "Updated Beliefs"
)
compliance_results_SA_T2$Predictor <- factor(
  sa_T2_compliance_labels[compliance_results_SA_T2$rhs],
  levels = all_predictor_labels
)

# --- Belief Formation SEM for T2 (Static) ---
df$belief_compliance_pm_T2    <- 1 - df$beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini / 50
df$belief_compliance_union_T2 <- 1 - df$beliefsT2inicial.1.player.T2_belief_caleta_ini / 50

sem_model_beliefs_SA_T2 <- '
  belief_compliance_pm_T2    ~ survey2.1.player.confianza_caleta_conocida_mean + survey2.1.player.conflicto_caleta_conocida_mean
  belief_compliance_union_T2 ~ survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
'

fit_bel_SA_T2 <- sem(sem_model_beliefs_SA_T2, data = df, estimator = "ML", se = "bootstrap", bootstrap = 200, parallel = "multicore", ncpus = 4)
pe_bel_SA_T2 <- parameterEstimates(fit_bel_SA_T2)
bel_SA_T2 <- subset(pe_bel_SA_T2, op == "~", select = c("lhs", "rhs", "est", "se", "pvalue"))

pred_labs_SA_T2 <- c(
  "survey2.1.player.confianza_caleta_conocida_mean" = "Trust Outsiders",
  "survey2.1.player.conflicto_caleta_conocida_mean" = "Conflict Outsiders",
  "survey1.1.player.confianza_caleta"               = "Trust Insiders",
  "survey1.1.player.conflicto_caleta"               = "Conflict Insiders"
)

bel_SA_T2$Predictor <- factor(pred_labs_SA_T2[bel_SA_T2$rhs], levels = all_predictor_labels)
bel_SA_T2$Outcome <- factor(c(
  "belief_compliance_pm_T2"    = "Beliefs Outsiders (Known)",
  "belief_compliance_union_T2" = "Beliefs Insiders (Known)"
)[bel_SA_T2$lhs])


# --- NEW SECTION: Generate and Save Individual Plots ---

# --- Plot 1: Combined Beliefs ---
# Combine belief results from T1 and T2
combined_beliefs <- rbind(bel_SA_T1, bel_SA_T2)
combined_beliefs$Significance <- ifelse(combined_beliefs$pvalue < 0.05, "p < 0.05", "p >= 0.05")

# FIX 1: Reorder the 'Outcome' factor to have "Unknown" on top
outcome_order <- c(
  "Beliefs Outsiders (Unknown)", "Beliefs Insiders (Unknown)",
  "Beliefs Outsiders (Known)", "Beliefs Insiders (Known)"
)
combined_beliefs$Outcome <- factor(combined_beliefs$Outcome, levels = rev(outcome_order))

p_beliefs <- ggplot(combined_beliefs, aes(x = est, y = Outcome, color = Predictor, shape = Predictor)) +
  # FIX 3: Add a horizontal line for visual separation
  geom_hline(yintercept = 2.5, linetype = "dashed", color = "gray70") +
  # FIX 2: Map 'alpha' to Significance for points and error bars
  geom_point(aes(alpha = Significance), position = position_dodge(width = 0.5), size = 4) +
  geom_errorbarh(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se, alpha = Significance), height = 0.2, position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = predictor_colors) +
  scale_shape_manual(values = predictor_shapes) +
  # Add alpha scale for fading
  scale_alpha_manual(values = c("p < 0.05" = 1, "p >= 0.05" = 0.4), guide = "none") + # Hide alpha legend
  labs(
    title = "Belief Formation by Group and Condition",
    x = "Coefficient (95% CI)",
    y = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(face="bold")
  )

ggsave(paste0(path_github, "Outputs/Beliefs_Plot.pdf"), p_beliefs, width = 8, height = 6)
print(p_beliefs)


# --- Plot 2: Dynamic Compliance T1 (Unknown) ---
# FIX: Define position_dodge object once to ensure alignment
pd <- position_dodge(width = 0.6)

p_compliance_T1 <- ggplot(compliance_results_SA, aes(x = est, y = label, group = Predictor)) +
  geom_point(aes(color = Predictor, shape = Predictor, alpha = Significance), position = pd, size = 3) +
  geom_errorbarh(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se, color = Predictor, alpha = Significance), height = 0, position = pd) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = predictor_colors) +
  scale_shape_manual(values = predictor_shapes) +
  scale_alpha_manual(values = c("p < 0.05" = 1, "p >= 0.05" = 0.4)) +
  scale_y_discrete(limits = rev(levels(compliance_results_SA$label))) + # Reverse order
  labs(
    title = "Compliance Determinants (Unknown Outsiders)",
    x = "Coefficient (95% CI)",
    y = "Averaging Window"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(paste0(path_github, "Outputs/Compliance_T1_Plot.pdf"), p_compliance_T1, width = 8, height = 6)
print(p_compliance_T1)


# --- Plot 3: Dynamic Compliance T2 (Known) ---
p_compliance_T2 <- ggplot(compliance_results_SA_T2, aes(x = est, y = label, group = Predictor)) +
  geom_point(aes(color = Predictor, shape = Predictor, alpha = Significance), position = pd, size = 3) +
  geom_errorbarh(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se, color = Predictor, alpha = Significance), height = 0, position = pd) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = predictor_colors) +
  scale_shape_manual(values = predictor_shapes) +
  scale_alpha_manual(values = c("p < 0.05" = 1, "p >= 0.05" = 0.4)) +
  scale_y_discrete(limits = rev(levels(compliance_results_SA_T2$label))) + # Reverse order
  labs(
    title = "Compliance Determinants (Known Outsiders)",
    x = "Coefficient (95% CI)",
    y = "Averaging Window"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(paste0(path_github, "Outputs/Compliance_T2_Plot.pdf"), p_compliance_T2, width = 8, height = 6)
print(p_compliance_T2)

