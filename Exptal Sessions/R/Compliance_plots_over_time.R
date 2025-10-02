library(lavaan)
library(semPlot)
library(ggplot2)
library(viridis)
library(patchwork) # For combining plots
library(RColorBrewer) # For "autumn" color palette
library(ggpubr) # For extracting legend

# install.packages("paletteer")
#library(paletteer)

# Use in a ggplot2 chart:
scale_colour_paletteer_d("ggthemes::Classic_Color_Blind")
scale_fill_paletteer_d("ggthemes::Classic_Color_Blind")

rm(list = ls())

# --- 1. Setup: Paths, Data, Colors, and Shapes ---
# Ensure these paths are correct for your system
path_github <- "C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos  <- "C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

# Load data
# Note: If these files are not found, the script will throw an error.
# You may need to update the path or comment these lines out to run.
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


############################################################
# Stage 1: Shared Area - Unknown Out-group (T1)
############################################################
set.seed(478)

coef_SA_T1_sem <- function(data, R_start, R_end) {
  cols <- declare_get_columns("T1juegoalgas", "T1_extraccion_libre", R_start, R_end, data)
  subset_ini <- data[, cols, drop = FALSE]
  data$average_extraction_ini <- rowMeans(subset_ini, na.rm = TRUE)
  data$average_compliance_ini <- 1 - data$average_extraction_ini / 50
  data$belief_compliance_pm   <- 1 - data$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50
  data$belief_compliance_union<- 1 - data$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50
  
  if (R_start > 1) {
    # For single rounds, lag is the previous round. For the mean, it's round 1.
    lag_round <- ifelse(R_start == R_end, R_start - 1, 1)
    cols_obs <- declare_get_columns("T1juegoalgas", "T1_extraccion_otros_libre", lag_round, lag_round, data)
    subset_obs <- data[, cols_obs, drop = FALSE]
    data$average_extraction_observed_ini      <- rowMeans(subset_obs, na.rm = TRUE)
    # The divisor logic needs to be robust to single/multiple columns
    data$average_compliance_observed_ini_lag <- 1 - data$average_extraction_observed_ini / (50 * (ifelse(is.null(ncol(subset_obs)), 1, ncol(subset_obs))))
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
  
  # MODIFICATION: Reduced bootstrap iterations
  fit <- sem(sem_model, data = data, estimator = "ML", se = "bootstrap", bootstrap = 100, parallel = "multicore", ncpus = 4)
  return(fit)
}

# --- Dynamic SEM for T1 ---
# MODIFICATION: New loop structure
sem_results_SA_T1 <- data.frame(label=character(), lhs=character(), rhs=character(), est=numeric(), se=numeric(), pvalue=numeric(), stringsAsFactors=FALSE)

# First, the mean of rounds 2-8
fit_mean <- coef_SA_T1_sem(df, 2, 8)
pe_mean  <- parameterEstimates(fit_mean)
all_coefs_mean <- subset(pe_mean, op == "~", select = c("lhs","rhs","est","se","pvalue"))
all_coefs_mean$label <- "Mean rounds 2-8"
sem_results_SA_T1 <- rbind(sem_results_SA_T1, all_coefs_mean)

# Then, each individual round from 2 to 8
for (R in 2:8) {
  fit <- coef_SA_T1_sem(df, R, R) # Start and end round are the same
  pe  <- parameterEstimates(fit)
  all_coefs <- subset(pe, op == "~", select = c("lhs","rhs","est","se","pvalue"))
  if (nrow(all_coefs) > 0) {
    all_coefs$label <- paste0("Round ", R)
    sem_results_SA_T1 <- rbind(sem_results_SA_T1, all_coefs)
  }
}

compliance_results_SA <- subset(sem_results_SA_T1, lhs == "average_compliance_ini")

# Standardize T1 results for plotting
label_order <- c("Mean rounds 2-8", paste0("Round ", 2:8))
compliance_results_SA$label <- factor(compliance_results_SA$label, levels = label_order)
compliance_results_SA$Significance <- ifelse(compliance_results_SA$pvalue < 0.05, "p < 0.05", "p >= 0.05")

sa_T1_compliance_labels <- c(
  "belief_compliance_pm"                = "Prior Beliefs Out-group",
  "belief_compliance_union"             = "Prior Beliefs In-group",
  "survey1.1.player.conflicto_pm"       = "Conflict Out-group",
  "survey1.1.player.conflicto_caleta"   = "Conflict In-group",
  "survey1.1.player.confianza_pm"       = "Trust Out-group",
  "survey1.1.player.confianza_caleta"   = "Trust In-group",
  "average_compliance_observed_ini_lag" = "Observed Compliance (t−1)"
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
# MODIFICATION: Reduced bootstrap iterations
fit_bel_SA_T1 <- sem(sem_model_beliefs_SA_T1, data = df, estimator = "ML", se = "bootstrap", bootstrap = 100, parallel = "multicore", ncpus = 4)
pe_bel_SA_T1 <- parameterEstimates(fit_bel_SA_T1)
bel_SA_T1 <- subset(pe_bel_SA_T1, op == "~", select = c("lhs", "rhs", "est", "se", "pvalue"))

# FIX 1: Use the full, correct mapping for belief predictors
bel_SA_T1$Predictor <- factor(sa_T1_compliance_labels[bel_SA_T1$rhs], levels = all_predictor_labels)
bel_SA_T1$Outcome <- factor(c("belief_compliance_pm" = "Beliefs Out-group (Unknown)", 
                              "belief_compliance_union" = "Beliefs In-group (Stage 1)")[bel_SA_T1$lhs])


############################################################
# Stage 2: Shared Area - Known Out-group (T2)
############################################################
set.seed(4523)

coef_SA_T2_sem <- function(data, R_start, R_end) {
  # compute averages and compliances
  cols <- declare_get_columns("T2juegoalgas", "T2_extraccion_metat", R_start, R_end, data)
  subset_ini <- data[, cols, drop = FALSE]
  data$average_extraction_ini <- rowMeans(subset_ini, na.rm = TRUE)
  data$average_compliance_ini <- 1 - data$average_extraction_ini / 50
  data$belief_compliance_pm   <- 1 - data$beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini / 50
  data$belief_compliance_union <- 1 - (data$beliefsT2inicial.1.player.T2_belief_caleta_ini / 50)
  
  # lagged observed compliance
  if (R_start > 1) {
    lag_round <- ifelse(R_start == R_end, R_start - 1, 1)
    cols_obs <- declare_get_columns("T2juegoalgas", "T2_extraccion_otros_metat", lag_round, lag_round, data)
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
  
  # MODIFICATION: Reduced bootstrap iterations
  fit <- sem(sem_model, data = data, estimator = "ML", se = "bootstrap", bootstrap = 100, parallel  = "multicore", ncpus = 4)
  return(fit)
}





# --- Dynamic SEM for T2 ---
# MODIFICATION: New loop structure
sem_results_SA_T2 <- data.frame(label=character(), lhs=character(), rhs=character(), est=numeric(), se=numeric(), pvalue=numeric(), stringsAsFactors=FALSE)

# First, the mean of rounds 2-8
fit_mean_T2 <- coef_SA_T2_sem(df, 2, 8)
pe_mean_T2  <- parameterEstimates(fit_mean_T2)
all_coefs_mean_T2 <- subset(pe_mean_T2, op == "~", select = c("lhs","rhs","est","se","pvalue"))
all_coefs_mean_T2$label <- "Mean rounds 2-8"
sem_results_SA_T2 <- rbind(sem_results_SA_T2, all_coefs_mean_T2)

# Then, each individual round from 2 to 8
for (R in 2:8) {
  fit <- coef_SA_T2_sem(df, R, R)
  pe  <- parameterEstimates(fit)
  all_coefs <- subset(pe, op == "~", select = c("lhs","rhs","est","se","pvalue"))
  if (nrow(all_coefs) > 0) {
    all_coefs$label <- paste0("Round ", R)
    sem_results_SA_T2 <- rbind(sem_results_SA_T2, all_coefs)
  }
}

compliance_results_SA_T2 <- subset(sem_results_SA_T2, lhs == "average_compliance_ini")

# --- Standardize T2 Results for Plotting ---
compliance_results_SA_T2$label <- factor(compliance_results_SA_T2$label, levels = label_order)
compliance_results_SA_T2$Significance <- ifelse(compliance_results_SA_T2$pvalue < 0.05, "p < 0.05", "p >= 0.05")

# Create standardized labels for the T2 predictors
sa_T2_compliance_labels <- c(
  "belief_compliance_pm"                          = "Prior Beliefs Out-group",
  "belief_compliance_union"                       = "Prior Beliefs In-group",
  "survey2.1.player.conflicto_caleta_conocida_mean" = "Conflict Out-group",
  "survey1.1.player.conflicto_caleta"             = "Conflict In-group",
  "survey2.1.player.confianza_caleta_conocida_mean" = "Trust Out-group",
  "survey1.1.player.confianza_caleta"             = "Trust In-group",
  "average_compliance_observed_ini_lag"           = "Observed Compliance (t−1)"
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

# MODIFICATION: Reduced bootstrap iterations
fit_bel_SA_T2 <- sem(sem_model_beliefs_SA_T2, data = df, estimator = "ML", se = "bootstrap", bootstrap = 100, parallel = "multicore", ncpus = 4)
pe_bel_SA_T2 <- parameterEstimates(fit_bel_SA_T2)
bel_SA_T2 <- subset(pe_bel_SA_T2, op == "~", select = c("lhs", "rhs", "est", "se", "pvalue"))

# FIX 1: Use the full, correct mapping for belief predictors
bel_SA_T2$Predictor <- factor(sa_T2_compliance_labels[bel_SA_T2$rhs], levels = all_predictor_labels)
bel_SA_T2$Outcome <- factor(c(
  "belief_compliance_pm_T2"    = "Beliefs Out-group (Known)",
  "belief_compliance_union_T2" = "Beliefs In-group (Stage 2)"
)[bel_SA_T2$lhs])

###################################################
# --- Prepare Individual Plots for Combination ---
#################################################### Define a consistent set of labels, colors, and shapes for all plots
all_predictor_labels <- c(
  "Trust In-group", "Conflict In-group",
  "Trust Out-group", "Conflict Out-group",
  "Prior Beliefs In-group", "Prior Beliefs Out-group",
  "Observed Compliance (t−1)"
)
# MODIFICATION: Changed to a high-contrast palette from RColorBrewer
#high_contrast_palette <- brewer.pal(n = 8, name = "Paired")[c(1:4, 6:8)] # Select 7 distinct colors
#high_contrast_palette<-c("#006BA4FF", "#FF800EFF", "#ABABABFF", "#595959FF", "#5F9ED1FF", "#C85200FF", "#898989FF","#A2C8ECFF") 

# install.packages("paletteer")
#library(paletteer)

# Use in a ggplot2 chart:
#scale_colour_paletteer_d("ggthemes::Classic_Cyclic")
#scale_fill_paletteer_d("ggthemes::Classic_Cyclic")

#high_contrast_palette<-c("#1F83B4FF", "#12A2A8FF", "#2CA030FF", "#78A641FF","#BCBD22FF", "#FFBF50FF", "#FFAA0EFF","#FF7F0EFF", "#D63A3AFF", "#C7519CFF","#BA43B4FF","#8A60B0FF","#6F63BBFF")
high_contrast_palette<-c("#1F83B4FF", "#12A2A8FF", "#2CA030FF", "#78A641FF","#BCBD22FF",  "#D63A3AFF","#8A60B0FF", "#FFAA0EFF")


predictor_colors <- setNames(
  high_contrast_palette,
  all_predictor_labels
)

# MODIFICATION: Added shape palette
predictor_shapes <- setNames(
  c(16, 17, 15, 18, 4, 3, 8), # A set of distinct shapes
  all_predictor_labels
)


# --- Plot A: Dynamic Compliance T1 (Unknown) ---
pd <- position_dodge(width = 0.6)
p_compliance_T1 <- ggplot(compliance_results_SA, aes(x = est, y = label, group = Predictor)) +
  # FIX 2: Add size mapping for bolder lines AND make height consistent
  geom_errorbarh(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se, color = Predictor, alpha = Significance, size = Significance), height = 0.0, position = pd) +
  geom_point(aes(color = Predictor, shape = Predictor, alpha = Significance), position = pd, size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  # FIX 3: Set consistent X-axis limits
  coord_cartesian(xlim = c(-0.3, 0.6)) +
  scale_color_manual(values = predictor_colors, name = "Predictor", drop = FALSE) +
  scale_shape_manual(values = predictor_shapes, name = "Predictor", drop = FALSE) +
  scale_alpha_manual(values = c("p < 0.05" = 1, "p >= 0.05" = 0.2), guide = "none") +
  # Add size scale for thicker lines
  scale_size_manual(values = c("p < 0.05" = 1, "p >= 0.05" = 0.5), guide = "none") +
  scale_y_discrete(limits = rev(levels(compliance_results_SA$label))) +
  labs(
    title = "A) Compliance in Shared Area (Unknown Out-group Stage)",
    x = NULL, # Remove individual x-axis titles
    y = "Time Period"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none")


# --- Plot B: Dynamic Compliance T2 (Known) ---
p_compliance_T2 <- ggplot(compliance_results_SA_T2, aes(x = est, y = label, group = Predictor)) +
  # FIX 2: Add size mapping for bolder lines AND make height consistent
  geom_errorbarh(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se, color = Predictor, alpha = Significance, size = Significance), height = 0.0, position = pd) +
  geom_point(aes(color = Predictor, shape = Predictor, alpha = Significance), position = pd, size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  # FIX 3: Set consistent X-axis limits
  coord_cartesian(xlim = c(-0.3, 0.6)) +
  scale_color_manual(values = predictor_colors, name = "Predictor", drop = FALSE) +
  scale_shape_manual(values = predictor_shapes, name = "Predictor", drop = FALSE) +
  scale_alpha_manual(values = c("p < 0.05" = 1, "p >= 0.05" = 0.2), guide = "none") +
  # Add size scale for thicker lines
  scale_size_manual(values = c("p < 0.05" = 1, "p >= 0.05" = 0.5), guide = "none") +
  scale_y_discrete(limits = rev(levels(compliance_results_SA_T2$label))) +
  labs(
    title = "Compliance in Shared Area (Known Out-group Stage)",
    x = "Coefficient (95% CI)", # Central x-axis title
    y = ""
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none")


# --- Plot C: Combined Beliefs ---
combined_beliefs <- rbind(bel_SA_T1, bel_SA_T2)
combined_beliefs$Significance <- ifelse(combined_beliefs$pvalue < 0.05, "p < 0.05", "p >= 0.05")
outcome_order <- c(
  "Beliefs Out-group (Unknown)", "Beliefs In-group (Stage 1)",
  "Beliefs Out-group (Known)", "Beliefs In-group (Stage 2)"
)
combined_beliefs$Outcome <- factor(combined_beliefs$Outcome, levels = rev(outcome_order))

p_beliefs <- ggplot(combined_beliefs, aes(x = est, y = Outcome, color = Predictor, shape = Predictor)) +
  geom_hline(yintercept = 2.5, linetype = "dashed", color = "gray70") +
  # FIX 2: Add size mapping for bolder lines
  geom_errorbarh(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se, alpha = Significance, size = Significance), height = 0.0, position = position_dodge(width = 0.5)) +
  geom_point(aes(alpha = Significance), position = position_dodge(width = 0.5), size = 4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  # FIX 3: Set consistent X-axis limits
  coord_cartesian(xlim = c(-0.3, 0.6)) +
  scale_color_manual(values = predictor_colors, name = "Predictor", drop = FALSE) +
  scale_shape_manual(values = predictor_shapes, name = "Predictor", drop = FALSE) +
  scale_alpha_manual(values = c("p < 0.05" = 1, "p >= 0.05" = 0.2), guide = "none") +
  # Add size scale for thicker lines
  scale_size_manual(values = c("p < 0.05" = 1, "p >= 0.05" = 0.5), guide = "none") +
  labs(
    title = "C) Beliefs about Behavior in Shared Area",
    x = NULL, # Remove individual x-axis titles
    y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(face="bold"),
    legend.position = "none"
  )

# --- Final Assembly with Manual Legend ---
# 1. Create a standalone legend plot
# Create a dummy plot with all geoms to ensure they appear in the legend
legend_plot_data <- data.frame(
  Predictor = factor(all_predictor_labels, levels = all_predictor_labels),
  est = 0, 
  se = 1 # Dummy data for aesthetics
)
legend_plot <- ggplot(legend_plot_data, aes(x = est, y = Predictor, color = Predictor, shape = Predictor)) +
  geom_errorbarh(aes(xmin = est - se, xmax = est + se)) +
  geom_point(size = 3) +
  scale_color_manual(values = predictor_colors, name = "Predictor", drop = FALSE) +
  scale_shape_manual(values = predictor_shapes, name = "Predictor", drop = FALSE) +
  theme_minimal() +
  guides(color = guide_legend(nrow=2), shape = guide_legend(nrow=2)) +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 12))

# 2. Extract the legend graphical object
shared_legend <- get_legend(legend_plot)

# 3. Arrange the three plots in the desired order
plots_row <- p_compliance_T1 + p_compliance_T2 + p_beliefs

# 4. Combine the row of plots with the shared legend underneath
final_plot <- plots_row / shared_legend +
  plot_layout(heights = c(1, 0.15)) # Give space for the legend


# Save the final combined plot
ggsave(
  paste0(path_github, "Outputs/Combined_SEM_Plot3.pdf"), 
  final_plot, 
  width = 18,
  height = 7
)

# Print the final plot to the viewer
#print(final_plot)

