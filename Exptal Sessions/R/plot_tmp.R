library(lavaan)
library(semPlot)
library(ggplot2)
library(viridis)
library(patchwork) # For combining plots
library(RColorBrewer) # For "autumn" color palette
library(ggpubr) # For extracting legend
library(flextable) # For exporting to Word
library(knitr)     # For exporting to text

# install.packages("paletteer")
#library(paletteer)

# Use in a ggplot2 chart:
#scale_colour_paletteer_d("ggthemes::Classic_Color_Blind")
#scale_fill_paletteer_d("ggthemes::Classic_Color_Blind")

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

all_predictor_labels <- c(
  "Observed Compliance (t−1)",
  "Prior Beliefs In-group", "Prior Beliefs Out-group",
  "Trust In-group", "Trust Out-group", 
  "Conflict In-group", "Conflict Out-group"
)

############################################################
# Stage 1: Shared Area - Unknown Out-group (T1)
############################################################
set.seed(478)

coef_SA_T1_sem <- function(data, R_start, R_end) {
  cols <- declare_get_columns("T1juegoalgas", "T1_extraccion_libre", R_start, R_end, data)
  subset_ini <- data[, cols, drop = FALSE]
  data$average_extraction_ini <- rowMeans(subset_ini, na.rm = TRUE)
  data$average_compliance_ini <- 1 - data$average_extraction_ini / 50
  data$belief_compliance_pm    <- 1 - data$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50
  data$belief_compliance_union <- 1 - data$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50
  
  data$confianza_caleta<- as.numeric(scale(data$survey1.1.player.confianza_caleta))
  data$conflicto_caleta<- as.numeric(scale(data$survey1.1.player.conflicto_caleta))
  data$confianza_pm<- as.numeric(scale(data$survey1.1.player.confianza_pm))
  data$conflicto_pm<- as.numeric(scale(data$survey1.1.player.conflicto_pm))  
  
  if (R_start > 1) {
    # For single rounds, lag is the previous round. For the mean, it's round 1.
    lag_round <- ifelse(R_start == R_end, R_start - 1, 1)
    cols_obs <- declare_get_columns("T1juegoalgas", "T1_extraccion_otros_libre", lag_round, lag_round, data)
    subset_obs <- data[, cols_obs, drop = FALSE]
    data$average_extraction_observed_ini       <- rowMeans(subset_obs, na.rm = TRUE)
    # The divisor logic needs to be robust to single/multiple columns
    data$average_compliance_observed_ini_lag <- 1 - data$average_extraction_observed_ini / (50 * (ifelse(is.null(ncol(subset_obs)), 1, ncol(subset_obs))))
  } else {
    data$average_compliance_observed_ini_lag <- NA
  }
  
  sem_model <- '
    belief_compliance_pm ~ confianza_pm + conflicto_pm
    belief_compliance_union ~ confianza_caleta + conflicto_caleta
    average_compliance_ini ~ belief_compliance_pm + belief_compliance_union + confianza_pm + conflicto_pm + confianza_caleta + conflicto_caleta + average_compliance_observed_ini_lag
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_union
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_pm
  '
  
  # MODIFICATION: Reduced bootstrap iterations
  fit <- sem(sem_model, data = data, estimator = "ML", se = "bootstrap", bootstrap = 1000, parallel = "multicore", ncpus = 4)
  return(fit)
}

# --- Dynamic SEM for T1 ---
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
  "conflicto_pm"        = "Conflict Out-group",
  "conflicto_caleta"    = "Conflict In-group",
  "confianza_pm"        = "Trust Out-group",
  "confianza_caleta"    = "Trust In-group",
  "average_compliance_observed_ini_lag" = "Observed Compliance (t−1)"
)
compliance_results_SA$Predictor <- factor(
  sa_T1_compliance_labels[compliance_results_SA$rhs],
  levels = all_predictor_labels
)

# --- Belief Formation SEM for T1 (Static) ---
sem_model_beliefs_SA_T1 <- '
  belief_compliance_pm    ~ confianza_pm + conflicto_pm
  belief_compliance_union ~ confianza_caleta + conflicto_caleta
'
df$belief_compliance_pm    <- 1 - df$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50
df$belief_compliance_union <- 1 - df$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50
df$confianza_caleta<- as.numeric(scale(df$survey1.1.player.confianza_caleta))
df$conflicto_caleta<- as.numeric(scale(df$survey1.1.player.conflicto_caleta))
df$confianza_pm<- as.numeric(scale(df$survey1.1.player.confianza_pm))
df$conflicto_pm<- as.numeric(scale(df$survey1.1.player.conflicto_pm))  


# MODIFICATION: Reduced bootstrap iterations
fit_bel_SA_T1 <- sem(sem_model_beliefs_SA_T1, data = df, estimator = "ML", se = "bootstrap", bootstrap = 1000, parallel = "multicore", ncpus = 4)
pe_bel_SA_T1 <- parameterEstimates(fit_bel_SA_T1)
bel_SA_T1 <- subset(pe_bel_SA_T1, op == "~", select = c("lhs", "rhs", "est", "se", "pvalue"))

# FIX 1: Use the full, correct mapping for belief predictors
bel_SA_T1$Predictor <- factor(sa_T1_compliance_labels[bel_SA_T1$rhs], levels = all_predictor_labels)
bel_SA_T1$Outcome <- factor(c("belief_compliance_pm" = "Out-group (Unknown)", 
                              "belief_compliance_union" = "In-group (T1)")[bel_SA_T1$lhs])


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
  data$belief_compliance_pm    <- 1 - data$beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini / 50
  data$belief_compliance_union <- 1 - (data$beliefsT2inicial.1.player.T2_belief_caleta_ini / 50)
  
  data$confianza_caleta<- as.numeric(scale(data$survey1.1.player.confianza_caleta))
  data$conflicto_caleta<- as.numeric(scale(data$survey1.1.player.conflicto_caleta))
  data$confianza_metat<- as.numeric(scale(data$survey2.1.player.confianza_caleta_conocida_mean))
  data$conflicto_metat<- as.numeric(scale(data$survey2.1.player.conflicto_caleta_conocida_mean))  
  
  # lagged observed compliance
  if (R_start > 1) {
    lag_round <- ifelse(R_start == R_end, R_start - 1, 1)
    cols_obs <- declare_get_columns("T2juegoalgas", "T2_extraccion_otros_metat", lag_round, lag_round, data)
    subset_obs <- data[, cols_obs, drop = FALSE]
    data$average_extraction_observed_ini       <- rowMeans(subset_obs, na.rm = TRUE)
    data$average_compliance_observed_ini_lag <- 1 - data$average_extraction_observed_ini / 50
  } else {
    data$average_compliance_observed_ini_lag <- NA
  }
  
  # SEM model specification
  sem_model <- '
    belief_compliance_pm ~ confianza_metat + conflicto_metat
    belief_compliance_union ~ confianza_caleta + conflicto_caleta
    average_compliance_ini ~ belief_compliance_pm + belief_compliance_union + 
    confianza_metat + conflicto_metat + 
    confianza_caleta + conflicto_caleta + 
    average_compliance_observed_ini_lag
    
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_union
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_pm
  '
  
  # MODIFICATION: Reduced bootstrap iterations
  fit <- sem(sem_model, data = data, estimator = "ML", se = "bootstrap", bootstrap = 1000, parallel  = "multicore", ncpus = 4)
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
  "belief_compliance_pm"    = "Prior Beliefs Out-group",
  "belief_compliance_union"   = "Prior Beliefs In-group",
  "conflicto_metat"         = "Conflict Out-group",
  "conflicto_caleta"        = "Conflict In-group",
  "confianza_metat"         = "Trust Out-group",
  "confianza_caleta"        = "Trust In-group",
  "average_compliance_observed_ini_lag"         = "Observed Compliance (t−1)"
)

compliance_results_SA_T2$Predictor <- factor(
  sa_T2_compliance_labels[compliance_results_SA_T2$rhs],
  levels = all_predictor_labels
)

# --- Belief Formation SEM for T2 (Static) ---
df$belief_compliance_pm_T2    <- 1 - df$beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini / 50
df$belief_compliance_union_T2 <- 1 - df$beliefsT2inicial.1.player.T2_belief_caleta_ini / 50
df$confianza_caleta<- as.numeric(scale(df$survey1.1.player.confianza_caleta))
df$conflicto_caleta<- as.numeric(scale(df$survey1.1.player.conflicto_caleta))
df$confianza_metat<- as.numeric(scale(df$survey2.1.player.confianza_caleta_conocida_mean))
df$conflicto_metat<- as.numeric(scale(df$survey2.1.player.conflicto_caleta_conocida_mean))  

sem_model_beliefs_SA_T2 <- '
  belief_compliance_pm_T2    ~ confianza_metat +conflicto_metat
  belief_compliance_union_T2 ~ confianza_caleta + conflicto_caleta
'

# MODIFICATION: Reduced bootstrap iterations
fit_bel_SA_T2 <- sem(sem_model_beliefs_SA_T2, data = df, estimator = "ML", se = "bootstrap", bootstrap = 1000, parallel = "multicore", ncpus = 4)
pe_bel_SA_T2 <- parameterEstimates(fit_bel_SA_T2)
bel_SA_T2 <- subset(pe_bel_SA_T2, op == "~", select = c("lhs", "rhs", "est", "se", "pvalue"))

# FIX 1: Use the full, correct mapping for belief predictors
bel_SA_T2$Predictor <- factor(sa_T2_compliance_labels[bel_SA_T2$rhs], levels = all_predictor_labels)
bel_SA_T2$Outcome <- factor(c(
  "belief_compliance_pm_T2"    = "Out-group (Known)",
  "belief_compliance_union_T2" = "In-group (T2)"
)[bel_SA_T2$lhs])


###################################################
# --- Prepare Individual Plots for Combination ---
#################################################### Define a consistent set of labels, colors, and shapes for all plots

# Use in a ggplot2 chart:
#scale_colour_paletteer_d("ggthemes::Classic_Cyclic")
#scale_fill_paletteer_d("ggthemes::Classic_Cyclic")

#high_contrast_palette<-c("#1F83B4FF", "#12A2A8FF", "#2CA030FF", "#78A641FF","#BCBD22FF", "#FFBF50FF", "#FFAA0EFF","#FF7F0EFF", "#D63A3AFF", "#C7519CFF","#BA43B4FF","#8A60B0FF","#6F63BBFF")
#palette<-c("#1F83B4FF", "#12A2A8FF", "#2CA030FF", "#78A641FF","#BCBD22FF",  "#D63A3AFF","#8A60B0FF", "#FFAA0EFF")
high_contrast_palette<-c( "#D63A3AFF","#8A60B0FF", "#FFAA0EFF","#1F83B4FF", "#12A2A8FF", "#2CA030FF", "#78A641FF","#BCBD22FF" )


predictor_colors <- setNames(
  high_contrast_palette,
  all_predictor_labels
)

# MODIFICATION: Added shape palette
predictor_shapes <- setNames(
  c(8, 3, 4, 18, 15, 17, 16),
  #c(16, 17, 15, 18, 4, 3, 8),
  all_predictor_labels
)


# --- Plot A: Dynamic Compliance T1 (Unknown) ---
pd <- position_dodge(width = 0.6)
p_compliance_T1 <- ggplot(compliance_results_SA, aes(x = est, y = label, group = Predictor)) +
  # FIX 2: Use linewidth for error bars, size for points
  geom_errorbarh(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se, color = Predictor, alpha = Significance, linewidth = Significance), height = 0.0, position = pd) +
  geom_point(aes(color = Predictor, shape = Predictor, alpha = Significance, size = Significance), position = pd) +
  geom_text(data = subset(compliance_results_SA, Significance == "p < 0.05"),
            aes(label = round(est, 2)),
            position = pd,
            vjust = -1,
            size = 12, # Increased from 10
            show.legend = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  # FIX 3: Set consistent X-axis limits
  coord_cartesian(xlim = c(-0.3, 0.6)) +
  scale_color_manual(values = predictor_colors, name = "Predictor", drop = FALSE) +
  scale_shape_manual(values = predictor_shapes, name = "Predictor", drop = FALSE) +
  scale_alpha_manual(values = c("p < 0.05" = 1, "p >= 0.05" = 0.4), guide = "none") +
  # Add size scale for points
  scale_size_manual(values = c("p < 0.05" = 8.0, "p >= 0.05" = 5.0), guide = "none") + # Increased from 6.0 / 4.0
  # Add linewidth scale for error bars (replaces old scale_size_manual)
  scale_linewidth_manual(values = c("p < 0.05" = 3.0, "p >= 0.05" = 1.5), guide = "none") + # Increased from 2.0 / 1.0
  scale_y_discrete(limits = rev(levels(compliance_results_SA$label))) +
  labs(
    title = "a.1) Compliance Shared Area (T1 - Unknown Out-group)",
    x = NULL, # Remove individual x-axis titles
    y = "Time Period"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 1, size = 32, face = "bold"), 
    legend.position = "none",
    axis.text = element_text(size = 32), # Increased from 28
    axis.title = element_text(size = 32),
    plot.margin = margin(b = 40)
  )


# --- Plot B: Dynamic Compliance T2 (Known) ---
p_compliance_T2 <- ggplot(compliance_results_SA_T2, aes(x = est, y = label, group = Predictor)) +
  # FIX 2: Use linewidth for error bars, size for points
  geom_errorbarh(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se, color = Predictor, alpha = Significance, linewidth = Significance), height = 0.0, position = pd) +
  geom_point(aes(color = Predictor, shape = Predictor, alpha = Significance, size = Significance), position = pd) +
  geom_text(data = subset(compliance_results_SA_T2, Significance == "p < 0.05"),
            aes(label = round(est, 2)),
            position = pd,
            vjust = -1,
            size = 12, # Increased from 10
            show.legend = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  # FIX 3: Set consistent X-axis limits
  coord_cartesian(xlim = c(-0.3, 0.6)) +
  scale_color_manual(values = predictor_colors, name = "Predictor", drop = FALSE) +
  scale_shape_manual(values = predictor_shapes, name = "Predictor", drop = FALSE) +
  scale_alpha_manual(values = c("p < 0.05" = 1, "p >= 0.05" = 0.4), guide = "none") +
  # Add size scale for points
  scale_size_manual(values = c("p < 0.05" = 8.0, "p >= 0.05" = 5.0), guide = "none") + # Increased from 6.0 / 4.0
  # Add linewidth scale for error bars (replaces old scale_size_manual)
  scale_linewidth_manual(values = c("p < 0.05" = 3.0, "p >= 0.05" = 1.5), guide = "none") + # Increased from 2.0 / 1.0
  scale_y_discrete(limits = rev(levels(compliance_results_SA_T2$label))) +
  labs(
    title = "a.2) Compliance Shared Area (T2 - Known Out-group)",
    x = NULL, 
    y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 1, size=32, face = "bold"), 
    legend.position = "none",
    axis.text = element_text(size = 32), # Increased from 28
    axis.title = element_text(size = 32),
    plot.margin = margin(b = 40)
  )


# --- Plot C: Beliefs Shared Area (T1 - Unknown Out-group) ---
bel_SA_T1$Significance <- ifelse(bel_SA_T1$pvalue < 0.05, "p < 0.05", "p >= 0.05")
outcome_order_T1 <- c("Out-group (Unknown)", "In-group (T1)")
bel_SA_T1$Outcome <- factor(bel_SA_T1$Outcome, levels = rev(outcome_order_T1))

p_beliefs_T1 <- ggplot(bel_SA_T1, aes(x = est, y = Outcome, color = Predictor, shape = Predictor)) +
  geom_errorbarh(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se, alpha = Significance, linewidth = Significance), height = 0.0, position = position_dodge(width = 0.6)) +
  geom_point(aes(alpha = Significance, size = Significance), position = position_dodge(width = 0.6)) +
  geom_text(data = subset(bel_SA_T1, Significance == "p < 0.05"),
            aes(label = round(est, 2)),
            position = pd,
            vjust = -1,
            size = 12, # Increased from 10
            show.legend = FALSE,
            color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  coord_cartesian(xlim = c(-0.3, 0.6)) +
  scale_color_manual(values = predictor_colors, name = "Predictor", drop = FALSE) +
  scale_shape_manual(values = predictor_shapes, name = "Predictor", drop = FALSE) +
  scale_alpha_manual(values = c("p < 0.05" = 1, "p >= 0.05" = 0.4), guide = "none") +
  scale_size_manual(values = c("p < 0.05" = 8.0, "p >= 0.05" = 5.0), guide = "none") + # Increased from 6.0 / 4.0
  # Add linewidth scale for error bars
  scale_linewidth_manual(values = c("p < 0.05" = 3.0, "p >= 0.05" = 1.5), guide = "none") + # Increased from 2.0 / 1.0
  labs(
    title = "b.1) Beliefs Shared Area (T1 - Unknown Out-group)",
    x = "Beta Coefficient (95% CI)",
    y = "Beliefs About"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 1, size = 32, face = "bold"),
    axis.text = element_text(size = 32), # Increased from 28
    axis.title = element_text(size = 32),
    legend.position = "none",
    plot.margin = margin(b = 40)
  )

# --- Plot D: Beliefs Shared Area (T2 - Known Out-group) ---
bel_SA_T2$Significance <- ifelse(bel_SA_T2$pvalue < 0.05, "p < 0.05", "p >= 0.05")
outcome_order_T2 <- c("Out-group (Known)", "In-group (T2)")
bel_SA_T2$Outcome <- factor(bel_SA_T2$Outcome, levels = rev(outcome_order_T2))

p_beliefs_T2 <- ggplot(bel_SA_T2, aes(x = est, y = Outcome, color = Predictor, shape = Predictor)) +
  geom_errorbarh(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se, alpha = Significance, linewidth = Significance), height = 0.0, position = position_dodge(width = 0.6)) +
  geom_point(aes(alpha = Significance, size = Significance), position = position_dodge(width = 0.6)) +
  geom_text(data = subset(bel_SA_T2, Significance == "p < 0.05"),
            aes(label = round(est, 2)),
            position = pd,
            vjust = -1,
            size = 12, # Increased from 10
            show.legend = FALSE,
            color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  coord_cartesian(xlim = c(-0.3, 0.6)) +
  scale_color_manual(values = predictor_colors, name = "Predictor", drop = FALSE) +
  scale_shape_manual(values = predictor_shapes, name = "Predictor", drop = FALSE) +
  scale_alpha_manual(values = c("p < 0.05" = 1, "p >= 0.05" = 0.4), guide = "none") +
  scale_size_manual(values = c("p < 0.05" = 8.0, "p >= 0.05" = 5.0), guide = "none") + # Increased from 6.0 / 4.0
  # Add linewidth scale for error bars
  scale_linewidth_manual(values = c("p < 0.05" = 3.0, "p >= 0.05" = 1.5), guide = "none") + # Increased from 2.0 / 1.0
  labs(
    title = "b.2) Beliefs Shared Area (T2 - Known Out-group)",
    x = "Beta Coefficient (95% CI)",
    y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 1, size = 32, face = "bold"),
    axis.text = element_text(size = 32), # Increased from 28
    axis.title = element_text(size = 32),
    legend.position = "none",
    plot.margin = margin(b = 40)
  )


# --- Final Assembly with Manual Legend ---

# 1. Create a standalone legend plot for Predictors
predictor_legend_plot <- ggplot(data.frame(
  Predictor = factor(all_predictor_labels, levels = all_predictor_labels),
  est = 0, se = 1
), aes(x = est, y = Predictor, color = Predictor, shape = Predictor)) +
  geom_errorbarh(aes(xmin = est - se, xmax = est + se)) +
  geom_point(size = 3) +
  scale_color_manual(values = predictor_colors, name = "Predictor", drop = FALSE) +
  scale_shape_manual(values = predictor_shapes, name = "Predictor", drop = FALSE) +
  theme_minimal() +
  guides(color = guide_legend(nrow = 2), shape = guide_legend(nrow = 2)) +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 32), # Increased from 28
        legend.text = element_text(size = 32)) # Increased from 28

# 2. Create a standalone legend plot for Significance
significance_legend_data <- data.frame(
  Significance = factor(c("p < 0.05", "p >= 0.05"), levels = c("p < 0.05", "p >= 0.05"))
)
significance_legend_plot <- ggplot(significance_legend_data, aes(x = 1, y = Significance, alpha = Significance)) +
  geom_point(size = 3, shape = 8, fill = "grey") +
  scale_alpha_manual(name = "Significance", values = c("p < 0.05" = 1, "p >= 0.05" = 0.4)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 32), # Increased from 28
        legend.text = element_text(size = 32)) # Increased from 28

# 3. Extract legends
predictor_legend <- get_legend(predictor_legend_plot)
significance_legend <- get_legend(significance_legend_plot)

# 4. Combine the two legends side-by-side
combined_legends <- wrap_plots(predictor_legend, significance_legend, ncol = 2, widths = c(3, 1))

# 5. Arrange all plots and the combined legend
final_plot <- (p_compliance_T1 + p_compliance_T2) / 
  (p_beliefs_T1 + p_beliefs_T2) / 
  combined_legends + 
  plot_layout(heights = c(0.7, 0.2, 0.1)) # Give more height to legend row for spacing


# Save the final combined plot
ggsave(
  paste0(path_github, "Outputs/Combined_SEM_Plot_2x2_manuscript2.tiff"), 
  final_plot, 
  width = 22,
  height = 30
)

###################################################
# --- Generate Supplementary Material Table ---
###################################################
# This section combines the data used for the figures into a single summary table.
# It includes the dynamic compliance results and the static belief formation results.

# 1. Wrangle T1 Compliance data
df_table_1 <- compliance_results_SA
df_table_1$Context <- "T1 (Unknown)"
df_table_1$Outcome <- "Compliance"
df_table_1$Time <- df_table_1$label
df_table_1 <- df_table_1[, c("Context", "Outcome", "Time", "Predictor", "est", "se", "pvalue")]
names(df_table_1) <- c("Context", "Outcome", "Time", "Predictor", "Estimate", "SE", "P_value")

# 2. Wrangle T2 Compliance data
df_table_2 <- compliance_results_SA_T2
df_table_2$Context <- "T2 (Known)"
df_table_2$Outcome <- "Compliance"
df_table_2$Time <- df_table_2$label
df_table_2 <- df_table_2[, c("Context", "Outcome", "Time", "Predictor", "est", "se", "pvalue")]
names(df_table_2) <- c("Context", "Outcome", "Time", "Predictor", "Estimate", "SE", "P_value")

# 3. Wrangle T1 Beliefs data (from static model)
df_table_3 <- bel_SA_T1
df_table_3$Context <- "T1 (Unknown)"
df_table_3$Time <- "Initial (Static)"
# 'Outcome' and 'Predictor' columns already exist
df_table_3 <- df_table_3[, c("Context", "Outcome", "Time", "Predictor", "est", "se", "pvalue")]
names(df_table_3) <- c("Context", "Outcome", "Time", "Predictor", "Estimate", "SE", "P_value")

# 4. Wrangle T2 Beliefs data (from static model)
df_table_4 <- bel_SA_T2
df_table_4$Context <- "T2 (Known)"
df_table_4$Time <- "Initial (Static)"
# 'Outcome' and 'Predictor' columns already exist
df_table_4 <- df_table_4[, c("Context", "Outcome", "Time", "Predictor", "est", "se", "pvalue")]
names(df_table_4) <- c("Context", "Outcome", "Time", "Predictor", "Estimate", "SE", "P_value")

# 5. Combine all dataframes
summary_table <- rbind(df_table_1, df_table_2, df_table_3, df_table_4)

# 6. Format and round the table
summary_table$Estimate <- round(summary_table$Estimate, 3)
summary_table$SE <- round(summary_table$SE, 3)
summary_table$P_value <- round(summary_table$P_value, 3)

# 7. Save to Word (.docx) using flextable
# Create a flextable object
ft <- flextable(summary_table)
ft <- autofit(ft) # Adjust column widths
ft <- theme_booktabs(ft) # Apply a clean theme

# Define file path for Word
table_file_path_docx <- paste0(path_github, "Outputs/SEM_Summary_Table_Supplementary.docx")

# Save the flextable as a Word document
# This requires the 'officer' package to be installed
if (requireNamespace("officer", quietly = TRUE)) {
  save_as_docx(ft, path = table_file_path_docx)
  message("Summary table saved to: ", table_file_path_docx)
} else {
  message("Please install the 'officer' package to save to .docx")
  message("You can install it with: install.packages(\"officer\")")
}

# 8. Save to Text (.txt) using knitr::kable
table_file_path_txt <- paste0(path_github, "Outputs/SEM_Summary_Table_Supplementary.txt")

# Create a text table (using a clean markdown format)
table_txt_lines <- kable(summary_table, format = "pipe")

# Save the text table to a file
writeLines(table_txt_lines, table_file_path_txt)
message("Summary table saved to: ", table_file_path_txt)