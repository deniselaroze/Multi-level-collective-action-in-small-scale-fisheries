# ==============================================================================
# Script: SEM Round 8 Analysis
# Description: Runs a combined SEM (SUR) to compare compliance decisions 
# between Stage 1 (Unknown) and Stage 2 (Known) for the same individuals.
# Uses Robust Standard Errors (MLR) instead of Bootstrap to ensure stability.
# ==============================================================================

# Load necessary libraries
library(lavaan)
library(dplyr)
library(tidyr)
library(modelsummary)
library(tinytable)
library(rlang)
library(pandoc)

rm(list = ls())

# --- 1. Setup: Paths and Data ---
path_github <- "C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos  <- "C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

# Load the recoded wide data
load(paste0(path_datos, "/Datos_islitas_recode.Rdata")) 

# --- 2. Data Preparation Function ---
prepare_combined_data <- function(data, R_round) {
  # --- Stage 1 Vars ---
  cols1 <- declare_get_columns("T1juegoalgas", "T1_extraccion_libre", R_round, R_round, data)
  data$S1_compliance <- 1 - rowMeans(data[, cols1, drop = FALSE], na.rm = TRUE) / 50
  data$S1_belief_pm   <- 1 - data$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50
  data$S1_belief_union<- 1 - data$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50
  
  obs_cols1 <- declare_get_columns("T1juegoalgas", "T1_extraccion_otros_libre", R_round - 1, R_round - 1, data)
  data$S1_obs_lag <- 1 - (rowMeans(data[, obs_cols1, drop = FALSE], na.rm = TRUE) / 150)
  
  data$S1_trust_out <- as.numeric(scale(data$survey1.1.player.confianza_pm))
  data$S1_conf_out  <- as.numeric(scale(data$survey1.1.player.conflicto_pm))
  data$S1_trust_in  <- as.numeric(scale(data$survey1.1.player.confianza_caleta))
  data$S1_conf_in   <- as.numeric(scale(data$survey1.1.player.conflicto_caleta))
  
  # --- Stage 2 Vars ---
  cols2 <- declare_get_columns("T2juegoalgas", "T2_extraccion_metat", R_round, R_round, data)
  data$S2_compliance <- 1 - rowMeans(data[, cols2, drop = FALSE], na.rm = TRUE) / 50
  data$S2_belief_pm   <- 1 - data$beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini / 50
  data$S2_belief_union <- 1 - (data$beliefsT2inicial.1.player.T2_belief_caleta_ini / 50)
  
  obs_cols2 <- declare_get_columns("T2juegoalgas", "T2_extraccion_otros_metat", R_round - 1, R_round - 1, data)
  data$S2_obs_lag <- 1 - (rowMeans(data[, obs_cols2, drop = FALSE], na.rm = TRUE) / 150)
  
  data$S2_trust_out <- as.numeric(scale(data$survey2.1.player.confianza_caleta_conocida_mean))
  data$S2_conf_out  <- as.numeric(scale(data$survey2.1.player.conflicto_caleta_conocida_mean))
  data$S2_trust_in  <- as.numeric(scale(data$survey1.1.player.confianza_caleta)) 
  data$S2_conf_in   <- as.numeric(scale(data$survey1.1.player.conflicto_caleta))
  
  return(data)
}

declare_get_columns <- function(prefix, suffix, start, end = NULL, data) {
  if (is.null(end)) end <- start
  rounds <- start:end
  cols <- unlist(lapply(rounds, function(r) {
    patt <- paste0("^", prefix, "\\.", r, "\\.player\\.", suffix, "$")
    grep(patt, names(data), value = TRUE)
  }))
  return(cols)
}

# --- 3. Combined SEM (SUR) Model ---
run_combined_sem <- function(data) {
  sem_model <- '
    # --- STAGE 1 EQUATIONS ---
    S1_belief_pm ~ S1_trust_out + S1_conf_out
    S1_belief_union ~ S1_trust_in + S1_conf_in
    S1_compliance ~ a1*S1_belief_pm + a2*S1_belief_union + S1_trust_out + S1_conf_out + S1_trust_in + S1_conf_in + a3*S1_obs_lag
    
    # --- STAGE 2 EQUATIONS ---
    S2_belief_pm ~ S2_trust_out + S2_conf_out
    S2_belief_union ~ S2_trust_in + S2_conf_in
    S2_compliance ~ b1*S2_belief_pm + b2*S2_belief_union + S2_trust_out + S2_conf_out + S2_trust_in + S2_conf_in + b3*S2_obs_lag
    
    # --- COVARIANCES (Accounting for same individuals) ---
    S1_compliance ~~ S2_compliance
    
    # --- DEFINED PARAMETERS ---
    total_S1 := a1 + a2 + a3
    total_S2 := b1 + b2 + b3
    
    # Statistical tests for difference (Magnitude comparison)
    diff_total_cc   := total_S1 - total_S2
    diff_belief_out := a1 - b1
    diff_belief_in  := a2 - b2
  '
  
  # estimator = "MLR" provides robust Huber-White standard errors 
  # which are much more stable for your sample size than bootstrapping.
  fit <- sem(sem_model, 
             data = data, 
             estimator = "MLR", 
             missing = "fiml",
             se = "robust")
  return(fit)
}

# --- 4. Run Analysis ---
df_combined <- prepare_combined_data(df, 8)

# Run model
set.seed(123)
fit_combined <- run_combined_sem(df_combined)

# --- 5. Extraction and Table Formatting ---

var_labels <- c(
  "S1_trust_out" = "Trust Out-group", "S2_trust_out" = "Trust Out-group",
  "S1_trust_in"  = "Trust In-group",  "S2_trust_in"  = "Trust In-group",
  "S1_conf_out"  = "Conflict Out-group", "S2_conf_out" = "Conflict Out-group",
  "S1_conf_in"   = "Conflict In-group",  "S2_conf_in"  = "Conflict In-group",
  "S1_belief_pm" = "Prior Beliefs Out-group", "S2_belief_pm" = "Prior Beliefs Out-group",
  "S1_belief_union" = "Prior Beliefs In-group", "S2_belief_union" = "Prior Beliefs In-group",
  "S1_obs_lag"   = "Observed Compliance (round 7)", "S2_obs_lag" = "Observed Compliance (round 7)",
  "total_S1"     = "Sum Conditional Cooperation", "total_S2" = "Sum Conditional Cooperation",
  "diff_total_cc" = "DIFFERENCE: Total CC (S1 - S2)",
  "diff_belief_out" = "DIFFERENCE: Beliefs Out-group",
  "diff_belief_in" = "DIFFERENCE: Beliefs In-group"
)

res <- parameterEstimates(fit_combined) %>% 
  filter(op %in% c("~", "~1", ":=")) %>%
  mutate(
    Stage = case_when(
      grepl("S1_", rhs) | grepl("S1_", lhs) | lhs == "total_S1" ~ "Stage 1",
      grepl("S2_", rhs) | grepl("S2_", lhs) | lhs == "total_S2" ~ "Stage 2",
      grepl("diff_", lhs) ~ "Comparison",
      TRUE ~ "Other"
    ),
    Predictor = case_when(
      op == "~1" ~ "Constant",
      op == ":=" ~ var_labels[lhs],
      rhs %in% names(var_labels) ~ var_labels[rhs],
      TRUE ~ rhs
    ),
    DV = case_when(
      lhs %in% c("S1_compliance", "S2_compliance", "total_S1", "total_S2", "diff_total_cc") ~ "Compliance",
      lhs %in% c("S1_belief_pm", "S2_belief_pm", "diff_belief_out") ~ "Beliefs Out-group",
      lhs %in% c("S1_belief_union", "S2_belief_union", "diff_belief_in") ~ "Beliefs In-group",
      TRUE ~ "Other"
    ),
    Column_Group = paste(Stage, "DV:", DV),
    # Use Robust p-values and SEs
    Signif = case_when(pvalue < 0.01 ~ "***", pvalue < 0.05 ~ "**", pvalue < 0.1 ~ "*", TRUE ~ ""),
    Formatted = sprintf("%.3f%s (%.3f)", est, Signif, se)
  )

# Create the Final Wide Table
# Fixed pivot_wider error by adding distinct() and ensuring list conversion doesn't occur
wide_table <- res %>%
  filter(!is.na(Predictor) & Stage != "Other") %>%
  select(Predictor, Column_Group, Formatted) %>%
  distinct(Predictor, Column_Group, .keep_all = TRUE) %>%
  pivot_wider(
    names_from = Column_Group, 
    values_from = Formatted, 
    values_fill = list(Formatted = "")
  )

# --- 6. Export to Word ---
table_file_path <- paste0(path_github, "Outputs/SEM_Round8_Combined_Robust.docx")

datasummary_df(
  wide_table, 
  title = "Combined SEM: Stage 1 (Unknown) vs Stage 2 (Known Out-group)",
  notes = c("* p < 0.1, ** p < 0.05, *** p < 0.01",
            "Standard errors are robust (MLR) to account for non-normality and sample size."),
  output = table_file_path
)

message("Success! Model estimated with robust standard errors and difference tests.")