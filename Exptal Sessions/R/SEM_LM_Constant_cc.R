# ==============================================================================
# Script: SEM Round 8 Analysis
# Description: Runs SEM models for compliance decisions specifically in Round 8
# for both Unknown (T1) and Known (T2) out-group scenarios and
# exports the results into a combined summary table including combined effects
# and explicit constants (intercepts).
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

# --- 2. Helper Functions ---

declare_get_columns <- function(prefix, suffix, start, end = NULL, data) {
  if (is.null(end)) end <- start
  rounds <- start:end
  cols <- unlist(lapply(rounds, function(r) {
    patt <- paste0("^", prefix, "\\.", r, "\\.player\\.", suffix, "$")
    grep(patt, names(data), value = TRUE)
  }))
  return(cols)
}

# SEM Function for Stage 1 (Unknown Out-group)
coef_SA_T1_sem <- function(data, R_start, R_end) {
  cols <- declare_get_columns("T1juegoalgas", "T1_extraccion_libre", R_start, R_end, data)
  subset_ini <- data[, cols, drop = FALSE]
  
  data$average_extraction_ini <- rowMeans(subset_ini, na.rm = TRUE)
  data$average_compliance_ini <- 1 - data$average_extraction_ini / 50
  data$belief_compliance_pm   <- 1 - data$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50
  data$belief_compliance_union<- 1 - data$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50
  
  data$confianza_caleta<- as.numeric(scale(data$survey1.1.player.confianza_caleta))
  data$conflicto_caleta<- as.numeric(scale(data$survey1.1.player.conflicto_caleta))
  data$confianza_pm<- as.numeric(scale(data$survey1.1.player.confianza_pm))
  data$conflicto_pm<- as.numeric(scale(data$survey1.1.player.conflicto_pm))
  
  if (R_start > 1) {
    cols_obs <- declare_get_columns("T1juegoalgas", "T1_extraccion_otros_libre", R_start - 1, R_start - 1, data)
    subset_obs <- data[, cols_obs, drop = FALSE]
    data$average_extraction_observed_ini <- rowMeans(subset_obs, na.rm = TRUE)
    data$average_compliance_observed_ini_lag <- 1 - (data$average_extraction_observed_ini / 150)
  } else {
    data$average_compliance_observed_ini_lag <- NA
  }
  
  sem_model <- '
    # Regressions
    belief_compliance_pm ~ confianza_pm + conflicto_pm
    belief_compliance_union ~ confianza_caleta + conflicto_caleta
    average_compliance_ini ~ c1*belief_compliance_pm + c2*belief_compliance_union + confianza_pm + conflicto_pm + confianza_caleta + conflicto_caleta + c3*average_compliance_observed_ini_lag
    
    # Explicitly estimate intercepts (Constants)
    belief_compliance_pm ~ 1
    belief_compliance_union ~ 1
    average_compliance_ini ~ 1

    # Covariances
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_union
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_pm
    
    # Combined Effect Parameter
    conditional_coop_total := c1 + c2 + c3
  '
  
  fit <- sem(sem_model, data = data, estimator = "ML", se = "bootstrap", bootstrap = 1000, parallel = "multicore", ncpus = 4)
  return(fit)
}

# SEM Function for Stage 2 (Known Out-group)
coef_SA_T2_sem <- function(data, R_start, R_end) {
  cols <- declare_get_columns("T2juegoalgas", "T2_extraccion_metat", R_start, R_end, data)
  subset_ini <- data[, cols, drop = FALSE]
  
  data$average_extraction_ini <- rowMeans(subset_ini, na.rm = TRUE)
  data$average_compliance_ini <- 1 - data$average_extraction_ini / 50
  data$belief_compliance_pm   <- 1 - data$beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini / 50
  data$belief_compliance_union <- 1 - (data$beliefsT2inicial.1.player.T2_belief_caleta_ini / 50)
  
  data$confianza_caleta<- as.numeric(scale(data$survey1.1.player.confianza_caleta))
  data$conflicto_caleta<- as.numeric(scale(data$survey1.1.player.conflicto_caleta))
  data$confianza_metat<- as.numeric(scale(data$survey2.1.player.confianza_caleta_conocida_mean))
  data$conflicto_metat<- as.numeric(scale(data$survey2.1.player.conflicto_caleta_conocida_mean))
  
  if (R_start > 1) {
    cols_obs <- declare_get_columns("T2juegoalgas", "T2_extraccion_otros_metat", R_start - 1, R_start - 1, data)
    subset_obs <- data[, cols_obs, drop = FALSE]
    data$average_extraction_observed_ini <- rowMeans(subset_obs, na.rm = TRUE)
    data$average_compliance_observed_ini_lag <- 1 - (data$average_extraction_observed_ini / 150)
  } else {
    data$average_compliance_observed_ini_lag <- NA
  }
  
  sem_model <- '
    # Regressions
    belief_compliance_pm ~ confianza_metat + conflicto_metat
    belief_compliance_union ~ confianza_caleta + conflicto_caleta
    average_compliance_ini ~ c1*belief_compliance_pm + c2*belief_compliance_union + confianza_metat + conflicto_metat + confianza_caleta + conflicto_caleta + c3*average_compliance_observed_ini_lag
    
    # Explicitly estimate intercepts (Constants)
    belief_compliance_pm ~ 1
    belief_compliance_union ~ 1
    average_compliance_ini ~ 1

    # Covariances
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_union
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_pm
    
    # Combined Effect Parameter
    conditional_coop_total := c1 + c2 + c3
  '
  
  fit <- sem(sem_model, data = data, estimator = "ML", se = "bootstrap", bootstrap = 1000, parallel  = "multicore", ncpus = 4)
  return(fit)
}

# --- 3. Run Models ---
set.seed(478)
fit_T1_R8 <- coef_SA_T1_sem(df, 8, 8)
set.seed(4523)
fit_T2_R8 <- coef_SA_T2_sem(df, 8, 8)

# --- 4. Extract and Label Results ---

var_labels <- c(
  "confianza_pm"                        = "Trust Out-group",
  "confianza_metat"                     = "Trust Out-group",
  "confianza_caleta"                    = "Trust In-group",
  "conflicto_pm"                        = "Conflict Out-group",
  "conflicto_metat"                     = "Conflict Out-group",
  "conflicto_caleta"                    = "Conflict In-group",
  "belief_compliance_pm"                = "Prior Beliefs Out-group",
  "belief_compliance_union"             = "Prior Beliefs In-group",
  "average_compliance_observed_ini_lag" = "Observed Compliance (round 7)",
  "conditional_coop_total"              = "Sum Conditional Cooperation"
)

# Extracting results: inclusion of op "~1" captures intercepts
all_coefs <- bind_rows(
  parameterEstimates(fit_T1_R8) %>% filter(op %in% c("~", "~1", ":=")) %>% mutate(Stage = "Stage 1"),
  parameterEstimates(fit_T2_R8) %>% filter(op %in% c("~", "~1", ":=")) %>% mutate(Stage = "Stage 2")
) %>%
  mutate(
    DV_Type = case_when(
      lhs == "average_compliance_ini" ~ "Compliance",
      lhs == "belief_compliance_pm" ~ "Beliefs Out-group",
      lhs == "belief_compliance_union" ~ "Beliefs In-group",
      lhs == "conditional_coop_total" ~ "Compliance" 
    ),
    Column_Name = paste(Stage, "DV:", DV_Type),
    
    # Fix the labeling logic for Predictor, including the Constant
    Predictor = case_when(
      op == "~1" ~ "Constant",
      op == ":=" ~ var_labels[lhs],
      rhs %in% names(var_labels) ~ var_labels[rhs],
      TRUE ~ rhs
    ),
    
    Significance = case_when(
      pvalue < 0.001 ~ "***",
      pvalue < 0.01 ~ "**",
      pvalue < 0.05 ~ "*",
      pvalue < 0.1  ~ "†",
      TRUE ~ ""
    ),
    Formatted = sprintf("%.3f%s (%.3f)", est, Significance, se)
  )

# Define factor levels to control the display order precisely
pred_order <- c(
  "Trust Out-group", "Trust In-group", 
  "Conflict Out-group", "Conflict In-group",
  "Prior Beliefs Out-group", "Prior Beliefs In-group", 
  "Observed Compliance (round 7)", 
  "Sum Conditional Cooperation",
  "Constant"
)
all_coefs$Predictor <- factor(all_coefs$Predictor, levels = pred_order)

# Pivot to wide format
wide_table <- all_coefs %>%
  filter(!is.na(Predictor)) %>%
  select(Predictor, Column_Name, Formatted) %>%
  # Handle potential duplicate entries if same predictor appears twice per column
  distinct(Predictor, Column_Name, .keep_all = TRUE) %>%
  pivot_wider(
    names_from = Column_Name,
    values_from = Formatted,
    values_fill = ""
  ) %>%
  arrange(Predictor) %>%
  select(
    Predictor,
    `Stage 1 DV: Beliefs In-group`,
    `Stage 1 DV: Beliefs Out-group`,
    `Stage 1 DV: Compliance`,
    `Stage 2 DV: Beliefs In-group`,
    `Stage 2 DV: Beliefs Out-group`,
    `Stage 2 DV: Compliance`
  ) %>%
  mutate(Predictor = as.character(Predictor))

# --- 5. Extract Goodness of Fit Statistics ---
gof_T1 <- fitMeasures(fit_T1_R8, c("ntotal", "logl", "aic", "bic"))
gof_T2 <- fitMeasures(fit_T2_R8, c("ntotal", "logl", "aic", "bic"))
r2_T1 <- lavInspect(fit_T1_R8, "rsquare")
r2_T2 <- lavInspect(fit_T2_R8, "rsquare")

gof_table <- data.frame(
  Predictor = c("Num.Obs.", "Log Likelihood", "AIC", "BIC", "R-squared"),
  `Stage 1 DV: Beliefs In-group` = c(round(gof_T1["ntotal"]), sprintf("%.3f", gof_T1["logl"]), sprintf("%.3f", gof_T1["aic"]), sprintf("%.3f", gof_T1["bic"]), sprintf("%.3f", r2_T1["belief_compliance_union"])),
  `Stage 1 DV: Beliefs Out-group` = c(round(gof_T1["ntotal"]), sprintf("%.3f", gof_T1["logl"]), sprintf("%.3f", gof_T1["aic"]), sprintf("%.3f", gof_T1["bic"]), sprintf("%.3f", r2_T1["belief_compliance_pm"])),
  `Stage 1 DV: Compliance` = c(round(gof_T1["ntotal"]), sprintf("%.3f", gof_T1["logl"]), sprintf("%.3f", gof_T1["aic"]), sprintf("%.3f", gof_T1["bic"]), sprintf("%.3f", r2_T1["average_compliance_ini"])),
  `Stage 2 DV: Beliefs In-group` = c(round(gof_T2["ntotal"]), sprintf("%.3f", gof_T2["logl"]), sprintf("%.3f", gof_T2["aic"]), sprintf("%.3f", gof_T2["bic"]), sprintf("%.3f", r2_T2["belief_compliance_union"])),
  `Stage 2 DV: Beliefs Out-group` = c(round(gof_T2["ntotal"]), sprintf("%.3f", gof_T2["logl"]), sprintf("%.3f", gof_T2["aic"]), sprintf("%.3f", gof_T2["bic"]), sprintf("%.3f", r2_T2["belief_compliance_pm"])),
  `Stage 2 DV: Compliance` = c(round(gof_T2["ntotal"]), sprintf("%.3f", gof_T2["logl"]), sprintf("%.3f", gof_T2["aic"]), sprintf("%.3f", gof_T2["bic"]), sprintf("%.3f", r2_T2["average_compliance_ini"])),
  check.names = FALSE
)

final_table <- bind_rows(wide_table, gof_table)

# --- 6. Export ---
table_file_path_docx <- paste0(path_github, "Outputs/SEM_Round8_Summary_Table_RG_Final.docx")
datasummary_df(
  final_table,
  title = "Structural Equation Models for Compliance and Beliefs in Round 8",
  notes = c("† p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001",
            "Standard errors for all coefficients (including the sum and constants) are bootstrapped (1000 iterations)."),
  output = table_file_path_docx
)

message("\nSuccess! Summary table saved to: ", table_file_path_docx)