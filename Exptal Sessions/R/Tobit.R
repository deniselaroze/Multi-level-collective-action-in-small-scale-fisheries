# ==============================================================================
# Script: Tobit Path Analysis Round 8
# Description: Runs Tobit (Censored) Path models for compliance decisions [0,1]
# specifically in Round 8 for both Unknown (T1) and Known (T2) out-group scenarios 
# and exports the results into a combined summary table.
# ==============================================================================

# Load necessary libraries
library(dplyr)
library(tidyr)
library(modelsummary)
library(tinytable)
library(rlang)
library(pandoc)
library(AER) # For tobit() estimation

rm(list = ls())

# --- 1. Setup: Paths and Data ---
# Ensure these paths are correct for your system
path_github <- "C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos  <- "C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

# Load the recoded wide data
load(paste0(path_datos, "/Datos_islitas_recode.Rdata")) # expects object 'df'

# --- 2. Helper Functions ---

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

# Tobit Path Function for Stage 1 (Unknown Out-group)
coef_SA_T1_tobit <- function(data, R_start, R_end) {
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
  
  # Select variables and perform listwise deletion (mimicking lavaan standard behavior)
  vars <- c("belief_compliance_pm", "belief_compliance_union", "average_compliance_ini", 
            "confianza_pm", "conflicto_pm", "confianza_caleta", "conflicto_caleta", 
            "average_compliance_observed_ini_lag")
  df_mod <- na.omit(data[, vars])
  
  # Estimate Equation-by-Equation Tobit bounded between 0 and 1
  fit_pm    <- AER::tobit(belief_compliance_pm ~ confianza_pm + conflicto_pm, left = 0, right = 1, data = df_mod)
  fit_union <- AER::tobit(belief_compliance_union ~ confianza_caleta + conflicto_caleta, left = 0, right = 1, data = df_mod)
  fit_comp  <- AER::tobit(average_compliance_ini ~ belief_compliance_pm + belief_compliance_union + confianza_pm + conflicto_pm + confianza_caleta + conflicto_caleta + average_compliance_observed_ini_lag, left = 0, right = 1, data = df_mod)
  
  # Extract Coefficients, explicitly keeping Intercepts and Tobit Scale parameters
  extract_coefs <- function(fit, lhs_name) {
    cf <- summary(fit)$coefficients
    data.frame(
      lhs = lhs_name, op = "~", rhs = rownames(cf),
      est = cf[, "Estimate"], se = cf[, "Std. Error"], pvalue = cf[, "Pr(>|z|)"],
      stringsAsFactors = FALSE
    )
  }
  
  all_coefs <- bind_rows(
    extract_coefs(fit_pm, "belief_compliance_pm"),
    extract_coefs(fit_union, "belief_compliance_union"),
    extract_coefs(fit_comp, "average_compliance_ini")
  )
  
  # System Goodness of Fit measures
  ll_sys  <- as.numeric(logLik(fit_pm)) + as.numeric(logLik(fit_union)) + as.numeric(logLik(fit_comp))
  k_sys   <- length(coef(fit_pm)) + length(coef(fit_union)) + length(coef(fit_comp)) + 3 # +3 for Tobit scales
  n_obs   <- nrow(df_mod)
  aic_sys <- -2 * ll_sys + 2 * k_sys
  bic_sys <- -2 * ll_sys + log(n_obs) * k_sys
  
  # Pseudo R-Squared (McFadden) - Computed inline to avoid NSE scope errors
  fit_pm_null    <- AER::tobit(belief_compliance_pm ~ 1, left = 0, right = 1, data = df_mod)
  fit_union_null <- AER::tobit(belief_compliance_union ~ 1, left = 0, right = 1, data = df_mod)
  fit_comp_null  <- AER::tobit(average_compliance_ini ~ 1, left = 0, right = 1, data = df_mod)
  
  r2_pm    <- 1 - as.numeric(logLik(fit_pm)) / as.numeric(logLik(fit_pm_null))
  r2_union <- 1 - as.numeric(logLik(fit_union)) / as.numeric(logLik(fit_union_null))
  r2_comp  <- 1 - as.numeric(logLik(fit_comp)) / as.numeric(logLik(fit_comp_null))
  
  return(list(
    coefs = all_coefs,
    gof = c(ntotal = n_obs, logl = ll_sys, aic = aic_sys, bic = bic_sys),
    r2 = c(belief_compliance_pm = r2_pm, belief_compliance_union = r2_union, average_compliance_ini = r2_comp)
  ))
}

# Tobit Path Function for Stage 2 (Known Out-group)
coef_SA_T2_tobit <- function(data, R_start, R_end) {
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
  
  vars <- c("belief_compliance_pm", "belief_compliance_union", "average_compliance_ini", 
            "confianza_metat", "conflicto_metat", "confianza_caleta", "conflicto_caleta", 
            "average_compliance_observed_ini_lag")
  df_mod <- na.omit(data[, vars])
  
  # Estimate Equation-by-Equation Tobit bounded between 0 and 1
  fit_pm    <- AER::tobit(belief_compliance_pm ~ confianza_metat + conflicto_metat, left = 0, right = 1, data = df_mod)
  fit_union <- AER::tobit(belief_compliance_union ~ confianza_caleta + conflicto_caleta, left = 0, right = 1, data = df_mod)
  fit_comp  <- AER::tobit(average_compliance_ini ~ belief_compliance_pm + belief_compliance_union + confianza_metat + conflicto_metat + confianza_caleta + conflicto_caleta + average_compliance_observed_ini_lag, left = 0, right = 1, data = df_mod)
  
  # Extract Coefficients, explicitly keeping Intercepts and Tobit Scale parameters
  extract_coefs <- function(fit, lhs_name) {
    cf <- summary(fit)$coefficients
    data.frame(
      lhs = lhs_name, op = "~", rhs = rownames(cf),
      est = cf[, "Estimate"], se = cf[, "Std. Error"], pvalue = cf[, "Pr(>|z|)"],
      stringsAsFactors = FALSE
    )
  }
  
  all_coefs <- bind_rows(
    extract_coefs(fit_pm, "belief_compliance_pm"),
    extract_coefs(fit_union, "belief_compliance_union"),
    extract_coefs(fit_comp, "average_compliance_ini")
  )
  
  # System Goodness of Fit measures
  ll_sys  <- as.numeric(logLik(fit_pm)) + as.numeric(logLik(fit_union)) + as.numeric(logLik(fit_comp))
  k_sys   <- length(coef(fit_pm)) + length(coef(fit_union)) + length(coef(fit_comp)) + 3 
  n_obs   <- nrow(df_mod)
  aic_sys <- -2 * ll_sys + 2 * k_sys
  bic_sys <- -2 * ll_sys + log(n_obs) * k_sys
  
  # Pseudo R-Squared (McFadden) - Computed inline to avoid NSE scope errors
  fit_pm_null    <- AER::tobit(belief_compliance_pm ~ 1, left = 0, right = 1, data = df_mod)
  fit_union_null <- AER::tobit(belief_compliance_union ~ 1, left = 0, right = 1, data = df_mod)
  fit_comp_null  <- AER::tobit(average_compliance_ini ~ 1, left = 0, right = 1, data = df_mod)
  
  r2_pm    <- 1 - as.numeric(logLik(fit_pm)) / as.numeric(logLik(fit_pm_null))
  r2_union <- 1 - as.numeric(logLik(fit_union)) / as.numeric(logLik(fit_union_null))
  r2_comp  <- 1 - as.numeric(logLik(fit_comp)) / as.numeric(logLik(fit_comp_null))
  
  return(list(
    coefs = all_coefs,
    gof = c(ntotal = n_obs, logl = ll_sys, aic = aic_sys, bic = bic_sys),
    r2 = c(belief_compliance_pm = r2_pm, belief_compliance_union = r2_union, average_compliance_ini = r2_comp)
  ))
}

# --- 3. Run Models for Round 8 Only ---

cat("\nRunning Tobit System for Stage 1 (Unknown Out-group) Round 8...\n")
set.seed(478)
res_T1_R8 <- coef_SA_T1_tobit(df, 8, 8)

cat("\nRunning Tobit System for Stage 2 (Known Out-group) Round 8...\n")
set.seed(4523)
res_T2_R8 <- coef_SA_T2_tobit(df, 8, 8)

# --- 4. Extract and Label Results ---

# Dictionary for formatting variable names (order matters for row sorting)
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
  "(Intercept)"                         = "Constant",
  "Log(scale)"                          = "Log(scale)"
)

# Bind the custom coefficient outputs instead of using lavaan's parameterEstimates
all_coefs <- bind_rows(
  res_T1_R8$coefs %>% mutate(Stage = "Stage 1"),
  res_T2_R8$coefs %>% mutate(Stage = "Stage 2")
) %>%
  mutate(
    # Classify the Dependent Variable type based on LHS
    DV_Type = case_when(
      lhs == "average_compliance_ini" ~ "Compliance",
      lhs == "belief_compliance_pm" ~ "Beliefs Out-group",
      lhs == "belief_compliance_union" ~ "Beliefs In-group"
    ),
    Column_Name = paste(Stage, "DV:", DV_Type),
    
    # Assign human-readable Predictor names based on RHS
    Predictor = ifelse(rhs %in% names(var_labels), var_labels[rhs], rhs),
    
    # Assign Significance Stars
    Significance = case_when(
      pvalue < 0.001 ~ "***",
      pvalue < 0.01 ~ "**",
      pvalue < 0.05 ~ "*",
      pvalue < 0.1  ~ "†",
      TRUE ~ ""
    ),
    
    # Format Cell as: Estimate*** (SE)
    Formatted = sprintf("%.3f%s (%.3f)", est, Significance, se)
  )

# Set Predictor as a factor to retain a logical, non-alphabetical sorting order
all_coefs$Predictor <- factor(all_coefs$Predictor, levels = unique(var_labels))

# Pivot to wide format to create the 6 requested columns
wide_table <- all_coefs %>%
  select(Predictor, Column_Name, Formatted) %>%
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

gof_T1 <- res_T1_R8$gof
gof_T2 <- res_T2_R8$gof
r2_T1  <- res_T1_R8$r2
r2_T2  <- res_T2_R8$r2

gof_table <- data.frame(
  Predictor = c(
    "Num.Obs.",
    "Log Likelihood",
    "AIC",
    "BIC",
    "Pseudo R-squared" # Renamed slightly to clarify it's McFadden's R2
  ),
  `Stage 1 DV: Beliefs In-group` = c(
    as.character(round(gof_T1["ntotal"])),
    sprintf("%.3f", gof_T1["logl"]),
    sprintf("%.3f", gof_T1["aic"]),
    sprintf("%.3f", gof_T1["bic"]),
    sprintf("%.3f", r2_T1["belief_compliance_union"])
  ),
  `Stage 1 DV: Beliefs Out-group` = c(
    as.character(round(gof_T1["ntotal"])),
    sprintf("%.3f", gof_T1["logl"]),
    sprintf("%.3f", gof_T1["aic"]),
    sprintf("%.3f", gof_T1["bic"]),
    sprintf("%.3f", r2_T1["belief_compliance_pm"])
  ),
  `Stage 1 DV: Compliance` = c(
    as.character(round(gof_T1["ntotal"])),
    sprintf("%.3f", gof_T1["logl"]),
    sprintf("%.3f", gof_T1["aic"]),
    sprintf("%.3f", gof_T1["bic"]),
    sprintf("%.3f", r2_T1["average_compliance_ini"])
  ),
  `Stage 2 DV: Beliefs In-group` = c(
    as.character(round(gof_T2["ntotal"])),
    sprintf("%.3f", gof_T2["logl"]),
    sprintf("%.3f", gof_T2["aic"]),
    sprintf("%.3f", gof_T2["bic"]),
    sprintf("%.3f", r2_T2["belief_compliance_union"])
  ),
  `Stage 2 DV: Beliefs Out-group` = c(
    as.character(round(gof_T2["ntotal"])),
    sprintf("%.3f", gof_T2["logl"]),
    sprintf("%.3f", gof_T2["aic"]),
    sprintf("%.3f", gof_T2["bic"]),
    sprintf("%.3f", r2_T2["belief_compliance_pm"])
  ),
  `Stage 2 DV: Compliance` = c(
    as.character(round(gof_T2["ntotal"])),
    sprintf("%.3f", gof_T2["logl"]),
    sprintf("%.3f", gof_T2["aic"]),
    sprintf("%.3f", gof_T2["bic"]),
    sprintf("%.3f", r2_T2["average_compliance_ini"])
  ),
  check.names = FALSE
)

# Combine coefficients and Goodness of Fit statistics
final_table <- bind_rows(wide_table, gof_table)

# --- 6. Export to Word Table ---

# Define path for the output Word file
table_file_path_docx <- paste0(path_github, "Outputs/Tobit.docx")

# Export to Word using datasummary_df
datasummary_df(
  final_table,
  title = "Tobit Path Models for Compliance and Beliefs in Round 8",
  notes = c("† p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001",
            "Models are Tobit regressions constrained between 0 and 1. Standard errors are classical, and Pseudo R-squared is McFadden's."),
  output = table_file_path_docx
)

message("\nSuccess! Summary table saved to: ", table_file_path_docx)