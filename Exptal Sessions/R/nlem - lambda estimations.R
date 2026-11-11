# ==============================================================================
# Script: Restricted Non-Linear Mixed-Effects Model (NLME)
# Description: Fits the structurally restricted model where \u03BB and (1-\u03BB) 
# sum to 1, and total conditional cooperation (\u03B3) is constant across stages.
# Bypasses modelsummary() error by manually extracting coefficients.
# ==============================================================================

library(dplyr)
library(tidyr)
library(nlme)
library(car)
library(modelsummary)

# --- 1. SETUP PATHS AND LOAD DATA ---
rm(list=ls())
path_github <- "C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos  <- "C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"
setwd(path_github)

load(paste0(path_datos, "/Datos_islitas_long.Rdata"))

# --- 2. DATA PREPARATION & FILTERING ---
dfs_long_8 <- dfs_long %>%
  filter(round == 8) %>%
  mutate(
    # Standardize attitudinal variables
    trustOutGroup    = as.numeric(scale(survey1.1.player.confianza_pm)),
    conflictOutGroup = as.numeric(scale(survey1.1.player.conflicto_pm)),
    trustInGroup     = as.numeric(scale(survey1.1.player.confianza_caleta)),
    conflictInGroup  = as.numeric(scale(survey1.1.player.conflicto_caleta)),
    
    # Rename variables for clarity
    compliance         = compliance_extraction_OA,
    previousCompliance = compliance_lag_extraction_others_OA_mean,
    
    # Consolidate Prior Beliefs
    priorBeliefSum = (compliance_beliefs_OA_caleta + compliance_beliefs_OA_others) / 2,
    
    # Create a numeric 0/1 dummy for Stage 2 (required for nlme math)
    treatmentT2_num = ifelse(treatment == "T2", 1, 0)
  )

# --- 3. FIT THE NON-LINEAR MIXED-EFFECTS MODEL (NLME) ---
nlme_formula <- compliance ~ (beta1 + beta1_diff * treatmentT2_num) + 
  beta2 * trustInGroup + 
  beta3 * conflictInGroup + 
  beta4 * trustOutGroup + 
  beta5 * conflictOutGroup + 
  gamma * ( (lambda1 + delta * treatmentT2_num) * priorBeliefSum + 
              (1 - (lambda1 + delta * treatmentT2_num)) * previousCompliance )

fit_nlme <- nlme(
  nlme_formula,
  data = dfs_long_8,
  fixed = beta1 + beta1_diff + beta2 + beta3 + beta4 + beta5 + gamma + lambda1 + delta ~ 1,
  random = beta1 ~ 1 | gid.treat,
  start = c(beta1 = 0.5, beta1_diff = -0.1, 
            beta2 = 0, beta3 = 0, beta4 = 0, beta5 = 0, 
            gamma = 0.5, lambda1 = 0.5, delta = 0)
)

# --- 4. MANUALLY EXTRACT AND FORMAT COEFFICIENTS ---
cat("\n=================================================================\n")
cat(" RESTRICTED NON-LINEAR MIXED-EFFECTS MODEL (ROUND 8) \n")
cat("=================================================================\n")
print(summary(fit_nlme))

# Extract the fixed effects table
coef_table <- as.data.frame(summary(fit_nlme)$tTable)

# Format with significance stars
clean_table <- coef_table %>%
  mutate(
    Parameter = rownames(coef_table),
    Estimate_raw = Value,
    SE_raw = `Std.Error`,
    p_val = `p-value`,
    stars = case_when(
      p_val < 0.001 ~ "***",
      p_val < 0.01  ~ "**",
      p_val < 0.05  ~ "*",
      p_val < 0.1   ~ "+",
      TRUE          ~ ""
    ),
    `Estimate (SE)` = sprintf("%.3f%s (%.3f)", Estimate_raw, stars, SE_raw)
  ) %>%
  dplyr::select(Parameter, `Estimate (SE)`)

# --- 5. EXPORT REGRESSION MODEL TABLE ---
out_file_reg <- paste0(path_github, "Outputs/Restricted_NLME_Model_Round8.docx")

datasummary_df(
  clean_table,
  title = "Restricted Non-Linear Mixed-Effects Model (\u03B3 is constant, \u03BB sums to 1)",
  notes = c("Significance codes: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001",
            "gamma = \u03B3 (Total Conditional Cooperation).",
            "lambda1 = \u03BB_1 (Weight of Beliefs in Stage 1).",
            "delta = \u0394 (Shift in weight of Beliefs in Stage 2)."),
  align = "lc",
  output = out_file_reg
)
cat(sprintf("\nSuccess! Restricted NLME model exported to: %s\n", out_file_reg))

# --- 6. TEST STAGE 2 LAMBDA (Optional Delta Method) ---
cat("\n--- Recovering \u03BB_2 (Stage 2 Weight of Beliefs) ---\n")
b_nlme <- fixef(fit_nlme)
V_nlme <- vcov(fit_nlme)
lambda2_test <- deltaMethod(b_nlme, "lambda1 + delta", vcov. = V_nlme)
print(lambda2_test)