# ==============================================================================
# Script: Mixed-Effects Model & Structural Shifts in Conditional Cooperation
# Description: Evaluates raw marginal effects (coefficients) for ROUND 8 ONLY.
# Participant random effects are removed to prevent singular fits. Includes 
# direct comparisons between In-group (C1) and Out-group (C2) mechanisms.
# Attitudinal variables (trust/conflict) are standardized and added as controls.
# ==============================================================================

library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(car)
library(modelsummary)

# --- 1. SETUP PATHS AND LOAD DATA ---
rm(list=ls())
path_github <- "C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos  <- "C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"
setwd(path_github)

load(paste0(path_datos, "/Datos_islitas_long.Rdata"))

# --- 2. DATA PREPARATION & FILTERING ---
# Standardize the attitudinal variables across the full dataset first
dfs_long <- dfs_long %>%
  mutate(
    confianza_pm_scaled      = as.numeric(scale(survey1.1.player.confianza_pm)),
    conflicto_pm_scaled      = as.numeric(scale(survey1.1.player.conflicto_pm)),
    confianza_caleta_scaled  = as.numeric(scale(survey1.1.player.confianza_caleta)),
    conflicto_caleta_scaled  = as.numeric(scale(survey1.1.player.conflicto_caleta))
  )

# Replace 'period' with your actual dataset variable name for the rounds
dfs_long_8 <- dfs_long %>%
  filter(round == 8) 

# --- 3. FIT THE MIXED-EFFECTS MODEL ---
ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))

# Participant random effect removed; only group clustering remains
# Trust and conflict standardized variables are added as linear controls
lambda_mod <- compliance_extraction_OA ~
  (compliance_beliefs_OA_caleta + compliance_beliefs_OA_others + compliance_lag_extraction_others_OA_mean) * treatment +
  confianza_pm_scaled + conflicto_pm_scaled +
  confianza_caleta_scaled + conflicto_caleta_scaled +
  (1 | gid.treat)

fit_mixed <- lmer(lambda_mod, data = dfs_long_8, control = ctrl, REML = FALSE)

# --- 4. DEFINE VARIABLES FOR DELTA METHOD ---
b <- fixef(fit_mixed)
V <- vcov(fit_mixed)

safe_names <- gsub(":", "_X_", names(b))
names(b) <- safe_names
colnames(V) <- rownames(V) <- safe_names

# Base coefficients (Stage 1) -> C1, C2, C3
c_in  <- "compliance_beliefs_OA_caleta"
c_out <- "compliance_beliefs_OA_others"
c_obs <- "compliance_lag_extraction_others_OA_mean"

# Interaction coefficients (Stage 2 differences) -> d1, d2, d3
d_in  <- "compliance_beliefs_OA_caleta_X_treatmentT2"
d_out <- "compliance_beliefs_OA_others_X_treatmentT2"
d_obs <- "compliance_lag_extraction_others_OA_mean_X_treatmentT2"

# --- 5. DEFINE THE TARGETED TESTS (RAW COEFFICIENTS) ---
tests <- list(
  # --- 1. In-Group Beliefs ---
  "C1 In-group Beliefs (Stage 1)" = c_in,
  "C1 In-group Beliefs (Stage 2)" = sprintf("%s + %s", c_in, d_in),
  "\u0394 C1 Diff (S2 - S1)"      = d_in,
  
  # --- 2. Out-Group Beliefs ---
  "C2 Out-group Beliefs (Stage 1)" = c_out,
  "C2 Out-group Beliefs (Stage 2)" = sprintf("%s + %s", c_out, d_out),
  "\u0394 C2 Diff (S2 - S1)"       = d_out,
  
  # --- 3. Observed Compliance ---
  "C3 Observed Compliance (Stage 1)" = c_obs,
  "C3 Observed Compliance (Stage 2)" = sprintf("%s + %s", c_obs, d_obs),
  "\u0394 C3 Diff (S2 - S1)"         = d_obs,
  
  # --- 4. Structural Comparisons: (C1 + C2) - C3 ---
  "(C1 + C2) - C3 (Stage 1)" = sprintf("(%s + %s) - %s", c_in, c_out, c_obs),
  "(C1 + C2) - C3 (Stage 2)" = sprintf("((%s + %s) + (%s + %s)) - (%s + %s)", c_in, d_in, c_out, d_out, c_obs, d_obs),
  "DiD: [(C1 + C2) - C3] (S2 - S1)" = sprintf("(((%s + %s) + (%s + %s)) - (%s + %s)) - ((%s + %s) - %s)", 
                                              c_in, d_in, c_out, d_out, c_obs, d_obs, 
                                              c_in, c_out, c_obs),
  
  # --- 5. Structural Comparisons: C1 vs C2 (In-group vs Out-group) ---
  "C1 - C2 (Stage 1)" = sprintf("%s - %s", c_in, c_out),
  "C1 - C2 (Stage 2)" = sprintf("(%s + %s) - (%s + %s)", c_in, d_in, c_out, d_out),
  "DiD: [C1 - C2] (S2 - S1)" = sprintf("((%s + %s) - (%s + %s)) - (%s - %s)", 
                                       c_in, d_in, c_out, d_out, 
                                       c_in, c_out)
)

# --- 6. EXECUTE TESTS & BUILD TABLE ---
results_list <- lapply(names(tests), function(test_name) {
  res <- deltaMethod(b, tests[[test_name]], vcov. = V)
  data.frame(
    Hypothesis = test_name,
    Estimate = res$Estimate,
    SE = res$SE,
    Z_value = res$Estimate / res$SE,
    P_value = 2 * pnorm(-abs(res$Estimate / res$SE)) # Two-Sided P-value
  )
})

hypothesis_table <- bind_rows(results_list) %>%
  mutate(
    Estimate = sprintf("%.3f", Estimate),
    SE = sprintf("%.3f", SE),
    Z_value = sprintf("%.3f", Z_value),
    P_value = sprintf("%.3f", P_value)
  )

# --- 7. PRINT AND EXPORT ---
cat("\n=================================================================\n")
cat(" CROSS-STAGE DIFFERENCES (RAW COEFFICIENTS) - ROUND 8 ONLY \n")
cat("=================================================================\n\n")
print(hypothesis_table, row.names = FALSE)

out_file <- paste0(path_github, "Outputs/Cross_Stage_Coefficient_Tests_Round8.docx")

datasummary_df(
  hypothesis_table,
  title = "Absolute Effects and Shifts in Raw Mechanism Coefficients (Round 8)",
  notes = c("Estimates calculated via linear combinations from the mixed-effects model.",
            "Sample filtered to Round 8 only. Participant random effects removed.",
            "Attitudinal controls (trust/conflict) were included in the estimation.",
            "Stage 2 effects represent the sum of the baseline coefficient and the interaction term.",
            "\u0394 represents the interaction term (Stage 2 - Stage 1)."),
  align = "llccc",
  output = out_file
)

cat(sprintf("\nSuccess! Results table exported to: %s\n", out_file))