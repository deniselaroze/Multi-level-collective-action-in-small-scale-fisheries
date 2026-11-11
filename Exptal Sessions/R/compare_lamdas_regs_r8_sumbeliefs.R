# ==============================================================================
# Script: Mixed-Effects Model with Consolidated Prior Beliefs
# Description: Averages in-group and out-group beliefs into a single "Prior 
# beliefs" variable using manuscript notation. Evaluates Lambdas for Round 8 
# using LME and outputs both the base regression model and structural differences.
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
dfs_long <- dfs_long %>%
  mutate(
    # Standardize attitudinal variables to match manuscript notation
    trustOutGroup    = as.numeric(scale(survey1.1.player.confianza_pm)),
    conflictOutGroup = as.numeric(scale(survey1.1.player.conflicto_pm)),
    trustInGroup     = as.numeric(scale(survey1.1.player.confianza_caleta)),
    conflictInGroup  = as.numeric(scale(survey1.1.player.conflicto_caleta)),
    
    # Rename core behavioral variables to match manuscript notation
    compliance         = compliance_extraction_OA,
    previousCompliance = compliance_lag_extraction_others_OA_mean,
    
    # Consolidate Prior Beliefs: (In-group + Out-group) / 2
    priorBeliefSum = (compliance_beliefs_OA_caleta + compliance_beliefs_OA_others) / 2
  )

# Filter for round 8
dfs_long_8 <- dfs_long %>%
  filter(round == 8) 

# --- 3. FIT THE MIXED-EFFECTS MODEL ---
ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))

# Model using the consolidated beliefs variable and manuscript notation. 
lambda_mod <- compliance ~
  (priorBeliefSum + previousCompliance) * treatment +
  trustInGroup + conflictInGroup +
  trustOutGroup + conflictOutGroup +
  (1 | gid.treat)

fit_mixed <- lmer(lambda_mod, data = dfs_long_8, control = ctrl, REML = FALSE)

# --- 4. DEFINE VARIABLES FOR DELTA METHOD ---
b <- fixef(fit_mixed)
V <- vcov(fit_mixed)

safe_names <- gsub(":", "_X_", names(b))
names(b) <- safe_names
colnames(V) <- rownames(V) <- safe_names

# Base coefficients (Stage 1) -> C_bel, C_obs
c_bel <- "priorBeliefSum"
c_obs <- "previousCompliance"

# Interaction coefficients (Stage 2 differences) -> d_bel, d_obs
d_bel <- "priorBeliefSum_X_treatmentT2"
d_obs <- "previousCompliance_X_treatmentT2"

# Gammas (Total effect denominators: Sum of the 2 mechanisms)
G1 <- sprintf("(%s + %s)", c_bel, c_obs)
G2 <- sprintf("((%s + %s) + (%s + %s))", c_bel, d_bel, c_obs, d_obs)

# --- 5. DEFINE THE TARGETED TESTS (LAMBDAS) ---
tests <- list(
  # --- 1. Total Gamma ---
  "\u0394 Gamma (S2 - S1)" = sprintf("%s - %s", G2, G1),
  
  # --- 2. Consolidated Prior Beliefs (\u03BB_bel) ---
  "\u03BB_Beliefs (Stage 1)" = sprintf("(%s) / %s", c_bel, G1),
  "\u03BB_Beliefs (Stage 2)" = sprintf("(%s + %s) / %s", c_bel, d_bel, G2),
  "\u0394 \u03BB_Beliefs Diff (S2 - S1)" = sprintf("((%s + %s) / %s) - (%s / %s)", c_bel, d_bel, G2, c_bel, G1),
  
  # --- 3. Observed Compliance (\u03BB_obs) ---
  "\u03BB_Observed (Stage 1)" = sprintf("(%s) / %s", c_obs, G1),
  "\u03BB_Observed (Stage 2)" = sprintf("(%s + %s) / %s", c_obs, d_obs, G2),
  "\u0394 \u03BB_Observed Diff (S2 - S1)" = sprintf("((%s + %s) / %s) - (%s / %s)", c_obs, d_obs, G2, c_obs, G1),
  
  # --- 4. Structural Comparisons: Beliefs vs Observed ---
  "\u03BB_Beliefs - \u03BB_Observed (Stage 1)" = sprintf("(%s - %s) / %s", c_bel, c_obs, G1),
  "\u03BB_Beliefs - \u03BB_Observed (Stage 2)" = sprintf("((%s + %s) - (%s + %s)) / %s", c_bel, d_bel, c_obs, d_obs, G2),
  "DiD: [Beliefs - Observed] (S2 - S1)" = sprintf("(((%s + %s) - (%s + %s)) / %s) - ((%s - %s) / %s)", 
                                                  c_bel, d_bel, c_obs, d_obs, G2, 
                                                  c_bel, c_obs, G1)
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

# --- 7. EXPORT REGRESSION MODEL TABLE ---
cat("\n=================================================================\n")
cat(" FITTING MIXED-EFFECTS REGRESSION MODEL (ROUND 8) \n")
cat("=================================================================\n")
print(summary(fit_mixed))

out_file_reg <- paste0(path_github, "Outputs/Regression_Model_Round8.docx")
modelsummary(
  fit_mixed,
  title = "Mixed-Effects Regression Model (Round 8)",
  stars = TRUE,
  estimate = "{estimate}{stars}",
  statistic = "std.error",
  notes = list("Significance codes: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"),
  output = out_file_reg
)
cat(sprintf("\nSuccess! Regression model table exported to: %s\n", out_file_reg))

# --- 8. EXPORT LAMBDA SHIFTS TABLE ---
cat("\n=================================================================\n")
cat(" CONSOLIDATED LAMBDAS (ROUND 8 ONLY - LME) \n")
cat("=================================================================\n\n")
print(hypothesis_table, row.names = FALSE)

out_file_lambda <- paste0(path_github, "Outputs/Consolidated_Lambda_Tests_Round8_v2.docx")
datasummary_df(
  hypothesis_table,
  title = "Absolute Effects and Shifts in Lambda Proportions (Consolidated Beliefs)",
  notes = c("Estimates calculated via delta method from the LME model.",
            "In-group and out-group beliefs were averaged into a single mechanism.",
            "\u0394 represents the shift in proportional weight between stages (Stage 2 - Stage 1)."),
  align = "llccc",
  output = out_file_lambda
)
cat(sprintf("\nSuccess! Lambda results table exported to: %s\n", out_file_lambda))