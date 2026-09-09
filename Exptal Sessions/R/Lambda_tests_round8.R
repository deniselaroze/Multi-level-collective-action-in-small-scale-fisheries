# ==============================================================================
# Script: OLS Model & Directional Lambda Differences (Round 8, Stage 2 vs Stage 1)
# Description: Tests directional hypotheses regarding the relative weights (lambdas) 
# of behavioral mechanisms shifting when the out-group becomes known. 
# Restricted to Round 8 data with group-clustered standard errors.
# ==============================================================================

library(dplyr)
library(tidyr)
library(car)
library(sandwich)
library(lmtest)
library(modelsummary)
library(flextable)

# --- 1. SETUP PATHS AND LOAD DATA ---
rm(list=ls())
path_github <- "C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos  <- "C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"
setwd(path_github)

# Load the long format data
load(paste0(path_datos, "/Datos_islitas_long.Rdata"))

# --- 2. DATA PREPARATION ---
# Standardize attitudinal variables and filter for ONLY Round 8
dfs_last_round <- dfs_long %>%
  mutate(
    confianza_pm_scaled      = as.numeric(scale(survey1.1.player.confianza_pm)),
    conflicto_pm_scaled      = as.numeric(scale(survey1.1.player.conflicto_pm)),
    confianza_caleta_scaled  = as.numeric(scale(survey1.1.player.confianza_caleta)),
    conflicto_caleta_scaled  = as.numeric(scale(survey1.1.player.conflicto_caleta)),
    round_num                = as.numeric(as.character(round))
  ) %>%
  filter(round_num == 8) # Restrict to final round of each stage

# --- 3. FIT THE OLS MODEL & EXTRACT CLUSTERED VCOV ---
# Standard OLS Model interacting core behavioral mechanisms with Stage (treatment)
lambda_mod_ols <- compliance_extraction_OA ~
  (compliance_beliefs_OA_caleta + compliance_beliefs_OA_others + compliance_lag_extraction_others_OA_mean) * treatment +
  confianza_pm_scaled + conflicto_pm_scaled +
  confianza_caleta_scaled + conflicto_caleta_scaled

fit_ols <- lm(lambda_mod_ols, data = dfs_last_round)

# Calculate Group-Clustered Variance-Covariance Matrix (gid.treat)
V_clustered <- vcovCL(fit_ols, cluster = ~gid.treat)

# --- 4. TEST DIRECTIONAL LAMBDA DIFFERENCES (STAGE 2 - STAGE 1) ---

b <- coef(fit_ols)
V <- V_clustered

safe_names <- gsub(":", "_X_", names(b))
names(b) <- safe_names
colnames(V) <- rownames(V) <- safe_names

c_in  <- "compliance_beliefs_OA_caleta"
c_out <- "compliance_beliefs_OA_others"
c_obs <- "compliance_lag_extraction_others_OA_mean"

d_in  <- "compliance_beliefs_OA_caleta_X_treatmentT2"
d_out <- "compliance_beliefs_OA_others_X_treatmentT2"
d_obs <- "compliance_lag_extraction_others_OA_mean_X_treatmentT2"

# Redefined formula: Stage 2 Lambda - Stage 1 Lambda
generate_lambda_test_directional <- function(base_target, diff_target) {
  sprintf(
    "((%s + %s) / ((%s + %s) + (%s + %s) + (%s + %s))) - (%s / (%s + %s + %s))",
    base_target, diff_target,
    c_in, d_in, c_out, d_out, c_obs, d_obs,
    base_target,
    c_in, c_out, c_obs
  )
}

test_lambda_in  <- generate_lambda_test_directional(c_in, d_in)
test_lambda_out <- generate_lambda_test_directional(c_out, d_out)
test_lambda_obs <- generate_lambda_test_directional(c_obs, d_obs)

diff_in  <- deltaMethod(b, test_lambda_in, vcov. = V)
diff_out <- deltaMethod(b, test_lambda_out, vcov. = V)
diff_obs <- deltaMethod(b, test_lambda_obs, vcov. = V)

# --- 5. COMPUTE ONE-SIDED P-VALUES FOR DIRECTIONAL HYPOTHESES ---

# Extract Estimates and SEs
est_in <- diff_in$Estimate
se_in  <- diff_in$SE
est_out <- diff_out$Estimate
se_out  <- diff_out$SE
est_obs <- diff_obs$Estimate
se_obs  <- diff_obs$SE

# Calculate Z-scores
z_in  <- est_in / se_in
z_out <- est_out / se_out
z_obs <- est_obs / se_obs

# Calculate one-sided p-values based on specific hypotheses
# H1: In-group decreases (Stage 2 - Stage 1 < 0)
pval_in  <- pnorm(z_in, lower.tail = TRUE)
# H1: Out-group increases (Stage 2 - Stage 1 > 0)
pval_out <- pnorm(z_out, lower.tail = FALSE)
# H1: Observed decreases (Stage 2 - Stage 1 < 0)
pval_obs <- pnorm(z_obs, lower.tail = TRUE)

# --- 6. COMPILE RESULTS TABLE ---
hypothesis_table <- data.frame(
  Mechanism = c(
    "Lambda 1: Prior beliefs (in-group)", 
    "Lambda 2: Prior beliefs (out-group)", 
    "Lambda 3: Observed compliance (t-1)"
  ),
  `Directional_Hypothesis` = c(
    "H1: \u0394 < 0", 
    "H1: \u0394 > 0", 
    "H1: \u0394 < 0"
  ),
  `Estimate_Diff` = sprintf("%.3f", c(est_in, est_out, est_obs)),
  `Std_Error` = sprintf("%.3f", c(se_in, se_out, se_obs)),
  `Z_value` = sprintf("%.3f", c(z_in, z_out, z_obs)),
  `One_Sided_P_Value` = sprintf("%.3f", c(pval_in, pval_out, pval_obs)),
  check.names = FALSE
)

# Print cleanly to console
cat("\n=================================================================\n")
cat(" DIRECTIONAL HYPOTHESIS TESTS (Round 8: Stage 2 - Stage 1) \n")
cat("=================================================================\n\n")
print(hypothesis_table, row.names = FALSE)

# --- 7. EXPORT TO WORD ---
out_file <- paste0(path_github, "Outputs/Lambda_Directional_Tests_Round8.docx")

datasummary_df(
  hypothesis_table,
  title = "One-Sided Tests for Shifts in Behavioral Mechanism Weights (Round 8: Stage 2 \u2212 Stage 1)",
  notes = c("Estimates represent the structural shift in lambda proportions calculated via the delta method.",
            "Standard errors are clustered at the group level (gid.treat).",
            "Directional Hypotheses refer to shifts when out-group associations are revealed."),
  align = "llcccc",
  output = out_file
)

cat(sprintf("\nSuccess! Results table exported to: %s\n", out_file))