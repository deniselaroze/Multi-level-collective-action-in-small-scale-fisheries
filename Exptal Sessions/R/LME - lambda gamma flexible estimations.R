# ==============================================================================
# Script: Full System Model (Separated Beliefs & Dynamic Gamma)
# Description: Fits the NLME for compliance using separate in-group and out-group 
# beliefs. Allows total conditional cooperation (\u03B3) to shift between stages 
# (gamma + gamma_diff) while ensuring that (\u03BB_in + \u03BB_out + \u03BB_obs) = 1.
# Includes OLS models for belief formation.
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
    # Standardize attitudinal variables based on original code naming
    confianza_pm     = as.numeric(scale(survey1.1.player.confianza_pm)),
    conflicto_pm     = as.numeric(scale(survey1.1.player.conflicto_pm)),
    confianza_caleta = as.numeric(scale(survey1.1.player.confianza_caleta)),
    conflicto_caleta = as.numeric(scale(survey1.1.player.conflicto_caleta)),
    
    # Map long-format data variables to original naming convention
    average_compliance_ini              = compliance_extraction_OA,
    average_compliance_observed_ini_lag = compliance_lag_extraction_others_OA_mean,
    belief_compliance_union             = compliance_beliefs_OA_caleta,
    belief_compliance_pm                = compliance_beliefs_OA_others,
    
    # Create a numeric 0/1 dummy for Stage 2 (required for nlme math)
    treatmentT2_num = ifelse(treatment == "T2", 1, 0)
  )

# --- 3. FIT THE COMPLIANCE MODEL (Dynamic \u03B3 NLME) ---
# Formula allows \u03B3 to change via gamma_diff, and ensures all 3 \u03BB weights sum to 1.
nlme_formula <- average_compliance_ini ~ (beta1 + beta1_diff * treatmentT2_num) + 
  beta2 * confianza_caleta + 
  beta3 * conflicto_caleta + 
  beta4 * confianza_pm + 
  beta5 * conflicto_pm + 
  (gamma + gamma_diff * treatmentT2_num) * ( 
    (lambda_in + delta_in * treatmentT2_num) * belief_compliance_union + 
      (lambda_out + delta_out * treatmentT2_num) * belief_compliance_pm + 
      (1 - (lambda_in + delta_in * treatmentT2_num) - (lambda_out + delta_out * treatmentT2_num)) * average_compliance_observed_ini_lag 
  )

fit_nlme_compliance <- nlme(
  nlme_formula,
  data = dfs_long_8,
  fixed = beta1 + beta1_diff + beta2 + beta3 + beta4 + beta5 + gamma + gamma_diff + lambda_in + lambda_out + delta_in + delta_out ~ 1,
  random = beta1 ~ 1 | gid.treat,
  start = c(beta1 = 0.5, beta1_diff = 0, 
            beta2 = 0, beta3 = 0, beta4 = 0, beta5 = 0, 
            gamma = 0.4, gamma_diff = 0, 
            lambda_in = 0.33, lambda_out = 0.33, delta_in = 0, delta_out = 0)
)

# --- 4. FIT THE PRIOR BELIEF MODELS (OLS) ---
# In-group beliefs formation
fit_prior_in <- lm(belief_compliance_union ~ confianza_caleta + conflicto_caleta, data = dfs_long_8)

# Out-group beliefs formation
fit_prior_out <- lm(belief_compliance_pm ~ confianza_pm + conflicto_pm, data = dfs_long_8)

# --- 5. EXTRACT AND FORMAT NLME COEFFICIENTS ---
coef_table_comp <- as.data.frame(summary(fit_nlme_compliance)$tTable)

clean_table_comp <- coef_table_comp %>%
  mutate(
    Parameter = rownames(coef_table_comp),
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

# --- 6. EXPORT ALL RESULTS ---

# Print to console
cat("\n=================================================================\n")
cat(" 1. COMPLIANCE EQUATION (Dynamic \u03B3 NLME) \n")
cat("=================================================================\n")
print(clean_table_comp)

cat("\n=================================================================\n")
cat(" 2. PRIOR BELIEF EQUATIONS (In-group and Out-group) \n")
cat("=================================================================\n")
print(modelsummary(list("In-group Beliefs" = fit_prior_in, 
                        "Out-group Beliefs" = fit_prior_out), 
                   stars = TRUE))

# Export Compliance Model
out_file_comp <- paste0(path_github, "Outputs/Dynamic_Gamma_NLME_Compliance_Round8.docx")
datasummary_df(
  clean_table_comp,
  title = "Compliance Equation: NLME Model (Separated Beliefs, Dynamic \u03B3)",
  notes = c("Significance codes: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001",
            "gamma = \u03B3_1 (Total Conditional Cooperation in Stage 1).",
            "gamma_diff = \u0394\u03B3 (Shift in Total Conditional Cooperation in Stage 2).",
            "lambda_in = \u03BB_in (Weight of In-group Beliefs in Stage 1).",
            "lambda_out = \u03BB_out (Weight of Out-group Beliefs in Stage 1).",
            "delta_in/out = \u0394 (Shift in weight of respective beliefs in Stage 2)."),
  align = "lc",
  output = out_file_comp
)

# Export Beliefs Models
out_file_beliefs <- paste0(path_github, "Outputs/Prior_Beliefs_Separated_Models_Round8.docx")
modelsummary(
  list("In-group Beliefs" = fit_prior_in, "Out-group Beliefs" = fit_prior_out),
  title = "Prior Belief Formation Equations (OLS)",
  stars = TRUE,
  estimate = "{estimate}{stars}",
  statistic = "std.error",
  output = out_file_beliefs
)

cat(sprintf("\nSuccess! Compliance model exported to: %s\n", out_file_comp))
cat(sprintf("Success! Beliefs models exported to: %s\n", out_file_beliefs))

# --- 7. RECOVER STRUCTURAL PARAMETERS (\u03B3 AND \u03BB) ---
cat("\n=================================================================\n")
cat(" RECOVERED PARAMETERS BY STAGE \n")
cat("=================================================================\n")
b_nlme <- fixef(fit_nlme_compliance)
V_nlme <- vcov(fit_nlme_compliance)

recovered_tests <- list(
  "\u03B3_Total (Stage 2)" = "gamma + gamma_diff",
  "\u03BB_Observed (Stage 1)" = "1 - lambda_in - lambda_out",
  "\u03BB_In-group (Stage 2)" = "lambda_in + delta_in",
  "\u03BB_Out-group (Stage 2)" = "lambda_out + delta_out",
  "\u03BB_Observed (Stage 2)" = "1 - (lambda_in + delta_in) - (lambda_out + delta_out)",
  "\u0394 \u03BB_Observed (S2 - S1)" = "-(delta_in + delta_out)"
)

recovered_results <- lapply(names(recovered_tests), function(test_name) {
  res <- deltaMethod(b_nlme, recovered_tests[[test_name]], vcov. = V_nlme)
  data.frame(
    Parameter = test_name,
    Estimate = sprintf("%.3f", res$Estimate),
    SE = sprintf("%.3f", res$SE),
    P_value = sprintf("%.3f", 2 * pnorm(-abs(res$Estimate / res$SE)))
  )
})

recovered_table <- bind_rows(recovered_results)
print(recovered_table, row.names = FALSE)