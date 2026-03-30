# ==============================================================================
# Script: Tobit Path Analysis Round 8 (with Parameter Recovery)
# Description: Runs Tobit (Censored) Path models for compliance decisions [0,1]
# specifically in Round 8. Recovers Gamma and Lambda parameters using the 
# Delta Method for standard errors.
# ==============================================================================

# Load necessary libraries
library(dplyr)
library(tidyr)
library(modelsummary)
library(tinytable)
library(rlang)
library(pandoc)
library(AER) # For tobit() estimation
library(msm) # For Delta Method (deltamethod)

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

# General Function to extract and recover parameters from a Tobit model
recover_tobit_params <- function(fit, x_vars, lhs_name) {
  # Get regression coefficients (excludes Log(scale))
  beta_all <- coef(fit)
  
  # Get VCV matrix and subset it to match regression coefficients only
  # AER::tobit vcov includes the scale parameter at the end, so we trim it.
  v_cov_full <- vcov(fit)
  v_cov <- v_cov_full[names(beta_all), names(beta_all)]
  
  # Identify positions of our x variables in the coef vector
  # Note: deltamethod uses x1, x2, ... based on the order in beta_all
  idx_pos <- match(x_vars, names(beta_all))
  
  # 1. Calculate Gamma (Sum)
  gamma_val <- sum(beta_all[idx_pos])
  
  # Delta method string for Gamma: "x1 + x2 + x3" mapping to correct positions
  gamma_form <- paste0("~ ", paste0("x", idx_pos, collapse = " + "))
  se_gamma <- deltamethod(as.formula(gamma_form), beta_all, v_cov)
  
  # 2. Calculate Lambdas (Weights)
  lambdas_list <- lapply(seq_along(idx_pos), function(i) {
    curr_idx <- idx_pos[i]
    # Lambda Formula: xi / (x1 + x2 + x3)
    denom <- paste0("(", paste0("x", idx_pos, collapse = " + "), ")")
    lambda_form <- as.formula(paste0("~ x", curr_idx, " / ", denom))
    
    est_l <- beta_all[curr_idx] / gamma_val
    se_l  <- deltamethod(lambda_form, beta_all, v_cov)
    
    return(data.frame(rhs = paste0("Lambda_", i), est = est_l, se = se_l))
  })
  
  lambdas_df <- do.call(rbind, lambdas_list)
  
  # Combine into a result df
  res <- rbind(
    data.frame(rhs = "Gamma", est = gamma_val, se = se_gamma),
    lambdas_df
  ) %>%
    mutate(
      lhs = lhs_name,
      op = ":=",
      z = est / se,
      pvalue = 2 * (1 - pnorm(abs(z)))
    )
  
  return(res)
}

# Tobit Path Function wrapper
run_tobit_system <- function(data, stage_num) {
  # Data Prep (logic consolidated from Stage 1/2)
  if(stage_num == 1) {
    prefix <- "T1juegoalgas"
    out_belief_col <- "beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini"
    in_belief_col <- "beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini"
    out_trust <- "survey1.1.player.confianza_pm"
    out_conflict <- "survey1.1.player.conflicto_pm"
    obs_col <- "T1_extraccion_otros_libre"
  } else {
    prefix <- "T2juegoalgas"
    out_belief_col <- "beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini"
    in_belief_col <- "beliefsT2inicial.1.player.T2_belief_caleta_ini"
    out_trust <- "survey2.1.player.confianza_caleta_conocida_mean"
    out_conflict <- "survey2.1.player.conflicto_caleta_conocida_mean"
    obs_col <- "T2_extraccion_otros_metat"
  }
  
  cols <- declare_get_columns(prefix, ifelse(stage_num==1, "T1_extraccion_libre", "T2_extraccion_metat"), 8, 8, data)
  data$average_extraction_ini <- rowMeans(data[, cols, drop = FALSE], na.rm = TRUE)
  data$average_compliance_ini <- 1 - data$average_extraction_ini / 50
  data$belief_compliance_pm   <- 1 - data[, out_belief_col] / 50
  data$belief_compliance_union<- 1 - data[, in_belief_col] / 50
  
  data$confianza_caleta <- as.numeric(scale(data$survey1.1.player.confianza_caleta))
  data$conflicto_caleta <- as.numeric(scale(data$survey1.1.player.conflicto_caleta))
  data$confianza_pm     <- as.numeric(scale(data[, out_trust]))
  data$conflicto_pm     <- as.numeric(scale(data[, out_conflict]))
  
  cols_obs <- declare_get_columns(prefix, obs_col, 7, 7, data)
  data$average_compliance_observed_ini_lag <- 1 - (rowMeans(data[, cols_obs, drop = FALSE], na.rm = TRUE) / 150)
  
  vars <- c("belief_compliance_pm", "belief_compliance_union", "average_compliance_ini", 
            "confianza_pm", "conflicto_pm", "confianza_caleta", "conflicto_caleta", 
            "average_compliance_observed_ini_lag")
  df_mod <- na.omit(data[, vars])
  
  # Models
  fit_pm    <- AER::tobit(belief_compliance_pm ~ confianza_pm + conflicto_pm, left = 0, right = 1, data = df_mod)
  fit_union <- AER::tobit(belief_compliance_union ~ confianza_caleta + conflicto_caleta, left = 0, right = 1, data = df_mod)
  fit_comp  <- AER::tobit(average_compliance_ini ~ belief_compliance_pm + belief_compliance_union + 
                            confianza_pm + conflicto_pm + confianza_caleta + conflicto_caleta + 
                            average_compliance_observed_ini_lag, left = 0, right = 1, data = df_mod)
  
  # Extract Base Coefs
  extract_cf <- function(fit, lhs_name) {
    cf <- summary(fit)$coefficients
    data.frame(lhs = lhs_name, op = "~", rhs = rownames(cf), est = cf[,1], se = cf[,2], pvalue = cf[,4])
  }
  
  base_coefs <- bind_rows(extract_cf(fit_pm, "belief_compliance_pm"), 
                          extract_cf(fit_union, "belief_compliance_union"), 
                          extract_cf(fit_comp, "average_compliance_ini"))
  
  # Recover Gamma/Lambdas for Compliance equation
  x_vars <- c("belief_compliance_pm", "belief_compliance_union", "average_compliance_observed_ini_lag")
  recovered <- recover_tobit_params(fit_comp, x_vars, "average_compliance_ini")
  
  # GOF
  ll_sys <- as.numeric(logLik(fit_pm) + logLik(fit_union) + logLik(fit_comp))
  
  return(list(coefs = bind_rows(base_coefs, recovered), n = nrow(df_mod), ll = ll_sys, 
              r2 = c(pm = 1-as.numeric(logLik(fit_pm)/logLik(update(fit_pm, .~1))),
                     un = 1-as.numeric(logLik(fit_union)/logLik(update(fit_union, .~1))),
                     cp = 1-as.numeric(logLik(fit_comp)/logLik(update(fit_comp, .~1))))))
}

# --- 3. Execution ---
res1 <- run_tobit_system(df, 1)
res2 <- run_tobit_system(df, 2)

# --- 4. Table Formatting ---

# Update the variable labels and set factor levels for row order
# Row Order: Trust -> Conflict -> Prior Beliefs -> Observed -> Constant -> Gamma -> Lambdas
pred_order <- c(
  "Trust Out-group", "Trust In-group",
  "Conflict Out-group", "Conflict In-group",
  "Prior Beliefs Out-group", "Prior Beliefs In-group",
  "Observed Compliance (round 7)",
  "Constant",
  "Sum Conditional Cooperation (Gamma)",
  "Lambda 1 (Weight Beliefs Out-group)",
  "Lambda 2 (Weight Beliefs In-group)",
  "Lambda 3 (Weight Observed Comp.)"
)

var_labels <- c(
  "confianza_pm"                        = "Trust Out-group",
  "confianza_caleta"                    = "Trust In-group",
  "conflicto_pm"                        = "Conflict Out-group",
  "conflicto_caleta"                    = "Conflict In-group",
  "belief_compliance_pm"                = "Prior Beliefs Out-group",
  "belief_compliance_union"             = "Prior Beliefs In-group",
  "average_compliance_observed_ini_lag" = "Observed Compliance (round 7)",
  "(Intercept)"                         = "Constant",
  "Gamma"                               = "Sum Conditional Cooperation (Gamma)",
  "Lambda_1"                            = "Lambda 1 (Weight Beliefs Out-group)",
  "Lambda_2"                            = "Lambda 2 (Weight Beliefs In-group)",
  "Lambda_3"                            = "Lambda 3 (Weight Observed Comp.)"
)

all_results <- bind_rows(res1$coefs %>% mutate(St = "Stage 1"), res2$coefs %>% mutate(St = "Stage 2")) %>%
  mutate(
    DV_Type = case_when(lhs == "average_compliance_ini" ~ "Compliance",
                        lhs == "belief_compliance_pm" ~ "Beliefs Out-group",
                        lhs == "belief_compliance_union" ~ "Beliefs In-group"),
    Col = paste(St, "DV:", DV_Type),
    Pred = ifelse(rhs %in% names(var_labels), var_labels[rhs], rhs),
    Stars = case_when(pvalue < .001 ~ "***", pvalue < .01 ~ "**", pvalue < .05 ~ "*", pvalue < .1 ~ "†", TRUE ~ ""),
    Fmt = sprintf("%.3f%s (%.3f)", est, Stars, se)
  ) %>%
  filter(Pred %in% var_labels)

# Pivot and Build Final Table
wide <- all_results %>% select(Pred, Col, Fmt) %>% pivot_wider(names_from = Col, values_from = Fmt, values_fill = "")

# Sort Pred based on requested row order
wide$Pred <- factor(wide$Pred, levels = pred_order)
wide <- wide[order(wide$Pred), ]

# Column Order: Stage 1 (Beliefs In, Beliefs Out, Compliance), then Stage 2
column_order <- c(
  "Pred",
  "Stage 1 DV: Beliefs In-group",
  "Stage 1 DV: Beliefs Out-group",
  "Stage 1 DV: Compliance",
  "Stage 2 DV: Beliefs In-group",
  "Stage 2 DV: Beliefs Out-group",
  "Stage 2 DV: Compliance"
)

wide <- wide %>% select(any_of(column_order))

# Add GOF rows (N and R2)
gof_rows <- data.frame(
  Pred = c("Num.Obs.", "Pseudo R-squared"),
  `Stage 1 DV: Beliefs In-group`  = c(as.character(res1$n), sprintf("%.3f", res1$r2["un"])),
  `Stage 1 DV: Beliefs Out-group` = c(as.character(res1$n), sprintf("%.3f", res1$r2["pm"])),
  `Stage 1 DV: Compliance`        = c(as.character(res1$n), sprintf("%.3f", res1$r2["cp"])),
  `Stage 2 DV: Beliefs In-group`  = c(as.character(res2$n), sprintf("%.3f", res2$r2["un"])),
  `Stage 2 DV: Beliefs Out-group` = c(as.character(res2$n), sprintf("%.3f", res2$r2["pm"])),
  `Stage 2 DV: Compliance`        = c(as.character(res2$n), sprintf("%.3f", res2$r2["cp"])),
  check.names = FALSE
)

final_table <- bind_rows(wide, gof_rows)

# --- 5. Export ---
datasummary_df(
  final_table,
  title = "Tobit Path Models for Compliance and Beliefs (Round 8) with Parameter Recovery",
  notes = c("† p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001",
            "Models are Tobit regressions [0,1]. Gamma and Lambdas are recovered via Delta Method."),
  output = paste0(path_github, "Outputs/Tobit_Lambdas_Recovered_Ordered_v2.docx")
)