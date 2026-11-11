# ==============================================================================
# Script: SEM Round 8 Analysis (with Lambda Parameter Recovery)
#         -- CLUSTER (matching-group) BOOTSTRAP version --
# Description: Runs SEM models for compliance decisions in Round 8 for both
# Unknown (T1) and Known (T2) out-group scenarios and exports a combined
# summary table including:
#   - Gamma (total conditional cooperation effect)
#   - Lambdas (relative weights of each component)
#   - Intercepts (constants)
#
# Inference: nonparametric CLUSTER bootstrap. In each replication, matching
# groups ("gid.treat") are drawn with replacement and ALL participants of each
# drawn group enter the resample. The SAME resample is used for Stage 1 and
# Stage 2, so the bootstrap also delivers tests of cross-stage differences in
# gamma, the lambdas and beta6-beta8 (Section 4b).
# Point estimates are unchanged; only SEs, p-values and CIs change.
# ==============================================================================

library(lavaan)
library(dplyr)
library(tidyr)
library(modelsummary)
library(tinytable)
library(rlang)
library(ggplot2)

rm(list = ls())

# --- 1. Setup: Paths, Data and Bootstrap Settings ---
path_github <- "C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos  <- "C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

load(paste0(path_datos, "/Datos_islitas_recode.Rdata"))

cluster_var <- "gid.treat"   # resampling unit (matching group)
B           <- 1000          # bootstrap replications
seed_boot   <- 478

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

# 2a. Data preparation (identical transformations to the original script;
#     estimation is now done separately so the data can be resampled by group)
prep_T1 <- function(data, R_start, R_end) {
  cols <- declare_get_columns("T1juegoalgas", "T1_extraccion_libre", R_start, R_end, data)
  data$average_extraction_ini  <- rowMeans(data[, cols, drop = FALSE], na.rm = TRUE)
  data$average_compliance_ini  <- 1 - data$average_extraction_ini / 50
  data$belief_compliance_pm    <- 1 - data$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50
  data$belief_compliance_union <- 1 - data$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50
  
  data$confianza_caleta <- as.numeric(scale(data$survey1.1.player.confianza_caleta))
  data$conflicto_caleta <- as.numeric(scale(data$survey1.1.player.conflicto_caleta))
  data$confianza_pm     <- as.numeric(scale(data$survey1.1.player.confianza_pm))
  data$conflicto_pm     <- as.numeric(scale(data$survey1.1.player.conflicto_pm))
  
  cols_obs <- declare_get_columns("T1juegoalgas", "T1_extraccion_otros_libre", R_start - 1, R_start - 1, data)
  data$average_compliance_observed_ini_lag <- 1 - (rowMeans(data[, cols_obs, drop = FALSE], na.rm = TRUE) / 150)
  data
}

prep_T2 <- function(data, R_start, R_end) {
  cols <- declare_get_columns("T2juegoalgas", "T2_extraccion_metat", R_start, R_end, data)
  data$average_extraction_ini  <- rowMeans(data[, cols, drop = FALSE], na.rm = TRUE)
  data$average_compliance_ini  <- 1 - data$average_extraction_ini / 50
  data$belief_compliance_pm    <- 1 - data$beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini / 50
  data$belief_compliance_union <- 1 - (data$beliefsT2inicial.1.player.T2_belief_caleta_ini / 50)
  
  data$confianza_caleta <- as.numeric(scale(data$survey1.1.player.confianza_caleta))
  data$conflicto_caleta <- as.numeric(scale(data$survey1.1.player.conflicto_caleta))
  data$confianza_metat  <- as.numeric(scale(data$survey2.1.player.confianza_caleta_conocida_mean))
  data$conflicto_metat  <- as.numeric(scale(data$survey2.1.player.conflicto_caleta_conocida_mean))
  
  cols_obs <- declare_get_columns("T2juegoalgas", "T2_extraccion_otros_metat", R_start - 1, R_start - 1, data)
  data$average_compliance_observed_ini_lag <- 1 - (rowMeans(data[, cols_obs, drop = FALSE], na.rm = TRUE) / 150)
  data
}

# 2b. Model syntax (unchanged)
model_T1 <- '
  # Regressions
  belief_compliance_pm ~ confianza_pm + conflicto_pm
  belief_compliance_union ~ confianza_caleta + conflicto_caleta
  average_compliance_ini ~ c1*belief_compliance_pm + c2*belief_compliance_union + confianza_pm + conflicto_pm + confianza_caleta + conflicto_caleta + c3*average_compliance_observed_ini_lag

  # Intercepts
  belief_compliance_pm ~ 1
  belief_compliance_union ~ 1
  average_compliance_ini ~ 1

  # --- PARAMETER RECOVERY ---
  gamma_cc     := c1 + c2 + c3
  lambda_pm    := c1 / (c1 + c2 + c3)
  lambda_union := c2 / (c1 + c2 + c3)
  lambda_obs   := c3 / (c1 + c2 + c3)
'

model_T2 <- '
  # Regressions
  belief_compliance_pm ~ confianza_metat + conflicto_metat
  belief_compliance_union ~ confianza_caleta + conflicto_caleta
  average_compliance_ini ~ c1*belief_compliance_pm + c2*belief_compliance_union + confianza_metat + conflicto_metat + confianza_caleta + conflicto_caleta + c3*average_compliance_observed_ini_lag

  # Intercepts
  belief_compliance_pm ~ 1
  belief_compliance_union ~ 1
  average_compliance_ini ~ 1

  # --- PARAMETER RECOVERY ---
  gamma_cc     := c1 + c2 + c3
  lambda_pm    := c1 / (c1 + c2 + c3)
  lambda_union := c2 / (c1 + c2 + c3)
  lambda_obs   := c3 / (c1 + c2 + c3)
'

# 2c. Point estimates as a named vector (key = "lhs op rhs")
get_est <- function(fit) {
  pe <- parameterEstimates(fit, se = FALSE, zstat = FALSE, pvalue = FALSE, ci = FALSE)
  pe <- pe[pe$op %in% c("~", "~1", ":="), ]
  setNames(pe$est, paste(pe$lhs, pe$op, pe$rhs))
}

# 2d. Re-estimate on one resample; returns NA for failed / non-converged fits
safe_est <- function(model, data, template) {
  out <- setNames(rep(NA_real_, length(template)), template)
  fit <- tryCatch(suppressWarnings(sem(model, data = data, estimator = "ML", se = "none")),
                  error = function(e) NULL)
  if (is.null(fit) || !isTRUE(lavInspect(fit, "converged"))) return(out)
  est <- get_est(fit)
  out[names(est)] <- est
  out
}

# 2e. Joint cluster bootstrap for both stages
#     d1 and d2 must come from the same data frame (same rows, same order).
cluster_boot <- function(d1, d2, model1, model2, fit1, fit2, cluster, B, seed) {
  cl <- d1[[cluster]]
  if (is.null(cl)) stop("Cluster variable '", cluster, "' not found in data.")
  if (anyNA(cl))   stop("Cluster variable '", cluster, "' has missing values.")
  if (!identical(cl, d2[[cluster]])) stop("Stage 1 and Stage 2 data are not row-aligned.")
  
  rows_by_cl <- split(seq_len(nrow(d1)), cl)
  G <- length(rows_by_cl)
  tmpl1 <- names(get_est(fit1))
  tmpl2 <- names(get_est(fit2))
  
  set.seed(seed)
  boot1 <- matrix(NA_real_, B, length(tmpl1), dimnames = list(NULL, tmpl1))
  boot2 <- matrix(NA_real_, B, length(tmpl2), dimnames = list(NULL, tmpl2))
  pb <- txtProgressBar(min = 0, max = B, style = 3)
  for (b in seq_len(B)) {
    draw <- sample(names(rows_by_cl), G, replace = TRUE)
    idx  <- unlist(rows_by_cl[draw], use.names = FALSE)
    boot1[b, ] <- safe_est(model1, d1[idx, , drop = FALSE], tmpl1)
    boot2[b, ] <- safe_est(model2, d2[idx, , drop = FALSE], tmpl2)
    setTxtProgressBar(pb, b)
  }
  close(pb)
  list(T1 = boot1, T2 = boot2, G = G)
}

# 2f. Replace analytic SEs with bootstrap SEs, z-based p-values and
#     percentile 95% CIs (same column names as parameterEstimates()).
#     p-values use est / bootstrap SE, as lavaan's se = "bootstrap" did before.
summarise_boot <- function(fit, boot_mat) {
  pe  <- parameterEstimates(fit, se = FALSE, zstat = FALSE, pvalue = FALSE, ci = FALSE) %>%
    filter(op %in% c("~", "~1", ":="))
  key <- paste(pe$lhs, pe$op, pe$rhs)
  bm  <- boot_mat[, key, drop = FALSE]
  bm  <- bm[complete.cases(bm), , drop = FALSE]
  pe$se       <- apply(bm, 2, sd)
  pe$z        <- pe$est / pe$se
  pe$pvalue   <- 2 * pnorm(-abs(pe$z))
  pe$ci.lower <- apply(bm, 2, quantile, probs = 0.025, names = FALSE)
  pe$ci.upper <- apply(bm, 2, quantile, probs = 0.975, names = FALSE)
  attr(pe, "n_ok") <- nrow(bm)
  pe
}

# --- 3. Prepare Data and Fit Models (point estimates) ---
d_T1 <- prep_T1(df, 8, 8)
d_T2 <- prep_T2(df, 8, 8)

fit_T1_R8 <- sem(model_T1, data = d_T1, estimator = "ML")
fit_T2_R8 <- sem(model_T2, data = d_T2, estimator = "ML")

# Inspect the clustering structure before bootstrapping
cl_sizes <- table(df[[cluster_var]])
message("Number of clusters (", cluster_var, "): ", length(cl_sizes),
        " | participants per cluster: ", paste(range(cl_sizes), collapse = "-"))

# --- 4. Cluster Bootstrap (both stages on the same resamples) ---
boot <- cluster_boot(d_T1, d_T2, model_T1, model_T2, fit_T1_R8, fit_T2_R8,
                     cluster = cluster_var, B = B, seed = seed_boot)

pe_T1 <- summarise_boot(fit_T1_R8, boot$T1)
pe_T2 <- summarise_boot(fit_T2_R8, boot$T2)
message("Usable replications: Stage 1 = ", attr(pe_T1, "n_ok"),
        ", Stage 2 = ", attr(pe_T2, "n_ok"), " of ", B)

# --- 4b. Cross-stage differences (Stage 2 - Stage 1) ---
diff_keys <- c(
  "gamma_cc := c1+c2+c3",
  "lambda_union := c2/(c1+c2+c3)",
  "lambda_pm := c1/(c1+c2+c3)",
  "lambda_obs := c3/(c1+c2+c3)",
  "average_compliance_ini ~ belief_compliance_union",
  "average_compliance_ini ~ belief_compliance_pm",
  "average_compliance_ini ~ average_compliance_observed_ini_lag"
)
diff_labels <- c(
  "Conditional cooperation (\u03b3)",
  "\u03bb\u2081 Prior beliefs in-group",
  "\u03bb\u2082 Prior beliefs out-group",
  "\u03bb\u2083 Observed compliance",
  "\u03b2\u2086 Prior beliefs in-group",
  "\u03b2\u2087 Prior beliefs out-group",
  "\u03b2\u2088 Observed compliance"
)
# guard against small spacing differences in lavaan's rhs strings
match_key <- function(k, keys) keys[gsub(" ", "", keys) == gsub(" ", "", k)]
diff_keys <- vapply(diff_keys, match_key, character(1), keys = colnames(boot$T1))

est1 <- get_est(fit_T1_R8)[diff_keys]
est2 <- get_est(fit_T2_R8)[diff_keys]
ok   <- complete.cases(boot$T1[, diff_keys]) & complete.cases(boot$T2[, diff_keys])
dmat <- boot$T2[ok, diff_keys, drop = FALSE] - boot$T1[ok, diff_keys, drop = FALSE]

diff_table <- tibble(
  Parameter  = diff_labels,
  Stage1     = est1,
  Stage2     = est2,
  Difference = est2 - est1,
  SE         = apply(dmat, 2, sd),
  CI_lower   = apply(dmat, 2, quantile, probs = 0.025, names = FALSE),
  CI_upper   = apply(dmat, 2, quantile, probs = 0.975, names = FALSE)
) %>%
  mutate(p_value = 2 * pnorm(-abs(Difference / SE)))
print(diff_table, width = Inf)

# --- 5. Extract and Label Results ---

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
  "gamma_cc"                            = "Sum Conditional Cooperation (Gamma)",
  # FIX: labels now match pred_order and the manuscript (lambda1 = in-group,
  # lambda2 = out-group). Before, these two rows became NA and were dropped.
  "lambda_union"                        = "Lambda 1 (Weight Beliefs In-group)",
  "lambda_pm"                           = "Lambda 2 (Weight Beliefs Out-group)",
  "lambda_obs"                          = "Lambda 3 (Weight Observed Comp.)"
)

all_coefs <- bind_rows(
  pe_T1 %>% mutate(Stage = "Stage 1"),
  pe_T2 %>% mutate(Stage = "Stage 2")
) %>%
  mutate(
    DV_Type = case_when(
      lhs == "average_compliance_ini" ~ "Compliance",
      lhs == "belief_compliance_pm" ~ "Beliefs Out-group",
      lhs == "belief_compliance_union" ~ "Beliefs In-group",
      lhs %in% c("gamma_cc", "lambda_pm", "lambda_union", "lambda_obs") ~ "Compliance"
    ),
    Column_Name = paste(Stage, "DV:", DV_Type),
    Predictor = case_when(
      op == "~1" ~ "Constant",
      op == ":=" ~ var_labels[lhs],
      rhs %in% names(var_labels) ~ var_labels[rhs],
      TRUE ~ rhs
    ),
    Significance = case_when(
      pvalue < 0.001 ~ "***",
      pvalue < 0.01  ~ "**",
      pvalue < 0.05  ~ "*",
      pvalue < 0.1   ~ "\u2020",
      TRUE ~ ""
    ),
    Formatted = sprintf("%.3f%s (%.3f)", est, Significance, se)
  )

pred_order <- c(
  "Trust Out-group", "Trust In-group",
  "Conflict Out-group", "Conflict In-group",
  "Prior Beliefs Out-group", "Prior Beliefs In-group",
  "Observed Compliance (round 7)",
  "Sum Conditional Cooperation (Gamma)",
  "Lambda 1 (Weight Beliefs In-group)",
  "Lambda 2 (Weight Beliefs Out-group)",
  "Lambda 3 (Weight Observed Comp.)",
  "Constant"
)
all_coefs$Predictor <- factor(all_coefs$Predictor, levels = pred_order)

wide_table <- all_coefs %>%
  filter(!is.na(Predictor)) %>%
  select(Predictor, Column_Name, Formatted) %>%
  distinct(Predictor, Column_Name, .keep_all = TRUE) %>%
  pivot_wider(names_from = Column_Name, values_from = Formatted, values_fill = "") %>%
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

# --- 6. Goodness of Fit Statistics (not affected by the bootstrap) ---
gof_T1 <- fitMeasures(fit_T1_R8, c("ntotal", "logl", "aic", "bic"))
gof_T2 <- fitMeasures(fit_T2_R8, c("ntotal", "logl", "aic", "bic"))
r2_T1  <- lavInspect(fit_T1_R8, "rsquare")
r2_T2  <- lavInspect(fit_T2_R8, "rsquare")

gof_col <- function(gof, r2, dv) {
  c(as.character(round(gof["ntotal"])), sprintf("%.3f", gof["logl"]),
    sprintf("%.3f", gof["aic"]), sprintf("%.3f", gof["bic"]), sprintf("%.3f", r2[dv]))
}
gof_table <- data.frame(
  Predictor = c("Num.Obs.", "Log Likelihood", "AIC", "BIC", "R-squared"),
  `Stage 1 DV: Beliefs In-group`  = gof_col(gof_T1, r2_T1, "belief_compliance_union"),
  `Stage 1 DV: Beliefs Out-group` = gof_col(gof_T1, r2_T1, "belief_compliance_pm"),
  `Stage 1 DV: Compliance`        = gof_col(gof_T1, r2_T1, "average_compliance_ini"),
  `Stage 2 DV: Beliefs In-group`  = gof_col(gof_T2, r2_T2, "belief_compliance_union"),
  `Stage 2 DV: Beliefs Out-group` = gof_col(gof_T2, r2_T2, "belief_compliance_pm"),
  `Stage 2 DV: Compliance`        = gof_col(gof_T2, r2_T2, "average_compliance_ini"),
  check.names = FALSE
)

final_table <- bind_rows(wide_table, gof_table)

# --- 7. Export Tables ---
boot_note <- sprintf(paste0(
  "Standard errors (in parentheses) are from a cluster bootstrap with %d replications ",
  "that resamples matching groups (%d clusters) with replacement; Stage 1 and Stage 2 ",
  "are re-estimated on the same resamples."), B, boot$G)

table_file_path_docx <- paste0(path_github, "Outputs/SEM_Round8_Gamma_lambdas_clusterboot.docx")
datasummary_df(
  final_table,
  title = "Structural Equation Models for Compliance and Beliefs in Round 8",
  notes = c("\u2020 p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001", boot_note),
  output = table_file_path_docx
)

diff_out <- diff_table %>%
  mutate(across(c(Stage1, Stage2, Difference, SE), ~ sprintf("%.3f", .x)),
         `95% CI` = sprintf("[%.3f, %.3f]", CI_lower, CI_upper),
         p_value  = sprintf("%.3f", p_value)) %>%
  select(Parameter, `Stage 1` = Stage1, `Stage 2` = Stage2, `Difference (S2 - S1)` = Difference,
         SE, `95% CI`, `p-value` = p_value)

diff_file_path_docx <- paste0(path_github, "Outputs/SEM_Round8_StageDifferences_clusterboot.docx")
datasummary_df(
  diff_out,
  title = "Differences in recovered SEM parameters between stages (Round 8)",
  notes = c(boot_note, "95% CIs are bootstrap percentile intervals; p-values use the bootstrap SE."),
  output = diff_file_path_docx
)

message("\nSuccess! Tables saved to:\n  ", table_file_path_docx, "\n  ", diff_file_path_docx)

# --- 8. Coefficient Plot for Gamma and Lambdas ---
# 95% intervals are now bootstrap percentile CIs (ci.lower / ci.upper) rather
# than est +/- 1.96*SE, consistent with the manuscript's argument that the
# ratios need not be normally distributed.
plot_data <- all_coefs %>%
  filter(lhs %in% c("gamma_cc", "lambda_pm", "lambda_union", "lambda_obs")) %>%
  mutate(
    Predictor_Clean = case_when(
      lhs == "gamma_cc"     ~ "Sum Conditional Cooperation (\u03b3)",
      lhs == "lambda_union" ~ "\u03bb\u2081 (Weight Beliefs In-group)",
      lhs == "lambda_pm"    ~ "\u03bb\u2082 (Weight Beliefs Out-group)",
      lhs == "lambda_obs"   ~ "\u03bb\u2083 (Weight Observed Comp.)"
    )
  )

plot_data$Predictor_Clean <- factor(plot_data$Predictor_Clean, levels = rev(c(
  "Sum Conditional Cooperation (\u03b3)",
  "\u03bb\u2081 (Weight Beliefs In-group)",
  "\u03bb\u2082 (Weight Beliefs Out-group)",
  "\u03bb\u2083 (Weight Observed Comp.)"
)))

p_coef <- ggplot(plot_data, aes(x = est, y = Predictor_Clean, color = Stage, shape = Stage)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_errorbar(aes(xmin = ci.lower, xmax = ci.upper), orientation = "y",
                width = 0.2, position = position_dodge(width = 0.5), linewidth = 1) +
  geom_point(position = position_dodge(width = 0.5), size = 4) +
  scale_color_manual(values = c("Stage 1" = "#3F4A8A", "Stage 2" = "#21908CFF")) +
  theme_minimal(base_size = 14) +
  labs(
    title    = "Recovered SEM Parameters (Round 8)",
    subtitle = "Total Conditional Cooperation (\u03b3) and Belief Weights (\u03bb)",
    x        = "Parameter Estimate (95% cluster-bootstrap percentile CI)",
    y        = ""
  ) +
  theme(
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 12),
    axis.text.y      = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )

print(p_coef)

plot_file_path <- paste0(path_github, "Outputs/SEM_Round8_CoefPlot_clusterboot.jpg")
ggsave(plot_file_path, plot = p_coef, width = 10, height = 6, dpi = 300)
message("Success! Coefficient plot saved to: ", plot_file_path)

# --- 9. (Optional) Quick check: analytic cluster-robust SEs ---
# Sandwich SEs with the same clustering; the lambdas here use the delta method,
# so treat this only as a sanity check on the bootstrap SEs for beta6-beta8.
# fit_T1_cr <- sem(model_T1, data = d_T1, estimator = "MLR", cluster = cluster_var)
# fit_T2_cr <- sem(model_T2, data = d_T2, estimator = "MLR", cluster = cluster_var)
# parameterEstimates(fit_T1_cr) %>% filter(label %in% c("c1", "c2", "c3") | op == ":=")