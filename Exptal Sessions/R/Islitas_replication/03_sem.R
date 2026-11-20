# ==============================================================================
# Replication package -- 03_sem.R
#   Table 3        Recovered conditional cooperation (gamma) and lambda parameters
#   Table 4        Associations of trust and conflict with prior beliefs (delta)
#   Table SM.D.5   Full SEM coefficient estimates for compliance (beta)
#   Table SM.E.2   Cross-stage differences in gamma, lambda and beta
#   Figure SM.E.1  Sensitivity of lambda to round selection and aggregation window
#   Table SM.E.3   Estimates underlying Figure SM.E.1
#   Not in the manuscript (files start with Extra_): full parameter output and a
#   gamma/lambda coefficient plot
#
# Inference: wild cluster bootstrap with one weight per matching group (00_setup.R).
# In each replication the outcomes of the three SEM equations are rebuilt as fitted
# value + residual x weight, and Stage 1 and Stage 2 are re-estimated with the same
# weights, so cross-stage differences can be tested.
# ==============================================================================

if (!exists("setup_done")) source("00_setup.R")

# ------------------------------------------------------------------------------
# Main specification: final round of each stage (Round 8 within stage)
# ------------------------------------------------------------------------------
message("Main SEM: wild cluster bootstrap with ", B_boot, " replications ...")
sem_main <- run_sem_spec(8, 8, B = B_boot, seed = seed_main)
pe1 <- sem_main$pe1; pe2 <- sem_main$pe2
message("Usable replications: Stage 1 = ", attr(pe1, "n_ok"),
        ", Stage 2 = ", attr(pe2, "n_ok"), " of ", B_boot)

r2_1 <- lavInspect(sem_main$fit1, "rsquare"); r2_2 <- lavInspect(sem_main$fit2, "rsquare")
n_1  <- lavInspect(sem_main$fit1, "nobs");    n_2  <- lavInspect(sem_main$fit2, "nobs")

sem_note  <- boot_note(sem_main$G, model = "sem")
same_note <- "In each bootstrap replication the same cluster weights are used for Stage 1 and Stage 2."

# Full parameter output (all equations, both stages; not in the manuscript)
full_out <- bind_rows(pe1 %>% mutate(Stage = "Stage 1"), pe2 %>% mutate(Stage = "Stage 2")) %>%
  transmute(Stage, Parameter = ifelse(op == ":=", lhs, paste(lhs, op, rhs)),
            Estimate = sprintf("%.3f", est), `Bootstrap SE` = sprintf("%.3f", se),
            z = sprintf("%.2f", z), p = sprintf("%.3f", pvalue),
            `95% CI` = sprintf("[%.3f, %.3f]", ci.lower, ci.upper))
export_table(full_out, "Extra_SEM_Round8_all_parameters",
             "SEM parameter estimates, final round of each stage (all equations)",
             notes = c(paste("Estimates are based on the final round of each stage (Rounds 8 and 16).",
                             "Stage 1 corresponds to the unknown-identity condition and Stage 2 to the",
                             "known-identity condition."),
                       sem_note, same_note,
                       "95% confidence intervals are bootstrap percentile intervals."))

# ------------------------------------------------------------------------------
# Table 3: gamma and lambdas
# ------------------------------------------------------------------------------
table3 <- data.frame(
  Predictor = c("Conditional cooperation coefficient (γ)",
                "Information weighted expectation parameters",
                "   Prior belief in-groups (λ₁)",
                "   Prior belief out-groups (λ₂)",
                "   Compliance in previous round (λ₃)",
                "Num. obs.", "R²"),
  `Stage 1` = c(pe_cell(pe1, "gamma_cc", ":="), "",
                pe_cell(pe1, "lambda_union", ":="), pe_cell(pe1, "lambda_pm", ":="),
                pe_cell(pe1, "lambda_obs", ":="),
                as.character(n_1), sprintf("%.3f", r2_1["average_compliance_ini"])),
  `Stage 2` = c(pe_cell(pe2, "gamma_cc", ":="), "",
                pe_cell(pe2, "lambda_union", ":="), pe_cell(pe2, "lambda_pm", ":="),
                pe_cell(pe2, "lambda_obs", ":="),
                as.character(n_2), sprintf("%.3f", r2_2["average_compliance_ini"])),
  check.names = FALSE
)
export_table(table3, "Table_3_SEM_gamma_lambda",
             "Table 3 | Recovered conditional cooperation and information weighted expectation parameters across identity conditions",
             notes = c(paste("The conditional cooperation coefficient (\u03b3) captures the responsiveness of",
                             "individual compliance to expected compliance by other group members. The information",
                             "weighted expectation parameters (\u03bb\u2081, \u03bb\u2082, \u03bb\u2083) summarize the relative",
                             "associations of prior beliefs about in-group compliance, prior beliefs about",
                             "out-group compliance, and observed compliance by other group members in the",
                             "preceding round, respectively. The three coefficients sum to one within each stage;",
                             "no nonnegativity restrictions were imposed. Estimates are based on the final round",
                             "of each stage (Rounds 8 and 16)."),
                       sem_note, star_note))

# ------------------------------------------------------------------------------
# Table 4: belief equations (delta)
# ------------------------------------------------------------------------------
in_dv <- "belief_compliance_union"; out_dv <- "belief_compliance_pm"
table4 <- data.frame(
  Predictor = c("Trust in-groups (δ₁₂)", "Conflict in-groups (δ₁₃)",
                "Trust out-groups (δ₂₂)", "Conflict out-groups (δ₂₃)",
                "Constant (δ₁₁, δ₂₁)", "Num. obs.", "R²"),
  `Stage 1: In-groups` = c(pe_cell(pe1, in_dv, "~", "confianza_caleta"),
                           pe_cell(pe1, in_dv, "~", "conflicto_caleta"), "–", "–",
                           pe_cell(pe1, in_dv, "~1"), as.character(n_1), sprintf("%.3f", r2_1[in_dv])),
  `Stage 1: Out-groups` = c("–", "–", pe_cell(pe1, out_dv, "~", "confianza_pm"),
                            pe_cell(pe1, out_dv, "~", "conflicto_pm"),
                            pe_cell(pe1, out_dv, "~1"), as.character(n_1), sprintf("%.3f", r2_1[out_dv])),
  `Stage 2: In-groups` = c(pe_cell(pe2, in_dv, "~", "confianza_caleta"),
                           pe_cell(pe2, in_dv, "~", "conflicto_caleta"), "–", "–",
                           pe_cell(pe2, in_dv, "~1"), as.character(n_2), sprintf("%.3f", r2_2[in_dv])),
  `Stage 2: Out-groups` = c("–", "–", pe_cell(pe2, out_dv, "~", "confianza_metat"),
                            pe_cell(pe2, out_dv, "~", "conflicto_metat"),
                            pe_cell(pe2, out_dv, "~1"), as.character(n_2), sprintf("%.3f", r2_2[out_dv])),
  check.names = FALSE
)
export_table(table4, "Table_4_SEM_belief_equations",
             "Table 4 | Associations of trust and previous conflict with prior beliefs about compliance",
             header_top = c(" " = 1, "Stage 1" = 2, "Stage 2" = 2),
             col_labels = c("Predictor", "In-groups", "Out-groups", "In-groups", "Out-groups"),
             notes = c(paste("The dependent variables are prior beliefs about in-group and out-group",
                             "compliance, elicited before each experimental stage and rescaled to the [0,1]",
                             "interval. Trust and previous conflict have been standardized (mean 0, SD 1) for",
                             "statistical interpretation, due to the categorical nature of the responses.",
                             "Stage 1 corresponds to the unknown-identity condition, whereas Stage 2",
                             "corresponds to the known-identity condition. Dashes indicate predictors not",
                             "included in the corresponding equation."),
                       sem_note, star_note))

# ------------------------------------------------------------------------------
# Table SM.D.5: full compliance equation (beta)
# ------------------------------------------------------------------------------
cdv <- "average_compliance_ini"
s6_cells <- function(pe, trust_out, conflict_out) c(
  pe_cell(pe, cdv, "~", "belief_compliance_union"),
  pe_cell(pe, cdv, "~", "belief_compliance_pm"),
  pe_cell(pe, cdv, "~", "confianza_caleta"),
  pe_cell(pe, cdv, "~", "conflicto_caleta"),
  pe_cell(pe, cdv, "~", trust_out),
  pe_cell(pe, cdv, "~", conflict_out),
  pe_cell(pe, cdv, "~", "average_compliance_observed_ini_lag"),
  pe_cell(pe, cdv, "~1"))
tableS5 <- data.frame(
  Predictor = c("Prior belief, in-group (β₆)", "Prior belief, out-group (β₇)",
                "Trust, in-group (β₂)", "Previous conflict, in-group (β₃)",
                "Trust, out-group (β₄)", "Previous conflict, out-group (β₅)",
                "Observed compliance in preceding round (β₈)", "Constant (β₁)",
                "Num. obs.", "R²"),
  `Stage 1: Unknown identity` = c(s6_cells(pe1, "confianza_pm", "conflicto_pm"),
                                  as.character(n_1), sprintf("%.3f", r2_1[cdv])),
  `Stage 2: Known identity`   = c(s6_cells(pe2, "confianza_metat", "conflicto_metat"),
                                  as.character(n_2), sprintf("%.3f", r2_2[cdv])),
  check.names = FALSE
)
export_table(tableS5, "Table_SM.D.5_SEM_full_compliance_equation",
             "Table SM.D.5 | Full SEM coefficient estimates for individual compliance in the CPR-Shared Area",
             notes = c(paste("The dependent variable is compliance in the final round of each stage (global",
                             "Rounds 8 and 16). Observed compliance is the other group members' average",
                             "compliance in the immediately preceding round (global Rounds 7 and 15). Trust and",
                             "previous conflict are standardized (mean 0, SD 1)."),
                       sem_note, star_note))

# ------------------------------------------------------------------------------
# Table SM.E.2: Stage 2 - Stage 1 differences, from the same bootstrap weights
# ------------------------------------------------------------------------------
diff_keys <- c(
  "Conditional cooperation (γ)"  = "gamma_cc",
  "Prior belief in-group (λ₁)"   = "lambda_union",
  "Prior belief out-group (λ₂)"  = "lambda_pm",
  "Observed compliance (λ₃)"     = "lambda_obs",
  "Prior belief in-group (β₆)"   = "average_compliance_ini ~ belief_compliance_union",
  "Prior belief out-group (β₇)"  = "average_compliance_ini ~ belief_compliance_pm",
  "Observed compliance (β₈)"     = "average_compliance_ini ~ average_compliance_observed_ini_lag"
)
e1 <- get_est(sem_main$fit1)[diff_keys]; e2 <- get_est(sem_main$fit2)[diff_keys]
diff_draws <- sem_main$draws2[, diff_keys, drop = FALSE] - sem_main$draws1[, diff_keys, drop = FALSE]
dsum <- boot_summary(setNames(e2 - e1, diff_keys), diff_draws)
tableS7 <- data.frame(
  Parameter = names(diff_keys),
  `Stage 1` = sprintf("%.3f", e1), `Stage 2` = sprintf("%.3f", e2),
  `Difference (S2 − S1)` = fmt_coef(dsum$est, dsum$se, dsum$pvalue),
  `95% CI` = sprintf("[%.3f, %.3f]", dsum$ci.lower, dsum$ci.upper),
  `p-value` = ifelse(dsum$pvalue < 0.001, "< 0.001", sprintf("%.3f", dsum$pvalue)),
  check.names = FALSE
)
export_table(tableS7, "Table_SM.E.2_SEM_stage_differences",
             "Table SM.E.2 | Differences in SEM parameters between identity-information conditions",
             notes = c(paste("Differences between Stage 2 (known identity) and Stage 1 (unknown identity) in the",
                             "recovered parameters of Table 3 and the corresponding compliance-equation",
                             "coefficients of Table SM.D.5, based on the final round of each stage",
                             "(Rounds 8 and 16)."),
                       sem_note, paste(same_note, "The difference is computed within each replication, so its",
                                       "bootstrap distribution accounts for the dependence between stages."),
                       "95% confidence intervals are bootstrap percentile intervals.", star_note))

# ------------------------------------------------------------------------------
# Figure (not in the manuscript): gamma and lambdas with percentile CIs
# ------------------------------------------------------------------------------

par_labels <- c(gamma_cc = gamma_label, lam_labels)
plot_main <- bind_rows(pe1 %>% mutate(Stage = stage_labels[1]), pe2 %>% mutate(Stage = stage_labels[2])) %>%
  filter(op == ":=") %>%
  mutate(Parameter = factor(par_labels[lhs], levels = rev(par_labels)),
         Stage = factor(Stage, levels = stage_labels))
p_main <- ggplot(plot_main, aes(est, Parameter, colour = Stage, shape = Stage)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "gray55", linewidth = 0.8) +
  geom_errorbar(aes(xmin = ci.lower, xmax = ci.upper), orientation = "y", width = 0,
                position = position_dodge(width = 0.6), linewidth = 1.6) +
  geom_point(position = position_dodge(width = 0.6), size = 5) +
  scale_colour_manual(values = setNames(c("#3F4A8A", "#21908C"), stage_labels)) +
  guides(colour = guide_legend(nrow = 2), shape = guide_legend(nrow = 2)) +
  labs(x = "Estimate (95% bootstrap percentile CI)", y = NULL) +
  fig_theme()
ggsave(file.path(path_out, "Extra_Figure_SEM_gamma_lambda.jpg"), p_main, width = 14, height = 8, dpi = 300)

# ------------------------------------------------------------------------------
# Figure SM.E.1 (estimates in Table SM.E.3): sensitivity to round selection and
# aggregation window
#   Single rounds 2-8 (round 1 has no lagged feedback) and windows [a, 8].
#   For a window [a, 8], compliance is averaged over rounds a..8 and observed
#   compliance is taken from round a-1 (as in the original estimation function).
# ------------------------------------------------------------------------------
sens <- run_sensitivity(function(a, b, B, seed) run_sem_spec(a, b, B = B, seed = seed),
                        main_res = sem_main, label = "Figure SM.E.1")

export_table(sens_table(sens),
             "Table_SM.E.3_Lambda_sensitivity_estimates",
             "Table SM.E.3 | Estimates underlying the sensitivity analysis in Figure SM.E.1",
             notes = c(paste("Information weighted expectation parameters (\u03bb\u2081, \u03bb\u2082, \u03bb\u2083) re-estimated",
                             "with compliance in each individual round (2\u20138) and averaged over windows of",
                             "rounds ending in the final round of each stage. For a window [a,8], observed",
                             "compliance is taken from round a \u2212 1. Rounds are numbered within each stage."),
                       paste0(boot_note(sem_main$G, B_sens, model = "sem"),
                              " Round 8 is the main specification (Table 3) and uses ",
                              format(B_boot, big.mark = ","), " replications."),
                       "95% confidence intervals are bootstrap percentile intervals."))

plot_lambda_sens(sens, "Figure_SM.E.1_Lambda_sensitivity.jpg")
