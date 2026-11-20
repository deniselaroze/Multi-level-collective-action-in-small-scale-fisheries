# ==============================================================================
# Replication package -- 02_regressions.R
#   Table 2       Average compliance across areas and identity conditions (OLS)
#   Table SM.D.4  Predictors of changes in CPR-Shared Area compliance (OLS)
#   Table SM.E.1  Round-level mixed-effects models of compliance
# All standard errors: wild cluster bootstrap with one weight per boot_cluster
# (matching group by default; set in 00_setup.R).
# ==============================================================================

if (!exists("setup_done")) source("00_setup.R")

# ------------------------------------------------------------------------------
# Table 2: OLS on participant x stage x area average compliance
# ------------------------------------------------------------------------------
df_treatment <- df_long %>%
  mutate(compliance_TURF = 1 - extraction_amerb / 50,
         compliance_SA   = 1 - extraction_OA / 50) %>%
  group_by(.data[[participant_var]], .data[[cluster_var]], treatment) %>%
  summarise(avg_comp_TURF = mean(compliance_TURF, na.rm = TRUE),
            avg_comp_SA   = mean(compliance_SA, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(c(avg_comp_TURF, avg_comp_SA), names_to = "area", values_to = "mean_compliance") %>%
  mutate(stageTwo       = as.integer(treatment == "T2"),
         sharedArea     = as.integer(area == "avg_comp_SA"),
         knownOutGroups = stageTwo * sharedArea) %>%
  as.data.frame()

fit_t2 <- list(`Average compliance` =
  lm_boot(mean_compliance ~ stageTwo + sharedArea + knownOutGroups, df_treatment,
          seed = seed_main + 2))

table2 <- reg_table(fit_t2, c(
  "(Intercept)"    = "Constant (α₀)",
  "stageTwo"       = "Stage 2 (α₁)",
  "sharedArea"     = "Shared Area (α₂)",
  "knownOutGroups" = "Shared Area × Stage 2 (α₃)"
), gof = c("nobs", "r2", "adj_r2"))
export_table(table2, "Table_2_Average_compliance_OLS",
             "Table 2 | Average compliance across areas and identity conditions",
             notes = c(boot_note(fit_t2[[1]]$G, model = "ols"),
                       "The reference category is the TURF game in Stage 1.", star_note))

# ------------------------------------------------------------------------------
# Table SM.D.4: Changes in Shared Area compliance between stages (one row per participant)
# ------------------------------------------------------------------------------
T1_SA_cols <- paste0("T1juegoalgas.", 1:8, ".player.T1_extraccion_libre")
T2_SA_cols <- paste0("T2juegoalgas.", 1:8, ".player.T2_extraccion_metat")

df_models <- df_wide %>%
  mutate(
    comp_sa_T1   = 1 - rowMeans(df_wide[, T1_SA_cols], na.rm = TRUE) / 50,
    comp_sa_T2   = 1 - rowMeans(df_wide[, T2_SA_cols], na.rm = TRUE) / 50,
    diff_comp_sa = comp_sa_T2 - comp_sa_T1,
    diff_belief_in  = (1 - beliefsT2inicial.1.player.T2_belief_caleta_ini / 50) -
                      (1 - beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50),
    diff_belief_out = (1 - beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini / 50) -
                      (1 - beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50),
    diff_trust_out    = survey2.1.player.confianza_caleta_conocida_mean - survey1.1.player.confianza_pm,
    diff_conflict_out = survey2.1.player.conflicto_caleta_conocida_mean - survey1.1.player.conflicto_pm
  )

fits_s4 <- list(
  `Model 1` = lm_boot(diff_comp_sa ~ diff_belief_in + diff_belief_out, df_models,
                      seed = seed_main + 3),
  `Model 2` = lm_boot(diff_comp_sa ~ diff_trust_out + diff_conflict_out, df_models,
                      seed = seed_main + 4),
  `Model 3` = lm_boot(diff_comp_sa ~ diff_belief_in + diff_belief_out + diff_trust_out +
                        diff_conflict_out, df_models, seed = seed_main + 5)
)
tableS4 <- reg_table(fits_s4, c(
  "(Intercept)"       = "Constant",
  "diff_belief_in"    = "Change in prior belief, in-group",
  "diff_belief_out"   = "Change in prior belief, out-group",
  "diff_trust_out"    = "Change in trust, out-group",
  "diff_conflict_out" = "Change in conflict, out-group"
), gof = c("nobs", "r2", "adj_r2", "F"))
export_table(tableS4, "Table_SM.D.4_Compliance_change_OLS",
             "Table SM.D.4 | Predictors of changes in CPR-Shared Area compliance",
             notes = c("OLS; one observation per participant (Stage 2 minus Stage 1).",
                       boot_note(fits_s4[[1]]$G, model = "ols"),
                       paste("F statistic: Wald test that all slopes are zero, using the bootstrap",
                             "covariance matrix (Wald statistic divided by the number of slopes)."),
                       star_note))

# ------------------------------------------------------------------------------
# Table SM.E.1: Round-level mixed-effects models (participant random intercept)
# ------------------------------------------------------------------------------
lmm_data <- df_long %>%
  mutate(pid = .data[[participant_var]],
         stageTwo = as.integer(treatment == "T2"),
         TURF = 1 - extraction_amerb / 50, SA = 1 - extraction_OA / 50) %>%
  select(all_of(unique(c(participant_var, cluster_var, boot_cluster))), pid, stageTwo, TURF, SA) %>%
  pivot_longer(c(TURF, SA), names_to = "area", values_to = "compliance") %>%
  mutate(sharedArea = as.integer(area == "SA")) %>%
  filter(!is.na(compliance)) %>%
  as.data.frame()

lmm_forms <- list(
  `Model 1: Area difference` = compliance ~ sharedArea + (1 | pid),
  `Model 2: Area × stage` = compliance ~ sharedArea * stageTwo + (1 | pid)
)
lmm_ctrl <- lmerControl(calc.derivs = FALSE)
lmm_fits <- lapply(lmm_forms, function(f) lmer(f, data = lmm_data, control = lmm_ctrl))

# Wild bootstrap: compliance is rebuilt as the fixed-effects prediction plus the
# marginal residual (random intercept + error) times the cluster weight, and the
# mixed model is re-fitted (lmm_wild() in 00_setup.R).
lmm_res <- lapply(seq_along(lmm_fits), function(k) {
  message("Table SM.E.1: wild bootstrap of ", names(lmm_fits)[k], " ...")
  lmm_wild(lmm_fits[[k]], lmm_data, B = B_boot, seed = seed_main + 6 + k, control = lmm_ctrl)
})
names(lmm_res) <- names(lmm_fits)

r2_nakagawa <- function(m) {
  vf <- var(drop(model.matrix(m) %*% fixef(m)))
  vr <- sum(vapply(VarCorr(m), function(v) v[1, 1], numeric(1)))
  ve <- sigma(m)^2
  c(marginal = vf / (vf + vr + ve), conditional = (vf + vr) / (vf + vr + ve))
}

s6_terms <- c("(Intercept)" = "Constant", "sharedArea" = "CPR-Shared Area",
              "stageTwo" = "Stage 2", "sharedArea:stageTwo" = "Shared Area × Stage 2")
s6_cols <- lapply(names(lmm_fits), function(k) {
  m  <- lmm_fits[[k]]
  bs <- boot_summary(fixef(m), lmm_res[[k]]$draws)
  cells <- vapply(names(s6_terms), function(t) {
    r <- bs[bs$term == t, ]
    if (nrow(r) == 0) "–" else fmt_coef(r$est, r$se, r$pvalue)
  }, character(1))
  r2 <- r2_nakagawa(m)
  c(cells, format(nobs(m), big.mark = ","), sprintf("%.3f", r2["marginal"]),
    sprintf("%.3f", r2["conditional"]), sprintf("%.3f", sqrt(mean(residuals(m)^2))),
    as.character(attr(bs, "n_ok")))
})
tableS6 <- data.frame(
  Predictor = c(unname(s6_terms), "Observations", "Marginal R²", "Conditional R²",
                "RMSE", "Bootstrap replications used"),
  do.call(cbind, s6_cols), check.names = FALSE
)
names(tableS6)[-1] <- names(lmm_fits)
export_table(tableS6, "Table_SM.E.1_Mixed_effects",
             "Table SM.E.1 | Round-level mixed-effects models of compliance",
             notes = c("Linear mixed models with a participant random intercept (REML).",
                       boot_note(lmm_res[[1]]$G, model = "lmm"),
                       star_note))
