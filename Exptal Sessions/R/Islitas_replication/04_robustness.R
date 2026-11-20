# ==============================================================================
# Replication package -- 04_robustness.R
# Robustness versions of Table 3 and Figure SM.E.1 (Supplementary Material E).
#
#   R1  Tobit compliance equation. Compliance is bounded in [0,1]; the
#       compliance equation of the SEM is re-estimated as a two-limit tobit
#       (censored at 0 = full extraction and 1 = full compliance).
#         Table SM.E.6   Table_SM.E.6_Tobit_gamma_lambda.docx
#         Figure SM.E.3  Figure_SM.E.3_Tobit_lambda_sensitivity.jpg
#         Table SM.E.7   Table_SM.E.7_Tobit_lambda_sensitivity_estimates.docx
#
#   R2  Observed compliance over the last three rounds. Observed compliance of
#       the other group members is averaged over the three rounds preceding the
#       decision round, instead of the immediately preceding round.
#         Table SM.E.4   Table_SM.E.4_Obs3_gamma_lambda.docx
#         Figure SM.E.2  Figure_SM.E.2_Obs3_lambda_sensitivity.jpg
#         Table SM.E.5   Table_SM.E.5_Obs3_lambda_sensitivity_estimates.docx
#
# Why the tobit only replaces the compliance equation: the SEM is recursive and
# the compliance-equation error is uncorrelated with the errors of the belief
# equations, so the likelihood factorizes by equation. The ML estimates of the
# compliance equation in the SEM are therefore identical to estimating that
# equation on its own (OLS), and the belief equations (Table 4) are unaffected.
# The tobit version replaces the Gaussian likelihood of the compliance equation
# with the two-limit tobit likelihood and keeps everything else.
#
# Inference: the wild cluster bootstrap of 00_setup.R, with the same seeds and
# therefore the same weights as Table 3 / Figure SM.E.1 in every replication.
#   R2: the SEM wild bootstrap (outcomes rebuilt, SEM re-estimated).
#   R1: a censored outcome cannot be rebuilt as fitted value + residual x weight,
#       so the tobit uses the score version of the wild bootstrap (Kline and
#       Santos, 2012): the score contributions of each cluster are multiplied by
#       its weight and the estimates are updated by one Newton step,
#       theta* = theta + V sum_g w_g s_g. For a linear model this is exactly the
#       wild bootstrap used for the other tables.
# ==============================================================================

if (!exists("setup_done")) source("00_setup.R")

run_R1_tobit <- TRUE   # R1, tobit compliance equation: Tables SM.E.6-SM.E.7, Figure SM.E.3
run_R2_obs3  <- TRUE   # R2, observed compliance over the last obs_k rounds: Tables SM.E.4-SM.E.5, Figure SM.E.2
obs_k_R2     <- 3      # number of preceding rounds averaged in R2

# Main specification (Table 3). Reused if 03_sem.R has already run in this session.
if (!exists("sem_main")) {
  message("Main SEM (Table 3) for comparison ...")
  sem_main <- run_sem_spec(8, 8, B = B_boot, seed = seed_main)
}

# Cell for a parameter of a boot_summary() table: "est*** (se)"
tb_cell <- function(tab, name) {
  r <- tab[tab$term == name, ]
  if (nrow(r) == 0 || is.na(r$est[1])) return("–")
  fmt_coef(r$est[1], r$se[1], r$pvalue[1])
}

# ==============================================================================
# R1. Tobit compliance equation (Tables SM.E.6-SM.E.7, Figure SM.E.3)
# ==============================================================================

# Regressors of the compliance equation (same as in the SEM)
comp_rhs <- function(stage) c(
  "belief_compliance_pm", "belief_compliance_union",
  if (stage == 1) c("confianza_pm", "conflicto_pm") else c("confianza_metat", "conflicto_metat"),
  "confianza_caleta", "conflicto_caleta", "average_compliance_observed_ini_lag")

# Two-limit tobit (Gaussian, censored at 0 and 1) with survival::survreg.
# Left-censored: y <= 0; right-censored: y >= 1; otherwise observed exactly.
tobit_fit <- function(d, rhs, lower = 0, upper = 1, tol = 1e-9) {
  d  <- d[complete.cases(d[, c("average_compliance_ini", rhs)]), , drop = FALSE]
  y  <- d$average_compliance_ini
  d$.lo <- ifelse(y <= lower + tol, NA, y)
  d$.hi <- ifelse(y >= upper - tol, NA, y)
  f <- as.formula(paste("survival::Surv(.lo, .hi, type = 'interval2') ~",
                        paste(rhs, collapse = " + ")))
  fit <- survival::survreg(f, data = d, dist = "gaussian")
  attr(fit, "data") <- d
  attr(fit, "X")    <- model.matrix(reformulate(rhs), d)
  fit
}

# Score contributions of each observation with respect to the coefficients and
# log(sigma), the parametrization of survreg's variance matrix.
tobit_scores <- function(fit) {
  d <- attr(fit, "data"); X <- attr(fit, "X"); s <- fit$scale
  # Standardized residual; for censored observations, the standardized censoring point
  z <- (d$average_compliance_ini - drop(X %*% coef(fit))) / s
  left <- is.na(d$.lo); right <- is.na(d$.hi); mid <- !left & !right
  g_xb <- g_ls <- numeric(length(z))   # d loglik / d(x'b) and d loglik / d log(sigma)
  g_xb[mid] <- z[mid] / s
  g_ls[mid] <- z[mid]^2 - 1
  mL <- exp(dnorm(z[left], log = TRUE) - pnorm(z[left], log.p = TRUE))
  g_xb[left] <- -mL / s
  g_ls[left] <- -z[left] * mL
  mR <- exp(dnorm(z[right], log = TRUE) - pnorm(z[right], lower.tail = FALSE, log.p = TRUE))
  g_xb[right] <- mR / s
  g_ls[right] <- z[right] * mR
  S <- cbind(X * g_xb, g_ls)
  colnames(S) <- c(colnames(X), "Log(scale)")
  S
}

# Compliance-equation coefficients, gamma, lambdas, and gamma as an average marginal
# effect on observed (censored) compliance, for each row of th (coefficients, Log(scale)).
tobit_derived <- function(th, X, lower = 0, upper = 1) {
  th  <- matrix(th, ncol = ncol(X) + 1, dimnames = list(NULL, c(colnames(X), "Log(scale)")))
  b   <- th[, colnames(X), drop = FALSE]
  sig <- exp(th[, "Log(scale)"])
  c1 <- b[, "belief_compliance_pm"]; c2 <- b[, "belief_compliance_union"]
  c3 <- b[, "average_compliance_observed_ini_lag"]; g <- c1 + c2 + c3
  xb <- X %*% t(b)                                   # observations x replications
  p_unc <- colMeans(pnorm(sweep(upper - xb, 2, sig, "/")) - pnorm(sweep(lower - xb, 2, sig, "/")))
  out <- cbind(b, gamma_cc = g, gamma_ame = g * p_unc,
               lambda_pm = c1 / g, lambda_union = c2 / g, lambda_obs = c3 / g, sigma = sig)
  colnames(out)[seq_len(ncol(b))] <- paste("average_compliance_ini ~", colnames(b))
  out
}

# Estimates and score wild bootstrap draws for one tobit fit, with weights W
tobit_boot <- function(fit, W) {
  X     <- attr(fit, "X")
  theta <- c(coef(fit), `Log(scale)` = log(fit$scale))
  V     <- vcov(fit)                     # order: coefficients, then Log(scale)
  if (nrow(V) != length(theta)) stop("Unexpected size of the tobit variance matrix.")
  if (!identical(rownames(V), names(theta))) dimnames(V) <- list(names(theta), names(theta))
  th_b  <- score_wild_draws(tobit_scores(fit), V, theta, cluster_index(attr(fit, "data")), W)
  list(est = tobit_derived(theta, X)[1, ], draws = tobit_derived(th_b, X))
}

# Both stages with the same weights as run_sem_spec() with the same seed
run_tobit_spec <- function(R_start, R_end, B, seed, obs_k = 1) {
  d1 <- prep_T1(df_wide, R_start, R_end, obs_k); d2 <- prep_T2(df_wide, R_start, R_end, obs_k)
  f1 <- tobit_fit(d1, comp_rhs(1)); f2 <- tobit_fit(d2, comp_rhs(2))
  W  <- wild_weight_matrix(B, seed)
  b1 <- tobit_boot(f1, W); b2 <- tobit_boot(f2, W)
  pe1 <- boot_summary(b1$est, b1$draws); pe1$lhs <- pe1$term
  pe2 <- boot_summary(b2$est, b2$draws); pe2$lhs <- pe2$term
  info <- function(f) {
    y <- attr(f, "data")$average_compliance_ini
    c(n = length(y), at0 = sum(y <= 1e-9), at1 = sum(y >= 1 - 1e-9))
  }
  G <- length(unique(c(cluster_index(attr(f1, "data")), cluster_index(attr(f2, "data")))))
  list(pe1 = pe1, pe2 = pe2, G = G, info1 = info(f1), info2 = info(f2))
}

if (run_R1_tobit) {
  message("Table SM.E.6 (tobit): main specification, ", B_boot, " replications ...")
  tob_main <- run_tobit_spec(8, 8, B = B_boot, seed = seed_main)

  n_1 <- lavInspect(sem_main$fit1, "nobs"); n_2 <- lavInspect(sem_main$fit2, "nobs")
  r2_1 <- lavInspect(sem_main$fit1, "rsquare")["average_compliance_ini"]
  r2_2 <- lavInspect(sem_main$fit2, "rsquare")["average_compliance_ini"]
  sem_col <- function(pe, n, r2, info) c(
    pe_cell(pe, "gamma_cc", ":="), "–", "",
    pe_cell(pe, "lambda_union", ":="), pe_cell(pe, "lambda_pm", ":="), pe_cell(pe, "lambda_obs", ":="),
    as.character(n), as.character(info["at0"]), as.character(info["at1"]), sprintf("%.3f", r2))
  tob_col <- function(pe, info) c(
    tb_cell(pe, "gamma_cc"), tb_cell(pe, "gamma_ame"), "",
    tb_cell(pe, "lambda_union"), tb_cell(pe, "lambda_pm"), tb_cell(pe, "lambda_obs"),
    as.character(info["n"]), as.character(info["at0"]), as.character(info["at1"]), "–")

  tableR1 <- data.frame(
    Predictor = c("Conditional cooperation coefficient (γ)",
                  "   γ, average marginal effect on observed compliance",
                  "Information weighted expectation parameters",
                  "   Prior belief in-groups (λ₁)",
                  "   Prior belief out-groups (λ₂)",
                  "   Compliance in previous round (λ₃)",
                  "Num. obs.", "Censored at 0 (full extraction)", "Censored at 1 (full compliance)", "R²"),
    S1_sem = sem_col(sem_main$pe1, n_1, r2_1, tob_main$info1), S1_tob = tob_col(tob_main$pe1, tob_main$info1),
    S2_sem = sem_col(sem_main$pe2, n_2, r2_2, tob_main$info2), S2_tob = tob_col(tob_main$pe2, tob_main$info2),
    check.names = FALSE)

  export_table(tableR1, "Table_SM.E.6_Tobit_gamma_lambda",
               "Table SM.E.6 | Recovered conditional cooperation and information weighted expectation parameters: tobit compliance equation",
               header_top = c(" " = 1, "Stage 1" = 2, "Stage 2" = 2),
               col_labels = c("", "SEM (Table 3)", "Tobit", "SEM (Table 3)", "Tobit"),
               notes = c(paste("The compliance equation of the SEM is estimated as a two-limit tobit model censored at",
                               "0 (full extraction) and 1 (full compliance) instead of a linear equation. Because the SEM is",
                               "recursive and the compliance-equation error is uncorrelated with the errors of the belief",
                               "equations, the likelihood factorizes by equation; the belief equations (Table 4) are",
                               "unchanged and only the compliance equation is re-estimated. For the tobit model, γ is the",
                               "sum of the coefficients on the latent compliance index, and the average marginal effect",
                               "multiplies it by the average probability of an uncensored outcome; for the linear SEM, γ",
                               "is itself the marginal effect. The λ parameters are ratios of coefficients and are therefore",
                               "the same for latent coefficients and marginal effects. Estimates are based on the final",
                               "round of each stage (Rounds 8 and 16)."),
                         boot_note(tob_main$G, model = "sem+tobit"), star_note))

  sens_R1 <- run_sensitivity(function(a, b, B, seed) run_tobit_spec(a, b, B = B, seed = seed),
                             main_res = tob_main, label = "Figure SM.E.3 (tobit)")
  export_table(sens_table(sens_R1), "Table_SM.E.7_Tobit_lambda_sensitivity_estimates",
               "Table SM.E.7 | Estimates underlying the sensitivity analysis in Figure SM.E.3: tobit compliance equation",
               notes = c(paste("As Table SM.E.3, with the compliance equation estimated as a two-limit",
                               "tobit model censored at 0 and 1. For a window [a,8], compliance is averaged over",
                               "rounds a–8 (censoring applies at the bounds of the average) and observed compliance is",
                               "taken from round a − 1."),
                         paste0(boot_note(tob_main$G, B_sens, model = "tobit"),
                                " Round 8 is the main specification (Table SM.E.6) and uses ",
                                format(B_boot, big.mark = ","), " replications."),
                         "95% confidence intervals are bootstrap percentile intervals."))
  plot_lambda_sens(sens_R1, "Figure_SM.E.3_Tobit_lambda_sensitivity.jpg")
}

# ==============================================================================
# R2. Observed compliance averaged over the last obs_k rounds (Tables SM.E.4-SM.E.5,
#     Figure SM.E.2)
# ==============================================================================
if (run_R2_obs3) {
  message("Table SM.E.4 (observed compliance over ", obs_k_R2, " rounds): main specification ...")
  obs_main <- run_sem_spec(8, 8, B = B_boot, seed = seed_main, obs_k = obs_k_R2)

  col3 <- function(res, stage) {
    pe  <- if (stage == 1) res$pe1 else res$pe2
    fit <- if (stage == 1) res$fit1 else res$fit2
    c(pe_cell(pe, "gamma_cc", ":="), "",
      pe_cell(pe, "lambda_union", ":="), pe_cell(pe, "lambda_pm", ":="), pe_cell(pe, "lambda_obs", ":="),
      as.character(lavInspect(fit, "nobs")),
      sprintf("%.3f", lavInspect(fit, "rsquare")["average_compliance_ini"]))
  }
  obs_lbl <- sprintf("Rounds %d–7", 8 - obs_k_R2)
  tableR2 <- data.frame(
    Predictor = c("Conditional cooperation coefficient (γ)",
                  "Information weighted expectation parameters",
                  "   Prior belief in-groups (λ₁)",
                  "   Prior belief out-groups (λ₂)",
                  sprintf("   Compliance in previous %d rounds (λ₃)", obs_k_R2),
                  "Num. obs.", "R²"),
    S1_main = col3(sem_main, 1), S1_obs = col3(obs_main, 1),
    S2_main = col3(sem_main, 2), S2_obs = col3(obs_main, 2),
    check.names = FALSE)

  export_table(tableR2, sprintf("Table_SM.E.4_Obs%d_gamma_lambda", obs_k_R2),
               sprintf("Table SM.E.4 | Recovered conditional cooperation and information weighted expectation parameters: observed compliance over the last %d rounds", obs_k_R2),
               header_top = c(" " = 1, "Stage 1" = 2, "Stage 2" = 2),
               col_labels = c("", "Round 7 (Table 3)", obs_lbl, "Round 7 (Table 3)", obs_lbl),
               notes = c(paste0("Observed compliance is the other group members' average compliance over the ", obs_k_R2,
                                " rounds preceding the decision round (", obs_lbl, " within each stage; global Rounds ",
                                8 - obs_k_R2, "–7 and ", 16 - obs_k_R2, "–15), instead of the immediately preceding",
                                " round (Round 7, as in Table 3). All other elements of the SEM are unchanged. Estimates",
                                " are based on the final round of each stage (Rounds 8 and 16)."),
                         boot_note(sem_main$G, model = "sem"),
                         "Both specifications use the same bootstrap weights in each replication.", star_note))

  sens_R2 <- run_sensitivity(function(a, b, B, seed) run_sem_spec(a, b, B = B, seed = seed, obs_k = obs_k_R2),
                             main_res = obs_main, label = sprintf("Figure SM.E.2 (observed compliance, %d rounds)", obs_k_R2))
  lab_R2 <- lam_labels
  lab_R2["lambda_obs"] <- sprintf("Compliance in previous %d rounds (λ₃)", obs_k_R2)
  export_table(sens_table(sens_R2, lab_R2), sprintf("Table_SM.E.5_Obs%d_lambda_sensitivity_estimates", obs_k_R2),
               sprintf("Table SM.E.5 | Estimates underlying the sensitivity analysis in Figure SM.E.2: observed compliance over the last %d rounds", obs_k_R2),
               notes = c(paste0("As Table SM.E.3, with observed compliance averaged over the ", obs_k_R2,
                                " rounds preceding the decision round (for a window [a,8], rounds a − ", obs_k_R2,
                                " to a − 1). When fewer than ", obs_k_R2, " preceding rounds exist (decision rounds 2 and ",
                                obs_k_R2, ", or windows starting there), all available preceding rounds are used."),
                         paste0(boot_note(sem_main$G, B_sens, model = "sem"),
                                " Round 8 is the main specification (Table SM.E.4) and uses ",
                                format(B_boot, big.mark = ","), " replications."),
                         "95% confidence intervals are bootstrap percentile intervals."))
  plot_lambda_sens(sens_R2, sprintf("Figure_SM.E.2_Obs%d_lambda_sensitivity.jpg", obs_k_R2), labels = lab_R2)
}
