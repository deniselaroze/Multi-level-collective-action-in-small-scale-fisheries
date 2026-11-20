# ==============================================================================
# Replication package -- 05_identity_vs_learning.R
# Supplementary Material F: are prior beliefs about named unions anchored on
# Stage 1 experience?
# Separating learning in Stage 1 from identity information in Stage 2.
#
# Every participant plays Stage 1 (unknown identity) before Stage 2 (known
# identity), so Stage 2 expectations could carry what was learned in Stage 1.
# These tests ask whether expectations reset between stages, i.e. whether Stage 2
# beliefs (and opening behaviour) depend on what participants experienced in
# Stage 1. Beliefs were elicited at the start and end of each stage, and the
# TURF is a benchmark without any change in identity information.
#
#   Table SM.F.1  Level reset of beliefs and restart of compliance (Shared Area vs TURF)
#   Table SM.F.2  Individual carry-over of Stage 1 updating, out-group vs in-group
#   Optional identity-content tables (trust, conflict, reputations): J1-J5, not in
#   the manuscript (files Extra_Table_J1_... to Extra_Table_J5_...)
#
# Beliefs are expected extraction (0-50), converted to compliance as 1 - x/50,
# so a positive change means a more optimistic belief.
# Inference: the wild cluster bootstrap of 00_setup.R (boot_cluster, B_boot).
# ==============================================================================

if (!exists("setup_done")) source("00_setup.R")

# --- Settings -------------------------------------------------------------------
# Which named unions define the Stage 2 out-group belief:
#   "pool" -> only named unions present in the participant's matching group
#   "all"  -> all named unions, as in the main analysis (in the two Z23 pools one
#             named union was not in the matching group)
named_scope <- "pool"
run_identity_content <- FALSE  # optional Tables J1-J5 (identity content, not reset)
run_option4 <- TRUE            # Table J5; requires the Stage 2 screens to show each member's union

# --- Helpers ----------------------------------------------------------------------
norm_union <- function(x) { x <- toupper(trimws(as.character(x))); x[x %in% c("", "NAN", "NA")] <- NA; x }
c01 <- function(x) 1 - x / 50    # expected extraction (0-50) -> compliance [0,1]
s01 <- function(x) (x - 1) / 3   # four-point scale -> [0,1]
col <- function(...) df_wide[[paste0(...)]]
shares <- function(x, tol = 1e-9) { x <- x[!is.na(x)]
  c(fmt_pct(mean(x < -tol), 1), fmt_pct(mean(abs(x) <= tol), 1), fmt_pct(mean(x > tol), 1)) }

# ==============================================================================
# 1. Participant-level variables
# ==============================================================================
w <- df_wide[, c(participant_var, cluster_var)]
w$union <- norm_union(df_wide$participant.caleta)
pool_unions <- tapply(w$union, w[[cluster_var]], function(x) unique(x))

for (k in 1:2) {
  nm <- norm_union(col("participant.caleta_conocida", k))
  w[[paste0("named", k)]]    <- nm
  w[[paste0("in_pool", k)]]  <- mapply(function(n, g) !is.na(n) && n %in% pool_unions[[g]], nm, w[[cluster_var]])
  w[[paste0("b", k, "_ini")]] <- c01(col("beliefsT2inicial.1.player.T2_belief_caleta_conocida", k, "_ini"))
  w[[paste0("b", k, "_fin")]] <- c01(col("beliefsT2final.1.player.T2_belief_caleta_conocida", k, "_fin"))
  w[[paste0("trust", k)]]    <- s01(col("survey2.1.player.confianza_caleta_conocida", k))
  w[[paste0("conflict", k)]] <- s01(col("survey2.1.player.conflicto_caleta_conocida", k))
  w[[paste0("exper", k)]]    <- s01(col("survey2.1.player.experiencia_caleta_conocida", k))
  w[[paste0("use", k)]] <- !is.na(nm) & !is.na(w[[paste0("b", k, "_ini")]]) &
    (named_scope == "all" | w[[paste0("in_pool", k)]])
}
# Mean over the named unions in scope (v = variable stem, e.g. "trust" -> trust1, trust2)
named_mean <- function(v1, v2 = NULL) {
  if (is.null(v2)) { v2 <- w[[paste0(v1, 2)]]; v1 <- w[[paste0(v1, 1)]] }
  m <- cbind(ifelse(w$use1, v1, NA), ifelse(w$use2, v2, NA))
  out <- rowMeans(m, na.rm = TRUE); out[is.nan(out)] <- NA; out
}

# Beliefs (compliance scale)
w$in_T1ini  <- c01(df_wide$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini)
w$in_T1fin  <- c01(df_wide$beliefsT1final.1.player.T1_belief_caleta_en_libre_fin)
w$in_T2ini  <- c01(df_wide$beliefsT2inicial.1.player.T2_belief_caleta_ini)
w$in_T2fin  <- c01(df_wide$beliefsT2final.1.player.T2_belief_caleta_fin)
w$out_T1ini <- c01(df_wide$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini)
w$out_T1fin <- c01(df_wide$beliefsT1final.1.player.T1_belief_pm_en_libre_fin)
w$out_T2ini <- named_mean(w$b1_ini, w$b2_ini)
w$out_T2fin <- named_mean(w$b1_fin, w$b2_fin)

# Belief changes
w$L_out <- w$out_T1fin - w$out_T1ini   # Stage 1 learning, out-group (generic)
w$L_in  <- w$in_T1fin  - w$in_T1ini    # Stage 1 learning, in-group
w$S_out <- w$out_T2ini - w$out_T1fin   # between stages: generic -> named unions
w$S_in  <- w$in_T2ini  - w$in_T1fin    # between stages: in-group (same referent)
w$I     <- w$S_out - w$S_in            # identity shift (difference-in-differences)
w$W_out <- w$out_T2fin - w$out_T2ini   # Stage 2 learning, named unions
w$W_in  <- w$in_T2fin  - w$in_T2ini    # Stage 2 learning, in-group

# Relational measures: named unions (in scope) minus generic out-group, [0,1] scales
w$d_trust    <- named_mean("trust")    - s01(df_wide$survey1.1.player.confianza_pm)
w$d_conflict <- named_mean("conflict") - s01(df_wide$survey1.1.player.conflicto_pm)
w$d_exper    <- named_mean("exper")    - s01(df_wide$survey1.1.player.experiencia_pm)

# Compliance by area and stage (mean over the 8 rounds of each stage)
rmean <- function(stage, area) rowMeans(sapply(1:8, function(r)
  c01(col(stage, "juegoalgas.", r, ".player.", stage, "_extraccion_", area))), na.rm = TRUE)
w$sa1 <- rmean("T1", "libre"); w$sa2 <- rmean("T2", "metat")
w$tu1 <- rmean("T1", "amerb"); w$tu2 <- rmean("T2", "amerb")
w$d_sa  <- w$sa2 - w$sa1
w$did   <- (w$sa2 - w$sa1) - (w$tu2 - w$tu1)

message("05: named-union scope = '", named_scope, "'; participants with a named-union belief: ",
        sum(!is.na(w$out_T2ini)), " of ", nrow(w))

# ==============================================================================
# 2. Reconstruct the Shared Area groups of each round
#    Members of a 4-person group share the same group total (own + others'
#    extraction) within a matching group and round. When two groups share a total,
#    the split consistent with every member's recorded composition (combi) is used;
#    if more than one split fits, the round is left unresolved for those members.
# ==============================================================================
combi_ok <- function(members, j, combi_j, u) {
  d <- as.integer(strsplit(sub("^G", "", combi_j), "")[[1]])
  uu <- u[members]; own <- sum(uu == u[j])
  oth <- sort(as.integer(table(uu[uu != u[j]])), decreasing = TRUE)
  d[1] == own && identical(sort(d[-1][d[-1] > 0], decreasing = TRUE), oth)
}
reconstruct <- function(stage) {
  area <- if (stage == "T1") "libre" else "metat"
  grp <- matrix(NA_character_, nrow(w), 8)
  for (r in 1:8) {
    tot <- col(stage, "juegoalgas.", r, ".player.", stage, "_extraccion_", area) +
           col(stage, "juegoalgas.", r, ".player.", stage, "_extraccion_otros_", area)
    cmb <- col(stage, "juegoalgas.", r, ".player.combi")
    key <- paste(w[[cluster_var]], tot)
    for (k in unique(key)) {
      m <- which(key == k)
      if (length(m) == 4) { grp[m, r] <- paste(k, 1); next }
      if (length(m) != 8) next
      fits <- list()
      for (cmb3 in combn(m[-1], 3, simplify = FALSE)) {
        g1 <- c(m[1], cmb3); g2 <- setdiff(m, g1)
        if (all(vapply(g1, function(j) combi_ok(g1, j, cmb[j], w$union), TRUE)) &&
            all(vapply(g2, function(j) combi_ok(g2, j, cmb[j], w$union), TRUE)))
          fits[[length(fits) + 1]] <- g1
      }
      if (length(fits) == 1) { grp[fits[[1]], r] <- paste(k, 1); grp[setdiff(m, fits[[1]]), r] <- paste(k, 2) }
    }
  }
  grp
}
grp1 <- reconstruct("T1"); grp2 <- reconstruct("T2")
message("05: groups reconstructed for ", fmt_pct(mean(!is.na(grp1)), 1), " (Stage 1) and ",
        fmt_pct(mean(!is.na(grp2)), 1), " (Stage 2) of participant-rounds")

comp_round <- function(stage, r) c01(col(stage, "juegoalgas.", r, ".player.", stage, "_extraccion_",
                                         if (stage == "T1") "libre" else "metat"))
others_in_round <- function(grp, i, r) { if (is.na(grp[i, r])) return(integer(0)); setdiff(which(grp[, r] == grp[i, r]), i) }

# Stage 1 compliance of the out-group members each participant was grouped with
exp_member <- function(i, keep_fun) {
  v <- unlist(lapply(1:8, function(r) { o <- others_in_round(grp1, i, r); o <- o[keep_fun(o)]
    comp_round("T1", r)[o] }))
  if (length(v) == 0) NA_real_ else mean(v)
}
w$exp_out_S1 <- vapply(seq_len(nrow(w)), function(i) exp_member(i, function(o) w$union[o] != w$union[i]), 0)
for (k in 1:2) {
  w[[paste0("exp", k, "_S1")]] <- vapply(seq_len(nrow(w)), function(i) {
    nk <- w[[paste0("named", k)]][i]; if (is.na(nk)) return(NA_real_)
    exp_member(i, function(o) w$union[o] == nk) }, 0)
  # Actual Stage 1 compliance of named union k in the participant's matching group
  act <- tapply(w$sa1, paste(w[[cluster_var]], w$union), mean)
  w[[paste0("act", k, "_S1")]] <- unname(act[paste(w[[cluster_var]], w[[paste0("named", k)]])])
}

# ==============================================================================
# 3. Supplementary Material F: are prior beliefs about named unions anchored on
#    Stage 1 experience?
#    Table SM.F.1  Level reset of beliefs and restart of compliance between stages
#    Table SM.F.2  Carry-over of individual Stage 1 updating into Stage 2 priors
# ==============================================================================
# Own Shared Area compliance in round 1 of Stage 1: chosen before any feedback (disposition)
w$own_r1 <- c01(df_wide$T1juegoalgas.1.player.T1_extraccion_libre)
# Shared Area and TURF compliance in round 1 and 8 of Stage 1 and round 1 of Stage 2 (Round 9)
rc <- function(stage, area, r) c01(col(stage, "juegoalgas.", r, ".player.", stage, "_extraccion_", area))
w$sa_s1r1 <- rc("T1", "libre", 1); w$sa_s1r8 <- rc("T1", "libre", 8); w$sa_s2r1 <- rc("T2", "metat", 1)
w$tu_s1r1 <- rc("T1", "amerb", 1); w$tu_s1r8 <- rc("T1", "amerb", 8); w$tu_s2r1 <- rc("T2", "amerb", 1)

scope_note <- if (named_scope == "pool")
  "Out-group beliefs in Stage 2 average the named unions present in the participant's matching group." else
  "Out-group beliefs in Stage 2 average all named unions, as in the main analysis."
fmt_ratio <- function(r) sprintf("%.2f [%.2f, %.2f]", r$est, r$ci.lower, r$ci.upper)
stars_note <- star_note

# ------------------------------------------------------------------------------
# Table SM.F.1. Level reset of beliefs (Panel A) and restart of compliance (Panel B)
#   Change within Stage 1, change between stages, net change (start of Stage 2 -
#   start of Stage 1), and the share of the Stage 1 change reversed between stages
#   (minus change between / change within; 1 = full reset).
#   Every statistic is a function of the means of the variables below, so the wild
#   bootstrap rebuilds each variable as mean + deviation x weight (wild_means()).
# ------------------------------------------------------------------------------
reset_stats <- function(mu, v1, v8, v9, tag) {
  m <- function(v) mu[[v]]
  D <- m(v8) - m(v1); R <- m(v9) - m(v8)
  setNames(c(m(v1), m(v8), m(v9), D, R, D + R, -R / D), paste0(tag, c("_1", "_8", "_9", "_D", "_R", "_N", "_sh")))
}
diff_stats <- function(a, b, tag) {
  k <- c("_D", "_R", "_N", "_sh")
  setNames(a[paste0(sub("_.*", "", names(a)[1]), k)] - b[paste0(sub("_.*", "", names(b)[1]), k)], paste0(tag, k))
}
statS8 <- function(mu) {   # mu: named vector of means
  o <- reset_stats(mu, "out_T1ini", "out_T1fin", "out_T2ini", "o")
  i <- reset_stats(mu, "in_T1ini",  "in_T1fin",  "in_T2ini",  "i")
  s <- reset_stats(mu, "sa_s1r1",   "sa_s1r8",   "sa_s2r1",   "s")
  t <- reset_stats(mu, "tu_s1r1",   "tu_s1r8",   "tu_s2r1",   "t")
  c(o, i, diff_stats(o, i, "dA"), s, t, diff_stats(s, t, "dB"))
}
vars8 <- c("out_T1ini", "out_T1fin", "out_T2ini", "in_T1ini", "in_T1fin", "in_T2ini",
           "sa_s1r1", "sa_s1r8", "sa_s2r1", "tu_s1r1", "tu_s1r8", "tu_s2r1")
w8  <- w[complete.cases(w[, vars8]), ]
bs8 <- wild_means(w8, vars8, statS8, B = B_boot, seed = seed_main + 600)
sm8 <- boot_summary(bs8$est, bs8$draws); g8 <- function(k) sm8[sm8$term == k, ]
c8  <- function(k) { r <- g8(k); fmt_coef(r$est, r$se, r$pvalue) }
row8 <- function(label, tag, levels = TRUE)
  c(label, if (levels) sprintf("%.3f", c(g8(paste0(tag, "_1"))$est, g8(paste0(tag, "_8"))$est, g8(paste0(tag, "_9"))$est))
           else rep("", 3),
    c8(paste0(tag, "_D")), c8(paste0(tag, "_R")), c8(paste0(tag, "_N")), fmt_ratio(g8(paste0(tag, "_sh"))))
tableS8 <- as.data.frame(rbind(
  c("Panel A. Prior beliefs about compliance", rep("", 7)),
  row8("   Out-group", "o"), row8("   In-group", "i"),
  row8("   Difference (out-group − in-group)", "dA", FALSE),
  c("Panel B. Compliance", rep("", 7)),
  row8("   CPR-Shared Area", "s"), row8("   CPR-TURF", "t"),
  row8("   Difference (Shared Area − TURF)", "dB", FALSE)), stringsAsFactors = FALSE)
names(tableS8) <- c(" ", "Start of Stage 1 (Round 1)", "End of Stage 1 (Round 8)", "Start of Stage 2 (Round 9)",
                    "Change within Stage 1", "Change between stages", "Net change",
                    "Share of Stage 1 change reversed [95% CI]")
export_table(tableS8, "Table_SM.F.1_Reset_beliefs_compliance",
             "Table SM.F.1 | Level reset of prior beliefs and restart of compliance between stages",
             notes = c(paste("Panel A reports mean beliefs about compliance in the CPR-Shared Area (one minus expected",
                             "extraction divided by 50), elicited at the start and end of Stage 1 and at the start of Stage 2;",
                             "out-group beliefs refer to a generic fisher from another union in Stage 1 and to the named",
                             "unions in Stage 2. Panel B reports mean individual compliance in Rounds 1, 8 and 9. Net change:",
                             "start of Stage 2 minus start of Stage 1. Share reversed: minus the change between stages divided",
                             "by the change within Stage 1 (1 = full reset)."), scope_note,
                       sprintf("N = %d.", nrow(w8)), boot_note(bs8$G, model = "means"),
                       "Confidence intervals are bootstrap percentile intervals.", stars_note))

# ------------------------------------------------------------------------------
# Table SM.F.2. Carry-over of individual Stage 1 updating
#   Belief_S2start = a + rho * Belief_S1end + kappa * Belief_S1start
#                    + theta * own Round-1 compliance + matching-group FE.
#   rho = share of the participant's own Stage 1 updating retained at the start
#   of Stage 2 (0 = full reset, 1 = full carry-over). Out-group and in-group
#   equations use the same bootstrap weights, so their difference is tested.
# ------------------------------------------------------------------------------
wb <- w[complete.cases(w[, c("out_T1ini", "out_T1fin", "out_T2ini", "in_T1ini", "in_T1fin", "in_T2ini", "own_r1")]), ]
fe_b <- model.matrix(~ factor(wb[[cluster_var]]))
Xo <- cbind(fe_b, rho = wb$out_T1fin, kappa = wb$out_T1ini, own = wb$own_r1)
Xi <- cbind(fe_b, rho = wb$in_T1fin,  kappa = wb$in_T1ini,  own = wb$own_r1)
kB <- c("rho", "kappa", "own")
WB <- wild_weight_matrix(B_boot, seed_main + 610)
fo <- ols_wild(Xo, wb$out_T2ini, cluster_index(wb), WB)   # out-group equation
fi <- ols_wild(Xi, wb$in_T2ini,  cluster_index(wb), WB)   # in-group equation, same weights
estB   <- c(setNames(fo$est[kB], paste0("o_", kB)), setNames(fi$est[kB], paste0("i_", kB)),
            setNames(fo$est[kB] - fi$est[kB], paste0("d_", kB)))
drawsB <- cbind(fo$draws[, kB], fi$draws[, kB], fo$draws[, kB] - fi$draws[, kB])
colnames(drawsB) <- names(estB)
smB <- boot_summary(estB, drawsB)
cB  <- function(k) { r <- smB[smB$term == k, ]; fmt_coef(r$est, r$se, r$pvalue) }
r2B <- function(X, y) sprintf("%.3f", 1 - sum(lm.fit(X, y)$residuals^2) / sum((y - mean(y))^2))
tableS9 <- data.frame(
  Predictor = c("End-of-Stage-1 belief: carry-over of Stage 1 updating (ρ)",
                "Start-of-Stage-1 belief (κ)", "Own compliance, Round 1",
                "Matching-group fixed effects", "Num. obs.", "R²"),
  out = c(cB("o_rho"), cB("o_kappa"), cB("o_own"), "Yes", as.character(nrow(wb)), r2B(Xo, wb$out_T2ini)),
  inn = c(cB("i_rho"), cB("i_kappa"), cB("i_own"), "Yes", as.character(nrow(wb)), r2B(Xi, wb$in_T2ini)),
  dif = c(cB("d_rho"), cB("d_kappa"), cB("d_own"), "", "", ""),
  check.names = FALSE, stringsAsFactors = FALSE)
export_table(tableS9, "Table_SM.F.2_Belief_carryover",
             "Table SM.F.2 | Carry-over of individual Stage 1 updating into prior beliefs at the start of Stage 2",
             col_labels = c("Predictor", "Out-group (named unions)", "In-group", "Difference (out-group − in-group)"),
             notes = c(paste("OLS; dependent variable: prior belief about compliance at the start of Stage 2 (one minus",
                             "expected extraction divided by 50)."), scope_note,
                       boot_note(n_clusters(wb), model = "ols"),
                       "Both equations use the same bootstrap weights in each replication.", stars_note))

# ==============================================================================
# Optional: identity-content tables (Tables J1-J5). Off by default. Not in the
# manuscript, so their files start with Extra_.
#   They ask what the identity information contains (trust, conflict, union
#   reputations), not whether expectations reset. Set run_identity_content <- TRUE.
# ==============================================================================
if (run_identity_content) {
  # ==============================================================================
  # J1. Predictors of the identity shift
  # ==============================================================================
  fits_J1 <- list(
    `Model 1` = lm_boot(I ~ d_trust + d_conflict + d_exper, w, seed = seed_main + 501),
    `Model 2` = lm_boot(I ~ d_trust + d_conflict + d_exper + L_out, w, seed = seed_main + 502))
  tableJ1 <- reg_table(fits_J1, c(
    "(Intercept)" = "Constant",
    "d_trust"     = "Trust: named unions − generic out-group",
    "d_conflict"  = "Previous conflict: named unions − generic out-group",
    "d_exper"     = "Experience: named unions − generic out-group",
    "L_out"       = "Stage 1 learning, out-group"), gof = c("nobs", "r2"))
  export_table(tableJ1, "Extra_Table_J1_Identity_shift_predictors",
               "Table J1 | Predictors of the identity shift in beliefs about out-group compliance",
               notes = c(paste("OLS; one observation per participant. Dependent variable: identity shift, the change",
                               "in the out-group belief between the end of Stage 1 and the start of Stage 2 minus the same change",
                               "for the in-group belief. Trust, previous conflict and experience are four-point scales rescaled",
                               "to [0,1]; each regressor is the value for the named unions minus the value for the generic",
                               "out-group reported before Stage 1."), scope_note, boot_note(fits_J1[[1]]$G), star_note))

  # ==============================================================================
  # J2. Within-person comparison of the two named unions
  #   In Stage 1 both outside unions were shown with the same colour, so Stage 1
  #   experience cannot tell them apart from the participant's point of view.
  #   Differencing the two beliefs removes everything common to the participant.
  # ==============================================================================
  w2 <- w[w$use1 & w$use2 & w$in_pool1 & w$in_pool2, ]
  w2 <- transform(w2,
    d_b      = b1_ini - b2_ini,
    dd_trust = trust1 - trust2, dd_conflict = conflict1 - conflict2, dd_exper = exper1 - exper2,
    dd_exp   = exp1_S1 - exp2_S1, dd_act = act1_S1 - act2_S1)
  fits_J2 <- list(
    `Model 1` = lm_boot(d_b ~ dd_trust + dd_conflict + dd_exper, w2, seed = seed_main + 510),
    `Model 2` = lm_boot(d_b ~ dd_exp + dd_act, w2, seed = seed_main + 511),
    `Model 3` = lm_boot(d_b ~ dd_trust + dd_conflict + dd_exper + dd_exp + dd_act, w2, seed = seed_main + 512))
  tableJ2 <- reg_table(fits_J2, c(
    "(Intercept)" = "Constant (named union 1 − named union 2)",
    "dd_trust"    = "Difference in trust",
    "dd_conflict" = "Difference in previous conflict",
    "dd_exper"    = "Difference in experience",
    "dd_exp"      = "Difference in Stage 1 compliance of members met (placebo)",
    "dd_act"      = "Difference in Stage 1 compliance of the union (placebo)"), gof = c("nobs", "r2"))
  export_table(tableJ2, "Extra_Table_J2_Within_person_named_unions",
               "Table J2 | Within-person differences in prior beliefs about the two named out-group unions",
               notes = c(paste("OLS; one observation per participant with two named unions present in the matching group.",
                               "Dependent variable: prior belief about the compliance of named union 1 minus that about named",
                               "union 2, at the start of Stage 2. Differencing removes everything common to the participant,",
                               "including what was learned in Stage 1 about out-group members in general. Trust, previous",
                               "conflict and experience are four-point scales rescaled to [0,1]. Placebo regressors: the Stage 1",
                               "compliance of the members of each union the participant was grouped with, and of all members of",
                               "each union in the matching group. In Stage 1 both outside unions were displayed alike, so",
                               "participants could not attribute this behaviour to a specific union. The constant captures the",
                               "order in which the two unions were asked about."), boot_note(fits_J2[[1]]$G), star_note))

  # ==============================================================================
  # J3. Individual difference-in-differences with the TURF, by belief component
  #   Identity does not change in the TURF (in-group only), so the change in TURF
  #   compliance between stages absorbs each participant's general stage trend.
  # ==============================================================================
  fits_J3 <- list(
    `(1) DiD` = lm_boot(did ~ 1, w, seed = seed_main + 520),
    `(2) DiD` = lm_boot(did ~ L_out + S_in + I, w, seed = seed_main + 521),
    `(3) DiD` = lm_boot(did ~ L_out + S_in + I + d_trust + d_conflict, w, seed = seed_main + 522),
    `(4) Δ Shared Area` = lm_boot(d_sa ~ L_out + S_in + I, w, seed = seed_main + 523))
  tableJ3 <- reg_table(fits_J3, c(
    "(Intercept)" = "Constant",
    "L_out"       = "Stage 1 learning, out-group",
    "S_in"        = "Between-stage shift, in-group",
    "I"           = "Identity shift",
    "d_trust"     = "Trust: named unions − generic out-group",
    "d_conflict"  = "Previous conflict: named unions − generic out-group"), gof = c("nobs", "r2"))
  export_table(tableJ3, "Extra_Table_J3_DiD_TURF",
               "Table J3 | Changes in CPR-Shared Area compliance relative to the CPR-TURF, by component of belief change",
               notes = c(paste("OLS; one observation per participant. Dependent variable in Models 1–3: change in average",
                               "CPR-Shared Area compliance between stages minus the change in average CPR-TURF compliance",
                               "(individual difference-in-differences; identity information changes only in the Shared Area).",
                               "Model 4 uses the change in Shared Area compliance alone. The change in the out-group belief",
                               "between the start of the two stages equals Stage 1 learning plus the between-stage shift of the",
                               "in-group belief plus the identity shift (out-group minus in-group change between stages); the three components enter separately."),
                         scope_note, boot_note(fits_J3[[1]]$G), star_note))

  # ==============================================================================
  # J4. Typology: Stage 1 experience x beliefs about the named unions
  #   Negative experience: the out-group members met in Stage 1 complied less, on
  #   average, than the participant expected at the start of Stage 1.
  # ==============================================================================
  w$neg_exp <- w$exp_out_S1 < w$out_T1ini
  typ_row <- function(sel, label) {
    d <- w[sel & !is.na(w$S_out), ]
    c(label, as.character(nrow(d)), sprintf("%.2f", mean(d$exp_out_S1, na.rm = TRUE)),
      sprintf("%.2f", mean(d$out_T1ini)), sprintf("%.2f", mean(d$out_T2ini)),
      shares(d$S_out), sprintf("%.3f", mean(d$I, na.rm = TRUE)))
  }
  tableJ4 <- data.frame(rbind(
    typ_row(w$neg_exp %in% TRUE,  "Negative Stage 1 experience"),
    typ_row(w$neg_exp %in% FALSE, "Neutral or positive Stage 1 experience"),
    typ_row(!is.na(w$neg_exp),    "All")), stringsAsFactors = FALSE)
  names(tableJ4) <- c("Stage 1 experience with out-group members", "Num. obs.",
                      "Compliance of out-group members met", "Belief, start of Stage 1",
                      "Belief about named unions, start of Stage 2",
                      "Named unions vs end of Stage 1: more pessimistic", "Unchanged", "More optimistic",
                      "Mean identity shift")
  export_table(tableJ4, "Extra_Table_J4_Experience_typology",
               "Table J4 | Stage 1 experience with out-group members and prior beliefs about the named unions",
               notes = c(paste("Negative experience: the out-group members a participant was grouped with in Stage 1 (identified",
                               "from the reconstructed groups of each round) complied less, on average, than the participant's",
                               "prior belief about out-group compliance at the start of Stage 1. Beliefs and compliance on the",
                               "[0,1] compliance scale. The shares compare the prior belief about the named unions at the start of",
                               "Stage 2 with the belief about the generic out-group at the end of Stage 1. The identity shift",
                               "nets out the between-stage change in the in-group belief."), scope_note))

  # ==============================================================================
  # J5. Round-level group composition, with Stage 1 as placebo
  #   Group composition is random in every round. In Stage 2 each member's union was
  #   displayed, so if identity matters, compliance should respond to the expected
  #   compliance of the round's out-group members implied by the participant's
  #   beliefs about each named union. In Stage 1 outside members were displayed
  #   alike, so the same measure should not matter. Participants with two named
  #   unions in the matching group; participant and round fixed effects; the number
  #   of in-group members (visible in both stages) is controlled for.
  # ==============================================================================
  if (run_option4) {
    ids <- which(w$use1 & w$use2 & w$in_pool1 & w$in_pool2)
    belief_of <- function(i, u) {
      if (u == w$union[i]) w$in_T2ini[i] else if (u == w$named1[i]) w$b1_ini[i] else
        if (u == w$named2[i]) w$b2_ini[i] else NA_real_ }
    panel <- bind_rows(lapply(c("T1", "T2"), function(st) {
      grp <- if (st == "T1") grp1 else grp2
      bind_rows(lapply(1:8, function(r) {
        y <- comp_round(st, r)
        bind_rows(lapply(ids, function(i) {
          o <- others_in_round(grp, i, r); if (length(o) != 3) return(NULL)
          out <- o[w$union[o] != w$union[i]]; if (length(out) == 0) return(NULL)
          data.frame(pid = w[[participant_var]][i], cl = w[[cluster_var]][i], stage2 = as.integer(st == "T2"),
                     round = r, y = y[i], n_in = sum(w$union[o] == w$union[i]),
                     E_out = mean(vapply(w$union[out], function(u) belief_of(i, u), 0)))
        }))
      }))
    }))
    names(panel)[names(panel) == "pid"] <- participant_var
    names(panel)[names(panel) == "cl"]  <- cluster_var
    panel$fe_p <- panel[[participant_var]]
    panel$fe_i <- paste(panel[[participant_var]], panel$stage2)
    panel$fe_r <- paste(panel$round, panel$stage2)
    keep6 <- c("E_out", "n_in", "E_out:stage2", "stage2:n_in")
    fits_J5 <- list(
      `Stage 1 (placebo)` = lm_boot(y ~ E_out + n_in + factor(round) + factor(fe_p),
                                    panel[panel$stage2 == 0, ], seed = seed_main + 530, keep = keep6),
      `Stage 2` = lm_boot(y ~ E_out + n_in + factor(round) + factor(fe_p),
                          panel[panel$stage2 == 1, ], seed = seed_main + 531, keep = keep6),
      `Pooled` = lm_boot(y ~ E_out + E_out:stage2 + n_in + stage2:n_in + factor(fe_r) + factor(fe_i),
                         panel, seed = seed_main + 532, keep = keep6))
    tableJ5 <- reg_table(fits_J5, c(
      "E_out"        = "Expected compliance of out-group members in the group",
      "E_out:stage2" = "   × Stage 2 (difference)",
      "n_in"         = "In-group members in the group",
      "stage2:n_in"  = "   × Stage 2"), gof = c("nobs"))
    export_table(tableJ5, "Extra_Table_J5_Round_composition",
                 "Table J5 | Round-level compliance and the identity composition of the group",
                 notes = c(paste("OLS of individual CPR-Shared Area compliance in each round. Expected compliance of out-group",
                                 "members: the mean, over the out-group members in the participant's group that round, of the",
                                 "participant's prior belief (start of Stage 2) about the compliance of each member's union.",
                                 "Group composition is random in every round; in Stage 2 each member's union was displayed,",
                                 "while in Stage 1 all outside members were displayed alike, so Stage 1 serves as a placebo.",
                                 "All models include participant and round fixed effects (by stage in the pooled model).",
                                 "Sample: participants with two named unions in their matching group; rounds with at least one",
                                 "out-group member and a reconstructed group."), boot_note(fits_J5[[1]]$G), star_note))
  }
}
