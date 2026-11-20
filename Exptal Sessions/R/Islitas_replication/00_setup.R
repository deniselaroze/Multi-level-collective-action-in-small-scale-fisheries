# ==============================================================================
# Replication package -- 00_setup.R
# "Multi-level collective action in small-scale fisheries" (Islitas experiment)
#
# Loads packages, paths, settings, data and all helper functions used by the
# analysis scripts (01_descriptives.R to 05_identity_vs_learning.R). Every
# analysis script sources this file, so settings are edited only here (paths
# only in 00_paths.R).
#
# Inference: every standard error, confidence interval and p-value in the
# package comes from a wild cluster bootstrap with one weight per matching
# group (section 6).
# ==============================================================================

# --- 1. Packages ----------------------------------------------------------------
pkgs <- c("dplyr", "tidyr", "lavaan", "lme4", "ggplot2", "flextable", "officer", "survival")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Please install: ", paste(missing_pkgs, collapse = ", "),
       "\n  install.packages(c(", paste0('"', missing_pkgs, '"', collapse = ", "), "))")
}
suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))

# Table labels contain UTF-8 characters (λ₁, γ, R², †), which need R >= 4.2 on Windows,
# or a UTF-8 locale on Linux and macOS
if (!isTRUE(l10n_info()[["UTF-8"]])) {
  warning("R is not running in a UTF-8 locale, so Greek letters, subscripts and other symbols may be ",
          "garbled in the Word tables. Use R >= 4.2 on Windows, or a UTF-8 locale (e.g. LANG=en_US.UTF-8).")
}

# --- 2. Settings ----------------------------------------------------------------
cluster_var     <- "gid.treat"         # matching group
participant_var <- "participant.code"  # participant

# WILD CLUSTER BOOTSTRAP -- used for every standard error in the package
#   OLS:           Tables 1, 2, SM.D.4, SM.F.2 (and the optional J1-J3, J5)
#   SEM:           Tables 3, 4, SM.D.5, SM.E.2, SM.E.4; Figures SM.E.1, SM.E.2
#                  (estimates in Tables SM.E.3, SM.E.5)
#   Mixed models:  Table SM.E.1
#   Tobit (score): Table SM.E.6; Figure SM.E.3 (estimates in Table SM.E.7)
#   Means:         Table SM.F.1
# In each replication, every cluster gets one random weight (mean 0, variance 1)
# that is shared by all its observations: all participants, rounds, areas, stages
# and equations. Outcomes are rebuilt as fitted value + residual x weight and the
# model is re-estimated (details in section 6). The data are never resampled.
#   boot_cluster : cluster_var     -> one weight per matching group (default)
#                  participant_var -> one weight per participant
#   wild_weights : "webb"       -> Webb six-point distribution (recommended with few clusters)
#                  "rademacher" -> -1 or +1 with equal probability
boot_cluster <- cluster_var
wild_weights <- "webb"

B_boot <- 2000   # bootstrap replications for all tables
B_sens <- 1000   # replications per specification in Figures SM.E.1-SM.E.3 (Round 8 uses B_boot)

# Bootstrap standard error shown in parentheses:
#   "iqr" -> interquartile range of the bootstrap estimates / 1.349 (robust; recommended
#            because the lambdas are ratios that explode when gamma is near zero in a few draws)
#   "sd"  -> standard deviation of the bootstrap estimates
# p-values and stars do not depend on this: they are always percentile bootstrap p-values.
boot_se <- "iqr"

seed_main <- 478

# quick_run = TRUE runs everything with few replications to check the pipeline.
# Its outputs go to the subfolder Outputs/Replication/quick_run, so a test never
# overwrites the full results.
quick_run <- FALSE
if (quick_run) { B_boot <- 99; B_sens <- 49 }

# R's default random number generators (R >= 3.6.0), set explicitly so that every
# computer draws the same bootstrap weights
RNGkind("Mersenne-Twister", "Inversion", "Rejection")

# --- 3. Paths (edit in 00_paths.R) -------------------------------------------------
source("00_paths.R")
if (quick_run) path_out <- file.path(path_out, "quick_run")
dir.create(path_out, recursive = TRUE, showWarnings = FALSE)

# --- 4. Data --------------------------------------------------------------------
# Built from the raw exports by 00_data_management.R
if (!file.exists(file_wide) || !file.exists(file_long)) {
  stop("Analysis data not found. Run 00_data_management.R first (or run_all.R).")
}
load(file_wide)  # wide data: df
load(file_long)  # long data: dfs_long
df_wide <- as.data.frame(df)
df_long <- as.data.frame(dfs_long)
rm(df, dfs_long)

# Checks on the bootstrap cluster
if (!boot_cluster %in% c(cluster_var, participant_var)) {
  warning("boot_cluster = '", boot_cluster, "' is neither the matching group nor the participant.")
}
for (nm in c("df_wide", "df_long")) {
  if (!boot_cluster %in% names(get(nm))) stop("'", boot_cluster, "' not found in ", nm)
  if (anyNA(get(nm)[[boot_cluster]]))    stop("'", boot_cluster, "' has missing values in ", nm)
}
rm(nm)
if (boot_cluster != participant_var) {
  n_by_part <- tapply(df_long[[boot_cluster]], df_long[[participant_var]],
                      function(x) length(unique(x)))
  if (any(n_by_part > 1)) {
    warning("'", boot_cluster, "' changes within participant in the long data. ",
            "The wild cluster bootstrap assumes each participant belongs to one cluster.")
  }
}

# Clusters in C-locale order (method = "radix"), so that every computer assigns the
# same bootstrap weights to the same cluster, whatever its language settings
clusters_all <- sort(unique(as.character(df_wide[[boot_cluster]])), method = "radix")
n_units <- length(clusters_all)
boot_unit_label <- if (boot_cluster == cluster_var) "matching groups" else
  if (boot_cluster == participant_var) "participants" else boot_cluster
boot_unit_one <- if (boot_cluster == cluster_var) "matching group" else
  if (boot_cluster == participant_var) "participant" else boot_cluster
message("Wild cluster bootstrap: ", if (wild_weights == "webb") "Webb" else "Rademacher",
        " weights by ", boot_unit_one, " (", boot_cluster, "), ", n_units, " clusters | participants: ",
        nrow(df_wide), " | replications: ", B_boot)

# --- 5. Formatting and Word export ----------------------------------------------
fmt_pct <- function(x, digits = 0) {
  ifelse(is.na(x), "–", sprintf(paste0("%.", digits, "f%%"), x * 100))
}
fmt_msd <- function(x) {
  if (all(is.na(x))) return("–")
  sprintf("%.2f (%.2f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
}
stars <- function(p) {
  dplyr::case_when(is.na(p) ~ "", p < 0.001 ~ "***", p < 0.01 ~ "**",
                   p < 0.05 ~ "*", p < 0.1 ~ "†", TRUE ~ "")
}
fmt_coef <- function(est, se, p, digits = 3) {
  sprintf(paste0("%.", digits, "f%s (%.", digits, "f)"), est, stars(p), se)
}
star_note <- "†p < 0.1, *p < 0.05, **p < 0.01, ***p < 0.001."

# Inference sentence used in every table note (manuscript style). model says how the
# outcomes are rebuilt in each replication:
#   "ols", "sem", "lmm" (mixed model), "tobit", "sem+tobit" (Table SM.E.6), "means" (Table SM.F.1)
boot_note <- function(G = n_units, B = B_boot, model = "ols") {
  w_txt <- if (boot_cluster == participant_var) "the weight of the participant" else
    paste0("the weight of the participant's ", boot_unit_one)
  newton_txt <- paste0(" and the estimates are updated by one Newton step (score wild bootstrap;",
                       " Kline and Santos, 2012)")
  sem_txt <- paste0("the outcomes of the SEM equations are rebuilt as fitted values plus residuals",
                    " multiplied by ", w_txt, " (the same weight for all equations and both stages)")
  how <- switch(model,
    ols   = paste0("in each replication, the dependent variable is rebuilt as the fitted value plus the",
                   " residual multiplied by ", w_txt, ", and the model is re-estimated."),
    sem   = paste0("in each replication, ", sem_txt, ", and the SEM is re-estimated."),
    lmm   = paste0("in each replication, compliance is rebuilt as the fixed-effects prediction plus the",
                   " marginal residual (participant random intercept plus error) multiplied by ", w_txt,
                   ", and the mixed model is re-estimated."),
    tobit = paste0("in each replication, the score contributions of the tobit model are multiplied by ",
                   w_txt, newton_txt, "."),
    `sem+tobit` = paste0("in each replication, ", sem_txt, " and the SEM is re-estimated; for the tobit",
                         " model, the score contributions are multiplied by the same weight", newton_txt, "."),
    means = paste0("in each replication, each variable is rebuilt as its sample mean plus the deviation",
                   " from the mean multiplied by ", w_txt, ", and the statistics are recomputed."),
    stop("Unknown model in boot_note(): ", model))
  paste0("Standard errors, reported in parentheses, are based on ", format(B, big.mark = ","),
         " wild cluster bootstrap replications with ",
         if (wild_weights == "webb") "Webb six-point" else "Rademacher",
         " weights drawn for each of the ", G, " ", boot_unit_label, ": ", how, " Standard errors are ",
         if (boot_se == "iqr") "the interquartile range of the bootstrap estimates divided by 1.349" else
           "the standard deviation of the bootstrap estimates",
         ", and significance levels are based on percentile bootstrap p-values ",
         "(p < 0.05 when the 95% percentile confidence interval excludes zero).")
}

# --- Word export -----------------------------------------------------------------
# Each table is written to its own .docx as: caption (bold, left-aligned),
# table, notes. File names start with the item's number in the manuscript
# (Table_3_..., Table_SM.E.2_..., Figure_SM.E.1_...); outputs that are not in the
# manuscript start with Extra_. Supplementary items (every name that does not start
# with Table_<number>) get the "Notes:" prefix and the Supplementary caption style,
# as in the manuscript. run_all.R also collects every table into All_tables.docx.
#   header_top : optional spanning header row, e.g. c(" " = 1, "Stage 1" = 2, "Stage 2" = 2)
#   col_labels : optional visible column labels (replaces the data frame names)
word_tables <- list()
word_font   <- "Times New Roman"

export_table <- function(tab, name, title, notes = NULL, header_top = NULL, col_labels = NULL) {
  tab <- as.data.frame(tab, stringsAsFactors = FALSE)
  supp <- !grepl("^Table_[0-9]", name)
  # Row labels that start with spaces are shown indented in Word
  indent <- grepl("^\\s+", tab[[1]])
  tab[[1]] <- trimws(tab[[1]])
  ft <- flextable::flextable(tab)
  if (!is.null(col_labels)) {
    ft <- flextable::set_header_labels(ft, values = setNames(as.list(col_labels), names(tab)))
  }
  if (!is.null(header_top)) {
    ft <- flextable::add_header_row(ft, values = names(header_top), colwidths = unname(header_top))
  }
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::font(ft, fontname = word_font, part = "all")
  ft <- flextable::fontsize(ft, size = 10, part = "all")
  ft <- flextable::align(ft, j = seq(2, max(2, ncol(tab))), align = "center", part = "all")
  ft <- flextable::align(ft, j = 1, align = "left", part = "all")
  if (any(indent)) ft <- flextable::padding(ft, i = which(indent), j = 1, padding.left = 18)
  ft <- flextable::autofit(ft)

  cap <- officer::fpar(
    officer::ftext(title, officer::fp_text(bold = TRUE, font.size = if (supp) 10.5 else 11,
                                           font.family = word_font,
                                           color = if (supp) "#26323F" else "#000000")),
    fp_p = officer::fp_par(text.align = "left", padding.top = 6, padding.bottom = 4))
  note_par <- NULL
  if (length(notes) > 0) {
    note_par <- officer::fpar(
      officer::ftext(paste0(if (supp) "Notes: " else "", paste(notes, collapse = " ")),
                     officer::fp_text(font.size = if (supp) 9 else 10, font.family = word_font)),
      fp_p = officer::fp_par(text.align = "justify", padding.top = 4))
  }
  item <- list(caption = cap, table = ft, notes = note_par)
  doc <- add_table_to_doc(officer::read_docx(), item)
  print(doc, target = file.path(path_out, paste0(name, ".docx")))
  word_tables[[name]] <<- item
  message("Saved: ", name, ".docx")
  invisible(ft)
}

add_table_to_doc <- function(doc, item) {
  doc <- officer::body_add_fpar(doc, item$caption)
  doc <- flextable::body_add_flextable(doc, item$table, align = "left")
  if (!is.null(item$notes)) doc <- officer::body_add_fpar(doc, item$notes)
  doc
}

# Writes every table exported in this session into one Word file, in manuscript order:
# main tables (Table_1_ ... Table_4_), Supplementary Material tables by section and
# number (Table_SM.D.1_ ... Table_SM.F.2_), then the outputs not in the manuscript (Extra_)
export_all_tables <- function(file = "All_tables.docx") {
  if (length(word_tables) == 0) return(invisible(NULL))
  nm   <- names(word_tables)
  main <- nm[grepl("^Table_[0-9]+_", nm)]
  sm   <- nm[grepl("^Table_SM\\.[A-Z]\\.[0-9]+_", nm)]
  main <- main[order(as.numeric(sub("^Table_([0-9]+)_.*", "\\1", main)))]
  sm   <- sm[order(sub("^Table_SM\\.([A-Z])\\..*", "\\1", sm),
                   as.numeric(sub("^Table_SM\\.[A-Z]\\.([0-9]+)_.*", "\\1", sm)), method = "radix")]
  ord  <- c(main, sm, sort(setdiff(nm, c(main, sm)), method = "radix"))
  doc <- officer::read_docx()
  for (k in seq_along(ord)) {
    doc <- add_table_to_doc(doc, word_tables[[ord[k]]])
    if (k < length(ord)) doc <- officer::body_add_break(doc)
  }
  print(doc, target = file.path(path_out, file))
  message("Saved: ", file, " (", length(ord), " tables)")
}

# --- 6. Wild cluster bootstrap ----------------------------------------------------
# The data are kept fixed. In each replication b, every cluster g gets a weight w_bg
# with mean 0 and variance 1, and each outcome is rebuilt as
#     y* = fitted value + residual x w_bg   (g = the observation's cluster),
# so all observations of a cluster (and all equations and stages) share the same
# weight and their correlation is preserved. The model is re-estimated on y*.

# Weights: B x G matrix, one row per replication and one column per cluster
# (clusters_all). The same seed gives the same weights, so analyses run with the same
# seed (Stage 1 and Stage 2, or Table 3 and Tables SM.E.4 and SM.E.6) share them in every
# replication.
wild_weight_matrix <- function(B, seed) {
  vals <- if (wild_weights == "webb") c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5)) else c(-1, 1)
  set.seed(seed)
  matrix(sample(vals, B * n_units, replace = TRUE), nrow = B, ncol = n_units, byrow = TRUE,
         dimnames = list(NULL, clusters_all))
}

# Column of the weight matrix that belongs to each row of d
cluster_index <- function(d) {
  g <- match(as.character(d[[boot_cluster]]), clusters_all)
  if (anyNA(g)) stop("Some rows have a cluster (", boot_cluster, ") that is not in the wide data.")
  g
}
n_clusters <- function(d) length(unique(d[[boot_cluster]]))

# OLS. Because OLS is linear in y, the coefficients re-estimated on
# y* = fitted + residual x weight are b + (X'X)^-1 X'(residual x weight). This computes
# all replications at once and gives exactly the same draws as re-running lm() on each y*.
#   qx: QR decomposition of the design matrix (e.g. lm()$qr); b: coefficients;
#   res: residuals; g: cluster_index() of the rows; W: wild_weight_matrix()
ols_wild_draws <- function(qx, b, res, g, W) {
  E   <- res * t(W)[g, , drop = FALSE]   # n x B: each residual times the weight of its cluster
  out <- t(b + qr.coef(qx, E))           # B x p
  colnames(out) <- names(b)
  out
}
ols_wild <- function(X, y, g, W) {
  qx <- qr(X); b <- qr.coef(qx, y)
  list(est = b, draws = ols_wild_draws(qx, b, qr.resid(qx, y), g, W))
}

# Linear mixed model: y* = X b + (y - X b) x weight, where y - X b is the marginal
# residual (random intercept + error), so all observations of a cluster keep their
# joint correlation (Modugno and Giannerini, 2015); the model is re-fitted on y* with
# lme4::refit(). data must hold the rows used by the model, in the same order.
lmm_wild <- function(m, data, B = B_boot, seed = seed_main, control = lme4::lmerControl()) {
  b    <- lme4::fixef(m)
  yhat <- drop(lme4::getME(m, "X") %*% b)
  res  <- lme4::getME(m, "y") - yhat
  g    <- cluster_index(data)
  if (length(g) != length(res)) stop("lmm_wild(): the data and the model have different rows.")
  W <- wild_weight_matrix(B, seed)
  draws <- matrix(NA_real_, B, length(b), dimnames = list(NULL, names(b)))
  for (r in seq_len(B)) {
    fit <- tryCatch(suppressMessages(suppressWarnings(
      lme4::refit(m, newresp = yhat + res * W[r, g], control = control))), error = function(e) NULL)
    if (!is.null(fit)) draws[r, ] <- lme4::fixef(fit)
  }
  list(est = b, draws = draws, G = n_clusters(data))
}

# Maximum-likelihood models without an additive error (tobit): score wild bootstrap
# (Kline and Santos, 2012). theta* = theta + V sum_g w_g s_g, where s_g is the sum of
# the score contributions of cluster g and V the inverse Hessian of minus the
# log-likelihood (one Newton step from the estimates). For OLS this is exactly the
# wild bootstrap of ols_wild_draws().
#   S: n x k score contributions; V: k x k; theta: estimates (length k); g, W as above
score_wild_draws <- function(S, V, theta, g, W) {
  Sg  <- rowsum(S, g)                                  # cluster sums of the scores
  Wg  <- W[, as.integer(rownames(Sg)), drop = FALSE]   # weights of the clusters in the sample
  out <- sweep(Wg %*% Sg %*% V, 2, theta, "+")
  colnames(out) <- names(theta)
  out
}

# Statistics that are functions of sample means (Table SM.F.1). Each variable is rebuilt as
# mean + (x - mean) x weight (an intercept-only regression), so its bootstrap mean is
# mean + sum_i (x_i - mean) w_g(i) / n. stat() takes a named vector of means.
wild_means <- function(data, vars, stat, B = B_boot, seed = seed_main) {
  X  <- as.matrix(data[, vars, drop = FALSE])
  mu <- colMeans(X)
  Cg <- rowsum(sweep(X, 2, mu), cluster_index(data))
  Wg <- wild_weight_matrix(B, seed)[, as.integer(rownames(Cg)), drop = FALSE]
  M  <- sweep(Wg %*% Cg / nrow(X), 2, mu, "+")
  colnames(M) <- vars
  list(est = stat(mu), draws = t(apply(M, 1, stat)), G = n_clusters(data))
}

# Bootstrap SE (IQR/1.349 or SD of the replications, see boot_se), percentile
# p-value, and 95% percentile confidence interval. Failed replications are dropped.
# The percentile p-value and the percentile CI always agree: p < 0.05 if and only
# if the 95% CI excludes zero (and p < 0.1 if and only if the 90% CI excludes zero).
boot_summary <- function(est, draws) {
  d <- draws[complete.cases(draws), names(est), drop = FALSE]
  se <- if (boot_se == "iqr") apply(d, 2, IQR) / 1.349 else apply(d, 2, sd)
  # Percentile p-value: 2 x share of draws on the other side of zero (capped at 1)
  p  <- pmin(1, 2 * pmin(colMeans(d <= 0), colMeans(d >= 0)))
  out <- data.frame(term = names(est), est = unname(est), se = unname(se),
                    z = unname(est / se), pvalue = unname(p),
                    ci.lower = apply(d, 2, quantile, 0.025, names = FALSE),
                    ci.upper = apply(d, 2, quantile, 0.975, names = FALSE),
                    row.names = NULL)
  attr(out, "n_ok") <- nrow(d)
  out
}

# --- 7. OLS with wild cluster bootstrap inference ---------------------------------
# keep: coefficients to report (default: all), e.g. to leave out fixed-effect dummies.
lm_boot <- function(formula, data, B = B_boot, seed = seed_main, keep = NULL) {
  vars <- intersect(unique(c(all.vars(formula), boot_cluster)), names(data))
  d <- data[complete.cases(data[, vars]), , drop = FALSE]
  m <- lm(formula, data = d)
  b <- coef(m)
  if (is.null(keep)) keep <- names(b)
  keep <- intersect(keep, names(b)[!is.na(b)])
  draws <- ols_wild_draws(m$qr, b, residuals(m), cluster_index(d),
                          wild_weight_matrix(B, seed))[, keep, drop = FALSE]
  coefs <- boot_summary(b[keep], draws)
  # Joint test that all reported slopes are zero: Wald test with the bootstrap covariance matrix
  slopes <- setdiff(keep, "(Intercept)")
  F_boot <- F_p <- NA_real_
  if (length(slopes) > 0) {
    V <- cov(draws[complete.cases(draws), slopes, drop = FALSE])
    W <- tryCatch(drop(t(b[slopes]) %*% solve(V) %*% b[slopes]), error = function(e) NA_real_)
    F_boot <- W / length(slopes)
    F_p    <- pchisq(W, df = length(slopes), lower.tail = FALSE)
  }
  list(model = m, coefs = coefs, G = n_clusters(d), F_boot = F_boot, F_p = F_p,
       n_ok = attr(coefs, "n_ok"))
}

# Assembles a regression table from lm_boot() results.
reg_table <- function(fits, coef_map, gof = c("nobs", "r2", "adj_r2")) {
  rows <- lapply(names(coef_map), function(term) {
    vals <- vapply(fits, function(f) {
      r <- f$coefs[f$coefs$term == term, ]
      if (nrow(r) == 0) "–" else fmt_coef(r$est, r$se, r$pvalue)
    }, character(1))
    c(Predictor = unname(coef_map[term]), vals)
  })
  gof_rows <- list(
    nobs   = c(Predictor = "Num. obs.", vapply(fits, function(f) as.character(nobs(f$model)), "")),
    r2     = c(Predictor = "R²", vapply(fits, function(f) sprintf("%.3f", summary(f$model)$r.squared), "")),
    adj_r2 = c(Predictor = "Adjusted R²", vapply(fits, function(f) sprintf("%.3f", summary(f$model)$adj.r.squared), "")),
    F      = c(Predictor = "F statistic",
               vapply(fits, function(f) paste0(sprintf("%.3f", f$F_boot), stars(f$F_p)), "")),
    G      = c(Predictor = paste0("Bootstrap clusters (", boot_unit_label, ")"),
               vapply(fits, function(f) as.character(f$G), ""))
  )
  out <- as.data.frame(do.call(rbind, c(rows, gof_rows[gof])), stringsAsFactors = FALSE)
  names(out) <- c("Predictor", names(fits))
  out
}

# --- 8. SEM helpers -------------------------------------------------------------
declare_get_columns <- function(prefix, suffix, start, end = NULL, data) {
  if (is.null(end)) end <- start
  unlist(lapply(start:end, function(r) {
    grep(paste0("^", prefix, "\\.", r, "\\.player\\.", suffix, "$"), names(data), value = TRUE)
  }))
}

# Stage 1 (unknown out-group). Compliance = mean over rounds R_start..R_end.
# Observed compliance of the other group members = mean over the obs_k rounds
# preceding R_start (default obs_k = 1: round R_start - 1; never before round 1).
prep_T1 <- function(data, R_start, R_end, obs_k = 1) {
  cols <- declare_get_columns("T1juegoalgas", "T1_extraccion_libre", R_start, R_end, data)
  data$average_compliance_ini  <- 1 - rowMeans(data[, cols, drop = FALSE], na.rm = TRUE) / 50
  data$belief_compliance_pm    <- 1 - data$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50
  data$belief_compliance_union <- 1 - data$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50
  data$confianza_caleta <- as.numeric(scale(data$survey1.1.player.confianza_caleta))
  data$conflicto_caleta <- as.numeric(scale(data$survey1.1.player.conflicto_caleta))
  data$confianza_pm     <- as.numeric(scale(data$survey1.1.player.confianza_pm))
  data$conflicto_pm     <- as.numeric(scale(data$survey1.1.player.conflicto_pm))
  cols_obs <- declare_get_columns("T1juegoalgas", "T1_extraccion_otros_libre",
                                  max(1, R_start - obs_k), R_start - 1, data)
  data$average_compliance_observed_ini_lag <- 1 - rowMeans(data[, cols_obs, drop = FALSE], na.rm = TRUE) / 150
  data
}

# Stage 2 (known out-group)
prep_T2 <- function(data, R_start, R_end, obs_k = 1) {
  cols <- declare_get_columns("T2juegoalgas", "T2_extraccion_metat", R_start, R_end, data)
  data$average_compliance_ini  <- 1 - rowMeans(data[, cols, drop = FALSE], na.rm = TRUE) / 50
  data$belief_compliance_pm    <- 1 - data$beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini / 50
  data$belief_compliance_union <- 1 - data$beliefsT2inicial.1.player.T2_belief_caleta_ini / 50
  data$confianza_caleta <- as.numeric(scale(data$survey1.1.player.confianza_caleta))
  data$conflicto_caleta <- as.numeric(scale(data$survey1.1.player.conflicto_caleta))
  data$confianza_metat  <- as.numeric(scale(data$survey2.1.player.confianza_caleta_conocida_mean))
  data$conflicto_metat  <- as.numeric(scale(data$survey2.1.player.conflicto_caleta_conocida_mean))
  cols_obs <- declare_get_columns("T2juegoalgas", "T2_extraccion_otros_metat",
                                  max(1, R_start - obs_k), R_start - 1, data)
  data$average_compliance_observed_ini_lag <- 1 - rowMeans(data[, cols_obs, drop = FALSE], na.rm = TRUE) / 150
  data
}

sem_syntax <- function(trust_out, conflict_out) {
  paste0('
    belief_compliance_pm ~ ', trust_out, ' + ', conflict_out, '
    belief_compliance_union ~ confianza_caleta + conflicto_caleta
    average_compliance_ini ~ c1*belief_compliance_pm + c2*belief_compliance_union + ',
         trust_out, ' + ', conflict_out, ' + confianza_caleta + conflicto_caleta + c3*average_compliance_observed_ini_lag
    belief_compliance_pm ~ 1
    belief_compliance_union ~ 1
    average_compliance_ini ~ 1
    gamma_cc     := c1 + c2 + c3
    lambda_pm    := c1 / (c1 + c2 + c3)
    lambda_union := c2 / (c1 + c2 + c3)
    lambda_obs   := c3 / (c1 + c2 + c3)
  ')
}
model_T1 <- sem_syntax("confianza_pm", "conflicto_pm")
model_T2 <- sem_syntax("confianza_metat", "conflicto_metat")

# Parameter names: "lhs op rhs" for regressions/intercepts, the label for := parameters
param_names <- function(pe) ifelse(pe$op == ":=", pe$lhs, paste(pe$lhs, pe$op, pe$rhs))

get_est <- function(fit) {
  pe <- parameterEstimates(fit, se = FALSE, zstat = FALSE, pvalue = FALSE, ci = FALSE)
  pe <- pe[pe$op %in% c("~", "~1", ":="), ]
  setNames(pe$est, param_names(pe))
}

# Re-estimates the SEM on a bootstrap sample (NA if it fails or does not converge)
safe_est <- function(model, data, template) {
  out <- setNames(rep(NA_real_, length(template)), template)
  fit <- tryCatch(suppressWarnings(sem(model, data = data, estimator = "ML",
                                       se = "none", test = "none")),
                  error = function(e) NULL)
  if (is.null(fit) || !isTRUE(lavInspect(fit, "converged"))) return(out)
  est <- get_est(fit)
  out[intersect(names(est), template)] <- est[intersect(names(est), template)]
  out
}

# Adds bootstrap SE, z, p and percentile CI to lavaan's parameter table
summarise_boot <- function(fit, draws) {
  pe <- parameterEstimates(fit, se = FALSE, zstat = FALSE, pvalue = FALSE, ci = FALSE)
  pe <- pe[pe$op %in% c("~", "~1", ":="), ]
  bs <- boot_summary(setNames(pe$est, param_names(pe)), draws)
  pe <- cbind(pe, bs[, c("se", "z", "pvalue", "ci.lower", "ci.upper")])
  attr(pe, "n_ok") <- attr(bs, "n_ok")
  pe
}

# The three SEM equations of one stage, estimated by OLS on the SEM estimation sample
# (complete cases, as lavaan's listwise deletion). With this recursive model and no
# residual covariances between equations, the ML estimates of each equation equal OLS
# (checked in run_sem_spec()), so these fitted values and residuals are those of the SEM.
sem_equations <- function(d, trust_out, conflict_out) {
  rhs_c <- c("belief_compliance_pm", "belief_compliance_union", trust_out, conflict_out,
             "confianza_caleta", "conflicto_caleta", "average_compliance_observed_ini_lag")
  d <- d[complete.cases(d[, c("average_compliance_ini", rhs_c)]), , drop = FALSE]
  list(d = d, g = cluster_index(d),
       pm = lm(reformulate(c(trust_out, conflict_out), "belief_compliance_pm"), d),
       un = lm(belief_compliance_union ~ confianza_caleta + conflicto_caleta, d),
       c  = lm(reformulate(rhs_c, "average_compliance_ini"), d))
}

# Largest absolute difference between the SEM (ML) and OLS estimates of the three equations
sem_ols_gap <- function(fit, eq) {
  est <- get_est(fit)
  gap <- function(m, lhs) {
    b <- coef(m)
    key <- ifelse(names(b) == "(Intercept)", paste(lhs, "~1", ""), paste(lhs, "~", names(b)))
    max(abs(est[key] - b))
  }
  max(gap(eq$pm, "belief_compliance_pm"), gap(eq$un, "belief_compliance_union"),
      gap(eq$c, "average_compliance_ini"))
}

# Rebuilds the outcomes with the weights w of one replication (one per cluster, in the
# order of clusters_all). Beliefs are rebuilt first and enter the compliance equation,
# because they are outcomes of the SEM themselves.
wild_data <- function(eq, w) {
  d  <- eq$d
  wi <- w[eq$g]
  d$belief_compliance_pm    <- fitted(eq$pm) + residuals(eq$pm) * wi
  d$belief_compliance_union <- fitted(eq$un) + residuals(eq$un) * wi
  d$average_compliance_ini  <- predict(eq$c, newdata = d) + residuals(eq$c) * wi
  d
}

# Runs the two-stage SEM for one outcome window. Stage 1 and Stage 2 are re-estimated
# with the same weights in every replication, so cross-stage differences (Table SM.E.2)
# can be tested.
run_sem_spec <- function(R_start, R_end, B, seed, obs_k = 1) {
  d1 <- prep_T1(df_wide, R_start, R_end, obs_k)
  d2 <- prep_T2(df_wide, R_start, R_end, obs_k)
  fit1 <- sem(model_T1, data = d1, estimator = "ML")
  fit2 <- sem(model_T2, data = d2, estimator = "ML")
  tmpl1 <- names(get_est(fit1)); tmpl2 <- names(get_est(fit2))
  eq1 <- sem_equations(d1, "confianza_pm", "conflicto_pm")
  eq2 <- sem_equations(d2, "confianza_metat", "conflicto_metat")
  gap <- max(sem_ols_gap(fit1, eq1), sem_ols_gap(fit2, eq2))
  if (is.na(gap) || gap > 1e-4) {
    warning("SEM (ML) and equation-by-equation OLS estimates differ (largest difference: ",
            signif(gap, 3), "). The wild bootstrap rebuilds the SEM outcomes from the OLS fits, ",
            "which requires both to coincide.")
  }
  W <- wild_weight_matrix(B, seed)
  draws1 <- matrix(NA_real_, B, length(tmpl1), dimnames = list(NULL, tmpl1))
  draws2 <- matrix(NA_real_, B, length(tmpl2), dimnames = list(NULL, tmpl2))
  for (r in seq_len(B)) {
    draws1[r, ] <- safe_est(model_T1, wild_data(eq1, W[r, ]), tmpl1)
    draws2[r, ] <- safe_est(model_T2, wild_data(eq2, W[r, ]), tmpl2)
  }
  list(fit1 = fit1, fit2 = fit2, draws1 = draws1, draws2 = draws2,
       G = length(unique(c(eq1$g, eq2$g))),
       pe1 = summarise_boot(fit1, draws1), pe2 = summarise_boot(fit2, draws2))
}

# Formats one SEM parameter as "est*** (se)"
pe_cell <- function(pe, lhs, op, rhs = NULL) {
  keep <- pe$lhs == lhs & pe$op == op
  if (!is.null(rhs)) keep <- keep & pe$rhs %in% rhs
  r <- pe[keep, ]
  if (nrow(r) == 0) return("–")
  fmt_coef(r$est[1], r$se[1], r$pvalue[1])
}

# --- 9. Figures: labels, theme, and lambda sensitivity (Figures SM.E.1-SM.E.3) --
# Labels follow the manuscript notation (Table 3).
lam_labels <- c(lambda_union = "Prior belief in-groups (λ₁)",
                lambda_pm    = "Prior belief out-groups (λ₂)",
                lambda_obs   = "Compliance in previous round (λ₃)")
lam_cols   <- c(lambda_union = "#440154", lambda_pm = "#2A788E", lambda_obs = "#7AD151")
gamma_label  <- "Conditional cooperation coefficient (γ)"
stage_labels <- c("Stage 1 (Unknown identity)", "Stage 2 (Known identity)")

# Text sizes are set for figures printed about 6.5 inches wide: a 12-inch figure
# with base size 22 prints at roughly 11-12 pt.
fig_base  <- 22
fig_theme <- function() {
  theme_minimal(base_size = fig_base) +
    theme(axis.text        = element_text(size = fig_base, colour = "black"),
          axis.title       = element_text(size = fig_base + 2),
          axis.title.x     = element_text(margin = margin(t = 12)),
          axis.title.y     = element_text(margin = margin(r = 12)),
          strip.text       = element_text(size = fig_base + 4, face = "bold"),
          legend.text      = element_text(size = fig_base),
          legend.title     = element_blank(),
          legend.position  = "bottom",
          legend.direction = "horizontal",
          legend.key.width = unit(2.5, "lines"),
          panel.grid.minor = element_blank(),
          panel.spacing    = unit(2.5, "lines"),
          plot.margin      = margin(15, 25, 15, 15))
}

# Single rounds 2-8 (round 1 has no lagged feedback) and windows [a, 8]
sens_specs <- c(lapply(2:8, function(r) list(start = r, end = r, label = paste("Round", r))),
                lapply(c(2, 5, 6, 7), function(a) list(start = a, end = 8,
                                                       label = sprintf("Rounds %d–8 (mean)", a))))
spec_levels <- c(paste("Round", 8:2), sprintf("Rounds %d–8 (mean)", c(7, 6, 5, 2)))

# Runs run_fun(R_start, R_end, B, seed) for every specification; Round 8 reuses
# main_res (the main specification). run_fun must return list(pe1, pe2) with
# columns lhs, est, se, pvalue, ci.lower, ci.upper and attribute n_ok.
run_sensitivity <- function(run_fun, main_res, label = "Sensitivity") {
  bind_rows(lapply(seq_along(sens_specs), function(i) {
    s <- sens_specs[[i]]
    message(label, ": ", s$label)
    res <- if (s$start == 8 && s$end == 8) main_res else
      run_fun(s$start, s$end, B_sens, seed_main + 100 + i)
    bind_rows(res$pe1 %>% mutate(Stage = stage_labels[1], n_boot_ok = attr(res$pe1, "n_ok")),
              res$pe2 %>% mutate(Stage = stage_labels[2], n_boot_ok = attr(res$pe2, "n_ok"))) %>%
      filter(lhs %in% names(lam_labels)) %>%
      mutate(spec = s$label)
  }))
}

sens_table <- function(sens, labels = lam_labels) {
  sens %>% transmute(Stage, Specification = spec, Parameter = unname(labels[lhs]),
                     Estimate = sprintf("%.3f", est), `Bootstrap SE` = sprintf("%.3f", se),
                     p = ifelse(pvalue < 0.001, "< 0.001", sprintf("%.3f", pvalue)),
                     `95% CI` = sprintf("[%.3f, %.3f]", ci.lower, ci.upper),
                     `Replications used` = n_boot_ok)
}

# Faded = 95% CI includes zero; dotted lines = mean estimate across specifications;
# numbers = estimates whose 95% CI excludes zero, printed at the right end of the CI.
plot_lambda_sens <- function(sens, file, labels = lam_labels) {
  cols <- setNames(unname(lam_cols[names(labels)]), unname(labels))
  plot_sens <- sens %>%
    mutate(spec = factor(spec, levels = spec_levels),
           Parameter = factor(labels[lhs], levels = labels),
           signif = ci.lower > 0 | ci.upper < 0)
  avg_lines <- plot_sens %>% group_by(Stage, Parameter) %>% summarise(m = mean(est), .groups = "drop")
  pd <- position_dodge(width = 0.8)
  p <- ggplot(plot_sens, aes(est, spec, colour = Parameter, shape = Parameter, group = Parameter)) +
    geom_vline(xintercept = 0, colour = "gray55", linewidth = 0.8) +
    geom_vline(data = avg_lines, aes(xintercept = m, colour = Parameter),
               linetype = "dotted", linewidth = 1.1, show.legend = FALSE) +
    geom_errorbar(aes(xmin = ci.lower, xmax = ci.upper, alpha = signif), orientation = "y",
                  width = 0, linewidth = 1.6, position = pd) +
    geom_point(aes(alpha = signif), size = 4.5, position = pd) +
    geom_text(data = filter(plot_sens, signif), aes(x = ci.upper, label = sprintf("%.2f", est)),
              position = pd, hjust = -0.15, size = fig_base * 0.27, show.legend = FALSE) +
    scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.3), guide = "none") +
    scale_colour_manual(values = cols) +
    scale_shape_manual(values = c(16, 17, 15)) +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.25))) +
    guides(colour = guide_legend(nrow = 1, override.aes = list(linewidth = 1.6, size = 5)),
           shape  = guide_legend(nrow = 1)) +
    facet_wrap(~ Stage) +
    labs(x = "Information weighted expectation parameter\n(95% bootstrap percentile CI)",
         y = "Outcome round or aggregation window") +
    fig_theme()
  ggsave(file.path(path_out, file), p, width = 18, height = 16, dpi = 300)
  message("Saved: ", file, " (faded = 95% CI includes 0; dotted = mean across specifications)")
  invisible(p)
}

setup_done <- TRUE
