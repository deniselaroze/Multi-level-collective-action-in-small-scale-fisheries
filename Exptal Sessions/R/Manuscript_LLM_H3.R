# --- Packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(lme4)
library(boot)
library(modelsummary)
library(broom.mixed)
library(tinytable)
library(rlang)
library(flextable)     # for DOCX
library(officer)       # for DOCX
options(modelsummary_factory_default = "flextable")  # force DOCX via flextable

# (your paths, loads, and formulas stay the same)

# Scale survey covariates used below (avoid /4 in formulas so names stay clean)
dfs_long <- dfs_long %>%
  mutate(
    confianza_pm_scaled      = survey1.1.player.confianza_pm/4,
    conflicto_pm_scaled      = survey1.1.player.conflicto_pm/4,
    confianza_caleta_scaled  = survey1.1.player.confianza_caleta/4,
    conflicto_caleta_scaled  = survey1.1.player.conflicto_caleta/4
  )


### Define models
model1 <- compliance_extraction_amerb ~
  compliance_lag_extraction_others_amerb_mean +
  compliance_beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini +
  treatment +
  (1 | participant.code)

model2 <- compliance_extraction_amerb ~
  compliance_lag_extraction_others_amerb_mean +
  treatment +
  confianza_caleta_scaled + conflicto_caleta_scaled +
  (1 | participant.code)

model3 <- compliance_extraction_amerb ~
  compliance_lag_extraction_others_amerb_mean +
  compliance_beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini +
  treatment +
  confianza_caleta_scaled + conflicto_caleta_scaled +
  (1 | participant.code)

model4 <- compliance_extraction_amerb ~
  compliance_lag_extraction_others_amerb_mean +
  compliance_beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini +
  treatment +
  confianza_caleta_scaled + conflicto_caleta_scaled +
  survey3.1.player.sexo + survey3.1.player.horas_trabajo +
  survey3.1.player.estudios + survey3.1.player.liderazgo +
  (1 | participant.code)

ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))

plm1 <- lmer(model1, data = dfs_long, control = ctrl, REML = FALSE)
plm2 <- lmer(model2, data = dfs_long, control = ctrl, REML = FALSE)
plm3 <- lmer(model3, data = dfs_long, control = ctrl, REML = FALSE)
plm4 <- lmer(model4, data = dfs_long, control = ctrl, REML = FALSE)

# --- Clustered bootstrap helper --------------------------------------------
bootstrap_lmer <- function(model_formula, data, cluster_var, B = 100, seed = 62354234) {
  # coefficient skeleton
  coef_names <- names(fixef(lmer(model_formula, data = data, control = ctrl, REML = FALSE)))
  
  # one bootstrap replication (cluster resampling with replacement)
  one_rep <- function(d, i_unused) {
    clusters <- unique(stats::na.omit(d[[cluster_var]]))   # <-- drop NA clusters
    if (length(clusters) < 2L) return(setNames(rep(NA_real_, length(coef_names)), coef_names))
    
    samp <- sample(clusters, length(clusters), replace = TRUE)
    d_b <- dplyr::bind_rows(lapply(samp, function(cl) d[d[[cluster_var]] == cl, , drop = FALSE]))
    
    fit <- try(lmer(model_formula, data = d_b, control = ctrl, REML = FALSE), silent = TRUE)
    out <- setNames(rep(NA_real_, length(coef_names)), coef_names)
    if (inherits(fit, "try-error")) return(out)
    
    cf <- try(fixef(fit), silent = TRUE)
    if (!inherits(cf, "try-error")) out[names(cf)] <- cf
    out
  }
  
  set.seed(seed)
  bt <- boot::boot(data = data,
                   statistic = function(d, i) one_rep(d, i),
                   R = B)
  
  draws <- bt$t
  colnames(draws) <- coef_names
  V <- stats::cov(draws, use = "pairwise.complete.obs")
  se <- sqrt(diag(V))
  list(boot = bt, V = V, se = se, coef_names = coef_names)
}

# --- Run bootstrap for each model ------------------------------------------
B <- 200  # try small while testing; use 500–1000 for paper
res1 <- bootstrap_lmer(model1, dfs_long, "gid.amerb", B = B)
res2 <- bootstrap_lmer(model2, dfs_long, "gid.amerb", B = B)
res3 <- bootstrap_lmer(model3, dfs_long, "gid.amerb", B = B)
res4 <- bootstrap_lmer(model4, dfs_long, "gid.amerb", B = B)

# --- Table (single DOCX) ----------------------------------------------------
out_file <- paste0(path_github, "Outputs/LMM_boot_H3_TURF3.docx")

coef_map <- c(
  "(Intercept)"                      = "Intercept (TURF rounds 1–8)",
  "compliance_lag_extraction_others_amerb_mean" = "Mean observed compliance (t-1)",
  "compliance_beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini" = "Prior Belief (TURF)",
  "treatmentT2" = "Stage (rounds 9-16)",
  "confianza_caleta_scaled" = "Trust (TURF)",
  "conflicto_caleta_scaled" = "Conflict (TURF)",
  "survey3.1.player.sexo" = "Female",
  "survey3.1.player.horas_trabajo" = "Hours of work a week",
  "survey3.1.player.estudios" = "Level of education",
  "survey3.1.player.liderazgoSí" = "Held ledership role",
  "SD (Intercept participant.code)" = "SD (Intercept participant)",
  "SD (Observations)" = "SD (Observations)"
)

modelsummary(
  list(
    "Base"                     = plm1,
    "Trust/Conflict only"      = plm2,
    "Beliefs + Trust/Conflict" = plm3,
    "Full ( + sociodemographics )" = plm4
  ),
  coef_map   = coef_map,
  vcov      = list(res1$V, res2$V, res3$V, res4$V),
  statistic = "({std.error})",
  stars     = c("*" = .05, "**" = .01, "***" = .001),
  output    = out_file
)
