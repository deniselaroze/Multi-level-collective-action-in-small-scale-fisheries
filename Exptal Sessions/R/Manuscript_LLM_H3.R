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

rm(list=ls())
path_github <-"C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos<-"C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

#path_github <-"C:/Users/Denise Laroze/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
#path_datos<-"C:/Users/Denise Laroze/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

setwd(path_github)


#load(paste0(path_datos, "/Datos_islitas.Rdata"))
load(paste0(path_datos, "/Datos_islitas_recode.Rdata"))
load(paste0(path_datos, "/Datos_islitas_long.Rdata"))

### Data management
dfs_long <- dfs_long %>%
  mutate(
    confianza_pm_scaled      = (survey1.1.player.confianza_pm- 1) / 3,
    conflicto_pm_scaled      = (survey1.1.player.conflicto_pm- 1) / 3,
    confianza_caleta_scaled  = (survey1.1.player.confianza_caleta- 1) / 3,
    conflicto_caleta_scaled  = (survey1.1.player.conflicto_caleta- 1) / 3
  )

dfs_long$round.plm<-ifelse(dfs_long$treatment=="T2", as.numeric(dfs_long$round)+8, as.numeric(dfs_long$round))
dfs_long$participant.code.plm<-ifelse(dfs_long$treatment=="T2", paste0(dfs_long$participant.code, ".T2"), dfs_long$participant.code)


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


################
### TURF
################

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

# --- Run bootstrap for each model ------------------------------------------
B <- 200  # try small while testing; use 500–1000 for paper
res1 <- bootstrap_lmer(model1, dfs_long, "gid.amerb", B = B)
res2 <- bootstrap_lmer(model2, dfs_long, "gid.amerb", B = B)
res3 <- bootstrap_lmer(model3, dfs_long, "gid.amerb", B = B)
res4 <- bootstrap_lmer(model4, dfs_long, "gid.amerb", B = B)

# --- Table (single DOCX) ----------------------------------------------------
out_file <- paste0(path_github, "Outputs/LMM_boot_H3_TURF.docx")

coef_map <- c(
  "(Intercept)" = "(Intercept)",
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

omit_sociodemo <- "^survey3\\.1\\.player\\.(sexo|horas_trabajo|estudios|liderazgo).*"


add_row <- tibble::tibble(
  term = "Socio-demographic controls",
  `SA: Base + Beliefs`             = "No",
  `SA: Base + Trust/Conflict`      = "No",
  `SA: Beliefs + Trust/Conflict`   = "No",
  `SA: Full (+ sociodemographics)` = "Yes"
)

modelsummary(
  list(
    "TURF: Base + Beliefs"                     = plm1,
    "TURF: Base + Trust/Conflict"      = plm2,
    "TURF: Beliefs + Trust/Conflict" = plm3,
    "TURF: Full (+ sociodemographics)" = plm4
  ),
  coef_map   = coef_map,
  coef_omit  = omit_sociodemo,
  add_rows   = add_row,  
  vcov      = list(res1$V, res2$V, res3$V, res4$V),
  statistic = "({std.error})",
  stars     = c("*" = .05, "**" = .01, "***" = .001),
  gof_omit   = "^(AIC|BIC|ICC|RMSE)$", 
  output    = out_file
)



#####################
### Shared Area Presentation ESA
####################

oa_m1 <- compliance_extraction_OA ~
  compliance_lag_extraction_others_OA_mean +
  treatment*confianza_pm_scaled + treatment*conflicto_pm_scaled +
  confianza_caleta_scaled + conflicto_caleta_scaled +
  (1 | participant.code.plm)

#oa_m2 <- compliance_extraction_OA ~
#  compliance_lag_extraction_others_OA_mean +
#  compliance_beliefs_OA_caleta + compliance_beliefs_OA_others +
#  treatment +
#  (1 | participant.code.plm)

oa_m3 <- compliance_extraction_OA ~
  compliance_lag_extraction_others_OA_mean +
  #treatment*confianza_pm_scaled + treatment*conflicto_pm_scaled +
  compliance_beliefs_OA_caleta + compliance_beliefs_OA_others*treatment +
  (1 | participant.code.plm)


# oa_m4 <- compliance_extraction_OA ~
#   compliance_lag_extraction_others_OA_mean +
#   compliance_beliefs_OA_caleta + compliance_beliefs_OA_others +
#   treatment + minority + n_identities +
#   confianza_pm_scaled + conflicto_pm_scaled +
#   confianza_caleta_scaled + conflicto_caleta_scaled +
#   (1 | participant.code.plm)

oa_m4 <- compliance_extraction_OA ~
  compliance_lag_extraction_others_OA_mean +
  treatment*confianza_pm_scaled + treatment*conflicto_pm_scaled +
  compliance_beliefs_OA_caleta + compliance_beliefs_OA_others*treatment +
  minority + n_identities +
  confianza_caleta_scaled + conflicto_caleta_scaled +
  survey3.1.player.sexo + survey3.1.player.horas_trabajo +
  survey3.1.player.estudios + survey3.1.player.liderazgo +
  (1 | participant.code.plm)

ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))

# --- Fit full-sample models (point estimates) ---
oaplm1 <- lmer(oa_m1, data = dfs_long, control = ctrl, REML = FALSE)
#oaplm2 <- lmer(oa_m2, data = dfs_long, control = ctrl, REML = FALSE)
oaplm3 <- lmer(oa_m3, data = dfs_long, control = ctrl, REML = FALSE)
oaplm4 <- lmer(oa_m4, data = dfs_long, control = ctrl, REML = FALSE)

# --- Clustered bootstrap (by OA cluster) ---
B <- 2000 # bump to 500–1000 for the paper
set.seed(2365)
oa1 <- bootstrap_lmer(oa_m1, dfs_long, "gid.treat", B = B)
#oa2 <- bootstrap_lmer(oa_m2, dfs_long, "gid.treat", B = B)
oa3 <- bootstrap_lmer(oa_m3, dfs_long, "gid.treat", B = B)
oa4 <- bootstrap_lmer(oa_m4, dfs_long, "gid.treat", B = B)

# --- Export a single DOCX table with bootstrapped SEs ---

# A dictionary that renames but never drops terms.
# Include both ':' and ' × ' versions for interactions so it works regardless of pretty rewriting.
coef_dict <- c(
  "(Intercept)"                                  = "(Intercept)",
  "compliance_lag_extraction_others_OA_mean"     = "Mean observed compliance (t-1)",
  "treatmentT2"                                  = "Stage (Known out-group)",
  "confianza_pm_scaled"                          = "Trust (out-group)",
  "conflicto_pm_scaled"                          = "Conflict (out-group)",
  "treatmentT2:confianza_pm_scaled"              = "Stage × Trust (out-group)",
  "treatmentT2:conflicto_pm_scaled"              = "Stage × Conflict (out-group)",
  "treatmentT2 × confianza_pm_scaled"            = "Stage × Trust (out-group)",
  "treatmentT2 × conflicto_pm_scaled"            = "Stage × Conflict (out-group)",
  "compliance_beliefs_OA_others:treatmentT2"     = "Stage x Beliefs (out-group)",
  "treatmentT2:compliance_beliefs_OA_others"     = "Stage x Beliefs (out-group)",
  "confianza_caleta_scaled"                      = "Trust (Union)",
  "conflicto_caleta_scaled"                      = "Conflict (in-group)",
  "compliance_beliefs_OA_caleta"                 = "Prior beliefs (in-group)",
  "compliance_beliefs_OA_others"                 = "Prior beliefs (out-group)",
  "minority"                                     = "Minority in round",
  "n_identities"                                 = "Three unions",
  "survey3.1.player.sexo"                        = "Female",
  "survey3.1.player.horas_trabajo"               = "Hours worked per week",
  "survey3.1.player.estudios"                    = "Education level",
  "survey3.1.player.liderazgoSí"                 = "Held leadership role",
  "SD (Intercept participant.code.plm)"          = "SD (Intercept participant)",
  "SD (Observations)"                            = "SD (Residual)"
)

# Optional: if you’d rather *stop* the automatic '×' rewrite entirely, uncomment:
# options(modelsummary_rewrite_terms = FALSE)

mods <- list(
  "(Trust/Conflict)"                        = oaplm1,
  "(Beliefs)"                        = oaplm3,
  "(Beliefs +)"                        = oaplm4
)

vcvs <- list(oa1$V, oa3$V, oa4$V)

out_file_oa <- paste0(path_github, "Outputs/LMM_boot_H3_SharedArea_presentation.docx")

modelsummary(
  mods,
  vcov        = vcvs,
  coef_rename = coef_dict,                 # rename without dropping anything
  statistic   = "({std.error})",
  stars       = c("*"=.05,"**"=.01,"***"=.001),
  gof_omit    = "^(AIC|BIC|ICC|RMSE)$",
  output      = out_file_oa
)




###############################
### Shared Area subset leaders
################################

oa_m1 <- compliance_extraction_OA ~
  compliance_lag_extraction_others_OA_mean +
  treatment*confianza_pm_scaled + treatment*conflicto_pm_scaled +
  confianza_caleta_scaled + conflicto_caleta_scaled +
  (1 | participant.code.plm)

#oa_m2 <- compliance_extraction_OA ~
#  compliance_lag_extraction_others_OA_mean +
#  compliance_beliefs_OA_caleta + compliance_beliefs_OA_others +
#  treatment +
#  (1 | participant.code.plm)

oa_m3 <- compliance_extraction_OA ~
  compliance_lag_extraction_others_OA_mean +
  #treatment*confianza_pm_scaled + treatment*conflicto_pm_scaled +
  compliance_beliefs_OA_caleta + compliance_beliefs_OA_others*treatment +
  (1 | participant.code.plm)


# oa_m4 <- compliance_extraction_OA ~
#   compliance_lag_extraction_others_OA_mean +
#   compliance_beliefs_OA_caleta + compliance_beliefs_OA_others +
#   treatment + minority + n_identities +
#   confianza_pm_scaled + conflicto_pm_scaled +
#   confianza_caleta_scaled + conflicto_caleta_scaled +
#   (1 | participant.code.plm)

oa_m4 <- compliance_extraction_OA ~
  compliance_lag_extraction_others_OA_mean +
  treatment*confianza_pm_scaled + treatment*conflicto_pm_scaled +
  compliance_beliefs_OA_caleta + compliance_beliefs_OA_others*treatment +
  minority + n_identities +
  confianza_caleta_scaled + conflicto_caleta_scaled +
  survey3.1.player.sexo + survey3.1.player.horas_trabajo +
  survey3.1.player.estudios  +
  (1 | participant.code.plm)

ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))


# Subset leaders
dfs_lead<-dfs_long[dfs_long$survey3.1.player.liderazgo=="Sí",]


# --- Fit full-sample models (point estimates) ---
oaplm1 <- lmer(oa_m1, data = dfs_lead, control = ctrl, REML = FALSE)
#oaplm2 <- lmer(oa_m2, data = dfs_long, control = ctrl, REML = FALSE)
oaplm3 <- lmer(oa_m3, data = dfs_lead, control = ctrl, REML = FALSE)
oaplm4 <- lmer(oa_m4, data = dfs_lead, control = ctrl, REML = FALSE)




# --- Clustered bootstrap (by OA cluster) ---
B <- 100 # bump to 500–1000 for the paper
set.seed(2365)
oa1 <- bootstrap_lmer(oa_m1, dfs_lead, "gid.treat", B = B)
#oa2 <- bootstrap_lmer(oa_m2, dfs_long, "gid.treat", B = B)
oa3 <- bootstrap_lmer(oa_m3, dfs_lead, "gid.treat", B = B)
oa4 <- bootstrap_lmer(oa_m4, dfs_lead, "gid.treat", B = B)

# --- Export a single DOCX table with bootstrapped SEs ---

# A dictionary that renames but never drops terms.
# Include both ':' and ' × ' versions for interactions so it works regardless of pretty rewriting.
coef_dict <- c(
  "(Intercept)"                                  = "(Intercept)",
  "compliance_lag_extraction_others_OA_mean"     = "Mean observed compliance (t-1)",
  "treatmentT2"                                  = "Stage (Known out-group)",
  "confianza_pm_scaled"                          = "Trust (out-group)",
  "conflicto_pm_scaled"                          = "Conflict (out-group)",
  "treatmentT2:confianza_pm_scaled"              = "Stage × Trust (out-group)",
  "treatmentT2:conflicto_pm_scaled"              = "Stage × Conflict (out-group)",
  "treatmentT2 × confianza_pm_scaled"            = "Stage × Trust (out-group)",
  "treatmentT2 × conflicto_pm_scaled"            = "Stage × Conflict (out-group)",
  "compliance_beliefs_OA_others:treatmentT2"     = "Stage x Beliefs (out-group)",
  "treatmentT2:compliance_beliefs_OA_others"     = "Stage x Beliefs (out-group)",
  "confianza_caleta_scaled"                      = "Trust (Union)",
  "conflicto_caleta_scaled"                      = "Conflict (in-group)",
  "compliance_beliefs_OA_caleta"                 = "Prior beliefs (in-group)",
  "compliance_beliefs_OA_others"                 = "Prior beliefs (out-group)",
  "minority"                                     = "Minority in round",
  "n_identities"                                 = "Three unions",
  "survey3.1.player.sexo"                        = "Female",
  "survey3.1.player.horas_trabajo"               = "Hours worked per week",
  "survey3.1.player.estudios"                    = "Education level",
  "survey3.1.player.liderazgoSí"                 = "Held leadership role",
  "SD (Intercept participant.code.plm)"          = "SD (Intercept participant)",
  "SD (Observations)"                            = "SD (Residual)"
)

# Optional: if you’d rather *stop* the automatic '×' rewrite entirely, uncomment:
# options(modelsummary_rewrite_terms = FALSE)

mods <- list(
  "(Trust/Conflict)"                        = oaplm1,
  "(Beliefs)"                        = oaplm3,
  "(Beliefs +)"                        = oaplm4
)

vcvs <- list(oa1$V, oa3$V, oa4$V)

out_file_oa <- paste0(path_github, "Outputs/LMM_boot_H3_SharedArea_presentation_liders.docx")

modelsummary(
  mods,
  vcov        = vcvs,
  coef_rename = coef_dict,                 # rename without dropping anything
  statistic   = "({std.error})",
  stars       = c("*"=.05,"**"=.01,"***"=.001),
  gof_omit    = "^(AIC|BIC|ICC|RMSE)$",
  output      = out_file_oa
)


