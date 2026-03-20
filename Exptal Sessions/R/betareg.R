# ==============================================================================
# Script: Beta Regressions Round 8 Analysis
# Description: Runs independent Beta models for compliance decisions and beliefs 
# in Round 8 for both Unknown (T1) and Known (T2) out-group scenarios and
# exports the results into a combined summary table.
# ==============================================================================

# Ensure necessary packages are installed before running:
# install.packages(c("betareg", "dplyr", "tidyr", "modelsummary", "tinytable", "boot", "lmtest"))

# Load necessary libraries
library(betareg)      # Used for Beta regressions
library(boot)         # Used for cluster bootstrapping
library(lmtest)       # Used to safely bind custom covariance matrices
library(dplyr)
library(tidyr)
library(modelsummary)
library(tinytable)

rm(list = ls())

# --- 1. Setup: Paths and Data ---
# Ensure these paths are correct for your system
path_github <- "C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos  <- "C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

# Load the recoded wide data
load(paste0(path_datos, "/Datos_islitas_recode.Rdata")) # expects object 'df'

# --- 2. Helper Functions ---

# Helper function to select columns by round
declare_get_columns <- function(prefix, suffix, start, end = NULL, dat) {
  if (is.null(end)) end <- start
  rounds <- start:end
  cols <- unlist(lapply(rounds, function(r) {
    patt <- paste0("^", prefix, "\\.", r, "\\.player\\.", suffix, "$")
    grep(patt, names(dat), value = TRUE)
  }))
  if (length(cols) == 0) message("No matching columns for ", prefix, suffix, " rounds ", start, "-", end)
  return(cols)
}

# Helper function to safely squeeze [0, 1] data into (0, 1) for Beta regression
# Uses the standard Smithson & Verkuilen (2006) transformation
squeeze_beta <- function(x) {
  x_clean <- na.omit(x)
  n <- length(x_clean)
  res <- x
  res[!is.na(x)] <- (x[!is.na(x)] * (n - 1) + 0.5) / n
  return(res)
}

# Data Preparation Function for Stage 1 (Unknown Out-group)
prep_data_T1 <- function(dat, R_start, R_end) {
  cols <- declare_get_columns("T1juegoalgas", "T1_extraccion_libre", R_start, R_end, dat)
  subset_ini <- dat[, cols, drop = FALSE]
  
  dat$average_extraction_ini <- rowMeans(subset_ini, na.rm = TRUE)
  
  # Calculate and squeeze DVs to strictly (0, 1)
  dat$average_compliance_ini <- squeeze_beta(1 - dat$average_extraction_ini / 50)
  dat$belief_compliance_pm   <- squeeze_beta(1 - dat$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50)
  dat$belief_compliance_union<- squeeze_beta(1 - dat$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50)
  
  dat$confianza_caleta <- as.numeric(scale(dat$survey1.1.player.confianza_caleta))
  dat$conflicto_caleta <- as.numeric(scale(dat$survey1.1.player.conflicto_caleta))
  dat$confianza_pm     <- as.numeric(scale(dat$survey1.1.player.confianza_pm))
  dat$conflicto_pm     <- as.numeric(scale(dat$survey1.1.player.conflicto_pm))
  
  if (R_start > 1) {
    cols_obs <- declare_get_columns("T1juegoalgas", "T1_extraccion_otros_libre", R_start - 1, R_start - 1, dat)
    subset_obs <- dat[, cols_obs, drop = FALSE]
    dat$average_extraction_observed_ini <- rowMeans(subset_obs, na.rm = TRUE)
    # IVs don't require squeezing, they can safely contain exact 0s or 1s
    dat$average_compliance_observed_ini_lag <- 1 - (dat$average_extraction_observed_ini / 150)
  } else {
    dat$average_compliance_observed_ini_lag <- NA
  }
  return(dat)
}

# Data Preparation Function for Stage 2 (Known Out-group)
prep_data_T2 <- function(dat, R_start, R_end) {
  cols <- declare_get_columns("T2juegoalgas", "T2_extraccion_metat", R_start, R_end, dat)
  subset_ini <- dat[, cols, drop = FALSE]
  
  dat$average_extraction_ini <- rowMeans(subset_ini, na.rm = TRUE)
  
  # Calculate and squeeze DVs to strictly (0, 1)
  dat$average_compliance_ini <- squeeze_beta(1 - dat$average_extraction_ini / 50)
  dat$belief_compliance_pm   <- squeeze_beta(1 - dat$beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini / 50)
  dat$belief_compliance_union <- squeeze_beta(1 - (dat$beliefsT2inicial.1.player.T2_belief_caleta_ini / 50))
  
  dat$confianza_caleta <- as.numeric(scale(dat$survey1.1.player.confianza_caleta))
  dat$conflicto_caleta <- as.numeric(scale(dat$survey1.1.player.conflicto_caleta))
  dat$confianza_metat  <- as.numeric(scale(dat$survey2.1.player.confianza_caleta_conocida_mean))
  dat$conflicto_metat  <- as.numeric(scale(dat$survey2.1.player.conflicto_caleta_conocida_mean))
  
  if (R_start > 1) {
    cols_obs <- declare_get_columns("T2juegoalgas", "T2_extraccion_otros_metat", R_start - 1, R_start - 1, dat)
    subset_obs <- dat[, cols_obs, drop = FALSE]
    dat$average_extraction_observed_ini <- rowMeans(subset_obs, na.rm = TRUE)
    # IVs don't require squeezing
    dat$average_compliance_observed_ini_lag <- 1 - (dat$average_extraction_observed_ini / 150)
  } else {
    dat$average_compliance_observed_ini_lag <- NA
  }
  return(dat)
}

# --- 2b. Cluster Bootstrap Function for Beta Regression ---
bootstrap_betareg <- function(model_formula, dat, cluster_var, B = 1000, seed = 62354234) {
  # UPDATE FORMULA ENVIRONMENT:
  # This tells betareg() to look for 'dat' and 'd_b' inside this function 
  # rather than in the Global Environment where the formula was typed.
  environment(model_formula) <- environment()
  
  # Fit once on the full data to get the coefficient skeleton & order
  base_fit <- betareg(model_formula, data = dat)
  coef_names <- names(coef(base_fit))
  
  # One bootstrap replication: sample clusters with replacement
  one_rep <- function(d, i_unused) {
    # unique, non-missing clusters
    clusters <- unique(stats::na.omit(d[[cluster_var]]))
    if (length(clusters) < 2L) return(setNames(rep(NA_real_, length(coef_names)), coef_names))
    
    # resample cluster ids (with replacement)
    samp <- sample(clusters, length(clusters), replace = TRUE)
    
    # keep all rows from chosen clusters
    d_b  <- dplyr::bind_rows(lapply(samp, function(cl) d[d[[cluster_var]] == cl, , drop = FALSE]))
    
    # fit Beta model on the bootstrap sample
    fit <- try(betareg(model_formula, data = d_b), silent = TRUE)
    out <- setNames(rep(NA_real_, length(coef_names)), coef_names)
    if (inherits(fit, "try-error")) return(out)
    
    cf <- try(coef(fit), silent = TRUE)
    if (!inherits(cf, "try-error")) out[names(cf)] <- as.numeric(cf)
    out
  }
  
  set.seed(seed)
  bt <- boot::boot(
    data = dat,
    statistic = function(d, i) one_rep(d, i), 
    R = B,
    parallel = "no" # set to "multicore" or "snow" to speed up if supported
  )
  
  draws <- bt$t
  colnames(draws) <- coef_names
  
  # Covariance matrix from the bootstrap draws
  V  <- stats::cov(draws, use = "pairwise.complete.obs")
  V  <- V[coef_names, coef_names, drop = FALSE] # Ensure exact alignment
  
  # Extract standard errors here to bind directly to modelsummary
  se <- sqrt(diag(V))
  names(se) <- coef_names
  
  list(
    base_fit = base_fit,
    V        = V,
    se       = se
  )
}


# --- 3. Prepare Data and Run Independent Beta Models ---

cat("\nPreparing data for Stage 1 & 2 Round 8...\n")
df_T1 <- prep_data_T1(df, 8, 8)
df_T2 <- prep_data_T2(df, 8, 8)

# ! IMPORTANT: Update this to exactly match your session/group column name in your dataset !
cluster_col <- "gid.treat" 

# Number of bootstrap iterations (easy to change here for publication later)
B_iters <- 100 

cat(sprintf("Running cluster-bootstrapped Beta regressions with B = %d iterations (this may take a few minutes)...\n", B_iters))

# Stage 1 (Unknown Out-group) Beta Regressions with Bootstrap
res1_beliefs_in  <- bootstrap_betareg(belief_compliance_union ~ confianza_caleta, df_T1, cluster_col, B = B_iters)
res1_beliefs_out <- bootstrap_betareg(belief_compliance_pm ~ confianza_pm, df_T1, cluster_col, B = B_iters)
res1_compliance  <- bootstrap_betareg(average_compliance_ini ~ belief_compliance_pm + belief_compliance_union + 
                                        conflicto_pm + conflicto_caleta + average_compliance_observed_ini_lag, 
                                      df_T1, cluster_col, B = B_iters)

# Stage 2 (Known Out-group) Beta Regressions with Bootstrap
res2_beliefs_in  <- bootstrap_betareg(belief_compliance_union ~ confianza_caleta, df_T2, cluster_col, B = B_iters)
res2_beliefs_out <- bootstrap_betareg(belief_compliance_pm ~ confianza_metat, df_T2, cluster_col, B = B_iters)
res2_compliance  <- bootstrap_betareg(average_compliance_ini ~ belief_compliance_pm + belief_compliance_union + 
                                        conflicto_metat + conflicto_caleta + average_compliance_observed_ini_lag, 
                                      df_T2, cluster_col, B = B_iters)


# --- 4. Format and Export Results ---

# 4a. Base models list
models <- list(
  "Stage 1 DV: Beliefs In-group"  = res1_beliefs_in$base_fit,
  "Stage 1 DV: Beliefs Out-group" = res1_beliefs_out$base_fit,
  "Stage 1 DV: Compliance"        = res1_compliance$base_fit,
  "Stage 2 DV: Beliefs In-group"  = res2_beliefs_in$base_fit,
  "Stage 2 DV: Beliefs Out-group" = res2_beliefs_out$base_fit,
  "Stage 2 DV: Compliance"        = res2_compliance$base_fit
)

# 4b. Extract custom standard errors (named vectors) from bootstrap
custom_se <- list(
  "Stage 1 DV: Beliefs In-group"  = res1_beliefs_in$se,
  "Stage 1 DV: Beliefs Out-group" = res1_beliefs_out$se,
  "Stage 1 DV: Compliance"        = res1_compliance$se,
  "Stage 2 DV: Beliefs In-group"  = res2_beliefs_in$se,
  "Stage 2 DV: Beliefs Out-group" = res2_beliefs_out$se,
  "Stage 2 DV: Compliance"        = res2_compliance$se
)

# 4c. Extract native betareg Pseudo R-squared safely
calc_pseudo_r2 <- function(model) {
  if (!is.null(model$pseudo.r.squared)) {
    return(sprintf("%.3f", model$pseudo.r.squared))
  }
  return("NA")
}

pseudo_r2_vals <- lapply(models, calc_pseudo_r2)

# Compile the exact GOF structure (only appending our Pseudo R2 now)
custom_gof <- data.frame(
  "term" = "Pseudo R-squared",
  "Stage 1 DV: Beliefs In-group"  = pseudo_r2_vals[[1]],
  "Stage 1 DV: Beliefs Out-group" = pseudo_r2_vals[[2]],
  "Stage 1 DV: Compliance"        = pseudo_r2_vals[[3]],
  "Stage 2 DV: Beliefs In-group"  = pseudo_r2_vals[[4]],
  "Stage 2 DV: Beliefs Out-group" = pseudo_r2_vals[[5]],
  "Stage 2 DV: Compliance"        = pseudo_r2_vals[[6]],
  check.names = FALSE
)

# Dictionary for mapping internal variable names to output labels.
coef_mapping <- c(
  "confianza_pm"                        = "Trust Out-group",
  "confianza_metat"                     = "Trust Out-group",
  "confianza_caleta"                    = "Trust In-group",
  "conflicto_pm"                        = "Conflict Out-group",
  "conflicto_metat"                     = "Conflict Out-group",
  "conflicto_caleta"                    = "Conflict In-group",
  "belief_compliance_pm"                = "Prior Beliefs Out-group",
  "belief_compliance_union"             = "Prior Beliefs In-group",
  "average_compliance_observed_ini_lag" = "Observed Compliance (round 7)"
)

# Standard Goodness-of-Fit metrics mapping
gof_mapping <- list(
  list("raw" = "nobs", "clean" = "Num.Obs.", "fmt" = 0),
  list("raw" = "logLik", "clean" = "Log Likelihood", "fmt" = 3),
  list("raw" = "AIC", "clean" = "AIC", "fmt" = 3),
  list("raw" = "BIC", "clean" = "BIC", "fmt" = 3)
)

# Define path for the output Word file
table_file_path_docx <- paste0(path_github, "Outputs/Betareg_Round8_Summary_Table_RG.docx")

# Generate the Word Document
cat("Exporting table to Word...\n")
modelsummary(
  models,
  vcov       = custom_se,             # Inject the named SE vectors safely!
  coef_map   = coef_mapping,            
  gof_map    = gof_mapping,           # Let modelsummary natively extract LL, AIC, BIC, etc.
  add_rows   = custom_gof,            # Append our Pseudo R-squared block
  statistic  = "({std.error})",       # Explicitly renders standard errors in parentheses
  stars      = c('*' = .05, '**' = .01, '***' = .001),
  title      = "Independent Beta Regressions for Compliance and Beliefs in Round 8",
  notes      = sprintf("Bootstrapped cluster-robust standard errors in parentheses (B = %d). Data algebraically squeezed to (0, 1) using the Smithson & Verkuilen transformation.", B_iters),
  output     = table_file_path_docx
)

message("\nSuccess! Summary table saved to: ", table_file_path_docx)