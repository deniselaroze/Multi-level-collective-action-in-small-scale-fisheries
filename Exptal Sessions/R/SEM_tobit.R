# ==============================================================================
# Script: Tobit Regressions Round 8 Analysis (piecewiseSEM approach)
# Description: Runs independent Tobit models for compliance and beliefs 
# in Round 8, unites them into a Structural Equation Model using piecewiseSEM,
# and exports the results into a combined summary table.
# ==============================================================================

# Ensure necessary packages are installed before running:
# install.packages(c("AER", "dplyr", "tidyr", "modelsummary", "tinytable"))

# If 'piecewiseSEM' is not available for your version of R via CRAN, 
# you can install it directly from GitHub using the 'remotes' package:
# install.packages("remotes")



# Load necessary libraries
library(AER)          # Used for Tobit regressions
library(piecewiseSEM) # Used to unite Tobit models into an SEM framework
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
declare_get_columns <- function(prefix, suffix, start, end = NULL, data) {
  if (is.null(end)) end <- start
  rounds <- start:end
  cols <- unlist(lapply(rounds, function(r) {
    patt <- paste0("^", prefix, "\\.", r, "\\.player\\.", suffix, "$")
    grep(patt, names(data), value = TRUE)
  }))
  if (length(cols) == 0) message("No matching columns for ", prefix, suffix, " rounds ", start, "-", end)
  return(cols)
}

# Data Preparation Function for Stage 1 (Unknown Out-group)
prep_data_T1 <- function(data, R_start, R_end) {
  cols <- declare_get_columns("T1juegoalgas", "T1_extraccion_libre", R_start, R_end, data)
  subset_ini <- data[, cols, drop = FALSE]
  
  data$average_extraction_ini <- rowMeans(subset_ini, na.rm = TRUE)
  data$average_compliance_ini <- 1 - data$average_extraction_ini / 50
  data$belief_compliance_pm   <- 1 - data$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50
  data$belief_compliance_union<- 1 - data$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50
  
  data$confianza_caleta <- as.numeric(scale(data$survey1.1.player.confianza_caleta))
  data$conflicto_caleta <- as.numeric(scale(data$survey1.1.player.conflicto_caleta))
  data$confianza_pm     <- as.numeric(scale(data$survey1.1.player.confianza_pm))
  data$conflicto_pm     <- as.numeric(scale(data$survey1.1.player.conflicto_pm))
  
  if (R_start > 1) {
    cols_obs <- declare_get_columns("T1juegoalgas", "T1_extraccion_otros_libre", R_start - 1, R_start - 1, data)
    subset_obs <- data[, cols_obs, drop = FALSE]
    data$average_extraction_observed_ini <- rowMeans(subset_obs, na.rm = TRUE)
    data$average_compliance_observed_ini_lag <- 1 - (data$average_extraction_observed_ini / 150)
  } else {
    data$average_compliance_observed_ini_lag <- NA
  }
  return(data)
}

# Data Preparation Function for Stage 2 (Known Out-group)
prep_data_T2 <- function(data, R_start, R_end) {
  cols <- declare_get_columns("T2juegoalgas", "T2_extraccion_metat", R_start, R_end, data)
  subset_ini <- data[, cols, drop = FALSE]
  
  data$average_extraction_ini <- rowMeans(subset_ini, na.rm = TRUE)
  data$average_compliance_ini <- 1 - data$average_extraction_ini / 50
  data$belief_compliance_pm   <- 1 - data$beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini / 50
  data$belief_compliance_union <- 1 - (data$beliefsT2inicial.1.player.T2_belief_caleta_ini / 50)
  
  data$confianza_caleta <- as.numeric(scale(data$survey1.1.player.confianza_caleta))
  data$conflicto_caleta <- as.numeric(scale(data$survey1.1.player.conflicto_caleta))
  data$confianza_metat  <- as.numeric(scale(data$survey2.1.player.confianza_caleta_conocida_mean))
  data$conflicto_metat  <- as.numeric(scale(data$survey2.1.player.conflicto_caleta_conocida_mean))
  
  if (R_start > 1) {
    cols_obs <- declare_get_columns("T2juegoalgas", "T2_extraccion_otros_metat", R_start - 1, R_start - 1, data)
    subset_obs <- data[, cols_obs, drop = FALSE]
    data$average_extraction_observed_ini <- rowMeans(subset_obs, na.rm = TRUE)
    data$average_compliance_observed_ini_lag <- 1 - (data$average_extraction_observed_ini / 150)
  } else {
    data$average_compliance_observed_ini_lag <- NA
  }
  return(data)
}


# --- 3. Prepare Data and Run piecewiseSEM Tobit Models ---

cat("\nPreparing data for Stage 1 & 2 Round 8...\n")
df_T1 <- prep_data_T1(df, 8, 8)
df_T2 <- prep_data_T2(df, 8, 8)

cat("Running independent Tobit models...\n")

# Stage 1 (Unknown Out-group) Tobits
m1_beliefs_in  <- tobit(belief_compliance_union ~ confianza_caleta + conflicto_caleta, left = 0, right = 1, data = df_T1)
m1_beliefs_out <- tobit(belief_compliance_pm ~ confianza_pm + conflicto_pm, left = 0, right = 1, data = df_T1)
m1_compliance  <- tobit(average_compliance_ini ~ belief_compliance_pm + belief_compliance_union + 
                          conflicto_pm + conflicto_caleta+ confianza_caleta + confianza_pm + average_compliance_observed_ini_lag, 
                        left = 0, right = 1, data = df_T1)

# Compile Stage 1 SEM using piecewiseSEM
sem_T1 <- psem(
  m1_beliefs_in,
  m1_beliefs_out,
  m1_compliance,
  data = df_T1
)

# Stage 2 (Known Out-group) Tobits
m2_beliefs_in  <- tobit(belief_compliance_union ~ confianza_caleta + conflicto_caleta, left = 0, right = 1, data = df_T2)
m2_beliefs_out <- tobit(belief_compliance_pm ~ confianza_metat + conflicto_metat, left = 0, right = 1, data = df_T2)
m2_compliance  <- tobit(average_compliance_ini ~ belief_compliance_pm + belief_compliance_union + 
                          conflicto_metat + conflicto_caleta + confianza_caleta + confianza_metat + average_compliance_observed_ini_lag, 
                        left = 0, right = 1, data = df_T2)

# Compile Stage 2 SEM using piecewiseSEM
sem_T2 <- psem(
  m2_beliefs_in,
  m2_beliefs_out,
  m2_compliance,
  data = df_T2
)


# --- 4. Format and Export Results ---

# Create a named list of models to populate the column headers
models <- list(
  "Stage 1 DV: Beliefs In-group"  = m1_beliefs_in,
  "Stage 1 DV: Beliefs Out-group" = m1_beliefs_out,
  "Stage 1 DV: Compliance"        = m1_compliance,
  "Stage 2 DV: Beliefs In-group"  = m2_beliefs_in,
  "Stage 2 DV: Beliefs Out-group" = m2_beliefs_out,
  "Stage 2 DV: Compliance"        = m2_compliance
)

# Helper function to calculate McFadden's Pseudo R-squared
calc_pseudo_r2 <- function(model) {
  null_model <- update(model, . ~ 1)
  ll_full <- as.numeric(logLik(model))
  ll_null <- as.numeric(logLik(null_model))
  r2 <- 1 - (ll_full / ll_null)
  return(sprintf("%.3f", r2))
}

pseudo_r2_vals <- lapply(models, calc_pseudo_r2)

# Extract global Goodness of Fit (Fisher's C) from piecewiseSEM safely
extract_fisher_c <- function(sem_model) {
  res <- tryCatch({
    s <- summary(sem_model, .progressBar = FALSE)
    list(
      C = sprintf("%.2f", s$Cstat$Fisher.C),
      P = sprintf("%.3f", s$Cstat$P.Value)
    )
  }, error = function(e) { list(C = "N/A", P = "N/A") })
  return(res)
}

fit_T1 <- extract_fisher_c(sem_T1)
fit_T2 <- extract_fisher_c(sem_T2)


# Create custom rows to add to the bottom of the table
custom_gof <- data.frame(
  "term" = c(
    "Pseudo R-squared (McFadden)", 
    "SEM Global Fit: Fisher's C", 
    "SEM Global Fit: P-Value"
  ),
  # Stage 1 block
  "Stage 1 DV: Beliefs In-group"  = c(pseudo_r2_vals[[1]], "", ""),
  "Stage 1 DV: Beliefs Out-group" = c(pseudo_r2_vals[[2]], "", ""),
  "Stage 1 DV: Compliance"        = c(pseudo_r2_vals[[3]], fit_T1$C, fit_T1$P),
  # Stage 2 block
  "Stage 2 DV: Beliefs In-group"  = c(pseudo_r2_vals[[4]], "", ""),
  "Stage 2 DV: Beliefs Out-group" = c(pseudo_r2_vals[[5]], "", ""),
  "Stage 2 DV: Compliance"        = c(pseudo_r2_vals[[6]], fit_T2$C, fit_T2$P),
  check.names = FALSE
)
colnames(custom_gof) <- c("term", names(models))


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

# Customize standard Goodness of Fit statistics
gof_mapping <- list(
  list("raw" = "nobs", "clean" = "Num.Obs.", "fmt" = 0),
  list("raw" = "logLik", "clean" = "Log Likelihood", "fmt" = 3)
)

# Define path for the output Word file
table_file_path_docx <- paste0(path_github, "Outputs/Tobit.docx")

# Generate the Word Document
cat("Exporting table to Word...\n")
modelsummary(
  models,
  coef_map = coef_mapping,            
  gof_map = gof_mapping,              
  add_rows = custom_gof,              
  stars = c('*' = .05, '**' = .01, '***' = .001),
  title = "Piecewise Structural Equation Models (Tobit) for Compliance and Beliefs in Round 8",
  notes = "Standard errors in parentheses. Data left-censored at 0 and right-censored at 1. SEM Global fit represents Fisher's C derived from directed separation tests.",
  output = table_file_path_docx
)

message("\nSuccess! Summary table saved to: ", table_file_path_docx)