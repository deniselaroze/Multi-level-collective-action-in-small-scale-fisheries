# ==============================================================================
# Table 1: Sample Information (Demographics) - Corrected Experience Mapping
# ==============================================================================

# Load necessary libraries
library(dplyr)
library(tidyr)
library(modelsummary)
library(sandwich)
library(lmtest)

# --- 1. SETUP PATHS AND LOAD DATA ---
rm(list=ls())
path_github <-"C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos<-"C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"
setwd(path_github)

# Reload the wide format data (crucial step!)
load(paste0(path_datos, "/Datos_islitas_recode.Rdata"))

# --- 2. CALCULATE STATISTICS ---

# Helper function to format proportions as percentages (no decimals)
fmt_pct <- function(x) {
  ifelse(is.na(x), "-", sprintf("%.0f%%", x * 100))
}

# A) Number of participants
n_participants <- nrow(df)

# B) Male (0 = Male, 1 = Female)
pct_male <- fmt_pct(mean(df$survey3.1.player.sexo == 0, na.rm = TRUE))

# C) Education Proportions
edu_props <- prop.table(table(df$survey3.1.player.estudios))
edu_1 <- fmt_pct(edu_props["1"]) # No formal studies
edu_2 <- fmt_pct(edu_props["2"]) # Incomplete Primary
edu_3 <- fmt_pct(edu_props["3"]) # Complete Primary
edu_4 <- fmt_pct(edu_props["4"]) # Incomplete High School
edu_5 <- fmt_pct(edu_props["5"]) # Complete High School

# D) Experience in TURF Proportions
# 1 = <1 year, 2 = 1 to 4 years, 3 = 5 to 9 years, 4 = 10+ years
exp_props <- prop.table(table(df$survey3.1.player.experiencia))

# Extract safely (defaults to 0 if a category is entirely missing)
val_1 <- ifelse("1" %in% names(exp_props), exp_props["1"], 0)
val_2 <- ifelse("2" %in% names(exp_props), exp_props["2"], 0)
val_3 <- ifelse("3" %in% names(exp_props), exp_props["3"], 0)
val_4 <- ifelse("4" %in% names(exp_props), exp_props["4"], 0)

# Sum categories 1 and 2 for the "4 years or less" bracket
exp_4_less  <- fmt_pct(val_1 + val_2)
exp_5_to_9  <- fmt_pct(val_3)
exp_more_10 <- fmt_pct(val_4)

# E) Leadership Role
pct_leader <- fmt_pct(mean(df$survey3.1.player.liderazgo %in% c("Sí", 1, "1"), na.rm = TRUE))

# F) Time spent gathering loco (mean)
avg_hours <- sprintf("%.1f hrs", mean(df$survey3.1.player.horas_trabajo, na.rm = TRUE))

# --- 3. BUILD THE TABLE STRUCTURE ---
table1_df <- data.frame(
  Variable = c(
    "Number of participants",
    "Male",
    "Education",
    "   No formal studies",
    "   Incomplete Primary",
    "   Complete Primary",
    "   Incomplete High School",
    "   Complete High School",
    "Experience in TURF",
    "   More than 10 years",
    "   5\u20139 years", # Using unicode en-dash for cleaner formatting
    "   4 years or less",
    "Has held leadership role",
    "Time spent gathering loco, per week avg."
  ),
  Value = c(
    as.character(n_participants),
    pct_male,
    "",             
    edu_1,
    edu_2,
    edu_3,
    edu_4,
    edu_5,
    "",             
    exp_more_10,
    exp_5_to_9,
    exp_4_less,
    pct_leader,
    avg_hours
  )
)

# Replace "NA%" with "-" in case any categories have 0 counts
table1_df$Value[table1_df$Value == "NA%"] <- "-"

# --- 4. DISPLAY AND EXPORT ---
print(table1_df)

# Export using datasummary_df
datasummary_df(
  table1_df,
  title = "Table 1: Sample information",
  align = "lc" # 'l' for left-align column 1, 'c' for center-align column 2
  , output = paste0(path_github, "Outputs/Table_1_Sample_Information.docx")
)





# ==============================================================================
# Table 2: Attitudinal scores
# ==============================================================================

# --- 2. CALCULATE SCALED VARIABLES ---
# The original 1-4 Likert scales are rescaled to 0-1 to match the manuscript
df <- df %>%
  mutate(
    # Trust
    Tst_in_group    = (survey1.1.player.confianza_caleta - 1) / 3,
    Tst_out_unknown = (survey1.1.player.confianza_pm - 1) / 3,
    Tst_out_known   = (survey2.1.player.confianza_caleta_conocida_mean - 1) / 3,
    
    # Conflict
    Cft_in_group    = (survey1.1.player.conflicto_caleta - 1) / 3,
    Cft_out_unknown = (survey1.1.player.conflicto_pm - 1) / 3,
    Cft_out_known   = (survey2.1.player.conflicto_caleta_conocida_mean - 1) / 3
  )

# --- 3. CALCULATE MEAN AND SD ---
# Helper function to easily extract, format, and round to 2 decimal places
get_stat <- function(var, stat) {
  if (stat == "mean") {
    sprintf("%.2f", mean(df[[var]], na.rm = TRUE))
  } else if (stat == "sd") {
    sprintf("%.2f", sd(df[[var]], na.rm = TRUE))
  }
}

# Build the exact table structure
table2_df <- data.frame(
  Score = c(
    "Trust",
    "   In-group",
    "   Unknown out-group",
    "   Known out-group",
    "Previous conflict",
    "   In-group",
    "   Unknown out-group",
    "   Known out-group"
  ),
  Mean = c(
    "", # Blank for header row
    get_stat("Tst_in_group", "mean"),
    get_stat("Tst_out_unknown", "mean"),
    get_stat("Tst_out_known", "mean"),
    "", # Blank for header row
    get_stat("Cft_in_group", "mean"),
    get_stat("Cft_out_unknown", "mean"),
    get_stat("Cft_out_known", "mean")
  ),
  SD = c(
    "",
    get_stat("Tst_in_group", "sd"),
    get_stat("Tst_out_unknown", "sd"),
    get_stat("Tst_out_known", "sd"),
    "",
    get_stat("Cft_in_group", "sd"),
    get_stat("Cft_out_unknown", "sd"),
    get_stat("Cft_out_known", "sd")
  )
)

# --- 4. DISPLAY AND EXPORT ---
print(table2_df)

datasummary_df(
  table2_df,
  title = "Table 2: Attitudinal scores",
  align = "lcc" # 'l' for Score column, 'c' for Mean and SD columns
  , output = paste0(path_github, "Outputs/Table_2_Attitudinal_Scores.docx")
)


# ==============================================================================
# Table 3: Compliance levels and OLS Trends (with Clustered SEs)
# ==============================================================================


# --- 1. LOAD DATA ---
# Load the wide format data (for beliefs)
load(paste0(path_datos, "/Datos_islitas_recode.Rdata"))
# Load the long format data (for average compliance and OLS trends across rounds)
load(paste0(path_datos, "/Datos_islitas_long.Rdata"))

# --- 2. PREPARE BELIEF VARIABLES (Wide Data) ---
# Helper function to safely extract and scale beliefs to 0-1
get_scaled_belief <- function(col_name) {
  if(col_name %in% names(df)) {
    return(1 - (df[[col_name]] / 50))
  } else {
    return(rep(NA, nrow(df)))
  }
}

df <- df %>%
  mutate(
    # In-group beliefs
    belief_turf_in_T1 = get_scaled_belief("beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini"),
    belief_turf_in_T2 = get_scaled_belief("beliefsT2inicial.1.player.T2_belief_caleta_en_amerb_ini"),
    belief_sa_in_T1   = get_scaled_belief("beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini"),
    belief_sa_in_T2   = get_scaled_belief("beliefsT2inicial.1.player.T2_belief_caleta_ini"),
    
    # Out-group beliefs (Shared Area only)
    belief_sa_out_T1  = get_scaled_belief("beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini"),
    belief_sa_out_T2  = get_scaled_belief("beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini")
  )

# Helper function to format Mean (SD)
fmt_msd <- function(val_vector) {
  if(all(is.na(val_vector))) return("-")
  sprintf("%.2f (%.2f)", mean(val_vector, na.rm = TRUE), sd(val_vector, na.rm = TRUE))
}

# --- 3. CALCULATE COMPLIANCE AVERAGES AND OLS TRENDS (Long Data) ---
# Ensure compliance is scaled 0-1 in the long data
dfs_long <- dfs_long %>%
  mutate(
    compliance_turf = 1 - (extraction_amerb / 50),
    compliance_sa   = 1 - (extraction_OA / 50)
  )

# Function to get Average Compliance: Mean (SD)
get_avg_comp <- function(dv_col, stage) {
  vals <- dfs_long %>% filter(treatment == stage) %>% pull(!!sym(dv_col))
  fmt_msd(vals)
}

# Function to get OLS Trend: Beta (Clustered SE) with significance stars
get_trend <- function(dv_col, stage) {
  # Filter data for the specific stage
  mod_data <- dfs_long %>% filter(treatment == stage)
  
  # Fit the standard OLS model
  f <- as.formula(paste(dv_col, "~ round"))
  mod <- lm(f, data = mod_data)
  
  # Calculate clustered standard errors by participant.code
  clustered_vcov <- vcovCL(mod, cluster = mod_data$participant.code)
  coef_test <- coeftest(mod, vcov. = clustered_vcov)
  
  # Extract the clustered estimates, SEs, and p-values
  est <- coef_test["round", "Estimate"]
  se  <- coef_test["round", "Std. Error"]
  p   <- coef_test["round", "Pr(>|t|)"]
  
  # Assign significance stars based on the clustered p-value
  stars <- ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
  
  # Format the output string
  sprintf("%.3f%s (%.3f)", est, stars, se)
}

# --- 4. ASSEMBLE THE TABLE ---
table3_df <- data.frame(
  Metric = c(
    "Information about outgroups",
    "Prior belief",
    "   In-groups compliance",
    "   Out-groups compliance",
    "Average level of compliance",
    "Trend in compliance (OLS)"
  ),
  TURF_Stage_1 = c(
    "-",
    "",
    fmt_msd(df$belief_turf_in_T1),
    "-",
    get_avg_comp("compliance_turf", "T1"),
    get_trend("compliance_turf", "T1")
  ),
  TURF_Stage_2 = c(
    "-",
    "",
    fmt_msd(df$belief_turf_in_T2),
    "-",
    get_avg_comp("compliance_turf", "T2"),
    get_trend("compliance_turf", "T2")
  ),
  Shared_Stage_1 = c(
    "Unknown union",
    "",
    fmt_msd(df$belief_sa_in_T1),
    fmt_msd(df$belief_sa_out_T1),
    get_avg_comp("compliance_sa", "T1"),
    get_trend("compliance_sa", "T1")
  ),
  Shared_Stage_2 = c(
    "Known union",
    "",
    fmt_msd(df$belief_sa_in_T2),
    fmt_msd(df$belief_sa_out_T2),
    get_avg_comp("compliance_sa", "T2"),
    get_trend("compliance_sa", "T2")
  )
)

# Rename columns for the final output
colnames(table3_df) <- c(
  " ", 
  "TURF (Stage 1)", 
  "TURF (Stage 2)", 
  "Shared Area (Stage 1)", 
  "Shared Area (Stage 2)"
)

# --- 5. DISPLAY AND EXPORT ---
print(table3_df)

datasummary_df(
  table3_df,
  title = "Table 3: Compliance levels and Trends",
  align = "lcccc", # Left align row names, center all data columns
  notes = "* p < 0.05, ** p < 0.01, *** p < 0.001"
  , output = paste0(path_github, "Outputs/Table_3_Compliance_Levels.docx")
)
  
  
  
  
  
  
  
  
  
  # ==============================================================================
  # Table 4: OLS Regression Treatment Effects Analysis (Diff-in-Diff)
  # ==============================================================================
  
  # --- 1. SETUP PATHS AND LOAD DATA ---
  # (Assuming your environment is clear, reload the long data)
  # rm(list=ls())
  # path_github <- "C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
  # path_datos <- "C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"
  # setwd(path_github)
  # load(paste0(path_datos, "/Datos_islitas_long.Rdata"))
  
  # --- 2. PREPARE THE DATA ---
  # Aggregate round-level data into participant-stage-area averages
  df_treatment <- dfs_long %>%
    mutate(
      # Ensure compliance is scaled 0-1
      compliance_TURF = 1 - (extraction_amerb / 50),
      compliance_SA   = 1 - (extraction_OA / 50)
    ) %>%
    # Group by participant and stage (treatment)
    group_by(participant.code, treatment) %>%
    summarise(
      avg_comp_TURF = mean(compliance_TURF, na.rm = TRUE),
      avg_comp_SA   = mean(compliance_SA, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Pivot longer so 'area' and 'mean_compliance' are in single columns
    pivot_longer(
      cols = c(avg_comp_TURF, avg_comp_SA),
      names_to = "area",
      values_to = "mean_compliance"
    ) %>%
    # Create the specific dummy variables from the manuscript equation
    mutate(
      stageTwo       = ifelse(treatment == "T2", 1, 0),
      sharedArea     = ifelse(area == "avg_comp_SA", 1, 0),
      knownOutGroups = stageTwo * sharedArea
    )
  
  # --- 3. FIT THE OLS MODEL ---
  ols_treat <- lm(mean_compliance ~ stageTwo + sharedArea + knownOutGroups, data = df_treatment)
  
  # --- 4. GENERATE THE REGRESSION TABLE ---
  # Output the table using modelsummary, clustering standard errors by participant
  modelsummary(
    list("Average Compliance" = ols_treat),
    vcov = ~participant.code, # Clustered SEs to account for repeated participant measures
    stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
    coef_map = c(
      "(Intercept)"    = "Constant (\U03B1\U2080 - TURF, Stage 1)",
      "stageTwo"       = "Stage 2 (\U03B1\U2081)",
      "sharedArea"     = "Shared Area (\U03B1\U2082)",
      "knownOutGroups" = "Known Out-groups (\U03B1\U2083)"
    ),
    title = "Table 4: OLS Regression - Treatment Effects on Average Compliance",
    notes = c("Standard errors are clustered at the participant level."),
    gof_omit = "IC|Log.Lik|AIC|BIC|RMSE"
    
    # Uncomment the line below to export directly to Word
    , output = paste0(path_github, "Outputs/Table_4_Treatment_Effects_OLS.docx")
  )
  
  # ==============================================================================
  # OLS Regressions: Predictors of Change in Compliance (Shared Area)
  # ==============================================================================
  
  # Load necessary libraries
  library(dplyr)
  library(modelsummary)
  
  # --- 1. SETUP PATHS AND LOAD DATA ---
  rm(list=ls())
  path_github <- "C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
  path_datos <- "C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"
  setwd(path_github)
  
  # Load the wide format data
  load(paste0(path_datos, "/Datos_islitas_recode.Rdata"))
  
  # Identify the column names for Shared Area extraction across the 8 rounds
  T1_SA_cols <- paste0("T1juegoalgas.", 1:8, ".player.T1_extraccion_libre")
  T2_SA_cols <- paste0("T2juegoalgas.", 1:8, ".player.T2_extraccion_metat")
  
  # --- 2. PREPARE VARIABLES ---
  df_models <- df %>%
    mutate(
      # A. Calculate Mean Compliance in Shared Area (Stage 1 and 2)
      mean_extraction_T1 = rowMeans(select(., all_of(T1_SA_cols)), na.rm = TRUE),
      mean_extraction_T2 = rowMeans(select(., all_of(T2_SA_cols)), na.rm = TRUE),
      comp_sa_T1 = 1 - (mean_extraction_T1 / 50),
      comp_sa_T2 = 1 - (mean_extraction_T2 / 50),
      
      # Difference in Compliance (Stage 2 - Stage 1)
      diff_comp_sa = comp_sa_T2 - comp_sa_T1,
      
      # B. Scale Beliefs (0 to 1)
      belief_sa_in_T1  = 1 - (beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50),
      belief_sa_in_T2  = 1 - (beliefsT2inicial.1.player.T2_belief_caleta_ini / 50),
      belief_sa_out_T1 = 1 - (beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50),
      belief_sa_out_T2 = 1 - (beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini / 50),
      
      # C. Calculate Difference in Beliefs (Stage 2 - Stage 1)
      diff_belief_in  = belief_sa_in_T2 - belief_sa_in_T1,
      diff_belief_out = belief_sa_out_T2 - belief_sa_out_T1,
      
      # D. Calculate Differences in Trust and Conflict (Out-group)
      diff_trust_out    = survey2.1.player.confianza_caleta_conocida_mean - survey1.1.player.confianza_pm,
      diff_conflict_out = survey2.1.player.conflicto_caleta_conocida_mean - survey1.1.player.conflicto_pm
    )
  
  # --- 3. FIT THE OLS MODELS ---
  # Model 1: Diff Compliance ~ Diff Beliefs (In-group & Out-group)
  m1_comp <- lm(diff_comp_sa ~ diff_belief_in + diff_belief_out, data = df_models)
  
  # Model 2: Diff Compliance ~ Diff Trust Out-group + Diff Conflict Out-group
  m2_comp <- lm(diff_comp_sa ~ diff_trust_out + diff_conflict_out, data = df_models)
  
  # Model 3: Diff Compliance ~ Diff Beliefs + Diff Trust + Diff Conflict
  m3_comp <- lm(diff_comp_sa ~ diff_belief_in + diff_belief_out + diff_trust_out + diff_conflict_out, data = df_models)
  
  
  # --- 4. GENERATE AND EXPORT TABLES ---
  models_list <- list(
    "Model 1" = m1_comp,
    "Model 2" = m2_comp,
    "Model 3" = m3_comp
  )
  
  coef_mapping <- c(
    "(Intercept)"       = "Constant",
    "diff_belief_in"    = "Diff. Beliefs (In-group)",
    "diff_belief_out"   = "Diff. Beliefs (Out-group)",
    "diff_trust_out"    = "Diff. Trust (Out-group)",
    "diff_conflict_out" = "Diff. Conflict (Out-group)"
  )
  
  # Custom significance stars
  custom_stars <- c('*' = 0.1, '**' = 0.05, '***' = 0.001)
  
  # A. Print to console (Markdown format)
  modelsummary(
    models_list,
    stars = custom_stars,
    statistic = "std.error", 
    coef_map = coef_mapping,
    title = "OLS Regressions: Predictors of Change in Compliance (Shared Area)",
    gof_omit = "IC|Log.Lik|AIC|BIC|RMSE",
    output = "markdown" 
  )
  
  # B. Export to Word Document (.docx)
  modelsummary(
    models_list,
    stars = custom_stars,
    statistic = "std.error",
    coef_map = coef_mapping,
    title = "OLS Regressions: Predictors of Change in Compliance (Shared Area)",
    gof_omit = "IC|Log.Lik|AIC|BIC|RMSE",
    output = paste0(path_github, "Outputs/Compliance_Diffs_Regressions.docx")
  )
  
  # C. Export to Markdown File (.md)
  modelsummary(
    models_list,
    stars = custom_stars,
    statistic = "std.error",
    coef_map = coef_mapping,
    title = "OLS Regressions: Predictors of Change in Compliance (Shared Area)",
    gof_omit = "IC|Log.Lik|AIC|BIC|RMSE",
    output = paste0(path_github, "Outputs/Compliance_Diffs_Regressions.md")
  )
  
  cat("\nSuccess! Regression tables have been printed to the console and saved as .docx and .md files in the Outputs folder.\n")