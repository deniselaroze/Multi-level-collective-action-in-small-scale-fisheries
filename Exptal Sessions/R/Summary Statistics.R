#############################
#### Manuscript information
#############################

# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(stargazer)
library(sandwich)   # For robust and clustered standard errors
library(lmtest)
library(lme4)
library(modelsummary)
library(tinytable)
library(rlang)
library(pandoc)
library(dplyr)
library(ggplot2)
library(viridis)
library(lavaan)
library(semPlot)

rm(list=ls())
path_github <-"C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos<-"C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

#path_github <-"C:/Users/Denise Laroze/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
#path_datos<-"C:/Users/Denise Laroze/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

setwd(path_github)


#load(paste0(path_datos, "/Datos_islitas.Rdata"))
load(paste0(path_datos, "/Datos_islitas_recode.Rdata"))
load(paste0(path_datos, "/Datos_islitas_long.Rdata"))

######################
#### Summary Table
######################

# --- calculate row means for Shared Area extraction over 8 rounds ---
T1_SA_cols <- intersect(paste0("T1juegoalgas.", 1:8, ".player.T1_extraccion_libre"), names(df))
T2_SA_cols <- intersect(paste0("T2juegoalgas.", 1:8, ".player.T2_extraccion_metat"), names(df))

df$extraction_SA_T1_mean <- rowMeans(df %>% select(all_of(T1_SA_cols)), na.rm = TRUE)
df$extraction_SA_T2_mean <- rowMeans(df %>% select(all_of(T2_SA_cols)), na.rm = TRUE)

# --- calculate row means for TURF extraction over 8 rounds ---
T1_TURF_cols <- intersect(paste0("T1juegoalgas.", 1:8, ".player.T1_extraccion_amerb"), names(df))
T2_TURF_cols <- intersect(paste0("T2juegoalgas.", 1:8, ".player.T2_extraccion_amerb"), names(df))

df$extraction_TURF_T1_mean <- rowMeans(df %>% select(all_of(T1_TURF_cols)), na.rm = TRUE)
df$extraction_TURF_T2_mean <- rowMeans(df %>% select(all_of(T2_TURF_cols)), na.rm = TRUE)

# --- calculate row means for Shared Area OBSERVED extraction over 8 rounds ---
T1_obs_SA_cols <- intersect(paste0("T1juegoalgas.", 1:8, ".player.T1_extraccion_otros_libre"), names(df))
T2_obs_SA_cols <- intersect(paste0("T2juegoalgas.", 1:8, ".player.T2_extraccion_otros_metat"), names(df))

df$obs_extraction_SA_T1_mean <- rowMeans(df %>% select(all_of(T1_obs_SA_cols)), na.rm = TRUE)
df$obs_extraction_SA_T2_mean <- rowMeans(df %>% select(all_of(T2_obs_SA_cols)), na.rm = TRUE)

# --- compute new/updated variables (single mutate for clarity) ---
df <- df %>%
  mutate(
    # Scales (0–1)
    Tst_sa_T1_scaled  = (survey1.1.player.confianza_pm - 1) / 3,
    Cft_sa_T1_scaled  = (survey1.1.player.conflicto_pm- 1) / 3,
    Tst_caleta_scaled = (survey1.1.player.confianza_caleta- 1) / 3,
    Cft_caleta_scaled = (survey1.1.player.conflicto_caleta- 1) / 3,
    Tst_sa_t2_scaled  = (survey2.1.player.confianza_caleta_conocida_mean- 1) / 3,
    Cft_sa_t2_scaled  = (survey2.1.player.conflicto_caleta_conocida_mean- 1) / 3,
    
    # Beliefs → compliance (0–1)
    belief_compliance_SA_T1    = 1 - (beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50),
    belief_compliance_union_T1 = 1 - (beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50),
    belief_compliance_SA_T2    = 1 - (beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini / 50),
    belief_compliance_union_T2 = 1 - (beliefsT2inicial.1.player.T2_belief_caleta_ini / 50),
    
    # New: Compliance in Shared Areas (Means and Round 8 - Round 1 Differences)
    compliance_SA_T1_mean = 1 - (extraction_SA_T1_mean / 50),
    compliance_SA_T2_mean = 1 - (extraction_SA_T2_mean / 50),
    compliance_SA_T1_diff8_1 = (1 - (T1juegoalgas.8.player.T1_extraccion_libre / 50)) - (1 - (T1juegoalgas.1.player.T1_extraccion_libre / 50)),
    compliance_SA_T2_diff8_1 = (1 - (T2juegoalgas.8.player.T2_extraccion_metat / 50)) - (1 - (T2juegoalgas.1.player.T2_extraccion_metat / 50)),
    
    # New: Observed Compliance in Shared Areas (Means and Round 7 - Round 1 Differences)
    # Divided by 150 because it represents the sum of 3 other players' extractions (3 * 50)
    obs_compliance_SA_T1_mean = 1 - (obs_extraction_SA_T1_mean / 150),
    obs_compliance_SA_T2_mean = 1 - (obs_extraction_SA_T2_mean / 150),
    obs_compliance_SA_T1_diff7_1 = (1 - (T1juegoalgas.7.player.T1_extraccion_otros_libre / 150)) - (1 - (T1juegoalgas.1.player.T1_extraccion_otros_libre / 150)),
    obs_compliance_SA_T2_diff7_1 = (1 - (T2juegoalgas.7.player.T2_extraccion_otros_metat / 150)) - (1 - (T2juegoalgas.1.player.T2_extraccion_otros_metat / 150)),
    
    # New: Compliance in TURF (0-1)
    compliance_TURF_T1_mean = 1 - (extraction_TURF_T1_mean / 50),
    compliance_TURF_T2_mean = 1 - (extraction_TURF_T2_mean / 50)
  )

# --- variables of interest (df-level; keep desired order) ---
vars_df <- c(
  "Tst_caleta_scaled", "Cft_caleta_scaled",
  "Tst_sa_T1_scaled", "Cft_sa_T1_scaled",
  "Tst_sa_t2_scaled",  "Cft_sa_t2_scaled",
  "belief_compliance_union_T1", "belief_compliance_SA_T1", 
  "belief_compliance_union_T2", "belief_compliance_SA_T2", 
  "compliance_TURF_T1_mean", "compliance_TURF_T2_mean",
  "compliance_SA_T1_mean", "compliance_SA_T2_mean",
  "compliance_SA_T1_diff8_1", "compliance_SA_T2_diff8_1",
  "obs_compliance_SA_T1_mean", "obs_compliance_SA_T2_mean",
  "obs_compliance_SA_T1_diff7_1", "obs_compliance_SA_T2_diff7_1",
  "survey3.1.player.horas_trabajo"
)

# --- summarise helper: one row per variable ---
summarize_vars <- function(df, vars) {
  df %>%
    select(all_of(vars)) %>%
    summarise(across(
      everything(),
      list(
        Mean = ~mean(., na.rm = TRUE),
        SD   = ~sd(.,   na.rm = TRUE),
        N    = ~sum(!is.na(.))
      ),
      .names = "{.col}__{.fn}"
    )) %>%
    pivot_longer(
      everything(),
      names_to = c("Variable", ".value"),
      names_sep = "__"
    )
}

# --- summaries ---
summary_df       <- summarize_vars(df, vars_df)

####################Categorical variables

cat_vars <- c("survey3.1.player.sexo",
              "survey3.1.player.estudios",
              "survey3.1.player.liderazgo")

summary_num <- summary_df %>%
  mutate(
    Variable = factor(Variable, levels = vars_df),
    Panel = "Continuous"
  ) %>%
  select(Panel, Variable, Mean, SD, N) %>%
  mutate(N = as.character(N))   # keep type compatible with categorical N

# --- helper to summarize categoricals with counts and percentages ---

summarize_cats <- function(df, vars) {
  df_long <- df |>
    mutate(across(all_of(vars), as.character)) |>
    tidyr::pivot_longer(all_of(vars),
                        names_to = "Variable", values_to = "value")
  
  counts <- df_long |>
    filter(!is.na(value) & value != "") |>
    count(Variable, value, name = "Count")
  
  totals <- df_long |>
    filter(!is.na(value) & value != "") |>
    count(Variable, name = "Ntotal")
  
  left_join(counts, totals, by = "Variable") |>
    mutate(
      Percent  = 100 * Count / Ntotal,
      # N column now includes count + percent
      N        = sprintf("%d (%.1f%%)", Count, Percent),
      Variable = paste0(Variable, ": ", value)
    ) |>
    select(Variable, N)
}

# --- build categorical panel (NO var_labels here) ---
cat_panel <- summarize_cats(df, cat_vars) |>
  mutate(
    Panel = "Categorical",
    Mean = NA_real_, SD = NA_real_
  ) |>
  select(Panel, Variable, Mean, SD, N)   # Variable is e.g. "survey3...sexo: 0"


# --- combined table: continuous first, then categorical ---
summary_combined <- bind_rows(summary_num, cat_panel) %>%
  mutate(Panel = factor(Panel, levels = c("Continuous", "Categorical")))
# no arrange() needed unless you want alpha-order within each panel



######################### Exporting correctly
var_labels <- c(
  # Compliance in TURF by Stage
  "compliance_TURF_T1_mean" = "Compliance TURF (Mean stage 1)",
  "compliance_TURF_T2_mean" = "Compliance TURF (Mean stage 2)",
  
  # Compliance in Shared Areas by Stage and Difference (Round 8 - Round 1)
  "compliance_SA_T1_mean" = "Compliance Shared Area (SA) (Mean stage 1)",
  "compliance_SA_T1_diff8_1"   = "Diff. Compliance SA (R8 - R1, stage 1)",
  "compliance_SA_T2_mean" = "Compliance SA (Mean stage 2)",
  "compliance_SA_T2_diff8_1"   = "Diff. Compliance SA (R8 - R1, stage 2)",
  
  # Belief-based compliance (0–1)
  "belief_compliance_SA_T1"    = "Prior Beliefs Unknown out-group in SA (stage 1)",
  "belief_compliance_union_T1" = "Prior Beliefs in-group in SA(stage 1)",
  "belief_compliance_SA_T2"    = "Prior Beliefs Known out-group in SA (stage 2)",
  "belief_compliance_union_T2" = "Prior Beliefs in-group in SA (stage 2)",
  
  # Observed Compliance in Shared Areas by Stage and Difference (Round 7 - Round 1)
  "obs_compliance_SA_T1_mean" = "Obs. Compliance SA (Mean stage 1)",
  "obs_compliance_SA_T1_diff7_1"   = "Diff. Obs. Compliance SA (R7 - R1, stage 1)",
  "obs_compliance_SA_T2_mean" = "Obs. Compliance SA (Mean stage 2)",
  "obs_compliance_SA_T2_diff7_1"   = "Diff. Obs. Compliance SA (R7 - R1, stage 2)",
  
  # Trust / Conflict (scales 0–1)
  "Tst_caleta_scaled"  = "Trust in-group",
  "Cft_caleta_scaled"  = "Conflict in-group",
  "Tst_sa_T1_scaled"   = "Trust Unknown out-group",
  "Cft_sa_T1_scaled"   = "Conflict Unknown out-group",
  "Tst_sa_t2_scaled"   = "Trust Known out-group",
  "Cft_sa_t2_scaled"   = "Conflict Known out-group",
  
  # Continuous demographic
  "survey3.1.player.horas_trabajo" = "Hours in loco fishing",
  
  # Base labels for categoricals (fallback)
  "survey3.1.player.liderazgo" = "Held leadership role",
  "survey3.1.player.estudios"  = "Level of education",
  "survey3.1.player.sexo"      = "Sex",
  
  # Category-specific labels (preferential when present)
  "survey3.1.player.liderazgo: No" = "Held leadership role: No",
  "survey3.1.player.liderazgo: Sí" = "Held leadership role: Yes",
  
  "survey3.1.player.estudios: 1" = "Level of education: No formal studies",
  "survey3.1.player.estudios: 2" = "Level of education: Incomplete Primary",
  "survey3.1.player.estudios: 3" = "Level of education: Complete Primary",
  "survey3.1.player.estudios: 4" = "Level of education: Incomplete High School",
  "survey3.1.player.estudios: 5" = "Level of education: Complete High School",
  
  "survey3.1.player.sexo: 0" = "Sex: Male",
  "survey3.1.player.sexo: 1" = "Sex: Female"
)

summary_all <- summary_combined %>%
  rowwise() %>%
  mutate(
    VariableLabel = {
      v <- as.character(Variable)
      # Try full key first (works for "var: category")
      full <- unname(var_labels[v])
      if (!is.na(full)) full else {
        # If it's "var: category", try base label + category
        parts <- strsplit(v, ": ", fixed = TRUE)[[1]]
        if (length(parts) == 2) {
          base <- parts[1]; cat <- parts[2]
          base_label <- unname(var_labels[base])
          if (!is.na(base_label)) paste0(base_label, ": ", cat) else v
        } else {
          # Just a base name
          base_label <- unname(var_labels[v])
          if (!is.na(base_label)) base_label else v
        }
      }
    }
  ) %>%
  ungroup() %>%
  select(Variable = VariableLabel, Mean, SD, N)

# --- Order the final table based on the var_labels dictionary ---
summary_all <- summary_all %>%
  mutate(Variable = factor(Variable, levels = unname(var_labels))) %>%
  arrange(Variable) %>%
  mutate(Variable = as.character(Variable)) # Convert back to character for safe exporting

# --- pretty table ---
datasummary_df(
  summary_all,
  title  = "Summary Statistics",
  output = paste0(path_github, "Outputs/summary_statistics.docx")
)


######################################################################
### Adaptation Strategies: Proportion & Cross-Tabulation Tables
######################################################################

# Load necessary libraries
library(dplyr)
library(tidyr)
library(modelsummary)
library(tinytable)

# --- 1. SETUP & DATA RE-CALCULATION ---
# (Assuming your environment is set up, but we quickly re-calculate 
# the differences here just to ensure this script is self-contained and runs safely)

path_github <- "C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos  <- "C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

load(paste0(path_datos, "/Datos_islitas_recode.Rdata"))

# Ensure the difference variables exist in df
df <- df %>%
  mutate(
    compliance_SA_T1_diff8_1 = (1 - (T1juegoalgas.8.player.T1_extraccion_libre / 50)) - (1 - (T1juegoalgas.1.player.T1_extraccion_libre / 50)),
    compliance_SA_T2_diff8_1 = (1 - (T2juegoalgas.8.player.T2_extraccion_metat / 50)) - (1 - (T2juegoalgas.1.player.T2_extraccion_metat / 50)),
    obs_compliance_SA_T1_diff7_1 = (1 - (T1juegoalgas.7.player.T1_extraccion_otros_libre / 150)) - (1 - (T1juegoalgas.1.player.T1_extraccion_otros_libre / 150)),
    obs_compliance_SA_T2_diff7_1 = (1 - (T2juegoalgas.7.player.T2_extraccion_otros_metat / 150)) - (1 - (T2juegoalgas.1.player.T2_extraccion_otros_metat / 150))
  )

# Create categorical variables using the sign() function (-1, 0, 1)
df <- df %>%
  mutate(
    own_cat_T1 = factor(sign(compliance_SA_T1_diff8_1), levels = c(-1, 0, 1), labels = c("Decreased", "Maintained", "Increased")),
    own_cat_T2 = factor(sign(compliance_SA_T2_diff8_1), levels = c(-1, 0, 1), labels = c("Decreased", "Maintained", "Increased")),
    obs_cat_T1 = factor(sign(obs_compliance_SA_T1_diff7_1), levels = c(-1, 0, 1), labels = c("Decreased", "Maintained", "Increased")),
    obs_cat_T2 = factor(sign(obs_compliance_SA_T2_diff7_1), levels = c(-1, 0, 1), labels = c("Decreased", "Maintained", "Increased"))
  )

# --- 2. TABLE 1: MARGINAL PROPORTIONS ---
# Helper function to get formatted proportions
get_props <- function(variable) {
  props <- prop.table(table(df[[variable]])) * 100
  sprintf("%.1f%%", props)
}

table1_proportions <- data.frame(
  `Variable` = c(
    "Own Compliance (Stage 1)", 
    "Own Compliance (Stage 2)", 
    "Observed Compliance of Others (Stage 1)", 
    "Observed Compliance of Others (Stage 2)"
  ),
  `Decreased`  = c(get_props("own_cat_T1")[1], get_props("own_cat_T2")[1], get_props("obs_cat_T1")[1], get_props("obs_cat_T2")[1]),
  `Maintained` = c(get_props("own_cat_T1")[2], get_props("own_cat_T2")[2], get_props("obs_cat_T1")[2], get_props("obs_cat_T2")[2]),
  `Increased`  = c(get_props("own_cat_T1")[3], get_props("own_cat_T2")[3], get_props("obs_cat_T1")[3], get_props("obs_cat_T2")[3]),
  check.names = FALSE
)


# --- 3. TABLE 2: CROSS-TABULATIONS ---
# Generate cross-tabs for Stage 1
xtab_T1 <- table(Own = df$own_cat_T1, Observed = df$obs_cat_T1)
prop_xtab_T1 <- prop.table(xtab_T1) * 100
df_xtab_T1 <- as.data.frame.matrix(apply(prop_xtab_T1, c(1,2), function(x) sprintf("%.1f%%", x)))
df_xtab_T1 <- cbind(Stage = "Stage 1 (Unknown out-group)", `Own Compliance` = rownames(df_xtab_T1), df_xtab_T1)

# Generate cross-tabs for Stage 2
xtab_T2 <- table(Own = df$own_cat_T2, Observed = df$obs_cat_T2)
prop_xtab_T2 <- prop.table(xtab_T2) * 100
df_xtab_T2 <- as.data.frame.matrix(apply(prop_xtab_T2, c(1,2), function(x) sprintf("%.1f%%", x)))
df_xtab_T2 <- cbind(Stage = "Stage 2 (Known out-group)", `Own Compliance` = rownames(df_xtab_T2), df_xtab_T2)

# Combine both cross-tabs into a single elegant table
table2_crosstab <- bind_rows(df_xtab_T1, df_xtab_T2)
rownames(table2_crosstab) <- NULL

# Rename columns to clearly indicate these are the "Observed Compliance" categories
colnames(table2_crosstab)[3:5] <- paste("Observed", colnames(table2_crosstab)[3:5])


# --- 4. EXPORT TABLES ---
datasummary_df(
  table1_proportions,
  title = "Table A: Proportion of Participants by Compliance Adaptation Strategy",
  notes = "Values represent the percentage of participants who decreased, maintained, or increased their compliance (Round 8 vs Round 1) or observed compliance (Round 7 vs Round 1).",
  output = paste0(path_github, "Outputs/Adaptation_Proportions.docx")
)

datasummary_df(
  table2_crosstab,
  title = "Table B: Cross-Tabulation of Own Compliance Change vs. Observed Compliance Change",
  notes = "Values represent the joint percentage of the sample falling into each combination of behavioral adaptation.",
  output = paste0(path_github, "Outputs/Adaptation_CrossTab.docx")
)

cat("\nSuccess! Both adaptation tables have been exported to the Outputs folder.\n")



