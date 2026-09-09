



# ==============================================================================
# Histograms for Raw Differences in Out-group Trust and Conflict
# ==============================================================================

# --- 1. CALCULATE RAW DIFFERENCES ---
df_clean <- df_clean %>%
  mutate(
    # Difference in Trust (Stage 2 - Stage 1) using raw variables
    diff_trust = survey2.1.player.confianza_caleta_conocida_mean - survey1.1.player.confianza_pm,
    
    # Difference in Conflict (Stage 2 - Stage 1) using raw variables
    diff_conflict = survey2.1.player.conflicto_caleta_conocida_mean - survey1.1.player.conflicto_pm
  )

# --- 2. GENERATE HISTOGRAMS ---

# Histogram 1: Difference in Trust
p_hist_trust <- ggplot(df_clean, aes(x = diff_trust)) +
  geom_histogram(
    fill = "#21908CFF",  # Viridis teal to match previous plots
    color = "black", 
    alpha = 0.7, 
    bins = 15            # Adjust bins as needed based on the integer spread
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Difference in Out-group Trust",
    subtitle = "Stage 2 (Known) - Stage 1 (Unknown) [Raw Values]",
    x = "Difference in Trust",
    y = "Frequency (Count)"
  )

print(p_hist_trust)

# Histogram 2: Difference in Conflict
p_hist_conflict <- ggplot(df_clean, aes(x = diff_conflict)) +
  geom_histogram(
    fill = "#3F4A8A",    # Dark blue to match previous plots
    color = "black", 
    alpha = 0.7, 
    bins = 15            # Adjust bins as needed
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Difference in Out-group Conflict",
    subtitle = "Stage 2 (Known) - Stage 1 (Unknown) [Raw Values]",
    x = "Difference in Conflict",
    y = "Frequency (Count)"
  )

print(p_hist_conflict)





# ==============================================================================
# Multiple OLS Regression: Predictors of Change in Compliance
# ==============================================================================

# Ensure modelsummary is loaded for the table output
library(modelsummary)

# ==============================================================================
# Multiple OLS Regression: Predictors of Change in Compliance
# ==============================================================================

# Ensure modelsummary is loaded for the table output
library(modelsummary)

# --- 1. FIT THE OLS MODELS ---
# Regress the difference in compliance on the differences in beliefs, trust, and conflict
ols_multiple <- lm(diff_compliance ~ diff_beliefs + diff_trust + diff_conflict, data = df_clean)

ols_3 <- lm(diff_compliance ~ diff_beliefs , data = df_clean)

# Regress the difference in compliance on the differences in trust and conflict only
ols2 <- lm(diff_compliance ~ diff_trust + diff_conflict, data = df_clean)

# --- 2. REGRESSION TABLE ---
# Output a clean regression table comparing both models, with the CI displayed below the estimate
modelsummary(
  list(
    "Model 1 (Full)" = ols_multiple, 
    "Model 2 (beliefs)" =ols_3,
    "Model 3"        = ols2
  ),
  stars     = c('*' = .05, '**' = .01, '***' = .001),
  statistic = "conf.int", # Displays the 95% CI below the estimate
  coef_map  = c(
    "(Intercept)"   = "Constant",
    "diff_beliefs"  = "Difference in Beliefs",
    "diff_trust"    = "Difference in Trust",
    "diff_conflict" = "Difference in Conflict"
  ),
  title     = "OLS Regressions: Predictors of Change in Compliance",
  gof_omit  = "IC|Log.Lik|AIC|BIC|RMSE" # Keeps the goodness-of-fit section clean
  
  # Uncomment the line below to export directly to Word
  # , output = paste0(path_github, "Outputs/OLS_Multiple_Diff.docx")
)




# ==============================================================================
# SEM Alternative: Coefficient Forest Plot and Separated Table
# ==============================================================================

# Load necessary libraries
library(lavaan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(modelsummary)

# --- 1. SPECIFY AND FIT THE SIMPLIFIED SEM ---
# Strictly modeling direct regressions; no indirect effects calculated.
sem_simple <- '
  # Regression on Beliefs
  diff_beliefs ~ diff_trust + diff_conflict
  
  # Regression on Compliance
  diff_compliance ~ diff_beliefs + diff_trust + diff_conflict
'

# Fit the model
fit_simple <- sem(sem_simple, data = df_clean, estimator = "ML")


# --- 2. PREPARE DATA FOR PLOTTING & TABLE ---
# Extract standardized estimates (std.all is usually best for plotting relative effect sizes)
sem_params <- parameterEstimates(fit_simple, standardized = TRUE) %>%
  filter(op == "~") %>% # Keep only the regression paths
  mutate(
    # Clean up dependent variable names for faceting
    DV = ifelse(lhs == "diff_beliefs", "DV: Difference in Beliefs", "DV: Difference in Compliance"),
    
    # Clean up predictor names
    Predictor = case_when(
      rhs == "diff_beliefs" ~ "Difference in Beliefs",
      rhs == "diff_trust" ~ "Difference in Trust",
      rhs == "diff_conflict" ~ "Difference in Conflict"
    ),
    
    # Add significance stars for the table
    Significance = case_when(
      pvalue < 0.001 ~ "***",
      pvalue < 0.01 ~ "**",
      pvalue < 0.05 ~ "*",
      pvalue < 0.1  ~ "\u2020", 
      TRUE ~ ""
    ),
    
    # Format text for the table: Estimate*** (SE) - Using unstandardized 'est' for the table
    Formatted = sprintf("%.3f%s (%.3f)", est, Significance, se)
  )


# --- 3. GENERATE THE COEFFICIENT PLOT ---
# This plot separates the two DVs into two panels and plots the standardized effects with 95% CIs
p_coef <- ggplot(sem_params, aes(x = std.all, y = Predictor, color = DV)) +
  # Add a vertical reference line at zero
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  
  # Plot the point estimates and confidence intervals (using SE * 1.96 for 95% CI)
  geom_errorbarh(aes(xmin = std.all - (1.96 * se), xmax = std.all + (1.96 * se)), 
                 height = 0.2, linewidth = 1) +
  geom_point(size = 4) +
  
  # Separate into two panels based on the Dependent Variable
  facet_wrap(~ DV, scales = "free_y", ncol = 1) +
  
  # Styling to match your previous plots
  scale_color_manual(values = c("#3F4A8A", "#21908CFF")) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "gray90", color = "black"),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.y = element_text(face = "bold")
  ) +
  labs(
    title = "Standardized Effects on Beliefs and Compliance",
    subtitle = "Coefficient Plot with 95% Confidence Intervals",
    x = "Standardized Coefficient (\u03b2)",
    y = ""
  )

# Display the cleaner visual output
print(p_coef)


# --- 4. CREATE SEPARATED REGRESSION TABLE ---
# Pivot the formatted text wider to separate the two regressions into side-by-side columns
table_data <- sem_params %>%
  select(Predictor, DV, Formatted) %>%
  pivot_wider(names_from = DV, values_from = Formatted, values_fill = "") %>%
  arrange(match(Predictor, c("Difference in Beliefs", "Difference in Trust", "Difference in Conflict")))

# Output using datasummary_df for a clean exportable table
datasummary_df(
  table_data,
  title = "SEM Path Estimates: Predictors of Change in Beliefs and Compliance",
  notes = c("\u2020 p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001",
            "Unstandardized estimates reported. Standard errors in parentheses.")
  
  # Uncomment below to export directly
  # , output = paste0(path_github, "Outputs/SEM_Separated_Table.docx")
)