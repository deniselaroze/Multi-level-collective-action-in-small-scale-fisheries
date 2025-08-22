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
#path_github <-"C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
#path_datos<-"C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

path_github <-"C:/Users/Denise Laroze/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos<-"C:/Users/Denise Laroze/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

setwd(path_github)


#load(paste0(path_datos, "/Datos_islitas.Rdata"))
load(paste0(path_datos, "/Datos_islitas_recode.Rdata"))
load(paste0(path_datos, "/Datos_islitas_long.Rdata"))

######################
#### Summary Table
######################



# --- compute new/updated variables (single mutate for clarity) ---
df <- df %>%
  mutate(
    # Scales (0–1)
    Tst_sa_T1_scaled  = (survey1.1.player.confianza_pm        - 1) / 3,
    Cft_sa_T1_scaled  = (survey1.1.player.conflicto_pm        - 1) / 3,
    Tst_caleta_scaled = (survey1.1.player.confianza_caleta    - 1) / 3,
    Cft_caleta_scaled = (survey1.1.player.conflicto_caleta    - 1) / 3,
    Tst_sa_t2_scaled  = (survey2.1.player.confianza_caleta_conocida_mean  - 1) / 3,
    Cft_sa_t2_scaled  = (survey2.1.player.conflicto_caleta_conocida_mean  - 1) / 3,
    
    # Beliefs → compliance (0–1)
    belief_compliance_SA_T1    = 1 - (beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50),
    belief_compliance_union_T1 = 1 - (beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50),
    belief_compliance_SA_T2    = 1 - (beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini / 50),
    belief_compliance_union_T2 = 1 - (beliefsT2inicial.1.player.T2_belief_caleta_ini / 50)
  )

# --- variables of interest (df-level; keep desired order) ---
vars_df <- c(
  "Tst_caleta_scaled", "Cft_caleta_scaled",
  "Tst_sa_T1_scaled", "Cft_sa_T1_scaled",
  "Tst_sa_t2_scaled",  "Cft_sa_t2_scaled",
  "belief_compliance_union_T1", "belief_compliance_SA_T1", 
  "belief_compliance_union_T2", "belief_compliance_SA_T2", 
  "survey3.1.player.horas_trabajo"
)

# --- long df vars (unchanged) ---
vars_dfs_long <- c(
  "compliance_extraction_amerb",                                         
  "compliance_lag_extraction_others_amerb_mean",
  "compliance_extraction_OA", 
  "compliance_lag_extraction_others_OA_mean"
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
    tidyr::pivot_longer(
      everything(),
      names_to = c("Variable", ".value"),
      names_sep = "__"
    )
}

# --- summaries ---
summary_df       <- summarize_vars(df, vars_df)
summary_dfs_long <- summarize_vars(dfs_long, vars_dfs_long)


####################Categorical variables

cat_vars <- c("survey3.1.player.sexo",
              "survey3.1.player.estudios",
              "survey3.1.player.liderazgo")

summary_num <- bind_rows(summary_dfs_long, summary_df) %>%
  mutate(
    Variable = factor(Variable, levels = c(vars_df, vars_dfs_long)),
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
  # Trust / Conflict (scales 0–1)
  "Tst_sa_T1_scaled"   = "Trust Unknown out-group",
  "Cft_sa_T1_scaled"   = "Conflict Unknown out-group",
  "Tst_caleta_scaled"  = "Trust Union",
  "Cft_caleta_scaled"  = "Conflict Union",
  "Tst_sa_t2_scaled"   = "Trust Known out-group",
  "Cft_sa_t2_scaled"   = "Conflict Known out-group",
  
  # Belief-based compliance (0–1)
  "belief_compliance_SA_T1"    = "Prior Beliefs Compliance Shared Area Unknown out-group",
  "belief_compliance_union_T1" = "Prior Beliefs Compliance Shared Area in-group (rounds 1–8)",
  "belief_compliance_SA_T2"    = "Prior Beliefs Compliance Shared Area Known out-group",
  "belief_compliance_union_T2" = "Prior Beliefs Compliance Shared Area in-group (rounds 9–16)",
  
  # Continuous demographic
  "survey3.1.player.horas_trabajo" = "Hours in loco fishing",
  
  # Outcomes & lags
  "compliance_extraction_amerb"                     = "Compliance (TURF)",
  "compliance_extraction_OA"                        = "Compliance (Shared Area)",
  "compliance_lag_extraction_others_amerb_mean"     = "Observed Compliance TURF (t−1)",
  "compliance_lag_extraction_others_OA_mean"        = "Observed Compliance Shared Area (t−1)",
  
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

# --- pretty table ---
modelsummary::datasummary_df(
  summary_all,
  title  = "Summary Statistics",
  output = paste0(path_github, "Outputs/summary.docx")
)


##########################
### Descriptive Statistics
##########################
prop.table(table(df$survey3.1.player.sexo))
prop.table(table(df$survey3.1.player.liderazgo))
prop.table(table(df$survey3.1.player.experiencia))
prop.table(table(df$survey3.1.player.estudios))


age<-2024-df$survey3.1.player.nacimiento
summary(age)
sd(age)

summary(df$survey3.1.player.horas_trabajo)
sd(df$survey3.1.player.horas_trabajo)


##########################
### Mean comparisons
##########################

# Function to calculate mean, standard deviation, and confidence intervals
calculate_mean_ci <- function(x, conf_level = 0.95) {
  n <- sum(!is.na(x))
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  se_x <- sd_x / sqrt(n)
  ci_margin <- qt(conf_level + (1 - conf_level) / 2, df = n - 1) * se_x
  
  return(data.frame(mean = mean_x, sd = sd_x, lower_ci = mean_x - ci_margin, upper_ci = mean_x + ci_margin))
}

# Compute means, SDs, and confidence intervals by treatment
means_ci_by_treatment <- dfs_long %>%
  group_by(treatment) %>%
  summarise(
    amerb_mean = calculate_mean_ci(compliance_extraction_amerb)$mean,
    amerb_sd = calculate_mean_ci(compliance_extraction_amerb)$sd,
    amerb_lower_ci = calculate_mean_ci(compliance_extraction_amerb)$lower_ci,
    amerb_upper_ci = calculate_mean_ci(compliance_extraction_amerb)$upper_ci,
    OA_mean = calculate_mean_ci(compliance_extraction_OA)$mean,
    OA_sd = calculate_mean_ci(compliance_extraction_OA)$sd,
    OA_lower_ci = calculate_mean_ci(compliance_extraction_OA)$lower_ci,
    OA_upper_ci = calculate_mean_ci(compliance_extraction_OA)$upper_ci
  ) %>%
  pivot_longer(cols = -treatment, names_to = c("variable", "stat"), names_sep = "_", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value)

# Update variable names
means_ci_by_treatment$variable <- recode(means_ci_by_treatment$variable, 
                                         "amerb" = "TURF", 
                                         "OA" = "Open Access")
means_ci_by_treatment$variable <- factor(means_ci_by_treatment$variable, 
                                         levels = c("TURF", "Open Access"))

means_ci_by_treatment$treatment <- recode(means_ci_by_treatment$treatment, 
                                          "T1" = "Unknown Outsiders", 
                                          "T2" = "Known Outsiders")
means_ci_by_treatment$treatment <- factor(means_ci_by_treatment$treatment, 
                                          levels = c("Unknown Outsiders", "Known Outsiders"))


print(means_ci_by_treatment)

#######################################
#### Data Manipulation for H1 and H2
#######################################
df_long_ext <- dfs_long %>%
  pivot_longer(
    cols = c(extraction_amerb, extraction_OA),
    names_to = "area",
    values_to = "extraction"
  ) %>%
  mutate(
    # simplify area labels
    area = recode(
      area,
      extraction_amerb = "TURF",
      extraction_OA    = "Shared_Area"
    ),
    # compute compliance
    compliance = 1 - (extraction / 50),
    complianceDummy = if_else(compliance == 1, 1, 0)  # vectorised if
  )


with(df_long_ext, {
  treat.area <- paste0(area, "_", treatment)
  # Define factor with desired order
  treat.area <- factor(treat.area,
                       levels = c(
                         "TURF_T1", "TURF_T2",
                         "Shared_Area_T1", "Shared_Area_T2"
                       ))
  # Explicitly relevel to ensure baseline
  treat.area <- relevel(treat.area, ref = "TURF_T1")
  df_long_ext$treat.area <<- treat.area
})

df_long_ext$area <- relevel(factor(df_long_ext$area), ref = "TURF")


######################################################################################
### Empirical tests of H1 and H2: differences between SA in T1 and Turf in T1 and T2:
######################################################################################

# A robust lmerControl (helps reduce spurious convergence warnings)
ctrl <- lmerControl(
  optimizer = "bobyqa",
  optCtrl   = list(maxfun = 2e5),
  check.conv.grad = .makeCC("warning", tol = 1e-3, relTol = NULL)
)

# ---------- Cluster bootstrap for lmer ----------
bootstrap_lmer <- function(model_formula, data, cluster_var, B = 2000, seed = 62354234) {
  # Fit once on the full data (ML) to get the coefficient skeleton & order
  base_fit <- lmer(model_formula, data = data, control = ctrl, REML = FALSE)
  coef_names <- names(fixef(base_fit))
  
  # One bootstrap replication: sample clusters with replacement
  one_rep <- function(d, i_unused) {
    # unique, non-missing clusters
    clusters <- unique(stats::na.omit(d[[cluster_var]]))
    # if too few clusters, return NAs
    if (length(clusters) < 2L) return(setNames(rep(NA_real_, length(coef_names)), coef_names))
    
    # resample cluster ids (with replacement), keep all rows from chosen clusters
    samp <- sample(clusters, length(clusters), replace = TRUE)
    d_b  <- dplyr::bind_rows(lapply(samp, function(cl) d[d[[cluster_var]] == cl, , drop = FALSE]))
    
    # fit
    fit <- try(lmer(model_formula, data = d_b, control = ctrl, REML = FALSE), silent = TRUE)
    out <- setNames(rep(NA_real_, length(coef_names)), coef_names)
    if (inherits(fit, "try-error")) return(out)
    
    cf <- try(fixef(fit), silent = TRUE)
    if (!inherits(cf, "try-error")) out[names(cf)] <- as.numeric(cf)
    out
  }
  
  set.seed(seed)
  bt <- boot::boot(
    data = data,
    statistic = function(d, i) one_rep(d, i),  # i is unused; we resample clusters internally
    R = B,
    parallel = "no"  # set to "multicore"/"snow" if you want parallelism
  )
  
  draws <- bt$t
  colnames(draws) <- coef_names
  
  # Covariance & SEs from the bootstrap draws
  V  <- stats::cov(draws, use = "pairwise.complete.obs")
  se <- sqrt(diag(V))
  
  # Percentile CIs (quick, vectorized)
  qs <- apply(draws, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
  ci_lower <- qs[1,]
  ci_upper <- qs[2,]
  
  list(
    base_fit  = base_fit,   # lmer fit on full data (ML)
    boot      = bt,         # full boot object
    draws     = draws,      # B x p matrix of bootstrap coefficients
    V         = V,          # bootstrap covariance (cluster-robust)
    se        = se,         # bootstrap SEs
    ci_lower  = ci_lower,   # 2.5% percentile CI
    ci_upper  = ci_upper,   # 97.5% percentile CI
    coef_names = coef_names
  )
}



#----------- Estimate models ##############
# Your models (ML fits are inside the bootstrap anyway)
model  <- lmer(compliance ~ treat.area + (1 | participant.code), data = df_long_ext)
model2 <- lmer(compliance ~ area * treatment + (1 | participant.code), data = df_long_ext)

# Cluster variable is the subject id:
clvar <- "gid.treat" # for TURF or "gid.treat" for Shared Area

# Run the cluster bootstrap (increase B for publication results)
res1 <- bootstrap_lmer(compliance ~ treat.area + (1 | participant.code),
                       data = df_long_ext, cluster_var = clvar, B = 2000)
res2 <- bootstrap_lmer(compliance ~ area * treatment + (1 | participant.code),
                       data = df_long_ext, cluster_var = clvar, B = 2000)

# If you use modelsummary, pass the bootstrap vcov so p-values/SEs reflect the cluster bootstrap:
library(modelsummary)

# Optional: label coefficients
my_labels <- c(
  "(Intercept)"              = "Intercept (TURF rounds 1–8)",
  "treat.areaTURF_T2"        = "TURF rounds 9–16",
  "treat.areaShared_Area_T1" = "Shared Area Unknown Out-group",
  "treat.areaShared_Area_T2" = "Shared Area Known Out-group",
  "areaShared_Area"          = "Area (Shared)",
  "treatmentT2"              = "Stage (Rounds 9–16)",
  "areaShared_Area:treatmentT2" = "Area × Stage"
)

# Make sure the vcov matrices align with the fitted object coef order
vcov1 <- res1$V[ names(fixef(res1$base_fit)), names(fixef(res1$base_fit)) ]
vcov2 <- res2$V[ names(fixef(res2$base_fit)), names(fixef(res2$base_fit)) ]

modelsummary(
  list("H1: TURF vs Shared Area" = res1$base_fit,
       "H2: Area × Stage" = res2$base_fit),
  vcov      = list(vcov1, vcov2),
  coef_map  = my_labels,
  stars     = c("*" = .05, "**" = .01, "***" = .001),  # only these three
  gof_omit  = "IC|Log.Lik|AIC|BIC",                    # cleaner table
  output    = paste0(path_github, "Outputs/LMM_H1_H2_boot_gid.treat.docx")
)


######################################################################################
### Robustness tests H1 and H2
######################################################################################


model3  <- lmer(complianceDummy ~ treat.area + (1 | participant.code), data = df_long_ext)
model4 <- lmer(complianceDummy ~ area * treatment + (1 | participant.code), data = df_long_ext)

# One coef map covering ALL terms that appear in either model (order = row order)
coef_map <- c(
  "(Intercept)"                      = "Intercept (TURF rounds 1–8)",
  "treat.areaTURF_T2"                = "TURF (rounds 9–16)",
  "treat.areaShared_Area_T1"         = "Shared Area Unknown Out-group",
  "treat.areaShared_Area_T2"         = "Shared Area Known Out-group",
  "areaShared_Area"                  = "Area (Shared)",
  "treatmentT2"                      = "Stage (rounds 9-16)",
  "areaShared_Area:treatmentT2"      = "Area × Stage"
)

# Model names for columns
models <- list(
  "H1: TURF vs Shared Area"     = model3,
  "H2: Area × Stage"    = model4
)

modelsummary(
  models,
  coef_map   = coef_map,
  # optional: keep only the mapped rows (drop any stray contrasts)
  coef_omit  = "^$",
  stars      = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
  statistic  = "({std.error})",
  gof_omit   = "IC|Log|RMSE",  # hide AIC/BIC/LogLik if you want a cleaner table
  title      = "Empirical tests of H1 & H2 (LMM with random intercept by participant). Compliance defined as 0 or 1 (full compliance). ",
  output     = paste0(path_github, "Outputs/LMM_H1_H2_SM.docx")
)




### Summary statistics and tests
treatment_summary <- df_long_ext %>%
  group_by(treat.area) %>%
  summarise(
    n = n(),
    mean_compliance = mean(compliance, na.rm = TRUE),
    sd_compliance   = sd(compliance,   na.rm = TRUE),
    se_compliance   = sd_compliance / sqrt(n),
    ci_lower        = mean_compliance - qt(0.975, df = n - 1) * se_compliance,
    ci_upper        = mean_compliance + qt(0.975, df = n - 1) * se_compliance
  ) %>%
  ungroup()


print(treatment_summary)

kw_all <- kruskal.test(compliance ~ treat.area, data = df_long_ext)
cat("\nKruskal-Wallis test across treat.area levels:\n")
print(kw_all)

pairwise_res <- pairwise.wilcox.test(df_long_ext$compliance, df_long_ext$treat.area,
                                     p.adjust.method = "bonferroni")
cat("\nPairwise Wilcoxon tests (Bonferroni-adjusted p-values):\n")
print(pairwise_res)

##################################################
#### Diff in diff TURF T1 and T2, vs SA T1 and T2
##################################################
did_df <- df_long_ext %>%
  group_by(participant.code, treat.area, round) %>%
  summarise(mean_comp = mean(compliance, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = treat.area,
    values_from = mean_comp
  ) %>%
  mutate(
    diff_turf   = TURF_T2 - TURF_T1,
    diff_shared = Shared_Area_T2 - Shared_Area_T1,
    DiD         = diff_turf - diff_shared
  )

# View the first few rows of DiD calculations
print(head(did_df))

# 9. Test if mean DiD differs from zero
# 9a. One-sample t-test
t_test_did <- t.test(did_df$DiD, mu = 0)
cat("One-sample t-test for DiD ≠ 0:
")
print(t_test_did)

# 9b. Wilcoxon signed-rank test (non-parametric)
wilcox_did <- wilcox.test(did_df$DiD, mu = 0)
cat("Wilcoxon signed-rank test for DiD ≠ 0:
")
print(wilcox_did)

# (Optional) 10. Summary statistics for DiD
did_summary <- did_df %>%
  summarise(
    n      = n(),
    mean_DiD = mean(DiD, na.rm = TRUE),
    sd_DiD   = sd(DiD, na.rm = TRUE),
    se_DiD   = sd_DiD / sqrt(n),
    ci_lower = mean_DiD - qt(0.975, df = n - 1) * se_DiD,
    ci_upper = mean_DiD + qt(0.975, df = n - 1) * se_DiD
  )

print(did_summary)

########################################
### Descriptive graph H1 and H2 - SM
########################################

# --- summary by round × treat.area ---
sum_ext <- df_long_ext %>%
  group_by(round, treat.area) %>%
  summarise(
    n    = dplyr::n(),
    mean = mean(compliance, na.rm = TRUE),
    sd   = sd(compliance,   na.rm = TRUE),
    se   = sd / sqrt(n),
    ci   = 1.96 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ymin = pmax(0, mean - ci),
    ymax = pmin(1, mean + ci)
  )

# legend labels for treat.area
labs_map <- c(
  "TURF_T1"        = "TURF (rounds 1-8)",
  "TURF_T2"        = "TURF (rounds 9-16)",
  "Shared_Area_T1" = "Shared Area (Unknown outsiders)",
  "Shared_Area_T2" = "Shared Area (Known out-siders)"
)

# map linetype & shape in aes so scales take effect
p <- ggplot(
  sum_ext,
  aes(x = round, y = mean,
      color = treat.area, linetype = treat.area, shape = treat.area,
      group = treat.area)
) +
  geom_line() +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.15, alpha = 0.85) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1), expand = expansion(mult = c(0.02, 0.04))
  ) +
  # give all three scales the same name & breaks so legend combines cleanly
  scale_color_viridis_d(
    end = 0.9, option = "D", name = "Area × Stage",
    breaks = names(labs_map), labels = unname(labs_map)
  ) +
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted", "longdash"),
    name = "Area × Stage",
    breaks = names(labs_map), labels = unname(labs_map)
  ) +
  scale_shape_manual(
    values = c(16,17,15,18),
    name   = "Area × Stage",
    breaks = names(labs_map), labels = unname(labs_map)
  ) +
  labs(
    x = "Round",
    y = "Mean compliance",
    title = "Compliance by area × stage across rounds"
  ) +
  theme_bw(base_size = 14) +  # keep fonts large under theme_bw
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 12),
    plot.title   = element_text(size = 14)
  )

print(p)

ggsave(
  filename = paste0(path_github, "Outputs/compliance_by_round_treat_area.png"),
  plot = p, width = 12, height = 8, dpi = 300
)





##################################################
### Graph beliefs by extraction area and scenario
##################################################

# Belief compliance for amerb
df$belief_compliance_amerb_T1 <- 1 - (df$beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini / 50)
df$belief_compliance_pm_T1 <- 1 - (df$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50)
df$belief_compliance_union_T1 <- 1 - (df$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50)
df$belief_compliance_pm_T2 <- 1 - (df$beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini / 50)
df$belief_compliance_union_T2 <- 1 - (df$beliefsT2inicial.1.player.T2_belief_caleta_ini/ 50)



# Create long dataframe
beliefs_df <- data.frame(
  value = c(
    df$belief_compliance_amerb_T1,
    df$belief_compliance_union_T1,
    df$belief_compliance_pm_T1,
    df$belief_compliance_union_T2,
    df$belief_compliance_pm_T2
  ),
  group = rep(c("Amerb", "Union OA", "Others OA","Union OA", "Others OA"), each = nrow(df)),
  treatment = rep(c("T1", "T1", "T1", "T2", "T2"), each = nrow(df))
)

# Summarize with mean and CI
beliefs_summary <- beliefs_df %>%
  group_by(group, treatment) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    n = sum(!is.na(value)),
    lower_ci = mean - 1.96 * sd / sqrt(n),
    upper_ci = mean + 1.96 * sd / sqrt(n),
    .groups = "drop"
  )

# Clean group names and set order
beliefs_summary$group <- factor(
  beliefs_summary$group,
  levels = c("Amerb", "Union OA", "Others OA")
)


print(beliefs_summary)
# Re-plot with fixed order
# p<-ggplot(beliefs_summary, aes(x = group, y = mean, fill = treatment)) +
#   geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
#   geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
#                 width = 0.25,
#                 position = position_dodge(0.8)) +
#   scale_fill_manual(
#     values = c("T1" = "#3F4A8A", "T2" = "#6CCE5A"),
#     labels = c("Unknown Outsiders", "Known Outsiders")
#   ) +
#   labs(
#     title = "Belief-based Compliance by Group and Scenario",
#     x = "",
#     y = "Belief Compliance (0–1)",
#     fill = "Scenarios"
#   ) +
#   scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     axis.title.y = element_text(size = 14),
#     legend.title = element_text(face = "bold")
#   )
# 
# 
# ggsave(file = paste0(path_github, "Outputs/beliefs_per_extraction_area.png"), 
#        plot = p, device = "png", width = 10, height = 8)



