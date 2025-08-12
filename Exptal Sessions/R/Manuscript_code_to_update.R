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

if (!requireNamespace("semPlot", quietly = TRUE)) install.packages("semPlot")
if (!requireNamespace("lavaan", quietly = TRUE)) install.packages("lavaan")

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

####################################################################
#### Statistics for differences in extraction per area and scenario
####################################################################
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

model  <- lmer(compliance ~ treat.area + (1 | participant.code), data = df_long_ext)
model2 <- lmer(compliance ~ area * treatment + (1 | participant.code), data = df_long_ext)

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
  "H1: TURF vs Shared Area"     = model,
  "H2: Area × Stage"    = model2
)

modelsummary(
  models,
  coef_map   = coef_map,
  # optional: keep only the mapped rows (drop any stray contrasts)
  coef_omit  = "^$",
  stars      = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
  statistic  = "({std.error})",
  gof_omit   = "IC|Log|RMSE",  # hide AIC/BIC/LogLik if you want a cleaner table
  title      = "Empirical tests of H1 & H2 (LMM with random intercept by participant)",
  output     = paste0(path_github, "Outputs/LMM_H1_H2.docx")
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



