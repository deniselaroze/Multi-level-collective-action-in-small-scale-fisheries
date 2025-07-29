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



