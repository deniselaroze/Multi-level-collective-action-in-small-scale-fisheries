##########################
### Code for presentation
##########################
# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(stargazer)


rm(list=ls())
path_github <-"C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos<-"C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

#path_github <-"C:/Users/Denise Laroze/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
#path_datos<-"C:/Users/Denise Laroze/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

setwd(path_github)


#load(paste0(path_datos, "/Datos_islitas.Rdata"))
load(paste0(path_datos, "/Datos_islitas_recode.Rdata"))
load(paste0(path_datos, "/Datos_islitas_long.Rdata"))




#### Color pallet

#Dark Purple → #40103D
#Dark Blue → #2F4B7C 
#Teal → #1B808D
#Light Green → #56B870 (23%)
#Yellow → #F7DC38 (6%)



########################################################
### Graph Compliance per management system and scenario
########################################################

# Function to calculate mean and confidence intervals
calculate_mean_ci <- function(x, conf_level = 0.95) {
  n <- sum(!is.na(x)) # Count non-NA values
  mean_x <- mean(x, na.rm = TRUE) # Compute mean
  se_x <- sd(x, na.rm = TRUE) / sqrt(n) # Standard Error
  ci_margin <- qt(conf_level + (1 - conf_level) / 2, df = n - 1) * se_x # CI margin
  
  return(data.frame(mean = mean_x, lower_ci = mean_x - ci_margin, upper_ci = mean_x + ci_margin))
}

# Compute means and confidence intervals by treatment
means_ci_by_treatment <- dfs_long %>%
  group_by(treatment) %>%
  summarise(
    amerb_mean = mean(compliance_extraction_amerb, na.rm = TRUE),
    amerb_lower_ci = calculate_mean_ci(compliance_extraction_amerb)$lower_ci,
    amerb_upper_ci = calculate_mean_ci(compliance_extraction_amerb)$upper_ci,
    OA_mean = mean(compliance_extraction_OA, na.rm = TRUE),
    OA_lower_ci = calculate_mean_ci(compliance_extraction_OA)$lower_ci,
    OA_upper_ci = calculate_mean_ci(compliance_extraction_OA)$upper_ci
  ) %>%
  pivot_longer(cols = -treatment, names_to = c("variable", "stat"), names_sep = "_", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value)

# Update variable names for proper labeling
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


# Define custom colors
custom_colors <- c("TURF" = "#2F4B7C", "Open Access" = "#56B870")

# Plot bar graph with error bars and mean labels
p <- ggplot(means_ci_by_treatment, aes(x = treatment, y = mean, fill = variable)) +
  
  # Bar plot
  geom_bar(stat = "identity", position = position_dodge(), alpha = 1) +
  
  # Error bars
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(0.9), width = 0.2) +
  
  # Add mean values as text labels above bars
  geom_text(aes(label = round(mean, 2)), 
            position = position_dodge(0.9), 
            vjust = -1.3, 
            size = 5) +
  
  # Custom colors
  scale_fill_manual(values = custom_colors, 
                    labels = c("TURF" = "TURF", "Open Access" = "Open Access")) +
  
  # Labels
  labs(title = "",
       y = "Compliance",
       x = "Scenario",
       fill = "Management System") +
  
  # Y-axis settings
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0), breaks = seq(0, 1, by = 0.1)) +
  
  # Theme
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)
  )

# Print the plot
print(p)

# Save the figure
ggsave(file = paste0(path_github, "Outputs/compliance_per_extraction_area.png"), 
       plot = p, device = "png", width = 10, height = 8)





#################################################
### Graph of Compliance on lag Compliance others
#################################################

#### AMERB

p <- ggplot(dfs_long, aes(x = compliance_lag_extraction_others_amerb_mean, 
                          y = compliance_extraction_amerb, 
                          color = treatment)) +
  
  # Regression lines (one for each treatment group)
  geom_smooth(method = "lm", se = TRUE, linetype = "solid", size = 1.2) +
  
  # Y-axis settings
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1), expand = c(0, 0)) +
  
  # X-axis settings
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1), expand = c(0, 0)) +
  
  # Custom colors for treatment groups (Fixed color code issue)
  scale_color_manual(values = c("T1" = "#40103D", "T2" = "#F7DC38"), 
                     labels = c("T1" = "Unknown Outsiders", "T2" = "Known Outsiders")) +
  
  # Labels
  labs(title = "Compliance in TURF",
       y = "Compliance Decision Maker",
       x = "Lagged Compliance Others in Group",
       color = "Scenario") +
  
  # Theme
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)
  )

# Print the plot
print(p)
ggsave( file=paste0(path_github, "Outputs/compliance_per_lag_TURF.png") , plot = p, device = "png", width = 10, height = 8)



##### Open Access

p <- ggplot(dfs_long, aes(x = compliance_lag_extraction_others_OA_mean, 
                          y = compliance_extraction_OA, 
                          color = treatment)) +
  
  # Regression lines (one for each treatment group)
  geom_smooth(method = "lm", se = TRUE, linetype = "solid", size = 1.2) +
  
  # Y-axis settings
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1), expand = c(0, 0)) +
  
  # X-axis settings
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1), expand = c(0, 0)) +
  
  # Custom colors for treatment groups (Fixed color code issue)
  scale_color_manual(values = c("T1" = "#40103D", "T2" = "#F7DC38"), 
                     labels = c("T1" = "Unknown Outsiders", "T2" = "Known Outsiders")) +
  
  # Labels
  labs(title = "Compliance in Open Access area",
       y = "Compliance Decision Maker",
       x = "Lagged Compliance Others in Group",
       color = "Scenario") +
  
  # Theme
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)
  )

# Print the plot
print(p)
ggsave( file=paste0(path_github, "Outputs/compliance_per_lag_OA.png") , plot = p, device = "png", width = 10, height = 8)



###############################################
#### Reg on extraction by beliefs and controls
###############################################


# TURF compliance controlling for the number identities people see on screen -- produces the same results

lm2 <- lm(compliance_extraction_amerb ~ compliance_lag_extraction_others_amerb_mean + compliance_beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini + treatment + n_identities, data = dfs_long)
lm3 <- lm(compliance_extraction_amerb ~ compliance_lag_extraction_others_amerb_mean + compliance_beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini + treatment + n_identities + survey3.1.player.sexo
            + survey3.1.player.horas_trabajo + survey3.1.player.estudios + survey3.1.player.liderazgo, data = dfs_long)

# Calculate clustered standard errors by 'participant.code'
clustered_se_lm2 <- sqrt(diag(vcovCL(lm2, cluster = ~gid.amerb)))
clustered_se_lm3 <- sqrt(diag(vcovCL(lm3, cluster = ~gid.amerb)))

# Export to stargazer with clustered standard errors

stargazer(lm2, lm3,
          se = list(clustered_se_lm1, clustered_se_lm2, clustered_se_lm3),
          type = "html",
          out = paste0(path_github, "Outputs/compliance_extraction_AMERB_controls_gid_clustered_se.html"),
          title = "Regression Results with Clustered Standard Errors",
          dep.var.labels = "Proportion Compliance TURF",
          covariate.labels = c("Mean Compliance by Group (t-1)", "Beliefs Compliance by Ingroup in TURF", 
                               "Scenario Know Outsiders (dummy)", "Sessions with 3 Unions (Dummy)" , "Female (Dummy)",
                               "Fishing hrs/w", "Level of Education" ,"Held Leadership Role (Dummy)", "Constant" ),
          notes = "OLS regressions with TURF matching group clustered s.e. in parentheses.")




# OA compliance with controls

lm1 <- lm(compliance_extraction_OA ~ compliance_lag_extraction_others_OA_mean + compliance_beliefs_OA_caleta + compliance_beliefs_OA_others + treatment + n_identities + minority, data = dfs_long)

lm2 <- lm(compliance_extraction_OA ~ compliance_lag_extraction_others_OA_mean + compliance_beliefs_OA_caleta + compliance_beliefs_OA_others + treatment + n_identities + minority + survey3.1.player.sexo + 
            survey3.1.player.horas_trabajo + survey3.1.player.estudios + survey3.1.player.liderazgo, data = dfs_long)


# Calculate clustered standard errors by 'participant.code'
clustered_se_lm1 <- sqrt(diag(vcovCL(lm1, cluster = ~gid.treat)))
clustered_se_lm2 <- sqrt(diag(vcovCL(lm2, cluster = ~gid.treat)))
#clustered_se_lm3 <- sqrt(diag(vcovCL(lm3, cluster = ~gid.treat)))
#clustered_se_lm4 <- sqrt(diag(vcovCL(lm4, cluster = ~gid.treat)))

# Export to stargazer with clustered standard errors
stargazer(lm1, lm2,
          se = list(clustered_se_lm1, clustered_se_lm2),
          type = "html",
          out = paste0(path_github, "Outputs/compliance_OA_beliefs_clustered_se.html"),
          title = "Regression Results with Clustered Standard Errors",
          dep.var.labels = "Proportion Compliance Open Access",
          covariate.labels = c("Mean Compliance by Group (t-1)", "Beliefs Compliance by Ingroup in TURF", "Beliefs Compliance by Outsiders in TURF", 
                               "Scenario Know Outsiders (dummy)", "Sessions with 3 Unions (Dummy)" , "Minority (Dummy)","Female (Dummy)",
                               "Fishing hrs/w", "Level of Education" ,"Held Leadership Role (Dummy)", "Constant" ),
          notes = "OLS regressions with session matching group clustered s.e. in parentheses.")



