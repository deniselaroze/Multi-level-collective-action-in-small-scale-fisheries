##########################
### Code for presentation
##########################
# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)


#### Color pallet

#Dark Purple → #40103D
#Dark Blue → #2F4B7C 
#Teal → #1B808D
#Light Green → #56B870 (23%)
#Yellow → #F7DC38 (6%)



###########################
### Descriptive Statistics
###########################

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
means_ci_by_treatment$treatment <- recode(means_ci_by_treatment$treatment, 
                                          "T1" = "Unknown outsiders", 
                                          "T2" = "Known outsiders")

# Define custom colors
custom_colors <- c("TURF" = "#2F4B7C", "Open Access" = "#56B870")

# Plot bar graph with error bars
p<-ggplot(means_ci_by_treatment, aes(x = treatment, y = mean, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha =1) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(0.9), width = 0.2) +
  scale_fill_manual(values = custom_colors, 
                    labels = c("TURF" = "TURF", "Open Access" = "Open Access")) +
  labs(title = "Mean Compliance Extraction by Treatment",
       y = "Compliance",
       x = "Scenario",
       fill = "Management System") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"), # Bigger title
    axis.title.x = element_text(size = 16, face = "bold"), # X-axis title
    axis.title.y = element_text(size = 16, face = "bold"), # Y-axis title
    axis.text.x = element_text(size = 14), # X-axis labels
    axis.text.y = element_text(size = 14), # Y-axis labels
    legend.title = element_text(size = 16, face = "bold"), # Legend title
    legend.text = element_text(size = 14) # Legend text
  )

ggsave( file=paste0(path_github, "Outputs/compliance_per_extraction_area.pdf") , plot = p, device = "pdf", width = 10, height = 8)
















