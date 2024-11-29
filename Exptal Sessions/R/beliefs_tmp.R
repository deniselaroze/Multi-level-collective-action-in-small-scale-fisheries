# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)



########################################################
#### Data management plots of mean extraction by others
########################################################

df_o<- df %>%
  select(contains("otros"))
names(df_o)


df_o_long <- df_o %>%
  pivot_longer(
    cols = starts_with("T"),   # All columns starting with "T" (T1 and T2 variables)
    names_to = c("treatment", "round", "area"),  # Split the names into three parts
    names_pattern = "(T\\d)juegoalgas\\.(\\d+)\\.player\\..+_extraccion_(.+)",  # Regex to extract treatment, round, and variable
    values_to = "OE_others"   # Name of the column for values
  ) %>%
  mutate(round = as.integer(round))  # Ensure round is numeric

print(df_o_long_ini)

"T1juegoalgas.2.player.T1_extraccion_otros_amerb"            
"T1juegoalgas.2.player.T1_extraccion_otros_libre"   

df_o_long$mean_OE_o<-df_o_long$OE_others/3

OE_o_stats <- df_o_long %>%
  group_by(treatment, area) %>%  # Group by treatment and area
  summarise(
    mean_extraction_o = mean(mean_OE_o, na.rm = TRUE),
    sd_extraction_o = sd(mean_OE_o, na.rm = TRUE),
    n = sum(!is.na(mean_OE_o)),  # Count of non-missing values
    .groups = "drop"  # Avoid nested grouping
  ) %>%
  mutate(
    lower_ci = mean_extraction_o - 1.96 * (sd_extraction_o / sqrt(n)),  # Lower bound of 95% CI
    upper_ci = mean_extraction_o + 1.96 * (sd_extraction_o / sqrt(n))   # Upper bound of 95% CI
  )

# View the resulting subset
print(OE_o_stats)

# Bar plot with confidence intervals and custom y-axis limits
ggplot(OE_o_stats, aes(x = area, y = mean_extraction_o, fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +  # Bar plot
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    position = position_dodge(width = 0.8),
    width = 0.25
  ) +  # Error bars for 95% CI
  scale_y_continuous(limits = c(0, 50)) +  # Set y-axis range to 0-50
  labs(
    title = "Mean Over Extraction others with 95% CI by Area and Treatment",
    x = "Area",
    y = "Mean Extraction",
    fill = "Treatment"
  ) +
  theme_minimal() +  # Clean theme
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )



##############################################
#### Data for plots of beliefs mean (ini+fin)
##################################


# Create a vector with variable names
variable_names <- c(
  "beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini",
  "beliefsT1final.1.player.T1_belief_caleta_en_amerb_fin",
  "beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini",
  "beliefsT1final.1.player.T1_belief_caleta_en_libre_fin",
  "beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini",
  "beliefsT1final.1.player.T1_belief_pm_en_libre_fin",
  "beliefsT2inicial.1.player.T2_belief_caleta_ini",
  "beliefsT2final.1.player.T2_belief_caleta_fin",
  "beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini",
  "beliefsT2inicial.1.player.T2_belief_caleta_conocida1_ini",
  "beliefsT2final.1.player.T2_belief_caleta_conocida1_fin",
  "beliefsT2inicial.1.player.T2_belief_caleta_conocida2_ini",
  "beliefsT2final.1.player.T2_belief_caleta_conocida2_fin"
)

# Subset the dataframe using the variable names
df_bfs <- df[, variable_names]


# Reshape data for visualization and comparisons
long_df_bfs <- df_bfs %>%
  pivot_longer(
    cols = starts_with("beliefs"),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Treatment = case_when(
      str_detect(Variable, "T1") ~ "T1",
      str_detect(Variable, "T2") ~ "T2",
      TRUE ~ NA_character_
    ),
    Area = case_when(
      str_detect(Variable, "amerb") ~ "ingroup_amerb",
      str_detect(Variable, "caleta_en_libre") ~ "ingroup_OA",
      str_detect(Variable, "pm") ~ "others_OA",
      str_detect(Variable, "caleta_ini") ~ "ingroup_OA",
      str_detect(Variable, "caleta_fin") ~ "ingroup_OA",
      str_detect(Variable, "conocida") ~ "others_OA",
      TRUE ~ "Other"
    )
  )



# Calculate means, SD, and 95% CI
plot_stats <- long_df_bfs %>%
  group_by(Area, Treatment) %>%
  summarize(
    mean = mean(Value, na.rm = TRUE),
    sd = sd(Value, na.rm = TRUE),
    n = sum(!is.na(Value)),  # Count of non-NA values
    .groups = "drop"  # Avoid nested group structure
  ) %>%
  mutate(
    lower_ci = mean - 1.96 * (sd / sqrt(n)),
    upper_ci = mean + 1.96 * (sd / sqrt(n))
  )

# View the resulting dataframe
print(plot_stats)

# Reorder the table so ingroup_amerb is first
beliefs_stats <- plot_stats %>%
  arrange(factor(Area, levels = c("ingroup_amerb", "ingroup_OA", "others_OA")), Treatment)

# View the ordered table
print(beliefs_stats)


######################################
#### Data for plots of beliefs initial
######################################


# Create a vector with variable names
var_names<- c(
  "beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini",
  "beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini",
  "beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini",
  "beliefsT2inicial.1.player.T2_belief_caleta_ini",
  "beliefsT2inicial.1.player.T2_belief_caleta_conocida1_ini",
  "beliefsT2inicial.1.player.T2_belief_caleta_conocida2_ini")

# Subset the dataframe using the variable names
df_bfs_ini <- df[, var_names]

# Reshape data for visualization and comparisons
long_df_bfs_ini <- df_bfs_ini %>%
  pivot_longer(
    cols = starts_with("beliefs"),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Treatment = case_when(
      str_detect(Variable, "T1") ~ "T1",
      str_detect(Variable, "T2") ~ "T2",
      TRUE ~ NA_character_
    ),
    Area = case_when(
      str_detect(Variable, "amerb") ~ "ingroup_amerb",
      str_detect(Variable, "caleta_en_libre") ~ "ingroup_OA",
      str_detect(Variable, "pm") ~ "others_OA",
      str_detect(Variable, "caleta_ini") ~ "ingroup_OA",
      str_detect(Variable, "caleta_fin") ~ "ingroup_OA",
      str_detect(Variable, "conocida") ~ "others_OA",
      TRUE ~ "Other"
    )
  )



# Calculate means, SD, and 95% CI
plot_stats <- long_df_bfs_ini %>%
  group_by(Area, Treatment) %>%
  summarize(
    mean = mean(Value, na.rm = TRUE),
    sd = sd(Value, na.rm = TRUE),
    n = sum(!is.na(Value)),  # Count of non-NA values
    .groups = "drop"  # Avoid nested group structure
  ) %>%
  mutate(
    lower_ci = mean - 1.96 * (sd / sqrt(n)),
    upper_ci = mean + 1.96 * (sd / sqrt(n))
  )

# View the resulting dataframe
print(plot_stats)

# Reorder the table so ingroup_amerb is first
beliefs_stats_ini <- plot_stats %>%
  arrange(factor(Area, levels = c("ingroup_amerb", "ingroup_OA", "others_OA")), Treatment)



###########################################################################
#### Data management plots of mean extraction (all 8 rounds per treatment) 
###########################################################################

# Calculate mean, SD, and 95% CI for each treatment and area
extraction_stats <- dfs_long %>%
  group_by(treatment, area) %>%  # Group by treatment and area
  summarise(
    mean_extraction = mean(extraction, na.rm = TRUE),
    sd_extraction = sd(extraction, na.rm = TRUE),
    n = sum(!is.na(extraction)),  # Count of non-missing values
    .groups = "drop"  # Avoid nested grouping
  ) %>%
  mutate(
    lower_ci = mean_extraction - 1.96 * (sd_extraction / sqrt(n)),  # Lower bound of 95% CI
    upper_ci = mean_extraction + 1.96 * (sd_extraction / sqrt(n))   # Upper bound of 95% CI
  )

# View the resulting subset
print(extraction_stats)

# # Bar plot with confidence intervals and custom y-axis limits
# ggplot(extraction_stats, aes(x = area, y = mean_extraction, fill = treatment)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +  # Bar plot
#   geom_errorbar(
#     aes(ymin = lower_ci, ymax = upper_ci),
#     position = position_dodge(width = 0.8),
#     width = 0.25
#   ) +  # Error bars for 95% CI
#   scale_y_continuous(limits = c(0, 50)) +  # Set y-axis range to 0-50
#   labs(
#     title = "Mean Extraction with 95% CI by Area and Treatment",
#     x = "Area",
#     y = "Mean Extraction",
#     fill = "Treatment"
#   ) +
#   theme_minimal() +  # Clean theme
#   theme(
#     plot.title = element_text(hjust = 0.5),  # Center the title
#     axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
#   )





###########################################
### Plots Beliefs and Mean Over-extraction
##########################################

# Mean Beliefs Graph 
plot1 <- ggplot(beliefs_stats, aes(x = Area, y = mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +  # Bar plot
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    position = position_dodge(width = 0.8),
    width = 0.25
  ) +  # Error bars
  labs(
    title = "Beliefs per Area and Treatment",
    x = "",
    y = "Mean beliefs of over-extraction",
    fill = "Treatment"
  ) +
  scale_y_continuous(limits = c(0, 50)) +  # Set y-axis limits
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Mean Beliefs Graph 
plot2 <- ggplot(beliefs_stats_ini, aes(x = Area, y = mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +  # Bar plot
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    position = position_dodge(width = 0.8),
    width = 0.25
  ) +  # Error bars
  labs(
    title = "Initial Beliefs per Area and Treatment",
    x = "",
    y = "Round 0 beliefs of over-extraction",
    fill = "Treatment"
  ) +
  scale_y_continuous(limits = c(0, 50)) +  # Set y-axis limits
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Mean Extraction plot (mean rounds 1-8)
plot3 <- ggplot(extraction_stats, aes(x = area, y = mean_extraction, fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    position = position_dodge(width = 0.8),
    width = 0.25
  ) +
  scale_y_continuous(limits = c(0, 50)) +
  #scale_fill_manual(values = c("#1b9e77", "#d95f02")) +  # Custom colors for treatments
  labs(
    title = "Mean personal Over-extraction per Area and Treatment",
    x = "",
    y = "Mean personal Over-extraction",
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot1
plot2
# Combine the two plots vertically
combined_plot1 <- plot1 + plot3  # Stack the plots using patchwork
combined_plot2 <- plot2 + plot3  # Stack the plots using patchwork

# Display the combined plot
print(combined_plot1)
print(combined_plot2)






