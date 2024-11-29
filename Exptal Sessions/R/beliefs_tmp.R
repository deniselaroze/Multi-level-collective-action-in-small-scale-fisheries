# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)



####################################
#### Beliefs per Area and Treatment
####################################


# Create a vector with variable names
variable_names <- c(
  "beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini",
  "beliefsT1final.1.player.T1_belief_caleta_en_amerb_fin",
  "beliefsT1final.1.player.T1_belief_caleta_en_libre_fin",
  "beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini",
  "beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini",
  "beliefsT1final.1.player.T1_belief_pm_en_libre_fin",
  "beliefsT2inicial.1.player.T2_belief_caleta_ini",
  "beliefsT2inicial.1.player.T2_belief_caleta_conocida1_ini",
  "beliefsT2inicial.1.player.T2_belief_caleta_conocida2_ini",
  "beliefsT2final.1.player.T2_belief_caleta_fin",
  "beliefsT2final.1.player.T2_belief_caleta_conocida1_fin",
  "beliefsT2final.1.player.T2_belief_caleta_conocida2_fin"
)

# Subset the dataframe using the variable names
df_bfs <- df[, variable_names]

# Print the subsetted dataframe
print(df_bfs)


# Compute summary statistics for each variable
summary_stats <- df_bfs %>%
  summarize(
    across(
      starts_with("beliefs"),
      list(
        mean = ~mean(., na.rm = TRUE),
        median = ~median(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

summary_stats 

# Filter only the mean columns from summary_stats
mean_stats <- summary_stats %>%
  select(contains("_mean"))

# View the means in the R Viewer
#View(mean_stats)

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





#################################
####
################################

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

# Bar plot with confidence intervals and custom y-axis limits
ggplot(extraction_stats, aes(x = area, y = mean_extraction, fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +  # Bar plot
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    position = position_dodge(width = 0.8),
    width = 0.25
  ) +  # Error bars for 95% CI
  scale_y_continuous(limits = c(0, 50)) +  # Set y-axis range to 0-50
  labs(
    title = "Mean Extraction with 95% CI by Area and Treatment",
    x = "Area",
    y = "Mean Extraction",
    fill = "Treatment"
  ) +
  theme_minimal() +  # Clean theme
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )






library(ggplot2)

# Create the bar plot with confidence intervals
ggplot(beliefs_stats, aes(x = Area, y = mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +  # Bar plot
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    position = position_dodge(width = 0.8),
    width = 0.25
  ) +  # Add error bars for 95% CI
  labs(
    title = "Beliefs per area and treatment",
    x = "",
    y = "Mean Extraction Belief",
    fill = "Treatment"
  ) +
  scale_y_continuous(limits = c(0, 50)) +  # Set y-axis limits from 0 to 50
  theme_minimal()  # Apply a clean theme


library(patchwork)  # For combining plots

# First graph (original colors)
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



# Second graph (distinct colors)
plot2 <- ggplot(extraction_stats, aes(x = area, y = mean_extraction, fill = treatment)) +
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
combined_plot <- plot1 + plot2  # Stack the plots using patchwork

# Display the combined plot
print(combined_plot)







#################
### OLD
#---------------------------------------------------------------------------------

# Plot initial vs. final beliefs for T1 and T2
ggplot(long_df_bfs, aes(x = Treatment, y = Value, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bar plot with y-values
  #facet_wrap(~Variable, scales = "free") +  # Separate plots for each variable
  labs(
    title = "Belief Changes Over Time",
    x = "Treatment",
    y = "Belief Value",
    fill = "Area"
  ) +
  theme_minimal() 


# Compute differences between initial and final beliefs
difference_stats <- df_bfs %>%
  mutate(
    T1_amerb_diff = beliefsT1final.1.player.T1_belief_caleta_en_amerb_fin -
      beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini,
    T2_amerb_diff = beliefsT2final.1.player.T2_belief_caleta_fin -
      beliefsT2inicial.1.player.T2_belief_caleta_ini,
    T1_pm_diff = beliefsT1final.1.player.T1_belief_pm_en_libre_fin -
      beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini
  ) %>%
  summarize(
    across(
      starts_with("T"),
      list(mean = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    )
  )

# Plot differences between T1 and T2
ggplot(long_df_bfs, aes(x = Area, y = Value, fill = Treatment)) +
  geom_boxplot() +
  facet_wrap(~Variable, scales = "free") +
  labs(
    title = "Differences in Beliefs Between T1 and T2",
    x = "Area",
    y = "Belief Value",
    fill = "Treatment"
  )










