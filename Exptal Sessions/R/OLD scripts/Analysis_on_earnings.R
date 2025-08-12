### Earnings

library(dplyr)
library(ggplot2)
library(viridis)


# variable subsets

# Get all variable names
#all_vars <- names(df)

# Identify payoff-related variables using common keywords
#payoff_vars <- grep("payoff|pago", all_vars, value = TRUE, ignore.case = TRUE)

# View result
#print(payoff_vars)

# Optionally: create a subset dataframe with only those variables
#df_payoffs <- df[, payoff_vars]


# Summarize payoff by belief category
payoff_summary <- df %>%
  group_by(belief_cat_union_T2) %>%
  summarise(
    mean = mean(participant.payoff, na.rm = TRUE),
    sd = sd(participant.payoff, na.rm = TRUE),
    n = sum(!is.na(participant.payoff)),
    lower_ci = mean - 1.96 * sd / sqrt(n),
    upper_ci = mean + 1.96 * sd / sqrt(n),
    .groups = "drop"
  )

# Plot
ggplot(payoff_summary, aes(x = belief_cat_union_T2, y = mean, fill = belief_cat_union_T2)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  scale_fill_viridis_d(option = "D", end = 0.9) +
  labs(
    title = "Participant Payoff by Belief Category (T1 - Unknown Outsiders)",
    x = "Belief Category",
    y = "Mean Payoff",
    fill = "Belief Category"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    legend.position = "none"
  )


#### Plot Belief categories
#############################

# Define categories to loop
belief_vars <- c(
  "belief_cat_amerb_T1",
  "belief_cat_pm_T1",
  "belief_cat_union_T1",
  "belief_cat_pm_T2",
  "belief_cat_union_T2"
)

# Combine into long format
payoff_beliefs <- lapply(belief_vars, function(var) {
  df %>%
    select(participant.payoff, belief_cat = all_of(var)) %>%
    mutate(belief_type = var)
}) %>% bind_rows()

# Summary
payoff_summary_all <- payoff_beliefs %>%
  group_by(belief_type, belief_cat) %>%
  summarise(
    mean = mean(participant.payoff, na.rm = TRUE),
    sd = sd(participant.payoff, na.rm = TRUE),
    n = sum(!is.na(participant.payoff)),
    lower_ci = mean - 1.96 * sd / sqrt(n),
    upper_ci = mean + 1.96 * sd / sqrt(n),
    .groups = "drop"
  )

# Format labels for prettier output
payoff_summary_all$belief_type <- recode(
  payoff_summary_all$belief_type,
  belief_cat_amerb_T1 = "Union AMERB T1",
  belief_cat_pm_T1 = "Others OA T1",
  belief_cat_union_T1 = "Union OA T1",
  belief_cat_pm_T2 = "Others OA T2",
  belief_cat_union_T2 = "Union OA T2"
)

# Plot
ggplot(payoff_summary_all, aes(x = belief_cat, y = mean, fill = belief_cat)) +
  geom_col(position = position_dodge(), width = 0.6) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = position_dodge(width = 0.6)) +
  facet_wrap(~ belief_type) +
  scale_fill_viridis_d(option = "D", end = 0.9) +
  labs(
    title = "Mean Participant Payoff by Belief Category and Group",
    x = "Belief Category",
    y = "Mean Payoff",
    fill = "Belief Category"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    legend.position = "none"
  )



###### For all belief categories

library(dplyr)
library(ggplot2)
library(viridis)

# Define categories to loop
belief_vars <- c(
  "belief_cat_amerb_T1",
  "belief_cat_pm_T1",
  "belief_cat_union_T1",
  "belief_cat_pm_T2",
  "belief_cat_union_T2"
)

# Combine into long format
payoff_beliefs <- lapply(belief_vars, function(var) {
  df %>%
    select(participant.payoff, belief_cat = all_of(var)) %>%
    mutate(belief_type = var)
}) %>% bind_rows()

# Summary
payoff_summary_all <- payoff_beliefs %>%
  group_by(belief_type, belief_cat) %>%
  summarise(
    mean = mean(participant.payoff, na.rm = TRUE),
    sd = sd(participant.payoff, na.rm = TRUE),
    n = sum(!is.na(participant.payoff)),
    lower_ci = mean - 1.96 * sd / sqrt(n),
    upper_ci = mean + 1.96 * sd / sqrt(n),
    .groups = "drop"
  )

# Format labels for prettier output
payoff_summary_all$belief_type <- recode(
  payoff_summary_all$belief_type,
  belief_cat_amerb_T1 = "AMERB T1",
  belief_cat_pm_T1 = "Others OA T1",
  belief_cat_union_T1 = "Union OA T1",
  belief_cat_pm_T2 = "Others OA T2",
  belief_cat_union_T2 = "Union OA T2"
)

# Plot
ggplot(payoff_summary_all, aes(x = belief_cat, y = mean, fill = belief_cat)) +
  geom_col(position = position_dodge(), width = 0.6) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = position_dodge(width = 0.6)) +
  facet_wrap(~ belief_type) +
  scale_fill_viridis_d(option = "D", end = 0.9) +
  labs(
    title = "Mean Participant Payoff by Belief Category and Group",
    x = "Belief Category",
    y = "Mean Payoff",
    fill = "Belief Category"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    legend.position = "none"
  )




#####################################################
#################### Figures with loop and exporting 
#####################################################


library(dplyr)
library(ggplot2)
library(viridis)

# List of belief category vars
belief_vars <- c(
  "belief_cat_amerb_T1",
  "belief_cat_pm_T1",
  "belief_cat_union_T1",
  "belief_cat_pm_T2",
  "belief_cat_union_T2"
)

# Combine into long format with labels
payoff_beliefs <- lapply(belief_vars, function(var) {
  df %>%
    select(participant.payoff, belief_cat = all_of(var)) %>%
    mutate(belief_type = var)
}) %>% bind_rows()

# Summary stats
payoff_summary_all <- payoff_beliefs %>%
  group_by(belief_type, belief_cat) %>%
  summarise(
    mean = mean(participant.payoff, na.rm = TRUE),
    sd = sd(participant.payoff, na.rm = TRUE),
    n = sum(!is.na(participant.payoff)),
    lower_ci = mean - 1.96 * sd / sqrt(n),
    upper_ci = mean + 1.96 * sd / sqrt(n),
    .groups = "drop"
  )

# Nicer labels
payoff_summary_all$belief_type <- recode(
  payoff_summary_all$belief_type,
  belief_cat_amerb_T1 = "AMERB_T1",
  belief_cat_pm_T1 = "Others_OA_T1",
  belief_cat_union_T1 = "Union_OA_T1",
  belief_cat_pm_T2 = "Others_OA_T2",
  belief_cat_union_T2 = "Union_OA_T2"
)

# === Combined facet plot ===
p_all <- ggplot(payoff_summary_all, aes(x = belief_cat, y = mean, fill = belief_cat)) +
  geom_col(position = position_dodge(), width = 0.6) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = position_dodge(width = 0.6)) +
  facet_wrap(~ belief_type) +
  scale_fill_viridis_d(option = "D", end = 0.9) +
  labs(
    title = "Mean Participant Payoff by Belief Category and Group",
    x = "Belief Category", y = "Mean Payoff", fill = "Belief Category"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    legend.position = "none"
  )

# Save combined plot
ggsave(paste0(path_github, "Outputs/payoffs_by_belief_all_facet.png"), plot = p_all, width = 12, height = 8)

# === Individual plots per belief category ===
belief_types <- unique(payoff_summary_all$belief_type)

for (bt in belief_types) {
  df_sub <- payoff_summary_all %>% filter(belief_type == bt)
  
  p <- ggplot(df_sub, aes(x = belief_cat, y = mean, fill = belief_cat)) +
    geom_col(position = position_dodge(), width = 0.6) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = position_dodge(width = 0.6)) +
    scale_fill_viridis_d(option = "D", end = 0.9) +
    labs(
      title = paste("Mean Participant Payoff -", bt),
      x = "Belief Category", y = "Mean Payoff", fill = "Belief Category"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 14),
      legend.position = "none"
    )
  
  ggsave(paste0(path_github, "Outputs/payoffs_by_belief_", bt, ".png"), plot = p, width = 10, height = 8)
}



