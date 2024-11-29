###########################
### Data Analysis
############################
library(stargazer)




rm(list=ls())
path_github <-"C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos<-"C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

setwd(path_github)


load(paste0(path_datos, "/Datos_islitas.Rdata"))

#################################################
################# Subsets #######################
#################################################

# T1
rounds <- 1:8  # Sequence from 1 to 10
treats <- c("T1")  # Time periods T1 and T2
vars <- c("amerb", "libre")  # Extraction types

# Generate the list of variables using expand.grid to generate all combinations
combinations <- expand.grid(treats, rounds, vars)

# Create the final object with formatted strings
variable_names1 <- paste0(
  "", combinations$Var1, "juegoalgas.", combinations$Var2, 
  ".player.", combinations$Var1, "_extraccion_", combinations$Var3
)


# T2"
rounds <- 1:8  # Sequence from 1 to 10
treats <- c("T2")  # Time periods T1 and T2
vars <- c("amerb", "metat")  # Extraction types

# Generate the list of variables using expand.grid to generate all combinations
combinations <- expand.grid(treats, rounds, vars)

# Create the final object with formatted strings
variable_names2 <- paste0(
  "", combinations$Var1, "juegoalgas.", combinations$Var2, 
  ".player.", combinations$Var1, "_extraccion_", combinations$Var3
)
# Print or use the variable_names
variable_names<-c(variable_names1, variable_names2, "gid.amerb", "gid.treat")


##### Treatment variables subset
dfs<-(df[, variable_names])


#### Long data frame all observations
dfs_long <- dfs %>%
  pivot_longer(
    cols = starts_with("T"),   # All columns starting with "T" (T1 and T2 variables)
    names_to = c("treatment", "round", "area"),  # Split the names into three parts
    names_pattern = "(T\\d)juegoalgas\\.(\\d+)\\.player\\..+_extraccion_(.+)",  # Regex to extract treatment, round, and variable
    values_to = "extraction"   # Name of the column for values
  ) %>%
  mutate(round = as.integer(round))  # Ensure round is numeric


# Mean by treatment and area, for each round for diff-in-diff 

rm <- dfs_long %>%
  group_by(treatment, area, round) %>%  # Group by treatment and variable
  summarise(mean_extraction = mean(extraction, na.rm = TRUE))  # Calculate mean and handle missing values


#Belief Columns 
belief_columns <- grep("belief", colnames(df), value = TRUE, ignore.case = TRUE)
# Now filter to keep only the ones that end in "_ini" or "_fin"
filtered_belief_columns <- grep("_ini$|_fin$|id", belief_columns, value = TRUE, ignore.case = TRUE)

# subset a dataframe of beliefs 
print(filtered_belief_columns)
df.b<-df[, c("participant.code","gid.treat", "gid.amerb", filtered_belief_columns )]

#################################################
############### Data Analysis ###################
#################################################

#Descriptive statistics
20000+mean(df$participant.payoff, na.rm=T)
summary(df$participant.payoff, na.rm=T)


table(df$T2juegoalgas.1.player.T2_grupo_mixto)
table(df$participant.grupo_amerb)


#Trust
table(df$survey1.1.player.confianza_caleta)
table(df$survey1.1.player.confianza_pm)
table(df$survey2.1.player.confianza_caleta_conocida1)
table(df$survey2.1.player.confianza_caleta_conocida2)

#Conflict
table(df$survey1.1.player.conflicto_caleta)
table(df$survey1.1.player.conflicto_pm)
table(df$survey2.1.player.conflicto_caleta_conocida1)
table(df$survey2.1.player.conflicto_caleta_conocida2)


#############
### Graphs
##############


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

###########################################
### Plots Beliefs and Mean Over-extraction
##########################################

##### Plots for mean beliefs 
plot1 <- ggplot(beliefs_stats, aes(x = Area, y = mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +  # Bar plot
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    position = position_dodge(width = 0.8),
    width = 0.25
  ) +  # Error bars
  labs(
    #title = "Mean Beliefs about others",
    x = "",
    y = "Mean beliefs of over-extraction by others",
    fill = "Treatment"
  ) +
  scale_y_continuous(limits = c(0, 50)) +  # Set y-axis limits
  scale_x_discrete(labels = c(
    "ingroup_amerb" = "Ingroup in Amerb",
    "ingroup_OA" = "Ingroup in Other Area",
    "others_OA" = "Strangers in Other Area"
  )) +  # Custom x-axis labels
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Plot for initial beliefs  
plot2 <- ggplot(beliefs_stats_ini, aes(x = Area, y = mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +  # Bar plot
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    position = position_dodge(width = 0.8),
    width = 0.25
  ) +  # Error bars
  labs(
    #title = "Initial Beliefs about Others",
    x = "",
    y = "Round 0 beliefs of over-extraction by others",
    fill = "Treatment"
  ) +
  scale_y_continuous(limits = c(0, 50)) +  # Set y-axis limits
  scale_x_discrete(labels = c(
    "ingroup_amerb" = "Ingroup in Amerb",
    "ingroup_OA" = "Ingroup in Other Area",
    "others_OA" = "Strangers in Other Area"
  )) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Plot for personal extraction (mean rounds 1-8)
plot3 <- ggplot(extraction_stats, aes(x = area, y = mean_extraction, fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    position = position_dodge(width = 0.8),
    width = 0.25
  ) +
  scale_y_continuous(limits = c(0, 50)) +
  scale_x_discrete(labels = c(
    "amerb" = "Person in Amerb",
    "libre" = "Person in Open Acces (T1)",
    "metat" = "Person in Metaturf (T2)"
  )) +
  #scale_fill_manual(values = c("#1b9e77", "#d95f02")) +  # Custom colors for treatments
  labs(
    #title = "Personal Over-extraction",
    x = "",
    y = "Mean personal Over-extraction",
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot4<- ggplot(OE_o_stats, aes(x = area, y = mean_extraction_o, fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +  # Bar plot
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    position = position_dodge(width = 0.8),
    width = 0.25
  ) +  # Error bars for 95% CI
  scale_y_continuous(limits = c(0, 50)) +  # Set y-axis range to 0-50
  scale_x_discrete(labels = c(
    "otros_amerb" = "Ingroup in Amerb",
    "otros_libre" = "Hetero. group in Open Access",
    "otros_metat" = "Hetero. group in Metaturf"
  )) +
  labs(
    #title = "Over-extraction by others",
    x = "",
    y = "Mean Extraction Others in Same Group",
    fill = "Treatment"
  ) +
  theme_minimal() +  # Clean theme
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )



# Combine the two plots vertically
combined_plot1 <- plot1 + plot4 + plot3# Stack the plots using patchwork
combined_plot2 <- plot2 + plot4 + plot3 # Stack the plots using patchwork

# Display the combined plot
print(combined_plot1)
print(combined_plot2)


ggsave( file=paste0(path_github, "Outputs/Plot_beliefs_extraction.pdf") , plot = combined_plot2, device = "pdf", width = 12, height = 6)








#################################################
#### Diff between T2 and T1, per extraction area
#################################################
rm_wide <- rm %>%
  mutate(area = ifelse(area %in% c("metat", "libre"), "other_area", "amerb")) %>%  # Update area values
  pivot_wider(
    names_from = treatment,  # Create columns based on treatment
    values_from = mean_extraction  # Populate with mean_extraction values
  ) %>%
  mutate(diff = T2 - T1)  # Add the difference between T2 and T1


pdid<-ggplot(rm_wide, aes(x = round, y = diff, color = area, group = area)) +
  geom_line() +  # Line plot for each extraction type
  geom_point() +  # Add points to indicate the data
  labs(
    title = "Difference between T2 and T1 per Round",
    x = "Round",
    y = "Difference (T2 - T1)",
    color = "Extraction Area"
  ) +
  scale_x_continuous(breaks = 1:10) + 
  theme_minimal() +  # Use a minimal theme for better visualization
  theme(legend.position = "top") 
pdid
ggsave( file=paste0(path_github, "Outputs/plot_difference_T2_T1.pdf") , plot = pdid, device = "pdf", width = 8, height = 6)




##################################################
### Diff in Amerb - Other area per treatment
##################################################

diff_area <- rm %>%
  pivot_wider(
    names_from = area,        # Spread area (libre, metat, amerb) into separate columns
    values_from = mean_extraction  # The mean extraction values
  ) %>%
  arrange(round) %>%
  mutate(otra_zona = coalesce(libre, metat),
         diff = otra_zona - amerb)  # Calculate the difference between T1 and T2

# Step 3: Plot the difference per round for amerb and libre
pdiff<-ggplot(diff_area , aes(x = round, y = diff, color = treatment, group = treatment)) +
  geom_line() +  # Line plot for each extraction type
  geom_point() +  # Add points to indicate the data
  labs(
    title = "In and out-group bias ",
    x = "Round",
    y = "Difference (Otra zona- Amerb)",
    color = "Tratamiento"
  ) +
  scale_x_continuous(breaks = 1:10) + 
  theme_minimal() +  # Use a minimal theme for better visualization
  theme(legend.position = "top") 
pdiff

ggsave( file=paste0(path_github, "Outputs/plot_difference_amerb_otrazona.pdf") , plot = pdiff, device = "pdf", width = 8, height = 6)


###########################################
### Beliefs
###########################################

#Scatterplot of extraction given beliefs in AMERB
ba<-ggplot(df, aes(x = beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini, 
                   y = T1juegoalgas.1.player.T1_extraccion_amerb)) +
  geom_point() +  # Scatter plot points
  geom_smooth(method = "lm", se = TRUE) +  # Adds lm line with confidence intervals
  labs(
    title = "Extraction AMERB given Beliefs - Ingroup",
    x = "Beliefs Ingroup in Amerb - T1 Round 0",
    y = "Player Extraction Amerb - T1 Round 1"
  ) +
  theme_minimal()
ba

lm<-lm(T1juegoalgas.1.player.T1_extraccion_amerb ~ beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini, data=df)
summary(lm)

ggsave( file=paste0(path_github, "Outputs/Extraction_Amerb_Beliefs.pdf") , plot = ba, device = "pdf", width = 8, height = 6)


#Scatterplot of extraction given beliefs in open area

df_combined <- rbind(
  data.frame(
    Beliefs = df$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini,
    Extraction = df$T1juegoalgas.1.player.T1_extraccion_libre,
    Type = "Ingroup"
  ),
  data.frame(
    Beliefs = df$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini,
    Extraction = df$T1juegoalgas.1.player.T1_extraccion_libre,
    Type = "Outgroup"
  )
)

# Scatter plot with different color and shape for each belief type
boa<-ggplot(df_combined, aes(x = Beliefs, y = Extraction, color = Type, shape = Type)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +  # Adds lm line with confidence intervals for each type
  labs(
    title = "Extraction Open Access given Ingroup and Outgroup Beliefs",
    x = "Beliefs in Libre - Ingroup and Outgroup",
    y = "Player Extraction Libre"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Ingroup" = "blue", "Outgroup" = "red")) +  # Optional color customization
  scale_shape_manual(values = c("Ingroup" = 16, "Outgroup" = 17))  # Optional shape customization
boa

lm1<-lm(T1juegoalgas.1.player.T1_extraccion_libre ~ beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini, data=df)
summary(lm1)

lm2<-lm(T1juegoalgas.1.player.T1_extraccion_libre ~ beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini, data=df)
summary(lm2)

lm3<-lm(T1juegoalgas.1.player.T1_extraccion_libre ~ beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini+ beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini , data=df)
summary(lm3)


ggsave( file=paste0(path_github, "Outputs/Extraction_OA_Beliefs.pdf") , plot = boa, device = "pdf", width = 8, height = 6)


########################################
#### Means per group
########################################

#### means per groups in each
### var selection
rounds <- 1:8  # Sequence from 1 to 10
treats <- c("T1")  # Time periods T1 and T2
vars <- c("amerb")  # Extraction types

# Generate the list of variables using expand.grid to generate all combinations
combinations <- expand.grid(treats, rounds, vars)
vars <- paste0(
  "", combinations$Var1, "juegoalgas.", combinations$Var2, 
  ".player.", combinations$Var1, "_extraccion_", combinations$Var3
)



# Function: calculate means for each variable by group
calculate_means_by_group <- function(df, group_var, variable_names) {
  means_list <- list()
  
  for (var in variable_names) {
    if (var %in% colnames(df)) {
      # Calculate the mean by group for the current variable
      means <- df %>%
        group_by(!!sym(group_var)) %>%
        summarise(mean_extraction = mean(!!sym(var), na.rm = TRUE)) %>%
        mutate(variable = var)
      
      # Append the result to the list
      means_list[[var]] <- means
    } else {
      warning(paste("Variable", var, "not found in the data frame."))
    }
  }
  
  # Combine all the results into one data frame
  result <- bind_rows(means_list)
  return(result)
}

# Run the function to calculate the means
gid.means.amerb.t1 <- calculate_means_by_group(df, "gid.amerb", vars)

gid.means.amerb.t1$round <- str_extract(gid.means.amerb.t1$variable, "(?<=\\.)\\d+(?=\\.)")

# Mean payoff per round T1 amerb
gid.means.amerb.t1$round <- as.numeric(gid.means.amerb.t1$round)

# Create the line plot 1
p1<-ggplot(gid.means.amerb.t1, aes(x = round, y = mean_extraction, color = as.factor(gid.amerb), group = gid.amerb)) +
  geom_line() +  # Line for each gid.amerb group 
  scale_y_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 50)) + 
  geom_point() +  # Optional: Add points on the lines for clarity
  labs(
    title = "T1 AMERB",
    x = "Round",
    y = "Mean extraction by group amerb T1",
    color = "Group (gid.amerb)"
  ) +
  theme_minimal() +  # Use a clean theme for better visualization
  theme(legend.position = "none")
p1



#### Graph amerb t2

rounds <- 1:10  # Sequence from 1 to 10
treats <- c("T2")  # Time periods T1 and T2
vars <- c("amerb")  # Extraction types

# Generate the list of variables using expand.grid to generate all combinations
combinations <- expand.grid(treats, rounds, vars)
vars <- paste0(
  "", combinations$Var1, "juegoalgas.", combinations$Var2, 
  ".player.", combinations$Var1, "_extraccion_", combinations$Var3
)

gid.means.amerb.t2 <- calculate_means_by_group(df, "gid.amerb", vars)

gid.means.amerb.t2$round <- str_extract(gid.means.amerb.t2$variable, "(?<=\\.)\\d+(?=\\.)")

gid.means.amerb.t2$round <- as.numeric(gid.means.amerb.t2$round)

# Create the line plot 1
p2<-ggplot(gid.means.amerb.t2, aes(x = round, y = mean_extraction, color = as.factor(gid.amerb), group = gid.amerb)) +
  geom_line() +  # Line for each gid.amerb group
  scale_y_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 50)) + 
  geom_point() +  # Optional: Add points on the lines for clarity
  labs(
    title = "T2 - AMERB",
    x = "Round",
    y = "Mean extraction by group amerb T2",
    color = "Group (gid.amerb)"
  ) +
  theme_minimal()  # Use a clean theme for better visualization

p2

grid.arrange(p1, p2, ncol = 2)



#### means per groups in each
### var selection
rounds <- 1:10  # Sequence from 1 to 10
treats <- c("T1")  # Time periods T1 and T2
vars <- c("libre")  # Extraction types

# Generate the list of variables using expand.grid to generate all combinations
combinations <- expand.grid(treats, rounds, vars)
vars <- paste0(
  "", combinations$Var1, "juegoalgas.", combinations$Var2, 
  ".player.", combinations$Var1, "_extraccion_", combinations$Var3
)


# Run the function to calculate the means
gid.means.zl.t1 <- calculate_means_by_group(df, "participant.zonaT2", vars)

gid.means.zl.t1$round <- str_extract(gid.means.zl.t1$variable, "(?<=\\.)\\d+(?=\\.)")

# Mean payoff per round T1 zl
gid.means.zl.t1$round <- as.numeric(gid.means.zl.t1$round)

# Create the line plot 1
p3<-ggplot(gid.means.zl.t1, aes(x = round, y = mean_extraction, color = as.factor(participant.zonaT2), group = participant.zonaT2)) +
  geom_line() +  # Line for each gid.zl group 
  scale_y_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 50)) + 
  geom_point() +  # Optional: Add points on the lines for clarity
  labs(
    title = "T1 Zona Libre",
    x = "Round",
    y = "Mean extraction by group zona libre T1",
    color = "Group (gid.zl)"
  ) +
  theme_minimal() + # Use a clean theme for better visualization
  theme(legend.position = "none")
p3



#### Graph amerb t2

rounds <- 1:10  # Sequence from 1 to 10
treats <- c("T2")  # Time periods T1 and T2
vars <- c("metat")  # Extraction types

# Generate the list of variables using expand.grid to generate all combinations
combinations <- expand.grid(treats, rounds, vars)
vars <- paste0(
  "", combinations$Var1, "juegoalgas.", combinations$Var2, 
  ".player.", combinations$Var1, "_extraccion_", combinations$Var3
)

gid.means.zl.t2 <- calculate_means_by_group(df, "participant.zonaT2", vars)

gid.means.zl.t2$round <- str_extract(gid.means.zl.t2$variable, "(?<=\\.)\\d+(?=\\.)")

gid.means.zl.t2$round <- as.numeric(gid.means.zl.t2$round)

# Create the line plot 1
p4<-ggplot(gid.means.zl.t2, aes(x = round, y = mean_extraction, color = as.factor(participant.zonaT2), group = participant.zonaT2)) +
  geom_line() +  # Line for each gid.amerb group
  scale_y_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 50)) +
  geom_point() +  # Optional: Add points on the lines for clarity
  labs(
    title = "T2 - Meta-Turf",
    x = "Round",
    y = "Mean extraction by group Meta-Turf T2",
    color = "Group (gid.amerb)"
  ) +
  theme_minimal()  # Use a clean theme for better visualization

p4

grid.arrange(p1, p2, p3, p4, ncol = 2, nrow= 2)

ggsave( file=paste0(path_github, "outputs/group_means.pdf") , plot = p4, device = "pdf", width = 8, height = 6)




