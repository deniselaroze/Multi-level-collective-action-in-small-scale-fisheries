##########################
### Code for presentation
##########################
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




#### Color pallet

#Dark Purple (#440154)
#  Purple-Blue (#482777)
#    Blue (#3F4A8A)
#      Teal-Blue (#31688E)
#        Green-Teal (#26828E)
#          Light Green (#1F9D8A)
#            Yellow-Green (#6CCE5A)
#              Bright Yellow (#FDE725)



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


# Adjust bar position by grouping variable first
p <- ggplot(means_ci_by_treatment, aes(x = variable, y = mean, fill = treatment)) +
  
  # Bar plot
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), alpha = 1) +
  
  # Error bars
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(0.8), width = 0.2) +
  
  # Add mean values as text labels above bars
  geom_text(aes(label = round(mean, 2)), 
            position = position_dodge(0.8), 
            vjust = -1.3, 
            size = 5) +
  
  # Custom colors (Grouping by treatment)
  scale_fill_manual(values = c("Unknown Outsiders" = "#3F4A8A", "Known Outsiders" ="#6CCE5A"), 
                    labels = c("Unknown Outsiders", "Known Outsiders")) +
  
  # Labels
  labs(title = "",
       y = "Compliance",
       x = "Management System",
       fill = "Scenario") +
  
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

# Print the updated plot
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
  scale_color_manual(values = c("T1" = "#3F4A8A", "T2" = "#6CCE5A"), 
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
  scale_color_manual(values = c("T1" = "#3F4A8A", "T2" = "#6CCE5A"), 
                     labels = c("T1" = "Unknown Outsiders", "T2" = "Known Outsiders")) +
  
  # Labels
  labs(title = "Compliance in Open Access Area",
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
          se = list(clustered_se_lm2, clustered_se_lm3),
          type = "html",
          out = paste0(path_github, "Outputs/compliance_extraction_AMERB_controls_gid_clustered_se.html"),
          title = "Regression Results with Clustered Standard Errors",
          dep.var.labels = "Proportion Compliance TURF",
          covariate.labels = c("Mean Compliance by Group (t-1)", "Beliefs Compliance by Ingroup in TURF", 
                               "Scenario Know Outsiders (dummy)", "Sessions with 3 Unions (Dummy)" , "Female (Dummy)",
                               "Fishing h/w", "Level of Education" ,"Held Leadership Role (Dummy)", "Constant" ),
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
                               "Fishing h/w", "Level of Education" ,"Held Leadership Role (Dummy)", "Constant" ),
          notes = "OLS regressions with session matching group clustered s.e. in parentheses.")




##################################
### SEM in OA Scenario 1 Round 1
#################################


# Set R to 1
R <- 1

# Subset variables dynamically
variable_subset <- df[, grep("^T1juegoalgas\\.\\d+\\.player\\.T1_extraccion_libre$", names(df))]
variable_subset <- variable_subset[, R, drop = FALSE]  # Select only the first round

# Calculate the row-wise mean for the selected column
df$average_extraction_ini <- rowMeans(variable_subset, na.rm = TRUE)
df$average_compliance_ini <- 1 - (df$average_extraction_ini / 50)

# Belief compliance
df$belief_compliance_pm <- 1 - (df$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50)
df$belief_compliance_union <- 1 - (df$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50)

# Define the SEM model
sem_model <- '
  # Relationships for beliefs
  belief_compliance_pm  ~  survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm
  belief_compliance_union   ~  survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta

  # Relationship for extraction
  average_compliance_ini ~ belief_compliance_pm + belief_compliance_union +   
  survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm + 
  survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta 
  '

node_labels <- c(
  belief_compliance_pm = "Beliefs Compl. Others",
  belief_compliance_union = "Beliefs Compl. Union",
  average_compliance_ini = "Compliance",
  survey1.1.player.confianza_pm = "Trust Others",
  survey1.1.player.conflicto_pm = "Conflict Others",
  survey1.1.player.confianza_caleta = "Trust Union",
  survey1.1.player.conflicto_caleta = "Conflict Union"
)

# Fit the SEM model
fit <- sem(sem_model, data = df)

# Extract path results and edge colors dynamically
path_results <- parameterEstimates(fit, standardized = TRUE)
regression_paths <- path_results[path_results$op == "~", ]
param_labels <- paste(regression_paths$lhs, regression_paths$op, regression_paths$rhs, sep = " ")
edge_colors_map <- setNames(ifelse(regression_paths$pvalue < 0.05, "black", "red"), param_labels)
edge_colors <- ifelse(regression_paths$pvalue < 0.05, "black", "red")

# Ensure edge_colors matches the number of paths
n_edges <- length(regression_paths$lhs)
if (length(edge_colors) < n_edges) {
  edge_colors <- c(edge_colors, rep("gray", n_edges - length(edge_colors)))
}

# Save SEM plot with a dynamic filename
output_file <- paste0(path_github, "Outputs/SEM_compliance_OA_T1_plot_Round_1.png")
png(output_file, width = 1300, height = 800, res = 300)

# Generate SEM plot
semPaths(
  fit,
  what = "std",
  layout = "tree",
  edge.label.cex = 1,
  nodeLabels = node_labels,
  sizeMan = 9,
  label.cex = 1,
  node.width = 2, 
  node.height = 0.7,
  shapeMan = "ellipse", 
  edge.color = edge_colors,
  residuals = FALSE,
  intercepts = FALSE,
  optimizeLatRes = TRUE,
  fade = FALSE
)

# Add title
title(main = "Mean Compliance Scenario 1 Open Access Round 1", line = 3.5, cex.main = 0.8)

# Close the PDF device
dev.off()


########################################
#### SEM in OA Scenario 1 Rounds (5:8)
########################################

R<-5 # From which round to start

# Loop through N from 1 to 8
for (N in (R+1):8) {
  # Subset variables dynamically
  variable_subset <- df[, grep("^T1juegoalgas\\.\\d+\\.player\\.T1_extraccion_libre$", names(df))]
  variable_subset <- variable_subset[, R:N]  # Select columns for current N
  
  # Calculate the row-wise mean for selected columns
  df$average_extraction_ini <- rowMeans(variable_subset, na.rm = TRUE)
  df$average_compliance_ini <- 1 - (df$average_extraction_ini / 50)
  
  # Belief compliance
  df$belief_compliance_pm <- 1 - (df$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50)
  df$belief_compliance_union <- 1 - (df$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50)
  
  # Observed compliance
  variable_subset <- df[, grep("T1juegoalgas\\.(\\d+)\\.player\\..+_extraccion_otros_libre$", names(df))]
  variable_subset <- variable_subset[, R:N-1]
  df$average_extraction_observed_ini <- rowMeans(variable_subset, na.rm = TRUE)
  df$average_compliance_observed_ini_lag <- 1 - (df$average_extraction_observed_ini / 150)
} 
  
  
  # Specify the SEM model
sem_model <- '
  # Relationships for beliefs
  belief_compliance_pm  ~  survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm
  belief_compliance_union   ~  survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
  
  # Relationship for extraction
  average_compliance_ini ~ belief_compliance_pm + belief_compliance_union +   
  survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm + survey1.1.player.confianza_caleta + 
  survey1.1.player.conflicto_caleta  

  # Ensure no direct effect from average_compliance_observed_ini_lag to restricted variables
  average_compliance_ini ~ average_compliance_observed_ini_lag

  # Restrict correlations between average_compliance_observed_ini_lag and other variables
  average_compliance_observed_ini_lag ~~ 0*belief_compliance_union
   average_compliance_observed_ini_lag ~~ 0*belief_compliance_pm
  #average_compliance_observed_ini_lag ~~ 0*survey1.1.player.confianza_pm
  #average_compliance_observed_ini_lag ~~ 0*survey1.1.player.conflicto_pm
  #average_compliance_observed_ini_lag ~~ 0*survey1.1.player.confianza_caleta
  #average_compliance_observed_ini_lag ~~ 0*survey1.1.player.conflicto_caleta
'

  #name variable so that there are comprensible
  node_labels <- c(belief_compliance_pm =   "Beliefs Compl. Others" ,
                   belief_compliance_union =   "Beliefs Compl. Union", 
                   average_compliance_ini = "Compliance" ,
                   survey1.1.player.confianza_pm  =   "Trust Others" ,
                   survey1.1.player.conflicto_pm =   "Conflict Others" ,
                   survey1.1.player.confianza_caleta =   "Trust Union", 
                   survey1.1.player.conflicto_caleta =   "Conflict Union",
                   average_compliance_observed_ini_lag = "Observed Compliance (t-1)"
  )
  
  
  # Fit the SEM model
  fit <- sem(sem_model, data = df)
  
  # Extract path results and edge colors dynamically
  path_results <- parameterEstimates(fit, standardized = TRUE)
  regression_paths <- path_results[path_results$op == "~", ]
  param_labels <- paste(regression_paths$lhs, regression_paths$op, regression_paths$rhs, sep = " ")
  edge_colors_map <- setNames(ifelse(regression_paths$pvalue < 0.05, "black", "red"), param_labels)
  edge_colors <- ifelse(regression_paths$pvalue < 0.05, "black", "red")
  
  # Ensure edge_colors matches the number of paths
  n_edges <- length(regression_paths$lhs)
  if (length(edge_colors) < n_edges) {
    edge_colors <- c(edge_colors, rep("gray", n_edges - length(edge_colors)))
  }
  
  # Save each SEM plot with a dynamic filename
  output_file <- paste0(path_github, "Outputs/SEM_compliance_OA_T1_plot_Rounds_", R, "_to_", N, ".png")
  png(output_file, width = 1300, height = 800, res = 300)
  
  # Generate SEM plot
  semPaths(
    fit,
    what = "std",
    layout = "tree",
    edge.label.cex = 1,
    nodeLabels = node_labels,
    sizeMan = 9,
    label.cex = 1,
    node.width = 2, 
    node.height = 0.7,
    shapeMan = "ellipse", 
    edge.color = edge_colors,
    residuals = FALSE,
    intercepts = FALSE,
    optimizeLatRes = TRUE,
    fade = FALSE
  )
  
  # Add dynamic title
  title(main = paste("Mean Compliance Scenario 1 Open Access Rounds ", R ," to ", N),line = 3.5, cex.main = 0.8)
  
  # Close the PDF device
  dev.off()


# End of script

########################################
### SEM in TURF Scenario 1 Round 1
########################################
  
  
  
  
  # Set R to 1
  R <- 1
  
  # Subset variables dynamically
  variable_subset <- df[, grep("^T1juegoalgas\\.\\d+\\.player\\.T1_extraccion_amerb$", names(df))]
  variable_subset <- variable_subset[, R, drop = FALSE]  # Select only the first round
  
  # Calculate the row-wise mean for the selected column
  df$average_extraction_ini <- rowMeans(variable_subset, na.rm = TRUE)
  df$average_compliance_ini <- 1 - (df$average_extraction_ini / 50)
  
  # Belief compliance
  df$belief_compliance_amerb <- 1 - (df$beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini / 50)
  
  # Define the SEM model
  sem_model <- '
  # Relationships for beliefs
  belief_compliance_amerb  ~  survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
  

  # Relationship for extraction
  average_compliance_ini ~ belief_compliance_amerb +  survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta 
  '
  
  node_labels <- c(
    belief_compliance_amerb = "Beliefs Compl. Union",
    average_compliance_ini = "Compliance",
    survey1.1.player.confianza_caleta = "Trust Union",
    survey1.1.player.conflicto_caleta = "Conflict Union"
  )
  
  # Fit the SEM model
  fit <- sem(sem_model, data = df)
  
  # Extract path results and edge colors dynamically
  path_results <- parameterEstimates(fit, standardized = TRUE)
  regression_paths <- path_results[path_results$op == "~", ]
  param_labels <- paste(regression_paths$lhs, regression_paths$op, regression_paths$rhs, sep = " ")
  edge_colors_map <- setNames(ifelse(regression_paths$pvalue < 0.05, "black", "red"), param_labels)
  edge_colors <- ifelse(regression_paths$pvalue < 0.05, "black", "red")
  
  # Ensure edge_colors matches the number of paths
  n_edges <- length(regression_paths$lhs)
  if (length(edge_colors) < n_edges) {
    edge_colors <- c(edge_colors, rep("gray", n_edges - length(edge_colors)))
  }
  
  # Save SEM plot with a dynamic filename
  output_file <- paste0(path_github, "Outputs/SEM_compliance_amerb_T1_plot_Round_1.png")
  png(output_file, width = 1300, height = 800, res = 300)
  
  # Generate SEM plot
  semPaths(
    fit,
    what = "std",
    layout = "tree",
    edge.label.cex = 1,
    nodeLabels = node_labels,
    sizeMan = 9,
    label.cex = 1,
    node.width = 2, 
    node.height = 0.7,
    shapeMan = "ellipse", 
    edge.color = edge_colors,
    residuals = FALSE,
    intercepts = FALSE,
    optimizeLatRes = TRUE,
    fade = FALSE
  )
  
  # Add title
  title(main = "Mean Compliance TURF Round 1", line = 3.5, cex.main = 0.8)
  
  # Close the PDF device
  dev.off()
  
  
#########################
### SEM in TURF Scenario 1 rounds (5:8)
#########################
  
  
  
  # Set R (starting round)
  R <- 5
  
  # Loop through N from (R+1) to 8
  for (N in (R+1):8) {
    
    # Subset variables dynamically for amerb extraction
    variable_subset <- df[, grep("^T1juegoalgas\\.\\d+\\.player\\.T1_extraccion_amerb$", names(df))]
    variable_subset <- variable_subset[, R:N]  # Select columns for current N
    
    # Calculate the row-wise mean for selected columns
    df$average_extraction_ini <- rowMeans(variable_subset, na.rm = TRUE)
    df$average_compliance_ini <- 1 - (df$average_extraction_ini / 50)
    
    # Belief compliance for amerb
    df$belief_compliance_amerb <- 1 - (df$beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini / 50)
    
    
    # Observed compliance
    variable_subset <- df[, grep("T1juegoalgas\\.(\\d+)\\.player\\..+_extraccion_otros_amerb$", names(df))]
    variable_subset <- variable_subset[, R:N-1]
    df$average_extraction_observed_ini <- rowMeans(variable_subset, na.rm = TRUE)
    df$average_compliance_observed_ini_lag <- 1 - (df$average_extraction_observed_ini / 150)
  } 
  
  # Define the SEM model for amerb
  sem_model <- '
  # Relationships for beliefs
  belief_compliance_amerb  ~  survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
  
  # Relationship for extraction
  average_compliance_ini ~ belief_compliance_amerb +  
  survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta

 # Ensure no direct effect from average_compliance_observed_ini_lag to restricted variables
  average_compliance_ini ~ average_compliance_observed_ini_lag

  # Restrict correlations between average_compliance_observed_ini_lag and other variables
  average_compliance_observed_ini_lag ~~ 0*belief_compliance_amerb
   '
  
  # Node labels
  node_labels <- c(
    belief_compliance_amerb = "Beliefs Compl. Union",
    average_compliance_ini = "Compliance",
    survey1.1.player.confianza_caleta = "Trust Union",
    survey1.1.player.conflicto_caleta = "Conflict Union",
    average_compliance_observed_ini_lag = "Observed Compliance (t-1)"
  )
  
  # Fit the SEM model
  fit <- sem(sem_model, data = df)
  
  # Extract path results and edge colors dynamically
  path_results <- parameterEstimates(fit, standardized = TRUE)
  regression_paths <- path_results[path_results$op == "~", ]
  param_labels <- paste(regression_paths$lhs, regression_paths$op, regression_paths$rhs, sep = " ")
  edge_colors_map <- setNames(ifelse(regression_paths$pvalue < 0.05, "black", "red"), param_labels)
  edge_colors <- ifelse(regression_paths$pvalue < 0.05, "black", "red")
  
  # Ensure edge_colors matches the number of paths
  n_edges <- length(regression_paths$lhs)
  if (length(edge_colors) < n_edges) {
    edge_colors <- c(edge_colors, rep("gray", n_edges - length(edge_colors)))
  }
  
  # Save SEM plot with a dynamic filename
  output_file <- paste0(path_github, "Outputs/SEM_compliance_amerb_T1_plot_Rounds_", R, "_to_", N, ".png")
  png(output_file, width = 1300, height = 800, res = 300)
  
  # Generate SEM plot
  semPaths(
    fit,
    what = "std",
    layout = "tree",
    edge.label.cex = 1,
    nodeLabels = node_labels,
    sizeMan = 9,
    label.cex = 1,
    node.width = 2, 
    node.height = 0.7,
    shapeMan = "ellipse", 
    edge.color = edge_colors,
    residuals = FALSE,
    intercepts = FALSE,
    optimizeLatRes = TRUE,
    fade = FALSE
  )
  
  # Add dynamic title
  title(main = paste("Mean Compliance Scenario 1 TURF Rounds  ", R ," to ", N), line = 3.5, cex.main = 0.8)
  
  # Close the P
  # Close the PDF device
  dev.off()
  
  
  
  
  










  
##################################
### SEM in OA Scenario 2 Round 1
##################################
  
  
  # Set R to 1
  R <- 1
  
  # Subset variables dynamically
  variable_subset <- df[, grep("^T2juegoalgas\\.\\d+\\.player\\.T2_extraccion_metat$", names(df))]
  variable_subset <- variable_subset[, R, drop = FALSE]  # Select only the first round
  
  # Calculate the row-wise mean for the selected column
  df$average_extraction_ini <- rowMeans(variable_subset, na.rm = TRUE)
  df$average_compliance_ini <- 1 - (df$average_extraction_ini / 50)
  
  # Belief compliance
  df$belief_compliance_pm <- 1 - (df$beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini / 50)
  df$belief_compliance_union <- 1 - (df$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50)
  
  # Define the SEM model
  sem_model <- '
  # Relationships for beliefs
  belief_compliance_pm  ~  survey2.1.player.confianza_caleta_conocida_mean + survey2.1.player.conflicto_caleta_conocida_mean
  belief_compliance_union   ~  survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta

  # Relationship for extraction
  average_compliance_ini ~ belief_compliance_pm + belief_compliance_union +   
  survey2.1.player.confianza_caleta_conocida_mean + survey2.1.player.conflicto_caleta_conocida_mean +
  survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta 
  '
  
  node_labels <- c(
    belief_compliance_pm = "Beliefs Compl. Others",
    belief_compliance_union = "Beliefs Compl. Union",
    average_compliance_ini = "Compliance",
    survey2.1.player.confianza_caleta_conocida_mean = "Trust Others (mean)",
    survey2.1.player.conflicto_caleta_conocida_mean = "Conflict Others (mean)",
    survey1.1.player.confianza_caleta = "Trust Union",
    survey1.1.player.conflicto_caleta = "Conflict Union"
  )
  
  # Fit the SEM model
  fit <- sem(sem_model, data = df)
  
  # Extract path results and edge colors dynamically
  path_results <- parameterEstimates(fit, standardized = TRUE)
  regression_paths <- path_results[path_results$op == "~", ]
  param_labels <- paste(regression_paths$lhs, regression_paths$op, regression_paths$rhs, sep = " ")
  edge_colors_map <- setNames(ifelse(regression_paths$pvalue < 0.05, "black", "red"), param_labels)
  edge_colors <- ifelse(regression_paths$pvalue < 0.05, "black", "red")
  
  # Ensure edge_colors matches the number of paths
  n_edges <- length(regression_paths$lhs)
  if (length(edge_colors) < n_edges) {
    edge_colors <- c(edge_colors, rep("gray", n_edges - length(edge_colors)))
  }
  
  # Save SEM plot with a dynamic filename
  output_file <- paste0(path_github, "Outputs/SEM_compliance_OA_T2_plot_Round_1.png")
  png(output_file, width = 1300, height = 800, res = 300)
  
  # Generate SEM plot
  semPaths(
    fit,
    what = "std",
    layout = "tree",
    edge.label.cex = 1,
    nodeLabels = node_labels,
    sizeMan = 9,
    label.cex = 1,
    node.width = 2, 
    node.height = 0.7,
    shapeMan = "ellipse", 
    edge.color = edge_colors,
    residuals = FALSE,
    intercepts = FALSE,
    optimizeLatRes = TRUE,
    fade = FALSE
  )
  
  # Add title
  title(main = "Mean Compliance Scenario 2 Meta-TURF Round 1", line = 3.5, cex.main = 0.8)
  
  # Close the PDF device
  dev.off()
  
  
########################################
#### SEM in OA Scenario 2 Rounds (5:8)
########################################
  
  R<-5 # From which round to start
  
  # Loop through N from 1 to 8
  for (N in (R+1):8) {
    # Subset variables dynamically
    variable_subset <- df[, grep("^T2juegoalgas\\.\\d+\\.player\\.T2_extraccion_metat$", names(df))]
    variable_subset <- variable_subset[, R:N]  # Select columns for current N
    
    # Calculate the row-wise mean for selected columns
    df$average_extraction_ini <- rowMeans(variable_subset, na.rm = TRUE)
    df$average_compliance_ini <- 1 - (df$average_extraction_ini / 50)
    
    # Belief compliance
    df$belief_compliance_pm <- 1 - (df$beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini / 50)
    df$belief_compliance_union <- 1 - (df$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50)
    
    # Observed compliance
    variable_subset <- df[, grep("T2juegoalgas\\.(\\d+)\\.player\\..+_extraccion_otros_metat$", names(df))]
    variable_subset <- variable_subset[, R:N-1]
    df$average_extraction_observed_ini <- rowMeans(variable_subset, na.rm = TRUE)
    df$average_compliance_observed_ini_lag <- 1 - (df$average_extraction_observed_ini / 150)
  } 
  
  
  
  
  # Specify the SEM model
  sem_model <- '
  # Relationships for beliefs
  belief_compliance_pm  ~  survey2.1.player.confianza_caleta_conocida_mean + survey2.1.player.conflicto_caleta_conocida_mean
  belief_compliance_union   ~  survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
  
  # Relationship for extraction
  average_compliance_ini ~ belief_compliance_pm + belief_compliance_union +   
  survey2.1.player.confianza_caleta_conocida_mean + survey2.1.player.conflicto_caleta_conocida_mean + survey1.1.player.confianza_caleta + 
  survey1.1.player.conflicto_caleta
  
   # Ensure no direct effect from average_compliance_observed_ini_lag to restricted variables
  average_compliance_ini ~ average_compliance_observed_ini_lag

  # Restrict correlations between average_compliance_observed_ini_lag and other variables
  average_compliance_observed_ini_lag ~~ 0*belief_compliance_union
   average_compliance_observed_ini_lag ~~ 0*belief_compliance_pm
  
  
'
  #name variable so that there are comprehensible
  node_labels <- c(belief_compliance_pm =   "Beliefs Compl. Others" ,
                   belief_compliance_union =   "Beliefs Compl. Union", 
                   average_compliance_ini = "Compliance" ,
                   survey2.1.player.confianza_caleta_conocida_mean  =   "Trust Others (mean)" ,
                   survey2.1.player.conflicto_caleta_conocida_mean =   "Conflict Others (mean)" ,
                   survey1.1.player.confianza_caleta =   "Trust Union", 
                   survey1.1.player.conflicto_caleta =   "Conflict Union",
                   average_compliance_observed_ini_lag = "Observed Compliance (t-1)"
  )
  
  
  # Fit the SEM model
  fit <- sem(sem_model, data = df)
  
  # Extract path results and edge colors dynamically
  path_results <- parameterEstimates(fit, standardized = TRUE)
  regression_paths <- path_results[path_results$op == "~", ]
  param_labels <- paste(regression_paths$lhs, regression_paths$op, regression_paths$rhs, sep = " ")
  edge_colors_map <- setNames(ifelse(regression_paths$pvalue < 0.05, "black", "red"), param_labels)
  edge_colors <- ifelse(regression_paths$pvalue < 0.05, "black", "red")
  
  # Ensure edge_colors matches the number of paths
  n_edges <- length(regression_paths$lhs)
  if (length(edge_colors) < n_edges) {
    edge_colors <- c(edge_colors, rep("gray", n_edges - length(edge_colors)))
  }
  
  # Save each SEM plot with a dynamic filename
  output_file <- paste0(path_github, "Outputs/SEM_compliance_OA_T2_plot_Rounds_", R, "_to_", N, ".png")
  png(output_file, width = 1300, height = 800, res = 300)
  
  # Generate SEM plot
  semPaths(
    fit,
    what = "std",
    layout = "tree",
    edge.label.cex = 1,
    nodeLabels = node_labels,
    sizeMan = 9,
    label.cex = 1,
    node.width = 2, 
    node.height = 0.7,
    shapeMan = "ellipse", 
    edge.color = edge_colors,
    residuals = FALSE,
    intercepts = FALSE,
    optimizeLatRes = TRUE,
    fade = FALSE
  )
  
  # Add dynamic title
  title(main = paste("Mean Compliance Scenario 2 Open Access Rounds ", R ," to ", N),line = 3.5, cex.main = 0.8)
  
  # Close the PDF device
  dev.off()
  
  
  # End of script
  
  
  
########################################
### SEM in TURF Scenario 2 Round 1
########################################

# Set R to 1
R <- 1

# Subset variables dynamically
variable_subset <- df[, grep("^T2juegoalgas\\.\\d+\\.player\\.T2_extraccion_amerb$", names(df))]
variable_subset <- variable_subset[, R, drop = FALSE]  # Select only the first round

# Calculate the row-wise mean for the selected column
df$average_extraction_ini <- rowMeans(variable_subset, na.rm = TRUE)
df$average_compliance_ini <- 1 - (df$average_extraction_ini / 50)

# Belief compliance
df$belief_compliance_amerb <- 1 - (df$beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini / 50)

# Define the SEM model
sem_model <- '
  # Relationships for beliefs
  belief_compliance_amerb  ~  survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
  

  # Relationship for extraction
  average_compliance_ini ~ belief_compliance_amerb +  survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta 
  '

node_labels <- c(
  belief_compliance_amerb = "Beliefs Compl. Union",
  average_compliance_ini = "Compliance",
  survey1.1.player.confianza_caleta = "Trust Union",
  survey1.1.player.conflicto_caleta = "Conflict Union"
)

# Fit the SEM model
fit <- sem(sem_model, data = df)

# Extract path results and edge colors dynamically
path_results <- parameterEstimates(fit, standardized = TRUE)
regression_paths <- path_results[path_results$op == "~", ]
param_labels <- paste(regression_paths$lhs, regression_paths$op, regression_paths$rhs, sep = " ")
edge_colors_map <- setNames(ifelse(regression_paths$pvalue < 0.05, "black", "red"), param_labels)
edge_colors <- ifelse(regression_paths$pvalue < 0.05, "black", "red")

# Ensure edge_colors matches the number of paths
n_edges <- length(regression_paths$lhs)
if (length(edge_colors) < n_edges) {
  edge_colors <- c(edge_colors, rep("gray", n_edges - length(edge_colors)))
}

# Save SEM plot with a dynamic filename
output_file <- paste0(path_github, "Outputs/SEM_compliance_amerb_T2_plot_Round_1.png")
png(output_file, width = 1300, height = 800, res = 300)

# Generate SEM plot
semPaths(
  fit,
  what = "std",
  layout = "tree",
  edge.label.cex = 1,
  nodeLabels = node_labels,
  sizeMan = 9,
  label.cex = 1,
  node.width = 2, 
  node.height = 0.7,
  shapeMan = "ellipse", 
  edge.color = edge_colors,
  residuals = FALSE,
  intercepts = FALSE,
  optimizeLatRes = TRUE,
  fade = FALSE
)

# Add title
title(main = "Mean Compliance TURF Round 1 Scenario 2", line = 3.5, cex.main = 0.8)

# Close the PDF device
dev.off()


###################################
### SEM TURF Scenario 2 rounds (5:8)
###################################



# Set R (starting round)
R <- 5

# Loop through N from (R+1) to 8
for (N in (R+1):8) {
  
  # Subset variables dynamically for amerb extraction
  variable_subset <- df[, grep("^T2juegoalgas\\.\\d+\\.player\\.T2_extraccion_amerb$", names(df))]
  variable_subset <- variable_subset[, R:N]  # Select columns for current N
  
  # Calculate the row-wise mean for selected columns
  df$average_extraction_ini <- rowMeans(variable_subset, na.rm = TRUE)
  df$average_compliance_ini <- 1 - (df$average_extraction_ini / 50)
  
  # Belief compliance for amerb
  df$belief_compliance_amerb <- 1 - (df$beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini / 50)
  
  
  # Observed compliance
  variable_subset <- df[, grep("T2juegoalgas\\.(\\d+)\\.player\\..+_extraccion_otros_amerb$", names(df))]
  variable_subset <- variable_subset[, R:N-1]
  df$average_extraction_observed_ini <- rowMeans(variable_subset, na.rm = TRUE)
  df$average_compliance_observed_ini_lag <- 1 - (df$average_extraction_observed_ini / 150)
} 

# Define the SEM model for amerb
sem_model <- '
  # Relationships for beliefs
  belief_compliance_amerb  ~  survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
  
  # Relationship for extraction
  average_compliance_ini ~ belief_compliance_amerb +  
  survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta 
  
 # Ensure no direct effect from average_compliance_observed_ini_lag to restricted variables
  average_compliance_ini ~ average_compliance_observed_ini_lag

  # Restrict correlations between average_compliance_observed_ini_lag and other variables
  average_compliance_observed_ini_lag ~~ 0*belief_compliance_amerb
'

# Node labels
node_labels <- c(
  belief_compliance_amerb = "Beliefs Compl. Union",
  average_compliance_ini = "Compliance",
  survey1.1.player.confianza_caleta = "Trust Union",
  survey1.1.player.conflicto_caleta = "Conflict Union",
  average_compliance_observed_ini_lag = "Observed Compliance (t-1)"
)

# Fit the SEM model
fit <- sem(sem_model, data = df)

# Extract path results and edge colors dynamically
path_results <- parameterEstimates(fit, standardized = TRUE)
regression_paths <- path_results[path_results$op == "~", ]
param_labels <- paste(regression_paths$lhs, regression_paths$op, regression_paths$rhs, sep = " ")
edge_colors_map <- setNames(ifelse(regression_paths$pvalue < 0.05, "black", "red"), param_labels)
edge_colors <- ifelse(regression_paths$pvalue < 0.05, "black", "red")

# Ensure edge_colors matches the number of paths
n_edges <- length(regression_paths$lhs)
if (length(edge_colors) < n_edges) {
  edge_colors <- c(edge_colors, rep("gray", n_edges - length(edge_colors)))
}

# Save SEM plot with a dynamic filename
output_file <- paste0(path_github, "Outputs/SEM_compliance_amerb_T2_plot_Rounds_", R, "_to_", N, ".png")
png(output_file, width = 1300, height = 800, res = 300)

# Generate SEM plot
semPaths(
  fit,
  what = "std",
  layout = "tree",
  edge.label.cex = 1,
  nodeLabels = node_labels,
  sizeMan = 9,
  label.cex = 1,
  node.width = 2, 
  node.height = 0.7,
  shapeMan = "ellipse", 
  edge.color = edge_colors,
  residuals = FALSE,
  intercepts = FALSE,
  optimizeLatRes = TRUE,
  fade = FALSE
)

# Add dynamic title
title(main = paste("Mean Compliance Scenario 2 TURF Rounds ", R ," to ", N), line = 3.5, cex.main = 0.8)

# Close the P
# Close the PDF device
dev.off()



