


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
df$belief_compliance_amerb <- 1 - (df$beliefsT1final.1.player.T1_belief_caleta_en_amerb_fin / 50)

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
output_file <- paste0(path_github, "Outputs/SEM_compliance_amerb_T2_plot_Round_1_tmp.png")
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
  df$belief_compliance_amerb <- 1 - (df$beliefsT1final.1.player.T1_belief_caleta_en_amerb_fin / 50)
  
  
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
output_file <- paste0(path_github, "Outputs/SEM_compliance_amerb_T2_plot_Rounds_", R, "_to_", N, "_tmp.png")
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



