

if (!requireNamespace("semPlot", quietly = TRUE)) install.packages("semPlot")
if (!requireNamespace("lavaan", quietly = TRUE)) install.packages("lavaan")

library(lavaan)
library(semPlot)



####################################
#### SEM --- Average extraction OA
#####################################

# Ensure your data frame has the relevant columns
variable_subset <- df[, grep("^T1juegoalgas\\.\\d+\\.player\\.T1_extraccion_libre$", names(df))]

# Calculate the row-wise mean for the selected columns
df$average_extraction <- rowMeans(variable_subset, na.rm = TRUE)

# View the first few rows of the updated data frame
head(df$average_extraction)


# Specify the SEM model
sem_model <- '
  # Relationships for beliefs
  beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini  ~  survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm
  beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini   ~  survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
  
  # Relationship for extraction
  average_extraction ~ beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini + beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini +   survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm + survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta   
'

# Fit the SEM model
fit <- sem(sem_model, data = df)

# Summarize the SEM model results
summary(fit, fit.measures = TRUE, standardized = TRUE)

node_order <- semPaths(fit, what = "std", plot = FALSE)$graphAttributes$Nodes$labels

# Print node order
print(node_order)


node_labels <- c(beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini =   "Beliefs Others OA" ,
                 beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini =   "Beliefs Union OA", 
                 average_extraction = "Mean Extraction" ,
                 survey1.1.player.confianza_pm  =   "Trust Others" ,
                 survey1.1.player.conflicto_pm =   "Conflict Others" ,
                 survey1.1.player.confianza_caleta =   "Trust Union", 
                 survey1.1.player.conflicto_caleta =   "Conflict Union" 
)

# Extract parameter estimates from the fitted SEM model
path_results <- parameterEstimates(fit, standardized = TRUE)

# Filter regression paths (op == "~")
regression_paths <- path_results[path_results$op == "~", ]

# Create a vector of path labels for matching
param_labels <- paste(regression_paths$lhs, regression_paths$op, regression_paths$rhs, sep = " ")


# Create a named vector for edge colors based on p-value
edge_colors_map <- setNames(ifelse(regression_paths$pvalue < 0.05, "black", "red"), param_labels)


# Dynamically generate edge colors
n_edges <- length(regression_paths$lhs)  # Number of regression paths
edge_colors <- ifelse(regression_paths$pvalue < 0.05, "black", "red")

# Check the length of edge_colors matches the number of paths
if (length(edge_colors) < n_edges) {
  edge_colors <- c(edge_colors, rep("gray", n_edges - length(edge_colors)))
}

SEM_plot<-semPaths(
  fit,
  what = "std",
  layout = "tree",
  edge.label.cex = 1.2,
  nodeLabels = node_labels,
  sizeMan = 8,
  sizeLat = 10,
  label.cex = 2,
 edge.color = edge_colors,
  residuals = FALSE,  # Hide residuals
  intercepts = FALSE, # Hide intercepts
  optimizeLatRes = TRUE  # Optimize latent and residual placement
)

# Save the SEM plot as a PDF
pdf(paste0(path_github, "Outputs/SEM_plot.pdf"), width = 12, height = 8)  # Set dimensions in inches
semPaths(
  fit,
  what = "std",
  layout = "tree",
  edge.label.cex = 1.2,
  nodeLabels = node_labels,
  sizeMan = 8,
  sizeLat = 10,
  label.cex = 2,
  edge.color = edge_colors,
  residuals = FALSE,
  intercepts = FALSE,
  optimizeLatRes = TRUE
)
dev.off()  # Close the PDF device




####################################
#### SEM --- Average compliance OA
#####################################

# Ensure your data frame has the relevant columns
variable_subset <- df[, grep("^T1juegoalgas\\.\\d+\\.player\\.T1_extraccion_libre$", names(df))]

# Calculate the row-wise mean for the selected columns
df$average_extraction <- rowMeans(variable_subset, na.rm = TRUE)
df$average_compliance<- 1-(df$average_extraction/50) 

df$belief_compliance_pm<-1-(df$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini/50)
df$belief_compliance_union<-1-(df$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini/50)

# View the first few rows of the updated data frame
head(df$average_extraction)
head(df$average_compliance)


# Specify the SEM model
sem_model <- '
  # Relationships for beliefs
  belief_compliance_pm  ~  survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm
  belief_compliance_union   ~  survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
  
  # Relationship for extraction
  average_compliance ~ belief_compliance_pm + belief_compliance_union +   survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm + survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta   
'

# Fit the SEM model
fit <- sem(sem_model, data = df)

# Summarize the SEM model results
summary(fit, fit.measures = TRUE, standardized = TRUE)

node_order <- semPaths(fit, what = "std", plot = FALSE)$graphAttributes$Nodes$labels

# Print node order
print(node_order)


node_labels <- c(belief_compliance_pm =   "Beliefs Compliance Others OA" ,
                 belief_compliance_union =   "Beliefs Compliance Union OA", 
                 average_compliance = "Mean Compliance OA" ,
                 survey1.1.player.confianza_pm  =   "Trust Others" ,
                 survey1.1.player.conflicto_pm =   "Conflict Others" ,
                 survey1.1.player.confianza_caleta =   "Trust Union", 
                 survey1.1.player.conflicto_caleta =   "Conflict Union" 
)

# Extract parameter estimates from the fitted SEM model
path_results <- parameterEstimates(fit, standardized = TRUE)

# Filter regression paths (op == "~")
regression_paths <- path_results[path_results$op == "~", ]

# Create a vector of path labels for matching
param_labels <- paste(regression_paths$lhs, regression_paths$op, regression_paths$rhs, sep = " ")


# Create a named vector for edge colors based on p-value
edge_colors_map <- setNames(ifelse(regression_paths$pvalue < 0.05, "black", "red"), param_labels)


# Dynamically generate edge colors
n_edges <- length(regression_paths$lhs)  # Number of regression paths
edge_colors <- ifelse(regression_paths$pvalue < 0.05, "black", "red")

# Check the length of edge_colors matches the number of paths
if (length(edge_colors) < n_edges) {
  edge_colors <- c(edge_colors, rep("gray", n_edges - length(edge_colors)))
}


# Save the SEM plot as a PDF
pdf(paste0(path_github, "Outputs/SEM_compliance_plot.pdf"), width = 12, height = 8)  # Set dimensions in inches
semPaths(
  fit,
  what = "std",
  layout = "tree",
  edge.label.cex = 1,
  nodeLabels = node_labels,
  sizeMan = 9,
  sizeLat = 10,
  label.cex = 1,
  node.width = 2, 
  node.height = 0.7,
  shapeMan = "ellipse", 
  edge.color = edge_colors,
  residuals = FALSE,
  intercepts = FALSE,
  optimizeLatRes = TRUE
)
dev.off()  # Close the PDF device







###############################
### SEM extraction first round
################################



# Specify the SEM model
sem_model <- '
  # Relationships for beliefs
  beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini  ~  survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm
  beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini   ~  survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
  
  # Relationship for extraction
  T1juegoalgas.1.player.T1_extraccion_libre ~ beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini + beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini
'

# Fit the SEM model
fit <- sem(sem_model, data = df)

# Summarize the SEM model results
summary(fit, fit.measures = TRUE, standardized = TRUE)

node_order <- semPaths(fit, what = "std", plot = FALSE)$graphAttributes$Nodes$labels

# Print node order
#print(node_order)


node_labels <- c(beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini =   "Beliefs Others OA" ,
                 beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini =   "Beliefs Union OA", 
                 T1juegoalgas.1.player.T1_extraccion_libre = "Extraction OA Ronda 1" ,
                 survey1.1.player.confianza_pm  =   "Trust Others" ,
                 survey1.1.player.conflicto_pm =   "Conflict Others" ,
                 survey1.1.player.confianza_caleta =   "Trust Union", 
                 survey1.1.player.conflicto_caleta =   "Conflict Union" 
)

# Extract parameter estimates from the fitted SEM model
path_results <- parameterEstimates(fit, standardized = TRUE)

# Filter regression paths (op == "~")
regression_paths <- path_results[path_results$op == "~", ]

# Create a vector of path labels for matching
param_labels <- paste(regression_paths$lhs, regression_paths$op, regression_paths$rhs, sep = " ")


# Create a named vector for edge colors based on p-value
edge_colors_map <- setNames(ifelse(regression_paths$pvalue < 0.05, "black", "red"), param_labels)


# Dynamically generate edge colors
n_edges <- length(regression_paths$lhs)  # Number of regression paths
edge_colors <- ifelse(regression_paths$pvalue < 0.05, "black", "red")

# Check the length of edge_colors matches the number of paths
if (length(edge_colors) < n_edges) {
  edge_colors <- c(edge_colors, rep("gray", n_edges - length(edge_colors)))
}

semPaths(
  fit,
  what = "std",
  layout = "tree",
  edge.label.cex = 3,
  nodeLabels = node_labels,
  sizeMan = 8,
  sizeLat = 10,
  label.cex = 2,
  edge.color = edge_colors,
  residuals = FALSE,  # Hide residuals
  intercepts = FALSE, # Hide intercepts
  optimizeLatRes = TRUE  # Optimize latent and residual placement
)


