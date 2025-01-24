









if (!requireNamespace("semPlot", quietly = TRUE)) install.packages("semPlot")
if (!requireNamespace("lavaan", quietly = TRUE)) install.packages("lavaan")

library(lavaan)
library(semPlot)


rm(list=ls())
#path_github <-"C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
#path_datos<-"C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

path_github <-"C:/Users/Denise Laroze/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos<-"C:/Users/Denise Laroze/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

setwd(path_github)


#load(paste0(path_datos, "/Datos_islitas.Rdata"))
load(paste0(path_datos, "/Datos_islitas_recode.Rdata"))
load(paste0(path_datos, "/Datos_islitas_long.Rdata"))



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


############################################################
#### SEM --- Average compliance OA with observed compliance
########################################################

# Ensure your data frame has the relevant columns
variable_subset <- df[, grep("^T1juegoalgas\\.\\d+\\.player\\.T1_extraccion_libre$", names(df))]

# Calculate the row-wise mean for the selected columns
df$average_extraction <- rowMeans(variable_subset, na.rm = TRUE)
df$average_compliance<- 1-(df$average_extraction/50) 

df$belief_compliance_pm<-1-(df$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini/50)
df$belief_compliance_union<-1-(df$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini/50)
df$real_compliance_others<- 1-(df$otros_libre_t1_mean/50)

# View the first few rows of the updated data frame
#head(df$average_extraction)
#head(df$average_compliance)


# Specify the SEM model
sem_model <- '
  # Relationships for beliefs
  belief_compliance_pm  ~  survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm
  belief_compliance_union   ~  survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
  
  # Relationship for extraction
  average_compliance ~ belief_compliance_pm + belief_compliance_union +   survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm + survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta  + real_compliance_others 
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
                 survey1.1.player.conflicto_caleta =   "Conflict Union",
                 real_compliance_others = "Observed Compliance OA"
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
pdf(paste0(path_github, "Outputs/SEM_compliance_plot_controling_experience.pdf"), width = 12, height = 8)  # Set dimensions in inches
semPaths(
  fit,
  what = "std",
  #layout = "tree",
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







#################################################
#### Compliance by different subsets of rounds T1
###############################################

# Loop through N from 1 to 8
for (N in 2:8) {
  # Subset variables dynamically
  variable_subset <- df[, grep("^T1juegoalgas\\.\\d+\\.player\\.T1_extraccion_libre$", names(df))]
  variable_subset <- variable_subset[, 1:N]  # Select columns for current N
  
  # Calculate the row-wise mean for selected columns
  df$average_extraction_ini <- rowMeans(variable_subset, na.rm = TRUE)
  df$average_compliance_ini <- 1 - (df$average_extraction_ini / 50)
  
  # Belief compliance
  df$belief_compliance_pm <- 1 - (df$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50)
  df$belief_compliance_union <- 1 - (df$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50)
  
  # Observed compliance
  variable_subset <- df[, grep("(T\\d)juegoalgas\\.(\\d+)\\.player\\..+_extraccion_otros_libre$", names(df))]
  variable_subset <- variable_subset[, 1:N]
  df$average_extraction_observed_ini <- rowMeans(variable_subset, na.rm = TRUE)
  df$average_compliance_observed_ini <- 1 - (df$average_extraction_observed_ini / 150)

  
  
  # Specify the SEM model
  sem_model <- '
  # Relationships for beliefs
  belief_compliance_pm  ~  survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm
  belief_compliance_union   ~  survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
  
  # Relationship for extraction
  average_compliance_ini ~ belief_compliance_pm + belief_compliance_union +   
  survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm + survey1.1.player.confianza_caleta + 
  survey1.1.player.conflicto_caleta  + average_compliance_observed_ini 
'
  #name variable so that there are comprensible
  node_labels <- c(belief_compliance_pm =   "Beliefs Compliance Others OA" ,
                   belief_compliance_union =   "Beliefs Compliance Union OA", 
                   average_compliance_ini = "Mean Compliance OA early rounds" ,
                   survey1.1.player.confianza_pm  =   "Trust Others" ,
                   survey1.1.player.conflicto_pm =   "Conflict Others" ,
                   survey1.1.player.confianza_caleta =   "Trust Union", 
                   survey1.1.player.conflicto_caleta =   "Conflict Union",
                   average_compliance_observed_ini = "Observed Compliance OA"
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
  output_file <- paste0(path_github, "Outputs/SEM_compliance_T1_plot_Rounds_1_to_", N, ".pdf")
  pdf(output_file, width = 12, height = 8)
  
  # Generate SEM plot
  semPaths(
    fit,
    what = "std",
    layout = "circle2",
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
    optimizeLatRes = TRUE
  )
  
  # Add dynamic title
  title(main = paste("DV: Mean Compliance T1 Rounds 1 to", N), line = 2, cex.main = 1.5)
  
  # Close the PDF device
  dev.off()
}

# End of script







######################################################
#### Compliance byt different subsets of rounds T2
#######################################################


# Loop through N from 1 to 8
for (N in 2:8) {
  # Subset variables dynamically
  variable_subset <- df[, grep("^T2juegoalgas\\.\\d+\\.player\\.T2_extraccion_metat$", names(df))]
  variable_subset <- variable_subset[, 1:N]  # Select columns for current N
  
  # Calculate the row-wise mean for selected columns
  df$average_extraction_ini <- rowMeans(variable_subset, na.rm = TRUE)
  df$average_compliance_ini <- 1 - (df$average_extraction_ini / 50)
  
  # Belief compliance
  df$belief_compliance_metat <- 1 - (df$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50)
  df$belief_compliance_union <- 1 - (df$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50)
  
  # Observed compliance
  variable_subset <- df[, grep("(T\\d)juegoalgas\\.(\\d+)\\.player\\..+_extraccion_otros_metat$", names(df))]
  variable_subset <- variable_subset[, 1:N]
  df$average_extraction_observed_ini <- rowMeans(variable_subset, na.rm = TRUE)
  df$average_compliance_observed_ini <- 1 - (df$average_extraction_observed_ini / 150)
  
  
  sem_model <- '
  # Relationships for beliefs
  belief_compliance_metat  ~   survey2.1.player.confianza_caleta_conocida1 + survey2.1.player.conflicto_caleta_conocida1
  belief_compliance_union   ~  survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
  
  # Relationship for extraction
  average_compliance_ini ~ belief_compliance_metat + belief_compliance_union +   
  survey2.1.player.confianza_caleta_conocida1 + survey2.1.player.conflicto_caleta_conocida1 + survey1.1.player.confianza_caleta + 
  survey1.1.player.conflicto_caleta  + average_compliance_observed_ini 
'
  
  node_labels <- c(belief_compliance_metat =   "Beliefs Compliance Others OA" ,
                   belief_compliance_union =   "Beliefs Compliance Union OA", 
                   average_compliance_ini = "Mean Compliance T2 early rounds" ,
                   survey2.1.player.confianza_caleta_conocida1  =   "Trust Others 1 T2" ,
                   survey2.1.player.conflicto_caleta_conocida1 =   "Conflict Others 1 T2" ,
                   survey1.1.player.confianza_caleta =   "Trust Union", 
                   survey1.1.player.conflicto_caleta =   "Conflict Union",
                   average_compliance_observed_ini = "Observed Compliance OA"
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
  output_file <- paste0(path_github, "Outputs/SEM_compliance_T2_plot_Rounds_1_to_", N, ".pdf")
  pdf(output_file, width = 12, height = 8)
  
  # Generate SEM plot
  semPaths(
    fit,
    what = "std",
    layout = "circle2",
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
    optimizeLatRes = TRUE
  )
  
  # Add dynamic title
  title(main = paste("DV: Mean Compliance T2 Rounds 1 to", N), line = 2, cex.main = 1.5)
  
  # Close the PDF device
  dev.off()
}

# End of script


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


