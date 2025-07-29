

if (!requireNamespace("semPlot", quietly = TRUE)) install.packages("semPlot")
if (!requireNamespace("lavaan", quietly = TRUE)) install.packages("lavaan")

library(lavaan)
library(semPlot)

rm(list=ls())
path_github <- "C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos <- "C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

setwd(path_github)

load(paste0(path_datos, "/Datos_islitas_recode.Rdata"))
load(paste0(path_datos, "/Datos_islitas_long.Rdata"))

get_columns_by_round <- function(prefix, suffix, start, end = NULL) {
  if (is.null(end)) end <- start
  colnames <- character()
  rounds <- start:end
  for (r in rounds) {
    pattern <- paste0("^", prefix, "\\.", r, "\\.player\\.", suffix, "$")
    matched <- grep(pattern, names(df), value = TRUE)
    if (length(matched) > 0) {
      colnames <- c(colnames, matched)
    }
  }
  if (length(colnames) == 0) stop(paste("No matching columns for", prefix, suffix, start, end))
  colnames
}


#####################
### Turf Round 1 - T1
###################


save_sem_plot <- function(df, R, N, path_github) {
  cols <- get_columns_by_round("T1juegoalgas", "T1_extraccion_amerb", R, N)
  df$average_extraction_ini <- rowMeans(df[, cols, drop = FALSE], na.rm = TRUE)
  df$average_compliance_ini <- 1 - (df$average_extraction_ini / 50)
  df$belief_compliance_amerb <- 1 - (df$beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini / 50)
  
  sem_model <- 'belief_compliance_amerb ~ survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
                average_compliance_ini ~ belief_compliance_amerb + survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta'
  
  node_labels <- c(
    "belief_compliance_amerb" = "Prior Beliefs",
    "average_compliance_ini" = "Compliance",
    "survey1.1.player.confianza_caleta" = "Trust TURF",
    "survey1.1.player.conflicto_caleta" = "Conflict TURF"
  )
  
  fit <- sem(sem_model, data = df)
  
  # Debug: print node names used in the semPaths graph
  node_names <- semPlot::semPlotModel(fit)@Vars$name
  cat("Node names detected for semPaths labeling:\n")
  print(node_names)
  
  paths <- parameterEstimates(fit, standardized = TRUE)
  edge_colors <- ifelse(paths$pvalue[paths$op == "~"] < 0.05, "black", "transparent")
  
  output_file <- paste0(path_github, "Outputs/SEM_Turf_T1_plot_Rounds_", R, "_to_", N, ".pdf")
  pdf(output_file, width = 12, height = 8)
  
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
    shapeMan = "rectangle",
    edge.color = edge_colors,
    residuals = FALSE,
    intercepts = FALSE,
    optimizeLatRes = TRUE
  )
  
  title(main = paste("Mean Compliance TURF Round ", R), line = 2, cex.main = 1.5)
  dev.off()
}


# Usage:
save_sem_plot(df, 1, 1, path_github)

#############################
### Turf rounds 2-8 T1
#############################


save_dynamic_sem_plot <- function(df, R, N, path_github) {
  # 1. Compute average extraction and compliance for the selected rounds
  cols <- get_columns_by_round("T1juegoalgas", "T1_extraccion_amerb", R, N)
  variable_subset <- df[, cols, drop = FALSE]
  df$average_extraction_ini <- rowMeans(variable_subset, na.rm = TRUE)
  df$average_compliance_ini <- 1 - (df$average_extraction_ini / 50)
  df$belief_compliance_amerb <- 1 - (df$beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini / 50)
  
  # 2. Compute observed compliance in previous rounds (lag)
  # Careful: if R == 1, this will fail; you may want to check for valid lag
  if (R > 1) {
    cols_obs <- get_columns_by_round("T1juegoalgas", "T1_extraccion_otros_amerb", R - 1, N - 1)
    variable_subset_obs <- df[, cols_obs, drop = FALSE]
    df$average_extraction_observed_ini <- rowMeans(variable_subset_obs, na.rm = TRUE)
    df$average_compliance_observed_ini_lag <- 1 - (df$average_extraction_observed_ini / 150)
  } else {
    df$average_compliance_observed_ini_lag <- NA  # Or 0, but NA is safest for modeling
  }
  
  # 3. SEM model: now includes the lagged compliance
  sem_model <- '
    belief_compliance_amerb ~ survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
    average_compliance_ini ~ belief_compliance_amerb + survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta + average_compliance_observed_ini_lag
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_amerb
  '
  
  node_labels <- c(
    "belief_compliance_amerb" = "Prior Beliefs",
    "average_compliance_ini" = "Compliance",
    "survey1.1.player.confianza_caleta" = "Trust TURF",
    "survey1.1.player.conflicto_caleta" = "Conflict TURF",
    "average_compliance_observed_ini_lag" = "Updated Beliefs"
  )
  
  fit <- sem(sem_model, data = df)
  
  # Debug: print node names for this dynamic model
  node_names <- semPlot::semPlotModel(fit)@Vars$name
  cat("Node names detected for dynamic SEM labeling:\n")
  print(node_names)
  
  paths <- parameterEstimates(fit, standardized = TRUE)
  edge_colors <- ifelse(paths$pvalue[paths$op == "~"] < 0.05, "black", "transparent")
  
  # Save to PDF (rectangle shape)
  output_file <- paste0(path_github, "Outputs/SEM_Turf_T1_dynamic_plot_Rounds_", R, "_to_", N, ".pdf")
  pdf(output_file, width = 12, height = 8)
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
    shapeMan = "rectangle",
    edge.color = edge_colors,
    residuals = FALSE,
    intercepts = FALSE,
    optimizeLatRes = TRUE,
    fade = FALSE
  )

  title(main = paste("Mean Compliance TURF Rounds", R, "to", N), line = 2, cex.main = 1.5)
  dev.off()
}

save_dynamic_sem_plot(df, 2, 8, path_github)

#####################
### Turf Round 1 - T2
###################


save_sem_plot <- function(df, R, N, path_github) {
  cols <- get_columns_by_round("T2juegoalgas", "T2_extraccion_amerb", R, N)
  df$average_extraction_ini <- rowMeans(df[, cols, drop = FALSE], na.rm = TRUE)
  df$average_compliance_ini <- 1 - (df$average_extraction_ini / 50)
  df$belief_compliance_amerb <- 1 - (df$beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini / 50)
  
  sem_model <- 'belief_compliance_amerb ~ survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
                average_compliance_ini ~ belief_compliance_amerb + survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta'
  
  node_labels <- c(
    "belief_compliance_amerb" = "Prior Beliefs",
    "average_compliance_ini" = "Compliance",
    "survey1.1.player.confianza_caleta" = "Trust TURF",
    "survey1.1.player.conflicto_caleta" = "Conflict TURF"
  )
  
  fit <- sem(sem_model, data = df)
  
  # Debug: print node names used in the semPaths graph
  node_names <- semPlot::semPlotModel(fit)@Vars$name
  cat("Node names detected for semPaths labeling:\n")
  print(node_names)
  
  paths <- parameterEstimates(fit, standardized = TRUE)
  edge_colors <- ifelse(paths$pvalue[paths$op == "~"] < 0.05, "black", "transparent")
  
  output_file <- paste0(path_github, "Outputs/SEM_Turf_T2_plot_Rounds_", R+8, "_to_", N+8, ".pdf")
  pdf(output_file, width = 12, height = 8)
  
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
    shapeMan = "rectangle",
    edge.color = edge_colors,
    residuals = FALSE,
    intercepts = FALSE,
    optimizeLatRes = TRUE
  )
  
  title(main = paste("Mean Compliance TURF Round", R+8), line = 2, cex.main = 1.5)
  dev.off()
}


# Usage:
save_sem_plot(df, 1, 1, path_github)

#############################
### Turf rounds 2-8 T2
#############################


save_dynamic_sem_plot <- function(df, R, N, path_github) {
  # 1. Compute average extraction and compliance for the selected rounds
  cols <- get_columns_by_round("T2juegoalgas", "T2_extraccion_amerb", R, N)
  variable_subset <- df[, cols, drop = FALSE]
  df$average_extraction_ini <- rowMeans(variable_subset, na.rm = TRUE)
  df$average_compliance_ini <- 1 - (df$average_extraction_ini / 50)
  df$belief_compliance_amerb <- 1 - (df$beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini / 50)
  
  # 2. Compute observed compliance in previous rounds (lag)
  # Careful: if R == 1, this will fail; you may want to check for valid lag
  if (R > 1) {
    cols_obs <- get_columns_by_round("T2juegoalgas", "T2_extraccion_otros_amerb", R - 1, N - 1)
    variable_subset_obs <- df[, cols_obs, drop = FALSE]
    df$average_extraction_observed_ini <- rowMeans(variable_subset_obs, na.rm = TRUE)
    df$average_compliance_observed_ini_lag <- 1 - (df$average_extraction_observed_ini / 150)
  } else {
    df$average_compliance_observed_ini_lag <- NA  # Or 0, but NA is safest for modeling
  }
  
  # 3. SEM model: now includes the lagged compliance
  sem_model <- '
    belief_compliance_amerb ~ survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
    average_compliance_ini ~ belief_compliance_amerb + survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta + average_compliance_observed_ini_lag
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_amerb
  '
  
  node_labels <- c(
    "belief_compliance_amerb" = "Prior Beliefs",
    "average_compliance_ini" = "Compliance",
    "survey1.1.player.confianza_caleta" = "Trust TURF",
    "survey1.1.player.conflicto_caleta" = "Conflict TURF",
    "average_compliance_observed_ini_lag" = "Updated Beliefs"
  )
  
  fit <- sem(sem_model, data = df)
  
  # Debug: print node names for this dynamic model
  node_names <- semPlot::semPlotModel(fit)@Vars$name
  cat("Node names detected for dynamic SEM labeling:\n")
  print(node_names)
  
  paths <- parameterEstimates(fit, standardized = TRUE)
  edge_colors <- ifelse(paths$pvalue[paths$op == "~"] < 0.05, "black", "transparent")
  
  # Save to PDF (rectangle shape)
  output_file <- paste0(path_github, "Outputs/SEM_Turf_T2_dynamic_plot_Rounds_", R+8, "_to_", N+8, ".pdf")
  pdf(output_file, width = 12, height = 8)
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
    shapeMan = "rectangle",
    edge.color = edge_colors,
    residuals = FALSE,
    intercepts = FALSE,
    optimizeLatRes = TRUE,
    fade = FALSE
  )
  
  title(main = paste("Mean Compliance TURF Rounds", R+8, "to", N+8), line = 2, cex.main = 1.5)
  dev.off()
}

save_dynamic_sem_plot(df, 2, 8, path_github)









##############################
### Shared Area Round 1 - T1
##############################

save_sharedarea_sem_plot <- function(df, R, N, path_github) {
  cols <- get_columns_by_round("T1juegoalgas", "T1_extraccion_libre", R, N)
  df$average_extraction_ini <- df[, cols, drop = TRUE]
  df$average_compliance_ini <- 1 - (df$average_extraction_ini / 50)
  df$belief_compliance_pm <- 1 - (df$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50)
  df$belief_compliance_union <- 1 - (df$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50)
  
  sem_model <- '
    belief_compliance_pm ~ survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm
    belief_compliance_union ~ survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
    average_compliance_ini ~ belief_compliance_pm + belief_compliance_union + survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm + survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
  '
  node_labels <- c(
    "belief_compliance_pm" = "Prior Beliefs Outsiders",
    "belief_compliance_union" = "Prior Beliefs TURF",
    "average_compliance_ini" = "Compliance",
    "survey1.1.player.confianza_pm" = "Trust Outsiders",
    "survey1.1.player.conflicto_pm" = "Conflict Outsiders",
    "survey1.1.player.confianza_caleta" = "Trust TURF",
    "survey1.1.player.conflicto_caleta" = "Conflict TURF"
  )
  
  fit <- sem(sem_model, data = df)
  node_names <- semPlot::semPlotModel(fit)@Vars$name
  cat("Node names detected for Shared Area static SEM labeling:\n")
  print(node_names)
  
  paths <- parameterEstimates(fit, standardized = TRUE)
  edge_colors <- ifelse(paths$pvalue[paths$op == "~"] < 0.05, "black", "transparent")
  
  output_file <- paste0(path_github, "Outputs/SEM_sharedarea_T1_plot_Rounds_", R, "_to_", N, ".pdf")
  pdf(output_file, width = 12, height = 8)
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
    shapeMan = "rectangle",
    edge.color = edge_colors,
    residuals = FALSE,
    intercepts = FALSE,
    optimizeLatRes = TRUE
  )
  title(main = paste("Shared Area (Unknow Outsiders) Round", R), line = 2, cex.main = 1.5)
  dev.off()
}

# Usage:
save_sharedarea_sem_plot(df, 1, 1, path_github)

##############################
### Shared Area Round 2-8 - T1
##############################
save_sharedarea_dynamic_sem_plot <- function(df, R, N, path_github) {
  cols <- get_columns_by_round("T1juegoalgas", "T1_extraccion_libre", R, N)
  variable_subset <- df[, cols, drop = FALSE]
  df$average_extraction_ini <- rowMeans(variable_subset, na.rm = TRUE)
  df$average_compliance_ini <- 1 - (df$average_extraction_ini / 50)
  df$belief_compliance_pm <- 1 - (df$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50)
  df$belief_compliance_union <- 1 - (df$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50)
  
  if (R > 1) {
    cols_obs <- get_columns_by_round("T1juegoalgas", "T1_extraccion_otros_libre", R - 1, N - 1)
    variable_subset_obs <- df[, cols_obs, drop = FALSE]
    df$average_extraction_observed_ini <- rowMeans(variable_subset_obs, na.rm = TRUE)
    df$average_compliance_observed_ini_lag <- 1 - (df$average_extraction_observed_ini / 150)
  } else {
    df$average_compliance_observed_ini_lag <- NA
  }
  
  sem_model <- '
    belief_compliance_pm ~ survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm
    belief_compliance_union ~ survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
    average_compliance_ini ~ belief_compliance_pm + belief_compliance_union + survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm + survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta + average_compliance_observed_ini_lag
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_union
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_pm
  '
  node_labels <- c(
    "belief_compliance_pm" = "Prior Beliefs Outsiders",
    "belief_compliance_union" = "Prior Beliefs TURF",
    "average_compliance_ini" = "Compliance",
    "survey1.1.player.confianza_pm" = "Trust Outsiders",
    "survey1.1.player.conflicto_pm" = "Conflict Outsiders",
    "survey1.1.player.confianza_caleta" = "Trust TURF",
    "survey1.1.player.conflicto_caleta" = "Conflict TURF",
    "average_compliance_observed_ini_lag" = "Updated Beliefs"
  )
  
  fit <- sem(sem_model, data = df)
  node_names <- semPlot::semPlotModel(fit)@Vars$name
  cat("Node names detected for Shared Area dynamic SEM labeling:\n")
  print(node_names)
  
  paths <- parameterEstimates(fit, standardized = TRUE)
  edge_colors <- ifelse(paths$pvalue[paths$op == "~"] < 0.05, "black", "transparent")
  
  output_file <- paste0(path_github, "Outputs/SEM_sharedarea_T1_dynamic_plot_Rounds_", R, "_to_", N, ".pdf")
  pdf(output_file, width = 12, height = 8)
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
    shapeMan = "rectangle",
    edge.color = edge_colors,
    residuals = FALSE,
    intercepts = FALSE,
    optimizeLatRes = TRUE,
    fade = FALSE
  )
  title(main = paste("Shared Area (Unknow Outsiders) Rounds", R, "to", N), line = 2, cex.main = 1.5)
  dev.off()
}

# Usage:
save_sharedarea_dynamic_sem_plot(df, 2, 8, path_github)





##############################
### Shared Area Round 1 - T2
#############################
save_sharedarea_t2_sem_plot <- function(df, R, N, path_github) {
  cols <- get_columns_by_round("T2juegoalgas", "T2_extraccion_metat", R, N)
  df$average_extraction_ini <- df[, cols, drop = TRUE]
  df$average_compliance_ini <- 1 - (df$average_extraction_ini / 50)
  df$belief_compliance_pm <- 1 - (df$beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini / 50)
  df$belief_compliance_union <- 1 - (df$beliefsT2inicial.1.player.T2_belief_caleta_ini / 50)
  
  sem_model <- '
    belief_compliance_pm ~ survey2.1.player.confianza_caleta_conocida_mean + survey2.1.player.conflicto_caleta_conocida_mean
    belief_compliance_union ~ survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
    average_compliance_ini ~ belief_compliance_pm + belief_compliance_union + survey2.1.player.confianza_caleta_conocida_mean + survey2.1.player.conflicto_caleta_conocida_mean + survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
  '
  node_labels <- c(
    "belief_compliance_pm" = "Prior Beliefs Outsiders",
    "belief_compliance_union" = "Prior Beliefs TURF",
    "average_compliance_ini" = "Compliance",
    "survey2.1.player.confianza_caleta_conocida_mean" = "Trust Know Outsiders",
    "survey2.1.player.conflicto_caleta_conocida_mean" = "Conflict Know Outsider",
    "survey1.1.player.confianza_caleta" = "Trust TURF",
    "survey1.1.player.conflicto_caleta" = "Conflict TURF"
  )
  
  fit <- sem(sem_model, data = df)
  node_names <- semPlot::semPlotModel(fit)@Vars$name
  cat("Node names detected for Shared Area T2 static SEM labeling:\n")
  print(node_names)
  
  paths <- parameterEstimates(fit, standardized = TRUE)
  edge_colors <- ifelse(paths$pvalue[paths$op == "~"] < 0.05, "black", "transparent")
  
  output_file <- paste0(path_github, "Outputs/SEM_sharedarea_T2_plot_Rounds_", R+8, "_to_", N+8, ".pdf")
  pdf(output_file, width = 12, height = 8)
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
    shapeMan = "rectangle",
    edge.color = edge_colors,
    residuals = FALSE,
    intercepts = FALSE,
    optimizeLatRes = TRUE
  )
  title(main = paste("Shared Area (Know Outsiders) Round", R+8), line = 2, cex.main = 1.5)
  dev.off()
}

# Usage:
save_sharedarea_t2_sem_plot(df, 1, 1, path_github)




##############################
### Shared Area Round 2-8 - T2
#############################

save_sharedarea_t2_dynamic_sem_plot <- function(df, R, N, path_github) {
  cols <- get_columns_by_round("T2juegoalgas", "T2_extraccion_metat", R, N)
  variable_subset <- df[, cols, drop = FALSE]
  df$average_extraction_ini <- rowMeans(variable_subset, na.rm = TRUE)
  df$average_compliance_ini <- 1 - (df$average_extraction_ini / 50)
  df$belief_compliance_pm <- 1 - (df$beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini / 50)
  df$belief_compliance_union <- 1 - (df$beliefsT2inicial.1.player.T2_belief_caleta_ini / 50)
  
  if (R > 1) {
    cols_obs <- get_columns_by_round("T2juegoalgas", "T2_extraccion_otros_metat", R - 1, N - 1)
    variable_subset_obs <- df[, cols_obs, drop = FALSE]
    df$average_extraction_observed_ini <- rowMeans(variable_subset_obs, na.rm = TRUE)
    df$average_compliance_observed_ini_lag <- 1 - (df$average_extraction_observed_ini / 150)
  } else {
    df$average_compliance_observed_ini_lag <- NA
  }
  
  sem_model <- '
    belief_compliance_pm ~ survey2.1.player.confianza_caleta_conocida_mean + survey2.1.player.conflicto_caleta_conocida_mean
    belief_compliance_union ~ survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
    average_compliance_ini ~ belief_compliance_pm + belief_compliance_union + survey2.1.player.confianza_caleta_conocida_mean + survey2.1.player.conflicto_caleta_conocida_mean + survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta + average_compliance_observed_ini_lag
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_union
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_pm
  '
  node_labels <- c(
    "belief_compliance_pm" = "Prior Beliefs Outsiders",
    "belief_compliance_union" = "Prior Beliefs TURF",
    "average_compliance_ini" = "Compliance",
    "survey2.1.player.confianza_caleta_conocida_mean" = "Trust Outsiders",
    "survey2.1.player.conflicto_caleta_conocida_mean" = "Conflict Outsiders",
    "survey1.1.player.confianza_caleta" = "Trust TURF",
    "survey1.1.player.conflicto_caleta" = "Conflict TURF",
    "average_compliance_observed_ini_lag" = "Updated Beliefs"
  )
  
  fit <- sem(sem_model, data = df)
  node_names <- semPlot::semPlotModel(fit)@Vars$name
  cat("Node names detected for Shared Area T2 dynamic SEM labeling:\n")
  print(node_names)
  
  paths <- parameterEstimates(fit, standardized = TRUE)
  edge_colors <- ifelse(paths$pvalue[paths$op == "~"] < 0.05, "black", "transparent")
  
  output_file <- paste0(path_github, "Outputs/SEM_sharedarea_T2_dynamic_plot_Rounds_", R+8, "_to_", N+8, ".pdf")
  pdf(output_file, width = 12, height = 8)
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
    shapeMan = "rectangle",
    edge.color = edge_colors,
    residuals = FALSE,
    intercepts = FALSE,
    optimizeLatRes = TRUE,
    fade = FALSE
  )
  title(main = paste("Shared Area (Unknow Outsiders) Rounds", R+8, "to", N+8), line = 2, cex.main = 1.5)
  dev.off()
}

# Usage:
save_sharedarea_t2_dynamic_sem_plot(df, 2, 8, path_github)




