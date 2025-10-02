



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


############### functions

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
set.seed(5326)

save_sem_plot <- function(df, R, N, path_github) {
  cols <- get_columns_by_round("T1juegoalgas", "T1_extraccion_amerb", R, N)
  df$average_extraction_ini <- rowMeans(df[, cols, drop = FALSE], na.rm = TRUE)
  df$average_compliance_ini <- 1 - (df$average_extraction_ini / 50)
  df$belief_compliance_amerb <- 1 - (df$beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini / 50)
  df$Trust_Amerb_Union<-(df$survey1.1.player.confianza_caleta-1)/3
  df$Conflict_Amerb_Union<-(df$survey1.1.player.conflicto_caleta-1)/3
  
  
  sem_model <- 'belief_compliance_amerb ~ Trust_Amerb_Union + Conflict_Amerb_Union
                average_compliance_ini ~ belief_compliance_amerb + Trust_Amerb_Union + Conflict_Amerb_Union'
  
  node_labels <- c(
    "belief_compliance_amerb" = "Prior Beliefs Union",
    "average_compliance_ini" = "Compliance",
    "Trust_Amerb_Union" = "Trust Union",
    "Conflict_Amerb_Union" = "Conflict Union"
  )
  
  fit <- sem(sem_model,
             data       = df,
             estimator  = "ML",
             se         = "bootstrap",
             bootstrap  = 2000,
             parallel   = "multicore",
             ncpus      = 4)  
  
  # Debug: print node names used in the semPaths graph
  node_names <- semPlot::semPlotModel(fit)@Vars$name
  cat("Node names detected for semPaths labeling:\n")
  print(node_names)
  
  paths <- parameterEstimates(fit, standardized = TRUE)
  edge_colors <- ifelse(paths$pvalue[paths$op == "~"] < 0.05, "black", "transparent")
  
  output_file <- paste0(path_github, "Outputs/SEM_Turf_T1_plot_Rounds_ClusterLag1_rescaled_", R, "_to_", N, ".pdf")
  pdf(output_file, width = 12, height = 8)
  
  semPaths(
    fit,
    what = "std",
    layout = "tree",
    #edge.label.cex = 1,
    nodeLabels = node_labels,
    sizeMan = 7,
    label.cex = 1,
    node.width = 2,
    #node.height = 1,
    shapeMan = "rectangle",
    edge.color = edge_colors,
    residuals = FALSE,
    intercepts = FALSE,
    optimizeLatRes = TRUE,
    fade = FALSE,
    nCharNodes = 25,
    mar = c(6, 6, 7, 6)
  )
  
  title(main = paste("Mean Compliance TURF Round ", R), line = 2, cex.main = 1.5)
  dev.off()
}


# Usage:
#save_sem_plot(df, 1, 1, path_github)

#############################
### Turf rounds 2-8 T1
#############################
set.seed(5326)
save_dynamic_sem_plot <- function(df, R, N, path_github) {
  # 1. Compute average extraction and compliance for the selected rounds
  cols <- get_columns_by_round("T1juegoalgas", "T1_extraccion_amerb", R, N)
  variable_subset <- df[, cols, drop = FALSE]
  df$average_extraction_ini <- rowMeans(variable_subset, na.rm = TRUE)
  df$average_compliance_ini <- 1 - (df$average_extraction_ini / 50)
  df$belief_compliance_amerb <- 1 - (df$beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini / 50)
  df$Trust_Amerb_Union<-(df$survey1.1.player.confianza_caleta-1)/3
  df$Conflict_Amerb_Union<-(df$survey1.1.player.conflicto_caleta-1)/3

  
    # 2. Compute observed compliance in previous rounds (lag)
  # Careful: if R == 1, this will fail; you may want to check for valid lag
  if (R > 1) {
    cols_obs <- get_columns_by_round("T1juegoalgas", "T1_extraccion_otros_amerb", 1, R - 1)   
    variable_subset_obs <- df[, cols_obs, drop = FALSE]
    df$average_extraction_observed_ini <- rowMeans(variable_subset_obs, na.rm = TRUE)
    df$average_compliance_observed_ini_lag <- 1 - (df$average_extraction_observed_ini / 150)
  } else {
    df$average_compliance_observed_ini_lag <- NA  # Or 0, but NA is safest for modeling
  }
  
  # 3. SEM model: now includes the lagged compliance
  sem_model <- '
    belief_compliance_amerb ~ Trust_Amerb_Union + Conflict_Amerb_Union
    average_compliance_ini ~ belief_compliance_amerb + Trust_Amerb_Union + Conflict_Amerb_Union + average_compliance_observed_ini_lag
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_amerb
  '
  
  node_labels <- c(
    "belief_compliance_amerb" = "Prior Beliefs Union",
    "average_compliance_ini" = "Compliance",
    "Trust_Amerb_Union" = "Trust Union",
    "Conflict_Amerb_Union" = "Conflict Union",
    "average_compliance_observed_ini_lag" = "Observed Compliance"
  )
  
  
  fit <- sem(sem_model,
             data       = df,
             estimator  = "ML",
             se         = "bootstrap",
             bootstrap  = 2000,
             parallel   = "multicore",
             ncpus      = 4)
  
  # Debug: print node names for this dynamic model
  node_names <- semPlot::semPlotModel(fit)@Vars$name
  cat("Node names detected for dynamic SEM labeling:\n")
  print(node_names)
  
  paths <- parameterEstimates(fit, standardized = TRUE)
  edge_colors <- ifelse(paths$pvalue[paths$op == "~"] < 0.05, "black", "transparent")
  
  # Save to PDF (rectangle shape)
  output_file <- paste0(path_github, "Outputs/SEM_Turf_T1_dynamic_plot_Rounds_ClusterLag1_rescaled_", R, "_to_", N, ".pdf")
  pdf(output_file, width = 12, height = 8)
  semPaths(
    fit,
    what = "std",
    layout = "tree",
    #edge.label.cex = 1,
    nodeLabels = node_labels,
    sizeMan = 7,
    label.cex = 1,
    node.width = 2,
    #node.height = 1,
    shapeMan = "rectangle",
    edge.color = edge_colors,
    residuals = FALSE,
    intercepts = FALSE,
    optimizeLatRes = TRUE,
    fade = FALSE,
    nCharNodes = 25,
    mar = c(6, 6, 7, 6)
  )
  
  title(main = paste("Mean Compliance TURF Rounds", R, "to", N), line = 2, cex.main = 1.5)
  dev.off()
}
#save_dynamic_sem_plot(df, 2, 8, path_github)

#####################
### Turf Round 1 - T2
###################
set.seed(5326)
save_t2_sem_plot <- function(df, R, N, path_github) {
  cols <- get_columns_by_round("T2juegoalgas", "T2_extraccion_amerb", R, N)
  df$average_extraction_ini <- rowMeans(df[, cols, drop = FALSE], na.rm = TRUE)
  df$average_compliance_ini <- 1 - (df$average_extraction_ini / 50)
  df$belief_compliance_amerb <- 1 - (df$beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini / 50)
  df$Trust_Amerb_Union<-(df$survey1.1.player.confianza_caleta-1)/3
  df$Conflict_Amerb_Union<-(df$survey1.1.player.conflicto_caleta-1)/3
  
  sem_model <- 'belief_compliance_amerb ~ Trust_Amerb_Union + Conflict_Amerb_Union
                average_compliance_ini ~ belief_compliance_amerb + Trust_Amerb_Union + Conflict_Amerb_Union'
  
  node_labels <- c(
    "belief_compliance_amerb" = "Prior Beliefs Union",
    "average_compliance_ini" = "Compliance",
    "Trust_Amerb_Union" = "Trust Union",
    "Conflict_Amerb_Union" = "Conflict Union"
  )
  
  
  fit <- sem(sem_model,
             data       = df,
             estimator  = "ML",
             se         = "bootstrap",
             bootstrap  = 2000,
             parallel   = "multicore",
             ncpus      = 4)
  
  # Debug: print node names used in the semPaths graph
  node_names <- semPlot::semPlotModel(fit)@Vars$name
  cat("Node names detected for semPaths labeling:\n")
  print(node_names)
  
  paths <- parameterEstimates(fit, standardized = TRUE)
  edge_colors <- ifelse(paths$pvalue[paths$op == "~"] < 0.05, "black", "transparent")
  
  output_file <- paste0(path_github, "Outputs/SEM_Turf_T2_plot_Rounds_ClusterLag1_rescaled_", R+8, "_to_", N+8, ".pdf")
  pdf(output_file, width = 12, height = 8)
  
  semPaths(
    fit,
    what = "std",
    layout = "tree",
    #edge.label.cex = 1,
    nodeLabels = node_labels,
    sizeMan = 7,
    label.cex = 1,
    node.width = 2,
    #node.height = 1,
    shapeMan = "rectangle",
    edge.color = edge_colors,
    residuals = FALSE,
    intercepts = FALSE,
    optimizeLatRes = TRUE,
    fade = FALSE,
    nCharNodes = 25,
    mar = c(6, 6, 7, 6)
  )
  
  title(main = paste("Mean Compliance TURF Round", R+8), line = 2, cex.main = 1.5)
  dev.off()
}


# Usage:
#save_t2_sem_plot(df, 1, 1, path_github)

#############################
### Turf rounds 2-8 T2
#############################
set.seed(5326)

save_dynamic_t2_sem_plot <- function(df, R, N, path_github) {
  # 1. Compute average extraction and compliance for the selected rounds
  cols <- get_columns_by_round("T2juegoalgas", "T2_extraccion_amerb", R, N)
  variable_subset <- df[, cols, drop = FALSE]
  df$average_extraction_ini <- rowMeans(variable_subset, na.rm = TRUE)
  df$average_compliance_ini <- 1 - (df$average_extraction_ini / 50)
  df$belief_compliance_amerb <- 1 - (df$beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini / 50)
  df$Trust_Amerb_Union<-(df$survey1.1.player.confianza_caleta-1)/3
  df$Conflict_Amerb_Union<-(df$survey1.1.player.conflicto_caleta-1)/3
  
  # 2. Compute observed compliance in previous rounds (lag)
  # Careful: if R == 1, this will fail; you may want to check for valid lag
  if (R > 1) {
    cols_obs <- get_columns_by_round("T2juegoalgas", "T2_extraccion_otros_amerb", 1, R - 1)   
    variable_subset_obs <- df[, cols_obs, drop = FALSE]
    df$average_extraction_observed_ini <- rowMeans(variable_subset_obs, na.rm = TRUE)
    df$average_compliance_observed_ini_lag <- 1 - (df$average_extraction_observed_ini / 150)
  } else {
    df$average_compliance_observed_ini_lag <- NA  # Or 0, but NA is safest for modeling
  }
  
   # 3. SEM model: now includes the lagged compliance
  sem_model <- '
    belief_compliance_amerb ~ sTrust_Amerb_Union + Conflict_Amerb_Union
    average_compliance_ini ~ belief_compliance_amerb + Trust_Amerb_Union + Conflict_Amerb_Union + average_compliance_observed_ini_lag
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_amerb
  '
  
  node_labels <- c(
    "belief_compliance_amerb" = "Prior Beliefs Union",
    "average_compliance_ini" = "Compliance",
    "Trust_Amerb_Union" = "Trust Union",
    "Conflict_Amerb_Union" = "Conflict Union",
    "average_compliance_observed_ini_lag" = "Observed Compliance"
  )
  
  
  fit <- sem(sem_model,
             data       = df,
             estimator  = "ML",
             se         = "bootstrap",
             bootstrap  = 2000,
             parallel   = "multicore",
             ncpus      = 4)
  
  # Debug: print node names for this dynamic model
  node_names <- semPlot::semPlotModel(fit)@Vars$name
  cat("Node names detected for dynamic SEM labeling:\n")
  print(node_names)
  
  paths <- parameterEstimates(fit, standardized = TRUE)
  edge_colors <- ifelse(paths$pvalue[paths$op == "~"] < 0.05, "black", "transparent")
  
  # Save to PDF (rectangle shape)
  output_file <- paste0(path_github, "Outputs/SEM_Turf_T2_dynamic_plot_Rounds_ClusterLag1_rescaled_", R+8, "_to_", N+8, ".pdf")
  pdf(output_file, width = 12, height = 8)
  semPaths(
    fit,
    what = "std",
    layout = "tree",
    #edge.label.cex = 1,
    nodeLabels = node_labels,
    sizeMan = 7,
    label.cex = 1,
    node.width = 2,
    #node.height = 1,
    shapeMan = "rectangle",
    edge.color = edge_colors,
    residuals = FALSE,
    intercepts = FALSE,
    optimizeLatRes = TRUE,
    fade = FALSE,
    nCharNodes = 25,
    mar = c(6, 6, 7, 6)
  )
  
  title(main = paste("Mean Compliance TURF Rounds", R+8, "to", N+8), line = 2, cex.main = 1.5)
  dev.off()
}

#save_dynamic_t2_sem_plot(df, 2, 8, path_github)
##############################
### Shared Area Round 1 - T1
##############################
set.seed(5326)
save_sharedarea_sem_plot <- function(df, R, N, path_github) {
  cols <- get_columns_by_round("T1juegoalgas", "T1_extraccion_libre", R, N)
  df$average_extraction_ini <- df[, cols, drop = TRUE]
  df$average_compliance_ini <- 1 - (df$average_extraction_ini / 50)
  df$belief_compliance_pm <- 1 - (df$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50)
  df$belief_compliance_union <- 1 - (df$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50)
  df$Trust_SA_Outgroup<-(df$survey1.1.player.confianza_pm -1)/3
  df$Conflict_SA_Outgroup<-(df$survey1.1.player.conflicto_pm -1)/3
  df$Trust_SA_Ingroup<-(df$survey1.1.player.confianza_caleta -1)/3
  df$Conflict_SA_Ingroup<-(df$survey1.1.player.conflicto_caleta -1)/3
  
  sem_model <- '
    belief_compliance_pm ~ Trust_SA_Outgroup + Conflict_SA_Outgroup
    belief_compliance_union ~ Trust_SA_Ingroup + Conflict_SA_Ingroup
    average_compliance_ini ~ belief_compliance_pm + belief_compliance_union + Trust_SA_Outgroup + Conflict_SA_Outgroup + Trust_SA_Ingroup + Conflict_SA_Ingroup
    Trust_SA_Outgroup ~~ 0*Conflict_SA_Outgroup
    Trust_SA_Outgroup ~~ 0*Trust_SA_Ingroup
    Trust_SA_Outgroup ~~ 0*Conflict_SA_Ingroup
    Conflict_SA_Outgroup ~~ 0*Trust_SA_Ingroup
    Conflict_SA_Outgroup ~~ 0*Conflict_SA_Ingroup
    Trust_SA_Ingroup ~~ 0*Conflict_SA_Ingroup
  '
  node_labels <- c(
    "belief_compliance_pm" = "Prior Beliefs Out-group",
    "belief_compliance_union" = "Prior Beliefs Union",
    "average_compliance_ini" = "Compliance",
    "Trust_SA_Outgroup" = "Trust Out-group",
    "Conflict_SA_Outgroup" = "Conflict Out-group",
    "Trust_SA_Ingroup" = "Trust Union",
    "Conflict_SA_Ingroup" = "Conflict Union"
  )
  
  fit <- sem(sem_model,
             data       = df,
             estimator  = "ML",
             se         = "bootstrap",
             bootstrap  = 2000,
             parallel   = "multicore",
             ncpus      = 4)
  
  node_names <- semPlot::semPlotModel(fit)@Vars$name
  cat("Node names detected for Shared Area static SEM labeling:\n")
  print(node_names)
  
  paths <- parameterEstimates(fit, standardized = TRUE)
  edge_colors <- ifelse(paths$pvalue[paths$op == "~"] < 0.05, "black", "transparent")
  
  output_file <- paste0(path_github, "Outputs/SEM_sharedarea_T1_plot_Rounds_ClusterLag1_rescaled_", R, "_to_", N, ".pdf")
  pdf(output_file, width = 12, height = 8)
  semPaths(
    fit,
    what = "std",
    layout = "tree",
    #edge.label.cex = 1,
    nodeLabels = node_labels,
    sizeMan = 7,
    label.cex = 1,
    node.width = 2,
    #node.height = 1,
    shapeMan = "rectangle",
    edge.color = edge_colors,
    residuals = FALSE,
    intercepts = FALSE,
    optimizeLatRes = TRUE,
    fade = FALSE,
    nCharNodes = 25,
    mar = c(6, 6, 7, 6)
  )
  title(main = paste("Shared Area (Unknow Out-group) Round", R), line = 2, cex.main = 1.5)
  dev.off()
}

# Usage:
#save_sharedarea_sem_plot(df, 1, 1, path_github)

##############################
### Shared Area Round 2-8 - T1
##############################
set.seed(5326)
save_sharedarea_dynamic_sem_plot <- function(df, R, N, path_github) {
  cols <- get_columns_by_round("T1juegoalgas", "T1_extraccion_libre", R, N)
  variable_subset <- df[, cols, drop = FALSE]
  df$average_extraction_ini <- rowMeans(variable_subset, na.rm = TRUE)
  df$average_compliance_ini <- 1 - (df$average_extraction_ini / 50)
  df$belief_compliance_pm <- 1 - (df$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50)
  df$belief_compliance_union <- 1 - (df$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50)
  df$Trust_SA_Outgroup<-(df$survey1.1.player.confianza_pm -1)/3
  df$Conflict_SA_Outgroup<-(df$survey1.1.player.conflicto_pm -1)/3
  df$Trust_SA_Ingroup<-(df$survey1.1.player.confianza_caleta -1)/3
  df$Conflict_SA_Ingroup<-(df$survey1.1.player.conflicto_caleta -1)/3
  
  if (R > 1) {
    cols_obs <- get_columns_by_round("T1juegoalgas", "T1_extraccion_otros_libre", 1, R - 1)   
    variable_subset_obs <- df[, cols_obs, drop = FALSE]
    df$average_extraction_observed_ini <- rowMeans(variable_subset_obs, na.rm = TRUE)
    df$average_compliance_observed_ini_lag <- 1 - (df$average_extraction_observed_ini / 150)
  } else {
    df$average_compliance_observed_ini_lag <- NA
  }
  
  sem_model <- '
    belief_compliance_pm ~ Trust_SA_Outgroup + Conflict_SA_Outgroup
    belief_compliance_union ~ Trust_SA_Ingroup + Conflict_SA_Ingroup
    average_compliance_ini ~ belief_compliance_pm + belief_compliance_union +Trust_SA_Outgroup + Conflict_SA_Outgroup + Trust_SA_Ingroup + Conflict_SA_Ingroup + average_compliance_observed_ini_lag
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_union
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_pm
    Trust_SA_Outgroup ~~ 0*Conflict_SA_Outgroup
    Trust_SA_Outgroup ~~ 0*Trust_SA_Ingroup
    Trust_SA_Outgroup ~~ 0*Conflict_SA_Ingroup
    Conflict_SA_Outgroup ~~ 0*Trust_SA_Ingroup
    Conflict_SA_Outgroup ~~ 0*Conflict_SA_Ingroup
    Trust_SA_Ingroup ~~ 0*Conflict_SA_Ingroup
  '
  node_labels <- c(
    "belief_compliance_pm" = "Prior Beliefs Out-group",
    "belief_compliance_union" = "Prior Beliefs Union",
    "average_compliance_ini" = "Compliance",
    "Trust_SA_Outgroup" = "Trust Out-group",
    "Conflict_SA_Outgroup" = "Conflict Out-group",
    "Trust_SA_Ingroup" = "Trust Union",
    "Conflict_SA_Ingroup" = "Conflict Union",
    "average_compliance_observed_ini_lag" = "Observed Compliance"
  )
  
  fit <- sem(sem_model,
             data       = df,
             estimator  = "ML",
             se         = "bootstrap",
             bootstrap  = 2000,
             parallel   = "multicore",
             ncpus      = 4)
  
  node_names <- semPlot::semPlotModel(fit)@Vars$name
  cat("Node names detected for Shared Area dynamic SEM labeling:\n")
  print(node_names)
  
  paths <- parameterEstimates(fit, standardized = TRUE)
  edge_colors <- ifelse(paths$pvalue[paths$op == "~"] < 0.05, "black", "transparent")
  
  output_file <- paste0(path_github, "Outputs/SEM_sharedarea_T1_dynamic_plot_Rounds_ClusterLag1_rescaled_", R, "_to_", N, ".pdf")
  pdf(output_file, width = 12, height = 8)
  semPaths(
    fit,
    what = "std",
    layout = "tree",
    #edge.label.cex = 1,
    nodeLabels = node_labels,
    sizeMan = 7,
    label.cex = 1,
    node.width = 2,
    #node.height = 1,
    shapeMan = "rectangle",
    edge.color = edge_colors,
    residuals = FALSE,
    intercepts = FALSE,
    optimizeLatRes = TRUE,
    fade = FALSE,
    nCharNodes = 25,
    mar = c(6, 6, 7, 6)
  )
  title(main = paste("Shared Area (Unknow Out-group) Mean Rounds", R, "to", N), line = 2, cex.main = 1.5)
  dev.off()
}

# Usage:
#save_sharedarea_dynamic_sem_plot(df, 2, 8, path_github)





##############################
### Shared Area Round 1 - T2
#############################
set.seed(5326)
save_sharedarea_t2_sem_plot <- function(df, R, N, path_github) {
  cols <- get_columns_by_round("T2juegoalgas", "T2_extraccion_metat", R, N)
  df$average_extraction_ini <- df[, cols, drop = TRUE]
  df$average_compliance_ini <- 1 - (df$average_extraction_ini / 50)
  df$belief_compliance_pm <- 1 - (df$beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini / 50)
  df$belief_compliance_union <- 1 - (df$beliefsT2inicial.1.player.T2_belief_caleta_ini / 50)
  df$Trust_SA_Outgroup<-(df$survey2.1.player.confianza_caleta_conocida_mean -1)/3 
  df$Conflict_SA_Outgroup<-(df$survey2.1.player.conflicto_caleta_conocida_mean -1)/3
  df$Trust_SA_Ingroup<-(df$survey1.1.player.confianza_caleta -1)/3
  df$Conflict_SA_Ingroup<-(df$survey1.1.player.conflicto_caleta -1)/3
  
  sem_model <- '
    belief_compliance_pm ~ Trust_SA_Outgroup + Conflict_SA_Outgroup
    belief_compliance_union ~ Trust_SA_Ingroup + Conflict_SA_Ingroup
    average_compliance_ini ~ belief_compliance_pm + belief_compliance_union + Trust_SA_Outgroup + Conflict_SA_Outgroup + Trust_SA_Ingroup + Conflict_SA_Ingroup
    Trust_SA_Outgroup ~~ 0*Conflict_SA_Outgroup
    Trust_SA_Outgroup ~~ 0*Trust_SA_Ingroup
    Trust_SA_Outgroup ~~ 0*Conflict_SA_Ingroup
    Conflict_SA_Outgroup ~~ 0*Trust_SA_Ingroup
    Conflict_SA_Outgroup ~~ 0*Conflict_SA_Ingroup
    Trust_SA_Ingroup ~~ 0*Conflict_SA_Ingroup
  '
  node_labels <- c(
    "belief_compliance_pm" = "Prior Beliefs Out-group",
    "belief_compliance_union" = "Prior Beliefs Union",
    "average_compliance_ini" = "Compliance",
    "Trust_SA_Outgroup" = "Trust Out-group",
    "Conflict_SA_Outgroup" = "Conflict Out-group",
    "Trust_SA_Ingroup" = "Trust Union",
    "Conflict_SA_Ingroup" = "Conflict Union"
  )
  
  fit <- sem(sem_model,
             data       = df,
             estimator  = "ML",
             se         = "bootstrap",
             bootstrap  = 2000,
             parallel   = "multicore",
             ncpus      = 4)
  
  node_names <- semPlot::semPlotModel(fit)@Vars$name
  cat("Node names detected for Shared Area T2 static SEM labeling:\n")
  print(node_names)
  
  paths <- parameterEstimates(fit, standardized = TRUE)
  edge_colors <- ifelse(paths$pvalue[paths$op == "~"] < 0.05, "black", "transparent")
  
  output_file <- paste0(path_github, "Outputs/SEM_sharedarea_T2_plot_Rounds_ClusterLag1_rescaled_", R+8, "_to_", N+8, ".pdf")
  pdf(output_file, width = 12, height = 8)
  semPaths(
    fit,
    what = "std",
    layout = "tree",
    #edge.label.cex = 1,
    nodeLabels = node_labels,
    sizeMan = 7,
    label.cex = 1,
    node.width = 2,
    #node.height = 1,
    shapeMan = "rectangle",
    edge.color = edge_colors,
    residuals = FALSE,
    intercepts = FALSE,
    optimizeLatRes = TRUE,
    fade = FALSE,
    nCharNodes = 25,
    mar = c(6, 6, 7, 6)
  )
  title(main = paste("Shared Area (Know Out-group) Round", R+8), line = 2, cex.main = 1.5)
  dev.off()
}

# Usage:
#save_sharedarea_t2_sem_plot(df, 1, 1, path_github)




##############################
### Shared Area Round 2-8 - T2
#############################
set.seed(5326)
save_sharedarea_t2_dynamic_sem_plot <- function(df, R, N, path_github) {
  cols <- get_columns_by_round("T2juegoalgas", "T2_extraccion_metat", R, N)
  variable_subset <- df[, cols, drop = FALSE]
  df$average_extraction_ini <- rowMeans(variable_subset, na.rm = TRUE)
  df$average_compliance_ini <- 1 - (df$average_extraction_ini / 50)
  df$belief_compliance_pm <- 1 - (df$beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini / 50)
  df$belief_compliance_union <- 1 - (df$beliefsT2inicial.1.player.T2_belief_caleta_ini / 50)
  df$Trust_SA_Outgroup<-(df$survey2.1.player.confianza_caleta_conocida_mean -1)/3 
  df$Conflict_SA_Outgroup<-(df$survey2.1.player.conflicto_caleta_conocida_mean -1)/3
  df$Trust_SA_Ingroup<-(df$survey1.1.player.confianza_caleta -1)/3
  df$Conflict_SA_Ingroup<-(df$survey1.1.player.conflicto_caleta -1)/3
  
  if (R > 1) {
    cols_obs <- get_columns_by_round("T2juegoalgas", "T2_extraccion_otros_metat", 1, R - 1)   
    variable_subset_obs <- df[, cols_obs, drop = FALSE]
    df$average_extraction_observed_ini <- rowMeans(variable_subset_obs, na.rm = TRUE)
    df$average_compliance_observed_ini_lag <- 1 - (df$average_extraction_observed_ini / 150)
  } else {
    df$average_compliance_observed_ini_lag <- NA
  }
  
  sem_model <- '
    belief_compliance_pm ~ Trust_SA_Outgroup + Conflict_SA_Outgroup
    belief_compliance_union ~ Trust_SA_Ingroup + Conflict_SA_Ingroup
    average_compliance_ini ~ belief_compliance_pm + belief_compliance_union + Trust_SA_Outgroup + Conflict_SA_Outgroup + Trust_SA_Ingroup + Conflict_SA_Ingroup + average_compliance_observed_ini_lag
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_union
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_pm
    Trust_SA_Outgroup ~~ 0*Conflict_SA_Outgroup
    Trust_SA_Outgroup ~~ 0*Trust_SA_Ingroup
    Trust_SA_Outgroup ~~ 0*Conflict_SA_Ingroup
    Conflict_SA_Outgroup ~~ 0*Trust_SA_Ingroup
    Conflict_SA_Outgroup ~~ 0*Conflict_SA_Ingroup
    Trust_SA_Ingroup ~~ 0*Conflict_SA_Ingroup
  '
  node_labels <- c(
    "belief_compliance_pm"                = expression(PB[italic(out) ~ "-" ~ italic(group)]),
    "belief_compliance_union"             = expression(PB[italic("in")  ~ "-" ~ italic(group)]),
    "average_compliance_ini"              = expression(bar(C)[italic(i)]),
    "Trust_SA_Outgroup"                   = expression(Tst[italic(out) ~ "-" ~ italic(group)]),
    "Conflict_SA_Outgroup"                = expression(Cft[italic(out) ~ "-" ~ italic(group)]),
    "Trust_SA_Ingroup"                    = expression(Tst[italic("in")  ~ "-" ~ italic(group)]),
    "Conflict_SA_Ingroup"                 = expression(Cft[italic("in")  ~ "-" ~ italic(group)]),
    "average_compliance_observed_ini_lag" = expression(bar(C)[italic(list(-i, t - 1))])
  )
  
  
  fit <- sem(sem_model,
             data       = df,
             estimator  = "ML",
             se         = "bootstrap",
             bootstrap  = 100,
             parallel   = "multicore",
             ncpus      = 4,
             orthogonal = TRUE)
  
  node_names <- semPlot::semPlotModel(fit)@Vars$name
  cat("Node names detected for Shared Area T2 dynamic SEM labeling:\n")
  print(node_names)
  print(summary(fit))
  
  paths <- parameterEstimates(fit, standardized = TRUE)
  edge_colors <- ifelse(paths$pvalue[paths$op == "~"] < 0.05, "black", "transparent")
  
  output_file <- paste0(path_github, "Outputs/SEM_sharedarea_T2_dynamic_plot_Rounds_ClusterLag1_rescaled_", R+8, "_to_", N+8, ".pdf")
  pdf(output_file, width = 12, height = 8)
  semPaths(
    fit,
    what = "std",
    layout = "tree",
    edge.label.cex = 1,
    nodeLabels = node_labels,
    sizeMan = 7,
    label.cex = 1,
    node.width = 2,
    #node.height = 1,
    shapeMan = "rectangle",
    edge.color = edge_colors,
    residuals = FALSE,
    intercepts = FALSE,
    optimizeLatRes = TRUE,
    fade = FALSE,
    nCharNodes = 25,
    mar = c(6, 6, 7, 6)
   
  )
  title(main = paste("Shared Area (Know Out-group) Mean Rounds", R+8, "to", N+8), line = 2, cex.main = 1.5)
  dev.off()
}

# Usage:
save_sharedarea_t2_dynamic_sem_plot(df, 2, 8, path_github)

#################
### Plot Figures
#################

#Turf
#T1
save_sem_plot(df, 1, 1, path_github)
save_dynamic_sem_plot(df, 2, 8, path_github)
#T2
save_t2_sem_plot(df, 1, 1, path_github)
save_dynamic_t2_sem_plot(df, 2, 8, path_github)

# Shared Area
#T1
save_sharedarea_sem_plot(df, 1, 1, path_github)
save_sharedarea_dynamic_sem_plot(df,2, 8, path_github)
#T2
save_sharedarea_t2_sem_plot(df, 1, 1, path_github)
save_sharedarea_t2_dynamic_sem_plot(df, 2, 8, path_github)



