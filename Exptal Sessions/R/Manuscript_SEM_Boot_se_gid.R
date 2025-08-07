##############################
### Shared Area Round 1 - T1 (Manual Cluster Bootstrap)
##############################

save_sharedarea_sem_plot <- function(df, R, N, path_github, B = 200) {
  # compute averages and compliances
  cols <- get_columns_by_round("T1juegoalgas", "T1_extraccion_libre", R, N)
  df$average_extraction_ini  <- rowMeans(df[, cols, drop = FALSE], na.rm = TRUE)
  df$average_compliance_ini  <- 1 - df$average_extraction_ini / 50
  df$belief_compliance_pm    <- 1 - df$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50
  df$belief_compliance_union <- 1 - df$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50
  
  # SEM model specification
  sem_model <- '
    belief_compliance_pm    ~ survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm
    belief_compliance_union ~ survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
    average_compliance_ini  ~ belief_compliance_pm + belief_compliance_union + 
                              survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm + 
                              survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
  '
  
  # Fit initial model without bootstrap
  fit_orig <- sem(sem_model,
                  data      = df,
                  estimator = "ML",
                  se        = "none")
  
  # Manual cluster bootstrap
  clusters    <- unique(df$gid.treat)
  nclust      <- length(clusters)
  pe_orig     <- parameterEstimates(fit_orig)
  param_index <- which(pe_orig$op == "~")
  npars       <- length(param_index)
  boot_mat    <- matrix(NA, nrow = B, ncol = npars)
  
  for (b in seq_len(B)) {
    samp_clust    <- sample(clusters, size = nclust, replace = TRUE)
    df_b          <- do.call(rbind, lapply(samp_clust, function(cl) df[df$gid.treat == cl, ]))
    fit_b         <- sem(sem_model, data = df_b, estimator = "ML", se = "none")
    pe_b          <- parameterEstimates(fit_b)
    boot_mat[b, ] <- pe_b$est[param_index]
  }
  
  # Compute bootstrap SEs and p-values
  bs_se       <- apply(boot_mat, 2, sd)
  new_se      <- rep(NA, nrow(pe_orig))
  new_pvalue  <- rep(NA, nrow(pe_orig))
  new_se[param_index]    <- bs_se
  new_pvalue[param_index]<- 2 * pnorm(-abs(pe_orig$est[param_index] / bs_se))
  pe_orig$se     <- new_se
  pe_orig$pvalue <- new_pvalue
  
  # Prepare edge aesthetics directly
  edge_labs   <- round(pe_orig$est[param_index], 2)
  edge_widths <- ifelse(pe_orig$pvalue[param_index] < .05, 2, 1)
  edge_colors <- ifelse(pe_orig$pvalue[param_index] < .05, "black", "transparent")
  
  # Plot the SEM diagram with manual bootstrap aesthetics
  output_file <- paste0(path_github, "Outputs/SEM_sharedarea_T1_cluster_bootstrap_R", R, ".pdf")
  pdf(output_file, width = 12, height = 8)
  semPaths(
    fit_orig,
    whatLabels  = "std",
    layout      = "tree",
    nodeLabels  = c(
      "belief_compliance_pm"     = "Prior Beliefs Outsiders",
      "belief_compliance_union"  = "Prior Beliefs TURF",
      "average_compliance_ini"    = "Compliance",
      "survey1.1.player.confianza_pm"     = "Trust Outsiders",
      "survey1.1.player.conflicto_pm"     = "Conflict Outsiders",
      "survey1.1.player.confianza_caleta" = "Trust TURF",
      "survey1.1.player.conflicto_caleta" = "Conflict TURF"
    ),
    shapeMan     = "rectangle",
    sizeMan      = 8,
    node.width   = 3,
    node.height  = 1.2,
    edgeLabels   = edge_labs,
    asize        = edge_widths,
    edge.color   = edge_colors,
    label.cex    = 1,
    residuals    = FALSE,
    intercepts   = FALSE
  )
  title(main = paste0("Shared Area Cluster‐Bootstrap Round ", R), line = 2, cex.main = 1.5)
  dev.off()
  
  # Return updated parameter table
  return(pe_orig)
}

save_sharedarea_sem_plot(df, 1, 1, path_github)



############################
### Shared Area dynamic (Manual Cluster Bootstrap
#############################


save_sharedarea_dynamic_sem_plot <- function(df, R, N, path_github, B = 100) {
  # compute averages and compliance
  cols <- get_columns_by_round("T1juegoalgas", "T1_extraccion_libre", R, N)
  subset_ini <- df[, cols, drop = FALSE]
  df$average_extraction_ini   <- rowMeans(subset_ini, na.rm = TRUE)
  df$average_compliance_ini   <- 1 - df$average_extraction_ini / 50
  df$belief_compliance_pm     <- 1 - df$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50
  df$belief_compliance_union  <- 1 - df$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50
  if (R > 1) {
    cols_obs <- get_columns_by_round("T1juegoalgas", "T1_extraccion_otros_libre", 1, R - 1)
    subset_obs <- df[, cols_obs, drop = FALSE]
    df$average_extraction_observed_ini     <- rowMeans(subset_obs, na.rm = TRUE)
    df$average_compliance_observed_ini_lag <- 1 - df$average_extraction_observed_ini / (50 * length(cols_obs))
  } else {
    df$average_compliance_observed_ini_lag <- NA
  }
  
  # SEM model
  sem_model <- '
    belief_compliance_pm    ~ survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm
    belief_compliance_union ~ survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
    average_compliance_ini  ~ belief_compliance_pm + belief_compliance_union + survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm + survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta + average_compliance_observed_ini_lag
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_union
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_pm
  '
  
  # initial fit
  fit_orig <- sem(sem_model, data = df, estimator = "ML", se = "none")
  
  # manual cluster bootstrap
  clusters  <- unique(df$gid.treat)
  nclust    <- length(clusters)
  pe_orig   <- parameterEstimates(fit_orig)
  param_idx <- which(pe_orig$op == "~")
  npars     <- length(param_idx)
  boot_mat  <- matrix(NA, nrow = B, ncol = npars)
  for(b in seq_len(B)){
    samp    <- sample(clusters, nclust, replace = TRUE)
    df_b    <- do.call(rbind, lapply(samp, function(cl) df[df$gid.treat==cl, ]))
    fit_b   <- sem(sem_model, data = df_b, estimator = "ML", se = "none")
    pe_b    <- parameterEstimates(fit_b)
    boot_mat[b,] <- pe_b$est[param_idx]
  }
  bs_se       <- apply(boot_mat, 2, sd)
  new_se      <- rep(NA, nrow(pe_orig))
  new_pvals   <- rep(NA, nrow(pe_orig))
  new_se[param_idx]    <- bs_se
  new_pvals[param_idx] <- 2 * pnorm(-abs(pe_orig$est[param_idx]/bs_se))
  pe_orig$se     <- new_se
  pe_orig$pvalue <- new_pvals
  
  # aesthetics
  edge_labs    <- round(pe_orig$est[param_idx], 2)
  edge_widths  <- ifelse(pe_orig$pvalue[param_idx]<.05, 2,1)
  edge_colors  <- ifelse(pe_orig$pvalue[param_idx]<.05, "black","transparent")
  
  # plot
  out <- paste0(path_github, "Outputs/SEM_sharedarea_T1_dynamic_cluster_R", R, "_to_", N, ".pdf")
  pdf(out, width=12, height=8)
  semPaths(
    fit_orig,
    whatLabels = "std",
    layout     = "tree",
    nodeLabels = c(
      "belief_compliance_pm"     = "Prior Beliefs Outsiders",
      "belief_compliance_union"  = "Prior Beliefs TURF",
      "average_compliance_ini"    = "Compliance",
      "survey1.1.player.confianza_pm"     = "Trust Outsiders",
      "survey1.1.player.conflicto_pm"     = "Conflict Outsiders",
      "survey1.1.player.confianza_caleta" = "Trust TURF",
      "survey1.1.player.conflicto_caleta" = "Conflict TURF",
      "average_compliance_observed_ini_lag" = "Updated Beliefs"
    ),
    shapeMan    = "rectangle",
    sizeMan     = 9,
    node.width  = 2,
    node.height = 0.7,
    edgeLabels  = edge_labs,
    asize       = edge_widths,
    edge.color  = edge_colors,
    label.cex   = 1,
    residuals   = FALSE,
    intercepts  = FALSE,
    optimizeLatRes = TRUE,
    fade        = FALSE
  )
  title(main = paste0("Dynamic Shared Area Cluster‐Bootstrap Rounds ", R, " to ", N), line=2, cex.main=1.5)
  dev.off()
  
  return(pe_orig)
}


save_sharedarea_dynamic_sem_plot(df,5, 8, path_github)