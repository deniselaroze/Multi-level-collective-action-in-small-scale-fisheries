# Load required packages
library(plm)
library(boot)
library(stargazer)

# Convert panel data correctly
dfs_long <- pdata.frame(dfs_long, index = c("participant.code", "round"))

# Define a function to resample clusters and fit a plm model
bootstrap_model <- function(data, indices, model_formula, cluster_var) {
  # Get unique clusters
  unique_clusters <- unique(data[[cluster_var]])
  
  # Sample clusters (with replacement)
  sampled_clusters <- sample(unique_clusters, length(unique_clusters), replace = TRUE)
  
  # Subset data based on sampled clusters
  data_sample <- data[data[[cluster_var]] %in% sampled_clusters, ]
  
  # Ensure the subset is also a pdata.frame
  data_sample <- pdata.frame(data_sample, index = c("participant.code", "round"))
  
  # Fit the model
  model <- plm(model_formula, data = data_sample, model = "random")
  
  # Return coefficients
  return(coef(model))
}

# Number of bootstrap replications
B <- 500 
set.seed(62354)

# Define model formulas
formula_plm1 <- compliance_extraction_amerb ~ compliance_lag_extraction_others_amerb_mean + 
  compliance_beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini + 
  treatment + n_identities

formula_plm2 <- compliance_extraction_amerb ~ compliance_lag_extraction_others_amerb_mean + 
  compliance_beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini + 
  treatment + n_identities + survey3.1.player.sexo + 
  survey3.1.player.horas_trabajo + survey3.1.player.estudios + 
  survey3.1.player.liderazgo

formula_plm3 <- compliance_extraction_OA ~ compliance_lag_extraction_others_OA_mean + 
  compliance_beliefs_OA_caleta + compliance_beliefs_OA_others + 
  treatment + n_identities + minority

formula_plm4 <- compliance_extraction_OA ~ compliance_lag_extraction_others_OA_mean + 
  compliance_beliefs_OA_caleta*treatment + compliance_beliefs_OA_others + 
  treatment + n_identities + minority 

formula_plm5 <- compliance_extraction_OA ~ compliance_lag_extraction_others_OA_mean + 
  compliance_beliefs_OA_caleta*treatment + compliance_beliefs_OA_others + 
  treatment + n_identities + minority + survey3.1.player.sexo + 
  survey3.1.player.horas_trabajo + survey3.1.player.estudios + 
  survey3.1.player.liderazgo

# Run bootstrap with correct cluster resampling
boot_se_plm1 <- boot(data = dfs_long, statistic = bootstrap_model, R = B, 
                     model_formula = formula_plm1, cluster_var = "gid.amerb")

boot_se_plm2 <- boot(data = dfs_long, statistic = bootstrap_model, R = B, 
                     model_formula = formula_plm2, cluster_var = "gid.amerb")

boot_se_plm3 <- boot(data = dfs_long, statistic = bootstrap_model, R = B, 
                     model_formula = formula_plm3, cluster_var = "gid.treat")

boot_se_plm4 <- boot(data = dfs_long, statistic = bootstrap_model, R = B, 
                     model_formula = formula_plm4, cluster_var = "gid.treat")

boot_se_plm5 <- boot(data = dfs_long, statistic = bootstrap_model, R = B, 
                     model_formula = formula_plm5, cluster_var = "gid.treat")

# Extract Bootstrapped SEs
se_list_boot <- list(apply(boot_se_plm1$t, 2, sd),
                     apply(boot_se_plm2$t, 2, sd),
                     apply(boot_se_plm3$t, 2, sd),
                     apply(boot_se_plm4$t, 2, sd),
                     apply(boot_se_plm5$t, 2, sd))

# Fit models on full dataset to get coefficient estimates
plm1 <- plm(formula_plm1, data = dfs_long, model = "random")
plm2 <- plm(formula_plm2, data = dfs_long, model = "random")
plm3 <- plm(formula_plm3, data = dfs_long, model = "random")
plm4 <- plm(formula_plm4, data = dfs_long, model = "random")
plm5 <- plm(formula_plm5, data = dfs_long, model = "random")

# Stargazer Table with Corrected Bootstrapped Standard Errors
stargazer(plm1, plm2, plm3, plm4, plm5,
          se = se_list_boot, 
          type = "html",  # Change to "html" or "latex" if needed
          title = "Random Effects Panel Models with Bootstrapped Standard Errors",
          dep.var.labels = c("Compliance Extraction Amerb", "Compliance Extraction OA"),
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
          star.cutoffs = c(0.1, 0.05, 0.01),
          notes = "Bootstrapped SEs based on 500 replications, resampling clusters (gid.amerb for M1 & M2, gid.treat for M3-M5).",
          out = paste0(path_github, "Outputs/compliance_extraction_OAandTurf_plm_boot_se.html")
)



###################
### "HC1" clusters
###################





# Load required packages
library(plm)
library(lmtest)
library(sandwich)  # Needed for robust SEs
library(stargazer)

# Define panel data
dfs_long <- pdata.frame(dfs_long, index = c("participant.code", "round"))

# Define models
plm1 <- plm(compliance_extraction_amerb ~ compliance_lag_extraction_others_amerb_mean + 
              compliance_beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini + 
              treatment + n_identities, 
            data = dfs_long, model = "random")

plm2 <- plm(compliance_extraction_amerb ~ compliance_lag_extraction_others_amerb_mean + 
              compliance_beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini + 
              treatment + n_identities + survey3.1.player.sexo + 
              survey3.1.player.horas_trabajo + survey3.1.player.estudios + 
              survey3.1.player.liderazgo, 
            data = dfs_long, model = "random")

plm3 <- plm(compliance_extraction_OA ~ compliance_lag_extraction_others_OA_mean + 
              compliance_beliefs_OA_caleta + compliance_beliefs_OA_others + 
              treatment + n_identities + minority, 
            data = dfs_long, model = "random")
plm4 <- plm(compliance_extraction_OA ~ compliance_lag_extraction_others_OA_mean + 
              compliance_beliefs_OA_caleta + compliance_beliefs_OA_others + 
              treatment + n_identities + minority, 
            data = dfs_long, model = "random")
plm5 <- plm(compliance_extraction_OA ~ compliance_lag_extraction_others_OA_mean + 
              compliance_beliefs_OA_caleta*treatment + compliance_beliefs_OA_others + 
              treatment + n_identities + minority + survey3.1.player.sexo + 
              survey3.1.player.horas_trabajo + survey3.1.player.estudios + 
              survey3.1.player.liderazgo, 
            data = dfs_long, model = "random")

# Compute clustered standard errors
se_plm1 <- vcovHC(plm1, type = "HC1", cluster = "group", group = dfs_long$gid.amerb)
se_plm2 <- vcovHC(plm2, type = "HC1", cluster = "group", group = dfs_long$gid.amerb)
se_plm3 <- vcovHC(plm3, type = "HC1", cluster = "group", group = dfs_long$gid.treat)
se_plm4 <- vcovHC(plm4, type = "HC1", cluster = "group", group = dfs_long$gid.treat)
se_plm5 <- vcovHC(plm5, type = "HC1", cluster = "group", group = dfs_long$gid.treat)

# Extract standard errors
se_list <- list(sqrt(diag(se_plm1)), sqrt(diag(se_plm2)), 
                sqrt(diag(se_plm3)), sqrt(diag(se_plm4)), sqrt(diag(se_plm5)))

# Generate Stargazer table with clustered SEs
stargazer(plm1, plm2, plm3, plm4, plm5,
          se = se_list, 
          type = "text",  # Change to "html" or "latex" if needed
          title = "Random Effects Panel Models with Clustered Standard Errors",
          dep.var.labels = c("Compliance Extraction Amerb", "Compliance Extraction OA"),
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
          star.cutoffs = c(0.1, 0.05, 0.01),
          notes = "Standard errors are clustered at gid.amerb for Models 1 & 2, and gid.treat for Models 3 -5 ."
)




