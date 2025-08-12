


library(plm)
library(boot)
library(stargazer)

rm(list=ls())
path_github <-"C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos<-"C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

#path_github <-"C:/Users/Denise Laroze/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
#path_datos<-"C:/Users/Denise Laroze/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

setwd(path_github)


#load(paste0(path_datos, "/Datos_islitas.Rdata"))
load(paste0(path_datos, "/Datos_islitas_recode.Rdata"))
load(paste0(path_datos, "/Datos_islitas_long.Rdata"))


dfs_long$round.plm<-ifelse(dfs_long$treatment=="T2", as.numeric(dfs_long$round)+8, as.numeric(dfs_long$round))
dfs_long$participant.code.plm<-ifelse(dfs_long$treatment=="T2", paste0(dfs_long$participant.code, ".T2"), dfs_long$participant.code)
dfs_long$compliance_beliefs_turf_caleta<-ifelse(dfs_long$treatment=="T1", dfs_long$compliance_beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini,
                                                dfs_long$compliance_beliefsT1final.1.player.T1_belief_caleta_en_amerb_fin)

dfs_long$conflicto_OA<-ifelse(dfs_long$treatment=="T1", dfs_long$survey1.1.player.conflicto_pm,
                                                dfs_long$survey2.1.player.conflicto_caleta_conocida_mean)

dfs_long$confianza_OA<-ifelse(dfs_long$treatment=="T1", dfs_long$survey1.1.player.confianza_pm,
                                                dfs_long$survey2.1.player.confianza_caleta_conocida_mean)


# Convert panel data correctly
dfs_long <- pdata.frame(dfs_long, index = c("participant.code.plm", "round.plm"))

# Define a function to resample clusters and fit a plm model
bootstrap_model <- function(data, indices, model_formula, cluster_var) {
  # Get unique clusters
  unique_clusters <- unique(data[[cluster_var]])
  
  # Sample clusters (with replacement)
  sampled_clusters <- sample(unique_clusters, length(unique_clusters), replace = TRUE)
  
  # Subset data based on sampled clusters
  data_sample <- data[data[[cluster_var]] %in% sampled_clusters, ]
  
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
  compliance_beliefs_turf_caleta + 
  treatment + n_identities

formula_plm2 <- compliance_extraction_amerb ~ compliance_lag_extraction_others_amerb_mean + 
  compliance_beliefs_turf_caleta + 
  treatment +  n_identities +
  survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
  

formula_plm3 <- compliance_extraction_OA ~ compliance_lag_extraction_others_OA_mean + 
  compliance_beliefs_OA_caleta + compliance_beliefs_OA_others + 
  treatment + minority+ n_identities
  

formula_plm4 <- compliance_extraction_OA ~ compliance_lag_extraction_others_OA_mean + 
  compliance_beliefs_OA_caleta + compliance_beliefs_OA_others + 
  treatment + minority + n_identities+
  survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta +
  conflicto_OA + confianza_OA
 

# Run bootstrap with correct cluster resampling
boot_se_plm1 <- boot(data = dfs_long, statistic = bootstrap_model, R = B, 
                     model_formula = formula_plm1, cluster_var = "gid.amerb")

boot_se_plm2 <- boot(data = dfs_long, statistic = bootstrap_model, R = B, 
                     model_formula = formula_plm2, cluster_var = "gid.amerb")

boot_se_plm3 <- boot(data = dfs_long, statistic = bootstrap_model, R = B, 
                     model_formula = formula_plm3, cluster_var = "gid.treat")

boot_se_plm4 <- boot(data = dfs_long, statistic = bootstrap_model, R = B, 
                     model_formula = formula_plm4, cluster_var = "gid.treat")

# Extract Bootstrapped SEs
se_list_boot <- list(apply(boot_se_plm1$t, 2, sd),
                     apply(boot_se_plm2$t, 2, sd),
                     apply(boot_se_plm3$t, 2, sd),
                     apply(boot_se_plm4$t, 2, sd)
)

# Fit models on full dataset to get coefficient estimates
plm1 <- plm(formula_plm1, data = dfs_long, model = "pooling")
plm2 <- plm(formula_plm2, data = dfs_long, model = "pooling")
plm3 <- plm(formula_plm3, data = dfs_long, model = "pooling")
plm4 <- plm(formula_plm4, data = dfs_long, model = "pooling")


stargazer(plm1, plm2, plm3, plm4)



# Stargazer Table with Corrected Bootstrapped Standard Errors
stargazer(plm1, plm2, plm3, plm4,
          se = se_list_boot, 
          type = "html",  # Change to "html" or "latex" if needed
          title = "Random Effects Panel Models",
          #order = c(1, 2, 3, 4, 5, 6, 7, 12, 8, 9, 10, 11, 13 ),
          #covariate.labels = c("Mean Compliance by Group TURF (t-1)", "Beliefs Compliance by Ingroup in TURF", 
          #                     "Mean Compliance by Group OA (t-1)", 
          #                     "Beliefs Compliance by Ingroup in OA", "Beliefs Compliance by Outsiders in OA",
          #                     "Scenario Know Outsiders (dummy)", "Sessions with 3 Unions (Dummy)" , "Minority (Dummy)",
          #                     "Female (Dummy)", "Fishing h/w", 
          #                     "Level of Education" ,"Held Leadership Role (Dummy)",  
          #                     "Constant" ),
          dep.var.labels = c("Compliance Extraction Amerb", "Compliance Extraction OA"),
          #column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
          star.cutoffs = c(0.1, 0.05, 0.01),
          notes = paste0("Bootstrapped SEs of ", B , " samples of expt. matching groups (TURF group for M1-2, OA group M3-4)."),
          notes.align = "l", 
          out = paste0(path_github, "Outputs/tests.html")
)
