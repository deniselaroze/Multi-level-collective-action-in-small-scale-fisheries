# Load required packages
library(plm)
library(boot)
library(stargazer)

library(dplyr)
library(tidyr)
library(lme4)

rm(list=ls())
path_github <-"C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos<-"C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

#path_github <-"C:/Users/Denise Laroze/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
#path_datos<-"C:/Users/Denise Laroze/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

setwd(path_github)


#load(paste0(path_datos, "/Datos_islitas.Rdata"))
load(paste0(path_datos, "/Datos_islitas_recode.Rdata"))
load(paste0(path_datos, "/Datos_islitas_long.Rdata"))



##################################
### Descriptives about legitimacy
##################################

motivation_vars <- c(
  "survey1.1.player.T1_motiv_legit_amerb",
  "survey1.1.player.T1_motiv_instr_amerb",
  "survey1.1.player.T1_motiv_socnorm_amerb",
  "survey1.1.player.T1_motiv_legit_pm",
  "survey1.1.player.T1_motiv_instr_pm",
  "survey1.1.player.T1_motiv_socnorm_ingroup_pm",
  "survey1.1.player.T1_motiv_socnorm_outgroup_pm"
)


# 4. Compute counts and proportions for each response category by variable
prop_summary <- df %>%
  select(all_of(motivation_vars)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "response"
  ) %>%
  # Drop missing values if any
  filter(!is.na(response)) %>%
  group_by(variable, response) %>%
  summarise(
    count = n(),
    .groups = "drop"
  ) %>%
  group_by(variable) %>%
  mutate(
    proportion = count / sum(count)
  ) %>%
  ungroup()

# 5. View the proportion summary
print(prop_summary)

# 6. (Optional) Pivot to wide format for a cleaner table
prop_summary_wide <- prop_summary %>%
  pivot_wider(
    names_from = response,
    values_from = proportion,
    values_fill = 0
  )

# 7. View wide-format proportions
print(prop_summary_wide)





#######################
### Panel Models
#########################


dfs_long$round.plm<-ifelse(dfs_long$treatment=="T2", as.numeric(dfs_long$round)+8, as.numeric(dfs_long$round))
dfs_long$participant.code.plm<-ifelse(dfs_long$treatment=="T2", paste0(dfs_long$participant.code, ".T2"), dfs_long$participant.code)


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
B <- 100 
set.seed(62354)

###############################
###### Shared area models
###############################

formula_plm1 <- compliance_extraction_OA ~ compliance_lag_extraction_others_OA_mean + 
  compliance_beliefs_OA_caleta + compliance_beliefs_OA_others + 
  treatment + minority  

formula_plm2 <- compliance_extraction_OA ~ compliance_lag_extraction_others_OA_mean + 
  compliance_beliefs_OA_caleta + compliance_beliefs_OA_others + 
  treatment + minority + 
  survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm + survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta

formula_plm3 <- compliance_extraction_OA ~ compliance_lag_extraction_others_OA_mean + 
  compliance_beliefs_OA_caleta + compliance_beliefs_OA_others + 
  treatment + minority +
  survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm + survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta +
  survey3.1.player.sexo + survey3.1.player.horas_trabajo + 
  survey3.1.player.estudios + survey3.1.player.liderazgo

# Run bootstrap with correct cluster resampling
boot_se_plm1 <- boot(data = dfs_long, statistic = bootstrap_model, R = B, 
                     model_formula = formula_plm1, cluster_var = "gid.treat")

boot_se_plm2 <- boot(data = dfs_long, statistic = bootstrap_model, R = B, 
                     model_formula = formula_plm2, cluster_var = "gid.treat")

boot_se_plm3 <- boot(data = dfs_long, statistic = bootstrap_model, R = B, 
                     model_formula = formula_plm3, cluster_var = "gid.treat")

# Extract Bootstrapped SEs
se_list_boot <- list(apply(boot_se_plm1$t, 2, sd),
                     apply(boot_se_plm2$t, 2, sd),
                     apply(boot_se_plm3$t, 2, sd)
)

# Fit models on full dataset to get coefficient estimates
plm1 <- plm(formula_plm1, data = dfs_long, model = "random")
plm2 <- plm(formula_plm2, data = dfs_long, model = "random")
plm3 <- plm(formula_plm3, data = dfs_long, model = "random")

#stargazer(plm1, plm2, plm3)

# Stargazer Table with Corrected Bootstrapped Standard Errors
stargazer(plm1, plm2, plm3,
          se = se_list_boot, 
          type = "html",  # Change to "html" or "latex" if needed
          title = "Random Effects Panel Models",
          #order = c(1, 2, 3, 4, 5, 6, 7, 12, 8, 9, 10, 11, 13 ),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = paste0("Bootstrapped SEs of ", B , " samples of expt. matching groups (TURF group for M1-2, OA group M3-4)."),
          notes.align = "l", 
          out = paste0(path_github, "Outputs/model_pre_reg_boot_SA.html")
)


###################################
#### Alternative models SA
###################################

formula_plm1 <- compliance_extraction_OA ~ compliance_lag_extraction_others_OA_mean + 
  compliance_beliefs_OA_caleta + compliance_beliefs_OA_others + 
  treatment + minority  

formula_plm2 <- compliance_extraction_OA ~ compliance_lag_extraction_others_OA_mean + 
  compliance_beliefs_OA_caleta + compliance_beliefs_OA_others + 
  treatment + minority + 
  survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm + survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta

formula_plm3 <- compliance_extraction_OA ~ compliance_lag_extraction_others_OA_mean + 
  #compliance_beliefs_OA_caleta + compliance_beliefs_OA_others + 
  treatment + minority + 
  survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm + survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta


formula_plm4 <- compliance_extraction_OA ~ compliance_lag_extraction_others_OA_mean + 
  compliance_beliefs_OA_caleta + compliance_beliefs_OA_others*treatment + 
  treatment + minority
  

# Run bootstrap with correct cluster resampling
boot_se_plm1 <- boot(data = dfs_long, statistic = bootstrap_model, R = B, 
                     model_formula = formula_plm1, cluster_var = "participant.code")

boot_se_plm2 <- boot(data = dfs_long, statistic = bootstrap_model, R = B, 
                     model_formula = formula_plm2, cluster_var = "participant.code")

boot_se_plm3 <- boot(data = dfs_long, statistic = bootstrap_model, R = B, 
                     model_formula = formula_plm3, cluster_var = "participant.code")

boot_se_plm4 <- boot(data = dfs_long, statistic = bootstrap_model, R = B, 
                     model_formula = formula_plm4, cluster_var = "participant.code")

# Extract Bootstrapped SEs
se_list_boot <- list(apply(boot_se_plm1$t, 2, sd),
                     apply(boot_se_plm2$t, 2, sd),
                     apply(boot_se_plm3$t, 2, sd),
                     apply(boot_se_plm4$t, 2, sd)
)

# Fit models on full dataset to get coefficient estimates
plm1 <- plm(formula_plm1, data = dfs_long, model = "random")
plm2 <- plm(formula_plm2, data = dfs_long, model = "random")
plm3 <- plm(formula_plm3, data = dfs_long, model = "random")
plm4 <- plm(formula_plm4, data = dfs_long, model = "random")

#stargazer(plm1, plm2, plm3)

# Stargazer Table with Corrected Bootstrapped Standard Errors
stargazer(plm1, plm2, plm3, plm4,
          se = se_list_boot, 
          type = "html",  # Change to "html" or "latex" if needed
          title = "Random Effects Panel Models",
          #order = c(1, 2, 3, 4, 5, 6, 7, 12, 8, 9, 10, 11, 13 ),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = paste0("Bootstrapped SEs of ", B , " samples of expt. matching groups (TURF group for M1-2, OA group M3-4)."),
          notes.align = "l", 
          out = paste0(path_github, "Outputs/model_pre_reg_boot_SA_alternative.html")
)










####################################
#### ¨re-registrations models TURF
####################################



# Define model formulas
formula_plm1 <- compliance_extraction_amerb ~ compliance_lag_extraction_others_amerb_mean + 
  compliance_beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini + 
  treatment 

formula_plm2 <- compliance_extraction_amerb ~ compliance_lag_extraction_others_amerb_mean + 
  compliance_beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini + 
  treatment  +
  survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta

formula_plm3 <- compliance_extraction_amerb ~ compliance_lag_extraction_others_amerb_mean + 
  compliance_beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini + 
  treatment +
  survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta+
  survey1.1.player.T1_motiv_legit_amerb + survey1.1.player.T1_motiv_instr_amerb + survey1.1.player.T1_motiv_socnorm_amerb

formula_plm4 <- compliance_extraction_amerb ~ compliance_lag_extraction_others_amerb_mean + 
  compliance_beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini + 
  treatment +
  survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta+
  survey3.1.player.sexo + survey3.1.player.horas_trabajo + survey3.1.player.estudios + survey3.1.player.liderazgo



# Fit models on full dataset to get coefficient estimates
plm1 <- plm(formula_plm1, data = dfs_long, model = "random")
plm2 <- plm(formula_plm2, data = dfs_long, model = "random")
plm3 <- plm(formula_plm3, data = dfs_long, model = "random")
plm4 <- plm(formula_plm4, data = dfs_long, model = "random")

# Run bootstrap with correct cluster resampling
boot_se_plm1 <- boot(data = dfs_long, statistic = bootstrap_model, R = B, 
                     model_formula = formula_plm1, cluster_var = "gid.amerb")

boot_se_plm2 <- boot(data = dfs_long, statistic = bootstrap_model, R = B, 
                     model_formula = formula_plm2, cluster_var = "gid.amerb")

boot_se_plm3 <- boot(data = dfs_long, statistic = bootstrap_model, R = B, 
                     model_formula = formula_plm3, cluster_var = "gid.amerb")

boot_se_plm4 <- boot(data = dfs_long, statistic = bootstrap_model, R = B, 
                     model_formula = formula_plm4, cluster_var = "gid.amerb")


# Extract Bootstrapped SEs
se_list_boot <- list(apply(boot_se_plm1$t, 2, sd),
                     apply(boot_se_plm2$t, 2, sd),
                     apply(boot_se_plm3$t, 2, sd),
                     apply(boot_se_plm4$t, 2, sd)
)

stargazer(plm1, plm2, plm3, plm4)



# Stargazer Table with Corrected Bootstrapped Standard Errors
stargazer(plm1, plm2, plm3, plm4,
          se = se_list_boot, 
          type = "html",  # Change to "html" or "latex" if needed
          title = "Random Effects Panel Models",
          #order = c(1, 2, 3, 4, 5, 6, 7, 12, 8, 9, 10, 11, 13 ),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = paste0("Bootstrapped SEs of ", B , " samples of expt. matching groups (TURF group for M1-2, OA group M3-4)."),
          notes.align = "l", 
          out = paste0(path_github, "Outputs/model_pre_reg_boot_TURF.html")
)





