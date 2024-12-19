###########################
### Data Analysis
############################
library(stargazer)
library(foreign)
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(sandwich)   # For robust and clustered standard errors
library(lmtest)     # For coeftest
library(stargazer)  # For model output tables
library(ggeffects)



rm(list=ls())
path_github <-"C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos<-"C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

#path_github <-"C:/Users/Denise Laroze/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
#path_datos<-"C:/Users/Denise Laroze/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

setwd(path_github)


#load(paste0(path_datos, "/Datos_islitas.Rdata"))
load(paste0(path_datos, "/Datos_islitas_recode.Rdata"))
load(paste0(path_datos, "/Datos_islitas_long.Rdata"))


#################################################
################# Subsets #######################
#################################################

# Mean by treatment and area, for each round for diff-in-diff 

#rm <- dfs_long %>%
#  group_by(treatment, area, round) %>%  # Group by treatment and variable
#  summarise(mean_extraction = mean(extraction, na.rm = TRUE))  # Calculate mean and handle missing values


#Belief Columns 
#belief_columns <- grep("belief", colnames(df), value = TRUE, ignore.case = TRUE)
# Now filter to keep only the ones that end in "_ini" or "_fin"
#filtered_belief_columns <- grep("_ini$|_fin$|id", belief_columns, value = TRUE, ignore.case = TRUE)

# subset a dataframe of beliefs 
#print(filtered_belief_columns)
#df.b<-df[, c("participant.code","gid.treat", "gid.amerb", filtered_belief_columns )]

#################################################
############### Data Analysis ###################
#################################################

########################
#Descriptive statistics
########################

20000+mean(df$participant.payoff, na.rm=T)
summary(df$participant.payoff, na.rm=T)


table(df$T2juegoalgas.1.player.T2_grupo_mixto)
table(df$participant.grupo_amerb)


#Trust
table(df$survey1.1.player.confianza_caleta)
table(df$survey1.1.player.confianza_pm)
table(df$survey2.1.player.confianza_caleta_conocida1)
table(df$survey2.1.player.confianza_caleta_conocida2)

#Conflict
table(df$survey1.1.player.conflicto_caleta)
table(df$survey1.1.player.conflicto_pm)
table(df$survey2.1.player.conflicto_caleta_conocida1)
table(df$survey2.1.player.conflicto_caleta_conocida2)

########################################################################
##### Preliminary regression analysis on extraction in OA given beliefs
########################################################################


# Run models with individual clustered s.e.
lm1 <- lm(extraction_OA ~ beliefs_OA_caleta + beliefs_OA_others + treatment, data = dfs_long)
lm2 <- lm(extraction_OA ~ survey1.1.player.confianza_caleta + survey1.1.player.confianza_pm +  
            survey1.1.player.conflicto_caleta + survey1.1.player.conflicto_pm + treatment, data = dfs_long)
lm3 <- lm(extraction_OA ~ beliefs_OA_caleta + beliefs_OA_others +  
            survey1.1.player.confianza_caleta + survey1.1.player.confianza_pm +  
            survey1.1.player.conflicto_caleta + survey1.1.player.conflicto_pm + treatment, data = dfs_long)
lm4 <- lm(extraction_OA ~ lag_extraction_others_OA_mean + beliefs_OA_caleta + beliefs_OA_others + 
            survey1.1.player.confianza_caleta + survey1.1.player.confianza_pm +  
            survey1.1.player.conflicto_caleta + survey1.1.player.conflicto_pm + treatment, data = dfs_long)


# Calculate clustered standard errors by 'participant.code'
clustered_se_lm1 <- sqrt(diag(vcovCL(lm1, cluster = ~participant.code)))
clustered_se_lm2 <- sqrt(diag(vcovCL(lm2, cluster = ~participant.code)))
clustered_se_lm3 <- sqrt(diag(vcovCL(lm3, cluster = ~participant.code)))
clustered_se_lm4 <- sqrt(diag(vcovCL(lm4, cluster = ~participant.code)))

# Export to stargazer with clustered standard errors
stargazer(lm1, lm2, lm3, lm4,
          se = list(clustered_se_lm1, clustered_se_lm2, clustered_se_lm3, clustered_se_lm4),
          type = "html",
          out = paste0(path_github, "Outputs/extraction_clustered_se.html"),
          title = "Regression Results with Clustered Standard Errors",
          dep.var.labels = "Extraction OA",
          #covariate.labels = c("Beliefs Caleta", "Beliefs Others", "Treatment",
          #                     "Confianza Caleta", "Confianza PM", 
          #                     "Conflicto Caleta", "Conflicto PM", 
          #                     "Lag Extraction Others Mean"),
          notes = "Clustered standard errors by participant code are reported in parentheses.")

#### with group clustered s.e.
# Run models
lm1 <- lm(extraction_OA ~ beliefs_OA_caleta + beliefs_OA_others + treatment, data = dfs_long)
lm2 <- lm(extraction_OA ~ survey1.1.player.confianza_caleta + survey1.1.player.confianza_pm +  
            survey1.1.player.conflicto_caleta + survey1.1.player.conflicto_pm + treatment, data = dfs_long)
lm3 <- lm(extraction_OA ~ beliefs_OA_caleta + beliefs_OA_others +  
            survey1.1.player.confianza_caleta + survey1.1.player.confianza_pm +  
            survey1.1.player.conflicto_caleta + survey1.1.player.conflicto_pm + treatment, data = dfs_long)
lm4 <- lm(extraction_OA ~ lag_extraction_others_OA_mean + beliefs_OA_caleta + beliefs_OA_others + 
            survey1.1.player.confianza_caleta + survey1.1.player.confianza_pm +  
            survey1.1.player.conflicto_caleta + survey1.1.player.conflicto_pm + treatment, data = dfs_long)

# Calculate clustered standard errors by 'participant.code'
clustered_se_lm1 <- sqrt(diag(vcovCL(lm1, cluster = ~gid.treat)))
clustered_se_lm2 <- sqrt(diag(vcovCL(lm2, cluster = ~gid.treat)))
clustered_se_lm3 <- sqrt(diag(vcovCL(lm3, cluster = ~gid.treat)))
clustered_se_lm4 <- sqrt(diag(vcovCL(lm4, cluster = ~gid.treat)))

# Export to stargazer with clustered standard errors
stargazer(lm1, lm2, lm3, lm4,
          se = list(clustered_se_lm1, clustered_se_lm2, clustered_se_lm3, clustered_se_lm4),
          type = "html",
          out = paste0(path_github, "Outputs/extraction_clustered_se.html"),
          title = "Regression Results with Clustered Standard Errors",
          dep.var.labels = "Extraction OA",
          #covariate.labels = c("Beliefs Caleta", "Beliefs Others", "Treatment",
          #                     "Confianza Caleta", "Confianza PM", 
          #                     "Conflicto Caleta", "Conflicto PM", 
          #                     "Lag Extraction Others Mean"),
          notes = "Clustered standard errors by matching group are reported in parentheses.")

dfs_long_t1 <- dfs_long %>%
  filter(treatment == "T1")

lm<-lm(extraction_amerb ~ lag_beliefs_amerb_updated, data=dfs_long_t1)
summary(lm)


lm<-lm(extraction_amerb ~ lag_extraction_others_amerb_mean + beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini , data=dfs_long)
summary(lm)

lm1<-lm(extraction_OA ~ lag_extraction_others_OA_mean + beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini + beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini + treatment, data=dfs_long)
summary(lm1)


lm2<-lm(extraction_OA ~ lag_extraction_others_OA_mean + beliefs_OA_caleta + beliefs_OA_others
        + treatment, data=dfs_long)
summary(lm2)


lm2<-lm(extraction_OA ~ lag_extraction_others_OA_mean + beliefs_OA_caleta + beliefs_OA_others
        + treatment, data=dfs_long[dfs_long$treatment=="T1", ])
summary(lm2)

lm3<-lm(extraction_OA ~ lag_extraction_others_OA_mean + beliefs_OA_caleta + beliefs_OA_others
        + treatment, data=dfs_long[dfs_long$treatment=="T2",])
summary(lm3)


# Subset the data for treatment == "T1"
dfs_long_t1 <- dfs_long %>%
  filter(treatment == "T1")

# Fit the linear model using the subsetted data
lm2 <- lm(
  extraction_OA ~ lag_extraction_others_OA_mean + beliefs_OA_caleta + beliefs_OA_others,
  data = dfs_long_t1
)

# Display the summary of the model
summary(lm2)

dfs_long_t2 <- dfs_long %>%
  filter(treatment == "T2")

# Fit the linear model using the subsetted data
lm3 <- lm(
  extraction_OA ~ lag_extraction_others_OA_mean + beliefs_OA_caleta + beliefs_OA_others,
  data = dfs_long_t2
)

# Display the summary of the model
summary(lm3)

stargazer(
  lm2, lm3, # Select the models to include
  type = "html",
  out = paste0(path_github, "Outputs/extraction_long.html")
)


# controlling for the number identities people see on screen -- produces the same results

lm1 <- lm(extraction_OA ~ lag_extraction_others_OA_mean +  beliefs_OA_caleta + beliefs_OA_others + treatment, data = dfs_long)
lm2 <- lm(extraction_OA ~ lag_extraction_others_OA_mean + beliefs_OA_caleta + beliefs_OA_others + treatment + n_identities + survey3.1.player.sexo + 
            survey3.1.player.nacimiento + survey3.1.player.horas_trabajo, data = dfs_long)
lm3 <- lm(extraction_OA ~ lag_extraction_others_OA_mean + beliefs_OA_caleta + beliefs_OA_others + treatment + n_identities +  
            + survey3.1.player.horas_trabajo + survey3.1.player.estudios + survey3.1.player.liderazgo, data = dfs_long)

lm4 <- lm(extraction_OA ~ lag_extraction_others_OA_mean + beliefs_OA_caleta + beliefs_OA_others + 
            treatment + n_identities + survey1.1.player.T1_motiv_legit_pm + survey1.1.player.T1_motiv_instr_pm + 
            survey1.1.player.T1_motiv_socnorm_ingroup_pm + survey1.1.player.T1_motiv_socnorm_outgroup_pm, 
          data = dfs_long)

lm4 <- lm(extraction_OA ~ lag_extraction_others_OA_mean + beliefs_OA_caleta + beliefs_OA_others + 
            treatment + n_identities + survey1.1.player.T1_motiv_legit_amerb + 
            survey1.1.player.T1_motiv_instr_amerb + survey1.1.player.T1_motiv_socnorm_amerb + 
            survey1.1.player.T1_motiv_legit_pm + survey1.1.player.T1_motiv_instr_pm + 
            survey1.1.player.T1_motiv_socnorm_ingroup_pm + survey1.1.player.T1_motiv_socnorm_outgroup_pm, 
          data = dfs_long)


# Calculate clustered standard errors by 'participant.code'
clustered_se_lm1 <- sqrt(diag(vcovCL(lm1, cluster = ~participant.code)))
clustered_se_lm2 <- sqrt(diag(vcovCL(lm2, cluster = ~participant.code)))
clustered_se_lm3 <- sqrt(diag(vcovCL(lm3, cluster = ~participant.code)))
clustered_se_lm4 <- sqrt(diag(vcovCL(lm4, cluster = ~participant.code)))

# Export to stargazer with clustered standard errors

stargazer(lm1,lm2, lm3, lm4,
          se = list(clustered_se_lm1, clustered_se_lm2, clustered_se_lm3, clustered_se_lm4),
          type = "html",
          out = paste0(path_github, "Outputs/extraction_OA_controls_clustered_se.html"),
          title = "Regression Results with Clustered Standard Errors",
          dep.var.labels = "Extraction OA",
          #covariate.labels = c("Beliefs Caleta", "Beliefs Others", "Treatment",
          #                     "Confianza Caleta", "Confianza PM", 
          #                     "Conflicto Caleta", "Conflicto PM", 
          #                     "Lag Extraction Others Mean"),
          notes = "Clustered standard errors by participant code are reported in parentheses.")


### Robustness checks on the number/ minority status in the round

# Run models on extraction with number of out_group participants in the area and matching group clustered s.e.
lm1 <- lm(extraction_OA ~ lag_extraction_others_OA_mean + beliefs_OA_caleta + beliefs_OA_others + treatment + n_identities, data = dfs_long)

lm2 <- lm(extraction_OA ~ lag_extraction_others_OA_mean + beliefs_OA_caleta + beliefs_OA_others + treatment + minority + n_identities, data = dfs_long)

lm3 <- lm(extraction_OA ~ lag_extraction_others_OA_mean + beliefs_OA_caleta + beliefs_OA_others*minority + treatment + n_identities, data = dfs_long)

lm4 <- lm(extraction_OA ~ lag_extraction_others_OA_mean + beliefs_OA_caleta*minority  + beliefs_OA_others + treatment + n_identities, data = dfs_long)


# Calculate clustered standard errors by 'participant.code'
clustered_se_lm1 <- sqrt(diag(vcovCL(lm1, cluster = ~gid.treat)))
clustered_se_lm2 <- sqrt(diag(vcovCL(lm2, cluster = ~gid.treat)))
clustered_se_lm3 <- sqrt(diag(vcovCL(lm3, cluster = ~gid.treat)))
clustered_se_lm4 <- sqrt(diag(vcovCL(lm4, cluster = ~gid.treat)))

# Export to stargazer with clustered standard errors
stargazer(lm1, lm2, lm3, lm4,
          se = list(clustered_se_lm1, clustered_se_lm2, clustered_se_lm3, clustered_se_lm4),
          type = "html",
          out = paste0(path_github, "Outputs/extraction_clustered_se_n_ingroup.html"),
          title = "Regression Results with Clustered Standard Errors",
          dep.var.labels = "Extraction OA",
          #covariate.labels = c("Beliefs Caleta", "Beliefs Others", "Treatment",
          #                     "Confianza Caleta", "Confianza PM", 
          #                     "Conflicto Caleta", "Conflicto PM", 
          #                     "Lag Extraction Others Mean"),
          notes = "Clustered standard errors by matching group are reported in parentheses.")

interaction_data <- ggpredict(lm3, terms = c("beliefs_OA_others", "minority"))

# Plot the interaction using ggplot2
ggplot(interaction_data, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  labs(
    title = "Interaction Effect: beliefs_OA_others * minority",
    x = "Beliefs about Others in OA",
    y = "Predicted Extraction in OA",
    color = "Minority",
    fill = "Minority"
  ) +
  theme_minimal()

##### Regressions with updated beliefs
lm1 <- lm(extraction_OA ~ beliefs_OA_caleta + beliefs_OA_others + treatment, data = dfs_long)
lm2 <- lm(extraction_OA ~ lag_beliefs_ingroup_OA_updated + lag_beliefs_outgroup_OA_updated + 
            treatment, data = dfs_long)
lm3 <- lm(extraction_OA ~ lag_extraction_others_OA_mean + beliefs_OA_caleta + beliefs_OA_others + 
            treatment, data = dfs_long)
lm4 <- lm(extraction_OA ~ lag_extraction_others_OA_mean + 
            lag_beliefs_ingroup_OA_updated + lag_beliefs_outgroup_OA_updated+
            treatment, data = dfs_long)


# Calculate clustered standard errors by 'participant.code'
clustered_se_lm1 <- sqrt(diag(vcovCL(lm1, cluster = ~gid.treat)))
clustered_se_lm2 <- sqrt(diag(vcovCL(lm2, cluster = ~gid.treat)))
clustered_se_lm3 <- sqrt(diag(vcovCL(lm3, cluster = ~gid.treat)))
clustered_se_lm4 <- sqrt(diag(vcovCL(lm4, cluster = ~gid.treat)))

# Export to stargazer with clustered standard errors
stargazer(lm1, lm2, lm3, lm4, 
          se = list(clustered_se_lm1, clustered_se_lm2, clustered_se_lm3, clustered_se_lm4),
          type = "html",
          out = paste0(path_github, "Outputs/extraction_imput_beliefs_clustered_se.html"),
          title = "Regression Results with Clustered Standard Errors",
          dep.var.labels = "Extraction OA",
          #covariate.labels = c("Beliefs Caleta", "Beliefs Others", "Treatment",
          #                     "Confianza Caleta", "Confianza PM", 
          #                     "Conflicto Caleta", "Conflicto PM", 
          #                     "Lag Extraction Others Mean"),
          notes = "Clustered standard errors by participant code are reported in parentheses.")

##### regressions with imputed beliefs

# Run models on extraction with number of out_group participants in the area and matching group clustered s.e.
lm1 <- lm(extraction_OA ~ lag_extraction_others_OA_mean +  imputed_beliefs_OA_caleta + imputed_beliefs_OA_others + treatment + n_identities, data = dfs_long)

lm2 <- lm(extraction_OA ~ lag_extraction_others_OA_mean +  imputed_beliefs_OA_caleta + imputed_beliefs_OA_others + treatment + minority + n_identities, data = dfs_long)

lm3 <- lm(extraction_OA ~ lag_extraction_others_OA_mean +  imputed_beliefs_OA_caleta + imputed_beliefs_OA_others*minority + treatment + n_identities, data = dfs_long)

lm4 <- lm(extraction_OA ~ lag_extraction_others_OA_mean +  imputed_beliefs_OA_caleta*minority + imputed_beliefs_OA_others + treatment + n_identities, data = dfs_long)


# Calculate clustered standard errors by 'participant.code'
clustered_se_lm1 <- sqrt(diag(vcovCL(lm1, cluster = ~gid.treat)))
clustered_se_lm2 <- sqrt(diag(vcovCL(lm2, cluster = ~gid.treat)))
clustered_se_lm3 <- sqrt(diag(vcovCL(lm3, cluster = ~gid.treat)))
clustered_se_lm4 <- sqrt(diag(vcovCL(lm4, cluster = ~gid.treat)))

# Export to stargazer with clustered standard errors
stargazer(lm1, lm2, lm3, lm4,
          se = list(clustered_se_lm1, clustered_se_lm2, clustered_se_lm3, clustered_se_lm4),
          type = "html",
          out = paste0(path_github, "Outputs/extraction_imputed_beliefs_cl_se.html"),
          title = "Regression Results with Clustered Standard Errors",
          dep.var.labels = "Extraction OA",
          #covariate.labels = c("Beliefs Caleta", "Beliefs Others", "Treatment",
          #                     "Confianza Caleta", "Confianza PM", 
          #                     "Conflicto Caleta", "Conflicto PM", 
          #                     "Lag Extraction Others Mean"),
          notes = "Clustered standard errors by matching group are reported in parentheses.")



interaction_data <- ggpredict(lm3, terms = c("imputed_beliefs_OA_others", "minority"))

# Plot the interaction using ggplot2
p<-ggplot(interaction_data, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  labs(
    title = "Interaction Effect: imputed_beliefs_OA_others * minority",
    x = "Beliefs about Others in OA",
    y = "Predicted Extraction in OA",
    color = "Minority",
    fill = "Minority"
  ) +
  theme_minimal()

ggsave( file=paste0(path_github, "Outputs/interaction_minority_beliefs.pdf") , plot = p, device = "pdf", width = 12, height = 6)


###################################################################
#### Plot for mean over extraction in OA by categories of beliefs 
##################################################################


##### Beliefs about how outsiders will behave in OA

# Calculate mean and confidence intervals
mean_extraction_round <- dfs_long %>%
  group_by(treatment, round, beliefs_OA_others_cat) %>%
  summarize(
    extraction_OA_mean = mean(extraction_OA, na.rm = TRUE),
    extraction_OA_se = sd(extraction_OA, na.rm = TRUE) / sqrt(n()),  # Standard Error
    .groups = "drop"
  ) %>%
  mutate(
    lower_ci = extraction_OA_mean - qt(0.975, df = n() - 1) * extraction_OA_se,  # 95% CI lower bound
    upper_ci = extraction_OA_mean + qt(0.975, df = n() - 1) * extraction_OA_se   # 95% CI upper bound
  )

# Plot with confidence intervals
plot<-ggplot(mean_extraction_round, aes(x = round, y = extraction_OA_mean, color = beliefs_OA_others_cat)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = beliefs_OA_others_cat), alpha = 0.2, color = NA) +
  facet_wrap(~treatment) +
  labs(
    title = "Mean Extraction OA with Confidence Intervals, by Beliefs in Outgroup",
    x = "Round",
    y = "Mean Extraction OA"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

plot

ggsave(file=paste0(path_github, "Outputs/Plot_cat_beliefs_others_extraction_OA.pdf") , plot = plot, device = "pdf", width = 12, height = 6)


##### Beliefs about how outsiders will behave in OA

# Calculate mean and confidence intervals
mean_extraction_round <- dfs_long %>%
  group_by(treatment, round, beliefs_OA_caleta_cat) %>%
  summarize(
    extraction_OA_mean = mean(extraction_OA, na.rm = TRUE),
    extraction_OA_se = sd(extraction_OA, na.rm = TRUE) / sqrt(n()),  # Standard Error
    .groups = "drop"
  ) %>%
  mutate(
    lower_ci = extraction_OA_mean - qt(0.975, df = n() - 1) * extraction_OA_se,  # 95% CI lower bound
    upper_ci = extraction_OA_mean + qt(0.975, df = n() - 1) * extraction_OA_se   # 95% CI upper bound
  )

# Plot with confidence intervals
plot<-ggplot(mean_extraction_round, aes(x = round, y = extraction_OA_mean, color = beliefs_OA_caleta_cat)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = beliefs_OA_caleta_cat), alpha = 0.2, color = NA) +
  facet_wrap(~treatment) +
  labs(
    title = "Mean Extraction OA with Confidence Intervals, by Beliefs in ingroup",
    x = "Round",
    y = "Mean Extraction OA"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

plot

ggsave(file=paste0(path_github, "Outputs/Plot_cat_beliefs__caleta_extraction_OA.pdf") , plot = plot, device = "pdf", width = 12, height = 6)







##### Beliefs about how ingroup will behave in OA
# Calculate mean and confidence intervals
mean_extraction_round <- dfs_long %>%
  group_by(treatment, round, beliefs_OA_others_cat) %>%
  summarize(
    extraction_OA_mean = mean(extraction_OA, na.rm = TRUE),
    extraction_OA_se = sd(extraction_OA, na.rm = TRUE) / sqrt(n()),  # Standard Error
    .groups = "drop"
  ) %>%
  mutate(
    lower_ci = extraction_OA_mean - qt(0.975, df = n() - 1) * extraction_OA_se,  # 95% CI lower bound
    upper_ci = extraction_OA_mean + qt(0.975, df = n() - 1) * extraction_OA_se   # 95% CI upper bound
  )

# Plot with confidence intervals
plot<-ggplot(mean_extraction_round, aes(x = round, y = extraction_OA_mean, color = beliefs_OA_amerb_cat)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = beliefs_OA_others_cat), alpha = 0.2, color = NA) +
  facet_wrap(~treatment) +
  labs(
    title = "Mean Extraction OA with Confidence Intervals",
    x = "Round",
    y = "Mean Extraction OA"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )



ggsave(file=paste0(path_github, "Outputs/Plot_cat_beliefs_others_extraction.pdf") , plot = plot, device = "pdf", width = 12, height = 6)












############################
#### Regressions on beliefs
#############################
# Initial beliefs regressions
lm1 <- lm(beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini ~ 
            survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta + 
            survey1.1.player.experiencia_caleta, 
          data = df)

lm2 <- lm(beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini ~ 
            as.character(survey1.1.player.confianza_caleta) + as.character(survey1.1.player.conflicto_caleta) + 
            as.character(survey1.1.player.experiencia_caleta),  
          data = df)

lm3 <- lm(beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini ~ 
            survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta + 
            survey1.1.player.experiencia_caleta + survey1.1.player.experiencia_pm,  
          data = df)

lm4 <- lm(beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini ~ 
            as.factor(survey1.1.player.confianza_caleta) + as.factor(survey1.1.player.conflicto_caleta) + 
            as.factor(survey1.1.player.experiencia_caleta) , 
          data = df)

# Beliefs out-group regressions
lm5 <- lm(beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini ~ 
            survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm + survey1.1.player.experiencia_pm, 
          data = df)

lm6 <- lm(beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini ~ 
            as.factor(survey1.1.player.confianza_pm) + as.factor(survey1.1.player.conflicto_pm) + as.factor(survey1.1.player.experiencia_pm), 
          data = df)

lm7 <- lm(beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini ~ 
            survey2.1.player.confianza_caleta_conocida_mean + survey2.1.player.conflicto_caleta_conocida_mean +
            survey2.1.player.experiencia_caleta_conocida1, 
          data = df)

lm8 <- lm(beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini ~ 
            as.factor(survey2.1.player.confianza_caleta_conocida_mean) + as.factor(survey2.1.player.conflicto_caleta_conocida_mean)+
            as.factor(survey2.1.player.experiencia_caleta_conocida1), 
          data = df)

# Heteroskedasticity-robust clustered standard errors (HC1) by participant.code
clustered_se <- list(
  sqrt(diag(vcovCL(lm1, cluster = ~participant.code, type = "HC1"))),
  sqrt(diag(vcovCL(lm3, cluster = ~participant.code, type = "HC1"))),
  sqrt(diag(vcovCL(lm5, cluster = ~participant.code, type = "HC1"))),
  sqrt(diag(vcovCL(lm7, cluster = ~participant.code, type = "HC1")))
)

# Dependent variable labels
dep_var_labels <- c(
  "Initial Beliefs Ingroup-Amerb T1",
  "Initial Beliefs Ingroup-OA T1",
  "Initial Beliefs Others-Metat T2",
  "Initial Beliefs Others-Metat T2"
)

# Export the models to an HTML table with robust SEs
stargazer(
  lm1, lm3, lm5, lm7,
  se = clustered_se,  # Include robust standard errors
  type = "html",
  dep.var.labels = dep_var_labels,
  #covariate.labels = c("Trust Ingroup", "Conflict Ingroup",
  #                     "Trust Others (T1)", "Conflict Others (T1)",
  #                     "Trust Others (T2)", "Conflict Others (T2)"),
  out = paste0(path_github, "Outputs/Beliefs_Ini_HC1.html")
)




#### Beliefs at the end
lm1<-lm(beliefsT1final.1.player.T1_belief_caleta_en_amerb_fin ~ survey1.1.player.confianza_caleta +survey1.1.player.conflicto_caleta + otros_amerb_t1_mean , data=df)

lm2<-lm(beliefsT1final.1.player.T1_belief_caleta_en_amerb_fin ~ as.character(survey1.1.player.confianza_caleta) + as.character(survey1.1.player.conflicto_caleta) +otros_amerb_t1_mean , data=df)

lm3<-lm(beliefsT1final.1.player.T1_belief_caleta_en_libre_fin ~  survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta + otros_libre_t1_mean , data=df)
lm4<-lm(beliefsT1final.1.player.T1_belief_caleta_en_libre_fin ~  as.factor(survey1.1.player.confianza_caleta) + as.factor(survey1.1.player.conflicto_caleta) + otros_libre_t1_mean , data=df)



lm5<-lm(beliefsT1final.1.player.T1_belief_pm_en_libre_fin ~ survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm + otros_libre_t1_mean , data=df)

lm6<-lm(beliefsT1final.1.player.T1_belief_pm_en_libre_fin ~ as.factor(survey1.1.player.confianza_pm) + as.factor(survey1.1.player.conflicto_pm) + otros_libre_t1_mean, data=df)

lm7<-lm(beliefsT2final.1.player.T2_belief_caleta_conocida_mean_fin ~ survey2.1.player.confianza_caleta_conocida_mean + survey2.1.player.conflicto_caleta_conocida_mean + otros_metat_t2_mean, data=df)

lm8<-lm(beliefsT2final.1.player.T2_belief_caleta_conocida_mean_fin ~ as.factor(survey2.1.player.confianza_caleta_conocida_mean) + as.factor(survey2.1.player.conflicto_caleta_conocida1) + otros_metat_t2_mean , data=df)

stargazer(lm1, lm3, lm5, lm7, out=paste0(path_github,"Outputs/Beliefs_fin.html"),type="html")


library(stargazer)

# Rename dependent variables for clarity
dep_var_labels <- c(
  "End Beliefs Ingroup-Amerb T1 ",
  "End Beliefs Ingroup-OA T1",
  "End Beliefs Ingroup-Metat T2",
  "End Beliefs Others-Metat T2"
)

# Export the models to an HTML table
stargazer(
  lm1, lm3, lm5, lm7, # Select the models to include
  type = "html",
  dep.var.labels = dep_var_labels, # Use custom dependent variable labels
  covariate.labels = c("Trust Ingroup","Conflict Ingroup",
                       "Mean extraction group Amerb (T1)",
                       "Trust Others (T1)","Conflict Others (T1)",
                       "Mean extraction group OA (T1)",
                       "Trust Others (T2)","Conflict Others (T2)",
                       "Mean extraction group Metat (T2)" ),
  out = paste0(path_github, "Outputs/Beliefs_fin.html")
)




#### Diferences in Beliefs
lm1<-lm(T1_diff_ingroup_OA ~  survey1.1.player.confianza_caleta +survey1.1.player.conflicto_caleta + otros_libre_t1_mean, data=df)
lm2<-lm(T1_diff_others_OA ~  survey1.1.player.confianza_pm +survey1.1.player.conflicto_pm + otros_libre_t1_mean, data=df)


lm3<-lm(T2_diff_ingroup_metat ~  survey1.1.player.confianza_caleta +survey1.1.player.conflicto_caleta + otros_metat_t2_mean, data=df)
lm4<-lm(T2_diff_others_metat ~  survey2.1.player.confianza_caleta_conocida_mean +survey2.1.player.conflicto_caleta_conocida_mean + otros_metat_t2_mean, data=df)

stargazer(lm1, lm2, lm3, lm4, out=paste0(path_github,"Outputs/Diff_Beliefs.html"),type="html")






#############
### Graphs
##############
#####################################################
### Plots for experience/trust/conflict with others
####################################################

# List of dummy variables and corresponding plot titles
dummy_vars <- c("dummy_confianza_caleta", "dummy_confianza_pm", 
                "dummy_conflicto_caleta", "dummy_conflicto_pm", 
                "dummy_experiencia_caleta", "dummy_experiencia_pm")

plot_titles <- c("Confianza Caleta", "Confianza PM", 
                 "Conflicto Caleta", "Conflicto PM", 
                 "Experiencia Caleta", "Experiencia PM")

# Loop through each dummy variable
for (i in seq_along(dummy_vars)) {
  var_name <- dummy_vars[i]
  plot_title <- paste("Mean Extraction OA with Confidence Intervals, by", plot_titles[i])
  
  # Summarize data: mean extraction and confidence intervals
  summarized_data <- dfs_long %>%
    group_by(treatment, round, !!sym(var_name)) %>%
    summarize(
      extraction_OA_mean = mean(extraction_OA, na.rm = TRUE),
      extraction_OA_se = sd(extraction_OA, na.rm = TRUE) / sqrt(n()), # Standard Error
      .groups = "drop"
    ) %>%
    mutate(
      lower_ci = extraction_OA_mean - qt(0.975, df = n() - 1) * extraction_OA_se,
      upper_ci = extraction_OA_mean + qt(0.975, df = n() - 1) * extraction_OA_se
    )
  
  # Convert dummy variable to a factor for plotting
  summarized_data[[var_name]] <- as.factor(summarized_data[[var_name]])
  
  # Generate the plot
  plot <- ggplot(summarized_data, aes(x = round, y = extraction_OA_mean, color = !!sym(var_name))) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = !!sym(var_name)), alpha = 0.2, color = NA) +
    facet_wrap(~treatment) +
    labs(
      title = plot_title,
      x = "Round",
      y = "Mean Extraction OA",
      color = plot_titles[i],
      fill = plot_titles[i]
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  # Display the plot
  print(plot)
  
  # Save each plot as a PDF
  ggsave(
    filename = paste0(path_github, "Outputs/Plot_extraction_", var_name, ".pdf"),
    plot = plot, device = "pdf", width = 12, height = 6
  )
}




##############################################
#### Data for plots of beliefs mean (ini+fin/2)
##################################


# Create a vector with variable names
variable_names <- c(
  "beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini",
  "beliefsT1final.1.player.T1_belief_caleta_en_amerb_fin",
  "beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini",
  "beliefsT1final.1.player.T1_belief_caleta_en_libre_fin",
  "beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini",
  "beliefsT1final.1.player.T1_belief_pm_en_libre_fin",
  "beliefsT2inicial.1.player.T2_belief_caleta_ini",
  "beliefsT2final.1.player.T2_belief_caleta_fin",
  "beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini",
  "beliefsT2inicial.1.player.T2_belief_caleta_conocida1_ini",
  "beliefsT2final.1.player.T2_belief_caleta_conocida1_fin",
  "beliefsT2inicial.1.player.T2_belief_caleta_conocida2_ini",
  "beliefsT2final.1.player.T2_belief_caleta_conocida2_fin"
)

# Subset the dataframe using the variable names
df_bfs <- df[, variable_names]


# Reshape data for visualization and comparisons
long_df_bfs <- df_bfs %>%
  pivot_longer(
    cols = starts_with("beliefs"),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Treatment = case_when(
      str_detect(Variable, "T1") ~ "T1",
      str_detect(Variable, "T2") ~ "T2",
      TRUE ~ NA_character_
    ),
    Area = case_when(
      str_detect(Variable, "amerb") ~ "ingroup_amerb",
      str_detect(Variable, "caleta_en_libre") ~ "ingroup_OA",
      str_detect(Variable, "pm") ~ "others_OA",
      str_detect(Variable, "caleta_ini") ~ "ingroup_OA",
      str_detect(Variable, "caleta_fin") ~ "ingroup_OA",
      str_detect(Variable, "conocida") ~ "others_OA",
      TRUE ~ "Other"
    )
  )

# Calculate means, SD, and 95% CI
plot_stats <- long_df_bfs %>%
  group_by(Area, Treatment) %>%
  summarize(
    mean = mean(Value, na.rm = TRUE),
    sd = sd(Value, na.rm = TRUE),
    n = sum(!is.na(Value)),  # Count of non-NA values
    .groups = "drop"  # Avoid nested group structure
  ) %>%
  mutate(
    lower_ci = mean - 1.96 * (sd / sqrt(n)),
    upper_ci = mean + 1.96 * (sd / sqrt(n))
  )

# View the resulting dataframe
print(plot_stats)

# Reorder the table so ingroup_amerb is first
beliefs_stats <- plot_stats %>%
  arrange(factor(Area, levels = c("ingroup_amerb", "ingroup_OA", "others_OA")), Treatment)

# View the ordered table
print(beliefs_stats)


######################################
#### Data for plots of beliefs initial
######################################


# Create a vector with variable names
var_names<- c(
  "beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini",
  "beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini",
  "beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini",
  "beliefsT2inicial.1.player.T2_belief_caleta_ini",
  "beliefsT2inicial.1.player.T2_belief_caleta_conocida1_ini",
  "beliefsT2inicial.1.player.T2_belief_caleta_conocida2_ini")

# Subset the dataframe using the variable names
df_bfs_ini <- df[, var_names]

# Reshape data for visualization and comparisons
long_df_bfs_ini <- df_bfs_ini %>%
  pivot_longer(
    cols = starts_with("beliefs"),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Treatment = case_when(
      str_detect(Variable, "T1") ~ "T1",
      str_detect(Variable, "T2") ~ "T2",
      TRUE ~ NA_character_
    ),
    Area = case_when(
      str_detect(Variable, "amerb") ~ "ingroup_amerb",
      str_detect(Variable, "caleta_en_libre") ~ "ingroup_OA",
      str_detect(Variable, "pm") ~ "others_OA",
      str_detect(Variable, "caleta_ini") ~ "ingroup_OA",
      str_detect(Variable, "caleta_fin") ~ "ingroup_OA",
      str_detect(Variable, "conocida") ~ "others_OA",
      TRUE ~ "Other"
    )
  )



# Calculate means, SD, and 95% CI
plot_stats <- long_df_bfs_ini %>%
  group_by(Area, Treatment) %>%
  summarize(
    mean = mean(Value, na.rm = TRUE),
    sd = sd(Value, na.rm = TRUE),
    n = sum(!is.na(Value)),  # Count of non-NA values
    .groups = "drop"  # Avoid nested group structure
  ) %>%
  mutate(
    lower_ci = mean - 1.96 * (sd / sqrt(n)),
    upper_ci = mean + 1.96 * (sd / sqrt(n))
  )

# View the resulting dataframe
print(plot_stats)

# Reorder the table so ingroup_amerb is first
beliefs_stats_ini <- plot_stats %>%
  arrange(factor(Area, levels = c("ingroup_amerb", "ingroup_OA", "others_OA")), Treatment)



#########################################
### Differences in beliefs
#########################################
# Create a new data frame with differences between `fin` and `ini`
df_diff <- df_bfs %>%
  mutate(
    T1_diff_amerb = beliefsT1final.1.player.T1_belief_caleta_en_amerb_fin - 
      beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini,
    T1_diff_libre = beliefsT1final.1.player.T1_belief_caleta_en_libre_fin - 
      beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini,
    T1_diff_pm = beliefsT1final.1.player.T1_belief_pm_en_libre_fin - 
      beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini,
    T2_diff_caleta = beliefsT2final.1.player.T2_belief_caleta_fin - 
      beliefsT2inicial.1.player.T2_belief_caleta_ini,
    T2_diff_pm = if_else(
      is.na(beliefsT2final.1.player.T2_belief_caleta_conocida2_fin) | 
        is.na(beliefsT2inicial.1.player.T2_belief_caleta_conocida2_ini),
      as.double(beliefsT2final.1.player.T2_belief_caleta_conocida1_fin - 
                  beliefsT2inicial.1.player.T2_belief_caleta_conocida1_ini),
      as.double((beliefsT2final.1.player.T2_belief_caleta_conocida1_fin - 
                   beliefsT2inicial.1.player.T2_belief_caleta_conocida1_ini + 
                   beliefsT2final.1.player.T2_belief_caleta_conocida2_fin - 
                   beliefsT2inicial.1.player.T2_belief_caleta_conocida2_ini) / 2)
    )
  )



# Reshape the differences into a long format for summarization
long_diff <- df_diff %>%
  select(starts_with("T1_diff"), starts_with("T2_diff")) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Treatment", "Variable"),
    names_sep = "_diff_",
    values_to = "Difference"
  ) %>%
  mutate(Treatment = ifelse(str_detect(Treatment, "T1"), "T1", "T2"))

# Calculate mean and SD of the differences
summary_table <- long_diff %>%
  group_by(Treatment, Variable) %>%
  summarize(
    mean_diff = mean(Difference, na.rm = TRUE),
    sd_diff = sd(Difference, na.rm = TRUE),
    .groups = "drop"
  )


# Add confidence intervals to the summary table
summary_table <- summary_table %>%
  mutate(
    n = nrow(df_diff),  # Use the total number of rows as n
    lower_ci = mean_diff - 1.96 * (sd_diff / sqrt(n)),
    upper_ci = mean_diff + 1.96 * (sd_diff / sqrt(n))
  )

# Create a combined sorting key (to ensure T1 precedes T2 for each variable)
summary_table <- summary_table %>%
  arrange(desc(mean_diff)) %>% # Sort by mean_diff in descending order
  mutate(
    x_order = factor(paste(Treatment, Variable), 
                     levels = unique(paste(Treatment, Variable)))
  )

# Create the plot with ordered x-axis
ggplot(summary_table, aes(x = x_order, y = mean_diff, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    position = position_dodge(0.8),
    width = 0.25
  ) +
  labs(
    title = "",
    x = "",
    y = "Mean Difference in beliefs (End minus Beggining)"
  ) +
  scale_fill_manual(values = c("T1" = "blue", "T2" = "red")) +
  scale_x_discrete(labels = c(
    "T1 amerb" = "Ingroup in Amerb",
    "T1 libre" = "Ingroup in Open Access",
    "T1 pm" = "Strangers in Open Access",
    "T2 caleta" = "Ingroup in Metaturf",
    "T2 pm" = "Named fishers in Metaturf"#, 
    #"T2 conocida2" = "Named fishers 2 in Metaturf"
  )) +  # Custom x-axis labels
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5)  # Center the title
  )



###########################################################################
#### Data management for plots of mean extraction (all 8 rounds per treatment) 
###########################################################################

# Calculate mean, SD, and 95% CI for each treatment and area
extraction_stats <- dfs_long %>%
  group_by(treatment) %>%  # Group by treatment and area
  summarise(
    mean_extraction_amerb = mean(extraction_amerb, na.rm = TRUE),
    sd_extraction = sd(extraction_amerb, na.rm = TRUE),
    n_amerb = sum(!is.na(extraction_amerb)),  # Count of non-missing values
    mean_extraction_OA = mean(extraction_OA, na.rm = TRUE),
    sd_extraction_OA = sd(extraction_OA, na.rm = TRUE),
    n_OA = sum(!is.na(extraction_OA)),  # Count of non-missing values
    
        .groups = "drop"  # Avoid nested grouping
  ) %>%
  mutate(
    lower_ci_amerb = mean_extraction_amerb - 1.96 * (sd_extraction_amerb / sqrt(n_amerb)),  # Lower bound of 95% CI
    upper_ci_amerb = mean_extraction_amerb + 1.96 * (sd_extraction_amerb / sqrt(n_amerb)),   # Upper bound of 95% CI
    lower_ci_OA = mean_extraction_OA - 1.96 * (sd_extraction_OA / sqrt(n_OA)),  # Lower bound of 95% CI
    upper_ci_OA = mean_extraction_OA + 1.96 * (sd_extraction_OA / sqrt(n_OA))   # Upper bound of 95% CI
    )

# View the resulting subset
print(extraction_stats)

# Bar plot with confidence intervals and custom y-axis limits
ggplot(extraction_stats, aes(x = area, y = mean_extraction, fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +  # Bar plot
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    position = position_dodge(width = 0.8),
    width = 0.25
  ) +  # Error bars for 95% CI
  scale_y_continuous(limits = c(0, 50)) +  # Set y-axis range to 0-50
  labs(
    title = "Mean Extraction with 95% CI by Area and Treatment",
    x = "Area",
    y = "Mean Extraction",
    fill = "Treatment"
  ) +
  theme_minimal() +  # Clean theme
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )





########################################################
#### Data management for plots of mean extraction by others
########################################################

df_o<- df %>%
  select(contains("otros"))
names(df_o)


df_o_long <- df_o %>%
  pivot_longer(
    cols = starts_with("T"),   # All columns starting with "T" (T1 and T2 variables)
    names_to = c("treatment", "round", "area"),  # Split the names into three parts
    names_pattern = "(T\\d)juegoalgas\\.(\\d+)\\.player\\..+_extraccion_(.+)",  # Regex to extract treatment, round, and variable
    values_to = "OE_others"   # Name of the column for values
  ) %>%
  mutate(round = as.integer(round))  # Ensure round is numeric

print(df_o_long_ini)

df_o_long$mean_OE_o<-df_o_long$OE_others/3

OE_o_stats <- df_o_long %>%
  group_by(treatment, area) %>%  # Group by treatment and area
  summarise(
    mean_extraction_o = mean(mean_OE_o, na.rm = TRUE),
    sd_extraction_o = sd(mean_OE_o, na.rm = TRUE),
    n = sum(!is.na(mean_OE_o)),  # Count of non-missing values
    .groups = "drop"  # Avoid nested grouping
  ) %>%
  mutate(
    lower_ci = mean_extraction_o - 1.96 * (sd_extraction_o / sqrt(n)),  # Lower bound of 95% CI
    upper_ci = mean_extraction_o + 1.96 * (sd_extraction_o / sqrt(n))   # Upper bound of 95% CI
  )

###########################################
### Plots Beliefs and Mean Over-extraction
##########################################

##### Plots for mean beliefs 
plot1 <- ggplot(beliefs_stats, aes(x = Area, y = mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +  # Bar plot
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    position = position_dodge(width = 0.8),
    width = 0.25
  ) +  # Error bars
  labs(
    #title = "Mean Beliefs about others",
    x = "",
    y = "Mean beliefs of over-extraction by others",
    fill = "Treatment"
  ) +
  scale_y_continuous(limits = c(0, 50)) +  # Set y-axis limits
  scale_x_discrete(labels = c(
    "ingroup_amerb" = "Ingroup in Amerb",
    "ingroup_OA" = "Ingroup in Other Area",
    "others_OA" = "Strangers in Other Area"
  )) +  # Custom x-axis labels
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Plot for initial beliefs  
plot2 <- ggplot(beliefs_stats_ini, aes(x = Area, y = mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +  # Bar plot
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    position = position_dodge(width = 0.8),
    width = 0.25
  ) +  # Error bars
  labs(
    #title = "Initial Beliefs about Others",
    x = "",
    y = "Round 0 beliefs of over-extraction by others",
    fill = "Treatment"
  ) +
  scale_y_continuous(limits = c(0, 50)) +  # Set y-axis limits
  scale_x_discrete(labels = c(
    "ingroup_amerb" = "Ingroup in Amerb",
    "ingroup_OA" = "Ingroup in Other Area",
    "others_OA" = "Strangers in Other Area"
  )) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Plot for personal extraction (mean rounds 1-8)
plot3 <- ggplot(extraction_stats, aes(x = area, y = mean_extraction, fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    position = position_dodge(width = 0.8),
    width = 0.25
  ) +
  scale_y_continuous(limits = c(0, 50)) +
  scale_x_discrete(labels = c(
    "amerb" = "Person in Amerb",
    "libre" = "Person in Open Acces (T1)",
    "metat" = "Person in Metaturf (T2)"
  )) +
  #scale_fill_manual(values = c("#1b9e77", "#d95f02")) +  # Custom colors for treatments
  labs(
    #title = "Personal Over-extraction",
    x = "",
    y = "Mean personal Over-extraction",
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot4<- ggplot(OE_o_stats, aes(x = area, y = mean_extraction_o, fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +  # Bar plot
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    position = position_dodge(width = 0.8),
    width = 0.25
  ) +  # Error bars for 95% CI
  scale_y_continuous(limits = c(0, 50)) +  # Set y-axis range to 0-50
  scale_x_discrete(labels = c(
    "otros_amerb" = "Ingroup in Amerb",
    "otros_libre" = "Hetero. group in Open Access",
    "otros_metat" = "Hetero. group in Metaturf"
  )) +
  labs(
    #title = "Over-extraction by others",
    x = "",
    y = "Mean Extraction Others in Same Group",
    fill = "Treatment"
  ) +
  theme_minimal() +  # Clean theme
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )



# Combine the two plots vertically
combined_plot1 <- plot1 + plot4 + plot3# Stack the plots using patchwork
combined_plot2 <- plot2 + plot4 + plot3 # Stack the plots using patchwork

# Display the combined plot
print(combined_plot1)
print(combined_plot2)


ggsave( file=paste0(path_github, "Outputs/Plot_beliefs_extraction.pdf") , plot = combined_plot2, device = "pdf", width = 12, height = 6)



#################################################
#### Diff between T2 and T1, per extraction area
#################################################
rm_wide <- rm %>%
  mutate(area = ifelse(area %in% c("metat", "libre"), "other_area", "amerb")) %>%  # Update area values
  pivot_wider(
    names_from = treatment,  # Create columns based on treatment
    values_from = mean_extraction  # Populate with mean_extraction values
  ) %>%
  mutate(diff = T2 - T1)  # Add the difference between T2 and T1


pdid<-ggplot(rm_wide, aes(x = round, y = diff, color = area, group = area)) +
  geom_line() +  # Line plot for each extraction type
  geom_point() +  # Add points to indicate the data
  labs(
    title = "Difference between T2 and T1 per Round",
    x = "Round",
    y = "Difference (T2 - T1)",
    color = "Extraction Area"
  ) +
  scale_x_continuous(breaks = 1:10) + 
  theme_minimal() +  # Use a minimal theme for better visualization
  theme(legend.position = "top") 
pdid
ggsave( file=paste0(path_github, "Outputs/plot_difference_T2_T1.pdf") , plot = pdid, device = "pdf", width = 8, height = 6)




##################################################
### Diff in Amerb - Other area per treatment
##################################################

diff_area <- rm %>%
  pivot_wider(
    names_from = area,        # Spread area (libre, metat, amerb) into separate columns
    values_from = mean_extraction  # The mean extraction values
  ) %>%
  arrange(round) %>%
  mutate(otra_zona = coalesce(libre, metat),
         diff = otra_zona - amerb)  # Calculate the difference between T1 and T2

# Step 3: Plot the difference per round for amerb and libre
pdiff<-ggplot(diff_area , aes(x = round, y = diff, color = treatment, group = treatment)) +
  geom_line() +  # Line plot for each extraction type
  geom_point() +  # Add points to indicate the data
  labs(
    title = "In and out-group bias ",
    x = "Round",
    y = "Difference (Otra zona- Amerb)",
    color = "Tratamiento"
  ) +
  scale_x_continuous(breaks = 1:10) + 
  theme_minimal() +  # Use a minimal theme for better visualization
  theme(legend.position = "top") 
pdiff

ggsave( file=paste0(path_github, "Outputs/plot_difference_amerb_otrazona.pdf") , plot = pdiff, device = "pdf", width = 8, height = 6)


###########################################
### Beliefs
###########################################

#Scatterplot of extraction given beliefs in AMERB
ba<-ggplot(df, aes(x = beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini, 
                   y = T1juegoalgas.1.player.T1_extraccion_amerb)) +
  geom_point() +  # Scatter plot points
  geom_smooth(method = "lm", se = TRUE) +  # Adds lm line with confidence intervals
  labs(
    title = "Extraction AMERB given Beliefs - Ingroup",
    x = "Beliefs Ingroup in Amerb - T1 Round 0",
    y = "Player Extraction Amerb - T1 Round 1"
  ) +
  theme_minimal()
ba

lm<-lm(T1juegoalgas.1.player.T1_extraccion_amerb ~ beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini, data=df)
summary(lm)

ggsave( file=paste0(path_github, "Outputs/Extraction_Amerb_Beliefs.pdf") , plot = ba, device = "pdf", width = 8, height = 6)


#Scatterplot of extraction given beliefs in open area

df_combined <- rbind(
  data.frame(
    Beliefs = df$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini,
    Extraction = df$T1juegoalgas.1.player.T1_extraccion_libre,
    Type = "Ingroup"
  ),
  data.frame(
    Beliefs = df$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini,
    Extraction = df$T1juegoalgas.1.player.T1_extraccion_libre,
    Type = "Outgroup"
  )
)

# Scatter plot with different color and shape for each belief type
boa<-ggplot(df_combined, aes(x = Beliefs, y = Extraction, color = Type, shape = Type)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +  # Adds lm line with confidence intervals for each type
  labs(
    title = "Extraction Open Access given Ingroup and Outgroup Beliefs",
    x = "Beliefs in Libre - Ingroup and Outgroup",
    y = "Player Extraction Libre"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Ingroup" = "blue", "Outgroup" = "red")) +  # Optional color customization
  scale_shape_manual(values = c("Ingroup" = 16, "Outgroup" = 17))  # Optional shape customization
boa

lm1<-lm(T1juegoalgas.1.player.T1_extraccion_libre ~ beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini, data=df)
summary(lm1)

lm2<-lm(T1juegoalgas.1.player.T1_extraccion_libre ~ beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini, data=df)
summary(lm2)

lm3<-lm(T1juegoalgas.1.player.T1_extraccion_libre ~ beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini+ beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini , data=df)
summary(lm3)


ggsave( file=paste0(path_github, "Outputs/Extraction_OA_Beliefs.pdf") , plot = boa, device = "pdf", width = 8, height = 6)


########################################
#### Means per group
########################################

#### means per groups in each
### var selection
rounds <- 1:8  # Sequence from 1 to 10
treats <- c("T1")  # Time periods T1 and T2
vars <- c("amerb")  # Extraction types

# Generate the list of variables using expand.grid to generate all combinations
combinations <- expand.grid(treats, rounds, vars)
vars <- paste0(
  "", combinations$Var1, "juegoalgas.", combinations$Var2, 
  ".player.", combinations$Var1, "_extraccion_", combinations$Var3
)



# Function: calculate means for each variable by group
calculate_means_by_group <- function(df, group_var, variable_names) {
  means_list <- list()
  
  for (var in variable_names) {
    if (var %in% colnames(df)) {
      # Calculate the mean by group for the current variable
      means <- df %>%
        group_by(!!sym(group_var)) %>%
        summarise(mean_extraction = mean(!!sym(var), na.rm = TRUE)) %>%
        mutate(variable = var)
      
      # Append the result to the list
      means_list[[var]] <- means
    } else {
      warning(paste("Variable", var, "not found in the data frame."))
    }
  }
  
  # Combine all the results into one data frame
  result <- bind_rows(means_list)
  return(result)
}

# Run the function to calculate the means
gid.means.amerb.t1 <- calculate_means_by_group(df, "gid.amerb", vars)

gid.means.amerb.t1$round <- str_extract(gid.means.amerb.t1$variable, "(?<=\\.)\\d+(?=\\.)")

# Mean payoff per round T1 amerb
gid.means.amerb.t1$round <- as.numeric(gid.means.amerb.t1$round)

# Create the line plot 1
p1<-ggplot(gid.means.amerb.t1, aes(x = round, y = mean_extraction, color = as.factor(gid.amerb), group = gid.amerb)) +
  geom_line() +  # Line for each gid.amerb group 
  scale_y_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 50)) + 
  geom_point() +  # Optional: Add points on the lines for clarity
  labs(
    title = "T1 AMERB",
    x = "Round",
    y = "Mean extraction by group amerb T1",
    color = "Group (gid.amerb)"
  ) +
  theme_minimal() +  # Use a clean theme for better visualization
  theme(legend.position = "none")
p1



#### Graph amerb t2

rounds <- 1:10  # Sequence from 1 to 10
treats <- c("T2")  # Time periods T1 and T2
vars <- c("amerb")  # Extraction types

# Generate the list of variables using expand.grid to generate all combinations
combinations <- expand.grid(treats, rounds, vars)
vars <- paste0(
  "", combinations$Var1, "juegoalgas.", combinations$Var2, 
  ".player.", combinations$Var1, "_extraccion_", combinations$Var3
)

gid.means.amerb.t2 <- calculate_means_by_group(df, "gid.amerb", vars)

gid.means.amerb.t2$round <- str_extract(gid.means.amerb.t2$variable, "(?<=\\.)\\d+(?=\\.)")

gid.means.amerb.t2$round <- as.numeric(gid.means.amerb.t2$round)

# Create the line plot 1
p2<-ggplot(gid.means.amerb.t2, aes(x = round, y = mean_extraction, color = as.factor(gid.amerb), group = gid.amerb)) +
  geom_line() +  # Line for each gid.amerb group
  scale_y_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 50)) + 
  geom_point() +  # Optional: Add points on the lines for clarity
  labs(
    title = "T2 - AMERB",
    x = "Round",
    y = "Mean extraction by group amerb T2",
    color = "Group (gid.amerb)"
  ) +
  theme_minimal()  # Use a clean theme for better visualization

p2

grid.arrange(p1, p2, ncol = 2)



#### means per groups in each
### var selection
rounds <- 1:10  # Sequence from 1 to 10
treats <- c("T1")  # Time periods T1 and T2
vars <- c("libre")  # Extraction types

# Generate the list of variables using expand.grid to generate all combinations
combinations <- expand.grid(treats, rounds, vars)
vars <- paste0(
  "", combinations$Var1, "juegoalgas.", combinations$Var2, 
  ".player.", combinations$Var1, "_extraccion_", combinations$Var3
)


# Run the function to calculate the means
gid.means.zl.t1 <- calculate_means_by_group(df, "participant.zonaT2", vars)

gid.means.zl.t1$round <- str_extract(gid.means.zl.t1$variable, "(?<=\\.)\\d+(?=\\.)")

# Mean payoff per round T1 zl
gid.means.zl.t1$round <- as.numeric(gid.means.zl.t1$round)

# Create the line plot 1
p3<-ggplot(gid.means.zl.t1, aes(x = round, y = mean_extraction, color = as.factor(participant.zonaT2), group = participant.zonaT2)) +
  geom_line() +  # Line for each gid.zl group 
  scale_y_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 50)) + 
  geom_point() +  # Optional: Add points on the lines for clarity
  labs(
    title = "T1 Zona Libre",
    x = "Round",
    y = "Mean extraction by group zona libre T1",
    color = "Group (gid.zl)"
  ) +
  theme_minimal() + # Use a clean theme for better visualization
  theme(legend.position = "none")
p3



#### Graph amerb t2

rounds <- 1:10  # Sequence from 1 to 10
treats <- c("T2")  # Time periods T1 and T2
vars <- c("metat")  # Extraction types

# Generate the list of variables using expand.grid to generate all combinations
combinations <- expand.grid(treats, rounds, vars)
vars <- paste0(
  "", combinations$Var1, "juegoalgas.", combinations$Var2, 
  ".player.", combinations$Var1, "_extraccion_", combinations$Var3
)

gid.means.zl.t2 <- calculate_means_by_group(df, "participant.zonaT2", vars)

gid.means.zl.t2$round <- str_extract(gid.means.zl.t2$variable, "(?<=\\.)\\d+(?=\\.)")

gid.means.zl.t2$round <- as.numeric(gid.means.zl.t2$round)

# Create the line plot 1
p4<-ggplot(gid.means.zl.t2, aes(x = round, y = mean_extraction, color = as.factor(participant.zonaT2), group = participant.zonaT2)) +
  geom_line() +  # Line for each gid.amerb group
  scale_y_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 50)) +
  geom_point() +  # Optional: Add points on the lines for clarity
  labs(
    title = "T2 - Meta-Turf",
    x = "Round",
    y = "Mean extraction by group Meta-Turf T2",
    color = "Group (gid.amerb)"
  ) +
  theme_minimal()  # Use a clean theme for better visualization

p4

grid.arrange(p1, p2, p3, p4, ncol = 2, nrow= 2)

ggsave( file=paste0(path_github, "outputs/group_means.pdf") , plot = p4, device = "pdf", width = 8, height = 6)




