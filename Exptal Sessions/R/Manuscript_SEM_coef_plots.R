# Setup: install & load packages
if (!requireNamespace("lavaan", quietly = TRUE)) install.packages("lavaan")
if (!requireNamespace("semPlot", quietly = TRUE)) install.packages("semPlot")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("viridis", quietly = TRUE)) install.packages("viridis")

library(lavaan)
library(semPlot)
library(ggplot2)
library(viridis)

rm(list=ls())

# Paths
path_github <- "C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos  <- "C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

# Load data
load(paste0(path_datos, "/Datos_islitas_recode.Rdata"))   # expects object 'df'
load(paste0(path_datos, "/Datos_islitas_long.Rdata"))     # expects object 'df_long'

# Helper: select columns by round
declare_get_columns <- function(prefix, suffix, start, end = NULL, data) {
  if (is.null(end)) end <- start
  rounds <- start:end
  cols <- unlist(lapply(rounds, function(r) {
    patt <- paste0("^", prefix, "\\.", r, "\\.player\\.", suffix, "$")
    grep(patt, names(data), value = TRUE)
  }))
  if (length(cols) == 0) stop("No matching columns for ", prefix, suffix, " rounds ", start, "-", end)
  cols
}


########################################
### TURF Rounds 2-8
########################################
set.seed(53698)
# SEM fit function, returns lavaan fit
coef_TURF_T1_sem <- function(data, R, N) {
  # compute averages and compliance
  cols_t1 <- declare_get_columns("T1juegoalgas", "T1_extraccion_amerb", R, N, data)
  subset_ini <- data[, cols_t1, drop = FALSE]
  data$average_extraction_ini   <- rowMeans(subset_ini, na.rm = TRUE)
  data$average_compliance_ini   <- 1 - data$average_extraction_ini / 50
  data$belief_compliance_amerb  <- 1 - data$beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini / 50
  
  # lagged observed compliance
  if (R > 1) {
    cols_obs <- declare_get_columns("T1juegoalgas", "T1_extraccion_otros_amerb", R - 1, R - 1, data)
    subset_obs <- data[, cols_obs, drop = FALSE]
    data$average_extraction_observed_ini     <- rowMeans(subset_obs, na.rm = TRUE)
    data$average_compliance_observed_ini_lag <- 1 - data$average_extraction_observed_ini / (50 * length(cols_obs))
  } else {
    data$average_compliance_observed_ini_lag <- NA
  }
  
  # specify SEM
  sem_model <- ' 
    belief_compliance_amerb ~ survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
    average_compliance_ini    ~ belief_compliance_amerb + survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta + average_compliance_observed_ini_lag
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_amerb
  '
  
  # fit
  fit <- sem(sem_model,
             data      = data,
             estimator = "ML",
             se        = "bootstrap",
             bootstrap = 200,
             parallel  = "multicore",
             ncpus     = 4)
  return(fit)
}

# Loop over rounds 2:8, collect significant coefficients
round_vals  <- 2:8
N<-8
sem_results <- data.frame(
  round  = integer(),
  lhs    = character(),
  rhs    = character(),
  est    = numeric(),
  se     = numeric(),
  pvalue = numeric(),
  stringsAsFactors = FALSE
)

for (R in round_vals) {
  fit <- coef_TURF_T1_sem(df, R, N)
  pe  <- parameterEstimates(fit)
  sig <- subset(pe, op == "~" & pvalue < 0.05,
                select = c("lhs","rhs","est","se","pvalue"))
  if (nrow(sig) > 0) {
    sig$round <- R
    sig$endR<-N
    sem_results <- rbind(sem_results, sig)
  }
}

sem_results$label<-paste0("Mean rounds ",sem_results$round,"-",sem_results$endR)

# Split results by outcome
compliance_results <- subset(sem_results, lhs == "average_compliance_ini")
belief_results     <- subset(sem_results, lhs == "belief_compliance_amerb")

# Plot compliance coefficients with 95% CIs
compliance_results$round <- factor(compliance_results$round, levels = rev(round_vals))
compliance_results$rhs   <- factor(compliance_results$rhs,
                                   levels = unique(compliance_results$rhs))

p <- ggplot(compliance_results,
            aes(x = est,
                y = label,
                color = rhs)) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.6) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbarh(aes(xmin = est - 1.96 * se,
                     xmax = est + 1.96 * se),
                 position = position_dodge(width = 0.5),
                 height = 0.2,
                 size = 0.8) +
  scale_x_continuous(limits = c(-0.5, 1)) +
  scale_color_viridis_d(name = "Predictor",
                        labels = c(
                          "belief_compliance_amerb" = "Prior Beliefs",
                          "average_compliance_ini" = "Compliance",
                          "survey1.1.player.conflicto_caleta" = "Conflict TURF",
                          "average_compliance_observed_ini_lag" = "Updated Beliefs"
                        ),
                        option = "D") +
  labs(title = "Compliance in TURF Rounds 2 to 8",
       y = "Round",
       x = "Coefficient (95% CI)") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

print(p)
ggsave(file = paste0(path_github, "Outputs/Multiple_Coef_SEM_T1_Turf.pdf"), 
               plot = p, device = "pdf", width = 10, height = 8)

########################################
### TURF Rounds 9-16
########################################
set.seed(53698)
# SEM fit function, returns lavaan fit
coef_TURF_T2_sem <- function(data, R, N) {
  # compute averages and compliance
  cols_t1 <- declare_get_columns("T2juegoalgas", "T2_extraccion_amerb", R, N, data)
  subset_ini <- data[, cols_t1, drop = FALSE]
  data$average_extraction_ini   <- rowMeans(subset_ini, na.rm = TRUE)
  data$average_compliance_ini   <- 1 - data$average_extraction_ini / 50
  data$belief_compliance_amerb  <- 1 - data$beliefsT1final.1.player.T1_belief_caleta_en_amerb_fin / 50
  
  # lagged observed compliance
  if (R > 1) {
    cols_obs <- declare_get_columns("T2juegoalgas", "T2_extraccion_otros_amerb", R - 1, R - 1, data)
    subset_obs <- data[, cols_obs, drop = FALSE]
    data$average_extraction_observed_ini     <- rowMeans(subset_obs, na.rm = TRUE)
    data$average_compliance_observed_ini_lag <- 1 - data$average_extraction_observed_ini / (50 * length(cols_obs))
  } else {
    data$average_compliance_observed_ini_lag <- NA
  }
  
  # specify SEM
  sem_model <- ' 
    belief_compliance_amerb ~ survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
    average_compliance_ini    ~ belief_compliance_amerb + survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta + average_compliance_observed_ini_lag
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_amerb
  '
  
  # fit
  fit <- sem(sem_model,
             data      = data,
             estimator = "ML",
             se        = "bootstrap",
             bootstrap = 200,
             parallel  = "multicore",
             ncpus     = 4)
  return(fit)
}

# Loop over rounds 2:8, collect significant coefficients
round_vals  <- 2:8
N<-8
sem_results <- data.frame(
  round  = integer(),
  lhs    = character(),
  rhs    = character(),
  est    = numeric(),
  se     = numeric(),
  pvalue = numeric(),
  stringsAsFactors = FALSE
)

for (R in round_vals) {
  fit <- coef_TURF_T2_sem(df, R, N)
  pe  <- parameterEstimates(fit)
  sig <- subset(pe, op == "~" & pvalue < 0.05,
                select = c("lhs","rhs","est","se","pvalue"))
  if (nrow(sig) > 0) {
    sig$round <- R
    sig$endR<-N
    sem_results <- rbind(sem_results, sig)
  }
}

sem_results$label<-paste0("Mean rounds ",sem_results$round,"-",sem_results$endR)

# Split results by outcome
compliance_results <- subset(sem_results, lhs == "average_compliance_ini")
belief_results     <- subset(sem_results, lhs == "belief_compliance_amerb")

cols <- viridis(3, option = "D")
desired_cols <- cols[c(1,2)]  # purple & mid

# Plot compliance coefficients with 95% CIs
compliance_results$round <- factor(compliance_results$round, levels = rev(round_vals))
compliance_results$rhs   <- factor(compliance_results$rhs, levels = unique(compliance_results$rhs))

p <- ggplot(compliance_results, aes(x = est, y = label, color = rhs)) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.6) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbarh(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se),
                 position = position_dodge(width = 0.5), height = 0.2, size = 0.8) +
  scale_x_continuous(limits = c(-0.5, 1)) +
   scale_color_viridis_d(name = "Predictor",
                     labels = c(
                       "belief_compliance_amerb" = "Prior Beliefs",
                       "average_compliance_ini" = "Compliance",
                       "survey1.1.player.conflicto_caleta" = "Conflict TURF",
                       "average_compliance_observed_ini_lag" = "Updated Beliefs"
                     ), option = "D") +
  labs(title = "Compliance in TURF Rounds 9 to 16",
       y = "Round",
       x = "Coefficient (95% CI)") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))

print(p)
ggsave(file = paste0(path_github, "Outputs/Multiple_Coef_SEM_T2_Turf.pdf"), 
       plot = p, device = "pdf", width = 10, height = 8)


##############################################
#### Shared Area Rounds 1-8 Unknown Outsiders
################################################
set.seed(478)
coef_SA_T1_sem <- function(data, R, N) {
  # compute averages and compliances
  cols <- declare_get_columns("T1juegoalgas", "T1_extraccion_libre", R, N, data)
  subset_ini <- data[, cols, drop = FALSE]
  data$average_extraction_ini <- rowMeans(subset_ini, na.rm = TRUE)
  data$average_compliance_ini <- 1 - data$average_extraction_ini / 50
  data$belief_compliance_pm   <- 1 - data$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50
  data$belief_compliance_union<- 1 - data$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50
  
  # lagged observed compliance
  if (R > 1) {
    cols_obs <- declare_get_columns("T1juegoalgas", "T1_extraccion_otros_libre", R - 1, R - 1, data)
    subset_obs <- data[, cols_obs, drop = FALSE]
    data$average_extraction_observed_ini     <- rowMeans(subset_obs, na.rm = TRUE)
    data$average_compliance_observed_ini_lag <- 1 - data$average_extraction_observed_ini / (50 * length(cols_obs))
  } else {
    data$average_compliance_observed_ini_lag <- NA
  }
  
  # SEM model specification
  sem_model <- '
    belief_compliance_pm ~ survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm
    belief_compliance_union ~ survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
    average_compliance_ini ~ belief_compliance_pm + belief_compliance_union + survey1.1.player.confianza_pm + survey1.1.player.conflicto_pm + survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta + average_compliance_observed_ini_lag
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_union
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_pm
  '
  
  # Fit SEM with clustering and bootstrap
  fit <- sem(sem_model,
             data      = data,
             estimator = "ML",
             se        = "bootstrap",
             bootstrap = 200,
             parallel  = "multicore",
             ncpus     = 4
             )
  return(fit)
}

# Loop over rounds 2-8, collect significant coefficients
round_vals <- 2:8
N<-8
sem_results <- data.frame(
  round  = integer(),
  lhs    = character(),
  rhs    = character(),
  est    = numeric(),
  se     = numeric(),
  pvalue = numeric(),
  stringsAsFactors = FALSE
)

for (R in round_vals) {
  fit <- coef_SA_T1_sem(df, R, N)
  pe  <- parameterEstimates(fit)
  sig <- subset(pe, op == "~" & pvalue < 0.05,
                select = c("lhs","rhs","est","se","pvalue"))
  if (nrow(sig) > 0) {
    sig$round <- R
    sig$endR<-N
    sem_results <- rbind(sem_results, sig)
  }
}

sem_results$label<-paste0("Mean rounds ",sem_results$round,"-",sem_results$endR)

# Split results by outcome
compliance_results   <- subset(sem_results, lhs == "average_compliance_ini")
belief_pm_results    <- subset(sem_results, lhs == "belief_compliance_pm")
belief_union_results <- subset(sem_results, lhs == "belief_compliance_union")

# Inspect results
print(compliance_results)
print(belief_pm_results)
print(belief_union_results)

# Plot compliance coefficients with 95% CIs

p <- ggplot(compliance_results,
            aes(x = est,
                y = label,
                color = rhs)) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.6) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbarh(aes(xmin = est - 1.96 * se,
                     xmax = est + 1.96 * se),
                 position = position_dodge(width = 0.5),
                 height = 0.2,
                 size = 0.8) +
  scale_x_continuous(limits = c(-0.5, 1)) +
  scale_color_viridis_d(name = "Predictor",
                        option = "D",
                        labels = c(
                          "belief_compliance_pm"    = "Prior Beliefs Outsiders",
                          "belief_compliance_union" = "Prior Beliefs TURF",
                          "survey1.1.player.conflicto_pm"     = "Conflict Outsiders",
                          "survey1.1.player.conflicto_caleta" = "Conflict TURF",
                          "survey1.1.player.confianza_pm"     = "Trust Outsiders",
                          "survey1.1.player.confianza_caleta" = "Trust TURF",
                          "average_compliance_observed_ini_lag" = "Updated Beliefs"
                        )) +
  labs(title = "Compliance in Shared Area (Unknown Outsiders) Rounds 2 to 8",
       y     = "Round",
       x     = "Coefficient (95% CI)") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))
print(p)
ggsave(file = paste0(path_github, "Outputs/Multiple_Coef_SEM_T1_SA.pdf"), 
       plot = p, device = "pdf", width = 10, height = 8)


##############################################
#### Shared Area Rounds 9-16 Known Outsiders
################################################
set.seed(4523)
coef_SA_T2_sem <- function(data, R, N) {
  # compute averages and compliances
  cols <- declare_get_columns("T2juegoalgas", "T2_extraccion_metat", R, N, data)
  subset_ini <- data[, cols, drop = FALSE]
  data$average_extraction_ini <- rowMeans(subset_ini, na.rm = TRUE)
  data$average_compliance_ini <- 1 - data$average_extraction_ini / 50
  data$belief_compliance_pm   <- 1 - data$beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini / 50
  df$belief_compliance_union <- 1 - (df$beliefsT2inicial.1.player.T2_belief_caleta_ini / 50)
  
  # lagged observed compliance
  if (R > 1) {
    cols_obs <- declare_get_columns("T2juegoalgas", "T2_extraccion_otros_metat", R - 1, R - 1, data)
    subset_obs <- data[, cols_obs, drop = FALSE]
    data$average_extraction_observed_ini     <- rowMeans(subset_obs, na.rm = TRUE)
    data$average_compliance_observed_ini_lag <- 1 - data$average_extraction_observed_ini / 50
  } else {
    data$average_compliance_observed_ini_lag <- NA
  }
  
  # SEM model specification
  sem_model <- '
    belief_compliance_pm ~ survey2.1.player.confianza_caleta_conocida_mean + survey2.1.player.conflicto_caleta_conocida_mean
    belief_compliance_union ~ survey1.1.player.confianza_caleta + survey1.1.player.conflicto_caleta
    average_compliance_ini ~ belief_compliance_pm + belief_compliance_union + survey2.1.player.confianza_caleta_conocida_mean + 
    survey2.1.player.conflicto_caleta_conocida_mean + survey1.1.player.confianza_caleta +
    survey1.1.player.conflicto_caleta + average_compliance_observed_ini_lag
    
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_union
    average_compliance_observed_ini_lag ~~ 0*belief_compliance_pm
  '
  
  fit <- sem(sem_model,
             data      = data,
             estimator = "ML",
             se        = "bootstrap",
             bootstrap = 200,
             parallel  = "multicore",
             ncpus     = 4
             )
  return(fit)
}

# Loop over rounds 9-16, collect significant coefficients
round_vals <- 2:8
N<-8
sem_results <- data.frame(
  round  = integer(),
  lhs    = character(),
  rhs    = character(),
  est    = numeric(),
  se     = numeric(),
  pvalue = numeric(),
  stringsAsFactors = FALSE
)
for (R in round_vals) {
  fit <- coef_SA_T2_sem(df, R, N)
  pe  <- parameterEstimates(fit)
  sig <- subset(pe, op == "~" & pvalue < 0.05,
                select = c("lhs","rhs","est","se","pvalue"))
  if (nrow(sig) > 0) {
    sig$round <- R
    sig$endR<-N
    sem_results <- rbind(sem_results, sig)
  }
}

sem_results$label<-paste0("Mean rounds ",sem_results$round,"-",sem_results$endR)
# Split results by outcome
compliance_results   <- subset(sem_results, lhs == "average_compliance_ini")
belief_pm_results    <- subset(sem_results, lhs == "belief_compliance_pm")
belief_union_results <- subset(sem_results, lhs == "belief_compliance_union")

# Inspect results
print(compliance_results)
print(belief_pm_results)
print(belief_union_results)

# Plot compliance coefficients with 95% CIs
compliance_results$round <- factor(compliance_results$round, levels = rev(round_vals))
compliance_results$rhs   <- factor(compliance_results$rhs, levels = unique(compliance_results$rhs))

p <- ggplot(compliance_results,
            aes(x = est,
                y = label,
                color = rhs)) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.6) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbarh(aes(xmin = est - 1.96 * se,
                     xmax = est + 1.96 * se),
                 position = position_dodge(width = 0.5),
                 height = 0.2,
                 size = 0.8) +
  scale_x_continuous(limits = c(-0.5, 1)) +
  scale_color_viridis_d(name = "Predictor",
                        option = "D",
                        labels = c(
                          "belief_compliance_pm"    = "Prior Beliefs Outsiders",
                          "belief_compliance_union" = "Prior Beliefs TURF",
                          "survey2.1.player.conflicto_caleta_conocida_mean"     = "Conflict Outsiders",
                          "survey1.1.player.conflicto_caleta" = "Conflict TURF",
                          "survey2.1.player.confianza_caleta_conocida_mean"     = "Trust Outsiders",
                          "survey1.1.player.confianza_caleta" = "Trust TURF",
                          "average_compliance_observed_ini_lag" = "Updated Beliefs"
                        )) +

  labs(title = "Compliance in Shared Area (Known Outsiders) Rounds 9 to 16",
       y     = "Round",
       x     = "Coefficient (95% CI)") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))

print(p)
ggsave(file = paste0(path_github, "Outputs/Multiple_Coef_SEM_T2_SA.pdf"), 
       plot = p, device = "pdf", width = 10, height = 8)
