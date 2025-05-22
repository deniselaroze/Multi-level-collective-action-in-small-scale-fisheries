
library(dplyr)
library(stargazer)
library(tidyr)
library(ggplot2)
library(viridis)

rm(list=ls())
path_github <-"C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos<-"C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

#path_github <-"C:/Users/Denise Laroze/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
#path_datos<-"C:/Users/Denise Laroze/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

setwd(path_github)


#load(paste0(path_datos, "/Datos_islitas.Rdata"))
load(paste0(path_datos, "/Datos_islitas_recode.Rdata"))
load(paste0(path_datos, "/Datos_islitas_long.Rdata"))



##### identifying data
#df_payoff <- df[, grep("payoff|pago", names(df), ignore.case = TRUE)]


df_payoff <- df[, grep("pago_acumulado", names(df), ignore.case = TRUE)]

##### Variables of interest

#df$T1juegoalgas.8.player.T1_pago_acumulado_amerb
#df$T1juegoalgas.8.player.T1_pago_acumulado_libre

#df$T2juegoalgas.8.player.T2_pago_acumulado_amerb
#df$T2juegoalgas.8.player.T2_pago_acumulado_metat
#df$gid.amerb

#df$gid.treat


df$ganancias_amerb<-df$T1juegoalgas.8.player.T1_pago_acumulado_amerb + df$T2juegoalgas.8.player.T2_pago_acumulado_metat
df$ganancias_OA<-df$T1juegoalgas.8.player.T1_pago_acumulado_libre + df$T2juegoalgas.8.player.T2_pago_acumulado_metat


### Descriptive statistics:
mean(df$T1juegoalgas.8.player.T1_pago_acumulado_libre)
sd(df$T1juegoalgas.8.player.T1_pago_acumulado_libre )

mean(df$T2juegoalgas.8.player.T2_pago_acumulado_metat)
sd(df$T2juegoalgas.8.player.T2_pago_acumulado_metat)


### group level 


#Amerb
df$belief_compliance_union_ini<-1-(df$beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini/50)
df$belief_compliance_union_fin<-1-(df$beliefsT1final.1.player.T1_belief_caleta_en_amerb_fin/50)

tmp<-df %>%
  group_by(gid.amerb) %>%
  summarise(
    mean_belief_ini  = mean(belief_compliance_union_ini, na.rm = TRUE),
    sd_belief_ini    = sd(belief_compliance_union_ini,   na.rm = TRUE),
    range_belief_ini = abs(
      max(belief_compliance_union_ini, na.rm = TRUE) -
        min(belief_compliance_union_ini, na.rm = TRUE)
    ),
    mean_belief_fin  = mean(belief_compliance_union_fin, na.rm = TRUE),
    sd_belief_fin    = sd(belief_compliance_union_fin,   na.rm = TRUE),
    range_belief_fin = abs(
      max(belief_compliance_union_fin, na.rm = TRUE) -
        min(belief_compliance_union_fin, na.rm = TRUE)
    ),
    group_payoff_t1 = mean(T1juegoalgas.8.player.T1_pago_acumulado_amerb , na.rm = TRUE),
    group_payoff_t2 = mean(T2juegoalgas.8.player.T2_pago_acumulado_amerb , na.rm = TRUE),
    group_payoff_tot = mean(ganancias_amerb , na.rm = TRUE),
    .groups = "drop"
  )


#### Regression analysis 


lm1<-lm(group_payoff_t1~  mean_belief_ini, tmp)
summary(lm1)


lm<-lm(group_payoff_t1~ sd_belief_ini, tmp)
summary(lm)

lm<-lm(group_payoff_t1~ range_belief_ini, tmp)
summary(lm)

lm<-lm(group_payoff_tot~ mean_belief_ini+ sd_belief_ini+ range_belief_ini +mean_belief_fin+ sd_belief_fin+ range_belief_fin, tmp)
summary(lm)


###### lm analyses 
# Load required package
library(stargazer)

# Individual models for group_payoff_t1
m1_t1 <- lm(group_payoff_t1 ~ mean_belief_ini, data = tmp)
summary(m1_t1)
m2_t1 <- lm(group_payoff_t1 ~ sd_belief_ini, data = tmp)
m3_t1 <- lm(group_payoff_t1 ~ range_belief_ini, data = tmp)
m4_t1 <- lm(group_payoff_t1 ~ mean_belief_fin, data = tmp)
m5_t1 <- lm(group_payoff_t1 ~ sd_belief_fin, data = tmp)
m6_t1 <- lm(group_payoff_t1 ~ range_belief_fin, data = tmp)

# Stargazer table for group_payoff_t1
stargazer(m1_t1, m2_t1, m3_t1, m4_t1, m5_t1, m6_t1,
          type = "text")

# Individual models for group_payoff_t2
m1_t2 <- lm(group_payoff_t2 ~ mean_belief_ini, data = tmp)
m2_t2 <- lm(group_payoff_t2 ~ sd_belief_ini, data = tmp)
m3_t2 <- lm(group_payoff_t2 ~ range_belief_ini, data = tmp)
m4_t2 <- lm(group_payoff_t2 ~ mean_belief_fin, data = tmp)
m5_t2 <- lm(group_payoff_t2 ~ sd_belief_fin, data = tmp)
m6_t2 <- lm(group_payoff_t2 ~ range_belief_fin, data = tmp)

# Stargazer table for group_payoff_t2
stargazer(m1_t2, m2_t2, m3_t2, m4_t2, m5_t2, m6_t2,
          type = "text")

# Individual models for group_payoff_tot
m1_tot <- lm(group_payoff_tot ~ mean_belief_ini, data = tmp)
m2_tot <- lm(group_payoff_tot ~ sd_belief_ini, data = tmp)
m3_tot <- lm(group_payoff_tot ~ range_belief_ini, data = tmp)
m4_tot <- lm(group_payoff_tot ~ mean_belief_fin, data = tmp)
m5_tot <- lm(group_payoff_tot ~ sd_belief_fin, data = tmp)
m6_tot <- lm(group_payoff_tot ~ range_belief_fin, data = tmp)


# Stargazer table for group_payoff_tot
stargazer(m1_tot, m2_tot, m3_tot, m4_tot, m5_tot, m6_tot,
          type = "text")


###################
#### Open Access
######################
df$belief_compliance_pm_OA_T1 <- 1 - (df$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50)
df$belief_compliance_pm_OA_T2 <- 1 - (df$beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini / 50)
df$belief_compliance_union_OA_T1 <- 1 - (df$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50)
df$belief_compliance_union_OA_T2 <- 1 - (df$beliefsT2inicial.1.player.T2_belief_caleta_ini / 50)


tmp2 <- df %>%
  group_by(gid.treat) %>%
  summarise(
    # PM OA T1
    mean_pm_OA_T1  = mean(belief_compliance_pm_OA_T1, na.rm = TRUE),
    sd_pm_OA_T1    = sd(  belief_compliance_pm_OA_T1, na.rm = TRUE),
    range_pm_OA_T1 = abs(
      max(belief_compliance_pm_OA_T1, na.rm = TRUE) - min(belief_compliance_pm_OA_T1, na.rm = TRUE)
    ),
    
    # PM OA T2
    mean_pm_OA_T2  = mean(belief_compliance_pm_OA_T2, na.rm = TRUE),
    sd_pm_OA_T2    = sd(  belief_compliance_pm_OA_T2, na.rm = TRUE),
    range_pm_OA_T2 = abs(
      max(belief_compliance_pm_OA_T2, na.rm = TRUE) - min(belief_compliance_pm_OA_T2, na.rm = TRUE)
    ),
    
    # UNION OA T1
    mean_union_OA_T1  = mean(belief_compliance_union_OA_T1, na.rm = TRUE),
    sd_union_OA_T1    = sd(  belief_compliance_union_OA_T1, na.rm = TRUE),
    range_union_OA_T1 = abs(
      max(belief_compliance_union_OA_T1, na.rm = TRUE) - min(belief_compliance_union_OA_T1, na.rm = TRUE)
    ),
    
    # UNION OA T2
    mean_union_OA_T2  = mean(belief_compliance_union_OA_T2, na.rm = TRUE),
    sd_union_OA_T2    = sd(  belief_compliance_union_OA_T2, na.rm = TRUE),
    range_union_OA_T2 = abs(
      max(belief_compliance_union_OA_T2, na.rm = TRUE) - min(belief_compliance_union_OA_T2, na.rm = TRUE)
    ),
    
    # PAYOFFS OA
    group_payoff_OA_t1 = mean(T1juegoalgas.8.player.T1_pago_acumulado_libre, na.rm = TRUE),
    group_payoff_OA_t2 = mean(T2juegoalgas.8.player.T2_pago_acumulado_metat, na.rm = TRUE),
    group_payoff_OA_tot = mean(ganancias_OA, na.rm = TRUE),
    
    .groups = "drop"
  )




#### Regression analysis 

##### Simple, bivariate models
# Individual models for group_payoff_OA_t1
m1_OA_t1 <- lm(group_payoff_OA_t1 ~ mean_pm_OA_T1, data = tmp2)
m2_OA_t1 <- lm(group_payoff_OA_t1 ~ sd_pm_OA_T1, data = tmp2)
m3_OA_t1 <- lm(group_payoff_OA_t1 ~ range_pm_OA_T1, data = tmp2)
m4_OA_t1 <- lm(group_payoff_OA_t1 ~ mean_union_OA_T1, data = tmp2)
m5_OA_t1 <- lm(group_payoff_OA_t1 ~ sd_union_OA_T1, data = tmp2)
m6_OA_t1 <- lm(group_payoff_OA_t1 ~ range_union_OA_T1, data = tmp2)

# Stargazer table for group_payoff_OA_t1
stargazer(m1_OA_t1, m2_OA_t1, m3_OA_t1, m4_OA_t1, m5_OA_t1, m6_OA_t1,
          type = "text",
          title = "Bivariate Regressions - Dependent Variable: group_payoff_OA_t1")

# Individual models for group_payoff_OA_t2
m1_OA_t2 <- lm(group_payoff_OA_t2 ~ mean_pm_OA_T2, data = tmp2)
m2_OA_t2 <- lm(group_payoff_OA_t2 ~ sd_pm_OA_T2, data = tmp2)
m3_OA_t2 <- lm(group_payoff_OA_t2 ~ range_pm_OA_T2, data = tmp2)
m4_OA_t2 <- lm(group_payoff_OA_t2 ~ mean_union_OA_T2, data = tmp2)
m5_OA_t2 <- lm(group_payoff_OA_t2 ~ sd_union_OA_T2, data = tmp2)
m6_OA_t2 <- lm(group_payoff_OA_t2 ~ range_union_OA_T2, data = tmp2)

# Stargazer table for group_payoff_OA_t2
stargazer(m1_OA_t2, m2_OA_t2, m3_OA_t2, m4_OA_t2, m5_OA_t2, m6_OA_t2,
          type = "text",
          title = "Bivariate Regressions - Dependent Variable: group_payoff_OA_t2")

# Individual models for group_payoff_OA_tot
m1_OA_tot <- lm(group_payoff_OA_tot ~ mean_pm_OA_T1, data = tmp2)
m2_OA_tot <- lm(group_payoff_OA_tot ~ sd_pm_OA_T1, data = tmp2)
m3_OA_tot <- lm(group_payoff_OA_tot ~ range_pm_OA_T1, data = tmp2)
m4_OA_tot <- lm(group_payoff_OA_tot ~ mean_pm_OA_T2, data = tmp2)
m5_OA_tot <- lm(group_payoff_OA_tot ~ sd_pm_OA_T2, data = tmp2)
m6_OA_tot <- lm(group_payoff_OA_tot ~ range_pm_OA_T2, data = tmp2)
m7_OA_tot <- lm(group_payoff_OA_tot ~ mean_union_OA_T1, data = tmp2)
m8_OA_tot <- lm(group_payoff_OA_tot ~ sd_union_OA_T1, data = tmp2)
m9_OA_tot <- lm(group_payoff_OA_tot ~ range_union_OA_T1, data = tmp2)
m10_OA_tot <- lm(group_payoff_OA_tot ~ mean_union_OA_T2, data = tmp2)
m11_OA_tot <- lm(group_payoff_OA_tot ~ sd_union_OA_T2, data = tmp2)
m12_OA_tot <- lm(group_payoff_OA_tot ~ range_union_OA_T2, data = tmp2)

# Stargazer table for group_payoff_OA_tot
stargazer(m1_OA_tot, m2_OA_tot, m3_OA_tot, m4_OA_tot, m5_OA_tot, m6_OA_tot,
          type = "text",
          title = "Bivariate Regressions - Dependent Variable: group_payoff_OA_tot")

# Refit models cleanly to remove any attribute inconsistencies
models_OA <- lapply(
  list(
    "mean_union_OA_T1",
    "sd_union_OA_T1",
    "range_union_OA_T1",
    "mean_union_OA_T2",
    "sd_union_OA_T2",
    "range_union_OA_T2"
  ),
  function(var) lm(as.formula(paste("group_payoff_OA_tot ~", var)), data = tmp2)
)

# Call stargazer safely
stargazer(models_OA,
          type = "text",
          title = "Bivariate Regressions - Dependent Variable: group_payoff_OA_tot (Union OA)")



# Amerb

# T1
m1_t1 <- lm(group_payoff_t1 ~ mean_belief_ini, data = tmp)

# T2
m4_t2 <- lm(group_payoff_t2 ~ mean_belief_fin, data = tmp)

# Total 
m1_tot <- lm(group_payoff_tot ~ mean_belief_ini, data = tmp)
m4_tot <- lm(group_payoff_tot ~ mean_belief_fin, data = tmp)


# Open Access 

# Unknown Outsiders T1
m4_OA_t1 <- lm(group_payoff_OA_t1 ~ mean_union_OA_T1, data = tmp2)
m1_OA_t1 <- lm(group_payoff_OA_t1 ~ mean_pm_OA_T1, data = tmp2)

# Known Outsiders T2
m4_OA_t2 <- lm(group_payoff_OA_t2 ~ mean_union_OA_T2, data = tmp2)
m1_OA_t2 <- lm(group_payoff_OA_t2 ~ mean_pm_OA_T2, data = tmp2)


# Total Open Access 
m7_OA_tot <- lm(group_payoff_OA_tot ~ mean_union_OA_T1, data = tmp2)
m10_OA_tot <- lm(group_payoff_OA_tot ~ mean_union_OA_T2, data = tmp2)
m1_OA_tot <- lm(group_payoff_OA_tot ~ mean_pm_OA_T1, data = tmp2)
m4_OA_tot <- lm(group_payoff_OA_tot ~ mean_pm_OA_T2, data = tmp2)




#######################################
##### Scatterplots of the regressions
##########################################
library(ggplot2)
library(dplyr)

# T1 and T2 subset
amerb_t <- bind_rows(
  tmp %>% 
    select(mean_belief = mean_belief_ini, group_payoff = group_payoff_t1) %>% 
    mutate(model = "T1: belief_ini → payoff_t1"),
  
  tmp %>%
    select(mean_belief = mean_belief_fin, group_payoff = group_payoff_t2) %>%
    mutate(model = "T2: belief_fin → payoff_t2")
)

# Tot1 and Tot2 subset
amerb_tot <- bind_rows(
  tmp %>%
    select(mean_belief = mean_belief_ini, group_payoff = group_payoff_tot) %>%
    mutate(model = "Tot1: belief_ini → payoff_tot"),
  
  tmp %>%
    select(mean_belief = mean_belief_fin, group_payoff = group_payoff_tot) %>%
    mutate(model = "Tot2: belief_fin → payoff_tot")
)

# T1 & T2 plot
p<-ggplot(amerb_t, aes(x = mean_belief, y = group_payoff)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(~model) +
  coord_cartesian(ylim = c(1100, 1700), xlim = c(0,1)) +
  labs(
    title = "Amerb Regressions (T1, T2)",
    x = "Mean Belief",
    y = "Group Payoff"
  ) +
  theme_minimal()

ggsave(file = paste0(path_github, "Outputs/group_payoffs_per_beliefs_Turf_T1_T2.png"), 
       plot = p, device = "png", width = 10, height = 8)


# Tot1 & Tot2 plot
p<-ggplot(amerb_tot, aes(x = mean_belief, y = group_payoff)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(~model) +
  coord_cartesian(ylim = c(2400, 3300), xlim = c(0,1)) +
  labs(
    title = "Amerb Regressions (Total)",
    x = "Mean Belief",
    y = "Group Payoff"
  ) +
  theme_minimal()

ggsave(file = paste0(path_github, "Outputs/group_payoffs_per_beliefs_Turf_Tot.png"), 
       plot = p, device = "png", width = 10, height = 8)


# File: plots/open_access_regression_plots.R

######################
### Open Access plots
######################

# T1 OA
oa_t1 <- bind_rows(
  tmp2 %>% 
    select(x = mean_union_OA_T1, y = group_payoff_OA_t1) %>% 
    mutate(model = "T1: union → payoff"),
  
  tmp2 %>% 
    select(x = mean_pm_OA_T1, y = group_payoff_OA_t1) %>% 
    mutate(model = "T1: PM → payoff")
)

# T2 OA
oa_t2 <- bind_rows(
  tmp2 %>% 
    select(x = mean_union_OA_T2, y = group_payoff_OA_t2) %>% 
    mutate(model = "T2: union → payoff"),
  
  tmp2 %>% 
    select(x = mean_pm_OA_T2, y = group_payoff_OA_t2) %>% 
    mutate(model = "T2: PM → payoff")
)

# Totals OA
oa_tot <- bind_rows(
  tmp2 %>%
    select(x = mean_union_OA_T1, y = group_payoff_OA_tot) %>%
    mutate(model = "Tot: union_T1 → payoff"),
  
  tmp2 %>%
    select(x = mean_union_OA_T2, y = group_payoff_OA_tot) %>%
    mutate(model = "Tot: union_T2 → payoff"),
  
  tmp2 %>%
    select(x = mean_pm_OA_T1, y = group_payoff_OA_tot) %>%
    mutate(model = "Tot: PM_T1 → payoff"),
  
  tmp2 %>%
    select(x = mean_pm_OA_T2, y = group_payoff_OA_tot) %>%
    mutate(model = "Tot: PM_T2 → payoff")
)

# Plot T1
p<-ggplot(oa_t1, aes(x = x, y = y)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(~model) +
  coord_cartesian(ylim = c(1100, 1700), xlim = c(0,1)) +
  labs(
    title = "Open Access T1: Unknown Outsiders",
    x = "Mean Belief",
    y = "Group Payoff"
  ) +
  theme_minimal()

ggsave(file = paste0(path_github, "Outputs/group_payoffs_per_beliefs_OA_.png"), 
       plot = p, device = "png", width = 10, height = 8)


# Plot T2
p<-ggplot(oa_t2, aes(x = x, y = y)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(~model) +
  coord_cartesian(ylim = c(1100, 1700), xlim = c(0,1)) +
  labs(
    title = "Open Access T2: Known Outsiders",
    x = "Mean Belief",
    y = "Group Payoff"
  ) +
  theme_minimal()
ggsave(file = paste0(path_github, "Outputs/group_payoffs_per_beliefs_OA_T2.png"), 
       plot = p, device = "png", width = 10, height = 8)

# Plot Totals with fixed y-axis
p<-ggplot(oa_tot, aes(x = x, y = y)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(~model) +
  coord_cartesian(ylim = c(2400, 3300), xlim = c(0,1)) +  # Ensures comparable payoff range
  labs(
    title = "Open Access Total: All Models",
    x = "Mean Belief",
    y = "Group Payoff"
  ) +
  theme_minimal()

ggsave(file = paste0(path_github, "Outputs/group_payoffs_per_beliefs_OA_Tot.png"), 
       plot = p, device = "png", width = 10, height = 8)

######################################
### barplots of earnings per category
######################################



library(dplyr)
library(ggplot2)
library(viridis)
library(tidyr)


tmp2 <- df %>%
  group_by(gid.treat) %>%
  summarise(
    mean_pm_OA_T1  = mean(belief_compliance_pm_OA_T1, na.rm = TRUE),
    mean_pm_OA_T2  = mean(belief_compliance_pm_OA_T2, na.rm = TRUE),
    mean_union_OA_T1  = mean(belief_compliance_union_OA_T1, na.rm = TRUE),
    mean_union_OA_T2  = mean(belief_compliance_union_OA_T2, na.rm = TRUE),
    group_payoff_OA_t1 = mean(T1juegoalgas.8.player.T1_pago_acumulado_libre, na.rm = TRUE),
    group_payoff_OA_t2 = mean(T2juegoalgas.8.player.T2_pago_acumulado_metat, na.rm = TRUE),
    group_payoff_OA_tot = mean(ganancias_OA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    category_pm_OA_T1 = factor(case_when(
      mean_pm_OA_T1 < 0.3 ~ "Low Belief Others (mean<0.3)",
      mean_pm_OA_T1 >= 0.3 & mean_pm_OA_T1 < 0.7 ~ "Mid-Range Belief Others (mean >=0.3, <0.7)",
      TRUE ~ "High Belief Others (mean >=0.7)"
    ), levels = c(
      "Low Belief Others (mean<0.3)",
      "Mid-Range Belief Others (mean >=0.3, <0.7)",
      "High Belief Others (mean >=0.7)"
    )),
    
    category_pm_OA_T2 = factor(case_when(
      mean_pm_OA_T2 < 0.3 ~ "Low Belief Others (mean<0.3)",
      mean_pm_OA_T2 >= 0.3 & mean_pm_OA_T2 < 0.7 ~ "Mid-Range Belief Others (mean >=0.3, <0.7)",
      TRUE ~ "High Belief Others (mean >=0.7)"
    ), levels = c(
      "Low Belief Others (mean<0.3)",
      "Mid-Range Belief Others (mean >=0.3, <0.7)",
      "High Belief Others (mean >=0.7)"
    )),
    
    category_union_OA_T1 = factor(case_when(
      mean_union_OA_T1 < 0.3 ~ "Low Belief Others (mean<0.3)",
      mean_union_OA_T1 >= 0.3 & mean_union_OA_T1 < 0.7 ~ "Mid-Range Belief Others (mean >=0.3, <0.7)",
      TRUE ~ "High Belief Others (mean >=0.7)"
    ), levels = c(
      "Low Belief Others (mean<0.3)",
      "Mid-Range Belief Others (mean >=0.3, <0.7)",
      "High Belief Others (mean >=0.7)"
    )),
    
    category_union_OA_T2 = factor(case_when(
      mean_union_OA_T2 < 0.3 ~ "Low Belief Others (mean<0.3)",
      mean_union_OA_T2 >= 0.3 & mean_union_OA_T2 < 0.7 ~ "Mid-Range Belief Others (mean >=0.3, <0.7)",
      TRUE ~ "High Belief Others (mean >=0.7)"
    ), levels = c(
      "Low Belief Others (mean<0.3)",
      "Mid-Range Belief Others (mean >=0.3, <0.7)",
      "High Belief Others (mean >=0.7)"
    ))
  )


plot_bar_with_ci <- function(df, belief_category, payoff_var, title) {
  df_ci <- df %>%
    group_by(.data[[belief_category]]) %>%
    summarise(
      mean_payoff = mean(.data[[payoff_var]], na.rm = TRUE),
      sd = sd(.data[[payoff_var]], na.rm = TRUE),
      n = sum(!is.na(.data[[payoff_var]])),
      .groups = "drop"
    ) %>%
    mutate(
      se = ifelse(n > 1, sd / sqrt(n), NA),
      ci_low = ifelse(n > 1, mean_payoff - 1.96 * se, NA),
      ci_high = ifelse(n > 1, mean_payoff + 1.96 * se, NA)
    ) %>%
    complete(!!sym(belief_category),
             fill = list(mean_payoff = NA, sd = NA, n = 0, se = NA, ci_low = NA, ci_high = NA))
  
  ggplot(df_ci, aes_string(x = belief_category, y = "mean_payoff", fill = belief_category)) +
    geom_col(na.rm = TRUE) +
    geom_errorbar(
      aes(ymin = ci_low, ymax = ci_high),
      width = 0.2,
      linewidth = 0.7,
      na.rm = TRUE
    ) +
    geom_text(aes(
      y = mean_payoff + 120,
      label = ifelse(n > 0, paste0("n=", n), "n=0")),
      size = 3,
      vjust = 0,
      na.rm = TRUE
    ) +
    labs(
      x = "Belief in Others' Compliance Category",
      y = "Mean Group Earnings",
      title = title
    ) +
    scale_fill_viridis_d(option = "D", end = 0.9, direction = -1) +
    scale_x_discrete(drop = FALSE) +
    coord_cartesian(ylim = c(0, 1600)) +
    theme_minimal() +
    theme(legend.position = "none")
}



# Plot 1: Outsiders, OA T1
p1 <- plot_bar_with_ci(tmp2, "category_pm_OA_T1", "group_payoff_OA_t1",
                       "Outsiders in OA T1: Group Earnings by Belief Category")
ggsave(filename = paste0(path_github, "Outputs/group_payoffs_belief_categories_outsiders_OA_T1.png"),
       plot = p1, device = "png", width = 10, height = 8)

# Plot 2: Outsiders, OA T2
p2 <- plot_bar_with_ci(tmp2, "category_pm_OA_T2", "group_payoff_OA_t2",
                       "Outsiders in OA T2: Group Earnings by Belief Category")
ggsave(filename = paste0(path_github, "Outputs/group_payoffs_belief_categories_outsiders_OA_T2.png"),
       plot = p2, device = "png", width = 10, height = 8)

# Plot 3: Union, OA T1
p3 <- plot_bar_with_ci(tmp2, "category_union_OA_T1", "group_payoff_OA_t1",
                       "Union in OA T1: Group Earnings by Belief Category")
ggsave(filename = paste0(path_github, "Outputs/group_payoffs_belief_categories_union_OA_T1.png"),
       plot = p3, device = "png", width = 10, height = 8)

# Plot 4: Union, OA T2
p4 <- plot_bar_with_ci(tmp2, "category_union_OA_T2", "group_payoff_OA_t2",
                       "Union in OA T2: Group Earnings by Belief Category")
ggsave(filename = paste0(path_github, "Outputs/group_payoffs_belief_categories_union_OA_T2.png"),
       plot = p4, device = "png", width = 10, height = 8)


library(patchwork)  # install if needed: install.packages("patchwork")

# Combine p1 to p4 in a 2x2 layout
combined_plot <- (p1 | p2) / (p3 | p4)

# Save the combined plot
ggsave(filename = paste0(path_github, "Outputs/group_payoffs_belief_categories_combined.png"),
       plot = combined_plot, device = "png", width = 16, height = 12)

######################################################
################### Trust and conflict with earnings
######################################################

trust_summary <- df %>%
  group_by(gid.treat) %>%
  summarise(
    confianza_mean_t1 = mean(survey1.1.player.confianza_pm, na.rm = TRUE),
    confianza_mean_t2 = mean(survey2.1.player.confianza_caleta_conocida_mean, na.rm = TRUE),
    conflicto_mean_t1 = mean(survey1.1.player.conflicto_pm, na.rm = TRUE),
    conflicto_mean_t2 = mean(survey2.1.player.conflicto_caleta_conocida_mean, na.rm = TRUE),
    group_payoff_OA_t1 = mean(T1juegoalgas.8.player.T1_pago_acumulado_libre, na.rm = TRUE),
    group_payoff_OA_t2 = mean(T2juegoalgas.8.player.T2_pago_acumulado_metat, na.rm = TRUE),
    group_payoff_OA_tot = mean(ganancias_OA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    trust_category_t1 = factor(case_when(
      confianza_mean_t1 < 2 ~ "Low Trust (mean < 2)",
      TRUE ~ "High Trust (mean ≥ 2)"
    ), levels = c("Low Trust (mean < 2)", "High Trust (mean ≥ 2)")),
    
    trust_category_t2 = factor(case_when(
      confianza_mean_t2 < 2 ~ "Low Trust (mean < 2)",
      TRUE ~ "High Trust (mean ≥ 2)"
    ), levels = c("Low Trust (mean < 2)", "High Trust (mean ≥ 2)")),
    
    conflict_category_t1 = factor(case_when(
      conflicto_mean_t1 < 2 ~ "Low Conflict (mean < 2)",
      TRUE ~ "High Conflict (mean ≥ 2)"
    ), levels = c("Low Conflict (mean < 2)", "High Conflict (mean ≥ 2)")),
    
    conflict_category_t2 = factor(case_when(
      conflicto_mean_t2 < 2 ~ "Low Conflict (mean < 2)",
      TRUE ~ "High Conflict (mean ≥ 2)"
    ), levels = c("Low Conflict (mean < 2)", "High Conflict (mean ≥ 2)"))
  )

# Trust T1
p_trust_t1 <- plot_bar_with_ci(trust_summary, "trust_category_t1", "group_payoff_OA_t1",
                               "Trust Category T1: Group Earnings OA T1")
ggsave(paste0(path_github, "Outputs/group_payoffs_trust_category_t1.pdf"),
       p_trust_t1, width = 10, height = 8)

# Trust T2
p_trust_t2 <- plot_bar_with_ci(trust_summary, "trust_category_t2", "group_payoff_OA_t2",
                               "Trust Category T2: Group Earnings OA T2")
ggsave(paste0(path_github, "Outputs/group_payoffs_trust_category_t2.pdf"),
       p_trust_t2, width = 10, height = 8)

# Conflict T1
p_conflict_t1 <- plot_bar_with_ci(trust_summary, "conflict_category_t1", "group_payoff_OA_t1",
                                  "Conflict Category T1: Group Earnings OA T1")
ggsave(paste0(path_github, "Outputs/group_payoffs_conflict_category_t1.pdf"),
       p_conflict_t1, width = 10, height = 8)

# Conflict T2
p_conflict_t2 <- plot_bar_with_ci(trust_summary, "conflict_category_t2", "group_payoff_OA_t2",
                                  "Conflict Category T2: Group Earnings OA T2")
ggsave(paste0(path_github, "Outputs/group_payoffs_conflict_category_t2.pdf"),
       p_conflict_t2, width = 10, height = 8)


###########################################
### Earnings v belief categories in groups
##########################################


tmp2 <- df %>%
  group_by(gid.treat) %>%
  summarise(
    mean_pm_OA_T1  = mean(belief_compliance_pm_OA_T1, na.rm = TRUE),
    mean_pm_OA_T2  = mean(belief_compliance_pm_OA_T2, na.rm = TRUE),
    mean_union_OA_T1  = mean(belief_compliance_union_OA_T1, na.rm = TRUE),
    mean_union_OA_T2  = mean(belief_compliance_union_OA_T2, na.rm = TRUE),
    group_payoff_OA_t1 = mean(T1juegoalgas.8.player.T1_pago_acumulado_libre, na.rm = TRUE),
    group_payoff_OA_t2 = mean(T2juegoalgas.8.player.T2_pago_acumulado_metat, na.rm = TRUE),
    group_payoff_OA_tot = mean(ganancias_OA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    category_pm_OA_T1 = factor(case_when(
      mean_pm_OA_T1 < 0.3 ~ "Low Belief Others (mean<0.3)",
      mean_pm_OA_T1 >= 0.3 & mean_pm_OA_T1 < 0.7 ~ "Mid-Range Belief Others (mean >=0.3, <0.7)",
      TRUE ~ "High Belief Others (mean >=0.7)"
    ), levels = c(
      "Low Belief Others (mean<0.3)",
      "Mid-Range Belief Others (mean >=0.3, <0.7)",
      "High Belief Others (mean >=0.7)"
    )),
    
    category_pm_OA_T2 = factor(case_when(
      mean_pm_OA_T2 < 0.3 ~ "Low Belief Others (mean<0.3)",
      mean_pm_OA_T2 >= 0.3 & mean_pm_OA_T2 < 0.7 ~ "Mid-Range Belief Others (mean >=0.3, <0.7)",
      TRUE ~ "High Belief Others (mean >=0.7)"
    ), levels = c(
      "Low Belief Others (mean<0.3)",
      "Mid-Range Belief Others (mean >=0.3, <0.7)",
      "High Belief Others (mean >=0.7)"
    )),
    
    category_union_OA_T1 = factor(case_when(
      mean_union_OA_T1 < 0.3 ~ "Low Belief Others (mean<0.3)",
      mean_union_OA_T1 >= 0.3 & mean_union_OA_T1 < 0.7 ~ "Mid-Range Belief Others (mean >=0.3, <0.7)",
      TRUE ~ "High Belief Others (mean >=0.7)"
    ), levels = c(
      "Low Belief Others (mean<0.3)",
      "Mid-Range Belief Others (mean >=0.3, <0.7)",
      "High Belief Others (mean >=0.7)"
    )),
    
    category_union_OA_T2 = factor(case_when(
      mean_union_OA_T2 < 0.3 ~ "Low Belief Others (mean<0.3)",
      mean_union_OA_T2 >= 0.3 & mean_union_OA_T2 < 0.7 ~ "Mid-Range Belief Others (mean >=0.3, <0.7)",
      TRUE ~ "High Belief Others (mean >=0.7)"
    ), levels = c(
      "Low Belief Others (mean<0.3)",
      "Mid-Range Belief Others (mean >=0.3, <0.7)",
      "High Belief Others (mean >=0.7)"
    ))
  )


plot_bar_with_ci <- function(df, belief_category, payoff_var, title) {
  df_ci <- df %>%
    group_by(.data[[belief_category]]) %>%
    summarise(
      mean_payoff = mean(.data[[payoff_var]], na.rm = TRUE),
      sd = sd(.data[[payoff_var]], na.rm = TRUE),
      n = sum(!is.na(.data[[payoff_var]])),
      .groups = "drop"
    ) %>%
    mutate(
      se = ifelse(n > 1, sd / sqrt(n), NA),
      ci_low = ifelse(n > 1, mean_payoff - 1.96 * se, NA),
      ci_high = ifelse(n > 1, mean_payoff + 1.96 * se, NA)
    ) %>%
    complete(!!sym(belief_category),
             fill = list(mean_payoff = NA, sd = NA, n = 0, se = NA, ci_low = NA, ci_high = NA))
  
  ggplot(df_ci, aes_string(x = belief_category, y = "mean_payoff", fill = belief_category)) +
    geom_col(na.rm = TRUE) +
    geom_errorbar(
      aes(ymin = ci_low, ymax = ci_high),
      width = 0.2,
      linewidth = 0.7,
      na.rm = TRUE
    ) +
    geom_text(aes(
      y = mean_payoff + 120,
      label = ifelse(n > 0, paste0("n=", n), "n=0")),
      size = 3,
      vjust = 0,
      na.rm = TRUE
    ) +
    labs(
      x = "Belief in Others' Compliance Category",
      y = "Mean Group Earnings",
      title = title
    ) +
    scale_fill_viridis_d(option = "D", end = 1, direction = -1) +
    scale_x_discrete(drop = FALSE) +
    coord_cartesian(ylim = c(0, 1600)) +
    theme_minimal() +
    theme(legend.position = "none")
}



# Plot 1: Outsiders, OA T1
p1 <- plot_bar_with_ci(tmp2, "category_pm_OA_T1", "group_payoff_OA_t1",
                       "Outsiders in OA T1: Group Earnings by Belief Category")
ggsave(filename = paste0(path_github, "Outputs/group_payoffs_belief_categories_outsiders_OA_T1.png"),
       plot = p1, device = "png", width = 10, height = 8)

# Plot 2: Outsiders, OA T2
p2 <- plot_bar_with_ci(tmp2, "category_pm_OA_T2", "group_payoff_OA_t2",
                       "Outsiders in OA T2: Group Earnings by Belief Category")
ggsave(filename = paste0(path_github, "Outputs/group_payoffs_belief_categories_outsiders_OA_T2.png"),
       plot = p2, device = "png", width = 10, height = 8)

# Plot 3: Union, OA T1
p3 <- plot_bar_with_ci(tmp2, "category_union_OA_T1", "group_payoff_OA_t1",
                       "Union in OA T1: Group Earnings by Belief Category")
ggsave(filename = paste0(path_github, "Outputs/group_payoffs_belief_categories_union_OA_T1.png"),
       plot = p3, device = "png", width = 10, height = 8)

# Plot 4: Union, OA T2
p4 <- plot_bar_with_ci(tmp2, "category_union_OA_T2", "group_payoff_OA_t2",
                       "Union in OA T2: Group Earnings by Belief Category")
ggsave(filename = paste0(path_github, "Outputs/group_payoffs_belief_categories_union_OA_T2.png"),
       plot = p4, device = "png", width = 10, height = 8)


library(patchwork)  # install if needed: install.packages("patchwork")

# Combine p1 to p4 in a 2x2 layout
combined_plot <- (p1 | p2) / (p3 | p4)

# Save the combined plot
ggsave(filename = paste0(path_github, "Outputs/group_payoffs_belief_categories_combined.png"),
       plot = combined_plot, device = "png", width = 16, height = 12)



