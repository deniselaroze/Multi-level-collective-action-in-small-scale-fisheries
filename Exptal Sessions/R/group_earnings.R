
library(dplyr)
library(stargazer)
rm(list=ls())
#path_github <-"C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
#path_datos<-"C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

path_github <-"C:/Users/Denise Laroze/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos<-"C:/Users/Denise Laroze/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

setwd(path_github)


#load(paste0(path_datos, "/Datos_islitas.Rdata"))
load(paste0(path_datos, "/Datos_islitas_recode.Rdata"))
load(paste0(path_datos, "/Datos_islitas_long.Rdata"))



##### identifying data
df_payoff <- df[, grep("payoff|pago", names(df), ignore.case = TRUE)]


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



#### Open Access

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



