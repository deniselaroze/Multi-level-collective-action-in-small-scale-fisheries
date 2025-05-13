
library(dplyr)



df_payoff <- df[, grep("payoff|pago", names(df), ignore.case = TRUE)]


df_payoff <- df[, grep("pago_acumulado", names(df), ignore.case = TRUE)]

df$T1juegoalgas.8.player.T1_pago_acumulado_amerb
df$T1juegoalgas.8.player.T1_pago_acumulado_libre

df$T2juegoalgas.8.player.T2_pago_acumulado_amerb
df$T2juegoalgas.8.player.T2_pago_acumulado_metat

df$ganancias_amerb<-df$T1juegoalgas.8.player.T1_pago_acumulado_amerb + df$T2juegoalgas.8.player.T2_pago_acumulado_metat
df$ganancias_OA<-df$T1juegoalgas.8.player.T1_pago_acumulado_libre + df$T2juegoalgas.8.player.T2_pago_acumulado_metat
#belief_compliance_union

#belief_compliance_pm

#df$gid.amerb

#df$gid.treat

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

lm<-lm(group_payoff_t1~ mean_belief_ini+ sd_belief_ini+ range_belief_ini, tmp)
summary(lm)

lm<-lm(group_payoff_t2~ mean_belief_fin+ sd_belief_fin+ range_belief_fin, tmp)
summary(lm)

lm<-lm(group_payoff_tot~ mean_belief_ini+ sd_belief_ini+ range_belief_ini +mean_belief_fin+ sd_belief_fin+ range_belief_fin, tmp)
summary(lm)



#### Open Access

df$belief_compliance_pm_OA_T1 <- 1 - (df$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini / 50)
df$belief_compliance_pm_OA_T2 <- 1 - (df$beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini / 50)
df$belief_compliance_union_OA_T1 <- 1 - (df$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini / 50)
df$belief_compliance_union_OA_T2 <- 1 - (df$beliefsT2inicial.1.player.T2_belief_caleta_ini / 50)


#Open Access
library(dplyr)

tmp2 <- df %>%
  group_by(gid.treat) %>%
  summarise(
    # PM OA T1
    mean_pm_OA_T1  = mean(belief_compliance_pm_OA_T1, na.rm = TRUE),
    sd_pm_OA_T1    = sd(  belief_compliance_pm_OA_T1, na.rm = TRUE),
    range_pm_OA_T1 = abs(
      max(belief_compliance_pm_OA_T1, na.rm = TRUE) -
        min(belief_compliance_pm_OA_T1, na.rm = TRUE)
    ),
    
    # PM OA T2
    mean_pm_OA_T2  = mean(belief_compliance_pm_OA_T2, na.rm = TRUE),
    sd_pm_OA_T2    = sd(  belief_compliance_pm_OA_T2, na.rm = TRUE),
    range_pm_OA_T2 = abs(
      max(belief_compliance_pm_OA_T2, na.rm = TRUE) -
        min(belief_compliance_pm_OA_T2, na.rm = TRUE)
    ),
    
    # UNION OA T1
    mean_union_OA_T1  = mean(belief_compliance_union_OA_T1, na.rm = TRUE),
    sd_union_OA_T1    = sd(  belief_compliance_union_OA_T1, na.rm = TRUE),
    range_union_OA_T1 = abs(
      max(belief_compliance_union_OA_T1, na.rm = TRUE) -
        min(belief_compliance_union_OA_T1, na.rm = TRUE)
    ),
    
    # UNION OA T2
    mean_union_OA_T2  = mean(belief_compliance_union_OA_T2, na.rm = TRUE),
    sd_union_OA_T2    = sd(  belief_compliance_union_OA_T2, na.rm = TRUE),
    range_union_OA_T2 = abs(
      max(belief_compliance_union_OA_T2, na.rm = TRUE) -
        min(belief_compliance_union_OA_T2, na.rm = TRUE)
    ),
    group_payoff_OA_t1 = mean(T1juegoalgas.8.player.T1_pago_acumulado_libre , na.rm = TRUE),
    group_payoff_OA_t2 = mean(df$T2juegoalgas.8.player.T2_pago_acumulado_metat , na.rm = TRUE),
    group_payoff_OA_tot = mean(ganancias_OA , na.rm = TRUE),
    
    .groups = "drop"
  )

#### Regression analysis 

lm<-lm(group_payoff_OA_t1~ mean_pm_OA_T1 + mean_union_OA_T1, tmp2)
summary(lm)

lm<-lm(group_payoff_OA_t2~ mean_pm_OA_T2 + mean_union_OA_T2 , tmp2)
summary(lm)

lm<-lm(group_payoff_OA_tot~ mean_pm_OA_T1 + mean_union_OA_T1+ mean_pm_OA_T2 + mean_union_OA_T2, tmp2)
summary(lm)
