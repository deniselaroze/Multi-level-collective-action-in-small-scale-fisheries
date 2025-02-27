#############################
#### Data Management
#############################


library(foreign)
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(readr)
library(haven)
library(R.matlab)


rm(list=ls())
#path_github <-"C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
#path_datos<-"C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"


path_github <-"C:/Users/Denise Laroze/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos<-"C:/Users/Denise Laroze/Dropbox/CICS/Experiments/Islitas/Data/Sessions"


setwd(path_github)

datos_csv<-"datos_piloto_islitas.csv"

# List all CSV files in the specified folder
file_list <- list.files(path = path_datos, pattern = "^all_apps_wide.*\\.csv$", full.names = TRUE)

# Read each CSV file and combine them into one data frame
df<- do.call(rbind, lapply(file_list, read.csv))


##################################################
############ recode ##############################
##################################################

#Each person person participants in two group /matching groups at the same time
# This is the group ID in the Open Access area
df$gid.treat<-paste0(df$participant.zonaT2, ".", df$session.code)

#This is the group ID in the TURF
df$gid.amerb<-paste0(df$participant.caleta, ".",df$participant.zonaT2, ".",df$participant.id_caleta)
#df$gid.amerb <- gsub(" ", ".", df$ugid)

# 



#########################
### Export Raw Data 
#########################
#save(df, file = paste0(path_datos, "/Datos_islitas.Rdata"))

#write.csv(df, paste0(path_datos, "/Datos_islitas_base.csv"), row.names = FALSE)
#write.table(df, paste0(path_datos, "/Datos_islitas_tab_base.txt"), sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)


#####################################
### Re-coding and reshaping variables
#####################################


############################
### Data Management
############################

# Add variables for aggregated row sums based on categories
df <- df %>%
  mutate(
    # Sum of `amerb` for T1
    otros_amerb_t1_mean = rowSums(
      across(starts_with("T1juegoalgas") & ends_with("extraccion_otros_amerb")),
      na.rm = TRUE
    ) / 8 / 3,
    # Sum of `amerb` for T2
    otros_amerb_t2_mean = rowSums(
      across(starts_with("T2juegoalgas") & ends_with("extraccion_otros_amerb")),
      na.rm = TRUE
    ) / 8 / 3,
    # Sum of `libre` for T1
    otros_libre_t1_mean = rowSums(
      across(starts_with("T1juegoalgas") & ends_with("extraccion_otros_libre")),
      na.rm = TRUE
    ) / 8 / 3,
    # Sum of `metat` for T2
    otros_metat_t2_mean = rowSums(
      across(starts_with("T2juegoalgas") & ends_with("extraccion_otros_metat")),
      na.rm = TRUE
    ) / 8 / 3
  )


# Difference in beliefs and aggregates for caleta conocida 1 and 2

df<- df %>%
  mutate(
    T1_diff_ingroup_amerb = beliefsT1final.1.player.T1_belief_caleta_en_amerb_fin - 
      beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini,
    T1_diff_ingroup_OA = beliefsT1final.1.player.T1_belief_caleta_en_libre_fin - 
      beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini,
    T1_diff_others_OA = beliefsT1final.1.player.T1_belief_pm_en_libre_fin - 
      beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini,
    T2_diff_ingroup_metat = beliefsT2final.1.player.T2_belief_caleta_fin - 
      beliefsT2inicial.1.player.T2_belief_caleta_ini,
    T2_diff_others_metat = if_else(
      is.na(beliefsT2final.1.player.T2_belief_caleta_conocida2_fin) | 
        is.na(beliefsT2inicial.1.player.T2_belief_caleta_conocida2_ini),
      as.double(beliefsT2final.1.player.T2_belief_caleta_conocida1_fin - 
                  beliefsT2inicial.1.player.T2_belief_caleta_conocida1_ini),
      as.double((beliefsT2final.1.player.T2_belief_caleta_conocida1_fin - 
                   beliefsT2inicial.1.player.T2_belief_caleta_conocida1_ini + 
                   beliefsT2final.1.player.T2_belief_caleta_conocida2_fin - 
                   beliefsT2inicial.1.player.T2_belief_caleta_conocida2_ini) / 2)
    ),
    beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini = if_else(
      !is.na(beliefsT2inicial.1.player.T2_belief_caleta_conocida2_ini),
      as.double((beliefsT2inicial.1.player.T2_belief_caleta_conocida1_ini + 
                   beliefsT2inicial.1.player.T2_belief_caleta_conocida2_ini) / 2),
      as.double(beliefsT2inicial.1.player.T2_belief_caleta_conocida1_ini)
    ),
    beliefsT2final.1.player.T2_belief_caleta_conocida_mean_fin = if_else(
      !is.na(beliefsT2final.1.player.T2_belief_caleta_conocida2_fin),
      as.double((beliefsT2final.1.player.T2_belief_caleta_conocida1_fin + 
                   beliefsT2final.1.player.T2_belief_caleta_conocida2_fin) / 2),
      as.double(beliefsT2final.1.player.T2_belief_caleta_conocida1_fin)
    )
  )

# Create the mean variables
df$survey2.1.player.confianza_caleta_conocida_mean <- ifelse(
  !is.na(df$survey2.1.player.confianza_caleta_conocida2),
  (df$survey2.1.player.confianza_caleta_conocida1 + df$survey2.1.player.confianza_caleta_conocida2) / 2,
  df$survey2.1.player.confianza_caleta_conocida1
)

df$survey2.1.player.conflicto_caleta_conocida_mean <- ifelse(
  !is.na(df$survey2.1.player.conflicto_caleta_conocida2),
  (df$survey2.1.player.conflicto_caleta_conocida1 + df$survey2.1.player.conflicto_caleta_conocida2) / 2,
  df$survey2.1.player.conflicto_caleta_conocida1
)


# Generate group_size column with NA
df$group_size <- NA_integer_

# Assign values based on conditions
df$group_size[!is.na(df$participant.zonaT2) & grepl("Z123", df$participant.zonaT2)] <- 3
df$group_size[!is.na(df$participant.zonaT2) & grepl("Z12A|Z12B|Z12C|Z23", df$participant.zonaT2)] <- 2


#table(df$participant.zonaT2, df$group_size)

# Transformation of relevant variables from extraction to Compliance 

#Extraction for the T1 OA
variable_subset <- df[, grep("^T1juegoalgas\\.\\d+\\.player\\.T1_extraccion_libre$", names(df))]

# Calculate the row-wise mean for the selected columns
df$average_extraction <- rowMeans(variable_subset, na.rm = TRUE)
df$average_compliance<- 1-(df$average_extraction/50) 

df$belief_compliance_pm<-1-(df$beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini/50)
df$belief_compliance_union<-1-(df$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini/50)

df$real_compliance_others_OA_t1_mean<- 1-(df$otros_libre_t1_mean/50)
df$real_compliance_others_OA_t2_mean<- 1-(df$otros_metat_t2_mean/50)
df$real_compliance_others_amerb_t1_mean<- 1-(df$otros_amerb_t1_mean/50)
df$real_compliance_others_amerb_t2_mean<- 1-(df$otros_amerb_t2_mean/50)


# Save file df at the end



#########################3
#### Reshape datasets long
##########################

# T1
rounds <- 1:8  # Sequence from 1 to 10
treats <- c("T1")  # Time periods T1 and T2
vars <- c("amerb", "libre")  # Extraction types

# Generate the list of variables using expand.grid to generate all combinations
combinations <- expand.grid(treats, rounds, vars)

# Create the final object with formatted strings
variable_names1 <- paste0(
  "", combinations$Var1, "juegoalgas.", combinations$Var2, 
  ".player.", combinations$Var1, "_extraccion_", combinations$Var3
)


# T2"
rounds <- 1:8  # Sequence from 1 to 10
treats <- c("T2")  # Time periods T1 and T2
vars <- c("amerb", "metat")  # Extraction types

# Generate the list of variables using expand.grid to generate all combinations
combinations <- expand.grid(treats, rounds, vars)

# Create the final object with formatted strings
variable_names2 <- paste0(
  "", combinations$Var1, "juegoalgas.", combinations$Var2, 
  ".player.", combinations$Var1, "_extraccion_", combinations$Var3
)

belief_columns <- c(
  "participant.code", "group_size",  # Add participant or player identifier for merging and groupsize for regressions
  "beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini",
  "beliefsT1final.1.player.T1_belief_caleta_en_amerb_fin",
  "beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini",
  "beliefsT1final.1.player.T1_belief_caleta_en_libre_fin",
  "beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini",
  "beliefsT1final.1.player.T1_belief_pm_en_libre_fin",
  "beliefsT2inicial.1.player.T2_belief_caleta_ini",
  "beliefsT2final.1.player.T2_belief_caleta_fin",
  "beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini",
  "beliefsT2final.1.player.T2_belief_caleta_conocida_mean_fin"
)

experience <- c("survey1.1.player.confianza_caleta", "survey1.1.player.confianza_pm", 
                "survey1.1.player.conflicto_caleta", "survey1.1.player.conflicto_pm", 
                "survey1.1.player.experiencia_caleta", "survey1.1.player.experiencia_pm", 
                "survey1.1.player.T1_motiv_legit_amerb", "survey1.1.player.T1_motiv_instr_amerb", 
                "survey1.1.player.T1_motiv_socnorm_amerb", "survey1.1.player.T1_motiv_legit_pm", 
                "survey1.1.player.T1_motiv_instr_pm", "survey1.1.player.T1_motiv_socnorm_ingroup_pm", 
                "survey1.1.player.T1_motiv_socnorm_outgroup_pm", "survey3.1.player.sexo", 
                "survey3.1.player.nacimiento", "survey3.1.player.estudios", 
                "survey3.1.player.horas_trabajo", "survey3.1.player.liderazgo", 
                "survey3.1.player.experiencia", "survey3.1.player.motivinstrum_amerb", 
                "survey3.1.player.motivinstrum_libre", "survey3.1.player.motivlegit_amerb", 
                "survey3.1.player.motivlegit_libre", "survey3.1.player.awareness_amerb", 
                "survey3.1.player.awareness_libre", "survey3.1.player.pregunta_abierta")


variable_names<-c("participant.code", "gid.amerb", "gid.treat", variable_names1, variable_names2, belief_columns, experience )


##### Treatment variables subset
dfs<-(df[, variable_names])
dfs2<-(df[, c("participant.code",variable_names1, variable_names2)])




##########################
### Reshaping data.frame
##########################
# Reshape for 'amerb' area

dfs_amerb <- dfs %>%
  pivot_longer(
    cols = starts_with("T"),
    names_to = c("treatment", "round", "area"),
    names_pattern = "(T\\d)juegoalgas\\.(\\d+)\\.player\\..+_extraccion_(.+)",
    values_to = "extraction"
  ) %>%
  filter(area == "amerb") %>%  # Filter only 'amerb'
  mutate(round = as.integer(round)) %>%
  select(-area) %>%  # Drop the 'area' column
  rename(extraction_amerb = extraction)

# Reshape for 'libre' or 'metat' (OA)
dfs_oa <- dfs2 %>%
  pivot_longer(
    cols = starts_with("T"),
    names_to = c("treatment", "round", "area"),
    names_pattern = "(T\\d)juegoalgas\\.(\\d+)\\.player\\..+_extraccion_(.+)",
    values_to = "extraction"
  ) %>%
  filter(area %in% c("libre", "metat")) %>%  # Filter only 'libre' and 'metat'
  mutate(round = as.integer(round)) %>%
  select(-area) %>%  # Drop the 'area' column
  rename(extraction_OA = extraction)



subset_df <- df %>%
  select(participant.code, matches("extraccion_otros"))
# Reshape for 'amerb' (otros)
dfs_otros_amerb <- subset_df %>%
  pivot_longer(
    cols = -participant.code,  # Exclude participant.code from pivoting
    names_to = c("treatment", "round", "area"),
    names_pattern = "(T\\d)juegoalgas\\.(\\d+)\\.player\\..+_extraccion_otros_(.+)",
    values_to = "extraction_others_amerb"
  ) %>%
  filter(area == "amerb") %>%  # Filter only 'amerb'
  mutate(round = as.integer(round)) %>%  # Ensure round is numeric
  select(participant.code, treatment, round, extraction_others_amerb)  # Retain participant.code

# Reshape for 'libre' or 'metat' (otros)
dfs_otros_oa <- subset_df %>%
  pivot_longer(
    cols = -participant.code,  # Exclude participant.code from pivoting
    names_to = c("treatment", "round", "area"),
    names_pattern = "(T\\d)juegoalgas\\.(\\d+)\\.player\\..+_extraccion_otros_(.+)",
    values_to = "extraction_others_OA"
  ) %>%
  filter(area %in% c("libre", "metat")) %>%  # Filter only 'libre' and 'metat'
  mutate(round = as.integer(round)) %>%  # Ensure round is numeric
  select(participant.code, treatment, round, extraction_others_OA)  # Retain participant.code

subset_df <- df %>%
  select(participant.code, matches("combi"))

# Reshape for number of people from each group
dfs_t <- subset_df %>%
  pivot_longer(
    cols = starts_with("T"), # Match all columns starting with "T"
    names_to = c("treatment", "round"),
    names_pattern = "(T\\d)juegoalgas\\.(\\d+)\\.player\\.combi",
    values_to = "combi"
  ) %>%
  mutate(
    round = as.integer(round) # Convert round to integer
  )

# Step 2: Apply transformation to the "combi" column
dfs_n_group <- dfs_t %>%
  mutate(
    combi_cleaned = str_remove(combi, "^G"), # Remove "G"
    combi_padded = str_pad(combi_cleaned, width = 3, side = "right", pad = "0"), # Pad with 0s
    n_ingroup = str_sub(combi_padded, 1, 1), # Extract first digit
    n_outgroup1 = str_sub(combi_padded, 2, 2), # Extract second digit
    n_outgroup2 = str_sub(combi_padded, 3, 3) # Extract third digit
  ) %>%
  select(
    participant.code, treatment, round, combi, n_ingroup, n_outgroup1, n_outgroup2
  ) # Keep only relevant columns



# Merge all data frames step-by-step
dfs_long <- dfs_amerb %>%
  full_join(dfs_oa, by = c("participant.code", "treatment", "round")) %>%
  full_join(dfs_otros_amerb, by = c("participant.code", "treatment", "round")) %>%
  full_join(dfs_otros_oa, by = c("participant.code", "treatment", "round")) %>%
  full_join(dfs_n_group, by = c("participant.code", "treatment", "round"))


#### Recode variables 

# number of colors (identities) on the screen 2 or 3
dfs_long$n_identities<-ifelse(dfs_long$treatment=="T1", 2, dfs_long$group_size)


# Calculate mean extractions by others
dfs_long <- dfs_long %>%
  mutate(
    extraction_others_amerb_mean = extraction_others_amerb / 3,
    extraction_others_OA_mean = extraction_others_OA / 3
  )

# Update beliefs iteratively
dfs_long <- dfs_long %>%
  arrange(participant.code, treatment, round) %>%  # Sort by key columns
  group_by(participant.code, treatment) %>%  # Group by participant and treatment
  mutate(beliefs_amerb_updated = {
    # Initialize a vector for updated beliefs
    beliefs_updated <- numeric(n())
    
    # Round 1 belief update
    beliefs_updated[1] <- beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini[1] +
      (extraction_others_amerb_mean[1] - beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini[1]) / 2
    
    # Iterative belief update for rounds 2 to n
    for (i in 2:n()) {
      beliefs_updated[i] <- beliefs_updated[i - 1] +
        (extraction_others_amerb_mean[i] - beliefs_updated[i - 1]) / 2
    }
    
    beliefs_updated
  }) %>%
  ungroup()

# Update beliefs ingroup in OA iteratively
dfs_long <- dfs_long %>%
  arrange(participant.code, treatment, round) %>%  # Sort by key columns
  group_by(participant.code, treatment) %>%  # Group by participant and treatment
  mutate(beliefs_ingroup_OA_updated = {
    # Initialize a vector for updated beliefs
    beliefs_updated <- numeric(n())
    
    # Round 1 belief update based on treatment
    if (treatment[1] == "T1") {
      beliefs_updated[1] <- beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini[1] +
        (extraction_others_OA_mean[1] - beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini[1]) / 2
    } else if (treatment[1] == "T2") {
      beliefs_updated[1] <- beliefsT2inicial.1.player.T2_belief_caleta_ini[1] +
        (extraction_others_OA_mean[1] - beliefsT2inicial.1.player.T2_belief_caleta_ini[1]) / 2
    }
    
    # Iterative belief update for rounds 2 to n
    for (i in 2:n()) {
      beliefs_updated[i] <- beliefs_updated[i - 1] +
        (extraction_others_OA_mean[i] - beliefs_updated[i - 1]) / 2
    }
    
    beliefs_updated
  }) %>%
  ungroup()

# Update beliefs outgroup in OA iteratively
dfs_long <- dfs_long %>%
  arrange(participant.code, treatment, round) %>%  # Sort by key columns
  group_by(participant.code, treatment) %>%  # Group by participant and treatment
  mutate(beliefs_outgroup_OA_updated = {
    # Initialize a vector for updated beliefs
    beliefs_updated <- numeric(n())
    
    # Round 1 belief update based on treatment
    if (treatment[1] == "T1") {
      beliefs_updated[1] <- beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini[1] +
        (extraction_others_OA_mean[1] - beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini[1]) / 2
    } else if (treatment[1] == "T2") {
      beliefs_updated[1] <- beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini[1] +
        (extraction_others_OA_mean[1] - beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini[1]) / 2
    }
    
    # Iterative belief update for rounds 2 to n
    for (i in 2:n()) {
      beliefs_updated[i] <- beliefs_updated[i - 1] +
        (extraction_others_OA_mean[i] - beliefs_updated[i - 1]) / 2
    }
    
    beliefs_updated
  }) %>%
  ungroup()


dfs_long <- dfs_long %>%
  arrange(participant.code, treatment, round) %>%  # Ensure correct order
  group_by(participant.code, treatment) %>%  # Group by participant and treatment
  mutate(lag_beliefs_amerb_updated = lag(beliefs_amerb_updated),
         lag_beliefs_ingroup_OA_updated = lag(beliefs_ingroup_OA_updated),
         lag_beliefs_outgroup_OA_updated = lag(beliefs_outgroup_OA_updated),
         lag_extraction_others_amerb_mean = lag(extraction_others_amerb_mean),
         lag_extraction_others_OA_mean = lag(extraction_others_OA_mean)
  ) %>%  # Create lagged column
  ungroup()

#### Updating beliefs --- imputation 
dfs_long <- dfs_long %>%
  mutate(
    lag_beliefs_amerb_updated = if_else(
      is.na(lag_beliefs_amerb_updated) & round == 1,
      as.numeric(beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini),  # Convert to numeric
      lag_beliefs_amerb_updated
    )
  )


#### Generating one long beliefs variables that has the beliefs for extraction in OA
dfs_long <- dfs_long %>%
  mutate(
    # Generate beliefs_OA_caleta
    beliefs_OA_caleta = if_else(
      treatment == "T1",
      as.numeric(beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini),
      as.numeric(beliefsT2inicial.1.player.T2_belief_caleta_ini)
    ),
    # Generate beliefs_OA_others
    beliefs_OA_others = if_else(
      treatment == "T1",
      as.numeric(beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini),
      as.numeric(beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini)
    ),
    # Categorical version of beliefs_OA_caleta
    beliefs_OA_caleta_cat = case_when(
      beliefs_OA_caleta == 0 ~ "UC",
      beliefs_OA_caleta > 0 & beliefs_OA_caleta < 50 ~ "NC or CC",
      beliefs_OA_caleta == 50 ~ "FR",
      TRUE ~ NA_character_  # Handle unexpected cases
    ),
    # Categorical version of beliefs_OA_others
    beliefs_OA_others_cat = case_when(
      beliefs_OA_others == 0 ~ "UC",
      beliefs_OA_others > 0 & beliefs_OA_others < 50 ~ "NC or CC",
      beliefs_OA_others == 50 ~ "FR",
      TRUE ~ NA_character_  # Handle unexpected cases
    )
  )

# # Imputing beliefs based on the difference between the begining and the end.
# dfs_long <- dfs_long %>%
#   mutate(
#     # For T1 - Caleta
#     imputed_belief_T1_caleta = if_else(
#       round == 1,
#       beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini,
#       beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini + 
#         ((beliefsT1final.1.player.T1_belief_caleta_en_libre_fin - 
#             beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini) / 8) * round
#     ),
#     
#     # For T1 - PM
#     imputed_belief_T1_pm = if_else(
#       round == 1,
#       beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini,
#       beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini + 
#         ((beliefsT1final.1.player.T1_belief_pm_en_libre_fin - 
#             beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini) / 8) * round
#     ),
#     
#     # For T2 - Caleta Conocida Mean
#     imputed_belief_T2_caleta_conocida_mean = if_else(
#       round == 1,
#       beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini,
#       beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini + 
#         ((beliefsT2final.1.player.T2_belief_caleta_conocida_mean_fin - 
#             beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini) / 8) * round
#     ),
#     
#     # For T2 - Caleta
#     imputed_belief_T2_caleta = if_else(
#       round == 1,
#       beliefsT2inicial.1.player.T2_belief_caleta_ini,
#       beliefsT2inicial.1.player.T2_belief_caleta_ini + 
#         ((beliefsT2final.1.player.T2_belief_caleta_fin - 
#             beliefsT2inicial.1.player.T2_belief_caleta_ini) / 8) * round
#     )
#   )
# 
# # Create new variables based on the treatment
# dfs_long <- dfs_long %>%
#   mutate(
#     # Impute beliefs for OA Caleta
#     imputed_beliefs_OA_caleta = if_else(
#       treatment == "T1",
#       imputed_belief_T1_caleta,
#       imputed_belief_T2_caleta
#     ),
#     
#     # Impute beliefs for OA Others
#     imputed_beliefs_OA_others = if_else(
#       treatment == "T1",
#       imputed_belief_T1_pm,
#       imputed_belief_T2_caleta_conocida_mean
#     )
#   )


group_counts <- table(dfs_long$beliefs_OA_others_cat)
group_proportions <- prop.table(group_counts)
group_proportions

# Dummy variables for ingrouo/outgroup experience, trust and conflict
dfs_long <- dfs_long %>%
  mutate(
    # Create dummy for confianza_caleta
    dummy_confianza_caleta = if_else(survey1.1.player.confianza_caleta > 1, 1, 0),
    
    # Create dummy for confianza_pm
    dummy_confianza_pm = if_else(survey1.1.player.confianza_pm  > 1, 1, 0),
    
    # Create dummy for conflicto_caleta
    dummy_conflicto_caleta = if_else(survey1.1.player.conflicto_caleta == 1, 0, 1),
    
    # Create dummy for conflicto_pm
    dummy_conflicto_pm = if_else(survey1.1.player.conflicto_pm == 1, 0, 1),
    
    # Create dummy for experiencia_caleta
    dummy_experiencia_caleta = if_else(survey1.1.player.experiencia_caleta >1, 1, 0),
    
    # Create dummy for experiencia_pm
    dummy_experiencia_pm = if_else(survey1.1.player.experiencia_pm >1, 1, 0)
  )

#### Group number variables recoded
dfs_long$n_outgroup=4-as.numeric(dfs_long$n_ingroup)

dfs_long$minority<- ifelse(dfs_long$n_ingroup<2, 1, 0)




#View(dfs_long[, c("beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini", "extraction_others_amerb_mean","beliefs_amerb_updated")])

#### Transforming main extraction and belief variables into compliance:
# Define transformation function
scale_to_01 <- function(x) {
  return(1 - (x / 50))
}

# List of variables to transform
vars_to_transform <- c("beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini",     
                       "beliefsT1final.1.player.T1_belief_caleta_en_amerb_fin",        
                       "beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini",     
                       "beliefsT1final.1.player.T1_belief_caleta_en_libre_fin",        
                       "beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini",         
                       "beliefsT1final.1.player.T1_belief_pm_en_libre_fin",            
                       "beliefsT2inicial.1.player.T2_belief_caleta_ini",              
                       "beliefsT2final.1.player.T2_belief_caleta_fin",                 
                       "beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini",
                       "beliefsT2final.1.player.T2_belief_caleta_conocida_mean_fin",
                       "extraction_amerb",                                            
                       "extraction_OA",                                      
                       "extraction_others_amerb_mean",                                 
                       "extraction_others_OA_mean",                                   
                       "beliefs_amerb_updated",                                       
                       "beliefs_ingroup_OA_updated",                                  
                       "beliefs_outgroup_OA_updated",                                  
                       "lag_beliefs_amerb_updated",                                   
                       "lag_beliefs_ingroup_OA_updated",                              
                       "lag_beliefs_outgroup_OA_updated",                             
                       "lag_extraction_others_amerb_mean",                             
                       "lag_extraction_others_OA_mean")    

# Apply transformation with new column names
dfs_long <- dfs_long %>%
  mutate(across(all_of(vars_to_transform), scale_to_01, .names = "compliance_{.col}"))


save(df, file = paste0(path_datos, "/Datos_islitas_recode.Rdata"))
save(dfs_long, file = paste0(path_datos, "/Datos_islitas_long.Rdata"))





