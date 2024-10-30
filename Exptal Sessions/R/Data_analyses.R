###########################
### Data Analysis
############################


rm(list=ls())
path_github <-"C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos<-"C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

setwd(path_github)


load(paste0(path_datos, "/Datos_islitas.Rdata"))

#################################################
################# Subsets #######################
#################################################

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
# Print or use the variable_names
variable_names<-c(variable_names1, variable_names2, "gid.amerb", "gid.treat")


##### Treatment variables subset
dfs<-(df[, variable_names])


#### Long data frame all observations
dfs_long <- dfs %>%
  pivot_longer(
    cols = starts_with("T"),   # All columns starting with "T" (T1 and T2 variables)
    names_to = c("treatment", "round", "area"),  # Split the names into three parts
    names_pattern = "(T\\d)juegoalgas\\.(\\d+)\\.player\\..+_extraccion_(.+)",  # Regex to extract treatment, round, and variable
    values_to = "extraction"   # Name of the column for values
  ) %>%
  mutate(round = as.integer(round))  # Ensure round is numeric


# Mean by treatment and area, for each round for diff-in-diff 

rm <- dfs_long %>%
  group_by(treatment, area, round) %>%  # Group by treatment and variable
  summarise(mean_extraction = mean(extraction, na.rm = TRUE))  # Calculate mean and handle missing values


#Belief Columns 

names(df)
df$beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini
df$beliefsT1final.1.player.T1_belief_caleta_en_libre_fin
df$beliefsT1final.1.player.T1_belief_pm_en_libre_fin
df$beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini


belief_columns <- grep("belief", colnames(df), value = TRUE, ignore.case = TRUE)
# Now filter to keep only the ones that end in "_ini" or "_fin"
filtered_belief_columns <- grep("_ini$|_fin$|id", belief_columns, value = TRUE, ignore.case = TRUE)

# View the result
print(filtered_belief_columns)
View(df[,c(filtered_belief_columns,  "gid.treat", "gid.amerb")])

#################################################
############### Data Analysis ###################
#################################################

#Descriptive statistics
20000+mean(df$participant.payoff, na.rm=T)
summary(df$participant.payoff, na.rm=T)


table(df$T2juegoalgas.1.player.T2_grupo_mixto)
table(df$participant.grupo_amerb)

#############
### Graphs
##############


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
### diff in Amerb - Other area per treatment
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




