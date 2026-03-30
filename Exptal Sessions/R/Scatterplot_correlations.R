# ====================================================================
# SCATTERPLOT MATRIX ESTIMATION CODE
# ====================================================================

# Install these packages if you don't have them already:
# install.packages("psych")
# install.packages("GGally")

library(dplyr)
library(psych)
library(GGally)

# --------------------------------------------------------------------
# 1. DATA PREPARATION
# --------------------------------------------------------------------
# Based on your code, we will select the variables of interest from 'df'.
#
# IMPORTANT NOTE: Your code defines 'vars_df' (likely one row per subject)
# and 'vars_dfs_long' (likely panel/multiple rounds per subject). 
# A scatterplot matrix requires variables to be in the same dataframe 
# with the same number of rows. 
# Here, we will generate the figure for your cross-sectional variables (vars_df).

plot_data <- df %>%
  select(all_of(vars_df[c(1:10)]))

# Optional: With 11 variables, the plot might get very crowded. 
# If it's too hard to read, you can subset specific variables like this:
# plot_data_subset <- df %>% select(Tst_caleta_scaled, Cft_caleta_scaled, Tst_sa_T1_scaled)


# --------------------------------------------------------------------
# OPTION 1: 'psych' package (Closest visual match to your image)
# --------------------------------------------------------------------
# This function puts histograms on the diagonal, scatterplots on the 
# lower triangle, and correlation coefficients on the upper triangle.

pairs.panels(plot_data,
             method = "pearson",   # Use pearson or spearman correlation
             hist.col = "darkgray",# Matches the grey histograms in your image
             density = FALSE,      # Turns off density curves over the histograms
             ellipses = FALSE,     # Turns off correlation ellipses in the scatters
             lm = TRUE,            # Adds a linear regression line to scatterplots
             pch = 20,             # Smaller, solid points for the scatterplots
             cex.cor = 1.5,        # Size of the correlation text
             main = "Scatterplot Matrix of Survey Variables")


# --------------------------------------------------------------------
# OPTION 2: 'GGally' package (Modern, Tidyverse/ggplot2 style)
# --------------------------------------------------------------------
# This generates a beautiful ggplot2-based matrix. It is highly 
# customizable if you want to change colors, themes, or add groupings later.

modern_plot <- ggpairs(
  plot_data,
  title = "Scatterplot Matrix of Survey Variables",
  
  # Customizing the lower triangle (scatterplots)
  lower = list(continuous = wrap("points", alpha = 0.4, size = 0.8, color = "blue")),
  
  # Customizing the diagonal (histograms)
  diag = list(continuous = wrap("barDiag", fill = "darkgray", color = "black", bins = 10))
) + 
  theme_bw() + # Apply a clean black and white theme
  theme(
    strip.text = element_text(size = 7), # Make variable names smaller so they fit
    axis.text = element_text(size = 6)   # Make axis numbers smaller
  )

# Display the ggplot matrix
print(modern_plot)






modern_plot1 <- ggpairs(
  plot_data[,c(1:6)],
  title = "Scatterplot Matrix of Survey Variables",
  
  # Customizing the lower triangle (scatterplots)
  lower = list(continuous = wrap("points", alpha = 0.4, size = 0.8, color = "blue")),
  
  # Customizing the diagonal (histograms)
  diag = list(continuous = wrap("barDiag", fill = "darkgray", color = "black", bins = 10))
) + 
  theme_bw() + # Apply a clean black and white theme
  theme(
    strip.text = element_text(size = 7), # Make variable names smaller so they fit
    axis.text = element_text(size = 6)   # Make axis numbers smaller
  )

# Display the ggplot matrix
print(modern_plot1)


modern_plot2 <- ggpairs(
  plot_data[,c(7:10)],
  title = "Scatterplot Matrix of Survey Variables",
  
  # Customizing the lower triangle (scatterplots)
  lower = list(continuous = wrap("points", alpha = 0.4, size = 0.8, color = "blue")),
  
  # Customizing the diagonal (histograms)
  diag = list(continuous = wrap("barDiag", fill = "darkgray", color = "black", bins = 10))
) + 
  theme_bw() + # Apply a clean black and white theme
  theme(
    strip.text = element_text(size = 7), # Make variable names smaller so they fit
    axis.text = element_text(size = 6)   # Make axis numbers smaller
  )

# Display the ggplot matrix
print(modern_plot2)
