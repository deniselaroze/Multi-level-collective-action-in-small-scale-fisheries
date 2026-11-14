# ==============================================================================
# Replication package -- 00_paths.R
# The only place where folder paths are set. Sourced by 00_data_management.R
# and 00_setup.R.
#   path_datos  : folder with the raw oTree exports (all_apps_wide*.csv); the
#                 analysis files built from them are saved here too
#   path_github : repository folder; outputs go to path_github/Outputs/Replication
# ==============================================================================

path_github <- "C:/Users/Denise/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos  <- "C:/Users/Denise/Dropbox/CICS/Experiments/Islitas/Data/Sessions"
# path_github <- "C:/Users/Denise Laroze/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
# path_datos  <- "C:/Users/Denise Laroze/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

path_out <- file.path(path_github, "Outputs", "Replication")

# Analysis files written by 00_data_management.R and read by 00_setup.R
file_wide <- file.path(path_datos, "Datos_islitas_replication_wide.Rdata")  # object: df
file_long <- file.path(path_datos, "Datos_islitas_replication_long.Rdata")  # object: dfs_long
