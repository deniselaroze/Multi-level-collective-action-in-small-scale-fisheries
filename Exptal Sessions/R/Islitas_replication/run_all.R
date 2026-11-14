# ==============================================================================
# Replication package -- run_all.R
# Runs every analysis in the manuscript and writes all tables to Word (.docx)
# and figures (.jpg) to path_out. All tables are also collected in
# All_tables.docx.
#
# Set the working directory to this folder first, e.g. in RStudio:
#   Session > Set Working Directory > To Source File Location
# Edit paths in 00_paths.R and settings (wild bootstrap, replications, quick_run)
# in 00_setup.R.
# ==============================================================================

rm(list = ls())
t0 <- Sys.time()
source("00_paths.R")
source("00_data_management.R")  # raw oTree exports -> analysis data files
source("00_setup.R")            # packages, settings, helpers; loads the analysis data
source("01_descriptives.R")   # Table 1, Tables S1, S2, S3
source("02_regressions.R")    # Table 2, Tables S4, S6
source("03_sem.R")            # Tables 3, 4, S5, S7, Figure S2
source("04_robustness.R")     # Robustness: Tables R1, R2, Figures R1, R2 (tobit; observed compliance over 3 rounds)
source("05_identity_vs_learning.R")  # Supplementary Material F: Tables S8-S9 (optional J1-J5)

export_all_tables("All_tables.docx")
writeLines(capture.output(sessionInfo()), file.path(path_out, "sessionInfo.txt"))
message("\nDone in ", round(difftime(Sys.time(), t0, units = "mins"), 1),
        " minutes. Outputs in: ", path_out)
