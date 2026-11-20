# ==============================================================================
# Replication package -- run_all.R
# Runs every analysis in the manuscript and writes all tables to Word (.docx)
# and figures (.jpg) to path_out. All tables are also collected in
# All_tables.docx. Each file name starts with the item's number in the
# manuscript (e.g. Table_3_..., Table_SM.E.2_..., Figure_SM.E.1_...); outputs
# that are not in the manuscript start with Extra_.
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
source("01_descriptives.R")   # Table 1; Tables SM.D.1-SM.D.3
source("02_regressions.R")    # Table 2; Tables SM.D.4, SM.E.1
source("03_sem.R")            # Tables 3, 4; Tables SM.D.5, SM.E.2, SM.E.3; Figure SM.E.1
source("04_robustness.R")     # Tables SM.E.4-SM.E.7; Figures SM.E.2-SM.E.3 (observed compliance over 3 rounds; tobit)
source("05_identity_vs_learning.R")  # Supplementary Material F: Tables SM.F.1-SM.F.2 (optional extra tables J1-J5)

export_all_tables("All_tables.docx")
writeLines(capture.output(sessionInfo()), file.path(path_out, "sessionInfo.txt"))
message("\nDone in ", round(difftime(Sys.time(), t0, units = "mins"), 1),
        " minutes. Outputs in: ", path_out)
