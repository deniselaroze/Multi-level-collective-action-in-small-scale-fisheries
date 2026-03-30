######################################################################################
### Empirical tests of H2 and Diff-in-Diff:
### Difference in the mean level of compliance across all rounds, per stage
######################################################################################

# Load necessary libraries
library(dplyr)
library(tidyr)
library(lme4)
library(modelsummary)
library(boot)
library(stats)

# --- SETUP PATHS AND LOAD DATA ---
path_github <- "C:/Users/DCCS2/Documents/GitHub/Multi-level-collective-action-in-small-scale-fisheries/Exptal Sessions/R/"
path_datos  <- "C:/Users/DCCS2/Dropbox/CICS/Experiments/Islitas/Data/Sessions"

# Load the long format data
load(paste0(path_datos, "/Datos_islitas_long.Rdata"))

# --- PREPARE DATA FOR DiD & LMMs ---
# dfs_long has compliance for TURF (compliance_extraction_amerb) and Shared Area (compliance_extraction_OA)
# We pivot these so 'area' is a categorical column and 'compliance' is a single dependent variable.
df_long_ext <- dfs_long %>%
  select(participant.code, round, treatment, gid.treat, compliance_extraction_amerb, compliance_extraction_OA) %>%
  pivot_longer(
    cols = c(compliance_extraction_amerb, compliance_extraction_OA),
    names_to = "area",
    values_to = "compliance"
  ) %>%
  mutate(
    area = if_else(area == "compliance_extraction_amerb", "TURF", "Shared_Area"),
    treat.area = paste(area, treatment, sep = "_")
  )

# ====================================================================================
# 1. DIFF-IN-DIFF AND SHARED AREA ESTIMATIONS (Means and Tests)
# ====================================================================================

# Calculate mean compliance per participant and area/stage, averaging across all rounds.
# (Dropping 'round' from group_by ensures we get the stage-level mean per individual)
did_df <- df_long_ext %>%
  group_by(participant.code, treat.area) %>%
  summarise(mean_comp = mean(compliance, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = treat.area,
    values_from = mean_comp
  ) %>%
  mutate(
    diff_turf   = TURF_T2 - TURF_T1,
    diff_shared = Shared_Area_T2 - Shared_Area_T1,
    DiD         = diff_turf - diff_shared
  )

# --- Summary Statistics for the Differences ---
did_summary <- did_df %>%
  summarise(
    n            = n(),
    mean_diff_SA = mean(diff_shared, na.rm = TRUE),
    sd_diff_SA   = sd(diff_shared, na.rm = TRUE),
    se_diff_SA   = sd_diff_SA / sqrt(n),
    mean_DiD     = mean(DiD, na.rm = TRUE),
    sd_DiD       = sd(DiD, na.rm = TRUE),
    se_DiD       = sd_DiD / sqrt(n)
  )

cat("\n=== Summary of Mean Differences ===\n")
print(did_summary)

# --- H2: Difference in Shared Area (T2 - T1) ---
cat("\n=== Testing H2: Difference in Shared Area (T2 vs T1) ===\n")
t_test_shared <- t.test(did_df$diff_shared, mu = 0)
print(t_test_shared)

wilcox_shared <- wilcox.test(did_df$diff_shared, mu = 0)
print(wilcox_shared)


# --- Diff-in-Diff Test (TURF Diff vs Shared Area Diff) ---
cat("\n=== Testing Diff-in-Diff (TURF diff vs Shared Area diff) ===\n")
t_test_did <- t.test(did_df$DiD, mu = 0)
print(t_test_did)

wilcox_did <- wilcox.test(did_df$DiD, mu = 0)
print(wilcox_did)


# ====================================================================================
# 2. MIXED EFFECTS MODELS (Bootstrapped for Supplementary Material)
# ====================================================================================

# A robust lmerControl (helps reduce spurious convergence warnings)
ctrl <- lmerControl(
  optimizer = "bobyqa",
  optCtrl   = list(maxfun = 2e5),
  check.conv.grad = .makeCC("warning", tol = 1e-3, relTol = NULL)
)

# ---------- Cluster bootstrap for lmer ----------
bootstrap_lmer <- function(model_formula, data, cluster_var, B = 2000, seed = 62354234) {
  # Fit once on the full data (ML) to get the coefficient skeleton & order
  base_fit <- lmer(model_formula, data = data, control = ctrl, REML = FALSE)
  coef_names <- names(fixef(base_fit))
  
  # One bootstrap replication: sample clusters with replacement
  one_rep <- function(d, i_unused) {
    clusters <- unique(stats::na.omit(d[[cluster_var]]))
    if (length(clusters) < 2L) return(setNames(rep(NA_real_, length(coef_names)), coef_names))
    
    samp <- sample(clusters, length(clusters), replace = TRUE)
    d_b  <- dplyr::bind_rows(lapply(samp, function(cl) d[d[[cluster_var]] == cl, , drop = FALSE]))
    
    fit <- try(lmer(model_formula, data = d_b, control = ctrl, REML = FALSE), silent = TRUE)
    out <- setNames(rep(NA_real_, length(coef_names)), coef_names)
    if (inherits(fit, "try-error")) return(out)
    
    cf <- try(fixef(fit), silent = TRUE)
    if (!inherits(cf, "try-error")) out[names(cf)] <- as.numeric(cf)
    out
  }
  
  set.seed(seed)
  bt <- boot::boot(
    data = data,
    statistic = function(d, i) one_rep(d, i),
    R = B,
    parallel = "no"  # set to "multicore" if you want parallelism
  )
  
  draws <- bt$t
  colnames(draws) <- coef_names
  
  # Covariance & SEs from the bootstrap draws
  V  <- stats::cov(draws, use = "pairwise.complete.obs")
  se <- sqrt(diag(V))
  
  list(
    base_fit  = base_fit,
    V         = V,
    se        = se,
    coef_names = coef_names
  )
}

# Define the cluster variable for bootstrapping
clvar <- "gid.treat"

# --- Run the Bootstrapped Models ---
cat("\nRunning bootstrapped models (this may take a moment)...\n")

# Model 1: Testing H2 specifically in the Shared Area
df_shared <- df_long_ext %>% filter(area == "Shared_Area")
res_shared <- bootstrap_lmer(compliance ~ treatment + (1 | participant.code),
                             data = df_shared, cluster_var = clvar, B = 2000)

# Model 2: The full DiD interaction model
res_did <- bootstrap_lmer(compliance ~ area * treatment + (1 | participant.code),
                          data = df_long_ext, cluster_var = clvar, B = 2000)

# Extract properly aligned vcov matrices
vcov_shared <- res_shared$V[ names(fixef(res_shared$base_fit)), names(fixef(res_shared$base_fit)) ]
vcov_did    <- res_did$V[ names(fixef(res_did$base_fit)), names(fixef(res_did$base_fit)) ]


# --- Export Supplementary Table ---
# Clean labels for the supplementary material
my_labels <- c(
  "(Intercept)"                 = "Constant",
  "treatmentT2"                 = "Stage 2 (Known Out-group)",
  "areaShared_Area"             = "Shared Area",
  "areaShared_Area:treatmentT2" = "Shared Area \u00d7 Stage 2 (DiD)"
)

# Output using modelsummary
modelsummary(
  list("H2: Shared Area Only"     = res_shared$base_fit,
       "Diff-in-Diff (Full Data)" = res_did$base_fit),
  vcov      = list(vcov_shared, vcov_did),
  coef_map  = my_labels,
  stars     = c("*" = .05, "**" = .01, "***" = .001), 
  gof_omit  = "IC|Log.Lik|AIC|BIC|RMSE",
  title     = "Supplementary Table: Mixed Effects Models for Compliance (Bootstrapped SEs)",
  notes     = "Standard errors are cluster-bootstrapped (2000 replications) at the group level.",
  output    = paste0(path_github, "Outputs/LMM_H2_DiD_Supplementary.docx")
)

cat("\nDone! Supplementary table saved.\n")