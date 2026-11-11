# Replication package — Multi-level collective action in small-scale fisheries (Islitas)

This package reproduces every table and figure in the manuscript and Supplementary Material from the two session data files.

## How to run

1. Place the five `.R` files in one folder, e.g. `.../Exptal Sessions/R/replication/`.
2. Edit the paths at the top of `00_setup.R` (`path_github`, `path_datos`). Outputs go to `path_github/Outputs/Replication/`.
3. Set the working directory to the package folder (RStudio: *Session > Set Working Directory > To Source File Location*) and run `run_all.R`.

Each script can also be run on its own, because each one sources `00_setup.R` if it hasn't been loaded yet. To check the pipeline in about 2 minutes, set `quick_run <- TRUE` in `00_setup.R`, which uses few bootstrap replications. The full run takes roughly 10–20 minutes, depending on the machine.

**Required R packages:** dplyr, tidyr, lavaan, lme4, sandwich, lmtest, ggplot2. *Optional:* modelsummary with pandoc, for the `.docx` tables. Every table is always written as `.csv` too.

## Input data

| File | Object | Used for |
|---|---|---|
| `Datos_islitas_recode.Rdata` | `df` (wide, one row per participant) | Tables 1, 3, 4, S1, S2, S4, S5, S6, S8; Figure S2 |
| `Datos_islitas_long.Rdata` | `dfs_long` (one row per participant × round) | Tables 1, 2, S7 |

Both files must contain `gid.treat` (the matching group) and `participant.code`.

## Map of outputs

| Manuscript item | Script | Output file (`Outputs/Replication/`) |
|---|---|---|
| Table 1 — Compliance, prior beliefs and trends | `01_descriptives.R` | `Table_1_Compliance_beliefs_trends` (+ `_trends_inference_comparison.csv`) |
| Table 2 — Average compliance OLS | `02_regressions.R` | `Table_2_Average_compliance_OLS` (+ `Table_2_inference_comparison.csv`) |
| Table 3 — γ and λ | `03_sem.R` | `Table_3_SEM_gamma_lambda` |
| Table 4 — Belief equations (δ) | `03_sem.R` | `Table_4_SEM_belief_equations` |
| Table S1 — Sample characteristics | `01_descriptives.R` | `Table_S1_Sample_characteristics` |
| Table S2 — Trust and conflict | `01_descriptives.R` | `Table_S2_Trust_conflict` |
| Table S3 — (removed from manuscript) | — | — |
| Table S4 — Changes in prior beliefs | `01_descriptives.R` | `Table_S4_Belief_changes` |
| Table S5 — Changes in compliance OLS | `02_regressions.R` | `Table_S5_Compliance_change_OLS` |
| Table S6 — Full SEM compliance equation (β) | `03_sem.R` | `Table_S6_SEM_full_compliance_equation` |
| Table S7 — Mixed-effects models | `02_regressions.R` | `Table_S7_Mixed_effects` |
| *New* Table S8 — Stage 2 − Stage 1 differences | `03_sem.R` | `Table_S8_SEM_stage_differences` |
| Figure S2 — λ sensitivity | `03_sem.R` | `Figure_S2_Lambda_sensitivity.jpg`, `Figure_S2_estimates.csv` |
| (not in manuscript) γ/λ plot; all SEM parameters | `03_sem.R` | `Figure_SEM_gamma_lambda.jpg`, `SEM_Round8_all_parameters.csv` |

## Inference: all at the matching-group level (`gid.treat`)

- **OLS (Tables 1, 2, S5).** Standard errors are cluster-robust (CR1), clustered by matching group. Significance stars use wild cluster bootstrap p-values (restricted, Webb weights, 9,999 replications), which hold up with few clusters. To base the stars on cluster-robust t-tests with G−1 degrees of freedom instead, set `stars_from <- "cluster"`. The `*_inference_comparison.csv` files report participant-clustered standard errors next to the new ones.
- **SEM (Tables 3, 4, S6, S8; Figure S2).** Cluster bootstrap: matching groups are drawn with replacement and every participant in a drawn group is kept. Stage 1 and Stage 2 are re-estimated on the same resamples, which makes the cross-stage differences in Table S8 testable. Standard error = bootstrap SD; p = est/SE with a normal reference distribution; CIs are percentile intervals.
- **Mixed models (Table S7).** Cluster bootstrap by matching group (2,000 replications). A participant drawn twice gets a new ID, so each copy has its own random intercept.

## Changes from the earlier scripts and items to align in the manuscript

1. **Table 1 trends:** the note says SEs are clustered by participant, but the earlier code already clustered by `gid.treat`. Both the code and the note now refer to the matching group.
2. **Table 2:** earlier clustered by participant; now clustered by matching group, so the note needs updating.
3. **Table 3 (earlier `SEM_lambdas.R`):** the labels for λ₁ and λ₂ in `var_labels` didn't match `pred_order`, so those two rows were dropped from the exported table. Fixed. λ₁ = in-group and λ₂ = out-group, as in the manuscript.
4. **Table S2:** the values are four-point scales rescaled to [0,1], but the manuscript note says "standardized (mean 0, SD 1)". Standardized versions are used only inside the SEM, so the note should be corrected (see comments 90, 91 and 157).
5. **Table S5:** the earlier code used stars at 10%/5%/0.1% and no clustering, even though the output file was named `_clusterGroup`. It now uses the standard thresholds and matching-group clustering. The F statistic is a cluster-robust Wald F.
6. **Table S7:** the empty "Compliance TURF" row is dropped and the resampling unit is stated (comment 607).
7. **Figure S2:** for windows `[a,8]`, compliance is averaged over rounds a–8 and observed compliance is taken from round a−1, as the original estimation function did. Faded markers are CIs that include 0, and dotted lines are the mean λ across specifications (comment 609). Please confirm both match the original figure.
8. **Table 1, TURF Stage 2 in-group prior:** this belief was not elicited, so the cell shows "–" (comment 446).
