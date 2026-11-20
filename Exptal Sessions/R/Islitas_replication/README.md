# Replication package — Multi-level collective action in small-scale fisheries (Islitas)

This package reproduces every table and figure in the manuscript and Supplementary Material, starting from the raw oTree session exports. Tables are written to Word (`.docx`); figures to `.jpg`. Every file name starts with the item's number in the manuscript (e.g. `Table_3_…`, `Table_SM.E.2_…`, `Figure_SM.E.1_…`), and outputs that are not in the manuscript start with `Extra_` (see *Map of outputs*).

## How to run

1. Place the nine `.R` files in one folder, e.g. `.../Exptal Sessions/R/replication/`.
2. Edit the paths in `00_paths.R` (`path_github`, `path_datos`). Outputs go to `path_github/Outputs/Replication/`.
3. Check the settings in `00_setup.R` (see below). The defaults reproduce the manuscript.
4. Set the working directory to the package folder (RStudio: *Session > Set Working Directory > To Source File Location*) and run `run_all.R`.

`run_all.R` runs, in order: `00_data_management.R` (raw data → analysis files), `00_setup.R`, `01_descriptives.R`, `02_regressions.R`, `03_sem.R`, `04_robustness.R`, `05_identity_vs_learning.R`. Once the analysis files exist, each analysis script can also be run on its own, because each one sources `00_setup.R` if it hasn't been loaded yet.

To check the pipeline in about two minutes, set `quick_run <- TRUE` in `00_setup.R`. It uses few bootstrap replications and writes its outputs to `Outputs/Replication/quick_run/`, so a test never overwrites the full results. The full run takes roughly 30–50 minutes, depending on the machine; the SEM bootstraps behind Figures SM.E.1 and SM.E.2 take most of it. To skip a robustness check, set `run_R1_tobit` or `run_R2_obs3` to `FALSE` at the top of `04_robustness.R`, or remove `04_robustness.R` from `run_all.R`.

Files from runs made before the outputs were renamed (`Table_S*`, `Table_R*`, `Figure_S2_*`, `Figure_R*`, `SEM_Round8_all_parameters.docx`, `Figure_SEM_gamma_lambda.jpg`) are not overwritten by the new names. Delete them from `Outputs/Replication/` so the folder only holds current results.

**Requirements:** R ≥ 4.2 on Windows, or any recent R in a UTF-8 locale on Linux/macOS (table labels contain λ₁, γ, R², †; `00_setup.R` warns if the locale is not UTF-8). R packages: dplyr, tidyr, lavaan, lme4, ggplot2, flextable, officer, survival (installed with R). Pandoc is not needed.

## Settings in `00_setup.R`

| Setting | Default | Meaning |
|---|---|---|
| `boot_cluster` | `cluster_var` (`"gid.treat"`) | Level at which wild bootstrap weights are drawn: matching groups (`cluster_var`) or participants (`participant_var`) |
| `wild_weights` | `"webb"` | Weight distribution: `"webb"` (six-point; recommended with few clusters) or `"rademacher"` (±1) |
| `B_boot` | 2000 | Bootstrap replications for every table |
| `B_sens` | 1000 | Bootstrap replications per specification in Figures SM.E.1–SM.E.3 (Round 8 uses `B_boot`) |
| `boot_se` | `"iqr"` | Standard error in parentheses: IQR/1.349 (`"iqr"`) or standard deviation (`"sd"`) of the replications |
| `seed_main` | 478 | Random seed |
| `quick_run` | `FALSE` | `TRUE` = few replications, to test the pipeline |

## Inference: one wild cluster bootstrap for everything

All standard errors, confidence intervals and p-values (OLS, SEM, mixed models, tobit, and the means in Table SM.F.1) come from the same wild cluster bootstrap, clustered by matching group (Cameron, Gelbach and Miller, 2008):

1. Estimate the model and keep the fitted values and residuals. The data are never resampled.
2. In each replication, draw one weight per matching group (17 weights) from the Webb six-point distribution (±√1.5, ±1, ±√0.5, each with probability 1/6; mean 0, variance 1; Webb, 2023). All observations of the participants in a matching group (all rounds, both areas, both stages and all equations) share that weight, so their correlation is preserved.
3. Rebuild each outcome as *fitted value + residual × weight* and re-estimate the model.
4. Repeat `B_boot` times. 95% CIs are percentile intervals (2.5th and 97.5th percentiles of the replications). p-values are percentile p-values: twice the share of replications on the other side of zero. So p < 0.05 exactly when the 95% CI excludes zero, and p < 0.1 exactly when the 90% CI excludes zero; tables and figures always agree.
5. The standard error in parentheses is the interquartile range of the replications divided by 1.349 (`boot_se <- "iqr"`), or their standard deviation (`boot_se <- "sd"`). The robust version is the default because the λ's are ratios c_k / (c1 + c2 + c3): in the few replications where γ is close to zero they take extreme values, which inflates the standard deviation without moving the percentiles.

**Same seed, same weights.** The weights depend only on the seed: analyses run with the same seed use identical weights in every replication. Stage 1 and Stage 2 always share them (so the Stage 2 − Stage 1 differences in Table SM.E.2 have a bootstrap distribution), and Tables SM.E.4 and SM.E.6 use the same weights as Table 3. Matching groups are put in C-locale order before the weights are assigned, so the results are identical on every computer, whatever its language settings.

How the outcome is rebuilt, by model:

| Model | Tables | What is rebuilt in each replication |
|---|---|---|
| OLS | 1 (trends), 2, SM.D.4, SM.F.2; optional J1–J3, J5 | Dependent variable = fitted value + residual × weight. Because OLS is linear in y, all replications are computed at once; this gives exactly the same estimates as re-running `lm()` on each rebuilt outcome. |
| SEM | 3, 4, SM.D.5, SM.E.2, SM.E.4; Figures SM.E.1, SM.E.2 (estimates in SM.E.3, SM.E.5) | The three equations' outcomes: both beliefs first, then compliance, whose fitted value uses the rebuilt beliefs. The SEM is then re-estimated with lavaan. |
| Mixed models | SM.E.1 | Compliance = fixed-effects prediction + marginal residual (participant random intercept + error) × weight; the model is re-fitted with `lme4::refit()` (Modugno and Giannerini, 2015). |
| Tobit | SM.E.6; Figure SM.E.3 (estimates in SM.E.7) | Score version of the wild bootstrap (Kline and Santos, 2012); see below. |
| Means | SM.F.1 | Each variable = its mean + deviation from the mean × weight; the reset statistics (changes and ratios of means) are recomputed. |

Details:
- **SEM (Tables 3, 4, SM.D.5, SM.E.2; Figure SM.E.1).** The wild bootstrap rebuilds the SEM outcomes from equation-by-equation OLS fits. With this recursive model and no residual covariances between equations, the ML estimates of each equation equal OLS; `run_sem_spec()` checks this every time and warns if they ever differ. Replications where lavaan fails to converge are dropped (the count is printed and reported as "Replications used" in Table SM.E.3).
- **Tobit (Table SM.E.6, Figure SM.E.3).** A censored outcome cannot be rebuilt as fitted value + residual × weight, so the tobit uses the score wild bootstrap: in each replication, the score contributions of each matching group are multiplied by its weight and the estimates are updated by one Newton step, θ* = θ̂ + V Σ_g w_g s_g. For a linear model this is exactly the wild bootstrap of the other tables (checked numerically). The same weights as Table 3 are used.
- **Table SM.F.2.** The out-group and in-group equations (with matching-group fixed effects) use the same weights, so their difference is tested.
- **Table SM.D.4 F statistic.** Wald test that all slopes are zero, using the bootstrap covariance matrix of the coefficients (Wald statistic / number of slopes; p from χ²).

Suggested text for the manuscript: *"Standard errors are obtained from a wild cluster bootstrap with 2,000 replications, clustered by matching group (Cameron, Gelbach and Miller, 2008). In each replication, each of the 17 matching groups receives a random weight from the six-point distribution of Webb (2023), the dependent variables are rebuilt as their fitted values plus the residuals multiplied by the weight of the participant's matching group (the same weight for all of a participant's observations, all equations and both stages), and the model is re-estimated. Confidence intervals and p-values are obtained from the percentiles of the bootstrap distribution, which does not assume normality and is appropriate for the λ parameters, which are ratios of estimated coefficients. Standard errors are the interquartile range of the bootstrap estimates divided by 1.349."*

## Table captions and notes

Captions and notes follow the manuscript: bold, left-aligned captions with the manuscript's numbers ("Table 3 | …", "Table SM.E.2 | …"), notes below each table, and a "Notes:" prefix for Supplementary items. Cross-references inside the notes use the same numbers ("As Table SM.E.3, …", "Round 8 is the main specification (Table SM.E.6)"). Every note ends with the same inference sentence, generated from the settings above (cluster level, number of clusters, weights, replications, type of standard error) and adapted to the model (OLS, SEM, mixed model, tobit, means), so it stays correct if a setting changes.

Figures are saved as `.jpg` files without a caption; their captions and notes are in the manuscript (Figures SM.E.1–SM.E.3), and the estimates behind each figure are in Tables SM.E.3, SM.E.5 and SM.E.7.

## Data

**Input:** the raw oTree exports `all_apps_wide*.csv` (one per session) in `path_datos`.

`00_data_management.R` builds only the variables the analysis uses:

- `gid.treat`: matching group in the Shared Area (`participant.zonaT2` × `session.code`), the bootstrap cluster.
- Stage 2 out-group measures averaged over the one or two identified unions: `beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini`, `survey2.1.player.confianza_caleta_conocida_mean`, `survey2.1.player.conflicto_caleta_conocida_mean`.
- The long data: `participant.code`, `gid.treat`, `treatment` (T1, T2), `round` (1–8 within stage), `extraction_amerb` (TURF), `extraction_OA` (Shared Area).

All other variables are used as they come in the raw export (survey answers, prior beliefs, round-by-round extraction and others' extraction).

**Output (saved in `path_datos`):**

| File | Object | Used for |
|---|---|---|
| `Datos_islitas_replication_wide.Rdata` | `df` (one row per participant) | Tables 1, 3, 4, SM.D.1–SM.D.5, SM.E.2–SM.E.7, SM.F.1–SM.F.2; Figures SM.E.1–SM.E.3 |
| `Datos_islitas_replication_long.Rdata` | `dfs_long` (one row per participant × stage × round) | Tables 1, 2, SM.E.1 |

New file names are used so the earlier `Datos_islitas_recode.Rdata` and `Datos_islitas_long.Rdata`, which other scripts may rely on, are not overwritten. If those earlier files are in `path_datos`, `00_data_management.R` compares every analysis variable with them and reports whether they are identical.

## Map of outputs

Every file name starts with the item's number in the manuscript, so sorting the output folder by name lists the items in manuscript order. All tables are also collected, in manuscript order, in `All_tables.docx` (written by `run_all.R`): main tables, then Supplementary Material tables by section (D, E, F), then the extras.

| Manuscript item | Script | Output file (`Outputs/Replication/`) |
|---|---|---|
| Table 1 — Descriptive compliance and prior beliefs | `01_descriptives.R` | `Table_1_Compliance_beliefs_trends.docx` |
| Table 2 — Average compliance (OLS) | `02_regressions.R` | `Table_2_Average_compliance_OLS.docx` |
| Table 3 — γ and λ | `03_sem.R` | `Table_3_SEM_gamma_lambda.docx` |
| Table 4 — Belief equations (δ) | `03_sem.R` | `Table_4_SEM_belief_equations.docx` |
| Table SM.D.1 — Sample characteristics | `01_descriptives.R` | `Table_SM.D.1_Sample_characteristics.docx` |
| Table SM.D.2 — Trust and previous conflict | `01_descriptives.R` | `Table_SM.D.2_Trust_conflict.docx` |
| Table SM.D.3 — Changes in prior beliefs | `01_descriptives.R` | `Table_SM.D.3_Belief_changes.docx` |
| Table SM.D.4 — Changes in compliance (OLS) | `02_regressions.R` | `Table_SM.D.4_Compliance_change_OLS.docx` |
| Table SM.D.5 — Full SEM compliance equation (β) | `03_sem.R` | `Table_SM.D.5_SEM_full_compliance_equation.docx` |
| Table SM.E.1 — Mixed-effects models | `02_regressions.R` | `Table_SM.E.1_Mixed_effects.docx` |
| Table SM.E.2 — Stage 2 − Stage 1 differences | `03_sem.R` | `Table_SM.E.2_SEM_stage_differences.docx` |
| Figure SM.E.1 — λ sensitivity | `03_sem.R` | `Figure_SM.E.1_Lambda_sensitivity.jpg` |
| Table SM.E.3 — Estimates behind Figure SM.E.1 | `03_sem.R` | `Table_SM.E.3_Lambda_sensitivity_estimates.docx` |
| Table SM.E.4 — γ and λ, observed compliance over 3 rounds | `04_robustness.R` | `Table_SM.E.4_Obs3_gamma_lambda.docx` |
| Figure SM.E.2 — λ sensitivity, observed compliance over 3 rounds | `04_robustness.R` | `Figure_SM.E.2_Obs3_lambda_sensitivity.jpg` |
| Table SM.E.5 — Estimates behind Figure SM.E.2 | `04_robustness.R` | `Table_SM.E.5_Obs3_lambda_sensitivity_estimates.docx` |
| Table SM.E.6 — γ and λ, tobit compliance equation | `04_robustness.R` | `Table_SM.E.6_Tobit_gamma_lambda.docx` |
| Figure SM.E.3 — λ sensitivity, tobit compliance equation | `04_robustness.R` | `Figure_SM.E.3_Tobit_lambda_sensitivity.jpg` |
| Table SM.E.7 — Estimates behind Figure SM.E.3 | `04_robustness.R` | `Table_SM.E.7_Tobit_lambda_sensitivity_estimates.docx` |
| Table SM.F.1 — Reset of beliefs and compliance | `05_identity_vs_learning.R` | `Table_SM.F.1_Reset_beliefs_compliance.docx` |
| Table SM.F.2 — Carry-over of Stage 1 updating | `05_identity_vs_learning.R` | `Table_SM.F.2_Belief_carryover.docx` |
| *Not in the manuscript:* all SEM parameters; γ/λ plot | `03_sem.R` | `Extra_SEM_Round8_all_parameters.docx`, `Extra_Figure_SEM_gamma_lambda.jpg` |
| *Not in the manuscript (optional, off by default):* Tables J1–J5 | `05_identity_vs_learning.R` | `Extra_Table_J1_…` to `Extra_Table_J5_…` |

The R2 file names follow `obs_k_R2` (`Obs3` for the default of three rounds). Figures 1–3, Supplementary Figure S1 and Figures SM.C.1–SM.C.3 (map, design, conceptual framework, poster, screens) are not produced by the code.

Tables without standard errors (Tables SM.D.1–SM.D.3, the means and SDs in Table 1, and the optional Table J4) are descriptive and involve no bootstrap.

## Robustness checks (`04_robustness.R`)

| Manuscript items | Label in the code | What changes | Output files |
|---|---|---|---|
| Tables SM.E.4–SM.E.5, Figure SM.E.2 | R2 | Observed compliance = mean of the **three rounds** preceding the decision round (Rounds 5–7 for Round 8), instead of the preceding round | `Table_SM.E.4_Obs3_gamma_lambda.docx`, `Figure_SM.E.2_Obs3_lambda_sensitivity.jpg`, `Table_SM.E.5_Obs3_lambda_sensitivity_estimates.docx` |
| Tables SM.E.6–SM.E.7, Figure SM.E.3 | R1 | Compliance equation estimated as a two-limit **tobit** (censored at 0 and 1) | `Table_SM.E.6_Tobit_gamma_lambda.docx`, `Figure_SM.E.3_Tobit_lambda_sensitivity.jpg`, `Table_SM.E.7_Tobit_lambda_sensitivity_estimates.docx` |

- **Why only the compliance equation changes in the tobit version.** The SEM is recursive, and the compliance-equation error is uncorrelated with the errors of the belief equations, so the likelihood factorizes by equation. The SEM's compliance-equation estimates are identical to OLS of that equation (checked numerically), and the belief equations (Table 4) do not change. The tobit version replaces the Gaussian likelihood of the compliance equation with a two-limit tobit (`survival::survreg`).
- **γ in Table SM.E.6** is reported twice: as the sum of latent-index coefficients, and as the average marginal effect on observed compliance (γ × average probability of an uncensored outcome). The λ's are ratios, so they are the same in both scales.
- **Inference for the tobit (Tables SM.E.6–SM.E.7, Figure SM.E.3)** uses the score wild bootstrap (see *Inference*), with the same weights as Table 3. Its standard errors match the cluster-robust (sandwich) standard errors of the tobit as the number of replications grows (checked numerically).
- **Three-round window.** `obs_k_R2` (default 3) sets the number of rounds. For decision rounds 2 and 3, fewer preceding rounds exist; all available ones are used.
- Tables SM.E.4 and SM.E.6 put the main estimates (Table 3) next to the robustness estimates. Both use the same seeds and therefore the same bootstrap weights, so differences are not due to bootstrap noise.

## Supplementary Material F: are Stage 2 beliefs anchored on Stage 1 experience? (`05_identity_vs_learning.R`)

Stage 1 always precedes Stage 2. These analyses ask whether prior beliefs about the named unions depend on what participants experienced in Stage 1, using beliefs elicited at the start and end of each stage, the in-group as a benchmark whose referent does not change, and the TURF as a behavioural benchmark without any change in identity information.

| Table | Question |
|---|---|
| SM.F.1 | Do mean beliefs (Panel A) and compliance (Panel B) return to their start-of-Stage-1 level at the start of Stage 2? Shared Area vs TURF for compliance. |
| SM.F.2 | What share of each participant's own Stage 1 updating is retained in their Stage 2 prior, for the named out-group unions vs the in-group? |

- **Inference.** The same wild cluster bootstrap: Table SM.F.1 rebuilds each variable around its mean (all statistics are functions of means); Table SM.F.2 and the optional J tables are OLS.
- **Group reconstruction (optional tables only).** Members of a group share the same group total (own + others' extraction) within a matching group and round; ties are resolved with each member's recorded composition (`combi`). Groups are recovered for about 98% of participant-rounds.
- **Optional tables J1–J5** (`run_identity_content <- TRUE`): what the identity information contains (trust, conflict, union reputations, round-level composition). They do not address the reset question.
- **Settings:** `named_scope` (`"pool"`: only named unions present in the matching group; `"all"`: as in the main analysis), `run_identity_content`, `run_option4`.
- **Z23 pools.** In the two Z23 matching groups, participants were asked about two named unions, but only one was present in their matching group. With `named_scope = "pool"` the absent union is excluded here; the main analysis (Tables 1, 3, 4, SM.D.3–SM.D.5) averages both.

## Items to align in the manuscript

Checked against `Manuscrito_Islitas_RGDLedit_13.11.26.docx`. Table and figure numbers in the code now match the manuscript, so the earlier renumbering list is no longer needed.

1. **Inference text.** A few passages still describe the earlier bootstrap, while every table note describes the wild cluster bootstrap (2,000 replications, Webb six-point weights, 17 matching groups):
   - Section 4.3: "1,000 nonparametric bootstrap replications", re-estimated "on a resample of participants drawn with replacement".
   - Section 5.1 (Table 2): "standard errors are clustered at the participant level".
   - Supplementary Material F.1: "2,000 bootstrap replications that resample the 17 matching groups with replacement"; F.3: "estimated on the same bootstrap resamples" (the two equations now share the same bootstrap weights).
   The suggested text in *Inference* can replace the first two.
2. **Carry-over of out-group priors.** Table SM.F.2 marks ρ for the named unions with † (p < 0.1), while the text (Sections 4.1 and 5, and the introduction and F.3 of Supplementary Material F) describes it as statistically non-significant. Describing it as not significant at the 5% level would match the table.
3. **References.** Kline and Santos (2012) is cited in the notes of Tables SM.E.6 and SM.E.7 but is not in the reference list. If the suggested inference text is used, Cameron, Gelbach and Miller (2008) and Webb (2023) are needed too (see *References*).
4. **Table SM.D.4 F statistic.** It is a Wald test using the bootstrap covariance matrix (Wald statistic divided by the number of slopes). The note written by the code says so; the manuscript note does not.
5. **Table 3 (earlier `SEM_lambdas.R`):** the labels for λ₁ and λ₂ in `var_labels` didn't match `pred_order`, so those two rows were dropped from the exported table. Fixed. λ₁ = in-group and λ₂ = out-group, as in the manuscript.

## Changes in this version

- **Output names follow the manuscript.** Every output file starts with its number in the manuscript, and captions and cross-references inside the notes use the same numbers. The tables of estimates behind the sensitivity figures are now Tables SM.E.3, SM.E.5 and SM.E.7, as in the manuscript. Outputs that are not in the manuscript start with `Extra_`. `All_tables.docx` lists the main tables, then the Supplementary Material tables by section (D, E, F), then the extras. Main tables keep their names (`Table_1_…` to `Table_4_…`); the other files were renamed as follows:

| Earlier file | Now |
|---|---|
| `Table_S1_Sample_characteristics.docx` | `Table_SM.D.1_Sample_characteristics.docx` |
| `Table_S2_Trust_conflict.docx` | `Table_SM.D.2_Trust_conflict.docx` |
| `Table_S3_Belief_changes.docx` | `Table_SM.D.3_Belief_changes.docx` |
| `Table_S4_Compliance_change_OLS.docx` | `Table_SM.D.4_Compliance_change_OLS.docx` |
| `Table_S5_SEM_full_compliance_equation.docx` | `Table_SM.D.5_SEM_full_compliance_equation.docx` |
| `Table_S6_Mixed_effects.docx` | `Table_SM.E.1_Mixed_effects.docx` |
| `Table_S7_SEM_stage_differences.docx` | `Table_SM.E.2_SEM_stage_differences.docx` |
| `Figure_S2_Lambda_sensitivity.jpg` | `Figure_SM.E.1_Lambda_sensitivity.jpg` |
| `Figure_S2_estimates.docx` | `Table_SM.E.3_Lambda_sensitivity_estimates.docx` |
| `Table_R2_Obs3_gamma_lambda.docx` | `Table_SM.E.4_Obs3_gamma_lambda.docx` |
| `Figure_R2_Obs3_lambda_sensitivity.jpg` | `Figure_SM.E.2_Obs3_lambda_sensitivity.jpg` |
| `Figure_R2_estimates.docx` | `Table_SM.E.5_Obs3_lambda_sensitivity_estimates.docx` |
| `Table_R1_Tobit_gamma_lambda.docx` | `Table_SM.E.6_Tobit_gamma_lambda.docx` |
| `Figure_R1_Tobit_lambda_sensitivity.jpg` | `Figure_SM.E.3_Tobit_lambda_sensitivity.jpg` |
| `Figure_R1_estimates.docx` | `Table_SM.E.7_Tobit_lambda_sensitivity_estimates.docx` |
| `Table_S8_Reset_beliefs_compliance.docx` | `Table_SM.F.1_Reset_beliefs_compliance.docx` |
| `Table_S9_Belief_carryover.docx` | `Table_SM.F.2_Belief_carryover.docx` |
| `SEM_Round8_all_parameters.docx` | `Extra_SEM_Round8_all_parameters.docx` |
| `Figure_SEM_gamma_lambda.jpg` | `Extra_Figure_SEM_gamma_lambda.jpg` |
| `Table_J1_…` to `Table_J5_…` (optional) | `Extra_Table_J1_…` to `Extra_Table_J5_…` |

## Earlier changes

- **One inference method.** The wild cluster bootstrap (previously only an option for the SEM, in `00_setup2.R` / `03_sem2.R`) now produces every standard error in the package: Table 1 trends, Tables 2, SM.D.4, SM.E.1, SM.E.4–SM.E.7, SM.F.1, SM.F.2 and J1–J5 and Figures SM.E.2–SM.E.3, in addition to the SEM tables and Figure SM.E.1. The pairs (resampling) bootstrap and the `boot_unit` / `sem_boot_method` settings were removed; `boot_cluster` selects the clustering level.
- **SEM results unchanged.** With the same seed and number of replications, the SEM draws are identical to those of the earlier `00_setup2.R` / `03_sem2.R` wild version (checked to machine precision), provided the matching groups sort in the same order. The weights are now assigned to matching groups in C-locale order, so on a computer whose language settings order the group names differently, the bootstrap SEs can differ slightly from earlier runs (Monte Carlo variation only).
- **Quick runs** go to `Outputs/Replication/quick_run/` and never overwrite the full results.
- **Checks added.** `run_sem_spec()` verifies that the SEM's ML estimates equal equation-by-equation OLS (the basis of the SEM wild bootstrap), and `00_setup.R` warns if R is not running in a UTF-8 locale.

## References

- Cameron, A. C., Gelbach, J. B., and Miller, D. L. (2008). Bootstrap-based improvements for inference with clustered errors. *Review of Economics and Statistics*, 90(3), 414–427.
- Kline, P., and Santos, A. (2012). A score based approach to wild bootstrap inference. *Journal of Econometric Methods*, 1(1), 23–41.
- Modugno, L., and Giannerini, S. (2015). The wild bootstrap for multilevel models. *Communications in Statistics – Theory and Methods*, 44(22), 4812–4825.
- Webb, M. D. (2023). Reworking wild bootstrap-based inference for clustered errors. *Canadian Journal of Economics*, 56(3), 839–858.
