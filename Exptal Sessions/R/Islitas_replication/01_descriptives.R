# ==============================================================================
# Replication package -- 01_descriptives.R
#   Table 1       Descriptive compliance and prior beliefs across conditions
#   Table SM.D.1  Sample characteristics
#   Table SM.D.2  Trust and previous conflict by group-affiliation condition
#   Table SM.D.3  Changes in prior beliefs across identity-information conditions
#
# Inference (Table 1 trends): wild cluster bootstrap of 00_setup.R.
# ==============================================================================

if (!exists("setup_done")) source("00_setup.R")

# ------------------------------------------------------------------------------
# Table SM.D.1: Sample characteristics
# ------------------------------------------------------------------------------
edu_props <- prop.table(table(df_wide$survey3.1.player.estudios))
exp_props <- prop.table(table(df_wide$survey3.1.player.experiencia))
get_prop  <- function(tab, k) if (k %in% names(tab)) unname(tab[k]) else 0

tableS1 <- data.frame(
  Characteristic = c(
    "Number of participants", "Male", "Education",
    "   No formal studies", "   Incomplete primary education", "   Complete primary education",
    "   Incomplete secondary education", "   Complete secondary education",
    "Experience in TURFs", "   More than 10 years", "   5–9 years", "   4 years or less",
    "Has held a union leadership role", "Average weekly time spent gathering loco"
  ),
  Value = c(
    as.character(nrow(df_wide)),
    fmt_pct(mean(df_wide$survey3.1.player.sexo == 0, na.rm = TRUE)),   # 0 = male
    "",
    fmt_pct(get_prop(edu_props, "1")), fmt_pct(get_prop(edu_props, "2")),
    fmt_pct(get_prop(edu_props, "3")), fmt_pct(get_prop(edu_props, "4")),
    fmt_pct(get_prop(edu_props, "5")),
    "",
    fmt_pct(get_prop(exp_props, "4")),                                  # 10+ years
    fmt_pct(get_prop(exp_props, "3")),                                  # 5-9 years
    fmt_pct(get_prop(exp_props, "1") + get_prop(exp_props, "2")),       # <1 and 1-4 years
    fmt_pct(mean(df_wide$survey3.1.player.liderazgo %in% c("Sí", 1, "1"), na.rm = TRUE)),
    sprintf("%.1f hours", mean(df_wide$survey3.1.player.horas_trabajo, na.rm = TRUE))
  )
)
export_table(tableS1, "Table_SM.D.1_Sample_characteristics", "Table SM.D.1 | Sample characteristics",
             notes = "Percentages are proportions of participants, unless otherwise stated.")

# ------------------------------------------------------------------------------
# Table SM.D.2: Trust and previous conflict (four-point scales rescaled to [0,1])
# ------------------------------------------------------------------------------
rescale01 <- function(x) (x - 1) / 3
att <- list(
  "   In-group"          = df_wide$survey1.1.player.confianza_caleta,
  "   Unknown out-group" = df_wide$survey1.1.player.confianza_pm,
  "   Known out-group"   = df_wide$survey2.1.player.confianza_caleta_conocida_mean,
  "   In-group "         = df_wide$survey1.1.player.conflicto_caleta,
  "   Unknown out-group "= df_wide$survey1.1.player.conflicto_pm,
  "   Known out-group "  = df_wide$survey2.1.player.conflicto_caleta_conocida_mean
)
att_stats <- lapply(att, function(x) {
  x <- rescale01(x); c(sprintf("%.2f", mean(x, na.rm = TRUE)), sprintf("%.2f", sd(x, na.rm = TRUE)))
})
tableS2 <- data.frame(
  `Attitudinal measure` = c("Trust", names(att)[1:3], "Previous conflict", names(att)[4:6]),
  Mean = c("", sapply(att_stats[1:3], `[`, 1), "", sapply(att_stats[4:6], `[`, 1)),
  SD   = c("", sapply(att_stats[1:3], `[`, 2), "", sapply(att_stats[4:6], `[`, 2)),
  check.names = FALSE
)
tableS2$`Attitudinal measure` <- sub(" +$", "", tableS2$`Attitudinal measure`)
export_table(tableS2, "Table_SM.D.2_Trust_conflict",
             "Table SM.D.2 | Trust and previous conflict by group-affiliation condition",
             notes = paste("Four-point scales rescaled to [0,1] as (x - 1)/3. In the SEM these",
                           "measures enter standardized (mean 0, SD 1)."))

# ------------------------------------------------------------------------------
# Table 1: Descriptive compliance, prior beliefs and within-stage trends
# ------------------------------------------------------------------------------
belief01 <- function(col) {
  if (col %in% names(df_wide)) 1 - df_wide[[col]] / 50 else rep(NA_real_, nrow(df_wide))
}
beliefs <- list(
  turf_in_T1 = belief01("beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini"),
  turf_in_T2 = belief01("beliefsT2inicial.1.player.T2_belief_caleta_en_amerb_ini"),  # not elicited -> "-"
  sa_in_T1   = belief01("beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini"),
  sa_in_T2   = belief01("beliefsT2inicial.1.player.T2_belief_caleta_ini"),
  sa_out_T1  = belief01("beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini"),
  sa_out_T2  = belief01("beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini")
)

long_c <- df_long %>%
  mutate(compliance_turf = 1 - extraction_amerb / 50,
         compliance_sa   = 1 - extraction_OA / 50)

trend_fits <- list()
for (area in c("compliance_turf", "compliance_sa")) {
  for (stage in c("T1", "T2")) {
    trend_fits[[paste(area, stage, sep = "_")]] <-
      lm_boot(as.formula(paste(area, "~ round")), filter(long_c, treatment == stage),
              seed = seed_main + 1)
  }
}
trend_cell <- function(key) {
  r <- trend_fits[[key]]$coefs; r <- r[r$term == "round", ]
  fmt_coef(r$est, r$se, r$pvalue)
}
avg_cell <- function(area, stage) fmt_msd(long_c[[area]][long_c$treatment == stage])

table1 <- data.frame(
  ` ` = c("Information about out-groups", "Prior belief", "   In-groups compliance",
          "   Out-groups compliance", "Average level of compliance", "Trend in compliance (OLS)"),
  `TURF (Stage 1)` = c("–", "", fmt_msd(beliefs$turf_in_T1), "–",
                       avg_cell("compliance_turf", "T1"), trend_cell("compliance_turf_T1")),
  `TURF (Stage 2)` = c("–", "", fmt_msd(beliefs$turf_in_T2), "–",
                       avg_cell("compliance_turf", "T2"), trend_cell("compliance_turf_T2")),
  `Shared Area (Stage 1)` = c("Unknown union", "", fmt_msd(beliefs$sa_in_T1), fmt_msd(beliefs$sa_out_T1),
                              avg_cell("compliance_sa", "T1"), trend_cell("compliance_sa_T1")),
  `Shared Area (Stage 2)` = c("Known union", "", fmt_msd(beliefs$sa_in_T2), fmt_msd(beliefs$sa_out_T2),
                              avg_cell("compliance_sa", "T2"), trend_cell("compliance_sa_T2")),
  check.names = FALSE
)
export_table(table1, "Table_1_Compliance_beliefs_trends",
             "Table 1 | Descriptive compliance and prior beliefs across experimental conditions",
             notes = c("Trends: OLS of round-level compliance on round number, by game and stage.",
                       boot_note(trend_fits[[1]]$G, model = "ols"), star_note))

# ------------------------------------------------------------------------------
# Table SM.D.3: Changes in prior beliefs between stages (Shared Area)
# ------------------------------------------------------------------------------
change_dist <- function(after, before) {
  d <- after - before; d <- d[!is.na(d)]
  c(mean(d < -1e-9), mean(abs(d) <= 1e-9), mean(d > 1e-9))
}
s3 <- rbind(change_dist(beliefs$sa_out_T2, beliefs$sa_out_T1),
            change_dist(beliefs$sa_in_T2,  beliefs$sa_in_T1))
tableS3 <- data.frame(
  `Prior beliefs` = c("Out-group compliance: Stage 2 vs Stage 1",
                      "In-group compliance: Stage 2 vs Stage 1"),
  `More pessimistic` = fmt_pct(s3[, 1], 1),
  Unchanged          = fmt_pct(s3[, 2], 1),
  `More optimistic`  = fmt_pct(s3[, 3], 1),
  check.names = FALSE
)
export_table(tableS3, "Table_SM.D.3_Belief_changes",
             "Table SM.D.3 | Changes in prior beliefs across identity-information conditions",
             notes = "Share of participants whose elicited belief about compliance fell, did not change, or rose.")
