# ==============================================================================
# Replication package -- 00_data_management.R
# Builds the two analysis files from the raw oTree exports (all_apps_wide*.csv,
# one file per session) in path_datos:
#   Datos_islitas_replication_wide.Rdata  object df       one row per participant
#   Datos_islitas_replication_long.Rdata  object dfs_long one row per participant x stage x round
#
# Only the variables used by 01_descriptives.R, 02_regressions.R and 03_sem.R
# are constructed. Everything else in df is the raw export, unchanged.
# ==============================================================================

if (!exists("path_datos")) source("00_paths.R")
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

# ------------------------------------------------------------------------------
# 1. Read and stack the raw session files
# ------------------------------------------------------------------------------
file_list <- list.files(path = path_datos, pattern = "^all_apps_wide.*\\.csv$", full.names = TRUE)
if (length(file_list) == 0) stop("No all_apps_wide*.csv files found in ", path_datos)
df <- do.call(rbind, lapply(file_list, read.csv))
message("Raw data: ", length(file_list), " session files, ", nrow(df), " participants")

# ------------------------------------------------------------------------------
# 2. Matching group in the Shared Area (bootstrap unit): zone x session
# ------------------------------------------------------------------------------
df$gid.treat <- paste0(df$participant.zonaT2, ".", df$session.code)

# ------------------------------------------------------------------------------
# 3. Stage 2 out-group measures. In Stage 2 each participant faced one or two
#    identified unions; beliefs, trust and conflict are averaged over them
#    (only the first union is used when there was a single one).
# ------------------------------------------------------------------------------
df <- df %>%
  mutate(
    beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini = if_else(
      !is.na(beliefsT2inicial.1.player.T2_belief_caleta_conocida2_ini),
      as.double((beliefsT2inicial.1.player.T2_belief_caleta_conocida1_ini +
                   beliefsT2inicial.1.player.T2_belief_caleta_conocida2_ini) / 2),
      as.double(beliefsT2inicial.1.player.T2_belief_caleta_conocida1_ini)
    )
  )

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

# ------------------------------------------------------------------------------
# 4. Long data: one row per participant x stage (T1, T2) x round (1-8 within stage)
#    extraction_amerb = own extraction in the TURF
#    extraction_OA    = own extraction in the Shared Area (T1: "libre", T2: "metat")
# ------------------------------------------------------------------------------
ext_cols <- c(paste0("T1juegoalgas.", 1:8, ".player.T1_extraccion_", rep(c("amerb", "libre"), each = 8)),
              paste0("T2juegoalgas.", 1:8, ".player.T2_extraccion_", rep(c("amerb", "metat"), each = 8)))

dfs_long <- df %>%
  select(participant.code, gid.treat, all_of(ext_cols)) %>%
  pivot_longer(all_of(ext_cols),
               names_to = c("treatment", "round", "area"),
               names_pattern = "(T\\d)juegoalgas\\.(\\d+)\\.player\\.T\\d_extraccion_(.+)",
               values_to = "extraction") %>%
  mutate(round = as.integer(round),
         area  = if_else(area == "amerb", "extraction_amerb", "extraction_OA")) %>%
  pivot_wider(names_from = area, values_from = extraction) %>%
  arrange(participant.code, treatment, round) %>%
  as.data.frame()

# ------------------------------------------------------------------------------
# 5. Save
# ------------------------------------------------------------------------------
save(df, file = file_wide)
save(dfs_long, file = file_long)
message("Saved: ", basename(file_wide), " (", nrow(df), " rows) and ",
        basename(file_long), " (", nrow(dfs_long), " rows)")

# ------------------------------------------------------------------------------
# 6. Check against the files produced by the earlier Data_management.R
#    (Datos_islitas_recode.Rdata / Datos_islitas_long.Rdata), if present.
#    Compares every variable the analysis uses.
# ------------------------------------------------------------------------------
old_wide <- file.path(path_datos, "Datos_islitas_recode.Rdata")
old_long <- file.path(path_datos, "Datos_islitas_long.Rdata")
if (file.exists(old_wide) && file.exists(old_long)) {
  old <- new.env(); load(old_wide, envir = old); load(old_long, envir = old)
  vars_wide <- c(
    "gid.treat",
    "survey1.1.player.confianza_caleta", "survey1.1.player.confianza_pm",
    "survey1.1.player.conflicto_caleta", "survey1.1.player.conflicto_pm",
    "survey2.1.player.confianza_caleta_conocida_mean", "survey2.1.player.conflicto_caleta_conocida_mean",
    "survey3.1.player.sexo", "survey3.1.player.estudios", "survey3.1.player.experiencia",
    "survey3.1.player.liderazgo", "survey3.1.player.horas_trabajo",
    "beliefsT1inicial.1.player.T1_belief_caleta_en_amerb_ini",
    "beliefsT1inicial.1.player.T1_belief_caleta_en_libre_ini",
    "beliefsT1inicial.1.player.T1_belief_pm_en_libre_ini",
    "beliefsT2inicial.1.player.T2_belief_caleta_ini",
    "beliefsT2inicial.1.player.T2_belief_caleta_conocida_mean_ini",
    grep("^T[12]juegoalgas\\.[1-8]\\.player\\.T[12]_extraccion_(otros_)?(libre|metat)$", names(df), value = TRUE))
  ow <- as.data.frame(old$df); ol <- as.data.frame(old$dfs_long)
  ow <- ow[order(ow$participant.code), ]; nw <- df[order(df$participant.code), ]
  bad_w <- vars_wide[!vapply(vars_wide, function(v)
    v %in% names(ow) && isTRUE(all.equal(ow[[v]], nw[[v]], check.attributes = FALSE)), logical(1))]
  key <- c("participant.code", "treatment", "round")
  m <- merge(ol[, c(key, "gid.treat", "extraction_amerb", "extraction_OA")], dfs_long,
             by = key, suffixes = c(".old", ".new"), all = TRUE)
  bad_l <- c("gid.treat", "extraction_amerb", "extraction_OA")[!vapply(
    c("gid.treat", "extraction_amerb", "extraction_OA"), function(v)
      isTRUE(all.equal(m[[paste0(v, ".old")]], m[[paste0(v, ".new")]], check.attributes = FALSE)), logical(1))]
  if (nrow(ow) == nrow(nw) && length(bad_w) == 0 && nrow(m) == nrow(dfs_long) && length(bad_l) == 0) {
    message("Check: all analysis variables are identical to the earlier data files.")
  } else {
    warning("Differences with the earlier data files in: ",
            paste(c(if (nrow(ow) != nrow(nw)) "number of participants", bad_w,
                    if (nrow(m) != nrow(dfs_long)) "long-data rows", bad_l), collapse = ", "))
  }
  rm(old, ow, ol, nw, m)
}
