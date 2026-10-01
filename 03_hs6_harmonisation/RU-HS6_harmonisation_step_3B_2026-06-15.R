# =============================================================================
# ADD-ON SCRIPT — goods type  x  direction, monthly.   Run AFTER Step 3.    [RU]
#
# Standalone: reads Step 3's on-disk outputs, edits nothing. Source it, or run
#   Rscript RU-HS6_harmonisation_step_3B_2026-06-15.R
#
# Inputs (from the newest RU-Output_R_Step3_* under BASE_DIR):
#   RU-STEP3_long_state_<T>_hs6.csv   monthly ON panels (direction, hs6, month)
#   RU-STEP3_<T>_hs6.csv              wide per-track files (for dual_use / chpi)
# Output (written into that same Step 3 dir, timestamped, never overwrites):
#   RU-STEP3_goods_by_direction_monthly_<ts>.csv
#     direction, month, goods_category, n_hs6, n_hs6_dual_use, n_hs6_chpi
#
# n_hs6 = distinct HS6 sanctioned in that direction, that month, that band
# (active stock — growth AND de-listing dips both show). Expanded HS6-panel
# view; the raw "as-listed CN" view is RU-CN_summary_by_goods_*.
# =============================================================================
suppressMessages({library(dplyr); library(readr); library(stringr); library(purrr)})

# ---- CONFIG (matches your Step 3 BASE_DIR) ----------------------------------
BASE_DIR <- "data/RU-A_recent_run"
# Override with a specific Step 3 folder if you don't want the newest:
STEP3_DIR <- NULL

# ---- locate the Step 3 output dir -------------------------------------------
if (is.null(STEP3_DIR)) {
  cand <- list.dirs(BASE_DIR, recursive = FALSE, full.names = TRUE)
  cand <- cand[grepl("^RU-Output_R_Step3", basename(cand))]
  if (!length(cand)) stop("No RU-Output_R_Step3_* dir under BASE_DIR = ", BASE_DIR)
  STEP3_DIR <- cand[which.max(file.mtime(cand))]
}
message("Step 3 dir: ", STEP3_DIR)

# ---- goods band from HS chapter (first 2 digits) ----------------------------
goods_band <- function(hs6) {
  ch <- suppressWarnings(as.integer(substr(gsub("\\D", "", hs6), 1, 2)))
  case_when(
    ch %in% c(25, 26, 27) ~ "Mineral products / fuels",
    ch %in% 28:38         ~ "Chemicals & allied",
    ch %in% 72:83         ~ "Base metals & articles",
    ch %in% c(84, 85)     ~ "Machinery & electrical",
    ch %in% 86:89         ~ "Transport equipment",
    ch %in% 90:92         ~ "Precision & optical",
    TRUE                  ~ "Other"
  )
}
CAT_ORDER <- c("Mineral products / fuels", "Chemicals & allied",
               "Base metals & articles", "Machinery & electrical",
               "Transport equipment", "Precision & optical", "Other")

# ---- monthly ON panels (direction, hs6, month) ------------------------------
ls_files <- list.files(STEP3_DIR, pattern = "^RU-STEP3_long_state_.*_hs6\\.csv$",
                       full.names = TRUE)
if (!length(ls_files)) stop("No RU-STEP3_long_state_*_hs6.csv in ", STEP3_DIR)
panel <- map_dfr(ls_files, ~ read_csv(.x, show_col_types = FALSE,
                                      col_types = cols(.default = "c")))

# ---- per-HS6 flags from the wide per-track files (exclude long_state) -------
wide_files <- list.files(STEP3_DIR, pattern = "^RU-STEP3_[A-Z_]+_hs6\\.csv$",
                         full.names = TRUE)
wide_files <- wide_files[!grepl("long_state", wide_files)]
flags <- map_dfr(wide_files, ~ read_csv(.x, show_col_types = FALSE,
                                        col_types = cols(.default = "c")) %>%
                                 select(hs6, dual_use, chpi)) %>%
         distinct(hs6, .keep_all = TRUE)

# ---- tag + aggregate --------------------------------------------------------
agg <- panel %>%
  left_join(flags, by = "hs6") %>%
  mutate(goods_category = factor(goods_band(hs6), levels = CAT_ORDER),
         direction      = str_to_upper(direction)) %>%
  group_by(direction, month, goods_category) %>%
  summarise(n_hs6          = n_distinct(hs6),
            n_hs6_dual_use = n_distinct(hs6[dual_use == "Y"]),
            n_hs6_chpi     = n_distinct(hs6[chpi == "Y"]),
            .groups = "drop") %>%
  arrange(direction, month, goods_category)

ts  <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
out <- file.path(STEP3_DIR, sprintf("RU-STEP3_goods_by_direction_monthly_%s.csv", ts))
write_csv(agg, out)
message(sprintf("wrote %s  (%d rows)", basename(out), nrow(agg)))
print(agg %>% filter(direction == "EXPORT", month == max(month)) %>%
        select(goods_category, n_hs6, n_hs6_dual_use))
