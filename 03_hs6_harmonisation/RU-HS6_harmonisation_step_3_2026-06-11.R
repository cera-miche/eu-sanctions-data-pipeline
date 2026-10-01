# =============================================================================
# STEP 3 (directional) — per-HS6 sanctioning-cycle lookup, ONE FILE PER TRACK
#                                                                          [RU]
#
# Replaces the interim direction-AGNOSTIC Step 3. Joins the HS6 panel (STEP2)
# with the Imp/Exp classifier's per-annex directions, then for EACH of five
# tracks builds a wide one-row-per-HS6 table recording the ON/OFF cycle:
#   EXPORT, IMPORT, TRANSIT_BAN, NO_RE_EXPORT, PRICE_CAP.
#
# Per track, an HS6 is "sanctioned in version V" if it sits (in STEP2) under
# at least one annex that the classifier tags with that direction in V
# (logical OR across annexes). The three-category annex rule falls out for
# free: set-aside / non-sanctioning annexes never appear in the directions
# file, so they never join.
#
# OUTPUT (per track T): RU-STEP3_<T>_hs6.csv, columns:
#   hs6, chpi[Y/N], dual_use[Y/N], ever_sanctioned[Y/N], ever_de_sanctioned[Y/N],
#   first_sanc_version, first_sanc_month,            (cycle 1 ON)
#   first_de_sanc_version, first_de_sanc_month,      (cycle 1 OFF)
#   first_re_sanc_version, first_re_sanc_month,      (cycle 2 ON)
#   first_re_de_sanc_version, first_re_de_sanc_month,(cycle 2 OFF)
#   n_episodes, n_transitions, truncated_extra_cycles[Y/N]
#   (+ partC_indirect_pending[Y/N] in the IMPORT file only)
# Plus RU-STEP3_de_sanction_events.csv — every OFF transition across tracks,
#   for human review (de-sanctions are often vintage-renumbering / annex-
#   restructuring artefacts rather than real policy reversals).
#
# ADDED 2026-06-15: RU-STEP3_long_state_<T>_hs6.csv — the COMPLETE monthly ON
#   panel per track (one row per hs6 x month the code is sanctioned in that
#   direction), written straight from panel_T BEFORE the two-episode collapse.
#   This is the LOSSLESS source for the Stage-4 trade merge (treatment
#   representation "B"): unlike the wide cycle table it is NOT capped at two
#   episodes, so the ~57 codes flagged truncated_extra_cycles=Y (3rd+ on/off
#   cycle) are fully represented. Columns: direction, hs6, month("%Y-%m").
#   SPARSE: only sanctioned months are listed; absence of a (hs6, month) row
#   means "not sanctioned that month for that direction" (-> 0 in the merge).
#
# DECISIONS BAKED IN (locked 2026-06-10):
#   - Annex II: EXPORT from 2022-03-16. AUTHORISATION_REQUIRED_EXPORT is NOT
#     folded into EXPORT by default; set INCLUDE_AUTH_WINDOW = TRUE to also
#     count the 2014-2022 authorisation regime as EXPORT (pushes those codes'
#     first-sanctioned date back to 2014).
#   - XXIII wind-down family: EXCLUDED in v1 (those annexes are set-aside, so
#     they never join here). ~65 EXPORT HS6 under-counted; reversible later by
#     reading RU-annex_verified_refinements.csv.
#   - Annex XXXVIIIA: import-treated from its effective date; the IMPORT file
#     carries partC_indirect_pending = Y for the Part C codes (all XXXVIIIA
#     codes EXCEPT the five recovered Part A/B diamonds), flagging the leg
#     whose indirect/third-country ban is suspended pending a Council date.
#
# 15th-day rule: effective day <= 14 -> that month; >= 15 -> next month
#   (matches the interim Step 3; source of truth is THIS code).
#
# GUARD: asserts the directions file covers every STEP2 version. A version
#   present in STEP2 but missing from directions would make every code read
#   un-sanctioned for that version, manufacturing a phantom de-sanction cycle.
#   The script halts (writing RU-STEP3_MISSING_versions.csv) rather than emit
#   a corrupted panel.
#
# =============================================================================

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(purrr)
library(lubridate)

# ---- Configuration ----------------------------------------------------------
BASE_DIR   <- "data/RU-A_recent_run"

# --- output folders are timestamped so runs are never overwritten ------------
run_timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

# newest folder in `base` whose name matches regex `prefix`; follows the
# timestamped output folders of upstream steps (also matches the old fixed
# folder names). Newest by mtime wins.
latest_dir <- function(base, prefix) {
  d <- list.dirs(base, recursive = FALSE, full.names = TRUE)
  d <- d[grepl(prefix, basename(d))]
  if (length(d) == 0) stop("No folder matching '", prefix, "' inside ", base)
  d[which.max(file.mtime(d))]
}
STEP2_FILE <- file.path(latest_dir(BASE_DIR, "^RU-Output_R_Step2($|_)"), "RU-STEP2_hs6_long.csv")
DUALUSE_FILE <- file.path(latest_dir(BASE_DIR, "^RU-Output_R_Step2B($|_)"), "RU-STEP2B_dualuse_hs6.csv")
OUTPUT_DIR <- file.path(BASE_DIR, paste0("RU-Output_R_Step3_", run_timestamp))

# Directions CSV: auto-detect the most recent RU-annex_directions_all_versions_*
DIR_DIR <- BASE_DIR
INCLUDE_AUTH_WINDOW <- FALSE   # see header; FALSE -> Annex II EXPORT from 2022-03-16

TRACKS <- list(
  EXPORT          = if (INCLUDE_AUTH_WINDOW) c("EXPORT", "AUTHORISATION_REQUIRED_EXPORT") else c("EXPORT"),
  IMPORT          = c("IMPORT"),
  TRANSIT_BAN     = c("TRANSIT_BAN"),
  NO_RE_EXPORT    = c("NO_RE_EXPORT"),
  PRICE_CAP       = c("PRICE_CAP"),
  AUTH_REQ_EXPORT = c("AUTHORISATION_REQUIRED_EXPORT"),   # authorisation to SELL (export licence)
  AUTH_REQ_IMPORT = c("AUTHORISATION_REQUIRED_IMPORT")    # authorisation to BUY  (import licence)
)

# Part A/B diamond codes (recovered in CN extractor v6); everything else under
# Annex XXXVIIIA is Part C (indirect ban suspended pending Council decision).
PART_AB_DIAMONDS <- c("710210", "710231", "710239", "710421", "710491")

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

dir_cands <- list.files(DIR_DIR, pattern = "^RU-annex_directions_all_versions_.*\\.csv$",
                        full.names = TRUE)
if (length(dir_cands) == 0) stop("No RU-annex_directions_all_versions_*.csv in ", DIR_DIR)
DIRECTIONS_FILE <- dir_cands[which.max(file.mtime(dir_cands))]
cat("Directions file:", basename(DIRECTIONS_FILE), "\n")

# ---- Helpers ----------------------------------------------------------------
# CELEX -> consolidated-version date. "32014R0833" is the original act.
celex_to_date <- function(x) {
  out <- as.Date(str_extract(x, "\\d{8}$"), format = "%Y%m%d")
  out[x == "32014R0833"] <- as.Date("2014-07-31")
  out
}

# 15th-day effective-month rule.
effective_month <- function(d) {
  if_else(day(d) <= 14, floor_date(d, "month"), floor_date(d %m+% months(1), "month"))
}

norm_annex <- function(s) str_squish(str_to_upper(s))

# ---- Read inputs -------------------------------------------------------------
step2 <- read_csv(STEP2_FILE,      show_col_types = FALSE) %>% mutate(across(everything(), as.character))
dirs  <- read_csv(DIRECTIONS_FILE, show_col_types = FALSE) %>% mutate(across(everything(), as.character))
dual  <- read_csv(DUALUSE_FILE,    show_col_types = FALSE) %>% mutate(across(everything(), as.character))

step2 <- step2 %>%
  filter(!is.na(hs6), str_trim(hs6) != "") %>%
  mutate(annex_k = norm_annex(annex), version = as.Date(version))
dirs <- dirs %>%
  mutate(annex_k = norm_annex(annex),
         direction = str_to_upper(str_trim(direction)),
         vdate = celex_to_date(law_version))

# ---- GUARD: directions must cover every STEP2 version -----------------------
step2_versions <- sort(unique(step2$version))
dir_versions   <- sort(unique(dirs$vdate))
missing <- setdiff(as.character(step2_versions), as.character(dir_versions))
if (length(missing) > 0) {
  write_csv(tibble(missing_version = missing),
            file.path(OUTPUT_DIR, "RU-STEP3_MISSING_versions.csv"))
  stop("Directions file is missing ", length(missing),
       " STEP2 version(s) (see RU-STEP3_MISSING_versions.csv). ",
       "A missing version would inject phantom de-sanction cycles; fix before Step 3.")
}
cat("Version-coverage guard passed:", length(step2_versions), "versions, none missing.\n\n")

# ---- Month timeline + version-in-force --------------------------------------
versions_active <- tibble(version = step2_versions) %>%
  mutate(eff_month = effective_month(version)) %>%
  group_by(eff_month) %>%              # if two versions share an eff month, last wins
  summarise(version = max(version), .groups = "drop") %>%
  arrange(eff_month)

panel_start <- min(versions_active$eff_month) %m-% months(1)
panel_end   <- max(versions_active$eff_month)
all_months  <- tibble(month = seq(panel_start, panel_end, by = "month"))

# version in force at the start of each month = most recent eff_month <= month
months_with_version <- all_months %>%
  mutate(version_in_force = map_chr(month, function(m) {
    vv <- versions_active$version[versions_active$eff_month <= m]
    if (length(vv) == 0) NA_character_ else as.character(max(vv))
  }))
vif_lookup <- setNames(months_with_version$version_in_force,
                       as.character(months_with_version$month))
get_vif <- function(mv) unname(vif_lookup[as.character(mv)])

# ---- Per-HS6 helpers: CHPI, dual-use, Part C --------------------------------
chpi_hs6 <- step2 %>%
  group_by(hs6) %>%
  summarise(chpi = any(is_high_priority %in% c("TRUE", "True", "1")), .groups = "drop")
dualuse_hs6 <- unique(dual$hs6)
xxxviiia_hs6 <- step2 %>% filter(annex_k == "ANNEX XXXVIIIA") %>% distinct(hs6) %>% pull(hs6)
partC_hs6 <- setdiff(xxxviiia_hs6, PART_AB_DIAMONDS)

yn <- function(x) if_else(x %in% TRUE, "Y", "N")

# =============================================================================
# Build one track's wide cycle table
# =============================================================================
build_track <- function(track_name, dir_values, is_import) {
  
  # (version, hs6) treated for this track = hs6 sits under a track-tagged annex
  treated_vh <- dirs %>%
    filter(direction %in% dir_values) %>%
    distinct(vdate, annex_k) %>%
    inner_join(step2 %>% distinct(version, annex_k, hs6),
               by = c("vdate" = "version", "annex_k")) %>%
    distinct(version = as.character(vdate), hs6)   # char, to match version_in_force
  
  if (nrow(treated_vh) == 0) {
    cat(sprintf("  %-13s: 0 HS6 (no annex tagged this track)\n", track_name))
    return(invisible(NULL))
  }
  
  # Monthly ON panel: a month is ON for an hs6 if the version in force that
  # month is one in which the hs6 is treated.
  panel_T <- months_with_version %>%
    inner_join(treated_vh, by = c("version_in_force" = "version"),
               relationship = "many-to-many") %>%   # intended: months x codes per version
    distinct(hs6, month)
  
  # ---- ADDED 2026-06-15: lossless monthly long state (Stage-4 merge source) ---
  # Written straight from panel_T, BEFORE the two-episode collapse below, so it
  # captures every ON month — including the 3rd+ cycle of codes that the wide
  # table truncates (truncated_extra_cycles=Y). Sparse: only ON months. The
  # Stage-4 merge equi-joins this to trade on (hs6, direction, substr(period,1,7)).
  long_state <- panel_T %>%
    transmute(direction = track_name, hs6, month = format(month, "%Y-%m")) %>%
    arrange(hs6, month)
  write_csv(long_state,
            file.path(OUTPUT_DIR, sprintf("RU-STEP3_long_state_%s_hs6.csv", track_name)))
  
  # Episodes = maximal runs of consecutive ON months (gap of 1 month breaks).
  episodes <- panel_T %>%
    arrange(hs6, month) %>%
    group_by(hs6) %>%
    mutate(idx = year(month) * 12 + month(month),
           new_ep = row_number() == 1 | (idx - lag(idx)) != 1,
           ep = cumsum(new_ep)) %>%
    group_by(hs6, ep) %>%
    summarise(start = min(month), end = max(month), .groups = "drop")
  
  # n_transitions / truncation, from ALL episodes
  trans <- episodes %>%
    group_by(hs6) %>%
    summarise(n_episodes  = n(),
              n_on        = n(),
              n_off       = sum(end < panel_end),
              .groups = "drop") %>%
    mutate(n_transitions = n_on + n_off,
           truncated_extra_cycles = yn(n_episodes > 2))
  
  # First two episodes -> cycle columns
  cyc <- episodes %>%
    arrange(hs6, start) %>%
    group_by(hs6) %>%
    summarise(e1_start = nth(start, 1), e1_end = nth(end, 1),
              e2_start = nth(start, 2), e2_end = nth(end, 2),
              .groups = "drop") %>%
    mutate(
      # cycle 1
      first_sanc_month     = e1_start,
      first_de_sanc_month  = if_else(!is.na(e1_end) & e1_end < panel_end,
                                     e1_end %m+% months(1), as.Date(NA)),
      # cycle 2
      first_re_sanc_month  = e2_start,
      first_re_de_sanc_month = if_else(!is.na(e2_end) & e2_end < panel_end,
                                       e2_end %m+% months(1), as.Date(NA)),
      ever_sanctioned     = "Y",
      ever_de_sanctioned  = yn(!is.na(first_de_sanc_month) | !is.na(first_re_sanc_month))
    )
  
  fmt_m <- function(d) if_else(is.na(d), NA_character_, format(d, "%Y-%m"))
  
  out <- cyc %>%
    transmute(
      hs6,
      first_sanc_version       = get_vif(first_sanc_month),
      first_sanc_month         = fmt_m(first_sanc_month),
      first_de_sanc_version    = get_vif(first_de_sanc_month),
      first_de_sanc_month      = fmt_m(first_de_sanc_month),
      first_re_sanc_version    = get_vif(first_re_sanc_month),
      first_re_sanc_month      = fmt_m(first_re_sanc_month),
      first_re_de_sanc_version = get_vif(first_re_de_sanc_month),
      first_re_de_sanc_month   = fmt_m(first_re_de_sanc_month),
      ever_sanctioned, ever_de_sanctioned
    ) %>%
    left_join(trans %>% select(hs6, n_episodes, n_transitions, truncated_extra_cycles),
              by = "hs6") %>%
    left_join(chpi_hs6, by = "hs6") %>%
    mutate(chpi = yn(chpi),
           dual_use = yn(hs6 %in% dualuse_hs6)) %>%
    { if (is_import) mutate(., partC_indirect_pending = yn(hs6 %in% partC_hs6)) else . } %>%
    select(hs6, chpi, dual_use, ever_sanctioned, ever_de_sanctioned,
           first_sanc_version, first_sanc_month,
           first_de_sanc_version, first_de_sanc_month,
           first_re_sanc_version, first_re_sanc_month,
           first_re_de_sanc_version, first_re_de_sanc_month,
           n_episodes, n_transitions, truncated_extra_cycles,
           any_of("partC_indirect_pending")) %>%
    arrange(hs6)
  
  write_csv(out, file.path(OUTPUT_DIR, sprintf("RU-STEP3_%s_hs6.csv", track_name)))
  
  cat(sprintf("  %-13s: %5d HS6 | ever de-sanctioned %4d | >2 cycles %3d | long-state %6d on-rows\n",
              track_name, nrow(out),
              sum(out$ever_de_sanctioned == "Y"),
              sum(out$truncated_extra_cycles == "Y"),
              nrow(long_state)))
  
  # de-sanction events for the audit file
  de_events <- out %>%
    filter(ever_de_sanctioned == "Y") %>%
    transmute(track = track_name, hs6,
              de_sanc_version = first_de_sanc_version,
              de_sanc_month   = first_de_sanc_month,
              re_sanctioned   = yn(!is.na(first_re_sanc_month)),
              n_episodes)
  de_events
}

# ---- Run all tracks ----------------------------------------------------------
cat("STEP 3 (directional) — building per-track HS6 cycle tables\n")
cat("==========================================================\n")
cat(sprintf("Panel: %s to %s | INCLUDE_AUTH_WINDOW = %s\n\n",
            format(panel_start, "%Y-%m"), format(panel_end, "%Y-%m"),
            INCLUDE_AUTH_WINDOW))

all_de_events <- imap_dfr(TRACKS, function(dir_values, track_name) {
  build_track(track_name, dir_values, is_import = (track_name == "IMPORT"))
})

write_csv(all_de_events, file.path(OUTPUT_DIR, "RU-STEP3_de_sanction_events.csv"))

cat(sprintf("\nDe-sanction events (all tracks): %d -> RU-STEP3_de_sanction_events.csv\n",
            nrow(all_de_events)))
cat("   (review these — many are vintage-renumbering / annex-restructuring\n")
cat("    artefacts rather than real policy reversals)\n")
cat("\nOutputs written to:", normalizePath(OUTPUT_DIR), "\n")