# =============================================================================
# STEP 1 — Eliminate ex-redundancy within each annex of each version  [RU]
#
# Logic: if cn_code "exXXXX..." exists in version V, annex A and "XXXX..."
# (the non-ex twin at the same digit level) also exists in the SAME annex A
# of the same version, drop the ex row. The non-ex row already covers
# everything the ex row would within that annex, so removing it is lossless.
#
# The check is WITHIN-ANNEX, not cross-annex. Two different annexes can
# carry different trade directions (export ban vs anti-circumvention),
# so an ex+full coexistence across annexes is a real signal — not a
# drafting redundancy — and must be preserved.
#
# Outputs (all written to OUTPUT_DIR):
#   1. RU-CN_extraction_YYYY-MM-DD_clean.csv  one cleaned file per version
#   2. RU-STEP1_combined_clean.csv            all versions stacked, with
#                                             an extra `version` column
#   3. RU-STEP1_summary.csv                   per-version row counts in/out
#   4. RU-STEP1_dropped_audit.csv             every row that was removed,
#                                             with the twin and annex it
#                                             matched in
# =============================================================================

library(dplyr)
library(readr)
library(stringr)
library(purrr)

# ---- Configuration ----------------------------------------------------------
# BASE_DIR holds both the parser output folder (timestamped) and the
# RU-Output_R_Step1/ folder this script writes into.
BASE_DIR <- "data/RU-A_recent_run"

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

INPUT_DIR  <- latest_dir(BASE_DIR, "^RU-CN_results_")   # newest run wins
OUTPUT_DIR <- file.path(BASE_DIR, paste0("RU-Output_R_Step1_", run_timestamp))
cat("Using INPUT_DIR  =", INPUT_DIR,  "\n")
cat("Writing OUTPUT_DIR =", OUTPUT_DIR, "\n\n")

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ---- Helpers ----------------------------------------------------------------
strip_ex <- function(code) sub("^ex", "", code)

read_version <- function(file) {
  version_date <- str_extract(basename(file), "\\d{4}-\\d{2}-\\d{2}")
  read_csv(file, show_col_types = FALSE) %>%
    mutate(version       = version_date,
           is_ex         = str_starts(cn_code, "ex"),
           code_stripped = strip_ex(cn_code))
}

# ---- Read every CN_extraction_*.csv -----------------------------------------
files <- list.files(
  INPUT_DIR,
  pattern    = "^RU-CN_extraction_\\d{4}-\\d{2}-\\d{2}\\.csv$",
  full.names = TRUE
)
stopifnot(length(files) > 0)
cat("Found", length(files), "version files.\n\n")

all_rows <- map_dfr(files, read_version)

# ---- Flag redundant ex rows -------------------------------------------------
# An ex row is redundant in (version V, annex A) if THE SAME ANNEX of THE
# SAME VERSION contains a row whose cn_code equals the ex row's stripped code.
#
# WITHIN-ANNEX check (group_by(version, annex)): cross-annex ex+full
# coexistence is preserved because different annexes can carry different
# trade directions (export ban vs anti-circumvention list) and collapsing
# them is lossy. See the header comment for full rationale.
all_rows <- all_rows %>%
  group_by(version, annex) %>%
  mutate(
    has_non_ex_twin = code_stripped %in% cn_code[!is_ex],
    redundant_ex    = is_ex & has_non_ex_twin
  ) %>%
  ungroup()

# ---- Build outputs ----------------------------------------------------------
internal_cols <- c("is_ex", "code_stripped", "has_non_ex_twin", "redundant_ex")

# (1) per-version cleaned files
kept <- all_rows %>% filter(!redundant_ex)
walk(unique(kept$version), function(v) {
  out <- kept %>%
    filter(version == v) %>%
    select(-version, -all_of(internal_cols))
  write_csv(out, file.path(OUTPUT_DIR, sprintf("RU-CN_extraction_%s_clean.csv", v)))
})

# (2) combined long file (keep version column)
write_csv(
  kept %>% select(-all_of(internal_cols)),
  file.path(OUTPUT_DIR, "RU-STEP1_combined_clean.csv")
)

# (3) per-version summary
summary_step1 <- all_rows %>%
  group_by(version) %>%
  summarise(
    n_rows_in      = n(),
    n_ex_rows      = sum(is_ex),
    n_redundant_ex = sum(redundant_ex),
    n_rows_out     = sum(!redundant_ex),
    .groups = "drop"
  ) %>%
  arrange(version)

write_csv(summary_step1, file.path(OUTPUT_DIR, "RU-STEP1_summary.csv"))

# (4) audit log of dropped rows, with the twin they matched
dropped_audit <- all_rows %>%
  filter(redundant_ex) %>%
  mutate(matched_twin = code_stripped) %>%
  select(version, annex, cn_code, cn_type, matched_twin, description)

write_csv(dropped_audit, file.path(OUTPUT_DIR, "RU-STEP1_dropped_audit.csv"))

# ---- Console report ---------------------------------------------------------
cat("STEP 1 SUMMARY\n")
cat("==============\n")
print(summary_step1, n = Inf)
cat("\n")
cat("Total rows in :", nrow(all_rows), "\n")
cat("Total dropped :", nrow(dropped_audit), "(redundant ex)\n")
cat("Total rows out:", nrow(kept), "\n\n")
cat("Outputs written to:", normalizePath(OUTPUT_DIR), "\n")