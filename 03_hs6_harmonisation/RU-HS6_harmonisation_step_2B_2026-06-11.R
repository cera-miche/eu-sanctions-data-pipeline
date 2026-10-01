# =============================================================================
# STEP 2B — Dual-use HS6 lookup from the EU Annex VII correlation table   [RU]
#
# Standalone side-step (does NOT renumber Steps 1/2/3). It turns the European
# Commission's correlation table — "Correlation table between the goods of
# Annex VII to Reg. 833/2014 and CN codes" — into a flat HS6 dual-use lookup
# that Step 3 joins on to fill the "Dual Use [Y/N]" column.
#
# Source file: faqs-sanctions-russia-correlation-table-goods-regulation-833_en
#              (.xlsx, sheet "Annex VII"). Columns used (by POSITION, because
#              the header cell for the CN column is the awkward "2026\n CN Code"):
#                1 Category   2 EU code   3 Control text
#                4 CN Code    5 CN text   6 Date
#
# Rules (mirror of the verified Python extraction, 2026-06-10):
#   - A CN cell may hold several codes on separate lines  -> split first.
#   - Strip non-digits. >=6 digits -> hs6 = first 6 (kept).
#                       ==4 digits -> HS4-level only, NOT expanded -> skipped log.
#                       else (N/A, blanks, section rows) -> skipped log.
#   - Date column is MIXED: most cells are real Excel dates, ~54 are text like
#     "06-Oct-22"/"23-Jun-23" for the SAME dates that appear ISO elsewhere.
#     Reading as text returns Excel serials for the date cells, so we coerce:
#     all-digit -> serial (origin 1899-12-30); otherwise parse dmy/ymd.
#   - dualuse_first_date per hs6 = the EARLIEST date across its source codes.
#     (Used in Step 3 as a sanity cross-check against the EXPORT first-sanctioned
#      month — Annex VII dual-use goods are export-controlled, so the two should
#      sit close; a large divergence is worth a look, not an error.)
#
# Outputs (written to OUTPUT_DIR):
#   RU-STEP2B_dualuse_hs6.csv   hs6, is_dual_use, dualuse_first_date,
#                               n_source_codes, example_eu_code, example_category
#   RU-STEP2B_skipped.csv       every token NOT turned into an hs6, with reason
#                               (HS4_only_not_expanded / no_usable_digits) so
#                               nothing is silently dropped
#
# =============================================================================

library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(tidyr)
library(purrr)
library(lubridate)

# ---- Configuration ----------------------------------------------------------
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
REF_DIR  <- "data/RU-AAA-Files_for_a_rerun/RU-3-R_HS6_harmonisation"
OUTPUT_DIR <- file.path(BASE_DIR, paste0("RU-Output_R_Step2B_", run_timestamp))
SHEET <- "Annex VII"

# Auto-detect the correlation file (robust to the "__5_" suffix and to future
# revisions): the one .xlsx in REF_DIR whose name carries the tell-tale string.
CORR_PATTERN <- "correlation-table-goods-regulation-833"
cands <- list.files(REF_DIR, pattern = "\\.xlsx$", full.names = TRUE)
cands <- cands[grepl(CORR_PATTERN, basename(cands), ignore.case = TRUE)]
if (length(cands) == 0) stop("No correlation .xlsx matching '", CORR_PATTERN, "' in ", REF_DIR)
if (length(cands) > 1)  stop("More than one correlation .xlsx in ", REF_DIR, ":\n  ",
                             paste(basename(cands), collapse = "\n  "),
                             "\nKeep only the version you want.")
CORR_FILE <- cands[1]
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
cat("Using correlation file:", basename(CORR_FILE), "\n\n")

# ---- Read (everything as text so date cells don't get type-guessed away) ----
raw <- read_excel(CORR_FILE, sheet = SHEET, col_types = "text")

# Keep the first 6 columns by position and give them stable names.
stopifnot(ncol(raw) >= 6)
dat <- raw[, 1:6]
names(dat) <- c("category", "eu_code", "control_text", "cn_code", "cn_text", "date")

# ---- Date coercion: handle Excel serials AND dmy/ymd text -------------------
coerce_date <- function(x) {
  x <- str_trim(as.character(x))
  out <- as.Date(rep(NA, length(x)))
  is_serial <- !is.na(x) & str_detect(x, "^[0-9]+$")
  # Excel origin 1899-12-30 (accounts for the 1900 leap-year bug).
  out[is_serial] <- as.Date(as.numeric(x[is_serial]), origin = "1899-12-30")
  txt <- !is_serial & !is.na(x) & x != ""
  # dmy BEFORE ymd: the only text dates here are dd-Mon-yy ("06-Oct-22");
  # ymd-first would misread that as 2006-10-22. (ISO date cells arrive as
  # Excel serials, handled above, so they never reach this branch — but ymd
  # is kept as a fallback in case a future file stores an ISO date as text.)
  out[txt] <- as.Date(parse_date_time(x[txt], orders = c("dmy", "ymd"), quiet = TRUE))
  out
}

# ---- Split multi-code cells, classify each token ----------------------------
tokens <- dat %>%
  filter(!is.na(cn_code) & str_trim(cn_code) != "") %>%
  mutate(date_parsed = coerce_date(date)) %>%
  separate_rows(cn_code, sep = "[\n;,]+") %>%
  mutate(
    token  = str_trim(cn_code),
    digits = str_remove_all(token, "\\D"),
    nd     = nchar(digits),
    hs6    = if_else(nd >= 6, substr(digits, 1, 6), NA_character_),
    reason = case_when(nd >= 6 ~ "ok",
                       nd == 4 ~ "HS4_only_not_expanded",
                       TRUE    ~ "no_usable_digits")
  ) %>%
  filter(token != "")

# ---- Build the dual-use HS6 lookup ------------------------------------------
dualuse <- tokens %>%
  filter(reason == "ok") %>%
  group_by(hs6) %>%
  summarise(
    is_dual_use        = TRUE,
    dualuse_first_date = suppressWarnings(min(date_parsed, na.rm = TRUE)),
    n_source_codes     = n(),
    example_eu_code    = first(eu_code),
    example_category   = first(category),
    .groups = "drop"
  ) %>%
  mutate(dualuse_first_date = if_else(is.infinite(as.numeric(dualuse_first_date)),
                                      as.Date(NA), dualuse_first_date)) %>%
  arrange(hs6)

skipped <- tokens %>%
  filter(reason != "ok") %>%
  transmute(token, digits, reason,
            date = date_parsed, eu_code, category)

# ---- Write ------------------------------------------------------------------
write_csv(dualuse, file.path(OUTPUT_DIR, "RU-STEP2B_dualuse_hs6.csv"))
write_csv(skipped, file.path(OUTPUT_DIR, "RU-STEP2B_skipped.csv"))

# ---- Console report ---------------------------------------------------------
cat("STEP 2B SUMMARY\n")
cat("===============\n")
cat("distinct dual-use HS6 :", nrow(dualuse), "\n")
cat("codes with no date    :", sum(is.na(dualuse$dualuse_first_date)), "\n")
cat("skipped tokens        :", nrow(skipped),
    " (HS4-only:", sum(skipped$reason == "HS4_only_not_expanded"),
    ", no-digits:", sum(skipped$reason == "no_usable_digits"), ")\n\n")
cat("dual-use HS6 by first-appearance date:\n")
print(dualuse %>% count(dualuse_first_date) %>% arrange(dualuse_first_date), n = Inf)
cat("\nOutputs written to:", normalizePath(OUTPUT_DIR), "\n")
