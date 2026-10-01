# =============================================================================
# STEP 2 (v3) — Roll up to HS6, with proper catch-all handling   [RU]
#
# Resolution strategy for each input row:
#
#   HS2 chapter heading           → expand to all HS6 children of chapter
#                                    (e.g. "88" → all HS6 of chapter 88)
#
#   HS4 with real children        → expand to all HS6 children of heading
#   HS4 catch-all of chapter      → expand to all HS6 of chapter (e.g. ex9700 = ex97)
#   HS4 with no resolution        → keep as orphan (flagged unmatched)
#
#   HS6 in reference              → keep as is
#   HS6 catch-all of heading      → expand to all HS6 children of heading
#   HS6 not in ref, not catch-all → keep as orphan (e.g. deprecated 4412 94)
#
#   HS8 rollup in reference       → roll up to first 6, flag improperly_uplevelled
#   HS8 catch-all of heading      → expand to all HS6 children of heading
#   HS8 rollup not in ref, not catch-all → keep as orphan
#
#   HS10 same logic as HS8
#
# A row is a "catch-all" if its trailing positions below the heading are
# all zeros AND the literal rolled-up HS6 doesn't exist in the reference.
#
# Outputs (all RU- prefixed, written to OUTPUT_DIR):
#   RU-STEP2_hs6_long.csv               long format, all versions
#   RU-STEP2_hs6_summary.csv            per-version statistics
#   RU-STEP2_unmatched.csv              orphan rows (no HS6 resolution)
#   RU-STEP2_hs6_per_version/
#     RU-hs6_YYYY-MM-DD.csv             same data, split per version
#   RU-STEP2_LOST_rows.csv              [guard] any input rows that
#                                       failed the conservation check
#                                       (should never appear; empty = OK)
#
# 2026-06-10: added a conservation guard after the strategy bind_rows
#             (stops the run if any input row is silently dropped).
# =============================================================================

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)

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

INPUT_FILE <- file.path(latest_dir(BASE_DIR, "^RU-Output_R_Step1($|_)"), "RU-STEP1_combined_clean.csv")
REF_FILE   <- file.path(REF_DIR,  "GENERAL-6-digits_codes_2012_2017_2022.csv")
HP_FILE    <- file.path(REF_DIR,  "GENERAL-CN_common_high_priority_items.csv")
OUTPUT_DIR <- file.path(BASE_DIR, paste0("RU-Output_R_Step2_", run_timestamp))

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(OUTPUT_DIR, "RU-STEP2_hs6_per_version"),
           showWarnings = FALSE, recursive = TRUE)

# ---- Read inputs ------------------------------------------------------------
df_in <- read_csv(INPUT_FILE, show_col_types = FALSE)
ref   <- read_csv(REF_FILE,   show_col_types = FALSE)
hp    <- read_csv(HP_FILE,    show_col_types = FALSE)

# ---- Reference tables -------------------------------------------------------
ref_long <- bind_rows(
  tibble(vintage = "Y2012", hs6 = unique(na.omit(ref$Y2012))),
  tibble(vintage = "Y2017", hs6 = unique(na.omit(ref$Y2017))),
  tibble(vintage = "Y2022", hs6 = unique(na.omit(ref$Y2022)))
) %>% distinct()

# Headings (first 4 digits) and chapters (first 2 digits) that exist per vintage
heading_exists <- ref_long %>%
  mutate(heading4 = substr(hs6, 1, 4)) %>%
  distinct(vintage, heading4) %>%
  mutate(heading_has_children = TRUE)

chapter_exists <- ref_long %>%
  mutate(chapter2 = substr(hs6, 1, 2)) %>%
  distinct(vintage, chapter2) %>%
  mutate(chapter_has_children = TRUE)

hp_lookup <- hp %>%
  transmute(hs6 = gsub(" ", "", cn_code),
            hp_tier = tier,
            hp_description = description) %>%
  distinct(hs6, .keep_all = TRUE)

# ---- Helpers ----------------------------------------------------------------
# Full-date string comparison (not year-int): byte-equivalent to the upstream
# Python pipeline's pick_vintage() and robust to hypothetical Jan-of-cliff-year
# amendments. Matches the form used in BY-Harmonisation_step_2.R.
vintage_for_v <- function(dates) {
  case_when(dates < "2017-01-01" ~ "Y2012",
            dates < "2022-01-01" ~ "Y2017",
            TRUE                 ~ "Y2022")
}

# ---- Normalise input and pre-compute lookups --------------------------------
df <- df_in %>%
  mutate(
    is_ex       = str_starts(cn_code, "ex"),
    code_clean  = sub("^ex", "", cn_code),
    code_digits = gsub(" ", "", code_clean),
    n_digits    = nchar(code_digits),
    vintage     = vintage_for_v(version),
    heading4    = substr(code_digits, 1, 4),
    chapter2    = substr(code_digits, 1, 2),
    rollup_hs6  = if_else(n_digits >= 6, substr(code_digits, 1, 6), NA_character_),
    
    # Trailing-zero structure
    looks_catch_all_heading = case_when(
      n_digits == 6  & substr(code_digits, 5, 6)  == "00"     ~ TRUE,
      n_digits == 8  & substr(code_digits, 5, 8)  == "0000"   ~ TRUE,
      n_digits == 10 & substr(code_digits, 5, 10) == "000000" ~ TRUE,
      TRUE                                                     ~ FALSE
    ),
    looks_catch_all_chapter = (n_digits == 4 & substr(code_digits, 3, 4) == "00")
  )

stopifnot(all(df$n_digits %in% c(2, 4, 6, 8, 10)))

# Is the rolled-up HS6 a real code in the right vintage?
df <- df %>%
  left_join(ref_long %>% rename(rollup_hs6 = hs6) %>% mutate(rollup_in_ref = TRUE),
            by = c("vintage", "rollup_hs6")) %>%
  mutate(rollup_in_ref = coalesce(rollup_in_ref, FALSE))

# Does the heading have any HS6 children in the right vintage?
df <- df %>%
  left_join(heading_exists, by = c("vintage", "heading4")) %>%
  mutate(heading_has_children = coalesce(heading_has_children, FALSE))

# Does the chapter have any HS6 children in the right vintage?
df <- df %>%
  left_join(chapter_exists, by = c("vintage", "chapter2")) %>%
  mutate(chapter_has_children = coalesce(chapter_has_children, FALSE))

# ---- Decide resolution strategy ---------------------------------------------
df <- df %>%
  mutate(
    strategy = case_when(
      # HS2: expand to all HS6 in chapter (same machinery as expand_chapter,
      # triggered by a 2-digit input rather than an HS4 catch-all).
      # Russia's 24 April 2026 amendment introduced "88" (Aircraft) in Annex XI.
      n_digits == 2 & chapter_has_children ~ "expand_chapter",
      
      # HS6 / HS8 / HS10 with valid rollup → normal handling
      n_digits == 6  & rollup_in_ref ~ "as_is",
      n_digits == 8  & rollup_in_ref ~ "rollup_hs8",
      n_digits == 10 & rollup_in_ref ~ "rollup_hs10",
      
      # HS6 / HS8 / HS10 catch-all of heading
      n_digits >= 6 & !rollup_in_ref &
        looks_catch_all_heading & heading_has_children ~ "catchall_heading",
      
      # HS4 with real children
      n_digits == 4 & heading_has_children ~ "expand_hs4",
      
      # HS4 catch-all of chapter (e.g. ex9700 → ex97)
      n_digits == 4 & !heading_has_children &
        looks_catch_all_chapter & chapter_has_children ~ "expand_chapter",
      
      # Anything else: orphan (kept in output, flagged unmatched)
      TRUE ~ "orphan"
    )
  )

# ---- Apply each strategy ----------------------------------------------------
ref_children_heading <- ref_long %>% mutate(heading4 = substr(hs6, 1, 4))
ref_children_chapter <- ref_long %>% mutate(chapter2 = substr(hs6, 1, 2))

df_asis <- df %>% filter(strategy == "as_is") %>%
  mutate(hs6 = rollup_hs6, hs6_source = "as_is",
         improperly_uplevelled = FALSE, is_catch_all = FALSE)

df_rollup8 <- df %>% filter(strategy == "rollup_hs8") %>%
  mutate(hs6 = rollup_hs6, hs6_source = "rolled_up_from_hs8",
         improperly_uplevelled = TRUE, is_catch_all = FALSE)

df_rollup10 <- df %>% filter(strategy == "rollup_hs10") %>%
  mutate(hs6 = rollup_hs6, hs6_source = "rolled_up_from_hs10",
         improperly_uplevelled = TRUE, is_catch_all = FALSE)

df_expand_hs4 <- df %>% filter(strategy == "expand_hs4") %>%
  select(-rollup_hs6) %>%
  inner_join(ref_children_heading, by = c("vintage", "heading4"),
             relationship = "many-to-many") %>%
  mutate(hs6_source = "expanded_from_hs4",
         improperly_uplevelled = FALSE, is_catch_all = FALSE)

df_expand_chapter <- df %>% filter(strategy == "expand_chapter") %>%
  select(-rollup_hs6) %>%
  inner_join(ref_children_chapter, by = c("vintage", "chapter2"),
             relationship = "many-to-many") %>%
  mutate(hs6_source = if_else(n_digits == 2,
                              "expanded_from_hs2_chapter_level",
                              "expanded_from_hs4_chapter_level"),
         improperly_uplevelled = FALSE, is_catch_all = TRUE)


# ---- Ex carve-out: drop children explicitly excepted in the description -----
# An "ex"-chapter such as "Copper and articles thereof, except CN code 7401 00
# 00" is expanded (above) to ALL of chapter 74; the excepted code 7401 00 00
# (-> HS6 740100) must then be removed. Handles one or several excepted codes
# ("... except CN codes 7401 00 00 and 7404 00 00"). Each excepted CN code is
# reduced to a <=6-digit prefix; any expanded child HS6 matching a prefix from
# its OWN row's description is dropped. Rows with no "except" clause are left
# untouched. Base R only -- no extra package dependency.
excepted_prefixes <- function(desc) {
  if (is.na(desc) || !grepl("except", desc, ignore.case = TRUE)) return(character(0))
  rest <- sub("^.*?except[a-z]*\\s+(of\\s+)?CN\\s+codes?\\s*", "", desc,
              ignore.case = TRUE, perl = TRUE)
  m <- regmatches(rest, gregexpr("\\d{4}(?:\\s?\\d{2}){0,2}", rest, perl = TRUE))[[1]]
  if (length(m) == 0) return(character(0))
  unique(substr(gsub("\\s", "", m), 1, 6))
}

if (nrow(df_expand_chapter) > 0) {
  desc_u  <- unique(df_expand_chapter$description)
  pfx_map <- stats::setNames(lapply(desc_u, excepted_prefixes), desc_u)
  drop_flag <- mapply(function(d, h) {
                 if (is.na(d)) return(FALSE)
                 pfx <- pfx_map[[d]]
                 length(pfx) > 0 && any(startsWith(h, pfx))
               }, df_expand_chapter$description, df_expand_chapter$hs6,
               USE.NAMES = FALSE)
  cat(sprintf("Ex carve-out: removed %d excepted child code(s) from chapter expansions.\n",
              sum(drop_flag)))
  df_expand_chapter <- df_expand_chapter[!drop_flag, , drop = FALSE]
}

df_catchall <- df %>% filter(strategy == "catchall_heading") %>%
  select(-rollup_hs6) %>%
  inner_join(ref_children_heading, by = c("vintage", "heading4"),
             relationship = "many-to-many") %>%
  mutate(hs6_source = paste0("catch_all_heading_from_hs", n_digits),
         improperly_uplevelled = (n_digits >= 8),
         is_catch_all = TRUE)

df_orphan <- df %>% filter(strategy == "orphan") %>%
  mutate(hs6 = rollup_hs6,
         hs6_source = "orphan_unresolved",
         improperly_uplevelled = FALSE, is_catch_all = FALSE)

df_step2 <- bind_rows(df_asis, df_rollup8, df_rollup10,
                      df_expand_hs4, df_expand_chapter,
                      df_catchall, df_orphan)

# ---- Conservation guard: no input row may be silently dropped ---------------
# By construction the rollup is loss-free: every input row is assigned exactly
# one strategy (the case_when above ends in `TRUE ~ "orphan"`), and every
# strategy frame is bound back into df_step2 just above. The expand_/catchall_
# strategies use inner_join, which COULD drop a left row if its heading/chapter
# had no children in the reference -- but each is gated on heading_has_children
# / chapter_has_children, derived from the SAME ref_long as the join tables, so
# a match is guaranteed and nothing drops today.
#
# This block turns that 'by construction' promise into a CHECKED invariant: if
# a future reference-table swap or a new code shape ever breaks it, the script
# STOPS here instead of quietly writing an under-counted file. Keys are compared
# at the (version, annex, cn_code, description) level -- one key per input row,
# and cn_code is still named cn_code here (it is renamed to cn_code_original
# only in df_out below).
input_keys  <- df       %>% distinct(version, annex, cn_code, description)
output_keys <- df_step2 %>% distinct(version, annex, cn_code, description)
lost_rows   <- anti_join(input_keys, output_keys,
                         by = c("version", "annex", "cn_code", "description"))

# If anything was dropped, write the offending rows for inspection, then stop.
if (nrow(lost_rows) > 0) {
  write_csv(lost_rows, file.path(OUTPUT_DIR, "RU-STEP2_LOST_rows.csv"))
  cat("\n*** CONSERVATION CHECK FAILED ***\n")
  cat(nrow(lost_rows), "input row(s) did not survive the rollup.\n")
  cat("Wrote RU-STEP2_LOST_rows.csv for inspection.\n")
}
stopifnot(
  "Step 2 conservation check failed: input rows were dropped (see RU-STEP2_LOST_rows.csv)" =
    nrow(lost_rows) == 0
)
cat("Conservation check passed: all", nrow(input_keys),
    "distinct input rows survived the rollup.\n")

# ---- Final reference and HP flags -------------------------------------------
df_step2 <- df_step2 %>%
  left_join(ref_long %>% mutate(hs6_in_reference = TRUE),
            by = c("vintage", "hs6")) %>%
  mutate(hs6_in_reference = !is.na(hs6_in_reference) & !is.na(hs6)) %>%
  left_join(hp_lookup, by = "hs6") %>%
  mutate(is_high_priority = !is.na(hp_tier),
         ex_approximation = is_ex)

df_out <- df_step2 %>%
  select(version, annex,
         cn_code_original     = cn_code,
         cn_type_original     = cn_type,
         hs6, hs6_source,
         is_catch_all, improperly_uplevelled, ex_approximation,
         vintage, hs6_in_reference,
         is_high_priority, hp_tier, hp_description,
         description) %>%
  arrange(version, annex, cn_code_original, hs6)

# ---- Outputs ----------------------------------------------------------------
write_csv(df_out, file.path(OUTPUT_DIR, "RU-STEP2_hs6_long.csv"))

walk(unique(df_out$version), function(v) {
  out <- df_out %>% filter(version == v) %>% select(-version)
  write_csv(out, file.path(OUTPUT_DIR, "RU-STEP2_hs6_per_version",
                           sprintf("RU-hs6_%s.csv", v)))
})

summary_step2 <- df_out %>%
  group_by(version, vintage) %>%
  summarise(
    n_input_raw_rows       = n_distinct(paste(annex, cn_code_original, description)),
    n_output_rows          = n(),
    n_uplevelled           = sum(improperly_uplevelled),
    n_ex_approximation     = sum(ex_approximation),
    n_catch_all_rows       = sum(is_catch_all),
    n_orphan_rows          = sum(hs6_source == "orphan_unresolved"),
    n_unmatched_in_ref     = sum(!hs6_in_reference),
    n_high_priority_rows   = sum(is_high_priority),
    n_unique_hs6           = n_distinct(hs6, na.rm = TRUE),
    n_unique_hs6_high_prio = n_distinct(hs6[is_high_priority], na.rm = TRUE),
    .groups = "drop"
  ) %>% arrange(version)

write_csv(summary_step2, file.path(OUTPUT_DIR, "RU-STEP2_hs6_summary.csv"))

unmatched <- df_out %>% filter(!hs6_in_reference)
write_csv(unmatched, file.path(OUTPUT_DIR, "RU-STEP2_unmatched.csv"))

# ---- Console report ---------------------------------------------------------
cat("STEP 2 (v3) SUMMARY\n")
cat("===================\n")
print(summary_step2, n = Inf)
cat("\nStrategy breakdown:\n")
print(df_out %>% count(hs6_source))
cat("\nTotal output rows :", nrow(df_out), "\n")
cat("  catch-all rows  :", sum(df_out$is_catch_all), "\n")
cat("  uplevelled      :", sum(df_out$improperly_uplevelled), "\n")
cat("  ex approximation:", sum(df_out$ex_approximation), "\n")
cat("  high priority   :", sum(df_out$is_high_priority), "\n")
cat("  unmatched in ref:", nrow(unmatched), "\n")
cat("\nOutputs written to:", normalizePath(OUTPUT_DIR), "\n")