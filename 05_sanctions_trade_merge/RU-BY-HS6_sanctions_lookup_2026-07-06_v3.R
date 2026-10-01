# RU-BY-HS6_sanctions_lookup_2026-07-06_v3.R
# Two per-HS6 lookups on the FULL universe of codes, one per direction:
#   RU-BY-HS6_sanctions_lookup_EXPORT_2026-07-06_v3.csv
#   RU-BY-HS6_sanctions_lookup_IMPORT_2026-07-06_v3.csv
# Columns for each country C in {RU, BY}:
#   C_ever_sanctioned, C_ever_desanctioned                (0/1)
#   C_first_sanctioned,  C_first_desanctioned             ('YYYY-MM')
#   C_second_sanctioned, C_second_desanctioned
#   C_third_sanctioned,  C_third_desanctioned
# + chpi (0/1), dual_use (0/1), in_Y2002..in_Y2022 (0/1), ever_any (0/1).
# Episode convention: sanctioned = first month ON; desanctioned = first
# month OFF after the episode; NA if the episode runs to the end of the panel.
# Source: the long-state panels of Step 3 (lossless). Never overwrites.

library(dplyr); library(readr); library(tidyr); library(purrr); library(stringr)

RU  <- "data/RU-A_recent_run/RU-Output_R_Step3_2026-06-15_19-16-35"
BY  <- "data/BY-A_recent_run/BY-Output_R_Step3_2026-07-06_16-46-31"
REF <- "data/GENERAL-6-digits_codes_2002_2007_2012_2017_2022.csv"
# CHPI: first existing path among the candidates (edit if needed)
CHPI_CANDS <- c(
  "data/GENERAL-CN_common_high_priority_items.csv",
  "data/BY-AAA-Files_for_a_rerun/GENERAL-CN_common_high_priority_items.csv")
CHPI_FILE <- CHPI_CANDS[file.exists(CHPI_CANDS)][1]
# dual-use: RU Step 2B (list of Reg. 2021/821, country-agnostic)
latest_dir <- function(base, prefix) {
  d <- list.dirs(base, recursive = FALSE); d <- d[grepl(prefix, basename(d))]
  if (!length(d)) return(NA_character_); d[which.max(file.mtime(d))]
}
DU_DIR  <- latest_dir("data/RU-A_recent_run", "^RU-Output_R_Step2B($|_)")
DU_FILE <- if (!is.na(DU_DIR)) file.path(DU_DIR, "RU-STEP2B_dualuse_hs6.csv") else NA_character_

OUT_X <- "output/RU-BY-HS6_sanctions_lookup_EXPORT_2026-07-06_v3.csv"
OUT_M <- "output/RU-BY-HS6_sanctions_lookup_IMPORT_2026-07-06_v3.csv"
stopifnot(file.exists(REF), !is.na(CHPI_FILE), !file.exists(OUT_X), !file.exists(OUT_M))
dir.create("output", showWarnings = FALSE)

pad6 <- function(x) str_pad(str_remove_all(x, "\\D"), 6, pad = "0")

## 1. UNIVERSE: 0/1 columns by vintage -----------------------------------------
ref   <- read_csv(REF, col_types = cols(.default = "c"))
vcols <- grep("^Y\\d{4}$", names(ref), value = TRUE)
cat("Vintages in the reference:", paste(vcols, collapse = ", "), "\n")
universe <- ref |> select(all_of(vcols)) |>
  pivot_longer(everything(), names_to = "vintage", values_to = "hs6") |>
  filter(!is.na(hs6), hs6 != "") |>
  mutate(hs6 = pad6(hs6), flag = 1L) |>
  distinct(hs6, vintage, flag) |>
  pivot_wider(names_from = vintage, values_from = flag,
              values_fill = 0L, names_prefix = "in_") |>
  arrange(hs6)
cat("HS6 universe:", nrow(universe), "\n")

## 2. CHPI AND DUAL-USE ---------------------------------------------------------
chpi_raw <- read_csv(CHPI_FILE, col_types = cols(.default = "c"))
chpi_col <- names(chpi_raw)[str_detect(tolower(names(chpi_raw)), "hs|cn|code")][1]
if (is.na(chpi_col)) chpi_col <- names(chpi_raw)[1]
chpi_set <- unique(pad6(chpi_raw[[chpi_col]]))
cat("CHPI: column '", chpi_col, "', ", length(chpi_set), " codes (50 expected)\n", sep = "")

du_set <- if (!is.na(DU_FILE) && file.exists(DU_FILE)) {
  du <- read_csv(DU_FILE, col_types = cols(.default = "c"))
  unique(pad6(du$hs6))
} else { cat("NOTE: RU-STEP2B_dualuse_hs6.csv not found -> dual_use = 0 everywhere\n"); character(0) }
cat("Dual-use:", length(du_set), "codes\n")

## 3. EPISODES from the long-state -------------------------------------------------
mkey   <- function(m) as.integer(substr(m,1,4))*12L + as.integer(substr(m,6,7))
munkey <- function(k) sprintf("%04d-%02d", (k-1L) %/% 12L, (k-1L) %% 12L + 1L)

episodes_tab <- function(dir_path, ctry, trk) {
  f <- file.path(dir_path, sprintf("%s-STEP3_long_state_%s_hs6.csv", ctry, trk))
  stopifnot(file.exists(f))
  long <- read_csv(f, col_types = cols(.default = "c"))
  pend <- max(mkey(long$month))                       # end of the panel for THIS track
  epi <- long |> mutate(k = mkey(month)) |> arrange(hs6, k) |>
    group_by(hs6) |>
    mutate(ep = cumsum(c(1L, diff(k) != 1L))) |>
    group_by(hs6, ep) |>
    summarise(sanc   = munkey(min(k)),
              desanc = if_else(max(k) == pend, NA_character_, munkey(max(k)+1L)),
              .groups = "drop")
  stopifnot(max(epi$ep) <= 3L)
  ord <- c("first","second","third")
  wide <- epi |> mutate(lbl = ord[ep]) |>
    select(hs6, lbl, sanc, desanc) |>
    pivot_wider(names_from = lbl, values_from = c(sanc, desanc),
                names_glue = "{lbl}_{.value}")
  # missing columns (e.g. never a 2nd/3rd episode) -> NA, stable schema
  need <- as.vector(outer(ord, c("sanc","desanc"), function(a,b) paste0(a,"_",b)))
  for (cc in setdiff(need, names(wide))) wide[[cc]] <- NA_character_
  wide |>
    transmute(hs6,
              ever_sanctioned    = 1L,
              ever_desanctioned  = as.integer(!is.na(first_desanc) |
                                              !is.na(second_desanc) |
                                              !is.na(third_desanc)),
              first_sanctioned    = first_sanc,   first_desanctioned  = first_desanc,
              second_sanctioned   = second_sanc,  second_desanctioned = second_desanc,
              third_sanctioned    = third_sanc,   third_desanctioned  = third_desanc) |>
    rename_with(~ paste0(ctry, "_", .x), -hs6)
}

## 4. BUILD AND SAVE, per direction ------------------------------------------------
build_dir <- function(trk, out_file) {
  tab <- universe |>
    full_join(episodes_tab(RU, "RU", trk), by = "hs6") |>
    full_join(episodes_tab(BY, "BY", trk), by = "hs6") |>
    mutate(across(matches("^(RU|BY)_ever_"), ~ coalesce(., 0L)),
           chpi     = as.integer(hs6 %in% chpi_set),
           dual_use = as.integer(hs6 %in% du_set),
           ever_any = as.integer(RU_ever_sanctioned + BY_ever_sanctioned > 0),
           across(starts_with("in_Y"), ~ coalesce(., 0L))) |>
    arrange(hs6)
  orph <- sum(rowSums(select(tab, starts_with("in_Y"))) == 0)
  cat(sprintf("%s: %d HS6 | RU ever %d | BY ever %d | RU de-sanct %d | BY de-sanct %d | outside reference %d\n",
      trk, nrow(tab), sum(tab$RU_ever_sanctioned), sum(tab$BY_ever_sanctioned),
      sum(tab$RU_ever_desanctioned), sum(tab$BY_ever_desanctioned), orph))
  write_csv(tab, out_file); cat("Written:", out_file, "\n")
}
build_dir("EXPORT", OUT_X)   # expected: RU ever 3217, BY ever 2700, BY de-sanct 0
build_dir("IMPORT", OUT_M)   # expected: RU ever 1939, BY ever 1809, BY de-sanct 0
