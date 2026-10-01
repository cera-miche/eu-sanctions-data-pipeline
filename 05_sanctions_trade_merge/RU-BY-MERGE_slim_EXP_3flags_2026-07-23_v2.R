# RU-BY-MERGE_slim_EXP_3flags_2026-07-23_v2.R
# "Lean" merge COMTRADE x sanctions, EXPORT track only, 3 columns:
#   ever_sanctioned   0/1  the code has ever been under an export ban
#   ever_desanctioned 0/1  ...and has left the list at least once
#   is_sanctioned     0/1  sanctioned IN THAT MONTH (monthly state from the long-state)
# Country-resolved: rows with 643 -> RU panel; rows with 112 -> BY panel;
# transit flows EU <-> Central Asia, Caucasus and Turkiye -> RU panel
# (covariate). + treat_country {RU, BY, TRANSIT}. Never overwrites.
#
# VERSION HISTORY
#   2026-07-06 (v1) = input slim v2 (12 cols); output EXP3flags 16 cols.
#   2026-07-23 v2   = input SLIM v3 (slim v2 + QTYUNITCODE, QTY, NETWGT
#     at the end; built by RU-BY-SLIM_v3_rebuild_quantities v3, checks
#     C1-C5 green). The merge logic is UNCHANGED: t.* EXCLUDE(month)
#     carries the 3 quantity columns automatically. Output with 19
#     columns. Added guards: 15 columns expected in input, 19 in
#     output, quantities present by name.
# NOTE ON USE (PPML v16): netwgt = main control (coverage 95-99%);
# qty = robustness (coverage 68-93%, and 492 HS6 with several units:
# sum qty only where the unit is unique in the cell).

library(duckdb); library(DBI); library(dplyr); library(dbplyr); library(readr)

SLIM <- "data/REDUX_dataset_slim_v3_2026-07-17.csv"
RU   <- "data/RU-A_recent_run/RU-Output_R_Step3_2026-06-15_19-16-35"
BY   <- "data/BY-A_recent_run/BY-Output_R_Step3_2026-07-06_16-46-31"
OUT  <- "output/REDUX_dataset_EXP3flags_v2_2026-07-23.csv"
N_EXPECTED <- 41446891L
stopifnot(file.exists(SLIM), dir.exists(RU), dir.exists(BY), !file.exists(OUT))
dir.create("output", showWarnings = FALSE)

## 0. Input schema guard (v2): slim v3 = the 12 cols of v2 + 3 quantities ------
hdr <- names(read_csv(SLIM, n_max = 0, col_types = cols(.default = "c")))
cat("Input columns:", length(hdr), "(15 expected)\n")
stopifnot(length(hdr) == 15,
          all(c("QTYUNITCODE", "QTY", "NETWGT") %in% toupper(hdr)))

## 1. Lookups from the EXPORT track -------------------------------------------
mkey <- function(m) as.integer(substr(m,1,4))*12L + as.integer(substr(m,6,7))

mk_lk <- function(dir_path, ctry) {
  long <- read_csv(file.path(dir_path, sprintf("%s-STEP3_long_state_EXPORT_hs6.csv", ctry)),
                   col_types = cols(.default = "c"))
  pend <- max(mkey(long$month))
  monthly <- long |> distinct(hs6, month) |> mutate(is_s = 1L)
  codelvl <- long |> group_by(hs6) |>
    summarise(n_on = n(), k_min = min(mkey(month)), k_max = max(mkey(month)),
              .groups = "drop") |>
    transmute(hs6,
              ever_s  = 1L,
              # de-sanctioned if there is a gap in the ON sequence
              # or the last ON month comes before the end of the panel
              ever_de = as.integer(n_on < (k_max - k_min + 1L) | k_max < pend))
  list(monthly = monthly, codelvl = codelvl)
}
ru <- mk_lk(RU, "RU"); by <- mk_lk(BY, "BY")
cat("RU: ", nrow(ru$codelvl), "codes (3217 expected), de-sanct:",
    sum(ru$codelvl$ever_de), "| BY:", nrow(by$codelvl),
    "codes (2700 expected), de-sanct:", sum(by$codelvl$ever_de), "(0 expected)\n")

## 2. DuckDB --------------------------------------------------------------------
con <- dbConnect(duckdb::duckdb())
dbExecute(con, "SET memory_limit='8GB'")
dbExecute(con, "SET temp_directory='data/duckdb_tmp'")
copy_to(con, ru$monthly, "m_ru", temporary = TRUE, overwrite = TRUE)
copy_to(con, by$monthly, "m_by", temporary = TRUE, overwrite = TRUE)
copy_to(con, ru$codelvl, "c_ru", temporary = TRUE, overwrite = TRUE)
copy_to(con, by$codelvl, "c_by", temporary = TRUE, overwrite = TRUE)

## 3. Merge in a single streaming statement --------------------------------------
dbExecute(con, sprintf("
COPY (
  WITH t AS (
    SELECT *, substr(PERIOD,1,7) AS month,
           CASE WHEN REPORTERCODE='643' OR PARTNERCODE='643' THEN 'RU'
                WHEN REPORTERCODE='112' OR PARTNERCODE='112' THEN 'BY'
                ELSE 'TRANSIT' END AS treat_country
    FROM read_csv('%s', all_varchar=true, header=true)
  )
  SELECT t.* EXCLUDE (month),
    CASE WHEN t.treat_country='BY' THEN COALESCE(cb.ever_s, 0)
         ELSE COALESCE(cr.ever_s, 0) END                       AS ever_sanctioned,
    CASE WHEN t.treat_country='BY' THEN COALESCE(cb.ever_de, 0)
         ELSE COALESCE(cr.ever_de, 0) END                      AS ever_desanctioned,
    CASE WHEN t.treat_country='BY' THEN COALESCE(mb.is_s, 0)
         ELSE COALESCE(mr.is_s, 0) END                         AS is_sanctioned
  FROM t
  LEFT JOIN m_ru mr ON t.CMDCODE = mr.hs6 AND t.month = mr.month
  LEFT JOIN m_by mb ON t.CMDCODE = mb.hs6 AND t.month = mb.month
  LEFT JOIN c_ru cr ON t.CMDCODE = cr.hs6
  LEFT JOIN c_by cb ON t.CMDCODE = cb.hs6
) TO '%s' (HEADER, FORMAT CSV)", SLIM, OUT))
cat("Written:", OUT, "\n")

## 4. Guards -----------------------------------------------------------------------
n <- dbGetQuery(con, sprintf(
  "SELECT count(*) n FROM read_csv('%s', all_varchar=true, header=true)", OUT))$n
cat("Rows:", n, "\n"); stopifnot(n == N_EXPECTED)
hdr_out <- names(read_csv(OUT, n_max = 0, col_types = cols(.default = "c")))
cat("Output columns:", length(hdr_out), "(19 expected)\n")
stopifnot(length(hdr_out) == 19,
          all(c("QTYUNITCODE", "QTY", "NETWGT") %in% toupper(hdr_out)))
sanity <- dbGetQuery(con, sprintf("
  SELECT treat_country,
         sum(CASE WHEN is_sanctioned='1'     THEN 1 ELSE 0 END) AS on_month,
         sum(CASE WHEN ever_sanctioned='1'   THEN 1 ELSE 0 END) AS ever,
         sum(CASE WHEN ever_desanctioned='1' THEN 1 ELSE 0 END) AS ever_de
  FROM read_csv('%s', all_varchar=true, header=true)
  GROUP BY 1 ORDER BY 1", OUT))
print(sanity)
dbDisconnect(con, shutdown = TRUE)
