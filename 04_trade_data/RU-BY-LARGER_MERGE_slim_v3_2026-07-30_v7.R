# RU-BY-LARGER_MERGE_slim_v3_2026-07-30_v7.R
# =====================================================================
# MERGE of the LARGER trade extraction into the "slim v3" dataset
# (raw -> slim in a single pass).
# Built on two earlier scripts: RU-MERGE_trade_v2 (closed file lists,
# checks per view) and RU-BY-SLIM_v3_rebuild v3 (DuckDB reads every
# column as text end to end, so values stay byte-identical for the
# Benford tests; a single UNION ALL, a single COPY; log via sink).
#
# VERSION HISTORY
# v7 (2026-08-03, archive copy with the fix, for future re-runs; the
#   good run = v6 + a continuation script): the reader in S3 let the CSV
#   sniffer guess quote/escape. The first rows of our output contained
#   no quoted fields, so the sniffer chose quote=(empty) and the read
#   failed at the first comma inside a name ("China, Hong Kong SAR").
#   The S3 view now declares delim/quote/escape explicitly.
# v6 (2026-08-03): out-of-memory fix in S2. preserve_insertion_order=true
#   forces DuckDB to buffer the 28 input legs to keep the global order:
#   on 225.8M rows it fails ("Allocation failure"). Now
#   preserve_insertion_order=FALSE (streaming COPY) + threads=4.
#   Row order in the output is neither deterministic nor meaningful:
#   the origin of each row is in SOURCE_VIEW and checks S3-S5 do not
#   depend on order. Values stay byte-identical (all_varchar).
# v5 (2026-08-03): two critical fixes after a run that wrote nothing.
#   (a) Population filter R1 uses LIKE: the flag columns carry
#       end-of-line junk (the last quoted field contains CR/LF, as seen
#       in an earlier query: isreported LIKE '1%'). DuckDB's TRIM only
#       removes spaces, so the '=' predicate matched NOTHING and S2
#       wrote an empty file. Now: ISREPORTED LIKE '1%' AND
#       ISAGGREGATE LIKE '0%'.
#   (b) Non-vacuous checks: stop if a view expects 0 rows (the empty
#       run had passed as "green" because 0 == 0). Added S0-bis, a
#       diagnostic of the flag patterns on the smallest top-up file.
#   Also: decimal.mark in format(); CR/LF trimmed from units in S5.
# v4 (2026-08-03): fix in S0, the CSV headers are QUOTED
#   ("PERIOD","REPORTERCODE",...): quotes are stripped in the readLines
#   parser (fread used to remove them on its own).
# v3 (2026-08-03): fix in S0, fread(nrows=0) memory-maps the whole file
#   and fails on 29 GB ("could not memory map"); headers are now read
#   with readLines(n=1), no memory map. Added a guard that closes sinks
#   left open by failed runs.
# v2 (2026-07-31): (a) global cut-off P* = 2025-12 (design decision
#   after a period-by-period diagnosis: PERIOD < '2026-01-01' everywhere;
#   2026 stays in the TOPUP files on disk for a future refresh);
#   (b) expected TOPUP rows = block [E] certified on 2026-07-31
#   (2025 window, source = file); (c) R1 confirmed by
#   LARGER_aggstructure_probe v2 (no total-only flows in 4 sample views).
#
# RULES
#  R1 POPULATION: ISREPORTED = '1' AND ISAGGREGATE = '0' (same population
#     as the REDUX v2 release). Rows are kept at the granularity of the
#     bulk files: totals by hs6 x month coincide; ~0.2% of mixed cases
#     cannot be filtered (no mode-of-transport code) and are logged.
#  R2 WINDOW: base = PERIOD < '2025-01-01'; TOPUP = 2025-01..2025-12.
#     P* = 2025-12: slim v3 LARGER covers 2010-01..2025-12.
#  R3 SCHEMA = slim v3 REDUX, same 15 columns in the same order.
#  R4 NEVER OVERWRITE; log via sink.
# RUN WITH source(). LONG wait (hours: several scans of ~90 GB).
# Console messages and some variable names are in Italian.
# =====================================================================

library(duckdb)
library(data.table)

## CONFIG --------------------------------------------------------------------
DIR_L  <- "data/LARGER"   # folder with the 28 raw CSV extracts
F_OUT  <- file.path(DIR_L, "LARGER_dataset_slim_v3_2026-07-30.csv")
F_SUM  <- file.path(DIR_L, "RU-BY-LARGER_SLIM_v3_SUMMARY_2026-07-30.txt")
TMPDIR <- "data/duckdb_tmp"
TOPUP_TAG <- "__TOPUP_2026-07-30"
CUT_LO <- "'2025-01-01'"   # start of the TOPUP window
CUT_HI <- "'2026-01-01'"   # P*: the whole dataset is < 2026-01-01

while (sink.number() > 0) sink()   # closes logs left open by failed runs
stopifnot(dir.exists(DIR_L))
if (file.exists(F_OUT)) stop("Output gia' esistente: ", F_OUT,
                             "\nMai sovrascrivere: rinomina o sposta prima",
                             " (se e' il file VUOTO del run 08-03: quarantena).")
if (!dir.exists(TMPDIR)) dir.create(TMPDIR)

## FILE PLAN (CLOSED lists; base = row counts certified by the SQL extraction)
base_plan <- data.table(rbind(
  c("Larger_Exp_EU27_to_RU_BY.csv",              "EXP_EU27_TO_RU_BY",      23628730),
  c("Larger_Imp_EU27_from_RU_BY.csv",            "IMP_EU27_FROM_RU_BY",     9187526),
  c("Larger_Exp_RU_BY_to_EU27.csv",              "EXP_RU_BY_TO_EU27",       1718919),
  c("Larger_Imp_RU_BY_from_EU27.csv",            "IMP_RU_BY_FROM_EU27",     6926081),
  c("Larger_Exp_TRANSIT_to_RU_BY.csv",           "EXP_TRANSIT_TO_RU_BY",    6842633),
  c("Larger_Imp_TRANSIT_from_RU_BY.csv",         "IMP_TRANSIT_FROM_RU_BY",  6756960),
  c("Larger_Exp_RU_BY_to_TRANSIT.csv",           "EXP_RU_BY_TO_TRANSIT",    2373844),
  c("Larger_Imp_RU_BY_from_TRANSIT.csv",         "IMP_RU_BY_FROM_TRANSIT",  2752606),
  c("Larger_Imp_EU27_from_TRANSIT.csv",          "IMP_EU27_FROM_TRANSIT", 248013423),
  c("Larger_Exp_EU27_to_TRANSIT.csv",            "EXP_EU27_TO_TRANSIT",   178857446),
  c("LARGER_EXP_TRANSIT_TO_EU27__2010_2018.csv", "EXP_TRANSIT_TO_EU27",    27794621),
  c("LARGER_EXP_TRANSIT_TO_EU27__2019_2021.csv", "EXP_TRANSIT_TO_EU27",    16880268),
  c("LARGER_EXP_TRANSIT_TO_EU27__2022_2026.csv", "EXP_TRANSIT_TO_EU27",    27873892),
  c("LARGER_IMP_TRANSIT_FROM_EU27__2010_2014.csv","IMP_TRANSIT_FROM_EU27", 16581871),
  c("LARGER_IMP_TRANSIT_FROM_EU27__2015_2017.csv","IMP_TRANSIT_FROM_EU27", 27922535),
  c("LARGER_IMP_TRANSIT_FROM_EU27__2018_2019.csv","IMP_TRANSIT_FROM_EU27", 26646658),
  c("LARGER_IMP_TRANSIT_FROM_EU27__2020_2021.csv","IMP_TRANSIT_FROM_EU27", 27974620),
  c("LARGER_IMP_TRANSIT_FROM_EU27__2022_2023.csv","IMP_TRANSIT_FROM_EU27", 31210785),
  c("LARGER_IMP_TRANSIT_FROM_EU27__2024.csv",    "IMP_TRANSIT_FROM_EU27",  15598138),
  c("LARGER_IMP_TRANSIT_FROM_EU27__2025_2026.csv","IMP_TRANSIT_FROM_EU27", 18019267)))
setnames(base_plan, c("file", "view", "atteso"))
base_plan[, `:=`(atteso = as.numeric(atteso), kind = "base")]

# expected TOPUP rows in the P* window (block [E], certified 2026-07-31)
topup_E <- c(EXP_EU27_TO_RU_BY      =   594070,
             IMP_EU27_FROM_RU_BY    =   333544,
             EXP_TRANSIT_TO_RU_BY   =   644437,
             IMP_TRANSIT_FROM_RU_BY =   563221,
             EXP_TRANSIT_TO_EU27    =  6902106,
             IMP_TRANSIT_FROM_EU27  = 15061916,
             EXP_EU27_TO_TRANSIT    = 14406082,
             IMP_EU27_FROM_TRANSIT  = 26673956)

topup_plan <- data.table(
  file = paste0("LARGER_", names(topup_E), TOPUP_TAG, ".csv"),
  view = names(topup_E), atteso = as.numeric(topup_E), kind = "topup")

piano <- rbind(base_plan, topup_plan)
piano[, path := file.path(DIR_L, file)]
manca <- piano[!file.exists(path), file]
if (length(manca)) stop("File mancanti: ", paste(manca, collapse = ", "))

sink(F_SUM, split = TRUE)
cat("LARGER MERGE -> SLIM v3 — v2 —", format(Sys.time()), "\n")
cat("P* = 2025-12 (PERIOD <", CUT_HI, "); base <", CUT_LO,
    "; TOPUP", CUT_LO, "..<", CUT_HI, "\n")
cat("File in piano:", nrow(piano), "(", sum(piano$kind == "base"), "base +",
    sum(piano$kind == "topup"), "topup )\n")

## S0 — HEADER ----------------------------------------------------------------
cat("\n================ S0 HEADER ================\n")
HDR <- c("PERIOD","REPORTERCODE","REPORTER_NAME","REPORTER_GROUP",
         "PARTNERCODE","PARTNER_NAME","PARTNER_GROUP","FLOWCODE",
         "CMDCODE","CLASSIFICATIONCODE","QTYUNITCODE","QTY","NETWGT",
         "CIFVALUE","FOBVALUE","PRIMARYVALUE","ISREPORTED","ISAGGREGATE")
leggi_header <- function(p) {          # streaming: no memory map
  con <- file(p, "r"); on.exit(close(con))
  toupper(gsub('"', "", trimws(
    strsplit(readLines(con, n = 1), ",", fixed = TRUE)[[1]])))
}
for (p in piano$path) {
  h <- leggi_header(p)
  if (!identical(sort(h), sort(HDR)))
    stop("Header inatteso in ", basename(p), ":\n  ", paste(h, collapse=", "))
}
cat("18 colonne attese su", nrow(piano), "file: OK\n")

## SQL CONSTANTS ---------------------------------------------------------------
POP    <- "ISREPORTED LIKE '1%' AND ISAGGREGATE LIKE '0%'"  # see v5: CR/LF junk in the flags
CORE11 <- c("PERIOD","REPORTERCODE","REPORTER_NAME","REPORTER_GROUP",
            "PARTNERCODE","PARTNER_NAME","PARTNER_GROUP","FLOWCODE",
            "CMDCODE","CLASSIFICATIONCODE","PRIMARYVALUE")
QCOLS  <- c("QTYUNITCODE","QTY","NETWGT")
finestra <- function(kind) if (kind == "base")
  sprintf("PERIOD < %s", CUT_LO) else
    sprintf("PERIOD >= %s AND PERIOD < %s", CUT_LO, CUT_HI)
# raw integrity: base = whole file; topup = P* window only
integr <- function(kind) if (kind == "base") "1=1" else
  sprintf("PERIOD >= %s AND PERIOD < %s", CUT_LO, CUT_HI)

con <- dbConnect(duckdb())
dbExecute(con, "SET preserve_insertion_order = false")  # v6: streaming, no global buffer
dbExecute(con, "SET threads TO 4")
dbExecute(con, "SET enable_progress_bar = false")
dbExecute(con, "SET memory_limit = '8GB'")
dbExecute(con, sprintf("SET temp_directory = '%s'", TMPDIR))

## S0-bis — FLAG DIAGNOSTICS (patterns and lengths, logged) ------------------
cat("\n================ S0-bis PATTERN FLAG ================\n")
piccolo <- gsub("\\\\", "/", piano[file == "LARGER_IMP_EU27_FROM_RU_BY__TOPUP_2026-07-30.csv", path])
print(dbGetQuery(con, sprintf(
  "SELECT ISREPORTED, ISAGGREGATE, LENGTH(ISREPORTED) AS len_rep,
          LENGTH(ISAGGREGATE) AS len_agg, COUNT(*) AS n
   FROM read_csv('%s', all_varchar=TRUE)
   GROUP BY 1,2,3,4 ORDER BY n DESC LIMIT 8", piccolo)))
cat("(len > 1 = junk di fine riga nel valore: il predicato usa LIKE)\n")

## S1 — INTEGRITY + EXPECTED ROWS AFTER FILTERING (one scan) -------------------
cat("\n================ S1 INTEGRITA' E ATTESE ================\n")
cat("Scansione statistiche (attesa lunga)...\n")
s1_sel <- vapply(seq_len(nrow(piano)), function(i) sprintf(
  "SELECT '%s' AS f, '%s' AS sv, '%s' AS kind,
          SUM(CASE WHEN %s THEN 1 ELSE 0 END) AS n_integr,
          SUM(CASE WHEN %s AND %s THEN 1 ELSE 0 END) AS n_keep,
          SUM(CASE WHEN %s AND %s
                   THEN COALESCE(TRY_CAST(PRIMARYVALUE AS DOUBLE),0)
                   ELSE 0 END) AS somma_keep,
          SUM(CASE WHEN %s AND %s
                    AND TRY_CAST(PRIMARYVALUE AS DOUBLE) IS NULL
                   THEN 1 ELSE 0 END) AS n_prim_bad
   FROM read_csv('%s', all_varchar=TRUE)",
  piano$file[i], piano$view[i], piano$kind[i],
  integr(piano$kind[i]),
  POP, finestra(piano$kind[i]),
  POP, finestra(piano$kind[i]),
  POP, finestra(piano$kind[i]),
  gsub("\\\\", "/", piano$path[i])), "")
s1 <- as.data.table(dbGetQuery(con, paste(s1_sel, collapse = " UNION ALL ")))

s1 <- merge(s1, piano[, .(f = file, atteso)], by = "f", sort = FALSE)
s1[, ok := n_integr == atteso]
print(s1[, .(f, kind, n_integr, atteso, ok, n_keep, n_prim_bad)],
      row.names = FALSE)
stopifnot(all(s1$ok))
stopifnot(s1[, sum(n_keep)] > 0)          # never again an empty run
cat("Integrita': 20 certificati (file intero) e 8 TOPUP (finestra P*,\n",
    "blocco [E] 2026-07-31): OK\n")
stopifnot(s1[, sum(n_prim_bad)] == 0)

att_view <- s1[, .(n_att = sum(n_keep), somma_att = sum(somma_keep)), by = sv]
stopifnot(att_view[, all(n_att > 0)])     # every view must contribute
cat("\nAttese per vista (post R1+R2, finestra 2010-01..2025-12):\n")
print(att_view[order(sv)], row.names = FALSE)
cat("TOTALE atteso slim v3 LARGER:",
    format(att_view[, sum(n_att)], big.mark = ".", decimal.mark = ","), "righe\n")

## S2 — WRITE: ONE UNION ALL, ONE COPY -----------------------------------------
cat("\n================ S2 SCRITTURA ================\n")
sel_cols <- paste(c(CORE11, "SOURCE_VIEW", QCOLS), collapse = ", ")
s2_sel <- vapply(seq_len(nrow(piano)), function(i) sprintf(
  "SELECT %s, '%s' AS SOURCE_VIEW, %s
   FROM read_csv('%s', all_varchar=TRUE)
   WHERE %s AND %s",
  paste(CORE11, collapse = ", "), piano$view[i],
  paste(QCOLS, collapse = ", "),
  gsub("\\\\", "/", piano$path[i]), POP, finestra(piano$kind[i])), "")
cat("Scrittura in corso (28 gambe in un solo COPY; attesa MOLTO lunga)...\n")
t0 <- Sys.time()
dbExecute(con, sprintf(
  "COPY (SELECT %s FROM (%s)) TO '%s' (FORMAT CSV, HEADER TRUE)",
  sel_cols, paste(s2_sel, collapse = " UNION ALL "),
  gsub("\\\\", "/", F_OUT)))
cat("Scrittura completata in",
    round(difftime(Sys.time(), t0, units = "mins"), 1), "minuti.\n")

## S3 — OUTPUT RECONCILIATION ---------------------------------------------------
cat("\n================ S3 RICONCILIAZIONE ================\n")
dbExecute(con, sprintf(
  "CREATE VIEW v3 AS SELECT * FROM read_csv('%s', all_varchar=TRUE, header=TRUE, delim=',', quote='\"', escape='\"')",
  gsub("\\\\", "/", F_OUT)))
chk <- as.data.table(dbGetQuery(con,
                                "SELECT SOURCE_VIEW AS sv, COUNT(*) AS n,
          SUM(COALESCE(TRY_CAST(PRIMARYVALUE AS DOUBLE),0)) AS somma,
          SUM(CASE WHEN TRY_CAST(PRIMARYVALUE AS DOUBLE) IS NULL
                   THEN 1 ELSE 0 END) AS n_na,
          SUM(CASE WHEN TRY_CAST(PRIMARYVALUE AS DOUBLE) < 0
                   THEN 1 ELSE 0 END) AS n_neg,
          SUM(CASE WHEN TRY_CAST(PRIMARYVALUE AS DOUBLE) = 0
                   THEN 1 ELSE 0 END) AS n_zero
   FROM v3 GROUP BY 1 ORDER BY 1"))
m <- merge(att_view, chk, by = "sv")
m[, `:=`(righe_ok  = n == n_att,
         valore_ok = abs(somma - somma_att) <= 1e-6 * pmax(abs(somma_att), 1))]
print(m[, .(sv, n_att, n, righe_ok, valore_ok, n_na, n_neg, n_zero)],
      row.names = FALSE)
cat("C1 totale:", format(chk[, sum(n)], big.mark = ".", decimal.mark = ","),
    if (chk[, sum(n)] == att_view[, sum(n_att)]) "OK" else "** MISMATCH **", "\n")
stopifnot(m[, all(righe_ok)], m[, all(valore_ok)])
cat("C2 righe per vista: OK | C3 somme valori: OK",
    "| NA:", chk[, sum(n_na)], "| negativi:", chk[, sum(n_neg)],
    "| zeri:", chk[, sum(n_zero)], "\n")
cat("Byte-identita' dei VALORI: per costruzione (all_varchar).\n")
cat("Ordine di riga: non deterministico (streaming v6); provenienza\n")
cat("in SOURCE_VIEW; nessun significato analitico dell'ordine.\n")
cat("MIN/MAX PERIOD attesi 2010-01-01 / 2025-12-01:\n")
print(dbGetQuery(con, "SELECT MIN(PERIOD) min_p, MAX(PERIOD) max_p FROM v3"))

## S4 — QUANTITY COVERAGE -------------------------------------------------------
cat("\n================ S4 COPERTURA QUANTITA' ================\n")
print(as.data.table(dbGetQuery(con,
                               "SELECT SOURCE_VIEW AS sv, COUNT(*) AS n,
          ROUND(100.0*SUM(CASE WHEN COALESCE(TRY_CAST(QTY AS DOUBLE),0) > 0
                THEN 1 ELSE 0 END)/COUNT(*), 1) AS pct_qty,
          ROUND(100.0*SUM(CASE WHEN COALESCE(TRY_CAST(NETWGT AS DOUBLE),0) > 0
                THEN 1 ELSE 0 END)/COUNT(*), 1) AS pct_netwgt
   FROM v3 GROUP BY 1 ORDER BY 1")), row.names = FALSE)

## S5 — UNITS PER HS6 -----------------------------------------------------------
cat("\n================ S5 UNITA' PER HS6 ================\n")
print(as.data.table(dbGetQuery(con,
                               "SELECT n_unita, COUNT(*) AS n_hs6 FROM (
     SELECT CMDCODE, COUNT(DISTINCT NULLIF(TRIM(QTYUNITCODE,
                  ' ' || chr(9) || chr(10) || chr(13)),'')) AS n_unita
     FROM v3 WHERE COALESCE(TRY_CAST(QTY AS DOUBLE),0) > 0
     GROUP BY 1) GROUP BY 1 ORDER BY 1")), row.names = FALSE)

dbDisconnect(con, shutdown = TRUE)
cat("\nScritto:", F_OUT, "\n")
cat("LARGER slim v3 copre 2010-01..2025-12 (P*; congelamento certificato\n")
cat("2026-07-31); il 2026 resta nei TOPUP su disco per refresh futuro.\n")
cat("Colonne: 11 core + SOURCE_VIEW + QTYUNITCODE, QTY, NETWGT.\n")
cat("================ FINE MERGE LARGER v2 ================\n")
sink()
