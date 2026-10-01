# =====================================================================
# RU-BY-LARGER_descrittive_nonC1_2026-08-11_v1.R
# Descriptive statistics of the LARGER slim v3 dataset (no lookup or
# harmonisation): D1 structure, values and quantities by view; D2 view
# x year; D3 units per HS6; D4 universes; D5 monthly series. Each block
# is reconciled with the canonical SUMMARY written by the merge
# (RU-BY-LARGER_SLIM_v3_SUMMARY_2026-07-30.txt, run of 2026-08-03 14:39).
# ---------------------------------------------------------------------
# EFFECTS ON DISK:
#   - CREATES the subfolder LARGER_descrittive_nonC1_<ts>/
#   - WRITES inside it: 1 log + 5 csv (per_vista, vista_anno, unita_hs6,
#     universi, serie_mensile). NO existing file is modified; the slim
#     dataset is opened READ-ONLY.
# RUN: source() only; log with sink(split=TRUE).
# EXPECTED TIME: 5 full scans of the CSV (~34 GB), 10-30 minutes.
# Engine: DuckDB, explicit parser (all_varchar, declared quote),
# TRY_CAST on numeric columns, same mechanics as the merge checks.
# Console messages and some variable names are in Italian.
# =====================================================================
## Paths (in the project they come from a central paths_config file)
DIR_LARGER <- "data/LARGER"          # folder with the slim v3 CSV
DIR_P2_S6  <- "output/S6_LARGER"     # where the output folder is created
suppressPackageStartupMessages({ library(DBI); library(duckdb) })

SRC <- file.path(DIR_LARGER, "LARGER_dataset_slim_v3_2026-07-30.csv")
if (!file.exists(SRC)) stop("Slim v3 LARGER non trovato: ", SRC)

ts     <- format(Sys.time(), "%Y-%m-%d_%H-%M")
OUTDIR <- file.path(DIR_P2_S6, paste0("LARGER_descrittive_nonC1_", ts))
if (dir.exists(OUTDIR)) stop("OUTDIR gia' esistente: ", OUTDIR)
dir.create(OUTDIR, recursive = TRUE)
logf <- file.path(OUTDIR, paste0("LARGER_descrittive_nonC1_log_", ts, ".txt"))
sink(logf, split = TRUE)
cat("== LARGER descrittive non-C1 v1 ==", ts, "\nSorgente:", SRC, "\n\n")

## Expected values from the canonical SUMMARY (S1/S3/S4/S5, run of 2026-08-03 14:39)
att <- data.frame(
  sv = c("EXP_EU27_TO_RU_BY","EXP_EU27_TO_TRANSIT","EXP_RU_BY_TO_EU27",
         "EXP_RU_BY_TO_TRANSIT","EXP_TRANSIT_TO_EU27","EXP_TRANSIT_TO_RU_BY",
         "IMP_EU27_FROM_RU_BY","IMP_EU27_FROM_TRANSIT","IMP_RU_BY_FROM_EU27",
         "IMP_RU_BY_FROM_TRANSIT","IMP_TRANSIT_FROM_EU27","IMP_TRANSIT_FROM_RU_BY"),
  n_att = c(8013261,52629187,1353431,1787344,23668090,2307331,
            2587389,73032493,5083337,2058775,50583370,2666970),
  s_att = c(1.554648e12,8.690275e12,1.364495e12,9.017609e11,8.758334e12,
            1.083796e12,2.399906e12,1.204363e13,9.358134e11,6.688179e11,
            7.333340e12,1.969952e12),
  z_att = c(0,0,0,0,3,1,0,0,0,0,4,2),
  pq_att = c(65.9,73.6,66.0,68.8,74.3,79.3,71.0,81.9,61.4,69.6,74.6,79.0),
  pn_att = c(95.8,96.0,97.8,97.4,91.2,94.2,96.8,98.2,94.9,97.4,95.0,96.6),
  stringsAsFactors = FALSE)
N_TOT_ATT  <- 225770978
N_2025_ATT <- 17605491   # sum of n_keep over the 8 TOPUP files (block [E], 2026-07-31)

con <- dbConnect(duckdb())
invisible(dbExecute(con, "SET preserve_insertion_order=false"))
rd <- sprintf(paste0("read_csv('%s', header=true, all_varchar=true, ",
                     "delim=',', quote='\"', escape='\"')"), SRC)
base <- sprintf(paste0(
  "SELECT SOURCE_VIEW sv, PERIOD per, substr(PERIOD,1,4) yr, ",
  "TRY_CAST(PRIMARYVALUE AS DOUBLE) val, TRY_CAST(QTY AS DOUBLE) q, ",
  "TRY_CAST(NETWGT AS DOUBLE) nw, QTYUNITCODE unit, CMDCODE hs6, ",
  "REPORTERCODE rep, PARTNERCODE par FROM %s"), rd)
qt <- function(sql) { t0 <- Sys.time()
  out <- dbGetQuery(con, sql)
  cat(sprintf("   [%.1f min]\n",
      as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  out }
fmt <- function(x) format(x, big.mark = ".", scientific = FALSE)

## ---- D1: by view — rows, sums, NA/neg/zero, quantities, periods ------
cat("D1 per vista (scansione 1/5)...\n")
d1 <- qt(sprintf(paste0(
  "SELECT sv, count(*) n, sum(val) somma, ",
  "sum(CASE WHEN val IS NULL THEN 1 ELSE 0 END) n_castfail, ",
  "sum(CASE WHEN val < 0 THEN 1 ELSE 0 END) n_neg, ",
  "sum(CASE WHEN val = 0 THEN 1 ELSE 0 END) n_zero, ",
  "sum(CASE WHEN q  > 0 THEN 1 ELSE 0 END) n_qty_pos, ",
  "sum(CASE WHEN nw > 0 THEN 1 ELSE 0 END) n_nw_pos, ",
  "min(per) min_p, max(per) max_p FROM (%s) GROUP BY sv ORDER BY sv"),
  base))
d1$pct_qty    <- round(100 * d1$n_qty_pos / d1$n, 1)
d1$pct_netwgt <- round(100 * d1$n_nw_pos / d1$n, 1)
m <- merge(d1, att, by = "sv")
ok_n <- all(m$n == m$n_att)
ok_s <- all(abs(m$somma / m$s_att - 1) < 1e-6)
ok_z <- all(m$n_zero == m$z_att)
ok_c <- sum(m$n_castfail) == 0 && sum(m$n_neg) == 0
ok_p <- min(d1$min_p) == "2010-01-01" && max(d1$max_p) == "2025-12-01"
ok_q <- max(abs(m$pct_qty - m$pq_att)) <= 0.1 &&
        max(abs(m$pct_netwgt - m$pn_att)) <= 0.1
print(d1[, c("sv","n","somma","n_castfail","n_neg","n_zero",
             "pct_qty","pct_netwgt","min_p","max_p")],
      row.names = FALSE, right = FALSE)
cat(sprintf(paste0(
  "\nTOT righe: %s (atteso %s) %s\n",
  "Righe per vista: %s | Somme (tol 1e-6): %s | Zeri per vista: %s\n",
  "Cast-fail e negativi = 0: %s | MIN/MAX PERIOD attesi: %s | ",
  "S4 quantita' (tol 0.1 pp): %s\n\n"),
  fmt(sum(d1$n)), fmt(N_TOT_ATT),
  ifelse(sum(d1$n) == N_TOT_ATT, "OK", "FAIL"),
  ifelse(ok_n, "OK", "FAIL"), ifelse(ok_s, "OK", "FAIL"),
  ifelse(ok_z, "OK", "FAIL"), ifelse(ok_c, "OK", "FAIL"),
  ifelse(ok_p, "OK", "FAIL"), ifelse(ok_q, "OK", "FAIL")))
write.csv(d1, file.path(OUTDIR,
  paste0("LARGER_descr_per_vista_", ts, ".csv")), row.names = FALSE)

## ---- D2: view x year -----------------------------------------------
cat("D2 righe e valore per vista x anno (scansione 2/5)...\n")
d2 <- qt(sprintf(paste0("SELECT sv, yr, count(*) n, sum(val) somma ",
  "FROM (%s) GROUP BY sv, yr ORDER BY sv, yr"), base))
write.csv(d2, file.path(OUTDIR,
  paste0("LARGER_descr_vista_anno_", ts, ".csv")), row.names = FALSE)
ann <- aggregate(cbind(n, somma) ~ yr, d2, sum)
ann$somma <- signif(ann$somma, 6)
cat("Totali annui (tutte le viste):\n")
print(ann, row.names = FALSE)
n2025 <- sum(d2$n[d2$yr == "2025"])
cat(sprintf("\nRighe 2025 (= TOPUP, P* 2025-12): %s (atteso %s) %s\n\n",
  fmt(n2025), fmt(N_2025_ATT), ifelse(n2025 == N_2025_ATT, "OK", "FAIL")))

## ---- D3: units per HS6 (rows with qty > 0) ---------------------------
cat("D3 unita' per HS6, righe qty>0 (scansione 3/5)...\n")
d3 <- qt(sprintf(paste0(
  "SELECT n_unita, count(*) n_hs6 FROM (SELECT hs6, ",
  "count(DISTINCT unit) n_unita FROM (%s) WHERE q > 0 GROUP BY hs6) ",
  "GROUP BY n_unita ORDER BY n_unita"), base))
print(d3, row.names = FALSE)
ok3 <- nrow(d3) == 4 && all(d3$n_unita == 1:4) &&
       all(d3$n_hs6 == c(4651, 1331, 4, 3))
cat("Replica S5 (4651/1331/4/3):", ifelse(ok3, "OK", "FAIL"), "\n\n")
write.csv(d3, file.path(OUTDIR,
  paste0("LARGER_descr_unita_hs6_", ts, ".csv")), row.names = FALSE)

## ---- D4: universes by view (+ total) ---------------------------------
cat("D4 universi per vista (scansione 4/5)...\n")
d4 <- qt(sprintf(paste0(
  "SELECT sv, count(DISTINCT hs6) n_hs6, count(DISTINCT rep) n_rep, ",
  "count(DISTINCT par) n_par, count(DISTINCT per) n_mesi FROM (%s) ",
  "GROUP BY GROUPING SETS ((sv), ()) ORDER BY sv"), base))
d4$sv[is.na(d4$sv)] <- "TOTALE"
print(d4, row.names = FALSE)
cat("\n")
write.csv(d4, file.path(OUTDIR,
  paste0("LARGER_descr_universi_", ts, ".csv")), row.names = FALSE)

## ---- D5: monthly series by view (csv only) ---------------------------
cat("D5 serie mensile per vista (scansione 5/5)...\n")
d5 <- qt(sprintf(paste0("SELECT sv, per, count(*) n, sum(val) somma ",
  "FROM (%s) GROUP BY sv, per ORDER BY sv, per"), base))
write.csv(d5, file.path(OUTDIR,
  paste0("LARGER_descr_serie_mensile_", ts, ".csv")), row.names = FALSE)
cat("Serie mensile scritta:", nrow(d5),
    "righe vista x mese (figure in un passo successivo, salva_e_mostra()).\n\n")

dbDisconnect(con, shutdown = TRUE)
cat("Output in:", OUTDIR, "\nLog:", logf,
    "\n== FINE DESCRITTIVE NON-C1 v1 ==\n")
sink()
