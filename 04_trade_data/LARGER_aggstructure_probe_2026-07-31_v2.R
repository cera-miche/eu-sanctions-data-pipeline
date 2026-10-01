# LARGER_aggstructure_probe_2026-07-31_v2.R
# ---------------------------------------------------------------------------
# QUESTION: are there flows reported ONLY as totals (aggregate rows with no
#   detailed counterpart)? If ~0, filter R1 of the merge (keep reported,
#   non-aggregate rows) loses nothing; if material, the merge needs a
#   conditional rule.
# v2: (a) fix for fifelse inside by= (group column of length 1): the value
#   per key is now precomputed in val_use; (b) added M_TR_EU as a fourth
#   sample: it is the view where an earlier sample query showed pairs of
#   total and detail rows.
# Console messages are in Italian.
# ---------------------------------------------------------------------------
library(data.table)
dir_larger <- "data/LARGER"
TAG <- "__TOPUP_2026-07-30.csv"
campione <- c("LARGER_EXP_TRANSIT_TO_RU_BY",
              "LARGER_IMP_TRANSIT_FROM_RU_BY",
              "LARGER_IMP_EU27_FROM_RU_BY",
              "LARGER_IMP_TRANSIT_FROM_EU27")   # 18.4M rows: the slowest

chiave <- c("PERIOD","REPORTERCODE","PARTNERCODE","CMDCODE",
            "CLASSIFICATIONCODE","QTYUNITCODE")

for (v in campione) {
  p <- file.path(dir_larger, paste0(v, TAG))
  stopifnot(file.exists(p))
  dt <- fread(p, select = c(chiave, "ISREPORTED", "ISAGGREGATE",
                            "PRIMARYVALUE"),
              colClasses = "character", showProgress = FALSE)
  dt[, `:=`(rep = trimws(ISREPORTED) == "1" & trimws(ISAGGREGATE) == "0",
            agg = trimws(ISAGGREGATE) == "1",
            val = suppressWarnings(as.numeric(PRIMARYVALUE)))]
  k <- dt[, .(n_rep = sum(rep), n_agg = sum(agg),
              val_rep = sum(val[rep], na.rm = TRUE),
              val_agg = sum(val[agg], na.rm = TRUE)), by = chiave]
  k[, tipo := fifelse(n_rep > 0 & n_agg > 0, "entrambi",
                      fifelse(n_rep > 0, "solo_dettaglio", "SOLO_TOTALE"))]
  k[, val_use := fifelse(tipo == "SOLO_TOTALE", val_agg, val_rep)]
  cat("\n==", v, "==\n")
  print(k[, .(chiavi = .N, valore_M = round(sum(val_use) / 1e6, 1)),
          by = tipo], row.names = FALSE)
  co <- k[tipo == "entrambi"]
  if (nrow(co))
    cat("Coerenza dove coesistono: |val_agg - val_rep| <= 0,5% in",
        round(100 * co[, mean(abs(val_agg - val_rep) <=
                                0.005 * pmax(abs(val_agg), 1))], 1),
        "% delle chiavi\n")
  st <- k[tipo == "SOLO_TOTALE"]
  cat("SOLO-TOTALE:", nrow(st), "chiavi (",
      round(100 * nrow(st) / nrow(k), 2), "% ) | valore",
      round(st[, sum(val_agg)] / 1e6, 1), "M (",
      round(100 * st[, sum(val_agg)] / k[, sum(val_use)], 2),
      "% del totale vista )\n")
  rm(dt, k); gc(verbose = FALSE)
}
cat("\nLETTURA: SOLO_TOTALE ~0% ovunque -> R1 va bene (righe a livello\n",
    "dettaglio, totali hs6 x mese identici). Materiale -> regola\n",
    "condizionale nel merge v2.\n")
