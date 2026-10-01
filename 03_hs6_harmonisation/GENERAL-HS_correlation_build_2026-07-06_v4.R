# =============================================================================
# GENERAL-HS_correlation_build_2026-07-06_v4.R
#
# WHAT IT DOES: processes the official UNSD correlation table HS2022<->HS2017
# (downloaded by hand, a Stage 0 artefact) and produces:
#   1. GENERAL-HS2022_HS2017_correlation_2026-07-06_v2.csv
#        clean pairs (h6_2022, h6_2017) + type of correspondence
#   2. GENERAL-HS_families_2017_2022_2026-07-06_v2.csv
#        "families" = connected components of the correspondence graph
#        (the unit of aggregation for the family-level PPML robustness check)
#   3. GENERAL-HS_correlation_SUMMARY_2026-07-06.txt
#        all key numbers in one citable text file
#   4. four PNG figures (base R, no extra dependencies)
#
# HISTORY: v1 had the split/merge labels SWAPPED in the case_when (counts
# were right, names inverted). v2 fixes it and also prints the 1:1
# renamings and the identity of the "orphan" codes in the cross-check.
# v3: outputs are OVERWRITTEN at every run (a deliberate exception to the
# "never overwrite" convention: these files are deterministic derivatives,
# regenerated in seconds from an immutable input) + plain-language captions
# for panel (b). v4: SEPARATE figures (one PNG per panel) + an example figure
# that explains what a "group" is, drawn with TWO REAL GROUPS taken from the
# data (a renaming and a split).
#
# WHY IT EXISTS: COMTRADE classifies each record in the HS vintage in use
# that year (2019-2021 = H5/HS2017, 2022+ = H6/HS2022). To use the years
# before and after together, we need to know which codes are comparable.
# Design decision (2026-07-06): do NOT apply the map for now; use it only to
# COUNT the codes that change and to quantify the error of the "stable
# codes" restriction.
# =============================================================================

library(readxl)    # reads xlsx
library(dplyr); library(readr); library(stringr); library(tidyr)

## --- CONFIG ------------------------------------------------------------------
RAW <- "data/GENERAL-HS2022_HS2017_correlation_raw_2026-07-06.xlsx"
REF <- "data/GENERAL-6-digits_codes_2002_2007_2012_2017_2022.csv"
OUT_CORR <- "output/GENERAL-HS2022_HS2017_correlation_2026-07-06_v2.csv"
OUT_FAM  <- "output/GENERAL-HS_families_2017_2022_2026-07-06_v2.csv"
OUT_SUM  <- "output/GENERAL-HS_correlation_SUMMARY_2026-07-06.txt"
OUT_FIG1 <- "output/GENERAL-HS_fig1_types_2026-07-06.png"
OUT_FIG2 <- "output/GENERAL-HS_fig2_groups_2026-07-06.png"
OUT_FIG3 <- "output/GENERAL-HS_fig3_universe_2026-07-06.png"
OUT_FIG0 <- "output/GENERAL-HS_fig0_example_groups_2026-07-06.png"
stopifnot(file.exists(RAW), file.exists(REF))
dir.create("output", showWarnings = FALSE)
## v3: outputs are OVERWRITTEN at every run (no guard).

## code hygiene: drop every non-digit, zero-pad to 6 ("10121" -> "010121";
## a defence against Excel eating leading zeros)
pad6 <- function(x) str_pad(str_remove_all(as.character(x), "\\D"), 6, pad = "0")

## == 1. ROBUST READING OF THE UNSD XLSX ========================================
## Everything as text; then a HEURISTIC to find the two code columns: a
## column is a "code" column if >90% of its non-empty cells look like a 5-6
## digit code. This way the script survives extra descriptive columns or
## different headers in future UNSD editions.
raw <- read_excel(RAW, col_types = "text")
cat("Columns found:", paste(names(raw), collapse = " | "), "\n")
is_code_col <- sapply(raw, function(v) {
  v <- v[!is.na(v)]; if (!length(v)) return(FALSE)
  mean(grepl("^\\d{5,6}(\\.\\d+)?$", str_remove_all(v, "[ .]"))) > 0.9
})
code_cols <- names(raw)[is_code_col]
cat("Code columns detected:", paste(code_cols, collapse = " | "), "\n")
stopifnot(length(code_cols) >= 2)

## side assignment: look for "22"/"17" in the column NAMES; if that fails,
## use the order and WARN (swapping the sides would swap split and merge ->
## read the warning!)
c22 <- code_cols[grepl("22", code_cols)][1]
c17 <- code_cols[grepl("17", code_cols)][1]
if (is.na(c22) | is.na(c17)) {
  c22 <- code_cols[1]; c17 <- code_cols[2]
  cat("WARNING: years not recognised from the names, using the order:",
      "2022 =", c22, "| 2017 =", c17, "-> CHECK!\n")
}

corr <- raw |>
  transmute(h6_2022 = pad6(.data[[c22]]), h6_2017 = pad6(.data[[c17]])) |>
  filter(h6_2022 != "000000", h6_2017 != "000000") |>   # leftovers of empty cells
  distinct()
cat("Correspondence pairs:", nrow(corr), "\n")

## == 2. TYPES OF MAPPING =======================================================
## We count the "degree" of each code in the bipartite graph:
##   n17 = into how many 2017 codes a given 2022 code maps
##   n22 = into how many 2022 codes a given 2017 code maps
## Then we classify EACH PAIR. Mind the direction (bug in v1):
##   n22 > 1  => that 2017 code generates SEVERAL 2022 codes => SPLIT
##   n17 > 1  => that 2022 code absorbs SEVERAL 2017 codes   => MERGE
deg22 <- corr |> count(h6_2022, name = "n17")
deg17 <- corr |> count(h6_2017, name = "n22")
corr2 <- corr |>
  left_join(deg22, by = "h6_2022") |>
  left_join(deg17, by = "h6_2017") |>
  mutate(tipo = case_when(
    n17 == 1 & n22 == 1 ~ "1:1",
    n17 == 1 & n22 >  1 ~ "split (1 2017 -> n 2022)",   # [v2: label fixed]
    n17 >  1 & n22 == 1 ~ "merge (n 2017 -> 1 2022)",   # [v2: label fixed]
    TRUE                ~ "n:m"))
tipo_tab <- corr2 |> count(tipo); print(tipo_tab)

identiche <- corr2 |> filter(tipo == "1:1", h6_2022 == h6_2017)
rinomine  <- corr2 |> filter(tipo == "1:1", h6_2022 != h6_2017)
cat("1:1 with identical code (truly stable):", nrow(identiche), "\n")
cat("1:1 RENAMINGS (same scope, different number):", nrow(rinomine), "\n")
if (nrow(rinomine) > 0) print(rinomine |> select(h6_2017, h6_2022), n = 60)

## == 3. FAMILIES = CONNECTED COMPONENTS (union-find) ===========================
##
## WHY FAMILIES. If the 2017 code "A" was split into the 2022 codes "X" and
## "Y", the series of X alone drops artificially in 2022 (part of its trade
## is now called Y). The only product scope that stays CONSTANT across the
## revision is the set of all linked codes: summing their values gives a
## series that is comparable before and after. That set is the "family".
##
## WHY "ANY CHAIN" (transitive closure). Correspondences chain together:
## A(2017) -> {X,Y}(2022), but Y also receives from B(2017), which also sends
## pieces to Z(2022). The constant scope is not {A,X,Y}: inside Y there are
## goods that in 2017 were in B, so B (and with it Z) must be included.
## The right family = everything REACHABLE by following the edges = a
## connected component of the bipartite graph 2017<->2022.
## (In our data of 2026-07-06 there are no merges and no n:m, so every
## non-trivial family is a simple "star" of splits: 1 parent in 2017 + its
## children in 2022. The code stays general for future vintages.)
##
## WHY THE A/B PREFIXES (homonyms). The WCO recycles numbers: the SAME
## number can exist in both vintages for DIFFERENT goods (e.g. a 2017 030270
## remapped elsewhere, and a 2022 030270 born from other codes). With the
## bare number the two would be ONE node and would wrongly weld together two
## unrelated families. The prefix keeps the two worlds apart: "B030270" =
## the 2017 code, "A030270" = the 2022 code; they end up in the same family
## ONLY if a correspondence row (030270 -> 030270) actually links them.
##
## THE ALGORITHM (union-find / disjoint sets). Each node starts as its own
## family; for each correspondence we "weld" the families of the two ends.
## Representation: parent[x] = a pointer to another node of the same family;
## the REPRESENTATIVE is the node that points to itself (parent[x] == x).
## Two nodes are in the same family iff, following the pointers up, they
## reach the same representative.
nodes  <- unique(c(paste0("A", corr$h6_2022), paste0("B", corr$h6_2017)))
parent <- setNames(nodes, nodes)   # at the start every node points to itself
                                   # (= many one-node families)

## find(x): follows the pointers up to the representative of x's family.
## The line parent[[x]] <<- parent[[parent[[x]]]] is PATH COMPRESSION:
## on the way up it makes x skip one level of pointers, so future climbs
## are shorter. (<<- is needed because we modify `parent`, which lives
## OUTSIDE the function.) Without compression the algorithm would still be
## correct but could degenerate into long chains.
find <- function(x) {
  while (parent[[x]] != x) {
    parent[[x]] <<- parent[[parent[[x]]]]
    x <- parent[[x]]
  }; x
}

## UNION: for each row of the correlation table (= an edge of the graph)
## find the two representatives; if they differ, make one point to the
## other -> from then on the two families are ONE. Row order does not
## matter: the final components are the same.
for (i in seq_len(nrow(corr))) {
  a <- find(paste0("A", corr$h6_2022[i])); b <- find(paste0("B", corr$h6_2017[i]))
  if (a != b) parent[[a]] <- b
}

## Final labelling: the representative of each node identifies its
## component. vapply (find on all nodes) uses the paths already compressed.
comp <- vapply(nodes, find, "")

## From the internal structure to the table for the analysis:
##  - vintage/hs6: split the prefix ("A123456" -> Y2022 + "123456")
##  - family_id = min(hs6) of the members: a STABLE, reproducible identifier
##    (re-running the script, or changing the row order, does not change
##    the id, which the name of the representative would NOT guarantee)
##  - family_size = number of distinct codes: size 1 = 1:1 code (trivial),
##    size > 1 = family to AGGREGATE in the PPML robustness check
fam <- tibble(node = nodes, comp = comp) |>
  mutate(vintage = if_else(startsWith(node, "A"), "Y2022", "Y2017"),
         hs6 = substr(node, 2, 7)) |>
  group_by(comp) |>
  mutate(family_id   = min(hs6),
         family_size = n_distinct(hs6)) |>
  ungroup() |>
  select(family_id, family_size, vintage, hs6) |>
  arrange(family_id, vintage, hs6)
fam_sizes <- fam |> distinct(family_id, family_size)
cat("Families:", nrow(fam_sizes),
    "| non-trivial (size>1, to aggregate in robustness):",
    sum(fam_sizes$family_size > 1), "\n")

## == 4. CROSS-CHECK WITH THE REFERENCE ALREADY IN USE ==========================
## (a) validation: the codes in the correlation table must exist in the
##     Y2022/Y2017 columns of the reference (expected orphans ~0; if >0 we
##     inspect them);
## (b) quantification for the PPML: 'stable' = intersection of Y2017 and
##     Y2022, EXACTLY the criterion of the estimation script
##     (in_Y2017 & in_Y2022).
ref <- read_csv(REF, col_types = cols(.default = "c"))
y17 <- unique(pad6(ref$Y2017[!is.na(ref$Y2017) & ref$Y2017 != ""]))
y22 <- unique(pad6(ref$Y2022[!is.na(ref$Y2022) & ref$Y2022 != ""]))
orf22 <- setdiff(unique(corr$h6_2022), y22)   # in the correlation, not in the ref
orf17 <- setdiff(unique(corr$h6_2017), y17)
cat("Reference: Y2017", length(y17), "| Y2022", length(y22), "\n")
cat("Orphans on the 2022 side:", length(orf22), if (length(orf22)) paste("->", orf22), "\n")
cat("Orphans on the 2017 side:", length(orf17), if (length(orf17)) paste("->", orf17), "\n")
stable_v4  <- intersect(y17, y22)
esclusi_v4 <- setdiff(y22, stable_v4)
cat("Stable (PPML criterion):", length(stable_v4),
    "| excluded by the restriction:", length(esclusi_v4),
    sprintf("(%.1f%% of the Y2022 universe)\n", 100*length(esclusi_v4)/length(y22)))

## == 5. SUMMARY TXT (the citable numbers, in one place) ========================
sink(OUT_SUM)
cat("HS2017 <-> HS2022 CORRELATION — SUMMARY —", format(Sys.time()), "\n")
cat("Source: UNSD correlation table (", basename(RAW), ")\n\n", sep = "")
cat("Correspondence pairs              :", nrow(corr), "\n")
for (i in seq_len(nrow(tipo_tab)))
  cat(sprintf("  %-32s: %d\n", tipo_tab$tipo[i], tipo_tab$n[i]))
cat("1:1 with identical code           :", nrow(identiche), "\n")
cat("1:1 renamings                     :", nrow(rinomine), "\n")
cat("Families, total                   :", nrow(fam_sizes), "\n")
cat("Non-trivial families (size>1)     :", sum(fam_sizes$family_size > 1), "\n")
cat("Largest family size               :", max(fam_sizes$family_size), "\n\n")
cat("Reference Y2017 / Y2022           :", length(y17), "/", length(y22), "\n")
cat("Stable (Y2017 & Y2022)            :", length(stable_v4), "\n")
cat("Excluded by the 'stable' rule     :", length(esclusi_v4),
    sprintf("(%.1f%%)\n", 100*length(esclusi_v4)/length(y22)))
cat("Orphans, correlation vs reference : 2022 ->", length(orf22),
    "| 2017 ->", length(orf17), "\n")
if (length(orf22)) cat("  orphan codes 2022:", paste(orf22, collapse=", "), "\n")
if (length(orf17)) cat("  orphan codes 2017:", paste(orf17, collapse=", "), "\n")
cat("\nNote on use (decision of 2026-07-06): the map is NOT applied to the merge;\n")
cat("it is used to count the codes that change and to quantify the error of the\n")
cat("'stable codes' restriction in the PPML (attenuation = lower bound).\n")
sink()
cat("Written:", OUT_SUM, "\n")

## == 6. FIGURES (v4: four separate PNGs, all overwritten at every run) =========

## helper: draws a node (rectangle + code), returns nothing
draw_node <- function(x, y, lab, col = "grey92") {
  rect(x-0.16, y-0.07, x+0.16, y+0.07, col = col, border = "grey30")
  text(x, y, lab, cex = 1.05, font = 2)
}

## -- FIG 0: EXAMPLE OF A "GROUP", WITH REAL CODES FROM THE DATA --------------
## picks (a) a real renaming (group of 2) and (b) a real split (group of 3):
## the reader sees WHAT the group chart counts before reading it.
ren_ex <- rinomine |> slice(1)                       # a real renaming
fam3   <- fam_sizes |> filter(family_size == 3) |> slice(1)
sp_mem <- fam |> filter(family_id == fam3$family_id)
sp_17  <- sp_mem |> filter(vintage == "Y2017") |> pull(hs6)
sp_22  <- sp_mem |> filter(vintage == "Y2022") |> pull(hs6)

png(OUT_FIG0, width = 1400, height = 620, res = 130)
par(mfrow = c(1, 2), mar = c(1, 1, 3, 1))

## left panel: renaming (group of 2)
plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1))
title("Group of 2 codes: a renaming", cex.main = 1.1)
draw_node(0.22, 0.55, paste0(ren_ex$h6_2017, "\n(2017)"))
draw_node(0.78, 0.55, paste0(ren_ex$h6_2022, "\n(2022)"))
arrows(0.40, 0.55, 0.60, 0.55, length = 0.10, lwd = 1.5)
text(0.5, 0.25, "same product scope,\ndifferent number", cex = 0.95)

## right panel: split (group of 3)
plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1))
title("Group of 3 codes: a split", cex.main = 1.1)
draw_node(0.20, 0.55, paste0(sp_17[1], "\n(2017)"))
draw_node(0.80, 0.75, paste0(sp_22[1], "\n(2022)"))
draw_node(0.80, 0.35, paste0(sp_22[2], "\n(2022)"))
arrows(0.38, 0.60, 0.62, 0.73, length = 0.10, lwd = 1.5)
arrows(0.38, 0.50, 0.62, 0.37, length = 0.10, lwd = 1.5)
text(0.5, 0.08, "the goods of the old code are divided between two new codes:\nonly the SUM of the three is comparable over time", cex = 0.95)
dev.off()
cat("Written:", OUT_FIG0, "\n")

## -- FIG 1: composition of the correspondences --------------------------------
png(OUT_FIG1, width = 900, height = 620, res = 130)
par(mar = c(7, 4, 3, 1))
comp_lbl <- c("1:1 identical", "1:1 renamed", "split", "merge", "n:m")
comp_val <- c(nrow(identiche), nrow(rinomine),
              sum(corr2$tipo == "split (1 2017 -> n 2022)"),
              sum(corr2$tipo == "merge (n 2017 -> 1 2022)"),
              sum(corr2$tipo == "n:m"))
bp <- barplot(comp_val, names.arg = comp_lbl, las = 2,
              main = "HS2017-HS2022 correspondences by type", ylab = "pairs")
text(bp, comp_val, labels = comp_val, pos = 3, xpd = NA, cex = .9)
dev.off()
cat("Written:", OUT_FIG1, "\n")

## -- FIG 2: groups affected by the change, by size ----------------------------
png(OUT_FIG2, width = 900, height = 620, res = 130)
par(mar = c(6, 4, 4, 1))
sz <- table(pmin(fam_sizes$family_size[fam_sizes$family_size > 1], 8))
names(sz)[names(sz) == "8"] <- "8+"
bp2 <- barplot(as.numeric(sz), names.arg = names(sz),
               main = "Groups of codes affected by the 2017-2022 change",
               xlab = "HS6 codes in the group", ylab = "number of groups")
mtext("1 group = 1 code of 2017 + its successors in 2022 (see the example figure)",
      side = 3, line = 0.2, cex = .85)
text(bp2, as.numeric(sz), labels = as.numeric(sz), pos = 3, xpd = NA, cex = .9)
dev.off()
cat("Written:", OUT_FIG2, "\n")

## -- FIG 3: impact on the estimation universe ---------------------------------
png(OUT_FIG3, width = 900, height = 620, res = 130)
par(mar = c(4, 4, 3, 1))
imp_val <- c(length(stable_v4), length(esclusi_v4))
bp3 <- barplot(imp_val, names.arg = c("stable", "excluded"),
               main = "Y2022 universe and PPML restriction", ylab = "HS6 codes")
text(bp3, imp_val,
     labels = sprintf("%d (%.1f%%)", imp_val, 100*imp_val/length(y22)),
     pos = 3, xpd = NA, cex = .9)
dev.off()
cat("Written:", OUT_FIG3, "\n")

## == 7. SAVE CSV ===============================================================
write_csv(corr2 |> select(h6_2022, h6_2017, tipo), OUT_CORR)
write_csv(fam, OUT_FAM)
cat("Written:\n ", OUT_CORR, "\n ", OUT_FAM, "\n")
