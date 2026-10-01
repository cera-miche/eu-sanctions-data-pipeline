library(dplyr)
library(readr)

# Run from the project folder: inputs are read from data/, output is written here

# ── Load the 50 high priority codes ──────────────────────────
hp <- read_csv("data/CN_common_high_priority_items.csv",
               show_col_types = FALSE)

# Normalize: remove spaces so "8542 31" → "854231" for comparison
hp <- hp %>%
  mutate(cn_code_clean = gsub(" ", "", cn_code))

# ── Load all 33 version CSVs ──────────────────────────────────
input_folder <- "data/CN_extraction_results/CN_results_2026-04-24_12-27-37"

csv_files <- list.files(
  path       = input_folder,
  pattern    = "^CN_extraction_.*\\.csv$",
  full.names = TRUE
)

all_data <- bind_rows(
  lapply(csv_files, function(f) {
    df <- read_csv(f, show_col_types = FALSE)
    df$version_date <- gsub("CN_extraction_|\\.csv", "", basename(f))
    return(df)
  })
)

# Normalize CN codes in the same way
all_data <- all_data %>%
  mutate(cn_code_clean = gsub(" ", "", cn_code))

# ── Check presence of each high priority code ─────────────────
# For each of the 50 codes, find in which versions it appears
presence <- hp %>%
  left_join(
    all_data %>%
      select(cn_code_clean, version_date, annex) %>%
      distinct(),
    by = "cn_code_clean"
  ) %>%
  group_by(tier, cn_code, cn_type, description) %>%
  summarise(
    n_versions_found = sum(!is.na(version_date)),
    first_version    = min(version_date, na.rm = TRUE),
    last_version     = max(version_date, na.rm = TRUE),
    annexes_found    = paste(sort(unique(annex[!is.na(annex)])),
                             collapse = ", "),
    .groups = "drop"
  ) %>%
  mutate(
    found = n_versions_found > 0
  ) %>%
  arrange(tier, cn_code)

# ── Print summary ─────────────────────────────────────────────
cat("Total high priority codes     :", nrow(presence), "\n")
cat("Found in at least one version :", sum(presence$found), "\n")
cat("Never found                   :", sum(!presence$found), "\n\n")

cat("Codes NEVER found in any version:\n")
print(presence %>% filter(!found) %>% select(tier, cn_code, description))

cat("\nFull presence table:\n")
print(presence %>% select(tier, cn_code, found, n_versions_found,
                          first_version, last_version, annexes_found))

# ── Save results ──────────────────────────────────────────────
write_csv(presence, "HP_codes_presence_check.csv")
cat("\nSaved: HP_codes_presence_check.csv\n") 