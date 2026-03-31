# canonicalize_entity_names.R
# ============================================================================
# Canonical mapping for IRS SOI entity-type names across tax years 2015-2022.
# See CLAUDE.md "Entity Name Canonicalization" for full documentation.
#
# Usage:
#   source("canonicalize_entity_names.R")
#   data <- canonicalize_entity_names(data, col = "recipient_type")
#
# The function:
#   1. Strips footnote markers like [1], [2]
#   2. Applies explicit name mappings (singular/plural, name variants)
#   3. Merges split categories back to their combined form
#   4. Aggregates numeric columns where multiple raw rows map to one canonical name
# ============================================================================

library(dplyr)

# ---------------------------------------------------------------------------
# CANONICAL NAME MAPPING
# ---------------------------------------------------------------------------
# Each entry maps a raw IRS name to its canonical form.
# Only names that *differ* from their canonical form need to be listed here.
# Names that are already canonical (e.g. "Individuals", "Corporations") are
# left unchanged automatically.
#
# To review for correctness: every raw name that has ever appeared in the
# IRS data should either (a) appear as a key below, or (b) already be in
# canonical form. See CLAUDE.md for the full year-by-year inventory.
# ---------------------------------------------------------------------------

entity_name_mapping <- c(
  # --- Footnote variations (after stripping [N] suffixes) ---
  # These are handled by regex in step 1, so they don't need entries here.
  # Listed as comments for completeness:
  #   "Artists and athletes [1]"  -> "Artists and athletes"   (2019-2020)
  #   "Artists and athletes [2]"  -> "Artists and athletes"   (2017-2018, 2021-2022)
  #   "Tax-exempt organizations [1]" -> "Tax-exempt organizations" (2015-2018)

  # --- Singular/plural variation ---
  "Hybrid entities making treaty claim" = "Hybrid entity making treaty claim",
  # Raw: 2020-2022.  Canonical form uses singular (as in 2019).

  # --- Name variant ---
  "Pension plans"                       = "Pensions",
  # Raw: 2020-2022.  Canonical form uses the 2019 name.

  # --- Category split (2020+) -> merged back to 2015-2019 combined category ---
  "Foreign governments - integral part"   = "Governments and international organizations",
  "Foreign governments - controlled entity" = "Governments and international organizations",
  "International organizations"           = "Governments and international organizations"
  # Raw: 2020-2022.  In 2015-2019, these were reported as a single row.
)


# ---------------------------------------------------------------------------
# CANONICALIZATION FUNCTION
# ---------------------------------------------------------------------------

canonicalize_entity_names <- function(df, col = "recipient_type") {
  # Step 1: Strip footnote markers like " [1]", " [2]"
  df[[col]] <- gsub("\\s*\\[\\d+\\]", "", df[[col]])

  # Step 2: Apply the explicit name mapping
  mapped <- entity_name_mapping[df[[col]]]
  df[[col]] <- ifelse(is.na(mapped), df[[col]], mapped)

  # Step 3: Aggregate rows where multiple raw names mapped to the same
  # canonical name (e.g. the three government/intl org rows in 2020-2022).
  # Group by the entity name column + year, sum all numeric columns.
  df <- df %>%
    group_by(across(all_of(c(col, "year")))) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
              .groups = "drop")

  return(df)
}
