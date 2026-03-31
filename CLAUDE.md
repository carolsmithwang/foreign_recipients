# Foreign Recipients of U.S. Income — Project Guide

## Table of Contents

- [Overview](#overview)
- [Data Source](#data-source)
  - [About the IRS SOI Foreign Recipients Study](#about-the-irs-soi-foreign-recipients-study)
  - [Income Types Reported](#income-types-reported)
  - [Recipient Types](#recipient-types)
  - [Withholding Rules](#withholding-rules)
  - [How to Access the Raw Data](#how-to-access-the-raw-data)
- [Research Narrative](#research-narrative)
- [Project Structure](#project-structure)
  - [Key files](#key-files)
- [Analysis Pipeline](#analysis-pipeline)
  - [1. Data Ingestion](#1-data-ingestion)
  - [2. Data Cleaning — Corporate Analysis](#2-data-cleaning--corporate-analysis)
  - [3. Data Cleaning — Entity Type Analysis (WFP)](#3-data-cleaning--entity-type-analysis-wfp)
  - [Entity Name Canonicalization](#entity-name-canonicalization)
  - [4. Visualizations](#4-visualizations)
  - [5. Regression Models](#5-regression-models)
- [R Dependencies](#r-dependencies)
- [Notes for Future Work](#notes-for-future-work)

## Overview

This project analyzes IRS Statistics of Income (SOI) data on foreign recipients of U.S.-source income, drawn from Form 1042-S filings. The central research question is whether the **Tax Cuts and Jobs Act (TCJA) of 2017** had a measurable effect on withholding patterns, particularly for tax-haven jurisdictions and withholding foreign partnerships.

The analysis covers tax years **2015–2022** and is written entirely in R.

## Data Source

All data comes from the IRS Statistics of Income (SOI) division's **Foreign Recipients of U.S. Income** study:

- **Main page:** https://www.irs.gov/statistics/soi-tax-stats-foreign-recipients-of-us-income-statistics
- **Terms and concepts:** https://www.irs.gov/statistics/soi-tax-stats-foreign-recipients-of-us-income-study-terms-and-concepts
- **Data sources and limitations:** https://www.irs.gov/statistics/soi-tax-stats-foreign-recipients-of-us-income-study-data-sources-and-limitations

### About the IRS SOI Foreign Recipients Study

The IRS SOI division publishes aggregate statistics derived from **Form 1042-S** (Foreign Person's U.S. Source Income Subject to Withholding). Form 1042-S is filed by withholding agents — typically banks, investment firms, and other financial intermediaries — to report payments of U.S.-source income made to foreign persons (nonresident individuals, foreign corporations, foreign partnerships, foreign trusts, and foreign estates).

The SOI study provides several tables, of which **Table 2** is the primary source used in this project. Table 2 breaks down the data by **selected country** and **recipient type**, reporting:

- Number of Forms 1042-S filed
- Total U.S.-source income
- U.S. tax withheld
- Income subject to withholding vs. exempt from withholding
- Income by category (interest, dividends, rents/royalties, social security, personal services income, notional principal contracts, and — in later years — return on capital)

### Income Types Reported

The principal categories of U.S.-source income on Form 1042-S include:

- **Interest income** — payments on deposits, bonds, and other debt obligations
- **Dividends** — distributions from U.S. corporations
- **Rents and royalties** — payments for use of property or intellectual property
- **Social security benefits** — payments to foreign recipients of U.S. social security
- **Personal services income** — compensation for services performed in the U.S.
- **Notional principal contracts** — income from swaps and similar derivative instruments
- **Return on capital** — (available in 2021–2022 data only)

### Recipient Types

The data categorizes foreign recipients into types including: individuals, corporations, partnerships and trusts, tax-exempt organizations, withholding foreign partnerships and trusts, U.S. branches treated as U.S. persons, hybrid entities making treaty claims, and various withholding rate pool categories.

### Withholding Rules

Most U.S.-source income paid to foreign persons is subject to a **30% statutory withholding rate**. Reduced rates (including full exemption) can apply when a tax treaty exists between the recipient's country and the U.S., when income is portfolio interest, when income is effectively connected with a U.S. trade or business, or when payments flow through withholding foreign partnerships.

### How to Access the Raw Data

The Excel/XLS files are hosted at URLs following the pattern:
```
https://www.irs.gov/pub/irs-soi/{YY}it02tc.xlsx
```
where `{YY}` is the two-digit tax year (e.g., `22it02tc.xlsx` for 2022). Earlier years use the `.xls` extension. The R code in this project downloads these files automatically if they are not already present locally.

## Research Narrative

The presentation (`Direct lending funds before and after TCJA 10-10-25.pptx`) lays out the full research story, focused on **direct lending fund structures**:

**Pre-TCJA structure ("season-and-sell"):** Direct lending funds typically used an offshore feeder entity (often a Cayman corporation) to hold seasoned loans. Interest income flowed to this Cayman corporate entity. This structure served ECI-sensitive and UBTI-sensitive limited partners.

**Post-TCJA structure ("leveraged blocker"):** After the TCJA cut the corporate tax rate from 35% to 21%, funds shifted to using a **U.S. leveraged blocker corporation**. The lower corporate rate made it worthwhile to interpose a U.S. corporate entity that converts business income into investment income. Interest income now flows through a U.S. blocker rather than an offshore Cayman corporation.

**Hypotheses tested:**
- Interest income paid to foreign corporations (especially Cayman) should decrease after TCJA, since the blocker is now domestic.
- The number of Forms 1042-S filed for Cayman entities may change, depending on whether the post-TCJA structure reports forms to the Cayman partnership or to its underlying partners.

**Key findings from the presentation:**
- **Hypothesis A (entity data):** Controlling for total U.S.-source income, the share of interest paid to foreign corporations decreased by seven percent after TCJA with statistical significance. However, when controlling for share of total U.S.-source income or share of non-interest income instead, the decrease shrinks to 1.4% or 0.9% and loses significance.
- **Hypothesis B (country data):** Interest income to Cayman entities actually increased in levels after TCJA, but this may reflect that both corporations and partnerships are included in the country-level data. Significance disappears in log specifications, which better capture relative (percentage) changes given Cayman's disproportionately large share of interest income.
- **Forms 1042-S results:** Number of forms to Cayman entities increased in levels but significance disappeared in log specifications. Possible explanation: post-TCJA forms may still be reported to the Cayman partnership (if it elected WFP status) rather than to individual partners, so the count wouldn't change.
- **WFP result:** Forms 1042-S documenting payments to withholding foreign partnerships increased by approximately 1.8x (e^0.59) after TCJA with statistical significance, which may reflect the growing size of investment funds (larger funds can afford WFP election and withholding procedures).

## Project Structure

```
.
├── Direct lending funds before and           # Research presentation (31 slides)
│   after TCJA 10-10-25.pptx
├── data/                                   # Downloaded IRS Excel files
│   ├── {YY}it02tc.xls[x]                  #   Table 2 data (withholding by country/type)
│   └── {YY}pa02.xls[x]                    #   Additional SOI partnership data
├── analysis/                               # Primary working directory
│   ├── foreign_recipients_analysis.R       #   Analysis script (most developed version)
│   ├── canonicalize_entity_names.R         #   Entity name mapping & function
│   ├── Rplot.jpeg                          #   Generic plot export
│   ├── Total U.S. Source Income by Country.jpeg
│   ├── Total U.S. tax withheld by country.jpeg
│   ├── *.pdf                               #   Exported plot PDFs
│   └── (RStudio project files)
├── archive/                                # Older script versions
│   ├── 1042s_analysis.R                    #   Earlier copy (missing ForCorp, share regressions)
│   └── 1042s_analysis.RData               #   Saved R workspace from archived script
├── plots/                                  # Exported visualizations
│   ├── ETR for Certain Types of Entities.jpeg
│   ├── Forms 1042S by country over time.pdf
│   └── Interest income by country over time.pdf
└── README.md
```

### Key files

- **`analysis/foreign_recipients_analysis.R`** — The most complete version of the analysis script. Contains all data loading, cleaning, visualization, and regression code, including the ForCorp (foreign corporation) regressions, share-based regressions, and additional country-level plots not present in the root-level copy.
- **`analysis/canonicalize_entity_names.R`** — Standalone module defining the `entity_name_mapping` (a named character vector mapping raw IRS entity names to canonical forms) and the `canonicalize_entity_names()` function. Sourced by both R analysis scripts. This is the single source of truth for entity name normalization — edit this file to add or change mappings.
- **`Direct lending funds before and after TCJA 10-10-25.pptx`** — A 31-slide presentation summarizing the research. Covers pre- and post-TCJA fund structures, hypotheses, diff-in-diff methodology, regression results (Hypotheses A and B for interest income, Forms 1042-S results, and WFP analysis), and supporting plots. Includes embedded screenshots of regression output and ggplot visualizations.
- **`data/`** — Contains the raw Excel files downloaded from the IRS. Files prefixed `it02tc` are Table 2 (withholding by country and recipient type). Files prefixed `pa02` appear to be SOI partnership data.

## Analysis Pipeline

### 1. Data Ingestion

The `read_irs_withholding()` function downloads Excel files from the IRS website (caching them locally to avoid re-downloading) and reads them with `readxl::read_excel()`. Each year's data requires slightly different row-skipping and column selection because the IRS changed formatting between years (e.g., column names differ between 2015–2016 and 2017+, and the "return on capital" column only appears in 2021–2022).

### 2. Data Cleaning — Corporate Analysis

For the country-level corporate analysis, the script:
- Removes header/summary rows specific to each year
- Renames columns to consistent names
- Extracts a `country` column from the repeating row structure (every 4th row is a country header; the rows in between are recipient types)
- Filters to keep only "Corporations" as the recipient type
- Selects key columns: recipient type, number of forms, interest income, total U.S.-source income, U.S. tax withheld, country, and year

### 3. Data Cleaning — Entity Type Analysis (WFP)

A second data pipeline extracts the **aggregate entity-type rows** (not broken out by country) to compare across recipient types: individuals, corporations, partnerships and trusts, withholding foreign partnerships (WFP), tax-exempt organizations, etc. This enables analysis of whether WFPs behaved differently after TCJA.

### Entity Name Canonicalization

The IRS changed entity-type labels across years in several ways. The R code canonicalizes these so that names are consistent for time-series analysis and regression. The mapping is defined in **`canonicalize_entity_names.R`** as a named vector (`entity_name_mapping`) and applied via the `canonicalize_entity_names()` function, which both analysis scripts source. The table below documents every raw name that appears in the data, its canonical form, and which years it appears.

**Stable names (no changes needed):**

| Canonical Name | Years |
|---|---|
| Individuals | 2015–2022 |
| Corporations | 2015–2022 |
| Partnerships and trusts | 2015–2022 |
| U.S. branches treated as U.S. persons | 2015–2022 |
| Withholding foreign partnerships and trusts | 2015–2022 |
| Withholding rate pools (general) | 2015–2022 |
| Withholding rate pools (tax-exempt) | 2015–2022 |
| Other and unknown | 2015–2022 |

**Footnote-only variations** (resolved by stripping `[N]` suffixes):

| Raw Name | Canonical Name | Years |
|---|---|---|
| Artists and athletes | Artists and athletes | 2015–2016 |
| Artists and athletes [1] | Artists and athletes | 2019–2020 |
| Artists and athletes [2] | Artists and athletes | 2017–2018, 2021–2022 |
| Tax-exempt organizations [1] | Tax-exempt organizations | 2015–2018 |

**Singular/plural variation:**

| Raw Name | Canonical Name | Years |
|---|---|---|
| Hybrid entity making treaty claim | Hybrid entity making treaty claim | 2019 |
| Hybrid entities making treaty claim | Hybrid entity making treaty claim | 2020–2022 |

**Name variant:**

| Raw Name | Canonical Name | Years |
|---|---|---|
| Pensions | Pensions | 2019 |
| Pension plans | Pensions | 2020–2022 |

**Category split** (most significant change):

In 2015–2019, a single row called `Governments and international organizations` covered all government and international organization recipients. Starting in 2020, this was split into three separate rows:

| Raw Name | Canonical Name | Years |
|---|---|---|
| Governments and international organizations | Governments and international organizations | 2015–2019 |
| Foreign governments - integral part | Governments and international organizations | 2020–2022 |
| Foreign governments - controlled entity | Governments and international organizations | 2020–2022 |
| International organizations | Governments and international organizations | 2020–2022 |

The R code merges these three 2020+ rows back into the combined category and then aggregates (sums) the numeric columns so each year has one row per canonical type.

**Disappeared category:**

`Tax-exempt organizations` appears only in 2015–2018. It has no direct successor in 2019+. New categories `Hybrid entity making treaty claim` and `Pensions` first appeared in 2019, but these are not relabelings of tax-exempt organizations — they represent genuinely different recipient types. Any analysis filtering on tax-exempt organizations will only have data for four years.

### 4. Visualizations

The project generates `ggplot2` plots including:
- Forms 1042-S filed over time, faceted by country (log and non-log)
- Interest income paid to foreign corporations over time, faceted by country (log and non-log)
- Total U.S.-source income by country over time (log and non-log)
- Total U.S. tax withheld by country over time (log and non-log)
- Log and non-log versions of interest, forms count, total income, and tax withheld by entity type
- Effective withholding tax rates (tax withheld / total income) by entity type
- Share of total interest by entity type over time

### 5. Regression Models

The core empirical strategy uses OLS regressions with a **difference-in-differences–style** design to test whether the TCJA (effective 2018) differentially affected certain entity types or jurisdictions.

**Key variables:**
- `afchange` / `af_change` — binary indicator: 0 for pre-TCJA years (2015–2017), 1 for post-TCJA years (2018–2020 or 2018+)
- `tax_haven` — binary indicator for Bermuda, British Virgin Islands, Cayman Islands, and Hong Kong
- `cayman` — binary indicator specifically for the Cayman Islands
- `WFP` — binary indicator for withholding foreign partnerships and trusts
- `ForCorp` — binary indicator for foreign corporations
- `int_share_of_total_income` — interest as a fraction of total U.S.-source income for each entity type
- `share_of_total_interest` — each entity type's share of aggregate interest across all entity types in a given year
- `share_of_total_income` — each entity type's share of aggregate total income across all entity types in a given year
- `share_of_nonint_inc` — each entity type's share of aggregate non-interest income (used as an alternative control)

**Regression families:**

| Outcome | Treatment interaction | Specification |
|---|---|---|
| Number of Forms 1042-S | `afchange * tax_haven` | levels and log |
| Number of Forms 1042-S | `afchange * cayman` | levels and log |
| Interest income | `afchange * tax_haven` | levels and log |
| Interest income | `afchange * cayman` | levels and log |
| Interest income | `af_change * WFP` | levels and log |
| Interest income | `af_change * ForCorp` | levels and log |
| Number of Forms 1042-S | `af_change * WFP` | levels and log |
| Number of Forms 1042-S | `af_change * ForCorp` | levels and log |
| Interest share of total income | `af_change * ForCorp` | levels |
| Share of total interest | `af_change * ForCorp` | levels (with share_of_total_income control) |
| Share of total interest | `af_change * ForCorp` | levels (with share_of_nonint_inc control) |

All regressions control for year, total U.S.-source income (or share equivalents), and country or recipient type fixed effects.

## R Dependencies

- `readxl` — reading Excel files
- `tidyverse` (includes `ggplot2`, `dplyr`, `tidyr`) — data manipulation and visualization
- `janitor` — column name cleaning (`clean_names()`)

## Notes for Future Work

- The earlier R script (`archive/1042s_analysis.R`) is missing the ForCorp indicator, share-based variables, and several regressions. The primary version is `analysis/foreign_recipients_analysis.R`.
- The `pa02` files in `data/` (SOI partnership data) are not currently used in the analysis scripts but could support additional research on partnership structures.
- Year-to-year formatting differences in the IRS spreadsheets require manual adjustment of row-skip counts and column indices. Adding a new year of data will likely require inspecting the new file's layout.
- The `afchange` variable in the country-level analysis is `NA` for 2021–2022, effectively excluding those years from the diff-in-diff regressions. The entity-type analysis (`af_change`) includes all post-2017 years.
