# foreign_recipients_analysis.R
# =============================================================================
# Analysis of IRS SOI data on foreign recipients of U.S.-source income
# (Form 1042-S, Table 2), tax years 2015-2022.
#
# Research question: Did the TCJA (2017) measurably affect withholding patterns
# for tax-haven jurisdictions and withholding foreign partnerships?
#
# See CLAUDE.md for full project documentation.
# =============================================================================

library(readxl)
library(tidyverse)
library(janitor)

source("canonicalize_entity_names.R")

# =============================================================================
# 1. DATA LOADING
# =============================================================================

read_irs_withholding <- function(url, year, skip = 4, data_dir = "../data") {
  filename <- basename(url)
  filepath <- file.path(data_dir, filename)
  # Download to data/ directory if not already cached
  if (!file.exists(filepath)) {
    download.file(url, filepath, mode = "wb")
  }
  data <- read_excel(filepath, sheet = 1, skip = skip)
  data <- data %>%
    mutate(year = year,
           `Number of Forms 1042S` = as.numeric(`Number of Forms 1042S`),
           `Principal types of U.S.-source income` = as.numeric(`Principal types of U.S.-source income`))
  return(data)
}

# Per-year configuration for data loading.
# The IRS changed column names and row counts across years:
#   - 2015-2016: column header is "Selected country and selected recipient type"
#   - 2017-2019: "Recipient types and selected country and selected recipient type"
#   - 2020:      same as 2017-2019, but entity-type section has more rows (govt split)
#   - 2021-2022: "Recipient types and selected country [1] and selected recipient type"
#                and an extra column (return on capital)
year_config <- tribble(
  ~year, ~url,                                                ~corp_skip, ~wfp_end, ~has_return_on_capital,
  2015,  "https://www.irs.gov/pub/irs-soi/15it02tc.xls",     18,         18,       FALSE,
  2016,  "https://www.irs.gov/pub/irs-soi/16it02tc.xls",     18,         18,       FALSE,
  2017,  "https://www.irs.gov/pub/irs-soi/17it02tc.xls",     18,         18,       FALSE,
  2018,  "https://www.irs.gov/pub/irs-soi/18it02tc.xlsx",    18,         18,       FALSE,
  2019,  "https://www.irs.gov/pub/irs-soi/19it02tc.xlsx",    19,         19,       FALSE,
  2020,  "https://www.irs.gov/pub/irs-soi/20it02tc.xlsx",    21,         21,       FALSE,
  2021,  "https://www.irs.gov/pub/irs-soi/21it02tc.xlsx",    21,         21,       TRUE,
  2022,  "https://www.irs.gov/pub/irs-soi/22it02tc.xlsx",    21,         21,       TRUE
)

# =============================================================================
# 2. CORPORATE ANALYSIS (by country)
# =============================================================================
# Extracts "Corporations" rows from the country-level section of each year's
# Table 2 data, producing a panel of corporate interest income, forms filed,
# and tax withheld by country and year.

clean_corporate_data <- function(raw, year, corp_skip, has_return_on_capital) {
  # Find the recipient_type column (name varies by year)
  col1 <- names(raw)[1]

  data <- raw %>%
    slice(-(1:corp_skip)) %>%
    rename(Interest_income = `Principal types of U.S.-source income`,
           recipient_type = !!sym(col1))

  # Extract country from the repeating row structure (every 4th row is a country header)
  data <- data %>%
    mutate(country = ifelse(row_number() %% 4 == 1, recipient_type, NA)) %>%
    fill(country, .direction = "down")

  # Select columns: 1-5 are core fields, then country + year.
  # 2021-2022 have an extra column (return on capital) that shifts the indices.
  if (has_return_on_capital) {
    data <- data[, c(1:5, 12:13)]
  } else {
    data <- data[, c(1:5, 11:12)]
  }

  data %>% filter(recipient_type == "Corporations")
}

corp_data_list <- pmap(year_config, function(year, url, corp_skip, wfp_end, has_return_on_capital) {
  raw <- read_irs_withholding(url, year)
  clean_corporate_data(raw, year, corp_skip, has_return_on_capital)
})

wh_data <- bind_rows(corp_data_list)

# Filter to selected countries of interest
selected_countries <- c("Australia", "Bermuda", "British Virgin Islands", "Canada",
                        "Cayman Islands", "China", "France", "Germany",
                        "Hong Kong", "Mexico", "United Kingdom")

filtered_data <- wh_data %>%
  filter(country %in% selected_countries) %>%
  clean_names() %>%
  mutate(u_s_tax_withheld = as.numeric(u_s_tax_withheld),
         total_u_s_source_income = as.numeric(total_u_s_source_income))

# Treatment indicators
filtered_data <- filtered_data %>%
  mutate(
    tax_haven = ifelse(country %in% c("Bermuda", "British Virgin Islands",
                                       "Cayman Islands", "Hong Kong"), 1, 0),
    afchange  = ifelse(year %in% 2015:2017, 0, ifelse(year %in% 2018:2020, 1, NA)),
    cayman    = ifelse(country == "Cayman Islands", 1, 0)
  )

# =============================================================================
# 3. ENTITY-TYPE ANALYSIS (WFP / ForCorp)
# =============================================================================
# Extracts the aggregate entity-type rows (not broken out by country) to
# compare across recipient types: individuals, corporations, WFPs, etc.

clean_wfp_data <- function(raw, year, wfp_end, has_return_on_capital) {
  col1 <- names(raw)[1]

  # Keep only the entity-type summary rows (after first 6 header rows,
  # before country-level data starts)
  data <- raw %>% slice(-c(1:6, wfp_end:nrow(raw)))

  # Rename columns to consistent names
  rename_map <- c(
    recipient_type = col1,
    interest = "Principal types of U.S.-source income",
    dividends = "...6",
    rents_royalties = "...7",
    socialsecurity = "...8",
    personal_services_income = "...9",
    notional_principal_contract = "...10"
  )
  if (has_return_on_capital) {
    rename_map <- c(rename_map, return_on_capital = "...11")
  }
  data <- data %>% rename(!!!rename_map)

  data <- clean_names(data)

  # Drop any extra unnamed columns (e.g. x11, x12 in 2019)
  data <- data %>% select(-starts_with("x"))

  return(data)
}

wfp_data_list <- pmap(year_config, function(year, url, corp_skip, wfp_end, has_return_on_capital) {
  raw <- read_irs_withholding(url, year)
  clean_wfp_data(raw, year, wfp_end, has_return_on_capital)
})

data_WFP_all <- bind_rows(wfp_data_list) %>%
  mutate(u_s_tax_withheld = as.numeric(u_s_tax_withheld),
         total_u_s_source_income = as.numeric(total_u_s_source_income))

# Canonicalize entity names (handles footnote markers, name variants, category splits)
data_WFP_all <- canonicalize_entity_names(data_WFP_all, col = "recipient_type")

# Treatment and group indicators
data_WFP_all <- data_WFP_all %>%
  mutate(
    af_change = ifelse(year <= 2017, 0, 1),
    WFP       = ifelse(recipient_type == "Withholding foreign partnerships and trusts", 1, 0),
    ForCorp   = ifelse(recipient_type == "Corporations", 1, 0)
  )

# Subset excluding categories with gaps (Tax-exempt orgs: 2015-2018 only)
data_WFP_subset <- data_WFP_all %>%
  filter(recipient_type %in% c("Corporations", "Hybrid entity making treaty claim",
                                "Individuals", "Other and unknown",
                                "Partnerships and trusts",
                                "U.S. branches treated as U.S. persons",
                                "Withholding foreign partnerships and trusts",
                                "Withholding rate pools (general)",
                                "Withholding rate pools (tax-exempt)"))

# =============================================================================
# 4. COUNTRY-LEVEL PLOTS
# =============================================================================

# Forms 1042-S by country
ggplot(filtered_data, aes(x = year, y = number_of_forms_1042s)) +
  geom_line() + facet_wrap(~country) +
  labs(title = "Forms 1042S over Years by Country")

ggplot(filtered_data, aes(x = year, y = log(number_of_forms_1042s))) +
  geom_line() + facet_wrap(~country) +
  labs(title = "Log Forms 1042S over Years by Country")

# Interest income by country
ggplot(filtered_data, aes(x = year, y = interest_income)) +
  geom_line() + facet_wrap(~country) +
  labs(title = "Interest Income Paid to Corps over Years by Country")

ggplot(filtered_data, aes(x = year, y = log(interest_income))) +
  geom_line() + facet_wrap(~country) +
  labs(title = "Log Interest Income Paid to Corps over Years by Country")

# Total U.S. source income by country
ggplot(filtered_data, aes(x = year, y = total_u_s_source_income)) +
  geom_line() + facet_wrap(~country) +
  labs(title = "Total U.S. Source Income Paid to Corps by Country")

ggplot(filtered_data, aes(x = year, y = log(total_u_s_source_income))) +
  geom_line() + facet_wrap(~country) +
  labs(title = "Log Total U.S. Source Income Paid to Corps by Country")

# Tax withheld by country
ggplot(filtered_data, aes(x = year, y = u_s_tax_withheld)) +
  geom_line() + facet_wrap(~country) +
  labs(title = "Total U.S. tax withheld by Country")

ggplot(filtered_data, aes(x = year, y = log(u_s_tax_withheld))) +
  geom_line() + facet_wrap(~country) +
  labs(title = "Log Total U.S. tax withheld by Country")

# =============================================================================
# 5. ENTITY-TYPE PLOTS
# =============================================================================

# Entity types to include in filtered plots
main_entity_types <- c("Individuals", "Corporations", "Partnerships and trusts",
                        "Tax-exempt organizations",
                        "Withholding foreign partnerships and trusts")

# Interest by entity type
ggplot(data_WFP_all, aes(x = year, y = interest, color = recipient_type)) +
  geom_line() + geom_point(size = 3) +
  labs(title = "Non-Log Interest by Type of Foreign Entity",
       x = "Year", y = "Interest", color = "Type of Entity")

data_WFP_all %>% filter(recipient_type %in% main_entity_types) %>%
  ggplot(aes(x = year, y = log(interest), color = recipient_type)) +
  geom_line() + geom_point(size = 3) +
  labs(title = "Log Interest by Type of Foreign Entity",
       x = "Year", y = "Log Interest", color = "Type of Entity")

data_WFP_all %>%
  ggplot(aes(x = year, y = log(interest), color = recipient_type)) +
  geom_line() + facet_wrap(~recipient_type) +
  geom_point(size = 3) +
  labs(title = "Log Interest by Type of Foreign Entity",
       x = "Year", y = "Log Interest", color = "Type of Entity")

data_WFP_all %>% filter(recipient_type %in% main_entity_types) %>%
  ggplot(aes(x = year, y = interest, color = recipient_type)) +
  geom_line() + geom_point(size = 3) +
  labs(title = "Non-Log Interest by Type of Foreign Entity",
       x = "Year", y = "Interest", color = "Type of Entity")

# Forms 1042-S by entity type
data_WFP_all %>% filter(recipient_type %in% main_entity_types) %>%
  ggplot(aes(x = year, y = log(number_of_forms_1042s), color = recipient_type)) +
  geom_line() + geom_point(size = 3) +
  labs(title = "Log Number of Forms 1042s by Type of Foreign Entity",
       x = "Year", y = "Log Number of Forms 1042s", color = "Type of Entity")

data_WFP_all %>% filter(recipient_type %in% main_entity_types) %>%
  ggplot(aes(x = year, y = number_of_forms_1042s, color = recipient_type)) +
  geom_line() + geom_point(size = 3) +
  labs(title = "Non-Log Number of Forms 1042s by Type of Foreign Entity",
       x = "Year", y = "Number of Forms 1042s", color = "Type of Entity")

# Total U.S. source income by entity type (log and non-log)
data_WFP_all %>% filter(recipient_type %in% main_entity_types) %>%
  ggplot(aes(x = year, y = log(total_u_s_source_income), color = recipient_type)) +
  geom_smooth() + geom_point(size = 3) +
  labs(title = "Total Log U.S. Source Income By Type of Foreign Entity",
       x = "Year", y = "Total U.S. Source Income", color = "Type of Entity")

data_WFP_all %>% filter(recipient_type %in% main_entity_types) %>%
  ggplot(aes(x = year, y = total_u_s_source_income, color = recipient_type)) +
  geom_smooth() + geom_point(size = 3) +
  labs(title = "Total Non-Log U.S. Source Income to Certain Types of Entities",
       x = "Year", y = "Total U.S. Source Income", color = "Type of Entity")

# Tax withheld by entity type (log and non-log)
data_WFP_all %>% filter(recipient_type %in% main_entity_types) %>%
  ggplot(aes(x = year, y = log(u_s_tax_withheld), color = recipient_type)) +
  geom_line() + geom_point(size = 3) +
  labs(title = "U.S. Tax Withheld to Certain Types of Entities",
       x = "Year", y = "Total U.S. Source Income", color = "Type of Entity")

data_WFP_all %>% filter(recipient_type %in% main_entity_types) %>%
  ggplot(aes(x = year, y = u_s_tax_withheld, color = recipient_type)) +
  geom_line() + geom_point(size = 3) +
  labs(title = "U.S. Tax Withheld to Certain Types of Entities",
       x = "Year", y = "Total U.S. Source Income", color = "Type of Entity")

# Effective tax rates by entity type (log-ratio and level-ratio)
data_WFP_all %>% filter(recipient_type %in% main_entity_types) %>%
  ggplot(aes(x = year, y = log(u_s_tax_withheld) / log(total_u_s_source_income),
             color = recipient_type)) +
  geom_line() + geom_point(size = 3) +
  labs(title = "Effective Withholding Tax Rate by Type of Entities",
       x = "Year", y = "Effective Tax Rate", color = "Type of Entity")

data_WFP_all %>% filter(recipient_type %in% main_entity_types) %>%
  ggplot(aes(x = year, y = u_s_tax_withheld / total_u_s_source_income,
             color = recipient_type)) +
  geom_line() + geom_point(size = 3) +
  labs(title = "Effective Withholding Tax Rate by Type of Entities",
       x = "Year", y = "Effective Tax Rate", color = "Type of Entity")

# =============================================================================
# 6. COUNTRY-LEVEL REGRESSIONS (Hypotheses A & B)
# =============================================================================

# --- Forms 1042-S ---
# Tax haven interaction (levels and log)
forms_reg <- lm(number_of_forms_1042s ~ afchange + year + total_u_s_source_income +
                  country + afchange * tax_haven, data = filtered_data)
summary(forms_reg)

log_forms_reg <- lm(log(number_of_forms_1042s) ~ afchange + year +
                      log(total_u_s_source_income) + country + afchange * tax_haven,
                    data = filtered_data)
summary(log_forms_reg)

# Cayman interaction (levels and log)
forms_reg_cayman <- lm(number_of_forms_1042s ~ afchange + year + total_u_s_source_income +
                         country + afchange * cayman, data = filtered_data)
summary(forms_reg_cayman)

log_forms_reg_cayman <- lm(log(number_of_forms_1042s) ~ afchange + year +
                             log(total_u_s_source_income) + country + afchange * cayman,
                           data = filtered_data)
summary(log_forms_reg_cayman)

# --- Interest income ---
# Tax haven interaction (levels and log)
interest_reg <- lm(interest_income ~ afchange + year + total_u_s_source_income +
                     country + afchange * tax_haven, data = filtered_data)
summary(interest_reg)

log_interest_reg <- lm(log(interest_income) ~ afchange + year +
                         log(total_u_s_source_income) + country + afchange * tax_haven,
                       data = filtered_data)
summary(log_interest_reg)

# Cayman interaction (levels and log)
caymaninterest_reg <- lm(interest_income ~ afchange + year + total_u_s_source_income +
                           country + afchange * cayman, data = filtered_data)
summary(caymaninterest_reg)

log_caymaninterest_reg <- lm(log(interest_income) ~ afchange + year +
                               log(total_u_s_source_income) + country + afchange * cayman,
                             data = filtered_data)
summary(log_caymaninterest_reg)

# =============================================================================
# 7. ENTITY-TYPE REGRESSIONS (WFP and ForCorp)
# =============================================================================

# --- Log specifications ---
interest_WFP <- lm(log(interest) ~ af_change + year + log(total_u_s_source_income) +
                     recipient_type + af_change * WFP, data = data_WFP_all)
summary(interest_WFP)

interest_ForCorp <- lm(log(interest) ~ af_change + year + log(total_u_s_source_income) +
                         recipient_type + af_change * ForCorp, data = data_WFP_all)
summary(interest_ForCorp)

interest_ForCorp_subset <- lm(log(interest) ~ af_change + year + log(total_u_s_source_income) +
                                recipient_type + af_change * ForCorp, data = data_WFP_subset)
summary(interest_ForCorp_subset)

forms_reg_WFP <- lm(log(number_of_forms_1042s) ~ af_change + year +
                      log(total_u_s_source_income) + recipient_type + af_change * WFP,
                    data = data_WFP_all)
summary(forms_reg_WFP)

forms_reg_ForCorp <- lm(log(number_of_forms_1042s) ~ af_change + year +
                          log(total_u_s_source_income) + recipient_type + af_change * ForCorp,
                        data = data_WFP_all)
summary(forms_reg_ForCorp)

forms_reg10 <- lm(log10(number_of_forms_1042s) ~ af_change + year +
                    log10(total_u_s_source_income) + recipient_type + af_change * WFP,
                  data = data_WFP_all)
summary(forms_reg10)

# --- Level specifications ---
interest_WFP_lev <- lm(interest ~ af_change + year + total_u_s_source_income +
                         recipient_type + af_change * WFP, data = data_WFP_all)
summary(interest_WFP_lev)

interest_ForCorp_lev <- lm(interest ~ af_change + year + total_u_s_source_income +
                             recipient_type + af_change * ForCorp, data = data_WFP_all)
summary(interest_ForCorp_lev)

interest_ForCorp_subset_lev <- lm(interest ~ af_change + year + total_u_s_source_income +
                                    recipient_type + af_change * ForCorp, data = data_WFP_subset)
summary(interest_ForCorp_subset_lev)

forms_reg_WFP_lev <- lm(number_of_forms_1042s ~ af_change + year + total_u_s_source_income +
                          recipient_type + af_change * WFP, data = data_WFP_all)
summary(forms_reg_WFP_lev)

forms_reg_ForCorp_lev <- lm(number_of_forms_1042s ~ af_change + year + total_u_s_source_income +
                              recipient_type + af_change * ForCorp, data = data_WFP_all)
summary(forms_reg_ForCorp_lev)

# =============================================================================
# 8. SHARE-BASED REGRESSIONS (ForCorp)
# =============================================================================

data_WFP_all <- data_WFP_all %>%
  mutate(int_share_of_total_income = interest / total_u_s_source_income) %>%
  group_by(year) %>%
  mutate(share_of_total_interest = interest / sum(interest, na.rm = TRUE),
         share_of_total_income = total_u_s_source_income / sum(total_u_s_source_income, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(non_int_income = total_u_s_source_income - interest) %>%
  group_by(year) %>%
  mutate(share_of_nonint_inc = non_int_income / sum(non_int_income, na.rm = TRUE)) %>%
  ungroup()

# Interest share of total income ~ ForCorp
ForCorp_int_total <- lm(int_share_of_total_income ~ af_change + year + total_u_s_source_income +
                          recipient_type + af_change * ForCorp, data = data_WFP_all)
summary(ForCorp_int_total)

# Share of total interest ~ ForCorp (controlling for share of total income)
ForCorp_share_of_int <- lm(share_of_total_interest ~ af_change + year + share_of_total_income +
                             recipient_type + af_change * ForCorp, data = data_WFP_all)
summary(ForCorp_share_of_int)

# Share of total interest ~ ForCorp (controlling for share of non-interest income)
ForCorp_share_of_int_alt <- lm(share_of_total_interest ~ af_change + year + share_of_nonint_inc +
                                 recipient_type + af_change * ForCorp, data = data_WFP_all)
summary(ForCorp_share_of_int_alt)

# Share of total interest plots
data_WFP_all %>% filter(recipient_type %in% main_entity_types) %>%
  ggplot(aes(x = year, y = share_of_total_interest, color = recipient_type)) +
  geom_smooth() + geom_point(size = 3) +
  labs(title = "Share of total interest by type of foreign entity",
       x = "Year", y = "Share of total interest", color = "Type of Entity")

data_WFP_all %>%
  ggplot(aes(x = year, y = share_of_total_interest, color = recipient_type)) +
  geom_smooth() + geom_point(size = 3) +
  scale_y_log10() +
  labs(title = "Log share of total interest by type of foreign entity",
       x = "Year", y = "Log share of total interest", color = "Type of Entity")

# =============================================================================
# 9. EXPORT CLEANED DATA
# =============================================================================

write_csv(filtered_data, "filtered_data.csv")
write_csv(data_WFP_all, "data_WFP_all.csv")
write_csv(data_WFP_subset, "data_WFP_subset.csv")
