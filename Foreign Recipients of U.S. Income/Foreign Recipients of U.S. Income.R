#https://www.irs.gov/pub/irs-soi/89fo02fr.xls
#https://www.irs.gov/pub/irs-soi/22it02tc.xlsx

library(readxl)
library(tidyverse)

read_irs_withholding <- function(url, year, skip=4) {
  filename <- basename(url)
# Download the file to the current directory, to save for future use
# Because we source all commands and download all repositories each time we hit source, the below makes sure we only download when necessary
  if (!(file.exists(filename))) {
    # If we have already downloaded the file into the current directory, no need to download it again
    download.file(url, filename, mode = "wb")
  }
  data <- read_excel(filename, sheet = 1, skip=skip)
  data <- data %>% mutate(year = year)
  data <- data %>% mutate(`Number of Forms 1042S`= as.numeric(`Number of Forms 1042S`))
  data <- data %>% mutate (`Principal types of U.S.-source income` = as.numeric(`Principal types of U.S.-source income`))
  
  return(data)
}

url_wh_22 <- "https://www.irs.gov/pub/irs-soi/22it02tc.xlsx"

#further remove rows 5-25 except for 20
data_22 <- read_irs_withholding(url_wh_22, 2022)
data_22 <- data_22 %>% slice(-c(1:21))

#rename "principal types of U.S.-source income" and recipient type
data_22 <- data_22 %>% rename(
  Interest_income= `Principal types of U.S.-source income`,
  recipient_type = `Recipient types and selected country [1] and selected recipient type`)

#add a new column, new column will be called country
#take data 22 and first mutate it to create a new column named country and then fill in the missing values
#go through every row and for every multiple of four, put in recipient_type, and if it's not a multiple of four, fill in missing
#fill that goes into each missing NA value and replaces it with the nearest entry above with a value
data_22 <- data_22 %>%
  mutate(  
      country = ifelse(row_number() %% 4==1, recipient_type, NA)
    ) %>%
  fill(country, .direction="down")

#select only first five columns and 12th column
#Explanation: 1:5 selects columns 1 to 5, 11 selects the 11th column, c(1:5, 11) combines them into one vector of column indices
  data_22 <- data_22[, c(1:5,12:13)]
  
#select row 3 and every fourth row after that until row 363.  seq(3, 363, by = 4) generates the sequence: 3, 7, 11, ..., 363.  data_22[...] selects those rows.  
  #The comma , means you're selecting all columns of those rows
  #data_22 <- data_22[seq(3,363, by = 4),]
  # alternatively and equivalently, just keep the rows with type "Corporations"
  data_22 <- data_22 %>% filter(
    recipient_type == "Corporations"
  )
  
#BREAK FOR 2021
url_wh_21 <- "https://www.irs.gov/pub/irs-soi/21it02tc.xlsx"
data_21 <- read_irs_withholding(url_wh_21, 2021)
data_21 <- data_21 %>% slice(-(1:21))
data_21 <- data_21 %>% rename(
  Interest_income= `Principal types of U.S.-source income`,
  recipient_type = `Recipient types and selected country [1] and selected recipient type`)
data_21 <- data_21 %>%
  mutate(
    country = ifelse(row_number() %% 4==1, recipient_type, NA)) %>%
    fill(country, .direction="down")
data_21 <- data_21[, c(1:5,12:13)]
data_21 <- data_21 %>% filter(
  recipient_type == "Corporations"
)

#BREAK FOR 2020
url_wh_20 <- "https://www.irs.gov/pub/irs-soi/20it02tc.xlsx"
data_20 <- read_irs_withholding(url_wh_20, 2020)
data_20 <- data_20 %>% slice(-(1:21))
data_20 <- data_20 %>% rename(
  Interest_income= `Principal types of U.S.-source income`,
  recipient_type = `Recipient types and selected country and selected recipient type`)
data_20 <- data_20 %>%
  mutate(
    country = ifelse(row_number() %% 4==1, recipient_type, NA)) %>%
  fill(country, .direction="down")
#2020 data had one less column (did not have return to capital) than 2021-2022
data_20 <- data_20[, c(1:5,11:12)]
data_20 <- data_20 %>% filter(
  recipient_type == "Corporations"
)

#BREAK FOR 2019
url_wh_19 <- "https://www.irs.gov/pub/irs-soi/19it02tc.xlsx"
data_19 <- read_irs_withholding(url_wh_19, 2019)
#further remove rows 5-23, instead of rows 5-25
data_19 <- data_19 %>% slice(-(1:19))
data_19 <- data_19 %>% rename(
  Interest_income= `Principal types of U.S.-source income`,
  recipient_type = `Recipient types and selected country and selected recipient type`)
data_19 <- data_19 %>%
  mutate(
    country = ifelse(row_number() %% 4==1, recipient_type, NA)) %>%
  fill(country, .direction="down")
#2019 data had one less column (did not have return to capital) than 2021-2022
data_19 <- data_19[, c(1:5,11:12)]
data_19 <- data_19 %>% filter(
  recipient_type == "Corporations"
)

#BREAK FOR 2018
url_wh_18 <- "https://www.irs.gov/pub/irs-soi/18it02tc.xlsx"
data_18 <- read_irs_withholding(url_wh_18, 2018)
#further remove rows 5-22, instead of rows 5-23
data_18 <- data_18 %>% slice(-(1:18))
data_18 <- data_18 %>% rename(
  Interest_income = `Principal types of U.S.-source income`,
  recipient_type = `Recipient types and selected country and selected recipient type`)
data_18 <- data_18 %>%
  mutate(
    country = ifelse(row_number() %% 4==1, recipient_type, NA)) %>%
    fill(country, .direction="down")
#2018 data had one less column (did not have return to capital) than 2021-2022
data_18 <- data_18[, c(1:5,11:12)]
data_18 <- data_18 %>% filter(
  recipient_type == "Corporations"
)

#BREAK FOR 2017
url_wh_17 <- "https://www.irs.gov/pub/irs-soi/17it02tc.xls"
data_17 <- read_irs_withholding(url_wh_17, 2017)
#further remove rows 5-22, instead of rows 5-23
data_17 <- data_17 %>% slice(-(1:18))
data_17 <- data_17 %>% rename(
  Interest_income = `Principal types of U.S.-source income`,
  recipient_type = `Recipient types and selected country and selected recipient type`)
data_17 <- data_17 %>%
  mutate(
    country = ifelse(row_number() %% 4==1, recipient_type, NA)) %>%
  fill(country, .direction="down")
#2017 data had one less column (did not have return to capital) than 2021-2022
data_17 <- data_17[, c(1:5,11:12)]
data_17 <- data_17 %>% filter(
  recipient_type == "Corporations"
)

#BREAK FOR 2016
url_wh_16 <- "https://www.irs.gov/pub/irs-soi/16it02tc.xls"
data_16 <- read_irs_withholding(url_wh_16, 2016)
#further remove rows 5-22, instead of rows 5-23
data_16 <- data_16 %>% slice(-(1:18))
data_16 <- data_16 %>% rename(
  Interest_income = `Principal types of U.S.-source income`,
  recipient_type = `Selected country and selected recipient type`)
data_16 <- data_16 %>%
  mutate(
    country = ifelse(row_number() %% 4==1, recipient_type, NA)) %>%
  fill(country, .direction="down")
#2016 data had one less column (did not have return to capital) than 2021-2022
data_16 <- data_16[, c(1:5,11:12)]
data_16 <- data_16 %>% filter(
  recipient_type == "Corporations"
)

#BREAK FOR 2015
url_wh_15 <- "https://www.irs.gov/pub/irs-soi/15it02tc.xls"
data_15 <- read_irs_withholding(url_wh_15, 2015)
#further remove rows 5-22, instead of rows 5-23
data_15 <- data_15 %>% slice(-(1:18))
data_15 <- data_15 %>% rename(
  Interest_income = `Principal types of U.S.-source income`,
  recipient_type = `Selected country and selected recipient type`)
data_15 <- data_15 %>%
  mutate(
    country = ifelse(row_number() %% 4==1, recipient_type, NA)) %>%
  fill(country, .direction="down")
#2015 data had one less column (did not have return to capital) than 2021-2022
data_15 <- data_15[, c(1:5,11:12)]
data_15 <- data_15 %>% filter(
  recipient_type == "Corporations"
)

#Combine into one dataframehttp://127.0.0.1:30295/graphics/396dfa42-80ef-4722-94f7-80e481a8f38e.png
wh_data <- bind_rows(data_15, data_16, data_17, data_18, data_19, data_20, data_21, data_22)

#Make a line plot with year on the x axis, forms on the y axis, and color by country
#ggplot(wh_data, aes(x = year, y = `Number of Forms 1042S`, color = country)) +
  #geom_line() +
  #labs(title = "Forms 1042S over Years by Country", color = "Country")

filtered_data <- wh_data %>% filter(country %in% c("Australia", "Bermuda", "British Virgin Islands", "Canada", "Cayman Islands", "China", "France", "Germany", "Hong Kong", "Mexico", "United Kingdom"))

library(janitor)

filtered_data <- janitor::clean_names(filtered_data)

filtered_data$u_s_tax_withheld <- as.numeric(filtered_data$u_s_tax_withheld)

filtered_data$total_u_s_source_income <- as.numeric(filtered_data$total_u_s_source_income)

ggplot(filtered_data, aes(x = year, y = number_of_forms_1042s)) +
  geom_line() +
  facet_wrap(~country) +
  labs(title = "Forms 1042S over Years by Country")

ggplot(filtered_data, aes(x = year, y = (log(number_of_forms_1042s)))) +
  geom_line() +
  facet_wrap(~country) +
  labs(title = "Log Forms 1042S over Years by Country")

ggplot(filtered_data, aes(x = year, y = interest_income)) +
  geom_line() +
  facet_wrap(~country) +
  labs(title = "Interest Income Paid to Corps over Years by Country")

ggplot(filtered_data, aes(x = year, y = log(interest_income))) +
  geom_line() +
  facet_wrap(~country) +
  labs(title = "Log Interest Income Paid to Corps over Years by Country")

ggplot(filtered_data, aes(x = year, y = total_u_s_source_income)) + 
  geom_line() + 
  facet_wrap(~country) + 
  labs(title = "Total U.S. Source Income Paid to Corps by Country")

ggplot(filtered_data, aes(x = year, y = log(total_u_s_source_income))) + 
  geom_line() + 
  facet_wrap(~country) + 
  labs(title = "Log Total U.S. Source Income Paid to Corps by Country")

ggplot(filtered_data, aes(x = year, y = u_s_tax_withheld)) + 
  geom_line() + 
  facet_wrap(~country) + 
  labs(title = "Total U.S. tax withheld by Country")

ggplot(filtered_data, aes(x = year, y = log(u_s_tax_withheld))) + 
  geom_line() + 
  facet_wrap(~country) + 
  labs(title = "Log Total U.S. tax withheld by Country")

filtered_data$tax_haven <- ifelse(filtered_data$country %in% c("Bermuda", "British Virgin Islands", "Cayman Islands", "Hong Kong"), 1, 0)

filtered_data$afchange <- ifelse(filtered_data$year %in% c(2015, 2016, 2017), 0, 
                                 ifelse(filtered_data$year %in% c(2018, 2019, 2020), 1, NA))

filtered_data$cayman <- ifelse(filtered_data$country=="Cayman Islands", 1, 0)

#FORMS

forms_reg <- lm(number_of_forms_1042s ~ afchange + year + total_u_s_source_income + country + afchange*tax_haven, data = filtered_data)

summary(forms_reg)

log_forms_reg <- lm(log(number_of_forms_1042s) ~ afchange + year + log(total_u_s_source_income) + country + afchange*tax_haven, data = filtered_data)

summary(log_forms_reg)

forms_reg_cayman <- lm(number_of_forms_1042s ~ afchange + year + total_u_s_source_income + country + afchange*cayman, data = filtered_data)

summary(forms_reg_cayman)

log_forms_reg_cayman <- lm(log(number_of_forms_1042s) ~ afchange + year + log(total_u_s_source_income) + country + afchange*cayman, data = filtered_data)

summary(log_forms_reg_cayman)

#INTERESTPAIDTOCAYMANCORPS

interest_reg <- lm(interest_income ~ afchange + year + total_u_s_source_income + country + afchange*tax_haven, data = filtered_data)

summary(interest_reg)

log_interest_reg <- lm(log(interest_income) ~ afchange + year + log(total_u_s_source_income) + country + afchange*tax_haven, data = filtered_data)

summary(log_interest_reg)

caymaninterest_reg <- lm(interest_income ~ afchange + year + total_u_s_source_income + country + afchange*cayman, data = filtered_data)

summary(caymaninterest_reg)

log_caymaninterest_reg <- lm(log(interest_income) ~ afchange + year + log(total_u_s_source_income) + country + afchange*cayman, data = filtered_data)

summary(log_caymaninterest_reg)

#WFP and FOREIGN CORP REGRESSIONS

#create new data frame for 2022 withholding foreign partnership
data_22_WFP <- read_irs_withholding(url_wh_22, 2022)
data_22_WFP <- data_22_WFP %>% slice(-c(1:6, 21:391))
data_22_WFP <- data_22_WFP %>% rename(
  recipient_type = 'Recipient types and selected country [1] and selected recipient type',
  interest = 'Principal types of U.S.-source income',
  dividends = ...6,
  rents_royalties = ...7,
  socialsecurity = ...8,
  personal_services_income = ...9,
  notional_principal_contract = ...10,
  return_on_capital = ...11
)
data_22_WFP <- janitor::clean_names(data_22_WFP)

#create new data frame for 2021 withholding foreign partnership
url_wh_21 <- "https://www.irs.gov/pub/irs-soi/21it02tc.xlsx"
data_21_WFP <- read_irs_withholding(url_wh_21, 2021)
data_21_WFP <- data_21_WFP %>% slice(-c(1:6, 21:391))
data_21_WFP <- data_21_WFP %>% rename(
  recipient_type = 'Recipient types and selected country [1] and selected recipient type',
  interest = 'Principal types of U.S.-source income',
  dividends = ...6,
  rents_royalties = ...7,
  socialsecurity = ...8,
  personal_services_income = ...9,
  notional_principal_contract = ...10,
  return_on_capital = ...11
)
data_21_WFP <- janitor::clean_names(data_21_WFP)

#create new data frame for 2020 withholding foreign partnership
url_wh_20 <- "https://www.irs.gov/pub/irs-soi/20it02tc.xlsx"
data_20_WFP <- read_irs_withholding(url_wh_20, 2020)
data_20_WFP <- data_20_WFP %>% slice(-c(1:6, 21:391))
data_20_WFP <- data_20_WFP %>% rename(
  recipient_type = 'Recipient types and selected country and selected recipient type',
  interest = 'Principal types of U.S.-source income',
  dividends = ...6,
  rents_royalties = ...7,
  socialsecurity = ...8,
  personal_services_income = ...9,
  notional_principal_contract = ...10
)
data_20_WFP <- janitor::clean_names(data_20_WFP)

#create new data frame for 2019 withholding foreign partnership
url_wh_19 <- "https://www.irs.gov/pub/irs-soi/19it02tc.xlsx"
data_19_WFP <- read_irs_withholding(url_wh_19, 2019)
data_19_WFP <- data_19_WFP %>% slice(-c(1:6, 19:391))
data_19_WFP <- data_19_WFP %>% rename(
  recipient_type = 'Recipient types and selected country and selected recipient type',
  interest = 'Principal types of U.S.-source income',
  dividends = ...6,
  rents_royalties = ...7,
  socialsecurity = ...8,
  personal_services_income = ...9,
  notional_principal_contract = ...10
)
data_19_WFP <- janitor::clean_names(data_19_WFP)

data_19_WFP <- data_19_WFP[ , !(names(data_19_WFP) %in% c("x11", "x12")) ]

#create new data frame for 2018 withholding foreign partnership
url_wh_18 <- "https://www.irs.gov/pub/irs-soi/18it02tc.xlsx"
data_18_WFP <- read_irs_withholding(url_wh_18, 2018)
data_18_WFP <- data_18_WFP %>% slice(-c(1:6, 18:391))
data_18_WFP <- data_18_WFP %>% rename(
  recipient_type = 'Recipient types and selected country and selected recipient type',
  interest = 'Principal types of U.S.-source income',
  dividends = ...6,
  rents_royalties = ...7,
  socialsecurity = ...8,
  personal_services_income = ...9,
  notional_principal_contract = ...10
)
data_18_WFP <- janitor::clean_names(data_18_WFP)

#create new data frame for 2017 withholding foreign partnership
url_wh_17 <- "https://www.irs.gov/pub/irs-soi/17it02tc.xls"
data_17_WFP <- read_irs_withholding(url_wh_17, 2017)
data_17_WFP <- data_17_WFP %>% slice(-c(1:6, 18:391))
data_17_WFP <- data_17_WFP %>% rename(
  recipient_type = 'Recipient types and selected country and selected recipient type',
  interest = 'Principal types of U.S.-source income',
  dividends = ...6,
  rents_royalties = ...7,
  socialsecurity = ...8,
  personal_services_income = ...9,
  notional_principal_contract = ...10
)
data_17_WFP <- janitor::clean_names(data_17_WFP)

#create new data frame for 2016 withholding foreign partnership
url_wh_16 <- "https://www.irs.gov/pub/irs-soi/16it02tc.xls"
data_16_WFP <- read_irs_withholding(url_wh_16, 2016)
data_16_WFP <- data_16_WFP %>% slice(-c(1:6, 18:391))
data_16_WFP <- data_16_WFP %>% rename(
  recipient_type = 'Selected country and selected recipient type',
  interest = 'Principal types of U.S.-source income',
  dividends = ...6,
  rents_royalties = ...7,
  socialsecurity = ...8,
  personal_services_income = ...9,
  notional_principal_contract = ...10
)
data_16_WFP <- janitor::clean_names(data_16_WFP)

#create new data frame for 2015 withholding foreign partnership
url_wh_15 <- "https://www.irs.gov/pub/irs-soi/15it02tc.xls"
data_15_WFP <- read_irs_withholding(url_wh_15, 2015)
data_15_WFP <- data_15_WFP %>% slice(-c(1:6, 18:391))
data_15_WFP <- data_15_WFP %>% rename(
  recipient_type = 'Selected country and selected recipient type',
  interest = 'Principal types of U.S.-source income',
  dividends = ...6,
  rents_royalties = ...7,
  socialsecurity = ...8,
  personal_services_income = ...9,
  notional_principal_contract = ...10
)
data_15_WFP <- janitor::clean_names(data_15_WFP)

data_WFP_all <- bind_rows(data_15_WFP, data_16_WFP, data_17_WFP, data_18_WFP, data_19_WFP, data_20_WFP, data_21_WFP, data_22_WFP)

data_WFP_all <- data_WFP_all %>%
  mutate(u_s_tax_withheld = as.numeric(u_s_tax_withheld), 
         total_u_s_source_income = as.numeric(total_u_s_source_income))

data_WFP_all <- data_WFP_all %>%
  mutate(af_change = ifelse(year <=2017, 0, 1))

data_WFP_all$WFP <- ifelse (data_WFP_all$recipient_type == "Withholding foreign partnerships and trusts", 
                            1, 0)

data_WFP_all$ForCorp <- ifelse (data_WFP_all$recipient_type == "Corporations",
                              1, 0)

data_WFP_all$recipient_type <- gsub("\\s*\\[\\d+\\]", "", data_WFP_all$recipient_type)

data_WFP_all$recipient_type <- gsub("\\bPension plans\\b", "Pensions", data_WFP_all$recipient_type)

data_WFP_all$recipient_type <- gsub("Hybrid entities making treaty claim", 
                                    "Hybrid entity making treaty claim",
                                    data_WFP_all$recipient_type)

data_WFP_subset <- data_WFP_all %>% filter(recipient_type %in% c("Corporations", "Hybrid entity making treaty claim", "Individuals", "Other and unknown", "Partnerships and trusts", "U.S. branches treated as U.S. persons", "Withholding foreign partnerships and trusts", "Withholding rate pools (general)", "Withholding rate pools (tax-exempt)"))

#GGPLOTS

ggplot(data_WFP_all, aes(x = year, y = interest, color = `recipient_type`)) +
  geom_line() +
  geom_point(size = 3) +
  labs(
    title = "Non-Log Interest by Type of Foreign Entity",
    x = "Year",
    y = "Interest",
    color = "Type of Entity"
  )

data_WFP_all %>%   filter(`recipient_type` %in% c("Individuals", "Corporations", "Partnerships and trusts", "Tax-exempt organizations[1]", "Withholding foreign partnerships and trusts")) %>%
ggplot(aes(x = year, y = log(interest), color = `recipient_type`)) +
  geom_line() +
  geom_point(size = 3) +
  labs(
    title = "Log Interest by Type of Foreign Entity",
    x = "Year",
    y = "Log Interest",
    color = "Type of Entity"
  ) 

data_WFP_all %>%   
  ggplot(aes(x = year, y = log(interest), color = `recipient_type`, shape = recipient_type)) +
  geom_line() + facet_wrap(~recipient_type) +
  geom_point(size = 3) +
  labs(
    title = "Log Interest by Type of Foreign Entity",
    x = "Year",
    y = "Log Interest",
    color = "Type of Entity"
  ) 

data_WFP_all %>%   filter(`recipient_type` %in% c("Individuals", "Corporations", "Partnerships and trusts", "Tax-exempt organizations[1]", "Withholding foreign partnerships and trusts")) %>%
  ggplot(aes(x = year, y = interest, color = `recipient_type`)) +
  geom_line() +
  geom_point(size = 3) +
  labs(
    title = "Non-Log Interest by Type of Foreign Entity",
    x = "Year",
    y = "Interest",
    color = "Type of Entity"
  ) 

data_WFP_all %>%   filter(`recipient_type` %in% c("Individuals", "Corporations", "Partnerships and trusts", "Tax-exempt organizations[1]", "Withholding foreign partnerships and trusts")) %>%
  ggplot(aes(x = year, y = log(number_of_forms_1042s), color = `recipient_type`)) +
  geom_line() +
  geom_point(size = 3) +
  labs(
    title = "Log Number of Forms 1042s by Type of Foreign Entity",
    x = "Year",
    y = "Log Number of Forms 1042s",
    color = "Type of Entity"
  )
  
data_WFP_all %>%   filter(`recipient_type` %in% c("Individuals", "Corporations", "Partnerships and trusts", "Tax-exempt organizations[1]", "Withholding foreign partnerships and trusts")) %>%
  ggplot(aes(x = year, y = number_of_forms_1042s, color = `recipient_type`)) +
  geom_line() +
  geom_point(size = 3) +
  labs(
    title = "Non-Log Number of Forms 1042s by Type of Foreign Entity",
    x = "Year",
    y = "Number of Forms 1042s",
    color = "Type of Entity"
  )
#LOG 

data_WFP_all %>%   filter(`recipient_type` %in% c("Individuals", "Corporations", "Partnerships and trusts", "Tax-exempt organizations[1]", "Withholding foreign partnerships and trusts")) %>%
  ggplot(aes(x = year, y = log(total_u_s_source_income), color = `recipient_type`)) +
  geom_smooth() +
  geom_point(size = 3) +
  labs(
    title = "Total Log U.S. Source Income By Type of Foreign Entity",
    x = "Year",
    y = "Total U.S. Source Income",
    color = "Type of Entity"
  ) 
  

data_WFP_all %>%   filter(`recipient_type` %in% c("Individuals", "Corporations", "Partnerships and trusts", "Tax-exempt organizations[1]", "Withholding foreign partnerships and trusts")) %>%
  ggplot(aes(x = year, y = log(u_s_tax_withheld), color = `recipient_type`)) +
  geom_line() +
  geom_point(size = 3) +
  labs(
    title = "U.S. Tax Withheld to Certain Types of Entities",
    x = "Year",
    y = "Total U.S. Source Income",
    color = "Type of Entity"
  )

data_WFP_all %>%   filter(`recipient_type` %in% c("Individuals", "Corporations", "Partnerships and trusts", "Tax-exempt organizations[1]", "Withholding foreign partnerships and trusts")) %>%
  ggplot(aes(x = year, y = ((log(u_s_tax_withheld))/(log(total_u_s_source_income))), color = `recipient_type`)) +
  geom_line() +
  geom_point(size = 3) +
  labs(
    title = "Effective Withholding Tax Rate by Type of Entities",
    x = "Year",
    y = "Effective Tax Rate",
    color = "Type of Entity"
  )

#NONLOG

data_WFP_all %>%   filter(`recipient_type` %in% c("Individuals", "Corporations", "Partnerships and trusts", "Tax-exempt organizations[1]", "Withholding foreign partnerships and trusts")) %>%
  ggplot(aes(x = year, y = total_u_s_source_income, color = `recipient_type`)) +
  geom_smooth() +
  geom_point(size = 3) +
  labs(
    title = "Total Non-Log U.S. Source Income to Certain Types of Entities",
    x = "Year",
    y = "Total U.S. Source Income",
    color = "Type of Entity"
  ) 


data_WFP_all %>%   filter(`recipient_type` %in% c("Individuals", "Corporations", "Partnerships and trusts", "Tax-exempt organizations[1]", "Withholding foreign partnerships and trusts")) %>%
  ggplot(aes(x = year, y = u_s_tax_withheld, color = `recipient_type`)) +
  geom_line() +
  geom_point(size = 3) +
  labs(
    title = "U.S. Tax Withheld to Certain Types of Entities",
    x = "Year",
    y = "Total U.S. Source Income",
    color = "Type of Entity"
  )

data_WFP_all %>%   filter(`recipient_type` %in% c("Individuals", "Corporations", "Partnerships and trusts", "Tax-exempt organizations[1]", "Withholding foreign partnerships and trusts")) %>%
  ggplot(aes(x = year, y = ((u_s_tax_withheld)/(total_u_s_source_income)), color = `recipient_type`)) +
  geom_line() +
  geom_point(size = 3) +
  labs(
    title = "Effective Withholding Tax Rate by Type of Entities",
    x = "Year",
    y = "Effective Tax Rate",
    color = "Type of Entity"
  )

#REGRESSIONS WFP_ALL

#LOG
interest_WFP <- lm( log(interest) ~ af_change + year + log(total_u_s_source_income) + recipient_type + af_change * WFP, data = data_WFP_all)
summary(interest_WFP)

interest_ForCorp <- lm( log(interest) ~ af_change + year + log(total_u_s_source_income) + recipient_type + af_change * ForCorp, data = data_WFP_all)
summary(interest_ForCorp)

interest_ForCorp_subset <- lm( log(interest) ~ af_change + year + log(total_u_s_source_income) + recipient_type + af_change * ForCorp, data = data_WFP_subset)
summary(interest_ForCorp_subset)

forms_reg_WFP <- lm (log(number_of_forms_1042s) ~ af_change + year + log(total_u_s_source_income) + recipient_type + af_change * WFP, data = data_WFP_all)
summary(forms_reg_WFP)

forms_reg_ForCorp <- lm (log(number_of_forms_1042s) ~ af_change + year + log(total_u_s_source_income) + recipient_type + af_change * ForCorp, data = data_WFP_all)
summary(forms_reg_ForCorp)

forms_reg10 <- lm (log10(number_of_forms_1042s) ~ af_change + year + log10(total_u_s_source_income) + recipient_type + af_change * WFP, data = data_WFP_all)
summary(forms_reg10)

#NON-LOG
interest_WFP <- lm( interest ~ af_change + year + total_u_s_source_income + recipient_type + af_change * WFP, data = data_WFP_all)
summary(interest_WFP)

interest_ForCorp <- lm( interest ~ af_change + year + total_u_s_source_income + recipient_type + af_change * ForCorp, data = data_WFP_all)
summary(interest_ForCorp)

interest_ForCorp <- lm( interest ~ af_change + year + total_u_s_source_income + recipient_type + af_change * ForCorp, data = data_WFP_subset)
summary(interest_ForCorp)

forms_reg_WFP <- lm (number_of_forms_1042s ~ af_change + year + total_u_s_source_income + recipient_type + af_change * WFP, data = data_WFP_all)
summary(forms_reg_WFP)

forms_reg_ForCorp <- lm (number_of_forms_1042s ~ af_change + year + total_u_s_source_income + recipient_type + af_change * ForCorp, data = data_WFP_all)
summary(forms_reg_ForCorp)

data_WFP_all$int_share_of_total_income <- data_WFP_all$interest / data_WFP_all$total_u_s_source_income

library(dplyr)

data_WFP_all <- data_WFP_all %>%
  group_by(year) %>%
  mutate(share_of_total_interest = interest / sum(interest, na.rm = TRUE)) %>%
  ungroup()

data_WFP_all <- data_WFP_all %>%
  group_by(year) %>%
  mutate(share_of_total_income = total_u_s_source_income / sum(total_u_s_source_income, na.rm = TRUE)) %>%
  ungroup()

ForCorp_int_total <- lm( int_share_of_total_income ~ af_change + year + total_u_s_source_income + recipient_type + af_change * ForCorp, data = data_WFP_all)
summary(ForCorp_int_total)

ForCorp_share_of_int <- lm( share_of_total_interest ~ af_change + year + share_of_total_income + recipient_type + af_change * ForCorp, data = data_WFP_all)
summary(ForCorp_share_of_int)

data_WFP_all <- data_WFP_all %>%
  mutate(non_int_income = (total_u_s_source_income - interest))

data_WFP_all <- data_WFP_all %>%
  group_by(year) %>%
  mutate(share_of_nonint_inc = non_int_income / sum(non_int_income, na.rm = TRUE)) %>%
  ungroup()

ForCorp_share_of_int <- lm( share_of_total_interest ~ af_change + year + share_of_nonint_inc + recipient_type + af_change * ForCorp, data = data_WFP_all)
summary(ForCorp_share_of_int)


data_WFP_all %>%   filter(`recipient_type` %in% c("Individuals", "Corporations", "Partnerships and trusts", "Tax-exempt organizations[1]", "Withholding foreign partnerships and trusts")) %>%
  ggplot(aes(x = year, y = share_of_total_interest, color = `recipient_type`)) +
  geom_smooth() +
  geom_point(size = 3) +
  labs(
    title = "Share of total interest by type of foreign entity",
    x = "Year",
    y = "Share of total interest",
    color = "Type of Entity"
  ) 

data_WFP_all %>%   
#  filter(`recipient_type` %in% c("Individuals", "Corporations", "Partnerships and trusts", "Tax-exempt organizations[1]", "Withholding foreign partnerships and trusts")) %>%
  ggplot(aes(x = year, y = share_of_total_interest, color = recipient_type)) +
  geom_smooth() +
  geom_point(size = 3) +
  scale_y_log10()
  labs(
    title = "Log share of total interest by type of foreign entity",
    x = "Year",
    y = "Log share of total interest",
    color = "Type of Entity"
  ) 