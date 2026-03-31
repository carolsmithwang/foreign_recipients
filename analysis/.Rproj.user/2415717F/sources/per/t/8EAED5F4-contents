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

#further remove rows 5-25
data_22 <- read_irs_withholding(url_wh_22, 2022)
data_22 <- data_22 %>% slice(-(1:21))

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
ggplot(filtered_data, aes(x = year, y = `Number of Forms 1042S`)) +
  geom_line() +
  facet_wrap(~country) +
  #scale_y_log10() +
  labs(title = "Forms 1042S over Years by Country")

ggplot(filtered_data, aes(x = year, y = Interest_income)) +
  geom_line() +
  facet_wrap(~country) +
  #scale_y_log10() +
  labs(title = "Interest Income over Years by Country")
