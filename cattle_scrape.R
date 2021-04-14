# This script gets the # cattle sold and gbp sale price from UK national statistics and applies linear regression to the prices to measure price changes from the historical data
# Y = 10.991 +x*.857 - linear regression from historical cc bulls to historical dependent variable i.e. eurostat pre-2021 UK (EU) male calf prices
# for details of the linear regression calculation see tab 'Key Figures for R' in ~/projects/cattle_currency/cattle_currency_historical_data.ods

# load libraries
library(tidyverse)
library(rvest)
library(RSelenium)
library(cronR)
library(magrittr)
library(lubridate)
library(gridExtra)
library(ggthemes)
library(gridExtra)
library(quantmod)
library(readODS)

# load cattle object
load(file = "~/projects/cattle_currency/cattle.RData")

# obtain url for latest spreadsheet to download
url1 <- "https://www.gov.uk/government/statistical-data-sets/livestock-prices-finished-and-store#history"
h1 <- read_html(url1)
dynamic_url <- h1 %>% html_elements("span a")
url2 <- html_attr(dynamic_url[4], "href")

# download 
download.file(url2, "~/projects/cattle_currency/GB_cattle.ods", method = "wget")

# extraction of latest numbers of cc bulls sold and latest cattle price for UK CC bull calves and insertion into cattle object
all_data <- read_ods("~/projects/cattle_currency/GB_cattle.ods", sheet = 1, col_names = FALSE)
all_data <- all_data[4:ncol(all_data)]
all_data <- slice(all_data, 43)
latest_price <- as.numeric(all_data[last(which(!is.na(all_data)))])
latest_bulls_sold <- as.numeric(all_data[last(which(!is.na(all_data))) -1])
cattle$GBP_CC_Calves[1] <- latest_price
cattle$CC_Calves_Sold[1] <- latest_bulls_sold

# conversion of all last months data (IE, EU, UK (CC Bulls)) into USD, then UK USD CC Bulls into UK Monthly Price Per Head in USD via the regression equation
options(pillar.sigfig = 8)

# column statistic calculations
cattle$USD_UK_Calves[1] <- 10.991 + (cattle$GBP_CC_Calves[1]*cattle$`GBP→USD`[1])*.857
cattle$USD_CC_Calves[1] <- cattle$GBP_CC_Calves[1]*cattle$`GBP→USD`[1]
cattle$USD_EU_Calves[1] <- cattle$EUR_EU_Calves[1]*cattle$`EUR→USD`[1]
cattle$USD_IE_Calves[1] <- cattle$EUR_IE_Calves[1]*cattle$`EUR→USD`[1]

# save cattle
save(cattle,file = "~/projects/cattle_currency/cattle.RData")