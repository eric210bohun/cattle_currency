# https://ec.europa.eu/info/food-farming-fisheries/farming/facts-and-figures/markets/prices/price-monitoring-sector/animal-products/beef/live-animals_en
# EC price scraper

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
library(readxl)

# load previous data - weekly and master cattle data
load(file = "~/projects/cattle_currency/euro_week_calves.RData")
load(file = "~/projects/cattle_currency/cattle.RData")

# parts of a dynamic url system
z <- "https://ec.europa.eu/info/sites/info/files/food-farming-fisheries/farming/documents/beef-vi"
w <- ".xlsx"
r <- "-"
url1 <- "https://ec.europa.eu/info/food-farming-fisheries/farming/facts-and-figures/markets/prices/price-monitoring-sector/animal-products/beef/live-animals_en"

# read url as html object
h1 <- read_html(url1)

# extract nodes from html object
p1 <- h1 %>% html_nodes("td a") %>% html_text()

# extract latest week number from current ec live animals data webpage - dont I need a months worth of week numbers, therefore two different scrape scripts?
# sprintf("%02d", as.numeric(5))
week_no <- parse_number(p1[1])
add_zero <- sprintf("%02d", week_no)
week_no <- ifelse(week_no < 10, add_zero, week_no)

# infer the current year for scrape url
p2 <- h1 %>% html_nodes("p a") %>% html_text()
year <- p2[1]
year <- as.character.numeric_version(as.numeric(substr(year, 3:4, 4)) + 1)

# more dynamic url
url2 <- paste(z, year, r, week_no, w, sep = "")

# download latest API monthly .ods dataset
download.file(url2, "~/projects/cattle_currency/ec_cattle_weekly.xlsx", method = "wget")

# extraction of a weeks EC cattle prices
all_data <- (read_xlsx("/home/eric/projects/cattle_currency/ec_cattle_weekly.xlsx", sheet = "Current Weekly Live Bovine", col_names = FALSE))
wklyec_cattle_date <- as_date(as.numeric(all_data[3,18]),origin = "1899-12-30")
all_data <- all_data[4:17]
all_data <- slice(all_data, 17)
IE <- all_data[1,4]
IE <- as.numeric(IE)
IE <- signif(IE, digits = 8)
EU <- all_data[1,14]
EU <- as.numeric(EU)
EU <- signif(EU, digits = 8)

# binding into a tibble
options(pillar.sigfig = 8)
euro_week_calves <- add_row(euro_week_calves,"Date" = wklyec_cattle_date, "IE" = IE[[1]], "EU" = EU[[1]])

# month aggregation check
full_month <- ifelse(month(euro_week_calves$Date[1]) == month(euro_week_calves$Date[[NROW(euro_week_calves)]]),1,0)
month_chop <- euro_week_calves %>% filter(Date < euro_week_calves$Date[NROW(euro_week_calves)])
options(pillar.sigfig = 8)
EU_latest_month <- mean(month_chop$EU)
IE_latest_month <- mean(month_chop$IE)
options(pillar.sigfig = 8)

# add latest month to master cattle data
cattle$EUR_EU_Calves[1] <- ifelse(full_month==1,cattle$EUR_EU_Calves[1], EU_latest_month)
cattle$EUR_IE_Calves[1] <- ifelse(full_month==1,cattle$EUR_IE_Calves[1], IE_latest_month)

# trim off extracted weekly data (from last month)
trimmed_euro_week_calves <- filter(euro_week_calves,Date == euro_week_calves$Date[NROW(euro_week_calves)])
if(full_month==0) euro_week_calves <- trimmed_euro_week_calves

# save new weekly data & master cattle data
save(euro_week_calves ,file = "~/projects/cattle_currency/euro_week_calves.RData")
save(cattle,file = "~/projects/cattle_currency/cattle.RData")