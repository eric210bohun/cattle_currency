# load libraries    
library(tidyverse)
library(rvest)
library(RSelenium)
library(cronR)
library(emayili)
library(magrittr)
library(lubridate)
library(gridExtra)
library(ggthemes)
library(gridExtra)
library(quantmod)
library(readODS)
library(gmailr)
gm_auth_configure(key = "400677431814-mh09bji4vpgppkced4cab1vuk3g44hud.apps.googleusercontent.com", secret = "EeQTw6rHJMvEk03stCfSO--V", path = "~/projects/gmailr/gmailr.json")

# check to see if there was a problem with last month's data - anything missing? if yes then email alert
email_eric <- gm_mime() %>%
  gm_from("eric210bohun@gmail.com") %>%
  gm_to("eric210bohun@gmail.com") %>%
  gm_text_body("Last month's cattle currency is missing something") %>%
  gm_subject("Check out Cattle Currency")
if(sum(is.na(cattle)) > 0) gm_send_message(email_eric)

# prob dont need this but will keep anyway
load(file = "~/projects/cattle_currency/GBPUSD.RData")
load(file = "~/projects/cattle_currency/EURUSD.RData")

# regular collection of exchange rate data do 1st of the month minus one day then days in month then feed this into the getFX() call
options(pillar.sigfig = 7)
x <- today()
y <- as.Date.POSIXct(as.POSIXlt(today())-days_in_month(as.POSIXlt.Date(x) - 86400)*86400)
getFX("GBP/USD", from = y)
getFX("EUR/USD", from = y)
GBPUSD <- mean(GBPUSD)
EURUSD <- mean(EURUSD)
GBPUSD <- as.numeric(format(GBPUSD, nsmall = 7))
EURUSD <- as.numeric(format(EURUSD, nsmall = 7))

# add a new row to master cattle data
load(file = "~/projects/cattle_currency/cattle.RData")
cattle <- cattle %>% add_row(Year = as.numeric(year(y)), Month = month(y, label = FALSE, abbr = FALSE),.before = 1)
cattle$`GBP→USD`[1] <- GBPUSD
cattle$`EUR→USD`[1] <- EURUSD

# save everything
save(GBPUSD,file = "~/projects/cattle_currency/GBPUSD.RData")
save(EURUSD,file = "~/projects/cattle_currency/EURUSD.RData")
save(cattle,file = "~/projects/cattle_currency/cattle.RData")