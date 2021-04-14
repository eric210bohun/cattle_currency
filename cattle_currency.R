# cattle_currency - the final r script for the cattle_currency project. It should email the following visualisations
# box plots for UK/IE/EU cattle prices, sets them annually on the same plot for ongoing comparison
# a price line graph for UK/IE/EU,
# make annual box plots and perhaps a series of annual normal tests to test if/how the CC_Calves_Sold changes

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
library(gmailr)
gm_auth_configure(key = "400677431814-mh09bji4vpgppkced4cab1vuk3g44hud.apps.googleusercontent.com", secret = "EeQTw6rHJMvEk03stCfSO--V", path = "~/projects/gmailr/gmailr.json")

################# EVERY MONTH THE DATE MUST BE RETIDIED - SO I MUST AUTOMATE TIDYING ##################
# convert cattle into tidy format - this script should run after the last full month of data is complete
load(file = "~/projects/cattle_currency/cattle.RData")
tidy_cattle <- cattle
tidy_cattle <- as_tibble(tidy_cattle)
tidy_cattle <- tidy_cattle %>% select(Year, Month, CC_Calves_Sold, USD_UK_Calves, USD_EU_Calves, USD_IE_Calves)
options(pillar.sigfig = 5)
tidy_cattle <- pivot_longer(tidy_cattle, c("CC_Calves_Sold", "USD_UK_Calves", "USD_EU_Calves", "USD_IE_Calves"), names_to = "Data_Type", values_to = "Calf_Sales_Data")

# box plots to compare annual cattle sale price distributions
g1 <- tidy_cattle %>% filter(!Data_Type == "CC_Calves_Sold") %>%
  filter(Year %in% Year) %>%
  ggplot(aes(Data_Type, Calf_Sales_Data)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous() +
  xlab("REARING CALVES UNDER 3 WKS OLD, DOMESTIC MARKET PRICES $ PER HEAD") +
  facet_grid(. ~ Year)

# Line Graph Comparing Cattle Prices Time Series
tidy_cattle$Date <- as.Date(with(tidy_cattle, paste(Year, Month, days_in_month(as.Date(paste(tidy_cattle$Year, tidy_cattle$Month, 26,sep="-"))),sep="-")), "%Y-%m-%d")
g2 <- tidy_cattle %>% filter(!Data_Type == "CC_Calves_Sold") %>% ggplot() + geom_line(mapping = aes(x = Date, y = Calf_Sales_Data, color = Data_Type, show.legend = FALSE)) + theme_economist() + ylab("REARING CALVES UNDER 3 WKS OLD $ PER HEAD") + scale_y_continuous()

# z tests for annual UK cattle sales increase/decrease/no change 95% - p.s. 10% test subsequently added b/c lockdown calf sales effect should be triggering an extreme economic situation flag
CC_Tidy_Means <- tidy_cattle %>% filter(Data_Type == "CC_Calves_Sold") %>% group_by(Year) %>% mutate("annual_mean" = mean(Calf_Sales_Data))
mu <- mean(CC_Tidy_Means$Calf_Sales_Data)
sd <- sd(CC_Tidy_Means$Calf_Sales_Data)
CC_Tidy_Means <- tidy_cattle %>% filter(Data_Type == "CC_Calves_Sold") %>% group_by(Year) %>% mutate("annual_mean" = mean(Calf_Sales_Data)) %>% filter(Month == "1")
CC_Tidy_Means <- CC_Tidy_Means %>% mutate("Z_Score" = (annual_mean - mu)/sd)
CC_Tidy_Means <- CC_Tidy_Means %>% mutate("Z<-1.96_5%" = ifelse(Z_Score < -1.66, "Yes", "No"))
CC_Tidy_Means <- CC_Tidy_Means %>% mutate("Z>1.96_5%" = ifelse(Z_Score > 1.66, "Yes", "No"))
CC_Tidy_Means <- CC_Tidy_Means %>% mutate("Z<-1.29_10%" = ifelse(Z_Score < -1.29, "Yes", "No"))
CC_Tidy_Means <- CC_Tidy_Means %>% mutate("Z>1.29_10%" = ifelse(Z_Score > 1.29, "Yes", "No"))
CC_Tidy_Means <- CC_Tidy_Means %>% mutate("-1.96<Z>1.96_5%" = ifelse(abs(Z_Score) > 1.96, "Yes", "No"))

# monthly cattle calf email
ggsave("~/projects/cattle_currency/g1.png", plot = g1)
ggsave("~/projects/cattle_currency/g2.png", plot = g2)
write.csv(CC_Tidy_Means, "~/projects/cattle_currency/zTests.csv")
email_eric <- gm_mime() %>%
  gm_from("eric210bohun@gmail.com") %>%
  gm_to("eric210bohun@gmail.com") %>%
  gm_text_body("Boxplots, Time Series Line Graphs, Z-Tests") %>%
  gm_subject("Monthly Cattle Calf") %>%
  gm_attach_file("~/projects/cattle_currency/g1.png") %>%
  gm_attach_file("~/projects/cattle_currency/g2.png") %>%
  gm_attach_file("~/projects/cattle_currency/zTests.csv")
gm_send_message(email_eric)