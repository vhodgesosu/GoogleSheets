library(googlesheets4)
library(googledrive)
library(tidyverse)

gs4_auth(path = "strudioconnect-63781f9e833f.json")

#Read Google Sheets Data in the Execute drive for store RX data, need to pull in more sheets as new data is added
Jan22 <- read_sheet("1PlRAXOgZOHI3FhXitp1Y7pfbD7lbM4jZZItZkXXENJU", sheet = "Jan22")
Feb22 <- read_sheet("1PlRAXOgZOHI3FhXitp1Y7pfbD7lbM4jZZItZkXXENJU", sheet = "Feb22")
Mar22 <- read_sheet("1PlRAXOgZOHI3FhXitp1Y7pfbD7lbM4jZZItZkXXENJU", sheet = "Mar22")
Apr22 <- read_sheet("1PlRAXOgZOHI3FhXitp1Y7pfbD7lbM4jZZItZkXXENJU", sheet = "Apr22")

#Functions to Summarise Data either by Quarterly data or all data
summarise_gp <- function(gp) {
  gp_totals <- gp %>% group_by(`Pharmacy Name`) %>% 
    summarise(`Total Price Paid` = sum(`Total Price Paid`),
              `Total Gross Profits` = sum(`Gross Profit`),
              `GP Percentage` = sum(`Gross Profit` / `Total Price Paid`),
              `RX Totals` = n(),
              `GP Per RX` = sum(`Gross Profit` / n()))
}
summarise_all <- function(all_data){
  all_totals <- all_data %>% 
    group_by(`Pharmacy Name`, `Refill or New`, `Label Type`) %>%
    summarise(`Total Price Paid` = sum(`Total Price Paid`),
              `Total Gross Profit` = sum(`Gross Profit`),
              `GP Percentage` = sum(`Gross Profit` / `Total Price Paid`),
              `RX Totals` = n(),
              `GP Per RX` = sum(`Gross Profit` / n()))
}
summarise_tpdata_all <- function(all_tpdata){
  tpdata_all <- all_tpdata %>% 
    group_by(`Primary Third Party Bin`, `Primary Third Party PCN`, `Pharmacy Name`) %>% 
    summarise(`Total Price Paid` = sum(`Total Price Paid`),
              `Total Gross Profit` = sum(`Gross Profit`),
              `GP Percentage` = sum(`Gross Profit` / `Total Price Paid`),
              `RX Totals` = n(),
              `GP Per RX` = sum(`Gross Profit` / n()))
}



#Get Totals for Q1 22, will have to add data as new data added to google sheets
Totals_Q1_22 <- rbind(Jan22, Feb22, Mar22)
Jan22_Summary <- summarise_gp(Jan22)
Feb22_Summary <- summarise_gp(Feb22)
Mar22_Summary <- summarise_gp(Mar22)

#Get Totals for Q2 22, will have to add data as new data added to google sheets
Totals_Q2_22 <- rbind(Apr22)
Apr22_Summary <- summarise_gp(Apr22)

#Get RX Totals for All Stores regardless of month, will need to bind new data as new data is added
RX_All_Totals <- rbind(Jan22, Feb22, Mar22, Apr22)
RX_Summary <- summarise_all(RX_All_Totals)



#Summarise GP by quarter, Will need to update as new data is added
RX_Summary_Q1 <- summarise_all(Totals_Q1_22)
Q1_GP_Totals <- summarise_gp(Totals_Q1_22)
RX_Summary_Q2 <- summarise_all(Totals_Q2_22)
Q2_GP_Totals <- summarise_gp(Totals_Q2_22)


#Summary of third party data by all and quarterly
tp_data_all <- summarise_tpdata_all(RX_All_Totals)


