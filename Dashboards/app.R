library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)
library(rsconnect)
library(scales)
library(googlesheets4)
library(googledrive)
library(tidyverse)

source('PharmCapData.R', local = TRUE)

ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "PharmCap Store Performance", titleWidth = 300),
                    
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Total Gross Profits", tabName = "gross_profits", icon = icon("dollar-sign")),
                        menuItem("Gross Profits Breakdown", tabName = "gp_breakdown", icon = icon("dollar-sign")),
                        menuItem("RX Summary By Month", tabName = "gp_month", icon = icon("prescription")),
                        menuItem("Q1 RX Totals", tabName = "q1", icon = icon("prescription-bottle")),
                        menuItem("Q2 RX Totals", tabName = "q2", icon = icon("prescription-bottle")),
                        menuItem("Third Party Data", tabName = "third_party", icon = icon("frog"))
                      )
                    ),
                    
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "gross_profits",
                                fluidPage(
                                  h1("Total Gross Profits"),
                                  valueBoxOutput("kingston", width = 6),
                                  valueBoxOutput("nhdrug", width = 6),
                                  valueBoxOutput("rcdrug", width = 6),
                                  valueBoxOutput("superh", width = 6),
                                  valueBoxOutput("nac", width = 6),
                                  valueBoxOutput("shawnee", width = 6),
                                  valueBoxOutput("vinita", width = 6)
                                )),
                        tabItem(tabName = "gp_breakdown",
                                fluidPage(tabsetPanel(
                                  tabPanel("Total Gross Profits",
                                           dataTableOutput("gp_totals")),
                                  tabPanel("RX Totals Plot",
                                           box(
                                             title = "Store RX Totals", width = 12, background = "maroon", solidHeader = TRUE,
                                             plotOutput("totals_hist", height = 800))),
                                  tabPanel("GP Per RX",
                                           box(
                                             title= "GP Per RX By Store and RX Type", width = 12, background = "green", solidHeader = TRUE,
                                             plotOutput("gp_per_rx", height = 800))),
                                ))),
                        tabItem(tabName = "gp_month",
                                fluidPage(tabsetPanel(
                                  tabPanel("Jan 2022", h3("Jan 2022 RX Summary"),
                                           box(DT::dataTableOutput("Jan22_Summary"), width = 300)
                                  ),
                                  tabPanel("Feb 2022", h3("Feb 2022 RX Summary"),
                                           box(DT::dataTableOutput("Feb22_Summary"), width = 300)
                                  ),
                                  tabPanel("March 2022", h3("March 2022 RX Summary"),
                                           box(DT::dataTableOutput("Mar22_Summary"), width = 300)
                                  ),
                                  tabPanel("April 2022", h3("April 2022 RX Summary"),
                                           box(DT::dataTableOutput("Apr22_Summary"), width = 300)
                                  ),
                                  
                                ))),
                        
                        tabItem(tabName = "q1",
                                fluidRow(
                                  h1("Q1 RX Totals"),
                                  box(DT::dataTableOutput("view"), width = 300)
                                )),
                        tabItem(tabName = "q2",
                                fluidRow(
                                  h1("Q2 RX Totals"),
                                  box(DT::dataTableOutput("view2"), width = 300)
                                )),
                        tabItem(tabName = "third_party",
                                fluidRow(
                                  h1("Third Party Data"),
                                  dataTableOutput("df")
                                ))
                      )
                    )
)

server <- function(input, output) {
  
  output$kingston <- renderValueBox({
    RX_All_Totals %>%
      filter(`Pharmacy Name` == "Kingston Pharmacy") %>% 
      summarise(kp = sum(`Gross Profit`)) %>% 
      .$kp %>% 
      dollar(accuracy = NULL, largest_with_cents = 1e+07) %>% 
      valueBox(subtitle = "Kingston Pharmacy",
               icon = icon("store")
      )
  })
  
  output$nhdrug <- renderValueBox({
    RX_All_Totals %>%
      filter(`Pharmacy Name` == "Nichols Hills Drug") %>% 
      summarise(kp = sum(`Gross Profit`)) %>% 
      .$kp %>% 
      dollar(accuracy = NULL, largest_with_cents = 1e+07) %>% 
      valueBox(subtitle = "Nichols Hills Drug",
               icon = icon("store"),
               color = "red"
      )
  })
  
  output$rcdrug <- renderValueBox({
    RX_All_Totals %>%
      filter(`Pharmacy Name` == "Red Cross Drug") %>% 
      summarise(kp = sum(`Gross Profit`)) %>% 
      .$kp %>% 
      dollar(accuracy = NULL, largest_with_cents = 1e+07) %>% 
      valueBox(subtitle = "Red Cross Drug",
               icon = icon("store"), color = "yellow"
      )
  })
  
  output$superh <- renderValueBox({
    RX_All_Totals %>%
      filter(`Pharmacy Name` == "Super H Pharmacy") %>% 
      summarise(kp = sum(`Gross Profit`)) %>% 
      .$kp %>% 
      dollar(accuracy = NULL, largest_with_cents = 1e+07) %>% 
      valueBox(subtitle = "Super H Pharmacy",
               icon = icon("store"), color = "orange"
      )
  })
  
  output$nac <- renderValueBox({
    RX_All_Totals %>%
      filter(`Pharmacy Name` == "The Medicine Shoppe Nac") %>% 
      summarise(kp = sum(`Gross Profit`)) %>% 
      .$kp %>% 
      dollar(accuracy = NULL, largest_with_cents = 1e+07) %>% 
      valueBox(subtitle = "Nacogdoches",
               icon = icon("store"), color = "green"
      )
  })
  
  output$shawnee <- renderValueBox({
    RX_All_Totals %>%
      filter(`Pharmacy Name` == "The Medicine Shoppe Shawnee") %>% 
      summarise(kp = sum(`Gross Profit`)) %>% 
      .$kp %>% 
      dollar(accuracy = NULL, largest_with_cents = 1e+07) %>% 
      valueBox(subtitle = "Shawnee",
               icon = icon("store"), color = "maroon"
      )
  })
  
  output$vinita <- renderValueBox({
    RX_All_Totals %>%
      filter(`Pharmacy Name` == "V & V Drug") %>% 
      summarise(kp = sum(`Gross Profit`)) %>% 
      .$kp %>% 
      dollar(accuracy = NULL, largest_with_cents = 1e+07) %>% 
      valueBox(subtitle = "V&V Drug",
               icon = icon("store"), color = "fuchsia"
      )
  })
  
  output$gp_totals = DT::renderDataTable({
    DT::datatable(RX_Summary, options = list(
      pageLength = 50)) %>% 
      formatCurrency(c('Total Price Paid', 'Total Gross Profit', 'GP Per RX')) %>% 
      formatPercentage('GP Percentage') %>% 
      formatStyle(
        'Pharmacy Name', target = "row",
        backgroundColor = styleEqual(
          unique(RX_Summary_Q1$`Pharmacy Name`), 
          c('lightblue', 'lightyellow', 'lightgreen', 'pink', 'cyan', 'orange', 'lightgrey')
        )
      )
  })
  
  output$totals_hist = renderPlot({
    ggplot(RX_Summary, aes(fill = `Label Type`)) +
      geom_col(aes(y = `Pharmacy Name`, x = `RX Totals`)) +
      theme(text = element_text(size = 18))
  })
  
  output$gp_per_rx = renderPlot({
    ggplot(RX_Summary, aes(fill = `Label Type`)) +
      geom_bar(aes(x = `Pharmacy Name`, y = `GP Per RX`), stat = "identity", position = position_dodge()) +
      theme(text = element_text(size = 18))
  })
  
  output$Jan22_Summary <- renderDataTable({
    datatable(Jan22_Summary) %>% 
      formatCurrency(c('Total Price Paid', 'Total Gross Profits', 'GP Per RX')) %>% 
      formatPercentage('GP Percentage') %>% 
      formatStyle(
        'Pharmacy Name', target = "row",
        backgroundColor = styleEqual(
          unique(RX_Summary_Q1$`Pharmacy Name`), 
          c('lightblue', 'lightyellow', 'lightgreen', 'pink', 'cyan', 'orange', 'lightgrey')
        )
      ) 
  })
  
  output$Feb22_Summary <- renderDataTable({
    datatable(Feb22_Summary) %>% 
      formatCurrency(c('Total Price Paid', 'Total Gross Profits', 'GP Per RX')) %>% 
      formatPercentage('GP Percentage') %>% 
      formatStyle(
        'Pharmacy Name', target = "row",
        backgroundColor = styleEqual(
          unique(RX_Summary_Q1$`Pharmacy Name`), 
          c('lightblue', 'lightyellow', 'lightgreen', 'pink', 'cyan', 'orange', 'lightgrey')
        )
      ) 
  })
  
  output$Mar22_Summary <- renderDataTable({
    datatable(Mar22_Summary) %>% 
      formatCurrency(c('Total Price Paid', 'Total Gross Profits', 'GP Per RX')) %>% 
      formatPercentage('GP Percentage') %>% 
      formatStyle(
        'Pharmacy Name', target = "row",
        backgroundColor = styleEqual(
          unique(RX_Summary_Q1$`Pharmacy Name`), 
          c('lightblue', 'lightyellow', 'lightgreen', 'pink', 'cyan', 'orange', 'lightgrey')
        )
      ) 
  })
  
  output$Apr22_Summary <- renderDataTable({
    datatable(Apr22_Summary) %>% 
      formatCurrency(c('Total Price Paid', 'Total Gross Profits', 'GP Per RX')) %>% 
      formatPercentage('GP Percentage') %>% 
      formatStyle(
        'Pharmacy Name', target = "row",
        backgroundColor = styleEqual(
          unique(RX_Summary_Q1$`Pharmacy Name`), 
          c('lightblue', 'lightyellow', 'lightgreen', 'pink', 'cyan', 'orange', 'lightgrey')
        )
      ) 
  })
  
  output$view <- DT::renderDataTable({
    DT::datatable(RX_Summary_Q1, options = list(
      pageLength = 50
    )) %>% 
      formatCurrency(c('Total Price Paid', 'Total Gross Profit', 'GP Per RX')) %>% 
      formatPercentage('GP Percentage') %>%
      formatStyle(
        'Pharmacy Name', target = "row",
        backgroundColor = styleEqual(
          unique(RX_Summary_Q1$`Pharmacy Name`), 
          c('lightblue', 'lightyellow', 'lightgreen', 'pink', 'cyan', 'orange', 'lightgrey')
        )
      ) 
  })
  
  output$view2 <- DT::renderDataTable({
    DT::datatable(RX_Summary_Q2, options = list(
      pageLength = 50
    )) %>% 
      formatCurrency(c('Total Price Paid', 'Total Gross Profit', 'GP Per RX')) %>% 
      formatPercentage('GP Percentage') %>%
      formatStyle(
        'Pharmacy Name', target = "row",
        backgroundColor = styleEqual(
          unique(RX_Summary_Q1$`Pharmacy Name`), 
          c('lightblue', 'lightyellow', 'lightgreen', 'pink', 'cyan', 'orange', 'lightgrey')
        )
      ) 
  })
  
  output$df <- renderDataTable({
    tp_data_all
  })
  
}


shinyApp(ui, server)


