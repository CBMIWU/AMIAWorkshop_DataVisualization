# ui.R

library(shinydashboard)
library(shiny)
library(rCharts)

dashboardPage(
  dashboardHeader(title = "Basic Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("KPIs", tabName = "KPIs", icon = icon("line-chart")),
      menuItem("Operations", tabName = "Operations", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "KPIs",
              fluidRow(
                box(showOutput("PI_growth", "highcharts")),                
                box(showOutput("mostPopularTool", "highcharts")
                )
              ),
              fluidRow(
                column(width = 6, box(width = NULL, showOutput("new_PIs", "highcharts"))),
                column(width = 6, valueBoxOutput(width = 6, "PI_number"), 
                       valueBoxOutput(width = 6, "JIT_number"),
                       box(width = 12, plotOutput("bulletGraph", height = 178))) 
                       
                )
                
      ),
      
      # Second tab content
      tabItem(tabName = "Operations",
              h2("Add Oparational Charts Here")
      )
    )
  )
)