########################################################
# Model Selection                                      #
# Author: Sunil Mathew                                 #
########################################################

library(shiny)
library(shinydashboard)

header <- dashboardHeader(title = "Model Selection")

### SideBar:
sidebar <- dashboardSidebar(
  sidebarMenu(
   menuItem("Data viz.", tabName = "graphs", icon = icon("fa fa-circle")),
   menuItem("About", tabName = "about", icon = icon("fa fa-info-circle"))
   ),
  fileInput("file1", "Choose CSV File",
            accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
  )
 )

body <- dashboardBody(



  tabItems(

   tabItem(tabName = "graphs",

    fluidRow(
    box(width = 6,title = "Model options",
        solidHeader = FALSE, status = "warning",
        DT::dataTableOutput('modeloptions')),
    
    box(width = 6,title = "Model selection",
        solidHeader = F, status = "primary",
        tabsetPanel(type = "tabs",
                    tabPanel("Data", plotOutput("dataviz")),
                    tabPanel("Selected Model", plotOutput("regression")),
                    tabPanel("QQ plot", plotOutput("qqplot")),
                    tabPanel("Residuals vs Fitted values", plotOutput("residuals"))
                    ))),
    
    fluidRow(
        box(width = 6,title = "Summary",
            solidHeader = FALSE, status = "warning",
            verbatimTextOutput(outputId = "summary")))
    
   ),
   
   
   tabItem(tabName = "about",
           fluidPage(
               box(width = 10,status = "success",
                   shiny::includeMarkdown("README.md"))
           )
   )
           )
   )

ui <- dashboardPage(header, sidebar, body)
