#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)


fights <- read_csv("../data/UFC_Fights.csv")
fighters <- read_csv("../data/UFC_Fighters.csv")
source("../code/functions.R", local = T)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "UFC Dash"),
    dashboardSidebar(

        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard"),
            menuItem("Raw data", tabName = "rawdata")

        )
    ),
    dashboardBody(
        inputPanel(

            selectInput("method_filter", "Method Filter",
                        choices = list("ALL", "KO/TKO", "SUB")
                        ),
            checkboxInput("method_logx", "log x axis"),
            sliderInput("method_n", "n", min = -50, max = 50, value = 10)

        ),

        plotOutput("method_plot", width = "50%")

        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$method_plot <- renderPlot({

        source("../code/functions.R", local = T)
        methods_plot(input$method_filter, input$method_n , input$method_logx)

    })
}

# Run the application
shinyApp(ui = ui, server = server)
