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
library(scales)
library(zoo)

#setwd("C:/Users/natha/OneDrive/Desktop/Work/R/Rprojects/UFC Scraper and Dashboard")
events <- read_csv("../data/UFC_Events.csv")
fights <- read_csv("../data/UFC_Fights.csv")
fighters <- read_csv("../data/UFC_Fighters.csv")
source("../code/functions.R", local = T)

# Collecting inputs
# fighters
fighter_names <- fights %>%
    distinct(Fighter) %>% arrange(Fighter) %>% .[[1]]


# ==================
# UI
# ==================
header <- dashboardHeader(title = "UFC Dashboard")

sidebar <- dashboardSidebar(

    sidebarMenu(menuItem("Fight Dashboard", tabName = "fightdash"),
                menuItem("Fighter Dashboard", tabName = "fighterdash"),
                menuItem("Raw Data", tabName = "rawdata"))
)

# ==================
# Body
# ==================

fight_tab <- tabItem(tabName = "fightdash",

                     fluidRow(position = "right",

                              box(width = 3, background = "black",
                                  selectInput("method_filter", "Method Filter",
                                               choices = list("ALL", "KO/TKO", "SUB")),
                                  checkboxInput("method_logx", "log x axis"),
                                  numericInput("method_n", "n (for rare methods set n < 0)",
                                               min = -50, max = 50, value = 5)
                    ),

                    box(background = "red", width = 9, plotOutput("method_plot", width = "100%") )
                ))

fighter_tab <- tabItem(tabName = "fighterdash",


                       fluidRow(
                           column(width = 2,
                                  box(title = "Inputs", background = NULL,
                                      collapsible = F, width = NULL, status = NULL,
                                      solidHeader = T,
                                      selectInput("fighter", "Fighter",
                                                  choices = fighter_names,
                                                  selected = "Charles Oliveira",
                                                  multiple = F),
                                      conditionalPanel(
                                          condition = "input.tabs_a == 'win_loss'",
                                          dateRangeInput("date_range", "Date Range",
                                                         start = "1994-03-10",
                                                         min = "1994-03-10",
                                                         startview = "year"
                                          ) # Need to add more
                                      ),
                                      conditionalPanel(
                                          condition = "input.tabs_a == 'sig_str'",
                                          radioButtons("strike_dir", "Direction",
                                                       choices = c("Thrown", "Sustained")),
                                          radioButtons("strike_type", "Type",
                                                       choices = c("Landed", "Attempted"))
                                      ),

                                      #selectizeInput("weightclass", "Weight-Class", choices = "Lightweight"),

                                  )
                           ),
                           mainPanel(id = "Fighter Plots", width = 10,
                                     tabsetPanel(type = "tabs", id = "tabs_a",
                                                 tabPanel(type = "tabs",  value = "win_loss",
                                                          "Win/Loss Streaks",
                                                          plotOutput("streak_plot", width = "100%")),
                                                 tabPanel(type = "tabs",  value = "lan_abs",
                                                          "Strikes Landed/Absorbed",
                                                          plotOutput("landed_absorbed_plot", width = "100%")),
                                                 tabPanel(type = "tabs",  value = "sig_str",
                                                          "Distribution of Significant Strikes",
                                                          plotOutput("sig_str_loc_plot", width = "100%"))
                                     ))

                           # tabBox(title = "Fighter Plots", width = 10,
                           #        tabPanel("Win/Loss Streaks",
                           #                  plotOutput("streak_plot", width = "100%")),
                           #        tabPanel("Attacks Landed/Absorbed",
                           #                 plotOutput("landed_absorbed_plot", width = "100%")
                           #                 ))
                       ),
                       fluidRow(
                           box(title = "Dimensions", width = 2, height = NULL,
                               solidHeader = T, status = NULL,
                               infoBoxOutput("height", width = NULL), # color = "black", icon = icon("ruler-vertical")),
                               infoBoxOutput("weight", width = NULL), # color = "black", icon = icon("weight-hanging")),
                               infoBoxOutput("reach", width = NULL), # color = "black", icon = icon("ruler-horizontal")),
                           ),
                           box(title = "Record", width = 2, height = NULL,
                               solidHeader = T, status = NULL,
                               infoBoxOutput("wins", width = NULL),
                               infoBoxOutput("losses", width = NULL),
                               infoBoxOutput("other", width = NULL),
                           ),
                           box(title = "Stand-up Statistics", width = 4, height = NULL,
                               solidHeader = T, status = NULL,
                               infoBoxOutput("slpm", width = NULL),
                               infoBoxOutput("sacc", width = NULL),
                               infoBoxOutput("sapm", width = NULL),
                               infoBoxOutput("sdef", width = NULL),
                           ),
                           box(title = "Ground Game Statistics", width = 4, height = NULL,
                               solidHeader = T, status = NULL,
                               infoBoxOutput("tdav", width = NULL),
                               infoBoxOutput("tdac", width = NULL),
                               infoBoxOutput("tdd", width = NULL),
                               infoBoxOutput("subav", width = NULL),
                           )
                       )



)


body <-   dashboardBody(

    tabItems(fighter_tab, fight_tab)
)



ui <- dashboardPage(skin = "black",
                    header,
                    sidebar,
                    body

)



server <- function(input, output) {

    # Fight Tab
    output$method_plot <- renderPlot({

        #source("../code/functions.R", local = T)
        methods_plot(input$method_filter, input$method_n , input$method_logx)

    })

    # Fighter Tab
    output$streak_plot <- renderPlot({
        #source("../code/functions.R", local = T)
        streak_plot(input$fighter,
                    from_date = input$date_range[1],
                    to_date = input$date_range[2])
    })

    output$landed_absorbed_plot <- renderPlot({
        #source("../code/functions.R", local = T)
        landed_absorbed_plot(input$fighter)
    })

    output$sig_str_loc_plot <- renderPlot({
        #source("../code/functions.R", local = T)
        strike_location_plot(fighter = input$fighter,
                             tvs = input$strike_dir,
                             avl = input$strike_type)
    })

    output$height <- renderInfoBox({
        h <- fighters %>% filter(Name == input$fighter) %>% .$Height
        infoBox("Height", paste0(h,"m"), icon = icon("ruler-vertical"),
                color = "fuchsia")
    })

    output$weight <- renderInfoBox({
        h <- fighters %>% filter(Name == input$fighter) %>% .$Weight
        infoBox("Weight", paste0(h,"lbs"), icon = icon("weight-hanging"),
                color = "purple")
    })

    output$reach <- renderInfoBox({
        h <- fighters %>% filter(Name == input$fighter) %>% .$Reach
        infoBox("Reach", paste0(h,"m"), icon = icon("ruler-horizontal"),
                color = "maroon")
    })

    output$wins <- renderInfoBox({
        h <- fighters %>% filter(Name == input$fighter) %>% .$Wins
        infoBox("Wins", h, icon = icon("thumbs-up"),
                color = "green")
    })

    output$losses <- renderInfoBox({
        h <- fighters %>% filter(Name == input$fighter) %>% .$Losses
        infoBox("Losses", h, icon = icon("thumbs-down"),
                color = "red")
    })

    output$other <- renderInfoBox({
        h <- fighters %>% filter(Name == input$fighter) %>% .$Other
        infoBox("Other", h, icon = icon("meh"),
                color = "orange")
    })

    output$slpm <- renderInfoBox({
        h <- fighters %>% filter(Name == input$fighter) %>% .$SLpM
        infoBox("Significant Strikes Landed per Minute.", h, icon = icon("fist-raised"),
                color = "aqua")
    })

    output$sacc <- renderInfoBox({
        h <- fighters %>% filter(Name == input$fighter) %>% .$Str_Acc
        infoBox("Significant Strike Accuracy.", paste0(h*100,"%"), icon = icon("bullseye"),
                color = "blue")
    })

    output$sapm <- renderInfoBox({
        h <- fighters %>% filter(Name == input$fighter) %>% .$SApM
        infoBox("Significant Strikes Absorbed per Minute.", h, icon = icon("ambulance"),
                color = "light-blue")
    })

    output$sdef <- renderInfoBox({
        h <- fighters %>% filter(Name == input$fighter) %>% .$Str_Def
        infoBox("Significant Strike Defence.", paste0(h*100,"%"), icon = icon("shield-alt"),
                color = "navy")
    })

    output$tdav <- renderInfoBox({
        h <- fighters %>% filter(Name == input$fighter) %>% .$TD_Avg
        infoBox("Average Takedowns Landed per 15 minutes.", h, icon = icon("people-arrows"),
                color = "teal")
    })

    output$tdac <- renderInfoBox({
        h <- fighters %>% filter(Name == input$fighter) %>% .$TD_Acc
        infoBox("Takedown Accuracy.", paste0(h*100,"%"), icon = icon("bullseye"),
                color = "fuchsia")
    })

    output$tdd <- renderInfoBox({
        h <- fighters %>% filter(Name == input$fighter) %>% .$TD_Def
        infoBox("Takedown Defense.", paste0(h*100,"%"), icon = icon("shield-alt"),
                color = "purple")
    })

    output$subav <- renderInfoBox({
        h <- fighters %>% filter(Name == input$fighter) %>% .$Sub_Avg
        infoBox("Average Submissions Attempted per 15 minutes.", h, icon = icon("arrow-alt-circle-right"),
                color = "maroon")
    })

}

# Run the application
shinyApp(ui = ui, server = server)
