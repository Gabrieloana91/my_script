# Daily Dashboard User Interface

library(shiny)
source('~/r-lib/R/shared.R')


shinyUI(fluidPage(
  
  titlePanel("Project St Peter"),
  
  navbarPage(
    title = "Options",
    tabPanel('Tasks', 
             column(3, wellPanel(selectInput("game_code", "Select the Game", 
                                             c("Castle Creeps", "Castle Creeps Duel", "Alien Creeps", "Crafty Candy", "Booty Quest", "Mystery Match", "Bubble Genius")),
                                 selectInput("task_type", "Select Task", 
                                             c("Daily Dashboard", "Health Check", "Monetization Meeting", "Marketing Data"), selected = 0),
                                 uiOutput("ui"),
                                 conditionalPanel(
                                   condition = "input.task_type == 'Daily Dashboard'",
                                   dateInput("daily_dash_date", "Date")
                                 ),
                                 conditionalPanel(
                                   condition = "input.task_type = 'Daily Dashboard'",
                                   selectInput("input.task_type_segment", "Please Choose Segment",
                                               c("All","KPI","Design"))
                                 ),
                                 conditionalPanel(
                                   condition = "input.task_type == 'Monetization Meeting'",
                                   dateInput("monetization_date", "Date", format = "mm-yyyy")
                                 ),
                                 actionButton("compile_button", "Compile Report", icon("paper-plane"),
                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                    imageOutput("images")),
             
             column(4, plotOutput("revenue", height = "300px"),
                    plotOutput("new_users", height = "300px")),
             
             
             column(4, plotOutput("conversion_rate", height = "300px"),
                    plotOutput("DAU", height = "300px"))),
    tabPanel("Game Comparison",
             column(3, wellPanel(selectInput("game_comparison", label = h3("Game Comparison"),
                                                    c("Alien Creeps" = "AC",
                                                      "Castle Creeps" = "CT",
                                                      "Castle Creeps Duel" = "CCD",
                                                      "Crafty Candy" = "CC",
                                                      "Booty Quest" = "BQ",
                                                      "Mystery Match" = "MM",
                                                      "Bubble Genius" = "BG"), multiple = T, selected = 2),
                                 dateRangeInput("comparison_dates", label = ("Date Range"),
                                                min="2015-01-01", max=Sys.Date()-1),
                                 actionButton("comparison_button", "Run Comparison", icon("paper-plane"),
                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                 downloadButton("download", "Download"))),
             column(4, DT::dataTableOutput("comparison")))
    
    
  )))