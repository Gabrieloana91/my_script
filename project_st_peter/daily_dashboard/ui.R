# Daily Dashboard User Interface

library(shiny)
source('~/amplitude-dashboard/dashboard-code/SharedConfig.R')
source('~/r-lib/R/shared.R')


shinyUI(fluidPage(
  
  titlePanel("Project St Peter"),
  
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
            plotOutput("DAU", height = "300px")))
  
)