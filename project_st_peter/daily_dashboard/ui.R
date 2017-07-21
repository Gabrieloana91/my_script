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
                        condition = "input.task_type == 'Monetization Meeting'",
                        dateInput("monetization_date", "Date", format = "mm-yyyy")
                      ),
                      actionButton("compile_button", "Compile Report"),
                      actionButton("stop_button", "Stop!")),
                      imageOutput("images")))
        
  
  
)