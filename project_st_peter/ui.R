# setwd("C:\\Users\\Gabz\\Documents\\my_script\\project_st_peter")
library(shiny)



shinyUI(fluidPage(
  
  titlePanel("Project St Peter"),
  
  column(3, wellPanel(selectInput("command", label = h4("Task"),
                                  choices = list("Health Check" = "hc", "Daily Dashboard Revenue" = "daily_dash"),
                                  selected = 1),
                      selectInput("game_code", label = h4("Game"),
                                  choices = list("Alien Creeps" = "AC", "Castle Creeps" = "CT",
                                                 "Castle Creeps Duel" = "CCD",
                                                 "Crafty Candy" = "CC", "Mystery Match" = "MM",
                                                 "Bubble Genius" = "BG", "Booty Quest" = "BQ"),
                                  selected = 1),
                      dateRangeInput('dateRange',
                                     label = h4('Date range input'),
                                     start = Sys.Date() - 2, end = Sys.Date() + 2),
                      selectInput("version", label = h4("Version"),
                                  choices = list("2.1.0" = "2.1.0", "2.1.1" = "2.1.1", "2.1.2" = "2.1.2"),
                                  selected = 1),
                      submitButton("Update View")
                      # downloadButton('downloadbutton','Download'))
         
  )),
  column(3, wellPanel(radioButtons("cohorted", "Cohorted Metrics (Not Available for AC yet)", c("Cohorted" = "Cohorted_1", "Non-Cohorted" = "Non-Cohorted_1"))),
         imageOutput("AC")
         
  ),
  
  column(2, h4("Revenue"),
            tableOutput("table")
         
         )
       
  )
)