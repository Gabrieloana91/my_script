#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  titlePanel("Project Datenmeister V2"),
  
  column(4, wellPanel(selectInput("command", label = h4("Task"),
                                  choices = list("Health Check" = "hc", "Daily Dashboard Revenue" = "daily_dash"),
                                  selected = 1),
                      selectInput("game_code", label = h4("Game"),
                                  choices = list("Alien Creeps" = "AC", "Castle Creeps" = "CT"),
                                  selected = 1),
                      dateRangeInput('dateRange',
                                     label = h4('Date range input'),
                                     start = Sys.Date() - 2, end = Sys.Date() + 2),
                      selectInput("version", label = h4("Version"),
                                  choices = list("2.1.0" = "2.1.0", "2.1.1" = "2.1.1", "2.1.2" = "2.1.2"),
                                  selected = 1))
         
  ),
  column(3, wellPanel(radioButtons("cohorted", "Cohorted Metrics", c("Cohorted" = "Cohorted_1", "Non-Cohorted" = "Non-Cohorted_1"))),
         imageOutput("AC"),
         imageOutput("CT")
         ),
  
  column(3,
         verbatimTextOutput("dateRangeText"),
         verbatimTextOutput("version"),
         actionButton("compile",label = "Compile Report")
         )
  
))