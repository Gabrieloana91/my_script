library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Health Check V1"),
  
  navbarPage(
    title = "Options",
    tabPanel("Task",
             column(3, wellPanel(selectInput("game_code", "Select the Game",
                                             c("VTR", "COS", "TRE")),
                                 sliderInput("level_range", "Level Range", min = 1, max = 380, value = c(20, 25))
                                 
                                 )),
                                 
                              
             column(6, plotOutput("pass_rate", height = "300px")),
             tableOutput("metrics"))
  

)))
