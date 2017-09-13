#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


shinyUI(fluidPage(theme = "bootstrap.css",
                  
                  
                  titlePanel("World of Tanks Datamaster"),
                  
                  navbarPage(
                    title = "",
                    tabPanel("Summary",
                             
                             column(6, plotOutput("pass_rate", height = "250px", width = "600px")),
                             column(6, plotOutput("number_of_plays", height = "250px")),
                             column(6, plotOutput("medians", height = "250px",width = "600px"))),
                    
                    tabPanel("Live Info",
                             column(6, plotOutput("cumm", height = "250px",width = "600px")),
                             column(6, plotOutput("win_type", height = "250px",width = "600px")),
                             column(6, plotOutput("exp", height = "250px",width = "600px")),
                             column(6, plotOutput("win_tier", height = "250px",width = "600px"))),
                    tabPanel("Tanks",
                             column(6, plotOutput("tank_info", height = "500px",width = "1200px")))),
                    
                  
                  actionButton("action", "Refresh",icon = icon("refresh"))
                  
))



