#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#





library(shiny)



function(input, output){
  
  observeEvent(input$action, {
    source("Z:\\Analytics Work\\Side Projects\\World of Tanks\\wot_controller.R")

    output$number_of_plays <- renderPlot({ P1 })
    
    output$pass_rate <- renderPlot({ P2 })
      
    output$medians <- renderPlot({ P3 })
      
    output$cumm <- renderPlot({ P4 })
    
    output$win_type <- renderPlot({ P5 })
    
    output$exp <- renderPlot({ P6 })
    
    output$win_tier <- renderPlot({ P7 })
    
    output$tank_info <- renderPlot({ P8 })

  })}


