#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  output$dateRangeText <- renderText({
    paste("Date Range Selected:", 
          paste(as.character(input$dateRange), collapse = " to "))
  })
  
  
  output$version <- renderText({
    paste("Version Selected:",
          paste(input$version))
  }) 
  
  
  output$AC <- renderImage({
    
    if(input$game_code == "AC"){
      return(list(
        src = paste("www/ac_logo.png")))
    }
    else if(input$game_code == "CT"){
      return(list(
        src = paste("www/ct_logo.png")))
    }
    
  }, deleteFile = FALSE)
  

})
