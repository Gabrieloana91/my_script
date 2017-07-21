#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(datasets)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  datasetInput <- reactive({
    switch(input$game_code,
           "Alien Creeps" =  data.frame(c(0,2)),
           "Castle Creeps" = "CT",
           "Castle Creeps Duels" = "CCT",
           "Crafty Candy" = "CC",
           "Mystery Match" = "MM",
           "Bubble Genius" = "BG",
           "Booty Quest" = "BQ")
  })
  
  
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
    else if(input$game_code == "CCD"){
      return(list(
        src = paste("www/ccd_logo.png")))
    }
    else if(input$game_code %in% c("MM","CC","BQ","BG")){
      return(list(
        src = paste("www/casual_logo.png")))
    }
    
  }, deleteFile = FALSE)
  
  output$summary <- renderPrint({
    
    if(input$game_code == "AC"){
      return(list(
        dataset <- datasetInput(),
        summary(dataset))
      )
    }
    else if(input$game_code %in% c("MM","CC","BQ","BG")){
      return(
        paste("nothing")
      )
    }
    
  })
  
  
  
})
