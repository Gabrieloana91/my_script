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
library(DT)

source('~/amplitude-dashboard/dashboard-code/SharedConfig.R')
source('~/r-lib/R/shared.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  

  
  datasetInput <- reactive({
    switch(input$game_code,
           "Alien Creeps" =  "AC",
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
  
  output$table <- renderTable({
    if(input$game_code == "CT"){
    data <- data.frame(revenue <- amplitude("AC", event="unverified_revenue", measured_by="sums",
                                              group_by_properties=list(list(type="event", value="$revenue"), list(type="event", value="$revenueType")),
                                              start=Sys.Date()-5, end=Sys.Date()-1) %>%
                         rename(dt = date) %>% 
                         make_currency_values_USD(., "USD", "property", "value") %>% 
                         rename(date = dt) %>% 
                         group_by(Date = as.character(date)) %>% 
                         summarise(Revenue = sum(revenue))
    )
    data
    }
    else if(input$game_code != "CT"){
      paste()
    }
    
  })
  
 
})
