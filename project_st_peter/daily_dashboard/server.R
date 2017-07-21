# Server.R
# All computation takes place here


function(input, output){
  
  
  output$ui <- renderUI({
  if(is.null(input$task_type))
    return()
  
  
  # Depending on input$input_type, a different UI component will be generated and sent to the client
  # This part should be designed as a config file. 
    
  
  switch(input$task_type,
    "Daily Dashboard" = selectInput("daily_dash_type", "Please Choose Segment",
                                    choices = c("All" = "daily_dash_type_all",
                                                "KPI" = "daily_dash_type_kpi",
                                                "Design" = "daily_dash_type_design")),
   
    "Marketing Data" = selectInput("country", "Select Country", list(
                                    "Tier 1" = c("United States", "United Kingdom", "Australia", "Canada", "Germany", "Switzerland"),
                                    "Rest of World" = c("Spain", "Portugal", "Greece", "Romania", "France"))),
    
    "Health Check" = selectInput("hc_version", "Select Version",
                                 choices = c("2.4.1" = "2.4.1",
                                             "2.4.2" = "2.4.2",
                                             "2.4.3" = "2.4.3")))
    
  
  })  
  
  
  output$images <- renderImage({
    
    if(input$game_code == "Alien Creeps"){
      return(list(
        src = paste("www/ac_logo.png")))
    }
    else if(input$game_code == "Castle Creeps"){
      return(list(
        src = paste("www/ct_logo.png")))
    }
    else if(input$game_code == "Castle Creeps Duel"){
      return(list(
        src = paste("www/ccd_logo.png")))
    }
    else if(input$game_code %in% c("Mystery Match","Crafty Candy","Booty Quest","Bubble Genius")){
      return(list(
        src = paste("www/casual_logo.png")))
    }
    
  }, deleteFile = FALSE)
  
}