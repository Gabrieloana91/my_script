# Server.R
# All computation takes place here


function(input, output){
  
  
  output$ui <- renderUI({
  if(is.null(input$task_type))
    return()
  
  
  # Depending on input$input_type, a different UI component will be generated and sent to the client
  # This part should be designed as a config file. 
    
  
  switch(input$task_type,
    # "Daily Dashboard" = selectInput("daily_dash_type", "Please Choose Segment",
    #                                 choices = c("All" = "daily_dash_type_all",
    #                                             "KPI" = "daily_dash_type_kpi",
    #                                             "Design" = "daily_dash_type_design")),
   
    "Marketing Data" = selectInput("country", "Select Country", list(
                                    "Tier 1" = c("United States", "United Kingdom", "Australia", "Canada", "Germany", "Switzerland"),
                                    "Rest of World" = c("Spain", "Portugal", "Greece", "Romania", "France")),
                                   selectize = T, multiple = T),
    
    "Health Check" = selectInput("hc_version", "Select Version",
                                 choices = c("2.4.1" = "2.4.1",
                                             "2.4.2" = "2.4.2",
                                             "2.4.3" = "2.4.3")))
    
  
  })  
  
  # Output the images as the game is changed
  
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
  
  # Plot the revenue and assign "game" to game_code for data retrieval.
  
  output$revenue <- renderPlot({
  
    if(input$game_code == input$game_code & input$task_type == "Daily Dashboard" & input$compile_button == T){
      
      
      if(input$game_code == "Alien Creeps"){
        game = "AC"
        shade = "darkgreen"
      }
      else if(input$game_code == "Castle Creeps"){
        game = "CT"
        shade = "darkorchid3"
      }
      else(return("nothing"))
      
      
      
      data <- amplitude(game, event="unverified_revenue", measured_by="sums",
                        group_by_properties=list(list(type="event", value="$revenue"), list(type="event", value="$revenueType")),
                        start=input$daily_dash_date-30, end=input$daily_dash_date-1) %>%
        rename(dt = date) %>% 
        make_currency_values_USD(., "USD", "property", "value") %>% 
        rename(date = dt) %>% 
        group_by(Date = as.Date(date)) %>% 
        summarise(Revenue = sum(revenue)) %>% 
        as.data.frame()
      
      
      
      ggplot(data, aes(as.Date(Date), Revenue))+
        geom_line(size = 1, colour = shade)+
        scale_x_date(name = "",limits = c(min(data$Date), input$daily_dash_date-1), date_breaks = "2 day")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom", legend.title=element_blank() ,
              text=element_text(face="bold"), plot.title = element_text(hjust=0.5), 
              axis.title=element_text(face="plain", lineheight=2, vjust=1))+
        ggtitle("Net USD Revenue")
      
    }
    else(return(paste("nothing to display")))
    
  })

  ### New Users
      
  output$new_users <- renderPlot({
    
    if(input$game_code == input$game_code & input$task_type == "Daily Dashboard" & input$compile_button == T){
      
      if(input$game_code == "Alien Creeps"){
        game = "AC"
        first_launch = "First Launch"
        shade = "darkgreen"
      }
      else if(input$game_code == "Castle Creeps"){
        game = "CT"
        first_launch = "first_launch"
        shade = "darkorchid3"
      }
      else(return("nothing"))
      
    
      data <- amplitude(game, event=first_launch, measured_by="uniques",
                        group_by_properties=list(list(type="user", value="store")),
                        start=input$daily_dash_date-30, end=input$daily_dash_date-1) %>%
        filter(property != "(none)") %>% 
        rename(new_users = value) %>% 
        as.data.frame()
  
  
      ggplot(data, aes(as.Date(date), new_users, fill = property))+
        geom_bar(stat = "identity", position = "stack")+
        scale_x_date(name = "",limits = c(min(data$date), input$daily_dash_date-1), date_breaks = "2 day")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom", legend.title=element_blank() ,
              text=element_text(face="bold"), plot.title = element_text(hjust=0.5), 
              axis.title=element_text(face="plain", lineheight=2, vjust=1))+
        ggtitle("New Users")
    }
    else(return(paste("nothing to display")))
    
  })
  
  ### conversion Rate
    
  output$conversion_rate <- renderPlot({
    
    if(input$game_code == input$game_code & input$task_type == "Daily Dashboard" & (input$input.task_type_segment == "KPI" | input$input.task_type_segment == "All") & input$compile_button == T){
      
      if(input$game_code == "Alien Creeps"){
        game = "AC"
        shade = "darkgreen"
      }
      else if(input$game_code == "Castle Creeps"){
        game = "CT"
        shade = "darkorchid3"
      }
      else(return("nothing"))
      
      
      data <- amplitude(game, event="unverified_revenue", measured_by="pct_dau",
                        start=input$daily_dash_date-30, end=input$daily_dash_date-1) %>%
        rename(conversion_rate = value) %>% 
        as.data.frame()
      
      
      ggplot(data, aes(as.Date(date), conversion_rate))+
        geom_line(colour = shade, size = 1)+
        scale_x_date(name = "",limits = c(min(data$date), input$daily_dash_date-1), date_breaks = "2 day")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom", legend.title=element_blank() ,
              text=element_text(face="bold"), plot.title = element_text(hjust=0.5), 
              axis.title=element_text(face="plain", lineheight=2, vjust=1))+
        ggtitle("Conversion Rate")
    }
    else(return(paste("nothing to display")))
    
  })   
    
  ### DAU
  
  output$DAU <- renderPlot({
    
    if(input$game_code == input$game_code & input$task_type == "Daily Dashboard" & (input$input.task_type_segment == "KPI" | input$input.task_type_segment == "All") & input$compile_button == T){
      
      if(input$game_code == "Alien Creeps"){
        game = "AC"
        shade = "darkgreen"
      }
      else if(input$game_code == "Castle Creeps"){
        game = "CT"
        shade = "darkorchid3"
      }
      else(return("nothing"))
      
      
      data <- amplitude(game, event="_active", measured_by="uniques",
                        group_by_properties=list(list(type="user", value="store")),
                        start=input$daily_dash_date-30, end=input$daily_dash_date-1) %>%
        filter(property != "(none)") %>% 
        rename(DAU = value) %>% 
        as.data.frame()
      
      
      ggplot(data, aes(as.Date(date), DAU, fill = property))+
        geom_bar(stat = "identity", position = "stack")+
        scale_x_date(name = "",limits = c(min(data$date), input$daily_dash_date-1), date_breaks = "2 day")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom", legend.title=element_blank() ,
              text=element_text(face="bold"), plot.title = element_text(hjust=0.5), 
              axis.title=element_text(face="plain", lineheight=2, vjust=1))+
        ggtitle("DAU")
    }
    else(return(paste("nothing to display")))
    
  })   
}

  
  
  
  
  
  