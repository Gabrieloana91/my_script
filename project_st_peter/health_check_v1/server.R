source("C:\\Users\\gabriel.oana\\Documents\\my_script\\project_st_peter\\health_check_v1\\controller.R")

function(input, output){
  
  output$pass_rate <- renderPlot({
    
    if(input$game_code == input$game_code){
      
      x <- df %>%
        filter(game_code == input$game_code & event == "level_account") %>%
        filter(level_id %in% c(input$level_range[1] : input$level_range[2])) %>%
        group_by(cohort_id, level_id) %>%
        summarise(win_rate = length(unique(player_id[level_result == "won"])) / length(unique(player_id)) * 100) %>%
        filter(win_rate != "NaN")
      
      ggplot(x, aes(level_id, win_rate, fill = cohort_id)) + geom_bar(stat = "identity", position = "dodge")+
        ggtitle("Pass Rate by Level")+
        xlab("Current Level") +
        ylab("Win Rate")
    }
  })
  
  
  output$metrics <- renderTable({
    if(input$game_code == input$game_code){
      y <- df %>%
        filter(game_code == input$game_code) %>%
        filter(level_id %in% c(input$level_range[1] : input$level_range[2])) %>%
        group_by(cohort_id) %>%
        summarise(Players = round(length(unique(player_id)),1),
                  Payers = length(unique(player_id[revenue > 0])),
                  Conversion_Rate = round(length(unique(player_id[revenue > 0])) / length(unique(player_id)) * 100,2),
                  Total_Revenue = sum(revenue),
                  ARPPU = sum(revenue) / length(unique(player_id[revenue > 0])))
      
      y <- melt(y, id.vars = 1)
      y <- spread(data = y, key=cohort_id, value = value)
      y
      
    }
    
  })
  
}