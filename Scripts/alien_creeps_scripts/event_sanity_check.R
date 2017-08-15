


do_event_sanity_check <- function(game) {
  
  # source('~/amplitude-dashboard/dashboard-code/SharedConfig.R')
  
  # 1. Parameters 

  
  ref <- query_olga(paste0("SELECT game_code, event_name, event_id::numeric, start_dt::date FROM events_reference_table WHERE game_code = ","'",game,"'","ORDER BY start_dt DESC LIMIT (5)"),quietly = T) %>% 
    filter(game_code == game & start_dt >= "2017-01-01" & start_dt <= Sys.Date()) %>% 
    select(event_name, start_dt, event_id) %>% 
    rename(date = start_dt)

  load_analytics_parameters(game, compute_max_level = F)
  
  
  # 2. General Metrics
  
  # Start the connection for collecting the printed data. (Used for calculation the total API cost)
  # cost <- vector("character")
  # con <- textConnection("cost", "wr", local = T)
  # sink(con)
  # 
  
  ### DAU
  dau <- data.frame()
  for(date in ref$date){
    
    temp <- amplitude(game, event="_active", measured_by="uniques", show_aggregate_over_date_range=T, show_api_call_cost = T, 
                      start=as.Date(date), end=as.Date(date)) %>% 
      rename(dau = value)
    
    dau <- bind_rows(temp, dau)
  }
  
  ### Revenue 
  revenue <- data.frame()
  for(i in ref$date){
    
    temp <-  amplitude(game, event="unverified_revenue", measured_by="sums", show_api_call_cost = T,
                       group_by_properties=list(list(type="event", value="$revenue"), list(type="event", value="$revenueType")),
                       start=as.Date(i)-1, end=as.Date(i))
     
    revenue <- bind_rows(temp, revenue)
  }
  
  revenue <- revenue %>% 
    rename(dt = date,
           currency_code = property,
           currency_value = value) %>% 
    make_currency_values_USD(., "USD") %>% 
    rename(date = dt) %>% 
    group_by(date) %>% 
    summarise(revenue = sum(revenue, na.rm = T)) 
  
  ### New Users
  new_users <- data.frame()
  for(date in ref$date){
    
    temp <- amplitude(game, event = firstLaunch, measured_by="uniques", show_api_call_cost = T, 
                      start=as.Date(date), end=as.Date(date)) %>% 
      rename(new_users = value)
    
    new_users <- bind_rows(temp, new_users)
  }
  
  ### Event Players
  event_players <- data.frame()
  if(game == "AC"){
    for(date in ref$date){
      
      temp <- amplitude(game, event="event_level_finished", measured_by="uniques", show_api_call_cost = T,
                        start=as.Date(date), end=as.Date(date)) %>% 
        rename(event_players = value)
      
      event_players <- bind_rows(temp, event_players)
    }
  } else if(game == "CT"){
    for(date in ref$date){
    
      temp <- amplitude(game, event= "level_account", measured_by="uniques", show_api_call_cost = T,
                        where = list(list(subprop_type = "event", subprop_key = "mode", subprop_op = "is", subprop_value = "event")),
                        start=as.Date(date), end=as.Date(date)) %>% 
        rename(event_players = value)
      
      event_players <- bind_rows(temp, event_players)
    }
  }
  
  ### Spenders
  unique_spenders <- data.frame()
  for(date in ref$date){
    
    temp <- amplitude(game, event="unverified_revenue", measured_by="uniques", show_api_call_cost = T,
                      start=as.Date(date), end=as.Date(date)) %>% 
      rename(unique_spenders = value)
    
    unique_spenders <- bind_rows(temp, unique_spenders)
  }
  
  ### Stiching all metrics
  metrics <- join_all(list(dau, revenue, new_users, event_players, unique_spenders, ref), by="date")
  
  metrics <<- metrics %>% 
    group_by(event_name) %>% 
    mutate(ARPU = revenue / dau,
              conversion_rate = unique_spenders / dau)
  metrics <- as.data.frame(metrics)
  
  ### Purchases
  
  purchases <- data.frame()
  for(date in ref$date){
    
    temp <- amplitude(game, event="unverified_revenue", measured_by="uniques", show_api_call_cost = T,
                      group_by_properties=list(list(type="event", value="$revenue"), list(type = "event", value = "$productId")),
                      start=as.Date(date), end=as.Date(date)) %>% 
      rename(dt = date,
             product_id = property2,
             purchases = value) %>% 
      group_by(product_id, dt) %>% 
      summarise(purchases = sum(purchases)) %>% 
      rename(date = dt) %>% 
      inner_join(.,ref, by= "date") %>% 
      select(-date, -event_name)
    
    purchases <- bind_rows(temp, purchases)
  }
  
  purchases <<- as.data.frame(spread(data = purchases, key = event_id, value = purchases))
  
  
  rm(dau, event_players, new_users, revenue, temp, unique_spenders)
  
  
  # 3. segmented Offers
  segment <- data.frame()
  if(game == "AC"){
    for(date in ref$date){
      
      temp <- amplitude(game, event="store_interaction", measured_by="totals", show_aggregate_over_date_range=T, show_api_call_cost = T, 
                        group_by_properties = list(list(type = "event", value = "context")),
                        start=as.Date(date), end=as.Date(date)) %>% 
        rename(segment = value)
      
      segment <- bind_rows(temp, segment)
    }
    
    segment <- segment %>% 
      filter(grepl("SegmentedOffer",property) | grepl("EventTicketShop", property)) %>% 
      inner_join(., ref, by="date") %>% 
      select(-date, -event_name) %>% 
      group_by(property) 
    
    segmented_offers <<- as.data.frame(spread(segment, event_id, segment))
  }
  
  

  
  
  
  # Closing off the connection for print caption and printing the total cost of the function. 
  # sink()
  # close(con)
  # cost <- as.data.frame(gsub("The cost of your API request is","",cost))
  # names(cost) <- "cost"
  # cost$cost <- as.numeric(as.character(cost$cost))
  # print(paste0("The total API cost for your query is ", sum(cost$cost)))
  # 
  # Outputting the results

  write.xlsx2(metrics, file = "event_sanity_check.xlsx", sheetName = "metrics")
  write.xlsx2(purchases, file = "event_sanity_check.xlsx", sheetName = "purchases", append = T)
  
  if(game == "AC"){
    write.xlsx2(segmented_offers, file = "event_sanity_check.xlsx", sheetName = "segmented_offers", append = T)
  }
}



