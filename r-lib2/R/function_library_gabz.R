

do_sale_postmortem_CT <- function(sale_name, sale_date_start, sale_duration = "2", comparison_before, comparison_after){
  
  # This function performs a post-mortem analysis for sale events in Castle Creeps Only. 
  # The inputs are sale_name which is used for writing the csv file, sale_duration is expressed as a number of days. 
  # Comparison before and after are expressed in days. Eg: 14 if you would like to compare the sale period to a simillar period 14 days ago. 
  # Comparison after is set for 7 days however, if there is no week ahead to compare with, all metrics are excluded over the "next week" period. 
  
  
  # Parameters
  
  sale_duration <- as.numeric(sale_duration)
  sale_end = as.Date(sale_date_start) + sale_duration - 1
  
  week_before_start = as.Date(comparison_before)
  week_before_end = as.Date(comparison_before) + sale_duration - 1
  
  week_after_start = as.Date(sale_date_start)+as.numeric(comparison_after)
  week_after_end = as.Date(sale_end)+as.numeric(comparison_after)
  
  hackers_ids <- c("38egmj1")
  
  source('~/amplitude-dashboard/dashboard-code/SharedConfig.R')
  
  # Extended Date Range #
  
  ext_date_start <- as.Date(sale_date_start) - 70
  ext_date_end <- Sys.Date()-1
  
  
  # Start the connection for collecting all amplitude API cost
  
  cost <- vector("character")
  con <- textConnection("cost", "wr", local = T)
  sink(con)
  
  ############################################# -> Revenue 
  
  revenue <- amplitude("CT", event="unverified_revenue", measured_by="sums",
                       group_by_properties=list(list(type="event", value="$revenue"), list(type="event", value="$revenueType")),
                       # user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                       start=ext_date_start, end=Sys.Date()-1) %>% 
    filter(property != "(none)") %>% 
    rename(code = property,
           dt = date) %>% 
    make_currency_values_USD(., "USD", "code", "value") %>% 
    rename(date = dt) %>% 
    group_by(date) %>% 
    summarise(revenue = sum(revenue, na.rm = T))
  
  
  ############################################# -> Players
  
  players <- amplitude("CT", event="_active", measured_by="uniques", show_aggregate_over_date_range = T, 
                       user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                       start=sale_date_start, end=sale_end) %>% 
    bind_rows(amplitude("CT", event="_active", measured_by="uniques", show_aggregate_over_date_range = T, 
                        user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                        start=week_before_start, end=week_before_end)) %>% 
    mutate(group = ifelse(date >= week_before_start & date <= week_before_end, "week_before",
                          ifelse(date >= sale_date_start & date <= sale_end, sale_name,
                                 ifelse(date >= week_after_start & date <= week_after_end, "week_after", NA)))) %>% 
    group_by(group) %>% 
    summarise(unique_players = mean(uniques_over_date_range))
  
  if(as.Date(week_after_end) < Sys.Date()-1){
    
    temp <- amplitude("CT", event="_active", measured_by="uniques", show_aggregate_over_date_range = T, 
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                      start=week_after_start, end=week_after_end) %>% 
      mutate(group = ifelse(date >= week_before_start & date <= week_before_end, "week_before",
                            ifelse(date >= sale_date_start & date <= sale_end, sale_name,
                                   ifelse(date >= week_after_start & date <= week_after_end, "week_after", NA)))) %>% 
      group_by(group) %>% 
      summarise(unique_players = mean(uniques_over_date_range))
    
    players <- bind_rows(players, temp)
    
  }
  
  ############################################# -> Unique Spenders
  
  spender <- amplitude("CT", event="unverified_revenue", measured_by="uniques", show_aggregate_over_date_range = T,
                       user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                       start=sale_date_start, end=sale_end) %>% 
    bind_rows(amplitude("CT", event="unverified_revenue", measured_by="uniques", show_aggregate_over_date_range = T,
                        user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                        start=week_before_start, end=week_before_end)) %>% 
    mutate(group = ifelse(date >= week_before_start & date <= week_before_end, "week_before",
                          ifelse(date >= sale_date_start & date <= sale_end, sale_name,
                                 ifelse(date >= week_after_start & date <= week_after_end, "week_after", NA)))) %>% 
    rename(daily_spenders = value,
           unique_spenders = uniques_over_date_range)
  
  
  if(as.Date(week_after_end) < Sys.Date()-1){
    
    temp <- amplitude("CT", event="unverified_revenue", measured_by="uniques", show_aggregate_over_date_range = T,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                      start=week_after_start, end=week_after_end) %>% 
      mutate(group = ifelse(date >= week_before_start & date <= week_before_end, "week_before",
                            ifelse(date >= sale_date_start & date <= sale_end, sale_name,
                                   ifelse(date >= week_after_start & date <= week_after_end, "week_after", NA)))) %>% 
      rename(daily_spenders = value,
             unique_spenders = uniques_over_date_range)
    
    spender <- bind_rows(spender, temp)
    
  }
  
  ############################################# -> Join All Datasets
  
  metrics <- join_all(dfs = list(revenue,spender), by="date")
  metrics <- left_join(metrics, players, by="group")
  
  rm(players, revenue, spender)
  
  ############################################# -> ARPDAU, ARPPU and Conversion Rate
  
  
  metrics <- metrics %>% 
    left_join(amplitude("CT", event="_active", measured_by="uniques", 
                        user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                        start=ext_date_start, end=ext_date_end) %>% 
                rename(dau = value), by="date") %>% 
    mutate(ARPDAU = revenue / dau,
           ARPPU = revenue / daily_spenders,
           conversion_rate = daily_spenders / dau)
  
  
  
  
  
  
  # ACTIVE % #
  
  active <- amplitude("CT", event="currency_spent", group_by_properties = list(list(type = "event", value = "purchase")),
                      measured_by="pct_dau", start=week_before_start, end=Sys.Date()-1,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
    filter(property %in% c("coin", "hero_key", "play_on", "pp_boostSupplies", "pp_reinforce", "sentinel_charge", "ticket")) %>% 
    mutate(group = ifelse(date >= week_before_start & date <= week_before_end,"week_before",
                          ifelse(date >= sale_date_start & date <= sale_end, sale_name,
                                 ifelse(date >= week_after_start & date <= week_after_end, "week_after", NA)))) %>% 
    group_by(group, property) %>% 
    filter(!is.na(group)) %>% 
    summarise(active_prc = mean(value, na.rm = T))
  
  
  # Average purchases per user purchasing at least once #
  
  ave_purchases <- amplitude("CT", event="currency_spent", group_by_properties = list(list(type = "event", value = "purchase")),
                             measured_by="totals", start=week_before_start, end=Sys.Date()-1,
                             user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))))%>% 
    filter(property %in% c("coin", "hero_key", "play_on", "pp_boostSupplies", "pp_reinforce", "sentinel_charge", "ticket")) %>% 
    rename(totals = value) %>% 
    left_join(amplitude("CT", event="currency_spent",
                        measured_by="uniques", start=week_before_start, end=Sys.Date()-1,
                        user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
                select(value,date) %>% 
                rename(uniques = value), by="date") %>% 
    mutate(group = ifelse(date >= week_before_start & date <= week_before_end,"week_before",
                          ifelse(date >= sale_date_start & date <= sale_end, sale_name,
                                 ifelse(date >= week_after_start & date <= week_after_end, "week_after", NA)))) %>% 
    group_by(group, property) %>% 
    filter(!is.na(group)) %>% 
    summarise(ave_purchases_per_user = mean(totals)/mean(uniques)) %>% 
    left_join(., active, by=c("group","property")) 
  
  
  # Gem Investment #
  
  gem_data <- amplitude("CT", event="level_account", group_by_properties = list(list(type = "event", value = "energyboost_used")),
                        measured_by="totals", start=ext_date_start, end=ext_date_end,
                        user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
    mutate(prop_sum = value*as.numeric(property)) %>% 
    group_by(date) %>% 
    summarise(elixir = sum(prop_sum, na.rm=T) * 24) %>% 
    left_join(amplitude("CT", event="level_account", group_by_properties = list(list(type = "event", value = "sentinel_purchased")),
                        measured_by="totals", start=ext_date_start, end=ext_date_end,
                        user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
                mutate(prop_sum = value*as.numeric(property)) %>% 
                group_by(date) %>% 
                summarise(sentinel = sum(prop_sum, na.rm=T) * 8), by = "date") %>% 
    left_join(amplitude("CT", event="level_account", group_by_properties = list(list(type = "event", value = "reinforcements_used")),
                        measured_by="totals", start=ext_date_start, end=ext_date_end,
                        user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
                mutate(prop_sum = value*as.numeric(property)) %>% 
                group_by(date) %>% 
                summarise(reinforcements = sum(prop_sum, na.rm=T) * 8), by = "date") %>% 
    left_join(amplitude("CT", event="level_account", group_by_properties = list(list(type = "event", value = "play_on_offered")),
                        where = list(list(subprop_type = "event", subprop_key = "play_on_offered", subprop_op = "greater", subprop_value=list("0")),
                                     list(subprop_type = "event", subprop_key = "outcome", subprop_op = "is", subprop_value=list("loss"))),
                        measured_by="totals", start=ext_date_start, end=ext_date_end,
                        user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
                mutate(play_on_totals = value * as.numeric(property)) %>% 
                group_by(date) %>% 
                summarise(play_on_totals = sum(play_on_totals,na.rm=T)) %>% 
                left_join(amplitude("CT", event="level_account", group_by_properties = list(list(type = "event", value = "play_on_offered")),
                                    measured_by="totals", start=ext_date_start, end=ext_date_end,
                                    user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
                            mutate(play_on = value * as.numeric(property)) %>% 
                            group_by(date) %>% 
                            summarise(play_on = sum(play_on, na.rm = T)), by="date") %>% 
                group_by(date) %>% 
                summarise(play_on = (play_on - play_on_totals) * 60), by="date") %>% 
    left_join(metrics, by="date") %>% 
    mutate(ave_elixir = elixir / dau,
           ave_reinforcements = reinforcements / dau,
           ave_sentinel = sentinel / dau,
           ave_play_on = play_on / dau,
           ave_gem_investment = (elixir + reinforcements + sentinel + play_on) / dau)
  
  
  metrics <- gem_data %>%
    filter(!is.na(group)) %>% 
    group_by(group) %>% 
    summarise(total_revenue = sum(revenue),
              total_players = mean(unique_players),
              DAU = mean(dau),
              spenders = mean(unique_spenders),
              ARPDAU = mean(ARPDAU),
              ARPPU = mean(ARPPU),
              conversion_rate = mean(conversion_rate),
              ave_elixir = mean(ave_elixir),
              ave_reinforcements = mean(ave_reinforcements),
              ave_sentinel = mean(ave_sentinel),
              ave_play_on = mean(ave_play_on),
              ave_gem_investment = mean(ave_gem_investment)) %>% 
    left_join(ave_purchases, by="group")
  
  
  # Graphs # 
  
  # 1. Average gem investment # 
  
  temp <- melt(gem_data[,c(1,15:18)], id.vars = 1)
  
  p1 <- ggplot(temp, aes(date[date <= Sys.Date()-1],value, colour = variable)) + geom_line(size = 1) +
    geom_vline(xintercept = as.numeric(temp$date[temp$date == sale_date_start]), linetype = "dashed", colour = "coral4") +
    geom_vline(xintercept = as.numeric(temp$date[temp$date == sale_end]), linetype = "dashed", colour = "coral4") +
    annotate("text",x = as.Date(sale_date_start),y=max(temp$value)-0.3*max(temp$value), label = paste0("Start ",sale_name), angle = 90, size = 4) +
    annotate("text",x = as.Date(sale_end),y=max(temp$value)-0.3*max(temp$value), label = paste0("End ",sale_name), angle = 90, size = 4) +
    
    geom_vline(xintercept = as.numeric(temp$date[temp$date == week_before_start]), linetype = "dashed", colour = "coral4") +
    annotate("text",x = as.Date(week_before_start),y=max(temp$value)-0.3*max(temp$value), label = paste0("Start ","week before"), angle = 90, size = 4) +
    geom_vline(xintercept = as.numeric(temp$date[temp$date == week_before_end]), linetype = "dashed", colour = "coral4") +
    annotate("text",x = as.Date(week_before_end),y=max(temp$value)-0.3*max(temp$value), label = paste0("End ","week before"), angle = 90, size = 4) +
    
    geom_vline(xintercept = as.numeric(temp$date[temp$date == week_after_start]), linetype = "dashed", colour = "coral4") +
    annotate("text",x = as.Date(week_after_start),y=max(temp$value)-0.3*max(temp$value), label = paste0("Start ","week after"), angle = 90, size = 4) +
    geom_vline(xintercept = as.numeric(temp$date[temp$date == week_after_end]), linetype = "dashed", colour = "coral4")+
    annotate("text",x = as.Date(week_after_end),y=max(temp$value)-0.3*max(temp$value), label = paste0("End ","week after"), angle = 90, size = 4) +
    
    scale_x_date(name = "Date",limits = c(min(temp$date), Sys.Date()-1), date_breaks = "2 day")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_y_continuous(name = "Average Gem Investment")+
    ggtitle("Average Gem Investment per Player")
  
  print(p1)
  
  # 2. ARPDAU # 
  
  temp <- gem_data[,c(1,12)]
  
  p2 <- ggplot(temp, aes(date[date <= Sys.Date()-1],ARPDAU)) + geom_line(size = 1, colour = "darkorchid3") +
    geom_vline(xintercept = as.numeric(temp$date[temp$date == sale_date_start]), linetype = "dashed", colour = "coral4") +
    geom_vline(xintercept = as.numeric(temp$date[temp$date == sale_end]), linetype = "dashed", colour = "coral4") +
    annotate("text",x = as.Date(sale_date_start),y=max(temp$ARPDAU)-0.3*max(temp$ARPDAU), label = paste0("Start ",sale_name), angle = 90, size = 4) +
    annotate("text",x = as.Date(sale_end),y=max(temp$ARPDAU)-0.3*max(temp$ARPDAU), label = paste0("End ",sale_name), angle = 90, size = 4) +
    
    geom_vline(xintercept = as.numeric(temp$date[temp$date == week_before_start]), linetype = "dashed", colour = "coral4") +
    annotate("text",x = as.Date(week_before_start),y=max(temp$ARPDAU)-0.3*max(temp$ARPDAU), label = paste0("Start ","week before"), angle = 90, size = 4) +
    geom_vline(xintercept = as.numeric(temp$date[temp$date == week_before_end]), linetype = "dashed", colour = "coral4") +
    annotate("text",x = as.Date(week_before_end),y=max(temp$ARPDAU)-0.3*max(temp$ARPDAU), label = paste0("End ","week before"), angle = 90, size = 4) +
    
    geom_vline(xintercept = as.numeric(temp$date[temp$date == week_after_start]), linetype = "dashed", colour = "coral4") +
    annotate("text",x = as.Date(week_after_start),y=max(temp$ARPDAU)-0.3*max(temp$ARPDAU), label = paste0("Start ","week after"), angle = 90, size = 4) +
    geom_vline(xintercept = as.numeric(temp$date[temp$date == week_after_end]), linetype = "dashed", colour = "coral4")+
    annotate("text",x = as.Date(week_after_end),y=max(temp$ARPDAU)-0.3*max(temp$ARPDAU), label = paste0("End ","week after"), angle = 90, size = 4) +
    
    scale_x_date(name = "Date",limits = c(min(temp$date), Sys.Date()-1), date_breaks = "2 day")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_y_continuous(name = "ARPDAU")+
    ggtitle("Net ARPDAU")
  
  print(p2)
  
  
  # 3. Conversion Rate #  
  
  temp <-  amplitude("CT", event="unverified_revenue",
                     measured_by="pct_dau", start=ext_date_start, end=ext_date_end,
                     user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
    select(value,date) %>% 
    filter(date <= Sys.Date() - 1)
  
  p3 <- ggplot(temp, aes(date[date <= Sys.Date()-1],value)) + geom_line(size = 1, colour = "darkorchid3") +
    geom_vline(xintercept = as.numeric(temp$date[temp$date == sale_date_start]), linetype = "dashed", colour = "coral4") +
    geom_vline(xintercept = as.numeric(temp$date[temp$date == sale_end]), linetype = "dashed", colour = "coral4") +
    annotate("text",x = as.Date(sale_date_start),y=max(temp$value)-0.3*max(temp$value), label = paste0("Start ",sale_name), angle = 90, size = 4) +
    annotate("text",x = as.Date(sale_end),y=max(temp$value)-0.3*max(temp$value), label = paste0("End ",sale_name), angle = 90, size = 4) +
    
    geom_vline(xintercept = as.numeric(temp$date[temp$date == week_before_start]), linetype = "dashed", colour = "coral4") +
    annotate("text",x = as.Date(week_before_start),y=max(temp$value)-0.3*max(temp$value), label = paste0("Start ","week before"), angle = 90, size = 4) +
    geom_vline(xintercept = as.numeric(temp$date[temp$date == week_before_end]), linetype = "dashed", colour = "coral4") +
    annotate("text",x = as.Date(week_before_end),y=max(temp$value)-0.3*max(temp$value), label = paste0("End ","week before"), angle = 90, size = 4) +
    
    geom_vline(xintercept = as.numeric(temp$date[temp$date == week_after_start]), linetype = "dashed", colour = "coral4") +
    annotate("text",x = as.Date(week_after_start),y=max(temp$value)-0.3*max(temp$value), label = paste0("Start ","week after"), angle = 90, size = 4) +
    geom_vline(xintercept = as.numeric(temp$date[temp$date == week_after_end]), linetype = "dashed", colour = "coral4")+
    annotate("text",x = as.Date(week_after_end),y=max(temp$value)-0.3*max(temp$value), label = paste0("End ","week after"), angle = 90, size = 4) +
    
    scale_x_date(name = "Date",limits = c(min(temp$date), Sys.Date()-1), date_breaks = "2 day")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_y_continuous(name = "Conversion Rate")+
    ggtitle("Conversion Rate")
  
  print(p3)
  
  # 4. Revenue by Product_id #
  
  temp <- amplitude("CT", event="unverified_revenue", measured_by="sums",
                    group_by_properties=list(list(type="event", value="$revenue"), list(type="event", value="$productId"), list(type="event", value="$revenueType")),
                    user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                    start=ext_date_start, end=ext_date_end) %>% 
    separate(., col = property, into=c("product_id","code"), sep="; ") %>% 
    rename(dt = date) %>% 
    make_currency_values_USD(.,"USD","code","value") %>% 
    rename(date = dt) %>% 
    group_by(date, product_id) %>% 
    summarise(revenue = sum(revenue)) %>% 
    mutate(group = ifelse(date >= week_before_start & date <= week_before_end,"week_before",
                          ifelse(date >= sale_date_start & date <= sale_end, sale_name,
                                 ifelse(date >= week_after_start & date <= week_after_end, "week_after", NA)))) %>% 
    filter(!is.na(group) & revenue > 20) %>% 
    group_by(product_id, group) %>% 
    summarise(revenue = sum(revenue,na.rm = T))
  
  
  p4 <- ggplot(temp, aes(product_id, revenue, fill=group)) +
    geom_bar(stat="identity", position="dodge", colour = "darkblue") +
    scale_fill_manual(values = c("chartreuse4","dodgerblue4","darkorange3"))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    ggtitle("Revenue (gross USD) per product_id")
  
  print(p4)
  
  # 5. Currency conversion percentage # 
  
  prc <-  amplitude("CT", event="currency_spent", group_by_properties = list(list(type = "event", value = "purchase")),
                    measured_by="pct_dau", start=ext_date_start, end=ext_date_end,
                    user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
    filter(date <= Sys.Date() - 1) %>% 
    filter(property %in% c("coin", "hero_key", "play_on", "pp_boostSupplies", "pp_reinforce", "sentinel_charge", "ticket"))
  
  p5 <- ggplot(prc, aes(date,value, colour = property)) + geom_line(size = 1) + 
    geom_vline(xintercept = as.numeric(prc$date[prc$date == sale_date_start]), linetype = "dashed", colour = "coral4") +
    geom_vline(xintercept = as.numeric(prc$date[prc$date == sale_end]), linetype = "dashed", colour = "coral4") +
    annotate("text",x = as.Date(sale_date_start),y=max(prc$value)-0.5*max(prc$value), label = paste0("Start ",sale_name), angle = 90, size = 4) +
    annotate("text",x = as.Date(sale_end),y=max(prc$value)-0.5*max(prc$value), label = paste0("End ",sale_name), angle = 90, size = 4) +
    
    geom_vline(xintercept = as.numeric(prc$date[prc$date == week_before_start]), linetype = "dashed", colour = "coral4") +
    annotate("text",x = as.Date(week_before_start),y=max(prc$value)-0.5*max(prc$value), label = paste0("Start ","week before"), angle = 90, size =4) +
    geom_vline(xintercept = as.numeric(prc$date[prc$date == week_before_end]), linetype = "dashed", colour = "coral4") +
    annotate("text",x = as.Date(week_before_end),y=max(prc$value)-0.5*max(prc$value), label = paste0("End ","week before"), angle = 90, size =4) +
    
    geom_vline(xintercept = as.numeric(prc$date[prc$date == week_after_start]), linetype = "dashed", colour = "coral4") +
    annotate("text",x = as.Date(week_after_start),y=max(prc$value)-0.5*max(prc$value), label = paste0("Start ","week after"), angle = 90, size =4) +
    geom_vline(xintercept = as.numeric(prc$date[prc$date == week_after_end]), linetype = "dashed", colour = "coral4") +
    annotate("text",x = as.Date(week_after_end),y=max(prc$value)-0.5*max(prc$value), label = paste0("End ","week after"), angle = 90, size =4) +
    
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_x_date(name = "Date",limits = c(min(prc$date), Sys.Date()-1), date_breaks = "2 day")+
    ggtitle("Percentage of players converting their gems into boosts") 
  
  print(p5)
  rm(active, gem_data, prc, temp)  
  
  write.csv(metrics, paste0("CT_", sale_name, "_report","_", Sys.Date(), ".csv"))
  return(metrics)
  
  ave_purchases
  
  sink()
  close(con)
  cost <- as.data.frame(gsub("The cost of your API request is","",cost))
  names(cost) <- "cost"
  cost$cost <- as.numeric(as.character(cost$cost))
  print(paste0("The total API cost for your query is ", sum(cost$cost)))
  
}

rquery.cormat<-function(x, type=c('lower', 'upper', 'full', 'flatten'), graph=TRUE, graphType=c("correlogram", "heatmap"), col=NULL, ...){
  library(corrplot)
  # Helper functions
  #+++++++++++++++++
  # Compute the matrix of correlation p-values
  cor.pmat <- function(x, ...) {
    mat <- as.matrix(x)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  # Get lower triangle of the matrix
  getLower.tri<-function(mat){
    upper<-mat
    upper[upper.tri(mat)]<-""
    mat<-as.data.frame(upper)
    mat
  }
  # Get upper triangle of the matrix
  getUpper.tri<-function(mat){
    lt<-mat
    lt[lower.tri(mat)]<-""
    mat<-as.data.frame(lt)
    mat
  }
  # Get flatten matrix
  flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
      row = rownames(cormat)[row(cormat)[ut]],
      column = rownames(cormat)[col(cormat)[ut]],
      cor  =(cormat)[ut],
      p = pmat[ut]
    )
  }
  # Define color
  if (is.null(col)) {
    col <- colorRampPalette(
      c("#67001F", "#B2182B", "#D6604D", "#F4A582",
        "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", 
        "#4393C3", "#2166AC", "#053061"))(200)
    col<-rev(col)
  }
  
  # Correlation matrix
  cormat<-signif(cor(x, use = "complete.obs", ...),2)
  pmat<-signif(cor.pmat(x, ...),2)
  # Reorder correlation matrix
  ord<-corrMatOrder(cormat, order="hclust")
  cormat<-cormat[ord, ord]
  pmat<-pmat[ord, ord]
  # Replace correlation coeff by symbols
  sym<-symnum(cormat, abbr.colnames=FALSE)
  # Correlogram
  if(graph & graphType[1]=="correlogram"){
    corrplot(cormat, type=ifelse(type[1]=="flatten", "lower", type[1]),
             tl.col="black", tl.srt=45,col=col,...)
  }
  else if(graphType[1]=="heatmap")
    heatmap(cormat, col=col, symm=TRUE)
  # Get lower/upper triangle
  if(type[1]=="lower"){
    cormat<-getLower.tri(cormat)
    pmat<-getLower.tri(pmat)
  }
  else if(type[1]=="upper"){
    cormat<-getUpper.tri(cormat)
    pmat<-getUpper.tri(pmat)
    sym=t(sym)
  }
  else if(type[1]=="flatten"){
    cormat<-flattenCorrMatrix(cormat, pmat)
    pmat=NULL
    sym=NULL
  }
  list(r=cormat, p=pmat, sym=sym)
}


stat_smooth_func <- function(mapping = NULL, data = NULL,
                             geom = "smooth", position = "identity",
                             ...,
                             method = "auto",
                             formula = y ~ x,
                             se = TRUE,
                             n = 80,
                             span = 0.75,
                             fullrange = FALSE,
                             level = 0.95,
                             method.args = list(),
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE,
                             xpos = NULL,
                             ypos = NULL) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatSmoothFunc,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      formula = formula,
      se = se,
      n = n,
      fullrange = fullrange,
      level = level,
      na.rm = na.rm,
      method.args = method.args,
      span = span,
      xpos = xpos,
      ypos = ypos,
      ...
    )
  )
}


StatSmoothFunc <- ggproto("StatSmooth", Stat,
                          
                          setup_params = function(data, params) {
                            # Figure out what type of smoothing to do: loess for small datasets,
                            # gam with a cubic regression basis for large data
                            # This is based on the size of the _largest_ group.
                            if (identical(params$method, "auto")) {
                              max_group <- max(table(data$group))
                              
                              if (max_group < 1000) {
                                params$method <- "loess"
                              } else {
                                params$method <- "gam"
                                params$formula <- y ~ s(x, bs = "cs")
                              }
                            }
                            if (identical(params$method, "gam")) {
                              params$method <- mgcv::gam
                            }
                            
                            params
                          },
                          
                          compute_group = function(data, scales, method = "auto", formula = y~x,
                                                   se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                                                   xseq = NULL, level = 0.95, method.args = list(),
                                                   na.rm = FALSE, xpos=NULL, ypos=NULL) {
                            if (length(unique(data$x)) < 2) {
                              # Not enough data to perform fit
                              return(data.frame())
                            }
                            
                            if (is.null(data$weight)) data$weight <- 1
                            
                            if (is.null(xseq)) {
                              if (is.integer(data$x)) {
                                if (fullrange) {
                                  xseq <- scales$x$dimension()
                                } else {
                                  xseq <- sort(unique(data$x))
                                }
                              } else {
                                if (fullrange) {
                                  range <- scales$x$dimension()
                                } else {
                                  range <- range(data$x, na.rm = TRUE)
                                }
                                xseq <- seq(range[1], range[2], length.out = n)
                              }
                            }
                            # Special case span because it's the most commonly used model argument
                            if (identical(method, "loess")) {
                              method.args$span <- span
                            }
                            
                            if (is.character(method)) method <- match.fun(method)
                            
                            base.args <- list(quote(formula), data = quote(data), weights = quote(weight))
                            model <- do.call(method, c(base.args, method.args))
                            
                            m = model
                            eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                                             list(a = format(coef(m)[1], digits = 3), 
                                                  b = format(coef(m)[2], digits = 3), 
                                                  r2 = format(summary(m)$r.squared, digits = 3)))
                            func_string = as.character(as.expression(eq))
                            
                            if(is.null(xpos)) xpos = min(data$x)*0.9
                            if(is.null(ypos)) ypos = max(data$y)*0.9
                            data.frame(x=xpos, y=ypos, label=func_string)
                            
                          },
                          
                          required_aes = c("x", "y")
)
