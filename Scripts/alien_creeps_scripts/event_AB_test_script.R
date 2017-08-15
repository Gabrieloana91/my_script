### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

### Events Analysis for A/B Tests

### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

#### 1. Inputs ####
# hackers_ids is used to eliminate hackers. 
# Please update the cohort before analysing the results so the cohort is up to date with the latest hackers.

# Insert the date of the start of the event and the name of the events. 

event_0 <- "event_12"
event_0_date <- "2017-07-21"

control <- c("heh8v21")
group_1 <- c("ivcdbv7")
group_2 <- c("fca8nav")
group_3 <- c("l8ck5mz")





#### 2. Parameters ####

hackers_ids <- c("artzosc")
event_0_end <- as.Date(event_0_date)+9

ids <- data.frame(ids = c(control, group_1, group_2, group_3))
ids$group <- c("control", "group_1", "group_2", "group_3")

#### 3. Collecting player numbers ####
###########################################################################
cost <- vector("character")
con <- textConnection("cost", "wr", local = T)
sink(con)
###########################################################################




event_players <- data.frame()
for(i in ids$ids){
  
  temp <- amplitude("AC", event="event_level_finished", measured_by="uniques", show_aggregate_over_date_range=T, show_api_call_cost = T, 
                    user_segment=list(list(prop="userdata_cohort", op="is", values=list(i))),
                    start=event_0_date, end=event_0_end) %>% 
    summarise(event_players = mean(uniques_over_date_range)) %>% 
    mutate(ids = i)
    
  
  event_players <- bind_rows(event_players, temp)
}


#### 3.1 Get number of players grouped by the event level 

level_players <- data.frame()
for(i in ids$ids){
  
  temp <- amplitude("AC", event="event_level_finished", measured_by="uniques", show_aggregate_over_date_range=T, show_api_call_cost = T,
                    group_by_properties = list(list(type = "event", value = "levelName")),
                    user_segment=list(list(prop="userdata_cohort", op="is", values=list(i))),
                    start=event_0_date, end=event_0_end) %>% 
    rename(level = property) %>% 
    group_by(level) %>% 
    summarise(level_players = mean(uniques_over_date_range)) %>% 
    mutate(ids = i)
  
  level_players <- bind_rows(level_players, temp)
}

################## 4. General Performance Metrics #### 

#### 4.1 Spenders


unique_spenders <- data.frame()
for(i in ids$ids){

  temp <- amplitude("AC", event="unverified_revenue", measured_by="uniques", show_aggregate_over_date_range = T, show_api_call_cost = T, 
                    user_segment=list(list(prop="userdata_cohort", op="is", values=list(i))),
                    start=event_0_date, end=event_0_end) %>% 
    summarise(unique_spenders = mean(uniques_over_date_range))
  
  unique_spenders <- bind_rows(unique_spenders, temp)
}

unique_spenders$group <- ids$group


#### 4.2 Revenue 

revenue <- data.frame()
for(i in ids$ids){
  
  temp <- amplitude("AC", event="unverified_revenue", measured_by="sums", show_api_call_cost = T, 
                    group_by_properties=list(list(type="event", value="$revenue"), list(type="event", value="$revenueType")),
                    user_segment=list(list(prop="userdata_cohort", op="is", values=list(i))),
                    start=event_0_date, end=event_0_end) %>% filter(property != "(none)") %>% 
    rename(code = property,
           dt = date) %>% 
    make_currency_values_USD(.,"USD", "code", "value") %>% 
    rename(date = dt) %>% 
    group_by(date) %>% 
    summarise(revenue = sum(revenue)) %>% 
    mutate(ids = i) 

  
  revenue <- bind_rows(revenue, temp)
}



#### 4.3 DAU ####

dau <- data.frame()
for(i in ids$ids){
  
  temp <- amplitude("AC", event="_active", measured_by="uniques", show_api_call_cost = T, 
                   user_segment=list(list(prop="userdata_cohort", op="is", values=list(i))),
                   start=event_0_date, end=event_0_end) %>% 
    mutate(ids = i) %>% 
    rename(dau = value)
  
  dau <- bind_rows(dau, temp)
}

#### 4.4 Conversion Rate ####

spenders <- data.frame()
for(i in ids$ids){
  
  temp <- amplitude("AC", event="unverified_revenue", measured_by="uniques", show_api_call_cost = T, 
                    user_segment=list(list(prop="userdata_cohort", op="is", values=list(i))),
                    start=event_0_date, end=event_0_end) %>% 
    mutate(ids = i) %>% 
    rename(spenders = value)
  
  spenders <- bind_rows(spenders, temp)
}

### 4.5 Stitch data ####

metrics <- left_join(left_join(dau, revenue, by=c("date","ids")), spenders, by = c("date", "ids"))

metrics <- metrics %>% 
  group_by(ids, date) %>% 
  mutate(arpdau = revenue / dau,
          conversion_rate = spenders / dau) %>% 
  group_by(ids) %>% 
  summarise(total_revenue = sum(revenue),
            ave_dau = mean(dau),
            arpdau = mean(arpdau),
            conversion_rate = mean(conversion_rate)) %>% 
  left_join(.,ids,by="ids") %>% 
  left_join(., event_players, by = "ids")


################## 5. Gem Investment #### 

# All gem investment in and outside the event is captured by this magnific beast down here. 

ref_table <- data.frame(boost = c("energyboosts_used", "secondChancesBought", "airstrikesUsed", "reinforcesUsed", "cryobombs_used","superTowerRefillsBought"))

gem_data <- data.frame()

for(k in ref_table$boost){
  for(i in ids$ids){
    
    temp_1 <- amplitude("AC", event="event_level_finished", group_by_properties = list(list(type = "event", value = k), list(type = "event", value = "levelName")),
                        measured_by="totals", start=event_0_date, end=event_0_end, show_api_call_cost = T,
                        user_segment=list(list(prop="userdata_cohort", op="is", values=list(i)))) %>%
      rename(level = property2) %>% 
      mutate(prop_sum_value = value * as.numeric(property)) %>% 
      group_by(date, level) %>% 
      summarise(value = sum(prop_sum_value, na.rm = T)) %>% 
      mutate(ids = i,
             property = k)
    
    if(k == "airstrikesUsed"){
      temp_1 <- temp_1 %>% 
        group_by(date, level) %>% 
        summarise(value = value * 2) %>% 
        mutate(ids = i,
               property = k)
    } else if(k == "cryobombs_used"){
      temp_1 <- temp_1 %>% 
        group_by(date, level) %>% 
        summarise(value = value * 4) %>% 
        mutate(ids = i,
               property = k)
    } else if(k == "secondChancesBought"){
      temp_1 <- temp_1 %>% 
        group_by(date, level) %>% 
        summarise(value = value * 10) %>% 
        mutate(ids = i,
               property = k)
    } else if(k == "reinforcesUsed"){
      temp_1 <- temp_1 %>% 
        group_by(date, level) %>% 
        summarise(value = value * 6) %>% 
        mutate(ids = i,
               property = k)
    } else if(k == "superTowerRefillsBought"){
      temp_1 <- temp_1 %>% 
        group_by(date, level) %>% 
        summarise(value = value * 6) %>% 
        mutate(ids = i,
               property = k)
    } else if(k == "energyboosts_used"){
      temp_1 <- temp_1 %>% 
        group_by(date, level) %>% 
        summarise(value = value * 10) %>% 
        mutate(ids = i,
               property = k)
    }
    
    gem_data <- bind_rows(gem_data, temp_1)
  }
}

### Gem investment per player ###

gem_investment <- gem_data %>% 
  group_by(property, ids) %>% 
  summarise(total_gems_spent = sum(value)) %>% 
  left_join(., event_players, by="ids") %>% 
  group_by(ids, property) %>% 
  summarise(ave_gems_spent = total_gems_spent / event_players) %>% 
  left_join(., ids, by="ids") %>% 
  group_by(group, property) %>% 
  select(-ids)

ggplot(gem_investment, aes(property, ave_gems_spent, fill = group)) + geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(label=round(ave_gems_spent,1)),vjust=-0.5,position=position_dodge(1), size=3.5, colour = "black")+
  ggtitle("Average Gem Investment per Player")


temp <- gem_data %>% 
  group_by(ids, level, property) %>% 
  summarise(gem_investment = sum(value)) %>% 
  left_join(., level_players, by=c("level","ids")) %>% 
  left_join(., ids, by="ids") %>% 
  group_by(group, level, property) %>% 
  summarise(gem_investment_per_player = gem_investment / level_players)

ggplot(temp, aes(level, gem_investment_per_player, fill = group)) + geom_bar(stat = "identity", position = "dodge") + facet_grid(property~.) + 
  geom_text(aes(label=round(gem_investment_per_player,1)),vjust=-0.05,position=position_dodge(1), size=3.5, colour = "black")+
  ggtitle("Average Gem Investment per Player")


temp <- gem_data %>% 
  group_by(ids) %>% 
  summarise(gem_investment = sum(value)) %>% 
  left_join(.,event_players, by="ids") %>% 
  group_by(ids) %>% 
  summarise(gem_investment_per_event_player = gem_investment / event_players)

metrics <- metrics %>% 
  group_by(ids) %>% 
  left_join(., temp, by= "ids")


temp_1 <- gem_data %>% 
  group_by(ids, property) %>% 
  summarise(gem_investment = sum(value)) %>% 
  left_join(., event_players, by="ids") %>% 
  group_by(ids, property) %>% 
  summarise(value = gem_investment / event_players) %>% 
  rename(variable = property)

temp_1$value <- as.character(temp_1$value)

######### 6. Tickets #####

ticket_data <- data.frame()

for(i in ids$ids){
  
  temp <- amplitude("AC", event="unverified_revenue", measured_by="totals", show_api_call_cost = T, 
                    where = list(list(subprop_type = "event", subprop_key = "$productId", subprop_op = "is", subprop_value = "InfiniteEventTicket")),
                    user_segment=list(list(prop="userdata_cohort", op="is", values=list(i))),
                    start=event_0_date, end=event_0_end) %>% 
    rename(infinite_ticket = value) %>% 
    left_join(amplitude("AC", event="unverified_revenue", measured_by="totals", show_api_call_cost = T,
                        where = list(list(subprop_type = "event", subprop_key = "$productId", subprop_op = "is", subprop_value = "EventTicketRefill")),
                        user_segment=list(list(prop="userdata_cohort", op="is", values=list(i))),
                        start=event_0_date, end=event_0_end) %>% 
                rename(refill_ticket = value), by = "date") %>% 
    left_join(amplitude("AC", event="free_gift_received", measured_by="totals", show_api_call_cost = T,
                        where = list(list(subprop_type = "event", subprop_key = "gift_received", subprop_op = "is", subprop_value = "EventTicket")),
                        user_segment=list(list(prop="userdata_cohort", op="is", values=list(i))),
                        start=event_0_date, end=event_0_end) %>% 
                rename(free_ticket = value), by = "date") %>% 
    left_join(amplitude("AC", event="CurrencySpent", measured_by="totals", show_api_call_cost = T,
                        where = list(list(subprop_type = "event", subprop_key = "purchase", subprop_op = "is", subprop_value = "KillBoost")),
                        user_segment=list(list(prop="userdata_cohort", op="is", values=list(i))),
                        start=event_0_date, end=event_0_end) %>% 
                rename(kill_boost = value), by="date") %>% 
    mutate(ids = i)
  
  ticket_data <- bind_rows(temp, ticket_data)
  
}


ticket_data <- ticket_data %>% 
  group_by(ids) %>% 
  summarise(infinite_tickets = sum(infinite_ticket),
            refill_tickets = sum(refill_ticket),
            free_tickets = sum(free_ticket),
            kill_boosts = sum(kill_boost)) %>% 
  left_join(., ids, by="ids") %>% 
  left_join(., event_players, by="ids") %>% 
  group_by(group) %>% 
  summarise(ave_infinite_tickets = infinite_tickets / event_players,
            ave_refill_tickets = refill_tickets / event_players,
            ave_free_tickets = free_tickets / event_players,
            ave_kill_boosts = kill_boosts / event_players)

metrics <- metrics %>% 
  left_join(., ticket_data, by="group")


######### 7. Attempts ###########

attempts <- data.frame()

for(i in ids$ids){
  
  temp <- amplitude("AC", event="event_level_finished", measured_by="totals", show_api_call_cost = T, 
                    user_segment=list(list(prop="userdata_cohort", op="is", values=list(i))),
                    start=event_0_date, end=event_0_end) %>% 
    mutate(ids = i)
  
  attempts <- bind_rows(temp, attempts)
}

attempts <- attempts %>% 
  left_join(., dau, by=c("date", "ids")) %>% 
  group_by(date, ids) %>% 
  summarise(ave_attempts = value / dau) %>% 
  group_by(ids) %>% 
  summarise(ave_attempts = mean(ave_attempts))

metrics <- metrics %>% 
  left_join(., attempts, by="ids")


attempts_data <- data.frame()
for(i in ids$ids){
  
  temp <- amplitude("AC", event="event_level_finished", measured_by="totals", show_api_call_cost = T, 
                    group_by_properties = list(list(type = "event", value = "levelName")),
                    user_segment=list(list(prop="userdata_cohort", op="is", values=list(i))),
                    start=event_0_date, end=event_0_end) %>% 
    mutate(ids = i)
  
  attempts_data <- bind_rows(temp, attempts_data)
}

attempts_data <- attempts_data %>% 
  group_by(property, ids) %>% 
  summarise(attempts = sum(value)) %>% 
  rename(level = property) %>% 
  left_join(., level_players, by=c("ids", "level")) %>% 
  group_by(level, ids) %>% 
  summarise(ave_attempts = attempts / level_players) %>% 
  left_join(., ids, by="ids") %>%
  group_by(group, level) %>% 
  select(-ids)

ggplot(attempts_data, aes(level, ave_attempts, fill = group)) + geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(label=round(ave_attempts,1)),vjust=-0.2,position=position_dodge(1), size=3.5, colour = "black") + 
  ggtitle("Average Attempts per Player")


#### 8. Stitch all data together ####


########################################################################
sink()
close(con)
cost <- as.data.frame(gsub("The cost of your API request is","",cost))
names(cost) <- "cost"
cost$cost <- as.numeric(as.character(cost$cost))
print(paste0("The total API cost for your query is ", sum(cost$cost)))
########################################################################


metrics_temp <- melt(metrics, id.vars=1)

metrics_temp <- bind_rows(metrics_temp, temp_1)

metrics_temp <- metrics_temp %>% 
  filter(variable != "group") %>% 
  left_join(., ids, by="ids") %>% 
  select(-ids)


metrics_temp <- spread(metrics_temp, key = group, value = value)

metrics_temp$variable <- factor(metrics_temp$variable, levels = c("event_players", "ave_dau", "total_revenue", "arpdau", "conversion_rate", "ave_attempts", "gem_investment_per_event_player",
                                                                  "airstrikesUsed", "cryobombs_used", "energyboosts_used", "reinforcesUsed", "secondChancesBought", "superTowerRefillsBought",
                                                                  "ave_free_tickets", "ave_refill_tickets", "ave_infinite_tickets", "ave_kill_boosts"),
                                labels = c("event_players", "ave_dau", "total_revenue", "ave_arpdau", "conversion_rate", "ave_attempts", "gem_investment_per_event_player",
                                           "airstrikesUsed", "cryobombs_used", "energyboosts_used", "reinforcesUsed", "secondChancesBought", "superTowerRefillsBought",
                                           "ave_free_tickets", "ave_refill_tickets", "ave_infinite_tickets", "ave_kill_boosts"))
metrics_temp <- metrics_temp %>% 
  arrange(metrics_temp$variable)


write.csv(metrics_temp, "AB Test Metrics.csv")
