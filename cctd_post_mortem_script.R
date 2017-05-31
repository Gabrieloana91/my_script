  
# Sale Postmortem Script


### 1. Inputs ####

sale_start = "2017-05-29"
sale_end = "2017-05-30"

sale_identifier = "Flash Gem Sale"

cohort_ids <- c("38egmj1")

working_directory <- "D:\\R Working File\\2. CCTD"

week_before_start = as.Date(sale_start)-14
week_before_end = as.Date(sale_end)-14

week_after_start = as.Date(sale_start)+7
week_after_end = as.Date(sale_end)+7

### 2. Parameters for comparison ####


source('~/amplitude-dashboard/dashboard-code/SharedConfig.R')

setwd(working_directory)

# Extended Date Range #

ext_date_start <- as.Date(sale_start) - 30
ext_date_end <- as.Date(sale_end) + 7


### 3. KPI's ####


############################################# -> Revenue 

revenue <- amplitude("CT", event="unverified_revenue", measured_by="sums",
                  group_by_properties=list(list(type="event", value="$revenue"), list(type="event", value="$revenueType")),
                  user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids))),
                  start=ext_date_start, end=Sys.Date()) %>% 
  filter(property != "(none)") %>% 
  rename(code = property)


conv <- get_exchange_rates(unique(revenue$code), toCurr="USD", fromDt=ext_date_start, toDt=ext_date_end)

revenue <- left_join(revenue, conv, by=c("code", "date"))
revenue$revenue <- revenue$value * revenue$exch 
revenue <- revenue %>% 
  group_by(date) %>% 
  summarise(revenue = sum(revenue, na.rm = T))



############################################# -> Players


player <- amplitude("CT", event="_active", measured_by="uniques", 
                    user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids))),
                    start=ext_date_start, end=Sys.Date()) %>% 
  select(value,date) %>% 
  rename(players = value)

############################################# -> Total Spenders

spender <- amplitude("CT", event="unverified_revenue", measured_by="uniques",
                     user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids))),
                     start=ext_date_start, end=Sys.Date()) %>% 
  select(value,date) %>% 
  rename(spenders = value)

############################################# -> Join All Datasets

metrics <- join_all(dfs = list(revenue,player,spender), by="date")

metrics$group <- ifelse(metrics$date >= week_before_start & metrics$date <= week_before_end, "week_before",
                        ifelse(metrics$date >= sale_start & metrics$date <= sale_end, sale_identifier,
                               ifelse(metrics$date >= week_after_start & metrics$date <= week_after_end, "week_after", NA)))

metrics <- metrics %>% 
  filter(!is.na(group)) %>% 
  group_by(group) %>% 
  summarise(total_revenue = sum(revenue, na.rm = T),
            total_players = sum(players,na.rm = T),
            total_spenders = sum(spenders,na.rm = T),
            percent_spenders = sum(spenders,na.rm = T) / sum(players,na.rm = T))

############################################# -> Average DAU

time_difference = as.numeric(as.POSIXlt(sale_end) - as.POSIXlt(sale_start)) + 1

metrics <- metrics %>% 
  group_by(group) %>% 
  mutate(ave_DAU = total_players / time_difference,
         ARPU = total_revenue / total_players)

############################################# -> Average ARPDAU

metrics <- metrics %>% 
  group_by(group) %>% 
  mutate(ave_ARPDAU = total_revenue / ave_DAU)





#### 4. Active % ####


active <- amplitude("CT", event="currency_spent", group_by_properties = list(list(type = "event", value = "purchase")),
                    measured_by="pct_dau", start=week_before_start, end=week_after_end,
                    user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids))))

active$group <- ifelse(active$date >= week_before_start & active$date <= week_before_end,"week_before",
                       ifelse(active$date >= sale_start & active$date <= sale_end, sale_identifier,
                              ifelse(active$date >= week_after_start & active$date <= week_after_end, "week_after", NA)))


active <- active %>% 
  group_by(group,property) %>% 
  filter(group != "NA") %>% 
  summarise(active_prc = mean(value,na.rm=T)) 


#### 5. Average purchases per user purchasing at least once ####

ave_purchases_totals <- amplitude("CT", event="currency_spent", group_by_properties = list(list(type = "event", value = "purchase")),
                                  measured_by="totals", start=week_before_start, end=week_after_end,
                                  user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids))))

ave_purchases_uniques <- amplitude("CT", event="currency_spent",
                                   measured_by="uniques", start=week_before_start, end=week_after_end,
                                   user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids)))) %>% 
  select(value,date) %>% 
  rename(uniques = value)

ave_purchases <- left_join(ave_purchases_totals,ave_purchases_uniques, by="date")

ave_purchases$group <- ifelse(ave_purchases$date >= week_before_start & ave_purchases$date <= week_before_end,"week_before",
                              ifelse(ave_purchases$date >= sale_start & ave_purchases$date <= sale_end, sale_identifier,
                                     ifelse(ave_purchases$date >= week_after_start & ave_purchases$date <= week_after_end, "week_after", NA)))


ave_purchases <- ave_purchases %>% 
  group_by(group,property) %>% 
  filter(group != "NA") %>% 
  summarise(ave_purchases_per_user = mean(value)/mean(uniques))

rm(ave_purchases_totals,ave_purchases_uniques)

active <- left_join(active,ave_purchases, by = c("group","property"))


rm(ave_purchases)

#### 6. Gem Investment ####



############################################# -> Elixir Boosts Investment

elixir_data <- amplitude("CT", event="level_account", group_by_properties = list(list(type = "event", value = "energyboost_used")),
                    measured_by="totals", start=ext_date_start, end=ext_date_end,
                    user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids))))

elixir_data$property <- as.numeric(elixir_data$property)

elixir_data <- elixir_data %>% 
  mutate(prop_sum = value * property) %>% 
  group_by(date) %>% 
  summarise(elixir_used = sum(prop_sum,na.rm = T) * 24) 

############################################# -> Sentinel Gem Investment

sentinel_data <- amplitude("CT", event="level_account", group_by_properties = list(list(type = "event", value = "sentinel_purchased")),
                           measured_by="totals", start=ext_date_start, end=ext_date_end,
                           user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids))))

sentinel_data$property <- as.numeric(sentinel_data$property)

sentinel_data <- sentinel_data %>% 
  mutate(prop_sum = value * property) %>% 
  group_by(date) %>% 
  summarise(sentinel_purchased = sum(prop_sum,na.rm = T) * 8) 

############################################# -> Reinforcements used

reinforcements_data <- amplitude("CT", event="level_account", group_by_properties = list(list(type = "event", value = "reinforcements_used")),
                           measured_by="totals", start=ext_date_start, end=ext_date_end,
                           user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids))))

reinforcements_data$property <- as.numeric(reinforcements_data$property)

reinforcements_data <- reinforcements_data %>% 
  mutate(prop_sum = value * property) %>% 
  group_by(date) %>% 
  summarise(reinforcements_purchased = sum(prop_sum,na.rm = T) * 8) 

############################################# -> Play-On Investment

play_on_not <- amplitude("CT", event="level_account", group_by_properties = list(list(type = "event", value = "play_on_offered")),
                         where = list(list(subprop_type = "event", subprop_key = "play_on_offered",subprop_op = "greater", subprop_value=list("0")),
                                      list(subprop_type = "event", subprop_key = "outcome", subprop_op = "is", subprop_value=list("loss"))),
                         measured_by="totals", start=ext_date_start, end=ext_date_end,
                         user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids))))


play_on_not$property <- as.numeric(play_on_not$property)

play_on_not <- play_on_not %>% 
  group_by(date) %>% 
  summarise(play_on_totals = sum(value,na.rm=T)) 


play_on <- amplitude("CT", event="level_account", group_by_properties = list(list(type = "event", value = "play_on_offered")),
                     measured_by="totals", start=ext_date_start, end=ext_date_end,
                     user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids))))
play_on$property <- as.numeric(play_on$property)


play_on <- play_on %>% 
  mutate(prop_sum = value * property) %>% 
  group_by(date) %>% 
  summarise(play_on_used = sum(prop_sum,na.rm = T)) 



play_on <- left_join(play_on,play_on_not,by="date")
play_on$play_on <- (play_on$play_on_used - play_on$play_on_totals)*60
play_on <- play_on[,c(1,4)]


rm(play_on_not)

############################################# -> Players

player_data <- amplitude("CT", event="_active", measured_by="uniques", 
                                   user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids))),
                                   start=ext_date_start, end=ext_date_end) %>% 
  select(value,date) %>% 
  rename(players = value)


# Join all data together # 

gem_investment_data <- join_all(dfs = list(sentinel_data, reinforcements_data, elixir_data, play_on, player_data), by = "date")


gem_investment_data <- gem_investment_data %>% 
  filter(date <= Sys.Date()-1) %>% 
  group_by(date) %>% 
  summarise(ave_sentinel_purchased = sentinel_purchased / players,
            ave_reinforcements_purchased = reinforcements_purchased / players,
            ave_elixir_used = elixir_used / players,
            ave_play_on = play_on / players,
            ave_gem_investment = (sentinel_purchased + reinforcements_purchased + elixir_used + play_on)/players)

gem_investment_data$group <- ifelse(gem_investment_data$date >= week_before_start & gem_investment_data$date <= week_before_end,"week_before",
                        ifelse(gem_investment_data$date >= sale_start & gem_investment_data$date <= sale_end, sale_identifier,
                               ifelse(gem_investment_data$date >= week_after_start & gem_investment_data$date <= week_after_end, "week_after", NA)))

export <- gem_investment_data %>% 
  group_by(group) %>% 
  filter(!is.na(group)) %>% 
  summarise(ave_sentinel_purchased = mean(ave_sentinel_purchased),
            ave_reinforcements_purchased = mean(ave_reinforcements_purchased),
            ave_elixir_used = mean(ave_elixir_used),
            ave_play_on = mean(ave_play_on),
            ave_gem_investment = mean(ave_gem_investment))

metrics <- left_join(metrics,export, by = "group")

rm(elixir_data,play_on,reinforcements_data,sentinel_data,export)

temp <- melt(gem_investment_data[,c(1:5)],id.vars=1)

ggplot(temp, aes(date[date <= Sys.Date()-1],value, fill = variable)) + geom_area() + scale_fill_brewer(palette = "Dark2")+
  geom_vline(xintercept = as.numeric(gem_investment_data$date[gem_investment_data$date == sale_start]), linetype = "dashed", colour = "coral4") +
  geom_vline(xintercept = as.numeric(gem_investment_data$date[gem_investment_data$date == sale_end]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(sale_start),y=max(gem_investment_data$ave_gem_investment)-0.15*max(gem_investment_data$ave_gem_investment), label = paste0("Start ",sale_identifier), angle = 90, size = 4) +
  annotate("text",x = as.Date(sale_end),y=max(gem_investment_data$ave_gem_investment)-0.15*max(gem_investment_data$ave_gem_investment), label = paste0("End ",sale_identifier), angle = 90, size = 4) +
  
  geom_vline(xintercept = as.numeric(gem_investment_data$date[gem_investment_data$date == week_before_start]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(week_before_start),y=max(gem_investment_data$ave_gem_investment)-0.15*max(gem_investment_data$ave_gem_investment), label = paste0("Start ","week before"), angle = 90, size = 4) +
  geom_vline(xintercept = as.numeric(gem_investment_data$date[gem_investment_data$date == week_before_end]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(week_before_end),y=max(gem_investment_data$ave_gem_investment)-0.15*max(gem_investment_data$ave_gem_investment), label = paste0("End ","week before"), angle = 90, size = 4) +
  
  geom_vline(xintercept = as.numeric(gem_investment_data$date[gem_investment_data$date == week_after_start]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(week_after_start),y=max(gem_investment_data$ave_gem_investment)-0.15*max(gem_investment_data$ave_gem_investment), label = paste0("Start ","week after"), angle = 90, size = 4) +
  geom_vline(xintercept = as.numeric(gem_investment_data$date[gem_investment_data$date == week_after_end]), linetype = "dashed", colour = "coral4")+
  annotate("text",x = as.Date(week_after_end),y=max(gem_investment_data$ave_gem_investment)-0.15*max(gem_investment_data$ave_gem_investment), label = paste0("End ","week after"), angle = 90, size = 4) +
  
  scale_x_date(limits = c(min(temp$date), Sys.Date()-1))+
  ggtitle("Average Gem Investment per Player") 

rm(temp)
#### 7. Revenue by product_id ####

revenue <- amplitude("CT", event="unverified_revenue", measured_by="sums",
                             group_by_properties=list(list(type="event", value="$revenue"), list(type="event", value="$productId"), list(type="event", value="$revenueType")),
                             user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids))),
                             start=ext_date_start, end=ext_date_end)

revenue <- separate(revenue, col = property, into=c("product_id","code"), sep="; ")
revenue <- revenue %>% filter(code != "(none)")
revenue <- inner_join(revenue, conv, by=c("date","code"))
revenue$revenue <- revenue$value * revenue$exch

revenue$group <- ifelse(revenue$date >= week_before_start & revenue$date <= week_before_end, "week_before",
                              ifelse(revenue$date >= sale_start & revenue$date <= sale_end, sale_identifier,
                                     ifelse(revenue$date >= week_after_start & revenue$date <= week_after_end, "week_after", NA)))


revenue_1 <- revenue %>% 
  filter(!is.na(group) & revenue > 20) %>% 
  group_by(group, product_id) %>%
  summarise(total_revenue = sum(revenue)) 
  


ggplot(revenue_1, aes(product_id,total_revenue, fill=group)) +
  geom_bar(stat="identity", position="dodge", colour = "darkblue") +
  scale_fill_manual( values = c("dodgerblue2","dodgerblue4","dodgerblue"))+
  ggtitle("Revenue (gross USD) per product_id")

rm(revenue_1)

#### 8. ARPDAU ####

revenue_2 <- revenue %>% 
  group_by(date) %>% 
  summarise(total_revenue = sum(revenue)) 

revenue_2 <- left_join(revenue_2,player_data, by = "date") %>% select(date,total_revenue,players) %>% group_by(date) %>% summarise(ARPDAU = total_revenue / players)

ggplot(revenue_2, aes(date,ARPDAU)) + geom_line(size = 1, colour = "darkorchid3")+
  geom_vline(xintercept = as.numeric(revenue_2$date[revenue_2$date == sale_start]), linetype = "dashed", colour = "coral4") +
  geom_vline(xintercept = as.numeric(revenue_2$date[revenue_2$date == sale_end]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(sale_start),y=max(revenue_2$ARPDAU)-0.15*max(revenue_2$ARPDAU), label = paste0("Start ",sale_identifier), angle = 90, size = 4) +
  annotate("text",x = as.Date(sale_end),y=max(revenue_2$ARPDAU)-0.15*max(revenue_2$ARPDAU), label = paste0("End ",sale_identifier), angle = 90, size = 4) +
  
  geom_vline(xintercept = as.numeric(revenue_2$date[revenue_2$date == week_before_start]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(week_before_start),y=max(revenue_2$ARPDAU)-0.15*max(revenue_2$ARPDAU), label = paste0("Start ","week before"), angle = 90, size = 4) +
  geom_vline(xintercept = as.numeric(revenue_2$date[revenue_2$date == week_before_end]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(week_before_end),y=max(revenue_2$ARPDAU)-0.15*max(revenue_2$ARPDAU), label = paste0("Start ","week before"), angle = 90, size = 4) +
  
  geom_vline(xintercept = as.numeric(revenue_2$date[revenue_2$date == week_after_start]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(week_after_start),y=max(revenue_2$ARPDAU)-0.15*max(revenue_2$ARPDAU), label = paste0("Start ","week after"), angle = 90, size = 4) +
  geom_vline(xintercept = as.numeric(revenue_2$date[revenue_2$date == week_after_end]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(week_after_end),y=max(revenue_2$ARPDAU)-0.15*max(revenue_2$ARPDAU), label = paste0("Start ","week after"), angle = 90, size = 4) +

  scale_x_date(limits = c(min(revenue_2$date), Sys.Date()-1)) +
  ggtitle("ARPDAU (USD gross)") 

rm(revenue_2, player_data,conv)

#### 9. Conversion Rate #### 

conversion_rate <-  amplitude("CT", event="unverified_revenue",
                              measured_by="pct_dau", start=ext_date_start, end=ext_date_end,
                              user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids)))) %>% select(value,date) %>% filter(date <= Sys.Date() - 1)


ggplot(conversion_rate, aes(date,value)) + geom_line(size = 1, colour = "darkorchid3")+
  geom_vline(xintercept = as.numeric(conversion_rate$date[conversion_rate$date == sale_start]), linetype = "dashed", colour = "coral4") +
  geom_vline(xintercept = as.numeric(conversion_rate$date[conversion_rate$date == sale_end]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(sale_start),y=max(conversion_rate$value)-0.15*max(conversion_rate$value), label = paste0("Start ",sale_identifier), angle = 90, size = 4) +
  annotate("text",x = as.Date(sale_end),y=max(conversion_rate$value)-0.15*max(conversion_rate$value), label = paste0("End ",sale_identifier), angle = 90, size = 4) +
  
  geom_vline(xintercept = as.numeric(conversion_rate$date[conversion_rate$date == week_before_start]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(week_before_start),y=max(conversion_rate$value)-0.15*max(conversion_rate$value), label = paste0("Start ","week before"), angle = 90, size = 4) +
  geom_vline(xintercept = as.numeric(conversion_rate$date[conversion_rate$date == week_before_end]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(week_before_end),y=max(conversion_rate$value)-0.15*max(conversion_rate$value), label = paste0("Start ","week before"), angle = 90, size = 4) +
  
  geom_vline(xintercept = as.numeric(conversion_rate$date[conversion_rate$date == week_after_start]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(week_after_start),y=max(conversion_rate$value)-0.15*max(conversion_rate$value), label = paste0("Start ","week after"), angle = 90, size = 4) +
  geom_vline(xintercept = as.numeric(conversion_rate$date[conversion_rate$date == week_after_end]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(week_after_end),y=max(conversion_rate$value)-0.15*max(conversion_rate$value), label = paste0("Start ","week after"), angle = 90, size = 4) +
  
  
  scale_x_date(limits = c(min(conversion_rate$date), Sys.Date()-1))+
  ggtitle("Conversion Rate") 

rm(conversion_rate, revenue)

#### 10. Currency conversion percentage ####

prc <-  amplitude("CT", event="currency_spent", group_by_properties = list(list(type = "event", value = "purchase")),
                      measured_by="pct_dau", start=ext_date_start, end=ext_date_end,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids)))) %>% filter(date <= Sys.Date() - 1) %>% filter(property != "ticket")
                                                                                                                                                             
ggplot(prc, aes(date,value, fill = property)) + geom_area()+scale_fill_brewer(palette = "Dark2",direction = -1) +
  geom_vline(xintercept = as.numeric(prc$date[prc$date == sale_start]), linetype = "dashed", colour = "coral4") +
  geom_vline(xintercept = as.numeric(prc$date[prc$date == sale_end]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(sale_start),y=max(prc$value)-0.5*max(prc$value), label = paste0("Start ",sale_identifier), angle = 90, size = 4) +
  annotate("text",x = as.Date(sale_end),y=max(prc$value)-0.5*max(prc$value), label = paste0("End ",sale_identifier), angle = 90, size = 4) +
  
  geom_vline(xintercept = as.numeric(prc$date[prc$date == week_before_start]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(week_before_start),y=max(prc$value)-0.5*max(prc$value), label = paste0("Start ","week before"), angle = 90, size =4) +
  geom_vline(xintercept = as.numeric(prc$date[prc$date == week_before_end]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(week_before_end),y=max(prc$value)-0.5*max(prc$value), label = paste0("Start ","week before"), angle = 90, size =4) +
  
  geom_vline(xintercept = as.numeric(prc$date[prc$date == week_after_start]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(week_after_start),y=max(prc$value)-0.5*max(prc$value), label = paste0("Start ","week after"), angle = 90, size =4) +
  geom_vline(xintercept = as.numeric(prc$date[prc$date == week_after_end]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(week_after_end),y=max(prc$value)-0.5*max(prc$value), label = paste0("Start ","week after"), angle = 90, size =4) +
  
  scale_x_date(limits = c(min(prc$date), Sys.Date()-1))+
  ggtitle("Currency conversion percentage") 


#### 11. Export ####


write.csv(metrics, paste0("CT_", sale_identifier,"_report","_1","_", Sys.Date(), ".csv"))
write.csv(active, paste0("CT_", sale_identifier,"_report","_2","_", Sys.Date(), ".csv"))


