### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

### Sale Analysis

### ### ### ### ### ### ### ### ### ### ### ### ### ### ###




#### 1. Inputs ####
# cohort_ids is used to eliminate hackers. 
# Please update the cohort before analysing the results so the cohort is up to date with the latest hackers.

sale_identifier = "Event 10 Sale Postmortem"
cohort_ids <- c("artzosc")


# Insert the date of the start of the event not the start of sale

event_0 <- "event_10"
event_1 <- "event_09"
event_2 <- "event_08"
 
event_0_date <- "2017-05-19"
event_1_date <- "2017-04-14"
event_2_date <- "2017-03-17"

#### 2. Parameters ####


event_0_sale <- as.Date(event_0_date)+1
event_0_bundle_start <- as.Date(event_0_date)+7
event_0_bundle_end <- as.Date(event_0_date)+9

event_1_sale <- as.Date(event_1_date)+1
event_1_bundle_start <- as.Date(event_1_date)+7
event_1_bundle_end <- as.Date(event_1_date)+9

event_2_sale <- as.Date(event_2_date)+1
event_2_bundle_start <- as.Date(event_2_date)+7
event_2_bundle_end <- as.Date(event_2_date)+9


ext_date_start <- Sys.Date()-75
ext_date_end <- Sys.Date()-1


source('~/amplitude-dashboard/dashboard-code/SharedConfig.R')

#### 3. Data Retrieval ####

############################################# -> Revenue 

revenue <- amplitude("AC", event="unverified_revenue", measured_by="sums",
                  group_by_properties=list(list(type="event", value="$revenue"), list(type="event", value="$revenueType")),
                  user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids))),
                  start=ext_date_start, end=ext_date_end) %>% filter(property != "(none)") %>% 
                  rename(code = property)

conv <- get_exchange_rates(unique(revenue$code), toCurr="USD", fromDt=ext_date_start, toDt=ext_date_end)
revenue <- inner_join(revenue, conv, by=c("code", "date"))
revenue$revenue <- revenue$value * revenue$exch 

revenue <- revenue %>% 
  group_by(date) %>% 
  summarise(revenue = sum(revenue))

############################################# -> Players

player_temp <- amplitude("AC", event="_active", measured_by="uniques", 
                         user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids))),
                         start=ext_date_start, end=ext_date_end) %>% 
  select(value,date) %>% 
  rename(players = value)

############################################# -> Total Spenders

spender <- amplitude("AC", event="unverified_revenue", measured_by="uniques",
                     user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids))),
                     start=ext_date_start, end=ext_date_end) %>% 
  select(value,date) %>% 
  rename(spenders = value)

############################################# -> Join all the datasets

metrics <- inner_join(inner_join(revenue, player_temp,by="date"), spender, by="date")


metrics$sale <- ifelse(metrics$date == event_2_sale, "first_day_sale",
                       ifelse(metrics$date >= event_2_bundle_start & metrics$date <= event_2_bundle_end, "bundle_sale",
                              ifelse(metrics$date == event_1_sale, "first_day_sale",
                                     ifelse(metrics$date >= event_1_bundle_start & metrics$date <= event_1_bundle_end,"bundle_sale",
                                            ifelse(metrics$date == event_0_sale, "first_day_sale",
                                                   ifelse(metrics$date >= event_0_bundle_start & metrics$date <= event_0_bundle_end, "bundle_sale",NA))))))
metrics$event <- ifelse(metrics$date >= event_2_sale & metrics$date <= (event_2_bundle_end ),event_2,
                        ifelse(metrics$date >= event_1_sale & metrics$date <= event_1_bundle_end , event_1,
                               ifelse(metrics$date >= event_0_sale & metrics$date <= event_0_bundle_end , event_0, NA)))



metrics <- metrics %>% 
  filter(event != "NA" & sale != "NA") %>% 
  group_by(event,sale) %>% 
  summarise(total_revenue = 0.7 * sum(revenue, na.rm = T),
            total_players = sum(players,na.rm = T),
            total_spenders = sum(spenders,na.rm = T),
            percent_spenders = sum(spenders,na.rm = T) / sum(players,na.rm = T))


############################################# -> Average DAU

metrics <- metrics %>% 
  group_by(event,sale) %>% 
  mutate(ave_DAU = ifelse(sale == "first_day_sale", total_players / 1, total_players / 3),
         ARPU = total_revenue/total_players)

############################################# -> Average ARPDAU

metrics <- metrics %>% 
  group_by(event,sale) %>% 
  mutate(ave_ARPDAU = total_revenue / ave_DAU)


rm(spender, revenue)


#### 4. Active % ####

active <- amplitude("AC", event="CurrencySpent", group_by_properties = list(list(type = "event", value = "purchase")),
                    measured_by="pct_dau", start=ext_date_start, end=ext_date_end,
                    user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids))))

active$sale <- ifelse(active$date == event_2_sale, "first_day_sale",
                       ifelse(active$date >= event_2_bundle_start & active$date <= event_2_bundle_end, "bundle_sale",
                              ifelse(active$date == event_1_sale, "first_day_sale",
                                     ifelse(active$date >= event_1_bundle_start & active$date <= event_1_bundle_end,"bundle_sale",
                                            ifelse(active$date == event_0_sale, "first_day_sale",
                                                   ifelse(active$date >= event_0_bundle_start & active$date <= event_0_bundle_end, "bundle_sale",NA))))))
active$event <-  ifelse(active$date >= event_2_sale & active$date <= event_2_bundle_end,event_2,
                        ifelse(active$date >= event_1_sale & active$date <= event_1_bundle_end , event_1,
                               ifelse(active$date >= event_0_sale & active$date <= event_0_bundle_end , event_0, NA)))

active <- active %>% 
    group_by(sale,event,property) %>% 
    filter(event != "NA" & sale != "NA") %>% 
    filter(property == "SuperTowerRefill" | property == "credit" | property == "callin_cryobomb" | property == "callin_energyboost" 
           | property == "callin_reinforcement" | property =="callin_bomb" | property == "KillBoost") %>% 
    summarise(active_prc = mean(value,na.rm=T)) 





#### 5. Average purchases per user purchasing at least once ####



ave_purchases_totals <- amplitude("AC", event="CurrencySpent", group_by_properties = list(list(type = "event", value = "purchase")),
                                    measured_by="totals", start=ext_date_start, end=ext_date_end,
                                    user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids))))
  
ave_purchases_uniques <- amplitude("AC", event="CurrencySpent",
                                     measured_by="uniques", start=ext_date_start, end=ext_date_end,
                                     user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids)))) %>% 
  select(value,date) %>% 
  rename(uniques = value)
  


ave_purchases <- left_join(ave_purchases_totals,ave_purchases_uniques, by="date")

ave_purchases$sale <- ifelse(ave_purchases$date == event_2_sale, "first_day_sale",
                      ifelse(ave_purchases$date >= event_2_bundle_start & ave_purchases$date <= event_2_bundle_end, "bundle_sale",
                             ifelse(ave_purchases$date == event_1_sale, "first_day_sale",
                                    ifelse(ave_purchases$date >= event_1_bundle_start & ave_purchases$date <= event_1_bundle_end,"bundle_sale",
                                           ifelse(ave_purchases$date == event_0_sale, "first_day_sale",
                                                  ifelse(ave_purchases$date >= event_0_bundle_start & ave_purchases$date <= event_0_bundle_end, "bundle_sale",NA))))))

ave_purchases$event <- ifelse(ave_purchases$date >= event_2_sale & ave_purchases$date <= event_2_bundle_end ,event_2,
                              ifelse(ave_purchases$date >= event_1_sale & ave_purchases$date <= event_1_bundle_end, event_1,
                                     ifelse(ave_purchases$date >= event_0_sale & ave_purchases$date <= event_0_bundle_end, event_0, NA)))


ave_purchases <- ave_purchases %>% 
  group_by(sale,event,property) %>% 
  filter(property == "SuperTowerRefill" | property == "credit" | property == "callin_cryobomb" | property == "callin_energyboost" 
         | property == "callin_reinforcement" | property =="callin_bomb" | property == "KillBoost") %>% 
  filter(event != "NA" & sale != "NA") %>% 
  summarise(ave_purchases_per_user = mean(value)/mean(uniques))

rm(ave_purchases_totals,ave_purchases_uniques)

active <- left_join(active,ave_purchases, by = c("sale","event","property"))


rm(ave_purchases)

#### 6. Gem Investment ####

# Gem investment is split between event levels and level finished so both have to be extracted

############################################# -> Tesla Boosts Investment

tesla <- amplitude("AC", event="event_level_finished", group_by_properties = list(list(type = "event", value = "superTowerRefillsBought")),
                         measured_by="totals", start=ext_date_start , end=ext_date_end,
                         user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids)))) %>% 
  mutate(prop_sum_tower = value * as.numeric(property)) %>% 
  group_by(date) %>% 
  summarise(tesla = sum(prop_sum_tower, na.rm = T) * 6)



tesla_c <- amplitude("AC", event="Level%20Finished", group_by_properties = list(list(type = "event", value = "superTowerRefillsBought")),
                   measured_by="totals", start=ext_date_start , end=ext_date_end,
                   user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids)))) %>% 
  mutate(prop_sum_tower = value * as.numeric(property)) %>% 
  group_by(date) %>% 
  summarise(tesla = sum(prop_sum_tower, na.rm = T) * 6)

tesla <- inner_join(tesla,tesla_c,by="date") %>% mutate(tesla = tesla.x + tesla.y) %>% select(date, tesla)
rm(tesla_c)

############################################# -> Cryobombs Gem Investment

cryo <- amplitude("AC", event="event_level_finished", group_by_properties = list(list(type = "event", value = "cryobombs_used")),
                   measured_by="totals", start=ext_date_start, end=ext_date_end,
                   user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids)))) %>% 
  mutate(prop_sum_cryo = value * as.numeric(property)) %>% 
  group_by(date) %>% 
  summarise(cryo_used = sum(prop_sum_cryo, na.rm = T) * 4)

cryo_c <- amplitude("AC", event="Level%20Finished", group_by_properties = list(list(type = "event", value = "cryobombs_used")),
                  measured_by="totals", start=ext_date_start, end=ext_date_end,
                  user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids)))) %>% 
  mutate(prop_sum_cryo = value * as.numeric(property)) %>% 
  group_by(date) %>% 
  summarise(cryo_used = sum(prop_sum_cryo, na.rm = T) * 4)

cryo <- inner_join(cryo,cryo_c,by="date") %>% mutate(cryo_used = cryo_used.x + cryo_used.y) %>% select(date, cryo_used)
rm(cryo_c)

############################################# -> Reinforcements used

reinforcements <- amplitude("AC", event="event_level_finished", group_by_properties = list(list(type = "event", value = "reinforcesUsed")),
                  measured_by="totals", start=ext_date_start, end=ext_date_end,
                  user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids)))) %>% 
  mutate(prop_sum_reinforcements = value * as.numeric(property)) %>% 
  group_by(date) %>% 
  summarise(reinforcements_used = sum(prop_sum_reinforcements, na.rm = T) * 6)

reinforcements_c <- amplitude("AC", event="Level%20Finished", group_by_properties = list(list(type = "event", value = "reinforcesUsed")),
                            measured_by="totals", start=ext_date_start, end=ext_date_end,
                            user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids)))) %>% 
  mutate(prop_sum_reinforcements = value * as.numeric(property)) %>% 
  group_by(date) %>% 
  summarise(reinforcements_used = sum(prop_sum_reinforcements, na.rm = T) * 6)

reinforcements <- inner_join(reinforcements,reinforcements_c,by="date") %>% mutate(reinforcements_used = reinforcements_used.x + reinforcements_used.y) %>% select(date, reinforcements_used)
rm(reinforcements_c)

############################################# -> Airstrikes used

airstrikes <- amplitude("AC", event="event_level_finished", group_by_properties = list(list(type = "event", value = "airstrikesUsed")),
                            measured_by="totals", start=ext_date_start, end=ext_date_end,
                            user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids)))) %>% 
  mutate(prop_sum_airstrikes = value * as.numeric(property)) %>% 
  group_by(date) %>% 
  summarise(airstrikes_used = sum(prop_sum_airstrikes, na.rm = T) * 2)

airstrikes_c <- amplitude("AC", event="Level%20Finished", group_by_properties = list(list(type = "event", value = "airstrikesUsed")),
                        measured_by="totals", start=ext_date_start, end=ext_date_end,
                        user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids)))) %>% 
  mutate(prop_sum_airstrikes = value * as.numeric(property)) %>% 
  group_by(date) %>% 
  summarise(airstrikes_used = sum(prop_sum_airstrikes, na.rm = T) * 2)

airstrikes <- inner_join(airstrikes,airstrikes_c,by="date") %>% mutate(airstrikes_used = airstrikes_used.x + airstrikes_used.y) %>% select(date, airstrikes_used)
rm(airstrikes_c)


############################################# -> Second Chances Bought

second_chance <- amplitude("AC", event="event_level_finished", group_by_properties = list(list(type = "event", value = "secondChancesBought")),
                       measured_by="totals", start=ext_date_start, end=ext_date_end,
                       user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids)))) %>% 
  mutate(prop_sum_second_chance = value * as.numeric(property)) %>% 
  group_by(date) %>% 
  summarise(second_chance = sum(prop_sum_second_chance, na.rm = T) * 10)


second_chance_c <- amplitude("AC", event="Level%20Finished", group_by_properties = list(list(type = "event", value = "secondChancesBought")),
                           measured_by="totals", start=ext_date_start, end=ext_date_end,
                           user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids)))) %>% 
  mutate(prop_sum_second_chance = value * as.numeric(property)) %>% 
  group_by(date) %>% 
  summarise(second_chance = sum(prop_sum_second_chance, na.rm = T) * 10)

second_chance <- inner_join(second_chance,second_chance_c,by="date") %>% mutate(second_chance = second_chance.x + second_chance.y) %>% select(date, second_chance)
rm(second_chance_c)

############################################# -> Energy Boosts 

energy <- amplitude("AC", event="event_level_finished", group_by_properties = list(list(type = "event", value = "energyboosts_used")),
                           measured_by="totals", start=ext_date_start, end=ext_date_end,
                           user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids)))) %>% 
  mutate(prop_sum_energy = value * as.numeric(property)) %>% 
  group_by(date) %>% 
  summarise(energy_used = sum(prop_sum_energy, na.rm = T) * 10)

energy_c <- amplitude("AC", event="Level%20Finished", group_by_properties = list(list(type = "event", value = "energyboosts_used")),
                    measured_by="totals", start=ext_date_start, end=ext_date_end,
                    user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids)))) %>% 
  mutate(prop_sum_energy = value * as.numeric(property)) %>% 
  group_by(date) %>% 
  summarise(energy_used = sum(prop_sum_energy, na.rm = T) * 10)

energy <- inner_join(energy,energy_c,by="date") %>% mutate(energy_used = energy_used.x + energy_used.y) %>% select(date, energy_used)
rm(energy_c)

########################################## -> Average gem investment


gem_investment <- join_all(dfs = list(tesla,second_chance,airstrikes,cryo,reinforcements,player_temp,energy),by = "date")

gem_investment <- gem_investment %>% 
  filter(date <= Sys.Date()) %>% 
  group_by(date) %>% 
  summarise(ave_tesla_bought = tesla/players,
            ave_second_chances_bought = second_chance/players,
            ave_cryoboms_used = cryo_used/players,
            ave_airstrikes_used = airstrikes_used/players,
            ave_reinforcements_used = reinforcements_used/players,
            ave_energy_used = energy_used/players,
            ave_gem_investment = (tesla+second_chance+cryo_used+airstrikes_used+reinforcements_used)/players)

gem_investment$sale <- ifelse(gem_investment$date == event_2_sale, "first_day_sale",
                             ifelse(gem_investment$date >= event_2_bundle_start & gem_investment$date <= event_2_bundle_end, "bundle_sale",
                                    ifelse(gem_investment$date == event_1_sale, "first_day_sale",
                                           ifelse(gem_investment$date >= event_1_bundle_start & gem_investment$date <= event_1_bundle_end,"bundle_sale",
                                                  ifelse(gem_investment$date == event_0_sale, "first_day_sale",
                                                         ifelse(gem_investment$date >= event_0_bundle_start & gem_investment$date <= event_0_bundle_end, "bundle_sale",NA))))))

gem_investment$event <- ifelse(gem_investment$date >= event_2_sale & gem_investment$date <= event_2_bundle_end, event_2,
                              ifelse(gem_investment$date >= event_1_sale & gem_investment$date <= event_1_bundle_end, event_1,
                                     ifelse(gem_investment$date >= event_0_sale & gem_investment$date <= event_0_bundle_end, event_0, NA)))


rm(airstrikes,cryo,reinforcements,second_chance,tesla, energy)

gem_investment_export <- gem_investment %>% 
  filter(sale != "sale" & event != "event") %>% 
  group_by(sale,event)

#### 7. Graphs #####


ggplot(gem_investment, aes(date, ave_gem_investment)) + geom_line(colour = "darkorchid3", size = 1) + 
  geom_vline(xintercept = as.numeric(event_2_sale), linetype = "dashed", colour = "coral4") +
  annotate("text",x = event_2_sale+0.35,y=max(gem_investment$ave_gem_investment)-0.05*max(gem_investment$ave_gem_investment), label = "first_day_sale", angle = 90, size =4.0)+
  geom_vline(xintercept = as.numeric(event_2_bundle_start), linetype = "dashed", colour = "coral4")+
  annotate("text",x = event_2_bundle_start + 0.35 ,y=max(gem_investment$ave_gem_investment)-0.05*max(gem_investment$ave_gem_investment), label = "start_bundle_sale", angle = 90, size = 4)+
  geom_vline(xintercept = as.numeric(event_2_bundle_end), linetype = "dashed", colour = "coral4")+
  annotate("text",x = event_2_bundle_end + 0.35,y=max(gem_investment$ave_gem_investment)-0.05*max(gem_investment$ave_gem_investment), label = "end_bundle_sale", angle = 90, size =4)+
  geom_vline(xintercept = as.numeric(event_1_sale), linetype = "dashed", colour = "coral4") +
  annotate("text",x = event_1_sale+0.35,y=max(gem_investment$ave_gem_investment)-0.05*max(gem_investment$ave_gem_investment), label = "first_day_sale", angle = 90, size =4.0)+
  geom_vline(xintercept = as.numeric(event_1_bundle_start), linetype = "dashed", colour = "coral4")+
  annotate("text",x = event_1_bundle_start + 0.35 ,y=max(gem_investment$ave_gem_investment)-0.05*max(gem_investment$ave_gem_investment), label = "start_bundle_sale", angle = 90, size = 4)+
  geom_vline(xintercept = as.numeric(event_1_bundle_end), linetype = "dashed", colour = "coral4")+
  annotate("text",x = event_1_bundle_end + 0.35,y=max(gem_investment$ave_gem_investment)-0.05*max(gem_investment$ave_gem_investment), label = "end_bundle_sale", angle = 90, size =4)+
  
  geom_vline(xintercept = as.numeric(event_0_sale), linetype = "dashed", colour = "coral4") +
  annotate("text",x = event_0_sale+0.35,y=max(gem_investment$ave_gem_investment)-0.05*max(gem_investment$ave_gem_investment), label = "first_day_sale", angle = 90, size =4.0)+
  geom_vline(xintercept = as.numeric(event_0_bundle_start), linetype = "dashed", colour = "coral4") +
  annotate("text",x = event_0_bundle_start+0.35,y=max(gem_investment$ave_gem_investment)-0.05*max(gem_investment$ave_gem_investment), label = "start_bundle_sale", angle = 90, size =4.0)+
  geom_vline(xintercept = as.numeric(event_0_bundle_end), linetype = "dashed", colour = "coral4") +
  annotate("text",x = event_0_bundle_end+0.35,y=max(gem_investment$ave_gem_investment)-0.05*max(gem_investment$ave_gem_investment), label = "end_bundle_sale", angle = 90, size =4.0)+
  
  annotate("text",x = event_2_sale + 4, y=min(ARPDAU$ARPDAU),label = event_0, colour = "dodgerblue4")+
  annotate("text",x = event_1_sale + 4, y=min(ARPDAU$ARPDAU),label = event_1, colour = "dodgerblue4")+
  annotate("text",x = event_0_sale + 4, y=min(ARPDAU$ARPDAU),label = event_2, colour = "dodgerblue4")+
  ggtitle("Average Gem Investment per Player") +
  ylab("Average Gems Used")

temp <- melt(gem_investment[,c(1:8)],id.vars=1)

ggplot(temp, aes(date,value, fill = variable)) + geom_area() + scale_fill_brewer(palette = "Dark2")+
  geom_vline(xintercept = as.numeric(event_2_sale), linetype = "dashed", colour = "coral4") +
  annotate("text",x = event_2_sale+0.35,y=max(gem_investment$ave_gem_investment)-0.05*max(gem_investment$ave_gem_investment), label = "first_day_sale", angle = 90, size =4.0)+
  geom_vline(xintercept = as.numeric(event_2_bundle_start), linetype = "dashed", colour = "coral4")+
  annotate("text",x = event_2_bundle_start + 0.35 ,y=max(gem_investment$ave_gem_investment)-0.05*max(gem_investment$ave_gem_investment), label = "start_bundle_sale", angle = 90, size = 4)+
  geom_vline(xintercept = as.numeric(event_2_bundle_end), linetype = "dashed", colour = "coral4")+
  annotate("text",x = event_2_bundle_end + 0.35,y=max(gem_investment$ave_gem_investment)-0.05*max(gem_investment$ave_gem_investment), label = "end_bundle_sale", angle = 90, size =4)+
  geom_vline(xintercept = as.numeric(event_1_sale), linetype = "dashed", colour = "coral4") +
  annotate("text",x = event_1_sale+0.35,y=max(gem_investment$ave_gem_investment)-0.05*max(gem_investment$ave_gem_investment), label = "first_day_sale", angle = 90, size =4.0)+
  geom_vline(xintercept = as.numeric(event_1_bundle_start), linetype = "dashed", colour = "coral4")+
  annotate("text",x = event_1_bundle_start + 0.35 ,y=max(gem_investment$ave_gem_investment)-0.05*max(gem_investment$ave_gem_investment), label = "start_bundle_sale", angle = 90, size = 4)+
  geom_vline(xintercept = as.numeric(event_1_bundle_end), linetype = "dashed", colour = "coral4")+
  annotate("text",x = event_1_bundle_end + 0.35,y=max(gem_investment$ave_gem_investment)-0.05*max(gem_investment$ave_gem_investment), label = "end_bundle_sale", angle = 90, size =4)+
  
  geom_vline(xintercept = as.numeric(event_0_sale), linetype = "dashed", colour = "coral4") +
  annotate("text",x = event_0_sale+0.35,y=max(gem_investment$ave_gem_investment)-0.05*max(gem_investment$ave_gem_investment), label = "first_day_sale", angle = 90, size =4.0)+
  geom_vline(xintercept = as.numeric(event_0_bundle_start), linetype = "dashed", colour = "coral4") +
  annotate("text",x = event_0_bundle_start+0.35,y=max(gem_investment$ave_gem_investment)-0.05*max(gem_investment$ave_gem_investment), label = "start_bundle_sale", angle = 90, size =4.0)+
  geom_vline(xintercept = as.numeric(event_0_bundle_end), linetype = "dashed", colour = "coral4") +
  annotate("text",x = event_0_bundle_end+0.35,y=max(gem_investment$ave_gem_investment)-0.05*max(gem_investment$ave_gem_investment), label = "end_bundle_sale", angle = 90, size =4.0)+
  
  annotate("text",x = event_2_sale + 4, y=min(ARPDAU$ARPDAU),label = event_0, colour = "dodgerblue4")+
  annotate("text",x = event_1_sale + 4, y=min(ARPDAU$ARPDAU),label = event_1, colour = "dodgerblue4")+
  annotate("text",x = event_0_sale + 4, y=min(ARPDAU$ARPDAU),label = event_2, colour = "dodgerblue4")+
  
  ggtitle("Average Gem Investment per Player") +
  ylab("Average Gems Used")
  
rm(temp)

#### 7. Revenue by product_id ####

revenue_temp <- amplitude("AC", event="unverified_revenue", measured_by="sums",
                     group_by_properties=list(list(type="event", value="$revenue"), list(type="event", value="$productId"), list(type="event", value="$revenueType")),
                     user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids))),
                     start=ext_date_start, end=ext_date_end)

revenue_temp <- separate(revenue_temp, col = property, into=c("product_id","code"), sep="; ")
revenue_temp <- revenue_temp %>% filter(code != "(none)")
revenue_temp <- inner_join(revenue_temp, conv, by=c("date","code"))
revenue_temp$revenue <- revenue_temp$value * revenue_temp$exch * 0.7

revenue_temp$sale <- ifelse(revenue_temp$date == event_2_sale, "first_day_sale",
                              ifelse(revenue_temp$date >= event_2_bundle_start & revenue_temp$date <= event_2_bundle_end, "bundle_sale",
                                     ifelse(revenue_temp$date == event_1_sale, "first_day_sale",
                                            ifelse(revenue_temp$date >= event_1_bundle_start & revenue_temp$date <= event_1_bundle_end,"bundle_sale",
                                                   ifelse(revenue_temp$date == event_0_sale, "first_day_sale",
                                                          ifelse(revenue_temp$date >= event_0_bundle_start & revenue_temp$date <= event_0_bundle_end, "bundle_sale",NA))))))

revenue_temp$event <- ifelse(revenue_temp$date >= event_2_sale & revenue_temp$date <= event_2_bundle_end, event_2,
                               ifelse(revenue_temp$date >= event_1_sale & revenue_temp$date <= event_1_bundle_end, event_1,
                                      ifelse(revenue_temp$date >= event_0_sale & revenue_temp$date <= event_0_bundle_end, event_0, NA)))


revenue_1 <- revenue_temp %>% 
  filter(!is.na(sale) & !is.na(event) & revenue > 20) %>% 
  group_by(sale,event, product_id) %>%
  summarise(total_revenue = sum(revenue)) 

revenue_2 <- revenue_1 %>% filter(sale == "first_day_sale")

ggplot(revenue_2, aes(product_id,total_revenue, fill = event)) +
  scale_fill_brewer(palette = "Dark2")+
  geom_bar(stat="identity", position="dodge", colour = "darkblue") +
  ggtitle("Revenue (gross USD) per product_id - First Day Sale")


revenue_3 <- revenue_1 %>% filter(sale == "bundle_sale")

ggplot(revenue_3, aes(product_id,total_revenue, fill = event)) +
  scale_fill_brewer(palette = "Dark2") +
  geom_bar(stat="identity", position="dodge", colour = "darkblue") +
  scale_fill_manual( values = c("dodgerblue2","dodgerblue4","darkorchid")) +
  ggtitle("Revenue (gross USD) per product_id - Bundle Sale")

rm(revenue_2,revenue_3)


#### 8. ARPDAU ####


ARPDAU <- revenue_temp %>% 
  group_by(date) %>% 
  summarise(total_revenue = sum(revenue,na.rm = T)) 

ARPDAU <- inner_join(ARPDAU, player_temp, by="date") %>% group_by(date) %>% mutate(ARPDAU = total_revenue / players)  

ggplot(ARPDAU, aes(date,ARPDAU)) + geom_line(size = 1, colour = "darkorchid3") +
  geom_vline(xintercept = as.numeric(event_2_sale), linetype = "dashed", colour = "coral4") +
  annotate("text",x = event_2_sale+0.35,y=max(ARPDAU$ARPDAU)-0.1*max(ARPDAU$ARPDAU), label = "first_day_sale", angle = 90, size =4.0)+
  geom_vline(xintercept = as.numeric(event_2_bundle_start), linetype = "dashed", colour = "coral4")+
  annotate("text",x = event_2_bundle_start + 0.35 ,y=max(ARPDAU$ARPDAU)-0.1*max(ARPDAU$ARPDAU), label = "start_bundle_sale", angle = 90, size = 4)+
  geom_vline(xintercept = as.numeric(event_2_bundle_end), linetype = "dashed", colour = "coral4")+
  annotate("text",x = event_2_bundle_end + 0.35,y=max(ARPDAU$ARPDAU)-0.1*max(ARPDAU$ARPDAU), label = "end_bundle_sale", angle = 90, size =4)+
  geom_vline(xintercept = as.numeric(event_1_sale), linetype = "dashed", colour = "coral4") +
  annotate("text",x = event_1_sale+0.35,y=max(ARPDAU$ARPDAU)-0.1*max(ARPDAU$ARPDAU), label = "first_day_sale", angle = 90, size =4.0)+
  geom_vline(xintercept = as.numeric(event_1_bundle_start), linetype = "dashed", colour = "coral4")+
  annotate("text",x = event_1_bundle_start + 0.35 ,y=max(ARPDAU$ARPDAU)-0.1*max(ARPDAU$ARPDAU), label = "start_bundle_sale", angle = 90, size = 4)+
  geom_vline(xintercept = as.numeric(event_1_bundle_end), linetype = "dashed", colour = "coral4")+
  annotate("text",x = event_1_bundle_end + 0.35,y=max(ARPDAU$ARPDAU)-0.1*max(ARPDAU$ARPDAU), label = "end_bundle_sale", angle = 90, size =4)+
  
  geom_vline(xintercept = as.numeric(event_0_sale), linetype = "dashed", colour = "coral4") +
  annotate("text",x = event_0_sale+0.35,y=max(ARPDAU$ARPDAU)-0.1*max(ARPDAU$ARPDAU), label = "first_day_sale", angle = 90, size =4.0)+
  geom_vline(xintercept = as.numeric(event_0_bundle_start), linetype = "dashed", colour = "coral4") +
  annotate("text",x = event_0_bundle_start+0.35,y=max(ARPDAU$ARPDAU)-0.1*max(ARPDAU$ARPDAU), label = "start_bundle_sale", angle = 90, size =4.0)+
  geom_vline(xintercept = as.numeric(event_0_bundle_end), linetype = "dashed", colour = "coral4") +
  annotate("text",x = event_0_bundle_end+0.35,y=max(ARPDAU$ARPDAU)-0.1*max(ARPDAU$ARPDAU), label = "end_bundle_sale", angle = 90, size =4.0)+
  
  annotate("text",x = event_2_sale + 4, y=min(ARPDAU$ARPDAU),label = event_0, colour = "dodgerblue4")+
  annotate("text",x = event_1_sale + 4, y=min(ARPDAU$ARPDAU),label = event_1, colour = "dodgerblue4")+
  annotate("text",x = event_0_sale + 4, y=min(ARPDAU$ARPDAU),label = event_2, colour = "dodgerblue4")+
  
  ggtitle("ARPDAU (net USD)") +
  scale_x_date(date_breaks = "4 day")


#### 9. Conversion Rate #### 


conversion_rate <-  amplitude("AC", event="unverified_revenue",
                              measured_by="pct_dau", start=ext_date_start, end=ext_date_end,
                              user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids)))) %>% select(value,date) %>% filter(date <= Sys.Date() - 1)

ggplot(conversion_rate, aes(date,value)) + geom_line(size = 1, colour = "darkorchid3") +
  geom_vline(xintercept = as.numeric(event_2_sale), linetype = "dashed", colour = "coral4") +
  annotate("text",x = event_2_sale+0.35,y=max(conversion_rate$value)-0.1*max(conversion_rate$value), label = "first_day_sale", angle = 90, size =4)+
  geom_vline(xintercept = as.numeric(event_2_bundle_start), linetype = "dashed", colour = "coral4")+
  annotate("text",x = event_2_bundle_start + 0.35 ,y=max(conversion_rate$value)-0.1*max(conversion_rate$value), label = "start_bundle_sale", angle = 90, size = 4)+
  geom_vline(xintercept = as.numeric(event_2_bundle_end), linetype = "dashed", colour = "coral4")+
  annotate("text",x = event_2_bundle_end + 0.35,y=max(conversion_rate$value)-0.1*max(conversion_rate$value), label = "end_bundle_sale", angle = 90, size =4)+
  geom_vline(xintercept = as.numeric(event_1_sale), linetype = "dashed", colour = "coral4") +
  annotate("text",x = event_1_sale+0.35,y=max(conversion_rate$value)-0.1*max(conversion_rate$value), label = "first_day_sale", angle = 90, size =4)+
  geom_vline(xintercept = as.numeric(event_1_bundle_start), linetype = "dashed", colour = "coral4")+
  annotate("text",x = event_1_bundle_start + 0.35 ,y=max(conversion_rate$value)-0.1*max(conversion_rate$value), label = "start_bundle_sale", angle = 90, size = 4)+
  geom_vline(xintercept = as.numeric(event_1_bundle_end), linetype = "dashed", colour = "coral4")+
  annotate("text",x = event_1_bundle_end + 0.35,y=max(conversion_rate$value)-0.1*max(conversion_rate$value), label = "end_bundle_sale", angle = 90, size =4)+
  
  geom_vline(xintercept = as.numeric(event_0_sale), linetype = "dashed", colour = "coral4") +
  annotate("text",x = event_0_sale+0.35,y=max(conversion_rate$value)-0.1*max(conversion_rate$value), label = "first_day_sale", angle = 90, size =4)+
  geom_vline(xintercept = as.numeric(event_0_bundle_start), linetype = "dashed", colour = "coral4") +
  annotate("text",x = event_0_bundle_start+0.35,y=max(conversion_rate$value)-0.1*max(conversion_rate$value), label = "first_day_sale", angle = 90, size =4)+
  geom_vline(xintercept = as.numeric(event_0_bundle_end), linetype = "dashed", colour = "coral4") +
  annotate("text",x = event_0_bundle_end+0.35,y=max(conversion_rate$value)-0.1*max(conversion_rate$value), label = "first_day_sale", angle = 90, size =4)+
  
  annotate("text",x = event_2_sale + 4, y=min(conversion_rate$value),label = event_0, colour = "dodgerblue4")+
  annotate("text",x = event_1_sale + 4, y=min(conversion_rate$value),label = event_1, colour = "dodgerblue4")+
  annotate("text",x = event_0_sale + 4, y=min(conversion_rate$value),label = event_2, colour = "dodgerblue4")+
  
  ggtitle("ARPDAU (net USD)") +
  scale_x_date(date_breaks = "4 day")+
  ggtitle("Conversion Rate")

rm(conversion_rate, ARPDAU)
#### 10. Currency conversion percentage ####

prc <-  amplitude("AC", event="CurrencySpent", group_by_properties = list(list(type = "event", value = "purchase")),
                  measured_by="pct_dau", start=ext_date_start, end=ext_date_end,
                  user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(cohort_ids)))) %>% filter(date <= Sys.Date() - 1) %>% 
  filter(property == "SuperTowerRefill" | property == "credit" | property == "callin_cryobomb" | property == "callin_energyboost" 
         | property == "callin_reinforcement" | property =="callin_bomb" | property == "KillBoost") %>% 
  mutate(conversion = value * 100)

ggplot(prc, aes(date,conversion, fill = property)) + geom_area()+  scale_fill_brewer(palette = "Dark2",direction = -1)+
  geom_vline(xintercept = as.numeric(event_2_sale), linetype = "dashed", colour = "coral4") +
  annotate("text",x = event_2_sale+0.35,y=max(prc$conversion)-0.1*max(prc$conversion), label = "first_day_sale", angle = 90, size =4)+
  geom_vline(xintercept = as.numeric(event_2_bundle_start), linetype = "dashed", colour = "coral4")+
  annotate("text",x = event_2_bundle_start + 0.35 ,y=max(prc$conversion)-0.1*max(prc$conversion), label = "start_bundle_sale", angle = 90, size = 4)+
  geom_vline(xintercept = as.numeric(event_2_bundle_end), linetype = "dashed", colour = "coral4")+
  annotate("text",x = event_2_bundle_end + 0.35,y=max(prc$conversion)-0.1*max(prc$conversion), label = "end_bundle_sale", angle = 90, size =4)+
  geom_vline(xintercept = as.numeric(event_1_sale), linetype = "dashed", colour = "coral4") +
  annotate("text",x = event_1_sale+0.35,y=max(prc$conversion)-0.1*max(prc$conversion), label = "first_day_sale", angle = 90, size =4)+
  geom_vline(xintercept = as.numeric(event_1_bundle_start), linetype = "dashed", colour = "coral4")+
  annotate("text",x = event_1_bundle_start + 0.35 ,y=max(prc$conversion)-0.1*max(prc$conversion), label = "start_bundle_sale", angle = 90, size = 4)+
  geom_vline(xintercept = as.numeric(event_1_bundle_end), linetype = "dashed", colour = "coral4")+
  annotate("text",x = event_1_bundle_end + 0.35,y=max(prc$conversion)-0.1*max(prc$conversion), label = "end_bundle_sale", angle = 90, size =4)+
  
  geom_vline(xintercept = as.numeric(event_0_sale), linetype = "dashed", colour = "coral4") +
  annotate("text",x = event_0_sale+0.35,y=max(prc$conversion)-0.1*max(prc$conversion), label = "first_day_sale", angle = 90, size =4)+
  geom_vline(xintercept = as.numeric(event_0_bundle_start), linetype = "dashed", colour = "coral4") +
  annotate("text",x = event_0_bundle_start+0.35,y=max(prc$conversion)-0.1*max(prc$conversion), label = "first_day_sale", angle = 90, size =4)+
  geom_vline(xintercept = as.numeric(event_0_bundle_end), linetype = "dashed", colour = "coral4") +
  annotate("text",x = event_0_bundle_end+0.35,y=max(prc$conversion)-0.1*max(prc$conversion), label = "first_day_sale", angle = 90, size =4)+
  
  annotate("text",x = event_2_sale + 4, y=min(prc$conversion)+0.15,label = event_0, colour = "black")+
  annotate("text",x = event_1_sale + 4, y=min(prc$conversion)+0.15,label = event_1, colour = "black")+
  annotate("text",x = event_0_sale + 4, y=min(prc$conversion)+0.15,label = event_2, colour = "black")+
  scale_x_date(date_breaks = "4 day")+
  ggtitle("% Players spending gems on boosts")


#### 11. Export ####


write.csv(metrics, paste0("AC_", sale_identifier,"_report","_1","_", Sys.Date(), ".csv"))
write.csv(active, paste0("AC_", sale_identifier,"_report","_2","_", Sys.Date(), ".csv"))
write.csv(revenue_1, paste0("AC_", sale_identifier,"_report","_3","_", Sys.Date(), ".csv"))




















