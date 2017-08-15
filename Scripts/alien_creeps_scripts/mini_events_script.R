### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

### Mini-Events Analysis

### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


#### 1. Inputs ####
# hackers_ids is used to eliminate hackers. 
# Please update the cohort before analysing the results so the cohort is up to date with the latest hackers.

mini_event <- "Hero Event"

duration_mini_event <- "2" #Please input the duration of the mini event in days 

event_start_date <- "2017-08-01"
week_before_start <- "2017-07-04"

#### 2. Parameters ####

hackers_ids <- c("artzosc")

event_end_date <- as.Date(event_start_date) + as.numeric(duration_mini_event) - 1
week_before_end <- as.Date(week_before_start) + as.numeric(duration_mini_event) - 1 

ext_date_start <- week_before_start
ext_date_end <- event_end_date + 3

#### 3. Players ####

total_players <- amplitude("AC", event="_active", measured_by="uniques", show_aggregate_over_date_range=T,
                           user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                           start=event_start_date, end=event_end_date) %>% 
  bind_rows(amplitude("AC", event="_active", measured_by="uniques", show_aggregate_over_date_range=T,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                      start=week_before_start, end=week_before_end)) %>% 
  rename(total_players = uniques_over_date_range) %>% 
  select(date, total_players)

dau <- amplitude("AC", event="_active", measured_by="uniques",
                 user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                 start=ext_date_start, end=ext_date_end) %>% 
  rename(dau = value)

#### 3.1 Unique Spenders 

unique_spenders <- amplitude("AC", event="unverified_revenue", measured_by="uniques", show_aggregate_over_date_range = T, 
                             start=event_start_date, end=event_end_date) %>% 
  bind_rows(amplitude("AC", event="unverified_revenue", measured_by="uniques", show_aggregate_over_date_range = T, 
                      start=week_before_start, end=week_before_end)) %>% 
  rename(unique_spenders = uniques_over_date_range) %>% 
  select(date, unique_spenders)

################## 4. Active % #######


active <- amplitude("AC", event="CurrencySpent", group_by_properties = list(list(type = "event", value = "purchase")),
                    measured_by="pct_dau", start=ext_date_start, end=ext_date_end,
                    user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
  mutate(group = ifelse(date >= event_start_date & date <= event_end_date, mini_event, 
                        ifelse(date >= week_before_start & date <= week_before_end, "Week Before", NA))) %>% 
  filter(property == "SuperTowerRefill" | property == "credit" | property == "callin_cryobomb" | property == "callin_energyboost" 
         | property == "callin_reinforcements" | property =="callin_bomb" | property == "KillBoost" | property == "SecondChance") %>% 
  filter(!is.na(group)) %>% 
  group_by(group, property) %>% 
  summarise(active_prc = mean(value, na.rm = T))


################### 5. Average purchases per user purchasing at least once ####



metrics_2 <- amplitude("AC", event="CurrencySpent", group_by_properties = list(list(type = "event", value = "purchase")),
                                  measured_by="totals", start=ext_date_start, end=ext_date_end,
                                  user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>%
  rename(totals = value) %>% 
  left_join(amplitude("AC", event="CurrencySpent",
                      measured_by="uniques", start=ext_date_start, end=ext_date_end,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
              select(value,date) %>% 
              rename(uniques = value), by = "date") %>% 
  mutate(group = ifelse(date >= event_start_date & date <= event_end_date, mini_event, 
                        ifelse(date >= week_before_start & date <= week_before_end, "Week Before", NA))) %>% 
  filter(property == "SuperTowerRefill" | property == "credit" | property == "callin_cryobomb" | property == "callin_energyboost" 
         | property == "callin_reinforcements" | property =="callin_bomb" | property == "KillBoost" | property == "SecondChance") %>% 
  filter(!is.na(group)) %>% 
  group_by(group, property) %>% 
  summarise(ave_purchases_per_user = mean(totals) / mean(uniques)) %>% 
  left_join(., active, by=c("group","property"))



rm(active)

################## 6. Gem Investment #### 

# All gem investment in and outside the event is captured by this magnific beast down here. 

gem_data <- amplitude("AC", event="event_level_finished", group_by_properties = list(list(type = "event", value = "energyboosts_used")),
                      measured_by="totals", start=ext_date_start, end=ext_date_end,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>%
  mutate(prop_sum_energy = value * as.numeric(property)) %>% 
  group_by(date) %>% 
  summarise(event_energy = sum(prop_sum_energy, na.rm = T) * 10) %>% 
  left_join(amplitude("AC", event="Level%20Finished", group_by_properties = list(list(type = "event", value = "energyboosts_used")),
                      measured_by="totals", start=ext_date_start, end=ext_date_end,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
              mutate(prop_sum_energy = value * as.numeric(property)) %>% 
              group_by(date) %>% 
              summarise(energy = sum(prop_sum_energy, na.rm = T) * 10),by="date") %>% 
  left_join(amplitude("AC", event="event_level_finished", group_by_properties = list(list(type = "event", value = "secondChancesBought")),
                      measured_by="totals", start=ext_date_start, end=ext_date_end,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>%
              mutate(prop_sum_second_chance = value * as.numeric(property)) %>% 
              group_by(date) %>% 
              summarise(event_second_chance = sum(prop_sum_second_chance, na.rm = T) * 10), by=c("date")) %>% 
  left_join(amplitude("AC", event="Level%20Finished", group_by_properties = list(list(type = "event", value = "secondChancesBought")),
                      measured_by="totals", start=ext_date_start, end=ext_date_end,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
              mutate(prop_sum_second_chance = value * as.numeric(property)) %>% 
              group_by(date) %>% 
              summarise(second_chance = sum(prop_sum_second_chance, na.rm = T) * 10), by = "date") %>% 
  left_join(amplitude("AC", event="event_level_finished", group_by_properties = list(list(type = "event", value = "airstrikesUsed")),
                      measured_by="totals", start=ext_date_start, end=ext_date_end,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
              mutate(prop_sum_airstrikes = value * as.numeric(property)) %>% 
              group_by(date) %>% 
              summarise(event_airstrikes = sum(prop_sum_airstrikes, na.rm = T) * 2), by=c("date")) %>% 
  left_join(amplitude("AC", event="Level%20Finished", group_by_properties = list(list(type = "event", value = "airstrikesUsed")),
                      measured_by="totals", start=ext_date_start, end=ext_date_end,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
              mutate(prop_sum_airstrikes = value * as.numeric(property)) %>% 
              group_by(date) %>% 
              summarise(airstrikes = sum(prop_sum_airstrikes, na.rm = T) * 2), by = "date") %>% 
  left_join(amplitude("AC", event="event_level_finished", group_by_properties = list(list(type = "event", value = "reinforcesUsed")),
                      measured_by="totals", start=ext_date_start, end=ext_date_end,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
              mutate(prop_sum_reinforcements = value * as.numeric(property)) %>% 
              group_by(date) %>% 
              summarise(event_reinforcements = sum(prop_sum_reinforcements, na.rm = T) * 6), by=c("date")) %>% 
  left_join(amplitude("AC", event="Level%20Finished", group_by_properties = list(list(type = "event", value = "reinforcesUsed")),
                      measured_by="totals", start=ext_date_start, end=ext_date_end,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
              mutate(prop_sum_reinforcements = value * as.numeric(property)) %>% 
              group_by(date) %>% 
              summarise(reinforcements = sum(prop_sum_reinforcements, na.rm = T) * 6), by="date") %>% 
  left_join(amplitude("AC", event="event_level_finished", group_by_properties = list(list(type = "event", value = "cryobombs_used")),
                      measured_by="totals", start=ext_date_start, end=ext_date_end,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
              mutate(prop_sum_cryo = value * as.numeric(property)) %>% 
              group_by(date) %>% 
              summarise(event_cryo = sum(prop_sum_cryo, na.rm = T) * 4), by = c("date")) %>% 
  left_join(amplitude("AC", event="Level%20Finished", group_by_properties = list(list(type = "event", value = "cryobombs_used")),
                      measured_by="totals", start=ext_date_start, end=ext_date_end,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
              mutate(prop_sum_cryo = value * as.numeric(property)) %>% 
              group_by(date) %>% 
              summarise(cryo = sum(prop_sum_cryo, na.rm = T) * 4), by = "date") %>% 
  left_join(amplitude("AC", event="event_level_finished", group_by_properties = list(list(type = "event", value = "superTowerRefillsBought")),
                      measured_by="totals", start=ext_date_start , end=ext_date_end,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
              mutate(prop_sum_tower = value * as.numeric(property)) %>% 
              group_by(date) %>% 
              summarise(event_tesla = sum(prop_sum_tower, na.rm = T) * 6), by=c("date")) %>% 
  left_join(amplitude("AC", event="Level%20Finished", group_by_properties = list(list(type = "event", value = "superTowerRefillsBought")),
                      measured_by="totals", start=ext_date_start , end=ext_date_end,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
              mutate(prop_sum_tower = value * as.numeric(property)) %>% 
              group_by(date) %>% 
              summarise(tesla = sum(prop_sum_tower, na.rm = T) * 6), by="date") %>% 
  left_join(., dau, by="date") %>% 
  mutate(group = ifelse(date >= event_start_date & date <= event_end_date, mini_event, 
                        ifelse(date >= week_before_start & date <= week_before_end, "Week Before", NA)))




########################################## -> Average gem investment

metrics_3 <- gem_data %>% 
  filter(!is.na(group)) %>% 
  group_by(date, group) %>% 
  summarise(ave_energy = (event_energy + energy) / dau,
            ave_second_chance = (event_second_chance + second_chance) / dau,
            ave_airstrikes = (event_airstrikes + airstrikes) / dau,
            ave_reinforcements = (event_reinforcements + reinforcements) / dau,
            ave_cryo =(event_cryo + cryo) / dau, 
            ave_tesla = (event_tesla + tesla) / dau,
            ave_gem_investment = (event_energy + energy + event_second_chance + second_chance + event_airstrikes + airstrikes + 
                                    event_reinforcements + reinforcements + event_cryo + cryo + event_tesla + tesla) / dau) %>% 
  group_by(group) %>% 
  summarise_at(vars(starts_with("ave_")), funs(mean)) %>% 
  melt(.,id.vars=1)


################## 7. General Performance Metrics #### 

revenue <- amplitude("AC", event="unverified_revenue", measured_by="sums",
                     group_by_properties=list(list(type="event", value="$revenue"), list(type="event", value="$revenueType")),
                     start=ext_date_start, end=ext_date_end) %>% filter(property != "(none)") %>% 
  rename(code = property,
         dt = date)

revenue <- make_currency_values_USD(revenue, "USD", "code", "value")

revenue <- revenue %>% 
  rename(date = dt) %>% 
  group_by(date) %>% 
  summarise(revenue = sum(revenue,na.rm=T)) 



# ARPDAU

arpdau <- left_join(dau,revenue,by="date")

arpdau <- arpdau %>% 
  group_by(date) %>% 
  summarise(arpdau = revenue / dau)

ggplot(arpdau, aes(date,arpdau)) + geom_line(size = 1, colour = "green4") + geom_smooth(method = "loess")+
  geom_vline(xintercept = as.numeric(arpdau$date[arpdau$date == event_start_date]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(event_start_date)+3, y=min(arpdau$arpdau),label = mini_event, colour = "dodgerblue4", angle = 90)+
  geom_vline(xintercept = as.numeric(arpdau$date[arpdau$date == event_end_date]), linetype = "dashed", colour = "coral4") +
  geom_vline(xintercept = as.numeric(arpdau$date[arpdau$date == week_before_start]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(week_before_start) + 3, y=min(arpdau$arpdau),label = "Week Before", colour = "dodgerblue4", angle = 90)+
  geom_vline(xintercept = as.numeric(arpdau$date[arpdau$date == week_before_end]), linetype = "dashed", colour = "coral4") +
  scale_x_date(limits = c(min(arpdau$date), Sys.Date()-1)) +
  ggtitle("ARPDAU (net USD)") 


# Conversion Rate

conv_rate <- amplitude("AC", event="unverified_revenue", measured_by="uniques", 
                       start=ext_date_start, end=ext_date_end) %>% 
  rename(spenders = value) %>% 
  left_join(., dau, by="date") %>% 
  group_by(date) %>% 
  summarise(conversion_rate = spenders / dau)


ggplot(conv_rate, aes(date,conversion_rate)) + geom_line(size = 1, colour = "green4") + geom_smooth()+
  geom_vline(xintercept = as.numeric(conv_rate$date[conv_rate$date == event_start_date]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(event_start_date) + 3, y=min(conv_rate$conversion_rate) - 0.5*(min(conv_rate$conversion_rate)),label = mini_event, colour = "dodgerblue4")+
  geom_vline(xintercept = as.numeric(conv_rate$date[conv_rate$date == event_end_date]), linetype = "dashed", colour = "coral4") +
  geom_vline(xintercept = as.numeric(conv_rate$date[conv_rate$date == week_before_start]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(week_before_start) + 3, y=min(conv_rate$conversion_rate)- 0.5*(min(conv_rate$conversion_rate)),label = "Week Before", colour = "dodgerblue4")+
  geom_vline(xintercept = as.numeric(conv_rate$date[conv_rate$date == week_before_end]), linetype = "dashed", colour = "coral4") +
  scale_x_date(date_breaks = "7 day")+
  ggtitle("Conversion Rate")


metrics <- join_all(list(total_players, revenue, unique_spenders, dau, conv_rate, arpdau), by=c("date"))
metrics <- metrics %>% 
  group_by(date) %>% 
  mutate(ARPU = revenue / dau) %>% 
  mutate(group = ifelse(date >= event_start_date & date <= event_end_date, mini_event, 
                        ifelse(date >= week_before_start & date <= week_before_end, "Week Before", NA))) %>% 
  group_by(group) %>% 
  summarise(total_players = mean(total_players),
            revenue = sum(revenue),
            spenders = mean(unique_spenders),
            dau = mean(dau),
            conversion_rate = mean(conversion_rate),
            ARPDAU = mean(arpdau)) %>% 
  mutate(ARPU = revenue / total_players,
         ARPPU = revenue / spenders)


# Revenue by product_id

revenue_id <- amplitude("AC", event="unverified_revenue", measured_by="sums",
                     group_by_properties=list(list(type="event", value="$revenue"), list(type="event", value="$revenueType"), list(type="event", value="$productId")),
                     start=event_start_date, end=event_end_date) %>% filter(property != "(none)") %>% 
  rename(dt = date) %>% 
  bind_rows(amplitude("AC", event="unverified_revenue", measured_by="sums",
                      group_by_properties=list(list(type="event", value="$revenue"), list(type="event", value="$revenueType"), list(type="event", value="$productId")),
                      start=week_before_start, end=week_before_end) %>% filter(property != "(none)") %>% 
              rename(dt = date))
revenue_id <- separate(revenue_id, col = property, into=c("code","product_id"), sep="; ")

revenue_id <- make_currency_values_USD(revenue_id,"USD","code","value")

revenue_id <- revenue_id %>% 
  group_by(dt, product_id) %>% 
  summarise(revenue = sum(revenue,na.rm=T)) %>% 
  mutate(group = ifelse(dt >= event_start_date & dt <= event_end_date, mini_event, 
                                ifelse(dt >= week_before_start & dt <= week_before_end, "Week Before", NA))) %>% 
  group_by(group, product_id) %>% 
  summarise(revenue = sum(revenue,na.rm=T)) %>% 
  filter(product_id %nin% c("InfiniteEventTicket", "EventTicketRefill")) 

revenue_id$group <- factor(revenue_id$group, levels = c("Week Before",mini_event), labels=c("Week Before", mini_event))


ggplot(revenue_id, aes(product_id,revenue,fill=group)) + geom_bar(stat = "identity", position = "dodge") + coord_flip()+
  scale_fill_manual(values = c("dodgerblue3","green3"))+
  ggtitle("Revenue by Product-Id")


# Percentage change in the revenue by product

change <- revenue_id %>% 
  group_by() %>% 
  summarise(SkyBolt = ((revenue[product_id == "HeroSkyBolt" & group == mini_event]) / (revenue[product_id == "HeroSkyBolt" & group == "Week Before"]) - 1) * 100 ,
            Electro = ((revenue[product_id == "HeroElectro" & group == mini_event]) / (revenue[product_id == "HeroElectro" & group == "Week Before"]) - 1) * 100 ,
            Mech = ((revenue[product_id == "HeroMech" & group == mini_event]) / (revenue[product_id == "HeroMech" & group == "Week Before"]) - 1) * 100,
            Psyker = ((revenue[product_id == "HeroPsyker" & group == mini_event]) / (revenue[product_id == "HeroPsyker" & group == "Week Before"]) - 1) * 100,
            Roxie = ((revenue[product_id == "HeroRoxie" & group == mini_event]) / (revenue[product_id == "HeroRoxie" & group == "Week Before"]) - 1) * 100,
            GemPack1 = ((revenue[product_id == "GemPack1" & group == mini_event]) / (revenue[product_id == "GemPack1" & group == "Week Before"]) - 1) * 100,
            GemPack2 = ((revenue[product_id == "GemPack2" & group == mini_event]) / (revenue[product_id == "GemPack2" & group == "Week Before"]) - 1) * 100,
            GemPack3 = ((revenue[product_id == "GemPack3" & group == mini_event]) / (revenue[product_id == "GemPack3" & group == "Week Before"]) - 1) * 100,
            GemPack4 = ((revenue[product_id == "GemPack4" & group == mini_event]) / (revenue[product_id == "GemPack4" & group == "Week Before"]) - 1) * 100,
            GemPack5 = ((revenue[product_id == "GemPack5" & group == mini_event]) / (revenue[product_id == "GemPack5" & group == "Week Before"]) - 1) * 100) %>% 
  round(1) %>% 
  melt(.,id.vars=0)

ggplot(change, aes(variable,value)) + geom_bar(stat = "identity", fill = "dodgerblue3") +
  geom_text(aes(label=paste0(value,"%")),vjust=-0.25,position=position_dodge(.9), size=3.5, colour = "black")+
  ggtitle("Revenue Amount Change During Mini-Event Compared to Week Before")

# Currency Spent

currency_spent <- amplitude("AC", event="CurrencySpent", measured_by="totals",
                            group_by_properties=list(list(type="event", value="purchase")),
                            start=event_start_date, end=event_end_date) %>% filter(property != "(none)") %>% 
  bind_rows(amplitude("AC", event="CurrencySpent", measured_by="totals",
                      group_by_properties=list(list(type="event", value="purchase")),
                      start=week_before_start, end=week_before_end) %>% filter(property != "(none)")) %>% 
  mutate(group = ifelse(date >= event_start_date & date <= event_end_date, mini_event, 
                        ifelse(date >= week_before_start & date <= week_before_end, "Week Before", NA))) %>% 
  filter(grepl("Hero",property)) %>% 
  group_by(group) %>% 
  summarise(Sarge = sum(value[grepl("HeroSarge",property)]),
            Roxie = sum(value[grepl("HeroRoxie",property)]),
            Apache = sum(value[grepl("HeroApache",property)]),
            Electro = sum(value[grepl("HeroElectro",property)]),
            Psyker = sum(value[grepl("HeroPsyker",property)]),
            Mech = sum(value[grepl("HeroMech",property)]),
            Barrage = sum(value[grepl("HeroCyborg",property)]),
            Scorch = sum(value[grepl("LaserHero",property)]),
            Avalanche = sum(value[grepl("HeroFreeze",property)]),
            Gravitas = sum(value[grepl("HeroGravitas",property)])) %>% 
  melt(.,id.vars=1) %>% 
  summarise(Sarge = (value[variable == "Sarge" & group == mini_event] / value[variable == "Sarge" & group == "Week Before"] - 1) * 100,
            Roxie = (value[variable == "Roxie" & group == mini_event] / value[variable == "Roxie" & group == "Week Before"] - 1) * 100,
            SkyBolt = (value[variable == "Apache" & group == mini_event] / value[variable == "Apache" & group == "Week Before"] - 1) * 100,
            Electro = (value[variable == "Electro" & group == mini_event] / value[variable == "Electro" & group == "Week Before"] - 1) * 100,
            Psyker = (value[variable == "Psyker" & group == mini_event] / value[variable == "Psyker" & group == "Week Before"] - 1) * 100,
            Mech = (value[variable == "Mech" & group == mini_event] / value[variable == "Mech" & group == "Week Before"] - 1) * 100,
            Barrage = (value[variable == "Barrage" & group == mini_event] / value[variable == "Barrage" & group == "Week Before"] - 1) * 100,
            Scorch = (value[variable == "Scorch" & group == mini_event] / value[variable == "Scorch" & group == "Week Before"] - 1) * 100,
            Avalanche = (value[variable == "Avalanche" & group == mini_event] / value[variable == "Avalanche" & group == "Week Before"] - 1) * 100,
            Gravitas = (value[variable == "Gravitas" & group == mini_event] / value[variable == "Gravitas" & group == "Week Before"] - 1) * 100) %>% 
  melt(.,id.vars=0) %>% 
  filter(value <= 200)

ggplot(currency_spent, aes(variable,value)) + geom_bar(stat = "identity", fill = "dodgerblue3") +
  geom_text(aes(label=paste0(round(value,1),"%")),vjust=-0.25,position=position_dodge(.9), size=3.5, colour = "black")+
  ggtitle("Hero Upgrades Compared to Week Before")            


# Progression through levels 

# funnel_frame <- data.frame(event_type=c("First Launch", "Level Finished", "Level Finished", "Level Finished", "Level Finished", "Level Finished", "Level Finished", "Level Finished", "Level Finished", "Level Finished", "Level Finished",
#                                         "Level Finished", "Level Finished", "Level Finished", "Level Finished", "Level Finished", "Level Finished", "Level Finished", "Level Finished", "Level Finished", "Level Finished"),
#                            subprop_key=c(NA, "levelName", "levelName", "levelName", "levelName", "levelName", "levelName", "levelName", "levelName", "levelName", "levelName",
#                                          "levelName", "levelName", "levelName", "levelName", "levelName", "levelName", "levelName", "levelName", "levelName", "levelName"),
#                            subprop_op=c(NA, "is", "is", "is", "is", "is", "is", "is", "is", "is", "is",
#                                         "is", "is", "is", "is", "is", "is", "is", "is", "is", "is"),
#                            subprop_value=c(NA, "C1_1e", "C1_9e", "C1_2e", "C1_7e", "C1_3e", "C1_8e", "C1_4e", "C1_6e", "C1_10e", "C1_5e",
#                                            "C2_1e", "C2_7e", "C2_6e", "C2_2e", "C2_8e", "C2_9e", "C2_3e", "C2_10e", "C2_4e", "C2_5e"), stringsAsFactors=F)
# 
# 
# 
# progression <- amplitude("AC", f="funnel", funnel_list = funnel_frame, 
#                          start=event_start_date, end=event_end_date) %>% 
#   rename(progression_mini_event = value) %>% 
#   bind_cols(amplitude("AC", f="funnel", funnel_list = funnel_frame, 
#                       start=week_before_start, end=week_before_end) %>% 
#               rename(progression_before = value)) %>% 
#   mutate(Level = 0:20) %>% 
#   melt(.,id.vars = 3)
# 
# progression$variable <- factor(progression$variable, levels = c("progression_before","progression_mini_event"), labels=c("Week Before", mini_event))
# 
# ggplot(progression, aes(Level, value, fill=variable))+geom_bar(stat = "identity", position = "dodge")+
#   scale_fill_manual(values = c("dodgerblue3","green3"))+
#   scale_x_continuous(name = "Level", breaks = 1:20)+
#   geom_text(aes(label=round(value,2)),vjust=-0.25,position=position_dodge(.9), size=3.5, colour = "black")+
#   ggtitle("Progression Of New Players")
#   
#   


# Hero Leveling Up 

hero_1 <- amplitude("AC", event="Level Finished", measured_by="sums", 
                    group_by_properties=list(list(type="event", value="heroLevel")),
                    start=ext_date_start, end=ext_date_end) %>% 
  select(date, value) %>% 
  rename(propsum = value) %>% 
  left_join((amplitude("AC", event="Level Finished", measured_by="totals", 
                      group_by_properties=list(list(type="event", value="heroLevel")),
                      start=ext_date_start, end=ext_date_end) %>% 
              group_by(date) %>% 
              summarise(totals = sum(value))), by = "date") %>% 
  group_by(date) %>% 
  summarise(avg_first_hero_level = propsum / totals) 

hero_2 <- amplitude("AC", event="Level Finished", measured_by="sums", 
                    group_by_properties=list(list(type="event", value="heroLevel2")),
                    start=ext_date_start, end=ext_date_end) %>% 
  select(date, value) %>% 
  rename(propsum = value) %>% 
  left_join((amplitude("AC", event="Level Finished", measured_by="totals", 
                       group_by_properties=list(list(type="event", value="heroLevel2")),
                       start=ext_date_start, end=ext_date_end) %>% 
               group_by(date) %>% 
               summarise(totals = sum(value))), by = "date") %>% 
  group_by(date) %>% 
  summarise(avg_second_hero_level = propsum / totals)

  
ggplot(hero_1, aes(date, avg_first_hero_level)) + geom_line(size = 1, colour = "green 4") + 
  geom_vline(xintercept = as.numeric(hero_1$date[hero_1$date == event_start_date]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(event_start_date) + 3, y=min(hero_1$avg_first_hero_level) - 0.01*(min(hero_1$avg_first_hero_level)),label = mini_event, colour = "dodgerblue4")+
  geom_vline(xintercept = as.numeric(hero_1$date[hero_1$date == event_end_date]), linetype = "dashed", colour = "coral4") +
  geom_vline(xintercept = as.numeric(hero_1$date[hero_1$date == week_before_start]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(week_before_start) + 3, y=min(hero_1$avg_first_hero_level)- 0.01*(min(hero_1$avg_first_hero_level)),label = "Week Before", colour = "dodgerblue4")+
  geom_vline(xintercept = as.numeric(hero_1$date[hero_1$date == week_before_end]), linetype = "dashed", colour = "coral4") +
  scale_x_date(date_breaks = "7 day")+
  ggtitle("Average Level of Hero 1")
   
ggplot(hero_2, aes(date, avg_second_hero_level)) + geom_line(size = 1, colour = "green 4") + 
  geom_vline(xintercept = as.numeric(hero_2$date[hero_2$date == event_start_date]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(event_start_date) + 3, y=min(hero_2$avg_second_hero_level) - 0.05*(min(hero_2$avg_second_hero_level)),label = mini_event, colour = "dodgerblue4")+
  geom_vline(xintercept = as.numeric(hero_2$date[hero_2$date == event_end_date]), linetype = "dashed", colour = "coral4") +
  geom_vline(xintercept = as.numeric(hero_2$date[hero_2$date == week_before_start]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(week_before_start) + 3, y=min(hero_2$avg_second_hero_level)- 0.05*(min(hero_2$avg_second_hero_level)),label = "Week Before", colour = "dodgerblue4")+
  geom_vline(xintercept = as.numeric(hero_2$date[hero_2$date == week_before_end]), linetype = "dashed", colour = "coral4") +
  scale_x_date(date_breaks = "7 day")+
  ggtitle("Average Level of Hero 2")



hero <- left_join(hero_1, hero_2, by="date") %>% 
  mutate(group = ifelse(date >= event_start_date & date <= event_end_date, mini_event, 
                        ifelse(date >= week_before_start & date <= week_before_end, "Week Before", NA))) %>% 
  group_by(group) %>% 
  filter(!is.na(group)) %>% 
  summarise(avg_hero_1_level = mean(avg_first_hero_level),
            avg_hero_2_level = mean(avg_second_hero_level))

metrics <- left_join(metrics, hero, by="group")

rm(arpdau, change, conv_rate, currency_spent, dau, funnel_frame, hero, hero_1, hero_2, progression, revenue, revenue_id, total_players, unique_spenders)


# Levels Played

levels_played <- amplitude("AC", event="Level Finished", measured_by="totals", 
                       group_by_properties=list(list(type="user", value="gp:furthestLevel")),
                       user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                       start=ext_date_start, end=ext_date_end) %>% 
  rename(totals = value,
         level = property) %>% 
  left_join(amplitude("AC", event = "Level Finished", measured_by="uniques",
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                      start=ext_date_start, end=ext_date_end), by="date") %>% 
  group_by(date, level) %>% 
  summarise(attempts = totals / value) %>% 
  filter(level != "(none)") %>% 
  mutate(group = ifelse(date >= event_start_date & date <= event_end_date, mini_event, 
                        ifelse(date >= week_before_start & date <= week_before_end, "Week Before", NA))) %>% 
  filter(!is.na(group)) %>% 
  group_by(level, group) %>% 
  summarise(ave_attempt_per_level = mean(attempts)) 


ggplot(levels_played, aes(as.numeric(level), ave_attempt_per_level, fill = group)) + geom_bar(stat = "identity", position="dodge") + 
  scale_fill_manual(values = c("green3","dodgerblue3"))+
  scale_x_continuous(name = "Furthest Level", breaks = 1:50)+
  ggtitle("Average Attempts Per Player Per Furthest Level Reached")


# Pass Rates

pass_rate <- amplitude("AC", event="Level Finished", measured_by="totals", 
                           where = list(list(subprop_type = "event", subprop_key = "outcome", subprop_op = "is", subprop_value=list("won"))),
                           group_by_properties=list(list(type="user", value="gp:furthestLevel")),
                           user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                           start=ext_date_start, end=ext_date_end) %>% 
  rename(totals_won = value) %>% 
  left_join(amplitude("AC", event="Level Finished", measured_by="totals", 
                      group_by_properties=list(list(type="user", value="gp:furthestLevel")),
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                      start=ext_date_start, end=ext_date_end), by=c("date","property")) %>% 
  mutate(group = ifelse(date >= event_start_date & date <= event_end_date, mini_event, 
                        ifelse(date >= week_before_start & date <= week_before_end, "Week Before", NA))) %>% 
  filter(!is.na(group) & property != "(none)") %>% 
  group_by(date, group, property) %>%
  summarise(pass_rate = totals_won / value) %>% 
  group_by(group, property) %>% 
  summarise(ave_pass_rate = mean(pass_rate))

ggplot(pass_rate, aes(as.numeric(property), ave_pass_rate, fill = group)) + geom_bar(stat = "identity", position="dodge") + 
  scale_fill_manual(values = c("green3", "dodgerblue3"))+
  scale_x_continuous(name = "Furthest Level", breaks = 1:50)+
  ggtitle("Average Pass Rate Per Furthest Level Reached")

rm(gem_data, levels_played, pass_rate)

################# 8. Write-up Results ####

dir.create(paste0("D:\\R Working File\\1. ACTD\\",Sys.Date()," - ACTD - ", mini_event))
setwd(paste0("D:\\R Working File\\1. ACTD\\",Sys.Date()," - ACTD - ", mini_event))

write.csv(metrics, "metrics_1.csv")
write.csv(metrics_2, "metrics_2.csv")
write.csv(metrics_3, "metrics_3.csv")

