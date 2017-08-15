### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

### Events Analysis

### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


#### 1. Inputs ####
# hackers_ids is used to eliminate hackers. 
# Please update the cohort before analysing the results so the cohort is up to date with the latest hackers.


sale_identifier = "Event 12 - Gunker (II)"

# Insert the date of the start of the event and the name of the events. 

event_0 <- "event_12"
event_1 <- "event_11"

event_0_date <- "2017-07-21"
event_1_date <- "2017-06-16"



#### 2. Parameters ####

source('~/amplitude-dashboard/dashboard-code/SharedConfig.R')
hackers_ids <- c("artzosc")
event_0_end <- as.Date(event_0_date)+9
event_1_end <- as.Date(event_1_date)+9

ext_date_start <- Sys.Date()-60
ext_date_end <- Sys.Date()-1


#### 3. Collecting player numbers ####


event_players <- amplitude("AC", event="event_level_finished", measured_by="uniques", show_aggregate_over_date_range=T,
                           user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                           start=event_0_date, end=event_0_end) %>% 
  bind_rows(amplitude("AC", event="event_level_finished", measured_by="uniques", show_aggregate_over_date_range=T,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                      start=event_1_date, end=event_1_end)) %>% 
  rename(event_players = uniques_over_date_range,
         event_dau = value) %>% 
  mutate(group=ifelse(date >= event_0_date & date <= event_0_end, event_0, 
                      ifelse(date >= event_1_date & date <= event_1_end, event_1, NA)))



#### 3.1 Get number of players grouped by the event level 


level_players <- amplitude("AC", event="event_level_finished", measured_by="uniques", show_aggregate_over_date_range=T,
                           group_by_properties = list(list(type = "event", value = "levelName")),
                           user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                           start=event_0_date, end=event_0_end) %>% 
  bind_rows(amplitude("AC", event="event_level_finished", measured_by="uniques", show_aggregate_over_date_range=T,
                      group_by_properties = list(list(type = "event", value = "levelName")),
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                      start=event_1_date, end=event_1_end)) %>% 
  rename(event_players = uniques_over_date_range,
         event_dau = value,
         level = property) %>% 
  mutate(group=ifelse(date >= event_0_date & date <= event_0_end, event_0, 
                      ifelse(date >= event_1_date & date <= event_1_end, event_1, NA)))



#### 3.2 Get number of players active during the event period (not necesarily playing the event) 


total_players <- amplitude("AC", event="_active", measured_by="uniques", show_aggregate_over_date_range=T,
                           user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                           start=event_0_date, end=event_0_end) %>% 
  bind_rows(amplitude("AC", event="_active", measured_by="uniques", show_aggregate_over_date_range=T,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                      start=event_1_date, end=event_1_end)) %>% 
  rename(total_players = uniques_over_date_range)


################## 4. General Performance Metrics #### 

#### 4.1 Spenders 


unique_spenders <- amplitude("AC", event="unverified_revenue", measured_by="uniques", show_aggregate_over_date_range = T, 
                             start=event_0_date, end=event_0_end) %>% 
  bind_rows(amplitude("AC", event="unverified_revenue", measured_by="uniques", show_aggregate_over_date_range = T, 
                      start=event_1_date, end=event_1_end)) %>% 
  rename(unique_spenders = uniques_over_date_range) %>% 
  select(date, unique_spenders)


#### 4.2 Revenue 

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

metrics <- join_all(list(total_players, revenue, unique_spenders, event_players), by=c("date"))

#### 4.2.1 Revenue Graph


temp <- revenue %>% 
  mutate(group=ifelse(date >= event_0_date & date <= event_0_end, event_0, 
                      ifelse(date >= event_1_date & date <= event_1_end, event_1, NA)))

ggplot(temp, aes(date, revenue)) + geom_line(size = 1, colour = "green4")+
  geom_vline(xintercept = as.numeric(temp$date[temp$date == event_0_date]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(event_0_date) + 5, y=min(temp$revenue),label = event_0, colour = "dodgerblue4")+
  geom_vline(xintercept = as.numeric(temp$date[temp$date == event_0_end]), linetype = "dashed", colour = "coral4") +
  geom_vline(xintercept = as.numeric(temp$date[temp$date == event_1_date]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(event_1_date) + 5, y=min(temp$revenue),label = event_1, colour = "dodgerblue4")+
  geom_vline(xintercept = as.numeric(temp$date[temp$date == event_1_end]), linetype = "dashed", colour = "coral4") +
  scale_x_date(limits = c(min(temp$date), Sys.Date()-1)) +
  ggtitle("Revenue (net USD)") 


#### 4.4 Graph ARPDAU and Conversion Rate ####

# For graphs the above data is not valid since it has only events data and not long term trends. New API queries are required.

# The number of DAU is required for everyday. 

dau <- amplitude("AC", event="_active", measured_by="uniques",
                 user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                 start=ext_date_start, end=ext_date_end) %>% 
  rename(dau = value) 
  # %>% 
  # mutate(group = ifelse(date >= event_0_date & date <= event_0_end, event_0, 
                        # ifelse(date >= event_1_date & date <= event_1_end, event_1, NA)))


## ARPDAU

arpdau <- left_join(dau,revenue,by="date")

arpdau <- arpdau %>% 
  group_by(date) %>% 
  summarise(arpdau = revenue / dau)

ggplot(arpdau, aes(date,arpdau)) + geom_line(size = 1, colour = "green4") + geom_smooth()+
  geom_vline(xintercept = as.numeric(arpdau$date[arpdau$date == event_0_date]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(event_0_date) + 5, y=min(arpdau$arpdau),label = event_0, colour = "dodgerblue4")+
  geom_vline(xintercept = as.numeric(arpdau$date[arpdau$date == event_0_end]), linetype = "dashed", colour = "coral4") +
  geom_vline(xintercept = as.numeric(arpdau$date[arpdau$date == event_1_date]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(event_1_date) + 5, y=min(arpdau$arpdau),label = event_1, colour = "dodgerblue4")+
  geom_vline(xintercept = as.numeric(arpdau$date[arpdau$date == event_1_end]), linetype = "dashed", colour = "coral4") +
  scale_x_date(limits = c(min(arpdau$date), Sys.Date()-1)) +
  ggtitle("ARPDAU (net USD)") 

## Conversion Rate

conv_rate <- amplitude("AC", event="unverified_revenue", measured_by="uniques", 
                       start=ext_date_start, end=ext_date_end) %>% 
  rename(spenders = value) %>% 
  left_join(., dau, by="date") %>% 
  group_by(date) %>% 
  summarise(conversion_rate = spenders / dau)


ggplot(conv_rate, aes(date,conversion_rate)) + geom_line(size = 1, colour = "green4") + geom_smooth()+
  geom_vline(xintercept = as.numeric(conv_rate$date[conv_rate$date == event_0_date]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(event_0_date) + 5, y=min(conv_rate$conversion_rate),label = event_0, colour = "dodgerblue4")+
  geom_vline(xintercept = as.numeric(conv_rate$date[conv_rate$date == event_0_end]), linetype = "dashed", colour = "coral4") +
  geom_vline(xintercept = as.numeric(conv_rate$date[conv_rate$date == event_1_date]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(event_1_date) + 5, y=min(conv_rate$conversion_rate),label = event_1, colour = "dodgerblue4")+
  geom_vline(xintercept = as.numeric(conv_rate$date[conv_rate$date == event_1_end]), linetype = "dashed", colour = "coral4") +
  scale_x_date(date_breaks = "7 day")+
  ggtitle("Conversion Rate")


metrics <- join_all(list(dau, arpdau, conv_rate, metrics),by="date")
rm(revenue, arpdau, conv_rate, revenue, total_players, unique_spenders)


################## 5. Gem Investment #### 

# All gem investment in and outside the event is captured by this magnific beast down here. 

gem_data <- amplitude("AC", event="event_level_finished", group_by_properties = list(list(type = "event", value = "energyboosts_used"), list(type = "event", value = "levelName")),
                  measured_by="totals", start=ext_date_start, end=ext_date_end,
                  user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>%
  rename(level = property2) %>% 
  mutate(prop_sum_energy = value * as.numeric(property)) %>% 
  group_by(date, level) %>% 
  summarise(event_energy = sum(prop_sum_energy, na.rm = T) * 10) %>% 
  left_join(amplitude("AC", event="Level%20Finished", group_by_properties = list(list(type = "event", value = "energyboosts_used")),
                      measured_by="totals", start=ext_date_start, end=ext_date_end,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
              mutate(prop_sum_energy = value * as.numeric(property)) %>% 
              group_by(date) %>% 
              summarise(energy = sum(prop_sum_energy, na.rm = T) * 10),by="date") %>% 
  left_join(amplitude("AC", event="event_level_finished", group_by_properties = list(list(type = "event", value = "secondChancesBought"), list(type = "event", value = "levelName")),
                      measured_by="totals", start=ext_date_start, end=ext_date_end,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
              rename(level = property2) %>% 
              mutate(prop_sum_second_chance = value * as.numeric(property)) %>% 
              group_by(date, level) %>% 
              summarise(event_second_chance = sum(prop_sum_second_chance, na.rm = T) * 10), by=c("date","level")) %>% 
  left_join(amplitude("AC", event="Level%20Finished", group_by_properties = list(list(type = "event", value = "secondChancesBought")),
                      measured_by="totals", start=ext_date_start, end=ext_date_end,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
              mutate(prop_sum_second_chance = value * as.numeric(property)) %>% 
              group_by(date) %>% 
              summarise(second_chance = sum(prop_sum_second_chance, na.rm = T) * 10), by = "date") %>% 
  left_join(amplitude("AC", event="event_level_finished", group_by_properties = list(list(type = "event", value = "airstrikesUsed"), list(type = "event", value = "levelName")),
                      measured_by="totals", start=ext_date_start, end=ext_date_end,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
              rename(level = property2) %>% 
              mutate(prop_sum_airstrikes = value * as.numeric(property)) %>% 
              group_by(date, level) %>% 
              summarise(event_airstrikes = sum(prop_sum_airstrikes, na.rm = T) * 2), by=c("date","level")) %>% 
  left_join(amplitude("AC", event="Level%20Finished", group_by_properties = list(list(type = "event", value = "airstrikesUsed")),
                      measured_by="totals", start=ext_date_start, end=ext_date_end,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
              mutate(prop_sum_airstrikes = value * as.numeric(property)) %>% 
              group_by(date) %>% 
              summarise(airstrikes = sum(prop_sum_airstrikes, na.rm = T) * 2), by = "date") %>% 
  left_join(amplitude("AC", event="event_level_finished", group_by_properties = list(list(type = "event", value = "reinforcesUsed"), list(type = "event", value = "levelName")),
                      measured_by="totals", start=ext_date_start, end=ext_date_end,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
              rename(level = property2) %>% 
              mutate(prop_sum_reinforcements = value * as.numeric(property)) %>% 
              group_by(date, level) %>% 
              summarise(event_reinforcements = sum(prop_sum_reinforcements, na.rm = T) * 6), by=c("date","level")) %>% 
  left_join(amplitude("AC", event="Level%20Finished", group_by_properties = list(list(type = "event", value = "reinforcesUsed")),
                      measured_by="totals", start=ext_date_start, end=ext_date_end,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
              mutate(prop_sum_reinforcements = value * as.numeric(property)) %>% 
              group_by(date) %>% 
              summarise(reinforcements = sum(prop_sum_reinforcements, na.rm = T) * 6), by="date") %>% 
  left_join(amplitude("AC", event="event_level_finished", group_by_properties = list(list(type = "event", value = "cryobombs_used"), list(type = "event", value = "levelName")),
                      measured_by="totals", start=ext_date_start, end=ext_date_end,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
              rename(level = property2) %>% 
              mutate(prop_sum_cryo = value * as.numeric(property)) %>% 
              group_by(date, level) %>% 
              summarise(event_cryo = sum(prop_sum_cryo, na.rm = T) * 4), by = c("date","level")) %>% 
  left_join(amplitude("AC", event="Level%20Finished", group_by_properties = list(list(type = "event", value = "cryobombs_used")),
                      measured_by="totals", start=ext_date_start, end=ext_date_end,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
              mutate(prop_sum_cryo = value * as.numeric(property)) %>% 
              group_by(date) %>% 
              summarise(cryo = sum(prop_sum_cryo, na.rm = T) * 4), by = "date") %>% 
  left_join(amplitude("AC", event="event_level_finished", group_by_properties = list(list(type = "event", value = "superTowerRefillsBought"), list(type = "event", value = "levelName")),
                      measured_by="totals", start=ext_date_start , end=ext_date_end,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
              rename(level = property2) %>% 
              mutate(prop_sum_tower = value * as.numeric(property)) %>% 
              group_by(date, level) %>% 
              summarise(event_tesla = sum(prop_sum_tower, na.rm = T) * 6), by=c("date", "level")) %>% 
  left_join(amplitude("AC", event="Level%20Finished", group_by_properties = list(list(type = "event", value = "superTowerRefillsBought")),
                      measured_by="totals", start=ext_date_start , end=ext_date_end,
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids)))) %>% 
              mutate(prop_sum_tower = value * as.numeric(property)) %>% 
              group_by(date) %>% 
              summarise(tesla = sum(prop_sum_tower, na.rm = T) * 6), by="date") %>% 
  left_join(level_players, by=c("date","level"))




########################################## -> Average gem investment

gem_event <- gem_data %>% 
  group_by(date) %>% 
  summarise(event_energy = sum(event_energy, na.rm = T),
            energy = mean(energy),
            event_second_chance = sum(event_second_chance),
            second_chance = mean(second_chance),
            event_airstrikes = sum(event_airstrikes),
            airstrikes = mean(airstrikes),
            event_reinforcements = sum(event_reinforcements),
            reinforcements = mean(reinforcements),
            event_cryo = sum(event_cryo),
            cryo = mean(cryo),
            event_tesla = sum(event_tesla),
            tesla = mean(tesla)) %>% 
  mutate(out_of_event_gem_investment = airstrikes + cryo + energy + reinforcements + second_chance + tesla,
         event_gem_investment = event_airstrikes + event_cryo + event_energy + event_reinforcements + event_second_chance + event_tesla,
         all_gem_investment = event_airstrikes + airstrikes + event_cryo + cryo + energy + event_energy + reinforcements + event_reinforcements + second_chance + event_second_chance + tesla + event_tesla) %>% 
  left_join(., dau, by="date") %>%
  left_join(.,event_players, by=c("date")) %>% 
  group_by(date) %>% 
  mutate(out_of_event_gem_investment = out_of_event_gem_investment / event_dau,
            event_gem_investment = event_gem_investment / event_dau,
            all_gem_investment = all_gem_investment / dau)



# Overall Gem Investment Graph

ggplot(gem_event, aes(date,all_gem_investment)) + geom_line(size = 1, colour = "green4") + geom_smooth()+
  geom_vline(xintercept = as.numeric(gem_event$date[gem_event$date == event_0_date]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(event_0_date) + 5, y=min(gem_event$all_gem_investment),label = event_0, colour = "dodgerblue4") +
  geom_vline(xintercept = as.numeric(gem_event$date[gem_event$date == event_0_end]), linetype = "dashed", colour = "coral4") +
  geom_vline(xintercept = as.numeric(gem_event$date[gem_event$date == event_1_date]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(event_1_date) + 5, y=min(gem_event$all_gem_investment),label = event_1, colour = "dodgerblue4") +
  geom_vline(xintercept = as.numeric(gem_event$date[gem_event$date == event_1_end]), linetype = "dashed", colour = "coral4") +
  ggtitle("Overall Gem Investment")

# Gem investment on and out of the event by event players.

temp <- gem_event %>% 
  select(date, event_gem_investment, out_of_event_gem_investment) %>% 
  melt(.,id.vars=1)

temp$variable <- factor(temp$variable, levels = c("event_gem_investment", "out_of_event_gem_investment"), labels=c("Event Investment","Out of Event Investment"))
temp$value[is.na(temp$value)] <- 0

ggplot(temp, aes(date, value, fill = variable)) + geom_area() +
  geom_vline(xintercept = as.numeric(temp$date[temp$date == event_0_date]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(event_0_date) + 5, y=max(temp$value),label = event_0, colour = "dodgerblue4") +
  geom_vline(xintercept = as.numeric(temp$date[temp$date == event_0_end]), linetype = "dashed", colour = "coral4") +
  geom_vline(xintercept = as.numeric(temp$date[temp$date == event_1_date]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(event_1_date) + 5, y=max(temp$value),label = event_1, colour = "dodgerblue4") +
  geom_vline(xintercept = as.numeric(temp$date[temp$date == event_1_end]), linetype = "dashed", colour = "coral4") +
  scale_fill_manual(values = c("dodgerblue3","green3"))+
  ggtitle("Event Gem Investment by Event Players")

# Area graph of all the boosts used throughout the selected period of time.

temp <- gem_event %>% 
  group_by(date) %>% 
  summarise(energy = event_energy + energy,
            second_chance = event_second_chance + second_chance,
            airstrikes = event_airstrikes + airstrikes,
            reinforcements = event_reinforcements + reinforcements,
            cryo = event_cryo + cryo,
            tesla = event_tesla + tesla) %>% 
  left_join(., dau, by="date") %>% 
  group_by(date) %>% 
  summarise(energy = energy / dau,
            second_chance = second_chance / dau,
            airstrikes = airstrikes / dau,
            reinforcements = reinforcements / dau,
            cryo = cryo / dau,
            tesla = tesla / dau) %>% 
  melt(.,id.vars = 1)

ggplot(temp, aes(date, value, fill = variable)) + geom_area() + scale_fill_brewer(palette = "Dark2",direction = -1) +
  geom_vline(xintercept = as.numeric(temp$date[temp$date == event_0_date]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(event_0_date) + 5, y=max(temp$value),label = event_0, colour = "dodgerblue4") +
  geom_vline(xintercept = as.numeric(temp$date[temp$date == event_0_end]), linetype = "dashed", colour = "coral4") +
  geom_vline(xintercept = as.numeric(temp$date[temp$date == event_1_date]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(event_1_date) + 5, y=max(temp$value),label = event_1, colour = "dodgerblue4") +
  geom_vline(xintercept = as.numeric(temp$date[temp$date == event_1_end]), linetype = "dashed", colour = "coral4") +
  ggtitle("Gem Investment by All Active Players")


# Event level based graphs

gem_level <- gem_data %>% 
  filter(!is.na(group)) %>% 
  group_by(date, level) %>% 
  summarise(ave_energy = event_energy / event_dau,
            ave_second_chance = event_second_chance / event_dau,
            ave_airstrikes = event_airstrikes / event_dau,
            ave_reinforcements = event_reinforcements / event_dau,
            ave_cryo = event_cryo / event_dau, 
            ave_tesla = event_tesla / event_dau,
            ave_gem_investment = (event_energy + event_second_chance + event_airstrikes + event_reinforcements + event_cryo + event_tesla) / event_dau) %>% 
  mutate(group = ifelse(date >= event_0_date & date <= event_0_end, event_0, 
                        ifelse(date >= event_1_date & date <= event_1_end, event_1, NA))) %>% 
  group_by(level, group) %>% 
  summarise_at(vars(starts_with("ave_")), funs(mean))
  

temp <- melt(gem_level,id.vars=1:2) %>% 
  filter(variable != "ave_gem_investment")

ggplot(temp, aes(level,value,fill=group)) + geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(.~variable) + 
  scale_fill_manual(values = c("dodgerblue3","green3"))+
  ggtitle("Average Event Gem Investment")


rm(temp)


################## 6. Tickets #### 


ticket_data <- amplitude("AC", event="unverified_revenue", measured_by="totals", 
                             where = list(list(subprop_type = "event", subprop_key = "$productId", subprop_op = "is", subprop_value = "InfiniteEventTicket")),
                             user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                             start=ext_date_start, end=ext_date_end) %>% 
  rename(infinite_ticket = value) %>%
  left_join(amplitude("AC", event="unverified_revenue", measured_by="totals", 
                      where = list(list(subprop_type = "event", subprop_key = "$productId", subprop_op = "is", subprop_value = "EventTicketRefill")),
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                      start=ext_date_start, end=ext_date_end) %>% 
              rename(refill_ticket = value), by = "date") %>% 
  left_join(amplitude("AC", event="free_gift_received", measured_by="totals", 
                      where = list(list(subprop_type = "event", subprop_key = "gift_received", subprop_op = "is", subprop_value = "EventTicket")),
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                      start=ext_date_start, end=ext_date_end) %>% 
              rename(free_ticket = value), by = "date") %>% 
  left_join(amplitude("AC", event="CurrencySpent", measured_by="totals", 
                      where = list(list(subprop_type = "event", subprop_key = "purchase", subprop_op = "is", subprop_value = "KillBoost")),
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                      start=ext_date_start, end=ext_date_end) %>% 
              rename(kill_boost = value), by="date") %>% 
  left_join(.,event_players,by="date") 


ticket_data <- ticket_data %>% 
  group_by(group) %>% 
  filter(!is.na(group)) %>% 
  summarise(infinite_ticket = sum(infinite_ticket),
            refill_ticket = sum(refill_ticket),
            free_ticket = sum(free_ticket),
            kill_boost = sum(kill_boost),
            event_players = mean(event_players)) %>% 
  mutate(ave_infinite_tickets = infinite_ticket / event_players,
         ave_refill_tickets = refill_ticket / event_players,
         ave_free_tickets = free_ticket / event_players,
         ave_kill_boosts = kill_boost / event_players)

################## 7. Pass Rates ####

pass_rate <- amplitude("AC", event="event_level_finished", measured_by="totals", 
                       group_by_properties = list(list(type = "event", value = "levelName"), list(type = "event", value = "outcome")),
                       user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                       start=ext_date_start, end=ext_date_end) %>% 
  group_by(property, date) %>% 
  summarise(win_rate =  sum(value[property2 == "won"] / sum(value))) %>% 
  left_join(amplitude("AC", event="event_level_finished", measured_by="totals", 
                      where = list(list(subprop_type = "event", subprop_key = "hero2", subprop_op = "is", subprop_value = "(none)")),
                      group_by_properties = list(list(type = "event", value = "levelName"), list(type = "event", value = "outcome")),
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                      start=ext_date_start, end=ext_date_end)  %>% 
              group_by(property, date) %>% 
              summarise(win_rate_one_hero =  sum(value[property2 == "won"] / sum(value))), by=c("date","property")) %>% 
  left_join(amplitude("AC", event="event_level_finished", measured_by="totals", 
                      where = list(list(subprop_type = "event", subprop_key = "hero2", subprop_op = "is%20not", subprop_value = "(none)")),
                      group_by_properties = list(list(type = "event", value = "levelName"), list(type = "event", value = "outcome")),
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                      start=ext_date_start, end=ext_date_end) %>% 
              group_by(property, date) %>% 
              summarise(win_rate_two_hero =  sum(value[property2 == "won"] / sum(value))), by=c("date","property")) %>% 
  left_join(amplitude("AC", event="event_level_finished", measured_by="totals", 
                      where = list(list(subprop_type = "event", subprop_key = "airstrikesUsed", subprop_op = "is", subprop_value = "0"),
                                   list(subprop_type = "event", subprop_key = "cryobombs_used", subprop_op = "is", subprop_value = "0"),
                                   list(subprop_type = "event", subprop_key = "energyboosts_used", subprop_op = "is", subprop_value = "0"),
                                   list(subprop_type = "event", subprop_key = "reinforcesUsed", subprop_op = "is", subprop_value = "0"),
                                   list(subprop_type = "event", subprop_key = "secondChancesBought", subprop_op = "is", subprop_value = "0"),
                                   list(subprop_type = "event", subprop_key = "superTowerRefillsBought", subprop_op = "is", subprop_value = "0")),
                      group_by_properties = list(list(type = "event", value = "levelName"), list(type = "event", value = "outcome")),
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                      start=ext_date_start, end=ext_date_end) %>% 
              group_by(property, date) %>% 
              summarise(win_rate_no_boost = sum(value[property2 == "won"] / sum(value))), by=c("date","property")) %>% 
  mutate(group=ifelse(date >= event_0_date & date <= event_0_end, event_0, 
                      ifelse(date >= event_1_date & date <= event_1_end, event_1, NA))) %>% 
  rename(level = property) %>% 
  filter(!is.na(group)) %>% 
  group_by(group, level) %>% 
  summarise_at(vars(starts_with("win")), funs(mean))

# Plot pass_rate 

temp <- melt(pass_rate, id.vars = 1:2)

ggplot(temp,aes(level,value,fill=group)) + geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(.~variable) + 
  scale_fill_manual(values = c("dodgerblue3","green3"))+
  geom_text(aes(label=paste0(round(value,2)*100,"%")),vjust=1.9,position=position_dodge(.8), size=3.5, colour = "black")+
  ggtitle("Event Pass Rates")
rm(temp)

################## 8. Progression ####

progression_0 <- amplitude("AC", event="event_level_finished", measured_by="uniques", show_aggregate_over_date_range = T, 
                         where = list(list(subprop_type = "event", subprop_key = "reward_tier_reached", subprop_op = "greater%20or%20equal", subprop_value = c("0","(none)"))),
                         user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                         start=event_0_date, end=event_0_end) %>%
  rename(tier_0 = uniques_over_date_range) %>% 
  left_join(amplitude("AC", event="event_level_finished", measured_by="uniques", show_aggregate_over_date_range = T, 
                      where = list(list(subprop_type = "event", subprop_key = "reward_tier_reached", subprop_op = "greater%20or%20equal", subprop_value = c("1"))),
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                      start=event_0_date, end=event_0_end) %>% 
              rename(tier_1 = uniques_over_date_range) %>% 
              select(date,tier_1), by="date") %>%
  left_join(amplitude("AC", event="event_level_finished", measured_by="uniques", show_aggregate_over_date_range = T, 
                      where = list(list(subprop_type = "event", subprop_key = "reward_tier_reached", subprop_op = "greater%20or%20equal", subprop_value = c("2"))),
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                      start=event_0_date, end=event_0_end) %>% 
              rename(tier_2 = uniques_over_date_range) %>% 
              select(date,tier_2), by="date") %>% 
  left_join(amplitude("AC", event="event_level_finished", measured_by="uniques", show_aggregate_over_date_range = T, 
                      where = list(list(subprop_type = "event", subprop_key = "reward_tier_reached", subprop_op = "greater%20or%20equal", subprop_value = c("3"))),
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                      start=event_0_date, end=event_0_end) %>% 
              rename(tier_3 = uniques_over_date_range) %>% 
              select(date,tier_3), by="date") %>% 
  left_join(amplitude("AC", event="event_level_finished", measured_by="uniques", show_aggregate_over_date_range = T, 
                      where = list(list(subprop_type = "event", subprop_key = "reward_tier_reached", subprop_op = "greater%20or%20equal", subprop_value = c("4"))),
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                      start=event_0_date, end=event_0_end) %>% 
              rename(tier_4 = uniques_over_date_range) %>% 
              select(date,tier_4), by="date") %>% 
  left_join(amplitude("AC", event="event_level_finished", measured_by="uniques", show_aggregate_over_date_range = T, 
                      where = list(list(subprop_type = "event", subprop_key = "reward_tier_reached", subprop_op = "greater%20or%20equal", subprop_value = c("5"))),
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                      start=event_0_date, end=event_0_end) %>% 
              rename(tier_5 = uniques_over_date_range) %>% 
              select(date,tier_5), by="date") %>% 
  summarise("0" = mean(tier_0),
            "1" = mean(tier_1),
            "2" = mean(tier_2),
            "3" = mean(tier_3),
            "4" = mean(tier_4),
            "5" = mean(tier_5)) %>% 
  melt(.) %>% 
  rename(tier = variable,
         players_at_tier_event_0 = value) %>% 
  bind_rows(amplitude("AC", event="event_level_finished", measured_by="uniques", show_aggregate_over_date_range = T, 
                      where = list(list(subprop_type = "event", subprop_key = "reward_tier_reached", subprop_op = "greater%20or%20equal", subprop_value = c("6"))),
                      group_by_properties = list(list(type = "event", value = "reward_tier_reached")),
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                      start=event_0_date, end=event_0_end) %>% 
              rename(tier = property) %>% 
              group_by(tier) %>% 
              summarise(players_at_tier_event_0 = mean(uniques_over_date_range))) %>% 
  arrange(as.numeric(tier))
  


progression <- amplitude("AC", event="event_level_finished", measured_by="uniques", show_aggregate_over_date_range = T, 
                           where = list(list(subprop_type = "event", subprop_key = "reward_tier_reached", subprop_op = "greater%20or%20equal", subprop_value = c("0","(none)"))),
                           user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                           start=event_1_date, end=event_1_end) %>%
  rename(tier_0 = uniques_over_date_range) %>% 
  left_join(amplitude("AC", event="event_level_finished", measured_by="uniques", show_aggregate_over_date_range = T, 
                      where = list(list(subprop_type = "event", subprop_key = "reward_tier_reached", subprop_op = "greater%20or%20equal", subprop_value = c("1"))),
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                      start=event_1_date, end=event_1_end) %>% 
              rename(tier_1 = uniques_over_date_range) %>% 
              select(date,tier_1), by="date") %>%
  left_join(amplitude("AC", event="event_level_finished", measured_by="uniques", show_aggregate_over_date_range = T, 
                      where = list(list(subprop_type = "event", subprop_key = "reward_tier_reached", subprop_op = "greater%20or%20equal", subprop_value = c("2"))),
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                      start=event_1_date, end=event_1_end) %>% 
              rename(tier_2 = uniques_over_date_range) %>% 
              select(date,tier_2), by="date") %>% 
  left_join(amplitude("AC", event="event_level_finished", measured_by="uniques", show_aggregate_over_date_range = T, 
                      where = list(list(subprop_type = "event", subprop_key = "reward_tier_reached", subprop_op = "greater%20or%20equal", subprop_value = c("3"))),
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                      start=event_1_date, end=event_1_end) %>% 
              rename(tier_3 = uniques_over_date_range) %>% 
              select(date,tier_3), by="date") %>% 
  left_join(amplitude("AC", event="event_level_finished", measured_by="uniques", show_aggregate_over_date_range = T, 
                      where = list(list(subprop_type = "event", subprop_key = "reward_tier_reached", subprop_op = "greater%20or%20equal", subprop_value = c("4"))),
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                      start=event_1_date, end=event_1_end) %>% 
              rename(tier_4 = uniques_over_date_range) %>% 
              select(date,tier_4), by="date") %>% 
  left_join(amplitude("AC", event="event_level_finished", measured_by="uniques", show_aggregate_over_date_range = T, 
                      where = list(list(subprop_type = "event", subprop_key = "reward_tier_reached", subprop_op = "greater%20or%20equal", subprop_value = c("5"))),
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                      start=event_1_date, end=event_1_end) %>% 
              rename(tier_5 = uniques_over_date_range) %>% 
              select(date,tier_5), by="date") %>% 
  summarise("0" = mean(tier_0),
            "1" = mean(tier_1),
            "2" = mean(tier_2),
            "3" = mean(tier_3),
            "4" = mean(tier_4),
            "5" = mean(tier_5)) %>% 
  melt(.) %>% 
  rename(tier = variable,
         players_at_tier_event_1 = value) %>% 
  bind_rows(amplitude("AC", event="event_level_finished", measured_by="uniques", show_aggregate_over_date_range = T, 
                      where = list(list(subprop_type = "event", subprop_key = "reward_tier_reached", subprop_op = "greater%20or%20equal", subprop_value = c("6"))),
                      group_by_properties = list(list(type = "event", value = "reward_tier_reached")),
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                      start=event_1_date, end=event_1_end) %>% 
              rename(tier = property) %>% 
              group_by(tier) %>% 
              summarise(players_at_tier_event_1 = mean(uniques_over_date_range))) %>% 
  arrange(as.numeric(tier)) %>% 
  left_join(., progression_0, by="tier") %>% 
  mutate(ev_0 = players_at_tier_event_0 / max(players_at_tier_event_0),
            ev_1 = players_at_tier_event_1 / max(players_at_tier_event_1)) %>% 
  select(tier, ev_0, ev_1) %>% 
  melt(.)

rm(progression_0)


progression$variable <- factor(progression$variable, levels = c("ev_1", "ev_0"), labels=c(event_1,event_0))

ggplot(progression, aes(as.numeric(tier), value, fill=variable))+
  geom_bar(position="dodge", stat = "identity")+
  ggtitle("Reward Tier Progression")+
  scale_fill_manual(values = c("dodgerblue3","green3"))+
  geom_text(aes(label=paste0(round(value,3)*100,"%")),vjust=-0.25,position=position_dodge(.9), size=3.5, colour = "black")+
  scale_x_continuous(breaks=1* 0:max(as.numeric(progression$tier)))


################## 9. Average Tier Reached ####


avg_tier <- amplitude("AC", event="event_level_finished", measured_by="sums", 
                      group_by_properties = list(list(type = "event", value = "reward_tier_reached")),
                      user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                      start=ext_date_start, end=ext_date_end) %>% 
  rename(prop_sum = value) %>% 
  left_join(.,amplitude("AC", event="event_level_finished", measured_by="totals", 
                        user_segment=list(list(prop="userdata_cohort", op="is%20not", values=list(hackers_ids))),
                        start=ext_date_start, end=ext_date_end) %>% 
              rename(attempts = value),by="date") %>% 
  mutate(avg_tier_reached = prop_sum / attempts,
         group=ifelse(date >= event_0_date & date <= event_0_end, event_0, 
                      ifelse(date >= event_1_date & date <= event_1_end, event_1, NA))) %>% 
  filter(!is.na(group)) %>% 
  select(date, group, avg_tier_reached) %>% 
  group_by(group) %>% 
  mutate(event_day = date + 1 - min(date))


ggplot(avg_tier, aes(event_day, avg_tier_reached, colour = group)) + 
  geom_line(size = 1) +
  scale_x_continuous(breaks=1* 1:max(avg_tier$event_day))+
  ggtitle("Average Reward Tier Reached by Event Day")+
  geom_text(aes(label=round(avg_tier_reached,1),vjust=-0.5))

################## 10. Stiching General Performance Metrics ####

performance_metrics <- metrics %>% 
  filter(!is.na(group)) %>% 
  group_by(group) %>% 
  summarise(ave_dau = round(mean(dau),0),
            ave_arpdau = mean(arpdau),
            ave_conversion_rate = mean(conversion_rate),
            total_players = mean(total_players),
            revenue = sum(revenue),
            spenders = mean(unique_spenders),
            ave_event_dau = round(mean(event_dau),0),
            event_players = mean(event_players)) %>% 
  mutate(arpu = revenue / total_players,
         event_player_arpu = revenue / event_players) %>% 
  left_join(., ticket_data, by="group") %>% 
  left_join(., (gem_event %>% 
                  filter(!is.na(group)) %>% 
                  group_by(group) %>% 
                  summarise(out_of_event_gem_investment = mean(out_of_event_gem_investment),
                            event_gem_investment = mean(event_gem_investment),
                            all_gem_investment = mean(all_gem_investment))), by="group")
  
################## 11. Stiching Event Level Metrics ####

level_metrics <- level_players %>% 
  group_by(level, group) %>% 
  summarise(players = mean(event_players)) %>% 
  left_join(.,(event_players %>% 
                 group_by(group) %>% 
                 summarise(total_players = mean(event_players))), by="group") %>% 
  mutate(player_percentage = players / total_players) %>% 
  left_join(., pass_rate, by=c("group", "level")) %>% 
  left_join(., gem_level, by=c("group", "level")) %>% 
  arrange(group)

rm(avg_tier, dau, event_players, gem_data, gem_event, gem_level, level_players, pass_rate, metrics, progression, ticket_data)


################################################################################################################## 12. Results ####

dir.create(paste0("D:\\R Working File\\1. ACTD\\",Sys.Date()," - ACTD - ", sale_identifier))
setwd(paste0("D:\\R Working File\\1. ACTD\\",Sys.Date()," - ACTD - ", sale_identifier))

write.csv(performance_metrics, "performance_metrics.csv")
write.csv(level_metrics, "level_metrics.csv")






















  