########################################################
############## Sale Post-mortem Function ###############
########################################################


### Inputs ###

sale_start = "2017-05-05"
sale_end = "2017-05-07"

sale_identifier = "Reinforcements"



### Parameters ###

week_before_start = as.Date(sale_start)-7
week_before_end = as.Date(sale_end)-7

week_after_start = as.Date(sale_start)+7
week_after_end = as.Date(sale_end)+7

source('~/amplitude-dashboard/dashboard-code/SharedConfig.R')

########################################################
###################### Main KPI ########################
########################################################


###### Revenue ######

temp <- amplitude("CT", event="unverified_revenue",
                  group_by_properties=list(list(type="event", value="$revenue"), list(type="event", value="$revenueType")),
                  measured_by="sums", start=week_before_start, end=week_after_end)

names(temp)[names(temp)=="property"] <- "code"

temp <- temp %>% filter(code != "(none)")

conv <- get_exchange_rates(unique(temp$code), toCurr="USD", fromDt=min(temp$date), toDt=max(temp$date))

revenue <- inner_join(temp, conv, by=c("code", "date"))
revenue$revenue <- revenue$value * revenue$exch 

revenue$group <- ifelse(revenue$date >= week_before_start & revenue$date <= week_before_end,"week_before",
                        ifelse(revenue$date >= sale_start & revenue$date <= sale_end, "sale_period",
                               ifelse(revenue$date >= week_after_start & revenue$date <= week_after_end, "week_after", NA)))

revenue <- revenue %>% 
  filter(group != "NA") %>% 
  group_by(group) %>% 
  summarise(total_gross_revenue = sum(revenue,na.rm = T)) 



rm(conv,temp)


##### Players #####


player <- amplitude("CT", event="_active",
                    measured_by="uniques", start=week_before_start, end=week_after_end)

player$group <- ifelse(player$date >= week_before_start & player$date <= week_before_end,"week_before",
                       ifelse(player$date >= sale_start & player$date <= sale_end, "sale_period",
                              ifelse(player$date >= week_after_start & player$date <= week_after_end, "week_after", NA)))

player <- player %>% 
  filter(group != "NA") %>% 
  group_by(group) %>% 
  summarise(total_players = sum(value,na.rm=T))

metrics <- left_join(revenue,player,by="group")

rm(revenue,player)

##### Average DAU #####

time_difference = as.numeric(as.POSIXlt(sale_end) - as.POSIXlt(sale_start)) + 1

metrics <- metrics %>% 
  group_by(group) %>% 
  mutate(ave_DAU = total_players / time_difference,
         ARPU = total_gross_revenue/total_players)

##### Average ARPDAU #####

metrics <- metrics %>% 
  group_by(group) %>% 
  mutate(ave_ARPDAU = total_gross_revenue / ave_DAU)

#### Total Spenders #####

spender <- amplitude("CT", event="unverified_revenue",
                     measured_by="uniques", start=week_before_start, end=week_after_end)

spender$group <- ifelse(spender$date >= week_before_start & spender$date <= week_before_end,"week_before",
                        ifelse(spender$date >= sale_start & spender$date <= sale_end, "sale_period",
                               ifelse(spender$date >= week_after_start & spender$date <= week_after_end, "week_after", NA)))

spender <- spender %>% 
  filter(group != "NA") %>% 
  group_by(group) %>% 
  summarise(total_spenders = sum(value,na.rm=T))


metrics <- metrics %>% 
  left_join(spender,by="group") %>% 
  mutate(percent_purchasing_players = total_spenders / total_players)

rm(spender)


################################################################
##################### Gem Investment ###########################
################################################################

# 1. Sentinel # 

sentinel <- amplitude("CT", event="level_account", group_by_properties = list(list(type = "event", value = "sentinel_purchased")),
                      measured_by="totals", start=week_before_start, end=week_after_end)
sentinel$property <- as.numeric(sentinel$property)

sentinel$group <- ifelse(sentinel$date >= week_before_start & sentinel$date <= week_before_end,"week_before",
                         ifelse(sentinel$date >= sale_start & sentinel$date <= sale_end, "sale_period",
                                ifelse(sentinel$date >= week_after_start & sentinel$date <= week_after_end, "week_after", NA)))
sentinel <- sentinel %>% 
  mutate(prop_sum = value * property) %>% 
  group_by(group) %>% 
  summarise(sentinel_purchased = sum(prop_sum,na.rm = T)*8) %>% 
  filter(group != "NA")

# 2. Elixir #


elixir <- amplitude("CT", event="level_account", group_by_properties = list(list(type = "event", value = "energyboost_used")),
                    measured_by="totals", start=week_before_start, end=week_after_end)
elixir$property <- as.numeric(elixir$property)

elixir$group <- ifelse(elixir$date >= week_before_start & elixir$date <= week_before_end,"week_before",
                       ifelse(elixir$date >= sale_start & elixir$date <= sale_end, "sale_period",
                              ifelse(elixir$date >= week_after_start & elixir$date <= week_after_end, "week_after", NA)))
elixir <- elixir %>% 
  mutate(prop_sum = value * property) %>% 
  group_by(group) %>% 
  summarise(elixir_used = sum(prop_sum,na.rm = T) * 24) %>% 
  filter(group != "NA")

# 3. Reinforcements


reinforcements <- amplitude("CT", event="level_account", group_by_properties = list(list(type = "event", value = "reinforcements_used")),
                            measured_by="totals", start=week_before_start, end=week_after_end)
reinforcements$property <- as.numeric(reinforcements$property)

reinforcements$group <- ifelse(reinforcements$date >= week_before_start & reinforcements$date <= week_before_end,"week_before",
                               ifelse(reinforcements$date >= sale_start & reinforcements$date <= sale_end, "sale_period",
                                      ifelse(reinforcements$date >= week_after_start & reinforcements$date <= week_after_end, "week_after", NA)))
reinforcements <- reinforcements %>% 
  mutate(prop_sum = value * property) %>% 
  group_by(group) %>% 
  summarise(reinforcements_used = sum(prop_sum,na.rm = T)*8) %>% 
  filter(group != "NA")


# 4. Play-On # 


play_on_not <- amplitude("CT", event="level_account", group_by_properties = list(list(type = "event", value = "play_on_offered")),
                         where = list(list(subprop_type = "event", subprop_key = "play_on_offered",subprop_op = "greater", subprop_value=list("0")),
                                      list(subprop_type = "event", subprop_key = "outcome", subprop_op = "is", subprop_value=list("loss"))),
                         measured_by="totals", start=week_before_start, end=week_after_end)



play_on_not$property <- as.numeric(play_on_not$property)
play_on_not$group <- ifelse(play_on_not$date >= week_before_start & play_on_not$date <= week_before_end,"week_before",
                            ifelse(play_on_not$date >= sale_start & play_on_not$date <= sale_end, "sale_period",
                                   ifelse(play_on_not$date >= week_after_start & play_on_not$date <= week_after_end, "week_after", NA)))
play_on_not <- play_on_not %>% 
  group_by(group) %>% 
  filter(group != "NA") %>% 
  summarise(totals = sum(value,na.rm=T)) 


play_on <- amplitude("CT", event="level_account", group_by_properties = list(list(type = "event", value = "play_on_offered")),
                     measured_by="totals", start=week_before_start, end=week_after_end)
play_on$property <- as.numeric(play_on$property)
play_on$group <- ifelse(play_on$date >= week_before_start & play_on$date <= week_before_end,"week_before",
                        ifelse(play_on$date >= sale_start & play_on$date <= sale_end, "sale_period",
                               ifelse(play_on$date >= week_after_start & play_on$date <= week_after_end, "week_after", NA)))

play_on <- play_on %>% 
  filter(group != "NA") %>% 
  mutate(prop_sum = value * property) %>% 
  group_by(group) %>% 
  summarise(play_on_used = sum(prop_sum,na.rm = T)) 



play_on <- left_join(play_on,play_on_not,by="group")
play_on$play_on <- (play_on$play_on_used - play_on$totals)*60
play_on <- play_on[,c(1,4)]

# Total gems invested #

total_gems <- left_join(left_join(left_join(left_join(sentinel,reinforcements,by="group"),elixir,by="group"),play_on,by="group"),metrics, by="group")

total_gems <- total_gems %>% 
  group_by(group) %>% 
  mutate(ave_gems_investment_per_player = (sentinel_purchased + reinforcements_used + elixir_used + play_on)/total_players)

rm(elixir,play_on, play_on_not, reinforcements, sentinel)

# Average Gem investment per player # 

ave_gem_investment <- total_gems %>% 
  group_by(group) %>% 
  summarise(ave_sentinel = sentinel_purchased / total_players,
            ave_reinforcements = reinforcements_used / total_players,
            ave_elixir = elixir_used / total_players,
            ave_play_on = play_on / total_players)
rm(total_gems)  

metrics <- left_join(metrics, ave_gem_investment, by="group")

rm(ave_gem_investment)




################################################################
########################### Active % ###########################
################################################################



active <- amplitude("CT", event="currency_spent", group_by_properties = list(list(type = "event", value = "purchase")),
                    measured_by="pct_dau", start=week_before_start, end=week_after_end)

active$group <- ifelse(active$date >= week_before_start & active$date <= week_before_end,"week_before",
                       ifelse(active$date >= sale_start & active$date <= sale_end, "sale_period",
                              ifelse(active$date >= week_after_start & active$date <= week_after_end, "week_after", NA)))

active <- active %>% 
  group_by(group,property) %>% 
  filter(group != "NA") %>% 
  summarise(totals = mean(value,na.rm=T)) 


################################################################
##### Average purchases per user purchasing at least once ######
################################################################

ave_purchases_totals <- amplitude("CT", event="currency_spent", group_by_properties = list(list(type = "event", value = "purchase")),
                                  measured_by="totals", start=week_before_start, end=week_after_end)

ave_purchases_uniques <- amplitude("CT", event="currency_spent",
                                   measured_by="uniques", start=week_before_start, end=week_after_end) %>% 
  select(value,date)

names(ave_purchases_uniques)[names(ave_purchases_uniques)=="value"] <- "uniques"
ave_purchases <- left_join(ave_purchases_totals,ave_purchases_uniques, by="date")

ave_purchases$group <- ifelse(ave_purchases$date >= week_before_start & ave_purchases$date <= week_before_end,"week_before",
                              ifelse(ave_purchases$date >= sale_start & ave_purchases$date <= sale_end, "sale_period",
                                     ifelse(ave_purchases$date >= week_after_start & ave_purchases$date <= week_after_end, "week_after", NA)))


ave_purchases <- ave_purchases %>% 
  group_by(group,property) %>% 
  filter(group != "NA") %>% 
  summarise(ave_purchases = mean(value)/mean(uniques))

rm(ave_purchases_totals,ave_purchases_uniques)


###############################################################
####################### Graphs ################################
###############################################################

temp <- melt(metrics,id.vars = 1) 

ggplot(temp, aes(group, value))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(.~ variable, scales = "free")





####################################
## Export ##
####################################


write.csv(metrics, paste0("CT_Sale_Report_", sale_identifier,"_1", ".csv"))
write.csv(ave_purchases, paste0("CT_Sale_Report_", sale_identifier,"_2" , ".csv"))
write.csv(active, paste0("CT_Sale_Report_", sale_identifier,"_3", ".csv"))


