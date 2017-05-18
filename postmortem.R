
########################################################
####### Get Revenue over a range of days ###############
########################################################


start_date = "2017-04-28"
end_date = "2017-05-14"

week_before_start = "2017-04-28"
week_before_end = "2017-04-30"

sale_start = "2017-05-05"
sale_end = "2017-05-07"

week_after_start = "2017-05-12"
week_after_end = "2017-05-14"

source('~/amplitude-dashboard/dashboard-code/SharedConfig.R')

########################################################
###################### Main KPI ########################
########################################################


###### Revenue ######

temp <- amplitude("CT", event="unverified_revenue",
                  group_by_properties=list(list(type="event", value="$revenue"), list(type="event", value="$revenueType")),
                  measured_by="sums", start=start_date, end=end_date)

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
                    measured_by="uniques", start=start_date, end=end_date)

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
                     measured_by="uniques", start=start_date, end=end_date)

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

total_reinforcements <- amplitude("CT", event="level_account", group_by_properties = list(list(type = "event", value = "sentinel_purchased")),
                                  measured_by="sums", start=start_date, end=end_date)

