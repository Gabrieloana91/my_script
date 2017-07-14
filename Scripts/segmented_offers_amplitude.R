### ### ### ### ### ### ### ###

# Segmented Offers Amplitude Script

### ### ### ### ### ### ### ###
 
source('~/amplitude-dashboard/dashboard-code/SharedConfig.R')

start_date_1 <- "2017-06-05"
end_date_1 <- "2017-06-12"

start_date_2 <- "2017-07-05"
end_date_2 <- "2017-07-12"

#### 1. Collecting Players ####

players_1 <- amplitude("AC", event="_active", measured_by="uniques", show_aggregate_over_date_range=T,
                       start=start_date_1, end=end_date_1) %>% 
  bind_rows(amplitude("AC", event="_active", measured_by="uniques", show_aggregate_over_date_range=T,
                      start=start_date_2, end=end_date_2)) %>% 
  select(date, uniques_over_date_range) %>% 
  rename(total_players = uniques_over_date_range)


# Spenders # 

unique_spenders <- amplitude("AC", event="unverified_revenue", measured_by="uniques", show_aggregate_over_date_range = T, 
                             start=start_date_1, end=end_date_1) %>% 
  bind_rows(amplitude("AC", event="unverified_revenue", measured_by="uniques", show_aggregate_over_date_range = T, 
                      start=start_date_2, end=end_date_2)) %>% 
  rename(unique_spenders = uniques_over_date_range) %>% 
  select(date, unique_spenders)


#### 2. Revenue ####


revenue <- amplitude("AC", event="unverified_revenue", measured_by="sums",
                     group_by_properties=list(list(type="event", value="$revenue"), list(type="event", value="$revenueType")),
                     start=start_date_1, end=end_date_2) %>% filter(property != "(none)") %>% 
  rename(code = property,
         dt = date)

revenue <- make_currency_values_USD(revenue, "USD", "code", "value")

revenue <- revenue %>% 
  rename(date = dt) %>% 
  group_by(date) %>% 
  summarise(revenue = sum(revenue,na.rm=T)) 

dau <- amplitude("AC", event="_active", measured_by="uniques",
                 start=start_date_1, end=end_date_2) %>% 
  rename(dau = value) 

metrics <- join_all(list(players_1, revenue, dau, unique_spenders), by="date")

rm(players_1, unique_spenders)

metrics2 <- left_join(revenue, dau, by="date") %>% 
  mutate(arpdau = revenue / dau) %>% 
  mutate(group = ifelse(date >= start_date_1 & date <= end_date_1, "Normal Sales", 
                        ifelse(date >= start_date_2 & date <= end_date_2, "Segmented Offers", NA)))

ggplot(metrics2, aes(date,arpdau)) + geom_line(size = 1, colour = "green4") + geom_smooth(span = 0.4, method = "loess")+
  geom_vline(xintercept = as.numeric(metrics2$date[metrics2$date == start_date_1]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(start_date_1) + 3, y=max(metrics2$arpdau),label = "Normal Sales", colour = "dodgerblue4")+
  geom_vline(xintercept = as.numeric(metrics2$date[metrics2$date == end_date_1]), linetype = "dashed", colour = "coral4") +
  geom_vline(xintercept = as.numeric(metrics2$date[metrics2$date == start_date_2]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(start_date_2) + 3, y=max(metrics2$arpdau),label = "Segmented Offers", colour = "dodgerblue4")+
  geom_vline(xintercept = as.numeric(metrics2$date[metrics2$date == end_date_2]), linetype = "dashed", colour = "coral4") +
  scale_x_date(limits = c(min(metrics2$date), Sys.Date()-1)) +
  ggtitle("ARPDAU (net USD)")


# Conversion Rate # 

conv_rate <- amplitude("AC", event="unverified_revenue", measured_by="uniques", 
                       start=start_date_1, end=end_date_2) %>% 
  rename(spenders = value) %>% 
  left_join(., dau, by="date") %>% 
  group_by(date) %>% 
  summarise(conversion_rate = spenders / dau)

ggplot(conv_rate, aes(date,conversion_rate)) + geom_line(size = 1, colour = "green4") + geom_smooth(span = 0.4, method = "loess")+
  geom_vline(xintercept = as.numeric(conv_rate$date[conv_rate$date == start_date_1]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(start_date_1) + 3, y=max(conv_rate$conversion_rate),label = "Normal Sales", colour = "dodgerblue4")+
  geom_vline(xintercept = as.numeric(conv_rate$date[conv_rate$date == end_date_1]), linetype = "dashed", colour = "coral4") +
  geom_vline(xintercept = as.numeric(conv_rate$date[conv_rate$date == start_date_2]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(start_date_2) + 3, y=max(conv_rate$conversion_rate),label = "Segmented Offers", colour = "dodgerblue4")+
  geom_vline(xintercept = as.numeric(conv_rate$date[conv_rate$date == end_date_2]), linetype = "dashed", colour = "coral4") +
  scale_x_date(date_breaks = "7 day")+
  ggtitle("Conversion Rate")



# ARPPU

arppu <- amplitude("AC", event="unverified_revenue", measured_by="uniques", show_aggregate_over_date_range = T, 
                     start=start_date_1, end=end_date_2) %>% 
  rename(spender = value) %>% 
  select(date, spender) %>% 
  left_join(.,revenue, by = "date") %>% 
  group_by(date) %>% 
  summarise(arppu = revenue / spender)

ggplot(arppu, aes(date,arppu)) + geom_line(size = 1, colour = "green4") + 
  geom_smooth(span = 0.4, method = "loess")+
  geom_vline(xintercept = as.numeric(arppu$date[arppu$date == start_date_1]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(start_date_1) + 3, y=max(arppu$arppu),label = "Normal Sales", colour = "dodgerblue4")+
  geom_vline(xintercept = as.numeric(arppu$date[arppu$date == end_date_1]), linetype = "dashed", colour = "coral4") +
  geom_vline(xintercept = as.numeric(arppu$date[arppu$date == start_date_2]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(start_date_2) + 3, y=max(arppu$arppu),label = "Segmented Offers", colour = "dodgerblue4")+
  geom_vline(xintercept = as.numeric(arppu$date[arppu$date == end_date_2]), linetype = "dashed", colour = "coral4") +
  scale_x_date(date_breaks = "7 day")+
  ggtitle("ARPPU")


# Revenue 

ggplot(revenue, aes(date,revenue)) + geom_line(size = 1, colour = "green4") + 
  # geom_smooth(span = 0.4, method = "loess")+
  geom_vline(xintercept = as.numeric(revenue$date[revenue$date == start_date_1]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(start_date_1) + 3, y=max(revenue$revenue),label = "Normal Sales", colour = "dodgerblue4")+
  geom_vline(xintercept = as.numeric(revenue$date[revenue$date == end_date_1]), linetype = "dashed", colour = "coral4") +
  geom_vline(xintercept = as.numeric(revenue$date[revenue$date == start_date_2]), linetype = "dashed", colour = "coral4") +
  annotate("text",x = as.Date(start_date_2) + 3, y=max(revenue$revenue),label = "Segmented Offers", colour = "dodgerblue4")+
  geom_vline(xintercept = as.numeric(revenue$date[revenue$date == end_date_2]), linetype = "dashed", colour = "coral4") +
  scale_x_date(date_breaks = "7 day")+
  ggtitle("Net Revenue")


# Stiching metrics together 

metrics3 <- metrics %>% 
  left_join(., conv_rate, by="date") %>% 
  left_join(., arppu, by="date") %>% 
  mutate(arpdau = revenue / dau) %>% 
  mutate(group = ifelse(date >= start_date_1 & date <= end_date_1, "Normal Sales", 
                        ifelse(date >= start_date_2 & date <= end_date_2, "Segmented Offers", NA))) %>% 
  group_by(group) %>% 
  summarise(total_players = mean(total_players),
            total_revenue = sum(revenue),
            ave_dau = mean(dau),
            spenders = mean(unique_spenders),
            conversion_rate = mean(conversion_rate),
            arppu = mean(arppu),
            arpdau = mean(arpdau))










# Revenue by product_id


revenue_id <- amplitude("AC", event="unverified_revenue", measured_by="sums",
                        group_by_properties=list(list(type="event", value="$revenue"), list(type="event", value="$revenueType"), list(type="event", value="$productId")),
                        start=start_date_1, end=end_date_1) %>% filter(property != "(none)") %>% 
  rename(dt = date) %>% 
  bind_rows(amplitude("AC", event="unverified_revenue", measured_by="sums",
                      group_by_properties=list(list(type="event", value="$revenue"), list(type="event", value="$revenueType"), list(type="event", value="$productId")),
                      start=start_date_2, end=end_date_2) %>% filter(property != "(none)") %>% 
              rename(dt = date)) %>% 
  separate(., col = property, into=c("code","product_id"), sep="; ") %>% 
  make_currency_values_USD(., "USD","code","value") %>% 
  group_by(dt, product_id) %>% 
  summarise(revenue = sum(revenue, na.rm = T)) %>% 
  mutate(group = ifelse(dt >= start_date_1 & dt <= end_date_1, "Normal Sales", 
                        ifelse(dt >= start_date_2 & dt <= end_date_2, "Segmented Offers", NA))) %>% 
  filter(revenue > 0) %>% 
  group_by(group, product_id) %>% 
  summarise(revenue = sum(revenue)) %>% 
  filter(revenue > 15) %>% 
  mutate(sale_group = ifelse(product_id %in% c("GemPack1","GemPack2","GemPack3","GemPack4","GemPack5", "HeroElectro","HeroRoxie","HeroMech","HeroPsyker","HeroSkyBolt", "StarterPack1"), "Shop Offer", 
                             ifelse(product_id %in% c("starterPack1", "starterPack2", "starterPack3", "starterPack4"), "Starter Pack Bundle",
                                    ifelse(product_id %in% c("surgePack1", "surgePack2", "skyboltPack1", "skyboltPack2", "oraclePack1", "oraclePack2", "cinderPack1", "cinderPack2"), "Hero Bundle", 
                                           ifelse(product_id %in% c("purchaseBreak1", "purchaseBreak2","purchaseBreak3","purchaseBreak4", "purchaseBreak5"), "Purchase Break Bundle",
                                                  ifelse(product_id %in% c("area2Pack01", "area3Pack01", "area4Pack01", "area5Pack01"), "Area Bundle",
                                                         ifelse(product_id %in% c("lapsedPlayer1", "lapsedPlayer2", "lapsedPlayer3", "lapsedPlayer4", "lapsedPlayer5", "lapsedPlayer6", "lapsedPlayer7", "lapsedPlayer8"), "Lapsed Player", 
                                                                ifelse(product_id %in% c("outOfGems1", "outOfGems2"), "Out Of Gems Bundle",
                                                                       ifelse(product_id %in% c("levelFailsBundle"), "Consecutive Fails Bundle",
                                                                              ifelse(product_id %in% c("eventBundle1", "eventBundle2"), "Event Bundle", NA))))))))))









ggplot(revenue_id, aes(product_id, revenue))+geom_bar(stat="identity", fill = "darkgreen") +
  coord_flip()+
  facet_wrap(~group)+
  ggtitle("Total Net Revenue by Product Id")


shop_offer <- revenue_id %>% 
  filter(sale_group == "Shop Offer")

ggplot(shop_offer, aes(product_id, revenue, fill=group))+geom_bar(stat="identity", position = "dodge") +
  scale_fill_manual(values = c("green3","dodgerblue3"))+
  ggtitle("Total Net Revenue by Product Id (Shop Offers)")
  








