### ### ### ### ### ### ### ### ### ###
 
### A/B Experiment Analysis Template

### ### ### ### ### ### ### ### ### ###


#### 0. Data Loading ####


start_dt <- "2017-06-15"
end_dt <- "2017-07-20"
game <- "AC"
cut_off_day <- 14

events <- c("unverified_revenue", "level_finished", "currencyspent", "session_account", "currency_converted", "event_level_finished")

properties <- c("e_currency_code", "e_currency_value", "e_purchase", "u_fb_user_id", "e_outcome", "e_level_id", "u_currentgold", "u_currentcredits", "e__productid", 
                "e_livesleft","e_hero", "e_hero2", "e_herolevel", "e_herolevel2", "e_levelname", "e_starsearned", "u_totalstars",
                "e_cryobombs_used", "e_reinforcesused", "e_supertoweruses", "e_airstrikesused", "e_energyboosts_used",
                "e_difficulty", "u_experiment_groups", "u_experiment_names","e_gem_spent", "e_gold_spent", "e_credit_spent", "e_coins_added", "e_gems_spent",
                "e_creditsspent", "e_purchase", "e_context", "u_email", "u_store", "u_furthestlevel",
                "e_event_identifier", "e_levelname", "e_event_grandreward")

first_events <- c("first_launch", "adjust_install", "session_account")

df <- query_cohort_events(game,start_dt,end_dt,cut_off_day+1,first_events,events,properties)

df$e_gem_spent[is.na(df$e_gem_spent)] <- 0
df$e_gold_spent[is.na(df$e_gold_spent)] <- 0
df$e_gems_spent[is.na(df$e_gems_spent)] <- 0



# z <- df # Backup


#### 1. Data Cleaning + Analysis #

df1 <- df %>% 
  make_currency_values_USD("USD","e_currency_code","e_currency_value") %>% 
  remove_players_in_multiple_test_groups("u_experiment_groups") %>% 
  mutate(current_gem = as.numeric(u_currentgold),
         current_coin = as.numeric(u_currentcredits),
         gem_investment = coalesce(as.numeric(e_cryobombs_used),0) * 4 + coalesce(as.numeric(e_reinforcesused), 0) * 6 + coalesce(as.numeric(e_supertoweruses),0) * 6 + 
           coalesce(as.numeric(e_airstrikesused),0) * 2 + coalesce(as.numeric(e_energyboosts_used),0) * 10, ### as soon as 1 is NA all are
         gem_spent = coalesce(as.numeric(e_gem_spent),0) + coalesce(as.numeric(e_gold_spent),0) + coalesce(as.numeric(e_gems_spent), 0)) %>%
  group_by(amplitude_id) %>% 
  mutate(experiment_group = head(u_experiment_groups[!is.na(u_experiment_groups)],1),
         u_experiment_names = head(u_experiment_names[!is.na(u_experiment_names)],1),
         hacker =  max(c(0, current_gem), na.rm = T) > 99999 | 
           max(c(0, current_coin), na.rm = T) > 999999 | grepl("@tfbnw|@outplay.com", u_email) | min(c(0,gem_spent)) < 0) %>% 
  filter(grepl("InfiniteTicket",u_experiment_names) & !hacker) %>% 
  select(-u_currentgold, - u_currentcredits, -e_cryobombs_used, -e_reinforcesused, -e_supertoweruses, -e_airstrikesused,
         -e_energyboosts_used) %>% 
  as.data.table()


df1$e_level_id <- na.locf(df1$e_level_id)
df1$e_credit_spent <- as.numeric(df1$e_credit_spent)
df1 <- df1[!duplicated(df1)]

gc()


df1 %>% make_bootstrap_graph(grouping_name = "experiment_group", day_x = 7)
df1 %>% make_bootstrap_graph("retention",grouping_name = "experiment_group", day_x = 7)


res <- df1 %>% as.data.frame() %>% 
  # filter(country_code %in% c("United States","united Kingdom","Germany","France","Australia")) %>%
  filter(day <= cut_off_day) %>% 
  group_by(amplitude_id) %>% 
  mutate(purchases = sum(revenue >0, na.rm = T)) %>% 
  group_by(experiment_group) %>% 
  summarise(users = length(unique(amplitude_id)),
            purchasers = length(unique(amplitude_id[revenue > 0])),
            total_revenue = sum(revenue, na.rm =T),
            arpu = total_revenue / users,
            arppu = total_revenue / purchasers,
            conversion = purchasers / users,
            repeat_purchaser_ratio = length(unique(amplitude_id[purchases > 1])) / purchasers,
            purchases_per_purchaser = sum(revenue > 0) / purchasers,
            fb_login_rate = length(unique(amplitude_id[!is.na(u_fb_user_id)])) / users,
            ave_revenue_per_purchase = total_revenue / sum(revenue > 0),
            day1retention = length(unique(amplitude_id[day == 1])) / users,
            day7retention = length(unique(amplitude_id[day == 7])) / users,
            day7retention_unbounded = length(unique(amplitude_id[day >= 7])) / users,
            gem_investment_per_user = sum(gem_investment, na.rm = T) / users,
            gem_investment_per_level = sum(gem_investment, na.rm = T) / sum(event_type == "Level Finished"),
            gems_spent_per_user = sum(as.numeric(gem_spent)) / length(unique(amplitude_id))) %>% 
  as.data.table()

res1 <- df1 %>% as.data.frame() %>% 
  # filter(country_code %in% c("United States","united Kingdom","Germany","France","Australia")) %>%
  remove_top_percent_of_spenders("experiment_group",0.98) %>% 
  filter(day <= cut_off_day) %>% 
  group_by(amplitude_id) %>% 
  mutate(purchases = sum(revenue >0, na.rm = T)) %>% 
  group_by(experiment_group) %>% 
  summarise(users = length(unique(amplitude_id)),
            purchasers = length(unique(amplitude_id[revenue > 0])),
            total_revenue = sum(revenue, na.rm =T),
            arpu = total_revenue / users,
            arppu = total_revenue / purchasers,
            conversion = purchasers / users,
            repeat_purchaser_ratio = length(unique(amplitude_id[purchases > 1])) / purchasers,
            purchases_per_purchaser = sum(revenue > 0) / purchasers,
            fb_login_rate = length(unique(amplitude_id[!is.na(u_email)])) / users,
            ave_revenue_per_purchase = total_revenue / sum(revenue > 0),
            day1retention = length(unique(amplitude_id[day == 1])) / users,
            day7retention = length(unique(amplitude_id[day == 7])) / users,
            day7retention_unbounded = length(unique(amplitude_id[day >= 7])) / users,
            gem_investment_per_user = sum(gem_investment, na.rm = T) / users,
            gem_investment_per_level = sum(gem_investment, na.rm = T) / sum(event_type == "Level Finished"),
            gems_spent_per_user = sum(as.numeric(gem_spent)) / length(unique(amplitude_id)),
            research_rate = sum(grepl("StartResearch", e_purchase)) / length(unique(amplitude_id)),
            hurry_rate = sum(grepl("HurryResearch", e_purchase)) / sum(grepl("StartResearch", e_purchase))) %>% 
  as.data.table()


df2 <- df1 %>% 
  remove_top_percent_of_spenders("experiment_group",0.98) %>% 
  filter(!is.na(e_purchase)) %>% 
  mutate(e_purchase = case_when(
    grepl("StartResearch", e_purchase)==T ~ "Research Start",
    grepl("StarGateSkip", e_purchase)==T ~ "StarGateSkip",
    grepl("LevelUpTo", e_purchase)==T ~ "Hero Level Up",
    grepl("HurryResearch", e_purchase)==T ~ "Research Hurry",
    grepl("credit", e_purchase)==T ~ "Credit",
    e_purchase %in% c("callin_bomb", "callin_cryobomb", "callin_energyboost", "callin_reinforcements", "KillBoost", "SuperTowerRefill", "SecondChance") ~ "Boosts",
    grepl("InGameCarePackage", e_purchase)==T ~ "Boosts",
    grepl("CoinPack", e_purchase)==T ~ "CoinPack",
    grepl("ExactConversion", e_purchase)==T ~ "ExactConversion",
    T ~ "Other"
  )) %>% 
  group_by(experiment_group, u_furthestlevel) %>% 
  summarise(research_started = sum(e_purchase == "Research Start", na.rm = T),
            research_hurry = sum(e_purchase == "Research Hurry"),
            hurry_rate = sum(e_purchase == "Research Hurry") / sum(e_purchase == "Research Start"))


# Hurry Rate Plot

ggplot(df2, aes(as.numeric(u_furthestlevel), hurry_rate, colour = experiment_group)) + geom_line(size = 1) + 
  ggtitle("Hurry Rate per Furthest Level Reached") + 
  xlab("Furthest Level")+
  theme(legend.position = "bottom")


## Test ARPU significance
pairwise.prop.test(res1$total_revenue, res1$users)

## Test conversion rate significance
pairwise.prop.test(res1$purchasers, res1$users)

## Test retention significance
pairwise.prop.test(round(unlist(res1$day7retention * res1$users)), res1$users)

##Test ARPPU significance
res2 <- df1[, .(total_revenue = sum(revenue)),by = list(experiment_group, amplitude_id)]
ggplot(res2[total_revenue > 0], aes(experiment_group,total_revenue)) + 
  ggtitle("Revenue distribution of players")+
  geom_boxplot()

pairwise.wilcox.test(res2[total_revenue > 0]$total_revenue, res2[total_revenue > 0]$experiment_group)


# Build graphs for experiment template

# Level progression

res3 <- df1 %>% 
  group_by(experiment_group, amplitude_id) %>% 
  mutate(highest_level_reached = max(coalesce(as.numeric(u_furthestlevel), 0))) %>% 
  group_by(experiment_group, highest_level_reached) %>% 
  summarise(users = length(unique(amplitude_id))) %>% 
  arrange(desc(highest_level_reached)) %>% 
  group_by(experiment_group) %>% 
  mutate(users = cumsum(users)) %>% 
  arrange(highest_level_reached) %>%
  group_by(experiment_group) %>%
  mutate(progression_rate = users / max(users))

ggplot(res3[res3$highest_level_reached %in% 0:50,],aes(highest_level_reached,progression_rate, colour = experiment_group)) +
  geom_line(size = 1) + 
  theme(legend.position = "bottom") +
  scale_x_continuous(name = "Level by position", breaks = 0:23 * 4)+
  ggtitle("Level Progression")


# Retention

res4 <- df1 %>% 
  group_by(experiment_group,day) %>% 
  summarise(users = length(unique(amplitude_id))) %>% 
  group_by(experiment_group) %>%
  mutate(retention = users / max(users)) %>% 
  filter(day <= 14)

ggplot(res4[res4$day > 0,], aes(day, retention, colour = experiment_group)) +
  geom_line(size = 1) +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks= 0:floor(max(res4$day) / 2) * 2 + 1) +
  ggtitle("Retention")



# Fall off

res5 <- res3 %>%
  arrange(experiment_group, highest_level_reached) %>%
  group_by(experiment_group) %>%
  mutate(fall_off = 1- (data.table::shift(as.numeric(users), 1, 1, "lead") / users))

ggplot(res5[res5$highest_level_reached %in% 1:49,], aes(highest_level_reached, fall_off, colour = experiment_group)) +
  geom_line() +
  theme(legend.position = "bottom") + 
  scale_x_continuous(name = "Level by position", breaks = 0:20 * 4)+
  ggtitle("Fall off")


# LTV 


res6 <- df1 %>% 
  group_by(experiment_group, day) %>% 
  summarise(arpu = sum(revenue, na.rm = T),
            users = length(unique(amplitude_id))) %>% 
  arrange(day) %>% 
  group_by(experiment_group) %>% 
  mutate(cumulative = cumsum(arpu) / max(users)) %>% 
  filter(day <= 14)

ggplot(res6, aes(day, cumulative, color = experiment_group)) +
  geom_smooth(method = "loess", se = F)+
  theme(legend.position = "bottom") +
  scale_x_continuous(name = "Day", breaks = 0:20 * 4)+
  ggtitle("LTV")

# Revenue, Credits, Gems per player per level


res7 <- df1 %>% 
  group_by(experiment_group, u_furthestlevel) %>% 
  summarise(credits_spent_per_player = sum(e_credit_spent, na.rm = T) / length(unique(amplitude_id)),
            gems_spent_per_player = sum(gem_spent) / length(unique(amplitude_id)),
            ARPU = sum(revenue, na.rm = T) / length(unique(amplitude_id)))%>% 
  arrange(as.numeric(u_furthestlevel)) %>% 
  filter(!is.na(u_furthestlevel))


ggplot(res7, aes(as.numeric(u_furthestlevel), ARPU, color = experiment_group)) +
  geom_smooth(method = "loess", se = F)+
  ggtitle("ARPU per level") + 
  theme(legend.position = "bottom") + 
  xlab("Furthest Level Reached")


ggplot(res7, aes(as.numeric(u_furthestlevel), gems_spent_per_player, color = experiment_group)) +
  geom_smooth(method = "loess", se = F)+
  ggtitle("Gem Spent per Player per Level")+
  theme(legend.position = "bottom") + 
  xlab("Furthest Level Reached")

ggplot(res7, aes(as.numeric(u_furthestlevel), credits_spent_per_player, color = experiment_group)) +
  geom_smooth(method = "loess", se = F)+
  theme(legend.position = "bottom") + 
  ggtitle("Ave Credits Spent per Player per Level")+ 
  xlab("Furthest Level Reached")


# Research started per player per level 

research <- df1 %>% 
  filter(grepl("StartResearch", e_purchase) == T) %>% 
  mutate(research = case_when(
    grepl("StartResearchRanged", e_purchase) == T ~ "Ranged",
    grepl("StartResearchReinforcement", e_purchase) == T ~ "Reinforcements",
    grepl("StartResearchScience", e_purchase) == T ~ "Science",
    grepl("StartResearchInfantry", e_purchase) == T ~ "Infantry",
    grepl("StartResearchEnergy", e_purchase) == T ~ "Energy",
    grepl("StartResearchCryo", e_purchase) == T ~ "Cryobomb",
    grepl("StartResearchArtillery", e_purchase) == T ~ "Artillery",
    grepl("StartResearchbomb", e_purchase) == T ~ "Bomb",
    T ~ "other"
  )) %>% 
  group_by(research, u_furthestlevel, experiment_group) %>% 
  summarise(research_per_user = length(amplitude_id) / length(unique(amplitude_id))) %>% 
  filter(!is.na(u_furthestlevel)) %>% 
  dcast(u_furthestlevel + research ~ experiment_group, value.var = "research_per_user") %>% 
  arrange(research, as.numeric(u_furthestlevel))

# Purchases 

revenue <- df1 %>% 
  remove_top_percent_of_spenders("experiment_group", 0.97) %>% 
  filter(event_type == "unverified_revenue") %>% 
  filter(!is.na(e__productid)) %>% 
  group_by(e__productid, experiment_group) %>% 
  summarise(purchases = length(amplitude_id))

ggplot(revenue, aes(e__productid, purchases, fill = experiment_group)) + geom_bar(stat = "identity", position = "dodge") + coord_flip() + 
  ggtitle("Number of purchases - top 3% of outliers removed")


######## Event Analysis ########

event <- df1 %>% 
  filter(e_event_identifier == "July2017Event") %>% 
  group_by(u_experiment_groups) %>% 
  summarise(players = length(unique(amplitude_id)),
            revenue = sum(revenue, na.rm = T))






