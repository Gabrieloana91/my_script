#### WOT Project ####

df <- read.csv("Z:\\Analytics Work\\Side Projects\\World of Tanks\\raw_data.csv")
df<- df[1:36,]

df$survived <- as.numeric(df$survived)
df$player <- as.character(df$player)
df$tank_type <- as.character(df$tank_type)
names(df)[1] <- "date"
df$date <- as.Date(df$date)
df <- as.data.frame(df)
### analysis ###

temp <- df %>% 
  group_by(player, tank_type) %>% 
  summarise(number_of_plays = max(game_number),
            number_of_games_survived = sum(survived),
            max_damage = max(damage_dealt),
            average_damage = mean(damage_dealt),
            number_of_kills = sum(kills)) %>% 
  melt(.,id.vars= 1:2)


ggplot(temp, aes(tank_type, value, fill = player)) + geom_bar(stat = "identity", position = "dodge") + facet_grid(variable~., scales = "free")+
  scale_fill_manual(values = c("goldenrod3","darkorchid3"))

### pass rates ###

tank_info <- df %>% 
  group_by(player, tank, tank_type) %>%
  summarise(days_played = length(unique(date)),
            played = length(player),
            won = length(player[outcome == "win"]),
            survived = length(player[survived == 1]),
            avg_damage = round(mean(damage_dealt)),
            avg_experience = round(mean(experience_earned)),
            avg_spotting = round(mean(damage_spotting)),
            avg_blocked = round(mean(damage_blocked)),
            avg_kills = round(mean(kills)),
            avg_bonds = round(mean(bonds_earned)),
            win_rate = round(length(player[outcome == "win"]) / length(player) * 100))
  
  

type_info <- df %>% 
  group_by(player,  tank_type) %>%
  summarise(days_played = length(unique(date)),
            played = length(player),
            won = length(player[outcome == "win"]),
            survived = length(player[survived == 1]),
            median_tier = round(median(tier)),
            avg_damage = round(mean(damage_dealt)),
            avg_experience = round(mean(experience_earned)),
            avg_spotting = round(mean(damage_spotting)),
            avg_blocked = round(mean(damage_blocked)),
            avg_kills = round(mean(kills)),
            avg_bonds = round(mean(bonds_earned)),
            win_rate = round(length(player[outcome == "win"]) / length(player) * 100))


map_info <- df %>% 
  group_by(player, map) %>% 
  summarise(days_played = length(unique(date)),
            played = length(player),
            won = length(player[outcome == "win"]),
            survived = length(player[survived == 1]),
            median_tier = round(median(tier)),
            avg_damage = round(mean(damage_dealt)),
            avg_experience = round(mean(experience_earned)),
            avg_spotting = round(mean(damage_spotting)),
            avg_blocked = round(mean(damage_blocked)),
            avg_kills = round(mean(kills)),
            avg_bonds = round(mean(bonds_earned)),
            win_rate = round(length(player[outcome == "win"]) / length(player) * 100))


### session summary ####

session <- df %>% 
  filter(player == "gabi") %>% 
  group_by(date) %>% 
  summarise(games_played = max(game_number),
            win_rate = round(length(player[outcome == "win"]) / max(game_number),3)*100,
            survive_rate = round(length(player[survived == 1]) / max(game_number),3)*100,
            total_experience = round(sum(experience_earned)),
            median_tier = round(median(tier)),
            median_damage = round(median(damage_dealt)),
            median_experience = round(median(experience_earned)),
            median_spotting = round(median(damage_spotting)),
            median_blocked = round(median(damage_blocked)),
            kills = round(sum(kills)),
            bonds = round(sum(bonds_earned))
            ) %>% 
  melt(., id.vars = 1)

ggplot(session[session$variable %in% c("games_played"),], aes(date, value)) + 
  geom_bar(stat = "identity", fill = "goldenrod3") + 
  theme_solarized_2(light = FALSE) +
  scale_colour_solarized("blue")+
  ggtitle("Games")+
  theme(plot.title = element_text(hjust = 0.5, colour = "goldenrod3"))+
  ylab("")+
  xlab("")+
  geom_text(aes(label=paste0(round(value,3),"")),vjust=-0.25,position=position_dodge(.9), size=3.5, colour = "goldenrod3")+
  scale_y_continuous(breaks = seq(0,100,by=1))

