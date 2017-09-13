### Wot Controller File ###
library(shiny)
library(ggthemes)
options(scipen = 999)
player_name = "andrei"


### Loading Data

df <- read.csv("Z:\\Analytics Work\\Side Projects\\World of Tanks\\raw_data.csv")

names(df)[1] <- "date"
df$credits[is.na(df$credits)] <- 0
df$date <- as.Date(df$date, format = "%d/%m/%Y")
df <- df %>% filter(!is.na(experience_earned))
df$survived <- as.numeric(df$survived)
df$player <- as.character(df$player)
df$tank_type <- as.character(df$tank_type)
df <- as.data.frame(df)
df <- df %>% 
  mutate(damage_blocked = as.numeric(damage_blocked),
         damage_spotting = as.numeric(damage_spotting),
         damage_blocked = as.numeric(damage_blocked),
         credits = as.numeric(credits),
         tier = as.numeric(tier))
df$tank_type <- factor(df$tank_type, levels = c("heavy","medium","light","tank destroyer","SPG"), labels = c("Heavy", "Medium", "Light", "Tank Destroyer", "SPG"))


df <- df %>% 
  select(date, player) %>% 
  group_by(date, player) %>% 
  mutate(session = length(unique(date))) %>% 
  distinct(.) %>% 
  group_by(player) %>% 
  mutate(session = cumsum(session)) %>% 
  inner_join(., df, by = c("player","date"))


### session summary ####

session <- df %>% 
  filter(player == player & session %in% tail(unique(df$session),10)) %>% 
  group_by(session) %>% 
  summarise(games_played = max(game_number),
            win_rate = round(length(player[outcome == "win"]) / max(game_number),3)*100,
            survive_rate = round(length(player[survived == 1]) / max(game_number),3)*100,
            total_experience = round(sum(experience_earned)),
            median_tier = round(mean(tier)),
            median_damage = round(mean(damage_dealt)),
            median_experience = round(mean(experience_earned)),
            median_spotting = round(mean(damage_spotting)),
            median_blocked = round(mean(damage_blocked)),
            kills = round(sum(kills)),
            bonds = round(sum(bonds_earned))
  ) %>% 
  melt(., id.vars = 1)

#### Cumulative graph ###

cumm <- df %>% 
  group_by(session) %>% 
  filter(player == player_name) %>% 
  filter(session %in% tail(unique(session),2)) %>% 
  group_by(game_number) %>% 
  select(session,game_number, outcome, player) %>%
  group_by(game_number) %>% 
  mutate(victories = ifelse(outcome == "win", 1, 0)) %>% 
  group_by(player, session) %>% 
  mutate(victories = cumsum(victories)) %>% 
  group_by(game_number, session) %>% 
  mutate(pass_rate = round(victories / game_number * 100,1)) %>% 
  select(session, pass_rate, game_number) %>% 
  mutate(session_temp = paste0("Session ",session))

################################################### Plots ###

### Win Rate ###

P1 <- ggplot(session[(session$variable %in% c("games_played")), ], aes(session, value)) + 
  geom_bar(stat = "identity", fill = "goldenrod3") + 
  theme_solarized_2(light = FALSE) +
  scale_colour_solarized("blue")+
  ggtitle("Games")+
  theme(plot.title = element_text(hjust = 0.5, colour = "goldenrod3"))+
  ylab("")+
  xlab("")+
  geom_text(aes(label=paste0(round(value,3),"")),vjust=-0.25,position=position_dodge(.9), size=3.5, colour = "goldenrod3")+
  scale_y_continuous(breaks = seq(0,100,by=1))+
  scale_x_continuous(breaks = seq(0,100,by=1))

### Games ###

P2 <- ggplot(session[(session$variable %in% c("win_rate", "survive_rate")),], aes(session, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("goldenrod3", "darkorchid3"), labels=c("Win Rate", "Survival Rate"))+
  theme_solarized_2(light = FALSE) +
  scale_colour_solarized("blue")+
  ggtitle("Win Rate and Survival Rate")+
  theme(plot.title = element_text(hjust = 0.5, colour = "goldenrod3"), legend.title=element_blank())+
  ylab("")+
  xlab("")+
  geom_text(aes(label=paste0(round(value,3),"%")),vjust=-0.25,position=position_dodge(.9), size=3.5, colour = "goldenrod3")+
  geom_hline(yintercept = 50, colour = "red3") +
  expand_limits(y=c(0:100))+
  scale_y_continuous(breaks = seq(0,100,by=20))

### Medians ###

P3 <- ggplot(session[(session$variable %in% c("median_damage", "median_spotting", "median_blocked", "median_experience")),], aes(session, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("goldenrod3", "darkorchid3", "darkred", "darkblue"), labels=c("Damage", "Experience" ,"Spotting Dmg", "Blocked"))+
  theme_solarized_2(light = FALSE) +
  scale_colour_solarized("blue")+
  ggtitle("Average Damage and Experience")+
  theme(plot.title = element_text(hjust = 0.5, colour = "goldenrod3"), legend.title=element_blank())+
  ylab("")+
  xlab("")+
  geom_text(aes(label=paste0(round(value,3),"")),vjust=-0.25,position=position_dodge(.9), size=3.5, colour = "goldenrod3")

### Cumulative Experience ### 

P4 <- ggplot(cumm, aes(game_number, pass_rate, color = session_temp)) +
  geom_line(size = 1)+
  theme_solarized_2(light = FALSE) +
  scale_colour_solarized("blue")+
  scale_colour_manual(values = c("goldenrod3", "darkorchid3"))+
  ggtitle("Cummulative Experience")+
  theme(plot.title = element_text(hjust = 0.5, colour = "goldenrod3"), legend.title=element_blank())+
  ylab("")+
  xlab("")+
  geom_text(aes(label=paste0(round(pass_rate,3),"")),vjust=-0.25,position=position_dodge(.9), size=3.5, colour = "goldenrod3")+
  geom_hline(yintercept = 50, colour = "red3") +
  expand_limits(y=c(0:100))+
  scale_x_continuous(breaks = seq(1,max(cumm$game_number),by=1))

##### Live Info #####

### Cumulative Win Rate ### 

P4 <- ggplot(cumm, aes(game_number, pass_rate, color = session_temp)) +
  geom_line(size = 1)+
  theme_solarized_2(light = FALSE) +
  scale_colour_solarized("blue")+
  scale_colour_manual(values = c("goldenrod3", "darkorchid3"))+
  ggtitle("Cummulative Win Rate")+
  theme(plot.title = element_text(hjust = 0.5, colour = "goldenrod3"), legend.title=element_blank())+
  ylab("")+
  xlab("")+
  geom_text(aes(label=paste0(round(pass_rate,3),"")),vjust=-0.25,position=position_dodge(.9), size=3.5, colour = "goldenrod3")+
  geom_hline(yintercept = 50, colour = "red3") +
  expand_limits(y=c(0:100))+
  scale_x_continuous(breaks = seq(1,max(cumm$game_number),by=1))


### Credits History ###

credits <- df %>% 
  filter(session %in% tail(unique(session),2) & player == player_name) %>% 
  select(session, game_number, credits, player) %>% 
  mutate(session = paste0("Session ",session)) %>% 
  group_by(session, player) %>% 
  mutate(credits = cumsum(credits))


P5 <- ggplot(credits, aes(game_number, credits, color = session)) +
  geom_line(size = 1)+
  theme_solarized_2(light = FALSE) +
  scale_colour_solarized("blue")+
  scale_colour_manual(values = c("goldenrod3","darkorchid3"))+
  ggtitle("Credits Generated")+
  theme(plot.title = element_text(hjust = 0.5, colour = "goldenrod3"), legend.title=element_blank())+
  ylab("")+
  xlab("")+
  # geom_text(aes(label=paste0(round(credits/1000,0),"k")),vjust=-0.25,position=position_dodge(.9), size=3.5, colour = "goldenrod3")+
  scale_x_continuous(breaks = seq(1,max(credits$game_number),by=1))

###  ### Median Experience per Game ### 

experience <- df %>% 
  filter(session == last(session)) %>% 
  filter(player == player_name) %>% 
  group_by(game_number, tank_type) %>% 
  mutate(battles_in_tank = length(player)) %>% 
  group_by(player,tank_type) %>% 
  mutate(battles_in_tank = cumsum(battles_in_tank)) %>% 
  select(game_number, experience_earned, player, tank_type, battles_in_tank) 

P6 <- ggplot(experience, aes(battles_in_tank, experience_earned, fill = tank_type)) +
  geom_bar(stat = "identity", position = "dodge")+
  theme_solarized_2(light = FALSE) +
  scale_colour_solarized("blue")+
  scale_fill_manual(values = c("goldenrod3", "darkorchid3", "deepskyblue3", "chartreuse3", "red3"))+
  ggtitle("Experience Per Tank Type")+
  theme(plot.title = element_text(hjust = 0.5, colour = "goldenrod3"), legend.title=element_blank())+
  ylab("")+
  xlab("")+
  geom_text(aes(label=paste0(round(experience_earned,3),"")),vjust=-0.25,position=position_dodge(.9), size=3.5, colour = "goldenrod3")+
  expand_limits(y=c(0:100))+
  scale_x_continuous(breaks = seq(1,max(cumm$game_number),by=1))

### Win Rate per Tier ###


tier_type <- df %>% 
  filter(session %in% tail(unique(session),2) & player == player_name) %>% 
  group_by(tier) %>% 
  select(session, outcome, player, tier, tank_type) %>%
  group_by(session, tier, tank_type) %>% 
  summarise(pass_rate = round(length(player[outcome == "win"]) / length(player)*100,1)) %>%
  group_by(tier, tank_type) %>% 
  mutate(session = paste0("Session ",session))


  

P7 <- ggplot(tier_type, aes(tier, pass_rate, fill = tank_type)) +
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(session ~ . )+
  theme_solarized_2(light = FALSE) +
  scale_colour_solarized("blue")+
  scale_fill_manual(values = c("goldenrod3", "darkorchid3", "deepskyblue3", "chartreuse3", "red3"))+
  ggtitle("Win Rate Per Tank Type")+
  theme(plot.title = element_text(hjust = 0.5, colour = "goldenrod3"), legend.title=element_blank())+
  ylab("")+
  xlab("")+
  geom_text(aes(label=paste0(round(pass_rate,3),"")),vjust=-0.25,position=position_dodge(.9), size=3.5, colour = "goldenrod3")+
  expand_limits(y=c(0:105))+
  scale_x_continuous(breaks = seq(1,max(tier_type$tier),by=1))


##### TANK INFO #####

tank_info <- df %>% 
  filter(session %in% tail(unique(session),1) & player == player_name) %>% 
  select(player, session, tank, tank_type, tier, outcome, credits, damage_dealt) %>% 
  group_by(tank, tank_type, tier) %>% 
  summarise(games = length(player),
            win_rate = round(length(player[outcome == "win"]) / length(player)*100,2),
            avg_credits = round(mean(credits)),
            avg_damage = round(mean(damage_dealt))) %>% 
  rename(Games = games,
         Win_Rate = win_rate,
         Average_Credits = avg_credits,
         Average_Damage = avg_damage) %>% 
  melt(id.vars = 1:3)

tank_info$tier <- factor(tank_info$tier, levels = c("1","2","3","4","5","6","7","8","9","10"), 
                  labels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4", "Tier 5","Tier 6","Tier 7","Tier 8","Tier 9","Tier 10"))
  
P8 <- ggplot(tank_info, aes(tank, value , fill = tank_type)) +
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(tier ~ variable, scales = "free")+
  theme_solarized_2(light = FALSE) +
  scale_colour_solarized("blue")+
  scale_fill_tableau(palette = "tableau10")+
  ggtitle("Tank Summary")+
  theme(plot.title = element_text(hjust = 0.5, colour = "goldenrod3"), legend.title=element_blank())+
  coord_flip()+
  ylab("")+
  xlab("")+
  geom_text(aes(label=paste0(round(value,3),"")),vjust=0,position=position_dodge(0), size=3.5, colour = "white")


