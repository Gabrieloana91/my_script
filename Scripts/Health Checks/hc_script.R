##########################################

# Alien Creeps - Query Prometheus

##########################################


source("D:\\R Working File\\1. ACTD\\2017-03-21 - ACTD - HealthCheck\\Query\\sql_query.r")


ac_hc_join <- "
INNER JOIN (
SELECT 
amplitude_id,
min(event_time::date) AS fl_date
FROM events.ac_first_launch
GROUP BY 1
) fl ON fl.amplitude_id = oe.amplitude_id
"

ac_hc_filter <- "
WHERE
fl_date BETWEEN '2017-05-01'AND '2017-05-15'
AND
event_time::date >= fl_date
"

ac_hc_filter_2 <- "
WHERE
fl_date BETWEEN '2017-05-01'AND '2017-05-15'
"


# first_launch <- "2017-05-01"


df <- as.data.frame(make_currency_values_USD(query_prometheus(paste0(ac_hc_query_revenue, ac_hc_join, ac_hc_filter))))
df <- rbind.fill(df, query_prometheus(paste0(ac_hc_query_session_account, ac_hc_join, ac_hc_filter)))
df <- rbind.fill(df, query_prometheus(paste0(ac_hc_query_level_finished, ac_hc_join, ac_hc_filter)))
df <- rbind.fill(df, query_prometheus(paste0(ac_hc_query_challenges, ac_hc_join, ac_hc_filter)))
df <- rbind.fill(df, query_prometheus(paste0(ac_hc_query_currency_spent, ac_hc_join, ac_hc_filter)))
df <- rbind.fill(df, query_prometheus(paste0(ac_hc_query_first_launch, ac_hc_filter_2)))
df <- as.data.table(df)

df$amplitude_id <- as.character(df$amplitude_id)

# Make a backup 

z <- df
#df <- z




#### 0. Data cleaning ####


challenge_order <- c("A1_2r_b","A1_6s_b","A1_6r_e","A1_7r2_b","A1_4s_e","A1_1r_n","A1_7r_b","A2_6r2_b","A1_7s_n",
                     "A1_6r2_e","A2_6mt_e","A1_3s_h","A1_4r_h","A1_5s2_e","A2_2r_n","A1_5s_e","A1_2r2_b","A2_3r_x",
                     "A2_4mt_n","A1_6s2_b","A2_1sos_h","A2_7r2_b","A2_6s_n","A2_6mt2_e","A1_4s2_e","A2_6r_b","A2_7r_b",
                     "A1_6s_x","A2_7sos_h","A2_5s_b","A2_5s2_b","A3_1r_e","A3_2s2_e","A3_2s_e","A3_1r2_e","A3_3s_x","A3_6sos_h",
                     "A4_2r_e","A4_1s_e","A3_6s_e","A4_3sos_e","A4_4mt_e","Ross1","Ross2","Ross4","Ross3","Ross5","A3_7s_e","A2_2r2_n",
                     "A1_5sos_e","A4_1r_e","A3_7sos_e","A4_5r_e","A4_6mt_b","A3_1s_e","A4_2sos_e","A4_10s_e")

df <- df[amplitude_id!="00000000-0000-0000-0000-000000000000"]
temp <- unique(df[current_gems>9999 | current_gems < 0 | current_credits > 500000 | current_credits < 0]$amplitude_id)
df <- df[amplitude_id %nin% temp]

# df <- df[!grepl("^1\\.",version)]

df <- df[!duplicated(df)]

# df <- df[dt>=fl_date]
df[,lives_lost:= strsplit(gsub(" ","",gsub("\\[|\\]","",df$lives_lost)),",")]
df$lives_lost <-  sapply(df$lives_lost,function(x) as.numeric(unlist(x)))
##df$most_lost <- sapply(df$lives_lost,function(x) which.max(x))


df$current_level <- as.numeric(df$current_level)
df$furthest_level <- as.numeric(df$furthest_level)
df$max_wave <- as.numeric(df$max_wave)
df$level_name <- gsub(".challenge","",df$level_name)
df$level_kind <- factor(df$level_kind,levels = c("campaign","veteran","specops","0","1","2"),labels = c("campaign","veteran","specops","0","1","2"))


temp <- df %>% group_by(amplitude_id) %>%
  summarise(max_position = max(furthest_level, na.rm = T), unique_positions = length(unique(furthest_level[!is.na(furthest_level)]))) %>%
  filter(max_position <= unique_positions) %>%
  select(amplitude_id)
df <- df %>% filter(amplitude_id %in% temp$amplitude_id)


df <- df[order(ts)]

df[event=="Level Finished",attempts:=1:length(ts),by=list(amplitude_id,current_level,level_kind)]
df[event=="challenge_finished",attempts:=1:length(ts),by=list(amplitude_id,level_name,level_kind)]

inac <- df[,list(
  max_day = max(dt),
  furthest_level = max(furthest_level,na.rm=T), 
  last_level = tail(current_level[!is.na(current_level)],1), 
  last_mode = tail(level_kind[!is.na(level_kind)],1)),
  by=amplitude_id]
inac[furthest_level < 1, furthest_level := 1]
inac <- inac[max_day < max(max_day) - 6]
inac[last_mode %in% c("0","1","2"), last_mode := "challenge"]
inac[,last_mode := as.character(last_mode)]




level_fail <- as.data.frame(tapply(df[level_kind=="campaign"&!is.na(lives_lost)]$lives_lost,df[level_kind=="campaign"&!is.na(lives_lost)]$current_level,function(x) Reduce("+",x)))
names(level_fail) <- "lives_lost_at_wave"
level_fail$current_level <- as.numeric(row.names(level_fail))
level_fail$level_kind <- "campaign"
level_fail$most_lives_lost_at_wave <- sapply(level_fail$lives_lost_at_wave,which.max)
level_fail <- left_join(level_fail,df[,list(attempts=length(amplitude_id)),by=list(current_level,level_kind)],by=c("current_level","level_kind"))
for(i in 1:6) {level_fail[,paste0("lives_lost_at_wave",i)]<- mapply(function(x,y) ifelse(length(x)<i,0,x[[i]]/y),level_fail$lives_lost_at_wave,level_fail$attempts)}





df[is.na(current_level),current_level:=furthest_level]

df$time <- df$ts
df <- get_sessions_new(df,"amplitude_id")
df$new_session <- as.numeric(df$new_session)
df$second_chances_bought <- as.numeric(df$second_chances_bought)
df$carepackage_bought_bomb <- as.numeric(df$carepackage_bought_bomb)
df$carepackage_bought_cryobomb <- as.numeric(df$carepackage_bought_cryobomb)
df$carepackage_bought_reinforcement <- as.numeric(df$carepackage_bought_reinforcement)
df$carepackage_bought_energyboost <- as.numeric(df$carepackage_bought_energyboost)
df$maxwave <- as.numeric(df$maxwave)

df <- df[grepl("Finished",event),paid_heroes:= as.numeric(hero_1!="HeroSarge")+ifelse(is.na(hero_2),0,as.numeric(hero_2!="HeroSarge"))]

split_levels <- df[event=="Level Finished"&!is.na(current_level)&!is.na(level_kind), list(
  players = length(unique(amplitude_id)),
  times_played = length(amplitude_id),
  airstrikes = sum(airstrikes_used,na.rm=T),
  reinforcements = sum(reinforces_used,na.rm=T),
  super_tower = sum(supertower_uses,na.rm=T),
  super_tower_refills = sum(super_tower_refills_bought,na.rm=T),
  second_chances = sum(second_chances_bought,na.rm=T),
  care_packs = sum(carepackage_bought_bomb,na.rm=T) + sum(carepackage_bought_cryobomb,na.rm=T) + sum(carepackage_bought_reinforcement,na.rm=T) + sum(carepackage_bought_energyboost,na.rm=T),
  med_gems = median(current_gems,na.rm=T),
  med_credits = median(current_credits,na.rm=T),
  paid_hero_used = sum(paid_heroes,na.rm=T),
  dual_heroes_used = sum(!is.na(hero_2)),
  three_stars = sum(stars_earned==3)/sum(outcome=="won"),
  cryo_used = sum(cryobombs_used,na.rm=T),
  energy_boost_used = sum(energyboost_used,na.rm=T),
  ave_max_wave_loss = mean(maxwave[outcome == "lost"],na.rm = T)
), by=list(current_level,level_kind)]

split_levels <- merge(split_levels,level_fail,by=c("current_level","level_kind"),all=T)

split_challenges <- df[event=="Challenge Finished", list(
  players = length(unique(amplitude_id)),
  times_played = length(amplitude_id),
  airstrikes = sum(airstrikes_used,na.rm=T),
  reinforcements = sum(reinforces_used,na.rm=T),
  super_tower = sum(supertower_uses,na.rm=T),
  super_tower_refills = sum(super_tower_refills_bought,na.rm=T),
  second_chances = sum(second_chances_bought,na.rm=T),
  care_packs = (sum(carepackage_bought_energyboost,na.rm=T) + sum(carepackage_bought_reinforcement,na.rm=T) + sum(carepackage_bought_cryobomb,na.rm=T) + sum(carepackage_bought_bomb,na.rm=T)),
  med_gems = median(current_gems,na.rm=T),
  med_credits = median(current_credits,na.rm=T),
  paid_hero_used = sum(paid_heroes,na.rm=T),
  dual_heroes_used = sum(!is.na(hero_2)),
  cryo_used = sum(cryobombs_used,na.rm=T),
  energy_boost_used = sum(energyboost_used,na.rm=T),
  ave_max_wave_loss = mean(maxwave[outcome == "lost"],na.rm = T)
), by=list(level_name,level_kind)]

setnames(split_challenges,"level_name","current_level")

levels <- df[!is.na(current_level), list(
  players = length(unique(amplitude_id[!is.na(level_kind)])),
  revenue =  sum(revenue,na.rm=T),
  purchasers_on_level = length(unique(amplitude_id[revenue>0]))
), by=current_level]


levels <- df %>%
  group_by(current_level) %>% 
  filter(current_level >= 0 & !is.na(current_level)) %>% 
  summarise(players = length(unique(amplitude_id[!is.na(level_kind)])),
            revenue = sum(revenue,na.rm = T),
            purchasers_on_level = length(unique(amplitude_id[revenue>0]))) %>% 
  group_by(current_level) %>% 
  mutate(conversion = purchasers_on_level / players,
         arpu = revenue/players)


levels <- levels[order(current_level)]

split_levels <- split_levels[order(current_level,level_kind)]
split_challenges <- split_challenges[order(current_level,level_kind)]


levels$futureArpu <- merge(levels, ddply(levels, .(current_level), function(x) {
  
  return(sum(subset(df, df$current_level > x$current_level)$revenue,na.rm=T) / count_unique(identifier = "amplitude_id",data = subset(df, df$current_level == (x$current_level + 1))))
}), all=T, by="current_level")[sort(current_level)]$V1






levelAccount <- df[event == "Level Finished"]

############################
# 
# Pass rate
# 
############################

split_levels$passRate <- merge(split_levels, ddply(split_levels, .(current_level,level_kind), function(x) {
  
  return(sum( levelAccount$current_level == x$current_level & levelAccount$level_kind==x$level_kind & levelAccount$outcome == "won",na.rm=T) / sum( levelAccount$current_level == x$current_level & levelAccount$level_kind==x$level_kind,na.rm=T))
}, .progress = "text"), all=T, by=c("current_level","level_kind"))[order(current_level,level_kind)]$V1


############################
# 
# Pass rate Excl. Boosts
# 
############################

split_levels$passRateNoConsumables <- merge(split_levels, ddply(split_levels, .(current_level,level_kind), function(x) {
  
  return(sum( levelAccount$current_level == x$current_level & levelAccount$level_kind==x$level_kind & (levelAccount$reinforces_used+levelAccount$airstrikes_used+levelAccount$super_tower_refills_bought)==0 & levelAccount$outcome == "won",na.rm=T) / sum( levelAccount$current_level == x$current_level & (levelAccount$reinforces_used+levelAccount$airstrikes_used+levelAccount$super_tower_refills_bought)==0 & levelAccount$level_kind==x$level_kind,na.rm=T))
}, .progress = "text"), all=T, by=c("current_level","level_kind"))[order(current_level,level_kind)]$V1


# ############################
# # 
# # Pass rate Incl. Boosts
# # 
# ############################

split_levels$passRateWithConsumables <- merge(split_levels, ddply(split_levels, .(current_level,level_kind), function(x) {
  
  return(sum( levelAccount$current_level == x$current_level & levelAccount$level_kind==x$level_kind & (levelAccount$reinforces_used+levelAccount$airstrikes_used+levelAccount$super_tower_refills_bought)>0 & levelAccount$outcome == "won",na.rm=T) / sum( levelAccount$current_level == x$current_level & (levelAccount$reinforces_used+levelAccount$airstrikes_used+levelAccount$super_tower_refills_bought)>0 & levelAccount$level_kind==x$level_kind,na.rm=T))
}, .progress = "text"), all=T, by=c("current_level","level_kind"))[order(current_level,level_kind)]$V1

###### Attempts to pass

test <- levelAccount[outcome=="won",list(firstPass=min(attempts),min_to_firstPass= min(game_time), sess_of_first_pass=min(session),day_of_first_pass=min(dt-fl_date)),by=list(amplitude_id,current_level,level_kind)]
test <- test[,list(firstPass=as.numeric(median(firstPass)),min_to_firstPass=as.numeric(median(min_to_firstPass)),sess_of_first_pass=as.numeric(median(sess_of_first_pass)),med_day_of_first_pass=median(day_of_first_pass),ave_day_of_first_pass=mean(day_of_first_pass)),by=list(current_level,level_kind)]

split_levels <- merge(split_levels,test,by=c("current_level","level_kind"))

############################
# 
# Drop off rate
# 
############################

split_levels$dropOff <- merge(split_levels, ddply(split_levels, .(current_level,level_kind), function(x) {
  
  return(sum( inac$last_level == x$current_level & inac$last_mode==as.character(x$level_kind),na.rm=T) / max(split_levels$players,na.rm = T))
}, .progress = "text"), all=T, by=c("current_level","level_kind"))[order(current_level,level_kind)]$V1

########	Challenge data

challengeAccount <- df[event == "Challenge Finished"]
challengeAccount$current_level<- challengeAccount$level_name
############################
# 
# Pass rate
# 
############################

split_challenges$passRate <- merge(split_challenges, ddply(split_challenges, .(current_level,level_kind), function(x) {
  
  return(sum( challengeAccount$current_level == x$current_level & challengeAccount$level_kind==x$level_kind & challengeAccount$outcome == "won",na.rm=T) / sum( challengeAccount$current_level == x$current_level & challengeAccount$level_kind==x$level_kind,na.rm=T))
}, .progress = "text"), all=T, by=c("current_level","level_kind"))[order(current_level,level_kind)]$V1


############################
# 
# Pass rate Excl. Boosts
# 
############################

split_challenges$passRateNoConsumables <- merge(split_challenges, ddply(split_challenges, .(current_level,level_kind), function(x) {
  
  return(sum( challengeAccount$current_level == x$current_level & challengeAccount$level_kind==x$level_kind & (challengeAccount$reinforces_used+challengeAccount$airstrikes_used+challengeAccount$super_tower_refills_bought)==0 & challengeAccount$outcome == "won",na.rm=T) / sum( challengeAccount$current_level == x$current_level & (challengeAccount$reinforces_used+challengeAccount$airstrikes_used+challengeAccount$super_tower_refills_bought)==0 & challengeAccount$level_kind==x$level_kind,na.rm=T))
}, .progress = "text"), all=T, by=c("current_level","level_kind"))[order(current_level,level_kind)]$V1


# ############################
# # 
# # Pass rate Incl. Boosts
# # 
# ############################

split_challenges$passRateWithConsumables <- merge(split_challenges, ddply(split_challenges, .(current_level,level_kind), function(x) {
  
  return(sum( challengeAccount$current_level == x$current_level & challengeAccount$level_kind==x$level_kind & (challengeAccount$reinforces_used+challengeAccount$airstrikes_used+challengeAccount$super_tower_refills_bought)>0 & challengeAccount$outcome == "won",na.rm=T) / sum( challengeAccount$current_level == x$current_level & (challengeAccount$reinforces_used+challengeAccount$airstrikes_used+challengeAccount$super_tower_refills_bought)>0 & challengeAccount$level_kind==x$level_kind,na.rm=T))
}, .progress = "text"), all=T, by=c("current_level","level_kind"))[order(current_level,level_kind)]$V1

###### Attempts to pass

test <- challengeAccount[outcome=="won",list(firstPass=min(attempts),min_to_firstPass= min(game_time), sess_of_first_pass=min(session), day_of_first_pass=min(dt-fl_date)),by=list(amplitude_id,current_level,level_kind)]
test <- test[,list(firstPass=as.numeric(median(firstPass)),min_to_firstPass=as.numeric(median(min_to_firstPass)),sess_of_first_pass=as.numeric(median(sess_of_first_pass)),med_day_of_first_pass=median(day_of_first_pass),ave_day_of_first_pass=mean(day_of_first_pass)),by=list(current_level,level_kind)]

split_challenges <- merge(split_challenges,test,by=c("current_level","level_kind"))

split_levels <- rbind(split_levels,split_challenges, fill=T)


############################
# 
# Progression rate
# 
############################

levels$progressionRate <- shift(levels$players,1,0,"lead") / levels$players


#levels$progressionRateFlag <- 0

#levels[progressionRate <= progressionRateCutoff, progressionRateFlag := progressionRateWeight]




############################
# 
# Fall-off rate
# 
############################

#setnames(inac, "user_id", "amplitude_id")

levels$fallOff <- merge(levels, ddply(levels, .(current_level), function(x) {
  
  return(nrow(subset(inac, inac$furthest_level == x$current_level)) / count_unique(identifier = "amplitude_id",data = subset(levelAccount, levelAccount$current_level == x$current_level)))
}, .progress = "text"), all=T, by="current_level")[sort(current_level)]$V1




#levels$fallOffFlag <- 0

#levels[fallOff >= fallOffCutoff, fallOffFlag := fallOffWeight]



levels$level_kind <- factor("campaign",levels = c("campaign","veteran","specops","0","1","2"),labels = c("campaign","veteran","specops","0","1","2"))
levels$players <- NULL

levels$current_level <- as.character(levels$current_level)
levels <- merge(levels,split_levels,by=c("current_level","level_kind"),all=T)



levelsCSV <- data.table(
  
  level = factor(levels$current_level,c(0:max(df$current_level,na.rm=T),challenge_order),c(0:max(df$current_level,na.rm=T),challenge_order)),
  kind = levels$level_kind,
  revenue = levels$revenue,
  players = levels$players,
  total_times_played = levels$times_played,
  purchasers_at_this_level = levels$purchasers_on_level,
  arpu = levels$arpu,
  future_arpu = levels$futureArpu,
  progression_rate = levels$progressionRate,
  fall_off = levels$fallOff,
  pass_rate = levels$passRate,
  pass_rate_no_consumables = levels$passRateNoConsumables,
  pass_rate_with_consumables = levels$passRateWithConsumables,
  airstrikes_used = levels$airstrikes,
  reinforcements_used = levels$reinforcements,
  tesla_used = levels$super_tower,
  tesla_refills = levels$super_tower_refills,
  care_packs_bought = levels$care_packs,
  paid_hero_used = levels$paid_hero_used,
  dual_heroes_used = levels$dual_heroes_used,
  median_gems = levels$med_gems,
  median_credits = levels$med_credits,
  three_stars_rate  = levels$three_stars,
  med_attempts_to_pass = levels$firstPass,
  second_chances_bought = levels$second_chances,
  ingame_minutes_to_first_pass = levels$min_to_firstPass,
  session_of_first_pass = levels$sess_of_first_pass,
  med_day_of_first_pass = levels$med_day_of_first_pass,
  ave_day_of_first_pass = levels$ave_day_of_first_pass,
  cryo_used = levels$cryo_used,
  energy_boost_used = levels$energy_boost_used,
  drop_off = levels$dropOff,
  most_lives_lost_at_wave = levels$most_lives_lost_at_wave,
  ave_lives_lost_at_wave1 = levels$lives_lost_at_wave1,
  ave_lives_lost_at_wave2 = levels$lives_lost_at_wave2,
  ave_lives_lost_at_wave3 = levels$lives_lost_at_wave3,
  ave_lives_lost_at_wave4 = levels$lives_lost_at_wave4,
  ave_lives_lost_at_wave5 = levels$lives_lost_at_wave5,
  ave_lives_lost_at_wave6 = levels$lives_lost_at_wave6,
  ave_max_wave_on_loss = levels$ave_max_wave_loss
)

levelsCSV <- levelsCSV[order(kind,level),]

write.csv(levelsCSV, "AC_HealthCheck_LevelData.csv", row.names=FALSE)

rm(levelAccount,challengeAccount,inac,levels,split_levels,split_challenges,challenge_order,test)


df$furthest_level <- na.locf(df$furthest_level)

researchData <- df[grepl("Research",product_id)]
researches <- researchData[grepl("^Start",product_id),list(players=length(unique(amplitude_id)),
                                                           ave_level=mean(furthest_level,na.rm = T), 
                                                           median_level=median(furthest_level,na.rm = T), 
                                                           med_credits=median(current_credits,na.rm = T),
                                                           med_gems=median(current_gems,na.rm = T),
                                                           hurry_rate= sum(researchData$product_id==gsub("Start","Hurry",head(product_id,1)))/sum(researchData$product_id==head(product_id,1),na.rm=T)),
                           by=gsub("StartResearch","",product_id)]

setnames(researches,"gsub","research")

research_order <- c("RangedBoost2_1","RangedUpgrade1","RangedBoost1_1","RangedBoost3_1","RangedBoost4_1","RangedUpgrade2","RangedBoost5_1","RangedBoost6_1","RangedSpec1","RangedSpec1_1","RangedSpec1_2","RangedSpec2","RangedSpec2_1","RangedSpec2_2","RangedBoost7_1","RangedBoost8_1","RangedSpec3_1","RangedSpec3","RangedSpec3_2","InfantryBoost1_1","InfantryUpgrade1","InfantryBoost2_1","InfantryBoost3_1","InfantryBoost4_1","InfantryUpgrade2","InfantryBoost5_1","InfantryBoost6_1","InfantrySpec1","InfantrySpec1_1","InfantrySpec1_2","InfantrySpec2","InfantrySpec2_1","InfantrySpec2_2","InfantryBoost7_1","InfantryBoost8_1","InfantrySpec3_1","InfantrySpec3","InfantrySpec3_2","ScienceBoost1_1","ScienceUpgrade1","ScienceBoost2_1","ScienceBoost3_1","ScienceBoost4_1","ScienceUpgrade2","ScienceBoost5_1","ScienceBoost6_1","ScienceSpec1","ScienceSpec1_1","ScienceSpec1_2","ScienceSpec2","ScienceSpec2_1","ScienceSpec2_2","ScienceBoost7_1","ScienceBoost8_1","ArtilleryBoost1","ArtilleryUpgrade1","ArtilleryBoost2","ArtilleryBoost3","ArtilleryBoost4","ArtilleryUpgrade2","ArtilleryBoost5","ArtilleryBoost6","ArtillerySpec1","ArtillerySpec1_1","ArtillerySpec1_2","ArtillerySpec2","ArtillerySpec2_1","ArtillerySpec2_2","ArtilleryBoost7_1","ArtilleryBoost8_1","ReinforcementHealth1","ReinforcementDamage1","ReinforcementArmour","ReinforcementIncreaseNumber","ReinforcementCooldown","bombRadius","bombDamage1","bombCooldown","bombNapalm","bombWingman","EnergyCooldown_1","EnergyAmount_1","EnergyCooldown_2","EnergyAmount_2","EnergyCooldown_3","CryoBombFreeze_1","CryoBombSlow_1","CryoBombCooldown_1","CryoBombFreeze_2","CryoBombCooldown_2","EnergyBoostStarting","CryoBombStarting")

researches$research <- factor(researches$research,research_order,research_order)
researches <- researches[order(research)]

write.csv(researches, "AC_HealthCheck_ResearchData.csv", row.names=FALSE)































