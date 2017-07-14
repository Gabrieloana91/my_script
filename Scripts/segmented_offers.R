

revenue <- "SELECT 
              oe.amplitude_id AS amplitude_id,
              event_time::date AS dt,
              cast(extract(epoch from event_time) as integer) as ts,
              e_productid AS product_id,
              event_type AS event,
              fl.fl_date AS fl_date,
              country AS country_code, 
              u_store as store_id,
              u_currentcredits::numeric AS current_credits,
              u_currentgold::numeric AS current_gems,
              e_currency_code::text AS currency_code,
              e_currency_value::float AS currency_value,
              COALESCE(u_furthestlevel) AS current_level
              
              
              FROM	
              events.ac_unverified_revenue oe"


session_account <- "SELECT 
              oe.amplitude_id AS amplitude_id,
              event_time::date AS dt,
              cast(extract(epoch from event_time) as integer) as ts,
              event_type AS event,
              fl.fl_date AS fl_date,
              u_store as store_id,
              country AS country_code, 
              u_currentcredits::numeric AS current_credits,
              u_currentgold::numeric AS current_gems,
              version_name,
              COALESCE(u_furthestlevel) AS current_level,
              u_furthestlevel::numeric AS furthest_level
              
              FROM	
              events.ac_session_account oe"

first_launch <- " 
				      SELECT * FROM 
              (
              SELECT
              amplitude_id,
              event_type AS event, 
              min(event_time::date) AS fl_date,
              min(event_time::date) AS dt,
              min(cast(extract(epoch from event_time) as integer)) AS ts
              FROM events.ac_first_launch 
              GROUP BY 1,2
              )
				      "


level_finished <- "SELECT 
              oe.amplitude_id AS amplitude_id,
              event_time::date AS dt,
              cast(extract(epoch from event_time) as integer) as ts,
              event_type AS event,
              fl.fl_date AS fl_date,
              u_store as store_id,
              e_level_id as current_level,
              u_currentcredits::numeric AS current_credits,
              u_currentgold::numeric AS current_gems,
              u_furthest_level_id::numeric AS furthest_level,
              e_outcome as outcome,
              e_hero as hero_1,
              e_herolevel::numeric as hero1_level,
              e_hero2 as hero_2,
              e_herolevel2::numeric as hero2_level,
              e_cryobombs_used::numeric as cryobombs_used,
              e_supertoweruses::numeric as supertower_uses,
              e_reinforcesused::numeric as reinforces_used,
              e_airstrikesused::numeric as airstrikes_used,
              e_energyboosts_used::numeric as energyboost_used,
              e_levelname as level_name,
              e_level_kind as level_kind

              
              FROM
              events.ac_level_finished oe"

        
event_level <- "SELECT 
              oe.amplitude_id AS amplitude_id,
              event_time::date as dt,
              cast(extract(epoch from event_time) as integer) AS ts,
              event_type AS event,
              fl.fl_date AS fl_date,
              u_store AS store,
              u_currentcredits::numeric AS current_credits,
              u_currentgold::numeric AS current_gems,
              e_levelname as level_name,
              e_hero as hero_1,
              e_herolevel::numeric AS hero1_level,
              e_hero2 as hero_2,
              e_herolevel2::numeric AS hero2_level,
              e_reward_tier_reached::numeric AS reward_tier_reached,
              e_attempts::numeric AS attempts,
              e_outcome AS outcome, 
              e_cryobombs_used::numeric as cryobombs_used,
              e_supertoweruses::numeric as supertower_uses,
              e_reinforcesused::numeric as reinforces_used,
              e_airstrikesused::numeric as airstrikes_used,
              e_energyboosts_used::numeric as energyboost_used,
              e_event_identifier AS event_identifier
      
              FROM events.ac_event_level_finished oe
              "


filter <- "
        WHERE
        event_time BETWEEN '2017-06-16'AND '2017-07-01'
        AND
        event_time::date >= fl_date
        "

join <- " INNER JOIN (
        SELECT 
        amplitude_id,
        min(event_time::date) AS fl_date
        FROM events.ac_first_launch
        GROUP BY 1
        ) fl ON fl.amplitude_id = oe.amplitude_id
        "


non_spender <- "SELECT amplitude_id 
            FROM events.ac_session_account 
            WHERE amplitude_id NOT IN (SELECT DISTINCT amplitude_id FROM events.ac_unverified_revenue)"



# Query the data


df <- as.data.frame(make_currency_values_USD(query_prometheus(paste0(revenue, join, filter))))
df <- rbind.fill(df, query_prometheus(paste0(session_account, join, filter)))
df <- rbind.fill(df, query_prometheus(paste0(level_finished, join, filter)))
df <- rbind.fill(df, query_prometheus(paste0(event_level, join, filter)))
non_spender_database <- query_prometheus(non_spender)



df <- as.data.table(df)
df$amplitude_id <- as.character(df$amplitude_id)
z <- df
# df <- z
non_spender_database <- non_spender_database %>% 
  summarise(amplitude_id = unique(amplitude_id))


#### 0. Data Cleaning ####



df <- get_sessions_new(df)
df$furthest_level <- na.locf(df$furthest_level)
df <- add_day(df)
z <- df

df <- df %>%
  group_by(amplitude_id) %>% 
  mutate(max_furthest_level = max(furthest_level[!is.na(furthest_level)], na.rm=T),
         total_revenue = sum(revenue, na.rm=T),
         day_diff = day - min(day))
# 
# spenders <- df %>% 
#   group_by(amplitude_id) %>% 
#   mutate(total_revenue = sum(revenue,na.rm=T)) %>% 
#   filter(total_revenue > 0)
# df <- as.data.table(df)
# 
# non_spenders <- df[df$amplitude_id %nin% spenders$amplitude_id]
# 




#### 2. Starter Pack Bundle ####

### spb1
# Requirements :  1. Non_payer
#                 2. Does not own Roxie
#                 3. Max_furthest_level > 3
#                 4. Days played > 3 days
###




spb1 <-  df %>% 
  filter(day_diff < 3) %>% 
  filter(max_furthest_level > 3 & current_level != 3) %>% 
  filter(hero_1 != "HeroRoxie") %>% 
  filter(hero_2 != "HeroRoxie") %>% 
  group_by() %>% 
  summarise(total_players = length(unique(amplitude_id)),
            payers = length(unique(amplitude_id[total_revenue > 0])))

temp <- df %>% 
  filter(day_diff < 5)

temp <- temp %>% 
  mutate(payer = ifelse(total_revenue > 0, T, F))
  
temp <- temp %>% 
  mutate(spb1 = case_when(
    hero_1 == "HeroRoxie" ~ F,
    hero_2 == "HeroRoxie" ~ F,
    max_furthest_level > 3 ~ T,
    max_furthest_level <= 3 ~ F,
    current_level != 3 ~ T,
    current_level == 3 ~ F
  ))
           
           
           ifelse(hero_1 != "HeroRoxie" & hero_2 != "HeroRoxie" & day_diff < 3 & max_furthest_level > 3 & (current_level != 3 | current_level == "NA") , T, F))




### spb2
# Requirements :  1. Non_payer
#                 2. Does not own Roxie
#                 3. Max_furthest_level > 3
#                 4. Day_diff > 4
###


































