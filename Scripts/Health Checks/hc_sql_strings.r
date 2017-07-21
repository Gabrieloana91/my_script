#########################################

# Alien Creeps - SQL Query

#########################################

ac_hc_join <- "
        INNER JOIN (
        SELECT 
        amplitude_id,
        min(event_time::date) AS fl_date
        FROM events.ac_first_launch
        GROUP BY 1
        ) fl ON fl.amplitude_id = oe.amplitude_id
        "

ac_hc_query_revenue <- "SELECT 
				oe.amplitude_id AS amplitude_id,
				event_time::date AS dt,
				cast(extract(epoch from event_time) as integer) as ts,
        e_productid AS product_id,
				event_type AS event,
        fl.fl_date AS fl_date,
        start_version As version,
        country AS country_code, 
        u_store as store_id,
				u_currentcredits::numeric AS current_credits,
				u_currentgold::numeric AS current_gems,
				e_revenuetype::text AS currency_code,
				e_revenue::float AS currency_value,
        COALESCE(u_furthestlevel) AS current_level
				
			
			FROM	
				events.ac_unverified_revenue oe"

ac_e_query_revenue1 <- "SELECT 
      				oe.amplitude_id AS amplitude_id,
      event_time::date AS dt,
      cast(extract(epoch from event_time) as integer) as ts,
      e_productid AS product_id,
      event_type AS event,
      fl.fl_date AS fl_date,
      start_version As version,
      country AS country_code, 
      u_store as store_id,
      u_currentcredits::numeric AS current_credits,
      u_currentgold::numeric AS current_gems,
      e_revenuetype::text AS currency_code,
      e_revenue::float AS currency_value,
      COALESCE(u_furthestlevel) AS current_level
      
      
      FROM	
      events.ac_revenue_amount oe"

ac_hc_query_session_account <- "SELECT 
				oe.amplitude_id AS amplitude_id,
				event_time::date AS dt,
				cast(extract(epoch from event_time) as integer) as ts,
				event_type AS event,
        fl.fl_date AS fl_date,
				u_store as store_id,
        start_version AS version,
        country AS country_code, 
				u_currentcredits::numeric AS current_credits,
				u_currentgold::numeric AS current_gems,
				version_name,
        COALESCE(u_furthestlevel) AS current_level,
				u_furthestlevel::numeric AS furthest_level
				
			FROM	
				events.ac_session_account oe"

ac_hc_query_first_launch <- " 
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
		
		
ac_hc_query_level_finished <- "SELECT 
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
				e_herokills::numeric as hero1_kills,
				e_hero2 as hero_2,
				e_herolevel2::numeric as hero2_level,
				e_herokills2::numeric as hero2_kills,
				e_tower_mods_available_white::numeric AS white_mod,
				e_tower_mods_available_green::numeric AS green_mod,
				e_tower_mods_available_blue::numeric AS blue_mod,
				e_tower_mods_available_purple::numeric AS purple_mod,
				e_tower_mods_used as tower_mods_used,
				e_timetaken AS time_taken,
				e_carepackagesoffered::numeric as carepackage_offered,
				e_cryobombs_used::numeric as cryobombs_used,
				e_supertoweruses::numeric as supertower_uses,
				e_reinforcesused::numeric as reinforces_used,
				e_airstrikesused::numeric as airstrikes_used,
				e_energyboosts_used::numeric as energyboost_used,
				e_secondchancesoffered::numeric as second_chances_offered,
				e_supertowerrefillsbought::numeric as super_tower_refills_bought,
				e_supertowerrefillsoffered::numeric as super_tower_refills_offered,
				e_carepackagesbought_energyboost::numeric as carepackage_bought_energyboost,
				e_carepackagesbought_reinforce::numeric as  carepackage_bought_reinforcement,
				e_carepackagesbought_cryobomb::numeric as carepackage_bought_cryobomb,
				e_carepackagesbought_bomb::numeric as carepackage_bought_bomb,
				e_secondchancesbought::numeric as second_chances_bought,
				e_session_time as session_time,
        e_liveslostperwave as lives_lost,
        e_levelname as level_name,
        e_level_kind as level_kind,
				e_maxwave as maxwave,
        e_starsearned::numeric as stars_earned
			
			FROM
				events.ac_level_finished oe"
				
ac_hc_query_challenges <- "SELECT
				oe.amplitude_id AS amplitude_id,
				event_time::date AS dt,
				cast(extract(epoch from event_time) as integer) as ts,
				event_type AS event,
				fl.fl_date AS fl_date,
				u_store as store_id,
				u_currentcredits::numeric AS current_credits,
				u_currentgold::numeric AS current_gems,
				version_name,
				u_furthest_level_id::numeric AS furthest_level,
				e_challengename as challenge,
				e_outcome as outcome,
				e_hero as hero_1,
				e_herolevel::numeric as hero1_level,
				e_herokills::numeric as hero1_kills,
				e_hero2 as hero_2,
				e_herolevel2::numeric as hero2_level,
				e_herokills2::numeric as hero2_kills,
				u_totalchallenges::numeric as total_challenges,
				e_tower_mods_available_white::numeric AS white_mod,
				e_tower_mods_available_green::numeric AS green_mod,
				e_tower_mods_available_blue::numeric AS blue_mod,
				e_tower_mods_available_purple::numeric AS purple_mod,
				e_tower_mods_used as tower_mods_used,
				e_timetaken AS time_taken,
				e_carepackagesoffered::numeric as carepackage_offered,
				e_cryobombs_used::numeric as cryobombs_used,
				e_supertoweruses::numeric as supertower_uses,
				e_reinforcesused::numeric as reinforces_used,
				e_airstrikesused::numeric as airstrikes_used,
				e_energyboosts_used::numeric as energyboost_used,
				e_secondchancesoffered::numeric as second_chances_offered,
				e_supertowerrefillsbought::numeric as super_tower_refills_bought,
				e_supertowerrefillsoffered::numeric as super_tower_refills_offered,
				e_carepackagesbought_energyboost as carepackage_bought_energyboost,
				e_carepackagesbought_reinforce as carepackage_bought_reinforcement,
				e_carepackagesbought_cryobomb as carepackage_bought_cryobomb,
				e_carepackagesbought_bomb as carepackage_bought_bomb,
				e_secondchancesbought as second_chances_bought,
        e_liveslostperwave as lives_lost,
				e_session_time as session_time,
        e_challengename as level_name,
        COALESCE(e_challengedifficulty) as level_kind,
				e_maxwave as maxwave
				
			FROM 
				events.ac_challenge_finished oe"
				
ac_hc_query_currency_spent <- "
        SELECT 
        oe.amplitude_id AS amplitude_id,
        event_type AS event,
        cast(extract(epoch from event_time) as integer) AS ts,
        event_time::date AS dt,
        e_purchase as product_id,
        u_fb_email AS email,
        start_version AS version,
        country AS country_code,
        u_store AS store,
        COALESCE(u_furthestlevel) AS current_level,
        u_currentcredits::numeric AS current_credits,
				u_currentgold::numeric AS current_gems,
        e_creditsspent AS credits_spent,
        e_purchase AS purchase,
        e_context as context

        FROM events.ac_currencyspent oe
        "
				
#########################################

# Events 

#########################################

ac_e_query_revenue <- "SELECT 
				oe.amplitude_id AS amplitude_id,
				event_time::date AS dt,
				cast(extract(epoch from event_time) as integer) as ts,
        e_productid AS product_id,
				event_type AS event,
        fl.fl_date AS fl_date,
        start_version As version,
        country AS country_code, 
        u_store as store_id,
				u_currentcredits::numeric AS current_credits,
				u_currentgold::numeric AS current_gems,
				e_revenuetype::text AS currency_code,
				e_revenue::float AS currency_value,
        COALESCE(u_furthestlevel) AS current_level
				
			
			FROM	
				events.ac_unverified_revenue oe"


ac_e_query_session_account <- "SELECT 
				oe.amplitude_id AS amplitude_id,
				event_time::date AS dt,
				cast(extract(epoch from event_time) as integer) as ts,
				event_type AS event,
        fl.fl_date AS fl_date,
				u_store as store_id,
        start_version AS version,
        country AS country_code, 
				u_currentcredits::numeric AS current_credits,
				u_currentgold::numeric AS current_gems,
				version_name,
        COALESCE(u_furthestlevel) AS current_level,
				u_furthestlevel::numeric AS furthest_level
				
			FROM	
				events.ac_session_account oe"


ac_e_query_first_launch <- " 
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


ac_e_query_event_level_finished <- "SELECT 
        oe.amplitude_id AS amplitude_id,
        event_time::date AS dt,
        cast(extract(epoch from event_time) as integer) as ts,
        event_type AS event,
        fl.fl_date AS fl_date,
        u_store as store_id,
        country AS country_code,
        e_levelname as current_level,
        u_currentcredits::numeric AS current_credits,
        u_currentgold::numeric AS current_gems,
        u_furthest_level_id::numeric AS furthest_level,
        e_outcome as outcome,
        e_event_identifier as event_identifier,
        e_hero as hero_1,
        e_herolevel::numeric as hero1_level,
        e_herokills::numeric as hero1_kills,
        e_hero2 as hero_2,
        e_herolevel2::numeric as hero2_level,
        e_herokills2::numeric as hero2_kills,
        e_tower_mods_available_white::numeric AS white_mod,
        e_tower_mods_available_green::numeric AS green_mod,
        e_tower_mods_available_blue::numeric AS blue_mod,
        e_tower_mods_available_purple::numeric AS purple_mod,
        e_tower_mods_used as tower_mods_used,
        e_timetaken AS time_taken,
        e_carepackagesoffered::numeric as carepackage_offered,
        e_cryobombs_used::numeric as cryobombs_used,
        e_supertoweruses::numeric as supertower_uses,
        e_reinforcesused::numeric as reinforces_used,
        e_airstrikesused::numeric as airstrikes_used,
        e_energyboosts_used::numeric as energyboost_used,
        e_secondchancesoffered::numeric as second_chances_offered,
        e_supertowerrefillsbought::numeric as super_tower_refills_bought,
        e_supertowerrefillsoffered::numeric as super_tower_refills_offered,
        e_carepackagesbought_energyboost::numeric as carepackage_bought_energyboost,
        e_carepackagesbought_reinforce::numeric as  carepackage_bought_reinforcement,
        e_carepackagesbought_cryobomb::numeric as carepackage_bought_cryobomb,
        e_carepackagesbought_bomb::numeric as carepackage_bought_bomb,
        e_secondchancesbought::numeric as second_chances_bought,
        e_session_time as session_time,
        e_liveslostperwave as lives_lost,
        e_levelname as level_name,
        e_maxwave as maxwave,
        e_tickets_left as tickets_left,
        e_reward_tier_reached as reward_tier_reached,
        e_attempts as attempts,
        e_aliens_killed as aliens_killed
        
        FROM
        events.ac_event_level_finished oe"
				

ac_e_query_currency_spent <- "
                SELECT 
        oe.amplitude_id AS amplitude_id,
        event_type AS event,
        cast(extract(epoch from event_time) as integer) AS ts,
        event_time::date AS dt,
        e_purchase as product_id,
        u_fb_email AS email,
        start_version AS version,
        country AS country_code,
        u_store AS store,
        COALESCE(u_furthestlevel) AS current_level,
        u_currentcredits::numeric AS current_credits,
        u_currentgold::numeric AS current_gems,
        e_creditsspent AS credits_spent,
        e_purchase AS purchase,
        e_context as context
        
        FROM events.ac_currencyspent oe
        "