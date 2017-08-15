df <- fread("D:\\data.csv")

# 1. cleaning 

names(df)[names(df) == "revenue_usd"] <- "revenue"

df$level_id <- as.numeric(df$level_id)
df$revenue <- as.numeric(df$revenue)
df$revenue[is.na(df$revenue)] <- 0
# Selecting only cohort_ids.

df <- df %>% 
  filter(cohort_id != ".")


